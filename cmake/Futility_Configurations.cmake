#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
INCLUDE(CheckMPIF2003StandardCompiles)
IF(${PROJECT_NAME}_VERBOSE_CONFIGURE)
    MESSAGE("Configuring Futility compiler options for:")
    PRINT_VAR(PACKAGE_NAME)
    PRINT_VAR(${PACKAGE_NAME}_ENABLE_TESTS)
    PRINT_VAR(${PACKAGE_NAME}_ENABLE_DBLREAL)
    PRINT_VAR(${PACKAGE_NAME}_ENABLE_DBLINT)
    PRINT_VAR(${PACKAGE_NAME}_ENABLE_MPI)
    PRINT_VAR(${PACKAGE_NAME}_ENABLE_PETSC)
    PRINT_VAR(${PACKAGE_NAME}_ENABLE_SLEPC)
    PRINT_VAR(${PACKAGE_NAME}_ENABLE_HDF5)
    PRINT_VAR(${PACKAGE_NAME}_ENABLE_MKL)
    PRINT_VAR(${PACKAGE_NAME}_ENABLE_PARDISO)
    PRINT_VAR(${PACKAGE_NAME}_ENABLE_BLAS)
    PRINT_VAR(${PACKAGE_NAME}_ENABLE_OpenMP)
    PRINT_VAR(${PACKAGE_NAME}_ENABLE_PAPI)
    PRINT_VAR(${PROJECT_NAME}_BUILD_STANDARD)
    PRINT_VAR(${PROJECT_NAME}_DISABLE_PURE)
ENDIF()

IF(NOT DEFINED ${PROJECT_NAME}_FPE_TRAP)
   SET(${PROJECT_NAME}_FPE_TRAP FALSE)
ENDIF()
IF(NOT DEFINED ${PROJECT_NAME}_BUILD_STANDARD)
   GLOBAL_SET(${PROJECT_NAME}_BUILD_STANDARD TRUE)
ENDIF()

# Define known configuration types for Futility
SET(CONFIG_TYPES
    DEBUG
    RELEASE
    RELWITHDEBINFO
   )

# Set Default configuration for Futility
LIST(GET CONFIG_TYPES 0 ${PACKAGE_NAME}_BUILD_TYPE)

# Determine if Project configuration is supported by Futility and override default.
FOREACH(ctyp ${CONFIG_TYPES})
    IF("${CMAKE_BUILD_TYPE}" STREQUAL "${ctyp}")
        SET(${PACKAGE_NAME}_BUILD_TYPE
            ${CMAKE_BUILD_TYPE}
           )
    ENDIF()
ENDFOREACH()

# Determine symbol for command line compiler option
IF("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows" AND CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    SET(CSYM "/")
ELSE()
    SET(CSYM "-")
ENDIF()

# Preprocessor definition if Windows
IF("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} WIN32)
ENDIF()

IF(${PROJECT_NAME}_ENABLE_DBC)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} FUTILITY_DBC)
ENDIF()

# Preprocessor symbol for tests
IF(${PACKAGE_NAME}_ENABLE_TESTS)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} UNIT_TEST)
ENDIF()

# Preprocessor symbol for colored outputs
IF(${PROJECT_NAME}_COLOR_LINUX)
    MESSAGE("Enabling colored output for linux")
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} COLOR_LINUX)
ENDIF()
IF(${PROJECT_NAME}_COLOR_WIN32)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} COLOR_WIN32)
ENDIF()

# Other Futility preprocessor symbols
IF(${PACKAGE_NAME}_ENABLE_DBLREAL)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} DBL)
ENDIF()
IF(${PACKAGE_NAME}_ENABLE_DBLINT)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} DBLINT)
ENDIF()

# Disable PURE and ELEMENTAL
IF(${PROJECT_NAME}_DISABLE_PURE)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} PURE=)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} ELEMENTAL=)
ENDIF()

# Preprocessor symbol for using memory profile
IF(${PROJECT_NAME}_ENABLE_MEMPROF)
    SET(${PACKAGE_NAME}_DEFINES ${${PACKAGE_NAME}_DEFINES} FUTILITY_MEMPROF)
ENDIF()

# OpenMP Compiler flag (like a TPL)
IF(${PACKAGE_NAME}_ENABLE_OpenMP AND DEFINED OpenMP_C_FLAGS)
    SET(OpenMP_Fortran_FLAGS ${OpenMP_C_FLAGS})
ENDIF()

# Check if MPI supports Fortran 2003 standard
IF(${PROJECT_NAME}_BUILD_STANDARD)
    CHECK_MPI_F2003_STANDARD_COMPILES()
    IF(${PROJECT_NAME}_BUILD_STANDARD AND NOT MPI_F2003_STANDARD_COMPLIANT)
        MESSAGE(STATUS "Disabling F2003 Standard since MPI install is not F2003 compliant")
        GLOBAL_SET(${PROJECT_NAME}_BUILD_STANDARD ${MPI_F2003_STANDARD_COMPLIANT})
    ENDIF()
ELSE()
    IF(${PACKAGE_NAME} STREQUAL "Futility")
        MESSAGE(STATUS "Project has disabled F2003 standard compilation")
    ENDIF()
ENDIF()

# Override the CMAKE default Fortran flags (cause they're stupid)
# Done for each supported compiler
IF(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR
   (MSVC_IDE AND "${CMAKE_Fortran_COMPILER}" STREQUAL "ifort"))

    IF("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
        SET(s ":")

        #Overwrite OpenMP flag (cause Intel is stupi--special)
        IF(DEFINED OpenMP_Fortran_FLAGS)
          SET(OpenMP_Fortran_FLAGS ${CSYM}Qopenmp)
        ENDIF()
    ELSE()
        SET(s " ")
    ENDIF()

    SET(Fortran_FLAGS
        ${CSYM}nologo
        ${CSYM}fpp
        ${CSYM}threads
       )
    IF(${PROJECT_NAME}_BUILD_STANDARD)
        SET(Fortran_FLAGS
            ${Fortran_FLAGS}
            ${CSYM}stand${s}f03
           )
    ENDIF()

    IF(${PROJECT_NAME}_FPE_TRAP)
        SET(Fortran_FLAGS
            ${Fortran_FLAGS}
            ${CSYM}fpe-all${s}0
           )
    ENDIF()

    IF(${PROJECT_NAME}_SUPPRESS_WARNINGS)
        SET(Fortran_FLAGS
            ${Fortran_FLAGS}
            ${CSYM}w
        )
    ENDIF()

    IF(BUILD_SHARED_LIBS AND NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
        SET(Fortran_FLAGS
            ${Fortran_FLAGS}
            ${CSYM}fpic
        )
    ENDIF()

    IF(BUILD_SHARED_LIBS AND "${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
        MESSAGE(STATUS "Shared libraries not supported in windows.")
        SET(Fortran_FLAGS ${Fortran_FLAGS} ${CSYM}libs:static)
        SET(BUILD_SHARED_LIBS FALSE)
    ENDIF()


    SET(Fortran_FLAGS_DEBUG
        ${CSYM}debug${s}full
        ${CSYM}debug-parameters${s}all
        ${CSYM}warn${s}all
        ${CSYM}check${s}all
        ${CSYM}traceback
       )
    IF("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
        SET(Fortran_FLAGS_DEBUG ${Fortran_FLAGS_DEBUG}
            ${CSYM}Od ${CSYM}dbglibs)
    ENDIF()

    SET(Fortran_FLAGS_RELEASE
        ${CSYM}O3
        ${CSYM}Os
       )

    IF(${PACKAGE_NAME}_ENABLE_MKL)
        IF(DEFINED OpenMP_Fortran_FLAGS)
            SET(mkllib parallel)
        ELSEIF(${PACKAGE_NAME}_ENABLE_MPI)
            SET(mkllib cluster)
        ELSE()
            SET(mkllib sequential)
        ENDIF()
        IF("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
            SET(Fortran_FLAGS ${Fortran_FLAGS} ${CSYM}Qmkl:${mkllib})
        ELSE()
            SET(Fortran_FLAGS ${Fortran_FLAGS} ${CSYM}mkl=${mkllib})
        ENDIF()
        UNSET(mkllib)
    ENDIF()

    IF("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
        SET(Fortran_FLAGS_RELEASE ${Fortran_FLAGS_RELEASE}
            ${CSYM}QxHost ${CSYM}inline:speed)
    ELSE()
        SET(Fortran_FLAGS_RELEASE ${Fortran_FLAGS_RELEASE}
            ${CSYM}inline-level=2)
    ENDIF()
ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    SET(Fortran_FLAGS
        ${CSYM}cpp
        ${CSYM}fall-intrinsics
        ${CSYM}ffree-line-length-none
       )

    SET(C_FLAGS
        ${CSYM}lm
       )

    IF(${PROJECT_NAME}_BUILD_STANDARD)
        SET(Fortran_FLAGS
            ${Fortran_FLAGS}
            ${CSYM}std=f2003
           )
    ENDIF()

    IF(${PROJECT_NAME}_FPE_TRAP)
        SET(Fortran_FLAGS
            ${Fortran_FLAGS}
            ${CSYM}ffpe-trap=invalid,zero,overflow,underflow,denormal
           )
    ENDIF()

    IF(${PROJECT_NAME}_SUPPRESS_WARNINGS)
        SET(Fortran_FLAGS
            ${Fortran_FLAGS}
            ${CSYM}w
           )
    ENDIF()

    IF(BUILD_SHARED_LIBS)
        SET(Fortan_FLAGS
            ${Fortran_FLAGS}
            ${CSYM}fPIC
           )
    ELSE()
        SET(Fotran_FLAGS
            ${Fortran_FLAGS}
            ${CSYM}static-libgfortran
        )
    ENDIF()

    IF(${PROJECT_NAME}_GPROF)
        SET(Fortran_FLAGS
            ${Fortran_FLAGS}
            ${CSYM}pg
           )
    ENDIF()

    SET(Fortran_FLAGS_DEBUG
        ${CSYM}O0
        ${CSYM}Wall
        ${CSYM}fcheck=all
        ${CSYM}fbacktrace
        ${CSYM}g
       )

    SET(Fortran_FLAGS_RELEASE
        ${CSYM}O3
       )

    SET(Fortran_FLAGS_RELWITHDEBINFO
        ${CSYM}O3
        ${CSYM}fbounds-check
       )
ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")

    SET(Fortran_FLAGS
        ${CSYM}Mpreprocess
        ${CSYM}Mstandard
        ${CSYM}Mallocatable=03
        ${CSYM}pgf90libs
       )
    ## TODO: Figure out what sets 2003 standard for PGI and add same if statement for Futility_BUILD_NOSTANDARD
    SET(Fortran_FLAGS_DEBUG
        ${CSYM}g
        ${CSYM}Mbounds
        ${CSYM}Mdclchk
        ${CSYM}Mchkptr
        ${CSYM}traceback
       )

# RELEASE to be added for this compiler
    SET(Fortran_FLAGS_RELEASE
        ${CSYM}O3
       )
ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "Cray")

    SET(Fortran_FLAGS_DEBUG
        ${CSYM}O0
        ${CSYM}g
       )

    SET(Fortran_FLAGS_RELEASE
        ${CSYM}O2
       )
ELSE()
    MESSAGE(WARNING " Fortran compiler: ${CMAKE_Fortran_COMPILER_ID} not supported!")
ENDIF()


IF(${PROJECT_NAME}_ENABLE_CODECOV)
    MESSAGE(STATUS " -- Configuring code coverage compiler flags...")
    INCLUDE(CodeCoverage_Config)
    CodeCoverage_SetCompilerFlags(${PROJECT_NAME}_ENABLE_CODECOV)
ENDIF()


SET(Fortran_FLAGS ${Fortran_FLAGS} ${OpenMP_Fortran_FLAGS})

# Add define symbols to Fortran_FLAGS and CXX_Flags
FOREACH(def ${${PACKAGE_NAME}_DEFINES})
    SET(Fortran_FLAGS ${Fortran_FLAGS} ${CSYM}D${def})
    SET(CXX_FLAGS ${CXX_FLAGS} ${CSYM}D${def})
    SET(C_FLAGS ${C_FLAGS} ${CSYM}D${def})
ENDFOREACH()
SET(Fortran_FLAGS ${Fortran_FLAGS} ${petsc_include} ${petsc_lib})
SET(CXX_FLAGS ${CXX_FLAGS} ${petsc_include} ${petsc_lib})
#MESSAGE(STATUS "fortran_flags ${Fortran_FLAGS}")

# Append Futility flags to existing CMAKE_Fortran_FLAGS
FOREACH(flag ${Fortran_FLAGS})
    #STRING(REGEX MATCH "${flag}" ispresent "${CMAKE_Fortran_FLAGS}")
    #IF(NOT ispresent)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${flag}")
    #ENDIF()m
ENDFOREACH()

# Append Futility flags to existing CMAKE_CXX_FLAGS
FOREACH(flag ${CXX_FLAGS})
    #STRING(REGEX MATCH "${flag}" ispresent "${CMAKE_CXX_FLAGS}")
    #IF(NOT ispresent)
        SET(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} ${flag})
    #ENDIF()m
ENDFOREACH()
# Not sure if this automatically happens to the CMAKE_Fortran_FLAGS somewhere, but the CMAKE ;'s disappear
# but remain for the CXX flags.  Manually removing them here.
STRING(REGEX REPLACE ";" " " CMAKE_CXX_FLAGS_TEMP "${CMAKE_CXX_FLAGS}")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS_TEMP}")

# Append Futility flags to existing CMAKE_C_FLAGS
FOREACH(flag ${C_FLAGS})
    #STRING(REGEX MATCH "${flag}" ispresent "${CMAKE_C_FLAGS}")
    #IF(NOT ispresent)
        SET(CMAKE_C_FLAGS ${CMAKE_C_FLAGS} ${flag})
    #ENDIF()m
ENDFOREACH()
# Not sure if this automatically happens to the CMAKE_Fortran_FLAGS somewhere, but the CMAKE ;'s disappear
# but remain for the C flags.  Manually removing them here.
STRING(REGEX REPLACE ";" " " CMAKE_C_FLAGS_TEMP "${CMAKE_C_FLAGS}")
SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS_TEMP}")

# Append Futility Debug flags to existing CMAKE_Fortran_FLAGS_DEBUG
FOREACH(flag ${Fortran_FLAGS_DEBUG})
    STRING(REGEX MATCH "${flag}" ispresent "${CMAKE_Fortran_FLAGS_DEBUG}")
    IF(NOT ispresent)
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${flag}")
    ENDIF()
ENDFOREACH()

# Append Futility Release flags to existing CMAKE_Fortran_FLAGS_RELEASE
FOREACH(flag ${Fortran_FLAGS_RELEASE})
    STRING(REGEX MATCH "${flag}" ispresent "${CMAKE_Fortran_FLAGS_RELEASE}")
    IF(NOT ispresent)
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${flag}")
    ENDIF()
ENDFOREACH()

# Append Futility Release flags to existing CMAKE_Fortran_FLAGS_RELWITHDEBINFO
FOREACH(flag ${Fortran_FLAGS_RELWITHDEBINFO})
    STRING(REGEX MATCH "${flag}" ispresent "${CMAKE_Fortran_FLAGS_RELWITHDEBINFO}")
    IF(NOT ispresent)
        SET(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELWITHDEBINFO} ${flag}")
    ENDIF()
ENDFOREACH()

# Clear local variables
UNSET(CONFIG_TYPES)
UNSET(CONFIG_EXIST)
UNSET(CSYM)
UNSET(${PACKAGE_NAME}_DEFINES)
UNSET(OpenMP_Fortran_FLAGS)
UNSET(Fortran_FLAGS)
UNSET(Fortran_FLAGS_STRING)
UNSET(Fortran_FLAGS_DEBUG)
UNSET(Fortran_FLAGS_DEBUG_STRING)
UNSET(Fortran_FLAGS_RELEASE)
UNSET(Fortran_FLAGS_RELEASE_STRING)
UNSET(CXX_FLAGS)
UNSET(CMAKE_CXX_FLAGS_TEMP)
UNSET(flag)
UNSET(def)
UNSET(s)
UNSET(ctyp)
