#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

FUNCTION(CHECK_MPI_F2008_STANDARD_COMPILES )
    SET(CHECK_TEST_SOURCE
    "
      PROGRAM main
        INCLUDE 'mpif.h'
        WRITE(*,*) 'HELLO WORLD'
      ENDPROGRAM
    "
    )

    IF(TPL_ENABLE_MPI AND NOT DEFINED MPI_F2008_STANDARD_COMPLIANT)
        IF("3.1.3" VERSION_GREATER CMAKE_VERSION)
            MESSAGE(STATUS "Skipping check if MPI is F2008 standard compliant")
            GLOBAL_SET(MPI_F2008_STANDARD_COMPLIANT TRUE)
        ELSE()
            #CMake module "CHECK_Fortran_SOURCE_COMPILERS" is only available in
            #version 3.1.3 and later
            INCLUDE(CheckFortranSourceCompiles)

            #Determine appropriate flag for compiler test
            IF(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR
              (MSVC_IDE AND "${CMAKE_Fortran_COMPILER}" STREQUAL "ifort"))
              IF("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
                SET(FSTDFLAG "/std:f08")
              ELSE()
                SET(FSTDFLAG "-std f08")
              ENDIF()
            ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
              SET(FSTDFLAG "-std=f2008")
#            ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
#              SET(FSTDFLAG "-Mstandard")
#            ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "Cray")
#              SET(FSTDFLAG "-e n")
#            ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "IBM")
#
#            ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "Pathscale")
#
            ENDIF()
            SET(OLD_CRF ${CMAKE_REQUIRED_FLAGS})
            SET(CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS} ${FSTDFLAG})

            CHECK_Fortran_SOURCE_COMPILES(${CHECK_TEST_SOURCE}
                MPI_F2008_STANDARD_COMPILES)
            IF(MPI_F2008_STANDARD_COMPILES)
                GLOBAL_SET(MPI_F2008_STANDARD_COMPLIANT TRUE)
            ELSE()
                GLOBAL_SET(MPI_F2008_STANDARD_COMPLIANT FALSE)
            ENDIF()

            SET(CMAKE_REQUIRED_FLAGS ${OLD_CRF})
        ENDIF()
    ELSE()
        IF(TPL_ENABLE_MPI)
            MESSAGE(STATUS
                "MPI F2008 standard compliance already set as: "
                ${MPI_F2008_STANDARD_COMPLIANT}
            )
        ELSE()
            GLOBAL_SET(MPI_F2008_STANDARD_COMPLIANT TRUE)
        ENDIF()
    ENDIF()
ENDFUNCTION()

