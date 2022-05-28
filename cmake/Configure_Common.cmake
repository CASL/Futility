#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

# Do all of the processing for this project with TriBITS
TRIBITS_PROJECT()
MESSAGE(STATUS "------------------------------------------------------------")
MESSAGE(STATUS "")

# Additional processing for custom targets and compiler commands
MESSAGE(STATUS "${PROJECT_NAME}: Additional processing...")

# Enable Documentation, Coverage, and BLAS
# if using Microsoft Visual Studio
IF(MSVC_IDE)
    IF(NOT DEFINED ${PROJECT_NAME}_ENABLE_DOCS)
        SET(${PROJECT_NAME}_ENABLE_DOCS TRUE)
    ENDIF()
    IF(NOT DEFINED TPL_ENABLE_BLAS)
        SET(TPL_ENABLE_BLAS TRUE)
    ENDIF()
ENDIF()

# Add valgrind tests
IF(${PROJECT_NAME}_ENABLE_MEMCHECK AND ${PROJECT_NAME}_ENABLE_TESTS AND NOT(WIN32 OR CYGWIN))
    MESSAGE(STATUS "${PROJECT_NAME}: Adding Valgrind tests...")
    INCLUDE(Valgrind_Test_Config)
    Valgrind_Test_Config(${PROJECT_NAME}_ENABLE_MEMCHECK)
ENDIF()

# Add code coverage
IF(${PROJECT_NAME}_ENABLE_CODECOV)
    MESSAGE(STATUS "${PROJECT_NAME}: Configuring code coverage targets...")
    INCLUDE(CodeCoverage_Config)
    CodeCoverage_CreateTargets(${PROJECT_NAME}_ENABLE_CODECOV)
ENDIF()

# Add documentation if enabled
IF(${PROJECT_NAME}_ENABLE_DOCS AND "${DOXYGEN_FOUND}" STREQUAL "YES")
    MESSAGE(STATUS "${PROJECT_NAME}: Configuring documentation...")
    ADD_SUBDIRECTORY(doc)
ENDIF()


# Add UM2 package
MESSAGE(STATUS "CMAKE_MODULE_PATH=${CMAKE_MODULE_PATH}")
IF(${PROJECT_NAME}_ENABLE_UM2)
    MESSAGE(STATUS "${PROJECT_NAME}: Configuring UM2 Julia Library...")
    find_package(Julia)
    #ADD_SUBDIRECTORY(extern/UM2)
ENDIF()

# Add validation test subdirectory
MESSAGE(STATUS "${PROJECT_NAME}: Configuring validation tests...")
MESSAGE("    ${${PROJECT_NAME}_TEST_CATEGORIES} test categories are enabled.")
MESSAGE(STATUS "${PROJECT_NAME}: Processing Completed...")
