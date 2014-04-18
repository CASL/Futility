################################################################################
#
# CMake Configuration File for MPACT unit tests
#
# Description: Adds the subdirectories for the unit tests
#
# Author: Derek Lax, Brendan Kochunas
#   Date: 06/05/2012
#
################################################################################

# List the names of all the tests (same as subdirectory)
# The order of the names is reflective of the hierarchy of the code dependencies.
# The tests at the beginning of the list have the least dependencies and the
# the tests at the end have the most dependencies.
# 
SET(UNIT_TEST_NAMES
    testUnitTest
    testSelectedKinds
    testBLAS
    testStrings
    testTimes
    testGeom
    testSorting
    testBinaryTrees
    testExceptionHandler
    testSpaceFillingCurve
    testAllocs
    testParameterLists
    testIOutil
    testFileType_Base
    testFileType_Fortran
    testFileType_Input
    testFileType_Log
    testFileType_DA32
    testFileType_Checkpoint
    testParallelEnv
    testExpTables
    testVTKFiles
    testCmdLineProc
    testVectorTypes
    testMatrixTypes
    testPreconditionerTypes
    testLinearSolver
    testStochasticSampler
)

IF("${PROJECT_NAME}" STREQUAL "MPACT")
    SET(UNIT_TEST_NAMES ${UNIT_TEST_NAMES} testAllocsOOM)
ENDIF()

# add test for PARDISO if enabled
IF(${PACKAGE_NAME}_ENABLE_PARDISO)
    SET(UNIT_TEST_NAMES
        ${UNIT_TEST_NAMES}
        testTPLPARDISO
        )
ENDIF()

# add test for PETSC if enabled
IF(${PACKAGE_NAME}_ENABLE_PETSC)
    SET(UNIT_TEST_NAMES
        ${UNIT_TEST_NAMES}
        testTPLPETSC
        testParTPLPETSC
        )
ENDIF()

# add test for SLEPC if enabled
IF(${PACKAGE_NAME}_ENABLE_SLEPC)
    SET(UNIT_TEST_NAMES
        ${UNIT_TEST_NAMES}
        testTPLSLEPC
        )
ENDIF()

# add test for HDF5 if enabled
IF(${PACKAGE_NAME}_ENABLE_HDF5)
    SET(UNIT_TEST_NAMES
        ${UNIT_TEST_NAMES}
        testHDF5FileType
        testTPLHDF5
        )
ENDIF()

FOREACH(test ${UNIT_TEST_NAMES})
    ADD_SUBDIRECTORY(${test})
ENDFOREACH()

# Clear these variables
UNSET(UNIT_TEST_NAMES)
UNSET(test)

