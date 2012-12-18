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
    testStrings
    testTimes
    testGeom
    testBinaryTrees
    testSpaceFillingCurve
    testExceptionHandler      
    testAllocs
    testExpTables
    testStochasticSampler
    testParallelEnv
    testIOutil
    testParameterLists
    testVTKFiles
    testCmdLineProc
    testBLAS
    testMatrixTypes
    testVectorTypes
    testLinearSolver
#    testTPLS
)
FOREACH(test ${UNIT_TEST_NAMES})
    ADD_SUBDIRECTORY(${test})
ENDFOREACH()

# Clear these variables
UNSET(UNIT_TEST_NAMES)
UNSET(test)
