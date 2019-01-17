
#
# This is a CMake script and must be run as "cmake -P <SCRIPT_NAME>"
#
# NOTE: To see what commands this script runs, run it as:
#
#    $ cmake -DSHOW_COMMANDS_ONLY=ON -P <SCRIPT_NAME>
#

#
# Variables
#

SET( TEST_NAME Futility_testDBC )

SET( TEST_0_CMND "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testDBC/testDBC.exe" "1" )

SET( TEST_0_ALWAYS_FAIL_ON_NONZERO_RETURN TRUE )

SET( TEST_1_CMND "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testDBC/testDBC.exe" "2" )

SET( TEST_1_ALWAYS_FAIL_ON_NONZERO_RETURN TRUE )

SET( TEST_2_CMND "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testDBC/testDBC.exe" "3" )

SET( TEST_2_WILL_FAIL TRUE )

SET( TEST_3_CMND "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testDBC/testDBC.exe" "4" )

SET( TEST_3_WILL_FAIL TRUE )

SET( TEST_4_CMND "/home/nfherrin/research/MPACT/Futility/Futilitybuild/unit_tests/testDBC/testDBC.exe" "5" )

SET( TEST_4_ALWAYS_FAIL_ON_NONZERO_RETURN TRUE )

SET(PROJECT_NAME Futility)

SET(Futility_TRIBITS_DIR /home/nfherrin/research/MPACT/Futility/cmake/tribits)

SET(TEST_NAME Futility_testDBC)

SET(NUM_CMNDS 5)

SET(OVERALL_WORKING_DIRECTORY "")

SET(SKIP_CLEAN_OVERALL_WORKING_DIRECTORY "FALSE")

SET(FAIL_FAST FALSE)

SET(SHOW_START_END_DATE_TIME OFF)

SET(SHOW_MACHINE_LOAD OFF)

SET(CATEGORIES BASIC)

SET(PROCESSORS 1)

SET(TIMEOUT 1500)

#
# Test invocation
#

SET(CMAKE_MODULE_PATH /home/nfherrin/research/MPACT/Futility/cmake/tribits/core/utils)

INCLUDE(DriveAdvancedTest)

DRIVE_ADVANCED_TEST()
