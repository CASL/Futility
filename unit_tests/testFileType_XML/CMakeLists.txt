################################################################################
#
# CMake File for MPACT Unit Tests
#
# Description: Template CMakeLists.txt file for a unit test. Define any 
#              additional sources from the main source directory that are 
#              required to build the unit test.
#
# Author: Brendan Kochunas
#   Date: 04/21/2011
#
################################################################################
INCLUDE(MPACT_CreateUnitTest)

# List any files from the main source directory that the test depends on
SET(ADDITIONAL_FILES
    testFile.xml
)

MPACT_CreateUnitTest(testFileType_XML)

MPACT_CopyFiles(ADDITIONAL_FILES)
UNSET(ADDITIONAL_FILES)