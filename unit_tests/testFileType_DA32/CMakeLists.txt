################################################################################
#
# CMake File for MPACT Unit Tests
#
# Description: Template CMakeLists.txt file for a unit test. Define any 
#              additional sources from the main source directory that are 
#              required to build the unit test.
#
# Author: Brendan Kochunas
#   Date: 01/15/2014
#
################################################################################
INCLUDE(MPACT_CreateUnitTest)
INCLUDE(MPACT_CopyFiles)
                           
# List any files from the main source directory that the test depends on
# SET(ADDITIONAL_FILES
#     
# )

MPACT_CreateUnitTest(testFileType_DA32)         
       
# MPACT_CopyFiles(ADDITIONAL_FILES)
# UNSET(ADDITIONAL_FILES)
