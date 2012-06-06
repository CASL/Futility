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
MPACT_CreateUnitTest(testBinaryTrees 0)
