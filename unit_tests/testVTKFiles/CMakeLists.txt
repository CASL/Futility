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
INCLUDE(MPACT_CopyFiles)
                           
# List any files from the main source directory that the test depends on
SET(ADDITIONAL_FILES
    mesh3Points.txt
    mesh3Data.txt
    VtktestCyl.vtk
    mesh5ref.txt
)

MPACT_CreateUnitTest(testVTKFiles)         
       
MPACT_CopyFiles(ADDITIONAL_FILES)
UNSET(ADDITIONAL_FILES)
