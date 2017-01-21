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

SET(EXAMPLENAME exampleTAU_Stubs)

IF("${PROJECT_NAME}" STREQUAL "MPACT")
    # Do not prepend test name with package name
    TRIBITS_ADD_EXECUTABLE(${EXAMPLENAME}
        SOURCES ${EXAMPLENAME}.f90
        NOEXEPREFIX
        LINKER_LANGUAGE Fortran
    )
ELSE()
    TRIBITS_ADD_EXECUTABLE(${EXAMPLENAME}
        SOURCES ${EXAMPLENAME}.f90
        LINKER_LANGUAGE Fortran
    )
ENDIF()
UNSET(EXAMPLENAME)
