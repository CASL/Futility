################################################################################
#
# CMake/TriBITS File for MPACT
#
# Description: Loads the SLEPC TPL.
#
# Author: Shane Stimpson
#   Date: 09/18/2013
#
################################################################################

IF("${SLEPC_INCLUDE_DIRS}" STREQUAL "")
  GLOBAL_SET(SLEPC_INCLUDE_DIRS "$ENV{SLEPC_DIR}/include;$ENV{SLEPC_DIR}/$ENV{SLEPC_ARCH}/include")
ENDIF()
IF("${SLEPC_LIBRARY_DIRS}" STREQUAL "")
  GLOBAL_SET(SLEPC_LIBRARY_DIRS "$ENV{SLEPC_DIR}/$ENV{SLEPC_ARCH}/lib")
ENDIF()

TRIBITS_TPL_FIND_INCLUDE_DIRS_AND_LIBRARIES( SLEPC
  REQUIRED_HEADERS slepc.h
  REQUIRED_LIBS_NAMES slepc)

