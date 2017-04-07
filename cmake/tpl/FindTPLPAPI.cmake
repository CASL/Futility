################################################################################
#
# CMake/TriBITS File for MPACT
#
# Description: Loads the PAPI TPL.
#
# Author: Brendan Kochunas
#   Date: 04/25/2013
#
################################################################################


TRIBITS_TPL_FIND_INCLUDE_DIRS_AND_LIBRARIES( PAPI
  REQUIRED_HEADERS f77papi.h f90papi.h fpapi.h papi.h
  REQUIRED_LIBS_NAMES papi)

