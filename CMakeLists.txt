################################################################################
#
# CMake File for MPACT
#
# Description: CMake build script for creating main executable. Adds target
#              for main executable.
#
# Author: Derek Lax, Brendan Kochunas
#   Date: 06/04/2012
#
################################################################################
INCLUDE(TribitsSubPackageMacros)
TRIBITS_SUBPACKAGE(MPACT_Utils)
TRIBITS_PACKAGE_DEF()
TRIBITS_ADD_TEST_DIRECTORIES(tests)
ADD_SUBDIRECTORY(src)
TRIBITS_SUBPACKAGE_POSTPROCESS()
