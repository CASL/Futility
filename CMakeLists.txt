################################################################################
#
# CMake File for MPACT
#
# Description: Sets up the Utils subpackage.
#             
#
# Author: Derek Lax, Brendan Kochunas
#   Date: 06/05/2012
#
################################################################################
INCLUDE(TribitsSubPackageMacros)
TRIBITS_SUBPACKAGE(MPACT_Utils) 
ADD_SUBDIRECTORY(src)
TRIBITS_ADD_TEST_DIRECTORIES(tests)
TRIBITS_SUBPACKAGE_POSTPROCESS()
