################################################################################
#
# CMake/TriBITS File for MPACT
#
# Description: Loads the Windows Processor status API (Psapi).
#
# Author: Brendan Kochunas
#   Date: 02/29/2016
#
################################################################################

IF(WIN32)
	SET(TPL_Psapi_LIBRARIES "C:/Windows/System32/psapi.dll")

	TRIBITS_TPL_FIND_INCLUDE_DIRS_AND_LIBRARIES(Psapi
	  REQUIRED_LIBS_NAMES psapi)
ELSE()
	MESSAGE("TPL Psapi is only available on Windows!")
ENDIF()