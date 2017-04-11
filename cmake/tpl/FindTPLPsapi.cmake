#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

IF(WIN32)
	SET(TPL_Psapi_LIBRARIES "C:/Windows/System32/psapi.dll")

	TRIBITS_TPL_FIND_INCLUDE_DIRS_AND_LIBRARIES(Psapi
	  REQUIRED_LIBS_NAMES psapi)
ELSE()
	MESSAGE("TPL Psapi is only available on Windows!")
ENDIF()