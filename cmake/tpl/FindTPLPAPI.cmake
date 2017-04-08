#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

TRIBITS_TPL_FIND_INCLUDE_DIRS_AND_LIBRARIES( PAPI
  REQUIRED_HEADERS f77papi.h f90papi.h fpapi.h papi.h
  REQUIRED_LIBS_NAMES papi)

