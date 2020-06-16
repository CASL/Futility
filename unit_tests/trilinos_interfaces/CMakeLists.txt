#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

SET(UNIT_TEST_NAMES
    testKokkosInit
    testInterfaces
)

FOREACH(test ${UNIT_TEST_NAMES})
    ADD_SUBDIRECTORY(${test})
ENDFOREACH()

# Clear these variables
UNSET(UNIT_TEST_NAMES)
UNSET(test)
