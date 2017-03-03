#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
INCLUDE(Futility_CreateUnitTest)
INCLUDE(Futility_CopyFiles)

SET(ADDITIONAL_FILES
    testInit.xml
)

Futility_CreateUnitTest(testParameterLists)
Futility_CopyFiles(ADDITIONAL_FILES)
UNSET(ADDITIONAL_FILES)
