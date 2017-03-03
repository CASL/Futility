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

# List any files from the main source directory that the test depends on
SET(ADDITIONAL_FILES
)

Futility_CreateUnitTest(testUnitTest)

Futility_CopyFiles(ADDITIONAL_FILES)
UNSET(ADDITIONAL_FILES)
