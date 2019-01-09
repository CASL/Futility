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
    matrices/LpU.txt
    matrices/LU.txt
    matrices/matrix.txt
    matrices/solution.txt
    matrices/vector.txt
)

Futility_CreateUnitTest(testRSORprecon)

Futility_CopyFiles(ADDITIONAL_FILES)
UNSET(ADDITIONAL_FILES)
