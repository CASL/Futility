#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
INCLUDE(MPACT_CreateUnitTest)
INCLUDE(MPACT_CopyFiles)

# List any files from the main source directory that the test depends on
SET(ADDITIONAL_FILES
    readtest.h5
)

MPACT_CreateUnitTest(testHDF5FileType)

MPACT_CopyFiles(ADDITIONAL_FILES)
UNSET(ADDITIONAL_FILES)
