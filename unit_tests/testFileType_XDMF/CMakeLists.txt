#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
INCLUDE(Futility_CreateUnitTest)

# List any files from the main source directory that the test depends on
SET(ADDITIONAL_FILES
  gridmesh_two_pins.xdmf
  gridmesh_two_pins.h5
  gridmesh_three_level_grid.xdmf
  gridmesh_three_level_grid.h5
  three_level_grid.xdmf
  three_level_grid.h5
)


Futility_CreateUnitTest(testFileType_XDMF)

Futility_CopyFiles(ADDITIONAL_FILES)
UNSET(ADDITIONAL_FILES)
