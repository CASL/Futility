#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
INCLUDE(Futility_CreateUnitTest)

IF (BUILD_SHARED_LIBS)
    ADD_SUBDIRECTORY(testFMU_BouncingBall)
    Futility_CreateUnitTest(testFMU_Wrapper)
ENDIF()

