#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

SET(EXAMPLENAME exampleFMU2_rectifier)

TRIBITS_ADD_EXECUTABLE(${EXAMPLENAME}
    SOURCES ${EXAMPLENAME}.f90
    LINKER_LANGUAGE Fortran
)
UNSET(EXAMPLENAME)


SET(EXAMPLENAME exampleFMU2_bouncing_ball)

TRIBITS_ADD_EXECUTABLE(${EXAMPLENAME}
    SOURCES ${EXAMPLENAME}.f90
    LINKER_LANGUAGE Fortran
)
UNSET(EXAMPLENAME)
