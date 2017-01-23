#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

SET(EXAMPLENAME exampleTAU_Stubs)

IF("${PROJECT_NAME}" STREQUAL "MPACT")
    # Do not prepend test name with package name
    TRIBITS_ADD_EXECUTABLE(${EXAMPLENAME}
        SOURCES ${EXAMPLENAME}.f90
        NOEXEPREFIX
        LINKER_LANGUAGE Fortran
    )
ELSE()
    TRIBITS_ADD_EXECUTABLE(${EXAMPLENAME}
        SOURCES ${EXAMPLENAME}.f90
        LINKER_LANGUAGE Fortran
    )
ENDIF()
UNSET(EXAMPLENAME)
