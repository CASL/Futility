#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
INCLUDE(SetTestLabels)

FUNCTION(Futility_CreateUnitTest TESTNAME)
    TRIBITS_ADD_EXECUTABLE_AND_TEST(${TESTNAME}
        SOURCES ${TESTNAME}.f90
        NUM_MPI_PROCS 1
        LINKER_LANGUAGE Fortran
        TIMEOUT ${DART_TESTING_TIMEOUT_IN}
        ADDED_TESTS_NAMES_OUT TESTNAME_OUT
    )
    SetTestLabels(${TESTNAME_OUT} "BASIC")
    UNSET(TESTNAME)
ENDFUNCTION()

FUNCTION(Futility_CreateParUnitTest TESTNAME NPROC)
    TRIBITS_ADD_EXECUTABLE_AND_TEST(${TESTNAME}
        SOURCES ${TESTNAME}.f90
        NUM_MPI_PROCS ${NPROC}
        LINKER_LANGUAGE Fortran
        TIMEOUT ${DART_TESTING_TIMEOUT_IN}
        ADDED_TESTS_NAMES_OUT TESTNAME_OUT
    )
    SetTestLabels(${TESTNAME_OUT} "BASIC")
    UNSET(TESTNAME)
ENDFUNCTION()

FUNCTION(Futility_CreateUnitTest_C TESTNAME)
    TRIBITS_ADD_EXECUTABLE_AND_TEST(${TESTNAME}
        SOURCES ${TESTNAME}.c
        NUM_MPI_PROCS 1
        LINKER_LANGUAGE C
        TIMEOUT ${DART_TESTING_TIMEOUT_IN}
        ADDED_TESTS_NAMES_OUT TESTNAME_OUT
    )
    SetTestLabels(${TESTNAME_OUT} "BASIC")
    UNSET(TESTNAME)
ENDFUNCTION()

FUNCTION(Futility_CreateParUnitTest_C TESTNAME NPROC)
    TRIBITS_ADD_EXECUTABLE_AND_TEST(${TESTNAME}
        SOURCES ${TESTNAME}.c
        NUM_MPI_PROCS ${NPROC}
        LINKER_LANGUAGE C
        TIMEOUT ${DART_TESTING_TIMEOUT_IN}
        ADDED_TESTS_NAMES_OUT TESTNAME_OUT
    )
    SetTestLabels(${TESTNAME_OUT} "BASIC")
    UNSET(TESTNAME)
ENDFUNCTION()

FUNCTION(Futilty_CreateUnitTest_CPP TESTNAME)
    TRIBITS_ADD_EXECUTABLE_AND_TEST(${TESTNAME}
        SOURCES ${TESTNAME}.cpp
        NUM_MPI_PROCS 1
        LINKER_LANGUAGE CXX
        TIMEOUT ${DART_TESTING_TIMEOUT_IN}
        ADDED_TESTS_NAMES_OUT TESTNAME_OUT
    )
    SetTestLabels(${TESTNAME_OUT} "BASIC")
    UNSET(TESTNAME)
ENDFUNCTION()

FUNCTION(Futility_CreateParUnitTest_CPP TESTNAME NPROC)
    TRIBITS_ADD_EXECUTABLE_AND_TEST(${TESTNAME}
        SOURCES ${TESTNAME}.cpp
        NUM_MPI_PROCS ${NPROC}
        LINKER_LANGUAGE CXX
        TIMEOUT ${DART_TESTING_TIMEOUT_IN}
        ADDED_TESTS_NAMES_OUT TESTNAME_OUT
    )
    SetTestLabels(${TESTNAME_OUT} "BASIC")
    UNSET(TESTNAME)
ENDFUNCTION()
