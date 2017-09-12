#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

TRIBITS_ADD_EXECUTABLE(testDBC
     SOURCES testDBC.f90
     NOEXEPREFIX
     LINKER_LANGUAGE Fortran
)

IF(${${PROJECT_NAME}_ENABLE_DBC})
  TRIBITS_ADD_ADVANCED_TEST(testDBC
    TIMEOUT ${DART_TESTING_TIMEOUT_IN}
    CATEGORIES BASIC
    TEST_0
        NOEXEPREFIX
        EXEC testDBC
        NUM_MPI_PROCS 1
        ARGS 1
        ALWAYS_FAIL_ON_NONZERO_RETURN
    TEST_1
        NOEXEPREFIX
        EXEC testDBC
        NUM_MPI_PROCS 1
        ARGS 2
        ALWAYS_FAIL_ON_NONZERO_RETURN
    TEST_2
        NOEXEPREFIX
        EXEC testDBC
        NUM_MPI_PROCS 1
        ARGS 3
        WILL_FAIL
    TEST_3
        NOEXEPREFIX
        EXEC testDBC
        NUM_MPI_PROCS 1
        ARGS 4
        WILL_FAIL
  )
ELSE()
  TRIBITS_ADD_ADVANCED_TEST(testDBC
    TIMEOUT ${DART_TESTING_TIMEOUT_IN}
    CATEGORIES BASIC
    TEST_0
        NOEXEPREFIX
        EXEC testDBC
        NUM_MPI_PROCS 1
        ARGS 1
        ALWAYS_FAIL_ON_NONZERO_RETURN
    TEST_1
        NOEXEPREFIX
        EXEC testDBC
        NUM_MPI_PROCS 1
        ARGS 2
        ALWAYS_FAIL_ON_NONZERO_RETURN
    TEST_2
        NOEXEPREFIX
        EXEC testDBC
        NUM_MPI_PROCS 1
        ARGS 3
        ALWAYS_FAIL_ON_NONZERO_RETURN
    TEST_3
        NOEXEPREFIX
        EXEC testDBC
        NUM_MPI_PROCS 1
        ARGS 4
        ALWAYS_FAIL_ON_NONZERO_RETURN
  )
ENDIF()