#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
INCLUDE(${Futility_SOURCE_DIR}/cmake/Export_Test_Trace.cmake)

# This function sets the test labels appropriately when passed the desired test
# name and category
FUNCTION(SetTestLabels TESTTMP)
    # The test shall have one argument in the case Tribits failed to add the test
    # This results in the test name being empty, which must be guarded against
    SET(TESTNAME "INVALID")
    SET(extra_args ${ARGN})
    LIST(LENGTH extra_args num_args)
    IF(${num_args} GREATER 0)
      LIST(GET extra_args 0 TESTCAT)
      SET(TESTNAME ${TESTTMP})
    ENDIF()

    IF(NOT "${TESTNAME}" STREQUAL "INVALID")
      SET_PROPERTY(TEST ${TESTNAME} APPEND PROPERTY LABELS ${TESTCAT})
      IF("${TESTCAT}" STREQUAL "BASIC")
	SET_PROPERTY(TEST ${TESTNAME} APPEND PROPERTY LABELS "CONTINUOUS")
	SET_PROPERTY(TEST ${TESTNAME} APPEND PROPERTY LABELS "NIGHTLY")
	SET_PROPERTY(TEST ${TESTNAME} APPEND PROPERTY LABELS "HEAVY")
      ELSEIF("${TESTCAT}" STREQUAL "CONTINUOUS")
	SET_PROPERTY(TEST ${TESTNAME} APPEND PROPERTY LABELS "NIGHTLY")
	SET_PROPERTY(TEST ${TESTNAME} APPEND PROPERTY LABELS "HEAVY")
      ELSEIF("${TESTCAT}" STREQUAL "NIGHTLY")
	SET_PROPERTY(TEST ${TESTNAME} APPEND PROPERTY LABELS "HEAVY")
      ENDIF()
      UNSET(TESTNAME)
      UNSET(TESTTMP)
    ENDIF()
ENDFUNCTION()

FUNCTION(FUTILITY_POST_ADD_TEST TESTNAME TESTNAME_OUT TESTCAT)
  IF(NOT ${TESTNAME_OUT} STREQUAL "")
    EXPORT_TEST_TRACE(
      ${CMAKE_CURRENT_SOURCE_DIR}
      ${TESTNAME}
      ${TESTNAME_OUT}
    )
  ENDIF()
  SetTestLabels(${TESTNAME_OUT} "${TESTCAT}")
ENDFUNCTION()

FUNCTION(Futility_CreateUnitTest TESTNAME)
    TRIBITS_ADD_EXECUTABLE_AND_TEST(${TESTNAME}
        SOURCES ${TESTNAME}.f90
        NUM_MPI_PROCS 1
        LINKER_LANGUAGE Fortran
        TIMEOUT ${DART_TESTING_TIMEOUT_IN}
        ADDED_TESTS_NAMES_OUT TESTNAME_OUT
    )
    IF(TESTNAME_OUT)
      FUTILITY_POST_ADD_TEST(${TESTNAME} ${TESTNAME_OUT} "BASIC")
    ENDIF()
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
    IF(TESTNAME_OUT)
      FUTILITY_POST_ADD_TEST(${TESTNAME} ${TESTNAME_OUT} "BASIC")
    ENDIF()
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
    IF(TESTNAME_OUT)
      FUTILITY_POST_ADD_TEST(${TESTNAME} ${TESTNAME_OUT} "BASIC")
    ENDIF()
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
    IF(TESTNAME_OUT)
      FUTILITY_POST_ADD_TEST(${TESTNAME} ${TESTNAME_OUT} "BASIC")
    ENDIF()
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
    IF(TESTNAME_OUT)
      FUTILITY_POST_ADD_TEST(${TESTNAME} ${TESTNAME_OUT} "BASIC")
    ENDIF()
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
    IF(TESTNAME_OUT)
      FUTILITY_POST_ADD_TEST(${TESTNAME} ${TESTNAME_OUT} "BASIC")
    ENDIF()
    UNSET(TESTNAME)
ENDFUNCTION()
