#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

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

