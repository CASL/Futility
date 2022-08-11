########################################################################
# Test tracing function for building the TestTracking.txt file 
# Creates a file in the build tree at CTF/cobra_tf/test_matrix
# This file contains information about test cases to build the 
# requirements table with Futility/cmake/GenerateRequirementsTable.py
#
# TEST_PATH       The directory where the test input files will be found
# FILE_LIST       List with the base name of source files or input files 
#                 containing a test
# TEST_NAME_OUT   The name of the test
########################################################################
FUNCTION(EXPORT_TEST_TRACE TEST_PATH FILE_LIST TEST_NAME_OUT)

   SET(INDX  0)
   LIST(LENGTH FILE_LIST NSRC)
   WHILE(INDX LESS NSRC)
      LIST(GET FILE_LIST ${INDX} FILE_NAME)

      STRING(REPLACE "${${${PACKAGE_NAME}_PARENT_REPOSITORY}_SOURCE_DIR}/" ""
         TEST_INPUT "${TEST_PATH}/${FILE_NAME}")
      STRING(REPLACE "//" "/" TEST_INPUT "${TEST_INPUT}")
      STRING(REPLACE "/./" "/" TEST_INPUT "${TEST_INPUT}")

      IF(NOT DEFINED ${${PACKAGE_NAME}_PARENT_REPOSITORY}_BEGIN_FILE_NAME_EXPORT)
        GLOBAL_SET(${${PACKAGE_NAME}_PARENT_REPOSITORY}_BEGIN_FILE_NAME_EXPORT 1)
        FILE(WRITE ${${${PACKAGE_NAME}_PARENT_REPOSITORY}_BINARY_DIR}/Testing/Requirements/TestTracking.txt
           "${TEST_NAME_OUT} ${TEST_INPUT}\n")
      ELSE()
        FILE(APPEND ${${${PACKAGE_NAME}_PARENT_REPOSITORY}_BINARY_DIR}/Testing/Requirements/TestTracking.txt
           "${TEST_NAME_OUT} ${TEST_INPUT}\n")
      ENDIF()

      MATH(EXPR INDXn "${INDX} + 1")
      SET(INDX ${INDXn})
   ENDWHILE()

ENDFUNCTION()

