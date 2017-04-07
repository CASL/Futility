#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

FUNCTION(CHECK_LATEX_PACKAGES LATEX_EXE INPUT_FILE WORK_DIR RESULT_NAME)
  MESSAGE(STATUS "Checking for LaTeX packages..." )
  EXECUTE_PROCESS( COMMAND ${LATEX_EXE} ${INPUT_FILE}
                   TIMEOUT 5
                   OUTPUT_QUIET
                   ERROR_QUIET
                   WORKING_DIRECTORY ${WORK_DIR}
                   RESULT_VARIABLE HAS_LATEX_PACKAGES_LOCAL )
  IF( NOT ${HAS_LATEX_PACKAGES_LOCAL} STREQUAL "0")
    MESSAGE(STATUS "LaTeX packages NOT FOUND!")
  ENDIF()
  GLOBAL_SET( ${RESULT_NAME} ${HAS_LATEX_PACKAGES_LOCAL} )
  #GLOBAL_SET( HAS_LATEX_PACKAGES ${HAS_LATEX_PACKAGES_LOCAL} )
ENDFUNCTION()
