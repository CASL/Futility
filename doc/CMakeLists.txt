#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

# Prepare doxygen input file
IF( NOT DOXYGEN_DOT_FOUND )
  SET( DOXYGEN_DOT_FOUND "NO" )
ENDIF()
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME}.doxy.in
               ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.doxy
)

# Create target for documentation
SET( DOXYGEN_MIN_VERSION "1.7.6.1" )
IF(DOXYGEN_EXECUTABLE AND ${DOXYGEN_DOT_FOUND} STREQUAL "YES" AND NOT DOXYGEN_VERSION VERSION_LESS DOXYGEN_MIN_VERSION)
  ADD_CUSTOM_TARGET("Gen_Doc"
    COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.doxy
    #COMMAND ${DOXYGEN_DOT_EXECUTABLE} -Tpng ${CMAKE_CURRENT_SOURCE_DIR}/module_dependencies.dot
    #     -o ${CMAKE_CURRENT_BINARY_DIR}/documentation/html/module_dependencies.png
  )
ELSE()
  ADD_CUSTOM_TARGET("Gen_Doc"
    COMMAND echo "Functionality not present in current environment.  Doxygen ${DOXYGEN_MIN_VERSION}+ exexecutable not found or doxygen_dot not found.  Files in ${CMAKE_CURRENT_BINARY_DIR}/doc/documentation"
  )
ENDIF()

# Create target to clean documentation
IF(WIN32 AND NOT CYGWIN)
  ADD_CUSTOM_TARGET("Clean_Doc"
    COMMAND cd ${CMAKE_CURRENT_BINARY_DIR}/documentation
    COMMAND del *
  )
ELSE()
  ADD_CUSTOM_TARGET("Clean_Doc"
    COMMAND cd ${CMAKE_CURRENT_BINARY_DIR}/documentation
    COMMAND rm -rf *
  )
ENDIF()