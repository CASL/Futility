#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

TRIBITS_ADD_LIBRARY(testFMUBouncingBall SHARED
    HEADERS
      fmu2_model.h
      fmu2_model_defines.h
      ${Futility_SOURCE_DIR}/src/fmu_interfaces/fmi2Functions.h
      ${Futility_SOURCE_DIR}/src/fmu_interfaces/fmi2FunctionTypes.h
      ${Futility_SOURCE_DIR}/src/fmu_interfaces/fmi2TypesPlatform.h
    SOURCES
      fmu2_model_cs.c
)

set_target_properties(testFMUBouncingBall PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/binaries/linux64
    PREFIX "")

# List any files from the main source directory that the test depends on
SET(ADDITIONAL_FILES
    modelDescription.xml
)
Futility_CopyFiles(ADDITIONAL_FILES)
UNSET(ADDITIONAL_FILES)
