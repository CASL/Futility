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

TRIBITS_ADD_LIBRARY(FmuUtils
    HEADERS
      FMU.h
      FMU2.h
      fmi2FunctionTypes.h
      fmi2Functions.h
      fmi2TypesPlatform.h
      FMU2_c_api.cpp
      FMU2_c_api.h
      FMU2_f_api.f90
    SOURCES
      FMU.cpp
      FMU2.cpp
)

MESSAGE("-- Enabling Futility FMU Wrappers. CMAKE_DL_LIBS: " ${CMAKE_DL_LIBS})
target_link_libraries(FmuUtils ${CMAKE_DL_LIBS})

