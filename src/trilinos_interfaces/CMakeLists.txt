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

TRIBITS_ADD_LIBRARY(TrilinosUtils
    HEADERS
      trilinos_mat_vec.hpp
      trilinos_solvers.hpp
      trilinos_pc.hpp
      trilinos_anderson.hpp
    SOURCES
      trilinos_mat_vec.cpp
      trilinos_pc.cpp
      trilinos_solvers.cpp
      trilinos_c_interfaces.cpp
)
