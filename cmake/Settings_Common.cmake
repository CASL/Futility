#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

# Set PROJECT_NAME (must be in file for other parts of system)
INCLUDE("${CMAKE_CURRENT_SOURCE_DIR}/ProjectName.cmake")

# Set MPACT Project defaults for TriBITS
SET(${PROJECT_NAME}_ENABLE_Fortran ON CACHE BOOL
    "${PROJECT_NAME} always requires Fortran!" FORCE) #Always Fortran
SET(${PROJECT_NAME}_DEFAULT_BUILD_TYPE DEBUG)         #Default build type
IF(NOT DEFINED ${PROJECT_NAME}_ENABLE_TESTS)
  SET(${PROJECT_NAME}_ENABLE_TESTS ON)
ENDIF()
IF(NOT DEFINED ${PROJECT_NAME}_BUILD_NOSTANDARD)
  SET(${PROJECT_NAME}_BUILD_NOSTANDARD FALSE)
ENDIF()
#Set Repo Version File on
IF(NOT DEFINED ${PROJECT_NAME}_GENERATE_REPO_VERSION_FILE)
  SET(${PROJECT_NAME}_GENERATE_REPO_VERSION_FILE ON CACHE BOOL "")
ENDIF()

# Don't install under /usr/local by default (that is crazy)
IF (NOT EXISTS ${CMAKE_INSTALL_PREFIX} OR ${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/local")
  SET(CMAKE_INSTALL_PREFIX ${CMAKE_CURRENT_BINARY_DIR}/install CACHE PATH
  "Base install directory (overridde default from \"/usr/local\")"
  FORCE)
ENDIF()

#Set main project source and binary directories
#These variables get used in configuring inputs to doxygen and code coverage stuff
SET(${PROJECT_NAME}_MAIN_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR} CACHE INTERNAL "")
SET(${PROJECT_NAME}_MAIN_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR} CACHE INTERNAL "")
SET(${PROJECT_NAME}_ROOT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR} CACHE INTERNAL "")
SET(${PROJECT_NAME}_ROOT_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR} CACHE INTERNAL "")

#Fixing issues with Trilinos dependencies
IF(DEFINED ${PROJECT_NAME}_ENABLE_Teuchos)
  IF("${${PROJECT_NAME}_ENABLE_Teuchos}" STREQUAL "OFF")
    SET(${PROJECT_NAME}_ENABLE_ML OFF)
    SET(${PROJECT_NAME}_ENABLE_Triutils OFF)
  ENDIF()
ENDIF()

# CMake requires that you declare the CMake project in the top-level file and
# not in an include file :-(
PROJECT(${PROJECT_NAME} NONE)

# Pull in the TriBITS system and execute
IF(NOT DEFINED(${${PROJECT_NAME}_TRIBITS_DIR}))
  SET(${PROJECT_NAME}_TRIBITS_DIR
      "${${PROJECT_NAME}_MAIN_SOURCE_DIR}/Futility/cmake/tribits" CACHE INTERNAL ""
  )
ENDIF()

# Include the modules in TriBITS
SET(CMAKE_MODULE_PATH "${${PROJECT_NAME}_TRIBITS_DIR}/package_arch" CACHE INTERNAL "")
INCLUDE(${${PROJECT_NAME}_TRIBITS_DIR}/TriBITS.cmake)

# Set build type to default
IF(NOT CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE ${${PROJECT_NAME}_DEFAULT_BUILD_TYPE} CACHE STRING "" FORCE)
ENDIF()

IF(WIN32)
  SET(MPACT_GENERATE_REPO_VERSION_FILE OFF)
  SET(TPL_ENABLE_Psapi ON)
ENDIF()

#Set DART_TESTING_TIMEOUT so it gets properly scaled by TriBITS and default
#from CMake installed module CTest.cmake is not set after scaling.
IF(NOT DEFINED DART_TESTING_TIMEOUT)
  MESSAGE(STATUS "${PROJECT_NAME}: Setting default DART_TESTING_TIMEOUT=1500")
  SET(DART_TESTING_TIMEOUT 1500 CACHE STRING "Set by Futility top level Setting_Common.cmake")
ENDIF()
