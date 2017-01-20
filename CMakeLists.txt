################################################################################
#
# CMake/TriBITS File for MPACT
#
# Description: Defines the MPACT_Utils package. This package is the bulk of the
#              source for MPACT. Here we define the package name, source
#              directory and tests and examples.
#
# Author: Brendan Kochunas
#   Date: 08/29/2016
#
#################################################################################

# Set the package name to be used locally in this CMakeLists.txt file
SET(PNAME MPACT_Utils)
TRIBITS_PACKAGE_DECL(${PNAME})

# 
# Configure CMake/TriBITS options for package
#
# Add CMake option for enabling OpenMP
OPTION(${PNAME}_ENABLE_OpenMP 
    "Use compiler flags for OpenMP support."
    ${${PROJECT_NAME}_ENABLE_OpenMP}
)

# Define options for 64-bit reals and 64-bit integers
TRIBITS_ADD_OPTION_AND_DEFINE(
    ${PNAME}_ENABLE_DBLREAL
    DBL
    "Option for using double precision floating point reals as default reals."
    ON
)
TRIBITS_ADD_OPTION_AND_DEFINE(
    ${PNAME}_ENABLE_DBLINT
    DBLINT
    "Option for using 64-bit integers as the default integer."
    OFF
)

# Define Package dependent option for MPI TPL
IF(TPL_ENABLE_MPI)
    SET(${PNAME}_ENABLE_MPI_DEFAULT ON)
ELSE()
    SET(${PNAME}_ENABLE_MPI_DEFAULT OFF)
ENDIF()
TRIBITS_ADD_OPTION_AND_DEFINE(
    ${PNAME}_ENABLE_MPI
    HAVE_MPI
    "Enable use of MPI libraries."
    ${${PNAME}_ENABLE_MPI_DEFAULT}
)


# Define Package dependent option for PETSC TPL
IF(TPL_ENABLE_PETSC)
    SET(${PNAME}_ENABLE_PETSC_DEFAULT ON)
ELSE()
    SET(${PNAME}_ENABLE_PETSC_DEFAULT OFF)
ENDIF()
TRIBITS_ADD_OPTION_AND_DEFINE(
    ${PNAME}_ENABLE_PETSC
    MPACT_HAVE_PETSC
    "Enable use of PETSC libraries."
    ${${PNAME}_ENABLE_PETSC_DEFAULT}
)

# Define Package dependent option for SLEPC TPL
IF(TPL_ENABLE_SLEPC)
    SET(${PNAME}_ENABLE_SLEPC_DEFAULT ON)
ELSE()
    SET(${PNAME}_ENABLE_SLEPC_DEFAULT OFF)
ENDIF()
TRIBITS_ADD_OPTION_AND_DEFINE(
    ${PNAME}_ENABLE_SLEPC
    MPACT_HAVE_SLEPC
    "Enable use of SLEPC libraries."
    ${${PNAME}_ENABLE_SLEPC_DEFAULT}
)

MESSAGE("ForTeuchos enabled: ${${PNAME}_ENABLE_ForTeuchos} ${MPACT_ENABLE_ForTeuchos} ${PNAME}")
TRIBITS_ADD_OPTION_AND_DEFINE(
    ${PNAME}_ENABLE_ForTeuchos
    MPACT_HAVE_ForTeuchos
    "Enable use of ForTeuchos wrappers."
    OFF
)

# Define Package dependent option for HDF5 TPL
IF(TPL_ENABLE_HDF5)
    SET(${PNAME}_ENABLE_HDF5_DEFAULT ON)
ELSE()
    SET(${PNAME}_ENABLE_HDF5_DEFAULT OFF)
ENDIF()
TRIBITS_ADD_OPTION_AND_DEFINE(
    ${PNAME}_ENABLE_HDF5
    HAVE_HDF5
    "Enable use of HDF5 library."
    ${${PNAME}_ENABLE_HDF5_DEFAULT}
)

# Define Package dependent option for PARDISO TPL
IF(TPL_ENABLE_PARDISO)
    SET(${PNAME}_ENABLE_PARDISO_DEFAULT ON)
ELSE()
    SET(${PNAME}_ENABLE_PARDISO_DEFAULT OFF)
ENDIF()
TRIBITS_ADD_OPTION_AND_DEFINE(
    ${PNAME}_ENABLE_PARDISO
    HAVE_PARDISO
    "Enable use of PETSC libraries."
    ${${PNAME}_ENABLE_PARDISO_DEFAULT}
)

# Package dependent option for BLAS TPL
IF(TPL_ENABLE_BLAS OR TPL_ENABLE_MKL)
    SET(${PNAME}_ENABLE_BLAS_DEFAULT ON)
ELSE()
    SET(${PNAME}_ENABLE_BLAS_DEFAULT OFF)
ENDIF()
TRIBITS_ADD_OPTION_AND_DEFINE(
    ${PNAME}_ENABLE_BLAS
    HAVE_BLAS
    "Enable use of BLAS libraries."
    ${${PNAME}_ENABLE_BLAS_DEFAULT}
)

# Define Package dependent option for MKL TPL
IF(TPL_ENABLE_MKL)
    SET(${PNAME}_ENABLE_MKL_DEFAULT ON)
ELSE()
    SET(${PNAME}_ENABLE_MKL_DEFAULT OFF)
ENDIF()
TRIBITS_ADD_OPTION_AND_DEFINE(
    ${PNAME}_ENABLE_MKL
    HAVE_MKL
    "Enable use of MKL libraries."
    ${${PNAME}_ENABLE_MKL_DEFAULT}
)

# Define Package dependent option for PAPI TPL
IF(TPL_ENABLE_PAPI)
    SET(${PNAME}_ENABLE_PAPI_DEFAULT ON)
ELSE()
    SET(${PNAME}_ENABLE_PAPI_DEFAULT OFF)
ENDIF()
TRIBITS_ADD_OPTION_AND_DEFINE(
    ${PNAME}_ENABLE_PAPI
    HAVE_PAPI
    "Enable use of PAPI libraries."
    ${${PNAME}_ENABLE_PAPI_DEFAULT}
)

# Set compiler options for this package using the MPACT configuration.
# NOTE: It appears that this must come before the package name is defined.
#       These options are only defined for this package.
INCLUDE(${PNAME}_Compiler_Config)

#
#Enable test categories 
#
# IF ("${PROJECT_NAME}" STREQUAL "MPACT" AND ${PROJECT_NAME}_ENABLE_VALIDATION) 
#     SET(${PROJECT_NAME}_TEST_CATEGORIES ${${PROJECT_NAME}_TEST_CATEGORIES}
#                                          PERFORMANCE)
# ENDIF()

#
# Have TRIBITS process the package
# 
TRIBITS_PACKAGE_DEF()
ADD_SUBDIRECTORY(src)
TRIBITS_ADD_TEST_DIRECTORIES(unit_tests)
TRIBITS_ADD_EXAMPLE_DIRECTORIES(examples)

# Standard post-processing
TRIBITS_PACKAGE_POSTPROCESS()

# Clear locals
UNSET(PNAME)
