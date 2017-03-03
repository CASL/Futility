#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#                          Futility Development Group                          !
#                             All rights reserved.                             !
#                                                                              !
# Futility is a jointly-maintained, open-source project between the University !
# of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
# can be found in LICENSE.txt in the head directory of this repository.        !
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

SET(PNAME Futility)
TRIBITS_PACKAGE(${PNAME})

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
    FUTILITY_HAVE_PETSC
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
    FUTILITY_HAVE_SLEPC
    "Enable use of SLEPC libraries."
    ${${PNAME}_ENABLE_SLEPC_DEFAULT}
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

# Set compiler options for this package using the Futility configuration.
# NOTE: It appears that this must come before the package name is defined.
#       These options are only defined for this package.
INCLUDE(${PNAME}_Compiler_Config)

#
#Enable test categories
#

#
# Have TRIBITS process the package
#
ADD_SUBDIRECTORY(src)
TRIBITS_ADD_TEST_DIRECTORIES(unit_tests)
TRIBITS_ADD_EXAMPLE_DIRECTORIES(examples)

TRIBITS_PACKAGE_POSTPROCESS()
UNSET(PNAME)
