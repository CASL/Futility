################################################################################
#
# CMake/TriBITS File for MPACT
#
# Description: Defines the dependencies for the MPACT_libs package. Presently
#              there are no required dependencies on this library. Some
#              optional dependencies will be TPLS for: BLAS, LAPACK, MKL, MPI,
#              HDF5, SILO, PETSc, some Trilinos packages.
#
# Author: Derek Lax, Ben Collins, Brendan Kochunas
#   Date: 05/29/2012
#
################################################################################

SET(LIB_REQUIRED_DEP_PACKAGES)
SET(LIB_OPTIONAL_DEP_PACKAGES)
SET(TEST_REQUIRED_DEP_PACKAGES)
SET(TEST_OPTIONAL_DEP_PACKAGES)
SET(LIB_REQUIRED_DEP_TPLS)
SET(LIB_OPTIONAL_DEP_TPLS BLAS MPI PETSC PARDISO HDF5)
SET(TEST_REQUIRED_DEP_TPLS)
SET(TEST_OPTIONAL_DEP_TPLS MPI)
