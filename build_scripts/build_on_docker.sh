#!/bin/bash
TPL_DIR=/tools/vera/gcc-5.4.0/tpls/opt

BLAS_LIBRARY_DIRS=${TPL_DIR}/lapack-3.3.1/lib
LAPACK_LIBRARY_DIRS=${TPL_DIR}/lapack-3.3.1/lib

PETSC_DIR=${TPL_DIR}/petsc-3.5.4
SLEPC_DIR=${TPL_DIR}/slepc-3.5.4
HYPRE_DIR=${TPL_DIR}/hypre-2.9.1a
SILO_DIR=${TPL_DIR}/silo-4.10.2
PETSC_INCLUDE_DIRS="${PETSC_DIR}/include;${HYPRE_DIR}/include"
PETSC_LIBRARY_DIRS="${PETSC_DIR}/lib;${HYPRE_DIR}/lib"
PETSC_LIBRARY_NAMES="petsc;HYPRE"
SLEPC_INCLUDE_DIRS="${SLEPC_DIR}/include"
SLEPC_LIBRARY_DIRS="${SLEPC_DIR}/lib"

HDF5_LIBRARY_NAMES="hdf5_hl;hdf5;hdf5_cpp;hdf5_fortran"
HDF5_INCLUDE_DIRS=${TPL_DIR}/hdf5-1.8.10/include
HDF5_LIBRARY_DIRS=${TPL_DIR}/hdf5-1.8.10/lib
SILO_INCLUDE_DIRS="${SILO_DIR}/include"
SILO_LIBRARY_DIRS="${SILO_DIR}/lib"

cmake -Wno-dev                                  \
 -DCMAKE_BUILD_TYPE:STRING="RELEASE"            \
 -DCMAKE_Fortran_COMPILER=mpif90                \
 -DCMAKE_C_COMPILER=mpicc                       \
 -DCMAKE_CXX_COMPILER=mpicxx                    \
 -DFutility_ENABLE_TESTS:BOOL=ON                \
 -DFutility_ENABLE_DOCS:BOOL=TRUE               \
 -DTPL_ENABLE_MPI:BOOL=ON                       \
 -DTPL_ENABLE_PETSC:BOOL=ON                     \
 -DFutility_BUILD_STANDARD=OFF                  \
 -DTPL_ENABLE_BLAS=ON   -DBLAS_LIBRARY_DIRS:FILENAME=${BLAS_LIBRARY_DIRS}      \
 -DTPL_ENABLE_LAPACK=ON -DLAPACK_LIBRARY_DIRS:FILEPATH=${LAPACK_LIBRARY_DIRS}  \
 -DTPL_ENABLE_HDF5=ON   -DHDF5_LIBRARY_DIRS:FILEPATH=${HDF5_LIBRARY_DIRS}      \
                        -DHDF5_INCLUDE_DIRS:FILEPATH=${HDF5_INCLUDE_DIRS}      \
                        -DHDF5_LIBRARY_NAMES:STRING=${HDF5_LIBRARY_NAMES}      \
 -DTPL_ENABLE_SILO=ON   -DSILO_LIBRARY_DIRS:FILEPATH=${SILO_LIBRARY_DIRS}      \
                        -DSILO_INCLUDE_DIRS:FILEPATH=${SILO_INCLUDE_DIRS}      \
                        -DPETSC_LIBRARY_DIRS:FILEPATH=${PETSC_LIBRARY_DIRS}    \
                        -DPETSC_INCLUDE_DIRS:FILEPATH=${PETSC_INCLUDE_DIRS}    \
                        -DPETSC_LIBRARY_NAMES:STRING=${PETSC_LIBRARY_NAMES}    \
 -DTPL_ENABLE_SLEPC=ON  -DSLEPC_LIBRARY_DIRS:FILEPATH=${SLEPC_LIBRARY_DIRS}    \
                        -DSLEPC_INCLUDE_DIRS:FILEPATH=${SLEPC_INCLUDE_DIRS}    \
 $@
