#!/bin/bash

# Set the source directory
SOURCE_DIR=
if [ -n "$1"  ]; then
 SOURCE_DIR="$1"
 echo "Source directory is ${SOURCE_DIR}"
 shift
 cmake -Wno-dev                                  \
 -DCMAKE_BUILD_TYPE:STRING="DEBUG"\
 -DCMAKE_Fortran_COMPILER=mpif90\
 -DCMAKE_C_COMPILER=mpicc \
 -DCMAKE_CXX_COMPILER=mpicxx\
 -DFUTILITY_ENABLE_ALL_CODE:BOOL=ON \
 -DFUTILITY_ENABLE_TESTS:BOOL=ON\
 -DFUTILITY_ENABLE_DOCS:BOOL=TRUE \
 -DFUTILITY_INLINE_EXPT:BOOL=OFF\
 -DFUTILITY_ENABLE_OpenMP:BOOL=TRUE \
 -DFUTILITY_DEBUG_MSG:BOOL=TRUE \
 -DTPL_ENABLE_MPI:BOOL=ON \
 "$@"\
 ${SOURCE_DIR}
else
 echo "Build script requires argument for package source directory"
fi