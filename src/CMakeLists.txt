################################################################################
#
# CMake File for MPACT
#
# Description: CMake script to add the MPACT_Utils library.
#              
#
# Author: Derek Lax, Brendan Kochunas
#   Date: 06/05/2012
#
################################################################################

INCLUDE(TribitsLibraryMacros)
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

#Library for utility code
TRIBITS_ADD_LIBRARY(Utils
    HEADERS
	  UnitTest_harness.h
    SOURCES
      #UnitTest.f90
      IntrType.f90
      Strings.f90
      Times.f90
      BinaryTrees.f90
      MortonOrdering.f90
      ExceptionHandler.f90
      Allocs.f90
      ParallelEnv.f90
      IO_Strings.f90
      FileType_Base.f90
      FileType_Fortran.f90
      FileType_Log.f90
      FileType_Input.f90
      VTKFiles.f90
      IOutil.f90
      ParameterLists.f90
      CommandLineProcessor.f90
      Geom_Points.f90
      Geom_Line.f90
      Geom_Plane.f90
      Geom_CircCyl.f90
      Geom_Box.f90
      Geom.f90
      ExpTables.f90
      BLAS1.f90
      BLAS2.f90
      BLAS3.f90
      BLAS.f90
      MatrixTypes.f90
      VectorTypes.f90
      LinearSolverTypes.f90
      StochasticSampling.f90
)
