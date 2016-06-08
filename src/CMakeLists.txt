################################################################################
#
# Description: CMake script to add the MPACT_Utils library.
#
#
# Author: Derek Lax, Brendan Kochunas
#   Date: 06/05/2012
#
################################################################################

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

IF(WIN32)
    TRIBITS_ADD_LIBRARY(CUtils
        HEADERS
          getSysProcInfo.h
          floatingpointchecks.h
        SOURCES
          getSysProcInfo.c
          floatingpointchecks.c
        DEPLIBS Psapi
    )
ELSE()
    TRIBITS_ADD_LIBRARY(CUtils
        HEADERS
          getSysProcInfo.h
          floatingpointchecks.h
        SOURCES
          getSysProcInfo.c
          floatingpointchecks.c
    )
ENDIF()

TRIBITS_ADD_LIBRARY(TrilinosUtils
    HEADERS
      trilinos_interfaces/trilinos_mat_vec.hpp trilinos_interfaces/trilinos_solvers.hpp trilinos_interfaces/trilinos_pc.hpp
    SOURCES
      trilinos_interfaces/trilinos_c_interfaces.cpp
)

#Library for utility code
TRIBITS_ADD_LIBRARY(Utils
    HEADERS
      UnitTest.h
      getSysProcInfo_F.h
    SOURCES
      UnitTest.f90
      IntrType.f90
      Constants_Conversion.f90
      ExtendedMath.f90
      WaterSatProperties.f90
      BLAS1.f90
      BLAS2.f90
      BLAS3.f90
      BLAS.f90
      Strings.f90
      Times.f90
      TAU_Stubs.f90
      BinaryTrees.f90
      MortonOrdering.f90
      Geom_Points.f90
      Geom_Line.f90
      Geom_Plane.f90
      Geom_CircCyl.f90
      Geom_Box.f90
      Geom_Graph.f90
      Geom_Poly.f90
      Geom.f90
      ExceptionHandler.f90
      Allocs.f90
      ExpTables.f90
      ParallelEnv.f90
      IO_Strings.f90
      FileType_Base.f90
      FileType_Fortran.f90
      FileType_DA32.f90
      FileType_HDF5.f90
      FileType_XML.f90
      FileType_Log.f90
      FileType_Checkpoint.f90
      FileType_Input.f90
      VTKFiles.f90
      IOutil.f90
      ParameterLists.f90
      CommandLineProcessor.f90
      MatrixTypes.f90
      VectorTypes.f90
      PreconditionerTypes.f90
      LinearSolverTypes.f90
      EigenvalueSolverTypes.f90
      StochasticSampling.f90
      Sorting.f90
      ArrayUtils.f90
    DEPLIBS
      CUtils
      TrilinosUtils
)
