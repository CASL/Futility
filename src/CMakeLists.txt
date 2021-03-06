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

TRIBITS_ADD_LIBRARY(CUtils
        HEADERS
          getSysProcInfo.h
          floatingpointchecks.h
        SOURCES
          getSysProcInfo.c
          floatingpointchecks.c
    )

ADD_SUBDIRECTORY(trilinos_interfaces)
ADD_SUBDIRECTORY(fmu_interfaces)

#Library for utility code
TRIBITS_ADD_LIBRARY(Utils
    HEADERS
      Futility_DBC.h
      UnitTest.h
      getSysProcInfo_F.h
    SOURCES
      Futility_DBC.f90
      UnitTest.f90
      IntrType.f90
      Search.f90
      Constants_Conversion.f90
      ExtendedMath.f90
      Interpolators.f90
      ElementsIsotopes.f90
      IAPWS_WaterProperties.f90
      WaterSatProperties.f90
      BLAS1.f90
      BLAS2.f90
      BLAS3.f90
      BLAS.f90
      FMU_Wrapper.f90
      trilinos_interfaces/trilinos_f_interfaces.f90
      Strings.f90
      Times.f90
      TAU_Stubs.f90
      MemProf.f90
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
      VTUFiles.f90
      IOutil.f90
      ParameterLists.f90
      CommandLineProcessor.f90
      MatrixTypes_Base.f90
      MatrixTypes_Native.f90
      MatrixTypes_PETSc.f90
      MatrixTypes_Trilinos.f90
      MatrixTypes.f90
      VectorTypes_Base.f90
      VectorTypes_Native.f90
      VectorTypes_PETSc.f90
      VectorTypes_Trilinos.f90
      VectorTypes.f90
      PreconditionerTypes.f90
      pcmg_supplement.c
      LinearSolverTypes.f90
      LinearSolverTypes_Multigrid.f90
      MultigridMesh.f90
      EigenvalueSolverTypes.f90
      ODESolverTypes.f90
      NonLinearSolver.f90
      SolutionAcceleration.f90
      AndersonAcceleration.f90
      StochasticSampling.f90
      Sorting.f90
      MeshTransfer.f90
      ArrayUtils.f90
      PartitionGraph.f90
      FutilityComputingEnvironment.f90
      SchemaParser.f90
      SpeciesElements.f90
    DEPLIBS
      CUtils
      TrilinosUtils
      FmuUtils
)

