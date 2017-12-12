!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module provides 3 types that help define a hierachy of meshes in a
!>    multigrid context.  It is used to help define the interpolation operators
!>    for a multigrid method.  Note that the meshes should be thought of as
!>    points rather than cells.  However, it is generally okay to treat cells
!>    as points when applying a multigrid method that is not fully geometric.
!>
!> For valid reference lists
!> see @ref MatrixTypes::LinearSolverTypes_Declare_ValidParams
!> "LinearSolverTypes_Declare_ValidParams".
!>
!> Currently supported TPLs include:
!>  - PETSc (with interfaces to KSP)
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref BLAS "BLAS": @copybrief BLAS
!>  - @ref Times "Times": @copybrief Times
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref Allocs "Allocs": @copybrief Allocs
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref ParallelEnv "ParallelEnv": @copybrief ParallelEnv
!>  - @ref VectorTypes "VectorTypes": @copybrief VectorTypes
!>  - @ref MatrixTypes "MatrixTypes": @copybrief MatrixTypes
!>  - @ref LinearSolverTypes "LinearSolverTypes": @copybrief LinearSolverTypes
!>
!> @par EXAMPLES
!> @code
!>
!> @endcode
!>
!> @author Ben C. Yee
!>   @date 09/11/2017
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

MODULE MultigridMesh
  USE IntrType
  USE ExceptionHandler
  USE ParameterLists
  USE VectorTypes
  USE MatrixTypes
  USE LinearSolverTypes
  IMPLICIT NONE
  PRIVATE

  !PUBLIC :: MultigridMeshElementType
  PUBLIC :: MultigridMeshType
  PUBLIC :: MultigridMeshStructureType
  PUBLIC :: InterpWeightsLevelType
  PUBLIC :: InterpWeightsStructureType

  TYPE :: MultigridMeshElementType
    !> Index of the point on the finest grid that this point eventually injects
    !>  into.  May have another use/definition in cases where the interpolation
    !>  operator has no injections.  This is not needed for fillInterpMatrices
    !>  in LinearSolverTypes_Multigrid, it is only here for the user's
    !>  convenience and using/setting this variable is optional.
    INTEGER(SIK) :: finestGridIndex=-1_SIK
    !> For degree 0 points, childIndices is a size-1 array containing the
    !>  child mesh index of the point that it injects into.
    !> For degree >0 points, childIndices is a size >1 array containing the
    !>  current mesh indices of the points from which the value on this point
    !>  is interpolated.
    !> For MPACT, ordering should be WSENBT on finest mesh and WESNBT on
    !>  all other meshes, but, in general, ordering is arbitrary
    INTEGER(SIK),ALLOCATABLE :: childIndices(:)

    CONTAINS
      !> @copybrief MultigridMesh::clear_MultigridMeshElement
      !> @copydetails MultigridMesh::clear_MultigridMeshElement
      PROCEDURE,PASS :: clear => clear_MultigridMeshElement
  ENDTYPE MultigridMeshElementType

  TYPE :: MultigridMeshType
    !> Which level the mesh corresponds to (1 = coarsest)
    INTEGER(SIK) :: iLevel=-1_SIK
    !> Whether or not it is the finest level:
    LOGICAL(SBK) :: isFinestLevel=.FALSE.
    !> Whether or not it is the coarsest level:
    !> NOTE: On the coarsest level, mmData, interpDegrees, and xyzMap need
    !>  not be allocated!
    LOGICAL(SBK) :: isCoarsestLevel=.FALSE.
    !> Number of cells/points on this level locally:
    INTEGER(SIK) :: nPointsLocal=-1_SIK
    !> Local starting index of mesh points:
    INTEGER(SIK) :: istt=-1_SIK
    !> Local end index of mesh points:
    INTEGER(SIK) :: istp=-1_SIK
    !> Global x,y,z location of each element in units of the finest mesh
    !> This means that adjacent cells in coarser meshes do not have
    !>   adjacent x/y/z values.  Range is (3,istt:istp)
    INTEGER(SIK),ALLOCATABLE :: xyzMap(:,:)
    !> Data for the individual coarse mesh elements. indices are istt:istp
    TYPE(MultigridMeshElementType),ALLOCATABLE :: mmData(:)
    !> How many "degrees" each point is from a coarse point.
    !>  0 = corresponds to a point on the coarse grid
    !>  1 = between two 0-th degree points
    !>  2 = between 4 1st degree points, should only exist for 2D/3D problems
    !>  3 = between 6 2nd degree points, should only exist for 3D problems
    !>  size of mmData(i)%childIndices or mmData(i)%childWeights is
    !>    MAX(1,2*interpDegrees(i))
    !>  indices of interpDegrees should be istt:istp
    !> Ideally, this would be stored in MultigridMeshElementType, but having
    !>  it stored as an array is really handy for PETSc's dnnz arguments.
    INTEGER(SIK),ALLOCATABLE :: interpDegrees(:)
    !TODO Generalize this logic beyond structured Cartesian grids.
    !  Actually, I think it should work as is for unstructured grids.
    !TODO allow for interpolation across processors?  Not needed unless we
    !  encounter a problem where we want to restrict a grid with very few
    !  spatial points per processor.  At that point though, you might want to
    !  gather the data onto fewer processors and continue from there...

    CONTAINS
      !> @copybrief MultigridMesh::clear_MultigridMesh
      !> @copydetails MultigridMesh::clear_MultigridMesh
      PROCEDURE,PASS :: clear => clear_MultigridMesh
  ENDTYPE MultigridMeshType

  TYPE :: MultigridMeshStructureType
    !> Number of multigrid levels, including the coarsest and finest
    INTEGER(SIK) :: nLevels=-1_SIK
    !> Data for each multigrid level. 1 = coarsest level, nLevels = finest level
    TYPE(MultigridMeshType),ALLOCATABLE :: meshes(:)
    !> Whether or not the mesh structure has been initialized:
    LOGICAL(SBK) :: isInit

    CONTAINS
      !> @copybrief MultigridMesh::clear_MultigridMeshStructure
      !> @copydetails MultigridMesh::clear_MultigridMeshStructure
      PROCEDURE,PASS :: clear => clear_MultigridMeshStructure
      !> @copybrief MultigridMesh::init_MultigridMeshStructure
      !> @copydetails MultigridMesh::init_MultigridMeshStructure
      PROCEDURE,PASS :: init => init_MultigridMeshStructure
  ENDTYPE MultigridMeshStructureType

  TYPE :: InterpWeightsPointType
    !> Array of weights, size (num_eqns,nChildren)
    REAL(SRK),ALLOCATABLE :: wts(:,:)
  ENDTYPE InterpWeightsPointType

  TYPE :: InterpWeightsLevelType
    !> Pointer to MultigridMeshType for which these weights are used
    TYPE(MultigridMeshType),POINTER :: meshes => NULL()
    !> Pointer to array of InterpWeightsPointType
    !>  Bounds are meshes(iLevel)%istt:istp
    TYPE(InterpWeightsPointType),ALLOCATABLE :: wts_point(:)
    !> Number of equations (# of components per spatial point)
    !>   Right now, there is no support for simultaneous collapse in both space
    !>   and the number of eqns in this module.  Collapse of eqns must be done
    !>   separately.
    INTEGER(SIK) :: num_eqns=1_SIK

    CONTAINS
      !> @copybrief MultigridMesh::clear_InterpWeightsLevel
      !> @copydetails MultigridMesh::clear_InterpWeightsLevel
      PROCEDURE,PASS :: clear => clear_InterpWeightsLevel
      !> @copybrief MultigridMesh::init_InterpWeightsLevel
      !> @copydetails MultigridMesh::init_InterpWeightsLevel
      PROCEDURE,PASS :: init => init_InterpWeightsLevel

  ENDTYPE InterpWeightsLevelType

  TYPE :: InterpWeightsStructureType
    !> Pointer to MultigridMeshStructureType for which these weights are used
    TYPE(MultigridMeshStructureType),POINTER :: myMeshes => NULL()
    !> Pointer to array of InterpWeightsLevelType, 2:myMMeshes%nLevels
    !>   wts_level(i) has weights for interpolation from level i-1 to level i
    !>   1 = coarsest, nLevels = finest level
    TYPE(InterpWeightsLevelType),ALLOCATABLE :: wts_level(:)
    !> Whether or not the interp weights structure has been initialized:
    LOGICAL(SBK) :: isInit

    CONTAINS
      !> @copybrief MultigridMesh::clear_InterpWeightsStructure
      !> @copydetails MultigridMesh::clear_InterpWeightsStructure
      PROCEDURE,PASS :: clear => clear_InterpWeightsStructure
      !> @copybrief MultigridMesh::init_InterpWeightsStructure
      !> @copydetails MultigridMesh::init_InterpWeightsStructure
      PROCEDURE,PASS :: init => init_InterpWeightsStructure

  ENDTYPE InterpWeightsStructureType

  !> Exception Handler for use in MatrixTypes
  TYPE(ExceptionHandlerType),SAVE :: eMultigridMesh

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='MULTIGRIDMESH'
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the MultigridMeshStructure type
!>
!> @param myMeshes the mesh structure
!> @param nLevels number of levels
!>
    SUBROUTINE init_MultigridMeshStructure(myMeshes,nLevels,num_eqns)
      CHARACTER(LEN=*),PARAMETER :: myName='init_MultigridMeshStructure'
      CLASS(MultigridMeshStructureType),INTENT(INOUT) :: myMeshes
      INTEGER(SIK),INTENT(IN) :: nLevels
      INTEGER(SIK),INTENT(IN),OPTIONAL :: num_eqns

      INTEGER(SIK) :: iLevel

      IF(nLevels < 1_SIK) &
        CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
          "nLevels must be a positive integer!")

      myMeshes%nLevels=nLevels
      ALLOCATE(myMeshes%meshes(nLevels))
      DO iLevel=1,nLevels
        myMeshes%meshes(iLevel)%iLevel=iLevel
      ENDDO
      myMeshes%meshes(nLevels)%isFinestLevel=.TRUE.
      myMeshes%meshes(1)%isCoarsestLevel=.TRUE.

      myMeshes%isInit=.TRUE.

    ENDSUBROUTINE init_MultigridMeshStructure
!
!-------------------------------------------------------------------------------
!> @brief Clears the MultigridMeshStructure type
!>
!> @param myMeshes the mesh structure
!>
    SUBROUTINE clear_MultigridMeshStructure(myMeshes)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_MultigridMeshStructure'
      CLASS(MultigridMeshStructureType),INTENT(INOUT) :: myMeshes
      INTEGER(SIK) :: iLevel

      IF(ALLOCATED(myMeshes%meshes)) THEN
        DO iLevel=1,myMeshes%nLevels-1
          CALL myMeshes%meshes(iLevel)%clear()
        ENDDO
        DEALLOCATE(myMeshes%meshes)
      ENDIF
      myMeshes%nLevels=0
      myMeshes%isInit=.FALSE.

    ENDSUBROUTINE clear_MultigridMeshStructure
!
!-------------------------------------------------------------------------------
!> @brief Clears the MultigridMesh type.  Should only be called via
!>          clear_MultigridMeshStructure
!>
!> @param myMesh the mesh
!>
    SUBROUTINE clear_MultigridMesh(myMesh)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_MultigridMesh'
      CLASS(MultigridMeshType),INTENT(INOUT) :: myMesh
      INTEGER(SIK) :: i

      IF(ALLOCATED(myMesh%interpDegrees)) DEALLOCATE(myMesh%interpDegrees)

      IF(ALLOCATED(myMesh%xyzMap)) DEALLOCATE(myMesh%xyzMap)

      IF(ALLOCATED(myMesh%mmData)) THEN
        DO i=myMesh%istt,myMesh%istp
          CALL myMesh%mmData(i)%clear()
        ENDDO
        DEALLOCATE(myMesh%mmData)
      ENDIF

    ENDSUBROUTINE clear_MultigridMesh
!
!-------------------------------------------------------------------------------
!> @brief Clears the MultigridMeshElement type.  Should only be called via
!>          clear_MultigridMesh
!>
!> @param myMeshElement the mesh element
!>
    SUBROUTINE clear_MultigridMeshElement(myMeshElement)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_MultigridMeshElement'
      CLASS(MultigridMeshElementType),INTENT(INOUT) :: myMeshElement

      IF(ALLOCATED(myMeshElement%childIndices)) &
              DEALLOCATE(myMeshElement%childIndices)

    ENDSUBROUTINE clear_MultigridMeshElement
!
!-------------------------------------------------------------------------------
!> @brief Initializes the InterpWeightsStructure type
!>
!> @param myWtStructure the interp weight structure instance
!> @param myMeshes pointer to initialized MultigridMeshStructureType
!> @param num_eqns number of equations (# of unknowns per spatial point)
!>
    SUBROUTINE init_InterpWeightsStructure(myWtStructure,myMeshes,num_eqns)
      CHARACTER(LEN=*),PARAMETER :: myName='init_InterpWeightsStructure'
      CLASS(InterpWeightsStructureType),INTENT(INOUT) :: myWtStructure
      TYPE(MultigridMeshStructureType),POINTER,INTENT(IN) :: myMeshes
      INTEGER(SIK),INTENT(IN) :: num_eqns

      INTEGER(SIK) :: iLevel

      IF(.NOT. myMeshes%isInit) &
        CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
          "input MultigridMeshStructure must be initialized!")

      myWtStructure%myMeshes => myMeshes

      ALLOCATE(myWtStructure%wts_level(2:myMeshes%nLevels))
      DO iLevel=2,myMeshes%nLevels
        CALL myWtStructure%wts_level(iLevel)%init( &
               myMeshes%meshes(iLevel),num_eqns)
      ENDDO

      myWtStructure%isInit=.TRUE.

    ENDSUBROUTINE init_InterpWeightsStructure
!
!-------------------------------------------------------------------------------
!> @brief Clears the InterpWeightsStructure type
!>
!> @param myWtStructure the interp weight structure instance
!>
    SUBROUTINE clear_InterpWeightsStructure(myWtStructure)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_InterpWeightsStructure'
      CLASS(InterpWeightsStructureType),INTENT(INOUT) :: myWtStructure

      INTEGER(SIK) :: iLevel

      IF(ASSOCIATED(myWtStructure%myMeshes) .AND. &
          myWtStructure%myMeshes%isInit) THEN
        IF(ALLOCATED(myWtStructure%wts_level)) THEN
          DO iLevel=2,myWtStructure%myMeshes%nLevels
            CALL myWtStructure%wts_level(iLevel)%clear()
          ENDDO
          DEALLOCATE(myWtStructure%wts_level)
        ENDIF
        NULLIFY(myWtStructure%myMeshes)
      ELSE IF(ALLOCATED(myWtStructure%wts_level)) THEN
        DO iLevel=LBOUND(myWtStructure%wts_level,1), &
                  UBOUND(myWtStructure%wts_level,1)
          NULLIFY(myWtStructure%wts_level(iLevel)%meshes)
          CALL myWtStructure%wts_level(iLevel)%clear()
        ENDDO
        DEALLOCATE(myWtStructure%wts_level)
      ENDIF

      myWtStructure%isInit=.FALSE.

    ENDSUBROUTINE clear_InterpWeightsStructure
!
!-------------------------------------------------------------------------------
!> @brief Initializes the InterpWeightsLevel type
!>
!> @param myWts the interp weight level instance
!> @param meshes pointer to MultigridMeshType corresponding to this level
!> @param num_eqns number of equations (# of unknowns per spatial point)
!>
    SUBROUTINE init_InterpWeightsLevel(myWts,meshes,num_eqns)
      CHARACTER(LEN=*),PARAMETER :: myName='init_InterpWeightsLevel'
      CLASS(InterpWeightsLevelType),INTENT(INOUT) :: myWts
      TYPE(MultigridMeshType),POINTER,INTENT(IN) :: meshes
      INTEGER(SIK),INTENT(IN) :: num_eqns

      INTEGER(SIK) :: i

      myWts%meshes => meshes
      myWts%num_eqns=num_eqns

      !TODO turn on option for flat weights to save on memory cost
      ALLOCATE(myWts%wts_point(meshes%istt:meshes%istp))
      DO i=meshes%istt,meshes%istp
        IF(meshes%interpDegrees(i) > 0) THEN
          ALLOCATE(myWts%wts_point(i)%wts(num_eqns,meshes%interpDegrees(i)*2))
          myWts%wts_point(i)%wts=1.0_SRK/(2*meshes%interpDegrees(i))
        ENDIF
      ENDDO

    ENDSUBROUTINE init_InterpWeightsLevel
!
!-------------------------------------------------------------------------------
!> @brief Clears the InterpWeightsLevel type
!>
!> @param myWts the interp weight level instance
!>
    SUBROUTINE clear_InterpWeightsLevel(myWts)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_InterpWeightsLevel'
      CLASS(InterpWeightsLevelType),INTENT(INOUT) :: myWts

      INTEGER(SIK) :: i

      IF(ASSOCIATED(myWts%meshes)) THEN
        IF(ALLOCATED(myWts%wts_point)) THEN
          DO i=myWts%meshes%istt,myWts%meshes%istp
            IF(myWts%meshes%interpDegrees(i) > 0) THEN
              DEALLOCATE(myWts%wts_point(i)%wts)
            ENDIF
          ENDDO
          DEALLOCATE(myWts%wts_point)
        ENDIF
        NULLIFY(myWts%meshes)
      ELSE IF(ALLOCATED(myWts%wts_point)) THEN
        DO i=LBOUND(myWts%wts_point,1),UBOUND(myWts%wts_point,1)
          IF(ALLOCATED(myWts%wts_point(i)%wts)) THEN
            DEALLOCATE(myWts%wts_point(i)%wts)
          ENDIF
        ENDDO
        DEALLOCATE(myWts%wts_point)
      ENDIF

    ENDSUBROUTINE clear_InterpWeightsLevel

ENDMODULE MultigridMesh
