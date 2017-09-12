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

  PUBLIC :: MultigridMeshElementType
  PUBLIC :: MultigridMeshType
  PUBLIC :: MultigridMeshStructureType

  TYPE :: MultigridMeshElementType
    !> Number of local neighbors
    INTEGER(SIK) :: nNeighLocal
    !> Global indices of its local neighbors on the current multigrid mesh.
    !>  Size nNeigh, should be ordered WSNEBT where possible
    INTEGER(SIK),ALLOCATABLE :: neighLocal(:)
    !> Indices of the points on the child mesh, see description of interpDegree
    !>  in MultigridMeshType for information about the size of the array
    INTEGER(SIK),ALLOCATABLE :: childIndices(:)
    !> Weights of each child point, same range as childIndices(:)
    REAL(SRK),ALLOCATABLE :: childWeights(:)

    CONTAINS
      !> @copybrief MultigridMesh::clear_MultigridMeshElement
      !> @copydetails MultigridMesh::clear_MultigridMeshElement
      PROCEDURE,PASS :: clear => clear_MultigridMeshElement
  ENDTYPE MultigridMeshElementType

  TYPE :: MultigridMeshType
    !> Which level the mesh corresponds to (0 = coarsest)
    INTEGER(SIK) :: iLevel
    !> Whether or not it is the finest level:
    LOGICAL(SBK) :: isFinestLevel
    !> Number of cells/points on this level locally:
    INTEGER(SIK) :: nPointsLocal
    !> Local starting index of mesh points:
    INTEGER(SIK) :: istt
    !> Local end index of mesh points:
    INTEGER(SIK) :: istp
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
    INTEGER(SIK) :: nLevels
    !> Data for each multigrid level.  NOTE: Typically 1:nLevels-1.
    !>  It is generally not necessary to define a MultigridMesh for the
    !>  coarsest level.  0 = coarsest level, nLevels-1 = finest level.  The
    !>  0-based index is consistent with PETSc indexing of multigrid levels.
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
    SUBROUTINE init_MultigridMeshStructure(myMeshes,nLevels)
      CHARACTER(LEN=*),PARAMETER :: myName='init_MultigridMeshStructure'
      CLASS(MultigridMeshStructureType),INTENT(INOUT) :: myMeshes
      INTEGER(SIK),INTENT(IN) :: nLevels

      myMeshes%nLevels=nLevels
      myMeshes%isInit=.TRUE.
      ALLOCATE(myMeshes%meshes(nLevels-1))
      myMeshes%meshes(nLevels-1)%isFinestLevel=.TRUE.

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
      CHARACTER(LEN=*),PARAMETER :: myName='clear_MultigridMesh'
      CLASS(MultigridMeshElementType),INTENT(INOUT) :: myMeshElement

      IF(ALLOCATED(myMeshElement%neighLocal)) &
              DEALLOCATE(myMeshElement%neighLocal)
      IF(ALLOCATED(myMeshElement%childIndices)) &
              DEALLOCATE(myMeshElement%childIndices)
      IF(ALLOCATED(myMeshElement%childWeights)) &
              DEALLOCATE(myMeshElement%childWeights)

    ENDSUBROUTINE clear_MultigridMeshElement

ENDMODULE MultigridMesh
