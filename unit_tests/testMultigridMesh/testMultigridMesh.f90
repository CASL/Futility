!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!
! MultigridMesh does not have much functionality on its own; it mostly serves
! as a template for storing mesh information used by LinearSolverTypes_Multigrid.
!
! For an example of how to set up MultigridMesh, there is a very simplified
! example in testLinearSolver_Multigrid under testFillInterpMats.  A more
! complex example can be found in MPACT.  For those attempting to use this in
! non-MPACT codes, please carefully read the descriptions of the attributes in
! MultigridMesh.f90
!
PROGRAM testMultigridMesh
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE MultigridMesh

IMPLICIT NONE

CREATE_TEST('Test MultigridMesh')

REGISTER_SUBTEST('Test Clear Structure',testClearStructure)
REGISTER_SUBTEST('Test Clear Mesh',testClearMesh)
REGISTER_SUBTEST('Test Init',testInit)

FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testClearStructure
  TYPE(MultigridMeshStructureType) :: myMMeshes

  myMMeshes%nLevels=5
  ALLOCATE(myMMeshes%meshes(4))
  myMMeshes%isInit=.TRUE.

  CALL myMMeshes%clear()

  ASSERT(.NOT. myMMeshes%isInit,'should not be init')
  ASSERT(.NOT. ALLOCATED(myMMeshes%meshes),'meshes should not be allocated')
ENDSUBROUTINE testClearStructure
!
!-------------------------------------------------------------------------------
SUBROUTINE testClearMesh
  TYPE(MultigridMeshStructureType) :: myMMeshes

  myMMeshes%nLevels=5
  ALLOCATE(myMMeshes%meshes(5))
  myMMeshes%isInit=.TRUE.

  !Test clearing for one particular level:
  ALLOCATE(myMMeshes%meshes(3)%interpDegrees(10))
  ALLOCATE(myMMeshes%meshes(3)%xyzMap(3,10))
  ALLOCATE(myMMeshes%meshes(3)%mmData(10))
  myMMeshes%meshes(3)%istt=1
  myMMeshes%meshes(3)%istp=10
  ALLOCATE(myMMeshes%meshes(3)%mmData(1)%childIndices(4))
  CALL myMMeshes%meshes(3)%mmData(1)%clear()
  ASSERT(.NOT. ALLOCATED(myMMeshes%meshes(3)%mmData(1)%childIndices),'childindices not allocated')
  CALL myMMeshes%meshes(3)%clear()
  ASSERT(.NOT. ALLOCATED(myMMeshes%meshes(3)%interpDegrees),'interpDegrees not allocated')
  ASSERT(.NOT. ALLOCATED(myMMeshes%meshes(3)%xyzMap),'xyzmap not allocated')
  ASSERT(.NOT. ALLOCATED(myMMeshes%meshes(3)%mmData),'mmData not allocated')

  CALL myMMeshes%clear()
ENDSUBROUTINE testClearMesh
!
!-------------------------------------------------------------------------------
SUBROUTINE testInit
  TYPE(MultigridMeshStructureType) :: myMMeshes
  INTEGER(SIK),PARAMETER :: nLevels=6

  CALL myMMeshes%init(nLevels)

  ASSERT(myMMeshes%isInit,'is init')
  ASSERT(myMMeshes%nLevels == nLevels,'correct number of levels')
  ASSERT(ALLOCATED(myMMeshes%meshes),'meshes allocated')
  ASSERT(SIZE(myMMeshes%meshes) == nLevels,'meshes is the correct size')

  CALL myMMeshes%clear()
ENDSUBROUTINE testInit

ENDPROGRAM testMultigridMesh
