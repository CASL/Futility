!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testMultigridMesh
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE IntrType
  USE MultigridMesh

  IMPLICIT NONE

  CREATE_TEST('Test MultigridMesh')

  REGISTER_SUBTEST('Test Clear',testClear)
  REGISTER_SUBTEST('Test Init',testInit)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
  SUBROUTINE testClear
    TYPE(MultigridMeshStructureType) :: myMMeshes

    myMMeshes%nLevels=5
    ALLOCATE(myMMeshes%meshes(4))
    myMMeshes%isInit=.TRUE.

    CALL myMMeshes%clear()

    ASSERT(.NOT. myMMeshes%isInit,'should not be init')
    ASSERT(.NOT. ALLOCATED(myMMeshes%meshes),'meshes should not be allocated')
  ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
  SUBROUTINE testInit
    TYPE(MultigridMeshStructureType) :: myMMeshes
    INTEGER(SIK),PARAMETER :: nLevels=6

    CALL myMMeshes%init(nLevels)

    ASSERT(myMMeshes%isInit,'is init')
    ASSERT(myMMeshes%nLevels == nLevels,'correct number of levels')
    ASSERT(ALLOCATED(myMMeshes%meshes),'meshes allocated')
    ASSERT(SIZE(myMMeshes%meshes) == nLevels-1,'meshes is the correct size')

    CALL myMMeshes%clear()
  ENDSUBROUTINE testInit

ENDPROGRAM testMultigridMesh
