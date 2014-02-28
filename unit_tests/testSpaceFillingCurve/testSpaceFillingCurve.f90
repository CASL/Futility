!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                              Copyright (C) 2012                              !
!                   The Regents of the University of Michigan                  !
!              MPACT Development Group and Prof. Thomas J. Downar              !
!                             All rights reserved.                             !
!                                                                              !
! Copyright is reserved to the University of Michigan for purposes of          !
! controlled dissemination, commercialization through formal licensing, or     !
! other disposition. The University of Michigan nor any of their employees,    !
! makes any warranty, express or implied, or assumes any liability or          !
! responsibility for the accuracy, completeness, or usefulness of any          !
! information, apparatus, product, or process disclosed, or represents that    !
! its use would not infringe privately owned rights. Reference herein to any   !
! specific commercial products, process, or service by trade name, trademark,  !
! manufacturer, or otherwise, does not necessarily constitute or imply its     !
! endorsement, recommendation, or favoring by the University of Michigan.      !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testSpaceFillingCurve
#include "UnitTest.h"
  USE UnitTest
  USE Utils
  
  IMPLICIT NONE

  INTEGER(SIK) :: i,j,k,iout,jout,kout,istt,istp,rx(2),ry(2),rz(2)

  TYPE(ZTreeNodeType),TARGET :: testZTree
  TYPE(ZTreeNodeType) :: tmpZTreeNode
  TYPE(ZTreeNodeType),POINTER :: tmpZTreeLevel(:)

  CREATE_TEST('SPACE FILLING CURVES')

  REGISTER_SUBTEST('Morton Ordering',testMortonIndex)
  REGISTER_SUBTEST('Z-Tree %clear()',testZtreeClear)
  REGISTER_SUBTEST('Z-Tree %init(...)',testZTreeInit)
  REGISTER_SUBTEST('Z-Tree %istpMax()',testZTreeIstpMax)
  REGISTER_SUBTEST('Z-Tree %ijk2oneD(...)',testZTreeIJK2oneD)
  REGISTER_SUBTEST('Z-Tree %oneD2ijk(...)',testZTreeOneD2ijk)
  REGISTER_SUBTEST('Z-Tree %getMaxLevels(...)',testZTreeGetMaxLevels)
  REGISTER_SUBTEST('Z-Tree %getNDomains(...)',testZTreeGetNDomains)
  REGISTER_SUBTEST('Z-Tree %getSubNodeBounds(...)',testZTreeGetSubNodeBounds)
  REGISTER_SUBTEST('Z-Tree %getSubNodePointer(...)',testZTreeGetSubNodePointer)
  REGISTER_SUBTEST('Z-Tree %getLeafNodePointer(...)',testZTreeGetLeafNodePointer)
  REGISTER_SUBTEST('Z-Tree %addToLeafs(...)',testZTreeAddToLeafs)
  REGISTER_SUBTEST('Z-Tree %flattenLeafs(...)',testZTreeflattenLeafs)
  REGISTER_SUBTEST('Z-Tree %renumber(...)',testZTreeRenumber)
  REGISTER_SUBTEST('Z-Tree %shave(...)',testZTreeShave)
  REGISTER_SUBTEST('Z-Tree %partition(...)',testZTreePartition)
  REGISTER_SUBTEST('Deferred construction',testDeferredConst)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!Test MortonIndex
    SUBROUTINE testMortonIndex()
      SET_PREFIX('MortonIndex 2D')
      ASSERT(MortonIndex(0,0) == 0,'(0,0)')
      ASSERT(MortonIndex(1,0) == 1,'(1,0)')
      ASSERT(MortonIndex(0,1) == 2,'(0,1)')
      ASSERT(MortonIndex(1,1) == 3,'(1,1)')
      ASSERT(MortonIndex(5,6) == 57,'(5,6)')

      SET_PREFIX('MortonIndex 3D')
      ASSERT(MortonIndex(0,0,0) == 0,'(0,0,0)')
      ASSERT(MortonIndex(1,0,0) == 1,'(1,0,0)')
      ASSERT(MortonIndex(0,1,0) == 2,'(0,1,0)')
      ASSERT(MortonIndex(1,1,0) == 3,'(1,1,0)')
      ASSERT(MortonIndex(0,0,1) == 4,'(0,0,1)')
      ASSERT(MortonIndex(1,0,1) == 5,'(1,0,1)')
      ASSERT(MortonIndex(0,1,1) == 6,'(0,1,1)')
      ASSERT(MortonIndex(1,1,1) == 7,'(1,1,1)')
      ASSERT(MortonIndex(1,4,5) == 389,'(1,4,5)')
    ENDSUBROUTINE testMortonIndex
!
!-------------------------------------------------------------------------------
!Test ZTree clear
    SUBROUTINE testZTreeClear()
!
!Initialize by hand
      testZTree%x=(/1,2/)
      testZTree%y=1
      testZTree%z=1
      testZTree%istt=1
      testZTree%istp=2
      testZTree%nsubdomains=2
      ALLOCATE(testZTree%subdomains(2))
      testZTree%subdomains(1)%x=1
      testZTree%subdomains(1)%y=1
      testZTree%subdomains(1)%z=1
      testZTree%subdomains(1)%istt=1
      testZTree%subdomains(1)%istp=1
      testZTree%subdomains(2)%x=2
      testZTree%subdomains(2)%y=1
      testZTree%subdomains(2)%z=1
      testZTree%subdomains(1)%istt=2
      testZTree%subdomains(1)%istp=2

      !Test clear
      CALL testZTree%clear()
      ASSERT(ALL(testZTree%x == 0),'%x')
      ASSERT(ALL(testZTree%y == 0),'%y')
      ASSERT(ALL(testZTree%z == 0),'%z')
      ASSERT(testZTree%istt == -1,'%istt')
      ASSERT(testZTree%istp == -1,'%istp')
      ASSERT(testZTree%nsubdomains == 0,'%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(testZTree%subdomains),'%subdomains')
    ENDSUBROUTINE testZTreeClear
!
!-------------------------------------------------------------------------------
!Test %init
    SUBROUTINE testZTreeInit()

      !Degenerate cases
      COMPONENT_TEST('%init(4,1,1,4,1,4,1)')
      CALL testZTree%init(4,1,1,4,1,4,1)
      ASSERT(ALL(testZTree%x == 0),'testZTree%x')
      ASSERT(ALL(testZTree%y == 0),'testZTree%y')
      ASSERT(ALL(testZTree%z == 0),'testZTree%z')
      ASSERT(testZTree%istt == -1,'testZTree%istt')

      ASSERT(testZTree%nsubdomains == 0,'testZTree%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(testZTree%subdomains),'testZTree%subdomains')

      COMPONENT_TEST('%init(1,4,4,1,1,4,1)')
      CALL testZTree%init(1,4,4,1,1,4,1)
      ASSERT(ALL(testZTree%x == 0),'testZTree%x')
      ASSERT(ALL(testZTree%y == 0),'testZTree%y')
      ASSERT(ALL(testZTree%z == 0),'testZTree%z')
      ASSERT(testZTree%istt == -1,'testZTree%istt')
      ASSERT(testZTree%nsubdomains == 0,'testZTree%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(testZTree%subdomains),'testZTree%subdomains')

      COMPONENT_TEST('%init(1,4,1,4,4,1,1)')
      CALL testZTree%init(1,4,1,4,4,1,1)
      ASSERT(ALL(testZTree%x == 0),'testZTree%x')
      ASSERT(ALL(testZTree%y == 0),'testZTree%y')
      ASSERT(ALL(testZTree%z == 0),'testZTree%z')
      ASSERT(testZTree%istt == -1,'testZTree%istt')
      ASSERT(testZTree%nsubdomains == 0,'testZTree%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(testZTree%subdomains),'testZTree%subdomains')

      COMPONENT_TEST('%init(1,4,1,4,1,4,-5)')
      CALL testZTree%init(1,4,1,4,1,4,-5)
      ASSERT(ALL(testZTree%x == 0),'testZTree%x')
      ASSERT(ALL(testZTree%y == 0),'testZTree%y')
      ASSERT(ALL(testZTree%z == 0),'testZTree%z')
      ASSERT(testZTree%istt == -1,'testZTree%istt')
      ASSERT(testZTree%nsubdomains == 0,'testZTree%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(testZTree%subdomains),'testZTree%subdomains')

      COMPONENT_TEST('%init(-1,4,1,4,1,4,1)')
      CALL testZTree%init(-1,4,1,4,1,4,1)
      ASSERT(ALL(testZTree%x == 0),'testZTree%x')
      ASSERT(ALL(testZTree%y == 0),'testZTree%y')
      ASSERT(ALL(testZTree%z == 0),'testZTree%z')
      ASSERT(testZTree%istt == -1,'testZTree%istt')
      ASSERT(testZTree%nsubdomains == 0,'testZTree%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(testZTree%subdomains),'testZTree%subdomains')

      COMPONENT_TEST('%init(-1,4,-1,4,1,4,1)')
      CALL testZTree%init(-1,4,-1,4,1,4,1)
      ASSERT(ALL(testZTree%x == 0),'testZTree%x')
      ASSERT(ALL(testZTree%y == 0),'testZTree%y')
      ASSERT(ALL(testZTree%z == 0),'testZTree%z')
      ASSERT(testZTree%istt == -1,'testZTree%istt')
      ASSERT(testZTree%nsubdomains == 0,'testZTree%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(testZTree%subdomains),'testZTree%subdomains')

      COMPONENT_TEST('%init(1,4,1,4,-1,4,1)')
      ASSERT(ALL(testZTree%x == 0),'testZTree%x')
      ASSERT(ALL(testZTree%y == 0),'testZTree%y')
      ASSERT(ALL(testZTree%z == 0),'testZTree%z')
      ASSERT(testZTree%istt == -1,'testZTree%istt')
      ASSERT(testZTree%nsubdomains == 0,'testZTree%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(testZTree%subdomains),'testZTree%subdomains')

      !Correct case
      COMPONENT_TEST('%init(1,4,1,4,1,4,1)')
      CALL testZTree%init(1,4,1,4,1,4,1)
      ASSERT(ALL(testZTree%x == (/1,4/)),'testZTree%x')
      ASSERT(ALL(testZTree%y == (/1,4/)),'testZTree%y')
      ASSERT(ALL(testZTree%z == (/1,4/)),'testZTree%z')
      ASSERT(testZTree%istt == 1,'testZTree%istt')
      ASSERT(testZTree%istp == 64,'testZTree%istp')
      ASSERT(testZTree%nsubdomains == 8,'testZTree%nsubdomains')
      ASSERT(SIZE(testZTree%subdomains,DIM=1) == 8,'SIZE(testZTree%nsubdomains)')

      CALL checkLevel1() !Check level 1 domains
      CALL checkLevel2d1() !Check level 2 domain 1
      CALL checkLevel2d2() !Check level 2 domain 2
      CALL checkLevel2d3() !Check level 2 domain 3
      CALL checkLevel2d4() !Check level 2 domain 4
      CALL checkLevel2d5() !Check level 2 domain 5
      CALL checkLevel2d6() !Check level 2 domain 6
      CALL checkLevel2d7() !Check level 2 domain 7
      CALL checkLevel2d8() !Check level 2 domain 8
      CALL testZTree%init(1,4,1,4,1,4,1) !Error check

      !
      !Check a few other grids to test for preferential splitting and uneven splitting
      CALL testZTree%clear()
      CALL testZTree%init(1,3,1,3,1,6,1)
      COMPONENT_TEST('%init(1,3,1,3,1,6,1)')
      ASSERT(testZTree%nsubdomains == 2,'aspect ratio == 2')
      CALL testZTree%clear()

      COMPONENT_TEST('%init(1,3,1,3,1,1,1)')
      CALL testZTree%init(1,3,1,3,1,1,1)
      ASSERT(testZTree%nsubdomains == 4,'%nsubdomains')
      ASSERT(testZTree%subdomains(1)%istt == 1,'%subdomains(1)%istt')
      ASSERT(testZTree%subdomains(1)%nsubdomains == 0,'%subdomains(1)%nsubdomains')
      ASSERT(testZTree%subdomains(2)%istt == 2,'%subdomains(2)%istt')
      ASSERT(testZTree%subdomains(2)%nsubdomains == 2,'%subdomains(2)%nsubdomains')
      ASSERT(testZTree%subdomains(3)%istt == 4,'%subdomains(3)%istt')
      ASSERT(testZTree%subdomains(3)%nsubdomains == 2,'%subdomains(3)%nsubdomains')
      ASSERT(testZTree%subdomains(4)%istt == 6,'%subdomains(4)%istt')
      ASSERT(testZTree%subdomains(4)%nsubdomains == 4,'%subdomains(4)%nsubdomains')
      CALL testZTree%clear()

      COMPONENT_TEST('%init(1,1,1,1,1,3,1)')
      CALL testZTree%init(1,1,1,1,1,3,1)
      ASSERT(testZTree%nsubdomains == 2,'%nsubdomains')
      ASSERT(testZTree%subdomains(1)%istt == 1,'%subdomains(1)%istt')
      ASSERT(testZTree%subdomains(1)%nsubdomains == 0,'%subdomains(1)%nsubdomains')
      ASSERT(testZTree%subdomains(2)%istt == 2,'%subdomains(2)%istt')
      ASSERT(testZTree%subdomains(2)%nsubdomains == 2,'%subdomains(2)%nsubdomains')
      CALL testZTree%clear()
    ENDSUBROUTINE testZTreeInit
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel1()
      tmpZTreeLevel => testZTree%subdomains

      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      ASSERT(ALL(tmpZTreeNode%x == (/1,2/)),'%subdomains(1)%x')
      ASSERT(ALL(tmpZTreeNode%y == (/1,2/)),'%subdomains(1)%y')
      ASSERT(ALL(tmpZTreeNode%z == (/1,2/)),'%subdomains(1)%z')
      ASSERT(tmpZTreeNode%istt == 1,'%subdomains(1)%istt')
      ASSERT(tmpZTreeNode%istp == 8,'%subdomains(1)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 8,'%subdomains(1)%nsubdomains')
      ASSERT(SIZE(tmpZTreeNode%subdomains,DIM=1) == 8,'SIZE(%subdomains(1)%subdomains)')

      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      ASSERT(ALL(tmpZTreeNode%x == (/3,4/)),'%subdomains(2)%x')
      ASSERT(ALL(tmpZTreeNode%y == (/1,2/)),'%subdomains(2)%y')
      ASSERT(ALL(tmpZTreeNode%z == (/1,2/)),'%subdomains(2)%z')
      ASSERT(tmpZTreeNode%istt == 9,'%subdomains(2)%istt')
      ASSERT(tmpZTreeNode%istp == 16,'%subdomains(2)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 8,'%subdomains(2)%nsubdomains')
      ASSERT(SIZE(tmpZTreeNode%subdomains,DIM=1) == 8,'SIZE(%subdomains(2)%subdomains)')

      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      ASSERT(ALL(tmpZTreeNode%x == (/1,2/)),'%subdomains(3)%x')
      ASSERT(ALL(tmpZTreeNode%y == (/3,4/)),'%subdomains(3)%y')
      ASSERT(ALL(tmpZTreeNode%z == (/1,2/)),'%subdomains(3)%z')
      ASSERT(tmpZTreeNode%istt == 17,'%subdomains(3)%istt')
      ASSERT(tmpZTreeNode%istp == 24,'%subdomains(3)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 8,'%subdomains(3)%nsubdomains')
      ASSERT(SIZE(tmpZTreeNode%subdomains,DIM=1) == 8,'SIZE(%subdomains(3)%subdomains)')

      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      ASSERT(ALL(tmpZTreeNode%x == (/3,4/)),'%subdomains(4)%x')
      ASSERT(ALL(tmpZTreeNode%y == (/3,4/)),'%subdomains(4)%y')
      ASSERT(ALL(tmpZTreeNode%z == (/1,2/)),'%subdomains(4)%z')
      ASSERT(tmpZTreeNode%istt == 25,'%subdomains(4)%istt')
      ASSERT(tmpZTreeNode%istp == 32,'%subdomains(4)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 8,'%subdomains(4)%nsubdomains')
      ASSERT(SIZE(tmpZTreeNode%subdomains,DIM=1) == 8,'SIZE(%subdomains(4)%subdomains)')

      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      ASSERT(ALL(tmpZTreeNode%x == (/1,2/)),'%subdomains(5)%x')
      ASSERT(ALL(tmpZTreeNode%y == (/1,2/)),'%subdomains(5)%y')
      ASSERT(ALL(tmpZTreeNode%z == (/3,4/)),'%subdomains(5)%z')
      ASSERT(tmpZTreeNode%istt == 33,'%subdomains(5)%istt')
      ASSERT(tmpZTreeNode%istp == 40,'%subdomains(5)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 8,'%subdomains(4)%nsubdomains')
      ASSERT(SIZE(tmpZTreeNode%subdomains,DIM=1) == 8,'SIZE(%subdomains(4)%subdomains)')

      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      ASSERT(ALL(tmpZTreeNode%x == (/3,4/)),'%subdomains(6)%x')
      ASSERT(ALL(tmpZTreeNode%y == (/1,2/)),'%subdomains(6)%y')
      ASSERT(ALL(tmpZTreeNode%z == (/3,4/)),'%subdomains(6)%z')
      ASSERT(tmpZTreeNode%istt == 41,'%subdomains(6)%istt')
      ASSERT(tmpZTreeNode%istp == 48,'%subdomains(6)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 8,'%subdomains(6)%nsubdomains')
      ASSERT(SIZE(tmpZTreeNode%subdomains,DIM=1) == 8,'SIZE(%subdomains(6)%subdomains)')

      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      ASSERT(ALL(tmpZTreeNode%x == (/1,2/)),'%subdomains(7)%x')
      ASSERT(ALL(tmpZTreeNode%y == (/3,4/)),'%subdomains(7)%y')
      ASSERT(ALL(tmpZTreeNode%z == (/3,4/)),'%subdomains(7)%z')
      ASSERT(tmpZTreeNode%istt == 49,'%subdomains(7)%istt')
      ASSERT(tmpZTreeNode%istp == 56,'%subdomains(7)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 8,'%subdomains(7)%nsubdomains')
      ASSERT(SIZE(tmpZTreeNode%subdomains,DIM=1) == 8,'SIZE(%subdomains(7)%subdomains)')

      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      ASSERT(ALL(tmpZTreeNode%x == (/3,4/)),'%subdomains(8)%x')
      ASSERT(ALL(tmpZTreeNode%y == (/3,4/)),'%subdomains(8)%y')
      ASSERT(ALL(tmpZTreeNode%z == (/3,4/)),'%subdomains(8)%z')
      ASSERT(tmpZTreeNode%istt == 57,'%subdomains(8)%istt')
      ASSERT(tmpZTreeNode%istp == 64,'%subdomains(8)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 8,'%subdomains(8)%nsubdomains')
      ASSERT(SIZE(tmpZTreeNode%subdomains,DIM=1) == 8,'SIZE(%subdomains(8)%subdomains)')
    ENDSUBROUTINE checkLevel1
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d1()
      tmpZTreeLevel => testZTree%subdomains(1)%subdomains

      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(1)%subdomains(1)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(1)%subdomains(1)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(1)%subdomains(1)%z')
      ASSERT(tmpZTreeNode%istt == 1,'%subdomains(1)%subdomains(1)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(1)%subdomains(1)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(1)%subdomains(1)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(1)%subdomains(1)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(1)%subdomains(2)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(1)%subdomains(2)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(1)%subdomains(2)%z')
      ASSERT(tmpZTreeNode%istt == 2,'%subdomains(1)%subdomains(2)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(1)%subdomains(2)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(1)%subdomains(2)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(1)%subdomains(2)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(1)%subdomains(3)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(1)%subdomains(3)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(1)%subdomains(3)%z')
      ASSERT(tmpZTreeNode%istt == 3,'%subdomains(1)%subdomains(3)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(1)%subdomains(3)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(1)%subdomains(3)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(1)%subdomains(3)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(1)%subdomains(4)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(1)%subdomains(4)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(1)%subdomains(4)%z')
      ASSERT(tmpZTreeNode%istt == 4,'%subdomains(1)%subdomains(4)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(1)%subdomains(4)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(1)%subdomains(4)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(1)%subdomains(4)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(1)%subdomains(5)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(1)%subdomains(5)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(1)%subdomains(5)%z')
      ASSERT(tmpZTreeNode%istt == 5,'%subdomains(1)%subdomains(5)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(1)%subdomains(5)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(1)%subdomains(5)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(1)%subdomains(5)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(1)%subdomains(6)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(1)%subdomains(6)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(1)%subdomains(6)%z')
      ASSERT(tmpZTreeNode%istt == 6,'%subdomains(1)%subdomains(6)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(1)%subdomains(6)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(1)%subdomains(6)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(1)%subdomains(6)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(1)%subdomains(7)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(1)%subdomains(7)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(1)%subdomains(7)%z')
      ASSERT(tmpZTreeNode%istt == 7,'%subdomains(1)%subdomains(7)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(1)%subdomains(7)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(1)%subdomains(7)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(1)%subdomains(7)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(1)%subdomains(8)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(1)%subdomains(8)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(1)%subdomains(8)%z')
      ASSERT(tmpZTreeNode%istt == 8,'%subdomains(1)%subdomains(8)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(1)%subdomains(8)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(1)%subdomains(8)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(1)%subdomains(8)%subdomains')
    ENDSUBROUTINE checkLevel2d1
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d2()
      tmpZTreeLevel => testZTree%subdomains(2)%subdomains

      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(2)%subdomains(1)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(2)%subdomains(1)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(2)%subdomains(1)%z')
      ASSERT(tmpZTreeNode%istt == 9,'%subdomains(2)%subdomains(1)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(2)%subdomains(1)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(2)%subdomains(1)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(2)%subdomains(1)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(2)%subdomains(2)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(2)%subdomains(2)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(2)%subdomains(2)%z')
      ASSERT(tmpZTreeNode%istt == 10,'%subdomains(2)%subdomains(2)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(2)%subdomains(2)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(2)%subdomains(2)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(2)%subdomains(2)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(2)%subdomains(3)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(2)%subdomains(3)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(2)%subdomains(3)%z')
      ASSERT(tmpZTreeNode%istt == 11,'%subdomains(2)%subdomains(3)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(2)%subdomains(3)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(2)%subdomains(3)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(2)%subdomains(3)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(2)%subdomains(4)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(2)%subdomains(4)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(2)%subdomains(4)%z')
      ASSERT(tmpZTreeNode%istt == 12,'%subdomains(2)%subdomains(4)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(2)%subdomains(4)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(2)%subdomains(4)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(2)%subdomains(4)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(2)%subdomains(5)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(2)%subdomains(5)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(2)%subdomains(5)%z')
      ASSERT(tmpZTreeNode%istt == 13,'%subdomains(2)%subdomains(5)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(2)%subdomains(5)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(2)%subdomains(5)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(2)%subdomains(5)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(2)%subdomains(6)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(2)%subdomains(6)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(2)%subdomains(6)%z')
      ASSERT(tmpZTreeNode%istt == 14,'%subdomains(2)%subdomains(6)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(2)%subdomains(6)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(2)%subdomains(6)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(2)%subdomains(6)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(2)%subdomains(7)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(2)%subdomains(7)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(2)%subdomains(7)%z')
      ASSERT(tmpZTreeNode%istt == 15,'%subdomains(2)%subdomains(7)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(2)%subdomains(7)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(2)%subdomains(7)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(2)%subdomains(7)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(2)%subdomains(8)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(2)%subdomains(8)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(2)%subdomains(8)%z')
      ASSERT(tmpZTreeNode%istt == 16,'%subdomains(2)%subdomains(8)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(2)%subdomains(8)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(2)%subdomains(8)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(2)%subdomains(8)%subdomains')
    ENDSUBROUTINE checkLevel2d2
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d3()
      tmpZTreeLevel => testZTree%subdomains(3)%subdomains

      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(3)%subdomains(1)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(3)%subdomains(1)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(3)%subdomains(1)%z')
      ASSERT(tmpZTreeNode%istt == 17,'%subdomains(3)%subdomains(1)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(3)%subdomains(1)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(3)%subdomains(1)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(3)%subdomains(1)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      ASSERT(ALL(tmpZTreeNode%x == (/2,2/)),'%subdomains(3)%subdomains(2)%x')
      ASSERT(ALL(tmpZTreeNode%y == (/3,3/)),'%subdomains(3)%subdomains(2)%y')
      ASSERT(ALL(tmpZTreeNode%z == (/1,1/)),'%subdomains(3)%subdomains(2)%z')
      ASSERT(tmpZTreeNode%istt == 18,'%subdomains(3)%subdomains(2)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(3)%subdomains(2)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(3)%subdomains(2)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(3)%subdomains(2)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(3)%subdomains(3)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(3)%subdomains(3)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(3)%subdomains(3)%z')
      ASSERT(tmpZTreeNode%istt == 19,'%subdomains(3)%subdomains(3)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(3)%subdomains(3)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(3)%subdomains(3)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(3)%subdomains(3)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(3)%subdomains(4)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(3)%subdomains(4)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(3)%subdomains(4)%z')
      ASSERT(tmpZTreeNode%istt == 20,'%subdomains(3)%subdomains(4)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(3)%subdomains(4)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(3)%subdomains(4)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(3)%subdomains(4)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(3)%subdomains(5)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(3)%subdomains(5)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(3)%subdomains(5)%z')
      ASSERT(tmpZTreeNode%istt == 21,'%subdomains(3)%subdomains(5)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(3)%subdomains(5)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(3)%subdomains(5)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(3)%subdomains(5)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(3)%subdomains(6)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(3)%subdomains(6)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(3)%subdomains(6)%z')
      ASSERT(tmpZTreeNode%istt == 22,'%subdomains(3)%subdomains(6)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(3)%subdomains(6)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(3)%subdomains(6)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(3)%subdomains(6)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(3)%subdomains(7)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(3)%subdomains(7)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(3)%subdomains(7)%z')
      ASSERT(tmpZTreeNode%istt == 23,'%subdomains(3)%subdomains(7)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(3)%subdomains(7)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(3)%subdomains(7)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(3)%subdomains(7)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(3)%subdomains(8)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(3)%subdomains(8)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(3)%subdomains(8)%z')
      ASSERT(tmpZTreeNode%istt == 24,'%subdomains(3)%subdomains(8)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(3)%subdomains(8)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(3)%subdomains(8)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(3)%subdomains(8)%subdomains')
    ENDSUBROUTINE checkLevel2d3
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d4()
      tmpZTreeLevel => testZTree%subdomains(4)%subdomains

      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(4)%subdomains(1)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(4)%subdomains(1)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(4)%subdomains(1)%z')
      ASSERT(tmpZTreeNode%istt == 25,'%subdomains(4)%subdomains(1)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(4)%subdomains(1)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(4)%subdomains(1)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(4)%subdomains(1)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(4)%subdomains(2)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(4)%subdomains(2)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(4)%subdomains(2)%z')
      ASSERT(tmpZTreeNode%istt == 26,'%subdomains(4)%subdomains(2)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(4)%subdomains(2)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(4)%subdomains(2)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(4)%subdomains(2)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(4)%subdomains(3)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(4)%subdomains(3)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(4)%subdomains(3)%z')
      ASSERT(tmpZTreeNode%istt == 27,'%subdomains(4)%subdomains(3)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(4)%subdomains(3)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(4)%subdomains(3)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(4)%subdomains(3)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(4)%subdomains(4)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(4)%subdomains(4)%y')
      ASSERT(ALL(tmpZTreeNode%z == 1),'%subdomains(4)%subdomains(4)%z')
      ASSERT(tmpZTreeNode%istt == 28,'%subdomains(4)%subdomains(4)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(4)%subdomains(4)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(4)%subdomains(4)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(4)%subdomains(4)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(4)%subdomains(5)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(4)%subdomains(5)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(4)%subdomains(5)%z')
      ASSERT(tmpZTreeNode%istt == 29,'%subdomains(4)%subdomains(5)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(4)%subdomains(5)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(4)%subdomains(5)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(4)%subdomains(5)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(4)%subdomains(6)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(4)%subdomains(6)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(4)%subdomains(6)%z')
      ASSERT(tmpZTreeNode%istt == 30,'%subdomains(4)%subdomains(6)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(4)%subdomains(6)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(4)%subdomains(6)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(4)%subdomains(6)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(4)%subdomains(7)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(4)%subdomains(7)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(4)%subdomains(7)%z')
      ASSERT(tmpZTreeNode%istt == 31,'%subdomains(4)%subdomains(7)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(4)%subdomains(7)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(4)%subdomains(7)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(4)%subdomains(7)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(4)%subdomains(8)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(4)%subdomains(8)%y')
      ASSERT(ALL(tmpZTreeNode%z == 2),'%subdomains(4)%subdomains(8)%z')
      ASSERT(tmpZTreeNode%istt == 32,'%subdomains(4)%subdomains(8)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(4)%subdomains(8)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(4)%subdomains(8)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(4)%subdomains(8)%subdomains')
    ENDSUBROUTINE checkLevel2d4
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d5()
      tmpZTreeLevel => testZTree%subdomains(5)%subdomains

      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(5)%subdomains(1)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(5)%subdomains(1)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(5)%subdomains(1)%z')
      ASSERT(tmpZTreeNode%istt == 33,'%subdomains(5)%subdomains(1)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(5)%subdomains(1)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(5)%subdomains(1)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(5)%subdomains(1)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(5)%subdomains(2)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(5)%subdomains(2)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(5)%subdomains(2)%z')
      ASSERT(tmpZTreeNode%istt == 34,'%subdomains(5)%subdomains(2)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(5)%subdomains(2)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(5)%subdomains(2)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(5)%subdomains(2)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(5)%subdomains(3)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(5)%subdomains(3)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(5)%subdomains(3)%z')
      ASSERT(tmpZTreeNode%istt == 35,'%subdomains(5)%subdomains(3)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(5)%subdomains(3)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(5)%subdomains(3)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(5)%subdomains(3)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(5)%subdomains(4)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(5)%subdomains(4)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(5)%subdomains(4)%z')
      ASSERT(tmpZTreeNode%istt == 36,'%subdomains(5)%subdomains(4)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(5)%subdomains(4)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(5)%subdomains(4)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(5)%subdomains(4)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(5)%subdomains(5)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(5)%subdomains(5)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(5)%subdomains(5)%z')
      ASSERT(tmpZTreeNode%istt == 37,'%subdomains(5)%subdomains(5)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(5)%subdomains(5)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(5)%subdomains(5)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(5)%subdomains(5)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(5)%subdomains(6)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(5)%subdomains(6)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(5)%subdomains(6)%z')
      ASSERT(tmpZTreeNode%istt == 38,'%subdomains(5)%subdomains(6)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(5)%subdomains(6)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(5)%subdomains(6)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(5)%subdomains(6)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(5)%subdomains(7)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(5)%subdomains(7)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(5)%subdomains(7)%z')
      ASSERT(tmpZTreeNode%istt == 39,'%subdomains(5)%subdomains(7)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(5)%subdomains(7)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(5)%subdomains(7)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(5)%subdomains(7)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(5)%subdomains(8)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(5)%subdomains(8)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(5)%subdomains(8)%z')
      ASSERT(tmpZTreeNode%istt == 40,'%subdomains(5)%subdomains(8)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(5)%subdomains(8)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(5)%subdomains(8)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(5)%subdomains(8)%subdomains')
    ENDSUBROUTINE checkLevel2d5
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d6()
      tmpZTreeLevel => testZTree%subdomains(6)%subdomains

      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(6)%subdomains(1)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(6)%subdomains(1)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(6)%subdomains(1)%z')
      ASSERT(tmpZTreeNode%istt == 41,'%subdomains(6)%subdomains(1)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(6)%subdomains(1)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(6)%subdomains(1)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(6)%subdomains(1)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(6)%subdomains(2)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(6)%subdomains(2)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(6)%subdomains(2)%z')
      ASSERT(tmpZTreeNode%istt == 42,'%subdomains(6)%subdomains(2)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(6)%subdomains(2)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(6)%subdomains(2)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(6)%subdomains(2)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(6)%subdomains(3)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(6)%subdomains(3)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(6)%subdomains(3)%z')
      ASSERT(tmpZTreeNode%istt == 43,'%subdomains(6)%subdomains(3)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(6)%subdomains(3)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(6)%subdomains(3)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(6)%subdomains(3)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(6)%subdomains(4)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(6)%subdomains(4)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(6)%subdomains(4)%z')
      ASSERT(tmpZTreeNode%istt == 44,'%subdomains(6)%subdomains(4)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(6)%subdomains(4)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(6)%subdomains(4)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(6)%subdomains(4)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(6)%subdomains(5)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(6)%subdomains(5)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(6)%subdomains(5)%z')
      ASSERT(tmpZTreeNode%istt == 45,'%subdomains(6)%subdomains(5)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(6)%subdomains(5)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(6)%subdomains(5)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(6)%subdomains(5)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(6)%subdomains(6)%x')
      ASSERT(ALL(tmpZTreeNode%y == 1),'%subdomains(6)%subdomains(6)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(6)%subdomains(6)%z')
      ASSERT(tmpZTreeNode%istt == 46,'%subdomains(6)%subdomains(6)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(6)%subdomains(6)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(6)%subdomains(6)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(6)%subdomains(6)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(6)%subdomains(7)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(6)%subdomains(7)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(6)%subdomains(7)%z')
      ASSERT(tmpZTreeNode%istt == 47,'%subdomains(6)%subdomains(7)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(6)%subdomains(7)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(6)%subdomains(7)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(6)%subdomains(7)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(6)%subdomains(8)%x')
      ASSERT(ALL(tmpZTreeNode%y == 2),'%subdomains(6)%subdomains(8)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(6)%subdomains(8)%z')
      ASSERT(tmpZTreeNode%istt == 48,'%subdomains(6)%subdomains(8)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(6)%subdomains(8)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(6)%subdomains(8)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(6)%subdomains(8)%subdomains')
    ENDSUBROUTINE checkLevel2d6
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d7()
      tmpZTreeLevel => testZTree%subdomains(7)%subdomains

      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(7)%subdomains(1)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(7)%subdomains(1)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(7)%subdomains(1)%z')
      ASSERT(tmpZTreeNode%istt == 49,'%subdomains(7)%subdomains(1)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(7)%subdomains(1)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(7)%subdomains(1)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(7)%subdomains(1)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(7)%subdomains(2)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(7)%subdomains(2)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(7)%subdomains(2)%z')
      ASSERT(tmpZTreeNode%istt == 50,'%subdomains(7)%subdomains(2)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(7)%subdomains(2)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(7)%subdomains(2)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(7)%subdomains(2)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(7)%subdomains(3)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(7)%subdomains(3)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(7)%subdomains(3)%z')
      ASSERT(tmpZTreeNode%istt == 51,'%subdomains(7)%subdomains(3)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(7)%subdomains(3)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(7)%subdomains(3)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(7)%subdomains(3)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(7)%subdomains(4)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(7)%subdomains(4)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(7)%subdomains(4)%z')
      ASSERT(tmpZTreeNode%istt == 52,'%subdomains(7)%subdomains(4)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(7)%subdomains(4)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(7)%subdomains(4)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(7)%subdomains(4)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(7)%subdomains(5)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(7)%subdomains(5)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(7)%subdomains(5)%z')
      ASSERT(tmpZTreeNode%istt == 53,'%subdomains(7)%subdomains(5)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(7)%subdomains(5)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(7)%subdomains(5)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(7)%subdomains(5)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(7)%subdomains(6)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(7)%subdomains(6)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(7)%subdomains(6)%z')
      ASSERT(tmpZTreeNode%istt == 54,'%subdomains(7)%subdomains(6)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(7)%subdomains(6)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(7)%subdomains(6)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(7)%subdomains(6)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      ASSERT(ALL(tmpZTreeNode%x == 1),'%subdomains(7)%subdomains(7)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(7)%subdomains(7)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(7)%subdomains(7)%z')
      ASSERT(tmpZTreeNode%istt == 55,'%subdomains(7)%subdomains(7)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(7)%subdomains(7)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(7)%subdomains(7)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(7)%subdomains(7)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      ASSERT(ALL(tmpZTreeNode%x == 2),'%subdomains(7)%subdomains(8)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(7)%subdomains(8)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(7)%subdomains(8)%z')
      ASSERT(tmpZTreeNode%istt == 56,'%subdomains(7)%subdomains(8)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(7)%subdomains(8)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(7)%subdomains(8)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(7)%subdomains(8)%subdomains')
    ENDSUBROUTINE checkLevel2d7
!
!-------------------------------------------------------------------------------
    SUBROUTINE checkLevel2d8()
      tmpZTreeLevel => testZTree%subdomains(8)%subdomains

      tmpZTreeNode=tmpZTreeLevel(1) !Domain 1
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(8)%subdomains(1)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(8)%subdomains(1)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(8)%subdomains(1)%z')
      ASSERT(tmpZTreeNode%istt == 57,'%subdomains(8)%subdomains(1)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(8)%subdomains(1)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(8)%subdomains(1)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(8)%subdomains(1)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(2) !Domain 2
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(8)%subdomains(2)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(8)%subdomains(2)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(8)%subdomains(2)%z')
      ASSERT(tmpZTreeNode%istt == 58,'%subdomains(8)%subdomains(2)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(8)%subdomains(2)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(8)%subdomains(2)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(8)%subdomains(2)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(3) !Domain 3
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(8)%subdomains(3)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(8)%subdomains(3)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(8)%subdomains(3)%z')
      ASSERT(tmpZTreeNode%istt == 59,'%subdomains(8)%subdomains(3)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(8)%subdomains(3)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(8)%subdomains(3)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(8)%subdomains(3)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(4) !Domain 4
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(8)%subdomains(4)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(8)%subdomains(4)%y')
      ASSERT(ALL(tmpZTreeNode%z == 3),'%subdomains(8)%subdomains(4)%z')
      ASSERT(tmpZTreeNode%istt == 60,'%subdomains(8)%subdomains(4)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(8)%subdomains(4)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(8)%subdomains(4)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(8)%subdomains(4)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(5) !Domain 5
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(8)%subdomains(5)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(8)%subdomains(5)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(8)%subdomains(5)%z')
      ASSERT(tmpZTreeNode%istt == 61,'%subdomains(8)%subdomains(5)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(8)%subdomains(5)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(8)%subdomains(5)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(8)%subdomains(5)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(6) !Domain 6
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(8)%subdomains(6)%x')
      ASSERT(ALL(tmpZTreeNode%y == 3),'%subdomains(8)%subdomains(6)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(8)%subdomains(6)%z')
      ASSERT(tmpZTreeNode%istt == 62,'%subdomains(8)%subdomains(6)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(8)%subdomains(6)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(8)%subdomains(6)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(8)%subdomains(6)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(7) !Domain 7
      ASSERT(ALL(tmpZTreeNode%x == 3),'%subdomains(8)%subdomains(7)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(8)%subdomains(7)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(8)%subdomains(7)%z')
      ASSERT(tmpZTreeNode%istt == 63,'%subdomains(8)%subdomains(7)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(8)%subdomains(7)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(8)%subdomains(7)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(8)%subdomains(7)%subdomains')

      tmpZTreeNode=tmpZTreeLevel(8) !Domain 8
      ASSERT(ALL(tmpZTreeNode%x == 4),'%subdomains(8)%subdomains(8)%x')
      ASSERT(ALL(tmpZTreeNode%y == 4),'%subdomains(8)%subdomains(8)%y')
      ASSERT(ALL(tmpZTreeNode%z == 4),'%subdomains(8)%subdomains(8)%z')
      ASSERT(tmpZTreeNode%istt == 64,'%subdomains(8)%subdomains(8)%istt')
      ASSERT(tmpZTreeNode%istp == tmpZTreeNode%istt,'%subdomains(8)%subdomains(8)%istp')
      ASSERT(tmpZTreeNode%nsubdomains == 0,'%subdomains(8)%subdomains(8)%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(tmpZTreeNode%subdomains),'%subdomains(8)%subdomains(8)%subdomains')
    ENDSUBROUTINE checkLevel2d8
!
!-------------------------------------------------------------------------------
!Test %istpMax()
    SUBROUTINE testZTreeIstpMax()

      ASSERT(testZTree%istpMax() == -1,'uninit')
      CALL testZTree%init(1,4,1,4,1,4,1)
      ASSERT(testZTree%istpMax() == 64,'testZTree%istpMax()')
      DO i=1,8
        ASSERT(testZTree%subdomains(i)%istpMax() == 8*i,'%subdomains(i)%istpMax()')
        FINFO()  'i=',i
        DO j=1,8
          ASSERT(testZTree%subdomains(i)%subdomains(j)%istpMax() == (i-1)*8+j,'... %subdomains(j)%istpMax()')
          FINFO() '(i,j)=',i,j
        ENDDO
      ENDDO
      CALL testZTree%clear()
    ENDSUBROUTINE testZTreeIstpMax
!
!-------------------------------------------------------------------------------
!Test %ijk2oneD
    SUBROUTINE testZTreeIJK2oneD()
      ASSERT(testZTree%ijk2oneD(1,1,1) == -1,'testZTree%ijk2oneD(1,1,1)')
      ASSERT(testZTree%ijk2oneD(0,0,0) == -1,'testZTree%ijk2oneD(0,0,0)')
      CALL testZTree%init(1,4,1,4,1,4,1)
      !Degenerate cases
      ASSERT(testZTree%ijk2oneD(0,1,1) == -1,'testZTree%ijk2oneD(0,1,1)')
      ASSERT(testZTree%ijk2oneD(1,0,1) == -1,'testZTree%ijk2oneD(1,0,1)')
      ASSERT(testZTree%ijk2oneD(1,1,0) == -1,'testZTree%ijk2oneD(1,1,0)')
      ASSERT(testZTree%ijk2oneD(5,1,1) == -1,'testZTree%ijk2oneD(5,1,1)')
      ASSERT(testZTree%ijk2oneD(1,5,1) == -1,'testZTree%ijk2oneD(1,5,1)')
      ASSERT(testZTree%ijk2oneD(1,1,5) == -1,'testZTree%ijk2oneD(1,1,5)')

      !Correctness
      DO k=1,4
        DO j=1,4
          DO i=1,4
            ASSERT(testZTree%ijk2oneD(i,j,k) == MortonIndex(i-1,j-1,k-1)+1,'testZTree%ijk2oneD(i,j,k)')
            FINFO() '(i,j,k)=',i,j,k
          ENDDO
        ENDDO
      ENDDO
      CALL testZTree%clear()
    ENDSUBROUTINE testZTreeIJK2oneD
!
!-------------------------------------------------------------------------------
!Test %oneD2ijk
    SUBROUTINE testZTreeOneD2ijk()

      CALL testZTree%oneD2ijk(0,iout,jout,kout)
      ASSERT(ALL((/iout,jout,kout/) == -1),'%oneD2ijk(0,..) (uninit)')
      CALL testZTree%oneD2ijk(-1,iout,jout,kout)
      ASSERT(ALL((/iout,jout,kout/) == -1),'%oneD2ijk(-1,...) (uninit)')

      CALL testZTree%init(1,4,1,4,1,4,1)
      !Degenerate cases
      CALL testZTree%oneD2ijk(-1,iout,jout,kout)
      ASSERT(ALL((/iout,jout,kout/) == -1),'%oneD2ijk(-1,...)')
      CALL testZTree%oneD2ijk(65,iout,jout,kout)
      ASSERT(ALL((/iout,jout,kout/) == -1),'%oneD2ijk(65,...)')

      !Correctness
      DO k=1,4
        DO j=1,4
          DO i=1,4
            CALL testZTree%oneD2ijk(MortonIndex(i-1,j-1,k-1)+1,iout,jout,kout)
            ASSERT(ALL((/i,j,k/) == (/iout,jout,kout/)),'%oneD2ijk(...)')
            FINFO() 'MortonIndex=',MortonIndex(i-1,j-1,k-1)+1,i,j,k
          ENDDO
        ENDDO
      ENDDO
      CALL testZTree%clear()
    ENDSUBROUTINE testZTreeOneD2ijk
!
!-------------------------------------------------------------------------------
!Test %renumber
    SUBROUTINE testZTreeRenumber()
      CALL testZTree%init(1,4,1,4,1,4,1)
      !Degenerate cases
      CALL testZTree%renumber(-1)
      DO k=1,4
        DO j=1,4
          DO i=1,4
            ASSERT(testZTree%ijk2oneD(i,j,k) == MortonIndex(i-1,j-1,k-1)+1,'%renumber(-1)')
            FINFO() '(i,j,k)=',i,j,k
          ENDDO
        ENDDO
      ENDDO

      testZTree%istt=-1
      CALL testZTree%renumber(1)
      DO k=1,4
        DO j=1,4
          DO i=1,4
            ASSERT(testZTree%ijk2oneD(i,j,k) == MortonIndex(i-1,j-1,k-1)+1,'%renumber(1)')
            FINFO() '(i,j,k)=',i,j,k
          ENDDO
        ENDDO
      ENDDO

      !Correctness
      testZTree%istt=1
      CALL testZTree%renumber(10)
      DO k=1,4
        DO j=1,4
          DO i=1,4
            ASSERT(testZTree%ijk2oneD(i,j,k) == MortonIndex(i-1,j-1,k-1)+10,'%renumber(10)')
            FINFO() '(i,j,k)=',i,j,k
          ENDDO
        ENDDO
      ENDDO
      CALL testZTree%clear()
    ENDSUBROUTINE testZTreeRenumber
!
!-------------------------------------------------------------------------------
!Test %shave
    SUBROUTINE testZTreeShave()
      COMPONENT_TEST('%shave(...) Degenerate cases')
      CALL testZTree%init(1,4,1,4,1,1,1)
      rx=1; ry=1; rz=1
      CALL testZTree%shave(rx,ry,rz)
      ASSERT(testZTree%istp == 15,'%istp (1,1,1)')
      ASSERT(testZTree%subdomains(1)%nsubdomains == 3,'%subdomains(1)%nsubdomains (1,1,1)')
      ASSERT(testZTree%subdomains(4)%istt == 12,'%subdomains(4)%istt (1,1,1)')
      ASSERT(testZTree%ijk2oneD(1,1,1) == -1,'%ijk2oneD(1,1,1) (1,1,1)')
      rx=4
      CALL testZTree%shave(rx,ry,rz)
      ASSERT(testZTree%istp == 14,'%istp (4,1,1)')
      ASSERT(testZTree%subdomains(2)%nsubdomains == 3,'%subdomains(2)%nsubdomains (4,1,1)')
      ASSERT(testZTree%subdomains(4)%istt == 11,'%subdomains(4)%istt (4,1,1)')
      ASSERT(testZTree%ijk2oneD(4,1,1) == -1,'%ijk2oneD(4,1,1) (4,1,1)')
      rx=1; ry=4
      CALL testZTree%shave(rx,ry,rz)
      ASSERT(testZTree%istp == 13,'%istp (1,4,1)')
      ASSERT(testZTree%subdomains(3)%nsubdomains == 3,'%subdomains(3)%nsubdomains (1,4,1)')
      ASSERT(testZTree%subdomains(4)%istt == 10,'%subdomains(4)%istt (1,4,1)')
      ASSERT(testZTree%ijk2oneD(1,4,1) == -1,'%ijk2oneD(1,4,1) (1,4,1)')
      rx=4
      CALL testZTree%shave(rx,ry,rz)
      ASSERT(testZTree%istp == 12,'%istp (4,4,1)')
      ASSERT(testZTree%subdomains(4)%nsubdomains == 3,'%subdomains(4)%nsubdomains (4,4,1)')
      ASSERT(testZTree%subdomains(4)%istt == 10,'%subdomains(4)%istt (4,4,1)')
      ASSERT(testZTree%ijk2oneD(4,4,1) == -1,'%ijk2oneD(4,4,1) (4,4,1)')
      rx=(/0,5/); ry=(/0,5/); rz=(/0,5/)
      CALL testZTree%shave(rx,ry,rz)
      ASSERT(testZTree%istt == -1,'%istt (0:5,0:5,0:5)')

      COMPONENT_TEST('%shave((/2,2/),(/2,2/),(/1,1/))')
      CALL testZTree%init(1,4,1,4,1,1,1)
      rx=2; ry=2; rz=1
      CALL testZTree%shave(rx,ry,rz)
      ASSERT(testZTree%istp == 15,'%istp')
      ASSERT(testZTree%subdomains(1)%nsubdomains == 3,'%subdomains(1)%nsubdomains')
      ASSERT(testZTree%subdomains(4)%istt == 12,'%subdomains(4)%istt')
      ASSERT(testZTree%ijk2oneD(2,2,1) == -1,'%ijk2oneD(2,2,1)')

      COMPONENT_TEST('%shave((/2,4/),(/2,4/),(/1,1/))')
      rx=(/3,4/); ry=(/3,4/)
      CALL testZTree%shave(rx,ry,rz)
      ASSERT(testZTree%istp == 11,'%istp')
      ASSERT(testZTree%nsubdomains == 3,'%nsubdomains')
      ASSERT(testZTree%subdomains(3)%istt == 8,'%subdomains(3)%istt')
      DO j=3,4
        DO i=3,4
          ASSERT(testZTree%ijk2oneD(i,j,1) == -1,'%ijk2oneD(i,j,1)')
          FINFO() '(i,j)=',i,j
        ENDDO
      ENDDO
      CALL testZTree%clear()

      COMPONENT_TEST('%shave((/3,4/),(/1,2/),(/1,1/))')
      CALL testZTree%init(1,4,1,4,1,1,1)
      rx=4; ry=4; rz=1
      CALL testZTree%shave(rx,ry,rz)
      rx=(/3,4/); ry=(/1,2/)
      CALL testZTree%shave(rx,ry,rz)
      ASSERT(testZTree%istp == 11,'%istp')
      ASSERT(testZTree%nsubdomains == 3,'%nsubdomains')
      ASSERT(testZTree%subdomains(3)%istt == 9,'%subdomains(3)%istt')
      ASSERT(testZTree%ijk2oneD(4,4,1) == -1,'%ijk2oneD(4,4,1)')
      DO j=1,2
        DO i=3,4
          ASSERT(testZTree%ijk2oneD(i,j,1) == -1,'%ijk2oneD(i,j,1)')
          FINFO() '(i,j)=',i,j
        ENDDO
      ENDDO
      CALL testZTree%clear()

      COMPONENT_TEST('%shave((/2,3/),(/2,3/),(/2,3/))')
      CALL testZTree%init(1,4,1,4,1,4,1)
      rx=(/2,3/); ry=(/2,3/); rz=(/2,3/)
      CALL testZTree%shave(rx,ry,rz)
      ASSERT(testZTree%istp == 56,'%istp')
      ASSERT(testZTree%nsubdomains == 8,'%nsubdomains')
      ASSERT(testZTree%subdomains(8)%istt == 50,'%subdomains(8)%istt')
      DO k=2,3
        DO j=2,3
          DO i=2,3
            ASSERT(testZTree%ijk2oneD(i,j,k) == -1,'%ijk2oneD(i,j,k)')
            FINFO() '(i,j,k)=',i,j,k
          ENDDO
        ENDDO
      ENDDO

      COMPONENT_TEST('%shave(...) (already shaved)')
      rx=2; ry=2; rz=2
      CALL testZTree%shave(rx,ry,rz)
      ASSERT(testZTree%istp == 56,'%istp')
      ASSERT(testZTree%nsubdomains == 8,'%nsubdomains')
      ASSERT(testZTree%subdomains(8)%istt == 50,'%subdomains(8)%istt')
      DO k=2,3
        DO j=2,3
          DO i=2,3
            ASSERT(testZTree%ijk2oneD(i,j,k) == -1,'%ijk2oneD(i,j,k)')
            FINFO() '(i,j,k)=',i,j,k
          ENDDO
        ENDDO
      ENDDO

      COMPONENT_TEST('%shave(...) (out of bounds)')
      rx=25; ry=25; rz=25
      CALL testZTree%shave(rx,ry,rz)
      ASSERT(testZTree%istp == 56,'%istp')
      ASSERT(testZTree%nsubdomains == 8,'%nsubdomains')
      ASSERT(testZTree%subdomains(8)%istt == 50,'%subdomains(8)%istt')
      DO k=2,3
        DO j=2,3
          DO i=2,3
            ASSERT(testZTree%ijk2oneD(i,j,k) == -1,'%ijk2oneD(i,j,k)')
            FINFO() '(i,j,k)=',i,j,k
          ENDDO
        ENDDO
      ENDDO
      CALL testZTree%clear()
    ENDSUBROUTINE testZTreeShave
!
!-------------------------------------------------------------------------------
!Test %getMaxLevels
    SUBROUTINE testZTreeGetMaxLevels()
      CALL testZTree%clear()
      ASSERT(testZTree%getMaxLevels(0) == -1,'%getMaxLevels(0) (uninit)')
      CALL testZTree%init(1,4,1,4,1,1,1)
      ASSERT(testZTree%getMaxLevels(-1) == -1,'%getMaxLevels(-1)')
      ASSERT(testZTree%getMaxLevels(0) == 2,'%getMaxLevels(0) 4x4x1')
      CALL testZTree%clear()
      CALL testZTree%init(1,1,1,1,1,3,1)
      ASSERT(testZTree%getMaxLevels(0) == 2,'%getMaxLevels(0) 1x1x3')
      CALL testZTree%clear()
      CALL testZTree%init(1,8,1,8,1,35,1)
      ASSERT(testZTree%getMaxLevels(0) == 6,'%getMaxLevels(0) big')
      CALL testZTree%clear()
      CALL testZTree%init(1,2,1,1,1,1,1)
      ASSERT(testZTree%getMaxLevels(0) == 1,'%getMaxLevels(0) 2x1x1')
      CALL testZTree%clear()
      CALL testZTree%init(1,1,1,1,1,1,1)
      ASSERT(testZTree%getMaxLevels(0) == 0,'%getMaxLevels(0) 1x1x1')
      CALL testZTree%clear()
    ENDSUBROUTINE testZTreeGetMaxLevels
!
!-------------------------------------------------------------------------------
!Test %getNDomains
    SUBROUTINE testZTreeGetNDomains()
      ASSERT(testZTree%getNDomains(4) == 0,'%getNDomains(4) (uninit)')
      ASSERT(testZTree%getNDomains(0) == 0,'%getNDomains(0) (uninit)')
      CALL testZTree%init(1,4,1,4,1,1,1)
      ASSERT(testZTree%getNDomains(0) == 1,'%getNDomains(0) 4x4x1')
      ASSERT(testZTree%getNDomains(1) == 4,'%getNDomains(1) 4x4x1')
      ASSERT(testZTree%getNDomains(2) == 16,'%getNDomains(2) 4x4x1')
      ASSERT(testZTree%getNDomains(3) == 0,'%getNDomains(3) 4x4x1')
      CALL testZTree%clear()
      CALL testZTree%init(1,1,1,1,1,3,1)
      ASSERT(testZTree%getNDomains(1) == 2,'%getNDomains(1) 1x1x3')
      ASSERT(testZTree%getNDomains(2) == 2,'%getNDomains(2) 1x1x3')
      CALL testZTree%clear()
      CALL testZTree%init(1,8,1,8,1,35,1)
      ASSERT(testZTree%getNDomains(1) == 2,'%getNDomains(1) 8x8x35')
      ASSERT(testZTree%getNDomains(2) == 4,'%getNDomains(2) 8x8x35')
      ASSERT(testZTree%getNDomains(3) == 32,'%getNDomains(3) 8x8x35')
      ASSERT(testZTree%getNDomains(4) == 256,'%getNDomains(4) 8x8x35')
      ASSERT(testZTree%getNDomains(5) == 2048,'%getNDomains(5) 8x8x35')
      ASSERT(testZTree%getNDomains(6) == 384,'%getNDomains(6) 8x8x35')
      CALL testZTree%clear()
    ENDSUBROUTINE testZTreeGetNDomains
!
!-------------------------------------------------------------------------------
!Test %getNodeBounds
    SUBROUTINE testZTreeGetSubNodeBounds()
      CALL testZTree%getSubNodeBounds(1,3,istt,istp)
      ASSERT(ALL((/istt,istp/) == -1),'(1,3,...) (uninit)')
      CALL testZTree%getSubNodeBounds(0,1,istt,istp)
      ASSERT(ALL((/istt,istp/) == -1),'(0,1,...) (uninit)')
      CALL testZTree%init(1,4,1,4,1,1,1)
      CALL testZTree%getSubNodeBounds(0,1,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/1,16/)),'(0,1,...) 4x4x1')
      CALL testZTree%getSubNodeBounds(1,3,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/9,12/)),'(1,3,...) 4x4x1')
      CALL testZTree%getSubNodeBounds(2,3,istt,istp)
      ASSERT(ALL((/istt,istp/) == 3),'(2,3,...) 4x4x1')
      CALL testZTree%getSubNodeBounds(2,13,istt,istp)
      ASSERT(ALL((/istt,istp/) == 13),'(2,13,...) 4x4x1')
      CALL testZTree%getSubNodeBounds(3,13,istt,istp)
      ASSERT(ALL((/istt,istp/) == -1),'(3,13,...) 4x4x1')
      CALL testZTree%clear()
      CALL testZTree%init(1,8,1,8,1,35,1)
      CALL testZTree%getSubNodeBounds(6,13,istt,istp)
      ASSERT(ALL((/istt,istp/) == 821),'(6,13,...) 8x8x35')
      CALL testZTree%getSubNodeBounds(5,400,istt,istp)
      ASSERT(ALL((/istt,istp/) == 400),'(5,400,...) 8x8x35')
      CALL testZTree%getSubNodeBounds(6,400,istt,istp)
      ASSERT(ALL((/istt,istp/) == -1),'(6,400,...) 8x8x35')
      CALL testZTree%clear()
    ENDSUBROUTINE testZTreeGetSubNodeBounds
!
!-------------------------------------------------------------------------------
!Test %getSubNodePointer
    SUBROUTINE testZTreeGetSubNodePointer()
      TYPE(ZTreeNodeType),POINTER :: pZTree

      CALL testZTree%getSubNodePointer(1,3,pZTree)
      ASSERT(.NOT.ASSOCIATED(pZTree),'(1,3,...) (uninit)')
      CALL testZTree%getSubNodePointer(0,1,pZTree)
      ASSERT(ASSOCIATED(pZTree,testZTree),'(0,1,...) (uninit)')
      CALL testZTree%init(1,4,1,4,1,1,1)
      CALL testZTree%getSubNodePointer(0,1,pZTree)
      CALL testZTree%getSubNodeBounds(0,1,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/pZTree%istt,pZTree%istp/)),'(0,1,...) 4x4x1')
      CALL testZTree%getSubNodePointer(1,3,pZTree)
      CALL testZTree%getSubNodeBounds(1,3,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/pZTree%istt,pZTree%istp/)),'(1,3,...) 4x4x1')
      CALL testZTree%getSubNodePointer(2,3,pZTree)
      CALL testZTree%getSubNodeBounds(2,3,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/pZTree%istt,pZTree%istp/)),'(2,3,...) 4x4x1')
      CALL testZTree%getSubNodePointer(2,13,pZTree)
      CALL testZTree%getSubNodeBounds(2,13,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/pZTree%istt,pZTree%istp/)),'(2,13,...) 4x4x1')
      CALL testZTree%getSubNodePointer(3,13,pZTree)
      ASSERT(.NOT.ASSOCIATED(pZTree),'(3,13,...) 4x4x1')
      CALL testZTree%clear()
      CALL testZTree%init(1,8,1,8,1,35,1)
      CALL testZTree%getSubNodePointer(6,13,pZTree)
      CALL testZTree%getSubNodeBounds(6,13,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/pZTree%istt,pZTree%istp/)),'(6,13,...) 8x8x35')
      CALL testZTree%getSubNodePointer(5,400,pZTree)
      CALL testZTree%getSubNodeBounds(5,400,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/pZTree%istt,pZTree%istp/)),'(5,400,...) 8x8x35')
      CALL testZTree%getSubNodePointer(6,400,pZTree)
      ASSERT(.NOT.ASSOCIATED(pZTree),'(6,400,...) 8x8x35')
      CALL testZTree%clear()
    ENDSUBROUTINE testZTreeGetSubNodePointer
!
!-------------------------------------------------------------------------------
!Test %getLeafNodePointer
    SUBROUTINE testZTreeGetLeafNodePointer()
      TYPE(ZTreeNodeType),POINTER :: pZTree

      CALL testZTree%getLeafNodePointer(1,pZTree)
      ASSERT(.NOT.ASSOCIATED(pZTree),'(1,...) (uninit)')
      CALL testZTree%getLeafNodePointer(-1,pZTree)
      ASSERT(.NOT.ASSOCIATED(pZTree),'(-1,...) (uninit)')
      CALL testZTree%init(1,4,1,4,1,4,1)
      CALL testZTree%getLeafNodePointer(65,pZTree)
      ASSERT(.NOT.ASSOCIATED(pZTree),'(65,...) 4x4x4')
      DO i=1,64
        CALL testZTree%getLeafNodePointer(i,pZTree)
        ASSERT(ALL((/pZTree%istt,pZTree%istp/) == i),'(i,...) %istt %istp 4x4x4')
        FINFO() 'i=',i
        ASSERT(pZTree%nsubdomains == 0,'(i,...) %nsubdomains 4x4x4')
        FINFO() 'i=',i
        ASSERT(.NOT.ASSOCIATED(pZTree%subdomains),'(i,...) %subdomains 4x4x4')
        FINFO() 'i=',i
      ENDDO
      CALL testZTree%clear()
    ENDSUBROUTINE testZTreeGetLeafNodePointer
!
!-------------------------------------------------------------------------------
!Test %getLeafNodePointer
    SUBROUTINE testZTreeflattenLeafs()
      !Test uninitialized state
      CALL testZTree%flattenLeafs()
      ASSERT(ALL(testZTree%x == 0),'%x')
      ASSERT(ALL(testZTree%y == 0),'%y')
      ASSERT(ALL(testZTree%z == 0),'%z')
      ASSERT(testZTree%istt == -1,'%istt')
      ASSERT(testZTree%istp == -1,'%istp')
      ASSERT(testZTree%nsubdomains == 0,'%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(testZTree%subdomains),'%subdomains')

      CALL testZTree%init(1,5,1,5,1,1,1)
      CALL testZTree%flattenLeafs()

      ASSERT(testZTree%getMaxLevels(0) == 2,'%getMaxLevels(0) 5x5x1')
      ASSERT(testZTree%getNDomains(2) == 25,'%getNDomains(2) 5x5x1')
      ASSERT(testZTree%subdomains(1)%nsubdomains == 4,'%subdomains(1)%nsubdomains 5x5x1')
      ASSERT(testZTree%subdomains(1)%istt == 1,'%subdomains(1)%istt 5x5x1')
      ASSERT(testZTree%subdomains(1)%istp == 4,'%subdomains(1)%istp 5x5x1')
      ASSERT(testZTree%subdomains(2)%nsubdomains == 6,'%subdomains(1)%nsubdomains 5x5x1')
      ASSERT(testZTree%subdomains(2)%istt == 5,'%subdomains(1)%istt 5x5x1')
      ASSERT(testZTree%subdomains(2)%istp == 10,'%subdomains(1)%istp 5x5x1')
      ASSERT(testZTree%subdomains(3)%nsubdomains == 6,'%subdomains(1)%nsubdomains 5x5x1')
      ASSERT(testZTree%subdomains(3)%istt == 11,'%subdomains(1)%istt 5x5x1')
      ASSERT(testZTree%subdomains(3)%istp == 16,'%subdomains(1)%istp 5x5x1')
      ASSERT(testZTree%subdomains(4)%nsubdomains == 9,'%subdomains(1)%nsubdomains 5x5x1')
      ASSERT(testZTree%subdomains(4)%istt == 17,'%subdomains(1)%istt 5x5x1')
      ASSERT(testZTree%subdomains(4)%istp == 25,'%subdomains(1)%istp 5x5x1')

      CALL testZTree%clear()
    ENDSUBROUTINE testZTreeflattenLeafs
!
!-------------------------------------------------------------------------------
!Test %getLeafNodePointer
    SUBROUTINE testZTreeAddtoLeafs()

      CALL testZTree%addToLeafs(2,2,2)
      ASSERT(ALL(testZTree%x == 0),'%x')
      ASSERT(ALL(testZTree%y == 0),'%y')
      ASSERT(ALL(testZTree%z == 0),'%z')
      ASSERT(testZTree%istt == -1,'%istt')
      ASSERT(testZTree%istp == -1,'%istp')
      ASSERT(testZTree%nsubdomains == 0,'%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(testZTree%subdomains),'%subdomains')

      CALL testZTree%init(1,1,1,1,1,1,1)
      CALL testZTree%addToLeafs(0,2,2)
      ASSERT(ALL(testZTree%x == 1),'%x')
      ASSERT(ALL(testZTree%y == 1),'%y')
      ASSERT(ALL(testZTree%z == 1),'%z')
      ASSERT(testZTree%istt == 1,'%istt')
      ASSERT(testZTree%istp == 1,'%istp')
      ASSERT(testZTree%nsubdomains == 0,'%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(testZTree%subdomains),'%subdomains')
      CALL testZTree%addToLeafs(2,0,2)
      ASSERT(ALL(testZTree%x == 1),'%x')
      ASSERT(ALL(testZTree%y == 1),'%y')
      ASSERT(ALL(testZTree%z == 1),'%z')
      ASSERT(testZTree%istt == 1,'%istt')
      ASSERT(testZTree%istp == 1,'%istp')
      ASSERT(testZTree%nsubdomains == 0,'%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(testZTree%subdomains),'%subdomains')
      CALL testZTree%addToLeafs(2,2,0)
      ASSERT(ALL(testZTree%x == 1),'%x')
      ASSERT(ALL(testZTree%y == 1),'%y')
      ASSERT(ALL(testZTree%z == 1),'%z')
      ASSERT(testZTree%istt == 1,'%istt')
      ASSERT(testZTree%istp == 1,'%istp')
      ASSERT(testZTree%nsubdomains == 0,'%nsubdomains')
      ASSERT(.NOT.ASSOCIATED(testZTree%subdomains),'%subdomains')
      CALL testZTree%addToLeafs(2,2,2)
      ASSERT(ALL(testZTree%x == (/1,2/)),'%x 2x2x2')
      ASSERT(ALL(testZTree%y == (/1,2/)),'%y 2x2x2')
      ASSERT(ALL(testZTree%z == (/1,2/)),'%z 2x2x2')
      ASSERT(testZTree%istt == 1,'%istt 2x2x2')
      ASSERT(testZTree%istp == 8,'%istp 2x2x2')
      ASSERT(testZTree%nsubdomains == 8,'%nsubdomains 2x2x2')
      ASSERT(testZTree%subdomains(1)%istt == 1,'%subdomains(1)%istt 2x2x2')
      ASSERT(testZTree%subdomains(1)%istp == 1,'%subdomains(1)%istp 2x2x2')
      ASSERT(ALL(testZTree%subdomains(1)%x == 1),'%subdomains(1)%x 2x2x2')
      ASSERT(ALL(testZTree%subdomains(1)%y == 1),'%subdomains(1)%y 2x2x2')
      ASSERT(ALL(testZTree%subdomains(1)%z == 1),'%subdomains(1)%z 2x2x2')
      ASSERT(testZTree%subdomains(2)%istt == 2,'%subdomains(2)%istt 2x2x2')
      ASSERT(testZTree%subdomains(2)%istp == 2,'%subdomains(2)%istp 2x2x2')
      ASSERT(ALL(testZTree%subdomains(2)%x == 2),'%subdomains(2)%x 2x2x2')
      ASSERT(ALL(testZTree%subdomains(2)%y == 1),'%subdomains(2)%y 2x2x2')
      ASSERT(ALL(testZTree%subdomains(2)%z == 1),'%subdomains(2)%z 2x2x2')
      ASSERT(testZTree%subdomains(3)%istt == 3,'%subdomains(3)%istt 2x2x2')
      ASSERT(testZTree%subdomains(3)%istp == 3,'%subdomains(3)%istp 2x2x2')
      ASSERT(ALL(testZTree%subdomains(3)%x == 1),'%subdomains(3)%x 2x2x2')
      ASSERT(ALL(testZTree%subdomains(3)%y == 2),'%subdomains(3)%y 2x2x2')
      ASSERT(ALL(testZTree%subdomains(3)%z == 1),'%subdomains(3)%z 2x2x2')
      ASSERT(testZTree%subdomains(4)%istt == 4,'%subdomains(4)%istt 2x2x2')
      ASSERT(testZTree%subdomains(4)%istp == 4,'%subdomains(4)%istp 2x2x2')
      ASSERT(ALL(testZTree%subdomains(4)%x == 2),'%subdomains(4)%x 2x2x2')
      ASSERT(ALL(testZTree%subdomains(4)%y == 2),'%subdomains(4)%y 2x2x2')
      ASSERT(ALL(testZTree%subdomains(4)%z == 1),'%subdomains(4)%z 2x2x2')
      ASSERT(testZTree%subdomains(5)%istt == 5,'%subdomains(5)%istt 2x2x2')
      ASSERT(testZTree%subdomains(5)%istp == 5,'%subdomains(5)%istp 2x2x2')
      ASSERT(ALL(testZTree%subdomains(5)%x == 1),'%subdomains(5)%x 2x2x2')
      ASSERT(ALL(testZTree%subdomains(5)%y == 1),'%subdomains(5)%y 2x2x2')
      ASSERT(ALL(testZTree%subdomains(5)%z == 2),'%subdomains(5)%z 2x2x2')
      ASSERT(testZTree%subdomains(6)%istt == 6,'%subdomains(6)%istt 2x2x2')
      ASSERT(testZTree%subdomains(6)%istp == 6,'%subdomains(6)%istp 2x2x2')
      ASSERT(ALL(testZTree%subdomains(6)%x == 2),'%subdomains(6)%x 2x2x2')
      ASSERT(ALL(testZTree%subdomains(6)%y == 1),'%subdomains(6)%y 2x2x2')
      ASSERT(ALL(testZTree%subdomains(6)%z == 2),'%subdomains(6)%z 2x2x2')
      ASSERT(testZTree%subdomains(7)%istt == 7,'%subdomains(7)%istt 2x2x2')
      ASSERT(testZTree%subdomains(7)%istp == 7,'%subdomains(7)%istp 2x2x2')
      ASSERT(ALL(testZTree%subdomains(7)%x == 1),'%subdomains(7)%x 2x2x2')
      ASSERT(ALL(testZTree%subdomains(7)%y == 2),'%subdomains(7)%y 2x2x2')
      ASSERT(ALL(testZTree%subdomains(7)%z == 2),'%subdomains(7)%z 2x2x2')
      ASSERT(testZTree%subdomains(8)%istt == 8,'%subdomains(8)%istt 2x2x2')
      ASSERT(testZTree%subdomains(8)%istp == 8,'%subdomains(8)%istp 2x2x2')
      ASSERT(ALL(testZTree%subdomains(8)%x == 2),'%subdomains(8)%x 2x2x2')
      ASSERT(ALL(testZTree%subdomains(8)%y == 2),'%subdomains(8)%y 2x2x2')
      ASSERT(ALL(testZTree%subdomains(8)%z == 2),'%subdomains(8)%z 2x2x2')
      CALL testZTree%addToLeafs(2,2,2)
      ASSERT(ALL(testZTree%x == (/1,4/)),'%x 4x4x4')
      ASSERT(ALL(testZTree%y == (/1,4/)),'%y 4x4x4')
      ASSERT(ALL(testZTree%z == (/1,4/)),'%z 4x4x4')
      ASSERT(testZTree%istt == 1,'%istt 4x4x4')
      ASSERT(testZTree%istp == 64,'%istp 4x4x4')
      ASSERT(testZTree%nsubdomains == 8,'%nsubdomains 4x4x4')
      ASSERT(testZTree%subdomains(1)%istt == 1,'%subdomains(1)%istt 4x4x4')
      ASSERT(testZTree%subdomains(1)%istp == 8,'%subdomains(1)%istp 4x4x4')
      ASSERT(ALL(testZTree%subdomains(1)%x == (/1,2/)),'%subdomains(1)%x 4x4x4')
      ASSERT(ALL(testZTree%subdomains(1)%y == (/1,2/)),'%subdomains(1)%y 4x4x4')
      ASSERT(ALL(testZTree%subdomains(1)%z == (/1,2/)),'%subdomains(1)%z 4x4x4')
      ASSERT(testZTree%subdomains(2)%istt == 9,'%subdomains(2)%istt 4x4x4')
      ASSERT(testZTree%subdomains(2)%istp == 16,'%subdomains(2)%istp 4x4x4')
      ASSERT(ALL(testZTree%subdomains(2)%x == (/3,4/)),'%subdomains(2)%x 4x4x4')
      ASSERT(ALL(testZTree%subdomains(2)%y == (/1,2/)),'%subdomains(2)%y 4x4x4')
      ASSERT(ALL(testZTree%subdomains(2)%z == (/1,2/)),'%subdomains(2)%z 4x4x4')
      ASSERT(testZTree%subdomains(3)%istt == 17,'%subdomains(3)%istt 4x4x4')
      ASSERT(testZTree%subdomains(3)%istp == 24,'%subdomains(3)%istp 4x4x4')
      ASSERT(ALL(testZTree%subdomains(3)%x == (/1,2/)),'%subdomains(3)%x 4x4x4')
      ASSERT(ALL(testZTree%subdomains(3)%y == (/3,4/)),'%subdomains(3)%y 4x4x4')
      ASSERT(ALL(testZTree%subdomains(3)%z == (/1,2/)),'%subdomains(3)%z 4x4x4')
      ASSERT(testZTree%subdomains(4)%istt == 25,'%subdomains(4)%istt 4x4x4')
      ASSERT(testZTree%subdomains(4)%istp == 32,'%subdomains(4)%istp 4x4x4')
      ASSERT(ALL(testZTree%subdomains(4)%x == (/3,4/)),'%subdomains(4)%x 4x4x4')
      ASSERT(ALL(testZTree%subdomains(4)%y == (/3,4/)),'%subdomains(4)%y 4x4x4')
      ASSERT(ALL(testZTree%subdomains(4)%z == (/1,2/)),'%subdomains(4)%z 4x4x4')
      ASSERT(testZTree%subdomains(5)%istt == 33,'%subdomains(5)%istt 4x4x4')
      ASSERT(testZTree%subdomains(5)%istp == 40,'%subdomains(5)%istp 4x4x4')
      ASSERT(ALL(testZTree%subdomains(5)%x == (/1,2/)),'%subdomains(5)%x 4x4x4')
      ASSERT(ALL(testZTree%subdomains(5)%y == (/1,2/)),'%subdomains(5)%y 4x4x4')
      ASSERT(ALL(testZTree%subdomains(5)%z == (/3,4/)),'%subdomains(5)%z 4x4x4')
      ASSERT(testZTree%subdomains(6)%istt == 41,'%subdomains(6)%istt 4x4x4')
      ASSERT(testZTree%subdomains(6)%istp == 48,'%subdomains(6)%istp 4x4x4')
      ASSERT(ALL(testZTree%subdomains(6)%x == (/3,4/)),'%subdomains(6)%x 4x4x4')
      ASSERT(ALL(testZTree%subdomains(6)%y == (/1,2/)),'%subdomains(6)%y 4x4x4')
      ASSERT(ALL(testZTree%subdomains(6)%z == (/3,4/)),'%subdomains(6)%z 4x4x4')
      ASSERT(testZTree%subdomains(7)%istt == 49,'%subdomains(7)%istt 4x4x4')
      ASSERT(testZTree%subdomains(7)%istp == 56,'%subdomains(7)%istp 4x4x4')
      ASSERT(ALL(testZTree%subdomains(7)%x == (/1,2/)),'%subdomains(7)%x 4x4x4')
      ASSERT(ALL(testZTree%subdomains(7)%y == (/3,4/)),'%subdomains(7)%y 4x4x4')
      ASSERT(ALL(testZTree%subdomains(7)%z == (/3,4/)),'%subdomains(7)%z 4x4x4')
      ASSERT(testZTree%subdomains(8)%istt == 57,'%subdomains(8)%istt 4x4x4')
      ASSERT(testZTree%subdomains(8)%istp == 64,'%subdomains(8)%istp 4x4x4')
      ASSERT(ALL(testZTree%subdomains(8)%x == (/3,4/)),'%subdomains(8)%x 4x4x4')
      ASSERT(ALL(testZTree%subdomains(8)%y == (/3,4/)),'%subdomains(8)%y 4x4x4')
      ASSERT(ALL(testZTree%subdomains(8)%z == (/3,4/)),'%subdomains(8)%z 4x4x4')
      CALL testZTree%clear()

      CALL testZTree%init(1,1,1,1,1,1,1)
      CALL testZTree%addToLeafs(1,1,2)
      CALL testZTree%addToLeafs(1,2,1)
      CALL testZTree%addToLeafs(2,1,1)
      ASSERT(ALL(testZTree%x == (/1,2/)),'%x 2x2x2 binary')
      ASSERT(ALL(testZTree%y == (/1,2/)),'%y 2x2x2 binary')
      ASSERT(ALL(testZTree%z == (/1,2/)),'%z 2x2x2 binary')
      ASSERT(testZTree%istt == 1,'%istt 2x2x2 binary')
      ASSERT(testZTree%istp == 8,'%istp 2x2x2 binary')
      ASSERT(testZTree%nsubdomains == 2,'%nsubdomains 2x2x2 binary')
      ASSERT(testZTree%getMaxLevels(0) == 3,'%getMaxLevels(0) 2x2x2 binary')
      ASSERT(testZTree%getNDomains(1) == 2,'%getNDomains(1) 2x2x2 binary')
      ASSERT(testZTree%getNDomains(2) == 4,'%getNDomains(2) 2x2x2 binary')
      ASSERT(testZTree%getNDomains(3) == 8,'%getNDomains(3) 2x2x2 binary')

      CALL testZTree%clear()
    ENDSUBROUTINE testZTreeAddtoLeafs
!
!-------------------------------------------------------------------------------
!Test %partition
    SUBROUTINE testZTreePartition()
      CALL testZTree%partition(4,0,istt,istp)
      ASSERT(ALL((/istt,istp/) == -1),'%partition(4,0,istt,istp) (uninit)')
      CALL testZTree%init(1,4,1,4,1,1,1)
      CALL testZTree%partition(4,-1,istt,istp)
      ASSERT(ALL((/istt,istp/) == -1),'%partition(4,-1,istt,istp)')
      CALL testZTree%partition(4,4,istt,istp)
      ASSERT(ALL((/istt,istp/) == -1),'%partition(4,4,istt,istp)')
      CALL testZTree%partition(4,0,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/1,4/)),'%partition(4,0,istt,istp)')
      CALL testZTree%partition(16,2,istt,istp)
      ASSERT(ALL((/istt,istp/) == 3),'%partition(16,2,istt,istp)')
      CALL testZTree%partition(3,0,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/1,8/)),'%partition(3,0,istt,istp)')
      CALL testZTree%partition(3,1,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/9,12/) ),'%partition(3,1,istt,istp)')
      CALL testZTree%partition(3,2,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/13,16/)),'%partition(3,2,istt,istp)')
      CALL testZTree%partition(5,0,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/1,4/)),'%partition(5,0,istt,istp)')
      CALL testZTree%partition(5,1,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/5,7/)),'%partition(5,1,istt,istp)')
      CALL testZTree%partition(5,2,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/8,10/)),'%partition(5,2,istt,istp)')
      CALL testZTree%partition(5,3,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/11,13/)),'%partition(5,3,istt,istp)')
      CALL testZTree%partition(5,4,istt,istp)
      ASSERT(ALL((/istt,istp/) == (/14,16/)),'%partition(5,4,istt,istp)')
      CALL testZTree%clear()
    ENDSUBROUTINE testZTreePartition

!
!-------------------------------------------------------------------------------
! This routine tests the initSingle and addChild routines, which are used for
! more exotic indexing schemes.
    SUBROUTINE testDeferredConst()
      TYPE(ZTreeNodeType) :: root
      TYPE(ZTreeNodeType),POINTER :: child,child2
      INTEGER(SIK) :: nx,ny,nz,nz_block
      INTEGER(SIK) :: i,ip
      INTEGER(SIK) :: istt,istp
      LOGICAL(SBK) :: bool

      nx=5
      ny=5
      nz=12
      nz_block=4
      CALL root%initSingle(1,nx,1,ny,1,nz,1,nz_block)
      DO i=1,nz_block
        istt=(i-1)*nz/nz_block+1
        istp=i*nz/nz_block
        child => root%addChild(1,nx,1,ny,istt,istp,nz/nz_block)
        DO ip=1,nz/nz_block
          child2 => child%addChild(1,nx,1,ny,ip,ip)
        ENDDO
      ENDDO

      ! make sure things are all good at each level
      ASSERT(root%istt == 1,"Bad starting index for root.")
      ASSERT(root%istp == 300,"Bad stopping index for root.")
      ASSERT(root%nsubdomains == 4, "Bad number of subdomains for root.")
      ASSERT(ALL(root%x == (/1,nx/)), "Bad X bounds on root.")
      ASSERT(ALL(root%y == (/1,ny/)), "Bad Y bounds on root.")
      ASSERT(ALL(root%z == (/1,nz/)), "Bad Z bounds on root.")

      bool=(root%subdomains(1)%istt == 1)
      ASSERT(bool, "Bad starting index for subdomain.")
      bool=(root%subdomains(1)%istp == 75)
      ASSERT(bool, "Bad stopping index for subdomain.")

      bool=(root%subdomains(2)%istt == 76)
      ASSERT(bool, "Bad starting index for subdomain.")
      bool=(root%subdomains(2)%istp == 150)
      ASSERT(bool, "Bad stopping index for subdomain.")

      bool=(root%subdomains(3)%istt == 151)
      ASSERT(bool, "Bad starting index for subdomain.")
      bool=(root%subdomains(3)%istp == 225)
      ASSERT(bool, "Bad stopping index for subdomain.")

      bool=(root%subdomains(4)%istt == 226)
      ASSERT(bool, "Bad starting index for subdomain.")
      bool=(root%subdomains(4)%istp == 300)
      ASSERT(bool, "Bad stopping index for subdomain.")

      bool=(root%subdomains(4)%nsubdomains == 3)
      ASSERT(bool, "Bad number of subdomains for subdomain.")

      bool=(root%subdomains(3)%subdomains(2)%istt == 176)
      ASSERT(bool, "Bad number of subdomains for subdomain.")
      CALL root%clear()
    ENDSUBROUTINE testDeferredConst
!
ENDPROGRAM testSpaceFillingCurve
