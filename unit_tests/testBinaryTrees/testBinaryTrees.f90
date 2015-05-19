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
PROGRAM testBinaryTrees
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE BinaryTrees
  
  IMPLICIT NONE

  TYPE(BinaryTreeType_Integer),POINTER :: t => NULL()
  TYPE(BinaryTreeType_Index),POINTER :: t2 => NULL()
  INTEGER(SIK),ALLOCATABLE :: emptyi(:)
  LOGICAL(SBK) :: bool

  CREATE_TEST('Binary Trees')

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING BINARY TREES...'
  WRITE(*,*) '==================================================='
  
  WRITE(*,*) 'TESTING INTEGER BINARY TREE'
  
  ALLOCATE(emptyi(1:0))
  CALL CreateBinaryTree(t,emptyi)
  CALL BurnBinaryTree(t)
  CALL CreateBinaryTreeRmDup(t,(/4,8,4,6,7,2,1/))
  ASSERT(t%val == 4, 'CreateBinaryTreeRmDup(t,...)')
  WRITE(*,*) '  Passed: CALL CreateBinaryTreeRmDup(t,...)'
  CALL BurnBinaryTree(t)
  CALL CreateBinaryTree(t,(/8,3,4,6,7,2,1/))
  ASSERT(t%val == 8, 'CreateBinaryTree(t,...)')
  WRITE(*,*) '  Passed: CALL CreateBinaryTree(t,...)'
  CALL CreateBinaryTree(t,(/8,3,4,6,7,2,1/))
  bool = .NOT.(.NOT.t%match(2) .AND. t%match(10) .AND. .NOT.t%match(4))
  ASSERT(bool, 't%match(...)')
  WRITE(*,*) '  Passed: t%match(...)'

  CALL BurnBinaryTree(t)
  ASSERT(.NOT.ASSOCIATED(t), 'BurnBinaryTree(t)')
  WRITE(*,*) '  Passed: CALL BurnBinaryTree(t)'
  WRITE(*,*) '---------------------------------------------------'
  
  WRITE(*,*) 'TESTING INDEXED BINARY TREE'
  CALL CreateBinaryTreeRmDup(t2,(/4,8,4,6,7,2,1/),1,7)
  ASSERT(t2%val == 4, 'CreateBinaryTreeRmDup(t,...)')
  WRITE(*,*) '  Passed: CALL CreateBinaryTreeRmDup(t,...)'
  CALL BurnBinaryTree(t2)
  CALL CreateBinaryTree(t2,(/8,3,4,6,7,2,1/))
  ASSERT(t2%val == 8, 'CreateBinaryTree(t2,...)')
  WRITE(*,*) '  Passed: CALL CreateBinaryTree(t2,...)'
  CALL CreateBinaryTree(t2,(/8,3,4,6,7,2,1/),1,7)
  bool = .NOT.(.NOT.t2%match(2) .AND. t2%match(10) .AND. .NOT.t2%match(4))
  ASSERT(bool, 't2%match(...)')
  WRITE(*,*) '  Passed: t2%match(...)'
  bool = .NOT.(t2%findIndex(4) /= 3 .AND. t2%findIndex(2) /= 6 .AND. &
               t2%findIndex(10) /= -1)
  ASSERT(bool, 't2%findIndex(...)')
  WRITE(*,*) '  Passed: t2%findIndex(...)'

  CALL BurnBinaryTree(t2)
  ASSERT(.NOT.ASSOCIATED(t), 'BurnBinaryTree(t2)')
  WRITE(*,*) '  Passed: CALL BurnBinaryTree(t2)'
  
  CALL SortedBinaryTree(t2,1,(/1,2,3,4,5,6/))
  bool = .NOT.((t2%val /= 4 .AND. t2%index /= 4) .AND. &
               (t2%left%val /= 2 .AND. t2%left%index /= 2) .AND. &
               (t2%right%val /= 6 .AND. t2%right%index /= 6) .AND. &
               (t2%left%left%val /= 1 .AND. t2%left%left%index /= 1) .AND. &
               (t2%left%right%val /= 3 .AND. t2%left%right%index /= 3) .AND. &
               (t2%right%left%val /= 5 .AND. t2%right%left%index /= 5))
  ASSERT(bool, 'SortedBinaryTree(...)')
  WRITE(*,*) '  Passed: CALL SortedBinaryTree(...)'
  CALL BurnBinaryTree(t2)
  WRITE(*,*) '---------------------------------------------------'

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING BINARY TREES PASSED!'
  WRITE(*,*) '==================================================='

  FINALIZE_TEST()

ENDPROGRAM testBinaryTrees
