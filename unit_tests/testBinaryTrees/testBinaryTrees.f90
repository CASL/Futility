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
  USE Utils
  
  IMPLICIT NONE

  TYPE(BinaryTreeType_Integer),POINTER :: t => NULL()
  TYPE(BinaryTreeType_Index),POINTER :: t2 => NULL()
  INTEGER(SIK),ALLOCATABLE :: emptyi(:)

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING BINARY TREES...'
  WRITE(*,*) '==================================================='
  
  WRITE(*,*) 'TESTING INTEGER BINARY TREE'
  
  ALLOCATE(emptyi(1:0))
  CALL CreateBinaryTree(t,emptyi)
  CALL BurnBinaryTree(t)
  CALL CreateBinaryTreeRmDup(t,(/4,8,4,6,7,2,1/))
  IF(t%val /= 4) THEN
    WRITE(*,*) 'CALL CreateBinaryTreeRmDup(t,...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL CreateBinaryTreeRmDup(t,...)'
  ENDIF
  CALL BurnBinaryTree(t)
  CALL CreateBinaryTree(t,(/8,3,4,6,7,2,1/))
  IF(t%val /= 8) THEN
    WRITE(*,*) 'CALL CreateBinaryTree(t,...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL CreateBinaryTree(t,...)'
  ENDIF
  CALL CreateBinaryTree(t,(/8,3,4,6,7,2,1/))
  IF(.NOT.t%match(2) .AND. t%match(10) .AND. .NOT.t%match(4)) THEN
    WRITE(*,*) 't%match(...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: t%match(...)'
  ENDIF

  CALL BurnBinaryTree(t)
  IF(ASSOCIATED(t)) THEN
    WRITE(*,*) 'CALL BurnBinaryTree(t) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL BurnBinaryTree(t)'
  ENDIF
  WRITE(*,*) '---------------------------------------------------'
  
  WRITE(*,*) 'TESTING INDEXED BINARY TREE'
  CALL CreateBinaryTreeRmDup(t2,(/4,8,4,6,7,2,1/),1,7)
  IF(t2%val /= 4) THEN
    WRITE(*,*) 'CALL CreateBinaryTreeRmDup(t,...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL CreateBinaryTreeRmDup(t,...)'
  ENDIF
  CALL BurnBinaryTree(t2)
  CALL CreateBinaryTree(t2,(/8,3,4,6,7,2,1/))
  IF(t2%val /= 8) THEN
    WRITE(*,*) 'CALL CreateBinaryTree(t2,...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL CreateBinaryTree(t2,...)'
  ENDIF
  CALL CreateBinaryTree(t2,(/8,3,4,6,7,2,1/),1,7)
  IF(.NOT.t2%match(2) .AND. t2%match(10) .AND. .NOT.t2%match(4)) THEN
    WRITE(*,*) 't2%match(...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: t2%match(...)'
  ENDIF
  IF(t2%findIndex(4) /= 3 .AND. t2%findIndex(2) /= 6 .AND. &
     t2%findIndex(10) /= -1) THEN
    WRITE(*,*) 't2%findIndex(...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: t2%findIndex(...)'
  ENDIF

  CALL BurnBinaryTree(t2)
  IF(ASSOCIATED(t)) THEN
    WRITE(*,*) 'CALL BurnBinaryTree(t2) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL BurnBinaryTree(t2)'
  ENDIF
  
  CALL SortedBinaryTree(t2,1,(/1,2,3,4,5,6/))
  IF((t2%val /= 4 .AND. t2%index /= 4) .AND. &
     (t2%left%val /= 2 .AND. t2%left%index /= 2) .AND. &
     (t2%right%val /= 6 .AND. t2%right%index /= 6) .AND. &
     (t2%left%left%val /= 1 .AND. t2%left%left%index /= 1) .AND. &
     (t2%left%right%val /= 3 .AND. t2%left%right%index /= 3) .AND. &
     (t2%right%left%val /= 5 .AND. t2%right%left%index /= 5)) THEN
     WRITE(*,*) 'CALL SortedBinaryTree(...) FAILED!'
     STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL SortedBinaryTree(...)'
  ENDIF
  CALL BurnBinaryTree(t2)
  WRITE(*,*) '---------------------------------------------------'

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING BINARY TREES PASSED!'
  WRITE(*,*) '==================================================='

ENDPROGRAM testBinaryTrees
