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
PROGRAM testAllocsOOM
#include "UnitTest.h"
  USE IntrType
  USE Allocs
  USE UnitTest
  IMPLICIT NONE
  
  LOGICAL(SBK) :: test
  
  CREATE_TEST('AllocsOOM')
  
  CALL eAllocs%setStopOnError(.FALSE.)
  CALL eAllocs%setQuietMode(.TRUE.)
  REGISTER_SUBTEST('testTOOBIGP()',testTOOBIGP)
  REGISTER_SUBTEST('testTOOBIGA()',testTOOBIGA)
  
  
  FINALIZE_TEST()
!
!===============================================================================  
  CONTAINS
!
!-------------------------------------------------------------------------------
! Test error checking for over allocation
    SUBROUTINE testTOOBIGP()
!        INTEGER(SNK),POINTER :: i1tb(:)
      INTEGER(SNK),POINTER :: i2tb(:,:)
      INTEGER(SNK),POINTER :: i3tb(:,:,:)
      INTEGER(SNK),POINTER :: i4tb(:,:,:,:)
      INTEGER(SNK),POINTER :: i5tb(:,:,:,:,:)
      INTEGER(SNK),POINTER :: i6tb(:,:,:,:,:,:)
      INTEGER(SNK),POINTER :: i7tb(:,:,:,:,:,:,:)
!        INTEGER(SLK),POINTER :: l1tb(:)
      INTEGER(SLK),POINTER :: l2tb(:,:)
      INTEGER(SLK),POINTER :: l3tb(:,:,:)
      INTEGER(SLK),POINTER :: l4tb(:,:,:,:)
      INTEGER(SLK),POINTER :: l5tb(:,:,:,:,:)
      INTEGER(SLK),POINTER :: l6tb(:,:,:,:,:,:)
      INTEGER(SLK),POINTER :: l7tb(:,:,:,:,:,:,:)
!        LOGICAL(SBK),POINTER :: b1tb(:)
      LOGICAL(SBK),POINTER :: b2tb(:,:)
      LOGICAL(SBK),POINTER :: b3tb(:,:,:)
      LOGICAL(SBK),POINTER :: b4tb(:,:,:,:)
      LOGICAL(SBK),POINTER :: b5tb(:,:,:,:,:)
      LOGICAL(SBK),POINTER :: b6tb(:,:,:,:,:,:)
      LOGICAL(SBK),POINTER :: b7tb(:,:,:,:,:,:,:)
!        REAL(SSK),POINTER :: s1tb(:)
      REAL(SSK),POINTER :: s2tb(:,:)
      REAL(SSK),POINTER :: s3tb(:,:,:)
      REAL(SSK),POINTER :: s4tb(:,:,:,:)
      REAL(SSK),POINTER :: s5tb(:,:,:,:,:)
      REAL(SSK),POINTER :: s6tb(:,:,:,:,:,:)
      REAL(SSK),POINTER :: s7tb(:,:,:,:,:,:,:)
!        REAL(SDK),POINTER :: d1tb(:)
      REAL(SDK),POINTER :: d2tb(:,:)
      REAL(SDK),POINTER :: d3tb(:,:,:)
      REAL(SDK),POINTER :: d4tb(:,:,:,:)
      REAL(SDK),POINTER :: d5tb(:,:,:,:,:)
      REAL(SDK),POINTER :: d6tb(:,:,:,:,:,:)
      REAL(SDK),POINTER :: d7tb(:,:,:,:,:,:,:)
      
      NULLIFY(i2tb,i3tb,i4tb,i5tb,i6tb,i7tb)
      NULLIFY(l2tb,l3tb,l4tb,l5tb,l6tb,l7tb)
      NULLIFY(b2tb,b3tb,b4tb,b5tb,b6tb,b7tb)
      NULLIFY(s2tb,s3tb,s4tb,s5tb,s6tb,s7tb)
      NULLIFY(d2tb,d3tb,d4tb,d5tb,d6tb,d7tb)
      
      ASSERT(SUM(eAllocs%getCounterAll()) == 0,'SUM(eAllocs%getCounterAll())')
      ASSERT(LEN_TRIM(eAllocs%getLastMessage()) == 0,'LEN_TRIM(eAllocs%getLastMessage())')
!
!Test logicals (booleans)
!       WRITE(*,*) '  Skipping: CALL dmallocP(b1tb,2147483647)'
!        CALL dmallocP(b1tb,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocP(b1tb,2147483647)')
!          CALL demallocP(b1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocP(b1tb,2147483647)'
!        ENDIF
!       WRITE(*,*) '  Skipping: CALL dmalloc0P(b1tb,-2147483648,2147483647)'
!        CALL dmalloc0P(b1tb,-2147483648,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0P(b1tb,-2147483648,2147483647)'
!          CALL demallocP(b1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmalloc0P(b1tb,-2147483648,2147483647)'
!        ENDIF
      CALL dmallocP(b2tb,1000000000,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(b2tb,1000000000,1000)')
      CALL dmalloc0P(b2tb,1,1000000000,1,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(b2tb,1,1000000000,1,1000)')
      CALL dmallocP(b3tb,1000000000,1000,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(b3tb,1000000000,1000,1)')
      CALL dmalloc0P(b3tb,1,1000000000,1,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(b3tb,1,1000000000,1,1000,1,1)')
      CALL dmallocP(b4tb,1000000000,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(b4tb,1000000000,1000,1,1)')
      CALL dmalloc0P(b4tb,1,1000000000,1,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(b4tb,1,1000000000,1,1000,1,1,1,1)')
      CALL dmallocP(b5tb,1000000000,1000,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(b5tb,1000000000,1000,1,1,1)')
      CALL dmalloc0P(b5tb,1,1000000000,1,1000,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(b5tb,1,1000000000,1,1000,1,1,1,1,1,1)')
      CALL dmallocP(b6tb,1000000000,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(b6tb,1000000000,1000,1,1,1,1)')
      CALL dmalloc0P(b6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(b6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)')
      CALL dmallocP(b7tb,1000000000,1000,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(b7tb,1000000000,1000,1,1,1,1,1)')
      CALL dmalloc0P(b7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(b7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)')
!
!Test regular integers
!       WRITE(*,*) '  Skipping: CALL dmallocP(i1tb,2147483647)'
!        CALL dmallocP(i1tb,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocP(i1tb,2147483647)')
!          CALL demallocP(i1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocP(i1tb,2147483647)'
!        ENDIF
!       WRITE(*,*) '  Skipping: CALL dmalloc0P(i1tb,-2147483648,2147483647)'
!        CALL dmalloc0P(i1tb,-2147483648,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0P(i1tb,-2147483648,2147483647)'
!          CALL demallocP(i1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmalloc0P(i1tb,-2147483648,2147483647)'
!        ENDIF
      CALL dmallocP(i2tb,1000000000,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(i2tb,1000000000,1000)')
      CALL dmalloc0P(i2tb,1,1000000000,1,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(i2tb,1,1000000000,1,1000)')
      CALL dmallocP(i3tb,1000000000,1000,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(i3tb,1000000000,1000,1)')
      CALL dmalloc0P(i3tb,1,1000000000,1,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(i3tb,1,1000000000,1,1000,1,1)')
      CALL dmallocP(i4tb,1000000000,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(i4tb,1000000000,1000,1,1)')
      CALL dmalloc0P(i4tb,1,1000000000,1,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(i4tb,1,1000000000,1,1000,1,1,1,1)')
      CALL dmallocP(i5tb,1000000000,1000,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(i5tb,1000000000,1000,1,1,1)')
      CALL dmalloc0P(i5tb,1,1000000000,1,1000,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(i5tb,1,1000000000,1,1000,1,1,1,1,1,1)')
      CALL dmallocP(i6tb,1000000000,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(i6tb,1000000000,1000,1,1,1,1)')
      CALL dmalloc0P(i6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(i6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)')
      CALL dmallocP(i7tb,1000000000,1000,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(i7tb,1000000000,1000,1,1,1,1,1)')
      CALL dmalloc0P(i7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(i7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)')
!
!Test long ints
!       WRITE(*,*) '  Skipping: CALL dmallocP(l1tb,2147483647)'
!        CALL dmallocP(l1tb,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocP(l1tb,2147483647)')
!          CALL demallocP(l1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocP(l1tb,2147483647)'
!        ENDIF
!       WRITE(*,*) '  Skipping: CALL dmalloc0P(l1tb,-2147483648,2147483647)'
!        CALL dmalloc0P(l1tb,-2147483648,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0P(l1tb,-2147483648,2147483647)'
!          CALL demallocP(l1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmalloc0P(l1tb,-2147483648,2147483647)'
!        ENDIF
      CALL dmallocP(l2tb,1000000000,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(l2tb,1000000000,1000)')
      CALL dmalloc0P(l2tb,1,1000000000,1,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(l2tb,1,1000000000,1,1000)')
      CALL dmallocP(l3tb,1000000000,1000,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(l3tb,1000000000,1000,1)')
      CALL dmalloc0P(l3tb,1,1000000000,1,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(l3tb,1,1000000000,1,1000,1,1)')
      CALL dmallocP(l4tb,1000000000,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(l4tb,1000000000,1000,1,1)')
      CALL dmalloc0P(l4tb,1,1000000000,1,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(l4tb,1,1000000000,1,1000,1,1,1,1)')
      CALL dmallocP(l5tb,1000000000,1000,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(l5tb,1000000000,1000,1,1,1)')
      CALL dmalloc0P(l5tb,1,1000000000,1,1000,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(l5tb,1,1000000000,1,1000,1,1,1,1,1,1)')
      CALL dmallocP(l6tb,1000000000,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(l6tb,1000000000,1000,1,1,1,1)')
      CALL dmalloc0P(l6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(l6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)')
      CALL dmallocP(l7tb,1000000000,1000,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(l7tb,1000000000,1000,1,1,1,1,1)')
      CALL dmalloc0P(l7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(l7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)')
!
!Test single reals
!       WRITE(*,*) '  Skipping: CALL dmallocP(s1tb,2147483647)'
!        CALL dmallocP(s1tb,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocP(s1tb,2147483647)')
!          CALL demallocP(s1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocP(s1tb,2147483647)'
!        ENDIF
!       WRITE(*,*) '  Skipping: CALL dmalloc0P(s1tb,-2147483648,2147483647)'
!        CALL dmalloc0P(s1tb,-2147483648,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0P(s1tb,-2147483648,2147483647)')
!          CALL demallocP(s1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmalloc0P(s1tb,-2147483648,2147483647)'
!        ENDIF
      CALL dmallocP(s2tb,1000000000,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(s2tb,1000000000,1000)')
      CALL dmalloc0P(s2tb,1,1000000000,1,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(s2tb,1,1000000000,1,1000)')
      CALL dmallocP(s3tb,1000000000,1000,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(s3tb,1000000000,1000,1)')
      CALL dmalloc0P(s3tb,1,1000000000,1,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(s3tb,1,1000000000,1,1000,1,1)')
      CALL dmallocP(s4tb,1000000000,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(s4tb,1000000000,1000,1,1)')
      CALL dmalloc0P(s4tb,1,1000000000,1,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(s4tb,1,1000000000,1,1000,1,1,1,1)')
      CALL dmallocP(s5tb,1000000000,1000,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(s5tb,1000000000,1000,1,1,1)')
      CALL dmalloc0P(s5tb,1,1000000000,1,1000,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(s5tb,1,1000000000,1,1000,1,1,1,1,1,1)')
      CALL dmallocP(s6tb,1000000000,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(s6tb,1000000000,1000,1,1,1,1)')
      CALL dmalloc0P(s6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(s6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)')
      CALL dmallocP(s7tb,1000000000,1000,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(s7tb,1000000000,1000,1,1,1,1,1)')
      CALL dmalloc0P(s7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(s7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)')
!
!Test double reals
!       WRITE(*,*) '  Skipping: CALL dmallocP(d1tb,2147483647)'
!        CALL dmallocP(d1tb,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocP(d1tb,2147483647)')
!          CALL demallocP(d1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocP(d1tb,2147483647)'
!        ENDIF
!       WRITE(*,*) '  Skipping: CALL dmalloc0P(d1tb,-2147483648,2147483647)'
!        CALL dmalloc0P(d1tb,-2147483648,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0P(d1tb,-2147483648,2147483647)')
!          CALL demallocP(d1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmalloc0P(d1tb,-2147483648,2147483647)'
!        ENDIF
      CALL dmallocP(d2tb,1000000000,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(d2tb,1000000000,1000)')
      CALL dmalloc0P(d2tb,1,1000000000,1,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(d2tb,1,1000000000,1,1000)')
      CALL dmallocP(d3tb,1000000000,1000,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(d3tb,1000000000,1000,1)')
      CALL dmalloc0P(d3tb,1,1000000000,1,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(d3tb,1,1000000000,1,1000,1,1)')
      CALL dmallocP(d4tb,1000000000,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(d4tb,1000000000,1000,1,1)')
      CALL dmalloc0P(d4tb,1,1000000000,1,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(d4tb,1,1000000000,1,1000,1,1,1,1)')
      CALL dmallocP(d5tb,1000000000,1000,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(d5tb,1000000000,1000,1,1,1)')
      CALL dmalloc0P(d5tb,1,1000000000,1,1000,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(d5tb,1,1000000000,1,1000,1,1,1,1,1,1)')
      CALL dmallocP(d6tb,1000000000,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(d6tb,1000000000,1000,1,1,1,1)')
      CALL dmalloc0P(d6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(d6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)')
      CALL dmallocP(d7tb,1000000000,1000,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocP(d7tb,1000000000,1000,1,1,1,1,1)')
      CALL dmalloc0P(d7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0P(d7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)')
      
    ENDSUBROUTINE testTOOBIGP
!
!===============================================================================
! Test error checking for over allocation
    SUBROUTINE testTOOBIGA()
      
!        INTEGER(SNK),ALLOCATABLE :: i1tb(:)
      INTEGER(SNK),ALLOCATABLE :: i2tb(:,:)
      INTEGER(SNK),ALLOCATABLE :: i3tb(:,:,:)
      INTEGER(SNK),ALLOCATABLE :: i4tb(:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: i5tb(:,:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: i6tb(:,:,:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: i7tb(:,:,:,:,:,:,:)
!        INTEGER(SLK),ALLOCATABLE :: l1tb(:)
      INTEGER(SLK),ALLOCATABLE :: l2tb(:,:)
      INTEGER(SLK),ALLOCATABLE :: l3tb(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: l4tb(:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: l5tb(:,:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: l6tb(:,:,:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: l7tb(:,:,:,:,:,:,:)
!        LOGICAL(SBK),ALLOCATABLE :: b1tb(:)
      LOGICAL(SBK),ALLOCATABLE :: b2tb(:,:)
      LOGICAL(SBK),ALLOCATABLE :: b3tb(:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: b4tb(:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: b5tb(:,:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: b6tb(:,:,:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: b7tb(:,:,:,:,:,:,:)
!        REAL(SSK),ALLOCATABLE :: s1tb(:)
      REAL(SSK),ALLOCATABLE :: s2tb(:,:)
      REAL(SSK),ALLOCATABLE :: s3tb(:,:,:)
      REAL(SSK),ALLOCATABLE :: s4tb(:,:,:,:)
      REAL(SSK),ALLOCATABLE :: s5tb(:,:,:,:,:)
      REAL(SSK),ALLOCATABLE :: s6tb(:,:,:,:,:,:)
      REAL(SSK),ALLOCATABLE :: s7tb(:,:,:,:,:,:,:)
!        REAL(SDK),ALLOCATABLE :: d1tb(:)
      REAL(SDK),ALLOCATABLE :: d2tb(:,:)
      REAL(SDK),ALLOCATABLE :: d3tb(:,:,:)
      REAL(SDK),ALLOCATABLE :: d4tb(:,:,:,:)
      REAL(SDK),ALLOCATABLE :: d5tb(:,:,:,:,:)
      REAL(SDK),ALLOCATABLE :: d6tb(:,:,:,:,:,:)
      REAL(SDK),ALLOCATABLE :: d7tb(:,:,:,:,:,:,:)
      
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'SUM(eAllocs%getCounterAll())')
!          STOP 666
!        ELSE
!          WRITE(*,*) '  Passed: SUM(eAllocs%getCounterAll()) = ',SUM(eAllocs%getCounterAll())
!        ENDIF
!
!        test=LEN_TRIM(eAllocs%getLastMessage()) /= 0) THEN
!          WRITE(*,*) 'eAllocs%getLastMessage()')
!          STOP 666
!        ELSE
!          WRITE(*,*) '  Passed: eAllocs%getLastMessage() = '//TRIM(eAllocs%getLastMessage())
!        ENDIF
!
!Test logicals (booleans)
!       WRITE(*,*) '  Skipping: CALL dmallocA(b1tb,2147483647)'
!        CALL dmallocA(b1tb,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocA(b1tb,2147483647)')
!          CALL demallocP(b1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocA(b1tb,2147483647)'
!        ENDIF
!       WRITE(*,*) '  Skipping: CALL dmalloc0A(b1tb,-2147483648,2147483647)'
!        CALL dmalloc0A(b1tb,-2147483648,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0A(b1tb,-2147483648,2147483647)'
!          CALL demallocP(b1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmalloc0A(b1tb,-2147483648,2147483647)'
!        ENDIF
      CALL dmallocA(b2tb,1000000000,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(b2tb,1000000000,1000)')
      CALL dmalloc0A(b2tb,1,1000000000,1,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(b2tb,1,1000000000,1,1000)')
      CALL dmallocA(b3tb,1000000000,1000,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(b3tb,1000000000,1000,1)')
      CALL dmalloc0A(b3tb,1,1000000000,1,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(b3tb,1,1000000000,1,1000,1,1)')
      CALL dmallocA(b4tb,1000000000,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(b4tb,1000000000,1000,1,1)')
      CALL dmalloc0A(b4tb,1,1000000000,1,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(b4tb,1,1000000000,1,1000,1,1,1,1)')
      
      CALL dmallocA(b5tb,1000000000,1000,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(b5tb,1000000000,1000,1,1,1)')
      CALL dmalloc0A(b5tb,1,1000000000,1,1000,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(b5tb,1,1000000000,1,1000,1,1,1,1,1,1)')
      CALL dmallocA(b6tb,1000000000,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(b6tb,1000000000,1000,1,1,1,1)')
      CALL dmalloc0A(b6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(b6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)')
      CALL dmallocA(b7tb,1000000000,1000,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(b7tb,1000000000,1000,1,1,1,1,1)')
      CALL dmalloc0A(b7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(b7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)')
!
!Test regular integers
!       WRITE(*,*) '  Skipping: CALL dmallocA(i1tb,2147483647)'
!        CALL dmallocA(i1tb,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocA(i1tb,2147483647)')
!          CALL demallocP(i1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocA(i1tb,2147483647)'
!        ENDIF
!       WRITE(*,*) '  Skipping: CALL dmalloc0A(i1tb,-2147483648,2147483647)'
!        CALL dmalloc0A(i1tb,-2147483648,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0A(i1tb,-2147483648,2147483647)'
!          CALL demallocP(i1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmalloc0A(i1tb,-2147483648,2147483647)'
!        ENDIF
      CALL dmallocA(i2tb,1000000000,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(i2tb,1000000000,1000)')
      CALL dmalloc0A(i2tb,1,1000000000,1,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(i2tb,1,1000000000,1,1000)')
      CALL dmallocA(i3tb,1000000000,1000,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(i3tb,1000000000,1000,1)')
      CALL dmalloc0A(i3tb,1,1000000000,1,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(i3tb,1,1000000000,1,1000,1,1)')
      CALL dmallocA(i4tb,1000000000,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(i4tb,1000000000,1000,1,1)')
      CALL dmalloc0A(i4tb,1,1000000000,1,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(i4tb,1,1000000000,1,1000,1,1,1,1)')
      CALL dmallocA(i5tb,1000000000,1000,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(i5tb,1000000000,1000,1,1,1)')
      CALL dmalloc0A(i5tb,1,1000000000,1,1000,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(i5tb,1,1000000000,1,1000,1,1,1,1,1,1)')
      CALL dmallocA(i6tb,1000000000,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(i6tb,1000000000,1000,1,1,1,1)')
      CALL dmalloc0A(i6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(i6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)')
      CALL dmallocA(i7tb,1000000000,1000,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(i7tb,1000000000,1000,1,1,1,1,1)')
      CALL dmalloc0A(i7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(i7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)')
!
!Test long ints
!       WRITE(*,*) '  Skipping: CALL dmallocA(l1tb,2147483647)'
!        CALL dmallocA(l1tb,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocA(l1tb,2147483647)')
!          CALL demallocP(l1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocA(l1tb,2147483647)'
!        ENDIF
!       WRITE(*,*) '  Skipping: CALL dmalloc0A(l1tb,-2147483648,2147483647)'
!        CALL dmalloc0A(l1tb,-2147483648,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0A(l1tb,-2147483648,2147483647)'
!          CALL demallocP(l1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmalloc0A(l1tb,-2147483648,2147483647)'
!        ENDIF
      CALL dmallocA(l2tb,1000000000,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(l2tb,1000000000,1000)')
      CALL dmalloc0A(l2tb,1,1000000000,1,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(l2tb,1,1000000000,1,1000)')
      CALL dmallocA(l3tb,1000000000,1000,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(l3tb,1000000000,1000,1)')
      CALL dmalloc0A(l3tb,1,1000000000,1,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(l3tb,1,1000000000,1,1000,1,1)')
      CALL dmallocA(l4tb,1000000000,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(l4tb,1000000000,1000,1,1)')
      CALL dmalloc0A(l4tb,1,1000000000,1,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(l4tb,1,1000000000,1,1000,1,1,1,1)')
      CALL dmallocA(l5tb,1000000000,1000,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(l5tb,1000000000,1000,1,1,1)')
      CALL dmalloc0A(l5tb,1,1000000000,1,1000,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(l5tb,1,1000000000,1,1000,1,1,1,1,1,1)')
      CALL dmallocA(l6tb,1000000000,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(l6tb,1000000000,1000,1,1,1,1)')
      CALL dmalloc0A(l6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(l6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)')
      CALL dmallocA(l7tb,1000000000,1000,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(l7tb,1000000000,1000,1,1,1,1,1)')
      CALL dmalloc0A(l7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(l7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)')
!
!Test single reals
!       WRITE(*,*) '  Skipping: CALL dmallocA(s1tb,2147483647)'
!        CALL dmallocA(s1tb,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocA(s1tb,2147483647)')
!          CALL demallocP(s1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocA(s1tb,2147483647)'
!        ENDIF
!       WRITE(*,*) '  Skipping: CALL dmalloc0A(s1tb,-2147483648,2147483647)'
!        CALL dmalloc0A(s1tb,-2147483648,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0A(s1tb,-2147483648,2147483647)')
!          CALL demallocP(s1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmalloc0A(s1tb,-2147483648,2147483647)'
!        ENDIF
      CALL dmallocA(s2tb,1000000000,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(s2tb,1000000000,1000)')
      CALL dmalloc0A(s2tb,1,1000000000,1,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(s2tb,1,1000000000,1,1000)')
      CALL dmallocA(s3tb,1000000000,1000,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(s3tb,1000000000,1000,1)')
      CALL dmalloc0A(s3tb,1,1000000000,1,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(s3tb,1,1000000000,1,1000,1,1)')
      CALL dmallocA(s4tb,1000000000,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(s4tb,1000000000,1000,1,1)')
      CALL dmalloc0A(s4tb,1,1000000000,1,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(s4tb,1,1000000000,1,1000,1,1,1,1)')
      CALL dmallocA(s5tb,1000000000,1000,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(s5tb,1000000000,1000,1,1,1)')
      CALL dmalloc0A(s5tb,1,1000000000,1,1000,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(s5tb,1,1000000000,1,1000,1,1,1,1,1,1)')
      CALL dmallocA(s6tb,1000000000,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(s6tb,1000000000,1000,1,1,1,1)')
      CALL dmalloc0A(s6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(s6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)')
      CALL dmallocA(s7tb,1000000000,1000,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(s7tb,1000000000,1000,1,1,1,1,1)')
      CALL dmalloc0A(s7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(s7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)')
!
!Test double reals
!       WRITE(*,*) '  Skipping: CALL dmallocA(d1tb,2147483647)'
!        CALL dmallocA(d1tb,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocA(d1tb,2147483647)')
!          CALL demallocP(d1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocA(d1tb,2147483647)'
!        ENDIF
!       WRITE(*,*) '  Skipping: CALL dmalloc0A(d1tb,-2147483648,2147483647)'
!        CALL dmalloc0A(d1tb,-2147483648,2147483647)
!        test=SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0A(d1tb,-2147483648,2147483647)')
!          CALL demallocP(d1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmalloc0A(d1tb,-2147483648,2147483647)'
!        ENDIF
      CALL dmallocA(d2tb,1000000000,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(d2tb,1000000000,1000)')
      CALL dmalloc0A(d2tb,1,1000000000,1,1000)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(d2tb,1,1000000000,1,1000)')
      CALL dmallocA(d3tb,1000000000,1000,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(d3tb,1000000000,1000,1)')
      CALL dmalloc0A(d3tb,1,1000000000,1,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(d3tb,1,1000000000,1,1000,1,1)')
      CALL dmallocA(d4tb,1000000000,1000,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(d4tb,1000000000,1000,1,1)')
      CALL dmalloc0A(d4tb,1,1000000000,1,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(d4tb,1,1000000000,1,1000,1,1,1,1)')
      CALL dmallocA(d5tb,1000000000,1000,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(d5tb,1000000000,1000,1,1,1)')
      CALL dmalloc0A(d5tb,1,1000000000,1,1000,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(d5tb,1,1000000000,1,1000,1,1,1,1,1,1)')
      CALL dmallocA(d6tb,1000000000,1000,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(d6tb,1000000000,1000,1,1,1,1)')
      CALL dmalloc0A(d6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(d6tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1)')
      CALL dmallocA(d7tb,1000000000,1000,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmallocA(d7tb,1000000000,1000,1,1,1,1,1)')
      CALL dmalloc0A(d7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)
      ASSERT(SUM(eAllocs%getCounterAll()) /= 0,'dmalloc0A(d7tb,1,1000000000,1,1000,1,1,1,1,1,1,1,1,1,1)')

    ENDSUBROUTINE testTOOBIGA
!    
ENDPROGRAM testAllocsOOM
