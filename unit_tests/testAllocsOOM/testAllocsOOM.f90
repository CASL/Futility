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
  USE ISO_C_BINDING
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE Allocs

  IMPLICIT NONE

  INCLUDE 'getSysProcInfo_F.h'
  
  INTEGER(SIK) :: nerror0,nerror1,n1,n2,i
  INTEGER(C_LONG_LONG),SAVE :: maxRam,maxSwap,memAvail,maxMem,memForTest
  
  CREATE_TEST('AllocsOOM')
  
  CALL eAllocs%setStopOnError(.FALSE.)
  CALL eAllocs%setQuietMode(.TRUE.)
  maxMem=0
  CALL getSysMemInfo(maxRam,maxSwap,memAvail)
  maxMem=maxRam+maxSwap
  n1=1000000000
  n2=1000
  DO i=1,6
    memForTest=INT(SNK,C_LONG_LONG)*INT(n1,C_LONG_LONG)*INT(n2,C_LONG_LONG)
    IF(memForTest > maxMem) EXIT
    n2=n2*10
  ENDDO
  IF(memForTest > maxMem) THEN
    REGISTER_SUBTEST('testTOOBIGP()',testTOOBIGP)
    REGISTER_SUBTEST('testTOOBIGA()',testTOOBIGA)
  ELSE
    ASSERT(.FALSE.,'SKIPPING TEST! MACHINE HAS MORE MEMORY THAN TEST CAN ALLOCATE!')
    FINFO() 'Machine has "',maxMem,'" bytes of memory,'
    FINFO() 'which is larger than what test will allocate (',memForTest,' bytes).'
  ENDIF
  
  FINALIZE_TEST()
!
!===============================================================================  
  CONTAINS
!
!-------------------------------------------------------------------------------
! Test error checking for over allocation
    SUBROUTINE testTOOBIGP()
      INTEGER(SNK),POINTER :: i1tb(:)
      INTEGER(SNK),POINTER :: i2tb(:,:)
      INTEGER(SNK),POINTER :: i3tb(:,:,:)
      INTEGER(SNK),POINTER :: i4tb(:,:,:,:)
      INTEGER(SNK),POINTER :: i5tb(:,:,:,:,:)
      INTEGER(SNK),POINTER :: i6tb(:,:,:,:,:,:)
      INTEGER(SNK),POINTER :: i7tb(:,:,:,:,:,:,:)
      INTEGER(SLK),POINTER :: l1tb(:)
      INTEGER(SLK),POINTER :: l2tb(:,:)
      INTEGER(SLK),POINTER :: l3tb(:,:,:)
      INTEGER(SLK),POINTER :: l4tb(:,:,:,:)
      INTEGER(SLK),POINTER :: l5tb(:,:,:,:,:)
      INTEGER(SLK),POINTER :: l6tb(:,:,:,:,:,:)
      INTEGER(SLK),POINTER :: l7tb(:,:,:,:,:,:,:)
      LOGICAL(SBK),POINTER :: b1tb(:)
      LOGICAL(SBK),POINTER :: b2tb(:,:)
      LOGICAL(SBK),POINTER :: b3tb(:,:,:)
      LOGICAL(SBK),POINTER :: b4tb(:,:,:,:)
      LOGICAL(SBK),POINTER :: b5tb(:,:,:,:,:)
      LOGICAL(SBK),POINTER :: b6tb(:,:,:,:,:,:)
      LOGICAL(SBK),POINTER :: b7tb(:,:,:,:,:,:,:)
      REAL(SSK),POINTER :: s1tb(:)
      REAL(SSK),POINTER :: s2tb(:,:)
      REAL(SSK),POINTER :: s3tb(:,:,:)
      REAL(SSK),POINTER :: s4tb(:,:,:,:)
      REAL(SSK),POINTER :: s5tb(:,:,:,:,:)
      REAL(SSK),POINTER :: s6tb(:,:,:,:,:,:)
      REAL(SSK),POINTER :: s7tb(:,:,:,:,:,:,:)
      REAL(SDK),POINTER :: d1tb(:)
      REAL(SDK),POINTER :: d2tb(:,:)
      REAL(SDK),POINTER :: d3tb(:,:,:)
      REAL(SDK),POINTER :: d4tb(:,:,:,:)
      REAL(SDK),POINTER :: d5tb(:,:,:,:,:)
      REAL(SDK),POINTER :: d6tb(:,:,:,:,:,:)
      REAL(SDK),POINTER :: d7tb(:,:,:,:,:,:,:)
      
      NULLIFY(i1tb,i2tb,i3tb,i4tb,i5tb,i6tb,i7tb)
      NULLIFY(l1tb,l2tb,l3tb,l4tb,l5tb,l6tb,l7tb)
      NULLIFY(b1tb,b2tb,b3tb,b4tb,b5tb,b6tb,b7tb)
      NULLIFY(s1tb,s2tb,s3tb,s4tb,s5tb,s6tb,s7tb)
      NULLIFY(d1tb,d2tb,d3tb,d4tb,d5tb,d6tb,d7tb)
      
      nerror0=eAllocs%getCounter(EXCEPTION_ERROR)
      
      CALL dmallocP(b2tb,n1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(b2tb,n1,n2)')
      nerror0=nerror1
      CALL dmalloc0P(b2tb,1,n1,1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(b2tb,1,n1,1,n2)')
      nerror0=nerror1
      CALL dmallocP(b3tb,n1,n2,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(b3tb,n1,n2,1)')
      nerror0=nerror1
      CALL dmalloc0P(b3tb,1,n1,1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(b3tb,1,n1,1,n2,1,1)')
      nerror0=nerror1
      CALL dmallocP(b4tb,n1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(b4tb,n1,n2,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(b4tb,1,n1,1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(b4tb,1,n1,1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(b5tb,n1,n2,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(b5tb,n1,n2,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(b5tb,1,n1,1,n2,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(b5tb,1,n1,1,n2,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(b6tb,n1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(b6tb,n1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(b6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(b6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(b7tb,n1,n2,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(b7tb,n1,n2,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(b7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(b7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      
      CALL dmallocP(i2tb,n1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(i2tb,n1,n2)')
      nerror0=nerror1
      CALL dmalloc0P(i2tb,1,n1,1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(i2tb,1,n1,1,n2)')
      nerror0=nerror1
      CALL dmallocP(i3tb,n1,n2,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(i3tb,n1,n2,1)')
      nerror0=nerror1
      CALL dmalloc0P(i3tb,1,n1,1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(i3tb,1,n1,1,n2,1,1)')
      nerror0=nerror1
      CALL dmallocP(i4tb,n1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(i4tb,n1,n2,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(i4tb,1,n1,1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(i4tb,1,n1,1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(i5tb,n1,n2,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(i5tb,n1,n2,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(i5tb,1,n1,1,n2,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(i5tb,1,n1,1,n2,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(i6tb,n1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(i6tb,n1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(i6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(i6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(i7tb,n1,n2,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(i7tb,n1,n2,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(i7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(i7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      
      CALL dmallocP(l2tb,n1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(l2tb,n1,n2)')
      nerror0=nerror1
      CALL dmalloc0P(l2tb,1,n1,1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(l2tb,1,n1,1,n2)')
      nerror0=nerror1
      CALL dmallocP(l3tb,n1,n2,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(l3tb,n1,n2,1)')
      nerror0=nerror1
      CALL dmalloc0P(l3tb,1,n1,1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(l3tb,1,n1,1,n2,1,1)')
      nerror0=nerror1
      CALL dmallocP(l4tb,n1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(l4tb,n1,n2,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(l4tb,1,n1,1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(l4tb,1,n1,1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(l5tb,n1,n2,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(l5tb,n1,n2,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(l5tb,1,n1,1,n2,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(l5tb,1,n1,1,n2,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(l6tb,n1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(l6tb,n1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(l6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(l6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(l7tb,n1,n2,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(l7tb,n1,n2,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(l7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(l7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      
      CALL dmallocP(s2tb,n1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(s2tb,n1,n2)')
      nerror0=nerror1
      CALL dmalloc0P(s2tb,1,n1,1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(s2tb,1,n1,1,n2)')
      nerror0=nerror1
      CALL dmallocP(s3tb,n1,n2,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(s3tb,n1,n2,1)')
      nerror0=nerror1
      CALL dmalloc0P(s3tb,1,n1,1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(s3tb,1,n1,1,n2,1,1)')
      nerror0=nerror1
      CALL dmallocP(s4tb,n1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(s4tb,n1,n2,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(s4tb,1,n1,1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(s4tb,1,n1,1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(s5tb,n1,n2,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(s5tb,n1,n2,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(s5tb,1,n1,1,n2,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(s5tb,1,n1,1,n2,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(s6tb,n1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(s6tb,n1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(s6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(s6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(s7tb,n1,n2,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(s7tb,n1,n2,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(s7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(s7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      
      CALL dmallocP(d2tb,n1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(d2tb,n1,n2)')
      nerror0=nerror1
      CALL dmalloc0P(d2tb,1,n1,1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(d2tb,1,n1,1,n2)')
      nerror0=nerror1
      CALL dmallocP(d3tb,n1,n2,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(d3tb,n1,n2,1)')
      nerror0=nerror1
      CALL dmalloc0P(d3tb,1,n1,1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(d3tb,1,n1,1,n2,1,1)')
      nerror0=nerror1
      CALL dmallocP(d4tb,n1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(d4tb,n1,n2,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(d4tb,1,n1,1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(d4tb,1,n1,1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(d5tb,n1,n2,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(d5tb,n1,n2,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(d5tb,1,n1,1,n2,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(d5tb,1,n1,1,n2,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(d6tb,n1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(d6tb,n1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(d6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(d6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocP(d7tb,n1,n2,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocP(d7tb,n1,n2,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0P(d7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0P(d7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
    ENDSUBROUTINE testTOOBIGP
!
!===============================================================================
! Test error checking for over allocation
    SUBROUTINE testTOOBIGA()
      
      INTEGER(SNK),ALLOCATABLE :: i1tb(:)
      INTEGER(SNK),ALLOCATABLE :: i2tb(:,:)
      INTEGER(SNK),ALLOCATABLE :: i3tb(:,:,:)
      INTEGER(SNK),ALLOCATABLE :: i4tb(:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: i5tb(:,:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: i6tb(:,:,:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: i7tb(:,:,:,:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: l1tb(:)
      INTEGER(SLK),ALLOCATABLE :: l2tb(:,:)
      INTEGER(SLK),ALLOCATABLE :: l3tb(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: l4tb(:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: l5tb(:,:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: l6tb(:,:,:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: l7tb(:,:,:,:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: b1tb(:)
      LOGICAL(SBK),ALLOCATABLE :: b2tb(:,:)
      LOGICAL(SBK),ALLOCATABLE :: b3tb(:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: b4tb(:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: b5tb(:,:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: b6tb(:,:,:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: b7tb(:,:,:,:,:,:,:)
      REAL(SSK),ALLOCATABLE :: s1tb(:)
      REAL(SSK),ALLOCATABLE :: s2tb(:,:)
      REAL(SSK),ALLOCATABLE :: s3tb(:,:,:)
      REAL(SSK),ALLOCATABLE :: s4tb(:,:,:,:)
      REAL(SSK),ALLOCATABLE :: s5tb(:,:,:,:,:)
      REAL(SSK),ALLOCATABLE :: s6tb(:,:,:,:,:,:)
      REAL(SSK),ALLOCATABLE :: s7tb(:,:,:,:,:,:,:)
      REAL(SDK),ALLOCATABLE :: d1tb(:)
      REAL(SDK),ALLOCATABLE :: d2tb(:,:)
      REAL(SDK),ALLOCATABLE :: d3tb(:,:,:)
      REAL(SDK),ALLOCATABLE :: d4tb(:,:,:,:)
      REAL(SDK),ALLOCATABLE :: d5tb(:,:,:,:,:)
      REAL(SDK),ALLOCATABLE :: d6tb(:,:,:,:,:,:)
      REAL(SDK),ALLOCATABLE :: d7tb(:,:,:,:,:,:,:)      
      
      CALL dmallocA(b2tb,n1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(b2tb,n1,n2)')
      nerror0=nerror1
      CALL dmalloc0A(b2tb,1,n1,1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(b2tb,1,n1,1,n2)')
      nerror0=nerror1
      CALL dmallocA(b3tb,n1,n2,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(b3tb,n1,n2,1)')
      nerror0=nerror1
      CALL dmalloc0A(b3tb,1,n1,1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(b3tb,1,n1,1,n2,1,1)')
      nerror0=nerror1
      CALL dmallocA(b4tb,n1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(b4tb,n1,n2,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(b4tb,1,n1,1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(b4tb,1,n1,1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(b5tb,n1,n2,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(b5tb,n1,n2,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(b5tb,1,n1,1,n2,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(b5tb,1,n1,1,n2,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(b6tb,n1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(b6tb,n1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(b6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(b6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(b7tb,n1,n2,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(b7tb,n1,n2,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(b7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(b7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      
      CALL dmallocA(i2tb,n1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(i2tb,n1,n2)')
      nerror0=nerror1
      CALL dmalloc0A(i2tb,1,n1,1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(i2tb,1,n1,1,n2)')
      nerror0=nerror1
      CALL dmallocA(i3tb,n1,n2,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(i3tb,n1,n2,1)')
      nerror0=nerror1
      CALL dmalloc0A(i3tb,1,n1,1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(i3tb,1,n1,1,n2,1,1)')
      nerror0=nerror1
      CALL dmallocA(i4tb,n1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(i4tb,n1,n2,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(i4tb,1,n1,1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(i4tb,1,n1,1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(i5tb,n1,n2,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(i5tb,n1,n2,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(i5tb,1,n1,1,n2,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(i5tb,1,n1,1,n2,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(i6tb,n1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(i6tb,n1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(i6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(i6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(i7tb,n1,n2,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(i7tb,n1,n2,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(i7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(i7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      
      CALL dmallocA(l2tb,n1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(l2tb,n1,n2)')
      nerror0=nerror1
      CALL dmalloc0A(l2tb,1,n1,1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(l2tb,1,n1,1,n2)')
      nerror0=nerror1
      CALL dmallocA(l3tb,n1,n2,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(l3tb,n1,n2,1)')
      nerror0=nerror1
      CALL dmalloc0A(l3tb,1,n1,1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(l3tb,1,n1,1,n2,1,1)')
      nerror0=nerror1
      CALL dmallocA(l4tb,n1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(l4tb,n1,n2,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(l4tb,1,n1,1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(l4tb,1,n1,1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(l5tb,n1,n2,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(l5tb,n1,n2,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(l5tb,1,n1,1,n2,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(l5tb,1,n1,1,n2,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(l6tb,n1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(l6tb,n1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(l6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(l6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(l7tb,n1,n2,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(l7tb,n1,n2,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(l7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(l7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      
      CALL dmallocA(s2tb,n1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(s2tb,n1,n2)')
      nerror0=nerror1
      CALL dmalloc0A(s2tb,1,n1,1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(s2tb,1,n1,1,n2)')
      nerror0=nerror1
      CALL dmallocA(s3tb,n1,n2,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(s3tb,n1,n2,1)')
      nerror0=nerror1
      CALL dmalloc0A(s3tb,1,n1,1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(s3tb,1,n1,1,n2,1,1)')
      nerror0=nerror1
      CALL dmallocA(s4tb,n1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(s4tb,n1,n2,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(s4tb,1,n1,1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(s4tb,1,n1,1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(s5tb,n1,n2,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(s5tb,n1,n2,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(s5tb,1,n1,1,n2,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(s5tb,1,n1,1,n2,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(s6tb,n1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(i7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(s6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(s6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(s7tb,n1,n2,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(s7tb,n1,n2,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(s7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(s7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      
      CALL dmallocA(d2tb,n1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(d2tb,n1,n2)')
      nerror0=nerror1
      CALL dmalloc0A(d2tb,1,n1,1,n2)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(d2tb,1,n1,1,n2)')
      nerror0=nerror1
      CALL dmallocA(d3tb,n1,n2,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(d3tb,n1,n2,1)')
      nerror0=nerror1
      CALL dmalloc0A(d3tb,1,n1,1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(d3tb,1,n1,1,n2,1,1)')
      nerror0=nerror1
      CALL dmallocA(d4tb,n1,n2,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(d4tb,n1,n2,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(d4tb,1,n1,1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(d4tb,1,n1,1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(d5tb,n1,n2,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(d5tb,n1,n2,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(d5tb,1,n1,1,n2,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(d5tb,1,n1,1,n2,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(d6tb,n1,n2,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(d6tb,n1,n2,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(d6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(d6tb,1,n1,1,n2,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmallocA(d7tb,n1,n2,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmallocA(d7tb,n1,n2,1,1,1,1,1)')
      nerror0=nerror1
      CALL dmalloc0A(d7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)
      nerror1=eAllocs%getCounter(EXCEPTION_ERROR)
      ASSERT(nerror0+1 == nerror1,'dmalloc0A(d7tb,1,n1,1,n2,1,1,1,1,1,1,1,1,1,1)')
      nerror0=nerror1
    ENDSUBROUTINE testTOOBIGA
!    
ENDPROGRAM testAllocsOOM
