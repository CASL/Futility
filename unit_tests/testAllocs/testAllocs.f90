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
#include "UnitTest.h"

PROGRAM testAllocs
      
  USE IntrType
  USE Allocs
  USE UnitTest
  IMPLICIT NONE
  
  CREATE_TEST("Allocs")
  
  REGISTER_SUBTEST("testGetMemUsage()",testGetMemUsage)
  
  CALL eAllocs%setStopOnError(.FALSE.)
  CALL eAllocs%setQuietMode(.TRUE.)
  REGISTER_SUBTEST("testTOOBIGP()",testTOOBIGP)
  REGISTER_SUBTEST("testTOOBIGA()",testTOOBIGA)
  CALL eAllocs%setStopOnError(.TRUE.)
  
  REGISTER_SUBTEST("testBOOLP()",testBOOLP)
  REGISTER_SUBTEST("testBOOLA()",testBOOLA)
  REGISTER_SUBTEST("testINTP()",testINTP)
  REGISTER_SUBTEST("testINTA()",testINTA)
  REGISTER_SUBTEST("testLONGINTP()",testLONGINTP)
  REGISTER_SUBTEST("testLONGINTA()",testLONGINTA)
  REGISTER_SUBTEST("testSINGLEP()",testSINGLEP)
  REGISTER_SUBTEST("testSINGLEA()",testSINGLEA)
  REGISTER_SUBTEST("testDOUBLEP()",testDOUBLEP)
  REGISTER_SUBTEST("testDOUBLEA()",testDOUBLEa)
  
  !    CALL testBOOLA()
  !    CALL testINTP()
  !    CALL testINTA()
  !    CALL testLONGINTP()
  !    CALL testLONGINTA()
  !    CALL testSINGLEP()
  !    CALL testSINGLEA()
  !    CALL testDOUBLEP()
  !    CALL testDOUBLEA()
  
  FINALIZE_TEST()
  
  CONTAINS
    SUBROUTINE testGetMemUsage()
      
      ASSERT(ALLOC_MEMSTRING_LENGTH == 14_SIK,'ALLOC_MEMSTRING_LENGTH')
      ASSERT(getMemUsageChar(563246226243._SRK) == '  524.56 GB   ','getMemUsageChar(563246226243._SRK)')  
      ASSERT(getMemUsageChar() == '    0.00 bytes','getMemUsageChar()')
  
    ENDSUBROUTINE testGetMemUsage
!
!===============================================================================
! Test allocation/deallocation for booleans
    SUBROUTINE testBOOLP
      USE IntrType
      USE Allocs
      IMPLICIT NONE
    
      LOGICAL(SBK) :: test
      LOGICAL(SBK),POINTER :: bool1(:)
      LOGICAL(SBK),POINTER :: bool2(:,:)
      LOGICAL(SBK),POINTER :: bool3(:,:,:)
      LOGICAL(SBK),POINTER :: bool4(:,:,:,:)
      LOGICAL(SBK),POINTER :: bool5(:,:,:,:,:)
      LOGICAL(SBK),POINTER :: bool6(:,:,:,:,:,:)
      LOGICAL(SBK),POINTER :: bool7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0

      NULLIFY(bool1,bool2,bool3,bool4,bool5,bool6,bool7)
  !
  ! rank 1 variable
      NULLIFY(bool1)
      CALL dmallocP(bool1,-10)
      ASSERT(.NOT.ASSOCIATED(bool1),'dmallocP(bool1,-10)')
      CALL dmallocP(bool1,10)
      test=(ASSOCIATED(bool1)) .AND. .NOT.ANY(bool1) .AND. &
            (UBOUND(bool1,1) == 10) .AND. (LBOUND(bool1,1)==1)
      ASSERT(test,'dmallocP(bool1,10)')
      
      nbytes0=Alloc_nbytes
      CALL dmallocP(bool1,100)
      test=ASSOCIATED(bool1) .AND. .NOT.ANY(bool1) .AND. Alloc_nbytes == nbytes0 &
            .AND. UBOUND(bool1,1) == 10 .AND. LBOUND(bool1,1) == 1
      ASSERT(test,'dmallocP(bool1,100)')
      
      CALL demallocP(bool1)
      ASSERT(.NOT.ASSOCIATED(bool1) .AND. Alloc_nbytes == 0.0_SRK,'demallocP(bool1)')
      
      CALL demallocP(bool1)
      ASSERT(.NOT.ASSOCIATED(bool1) .AND. Alloc_nbytes == 0.0_SRK,'demallocP(bool1)')
      
      CALL dmalloc0P(bool1,8,-1)
      CALL dmalloc0P(bool1,-1,8)
      test=(ASSOCIATED(bool1)) .AND. .NOT.ANY(bool1) .AND. &
          (UBOUND(bool1,1) == 8) .AND. (LBOUND(bool1,1) == -1)
      ASSERT(test,'dmalloc0P(bool1,-1,8)')
      
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(bool1,-1,1)
      test=(ASSOCIATED(bool1)) .AND. .NOT.ANY(bool1) .AND. Alloc_nbytes == nbytes0 &
          .AND. (UBOUND(bool1,1) == 8) .AND. (LBOUND(bool1,1) == -1)
      ASSERT(test,'dmalloc0P(bool1,-1,1)')
      CALL demallocP(bool1)
  !
  ! rank 2 variable
      NULLIFY(bool2)
      CALL dmallocP(bool2,-10,10)
      CALL dmallocP(bool2,10,-10)
      CALL dmallocP(bool2,10,10)
      IF((.NOT.ASSOCIATED(bool2)) .OR. ANY(bool2) .OR. &
          (UBOUND(bool2,1) /= 10) .OR. (LBOUND(bool2,1) /= 1) .OR. &
          (UBOUND(bool2,2) /= 10) .OR. (LBOUND(bool2,2) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(bool2,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(bool2,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(bool2,100,100)
      IF((.NOT.ASSOCIATED(bool2)) .OR. ANY(bool2) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool2,1) /= 10) .OR. (LBOUND(bool2,1) /= 1) .OR. &
          (UBOUND(bool2,2) /= 10) .OR. (LBOUND(bool2,2) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(bool2,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(bool2,100,100)'
      ENDIF
      CALL demallocP(bool2)
      IF( ASSOCIATED(bool2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(bool2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(bool2)'
      ENDIF
      CALL demallocP(bool2)
      IF( ASSOCIATED(bool2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(bool2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(bool2)'
      ENDIF
      CALL dmalloc0P(bool2,8,-1,-1,8)
      CALL dmalloc0P(bool2,-1,8,8,-1)
      CALL dmalloc0P(bool2,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(bool2)) .OR. ANY(bool2) .OR. &
          (UBOUND(bool2,1) /= 8) .OR. (LBOUND(bool2,1) /= -1) .OR. &
          (UBOUND(bool2,2) /= 8) .OR. (LBOUND(bool2,2) /= -1) ) THEN
        WRITE(*,*) 'CALL CALL dmalloc0P(bool2,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(bool2,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(bool2,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(bool2)) .OR. ANY(bool2) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool2,1) /= 8) .OR. (LBOUND(bool2,1) /= -1) .OR. &
          (UBOUND(bool2,2) /= 8) .OR. (LBOUND(bool2,2) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(bool2,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(bool2,-1,1,-1,1)'
      ENDIF
      CALL demallocP(bool2)
  !
  ! rank 3 variable
      NULLIFY(bool3)
      CALL dmallocP(bool3,-10,10,10)
      CALL dmallocP(bool3,10,-10,10)
      CALL dmallocP(bool3,10,10,-10)
      CALL dmallocP(bool3,10,10,10)
      IF((.NOT.ASSOCIATED(bool3)) .OR. ANY(bool3) .OR. &
          (UBOUND(bool3,1) /= 10) .OR. (LBOUND(bool3,1) /= 1) .OR. &
          (UBOUND(bool3,2) /= 10) .OR. (LBOUND(bool3,2) /= 1) .OR. &
          (UBOUND(bool3,3) /= 10) .OR. (LBOUND(bool3,3) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(bool3,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(bool3,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(bool3,100,100,100)
      IF((.NOT.ASSOCIATED(bool3)) .OR. ANY(bool3) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool3,1) /= 10) .OR. (LBOUND(bool3,1) /= 1) .OR. &
          (UBOUND(bool3,2) /= 10) .OR. (LBOUND(bool3,2) /= 1) .OR. &
          (UBOUND(bool3,3) /= 10) .OR. (LBOUND(bool3,3) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(bool3,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(bool3,100,100,100)'
      ENDIF
      CALL demallocP(bool3)
      IF( ASSOCIATED(bool3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(bool3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(bool3)'
      ENDIF
      CALL demallocP(bool3)
      IF( ASSOCIATED(bool3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(bool3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(bool3)'
      ENDIF
      CALL dmalloc0P(bool3,8,-1,-1,8,-1,8)
      CALL dmalloc0P(bool3,-1,8,8,-1,-1,8)
      CALL dmalloc0P(bool3,-1,8,-1,8,8,-1)
      CALL dmalloc0P(bool3,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(bool3)) .OR. ANY(bool3) .OR. &
          (UBOUND(bool3,1) /= 8) .OR. (LBOUND(bool3,1) /= -1) .OR. &
          (UBOUND(bool3,2) /= 8) .OR. (LBOUND(bool3,2) /= -1) .OR. &
          (UBOUND(bool3,3) /= 8) .OR. (LBOUND(bool3,3) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(bool3,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(bool3,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(bool3,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(bool3)) .OR. ANY(bool3) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool3,1) /= 8) .OR. (LBOUND(bool3,1) /= -1) .OR. &
          (UBOUND(bool3,2) /= 8) .OR. (LBOUND(bool3,2) /= -1) .OR. &
          (UBOUND(bool3,3) /= 8) .OR. (LBOUND(bool3,3) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(bool3,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(bool3,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(bool3)
  !
  ! rank 4 variable
      NULLIFY(bool4)
      CALL dmallocP(bool4,-10,10,10,10)
      CALL dmallocP(bool4,10,-10,10,10)
      CALL dmallocP(bool4,10,10,-10,10)
      CALL dmallocP(bool4,10,10,10,-10)
      CALL dmallocP(bool4,10,10,10,10)
      IF((.NOT.ASSOCIATED(bool4)) .OR. ANY(bool4) .OR. &
          (UBOUND(bool4,1) /= 10) .OR. (LBOUND(bool4,1) /= 1) .OR. &
          (UBOUND(bool4,2) /= 10) .OR. (LBOUND(bool4,2) /= 1) .OR. &
          (UBOUND(bool4,3) /= 10) .OR. (LBOUND(bool4,3) /= 1) .OR. &
          (UBOUND(bool4,4) /= 10) .OR. (LBOUND(bool4,4) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(bool4,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(bool4,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(bool4,100,100,100,100)
      IF((.NOT.ASSOCIATED(bool4)) .OR. ANY(bool4) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool4,1) /= 10) .OR. (LBOUND(bool4,1) /= 1) .OR. &
          (UBOUND(bool4,2) /= 10) .OR. (LBOUND(bool4,2) /= 1) .OR. &
          (UBOUND(bool4,3) /= 10) .OR. (LBOUND(bool4,3) /= 1) .OR. &
          (UBOUND(bool4,4) /= 10) .OR. (LBOUND(bool4,4) /= 1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmallocP(bool4,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP((bool4,100,100,100,100)'
      ENDIF
      CALL demallocP(bool4)
      IF( ASSOCIATED(bool4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(bool4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(bool4)'
      ENDIF
      CALL demallocP(bool4)
      IF( ASSOCIATED(bool4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(bool4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(bool4)'
      ENDIF
      CALL dmalloc0P(bool4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(bool4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(bool4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(bool4,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(bool4)) .OR. ANY(bool4) .OR. &
          (UBOUND(bool4,1) /= 8) .OR. (LBOUND(bool4,1) /= -1) .OR. &
          (UBOUND(bool4,2) /= 8) .OR. (LBOUND(bool4,2) /= -1) .OR. &
          (UBOUND(bool4,3) /= 8) .OR. (LBOUND(bool4,3) /= -1) .OR. &
          (UBOUND(bool4,4) /= 8) .OR. (LBOUND(bool4,4) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(bool4,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(bool4,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(bool4,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(bool4)) .OR. ANY(bool4) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool4,1) /= 8) .OR. (LBOUND(bool4,1) /= -1) .OR. &
          (UBOUND(bool4,2) /= 8) .OR. (LBOUND(bool4,2) /= -1) .OR. &
          (UBOUND(bool4,3) /= 8) .OR. (LBOUND(bool4,3) /= -1) .OR. &
          (UBOUND(bool4,4) /= 8) .OR. (LBOUND(bool4,4) /= -1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmalloc0P(bool4,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(bool4,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(bool4)
  !
  ! rank 5 variable
      NULLIFY(bool5)
      CALL dmallocP(bool5,-10,10,10,10,10)
      CALL dmallocP(bool5,10,-10,10,10,10)
      CALL dmallocP(bool5,10,10,-10,10,10)
      CALL dmallocP(bool5,10,10,10,-10,10)
      CALL dmallocP(bool5,10,10,10,10,-10)
      CALL dmallocP(bool5,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(bool5)) .OR. ANY(bool5) .OR. &
          (UBOUND(bool5,1) /= 10) .OR. (LBOUND(bool5,1) /= 1) .OR. &
          (UBOUND(bool5,2) /= 10) .OR. (LBOUND(bool5,2) /= 1) .OR. &
          (UBOUND(bool5,3) /= 10) .OR. (LBOUND(bool5,3) /= 1) .OR. &
          (UBOUND(bool5,4) /= 10) .OR. (LBOUND(bool5,4) /= 1) .OR. &
          (UBOUND(bool5,5) /= 10) .OR. (LBOUND(bool5,5) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(bool5,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(bool5,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(bool5,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(bool5)) .OR. ANY(bool5)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool5,1) /= 10) .OR. (LBOUND(bool5,1) /= 1) .OR. &
          (UBOUND(bool5,2) /= 10) .OR. (LBOUND(bool5,2) /= 1) .OR. &
          (UBOUND(bool5,3) /= 10) .OR. (LBOUND(bool5,3) /= 1) .OR. &
          (UBOUND(bool5,4) /= 10) .OR. (LBOUND(bool5,4) /= 1) .OR. &
          (UBOUND(bool5,5) /= 10) .OR. (LBOUND(bool5,5) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(bool5,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(bool5,100,100,100,100,100)'
      ENDIF
      CALL demallocP(bool5)
      IF( ASSOCIATED(bool5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(bool5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(bool5)'
      ENDIF
      CALL demallocP(bool5)
      IF( ASSOCIATED(bool5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(bool5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(bool5)'
      ENDIF
      CALL dmalloc0P(bool5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(bool5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(bool5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(bool5,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(bool5)) .OR. ANY(bool5) .OR. &
          (UBOUND(bool5,1) /= 8) .OR. (LBOUND(bool5,1) /= -1) .OR. &
          (UBOUND(bool5,2) /= 8) .OR. (LBOUND(bool5,2) /= -1) .OR. &
          (UBOUND(bool5,3) /= 8) .OR. (LBOUND(bool5,3) /= -1) .OR. &
          (UBOUND(bool5,4) /= 8) .OR. (LBOUND(bool5,4) /= -1) .OR. &
          (UBOUND(bool5,5) /= 8) .OR. (LBOUND(bool5,5) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocP(bool5,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(bool5,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(bool5,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(bool5)) .OR. ANY(bool5)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool5,1) /= 8) .OR. (LBOUND(bool5,1) /= -1) .OR. &
          (UBOUND(bool5,2) /= 8) .OR. (LBOUND(bool5,2) /= -1) .OR. &
          (UBOUND(bool5,3) /= 8) .OR. (LBOUND(bool5,3) /= -1) .OR. &
          (UBOUND(bool5,4) /= 8) .OR. (LBOUND(bool5,4) /= -1) .OR. &
          (UBOUND(bool5,5) /= 8) .OR. (LBOUND(bool5,5) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0P(bool5,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(bool5,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(bool5)
  !
  ! rank 6 variable
      CALL dmallocP(bool6,-10,10,10,10,10,10)
      CALL dmallocP(bool6,10,-10,10,10,10,10)
      CALL dmallocP(bool6,10,10,-10,10,10,10)
      CALL dmallocP(bool6,10,10,10,-10,10,10)
      CALL dmallocP(bool6,10,10,10,10,-10,10)
      CALL dmallocP(bool6,10,10,10,10,10,-10)
      CALL dmallocP(bool6,10,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(bool6)) .OR. ANY(bool6) .OR. &
          (UBOUND(bool6,1) /= 10) .OR. (LBOUND(bool6,1) /= 1) .OR. &
          (UBOUND(bool6,2) /= 10) .OR. (LBOUND(bool6,2) /= 1) .OR. &
          (UBOUND(bool6,3) /= 10) .OR. (LBOUND(bool6,3) /= 1) .OR. &
          (UBOUND(bool6,4) /= 10) .OR. (LBOUND(bool6,4) /= 1) .OR. &
          (UBOUND(bool6,5) /= 10) .OR. (LBOUND(bool6,5) /= 1) .OR. &
          (UBOUND(bool6,6) /= 10) .OR. (LBOUND(bool6,6) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(bool6,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(bool6,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(bool6,100,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(bool6)) .OR. ANY(bool6) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool6,1) /= 10) .OR. (LBOUND(bool6,1) /= 1) .OR. &
          (UBOUND(bool6,2) /= 10) .OR. (LBOUND(bool6,2) /= 1) .OR. &
          (UBOUND(bool6,3) /= 10) .OR. (LBOUND(bool6,3) /= 1) .OR. &
          (UBOUND(bool6,4) /= 10) .OR. (LBOUND(bool6,4) /= 1) .OR. &
          (UBOUND(bool6,5) /= 10) .OR. (LBOUND(bool6,5) /= 1) .OR. &
          (UBOUND(bool6,6) /= 10) .OR. (LBOUND(bool6,6) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(bool6,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(bool6,100,100,100,100,100,100)'
      ENDIF
      CALL demallocP(bool6)
      IF( ASSOCIATED(bool6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(bool6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(bool6)'
      ENDIF
      CALL demallocP(bool6)
      IF( ASSOCIATED(bool6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(bool6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(bool6)'
      ENDIF
      CALL dmalloc0P(bool6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(bool6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(bool6)) .OR. ANY(bool6) .OR. &
          (UBOUND(bool6,1) /= 8) .OR. (LBOUND(bool6,1) /= -1) .OR. &
          (UBOUND(bool6,2) /= 8) .OR. (LBOUND(bool6,2) /= -1) .OR. &
          (UBOUND(bool6,3) /= 8) .OR. (LBOUND(bool6,3) /= -1) .OR. &
          (UBOUND(bool6,4) /= 8) .OR. (LBOUND(bool6,4) /= -1) .OR. &
          (UBOUND(bool6,5) /= 8) .OR. (LBOUND(bool6,5) /= -1) .OR. &
          (UBOUND(bool6,6) /= 8) .OR. (LBOUND(bool6,6) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocP(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(bool6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(bool6)) .OR. ANY(bool6)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool6,1) /= 8) .OR. (LBOUND(bool6,1) /= -1) .OR. &
          (UBOUND(bool6,2) /= 8) .OR. (LBOUND(bool6,2) /= -1) .OR. &
          (UBOUND(bool6,3) /= 8) .OR. (LBOUND(bool6,3) /= -1) .OR. &
          (UBOUND(bool6,4) /= 8) .OR. (LBOUND(bool6,4) /= -1) .OR. &
          (UBOUND(bool6,5) /= 8) .OR. (LBOUND(bool6,5) /= -1) .OR. &
          (UBOUND(bool6,6) /= 8) .OR. (LBOUND(bool6,6) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0P(bool6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(bool6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(bool6)
  !
  ! rank 7 variable
      CALL dmallocP(bool7,-10,10,10,10,10,10,10)
      CALL dmallocP(bool7,10,-10,10,10,10,10,10)
      CALL dmallocP(bool7,10,10,-10,10,10,10,10)
      CALL dmallocP(bool7,10,10,10,-10,10,10,10)
      CALL dmallocP(bool7,10,10,10,10,-10,10,10)
      CALL dmallocP(bool7,10,10,10,10,10,-10,10)
      CALL dmallocP(bool7,10,10,10,10,10,10,-10)
      CALL dmallocP(bool7,10,10,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(bool7)) .OR. ANY(bool7) .OR. &
          (UBOUND(bool7,1) /= 10) .OR. (LBOUND(bool7,1) /= 1) .OR. &
          (UBOUND(bool7,2) /= 10) .OR. (LBOUND(bool7,2) /= 1) .OR. &
          (UBOUND(bool7,3) /= 10) .OR. (LBOUND(bool7,3) /= 1) .OR. &
          (UBOUND(bool7,4) /= 10) .OR. (LBOUND(bool7,4) /= 1) .OR. &
          (UBOUND(bool7,5) /= 10) .OR. (LBOUND(bool7,5) /= 1) .OR. &
          (UBOUND(bool7,6) /= 10) .OR. (LBOUND(bool7,6) /= 1) .OR. &
          (UBOUND(bool7,7) /= 10) .OR. (LBOUND(bool7,7) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(bool7,10,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(bool7,10,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(bool7,100,100,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(bool7)) .OR. ANY(bool7) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool7,1) /= 10) .OR. (LBOUND(bool7,1) /= 1) .OR. &
          (UBOUND(bool7,2) /= 10) .OR. (LBOUND(bool7,2) /= 1) .OR. &
          (UBOUND(bool7,3) /= 10) .OR. (LBOUND(bool7,3) /= 1) .OR. &
          (UBOUND(bool7,4) /= 10) .OR. (LBOUND(bool7,4) /= 1) .OR. &
          (UBOUND(bool7,5) /= 10) .OR. (LBOUND(bool7,5) /= 1) .OR. &
          (UBOUND(bool7,6) /= 10) .OR. (LBOUND(bool7,6) /= 1) .OR. &
          (UBOUND(bool7,7) /= 10) .OR. (LBOUND(bool7,7) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(bool7,100,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(bool7,100,100,100,100,100,100,100)'
      ENDIF
      CALL demallocP(bool7)
      IF( ASSOCIATED(bool7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(bool7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(bool7)'
      ENDIF
      CALL demallocP(bool7)
      IF( ASSOCIATED(bool7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(bool7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(bool7)'
      ENDIF
      CALL dmalloc0P(bool7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(bool7)) .OR. ANY(bool7) .OR. &
          (UBOUND(bool7,1) /= 8) .OR. (LBOUND(bool7,1) /= -1) .OR. &
          (UBOUND(bool7,2) /= 8) .OR. (LBOUND(bool7,2) /= -1) .OR. &
          (UBOUND(bool7,3) /= 8) .OR. (LBOUND(bool7,3) /= -1) .OR. &
          (UBOUND(bool7,4) /= 8) .OR. (LBOUND(bool7,4) /= -1) .OR. &
          (UBOUND(bool7,5) /= 8) .OR. (LBOUND(bool7,5) /= -1) .OR. &
          (UBOUND(bool7,6) /= 8) .OR. (LBOUND(bool7,6) /= -1) .OR. &
          (UBOUND(bool7,7) /= 8) .OR. (LBOUND(bool7,7) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(bool7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(bool7)) .OR. ANY(bool7)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool7,1) /= 8) .OR. (LBOUND(bool7,1) /= -1) .OR. &
          (UBOUND(bool7,2) /= 8) .OR. (LBOUND(bool7,2) /= -1) .OR. &
          (UBOUND(bool7,3) /= 8) .OR. (LBOUND(bool7,3) /= -1) .OR. &
          (UBOUND(bool7,4) /= 8) .OR. (LBOUND(bool7,4) /= -1) .OR. &
          (UBOUND(bool7,5) /= 8) .OR. (LBOUND(bool7,5) /= -1) .OR. &
          (UBOUND(bool7,6) /= 8) .OR. (LBOUND(bool7,6) /= -1) .OR. &
          (UBOUND(bool7,7) /= 8) .OR. (LBOUND(bool7,7) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(bool7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(bool7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(bool7)
      WRITE(*,*) '---------------------------------------------------'
    ENDSUBROUTINE testBOOLP
!
!===============================================================================
! Test allocation/deallocation for booleans
    SUBROUTINE testBOOLA
      USE IntrType
      USE Allocs
      IMPLICIT NONE
    
      LOGICAL(SBK),ALLOCATABLE :: bool1(:)
      LOGICAL(SBK),ALLOCATABLE :: bool2(:,:)
      LOGICAL(SBK),ALLOCATABLE :: bool3(:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: bool4(:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: bool5(:,:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: bool6(:,:,:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: bool7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0

      WRITE(*,*) 'TESTING ALLOCS FOR LOGICAL (BOOLEAN) ALLOCATABLE TYPES'
!
! rank 1 variable
      CALL dmallocA(bool1,-10)
      CALL dmallocA(bool1,10)
      IF( (.NOT.ALLOCATED(bool1)) .OR. ANY(bool1) .OR. &
          (UBOUND(bool1,1) /= 10) .OR. (LBOUND(bool1,1) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(bool1,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(bool1,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(bool1,100)
      IF( (.NOT.ALLOCATED(bool1)) .OR. ANY(bool1) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool1,1) /= 10) .OR. (LBOUND(bool1,1) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(bool1,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(bool1,100)'
      ENDIF
      CALL demallocA(bool1)
      IF( ALLOCATED(bool1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(bool1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(bool1)'
      ENDIF
      CALL demallocA(bool1)
      IF( ALLOCATED(bool1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(bool1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(bool1)'
      ENDIF
      CALL dmalloc0A(bool1,8,-1)
      CALL dmalloc0A(bool1,-1,8)
      IF( (.NOT.ALLOCATED(bool1)) .OR. ANY(bool1) .OR. &
          (UBOUND(bool1,1) /= 8) .OR. (LBOUND(bool1,1) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(bool1,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(bool1,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool1,-1,1)
      IF( (.NOT.ALLOCATED(bool1)) .OR. ANY(bool1) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool1,1) /= 8) .OR. (LBOUND(bool1,1) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(bool1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(bool1,-1,1) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      CALL demallocA(bool1)
!
! rank 2 variable
      CALL dmallocA(bool2,-10,10)
      CALL dmallocA(bool2,10,-10)
      CALL dmallocA(bool2,10,10)
      IF((.NOT.ALLOCATED(bool2)) .OR. ANY(bool2) .OR. &
          (UBOUND(bool2,1) /= 10) .OR. (LBOUND(bool2,1) /= 1) .OR. &
          (UBOUND(bool2,2) /= 10) .OR. (LBOUND(bool2,2) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(bool2,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(bool2,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(bool2,100,100)
      IF((.NOT.ALLOCATED(bool2)) .OR. ANY(bool2) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool2,1) /= 10) .OR. (LBOUND(bool2,1) /= 1) .OR. &
          (UBOUND(bool2,2) /= 10) .OR. (LBOUND(bool2,2) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(bool2,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(bool2,100,100)'
      ENDIF
      CALL demallocA(bool2)
      IF( ALLOCATED(bool2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(bool2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(bool2)'
      ENDIF
      CALL demallocA(bool2)
      IF( ALLOCATED(bool2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(bool2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(bool2)'
      ENDIF
      CALL dmalloc0A(bool2,8,-1,-1,8)
      CALL dmalloc0A(bool2,-1,8,8,-1)
      CALL dmalloc0A(bool2,-1,8,-1,8)
      IF((.NOT.ALLOCATED(bool2)) .OR. ANY(bool2) .OR. &
          (UBOUND(bool2,1) /= 8) .OR. (LBOUND(bool2,1) /= -1) .OR. &
          (UBOUND(bool2,2) /= 8) .OR. (LBOUND(bool2,2) /= -1) ) THEN
        WRITE(*,*) 'CALL CALL dmalloc0A(bool2,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(bool2,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool2,-1,1,-1,1)
      IF((.NOT.ALLOCATED(bool2)) .OR. ANY(bool2) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool2,1) /= 8) .OR. (LBOUND(bool2,1) /= -1) .OR. &
          (UBOUND(bool2,2) /= 8) .OR. (LBOUND(bool2,2) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(bool2,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(bool2,-1,1,-1,1)'
      ENDIF
      CALL demallocA(bool2)
!
! rank 3 variable
      CALL dmallocA(bool3,-10,10,10)
      CALL dmallocA(bool3,10,-10,10)
      CALL dmallocA(bool3,10,10,-10)
      CALL dmallocA(bool3,10,10,10)
      IF((.NOT.ALLOCATED(bool3)) .OR. ANY(bool3) .OR. &
          (UBOUND(bool3,1) /= 10) .OR. (LBOUND(bool3,1) /= 1) .OR. &
          (UBOUND(bool3,2) /= 10) .OR. (LBOUND(bool3,2) /= 1) .OR. &
          (UBOUND(bool3,3) /= 10) .OR. (LBOUND(bool3,3) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(bool3,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(bool3,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(bool3,100,100,100)
      IF((.NOT.ALLOCATED(bool3)) .OR. ANY(bool3) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool3,1) /= 10) .OR. (LBOUND(bool3,1) /= 1) .OR. &
          (UBOUND(bool3,2) /= 10) .OR. (LBOUND(bool3,2) /= 1) .OR. &
          (UBOUND(bool3,3) /= 10) .OR. (LBOUND(bool3,3) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(bool3,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(bool3,100,100,100)'
      ENDIF
      CALL demallocA(bool3)
      IF( ALLOCATED(bool3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(bool3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(bool3)'
      ENDIF
      CALL demallocA(bool3)
      IF( ALLOCATED(bool3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(bool3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(bool3)'
      ENDIF
      CALL dmalloc0A(bool3,8,-1,-1,8,-1,8)
      CALL dmalloc0A(bool3,-1,8,8,-1,-1,8)
      CALL dmalloc0A(bool3,-1,8,-1,8,8,-1)
      CALL dmalloc0A(bool3,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(bool3)) .OR. ANY(bool3) .OR. &
          (UBOUND(bool3,1) /= 8) .OR. (LBOUND(bool3,1) /= -1) .OR. &
          (UBOUND(bool3,2) /= 8) .OR. (LBOUND(bool3,2) /= -1) .OR. &
          (UBOUND(bool3,3) /= 8) .OR. (LBOUND(bool3,3) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(bool3,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(bool3,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool3,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(bool3)) .OR. ANY(bool3) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool3,1) /= 8) .OR. (LBOUND(bool3,1) /= -1) .OR. &
          (UBOUND(bool3,2) /= 8) .OR. (LBOUND(bool3,2) /= -1) .OR. &
          (UBOUND(bool3,3) /= 8) .OR. (LBOUND(bool3,3) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(bool3,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(bool3,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(bool3)
!
! rank 4 variable
      CALL dmallocA(bool4,-10,10,10,10)
      CALL dmallocA(bool4,10,-10,10,10)
      CALL dmallocA(bool4,10,10,-10,10)
      CALL dmallocA(bool4,10,10,10,-10)
      CALL dmallocA(bool4,10,10,10,10)
      IF((.NOT.ALLOCATED(bool4)) .OR. ANY(bool4) .OR. &
          (UBOUND(bool4,1) /= 10) .OR. (LBOUND(bool4,1) /= 1) .OR. &
          (UBOUND(bool4,2) /= 10) .OR. (LBOUND(bool4,2) /= 1) .OR. &
          (UBOUND(bool4,3) /= 10) .OR. (LBOUND(bool4,3) /= 1) .OR. &
          (UBOUND(bool4,4) /= 10) .OR. (LBOUND(bool4,4) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(bool4,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(bool4,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(bool4,100,100,100,100)
      IF((.NOT.ALLOCATED(bool4)) .OR. ANY(bool4) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool4,1) /= 10) .OR. (LBOUND(bool4,1) /= 1) .OR. &
          (UBOUND(bool4,2) /= 10) .OR. (LBOUND(bool4,2) /= 1) .OR. &
          (UBOUND(bool4,3) /= 10) .OR. (LBOUND(bool4,3) /= 1) .OR. &
          (UBOUND(bool4,4) /= 10) .OR. (LBOUND(bool4,4) /= 1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmallocA(bool4,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA((bool4,100,100,100,100)'
      ENDIF
      CALL demallocA(bool4)
      IF( ALLOCATED(bool4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(bool4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(bool4)'
      ENDIF
      CALL demallocA(bool4)
      IF( ALLOCATED(bool4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(bool4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(bool4)'
      ENDIF
      CALL dmalloc0A(bool4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(bool4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(bool4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(bool4,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(bool4)) .OR. ANY(bool4) .OR. &
          (UBOUND(bool4,1) /= 8) .OR. (LBOUND(bool4,1) /= -1) .OR. &
          (UBOUND(bool4,2) /= 8) .OR. (LBOUND(bool4,2) /= -1) .OR. &
          (UBOUND(bool4,3) /= 8) .OR. (LBOUND(bool4,3) /= -1) .OR. &
          (UBOUND(bool4,4) /= 8) .OR. (LBOUND(bool4,4) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(bool4,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(bool4,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool4,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(bool4)) .OR. ANY(bool4) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool4,1) /= 8) .OR. (LBOUND(bool4,1) /= -1) .OR. &
          (UBOUND(bool4,2) /= 8) .OR. (LBOUND(bool4,2) /= -1) .OR. &
          (UBOUND(bool4,3) /= 8) .OR. (LBOUND(bool4,3) /= -1) .OR. &
          (UBOUND(bool4,4) /= 8) .OR. (LBOUND(bool4,4) /= -1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmalloc0A(bool4,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(bool4,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(bool4)
!
! rank 5 variable
      CALL dmallocA(bool5,-10,10,10,10,10)
      CALL dmallocA(bool5,10,-10,10,10,10)
      CALL dmallocA(bool5,10,10,-10,10,10)
      CALL dmallocA(bool5,10,10,10,-10,10)
      CALL dmallocA(bool5,10,10,10,10,-10)
      CALL dmallocA(bool5,10,10,10,10,10)
      IF((.NOT.ALLOCATED(bool5)) .OR. ANY(bool5) .OR. &
          (UBOUND(bool5,1) /= 10) .OR. (LBOUND(bool5,1) /= 1) .OR. &
          (UBOUND(bool5,2) /= 10) .OR. (LBOUND(bool5,2) /= 1) .OR. &
          (UBOUND(bool5,3) /= 10) .OR. (LBOUND(bool5,3) /= 1) .OR. &
          (UBOUND(bool5,4) /= 10) .OR. (LBOUND(bool5,4) /= 1) .OR. &
          (UBOUND(bool5,5) /= 10) .OR. (LBOUND(bool5,5) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(bool5,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(bool5,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(bool5,100,100,100,100,100)
      IF((.NOT.ALLOCATED(bool5)) .OR. ANY(bool5)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool5,1) /= 10) .OR. (LBOUND(bool5,1) /= 1) .OR. &
          (UBOUND(bool5,2) /= 10) .OR. (LBOUND(bool5,2) /= 1) .OR. &
          (UBOUND(bool5,3) /= 10) .OR. (LBOUND(bool5,3) /= 1) .OR. &
          (UBOUND(bool5,4) /= 10) .OR. (LBOUND(bool5,4) /= 1) .OR. &
          (UBOUND(bool5,5) /= 10) .OR. (LBOUND(bool5,5) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(bool5,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(bool5,100,100,100,100,100)'
      ENDIF
      CALL demallocA(bool5)
      IF( ALLOCATED(bool5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(bool5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(bool5)'
      ENDIF
      CALL demallocA(bool5)
      IF( ALLOCATED(bool5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(bool5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(bool5)'
      ENDIF
      CALL dmalloc0A(bool5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(bool5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(bool5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(bool5,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(bool5)) .OR. ANY(bool5) .OR. &
          (UBOUND(bool5,1) /= 8) .OR. (LBOUND(bool5,1) /= -1) .OR. &
          (UBOUND(bool5,2) /= 8) .OR. (LBOUND(bool5,2) /= -1) .OR. &
          (UBOUND(bool5,3) /= 8) .OR. (LBOUND(bool5,3) /= -1) .OR. &
          (UBOUND(bool5,4) /= 8) .OR. (LBOUND(bool5,4) /= -1) .OR. &
          (UBOUND(bool5,5) /= 8) .OR. (LBOUND(bool5,5) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocA(bool5,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(bool5,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool5,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(bool5)) .OR. ANY(bool5)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool5,1) /= 8) .OR. (LBOUND(bool5,1) /= -1) .OR. &
          (UBOUND(bool5,2) /= 8) .OR. (LBOUND(bool5,2) /= -1) .OR. &
          (UBOUND(bool5,3) /= 8) .OR. (LBOUND(bool5,3) /= -1) .OR. &
          (UBOUND(bool5,4) /= 8) .OR. (LBOUND(bool5,4) /= -1) .OR. &
          (UBOUND(bool5,5) /= 8) .OR. (LBOUND(bool5,5) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0A(bool5,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(bool5,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(bool5)
!
! rank 6 variable
      CALL dmallocA(bool6,-10,10,10,10,10,10)
      CALL dmallocA(bool6,10,-10,10,10,10,10)
      CALL dmallocA(bool6,10,10,-10,10,10,10)
      CALL dmallocA(bool6,10,10,10,-10,10,10)
      CALL dmallocA(bool6,10,10,10,10,-10,10)
      CALL dmallocA(bool6,10,10,10,10,10,-10)
      CALL dmallocA(bool6,10,10,10,10,10,10)
      IF((.NOT.ALLOCATED(bool6)) .OR. ANY(bool6) .OR. &
          (UBOUND(bool6,1) /= 10) .OR. (LBOUND(bool6,1) /= 1) .OR. &
          (UBOUND(bool6,2) /= 10) .OR. (LBOUND(bool6,2) /= 1) .OR. &
          (UBOUND(bool6,3) /= 10) .OR. (LBOUND(bool6,3) /= 1) .OR. &
          (UBOUND(bool6,4) /= 10) .OR. (LBOUND(bool6,4) /= 1) .OR. &
          (UBOUND(bool6,5) /= 10) .OR. (LBOUND(bool6,5) /= 1) .OR. &
          (UBOUND(bool6,6) /= 10) .OR. (LBOUND(bool6,6) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(bool6,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(bool6,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(bool6,100,100,100,100,100,100)
      IF((.NOT.ALLOCATED(bool6)) .OR. ANY(bool6) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool6,1) /= 10) .OR. (LBOUND(bool6,1) /= 1) .OR. &
          (UBOUND(bool6,2) /= 10) .OR. (LBOUND(bool6,2) /= 1) .OR. &
          (UBOUND(bool6,3) /= 10) .OR. (LBOUND(bool6,3) /= 1) .OR. &
          (UBOUND(bool6,4) /= 10) .OR. (LBOUND(bool6,4) /= 1) .OR. &
          (UBOUND(bool6,5) /= 10) .OR. (LBOUND(bool6,5) /= 1) .OR. &
          (UBOUND(bool6,6) /= 10) .OR. (LBOUND(bool6,6) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(bool6,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(bool6,100,100,100,100,100,100)'
      ENDIF
      CALL demallocA(bool6)
      IF( ALLOCATED(bool6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(bool6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(bool6)'
      ENDIF
      CALL demallocA(bool6)
      IF( ALLOCATED(bool6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(bool6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(bool6)'
      ENDIF
      CALL dmalloc0A(bool6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(bool6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(bool6)) .OR. ANY(bool6) .OR. &
          (UBOUND(bool6,1) /= 8) .OR. (LBOUND(bool6,1) /= -1) .OR. &
          (UBOUND(bool6,2) /= 8) .OR. (LBOUND(bool6,2) /= -1) .OR. &
          (UBOUND(bool6,3) /= 8) .OR. (LBOUND(bool6,3) /= -1) .OR. &
          (UBOUND(bool6,4) /= 8) .OR. (LBOUND(bool6,4) /= -1) .OR. &
          (UBOUND(bool6,5) /= 8) .OR. (LBOUND(bool6,5) /= -1) .OR. &
          (UBOUND(bool6,6) /= 8) .OR. (LBOUND(bool6,6) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocA(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(bool6)) .OR. ANY(bool6)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool6,1) /= 8) .OR. (LBOUND(bool6,1) /= -1) .OR. &
          (UBOUND(bool6,2) /= 8) .OR. (LBOUND(bool6,2) /= -1) .OR. &
          (UBOUND(bool6,3) /= 8) .OR. (LBOUND(bool6,3) /= -1) .OR. &
          (UBOUND(bool6,4) /= 8) .OR. (LBOUND(bool6,4) /= -1) .OR. &
          (UBOUND(bool6,5) /= 8) .OR. (LBOUND(bool6,5) /= -1) .OR. &
          (UBOUND(bool6,6) /= 8) .OR. (LBOUND(bool6,6) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0A(bool6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(bool6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(bool6)
!
! rank 7 variable
      CALL dmallocA(bool7,-10,10,10,10,10,10,10)
      CALL dmallocA(bool7,10,-10,10,10,10,10,10)
      CALL dmallocA(bool7,10,10,-10,10,10,10,10)
      CALL dmallocA(bool7,10,10,10,-10,10,10,10)
      CALL dmallocA(bool7,10,10,10,10,-10,10,10)
      CALL dmallocA(bool7,10,10,10,10,10,-10,10)
      CALL dmallocA(bool7,10,10,10,10,10,10,-10)
      CALL dmallocA(bool7,10,10,10,10,10,10,10)
      IF((.NOT.ALLOCATED(bool7)) .OR. ANY(bool7) .OR. &
          (UBOUND(bool7,1) /= 10) .OR. (LBOUND(bool7,1) /= 1) .OR. &
          (UBOUND(bool7,2) /= 10) .OR. (LBOUND(bool7,2) /= 1) .OR. &
          (UBOUND(bool7,3) /= 10) .OR. (LBOUND(bool7,3) /= 1) .OR. &
          (UBOUND(bool7,4) /= 10) .OR. (LBOUND(bool7,4) /= 1) .OR. &
          (UBOUND(bool7,5) /= 10) .OR. (LBOUND(bool7,5) /= 1) .OR. &
          (UBOUND(bool7,6) /= 10) .OR. (LBOUND(bool7,6) /= 1) .OR. &
          (UBOUND(bool7,7) /= 10) .OR. (LBOUND(bool7,7) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(bool7,10,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(bool7,10,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(bool7,100,100,100,100,100,100,100)
      IF((.NOT.ALLOCATED(bool7)) .OR. ANY(bool7) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool7,1) /= 10) .OR. (LBOUND(bool7,1) /= 1) .OR. &
          (UBOUND(bool7,2) /= 10) .OR. (LBOUND(bool7,2) /= 1) .OR. &
          (UBOUND(bool7,3) /= 10) .OR. (LBOUND(bool7,3) /= 1) .OR. &
          (UBOUND(bool7,4) /= 10) .OR. (LBOUND(bool7,4) /= 1) .OR. &
          (UBOUND(bool7,5) /= 10) .OR. (LBOUND(bool7,5) /= 1) .OR. &
          (UBOUND(bool7,6) /= 10) .OR. (LBOUND(bool7,6) /= 1) .OR. &
          (UBOUND(bool7,7) /= 10) .OR. (LBOUND(bool7,7) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(bool7,100,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(bool7,100,100,100,100,100,100,100)'
      ENDIF
      CALL demallocA(bool7)
      IF( ALLOCATED(bool7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(bool7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(bool7)'
      ENDIF
      CALL demallocA(bool7)
      IF( ALLOCATED(bool7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(bool7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(bool7)'
      ENDIF
      CALL dmalloc0A(bool7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(bool7)) .OR. ANY(bool7) .OR. &
          (UBOUND(bool7,1) /= 8) .OR. (LBOUND(bool7,1) /= -1) .OR. &
          (UBOUND(bool7,2) /= 8) .OR. (LBOUND(bool7,2) /= -1) .OR. &
          (UBOUND(bool7,3) /= 8) .OR. (LBOUND(bool7,3) /= -1) .OR. &
          (UBOUND(bool7,4) /= 8) .OR. (LBOUND(bool7,4) /= -1) .OR. &
          (UBOUND(bool7,5) /= 8) .OR. (LBOUND(bool7,5) /= -1) .OR. &
          (UBOUND(bool7,6) /= 8) .OR. (LBOUND(bool7,6) /= -1) .OR. &
          (UBOUND(bool7,7) /= 8) .OR. (LBOUND(bool7,7) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(bool7)) .OR. ANY(bool7)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool7,1) /= 8) .OR. (LBOUND(bool7,1) /= -1) .OR. &
          (UBOUND(bool7,2) /= 8) .OR. (LBOUND(bool7,2) /= -1) .OR. &
          (UBOUND(bool7,3) /= 8) .OR. (LBOUND(bool7,3) /= -1) .OR. &
          (UBOUND(bool7,4) /= 8) .OR. (LBOUND(bool7,4) /= -1) .OR. &
          (UBOUND(bool7,5) /= 8) .OR. (LBOUND(bool7,5) /= -1) .OR. &
          (UBOUND(bool7,6) /= 8) .OR. (LBOUND(bool7,6) /= -1) .OR. &
          (UBOUND(bool7,7) /= 8) .OR. (LBOUND(bool7,7) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(bool7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(bool7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(bool7)
      WRITE(*,*) '---------------------------------------------------'
    ENDSUBROUTINE testBOOLA
!
!===============================================================================
! Test allocation/deallocation for integers
    SUBROUTINE testINTP
      USE IntrType
      USE Allocs
      IMPLICIT NONE
    
      INTEGER(SNK),POINTER :: int_1(:)
      INTEGER(SNK),POINTER :: int_2(:,:)
      INTEGER(SNK),POINTER :: int_3(:,:,:)
      INTEGER(SNK),POINTER :: int_4(:,:,:,:)
      INTEGER(SNK),POINTER :: int_5(:,:,:,:,:)
      INTEGER(SNK),POINTER :: int_6(:,:,:,:,:,:)
      INTEGER(SNK),POINTER :: int_7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0
      
      NULLIFY(int_1,int_2,int_3,int_4,int_5,int_6,int_7)
      
      WRITE(*,*) 'TESTING ALLOCS FOR INTEGER POINTER TYPES'
!
! rank 1 variable
      CALL dmallocP(int_1,-10)
      CALL dmallocP(int_1,10)
      IF( (.NOT.ASSOCIATED(int_1)) .OR. ANY(int_1 /= 0) .OR. &
          (UBOUND(int_1,DIM=1) /= 10) .OR. (LBOUND(int_1,DIM=1) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(int_1,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(int_1,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(int_1,100)
      IF( (.NOT.ASSOCIATED(int_1)) .OR. ANY(int_1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_1,1) /= 10) .OR. (LBOUND(int_1,1) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(int_1,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(int_1,100)'
      ENDIF
      CALL demallocP(int_1)
      IF( ASSOCIATED(int_1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(int_1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(int_1)'
      ENDIF
      CALL demallocP(int_1)
      IF( ASSOCIATED(int_1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(int_1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(int_1)'
      ENDIF
      CALL dmalloc0P(int_1,8,-1)
      CALL dmalloc0P(int_1,-1,8)
      IF( (.NOT.ASSOCIATED(int_1)) .OR. ANY(int_1 /= 0) .OR. &
          (UBOUND(int_1,1) /= 8) .OR. (LBOUND(int_1,1) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(int_1,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(int_1,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_1,-1,1)
      IF( (.NOT.ASSOCIATED(int_1)) .OR. ANY(int_1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_1,1) /= 8) .OR. (LBOUND(int_1,1) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(int_1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(int_1,-1,1) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      CALL demallocP(int_1)
!
! rank 2 variable
      CALL dmallocP(int_2,-10,10)
      CALL dmallocP(int_2,10,-10)
      CALL dmallocP(int_2,10,10)
      IF((.NOT.ASSOCIATED(int_2)) .OR. ANY(int_2 /= 0) .OR. &
          (UBOUND(int_2,1) /= 10) .OR. (LBOUND(int_2,1) /= 1) .OR. &
          (UBOUND(int_2,2) /= 10) .OR. (LBOUND(int_2,2) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(int_2,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(int_2,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(int_2,100,100)
      IF((.NOT.ASSOCIATED(int_2)) .OR. ANY(int_2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_2,1) /= 10) .OR. (LBOUND(int_2,1) /= 1) .OR. &
          (UBOUND(int_2,2) /= 10) .OR. (LBOUND(int_2,2) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(int_2,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(int_2,100,100)'
      ENDIF
      CALL demallocP(int_2)
      IF( ASSOCIATED(int_2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(int_2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(int_2)'
      ENDIF
      CALL demallocP(int_2)
      IF( ASSOCIATED(int_2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(int_2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(int_2)'
      ENDIF
      CALL dmalloc0P(int_2,8,-1,-1,8)
      CALL dmalloc0P(int_2,-1,8,8,-1)
      CALL dmalloc0P(int_2,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(int_2)) .OR. ANY(int_2 /= 0) .OR. &
          (UBOUND(int_2,1) /= 8) .OR. (LBOUND(int_2,1) /= -1) .OR. &
          (UBOUND(int_2,2) /= 8) .OR. (LBOUND(int_2,2) /= -1) ) THEN
        WRITE(*,*) 'CALL CALL dmalloc0P(int_2,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(int_2,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_2,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(int_2)) .OR. ANY(int_2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_2,1) /= 8) .OR. (LBOUND(int_2,1) /= -1) .OR. &
          (UBOUND(int_2,2) /= 8) .OR. (LBOUND(int_2,2) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(int_2,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(int_2,-1,1,-1,1)'
      ENDIF
      CALL demallocP(int_2)
!
! rank 3 variable
      CALL dmallocP(int_3,-10,10,10)
      CALL dmallocP(int_3,10,-10,10)
      CALL dmallocP(int_3,10,10,-10)
      CALL dmallocP(int_3,10,10,10)
      IF((.NOT.ASSOCIATED(int_3)) .OR. ANY(int_3 /= 0) .OR. &
          (UBOUND(int_3,1) /= 10) .OR. (LBOUND(int_3,1) /= 1) .OR. &
          (UBOUND(int_3,2) /= 10) .OR. (LBOUND(int_3,2) /= 1) .OR. &
          (UBOUND(int_3,3) /= 10) .OR. (LBOUND(int_3,3) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(int_3,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(int_3,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(int_3,100,100,100)
      IF((.NOT.ASSOCIATED(int_3)) .OR. ANY(int_3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_3,1) /= 10) .OR. (LBOUND(int_3,1) /= 1) .OR. &
          (UBOUND(int_3,2) /= 10) .OR. (LBOUND(int_3,2) /= 1) .OR. &
          (UBOUND(int_3,3) /= 10) .OR. (LBOUND(int_3,3) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(int_3,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(int_3,100,100,100)'
      ENDIF
      CALL demallocP(int_3)
      IF( ASSOCIATED(int_3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(int_3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(int_3)'
      ENDIF
      CALL demallocP(int_3)
      IF( ASSOCIATED(int_3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(int_3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(int_3)'
      ENDIF
      CALL dmalloc0P(int_3,8,-1,-1,8,-1,8)
      CALL dmalloc0P(int_3,-1,8,8,-1,-1,8)
      CALL dmalloc0P(int_3,-1,8,-1,8,8,-1)
      CALL dmalloc0P(int_3,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(int_3)) .OR. ANY(int_3 /= 0) .OR. &
          (UBOUND(int_3,1) /= 8) .OR. (LBOUND(int_3,1) /= -1) .OR. &
          (UBOUND(int_3,2) /= 8) .OR. (LBOUND(int_3,2) /= -1) .OR. &
          (UBOUND(int_3,3) /= 8) .OR. (LBOUND(int_3,3) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(int_3,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(int_3,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_3,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(int_3)) .OR. ANY(int_3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_3,1) /= 8) .OR. (LBOUND(int_3,1) /= -1) .OR. &
          (UBOUND(int_3,2) /= 8) .OR. (LBOUND(int_3,2) /= -1) .OR. &
          (UBOUND(int_3,3) /= 8) .OR. (LBOUND(int_3,3) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(int_3,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(int_3,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(int_3)
!
! rank 4 variable
      CALL dmallocP(int_4,-10,10,10,10)
      CALL dmallocP(int_4,10,-10,10,10)
      CALL dmallocP(int_4,10,10,-10,10)
      CALL dmallocP(int_4,10,10,10,-10)
      CALL dmallocP(int_4,10,10,10,10)
      IF((.NOT.ASSOCIATED(int_4)) .OR. ANY(int_4 /= 0) .OR. &
          (UBOUND(int_4,1) /= 10) .OR. (LBOUND(int_4,1) /= 1) .OR. &
          (UBOUND(int_4,2) /= 10) .OR. (LBOUND(int_4,2) /= 1) .OR. &
          (UBOUND(int_4,3) /= 10) .OR. (LBOUND(int_4,3) /= 1) .OR. &
          (UBOUND(int_4,4) /= 10) .OR. (LBOUND(int_4,4) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(int_4,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(int_4,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(int_4,100,100,100,100)
      IF((.NOT.ASSOCIATED(int_4)) .OR. ANY(int_4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_4,1) /= 10) .OR. (LBOUND(int_4,1) /= 1) .OR. &
          (UBOUND(int_4,2) /= 10) .OR. (LBOUND(int_4,2) /= 1) .OR. &
          (UBOUND(int_4,3) /= 10) .OR. (LBOUND(int_4,3) /= 1) .OR. &
          (UBOUND(int_4,4) /= 10) .OR. (LBOUND(int_4,4) /= 1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmallocP(int_4,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP((int_4,100,100,100,100)'
      ENDIF
      CALL demallocP(int_4)
      IF( ASSOCIATED(int_4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(int_4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(int_4)'
      ENDIF
      CALL demallocP(int_4)
      IF( ASSOCIATED(int_4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(int_4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(int_4)'
      ENDIF
      CALL dmalloc0P(int_4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(int_4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(int_4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(int_4,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(int_4)) .OR. ANY(int_4 /= 0) .OR. &
          (UBOUND(int_4,1) /= 8) .OR. (LBOUND(int_4,1) /= -1) .OR. &
          (UBOUND(int_4,2) /= 8) .OR. (LBOUND(int_4,2) /= -1) .OR. &
          (UBOUND(int_4,3) /= 8) .OR. (LBOUND(int_4,3) /= -1) .OR. &
          (UBOUND(int_4,4) /= 8) .OR. (LBOUND(int_4,4) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(int_4,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(int_4,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_4,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(int_4)) .OR. ANY(int_4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_4,1) /= 8) .OR. (LBOUND(int_4,1) /= -1) .OR. &
          (UBOUND(int_4,2) /= 8) .OR. (LBOUND(int_4,2) /= -1) .OR. &
          (UBOUND(int_4,3) /= 8) .OR. (LBOUND(int_4,3) /= -1) .OR. &
          (UBOUND(int_4,4) /= 8) .OR. (LBOUND(int_4,4) /= -1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmalloc0P(int_4,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(int_4,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(int_4)
!
! rank 5 variable
      CALL dmallocP(int_5,-10,10,10,10,10)
      CALL dmallocP(int_5,10,-10,10,10,10)
      CALL dmallocP(int_5,10,10,-10,10,10)
      CALL dmallocP(int_5,10,10,10,-10,10)
      CALL dmallocP(int_5,10,10,10,10,-10)
      CALL dmallocP(int_5,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(int_5)) .OR. ANY(int_5 /= 0) .OR. &
          (UBOUND(int_5,1) /= 10) .OR. (LBOUND(int_5,1) /= 1) .OR. &
          (UBOUND(int_5,2) /= 10) .OR. (LBOUND(int_5,2) /= 1) .OR. &
          (UBOUND(int_5,3) /= 10) .OR. (LBOUND(int_5,3) /= 1) .OR. &
          (UBOUND(int_5,4) /= 10) .OR. (LBOUND(int_5,4) /= 1) .OR. &
          (UBOUND(int_5,5) /= 10) .OR. (LBOUND(int_5,5) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(int_5,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(int_5,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(int_5,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(int_5)) .OR. ANY(int_5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_5,1) /= 10) .OR. (LBOUND(int_5,1) /= 1) .OR. &
          (UBOUND(int_5,2) /= 10) .OR. (LBOUND(int_5,2) /= 1) .OR. &
          (UBOUND(int_5,3) /= 10) .OR. (LBOUND(int_5,3) /= 1) .OR. &
          (UBOUND(int_5,4) /= 10) .OR. (LBOUND(int_5,4) /= 1) .OR. &
          (UBOUND(int_5,5) /= 10) .OR. (LBOUND(int_5,5) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(int_5,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(int_5,100,100,100,100,100)'
      ENDIF
      CALL demallocP(int_5)
      IF( ASSOCIATED(int_5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(int_5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(int_5)'
      ENDIF
      CALL demallocP(int_5)
      IF( ASSOCIATED(int_5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(int_5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(int_5)'
      ENDIF
      CALL dmalloc0P(int_5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(int_5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(int_5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(int_5,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(int_5)) .OR. ANY(int_5 /= 0) .OR. &
          (UBOUND(int_5,1) /= 8) .OR. (LBOUND(int_5,1) /= -1) .OR. &
          (UBOUND(int_5,2) /= 8) .OR. (LBOUND(int_5,2) /= -1) .OR. &
          (UBOUND(int_5,3) /= 8) .OR. (LBOUND(int_5,3) /= -1) .OR. &
          (UBOUND(int_5,4) /= 8) .OR. (LBOUND(int_5,4) /= -1) .OR. &
          (UBOUND(int_5,5) /= 8) .OR. (LBOUND(int_5,5) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocP(int_5,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(int_5,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_5,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(int_5)) .OR. ANY(int_5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_5,1) /= 8) .OR. (LBOUND(int_5,1) /= -1) .OR. &
          (UBOUND(int_5,2) /= 8) .OR. (LBOUND(int_5,2) /= -1) .OR. &
          (UBOUND(int_5,3) /= 8) .OR. (LBOUND(int_5,3) /= -1) .OR. &
          (UBOUND(int_5,4) /= 8) .OR. (LBOUND(int_5,4) /= -1) .OR. &
          (UBOUND(int_5,5) /= 8) .OR. (LBOUND(int_5,5) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0P(int_5,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(int_5,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(int_5)
!
! rank 6 variable
      CALL dmallocP(int_6,-10,10,10,10,10,10)
      CALL dmallocP(int_6,10,-10,10,10,10,10)
      CALL dmallocP(int_6,10,10,-10,10,10,10)
      CALL dmallocP(int_6,10,10,10,-10,10,10)
      CALL dmallocP(int_6,10,10,10,10,-10,10)
      CALL dmallocP(int_6,10,10,10,10,10,-10)
      CALL dmallocP(int_6,10,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(int_6)) .OR. ANY(int_6 /= 0) .OR. &
          (UBOUND(int_6,1) /= 10) .OR. (LBOUND(int_6,1) /= 1) .OR. &
          (UBOUND(int_6,2) /= 10) .OR. (LBOUND(int_6,2) /= 1) .OR. &
          (UBOUND(int_6,3) /= 10) .OR. (LBOUND(int_6,3) /= 1) .OR. &
          (UBOUND(int_6,4) /= 10) .OR. (LBOUND(int_6,4) /= 1) .OR. &
          (UBOUND(int_6,5) /= 10) .OR. (LBOUND(int_6,5) /= 1) .OR. &
          (UBOUND(int_6,6) /= 10) .OR. (LBOUND(int_6,6) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(int_6,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(int_6,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(int_6,100,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(int_6)) .OR. ANY(int_6 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_6,1) /= 10) .OR. (LBOUND(int_6,1) /= 1) .OR. &
          (UBOUND(int_6,2) /= 10) .OR. (LBOUND(int_6,2) /= 1) .OR. &
          (UBOUND(int_6,3) /= 10) .OR. (LBOUND(int_6,3) /= 1) .OR. &
          (UBOUND(int_6,4) /= 10) .OR. (LBOUND(int_6,4) /= 1) .OR. &
          (UBOUND(int_6,5) /= 10) .OR. (LBOUND(int_6,5) /= 1) .OR. &
          (UBOUND(int_6,6) /= 10) .OR. (LBOUND(int_6,6) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(int_6,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(int_6,100,100,100,100,100,100)'
      ENDIF
      CALL demallocP(int_6)
      IF( ASSOCIATED(int_6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(int_6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(int_6)'
      ENDIF
      CALL demallocP(int_6)
      IF( ASSOCIATED(int_6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(int_6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(int_6)'
      ENDIF
      CALL dmalloc0P(int_6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(int_6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(int_6)) .OR. ANY(int_6 /= 0) .OR. &
          (UBOUND(int_6,1) /= 8) .OR. (LBOUND(int_6,1) /= -1) .OR. &
          (UBOUND(int_6,2) /= 8) .OR. (LBOUND(int_6,2) /= -1) .OR. &
          (UBOUND(int_6,3) /= 8) .OR. (LBOUND(int_6,3) /= -1) .OR. &
          (UBOUND(int_6,4) /= 8) .OR. (LBOUND(int_6,4) /= -1) .OR. &
          (UBOUND(int_6,5) /= 8) .OR. (LBOUND(int_6,5) /= -1) .OR. &
          (UBOUND(int_6,6) /= 8) .OR. (LBOUND(int_6,6) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocP(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(int_6)) .OR. ANY(int_6 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_6,1) /= 8) .OR. (LBOUND(int_6,1) /= -1) .OR. &
          (UBOUND(int_6,2) /= 8) .OR. (LBOUND(int_6,2) /= -1) .OR. &
          (UBOUND(int_6,3) /= 8) .OR. (LBOUND(int_6,3) /= -1) .OR. &
          (UBOUND(int_6,4) /= 8) .OR. (LBOUND(int_6,4) /= -1) .OR. &
          (UBOUND(int_6,5) /= 8) .OR. (LBOUND(int_6,5) /= -1) .OR. &
          (UBOUND(int_6,6) /= 8) .OR. (LBOUND(int_6,6) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0P(int_6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(int_6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(int_6)
!
! rank 7 variable
      CALL dmallocP(int_7,-10,10,10,10,10,10,10)
      CALL dmallocP(int_7,10,-10,10,10,10,10,10)
      CALL dmallocP(int_7,10,10,-10,10,10,10,10)
      CALL dmallocP(int_7,10,10,10,-10,10,10,10)
      CALL dmallocP(int_7,10,10,10,10,-10,10,10)
      CALL dmallocP(int_7,10,10,10,10,10,-10,10)
      CALL dmallocP(int_7,10,10,10,10,10,10,-10)
      CALL dmallocP(int_7,10,10,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(int_7)) .OR. ANY(int_7 /= 0) .OR. &
          (UBOUND(int_7,1) /= 10) .OR. (LBOUND(int_7,1) /= 1) .OR. &
          (UBOUND(int_7,2) /= 10) .OR. (LBOUND(int_7,2) /= 1) .OR. &
          (UBOUND(int_7,3) /= 10) .OR. (LBOUND(int_7,3) /= 1) .OR. &
          (UBOUND(int_7,4) /= 10) .OR. (LBOUND(int_7,4) /= 1) .OR. &
          (UBOUND(int_7,5) /= 10) .OR. (LBOUND(int_7,5) /= 1) .OR. &
          (UBOUND(int_7,6) /= 10) .OR. (LBOUND(int_7,6) /= 1) .OR. &
          (UBOUND(int_7,7) /= 10) .OR. (LBOUND(int_7,7) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(int_7,10,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(int_7,10,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(int_7,100,100,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(int_7)) .OR. ANY(int_7 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_7,1) /= 10) .OR. (LBOUND(int_7,1) /= 1) .OR. &
          (UBOUND(int_7,2) /= 10) .OR. (LBOUND(int_7,2) /= 1) .OR. &
          (UBOUND(int_7,3) /= 10) .OR. (LBOUND(int_7,3) /= 1) .OR. &
          (UBOUND(int_7,4) /= 10) .OR. (LBOUND(int_7,4) /= 1) .OR. &
          (UBOUND(int_7,5) /= 10) .OR. (LBOUND(int_7,5) /= 1) .OR. &
          (UBOUND(int_7,6) /= 10) .OR. (LBOUND(int_7,6) /= 1) .OR. &
          (UBOUND(int_7,7) /= 10) .OR. (LBOUND(int_7,7) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(int_7,100,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(int_7,100,100,100,100,100,100,100)'
      ENDIF
      CALL demallocP(int_7)
      IF( ASSOCIATED(int_7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(int_7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(int_7)'
      ENDIF
      CALL demallocP(int_7)
      IF( ASSOCIATED(int_7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(int_7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(int_7)'
      ENDIF
      CALL dmalloc0P(int_7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(int_7)) .OR. ANY(int_7 /= 0) .OR. &
          (UBOUND(int_7,1) /= 8) .OR. (LBOUND(int_7,1) /= -1) .OR. &
          (UBOUND(int_7,2) /= 8) .OR. (LBOUND(int_7,2) /= -1) .OR. &
          (UBOUND(int_7,3) /= 8) .OR. (LBOUND(int_7,3) /= -1) .OR. &
          (UBOUND(int_7,4) /= 8) .OR. (LBOUND(int_7,4) /= -1) .OR. &
          (UBOUND(int_7,5) /= 8) .OR. (LBOUND(int_7,5) /= -1) .OR. &
          (UBOUND(int_7,6) /= 8) .OR. (LBOUND(int_7,6) /= -1) .OR. &
          (UBOUND(int_7,7) /= 8) .OR. (LBOUND(int_7,7) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(int_7)) .OR. ANY(int_7 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_7,1) /= 8) .OR. (LBOUND(int_7,1) /= -1) .OR. &
          (UBOUND(int_7,2) /= 8) .OR. (LBOUND(int_7,2) /= -1) .OR. &
          (UBOUND(int_7,3) /= 8) .OR. (LBOUND(int_7,3) /= -1) .OR. &
          (UBOUND(int_7,4) /= 8) .OR. (LBOUND(int_7,4) /= -1) .OR. &
          (UBOUND(int_7,5) /= 8) .OR. (LBOUND(int_7,5) /= -1) .OR. &
          (UBOUND(int_7,6) /= 8) .OR. (LBOUND(int_7,6) /= -1) .OR. &
          (UBOUND(int_7,7) /= 8) .OR. (LBOUND(int_7,7) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(int_7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(int_7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(int_7)
      WRITE(*,*) '---------------------------------------------------'
    ENDSUBROUTINE testINTP
!
!===============================================================================
! Test allocation/deallocation for integers
    SUBROUTINE testINTA
      USE IntrType
      USE Allocs
      IMPLICIT NONE
    
      INTEGER(SNK),ALLOCATABLE :: int_1(:)
      INTEGER(SNK),ALLOCATABLE :: int_2(:,:)
      INTEGER(SNK),ALLOCATABLE :: int_3(:,:,:)
      INTEGER(SNK),ALLOCATABLE :: int_4(:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: int_5(:,:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: int_6(:,:,:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: int_7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0

      WRITE(*,*) 'TESTING ALLOCS FOR INTEGER ALLOCATABLE TYPES'
!
! rank 1 variable
      CALL dmallocA(int_1,-10)
      CALL dmallocA(int_1,10)
      IF( (.NOT.ALLOCATED(int_1)) .OR. ANY(int_1 /= 0) .OR. &
          (UBOUND(int_1,1) /= 10) .OR. (LBOUND(int_1,1) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(int_1,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(int_1,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(int_1,100)
      IF( (.NOT.ALLOCATED(int_1)) .OR. ANY(int_1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_1,1) /= 10) .OR. (LBOUND(int_1,1) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(int_1,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(int_1,100)'
      ENDIF
      CALL demallocA(int_1)
      IF( ALLOCATED(int_1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(int_1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(int_1)'
      ENDIF
      CALL demallocA(int_1)
      IF( ALLOCATED(int_1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(int_1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(int_1)'
      ENDIF
      CALL dmalloc0A(int_1,8,-1)
      CALL dmalloc0A(int_1,-1,8)
      IF( (.NOT.ALLOCATED(int_1)) .OR. ANY(int_1 /= 0) .OR. &
          (UBOUND(int_1,1) /= 8) .OR. (LBOUND(int_1,1) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(int_1,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(int_1,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_1,-1,1)
      IF( (.NOT.ALLOCATED(int_1)) .OR. ANY(int_1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_1,1) /= 8) .OR. (LBOUND(int_1,1) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(int_1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(int_1,-1,1) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      CALL demallocA(int_1)
!
! rank 2 variable
      CALL dmallocA(int_2,-10,10)
      CALL dmallocA(int_2,10,-10)
      CALL dmallocA(int_2,10,10)
      IF((.NOT.ALLOCATED(int_2)) .OR. ANY(int_2 /= 0) .OR. &
          (UBOUND(int_2,1) /= 10) .OR. (LBOUND(int_2,1) /= 1) .OR. &
          (UBOUND(int_2,2) /= 10) .OR. (LBOUND(int_2,2) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(int_2,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(int_2,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(int_2,100,100)
      IF((.NOT.ALLOCATED(int_2)) .OR. ANY(int_2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_2,1) /= 10) .OR. (LBOUND(int_2,1) /= 1) .OR. &
          (UBOUND(int_2,2) /= 10) .OR. (LBOUND(int_2,2) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(int_2,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(int_2,100,100)'
      ENDIF
      CALL demallocA(int_2)
      IF( ALLOCATED(int_2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(int_2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(int_2)'
      ENDIF
      CALL demallocA(int_2)
      IF( ALLOCATED(int_2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(int_2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(int_2)'
      ENDIF
      CALL dmalloc0A(int_2,8,-1,-1,8)
      CALL dmalloc0A(int_2,-1,8,8,-1)
      CALL dmalloc0A(int_2,-1,8,-1,8)
      IF((.NOT.ALLOCATED(int_2)) .OR. ANY(int_2 /= 0) .OR. &
          (UBOUND(int_2,1) /= 8) .OR. (LBOUND(int_2,1) /= -1) .OR. &
          (UBOUND(int_2,2) /= 8) .OR. (LBOUND(int_2,2) /= -1) ) THEN
        WRITE(*,*) 'CALL CALL dmalloc0A(int_2,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(int_2,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_2,-1,1,-1,1)
      IF((.NOT.ALLOCATED(int_2)) .OR. ANY(int_2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_2,1) /= 8) .OR. (LBOUND(int_2,1) /= -1) .OR. &
          (UBOUND(int_2,2) /= 8) .OR. (LBOUND(int_2,2) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(int_2,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(int_2,-1,1,-1,1)'
      ENDIF
      CALL demallocA(int_2)
!
! rank 3 variable
      CALL dmallocA(int_3,-10,10,10)
      CALL dmallocA(int_3,10,-10,10)
      CALL dmallocA(int_3,10,10,-10)
      CALL dmallocA(int_3,10,10,10)
      IF((.NOT.ALLOCATED(int_3)) .OR. ANY(int_3 /= 0) .OR. &
          (UBOUND(int_3,1) /= 10) .OR. (LBOUND(int_3,1) /= 1) .OR. &
          (UBOUND(int_3,2) /= 10) .OR. (LBOUND(int_3,2) /= 1) .OR. &
          (UBOUND(int_3,3) /= 10) .OR. (LBOUND(int_3,3) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(int_3,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(int_3,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(int_3,100,100,100)
      IF((.NOT.ALLOCATED(int_3)) .OR. ANY(int_3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_3,1) /= 10) .OR. (LBOUND(int_3,1) /= 1) .OR. &
          (UBOUND(int_3,2) /= 10) .OR. (LBOUND(int_3,2) /= 1) .OR. &
          (UBOUND(int_3,3) /= 10) .OR. (LBOUND(int_3,3) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(int_3,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(int_3,100,100,100)'
      ENDIF
      CALL demallocA(int_3)
      IF( ALLOCATED(int_3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(int_3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(int_3)'
      ENDIF
      CALL demallocA(int_3)
      IF( ALLOCATED(int_3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(int_3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(int_3)'
      ENDIF
      CALL dmalloc0A(int_3,8,-1,-1,8,-1,8)
      CALL dmalloc0A(int_3,-1,8,8,-1,-1,8)
      CALL dmalloc0A(int_3,-1,8,-1,8,8,-1)
      CALL dmalloc0A(int_3,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(int_3)) .OR. ANY(int_3 /= 0) .OR. &
          (UBOUND(int_3,1) /= 8) .OR. (LBOUND(int_3,1) /= -1) .OR. &
          (UBOUND(int_3,2) /= 8) .OR. (LBOUND(int_3,2) /= -1) .OR. &
          (UBOUND(int_3,3) /= 8) .OR. (LBOUND(int_3,3) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(int_3,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(int_3,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_3,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(int_3)) .OR. ANY(int_3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_3,1) /= 8) .OR. (LBOUND(int_3,1) /= -1) .OR. &
          (UBOUND(int_3,2) /= 8) .OR. (LBOUND(int_3,2) /= -1) .OR. &
          (UBOUND(int_3,3) /= 8) .OR. (LBOUND(int_3,3) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(int_3,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(int_3,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(int_3)
!
! rank 4 variable
      CALL dmallocA(int_4,-10,10,10,10)
      CALL dmallocA(int_4,10,-10,10,10)
      CALL dmallocA(int_4,10,10,-10,10)
      CALL dmallocA(int_4,10,10,10,-10)
      CALL dmallocA(int_4,10,10,10,10)
      IF((.NOT.ALLOCATED(int_4)) .OR. ANY(int_4 /= 0) .OR. &
          (UBOUND(int_4,1) /= 10) .OR. (LBOUND(int_4,1) /= 1) .OR. &
          (UBOUND(int_4,2) /= 10) .OR. (LBOUND(int_4,2) /= 1) .OR. &
          (UBOUND(int_4,3) /= 10) .OR. (LBOUND(int_4,3) /= 1) .OR. &
          (UBOUND(int_4,4) /= 10) .OR. (LBOUND(int_4,4) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(int_4,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(int_4,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(int_4,100,100,100,100)
      IF((.NOT.ALLOCATED(int_4)) .OR. ANY(int_4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_4,1) /= 10) .OR. (LBOUND(int_4,1) /= 1) .OR. &
          (UBOUND(int_4,2) /= 10) .OR. (LBOUND(int_4,2) /= 1) .OR. &
          (UBOUND(int_4,3) /= 10) .OR. (LBOUND(int_4,3) /= 1) .OR. &
          (UBOUND(int_4,4) /= 10) .OR. (LBOUND(int_4,4) /= 1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmallocA(int_4,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA((int_4,100,100,100,100)'
      ENDIF
      CALL demallocA(int_4)
      IF( ALLOCATED(int_4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(int_4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(int_4)'
      ENDIF
      CALL demallocA(int_4)
      IF( ALLOCATED(int_4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(int_4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(int_4)'
      ENDIF
      CALL dmalloc0A(int_4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(int_4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(int_4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(int_4,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(int_4)) .OR. ANY(int_4 /= 0) .OR. &
          (UBOUND(int_4,1) /= 8) .OR. (LBOUND(int_4,1) /= -1) .OR. &
          (UBOUND(int_4,2) /= 8) .OR. (LBOUND(int_4,2) /= -1) .OR. &
          (UBOUND(int_4,3) /= 8) .OR. (LBOUND(int_4,3) /= -1) .OR. &
          (UBOUND(int_4,4) /= 8) .OR. (LBOUND(int_4,4) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(int_4,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(int_4,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_4,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(int_4)) .OR. ANY(int_4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_4,1) /= 8) .OR. (LBOUND(int_4,1) /= -1) .OR. &
          (UBOUND(int_4,2) /= 8) .OR. (LBOUND(int_4,2) /= -1) .OR. &
          (UBOUND(int_4,3) /= 8) .OR. (LBOUND(int_4,3) /= -1) .OR. &
          (UBOUND(int_4,4) /= 8) .OR. (LBOUND(int_4,4) /= -1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmalloc0A(int_4,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(int_4,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(int_4)
!
! rank 5 variable
      CALL dmallocA(int_5,-10,10,10,10,10)
      CALL dmallocA(int_5,10,-10,10,10,10)
      CALL dmallocA(int_5,10,10,-10,10,10)
      CALL dmallocA(int_5,10,10,10,-10,10)
      CALL dmallocA(int_5,10,10,10,10,-10)
      CALL dmallocA(int_5,10,10,10,10,10)
      IF((.NOT.ALLOCATED(int_5)) .OR. ANY(int_5 /= 0) .OR. &
          (UBOUND(int_5,1) /= 10) .OR. (LBOUND(int_5,1) /= 1) .OR. &
          (UBOUND(int_5,2) /= 10) .OR. (LBOUND(int_5,2) /= 1) .OR. &
          (UBOUND(int_5,3) /= 10) .OR. (LBOUND(int_5,3) /= 1) .OR. &
          (UBOUND(int_5,4) /= 10) .OR. (LBOUND(int_5,4) /= 1) .OR. &
          (UBOUND(int_5,5) /= 10) .OR. (LBOUND(int_5,5) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(int_5,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(int_5,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(int_5,100,100,100,100,100)
      IF((.NOT.ALLOCATED(int_5)) .OR. ANY(int_5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_5,1) /= 10) .OR. (LBOUND(int_5,1) /= 1) .OR. &
          (UBOUND(int_5,2) /= 10) .OR. (LBOUND(int_5,2) /= 1) .OR. &
          (UBOUND(int_5,3) /= 10) .OR. (LBOUND(int_5,3) /= 1) .OR. &
          (UBOUND(int_5,4) /= 10) .OR. (LBOUND(int_5,4) /= 1) .OR. &
          (UBOUND(int_5,5) /= 10) .OR. (LBOUND(int_5,5) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(int_5,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(int_5,100,100,100,100,100)'
      ENDIF
      CALL demallocA(int_5)
      IF( ALLOCATED(int_5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(int_5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(int_5)'
      ENDIF
      CALL demallocA(int_5)
      IF( ALLOCATED(int_5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(int_5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(int_5)'
      ENDIF
      CALL dmalloc0A(int_5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(int_5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(int_5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(int_5,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(int_5)) .OR. ANY(int_5 /= 0) .OR. &
          (UBOUND(int_5,1) /= 8) .OR. (LBOUND(int_5,1) /= -1) .OR. &
          (UBOUND(int_5,2) /= 8) .OR. (LBOUND(int_5,2) /= -1) .OR. &
          (UBOUND(int_5,3) /= 8) .OR. (LBOUND(int_5,3) /= -1) .OR. &
          (UBOUND(int_5,4) /= 8) .OR. (LBOUND(int_5,4) /= -1) .OR. &
          (UBOUND(int_5,5) /= 8) .OR. (LBOUND(int_5,5) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocA(int_5,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(int_5,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_5,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(int_5)) .OR. ANY(int_5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_5,1) /= 8) .OR. (LBOUND(int_5,1) /= -1) .OR. &
          (UBOUND(int_5,2) /= 8) .OR. (LBOUND(int_5,2) /= -1) .OR. &
          (UBOUND(int_5,3) /= 8) .OR. (LBOUND(int_5,3) /= -1) .OR. &
          (UBOUND(int_5,4) /= 8) .OR. (LBOUND(int_5,4) /= -1) .OR. &
          (UBOUND(int_5,5) /= 8) .OR. (LBOUND(int_5,5) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0A(int_5,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(int_5,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(int_5)
!
! rank 6 variable
      CALL dmallocA(int_6,-10,10,10,10,10,10)
      CALL dmallocA(int_6,10,-10,10,10,10,10)
      CALL dmallocA(int_6,10,10,-10,10,10,10)
      CALL dmallocA(int_6,10,10,10,-10,10,10)
      CALL dmallocA(int_6,10,10,10,10,-10,10)
      CALL dmallocA(int_6,10,10,10,10,10,-10)
      CALL dmallocA(int_6,10,10,10,10,10,10)
      IF((.NOT.ALLOCATED(int_6)) .OR. ANY(int_6 /= 0) .OR. &
          (UBOUND(int_6,1) /= 10) .OR. (LBOUND(int_6,1) /= 1) .OR. &
          (UBOUND(int_6,2) /= 10) .OR. (LBOUND(int_6,2) /= 1) .OR. &
          (UBOUND(int_6,3) /= 10) .OR. (LBOUND(int_6,3) /= 1) .OR. &
          (UBOUND(int_6,4) /= 10) .OR. (LBOUND(int_6,4) /= 1) .OR. &
          (UBOUND(int_6,5) /= 10) .OR. (LBOUND(int_6,5) /= 1) .OR. &
          (UBOUND(int_6,6) /= 10) .OR. (LBOUND(int_6,6) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(int_6,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(int_6,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(int_6,100,100,100,100,100,100)
      IF((.NOT.ALLOCATED(int_6)) .OR. ANY(int_6 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_6,1) /= 10) .OR. (LBOUND(int_6,1) /= 1) .OR. &
          (UBOUND(int_6,2) /= 10) .OR. (LBOUND(int_6,2) /= 1) .OR. &
          (UBOUND(int_6,3) /= 10) .OR. (LBOUND(int_6,3) /= 1) .OR. &
          (UBOUND(int_6,4) /= 10) .OR. (LBOUND(int_6,4) /= 1) .OR. &
          (UBOUND(int_6,5) /= 10) .OR. (LBOUND(int_6,5) /= 1) .OR. &
          (UBOUND(int_6,6) /= 10) .OR. (LBOUND(int_6,6) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(int_6,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(int_6,100,100,100,100,100,100)'
      ENDIF
      CALL demallocA(int_6)
      IF( ALLOCATED(int_6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(int_6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(int_6)'
      ENDIF
      CALL demallocA(int_6)
      IF( ALLOCATED(int_6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(int_6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(int_6)'
      ENDIF
      CALL dmalloc0A(int_6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(int_6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(int_6)) .OR. ANY(int_6 /= 0) .OR. &
          (UBOUND(int_6,1) /= 8) .OR. (LBOUND(int_6,1) /= -1) .OR. &
          (UBOUND(int_6,2) /= 8) .OR. (LBOUND(int_6,2) /= -1) .OR. &
          (UBOUND(int_6,3) /= 8) .OR. (LBOUND(int_6,3) /= -1) .OR. &
          (UBOUND(int_6,4) /= 8) .OR. (LBOUND(int_6,4) /= -1) .OR. &
          (UBOUND(int_6,5) /= 8) .OR. (LBOUND(int_6,5) /= -1) .OR. &
          (UBOUND(int_6,6) /= 8) .OR. (LBOUND(int_6,6) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocA(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(int_6)) .OR. ANY(int_6 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_6,1) /= 8) .OR. (LBOUND(int_6,1) /= -1) .OR. &
          (UBOUND(int_6,2) /= 8) .OR. (LBOUND(int_6,2) /= -1) .OR. &
          (UBOUND(int_6,3) /= 8) .OR. (LBOUND(int_6,3) /= -1) .OR. &
          (UBOUND(int_6,4) /= 8) .OR. (LBOUND(int_6,4) /= -1) .OR. &
          (UBOUND(int_6,5) /= 8) .OR. (LBOUND(int_6,5) /= -1) .OR. &
          (UBOUND(int_6,6) /= 8) .OR. (LBOUND(int_6,6) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0A(int_6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(int_6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(int_6)
!
! rank 7 variable
      CALL dmallocA(int_7,-10,10,10,10,10,10,10)
      CALL dmallocA(int_7,10,-10,10,10,10,10,10)
      CALL dmallocA(int_7,10,10,-10,10,10,10,10)
      CALL dmallocA(int_7,10,10,10,-10,10,10,10)
      CALL dmallocA(int_7,10,10,10,10,-10,10,10)
      CALL dmallocA(int_7,10,10,10,10,10,-10,10)
      CALL dmallocA(int_7,10,10,10,10,10,10,-10)
      CALL dmallocA(int_7,10,10,10,10,10,10,10)
      IF((.NOT.ALLOCATED(int_7)) .OR. ANY(int_7 /= 0) .OR. &
          (UBOUND(int_7,1) /= 10) .OR. (LBOUND(int_7,1) /= 1) .OR. &
          (UBOUND(int_7,2) /= 10) .OR. (LBOUND(int_7,2) /= 1) .OR. &
          (UBOUND(int_7,3) /= 10) .OR. (LBOUND(int_7,3) /= 1) .OR. &
          (UBOUND(int_7,4) /= 10) .OR. (LBOUND(int_7,4) /= 1) .OR. &
          (UBOUND(int_7,5) /= 10) .OR. (LBOUND(int_7,5) /= 1) .OR. &
          (UBOUND(int_7,6) /= 10) .OR. (LBOUND(int_7,6) /= 1) .OR. &
          (UBOUND(int_7,7) /= 10) .OR. (LBOUND(int_7,7) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(int_7,10,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(int_7,10,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(int_7,100,100,100,100,100,100,100)
      IF((.NOT.ALLOCATED(int_7)) .OR. ANY(int_7 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_7,1) /= 10) .OR. (LBOUND(int_7,1) /= 1) .OR. &
          (UBOUND(int_7,2) /= 10) .OR. (LBOUND(int_7,2) /= 1) .OR. &
          (UBOUND(int_7,3) /= 10) .OR. (LBOUND(int_7,3) /= 1) .OR. &
          (UBOUND(int_7,4) /= 10) .OR. (LBOUND(int_7,4) /= 1) .OR. &
          (UBOUND(int_7,5) /= 10) .OR. (LBOUND(int_7,5) /= 1) .OR. &
          (UBOUND(int_7,6) /= 10) .OR. (LBOUND(int_7,6) /= 1) .OR. &
          (UBOUND(int_7,7) /= 10) .OR. (LBOUND(int_7,7) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(int_7,100,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(int_7,100,100,100,100,100,100,100)'
      ENDIF
      CALL demallocA(int_7)
      IF( ALLOCATED(int_7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(int_7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(int_7)'
      ENDIF
      CALL demallocA(int_7)
      IF( ALLOCATED(int_7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(int_7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(int_7)'
      ENDIF
      CALL dmalloc0A(int_7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(int_7)) .OR. ANY(int_7 /= 0) .OR. &
          (UBOUND(int_7,1) /= 8) .OR. (LBOUND(int_7,1) /= -1) .OR. &
          (UBOUND(int_7,2) /= 8) .OR. (LBOUND(int_7,2) /= -1) .OR. &
          (UBOUND(int_7,3) /= 8) .OR. (LBOUND(int_7,3) /= -1) .OR. &
          (UBOUND(int_7,4) /= 8) .OR. (LBOUND(int_7,4) /= -1) .OR. &
          (UBOUND(int_7,5) /= 8) .OR. (LBOUND(int_7,5) /= -1) .OR. &
          (UBOUND(int_7,6) /= 8) .OR. (LBOUND(int_7,6) /= -1) .OR. &
          (UBOUND(int_7,7) /= 8) .OR. (LBOUND(int_7,7) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(int_7)) .OR. ANY(int_7 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_7,1) /= 8) .OR. (LBOUND(int_7,1) /= -1) .OR. &
          (UBOUND(int_7,2) /= 8) .OR. (LBOUND(int_7,2) /= -1) .OR. &
          (UBOUND(int_7,3) /= 8) .OR. (LBOUND(int_7,3) /= -1) .OR. &
          (UBOUND(int_7,4) /= 8) .OR. (LBOUND(int_7,4) /= -1) .OR. &
          (UBOUND(int_7,5) /= 8) .OR. (LBOUND(int_7,5) /= -1) .OR. &
          (UBOUND(int_7,6) /= 8) .OR. (LBOUND(int_7,6) /= -1) .OR. &
          (UBOUND(int_7,7) /= 8) .OR. (LBOUND(int_7,7) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(int_7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(int_7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(int_7)
      WRITE(*,*) '---------------------------------------------------'
    ENDSUBROUTINE testINTA
!
!===============================================================================
! Test allocation/deallocation for integers
    SUBROUTINE testLONGINTP
      USE IntrType
      USE Allocs
      IMPLICIT NONE
    
      INTEGER(SLK),POINTER :: lint1(:)
      INTEGER(SLK),POINTER :: lint2(:,:)
      INTEGER(SLK),POINTER :: lint3(:,:,:)
      INTEGER(SLK),POINTER :: lint4(:,:,:,:)
      INTEGER(SLK),POINTER :: lint5(:,:,:,:,:)
      INTEGER(SLK),POINTER :: lint6(:,:,:,:,:,:)
      INTEGER(SLK),POINTER :: lint7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0
      
      NULLIFY(lint1,lint2,lint3,lint4,lint5,lint6,lint7)
      
      WRITE(*,*) 'TESTING ALLOCS FOR LONG INTEGER TYPES'
!
! rank 1 variable
      CALL dmallocP(lint1,-10)
      CALL dmallocP(lint1,10)
      IF( (.NOT.ASSOCIATED(lint1)) .OR. ANY(lint1 /= 0) .OR. &
          (UBOUND(lint1,1) /= 10) .OR. (LBOUND(lint1,1) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(lint1,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(lint1,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(lint1,100)
      IF( (.NOT.ASSOCIATED(lint1)) .OR. ANY(lint1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint1,1) /= 10) .OR. (LBOUND(lint1,1) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(lint1,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(lint1,100)'
      ENDIF
      CALL demallocP(lint1)
      IF( ASSOCIATED(lint1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(lint1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(lint1)'
      ENDIF
      CALL demallocP(lint1)
      IF( ASSOCIATED(lint1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(lint1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(lint1)'
      ENDIF
      CALL dmalloc0P(lint1,8,-1)
      CALL dmalloc0P(lint1,-1,8)
      IF( (.NOT.ASSOCIATED(lint1)) .OR. ANY(lint1 /= 0) .OR. &
          (UBOUND(lint1,1) /= 8) .OR. (LBOUND(lint1,1) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(lint1,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(lint1,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint1,-1,1)
      IF( (.NOT.ASSOCIATED(lint1)) .OR. ANY(lint1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint1,1) /= 8) .OR. (LBOUND(lint1,1) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(lint1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(lint1,-1,1) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      CALL demallocP(lint1)
!
! rank 2 variable
      NULLIFY(lint2)
      CALL dmallocP(lint2,-10,10)
      CALL dmallocP(lint2,10,-10)
      CALL dmallocP(lint2,10,10)
      IF((.NOT.ASSOCIATED(lint2)) .OR. ANY(lint2 /= 0) .OR. &
          (UBOUND(lint2,1) /= 10) .OR. (LBOUND(lint2,1) /= 1) .OR. &
          (UBOUND(lint2,2) /= 10) .OR. (LBOUND(lint2,2) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(lint2,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(lint2,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(lint2,100,100)
      IF((.NOT.ASSOCIATED(lint2)) .OR. ANY(lint2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint2,1) /= 10) .OR. (LBOUND(lint2,1) /= 1) .OR. &
          (UBOUND(lint2,2) /= 10) .OR. (LBOUND(lint2,2) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(lint2,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(lint2,100,100)'
      ENDIF
      CALL demallocP(lint2)
      IF( ASSOCIATED(lint2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(lint2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(lint2)'
      ENDIF
      CALL demallocP(lint2)
      IF( ASSOCIATED(lint2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(lint2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(lint2)'
      ENDIF
      CALL dmalloc0P(lint2,8,-1,-1,8)
      CALL dmalloc0P(lint2,-1,8,8,-1)
      CALL dmalloc0P(lint2,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(lint2)) .OR. ANY(lint2 /= 0) .OR. &
          (UBOUND(lint2,1) /= 8) .OR. (LBOUND(lint2,1) /= -1) .OR. &
          (UBOUND(lint2,2) /= 8) .OR. (LBOUND(lint2,2) /= -1) ) THEN
        WRITE(*,*) 'CALL CALL dmalloc0P(lint2,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(lint2,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint2,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(lint2)) .OR. ANY(lint2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint2,1) /= 8) .OR. (LBOUND(lint2,1) /= -1) .OR. &
          (UBOUND(lint2,2) /= 8) .OR. (LBOUND(lint2,2) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(lint2,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(lint2,-1,1,-1,1)'
      ENDIF
      CALL demallocP(lint2)
!
! rank 3 variable
      NULLIFY(lint3)
      CALL dmallocP(lint3,-10,10,10)
      CALL dmallocP(lint3,10,-10,10)
      CALL dmallocP(lint3,10,10,-10)
      CALL dmallocP(lint3,10,10,10)
      IF((.NOT.ASSOCIATED(lint3)) .OR. ANY(lint3 /= 0) .OR. &
          (UBOUND(lint3,1) /= 10) .OR. (LBOUND(lint3,1) /= 1) .OR. &
          (UBOUND(lint3,2) /= 10) .OR. (LBOUND(lint3,2) /= 1) .OR. &
          (UBOUND(lint3,3) /= 10) .OR. (LBOUND(lint3,3) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(lint3,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(lint3,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(lint3,100,100,100)
      IF((.NOT.ASSOCIATED(lint3)) .OR. ANY(lint3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint3,1) /= 10) .OR. (LBOUND(lint3,1) /= 1) .OR. &
          (UBOUND(lint3,2) /= 10) .OR. (LBOUND(lint3,2) /= 1) .OR. &
          (UBOUND(lint3,3) /= 10) .OR. (LBOUND(lint3,3) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(lint3,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(lint3,100,100,100)'
      ENDIF
      CALL demallocP(lint3)
      IF( ASSOCIATED(lint3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(lint3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(lint3)'
      ENDIF
      CALL demallocP(lint3)
      IF( ASSOCIATED(lint3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(lint3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(lint3)'
      ENDIF
      CALL dmalloc0P(lint3,8,-1,-1,8,-1,8)
      CALL dmalloc0P(lint3,-1,8,8,-1,-1,8)
      CALL dmalloc0P(lint3,-1,8,-1,8,8,-1)
      CALL dmalloc0P(lint3,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(lint3)) .OR. ANY(lint3 /= 0) .OR. &
          (UBOUND(lint3,1) /= 8) .OR. (LBOUND(lint3,1) /= -1) .OR. &
          (UBOUND(lint3,2) /= 8) .OR. (LBOUND(lint3,2) /= -1) .OR. &
          (UBOUND(lint3,3) /= 8) .OR. (LBOUND(lint3,3) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(lint3,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(lint3,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint3,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(lint3)) .OR. ANY(lint3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint3,1) /= 8) .OR. (LBOUND(lint3,1) /= -1) .OR. &
          (UBOUND(lint3,2) /= 8) .OR. (LBOUND(lint3,2) /= -1) .OR. &
          (UBOUND(lint3,3) /= 8) .OR. (LBOUND(lint3,3) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(lint3,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(lint3,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(lint3)
!
! rank 4 variable
      NULLIFY(lint4)
      CALL dmallocP(lint4,-10,10,10,10)
      CALL dmallocP(lint4,10,-10,10,10)
      CALL dmallocP(lint4,10,10,-10,10)
      CALL dmallocP(lint4,10,10,10,-10)
      CALL dmallocP(lint4,10,10,10,10)
      IF((.NOT.ASSOCIATED(lint4)) .OR. ANY(lint4 /= 0) .OR. &
          (UBOUND(lint4,1) /= 10) .OR. (LBOUND(lint4,1) /= 1) .OR. &
          (UBOUND(lint4,2) /= 10) .OR. (LBOUND(lint4,2) /= 1) .OR. &
          (UBOUND(lint4,3) /= 10) .OR. (LBOUND(lint4,3) /= 1) .OR. &
          (UBOUND(lint4,4) /= 10) .OR. (LBOUND(lint4,4) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(lint4,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(lint4,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(lint4,100,100,100,100)
      IF((.NOT.ASSOCIATED(lint4)) .OR. ANY(lint4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint4,1) /= 10) .OR. (LBOUND(lint4,1) /= 1) .OR. &
          (UBOUND(lint4,2) /= 10) .OR. (LBOUND(lint4,2) /= 1) .OR. &
          (UBOUND(lint4,3) /= 10) .OR. (LBOUND(lint4,3) /= 1) .OR. &
          (UBOUND(lint4,4) /= 10) .OR. (LBOUND(lint4,4) /= 1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmallocP(lint4,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP((lint4,100,100,100,100)'
      ENDIF
      CALL demallocP(lint4)
      IF( ASSOCIATED(lint4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(lint4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(lint4)'
      ENDIF
      CALL demallocP(lint4)
      IF( ASSOCIATED(lint4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(lint4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(lint4)'
      ENDIF
      CALL dmalloc0P(lint4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(lint4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(lint4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(lint4,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(lint4)) .OR. ANY(lint4 /= 0) .OR. &
          (UBOUND(lint4,1) /= 8) .OR. (LBOUND(lint4,1) /= -1) .OR. &
          (UBOUND(lint4,2) /= 8) .OR. (LBOUND(lint4,2) /= -1) .OR. &
          (UBOUND(lint4,3) /= 8) .OR. (LBOUND(lint4,3) /= -1) .OR. &
          (UBOUND(lint4,4) /= 8) .OR. (LBOUND(lint4,4) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(lint4,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(lint4,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint4,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(lint4)) .OR. ANY(lint4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint4,1) /= 8) .OR. (LBOUND(lint4,1) /= -1) .OR. &
          (UBOUND(lint4,2) /= 8) .OR. (LBOUND(lint4,2) /= -1) .OR. &
          (UBOUND(lint4,3) /= 8) .OR. (LBOUND(lint4,3) /= -1) .OR. &
          (UBOUND(lint4,4) /= 8) .OR. (LBOUND(lint4,4) /= -1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmalloc0P(lint4,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(lint4,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(lint4)
!
! rank 5 variable
      NULLIFY(lint5)
      CALL dmallocP(lint5,-10,10,10,10,10)
      CALL dmallocP(lint5,10,-10,10,10,10)
      CALL dmallocP(lint5,10,10,-10,10,10)
      CALL dmallocP(lint5,10,10,10,-10,10)
      CALL dmallocP(lint5,10,10,10,10,-10)
      CALL dmallocP(lint5,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(lint5)) .OR. ANY(lint5 /= 0) .OR. &
          (UBOUND(lint5,1) /= 10) .OR. (LBOUND(lint5,1) /= 1) .OR. &
          (UBOUND(lint5,2) /= 10) .OR. (LBOUND(lint5,2) /= 1) .OR. &
          (UBOUND(lint5,3) /= 10) .OR. (LBOUND(lint5,3) /= 1) .OR. &
          (UBOUND(lint5,4) /= 10) .OR. (LBOUND(lint5,4) /= 1) .OR. &
          (UBOUND(lint5,5) /= 10) .OR. (LBOUND(lint5,5) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(lint5,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(lint5,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(lint5,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(lint5)) .OR. ANY(lint5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint5,1) /= 10) .OR. (LBOUND(lint5,1) /= 1) .OR. &
          (UBOUND(lint5,2) /= 10) .OR. (LBOUND(lint5,2) /= 1) .OR. &
          (UBOUND(lint5,3) /= 10) .OR. (LBOUND(lint5,3) /= 1) .OR. &
          (UBOUND(lint5,4) /= 10) .OR. (LBOUND(lint5,4) /= 1) .OR. &
          (UBOUND(lint5,5) /= 10) .OR. (LBOUND(lint5,5) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(lint5,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(lint5,100,100,100,100,100)'
      ENDIF
      CALL demallocP(lint5)
      IF( ASSOCIATED(lint5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(lint5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(lint5)'
      ENDIF
      CALL demallocP(lint5)
      IF( ASSOCIATED(lint5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(lint5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(lint5)'
      ENDIF
      CALL dmalloc0P(lint5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(lint5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(lint5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(lint5,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(lint5)) .OR. ANY(lint5 /= 0) .OR. &
          (UBOUND(lint5,1) /= 8) .OR. (LBOUND(lint5,1) /= -1) .OR. &
          (UBOUND(lint5,2) /= 8) .OR. (LBOUND(lint5,2) /= -1) .OR. &
          (UBOUND(lint5,3) /= 8) .OR. (LBOUND(lint5,3) /= -1) .OR. &
          (UBOUND(lint5,4) /= 8) .OR. (LBOUND(lint5,4) /= -1) .OR. &
          (UBOUND(lint5,5) /= 8) .OR. (LBOUND(lint5,5) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocP(lint5,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(lint5,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint5,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(lint5)) .OR. ANY(lint5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint5,1) /= 8) .OR. (LBOUND(lint5,1) /= -1) .OR. &
          (UBOUND(lint5,2) /= 8) .OR. (LBOUND(lint5,2) /= -1) .OR. &
          (UBOUND(lint5,3) /= 8) .OR. (LBOUND(lint5,3) /= -1) .OR. &
          (UBOUND(lint5,4) /= 8) .OR. (LBOUND(lint5,4) /= -1) .OR. &
          (UBOUND(lint5,5) /= 8) .OR. (LBOUND(lint5,5) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0P(lint5,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(lint5,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(lint5)
!
! rank 6 variable
      NULLIFY(lint6)
      CALL dmallocP(lint6,-10,10,10,10,10,10)
      CALL dmallocP(lint6,10,-10,10,10,10,10)
      CALL dmallocP(lint6,10,10,-10,10,10,10)
      CALL dmallocP(lint6,10,10,10,-10,10,10)
      CALL dmallocP(lint6,10,10,10,10,-10,10)
      CALL dmallocP(lint6,10,10,10,10,10,-10)
      CALL dmallocP(lint6,10,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(lint6)) .OR. ANY(lint6 /= 0) .OR. &
          (UBOUND(lint6,1) /= 10) .OR. (LBOUND(lint6,1) /= 1) .OR. &
          (UBOUND(lint6,2) /= 10) .OR. (LBOUND(lint6,2) /= 1) .OR. &
          (UBOUND(lint6,3) /= 10) .OR. (LBOUND(lint6,3) /= 1) .OR. &
          (UBOUND(lint6,4) /= 10) .OR. (LBOUND(lint6,4) /= 1) .OR. &
          (UBOUND(lint6,5) /= 10) .OR. (LBOUND(lint6,5) /= 1) .OR. &
          (UBOUND(lint6,6) /= 10) .OR. (LBOUND(lint6,6) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(lint6,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(lint6,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(lint6,100,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(lint6)) .OR. ANY(lint6 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint6,1) /= 10) .OR. (LBOUND(lint6,1) /= 1) .OR. &
          (UBOUND(lint6,2) /= 10) .OR. (LBOUND(lint6,2) /= 1) .OR. &
          (UBOUND(lint6,3) /= 10) .OR. (LBOUND(lint6,3) /= 1) .OR. &
          (UBOUND(lint6,4) /= 10) .OR. (LBOUND(lint6,4) /= 1) .OR. &
          (UBOUND(lint6,5) /= 10) .OR. (LBOUND(lint6,5) /= 1) .OR. &
          (UBOUND(lint6,6) /= 10) .OR. (LBOUND(lint6,6) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(lint6,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(lint6,100,100,100,100,100,100)'
      ENDIF
      CALL demallocP(lint6)
      IF( ASSOCIATED(lint6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(lint6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(lint6)'
      ENDIF
      CALL demallocP(lint6)
      IF( ASSOCIATED(lint6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(lint6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(lint6)'
      ENDIF
      CALL dmalloc0P(lint6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(lint6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(lint6)) .OR. ANY(lint6 /= 0) .OR. &
          (UBOUND(lint6,1) /= 8) .OR. (LBOUND(lint6,1) /= -1) .OR. &
          (UBOUND(lint6,2) /= 8) .OR. (LBOUND(lint6,2) /= -1) .OR. &
          (UBOUND(lint6,3) /= 8) .OR. (LBOUND(lint6,3) /= -1) .OR. &
          (UBOUND(lint6,4) /= 8) .OR. (LBOUND(lint6,4) /= -1) .OR. &
          (UBOUND(lint6,5) /= 8) .OR. (LBOUND(lint6,5) /= -1) .OR. &
          (UBOUND(lint6,6) /= 8) .OR. (LBOUND(lint6,6) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocP(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(lint6)) .OR. ANY(lint6 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint6,1) /= 8) .OR. (LBOUND(lint6,1) /= -1) .OR. &
          (UBOUND(lint6,2) /= 8) .OR. (LBOUND(lint6,2) /= -1) .OR. &
          (UBOUND(lint6,3) /= 8) .OR. (LBOUND(lint6,3) /= -1) .OR. &
          (UBOUND(lint6,4) /= 8) .OR. (LBOUND(lint6,4) /= -1) .OR. &
          (UBOUND(lint6,5) /= 8) .OR. (LBOUND(lint6,5) /= -1) .OR. &
          (UBOUND(lint6,6) /= 8) .OR. (LBOUND(lint6,6) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0P(lint6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(lint6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(lint6)
!
! rank 7 variable
      NULLIFY(lint7)
      CALL dmallocP(lint7,-10,10,10,10,10,10,10)
      CALL dmallocP(lint7,10,-10,10,10,10,10,10)
      CALL dmallocP(lint7,10,10,-10,10,10,10,10)
      CALL dmallocP(lint7,10,10,10,-10,10,10,10)
      CALL dmallocP(lint7,10,10,10,10,-10,10,10)
      CALL dmallocP(lint7,10,10,10,10,10,-10,10)
      CALL dmallocP(lint7,10,10,10,10,10,10,-10)
      CALL dmallocP(lint7,10,10,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(lint7)) .OR. ANY(lint7 /= 0) .OR. &
          (UBOUND(lint7,1) /= 10) .OR. (LBOUND(lint7,1) /= 1) .OR. &
          (UBOUND(lint7,2) /= 10) .OR. (LBOUND(lint7,2) /= 1) .OR. &
          (UBOUND(lint7,3) /= 10) .OR. (LBOUND(lint7,3) /= 1) .OR. &
          (UBOUND(lint7,4) /= 10) .OR. (LBOUND(lint7,4) /= 1) .OR. &
          (UBOUND(lint7,5) /= 10) .OR. (LBOUND(lint7,5) /= 1) .OR. &
          (UBOUND(lint7,6) /= 10) .OR. (LBOUND(lint7,6) /= 1) .OR. &
          (UBOUND(lint7,7) /= 10) .OR. (LBOUND(lint7,7) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(lint7,10,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(lint7,10,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(lint7,100,100,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(lint7)) .OR. ANY(lint7 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint7,1) /= 10) .OR. (LBOUND(lint7,1) /= 1) .OR. &
          (UBOUND(lint7,2) /= 10) .OR. (LBOUND(lint7,2) /= 1) .OR. &
          (UBOUND(lint7,3) /= 10) .OR. (LBOUND(lint7,3) /= 1) .OR. &
          (UBOUND(lint7,4) /= 10) .OR. (LBOUND(lint7,4) /= 1) .OR. &
          (UBOUND(lint7,5) /= 10) .OR. (LBOUND(lint7,5) /= 1) .OR. &
          (UBOUND(lint7,6) /= 10) .OR. (LBOUND(lint7,6) /= 1) .OR. &
          (UBOUND(lint7,7) /= 10) .OR. (LBOUND(lint7,7) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(lint7,100,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(lint7,100,100,100,100,100,100,100)'
      ENDIF
      CALL demallocP(lint7)
      IF( ASSOCIATED(lint7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(lint7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(lint7)'
      ENDIF
      CALL demallocP(lint7)
      IF( ASSOCIATED(lint7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(lint7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(lint7)'
      ENDIF
      CALL dmalloc0P(lint7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(lint7)) .OR. ANY(lint7 /= 0) .OR. &
          (UBOUND(lint7,1) /= 8) .OR. (LBOUND(lint7,1) /= -1) .OR. &
          (UBOUND(lint7,2) /= 8) .OR. (LBOUND(lint7,2) /= -1) .OR. &
          (UBOUND(lint7,3) /= 8) .OR. (LBOUND(lint7,3) /= -1) .OR. &
          (UBOUND(lint7,4) /= 8) .OR. (LBOUND(lint7,4) /= -1) .OR. &
          (UBOUND(lint7,5) /= 8) .OR. (LBOUND(lint7,5) /= -1) .OR. &
          (UBOUND(lint7,6) /= 8) .OR. (LBOUND(lint7,6) /= -1) .OR. &
          (UBOUND(lint7,7) /= 8) .OR. (LBOUND(lint7,7) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(lint7)) .OR. ANY(lint7 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint7,1) /= 8) .OR. (LBOUND(lint7,1) /= -1) .OR. &
          (UBOUND(lint7,2) /= 8) .OR. (LBOUND(lint7,2) /= -1) .OR. &
          (UBOUND(lint7,3) /= 8) .OR. (LBOUND(lint7,3) /= -1) .OR. &
          (UBOUND(lint7,4) /= 8) .OR. (LBOUND(lint7,4) /= -1) .OR. &
          (UBOUND(lint7,5) /= 8) .OR. (LBOUND(lint7,5) /= -1) .OR. &
          (UBOUND(lint7,6) /= 8) .OR. (LBOUND(lint7,6) /= -1) .OR. &
          (UBOUND(lint7,7) /= 8) .OR. (LBOUND(lint7,7) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(lint7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(lint7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(lint7)
      WRITE(*,*) '---------------------------------------------------'
    ENDSUBROUTINE testLONGINTP
!
!===============================================================================
! Test allocation/deallocation for integers
    SUBROUTINE testLONGINTA
      USE IntrType
      USE Allocs
      IMPLICIT NONE
    
      INTEGER(SLK),ALLOCATABLE :: lint1(:)
      INTEGER(SLK),ALLOCATABLE :: lint2(:,:)
      INTEGER(SLK),ALLOCATABLE :: lint3(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: lint4(:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: lint5(:,:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: lint6(:,:,:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: lint7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0

      WRITE(*,*) 'TESTING ALLOCS FOR LONG INTEGER TYPES'
!
! rank 1 variable
      CALL dmallocA(lint1,-10)
      CALL dmallocA(lint1,10)
      IF( (.NOT.ALLOCATED(lint1)) .OR. ANY(lint1 /= 0) .OR. &
          (UBOUND(lint1,1) /= 10) .OR. (LBOUND(lint1,1) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(lint1,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(lint1,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(lint1,100)
      IF( (.NOT.ALLOCATED(lint1)) .OR. ANY(lint1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint1,1) /= 10) .OR. (LBOUND(lint1,1) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(lint1,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(lint1,100)'
      ENDIF
      CALL demallocA(lint1)
      IF( ALLOCATED(lint1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(lint1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(lint1)'
      ENDIF
      CALL demallocA(lint1)
      IF( ALLOCATED(lint1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(lint1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(lint1)'
      ENDIF
      CALL dmalloc0A(lint1,8,-1)
      CALL dmalloc0A(lint1,-1,8)
      IF( (.NOT.ALLOCATED(lint1)) .OR. ANY(lint1 /= 0) .OR. &
          (UBOUND(lint1,1) /= 8) .OR. (LBOUND(lint1,1) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(lint1,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(lint1,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint1,-1,1)
      IF( (.NOT.ALLOCATED(lint1)) .OR. ANY(lint1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint1,1) /= 8) .OR. (LBOUND(lint1,1) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(lint1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(lint1,-1,1) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      CALL demallocA(lint1)
!
! rank 2 variable
      CALL dmallocA(lint2,-10,10)
      CALL dmallocA(lint2,10,-10)
      CALL dmallocA(lint2,10,10)
      IF((.NOT.ALLOCATED(lint2)) .OR. ANY(lint2 /= 0) .OR. &
          (UBOUND(lint2,1) /= 10) .OR. (LBOUND(lint2,1) /= 1) .OR. &
          (UBOUND(lint2,2) /= 10) .OR. (LBOUND(lint2,2) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(lint2,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(lint2,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(lint2,100,100)
      IF((.NOT.ALLOCATED(lint2)) .OR. ANY(lint2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint2,1) /= 10) .OR. (LBOUND(lint2,1) /= 1) .OR. &
          (UBOUND(lint2,2) /= 10) .OR. (LBOUND(lint2,2) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(lint2,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(lint2,100,100)'
      ENDIF
      CALL demallocA(lint2)
      IF( ALLOCATED(lint2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(lint2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(lint2)'
      ENDIF
      CALL demallocA(lint2)
      IF( ALLOCATED(lint2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(lint2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(lint2)'
      ENDIF
      CALL dmalloc0A(lint2,8,-1,-1,8)
      CALL dmalloc0A(lint2,-1,8,8,-1)
      CALL dmalloc0A(lint2,-1,8,-1,8)
      IF((.NOT.ALLOCATED(lint2)) .OR. ANY(lint2 /= 0) .OR. &
          (UBOUND(lint2,1) /= 8) .OR. (LBOUND(lint2,1) /= -1) .OR. &
          (UBOUND(lint2,2) /= 8) .OR. (LBOUND(lint2,2) /= -1) ) THEN
        WRITE(*,*) 'CALL CALL dmalloc0A(lint2,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(lint2,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint2,-1,1,-1,1)
      IF((.NOT.ALLOCATED(lint2)) .OR. ANY(lint2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint2,1) /= 8) .OR. (LBOUND(lint2,1) /= -1) .OR. &
          (UBOUND(lint2,2) /= 8) .OR. (LBOUND(lint2,2) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(lint2,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(lint2,-1,1,-1,1)'
      ENDIF
      CALL demallocA(lint2)
!
! rank 3 variable
      CALL dmallocA(lint3,-10,10,10)
      CALL dmallocA(lint3,10,-10,10)
      CALL dmallocA(lint3,10,10,-10)
      CALL dmallocA(lint3,10,10,10)
      IF((.NOT.ALLOCATED(lint3)) .OR. ANY(lint3 /= 0) .OR. &
          (UBOUND(lint3,1) /= 10) .OR. (LBOUND(lint3,1) /= 1) .OR. &
          (UBOUND(lint3,2) /= 10) .OR. (LBOUND(lint3,2) /= 1) .OR. &
          (UBOUND(lint3,3) /= 10) .OR. (LBOUND(lint3,3) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(lint3,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(lint3,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(lint3,100,100,100)
      IF((.NOT.ALLOCATED(lint3)) .OR. ANY(lint3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint3,1) /= 10) .OR. (LBOUND(lint3,1) /= 1) .OR. &
          (UBOUND(lint3,2) /= 10) .OR. (LBOUND(lint3,2) /= 1) .OR. &
          (UBOUND(lint3,3) /= 10) .OR. (LBOUND(lint3,3) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(lint3,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(lint3,100,100,100)'
      ENDIF
      CALL demallocA(lint3)
      IF( ALLOCATED(lint3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(lint3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(lint3)'
      ENDIF
      CALL demallocA(lint3)
      IF( ALLOCATED(lint3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(lint3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(lint3)'
      ENDIF
      CALL dmalloc0A(lint3,8,-1,-1,8,-1,8)
      CALL dmalloc0A(lint3,-1,8,8,-1,-1,8)
      CALL dmalloc0A(lint3,-1,8,-1,8,8,-1)
      CALL dmalloc0A(lint3,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(lint3)) .OR. ANY(lint3 /= 0) .OR. &
          (UBOUND(lint3,1) /= 8) .OR. (LBOUND(lint3,1) /= -1) .OR. &
          (UBOUND(lint3,2) /= 8) .OR. (LBOUND(lint3,2) /= -1) .OR. &
          (UBOUND(lint3,3) /= 8) .OR. (LBOUND(lint3,3) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(lint3,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(lint3,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint3,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(lint3)) .OR. ANY(lint3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint3,1) /= 8) .OR. (LBOUND(lint3,1) /= -1) .OR. &
          (UBOUND(lint3,2) /= 8) .OR. (LBOUND(lint3,2) /= -1) .OR. &
          (UBOUND(lint3,3) /= 8) .OR. (LBOUND(lint3,3) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(lint3,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(lint3,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(lint3)
!
! rank 4 variable
      CALL dmallocA(lint4,-10,10,10,10)
      CALL dmallocA(lint4,10,-10,10,10)
      CALL dmallocA(lint4,10,10,-10,10)
      CALL dmallocA(lint4,10,10,10,-10)
      CALL dmallocA(lint4,10,10,10,10)
      IF((.NOT.ALLOCATED(lint4)) .OR. ANY(lint4 /= 0) .OR. &
          (UBOUND(lint4,1) /= 10) .OR. (LBOUND(lint4,1) /= 1) .OR. &
          (UBOUND(lint4,2) /= 10) .OR. (LBOUND(lint4,2) /= 1) .OR. &
          (UBOUND(lint4,3) /= 10) .OR. (LBOUND(lint4,3) /= 1) .OR. &
          (UBOUND(lint4,4) /= 10) .OR. (LBOUND(lint4,4) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(lint4,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(lint4,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(lint4,100,100,100,100)
      IF((.NOT.ALLOCATED(lint4)) .OR. ANY(lint4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint4,1) /= 10) .OR. (LBOUND(lint4,1) /= 1) .OR. &
          (UBOUND(lint4,2) /= 10) .OR. (LBOUND(lint4,2) /= 1) .OR. &
          (UBOUND(lint4,3) /= 10) .OR. (LBOUND(lint4,3) /= 1) .OR. &
          (UBOUND(lint4,4) /= 10) .OR. (LBOUND(lint4,4) /= 1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmallocA(lint4,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA((lint4,100,100,100,100)'
      ENDIF
      CALL demallocA(lint4)
      IF( ALLOCATED(lint4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(lint4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(lint4)'
      ENDIF
      CALL demallocA(lint4)
      IF( ALLOCATED(lint4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(lint4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(lint4)'
      ENDIF
      CALL dmalloc0A(lint4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(lint4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(lint4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(lint4,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(lint4)) .OR. ANY(lint4 /= 0) .OR. &
          (UBOUND(lint4,1) /= 8) .OR. (LBOUND(lint4,1) /= -1) .OR. &
          (UBOUND(lint4,2) /= 8) .OR. (LBOUND(lint4,2) /= -1) .OR. &
          (UBOUND(lint4,3) /= 8) .OR. (LBOUND(lint4,3) /= -1) .OR. &
          (UBOUND(lint4,4) /= 8) .OR. (LBOUND(lint4,4) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(lint4,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(lint4,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint4,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(lint4)) .OR. ANY(lint4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint4,1) /= 8) .OR. (LBOUND(lint4,1) /= -1) .OR. &
          (UBOUND(lint4,2) /= 8) .OR. (LBOUND(lint4,2) /= -1) .OR. &
          (UBOUND(lint4,3) /= 8) .OR. (LBOUND(lint4,3) /= -1) .OR. &
          (UBOUND(lint4,4) /= 8) .OR. (LBOUND(lint4,4) /= -1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmalloc0A(lint4,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(lint4,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(lint4)
!
! rank 5 variable
      CALL dmallocA(lint5,-10,10,10,10,10)
      CALL dmallocA(lint5,10,-10,10,10,10)
      CALL dmallocA(lint5,10,10,-10,10,10)
      CALL dmallocA(lint5,10,10,10,-10,10)
      CALL dmallocA(lint5,10,10,10,10,-10)
      CALL dmallocA(lint5,10,10,10,10,10)
      IF((.NOT.ALLOCATED(lint5)) .OR. ANY(lint5 /= 0) .OR. &
          (UBOUND(lint5,1) /= 10) .OR. (LBOUND(lint5,1) /= 1) .OR. &
          (UBOUND(lint5,2) /= 10) .OR. (LBOUND(lint5,2) /= 1) .OR. &
          (UBOUND(lint5,3) /= 10) .OR. (LBOUND(lint5,3) /= 1) .OR. &
          (UBOUND(lint5,4) /= 10) .OR. (LBOUND(lint5,4) /= 1) .OR. &
          (UBOUND(lint5,5) /= 10) .OR. (LBOUND(lint5,5) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(lint5,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(lint5,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(lint5,100,100,100,100,100)
      IF((.NOT.ALLOCATED(lint5)) .OR. ANY(lint5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint5,1) /= 10) .OR. (LBOUND(lint5,1) /= 1) .OR. &
          (UBOUND(lint5,2) /= 10) .OR. (LBOUND(lint5,2) /= 1) .OR. &
          (UBOUND(lint5,3) /= 10) .OR. (LBOUND(lint5,3) /= 1) .OR. &
          (UBOUND(lint5,4) /= 10) .OR. (LBOUND(lint5,4) /= 1) .OR. &
          (UBOUND(lint5,5) /= 10) .OR. (LBOUND(lint5,5) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(lint5,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(lint5,100,100,100,100,100)'
      ENDIF
      CALL demallocA(lint5)
      IF( ALLOCATED(lint5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(lint5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(lint5)'
      ENDIF
      CALL demallocA(lint5)
      IF( ALLOCATED(lint5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(lint5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(lint5)'
      ENDIF
      CALL dmalloc0A(lint5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(lint5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(lint5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(lint5,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(lint5)) .OR. ANY(lint5 /= 0) .OR. &
          (UBOUND(lint5,1) /= 8) .OR. (LBOUND(lint5,1) /= -1) .OR. &
          (UBOUND(lint5,2) /= 8) .OR. (LBOUND(lint5,2) /= -1) .OR. &
          (UBOUND(lint5,3) /= 8) .OR. (LBOUND(lint5,3) /= -1) .OR. &
          (UBOUND(lint5,4) /= 8) .OR. (LBOUND(lint5,4) /= -1) .OR. &
          (UBOUND(lint5,5) /= 8) .OR. (LBOUND(lint5,5) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocA(lint5,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(lint5,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint5,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(lint5)) .OR. ANY(lint5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint5,1) /= 8) .OR. (LBOUND(lint5,1) /= -1) .OR. &
          (UBOUND(lint5,2) /= 8) .OR. (LBOUND(lint5,2) /= -1) .OR. &
          (UBOUND(lint5,3) /= 8) .OR. (LBOUND(lint5,3) /= -1) .OR. &
          (UBOUND(lint5,4) /= 8) .OR. (LBOUND(lint5,4) /= -1) .OR. &
          (UBOUND(lint5,5) /= 8) .OR. (LBOUND(lint5,5) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0A(lint5,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(lint5,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(lint5)
!
! rank 6 variable
      CALL dmallocA(lint6,-10,10,10,10,10,10)
      CALL dmallocA(lint6,10,-10,10,10,10,10)
      CALL dmallocA(lint6,10,10,-10,10,10,10)
      CALL dmallocA(lint6,10,10,10,-10,10,10)
      CALL dmallocA(lint6,10,10,10,10,-10,10)
      CALL dmallocA(lint6,10,10,10,10,10,-10)
      CALL dmallocA(lint6,10,10,10,10,10,10)
      IF((.NOT.ALLOCATED(lint6)) .OR. ANY(lint6 /= 0) .OR. &
          (UBOUND(lint6,1) /= 10) .OR. (LBOUND(lint6,1) /= 1) .OR. &
          (UBOUND(lint6,2) /= 10) .OR. (LBOUND(lint6,2) /= 1) .OR. &
          (UBOUND(lint6,3) /= 10) .OR. (LBOUND(lint6,3) /= 1) .OR. &
          (UBOUND(lint6,4) /= 10) .OR. (LBOUND(lint6,4) /= 1) .OR. &
          (UBOUND(lint6,5) /= 10) .OR. (LBOUND(lint6,5) /= 1) .OR. &
          (UBOUND(lint6,6) /= 10) .OR. (LBOUND(lint6,6) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(lint6,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(lint6,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(lint6,100,100,100,100,100,100)
      IF((.NOT.ALLOCATED(lint6)) .OR. ANY(lint6 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint6,1) /= 10) .OR. (LBOUND(lint6,1) /= 1) .OR. &
          (UBOUND(lint6,2) /= 10) .OR. (LBOUND(lint6,2) /= 1) .OR. &
          (UBOUND(lint6,3) /= 10) .OR. (LBOUND(lint6,3) /= 1) .OR. &
          (UBOUND(lint6,4) /= 10) .OR. (LBOUND(lint6,4) /= 1) .OR. &
          (UBOUND(lint6,5) /= 10) .OR. (LBOUND(lint6,5) /= 1) .OR. &
          (UBOUND(lint6,6) /= 10) .OR. (LBOUND(lint6,6) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(lint6,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(lint6,100,100,100,100,100,100)'
      ENDIF
      CALL demallocA(lint6)
      IF( ALLOCATED(lint6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(lint6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(lint6)'
      ENDIF
      CALL demallocA(lint6)
      IF( ALLOCATED(lint6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(lint6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(lint6)'
      ENDIF
      CALL dmalloc0A(lint6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(lint6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(lint6)) .OR. ANY(lint6 /= 0) .OR. &
          (UBOUND(lint6,1) /= 8) .OR. (LBOUND(lint6,1) /= -1) .OR. &
          (UBOUND(lint6,2) /= 8) .OR. (LBOUND(lint6,2) /= -1) .OR. &
          (UBOUND(lint6,3) /= 8) .OR. (LBOUND(lint6,3) /= -1) .OR. &
          (UBOUND(lint6,4) /= 8) .OR. (LBOUND(lint6,4) /= -1) .OR. &
          (UBOUND(lint6,5) /= 8) .OR. (LBOUND(lint6,5) /= -1) .OR. &
          (UBOUND(lint6,6) /= 8) .OR. (LBOUND(lint6,6) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocA(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(lint6)) .OR. ANY(lint6 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint6,1) /= 8) .OR. (LBOUND(lint6,1) /= -1) .OR. &
          (UBOUND(lint6,2) /= 8) .OR. (LBOUND(lint6,2) /= -1) .OR. &
          (UBOUND(lint6,3) /= 8) .OR. (LBOUND(lint6,3) /= -1) .OR. &
          (UBOUND(lint6,4) /= 8) .OR. (LBOUND(lint6,4) /= -1) .OR. &
          (UBOUND(lint6,5) /= 8) .OR. (LBOUND(lint6,5) /= -1) .OR. &
          (UBOUND(lint6,6) /= 8) .OR. (LBOUND(lint6,6) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0A(lint6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(lint6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(lint6)
!
! rank 7 variable
      CALL dmallocA(lint7,-10,10,10,10,10,10,10)
      CALL dmallocA(lint7,10,-10,10,10,10,10,10)
      CALL dmallocA(lint7,10,10,-10,10,10,10,10)
      CALL dmallocA(lint7,10,10,10,-10,10,10,10)
      CALL dmallocA(lint7,10,10,10,10,-10,10,10)
      CALL dmallocA(lint7,10,10,10,10,10,-10,10)
      CALL dmallocA(lint7,10,10,10,10,10,10,-10)
      CALL dmallocA(lint7,10,10,10,10,10,10,10)
      IF((.NOT.ALLOCATED(lint7)) .OR. ANY(lint7 /= 0) .OR. &
          (UBOUND(lint7,1) /= 10) .OR. (LBOUND(lint7,1) /= 1) .OR. &
          (UBOUND(lint7,2) /= 10) .OR. (LBOUND(lint7,2) /= 1) .OR. &
          (UBOUND(lint7,3) /= 10) .OR. (LBOUND(lint7,3) /= 1) .OR. &
          (UBOUND(lint7,4) /= 10) .OR. (LBOUND(lint7,4) /= 1) .OR. &
          (UBOUND(lint7,5) /= 10) .OR. (LBOUND(lint7,5) /= 1) .OR. &
          (UBOUND(lint7,6) /= 10) .OR. (LBOUND(lint7,6) /= 1) .OR. &
          (UBOUND(lint7,7) /= 10) .OR. (LBOUND(lint7,7) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(lint7,10,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(lint7,10,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(lint7,100,100,100,100,100,100,100)
      IF((.NOT.ALLOCATED(lint7)) .OR. ANY(lint7 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint7,1) /= 10) .OR. (LBOUND(lint7,1) /= 1) .OR. &
          (UBOUND(lint7,2) /= 10) .OR. (LBOUND(lint7,2) /= 1) .OR. &
          (UBOUND(lint7,3) /= 10) .OR. (LBOUND(lint7,3) /= 1) .OR. &
          (UBOUND(lint7,4) /= 10) .OR. (LBOUND(lint7,4) /= 1) .OR. &
          (UBOUND(lint7,5) /= 10) .OR. (LBOUND(lint7,5) /= 1) .OR. &
          (UBOUND(lint7,6) /= 10) .OR. (LBOUND(lint7,6) /= 1) .OR. &
          (UBOUND(lint7,7) /= 10) .OR. (LBOUND(lint7,7) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(lint7,100,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(lint7,100,100,100,100,100,100,100)'
      ENDIF
      CALL demallocA(lint7)
      IF( ALLOCATED(lint7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(lint7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(lint7)'
      ENDIF
      CALL demallocA(lint7)
      IF( ALLOCATED(lint7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(lint7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(lint7)'
      ENDIF
      CALL dmalloc0A(lint7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(lint7)) .OR. ANY(lint7 /= 0) .OR. &
          (UBOUND(lint7,1) /= 8) .OR. (LBOUND(lint7,1) /= -1) .OR. &
          (UBOUND(lint7,2) /= 8) .OR. (LBOUND(lint7,2) /= -1) .OR. &
          (UBOUND(lint7,3) /= 8) .OR. (LBOUND(lint7,3) /= -1) .OR. &
          (UBOUND(lint7,4) /= 8) .OR. (LBOUND(lint7,4) /= -1) .OR. &
          (UBOUND(lint7,5) /= 8) .OR. (LBOUND(lint7,5) /= -1) .OR. &
          (UBOUND(lint7,6) /= 8) .OR. (LBOUND(lint7,6) /= -1) .OR. &
          (UBOUND(lint7,7) /= 8) .OR. (LBOUND(lint7,7) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(lint7)) .OR. ANY(lint7 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint7,1) /= 8) .OR. (LBOUND(lint7,1) /= -1) .OR. &
          (UBOUND(lint7,2) /= 8) .OR. (LBOUND(lint7,2) /= -1) .OR. &
          (UBOUND(lint7,3) /= 8) .OR. (LBOUND(lint7,3) /= -1) .OR. &
          (UBOUND(lint7,4) /= 8) .OR. (LBOUND(lint7,4) /= -1) .OR. &
          (UBOUND(lint7,5) /= 8) .OR. (LBOUND(lint7,5) /= -1) .OR. &
          (UBOUND(lint7,6) /= 8) .OR. (LBOUND(lint7,6) /= -1) .OR. &
          (UBOUND(lint7,7) /= 8) .OR. (LBOUND(lint7,7) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(lint7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(lint7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(lint7)
      WRITE(*,*) '---------------------------------------------------'
    ENDSUBROUTINE testLONGINTA
!
!===============================================================================
! Test allocation/deallocation for single precision
    SUBROUTINE testSINGLEP
      USE IntrType
      USE Allocs
      IMPLICIT NONE
    
      REAL(SSK),POINTER :: sgl1(:)
      REAL(SSK),POINTER :: sgl2(:,:)
      REAL(SSK),POINTER :: sgl3(:,:,:)
      REAL(SSK),POINTER :: sgl4(:,:,:,:)
      REAL(SSK),POINTER :: sgl5(:,:,:,:,:)
      REAL(SSK),POINTER :: sgl6(:,:,:,:,:,:)
      REAL(SSK),POINTER :: sgl7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0
      
      NULLIFY(sgl1,sgl2,sgl3,sgl3,sgl4,sgl5,sgl6,sgl7)
      
      WRITE(*,*) 'TESTING ALLOCS FOR SINGLE PRECISION REAL POINTER TYPES'
!
! rank 1 variable
      CALL dmallocP(sgl1,-10)
      CALL dmallocP(sgl1,10)
      IF( (.NOT.ASSOCIATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. &
          (UBOUND(sgl1,1) /= 10) .OR. (LBOUND(sgl1,1) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(sgl1,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(sgl1,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl1,100)
      IF( (.NOT.ASSOCIATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl1,1) /= 10) .OR. (LBOUND(sgl1,1) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(sgl1,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(sgl1,100)'
      ENDIF
      CALL demallocP(sgl1)
      IF( ASSOCIATED(sgl1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(sgl1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(sgl1)'
      ENDIF
      CALL demallocP(sgl1)
      IF( ASSOCIATED(sgl1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(sgl1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(sgl1)'
      ENDIF
      CALL dmalloc0P(sgl1,8,-1)
      CALL dmalloc0P(sgl1,-1,8)
      IF( (.NOT.ASSOCIATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. &
          (UBOUND(sgl1,1) /= 8) .OR. (LBOUND(sgl1,1) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(sgl1,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(sgl1,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl1,-1,1)
      IF( (.NOT.ASSOCIATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl1,1) /= 8) .OR. (LBOUND(sgl1,1) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(sgl1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(sgl1,-1,1) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      CALL demallocP(sgl1)
!
! rank 2 variable
      NULLIFY(sgl2)
      CALL dmallocP(sgl2,-10,10)
      CALL dmallocP(sgl2,10,-10)
      CALL dmallocP(sgl2,10,10)
      IF((.NOT.ASSOCIATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. &
          (UBOUND(sgl2,1) /= 10) .OR. (LBOUND(sgl2,1) /= 1) .OR. &
          (UBOUND(sgl2,2) /= 10) .OR. (LBOUND(sgl2,2) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(sgl2,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(sgl2,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl2,100,100)
      IF((.NOT.ASSOCIATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl2,1) /= 10) .OR. (LBOUND(sgl2,1) /= 1) .OR. &
          (UBOUND(sgl2,2) /= 10) .OR. (LBOUND(sgl2,2) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(sgl2,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(sgl2,100,100)'
      ENDIF
      CALL demallocP(sgl2)
      IF( ASSOCIATED(sgl2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(sgl2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(sgl2)'
      ENDIF
      CALL demallocP(sgl2)
      IF( ASSOCIATED(sgl2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(sgl2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(sgl2)'
      ENDIF
      CALL dmalloc0P(sgl2,8,-1,-1,8)
      CALL dmalloc0P(sgl2,-1,8,8,-1)
      CALL dmalloc0P(sgl2,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. &
          (UBOUND(sgl2,1) /= 8) .OR. (LBOUND(sgl2,1) /= -1) .OR. &
          (UBOUND(sgl2,2) /= 8) .OR. (LBOUND(sgl2,2) /= -1) ) THEN
        WRITE(*,*) 'CALL CALL dmalloc0P(sgl2,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(sgl2,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl2,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl2,1) /= 8) .OR. (LBOUND(sgl2,1) /= -1) .OR. &
          (UBOUND(sgl2,2) /= 8) .OR. (LBOUND(sgl2,2) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(sgl2,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(sgl2,-1,1,-1,1)'
      ENDIF
      CALL demallocP(sgl2)
!
! rank 3 variable
      NULLIFY(sgl3)
      CALL dmallocP(sgl3,-10,10,10)
      CALL dmallocP(sgl3,10,-10,10)
      CALL dmallocP(sgl3,10,10,-10)
      CALL dmallocP(sgl3,10,10,10)
      IF((.NOT.ASSOCIATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. &
          (UBOUND(sgl3,1) /= 10) .OR. (LBOUND(sgl3,1) /= 1) .OR. &
          (UBOUND(sgl3,2) /= 10) .OR. (LBOUND(sgl3,2) /= 1) .OR. &
          (UBOUND(sgl3,3) /= 10) .OR. (LBOUND(sgl3,3) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(sgl3,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(sgl3,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl3,100,100,100)
      IF((.NOT.ASSOCIATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl3,1) /= 10) .OR. (LBOUND(sgl3,1) /= 1) .OR. &
          (UBOUND(sgl3,2) /= 10) .OR. (LBOUND(sgl3,2) /= 1) .OR. &
          (UBOUND(sgl3,3) /= 10) .OR. (LBOUND(sgl3,3) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(sgl3,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(sgl3,100,100,100)'
      ENDIF
      CALL demallocP(sgl3)
      IF( ASSOCIATED(sgl3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(sgl3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(sgl3)'
      ENDIF
      CALL demallocP(sgl3)
      IF( ASSOCIATED(sgl3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(sgl3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(sgl3)'
      ENDIF
      CALL dmalloc0P(sgl3,8,-1,-1,8,-1,8)
      CALL dmalloc0P(sgl3,-1,8,8,-1,-1,8)
      CALL dmalloc0P(sgl3,-1,8,-1,8,8,-1)
      CALL dmalloc0P(sgl3,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. &
          (UBOUND(sgl3,1) /= 8) .OR. (LBOUND(sgl3,1) /= -1) .OR. &
          (UBOUND(sgl3,2) /= 8) .OR. (LBOUND(sgl3,2) /= -1) .OR. &
          (UBOUND(sgl3,3) /= 8) .OR. (LBOUND(sgl3,3) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(sgl3,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(sgl3,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl3,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl3,1) /= 8) .OR. (LBOUND(sgl3,1) /= -1) .OR. &
          (UBOUND(sgl3,2) /= 8) .OR. (LBOUND(sgl3,2) /= -1) .OR. &
          (UBOUND(sgl3,3) /= 8) .OR. (LBOUND(sgl3,3) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(sgl3,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(sgl3,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(sgl3)
!
! rank 4 variable
      NULLIFY(sgl4)
      CALL dmallocP(sgl4,-10,10,10,10)
      CALL dmallocP(sgl4,10,-10,10,10)
      CALL dmallocP(sgl4,10,10,-10,10)
      CALL dmallocP(sgl4,10,10,10,-10)
      CALL dmallocP(sgl4,10,10,10,10)
      IF((.NOT.ASSOCIATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. &
          (UBOUND(sgl4,1) /= 10) .OR. (LBOUND(sgl4,1) /= 1) .OR. &
          (UBOUND(sgl4,2) /= 10) .OR. (LBOUND(sgl4,2) /= 1) .OR. &
          (UBOUND(sgl4,3) /= 10) .OR. (LBOUND(sgl4,3) /= 1) .OR. &
          (UBOUND(sgl4,4) /= 10) .OR. (LBOUND(sgl4,4) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(sgl4,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(sgl4,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl4,100,100,100,100)
      IF((.NOT.ASSOCIATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl4,1) /= 10) .OR. (LBOUND(sgl4,1) /= 1) .OR. &
          (UBOUND(sgl4,2) /= 10) .OR. (LBOUND(sgl4,2) /= 1) .OR. &
          (UBOUND(sgl4,3) /= 10) .OR. (LBOUND(sgl4,3) /= 1) .OR. &
          (UBOUND(sgl4,4) /= 10) .OR. (LBOUND(sgl4,4) /= 1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmallocP(sgl4,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP((sgl4,100,100,100,100)'
      ENDIF
      CALL demallocP(sgl4)
      IF( ASSOCIATED(sgl4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(sgl4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(sgl4)'
      ENDIF
      CALL demallocP(sgl4)
      IF( ASSOCIATED(sgl4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(sgl4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(sgl4)'
      ENDIF
      CALL dmalloc0P(sgl4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(sgl4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(sgl4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(sgl4,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. &
          (UBOUND(sgl4,1) /= 8) .OR. (LBOUND(sgl4,1) /= -1) .OR. &
          (UBOUND(sgl4,2) /= 8) .OR. (LBOUND(sgl4,2) /= -1) .OR. &
          (UBOUND(sgl4,3) /= 8) .OR. (LBOUND(sgl4,3) /= -1) .OR. &
          (UBOUND(sgl4,4) /= 8) .OR. (LBOUND(sgl4,4) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(sgl4,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(sgl4,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl4,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl4,1) /= 8) .OR. (LBOUND(sgl4,1) /= -1) .OR. &
          (UBOUND(sgl4,2) /= 8) .OR. (LBOUND(sgl4,2) /= -1) .OR. &
          (UBOUND(sgl4,3) /= 8) .OR. (LBOUND(sgl4,3) /= -1) .OR. &
          (UBOUND(sgl4,4) /= 8) .OR. (LBOUND(sgl4,4) /= -1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmalloc0P(sgl4,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(sgl4,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(sgl4)
!
! rank 5 variable
      NULLIFY(sgl5)
      CALL dmallocP(sgl5,-10,10,10,10,10)
      CALL dmallocP(sgl5,10,-10,10,10,10)
      CALL dmallocP(sgl5,10,10,-10,10,10)
      CALL dmallocP(sgl5,10,10,10,-10,10)
      CALL dmallocP(sgl5,10,10,10,10,-10)
      CALL dmallocP(sgl5,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK) .OR. &
          (UBOUND(sgl5,1) /= 10) .OR. (LBOUND(sgl5,1) /= 1) .OR. &
          (UBOUND(sgl5,2) /= 10) .OR. (LBOUND(sgl5,2) /= 1) .OR. &
          (UBOUND(sgl5,3) /= 10) .OR. (LBOUND(sgl5,3) /= 1) .OR. &
          (UBOUND(sgl5,4) /= 10) .OR. (LBOUND(sgl5,4) /= 1) .OR. &
          (UBOUND(sgl5,5) /= 10) .OR. (LBOUND(sgl5,5) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(sgl5,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(sgl5,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl5,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl5,1) /= 10) .OR. (LBOUND(sgl5,1) /= 1) .OR. &
          (UBOUND(sgl5,2) /= 10) .OR. (LBOUND(sgl5,2) /= 1) .OR. &
          (UBOUND(sgl5,3) /= 10) .OR. (LBOUND(sgl5,3) /= 1) .OR. &
          (UBOUND(sgl5,4) /= 10) .OR. (LBOUND(sgl5,4) /= 1) .OR. &
          (UBOUND(sgl5,5) /= 10) .OR. (LBOUND(sgl5,5) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(sgl5,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(sgl5,100,100,100,100,100)'
      ENDIF
      CALL demallocP(sgl5)
      IF( ASSOCIATED(sgl5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(sgl5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(sgl5)'
      ENDIF
      CALL demallocP(sgl5)
      IF( ASSOCIATED(sgl5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(sgl5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(sgl5)'
      ENDIF
      CALL dmalloc0P(sgl5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(sgl5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(sgl5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(sgl5,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK) .OR. &
          (UBOUND(sgl5,1) /= 8) .OR. (LBOUND(sgl5,1) /= -1) .OR. &
          (UBOUND(sgl5,2) /= 8) .OR. (LBOUND(sgl5,2) /= -1) .OR. &
          (UBOUND(sgl5,3) /= 8) .OR. (LBOUND(sgl5,3) /= -1) .OR. &
          (UBOUND(sgl5,4) /= 8) .OR. (LBOUND(sgl5,4) /= -1) .OR. &
          (UBOUND(sgl5,5) /= 8) .OR. (LBOUND(sgl5,5) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocP(sgl5,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(sgl5,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl5,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl5,1) /= 8) .OR. (LBOUND(sgl5,1) /= -1) .OR. &
          (UBOUND(sgl5,2) /= 8) .OR. (LBOUND(sgl5,2) /= -1) .OR. &
          (UBOUND(sgl5,3) /= 8) .OR. (LBOUND(sgl5,3) /= -1) .OR. &
          (UBOUND(sgl5,4) /= 8) .OR. (LBOUND(sgl5,4) /= -1) .OR. &
          (UBOUND(sgl5,5) /= 8) .OR. (LBOUND(sgl5,5) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0P(sgl5,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(sgl5,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(sgl5)
!
! rank 6 variable
      NULLIFY(sgl6)
      CALL dmallocP(sgl6,-10,10,10,10,10,10)
      CALL dmallocP(sgl6,10,-10,10,10,10,10)
      CALL dmallocP(sgl6,10,10,-10,10,10,10)
      CALL dmallocP(sgl6,10,10,10,-10,10,10)
      CALL dmallocP(sgl6,10,10,10,10,-10,10)
      CALL dmallocP(sgl6,10,10,10,10,10,-10)
      CALL dmallocP(sgl6,10,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK) .OR. &
          (UBOUND(sgl6,1) /= 10) .OR. (LBOUND(sgl6,1) /= 1) .OR. &
          (UBOUND(sgl6,2) /= 10) .OR. (LBOUND(sgl6,2) /= 1) .OR. &
          (UBOUND(sgl6,3) /= 10) .OR. (LBOUND(sgl6,3) /= 1) .OR. &
          (UBOUND(sgl6,4) /= 10) .OR. (LBOUND(sgl6,4) /= 1) .OR. &
          (UBOUND(sgl6,5) /= 10) .OR. (LBOUND(sgl6,5) /= 1) .OR. &
          (UBOUND(sgl6,6) /= 10) .OR. (LBOUND(sgl6,6) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(sgl6,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(sgl6,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl6,100,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl6,1) /= 10) .OR. (LBOUND(sgl6,1) /= 1) .OR. &
          (UBOUND(sgl6,2) /= 10) .OR. (LBOUND(sgl6,2) /= 1) .OR. &
          (UBOUND(sgl6,3) /= 10) .OR. (LBOUND(sgl6,3) /= 1) .OR. &
          (UBOUND(sgl6,4) /= 10) .OR. (LBOUND(sgl6,4) /= 1) .OR. &
          (UBOUND(sgl6,5) /= 10) .OR. (LBOUND(sgl6,5) /= 1) .OR. &
          (UBOUND(sgl6,6) /= 10) .OR. (LBOUND(sgl6,6) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(sgl6,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(sgl6,100,100,100,100,100,100)'
      ENDIF
      CALL demallocP(sgl6)
      IF( ASSOCIATED(sgl6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(sgl6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(sgl6)'
      ENDIF
      CALL demallocP(sgl6)
      IF( ASSOCIATED(sgl6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(sgl6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(sgl6)'
      ENDIF
      CALL dmalloc0P(sgl6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(sgl6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK) .OR. &
          (UBOUND(sgl6,1) /= 8) .OR. (LBOUND(sgl6,1) /= -1) .OR. &
          (UBOUND(sgl6,2) /= 8) .OR. (LBOUND(sgl6,2) /= -1) .OR. &
          (UBOUND(sgl6,3) /= 8) .OR. (LBOUND(sgl6,3) /= -1) .OR. &
          (UBOUND(sgl6,4) /= 8) .OR. (LBOUND(sgl6,4) /= -1) .OR. &
          (UBOUND(sgl6,5) /= 8) .OR. (LBOUND(sgl6,5) /= -1) .OR. &
          (UBOUND(sgl6,6) /= 8) .OR. (LBOUND(sgl6,6) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocP(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl6,1) /= 8) .OR. (LBOUND(sgl6,1) /= -1) .OR. &
          (UBOUND(sgl6,2) /= 8) .OR. (LBOUND(sgl6,2) /= -1) .OR. &
          (UBOUND(sgl6,3) /= 8) .OR. (LBOUND(sgl6,3) /= -1) .OR. &
          (UBOUND(sgl6,4) /= 8) .OR. (LBOUND(sgl6,4) /= -1) .OR. &
          (UBOUND(sgl6,5) /= 8) .OR. (LBOUND(sgl6,5) /= -1) .OR. &
          (UBOUND(sgl6,6) /= 8) .OR. (LBOUND(sgl6,6) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0P(sgl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(sgl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(sgl6)
!
! rank 7 variable
      NULLIFY(sgl7)
      CALL dmallocP(sgl7,-10,10,10,10,10,10,10)
      CALL dmallocP(sgl7,10,-10,10,10,10,10,10)
      CALL dmallocP(sgl7,10,10,-10,10,10,10,10)
      CALL dmallocP(sgl7,10,10,10,-10,10,10,10)
      CALL dmallocP(sgl7,10,10,10,10,-10,10,10)
      CALL dmallocP(sgl7,10,10,10,10,10,-10,10)
      CALL dmallocP(sgl7,10,10,10,10,10,10,-10)
      CALL dmallocP(sgl7,10,10,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK) .OR. &
          (UBOUND(sgl7,1) /= 10) .OR. (LBOUND(sgl7,1) /= 1) .OR. &
          (UBOUND(sgl7,2) /= 10) .OR. (LBOUND(sgl7,2) /= 1) .OR. &
          (UBOUND(sgl7,3) /= 10) .OR. (LBOUND(sgl7,3) /= 1) .OR. &
          (UBOUND(sgl7,4) /= 10) .OR. (LBOUND(sgl7,4) /= 1) .OR. &
          (UBOUND(sgl7,5) /= 10) .OR. (LBOUND(sgl7,5) /= 1) .OR. &
          (UBOUND(sgl7,6) /= 10) .OR. (LBOUND(sgl7,6) /= 1) .OR. &
          (UBOUND(sgl7,7) /= 10) .OR. (LBOUND(sgl7,7) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(sgl7,10,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(sgl7,10,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl7,100,100,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl7,1) /= 10) .OR. (LBOUND(sgl7,1) /= 1) .OR. &
          (UBOUND(sgl7,2) /= 10) .OR. (LBOUND(sgl7,2) /= 1) .OR. &
          (UBOUND(sgl7,3) /= 10) .OR. (LBOUND(sgl7,3) /= 1) .OR. &
          (UBOUND(sgl7,4) /= 10) .OR. (LBOUND(sgl7,4) /= 1) .OR. &
          (UBOUND(sgl7,5) /= 10) .OR. (LBOUND(sgl7,5) /= 1) .OR. &
          (UBOUND(sgl7,6) /= 10) .OR. (LBOUND(sgl7,6) /= 1) .OR. &
          (UBOUND(sgl7,7) /= 10) .OR. (LBOUND(sgl7,7) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(sgl7,100,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(sgl7,100,100,100,100,100,100,100)'
      ENDIF
      CALL demallocP(sgl7)
      IF( ASSOCIATED(sgl7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(sgl7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(sgl7)'
      ENDIF
      CALL demallocP(sgl7)
      IF( ASSOCIATED(sgl7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(sgl7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(sgl7)'
      ENDIF
      CALL dmalloc0P(sgl7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK) .OR. &
          (UBOUND(sgl7,1) /= 8) .OR. (LBOUND(sgl7,1) /= -1) .OR. &
          (UBOUND(sgl7,2) /= 8) .OR. (LBOUND(sgl7,2) /= -1) .OR. &
          (UBOUND(sgl7,3) /= 8) .OR. (LBOUND(sgl7,3) /= -1) .OR. &
          (UBOUND(sgl7,4) /= 8) .OR. (LBOUND(sgl7,4) /= -1) .OR. &
          (UBOUND(sgl7,5) /= 8) .OR. (LBOUND(sgl7,5) /= -1) .OR. &
          (UBOUND(sgl7,6) /= 8) .OR. (LBOUND(sgl7,6) /= -1) .OR. &
          (UBOUND(sgl7,7) /= 8) .OR. (LBOUND(sgl7,7) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl7,1) /= 8) .OR. (LBOUND(sgl7,1) /= -1) .OR. &
          (UBOUND(sgl7,2) /= 8) .OR. (LBOUND(sgl7,2) /= -1) .OR. &
          (UBOUND(sgl7,3) /= 8) .OR. (LBOUND(sgl7,3) /= -1) .OR. &
          (UBOUND(sgl7,4) /= 8) .OR. (LBOUND(sgl7,4) /= -1) .OR. &
          (UBOUND(sgl7,5) /= 8) .OR. (LBOUND(sgl7,5) /= -1) .OR. &
          (UBOUND(sgl7,6) /= 8) .OR. (LBOUND(sgl7,6) /= -1) .OR. &
          (UBOUND(sgl7,7) /= 8) .OR. (LBOUND(sgl7,7) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(sgl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(sgl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(sgl7)
      WRITE(*,*) '---------------------------------------------------'
    ENDSUBROUTINE testSINGLEP
!
!===============================================================================
! Test allocation/deallocation for single precision
    SUBROUTINE testSINGLEA
      USE IntrType
      USE Allocs
      IMPLICIT NONE
    
      REAL(SSK),ALLOCATABLE :: sgl1(:)
      REAL(SSK),ALLOCATABLE :: sgl2(:,:)
      REAL(SSK),ALLOCATABLE :: sgl3(:,:,:)
      REAL(SSK),ALLOCATABLE :: sgl4(:,:,:,:)
      REAL(SSK),ALLOCATABLE :: sgl5(:,:,:,:,:)
      REAL(SSK),ALLOCATABLE :: sgl6(:,:,:,:,:,:)
      REAL(SSK),ALLOCATABLE :: sgl7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0

      WRITE(*,*) 'TESTING ALLOCS FOR SINGLE PRECISION REAL ALLOCATABLE TYPES'
!
! rank 1 variable
      CALL dmallocA(sgl1,-10)
      CALL dmallocA(sgl1,10)
      IF( (.NOT.ALLOCATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. &
          (UBOUND(sgl1,1) /= 10) .OR. (LBOUND(sgl1,1) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(sgl1,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(sgl1,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl1,100)
      IF( (.NOT.ALLOCATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl1,1) /= 10) .OR. (LBOUND(sgl1,1) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(sgl1,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(sgl1,100)'
      ENDIF
      CALL demallocA(sgl1)
      IF( ALLOCATED(sgl1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(sgl1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(sgl1)'
      ENDIF
      CALL demallocA(sgl1)
      IF( ALLOCATED(sgl1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(sgl1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(sgl1)'
      ENDIF
      CALL dmalloc0A(sgl1,8,-1)
      CALL dmalloc0A(sgl1,-1,8)
      IF( (.NOT.ALLOCATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. &
          (UBOUND(sgl1,1) /= 8) .OR. (LBOUND(sgl1,1) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(sgl1,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(sgl1,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl1,-1,1)
      IF( (.NOT.ALLOCATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl1,1) /= 8) .OR. (LBOUND(sgl1,1) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(sgl1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(sgl1,-1,1) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      CALL demallocA(sgl1)
!
! rank 2 variable
      CALL dmallocA(sgl2,-10,10)
      CALL dmallocA(sgl2,10,-10)
      CALL dmallocA(sgl2,10,10)
      IF((.NOT.ALLOCATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. &
          (UBOUND(sgl2,1) /= 10) .OR. (LBOUND(sgl2,1) /= 1) .OR. &
          (UBOUND(sgl2,2) /= 10) .OR. (LBOUND(sgl2,2) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(sgl2,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(sgl2,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl2,100,100)
      IF((.NOT.ALLOCATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl2,1) /= 10) .OR. (LBOUND(sgl2,1) /= 1) .OR. &
          (UBOUND(sgl2,2) /= 10) .OR. (LBOUND(sgl2,2) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(sgl2,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(sgl2,100,100)'
      ENDIF
      CALL demallocA(sgl2)
      IF( ALLOCATED(sgl2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(sgl2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(sgl2)'
      ENDIF
      CALL demallocA(sgl2)
      IF( ALLOCATED(sgl2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(sgl2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(sgl2)'
      ENDIF
      CALL dmalloc0A(sgl2,8,-1,-1,8)
      CALL dmalloc0A(sgl2,-1,8,8,-1)
      CALL dmalloc0A(sgl2,-1,8,-1,8)
      IF((.NOT.ALLOCATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. &
          (UBOUND(sgl2,1) /= 8) .OR. (LBOUND(sgl2,1) /= -1) .OR. &
          (UBOUND(sgl2,2) /= 8) .OR. (LBOUND(sgl2,2) /= -1) ) THEN
        WRITE(*,*) 'CALL CALL dmalloc0A(sgl2,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(sgl2,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl2,-1,1,-1,1)
      IF((.NOT.ALLOCATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl2,1) /= 8) .OR. (LBOUND(sgl2,1) /= -1) .OR. &
          (UBOUND(sgl2,2) /= 8) .OR. (LBOUND(sgl2,2) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(sgl2,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(sgl2,-1,1,-1,1)'
      ENDIF
      CALL demallocA(sgl2)
!
! rank 3 variable
      CALL dmallocA(sgl3,-10,10,10)
      CALL dmallocA(sgl3,10,-10,10)
      CALL dmallocA(sgl3,10,10,-10)
      CALL dmallocA(sgl3,10,10,10)
      IF((.NOT.ALLOCATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. &
          (UBOUND(sgl3,1) /= 10) .OR. (LBOUND(sgl3,1) /= 1) .OR. &
          (UBOUND(sgl3,2) /= 10) .OR. (LBOUND(sgl3,2) /= 1) .OR. &
          (UBOUND(sgl3,3) /= 10) .OR. (LBOUND(sgl3,3) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(sgl3,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(sgl3,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl3,100,100,100)
      IF((.NOT.ALLOCATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl3,1) /= 10) .OR. (LBOUND(sgl3,1) /= 1) .OR. &
          (UBOUND(sgl3,2) /= 10) .OR. (LBOUND(sgl3,2) /= 1) .OR. &
          (UBOUND(sgl3,3) /= 10) .OR. (LBOUND(sgl3,3) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(sgl3,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(sgl3,100,100,100)'
      ENDIF
      CALL demallocA(sgl3)
      IF( ALLOCATED(sgl3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(sgl3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(sgl3)'
      ENDIF
      CALL demallocA(sgl3)
      IF( ALLOCATED(sgl3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(sgl3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(sgl3)'
      ENDIF
      CALL dmalloc0A(sgl3,8,-1,-1,8,-1,8)
      CALL dmalloc0A(sgl3,-1,8,8,-1,-1,8)
      CALL dmalloc0A(sgl3,-1,8,-1,8,8,-1)
      CALL dmalloc0A(sgl3,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. &
          (UBOUND(sgl3,1) /= 8) .OR. (LBOUND(sgl3,1) /= -1) .OR. &
          (UBOUND(sgl3,2) /= 8) .OR. (LBOUND(sgl3,2) /= -1) .OR. &
          (UBOUND(sgl3,3) /= 8) .OR. (LBOUND(sgl3,3) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(sgl3,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(sgl3,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl3,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl3,1) /= 8) .OR. (LBOUND(sgl3,1) /= -1) .OR. &
          (UBOUND(sgl3,2) /= 8) .OR. (LBOUND(sgl3,2) /= -1) .OR. &
          (UBOUND(sgl3,3) /= 8) .OR. (LBOUND(sgl3,3) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(sgl3,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(sgl3,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(sgl3)
!
! rank 4 variable
      CALL dmallocA(sgl4,-10,10,10,10)
      CALL dmallocA(sgl4,10,-10,10,10)
      CALL dmallocA(sgl4,10,10,-10,10)
      CALL dmallocA(sgl4,10,10,10,-10)
      CALL dmallocA(sgl4,10,10,10,10)
      IF((.NOT.ALLOCATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. &
          (UBOUND(sgl4,1) /= 10) .OR. (LBOUND(sgl4,1) /= 1) .OR. &
          (UBOUND(sgl4,2) /= 10) .OR. (LBOUND(sgl4,2) /= 1) .OR. &
          (UBOUND(sgl4,3) /= 10) .OR. (LBOUND(sgl4,3) /= 1) .OR. &
          (UBOUND(sgl4,4) /= 10) .OR. (LBOUND(sgl4,4) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(sgl4,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(sgl4,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl4,100,100,100,100)
      IF((.NOT.ALLOCATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl4,1) /= 10) .OR. (LBOUND(sgl4,1) /= 1) .OR. &
          (UBOUND(sgl4,2) /= 10) .OR. (LBOUND(sgl4,2) /= 1) .OR. &
          (UBOUND(sgl4,3) /= 10) .OR. (LBOUND(sgl4,3) /= 1) .OR. &
          (UBOUND(sgl4,4) /= 10) .OR. (LBOUND(sgl4,4) /= 1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmallocA(sgl4,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA((sgl4,100,100,100,100)'
      ENDIF
      CALL demallocA(sgl4)
      IF( ALLOCATED(sgl4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(sgl4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(sgl4)'
      ENDIF
      CALL demallocA(sgl4)
      IF( ALLOCATED(sgl4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(sgl4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(sgl4)'
      ENDIF
      CALL dmalloc0A(sgl4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(sgl4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(sgl4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(sgl4,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. &
          (UBOUND(sgl4,1) /= 8) .OR. (LBOUND(sgl4,1) /= -1) .OR. &
          (UBOUND(sgl4,2) /= 8) .OR. (LBOUND(sgl4,2) /= -1) .OR. &
          (UBOUND(sgl4,3) /= 8) .OR. (LBOUND(sgl4,3) /= -1) .OR. &
          (UBOUND(sgl4,4) /= 8) .OR. (LBOUND(sgl4,4) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(sgl4,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(sgl4,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl4,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl4,1) /= 8) .OR. (LBOUND(sgl4,1) /= -1) .OR. &
          (UBOUND(sgl4,2) /= 8) .OR. (LBOUND(sgl4,2) /= -1) .OR. &
          (UBOUND(sgl4,3) /= 8) .OR. (LBOUND(sgl4,3) /= -1) .OR. &
          (UBOUND(sgl4,4) /= 8) .OR. (LBOUND(sgl4,4) /= -1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmalloc0A(sgl4,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(sgl4,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(sgl4)
!
! rank 5 variable
      CALL dmallocA(sgl5,-10,10,10,10,10)
      CALL dmallocA(sgl5,10,-10,10,10,10)
      CALL dmallocA(sgl5,10,10,-10,10,10)
      CALL dmallocA(sgl5,10,10,10,-10,10)
      CALL dmallocA(sgl5,10,10,10,10,-10)
      CALL dmallocA(sgl5,10,10,10,10,10)
      IF((.NOT.ALLOCATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK) .OR. &
          (UBOUND(sgl5,1) /= 10) .OR. (LBOUND(sgl5,1) /= 1) .OR. &
          (UBOUND(sgl5,2) /= 10) .OR. (LBOUND(sgl5,2) /= 1) .OR. &
          (UBOUND(sgl5,3) /= 10) .OR. (LBOUND(sgl5,3) /= 1) .OR. &
          (UBOUND(sgl5,4) /= 10) .OR. (LBOUND(sgl5,4) /= 1) .OR. &
          (UBOUND(sgl5,5) /= 10) .OR. (LBOUND(sgl5,5) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(sgl5,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(sgl5,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl5,100,100,100,100,100)
      IF((.NOT.ALLOCATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl5,1) /= 10) .OR. (LBOUND(sgl5,1) /= 1) .OR. &
          (UBOUND(sgl5,2) /= 10) .OR. (LBOUND(sgl5,2) /= 1) .OR. &
          (UBOUND(sgl5,3) /= 10) .OR. (LBOUND(sgl5,3) /= 1) .OR. &
          (UBOUND(sgl5,4) /= 10) .OR. (LBOUND(sgl5,4) /= 1) .OR. &
          (UBOUND(sgl5,5) /= 10) .OR. (LBOUND(sgl5,5) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(sgl5,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(sgl5,100,100,100,100,100)'
      ENDIF
      CALL demallocA(sgl5)
      IF( ALLOCATED(sgl5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(sgl5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(sgl5)'
      ENDIF
      CALL demallocA(sgl5)
      IF( ALLOCATED(sgl5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(sgl5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(sgl5)'
      ENDIF
      CALL dmalloc0A(sgl5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(sgl5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(sgl5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(sgl5,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK) .OR. &
          (UBOUND(sgl5,1) /= 8) .OR. (LBOUND(sgl5,1) /= -1) .OR. &
          (UBOUND(sgl5,2) /= 8) .OR. (LBOUND(sgl5,2) /= -1) .OR. &
          (UBOUND(sgl5,3) /= 8) .OR. (LBOUND(sgl5,3) /= -1) .OR. &
          (UBOUND(sgl5,4) /= 8) .OR. (LBOUND(sgl5,4) /= -1) .OR. &
          (UBOUND(sgl5,5) /= 8) .OR. (LBOUND(sgl5,5) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocA(sgl5,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(sgl5,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl5,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl5,1) /= 8) .OR. (LBOUND(sgl5,1) /= -1) .OR. &
          (UBOUND(sgl5,2) /= 8) .OR. (LBOUND(sgl5,2) /= -1) .OR. &
          (UBOUND(sgl5,3) /= 8) .OR. (LBOUND(sgl5,3) /= -1) .OR. &
          (UBOUND(sgl5,4) /= 8) .OR. (LBOUND(sgl5,4) /= -1) .OR. &
          (UBOUND(sgl5,5) /= 8) .OR. (LBOUND(sgl5,5) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0A(sgl5,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(sgl5,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(sgl5)
!
! rank 6 variable
      CALL dmallocA(sgl6,-10,10,10,10,10,10)
      CALL dmallocA(sgl6,10,-10,10,10,10,10)
      CALL dmallocA(sgl6,10,10,-10,10,10,10)
      CALL dmallocA(sgl6,10,10,10,-10,10,10)
      CALL dmallocA(sgl6,10,10,10,10,-10,10)
      CALL dmallocA(sgl6,10,10,10,10,10,-10)
      CALL dmallocA(sgl6,10,10,10,10,10,10)
      IF((.NOT.ALLOCATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK) .OR. &
          (UBOUND(sgl6,1) /= 10) .OR. (LBOUND(sgl6,1) /= 1) .OR. &
          (UBOUND(sgl6,2) /= 10) .OR. (LBOUND(sgl6,2) /= 1) .OR. &
          (UBOUND(sgl6,3) /= 10) .OR. (LBOUND(sgl6,3) /= 1) .OR. &
          (UBOUND(sgl6,4) /= 10) .OR. (LBOUND(sgl6,4) /= 1) .OR. &
          (UBOUND(sgl6,5) /= 10) .OR. (LBOUND(sgl6,5) /= 1) .OR. &
          (UBOUND(sgl6,6) /= 10) .OR. (LBOUND(sgl6,6) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(sgl6,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(sgl6,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl6,100,100,100,100,100,100)
      IF((.NOT.ALLOCATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl6,1) /= 10) .OR. (LBOUND(sgl6,1) /= 1) .OR. &
          (UBOUND(sgl6,2) /= 10) .OR. (LBOUND(sgl6,2) /= 1) .OR. &
          (UBOUND(sgl6,3) /= 10) .OR. (LBOUND(sgl6,3) /= 1) .OR. &
          (UBOUND(sgl6,4) /= 10) .OR. (LBOUND(sgl6,4) /= 1) .OR. &
          (UBOUND(sgl6,5) /= 10) .OR. (LBOUND(sgl6,5) /= 1) .OR. &
          (UBOUND(sgl6,6) /= 10) .OR. (LBOUND(sgl6,6) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(sgl6,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(sgl6,100,100,100,100,100,100)'
      ENDIF
      CALL demallocA(sgl6)
      IF( ALLOCATED(sgl6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(sgl6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(sgl6)'
      ENDIF
      CALL demallocA(sgl6)
      IF( ALLOCATED(sgl6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(sgl6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(sgl6)'
      ENDIF
      CALL dmalloc0A(sgl6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(sgl6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK) .OR. &
          (UBOUND(sgl6,1) /= 8) .OR. (LBOUND(sgl6,1) /= -1) .OR. &
          (UBOUND(sgl6,2) /= 8) .OR. (LBOUND(sgl6,2) /= -1) .OR. &
          (UBOUND(sgl6,3) /= 8) .OR. (LBOUND(sgl6,3) /= -1) .OR. &
          (UBOUND(sgl6,4) /= 8) .OR. (LBOUND(sgl6,4) /= -1) .OR. &
          (UBOUND(sgl6,5) /= 8) .OR. (LBOUND(sgl6,5) /= -1) .OR. &
          (UBOUND(sgl6,6) /= 8) .OR. (LBOUND(sgl6,6) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocA(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl6,1) /= 8) .OR. (LBOUND(sgl6,1) /= -1) .OR. &
          (UBOUND(sgl6,2) /= 8) .OR. (LBOUND(sgl6,2) /= -1) .OR. &
          (UBOUND(sgl6,3) /= 8) .OR. (LBOUND(sgl6,3) /= -1) .OR. &
          (UBOUND(sgl6,4) /= 8) .OR. (LBOUND(sgl6,4) /= -1) .OR. &
          (UBOUND(sgl6,5) /= 8) .OR. (LBOUND(sgl6,5) /= -1) .OR. &
          (UBOUND(sgl6,6) /= 8) .OR. (LBOUND(sgl6,6) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0A(sgl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(sgl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(sgl6)
!
! rank 7 variable
      CALL dmallocA(sgl7,-10,10,10,10,10,10,10)
      CALL dmallocA(sgl7,10,-10,10,10,10,10,10)
      CALL dmallocA(sgl7,10,10,-10,10,10,10,10)
      CALL dmallocA(sgl7,10,10,10,-10,10,10,10)
      CALL dmallocA(sgl7,10,10,10,10,-10,10,10)
      CALL dmallocA(sgl7,10,10,10,10,10,-10,10)
      CALL dmallocA(sgl7,10,10,10,10,10,10,-10)
      CALL dmallocA(sgl7,10,10,10,10,10,10,10)
      IF((.NOT.ALLOCATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK) .OR. &
          (UBOUND(sgl7,1) /= 10) .OR. (LBOUND(sgl7,1) /= 1) .OR. &
          (UBOUND(sgl7,2) /= 10) .OR. (LBOUND(sgl7,2) /= 1) .OR. &
          (UBOUND(sgl7,3) /= 10) .OR. (LBOUND(sgl7,3) /= 1) .OR. &
          (UBOUND(sgl7,4) /= 10) .OR. (LBOUND(sgl7,4) /= 1) .OR. &
          (UBOUND(sgl7,5) /= 10) .OR. (LBOUND(sgl7,5) /= 1) .OR. &
          (UBOUND(sgl7,6) /= 10) .OR. (LBOUND(sgl7,6) /= 1) .OR. &
          (UBOUND(sgl7,7) /= 10) .OR. (LBOUND(sgl7,7) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(sgl7,10,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(sgl7,10,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl7,100,100,100,100,100,100,100)
      IF((.NOT.ALLOCATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl7,1) /= 10) .OR. (LBOUND(sgl7,1) /= 1) .OR. &
          (UBOUND(sgl7,2) /= 10) .OR. (LBOUND(sgl7,2) /= 1) .OR. &
          (UBOUND(sgl7,3) /= 10) .OR. (LBOUND(sgl7,3) /= 1) .OR. &
          (UBOUND(sgl7,4) /= 10) .OR. (LBOUND(sgl7,4) /= 1) .OR. &
          (UBOUND(sgl7,5) /= 10) .OR. (LBOUND(sgl7,5) /= 1) .OR. &
          (UBOUND(sgl7,6) /= 10) .OR. (LBOUND(sgl7,6) /= 1) .OR. &
          (UBOUND(sgl7,7) /= 10) .OR. (LBOUND(sgl7,7) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(sgl7,100,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(sgl7,100,100,100,100,100,100,100)'
      ENDIF
      CALL demallocA(sgl7)
      IF( ALLOCATED(sgl7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(sgl7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(sgl7)'
      ENDIF
      CALL demallocA(sgl7)
      IF( ALLOCATED(sgl7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(sgl7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(sgl7)'
      ENDIF
      CALL dmalloc0A(sgl7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK) .OR. &
          (UBOUND(sgl7,1) /= 8) .OR. (LBOUND(sgl7,1) /= -1) .OR. &
          (UBOUND(sgl7,2) /= 8) .OR. (LBOUND(sgl7,2) /= -1) .OR. &
          (UBOUND(sgl7,3) /= 8) .OR. (LBOUND(sgl7,3) /= -1) .OR. &
          (UBOUND(sgl7,4) /= 8) .OR. (LBOUND(sgl7,4) /= -1) .OR. &
          (UBOUND(sgl7,5) /= 8) .OR. (LBOUND(sgl7,5) /= -1) .OR. &
          (UBOUND(sgl7,6) /= 8) .OR. (LBOUND(sgl7,6) /= -1) .OR. &
          (UBOUND(sgl7,7) /= 8) .OR. (LBOUND(sgl7,7) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl7,1) /= 8) .OR. (LBOUND(sgl7,1) /= -1) .OR. &
          (UBOUND(sgl7,2) /= 8) .OR. (LBOUND(sgl7,2) /= -1) .OR. &
          (UBOUND(sgl7,3) /= 8) .OR. (LBOUND(sgl7,3) /= -1) .OR. &
          (UBOUND(sgl7,4) /= 8) .OR. (LBOUND(sgl7,4) /= -1) .OR. &
          (UBOUND(sgl7,5) /= 8) .OR. (LBOUND(sgl7,5) /= -1) .OR. &
          (UBOUND(sgl7,6) /= 8) .OR. (LBOUND(sgl7,6) /= -1) .OR. &
          (UBOUND(sgl7,7) /= 8) .OR. (LBOUND(sgl7,7) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(sgl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(sgl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(sgl7)
      WRITE(*,*) '---------------------------------------------------'
    ENDSUBROUTINE testSINGLEA
!
!===============================================================================
! Test allocation/deallocation for single precision
    SUBROUTINE testDOUBLEP
      USE IntrType
      USE Allocs
      IMPLICIT NONE
    
      REAL(SDK),POINTER :: dbl1(:)
      REAL(SDK),POINTER :: dbl2(:,:)
      REAL(SDK),POINTER :: dbl3(:,:,:)
      REAL(SDK),POINTER :: dbl4(:,:,:,:)
      REAL(SDK),POINTER :: dbl5(:,:,:,:,:)
      REAL(SDK),POINTER :: dbl6(:,:,:,:,:,:)
      REAL(SDK),POINTER :: dbl7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0
      
      NULLIFY(dbl1,dbl2,dbl3,dbl4,dbl5,dbl6,dbl7)
      
      WRITE(*,*) 'TESTING ALLOCS FOR DOUBLE PRECISION REAL POINTER TYPES'
!
! rank 1 variable
      CALL dmallocP(dbl1,-10)
      CALL dmallocP(dbl1,10)
      IF( (.NOT.ASSOCIATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. &
          (UBOUND(dbl1,1) /= 10) .OR. (LBOUND(dbl1,1) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(dbl1,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(dbl1,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl1,100)
      IF( (.NOT.ASSOCIATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl1,1) /= 10) .OR. (LBOUND(dbl1,1) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(dbl1,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(dbl1,100)'
      ENDIF
      CALL demallocP(dbl1)
      IF( ASSOCIATED(dbl1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(dbl1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(dbl1)'
      ENDIF
      CALL demallocP(dbl1)
      IF( ASSOCIATED(dbl1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(dbl1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(dbl1)'
      ENDIF
      CALL dmalloc0P(dbl1,8,-1)
      CALL dmalloc0P(dbl1,-1,8)
      IF( (.NOT.ASSOCIATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. &
          (UBOUND(dbl1,1) /= 8) .OR. (LBOUND(dbl1,1) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(dbl1,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(dbl1,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl1,-1,1)
      IF( (.NOT.ASSOCIATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl1,1) /= 8) .OR. (LBOUND(dbl1,1) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(dbl1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(dbl1,-1,1) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      CALL demallocP(dbl1)
!
! rank 2 variable
      NULLIFY(dbl2)
      CALL dmallocP(dbl2,-10,10)
      CALL dmallocP(dbl2,10,-10)
      CALL dmallocP(dbl2,10,10)
      IF((.NOT.ASSOCIATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. &
          (UBOUND(dbl2,1) /= 10) .OR. (LBOUND(dbl2,1) /= 1) .OR. &
          (UBOUND(dbl2,2) /= 10) .OR. (LBOUND(dbl2,2) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(dbl2,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(dbl2,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl2,100,100)
      IF((.NOT.ASSOCIATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl2,1) /= 10) .OR. (LBOUND(dbl2,1) /= 1) .OR. &
          (UBOUND(dbl2,2) /= 10) .OR. (LBOUND(dbl2,2) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(dbl2,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(dbl2,100,100)'
      ENDIF
      CALL demallocP(dbl2)
      IF( ASSOCIATED(dbl2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(dbl2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(dbl2)'
      ENDIF
      CALL demallocP(dbl2)
      IF( ASSOCIATED(dbl2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(dbl2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(dbl2)'
      ENDIF
      CALL dmalloc0P(dbl2,8,-1,-1,8)
      CALL dmalloc0P(dbl2,-1,8,8,-1)
      CALL dmalloc0P(dbl2,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. &
          (UBOUND(dbl2,1) /= 8) .OR. (LBOUND(dbl2,1) /= -1) .OR. &
          (UBOUND(dbl2,2) /= 8) .OR. (LBOUND(dbl2,2) /= -1) ) THEN
        WRITE(*,*) 'CALL CALL dmalloc0P(dbl2,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(dbl2,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl2,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl2,1) /= 8) .OR. (LBOUND(dbl2,1) /= -1) .OR. &
          (UBOUND(dbl2,2) /= 8) .OR. (LBOUND(dbl2,2) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(dbl2,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(dbl2,-1,1,-1,1)'
      ENDIF
      CALL demallocP(dbl2)
!
! rank 3 variable
      NULLIFY(dbl3)
      CALL dmallocP(dbl3,-10,10,10)
      CALL dmallocP(dbl3,10,-10,10)
      CALL dmallocP(dbl3,10,10,-10)
      CALL dmallocP(dbl3,10,10,10)
      IF((.NOT.ASSOCIATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. &
          (UBOUND(dbl3,1) /= 10) .OR. (LBOUND(dbl3,1) /= 1) .OR. &
          (UBOUND(dbl3,2) /= 10) .OR. (LBOUND(dbl3,2) /= 1) .OR. &
          (UBOUND(dbl3,3) /= 10) .OR. (LBOUND(dbl3,3) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(dbl3,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(dbl3,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl3,100,100,100)
      IF((.NOT.ASSOCIATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl3,1) /= 10) .OR. (LBOUND(dbl3,1) /= 1) .OR. &
          (UBOUND(dbl3,2) /= 10) .OR. (LBOUND(dbl3,2) /= 1) .OR. &
          (UBOUND(dbl3,3) /= 10) .OR. (LBOUND(dbl3,3) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(dbl3,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(dbl3,100,100,100)'
      ENDIF
      CALL demallocP(dbl3)
      IF( ASSOCIATED(dbl3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(dbl3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(dbl3)'
      ENDIF
      CALL demallocP(dbl3)
      IF( ASSOCIATED(dbl3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(dbl3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(dbl3)'
      ENDIF
      CALL dmalloc0P(dbl3,8,-1,-1,8,-1,8)
      CALL dmalloc0P(dbl3,-1,8,8,-1,-1,8)
      CALL dmalloc0P(dbl3,-1,8,-1,8,8,-1)
      CALL dmalloc0P(dbl3,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. &
          (UBOUND(dbl3,1) /= 8) .OR. (LBOUND(dbl3,1) /= -1) .OR. &
          (UBOUND(dbl3,2) /= 8) .OR. (LBOUND(dbl3,2) /= -1) .OR. &
          (UBOUND(dbl3,3) /= 8) .OR. (LBOUND(dbl3,3) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(dbl3,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(dbl3,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl3,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl3,1) /= 8) .OR. (LBOUND(dbl3,1) /= -1) .OR. &
          (UBOUND(dbl3,2) /= 8) .OR. (LBOUND(dbl3,2) /= -1) .OR. &
          (UBOUND(dbl3,3) /= 8) .OR. (LBOUND(dbl3,3) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(dbl3,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(dbl3,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(dbl3)
!
! rank 4 variable
      NULLIFY(dbl4)
      CALL dmallocP(dbl4,-10,10,10,10)
      CALL dmallocP(dbl4,10,-10,10,10)
      CALL dmallocP(dbl4,10,10,-10,10)
      CALL dmallocP(dbl4,10,10,10,-10)
      CALL dmallocP(dbl4,10,10,10,10)
      IF((.NOT.ASSOCIATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. &
          (UBOUND(dbl4,1) /= 10) .OR. (LBOUND(dbl4,1) /= 1) .OR. &
          (UBOUND(dbl4,2) /= 10) .OR. (LBOUND(dbl4,2) /= 1) .OR. &
          (UBOUND(dbl4,3) /= 10) .OR. (LBOUND(dbl4,3) /= 1) .OR. &
          (UBOUND(dbl4,4) /= 10) .OR. (LBOUND(dbl4,4) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(dbl4,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(dbl4,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl4,100,100,100,100)
      IF((.NOT.ASSOCIATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl4,1) /= 10) .OR. (LBOUND(dbl4,1) /= 1) .OR. &
          (UBOUND(dbl4,2) /= 10) .OR. (LBOUND(dbl4,2) /= 1) .OR. &
          (UBOUND(dbl4,3) /= 10) .OR. (LBOUND(dbl4,3) /= 1) .OR. &
          (UBOUND(dbl4,4) /= 10) .OR. (LBOUND(dbl4,4) /= 1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmallocP(dbl4,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP((dbl4,100,100,100,100)'
      ENDIF
      CALL demallocP(dbl4)
      IF( ASSOCIATED(dbl4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(dbl4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(dbl4)'
      ENDIF
      CALL demallocP(dbl4)
      IF( ASSOCIATED(dbl4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(dbl4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(dbl4)'
      ENDIF
      CALL dmalloc0P(dbl4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(dbl4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(dbl4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(dbl4,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. &
          (UBOUND(dbl4,1) /= 8) .OR. (LBOUND(dbl4,1) /= -1) .OR. &
          (UBOUND(dbl4,2) /= 8) .OR. (LBOUND(dbl4,2) /= -1) .OR. &
          (UBOUND(dbl4,3) /= 8) .OR. (LBOUND(dbl4,3) /= -1) .OR. &
          (UBOUND(dbl4,4) /= 8) .OR. (LBOUND(dbl4,4) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(dbl4,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(dbl4,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl4,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl4,1) /= 8) .OR. (LBOUND(dbl4,1) /= -1) .OR. &
          (UBOUND(dbl4,2) /= 8) .OR. (LBOUND(dbl4,2) /= -1) .OR. &
          (UBOUND(dbl4,3) /= 8) .OR. (LBOUND(dbl4,3) /= -1) .OR. &
          (UBOUND(dbl4,4) /= 8) .OR. (LBOUND(dbl4,4) /= -1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmalloc0P(dbl4,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(dbl4,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(dbl4)
!
! rank 5 variable
      NULLIFY(dbl5)
      CALL dmallocP(dbl5,-10,10,10,10,10)
      CALL dmallocP(dbl5,10,-10,10,10,10)
      CALL dmallocP(dbl5,10,10,-10,10,10)
      CALL dmallocP(dbl5,10,10,10,-10,10)
      CALL dmallocP(dbl5,10,10,10,10,-10)
      CALL dmallocP(dbl5,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK) .OR. &
          (UBOUND(dbl5,1) /= 10) .OR. (LBOUND(dbl5,1) /= 1) .OR. &
          (UBOUND(dbl5,2) /= 10) .OR. (LBOUND(dbl5,2) /= 1) .OR. &
          (UBOUND(dbl5,3) /= 10) .OR. (LBOUND(dbl5,3) /= 1) .OR. &
          (UBOUND(dbl5,4) /= 10) .OR. (LBOUND(dbl5,4) /= 1) .OR. &
          (UBOUND(dbl5,5) /= 10) .OR. (LBOUND(dbl5,5) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(dbl5,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(dbl5,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl5,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl5,1) /= 10) .OR. (LBOUND(dbl5,1) /= 1) .OR. &
          (UBOUND(dbl5,2) /= 10) .OR. (LBOUND(dbl5,2) /= 1) .OR. &
          (UBOUND(dbl5,3) /= 10) .OR. (LBOUND(dbl5,3) /= 1) .OR. &
          (UBOUND(dbl5,4) /= 10) .OR. (LBOUND(dbl5,4) /= 1) .OR. &
          (UBOUND(dbl5,5) /= 10) .OR. (LBOUND(dbl5,5) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(dbl5,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(dbl5,100,100,100,100,100)'
      ENDIF
      CALL demallocP(dbl5)
      IF( ASSOCIATED(dbl5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(dbl5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(dbl5)'
      ENDIF
      CALL demallocP(dbl5)
      IF( ASSOCIATED(dbl5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(dbl5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(dbl5)'
      ENDIF
      CALL dmalloc0P(dbl5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(dbl5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(dbl5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(dbl5,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK) .OR. &
          (UBOUND(dbl5,1) /= 8) .OR. (LBOUND(dbl5,1) /= -1) .OR. &
          (UBOUND(dbl5,2) /= 8) .OR. (LBOUND(dbl5,2) /= -1) .OR. &
          (UBOUND(dbl5,3) /= 8) .OR. (LBOUND(dbl5,3) /= -1) .OR. &
          (UBOUND(dbl5,4) /= 8) .OR. (LBOUND(dbl5,4) /= -1) .OR. &
          (UBOUND(dbl5,5) /= 8) .OR. (LBOUND(dbl5,5) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocP(dbl5,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(dbl5,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl5,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl5,1) /= 8) .OR. (LBOUND(dbl5,1) /= -1) .OR. &
          (UBOUND(dbl5,2) /= 8) .OR. (LBOUND(dbl5,2) /= -1) .OR. &
          (UBOUND(dbl5,3) /= 8) .OR. (LBOUND(dbl5,3) /= -1) .OR. &
          (UBOUND(dbl5,4) /= 8) .OR. (LBOUND(dbl5,4) /= -1) .OR. &
          (UBOUND(dbl5,5) /= 8) .OR. (LBOUND(dbl5,5) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0P(dbl5,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(dbl5,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(dbl5)
!
! rank 6 variable
      NULLIFY(dbl6)
      CALL dmallocP(dbl6,-10,10,10,10,10,10)
      CALL dmallocP(dbl6,10,-10,10,10,10,10)
      CALL dmallocP(dbl6,10,10,-10,10,10,10)
      CALL dmallocP(dbl6,10,10,10,-10,10,10)
      CALL dmallocP(dbl6,10,10,10,10,-10,10)
      CALL dmallocP(dbl6,10,10,10,10,10,-10)
      CALL dmallocP(dbl6,10,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK) .OR. &
          (UBOUND(dbl6,1) /= 10) .OR. (LBOUND(dbl6,1) /= 1) .OR. &
          (UBOUND(dbl6,2) /= 10) .OR. (LBOUND(dbl6,2) /= 1) .OR. &
          (UBOUND(dbl6,3) /= 10) .OR. (LBOUND(dbl6,3) /= 1) .OR. &
          (UBOUND(dbl6,4) /= 10) .OR. (LBOUND(dbl6,4) /= 1) .OR. &
          (UBOUND(dbl6,5) /= 10) .OR. (LBOUND(dbl6,5) /= 1) .OR. &
          (UBOUND(dbl6,6) /= 10) .OR. (LBOUND(dbl6,6) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(dbl6,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(dbl6,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl6,100,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl6,1) /= 10) .OR. (LBOUND(dbl6,1) /= 1) .OR. &
          (UBOUND(dbl6,2) /= 10) .OR. (LBOUND(dbl6,2) /= 1) .OR. &
          (UBOUND(dbl6,3) /= 10) .OR. (LBOUND(dbl6,3) /= 1) .OR. &
          (UBOUND(dbl6,4) /= 10) .OR. (LBOUND(dbl6,4) /= 1) .OR. &
          (UBOUND(dbl6,5) /= 10) .OR. (LBOUND(dbl6,5) /= 1) .OR. &
          (UBOUND(dbl6,6) /= 10) .OR. (LBOUND(dbl6,6) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(dbl6,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(dbl6,100,100,100,100,100,100)'
      ENDIF
      CALL demallocP(dbl6)
      IF( ASSOCIATED(dbl6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(dbl6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(dbl6)'
      ENDIF
      CALL demallocP(dbl6)
      IF( ASSOCIATED(dbl6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(dbl6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(dbl6)'
      ENDIF
      CALL dmalloc0P(dbl6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(dbl6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK) .OR. &
          (UBOUND(dbl6,1) /= 8) .OR. (LBOUND(dbl6,1) /= -1) .OR. &
          (UBOUND(dbl6,2) /= 8) .OR. (LBOUND(dbl6,2) /= -1) .OR. &
          (UBOUND(dbl6,3) /= 8) .OR. (LBOUND(dbl6,3) /= -1) .OR. &
          (UBOUND(dbl6,4) /= 8) .OR. (LBOUND(dbl6,4) /= -1) .OR. &
          (UBOUND(dbl6,5) /= 8) .OR. (LBOUND(dbl6,5) /= -1) .OR. &
          (UBOUND(dbl6,6) /= 8) .OR. (LBOUND(dbl6,6) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocP(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl6,1) /= 8) .OR. (LBOUND(dbl6,1) /= -1) .OR. &
          (UBOUND(dbl6,2) /= 8) .OR. (LBOUND(dbl6,2) /= -1) .OR. &
          (UBOUND(dbl6,3) /= 8) .OR. (LBOUND(dbl6,3) /= -1) .OR. &
          (UBOUND(dbl6,4) /= 8) .OR. (LBOUND(dbl6,4) /= -1) .OR. &
          (UBOUND(dbl6,5) /= 8) .OR. (LBOUND(dbl6,5) /= -1) .OR. &
          (UBOUND(dbl6,6) /= 8) .OR. (LBOUND(dbl6,6) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0P(dbl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(dbl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(dbl6)
!
! rank 7 variable
      NULLIFY(dbl7)
      CALL dmallocP(dbl7,-10,10,10,10,10,10,10)
      CALL dmallocP(dbl7,10,-10,10,10,10,10,10)
      CALL dmallocP(dbl7,10,10,-10,10,10,10,10)
      CALL dmallocP(dbl7,10,10,10,-10,10,10,10)
      CALL dmallocP(dbl7,10,10,10,10,-10,10,10)
      CALL dmallocP(dbl7,10,10,10,10,10,-10,10)
      CALL dmallocP(dbl7,10,10,10,10,10,10,-10)
      CALL dmallocP(dbl7,10,10,10,10,10,10,10)
      IF((.NOT.ASSOCIATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK) .OR. &
          (UBOUND(dbl7,1) /= 10) .OR. (LBOUND(dbl7,1) /= 1) .OR. &
          (UBOUND(dbl7,2) /= 10) .OR. (LBOUND(dbl7,2) /= 1) .OR. &
          (UBOUND(dbl7,3) /= 10) .OR. (LBOUND(dbl7,3) /= 1) .OR. &
          (UBOUND(dbl7,4) /= 10) .OR. (LBOUND(dbl7,4) /= 1) .OR. &
          (UBOUND(dbl7,5) /= 10) .OR. (LBOUND(dbl7,5) /= 1) .OR. &
          (UBOUND(dbl7,6) /= 10) .OR. (LBOUND(dbl7,6) /= 1) .OR. &
          (UBOUND(dbl7,7) /= 10) .OR. (LBOUND(dbl7,7) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocP(dbl7,10,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocP(dbl7,10,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl7,100,100,100,100,100,100,100)
      IF((.NOT.ASSOCIATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl7,1) /= 10) .OR. (LBOUND(dbl7,1) /= 1) .OR. &
          (UBOUND(dbl7,2) /= 10) .OR. (LBOUND(dbl7,2) /= 1) .OR. &
          (UBOUND(dbl7,3) /= 10) .OR. (LBOUND(dbl7,3) /= 1) .OR. &
          (UBOUND(dbl7,4) /= 10) .OR. (LBOUND(dbl7,4) /= 1) .OR. &
          (UBOUND(dbl7,5) /= 10) .OR. (LBOUND(dbl7,5) /= 1) .OR. &
          (UBOUND(dbl7,6) /= 10) .OR. (LBOUND(dbl7,6) /= 1) .OR. &
          (UBOUND(dbl7,7) /= 10) .OR. (LBOUND(dbl7,7) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocP(dbl7,100,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocP(dbl7,100,100,100,100,100,100,100)'
      ENDIF
      CALL demallocP(dbl7)
      IF( ASSOCIATED(dbl7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocP(dbl7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocP(dbl7)'
      ENDIF
      CALL demallocP(dbl7)
      IF( ASSOCIATED(dbl7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocP(dbl7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocP(dbl7)'
      ENDIF
      CALL dmalloc0P(dbl7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ASSOCIATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK) .OR. &
          (UBOUND(dbl7,1) /= 8) .OR. (LBOUND(dbl7,1) /= -1) .OR. &
          (UBOUND(dbl7,2) /= 8) .OR. (LBOUND(dbl7,2) /= -1) .OR. &
          (UBOUND(dbl7,3) /= 8) .OR. (LBOUND(dbl7,3) /= -1) .OR. &
          (UBOUND(dbl7,4) /= 8) .OR. (LBOUND(dbl7,4) /= -1) .OR. &
          (UBOUND(dbl7,5) /= 8) .OR. (LBOUND(dbl7,5) /= -1) .OR. &
          (UBOUND(dbl7,6) /= 8) .OR. (LBOUND(dbl7,6) /= -1) .OR. &
          (UBOUND(dbl7,7) /= 8) .OR. (LBOUND(dbl7,7) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0P(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0P(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ASSOCIATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl7,1) /= 8) .OR. (LBOUND(dbl7,1) /= -1) .OR. &
          (UBOUND(dbl7,2) /= 8) .OR. (LBOUND(dbl7,2) /= -1) .OR. &
          (UBOUND(dbl7,3) /= 8) .OR. (LBOUND(dbl7,3) /= -1) .OR. &
          (UBOUND(dbl7,4) /= 8) .OR. (LBOUND(dbl7,4) /= -1) .OR. &
          (UBOUND(dbl7,5) /= 8) .OR. (LBOUND(dbl7,5) /= -1) .OR. &
          (UBOUND(dbl7,6) /= 8) .OR. (LBOUND(dbl7,6) /= -1) .OR. &
          (UBOUND(dbl7,7) /= 8) .OR. (LBOUND(dbl7,7) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0P(dbl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0P(dbl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocP(dbl7)
      WRITE(*,*) '---------------------------------------------------'
    ENDSUBROUTINE testDOUBLEP
!
!===============================================================================
! Test allocation/deallocation for single precision
    SUBROUTINE testDOUBLEA
      USE IntrType
      USE Allocs
      IMPLICIT NONE
    
      REAL(SDK),ALLOCATABLE :: dbl1(:)
      REAL(SDK),ALLOCATABLE :: dbl2(:,:)
      REAL(SDK),ALLOCATABLE :: dbl3(:,:,:)
      REAL(SDK),ALLOCATABLE :: dbl4(:,:,:,:)
      REAL(SDK),ALLOCATABLE :: dbl5(:,:,:,:,:)
      REAL(SDK),ALLOCATABLE :: dbl6(:,:,:,:,:,:)
      REAL(SDK),ALLOCATABLE :: dbl7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0

      WRITE(*,*) 'TESTING ALLOCS FOR DOUBLE PRECISION REAL ALLOCATABLE TYPES'
!
! rank 1 variable
      CALL dmallocA(dbl1,-10)
      CALL dmallocA(dbl1,10)
      IF( (.NOT.ALLOCATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. &
          (UBOUND(dbl1,1) /= 10) .OR. (LBOUND(dbl1,1) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(dbl1,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(dbl1,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl1,100)
      IF( (.NOT.ALLOCATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl1,1) /= 10) .OR. (LBOUND(dbl1,1) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(dbl1,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(dbl1,100)'
      ENDIF
      CALL demallocA(dbl1)
      IF( ALLOCATED(dbl1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(dbl1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(dbl1)'
      ENDIF
      CALL demallocA(dbl1)
      IF( ALLOCATED(dbl1) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(dbl1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(dbl1)'
      ENDIF
      CALL dmalloc0A(dbl1,8,-1)
      CALL dmalloc0A(dbl1,-1,8)
      IF( (.NOT.ALLOCATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. &
          (UBOUND(dbl1,1) /= 8) .OR. (LBOUND(dbl1,1) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(dbl1,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(dbl1,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl1,-1,1)
      IF( (.NOT.ALLOCATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl1,1) /= 8) .OR. (LBOUND(dbl1,1) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(dbl1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(dbl1,-1,1) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      CALL demallocA(dbl1)
!
! rank 2 variable
      CALL dmallocA(dbl2,-10,10)
      CALL dmallocA(dbl2,10,-10)
      CALL dmallocA(dbl2,10,10)
      IF((.NOT.ALLOCATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. &
          (UBOUND(dbl2,1) /= 10) .OR. (LBOUND(dbl2,1) /= 1) .OR. &
          (UBOUND(dbl2,2) /= 10) .OR. (LBOUND(dbl2,2) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(dbl2,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(dbl2,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl2,100,100)
      IF((.NOT.ALLOCATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl2,1) /= 10) .OR. (LBOUND(dbl2,1) /= 1) .OR. &
          (UBOUND(dbl2,2) /= 10) .OR. (LBOUND(dbl2,2) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(dbl2,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(dbl2,100,100)'
      ENDIF
      CALL demallocA(dbl2)
      IF( ALLOCATED(dbl2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(dbl2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(dbl2)'
      ENDIF
      CALL demallocA(dbl2)
      IF( ALLOCATED(dbl2) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(dbl2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(dbl2)'
      ENDIF
      CALL dmalloc0A(dbl2,8,-1,-1,8)
      CALL dmalloc0A(dbl2,-1,8,8,-1)
      CALL dmalloc0A(dbl2,-1,8,-1,8)
      IF((.NOT.ALLOCATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. &
          (UBOUND(dbl2,1) /= 8) .OR. (LBOUND(dbl2,1) /= -1) .OR. &
          (UBOUND(dbl2,2) /= 8) .OR. (LBOUND(dbl2,2) /= -1) ) THEN
        WRITE(*,*) 'CALL CALL dmalloc0A(dbl2,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(dbl2,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl2,-1,1,-1,1)
      IF((.NOT.ALLOCATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl2,1) /= 8) .OR. (LBOUND(dbl2,1) /= -1) .OR. &
          (UBOUND(dbl2,2) /= 8) .OR. (LBOUND(dbl2,2) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(dbl2,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(dbl2,-1,1,-1,1)'
      ENDIF
      CALL demallocA(dbl2)
!
! rank 3 variable
      CALL dmallocA(dbl3,-10,10,10)
      CALL dmallocA(dbl3,10,-10,10)
      CALL dmallocA(dbl3,10,10,-10)
      CALL dmallocA(dbl3,10,10,10)
      IF((.NOT.ALLOCATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. &
          (UBOUND(dbl3,1) /= 10) .OR. (LBOUND(dbl3,1) /= 1) .OR. &
          (UBOUND(dbl3,2) /= 10) .OR. (LBOUND(dbl3,2) /= 1) .OR. &
          (UBOUND(dbl3,3) /= 10) .OR. (LBOUND(dbl3,3) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(dbl3,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(dbl3,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl3,100,100,100)
      IF((.NOT.ALLOCATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl3,1) /= 10) .OR. (LBOUND(dbl3,1) /= 1) .OR. &
          (UBOUND(dbl3,2) /= 10) .OR. (LBOUND(dbl3,2) /= 1) .OR. &
          (UBOUND(dbl3,3) /= 10) .OR. (LBOUND(dbl3,3) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(dbl3,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(dbl3,100,100,100)'
      ENDIF
      CALL demallocA(dbl3)
      IF( ALLOCATED(dbl3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(dbl3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(dbl3)'
      ENDIF
      CALL demallocA(dbl3)
      IF( ALLOCATED(dbl3) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(dbl3) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(dbl3)'
      ENDIF
      CALL dmalloc0A(dbl3,8,-1,-1,8,-1,8)
      CALL dmalloc0A(dbl3,-1,8,8,-1,-1,8)
      CALL dmalloc0A(dbl3,-1,8,-1,8,8,-1)
      CALL dmalloc0A(dbl3,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. &
          (UBOUND(dbl3,1) /= 8) .OR. (LBOUND(dbl3,1) /= -1) .OR. &
          (UBOUND(dbl3,2) /= 8) .OR. (LBOUND(dbl3,2) /= -1) .OR. &
          (UBOUND(dbl3,3) /= 8) .OR. (LBOUND(dbl3,3) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(dbl3,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(dbl3,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl3,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl3,1) /= 8) .OR. (LBOUND(dbl3,1) /= -1) .OR. &
          (UBOUND(dbl3,2) /= 8) .OR. (LBOUND(dbl3,2) /= -1) .OR. &
          (UBOUND(dbl3,3) /= 8) .OR. (LBOUND(dbl3,3) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(dbl3,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(dbl3,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(dbl3)
!
! rank 4 variable
      CALL dmallocA(dbl4,-10,10,10,10)
      CALL dmallocA(dbl4,10,-10,10,10)
      CALL dmallocA(dbl4,10,10,-10,10)
      CALL dmallocA(dbl4,10,10,10,-10)
      CALL dmallocA(dbl4,10,10,10,10)
      IF((.NOT.ALLOCATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. &
          (UBOUND(dbl4,1) /= 10) .OR. (LBOUND(dbl4,1) /= 1) .OR. &
          (UBOUND(dbl4,2) /= 10) .OR. (LBOUND(dbl4,2) /= 1) .OR. &
          (UBOUND(dbl4,3) /= 10) .OR. (LBOUND(dbl4,3) /= 1) .OR. &
          (UBOUND(dbl4,4) /= 10) .OR. (LBOUND(dbl4,4) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(dbl4,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(dbl4,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl4,100,100,100,100)
      IF((.NOT.ALLOCATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl4,1) /= 10) .OR. (LBOUND(dbl4,1) /= 1) .OR. &
          (UBOUND(dbl4,2) /= 10) .OR. (LBOUND(dbl4,2) /= 1) .OR. &
          (UBOUND(dbl4,3) /= 10) .OR. (LBOUND(dbl4,3) /= 1) .OR. &
          (UBOUND(dbl4,4) /= 10) .OR. (LBOUND(dbl4,4) /= 1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmallocA(dbl4,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA((dbl4,100,100,100,100)'
      ENDIF
      CALL demallocA(dbl4)
      IF( ALLOCATED(dbl4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(dbl4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(dbl4)'
      ENDIF
      CALL demallocA(dbl4)
      IF( ALLOCATED(dbl4) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(dbl4) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(dbl4)'
      ENDIF
      CALL dmalloc0A(dbl4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(dbl4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(dbl4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(dbl4,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. &
          (UBOUND(dbl4,1) /= 8) .OR. (LBOUND(dbl4,1) /= -1) .OR. &
          (UBOUND(dbl4,2) /= 8) .OR. (LBOUND(dbl4,2) /= -1) .OR. &
          (UBOUND(dbl4,3) /= 8) .OR. (LBOUND(dbl4,3) /= -1) .OR. &
          (UBOUND(dbl4,4) /= 8) .OR. (LBOUND(dbl4,4) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(dbl4,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(dbl4,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl4,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl4,1) /= 8) .OR. (LBOUND(dbl4,1) /= -1) .OR. &
          (UBOUND(dbl4,2) /= 8) .OR. (LBOUND(dbl4,2) /= -1) .OR. &
          (UBOUND(dbl4,3) /= 8) .OR. (LBOUND(dbl4,3) /= -1) .OR. &
          (UBOUND(dbl4,4) /= 8) .OR. (LBOUND(dbl4,4) /= -1) ) THEN
        WRITE(*,*) 'Rednundant CALL dmalloc0A(dbl4,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(dbl4,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(dbl4)
!
! rank 5 variable
      CALL dmallocA(dbl5,-10,10,10,10,10)
      CALL dmallocA(dbl5,10,-10,10,10,10)
      CALL dmallocA(dbl5,10,10,-10,10,10)
      CALL dmallocA(dbl5,10,10,10,-10,10)
      CALL dmallocA(dbl5,10,10,10,10,-10)
      CALL dmallocA(dbl5,10,10,10,10,10)
      IF((.NOT.ALLOCATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK) .OR. &
          (UBOUND(dbl5,1) /= 10) .OR. (LBOUND(dbl5,1) /= 1) .OR. &
          (UBOUND(dbl5,2) /= 10) .OR. (LBOUND(dbl5,2) /= 1) .OR. &
          (UBOUND(dbl5,3) /= 10) .OR. (LBOUND(dbl5,3) /= 1) .OR. &
          (UBOUND(dbl5,4) /= 10) .OR. (LBOUND(dbl5,4) /= 1) .OR. &
          (UBOUND(dbl5,5) /= 10) .OR. (LBOUND(dbl5,5) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(dbl5,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(dbl5,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl5,100,100,100,100,100)
      IF((.NOT.ALLOCATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl5,1) /= 10) .OR. (LBOUND(dbl5,1) /= 1) .OR. &
          (UBOUND(dbl5,2) /= 10) .OR. (LBOUND(dbl5,2) /= 1) .OR. &
          (UBOUND(dbl5,3) /= 10) .OR. (LBOUND(dbl5,3) /= 1) .OR. &
          (UBOUND(dbl5,4) /= 10) .OR. (LBOUND(dbl5,4) /= 1) .OR. &
          (UBOUND(dbl5,5) /= 10) .OR. (LBOUND(dbl5,5) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(dbl5,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(dbl5,100,100,100,100,100)'
      ENDIF
      CALL demallocA(dbl5)
      IF( ALLOCATED(dbl5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(dbl5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(dbl5)'
      ENDIF
      CALL demallocA(dbl5)
      IF( ALLOCATED(dbl5) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(dbl5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(dbl5)'
      ENDIF
      CALL dmalloc0A(dbl5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(dbl5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(dbl5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(dbl5,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK) .OR. &
          (UBOUND(dbl5,1) /= 8) .OR. (LBOUND(dbl5,1) /= -1) .OR. &
          (UBOUND(dbl5,2) /= 8) .OR. (LBOUND(dbl5,2) /= -1) .OR. &
          (UBOUND(dbl5,3) /= 8) .OR. (LBOUND(dbl5,3) /= -1) .OR. &
          (UBOUND(dbl5,4) /= 8) .OR. (LBOUND(dbl5,4) /= -1) .OR. &
          (UBOUND(dbl5,5) /= 8) .OR. (LBOUND(dbl5,5) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocA(dbl5,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(dbl5,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl5,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl5,1) /= 8) .OR. (LBOUND(dbl5,1) /= -1) .OR. &
          (UBOUND(dbl5,2) /= 8) .OR. (LBOUND(dbl5,2) /= -1) .OR. &
          (UBOUND(dbl5,3) /= 8) .OR. (LBOUND(dbl5,3) /= -1) .OR. &
          (UBOUND(dbl5,4) /= 8) .OR. (LBOUND(dbl5,4) /= -1) .OR. &
          (UBOUND(dbl5,5) /= 8) .OR. (LBOUND(dbl5,5) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0A(dbl5,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(dbl5,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(dbl5)
!
! rank 6 variable
      CALL dmallocA(dbl6,-10,10,10,10,10,10)
      CALL dmallocA(dbl6,10,-10,10,10,10,10)
      CALL dmallocA(dbl6,10,10,-10,10,10,10)
      CALL dmallocA(dbl6,10,10,10,-10,10,10)
      CALL dmallocA(dbl6,10,10,10,10,-10,10)
      CALL dmallocA(dbl6,10,10,10,10,10,-10)
      CALL dmallocA(dbl6,10,10,10,10,10,10)
      IF((.NOT.ALLOCATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK) .OR. &
          (UBOUND(dbl6,1) /= 10) .OR. (LBOUND(dbl6,1) /= 1) .OR. &
          (UBOUND(dbl6,2) /= 10) .OR. (LBOUND(dbl6,2) /= 1) .OR. &
          (UBOUND(dbl6,3) /= 10) .OR. (LBOUND(dbl6,3) /= 1) .OR. &
          (UBOUND(dbl6,4) /= 10) .OR. (LBOUND(dbl6,4) /= 1) .OR. &
          (UBOUND(dbl6,5) /= 10) .OR. (LBOUND(dbl6,5) /= 1) .OR. &
          (UBOUND(dbl6,6) /= 10) .OR. (LBOUND(dbl6,6) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(dbl6,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(dbl6,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl6,100,100,100,100,100,100)
      IF((.NOT.ALLOCATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl6,1) /= 10) .OR. (LBOUND(dbl6,1) /= 1) .OR. &
          (UBOUND(dbl6,2) /= 10) .OR. (LBOUND(dbl6,2) /= 1) .OR. &
          (UBOUND(dbl6,3) /= 10) .OR. (LBOUND(dbl6,3) /= 1) .OR. &
          (UBOUND(dbl6,4) /= 10) .OR. (LBOUND(dbl6,4) /= 1) .OR. &
          (UBOUND(dbl6,5) /= 10) .OR. (LBOUND(dbl6,5) /= 1) .OR. &
          (UBOUND(dbl6,6) /= 10) .OR. (LBOUND(dbl6,6) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(dbl6,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(dbl6,100,100,100,100,100,100)'
      ENDIF
      CALL demallocA(dbl6)
      IF( ALLOCATED(dbl6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(dbl6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(dbl6)'
      ENDIF
      CALL demallocA(dbl6)
      IF( ALLOCATED(dbl6) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(dbl6) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(dbl6)'
      ENDIF
      CALL dmalloc0A(dbl6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(dbl6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK) .OR. &
          (UBOUND(dbl6,1) /= 8) .OR. (LBOUND(dbl6,1) /= -1) .OR. &
          (UBOUND(dbl6,2) /= 8) .OR. (LBOUND(dbl6,2) /= -1) .OR. &
          (UBOUND(dbl6,3) /= 8) .OR. (LBOUND(dbl6,3) /= -1) .OR. &
          (UBOUND(dbl6,4) /= 8) .OR. (LBOUND(dbl6,4) /= -1) .OR. &
          (UBOUND(dbl6,5) /= 8) .OR. (LBOUND(dbl6,5) /= -1) .OR. &
          (UBOUND(dbl6,6) /= 8) .OR. (LBOUND(dbl6,6) /= -1) ) THEN
        WRITE(*,*) 'CALL dmallocA(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl6,1) /= 8) .OR. (LBOUND(dbl6,1) /= -1) .OR. &
          (UBOUND(dbl6,2) /= 8) .OR. (LBOUND(dbl6,2) /= -1) .OR. &
          (UBOUND(dbl6,3) /= 8) .OR. (LBOUND(dbl6,3) /= -1) .OR. &
          (UBOUND(dbl6,4) /= 8) .OR. (LBOUND(dbl6,4) /= -1) .OR. &
          (UBOUND(dbl6,5) /= 8) .OR. (LBOUND(dbl6,5) /= -1) .OR. &
          (UBOUND(dbl6,6) /= 8) .OR. (LBOUND(dbl6,6) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL CALL dmalloc0A(dbl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(dbl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(dbl6)
!
! rank 7 variable
      CALL dmallocA(dbl7,-10,10,10,10,10,10,10)
      CALL dmallocA(dbl7,10,-10,10,10,10,10,10)
      CALL dmallocA(dbl7,10,10,-10,10,10,10,10)
      CALL dmallocA(dbl7,10,10,10,-10,10,10,10)
      CALL dmallocA(dbl7,10,10,10,10,-10,10,10)
      CALL dmallocA(dbl7,10,10,10,10,10,-10,10)
      CALL dmallocA(dbl7,10,10,10,10,10,10,-10)
      CALL dmallocA(dbl7,10,10,10,10,10,10,10)
      IF((.NOT.ALLOCATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK) .OR. &
          (UBOUND(dbl7,1) /= 10) .OR. (LBOUND(dbl7,1) /= 1) .OR. &
          (UBOUND(dbl7,2) /= 10) .OR. (LBOUND(dbl7,2) /= 1) .OR. &
          (UBOUND(dbl7,3) /= 10) .OR. (LBOUND(dbl7,3) /= 1) .OR. &
          (UBOUND(dbl7,4) /= 10) .OR. (LBOUND(dbl7,4) /= 1) .OR. &
          (UBOUND(dbl7,5) /= 10) .OR. (LBOUND(dbl7,5) /= 1) .OR. &
          (UBOUND(dbl7,6) /= 10) .OR. (LBOUND(dbl7,6) /= 1) .OR. &
          (UBOUND(dbl7,7) /= 10) .OR. (LBOUND(dbl7,7) /= 1) ) THEN
        WRITE(*,*) 'CALL dmallocA(dbl7,10,10,10,10,10,10,10) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmallocA(dbl7,10,10,10,10,10,10,10) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl7,100,100,100,100,100,100,100)
      IF((.NOT.ALLOCATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl7,1) /= 10) .OR. (LBOUND(dbl7,1) /= 1) .OR. &
          (UBOUND(dbl7,2) /= 10) .OR. (LBOUND(dbl7,2) /= 1) .OR. &
          (UBOUND(dbl7,3) /= 10) .OR. (LBOUND(dbl7,3) /= 1) .OR. &
          (UBOUND(dbl7,4) /= 10) .OR. (LBOUND(dbl7,4) /= 1) .OR. &
          (UBOUND(dbl7,5) /= 10) .OR. (LBOUND(dbl7,5) /= 1) .OR. &
          (UBOUND(dbl7,6) /= 10) .OR. (LBOUND(dbl7,6) /= 1) .OR. &
          (UBOUND(dbl7,7) /= 10) .OR. (LBOUND(dbl7,7) /= 1) ) THEN
        WRITE(*,*) 'Redundant CALL dmallocA(dbl7,100,100,100,100,100,100,100) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmallocA(dbl7,100,100,100,100,100,100,100)'
      ENDIF
      CALL demallocA(dbl7)
      IF( ALLOCATED(dbl7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'CALL demallocA(dbl7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL demallocA(dbl7)'
      ENDIF
      CALL demallocA(dbl7)
      IF( ALLOCATED(dbl7) .OR. Alloc_nbytes /= 0.0_SRK) THEN
        WRITE(*,*) 'Redundant CALL demallocA(dbl7) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL demallocA(dbl7)'
      ENDIF
      CALL dmalloc0A(dbl7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      IF((.NOT.ALLOCATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK) .OR. &
          (UBOUND(dbl7,1) /= 8) .OR. (LBOUND(dbl7,1) /= -1) .OR. &
          (UBOUND(dbl7,2) /= 8) .OR. (LBOUND(dbl7,2) /= -1) .OR. &
          (UBOUND(dbl7,3) /= 8) .OR. (LBOUND(dbl7,3) /= -1) .OR. &
          (UBOUND(dbl7,4) /= 8) .OR. (LBOUND(dbl7,4) /= -1) .OR. &
          (UBOUND(dbl7,5) /= 8) .OR. (LBOUND(dbl7,5) /= -1) .OR. &
          (UBOUND(dbl7,6) /= 8) .OR. (LBOUND(dbl7,6) /= -1) .OR. &
          (UBOUND(dbl7,7) /= 8) .OR. (LBOUND(dbl7,7) /= -1) ) THEN
        WRITE(*,*) 'CALL dmalloc0A(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL dmalloc0A(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8) = ' &
                   //TRIM(getMemUsageChar())
      ENDIF
      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      IF((.NOT.ALLOCATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl7,1) /= 8) .OR. (LBOUND(dbl7,1) /= -1) .OR. &
          (UBOUND(dbl7,2) /= 8) .OR. (LBOUND(dbl7,2) /= -1) .OR. &
          (UBOUND(dbl7,3) /= 8) .OR. (LBOUND(dbl7,3) /= -1) .OR. &
          (UBOUND(dbl7,4) /= 8) .OR. (LBOUND(dbl7,4) /= -1) .OR. &
          (UBOUND(dbl7,5) /= 8) .OR. (LBOUND(dbl7,5) /= -1) .OR. &
          (UBOUND(dbl7,6) /= 8) .OR. (LBOUND(dbl7,6) /= -1) .OR. &
          (UBOUND(dbl7,7) /= 8) .OR. (LBOUND(dbl7,7) /= -1) ) THEN
        WRITE(*,*) 'Redundant CALL dmalloc0A(dbl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Redundant CALL dmalloc0A(dbl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)'
      ENDIF
      CALL demallocA(dbl7)
      WRITE(*,*) '---------------------------------------------------'
    ENDSUBROUTINE testDOUBLEA
!
!===============================================================================
! Test error checking for over allocation
    SUBROUTINE testTOOBIGP
      USE IntrType
      USE Allocs
      
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
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocP(b1tb,2147483647) FAILED!'
!          CALL demallocP(b1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocP(b1tb,2147483647)'
!        ENDIF
!       WRITE(*,*) '  Skipping: CALL dmalloc0P(b1tb,-2147483648,2147483647)'
!        CALL dmalloc0P(b1tb,-2147483648,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
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
       WRITE(*,*) '  Skipping: CALL dmallocP(i1tb,2147483647)'
!        CALL dmallocP(i1tb,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocP(i1tb,2147483647) FAILED!'
!          CALL demallocP(i1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocP(i1tb,2147483647)'
!        ENDIF
       WRITE(*,*) '  Skipping: CALL dmalloc0P(i1tb,-2147483648,2147483647)'
!        CALL dmalloc0P(i1tb,-2147483648,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
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
       WRITE(*,*) '  Skipping: CALL dmallocP(l1tb,2147483647)'
!        CALL dmallocP(l1tb,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocP(l1tb,2147483647) FAILED!'
!          CALL demallocP(l1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocP(l1tb,2147483647)'
!        ENDIF
       WRITE(*,*) '  Skipping: CALL dmalloc0P(l1tb,-2147483648,2147483647)'
!        CALL dmalloc0P(l1tb,-2147483648,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
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
       WRITE(*,*) '  Skipping: CALL dmallocP(s1tb,2147483647)'
!        CALL dmallocP(s1tb,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocP(s1tb,2147483647) FAILED!'
!          CALL demallocP(s1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocP(s1tb,2147483647)'
!        ENDIF
       WRITE(*,*) '  Skipping: CALL dmalloc0P(s1tb,-2147483648,2147483647)'
!        CALL dmalloc0P(s1tb,-2147483648,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0P(s1tb,-2147483648,2147483647) FAILED!'
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
       WRITE(*,*) '  Skipping: CALL dmallocP(d1tb,2147483647)'
!        CALL dmallocP(d1tb,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocP(d1tb,2147483647) FAILED!'
!          CALL demallocP(d1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocP(d1tb,2147483647)'
!        ENDIF
       WRITE(*,*) '  Skipping: CALL dmalloc0P(d1tb,-2147483648,2147483647)'
!        CALL dmalloc0P(d1tb,-2147483648,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0P(d1tb,-2147483648,2147483647) FAILED!'
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
    SUBROUTINE testTOOBIGA
      
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
      
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'SUM(eAllocs%getCounterAll()) FAILED!'
!          STOP 666
!        ELSE
!          WRITE(*,*) '  Passed: SUM(eAllocs%getCounterAll()) = ',SUM(eAllocs%getCounterAll())
!        ENDIF
!
!        IF(LEN_TRIM(eAllocs%getLastMessage()) /= 0) THEN
!          WRITE(*,*) 'eAllocs%getLastMessage() FAILED!'
!          STOP 666
!        ELSE
!          WRITE(*,*) '  Passed: eAllocs%getLastMessage() = '//TRIM(eAllocs%getLastMessage())
!        ENDIF
!
!Test logicals (booleans)
       WRITE(*,*) '  Skipping: CALL dmallocA(b1tb,2147483647)'
!        CALL dmallocA(b1tb,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocA(b1tb,2147483647) FAILED!'
!          CALL demallocP(b1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocA(b1tb,2147483647)'
!        ENDIF
       WRITE(*,*) '  Skipping: CALL dmalloc0A(b1tb,-2147483648,2147483647)'
!        CALL dmalloc0A(b1tb,-2147483648,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
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
       WRITE(*,*) '  Skipping: CALL dmallocA(i1tb,2147483647)'
!        CALL dmallocA(i1tb,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocA(i1tb,2147483647) FAILED!'
!          CALL demallocP(i1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocA(i1tb,2147483647)'
!        ENDIF
       WRITE(*,*) '  Skipping: CALL dmalloc0A(i1tb,-2147483648,2147483647)'
!        CALL dmalloc0A(i1tb,-2147483648,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
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
       WRITE(*,*) '  Skipping: CALL dmallocA(l1tb,2147483647)'
!        CALL dmallocA(l1tb,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocA(l1tb,2147483647) FAILED!'
!          CALL demallocP(l1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocA(l1tb,2147483647)'
!        ENDIF
       WRITE(*,*) '  Skipping: CALL dmalloc0A(l1tb,-2147483648,2147483647)'
!        CALL dmalloc0A(l1tb,-2147483648,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
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
       WRITE(*,*) '  Skipping: CALL dmallocA(s1tb,2147483647)'
!        CALL dmallocA(s1tb,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocA(s1tb,2147483647) FAILED!'
!          CALL demallocP(s1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocA(s1tb,2147483647)'
!        ENDIF
       WRITE(*,*) '  Skipping: CALL dmalloc0A(s1tb,-2147483648,2147483647)'
!        CALL dmalloc0A(s1tb,-2147483648,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0A(s1tb,-2147483648,2147483647) FAILED!'
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
       WRITE(*,*) '  Skipping: CALL dmallocA(d1tb,2147483647)'
!        CALL dmallocA(d1tb,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmallocA(d1tb,2147483647) FAILED!'
!          CALL demallocP(d1tb)
!        ELSE
!          WRITE(*,*) '  Passed: CALL dmallocA(d1tb,2147483647)'
!        ENDIF
       WRITE(*,*) '  Skipping: CALL dmalloc0A(d1tb,-2147483648,2147483647)'
!        CALL dmalloc0A(d1tb,-2147483648,2147483647)
!        IF(SUM(eAllocs%getCounterAll()) == 0) THEN
!          WRITE(*,*) 'CALL dmalloc0A(d1tb,-2147483648,2147483647) FAILED!'
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
ENDPROGRAM testAllocs
