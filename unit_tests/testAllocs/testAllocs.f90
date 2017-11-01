!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testAllocs
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE Allocs

  IMPLICIT NONE

  LOGICAL(SBK) :: test

  CREATE_TEST('Allocs')

  REGISTER_SUBTEST('testGetMemUsage()',testGetMemUsage)
  REGISTER_SUBTEST('testGetMemUsageChar()',testGetMemUsageChar)

  CALL eAllocs%setStopOnError(.FALSE.)
  CALL eAllocs%setQuietMode(.TRUE.)

  REGISTER_SUBTEST('testAllocsError',testAllocsError)
  REGISTER_SUBTEST('testBOOLP()',testBOOLP)
  REGISTER_SUBTEST('testBOOLA()',testBOOLA)
  REGISTER_SUBTEST('testINTP()',testINTP)
  REGISTER_SUBTEST('testINTA()',testINTA)
  REGISTER_SUBTEST('testLONGINTP()',testLONGINTP)
  REGISTER_SUBTEST('testLONGINTA()',testLONGINTA)
  REGISTER_SUBTEST('testSINGLEP()',testSINGLEP)
  REGISTER_SUBTEST('testSINGLEA()',testSINGLEA)
  REGISTER_SUBTEST('testDOUBLEP()',testDOUBLEP)
  REGISTER_SUBTEST('testDOUBLEA()',testDOUBLEa)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testAllocsError()

      CALL AllocsError(3_SIK,'test error message',50.0_SRK)
      ASSERT(SUM(eAllocs%getCounterAll()) == 1,'Allocs Error Handling')

    ENDSUBROUTINE testAllocsError
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetMemUsageChar()

      ASSERT(ALLOC_MEMSTRING_LENGTH == 14_SIK,'ALLOC_MEMSTRING_LENGTH')
      ASSERT(getMemUsageChar(563246226243._SRK) == '  524.56 GB   ','getMemUsageChar(563246226243._SRK)')
      ASSERT(getMemUsageChar() == '    0.00 bytes','getMemUsageChar()')

    ENDSUBROUTINE testGetMemUsageChar
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetMemUsage()
      REAL(SDK) :: memory
      REAL(SRK),ALLOCATABLE :: tmpvar(:)

      CALL dmallocA(tmpvar,1000000_SIK)
      CALL getMemUsage(memory,'whatever')
      ASSERT(memory .APPROXEQA. 8000000.0_SRK,'getMemUsage(memory,''bytes'')')

      CALL getMemUsage(memory,'KB')
      ASSERT(memory .APPROXEQA. 7812.5_SRK,'getMemUsage(memory,''KB'')')

      CALL getMemUsage(memory,'MB')
      ASSERT(memory .APPROXEQA. 7.62939453125_SRK,'getMemUsage(memory,''MB'')')

      CALL getMemUsage(memory,'GB')
      ASSERT(memory .APPROXEQA. 7.450580596923828E-3_SRK,'getMemUsage(memory,''GB'')')

      CALL demallocA(tmpvar)

    ENDSUBROUTINE testGetMemUsage
!
!-------------------------------------------------------------------------------
! Test allocation/deallocation for booleans
    SUBROUTINE testBOOLP()
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
      test=(.NOT.ASSOCIATED(bool2)) .OR. ANY(bool2) .OR. &
          (UBOUND(bool2,1) /= 10) .OR. (LBOUND(bool2,1) /= 1) .OR. &
          (UBOUND(bool2,2) /= 10) .OR. (LBOUND(bool2,2) /= 1)
      ASSERT(.NOT.test,'dmallocP(bool2,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(bool2,100,100)
      test=(.NOT.ASSOCIATED(bool2)) .OR. ANY(bool2) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool2,1) /= 10) .OR. (LBOUND(bool2,1) /= 1) .OR. &
          (UBOUND(bool2,2) /= 10) .OR. (LBOUND(bool2,2) /= 1)
      ASSERT(.NOT.test,'dmallocP(bool2,100,100)')

      CALL demallocP(bool2)
      test= ASSOCIATED(bool2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(bool2)')

      CALL demallocP(bool2)
      test= ASSOCIATED(bool2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(bool2)')

      CALL dmalloc0P(bool2,8,-1,-1,8)
      CALL dmalloc0P(bool2,-1,8,8,-1)
      CALL dmalloc0P(bool2,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(bool2)) .OR. ANY(bool2) .OR. &
          (UBOUND(bool2,1) /= 8) .OR. (LBOUND(bool2,1) /= -1) .OR. &
          (UBOUND(bool2,2) /= 8) .OR. (LBOUND(bool2,2) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(bool2,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(bool2,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(bool2)) .OR. ANY(bool2) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool2,1) /= 8) .OR. (LBOUND(bool2,1) /= -1) .OR. &
          (UBOUND(bool2,2) /= 8) .OR. (LBOUND(bool2,2) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(bool2,-1,1,-1,1)')
      CALL demallocP(bool2)
  !
  ! rank 3 variable
      NULLIFY(bool3)
      CALL dmallocP(bool3,-10,10,10)
      CALL dmallocP(bool3,10,-10,10)
      CALL dmallocP(bool3,10,10,-10)
      CALL dmallocP(bool3,10,10,10)
      test=(.NOT.ASSOCIATED(bool3)) .OR. ANY(bool3) .OR. &
          (UBOUND(bool3,1) /= 10) .OR. (LBOUND(bool3,1) /= 1) .OR. &
          (UBOUND(bool3,2) /= 10) .OR. (LBOUND(bool3,2) /= 1) .OR. &
          (UBOUND(bool3,3) /= 10) .OR. (LBOUND(bool3,3) /= 1)
      ASSERT(.NOT.test,'dmallocP(bool3,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(bool3,100,100,100)
      test=(.NOT.ASSOCIATED(bool3)) .OR. ANY(bool3) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool3,1) /= 10) .OR. (LBOUND(bool3,1) /= 1) .OR. &
          (UBOUND(bool3,2) /= 10) .OR. (LBOUND(bool3,2) /= 1) .OR. &
          (UBOUND(bool3,3) /= 10) .OR. (LBOUND(bool3,3) /= 1)
      ASSERT(.NOT.test,'dmallocP(bool3,100,100,100)')

      CALL demallocP(bool3)
      test= ASSOCIATED(bool3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(bool3)')

      CALL demallocP(bool3)
      test= ASSOCIATED(bool3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(bool3)')

      CALL dmalloc0P(bool3,8,-1,-1,8,-1,8)
      CALL dmalloc0P(bool3,-1,8,8,-1,-1,8)
      CALL dmalloc0P(bool3,-1,8,-1,8,8,-1)
      CALL dmalloc0P(bool3,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(bool3)) .OR. ANY(bool3) .OR. &
          (UBOUND(bool3,1) /= 8) .OR. (LBOUND(bool3,1) /= -1) .OR. &
          (UBOUND(bool3,2) /= 8) .OR. (LBOUND(bool3,2) /= -1) .OR. &
          (UBOUND(bool3,3) /= 8) .OR. (LBOUND(bool3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(bool3,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(bool3,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(bool3)) .OR. ANY(bool3) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool3,1) /= 8) .OR. (LBOUND(bool3,1) /= -1) .OR. &
          (UBOUND(bool3,2) /= 8) .OR. (LBOUND(bool3,2) /= -1) .OR. &
          (UBOUND(bool3,3) /= 8) .OR. (LBOUND(bool3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(bool3,-1,1,-1,1,-1,1)')
      CALL demallocP(bool3)
  !
  ! rank 4 variable
      NULLIFY(bool4)
      CALL dmallocP(bool4,-10,10,10,10)
      CALL dmallocP(bool4,10,-10,10,10)
      CALL dmallocP(bool4,10,10,-10,10)
      CALL dmallocP(bool4,10,10,10,-10)
      CALL dmallocP(bool4,10,10,10,10)
      test=(.NOT.ASSOCIATED(bool4)) .OR. ANY(bool4) .OR. &
          (UBOUND(bool4,1) /= 10) .OR. (LBOUND(bool4,1) /= 1) .OR. &
          (UBOUND(bool4,2) /= 10) .OR. (LBOUND(bool4,2) /= 1) .OR. &
          (UBOUND(bool4,3) /= 10) .OR. (LBOUND(bool4,3) /= 1) .OR. &
          (UBOUND(bool4,4) /= 10) .OR. (LBOUND(bool4,4) /= 1)
      ASSERT(.NOT.test,'dmallocP(bool4,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(bool4,100,100,100,100)
      test=(.NOT.ASSOCIATED(bool4)) .OR. ANY(bool4) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool4,1) /= 10) .OR. (LBOUND(bool4,1) /= 1) .OR. &
          (UBOUND(bool4,2) /= 10) .OR. (LBOUND(bool4,2) /= 1) .OR. &
          (UBOUND(bool4,3) /= 10) .OR. (LBOUND(bool4,3) /= 1) .OR. &
          (UBOUND(bool4,4) /= 10) .OR. (LBOUND(bool4,4) /= 1)
      ASSERT(.NOT.test,'dmallocP(bool4,100,100,100,100)')

      CALL demallocP(bool4)
      test= ASSOCIATED(bool4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(bool4)')

      CALL demallocP(bool4)
      test= ASSOCIATED(bool4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(bool4)')

      CALL dmalloc0P(bool4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(bool4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(bool4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(bool4,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(bool4)) .OR. ANY(bool4) .OR. &
          (UBOUND(bool4,1) /= 8) .OR. (LBOUND(bool4,1) /= -1) .OR. &
          (UBOUND(bool4,2) /= 8) .OR. (LBOUND(bool4,2) /= -1) .OR. &
          (UBOUND(bool4,3) /= 8) .OR. (LBOUND(bool4,3) /= -1) .OR. &
          (UBOUND(bool4,4) /= 8) .OR. (LBOUND(bool4,4) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(bool4,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(bool4,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(bool4)) .OR. ANY(bool4) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool4,1) /= 8) .OR. (LBOUND(bool4,1) /= -1) .OR. &
          (UBOUND(bool4,2) /= 8) .OR. (LBOUND(bool4,2) /= -1) .OR. &
          (UBOUND(bool4,3) /= 8) .OR. (LBOUND(bool4,3) /= -1) .OR. &
          (UBOUND(bool4,4) /= 8) .OR. (LBOUND(bool4,4) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(bool4,-1,1,-1,1,-1,1,-1,1)')
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
      test=(.NOT.ASSOCIATED(bool5)) .OR. ANY(bool5) .OR. &
          (UBOUND(bool5,1) /= 10) .OR. (LBOUND(bool5,1) /= 1) .OR. &
          (UBOUND(bool5,2) /= 10) .OR. (LBOUND(bool5,2) /= 1) .OR. &
          (UBOUND(bool5,3) /= 10) .OR. (LBOUND(bool5,3) /= 1) .OR. &
          (UBOUND(bool5,4) /= 10) .OR. (LBOUND(bool5,4) /= 1) .OR. &
          (UBOUND(bool5,5) /= 10) .OR. (LBOUND(bool5,5) /= 1)
      ASSERT(.NOT.test,'dmallocP(bool5,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(bool5,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(bool5)) .OR. ANY(bool5)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool5,1) /= 10) .OR. (LBOUND(bool5,1) /= 1) .OR. &
          (UBOUND(bool5,2) /= 10) .OR. (LBOUND(bool5,2) /= 1) .OR. &
          (UBOUND(bool5,3) /= 10) .OR. (LBOUND(bool5,3) /= 1) .OR. &
          (UBOUND(bool5,4) /= 10) .OR. (LBOUND(bool5,4) /= 1) .OR. &
          (UBOUND(bool5,5) /= 10) .OR. (LBOUND(bool5,5) /= 1)
      ASSERT(.NOT.test,'dmallocP(bool5,100,100,100,100,100)')

      CALL demallocP(bool5)
      test= ASSOCIATED(bool5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(bool5)')

      CALL demallocP(bool5)
      test= ASSOCIATED(bool5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(bool5)')

      CALL dmalloc0P(bool5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(bool5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(bool5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(bool5,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(bool5)) .OR. ANY(bool5) .OR. &
          (UBOUND(bool5,1) /= 8) .OR. (LBOUND(bool5,1) /= -1) .OR. &
          (UBOUND(bool5,2) /= 8) .OR. (LBOUND(bool5,2) /= -1) .OR. &
          (UBOUND(bool5,3) /= 8) .OR. (LBOUND(bool5,3) /= -1) .OR. &
          (UBOUND(bool5,4) /= 8) .OR. (LBOUND(bool5,4) /= -1) .OR. &
          (UBOUND(bool5,5) /= 8) .OR. (LBOUND(bool5,5) /= -1)
      ASSERT(.NOT.test,'dmallocP(bool5,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(bool5,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(bool5)) .OR. ANY(bool5)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool5,1) /= 8) .OR. (LBOUND(bool5,1) /= -1) .OR. &
          (UBOUND(bool5,2) /= 8) .OR. (LBOUND(bool5,2) /= -1) .OR. &
          (UBOUND(bool5,3) /= 8) .OR. (LBOUND(bool5,3) /= -1) .OR. &
          (UBOUND(bool5,4) /= 8) .OR. (LBOUND(bool5,4) /= -1) .OR. &
          (UBOUND(bool5,5) /= 8) .OR. (LBOUND(bool5,5) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(bool5,-1,1,-1,1,-1,1,-1,1,-1,1)')
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
      test=(.NOT.ASSOCIATED(bool6)) .OR. ANY(bool6) .OR. &
          (UBOUND(bool6,1) /= 10) .OR. (LBOUND(bool6,1) /= 1) .OR. &
          (UBOUND(bool6,2) /= 10) .OR. (LBOUND(bool6,2) /= 1) .OR. &
          (UBOUND(bool6,3) /= 10) .OR. (LBOUND(bool6,3) /= 1) .OR. &
          (UBOUND(bool6,4) /= 10) .OR. (LBOUND(bool6,4) /= 1) .OR. &
          (UBOUND(bool6,5) /= 10) .OR. (LBOUND(bool6,5) /= 1) .OR. &
          (UBOUND(bool6,6) /= 10) .OR. (LBOUND(bool6,6) /= 1)
      ASSERT(.NOT.test,'dmallocP(bool6,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(bool6,100,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(bool6)) .OR. ANY(bool6) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool6,1) /= 10) .OR. (LBOUND(bool6,1) /= 1) .OR. &
          (UBOUND(bool6,2) /= 10) .OR. (LBOUND(bool6,2) /= 1) .OR. &
          (UBOUND(bool6,3) /= 10) .OR. (LBOUND(bool6,3) /= 1) .OR. &
          (UBOUND(bool6,4) /= 10) .OR. (LBOUND(bool6,4) /= 1) .OR. &
          (UBOUND(bool6,5) /= 10) .OR. (LBOUND(bool6,5) /= 1) .OR. &
          (UBOUND(bool6,6) /= 10) .OR. (LBOUND(bool6,6) /= 1)
      ASSERT(.NOT.test,'dmallocP(bool6,100,100,100,100,100,100)')

      CALL demallocP(bool6)
      test=ASSOCIATED(bool6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(bool6)')

      CALL demallocP(bool6)
      test= ASSOCIATED(bool6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(bool6)')

      CALL dmalloc0P(bool6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(bool6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(bool6)) .OR. ANY(bool6) .OR. &
          (UBOUND(bool6,1) /= 8) .OR. (LBOUND(bool6,1) /= -1) .OR. &
          (UBOUND(bool6,2) /= 8) .OR. (LBOUND(bool6,2) /= -1) .OR. &
          (UBOUND(bool6,3) /= 8) .OR. (LBOUND(bool6,3) /= -1) .OR. &
          (UBOUND(bool6,4) /= 8) .OR. (LBOUND(bool6,4) /= -1) .OR. &
          (UBOUND(bool6,5) /= 8) .OR. (LBOUND(bool6,5) /= -1) .OR. &
          (UBOUND(bool6,6) /= 8) .OR. (LBOUND(bool6,6) /= -1)
      ASSERT(.NOT.test,'dmallocP(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(bool6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(bool6)) .OR. ANY(bool6)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool6,1) /= 8) .OR. (LBOUND(bool6,1) /= -1) .OR. &
          (UBOUND(bool6,2) /= 8) .OR. (LBOUND(bool6,2) /= -1) .OR. &
          (UBOUND(bool6,3) /= 8) .OR. (LBOUND(bool6,3) /= -1) .OR. &
          (UBOUND(bool6,4) /= 8) .OR. (LBOUND(bool6,4) /= -1) .OR. &
          (UBOUND(bool6,5) /= 8) .OR. (LBOUND(bool6,5) /= -1) .OR. &
          (UBOUND(bool6,6) /= 8) .OR. (LBOUND(bool6,6) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(bool6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')
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
      test=(.NOT.ASSOCIATED(bool7)) .OR. ANY(bool7) .OR. &
          (UBOUND(bool7,1) /= 10) .OR. (LBOUND(bool7,1) /= 1) .OR. &
          (UBOUND(bool7,2) /= 10) .OR. (LBOUND(bool7,2) /= 1) .OR. &
          (UBOUND(bool7,3) /= 10) .OR. (LBOUND(bool7,3) /= 1) .OR. &
          (UBOUND(bool7,4) /= 10) .OR. (LBOUND(bool7,4) /= 1) .OR. &
          (UBOUND(bool7,5) /= 10) .OR. (LBOUND(bool7,5) /= 1) .OR. &
          (UBOUND(bool7,6) /= 10) .OR. (LBOUND(bool7,6) /= 1) .OR. &
          (UBOUND(bool7,7) /= 10) .OR. (LBOUND(bool7,7) /= 1)
      ASSERT(.NOT.test,'dmallocP(bool7,10,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(bool7,100,100,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(bool7)) .OR. ANY(bool7) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool7,1) /= 10) .OR. (LBOUND(bool7,1) /= 1) .OR. &
          (UBOUND(bool7,2) /= 10) .OR. (LBOUND(bool7,2) /= 1) .OR. &
          (UBOUND(bool7,3) /= 10) .OR. (LBOUND(bool7,3) /= 1) .OR. &
          (UBOUND(bool7,4) /= 10) .OR. (LBOUND(bool7,4) /= 1) .OR. &
          (UBOUND(bool7,5) /= 10) .OR. (LBOUND(bool7,5) /= 1) .OR. &
          (UBOUND(bool7,6) /= 10) .OR. (LBOUND(bool7,6) /= 1) .OR. &
          (UBOUND(bool7,7) /= 10) .OR. (LBOUND(bool7,7) /= 1)
      ASSERT(.NOT.test,'dmallocP(bool7,100,100,100,100,100,100,100)')

      CALL demallocP(bool7)
      test= ASSOCIATED(bool7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(bool7)')

      CALL demallocP(bool7)
      test= ASSOCIATED(bool7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(bool7)')

      CALL dmalloc0P(bool7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(bool7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(bool7)) .OR. ANY(bool7) .OR. &
          (UBOUND(bool7,1) /= 8) .OR. (LBOUND(bool7,1) /= -1) .OR. &
          (UBOUND(bool7,2) /= 8) .OR. (LBOUND(bool7,2) /= -1) .OR. &
          (UBOUND(bool7,3) /= 8) .OR. (LBOUND(bool7,3) /= -1) .OR. &
          (UBOUND(bool7,4) /= 8) .OR. (LBOUND(bool7,4) /= -1) .OR. &
          (UBOUND(bool7,5) /= 8) .OR. (LBOUND(bool7,5) /= -1) .OR. &
          (UBOUND(bool7,6) /= 8) .OR. (LBOUND(bool7,6) /= -1) .OR. &
          (UBOUND(bool7,7) /= 8) .OR. (LBOUND(bool7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(bool7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(bool7)) .OR. ANY(bool7)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool7,1) /= 8) .OR. (LBOUND(bool7,1) /= -1) .OR. &
          (UBOUND(bool7,2) /= 8) .OR. (LBOUND(bool7,2) /= -1) .OR. &
          (UBOUND(bool7,3) /= 8) .OR. (LBOUND(bool7,3) /= -1) .OR. &
          (UBOUND(bool7,4) /= 8) .OR. (LBOUND(bool7,4) /= -1) .OR. &
          (UBOUND(bool7,5) /= 8) .OR. (LBOUND(bool7,5) /= -1) .OR. &
          (UBOUND(bool7,6) /= 8) .OR. (LBOUND(bool7,6) /= -1) .OR. &
          (UBOUND(bool7,7) /= 8) .OR. (LBOUND(bool7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(bool7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')
      CALL demallocP(bool7)

    ENDSUBROUTINE testBOOLP
!
!-------------------------------------------------------------------------------
! Test allocation/deallocation for booleans
    SUBROUTINE testBOOLA()
      LOGICAL(SBK),ALLOCATABLE :: bool1(:)
      LOGICAL(SBK),ALLOCATABLE :: bool2(:,:)
      LOGICAL(SBK),ALLOCATABLE :: bool3(:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: bool4(:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: bool5(:,:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: bool6(:,:,:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: bool7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0
!
! rank 1 variable
      CALL dmallocA(bool1,-10)
      CALL dmallocA(bool1,10)
      test=(.NOT.ALLOCATED(bool1)) .OR. ANY(bool1) .OR. &
          (UBOUND(bool1,1) /= 10) .OR. (LBOUND(bool1,1) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool1,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(bool1,100)
      test=(.NOT.ALLOCATED(bool1)) .OR. ANY(bool1) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool1,1) /= 10) .OR. (LBOUND(bool1,1) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool1,100)')

      CALL demallocA(bool1)
      test=ALLOCATED(bool1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool1)')
      CALL demallocA(bool1)

      test=ALLOCATED(bool1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool1)')

      CALL dmalloc0A(bool1,8,-1)
      CALL dmalloc0A(bool1,-1,8)
      test=(.NOT.ALLOCATED(bool1)) .OR. ANY(bool1) .OR. &
          (UBOUND(bool1,1) /= 8) .OR. (LBOUND(bool1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(bool1,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool1,-1,1)
      test=(.NOT.ALLOCATED(bool1)) .OR. ANY(bool1) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool1,1) /= 8) .OR. (LBOUND(bool1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(bool1,-1,1)')
      CALL demallocA(bool1)
!
! rank 2 variable
      CALL dmallocA(bool2,-10,10)
      CALL dmallocA(bool2,10,-10)
      CALL dmallocA(bool2,10,10)
      test=(.NOT.ALLOCATED(bool2)) .OR. ANY(bool2) .OR. &
          (UBOUND(bool2,1) /= 10) .OR. (LBOUND(bool2,1) /= 1) .OR. &
          (UBOUND(bool2,2) /= 10) .OR. (LBOUND(bool2,2) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool2,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(bool2,100,100)
      test=(.NOT.ALLOCATED(bool2)) .OR. ANY(bool2) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool2,1) /= 10) .OR. (LBOUND(bool2,1) /= 1) .OR. &
          (UBOUND(bool2,2) /= 10) .OR. (LBOUND(bool2,2) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool2,100,100)')

      CALL demallocA(bool2)
      test=ALLOCATED(bool2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool2)')

      CALL demallocA(bool2)
      test=ALLOCATED(bool2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool2)')

      CALL dmalloc0A(bool2,8,-1,-1,8)
      CALL dmalloc0A(bool2,-1,8,8,-1)
      CALL dmalloc0A(bool2,-1,8,-1,8)
      test=(.NOT.ALLOCATED(bool2)) .OR. ANY(bool2) .OR. &
          (UBOUND(bool2,1) /= 8) .OR. (LBOUND(bool2,1) /= -1) .OR. &
          (UBOUND(bool2,2) /= 8) .OR. (LBOUND(bool2,2) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(bool2,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool2,-1,1,-1,1)
      test=(.NOT.ALLOCATED(bool2)) .OR. ANY(bool2) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool2,1) /= 8) .OR. (LBOUND(bool2,1) /= -1) .OR. &
          (UBOUND(bool2,2) /= 8) .OR. (LBOUND(bool2,2) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(bool2,-1,1,-1,1)')
      CALL demallocA(bool2)
!
! rank 3 variable
      CALL dmallocA(bool3,-10,10,10)
      CALL dmallocA(bool3,10,-10,10)
      CALL dmallocA(bool3,10,10,-10)
      CALL dmallocA(bool3,10,10,10)
      test=(.NOT.ALLOCATED(bool3)) .OR. ANY(bool3) .OR. &
          (UBOUND(bool3,1) /= 10) .OR. (LBOUND(bool3,1) /= 1) .OR. &
          (UBOUND(bool3,2) /= 10) .OR. (LBOUND(bool3,2) /= 1) .OR. &
          (UBOUND(bool3,3) /= 10) .OR. (LBOUND(bool3,3) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool3,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(bool3,100,100,100)
      test=(.NOT.ALLOCATED(bool3)) .OR. ANY(bool3) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool3,1) /= 10) .OR. (LBOUND(bool3,1) /= 1) .OR. &
          (UBOUND(bool3,2) /= 10) .OR. (LBOUND(bool3,2) /= 1) .OR. &
          (UBOUND(bool3,3) /= 10) .OR. (LBOUND(bool3,3) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool3,100,100,100)')

      CALL demallocA(bool3)
      test=ALLOCATED(bool3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool3)')

      CALL demallocA(bool3)
      test=ALLOCATED(bool3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool3)')

      CALL dmalloc0A(bool3,8,-1,-1,8,-1,8)
      CALL dmalloc0A(bool3,-1,8,8,-1,-1,8)
      CALL dmalloc0A(bool3,-1,8,-1,8,8,-1)
      CALL dmalloc0A(bool3,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(bool3)) .OR. ANY(bool3) .OR. &
          (UBOUND(bool3,1) /= 8) .OR. (LBOUND(bool3,1) /= -1) .OR. &
          (UBOUND(bool3,2) /= 8) .OR. (LBOUND(bool3,2) /= -1) .OR. &
          (UBOUND(bool3,3) /= 8) .OR. (LBOUND(bool3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(bool3,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool3,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(bool3)) .OR. ANY(bool3) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool3,1) /= 8) .OR. (LBOUND(bool3,1) /= -1) .OR. &
          (UBOUND(bool3,2) /= 8) .OR. (LBOUND(bool3,2) /= -1) .OR. &
          (UBOUND(bool3,3) /= 8) .OR. (LBOUND(bool3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(bool3,-1,1,-1,1,-1,1)')
      CALL demallocA(bool3)
!
! rank 4 variable
      CALL dmallocA(bool4,-10,10,10,10)
      CALL dmallocA(bool4,10,-10,10,10)
      CALL dmallocA(bool4,10,10,-10,10)
      CALL dmallocA(bool4,10,10,10,-10)
      CALL dmallocA(bool4,10,10,10,10)
      test=(.NOT.ALLOCATED(bool4)) .OR. ANY(bool4) .OR. &
          (UBOUND(bool4,1) /= 10) .OR. (LBOUND(bool4,1) /= 1) .OR. &
          (UBOUND(bool4,2) /= 10) .OR. (LBOUND(bool4,2) /= 1) .OR. &
          (UBOUND(bool4,3) /= 10) .OR. (LBOUND(bool4,3) /= 1) .OR. &
          (UBOUND(bool4,4) /= 10) .OR. (LBOUND(bool4,4) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool4,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(bool4,100,100,100,100)
      test=(.NOT.ALLOCATED(bool4)) .OR. ANY(bool4) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool4,1) /= 10) .OR. (LBOUND(bool4,1) /= 1) .OR. &
          (UBOUND(bool4,2) /= 10) .OR. (LBOUND(bool4,2) /= 1) .OR. &
          (UBOUND(bool4,3) /= 10) .OR. (LBOUND(bool4,3) /= 1) .OR. &
          (UBOUND(bool4,4) /= 10) .OR. (LBOUND(bool4,4) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool4,100,100,100,100)')

      CALL demallocA(bool4)
      test=ALLOCATED(bool4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool4)')

      CALL demallocA(bool4)
      test=ALLOCATED(bool4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool4)')

      CALL dmalloc0A(bool4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(bool4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(bool4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(bool4,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(bool4)) .OR. ANY(bool4) .OR. &
          (UBOUND(bool4,1) /= 8) .OR. (LBOUND(bool4,1) /= -1) .OR. &
          (UBOUND(bool4,2) /= 8) .OR. (LBOUND(bool4,2) /= -1) .OR. &
          (UBOUND(bool4,3) /= 8) .OR. (LBOUND(bool4,3) /= -1) .OR. &
          (UBOUND(bool4,4) /= 8) .OR. (LBOUND(bool4,4) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(bool4,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool4,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(bool4)) .OR. ANY(bool4) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool4,1) /= 8) .OR. (LBOUND(bool4,1) /= -1) .OR. &
          (UBOUND(bool4,2) /= 8) .OR. (LBOUND(bool4,2) /= -1) .OR. &
          (UBOUND(bool4,3) /= 8) .OR. (LBOUND(bool4,3) /= -1) .OR. &
          (UBOUND(bool4,4) /= 8) .OR. (LBOUND(bool4,4) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(bool4,-1,1,-1,1,-1,1,-1,1)')
      CALL demallocA(bool4)
!
! rank 5 variable
      CALL dmallocA(bool5,-10,10,10,10,10)
      CALL dmallocA(bool5,10,-10,10,10,10)
      CALL dmallocA(bool5,10,10,-10,10,10)
      CALL dmallocA(bool5,10,10,10,-10,10)
      CALL dmallocA(bool5,10,10,10,10,-10)
      CALL dmallocA(bool5,10,10,10,10,10)
      test=(.NOT.ALLOCATED(bool5)) .OR. ANY(bool5) .OR. &
          (UBOUND(bool5,1) /= 10) .OR. (LBOUND(bool5,1) /= 1) .OR. &
          (UBOUND(bool5,2) /= 10) .OR. (LBOUND(bool5,2) /= 1) .OR. &
          (UBOUND(bool5,3) /= 10) .OR. (LBOUND(bool5,3) /= 1) .OR. &
          (UBOUND(bool5,4) /= 10) .OR. (LBOUND(bool5,4) /= 1) .OR. &
          (UBOUND(bool5,5) /= 10) .OR. (LBOUND(bool5,5) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool5,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(bool5,100,100,100,100,100)
      test=(.NOT.ALLOCATED(bool5)) .OR. ANY(bool5)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool5,1) /= 10) .OR. (LBOUND(bool5,1) /= 1) .OR. &
          (UBOUND(bool5,2) /= 10) .OR. (LBOUND(bool5,2) /= 1) .OR. &
          (UBOUND(bool5,3) /= 10) .OR. (LBOUND(bool5,3) /= 1) .OR. &
          (UBOUND(bool5,4) /= 10) .OR. (LBOUND(bool5,4) /= 1) .OR. &
          (UBOUND(bool5,5) /= 10) .OR. (LBOUND(bool5,5) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool5,100,100,100,100,100)')

      CALL demallocA(bool5)
      test=ALLOCATED(bool5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool5)')

      CALL demallocA(bool5)
      test=ALLOCATED(bool5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool5)')

      CALL dmalloc0A(bool5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(bool5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(bool5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(bool5,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(bool5)) .OR. ANY(bool5) .OR. &
          (UBOUND(bool5,1) /= 8) .OR. (LBOUND(bool5,1) /= -1) .OR. &
          (UBOUND(bool5,2) /= 8) .OR. (LBOUND(bool5,2) /= -1) .OR. &
          (UBOUND(bool5,3) /= 8) .OR. (LBOUND(bool5,3) /= -1) .OR. &
          (UBOUND(bool5,4) /= 8) .OR. (LBOUND(bool5,4) /= -1) .OR. &
          (UBOUND(bool5,5) /= 8) .OR. (LBOUND(bool5,5) /= -1)
      ASSERT(.NOT.test,'dmallocA(bool5,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool5,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(bool5)) .OR. ANY(bool5)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool5,1) /= 8) .OR. (LBOUND(bool5,1) /= -1) .OR. &
          (UBOUND(bool5,2) /= 8) .OR. (LBOUND(bool5,2) /= -1) .OR. &
          (UBOUND(bool5,3) /= 8) .OR. (LBOUND(bool5,3) /= -1) .OR. &
          (UBOUND(bool5,4) /= 8) .OR. (LBOUND(bool5,4) /= -1) .OR. &
          (UBOUND(bool5,5) /= 8) .OR. (LBOUND(bool5,5) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(bool5,-1,1,-1,1,-1,1,-1,1,-1,1)')
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
      test=(.NOT.ALLOCATED(bool6)) .OR. ANY(bool6) .OR. &
          (UBOUND(bool6,1) /= 10) .OR. (LBOUND(bool6,1) /= 1) .OR. &
          (UBOUND(bool6,2) /= 10) .OR. (LBOUND(bool6,2) /= 1) .OR. &
          (UBOUND(bool6,3) /= 10) .OR. (LBOUND(bool6,3) /= 1) .OR. &
          (UBOUND(bool6,4) /= 10) .OR. (LBOUND(bool6,4) /= 1) .OR. &
          (UBOUND(bool6,5) /= 10) .OR. (LBOUND(bool6,5) /= 1) .OR. &
          (UBOUND(bool6,6) /= 10) .OR. (LBOUND(bool6,6) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool6,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(bool6,100,100,100,100,100,100)
      test=(.NOT.ALLOCATED(bool6)) .OR. ANY(bool6) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool6,1) /= 10) .OR. (LBOUND(bool6,1) /= 1) .OR. &
          (UBOUND(bool6,2) /= 10) .OR. (LBOUND(bool6,2) /= 1) .OR. &
          (UBOUND(bool6,3) /= 10) .OR. (LBOUND(bool6,3) /= 1) .OR. &
          (UBOUND(bool6,4) /= 10) .OR. (LBOUND(bool6,4) /= 1) .OR. &
          (UBOUND(bool6,5) /= 10) .OR. (LBOUND(bool6,5) /= 1) .OR. &
          (UBOUND(bool6,6) /= 10) .OR. (LBOUND(bool6,6) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool6,100,100,100,100,100,100)')

      CALL demallocA(bool6)
      test=ALLOCATED(bool6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool6)')

      CALL demallocA(bool6)
      test=ALLOCATED(bool6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool6)')

      CALL dmalloc0A(bool6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(bool6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(bool6)) .OR. ANY(bool6) .OR. &
          (UBOUND(bool6,1) /= 8) .OR. (LBOUND(bool6,1) /= -1) .OR. &
          (UBOUND(bool6,2) /= 8) .OR. (LBOUND(bool6,2) /= -1) .OR. &
          (UBOUND(bool6,3) /= 8) .OR. (LBOUND(bool6,3) /= -1) .OR. &
          (UBOUND(bool6,4) /= 8) .OR. (LBOUND(bool6,4) /= -1) .OR. &
          (UBOUND(bool6,5) /= 8) .OR. (LBOUND(bool6,5) /= -1) .OR. &
          (UBOUND(bool6,6) /= 8) .OR. (LBOUND(bool6,6) /= -1)
      ASSERT(.NOT.test,'dmallocA(bool6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(bool6)) .OR. ANY(bool6)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool6,1) /= 8) .OR. (LBOUND(bool6,1) /= -1) .OR. &
          (UBOUND(bool6,2) /= 8) .OR. (LBOUND(bool6,2) /= -1) .OR. &
          (UBOUND(bool6,3) /= 8) .OR. (LBOUND(bool6,3) /= -1) .OR. &
          (UBOUND(bool6,4) /= 8) .OR. (LBOUND(bool6,4) /= -1) .OR. &
          (UBOUND(bool6,5) /= 8) .OR. (LBOUND(bool6,5) /= -1) .OR. &
          (UBOUND(bool6,6) /= 8) .OR. (LBOUND(bool6,6) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(bool6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')
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
      test=(.NOT.ALLOCATED(bool7)) .OR. ANY(bool7) .OR. &
          (UBOUND(bool7,1) /= 10) .OR. (LBOUND(bool7,1) /= 1) .OR. &
          (UBOUND(bool7,2) /= 10) .OR. (LBOUND(bool7,2) /= 1) .OR. &
          (UBOUND(bool7,3) /= 10) .OR. (LBOUND(bool7,3) /= 1) .OR. &
          (UBOUND(bool7,4) /= 10) .OR. (LBOUND(bool7,4) /= 1) .OR. &
          (UBOUND(bool7,5) /= 10) .OR. (LBOUND(bool7,5) /= 1) .OR. &
          (UBOUND(bool7,6) /= 10) .OR. (LBOUND(bool7,6) /= 1) .OR. &
          (UBOUND(bool7,7) /= 10) .OR. (LBOUND(bool7,7) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool7,10,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(bool7,100,100,100,100,100,100,100)
      test=(.NOT.ALLOCATED(bool7)) .OR. ANY(bool7) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool7,1) /= 10) .OR. (LBOUND(bool7,1) /= 1) .OR. &
          (UBOUND(bool7,2) /= 10) .OR. (LBOUND(bool7,2) /= 1) .OR. &
          (UBOUND(bool7,3) /= 10) .OR. (LBOUND(bool7,3) /= 1) .OR. &
          (UBOUND(bool7,4) /= 10) .OR. (LBOUND(bool7,4) /= 1) .OR. &
          (UBOUND(bool7,5) /= 10) .OR. (LBOUND(bool7,5) /= 1) .OR. &
          (UBOUND(bool7,6) /= 10) .OR. (LBOUND(bool7,6) /= 1) .OR. &
          (UBOUND(bool7,7) /= 10) .OR. (LBOUND(bool7,7) /= 1)
      ASSERT(.NOT.test,'dmallocA(bool7,100,100,100,100,100,100,100)')

      CALL demallocA(bool7)
      test=ALLOCATED(bool7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool7)')

      CALL demallocA(bool7)
      test=ALLOCATED(bool7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(bool7)')

      CALL dmalloc0A(bool7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(bool7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(bool7)) .OR. ANY(bool7) .OR. &
          (UBOUND(bool7,1) /= 8) .OR. (LBOUND(bool7,1) /= -1) .OR. &
          (UBOUND(bool7,2) /= 8) .OR. (LBOUND(bool7,2) /= -1) .OR. &
          (UBOUND(bool7,3) /= 8) .OR. (LBOUND(bool7,3) /= -1) .OR. &
          (UBOUND(bool7,4) /= 8) .OR. (LBOUND(bool7,4) /= -1) .OR. &
          (UBOUND(bool7,5) /= 8) .OR. (LBOUND(bool7,5) /= -1) .OR. &
          (UBOUND(bool7,6) /= 8) .OR. (LBOUND(bool7,6) /= -1) .OR. &
          (UBOUND(bool7,7) /= 8) .OR. (LBOUND(bool7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(bool7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(bool7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(bool7)) .OR. ANY(bool7)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(bool7,1) /= 8) .OR. (LBOUND(bool7,1) /= -1) .OR. &
          (UBOUND(bool7,2) /= 8) .OR. (LBOUND(bool7,2) /= -1) .OR. &
          (UBOUND(bool7,3) /= 8) .OR. (LBOUND(bool7,3) /= -1) .OR. &
          (UBOUND(bool7,4) /= 8) .OR. (LBOUND(bool7,4) /= -1) .OR. &
          (UBOUND(bool7,5) /= 8) .OR. (LBOUND(bool7,5) /= -1) .OR. &
          (UBOUND(bool7,6) /= 8) .OR. (LBOUND(bool7,6) /= -1) .OR. &
          (UBOUND(bool7,7) /= 8) .OR. (LBOUND(bool7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(bool7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')
      CALL demallocA(bool7)

    ENDSUBROUTINE testBOOLA
!
!-------------------------------------------------------------------------------
! Test allocation/deallocation for integers
    SUBROUTINE testINTP()
      INTEGER(SNK),POINTER :: int_1(:)
      INTEGER(SNK),POINTER :: int_2(:,:)
      INTEGER(SNK),POINTER :: int_3(:,:,:)
      INTEGER(SNK),POINTER :: int_4(:,:,:,:)
      INTEGER(SNK),POINTER :: int_5(:,:,:,:,:)
      INTEGER(SNK),POINTER :: int_6(:,:,:,:,:,:)
      INTEGER(SNK),POINTER :: int_7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0

      NULLIFY(int_1,int_2,int_3,int_4,int_5,int_6,int_7)
!
! rank 1 variable
      CALL dmallocP(int_1,-10)
      CALL dmallocP(int_1,10)
      test=(.NOT.ASSOCIATED(int_1)) .OR. ANY(int_1 /= 0) .OR. &
          (UBOUND(int_1,DIM=1) /= 10) .OR. (LBOUND(int_1,DIM=1) /= 1)
      ASSERT(.NOT.test,'dmallocP(int_1,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(int_1,100)
      test=(.NOT.ASSOCIATED(int_1)) .OR. ANY(int_1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_1,1) /= 10) .OR. (LBOUND(int_1,1) /= 1)
      ASSERT(.NOT.test,'dmallocP(int_1,100)')

      CALL demallocP(int_1)
      test=ASSOCIATED(int_1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_1)')

      CALL demallocP(int_1)
      test=ASSOCIATED(int_1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_1)')

      CALL dmalloc0P(int_1,8,-1)
      CALL dmalloc0P(int_1,-1,8)
      test=(.NOT.ASSOCIATED(int_1)) .OR. ANY(int_1 /= 0) .OR. &
          (UBOUND(int_1,1) /= 8) .OR. (LBOUND(int_1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(int_1,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_1,-1,1)
      test=(.NOT.ASSOCIATED(int_1)) .OR. ANY(int_1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_1,1) /= 8) .OR. (LBOUND(int_1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(int_1,-1,1)')
      CALL demallocP(int_1)
!
! rank 2 variable
      CALL dmallocP(int_2,-10,10)
      CALL dmallocP(int_2,10,-10)
      CALL dmallocP(int_2,10,10)
      test=(.NOT.ASSOCIATED(int_2)) .OR. ANY(int_2 /= 0) .OR. &
          (UBOUND(int_2,1) /= 10) .OR. (LBOUND(int_2,1) /= 1) .OR. &
          (UBOUND(int_2,2) /= 10) .OR. (LBOUND(int_2,2) /= 1)
      ASSERT(.NOT.test,'dmallocP(int_2,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(int_2,100,100)
      test=(.NOT.ASSOCIATED(int_2)) .OR. ANY(int_2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_2,1) /= 10) .OR. (LBOUND(int_2,1) /= 1) .OR. &
          (UBOUND(int_2,2) /= 10) .OR. (LBOUND(int_2,2) /= 1)
      ASSERT(.NOT.test,'dmallocP(int_2,100,100)')

      CALL demallocP(int_2)
      test=ASSOCIATED(int_2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_2)')

      CALL demallocP(int_2)
      test=ASSOCIATED(int_2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_2)')

      CALL dmalloc0P(int_2,8,-1,-1,8)
      CALL dmalloc0P(int_2,-1,8,8,-1)
      CALL dmalloc0P(int_2,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(int_2)) .OR. ANY(int_2 /= 0) .OR. &
          (UBOUND(int_2,1) /= 8) .OR. (LBOUND(int_2,1) /= -1) .OR. &
          (UBOUND(int_2,2) /= 8) .OR. (LBOUND(int_2,2) /= -1)
      ASSERT(.NOT.test,'CALL CALL dmalloc0P(int_2,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_2,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(int_2)) .OR. ANY(int_2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_2,1) /= 8) .OR. (LBOUND(int_2,1) /= -1) .OR. &
          (UBOUND(int_2,2) /= 8) .OR. (LBOUND(int_2,2) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(int_2,-1,1,-1,1)')
      CALL demallocP(int_2)
!
! rank 3 variable
      CALL dmallocP(int_3,-10,10,10)
      CALL dmallocP(int_3,10,-10,10)
      CALL dmallocP(int_3,10,10,-10)
      CALL dmallocP(int_3,10,10,10)
      test=(.NOT.ASSOCIATED(int_3)) .OR. ANY(int_3 /= 0) .OR. &
          (UBOUND(int_3,1) /= 10) .OR. (LBOUND(int_3,1) /= 1) .OR. &
          (UBOUND(int_3,2) /= 10) .OR. (LBOUND(int_3,2) /= 1) .OR. &
          (UBOUND(int_3,3) /= 10) .OR. (LBOUND(int_3,3) /= 1)
      ASSERT(.NOT.test,'dmallocP(int_3,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(int_3,100,100,100)
      test=(.NOT.ASSOCIATED(int_3)) .OR. ANY(int_3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_3,1) /= 10) .OR. (LBOUND(int_3,1) /= 1) .OR. &
          (UBOUND(int_3,2) /= 10) .OR. (LBOUND(int_3,2) /= 1) .OR. &
          (UBOUND(int_3,3) /= 10) .OR. (LBOUND(int_3,3) /= 1)
      ASSERT(.NOT.test,'dmallocP(int_3,100,100,100)')

      CALL demallocP(int_3)
      test=ASSOCIATED(int_3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_3)')

      CALL demallocP(int_3)
      test=ASSOCIATED(int_3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_3)')

      CALL dmalloc0P(int_3,8,-1,-1,8,-1,8)
      CALL dmalloc0P(int_3,-1,8,8,-1,-1,8)
      CALL dmalloc0P(int_3,-1,8,-1,8,8,-1)
      CALL dmalloc0P(int_3,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(int_3)) .OR. ANY(int_3 /= 0) .OR. &
          (UBOUND(int_3,1) /= 8) .OR. (LBOUND(int_3,1) /= -1) .OR. &
          (UBOUND(int_3,2) /= 8) .OR. (LBOUND(int_3,2) /= -1) .OR. &
          (UBOUND(int_3,3) /= 8) .OR. (LBOUND(int_3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(int_3,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_3,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(int_3)) .OR. ANY(int_3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_3,1) /= 8) .OR. (LBOUND(int_3,1) /= -1) .OR. &
          (UBOUND(int_3,2) /= 8) .OR. (LBOUND(int_3,2) /= -1) .OR. &
          (UBOUND(int_3,3) /= 8) .OR. (LBOUND(int_3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(int_3,-1,1,-1,1,-1,1)')
      CALL demallocP(int_3)
!
! rank 4 variable
      CALL dmallocP(int_4,-10,10,10,10)
      CALL dmallocP(int_4,10,-10,10,10)
      CALL dmallocP(int_4,10,10,-10,10)
      CALL dmallocP(int_4,10,10,10,-10)
      CALL dmallocP(int_4,10,10,10,10)
      test=(.NOT.ASSOCIATED(int_4)) .OR. ANY(int_4 /= 0) .OR. &
          (UBOUND(int_4,1) /= 10) .OR. (LBOUND(int_4,1) /= 1) .OR. &
          (UBOUND(int_4,2) /= 10) .OR. (LBOUND(int_4,2) /= 1) .OR. &
          (UBOUND(int_4,3) /= 10) .OR. (LBOUND(int_4,3) /= 1) .OR. &
          (UBOUND(int_4,4) /= 10) .OR. (LBOUND(int_4,4) /= 1)
      ASSERT(.NOT.test,'dmallocP(int_4,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(int_4,100,100,100,100)
      test=(.NOT.ASSOCIATED(int_4)) .OR. ANY(int_4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_4,1) /= 10) .OR. (LBOUND(int_4,1) /= 1) .OR. &
          (UBOUND(int_4,2) /= 10) .OR. (LBOUND(int_4,2) /= 1) .OR. &
          (UBOUND(int_4,3) /= 10) .OR. (LBOUND(int_4,3) /= 1) .OR. &
          (UBOUND(int_4,4) /= 10) .OR. (LBOUND(int_4,4) /= 1)
      ASSERT(.NOT.test,'Rednundant CALL dmallocP(int_4,100,100,100,100)')

      CALL demallocP(int_4)
      test=ASSOCIATED(int_4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_4)')

      CALL demallocP(int_4)
      test=ASSOCIATED(int_4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_4)')

      CALL dmalloc0P(int_4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(int_4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(int_4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(int_4,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(int_4)) .OR. ANY(int_4 /= 0) .OR. &
          (UBOUND(int_4,1) /= 8) .OR. (LBOUND(int_4,1) /= -1) .OR. &
          (UBOUND(int_4,2) /= 8) .OR. (LBOUND(int_4,2) /= -1) .OR. &
          (UBOUND(int_4,3) /= 8) .OR. (LBOUND(int_4,3) /= -1) .OR. &
          (UBOUND(int_4,4) /= 8) .OR. (LBOUND(int_4,4) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(int_4,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_4,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(int_4)) .OR. ANY(int_4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_4,1) /= 8) .OR. (LBOUND(int_4,1) /= -1) .OR. &
          (UBOUND(int_4,2) /= 8) .OR. (LBOUND(int_4,2) /= -1) .OR. &
          (UBOUND(int_4,3) /= 8) .OR. (LBOUND(int_4,3) /= -1) .OR. &
          (UBOUND(int_4,4) /= 8) .OR. (LBOUND(int_4,4) /= -1)
      ASSERT(.NOT.test,'Rednundant CALL dmalloc0P(int_4,-1,1,-1,1,-1,1,-1,1)')
      CALL demallocP(int_4)
!
! rank 5 variable
      CALL dmallocP(int_5,-10,10,10,10,10)
      CALL dmallocP(int_5,10,-10,10,10,10)
      CALL dmallocP(int_5,10,10,-10,10,10)
      CALL dmallocP(int_5,10,10,10,-10,10)
      CALL dmallocP(int_5,10,10,10,10,-10)
      CALL dmallocP(int_5,10,10,10,10,10)
      test=(.NOT.ASSOCIATED(int_5)) .OR. ANY(int_5 /= 0) .OR. &
          (UBOUND(int_5,1) /= 10) .OR. (LBOUND(int_5,1) /= 1) .OR. &
          (UBOUND(int_5,2) /= 10) .OR. (LBOUND(int_5,2) /= 1) .OR. &
          (UBOUND(int_5,3) /= 10) .OR. (LBOUND(int_5,3) /= 1) .OR. &
          (UBOUND(int_5,4) /= 10) .OR. (LBOUND(int_5,4) /= 1) .OR. &
          (UBOUND(int_5,5) /= 10) .OR. (LBOUND(int_5,5) /= 1)
      ASSERT(.NOT.test,'dmallocP(int_5,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(int_5,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(int_5)) .OR. ANY(int_5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_5,1) /= 10) .OR. (LBOUND(int_5,1) /= 1) .OR. &
          (UBOUND(int_5,2) /= 10) .OR. (LBOUND(int_5,2) /= 1) .OR. &
          (UBOUND(int_5,3) /= 10) .OR. (LBOUND(int_5,3) /= 1) .OR. &
          (UBOUND(int_5,4) /= 10) .OR. (LBOUND(int_5,4) /= 1) .OR. &
          (UBOUND(int_5,5) /= 10) .OR. (LBOUND(int_5,5) /= 1)
      ASSERT(.NOT.test,'dmallocP(int_5,100,100,100,100,100)')

      CALL demallocP(int_5)
      test=ASSOCIATED(int_5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_5)')

      CALL demallocP(int_5)
      test=ASSOCIATED(int_5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_5)')

      CALL dmalloc0P(int_5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(int_5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(int_5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(int_5,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(int_5)) .OR. ANY(int_5 /= 0) .OR. &
          (UBOUND(int_5,1) /= 8) .OR. (LBOUND(int_5,1) /= -1) .OR. &
          (UBOUND(int_5,2) /= 8) .OR. (LBOUND(int_5,2) /= -1) .OR. &
          (UBOUND(int_5,3) /= 8) .OR. (LBOUND(int_5,3) /= -1) .OR. &
          (UBOUND(int_5,4) /= 8) .OR. (LBOUND(int_5,4) /= -1) .OR. &
          (UBOUND(int_5,5) /= 8) .OR. (LBOUND(int_5,5) /= -1)
      ASSERT(.NOT.test,'dmallocP(int_5,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_5,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(int_5)) .OR. ANY(int_5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_5,1) /= 8) .OR. (LBOUND(int_5,1) /= -1) .OR. &
          (UBOUND(int_5,2) /= 8) .OR. (LBOUND(int_5,2) /= -1) .OR. &
          (UBOUND(int_5,3) /= 8) .OR. (LBOUND(int_5,3) /= -1) .OR. &
          (UBOUND(int_5,4) /= 8) .OR. (LBOUND(int_5,4) /= -1) .OR. &
          (UBOUND(int_5,5) /= 8) .OR. (LBOUND(int_5,5) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(int_5,-1,1,-1,1,-1,1,-1,1,-1,1)')
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
      test=(.NOT.ASSOCIATED(int_6)) .OR. ANY(int_6 /= 0) .OR. &
          (UBOUND(int_6,1) /= 10) .OR. (LBOUND(int_6,1) /= 1) .OR. &
          (UBOUND(int_6,2) /= 10) .OR. (LBOUND(int_6,2) /= 1) .OR. &
          (UBOUND(int_6,3) /= 10) .OR. (LBOUND(int_6,3) /= 1) .OR. &
          (UBOUND(int_6,4) /= 10) .OR. (LBOUND(int_6,4) /= 1) .OR. &
          (UBOUND(int_6,5) /= 10) .OR. (LBOUND(int_6,5) /= 1) .OR. &
          (UBOUND(int_6,6) /= 10) .OR. (LBOUND(int_6,6) /= 1)
      ASSERT(.NOT.test,'dmallocP(int_6,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(int_6,100,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(int_6)) .OR. ANY(int_6 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_6,1) /= 10) .OR. (LBOUND(int_6,1) /= 1) .OR. &
          (UBOUND(int_6,2) /= 10) .OR. (LBOUND(int_6,2) /= 1) .OR. &
          (UBOUND(int_6,3) /= 10) .OR. (LBOUND(int_6,3) /= 1) .OR. &
          (UBOUND(int_6,4) /= 10) .OR. (LBOUND(int_6,4) /= 1) .OR. &
          (UBOUND(int_6,5) /= 10) .OR. (LBOUND(int_6,5) /= 1) .OR. &
          (UBOUND(int_6,6) /= 10) .OR. (LBOUND(int_6,6) /= 1)
      ASSERT(.NOT.test,'dmallocP(int_6,100,100,100,100,100,100)')

      CALL demallocP(int_6)
      test=ASSOCIATED(int_6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_6)')

      CALL demallocP(int_6)
      test=ASSOCIATED(int_6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_6)')

      CALL dmalloc0P(int_6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(int_6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(int_6)) .OR. ANY(int_6 /= 0) .OR. &
          (UBOUND(int_6,1) /= 8) .OR. (LBOUND(int_6,1) /= -1) .OR. &
          (UBOUND(int_6,2) /= 8) .OR. (LBOUND(int_6,2) /= -1) .OR. &
          (UBOUND(int_6,3) /= 8) .OR. (LBOUND(int_6,3) /= -1) .OR. &
          (UBOUND(int_6,4) /= 8) .OR. (LBOUND(int_6,4) /= -1) .OR. &
          (UBOUND(int_6,5) /= 8) .OR. (LBOUND(int_6,5) /= -1) .OR. &
          (UBOUND(int_6,6) /= 8) .OR. (LBOUND(int_6,6) /= -1)
      ASSERT(.NOT.test,'dmallocP(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(int_6)) .OR. ANY(int_6 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_6,1) /= 8) .OR. (LBOUND(int_6,1) /= -1) .OR. &
          (UBOUND(int_6,2) /= 8) .OR. (LBOUND(int_6,2) /= -1) .OR. &
          (UBOUND(int_6,3) /= 8) .OR. (LBOUND(int_6,3) /= -1) .OR. &
          (UBOUND(int_6,4) /= 8) .OR. (LBOUND(int_6,4) /= -1) .OR. &
          (UBOUND(int_6,5) /= 8) .OR. (LBOUND(int_6,5) /= -1) .OR. &
          (UBOUND(int_6,6) /= 8) .OR. (LBOUND(int_6,6) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(int_6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')
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
      test=(.NOT.ASSOCIATED(int_7)) .OR. ANY(int_7 /= 0) .OR. &
          (UBOUND(int_7,1) /= 10) .OR. (LBOUND(int_7,1) /= 1) .OR. &
          (UBOUND(int_7,2) /= 10) .OR. (LBOUND(int_7,2) /= 1) .OR. &
          (UBOUND(int_7,3) /= 10) .OR. (LBOUND(int_7,3) /= 1) .OR. &
          (UBOUND(int_7,4) /= 10) .OR. (LBOUND(int_7,4) /= 1) .OR. &
          (UBOUND(int_7,5) /= 10) .OR. (LBOUND(int_7,5) /= 1) .OR. &
          (UBOUND(int_7,6) /= 10) .OR. (LBOUND(int_7,6) /= 1) .OR. &
          (UBOUND(int_7,7) /= 10) .OR. (LBOUND(int_7,7) /= 1)
      ASSERT(.NOT.test,'dmallocP(int_7,10,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(int_7,100,100,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(int_7)) .OR. ANY(int_7 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_7,1) /= 10) .OR. (LBOUND(int_7,1) /= 1) .OR. &
          (UBOUND(int_7,2) /= 10) .OR. (LBOUND(int_7,2) /= 1) .OR. &
          (UBOUND(int_7,3) /= 10) .OR. (LBOUND(int_7,3) /= 1) .OR. &
          (UBOUND(int_7,4) /= 10) .OR. (LBOUND(int_7,4) /= 1) .OR. &
          (UBOUND(int_7,5) /= 10) .OR. (LBOUND(int_7,5) /= 1) .OR. &
          (UBOUND(int_7,6) /= 10) .OR. (LBOUND(int_7,6) /= 1) .OR. &
          (UBOUND(int_7,7) /= 10) .OR. (LBOUND(int_7,7) /= 1)
      ASSERT(.NOT.test,'dmallocP(int_7,100,100,100,100,100,100,100)')

      CALL demallocP(int_7)
      test=ASSOCIATED(int_7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_7)')

      CALL demallocP(int_7)
      test=ASSOCIATED(int_7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(int_7)')

      CALL dmalloc0P(int_7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(int_7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(int_7)) .OR. ANY(int_7 /= 0) .OR. &
          (UBOUND(int_7,1) /= 8) .OR. (LBOUND(int_7,1) /= -1) .OR. &
          (UBOUND(int_7,2) /= 8) .OR. (LBOUND(int_7,2) /= -1) .OR. &
          (UBOUND(int_7,3) /= 8) .OR. (LBOUND(int_7,3) /= -1) .OR. &
          (UBOUND(int_7,4) /= 8) .OR. (LBOUND(int_7,4) /= -1) .OR. &
          (UBOUND(int_7,5) /= 8) .OR. (LBOUND(int_7,5) /= -1) .OR. &
          (UBOUND(int_7,6) /= 8) .OR. (LBOUND(int_7,6) /= -1) .OR. &
          (UBOUND(int_7,7) /= 8) .OR. (LBOUND(int_7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(int_7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(int_7)) .OR. ANY(int_7 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_7,1) /= 8) .OR. (LBOUND(int_7,1) /= -1) .OR. &
          (UBOUND(int_7,2) /= 8) .OR. (LBOUND(int_7,2) /= -1) .OR. &
          (UBOUND(int_7,3) /= 8) .OR. (LBOUND(int_7,3) /= -1) .OR. &
          (UBOUND(int_7,4) /= 8) .OR. (LBOUND(int_7,4) /= -1) .OR. &
          (UBOUND(int_7,5) /= 8) .OR. (LBOUND(int_7,5) /= -1) .OR. &
          (UBOUND(int_7,6) /= 8) .OR. (LBOUND(int_7,6) /= -1) .OR. &
          (UBOUND(int_7,7) /= 8) .OR. (LBOUND(int_7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(int_7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')
      CALL demallocP(int_7)

    ENDSUBROUTINE testINTP
!
!-------------------------------------------------------------------------------
! Test allocation/deallocation for integers
    SUBROUTINE testINTA()
      INTEGER(SNK),ALLOCATABLE :: int_1(:)
      INTEGER(SNK),ALLOCATABLE :: int_2(:,:)
      INTEGER(SNK),ALLOCATABLE :: int_3(:,:,:)
      INTEGER(SNK),ALLOCATABLE :: int_4(:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: int_5(:,:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: int_6(:,:,:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: int_7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0
!
! rank 1 variable
      CALL dmallocA(int_1,-10)
      CALL dmallocA(int_1,10)
      test=(.NOT.ALLOCATED(int_1)) .OR. ANY(int_1 /= 0) .OR. &
          (UBOUND(int_1,1) /= 10) .OR. (LBOUND(int_1,1) /= 1)
      ASSERT(.NOT.test,'dmallocA(int_1,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(int_1,100)
      test=(.NOT.ALLOCATED(int_1)) .OR. ANY(int_1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_1,1) /= 10) .OR. (LBOUND(int_1,1) /= 1)
      ASSERT(.NOT.test,'dmallocA(int_1,100)')

      CALL demallocA(int_1)
      test=ALLOCATED(int_1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_1)')

      CALL demallocA(int_1)
      test=ALLOCATED(int_1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_1)')

      CALL dmalloc0A(int_1,8,-1)
      CALL dmalloc0A(int_1,-1,8)
      test=(.NOT.ALLOCATED(int_1)) .OR. ANY(int_1 /= 0) .OR. &
          (UBOUND(int_1,1) /= 8) .OR. (LBOUND(int_1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(int_1,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_1,-1,1)
      test=(.NOT.ALLOCATED(int_1)) .OR. ANY(int_1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_1,1) /= 8) .OR. (LBOUND(int_1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(int_1,-1,1)')
      CALL demallocA(int_1)
!
! rank 2 variable
      CALL dmallocA(int_2,-10,10)
      CALL dmallocA(int_2,10,-10)
      CALL dmallocA(int_2,10,10)
      test=(.NOT.ALLOCATED(int_2)) .OR. ANY(int_2 /= 0) .OR. &
          (UBOUND(int_2,1) /= 10) .OR. (LBOUND(int_2,1) /= 1) .OR. &
          (UBOUND(int_2,2) /= 10) .OR. (LBOUND(int_2,2) /= 1)
      ASSERT(.NOT.test,'dmallocA(int_2,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(int_2,100,100)
      test=(.NOT.ALLOCATED(int_2)) .OR. ANY(int_2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_2,1) /= 10) .OR. (LBOUND(int_2,1) /= 1) .OR. &
          (UBOUND(int_2,2) /= 10) .OR. (LBOUND(int_2,2) /= 1)
      ASSERT(.NOT.test,'dmallocA(int_2,100,100)')

      CALL demallocA(int_2)
      test=ALLOCATED(int_2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_2)')

      CALL demallocA(int_2)
      test=ALLOCATED(int_2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_2)')

      CALL dmalloc0A(int_2,8,-1,-1,8)
      CALL dmalloc0A(int_2,-1,8,8,-1)
      CALL dmalloc0A(int_2,-1,8,-1,8)
      test=(.NOT.ALLOCATED(int_2)) .OR. ANY(int_2 /= 0) .OR. &
          (UBOUND(int_2,1) /= 8) .OR. (LBOUND(int_2,1) /= -1) .OR. &
          (UBOUND(int_2,2) /= 8) .OR. (LBOUND(int_2,2) /= -1)
      ASSERT(.NOT.test,'CALL CALL dmalloc0A(int_2,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_2,-1,1,-1,1)
      test=(.NOT.ALLOCATED(int_2)) .OR. ANY(int_2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_2,1) /= 8) .OR. (LBOUND(int_2,1) /= -1) .OR. &
          (UBOUND(int_2,2) /= 8) .OR. (LBOUND(int_2,2) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(int_2,-1,1,-1,1)')
      CALL demallocA(int_2)
!
! rank 3 variable
      CALL dmallocA(int_3,-10,10,10)
      CALL dmallocA(int_3,10,-10,10)
      CALL dmallocA(int_3,10,10,-10)
      CALL dmallocA(int_3,10,10,10)
      test=(.NOT.ALLOCATED(int_3)) .OR. ANY(int_3 /= 0) .OR. &
          (UBOUND(int_3,1) /= 10) .OR. (LBOUND(int_3,1) /= 1) .OR. &
          (UBOUND(int_3,2) /= 10) .OR. (LBOUND(int_3,2) /= 1) .OR. &
          (UBOUND(int_3,3) /= 10) .OR. (LBOUND(int_3,3) /= 1)
      ASSERT(.NOT.test,'dmallocA(int_3,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(int_3,100,100,100)
      test=(.NOT.ALLOCATED(int_3)) .OR. ANY(int_3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_3,1) /= 10) .OR. (LBOUND(int_3,1) /= 1) .OR. &
          (UBOUND(int_3,2) /= 10) .OR. (LBOUND(int_3,2) /= 1) .OR. &
          (UBOUND(int_3,3) /= 10) .OR. (LBOUND(int_3,3) /= 1)
      ASSERT(.NOT.test,'dmallocA(int_3,100,100,100)')

      CALL demallocA(int_3)
      test=ALLOCATED(int_3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_3)')

      CALL demallocA(int_3)
      test=ALLOCATED(int_3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_3)')

      CALL dmalloc0A(int_3,8,-1,-1,8,-1,8)
      CALL dmalloc0A(int_3,-1,8,8,-1,-1,8)
      CALL dmalloc0A(int_3,-1,8,-1,8,8,-1)
      CALL dmalloc0A(int_3,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(int_3)) .OR. ANY(int_3 /= 0) .OR. &
          (UBOUND(int_3,1) /= 8) .OR. (LBOUND(int_3,1) /= -1) .OR. &
          (UBOUND(int_3,2) /= 8) .OR. (LBOUND(int_3,2) /= -1) .OR. &
          (UBOUND(int_3,3) /= 8) .OR. (LBOUND(int_3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(int_3,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_3,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(int_3)) .OR. ANY(int_3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_3,1) /= 8) .OR. (LBOUND(int_3,1) /= -1) .OR. &
          (UBOUND(int_3,2) /= 8) .OR. (LBOUND(int_3,2) /= -1) .OR. &
          (UBOUND(int_3,3) /= 8) .OR. (LBOUND(int_3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(int_3,-1,1,-1,1,-1,1)')
      CALL demallocA(int_3)
!
! rank 4 variable
      CALL dmallocA(int_4,-10,10,10,10)
      CALL dmallocA(int_4,10,-10,10,10)
      CALL dmallocA(int_4,10,10,-10,10)
      CALL dmallocA(int_4,10,10,10,-10)
      CALL dmallocA(int_4,10,10,10,10)
      test=(.NOT.ALLOCATED(int_4)) .OR. ANY(int_4 /= 0) .OR. &
          (UBOUND(int_4,1) /= 10) .OR. (LBOUND(int_4,1) /= 1) .OR. &
          (UBOUND(int_4,2) /= 10) .OR. (LBOUND(int_4,2) /= 1) .OR. &
          (UBOUND(int_4,3) /= 10) .OR. (LBOUND(int_4,3) /= 1) .OR. &
          (UBOUND(int_4,4) /= 10) .OR. (LBOUND(int_4,4) /= 1)
      ASSERT(.NOT.test,'dmallocA(int_4,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(int_4,100,100,100,100)
      test=(.NOT.ALLOCATED(int_4)) .OR. ANY(int_4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_4,1) /= 10) .OR. (LBOUND(int_4,1) /= 1) .OR. &
          (UBOUND(int_4,2) /= 10) .OR. (LBOUND(int_4,2) /= 1) .OR. &
          (UBOUND(int_4,3) /= 10) .OR. (LBOUND(int_4,3) /= 1) .OR. &
          (UBOUND(int_4,4) /= 10) .OR. (LBOUND(int_4,4) /= 1)
      ASSERT(.NOT.test,'Rednundant CALL dmallocA(int_4,100,100,100,100)')

      CALL demallocA(int_4)
      test=ALLOCATED(int_4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_4)')

      CALL demallocA(int_4)
      test=ALLOCATED(int_4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_4)')

      CALL dmalloc0A(int_4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(int_4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(int_4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(int_4,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(int_4)) .OR. ANY(int_4 /= 0) .OR. &
          (UBOUND(int_4,1) /= 8) .OR. (LBOUND(int_4,1) /= -1) .OR. &
          (UBOUND(int_4,2) /= 8) .OR. (LBOUND(int_4,2) /= -1) .OR. &
          (UBOUND(int_4,3) /= 8) .OR. (LBOUND(int_4,3) /= -1) .OR. &
          (UBOUND(int_4,4) /= 8) .OR. (LBOUND(int_4,4) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(int_4,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_4,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(int_4)) .OR. ANY(int_4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_4,1) /= 8) .OR. (LBOUND(int_4,1) /= -1) .OR. &
          (UBOUND(int_4,2) /= 8) .OR. (LBOUND(int_4,2) /= -1) .OR. &
          (UBOUND(int_4,3) /= 8) .OR. (LBOUND(int_4,3) /= -1) .OR. &
          (UBOUND(int_4,4) /= 8) .OR. (LBOUND(int_4,4) /= -1)
      ASSERT(.NOT.test,'Rednundant CALL dmalloc0A(int_4,-1,1,-1,1,-1,1,-1,1)')
      CALL demallocA(int_4)
!
! rank 5 variable
      CALL dmallocA(int_5,-10,10,10,10,10)
      CALL dmallocA(int_5,10,-10,10,10,10)
      CALL dmallocA(int_5,10,10,-10,10,10)
      CALL dmallocA(int_5,10,10,10,-10,10)
      CALL dmallocA(int_5,10,10,10,10,-10)
      CALL dmallocA(int_5,10,10,10,10,10)
      test=(.NOT.ALLOCATED(int_5)) .OR. ANY(int_5 /= 0) .OR. &
          (UBOUND(int_5,1) /= 10) .OR. (LBOUND(int_5,1) /= 1) .OR. &
          (UBOUND(int_5,2) /= 10) .OR. (LBOUND(int_5,2) /= 1) .OR. &
          (UBOUND(int_5,3) /= 10) .OR. (LBOUND(int_5,3) /= 1) .OR. &
          (UBOUND(int_5,4) /= 10) .OR. (LBOUND(int_5,4) /= 1) .OR. &
          (UBOUND(int_5,5) /= 10) .OR. (LBOUND(int_5,5) /= 1)
      ASSERT(.NOT.test,'dmallocA(int_5,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(int_5,100,100,100,100,100)
      test=(.NOT.ALLOCATED(int_5)) .OR. ANY(int_5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_5,1) /= 10) .OR. (LBOUND(int_5,1) /= 1) .OR. &
          (UBOUND(int_5,2) /= 10) .OR. (LBOUND(int_5,2) /= 1) .OR. &
          (UBOUND(int_5,3) /= 10) .OR. (LBOUND(int_5,3) /= 1) .OR. &
          (UBOUND(int_5,4) /= 10) .OR. (LBOUND(int_5,4) /= 1) .OR. &
          (UBOUND(int_5,5) /= 10) .OR. (LBOUND(int_5,5) /= 1)
      ASSERT(.NOT.test,'dmallocA(int_5,100,100,100,100,100)')

      CALL demallocA(int_5)
      test=ALLOCATED(int_5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_5)')

      CALL demallocA(int_5)
      test=ALLOCATED(int_5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_5)')

      CALL dmalloc0A(int_5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(int_5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(int_5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(int_5,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(int_5)) .OR. ANY(int_5 /= 0) .OR. &
          (UBOUND(int_5,1) /= 8) .OR. (LBOUND(int_5,1) /= -1) .OR. &
          (UBOUND(int_5,2) /= 8) .OR. (LBOUND(int_5,2) /= -1) .OR. &
          (UBOUND(int_5,3) /= 8) .OR. (LBOUND(int_5,3) /= -1) .OR. &
          (UBOUND(int_5,4) /= 8) .OR. (LBOUND(int_5,4) /= -1) .OR. &
          (UBOUND(int_5,5) /= 8) .OR. (LBOUND(int_5,5) /= -1)
      ASSERT(.NOT.test,'dmallocA(int_5,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_5,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(int_5)) .OR. ANY(int_5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_5,1) /= 8) .OR. (LBOUND(int_5,1) /= -1) .OR. &
          (UBOUND(int_5,2) /= 8) .OR. (LBOUND(int_5,2) /= -1) .OR. &
          (UBOUND(int_5,3) /= 8) .OR. (LBOUND(int_5,3) /= -1) .OR. &
          (UBOUND(int_5,4) /= 8) .OR. (LBOUND(int_5,4) /= -1) .OR. &
          (UBOUND(int_5,5) /= 8) .OR. (LBOUND(int_5,5) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(int_5,-1,1,-1,1,-1,1,-1,1,-1,1)')
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
      test=(.NOT.ALLOCATED(int_6)) .OR. ANY(int_6 /= 0) .OR. &
          (UBOUND(int_6,1) /= 10) .OR. (LBOUND(int_6,1) /= 1) .OR. &
          (UBOUND(int_6,2) /= 10) .OR. (LBOUND(int_6,2) /= 1) .OR. &
          (UBOUND(int_6,3) /= 10) .OR. (LBOUND(int_6,3) /= 1) .OR. &
          (UBOUND(int_6,4) /= 10) .OR. (LBOUND(int_6,4) /= 1) .OR. &
          (UBOUND(int_6,5) /= 10) .OR. (LBOUND(int_6,5) /= 1) .OR. &
          (UBOUND(int_6,6) /= 10) .OR. (LBOUND(int_6,6) /= 1)
      ASSERT(.NOT.test,'dmallocA(int_6,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(int_6,100,100,100,100,100,100)
      test=(.NOT.ALLOCATED(int_6)) .OR. ANY(int_6 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_6,1) /= 10) .OR. (LBOUND(int_6,1) /= 1) .OR. &
          (UBOUND(int_6,2) /= 10) .OR. (LBOUND(int_6,2) /= 1) .OR. &
          (UBOUND(int_6,3) /= 10) .OR. (LBOUND(int_6,3) /= 1) .OR. &
          (UBOUND(int_6,4) /= 10) .OR. (LBOUND(int_6,4) /= 1) .OR. &
          (UBOUND(int_6,5) /= 10) .OR. (LBOUND(int_6,5) /= 1) .OR. &
          (UBOUND(int_6,6) /= 10) .OR. (LBOUND(int_6,6) /= 1)
      ASSERT(.NOT.test,'dmallocA(int_6,100,100,100,100,100,100)')

      CALL demallocA(int_6)
      test=ALLOCATED(int_6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_6)')

      CALL demallocA(int_6)
      test=ALLOCATED(int_6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_6)')

      CALL dmalloc0A(int_6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(int_6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(int_6)) .OR. ANY(int_6 /= 0) .OR. &
          (UBOUND(int_6,1) /= 8) .OR. (LBOUND(int_6,1) /= -1) .OR. &
          (UBOUND(int_6,2) /= 8) .OR. (LBOUND(int_6,2) /= -1) .OR. &
          (UBOUND(int_6,3) /= 8) .OR. (LBOUND(int_6,3) /= -1) .OR. &
          (UBOUND(int_6,4) /= 8) .OR. (LBOUND(int_6,4) /= -1) .OR. &
          (UBOUND(int_6,5) /= 8) .OR. (LBOUND(int_6,5) /= -1) .OR. &
          (UBOUND(int_6,6) /= 8) .OR. (LBOUND(int_6,6) /= -1)
      ASSERT(.NOT.test,'dmallocA(int_6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(int_6)) .OR. ANY(int_6 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_6,1) /= 8) .OR. (LBOUND(int_6,1) /= -1) .OR. &
          (UBOUND(int_6,2) /= 8) .OR. (LBOUND(int_6,2) /= -1) .OR. &
          (UBOUND(int_6,3) /= 8) .OR. (LBOUND(int_6,3) /= -1) .OR. &
          (UBOUND(int_6,4) /= 8) .OR. (LBOUND(int_6,4) /= -1) .OR. &
          (UBOUND(int_6,5) /= 8) .OR. (LBOUND(int_6,5) /= -1) .OR. &
          (UBOUND(int_6,6) /= 8) .OR. (LBOUND(int_6,6) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(int_6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')
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
      test=(.NOT.ALLOCATED(int_7)) .OR. ANY(int_7 /= 0) .OR. &
          (UBOUND(int_7,1) /= 10) .OR. (LBOUND(int_7,1) /= 1) .OR. &
          (UBOUND(int_7,2) /= 10) .OR. (LBOUND(int_7,2) /= 1) .OR. &
          (UBOUND(int_7,3) /= 10) .OR. (LBOUND(int_7,3) /= 1) .OR. &
          (UBOUND(int_7,4) /= 10) .OR. (LBOUND(int_7,4) /= 1) .OR. &
          (UBOUND(int_7,5) /= 10) .OR. (LBOUND(int_7,5) /= 1) .OR. &
          (UBOUND(int_7,6) /= 10) .OR. (LBOUND(int_7,6) /= 1) .OR. &
          (UBOUND(int_7,7) /= 10) .OR. (LBOUND(int_7,7) /= 1)
      ASSERT(.NOT.test,'dmallocA(int_7,10,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(int_7,100,100,100,100,100,100,100)
      test=(.NOT.ALLOCATED(int_7)) .OR. ANY(int_7 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_7,1) /= 10) .OR. (LBOUND(int_7,1) /= 1) .OR. &
          (UBOUND(int_7,2) /= 10) .OR. (LBOUND(int_7,2) /= 1) .OR. &
          (UBOUND(int_7,3) /= 10) .OR. (LBOUND(int_7,3) /= 1) .OR. &
          (UBOUND(int_7,4) /= 10) .OR. (LBOUND(int_7,4) /= 1) .OR. &
          (UBOUND(int_7,5) /= 10) .OR. (LBOUND(int_7,5) /= 1) .OR. &
          (UBOUND(int_7,6) /= 10) .OR. (LBOUND(int_7,6) /= 1) .OR. &
          (UBOUND(int_7,7) /= 10) .OR. (LBOUND(int_7,7) /= 1)
      ASSERT(.NOT.test,'dmallocA(int_7,100,100,100,100,100,100,100)')

      CALL demallocA(int_7)
      test=ALLOCATED(int_7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_7)')

      CALL demallocA(int_7)
      test=ALLOCATED(int_7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(int_7)')

      CALL dmalloc0A(int_7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(int_7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(int_7)) .OR. ANY(int_7 /= 0) .OR. &
          (UBOUND(int_7,1) /= 8) .OR. (LBOUND(int_7,1) /= -1) .OR. &
          (UBOUND(int_7,2) /= 8) .OR. (LBOUND(int_7,2) /= -1) .OR. &
          (UBOUND(int_7,3) /= 8) .OR. (LBOUND(int_7,3) /= -1) .OR. &
          (UBOUND(int_7,4) /= 8) .OR. (LBOUND(int_7,4) /= -1) .OR. &
          (UBOUND(int_7,5) /= 8) .OR. (LBOUND(int_7,5) /= -1) .OR. &
          (UBOUND(int_7,6) /= 8) .OR. (LBOUND(int_7,6) /= -1) .OR. &
          (UBOUND(int_7,7) /= 8) .OR. (LBOUND(int_7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(int_7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(int_7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(int_7)) .OR. ANY(int_7 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(int_7,1) /= 8) .OR. (LBOUND(int_7,1) /= -1) .OR. &
          (UBOUND(int_7,2) /= 8) .OR. (LBOUND(int_7,2) /= -1) .OR. &
          (UBOUND(int_7,3) /= 8) .OR. (LBOUND(int_7,3) /= -1) .OR. &
          (UBOUND(int_7,4) /= 8) .OR. (LBOUND(int_7,4) /= -1) .OR. &
          (UBOUND(int_7,5) /= 8) .OR. (LBOUND(int_7,5) /= -1) .OR. &
          (UBOUND(int_7,6) /= 8) .OR. (LBOUND(int_7,6) /= -1) .OR. &
          (UBOUND(int_7,7) /= 8) .OR. (LBOUND(int_7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(int_7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')
      CALL demallocA(int_7)

    ENDSUBROUTINE testINTA
!
!-------------------------------------------------------------------------------
! Test allocation/deallocation for integers
    SUBROUTINE testLONGINTP()
      INTEGER(SLK),POINTER :: lint1(:)
      INTEGER(SLK),POINTER :: lint2(:,:)
      INTEGER(SLK),POINTER :: lint3(:,:,:)
      INTEGER(SLK),POINTER :: lint4(:,:,:,:)
      INTEGER(SLK),POINTER :: lint5(:,:,:,:,:)
      INTEGER(SLK),POINTER :: lint6(:,:,:,:,:,:)
      INTEGER(SLK),POINTER :: lint7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0

      NULLIFY(lint1,lint2,lint3,lint4,lint5,lint6,lint7)
!
! rank 1 variable
      CALL dmallocP(lint1,-10)
      CALL dmallocP(lint1,10)
      test=(.NOT.ASSOCIATED(lint1)) .OR. ANY(lint1 /= 0) .OR. &
          (UBOUND(lint1,1) /= 10) .OR. (LBOUND(lint1,1) /= 1)
      ASSERT(.NOT.test,'dmallocP(lint1,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(lint1,100)
      test=(.NOT.ASSOCIATED(lint1)) .OR. ANY(lint1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint1,1) /= 10) .OR. (LBOUND(lint1,1) /= 1)
      ASSERT(.NOT.test,'dmallocP(lint1,100)')

      CALL demallocP(lint1)
      test=ASSOCIATED(lint1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint1)')

      CALL demallocP(lint1)
      test=ASSOCIATED(lint1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint1)')

      CALL dmalloc0P(lint1,8,-1)
      CALL dmalloc0P(lint1,-1,8)
      test=(.NOT.ASSOCIATED(lint1)) .OR. ANY(lint1 /= 0) .OR. &
          (UBOUND(lint1,1) /= 8) .OR. (LBOUND(lint1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(lint1,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint1,-1,1)
      test=(.NOT.ASSOCIATED(lint1)) .OR. ANY(lint1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint1,1) /= 8) .OR. (LBOUND(lint1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(lint1,-1,1)')

      CALL demallocP(lint1)
!
! rank 2 variable
      NULLIFY(lint2)
      CALL dmallocP(lint2,-10,10)
      CALL dmallocP(lint2,10,-10)
      CALL dmallocP(lint2,10,10)
      test=(.NOT.ASSOCIATED(lint2)) .OR. ANY(lint2 /= 0) .OR. &
          (UBOUND(lint2,1) /= 10) .OR. (LBOUND(lint2,1) /= 1) .OR. &
          (UBOUND(lint2,2) /= 10) .OR. (LBOUND(lint2,2) /= 1)
      ASSERT(.NOT.test,'dmallocP(lint2,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(lint2,100,100)
      test=(.NOT.ASSOCIATED(lint2)) .OR. ANY(lint2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint2,1) /= 10) .OR. (LBOUND(lint2,1) /= 1) .OR. &
          (UBOUND(lint2,2) /= 10) .OR. (LBOUND(lint2,2) /= 1)
      ASSERT(.NOT.test,'dmallocP(lint2,100,100)')

      CALL demallocP(lint2)
      test=ASSOCIATED(lint2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint2)')

      CALL demallocP(lint2)
      test=ASSOCIATED(lint2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint2)')

      CALL dmalloc0P(lint2,8,-1,-1,8)
      CALL dmalloc0P(lint2,-1,8,8,-1)
      CALL dmalloc0P(lint2,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(lint2)) .OR. ANY(lint2 /= 0) .OR. &
          (UBOUND(lint2,1) /= 8) .OR. (LBOUND(lint2,1) /= -1) .OR. &
          (UBOUND(lint2,2) /= 8) .OR. (LBOUND(lint2,2) /= -1)
      ASSERT(.NOT.test,'CALL CALL dmalloc0P(lint2,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint2,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(lint2)) .OR. ANY(lint2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint2,1) /= 8) .OR. (LBOUND(lint2,1) /= -1) .OR. &
          (UBOUND(lint2,2) /= 8) .OR. (LBOUND(lint2,2) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(lint2,-1,1,-1,1)')

      CALL demallocP(lint2)
!
! rank 3 variable
      NULLIFY(lint3)
      CALL dmallocP(lint3,-10,10,10)
      CALL dmallocP(lint3,10,-10,10)
      CALL dmallocP(lint3,10,10,-10)
      CALL dmallocP(lint3,10,10,10)
      test=(.NOT.ASSOCIATED(lint3)) .OR. ANY(lint3 /= 0) .OR. &
          (UBOUND(lint3,1) /= 10) .OR. (LBOUND(lint3,1) /= 1) .OR. &
          (UBOUND(lint3,2) /= 10) .OR. (LBOUND(lint3,2) /= 1) .OR. &
          (UBOUND(lint3,3) /= 10) .OR. (LBOUND(lint3,3) /= 1)
      ASSERT(.NOT.test,'dmallocP(lint3,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(lint3,100,100,100)
      test=(.NOT.ASSOCIATED(lint3)) .OR. ANY(lint3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint3,1) /= 10) .OR. (LBOUND(lint3,1) /= 1) .OR. &
          (UBOUND(lint3,2) /= 10) .OR. (LBOUND(lint3,2) /= 1) .OR. &
          (UBOUND(lint3,3) /= 10) .OR. (LBOUND(lint3,3) /= 1)
      ASSERT(.NOT.test,'dmallocP(lint3,100,100,100)')

      CALL demallocP(lint3)
      test=ASSOCIATED(lint3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint3)')

      CALL demallocP(lint3)
      test=ASSOCIATED(lint3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint3)')

      CALL dmalloc0P(lint3,8,-1,-1,8,-1,8)
      CALL dmalloc0P(lint3,-1,8,8,-1,-1,8)
      CALL dmalloc0P(lint3,-1,8,-1,8,8,-1)
      CALL dmalloc0P(lint3,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(lint3)) .OR. ANY(lint3 /= 0) .OR. &
          (UBOUND(lint3,1) /= 8) .OR. (LBOUND(lint3,1) /= -1) .OR. &
          (UBOUND(lint3,2) /= 8) .OR. (LBOUND(lint3,2) /= -1) .OR. &
          (UBOUND(lint3,3) /= 8) .OR. (LBOUND(lint3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(lint3,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint3,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(lint3)) .OR. ANY(lint3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint3,1) /= 8) .OR. (LBOUND(lint3,1) /= -1) .OR. &
          (UBOUND(lint3,2) /= 8) .OR. (LBOUND(lint3,2) /= -1) .OR. &
          (UBOUND(lint3,3) /= 8) .OR. (LBOUND(lint3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(lint3,-1,1,-1,1,-1,1)')

      CALL demallocP(lint3)
!
! rank 4 variable
      NULLIFY(lint4)
      CALL dmallocP(lint4,-10,10,10,10)
      CALL dmallocP(lint4,10,-10,10,10)
      CALL dmallocP(lint4,10,10,-10,10)
      CALL dmallocP(lint4,10,10,10,-10)
      CALL dmallocP(lint4,10,10,10,10)
      test=(.NOT.ASSOCIATED(lint4)) .OR. ANY(lint4 /= 0) .OR. &
          (UBOUND(lint4,1) /= 10) .OR. (LBOUND(lint4,1) /= 1) .OR. &
          (UBOUND(lint4,2) /= 10) .OR. (LBOUND(lint4,2) /= 1) .OR. &
          (UBOUND(lint4,3) /= 10) .OR. (LBOUND(lint4,3) /= 1) .OR. &
          (UBOUND(lint4,4) /= 10) .OR. (LBOUND(lint4,4) /= 1)
      ASSERT(.NOT.test,'dmallocP(lint4,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(lint4,100,100,100,100)
      test=(.NOT.ASSOCIATED(lint4)) .OR. ANY(lint4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint4,1) /= 10) .OR. (LBOUND(lint4,1) /= 1) .OR. &
          (UBOUND(lint4,2) /= 10) .OR. (LBOUND(lint4,2) /= 1) .OR. &
          (UBOUND(lint4,3) /= 10) .OR. (LBOUND(lint4,3) /= 1) .OR. &
          (UBOUND(lint4,4) /= 10) .OR. (LBOUND(lint4,4) /= 1)
      ASSERT(.NOT.test,'Rednundant CALL dmallocP(lint4,100,100,100,100)')

      CALL demallocP(lint4)
      test=ASSOCIATED(lint4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint4)')

      CALL demallocP(lint4)
      test=ASSOCIATED(lint4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint4)')

      CALL dmalloc0P(lint4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(lint4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(lint4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(lint4,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(lint4)) .OR. ANY(lint4 /= 0) .OR. &
          (UBOUND(lint4,1) /= 8) .OR. (LBOUND(lint4,1) /= -1) .OR. &
          (UBOUND(lint4,2) /= 8) .OR. (LBOUND(lint4,2) /= -1) .OR. &
          (UBOUND(lint4,3) /= 8) .OR. (LBOUND(lint4,3) /= -1) .OR. &
          (UBOUND(lint4,4) /= 8) .OR. (LBOUND(lint4,4) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(lint4,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint4,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(lint4)) .OR. ANY(lint4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint4,1) /= 8) .OR. (LBOUND(lint4,1) /= -1) .OR. &
          (UBOUND(lint4,2) /= 8) .OR. (LBOUND(lint4,2) /= -1) .OR. &
          (UBOUND(lint4,3) /= 8) .OR. (LBOUND(lint4,3) /= -1) .OR. &
          (UBOUND(lint4,4) /= 8) .OR. (LBOUND(lint4,4) /= -1)
      ASSERT(.NOT.test,'Rednundant CALL dmalloc0P(lint4,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ASSOCIATED(lint5)) .OR. ANY(lint5 /= 0) .OR. &
          (UBOUND(lint5,1) /= 10) .OR. (LBOUND(lint5,1) /= 1) .OR. &
          (UBOUND(lint5,2) /= 10) .OR. (LBOUND(lint5,2) /= 1) .OR. &
          (UBOUND(lint5,3) /= 10) .OR. (LBOUND(lint5,3) /= 1) .OR. &
          (UBOUND(lint5,4) /= 10) .OR. (LBOUND(lint5,4) /= 1) .OR. &
          (UBOUND(lint5,5) /= 10) .OR. (LBOUND(lint5,5) /= 1)
      ASSERT(.NOT.test,'dmallocP(lint5,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(lint5,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(lint5)) .OR. ANY(lint5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint5,1) /= 10) .OR. (LBOUND(lint5,1) /= 1) .OR. &
          (UBOUND(lint5,2) /= 10) .OR. (LBOUND(lint5,2) /= 1) .OR. &
          (UBOUND(lint5,3) /= 10) .OR. (LBOUND(lint5,3) /= 1) .OR. &
          (UBOUND(lint5,4) /= 10) .OR. (LBOUND(lint5,4) /= 1) .OR. &
          (UBOUND(lint5,5) /= 10) .OR. (LBOUND(lint5,5) /= 1)
      ASSERT(.NOT.test,'dmallocP(lint5,100,100,100,100,100)')

      CALL demallocP(lint5)
      test=ASSOCIATED(lint5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint5)')

      CALL demallocP(lint5)
      test=ASSOCIATED(lint5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint5)')

      CALL dmalloc0P(lint5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(lint5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(lint5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(lint5,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(lint5)) .OR. ANY(lint5 /= 0) .OR. &
          (UBOUND(lint5,1) /= 8) .OR. (LBOUND(lint5,1) /= -1) .OR. &
          (UBOUND(lint5,2) /= 8) .OR. (LBOUND(lint5,2) /= -1) .OR. &
          (UBOUND(lint5,3) /= 8) .OR. (LBOUND(lint5,3) /= -1) .OR. &
          (UBOUND(lint5,4) /= 8) .OR. (LBOUND(lint5,4) /= -1) .OR. &
          (UBOUND(lint5,5) /= 8) .OR. (LBOUND(lint5,5) /= -1)
      ASSERT(.NOT.test,'dmallocP(lint5,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint5,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(lint5)) .OR. ANY(lint5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint5,1) /= 8) .OR. (LBOUND(lint5,1) /= -1) .OR. &
          (UBOUND(lint5,2) /= 8) .OR. (LBOUND(lint5,2) /= -1) .OR. &
          (UBOUND(lint5,3) /= 8) .OR. (LBOUND(lint5,3) /= -1) .OR. &
          (UBOUND(lint5,4) /= 8) .OR. (LBOUND(lint5,4) /= -1) .OR. &
          (UBOUND(lint5,5) /= 8) .OR. (LBOUND(lint5,5) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(lint5,-1,1,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ASSOCIATED(lint6)) .OR. ANY(lint6 /= 0) .OR. &
          (UBOUND(lint6,1) /= 10) .OR. (LBOUND(lint6,1) /= 1) .OR. &
          (UBOUND(lint6,2) /= 10) .OR. (LBOUND(lint6,2) /= 1) .OR. &
          (UBOUND(lint6,3) /= 10) .OR. (LBOUND(lint6,3) /= 1) .OR. &
          (UBOUND(lint6,4) /= 10) .OR. (LBOUND(lint6,4) /= 1) .OR. &
          (UBOUND(lint6,5) /= 10) .OR. (LBOUND(lint6,5) /= 1) .OR. &
          (UBOUND(lint6,6) /= 10) .OR. (LBOUND(lint6,6) /= 1)
      ASSERT(.NOT.test,'dmallocP(lint6,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(lint6,100,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(lint6)) .OR. ANY(lint6 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint6,1) /= 10) .OR. (LBOUND(lint6,1) /= 1) .OR. &
          (UBOUND(lint6,2) /= 10) .OR. (LBOUND(lint6,2) /= 1) .OR. &
          (UBOUND(lint6,3) /= 10) .OR. (LBOUND(lint6,3) /= 1) .OR. &
          (UBOUND(lint6,4) /= 10) .OR. (LBOUND(lint6,4) /= 1) .OR. &
          (UBOUND(lint6,5) /= 10) .OR. (LBOUND(lint6,5) /= 1) .OR. &
          (UBOUND(lint6,6) /= 10) .OR. (LBOUND(lint6,6) /= 1)
      ASSERT(.NOT.test,'dmallocP(lint6,100,100,100,100,100,100)')

      CALL demallocP(lint6)
      test=ASSOCIATED(lint6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint6)')

      CALL demallocP(lint6)
      test=ASSOCIATED(lint6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint6)')

      CALL dmalloc0P(lint6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(lint6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(lint6)) .OR. ANY(lint6 /= 0) .OR. &
          (UBOUND(lint6,1) /= 8) .OR. (LBOUND(lint6,1) /= -1) .OR. &
          (UBOUND(lint6,2) /= 8) .OR. (LBOUND(lint6,2) /= -1) .OR. &
          (UBOUND(lint6,3) /= 8) .OR. (LBOUND(lint6,3) /= -1) .OR. &
          (UBOUND(lint6,4) /= 8) .OR. (LBOUND(lint6,4) /= -1) .OR. &
          (UBOUND(lint6,5) /= 8) .OR. (LBOUND(lint6,5) /= -1) .OR. &
          (UBOUND(lint6,6) /= 8) .OR. (LBOUND(lint6,6) /= -1)
      ASSERT(.NOT.test,'dmallocP(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(lint6)) .OR. ANY(lint6 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint6,1) /= 8) .OR. (LBOUND(lint6,1) /= -1) .OR. &
          (UBOUND(lint6,2) /= 8) .OR. (LBOUND(lint6,2) /= -1) .OR. &
          (UBOUND(lint6,3) /= 8) .OR. (LBOUND(lint6,3) /= -1) .OR. &
          (UBOUND(lint6,4) /= 8) .OR. (LBOUND(lint6,4) /= -1) .OR. &
          (UBOUND(lint6,5) /= 8) .OR. (LBOUND(lint6,5) /= -1) .OR. &
          (UBOUND(lint6,6) /= 8) .OR. (LBOUND(lint6,6) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(lint6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ASSOCIATED(lint7)) .OR. ANY(lint7 /= 0) .OR. &
          (UBOUND(lint7,1) /= 10) .OR. (LBOUND(lint7,1) /= 1) .OR. &
          (UBOUND(lint7,2) /= 10) .OR. (LBOUND(lint7,2) /= 1) .OR. &
          (UBOUND(lint7,3) /= 10) .OR. (LBOUND(lint7,3) /= 1) .OR. &
          (UBOUND(lint7,4) /= 10) .OR. (LBOUND(lint7,4) /= 1) .OR. &
          (UBOUND(lint7,5) /= 10) .OR. (LBOUND(lint7,5) /= 1) .OR. &
          (UBOUND(lint7,6) /= 10) .OR. (LBOUND(lint7,6) /= 1) .OR. &
          (UBOUND(lint7,7) /= 10) .OR. (LBOUND(lint7,7) /= 1)
      ASSERT(.NOT.test,'dmallocP(lint7,10,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(lint7,100,100,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(lint7)) .OR. ANY(lint7 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint7,1) /= 10) .OR. (LBOUND(lint7,1) /= 1) .OR. &
          (UBOUND(lint7,2) /= 10) .OR. (LBOUND(lint7,2) /= 1) .OR. &
          (UBOUND(lint7,3) /= 10) .OR. (LBOUND(lint7,3) /= 1) .OR. &
          (UBOUND(lint7,4) /= 10) .OR. (LBOUND(lint7,4) /= 1) .OR. &
          (UBOUND(lint7,5) /= 10) .OR. (LBOUND(lint7,5) /= 1) .OR. &
          (UBOUND(lint7,6) /= 10) .OR. (LBOUND(lint7,6) /= 1) .OR. &
          (UBOUND(lint7,7) /= 10) .OR. (LBOUND(lint7,7) /= 1)
      ASSERT(.NOT.test,'dmallocP(lint7,100,100,100,100,100,100,100)')

      CALL demallocP(lint7)
      test=ASSOCIATED(lint7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint7)')

      CALL demallocP(lint7)
      test=ASSOCIATED(lint7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(lint7)')

      CALL dmalloc0P(lint7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(lint7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(lint7)) .OR. ANY(lint7 /= 0) .OR. &
          (UBOUND(lint7,1) /= 8) .OR. (LBOUND(lint7,1) /= -1) .OR. &
          (UBOUND(lint7,2) /= 8) .OR. (LBOUND(lint7,2) /= -1) .OR. &
          (UBOUND(lint7,3) /= 8) .OR. (LBOUND(lint7,3) /= -1) .OR. &
          (UBOUND(lint7,4) /= 8) .OR. (LBOUND(lint7,4) /= -1) .OR. &
          (UBOUND(lint7,5) /= 8) .OR. (LBOUND(lint7,5) /= -1) .OR. &
          (UBOUND(lint7,6) /= 8) .OR. (LBOUND(lint7,6) /= -1) .OR. &
          (UBOUND(lint7,7) /= 8) .OR. (LBOUND(lint7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(lint7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(lint7)) .OR. ANY(lint7 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint7,1) /= 8) .OR. (LBOUND(lint7,1) /= -1) .OR. &
          (UBOUND(lint7,2) /= 8) .OR. (LBOUND(lint7,2) /= -1) .OR. &
          (UBOUND(lint7,3) /= 8) .OR. (LBOUND(lint7,3) /= -1) .OR. &
          (UBOUND(lint7,4) /= 8) .OR. (LBOUND(lint7,4) /= -1) .OR. &
          (UBOUND(lint7,5) /= 8) .OR. (LBOUND(lint7,5) /= -1) .OR. &
          (UBOUND(lint7,6) /= 8) .OR. (LBOUND(lint7,6) /= -1) .OR. &
          (UBOUND(lint7,7) /= 8) .OR. (LBOUND(lint7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(lint7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')

      CALL demallocP(lint7)

    ENDSUBROUTINE testLONGINTP
!
!-------------------------------------------------------------------------------
! Test allocation/deallocation for integers
    SUBROUTINE testLONGINTA()
      INTEGER(SLK),ALLOCATABLE :: lint1(:)
      INTEGER(SLK),ALLOCATABLE :: lint2(:,:)
      INTEGER(SLK),ALLOCATABLE :: lint3(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: lint4(:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: lint5(:,:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: lint6(:,:,:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: lint7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0
!
! rank 1 variable
      CALL dmallocA(lint1,-10)
      CALL dmallocA(lint1,10)
      test=(.NOT.ALLOCATED(lint1)) .OR. ANY(lint1 /= 0) .OR. &
          (UBOUND(lint1,1) /= 10) .OR. (LBOUND(lint1,1) /= 1)
      ASSERT(.NOT.test,'dmallocA(lint1,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(lint1,100)
      test=(.NOT.ALLOCATED(lint1)) .OR. ANY(lint1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint1,1) /= 10) .OR. (LBOUND(lint1,1) /= 1)
      ASSERT(.NOT.test,'dmallocA(lint1,100)')

      CALL demallocA(lint1)
      test=ALLOCATED(lint1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint1)')

      CALL demallocA(lint1)
      test=ALLOCATED(lint1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint1)')

      CALL dmalloc0A(lint1,8,-1)
      CALL dmalloc0A(lint1,-1,8)
      test=(.NOT.ALLOCATED(lint1)) .OR. ANY(lint1 /= 0) .OR. &
          (UBOUND(lint1,1) /= 8) .OR. (LBOUND(lint1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(lint1,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint1,-1,1)
      test=(.NOT.ALLOCATED(lint1)) .OR. ANY(lint1 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint1,1) /= 8) .OR. (LBOUND(lint1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(lint1,-1,1)')

      CALL demallocA(lint1)
!
! rank 2 variable
      CALL dmallocA(lint2,-10,10)
      CALL dmallocA(lint2,10,-10)
      CALL dmallocA(lint2,10,10)
      test=(.NOT.ALLOCATED(lint2)) .OR. ANY(lint2 /= 0) .OR. &
          (UBOUND(lint2,1) /= 10) .OR. (LBOUND(lint2,1) /= 1) .OR. &
          (UBOUND(lint2,2) /= 10) .OR. (LBOUND(lint2,2) /= 1)
      ASSERT(.NOT.test,'dmallocA(lint2,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(lint2,100,100)
      test=(.NOT.ALLOCATED(lint2)) .OR. ANY(lint2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint2,1) /= 10) .OR. (LBOUND(lint2,1) /= 1) .OR. &
          (UBOUND(lint2,2) /= 10) .OR. (LBOUND(lint2,2) /= 1)
      ASSERT(.NOT.test,'dmallocA(lint2,100,100)')

      CALL demallocA(lint2)
      test=ALLOCATED(lint2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint2)')

      CALL demallocA(lint2)
      test=ALLOCATED(lint2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint2)')

      CALL dmalloc0A(lint2,8,-1,-1,8)
      CALL dmalloc0A(lint2,-1,8,8,-1)
      CALL dmalloc0A(lint2,-1,8,-1,8)
      test=(.NOT.ALLOCATED(lint2)) .OR. ANY(lint2 /= 0) .OR. &
          (UBOUND(lint2,1) /= 8) .OR. (LBOUND(lint2,1) /= -1) .OR. &
          (UBOUND(lint2,2) /= 8) .OR. (LBOUND(lint2,2) /= -1)
      ASSERT(.NOT.test,'CALL CALL dmalloc0A(lint2,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint2,-1,1,-1,1)
      test=(.NOT.ALLOCATED(lint2)) .OR. ANY(lint2 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint2,1) /= 8) .OR. (LBOUND(lint2,1) /= -1) .OR. &
          (UBOUND(lint2,2) /= 8) .OR. (LBOUND(lint2,2) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(lint2,-1,1,-1,1)')

      CALL demallocA(lint2)
!
! rank 3 variable
      CALL dmallocA(lint3,-10,10,10)
      CALL dmallocA(lint3,10,-10,10)
      CALL dmallocA(lint3,10,10,-10)
      CALL dmallocA(lint3,10,10,10)
      test=(.NOT.ALLOCATED(lint3)) .OR. ANY(lint3 /= 0) .OR. &
          (UBOUND(lint3,1) /= 10) .OR. (LBOUND(lint3,1) /= 1) .OR. &
          (UBOUND(lint3,2) /= 10) .OR. (LBOUND(lint3,2) /= 1) .OR. &
          (UBOUND(lint3,3) /= 10) .OR. (LBOUND(lint3,3) /= 1)
      ASSERT(.NOT.test,'dmallocA(lint3,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(lint3,100,100,100)
      test=(.NOT.ALLOCATED(lint3)) .OR. ANY(lint3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint3,1) /= 10) .OR. (LBOUND(lint3,1) /= 1) .OR. &
          (UBOUND(lint3,2) /= 10) .OR. (LBOUND(lint3,2) /= 1) .OR. &
          (UBOUND(lint3,3) /= 10) .OR. (LBOUND(lint3,3) /= 1)
      ASSERT(.NOT.test,'dmallocA(lint3,100,100,100)')

      CALL demallocA(lint3)
      test=ALLOCATED(lint3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint3)')

      CALL demallocA(lint3)
      test=ALLOCATED(lint3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint3)')

      CALL dmalloc0A(lint3,8,-1,-1,8,-1,8)
      CALL dmalloc0A(lint3,-1,8,8,-1,-1,8)
      CALL dmalloc0A(lint3,-1,8,-1,8,8,-1)
      CALL dmalloc0A(lint3,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(lint3)) .OR. ANY(lint3 /= 0) .OR. &
          (UBOUND(lint3,1) /= 8) .OR. (LBOUND(lint3,1) /= -1) .OR. &
          (UBOUND(lint3,2) /= 8) .OR. (LBOUND(lint3,2) /= -1) .OR. &
          (UBOUND(lint3,3) /= 8) .OR. (LBOUND(lint3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(lint3,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint3,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(lint3)) .OR. ANY(lint3 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint3,1) /= 8) .OR. (LBOUND(lint3,1) /= -1) .OR. &
          (UBOUND(lint3,2) /= 8) .OR. (LBOUND(lint3,2) /= -1) .OR. &
          (UBOUND(lint3,3) /= 8) .OR. (LBOUND(lint3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(lint3,-1,1,-1,1,-1,1)')

      CALL demallocA(lint3)
!
! rank 4 variable
      CALL dmallocA(lint4,-10,10,10,10)
      CALL dmallocA(lint4,10,-10,10,10)
      CALL dmallocA(lint4,10,10,-10,10)
      CALL dmallocA(lint4,10,10,10,-10)
      CALL dmallocA(lint4,10,10,10,10)
      test=(.NOT.ALLOCATED(lint4)) .OR. ANY(lint4 /= 0) .OR. &
          (UBOUND(lint4,1) /= 10) .OR. (LBOUND(lint4,1) /= 1) .OR. &
          (UBOUND(lint4,2) /= 10) .OR. (LBOUND(lint4,2) /= 1) .OR. &
          (UBOUND(lint4,3) /= 10) .OR. (LBOUND(lint4,3) /= 1) .OR. &
          (UBOUND(lint4,4) /= 10) .OR. (LBOUND(lint4,4) /= 1)
      ASSERT(.NOT.test,'dmallocA(lint4,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(lint4,100,100,100,100)
      test=(.NOT.ALLOCATED(lint4)) .OR. ANY(lint4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint4,1) /= 10) .OR. (LBOUND(lint4,1) /= 1) .OR. &
          (UBOUND(lint4,2) /= 10) .OR. (LBOUND(lint4,2) /= 1) .OR. &
          (UBOUND(lint4,3) /= 10) .OR. (LBOUND(lint4,3) /= 1) .OR. &
          (UBOUND(lint4,4) /= 10) .OR. (LBOUND(lint4,4) /= 1)
      ASSERT(.NOT.test,'Rednundant CALL dmallocA(lint4,100,100,100,100)')

      CALL demallocA(lint4)
      test=ALLOCATED(lint4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint4)')

      CALL demallocA(lint4)
      test=ALLOCATED(lint4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint4)')

      CALL dmalloc0A(lint4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(lint4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(lint4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(lint4,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(lint4)) .OR. ANY(lint4 /= 0) .OR. &
          (UBOUND(lint4,1) /= 8) .OR. (LBOUND(lint4,1) /= -1) .OR. &
          (UBOUND(lint4,2) /= 8) .OR. (LBOUND(lint4,2) /= -1) .OR. &
          (UBOUND(lint4,3) /= 8) .OR. (LBOUND(lint4,3) /= -1) .OR. &
          (UBOUND(lint4,4) /= 8) .OR. (LBOUND(lint4,4) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(lint4,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint4,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(lint4)) .OR. ANY(lint4 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint4,1) /= 8) .OR. (LBOUND(lint4,1) /= -1) .OR. &
          (UBOUND(lint4,2) /= 8) .OR. (LBOUND(lint4,2) /= -1) .OR. &
          (UBOUND(lint4,3) /= 8) .OR. (LBOUND(lint4,3) /= -1) .OR. &
          (UBOUND(lint4,4) /= 8) .OR. (LBOUND(lint4,4) /= -1)
      ASSERT(.NOT.test,'Rednundant CALL dmalloc0A(lint4,-1,1,-1,1,-1,1,-1,1)')

      CALL demallocA(lint4)
!
! rank 5 variable
      CALL dmallocA(lint5,-10,10,10,10,10)
      CALL dmallocA(lint5,10,-10,10,10,10)
      CALL dmallocA(lint5,10,10,-10,10,10)
      CALL dmallocA(lint5,10,10,10,-10,10)
      CALL dmallocA(lint5,10,10,10,10,-10)
      CALL dmallocA(lint5,10,10,10,10,10)
      test=(.NOT.ALLOCATED(lint5)) .OR. ANY(lint5 /= 0) .OR. &
          (UBOUND(lint5,1) /= 10) .OR. (LBOUND(lint5,1) /= 1) .OR. &
          (UBOUND(lint5,2) /= 10) .OR. (LBOUND(lint5,2) /= 1) .OR. &
          (UBOUND(lint5,3) /= 10) .OR. (LBOUND(lint5,3) /= 1) .OR. &
          (UBOUND(lint5,4) /= 10) .OR. (LBOUND(lint5,4) /= 1) .OR. &
          (UBOUND(lint5,5) /= 10) .OR. (LBOUND(lint5,5) /= 1)
      ASSERT(.NOT.test,'dmallocA(lint5,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(lint5,100,100,100,100,100)
      test=(.NOT.ALLOCATED(lint5)) .OR. ANY(lint5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint5,1) /= 10) .OR. (LBOUND(lint5,1) /= 1) .OR. &
          (UBOUND(lint5,2) /= 10) .OR. (LBOUND(lint5,2) /= 1) .OR. &
          (UBOUND(lint5,3) /= 10) .OR. (LBOUND(lint5,3) /= 1) .OR. &
          (UBOUND(lint5,4) /= 10) .OR. (LBOUND(lint5,4) /= 1) .OR. &
          (UBOUND(lint5,5) /= 10) .OR. (LBOUND(lint5,5) /= 1)
      ASSERT(.NOT.test,'dmallocA(lint5,100,100,100,100,100)')

      CALL demallocA(lint5)
      test=ALLOCATED(lint5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint5)')

      CALL demallocA(lint5)
      test=ALLOCATED(lint5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint5)')

      CALL dmalloc0A(lint5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(lint5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(lint5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(lint5,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(lint5)) .OR. ANY(lint5 /= 0) .OR. &
          (UBOUND(lint5,1) /= 8) .OR. (LBOUND(lint5,1) /= -1) .OR. &
          (UBOUND(lint5,2) /= 8) .OR. (LBOUND(lint5,2) /= -1) .OR. &
          (UBOUND(lint5,3) /= 8) .OR. (LBOUND(lint5,3) /= -1) .OR. &
          (UBOUND(lint5,4) /= 8) .OR. (LBOUND(lint5,4) /= -1) .OR. &
          (UBOUND(lint5,5) /= 8) .OR. (LBOUND(lint5,5) /= -1)
      ASSERT(.NOT.test,'dmallocA(lint5,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint5,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(lint5)) .OR. ANY(lint5 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint5,1) /= 8) .OR. (LBOUND(lint5,1) /= -1) .OR. &
          (UBOUND(lint5,2) /= 8) .OR. (LBOUND(lint5,2) /= -1) .OR. &
          (UBOUND(lint5,3) /= 8) .OR. (LBOUND(lint5,3) /= -1) .OR. &
          (UBOUND(lint5,4) /= 8) .OR. (LBOUND(lint5,4) /= -1) .OR. &
          (UBOUND(lint5,5) /= 8) .OR. (LBOUND(lint5,5) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(lint5,-1,1,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ALLOCATED(lint6)) .OR. ANY(lint6 /= 0) .OR. &
          (UBOUND(lint6,1) /= 10) .OR. (LBOUND(lint6,1) /= 1) .OR. &
          (UBOUND(lint6,2) /= 10) .OR. (LBOUND(lint6,2) /= 1) .OR. &
          (UBOUND(lint6,3) /= 10) .OR. (LBOUND(lint6,3) /= 1) .OR. &
          (UBOUND(lint6,4) /= 10) .OR. (LBOUND(lint6,4) /= 1) .OR. &
          (UBOUND(lint6,5) /= 10) .OR. (LBOUND(lint6,5) /= 1) .OR. &
          (UBOUND(lint6,6) /= 10) .OR. (LBOUND(lint6,6) /= 1)
      ASSERT(.NOT.test,'dmallocA(lint6,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(lint6,100,100,100,100,100,100)
      test=(.NOT.ALLOCATED(lint6)) .OR. ANY(lint6 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint6,1) /= 10) .OR. (LBOUND(lint6,1) /= 1) .OR. &
          (UBOUND(lint6,2) /= 10) .OR. (LBOUND(lint6,2) /= 1) .OR. &
          (UBOUND(lint6,3) /= 10) .OR. (LBOUND(lint6,3) /= 1) .OR. &
          (UBOUND(lint6,4) /= 10) .OR. (LBOUND(lint6,4) /= 1) .OR. &
          (UBOUND(lint6,5) /= 10) .OR. (LBOUND(lint6,5) /= 1) .OR. &
          (UBOUND(lint6,6) /= 10) .OR. (LBOUND(lint6,6) /= 1)
      ASSERT(.NOT.test,'dmallocA(lint6,100,100,100,100,100,100)')

      CALL demallocA(lint6)
      test=ALLOCATED(lint6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint6)')

      CALL demallocA(lint6)
      test=ALLOCATED(lint6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint6)')

      CALL dmalloc0A(lint6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(lint6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(lint6)) .OR. ANY(lint6 /= 0) .OR. &
          (UBOUND(lint6,1) /= 8) .OR. (LBOUND(lint6,1) /= -1) .OR. &
          (UBOUND(lint6,2) /= 8) .OR. (LBOUND(lint6,2) /= -1) .OR. &
          (UBOUND(lint6,3) /= 8) .OR. (LBOUND(lint6,3) /= -1) .OR. &
          (UBOUND(lint6,4) /= 8) .OR. (LBOUND(lint6,4) /= -1) .OR. &
          (UBOUND(lint6,5) /= 8) .OR. (LBOUND(lint6,5) /= -1) .OR. &
          (UBOUND(lint6,6) /= 8) .OR. (LBOUND(lint6,6) /= -1)
      ASSERT(.NOT.test,'dmallocA(lint6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(lint6)) .OR. ANY(lint6 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint6,1) /= 8) .OR. (LBOUND(lint6,1) /= -1) .OR. &
          (UBOUND(lint6,2) /= 8) .OR. (LBOUND(lint6,2) /= -1) .OR. &
          (UBOUND(lint6,3) /= 8) .OR. (LBOUND(lint6,3) /= -1) .OR. &
          (UBOUND(lint6,4) /= 8) .OR. (LBOUND(lint6,4) /= -1) .OR. &
          (UBOUND(lint6,5) /= 8) .OR. (LBOUND(lint6,5) /= -1) .OR. &
          (UBOUND(lint6,6) /= 8) .OR. (LBOUND(lint6,6) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(lint6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ALLOCATED(lint7)) .OR. ANY(lint7 /= 0) .OR. &
          (UBOUND(lint7,1) /= 10) .OR. (LBOUND(lint7,1) /= 1) .OR. &
          (UBOUND(lint7,2) /= 10) .OR. (LBOUND(lint7,2) /= 1) .OR. &
          (UBOUND(lint7,3) /= 10) .OR. (LBOUND(lint7,3) /= 1) .OR. &
          (UBOUND(lint7,4) /= 10) .OR. (LBOUND(lint7,4) /= 1) .OR. &
          (UBOUND(lint7,5) /= 10) .OR. (LBOUND(lint7,5) /= 1) .OR. &
          (UBOUND(lint7,6) /= 10) .OR. (LBOUND(lint7,6) /= 1) .OR. &
          (UBOUND(lint7,7) /= 10) .OR. (LBOUND(lint7,7) /= 1)
      ASSERT(.NOT.test,'dmallocA(lint7,10,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(lint7,100,100,100,100,100,100,100)
      test=(.NOT.ALLOCATED(lint7)) .OR. ANY(lint7 /= 0) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint7,1) /= 10) .OR. (LBOUND(lint7,1) /= 1) .OR. &
          (UBOUND(lint7,2) /= 10) .OR. (LBOUND(lint7,2) /= 1) .OR. &
          (UBOUND(lint7,3) /= 10) .OR. (LBOUND(lint7,3) /= 1) .OR. &
          (UBOUND(lint7,4) /= 10) .OR. (LBOUND(lint7,4) /= 1) .OR. &
          (UBOUND(lint7,5) /= 10) .OR. (LBOUND(lint7,5) /= 1) .OR. &
          (UBOUND(lint7,6) /= 10) .OR. (LBOUND(lint7,6) /= 1) .OR. &
          (UBOUND(lint7,7) /= 10) .OR. (LBOUND(lint7,7) /= 1)
      ASSERT(.NOT.test,'dmallocA(lint7,100,100,100,100,100,100,100)')

      CALL demallocA(lint7)
      test=ALLOCATED(lint7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint7)')

      CALL demallocA(lint7)
      test=ALLOCATED(lint7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(lint7)')

      CALL dmalloc0A(lint7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(lint7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(lint7)) .OR. ANY(lint7 /= 0) .OR. &
          (UBOUND(lint7,1) /= 8) .OR. (LBOUND(lint7,1) /= -1) .OR. &
          (UBOUND(lint7,2) /= 8) .OR. (LBOUND(lint7,2) /= -1) .OR. &
          (UBOUND(lint7,3) /= 8) .OR. (LBOUND(lint7,3) /= -1) .OR. &
          (UBOUND(lint7,4) /= 8) .OR. (LBOUND(lint7,4) /= -1) .OR. &
          (UBOUND(lint7,5) /= 8) .OR. (LBOUND(lint7,5) /= -1) .OR. &
          (UBOUND(lint7,6) /= 8) .OR. (LBOUND(lint7,6) /= -1) .OR. &
          (UBOUND(lint7,7) /= 8) .OR. (LBOUND(lint7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(lint7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(lint7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(lint7)) .OR. ANY(lint7 /= 0)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(lint7,1) /= 8) .OR. (LBOUND(lint7,1) /= -1) .OR. &
          (UBOUND(lint7,2) /= 8) .OR. (LBOUND(lint7,2) /= -1) .OR. &
          (UBOUND(lint7,3) /= 8) .OR. (LBOUND(lint7,3) /= -1) .OR. &
          (UBOUND(lint7,4) /= 8) .OR. (LBOUND(lint7,4) /= -1) .OR. &
          (UBOUND(lint7,5) /= 8) .OR. (LBOUND(lint7,5) /= -1) .OR. &
          (UBOUND(lint7,6) /= 8) .OR. (LBOUND(lint7,6) /= -1) .OR. &
          (UBOUND(lint7,7) /= 8) .OR. (LBOUND(lint7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(lint7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')

      CALL demallocA(lint7)

    ENDSUBROUTINE testLONGINTA
!
!-------------------------------------------------------------------------------
! Test allocation/deallocation for single precision
    SUBROUTINE testSINGLEP()
      REAL(SSK),POINTER :: sgl1(:)
      REAL(SSK),POINTER :: sgl2(:,:)
      REAL(SSK),POINTER :: sgl3(:,:,:)
      REAL(SSK),POINTER :: sgl4(:,:,:,:)
      REAL(SSK),POINTER :: sgl5(:,:,:,:,:)
      REAL(SSK),POINTER :: sgl6(:,:,:,:,:,:)
      REAL(SSK),POINTER :: sgl7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0

      NULLIFY(sgl1,sgl2,sgl3,sgl3,sgl4,sgl5,sgl6,sgl7)
!
! rank 1 variable
      CALL dmallocP(sgl1,-10)
      CALL dmallocP(sgl1,10)
      test=(.NOT.ASSOCIATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. &
          (UBOUND(sgl1,1) /= 10) .OR. (LBOUND(sgl1,1) /= 1)
      ASSERT(.NOT.test,'dmallocP(sgl1,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl1,100)
      test=(.NOT.ASSOCIATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl1,1) /= 10) .OR. (LBOUND(sgl1,1) /= 1)
      ASSERT(.NOT.test,'dmallocP(sgl1,100)')

      CALL demallocP(sgl1)
      test=ASSOCIATED(sgl1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl1)')

      CALL demallocP(sgl1)
      test=ASSOCIATED(sgl1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl1)')

      CALL dmalloc0P(sgl1,8,-1)
      CALL dmalloc0P(sgl1,-1,8)
      test=(.NOT.ASSOCIATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. &
          (UBOUND(sgl1,1) /= 8) .OR. (LBOUND(sgl1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(sgl1,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl1,-1,1)
      test=(.NOT.ASSOCIATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl1,1) /= 8) .OR. (LBOUND(sgl1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(sgl1,-1,1)')

      CALL demallocP(sgl1)
!
! rank 2 variable
      NULLIFY(sgl2)
      CALL dmallocP(sgl2,-10,10)
      CALL dmallocP(sgl2,10,-10)
      CALL dmallocP(sgl2,10,10)
      test=(.NOT.ASSOCIATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. &
          (UBOUND(sgl2,1) /= 10) .OR. (LBOUND(sgl2,1) /= 1) .OR. &
          (UBOUND(sgl2,2) /= 10) .OR. (LBOUND(sgl2,2) /= 1)
      ASSERT(.NOT.test,'dmallocP(sgl2,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl2,100,100)
      test=(.NOT.ASSOCIATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl2,1) /= 10) .OR. (LBOUND(sgl2,1) /= 1) .OR. &
          (UBOUND(sgl2,2) /= 10) .OR. (LBOUND(sgl2,2) /= 1)
      ASSERT(.NOT.test,'dmallocP(sgl2,100,100)')

      CALL demallocP(sgl2)
      test=ASSOCIATED(sgl2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl2)')

      CALL demallocP(sgl2)
      test=ASSOCIATED(sgl2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl2)')

      CALL dmalloc0P(sgl2,8,-1,-1,8)
      CALL dmalloc0P(sgl2,-1,8,8,-1)
      CALL dmalloc0P(sgl2,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. &
          (UBOUND(sgl2,1) /= 8) .OR. (LBOUND(sgl2,1) /= -1) .OR. &
          (UBOUND(sgl2,2) /= 8) .OR. (LBOUND(sgl2,2) /= -1)
      ASSERT(.NOT.test,'CALL CALL dmalloc0P(sgl2,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl2,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl2,1) /= 8) .OR. (LBOUND(sgl2,1) /= -1) .OR. &
          (UBOUND(sgl2,2) /= 8) .OR. (LBOUND(sgl2,2) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(sgl2,-1,1,-1,1)')

      CALL demallocP(sgl2)
!
! rank 3 variable
      NULLIFY(sgl3)
      CALL dmallocP(sgl3,-10,10,10)
      CALL dmallocP(sgl3,10,-10,10)
      CALL dmallocP(sgl3,10,10,-10)
      CALL dmallocP(sgl3,10,10,10)
      test=(.NOT.ASSOCIATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. &
          (UBOUND(sgl3,1) /= 10) .OR. (LBOUND(sgl3,1) /= 1) .OR. &
          (UBOUND(sgl3,2) /= 10) .OR. (LBOUND(sgl3,2) /= 1) .OR. &
          (UBOUND(sgl3,3) /= 10) .OR. (LBOUND(sgl3,3) /= 1)
      ASSERT(.NOT.test,'dmallocP(sgl3,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl3,100,100,100)
      test=(.NOT.ASSOCIATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl3,1) /= 10) .OR. (LBOUND(sgl3,1) /= 1) .OR. &
          (UBOUND(sgl3,2) /= 10) .OR. (LBOUND(sgl3,2) /= 1) .OR. &
          (UBOUND(sgl3,3) /= 10) .OR. (LBOUND(sgl3,3) /= 1)
      ASSERT(.NOT.test,'dmallocP(sgl3,100,100,100)')

      CALL demallocP(sgl3)
      test=ASSOCIATED(sgl3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl3)')

      CALL demallocP(sgl3)
      test=ASSOCIATED(sgl3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl3)')

      CALL dmalloc0P(sgl3,8,-1,-1,8,-1,8)
      CALL dmalloc0P(sgl3,-1,8,8,-1,-1,8)
      CALL dmalloc0P(sgl3,-1,8,-1,8,8,-1)
      CALL dmalloc0P(sgl3,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. &
          (UBOUND(sgl3,1) /= 8) .OR. (LBOUND(sgl3,1) /= -1) .OR. &
          (UBOUND(sgl3,2) /= 8) .OR. (LBOUND(sgl3,2) /= -1) .OR. &
          (UBOUND(sgl3,3) /= 8) .OR. (LBOUND(sgl3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(sgl3,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl3,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl3,1) /= 8) .OR. (LBOUND(sgl3,1) /= -1) .OR. &
          (UBOUND(sgl3,2) /= 8) .OR. (LBOUND(sgl3,2) /= -1) .OR. &
          (UBOUND(sgl3,3) /= 8) .OR. (LBOUND(sgl3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(sgl3,-1,1,-1,1,-1,1)')

      CALL demallocP(sgl3)
!
! rank 4 variable
      NULLIFY(sgl4)
      CALL dmallocP(sgl4,-10,10,10,10)
      CALL dmallocP(sgl4,10,-10,10,10)
      CALL dmallocP(sgl4,10,10,-10,10)
      CALL dmallocP(sgl4,10,10,10,-10)
      CALL dmallocP(sgl4,10,10,10,10)
      test=(.NOT.ASSOCIATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. &
          (UBOUND(sgl4,1) /= 10) .OR. (LBOUND(sgl4,1) /= 1) .OR. &
          (UBOUND(sgl4,2) /= 10) .OR. (LBOUND(sgl4,2) /= 1) .OR. &
          (UBOUND(sgl4,3) /= 10) .OR. (LBOUND(sgl4,3) /= 1) .OR. &
          (UBOUND(sgl4,4) /= 10) .OR. (LBOUND(sgl4,4) /= 1)
      ASSERT(.NOT.test,'dmallocP(sgl4,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl4,100,100,100,100)
      test=(.NOT.ASSOCIATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl4,1) /= 10) .OR. (LBOUND(sgl4,1) /= 1) .OR. &
          (UBOUND(sgl4,2) /= 10) .OR. (LBOUND(sgl4,2) /= 1) .OR. &
          (UBOUND(sgl4,3) /= 10) .OR. (LBOUND(sgl4,3) /= 1) .OR. &
          (UBOUND(sgl4,4) /= 10) .OR. (LBOUND(sgl4,4) /= 1)
      ASSERT(.NOT.test,'Rednundant CALL dmallocP(sgl4,100,100,100,100)')

      CALL demallocP(sgl4)
      test=ASSOCIATED(sgl4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl4)')

      CALL demallocP(sgl4)
      test=ASSOCIATED(sgl4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl4)')

      CALL dmalloc0P(sgl4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(sgl4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(sgl4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(sgl4,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. &
          (UBOUND(sgl4,1) /= 8) .OR. (LBOUND(sgl4,1) /= -1) .OR. &
          (UBOUND(sgl4,2) /= 8) .OR. (LBOUND(sgl4,2) /= -1) .OR. &
          (UBOUND(sgl4,3) /= 8) .OR. (LBOUND(sgl4,3) /= -1) .OR. &
          (UBOUND(sgl4,4) /= 8) .OR. (LBOUND(sgl4,4) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(sgl4,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl4,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl4,1) /= 8) .OR. (LBOUND(sgl4,1) /= -1) .OR. &
          (UBOUND(sgl4,2) /= 8) .OR. (LBOUND(sgl4,2) /= -1) .OR. &
          (UBOUND(sgl4,3) /= 8) .OR. (LBOUND(sgl4,3) /= -1) .OR. &
          (UBOUND(sgl4,4) /= 8) .OR. (LBOUND(sgl4,4) /= -1)
      ASSERT(.NOT.test,'Rednundant CALL dmalloc0P(sgl4,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ASSOCIATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK) .OR. &
          (UBOUND(sgl5,1) /= 10) .OR. (LBOUND(sgl5,1) /= 1) .OR. &
          (UBOUND(sgl5,2) /= 10) .OR. (LBOUND(sgl5,2) /= 1) .OR. &
          (UBOUND(sgl5,3) /= 10) .OR. (LBOUND(sgl5,3) /= 1) .OR. &
          (UBOUND(sgl5,4) /= 10) .OR. (LBOUND(sgl5,4) /= 1) .OR. &
          (UBOUND(sgl5,5) /= 10) .OR. (LBOUND(sgl5,5) /= 1)
      ASSERT(.NOT.test,'dmallocP(sgl5,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl5,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl5,1) /= 10) .OR. (LBOUND(sgl5,1) /= 1) .OR. &
          (UBOUND(sgl5,2) /= 10) .OR. (LBOUND(sgl5,2) /= 1) .OR. &
          (UBOUND(sgl5,3) /= 10) .OR. (LBOUND(sgl5,3) /= 1) .OR. &
          (UBOUND(sgl5,4) /= 10) .OR. (LBOUND(sgl5,4) /= 1) .OR. &
          (UBOUND(sgl5,5) /= 10) .OR. (LBOUND(sgl5,5) /= 1)
      ASSERT(.NOT.test,'dmallocP(sgl5,100,100,100,100,100)')

      CALL demallocP(sgl5)
      test=ASSOCIATED(sgl5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl5)')

      CALL demallocP(sgl5)
      test=ASSOCIATED(sgl5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl5)')

      CALL dmalloc0P(sgl5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(sgl5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(sgl5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(sgl5,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK) .OR. &
          (UBOUND(sgl5,1) /= 8) .OR. (LBOUND(sgl5,1) /= -1) .OR. &
          (UBOUND(sgl5,2) /= 8) .OR. (LBOUND(sgl5,2) /= -1) .OR. &
          (UBOUND(sgl5,3) /= 8) .OR. (LBOUND(sgl5,3) /= -1) .OR. &
          (UBOUND(sgl5,4) /= 8) .OR. (LBOUND(sgl5,4) /= -1) .OR. &
          (UBOUND(sgl5,5) /= 8) .OR. (LBOUND(sgl5,5) /= -1)
      ASSERT(.NOT.test,'dmallocP(sgl5,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl5,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl5,1) /= 8) .OR. (LBOUND(sgl5,1) /= -1) .OR. &
          (UBOUND(sgl5,2) /= 8) .OR. (LBOUND(sgl5,2) /= -1) .OR. &
          (UBOUND(sgl5,3) /= 8) .OR. (LBOUND(sgl5,3) /= -1) .OR. &
          (UBOUND(sgl5,4) /= 8) .OR. (LBOUND(sgl5,4) /= -1) .OR. &
          (UBOUND(sgl5,5) /= 8) .OR. (LBOUND(sgl5,5) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(sgl5,-1,1,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ASSOCIATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK) .OR. &
          (UBOUND(sgl6,1) /= 10) .OR. (LBOUND(sgl6,1) /= 1) .OR. &
          (UBOUND(sgl6,2) /= 10) .OR. (LBOUND(sgl6,2) /= 1) .OR. &
          (UBOUND(sgl6,3) /= 10) .OR. (LBOUND(sgl6,3) /= 1) .OR. &
          (UBOUND(sgl6,4) /= 10) .OR. (LBOUND(sgl6,4) /= 1) .OR. &
          (UBOUND(sgl6,5) /= 10) .OR. (LBOUND(sgl6,5) /= 1) .OR. &
          (UBOUND(sgl6,6) /= 10) .OR. (LBOUND(sgl6,6) /= 1)
      ASSERT(.NOT.test,'dmallocP(sgl6,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl6,100,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl6,1) /= 10) .OR. (LBOUND(sgl6,1) /= 1) .OR. &
          (UBOUND(sgl6,2) /= 10) .OR. (LBOUND(sgl6,2) /= 1) .OR. &
          (UBOUND(sgl6,3) /= 10) .OR. (LBOUND(sgl6,3) /= 1) .OR. &
          (UBOUND(sgl6,4) /= 10) .OR. (LBOUND(sgl6,4) /= 1) .OR. &
          (UBOUND(sgl6,5) /= 10) .OR. (LBOUND(sgl6,5) /= 1) .OR. &
          (UBOUND(sgl6,6) /= 10) .OR. (LBOUND(sgl6,6) /= 1)
      ASSERT(.NOT.test,'dmallocP(sgl6,100,100,100,100,100,100)')

      CALL demallocP(sgl6)
      test=ASSOCIATED(sgl6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl6)')

      CALL demallocP(sgl6)
      test=ASSOCIATED(sgl6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl6)')

      CALL dmalloc0P(sgl6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(sgl6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK) .OR. &
          (UBOUND(sgl6,1) /= 8) .OR. (LBOUND(sgl6,1) /= -1) .OR. &
          (UBOUND(sgl6,2) /= 8) .OR. (LBOUND(sgl6,2) /= -1) .OR. &
          (UBOUND(sgl6,3) /= 8) .OR. (LBOUND(sgl6,3) /= -1) .OR. &
          (UBOUND(sgl6,4) /= 8) .OR. (LBOUND(sgl6,4) /= -1) .OR. &
          (UBOUND(sgl6,5) /= 8) .OR. (LBOUND(sgl6,5) /= -1) .OR. &
          (UBOUND(sgl6,6) /= 8) .OR. (LBOUND(sgl6,6) /= -1)
      ASSERT(.NOT.test,'dmallocP(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl6,1) /= 8) .OR. (LBOUND(sgl6,1) /= -1) .OR. &
          (UBOUND(sgl6,2) /= 8) .OR. (LBOUND(sgl6,2) /= -1) .OR. &
          (UBOUND(sgl6,3) /= 8) .OR. (LBOUND(sgl6,3) /= -1) .OR. &
          (UBOUND(sgl6,4) /= 8) .OR. (LBOUND(sgl6,4) /= -1) .OR. &
          (UBOUND(sgl6,5) /= 8) .OR. (LBOUND(sgl6,5) /= -1) .OR. &
          (UBOUND(sgl6,6) /= 8) .OR. (LBOUND(sgl6,6) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(sgl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ASSOCIATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK) .OR. &
          (UBOUND(sgl7,1) /= 10) .OR. (LBOUND(sgl7,1) /= 1) .OR. &
          (UBOUND(sgl7,2) /= 10) .OR. (LBOUND(sgl7,2) /= 1) .OR. &
          (UBOUND(sgl7,3) /= 10) .OR. (LBOUND(sgl7,3) /= 1) .OR. &
          (UBOUND(sgl7,4) /= 10) .OR. (LBOUND(sgl7,4) /= 1) .OR. &
          (UBOUND(sgl7,5) /= 10) .OR. (LBOUND(sgl7,5) /= 1) .OR. &
          (UBOUND(sgl7,6) /= 10) .OR. (LBOUND(sgl7,6) /= 1) .OR. &
          (UBOUND(sgl7,7) /= 10) .OR. (LBOUND(sgl7,7) /= 1)
      ASSERT(.NOT.test,'dmallocP(sgl7,10,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(sgl7,100,100,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl7,1) /= 10) .OR. (LBOUND(sgl7,1) /= 1) .OR. &
          (UBOUND(sgl7,2) /= 10) .OR. (LBOUND(sgl7,2) /= 1) .OR. &
          (UBOUND(sgl7,3) /= 10) .OR. (LBOUND(sgl7,3) /= 1) .OR. &
          (UBOUND(sgl7,4) /= 10) .OR. (LBOUND(sgl7,4) /= 1) .OR. &
          (UBOUND(sgl7,5) /= 10) .OR. (LBOUND(sgl7,5) /= 1) .OR. &
          (UBOUND(sgl7,6) /= 10) .OR. (LBOUND(sgl7,6) /= 1) .OR. &
          (UBOUND(sgl7,7) /= 10) .OR. (LBOUND(sgl7,7) /= 1)
      ASSERT(.NOT.test,'dmallocP(sgl7,100,100,100,100,100,100,100)')

      CALL demallocP(sgl7)
      test=ASSOCIATED(sgl7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl7)')

      CALL demallocP(sgl7)
      test=ASSOCIATED(sgl7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(sgl7)')

      CALL dmalloc0P(sgl7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(sgl7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK) .OR. &
          (UBOUND(sgl7,1) /= 8) .OR. (LBOUND(sgl7,1) /= -1) .OR. &
          (UBOUND(sgl7,2) /= 8) .OR. (LBOUND(sgl7,2) /= -1) .OR. &
          (UBOUND(sgl7,3) /= 8) .OR. (LBOUND(sgl7,3) /= -1) .OR. &
          (UBOUND(sgl7,4) /= 8) .OR. (LBOUND(sgl7,4) /= -1) .OR. &
          (UBOUND(sgl7,5) /= 8) .OR. (LBOUND(sgl7,5) /= -1) .OR. &
          (UBOUND(sgl7,6) /= 8) .OR. (LBOUND(sgl7,6) /= -1) .OR. &
          (UBOUND(sgl7,7) /= 8) .OR. (LBOUND(sgl7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(sgl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl7,1) /= 8) .OR. (LBOUND(sgl7,1) /= -1) .OR. &
          (UBOUND(sgl7,2) /= 8) .OR. (LBOUND(sgl7,2) /= -1) .OR. &
          (UBOUND(sgl7,3) /= 8) .OR. (LBOUND(sgl7,3) /= -1) .OR. &
          (UBOUND(sgl7,4) /= 8) .OR. (LBOUND(sgl7,4) /= -1) .OR. &
          (UBOUND(sgl7,5) /= 8) .OR. (LBOUND(sgl7,5) /= -1) .OR. &
          (UBOUND(sgl7,6) /= 8) .OR. (LBOUND(sgl7,6) /= -1) .OR. &
          (UBOUND(sgl7,7) /= 8) .OR. (LBOUND(sgl7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(sgl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')

      CALL demallocP(sgl7)

    ENDSUBROUTINE testSINGLEP
!
!-------------------------------------------------------------------------------
! Test allocation/deallocation for single precision
    SUBROUTINE testSINGLEA()
      REAL(SSK),ALLOCATABLE :: sgl1(:)
      REAL(SSK),ALLOCATABLE :: sgl2(:,:)
      REAL(SSK),ALLOCATABLE :: sgl3(:,:,:)
      REAL(SSK),ALLOCATABLE :: sgl4(:,:,:,:)
      REAL(SSK),ALLOCATABLE :: sgl5(:,:,:,:,:)
      REAL(SSK),ALLOCATABLE :: sgl6(:,:,:,:,:,:)
      REAL(SSK),ALLOCATABLE :: sgl7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0
!
! rank 1 variable
      CALL dmallocA(sgl1,-10)
      CALL dmallocA(sgl1,10)
      test=(.NOT.ALLOCATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. &
          (UBOUND(sgl1,1) /= 10) .OR. (LBOUND(sgl1,1) /= 1)
      ASSERT(.NOT.test,'dmallocA(sgl1,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl1,100)
      test=(.NOT.ALLOCATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl1,1) /= 10) .OR. (LBOUND(sgl1,1) /= 1)
      ASSERT(.NOT.test,'dmallocA(sgl1,100)')

      CALL demallocA(sgl1)
      test=ALLOCATED(sgl1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl1)')

      CALL demallocA(sgl1)
      test=ALLOCATED(sgl1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl1)')

      CALL dmalloc0A(sgl1,8,-1)
      CALL dmalloc0A(sgl1,-1,8)
      test=(.NOT.ALLOCATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. &
          (UBOUND(sgl1,1) /= 8) .OR. (LBOUND(sgl1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(sgl1,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl1,-1,1)
      test=(.NOT.ALLOCATED(sgl1)) .OR. ANY(sgl1 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl1,1) /= 8) .OR. (LBOUND(sgl1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(sgl1,-1,1)')

      CALL demallocA(sgl1)
!
! rank 2 variable
      CALL dmallocA(sgl2,-10,10)
      CALL dmallocA(sgl2,10,-10)
      CALL dmallocA(sgl2,10,10)
      test=(.NOT.ALLOCATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. &
          (UBOUND(sgl2,1) /= 10) .OR. (LBOUND(sgl2,1) /= 1) .OR. &
          (UBOUND(sgl2,2) /= 10) .OR. (LBOUND(sgl2,2) /= 1)
      ASSERT(.NOT.test,'dmallocA(sgl2,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl2,100,100)
      test=(.NOT.ALLOCATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl2,1) /= 10) .OR. (LBOUND(sgl2,1) /= 1) .OR. &
          (UBOUND(sgl2,2) /= 10) .OR. (LBOUND(sgl2,2) /= 1)
      ASSERT(.NOT.test,'dmallocA(sgl2,100,100)')

      CALL demallocA(sgl2)
      test=ALLOCATED(sgl2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl2)')

      CALL demallocA(sgl2)
      test=ALLOCATED(sgl2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl2)')

      CALL dmalloc0A(sgl2,8,-1,-1,8)
      CALL dmalloc0A(sgl2,-1,8,8,-1)
      CALL dmalloc0A(sgl2,-1,8,-1,8)
      test=(.NOT.ALLOCATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. &
          (UBOUND(sgl2,1) /= 8) .OR. (LBOUND(sgl2,1) /= -1) .OR. &
          (UBOUND(sgl2,2) /= 8) .OR. (LBOUND(sgl2,2) /= -1)
      ASSERT(.NOT.test,'CALL CALL dmalloc0A(sgl2,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl2,-1,1,-1,1)
      test=(.NOT.ALLOCATED(sgl2)) .OR. ANY(sgl2 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl2,1) /= 8) .OR. (LBOUND(sgl2,1) /= -1) .OR. &
          (UBOUND(sgl2,2) /= 8) .OR. (LBOUND(sgl2,2) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(sgl2,-1,1,-1,1)')

      CALL demallocA(sgl2)
!
! rank 3 variable
      CALL dmallocA(sgl3,-10,10,10)
      CALL dmallocA(sgl3,10,-10,10)
      CALL dmallocA(sgl3,10,10,-10)
      CALL dmallocA(sgl3,10,10,10)
      test=(.NOT.ALLOCATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. &
          (UBOUND(sgl3,1) /= 10) .OR. (LBOUND(sgl3,1) /= 1) .OR. &
          (UBOUND(sgl3,2) /= 10) .OR. (LBOUND(sgl3,2) /= 1) .OR. &
          (UBOUND(sgl3,3) /= 10) .OR. (LBOUND(sgl3,3) /= 1)
      ASSERT(.NOT.test,'dmallocA(sgl3,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl3,100,100,100)
      test=(.NOT.ALLOCATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl3,1) /= 10) .OR. (LBOUND(sgl3,1) /= 1) .OR. &
          (UBOUND(sgl3,2) /= 10) .OR. (LBOUND(sgl3,2) /= 1) .OR. &
          (UBOUND(sgl3,3) /= 10) .OR. (LBOUND(sgl3,3) /= 1)
      ASSERT(.NOT.test,'dmallocA(sgl3,100,100,100)')

      CALL demallocA(sgl3)
      test=ALLOCATED(sgl3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl3)')

      CALL demallocA(sgl3)
      test=ALLOCATED(sgl3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl3)')

      CALL dmalloc0A(sgl3,8,-1,-1,8,-1,8)
      CALL dmalloc0A(sgl3,-1,8,8,-1,-1,8)
      CALL dmalloc0A(sgl3,-1,8,-1,8,8,-1)
      CALL dmalloc0A(sgl3,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. &
          (UBOUND(sgl3,1) /= 8) .OR. (LBOUND(sgl3,1) /= -1) .OR. &
          (UBOUND(sgl3,2) /= 8) .OR. (LBOUND(sgl3,2) /= -1) .OR. &
          (UBOUND(sgl3,3) /= 8) .OR. (LBOUND(sgl3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(sgl3,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl3,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(sgl3)) .OR. ANY(sgl3 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl3,1) /= 8) .OR. (LBOUND(sgl3,1) /= -1) .OR. &
          (UBOUND(sgl3,2) /= 8) .OR. (LBOUND(sgl3,2) /= -1) .OR. &
          (UBOUND(sgl3,3) /= 8) .OR. (LBOUND(sgl3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(sgl3,-1,1,-1,1,-1,1)')

      CALL demallocA(sgl3)
!
! rank 4 variable
      CALL dmallocA(sgl4,-10,10,10,10)
      CALL dmallocA(sgl4,10,-10,10,10)
      CALL dmallocA(sgl4,10,10,-10,10)
      CALL dmallocA(sgl4,10,10,10,-10)
      CALL dmallocA(sgl4,10,10,10,10)
      test=(.NOT.ALLOCATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. &
          (UBOUND(sgl4,1) /= 10) .OR. (LBOUND(sgl4,1) /= 1) .OR. &
          (UBOUND(sgl4,2) /= 10) .OR. (LBOUND(sgl4,2) /= 1) .OR. &
          (UBOUND(sgl4,3) /= 10) .OR. (LBOUND(sgl4,3) /= 1) .OR. &
          (UBOUND(sgl4,4) /= 10) .OR. (LBOUND(sgl4,4) /= 1)
      ASSERT(.NOT.test,'dmallocA(sgl4,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl4,100,100,100,100)
      test=(.NOT.ALLOCATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl4,1) /= 10) .OR. (LBOUND(sgl4,1) /= 1) .OR. &
          (UBOUND(sgl4,2) /= 10) .OR. (LBOUND(sgl4,2) /= 1) .OR. &
          (UBOUND(sgl4,3) /= 10) .OR. (LBOUND(sgl4,3) /= 1) .OR. &
          (UBOUND(sgl4,4) /= 10) .OR. (LBOUND(sgl4,4) /= 1)
      ASSERT(.NOT.test,'Rednundant CALL dmallocA(sgl4,100,100,100,100)')

      CALL demallocA(sgl4)
      test=ALLOCATED(sgl4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl4)')

      CALL demallocA(sgl4)
      test=ALLOCATED(sgl4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl4)')

      CALL dmalloc0A(sgl4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(sgl4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(sgl4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(sgl4,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. &
          (UBOUND(sgl4,1) /= 8) .OR. (LBOUND(sgl4,1) /= -1) .OR. &
          (UBOUND(sgl4,2) /= 8) .OR. (LBOUND(sgl4,2) /= -1) .OR. &
          (UBOUND(sgl4,3) /= 8) .OR. (LBOUND(sgl4,3) /= -1) .OR. &
          (UBOUND(sgl4,4) /= 8) .OR. (LBOUND(sgl4,4) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(sgl4,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl4,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(sgl4)) .OR. ANY(sgl4 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl4,1) /= 8) .OR. (LBOUND(sgl4,1) /= -1) .OR. &
          (UBOUND(sgl4,2) /= 8) .OR. (LBOUND(sgl4,2) /= -1) .OR. &
          (UBOUND(sgl4,3) /= 8) .OR. (LBOUND(sgl4,3) /= -1) .OR. &
          (UBOUND(sgl4,4) /= 8) .OR. (LBOUND(sgl4,4) /= -1)
      ASSERT(.NOT.test,'Rednundant CALL dmalloc0A(sgl4,-1,1,-1,1,-1,1,-1,1)')

      CALL demallocA(sgl4)
!
! rank 5 variable
      CALL dmallocA(sgl5,-10,10,10,10,10)
      CALL dmallocA(sgl5,10,-10,10,10,10)
      CALL dmallocA(sgl5,10,10,-10,10,10)
      CALL dmallocA(sgl5,10,10,10,-10,10)
      CALL dmallocA(sgl5,10,10,10,10,-10)
      CALL dmallocA(sgl5,10,10,10,10,10)
      test=(.NOT.ALLOCATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK) .OR. &
          (UBOUND(sgl5,1) /= 10) .OR. (LBOUND(sgl5,1) /= 1) .OR. &
          (UBOUND(sgl5,2) /= 10) .OR. (LBOUND(sgl5,2) /= 1) .OR. &
          (UBOUND(sgl5,3) /= 10) .OR. (LBOUND(sgl5,3) /= 1) .OR. &
          (UBOUND(sgl5,4) /= 10) .OR. (LBOUND(sgl5,4) /= 1) .OR. &
          (UBOUND(sgl5,5) /= 10) .OR. (LBOUND(sgl5,5) /= 1)
      ASSERT(.NOT.test,'dmallocA(sgl5,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl5,100,100,100,100,100)
      test=(.NOT.ALLOCATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl5,1) /= 10) .OR. (LBOUND(sgl5,1) /= 1) .OR. &
          (UBOUND(sgl5,2) /= 10) .OR. (LBOUND(sgl5,2) /= 1) .OR. &
          (UBOUND(sgl5,3) /= 10) .OR. (LBOUND(sgl5,3) /= 1) .OR. &
          (UBOUND(sgl5,4) /= 10) .OR. (LBOUND(sgl5,4) /= 1) .OR. &
          (UBOUND(sgl5,5) /= 10) .OR. (LBOUND(sgl5,5) /= 1)
      ASSERT(.NOT.test,'dmallocA(sgl5,100,100,100,100,100)')

      CALL demallocA(sgl5)
      test=ALLOCATED(sgl5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl5)')

      CALL demallocA(sgl5)
      test=ALLOCATED(sgl5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl5)')

      CALL dmalloc0A(sgl5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(sgl5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(sgl5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(sgl5,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK) .OR. &
          (UBOUND(sgl5,1) /= 8) .OR. (LBOUND(sgl5,1) /= -1) .OR. &
          (UBOUND(sgl5,2) /= 8) .OR. (LBOUND(sgl5,2) /= -1) .OR. &
          (UBOUND(sgl5,3) /= 8) .OR. (LBOUND(sgl5,3) /= -1) .OR. &
          (UBOUND(sgl5,4) /= 8) .OR. (LBOUND(sgl5,4) /= -1) .OR. &
          (UBOUND(sgl5,5) /= 8) .OR. (LBOUND(sgl5,5) /= -1)
      ASSERT(.NOT.test,'dmallocA(sgl5,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl5,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(sgl5)) .OR. ANY(sgl5 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl5,1) /= 8) .OR. (LBOUND(sgl5,1) /= -1) .OR. &
          (UBOUND(sgl5,2) /= 8) .OR. (LBOUND(sgl5,2) /= -1) .OR. &
          (UBOUND(sgl5,3) /= 8) .OR. (LBOUND(sgl5,3) /= -1) .OR. &
          (UBOUND(sgl5,4) /= 8) .OR. (LBOUND(sgl5,4) /= -1) .OR. &
          (UBOUND(sgl5,5) /= 8) .OR. (LBOUND(sgl5,5) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(sgl5,-1,1,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ALLOCATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK) .OR. &
          (UBOUND(sgl6,1) /= 10) .OR. (LBOUND(sgl6,1) /= 1) .OR. &
          (UBOUND(sgl6,2) /= 10) .OR. (LBOUND(sgl6,2) /= 1) .OR. &
          (UBOUND(sgl6,3) /= 10) .OR. (LBOUND(sgl6,3) /= 1) .OR. &
          (UBOUND(sgl6,4) /= 10) .OR. (LBOUND(sgl6,4) /= 1) .OR. &
          (UBOUND(sgl6,5) /= 10) .OR. (LBOUND(sgl6,5) /= 1) .OR. &
          (UBOUND(sgl6,6) /= 10) .OR. (LBOUND(sgl6,6) /= 1)
      ASSERT(.NOT.test,'dmallocA(sgl6,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl6,100,100,100,100,100,100)
      test=(.NOT.ALLOCATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl6,1) /= 10) .OR. (LBOUND(sgl6,1) /= 1) .OR. &
          (UBOUND(sgl6,2) /= 10) .OR. (LBOUND(sgl6,2) /= 1) .OR. &
          (UBOUND(sgl6,3) /= 10) .OR. (LBOUND(sgl6,3) /= 1) .OR. &
          (UBOUND(sgl6,4) /= 10) .OR. (LBOUND(sgl6,4) /= 1) .OR. &
          (UBOUND(sgl6,5) /= 10) .OR. (LBOUND(sgl6,5) /= 1) .OR. &
          (UBOUND(sgl6,6) /= 10) .OR. (LBOUND(sgl6,6) /= 1)
      ASSERT(.NOT.test,'dmallocA(sgl6,100,100,100,100,100,100)')

      CALL demallocA(sgl6)
      test=ALLOCATED(sgl6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl6)')

      CALL demallocA(sgl6)
      test=ALLOCATED(sgl6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl6)')

      CALL dmalloc0A(sgl6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(sgl6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK) .OR. &
          (UBOUND(sgl6,1) /= 8) .OR. (LBOUND(sgl6,1) /= -1) .OR. &
          (UBOUND(sgl6,2) /= 8) .OR. (LBOUND(sgl6,2) /= -1) .OR. &
          (UBOUND(sgl6,3) /= 8) .OR. (LBOUND(sgl6,3) /= -1) .OR. &
          (UBOUND(sgl6,4) /= 8) .OR. (LBOUND(sgl6,4) /= -1) .OR. &
          (UBOUND(sgl6,5) /= 8) .OR. (LBOUND(sgl6,5) /= -1) .OR. &
          (UBOUND(sgl6,6) /= 8) .OR. (LBOUND(sgl6,6) /= -1)
      ASSERT(.NOT.test,'dmallocA(sgl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(sgl6)) .OR. ANY(sgl6 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl6,1) /= 8) .OR. (LBOUND(sgl6,1) /= -1) .OR. &
          (UBOUND(sgl6,2) /= 8) .OR. (LBOUND(sgl6,2) /= -1) .OR. &
          (UBOUND(sgl6,3) /= 8) .OR. (LBOUND(sgl6,3) /= -1) .OR. &
          (UBOUND(sgl6,4) /= 8) .OR. (LBOUND(sgl6,4) /= -1) .OR. &
          (UBOUND(sgl6,5) /= 8) .OR. (LBOUND(sgl6,5) /= -1) .OR. &
          (UBOUND(sgl6,6) /= 8) .OR. (LBOUND(sgl6,6) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(sgl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ALLOCATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK) .OR. &
          (UBOUND(sgl7,1) /= 10) .OR. (LBOUND(sgl7,1) /= 1) .OR. &
          (UBOUND(sgl7,2) /= 10) .OR. (LBOUND(sgl7,2) /= 1) .OR. &
          (UBOUND(sgl7,3) /= 10) .OR. (LBOUND(sgl7,3) /= 1) .OR. &
          (UBOUND(sgl7,4) /= 10) .OR. (LBOUND(sgl7,4) /= 1) .OR. &
          (UBOUND(sgl7,5) /= 10) .OR. (LBOUND(sgl7,5) /= 1) .OR. &
          (UBOUND(sgl7,6) /= 10) .OR. (LBOUND(sgl7,6) /= 1) .OR. &
          (UBOUND(sgl7,7) /= 10) .OR. (LBOUND(sgl7,7) /= 1)
      ASSERT(.NOT.test,'dmallocA(sgl7,10,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(sgl7,100,100,100,100,100,100,100)
      test=(.NOT.ALLOCATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl7,1) /= 10) .OR. (LBOUND(sgl7,1) /= 1) .OR. &
          (UBOUND(sgl7,2) /= 10) .OR. (LBOUND(sgl7,2) /= 1) .OR. &
          (UBOUND(sgl7,3) /= 10) .OR. (LBOUND(sgl7,3) /= 1) .OR. &
          (UBOUND(sgl7,4) /= 10) .OR. (LBOUND(sgl7,4) /= 1) .OR. &
          (UBOUND(sgl7,5) /= 10) .OR. (LBOUND(sgl7,5) /= 1) .OR. &
          (UBOUND(sgl7,6) /= 10) .OR. (LBOUND(sgl7,6) /= 1) .OR. &
          (UBOUND(sgl7,7) /= 10) .OR. (LBOUND(sgl7,7) /= 1)
      ASSERT(.NOT.test,'dmallocA(sgl7,100,100,100,100,100,100,100)')

      CALL demallocA(sgl7)
      test=ALLOCATED(sgl7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl7)')

      CALL demallocA(sgl7)
      test=ALLOCATED(sgl7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(sgl7)')

      CALL dmalloc0A(sgl7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(sgl7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK) .OR. &
          (UBOUND(sgl7,1) /= 8) .OR. (LBOUND(sgl7,1) /= -1) .OR. &
          (UBOUND(sgl7,2) /= 8) .OR. (LBOUND(sgl7,2) /= -1) .OR. &
          (UBOUND(sgl7,3) /= 8) .OR. (LBOUND(sgl7,3) /= -1) .OR. &
          (UBOUND(sgl7,4) /= 8) .OR. (LBOUND(sgl7,4) /= -1) .OR. &
          (UBOUND(sgl7,5) /= 8) .OR. (LBOUND(sgl7,5) /= -1) .OR. &
          (UBOUND(sgl7,6) /= 8) .OR. (LBOUND(sgl7,6) /= -1) .OR. &
          (UBOUND(sgl7,7) /= 8) .OR. (LBOUND(sgl7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(sgl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(sgl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(sgl7)) .OR. ANY(sgl7 /= 0.0_SSK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(sgl7,1) /= 8) .OR. (LBOUND(sgl7,1) /= -1) .OR. &
          (UBOUND(sgl7,2) /= 8) .OR. (LBOUND(sgl7,2) /= -1) .OR. &
          (UBOUND(sgl7,3) /= 8) .OR. (LBOUND(sgl7,3) /= -1) .OR. &
          (UBOUND(sgl7,4) /= 8) .OR. (LBOUND(sgl7,4) /= -1) .OR. &
          (UBOUND(sgl7,5) /= 8) .OR. (LBOUND(sgl7,5) /= -1) .OR. &
          (UBOUND(sgl7,6) /= 8) .OR. (LBOUND(sgl7,6) /= -1) .OR. &
          (UBOUND(sgl7,7) /= 8) .OR. (LBOUND(sgl7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(sgl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')

      CALL demallocA(sgl7)

    ENDSUBROUTINE testSINGLEA
!
!-------------------------------------------------------------------------------
! Test allocation/deallocation for single precision
    SUBROUTINE testDOUBLEP()
      REAL(SDK),POINTER :: dbl1(:)
      REAL(SDK),POINTER :: dbl2(:,:)
      REAL(SDK),POINTER :: dbl3(:,:,:)
      REAL(SDK),POINTER :: dbl4(:,:,:,:)
      REAL(SDK),POINTER :: dbl5(:,:,:,:,:)
      REAL(SDK),POINTER :: dbl6(:,:,:,:,:,:)
      REAL(SDK),POINTER :: dbl7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0

      NULLIFY(dbl1,dbl2,dbl3,dbl4,dbl5,dbl6,dbl7)
!
! rank 1 variable
      CALL dmallocP(dbl1,-10)
      CALL dmallocP(dbl1,10)
      test=(.NOT.ASSOCIATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. &
          (UBOUND(dbl1,1) /= 10) .OR. (LBOUND(dbl1,1) /= 1)
      ASSERT(.NOT.test,'dmallocP(dbl1,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl1,100)
      test=(.NOT.ASSOCIATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl1,1) /= 10) .OR. (LBOUND(dbl1,1) /= 1)
      ASSERT(.NOT.test,'dmallocP(dbl1,100)')

      CALL demallocP(dbl1)
      test=ASSOCIATED(dbl1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(dbl1)')

      CALL demallocP(dbl1)
      test=ASSOCIATED(dbl1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(dbl1)')

      CALL dmalloc0P(dbl1,8,-1)
      CALL dmalloc0P(dbl1,-1,8)
      test=(.NOT.ASSOCIATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. &
          (UBOUND(dbl1,1) /= 8) .OR. (LBOUND(dbl1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(dbl1,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl1,-1,1)
      test=(.NOT.ASSOCIATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl1,1) /= 8) .OR. (LBOUND(dbl1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(dbl1,-1,1)')

      CALL demallocP(dbl1)
!
! rank 2 variable
      NULLIFY(dbl2)
      CALL dmallocP(dbl2,-10,10)
      CALL dmallocP(dbl2,10,-10)
      CALL dmallocP(dbl2,10,10)
      test=(.NOT.ASSOCIATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. &
          (UBOUND(dbl2,1) /= 10) .OR. (LBOUND(dbl2,1) /= 1) .OR. &
          (UBOUND(dbl2,2) /= 10) .OR. (LBOUND(dbl2,2) /= 1)
      ASSERT(.NOT.test,'dmallocP(dbl2,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl2,100,100)
      test=(.NOT.ASSOCIATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl2,1) /= 10) .OR. (LBOUND(dbl2,1) /= 1) .OR. &
          (UBOUND(dbl2,2) /= 10) .OR. (LBOUND(dbl2,2) /= 1)
      ASSERT(.NOT.test,'dmallocP(dbl2,100,100)')

      CALL demallocP(dbl2)
      test=ASSOCIATED(dbl2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(dbl2)')

      CALL demallocP(dbl2)
      test=ASSOCIATED(dbl2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(dbl2)')

      CALL dmalloc0P(dbl2,8,-1,-1,8)
      CALL dmalloc0P(dbl2,-1,8,8,-1)
      CALL dmalloc0P(dbl2,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. &
          (UBOUND(dbl2,1) /= 8) .OR. (LBOUND(dbl2,1) /= -1) .OR. &
          (UBOUND(dbl2,2) /= 8) .OR. (LBOUND(dbl2,2) /= -1)
      ASSERT(.NOT.test,'CALL CALL dmalloc0P(dbl2,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl2,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl2,1) /= 8) .OR. (LBOUND(dbl2,1) /= -1) .OR. &
          (UBOUND(dbl2,2) /= 8) .OR. (LBOUND(dbl2,2) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(dbl2,-1,1,-1,1)')

      CALL demallocP(dbl2)
!
! rank 3 variable
      NULLIFY(dbl3)
      CALL dmallocP(dbl3,-10,10,10)
      CALL dmallocP(dbl3,10,-10,10)
      CALL dmallocP(dbl3,10,10,-10)
      CALL dmallocP(dbl3,10,10,10)
      test=(.NOT.ASSOCIATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. &
          (UBOUND(dbl3,1) /= 10) .OR. (LBOUND(dbl3,1) /= 1) .OR. &
          (UBOUND(dbl3,2) /= 10) .OR. (LBOUND(dbl3,2) /= 1) .OR. &
          (UBOUND(dbl3,3) /= 10) .OR. (LBOUND(dbl3,3) /= 1)
      ASSERT(.NOT.test,'dmallocP(dbl3,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl3,100,100,100)
      test=(.NOT.ASSOCIATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl3,1) /= 10) .OR. (LBOUND(dbl3,1) /= 1) .OR. &
          (UBOUND(dbl3,2) /= 10) .OR. (LBOUND(dbl3,2) /= 1) .OR. &
          (UBOUND(dbl3,3) /= 10) .OR. (LBOUND(dbl3,3) /= 1)
      ASSERT(.NOT.test,'dmallocP(dbl3,100,100,100)')

      CALL demallocP(dbl3)
      test=ASSOCIATED(dbl3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(dbl3)')

      CALL demallocP(dbl3)
      test=ASSOCIATED(dbl3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(dbl3)')

      CALL dmalloc0P(dbl3,8,-1,-1,8,-1,8)
      CALL dmalloc0P(dbl3,-1,8,8,-1,-1,8)
      CALL dmalloc0P(dbl3,-1,8,-1,8,8,-1)
      CALL dmalloc0P(dbl3,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. &
          (UBOUND(dbl3,1) /= 8) .OR. (LBOUND(dbl3,1) /= -1) .OR. &
          (UBOUND(dbl3,2) /= 8) .OR. (LBOUND(dbl3,2) /= -1) .OR. &
          (UBOUND(dbl3,3) /= 8) .OR. (LBOUND(dbl3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(dbl3,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl3,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl3,1) /= 8) .OR. (LBOUND(dbl3,1) /= -1) .OR. &
          (UBOUND(dbl3,2) /= 8) .OR. (LBOUND(dbl3,2) /= -1) .OR. &
          (UBOUND(dbl3,3) /= 8) .OR. (LBOUND(dbl3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(dbl3,-1,1,-1,1,-1,1)')

      CALL demallocP(dbl3)
!
! rank 4 variable
      NULLIFY(dbl4)
      CALL dmallocP(dbl4,-10,10,10,10)
      CALL dmallocP(dbl4,10,-10,10,10)
      CALL dmallocP(dbl4,10,10,-10,10)
      CALL dmallocP(dbl4,10,10,10,-10)
      CALL dmallocP(dbl4,10,10,10,10)
      test=(.NOT.ASSOCIATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. &
          (UBOUND(dbl4,1) /= 10) .OR. (LBOUND(dbl4,1) /= 1) .OR. &
          (UBOUND(dbl4,2) /= 10) .OR. (LBOUND(dbl4,2) /= 1) .OR. &
          (UBOUND(dbl4,3) /= 10) .OR. (LBOUND(dbl4,3) /= 1) .OR. &
          (UBOUND(dbl4,4) /= 10) .OR. (LBOUND(dbl4,4) /= 1)
      ASSERT(.NOT.test,'dmallocP(dbl4,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl4,100,100,100,100)
      test=(.NOT.ASSOCIATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl4,1) /= 10) .OR. (LBOUND(dbl4,1) /= 1) .OR. &
          (UBOUND(dbl4,2) /= 10) .OR. (LBOUND(dbl4,2) /= 1) .OR. &
          (UBOUND(dbl4,3) /= 10) .OR. (LBOUND(dbl4,3) /= 1) .OR. &
          (UBOUND(dbl4,4) /= 10) .OR. (LBOUND(dbl4,4) /= 1)
      ASSERT(.NOT.test,'Rednundant CALL dmallocP(dbl4,100,100,100,100)')

      CALL demallocP(dbl4)
      test=ASSOCIATED(dbl4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(dbl4)')

      CALL demallocP(dbl4)
      test=ASSOCIATED(dbl4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(dbl4)')

      CALL dmalloc0P(dbl4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(dbl4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(dbl4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(dbl4,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. &
          (UBOUND(dbl4,1) /= 8) .OR. (LBOUND(dbl4,1) /= -1) .OR. &
          (UBOUND(dbl4,2) /= 8) .OR. (LBOUND(dbl4,2) /= -1) .OR. &
          (UBOUND(dbl4,3) /= 8) .OR. (LBOUND(dbl4,3) /= -1) .OR. &
          (UBOUND(dbl4,4) /= 8) .OR. (LBOUND(dbl4,4) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(dbl4,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl4,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl4,1) /= 8) .OR. (LBOUND(dbl4,1) /= -1) .OR. &
          (UBOUND(dbl4,2) /= 8) .OR. (LBOUND(dbl4,2) /= -1) .OR. &
          (UBOUND(dbl4,3) /= 8) .OR. (LBOUND(dbl4,3) /= -1) .OR. &
          (UBOUND(dbl4,4) /= 8) .OR. (LBOUND(dbl4,4) /= -1)
      ASSERT(.NOT.test,'Rednundant CALL dmalloc0P(dbl4,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ASSOCIATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK) .OR. &
          (UBOUND(dbl5,1) /= 10) .OR. (LBOUND(dbl5,1) /= 1) .OR. &
          (UBOUND(dbl5,2) /= 10) .OR. (LBOUND(dbl5,2) /= 1) .OR. &
          (UBOUND(dbl5,3) /= 10) .OR. (LBOUND(dbl5,3) /= 1) .OR. &
          (UBOUND(dbl5,4) /= 10) .OR. (LBOUND(dbl5,4) /= 1) .OR. &
          (UBOUND(dbl5,5) /= 10) .OR. (LBOUND(dbl5,5) /= 1)
      ASSERT(.NOT.test,'dmallocP(dbl5,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl5,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl5,1) /= 10) .OR. (LBOUND(dbl5,1) /= 1) .OR. &
          (UBOUND(dbl5,2) /= 10) .OR. (LBOUND(dbl5,2) /= 1) .OR. &
          (UBOUND(dbl5,3) /= 10) .OR. (LBOUND(dbl5,3) /= 1) .OR. &
          (UBOUND(dbl5,4) /= 10) .OR. (LBOUND(dbl5,4) /= 1) .OR. &
          (UBOUND(dbl5,5) /= 10) .OR. (LBOUND(dbl5,5) /= 1)
      ASSERT(.NOT.test,'dmallocP(dbl5,100,100,100,100,100)')

      CALL demallocP(dbl5)
      test=ASSOCIATED(dbl5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(dbl5)')

      CALL demallocP(dbl5)
      test=ASSOCIATED(dbl5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(dbl5)')

      CALL dmalloc0P(dbl5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(dbl5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(dbl5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(dbl5,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK) .OR. &
          (UBOUND(dbl5,1) /= 8) .OR. (LBOUND(dbl5,1) /= -1) .OR. &
          (UBOUND(dbl5,2) /= 8) .OR. (LBOUND(dbl5,2) /= -1) .OR. &
          (UBOUND(dbl5,3) /= 8) .OR. (LBOUND(dbl5,3) /= -1) .OR. &
          (UBOUND(dbl5,4) /= 8) .OR. (LBOUND(dbl5,4) /= -1) .OR. &
          (UBOUND(dbl5,5) /= 8) .OR. (LBOUND(dbl5,5) /= -1)
      ASSERT(.NOT.test,'dmallocP(dbl5,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl5,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl5,1) /= 8) .OR. (LBOUND(dbl5,1) /= -1) .OR. &
          (UBOUND(dbl5,2) /= 8) .OR. (LBOUND(dbl5,2) /= -1) .OR. &
          (UBOUND(dbl5,3) /= 8) .OR. (LBOUND(dbl5,3) /= -1) .OR. &
          (UBOUND(dbl5,4) /= 8) .OR. (LBOUND(dbl5,4) /= -1) .OR. &
          (UBOUND(dbl5,5) /= 8) .OR. (LBOUND(dbl5,5) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(dbl5,-1,1,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ASSOCIATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK) .OR. &
          (UBOUND(dbl6,1) /= 10) .OR. (LBOUND(dbl6,1) /= 1) .OR. &
          (UBOUND(dbl6,2) /= 10) .OR. (LBOUND(dbl6,2) /= 1) .OR. &
          (UBOUND(dbl6,3) /= 10) .OR. (LBOUND(dbl6,3) /= 1) .OR. &
          (UBOUND(dbl6,4) /= 10) .OR. (LBOUND(dbl6,4) /= 1) .OR. &
          (UBOUND(dbl6,5) /= 10) .OR. (LBOUND(dbl6,5) /= 1) .OR. &
          (UBOUND(dbl6,6) /= 10) .OR. (LBOUND(dbl6,6) /= 1)
      ASSERT(.NOT.test,'dmallocP(dbl6,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl6,100,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl6,1) /= 10) .OR. (LBOUND(dbl6,1) /= 1) .OR. &
          (UBOUND(dbl6,2) /= 10) .OR. (LBOUND(dbl6,2) /= 1) .OR. &
          (UBOUND(dbl6,3) /= 10) .OR. (LBOUND(dbl6,3) /= 1) .OR. &
          (UBOUND(dbl6,4) /= 10) .OR. (LBOUND(dbl6,4) /= 1) .OR. &
          (UBOUND(dbl6,5) /= 10) .OR. (LBOUND(dbl6,5) /= 1) .OR. &
          (UBOUND(dbl6,6) /= 10) .OR. (LBOUND(dbl6,6) /= 1)
      ASSERT(.NOT.test,'dmallocP(dbl6,100,100,100,100,100,100)')

      CALL demallocP(dbl6)
      test=ASSOCIATED(dbl6) .OR. Alloc_nbytes /= 0.0_SRK

      CALL demallocP(dbl6)
      test=ASSOCIATED(dbl6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(dbl6)')

      CALL dmalloc0P(dbl6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(dbl6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK) .OR. &
          (UBOUND(dbl6,1) /= 8) .OR. (LBOUND(dbl6,1) /= -1) .OR. &
          (UBOUND(dbl6,2) /= 8) .OR. (LBOUND(dbl6,2) /= -1) .OR. &
          (UBOUND(dbl6,3) /= 8) .OR. (LBOUND(dbl6,3) /= -1) .OR. &
          (UBOUND(dbl6,4) /= 8) .OR. (LBOUND(dbl6,4) /= -1) .OR. &
          (UBOUND(dbl6,5) /= 8) .OR. (LBOUND(dbl6,5) /= -1) .OR. &
          (UBOUND(dbl6,6) /= 8) .OR. (LBOUND(dbl6,6) /= -1)
      ASSERT(.NOT.test,'dmallocP(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl6,1) /= 8) .OR. (LBOUND(dbl6,1) /= -1) .OR. &
          (UBOUND(dbl6,2) /= 8) .OR. (LBOUND(dbl6,2) /= -1) .OR. &
          (UBOUND(dbl6,3) /= 8) .OR. (LBOUND(dbl6,3) /= -1) .OR. &
          (UBOUND(dbl6,4) /= 8) .OR. (LBOUND(dbl6,4) /= -1) .OR. &
          (UBOUND(dbl6,5) /= 8) .OR. (LBOUND(dbl6,5) /= -1) .OR. &
          (UBOUND(dbl6,6) /= 8) .OR. (LBOUND(dbl6,6) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(dbl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ASSOCIATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK) .OR. &
          (UBOUND(dbl7,1) /= 10) .OR. (LBOUND(dbl7,1) /= 1) .OR. &
          (UBOUND(dbl7,2) /= 10) .OR. (LBOUND(dbl7,2) /= 1) .OR. &
          (UBOUND(dbl7,3) /= 10) .OR. (LBOUND(dbl7,3) /= 1) .OR. &
          (UBOUND(dbl7,4) /= 10) .OR. (LBOUND(dbl7,4) /= 1) .OR. &
          (UBOUND(dbl7,5) /= 10) .OR. (LBOUND(dbl7,5) /= 1) .OR. &
          (UBOUND(dbl7,6) /= 10) .OR. (LBOUND(dbl7,6) /= 1) .OR. &
          (UBOUND(dbl7,7) /= 10) .OR. (LBOUND(dbl7,7) /= 1)
      ASSERT(.NOT.test,'dmallocP(dbl7,10,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocP(dbl7,100,100,100,100,100,100,100)
      test=(.NOT.ASSOCIATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl7,1) /= 10) .OR. (LBOUND(dbl7,1) /= 1) .OR. &
          (UBOUND(dbl7,2) /= 10) .OR. (LBOUND(dbl7,2) /= 1) .OR. &
          (UBOUND(dbl7,3) /= 10) .OR. (LBOUND(dbl7,3) /= 1) .OR. &
          (UBOUND(dbl7,4) /= 10) .OR. (LBOUND(dbl7,4) /= 1) .OR. &
          (UBOUND(dbl7,5) /= 10) .OR. (LBOUND(dbl7,5) /= 1) .OR. &
          (UBOUND(dbl7,6) /= 10) .OR. (LBOUND(dbl7,6) /= 1) .OR. &
          (UBOUND(dbl7,7) /= 10) .OR. (LBOUND(dbl7,7) /= 1)
      ASSERT(.NOT.test,'dmallocP(dbl7,100,100,100,100,100,100,100)')

      CALL demallocP(dbl7)
      test=ASSOCIATED(dbl7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(dbl7)')

      CALL demallocP(dbl7)
      test=ASSOCIATED(dbl7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocP(dbl7)')

      CALL dmalloc0P(dbl7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0P(dbl7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0P(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0P(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0P(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ASSOCIATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK) .OR. &
          (UBOUND(dbl7,1) /= 8) .OR. (LBOUND(dbl7,1) /= -1) .OR. &
          (UBOUND(dbl7,2) /= 8) .OR. (LBOUND(dbl7,2) /= -1) .OR. &
          (UBOUND(dbl7,3) /= 8) .OR. (LBOUND(dbl7,3) /= -1) .OR. &
          (UBOUND(dbl7,4) /= 8) .OR. (LBOUND(dbl7,4) /= -1) .OR. &
          (UBOUND(dbl7,5) /= 8) .OR. (LBOUND(dbl7,5) /= -1) .OR. &
          (UBOUND(dbl7,6) /= 8) .OR. (LBOUND(dbl7,6) /= -1) .OR. &
          (UBOUND(dbl7,7) /= 8) .OR. (LBOUND(dbl7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0P(dbl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ASSOCIATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl7,1) /= 8) .OR. (LBOUND(dbl7,1) /= -1) .OR. &
          (UBOUND(dbl7,2) /= 8) .OR. (LBOUND(dbl7,2) /= -1) .OR. &
          (UBOUND(dbl7,3) /= 8) .OR. (LBOUND(dbl7,3) /= -1) .OR. &
          (UBOUND(dbl7,4) /= 8) .OR. (LBOUND(dbl7,4) /= -1) .OR. &
          (UBOUND(dbl7,5) /= 8) .OR. (LBOUND(dbl7,5) /= -1) .OR. &
          (UBOUND(dbl7,6) /= 8) .OR. (LBOUND(dbl7,6) /= -1) .OR. &
          (UBOUND(dbl7,7) /= 8) .OR. (LBOUND(dbl7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0P(dbl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')

      CALL demallocP(dbl7)

    ENDSUBROUTINE testDOUBLEP
!
!-------------------------------------------------------------------------------
! Test allocation/deallocation for single precision
    SUBROUTINE testDOUBLEA()
      REAL(SDK),ALLOCATABLE :: dbl1(:)
      REAL(SDK),ALLOCATABLE :: dbl2(:,:)
      REAL(SDK),ALLOCATABLE :: dbl3(:,:,:)
      REAL(SDK),ALLOCATABLE :: dbl4(:,:,:,:)
      REAL(SDK),ALLOCATABLE :: dbl5(:,:,:,:,:)
      REAL(SDK),ALLOCATABLE :: dbl6(:,:,:,:,:,:)
      REAL(SDK),ALLOCATABLE :: dbl7(:,:,:,:,:,:,:)
      REAL(SRK) :: nbytes0
!
! rank 1 variable
      CALL dmallocA(dbl1,-10)
      CALL dmallocA(dbl1,10)
      test=(.NOT.ALLOCATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. &
          (UBOUND(dbl1,1) /= 10) .OR. (LBOUND(dbl1,1) /= 1)
      ASSERT(.NOT.test,'dmallocA(dbl1,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl1,100)
      test=(.NOT.ALLOCATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl1,1) /= 10) .OR. (LBOUND(dbl1,1) /= 1)
      ASSERT(.NOT.test,'dmallocA(dbl1,100)')

      CALL demallocA(dbl1)
      test=ALLOCATED(dbl1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl1)')

      CALL demallocA(dbl1)
      test=ALLOCATED(dbl1) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl1)')

      CALL dmalloc0A(dbl1,8,-1)
      CALL dmalloc0A(dbl1,-1,8)
      test=(.NOT.ALLOCATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. &
          (UBOUND(dbl1,1) /= 8) .OR. (LBOUND(dbl1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(dbl1,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl1,-1,1)
      test=(.NOT.ALLOCATED(dbl1)) .OR. ANY(dbl1 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl1,1) /= 8) .OR. (LBOUND(dbl1,1) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(dbl1,-1,1)')

      CALL demallocA(dbl1)
!
! rank 2 variable
      CALL dmallocA(dbl2,-10,10)
      CALL dmallocA(dbl2,10,-10)
      CALL dmallocA(dbl2,10,10)
      test=(.NOT.ALLOCATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. &
          (UBOUND(dbl2,1) /= 10) .OR. (LBOUND(dbl2,1) /= 1) .OR. &
          (UBOUND(dbl2,2) /= 10) .OR. (LBOUND(dbl2,2) /= 1)
      ASSERT(.NOT.test,'dmallocA(dbl2,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl2,100,100)
      test=(.NOT.ALLOCATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl2,1) /= 10) .OR. (LBOUND(dbl2,1) /= 1) .OR. &
          (UBOUND(dbl2,2) /= 10) .OR. (LBOUND(dbl2,2) /= 1)
      ASSERT(.NOT.test,'dmallocA(dbl2,100,100)')

      CALL demallocA(dbl2)
      test=ALLOCATED(dbl2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl2)')

      CALL demallocA(dbl2)
      test=ALLOCATED(dbl2) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl2)')

      CALL dmalloc0A(dbl2,8,-1,-1,8)
      CALL dmalloc0A(dbl2,-1,8,8,-1)
      CALL dmalloc0A(dbl2,-1,8,-1,8)
      test=(.NOT.ALLOCATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. &
          (UBOUND(dbl2,1) /= 8) .OR. (LBOUND(dbl2,1) /= -1) .OR. &
          (UBOUND(dbl2,2) /= 8) .OR. (LBOUND(dbl2,2) /= -1)
      ASSERT(.NOT.test,'CALL CALL dmalloc0A(dbl2,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl2,-1,1,-1,1)
      test=(.NOT.ALLOCATED(dbl2)) .OR. ANY(dbl2 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl2,1) /= 8) .OR. (LBOUND(dbl2,1) /= -1) .OR. &
          (UBOUND(dbl2,2) /= 8) .OR. (LBOUND(dbl2,2) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(dbl2,-1,1,-1,1)')

      CALL demallocA(dbl2)
!
! rank 3 variable
      CALL dmallocA(dbl3,-10,10,10)
      CALL dmallocA(dbl3,10,-10,10)
      CALL dmallocA(dbl3,10,10,-10)
      CALL dmallocA(dbl3,10,10,10)
      test=(.NOT.ALLOCATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. &
          (UBOUND(dbl3,1) /= 10) .OR. (LBOUND(dbl3,1) /= 1) .OR. &
          (UBOUND(dbl3,2) /= 10) .OR. (LBOUND(dbl3,2) /= 1) .OR. &
          (UBOUND(dbl3,3) /= 10) .OR. (LBOUND(dbl3,3) /= 1)
      ASSERT(.NOT.test,'dmallocA(dbl3,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl3,100,100,100)
      test=(.NOT.ALLOCATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl3,1) /= 10) .OR. (LBOUND(dbl3,1) /= 1) .OR. &
          (UBOUND(dbl3,2) /= 10) .OR. (LBOUND(dbl3,2) /= 1) .OR. &
          (UBOUND(dbl3,3) /= 10) .OR. (LBOUND(dbl3,3) /= 1)
      ASSERT(.NOT.test,'dmallocA(dbl3,100,100,100)')

      CALL demallocA(dbl3)
      test=ALLOCATED(dbl3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl3)')

      CALL demallocA(dbl3)
      test=ALLOCATED(dbl3) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl3)')

      CALL dmalloc0A(dbl3,8,-1,-1,8,-1,8)
      CALL dmalloc0A(dbl3,-1,8,8,-1,-1,8)
      CALL dmalloc0A(dbl3,-1,8,-1,8,8,-1)
      CALL dmalloc0A(dbl3,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. &
          (UBOUND(dbl3,1) /= 8) .OR. (LBOUND(dbl3,1) /= -1) .OR. &
          (UBOUND(dbl3,2) /= 8) .OR. (LBOUND(dbl3,2) /= -1) .OR. &
          (UBOUND(dbl3,3) /= 8) .OR. (LBOUND(dbl3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(dbl3,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl3,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(dbl3)) .OR. ANY(dbl3 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl3,1) /= 8) .OR. (LBOUND(dbl3,1) /= -1) .OR. &
          (UBOUND(dbl3,2) /= 8) .OR. (LBOUND(dbl3,2) /= -1) .OR. &
          (UBOUND(dbl3,3) /= 8) .OR. (LBOUND(dbl3,3) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(dbl3,-1,1,-1,1,-1,1)')

      CALL demallocA(dbl3)
!
! rank 4 variable
      CALL dmallocA(dbl4,-10,10,10,10)
      CALL dmallocA(dbl4,10,-10,10,10)
      CALL dmallocA(dbl4,10,10,-10,10)
      CALL dmallocA(dbl4,10,10,10,-10)
      CALL dmallocA(dbl4,10,10,10,10)
      test=(.NOT.ALLOCATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. &
          (UBOUND(dbl4,1) /= 10) .OR. (LBOUND(dbl4,1) /= 1) .OR. &
          (UBOUND(dbl4,2) /= 10) .OR. (LBOUND(dbl4,2) /= 1) .OR. &
          (UBOUND(dbl4,3) /= 10) .OR. (LBOUND(dbl4,3) /= 1) .OR. &
          (UBOUND(dbl4,4) /= 10) .OR. (LBOUND(dbl4,4) /= 1)
      ASSERT(.NOT.test,'dmallocA(dbl4,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl4,100,100,100,100)
      test=(.NOT.ALLOCATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl4,1) /= 10) .OR. (LBOUND(dbl4,1) /= 1) .OR. &
          (UBOUND(dbl4,2) /= 10) .OR. (LBOUND(dbl4,2) /= 1) .OR. &
          (UBOUND(dbl4,3) /= 10) .OR. (LBOUND(dbl4,3) /= 1) .OR. &
          (UBOUND(dbl4,4) /= 10) .OR. (LBOUND(dbl4,4) /= 1)
      ASSERT(.NOT.test,'Rednundant CALL dmallocA(dbl4,100,100,100,100)')

      CALL demallocA(dbl4)
      test=ALLOCATED(dbl4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl4)')

      CALL demallocA(dbl4)
      test=ALLOCATED(dbl4) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl4)')

      CALL dmalloc0A(dbl4,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl4,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(dbl4,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(dbl4,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(dbl4,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. &
          (UBOUND(dbl4,1) /= 8) .OR. (LBOUND(dbl4,1) /= -1) .OR. &
          (UBOUND(dbl4,2) /= 8) .OR. (LBOUND(dbl4,2) /= -1) .OR. &
          (UBOUND(dbl4,3) /= 8) .OR. (LBOUND(dbl4,3) /= -1) .OR. &
          (UBOUND(dbl4,4) /= 8) .OR. (LBOUND(dbl4,4) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(dbl4,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl4,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(dbl4)) .OR. ANY(dbl4 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl4,1) /= 8) .OR. (LBOUND(dbl4,1) /= -1) .OR. &
          (UBOUND(dbl4,2) /= 8) .OR. (LBOUND(dbl4,2) /= -1) .OR. &
          (UBOUND(dbl4,3) /= 8) .OR. (LBOUND(dbl4,3) /= -1) .OR. &
          (UBOUND(dbl4,4) /= 8) .OR. (LBOUND(dbl4,4) /= -1)
      ASSERT(.NOT.test,'Rednundant CALL dmalloc0A(dbl4,-1,1,-1,1,-1,1,-1,1)')

      CALL demallocA(dbl4)
!
! rank 5 variable
      CALL dmallocA(dbl5,-10,10,10,10,10)
      CALL dmallocA(dbl5,10,-10,10,10,10)
      CALL dmallocA(dbl5,10,10,-10,10,10)
      CALL dmallocA(dbl5,10,10,10,-10,10)
      CALL dmallocA(dbl5,10,10,10,10,-10)
      CALL dmallocA(dbl5,10,10,10,10,10)
      test=(.NOT.ALLOCATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK) .OR. &
          (UBOUND(dbl5,1) /= 10) .OR. (LBOUND(dbl5,1) /= 1) .OR. &
          (UBOUND(dbl5,2) /= 10) .OR. (LBOUND(dbl5,2) /= 1) .OR. &
          (UBOUND(dbl5,3) /= 10) .OR. (LBOUND(dbl5,3) /= 1) .OR. &
          (UBOUND(dbl5,4) /= 10) .OR. (LBOUND(dbl5,4) /= 1) .OR. &
          (UBOUND(dbl5,5) /= 10) .OR. (LBOUND(dbl5,5) /= 1)
      ASSERT(.NOT.test,'dmallocA(dbl5,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl5,100,100,100,100,100)
      test=(.NOT.ALLOCATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl5,1) /= 10) .OR. (LBOUND(dbl5,1) /= 1) .OR. &
          (UBOUND(dbl5,2) /= 10) .OR. (LBOUND(dbl5,2) /= 1) .OR. &
          (UBOUND(dbl5,3) /= 10) .OR. (LBOUND(dbl5,3) /= 1) .OR. &
          (UBOUND(dbl5,4) /= 10) .OR. (LBOUND(dbl5,4) /= 1) .OR. &
          (UBOUND(dbl5,5) /= 10) .OR. (LBOUND(dbl5,5) /= 1)
      ASSERT(.NOT.test,'dmallocA(dbl5,100,100,100,100,100)')

      CALL demallocA(dbl5)
      test=ALLOCATED(dbl5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl5)')

      CALL demallocA(dbl5)
      test=ALLOCATED(dbl5) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl5)')

      CALL dmalloc0A(dbl5,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl5,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl5,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(dbl5,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(dbl5,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(dbl5,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK) .OR. &
          (UBOUND(dbl5,1) /= 8) .OR. (LBOUND(dbl5,1) /= -1) .OR. &
          (UBOUND(dbl5,2) /= 8) .OR. (LBOUND(dbl5,2) /= -1) .OR. &
          (UBOUND(dbl5,3) /= 8) .OR. (LBOUND(dbl5,3) /= -1) .OR. &
          (UBOUND(dbl5,4) /= 8) .OR. (LBOUND(dbl5,4) /= -1) .OR. &
          (UBOUND(dbl5,5) /= 8) .OR. (LBOUND(dbl5,5) /= -1)
      ASSERT(.NOT.test,'dmallocA(dbl5,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl5,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(dbl5)) .OR. ANY(dbl5 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl5,1) /= 8) .OR. (LBOUND(dbl5,1) /= -1) .OR. &
          (UBOUND(dbl5,2) /= 8) .OR. (LBOUND(dbl5,2) /= -1) .OR. &
          (UBOUND(dbl5,3) /= 8) .OR. (LBOUND(dbl5,3) /= -1) .OR. &
          (UBOUND(dbl5,4) /= 8) .OR. (LBOUND(dbl5,4) /= -1) .OR. &
          (UBOUND(dbl5,5) /= 8) .OR. (LBOUND(dbl5,5) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(dbl5,-1,1,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ALLOCATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK) .OR. &
          (UBOUND(dbl6,1) /= 10) .OR. (LBOUND(dbl6,1) /= 1) .OR. &
          (UBOUND(dbl6,2) /= 10) .OR. (LBOUND(dbl6,2) /= 1) .OR. &
          (UBOUND(dbl6,3) /= 10) .OR. (LBOUND(dbl6,3) /= 1) .OR. &
          (UBOUND(dbl6,4) /= 10) .OR. (LBOUND(dbl6,4) /= 1) .OR. &
          (UBOUND(dbl6,5) /= 10) .OR. (LBOUND(dbl6,5) /= 1) .OR. &
          (UBOUND(dbl6,6) /= 10) .OR. (LBOUND(dbl6,6) /= 1)
      ASSERT(.NOT.test,'dmallocA(dbl6,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl6,100,100,100,100,100,100)
      test=(.NOT.ALLOCATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl6,1) /= 10) .OR. (LBOUND(dbl6,1) /= 1) .OR. &
          (UBOUND(dbl6,2) /= 10) .OR. (LBOUND(dbl6,2) /= 1) .OR. &
          (UBOUND(dbl6,3) /= 10) .OR. (LBOUND(dbl6,3) /= 1) .OR. &
          (UBOUND(dbl6,4) /= 10) .OR. (LBOUND(dbl6,4) /= 1) .OR. &
          (UBOUND(dbl6,5) /= 10) .OR. (LBOUND(dbl6,5) /= 1) .OR. &
          (UBOUND(dbl6,6) /= 10) .OR. (LBOUND(dbl6,6) /= 1)
      ASSERT(.NOT.test,'dmallocA(dbl6,100,100,100,100,100,100)')

      CALL demallocA(dbl6)
      test=ALLOCATED(dbl6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl6)')

      CALL demallocA(dbl6)
      test=ALLOCATED(dbl6) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl6)')

      CALL dmalloc0A(dbl6,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl6,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl6,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl6,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(dbl6,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK) .OR. &
          (UBOUND(dbl6,1) /= 8) .OR. (LBOUND(dbl6,1) /= -1) .OR. &
          (UBOUND(dbl6,2) /= 8) .OR. (LBOUND(dbl6,2) /= -1) .OR. &
          (UBOUND(dbl6,3) /= 8) .OR. (LBOUND(dbl6,3) /= -1) .OR. &
          (UBOUND(dbl6,4) /= 8) .OR. (LBOUND(dbl6,4) /= -1) .OR. &
          (UBOUND(dbl6,5) /= 8) .OR. (LBOUND(dbl6,5) /= -1) .OR. &
          (UBOUND(dbl6,6) /= 8) .OR. (LBOUND(dbl6,6) /= -1)
      ASSERT(.NOT.test,'dmallocA(dbl6,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(dbl6)) .OR. ANY(dbl6 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl6,1) /= 8) .OR. (LBOUND(dbl6,1) /= -1) .OR. &
          (UBOUND(dbl6,2) /= 8) .OR. (LBOUND(dbl6,2) /= -1) .OR. &
          (UBOUND(dbl6,3) /= 8) .OR. (LBOUND(dbl6,3) /= -1) .OR. &
          (UBOUND(dbl6,4) /= 8) .OR. (LBOUND(dbl6,4) /= -1) .OR. &
          (UBOUND(dbl6,5) /= 8) .OR. (LBOUND(dbl6,5) /= -1) .OR. &
          (UBOUND(dbl6,6) /= 8) .OR. (LBOUND(dbl6,6) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(dbl6,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')

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
      test=(.NOT.ALLOCATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK) .OR. &
          (UBOUND(dbl7,1) /= 10) .OR. (LBOUND(dbl7,1) /= 1) .OR. &
          (UBOUND(dbl7,2) /= 10) .OR. (LBOUND(dbl7,2) /= 1) .OR. &
          (UBOUND(dbl7,3) /= 10) .OR. (LBOUND(dbl7,3) /= 1) .OR. &
          (UBOUND(dbl7,4) /= 10) .OR. (LBOUND(dbl7,4) /= 1) .OR. &
          (UBOUND(dbl7,5) /= 10) .OR. (LBOUND(dbl7,5) /= 1) .OR. &
          (UBOUND(dbl7,6) /= 10) .OR. (LBOUND(dbl7,6) /= 1) .OR. &
          (UBOUND(dbl7,7) /= 10) .OR. (LBOUND(dbl7,7) /= 1)
      ASSERT(.NOT.test,'dmallocA(dbl7,10,10,10,10,10,10,10)')

      nbytes0=Alloc_nbytes
      CALL dmallocA(dbl7,100,100,100,100,100,100,100)
      test=(.NOT.ALLOCATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK) .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl7,1) /= 10) .OR. (LBOUND(dbl7,1) /= 1) .OR. &
          (UBOUND(dbl7,2) /= 10) .OR. (LBOUND(dbl7,2) /= 1) .OR. &
          (UBOUND(dbl7,3) /= 10) .OR. (LBOUND(dbl7,3) /= 1) .OR. &
          (UBOUND(dbl7,4) /= 10) .OR. (LBOUND(dbl7,4) /= 1) .OR. &
          (UBOUND(dbl7,5) /= 10) .OR. (LBOUND(dbl7,5) /= 1) .OR. &
          (UBOUND(dbl7,6) /= 10) .OR. (LBOUND(dbl7,6) /= 1) .OR. &
          (UBOUND(dbl7,7) /= 10) .OR. (LBOUND(dbl7,7) /= 1)
      ASSERT(.NOT.test,'dmallocA(dbl7,100,100,100,100,100,100,100)')

      CALL demallocA(dbl7)
      test=ALLOCATED(dbl7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl7)')

      CALL demallocA(dbl7)
      test=ALLOCATED(dbl7) .OR. Alloc_nbytes /= 0.0_SRK
      ASSERT(.NOT.test,'demallocA(dbl7)')

      CALL dmalloc0A(dbl7,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl7,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl7,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl7,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8,-1,8)
      CALL dmalloc0A(dbl7,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8,-1,8)
      CALL dmalloc0A(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1,-1,8)
      CALL dmalloc0A(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,8,-1)
      CALL dmalloc0A(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)
      test=(.NOT.ALLOCATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK) .OR. &
          (UBOUND(dbl7,1) /= 8) .OR. (LBOUND(dbl7,1) /= -1) .OR. &
          (UBOUND(dbl7,2) /= 8) .OR. (LBOUND(dbl7,2) /= -1) .OR. &
          (UBOUND(dbl7,3) /= 8) .OR. (LBOUND(dbl7,3) /= -1) .OR. &
          (UBOUND(dbl7,4) /= 8) .OR. (LBOUND(dbl7,4) /= -1) .OR. &
          (UBOUND(dbl7,5) /= 8) .OR. (LBOUND(dbl7,5) /= -1) .OR. &
          (UBOUND(dbl7,6) /= 8) .OR. (LBOUND(dbl7,6) /= -1) .OR. &
          (UBOUND(dbl7,7) /= 8) .OR. (LBOUND(dbl7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(dbl7,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8,-1,8)')

      nbytes0=Alloc_nbytes
      CALL dmalloc0A(dbl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)
      test=(.NOT.ALLOCATED(dbl7)) .OR. ANY(dbl7 /= 0.0_SDK)  .OR. Alloc_nbytes /= nbytes0 &
          .OR. (UBOUND(dbl7,1) /= 8) .OR. (LBOUND(dbl7,1) /= -1) .OR. &
          (UBOUND(dbl7,2) /= 8) .OR. (LBOUND(dbl7,2) /= -1) .OR. &
          (UBOUND(dbl7,3) /= 8) .OR. (LBOUND(dbl7,3) /= -1) .OR. &
          (UBOUND(dbl7,4) /= 8) .OR. (LBOUND(dbl7,4) /= -1) .OR. &
          (UBOUND(dbl7,5) /= 8) .OR. (LBOUND(dbl7,5) /= -1) .OR. &
          (UBOUND(dbl7,6) /= 8) .OR. (LBOUND(dbl7,6) /= -1) .OR. &
          (UBOUND(dbl7,7) /= 8) .OR. (LBOUND(dbl7,7) /= -1)
      ASSERT(.NOT.test,'dmalloc0A(dbl7,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)')

      CALL demallocA(dbl7)

    ENDSUBROUTINE testDOUBLEA
!
ENDPROGRAM testAllocs
