!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testDBC
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
IMPLICIT NONE
INTEGER(SIK) :: iopt
CHARACTER(LEN=5) :: arg
#ifdef HAVE_MPI
INCLUDE "mpif.h"
INTEGER(SIK) :: mpierr
#endif

CALL GET_COMMAND_ARGUMENT(1,arg)
READ(arg,*) iopt

#ifdef HAVE_MPI
! Do not init MPI for the no_stop_on_fail test.  This test calls DBC_Fail
! but will not stop because of the failed REQUIRE.  It did still fail when trying
! to call MPI routines without MPI_Init being called before a fix was put in.
IF(iopt/=5) CALL MPI_Init(mpierr)
#endif

SELECTCASE(iopt)
  CASE(1)
    CALL require_pass()
  CASE(2)
    CALL ensure_pass()
  CASE(3)
    CALL require_fail()
  CASE(4)
    CALL ensure_fail()
  CASE(5)
    CALL no_stop_on_fail()
  CASE DEFAULT
    STOP 1
ENDSELECT

WRITE(*,*) "TEST PASSED"

#ifdef HAVE_MPI
IF(iopt/=5) CALL MPI_Finalize(mpierr)
#endif
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Tests the REQUIRE macro and associated function calls
!>
SUBROUTINE require_pass()
  WRITE(*,*) "Testing REQUIRE Passing"
  REQUIRE(5==5)

ENDSUBROUTINE require_pass
!
!-------------------------------------------------------------------------------
!> @brief Tests the ENSURE macro and associated function calls
!>
SUBROUTINE ensure_pass()
  WRITE(*,*) "Testing ENSURE Passing"
  ENSURE(5==5)

ENDSUBROUTINE ensure_pass
!
!-------------------------------------------------------------------------------
!> @brief Tests the REQUIRE macro and associated function calls
!>
SUBROUTINE require_fail()
  WRITE(*,*) "Testing REQUIRE Failing"
  REQUIRE(8==5)

ENDSUBROUTINE require_fail
!
!-------------------------------------------------------------------------------
!> @brief Tests the ENSURE macro and associated function calls
!>
SUBROUTINE ensure_fail()
  WRITE(*,*) "Testing ENSURE Failing"
  ENSURE(5==8)

ENDSUBROUTINE ensure_fail
!
!-------------------------------------------------------------------------------
!> @brief Tests the ENSURE macro and associated function calls
!>
SUBROUTINE no_stop_on_fail()
  INTEGER(SIK) :: ctr
  WRITE(*,*) "Testing STOP ON FAIL"
  DBC_STOP_ON_FAIL=.FALSE.

  ctr=DBC_COUNTER
  REQUIRE(5==5)
  REQUIRE(5==8)
  ENSURE(5==5)
  ENSURE(5==8)

  IF(DBC_COUNTER/=ctr+2) STOP 2
  DBC_STOP_ON_FAIL=.TRUE.
ENDSUBROUTINE no_stop_on_fail
ENDPROGRAM testDBC
