!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testDBC
#include "DBC.h"
  USE DBC
  USE IntrType
  IMPLICIT NONE
  CHARACTER(LEN=*),PARAMETER :: modName="testDBC"
  INTEGER(SIK) :: iopt
  CHARACTER(LEN=5) :: arg
#ifdef HAVE_MPI
  INCLUDE "mpif.h"
  INTEGER(SIK) :: mpierr
  CALL MPI_Init(mpierr)
#endif

  CALL GET_COMMAND_ARGUMENT(1,arg)
  READ(arg,*) iopt

  SELECTCASE(iopt)
    CASE(1)
      CALL require_pass()
    CASE(2)
      CALL ensure_pass()
    CASE(3)
      CALL require_fail()
    CASE(4)
      CALL ensure_fail()
    CASE DEFAULT
      STOP -1
  ENDSELECT

#ifdef HAVE_MPI
      CALL MPI_Finalize(mpierr)
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
ENDPROGRAM testDBC
