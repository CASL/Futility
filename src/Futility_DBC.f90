!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief description
!>  This module contains the DBC_Fail routine which in conjunction with DBC.h
!>  which defines the C-preprocessor macros REQUIRE and ENSURE, work to
!>  implement a basic Design by Contract testing.
!>  REQUIRE should be used to document contracts on the inputs to a routine
!>  ENSURE should be used to document contracts on the outputs of a routine
!>
!> @par EXAMPLES
!> @code
!> SUBROUTINE foo(x,y)
!>   TYPE(bar),INTENT(INOUT) :: x
!>   REAL(SRK),INTENT(OUT) :: y
!>
!>   REQUIRE(x%isInit)
!>   REQUIRE(x%hasData)
!>
!>   y=x%calcFoo()
!>
!>   ENSURE(y>0.0_SRK)
!> ENDSUBROUTINE foo
!> @endcode
!>
!>
!> @author Benjamin Collins
!>    @date 09/19/2017
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Futility_DBC
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE

#ifdef HAVE_MPI
  INCLUDE "mpif.h"
#endif

  PRIVATE
!
! List of Public items
  PUBLIC :: DBC_FAIL
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Design by Contract Failure
!>
!> Self-explanatory.
!>
  SUBROUTINE DBC_FAIL(test_char,mod_name,line)
    CHARACTER(LEN=*),INTENT(IN) :: test_char
    CHARACTER(LEN=*),INTENT(IN) :: mod_name
    INTEGER,INTENT(IN) :: line
    !> Variables for MPI tests
    INTEGER :: rank,nproc
#ifdef HAVE_MPI
    INTEGER :: mpierr
    CALL MPI_Comm_rank(MPI_COMM_WORLD,rank,mpierr)
    CALL MPI_Comm_size(MPI_COMM_WORLD,nproc,mpierr)
#else
    rank=0
    nproc=1
#endif

    WRITE(ERROR_UNIT,'(a,i5,a,i5,a,i5)') "DBC Failure: "//test_char//" in "// &
      mod_name//" on line",line,":  process",rank+1," of",nproc

#ifndef __INTEL_COMPILER
    CALL backtrace()
#endif
    STOP 2
  ENDSUBROUTINE DBC_FAIL
!
ENDMODULE Futility_DBC
