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
PUBLIC :: DBC_STOP_ON_FAIL
PUBLIC :: DBC_FAIL_VERBOSE
PUBLIC :: DBC_COUNTER

! These variables are created to be used in unit testing DBC
!This logical disables the "stop" from being called if DBC_FAIL is called
!  The user should be very careful when setting this to FALSE to set back to TRUE
!  as soon as possible to ensure DBC is working properly where intended
!  This logical should ONLY be changed in unit test code and NEVER in a routine.
LOGICAL :: DBC_STOP_ON_FAIL=.TRUE.
!This controls if DBC backtraces are printed
LOGICAL :: DBC_FAIL_VERBOSE=.TRUE.
!This integer counts the total number of DBC_FAIL statements called
INTEGER :: DBC_COUNTER=0
!
! Add interface to C abort function to exit intel-compiled code abnormally and generate
! traceback information.
#ifdef __INTEL_COMPILER
   interface
      subroutine abort() bind(C, name="abort")
      end subroutine
   end interface
#endif
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
  LOGICAL :: mpiInit
  CALL MPI_Initialized(mpiInit,mpierr)
  IF(mpiInit) THEN
    CALL MPI_Comm_rank(MPI_COMM_WORLD,rank,mpierr)
    CALL MPI_Comm_size(MPI_COMM_WORLD,nproc,mpierr)
 ELSE
    rank=0
    nproc=1
 ENDIF
#else
  rank=0
  nproc=1
#endif
  DBC_COUNTER=DBC_COUNTER+1
  IF(DBC_FAIL_VERBOSE) THEN
    WRITE(ERROR_UNIT,'(a,i5,a,i5,a,i5)') "DBC Failure: "//test_char//" in "// &
        mod_name//" on line",line,":  process",rank+1," of",nproc
  ENDIF

#ifdef __INTEL_COMPILER
  IF(DBC_STOP_ON_FAIL) CALL abort
#else
  IF(DBC_FAIL_VERBOSE) CALL backtrace()
  IF(DBC_STOP_ON_FAIL) STOP 2
#endif
ENDSUBROUTINE DBC_FAIL
!
ENDMODULE Futility_DBC
