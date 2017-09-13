!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief description
!>
!>
!> @par
!> @code
!> @endcode
!>
!>
!> @author Benjamin Collins
!>    @date 11/17/2012
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE DBC
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
  INTEGER :: rank=1,nproc=1
#ifdef HAVE_MPI
  INTEGER :: mpierr
  CALL MPI_Comm_rank(MPI_COMM_WORLD,rank,mpierr)
  CALL MPI_Comm_size(MPI_COMM_WORLD,nproc,mpierr)
#endif

  WRITE(ERROR_UNIT,'(a,i5,a,i5,a,i5)') "DBC Failure: " // test_char // " in " // mod_name // &
        " on line",line,":  process",rank+1," of",nproc
#ifndef __INTEL_COMPILER
  CALL backtrace()
#endif
  STOP 2
ENDSUBROUTINE DBC_FAIL
!
ENDMODULE DBC
