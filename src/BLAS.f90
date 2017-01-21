!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief The global module for collecting all public members of
!> other BLAS modules. This is the module that should be used elsewhere
!> in the code.
!>
!> @par Module Dependencies
!>  - @ref BLAS1 "BLAS1": @copybrief BLAS1
!>  - @ref BLAS2 "BLAS2": @copybrief BLAS2
!>  - @ref BLAS3 "BLAS3": @copybrief BLAS3
!>
!> @author Brendan Kochunas
!>    @date 03/16/2012
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE BLAS

  USE BLAS1
  USE BLAS2
  USE BLAS3
  IMPLICIT NONE
!
ENDMODULE BLAS
