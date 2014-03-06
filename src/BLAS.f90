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
