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
!> @brief A Fortran 2003 module implementing some basic sorting algorithms.
!> 
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>
!> @author Dan Jabaay
!>    @date 11/1/2013
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Sorting
  
  USE IntrType
  IMPLICIT NONE
  
  PUBLIC :: Sort
  
  !> @brief Generic interface to sort arrays.
  !>
  INTERFACE Sort
    !> @copybrief Sorting::Sort_1DReal
    !> @copydetails Sorting::Sort_1DReal
    MODULE PROCEDURE Sort_1DReal
    !> @copybrief Sorting::Sort_1DInt
    !> @copydetails Sorting::Sort_1DInt
    MODULE PROCEDURE Sort_1DInt
  ENDINTERFACE Sort

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief A simple sorting algorithm for a 1-D real vector sort.  The arguments
!>        data will be sorted in ascending/increasing order and returned.
!> @param r A vector of unsorted reals
!>
    SUBROUTINE Sort_1DReal(r) 
      REAL(SRK),INTENT(INOUT) :: r(:)
      !LOGICAL(SBK),INTENT(IN),OPTIONAL :: reverse
      LOGICAL(SBK) :: sorted
      INTEGER(SIK) :: i
      REAL(SRK) :: tmp
      
      sorted=.FALSE.
      DO WHILE(.NOT.sorted)
        sorted=.TRUE.
        DO i=1,SIZE(r)-1
          IF(r(i) > r(i+1)) THEN
            tmp=r(i+1)
            r(i+1)=r(i)
            r(i)=tmp
            sorted=.FALSE.
          ENDIF
        ENDDO
      ENDDO
      
    ENDSUBROUTINE Sort_1DReal
!
!-------------------------------------------------------------------------------
!> @brief A simple sorting algorithm for a 1-D integer vector sort.  The 
!>        arguments data will be sorted in ascending/increasing order and 
!>        returned.
!> @param r A vector of unsorted integers
!>
    SUBROUTINE Sort_1DInt(r) 
      INTEGER(SIK),INTENT(INOUT) :: r(:)
      !LOGICAL(SBK),INTENT(IN),OPTIONAL :: reverse
      LOGICAL(SBK) :: sorted
      INTEGER(SIK) :: i
      INTEGER(SIK) :: tmp
      
      sorted=.FALSE.
      DO WHILE(.NOT.sorted)
        sorted=.TRUE.
        DO i=1,SIZE(r)-1
          IF(r(i) > r(i+1)) THEN
            tmp=r(i+1)
            r(i+1)=r(i)
            r(i)=tmp
            sorted=.FALSE.
          ENDIF
        ENDDO
      ENDDO
      
    ENDSUBROUTINE Sort_1DInt
!
ENDMODULE Sorting
