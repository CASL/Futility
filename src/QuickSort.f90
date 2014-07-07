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
!> @brief Module defines the quick sort method for various input arrays
!>
!> @author Benjamin S Collins
!>   @date 07/06/2014
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE QuickSort
  USE IntrType
  USE ExceptionHandler

  IMPLICIT NONE
  PRIVATE

  !List of public members
  PUBLIC :: qsort
  PUBLIC :: partition_array

  !> Module name
  CHARACTER(LEN=*),PARAMETER :: modName='QuickSort'

  !> Exception handler for the module
  TYPE(ExceptionHandlerType),SAVE :: eQSort

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Exponential function
!> @param myET Exponential table type object
!> @param x The variable
!> @param ans The return value
!>
    RECURSIVE SUBROUTINE qsort(A)
      INTEGER(SIK),INTENT(INOUT) :: A(:)

      INTEGER(SIK) :: n,l,p

      n=SIZE(A)

      IF (n>1) THEN
        p=FLOOR(RAND()*REAL(n,SRK),SIK)+1
        CALL partition_array(A,p,l)
        CALL qsort(A(1:l-1))
        CALL qsort(A(l+1:n))
      ENDIF
    ENDSUBROUTINE

    SUBROUTINE partition_array(A,p,i)
      INTEGER(SIK),INTENT(INOUT) :: A(:)
      INTEGER(SIK),INTENT(IN) :: p
      INTEGER(SIK),INTENT(OUT) :: i

      INTEGER(SIK) :: j,n,tmp1,tmp2,pval

      pval=A(p)
      n=SIZE(A)
      IF (p>1) THEN
        tmp1=A(1)
        tmp2=A(p)
        A(1)=tmp2
        A(p)=tmp1
      ENDIF

      i=2
      DO j=2,n
        IF (A(j)<pval) THEN
          tmp1=A(i)
          tmp2=A(j)
          A(i)=tmp2
          A(j)=tmp1
          i=i+1
        ENDIF
      ENDDO
      i=i-1
      tmp1=A(1)
      tmp2=A(i)
      A(1)=tmp2
      A(i)=tmp1
    ENDSUBROUTINE

ENDMODULE QuickSort
