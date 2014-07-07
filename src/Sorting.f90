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
!> @par Revisions:
!>   (07/06/2014) - Benjamin Collins
!>   - Added additional interface for qsort (QuickSort) to give better
!>           performance when needed
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Sorting

  USE IntrType
  IMPLICIT NONE

  PUBLIC :: Sort
  PUBLIC :: qsort

  !> @brief Generic interface to sort arrays.
  !>
  INTERFACE Sort
    !> @copybrief Sorting::Sort_1DReal
    !> @copydetails Sorting::Sort_1DReal
    MODULE PROCEDURE Sort_1DReal
    !> @copybrief Sorting::Sort_1DInt
    !> @copydetails Sorting::Sort_1DInt
    MODULE PROCEDURE Sort_1DInt
    !> @copybrief Sorting::Sort_2DInt
    !> @copydetails Sorting::Sort_2DInt
    MODULE PROCEDURE Sort_2DInt
  ENDINTERFACE Sort

  INTERFACE qsort
    !> @copybrief QuickSort::qsort_1DReal
    !> @copydetails QuickSort::qsort_1DReal
    MODULE PROCEDURE qsort_1DReal
    !> @copybrief QuickSort::qsort_1DInt
    !> @copydetails QuickSort::qsort_1DInt
    MODULE PROCEDURE qsort_1DInt
  ENDINTERFACE qsort
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief A simple sorting algorithm for a 1-D real vector sort.  The arguments
!>        data will be sorted in ascending/increasing order and returned.
!> @param r A vector of unsorted reals
!>
    PURE SUBROUTINE Sort_1DReal(r)
      REAL(SRK),INTENT(INOUT) :: r(:)
      !LOGICAL(SBK),INTENT(IN),OPTIONAL :: reverse
      LOGICAL(SBK) :: sorted
      INTEGER(SIK) :: i,ncomp
      REAL(SRK) :: tmp

      sorted=.FALSE.
      ncomp=0
      DO WHILE(.NOT.sorted)
        sorted=.TRUE.
        ncomp=ncomp+1
        DO i=1,SIZE(r)-ncomp
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
    PURE SUBROUTINE Sort_1DInt(r)
      INTEGER(SIK),INTENT(INOUT) :: r(:)
      !LOGICAL(SBK),INTENT(IN),OPTIONAL :: reverse
      LOGICAL(SBK) :: sorted
      INTEGER(SIK) :: i,ncomp
      INTEGER(SIK) :: tmp

      sorted=.FALSE.
      ncomp=0
      DO WHILE(.NOT.sorted)
        sorted=.TRUE.
        ncomp=ncomp+1
        DO i=1,SIZE(r)-ncomp
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
!-------------------------------------------------------------------------------
!> @brief A simple sorting algorithm for a 2-D integer array sort.  The
!>        arguments data will be sorted in ascending/increasing order and
!>        returned.
!> @param r A 2-D array of unsorted integers
!>
    PURE SUBROUTINE Sort_2DInt(r)
      INTEGER(SIK),INTENT(INOUT) :: r(:,:)
      !LOGICAL(SBK),INTENT(IN),OPTIONAL :: reverse
      LOGICAL(SBK) :: sorted
      INTEGER(SIK) :: i1,i2,j1,j2,ni,nj,n,ncomp
      INTEGER(SIK) :: tmp

      sorted=.FALSE.
      ni=SIZE(r,DIM=1)
      nj=SIZE(r,DIM=2)
      ncomp=0
      DO WHILE(.NOT.sorted)
        sorted=.TRUE.
        ncomp=ncomp+1
        DO n=1,ni*nj-ncomp
          i1=MOD(n-1,ni)+1
          j1=(n-1)/ni+1
          i2=MOD(n,ni)+1
          j2=(n)/ni+1
          IF(r(i1,j1) > r(i2,j2)) THEN
            tmp=r(i2,j2)
            r(i2,j2)=r(i1,j1)
            r(i1,j1)=tmp
            sorted=.FALSE.
          ENDIF
        ENDDO
      ENDDO

    ENDSUBROUTINE Sort_2DInt
!
!-------------------------------------------------------------------------------
!  QuickSort
!-------------------------------------------------------------------------------
!> @brief QuickSort 1D integer array
!> @param A 1D integer array, modified in place and returned sorted
!>
    RECURSIVE SUBROUTINE qsort_1DInt(A)
      INTEGER(SIK),INTENT(INOUT) :: A(:)

      INTEGER(SIK) :: n,l,p

      n=SIZE(A)

      IF (n>1) THEN
        p=FLOOR(RAND()*REAL(n,SRK),SIK)+1
        CALL partition_array_1DInt(A,p,l)
        CALL qsort_1DInt(A(1:l-1))
        CALL qsort_1DInt(A(l+1:n))
      ENDIF
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Partition method 1D integer array.  For a given partion, sorts values
!>         greater than and less than the partition
!> @param A 1D integer array, modified in place and returned partioned
!> @param p Index of array element to partition with
!> @param i Index of the partition element once A is partitioned
!>
    SUBROUTINE partition_array_1DInt(A,p,i)
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
!
!-------------------------------------------------------------------------------
!> @brief QuickSort 1D real array
!> @param A 1D real array, modified in place and returned sorted
!>
    RECURSIVE SUBROUTINE qsort_1DReal(A)
      REAL(SRK),INTENT(INOUT) :: A(:)

      INTEGER(SIK) :: n,l,p

      n=SIZE(A)

      IF (n>1) THEN
        p=FLOOR(RAND()*REAL(n,SRK),SIK)+1
        CALL partition_array_1DReal(A,p,l)
        CALL qsort_1DReal(A(1:l-1))
        CALL qsort_1DReal(A(l+1:n))
      ENDIF
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Partition method 1D real array.  For a given partion, sorts values
!>         greater than and less than the partition
!> @param A 1D real array, modified in place and returned partioned
!> @param p Index of array element to partition with
!> @param i Index of the partition element once A is partitioned
!>
    SUBROUTINE partition_array_1DReal(A,p,i)
      REAL(SRK),INTENT(INOUT) :: A(:)
      INTEGER(SIK),INTENT(IN) :: p
      INTEGER(SIK),INTENT(OUT) :: i

      INTEGER(SIK) :: j,n
      REAL(SRK) :: tmp1,tmp2,pval

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
!
ENDMODULE Sorting
