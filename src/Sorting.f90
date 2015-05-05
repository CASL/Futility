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

  PUBLIC :: sort
  PUBLIC :: insert_sort
  PUBLIC :: bubble_sort

  !> @brief Generic interface to sort arrays.
  !>
  INTERFACE sort
    !> @copybrief Sorting::qsort_1DReal
    !> @copydetails Sorting::qsort_1DReal
    MODULE PROCEDURE qsort_1DReal
    !> @copybrief Sorting::qsort_1DInt
    !> @copydetails Sorting::qsort_1DInt
    MODULE PROCEDURE qsort_1DInt
    !> @copybrief Sorting::bubble_sort_2DInt
    !> @copydetails Sorting::bubble_sort_2DInt
    MODULE PROCEDURE bubble_sort_2DInt
  ENDINTERFACE sort


  !> @brief Generic interface to sort arrays with insertion sort.
  !>
  INTERFACE insert_sort
    !> @copybrief Sorting::insert_sort_int
    !> @copydetails Sorting::insert_sort_int
    MODULE PROCEDURE insert_sort_int
    !> @copybrief Sorting::insert_sort_real
    !> @copydetails Sorting::insert_sort_real
    MODULE PROCEDURE insert_sort_real
  ENDINTERFACE insert_sort


  !> @brief Generic interface to sort arrays with bubble sort.
  !>  
  INTERFACE bubble_sort
    !> @copybrief Sorting::bubble_sort_1DReal
    !> @copydetails Sorting::bubble_sort_1DReal
    MODULE PROCEDURE bubble_sort_1DReal
    !> @copybrief Sorting::bubble_sort_1DInt
    !> @copydetails Sorting::bubble_sort_1DInt
    MODULE PROCEDURE bubble_sort_1DInt
    !> @copybrief Sorting::bubble_sort_2DInt
    !> @copydetails Sorting::bubble_sort_2DInt
    MODULE PROCEDURE bubble_sort_2DInt
  ENDINTERFACE bubble_sort
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief A simple sorting algorithm for a 1-D real vector sort.  The arguments
!>        data will be sorted in ascending/increasing order and returned.
!> @param r A vector of unsorted reals
!>
    PURE SUBROUTINE bubble_sort_1DReal(r)
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

    ENDSUBROUTINE bubble_sort_1DReal
!
!-------------------------------------------------------------------------------
!> @brief A simple sorting algorithm for a 1-D integer vector sort.  The
!>        arguments data will be sorted in ascending/increasing order and
!>        returned.
!> @param r A vector of unsorted integers
!>
    PURE SUBROUTINE bubble_sort_1DInt(r)
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

    ENDSUBROUTINE bubble_sort_1DInt
!
!-------------------------------------------------------------------------------
!> @brief A simple sorting algorithm for a 2-D integer array sort.  The
!>        arguments data will be sorted in ascending/increasing order and
!>        returned.
!> @param r A 2-D array of unsorted integers
!>
    PURE SUBROUTINE bubble_sort_2DInt(r)
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

    ENDSUBROUTINE bubble_sort_2DInt
!-------------------------------------------------------------------------------
!  Insert Sort
!-------------------------------------------------------------------------------
!> @brief Sorts a list of integers using the insert sort algorithm
!> @param list the list to be sorted
    PURE SUBROUTINE insert_sort_int(list)
      INTEGER(SIK),INTENT(INOUT) :: list(:)
      !
      INTEGER(SIK) :: key,i,j,n

      n=SIZE(list)

      DO i=2,n
        key = list(i)
        j=i-1
        DO WHILE(list(j) > key)
          list(j+1) = list(j)
          j=j-1
          IF (j < 1) EXIT
        ENDDO
        list(j+1)=key
      ENDDO
    ENDSUBROUTINE insert_sort_int
!
!-------------------------------------------------------------------------------
!> @brief Sorts a list of integers using the insert sort algorithm
!> @param list the list to be sorted
    PURE SUBROUTINE insert_sort_real(list)
      REAL(SRK),INTENT(INOUT) :: list(:)
      !
      REAL(SRK) :: key
      INTEGER(SIK) :: i,j,n

      n=SIZE(list)

      DO i=2,n
        key = list(i)
        j=i-1
        DO WHILE(list(j) > key)
          list(j+1) = list(j)
          j=j-1
          IF (j < 1) EXIT
        ENDDO
        list(j+1)=key
      ENDDO
    ENDSUBROUTINE insert_sort_real
!
!-------------------------------------------------------------------------------
!  QuickSort
!-------------------------------------------------------------------------------
!> @brief QuickSort 1D integer array
!> @param A 1D integer array, modified in place and returned sorted
!>
    PURE RECURSIVE SUBROUTINE qsort_1DInt(A)
      INTEGER(SIK),INTENT(INOUT) :: A(:)

      INTEGER(SIK) :: n,l,p,c

      n=SIZE(A)

      IF (n>50) THEN
        !median of 3 pivot
        c=n/2
        IF (A(c) < A(1)) CALL swap_int(A,c,1)
        IF (A(n) < A(1)) CALL swap_int(A,1,n)
        IF (A(n) < A(c)) CALL swap_int(A,c,n)
        p=c
       ! p=1  ! left most pivot
       ! p=FLOOR(RAND()*REAL(n,SRK),SIK)+1  ! Randomized pivot
        CALL partition_array_1DInt(A,p,l)
        CALL qsort_1DInt(A(1:l-1))
        CALL qsort_1DInt(A(l+1:n))
      ELSE
        CALL insert_sort_int(A)
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
    PURE SUBROUTINE partition_array_1DInt(A,p,i)
      INTEGER(SIK),INTENT(INOUT) :: A(:)
      INTEGER(SIK),INTENT(IN) :: p
      INTEGER(SIK),INTENT(OUT) :: i

      INTEGER(SIK) :: j,n,pval

      pval=A(p)
      n=SIZE(A)
      IF (p>1) THEN
        ! if Mo3 sort, 1st element is smaller than pivot, don't throw away
        CALL swap_int(A,1,2)
        CALL swap_int(A,1,p)
      ENDIF

      i=2
      DO j=2,n
        IF (A(j)<pval) THEN
          CALL swap_int(A,i,j)
          i=i+1
        ENDIF
      ENDDO
      i=i-1
      CALL swap_int(A,1,i)
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Swap location of 2 locations in 1D integer array.
!> @param A 1D integer array, modified in place and returned partioned
!> @param i Index of location 1
!> @param j Index of location 2
!>

    PURE SUBROUTINE swap_int(A,i,j)
      INTEGER(SIK),INTENT(INOUT) :: A(:)
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      INTEGER(SIK) :: tmp
      tmp=A(i)
      A(i)=A(j)
      A(j)=tmp
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief QuickSort 1D real array
!> @param A 1D real array, modified in place and returned sorted
!>
    PURE RECURSIVE SUBROUTINE qsort_1DReal(A)
      REAL(SRK),INTENT(INOUT) :: A(:)

      INTEGER(SIK) :: n,l,p,c

      n=SIZE(A)

      IF (n>50) THEN
        !median of 3 pivot
        c=n/2
        IF (A(c) < A(1)) CALL swap_real(A,c,1)
        IF (A(n) < A(1)) CALL swap_real(A,1,n)
        IF (A(n) < A(c)) CALL swap_real(A,c,n)
        p=c
       ! p=1  ! left most pivot
       ! p=FLOOR(RAND()*REAL(n,SRK),SIK)+1  ! Randomized pivot
        CALL partition_array_1DReal(A,p,l)
        CALL qsort_1DReal(A(1:l-1))
        CALL qsort_1DReal(A(l+1:n))
      ELSE
        CALL insert_sort_real(A)
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
    PURE SUBROUTINE partition_array_1DReal(A,p,i)
      REAL(SRK),INTENT(INOUT) :: A(:)
      INTEGER(SIK),INTENT(IN) :: p
      INTEGER(SIK),INTENT(OUT) :: i

      INTEGER(SIK) :: j,n
      REAL(SRK) :: pval

      pval=A(p)
      n=SIZE(A)
      IF (p>1) THEN
        ! if Mo3 sort, 1st element is smaller than pivot, don't throw away
        CALL swap_real(A,1,2)
        CALL swap_real(A,1,p)
      ENDIF

      i=2
      DO j=2,n
        IF (A(j)<pval) THEN
          CALL swap_real(A,i,j)
          i=i+1
        ENDIF
      ENDDO
      i=i-1
      CALL swap_real(A,1,i)
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Swap location of 2 locations in 1D real array.
!> @param A 1D real array, modified in place and returned partioned
!> @param i Index of location 1
!> @param j Index of location 2
!>

    PURE SUBROUTINE swap_real(A,i,j)
      REAL(SRK),INTENT(INOUT) :: A(:)
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK) :: tmp
      tmp=A(i)
      A(i)=A(j)
      A(j)=tmp
    ENDSUBROUTINE
!
ENDMODULE Sorting
