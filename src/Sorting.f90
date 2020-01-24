!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module implementing some basic sorting algorithms.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Sorting

USE IntrType
USE Strings
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
  !> @copybrief Sorting::qsort_1DLong_1DInt
  !> @copydetails Sorting::qsort_1DLong_1DInt
  MODULE PROCEDURE qsort_1DLong_1DInt
  !> @copybrief Sorting::bubble_sort_2DInt
  !> @copydetails Sorting::bubble_sort_2DInt
  MODULE PROCEDURE bubble_sort_2DInt
  !> @copybrief Sorting::bubble_sort_1DReal_1DReal
  !> @copydetails Sorting::bubble_sort_1DReal_1DReal
  MODULE PROCEDURE bubble_sort_1DReal_1DReal
  !> @copybrief Sorting::bubble_sort_1DReal_1DInt
  !> @copydetails Sorting::bubble_sort_1DReal_1DInt
  MODULE PROCEDURE bubble_sort_1DReal_1DInt
  !> @copybrief Sorting::bubble_sort_1DInt_1DInt
  !> @copydetails Sorting::bubble_sort_1DInt_1DInt
  MODULE PROCEDURE bubble_sort_1DInt_1DInt
  !> @copybrief Sorting::bubble_sort_1DInt_1DStr
  !> @copydetails Sorting::bubble_sort_1DInt_1DStr
  MODULE PROCEDURE bubble_sort_1DInt_1DStr
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
  !> @copybrief Sorting::insert_sort_1DLong_1DInt
  !> @copydetails Sorting::insert_sort_1DLong_1DInt
  MODULE PROCEDURE insert_sort_1DLong_1DInt
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
  !> @copybrief Sorting::bubble_sort_1DReal_1DReal
  !> @copydetails Sorting::bubble_sort_1DReal_1DReal
  MODULE PROCEDURE bubble_sort_1DReal_1DReal
  !> @copybrief Sorting::bubble_sort_1DReal_1DInt
  !> @copydetails Sorting::bubble_sort_1DReal_1DInt
  MODULE PROCEDURE bubble_sort_1DReal_1DInt
  !> @copybrief Sorting::bubble_sort_1DReal_1DStr
  !> @copydetails Sorting::bubble_sort_1DReal_1DStr
  MODULE PROCEDURE bubble_sort_1DReal_1DStr
  !> @copybrief Sorting::bubble_sort_1DInt_1DInt
  !> @copydetails Sorting::bubble_sort_1DInt_1DInt
  MODULE PROCEDURE bubble_sort_1DInt_1DInt
  !> @copybrief Sorting::bubble_sort_1DInt_1DStr
  !> @copydetails Sorting::bubble_sort_1DInt_1DStr
  MODULE PROCEDURE bubble_sort_1DInt_1DStr

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
!
!-------------------------------------------------------------------------------
!> @brief A simple sorting algorithm for a 2 1-D array sorting.  The first array's
!>        data will be sorted in ascending/increasing order and returned.  The
!>        second array's data will be sorted positionally based on the first arrays
!>        sorting.
!> @example
!>        real1=(/5.0_SRK,-1.0_SRK,10.0_SRK/)
!>        real2=(/-1.0_SRK,-2.0_SRK,-3.0_SRK/)
!>        CALL bubble_sort_1DReal_1Dreal(real1,real2)
!>        real1=(/-1.0_SRK,5.0_SRK,10.0_SRK/)
!>        real2=(/-2.0_SRK,-1.0_SRK,-3.0_SRK/)
!>        !Reverse the arguments
!>        CALL bubble_sort_1DReal_1Dreal(real2,real1)
!>        real1=(/10.0_SRK,-1.0_SRK,5.0_SRK/)
!>        real2=(/-3.0_SRK,-2.0_SRK,-1.0_SRK/)
!> @param real1 A 1-D array of unsorted reals
!> @param real2 A 1-D array of unsorted reals
!>
PURE SUBROUTINE bubble_sort_1DReal_1DReal(real1,real2)
  REAL(SRK),INTENT(INOUT) :: real1(:)
  REAL(SRK),INTENT(INOUT) :: real2(:)
  !LOGICAL(SBK),INTENT(IN),OPTIONAL :: reverse
  LOGICAL(SBK) :: sorted
  INTEGER(SIK) :: i,ni,nr,ncomp
  REAL(SRK) :: tmpr

  sorted=.FALSE.
  ni=SIZE(real1,DIM=1)
  nr=SIZE(real2,DIM=1)
  ncomp=0
  IF(ni == nr) THEN
    DO WHILE(.NOT.sorted)
      sorted=.TRUE.
      ncomp=ncomp+1
      DO i=1,ni-ncomp
        IF(real1(i) > real1(i+1)) THEN
          tmpr=real1(i+1)
          real1(i+1)=real1(i)
          real1(i)=tmpr
          tmpr=real2(i+1)
          real2(i+1)=real2(i)
          real2(i)=tmpr
          sorted=.FALSE.
        ENDIF
      ENDDO
    ENDDO
  ENDIF
ENDSUBROUTINE bubble_sort_1DReal_1DReal
!
!-------------------------------------------------------------------------------
!> @brief A simple sorting algorithm for a 2 1-D array sorting.  The first array's
!>        data will be sorted in ascending/increasing order and returned.  The
!>        second array's data will be sorted positionally based on the first arrays
!>        sorting.
!> @example
!>        real1=(/5.0_SRK,-1.0_SRK,10.0_SRK/)
!>        int1=(/-1.0_SRK,-2.0_SRK,-3.0_SRK/)
!>        CALL bubble_sort_1DReal_1DInt(real1,int1)
!>        real1=(/-1.0_SRK,5.0_SRK,10.0_SRK/)
!>        int1=(/-2.0_SRK,-1.0_SRK,-3.0_SRK/)
!> @param real1 A 1-D array of unsorted reals
!> @param int1 A 1-D array of unsorted integers
!>
PURE SUBROUTINE bubble_sort_1DReal_1DInt(real1,int1,sortreal)
  REAL(SRK),INTENT(INOUT) :: real1(:)
  INTEGER(SIK),INTENT(INOUT) :: int1(:)
  LOGICAL(SBK),INTENT(IN) :: sortreal
  !LOGICAL(SBK),INTENT(IN),OPTIONAL :: reverse
  LOGICAL(SBK) :: sorted
  INTEGER(SIK) :: i,ni,nr,ncomp,tmpi
  REAL(SRK) :: tmpr

  sorted=.FALSE.
  ni=SIZE(real1,DIM=1)
  nr=SIZE(int1,DIM=1)
  ncomp=0
  IF(ni == nr) THEN
    IF(sortreal) THEN
      DO WHILE(.NOT.sorted)
        sorted=.TRUE.
        ncomp=ncomp+1
        DO i=1,ni-ncomp
          IF(real1(i) > real1(i+1)) THEN
            tmpr=real1(i+1)
            real1(i+1)=real1(i)
            real1(i)=tmpr
            tmpi=int1(i+1)
            int1(i+1)=int1(i)
            int1(i)=tmpi
            sorted=.FALSE.
          ENDIF
        ENDDO
      ENDDO
    ELSE
      DO WHILE(.NOT.sorted)
        sorted=.TRUE.
        ncomp=ncomp+1
        DO i=1,ni-ncomp
          IF(int1(i) > int1(i+1)) THEN
            tmpi=int1(i+1)
            int1(i+1)=int1(i)
            int1(i)=tmpi
            tmpr=real1(i+1)
            real1(i+1)=real1(i)
            real1(i)=tmpr
            sorted=.FALSE.
          ENDIF
        ENDDO
      ENDDO
    ENDIF
  ENDIF
ENDSUBROUTINE bubble_sort_1DReal_1DInt
!
!-------------------------------------------------------------------------------
!> @brief A simple sorting algorithm for a 2 1-D array sorting.  The first array's
!>        data will be sorted in ascending/increasing order and returned.  The
!>        second array's data will be sorted positionally based on the first arrays
!>        sorting.
!> @example
!>        real1=(/5,-1,10/)
!>        str1=(/'g','a','d'/)
!>        CALL bubble_sort_1DReal_1DStr(real1,str1)
!>        real1=(/-1,5,10/)
!>        str1=(/'a','g','d'/)
!> @param real1 A 1-D array of unsorted reals
!> @param str1 A 1-D array of unsorted strings
!>
PURE SUBROUTINE bubble_sort_1DReal_1DStr(real1,str1)
  REAL(SRK),INTENT(INOUT) :: real1(:)
  TYPE(StringType),INTENT(INOUT) :: str1(:)
  !LOGICAL(SBK),INTENT(IN),OPTIONAL :: reverse
  LOGICAL(SBK) :: sorted
  INTEGER(SIK) :: i,ni,nr,ncomp
  TYPE(StringType) :: tmpstr,tmpstr2
  REAL(SRK) :: tmpr

  sorted=.FALSE.
  ni=SIZE(real1,DIM=1)
  nr=SIZE(str1,DIM=1)
  ncomp=0
  IF(ni == nr) THEN
    DO WHILE(.NOT.sorted)
      sorted=.TRUE.
      ncomp=ncomp+1
      DO i=1,ni-ncomp
        IF(real1(i) > real1(i+1)) THEN
          tmpr=real1(i+1)
          real1(i+1)=real1(i)
          real1(i)=tmpr
          tmpstr=str1(i+1)
          tmpstr2=str1(i)
          str1(i+1)=tmpstr2
          !str1(i+1)=str1(i)
          str1(i)=tmpstr
          sorted=.FALSE.
        ENDIF
      ENDDO
    ENDDO
  ENDIF
ENDSUBROUTINE bubble_sort_1DReal_1DStr
!
!-------------------------------------------------------------------------------
!> @brief A simple sorting algorithm for a 2 1-D array sorting.  The first array's
!>        data will be sorted in ascending/increasing order and returned.  The
!>        second array's data will be sorted positionally based on the first arrays
!>        sorting.
!> @example
!>        int1=(/5,-1,10/)
!>        int2=(/-1,-2,-3/)
!>        CALL bubble_sort_1DInt_1DInt(int1,int2)
!>        int1=(/-1,5,10/)
!>        int2=(/-2,-1,-3/)
!>        !Reverse the arguments
!>        CALL bubble_sort_1DInt_1DInt(int2,int1)
!>        int1=(/10,-1,5/)
!>        int2=(/-3,-2,-1/)
!> @param real1 A 1-D array of unsorted reals
!> @param real2 A 1-D array of unsorted reals
!>
PURE SUBROUTINE bubble_sort_1DInt_1DInt(int1,int2)
  INTEGER(SIK),INTENT(INOUT) :: int1(:)
  INTEGER(SIK),INTENT(INOUT) :: int2(:)
  !LOGICAL(SBK),INTENT(IN),OPTIONAL :: reverse
  LOGICAL(SBK) :: sorted
  INTEGER(SIK) :: i,ni,nr,ncomp
  INTEGER(SIK) :: tmpi,tmpi2

  sorted=.FALSE.
  ni=SIZE(int1,DIM=1)
  nr=SIZE(int2,DIM=1)
  ncomp=0
  IF(ni == nr) THEN
    DO WHILE(.NOT.sorted)
      sorted=.TRUE.
      ncomp=ncomp+1
      DO i=1,ni-ncomp
        IF(int1(i) > int1(i+1)) THEN
          tmpi=int1(i+1)
          int1(i+1)=int1(i)
          int1(i)=tmpi
          tmpi2=int2(i+1)
          int2(i+1)=int2(i)
          int2(i)=tmpi2
          sorted=.FALSE.
        ENDIF
      ENDDO
    ENDDO
  ENDIF
ENDSUBROUTINE bubble_sort_1DInt_1DInt
!
!-------------------------------------------------------------------------------
!> @brief A simple sorting algorithm for a 2 1-D array sorting.  The first array's
!>        data will be sorted in ascending/increasing order and returned.  The
!>        second array's data will be sorted positionally based on the first arrays
!>        sorting.
!> @example
!>        int1=(/5,-1,10/)
!>        str1=(/'g','a','d'/)
!>        CALL bubble_sort_1DInt_1DStr(int1,str1)
!>        int1=(/-1,5,10/)
!>        str1=(/'a','g','d'/)
!> @param int1 A 1-D array of unsorted integers
!> @param str1 A 1-D array of unsorted strings
!>
PURE SUBROUTINE bubble_sort_1DInt_1DStr(int1,str1)
  INTEGER(SIK),INTENT(INOUT) :: int1(:)
  TYPE(StringType),INTENT(INOUT) :: str1(:)
  !LOGICAL(SBK),INTENT(IN),OPTIONAL :: reverse
  LOGICAL(SBK) :: sorted
  INTEGER(SIK) :: i,ni,nr,ncomp
  INTEGER(SIK) :: tmpi
  TYPE(StringType) :: tmpstr,tmpstr2

  sorted=.FALSE.
  ni=SIZE(int1,DIM=1)
  nr=SIZE(str1,DIM=1)
  ncomp=0
  IF(ni == nr) THEN
    DO WHILE(.NOT.sorted)
      sorted=.TRUE.
      ncomp=ncomp+1
      DO i=1,ni-ncomp
        IF(int1(i) > int1(i+1)) THEN
          tmpi=int1(i+1)
          int1(i+1)=int1(i)
          int1(i)=tmpi
          tmpstr=str1(i+1)
          tmpstr2=str1(i)
          str1(i+1)=tmpstr2
          str1(i)=tmpstr
          sorted=.FALSE.
        ENDIF
      ENDDO
    ENDDO
  ENDIF
ENDSUBROUTINE bubble_sort_1DInt_1DStr
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
!> @brief Sorts a list of long keys and integer values using the insert sort
!>        algorithm
!> @param keys the key values to sort over
!> @param vals the data values to move with key
    PURE SUBROUTINE insert_sort_1DLong_1DInt(keys,vals)
      INTEGER(SLK),INTENT(INOUT) :: keys(:)
      INTEGER(SIK),INTENT(INOUT) :: vals(:)
      !
      INTEGER(SLK) :: key
      INTEGER(SIK) :: i,j,n,val

      n=SIZE(vals)
      DO i=2,n
        key=keys(i)
        val=vals(i)
        j=i-1
        DO WHILE(keys(j) > key)
          keys(j+1)=keys(j)
          vals(j+1)=vals(j)
          j=j-1
          IF (j < 1) EXIT
        ENDDO
        keys(j+1)=key
        vals(j+1)=val
      ENDDO
    ENDSUBROUTINE insert_sort_1DLong_1DInt
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
ENDSUBROUTINE qsort_1DInt
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
ENDSUBROUTINE partition_array_1DInt

!
!-------------------------------------------------------------------------------
!> @brief Quicksort 1D array of Ints and apply same sorting operations to
!> 1D array of Reals
!> @param keys Integers to sort over
!> @param values ints to sort based on keys
PURE RECURSIVE SUBROUTINE qsort_1DLong_1DInt(keys, values)
  INTEGER(SLK),INTENT(INOUT) :: keys(:)
  INTEGER(SIK),INTENT(INOUT) :: values(:)

  INTEGER(SIK) :: n,l,p,c

  n=SIZE(values)

  IF (n>50) THEN
    !median of 3 pivot
    c=n/2+1
    IF (keys(c) < keys(1)) THEN
      CALL swap_long(keys,c,1)
      CALL swap_int(values,c,1)
    ENDIF
    IF (keys(n) < keys(1)) THEN
      CALL swap_long(keys,1,n)
      CALL swap_int(values,1,n)
    ENDIF
    IF (keys(n) < keys(c)) THEN
      CALL swap_long(keys,c,n)
      CALL swap_int(values,c,n)
    ENDIF
    p=c
    CALL partition_array_qsort_1DLong_1DInt(keys,values,p,l)
    CALL qsort_1DLong_1DInt(keys(1:l-1),values(1:l-1))
    CALL qsort_1DLong_1DInt(keys(l+1:n),values(l+1:n))
  ELSE
    CALL insert_sort_1DLong_1DInt(keys,values)
  ENDIF
ENDSUBROUTINE qsort_1DLong_1DInt
!
!-------------------------------------------------------------------------------
!> @brief Partition method 1D integer key array and associated 1D int
!>         array. For a given partion, sorts values greater than and less than
!>         the partition
!> @param keys 1D integer array, modified in place and returned partioned
!> @param values 1D int array, modified in place and returned partioned
!> @param p Index of array element to partition with
!> @param i Index of the partition element once A is partitioned
!>
PURE SUBROUTINE partition_array_qsort_1DLong_1DInt(keys,values,p,i)
  INTEGER(SLK),INTENT(INOUT) :: keys(:)
  INTEGER(SIK),INTENT(INOUT) :: values(:)
  INTEGER(SIK),INTENT(IN) :: p
  INTEGER(SIK),INTENT(OUT) :: i

  INTEGER(SIK) :: j,n
  INTEGER(SLK) :: pval

  pval=keys(p)
  n=SIZE(values)
  IF (p>1) THEN
    ! if Mo3 sort, 1st element is smaller than pivot, don't throw away
    CALL swap_long(keys,1,2)
    CALL swap_int(values,1,2)
    CALL swap_long(keys,1,p)
    CALL swap_int(values,1,p)
  ENDIF

  i=2
  DO j=2,n
    IF (keys(j)<pval) THEN
      CALL swap_long(keys,i,j)
      CALL swap_int(values,i,j)
      i=i+1
    ENDIF
  ENDDO
  i=i-1
  CALL swap_long(keys,1,i)
  CALL swap_int(values,1,i)
ENDSUBROUTINE partition_array_qsort_1DLong_1DInt
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
ENDSUBROUTINE swap_int
!
!-------------------------------------------------------------------------------
!> @brief Swap location of 2 locations in 1D long integer array.
!> @param A 1D integer array, modified in place and returned partioned
!> @param i Index of location 1
!> @param j Index of location 2
!>

PURE SUBROUTINE swap_long(A,i,j)
  INTEGER(SLK),INTENT(INOUT) :: A(:)
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK),INTENT(IN) :: j
  INTEGER(SLK) :: tmp
  tmp=A(i)
  A(i)=A(j)
  A(j)=tmp
ENDSUBROUTINE swap_long
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
ENDSUBROUTINE qsort_1DReal
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
ENDSUBROUTINE partition_array_1DReal
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
ENDSUBROUTINE swap_real
!
ENDMODULE Sorting
