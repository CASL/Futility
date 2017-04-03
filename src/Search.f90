!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module implementing some basic search algorithms.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>
!> @author Mitchell Young
!>    @date 3/30/2017
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Search
  USE IntrType
  USE Strings
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: upperBound
  PUBLIC :: lowerBound
  PUBLIC :: find
  PUBLIC :: binarySearch

  !> @brief Return the index of the first element of a list that compares
  !> greater than the passed value
  !> @param list the list to search. The list should be sorted, otherwise
  !> undefined behavior.
  !> @param val the value to find
  !> @param tol the comparison tolerance to use for floating-point
  !> implementations. Optional
  !> @returns the index to the first element of the list that compares greater
  !> than the passed search value
  !>
  !> If the value is greater than or equal to the largest element of the list,
  !> returns SIZE + 1.
  INTERFACE upperBound
    MODULE PROCEDURE upper_bound_i
    MODULE PROCEDURE upper_bound_r
  ENDINTERFACE upperBound

  !> @brief  Return the index of the first element of a list that does not
  !> compare less than the passed value
  !> @param list the list to search. The list should be sorted, otherwise
  !> undefined behavior
  !> @param val the value to find
  !> @param tol the comparison tolerance to use for floating-point
  !> implementations. Optional
  !> @returns the index to the first element of the list that does not compare
  !> less than the passed value
  !>
  !> If the value is greater than the last element of the list, returns SIZE+1
  INTERFACE lowerBound
    MODULE PROCEDURE lower_bound_i
    MODULE PROCEDURE lower_bound_r
  ENDINTERFACE lowerBound

  !> @brief Return the index of an element in a list which compares equal to the
  !> passed value.
  !> @param list the list to search. Unlike for binarySearch, the list need not
  !> be sorted.
  !> @param val the value to find
  !> @param tol the comparison tolerance to use for floating-point
  !> implementations. Optional
  !>
  INTERFACE find
    MODULE PROCEDURE find_i
    MODULE PROCEDURE find_r
  ENDINTERFACE find

  !> @brief Return the index of an element in a list which compares equal to the
  !> passed value.
  !> @param list the list to search. The list should be sorted, otherwise
  !> undefined behavior
  !> @param val the value to find
  !> @param tol the comparison tolerance to use for floating-point
  !> implementations. Optional
  !>
  INTERFACE binarySearch
    MODULE PROCEDURE binary_search_i
    MODULE PROCEDURE binary_search_r
  ENDINTERFACE binarySearch

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    PURE FUNCTION upper_bound_i(list,val) RESULT (i)
      INTEGER(SIK),INTENT(IN) :: list(:)
      INTEGER(SIK),INTENT(IN) :: val
      INTEGER(SIK) :: j
      !
      INTEGER(SIK) :: n, step, i

      j=1
      n=SIZE(list)

      DO WHILE(n>0)
        i=j
        step=n/2
        i=i+step
        IF(.NOT. (val < list(i))) THEN
          i=i+1
          j=i
          n=n-step-1
        ELSE
          n=step
        ENDIF
      ENDDO

    ENDFUNCTION upper_bound_i
!
!-------------------------------------------------------------------------------
    PURE FUNCTION upper_bound_r(list,val,tol) RESULT (i)
      REAL(SRK),INTENT(IN) :: list(:)
      REAL(SRK),INTENT(IN) :: val
      REAL(SRK),INTENT(IN),OPTIONAL :: tol
      INTEGER(SIK) :: j
      !
      INTEGER(SIK) :: n, step, i
      REAL(SRK) :: eps

      eps=EPSREAL
      IF(PRESENT(tol)) THEN
        eps=tol
      ENDIF

      j=1
      n=SIZE(list)

      DO WHILE(n>0)
        i=j
        step=n/2
        i=i+step
        IF(.NOT. SOFTLE(val,list(i),eps)) THEN
          i=i+1
          j=i
          n=n-step-1
        ELSE
          n=step
        ENDIF
      ENDDO

    ENDFUNCTION upper_bound_r
!
!-------------------------------------------------------------------------------
    PURE FUNCTION lower_bound_i(list,val) RESULT(j)
      INTEGER(SIK),INTENT(IN) :: list(:)
      INTEGER(SIK),INTENT(IN) :: val
      INTEGER(SIK) :: j
      !
      INTEGER(SIK) :: n, step, i

      j=1
      n=SIZE(list)

      DO WHILE(n>0)
        i=j
        step = n/2
        i=i+step
        IF(list(i) < val) THEN
          i=i+1
          j=i
          n=n-step-1
        ELSE
          n=step
        ENDIF
      ENDDO

    ENDFUNCTION lower_bound_i
!
!-------------------------------------------------------------------------------
    PURE FUNCTION lower_bound_r(list,val,tol) RESULT(j)
      REAL(SRK),INTENT(IN) :: list(:)
      REAL(SRK),INTENT(IN) :: val
      REAL(SRK),INTENT(IN),OPTIONAL :: tol
      INTEGER(SIK) :: j
      !
      INTEGER(SIK) :: n, step, i
      REAL(SRK) :: eps

      eps=EPSREAL
      IF(PRESENT(tol)) THEN
        eps=tol
      ENDIF

      j=1
      n=SIZE(list)

      DO WHILE(n>0)
        i=j
        step = n/2
        i=i+step
        IF(SOFTLE(list(i),val,eps)) THEN
          i=i+1
          j=i
          n=n-step-1
        ELSE
          n=step
        ENDIF
      ENDDO

    ENDFUNCTION lower_bound_r
!
!-------------------------------------------------------------------------------
    PURE FUNCTION find_i(list, val) RESULT(i)
      INTEGER(SIK),INTENT(IN) :: list(:)
      INTEGER(SIK),INTENT(IN) :: val
      INTEGER(SIK) :: i

      DO i=1, SIZE(list)
        IF(list(i) == val) RETURN
      ENDDO

      i=SIZE(list)+1
    ENDFUNCTION find_i
!
!-------------------------------------------------------------------------------
    PURE FUNCTION find_r(list,val,tol) RESULT(i)
      REAL(SRK),INTENT(IN) :: list(:)
      REAL(SRK),INTENT(IN) :: val
      REAL(SRK),INTENT(IN),OPTIONAL :: tol
      INTEGER(SIK) :: i
      !
      REAL(SRK) :: eps

      eps=EPSREAL
      IF(PRESENT(tol)) THEN
        eps=tol
      ENDIF

      DO i=1, SIZE(list)
        IF(SOFTEQ(list(i), val, eps)) RETURN
      ENDDO

      i=SIZE(list)+1
    ENDFUNCTION find_r
!
!-------------------------------------------------------------------------------
    PURE FUNCTION binary_search_i(list, val) RESULT(i)
      INTEGER(SIK),INTENT(IN) :: list(:)
      INTEGER(SIK),INTENT(IN) :: val
      INTEGER(SIK) :: i

      i=lowerBound(list,val)
      IF(i > 0 .AND. i <= SIZE(list)) THEN
        IF(list(i) == val) THEN
          RETURN
        ENDIF
      ENDIF

      i=SIZE(list)+1
    ENDFUNCTION binary_search_i
!
!-------------------------------------------------------------------------------
    PURE FUNCTION binary_search_r(list, val, tol) RESULT(i)
      REAL(SRK),INTENT(IN) :: list(:)
      REAL(SRK),INTENT(IN) :: val
      REAL(SRK),INTENT(IN),OPTIONAL :: tol
      INTEGER(SIK) :: i
      !
      REAL(SRK) :: eps

      eps=EPSREAL
      IF(PRESENT(tol)) THEN
        eps=tol
      ENDIF

      i=lowerBound(list,val,tol)
      IF(SOFTEQ(list(i),val,eps)) THEN
        RETURN
      ENDIF

      i=SIZE(list)+1
    ENDFUNCTION binary_search_r

      
ENDMODULE Search
