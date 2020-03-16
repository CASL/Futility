!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module implementing some basic search algorithms.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Search
#include "Futility_DBC.h"
USE IntrType
USE Strings
USE ArrayUtils
USE Futility_DBC

IMPLICIT NONE
PRIVATE

PUBLIC :: getFirstGreater
PUBLIC :: getFirstGreaterEqual
PUBLIC :: find
PUBLIC :: binarySearch

!> @brief Interface for getting "upper bounds"
!>
INTERFACE getFirstGreater
  !> @copybrief Search::getFirstGreater_i
  !> @copydetails Search::getFirstGreater_i
  MODULE PROCEDURE getFirstGreater_i
  !> @copybrief Search::getFirstGreater_r
  !> @copydetails Search::getFirstGreater_r
  MODULE PROCEDURE getFirstGreater_r
ENDINTERFACE getFirstGreater

!> @brief Interface for getting "lower bounds"
!>
INTERFACE getFirstGreaterEqual
  !> @copybrief Search::getFirstGreaterEqual_i
  !> @copydetails Search::getFirstGreaterEqual_i
  MODULE PROCEDURE getFirstGreaterEqual_i
  !> @copybrief Search::getFirstGreaterEqual_r
  !> @copydetails Search::getFirstGreaterEqual_r
  MODULE PROCEDURE getFirstGreaterEqual_r
ENDINTERFACE getFirstGreaterEqual

!> @brief Interface for finding entry of a 1D array using a linear search
!>
INTERFACE find
  !> @copybrief Search::linearSearch_i
  !> @copydetails Search::linearSearch_i
  MODULE PROCEDURE linearSearch_i
  !> @copybrief Search::linearSearch_r
  !> @copydetails Search::linearSearch_r
  MODULE PROCEDURE linearSearch_r
ENDINTERFACE find

!> @brief Interface for finding entry of a 1D array using a binary search
!>
INTERFACE binarySearch
  !> @copybrief Search::binarySearch_i
  !> @copydetails Search::binarySearch_i
  MODULE PROCEDURE binarySearch_i
  !> @copybrief Search::binarySearch_r
  !> @copydetails Search::binarySearch_r
  MODULE PROCEDURE binarySearch_r
ENDINTERFACE binarySearch
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Returns the index of the first element of a list of integers that is
!>        greater than val
!> @param list 1D array of integers to search through. The list should be sorted in
!>        ascending order
!> @param val integer value to find the first entry of list that is greater than
!> @returns i the index of the first element of list that is greater than val
!>
!> NOTE: If val is greater than or equal to the largest element of list this
!>       returns SIZE(list)+1, and if val is smaller than the smallest element of
!>       list this returns 1.
!>
FUNCTION getFirstGreater_i(list,val) RESULT (i)
  INTEGER(SIK),INTENT(IN) :: list(:)
  INTEGER(SIK),INTENT(IN) :: val
  INTEGER(SIK) :: j
  !
  INTEGER(SIK) :: n,step,i

  REQUIRE(isIncreasing(list))

  !Perform binary search
  j=1
  n=SIZE(list)
  DO WHILE(n>0)
    step=n/2
    i=j+step
    IF(.NOT. (val < list(i))) THEN
      i=i+1
      j=i
      n=n-step-1
    ELSE
      n=step
    ENDIF
  ENDDO

ENDFUNCTION getFirstGreater_i
!
!-------------------------------------------------------------------------------
!> @brief Returns the index of the first element of a list of reals that is
!>        greater than val
!> @param list 1D array of reals to search through. The list should be sorted in
!>        ascending order
!> @param val real value to find the first entry of list that is greater than
!> @param tol the comparison tolerance to use. Optional
!> @returns i the index of the first element of list that is greater than val
!>
!> NOTE: If val is greater than or equal to the largest element of list this
!>       returns SIZE(list)+1. If val is smaller than the smallest element of list
!>       this returns 1.
!>
FUNCTION getFirstGreater_r(list,val,tol) RESULT (i)
  REAL(SRK),INTENT(IN) :: list(:)
  REAL(SRK),INTENT(IN) :: val
  REAL(SRK),INTENT(IN),OPTIONAL :: tol
  INTEGER(SIK) :: j
  !
  INTEGER(SIK) :: n,step,i
  REAL(SRK) :: eps

  REQUIRE(isIncreasing(list))

  !Set Soft comparison tolerance
  IF(PRESENT(tol)) THEN
    eps=tol
  ELSE
    eps=EPSREAL
  ENDIF

  !Perform binary search
  j=1
  n=SIZE(list)
  DO WHILE(n>0)
    step=n/2
    i=j+step
    IF(.NOT. SOFTLT(val,list(i),eps)) THEN
      i=i+1
      j=i
      n=n-step-1
    ELSE
      n=step
    ENDIF
  ENDDO

ENDFUNCTION getFirstGreater_r
!
!-------------------------------------------------------------------------------
!> @brief Return the index of the first element of a list that does not
!>        compare less than the passed value
!> @param list 1D array of integers to search through. The list should be sorted in
!>        ascending order
!> @param val integer value to find
!> @returns j the index of the element of list that is immediatly less than val
!>
!> NOTE: If val is greater than the largest element of list this returns SIZE(list)+1.
!>       If val is smaller than the smallest element of list this returns 1.
!>
FUNCTION getFirstGreaterEqual_i(list,val) RESULT(j)
  INTEGER(SIK),INTENT(IN) :: list(:)
  INTEGER(SIK),INTENT(IN) :: val
  INTEGER(SIK) :: j
  !
  INTEGER(SIK) :: n,step,i
  n=SIZE(list)
  j=1

  REQUIRE(isMonotonic(list))

  !Check length and return if insufficient
  IF(n < 2) THEN
    IF(list(1) < val) j=2
    RETURN
  ENDIF

  !Perform binary search
  IF(list(1) < list(n)) THEN
    DO WHILE(n>0)
      step=n/2
      i=j+step
      IF(list(i) < val) THEN
        i=i+1
        j=i
        n=n-step-1
      ELSE
        n=step
      ENDIF
    ENDDO
  ELSE
    DO WHILE(n>0)
      step=n/2
      i=j+step
      IF(list(i) > val) THEN
        i=i+1
        j=i
        n=n-step-1
      ELSE
        n=step
      ENDIF
    ENDDO
  ENDIF

ENDFUNCTION getFirstGreaterEqual_i
!
!-------------------------------------------------------------------------------
!> @brief Return the index of the first element of a list that does not
!>        compare less than the passed value
!> @param list 1D array of reals to search through. The list should be sorted in
!>        ascending order
!> @param val real value to find
!> @param tol the comparison tolerance to use. Optional
!> @returns j the index of the element of list that is immediatly less than val
!>
!> NOTE: If val is greater than the largest element of list this returns SIZE(list)+1.
!>       If val is smaller than the smallest element of list this returns 1.
!>
FUNCTION getFirstGreaterEqual_r(list,val,tol) RESULT(j)
  REAL(SRK),INTENT(IN) :: list(:)
  REAL(SRK),INTENT(IN) :: val
  REAL(SRK),INTENT(IN),OPTIONAL :: tol
  INTEGER(SIK) :: j
  !
  PROCEDURE(SOFTLTwrap),POINTER :: condComp
  INTEGER(SIK) :: n,step,i
  REAL(SRK) :: eps
  n=SIZE(list)
  j=1

  REQUIRE(isMonotonic(list))

  !Set Soft comparison tolerance
  IF(PRESENT(tol)) THEN
    eps=tol
  ELSE
    eps=EPSREAL
  ENDIF

  !Check length and return if insufficient
  IF(n < 2) THEN
    IF(SOFTLT(list(1),val,eps)) j=2
    RETURN
  ENDIF

  IF(list(1) < list(n)) THEN
    condComp => SOFTLTwrap
  ELSE
    condComp => SOFTGTwrap
  ENDIF

  !Perform binary search
  DO WHILE(n>0)
    step=n/2
    i=j+step
    IF(condComp(list(i),val,eps)) THEN
      i=i+1
      j=i
      n=n-step-1
    ELSE
      n=step
    ENDIF
  ENDDO

ENDFUNCTION getFirstGreaterEqual_r
!
!-------------------------------------------------------------------------------
!> @brief Return the index of the element of a list of integers that is equal to
!>        val using a linear search algorithm
!> @param list 1D array of integers to search through. Unlike for binary_search,
!>        sorting does not matter
!> @param val integer value to find in list
!> @returns i index of the element of list that equals val
!>
!> NOTE: If the value is not found this returns SIZE(list)+1
!>
FUNCTION linearSearch_i(list, val) RESULT(i)
  INTEGER(SIK),INTENT(IN) :: list(:)
  INTEGER(SIK),INTENT(IN) :: val
  INTEGER(SIK) :: i

  !Perform linear search
  DO i=1,SIZE(list)
    IF(list(i) == val) RETURN
  ENDDO

  i=SIZE(list)+1
ENDFUNCTION linearSearch_i
!
!-------------------------------------------------------------------------------
!> @brief Return the index of the element of a list of reals that is equal to val
!>        within tol using a linear search algorithm
!> @param list 1D array of reals to search through. Unlike for binary_search,
!>        sorting does not matter
!> @param val real value to find in list
!> @param tol the comparison tolerance to use. Optional
!> @returns i index of the element of list that equals val
!>
!> NOTE: If the value is not found this returns SIZE(list)+1
!>
FUNCTION linearSearch_r(list,val,tol) RESULT(i)
  REAL(SRK),INTENT(IN) :: list(:)
  REAL(SRK),INTENT(IN) :: val
  REAL(SRK),INTENT(IN),OPTIONAL :: tol
  INTEGER(SIK) :: i
  !
  REAL(SRK) :: eps

  !Set Soft comparison tolerance
  IF(PRESENT(tol)) THEN
    eps=tol
  ELSE
    eps=EPSREAL
  ENDIF

  !Perform linear search
  DO i=1,SIZE(list)
    IF(SOFTEQ(list(i), val, eps)) RETURN
  ENDDO

  i=SIZE(list)+1
ENDFUNCTION linearSearch_r
!
!-------------------------------------------------------------------------------
!> @brief Return the index of the element of a list of integers that is equal to
!>        val using a binary search algorithm
!> @param list 1D array of integers to search through. The list should be sorted in
!>        ascending order
!> @param val integer value to find in list
!> @returns i index of the element of list that equals val
!>
!> NOTE: If the value is not found this returns SIZE(list)+1
!>
FUNCTION binarySearch_i(list,val) RESULT(i)
  INTEGER(SIK),INTENT(IN) :: list(:)
  INTEGER(SIK),INTENT(IN) :: val
  INTEGER(SIK) :: i

  REQUIRE(isIncreasing(list))

  !Find index of list entry at or just before val
  i=getFirstGreaterEqual(list,val)
  IF(i <= SIZE(list)) THEN
    IF(list(i) == val) THEN
      RETURN
    ENDIF
  ENDIF

  !If above equality check failed increment index by one
  i=SIZE(list)+1
ENDFUNCTION binarySearch_i
!
!-------------------------------------------------------------------------------
!> @brief Return the index of the element of a list of reals that is equal to val
!>        within tol using a binary search algorithm
!> @param list 1D array of reals to search through. The list should be sorted in
!>        ascending order
!> @param val real value to find in list
!> @param tol the comparison tolerance to use. Optional
!> @returns i index of the element of list that equals val
!>
!> NOTE: If the value is not found this returns SIZE(list)+1
!>
FUNCTION binarySearch_r(list,val,tol) RESULT(i)
  REAL(SRK),INTENT(IN) :: list(:)
  REAL(SRK),INTENT(IN) :: val
  REAL(SRK),INTENT(IN),OPTIONAL :: tol
  INTEGER(SIK) :: i
  !
  REAL(SRK) :: eps

  REQUIRE(isIncreasing(list))

  !Set Soft comparison tolerance
  IF(PRESENT(tol)) THEN
    eps=tol
  ELSE
    eps=EPSREAL
  ENDIF

  !Find index of list entry at or just before val
  i=getFirstGreaterEqual(list,val,tol)
  IF(i <= SIZE(list)) THEN
    IF(SOFTEQ(list(i),val,eps)) THEN
      RETURN
    ENDIF
  ENDIF

  !If above equality check failed increment index by one
  i=SIZE(list)+1
ENDFUNCTION binarySearch_r
!
!-------------------------------------------------------------------------------
!> @brief Wrapper functions for the SOFT GT and LT functions to give them
!>        explicitly SRK type arguments and allow for precedure pointer usage
!> @param r1 first real to compare
!> @param r2 second real to comapre
!> @param tol the comparison tolerance to use.
!> @returns bool result of comparison
!>
FUNCTION SOFTLTwrap(r1,r2,tol) RESULT(bool)
  REAL(SRK),INTENT(IN) :: r1
  REAL(SRK),INTENT(IN) :: r2
  REAL(SRK),INTENT(IN) :: tol
  LOGICAL(SBK) :: bool

  bool=SOFTLT(r1,r2,tol)

ENDFUNCTION SOFTLTwrap
!
FUNCTION SOFTGTwrap(r1,r2,tol) RESULT(bool)
  REAL(SRK),INTENT(IN) :: r1
  REAL(SRK),INTENT(IN) :: r2
  REAL(SRK),INTENT(IN) :: tol
  LOGICAL(SBK) :: bool

  bool=SOFTGT(r1,r2,tol)

ENDFUNCTION SOFTGTwrap
!
ENDMODULE Search