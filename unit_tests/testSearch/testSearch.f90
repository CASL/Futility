!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testSearch
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE Search

  IMPLICIT NONE

  INTEGER(SIK),DIMENSION(11) :: test_sorted = (/1,8,12,17,32,32,32,32,32,88,89/)
  INTEGER(SIK),DIMENSION(20) :: test_random = &
      (/91,5,57,81,42,81,64,25,97,99,92,25,64,18,84,20,39,9,39,86/)

!
!Check the timer resolution
  CREATE_TEST('SEARCH')
!
  REGISTER_SUBTEST('lower bound(int)',testLowerBound_i)
  REGISTER_SUBTEST('upper bound(int)',testUpperBound_i)
  REGISTER_SUBTEST('find(int)',testFind_i)
  REGISTER_SUBTEST('binarySearch(int)',testBinary_i)
  FINALIZE_TEST()
!
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testLowerBound_i()
      
      ASSERT(lowerBound(test_sorted, 0) == 1, "Wrong result for 0")

      ASSERT(lowerBound(test_sorted, 1) == 1, "Wrong result for 1")
      FINFO()lowerBound(test_sorted, 1)
      ASSERT(lowerBound(test_sorted, 2) == 2, "Wrong result for 2")
      FINFO()lowerBound(test_sorted, 2)
      ASSERT(lowerBound(test_sorted, 7) == 2, "Wrong result for 7")
      ASSERT(lowerBound(test_sorted, 8) == 2, "Wrong result for 8")
      ASSERT(lowerBound(test_sorted, 9) == 3, "Wrong result for 9")

      ASSERT(lowerBound(test_sorted, 11) == 3, "Wrong result for 12")
      ASSERT(lowerBound(test_sorted, 12) == 3, "Wrong result for 13")
      ASSERT(lowerBound(test_sorted, 13) == 4, "Wrong result for 14")

      ASSERT(lowerBound(test_sorted, 16) == 4, "Wrong result for 16")
      ASSERT(lowerBound(test_sorted, 17) == 4, "Wrong result for 17")
      ASSERT(lowerBound(test_sorted, 18) == 5, "Wrong result for 18")
      FINFO()lowerBound(test_sorted, 18)

      ASSERT(lowerBound(test_sorted, 31) == 5, "Wrong result for 31")
      ASSERT(lowerBound(test_sorted, 32) == 5, "Wrong result for 32")
      ASSERT(lowerBound(test_sorted, 33) == 10, "Wrong result for 33")

      ASSERT(lowerBound(test_sorted, 87) == 10, "Wrong result for 87")
      ASSERT(lowerBound(test_sorted, 88) == 10, "Wrong result for 88")
      ASSERT(lowerBound(test_sorted, 89) == 11, "Wrong result for 89")

      ASSERT(lowerBound(test_sorted, 90) == 12, "Wrong result for 90")
      ASSERT(lowerBound(test_sorted, 95) == 12, "Wrong result for 95")
    ENDSUBROUTINE

    SUBROUTINE testUpperBound_i()
      ASSERT(upperBound(test_sorted, 0) == 1, "Wrong result for 0")

      ASSERT(upperBound(test_sorted, 1) == 2, "Wrong result for 1")
      FINFO()upperBound(test_sorted, 1)
      ASSERT(upperBound(test_sorted, 2) == 2, "Wrong result for 2")
      FINFO()upperBound(test_sorted, 2)
      ASSERT(upperBound(test_sorted, 7) == 2, "Wrong result for 7")
      ASSERT(upperBound(test_sorted, 8) == 3, "Wrong result for 8")
      ASSERT(upperBound(test_sorted, 9) == 3, "Wrong result for 9")

      ASSERT(upperBound(test_sorted, 11) == 3, "Wrong result for 12")
      ASSERT(upperBound(test_sorted, 12) == 4, "Wrong result for 13")
      ASSERT(upperBound(test_sorted, 13) == 4, "Wrong result for 14")

      ASSERT(upperBound(test_sorted, 16) == 4, "Wrong result for 16")
      ASSERT(upperBound(test_sorted, 17) == 5, "Wrong result for 17")
      ASSERT(upperBound(test_sorted, 18) == 5, "Wrong result for 18")
      FINFO()upperBound(test_sorted, 18)

      ASSERT(upperBound(test_sorted, 31) == 5, "Wrong result for 31")
      ASSERT(upperBound(test_sorted, 32) == 10, "Wrong result for 32")
      ASSERT(upperBound(test_sorted, 33) == 10, "Wrong result for 33")

      ASSERT(upperBound(test_sorted, 87) == 10, "Wrong result for 87")
      ASSERT(upperBound(test_sorted, 88) == 11, "Wrong result for 88")
      ASSERT(upperBound(test_sorted, 89) == 12, "Wrong result for 89")

      ASSERT(upperBound(test_sorted, 90) == 12, "Wrong result for 90")
    ENDSUBROUTINE

    SUBROUTINE testFind_i()
      LOGICAL(SBK) :: bool
      ASSERT(find(test_random, 91) == 1, "Wrong result for 91")
      ASSERT(find(test_random, 86) == 20, "Wrong result for 86")
      ! this is a duplicate, technically the implementation may return either
      bool=find(test_random, 64) == 7 .OR. find(test_random, 64) == 13
      ASSERT(bool, "Wrong result for 64")
      ASSERT(find(test_random, 97) == 9, "Wrong result for 97")

      ! make sure that failed searches are handled right
      ASSERT(find(test_random, 19) == SIZE(test_random)+1, "Wrong nonexistant")
    ENDSUBROUTINE

    SUBROUTINE testBinary_i()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i
  INTEGER(SIK),DIMENSION(11) :: test_sorted = (/1,8,12,17,32,32,32,32,32,88,89/)

    ASSERT(binarySearch(test_sorted, 1) == 1, "failed with 1")
    ASSERT(binarySearch(test_sorted, 17) == 4, "failed with 17")
    i=binarySearch(test_sorted, 32)
    bool = i>=5 .AND. i<=9
    ASSERT(bool, "failed with 32")
    ASSERT(binarySearch(test_sorted, 89) == 11, "failed with 89")
    ASSERT(binarySearch(test_sorted, 13) == SIZE(test_sorted)+1, "failed 13")
    ASSERT(binarySearch(test_sorted, 0) == SIZE(test_sorted)+1, "failed 0")
    ASSERT(binarySearch(test_sorted, 90) == SIZE(test_sorted)+1, "failed 90")

    ENDSUBROUTINE testBinary_i
ENDPROGRAM testSearch
