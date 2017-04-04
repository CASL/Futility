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

  REAL(SRK),DIMENSION(11) :: sorted_r = (/1.0_SRK,8.0_SRK,12.0_SRK,17.0_SRK,&
    32.0_SRK,32.0_SRK,32.0_SRK,32.0_SRK,32.0_SRK,88.0_SRK,89.0_SRK/)
  REAL(SRK),DIMENSION(20) :: random_r = (/91.0_SRK,5.0_SRK,57.0_SRK, &
    81.0_SRK,42.0_SRK,81.0_SRK,64.0_SRK,25.0_SRK,97.0_SRK,99.0_SRK,92.0_SRK, &
    25.0_SRK,64.0_SRK,18.0_SRK,84.0_SRK,20.0_SRK,39.0_SRK,9.0_SRK,39.0_SRK, &
    86.0_SRK/)

!
!Check the timer resolution
  CREATE_TEST('SEARCH')
!
  REGISTER_SUBTEST('lower bound(int)',testLowerBound_i)
  REGISTER_SUBTEST('upper bound(int)',testUpperBound_i)
  REGISTER_SUBTEST('find(int)',testFind_i)
  REGISTER_SUBTEST('binarySearch(int)',testBinary_i)

  REGISTER_SUBTEST('lower bound(real)',testLowerBound_r)
  REGISTER_SUBTEST('upper bound(real)',testUpperBound_r)
  REGISTER_SUBTEST('find(real)',testFind_r)
  REGISTER_SUBTEST('binarySearch(real)',testBinary_r)

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
!
!-------------------------------------------------------------------------------
    SUBROUTINE testLowerBound_r()
      
      ASSERT(lowerBound(sorted_r, 0.0_SRK) == 1, "Wrong result for 0")
      FINFO()lowerBound(sorted_r, 0.0_SRK)

      ASSERT(lowerBound(sorted_r, 1.0_SRK) == 1, "Wrong result for 1")
      FINFO()lowerBound(sorted_r, 1.0_SRK)
      ASSERT(lowerBound(sorted_r, 2.0_SRK) == 2, "Wrong result for 2")
      ASSERT(lowerBound(sorted_r, 7.0_SRK) == 2, "Wrong result for 7")
      ASSERT(lowerBound(sorted_r, 8.0_SRK) == 2, "Wrong result for 8")
      ASSERT(lowerBound(sorted_r, 8.05_SRK, 0.1_SRK) == 2, "Wrong result for ~8")
      ASSERT(lowerBound(sorted_r, 8.11_SRK, 0.1_SRK) == 3, "Wrong result for ~8")
      ASSERT(lowerBound(sorted_r, 9.0_SRK) == 3, "Wrong result for 9")

      ASSERT(lowerBound(sorted_r, 11.0_SRK) == 3, "Wrong result for 12")
      ASSERT(lowerBound(sorted_r, 12.0_SRK) == 3, "Wrong result for 13")
      ASSERT(lowerBound(sorted_r, 13.0_SRK) == 4, "Wrong result for 14")

      ASSERT(lowerBound(sorted_r, 16.0_SRK) == 4, "Wrong result for 16")
      ASSERT(lowerBound(sorted_r, 17.0_SRK) == 4, "Wrong result for 17")
      ASSERT(lowerBound(sorted_r, 18.0_SRK) == 5, "Wrong result for 18")
      FINFO()lowerBound(sorted_r, 18.0_SRK)

      ASSERT(lowerBound(sorted_r, 31.0_SRK) == 5, "Wrong result for 31")
      ASSERT(lowerBound(sorted_r, 32.0_SRK) == 5, "Wrong result for 32")
      ASSERT(lowerBound(sorted_r, 33.0_SRK) == 10, "Wrong result for 33")

      ASSERT(lowerBound(sorted_r, 87.0_SRK) == 10, "Wrong result for 87")
      ASSERT(lowerBound(sorted_r, 88.0_SRK) == 10, "Wrong result for 88")
      ASSERT(lowerBound(sorted_r, 89.0_SRK) == 11, "Wrong result for 89")

      ASSERT(lowerBound(sorted_r, 90.0_SRK) == 12, "Wrong result for 90")
      ASSERT(lowerBound(sorted_r, 95.0_SRK) == 12, "Wrong result for 95")
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
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
!
!-------------------------------------------------------------------------------
    SUBROUTINE testUpperBound_r()
      ASSERT(upperBound(sorted_r, 0.0_SRK) == 1, "Wrong result for 0")

      ASSERT(upperBound(sorted_r, 1.0_SRK) == 2, "Wrong result for 1")
      ASSERT(upperBound(sorted_r, 2.0_SRK) == 2, "Wrong result for 2")
      ASSERT(upperBound(sorted_r, 7.0_SRK) == 2, "Wrong result for 7")
      ASSERT(upperBound(sorted_r, 8.0_SRK) == 3, "Wrong result for 8")
      ASSERT(upperBound(sorted_r, 7.89_SRK, 0.1_SRK) == 2, "Wrong result for 8")
      ASSERT(upperBound(sorted_r, 8.05_SRK, 0.1_SRK) == 3, "Wrong result for 8")
      ASSERT(upperBound(sorted_r, 8.11_SRK, 0.1_SRK) == 3, "Wrong result for 8")
      ASSERT(upperBound(sorted_r, 9.0_SRK) == 3, "Wrong result for 9")

      ASSERT(upperBound(sorted_r, 11.0_SRK) == 3, "Wrong result for 12")
      ASSERT(upperBound(sorted_r, 12.0_SRK) == 4, "Wrong result for 13")
      ASSERT(upperBound(sorted_r, 13.0_SRK) == 4, "Wrong result for 14")

      ASSERT(upperBound(sorted_r, 16.0_SRK) == 4, "Wrong result for 16")
      ASSERT(upperBound(sorted_r, 17.0_SRK) == 5, "Wrong result for 17")
      ASSERT(upperBound(sorted_r, 18.0_SRK) == 5, "Wrong result for 18")

      ASSERT(upperBound(sorted_r, 31.0_SRK) == 5, "Wrong result for 31")
      ASSERT(upperBound(sorted_r, 32.0_SRK) == 10, "Wrong result for 32")
      ASSERT(upperBound(sorted_r, 33.0_SRK) == 10, "Wrong result for 33")

      ASSERT(upperBound(sorted_r, 87.0_SRK) == 10, "Wrong result for 87")
      ASSERT(upperBound(sorted_r, 88.0_SRK) == 11, "Wrong result for 88")
      ASSERT(upperBound(sorted_r, 89.0_SRK) == 12, "Wrong result for 89")

      ASSERT(upperBound(sorted_r, 90.0_SRK) == 12, "Wrong result for 90")
    ENDSUBROUTINE

!
!-------------------------------------------------------------------------------
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
!
!-------------------------------------------------------------------------------
    SUBROUTINE testFind_r()
      LOGICAL(SBK) :: bool
      ASSERT(find(random_r, 91.0_SRK) == 1, "Wrong result for 91")
      ASSERT(find(random_r, 86.0_SRK) == 20, "Wrong result for 86")
      ! this is a duplicate, technically the implementation may return either
      bool=find(random_r, 64.0_SRK) == 7 .OR. find(random_r, 64.0_SRK) == 13
      ASSERT(bool, "Wrong result for 64")
      ASSERT(find(random_r, 97.0_SRK) == 9, "Wrong result for 97")

      ! make sure that failed searches are handled right
      ASSERT(find(random_r, 19.0_SRK) == SIZE(random_r)+1, "Wrong nonexistant")
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
    SUBROUTINE testBinary_i()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i

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
!
!-------------------------------------------------------------------------------
    SUBROUTINE testBinary_r()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i

      ASSERT(binarySearch(sorted_r, 1.0_SRK) == 1, "failed with 1")
      ASSERT(binarySearch(sorted_r, 17.0_SRK) == 4, "failed with 17")
      ASSERT(binarySearch(sorted_r, 17.05_SRK, 0.1_SRK) == 4, "failed with 17")
      i=binarySearch(sorted_r, 32.0_SRK)
      bool = i>=5 .AND. i<=9
      ASSERT(bool, "failed with 32")
      ASSERT(binarySearch(sorted_r, 89.0_SRK) == 11, "failed with 89")
      ASSERT(binarySearch(sorted_r, 13.0_SRK) == SIZE(sorted_r)+1, "failed 13")
      ASSERT(binarySearch(sorted_r, 0.0_SRK) == SIZE(sorted_r)+1, "failed 0")
      ASSERT(binarySearch(sorted_r, 90.0_SRK) == SIZE(sorted_r)+1, "failed 90")

    ENDSUBROUTINE
ENDPROGRAM testSearch
