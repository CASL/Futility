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

  INTEGER(SIK),DIMENSION(11) :: test_list = (/1,8,12,17,32,32,32,32,32,88,89/)

!
!Check the timer resolution
  CREATE_TEST('SEARCH')
!
  REGISTER_SUBTEST('lower bound',testLowerBound)
  REGISTER_SUBTEST('upper bound',testUpperBound)
  FINALIZE_TEST()
!
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testLowerBound()
      
      ASSERT(lower_bound(test_list, 0) == 1, "Wrong result for 0")

      ASSERT(lower_bound(test_list, 1) == 1, "Wrong result for 1")
      FINFO()lower_bound(test_list, 1)
      ASSERT(lower_bound(test_list, 2) == 2, "Wrong result for 2")
      FINFO()lower_bound(test_list, 2)
      ASSERT(lower_bound(test_list, 7) == 2, "Wrong result for 7")
      ASSERT(lower_bound(test_list, 8) == 2, "Wrong result for 8")
      ASSERT(lower_bound(test_list, 9) == 3, "Wrong result for 9")

      ASSERT(lower_bound(test_list, 11) == 3, "Wrong result for 12")
      ASSERT(lower_bound(test_list, 12) == 3, "Wrong result for 13")
      ASSERT(lower_bound(test_list, 13) == 4, "Wrong result for 14")

      ASSERT(lower_bound(test_list, 16) == 4, "Wrong result for 16")
      ASSERT(lower_bound(test_list, 17) == 4, "Wrong result for 17")
      ASSERT(lower_bound(test_list, 18) == 5, "Wrong result for 18")
      FINFO()lower_bound(test_list, 18)

      ASSERT(lower_bound(test_list, 31) == 5, "Wrong result for 31")
      ASSERT(lower_bound(test_list, 32) == 5, "Wrong result for 32")
      ASSERT(lower_bound(test_list, 33) == 10, "Wrong result for 33")

      ASSERT(lower_bound(test_list, 87) == 10, "Wrong result for 87")
      ASSERT(lower_bound(test_list, 88) == 10, "Wrong result for 88")
      ASSERT(lower_bound(test_list, 89) == 11, "Wrong result for 89")

      ASSERT(lower_bound(test_list, 90) == 12, "Wrong result for 90")


    ENDSUBROUTINE

SUBROUTINE testUpperBound()
      ASSERT(upper_bound(test_list, 0) == 1, "Wrong result for 0")

      ASSERT(upper_bound(test_list, 1) == 2, "Wrong result for 1")
      FINFO()upper_bound(test_list, 1)
      ASSERT(upper_bound(test_list, 2) == 2, "Wrong result for 2")
      FINFO()upper_bound(test_list, 2)
      ASSERT(upper_bound(test_list, 7) == 2, "Wrong result for 7")
      ASSERT(upper_bound(test_list, 8) == 3, "Wrong result for 8")
      ASSERT(upper_bound(test_list, 9) == 3, "Wrong result for 9")

      ASSERT(upper_bound(test_list, 11) == 3, "Wrong result for 12")
      ASSERT(upper_bound(test_list, 12) == 4, "Wrong result for 13")
      ASSERT(upper_bound(test_list, 13) == 4, "Wrong result for 14")

      ASSERT(upper_bound(test_list, 16) == 4, "Wrong result for 16")
      ASSERT(upper_bound(test_list, 17) == 5, "Wrong result for 17")
      ASSERT(upper_bound(test_list, 18) == 5, "Wrong result for 18")
      FINFO()upper_bound(test_list, 18)

      ASSERT(upper_bound(test_list, 31) == 5, "Wrong result for 31")
      ASSERT(upper_bound(test_list, 32) == 10, "Wrong result for 32")
      ASSERT(upper_bound(test_list, 33) == 10, "Wrong result for 33")

      ASSERT(upper_bound(test_list, 87) == 10, "Wrong result for 87")
      ASSERT(upper_bound(test_list, 88) == 11, "Wrong result for 88")
      ASSERT(upper_bound(test_list, 89) == 12, "Wrong result for 89")

      ASSERT(upper_bound(test_list, 90) == 12, "Wrong result for 90")


    ENDSUBROUTINE

      
ENDPROGRAM testSearch
