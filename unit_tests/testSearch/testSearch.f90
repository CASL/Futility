!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained,open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testSearch
#include"UnitTest.h"
USE UnitTest
USE IntrType
USE Search

IMPLICIT NONE

INTEGER(SIK),DIMENSION(11) :: increasing_i = (/1,8,12,17,32,32,32,32,32,88,89/)
INTEGER(SIK),DIMENSION(20) :: random_i = &
      (/91,5,57,81,42,81,64,25,97,99,92,25,64,18,84,20,39,9,39,86/)

REAL(SRK),DIMENSION(11) :: increasing_r = (/1.0_SRK,8.0_SRK,12.0_SRK,17.0_SRK,&
      32.0_SRK,32.0_SRK,32.0_SRK,32.0_SRK,32.0_SRK,88.0_SRK,89.0_SRK/)
REAL(SRK),DIMENSION(20) :: random_r = (/91.0_SRK,5.0_SRK,57.0_SRK,&
      81.0_SRK,42.0_SRK,81.0_SRK,64.0_SRK,25.0_SRK,97.0_SRK,99.0_SRK,92.0_SRK,&
      25.0_SRK,64.0_SRK,18.0_SRK,84.0_SRK,20.0_SRK,39.0_SRK,9.0_SRK,39.0_SRK,&
      86.0_SRK/)

!
!Check the timer resolution
CREATE_TEST('SEARCH')
!
REGISTER_SUBTEST('lower bound(int)',testgetFirstGreaterEqual_i)
REGISTER_SUBTEST('upper bound(int)',testgetFirstGreater_i)
REGISTER_SUBTEST('find(int)',testFind_i)
REGISTER_SUBTEST('binarySearch(int)',testBinary_i)

REGISTER_SUBTEST('lower bound(real)',testgetFirstGreaterEqual_r)
REGISTER_SUBTEST('upper bound(real)',testgetFirstGreater_r)
REGISTER_SUBTEST('find(real)',testFind_r)
REGISTER_SUBTEST('binarySearch(real)',testBinary_r)

FINALIZE_TEST()
!
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testgetFirstGreaterEqual_i()

  ASSERT_EQ(getFirstGreaterEqual(increasing_i,0),1,"Wrong result for 0")

  ASSERT_EQ(getFirstGreaterEqual(increasing_i,1),1,"Wrong result for 1")
  ASSERT_EQ(getFirstGreaterEqual(increasing_i,2),2,"Wrong result for 2")
  ASSERT_EQ(getFirstGreaterEqual(increasing_i,7),2,"Wrong result for 7")
  ASSERT_EQ(getFirstGreaterEqual(increasing_i,8),2,"Wrong result for 8")
  ASSERT_EQ(getFirstGreaterEqual(increasing_i,9),3,"Wrong result for 9")

  ASSERT_EQ(getFirstGreaterEqual(increasing_i,11),3,"Wrong result for 12")
  ASSERT_EQ(getFirstGreaterEqual(increasing_i,12),3,"Wrong result for 13")
  ASSERT_EQ(getFirstGreaterEqual(increasing_i,13),4,"Wrong result for 14")

  ASSERT_EQ(getFirstGreaterEqual(increasing_i,16),4,"Wrong result for 16")
  ASSERT_EQ(getFirstGreaterEqual(increasing_i,17),4,"Wrong result for 17")
  ASSERT_EQ(getFirstGreaterEqual(increasing_i,18),5,"Wrong result for 18")

  ASSERT_EQ(getFirstGreaterEqual(increasing_i,31),5,"Wrong result for 31")
  ASSERT_EQ(getFirstGreaterEqual(increasing_i,32),5,"Wrong result for 32")
  ASSERT_EQ(getFirstGreaterEqual(increasing_i,33),10,"Wrong result for 33")

  ASSERT_EQ(getFirstGreaterEqual(increasing_i,87),10,"Wrong result for 87")
  ASSERT_EQ(getFirstGreaterEqual(increasing_i,88),10,"Wrong result for 88")
  ASSERT_EQ(getFirstGreaterEqual(increasing_i,89),11,"Wrong result for 89")

  ASSERT_EQ(getFirstGreaterEqual(increasing_i,90),12,"Wrong result for 90")
  ASSERT_EQ(getFirstGreaterEqual(increasing_i,95),12,"Wrong result for 95")
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
SUBROUTINE testgetFirstGreaterEqual_r()

  ASSERT_EQ(getFirstGreaterEqual(increasing_r,0.0_SRK),1,"Wrong result for 0")

  ASSERT_EQ(getFirstGreaterEqual(increasing_r,1.0_SRK),1,"Wrong result for 1")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,2.0_SRK),2,"Wrong result for 2")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,7.0_SRK),2,"Wrong result for 7")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,8.0_SRK),2,"Wrong result for 8")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,8.05_SRK,0.1_SRK),2,"Wrong result for ~8")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,8.11_SRK,0.1_SRK),3,"Wrong result for ~8")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,9.0_SRK),3,"Wrong result for 9")

  ASSERT_EQ(getFirstGreaterEqual(increasing_r,11.0_SRK),3,"Wrong result for 12")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,12.0_SRK),3,"Wrong result for 13")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,13.0_SRK),4,"Wrong result for 14")

  ASSERT_EQ(getFirstGreaterEqual(increasing_r,16.0_SRK),4,"Wrong result for 16")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,17.0_SRK),4,"Wrong result for 17")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,18.0_SRK),5,"Wrong result for 18")

  ASSERT_EQ(getFirstGreaterEqual(increasing_r,31.0_SRK),5,"Wrong result for 31")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,32.0_SRK),5,"Wrong result for 32")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,33.0_SRK),10,"Wrong result for 33")

  ASSERT_EQ(getFirstGreaterEqual(increasing_r,87.0_SRK),10,"Wrong result for 87")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,88.0_SRK),10,"Wrong result for 88")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,89.0_SRK),11,"Wrong result for 89")

  ASSERT_EQ(getFirstGreaterEqual(increasing_r,90.0_SRK),12,"Wrong result for 90")
  ASSERT_EQ(getFirstGreaterEqual(increasing_r,95.0_SRK),12,"Wrong result for 95")
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
SUBROUTINE testgetFirstGreater_i()
  ASSERT_EQ(getFirstGreater(increasing_i,0),1,"Wrong result for 0")

  ASSERT_EQ(getFirstGreater(increasing_i,1),2,"Wrong result for 1")
  ASSERT_EQ(getFirstGreater(increasing_i,2),2,"Wrong result for 2")
  ASSERT_EQ(getFirstGreater(increasing_i,7),2,"Wrong result for 7")
  ASSERT_EQ(getFirstGreater(increasing_i,8),3,"Wrong result for 8")
  ASSERT_EQ(getFirstGreater(increasing_i,9),3,"Wrong result for 9")

  ASSERT_EQ(getFirstGreater(increasing_i,11),3,"Wrong result for 12")
  ASSERT_EQ(getFirstGreater(increasing_i,12),4,"Wrong result for 13")
  ASSERT_EQ(getFirstGreater(increasing_i,13),4,"Wrong result for 14")

  ASSERT_EQ(getFirstGreater(increasing_i,16),4,"Wrong result for 16")
  ASSERT_EQ(getFirstGreater(increasing_i,17),5,"Wrong result for 17")
  ASSERT_EQ(getFirstGreater(increasing_i,18),5,"Wrong result for 18")

  ASSERT_EQ(getFirstGreater(increasing_i,31),5,"Wrong result for 31")
  ASSERT_EQ(getFirstGreater(increasing_i,32),10,"Wrong result for 32")
  ASSERT_EQ(getFirstGreater(increasing_i,33),10,"Wrong result for 33")

  ASSERT_EQ(getFirstGreater(increasing_i,87),10,"Wrong result for 87")
  ASSERT_EQ(getFirstGreater(increasing_i,88),11,"Wrong result for 88")
  ASSERT_EQ(getFirstGreater(increasing_i,89),12,"Wrong result for 89")

  ASSERT_EQ(getFirstGreater(increasing_i,90),12,"Wrong result for 90")
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
SUBROUTINE testgetFirstGreater_r()
  ASSERT_EQ(getFirstGreater(increasing_r,0.0_SRK),1,"Wrong result for 0")

  ASSERT_EQ(getFirstGreater(increasing_r,1.0_SRK),2,"Wrong result for 1")
  ASSERT_EQ(getFirstGreater(increasing_r,2.0_SRK),2,"Wrong result for 2")
  ASSERT_EQ(getFirstGreater(increasing_r,7.0_SRK),2,"Wrong result for 7")
  ASSERT_EQ(getFirstGreater(increasing_r,8.0_SRK),3,"Wrong result for 8")
  ASSERT_EQ(getFirstGreater(increasing_r,7.89_SRK,0.1_SRK),2,"Wrong result for 8")
  ASSERT_EQ(getFirstGreater(increasing_r,8.05_SRK,0.1_SRK),3,"Wrong result for 8")
  ASSERT_EQ(getFirstGreater(increasing_r,8.11_SRK,0.1_SRK),3,"Wrong result for 8")
  ASSERT_EQ(getFirstGreater(increasing_r,9.0_SRK),3,"Wrong result for 9")

  ASSERT_EQ(getFirstGreater(increasing_r,11.0_SRK),3,"Wrong result for 12")
  ASSERT_EQ(getFirstGreater(increasing_r,12.0_SRK),4,"Wrong result for 13")
  ASSERT_EQ(getFirstGreater(increasing_r,13.0_SRK),4,"Wrong result for 14")

  ASSERT_EQ(getFirstGreater(increasing_r,16.0_SRK),4,"Wrong result for 16")
  ASSERT_EQ(getFirstGreater(increasing_r,17.0_SRK),5,"Wrong result for 17")
  ASSERT_EQ(getFirstGreater(increasing_r,18.0_SRK),5,"Wrong result for 18")

  ASSERT_EQ(getFirstGreater(increasing_r,31.0_SRK),5,"Wrong result for 31")
  ASSERT_EQ(getFirstGreater(increasing_r,32.0_SRK),10,"Wrong result for 32")
  ASSERT_EQ(getFirstGreater(increasing_r,33.0_SRK),10,"Wrong result for 33")

  ASSERT_EQ(getFirstGreater(increasing_r,87.0_SRK),10,"Wrong result for 87")
  ASSERT_EQ(getFirstGreater(increasing_r,88.0_SRK),11,"Wrong result for 88")
  ASSERT_EQ(getFirstGreater(increasing_r,89.0_SRK),12,"Wrong result for 89")

  ASSERT_EQ(getFirstGreater(increasing_r,90.0_SRK),12,"Wrong result for 90")
ENDSUBROUTINE

!
!-------------------------------------------------------------------------------
SUBROUTINE testFind_i()
  LOGICAL(SBK) :: bool
  ASSERT_EQ(find(random_i,91),1,"Wrong result for 91")
  ASSERT_EQ(find(random_i,86),20,"Wrong result for 86")
  ! this is a duplicate,technically the implementation may return either
  bool=(find(random_i,64) == 7 .OR. find(random_i,64) == 13)
  ASSERT(bool,"Wrong result for 64")
  ASSERT_EQ(find(random_i,97),9,"Wrong result for 97")

  ! make sure that failed searches are handled right
  ASSERT_EQ(find(random_i,19),SIZE(random_i)+1,"Wrong nonexistant")
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
SUBROUTINE testFind_r()
  LOGICAL(SBK) :: bool
  ASSERT_EQ(find(random_r,91.0_SRK),1,"Wrong result for 91")
  ASSERT_EQ(find(random_r,86.0_SRK),20,"Wrong result for 86")
  ! this is a duplicate,technically the implementation may return either
  bool=(find(random_r,64.0_SRK) == 7 .OR. find(random_r,64.0_SRK) == 13)
  ASSERT(bool,"Wrong result for 64")
  ASSERT_EQ(find(random_r,97.0_SRK),9,"Wrong result for 97")

  ! make sure that failed searches are handled right
  ASSERT_EQ(find(random_r,19.0_SRK),SIZE(random_r)+1,"Wrong nonexistant")
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
SUBROUTINE testBinary_i()
  LOGICAL(SBK) :: bool
  INTEGER(SIK) :: i

  ASSERT_EQ(binarySearch(increasing_i,1),1,"failed with 1")
  ASSERT_EQ(binarySearch(increasing_i,17),4,"failed with 17")
  i=binarySearch(increasing_i,32)
  bool = i>=5 .AND. i<=9
  ASSERT(bool,"failed with 32")
  ASSERT_EQ(binarySearch(increasing_i,89),11,"failed with 89")
  ASSERT_EQ(binarySearch(increasing_i,13),SIZE(increasing_i)+1,"failed 13")
  ASSERT_EQ(binarySearch(increasing_i,0),SIZE(increasing_i)+1,"failed 0")
  ASSERT_EQ(binarySearch(increasing_i,90),SIZE(increasing_i)+1,"failed 90")

ENDSUBROUTINE testBinary_i
!
!-------------------------------------------------------------------------------
SUBROUTINE testBinary_r()
  LOGICAL(SBK) :: bool
  INTEGER(SIK) :: i

  ASSERT_EQ(binarySearch(increasing_r,1.0_SRK),1,"failed with 1")
  ASSERT_EQ(binarySearch(increasing_r,17.0_SRK),4,"failed with 17")
  ASSERT_EQ(binarySearch(increasing_r,17.05_SRK,0.1_SRK),4,"failed with 17")
  i=binarySearch(increasing_r,32.0_SRK)
  bool = i>=5 .AND. i<=9
  ASSERT(bool,"failed with 32")
  ASSERT_EQ(binarySearch(increasing_r,89.0_SRK),11,"failed with 89")
  ASSERT_EQ(binarySearch(increasing_r,13.0_SRK),SIZE(increasing_r)+1,"failed 13")
  ASSERT_EQ(binarySearch(increasing_r,0.0_SRK),SIZE(increasing_r)+1,"failed 0")
  ASSERT_EQ(binarySearch(increasing_r,90.0_SRK),SIZE(increasing_r)+1,"failed 90")

ENDSUBROUTINE
ENDPROGRAM testSearch
