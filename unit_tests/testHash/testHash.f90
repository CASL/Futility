!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testHash
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE IO_Strings
USE HashModule

CREATE_TEST('Hash')

REGISTER_SUBTEST('stringHash',testStringHash)

FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testStringhash()
  INTEGER(SIK) :: i

  ASSERT_EQ(stringHash('',97_SLK,1000000009_SLK),0_SLK,'stringHash')
  ASSERT_EQ(stringHash('',31_SLK,1000000009_SLK),0_SLK,'stringHash')
  ASSERT_EQ(stringHash(' ',97_SLK,1000000009_SLK),1_SLK,'stringHash')
  ASSERT_EQ(stringHash('  ',97_SLK,1000000009_SLK),98_SLK,'stringHash')
  ASSERT_EQ(stringHash('test',97_SLK,1000000009_SLK),78374436_SLK,'stringHash')
  ASSERT_EQ(stringHash('TEST',97_SLK,1000000009_SLK),48864676_SLK,'stringHash')
  ASSERT_EQ(stringHash('a1*% /->9A',97_SLK,1000000009_SLK),140153404_SLK,'stringHash')
  DO i=32,126
    ASSERT_EQ(stringHash(CHAR(i),50_SLK,1000_SLK),i-31,'ASCII value '//str(i))
  ENDDO !i

ENDSUBROUTINE testStringHash
!
ENDPROGRAM testHash