!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testUnitTest
#include "UnitTest.h"
  USE UnitTest
  IMPLICIT NONE

#ifdef HAVE_MPI
  INCLUDE 'mpif.h'
  INTEGER :: mpierr
  CALL MPI_Init(mpierr)
#else
  INTEGER :: MPI_COMM_WORLD=0
#endif

  CREATE_TEST("UnitTest")

  !SET_INTERACTIVE()
  SET_PREFIX("testUTest")

  ! Subtests
  REGISTER_SUBTEST("A",mysubroutineA)

  REGISTER_SUBTEST("B",mysubroutineB)

  ASSERT(.TRUE.,"in main")

  REGISTER_SUBTEST("C",mysubroutineC)

  ! NEVER DO THIS IN YOUR UNIT TEST
  !    This is testing failures as well as passing tests.  Expect 3 failures.
  utest_nfail=utest_nfail-3
  FINALIZE_TEST()

  STAY()
#ifdef HAVE_MPI
  CALL MPI_Finalize(mpierr)
#endif
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE mysubroutineA()
      COMPONENT_TEST("SubA_1")
        ASSERT(.TRUE.,"test 1")
        FINFO() "This is a test that you shouldn't see"
      COMPONENT_TEST("SubA_2")
        ASSERTFAIL(.TRUE.,"test 2")
        ASSERT(.FALSE.,"test 3")
        FINFO() "This is a test that you should see"
    ENDSUBROUTINE mysubroutineA
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE mysubroutineB()
      COMPONENT_TEST("SubB")
        ASSERT(.TRUE.,"test 1")
        ASSERTFAIL(.TRUE.,"test 2")
        ASSERT(.FALSE.,"test 3")
    ENDSUBROUTINE mysubroutineB
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE mysubroutineC()
      COMPONENT_TEST("SubC")
        ASSERT(.TRUE.,"test 1")
        ASSERTFAIL(.TRUE.,"test 2")
        ASSERT(.FALSE.,"test 3")


    ENDSUBROUTINE mysubroutineC
!
ENDPROGRAM testUnitTest

