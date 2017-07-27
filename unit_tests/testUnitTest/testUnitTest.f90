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
  USE IntrType
  IMPLICIT NONE

#ifdef HAVE_MPI
  INCLUDE 'mpif.h'
  INTEGER :: mpierr
  CALL MPI_Init(mpierr)
#endif

  CREATE_TEST("UnitTest")

  !SET_INTERACTIVE()
  SET_PREFIX("testUTest")

  ! Subtests
  REGISTER_SUBTEST("ASSERT Test",testASSERT)
  REGISTER_SUBTEST("ASSERT_SOFTEQ_SDK Test",testASSERT_SOFTEQ_SDK)
  REGISTER_SUBTEST("ASSERT_SOFTEQ_SSK Test",testASSERT_SOFTEQ_SSK)
  REGISTER_SUBTEST("ASSERT_APPROXEQA_SDK Test",testASSERT_APPROXEQA_SDK)
  REGISTER_SUBTEST("ASSERT_APPROXEQA_SSK Test",testASSERT_APPROXEQA_SSK)
  REGISTER_SUBTEST("ASSERT_APPROXEQF_SDK",testASSERT_APPROXEQF_SDK)
  REGISTER_SUBTEST("ASSERT_APPROXEQF_SSK",testASSERT_APPROXEQF_SSK)
  REGISTER_SUBTEST("ASSERT_APPROXEQ_SDK Test",testASSERT_APPROXEQ_SDK)
  REGISTER_SUBTEST("ASSERT_APPROXEQ_SSK Test",testASSERT_APPROXEQ_SSK)
  REGISTER_SUBTEST("ASSERT_EQ_SDK Test",testASSERT_EQ_SDK)
  REGISTER_SUBTEST("ASSERT_EQ_SSK Test",testASSERT_EQ_SSK)
  REGISTER_SUBTEST("ASSERT_LE_SDK Test",testASSERT_LE_SDK)
  REGISTER_SUBTEST("ASSERT_LE_SSK Test",testASSERT_LE_SSK)
  REGISTER_SUBTEST("ASSERT_LT_SDK Test",testASSERT_LT_SDK)
  REGISTER_SUBTEST("ASSERT_LT_SSK Test",testASSERT_LT_SSK)
  REGISTER_SUBTEST("ASSERT_GE_SDK Test",testASSERT_GE_SDK)
  REGISTER_SUBTEST("ASSERT_GE_SSK Test",testASSERT_GE_SSK)
  REGISTER_SUBTEST("ASSERT_GT_SDK Test",testASSERT_GT_SDK)
  REGISTER_SUBTEST("ASSERT_GT_SSK Test",testASSERT_GT_SSK)
  REGISTER_SUBTEST("ASSERT_APPROXLE_SDK Test",testASSERT_APPROXLE_SDK)
  REGISTER_SUBTEST("ASSERT_APPROXLE_SSK Test",testASSERT_APPROXLE_SSK)
  REGISTER_SUBTEST("ASSERT_APPROXGE_SDK Test",testASSERT_APPROXGE_SDK)
  REGISTER_SUBTEST("ASSERT_APPROXGE_SSK Test",testASSERT_APPROXGE_SSK)
  ASSERT(.TRUE.,"in main")
  ! NEVER DO THIS IN YOUR UNIT TEST
  !    This is testing failures as well as passing tests.  Expect 37 failures.
  utest_nfail=utest_nfail-37
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
!> @brief Tests the ASSERT macro and associated function calls 
!>
    SUBROUTINE testASSERT()
      COMPONENT_TEST("Passing ASSERT")
        ASSERT(.TRUE.,"ASSERT given passing condtion")
        FINFO() "   This is a test that you shouldn't see"
      COMPONENT_TEST("Failing ASSERT")
        ASSERT(.FALSE.,"Assert given failing condition")
        FINFO() "   This is a test that you should see fail"
    ENDSUBROUTINE testASSERT
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_SOFTEQ macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param d - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
!> Supply reference and tolerance values. Construct two test values inside of
!> the range [a-tol,a+tol], and two outside of the range. Esxpect tests named
!> Failing SOFTEQ to fail.
!>
    SUBROUTINE testASSERT_SOFTEQ_SDK()
      REAL(SDK):: a,b,c,d,e,tol
      !Initialize ref value
      a=2.0_SDK
      ! Declare a tolerance to test functionality of the statements
      tol=1.0E-14_SDK
      ! Initialize test values on either side of tolerance
      b=a+0.98E-14_SDK
      c=a+1.0E-14_SDK
      d=a-0.98E-14_SDK
      e=a-1.02E-14_SDK

      COMPONENT_TEST("Passing SOFTEQ")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_SOFTEQ(b,a,tol,'ASSERT_SOFTEQ given passing condition')
      COMPONENT_TEST("Passing SOFTEQ")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_SOFTEQ(d,a,tol,'ASSERT_SOFTEQ given passing condition')
      COMPONENT_TEST("Failing SOFTEQ")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_SOFTEQ(c,a,tol,'ASSERT_SOFTEQ given failing condition')
      COMPONENT_TEST("Failing SOFTEQ")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_SOFTEQ(e,a,tol,'ASSERT_SOFTEQ given failing condition')
    ENDSUBROUTINE testASSERT_SOFTEQ_SDK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_SOFTEQ macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param d - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
!> Supply reference and tolerance values. Construct two test values inside of
!> the range [a-tol,a+tol], and two outside of the range. Esxpect tests named
!> Failing SOFTEQ to fail.
!>
    SUBROUTINE testASSERT_SOFTEQ_SSK()
      REAL(SSK):: a,b,c,d,e,tol
      !Initialize ref value
      a=2.0_SSK
      ! Declare a tolerance to test functionality of the statements
      tol=1.0E-5_SSK
      ! Initialize test values on either side of tolerance
      b=a+0.98E-5_SSK
      c=a+1.0E-5_SSK
      d=a-0.98E-5_SSK
      e=a-1.0E-5_SSK

      COMPONENT_TEST("Passing SOFTEQ")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_SOFTEQ(b,a,tol,'ASSERT_SOFTEQ given passing condition')
      COMPONENT_TEST("Passing SOFTEQ")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_SOFTEQ(d,a,tol,'ASSERT_SOFTEQ given passing condition')
      COMPONENT_TEST("Failing SOFTEQ")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_SOFTEQ(c,a,tol,'ASSERT_SOFTEQ given failing condition')
      COMPONENT_TEST("Failing SOFTEQ")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_SOFTEQ(e,a,tol,'ASSERT_SOFTEQ given failing condition')
    ENDSUBROUTINE testASSERT_SOFTEQ_SSK
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE testASSERT_APPROXEQA_SDK()
      REAL(SDK):: a,b,c,d,e

      a=2.0_SDK
      b=a+0.98E-14_SDK
      c=a+1.0E-14_SDK
      d=a-0.98E-14_SDK
      e=a-1.02E-14_SDK

      COMPONENT_TEST("Passing APPROXEQA")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXEQA(b,a,"ASSERT_APPROXEQA given passing condition")
      COMPONENT_TEST("Passing APPROXEQA")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXEQA(d,a,"ASSERT_APPROXEQA given passing condition")
      COMPONENT_TEST("Failing APPROXEQA")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXEQA(c,a,"ASSERT_APPROXEQA given failing condition")
      COMPONENT_TEST("Failing APPROXEQA")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXEQA(e,a,"ASSERT_APPROXEQA given failing condition")
    ENDSUBROUTINE testASSERT_APPROXEQA_SDK

!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE testASSERT_APPROXEQA_SSK()
      REAL(SSK):: a,b,c,d,e
      a=2.0_SSK
      b=a+0.98E-05_SSK
      c=a+1.00E-05_SSK
      d=a-0.98E-05_SSK
      e=a-1.00E-05_SSK


      COMPONENT_TEST("Passing APPROXEQA")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXEQA(b,a,"ASSERT_APPROXEQA given passing condition")
      COMPONENT_TEST("Passing APPROXEQA")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXEQA(d,a,"ASSERT_APPROXEQA given passing condition")
      COMPONENT_TEST("Failing APPROXEQA")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXEQA(c,a,"ASSERT_APPROXEQA given failing condition")
      COMPONENT_TEST("Failing APPROXEQA")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXEQA(e,a,"ASSERT_APPROXEQA given failing condition")
    ENDSUBROUTINE testASSERT_APPROXEQA_SSK
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE testASSERT_APPROXEQF_SSK()
      REAL(SSK):: a,b,c,d,e
      a=2.0_SSK
      b=a+0.0000025_SSK
      c=a+0.0000026_SSK
      d=a-0.0000012_SSK
      e=a-0.0000013_SSK

      COMPONENT_TEST("Passing _APPROXEQF")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXEQF(b,a,"should PASS")
      COMPONENT_TEST("Passing _APPROXEQF")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXEQF(d,a,"should PASS")
      COMPONENT_TEST("Failing _APPROXEQF")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXEQF(c,a,"should FAIL")
      COMPONENT_TEST("Failing _APPROXEQF")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXEQF(e,a,"should FAIL")
    ENDSUBROUTINE testASSERT_APPROXEQF_SSK
!
!-------------------------------------------------------------------------------
!> @brief description
!> @param parameter    description
!>
!> description
!>
    SUBROUTINE testASSERT_APPROXEQF_SDK()
      REAL(SDK):: a,b,c,d,e
      a=2.0_SDK
      b=a+0.0000000000000046_SDK
      c=a+0.0000000000000047_SDK
      d=a-0.0000000000000023_SDK
      e=a-0.0000000000000024_SDK

      COMPONENT_TEST("Passing _APPROXEQF")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXEQF(b,a,"should PASS")
      COMPONENT_TEST("Passing _APPROXEQF")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXEQF(d,a,"should PASS")
      COMPONENT_TEST("Failing _APPROXEQF")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXEQF(c,a,"should FAIL")
      COMPONENT_TEST("Failing _APPROXEQF")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXEQF(e,a,"should FAIL")
    ENDSUBROUTINE testASSERT_APPROXEQF_SDK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_APPROXEQ macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param d - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
!> Supply reference and tolerance values. Construct two test values inside of
!> the range [a-tol,a+tol], and two outside of the range. Esxpect tests named
!> Failing APPROXEQ to fail.
!>
    SUBROUTINE testASSERT_APPROXEQ_SDK()
      REAL(SDK):: a,b,c,d,e,tol
      !Initialize ref value
      a=2.0_SDK
      ! Declare a tolerance to test functionality of the statements
      tol=1.0E-14_SDK
      ! Initialize test values on either side of tolerance
      b=a+0.98E-14_SDK
      c=a+1.5E-14_SDK
      d=a-0.98E-14_SDK
      e=a-1.02E-14_SDK

      COMPONENT_TEST("Passing APPROXEQ")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXEQ(b,a,"should PASS")
      COMPONENT_TEST("Passing APPROXEQ")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXEQ(d,a,"should PASS")
      COMPONENT_TEST("Failing APPROXEQ")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXEQ(c,a,"should FAIL")
      COMPONENT_TEST("Failing APPROXEQ")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXEQ(e,a,"should FAIL")
    ENDSUBROUTINE testASSERT_APPROXEQ_SDK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_APPROXEQ macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param d - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
!> Supply reference and tolerance values. Construct two test values inside of
!> the range [a-tol,a+tol], and two outside of the range. Esxpect tests named
!> Failing APPROXEQ to fail.
!>
    SUBROUTINE testASSERT_APPROXEQ_SSK()
      REAL(SSK):: a,b,c,d,e

      !Initialize ref value
      a=2.0_SSK
      ! Initialize test values on either side of tolerance
      b=a+0.98E-5_SSK
      c=a+1.5E-5_SSK
      d=a-0.98E-5_SSK
      e=a-1.0E-5_SSK

      COMPONENT_TEST("Passing APPROXEQ")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXEQ(b,a,"should PASS")
      COMPONENT_TEST("Passing APPROXEQ")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXEQ(d,a,"should PASS")
      COMPONENT_TEST("Failing APPROXEQ")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXEQ(c,a,"should FAIL")
      COMPONENT_TEST("Failing APPROXEQ")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXEQ(e,a,"should FAIL")
    ENDSUBROUTINE testASSERT_APPROXEQ_SSK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_EQ macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param d - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
!> Supply reference and tolerance values. Construct two test values inside of
!> the range [a-tol,a+tol], and two outside of the range. Esxpect tests named
!> Failing EQ to fail.
!>
    SUBROUTINE testASSERT_EQ_SDK()
      REAL(SDK):: a,b,c,d,e,tol
      !Initialize ref value
      a=2.0_SDK
      ! Declare a tolerance to test functionality of the statements
      tol=1.0E-14_SDK
      ! Initialize test values on either side of tolerance
      b=a
      c=a+0.5E-15_SDK
      d=a
      e=a-0.2E-15_SDK

      COMPONENT_TEST("Passing EQ")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_EQ(b,a,"should PASS")
      COMPONENT_TEST("Passing EQ")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_EQ(d,a,"should PASS")
      COMPONENT_TEST("Failing EQ")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_EQ(c,a,"should FAIL")
      COMPONENT_TEST("Failing EQ")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_EQ(e,a,"should FAIL")
    ENDSUBROUTINE testASSERT_EQ_SDK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_EQ macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param d - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
!> Supply reference and tolerance values. Construct two test values inside of
!> the range [a-tol,a+tol], and two outside of the range. Esxpect tests named
!> Failing EQ to fail.
!>
    SUBROUTINE testASSERT_EQ_SSK()
      REAL(SSK):: a,b,c,d,e

      !Initialize ref value
      a=2.0_SSK
      ! Initialize test values on either side of tolerance
      b=a
      c=a+1.2E-7_SSK
      d=a
      e=a-0.7E-7_SSK

      COMPONENT_TEST("Passing EQ")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_EQ(b,a,"should PASS")
      COMPONENT_TEST("Passing EQ")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_EQ(d,a,"should PASS")
      COMPONENT_TEST("Failing EQ")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_EQ(c,a,"should FAIL")
      COMPONENT_TEST("Failing EQ")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_EQ(e,a,"should FAIL")
    ENDSUBROUTINE testASSERT_EQ_SSK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_LE macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
    SUBROUTINE testASSERT_LE_SDK()
      REAL(SDK):: a,b,c,e,tol
      !Initialize ref value
      a=2.0_SDK
      ! Declare a tolerance to test functionality of the statements
      tol=1.0E-14_SDK
      ! Initialize test values on either side of tolerance
      b=a
      c=a+0.5E-15_SDK
      e=a-0.2E-15_SDK

      COMPONENT_TEST("Passing LE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_LE(b,a,"should PASS")
      COMPONENT_TEST("Passing LE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_LE(e,a,"should PASS")
      COMPONENT_TEST("Failing LE")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_LE(c,a,"should FAIL")
    ENDSUBROUTINE testASSERT_LE_SDK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_LE macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
    SUBROUTINE testASSERT_LE_SSK()
      REAL(SSK):: a,b,c,e

      !Initialize ref value
      a=2.0_SSK
      ! Initialize test values on either side of tolerance
      b=a
      c=a+1.2E-7_SSK
      e=a-0.7E-7_SSK

      COMPONENT_TEST("Passing LE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_LE(b,a,"should PASS")
      COMPONENT_TEST("Passing LE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_LE(e,a,"should PASS")
      COMPONENT_TEST("Failing LE")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_LE(c,a,"should FAIL")
    ENDSUBROUTINE testASSERT_LE_SSK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_LT macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
    SUBROUTINE testASSERT_LT_SDK()
      REAL(SDK):: a,b,c,e,tol
      !Initialize ref value
      a=2.0_SDK
      ! Declare a tolerance to test functionality of the statements
      tol=1.0E-14_SDK
      ! Initialize test values on either side of tolerance
      b=a
      c=a+0.5E-15_SDK
      e=a-0.2E-15_SDK

      COMPONENT_TEST("Passing LT")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_LT(e,a,"should PASS")
      COMPONENT_TEST("Failing LT")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_LT(c,a,"should FAIL")
      COMPONENT_TEST("Failing LT")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_LT(b,a,"should FAIL")
    ENDSUBROUTINE testASSERT_LT_SDK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_LT macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
    SUBROUTINE testASSERT_LT_SSK()
      REAL(SSK):: a,b,c,e

      !Initialize ref value
      a=2.0_SSK
      ! Initialize test values on either side of tolerance
      b=a
      c=a+1.2E-7_SSK
      e=a-0.7E-7_SSK

      COMPONENT_TEST("Passing LT")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_LT(e,a,"should PASS")
      COMPONENT_TEST("Failing LT")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_LT(c,a,"should FAIL")
      COMPONENT_TEST("Failing LT")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_LT(b,a,"should FAIL")
    ENDSUBROUTINE testASSERT_LT_SSK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_GE macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
    SUBROUTINE testASSERT_GE_SDK()
      REAL(SDK):: a,b,c,e,tol
      !Initialize ref value
      a=2.0_SDK
      ! Declare a tolerance to test functionality of the statements
      tol=1.0E-14_SDK
      ! Initialize test values on either side of tolerance
      b=a
      c=a+0.5E-15_SDK
      e=a-0.2E-15_SDK

      COMPONENT_TEST("Passing GE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_GE(b,a,"should PASS")
      COMPONENT_TEST("Passing GE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_GE(c,a,"should PASS")
      COMPONENT_TEST("Failing GE")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_GE(e,a,"should FAIL")
    ENDSUBROUTINE testASSERT_GE_SDK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_GE macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
    SUBROUTINE testASSERT_GE_SSK()
      REAL(SSK):: a,b,c,e

      !Initialize ref value
      a=2.0_SSK
      ! Initialize test values on either side of tolerance
      b=a
      c=a+1.2E-7_SSK
      e=a-0.7E-7_SSK

      COMPONENT_TEST("Passing GE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_GE(b,a,"should PASS")
      COMPONENT_TEST("Passing GE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_GE(c,a,"should PASS")
      COMPONENT_TEST("Failing GE")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_GE(e,a,"should FAIL")
    ENDSUBROUTINE testASSERT_GE_SSK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_GT macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
    SUBROUTINE testASSERT_GT_SDK()
      REAL(SDK):: a,b,c,e,tol
      !Initialize ref value
      a=2.0_SDK
      ! Declare a tolerance to test functionality of the statements
      tol=1.0E-14_SDK
      ! Initialize test values on either side of tolerance
      b=a
      c=a+0.5E-15_SDK
      e=a-0.2E-15_SDK

      COMPONENT_TEST("Passing GT")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_GT(c,a,"should PASS")
      COMPONENT_TEST("Failing GT")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_GT(b,a,"should FAIL")
      COMPONENT_TEST("Failing GT")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_GT(e,a,"should FAIL")
    ENDSUBROUTINE testASSERT_GT_SDK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_GT macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
    SUBROUTINE testASSERT_GT_SSK()
      REAL(SSK):: a,b,c,e

      !Initialize ref value
      a=2.0_SSK
      ! Initialize test values on either side of tolerance
      b=a
      c=a+1.2E-7_SSK
      e=a-0.7E-7_SSK

      COMPONENT_TEST("Passing GT")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_GT(c,a,"should PASS")
      COMPONENT_TEST("Failing GT")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_GT(b,a,"should FAIL")
      COMPONENT_TEST("Failing GT")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_GT(e,a,"should FAIL")
    ENDSUBROUTINE testASSERT_GT_SSK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_APPROXLE macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param d - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
    SUBROUTINE testASSERT_APPROXLE_SDK()
      REAL(SDK):: a,b,c,d,e,tol
      !Initialize ref value
      a=2.0_SDK
      ! Declare a tolerance to test functionality of the statements
      tol=1.0E-14_SDK
      ! Initialize test values on either side of tolerance
      b=a+0.98E-14_SDK
      c=a+1.5E-14_SDK
      d=a-0.98E-14_SDK
      e=a-1.02E-14_SDK

      COMPONENT_TEST("Passing APPROXLE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXLE(b,a,"should PASS")
      COMPONENT_TEST("Passing APPROXLE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXLE(e,a,"should PASS")
      COMPONENT_TEST("Passing APPROXLE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXLE(d,a,"should PASS")
      COMPONENT_TEST("Failing APPROXLE")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXLE(c,a,"should FAIL")
    ENDSUBROUTINE testASSERT_APPROXLE_SDK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_APPROXLE macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param d - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
    SUBROUTINE testASSERT_APPROXLE_SSK()
      REAL(SSK):: a,b,c,d,e

      !Initialize ref value
      a=2.0_SSK
      ! Initialize test values on either side of tolerance
      b=a+0.98E-5_SSK
      c=a+1.5E-5_SSK
      d=a-0.98E-5_SSK
      e=a-1.0E-5_SSK

      COMPONENT_TEST("Passing APPROXLE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXLE(b,a,"should PASS")
      COMPONENT_TEST("Passing APPROXLE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXLE(e,a,"should PASS")
      COMPONENT_TEST("Passing APPROXLE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXLE(d,a,"should PASS")
      COMPONENT_TEST("Failing APPROXLE")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXLE(c,a,"should FAIL")
    ENDSUBROUTINE testASSERT_APPROXLE_SSK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_APPROXGE macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param d - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
    SUBROUTINE testASSERT_APPROXGE_SDK()
      REAL(SDK):: a,b,c,d,e,tol
      !Initialize ref value
      a=2.0_SDK
      ! Declare a tolerance to test functionality of the statements
      tol=1.0E-14_SDK
      ! Initialize test values on either side of tolerance
      b=a+0.98E-14_SDK
      c=a+1.5E-14_SDK
      d=a-0.98E-14_SDK
      e=a-1.02E-14_SDK

      COMPONENT_TEST("Passing APPROXGE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXGE(b,a,"should PASS")
      COMPONENT_TEST("Passing APPROXGE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXGE(c,a,"should PASS")
      COMPONENT_TEST("Passing APPROXGE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXGE(d,a,"should PASS")
      COMPONENT_TEST("Failing APPROXGE")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXGE(e,a,"should FAIL")
    ENDSUBROUTINE testASSERT_APPROXGE_SDK
!
!-------------------------------------------------------------------------------
!> @brief Tests the ASSERT_APPROXGE macro and associated function calss
!> @param a - reference value
!> @param b - test value
!> @param c - test value
!> @param d - test value
!> @param e - test value
!> @param tol - tolerance for test
!>
    SUBROUTINE testASSERT_APPROXGE_SSK()
      REAL(SSK):: a,b,c,d,e

      !Initialize ref value
      a=2.0_SSK
      ! Initialize test values on either side of tolerance
      b=a+0.98E-5_SSK
      c=a+1.5E-5_SSK
      d=a-0.98E-5_SSK
      e=a-1.0E-5_SSK

      COMPONENT_TEST("Passing APPROXGE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXGE(b,a,"should PASS")
      COMPONENT_TEST("Passing APPROXGE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXGE(c,a,"should PASS")
      COMPONENT_TEST("Passing APPROXGE")
      ! Test maximum delta without a failure, this should not cause output
        ASSERT_APPROXGE(d,a,"should PASS")
      COMPONENT_TEST("Failing APPROXGE")
      ! Test minimum delta to cause a failure, this should cause output
        ASSERT_APPROXGE(e,a,"should FAIL")
    ENDSUBROUTINE testASSERT_APPROXGE_SSK
ENDPROGRAM testUnitTest
