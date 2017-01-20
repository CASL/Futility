!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                              Copyright (C) 2012                              !
!                   The Regents of the University of Michigan                  !
!              MPACT Development Group and Prof. Thomas J. Downar              !
!                             All rights reserved.                             !
!                                                                              !
! Copyright is reserved to the University of Michigan for purposes of          !
! controlled dissemination, commercialization through formal licensing, or     !
! other disposition. The University of Michigan nor any of their employees,    !
! makes any warranty, express or implied, or assumes any liability or          !
! responsibility for the accuracy, completeness, or usefulness of any          !
! information, apparatus, product, or process disclosed, or represents that    !
! its use would not infringe privately owned rights. Reference herein to any   !
! specific commercial products, process, or service by trade name, trademark,  !
! manufacturer, or otherwise, does not necessarily constitute or imply its     !
! endorsement, recommendation, or favoring by the University of Michigan.      !
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

