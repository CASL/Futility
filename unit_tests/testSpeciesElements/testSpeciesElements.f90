!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                   
!                  !
!                             All rights reserved.                     
!!
!                      
!  !
! Futility is a jointly-maintained, open-source project between the
! University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and
! license    !
! can be found in LICENSE.txt in the head directory of this repository.
!   !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testSpeciesElements
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE Strings
  USE SpeciesElements
  USE ElementsIsotopes

  IMPLICIT NONE

  TYPE(SpeciesElementsType) :: mySpec
  TYPE(ElementsIsotopesType) :: myEI

  CREATE_TEST("SpeciesElements")

  REGISTER_SUBTEST('Test Species to Element array',testSpeciesElementsBase)


  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
   SUBROUTINE testSpeciesElementsBase()
      TYPE(StringType) :: testWord
      REAL(SRK), DIMENSION(0:118) :: testArray

      ! Test H2O
      testWord = 'H2O'
      testArray = mySpec%getElementArray(testWord)
      ASSERT_EQ(testArray(myEI%getAtomicNumber('H')),2.0,'')
      ASSERT_EQ(testArray(myEI%getAtomicNumber('O')),1.0,'')

      ! Test C2H6O
      testWord = 'C2H6O'
      testArray = mySpec%getElementArray(testWord)
      ASSERT_EQ(testArray(myEI%getAtomicNumber('C')),2.0,'')
      ASSERT_EQ(testArray(myEI%getAtomicNumber('H')),6.0,'')
      ASSERT_EQ(testArray(myEI%getAtomicNumber('O')),1.0,'')

      ! Test UF4
      testWord = 'UF4'
      testArray = mySpec%getElementArray(testWord)
      ASSERT_EQ(testArray(myEI%getAtomicNumber('U')),1.0,'')
      ASSERT_EQ(testArray(myEI%getAtomicNumber('F')),4.0,'')

      ! Test NaCl
      testWord = 'NaCl'
      testArray = mySpec%getElementArray(testWord)
      ASSERT_EQ(testArray(myEI%getAtomicNumber('Na')),1.0,'')
      ASSERT_EQ(testArray(myEI%getAtomicNumber('Cl')),1.0,'')

      ! Test PuO2
      testWord = 'PuO2'
      testArray = mySpec%getElementArray(testWord)
      ASSERT_EQ(testArray(myEI%getAtomicNumber('Pu')),1.0,'')
      ASSERT_EQ(testArray(myEI%getAtomicNumber('O')),2.0,'')

   ENDSUBROUTINE testSpeciesElementsBase
!
!-------------------------------------------------------------------------------
ENDPROGRAM testSpeciesElements
