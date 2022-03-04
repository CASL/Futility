!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testSpeciesElements
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE Strings
USE SpeciesElementsModule
USE ElementsIsotopes

IMPLICIT NONE

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
  REAL(SRK), DIMENSION(1:119) :: testArray

  ! Test H2O
  testWord = 'H2O'
  testArray = SpeciesElements_getElementArray(testWord)
  ASSERT_EQ(testArray(getAtomicNumber('H')),2.0,'H2')
  ASSERT_EQ(testArray(getAtomicNumber('O')),1.0,'O')

  ! Test C2H6O
  testWord = 'C2H6O'
  testArray = SpeciesElements_getElementArray(testWord)
  ASSERT_EQ(testArray(getAtomicNumber('C')),2.0,'C2')
  ASSERT_EQ(testArray(getAtomicNumber('H')),6.0,'H6')
  ASSERT_EQ(testArray(getAtomicNumber('O')),1.0,'O')

  ! Test UF4
  testWord = 'UF4'
  testArray = SpeciesElements_getElementArray(testWord)
  ASSERT_EQ(testArray(getAtomicNumber('U')),1.0,'U')
  ASSERT_EQ(testArray(getAtomicNumber('F')),4.0,'F4')

  ! Test NaCl
  testWord = 'NaCl'
  testArray = SpeciesElements_getElementArray(testWord)
  ASSERT_EQ(testArray(getAtomicNumber('Na')),1.0,'Na')
  ASSERT_EQ(testArray(getAtomicNumber('Cl')),1.0,'Cl')

  ! Test PuO2
  testWord = 'PuO2'
  testArray = SpeciesElements_getElementArray(testWord)
  ASSERT_EQ(testArray(getAtomicNumber('Pu')),1.0,'Pu')
  ASSERT_EQ(testArray(getAtomicNumber('O')),2.0,'O2')

ENDSUBROUTINE testSpeciesElementsBase
!
!-------------------------------------------------------------------------------
ENDPROGRAM testSpeciesElements
