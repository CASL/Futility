!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testElementsIsotopes
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE IntrType
  USE ElementsIsotopes

  IMPLICIT NONE

  TYPE(ElementsIsotopesType) :: myEI

  CREATE_TEST("ElementsIsotopes")

  REGISTER_SUBTEST('Initialize',testInit)
  REGISTER_SUBTEST('getZAID',testGetZAID)
  REGISTER_SUBTEST('getIsoName',testGetIsotopeName)
  REGISTER_SUBTEST('getElementName',testGetElementName)
  REGISTER_SUBTEST('getAtomicNumber',testGetAtomicNumber)
  REGISTER_SUBTEST('getMassNumber',testGetMassNumber)
  REGISTER_SUBTEST('isMetastable',testisMetastable)
  REGISTER_SUBTEST('Clear',testClear)


  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInit()

      CALL myEI%init()
      ASSERT(myEI%isInit,'isInit')
    ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testClear()

      CALL myEI%clear()
      ASSERT(.NOT. myEI%isInit,'isInit')
    ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetZAID()
      ASSERT_EQ(myEI%getZAID('H-2'),1002,'H-2')
      ASSERT_EQ(myEI%getZAID(' H-2'),1002,' H-2')
      ASSERT_EQ(myEI%getZAID('AG-110m'),47110,'AG-110m')
      ASSERT_EQ(myEI%getZAID('f-18m'),9018,'f-18m')
      ASSERT_EQ(myEI%getZAID(' f-18m'),9018,' f-18m')
      ASSERT_EQ(myEI%getZAID('B-NAT'),5000,'B-NAT')
    ENDSUBROUTINE testGetZAID
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetIsotopeName()
      ASSERT_EQ(myEI%getIsoName(1002),'H-2','1002')
      ASSERT_EQ(myEI%getIsoName(47710),'AG-710','47710')
      ASSERT_EQ(myEI%getIsoName(5000),'B-NAT','5000')
    ENDSUBROUTINE testGetIsotopeName
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetElementName()
      ASSERT_EQ(myEI%getElementName(1002),'H','1002')
      ASSERT_EQ(myEI%getElementName(47710),'AG','47710')

      ASSERT_EQ(myEI%getElementName(1),'H','1')
      ASSERT_EQ(myEI%getElementName(47),'AG','47')
      
      ASSERT_EQ(myEI%getElementName('U-235'),'U','U-235')
      ASSERT_EQ(myEI%getElementName('xe-135m'),'XE','xe-135m')
    ENDSUBROUTINE testGetElementName
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetAtomicNumber()
      ASSERT_EQ(myEI%getAtomicNumber(1002),1,'1002')
      ASSERT_EQ(myEI%getAtomicNumber(47710),47,'47710')
      
      ASSERT_EQ(myEI%getAtomicNumber('U  '),92,'U')
      ASSERT_EQ(myEI%getAtomicNumber('pu'),94,'pu')
      
      ASSERT_EQ(myEI%getAtomicNumber('U-235'),92,'U-235')
      ASSERT_EQ(myEI%getAtomicNumber('F-18m'),9,'F-18m')
    ENDSUBROUTINE testGetAtomicNumber
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetMassNumber()
      ASSERT_EQ(myEI%getMassNumber(1002),2,'1002')
      ASSERT_EQ(myEI%getMassNumber(47710),710,'47710')

      ASSERT_EQ(myEI%getMassNumber('U-235'),235,'U-235')
      ASSERT_EQ(myEI%getMassNumber('F-18m'),18,'F-18m')
    ENDSUBROUTINE testGetMassNumber
!
!-------------------------------------------------------------------------------
    SUBROUTINE testisMetastable()
      ASSERT(.NOT. myEI%isMetastable('U-235'),'U-235')
      ASSERT(myEI%isMetastable('Am-242m'),'Am-242m')
      ASSERT(myEI%isMetastable('Am-242M'),'Am-242M')
      ASSERT(myEI%isMetastable('f-18m'),'f-18m')
    ENDSUBROUTINE testisMetastable
!
ENDPROGRAM testElementsIsotopes
