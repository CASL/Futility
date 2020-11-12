!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testFMU_Wrapper
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE Strings
USE FileType_XML
USE ParameterLists
USE FMU_Wrapper
IMPLICIT NONE

!> FMU XML model description
TYPE(XMLFileType) :: testFMUXMLFile
!> XML derived model description
TYPE(ParamType) :: test_modelDescription

CREATE_TEST('Test FMU_Wrapper')

REGISTER_SUBTEST('%parseFMU_XML',testParseFMU_XML)
REGISTER_SUBTEST('%loadFMU',testLoadFMU)
REGISTER_SUBTEST('%stepFMU',testStepFMU)

FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!Test get on parse FMU XML
SUBROUTINE testParseFMU_XML()
  TYPE(StringType) :: tmp_str

  WRITE(*,*) "Calling testParseFMU_XML"
  CALL test_modelDescription%initFromXML('testFMU_modelDescription.xml',.TRUE.)

  ASSERT(test_modelDescription%has("guid"),'%has_guid')
  ASSERT(test_modelDescription%has("valueReference"),'%has_valueReference')
  ASSERT(test_modelDescription%has("modelIdentifier"),'%has_modelIdentifier')
  ASSERT(.NOT. test_modelDescription%has("not_present_dummy_var"),'%not_present_var')

  CALL test_modelDescription%get("guid",tmp_str)
  ASSERT(tmp_str=="123",'%guid')
  CALL test_modelDescription%get("modelIdentifier",tmp_str)
  ASSERT(tmp_str=="BouncingBall2",'%modelIdentifier')
  CALL test_modelDescription%get("HEIGHT->valueReference",tmp_str)
  ASSERT(tmp_str=="0",'%HEIGHT_vr')
  CALL test_modelDescription%get("HEIGHT_SPEED->valueReference",tmp_str)
  ASSERT(tmp_str=="1",'%HEIGHT_SPEED_vr')
  CALL test_modelDescription%get("GRAVITY->valueReference",tmp_str)
  ASSERT(tmp_str=="2",'%GRAVITY_vr')
  CALL test_modelDescription%get("BOUNCE_COF->valueReference",tmp_str)
  ASSERT(tmp_str=="3",'%BOUNCE_COF_vr')

ENDSUBROUTINE testParseFMU_XML
!
!-------------------------------------------------------------------------------
!Test get on load FMU share object library
SUBROUTINE testLoadFMU()

ENDSUBROUTINE testLoadFMU
!
!-------------------------------------------------------------------------------
!Test get on stepping FMU model forward in time
SUBROUTINE testStepFMU()

ENDSUBROUTINE testStepFMU

ENDPROGRAM
