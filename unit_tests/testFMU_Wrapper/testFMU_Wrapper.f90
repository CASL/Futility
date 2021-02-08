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

!> XML derived model description
TYPE(ParamType) :: test_modelDescription
!> Test FMU wrapper
TYPE(FMU2_Slave) :: test_fmu2_slave

CREATE_TEST('Test FMU_Wrapper')

REGISTER_SUBTEST('%parseFMU_XML',testParseFMU_XML)
REGISTER_SUBTEST('%loadFMU',testLoadFMU)
REGISTER_SUBTEST('%stepFMU',testStepFMU)
REGISTER_SUBTEST('%clearFMU',testClearFMU)

FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!Test on parse FMU XML
SUBROUTINE testParseFMU_XML()
  TYPE(StringType) :: tmp_str

  CALL test_modelDescription%initFromXML('testFMU_BouncingBall/modelDescription.xml',.TRUE.)

  ASSERT(test_modelDescription%has("guid"),'%has_guid')
  ASSERT(test_modelDescription%has("valueReference"),'%has_valueReference')
  ASSERT(test_modelDescription%has("modelIdentifier"),'%has_modelIdentifier')
  ASSERT(.NOT. test_modelDescription%has("not_present_dummy_var"),'%not_present_var')

  CALL test_modelDescription%get("guid",tmp_str)
  ASSERT(tmp_str == "123",'%guid')
  CALL test_modelDescription%get("modelIdentifier",tmp_str)
  ASSERT(tmp_str == "testFMUBouncingBall",'%modelIdentifier')
  CALL test_modelDescription%get("HEIGHT->valueReference",tmp_str)
  ASSERT(tmp_str == "0",'%HEIGHT_vr')
  CALL test_modelDescription%get("HEIGHT_SPEED->valueReference",tmp_str)
  ASSERT(tmp_str == "1",'%HEIGHT_SPEED_vr')
  CALL test_modelDescription%get("GRAVITY->valueReference",tmp_str)
  ASSERT(tmp_str == "2",'%GRAVITY_vr')
  CALL test_modelDescription%get("BOUNCE_COF->valueReference",tmp_str)
  ASSERT(tmp_str == "3",'%BOUNCE_COF_vr')

ENDSUBROUTINE testParseFMU_XML
!
!-------------------------------------------------------------------------------
!Test on load FMU shared object library
SUBROUTINE testLoadFMU()
  USE ISO_C_BINDING
  TYPE(ParamType) :: FMU_params
  INTEGER(SIK) :: id=1

  CALL FMU_params%clear()
  CALL FMU_params%add('FMU_Wrapper->id',id)
  CALL FMU_params%add('FMU_Wrapper->unzipDirectory', './testFMU_BouncingBall')

  CALL test_fmu2_slave%init(id, FMU_params)

  ASSERT(test_fmu2_slave%isInit,"%fmu_isinit")
  ASSERT(test_fmu2_slave%FMU_version == 2_SIK,"%fmu_version")

  CALL FMU_params%clear()

ENDSUBROUTINE testLoadFMU
!
!-------------------------------------------------------------------------------
!Test on stepping FMU model forward in time
SUBROUTINE testStepFMU()
  REAL(SRK) :: fmu_ode_tol=1e-7
  REAL(SRK) :: timeStart=0.0_SRK
  REAL(SRK) :: dt=1e-2_SRK, time=0.0_SRK
  REAL(SRK) :: grav_accl=-9.81_SRK, coeffRest=0.5_SRK

  INTEGER(SIK) :: i
  REAL(SRK) :: ballVel, ballHeight
  REAL(SRK) :: expected_ballHeight, expected_ballVel
  TYPE(StringType) :: ballHeight_name, ballVel_name, gravity_name, coeffRest_name

  ! Named FMU Variables
  ballHeight_name="HEIGHT"
  ballVel_name="HEIGHT_SPEED"
  gravity_name="GRAVITY"
  coeffRest_name="BOUNCE_COF"

  ! Setup FMU internal ode solver tolerance and start time
  CALL test_fmu2_slave%setupExperiment(.TRUE., fmu_ode_tol, timeStart, .FALSE., 1.0E20_SRK)

  ! Set model parameters
  CALL test_fmu2_slave%setNamedVariable(CHAR(gravity_name), grav_accl)
  CALL test_fmu2_slave%setNamedVariable(CHAR(coeffRest_name), coeffRest)

  ! Set initial conditions
  CALL test_fmu2_slave%setNamedVariable(CHAR(ballHeight_name), 1.0_SRK)
  CALL test_fmu2_slave%setNamedVariable(CHAR(ballVel_name), 0.0_SRK)

  ! Check variable valueReferences
  ASSERT_EQ(test_fmu2_slave%getValueReference(CHAR(ballHeight_name)),0_SIK,"%ballHeight_valueReference")
  CALL test_fmu2_slave%getReal(0_SIK, ballHeight)
  ASSERT_EQ(ballHeight, 1.0_SRK,"%ballHeight")
  ASSERT_EQ(test_fmu2_slave%getValueReference(CHAR(ballVel_name)),1_SIK,"%ballVel_valueReference")
  CALL test_fmu2_slave%getReal(1_SIK, ballVel)
  ASSERT_EQ(ballVel, 0.0_SRK, "%ballVel")

  expected_ballHeight=1.0_SRK
  expected_ballVel=0.0_SRK
  DO i=1,10
    CALL test_fmu2_slave%doStep(dt)
    expected_ballHeight=expected_ballHeight+dt*expected_ballVel
    time=time+dt
    CALL test_fmu2_slave%getNamedVariable(CHAR(ballHeight_name), ballHeight)
    CALL test_fmu2_slave%getNamedVariable(CHAR(ballVel_name), ballVel)
    expected_ballVel=expected_ballVel+dt*grav_accl
    ASSERT_SOFTEQ(ballHeight,expected_ballHeight,1.0E-8_SRK,"%ballHeight")
    ASSERT_SOFTEQ(ballVel,expected_ballVel,1.0E-8_SRK,"%ballVal")
  ENDDO

ENDSUBROUTINE testStepFMU
!
!-------------------------------------------------------------------------------
!Test on clearing FMU model
SUBROUTINE testClearFMU()
  USE ISO_C_BINDING

  CALL test_fmu2_slave%clear()

  ASSERT(.NOT. test_fmu2_slave%isInit,"%fmu_isinit")

ENDSUBROUTINE testClearFMU

ENDPROGRAM
