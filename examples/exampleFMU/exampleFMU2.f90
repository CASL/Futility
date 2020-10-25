!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testFMU2

  USE Strings
  USE IntrType
  USE ParameterLists
  USE FMU_Wrapper
  IMPLICIT NONE

  TYPE(FMU2_Slave) :: test_fmu2_slave
  TYPE(ParamType) :: FMU_params
  INTEGER(SIK) :: id=3_SIK
  REAL(SRK) :: h=1.0E-4_SRK
  REAL(SRK) :: timeStart=0.0_SRK
  REAL(SRK) :: timeEnd=1.0E-1_SRK
  REAL(SRK) :: tol=1.0E-7_SRK
  REAL(SRK) :: time, v1
  REAL(SRK) :: minsamplestep
  INTEGER(SIK) :: i
  TYPE(StringType) :: varName

  ! Example FMU parameter settings
  CALL FMU_params%clear()
  CALL FMU_params%add('FMU_Wrapper->id',id)
  CALL FMU_params%add('FMU_Wrapper->modelIdentifier','Rectifier')
  ! 2019 test (broken load state??)
  !CALL FMU_params%add('FMU_Wrapper->guid','{07a328c5-2b9e-d95c-a421-4975e2ade571}')
  !CALL FMU_params%add('FMU_Wrapper->unzipDirectory','/home/hephaestus/proj/Futility_dev/Futility/examples/exampleFMU/reference_fmus_2019')

  ! 2018 test (broken getFMUstate()!)
  ! CALL FMU_params%add('FMU_Wrapper->guid','{355fd1a1-5000-069a-f93c-629026df9008}')
  ! CALL FMU_params%add('FMU_Wrapper->unzipDirectory','/home/hephaestus/proj/Futility_dev/Futility/examples/exampleFMU/reference_fmus')

  ! 2016.2 test
  CALL FMU_params%add('FMU_Wrapper->guid','{2616f0f0-e784-7ed0-2697-de3336af74cf}')
  CALL FMU_params%add('FMU_Wrapper->unzipDirectory','/home/hephaestus/proj/Futility_dev/Futility/examples/exampleFMU/reference_fmus_2016')

  ! In the Rectifier model:
  ! valueReference == 0 : time (s)
  ! valueReference == 1 : Capacitor1 Voltage (Volts)
  ! default startTime == 0.0 (s)
  ! default stopTime == 1.0E-1 (s)
  ! default stepSize == 1.0E-7 (s)

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING FMU2...'
  WRITE(*,*) '==================================================='
  CALL test_fmu2_slave%init(id, FMU_params)
  CALL test_fmu2_slave%setupExperiment(.TRUE., tol, timeStart, .TRUE., timeEnd)

  ! set named variables/parameters
  minsamplestep=2.5e-2_SRK
  varName="minsamplestep"
  CALL test_fmu2_slave%setNamedVariable(varName, minsamplestep)

  ! set restart point
  CALL test_fmu2_slave%setRestart()

  DO i=1,10
    CALL test_fmu2_slave%getReal(0, time)
    CALL test_fmu2_slave%getReal(1, v1)
    CALL test_fmu2_slave%doStep(h)
    write(*,*) time, v1
  ENDDO

  ! get valueReference to variables
  varName = "internalTime"
  WRITE(*,*) test_fmu2_slave%getValueReference(varName)
  WRITE(*,*) CHAR(test_fmu2_slave%getCausality(varName))
  varName = "outputs"
  WRITE(*,*) test_fmu2_slave%getValueReference(varName)
  WRITE(*,*) CHAR(test_fmu2_slave%getCausality(varName))

  ! set restart point
  CALL test_fmu2_slave%setRestart()

  DO i=1,10
    CALL test_fmu2_slave%getReal(0, time)
    CALL test_fmu2_slave%getReal(1, v1)
    CALL test_fmu2_slave%doStep(h)
    write(*,*) time, v1
  ENDDO

  ! rewind to restart
  CALL test_fmu2_slave%rewindToRestart()

  DO i=1,10
    CALL test_fmu2_slave%getReal(0, time)
    CALL test_fmu2_slave%getReal(1, v1)
    CALL test_fmu2_slave%doStep(h)
    write(*,*) time, v1
  ENDDO

  ! get named variables
  varName = "internalTime"
  CALL test_fmu2_slave%getNamedVariable(varName, time)
  WRITE(*,*) "final time: ", time

  ! Clean up
  CALL test_fmu2_slave%clear()
  CALL FMU_params%clear()

ENDPROGRAM testFMU2
