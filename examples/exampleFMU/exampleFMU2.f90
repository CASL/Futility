!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testFMU2

  USE IntrType
  USE ParameterLists
  USE FMU_Wrapper
  IMPLICIT NONE

  TYPE(FMU2_Slave) :: test_fmu2_slave
  TYPE(ParamType) :: FMU_params
  INTEGER(SIK) :: id=3_SIK
  REAL(SRK) :: h=1.0E-7_SRK
  REAL(SRK) :: time, v1
  INTEGER(SIK) :: i

  ! Example FMU parameter settings
  CALL FMU_params%clear()
  CALL FMU_params%add('FMU_Wrapper->id',id)
  CALL FMU_params%add('FMU_Wrapper->guid','{355fd1a1-5000-069a-f93c-629026df9008}')
  CALL FMU_params%add('FMU_Wrapper->unzipDirectory','/home/hephaestus/proj/Futility_dev/Futility/examples/exampleFMU/reference_fmus')
  CALL FMU_params%add('FMU_Wrapper->modelIdentifier','Rectifier')

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
  CALL test_fmu2_slave%setupExperiment(.TRUE., 1.0E-7_SRK, 0.0_SRK, .TRUE., 1.0E-1_SRK)

  DO i=1,10
    CALL test_fmu2_slave%getReal(0, time)
    CALL test_fmu2_slave%getReal(1, v1)
    CALL test_fmu2_slave%doStep(h)
    write(*,*) time, v1
  ENDDO

  ! Clean up
  CALL FMU_params%clear()

ENDPROGRAM testFMU2
