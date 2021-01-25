!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Program to demo basic FMU model interaction
!>
!> To use, Download the example third party FMU from the fmi-cross-check repo on github:
!>   wget https://github.com/modelica/fmi-cross-check/raw/master/fmus/2.0/cs/linux64/MapleSim/2016.2/Rectifier/Rectifier.fmu
!>
!> Extract the FMU with unzip:
!>   unzip -d /path/to/fmu/rectifier_example_fmu Rectifier.fmu
!>
!> Run the example program:
!>   ./Futility_exampleFMU2.exe /path/to/fmu/rectifier_example_fmu
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testFMU2
  USE Strings
  USE IntrType
  USE ParameterLists
  USE FMU_Wrapper
  IMPLICIT NONE

  TYPE(FMU2_Slave) :: test_fmu2_slave
  TYPE(ParamType) :: FMU_params
  CHARACTER(len=256) :: unzipDirectory
  INTEGER(SIK) :: id=3_SIK
  REAL(SRK) :: h=1.0E-6_SRK
  REAL(SRK) :: timeStart=0.0_SRK
  REAL(SRK) :: timeEnd=1.0E-1_SRK
  REAL(SRK) :: tol=1.0E-9_SRK
  REAL(SRK) :: time, voltage1
  INTEGER(SIK) :: i

  ! Example FMU parameter settings
  CALL FMU_params%clear()
  CALL FMU_params%add('FMU_Wrapper->id',id)

  IF (IARGC()==1) THEN
    CALL getarg(1, unzipDirectory)
  ELSE
    unzipDirectory='/home/hephaestus/proj/Futility_dev/Futility/examples/exampleFMU/reference_fmus_2016'
  ENDIF
  CALL FMU_params%add('FMU_Wrapper->unzipDirectory', trim(unzipDirectory))

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING FMU2...'
  WRITE(*,*) '==================================================='
  CALL test_fmu2_slave%init(id, FMU_params)
  CALL test_fmu2_slave%setupExperiment(.TRUE., tol, timeStart, .TRUE., timeEnd)

  ! set named variables/parameters
  CALL test_fmu2_slave%setNamedVariable("minsamplestep", 2.5e-2_SRK)

  ! set restart point
  CALL test_fmu2_slave%setRestart()
  WRITE(*,*) "Set restart"

  DO i=1,10
    CALL test_fmu2_slave%getReal(0, time)
    CALL test_fmu2_slave%getReal(1, voltage1)
    CALL test_fmu2_slave%doStep(h)
    write(*,*) time, voltage1
  ENDDO

  ! get valueReference to variables
  WRITE(*,*) "internalTime", " valueReference: ", test_fmu2_slave%getValueReference("internalTime"), &
      " causality: ", CHAR(test_fmu2_slave%getCausality("internalTime"))
  WRITE(*,*) "outputs", " valueReference: ", test_fmu2_slave%getValueReference("outputs"), &
      " causality: ", CHAR(test_fmu2_slave%getCausality("outputs"))

  ! set restart point
  CALL test_fmu2_slave%setRestart()
  WRITE(*,*) "Set restart"

  DO i=1,10
    CALL test_fmu2_slave%getReal(0, time)
    CALL test_fmu2_slave%getReal(1, voltage1)
    CALL test_fmu2_slave%doStep(h)
    write(*,*) time, voltage1
  ENDDO

  ! rewind to restart
  CALL test_fmu2_slave%rewindToRestart()
  WRITE(*,*) "Rewind to restart"

  DO i=1,10
    CALL test_fmu2_slave%getReal(0, time)
    CALL test_fmu2_slave%getReal(1, voltage1)
    CALL test_fmu2_slave%doStep(h)
    write(*,*) time, voltage1
  ENDDO

  ! get named variables
  CALL test_fmu2_slave%getNamedVariable("internalTime", time)
  WRITE(*,*) "final time: ", time

  ! Clean up
  CALL test_fmu2_slave%clear()
  CALL FMU_params%clear()

ENDPROGRAM testFMU2
