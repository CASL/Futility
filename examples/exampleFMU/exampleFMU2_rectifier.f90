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
!>   ./Futility_exampleFMU2_rectifier.exe /path/to/fmu/rectifier_example_fmu
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testFMU2
  USE Strings
  USE IntrType
  USE ParameterLists
  USE FMU_Wrapper
  IMPLICIT NONE

  TYPE(FMU2_Slave) :: test_fmu2_cs
  TYPE(ParamType) :: FMU_params
  CHARACTER(len=256) :: unzipDirectory
  CHARACTER(len=256) :: goldFile
  INTEGER(SIK) :: id=3_SIK
  REAL(SRK) :: h=1.0E-7_SRK
  REAL(SRK) :: timeStart=0.0_SRK
  REAL(SRK) :: timeEnd=1.0E-1_SRK
  REAL(SRK) :: tol=1.0E-9_SRK
  REAL(SRK) :: time, voltage1
  REAL(SRK) :: write_time=0.0_SRK
  INTEGER(SIK) :: i

  ! Example FMU parameter settings
  CALL FMU_params%clear()
  CALL FMU_params%add('FMU_Wrapper->id',id)

  IF (IARGC()==1) THEN
    CALL getarg(1, unzipDirectory)
    CALL getarg(2, goldFile)
  ELSE
    WRITE(*,*) "WARNING: Attempting to download the FMU from the internet."
    ! Optionally download the third party FMU from the internet.
    CALL SYSTEM("wget -N https://github.com/modelica/fmi-cross-check/raw/master/fmus/2.0/cs/linux64/MapleSim/2016.2/Rectifier/Rectifier.fmu")
    CALL SYSTEM("unzip -o Rectifier.fmu -d Rectifier_unzipDir")
    unzipDirectory = "./Rectifier_unzipDir"
    CALL SYSTEM("wget -N https://github.com/modelica/fmi-cross-check/raw/master/fmus/2.0/cs/linux64/MapleSim/2016.2/Rectifier/Rectifier_ref.csv")
    goldFile = "Rectifier_ref.csv"
    ! Fix the extracted XML file format
    CALL SYSTEM("sed -i 's/ISO-8859-1/UTF-8/g' ./Rectifier_unzipDir/modelDescription.xml")
  ENDIF
  CALL FMU_params%add('FMU_Wrapper->unzipDirectory', trim(unzipDirectory))

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING FMU2...'
  WRITE(*,*) '==================================================='
  ! Init the FMU_Wrapper
  CALL test_fmu2_cs%init(id, FMU_params)
  CALL test_fmu2_cs%setupExperiment(.FALSE., tol, timeStart, .TRUE., timeEnd)

  ! Step the FMU forward
  DO
    CALL test_fmu2_cs%getReal(0, time)
    CALL test_fmu2_cs%getReal(1, voltage1)
    CALL test_fmu2_cs%doStep(h)
    IF(ABS(write_time-0.0002_SRK)<1.0e-8_SRK) THEN
      WRITE(*,*) time, voltage1
      write_time = 0.0_SRK
    ENDIF
    write_time = write_time + h
    IF(time > timeEnd-0.05) EXIT
  ENDDO

  ! get valueReference to variables
  WRITE(*,*) "internalTime", " valueReference: ", test_fmu2_cs%getValueReference("internalTime"), &
      " causality: ", CHAR(test_fmu2_cs%getCausality("internalTime"))
  WRITE(*,*) "outputs", " valueReference: ", test_fmu2_cs%getValueReference("outputs"), &
      " causality: ", CHAR(test_fmu2_cs%getCausality("outputs"))

  ! set restart point
  CALL test_fmu2_cs%setRestart()
  WRITE(*,*) "Set restart"

  DO i=1,10
    CALL test_fmu2_cs%getReal(0, time)
    CALL test_fmu2_cs%getReal(1, voltage1)
    CALL test_fmu2_cs%doStep(h)
    WRITE(*,*) time, voltage1
  ENDDO

  ! rewind to restart
  CALL test_fmu2_cs%rewindToRestart()
  WRITE(*,*) "Rewind to restart"

  DO i=1,10
    CALL test_fmu2_cs%getReal(0, time)
    CALL test_fmu2_cs%getReal(1, voltage1)
    CALL test_fmu2_cs%doStep(h)
    WRITE(*,*) time, voltage1
  ENDDO

  ! get named variables
  CALL test_fmu2_cs%getNamedVariable("internalTime", time)
  WRITE(*,*) "final time: ", time

  ! Clean up
  CALL test_fmu2_cs%clear()
  CALL FMU_params%clear()

ENDPROGRAM testFMU2
