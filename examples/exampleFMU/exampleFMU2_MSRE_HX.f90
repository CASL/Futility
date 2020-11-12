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
  CHARACTER(len=256) :: unzipDirectory
  INTEGER(SIK) :: id=3_SIK
  REAL(SRK) :: dt=1.0E-1_SRK
  REAL(SRK) :: time=0.0_SRK
  REAL(SRK) :: timeStart=0.0_SRK
  REAL(SRK) :: timeEnd=1.0E9_SRK
  REAL(SRK) :: tol=1.0E-7_SRK
  INTEGER(SIK) :: i
  ! Storage for in/out vars
  REAL(SRK) :: T_out, P_out, mflow_out
  REAL(SRK) :: T_in_out, mflow_pump
  ! --- Outputs from FMU model ---
  TYPE(StringType) :: T_out_name
  TYPE(StringType) :: P_out_name
  TYPE(StringType) :: mflow_out_name
  ! --- Inputs from CTF ---
  TYPE(StringType) :: T_in_name
  TYPE(StringType) :: P_in_name
  TYPE(StringType) :: mflow_in_name
  TYPE(StringType) :: mflow_pump_name
  TYPE(StringType) :: junk_var_name
  T_out_name="T_out"
  P_out_name="P_out"
  mflow_out_name="mflow_out"
  T_in_name="T_in"
  P_in_name="P_in"
  mflow_in_name="mflow_in"
  mflow_pump_name="mflow_pump"
  junk_var_name="dummy_junk"

  IF (IARGC()==1) THEN
    CALL getarg(1, unzipDirectory)
  ELSE
    unzipDirectory='/home/wll/proj/fmi_transform_ctf/Futility/examples/exampleFMU/transform_fmus/MSRE_HX_v9'
  ENDIF

  ! Example FMU parameter settings
  CALL FMU_params%clear()
  CALL FMU_params%add('FMU_Wrapper->id',id)

  ! MSRE heat exchanger test
  CALL FMU_params%add('FMU_Wrapper->unzipDirectory', trim(unzipDirectory))

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING FMU2...'
  WRITE(*,*) '==================================================='
  CALL test_fmu2_slave%init(id, FMU_params)
  WRITE(*,*) "Done init"
  WRITE(*,*) "guid: ", CHAR(test_fmu2_slave%guid)
  WRITE(*,*) "modelIdentifier: ", CHAR(test_fmu2_slave%modelIdentifier)

  ! initilize "constants" with "variablility" == "fixed", causality="input"??
  CALL test_fmu2_slave%setNamedVariable(T_in_name, 907.0_SRK)  ! K
  CALL test_fmu2_slave%setNamedVariable(mflow_in_name, 170.0_SRK)  ! kg/s
  CALL test_fmu2_slave%setNamedVariable(mflow_pump_name, 170.0_SRK)  ! kg/s
  CALL test_fmu2_slave%setNamedVariable(P_in_name, 101.33E3_SRK)  ! Pa

  CALL test_fmu2_slave%setupExperiment(.TRUE., tol, timeStart, .TRUE., timeEnd)

  WRITE(*,*) "Is T_in valid: ", test_fmu2_slave%isXmlVar(T_in_name)
  WRITE(*,*) "Is P_in valid: ", test_fmu2_slave%isXmlVar(P_in_name)
  WRITE(*,*) "Is dummy var valid: ", test_fmu2_slave%isXmlVar(junk_var_name)

  ! set vars with "variablility" == "discrete", causality="input"??
  !CALL test_fmu2_slave%setNamedVariable(T_in_name, 927.0_SRK)  ! K
  !CALL test_fmu2_slave%setNamedVariable(mflow_in_name, 171.238_SRK)  ! kg/s
  !CALL test_fmu2_slave%setNamedVariable(P_in_name, 101.33E3_SRK)  ! Pa

  ! Initialization loop, let system come to Steady State
  DO i=1,2000
    CALL test_fmu2_slave%doStep(1.0E-2_SRK)
  ENDDO

  ! set restart point
  CALL test_fmu2_slave%setRestart()
  WRITE(*,*) "Set restart"

  WRITE(*,*) "time(s), T_in(K), T_out(K), mflow_out(kg/s), P_out(Pa)"
  DO i=1,3000
    CALL test_fmu2_slave%getNamedVariable(T_out_name, T_out)
    CALL test_fmu2_slave%getNamedVariable(mflow_out_name, mflow_out)
    CALL test_fmu2_slave%getNamedVariable(mflow_pump_name, mflow_pump)
    CALL test_fmu2_slave%getNamedVariable(P_out_name, P_out)
    CALL test_fmu2_slave%getNamedVariable(T_in_name, T_in_out)
    CALL test_fmu2_slave%doStep(dt)
    time=time+dt
    WRITE(*,*) time, T_in_out, T_out, mflow_out, mflow_pump, P_out
  ENDDO

  ! increase inlet T
  CALL test_fmu2_slave%setNamedVariable(T_in_name, 927.0_SRK)  ! K

  DO i=1,3000
    CALL test_fmu2_slave%getNamedVariable(T_out_name, T_out)
    CALL test_fmu2_slave%getNamedVariable(mflow_out_name, mflow_out)
    CALL test_fmu2_slave%getNamedVariable(mflow_pump_name, mflow_pump)
    CALL test_fmu2_slave%getNamedVariable(P_out_name, P_out)
    CALL test_fmu2_slave%getNamedVariable(T_in_name, T_in_out)
    CALL test_fmu2_slave%doStep(dt)
    time=time+dt
    WRITE(*,*) time, T_in_out, T_out, mflow_out, mflow_pump, P_out
  ENDDO

  ! Increase the flow rate, keep T_in the same
  CALL test_fmu2_slave%setNamedVariable(mflow_in_name, 160.0_SRK)  ! kg/s
  CALL test_fmu2_slave%setNamedVariable(mflow_pump_name, 160.0_SRK)  ! kg/s

  dt=1.0E-1_SRK
  DO i=1,2000
    CALL test_fmu2_slave%getNamedVariable(T_out_name, T_out)
    CALL test_fmu2_slave%getNamedVariable(mflow_out_name, mflow_out)
    CALL test_fmu2_slave%getNamedVariable(mflow_pump_name, mflow_pump)
    CALL test_fmu2_slave%getNamedVariable(P_out_name, P_out)
    CALL test_fmu2_slave%getNamedVariable(T_in_name, T_in_out)
    CALL test_fmu2_slave%doStep(dt)
    time=time+dt
    WRITE(*,*) time, T_in_out, T_out, mflow_out, mflow_pump, P_out
  ENDDO
  CALL test_fmu2_slave%setRestart()

  CALL test_fmu2_slave%setNamedVariable(mflow_in_name, 165.0_SRK)  ! kg/s
  CALL test_fmu2_slave%setNamedVariable(mflow_pump_name, 165.0_SRK)  ! kg/s

  DO i=1,2000
    CALL test_fmu2_slave%getNamedVariable(T_out_name, T_out)
    CALL test_fmu2_slave%getNamedVariable(mflow_out_name, mflow_out)
    CALL test_fmu2_slave%getNamedVariable(mflow_pump_name, mflow_pump)
    CALL test_fmu2_slave%getNamedVariable(P_out_name, P_out)
    CALL test_fmu2_slave%getNamedVariable(T_in_name, T_in_out)
    CALL test_fmu2_slave%doStep(dt)
    time=time+dt
    WRITE(*,*) time, T_in_out, T_out, mflow_out, mflow_pump, P_out
  ENDDO

  ! call rewind
  CALL test_fmu2_slave%rewindToRestart()

  ! Clean up
  CALL test_fmu2_slave%clear()
  CALL FMU_params%clear()
ENDPROGRAM testFMU2
