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
!>   wget https://github.com/modelica/fmi-cross-check/raw/master/fmus/2.0/cs/linux64/Test-FMUs/0.0.2/BouncingBall/BouncingBall.fmu
!>
!> Benchmark results are provided by:
!>   wget https://github.com/modelica/fmi-cross-check/raw/master/fmus/2.0/cs/linux64/Test-FMUs/0.0.2/BouncingBall/BouncingBall_ref.csv
!>
!> Extract the FMU with unzip:
!>   unzip -p /path/to/fmu/BouncingBall BouncingBall.fmu
!>
!> Run the example program:
!>   ./Futility_exampleFMU2_bouncing_ball.exe /path/to/fmu/BouncingBall
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testFMU2_BouncingBall
  USE Strings
  USE IntrType
  USE ParameterLists
  USE FMU_Wrapper
  IMPLICIT NONE

  ! FMU CS Wrapper
  TYPE(FMU2_Slave) :: test_fmu2_cs
  ! FMU Initilization parameterlist
  TYPE(ParamType) :: FMU_params
  ! Path to FMU unzip directory
  CHARACTER(len=256) :: unzipDirectory
  ! Path to gold result file to benchmark against
  CHARACTER(len=256) :: goldFile
  ! ID of fmu used for bookeeping
  INTEGER(SIK) :: fmu_id=1_SIK
  ! Time step size
  REAL(SRK) :: dt=1.0E-3_SRK
  ! Start and end of simulation time
  REAL(SRK) :: timeStart=0.0_SRK
  REAL(SRK) :: timeEnd=1.2_SRK
  ! FMU ODE solver tolerance
  REAL(SRK) :: tol=1.0E-5_SRK
  ! Local time storage
  REAL(SRK) :: time=0.0_SRK
  REAL(SRK) :: write_time=0.0_SRK
  ! Variable to store ball height and velocity
  REAL(SRK) :: ball_velocity, ball_height
  TYPE(StringType) :: varName
  INTEGER(SIK) :: i, imax
  ! Temporary variable storage
  REAL(SRK),ALLOCATABLE :: t_calc(:)
  REAL(SRK),ALLOCATABLE :: h_calc(:)
  REAL(SRK),ALLOCATABLE :: v_calc(:)
  REAL(SRK),ALLOCATABLE :: t_gold(:)
  REAL(SRK),ALLOCATABLE :: h_gold(:)
  REAL(SRK),ALLOCATABLE :: v_gold(:)


  ! Example FMU parameter settings
  CALL FMU_params%clear()
  CALL FMU_params%add('FMU_Wrapper->id',fmu_id)

  IF (IARGC()==2) THEN
    CALL getarg(1, unzipDirectory)
    CALL getarg(2, goldFile)
  ELSE
    WRITE(*,*) "WARNING: Attempting to download the FMU from the internet."
    ! Optionally download the third party FMU from the internet.
    CALL SYSTEM("wget -N https://github.com/modelica/fmi-cross-check/raw/master/fmus/2.0/cs/linux64/Test-FMUs/0.0.2/BouncingBall/BouncingBall.fmu")
    CALL SYSTEM("unzip -o BouncingBall.fmu -d BouncingBallFmu_unzipDir")
    unzipDirectory = "./BouncingBallFmu_unzipDir"
    CALL SYSTEM("wget -N https://github.com/modelica/fmi-cross-check/raw/master/fmus/2.0/cs/linux64/Test-FMUs/0.0.2/BouncingBall/BouncingBall_ref.csv")
    goldFile = "BouncingBall_ref.csv"
    ! Fix the extracted XML file format
    CALL SYSTEM("sed -i 's/ISO-8859-1/UTF-8/g' ./BouncingBallFmu_unzipDir/modelDescription.xml")
  ENDIF
  CALL FMU_params%add('FMU_Wrapper->unzipDirectory', trim(unzipDirectory))

  ! Init the FMU_Wrapper
  CALL test_fmu2_cs%init(fmu_id, FMU_params)
  CALL test_fmu2_cs%setupExperiment(.TRUE., tol, timeStart, .TRUE., timeEnd)

  ! Ensure that desired variables are present in the FMU
  varName='g'
  IF(test_fmu2_cs%isXmlVar(CHAR(varName))) &
      WRITE(*,*) "FMU variable: ", CHAR(varName), " has causality: ", &
      CHAR(test_fmu2_cs%getCausality(CHAR(varName)))
  varName='e'
  IF(test_fmu2_cs%isXmlVar(CHAR(varName))) &
      WRITE(*,*) "FMU variable: ", CHAR(varName), " has causality: ", &
      CHAR(test_fmu2_cs%getCausality(CHAR(varName)))
  varName='h'
  IF(test_fmu2_cs%isXmlVar(CHAR(varName))) &
      WRITE(*,*) "FMU variable: ", CHAR(varName), " has causality: ", &
      CHAR(test_fmu2_cs%getCausality(CHAR(varName)))

  ! set gravity acceleration parameter
  !CALL test_fmu2_cs%setNamedVariable('g', -9.81_SRK)
  ! set the coefficient of restitution
  !CALL test_fmu2_cs%setNamedVariable('e', 0.7_SRK)

  ! Open an output file
  OPEN(unit=42, file="exampleFMU2_bouncing_ball_out.csv")

  ! Write output header
  WRITE(*,*) "Writing results to exampleFMU2_bouncing_ball_out.csv"
  WRITE(42,*) "time[s],   height[m],  velocity[m/s]"

  ! Step the CS FMU model forward in time
  imax=0_SIK
  write_time=0.0_SRK
  DO
    ! Get the variable values of interest from the FMU
    CALL test_fmu2_cs%getNamedVariable('v', ball_velocity)
    CALL test_fmu2_cs%getNamedVariable('h', ball_height)
    ! Print the result to stdout
    IF(ABS(write_time-0.01_SRK) < 1.0e-8_SRK .OR. time<=1.0e-16_SRK) THEN
      WRITE(42,*) time, ball_height, ball_velocity
      WRITE(*,*) time, ball_height, ball_velocity
      write_time = 0.0_SRK
      imax = imax + 1
    ENDIF
    time = time + dt
    write_time = write_time + dt
    IF(time >= timeEnd) EXIT
    ! Step the FMU forward
    CALL test_fmu2_cs%doStep(dt)
  ENDDO
  CLOSE(unit=42)

  ! Check result against fmi-cross-check benchmark values
  WRITE(*,*) "Checking results against BouncingBall_ref.csv"
  OPEN(unit=42, file="exampleFMU2_bouncing_ball_out.csv")
  OPEN(unit=43, file=goldFile)
  ALLOCATE(t_calc(imax))
  ALLOCATE(h_calc(imax))
  ALLOCATE(v_calc(imax))
  ALLOCATE(t_gold(imax))
  ALLOCATE(h_gold(imax))
  ALLOCATE(v_gold(imax))
  DO i=0,imax
    ! skip the header
    IF(i==0) THEN
      READ(42,*)
      READ(43,*)
    ELSE
      READ(42,*) t_calc(i), h_calc(i), v_calc(i)
      READ(43,*) t_gold(i), h_gold(i), v_gold(i)
      WRITE(*,*) "Time: ", t_calc(i), "Diff: ", h_calc(i) - h_gold(i)
    ENDIF
  ENDDO
  CLOSE(unit=42)
  CLOSE(unit=43)

  ! Clean up
  CALL test_fmu2_cs%clear()
  CALL FMU_params%clear()

ENDPROGRAM testFMU2_BouncingBall
