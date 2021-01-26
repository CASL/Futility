!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Program to demo basic FMU model interaction.
!> This example downloads a Co-Simulation FMU model of a rectifier circuit from
!> https://github.com/modelica/fmi-cross-check, unpacks the FMU zip archive into
!> a FMU shared object library and an XML model description file, then loads the
!> FMU shared object library and parses the XML model description using the
!> Futility FMU Wrapper. The loaded FMU is stepped forward in time by 1E-7(s)
!> increments until a final time of 0.1(s) is reached.
!> Results are written to file every 2.0E-4(s) to be consistant with
!> the available gold result files provided by the fmi-cross-check repository
!> for this FMU. The results from the Futility FMU Wrapper are compared to the
!> golden standard results. Finally, to test the rewind capability of the FMU
!> Wrapper, a restart point is set at 0.1(s), the solutin is stepped forward by
!> 10 time steps, then the FMU is rewound to the restart point and the same 10
!> time steps are performed again.  The results from the 10 steps before the
!> rewind are compared against the 10 steps following the rewind.
!>
!> For information regarding the Functional Mockup Interface (FMI) and
!> Functional Mockup Units (FMU), see https://fmi-standard.org.
!>
!> By default the program attempts to automatically download the FMU:
!>   ./Futility_exampleFMU2_rectifier.exe
!>
!> For manual execution:
!> Download the example third party FMU from the fmi-cross-check repo on github:
!>   wget https://github.com/modelica/fmi-cross-check/raw/master/fmus/2.0/cs/linux64/MapleSim/2016.2/Rectifier/Rectifier.fmu
!>
!> Benchmark results are provided by:
!>   wget https://github.com/modelica/fmi-cross-check/raw/master/fmus/2.0/cs/linux64/MapleSim/2016.2/Rectifier/Rectifier_ref.csv
!>
!> Extract the FMU with unzip:
!>   unzip Rectifier.fmu -d /path/to/fmu/rectifier_example_fmu
!>
!> Run the example program:
!>   ./Futility_exampleFMU2_rectifier.exe /path/to/fmu/rectifier_example_fmu Recitfier_ref.csv
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
  REAL(SRK) :: timeEnd=1.5E-1_SRK
  REAL(SRK) :: tol=1.0E-9_SRK
  REAL(SRK) :: time, voltage1
  REAL(SRK) :: write_time=0.0_SRK
  REAL(SRK) :: pre_rewind_t(10)
  REAL(SRK) :: pre_rewind_v(10)
  REAL(SRK) :: post_rewind_t(10)
  REAL(SRK) :: post_rewind_v(10)
  INTEGER(SIK) :: i

  ! Example FMU parameter settings
  CALL FMU_params%clear()
  CALL FMU_params%add('FMU_Wrapper->id',id)

  IF (IARGC()==2) THEN
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

  ! Open an output file
  OPEN(unit=42, file="exampleFMU2_rectifier_out.csv")

  ! Write output header
  WRITE(*,*) "Writing results to exampleFMU2_rectifier_out.csv"
  WRITE(42,*) "time[s],   voltage1[v]"

  ! Step the FMU forward
  DO
    CALL test_fmu2_cs%getReal(0, time)
    CALL test_fmu2_cs%getReal(1, voltage1)
    CALL test_fmu2_cs%doStep(h)
    IF(ABS(write_time-0.0002_SRK)<1.0e-8_SRK) THEN
      WRITE(42,*) time, voltage1
      write_time = 0.0_SRK
    ENDIF
    write_time = write_time + h
    IF(time >= timeEnd-0.5E-1_SRK) EXIT
  ENDDO
  CLOSE(unit=42)

  ! get valueReference to variables and causality
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
    pre_rewind_t(i) = time
    pre_rewind_v(i) = voltage1
  ENDDO

  ! rewind to restart
  CALL test_fmu2_cs%rewindToRestart()
  WRITE(*,*) "Rewind to restart"

  DO i=1,10
    CALL test_fmu2_cs%getReal(0, time)
    CALL test_fmu2_cs%getReal(1, voltage1)
    CALL test_fmu2_cs%doStep(h)
    post_rewind_t(i) = time
    post_rewind_v(i) = voltage1
  ENDDO

  WRITE(*,'(A15,A15,A15,A15,A15)') "Pre RW T(s)", "Post RW T(s)", "Pre RW (V)", "Post RW (V)", "Diff"
  DO i=1,10
    WRITE(*,'(F15.7,F15.7,F15.7,F15.7,E15.7)') pre_rewind_t(i), post_rewind_t(i), pre_rewind_v(i), &
      post_rewind_v(i), post_rewind_v(i)-pre_rewind_v(i)
  ENDDO

  ! get named variables
  CALL test_fmu2_cs%getNamedVariable("internalTime", time)
  WRITE(*,*) "final time: ", time

  ! Clean up
  CALL test_fmu2_cs%clear()
  CALL FMU_params%clear()

ENDPROGRAM testFMU2
