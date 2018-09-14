!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Defines the FutilityComputingEnvironment extended type
!>
!> This tyep is a simple struct-like object that bundles together commonly used
!> computing environment-releated objects such as input/output files, log files,
!> parallel environment objects, and others.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FutilityComputingEnvironmentModule
  USE ParallelEnv
  USE ExceptionHandler
  USE FileType_Log
  USE FileType_HDF5
  USE MemProf
  USE Times
  USE FileType_Input
  USE FileType_XML

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: FutilityComputingEnvironment

  TYPE :: FutilityComputingEnvironment
    CLASS(ParallelEnvType),POINTER :: parEnv => NULL()
    CLASS(ExceptionHandlerType),POINTER :: exceptHandler => NULL()
    CLASS(LogFileType),POINTER :: logFile => NULL()
    CLASS(InputFileType),POINTER :: inputFile => NULL()
    CLASS(XMLFileType),POINTER :: xmlFile => NULL()
    CLASS(HDF5FileType),POINTER :: h5outFile => NULL()
    CLASS(Memory_Profiler),POINTER :: memProf => NULL()
    CLASS(TimerType),POINTER :: timers(:) => NULL()
  ENDTYPE FutilityComputingEnvironment
ENDMODULE FutilityComputingEnvironmentModule
