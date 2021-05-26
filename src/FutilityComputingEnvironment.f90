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
!> This type is a simple struct-like object that bundles together commonly used
!> computing environment-releated objects such as input/output files, log files,
!> parallel environment objects, and others.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FutilityComputingEnvironmentModule
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE Strings
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
PUBLIC :: addExceptionHandlerSurrogates

!> This type bundles together objects that are commonly needed throughout
!> all parts of the calling code.  It is intended that each package would
!> instantiate only one computing environment object.  The environment
!> had an array of sub-environments that can be set up as well.  This is
!> intended for coupled calculations where one driver code sets up the
!> environments for all codes, then passes off the sub-environments to the
!> other codes.  The class contains methods to manage these sub-environments
!> as well as timers.
TYPE :: FutilityComputingEnvironment
  !> The name of the the computing environment
  TYPE(StringType) :: name
  !> Pointer to the parallel environment
  CLASS(ParallelEnvType),POINTER :: parEnv => NULL()
  !> Pointer to the ExceptionHandler
  CLASS(ExceptionHandlerType),POINTER :: exceptHandler => NULL()
  !> Pointer to the log file
  CLASS(LogFileType),POINTER :: logFile => NULL()
  !> Pointer to the (ASCII) input file
  CLASS(InputFileType),POINTER :: inputFile => NULL()
  !> Pointer to the XML file
  CLASS(XMLFileType),POINTER :: xmlFile => NULL()
  !> Pointer to the HDF5 output file
  CLASS(HDF5FileType),POINTER :: h5outFile => NULL()
  !> Pointer to the memory profiler object
  CLASS(Memory_Profiler),POINTER :: memProf => NULL()
  !> Number of timers own by the computing environment
  INTEGER(SIK) :: nTimers=0
  !> Pointer to the array of timers
  TYPE(TimerPtrArray),ALLOCATABLE :: timers(:)
  !> Number of sub-environments owned by the computing environment
  INTEGER(SIK) :: nSubCompEnvs=0
  !> Pointer to the array of sub-environments
  TYPE(FutilityComputingEnvironment),POINTER,PRIVATE :: subCompEnvs(:) => NULL()
  CONTAINS
    !> @copybrief FutilityComputingEnvironment::clear
    !> @copydetails FutilityComputingEnvironment::clear
    PROCEDURE,PASS :: clear
    !> @copybrief FutilityComputingEnvironment::addTimer
    !> @copydetails FutilityComputingEnvironment::addTimer
    PROCEDURE,PASS :: addTimer
    !> @copybrief FutilityComputingEnvironment::removeTimer
    !> @copydetails FutilityComputingEnvironment::removeTimer
    PROCEDURE,PASS :: removeTimer
    !> @copybrief FutilityComputingEnvironment::getTimer
    !> @copydetails FutilityComputingEnvironment::getTimer
    PROCEDURE,PASS :: getTimer
    !> @copybrief FutilityComputingEnvironment::clearTimers
    !> @copydetails FutilityComputingEnvironment::clearTimers
    PROCEDURE,PASS :: clearTimers
    !> @copybrief FutilityComputingEnvironment::addSubCompEnv
    !> @copydetails FutilityComputingEnvironment::addSubCompEnv
    PROCEDURE,PASS :: addSubCompEnv
    !> @copybrief FutilityComputingEnvironment::removeSubCompEnv
    !> @copydetails FutilityComputingEnvironment::removeSubCompEnv
    PROCEDURE,PASS :: removeSubCompEnv
    !> @copybrief FutilityComputingEnvironment::getSubCompEnv
    !> @copydetails FutilityComputingEnvironment::getSubCompEnv
    PROCEDURE,PASS :: getSubCompEnv
    !> @copybrief FutilityComputingEnvironment::clearSubCompEnvs
    !> @copydetails FutilityComputingEnvironment::clearSubCompEnvs
    PROCEDURE,PASS :: clearSubCompEnvs
ENDTYPE FutilityComputingEnvironment

!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Clears all data on @c this
!> @param this the FutilityComputingEnvironment object to operate on
!>
!> Deallocates all sub-environments and timers, but only nullifies pointers to
!> other objects.
!>
SUBROUTINE clear(this)
  CLASS(FutilityComputingEnvironment),INTENT(INOUT) :: this

  !Eventually we probably want this to have ownership of these
  !objects too and clear/deallocate them all, but for now just
  !nullify the pointers
  this%name=''
  this%parEnv => NULL()
  this%exceptHandler => NULL()
  this%logFile => NULL()
  this%inputFile => NULL()
  this%xmlFile => NULL()
  this%h5outFile => NULL()
  this%memProf => NULL()
  CALL this%clearTimers()
  CALL this%clearSubCompEnvs()

ENDSUBROUTINE clear
!
!-------------------------------------------------------------------------------
!> @brief Adds a timer to @c this
!> @param this the FutilityComputingEnvironment object to operate on
!> @param name the name of the timer to add
!> @returns timer the timer object that was added
!>
FUNCTION addTimer(this,name) RESULT(timer)
  CLASS(FutilityComputingEnvironment),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  CLASS(TimerType),POINTER :: timer
  !
  INTEGER(SIK) :: iTimer
  TYPE(TimerPtrArray),ALLOCATABLE :: oldTimers(:)

  REQUIRE(LEN_TRIM(name) > 0)

  !Check to see if a timer of this name already exists.  If so, return it
  timer => this%getTimer(name)

  !If no timer was found, create it and return a pointer
  IF(.NOT.ASSOCIATED(timer)) THEN
    !Save the old timers
    ALLOCATE(oldTimers(this%nTimers))
    DO iTimer=1,this%nTimers
      oldTimers(iTimer)%t => this%timers(iTimer)%t
    ENDDO !iTimer
    !Resize the timers array
    IF(ALLOCATED(this%timers)) DEALLOCATE(this%timers)
    ALLOCATE(this%timers(this%nTimers+1))
    IF(this%nTimers > 0) THEN
      DO iTimer=1,this%nTimers
        this%timers(iTimer)%t => oldTimers(iTimer)%t
      ENDDO !iTimer
      DEALLOCATE(oldTimers)
    ENDIF

    !Now add the new timer
    this%nTimers=this%nTimers+1
    ALLOCATE(this%timers(this%nTimers)%t)
    CALL this%timers(this%nTimers)%t%setTimerHiResMode(.TRUE.)
    CALL this%timers(this%nTimers)%t%setTimerName(TRIM(name))
    timer => this%timers(this%nTimers)%t
  ENDIF

  ENSURE(ASSOCIATED(timer))

ENDFUNCTION addTimer
!
!-------------------------------------------------------------------------------
!> @brief Removes a timer from @c this
!> @param this the FutilityComputingEnvironment object to operate on
!> @param name the name of the timer to remove
!>
!> If a timer with name @c name is not found, nothing is done.  If the timer is
!> found, it is deallocated.  The client is responsible for ensuring that any
!> dangling pointers are managed properly.
!>
SUBROUTINE removeTimer(this,name)
  CLASS(FutilityComputingEnvironment),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  !
  INTEGER(SIK) :: iTimer
  CLASS(TimerType),POINTER :: timer
  TYPE(TimerPtrArray),ALLOCATABLE :: oldTimers(:)

  timer => this%getTimer(name)

  IF(ASSOCIATED(timer)) THEN
    !Save the old timers
    ALLOCATE(oldTimers(this%nTimers))
    DO iTimer=1,this%nTimers
      oldTimers(iTimer)%t => this%timers(iTimer)%t
    ENDDO !iTimer
    !Resize timers
    IF(ALLOCATED(this%timers)) DEALLOCATE(this%timers)
    ALLOCATE(this%timers(this%nTimers-1))
    this%nTimers=0
    DO iTimer=1,SIZE(oldTimers)
      IF(TRIM(name) == oldTimers(iTimer)%t%getTimerName()) THEN
        DEALLOCATE(oldTimers(iTimer)%t)
      ELSE
        this%nTimers=this%nTimers+1
        this%timers(this%nTimers)%t => oldTimers(iTimer)%t
      ENDIF
    ENDDO !iTimer
    DEALLOCATE(oldTimers)
  ENDIF

ENDSUBROUTINE removeTimer
!
!-------------------------------------------------------------------------------
!> @brief Retrieves a pointer to a timer on @c this
!> @param this the FutilityComputingEnvironment object to operate on
!> @param name the name of the timer to retrieve
!> @returns timer pointer to the timer being retrieved
!>
!> If no timer with name @c name is found, @c timer is nullified.
!>
FUNCTION getTimer(this,name) RESULT(timer)
  CLASS(FutilityComputingEnvironment),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  CLASS(TimerType),POINTER :: timer
  !
  INTEGER(SIK) :: iTimer

  timer => NULL()
  DO iTimer=1,this%nTimers
    IF(TRIM(name) == this%timers(iTimer)%t%getTimerName()) THEN
      timer => this%timers(iTimer)%t
      EXIT
    ENDIF
  ENDDO !iTimer

ENDFUNCTION getTimer
!
!-------------------------------------------------------------------------------
!> @brief Clears and removes all timers from @c this
!> @param this the FutilityComputingEnvironment object to operate on
!>
SUBROUTINE clearTimers(this)
  CLASS(FutilityComputingEnvironment),INTENT(INOUT) :: this
  !
  INTEGER(SIK) :: iTimer

  IF(ALLOCATED(this%timers)) THEN
    DO iTimer=1,this%nTimers
      CALL this%timers(iTimer)%t%resetTimer()
      DEALLOCATE(this%timers(iTimer)%t)
    ENDDO !iTimer
    DEALLOCATE(this%timers)
  ENDIF
  this%nTimers=0

ENDSUBROUTINE clearTimers
!
!-------------------------------------------------------------------------------
!> @brief Adds a sub-environment to @c this
!> @param this the FutilityComputingEnvironment object to operate on
!> @param name the name of the sub-environment to add
!> @retruns compEnv the environment object that was added
!>
FUNCTION addSubCompEnv(this,name) RESULT(compEnv)
  CLASS(FutilityComputingEnvironment),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(FutilityComputingEnvironment),POINTER :: compEnv
  !
  INTEGER(SIK) :: iEnv
  TYPE(FutilityComputingEnvironment),POINTER :: oldSubCompEnvs(:)

  REQUIRE(LEN_TRIM(name) > 0)

  !Check to see if a subCompEnv of this name already exists.  If so, return it
  compEnv => this%getSubCompEnv(name)

  !If no compEnv was found, create it and return a pointer
  IF(.NOT.ASSOCIATED(compEnv)) THEN
    !Resize subCompEnvs array and copy pre-existing subCompEnvs into new array
    oldSubCompEnvs => this%subCompEnvs
    ALLOCATE(this%subCompEnvs(this%nSubCompEnvs+1))
    IF(this%nSubCompEnvs > 0) THEN
      DO iEnv=1,this%nSubCompEnvs
        this%subCompEnvs(iEnv)=oldSubCompEnvs(iEnv)
        CALL oldSubCompEnvs(iEnv)%clear()
      ENDDO !iEnv
      DEALLOCATE(oldSubCompEnvs)
      oldSubCompEnvs => NULL()
    ENDIF

    !Now add the new compEnv
    this%nSubCompEnvs=this%nSubCompEnvs+1
    this%subCompEnvs(this%nSubCompEnvs)%name=TRIM(name)
    compEnv => this%subCompEnvs(this%nSubCompEnvs)
  ENDIF

  ENSURE(ASSOCIATED(compEnv))

ENDFUNCTION addSubCompEnv
!
!-------------------------------------------------------------------------------
!> @brief Removes a sub-environment from @c this
!> @param this the FutilityComputingEnvironment object to operate on
!> @param name the name of the sub-environment to remove
!>
!> If a sub-environment with name @c name is not found, nothing is done.
!>
SUBROUTINE removeSubCompEnv(this,name)
  CLASS(FutilityComputingEnvironment),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  !
  INTEGER(SIK) :: iEnv
  TYPE(FutilityComputingEnvironment),POINTER :: compEnv
  TYPE(FutilityComputingEnvironment),POINTER :: oldSubCompEnvs(:)

  compEnv => this%getSubCompEnv(name)

  IF(ASSOCIATED(compEnv)) THEN
    oldSubCompEnvs => this%subCompEnvs
    ALLOCATE(this%subCompEnvs(this%nSubCompEnvs-1))
    this%nSubCompEnvs=0
    DO iEnv=1,SIZE(oldSubCompEnvs)
      IF(TRIM(name) == oldSubCompEnvs(iEnv)%name) THEN
        CONTINUE
      ELSE
        this%nSubCompEnvs=this%nSubCompEnvs+1
        this%subCompEnvs(this%nSubCompEnvs)=oldSubCompEnvs(iEnv)
      ENDIF
      CALL oldSubCompEnvs(iEnv)%clear()
    ENDDO !iEnv
    DEALLOCATE(oldSubCompEnvs)
  ENDIF

ENDSUBROUTINE removeSubCompEnv
!
!-------------------------------------------------------------------------------
!> @brief Retrieves a pointer to a sub-environment on @c this
!> @param this the FutilityComputingEnvironment object to operate on
!> @param name the name of the sub-environment to retrieve
!> @returns compEnv pointer to the sub-environment being retrieved
!>
!> If no sub-environment with name @c name is found, @c compEnv is nullified.
!>
FUNCTION getSubCompEnv(this,name) RESULT(compEnv)
  CLASS(FutilityComputingEnvironment),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  TYPE(FutilityComputingEnvironment),POINTER :: compEnv
  !
  INTEGER(SIK) :: iEnv

  compEnv => NULL()
  DO iEnv=1,this%nSubCompEnvs
    IF(TRIM(name) == CHAR(this%subCompEnvs(iEnv)%name)) THEN
      compEnv => this%subCompEnvs(iEnv)
      EXIT
    ENDIF
  ENDDO !iEnv

ENDFUNCTION getSubCompEnv
!
!-------------------------------------------------------------------------------
!> @brief Clears and removes all sub-environments from @c this
!> @param this the FutilityComputingEnvironment object to operate on
!>
RECURSIVE SUBROUTINE clearSubCompEnvs(this)
  CLASS(FutilityComputingEnvironment),INTENT(INOUT) :: this
  !
  INTEGER(SIK) :: iEnv

  IF(ASSOCIATED(this%subCompEnvs)) THEN
    DO iEnv=1,this%nSubCompEnvs
      CALL this%subCompEnvs(iEnv)%clear()
    ENDDO !iEnv
    DEALLOCATE(this%subCompEnvs)
  ENDIF
  this%subCompEnvs => NULL()
  this%nSubCompEnvs=0

ENDSUBROUTINE clearSubCompEnvs
!
!-------------------------------------------------------------------------------
!> @brief Adds exception handler as surrogate to all lower level Futility handlers
!> @param ce the exception handler to use as a surrogate
!>
SUBROUTINE addExceptionHandlerSurrogates(ce)
  USE Allocs, ONLY : eAllocs
  USE EigenvalueSolverTypes, ONLY : eEigenvalueSolverType
  USE ElementsIsotopes, ONLY : eElementsIsotopes
  USE ExpTables, ONLY : eExpTable
  USE FMU_Wrapper, ONLY : eFMU_Wrapper
  USE WaterSatProperties, ONLY : eWaterProp
  USE LinearSolverTypes, ONLY : eLinearSolverType
  USE MatrixTypes, ONLY : eMatrixType
  USE ODESolverTypes, ONLY : eODESolverType
  USE ParameterLists, ONLY : eParams
  USE PartitionGraph, ONLY : ePartitionGraph
  USE PreconditionerTypes, ONLY : ePreCondType
  USE SchemaParserModule, ONLY : eSchemaParser
  USE StochasticSampling, ONLY : eStochasticSampler
  USE VectorTypes, ONLY : eVectorType
  USE VTKFiles, ONLY : eVTK
  USE XDMFMesh, ONLY : eXDMF
  USE MeshTransfer, ONLY : eMeshTransfer
  TYPE(ExceptionHandlerType),TARGET,INTENT(IN) :: ce

  CALL eMeshTransfer%addSurrogate(ce)
  CALL eXDMF%addSurrogate(ce)
  CALL eVTK%addSurrogate(ce)
  CALL eVectorType%addSurrogate(ce)
  CALL eStochasticSampler%addSurrogate(ce)
  CALL eSchemaParser%addSurrogate(ce)
  CALL ePreCondType%addSurrogate(ce)
  CALL ePartitionGraph%addSurrogate(ce)
  CALL eParams%addSurrogate(ce)
  CALL eParEnv%addSurrogate(ce)
  CALL eODESolverType%addSurrogate(ce)
  CALL eMatrixType%addSurrogate(ce)
  CALL eLinearSolverType%addSurrogate(ce)
  CALL eWaterProp%addSurrogate(ce)
  CALL eFMU_Wrapper%addSurrogate(ce)
  CALL eExpTable%addSurrogate(ce)
  CALL eElementsIsotopes%addSurrogate(ce)
  CALL eEigenvalueSolverType%addSurrogate(ce)
  CALL eAllocs%addSurrogate(ce)

ENDSUBROUTINE addExceptionHandlerSurrogates
!
ENDMODULE FutilityComputingEnvironmentModule