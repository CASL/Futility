!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module defines the feedback solver type object and methods.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref XSMesh "XSMesh": @copybrief XSMesh
!>
!> @author Ben Collins
!>   @date 11/19/2014
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE MemProf
  USE ISO_C_BINDING
  USE IntrType
  USE Strings
  USE FileType_Log
  USE ExceptionHandler
  USE ParameterLists
  USE ParallelEnv

  IMPLICIT NONE
  PRIVATE

  INCLUDE 'getSysProcInfo_F.h'

  PUBLIC :: Memory_Profiler

  !> This is the type defined as a feedback operation for critical rod search
  TYPE :: Memory_Profiler
    !> Initialization status
    LOGICAL(SBK) :: isInit=.FALSE.
    !> Current memory
    REAL(SRK) :: mem_current=0.0_SRK
    !> Memory last time edit was called
    REAL(SRK) :: mem_old=0.0_SRK
    TYPE(LogFileType),POINTER :: mylog => NULL()
    !> Parallel Environment for the problem
    TYPE(ParallelEnvType),POINTER :: pe => NULL()
!
!List of type-bound procedures
    CONTAINS
      !> @copybrief Memory_Profiler::init_MemProf
      !> @copydetails Memory_Profiler::init_MemProf
      PROCEDURE,PASS :: init => init_MemProf
      !> @copybrief Memory_Profiler::clear_MemProf
      !> @copydetails Memory_Profiler::clear_MemProf
      PROCEDURE,PASS :: clear => clear_MemProf
      !> @copybrief Memory_Profiler::edit_MemProf
      !> @copydetails Memory_Profiler::edit_MemProf
      PROCEDURE,PASS :: edit => edit_MemProf
  ENDTYPE Memory_Profiler

  !> Module name
  CHARACTER(LEN=*),PARAMETER :: modName='MEMORYPROFILER'
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief the procudere to
!> @param thisMP the memory profiler
!> @param params
!>
!TODO  Need to get control rod movement passed in
    SUBROUTINE init_MemProf(thisMP,pe,mylog,params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_MemProf'
      CLASS(Memory_Profiler),INTENT(INOUT) :: thisMP
      TYPE(ParallelEnvType),TARGET,INTENT(IN) :: pe
      TYPE(LogFileType),POINTER,INTENT(INOUT) :: myLog
      TYPE(ParamType),INTENT(IN),OPTIONAL :: params

      INTEGER(C_LONG_LONG) :: tmpL1, tmpL2

      IF(ASSOCIATED(myLog)) THEN
        thisMP%pe=>pe
        thisMP%myLog=>myLog

        CALL getProcMemInfo(tmpL1,tmpL2)
        thisMP%mem_old=REAL(tmpL1,SRK)/(1024.0_SRK*1024.0_SRK)
        thisMP%mem_current=thisMP%mem_old
        thisMP%isInit=.TRUE.
      ENDIF
    ENDSUBROUTINE init_MemProf
!
!-------------------------------------------------------------------------------
!> @brief Default procedure for
!> @param thisMP the memory profiling object
!>
    SUBROUTINE clear_MemProf(thisMP)
      CLASS(Memory_Profiler),INTENT(INOUT) :: thisMP
      thisMP%isInit=.FALSE.
      thisMP%mem_current=0.0_SRK
      thisMP%mem_old=0.0_SRK
      thisMP%pe=>NULL()
      thisMP%myLog=>NULL()
    ENDSUBROUTINE clear_MemProf
!
!-------------------------------------------------------------------------------
!> @brief Default procedure for
!> @param thisMP the memory profiling object
!>
    SUBROUTINE edit_MemProf(thisMP,name)
      CLASS(Memory_Profiler),INTENT(INOUT) :: thisMP
      CHARACTER(LEN=*),INTENT(IN) :: name
#ifdef MPACT_MEMPROF
      CHARACTER(LEN=45)  :: tmpchar
      CHARACTER(LEN=128) :: amesg
      INTEGER(C_LONG_LONG) :: tmpL1, tmpL2
      REAL(SRK) :: mem(1), dmem(1), maxmem(1)

      CALL thisMP%pe%world%barrier()
      CALL getProcMemInfo(tmpL1,tmpL2)
      thisMP%mem_current=REAL(tmpL1,SRK)/(1024.0_SRK*1024.0_SRK)

      mem=thisMP%mem_current
      maxmem=mem
      dmem=thisMP%mem_current-thisMP%mem_old
      CALL thisMP%pe%world%allReduceMax(1,maxmem)
      CALL thisMP%pe%world%allReduceMax(1,dmem)
      CALL thisMP%pe%world%allReduce(1,mem)
      mem=mem/REAL(thisMP%pe%world%nproc,SRK)

      IF(ASSOCIATED(thisMP%myLog) .AND. thisMP%pe%world%master) THEN
        WRITE(tmpchar,'(a)') 'Memory Use at '//TRIM(name)//':'
        WRITE(amesg,'(a,3(f10.3))') ADJUSTL(tmpchar), mem, maxmem, dmem
        CALL thisMP%myLog%message(TRIM(amesg),.FALSE.,.TRUE.)
      ENDIF

      thisMP%mem_old=thisMP%mem_current
#endif
    ENDSUBROUTINE edit_MemProf
!
ENDMODULE MemProf

