!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module defines the feedback solver type object and methods.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE MemProf
USE ISO_C_BINDING
USE IntrType
USE Strings
USE FileType_Fortran
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
  !> Memory threshold
  REAL(SRK) :: mem_threshold=0.0_SRK
  !> logical to enable and disabled verbose edits
  LOGICAL(SBK) :: verbose=.FALSE.
  !> Verbose file to output to
  TYPE(FortranFileType) :: verbose_output
  !> logical to enable and disabled edits
  LOGICAL(SBK) :: mem_edit=.TRUE.
  !> Logical to print banner explaining what data is available
  LOGICAL(SBK) :: print_banner=.TRUE.
  !> log file
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
    !> @copybrief Memory_Profiler::enableEdits_MemProf
    !> @copydetails Memory_Profiler::enableEdits_MemProf
    PROCEDURE,PASS :: enableEdits => enableEdits_MemProf
    !> @copybrief Memory_Profiler::disableEdits_MemProf
    !> @copydetails Memory_Profiler::disableEdits_MemProf
    PROCEDURE,PASS :: disableEdits => disableEdits_MemProf
ENDTYPE Memory_Profiler

!> Module name
CHARACTER(LEN=*),PARAMETER :: modName='MEMORYPROFILER'
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief the procedure to intialize a memory profiler object
!> @param thisMP the memory profiler
!> @param pe the parallel environment
!> @param myLog the log file; optional
!> @param params the input parameter list; optional
!>
SUBROUTINE init_MemProf(thisMP,pe,mylog,params)
  CLASS(Memory_Profiler),INTENT(INOUT) :: thisMP
  TYPE(ParallelEnvType),TARGET,INTENT(IN) :: pe
  TYPE(LogFileType),POINTER,INTENT(INOUT) :: myLog
  TYPE(ParamType),INTENT(IN),OPTIONAL :: params

  INTEGER(C_LONG_LONG) :: tmpL1, tmpL2
  CHARACTER(LEN=16) :: filename

  thisMP%pe=>pe
  IF(ASSOCIATED(myLog)) THEN
    thisMP%myLog=>myLog

    CALL getProcMemInfo(tmpL1,tmpL2)
    thisMP%mem_old=REAL(tmpL1,SRK)/(1024.0_SRK*1024.0_SRK)
    thisMP%mem_current=thisMP%mem_old
    thisMP%isInit=.TRUE.
  ENDIF
  thisMP%print_banner=.TRUE.
  IF(PRESENT(params)) THEN
    IF(params%has('threshold')) CALL params%get('threshold',thisMP%mem_threshold)
    IF(params%has('verbose')) CALL params%get('verbose',thisMP%verbose)
  ENDIF

  IF(thisMP%verbose) THEN
    WRITE(filename,'("memprof.",I0.4,".dat")') pe%world%rank
    CALL thisMP%verbose_output%initialize(FILE=filename,STATUS='UNKNOWN')
    CALL thisMP%verbose_output%fopen()
    WRITE(thisMP%verbose_output%getUnitNo(),'(46x,"Used [GB] Change [MB]")')
    FLUSH(thisMP%verbose_output%getUnitNo())
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
  IF(thisMP%verbose) THEN
    CALL thisMP%verbose_output%fclose()
    CALL thisMP%verbose_output%clear()
  ENDIF
  thisMP%verbose=.FALSE.
ENDSUBROUTINE clear_MemProf
!
!-------------------------------------------------------------------------------
!> @brief Default procedure for
!> @param thisMP the memory profiling object
!>
SUBROUTINE edit_MemProf(thisMP,name)
  CLASS(Memory_Profiler),INTENT(INOUT) :: thisMP
  CHARACTER(LEN=*),INTENT(IN) :: name
#ifdef FUTILITY_MEMPROF
  CHARACTER(LEN=45)  :: tmpchar
  CHARACTER(LEN=128) :: amesg
  INTEGER(C_LONG_LONG) :: tmpL1, tmpL2
  INTEGER(SLK) :: loc(1)
  REAL(SRK) :: mem(1), dmem(1), maxmem(1)

  IF(thisMP%mem_edit) THEN
    IF(thisMP%print_banner) THEN
      IF(thisMP%pe%world%master) THEN
        WRITE(amesg, '(47x,"Avg [GB]  Max [GB]  Max Change [MB] (rank)")')
        CALL thisMP%myLog%message(TRIM(amesg),.FALSE.,.TRUE.)
      ENDIF
      thisMP%print_banner=.FALSE.
    ENDIF
    CALL thisMP%pe%world%barrier()
    CALL getProcMemInfo(tmpL1,tmpL2) !tmpL1 in kB, and tmpL2 in bytes
    thisMP%mem_current=REAL(tmpL1,SRK)*1000.0_SRK/(1024.0_SRK*1024.0_SRK*1024.0_SRK)
    loc(1)=thisMP%pe%world%rank

    mem=thisMP%mem_current
    maxmem=REAL(tmpL2,SRK)/(1024.0_SRK*1024.0_SRK*1024.0_SRK)
    dmem=(thisMP%mem_current-thisMP%mem_old)*1024.0_SRK
    IF(thisMP%verbose) THEN
      WRITE(tmpchar,'(a)') 'Memory Use at '//TRIM(name)//':'
      WRITE(thisMP%verbose_output%getUnitNo(),'(a,2(f10.3))') &
          ADJUSTL(tmpchar), mem, dmem
      FLUSH(thisMP%verbose_output%getUnitNo())
    ENDIF

    CALL thisMP%pe%world%allReduceMax(1,maxmem)
    CALL thisMP%pe%world%ReduceMaxLoc(1,dmem,loc)
    CALL thisMP%pe%world%allReduce(1,mem)
    mem=mem/REAL(thisMP%pe%world%nproc,SRK)

    IF(ABS(dmem(1))>=thisMP%mem_threshold .AND. ASSOCIATED(thisMP%myLog) .AND. &
        thisMP%pe%world%master) THEN
      WRITE(tmpchar,'(a)') 'Memory Use at '//TRIM(name)//':'
      WRITE(amesg,'(a,3(f10.3),"  (",I4,")")') ADJUSTL(tmpchar), mem, maxmem, dmem, loc
      CALL thisMP%myLog%message(TRIM(amesg),.FALSE.,.TRUE.)
    ENDIF

    thisMP%mem_old=thisMP%mem_current
  ENDIF
#endif
ENDSUBROUTINE edit_MemProf
!
!-------------------------------------------------------------------------------
!> @brief Default procedure for
!> @param thisMP the memory profiling object
!>
SUBROUTINE enableEdits_MemProf(thisMP)
  CLASS(Memory_Profiler),INTENT(INOUT) :: thisMP
  thisMP%mem_edit=.TRUE.
#ifndef FUTILITY_MEMPROF
    IF(ASSOCIATED(thisMP%myLog) .AND. thisMP%pe%world%master) &
        CALL thisMP%myLog%message("Unable to enable memory edits.  Need to " &
        //"reconfigure with Memory Profiling on.",.FALSE.,.TRUE.)
#endif
ENDSUBROUTINE enableEdits_MemProf
!
!-------------------------------------------------------------------------------
!> @brief Default procedure for
!> @param thisMP the memory profiling object
!>
SUBROUTINE disableEdits_MemProf(thisMP)
  CLASS(Memory_Profiler),INTENT(INOUT) :: thisMP
  thisMP%mem_edit=.FALSE.
ENDSUBROUTINE disableEdits_MemProf
!
ENDMODULE MemProf

