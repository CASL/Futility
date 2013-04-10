!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                              Copyright (C) 2012                              !
!                   The Regents of the University of Michigan                  !
!              MPACT Development Group and Prof. Thomas J. Downar              !
!                             All rights reserved.                             !
!                                                                              !
! Copyright is reserved to the University of Michigan for purposes of          !
! controlled dissemination, commercialization through formal licensing, or     !
! other disposition. The University of Michigan nor any of their employees,    !
! makes any warranty, express or implied, or assumes any liability or          !
! responsibility for the accuracy, completeness, or usefulness of any          !
! information, apparatus, product, or process disclosed, or represents that    !
! its use would not infringe privately owned rights. Reference herein to any   !
! specific commercial products, process, or service by trade name, trademark,  !
! manufacturer, or otherwise, does not necessarily constitute or imply its     !
! endorsement, recommendation, or favoring by the University of Michigan.      !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module to replicate some TAU interfaces when TAU is not
!>        available. Only measures time and number of calls.
!>
!> This module makes the following interfaces from the TAU API available:
!>  - @c TAU_PROFILE_INIT - must be called before any of the other routines
!>  - @c TAU_PROFILE_TIMER - defines a static timer
!>  - @c TAU_PROFILE_START - starts profiling a section
!>  - @c TAU_PROFILE_STOP - stops profiling a section (must be called after 
!>       @c TAU_PROFILE_START)
!>  - @c TAU_PROFILE_EXIT - stops all profiling and writes profiling data to
!>                          disk
!>
!> These can be used essentially the same way as they would be used by TAU. The
!> profiling capability here can be used to profile sections of code in serial,
!> MPI, OpenMP, or OpenMP/MPI applications. However, there are several caveats
!> the user must be aware of.
!>
!> @par First:
!> Automatic source instrumentation is not available, so using the 
!> profiling capability requires hand instrumentation of the source. When
!> instrumenting the source the <TT>USE TAU_Stubs</TT> statement must be
!> within the scope of the procedure.
!>
!> @par Second:
!> The @ref TAU_Stubs::TAU_PROFILE_EXIT "TAU_PROFILE_EXIT", routine must be
!> called prior to the halt of execution else no profile data is written to 
!> disk. 
!>
!> @par Third:
!> Applies only to situations where the application uses OpenMP. In
!> OpenMP applications it is @b ESSENTIAL that the following be added
!> to any @c PARALLEL construct:
!> @code
!> !$OMP SHARED(TauStubLibData)
!> @endcode
!>
!> Furthermore, there are some important subtleties to understand about the
!> profilers. When a profiler is defined with @ref TAU_Stubs::TAU_PROFILE_TIMER
!> "TAU_PROFILE_TIMER" it is thread dependent. The return argument @c profileID
!> essentially contains two indices: the thread on which the profiler was defined
!> and the number of profilers defined before it. This data functions as
!> the profiler database lookup key. When outside of @c PARALLEL
!> constructs, the master thread will be able to access any of the profilers, but
!> the correct profiler key must be passed to do so. Therefore, the @c profileID
!> return argument will need to persist with the correct value after the parallel
!> region has ended.
!> 
!> It gets slightly more complicated. When inside a parallel region, only those
!> threads for which a specific profiler is defined can access that profiler.
!> Note here it is therefore important that the @c profileID return argument
!> persist with the correct value into the parallel region. To do this it is
!> recommended that any profilerID variables be given the @c FIRSTPRIVATE data
!> scope.
!>
!> Some specific examples are provided below to illustrate the usage.
!>
!> @par Fourth:
!> The data is exported into the TAU profile format, and is therefore viewable
!> in ParaProf. All times reported have identical values for exclusive and
!> inclusive metrics. The edited times are strictly inclusive. No child calls
!> are recorded. All measurements are put into the "TAU_STUBS" group. The file
!> metadata is also extremely limited and only includes the
!> "TAU_PROFILE_FORMAT", "TAU Version", and a new meta-data element that reports
!> the estimated overhead from using these timers. The TAU Version is listed as
!> "TAU Stubs Library (MPACT)".
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref Strings "Strings": @copybrief Strings
!>  - @ref Times "Times": @copybrief Times
!>
!> @par EXAMPLES
!> @code
!> PROGRAM TestTauStubs
!> 
!>   USE TAU_Stubs
!>   IMPLICIT NONE
!>
!>   
!> END PROGRAM
!> @endcode
!>
!> @author Brendan Kochunas
!>   @date 02/06/2013
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE TAU_Stubs

!$ USE OMP_LIB  
  USE IntrType
  USE Strings
  USE Times
  IMPLICIT NONE
  PRIVATE
  
#ifdef HAVE_MPI
  INCLUDE 'mpif.h'
#endif
!
!List of public members
  PUBLIC :: TauStubLibData
  PUBLIC :: TAU_PROFILE_INIT
  PUBLIC :: TAU_PROFILE_TIMER
  PUBLIC :: TAU_PROFILE_START
  PUBLIC :: TAU_PROFILE_STOP
  PUBLIC :: TAU_PROFILE_EXIT
  
  !> @brief Derived type for defines a basic profiler.
  !>
  !> A profiler is given a name and keeps track of the execution time of
  !> a region and the number of times the start/stop operations were
  !> called for the specific profiler.
  !>
  TYPE :: ProfilerType
    INTEGER(SLK) :: num_calls=0
    LOGICAL(SBK) :: isStart=.FALSE.
    LOGICAL(SBK) :: inOMP=.FALSE.
    REAL(SDK) :: ompTime=0.0_SDK
    TYPE(StringType) :: name
    TYPE(TimerType) :: timer
    TYPE(ProfilerType),POINTER :: nextProfiler => NULL()
  ENDTYPE ProfilerType
  
  !> @brief A type for containing an array of scalar pointers for profilers
  !>
  !> Essentially a database.
  !>
  TYPE :: ProfilerDBType
    TYPE(ProfilerType),POINTER :: profiler => NULL()
  ENDTYPE ProfilerDBType
  
  !> @brief Type for storing all the profiling data amongst processes
  !>
  !> This is used to define one variable which is a singleton object
  !> of this type.
  !>
  TYPE :: TauStubModData
    LOGICAL(SBK) :: isInit=.FALSE.
    INTEGER(SIK) :: rankWorld=-1
    INTEGER(SLK) :: nthreads=-1
    REAL(SDK) :: tOverhead=-1.0_SDK
    INTEGER(SIK),POINTER :: nProfiles(:)
    TYPE(ProfilerDBType),POINTER :: threadProfilesDB(:)
  ENDTYPE TauStubModData
  
  !> Singleton object containing all the profilers and profiling data for the 
  !> given executable.
  TYPE(TauStubModData),SAVE :: TauStubLibData
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes module global variables
!>
!> Determines the number of threads and the number of MPI processors of the
!> execution environment. Allocates space for profilers.
!>
    SUBROUTINE TAU_PROFILE_INIT()
      INTEGER(SIK) :: isinit,mpierr

!$OMP SINGLE
      IF(.NOT.TauStubLibData%isInit) THEN
  
#ifdef HAVE_MPI
        CALL MPI_Initialized(isinit,mpierr)
        IF(isinit == 0) CALL MPI_Init(mpierr)
        CALL MPI_Comm_rank(MPI_COMM_WORLD,TauStubLibData%rankWorld,mpierr)
#else
        TauStubLibData%rankWorld=0
#endif
        TauStubLibData%nthreads=1
!$      TauStubLibData%nthreads=OMP_GET_MAX_THREADS()
        
        ALLOCATE(TauStubLibData%threadProfilesDB(TauStubLibData%nthreads))
        ALLOCATE(TauStubLibData%nProfiles(TauStubLibData%nthreads))
        TauStubLibData%nProfiles=0
  
        !Determine overhead
        CALL EstimateOverhead()
      ENDIF
      TauStubLibData%isInit=.TRUE.
!$OMP END SINGLE
    ENDSUBROUTINE TAU_PROFILE_INIT
!
!-------------------------------------------------------------------------------
!> @brief Initializes a profiler
!> @param profileID the profiler ID
!> @param profileName the name to give the region or event that is to be
!>        profiled.
!>
    SUBROUTINE TAU_PROFILE_TIMER(profileID,name)
      INTEGER(SIK),INTENT(INOUT) :: profileID(2)
      CHARACTER(LEN=*),INTENT(IN) :: name
      INTEGER(SIK) :: tid,it
      TYPE(ProfilerType),POINTER :: newThreadProfiler
  
      IF(ALL(profileID == 0) .AND. TauStubLibData%isInit) THEN
        tid=1
!$      tid=OMP_GET_THREAD_NUM()+1
        IF(ASSOCIATED(TauStubLibData%threadProfilesDB(tid)%profiler)) THEN
          newThreadProfiler => TauStubLibData%threadProfilesDB(tid)%profiler
          DO it=1,TauStubLibData%nProfiles(tid)-1
            newThreadProfiler => newThreadProfiler%nextProfiler
          ENDDO
          ALLOCATE(newThreadProfiler%nextProfiler)
          newThreadProfiler => newThreadProfiler%nextProfiler
        ELSE
          ALLOCATE(TauStubLibData%threadProfilesDB(tid)%profiler)
          newThreadProfiler => TauStubLibData%threadProfilesDB(tid)%profiler
        ENDIF
        TauStubLibData%nProfiles(tid)=TauStubLibData%nProfiles(tid)+1
        newThreadProfiler%name=TRIM(ADJUSTL(name))
        profileID(1)=tid
        profileID(2)=TauStubLibData%nProfiles(tid)
      ENDIF
    ENDSUBROUTINE TAU_PROFILE_TIMER
!
!-------------------------------------------------------------------------------
!> @brief Starts measurements for a profiler
!> @param profileID the profiler ID
!>
    SUBROUTINE TAU_PROFILE_START(profileID)
      INTEGER(SIK),INTENT(IN) :: profileID(2)
      LOGICAL(SBK) :: inOMP
      INTEGER(SIK) :: it,tid
      TYPE(ProfilerType),POINTER :: activeThreadProfiler
      
      IF(profileID(1) > 0 .AND. profileID(1) <= TauStubLibData%nthreads) THEN
!       Profilers are thread dependent, so only access profilers initialized on
!       other threads when NOT in a parallel region.
!
        tid=1
        inOMP=.FALSE.
!$      inOMP=OMP_IN_PARALLEL()
!$      tid=OMP_GET_THREAD_NUM()+1
!$      IF(.NOT.inOMP .OR. (inOMP .AND. profileID(1) == tid)) THEN
          tid=profileID(1)
          IF(profileID(2) > 0 .AND. profileID(2) <= TauStubLibData%nProfiles(tid)) THEN
            activeThreadProfiler => TauStubLibData%threadProfilesDB(tid)%profiler
            DO it=1,profileID(2)-1
              activeThreadProfiler => activeThreadProfiler%nextProfiler
            ENDDO
            activeThreadProfiler%isStart=.TRUE.
            activeThreadProfiler%inOMP=inOMP
            IF(.NOT.activeThreadProfiler%inOMP) THEN
              CALL activeThreadProfiler%timer%tic()
            ELSE
!$            activeThreadProfiler%ompTime=OMP_GET_WTIME()
            ENDIF
          ENDIF
!$      ENDIF
      ENDIF
    ENDSUBROUTINE TAU_PROFILE_START
!
!-------------------------------------------------------------------------------
!> @brief Stops measurements for a profiler
!> @param profileID the profiler ID
!>
    SUBROUTINE TAU_PROFILE_STOP(profileID)
      INTEGER(SIK),INTENT(IN) :: profileID(2)
      INTEGER(SIK) :: it,tid
      TYPE(ProfilerType),POINTER :: activeThreadProfiler
  
      IF(profileID(1) > 0 .AND. profileID(1) <= TauStubLibData%nthreads) THEN
        tid=profileID(1)
        activeThreadProfiler => TauStubLibData%threadProfilesDB(tid)%profiler
        IF(profileID(2) > 0 .AND. profileID(2) <= TauStubLibData%nProfiles(tid)) THEN
          DO it=1,profileID(2)-1
            activeThreadProfiler => activeThreadProfiler%nextProfiler
          ENDDO
          IF(activeThreadProfiler%isStart) THEN
            IF(.NOT.activeThreadProfiler%inOMP) THEN
              CALL activeThreadProfiler%timer%toc()
            ELSE
!$            activeThreadProfiler%timer%elapsedTime= &
!$              activeThreadProfiler%timer%elapsedTime+OMP_GET_WTIME()-&
!$                activeThreadProfiler%ompTime
            ENDIF
            activeThreadProfiler%isStart=.FALSE.
            activeThreadProfiler%num_calls=activeThreadProfiler%num_calls+1
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE TAU_PROFILE_STOP
!
!-------------------------------------------------------------------------------
!> @brief Outputs all the profiler measurement data to disk and removes
!> profilers from memory. Should be called prior to exiting or stopping 
!> execution.
!>
!> When adding this to an executable it should be the last line called before
!> @c "ENDPROGRAM". Although it can be placed elsewhere if you understand what
!> you're doing. If you are running an MPI executable then you should 
!> place this call just before the call to MPI_Finalize().
!>
!> If the program terminates prior to this routine being called (e.g. because of
!> an error) then no profile data will be output.
!>
!> It may be called from inside a parallel region, and it should work ok, but
!> probably better if you don't.
!>
    SUBROUTINE TAU_PROFILE_EXIT()
      CHARACTER(LEN=16) :: rankstr,tidstr,tmpIstr
      CHARACTER(LEN=20) :: tmpRstr
      LOGICAL(SBK) :: isopen
      INTEGER(SIK) :: it,ip,funit,ierr
      TYPE(StringType) :: fname,tline
      TYPE(ProfilerType),POINTER :: activeProfiler

!$OMP BARRIER
!$OMP MASTER
!
!Get an open unit number for the file
      funit=723
      isopen=.FALSE.
      INQUIRE(UNIT=funit,OPENED=isopen)
      DO WHILE(isopen)
        funit=funit+1
        INQUIRE(UNIT=funit,OPENED=isopen)
      ENDDO
      WRITE(rankstr,'(i16)') TauStubLibData%rankWorld
!
!Loop over threads and write files
      DO it=1,TauStubLibData%nthreads
        WRITE(tidstr,'(i16)') it-1
        fname='profile.'//TRIM(ADJUSTL(rankstr))//'.0.'//TRIM(ADJUSTL(tidstr))
        IF(TauStubLibData%nProfiles(it) > 0) THEN
          OPEN(UNIT=funit,ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='WRITE', &
            STATUS='REPLACE',FILE=CHAR(fname),IOSTAT=ierr)
          IF(ierr == 0) THEN
!
!Write the profile data to disk
            !Header
            WRITE(tmpIstr,'(i16)') TauStubLibData%nProfiles(it)
            tline=TRIM(ADJUSTL(tmpIstr))//' templated_functions_MULTI_LINUX_TIMERS'
            WRITE(UNIT=funit,FMT='(a)',IOSTAT=ierr) CHAR(tline)
          
            !Metadata
            WRITE(tmpRstr,'(g9.3)') TauStubLibData%tOverhead
            tline='# Name Calls Subrs Excl Incl ProfileCalls # <metadata>'
            tline=tline//'<attribute>'// &
              '<name>Metric Name</name><value>LINUX_TIMERS</value>'// &
             '</attribute><attribute>'// &
              '<name>TAU Version</name><value>TAU Stub Library (MPACT)</value>'// &
             '</attribute><attribute>'// &
              '<name>Est. overhead per call</name><value>'//TRIM(ADJUSTL(tmpRstr))// &
                ' microsecs.</value>'// &
             '</attribute><attribute>'// &
              '<name>TAU_PROFILE_FORMAT</name><value>profile</value>'
            tline=tline//'</attribute></metadata>'
            WRITE(UNIT=funit,FMT='(a)',IOSTAT=ierr) CHAR(tline)
          
            !Write profile data (Consists of INCLUSIVE times only)
            activeProfiler => TauStubLibData%threadProfilesDB(it)%profiler
            DO ip=1,TauStubLibData%nProfiles(it)
              
              IF(activeProfiler%isStart) THEN
                !Make sure the profiler has been stopped
                CALL activeProfiler%timer%toc()
                activeProfiler%isStart=.FALSE.
                activeProfiler%num_calls=activeProfiler%num_calls+1
              ENDIF
              IF(activeProfiler%timer%getTimeReal() < 0.0_SDK) THEN
                WRITE(tmpRstr,'(g20.15)') 0.0_SDK
              ELSE
                WRITE(tmpRstr,'(g20.15)') ((activeProfiler%timer%getTimeReal())*1.e6_SDK)
              ENDIF
              WRITE(tmpIstr,'(i16)') activeProfiler%num_calls
              tline='"'//CHAR(activeProfiler%name)//'" '//TRIM(ADJUSTL(tmpIstr))// &
                ' 0 '//TRIM(ADJUSTL(tmpRstr))//' '//TRIM(ADJUSTL(tmpRstr))// &
                ' 0 GROUP="TAU_STUB"'
              WRITE(UNIT=funit,FMT='(a)',IOSTAT=ierr) CHAR(tline)
              activeProfiler => activeProfiler%nextProfiler
            ENDDO
          
            !Write end of file
            WRITE(UNIT=funit,FMT='(a)',IOSTAT=ierr) '0 aggregates'
            WRITE(UNIT=funit,FMT='(a)',IOSTAT=ierr) '0 userevents'
            CLOSE(UNIT=funit,IOSTAT=ierr)
          ENDIF
        ENDIF
      ENDDO
!
!Destroy all the profilers.
      DO it=1,TauStubLibData%nthreads
        activeProfiler => TauStubLibData%threadProfilesDB(it)%profiler
        CALL Clear_ProfilerType(activeProfiler)
      ENDDO
      DEALLOCATE(TauStubLibData%nProfiles)
      TauStubLibData%nthreads=-1
      TauStubLibData%rankWorld=-1
      TauStubLibData%tOverhead=-1.0_SDK
      TauStubLibData%isInit=.FALSE.
!$OMP END MASTER
    ENDSUBROUTINE TAU_PROFILE_EXIT
!
!-------------------------------------------------------------------------------
!> @brief Recursively clears a linked list of profilers
!>
!> Not public. Should only be called by TAUSTUB_FINISH
!>
    RECURSIVE SUBROUTINE Clear_ProfilerType(thisProfiler)
      TYPE(ProfilerType),POINTER,INTENT(INOUT) :: thisProfiler
      IF(ASSOCIATED(thisProfiler)) THEN
        CALL Clear_ProfilerType(thisProfiler%nextProfiler)
        thisProfiler%name=''
        DEALLOCATE(thisProfiler)
      ENDIF
    ENDSUBROUTINE Clear_ProfilerType
!
!-------------------------------------------------------------------------------
!> @brief Attempts to estimate the overhead of the profiling calls.
!>
!> Not public. Should only be called by TAU_PROFILE_INIT
!>
    SUBROUTINE EstimateOverhead()
      INTEGER(SIK),PARAMETER,DIMENSION(2) :: pid=(/1,1/)  
      INTEGER(SIK) :: i
      
      TauStubLibData%nProfiles(1)=1
      ALLOCATE(TauStubLibData%threadProfilesDB(1)%profiler)
      
      !Estimate the overhead by repeatedly making 10000 start/stop calls
      !Until a measureable amount of time has passed.
      TauStubLibData%tOverhead=0.0_SDK
      DO WHILE(TauStubLibData%tOverhead == 0.0_SDK)
        DO i=1,1000
          CALL TAU_PROFILE_START(pid)
          CALL TAU_PROFILE_STOP(pid)
        ENDDO
        TauStubLibData%tOverhead=1.e6_SDK/ &
          REAL(TauStubLibData%threadProfilesDB(1)%profiler%num_calls,SDK)* &
            TauStubLibData%threadProfilesDB(1)%profiler%timer%getTimeReal()
      ENDDO
      
      TauStubLibData%nProfiles(1)=0
      DEALLOCATE(TauStubLibData%threadProfilesDB(1)%profiler)
    ENDSUBROUTINE EstimateOverhead
!
ENDMODULE TAU_Stubs
