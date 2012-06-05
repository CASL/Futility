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
!> @brief Utility module providing timers and clock information.
!>
!> This module provides a Fortran 2003 object for timing code and providing 
!> clock information. Timing is preformed by interfacing with the 
!> @c SYSTEM_CLOCK Fortran intrinsic subroutine. A timer can be set to high or 
!> low resolution. The default is high resolution which makes use of long
!> integers for interfacing with the intrinsic subroutine @c SYSTEM_CLOCK.
!> All the object attributes are private but type bound procedures are provided
!> to interface with the object attributes. The date and clock routines can be
!> accessed independently of a timer. This module is tested using @c 
!> testTimes.f90. An example of how to use this object is given below, the unit
!> test also shows how to use the object. Code coverage documentation can be
!> found on the @ref CodeCoverageReports page.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>
!> @par EXAMPLES
!> @code
!> PROGRAM TestTimerExample
!> 
!>   USE Times
!>   IMPLICIT NONE
!>
!>   ! ...other local variables...
!>
!>   TYPE(TimerType) :: myTimer
!>
!>   ! ...some executable code...
!>
!>   !give the timer a name
!>   CALL myTimer%setTimerName('My Timer')
!> 
!>   WRITE(*,*) 'Calculation started at '//getClockTime()//' on '//getDate()
!>
!>   !Start timing
!>   CALL myTimer%tic()
!>
!>   ! ...region of code to be timed...
!>
!>   !Stop timing
!>   CALL myTimer%toc()
!>
!>   WRITE(*,*) 'Timer '//myTimer%getTimerName()//' had time '// &
!>              myTimer%getTimeChar()
!>   WRITE(*,*) myTimer%getTimeHHMMSS()
!>   WRITE(*,*) myTimer%getTimeReal()
!> END PROGRAM
!> @endcode
!>
!> @author Brendan Kochunas
!>   @date 06/09/2009
!>
!> @par Revisions:
!> (01/19/2011) - Brendan Kochunas
!>   - Added documentation for doxygen. Set public private access to variables.
!>     Created timer derived data types and public methods for objects.
!> @par
!> (04/19/2011) - Brendan Kochunas
!>   - Added activeTimer and *_default methods for activeTimer
!>   - Put under test
!> @par
!> (06/23/2011) - Brendan Kochunas
!>   - Removed interfaces for module timers and active timer and made timer 
!>     object procedures type-bound.
!>   - Updated unit test.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Times

  USE IntrType
  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: TimerType
  PUBLIC :: getDate
  PUBLIC :: getClockTime
  PUBLIC :: MAXLEN_TIMER_NAME
  PUBLIC :: MAXLEN_TIME_STRING
  PUBLIC :: MAXLEN_DATE_STRING
  PUBLIC :: MAXLEN_CLOCK_STRING
      
  !> Maximum length of character string for timer names
  INTEGER(SIK),PARAMETER :: MAXLEN_TIMER_NAME=20
  !> Maximum length of character string for the reported time
  INTEGER(SIK),PARAMETER :: MAXLEN_TIME_STRING=18
  !> Maximum length of character string for the reported date
  INTEGER(SIK),PARAMETER :: MAXLEN_DATE_STRING=13
  !> Maximum length of character string for the elapsed time (private)
  INTEGER(SIK),PARAMETER :: MAXLEN_ETIME=9
  !> Maximum length of character string for the reported clock time
  INTEGER(SIK),PARAMETER :: MAXLEN_CLOCK_STRING=8
      
  !> @brief Derived Datatype for the Timer data object
  !>
  !> This is the data object is used to define default timers within the
  !> module for basic things a programmer may want to time. This derived
  !> data type is also public so the programmer can create their own timers
  !> if they wish for use elsewhere in the code.
  TYPE :: TimerType
    !> @brief Descriptive name for the timer (20 characters or less)
    CHARACTER(LEN=MAXLEN_TIMER_NAME),PRIVATE :: name=''
    !> @brief Clock cycle count value for start of the timer (set by 
    !> @ref Times::tic "tic")
    INTEGER(SLK),PRIVATE :: count=0_SDK
    !> @brief The elapsed time recorded for this timer in seconds.
    REAL(SDK) :: elapsedtime=0_SDK
    !> @brief The time recorded by this timer given as a string.
    CHARACTER(LEN=9),PRIVATE :: time=''
    !> @brief The units for the time given in @ref Times::TimerType::time 
    !> "%time", also a string.
    CHARACTER(LEN=9),PRIVATE :: unit=''
    !> @brief indicates if the timer is high resolution
    LOGICAL(SBK),PRIVATE :: HiResTimer=.TRUE.
    !> @brief Convert the processor clock count value to a time in seconds.
    REAL(SDK),PRIVATE :: count2sec=-1._SDK
    !> @brief The last count value taken from SYSTEM_CLOCK
    !>
    !> Needed to determine if the clock counter rolled over
    INTEGER(SLK),PRIVATE :: lastcount=0_SLK
    !> @brief The number of counts to shift due to clock cycle rollover.
    !>
    !> Add count_max each time the clock rolls over.
    INTEGER(SLK),PRIVATE :: clockcycleshift=0_SLK
!
!List of type bound procedures (methods) for the object
    CONTAINS
      !> @copybrief Times::getDate
      !> @copydetails Times::getDate
      PROCEDURE,NOPASS :: getDate
      !> @copybrief Times::getClockTime
      !> @copydetails Times::getClockTime
      PROCEDURE,NOPASS :: getClockTime
      !> @copybrief Times::getTimerResolution
      !> @copydetails Times::getTimerResolution
      PROCEDURE,PASS :: getTimerResolution
      !> @copybrief Times::getRemainingTime
      !> @copydetails Times::getRemainingTime
      PROCEDURE,PASS :: getRemainingTime
      !> @copybrief Times::ResetTimer
      !> @copydetails Times::ResetTimer
      PROCEDURE,PASS :: ResetTimer
      !> @copybrief Times::setTimerName
      !> @copydetails Times::setTimerName
      PROCEDURE,PASS :: setTimerName
      !> @copybrief Times::getTimerName
      !> @copydetails Times::getTimerName
      PROCEDURE,PASS :: getTimerName
      !> @copybrief Times::setTimerHiResMode
      !> @copydetails Times::setTimerHiResMode
      PROCEDURE,PASS :: setTimerHiResMode
      !> @copybrief Times::getTimerHiResMode
      !> @copydetails Times::getTimerHiResMode
      PROCEDURE,PASS :: getTimerHiResMode
      !> @copybrief Times::tic
      !> @copydetails Times::tic
      PROCEDURE,PASS :: tic
      !> @copybrief Times::toc
      !> @copydetails Times::toc
      PROCEDURE,PASS :: toc
      !> @copybrief Times::getTimeHHMMSS
      !> @copydetails Times::getTimeHHMMSS
      PROCEDURE,PASS :: getTimeHHMMSS
      !> @copybrief Times::getTimeReal
      !> @copydetails Times::getTimeHHMMSS
      PROCEDURE,PASS :: getTimeReal
      !> @copybrief Times::getTimeChar
      !> @copydetails Times::getTimeChar
      PROCEDURE,PASS :: getTimeChar
  ENDTYPE TimerType
    
  !> @brief The current value of the processor clock (for HI-RES timer)
  !>
  !> First output argument of the intrinsic Fortran subroutine SYSTEM_CLOCK.
  !> Not saved, value is changed on each call to SYSTEM_CLOCK. This is by
  !> default a long integer which increases the timer resolution at the
  !> potential expense of portability as SYSTEM_CLOCK is only defined
  !> for the default type integer.
  INTEGER(SLK) :: count_hi
  !> @brief The rate of processor clock counts per second (for HI-RES timer)
  !>
  !> Second output argument of the intrinsic Fortran subroutine
  !> SYSTEM_CLOCK. Allows one to convert counts to seconds. This is by
  !> default a long integer which increases the timer resolution at the
  !> potential expense of portability as SYSTEM_CLOCK is only defined
  !> for the default type integer.
  INTEGER(SLK) :: rate_hi
  !> @brief The total number of processor clock counts before rollover.
  !> (for HI-RES timer)
  !>
  !> Third output argument of the intrinsic Fortran subroutine SYSTEM_CLOCK.
  !> Not saved, although it is constant during run time. This is by
  !> default a long integer which increases the timer resolution at the
  !> potential expense of portability as SYSTEM_CLOCK is only defined
  !> for the default type integer.
  INTEGER(SLK) :: count_max_hi
  !> @brief The current value of the processor clock (for HI-RES timer)
  !>
  !> First output argument of the intrinsic Fortran subroutine SYSTEM_CLOCK.
  !> Not saved, value is changed on each call to SYSTEM_CLOCK. This is by
  !> default a long integer which increases the timer resolution at the
  !> potential expense of portability as SYSTEM_CLOCK is only defined
  !> for the default type integer.
  INTEGER :: count_lo
  !> @brief The rate of processor clock counts per second (for LO-RES timer)
  !>
  !> Second output argument of the intrinsic Fortran subroutine
  !> SYSTEM_CLOCK. Allows one to convert counts to seconds. This is by
  !> default a long integer which increases the timer resolution at the
  !> potential expense of portability as SYSTEM_CLOCK is only defined
  !> for the default type integer.
  INTEGER :: rate_lo
  !> @brief The total number of processor clock counts before rollover.
  !> (for LO-RES timer)
  !>
  !> Third output argument of the intrinsic Fortran subroutine SYSTEM_CLOCK.
  !> Not saved, although it is constant during run time. This is by
  !> default a long integer which increases the timer resolution at the
  !> potential expense of portability as SYSTEM_CLOCK is only defined
  !> for the default type integer.
  INTEGER :: count_max_lo
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Function returns current clock time as a string as "hh:mm:ss"
    FUNCTION getClockTime() RESULT(clocktime)
      CHARACTER(LEN=8) :: clocktime
      CHARACTER(LEN=10) :: atime
      CHARACTER(LEN=8) :: adate
      CHARACTER(LEN=2) :: hh,mm,ss

      clocktime=''
      CALL DATE_AND_TIME(adate,atime)
      hh=atime(1:2)
      mm=atime(3:4)
      ss=atime(5:6)
      
      clocktime=hh//':'//mm//':'//ss
    ENDFUNCTION getClockTime
!
!-------------------------------------------------------------------------------
!> @brief Function returns the current date as a string in a specified format
!> @param opt input argument, integer for format option (either 1 or 2)
!>
!> The format options are (Default is 1):
!> -# "mm/dd/yyyy" (length 8 characters)
!> -# "Month, day year" (length 13 characters)
    FUNCTION getDate(opt) RESULT(strdate)
      CHARACTER(LEN=4),DIMENSION(12),PARAMETER :: &
      mon=(/'Jan.','Feb.','Mar.','Apr.','May ','June', &
            'July','Aug.','Sep.','Oct.','Nov.','Dec.'/)
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: opt
      CHARACTER(LEN=13) :: strdate
      CHARACTER(LEN=8) :: adate
      CHARACTER(LEN=4) :: yy
      CHARACTER(LEN=2) :: dd,mm
      INTEGER(SIK) :: imon,fmt

      IF(PRESENT(opt)) THEN
        fmt=opt
      ELSE
        fmt=1
      ENDIF
      
      strdate=''
      CALL DATE_AND_TIME(adate)
      yy=adate(1:4)
      mm=adate(5:6)
      dd=adate(7:8)
      READ(mm,'(i2)') imon
      IF(fmt == 1_SIK .AND. LEN(strdate) >= 8_SIK) THEN
        strdate=mm//'/'//dd//'/'//yy
      ELSEIF(fmt == 2_SIK .AND. LEN(strdate) >= 13_SIK) THEN
        strdate=mon(imon)//' '//dd//', '//yy
      ENDIF
    ENDFUNCTION getDate
!
!-------------------------------------------------------------------------------
!> @brief Function returns the timer resolution of the system_clock on the
!> current machine.
!>
!> The timer resolution is given in units of microseconds as a double precision
!> real value.
    FUNCTION getTimerResolution(t) RESULT(tres)
      CLASS(TimerType),INTENT(INOUT) :: t
      REAL(SDK) :: tres

      IF(t%HiResTimer) THEN
        CALL SYSTEM_CLOCK(count_hi,rate_hi,count_max_hi)
        t%lastcount=count_hi
        tres=1000000._SDK/REAL(rate_hi,SDK)
      ELSE
        CALL SYSTEM_CLOCK(count_lo,rate_lo,count_max_lo)
        t%lastcount=count_lo
        tres=1000000._SDK/REAL(rate_lo,SDK)
      ENDIF
    ENDFUNCTION getTimerResolution
!
!-------------------------------------------------------------------------------
!> @brief Function returns the time remaining before the clock counter rolls
!> over.
!>
!> The result is returned as a double precision real.
    FUNCTION getRemainingTime(t) RESULT(tremain)
      CLASS(TimerType),INTENT(INOUT) :: t
      REAL(SDK) :: tremain

      IF(t%HiResTimer) THEN
        CALL SYSTEM_CLOCK(count_hi,rate_hi,count_max_hi)
        t%lastcount=count_hi
        tremain=REAL((count_max_hi-count_hi),SDK)/REAL(rate_hi,SDK)
      ELSE
        CALL SYSTEM_CLOCK(count_lo,rate_lo,count_max_lo)
        t%lastcount=count_lo
        tremain=REAL((count_max_lo-count_lo),SDK)/REAL(rate_lo,SDK)
      ENDIF
    ENDFUNCTION getRemainingTime

!
!-------------------------------------------------------------------------------
!> @brief Set the name of a timer @e myTimer
!> @param myTimer dummy argument of timer to modify the name of
!> @param name input argument, name to assign to the timer
!>
!> Names must be 20 characters or less.
    SUBROUTINE setTimerName(myTimer,name)
      CLASS(TimerType),INTENT(INOUT) :: myTimer
      CHARACTER(LEN=*),INTENT(IN) :: name
      myTimer%name=TRIM(name)
    ENDSUBROUTINE setTimerName
!
!-------------------------------------------------------------------------------
!> @brief Get the name of a timer @e myTimer
!> @param myTimer dummy argument of timer to return the name of
!> @returns name input argument, name output from timer
!>
!> Names are 20 characters or less.
    FUNCTION getTimerName(myTimer) RESULT(name)
      CLASS(TimerType),INTENT(IN) :: myTimer
      CHARACTER(LEN=MAXLEN_TIMER_NAME) :: name
      name=TRIM(myTimer%name)
    ENDFUNCTION getTimerName
!
!-------------------------------------------------------------------------------
!> @brief Set the mode of resolution of a timer @e myTimer
!> @param myTimer dummy argument of timer to modify the name of
!> @param resMode the timer resolution mode TRUE=HI-RES, FALSE=LO-RES
    SUBROUTINE setTimerHiResMode(myTimer,resMode)
      CLASS(TimerType),INTENT(INOUT) :: myTimer
      LOGICAL(SBK),INTENT(IN) :: resMode
      myTimer%HiResTimer=resMode
      CALL ResetTimer(myTimer)
    ENDSUBROUTINE setTimerHiResMode
!
!-------------------------------------------------------------------------------
!> @brief Get the mode of resolution of a timer @e myTimer
!> @param myTimer dummy argument of timer to return the name of
!> @returns tmode the timer resolution mode TRUE=HI-RES, FALSE=LO-RES
    FUNCTION getTimerHiResMode(myTimer) RESULT(tmode)
      CLASS(TimerType),INTENT(IN) :: myTimer
      LOGICAL(SBK) :: tmode
      tmode=myTimer%HiResTimer
    ENDFUNCTION getTimerHiResMode
!
!-------------------------------------------------------------------------------
!> @brief Function returns the value of @ref Times::TimerType::elapsedtime 
!> "%elapsedtime" as a real type.
!> @param myTimer input argument, a @ref Times::TimerType "TimerType" variable
!> @returns time, the elapsed time (REAL type) unit is seconds
    FUNCTION getTimeReal(myTimer) RESULT(time)
      CLASS(TimerType),INTENT(IN) :: myTimer
      REAL(SRK) :: time
      time=myTimer%elapsedtime
    ENDFUNCTION getTimeReal
!
!-------------------------------------------------------------------------------
!> @brief Subroutine returns the value of @ref Times::TimerType::elapsedtime 
!> "%elapsedtime" as a string with units.
!> @param myTimer input argument, a @ref Times::TimerType "TimerType" variable
!> @returns time output argument, the elapsed time as a string
    FUNCTION getTimeChar(myTimer) RESULT(time)
      CLASS(TimerType),INTENT(INOUT) :: myTimer
      CHARACTER(LEN=MAXLEN_TIME_STRING) :: time

      CALL SetTimeAndUnits(myTimer)
      time=TRIM(myTimer%time)//TRIM(myTimer%unit)
    ENDFUNCTION getTimeChar
!
!-------------------------------------------------------------------------------
!> @brief Function returns the value of @ref Times::TimerType::elapsedtime 
!> "%elapsedtime" as a string in HHMMSS format.
!> @param myTimer input argument, a @ref Times::TimerType "TimerType" variable
!> @returns hh_mm_ss, the elapsed time as a string (hhh:mm:ss or mmm:ss.ss)
    FUNCTION getTimeHHMMSS(myTimer) RESULT(hh_mm_ss)
      REAL(SDK),PARAMETER :: half=0.5_SDK
      CLASS(TimerType),INTENT(IN) :: myTimer
      CHARACTER(LEN=MAXLEN_ETIME) :: hh_mm_ss
      INTEGER(SIK) :: it,hrs,hr1,hr2,mins,min1,min2,secs,sec1,sec2
      REAL(SRK) :: t,sfrac
!
!hhmmss='mmm:ss.ss'
!hhmmss='hhh:mm:ss'
      t=myTimer%elapsedtime
      
      it=INT(t,SIK)
      hrs=it/3600_SIK                   ! Total number of hours
      hr1=hrs/10_SIK                    ! 10's digit of hours
      hr2=MOD(hrs,10_SIK)               ! 1's digit of hours
      
      mins=(it-hrs*3600_SIK)/60_SIK     ! Total number of minutes
      min1=mins/10_SIK                  ! 10's digit of minutes
      min2=MOD(mins,10_SIK)             ! 1's digit of minutes
      
      secs=it-hrs*3600_SIK-mins*60_SIK  ! Total number of seconds
      sec1=secs/10_SIK                  ! 10's digit of seconds
      sec2=MOD(secs,10_SIK)             ! 1's digit of seconds
      
      sfrac=t-it
      ! fraction of whole seconds
      IF(sfrac > 0.99_SRK) sfrac=0.99_SRK
      
      IF(hrs > 0_SIK) THEN
        ! round up to next whole second
        !IF(sfrac >= half) sec2=sec2+1_SIK
        IF(sfrac >= half .AND. sec2 < 9) sec2=sec2+1_SIK
        WRITE(hh_mm_ss,'(i2.2,i1.1,2(":",2i1.1))') hr1,hr2,min1,min2,sec1,sec2
      ELSE
        WRITE(hh_mm_ss,'(i2.2,i1.1,":",2i1.1,f3.2)') min1,min2,sec1,sec2,sfrac
      ENDIF
    ENDFUNCTION getTimeHHMMSS
!
!-------------------------------------------------------------------------------
!> @brief Sets the %count attribute of a timer
!> @param myTimer dummy argument of timer to start counting on.
    SUBROUTINE tic(myTimer)
      CLASS(TimerType),INTENT(INOUT) :: myTimer
      INTEGER(SLK) :: count,rate,count_max

      IF(myTimer%HiResTimer) THEN
        CALL SYSTEM_CLOCK(count_hi,rate_hi,count_max_hi)
        count=count_hi
        rate=rate_hi
        count_max=count_max_hi
      ELSE
        CALL SYSTEM_CLOCK(count_lo,rate_lo,count_max_lo)
        count=count_lo
        rate=rate_lo
        count_max=count_max_lo
      ENDIF
      IF(myTimer%count2sec < 0.0_SDK) myTimer%count2sec=1._SDK/REAL(rate,SDK)
      IF(count < myTimer%lastcount) THEN
        myTimer%clockcycleshift=myTimer%clockcycleshift+count_max
      ENDIF
      myTimer%lastcount=count
      myTimer%count=count+myTimer%clockcycleshift
    ENDSUBROUTINE tic
!
!-------------------------------------------------------------------------------
!> @brief Stops counting on a specified timer
!> this time to the total elapsed time of the counter.
!> @param myTimer dummy argument of timer to stop counting on.
!>
!> This routine must be called after @ref Times::tic "tic" or @ref
!> Times::ResetTimer "ResetTimer". If it is not, nothing is done.
    SUBROUTINE toc(myTimer)
      CLASS(TimerType),INTENT(INOUT) :: myTimer
      INTEGER(SLK) :: count,rate,count_max
      
      IF(myTimer%count2sec > 0.0_SDK) THEN
        IF(myTimer%HiResTimer) THEN
          CALL SYSTEM_CLOCK(count_hi,rate_hi,count_max_hi)
          count=count_hi
          rate=rate_hi
          count_max=count_max_hi
        ELSE
          CALL SYSTEM_CLOCK(count_lo,rate_lo,count_max_lo)
          count=count_lo
          rate=rate_lo
          count_max=count_max_lo
        ENDIF
        IF(count < myTimer%lastcount) THEN
          myTimer%clockcycleshift=myTimer%clockcycleshift+count_max
        ENDIF
        myTimer%lastcount=count
        count=count+myTimer%clockcycleshift
        myTimer%elapsedtime=myTimer%elapsedtime+ &
                           REAL((count-myTimer%count),SDK)*myTimer%count2sec
      ENDIF
    ENDSUBROUTINE toc
!
!-------------------------------------------------------------------------------
!> @brief Resets the attributes of a timer.
!> @param myTimer dummy argument of timer to be reset
!>
!> @ref Times::TimerType::name "%name" is cleared, @ref Times::TimerType::count
!> "%count" is reset and @ref Times::TimerType::elapsedtime "%elapsedtime" is 
!> set to 0. @ref Times::TimerType::time "%time" and @ref 
!> Times::TimerType::unit "%unit" are set based on %elapsedtime=0
    SUBROUTINE ResetTimer(myTimer)
      CLASS(TimerType),INTENT(INOUT) :: myTimer

      myTimer%name=''
      myTimer%elapsedtime=0_SDK
      myTimer%count2sec=-1_SDK
      CALL tic(myTimer)
      CALL SetTimeAndUnits(myTimer)
    ENDSUBROUTINE ResetTimer
!
!-------------------------------------------------------------------------------
!> @brief Sets the @ref Times::TimerType::time "%time" and @ref 
!> Times::TimerType::unit "%unit" of timer, @e myTimer
!> @param myTimer dummy argument of the timer
!>
!> This a private routine and is not accessible outisde of the module.
    SUBROUTINE SetTimeAndUnits(myTimer)
      REAL(SRK),PARAMETER :: sec2micsec=1.e-6_SRK
      REAL(SRK),PARAMETER :: sec2msec=1.e-3_SRK
      CLASS(TimerType),INTENT(INOUT) :: myTimer
      REAL(SRK) :: t
      CHARACTER(LEN=9) :: tstring
      CHARACTER(LEN=9) :: tunit

      t=myTimer%elapsedtime
      IF(0._SRK <= t .AND. t < sec2msec) THEN
        tunit=' microsec'
        WRITE(tstring,'(f9.3)') t/sec2micsec
      ELSEIF(sec2msec <= t .AND. t < 1._SRK) THEN
        tunit=' ms      '
        WRITE(tstring,'(f9.3)') t/sec2msec
      ELSEIF(1._SRK <= t .AND. t < 100000._SRK) THEN
        tunit=' s       '
        WRITE(tstring,'(f9.3)') t
      ELSEIF(100000._SRK <= t) THEN
        tunit=' hh:mm:ss'
        tstring=getTimeHHMMSS(myTimer)
      ENDIF
      myTimer%time=tstring
      myTimer%unit=tunit
    ENDSUBROUTINE SetTimeAndUnits
!
ENDMODULE Times
