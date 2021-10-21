!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
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
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Times
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE Strings
USE IO_Strings
IMPLICIT NONE
PRIVATE !Default private for module contents
!
! List of Public items
PUBLIC :: TimerType
PUBLIC :: ParentTimerType
PUBLIC :: TimerPtrArray
PUBLIC :: getDate
PUBLIC :: getClockTime
PUBLIC :: getTimeFromDate
PUBLIC :: MAXLEN_TIME_STRING
PUBLIC :: MAXLEN_DATE_STRING
PUBLIC :: MAXLEN_CLOCK_STRING
PUBLIC :: runTestTimes
INTERFACE
  MODULE SUBROUTINE runTestTimes()
  ENDSUBROUTINE runTestTimes
ENDINTERFACE

!> Maximum length of character string for the reported time
INTEGER(SIK),PARAMETER :: MAXLEN_TIME_STRING=20
!> Maximum length of character string for the reported date
INTEGER(SIK),PARAMETER :: MAXLEN_DATE_STRING=13
!> Maximum length of character string for the elapsed time (private)
INTEGER(SIK),PARAMETER :: MAXLEN_ETIME=11
!> Maximum length of character string for the reported clock time
INTEGER(SIK),PARAMETER :: MAXLEN_CLOCK_STRING=8
!> Maximum length of the time unit string
INTEGER(SIK),PARAMETER :: MAXLEN_TIME_UNITS=9

!> @brief Derived Datatype for the Timer data object
!>
!> This is the data object is used to define default timers within the
!> module for basic things a programmer may want to time. This derived
!> data type is also public so the programmer can create their own timers
!> if they wish for use elsewhere in the code.
TYPE :: TimerType
  !> Descriptive name for the timer
  TYPE(StringType),PRIVATE :: name
  !> Clock cycle count value for start of the timer (set by
  !> @ref Times::tic "tic")
  INTEGER(SLK),PRIVATE :: count=0_SDK
  !> The elapsed time recorded for this timer in seconds.
  REAL(SDK) :: elapsedtime=0_SDK
  !> The time recorded by this timer given as a string.
  CHARACTER(LEN=MAXLEN_ETIME),PRIVATE :: time=''
  !> The units for the time given in @ref Times::TimerType::time
  !> "%time", also a string.
  CHARACTER(LEN=MAXLEN_TIME_UNITS),PRIVATE :: unit=''
  !> indicates if the timer is high resolution
  LOGICAL(SBK),PRIVATE :: HiResTimer=.TRUE.
  !> Convert the processor clock count value to a time in seconds.
  REAL(SDK),PRIVATE :: count2sec=-1._SDK
  !> The last count value taken from SYSTEM_CLOCK
  !> Needed to determine if the clock counter rolled over
  INTEGER(SLK),PRIVATE :: lastcount=0_SLK
  !> The number of counts to shift due to clock cycle rollover.
  !> Add count_max each time the clock rolls over.
  INTEGER(SLK),PRIVATE :: clockcycleshift=0_SLK
  !
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
    !> @copydetails Times::getTimeReal
    PROCEDURE,PASS :: getTimeReal
    !> @copybrief Times::getTimeChar
    !> @copydetails Times::getTimeChar
    PROCEDURE,PASS :: getTimeChar
ENDTYPE TimerType

!> @brief Wrapper object for an allocatable array of pointers to timers
TYPE :: TimerPtrArray
  !> Pointer to the timer
  CLASS(TimerType),POINTER :: t => NULL()
ENDTYPE TimerPtrArray

!> @brief defines a parent timer which owns sub-timers
TYPE,EXTENDS(TimerType) :: ParentTimerType
  !> The sub-timers owned by this timer
  TYPE(TimerPtrArray),ALLOCATABLE :: timers(:)
  CONTAINS
    !> @copybrief Times::addTimer
    !> @copydetails Times::addTimer
    PROCEDURE,PASS :: addTimer
    !> @copybrief Times::getTimer
    !> @copydetails Times::getTimer
    PROCEDURE,PASS :: getTimer
    !> @copybrief Times::clearParentTimer
    !> @copydetails Times::clearParentTimer
    PROCEDURE,PASS :: clear => clearParentTimer
    !> @copybrief Times::getTimerResolution_Parent
    !> @copydetails Times::getTimerResolution_Parent
    PROCEDURE,PASS :: getTimerResolution => getTimerResolution_Parent
    !> @copybrief Times::getRemainingTime_Parent
    !> @copydetails Times::getRemainingTime_Parent
    PROCEDURE,PASS :: getRemainingTime => getRemainingTime_Parent
    !> @copybrief Times::ResetTimer_Parent
    !> @copydetails Times::ResetTimer_Parent
    PROCEDURE,PASS :: ResetTimer => ResetTimer_Parent
    !> @copybrief Times::setTimerHiResMode_Parent
    !> @copydetails Times::setTimerHiResMode_Parent
    PROCEDURE,PASS :: setTimerHiResMode => setTimerHiResMode_Parent
    !> @copybrief Times::tic_Parent
    !> @copydetails Times::tic_Parent
    PROCEDURE,PASS :: tic => tic_Parent
    !> @copybrief Times::toc_Parent
    !> @copydetails Times::toc_Parent
    PROCEDURE,PASS :: toc => toc_Parent
    !> @copybrief Times::getTimeHHMMSS_Parent
    !> @copydetails Times::getTimeHHMMSS_Parent
    PROCEDURE,PASS :: getTimeHHMMSS => getTimeHHMMSS_Parent
    !> @copybrief Times::getTimeReal_Parent
    !> @copydetails Times::getTimeReal_Parent
    PROCEDURE,PASS :: getTimeReal => getTimeReal_Parent
    FINAL :: finalize_parentTimertype
ENDTYPE ParentTimerType

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
FUNCTION getTimerResolution(this) RESULT(tres)
  CLASS(TimerType),INTENT(INOUT) :: this
  REAL(SDK) :: tres

  IF(this%HiResTimer) THEN
    CALL SYSTEM_CLOCK(count_hi,rate_hi,count_max_hi)
    this%lastcount=count_hi
    tres=1000000._SDK/REAL(rate_hi,SDK)
  ELSE
    CALL SYSTEM_CLOCK(count_lo,rate_lo,count_max_lo)
    this%lastcount=count_lo
    tres=1000000._SDK/REAL(rate_lo,SDK)
  ENDIF
ENDFUNCTION getTimerResolution
!
!-------------------------------------------------------------------------------
!> @brief Function returns the time remaining before the clock counter rolls
!> over.
!>
!> The result is returned as a double precision real.
FUNCTION getRemainingTime(this) RESULT(tremain)
  CLASS(TimerType),INTENT(INOUT) :: this
  REAL(SDK) :: tremain

  IF(this%HiResTimer) THEN
    CALL SYSTEM_CLOCK(count_hi,rate_hi,count_max_hi)
    this%lastcount=count_hi
    tremain=REAL((count_max_hi-count_hi),SDK)/REAL(rate_hi,SDK)
  ELSE
    CALL SYSTEM_CLOCK(count_lo,rate_lo,count_max_lo)
    this%lastcount=count_lo
    tremain=REAL((count_max_lo-count_lo),SDK)/REAL(rate_lo,SDK)
  ENDIF
ENDFUNCTION getRemainingTime

!
!-------------------------------------------------------------------------------
!> @brief Set the name of a timer @e this
!> @param this dummy argument of timer to modify the name of
!> @param name input argument, name to assign to the timer
!>
!> Names must be 20 characters or less.
SUBROUTINE setTimerName(this,name)
  CLASS(TimerType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  this%name=TRIM(name)
ENDSUBROUTINE setTimerName
!
!-------------------------------------------------------------------------------
!> @brief Get the name of a timer @e this
!> @param this dummy argument of timer to return the name of
!> @returns name input argument, name output from timer
!>
!> Names are 20 characters or less.
FUNCTION getTimerName(this) RESULT(name)
  CLASS(TimerType),INTENT(IN) :: this
  CHARACTER(LEN=:),ALLOCATABLE :: name
  name=CHAR(this%name)
ENDFUNCTION getTimerName
!
!-------------------------------------------------------------------------------
!> @brief Set the mode of resolution of a timer @e this
!> @param this dummy argument of timer to modify the name of
!> @param resMode the timer resolution mode TRUE=HI-RES, FALSE=LO-RES
SUBROUTINE setTimerHiResMode(this,resMode)
  CLASS(TimerType),INTENT(INOUT) :: this
  LOGICAL(SBK),INTENT(IN) :: resMode
  this%HiResTimer=resMode
  CALL this%ResetTimer()
ENDSUBROUTINE setTimerHiResMode
!
!-------------------------------------------------------------------------------
!> @brief Get the mode of resolution of a timer @e this
!> @param this dummy argument of timer to return the name of
!> @returns tmode the timer resolution mode TRUE=HI-RES, FALSE=LO-RES
FUNCTION getTimerHiResMode(this) RESULT(tmode)
  CLASS(TimerType),INTENT(IN) :: this
  LOGICAL(SBK) :: tmode
  tmode=this%HiResTimer
ENDFUNCTION getTimerHiResMode
!
!-------------------------------------------------------------------------------
!> @brief Function returns the value of @ref Times::TimerType::elapsedtime
!> "%elapsedtime" as a real type.
!> @param this input argument, a @ref Times::TimerType "TimerType" variable
!> @returns time, the elapsed time (REAL type) unit is seconds
FUNCTION getTimeReal(this) RESULT(time)
  CLASS(TimerType),INTENT(IN) :: this
  REAL(SRK) :: time
  time=this%elapsedtime
ENDFUNCTION getTimeReal
!
!-------------------------------------------------------------------------------
!> @brief Subroutine returns the value of @ref Times::TimerType::elapsedtime
!> "%elapsedtime" as a string with units.
!> @param this input argument, a @ref Times::TimerType "TimerType" variable
!> @returns time output argument, the elapsed time as a string
FUNCTION getTimeChar(this) RESULT(time)
  CLASS(TimerType),INTENT(INOUT) :: this
  CHARACTER(LEN=MAXLEN_TIME_STRING) :: time

  CALL SetTimeAndUnits(this)
  time=TRIM(this%time)//TRIM(this%unit)
ENDFUNCTION getTimeChar
!
!-------------------------------------------------------------------------------
!> @brief Function returns the value of @ref Times::TimerType::elapsedtime
!> "%elapsedtime" as a string in HHMMSS format.
!> @param this input argument, a @ref Times::TimerType "TimerType" variable
!> @returns hh_mm_ss, the elapsed time as a string (hhh:mm:ss or mmm:ss.ss)
FUNCTION getTimeHHMMSS(this,tsec,force_hour) RESULT(hh_mm_ss)
  CLASS(TimerType),INTENT(IN) :: this
  REAL(SRK),INTENT(IN),OPTIONAL :: tsec
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: force_hour
  CHARACTER(LEN=MAXLEN_ETIME) :: hh_mm_ss
  LOGICAL(SBK) :: force_hours
  INTEGER(SIK) :: it,hrs,mins
  REAL(SRK) :: t,secs

  IF(PRESENT(tsec)) THEN
    t=tsec
  ELSE
    t=this%elapsedtime
  ENDIF
  force_hours=.FALSE.
  IF(PRESENT(force_hour)) force_hours=force_hour

  it=INT(t,SIK)
  hrs=it/3600_SIK                   ! Total number of hours
  mins=(it-hrs*3600_SIK)/60_SIK     ! Total number of minutes
  secs=t-REAL(hrs*3600+mins*60_SIK,SRK)

  IF(hrs > 0_SIK .OR. force_hours) THEN
    IF(secs < 9.995_SRK) THEN
      WRITE(hh_mm_ss,'(a,":",a,":0",f4.2)') str(hrs,2),str(mins,2),secs
    ELSE
      WRITE(hh_mm_ss,'(a,":",a,":",f5.2)') str(hrs,2),str(mins,2),secs
    ENDIF
  ELSE
    IF(secs < 9.995_SRK) THEN
      WRITE(hh_mm_ss,'(a,":0",f4.2)') str(mins,2),secs
    ELSE
      WRITE(hh_mm_ss,'(a,":",f5.2)') str(mins,2),secs
    ENDIF
  ENDIF
ENDFUNCTION getTimeHHMMSS
!
!-------------------------------------------------------------------------------
!> @brief Sets the %count attribute of a timer
!> @param this dummy argument of timer to start counting on.
SUBROUTINE tic(this)
  CLASS(TimerType),INTENT(INOUT) :: this
  INTEGER(SLK) :: count,rate,count_max

  IF(this%HiResTimer) THEN
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
  IF(this%count2sec < 0.0_SDK) this%count2sec=1._SDK/REAL(rate,SDK)
  IF(count < this%lastcount) THEN
    this%clockcycleshift=this%clockcycleshift+count_max
  ENDIF
  this%lastcount=count
  this%count=count+this%clockcycleshift
ENDSUBROUTINE tic
!
!-------------------------------------------------------------------------------
!> @brief Stops counting on a specified timer
!> this time to the total elapsed time of the counter.
!> @param this dummy argument of timer to stop counting on.
!>
!> This routine must be called after @ref Times::tic "tic" or @ref
!> Times::ResetTimer "ResetTimer". If it is not, nothing is done.
SUBROUTINE toc(this)
  CLASS(TimerType),INTENT(INOUT) :: this
  INTEGER(SLK) :: count,rate,count_max

  IF(this%count2sec > 0.0_SDK) THEN
    IF(this%HiResTimer) THEN
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
    IF(count < this%lastcount) THEN
      this%clockcycleshift=this%clockcycleshift+count_max
    ENDIF
    this%lastcount=count
    count=count+this%clockcycleshift
    this%elapsedtime=this%elapsedtime+ &
        REAL((count-this%count),SDK)*this%count2sec
  ENDIF
ENDSUBROUTINE toc
!
!-------------------------------------------------------------------------------
!> @brief Resets the attributes of a timer.
!> @param this dummy argument of timer to be reset
!>
!> @ref Times::TimerType::name "%name" is cleared, @ref Times::TimerType::count
!> "%count" is reset and @ref Times::TimerType::elapsedtime "%elapsedtime" is
!> set to 0. @ref Times::TimerType::time "%time" and @ref
!> Times::TimerType::unit "%unit" are set based on %elapsedtime=0
SUBROUTINE ResetTimer(this)
  CLASS(TimerType),INTENT(INOUT) :: this

  this%name=''
  this%elapsedtime=0_SDK
  this%count2sec=-1_SDK
  CALL tic(this)
  CALL SetTimeAndUnits(this)
ENDSUBROUTINE ResetTimer
!
!-------------------------------------------------------------------------------
!> @brief Sets the @ref Times::TimerType::time "%time" and @ref
!> Times::TimerType::unit "%unit" of timer, @e this
!> @param this dummy argument of the timer
!>
!> This a private routine and is not accessible outisde of the module.
SUBROUTINE SetTimeAndUnits(this)
  REAL(SRK),PARAMETER :: sec2micsec=1.e-6_SRK
  REAL(SRK),PARAMETER :: sec2msec=1.e-3_SRK
  CLASS(TimerType),INTENT(INOUT) :: this
  REAL(SRK) :: t
  CHARACTER(LEN=MAXLEN_ETIME) :: tstring
  CHARACTER(LEN=MAXLEN_TIME_UNITS) :: tunit

  t=this%elapsedtime
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
    tstring=getTimeHHMMSS(this)
  ENDIF
  this%time=tstring
  this%unit=tunit
ENDSUBROUTINE SetTimeAndUnits
!
!-------------------------------------------------------------------------------
!> @brief
FUNCTION getTimeFromDate(Date1_inp,Date2_inp,outputunit_inp,fmt1_inp,fmt2_inp) RESULT(time)
  CHARACTER(LEN=*),INTENT(IN) :: Date1_inp
  CHARACTER(LEN=*),INTENT(IN) :: Date2_inp
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: outputunit_inp
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: fmt1_inp
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: fmt2_inp

  REAL(SRK) :: time
  INTEGER(SIK) :: Date1year,Date1month,Date1day,Date2year,Date2month,Date2day
  INTEGER(SIK) :: i,ind1,ind2,total1,total2,leapdays,tmpint
  !> The number of days per month.
  INTEGER(SIK) :: dayspermonth(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  CHARACTER(LEN=LEN(Date1_inp)) :: Date1
  CHARACTER(LEN=LEN(Date2_inp)) :: Date2
  CHARACTER(LEN=10) :: tmpdate
  TYPE(StringType) :: outputunit,fmt1,fmt2

  time=0.0_SRK
  IF(LEN_TRIM(Date1_inp) > 0 .AND. LEN_TRIM(Date2_inp) > 0) THEN
    !Set up Default Formats
    ind1=INDEX(Date1_inp,FSLASH)
    !Has a slash
    IF(ind1 > 0) THEN
      !Front part is the year
      IF(ind1 == 5) THEN
        IF(LEN_TRIM(Date1_inp) == 10) THEN
          fmt1='YYYY/MM/DD'
        ELSEIF(LEN_TRIM(Date1_inp) == 8) THEN
          fmt1='YYYY/M/D'
        ELSE
          IF(INDEX(Date1_inp,FSLASH,.TRUE.) == 8) THEN
            fmt1='YYYY/MM/D'
          ELSE
            fmt1='YYYY/M/DD'
          ENDIF
        ENDIF
      ELSE
        IF(LEN_TRIM(Date1_inp) == 10) THEN
          fmt1='MM/DD/YYYY'
        ELSEIF(LEN_TRIM(Date1_inp) == 8) THEN
          fmt1='M/D/YYYY'
        ELSE
          IF(INDEX(Date1_inp,FSLASH) == 3) THEN
            fmt1='MM/D/YYYY'
          ELSE
            fmt1='M/DD/YYYY'
          ENDIF
        ENDIF
      ENDIF
    !No slash, so the easy logical check is which set of 4 is greater than 1231 to find the year.
    !Not all enclusive.
    ELSE
      tmpdate=Date1_inp
      READ(tmpdate,'(i8)')  tmpint
      !Last 4 digits are the year
      IF(MOD(tmpint,10000) > 1231) THEN
        fmt1='MMDDYYYY'
      ELSE
        fmt1='YYYYMMDD'
      ENDIF
    ENDIF

    ind1=INDEX(Date2_inp,FSLASH)
    !Has a slash
    IF(ind1 > 0) THEN
      !Front part is the year
      IF(ind1 == 5) THEN
        IF(LEN_TRIM(Date2_inp) == 10) THEN
          fmt2='YYYY/MM/DD'
        ELSEIF(LEN_TRIM(Date2_inp) == 8) THEN
          fmt2='YYYY/M/D'
        ELSE
          IF(INDEX(Date2_inp,FSLASH,.TRUE.) == 8) THEN
            fmt2='YYYY/MM/D'
          ELSE
            fmt2='YYYY/M/DD'
          ENDIF
        ENDIF
      ELSE
        IF(LEN_TRIM(Date2_inp) == 10) THEN
          fmt2='MM/DD/YYYY'
        ELSEIF(LEN_TRIM(Date2_inp) == 8) THEN
          fmt2='M/D/YYYY'
        ELSE
          IF(INDEX(Date2_inp,FSLASH) == 3) THEN
            fmt2='MM/D/YYYY'
          ELSE
            fmt2='M/DD/YYYY'
          ENDIF
        ENDIF
      ENDIF
    !No slash, so the easy logical check is which set of 4 is greater than 1231 to find the year.
    !Not all enclusive.
    ELSE
      tmpdate=Date2_inp
      READ(tmpdate,'(i8)') tmpint
      !Last 4 digits are the year
      IF(MOD(tmpint,10000) > 1231) THEN
        fmt2='MMDDYYYY'
      ELSE
        fmt2='YYYYMMDD'
      ENDIF
    ENDIF

    !Set the optional inputs if they are present
    Date1=Date1_inp
    Date2=Date2_inp
    outputunit='DAY'
    IF(PRESENT(outputunit_inp)) THEN
      IF(outputunit_inp == 'HOUR' .OR. outputunit_inp == 'MIN' .OR. &
          outputunit_inp == 'SEC') outputunit=outputunit_inp
    ENDIF
    IF(PRESENT(fmt1_inp)) THEN
      IF((INDEX(fmt1_inp,"Y") > 0) .AND. (INDEX(fmt1_inp,"M") > 0) .AND. &
          (INDEX(fmt1_inp,"D") > 0)) fmt1=fmt1_inp
    ENDIF
    IF(PRESENT(fmt2_inp)) THEN
      IF((INDEX(fmt2_inp,"Y") > 0) .AND. (INDEX(fmt2_inp,"M") > 0) .AND. &
          (INDEX(fmt2_inp,"D") > 0)) fmt2=fmt2_inp
    ENDIF

    !Process the strings
    ind1=INDEX(fmt1,'Y')
    ind2=INDEX(fmt1,'Y',.TRUE.)
    READ(Date1(ind1:ind2),'(i4)') Date1year
    ind1=INDEX(fmt1,'M')
    ind2=INDEX(fmt1,'M',.TRUE.)
    READ(Date1(ind1:ind2),'(i2)') Date1month
    ind1=INDEX(fmt1,'D')
    ind2=INDEX(fmt1,'D',.TRUE.)
    READ(Date1(ind1:ind2),'(i2)') Date1day
    ind1=INDEX(fmt2,'Y')
    ind2=INDEX(fmt2,'Y',.TRUE.)
    READ(Date2(ind1:ind2),'(i4)') Date2year
    ind1=INDEX(fmt2,'M')
    ind2=INDEX(fmt2,'M',.TRUE.)
    READ(Date2(ind1:ind2),'(i2)') Date2month
    ind1=INDEX(fmt2,'D')
    ind2=INDEX(fmt2,'D',.TRUE.)
    READ(Date2(ind1:ind2),'(i2)') Date2day

    !Convert years and months to days from 0.
    !Get the number of leap days
    leapdays=countleapyears(0,1,1,Date1year,Date1month,Date1day)
    !We may need to adjust the leap days depending if the date
    !is on an actual leap day...
    !IF(Date1month == 2 .AND. Date1day == 29)
    total1=0
    DO i=1,Date1month-1
      total1=total1+dayspermonth(i)
    ENDDO
    total1=Date1year*365+leapdays+total1+Date1day

    leapdays=countleapyears(0,1,1,Date2year,Date2month,Date2day)
    total2=0
    DO i=1,Date2month-1
      total2=total2+dayspermonth(i)
    ENDDO
    total2=Date2year*365+leapdays+total2+Date2day

    time=REAL(total2-total1,SRK)

    IF(outputunit == 'HOUR') THEN
      time=time*24.0_SRK
    ELSEIF(outputunit == 'MIN') THEN
      time=time*1440.0_SRK
    ELSEIF(outputunit == 'SEC') THEN
      time=time*86400.0_SRK
    ENDIF
  ENDIF
ENDFUNCTION getTimeFromDate
!
!-------------------------------------------------------------------------------
!> @brief Routine that calculates the number of leap days between two years
!> @param yearstt Starting year
!> @param yearstp Stopping year
!> @param ndays resulting number of leap days between the two years.
!>
FUNCTION countleapyears(yearstt,monthstt,daystt,yearstp,monthstp,daystp) RESULT(ndays)
  INTEGER(SIK),INTENT(IN) :: yearstt
  INTEGER(SIK),INTENT(IN) :: monthstt
  INTEGER(SIK),INTENT(IN) :: daystt
  INTEGER(SIK),INTENT(IN) :: yearstp
  INTEGER(SIK),INTENT(IN) :: monthstp
  INTEGER(SIK),INTENT(IN) :: daystp
  INTEGER(SIK) :: ndays

  INTEGER(SIK) :: i
  !Avoids unsed dumy argument warning
  i=daystt
  ndays=0
  !Count the number of leap years from 0
  DO i=yearstt,yearstp
    IF(i == yearstt .AND. monthstt > 2) CYCLE
    IF(i == yearstp .AND. (monthstp < 2 .OR. (monthstp == 2 .AND. daystp <= 28))) CYCLE
    IF((MOD(i,4) == 0) .AND. (MOD(i,100) /= 0)) THEN
      ndays=ndays+1
    ELSEIF((MOD(i,4) == 0) .AND. (MOD(i,100) == 0) .AND. MOD(i,400) == 0) THEN
      ndays=ndays+1
    ENDIF
  ENDDO
ENDFUNCTION countleapyears
!
!-------------------------------------------------------------------------------
!> @brief Function returns the timer resolution of the system_clock on the
!> current machine.
!>
!> The timer resolution is given in units of microseconds as a double precision
!> real value.
FUNCTION getTimerResolution_Parent(this) RESULT(tres)
  CLASS(ParentTimerType),INTENT(INOUT) :: this
  REAL(SDK) :: tres
  !
  INTEGER(SIK) :: i

  REQUIRE(ALLOCATED(this%timers))
  REQUIRE(SIZE(this%timers) > 0)

  DO i=1,SIZE(this%timers)
    IF(.NOT.this%timers(i)%t%HiResTimer .OR. i == SIZE(this%timers)) THEN
      tres=this%timers(i)%t%getTimerResolution()
      EXIT
    ENDIF
  ENDDO !i

ENDFUNCTION getTimerResolution_Parent
!
!-------------------------------------------------------------------------------
!> @brief Function returns the time remaining before the clock counter rolls
!> over.
!>
!> The result is returned as a double precision real.
FUNCTION getRemainingTime_Parent(this) RESULT(tremain)
  CLASS(ParentTimerType),INTENT(INOUT) :: this
  REAL(SDK) :: tremain
  !
  CHARACTER(LEN=*),PARAMETER :: myName='getRemainingTime_Parent'

  tremain=0.0_SRK
  STOP "Function"//myName//" should not be called on a parent timer!"

ENDFUNCTION getRemainingTime_Parent
!
!-------------------------------------------------------------------------------
!> @brief Set the mode of resolution of a timer @e this
!> @param this dummy argument of timer to modify the name of
!> @param resMode the timer resolution mode TRUE=HI-RES, FALSE=LO-RES
SUBROUTINE setTimerHiResMode_Parent(this,resMode)
  CLASS(ParentTimerType),INTENT(INOUT) :: this
  LOGICAL(SBK),INTENT(IN) :: resMode
  !
  INTEGER(SIK) :: i

  IF(ALLOCATED(this%timers)) THEN
    DO i=1,SIZE(this%timers)
      CALL this%timers(i)%t%setTimerHiResMode(resMode)
    ENDDO !i
  ENDIF
  CALL this%TimerType%setTimerHiResMode(resMode)

ENDSUBROUTINE setTimerHiResMode_Parent
!
!-------------------------------------------------------------------------------
!> @brief Sets the %count attribute of a timer
!> @param this dummy argument of timer to start counting on.
SUBROUTINE tic_Parent(this)
  CLASS(ParentTimerType),INTENT(INOUT) :: this
  !
  CHARACTER(LEN=*),PARAMETER :: myName='tic_Parent'

  STOP "Function "//myName//" should not be called on a parent timer!"

ENDSUBROUTINE tic_Parent
!
!-------------------------------------------------------------------------------
!> @brief Stops counting on a specified timer
!> this time to the total elapsed time of the counter.
!> @param this dummy argument of timer to stop counting on.
!>
!> This routine must be called after @ref Times::tic "tic" or @ref
!> Times::ResetTimer "ResetTimer". If it is not, nothing is done.
SUBROUTINE toc_Parent(this)
  CLASS(ParentTimerType),INTENT(INOUT) :: this
  !
  INTEGER(SLK) :: i

  REQUIRE(ALLOCATED(this%timers))
  REQUIRE(SIZE(this%timers) > 0)

  DO i=1,SIZE(this%timers)
    CALL this%timers(i)%t%toc()
  ENDDO !i
  this%elapsedtime=this%getTimeReal()

ENDSUBROUTINE toc_Parent
!
!-------------------------------------------------------------------------------
!> @brief Function returns the value of @ref Times::TimerType::elapsedtime
!> "%elapsedtime" as a real type.
!> @param this input argument, a @ref Times::TimerType "TimerType" variable
!> @returns time, the elapsed time (REAL type) unit is seconds
FUNCTION getTimeReal_Parent(this) RESULT(time)
  CLASS(ParentTimerType),INTENT(IN) :: this
  REAL(SRK) :: time
  !
  INTEGER(SIK) :: i

  REQUIRE(ALLOCATED(this%timers))
  REQUIRE(SIZE(this%timers) > 0)

  time=0.0_SRK
  DO i=1,SIZE(this%timers)
    time=time+this%timers(i)%t%getTimeReal()
  ENDDO !i

ENDFUNCTION getTimeReal_Parent
!
!-------------------------------------------------------------------------------
!> @brief Function returns the value of @ref Times::TimerType::elapsedtime
!> "%elapsedtime" as a string in HHMMSS format.
!> @param this input argument, a @ref Times::TimerType "TimerType" variable
!> @returns hh_mm_ss, the elapsed time as a string (hhh:mm:ss or mmm:ss.ss)
FUNCTION getTimeHHMMSS_Parent(this,tsec,force_hour) RESULT(hh_mm_ss)
  CLASS(ParentTimerType),INTENT(IN) :: this
  REAL(SRK),INTENT(IN),OPTIONAL :: tsec
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: force_hour
  CHARACTER(LEN=MAXLEN_ETIME) :: hh_mm_ss

  REQUIRE(ALLOCATED(this%timers))
  REQUIRE(SIZE(this%timers) > 0)

  IF(PRESENT(tsec)) THEN
    hh_mm_ss=this%TimerType%getTimeHHMMSS(tsec,force_hour)
  ELSE
    hh_mm_ss=this%TimerType%getTimeHHMMSS(this%getTimeReal(),force_hour)
  ENDIF

ENDFUNCTION getTimeHHMMSS_Parent
!
!-------------------------------------------------------------------------------
!> @brief Resets the attributes of a timer.
!> @param this dummy argument of timer to be reset
!>
!> @ref Times::TimerType::name "%name" is cleared, @ref Times::TimerType::count
!> "%count" is reset and @ref Times::TimerType::elapsedtime "%elapsedtime" is
!> set to 0. @ref Times::TimerType::time "%time" and @ref
!> Times::TimerType::unit "%unit" are set based on %elapsedtime=0
SUBROUTINE ResetTimer_Parent(this)
  CLASS(ParentTimerType),INTENT(INOUT) :: this
  !
  INTEGER(SIK) :: i

  IF(ALLOCATED(this%timers)) THEN
    DO i=1,SIZE(this%timers)
      CALL this%timers(i)%t%resetTimer()
    ENDDO !i
  ENDIF
  CALL this%TimerType%resetTimer()

ENDSUBROUTINE ResetTimer_Parent
!
!-------------------------------------------------------------------------------
!> @brief Clears a @c ParentTimerType object
!> @param this the object to clear
!>
RECURSIVE SUBROUTINE clearParentTimer(this)
  CLASS(ParentTimerType),INTENT(INOUT) :: this
  !
  INTEGER(SIK) :: i

  IF(ALLOCATED(this%timers)) THEN
    DO i=1,SIZE(this%timers)
      CALL this%timers(i)%t%ResetTimer()
      DEALLOCATE(this%timers(i)%t)
    ENDDO
    DEALLOCATE(this%timers)
  ENDIF
  CALL this%ResetTimer()

ENDSUBROUTINE clearParentTimer
!
!-------------------------------------------------------------------------------
!> @brief Finalization routine to clear a @c ParentTimerType when it goes out of scope
!> @param this the object to finalize
RECURSIVE IMPURE SUBROUTINE finalize_parentTimerType(this)
  TYPE(ParentTimerType),INTENT(INOUT) :: this

  CALL this%clear()

ENDSUBROUTINE finalize_parentTimerType
!
!-------------------------------------------------------------------------------
!> @brief Function to get a sub-timer from a @c ParentTimerType object
!> @param this the object to query
!> @param name the name of the sub-timer to retrieve
!> @returns timer the pointer to the sub-timers
!>
!> If there are multiple levels of parent timers, "->" is used as a delimiter.
!>
RECURSIVE FUNCTION getTimer(this,name) RESULT(timer)
  CLASS(ParentTimerType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  CLASS(Timertype),POINTER :: timer
  !
  INTEGER(SIK) :: i
  TYPE(StringType) :: timername
  TYPE(StringType),ALLOCATABLE :: timernames(:)

  timername=name
  timernames=timername%split('->')
  DO i=1,SIZE(timernames)
    timernames(i)=TRIM(ADJUSTL(timernames(i)))
  ENDDO !i

  timer => NULL()
  DO i=1,SIZE(this%timers)
    IF(this%timers(i)%t%getTimerName() == timernames(1)) THEN
      IF(SIZE(timernames) > 1) THEN
        SELECTTYPE(timerptr => this%timers(i)%t)
        TYPE IS(ParentTimerType)
          timer => timerptr%getTimer(ADJUSTL(timername%substr(INDEX(timername,'->')+2)))
        TYPE IS(TimerType)
          STOP "Timer "//timernames(1)//" is not a parent timer and cannot match "//name
        ENDSELECT
      ELSE
        timer => this%timers(i)%t
      ENDIF
    ENDIF
  ENDDO !i

ENDFUNCTION getTimer
!
!-------------------------------------------------------------------------------
!> @brief Function to add a sub-timer to a @c ParentTimerType object
!> @param this the object to add to
!> @param name the name of the sub-timer to add
!>
!> If there are multiple levels of parent timers, "->" is used as a delimiter.
!>
RECURSIVE SUBROUTINE addTimer(this,name)
  CLASS(ParentTimerType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  !
  INTEGER(SIK) :: i
  TYPE(StringType) :: timername
  TYPE(StringType),ALLOCATABLE :: timernames(:)
  TYPE(TimerPtrArray),ALLOCATABLE :: oldtimers(:)

  IF(.NOT.ALLOCATED(this%timers)) THEN
    ALLOCATE(this%timers(0))
  ENDIF

  !See if the timer name matches a parent timer
  timername=name
  timernames=timername%split('->')
  DO i=1,SIZE(timernames)
    timernames(i)=TRIM(ADJUSTL(timernames(i)))
  ENDDO !i

  DO i=1,SIZE(this%timers)
    IF(this%timers(i)%t%getTimerName() == timernames(1)) THEN
      IF(SIZE(timernames) > 1) THEN
        SELECTTYPE(timer => this%timers(i)%t)
        TYPE IS(ParentTimerType)
          CALL timer%addTimer(ADJUSTL(timername%substr(INDEX(timername,'->')+2)))
        TYPE IS(TimerType)
          STOP "Cannot add sub-timer "//name//" to non-parent timer "//timernames(1)
        ENDSELECT
      ELSE
        CONTINUE !Just do nothing for now if the timer already exists
      ENDIF
      RETURN
    ENDIF
  ENDDO !i

  !Set the new timer
  CALL MOVE_ALLOC(this%timers,oldtimers)
  ALLOCATE(this%timers(SIZE(oldtimers)+1))
  DO i=1,SIZE(oldtimers)
    this%timers(i)%t => oldtimers(i)%t
  ENDDO
  IF(SIZE(timernames) > 1) THEN
    ALLOCATE(ParentTimerType :: this%timers(i)%t)
    CALL this%timers(i)%t%setTimerHiResMode(this%getTimerHiResMode())
    SELECTTYPE(timer => this%timers(i)%t); TYPE IS(ParentTimerType)
      CALL timer%addTimer(ADJUSTL(timername%substr(INDEX(timername,'->')+2)))
    ENDSELECT
  ELSE
    ALLOCATE(this%timers(i)%t)
    CALL this%timers(i)%t%setTimerHiResMode(this%getTimerHiResMode())
  ENDIF
  CALL this%timers(i)%t%setTimerName(CHAR(timernames(1)))
  DEALLOCATE(oldtimers)

ENDSUBROUTINE addTimer
!
ENDMODULE Times
