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
USE IntrType
USE Strings
USE IO_Strings
IMPLICIT NONE
PRIVATE !Default private for module contents
!
! List of Public items
PUBLIC :: TimerType
PUBLIC :: TimerPtrArray
PUBLIC :: getDate
PUBLIC :: getClockTime
PUBLIC :: getTimeFromDate
PUBLIC :: MAXLEN_TIME_STRING
PUBLIC :: MAXLEN_DATE_STRING
PUBLIC :: MAXLEN_CLOCK_STRING

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
  !> @brief Descriptive name for the timer
  TYPE(StringType),PRIVATE :: name
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
  CHARACTER(LEN=:),ALLOCATABLE :: name
  name=CHAR(myTimer%name)
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
FUNCTION getTimeHHMMSS(myTimer,tsec) RESULT(hh_mm_ss)
  REAL(SDK),PARAMETER :: half=0.5_SDK
  CLASS(TimerType),INTENT(IN) :: myTimer
  REAL(SRK),INTENT(IN),OPTIONAL :: tsec
  CHARACTER(LEN=MAXLEN_ETIME) :: hh_mm_ss
  INTEGER(SIK) :: it,hrs,hr1,hr2,mins,min1,min2,secs,sec1,sec2
  REAL(SRK) :: t,sfrac
!
!hhmmss='mmm:ss.ss'
!hhmmss='hhh:mm:ss'
  IF(PRESENT(tsec)) THEN
    t=tsec
  ELSE
    t=myTimer%elapsedtime
  ENDIF

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
    WRITE(hh_mm_ss,'(i2,i1.1,2(":",2i1.1))') hr1,hr2,min1,min2,sec1,sec2
  ELSE
    WRITE(hh_mm_ss,'(i2,i1.1,":",2i1.1,f3.2)') min1,min2,sec1,sec2,sfrac
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
ENDMODULE Times
