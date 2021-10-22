!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
SUBMODULE(Times) testTimesSubmodule
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE Times

IMPLICIT NONE

TYPE(TimerType) :: testTimer

INTEGER :: idum1,idum2,idum3,ioerr
CHARACTER(LEN=1) :: adum1,adum2
CHARACTER(LEN=5) :: adum3
CHARACTER(LEN=2) :: adum4
CHARACTER(LEN=MAXLEN_DATE_STRING) :: adate
CHARACTER(LEN=MAXLEN_CLOCK_STRING) :: aclock
REAL(SRK) :: totalElapsed
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE runTestTimes()

  CREATE_TEST('Timers')

  REGISTER_SUBTEST('Basic Timers',testTimers)
  REGISTER_SUBTEST('Parent Timers',testParentTimers)

  FINALIZE_TEST()

ENDSUBROUTINE runTestTimes
!
!-------------------------------------------------------------------------------
SUBROUTINE testTimers()

  COMPONENT_TEST('getDate()')
  !
  !Test getDate()
  adate=getDate()
  idum1=0
  idum2=0
  idum3=0
  READ(adate,'(i2,a1,i2,a1,i4)',iostat=ioerr) idum1,adum1,idum2,adum2,idum3
  ASSERT_EQ(ioerr,0,'ioerr')
  ASSERT_GT(idum1,0,'month')
  ASSERT_LT(idum1,13,'month')
  ASSERT_GT(idum2,0,'day')
  ASSERT_LT(idum2,32,'day')
  ASSERT_GT(idum3,0,'year')
  ASSERT_EQ(adum1,adum2,'adum')
  ASSERT_EQ(adum1,'/','adum1')
  INFO(0) 'getDate() = '//getDate()
  ASSERT_EQ(getDate(),getDate(1),'getDate(1)')

  idum1=0
  idum2=0
  adate=getDate(2)
  READ(adate,'(a5,i2,a2,i4)',iostat=ioerr) adum3,idum1,adum4,idum2
  ASSERT_EQ(ioerr,0,'ioerr')
  ASSERT_GT(idum1,0,'day')
  ASSERT_LT(idum1,32,'day')
  ASSERT_GT(idum2,0,'year')
  ASSERT_EQ(adum4(1:1),',','month')

  !Test getTimeFromDate
  COMPONENT_TEST('getTimeFromDate')
  ASSERT_APPROXEQA(getTimeFromDate('12/01/1990','12/02/1990'),1.0_SRK,'check Defaults')
  ASSERT_APPROXEQA(getTimeFromDate('12/1/1990','12/2/1990','HOUR'),24.0_SRK,'check HOUR and MM/D/YYYY fmt')
  ASSERT_APPROXEQA(getTimeFromDate('1/1/1990','1/2/1990','MIN'),1440.0_SRK,'check MIN and M/D/YYYY fmt')
  ASSERT_APPROXEQA(getTimeFromDate('1/10/1990','1/11/1990','SEC'),86400.0_SRK,'check SEC and M/DD/YYYY fmt')
  ASSERT_APPROXEQA(getTimeFromDate('1990/12/01','1994/12/01','DAY'),1461.0_SRK,'check DAY, YYYY/MM/DD fmt and leapyear calls')
  ASSERT_APPROXEQA(getTimeFromDate('1990/12/1','1998/12/1','DAY'),2922.0_SRK,'check DAY, YYYY/MM/D fmt and leapyear calls')
  ASSERT_APPROXEQA(getTimeFromDate('1990/1/01','2002/1/01','DAY'),4383.0_SRK,'check DAY, YYYY/M/DD fmt and leapyear calls')
  ASSERT_APPROXEQA(getTimeFromDate('1890/1/1','1906/1/1','DAY'),5843.0_SRK,'check DAY, YYYY/M/D fmt and leapyear calls')
  ASSERT_APPROXEQA(getTimeFromDate('09/05/1997','10/05/1997','SEC'),2592000.0_SRK,'check SEC 09/05/1997 and 10/05/1997')
  ASSERT_APPROXEQA(getTimeFromDate('9/20/2009','10/19/2009','HOUR'),696.0_SRK,'check HOUR 9/20/2009 and 10/19/2009')

  !Test getClockTime
  COMPONENT_TEST('getClockTime()')
  idum1=0
  idum2=0
  idum3=0
  aclock=getClockTime()
  READ(aclock,'(i2,a1,i2,a1,i2)',iostat=ioerr) idum1,adum1,idum2,adum2,idum3
  ASSERT_EQ(ioerr,0,'ioerr')
  ASSERT_GT(idum1,-1,'hour')
  ASSERT_LT(idum1,24,'hour')
  ASSERT_GT(idum2,-1,'minute')
  ASSERT_LT(idum2,60,'minute')
  ASSERT_GT(idum3,-1,'minute')
  ASSERT_LT(idum3,60,'minute')
  ASSERT_EQ(adum1,':',':')
  ASSERT_EQ(adum1,adum2,':')

  COMPONENT_TEST('HI-RES TIMER')
  ASSERT_EQ(LEN_TRIM(testTimer%getTimerName()),0,'%getTimerName()')
  CALL testTimer%setTimerName('myName')
  ASSERT_EQ(TRIM(testTimer%getTimerName()),'myName','%setTimerName()')
  testTimer%elapsedtime=0.0001_SRK
  ASSERT_EQ(testTimer%getTimeReal(),0.0001_SRK,'%getTimeReal()')
  ASSERT_EQ(testTimer%getTimeChar(),'  100.000 microsec','%getTimeChar() (us)')
  ASSERT_EQ(testTimer%getTimeHHMMSS(),'00:00.00   ','%getTimeHHMMSS() (us)')
  testTimer%elapsedtime=0.999_SRK
  ASSERT_EQ(testTimer%getTimeChar(),'  999.000 ms      ','%getTimeChar() (ms)')
  ASSERT_EQ(testTimer%getTimeHHMMSS(),'00:01.00   ','%getTimeHHMMSS() (ms)')
  testTimer%elapsedtime=100.637_SRK
  ASSERT_EQ(testTimer%getTimeChar(),'  100.637 s       ','%getTimeChar() (s)')
  ASSERT_EQ(testTimer%getTimeHHMMSS(),'01:40.64   ','%getTimeHHMMSS() (s)')
  testTimer%elapsedtime=100000.6_SRK
  ASSERT_EQ(testTimer%getTimeChar(),'27:46:40.60 hh:mm:ss','%getTimeChar() (hr)')
  ASSERT_EQ(testTimer%getTimeHHMMSS(),'27:46:40.60','%getTimeHHMMSS() (hr)')
  testTimer%elapsedtime=3719.6_SRK
  ASSERT_EQ(testTimer%getTimeHHMMSS(),'01:01:59.60','%getTimeHHMMSS() (round)')
  CALL testTimer%ResetTimer()
  ASSERT_EQ(LEN_TRIM(testTimer%getTimername()),0,'name')
  ASSERT_EQ(testTimer%elapsedtime,0._SRK,'elapsedtime')
  ASSERT_EQ(testTimer%getTimeChar(),'    0.000 microsec','time char')
  CALL testTimer%tic()
  CALL sleep(1)
  CALL testTimer%toc()
  totalElapsed=testTimer%elapsedtime
  ASSERT_GT(totalElapsed,0.0_SRK,'tic/toc')
  CALL testTimer%tic()
  CALL sleep(1)
  CALL testTimer%toc()
  ASSERT_GT(testTimer%elapsedtime,totalElapsed,'tic/toc')
  totalElapsed=testTimer%elapsedtime
  CALL testTimer%tic()
  CALL sleep(1)
  CALL testTimer%toc()
  ASSERT_GT(testTimer%elapsedtime,totalElapsed,'tic/toc')
  totalElapsed=testTimer%elapsedtime
  ASSERT_LT(totalElapsed,60.0_SRK,'tic/toc too slow')
  ASSERT(testTimer%getTimerHiResMode(),'%getTimerHiResMode()')

  COMPONENT_TEST('LO-RES TIMER')
  CALL testTimer%setTimerHiResMode(.FALSE.)
  ASSERT(.NOT.testTimer%getTimerHiResMode(),'%getTimerHiResMode()')
  adate=testTimer%getDate()
  idum1=0
  idum2=0
  idum3=0
  READ(adate,'(i2,a1,i2,a1,i4)',iostat=ioerr) idum1,adum1,idum2,adum2,idum3
  ASSERT_EQ(ioerr,0,'ioerr')
  ASSERT_GT(idum1,0,'month')
  ASSERT_LT(idum1,13,'month')
  ASSERT_GT(idum2,0,'day')
  ASSERT_LT(idum2,32,'day')
  ASSERT_GT(idum3,0,'year')
  ASSERT_EQ(adum1,adum2,'adum')
  ASSERT_EQ(adum1,'/','adum1')
  ASSERT_EQ(testTimer%getDate(),testTimer%getDate(1),'%getDate(1)')
  idum1=0
  idum2=0
  adate=testTimer%getDate(2)
  READ(adate,'(a5,i2,a2,i4)',iostat=ioerr) adum3,idum1,adum4,idum2
  ASSERT_EQ(ioerr,0,'ioerr')
  ASSERT_GT(idum1,0,'day')
  ASSERT_LT(idum1,32,'day')
  ASSERT(idum2 > 0,'year')
  ASSERT_EQ(adum4(1:1),',','month')
  idum1=0
  idum2=0
  idum3=0
  aclock=testTimer%getClockTime()
  READ(aclock,'(i2,a1,i2,a1,i2)',iostat=ioerr) idum1,adum1,idum2,adum2,idum3
  ASSERT_EQ(ioerr,0,'ioerr')
  ASSERT_GT(idum1,-1,'hour')
  ASSERT_LT(idum1,24,'hour')
  ASSERT_GT(idum2,-1,'minute')
  ASSERT_LT(idum2,60,'minute')
  ASSERT_GT(idum3,-1,'minute')
  ASSERT_LT(idum3,60,'minute')
  ASSERT_EQ(adum1,':',':')
  ASSERT_EQ(adum1,adum2,':')
  testTimer%elapsedtime=0.0001_SRK

  COMPONENT_TEST('testTimer%getRemainingTime')
  CALL testTimer%tic()
  CALL sleep(1)
  CALL testTimer%toc()
  totalElapsed=testTimer%elapsedtime
  ASSERT_GT(totalElapsed,0.0_SRK,'tic/toc')
  CALL testTimer%tic()
  CALL sleep(1)
  CALL testTimer%toc()
  ASSERT_GT(testTimer%elapsedtime,totalElapsed,'tic/toc')
  totalElapsed=testTimer%elapsedtime
  CALL testTimer%tic()
  CALL sleep(1)
  CALL testTimer%toc()
  ASSERT_GT(testTimer%elapsedtime,totalElapsed,'tic/toc')
  totalElapsed=testTimer%elapsedtime
  ASSERT_LT(totalElapsed,60.0_SRK,'tic/toc too slow')

ENDSUBROUTINE testTimers
!
!-------------------------------------------------------------------------------
SUBROUTINE testParentTimers()
  TYPE(ParentTimerType) :: parentTimer
  TYPE(TimerType),POINTER :: tmpptr
  CLASS(TimerType),POINTER :: timerptr

  COMPONENT_TEST('clear')
  CALL parentTimer%clear()
  ALLOCATE(parentTimer%timers(1))
  !Doing this tmpptr thing because of what seems like a compiler bug.  Allocating
  !timers(1)%t directly causes a segfault unless I have a WRITE statement first.
  !It only happens in this test, not elsewhere
  tmpptr => NULL()
  ALLOCATE(tmpptr)
  parentTimer%timers(1)%t => tmpptr
  parentTimer%name='testName'
  CALL parentTimer%clear()
  ASSERT(.NOT.ALLOCATED(parentTimer%timers),'ALLOCATED %timers')
  ASSERT_EQ(parentTimer%getTimerName(),'','%name')

  COMPONENT_TEST('addTimer')
  CALL parentTimer%addTimer('test1')
  ASSERT_EQ(SIZE(parentTimer%timers),1,'SIZE %timers')
  SELECTTYPE(t => parentTimer%timers(1)%t)
  TYPE IS(TimerType)
    ASSERT(.TRUE.,'timer(1) type')
  CLASS DEFAULT
    ASSERT(.FALSE.,'timer(1) type')
  ENDSELECT
  CALL parentTimer%addTimer('test2')
  ASSERT_EQ(SIZE(parentTimer%timers),2,'SIZE %timers')
  SELECTTYPE(t => parentTimer%timers(2)%t)
  TYPE IS(TimerType)
    ASSERT(.TRUE.,'timer(2) type')
  CLASS DEFAULT
    ASSERT(.FALSE.,'timer(2) type')
  ENDSELECT
  CALL parentTimer%addTimer('test3 -> subtest1')
  ASSERT_EQ(SIZE(parentTimer%timers),3,'SIZE %timers')
  SELECTTYPE(t => parentTimer%timers(3)%t)
  TYPE IS(ParentTimerType)
    ASSERT(.TRUE.,'timer(3) type')
    ASSERT_EQ(SIZE(t%timers),1,'SIZE %timers(3)%timers')
  CLASS DEFAULT
    ASSERT(.FALSE.,'timer(3) type')
  ENDSELECT
  CALL parentTimer%addTimer('test4 -> subtest2')
  ASSERT_EQ(SIZE(parentTimer%timers),4,'SIZE %timers')
  SELECTTYPE(t => parentTimer%timers(4)%t)
  TYPE IS(ParentTimerType)
    ASSERT(.TRUE.,'timer(4) type')
    ASSERT_EQ(SIZE(t%timers),1,'SIZE %timers(4)%timers')
  CLASS DEFAULT
    ASSERT(.FALSE.,'timer(4) type')
  ENDSELECT
  CALL parentTimer%addTimer('subtest1')
  ASSERT_EQ(SIZE(parentTimer%timers),5,'SIZE %timers')
  SELECTTYPE(t => parentTimer%timers(1)%t)
  TYPE IS(TimerType)
    ASSERT(.TRUE.,'timer(5) type')
  CLASS DEFAULT
    ASSERT(.FALSE.,'timer(5) type')
  ENDSELECT
  CALL parentTimer%addTimer('test4 -> subtest1')
  ASSERT_EQ(SIZE(parentTimer%timers),5,'SIZE %timers')
  SELECTTYPE(t => parentTimer%timers(4)%t)
  TYPE IS(ParentTimerType)
    ASSERT(.TRUE.,'timer(4) type')
    ASSERT_EQ(SIZE(t%timers),2,'SIZE %timers(4)%timers')
  CLASS DEFAULT
    ASSERT(.FALSE.,'timer(4) type')
  ENDSELECT
  CALL parentTimer%addTimer('test4 -> test1')
  ASSERT_EQ(SIZE(parentTimer%timers),5,'SIZE %timers')
  SELECTTYPE(t => parentTimer%timers(4)%t)
  TYPE IS(ParentTimerType)
    ASSERT(.TRUE.,'timer(4) type')
    ASSERT_EQ(SIZE(t%timers),3,'SIZE %timers(4)%timers')
  CLASS DEFAULT
    ASSERT(.FALSE.,'timer(4) type')
  ENDSELECT
  CALL parentTimer%addTimer('test2')
  ASSERT_EQ(SIZE(parentTimer%timers),5,'SIZE %timers')
  CALL parentTimer%addTimer('test4 -> test1')
  ASSERT_EQ(SIZE(parentTimer%timers),5,'SIZE %timers')
  SELECTTYPE(t => parentTimer%timers(4)%t)
  TYPE IS(ParentTimerType)
    ASSERT(.TRUE.,'timer(4) type')
    ASSERT_EQ(SIZE(t%timers),3,'SIZE %timers(4)%timers')
  CLASS DEFAULT
    ASSERT(.FALSE.,'timer(4) type')
  ENDSELECT

  COMPONENT_TEST('getTimer')
  timerptr => parentTimer%getTimer('test1')
  ASSERT(ASSOCIATED(timerptr),'ASSOCIATED %getTimer("test1")')
  ASSERT_EQ(CHAR(timerptr%name),'test1','getTimer("test1")')
  timerptr => parentTimer%getTimer('test2')
  ASSERT(ASSOCIATED(timerptr),'ASSOCIATED %getTimer("test2")')
  ASSERT_EQ(CHAR(timerptr%name),'test2','getTimer("test2")')
  timerptr => parentTimer%getTimer('test3 -> subtest1')
  ASSERT(ASSOCIATED(timerptr),'ASSOCIATED %getTimer("test3 -> subtest1")')
  ASSERT_EQ(CHAR(timerptr%name),'subtest1','getTimer("test3 -> subtest1")')
  timerptr => parentTimer%getTimer('test4 -> subtest2')
  ASSERT(ASSOCIATED(timerptr),'ASSOCIATED %getTimer("test4 -> subtest1")')
  ASSERT_EQ(CHAR(timerptr%name),'subtest2','getTimer("test4 -> subtest2")')
  timerptr => parentTimer%getTimer('subtest1')
  ASSERT(ASSOCIATED(timerptr),'ASSOCIATED %getTimer("subtest1")')
  ASSERT_EQ(CHAR(timerptr%name),'subtest1','getTimer("subtest1")')
  timerptr => parentTimer%getTimer('test4 -> subtest1')
  ASSERT(ASSOCIATED(timerptr),'ASSOCIATED %getTimer("test4 -> subtest1")')
  ASSERT_EQ(CHAR(timerptr%name),'subtest1','getTimer("test4 -> subtest1")')
  timerptr => parentTimer%getTimer('test4 -> test1')
  ASSERT(ASSOCIATED(timerptr),'ASSOCIATED %getTimer("test4 -> test1")')
  ASSERT_EQ(CHAR(timerptr%name),'test1','getTimer("test4 -> test1")')
  timerptr => parentTimer%getTimer('subtest2')
  ASSERT(.NOT.ASSOCIATED(timerptr),'ASSOCIATED %getTimer("subtest2")')
  timerptr => parentTimer%getTimer('test4 -> subtest1')
  ASSERT(ASSOCIATED(timerptr),'ASSOCIATED %getTimer("test4 -> subtest1")')
  ASSERT_EQ(CHAR(timerptr%name),'subtest1','getTimer("test4 -> subtest1")')
  timerptr => parentTimer%getTimer('test')
  ASSERT(.NOT.ASSOCIATED(timerptr),'ASSOCIATED %getTimer("test")')

ENDSUBROUTINE testParentTimers
!
!
!-------------------------------------------------------------------------------
ENDSUBMODULE testTimesSubmodule
PROGRAM testTimes
  USE Times
  CALL runTestTimes()
ENDPROGRAM testTimes
