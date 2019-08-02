!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testTimes
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE Times

  IMPLICIT NONE

  TYPE(TimerType) :: testTimer

  INTEGER :: idum1,idum2,idum3,ioerr
  LOGICAL(SBK) :: bool
  CHARACTER(LEN=1) :: adum1,adum2
  CHARACTER(LEN=5) :: adum3
  CHARACTER(LEN=2) :: adum4
  CHARACTER(LEN=MAXLEN_DATE_STRING) :: adate
  CHARACTER(LEN=MAXLEN_CLOCK_STRING) :: aclock
!
!Check the timer resolution
  CREATE_TEST('TIMERS')
!
!Print module constants
  COMPONENT_TEST('PARAMETERS')
  WRITE(*,*) '  Passed:     MAXLEN_TIME_STRING = ',MAXLEN_TIME_STRING
  WRITE(*,*) '  Passed:     MAXLEN_DATE_STRING = ',MAXLEN_DATE_STRING
  WRITE(*,*) '  Passed:    MAXLEN_CLOCK_STRING = ',MAXLEN_CLOCK_STRING

  COMPONENT_TEST('getDate()')
!
!Test getDate()
  adate=getDate()
  idum1=0
  idum2=0
  idum3=0
  READ(adate,'(i2,a1,i2,a1,i4)',iostat=ioerr) idum1,adum1,idum2,adum2,idum3
  ASSERT(ioerr == 0,'ioerr')
  ASSERT(.NOT.(idum1 < 1 .OR. idum1 > 12),'month')
  ASSERT(.NOT.(idum2 < 1 .OR. idum2 > 31),'day')
  ASSERT(idum3 > 0,'year')
  ASSERT(adum1 == adum2,'adum')
  ASSERT(adum1 == '/','adum1')
  INFO(0) 'getDate() = '//getDate()
  ASSERT(getDate() == getDate(1),'getDate(1)')

  idum1=0
  idum2=0
  adate=getDate(2)
  READ(adate,'(a5,i2,a2,i4)',iostat=ioerr) adum3,idum1,adum4,idum2
  ASSERT(ioerr == 0,'ioerr')
  ASSERT(.NOT.(idum1 < 1 .OR. idum1 > 31),'day')
  ASSERT(idum2 > 0,'year')
  ASSERT(adum4(1:1) == ',','month')

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
  ASSERT(ioerr == 0,'ioerr')
  ASSERT(.NOT.(idum1 < 0 .OR. idum1 > 23),'hour')
  ASSERT(.NOT.(idum2 < 0 .OR. idum2 > 59),'minute')
  ASSERT(.NOT.(idum3 < 0 .OR. idum3 > 59),'second')
  ASSERT(adum1 == adum2 .AND. adum1 == ':',':')
  INFO(0) 'getClockTime() = '//getClockTime()

  COMPONENT_TEST('HI-RES TIMER')
  ASSERT(LEN_TRIM(testTimer%getTimerName()) == 0,'%getTimerName()')
  CALL testTimer%setTimerName('myName')
  ASSERT(TRIM(testTimer%getTimerName()) == 'myName','%setTimerName()')
  INFO(0) '  Passed: testTimer%getTimerResolution()=', &
    testTimer%getTimerResolution()
  testTimer%elapsedtime=0.0001_SRK
  ASSERT(testTimer%getTimeReal() == 0.0001_SRK,'%getTimeReal()')
  ASSERT(testTimer%getTimeChar() == '  100.000 microsec','%getTimeChar() (us)')
  ASSERT(testTimer%getTimeHHMMSS() == ' 00:00.00         ','%getTimeHHMMSS() (us)')
  testTimer%elapsedtime=0.999_SRK
  ASSERT(testTimer%getTimeChar() == '  999.000 ms      ','%getTimeChar() (ms)')
  ASSERT(testTimer%getTimeHHMMSS() == ' 00:00.99         ','%getTimeHHMMSS() (ms)')
  testTimer%elapsedtime=100.637_SRK
  ASSERT(testTimer%getTimeChar() == '  100.637 s       ','%getTimeChar() (s)')
  ASSERT(testTimer%getTimeHHMMSS() == ' 01:40.64         ','%getTimeHHMMSS() (s)')
  testTimer%elapsedtime=100000.6_SRK
  ASSERT(testTimer%getTimeChar() == ' 27:46:41 hh:mm:ss','%getTimeChar() (hr)')
  ASSERT(testTimer%getTimeHHMMSS() == ' 27:46:41         ','%getTimeHHMMSS() (hr)')
  testTimer%elapsedtime=3719.6_SRK
  ASSERT(testTimer%getTimeHHMMSS() == ' 01:01:59         ','%getTimeHHMMSS() (round)')
  CALL testTimer%ResetTimer()
  ASSERT(LEN_TRIM(testTimer%getTimername()) == 0,'name')
  ASSERT(testTimer%elapsedtime == 0._SRK,'elapsedtime')
  ASSERT(testTimer%getTimeChar() == '    0.000 microsec','time char')
  CALL testTimer%tic()
  CALL sleep(1)
  CALL testTimer%toc()
  ASSERT(ABS(testTimer%elapsedtime-1._SRK) < 0.05_SRK,'tic/toc')
  FINFO() 'testTimer%toc()= ',testTimer%elapsedtime
  ASSERT(testTimer%getTimerHiResMode(),'%getTimerHiResMode()')
  INFO(0) '  Passed: testTimer%getRemainingTime()',testTimer%getRemainingTime()

  COMPONENT_TEST('LO-RES TIMER')
  CALL testTimer%setTimerHiResMode(.FALSE.)
  ASSERT(.NOT.testTimer%getTimerHiResMode(),'%getTimerHiResMode()')
  adate=testTimer%getDate()
  idum1=0
  idum2=0
  idum3=0
  READ(adate,'(i2,a1,i2,a1,i4)',iostat=ioerr) idum1,adum1,idum2,adum2,idum3
  ASSERT(ioerr == 0,'ioerr')
  ASSERT(.NOT.(idum1 < 1 .OR. idum1 > 12),'month')
  ASSERT(.NOT.(idum2 < 1 .OR. idum2 > 31),'day')
  ASSERT(idum3 > 0,'year')
  ASSERT(adum1 == adum2,'adum')
  ASSERT(adum1 == '/','adum1')
  ASSERT(testTimer%getDate() == testTimer%getDate(1),'%getDate(1)')
  idum1=0
  idum2=0
  adate=testTimer%getDate(2)
  READ(adate,'(a5,i2,a2,i4)',iostat=ioerr) adum3,idum1,adum4,idum2
  ASSERT(ioerr == 0,'ioerr')
  ASSERT(.NOT.(idum1 < 1 .OR. idum1 > 31),'day')
  ASSERT(idum2 > 0,'year')
  ASSERT(adum4(1:1) == ',','month')
  idum1=0
  idum2=0
  idum3=0
  aclock=testTimer%getClockTime()
  READ(aclock,'(i2,a1,i2,a1,i2)',iostat=ioerr) idum1,adum1,idum2,adum2,idum3
  ASSERT(ioerr == 0,'ioerr')
  ASSERT(.NOT.(idum1 < 0 .OR. idum1 > 23),'hour')
  ASSERT(.NOT.(idum2 < 0 .OR. idum2 > 59),'minute')
  ASSERT(.NOT.(idum3 < 0 .OR. idum3 > 59),'second')
  ASSERT(adum1 == adum2 .AND. adum1 == ':',':')
  INFO(0) '  Passed: testTimer%getTimerResolution()=', &
    testTimer%getTimerResolution()
  testTimer%elapsedtime=0.0001_SRK

  CALL testTimer%tic()
  CALL sleep(1)
  CALL testTimer%toc()
  ASSERT(ABS(testTimer%elapsedtime-1._SRK) < 0.05_SRK,'tic/toc')
  FINFO() 'testTimer%toc()= ',testTimer%elapsedtime
  INFO(0) '  Passed: testTimer%getRemainingTime()', &
    testTimer%getRemainingTime()

  FINALIZE_TEST()
!
ENDPROGRAM testTimes
