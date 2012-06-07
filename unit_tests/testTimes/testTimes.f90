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
PROGRAM testTimes
      
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
!
!Check the timer resolution
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING TIMERS...'
  WRITE(*,*) '==================================================='
!
!Print module constants
  WRITE(*,*) 'TESTING PARAMETERS'
  WRITE(*,*) '  Passed:      MAXLEN_TIMER_NAME = ',MAXLEN_TIMER_NAME
  WRITE(*,*) '  Passed:     MAXLEN_TIME_STRING = ',MAXLEN_TIME_STRING
  WRITE(*,*) '  Passed:     MAXLEN_DATE_STRING = ',MAXLEN_DATE_STRING
  WRITE(*,*) '  Passed:    MAXLEN_CLOCK_STRING = ',MAXLEN_CLOCK_STRING
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING DATE/CLOCK/TIMER ROUTINES'
!
!Test getDate()
  adate=getDate()
  idum1=0
  idum2=0
  idum3=0
  READ(adate,'(i2,a1,i2,a1,i4)',iostat=ioerr) idum1,adum1,idum2,adum2,idum3
  IF(ioerr /= 0 .OR. (idum1 < 1 .OR. idum1 > 12) &
                .OR. (idum2 < 1 .OR. idum2 > 31) &
                .OR. (idum3 <= 0)                &
                .OR. (adum1 /= adum2 .AND. adum1 /= '/') ) THEN
    WRITE(*,*) 'getDate() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: getDate() = '//getDate()
  ENDIF
  IF(getDate() /= getDate(1)) THEN
    WRITE(*,*) 'getDate(1) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: getDate(1) = '//getDate(1)
  ENDIF
  
  idum1=0
  idum2=0
  adate=getDate(2)
  READ(adate,'(a5,i2,a2,i4)',iostat=ioerr) adum3,idum1,adum4,idum2
  IF(ioerr /= 0 .OR. (idum1 < 1 .OR. idum1 > 31) &
                .OR. (idum2 <= 0)                &
                .OR. (adum4(1:1) /= ',')) THEN
    WRITE(*,*) 'getDate(2) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: getDate(2) = '//getDate(2)
  ENDIF
  
  idum1=0
  idum2=0
  idum3=0
  aclock=getClockTime()
  READ(aclock,'(i2,a1,i2,a1,i2)',iostat=ioerr) idum1,adum1,idum2,adum2,idum3
  IF(ioerr /= 0 .OR. (idum1 < 0 .OR. idum1 > 23) &
                .OR. (idum2 < 0 .OR. idum2 > 59) &
                .OR. (idum3 < 0 .OR. idum3 > 59) &
                .OR. (adum1 /= adum2 .AND. adum1 /= ':') ) THEN
    WRITE(*,*) 'getClockTime() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: getClockTime() = '//getClockTime()
  ENDIF
  WRITE(*,*) '---------------------------------------------------'
  
  WRITE(*,*) 'TESTING HI-RES TIMER FUNCTIONALITY'
  IF(LEN_TRIM(testTimer%getTimerName()) /= 0) THEN
    WRITE(*,*) 'testTimer%getTimerName() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimerName()'
  ENDIF
  CALL testTimer%setTimerName('myName')
  IF(TRIM(testTimer%getTimerName()) /= 'myName') THEN
    WRITE(*,*) 'CALL testTimer%setTimerName(''myName'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testTimer%setTimerName(''myName'')'
  ENDIF
  IF(TRIM(testTimer%getTimerName()) /= 'myName') THEN
    WRITE(*,*) 'testTimer%getTimerName() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimerName()'
  ENDIF
  WRITE(*,*) '  Passed: testTimer%getTimerResolution()=',testTimer%getTimerResolution()
  testTimer%elapsedtime=0.0001_SRK
  IF(testTimer%getTimeReal() /= 0.0001_SRK) THEN
    WRITE(*,*) 'testTimer%getTimeReal() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeReal()'
  ENDIF
  IF(testTimer%getTimeChar() /= '  100.000 microsec') THEN
    WRITE(*,*) 'testTimer%getTimeChar()=100 microsec FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeChar()='//testTimer%getTimeChar()
  ENDIF
  IF(testTimer%getTimeHHMMSS() /= '000:00.00         ') THEN
    WRITE(*,*) 'getTime(testTimer)=0.0001 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: getTime(testTimer) = '//testTimer%getTimeHHMMSS()
  ENDIF
  testTimer%elapsedtime=0.999_SRK
  IF(testTimer%getTimeChar() /= '  999.000 ms      ') THEN
    WRITE(*,*) 'testTimer%getTimeChar()=0.999 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeChar()= '// &
               TRIM(testTimer%getTimeChar())
  ENDIF
  IF(testTimer%getTimeHHMMSS() /= '000:00.99         ') THEN
    WRITE(*,*) 'testTimer%getTimeHHMMSS()=0.999 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeHHMMSS()= '// &
               TRIM(testTimer%getTimeHHMMSS())
  ENDIF
  testTimer%elapsedtime=100.637_SRK
  IF(testTimer%getTimeChar() /= '  100.637 s       ') THEN
    WRITE(*,*) 'testTimer%getTimeChar()=100.637 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeChar()= '// &
               TRIM(testTimer%getTimeChar())
  ENDIF
  IF(testTimer%getTimeHHMMSS() /= '001:40.64         ') THEN
    WRITE(*,*) 'getTime(testTimer)=100.637 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeHHMMSS()= '// &
               TRIM(testTimer%getTimeHHMMSS())
  ENDIF
  testTimer%elapsedtime=100000.6_SRK
  IF(testTimer%getTimeChar() /= '027:46:41 hh:mm:ss') THEN
    WRITE(*,*) 'testTimer%getTimeChar()=100000.6 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeChar()= '// &
                TRIM(testTimer%getTimeChar())
  ENDIF
  IF(testTimer%getTimeHHMMSS() /= '027:46:41         ') THEN
    WRITE(*,*) 'testTimer%getTimeHHMMSS()=100000.6 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeHHMMSS()= '// &
               TRIM(testTimer%getTimeHHMMSS())
  ENDIF
  testTimer%elapsedtime=3719.6_SRK
  IF(testTimer%getTimeHHMMSS() /= '001:01:59         ') THEN
    WRITE(*,*) 'testTimer%getTimeHHMMSS()=3719.6 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeHHMMSS()= '// &
               TRIM(testTimer%getTimeHHMMSS())
  ENDIF
  CALL testTimer%ResetTimer()
  IF((LEN_TRIM(testTimer%getTimername()) /= 0) &
           .OR. (testTimer%elapsedtime /= 0._SRK) &
           .OR. (testTimer%getTimeChar() /= '    0.000 microsec')) THEN
    WRITE(*,*) 'testTimer%ResetTimer() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%ResetTimer()'
  ENDIF
  CALL testTimer%tic()
  CALL sleep(1)
  CALL testTimer%toc()
  IF(ABS(testTimer%elapsedtime-1._SRK) > 0.05_SRK) THEN
    WRITE(*,*) 'tic(testTimer) or toc(testTimer) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%tic()'
    WRITE(*,*) '  Passed: testTimer%toc()= ',testTimer%elapsedtime
  ENDIF
  IF(.NOT.testTimer%getTimerHiResMode()) THEN
    WRITE(*,*) 'tesTimer%getTimerHiResMode() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: tesTimer%getTimerHiResMode()'
  ENDIF
  WRITE(*,*) '  Passed: testTimer%getRemainingTime()',testTimer%getRemainingTime()
  WRITE(*,*) '---------------------------------------------------'
  
  WRITE(*,*) 'TESTING LO-RES TIMER FUNCTIONALITY'
  CALL testTimer%setTimerHiResMode(.FALSE.)
  IF(testTimer%getTimerHiResMode()) THEN
    WRITE(*,*) 'tesTimer%getTimerHiResMode() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: tesTimer%getTimerHiResMode()'
  ENDIF
  adate=testTimer%getDate()
  idum1=0
  idum2=0
  idum3=0
  READ(adate,'(i2,a1,i2,a1,i4)',iostat=ioerr) idum1,adum1,idum2,adum2,idum3
  IF(ioerr /= 0 .OR. (idum1 < 1 .OR. idum1 > 12) &
                .OR. (idum2 < 1 .OR. idum2 > 31) &
                .OR. (idum3 <= 0)                &
                .OR. (adum1 /= adum2 .AND. adum1 /= '/') ) THEN
    WRITE(*,*) 'testTimer%getDate() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getDate()'
  ENDIF
  IF(testTimer%getDate() /= testTimer%getDate(1)) THEN
    WRITE(*,*) 'testTimer%getDate(1) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getDate(1)'
  ENDIF
  idum1=0
  idum2=0
  adate=testTimer%getDate(2)
  READ(adate,'(a5,i2,a2,i4)',iostat=ioerr) adum3,idum1,adum4,idum2
  IF(ioerr /= 0 .OR. (idum1 < 1 .OR. idum1 > 31) &
                .OR. (idum2 <= 0)                &
                .OR. (adum4(1:1) /= ',')) THEN
    WRITE(*,*) 'testTimer%getDate(2) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getDate(2)'
  ENDIF
  idum1=0
  idum2=0
  idum3=0
  aclock=testTimer%getClockTime()
  READ(aclock,'(i2,a1,i2,a1,i2)',iostat=ioerr) idum1,adum1,idum2,adum2,idum3
  IF(ioerr /= 0 .OR. (idum1 < 0 .OR. idum1 > 23) &
                .OR. (idum2 < 0 .OR. idum2 > 59) &
                .OR. (idum3 < 0 .OR. idum3 > 59) &
                .OR. (adum1 /= adum2 .AND. adum1 /= ':') ) THEN
    WRITE(*,*) 'testTimer%getClockTime() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getClockTime()'
  ENDIF
  IF(LEN_TRIM(testTimer%getTimerName()) /= 0) THEN
    WRITE(*,*) 'testTimer%getTimerName() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimerName()'
  ENDIF
  CALL testTimer%setTimerName('myName')
  IF(TRIM(testTimer%getTimerName()) /= 'myName') THEN
    WRITE(*,*) 'CALL testTimer%setTimerName(''myName'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testTimer%setTimerName(''myName'')'
  ENDIF
  IF(TRIM(testTimer%getTimerName()) /= 'myName') THEN
    WRITE(*,*) 'testTimer%getTimerName() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimerName()'
  ENDIF
  WRITE(*,*) '  Passed: testTimer%getTimerResolution()=',testTimer%getTimerResolution()
  testTimer%elapsedtime=0.0001_SRK
  IF(testTimer%getTimeReal() /= 0.0001_SRK) THEN
    WRITE(*,*) 'testTimer%getTimeReal() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeReal()'
  ENDIF
  IF(testTimer%getTimeChar() /= '  100.000 microsec') THEN
    WRITE(*,*) 'testTimer%getTimeChar()=100 microsec FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeChar()='//testTimer%getTimeChar()
  ENDIF
  IF(testTimer%getTimeHHMMSS() /= '000:00.00         ') THEN
    WRITE(*,*) 'getTime(testTimer)=0.0001 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: getTime(testTimer) = '//testTimer%getTimeHHMMSS()
  ENDIF
  testTimer%elapsedtime=0.999_SRK
  IF(testTimer%getTimeChar() /= '  999.000 ms      ') THEN
    WRITE(*,*) 'testTimer%getTimeChar()=0.999 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeChar()= '// &
               TRIM(testTimer%getTimeChar())
  ENDIF
  IF(testTimer%getTimeHHMMSS() /= '000:00.99         ') THEN
    WRITE(*,*) 'testTimer%getTimeHHMMSS()=0.999 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeHHMMSS()= '// &
               TRIM(testTimer%getTimeHHMMSS())
  ENDIF
  testTimer%elapsedtime=100.637_SRK
  IF(testTimer%getTimeChar() /= '  100.637 s       ') THEN
    WRITE(*,*) 'testTimer%getTimeChar()=100.637 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeChar()= '// &
               TRIM(testTimer%getTimeChar())
  ENDIF
  IF(testTimer%getTimeHHMMSS() /= '001:40.64         ') THEN
    WRITE(*,*) 'getTime(testTimer)=100.637 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeHHMMSS()= '// &
               TRIM(testTimer%getTimeHHMMSS())
  ENDIF
  testTimer%elapsedtime=100000.6_SRK
  IF(testTimer%getTimeChar() /= '027:46:41 hh:mm:ss') THEN
    WRITE(*,*) 'testTimer%getTimeChar()=100000.6 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeChar()= '// &
                TRIM(testTimer%getTimeChar())
  ENDIF
  IF(testTimer%getTimeHHMMSS() /= '027:46:41         ') THEN
    WRITE(*,*) 'testTimer%getTimeHHMMSS()=100000.6 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%getTimeHHMMSS()= '// &
               TRIM(testTimer%getTimeHHMMSS())
  ENDIF
  CALL testTimer%ResetTimer()
  IF((LEN_TRIM(testTimer%getTimername()) /= 0) &
           .OR. (testTimer%elapsedtime /= 0._SRK) &
           .OR. (testTimer%getTimeChar() /= '    0.000 microsec')) THEN
    WRITE(*,*) 'testTimer%ResetTimer() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%ResetTimer()'
  ENDIF
  CALL testTimer%tic()
  CALL sleep(1)
  CALL testTimer%toc()
  IF(ABS(testTimer%elapsedtime-1._SRK) > 0.05_SRK) THEN
    WRITE(*,*) 'tic(testTimer) or toc(testTimer) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testTimer%tic()'
    WRITE(*,*) '  Passed: testTimer%toc()= ',testTimer%elapsedtime
  ENDIF
  WRITE(*,*) '  Passed: testTimer%getRemainingTime()',testTimer%getRemainingTime()
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING TIMERS PASSED!'
  WRITE(*,*) '==================================================='    
ENDPROGRAM testTimes
