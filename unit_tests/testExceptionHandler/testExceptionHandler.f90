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
PROGRAM testExceptionHandler
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE Utils
  
  IMPLICIT NONE

  TYPE(ExceptionHandlerType) :: test
  CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: string
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING ExceptionHandler...'
  WRITE(*,*) '==================================================='
!
!Print module constants
  WRITE(*,*) 'TESTING PARAMETERS'
  WRITE(*,*) '  Passed:              EXCEPTION_OK = ',EXCEPTION_OK
  WRITE(*,*) '  Passed:     EXCEPTION_INFORMATION = ',EXCEPTION_INFORMATION
  WRITE(*,*) '  Passed:         EXCEPTION_WARNING = ',EXCEPTION_WARNING
  WRITE(*,*) '  Passed:   EXCEPTION_DEBUG_WARNING = ',EXCEPTION_DEBUG_WARNING
  WRITE(*,*) '  Passed:           EXCEPTION_ERROR = ',EXCEPTION_ERROR
  WRITE(*,*) '  Passed:     EXCEPTION_FATAL_ERROR = ',EXCEPTION_FATAL_ERROR
  WRITE(*,*) '  Passed:         EXCEPTION_FAILURE = ',EXCEPTION_FAILURE
  WRITE(*,*) '  Passed:            EXCEPTION_SIZE = ',EXCEPTION_SIZE
  WRITE(*,*) '  Passed: EXCEPTION_MAX_MESG_LENGTH = ',EXCEPTION_MAX_MESG_LENGTH
  WRITE(*,*) '---------------------------------------------------'
!
!Test all "Set" & "Get" methods
  WRITE(*,*) 'TESTING ALL METHODS'
  IF(test%isQuietMode()) THEN
    WRITE(*,*) 'test%isQuietMode() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: test%isQuietMode()'
  ENDIF
  CALL test%setQuietMode(.TRUE.)
  IF(.NOT.test%isQuietMode()) THEN
    WRITE(*,*) 'CALL test%setQuietMode(.TRUE.) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL test%setQuietMode(.TRUE.)'
  ENDIF
  
  IF(.NOT. test%isDebugMode()) THEN
    WRITE(*,*) 'test%isDebugMode() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: test%isDebugMode()'
  ENDIF
  CALL test%setDebugMode(.FALSE.)
  IF(test%isDebugMode()) THEN
    WRITE(*,*) 'CALL test%setDebugMode(.TRUE.) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL test%setDebugMode(.TRUE.)'
  ENDIF
  IF(.NOT.test%isStopOnError()) THEN
    WRITE(*,*) 'test%isStopOnError() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: test%isStopOnError()'
  ENDIF
  CALL test%setStopOnError(.FALSE.)
  IF(test%isStopOnError()) THEN
    WRITE(*,*) 'CALL test%setStopOnError(.FALSE.) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL test%setStopOnError(.FALSE.)'
  ENDIF
  IF(test%getLogFileUnit() /= 0) THEN
    WRITE(*,*) 'test%getLogFileUnit() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: test%getLogFileUnit()'
  ENDIF
  CALL test%setLogFileUnit(ERROR_UNIT)
  IF(test%getLogFileUnit() /= 0) THEN
    WRITE(*,*) 'test%setLogFileUnit(ERROR_UNIT) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: test%setLogFileUnit(ERROR_UNIT)'
  ENDIF
  CALL test%setLogFileUnit(OUTPUT_UNIT)
  IF(test%getLogFileUnit() /= 0) THEN
    WRITE(*,*) 'test%setLogFileUnit(OUTPUT_UNIT) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: test%setLogFileUnit(OUTPUT_UNIT)'
  ENDIF
  CALL test%setLogFileUnit(-1)
  IF(test%getLogFileUnit() /= 0) THEN
    WRITE(*,*) 'test%setLogFileUnit(-1) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: test%setLogFileUnit(-1)'
  ENDIF
  CALL test%setLogFileUnit(23)
  IF(test%getLogFileUnit() /= 23) THEN
    WRITE(*,*) 'test%setLogFileUnit(23) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: test%setLogFileUnit(23)'
  ENDIF
  string=test%getLastMessage()
  IF(TRIM(string) /= '#### EXCEPTION_WARNING #### - '// &
     'Illegal unit number for log file. Log file unit not set.') THEN
    WRITE(*,*) 'test%getLastMessage() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: test%getLastMessage()'
  ENDIF
  IF(ANY(test%getCounterAll() /= (/0,3,0,0,0/))) THEN
    WRITE(*,*) 'test%getCounterAll() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: test%getCounterAll()'
  ENDIF
  CALL test%initCounter()
  IF(ANY(test%getCounterAll() /= (/0,0,0,0,0/)) .OR. &
     LEN_TRIM(test%getLastMessage()) /= 0) THEN
    WRITE(*,*) 'CALL test%initCounter() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL test%initCounter()'
  ENDIF
  IF(test%isLogActive()) THEN
    WRITE(*,*) 'test%isLogActive() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: test%isLogActive()'
  ENDIF
  CALL test%setLogActive(.TRUE.)
  IF(test%isLogActive()) THEN
    WRITE(*,*) 'CALL setLogActive(.TRUE.) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL setLogActive(.TRUE.)'
  ENDIF
  OPEN(UNIT=test%getLogFileUnit(),FILE='Exception.log', &
       ACCESS='SEQUENTIAL',FORM='FORMATTED')
  CALL test%setLogActive(.TRUE.)
  IF(.NOT.test%isLogActive()) THEN
    WRITE(*,*) 'CALL setLogActive(.TRUE.) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL setLogActive(.TRUE.)'
  ENDIF
  CALL test%setLogActive(.FALSE.)
  IF(test%isLogActive()) THEN
    WRITE(*,*) 'CALL setLogActive(.FALSE.) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL setLogActive(.FALSE.)'
  ENDIF
  CALL test%setLogActive(.TRUE.)
  CALL test%initCounter()
  CALL test%raiseInformation('Test information')
  IF(ANY(test%getCounterAll() /= (/1,0,0,0,0/))) THEN
    WRITE(*,*) 'CALL test%raiseInformation(''Test information'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL test%raiseInformation(''Test information'')'
  ENDIF
  CALL test%raiseWarning('Test warning')
  IF(ANY(test%getCounterAll() /= (/1,1,0,0,0/))) THEN
    WRITE(*,*) 'CALL test%raiseWarning(''Test warning'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL test%raiseWarning(''Test warning'')'
  ENDIF
  CALL test%setDebugMode(.FALSE.)
  CALL test%raiseDebugWarning('Test warning')
  IF(ANY(test%getCounterAll() /= (/1,1,0,0,0/))) THEN
    WRITE(*,*) 'CALL test%raiseDebugWarning(''Test warning'') while debug is FALSE FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL test%raiseDebugWarning(''Test warning'') while debug is FALSE'
  ENDIF
  CALL test%setDebugMode(.TRUE.)
  CALL test%raiseDebugWarning('Test warning')
  IF(ANY(test%getCounterAll() /= (/1,2,0,0,0/))) THEN
    WRITE(*,*) 'CALL test%raiseDebugWarning(''Test warning'') while debug is TRUE FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL test%raiseDebugWarning(''Test warning'') while debug is TRUE'
  ENDIF
  CALL test%raiseError('Test error')
  IF(ANY(test%getCounterAll() /= (/1,2,1,0,0/))) THEN
    WRITE(*,*) 'CALL test%raiseError(''Test error'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL test%raiseError(''Test error'')'
  ENDIF
  CLOSE(test%getLogFileUnit())
  OPEN(UNIT=test%getLogFileUnit(),FILE='Exception.log', &
       ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ')
  CALL test%raiseFailure('Test failure')
  IF(ANY(test%getCounterAll() /= (/1,2,1,0,1/))) THEN
    WRITE(*,*) 'CALL test%raiseFailure(''Test failure'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL test%raiseFailure(''Test failure'')'
  ENDIF
  IF(test%getCounter(EXCEPTION_INFORMATION) /= 1 .AND. &
     test%getCounter(EXCEPTION_WARNING) /= 2 .AND. &
     test%getCounter(EXCEPTION_ERROR) /= 1 .AND. &
     test%getCounter(EXCEPTION_FATAL_ERROR) /= 0 .AND. &
     test%getCounter(EXCEPTION_FAILURE) /= 1 .AND. &
     test%getCounter(0) /= -1) THEN
    WRITE(*,*) 'CALL test%getCounter(...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL test%getCounter(...)'
  ENDIF
  CALL test%setQuietMode(.FALSE.)
  CALL test%raiseFailure('Test failure')
  CLOSE(test%getLogFileUnit(),STATUS='DELETE')
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING ExceptionHandler PASSED!'
  WRITE(*,*) '==================================================='
!  CALL test%raiseFatalError('test fatal error')
ENDPROGRAM testExceptionHandler
