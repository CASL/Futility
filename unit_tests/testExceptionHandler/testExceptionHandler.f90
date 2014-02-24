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

  TYPE(ExceptionHandlerType) :: testE,testE2
  CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: mesg,mesg2
  LOGICAL(SBK) :: bool
  
  CREATE_TEST('EXCEPTION HANDLER')
  
  REGISTER_SUBTEST('Parameters',testParameters)
  REGISTER_SUBTEST('Defaults',testDefault)
  REGISTER_SUBTEST('Raise',testRaise)
  REGISTER_SUBTEST('LogFile',testLogFile)
  REGISTER_SUBTEST('Verbosity',testVerbosity)
  REGISTER_SUBTEST('Surrogate',testSurrogate)
  REGISTER_SUBTEST('ASSIGNMENT(=)',testAssignment)
  REGISTER_SUBTEST('Reset',testReset)
  
  CLOSE(testE%getLogFileUnit(),STATUS='DELETE')
  
  FINALIZE_TEST()

  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testParameters()
      !Print module constants
      WRITE(*,*) 'TESTING PARAMETERS'
      WRITE(*,*) '  Passed:              EXCEPTION_OK = ',EXCEPTION_OK
      WRITE(*,*) '  Passed:     EXCEPTION_INFORMATION = ',EXCEPTION_INFORMATION
      WRITE(*,*) '  Passed:         EXCEPTION_WARNING = ',EXCEPTION_WARNING
      WRITE(*,*) '  Passed:           EXCEPTION_DEBUG = ',EXCEPTION_DEBUG
      WRITE(*,*) '  Passed:           EXCEPTION_ERROR = ',EXCEPTION_ERROR
      WRITE(*,*) '  Passed:     EXCEPTION_FATAL_ERROR = ',EXCEPTION_FATAL_ERROR
      WRITE(*,*) '  Passed:            EXCEPTION_SIZE = ',EXCEPTION_SIZE
      WRITE(*,*) '  Passed: EXCEPTION_MAX_MESG_LENGTH = ', &
        EXCEPTION_MAX_MESG_LENGTH
    ENDSUBROUTINE testParameters
!
!-------------------------------------------------------------------------------
    SUBROUTINE testDefault()
      COMPONENT_TEST('isQuiteMode')
      ASSERT(.NOT.testE%isQuietMode(),'%isQuietMode()')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_INFORMATION),'%isQuietMode(INFO)')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_WARNING),'%isQuietMode(WARNING)')
      ASSERT(testE%isQuietMode(EXCEPTION_DEBUG),'%isQuietMode(DEBUG)')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_ERROR),'%isQuietMode(ERROR)')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_FATAL_ERROR),'%isQuietMode(FATAL_ERROR)')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_OK),'%isQuietMode(OK)')
      
      COMPONENT_TEST('isStopOnError')
      ASSERT(testE%isStopOnError(),'%isStopOnError')
      
      COMPONENT_TEST('Counter')
      ASSERT(ALL(testE%getCounterAll() == 0),'getCounterAll()')
      ASSERT(testE%getCounter(EXCEPTION_INFORMATION) == 0,'INFO')
      ASSERT(testE%getCounter(EXCEPTION_WARNING) == 0,'WARN')
      ASSERT(testE%getCounter(EXCEPTION_DEBUG) == 0,'DEBUG')
      ASSERT(testE%getCounter(EXCEPTION_ERROR) == 0,'ERROR')
      ASSERT(testE%getCounter(EXCEPTION_FATAL_ERROR) == 0,'FATAL_ERROR')
      ASSERT(testE%getCounter(EXCEPTION_OK) == -1,'OK')
      ASSERT(testE%getLastMessage() == '','%getLastMessage()')
      
      COMPONENT_TEST('Log File')
      ASSERT(testE%getLogFileUnit() == 666,'%getLogFileUnit')
      ASSERT(.NOT.testE%isLogActive(),'%isLogActive')
      
      COMPONENT_TEST('setQuietMode')
      CALL testE%setQuietMode(.TRUE.)
      ASSERT(testE%isQuietMode(),'%setQuietMode(T)')
      ASSERT(testE%isQuietMode(EXCEPTION_INFORMATION),'%setQuietMode(T) INFO')
      ASSERT(testE%isQuietMode(EXCEPTION_WARNING),'%setQuietMode(T) WARN')
      ASSERT(testE%isQuietMode(EXCEPTION_DEBUG),'%setQuietMode(T) DEBUG')
      ASSERT(testE%isQuietMode(EXCEPTION_ERROR),'%setQuietMode(T) ERROR')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_FATAL_ERROR),'%setQuietMode(T) FATAL')
      CALL testE%setQuietMode(.FALSE.)
      ASSERT(.NOT.testE%isQuietMode(),'%setQuietMode(F)')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_INFORMATION),'%setQuietMode(F) INFO')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_WARNING),'%setQuietMode(F) WARNING')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_DEBUG),'%setQuietMode(F) DEBUG')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_ERROR),'%setQuietMode(F) ERROR')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_FATAL_ERROR),'%setQuietMode(F) FATAL')
      CALL testE%setQuietMode(EXCEPTION_INFORMATION,.TRUE.)
      ASSERT(.NOT.testE%isQuietMode(),'%setQuietMode(INFO,T)')
      ASSERT(testE%isQuietMode(EXCEPTION_INFORMATION),'%setQuietMode(INFO,T) INFO')
      CALL testE%setQuietMode(EXCEPTION_WARNING,.TRUE.)
      ASSERT(.NOT.testE%isQuietMode(),'%setQuietMode(WARN,T)')
      ASSERT(testE%isQuietMode(EXCEPTION_WARNING),'%setQuietMode(WARN,T) WARN')
      CALL testE%setQuietMode(EXCEPTION_DEBUG,.FALSE.)
      ASSERT(.NOT.testE%isQuietMode(),'%setQuietMode(DEBUG,F)')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_DEBUG),'%setQuietMode(DEBUG,F) DEBUG')
      CALL testE%setQuietMode(EXCEPTION_ERROR,.TRUE.)
      ASSERT(.NOT.testE%isQuietMode(),'%setQuietMode(ERROR,T)')
      ASSERT(testE%isQuietMode(EXCEPTION_ERROR),'%setQuietMode(ERROR,T) ERROR')
      CALL testE%setQuietMode(EXCEPTION_FATAL_ERROR,.TRUE.)
      ASSERT(.NOT.testE%isQuietMode(),'%setQuietMode(FATAL,T)')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_FATAL_ERROR),'%setQuietMode(FATAL,T) FATAL')
      CALL testE%setQuietMode(EXCEPTION_OK,.FALSE.)
      ASSERT(.NOT.testE%isQuietMode(),'%setQuietMode(OK,T)')
      ASSERT(.NOT.testE%isQuietMode(EXCEPTION_OK),'%setQuietMode(OK,T) OK')
      CALL testE%setQuietMode((/.FALSE.,.FALSE.,.FALSE.,.FALSE./))
      
      COMPONENT_TEST('setVerboseMode')
      CALL testE%setVerboseMode(.TRUE.)
      ASSERT(testE%isVerboseMode(),'%setVerboseMode(T)')
      ASSERT(testE%isVerboseMode(EXCEPTION_INFORMATION),'%setVerboseMode(T) INFO')
      ASSERT(testE%isVerboseMode(EXCEPTION_WARNING),'%setVerboseMode(T) WARN')
      ASSERT(testE%isVerboseMode(EXCEPTION_DEBUG),'%setVerboseMode(T) DEBUG')
      ASSERT(testE%isVerboseMode(EXCEPTION_ERROR),'%setVerboseMode(T) ERROR')
      ASSERT(.NOT.testE%isVerboseMode(EXCEPTION_FATAL_ERROR),'%setVerboseMode(T) FATAL')
      CALL testE%setVerboseMode(.FALSE.)
      ASSERT(.NOT.testE%isVerboseMode(),'%setVerboseMode(F)')
      ASSERT(.NOT.testE%isVerboseMode(EXCEPTION_INFORMATION),'%setVerboseMode(F) INFO')
      ASSERT(.NOT.testE%isVerboseMode(EXCEPTION_WARNING),'%setVerboseMode(F) WARNING')
      ASSERT(.NOT.testE%isVerboseMode(EXCEPTION_DEBUG),'%setVerboseMode(F) DEBUG')
      ASSERT(.NOT.testE%isVerboseMode(EXCEPTION_ERROR),'%setVerboseMode(F) ERROR')
      ASSERT(.NOT.testE%isVerboseMode(EXCEPTION_FATAL_ERROR),'%setVerboseMode(F) FATAL')
      CALL testE%setVerboseMode(EXCEPTION_INFORMATION,.TRUE.)
      ASSERT(.NOT.testE%isVerboseMode(),'%setVerboseMode(INFO,T)')
      ASSERT(testE%isVerboseMode(EXCEPTION_INFORMATION),'%setVerboseMode(INFO,T) INFO')
      CALL testE%setVerboseMode(EXCEPTION_WARNING,.TRUE.)
      ASSERT(.NOT.testE%isVerboseMode(),'%setVerboseMode(WARN,T)')
      ASSERT(testE%isVerboseMode(EXCEPTION_WARNING),'%setVerboseMode(WARN,T) WARN')
      CALL testE%setVerboseMode(EXCEPTION_DEBUG,.FALSE.)
      ASSERT(.NOT.testE%isVerboseMode(),'%setVerboseMode(DEBUG,F)')
      ASSERT(.NOT.testE%isVerboseMode(EXCEPTION_DEBUG),'%setVerboseMode(DEBUG,F) DEBUG')
      CALL testE%setVerboseMode(EXCEPTION_ERROR,.TRUE.)
      ASSERT(.NOT.testE%isVerboseMode(),'%setVerboseMode(ERROR,T)')
      ASSERT(testE%isVerboseMode(EXCEPTION_ERROR),'%setVerboseMode(ERROR,T) ERROR')
      CALL testE%setVerboseMode(EXCEPTION_FATAL_ERROR,.TRUE.)
      ASSERT(.NOT.testE%isVerboseMode(),'%setVerboseMode(FATAL,T)')
      ASSERT(.NOT.testE%isVerboseMode(EXCEPTION_FATAL_ERROR),'%setVerboseMode(FATAL,T) FATAL')
      CALL testE%setVerboseMode(EXCEPTION_OK,.FALSE.)
      ASSERT(.NOT.testE%isVerboseMode(),'%setVerboseMode(OK,T)')
      ASSERT(.NOT.testE%isVerboseMode(EXCEPTION_OK),'%setVerboseMode(OK,T) OK')
      CALL testE%setVerboseMode((/.TRUE.,.TRUE.,.TRUE.,.TRUE./))
    ENDSUBROUTINE testDefault
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRaise()
      COMPONENT_TEST('raiseInformation()')
      CALL testE%raiseInformation('Test information')
      ASSERT(ALL(testE%getCounterAll() == (/1,0,0,0,0/)),'%counterall')
      ASSERT(testE%getCounter(EXCEPTION_INFORMATION) == 1,'%counter(INFO)')
      mesg=' - EXCEPTION_INFORMATION: Test information'
      ASSERT(testE%getLastMessage() == TRIM(mesg),'mesg')
  
      COMPONENT_TEST('raiseWarning()')
      CALL testE%raiseWarning('Test warning')
      ASSERT(ALL(testE%getCounterAll() == (/1,1,0,0,0/)),'%counterall')
      ASSERT(testE%getCounter(EXCEPTION_WARNING) == 1,'%counter(WARNING)')
      mesg='#### EXCEPTION_WARNING #### - Test warning'
      ASSERT(testE%getLastMessage() == TRIM(mesg),'mesg')
  
      COMPONENT_TEST('raiseDebug()')
      CALL testE%raiseDebug('Test debug')
      ASSERT(ALL(testE%getCounterAll() == (/1,1,1,0,0/)),'%raiseDebug')
      ASSERT(testE%getCounter(EXCEPTION_DEBUG) == 1,'%counter(DEBUG)')
      mesg='#### EXCEPTION_DEBUG_MESG #### - Test debug'
      ASSERT(testE%getLastMessage() == TRIM(mesg),'mesg')
  
      COMPONENT_TEST('raiseError()')
      CALL testE%setStopOnError(.FALSE.)
      ASSERT(.NOT.testE%isStopOnError(),'setStopOnError(F)')
      CALL testE%setStopOnError(.TRUE.)
      ASSERT(testE%isStopOnError(),'setStopOnError(T)')
      CALL testE%setStopOnError(.FALSE.)
      ASSERT(.NOT.testE%isStopOnError(),'setStopOnError(F)')
      CALL testE%raiseError('Test error')
      ASSERT(ALL(testE%getCounterAll() == (/1,1,1,1,0/)),'%raiseError')
      ASSERT(testE%getCounter(EXCEPTION_ERROR) == 1,'%counter(ERROR)')
      mesg='#### EXCEPTION_ERROR #### - Test error'
      ASSERT(testE%getLastMessage() == TRIM(mesg),'mesg')
      
      COMPONENT_TEST('initCounter()')
      CALL testE%initCounter()
      ASSERT(ALL(testE%getCounterAll() == 0),'counterAll')
      ASSERT(testE%getCounter(EXCEPTION_INFORMATION) == 0,'%counter(INFO)')
      ASSERT(testE%getCounter(EXCEPTION_WARNING) == 0,'%counter(WARN)')
      ASSERT(testE%getCounter(EXCEPTION_DEBUG) == 0,'%counter(DEBUG)')
      ASSERT(testE%getCounter(EXCEPTION_ERROR) == 0,'%counter(ERROR)')
      ASSERT(testE%getCounter(EXCEPTION_FATAL_ERROR) == 0,'%counter(FATAL)')
      ASSERT(testE%getLastMessage() == '','mesg')
    ENDSUBROUTINE testRaise
!
!-------------------------------------------------------------------------------
    SUBROUTINE testLogFile()
      COMPONENT_TEST('setLogFileUnit()')
      CALL testE%setQuietMode(.TRUE.)
      CALL testE%setLogFileUnit(OUTPUT_UNIT)
      CALL testE%setLogFileUnit(ERROR_UNIT)
      CALL testE%setLogFileUnit(-1)
      ASSERT(testE%getCounter(EXCEPTION_WARNING) == 3,'%counter(WARN)')
      mesg='#### EXCEPTION_WARNING #### - '// &
        'Illegal unit number for log file. Log file unit not set.'
      ASSERT(TRIM(testE%getLastMessage()) == TRIM(mesg),'%getLastMessage')
      CALL testE%setLogFileUnit(23)
      ASSERT(testE%getLogFileUnit() == 23,'setLogFileUnit(23)')
      
      COMPONENT_TEST('isLogActive()')
      CALL testE%setQuietMode(.FALSE.)
      ASSERT(.NOT.testE%isLogActive(),'%isLogActive')
      CALL testE%setLogActive(.TRUE.)
      ASSERT(.NOT.testE%isLogActive(),'%setLogActive')
      OPEN(UNIT=testE%getLogFileUnit(),FILE='Exception.log', &
           ACCESS='SEQUENTIAL',FORM='FORMATTED')
      CALL testE%setLogActive(.TRUE.)
      ASSERT(testE%isLogActive(),'%setLogActive')
      CALL testE%setLogActive(.FALSE.)
      ASSERT(.NOT.testE%isLogActive(),'%setLogActive')
      CALL testE%setLogActive(.TRUE.)
      CALL testE%setQuietMode(.TRUE.)
      CALL testE%raiseInformation('Test information')
      CALL testE%raiseWarning('Test warning')
      CALL testE%raiseDebug('Test debug')
      CALL testE%raiseError('Test error')
      
!Verify the log file contents
      CLOSE(testE%getLogFileUnit())
      OPEN(UNIT=testE%getLogFileUnit(),FILE='Exception.log', &
           ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ')
      READ(testE%getLogFileUnit(),'(a)') mesg2
      mesg=''
      ASSERT(TRIM(mesg) == TRIM(mesg2),'blank line')
      READ(testE%getLogFileUnit(),'(a)') mesg2
      mesg='      EXCEPTION_INFORMATION: Test information'
      ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
      READ(testE%getLogFileUnit(),'(a)') mesg2
      mesg='#### EXCEPTION_WARNING ####'
      ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
      READ(testE%getLogFileUnit(),'(a)') mesg2
      mesg='      Test warning'
      ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
      READ(testE%getLogFileUnit(),'(a)') mesg2
      mesg='#### EXCEPTION_DEBUG_MESG ####'
      ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
      READ(testE%getLogFileUnit(),'(a)') mesg2
      mesg='      Test debug'
      ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
      READ(testE%getLogFileUnit(),'(a)') mesg2
      mesg='#### EXCEPTION_ERROR ####'
      ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
      READ(testE%getLogFileUnit(),'(a)') mesg2
      mesg='      Test error'
      ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
      CLOSE(testE%getLogFileUnit())
    ENDSUBROUTINE testLogFile
!
!-------------------------------------------------------------------------------
    SUBROUTINE testVerbosity()
      INTEGER(SIK) :: ioerr
      OPEN(UNIT=testE%getLogFileUnit(),FILE='Exception.log', &
           ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='REPLACE')
      CALL testE%setQuietMode(.FALSE.)
      CALL testE%setVerboseMode(EXCEPTION_INFORMATION,.FALSE.)
      CALL testE%setVerboseMode(EXCEPTION_WARNING,.FALSE.)
      CALL testE%setVerboseMode(EXCEPTION_DEBUG,.FALSE.)
      CALL testE%setVerboseMode(EXCEPTION_ERROR,.FALSE.)
      CALL testE%raiseInformation('Test information no log')
      CALL testE%raiseWarning('Test warning no log')
      CALL testE%raiseDebug('Test debug no log')
      CALL testE%raiseError('Test error no log')
      CLOSE(testE%getLogFileUnit())
      OPEN(UNIT=testE%getLogFileUnit(),FILE='Exception.log', &
           ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ')
      READ(testE%getLogFileUnit(),'(a)',IOSTAT=ioerr) mesg2
      ASSERT(ioerr == IOSTAT_END,'EOF Log file')
    ENDSUBROUTINE testVerbosity
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSurrogate()
      CALL testE2%addSurrogate(testE)
      ASSERT(testE2%isStopOnError() .EQV. testE%isStopOnError(),'isStopOnError')
      ASSERT(testE2%getLogFileUnit() == testE%getLogFileUnit(),'getLogFileUnit')
      ASSERT(testE2%isQuietMode() .EQV. testE%isQuietMode(),'isQuiet')
      ASSERT(ALL(testE2%getCounterAll() == testE%getCounterAll()),'getCounterAll')
      ASSERT(testE2%getLastMessage() == testE%getLastMessage(),'getLastMessage')
      ASSERT(testE2%isLogActive() .EQV. testE%isLogActive(),'isLogActive')
      CALL testE2%setQuietMode(.TRUE.)
      ASSERT(testE2%isQuietMode() .NEQV. testE%isQuietMode(),'isQuiet (NEQV)')
    ENDSUBROUTINE testSurrogate
!
!-------------------------------------------------------------------------------
    SUBROUTINE testAssignment()
      testE2=testE
      ASSERT(testE2%isStopOnError() .EQV. testE%isStopOnError(),'isStopOnError')
      ASSERT(testE2%getLogFileUnit() == testE%getLogFileUnit(),'getLogFileUnit')
      ASSERT(testE2%isQuietMode() .EQV. testE%isQuietMode(),'isQuiet')
      ASSERT(ALL(testE2%getCounterAll() == testE%getCounterAll()),'getCounterAll')
      ASSERT(testE2%getLastMessage() == testE%getLastMessage(),'getLastMessage')
      ASSERT(testE2%isLogActive() .EQV. testE%isLogActive(),'isLogActive')
    ENDSUBROUTINE testAssignment
!
!-------------------------------------------------------------------------------
    SUBROUTINE testReset()
      CALL testE%reset()
      ASSERT(.NOT.testE%isQuietMode(),'%isQuietMode()')
      ASSERT(testE%isStopOnError(),'%isStopOnError')
      ASSERT(testE%getLogFileUnit() == 666,'%getLogFileUnit')
      ASSERT(ALL(testE%getCounterAll() == 0),'getCounterAll()')
      ASSERT(testE%getLastMessage() == '','%getLastMessage()')
      ASSERT(.NOT.testE%isLogActive(),'%isLogActive')
    ENDSUBROUTINE testReset
!
ENDPROGRAM testExceptionHandler
