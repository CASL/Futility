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
PROGRAM testCmdLineProc
      
  USE CommandLineProcessor
  USE ExceptionHandler
  IMPLICIT NONE

  EXTERNAL testProcArgs
  
  TYPE(CmdLineProcType) :: testCLP
  INTEGER :: iarg
  CHARACTER(LEN=MAX_ARG_STRING_LENGTH) :: str
  CHARACTER(LEN=MAX_ARG_STRING_LENGTH+10) :: longstr
  TYPE(ExceptionHandlerType),POINTER :: e

  !Configure exception handler of CmdLineProc for testing
  ALLOCATE(e)
  testCLP%e => e
  CALL testCLP%e%setStopOnError(.FALSE.)
  CALL testCLP%e%setQuietMode(.TRUE.)

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING COMMAND LINE PROCESSOR...'
  WRITE(*,*) '==================================================='

  WRITE(*,*) 'TESTING PARAMETERS'
  WRITE(*,*) '  Passed: MAX_ARG_STRING_LENGTH = ',MAX_ARG_STRING_LENGTH
  WRITE(*,*) '  Passed:   MAX_EXECNAME_LENGTH = ',MAX_EXECNAME_LENGTH
  WRITE(*,*) '  Passed: MAX_CMD_LINE_OPT_NAME = ',MAX_CMD_LINE_OPT_NAME
  WRITE(*,*) '  Passed: MAX_CMD_LINE_OPT_DESC = ',MAX_CMD_LINE_OPT_DESC
  WRITE(*,*) '---------------------------------------------------'

  WRITE(*,*) 'TESTING METHODS FOR EXE NAME AND USAGE'
  str=testCLP%getExecName()
  CALL testCLP%setExecName('This is a really long name that is too long to fit.exe')
  testCLP%e => NULL()
  CALL testCLP%setExecName('This is a really long name that is too long to fit.exe')
  testCLP%e => e
  CALL testCLP%setExecName('MPACT.exe')
  IF(TRIM(testCLP%getExecName()) /= 'MPACT.exe') THEN
    WRITE(*,*) 'CALL testCLP%setExecName(''MPACT.exe'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testCLP%setExecName(''MPACT.exe'')'
    WRITE(*,*) '  Passed: CALL testCLP%getExecName()'
  ENDIF
  longstr='[[-help] | [input_file] [output_file]]'
  longstr(MAX_ARG_STRING_LENGTH+10:MAX_ARG_STRING_LENGTH+10)='.'
  CALL testCLP%defineUsage(longstr)
  testCLP%e => NULL()
  CALL testCLP%defineUsage(longstr)
  testCLP%e => e
  WRITE(*,*) '  Passed: CALL testCLP%defineUsage(''[[-help] | [input_file] [output_file]]'')'
  WRITE(*,*) '---------------------------------------------------'
  
  WRITE(*,*) 'TESTING METHODS FOR DEFINING OPTIONS'
  CALL testCLP%clearOpts()
  CALL testCLP%setNumOpts(-1)
  IF(testCLP%getNumOpts() /= 0) THEN
    WRITE(*,*) 'testCLP%getNumOpts() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testCLP%getNumOpts()'
  ENDIF
  testCLP%e => NULL()
  CALL testCLP%setNumOpts(3)
  testCLP%e => e
  IF(testCLP%getNumOpts() /= 3) THEN
    WRITE(*,*) 'CALL testCLP%setNumOpts(3) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testCLP%getNumOpts(3)'
  ENDIF
  CALL testCLP%setNumOpts(4)
  IF(testCLP%getNumOpts() /= 3) THEN
    WRITE(*,*) 'CALL testCLP%setNumOpts(4) FAILED!'
    STOP 666
  ENDIF
  CALL testCLP%defineOpt(0,'Oops!','Oh no!')
  CALL testCLP%defineOpt(1,'','')
  CALL testCLP%defineOpt(1,'This is way way way way way too long','')
  testCLP%e => NULL()
  CALL testCLP%defineOpt(1,'This is way way way way way too long','')
  testCLP%e => e
  longstr='Displays this help message.'
  longstr(MAX_ARG_STRING_LENGTH+10:MAX_ARG_STRING_LENGTH+10)='.'
  CALL testCLP%defineOpt(1,'-help',longstr)
  WRITE(str,'(a,i4,a)') ' Name of the input file (max.', &
                        MAX_ARG_STRING_LENGTH,' characters)'
  CALL testCLP%defineOpt(2,' input_file',TRIM(str))
  WRITE(str,'(a,i4,a)') 'Name of the output file (max.', &
                        MAX_ARG_STRING_LENGTH,' characters)'
  CALL testCLP%defineOpt(3,' output_file',TRIM(str))
  WRITE(*,*) '  Passed: CALL testCLP%defineOpt(...)'
  CALL testCLP%DisplayHelp()
  WRITE(*,*) '  Passed: CALL testCLP%DisplayHelp()'
  CALL testCLP%clearOpts()
  IF(testCLP%getNumOpts() /= 0) THEN
    WRITE(*,*) 'CALL testCLP%clearOpts() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testCLP%clearOpts()'
  ENDIF
  testCLP%e => NULL()
  CALL testCLP%clearOpts()
  testCLP%e => e
  WRITE(*,*) '---------------------------------------------------'
  
  WRITE(*,*) 'TESTING COMMAND LINE PROCESSING'
  CALL testCLP%clearCmdLine()
  CALL testCLP%getCmdArg(1,str)
  CALL testCLP%setCmdLine('')
  CALL testCLP%clearCmdLine()
  CALL testCLP%setCmdLine()
  IF(testCLP%getNargs() /= 0) THEN
    WRITE(*,*) 'CALL testCLP%setCmdLine() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testCLP%setCmdLine()'
  ENDIF
  longstr='test.inp'
  longstr(MAX_ARG_STRING_LENGTH+10:MAX_ARG_STRING_LENGTH+10)='.'
  CALL testCLP%setCmdLine(longstr)
  testCLP%e => NULL()
  CALL testCLP%setCmdLine()
  testCLP%e => e
  IF(testCLP%getNargs() /= 1) THEN
    WRITE(*,*) 'testCLP%getNargs() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testCLP%getNargs()'
  ENDIF
  CALL testCLP%getCmdArg(1,str)
  IF(TRIM(str) /= 'test.inp') THEN
    WRITE(*,*) 'CALL testCLP%getCmdArg(1,str) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testCLP%setCmdLine(...)'
    WRITE(*,*) '  Passed: CALL testCLP%getCmdLine(...)'
  ENDIF
  CALL testCLP%ProcCmdLineArgs(testProcArgs)
  CALL testCLP%clearCmdLine()
  IF(testCLP%getNargs() /= 0) THEN
    WRITE(*,*) 'CALL testCLP%clearCmdLine() FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testCLP%clearCmdLine()'
  ENDIF
  longstr='''/some directory/directory/file.file'' arg2 arg3'
  CALL testCLP%setCmdLine(longstr)
  IF(testCLP%getNargs() /= 3) THEN
    WRITE(*,*) 'CALL testCLP%getCmdLine(...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testCLP%getCmdLine(...) (space in path)'
  ENDIF
  testCLP%e => NULL()
  CALL testCLP%clearCmdLine()
  testCLP%e => e
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING COMMAND LINE PROCESSOR PASSED!'
  WRITE(*,*) '==================================================='
  DEALLOCATE(e)
ENDPROGRAM testCmdLineProc

SUBROUTINE testProcArgs(tclp)
  USE CommandLineProcessor
  CLASS(CmdLineProcType),INTENT(INOUT) :: tclp
  
  IF(tclp%getNargs() /= 1) THEN
    WRITE(*,*) 'CALL testCLP%ProcCmdLineArgs(testProcArgs) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testCLP%ProcCmdLineArgs(testProcArgs)'
  ENDIF
ENDSUBROUTINE testProcArgs
