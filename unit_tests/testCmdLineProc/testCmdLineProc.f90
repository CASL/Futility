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
#include "UnitTest.h"
  USE UnitTest
  USE Strings
  USE ExceptionHandler
  USE CommandLineProcessor
  
  IMPLICIT NONE

  EXTERNAL testProcArgs
  
  TYPE(CmdLineProcType) :: testCLP
  CHARACTER(LEN=128) :: str
  TYPE(ExceptionHandlerType),TARGET :: e

  !Configure exception handler of CmdLineProc for testing
  CALL testCLP%e%addSurrogate(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)

  CREATE_TEST('COMMAND_LINE_PROCESSOR')
  
  REGISTER_SUBTEST('ExecName',testExecName)
  REGISTER_SUBTEST('Usage',testUsage)
  REGISTER_SUBTEST('Options',testOptions)
  REGISTER_SUBTEST('Processing',testProcessing)
  
  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testExecName()
      ASSERT(testCLP%getExecName() == '','%getExecName() (uninit)')
      CALL testCLP%setExecName('test.exe')
      ASSERT(TRIM(testCLP%getExecName()) == 'test.exe','set/getExecName')
    ENDSUBROUTINE testExecname
!
!-------------------------------------------------------------------------------
    SUBROUTINE testUsage()
      CALL testCLP%defineUsage('[[-help] | [input_file] [output_file]]')
      !No assertionis possible here
    ENDSUBROUTINE testUsage
!
!-------------------------------------------------------------------------------
    SUBROUTINE testOptions()
      TYPE(StringType) :: optStr
      CALL testCLP%clearOpts()
      CALL testCLP%setExecName('test5.exe')
      CALL testCLP%defineUsage('[[-help] | [input_file] [output_file]]')
      
      CALL testCLP%setNumOpts(-1)
      ASSERT(testCLP%getNumOpts() == 0,'%setNumOpts(-1)')
      CALL testCLP%setNumOpts(3)
      ASSERT(testCLP%getNumOpts() == 3,'%setNumOpts(3)')
      CALL testCLP%setNumOpts(4)
      ASSERT(testCLP%getNumOpts() == 3,'%setNumOpts(4)')
      CALL testCLP%defineOpt(0,'Oops!','Oh no!')
      CALL testCLP%defineOpt(1,'','')
      CALL testCLP%defineOpt(1,'This is way way way way way too long','')
      CALL testCLP%defineOpt(1,'-help','Displays this help message.')
      CALL testCLP%defineOpt(2,' input_file','Name of the input file')
      CALL testCLP%defineOpt(3,' output_file','Name of the output file')
      CALL testCLP%DisplayHelp()
      CALL testCLP%getOptName(1,optStr)
      ASSERT(optStr == '-help','optName=-help')
      CALL testCLP%getOptName(2,optStr)
      ASSERT(optStr == 'input_file','optName=input_file')
      CALL testCLP%getOptName(3,optStr)
      ASSERT(optStr == 'output_file','optName=output_file')
      CALL testCLP%clearOpts()
      CALL testCLP%getOptName(1,optStr)
      ASSERT(LEN(optStr) == 0,'Empty 1')
      CALL testCLP%getOptName(2,optStr)
      ASSERT(LEN(optStr) == 0,'Empty 2')
      CALL testCLP%getOptName(3,optStr)
      ASSERT(LEN(optStr) == 0,'Empty 3')
      ASSERT(testCLP%getNumOpts() == 0,'%clearOpts()')
      CALL testCLP%DisplayHelp()
      CALL testCLP%clearOpts()
    ENDSUBROUTINE testOptions
!
!-------------------------------------------------------------------------------
    SUBROUTINE testProcessing()
      TYPE(StringType) :: tmpStr
    
      CALL testCLP%clearCmdLine()
      CALL testCLP%getCmdArg(1,str)
      CALL testCLP%setCmdLine('')
      CALL testCLP%clearCmdLine()
      CALL testCLP%setCmdLine()
      ASSERT(testCLP%getNargs() == 0,'%getNargs()')
      str='test.inp'
      CALL testCLP%setCmdLine(str)
      ASSERT(str == testCLP%cmdline,'%cmdline')
      CALL testCLP%setCmdLine()
      ASSERT(testCLP%getNargs() == 1,'%getNargs()')
      str=''
      CALL testCLP%getCmdArg(1,str(1:2))
      ASSERT(TRIM(str) == 'te','%getCmdArg(...)')
      CALL testCLP%getCmdArg(1,tmpStr)
      ASSERT(tmpStr == 'test.inp','%getCmdArg(...)')
      
      CALL testCLP%ProcCmdLineArgs(testProcArgs)
      CALL testCLP%clearCmdLine()
      ASSERT(testCLP%getNargs() == 0,'%clearCmdLine()')
      str='''/some directory/directory/file.file'' arg2 arg3'
      CALL testCLP%setCmdLine(str)
      ASSERT(testCLP%getNargs() == 3,'%setCmdLine(str)')
      CALL testCLP%clear()
    ENDSUBROUTINE testProcessing
!
ENDPROGRAM testCmdLineProc

SUBROUTINE testProcArgs(tclp)
  USE UnitTest
  USE CommandLineProcessor
  CLASS(CmdLineProcType),INTENT(INOUT) :: tclp
  COMPONENT_TEST('testProcArgs')
  ASSERT(tclp%getNargs() == 1,'nargs')
ENDSUBROUTINE testProcArgs
