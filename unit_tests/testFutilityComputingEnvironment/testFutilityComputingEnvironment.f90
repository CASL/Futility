!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testFutilityComputingEnvironment
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE Strings
USE ParallelEnv
USE ExceptionHandler
USE FileType_Log
USE FileType_HDF5
USE MemProf
USE Times
USE FileType_Input
USE FileType_XML
USE FutilityComputingEnvironmentModule

IMPLICIT NONE

TYPE(ParallelEnvType),TARGET :: parEnv
TYPE(ExceptionHandlerType),TARGET :: exceptHandler
TYPE(LogFileType),TARGET :: logFile
TYPE(InputFileType),TARGET :: inputFile
TYPE(XMLFileType),TARGET :: xmlFile
TYPE(HDF5FileType),TARGET :: h5outFile
TYPE(Memory_Profiler),TARGET :: memProf
TYPE(FutilityComputingEnvironment) :: testCompEnv

CREATE_TEST('FutilityComputingEnvironment')

testCompEnv%exceptHandler => exceptHandler
CALL exceptHandler%setStopOnError(.FALSE.)

REGISTER_SUBTEST('Timers',testTimers)
REGISTER_SUBTEST('Sub-environments',testSubEnvs)
REGISTER_SUBTEST('clear',testClear)

FINALIZE_TEST()

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testTimers()
  CLASS(TimerType),POINTER :: timer,timer2

  COMPONENT_TEST('addTimer')
  ASSERT_EQ(testCompEnv%nTimers,0,'%nTimers')
  timer => testCompEnv%addTimer('timer1')
  ASSERT_EQ(testCompEnv%nTimers,1,'%nTimers')
  ASSERT(ASSOCIATED(timer),'%addTimer')
  timer => testCompEnv%addTimer('timer2')
  ASSERT_EQ(testCompEnv%nTimers,2,'%nTimers')
  ASSERT(ASSOCIATED(timer),'%addTimer')
  timer => testCompEnv%addTimer('timer3')
  ASSERT_EQ(testCompEnv%nTimers,3,'%nTimers')
  ASSERT(ASSOCIATED(timer),'%addTimer')
  timer => testCompEnv%addTimer('timer4 -> subtimer')
  ASSERT_EQ(testCompEnv%nTimers,4,'%nTimers')
  ASSERT(ASSOCIATED(timer),'%addTimer')
  SELECTTYPE(timer)
  TYPE IS(TimerType)
    ASSERT(.TRUE.,'timer type')
    ASSERT_EQ(timer%getTimerName(),'subtimer','timer name')
  CLASS DEFAULT
    ASSERT(.FALSE.,'timer type')
  ENDSELECT
  timer => testCompEnv%addTimer('timer4 -> subtimer2 -> subsubtimer')
  ASSERT_EQ(testcompEnv%nTimers,4,'%nTimers')
  ASSERT(ASSOCIATED(timer),'%addTimer')
  SELECTTYPE(timer)
  TYPE IS(TimerType)
    ASSERT(.TRUE.,'timer type')
    ASSERT_EQ(timer%getTimerName(),'subsubtimer','timer name')
  CLASS DEFAULT
    ASSERT(.FALSE.,'timer type')
  ENDSELECT
  timer => testCompEnv%addTimer('timer1 -> subtimer')
  ASSERT(.NOT.ASSOCIATED(timer),'bad %addTimer')

  COMPONENT_TEST('getTimer')
  timer => testCompEnv%getTimer('timer1')
  ASSERT(ASSOCIATED(timer),'%getTimer')
  timer => testCompEnv%getTimer('timer2')
  ASSERT(ASSOCIATED(timer),'%getTimer')
  timer => testCompEnv%getTimer('timer3')
  ASSERT(ASSOCIATED(timer),'%getTimer')
  timer => testCompEnv%getTimer('timer4')
  ASSERT(ASSOCIATED(timer),'%getTimer')
  SELECTTYPE(timer)
  TYPE IS(ParentTimerType)
    ASSERT(.TRUE.,'parent timer type')
    timer2 => timer%getTimer('subtimer')
    ASSERT(ASSOCIATED(timer2),'getting subtimer')
  CLASS DEFAULT
    ASSERT(.FALSE.,'parent timer type')
  ENDSELECT
  timer => testCompEnv%getTimer('timer4 -> subtimer')
  ASSERT(ASSOCIATED(timer),'%getTimer')
  SELECTTYPE(timer)
  TYPE IS(TimerType)
    ASSERT(.TRUE.,'timer type')
    ASSERT_EQ(timer%getTimerName(),'subtimer','timer name')
  CLASS DEFAULT
    ASSERT(.FALSE.,'timer type')
  ENDSELECT
  timer => testCompEnv%getTimer('timer4 -> subtimer2 -> subsubtimer')
  ASSERT(ASSOCIATED(timer),'%getTimer')
  SELECTTYPE(timer)
  TYPE IS(TimerType)
    ASSERT(.TRUE.,'timer type')
    ASSERT_EQ(timer%getTimerName(),'subsubtimer','timer name')
  CLASS DEFAULT
    ASSERT(.FALSE.,'timer type')
  ENDSELECT
  timer => testCompEnv%getTimer('timer1 -> subtimer')
  ASSERT(.NOT.ASSOCIATED(timer),'%getTimer')
  timer => testCompEnv%getTimer('timer5')
  ASSERT(.NOT.ASSOCIATED(timer),'%getTimer')

  COMPONENT_TEST('removeTimer')
  CALL testCompEnv%removeTimer('timer2')
  ASSERT_EQ(testCompEnv%nTimers,3,'%nTimers')
  timer => testCompEnv%getTimer('timer2')
  ASSERT(.NOT.ASSOCIATED(timer),'%removeTimer')
  CALL testCompEnv%removeTimer('timer4 -> subtimer')
  ASSERT_EQ(testCompEnv%nTimers,3,'%nTimers')
  timer => testCompEnv%getTimer('timer4 -> subtimer')
  ASSERT(ASSOCIATED(timer),'ASSOCIATED subtimer')
  CALL testCompEnv%removeTimer('timer4')
  ASSERT_EQ(testCompEnv%nTimers,2,'%nTimers')
  timer => testCompEnv%getTimer('timer4 -> subtimer')
  ASSERT(.NOT.ASSOCIATED(timer),'ASSOCIATED subtimer')
  timer => testCompEnv%getTimer('timer4')
  ASSERT(.NOT.ASSOCIATED(timer),'ASSOCIATED subtimer')

  COMPONENT_TEST('clearTimers')
  CALL testCompEnv%clearTimers()
  ASSERT_EQ(testCompEnv%nTimers,0,'%nTimers')
  timer => testCompEnv%getTimer('timer1')
  ASSERT(.NOT.ASSOCIATED(timer),'%clearTimers')
  timer => testCompEnv%getTimer('timer3')
  ASSERT(.NOT.ASSOCIATED(timer),'%clearTimers')

ENDSUBROUTINE testTimers
!
!-------------------------------------------------------------------------------
SUBROUTINE testSubEnvs()
  TYPE(FutilityComputingEnvironment),POINTER :: subCompEnv

  testCompEnv%name='test'

  COMPONENT_TEST('addSubCompEnv')
  ASSERT_EQ(testCompEnv%nSubCompEnvs,0,'%nSubCompEnvs')
  subCompEnv => testCompEnv%addSubCompEnv('subCompEnv1')
  ASSERT_EQ(testCompEnv%nSubCompEnvs,1,'%nSubCompEnvs')
  ASSERT(ASSOCIATED(subCompEnv),'%addSubCompEnv')
  subCompEnv => testCompEnv%addSubCompEnv('subCompEnv2')
  ASSERT_EQ(testCompEnv%nSubCompEnvs,2,'%nSubCompEnvs')
  ASSERT(ASSOCIATED(subCompEnv),'%addSubCompEnv')
  subCompEnv => testCompEnv%addSubCompEnv('subCompEnv3')
  ASSERT_EQ(testCompEnv%nSubCompEnvs,3,'%nSubCompEnvs')
  ASSERT(ASSOCIATED(subCompEnv),'%addSubCompEnv')

  COMPONENT_TEST('getSubCompEnv')
  subCompEnv => testCompEnv%getSubCompEnv('subCompEnv1')
  ASSERT(ASSOCIATED(subCompEnv),'%getSubCompEnv')
  subCompEnv => testCompEnv%getSubCompEnv('subCompEnv2')
  ASSERT(ASSOCIATED(subCompEnv),'%getSubCompEnv')
  subCompEnv => testCompEnv%getSubCompEnv('subCompEnv3')
  ASSERT(ASSOCIATED(subCompEnv),'%getSubCompEnv')

  COMPONENT_TEST('removeSubCompEnv')
  CALL testCompEnv%removeSubCompEnv('subCompEnv2')
  ASSERT_EQ(testCompEnv%nSubCompEnvs,2,'%nSubCompEnvs')
  subCompEnv => testCompEnv%getSubCompEnv('subCompEnv2')
  ASSERT(.NOT.ASSOCIATED(subCompEnv),'%removeSubCompEnv')

  COMPONENT_TEST('clearSubCompEnvs')
  CALL testCompEnv%clearSubCompEnvs()
  ASSERT_EQ(testCompEnv%nSubCompEnvs,0,'%nSubCompEnvs')
  subCompEnv => testCompEnv%getSubCompEnv('subCompEnv1')
  ASSERT(.NOT.ASSOCIATED(subCompEnv),'%clearSubCompEnvs')
  subCompEnv => testCompEnv%getSubCompEnv('subCompEnv3')
  ASSERT(.NOT.ASSOCIATED(subCompEnv),'%clearSubCompEnvs')

ENDSUBROUTINE testSubEnvs
!
!-------------------------------------------------------------------------------
SUBROUTINE testClear()

  testCompEnv%name='test'
  testCompEnv%parEnv => parEnv
  testCompEnv%exceptHandler => exceptHandler
  testCompEnv%logFile => logFile
  testCompEnv%inputFile => inputFile
  testCompEnv%xmlFile => xmlFile
  testCompEnv%h5outFile => h5outFile
  testCompEnv%memProf => memProf

  CALL testCompEnv%clear()
  ASSERT_EQ(LEN_TRIM(testCompEnv%name),0,'%name')
  ASSERT(.NOT.ASSOCIATED(testCompEnv%parEnv),'ASSOCIATED %parEnv')
  ASSERT(.NOT.ASSOCIATED(testCompEnv%exceptHandler),'ASSOCIATED %exceptHandler')
  ASSERT(.NOT.ASSOCIATED(testCompEnv%logFile),'ASSOCIATED %logFile')
  ASSERT(.NOT.ASSOCIATED(testCompEnv%inputFile),'ASSOCIATED %inputFile')
  ASSERT(.NOT.ASSOCIATED(testCompEnv%xmlFile),'ASSOCIATED %xmlFile')
  ASSERT(.NOT.ASSOCIATED(testCompEnv%h5outFile),'ASSOCIATED %h5outFile')
  ASSERT(.NOT.ASSOCIATED(testCompEnv%memProf),'ASSOCIATED %memProf')
  ASSERT_EQ(testCompEnv%nTimers,0,'%nTimers')
  ASSERT_EQ(testCompEnv%nSubCompEnvs,0,'%nSubCompEnvs')

ENDSUBROUTINE testClear
!
ENDPROGRAM testFutilityComputingEnvironment
