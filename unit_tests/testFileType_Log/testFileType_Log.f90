!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testFileType_Log
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE ExceptionHandler
  USE FileType_Fortran
  USE FileType_Log

  IMPLICIT NONE

  CHARACTER(LEN=256) :: string
  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(FortranFileType) :: testFile
  TYPE(LogFileType) :: testLogFile

  CREATE_TEST('FILETYPE_LOG')

  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)

  REGISTER_SUBTEST('LogFileType',testLogFileType)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testLogFileType()
      LOGICAL :: bool
      CALL testLogFile%e%addSurrogate(e)
      ASSERT(.NOT.(testLogFile%isEcho()),'%isEcho()')
      CALL testLogFile%setEcho(.TRUE.)
      ASSERT(testLogFile%isEcho(),'%setEcho(...)')
      CALL testLogFile%initialize(UNIT=66,FILE='./test.log')
      ASSERT(TRIM(testLogFile%getFileName()) == 'test','%initialize(...)')
      CALL testLogFile%fopen()
      CALL testLogFile%message('Passed: CALL testLogFile%message(...)',.TRUE.,.TRUE.)
      CALL testLogFile%message('Passed: CALL testLogFile%message(...)',.FALSE.,.TRUE.)
      CALL testLogFile%clear(.FALSE.)
      ASSERT(testLogFile%getUnitNo() == -1,'%clear')
      CALL testFile%initialize(UNIT=66,FILE='./test.log',STATUS='OLD',ACTION='READ')
      CALL testFile%fopen()
      READ(66,'(a)') string
      bool=(TRIM(string(12:LEN(string))) == ' Passed: CALL testLogFile%message(...)')
      ASSERT(bool,'%message(...)')
      CALL testFile%clear(.TRUE.)
    ENDSUBROUTINE testLogFileType
!
ENDPROGRAM testFileType_Log
