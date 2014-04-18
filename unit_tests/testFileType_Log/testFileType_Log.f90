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
