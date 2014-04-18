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
MODULE DummyFile
  USE FileType_Base
  
  IMPLICIT NONE
  PRIVATE
  
  PUBLIC :: DummyFileType
  
  TYPE,EXTENDS(BaseFileType) :: DummyFileType
    CONTAINS
      PROCEDURE,PASS :: fopen
      PROCEDURE,PASS :: fclose
      PROCEDURE,PASS :: fdelete
  ENDTYPE DummyFileType
CONTAINS
  SUBROUTINE fopen(file)
    CLASS(DummyFileType),INTENT(INOUT) :: file
  ENDSUBROUTINE fopen
  SUBROUTINE fclose(file)
    CLASS(DummyFileType),INTENT(INOUT) :: file
  ENDSUBROUTINE fclose
  SUBROUTINE fdelete(file)
    CLASS(DummyFileType),INTENT(INOUT) :: file
  ENDSUBROUTINE fdelete
ENDMODULE DummyFile
!===============================================================================
!===============================================================================
!===============================================================================
PROGRAM testFileType_Base
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE Strings
  USE ExceptionHandler
  USE FileType_Base
  USE DummyFile
  
  IMPLICIT NONE
  
  TYPE(StringType) :: str1,str2,str3
  TYPE(ExceptionHandlerType),TARGET :: e
  CLASS(BaseFileType),ALLOCATABLE :: testFile
      
  CREATE_TEST('FILETYPE_BASE')
  
  ALLOCATE(DummyFileType :: testFile)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  
  REGISTER_SUBTEST('uninitialized',testUninit)
  REGISTER_SUBTEST('File Name',testNames)
  REGISTER_SUBTEST('File State',testState)
  
  FINALIZE_TEST()
  DEALLOCATE(testFile)
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testUninit()
      ASSERT(.NOT.(testFile%isRead()),'%isRead()')
      ASSERT(.NOT.(testFile%isWrite()),'%isWrite()')
      ASSERT(.NOT.(testFile%isEOF()),'%isEOF()')
      ASSERT(.NOT.(testFile%isOpen()),'%isOpen()')
      ASSERT(LEN_TRIM(testFile%getFilePath()) == 0,'%getFilePath() (empty)')
      ASSERT(LEN_TRIM(testFile%getFileName()) == 0,'%getFileName() (empty)')
      ASSERT(LEN_TRIM(testFile%getFileExt()) == 0,'%getFileExt() (empty)')
      CALL testFile%getFileParts(str1,str2,str3)
      ASSERT(LEN_TRIM(str1) == 0,'%getFileParts(...) path')
      ASSERT(LEN_TRIM(str2) == 0,'%getFileParts(...) name')
      ASSERT(LEN_TRIM(str3) == 0,'%getFileParts(...) ext')
    ENDSUBROUTINE testUninit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testNames()
      COMPONENT_TEST('File Path')
      CALL testFile%setFilePath('filepath/')
      ASSERT(TRIM(testFile%getFilePath()) == 'filepath/','%getFilePath()')
      COMPONENT_TEST('File Name')
      CALL testFile%setFileName('filename')
      ASSERT(TRIM(testFile%getFileName()) == 'filename','%getFileName()')
      COMPONENT_TEST('File Extension')
      CALL testFile%setFileExt('.fext')
      ASSERT(TRIM(testFile%getFileExt()) == '.fext','%getFileExt()')
      COMPONENT_TEST('Parts')
      CALL testFile%getFileParts(str1,str2,str3)
      ASSERT(TRIM(str1) == 'filepath/','%getFileParts(...) path')
      ASSERT(TRIM(str2) == 'filename','%getFileParts(...) name')
      ASSERT(TRIM(str3) == '.fext','%getFileParts(...) ext')
    ENDSUBROUTINE testNames
!
!-------------------------------------------------------------------------------
    SUBROUTINE testState()
      COMPONENT_TEST('Read State')
      CALL testFile%e%addSurrogate(e)
      CALL testFile%setReadStat(.TRUE.)
      ASSERT(testFile%isRead(),'T')
      CALL testFile%setReadStat(.FALSE.)
      ASSERT(.NOT.testFile%isRead(),'F')
      COMPONENT_TEST('Write State')
      CALL testFile%setWriteStat(.TRUE.)
      ASSERT(testFile%isWrite(),'T')
      CALL testFile%setWriteStat(.FALSE.)
      ASSERT(.NOT.testFile%isWrite(),'F')
      COMPONENT_TEST('Open State')
      CALL testFile%setOpenStat(.TRUE.)
      ASSERT(testFile%isOpen(),'T')
      CALL testFile%setOpenStat(.FALSE.)
      ASSERT(.NOT.testFile%isOpen(),'F')
      COMPONENT_TEST('EOF State')
      CALL testFile%setEOFstat(.TRUE.) !File is not open so state is not changed
      ASSERT(.NOT.testFile%isEOF(),'T')
      CALL testFile%setOpenStat(.TRUE.)
      CALL testFile%setEOFstat(.TRUE.)
      ASSERT(testFile%isEOF(),'T')
      CALL testFile%setEOFstat(.FALSE.)
      ASSERT(.NOT.testFile%isEOF(),'F')
    ENDSUBROUTINE testState
!
ENDPROGRAM testFileType_Base
