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
MODULE DummyCPFile
  USE IntrType
  USE FileType_Base
  USE FileType_DA32
  USE FileType_Checkpoint
  IMPLICIT NONE
  PRIVATE
  
  PUBLIC :: DummyCPFileType
  
  TYPE,EXTENDS(CheckpointFileType) :: DummyCPFileType
    INTEGER(SIK) :: ninterrupts=0
    INTEGER(SIK) :: nexports=0
    INTEGER(SIK) :: nimports=0
    CONTAINS
      PROCEDURE,PASS :: init => initFileDummy
      PROCEDURE,PASS :: importFile => importFileDummy
      PROCEDURE,PASS :: exportFile => exportFileDummy
  ENDTYPE DummyCPFileType

CONTAINS
  SUBROUTINE initFileDummy(thisCPF)
    CLASS(DummyCPFileType),INTENT(INOUT) :: thisCPF
    CLASS(BaseFileType),POINTER :: dummy
    ALLOCATE(DA32FileType :: dummy)
    thisCPF%version='0.0'
    SELECTTYPE(dummy); TYPE IS(DA32FileType)
      CALL dummy%initialize(FILE='./testCPF.dcp')
    ENDSELECT
    CALL thisCPF%initBase(dummy)
  ENDSUBROUTINE initFileDummy
  SUBROUTINE importFileDummy(thisCPF)
    CLASS(DummyCPFileType),INTENT(INOUT) :: thisCPF
    IF(thisCPF%isInit) thisCPF%nimports=thisCPF%nimports+1
  ENDSUBROUTINE importFileDummy
  SUBROUTINE exportFileDummy(thisCPF)
    CLASS(DummyCPFileType),INTENT(INOUT) :: thisCPF
    IF(thisCPF%isInit) thisCPF%nexports=thisCPF%nexports+1
    IF(thisCPF%calledFromInterrupt()) thisCPF%ninterrupts=thisCPF%ninterrupts+1
  ENDSUBROUTINE exportFileDummy
ENDMODULE DummyCPFile
!===============================================================================
!===============================================================================
!===============================================================================
PROGRAM testFileType_Checkpoint
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE Strings
  USE ExceptionHandler
  USE IO_Strings
  USE FileType_Checkpoint
  USE DummyCPFile
  
  IMPLICIT NONE
  
  TYPE(ExceptionHandlerType),TARGET :: eTest
  TYPE(DummyCPFileType):: testCPFile

  CALL eTest%setStopOnError(.FALSE.)
  CALL eTest%setQuietMode(.TRUE.)
  CALL testCPFile%e%addSurrogate(eTest)
  
  CREATE_TEST("CHECKPOINT FILE")
  REGISTER_SUBTEST('Uninitialized',testUninit)
  CALL testCPFile%init()
  REGISTER_SUBTEST('Base methods',testBaseMethods)
  REGISTER_SUBTEST('Checkpoint methods',testCPMethods)
  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
  SUBROUTINE testUninit()
    TYPE(StringType) :: str1,str2,str3
    
    COMPONENT_TEST('Uninit. State')
    ASSERT(.NOT.ASSOCIATED(testCPFile%basefile),'%basefile')
    ASSERT(.NOT.ASSOCIATED(testCPFile%daf),'%daf')
    ASSERT(.NOT.ASSOCIATED(testCPFile%h5f),'%h5f')
    
    ASSERT(LEN_TRIM(testCPFile%getFilePath()) == 0,'%getFilePath()')
    ASSERT(LEN_TRIM(testCPFile%getFileName()) == 0,'%getFileName()')
    ASSERT(LEN_TRIM(testCPFile%getFileExt()) == 0,'%getFileExt()')
    CALL testCPFile%getFileParts(str1,str2,str3)
    ASSERT(LEN_TRIM(str1) == 0,'%getFileParts (path)')
    ASSERT(LEN_TRIM(str2) == 0,'%getFileParts (name)')
    ASSERT(LEN_TRIM(str3) == 0,'%getFileParts (ext)')
    ASSERT(.NOT.testCPFile%isOpen(),'%isOpen')
    ASSERT(.NOT.testCPFile%isRead(),'%isRead()')
    ASSERT(.NOT.testCPFile%isWrite(),'%isWrite()')
    ASSERT(.NOT.testCPFile%isEOF(),'%isEOF()')
    
    ASSERT(LEN_TRIM(testCPFile%version) == 0,'%version')
    ASSERT(.NOT.testCPFile%isInit,'%isInit')
    ASSERT(.NOT.testCPFile%export_on_interrupt,'%export_on_interrupt')
    ASSERT(LEN_TRIM(testCPFile%interrupt_file) == 0,'%interrupt_file')

    COMPONENT_TEST('Bad sets')
    CALL testCPFile%setFilePath('something')
    ASSERT(LEN_TRIM(testCPFile%getFilePath()) == 0,'%setFilePath')
    ASSERT(testCPFile%e%getCounter(EXCEPTION_ERROR) == 1,'%setFilePath')
    CALL testCPFile%setFileName('something')
    ASSERT(LEN_TRIM(testCPFile%getFileName()) == 0,'%setFileName')
    ASSERT(testCPFile%e%getCounter(EXCEPTION_ERROR) == 2,'%setFileName')
    CALL testCPFile%setFileExt('something')
    ASSERT(LEN_TRIM(testCPFile%getFileExt()) == 0,'%setFileExt')
    ASSERT(testCPFile%e%getCounter(EXCEPTION_ERROR) == 3,'%setFileExt')
    CALL testCPFile%setEOFstat(.TRUE.)
    ASSERT(.NOT.testCPFile%isEOF(),'%isEOF()')
    ASSERT(testCPFile%e%getCounter(EXCEPTION_ERROR) == 4,'%setEOFstat')
    CALL testCPFile%setOpenstat(.TRUE.)
    ASSERT(.NOT.testCPFile%isOpen(),'%isOpen')
    ASSERT(testCPFile%e%getCounter(EXCEPTION_ERROR) == 5,'%setOpenstat')
    CALL testCPFile%setReadstat(.TRUE.)
    ASSERT(.NOT.testCPFile%isRead(),'%isRead()')
    ASSERT(testCPFile%e%getCounter(EXCEPTION_ERROR) == 6,'%setReadstat')
    CALL testCPFile%setWritestat(.TRUE.)
    ASSERT(.NOT.testCPFile%isWrite(),'%isWrite()')
    ASSERT(testCPFile%e%getCounter(EXCEPTION_ERROR) == 7,'%setWritestat')
    
    COMPONENT_TEST('Bad Ops.')
    CALL testCPFile%fopen()
    ASSERT(testCPFile%e%getCounter(EXCEPTION_ERROR) == 8,'%fopen')
    CALL testCPFile%fclose()
    ASSERT(testCPFile%e%getCounter(EXCEPTION_ERROR) == 9,'%fclose')
    CALL testCPFile%fdelete()
    ASSERT(testCPFile%e%getCounter(EXCEPTION_ERROR) == 10,'%fdelete')
    CALL testCPFile%setInterruptFile('somefile')
    ASSERT(testCPFile%e%getCounter(EXCEPTION_ERROR) == 11,'%setInterruptFile')
    ASSERT(LEN_TRIM(testCPFile%interrupt_file) == 0,'%interrupt_file')
    CALL testCPFile%setExportOnInterrupt(.TRUE.)
    ASSERT(testCPFile%e%getCounter(EXCEPTION_ERROR) == 12,'%exportOnInterrupt')
    ASSERT(.NOT.testCPFile%export_on_interrupt,'%export_on_interrupt')
    CALL testCPFile%checkForFileInterrupt()

    COMPONENT_TEST('clear')
    CALL testCPFile%clear()
    
  ENDSUBROUTINE testUninit 
!
!-------------------------------------------------------------------------------
  SUBROUTINE testBaseMethods()
    LOGICAL(SBK) :: lexist,bool
    TYPE(StringType) :: str1,str2,str3

    COMPONENT_TEST('FileParts')
    ASSERT(TRIM(testCPFile%getFilePath()) == '.'//SLASH,'%getFilePath()')
    ASSERT(TRIM(testCPFile%getFileName()) == 'testCPF','%getFileName()')
    ASSERT(TRIM(testCPFile%getFileExt()) == '.dcp','%getFileExt()')
    CALL testCPFile%getFileParts(str1,str2,str3)
    ASSERT(str1 == '.'//SLASH,'%getFileParts (path)')
    ASSERT(str2 == 'testCPF','%getFileParts (name)')
    ASSERT(str3 == '.dcp','%getFileParts (ext)')

    COMPONENT_TEST('State')
    ASSERT(testCPFile%isRead(),'%isRead')
    ASSERT(testCPFile%isWrite(),'%isWrite')
    ASSERT(.NOT.testCPFile%isEOF(),'%isEOF')
    ASSERT(.NOT.testCPFile%isOpen(),'%isOpen')

    COMPONENT_TEST('set*')
    CALL testCPFile%setFilePath('some/')
    ASSERT(TRIM(testCPFile%getFilePath()) == 'some/','%setFilePath')
    bool=TRIM(testCPFile%getFilePath()) == TRIM(testCPFile%basefile%getFilePath())
    ASSERT(bool,'path basefile')
    CALL testCPFile%setFileName('thing')
    ASSERT(TRIM(testCPFile%getFileName()) == 'thing','%setFileName')
    bool=TRIM(testCPFile%getFileName()) == TRIM(testCPFile%basefile%getFileName())
    ASSERT(bool,'name basefile')
    CALL testCPFile%setFileExt('.smg')
    ASSERT(TRIM(testCPFile%getFileExt()) == '.smg','%setFileExt')
    bool=TRIM(testCPFile%getFileExt()) == TRIM(testCPFile%basefile%getFileExt())
    ASSERT(bool,'ext basefile')
    CALL testCPFile%setFilePath('./')
    CALL testCPFile%setFileName('testCPF')
    CALL testCPFile%setFileExt('.dcp')
    
    CALL testCPFile%setReadstat(.FALSE.)
    ASSERT(.NOT.testCPFile%isRead(),'%setReadstat')
    ASSERT(testCPFile%isRead() .EQV. testCPFile%basefile%isRead(),'base read')
    CALL testCPFile%setWritestat(.FALSE.)
    ASSERT(.NOT.testCPFile%isWrite(),'%setWritestat')
    ASSERT(testCPFile%isWrite() .EQV. testCPFile%basefile%isWrite(),'base write')
    CALL testCPFile%setOpenstat(.TRUE.)
    ASSERT(testCPFile%isOpen(),'%setOpenstat')
    ASSERT(testCPFile%isOpen() .EQV. testCPFile%basefile%isOpen(),'base open')
    CALL testCPFile%setEOFstat(.TRUE.)
    ASSERT(testCPFile%isEOF(),'%setEOFstat')
    ASSERT(testCPFile%isEOF() .EQV. testCPFile%basefile%isEOF(),'base EOF')

    CALL testCPFile%setEOFstat(.FALSE.)
    CALL testCPFile%setOpenstat(.FALSE.)
    CALL testCPFile%setReadstat(.TRUE.)
    CALL testCPFile%setWritestat(.TRUE.)

    COMPONENT_TEST('fopen')
    CALL testCPFile%fopen()
    ASSERT(testCPFile%isOpen(),'isOpen() failed')
    INQUIRE(FILE='testCPF.dcp',EXIST=lexist)
    ASSERT(lexist,'failed to create file')
    
    COMPONENT_TEST('fclose')
    CALL testCPFile%fclose()
    ASSERT(.NOT.testCPFile%isOpen(),'isOpen() failed')
    
    COMPONENT_TEST('fdelete')
    CALL testCPFile%fdelete()
    INQUIRE(FILE='testCPF.dcp',EXIST=lexist)
    ASSERT(.NOT.lexist,'failed to delete file')
    
  ENDSUBROUTINE testBaseMethods
!
!-------------------------------------------------------------------------------
  SUBROUTINE testCPMethods()
    COMPONENT_TEST('calledFromInterrupt')
    ASSERT(.NOT.testCPFIle%calledFromInterrupt(),'%interrupt')
    
    COMPONENT_TEST('importFile')
    CALL testCPFile%importFile()
    ASSERT(testCPFile%nimports == 1,'nimports')
    
    COMPONENT_TEST('exportFile')
    CALL testCPFile%exportFile()
    ASSERT(testCPFile%nexports == 1,'nexports')
    ASSERT(testCPFile%ninterrupts == 0,'ninterrupts')
    
    COMPONENT_TEST('setInterruptFile')
    CALL testCPFile%setInterruptFile('do_export')
    ASSERT(testCPFile%interrupt_file == 'do_export','interrupt_file')
    
    COMPONENT_TEST('setExportOnInterrupt')
    CALL testCPFile%setExportOnInterrupt(.TRUE.)
    ASSERT(testCPFile%export_on_interrupt,'export_on_interrupt')
    
    COMPONENT_TEST('checkForFileInterrupt')
    CALL testCPFile%checkForFileInterrupt()
    ASSERT(testCPFile%nexports == 1,'nexports')
    ASSERT(testCPFile%ninterrupts == 0,'ninterrupts')
    OPEN(UNIT=23,FILE='do_export')
    CLOSE(23)
    CALL testCPFile%checkForFileInterrupt()
    ASSERT(testCPFile%nexports == 2,'nexports')
    ASSERT(testCPFile%ninterrupts == 1,'ninterrupts')
    OPEN(UNIT=23,FILE='do_export')
    CLOSE(23,STATUS='DELETE')
    
    COMPONENT_TEST('clear')
    CALL testCPFile%clear()
  ENDSUBROUTINE testCPMethods
!
ENDPROGRAM testFileType_Checkpoint
