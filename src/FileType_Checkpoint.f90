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
!> @brief Defines the base Checkpoint file type which is abstract.
!>
!> A checkpoint file is file type that is to be used for providing import and
!> export methods to the client. These methods are to be implemented such that
!> they "dump" information about the simulation to disk such that execution may
!> be halted and resumed later from the same point.
!>
!> The base checkpoint file type wraps either a DA32 file type or an HDF5 file
!> type which are the actual file formats and implementations used by the
!> checkpoint file. The type defines interfaces for import and export methods.
!> These are to be implemented in non-abstract extensions of this type which
!> are to define the import and export routines, specifically what data is
!> put into the file and read from the file.
!>
!> The abstract base checkpoint file implementation defines a generic 
!> initialization method, overloads all the base file type methods, and defines
!> a clear method. In addition to this it provides a basic feature to check
!> for an "interrupt file" and if this file exists and then the base checkpoint
!> file may call the export routine. In addition to the interrupt file other
!> code that can handle interrupt signals from the OS may want to be utilized
!> to take advantage of conditional exports.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref Strings "Strings": @copybrief Strings
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief Exceptionhandler
!>  - @ref FileType_Base "FileType_Base": @copybrief FileType_Base
!>  - @ref FileType_DA32 "FileType_DA32": @copybrief FileType_DA32
!>  - @ref FileType_HDF5 "FileType_HDF5": @copybrief FileType_HDF5
!>
!> @author Brendan Kochunas
!>   @date 02/12/2014
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FileType_Checkpoint    
  USE IntrType
  USE Strings
  USE ExceptionHandler
  USE FileType_Base
  USE FileType_DA32
  USE FileType_HDF5
  
  IMPLICIT NONE
  PRIVATE
  
  !List of Public Members
  PUBLIC :: CheckpointFileType
  PUBLIC :: clear_CheckpointFileType
  
  CHARACTER(LEN=*),PARAMETER :: modName='FILETYPE_CHECKPOINT'
  
  !> @brief The base checkpoint file type
  !>
  !> Extensions primarily provide implementations of the import and export
  !> routines. Additional attributes may be added as well to contain
  !> references to the data that will be stored in the file.
  !>
  !> This file extends the base file type and provides implementations for
  !> all the methods defined on the base type. Most of these defer the
  !> behavior to the basefile attribute.
  TYPE,ABSTRACT,EXTENDS(BaseFileType) :: CheckpointFileType
    !> The version of the checkpoint file.
    CHARACTER(LEN=3) :: version=''
    !> Initialization status for this file
    LOGICAL(SBK) :: isInit=.FALSE.
    !> Indicates whether or not exportFile is to be called when
    !> an interrupt signal is caught (not yet active) or when the interrupt
    !> file is detected.
    LOGICAL(SBK) :: export_on_interrupt=.FALSE.
    !> The name of the file to signal an interrupt.
    TYPE(StringType) :: interrupt_file
    !> The actual implementation of the checkpoint file format. It is
    !> either a DA32 file or an HDF5 file
    CLASS(BaseFileType),POINTER :: basefile => NULL()
    !> A pointer to the basefile object's extended type. It is associated
    !> when DA32 file format is being used for the checkpoint file.
    TYPE(DA32FileType),POINTER :: daf => NULL()
    !> A pointer to the basefile object's extended type. It is associated
    !> when HDF5 file format is being used for the checkpoint file.
    TYPE(HDF5FileType),POINTER :: h5f => NULL()
!
!List of type bound procedures (methods)
  CONTAINS
      !> Interface declaration for import method
      PROCEDURE(CheckPointFileType_absintfc),DEFERRED,PASS :: importFile
      !> Interface declaration for export method
      PROCEDURE(CheckPointFileType_absintfc),DEFERRED,PASS :: exportFile
      !> @copybrief FileType_Checkpoint::init_CheckPointFileType
      !> @copydetails FileType_Checkpoint::init_CheckPointFileType
      PROCEDURE,PASS :: initBase => init_CheckPointFileType
      !> @copybrief FileType_Checkpoint::setInterruptFile
      !> @copydetails FileType_Checkpoint::setInterruptFile
      PROCEDURE,PASS :: setInterruptFile
      !> @copybrief FileType_Checkpoint::setExportOnInterrupt
      !> @copydetails FileType_Checkpoint::setExportOnInterrupt
      PROCEDURE,PASS :: setExportOnInterrupt
      !> @copybrief FileType_Checkpoint::checkForFileInterrupt
      !> @copydetails FileType_Checkpoint::checkForFileInterrupt
      PROCEDURE,PASS :: checkForFileInterrupt
      !> @copybrief FileType_Checkpoint::clear_CheckpointFileType
      !> @copydetails FileType_Checkpoint::clear_CheckpointFileType
      PROCEDURE,PASS :: clear => clear_CheckpointFileType
      !> @copybrief FileType_Checkpoint::fopen_CheckpointFileType
      !> @copydetails FileType_Checkpoint::fopen_CheckpointFileType
      PROCEDURE,PASS :: fopen => fopen_CheckpointFileType
      !> @copybrief FileType_Checkpoint::fclose_CheckpointFileType
      !> @copydetails FileType_Checkpoint::fclose_CheckpointFileType
      PROCEDURE,PASS :: fclose => fclose_CheckpointFileType
      !> @copybrief FileType_Checkpoint::fdelete_CheckpointFileType
      !> @copydetails FileType_Checkpoint::fdelete_CheckpointFileType
      PROCEDURE,PASS :: fdelete => fdelete_CheckpointFileType
      !> @copybrief FileType_Checkpoint::setFilePath_CheckpointFileType
      !> @copydetails FileType_Checkpoint::setFilePath_CheckpointFileType
      PROCEDURE,PASS :: setFilePath => setFilePath_CheckpointFileType
      !> @copybrief FileType_Checkpoint::setFileName_CheckpointFileType
      !> @copydetails FileType_Checkpoint::setFileName_CheckpointFileType
      PROCEDURE,PASS :: setFileName => setFileName_CheckpointFileType
      !> @copybrief FileType_Checkpoint::setFileExt_CheckpointFileType
      !> @copydetails FileType_Checkpoint::setFileExt_CheckpointFileType
      PROCEDURE,PASS :: setFileExt => setFileExt_CheckpointFileType
      !> @copybrief FileType_CheckpointFile::getFilePath_CheckpointFileType
      !> @copydetails FileType_CheckpointFile::getFilePath_CheckpointFileType
      PROCEDURE,PASS :: getFilePath => getFilePath_CheckpointFileType
      !> @copybrief FileType_CheckpointFile::getFileName_CheckpointFileType
      !> @copydetails FileType_CheckpointFile::getFileName_CheckpointFileType
      PROCEDURE,PASS :: getFileName => getFileName_CheckpointFileType
      !> @copybrief FileType_CheckpointFile::getFileExt_CheckpointFileType
      !> @copydetails FileType_CheckpointFile::getFileExt_CheckpointFileType
      PROCEDURE,PASS :: getFileExt => getFileExt_CheckpointFileType
      !> @copybrief FileType_CheckpointFile::getFileParts_CheckpointFileType
      !> @copydetails FileType_CheckpointFile::getFileParts_CheckpointFileType
      PROCEDURE,PASS :: getFileParts => getFileParts_CheckpointFileType
      !> @copybrief FileType_Checkpoint::setEOFStat_CheckpointFileType
      !> @copydetails FileType_Checkpoint::setEOFStat_CheckpointFileType
      PROCEDURE,PASS :: setEOFstat => setEOFStat_CheckpointFileType
      !> @copybrief FileType_Checkpoint::setOpenStat_CheckpointFileType
      !> @copydetails FileType_Checkpoint::setOpenStat_CheckpointFileType
      PROCEDURE,PASS :: setOpenStat => setOpenStat_CheckpointFileType
      !> @copybrief FileType_Checkpoint::setReadStat_CheckpointFileType
      !> @copydetails FileType_Checkpoint::setReadStat_CheckpointFileType
      PROCEDURE,PASS :: setReadStat => setReadStat_CheckpointFileType
      !> @copybrief FileType_Checkpoint::setWriteStat_CheckpointFileType
      !> @copydetails FileType_Checkpoint::setWriteStat_CheckpointFileType
      PROCEDURE,PASS :: setWriteStat => setWriteStat_CheckpointFileType
      !> @copybrief FileType_Checkpoint::isOpen_CheckpointFileType
      !> @copydetails FileType_Checkpoint::isOpen_CheckpointFileType
      PROCEDURE,PASS :: isOpen => isOpen_CheckpointFileType
      !> @copybrief FileType_Checkpoint::isEOF_CheckpointFileType
      !> @copydetails FileType_Checkpoint::isEOF_CheckpointFileType
      PROCEDURE,PASS :: isEOF => isEOF_CheckpointFileType
      !> @copybrief FileType_Checkpoint::isRead_CheckpointFileType
      !> @copydetails FileType_Checkpoint::isRead_CheckpointFileType
      PROCEDURE,PASS :: isRead => isRead_CheckpointFileType
      !> @copybrief FileType_Checkpoint::isWrite_CheckpointFileType
      !> @copydetails FileType_Checkpoint::isWrite_CheckpointFileType
      PROCEDURE,PASS :: isWrite => isWrite_CheckpointFileType
  ENDTYPE CheckpointFileType
  
  !> Abstract interface used to declare import/export deferred methods.
  ABSTRACT INTERFACE
    SUBROUTINE CheckPointFileType_absintfc(thisCPF)
      IMPORT :: CheckpointFileType
      CLASS(CheckpointFileType),INTENT(INOUT) :: thisCPF
    ENDSUBROUTINE CheckPointFileType_absintfc
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the base checkpoint file type from an initialized base
!>        file type
!> @param thisCPF the checkpoint file object to initialize
!> @param myBaseFile the base file type to use for initialization
!>
!> The myBaseFile argument must be either a DA32FileType or HDF5FileType.
!>
    SUBROUTINE init_CheckPointFileType(thisCPF,myBaseFile)
      CHARACTER(LEN=*),PARAMETER :: myName='init_CheckPointFileType'
      CLASS(CheckPointFileType),INTENT(INOUT) :: thisCPF
      CLASS(BaseFileType),POINTER,INTENT(IN) :: myBaseFile
      INTEGER(SIK) :: nerror
      TYPE(DA32FileType),POINTER :: myDA32File
      TYPE(HDF5FileType),POINTER :: myHDF5File
      
      IF(ASSOCIATED(myBaseFile)) THEN
        nerror=thisCPF%e%getCounter(EXCEPTION_ERROR)
        myDa32File => NULL()
        myHDF5File => NULL()
        SELECTTYPE(myBaseFile)
          TYPE IS(DA32Filetype)
            IF(myBaseFile%isInit()) THEN
              myDA32File => myBaseFile
            ELSE
               CALL thisCPF%e%raiseError(modName//'::'//myName//' - '// &
                'The passed DA32FileType must be initialized!')
            ENDIF
          TYPE IS(HDF5FileType)
            IF(myBaseFile%isInit) THEN
              myHDF5File => myBaseFile
            ELSE
              CALL thisCPF%e%raiseError(modName//'::'//myName//' - '// &
                'The passed HDF5FileType must be initialized!')
            ENDIF
          CLASS DEFAULT
            CALL thisCPF%e%raiseError(modName//'::'//myName//' - '// &
              'The passed base file type must be either a DA32FileType '// &
                'or HDF5FileType!')
        ENDSELECT
        
        IF(nerror == thisCPF%e%getCounter(EXCEPTION_ERROR)) THEN
          thisCPF%daf => myDA32File
          thisCPF%h5f => myHDF5File
          thisCPF%basefile => myBaseFile
          CALL thisCPF%e%addSurrogate(thisCPF%basefile%e)
          
          !Set Checkpoint file values from base file type
          thisCPF%pathlen=LEN_TRIM(thisCPF%basefile%getFilePath())
          thisCPF%fnamelen=LEN_TRIM(thisCPF%basefile%getFileName())
          thisCPF%extlen=LEN_TRIM(thisCPF%basefile%getFileExt())
          
          thisCPF%isInit=.TRUE.
        ENDIF
      ELSE
        CALL thisCPF%e%raiseError(modName//'::'//myName//' - '// &
          'The passed base file type must be associated!')
      ENDIF
    ENDSUBROUTINE init_CheckPointFileType
!
!-------------------------------------------------------------------------------
!> @brief Set the name of the interrupt file to use when checking for
!>        interrupts.
!> @param thisCPF the checkpoint file object
!> @param fname the name to use for the interrupt file
!>
    SUBROUTINE setInterruptFile(thisCPF,fname)
      CHARACTER(LEN=*),PARAMETER :: myName='setInterruptFile'
      CLASS(CheckpointFileType),INTENT(INOUT) :: thisCPF
      CHARACTER(LEN=*),INTENT(IN) :: fname
      IF(thisCPF%isInit) THEN
        thisCPF%interrupt_file=fname
      ELSE
        CALL thisCPF%e%raiseError(modName//'::'//myName//' - '// &
          'Cannot set interrupt file for checkpoint file if it '// &
            'is not initialized!')
      ENDIF
    ENDSUBROUTINE setInterruptFile
!
!-------------------------------------------------------------------------------
!> @brief Set the logical on whether or not to export the checkpoint file when
!>        an interrupt is encountered.
!> @param thisCPF the checkpoint file object
!> @parm bool the value to use for exporting on interrupts
!>
    SUBROUTINE setExportOnInterrupt(thisCPF,bool)
      CHARACTER(LEN=*),PARAMETER :: myName='setExportOnInterrupt'
      CLASS(CheckpointFileType),INTENT(INOUT) :: thisCPF
      LOGICAL(SBK),INTENT(IN) :: bool
      IF(thisCPF%isInit) THEN
        thisCPF%export_on_interrupt=bool
      ELSE
        CALL thisCPF%e%raiseError(modName//'::'//myName//' - '// &
          'Cannot "set export on interrupt" for checkpoint file if it '// &
            'is not initialized!')
      ENDIF
    ENDSUBROUTINE setExportOnInterrupt
!
!-------------------------------------------------------------------------------
!> @brief Checks for the existence of the interrupt file
!> @param thisCPF the checkpoint file object
!>
    SUBROUTINE checkForFileInterrupt(thisCPF)
      CLASS(CheckpointFileType),INTENT(INOUT) :: thisCPF
      LOGICAL(SBK) :: file_exists
      IF(thisCPF%export_on_interrupt .AND. thisCPF%isInit) THEN
        file_exists=.FALSE.
        IF(LEN_TRIM(thisCPF%interrupt_file) > 0) &
          INQUIRE(FILE=TRIM(thisCPF%interrupt_file),EXIST=file_exists)
        IF(file_exists) CALL thisCPF%exportFile()
      ENDIF
    ENDSUBROUTINE checkForFileInterrupt
!
!-------------------------------------------------------------------------------
!> @brief Clears the checkpoint file object
!> @param thisCPF the checkpoint file object
!>
    SUBROUTINE clear_CheckpointFileType(thisCPF)
      CLASS(CheckpointFileType),INTENT(INOUT) :: thisCPF
      
      IF(ASSOCIATED(thisCPF%h5f)) CALL thisCPF%h5f%clear()
      IF(ASSOCIATED(thisCPF%daf)) CALL thisCPF%daf%clear()
      NULLIFY(thisCPF%h5f)
      NULLIFY(thisCPF%daf)
      IF(ASSOCIATED(thisCPF%basefile)) DEALLOCATE(thisCPF%basefile)
      thisCPF%version=''
      thisCPF%interrupt_file=''
      thisCPF%extlen=0
      thisCPF%fnamelen=0
      thisCPF%pathlen=0
      thisCPF%export_on_interrupt=.FALSE.
      thisCPF%isInit=.FALSE.
      CALL clear_base_file(thisCPF)
    ENDSUBROUTINE clear_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief The file open procedure for the checkpoint file type.
!> @param file the checkpoint file object
!>
    SUBROUTINE fopen_CheckpointFileType(file)
      CHARACTER(LEN=*),PARAMETER :: myName='fopen_CheckpointFileType'
      CLASS(CheckpointFileType),INTENT(INOUT) :: file
      IF(file%isInit) THEN
        CALL file%basefile%fopen()
      ELSE
        CALL file%e%raiseError(modName//'::'//myName//' - '// &
          'Cannot open checkpoint file if it is not initialized!')
      ENDIF
    ENDSUBROUTINE fopen_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief The file close procedure for the checkpoint file type.
!> @param file the checkpoint file object
!>
    SUBROUTINE fclose_CheckpointFileType(file)
      CHARACTER(LEN=*),PARAMETER :: myName='fclose_CheckpointFileType'
      CLASS(CheckpointFileType),INTENT(INOUT) :: file
      IF(file%isInit) THEN
        CALL file%basefile%fclose()
      ELSE
        CALL file%e%raiseError(modName//'::'//myName//' - '// &
          'Cannot close checkpoint file if it is not initialized!')
      ENDIF
    ENDSUBROUTINE fclose_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief The file delete procedure for the checkpoint file type.
!> @param file the checkpoint file object
!>
    SUBROUTINE fdelete_CheckpointFileType(file)
      CHARACTER(LEN=*),PARAMETER :: myName='fdelete_CheckpointFileType'
      CLASS(CheckpointFileType),INTENT(INOUT) :: file
      IF(file%isInit) THEN
        CALL file%basefile%fdelete()
      ELSE
        CALL file%e%raiseError(modName//'::'//myName//' - '// &
          'Cannot delete checkpoint file if it is not initialized!')
      ENDIF
    ENDSUBROUTINE fdelete_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Sets the value for the path of the file name
!>
!> Cannot be done when the file is open.
!>
    SUBROUTINE setFilePath_CheckpointFileType(file,pathstr)
      CHARACTER(LEN=*),PARAMETER :: myName='setFilePath_CheckpointFileType'
      CLASS(CheckpointFileType),INTENT(INOUT) :: file
      CHARACTER(LEN=*),INTENT(IN) :: pathstr
      IF(file%isInit) THEN
        CALL file%basefile%setFilePath(pathstr)
        file%pathlen=LEN_TRIM(file%basefile%getFilePath())
      ELSE
        CALL file%e%raiseError(modName//'::'//myName//' - '// &
          'Cannot change path of checkpoint file if it is not initialized!')
      ENDIF
    ENDSUBROUTINE setFilePath_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Sets the value for the file name
!>
!> Cannot be done when the file is open.
!>
    SUBROUTINE setFileName_CheckpointFileType(file,namestr)
      CHARACTER(LEN=*),PARAMETER :: myName='setFileName_CheckpointFileType'
      CLASS(CheckpointFileType),INTENT(INOUT) :: file
      CHARACTER(LEN=*),INTENT(IN) :: namestr
      IF(file%isInit) THEN
        CALL file%basefile%setFileName(namestr)
        file%fnamelen=LEN_TRIM(file%basefile%getFileName())
      ELSE
        CALL file%e%raiseError(modName//'::'//myName//' - '// &
          'Cannot change name of checkpoint file if it is not initialized!')
      ENDIF
    ENDSUBROUTINE setFileName_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Sets the value for the file name extension
!>
!> Cannot be done when the file is open.
!>
    SUBROUTINE setFileExt_CheckpointFileType(file,extstr)
      CHARACTER(LEN=*),PARAMETER :: myName='setFileExt_CheckpointFileType'
      CLASS(CheckpointFileType),INTENT(INOUT) :: file
      CHARACTER(LEN=*),INTENT(IN) :: extstr
      IF(file%isInit) THEN
        CALL file%basefile%setFileExt(extstr)
        file%extlen=LEN_TRIM(file%basefile%getFileExt())
      ELSE
        CALL file%e%raiseError(modName//'::'//myName//' - '// &
         'Cannot change extension of checkpoint file if it is not initialized!')
      ENDIF
    ENDSUBROUTINE setFileExt_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Get the path, filename, and extension of a file object.
!> @param file the file object
!> @param path output string containing just the path (includes file separator 
!>        at the end)
!> @param fname output string with the filename 
!> @param ext output string with the filename extension (including the '.')
!>
!> Given a file object it returns the path, filename, and file
!> extension as separate strings. The full path to the file can be reconstructed
!> as @c path//filename//ext. The path is everything in the string up to
!> and including the last SLASH character. The filename is everything after the
!> last SLASH and before the last DOT. If there are no slash characters the path
!> is an empty string and fname is essentially the string.
!> @par
!> The file extension is everything in string after and including the last '.'
!> character. If there is no '.' character in the file name then the extension
!> is an empty string.
!>
    SUBROUTINE getFileParts_CheckpointFileType(file,path,fname,ext)
      CLASS(CheckpointFileType),INTENT(INOUT) :: file
      TYPE(StringType),INTENT(OUT) :: path
      TYPE(StringType),INTENT(OUT) :: fname
      TYPE(StringType),INTENT(OUT) :: ext
      path=''
      fname=''
      ext=''
      IF(file%isInit) CALL file%basefile%getFileParts(path,fname,ext)
    ENDSUBROUTINE getFileParts_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Get the path of a file object.
!> @param file the file object
!> @param path output string containing just the path (includes file separator 
!>        at the end)
!>
    PURE FUNCTION getFilePath_CheckpointFileType(file) RESULT(path)
      CLASS(CheckpointFileType),INTENT(IN) :: file
      CHARACTER(LEN=MAX_FILE_STRING_LEN) :: path
      path=''
      IF(file%isInit) path=file%basefile%getFilePath()
    ENDFUNCTION getFilePath_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Get the file name of a file object.
!> @param file the file object
!> @param fname output string with the filename (excludes extension)
!>
    PURE FUNCTION getFileName_CheckpointFileType(file) RESULT(fname)
      CLASS(CheckpointFileType),INTENT(IN) :: file
      CHARACTER(LEN=MAX_FILE_STRING_LEN) :: fname
      fname=''
      IF(file%isInit) fname=file%basefile%getFileName()
    ENDFUNCTION getFileName_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Get the file name extension of a file object
!> @param file the file object
!> @param ext output string with the filename extension (including the '.')
!>
    PURE FUNCTION getFileExt_CheckpointFileType(file) RESULT(ext)
      CLASS(CheckpointFileType),INTENT(IN) :: file
      CHARACTER(LEN=MAX_FILE_STRING_LEN) :: ext
      ext=''
      IF(file%isInit) ext=file%basefile%getFileExt()
    ENDFUNCTION getFileExt_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Sets the status of whether or not the end of file record
!> has been reached.
!> 
!> Cannot be changed if the file is closed.
!>
    SUBROUTINE setEOFstat_CheckpointFileType(file,bool)
      CHARACTER(LEN=*),PARAMETER :: myName='setEOFstat_CheckpointFileType'
      CLASS(CheckpointFileType),INTENT(INOUT) :: file
      LOGICAL(SBK),INTENT(IN) :: bool
      IF(file%isInit) THEN
        CALL file%basefile%setEOFstat(bool)
      ELSE
        CALL file%e%raiseError(modName//'::'//myName// &
          ' - EOF status cannot be changed for uninitialized checkpoint file!')
      ENDIF
    ENDSUBROUTINE setEOFstat_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Sets the value for when the file is open or closed.
!>
!> There is no sufficient way to protect this attribute from being corrupted, 
!> but this method, in general should NEVER be called unless it is within the
!> fopen or fclose methods.
!>
    SUBROUTINE setOpenStat_CheckpointFileType(file,bool)
      CHARACTER(LEN=*),PARAMETER :: myName='setReadStat_CheckpointFileType'
      CLASS(CheckpointFileType),INTENT(INOUT) :: file
      LOGICAL(SBK),INTENT(IN) :: bool
      IF(file%isInit) THEN
        CALL file%basefile%setOpenStat(bool)
      ELSE
        CALL file%e%raiseError(modName//'::'//myName// &
          ' - Open status cannot be changed for uninitialized checkpoint file!')
      ENDIF
    ENDSUBROUTINE setOpenStat_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Sets the value for the status of whether or not the file will
!> be opened for reading.
!>
!> Cannot be changed if the file is open.
!>
    SUBROUTINE setReadStat_CheckpointFileType(file,bool)
      CHARACTER(LEN=*),PARAMETER :: myName='setReadStat_CheckpointFileType'
      CLASS(CheckpointFileType),INTENT(INOUT) :: file
      LOGICAL(SBK),INTENT(IN) :: bool
      IF(file%isInit) THEN
        CALL file%basefile%setReadstat(bool)
      ELSE
        CALL file%e%raiseError(modName//'::'//myName// &
          ' - Read status cannot be changed for uninitialized checkpoint file!')
      ENDIF
    ENDSUBROUTINE setReadStat_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Sets the value for the status of whether or not the file will
!> be opened for writing.
!>
!> Cannot be changed if the file is open.
!>
    SUBROUTINE setWriteStat_CheckpointFileType(file,bool)
      CHARACTER(LEN=*),PARAMETER :: myName='setWriteStat_CheckpointFileType'
      CLASS(CheckpointFileType),INTENT(INOUT) :: file
      LOGICAL(SBK),INTENT(IN) :: bool
      IF(file%isInit) THEN
        CALL file%basefile%setWritestat(bool)
      ELSE
        CALL file%e%raiseError(modName//'::'//myName// &
         ' - Write status cannot be changed for uninitialized checkpoint file!')
      ENDIF
    ENDSUBROUTINE setWriteStat_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Function to query if the end of file has been reached.
!> @param file the file object
!> @returns bool a logical for the end of file status
!>
    PURE FUNCTION isEOF_CheckpointFileType(file) RESULT(bool)
      CLASS(CheckpointFileType),INTENT(IN) :: file
      LOGICAL(SBK) :: bool
      bool=.FALSE.
      IF(file%isInit) bool=file%basefile%isEOF()
    ENDFUNCTION isEOF_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Function to query if the file is currently open
!> @param file the file object
!> @returns bool a logical for the open status
!>
    PURE FUNCTION isOpen_CheckpointFileType(file) RESULT(bool)
      CLASS(CheckpointFileType),INTENT(IN) :: file
      LOGICAL(SBK) :: bool
      bool=.FALSE.
      IF(file%isInit) bool=file%basefile%isOpen()
    ENDFUNCTION isOpen_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Function to query if the file was opened for reading.
!> @param file the file object
!> @returns bool a logical for the read status
!>
    PURE FUNCTION isRead_CheckpointFileType(file) RESULT(bool)
      CLASS(CheckpointFileType),INTENT(IN) :: file
      LOGICAL(SBK) :: bool
      bool=.FALSE.
      IF(file%isInit) bool=file%basefile%isRead()
    ENDFUNCTION isRead_CheckpointFileType
!
!-------------------------------------------------------------------------------
!> @brief Function to query if the file was opened for writing.
!> @param file the file object
!> @returns bool a logical for the write status
!>
    PURE FUNCTION isWrite_CheckpointFileType(file) RESULT(bool)
      CLASS(CheckpointFileType),INTENT(IN) :: file
      LOGICAL(SBK) :: bool
      bool=.FALSE.
      IF(file%isInit) bool=file%basefile%isWrite()
    ENDFUNCTION isWrite_CheckpointFileType
!
ENDMODULE FileType_Checkpoint
