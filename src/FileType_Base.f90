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
!> @brief Utility module for I/O defines the base file type object.
!> 
!> This type is an abstract type, so it has no specific implementation. It
!> exists only to provide a base for the extended types. It specifies the
!> maximum lengths for the file path, name, and extension, some basic attributes
!> of a file such as whether or not it is open and also if it is open for
!> reading or writing are provided. Methods to interface to all attributes are
!> also provided.
!>
!> @par
!> It should be noted that the @ref FileType_Base::setOpenStat_file 
!> "setOpenStat" method should never be used directly outside of an I/O utility
!> module it has the potential to corrupt the object and make its use unstable.
!> This module is considered to be an I/O utility module so it's public members
!> should be accessed through @ref IOutil "IOutil". This module  should not be
!> used directly except when it is needed by another I/O utility module. Since
!> this is an abstract type, it has no specific implementation so see one of
!> it's extended types for examples on how it should be used. This module is 
!> tested by @c testIOutil.f90 and the coverage report can be found at the @ref
!> CodeCoverageReports "Code Coverage Reports" page.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief Exceptionhandler
!>
!> @author Brendan Kochunas
!>   @date 07/06/2011
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FileType_Base
      
  USE IntrType
  USE ExceptionHandler
  IMPLICIT NONE
  PRIVATE
  
  !List of Public Members
  PUBLIC :: MAX_PATH_LENGTH
  PUBLIC :: MAX_FNAME_LENGTH
  PUBLIC :: MAX_FEXT_LENGTH
  PUBLIC :: BaseFileType
  PUBLIC :: clear_base_file
  
  !> The maximum allowable length for the path to a file.
  INTEGER(SIK),PARAMETER :: MAX_PATH_LENGTH=256
  !> The maximum allowable length for a file name
  !> (with extension, excludes path)
  INTEGER(SIK),PARAMETER :: MAX_FNAME_LENGTH=64
  !> The maximum allowable length for the extension to a file
  INTEGER(SIK),PARAMETER :: MAX_FEXT_LENGTH=16
  
  CHARACTER(LEN=*),PARAMETER :: modName='FILETYPE_BASE'
  
  !> @brief Base derived type for a file object
  !>
  !> This is an abstract type which means it has no basic implementation
  TYPE, ABSTRACT :: BaseFileType
    !> The path string to the file
    CHARACTER(LEN=MAX_PATH_LENGTH),PRIVATE :: path=''
    !> The name of the file (without the file extension)
    CHARACTER(LEN=MAX_FNAME_LENGTH),PRIVATE :: name=''
    !> The extension of the file name
    CHARACTER(LEN=MAX_FEXT_LENGTH),PRIVATE :: ext=''
    !> Whether or not the file is open
    LOGICAL(SBK),PRIVATE :: openstat=.FALSE.
    !> Whether or not the end of file has been reached
    LOGICAL(SBK),PRIVATE :: EOFstat=.FALSE.
    !> Whether or not the file is open for reading
    LOGICAL(SBK),PRIVATE :: readstat=.FALSE.
    !> Whether or not the file is open for writing
    LOGICAL(SBK),PRIVATE :: writestat=.FALSE.
    !> The exception handler for the object
    TYPE(ExceptionHandlerType) :: e
!
!List of type bound procedures (methods) for the Base File Type object
    CONTAINS
      !> Deferred routine for opening the file.
      PROCEDURE(FileBase_sub_absintfc),DEFERRED,PASS :: fopen
      !> Deferred routine for closing the file.
      PROCEDURE(FileBase_sub_absintfc),DEFERRED,PASS :: fclose
      !> Deferred routine for deleting the file.
      PROCEDURE(FileBase_sub_absintfc),DEFERRED,PASS :: fdelete
      !> @copybrief FileType_Base::setFilePath_file
      !> @copydetails FileType_Base::setFilePath_file
      PROCEDURE,PASS :: setFilePath => setFilePath_file
      !> @copybrief FileType_Base::setFileName_file
      !> @copydetails FileType_Base::setFileName_file
      PROCEDURE,PASS :: setFileName => setFileName_file
      !> @copybrief FileType_Base::setFileExt_file
      !> @copydetails FileType_Base::setFileExt_file
      PROCEDURE,PASS :: setFileExt => setFileExt_file
      !> @copybrief FileType_Base::getFilePath_file
      !> @copydetails FileType_Base::getFilePath_file
      PROCEDURE,PASS :: getFilePath => getFilePath_file
      !> @copybrief FileType_Base::getFileName_file
      !> @copydetails FileType_Base::getFileName_file
      PROCEDURE,PASS :: getFileName => getFileName_file
      !> @copybrief FileType_Base::getFileExt_file
      !> @copydetails FileType_Base::getFileExt_file
      PROCEDURE,PASS :: getFileExt => getFileExt_file
      !> @copybrief FileType_Base::getFileParts_file
      !> @copydetails FileType_Base::getFileParts_file
      PROCEDURE,PASS :: getFileParts => getFileParts_file
      !> @copybrief FileType_Base::setEOFStat_file
      !> @copydetails FileType_Base::setEOFStat_file
      PROCEDURE,PASS :: setEOFstat => setEOFStat_file
      !> @copybrief FileType_Base::setOpenStat_file
      !> @copydetails FileType_Base::setOpenStat_file
      PROCEDURE,PASS :: setOpenStat => setOpenStat_file
      !> @copybrief FileType_Base::setReadStat_file
      !> @copydetails FileType_Base::setReadStat_file
      PROCEDURE,PASS :: setReadStat => setReadStat_file
      !> @copybrief FileType_Base::setWriteStat_file
      !> @copydetails FileType_Base::setWriteStat_file
      PROCEDURE,PASS :: setWriteStat => setWriteStat_file
      !> @copybrief FileType_Base::isOpen_file
      !> @copydetails FileType_Base::isOpen_file
      PROCEDURE,PASS :: isOpen => isOpen_file
      !> @copybrief FileType_Base::isEOF_file
      !> @copydetails FileType_Base::isEOF_file
      PROCEDURE,PASS :: isEOF => isEOF_file
      !> @copybrief FileType_Base::isRead_file
      !> @copydetails FileType_Base::isRead_file
      PROCEDURE,PASS :: isRead => isRead_file
      !> @copybrief FileType_Base::isWrite_file
      !> @copydetails FileType_Base::isWrite_file
      PROCEDURE,PASS :: isWrite => isWrite_file
  ENDTYPE BaseFileType
  
  !> An abstract interface for type bound procedures for opening, closing,
  !> and deleting a file.
  ABSTRACT INTERFACE
    SUBROUTINE FileBase_sub_absintfc(file)
      IMPORT :: BaseFileType
      CLASS(BaseFileType),INTENT(INOUT) :: file
    ENDSUBROUTINE FileBase_sub_absintfc
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Sets the value for the path of the file name
!>
!> Cannot be done when the file is open.
    SUBROUTINE setFilePath_file(file,pathstr)
      CHARACTER(LEN=*),PARAMETER :: myName='setFilePath_file'
      CLASS(BaseFileType),INTENT(INOUT) :: file
      CHARACTER(LEN=*),INTENT(IN) :: pathstr
      IF(file%openstat) THEN
        CALL file%e%raiseError(modName//'::'//myName//' - '// &
          'Cannot change path of file while it is open!')
      ELSE
        IF(LEN_TRIM(pathstr) > MAX_PATH_LENGTH) CALL file%e%raiseError( &
          modName//'::'//myName//' - input string is too long to store!')
        file%path=TRIM(pathstr)
      ENDIF
    ENDSUBROUTINE setFilePath_file
!
!-------------------------------------------------------------------------------
!> @brief Sets the value for the file name
!>
!> Cannot be done when the file is open.
    SUBROUTINE setFileName_file(file,namestr)
      CHARACTER(LEN=*),PARAMETER :: myName='setFileName_file'
      CLASS(BaseFileType),INTENT(INOUT) :: file
      CHARACTER(LEN=*),INTENT(IN) :: namestr
      IF(file%openstat) THEN
        CALL file%e%raiseError(modName//'::'//myName//' - '// &
          'Cannot change name of file while it is open!')
      ELSE
        IF(LEN_TRIM(namestr) > MAX_FNAME_LENGTH) CALL file%e%raiseError( &
          modName//'::'//myName//' - input string is too long to store!')
        file%name=TRIM(namestr)
      ENDIF
    ENDSUBROUTINE setFileName_file
!
!-------------------------------------------------------------------------------
!> @brief Sets the value for the file name extension
!>
!> Cannot be done when the file is open.
    SUBROUTINE setFileExt_file(file,extstr)
      CHARACTER(LEN=*),PARAMETER :: myName='setFileExt_file'
      CLASS(BaseFileType),INTENT(INOUT) :: file
      CHARACTER(LEN=*),INTENT(IN) :: extstr
      IF(file%openstat) THEN
        CALL file%e%raiseError(modName//'::'//myName//' - '// &
          'Cannot change extension of file while it is open!!')
      ELSE
        IF(LEN_TRIM(extstr) > MAX_FEXT_LENGTH) CALL file%e%raiseError( &
          modName//'::'//myName//' - input string is too long to store!')
        file%ext=TRIM(extstr)
      ENDIF
    ENDSUBROUTINE setFileExt_file
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
    SUBROUTINE getFileParts_file(file,path,fname,ext)
      CHARACTER(LEN=*),PARAMETER :: myName='getFileParts_file'
      CLASS(BaseFileType),INTENT(INOUT) :: file
      CHARACTER(LEN=*),INTENT(OUT) :: path
      CHARACTER(LEN=*),INTENT(OUT) :: fname
      CHARACTER(LEN=*),INTENT(OUT) :: ext
      IF(LEN(path) < LEN_TRIM(file%path)) CALL file%e%raiseError(modName// &
        '::'//myName//' - input string for PATH is not long enough to '// &
          'contain full path!')
      path=TRIM(file%path)
      IF(LEN(fname) < LEN_TRIM(file%name)) CALL file%e%raiseError(modName// &
        '::'//myName//' - input string for file name is not long enough to'// &
          ' contain full path!')
      IF(LEN_TRIM(file%name) == 0) CALL file%e%raiseWarning(modName//'::'// &
        myName//' - the file name appears to be empty!')
      fname=TRIM(file%name)
      IF(LEN(ext) < LEN_TRIM(file%ext)) CALL file%e%raiseError(modName//'::'// &
        myName//' - input string for EXTENSION is not long enough to '// &
          'contain full string.')
      ext=TRIM(file%ext)
    ENDSUBROUTINE getFileParts_file
!
!-------------------------------------------------------------------------------
!> @brief Get the path of a file object.
!> @param file the file object
!> @param path output string containing just the path (includes file separator 
!>        at the end)
    PURE FUNCTION getFilePath_file(file) RESULT(path)
      CLASS(BaseFileType),INTENT(IN) :: file
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      path=TRIM(file%path)
    ENDFUNCTION getFilePath_file
!
!-------------------------------------------------------------------------------
!> @brief Get the file name of a file object.
!> @param file the file object
!> @param fname output string with the filename (excludes extension)
    PURE FUNCTION getFileName_file(file) RESULT(fname)
      CLASS(BaseFileType),INTENT(IN) :: file
      CHARACTER(LEN=MAX_FNAME_LENGTH) :: fname
      fname=TRIM(file%name)
    ENDFUNCTION getFileName_file
!
!-------------------------------------------------------------------------------
!> @brief Get the file name extension of a file object
!> @param file the file object
!> @param ext output string with the filename extension (including the '.')
    PURE FUNCTION getFileExt_file(file) RESULT(ext)
      CLASS(BaseFileType),INTENT(IN) :: file
      CHARACTER(LEN=MAX_FEXT_LENGTH) :: ext
      ext=TRIM(file%ext)
    ENDFUNCTION getFileExt_file
!
!-------------------------------------------------------------------------------
!> @brief Function to query if the file is currently open
!> @param file the file object
!> @returns bool a logical for the open status
    PURE FUNCTION isOpen_file(file) RESULT(bool)
      CLASS(BaseFileType),INTENT(IN) :: file
      LOGICAL(SBK) :: bool
      bool=file%openstat
    ENDFUNCTION isOpen_file
!
!-------------------------------------------------------------------------------
!> @brief Function to query if the end of file has been reached.
!> @param file the file object
!> @returns bool a logical for the end of file status
    PURE FUNCTION isEOF_file(file) RESULT(bool)
      CLASS(BaseFileType),INTENT(IN) :: file
      LOGICAL(SBK) :: bool
      bool=file%EOFstat
    ENDFUNCTION isEOF_file
!
!-------------------------------------------------------------------------------
!> @brief Function to query if the file was opened for writing.
!> @param file the file object
!> @returns bool a logical for the write status
    PURE FUNCTION isWrite_file(file) RESULT(bool)
      CLASS(BaseFileType),INTENT(IN) :: file
      LOGICAL(SBK) :: bool
      bool=file%writestat
    ENDFUNCTION isWrite_file
!
!-------------------------------------------------------------------------------
!> @brief Function to query if the file was opened for reading.
!> @param file the file object
!> @returns bool a logical for the read status
    PURE FUNCTION isRead_file(file) RESULT(bool)
      CLASS(BaseFileType),INTENT(IN) :: file
      LOGICAL(SBK) :: bool
      bool=file%readstat
    ENDFUNCTION isRead_file
!
!-------------------------------------------------------------------------------
!> @brief Sets the status of whether or not the end of file record
!> has been reached.
!> 
!> Cannot be changed if the file is closed.
    SUBROUTINE setEOFstat_file(file,bool)
      CHARACTER(LEN=*),PARAMETER :: myName='setEOFstat_file'
      CLASS(BaseFileType),INTENT(INOUT) :: file
      LOGICAL(SBK),INTENT(IN) :: bool
      IF(file%openstat) THEN
        file%EOFstat=bool
      ELSE
        CALL file%e%raiseDebug(modName//'::'//myName// &
          ' - EOF status cannot be changed on a file that is not open!')
      ENDIF
    ENDSUBROUTINE setEOFstat_file
!
!-------------------------------------------------------------------------------
!> @brief Sets the value for when the file is open or closed.
!>
!> There is no sufficient way to protect this attribute from being corrupted, 
!> but this method, in general should NEVER be called unless it is within the
!> fopen or fclose methods.
    SUBROUTINE setOpenStat_file(file,bool)
      CLASS(BaseFileType),INTENT(INOUT) :: file
      LOGICAL(SBK),INTENT(IN) :: bool
      file%openstat=bool
    ENDSUBROUTINE setOpenStat_file
!
!-------------------------------------------------------------------------------
!> @brief Sets the value for the status of whether or not the file will
!> be opened for reading.
!>
!> Cannot be changed if the file is open.
    SUBROUTINE setReadStat_file(file,bool)
      CHARACTER(LEN=*),PARAMETER :: myName='setReadStat_file'
      CLASS(BaseFileType),INTENT(INOUT) :: file
      LOGICAL(SBK),INTENT(IN) :: bool
      IF(file%openstat) THEN
        CALL file%e%raiseDebug(modName//'::'//myName// &
          ' - Cannot change read status of a file if it is open!')
      ELSE
        file%readstat=bool
      ENDIF
    ENDSUBROUTINE setReadStat_file
!
!-------------------------------------------------------------------------------
!> @brief Sets the value for the status of whether or not the file will
!> be opened for writing.
!>
!> Cannot be changed if the file is open.
    SUBROUTINE setWriteStat_file(file,bool)
      CHARACTER(LEN=*),PARAMETER :: myName='setWriteStat_file'
      CLASS(BaseFileType),INTENT(INOUT) :: file
      LOGICAL(SBK),INTENT(IN) :: bool
      IF(file%openstat) THEN
        CALL file%e%raiseDebug(modName//'::'//myName// &
          ' - Cannot change write status of a file if it is open!')
      ELSE
        file%writestat=bool
      ENDIF
    ENDSUBROUTINE setWriteStat_file
!
!-------------------------------------------------------------------------------
!> @brief Clears the attributes of the base file type.
!> @param file the base file type object
!>
!> Made public for use by other extended types.
    SUBROUTINE clear_base_file(file)
      CLASS(BaseFileType),INTENT(INOUT) :: file
      file%path=''
      file%name=''
      file%ext=''
      file%openstat=.FALSE.
      file%EOFstat=.FALSE.
      file%readstat=.FALSE.
      file%writestat=.FALSE.
      CALL file%e%reset()
    ENDSUBROUTINE
!
ENDMODULE FileType_Base
