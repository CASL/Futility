!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module for I/O defines the derived type for a Fortran File
!> object.
!>
!> The Fortan file type is an extension of the abstract @ref FileType_Base
!> "Base file type". It provides a simplified interface to the native Fortran
!> file capabilities and includes error checking. This module is considered to
!> be an I/O utility module so it's public members should be accessed through
!> @ref IOutil "IOutil". This module should not be used directly except when it
!> is needed by another I/O utility module. This module is tested by
!> @c testIOutil.f90 and the coverage report can be found at the @ref
!> CodeCoverageReports "Code Coverage Reports" page. An example of how to use
!> the Fortran file type is provided below and in the test.
!>
!> @par EXAMPLES
!> @code
!> PROGRAM FileExample
!>
!> USE IOutil
!> IMPLICIT NONE
!>
!> TYPE(FortranFileType) :: inputfile,outputfile
!> CHARACTER(LEN=128) :: string
!>
!> !Set the exception handler of the input file to NOT stop on errors
!> CALL inputfile%exception%setStopOnError(.FALSE.)
!>
!> !Initialize and open the input file, it should exist, opened for reading,
!> !and its a text file.
!> CALL inputfile%initialize(UNIT=25,FILE='test.inp',STATUS='OLD', &
!>   ACTION='READ',FORM='FORMATTED')
!> CALL inputfile%fopen()
!>
!> !Initialize and open the output file, it will be overwtitten
!> !and its a text file.
!> CALL outputfile%initialize(UNIT=26,FILE='test.out',FORM='FORMATTED')
!> CALL outputfile%fopen()
!>
!> !Read a line from the input file and write it to the output file
!> READ(inputfile%getUnitNo(),*) string
!> WRITE(outputfile%getUnitNo(),*) string
!>
!> !Close the files
!> CALL inputfile%fclose()
!> CALL outputfile%fclose()
!>
!> ENDPROGRAM
!> @endcode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FileType_Fortran
USE ISO_FORTRAN_ENV
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE Strings
USE ExceptionHandler
USE IO_Strings
USE FileType_Base
IMPLICIT NONE
PRIVATE

!List of Public Members
PUBLIC :: FortranFileType
PUBLIC :: init_fortran_file
PUBLIC :: clear_fortran_file
PUBLIC :: rewind_fortran_file
PUBLIC :: backspace_fortran_file
PUBLIC :: FortranFile_get_new_unit

!> Module name for error messages
CHARACTER(LEN=*),PARAMETER :: modName='FILETYPE_FORTRAN'
!> Scratch variable for exception messages of this module
CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: emesg
!> Scratch variable for IOSTAT values
INTEGER(SIK) :: ioerr

!> @brief Derived type object for files definable by the native Fortran I/O
!>
!> This is an extension of the abstract @ref FileType_Base "BaseFileType".
!> It provides specific implementations of fopen, fclose, and fdelete. It's
!> attributes and set/get methods for these attributes are based on the
!> native Fortran I/O capability.
TYPE,EXTENDS(BaseFileType) :: FortranFileType
  !> Whether or not this object has been initialized
  LOGICAL(SBK),PRIVATE :: initstat=.FALSE.
  !> The unit number of the file
  INTEGER(SIK),PRIVATE :: unitno=-1
  !> Whether or not the file is formatted
  LOGICAL(SBK),PRIVATE :: formatstat=.FALSE.
  !> Whether or not the file is direct or sequential
  LOGICAL(SBK),PRIVATE :: accessstat=.FALSE.
  !> Whether or not the file is stream
  LOGICAL(SBK),PRIVATE :: accessstream=.FALSE.
  !> The 'new' status of a file
  LOGICAL(SBK) :: newstat=.FALSE.
  !> Whether or not to replace the file (implies new)
  LOGICAL(SBK),PRIVATE :: overwrite=.FALSE.
  !> The record length (if direct access)
  INTEGER(SIK),PRIVATE :: reclval=-1
  !> Whether or not the file is being padded
  LOGICAL(SBK),PRIVATE :: padstat=.FALSE.
  !> Value for the position
  CHARACTER(LEN=6),PRIVATE :: posopt='ASIS  '
!
!List of type bound procedures (methods) for the Fortran File type
  CONTAINS
    !> @copybrief FileType_Fortran::FortranFile_get_new_unit
    !> @copydetails FileType_Fortran::FortranFile_get_new_unit
    PROCEDURE,NOPASS :: newUnitNo => FortranFile_get_new_unit
    !> @copybrief FileType_Fortran::init_fortran_file
    !> @copydetails FileType_Fortran::init_fortran_file
    PROCEDURE,PASS :: initialize => init_fortran_file
    !> @copybrief FileType_Fortran::clear_fortran_file
    !> @copydetails FileType_Fortran::clear_fortran_file
    PROCEDURE,PASS :: clear => clear_fortran_file
    !> @copybrief FileType_Fortran::open_fortran_file
    !> @copydetails FileType_Fortran::open_fortran_file
    PROCEDURE,PASS :: fopen => open_fortran_file
    !> @copybrief FileType_Fortran::close_fortran_file
    !> @copydetails FileType_Fortran::close_fortran_file
    PROCEDURE,PASS :: fclose => close_fortran_file
    !> @copybrief FileType_Fortran::delete_fortran_file
    !> @copydetails FileType_Fortran::delete_fortran_file
    PROCEDURE,PASS :: fdelete  => delete_fortran_file
    !> @copybrief FileType_Fortran::backspace_fortran_file
    !> @copydetails FileType_Fortran::backspace_fortran_file
    PROCEDURE,PASS :: fbackspace => backspace_fortran_file
    !> @copybrief FileType_Fortran::rewind_fortran_file
    !> @copydetails FileType_Fortran::rewind_fortran_file
    PROCEDURE,PASS :: frewind => rewind_fortran_file
    !> @copybrief FileType_Fortran::getUnitNo_fortran_file
    !> @copydetails FileType_Fortran::getUnitNo_fortran_file
    PROCEDURE,PASS :: getUnitNo => getUnitNo_fortran_file
    !> @copybrief FileType_Fortran::isFormatted_fortran_file
    !> @copydetails FileType_Fortran::isFormatted_fortran_file
    PROCEDURE,PASS :: isFormatted => isFormatted_fortran_file
    !> @copybrief FileType_Fortran::isDirect_fortran_file
    !> @copydetails FileType_Fortran::isDirect_fortran_file
    PROCEDURE,PASS :: isDirect => isDirect_fortran_file
    !> @copybrief FileType_Fortran::isStream_fortran_file
    !> @copydetails FileType_Fortran::isStream_fortran_file
    PROCEDURE,PASS :: isStream => isStream_fortran_file
    !> @copybrief FileType_Fortran::getRecLen_fortran_file
    !> @copydetails FileType_Fortran::getRecLen_fortran_file
    PROCEDURE,PASS :: getRecLen => getRecLen_fortran_file
    !> @copybrief FileType_Fortran::isPadded_fortran_file
    !> @copydetails FileType_Fortran::isPadded_fortran_file
    PROCEDURE,PASS :: isPadded => isPadded_fortran_file
    !> @copybrief FileType_Fortran::isNew_fortran_file
    !> @copydetails FileType_Fortran::isNew_fortran_file
    PROCEDURE,PASS :: isNew => isNew_fortran_file
    !> @copybrief FileType_Fortran::isOverwrite_fortran_file
    !> @copydetails FileType_Fortran::isOverwrite_fortran_file
    PROCEDURE,PASS :: isOverwrite => isOverwrite_fortran_file
    !> @copybrief FileType_Fortran::isInit_fortran_file
    !> @copydetails FileType_Fortran::isInit_fortran_file
    PROCEDURE,PASS :: isInit => isInit_fortran_file
    !> @copybrief FileType_Fortran::setStatus_fortran_file
    !> @copydetails FileType_Fortran::setStatus_fortran_file
    PROCEDURE,PASS :: setStatus => setStatus_fortran_file
    !> @copybrief FileType_Fortran::write_str_1a_fortran_file
    !> @copydetails FileType_Fortran::write_str_1a_fortran_file
    PROCEDURE,PASS,PRIVATE :: write_str_1a => write_str_1a_fortran_file
    GENERIC :: fwrite => write_str_1a
ENDTYPE FortranFileType
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Fortran file object.
!> @param fileobj Fortran file object.
!> @param unit Unit number to use for the file.
!> @param file Full path name of the file.
!> @param status Optional input. Value to use for the STATUS clause of the OPEN
!>               statement.
!> @param access Optional input. Value to use for the ACCESS clause of the OPEN
!>               statement.
!> @param form Optional input. Value to use for the FORM clause of the OPEN
!>             statement.
!> @param position Optional input. Value to use for the POSITION clause of the
!>                 OPEN statement.
!> @param action Optional input. Value to use for the ACTION clause of the OPEN
!>               statement.
!> @param pad Optional input. Value to use for the PAD clause of the OPEN
!>            statement.
!> @param recl Optional input. Value to use for the RECL clause of the OPEN
!>             statement.
!>
!> The interface to this routine is very similar to that of the OPEN statement
!> with a few exceptions. The value for the FILE clause and the UNIT must be
!> specified. The values for DELIM and BLANK cannot be set. If the status of
!> a file is specified as 'SCRATCH' or 'UNKNOWN' it is replaced with with
!> the value 'REPLACE'. It is made public for use by other extended types.
!>
SUBROUTINE init_fortran_file(fileobj,unit,file,status,access,form, &
    position,action,pad,recl)
  CHARACTER(LEN=*),PARAMETER :: myName='INIT_FORTRAN_FILE'
  CLASS(FortranFileType),INTENT(INOUT) :: fileobj
  INTEGER(SIK),OPTIONAL,INTENT(IN) :: unit
  CHARACTER(LEN=*),INTENT(IN) :: file
  CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: status
  CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: access
  CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: form
  CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: position
  CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: action
  CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: pad
  INTEGER(SIK),OPTIONAL,INTENT(IN) :: recl

  CHARACTER(LEN=7) :: statusval
  CHARACTER(LEN=10) :: accessval
  CHARACTER(LEN=11) :: formval
  CHARACTER(LEN=9) :: actionval
  CHARACTER(LEN=3) :: padval
  TYPE(StringType) :: fpath,fname,fext
  LOGICAL(SBK) :: ostat
  INTEGER(SIK) :: oldcnt

  !Initialize data
  statusval=''
  accessval=''
  formval=''
  actionval=''
  padval=''

  oldcnt=fileobj%e%getCounter(EXCEPTION_ERROR)

  IF(fileobj%initstat) THEN
    CALL fileobj%e%raiseError(modName//'::'//myName//' - '// &
        'Fortran file has already been initialized!')
  ELSE
    !Initialize the file
    CALL getFileParts(file,fpath,fname,fext,fileobj%e)
    CALL fileobj%setFilePath(CHAR(fpath))
    CALL fileobj%setFileName(CHAR(fname))
    CALL fileobj%setFileExt(CHAR(fext))

    IF(PRESENT(unit)) THEN
      IF(unit == OUTPUT_UNIT) THEN
        CALL fileobj%e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value for optional input argument UNIT! Value is equal to '// &
            'default OUTPUT_UNIT.')
      ELSEIF(unit == ERROR_UNIT) THEN
        CALL fileobj%e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value for optional input argument UNIT! Value is equal to '// &
            'default ERROR_UNIT.')
      ELSEIF(unit == INPUT_UNIT) THEN
        CALL fileobj%e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value for optional input argument UNIT! Value is equal to '// &
            'default INPUT_UNIT.')
      ELSE
        INQUIRE(UNIT=unit,OPENED=ostat)
        IF(ostat) THEN
          CALL fileobj%e%raiseError(modName//'::'//myName//' - Illegal '// &
              'value for optional input argument UNIT! Unit is being used'// &
              ' by another file!')
        ELSE
          fileobj%unitno=unit
        ENDIF
      ENDIF
    ELSE
      fileobj%unitno=fileobj%newUnitNo()
    ENDIF

    !STATUS clause for OPEN statement
    IF(PRESENT(status)) THEN
      SELECT CASE(status)
      CASE('OLD') !File already exists
        statusval='OLD'
      CASE('NEW') !File does not exist and will be created
        statusval='NEW'
      CASE('SCRATCH') !File is deleted after execution (treated as replace)
        statusval='REPLACE'
      CASE('REPLACE') !File may or may not exist, if it does it is replaced
        statusval='REPLACE'
      CASE('UNKNOWN') !Processor/Compiler dependent behavior
        statusval='REPLACE'
      CASE DEFAULT
        CALL fileobj%e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value ('//status//') for optional input argument STATUS!')
      ENDSELECT
    ELSE
      !Default value for status
      statusval='REPLACE'
    ENDIF

    !ACCESS clause for OPEN statement
    IF(PRESENT(access)) THEN
      SELECT CASE(access)
      CASE('SEQUENTIAL') !File is accessed sequentially
        accessval=access
      CASE('DIRECT') !File has direct access
        accessval=access
      CASE('STREAM') !File has streaming access !F2003, might have problems.
        accessval=access
      CASE DEFAULT
        CALL fileobj%e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value ('//access//') for optional input argument ACCESS!')
      ENDSELECT
    ELSE
      !Default value
      accessval='SEQUENTIAL'
    ENDIF

    !FORM clause for OPEN statement
    IF(PRESENT(form)) THEN
      SELECT CASE(form)
      CASE('FORMATTED') !File is a text file
        formval=form
      CASE('UNFORMATTED') !File a binary file
        formval=form
      CASE DEFAULT
        CALL fileobj%e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value ('//form//') for optional input argument FORM!')
      ENDSELECT
    ELSE
      !Default value
      formval='FORMATTED'
    ENDIF

    !POSITION clause for OPEN statement
    IF(PRESENT(position)) THEN
      SELECT CASE(position)
      CASE('REWIND') !File opens at beginning of file
        fileobj%posopt=position
      CASE('APPEND') !File opens at end of file
        fileobj%posopt=position
      CASE('ASIS') !File opens with file pointer as is
        fileobj%posopt=position
      CASE DEFAULT
        CALL fileobj%e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value ('//position//') for optional input argument POSITION!')
      ENDSELECT
    ENDIF

    !ACTION clause for OPEN statement
    IF(PRESENT(action)) THEN
      SELECT CASE(action)
      CASE('READ') !File opens with read access only
        actionval=action
      CASE('WRITE') !File opens with write access only
        actionval=action
      CASE('READWRITE') !File opens with read write access
        actionval=action
      CASE DEFAULT
        CALL fileobj%e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value ('//action//') for optional input argument ACTION!')
      ENDSELECT
    ELSE
      !Default value
      actionval='READWRITE'
    ENDIF

    IF(PRESENT(pad)) THEN
      SELECT CASE(pad)
      CASE('YES') !File is padded
        padval=pad
      CASE('NO') !File is not padded
        padval=pad
      CASE DEFAULT
        CALL fileobj%e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value ('//pad//') for optional input argument PAD!')
      ENDSELECT
    ELSE
      !Fortran default value
      padval='YES'
    ENDIF

    IF(PRESENT(recl)) THEN
      IF(recl < 1) THEN
        CALL fileobj%e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value for input option RECL must be set to greater than 0!')
      ELSE
        fileobj%reclval=recl
      ENDIF
    ENDIF

    IF(TRIM(statusval) /= 'OLD') THEN
      fileobj%newstat=.TRUE.
      fileobj%overwrite=(TRIM(statusval) == 'REPLACE')
    ENDIF
    fileobj%formatstat=(TRIM(formval) == 'FORMATTED')
    fileobj%padstat=(TRIM(padval) ==  'YES')
    fileobj%accessstream=(TRIM(accessval)=='STREAM')
    IF(TRIM(accessval) == 'DIRECT') THEN
      fileobj%accessstat=.TRUE.
      IF(fileobj%reclval < 1) CALL fileobj%e%raiseError(modName//'::'// &
          myName//' - Record length must be set to greater than 0 for '// &
          'direct access files!')
    ENDIF

    IF(TRIM(actionval) == 'READ') THEN
      CALL fileobj%setReadStat(.TRUE.)
      IF(fileobj%newstat) CALL fileobj%e%raiseError(modName//'::'// &
          myName//' - Cannot have a new file with a read only status!')
    ELSEIF(TRIM(actionval) == 'WRITE') THEN
      CALL fileobj%setWriteStat(.TRUE.)
    ELSEIF(TRIM(actionval) == 'READWRITE') THEN
      CALL fileobj%setReadStat(.TRUE.)
      CALL fileobj%setWriteStat(.TRUE.)
    ENDIF

    IF(oldcnt < fileobj%e%getCounter(EXCEPTION_ERROR)) THEN
      CALL fileobj%e%raiseError(modName//'::'//myName//' - Exceptions '// &
          'during file initialization! File not initialized!')
      !Reset all attributes if initialization failed.
      fileobj%unitno=-1
      fileobj%formatstat=.FALSE.
      fileobj%accessstat=.FALSE.
      fileobj%newstat=.FALSE.
      fileobj%overwrite=.FALSE.
      fileobj%reclval=-1
      fileobj%padstat=.FALSE.
      fileobj%posopt='ASIS  '
      CALL fileobj%setFilePath('')
      CALL fileobj%setFileName('')
      CALL fileobj%setFileExt('')
      CALL fileobj%setEOFstat(.FALSE.)
      CALL fileobj%setOpenStat(.FALSE.)
      CALL fileobj%setReadStat(.FALSE.)
      CALL fileobj%setWriteStat(.FALSE.)
    ELSE
      fileobj%initstat=.TRUE.
    ENDIF
  ENDIF
ENDSUBROUTINE init_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Clears the Fortran file object and resets its state to the unitialized
!> state.
!> @param file the fortran file object
!> @param ldel logical on whether or not to delete or close the file.
!>
!> Made public for use by other extended types.
!>
SUBROUTINE clear_fortran_file(file,ldel)
  CLASS(FortranFileType),INTENT(INOUT) :: file
  LOGICAL(SBK),OPTIONAL,INTENT(IN) :: ldel
  LOGICAL(SBK) :: bool

  !Close the file
  bool=.FALSE.
  IF(PRESENT(ldel)) bool=ldel
  IF(file%initstat) THEN
    IF(bool) THEN
      CALL file%fdelete()
    ELSE
      CALL file%fclose()
    ENDIF
  ENDIF

  !Set FortranFileType attributes to defaults
  file%initstat=.FALSE.
  file%unitno=-1
  file%formatstat=.FALSE.
  file%accessstat=.FALSE.
  file%newstat=.FALSE.
  file%overwrite=.FALSE.
  file%reclval=-1
  file%padstat=.FALSE.
  file%posopt='ASIS  '

  !Set BaseFileType attributes to default
  CALL clear_base_file(file)
ENDSUBROUTINE clear_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief gets the unit number used by the fortran file
!> @param file the fortran file type object
!> @returns val the value of the unit number
!>
!> Guaranteed to return -1 prior to initialize being called and after clear being
!> called
PURE FUNCTION getUnitNo_fortran_file(file) RESULT(val)
  CLASS(FortranFileType),INTENT(IN) :: file
  INTEGER(SIK) :: val
  val=file%unitno
ENDFUNCTION getUnitNo_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Returns whether or not the FORTRAN file is formatted text or binary.
!> @param file the fortran file type object
!> @returns bool TRUE/FALSE if the file is formatted
!>
PURE FUNCTION isFormatted_fortran_file(file) RESULT(bool)
  CLASS(FortranFileType),INTENT(IN) :: file
  LOGICAL(SBK) :: bool
  bool=file%formatstat
ENDFUNCTION isFormatted_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Returns whether or not the FORTRAN file is direct or sequential
!> access.
!> @param file the fortran file type object
!> @returns bool TRUE/FALSE if the file is direct access
!>
PURE FUNCTION isDirect_fortran_file(file) RESULT(bool)
  CLASS(FortranFileType),INTENT(IN) :: file
  LOGICAL(SBK) :: bool
  bool=file%accessstat
ENDFUNCTION isDirect_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Returns whether or not the FORTRAN file is stream
!> access.
!> @param file the fortran file type object
!> @returns bool TRUE/FALSE if the file is stream access
!>
PURE FUNCTION isStream_fortran_file(file) RESULT(bool)
  CLASS(FortranFileType),INTENT(IN) :: file
  LOGICAL(SBK) :: bool
  bool=file%accessstream
ENDFUNCTION isStream_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Returns the record length for a direct access file.
!> @param file the fortran file type object
!> @returns val the size of the records
!>
PURE FUNCTION getRecLen_fortran_file(file) RESULT(val)
  CLASS(FortranFileType),INTENT(IN) :: file
  INTEGER(SIK) :: val
  val=file%reclval
ENDFUNCTION getRecLen_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Returns whether or not the FORTRAN file has padded output.
!> @param file the fortran file type object
!> @returns bool TRUE/FALSE if the file is padded
!>
PURE FUNCTION isPadded_fortran_file(file) RESULT(bool)
  CLASS(FortranFileType),INTENT(IN) :: file
  LOGICAL(SBK) :: bool
  bool=file%padstat
ENDFUNCTION isPadded_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Returns whether or not the FORTRAN file is new.
!> @param file the fortran file type object
!> @returns bool TRUE/FALSE if the file is new
!>
PURE FUNCTION isNew_fortran_file(file) RESULT(bool)
  CLASS(FortranFileType),INTENT(IN) :: file
  LOGICAL(SBK) :: bool
  bool=file%newstat
ENDFUNCTION isNew_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Returns whether or not the FORTRAN file will be overwritten when
!> opened.
!> @param file the fortran file type object
!> @returns bool TRUE/FALSE if the file will be overwritten
!> @note if isOverwrite is true, this implies the file is also new
!>
PURE FUNCTION isOverwrite_fortran_file(file) RESULT(bool)
  CLASS(FortranFileType),INTENT(IN) :: file
  LOGICAL(SBK) :: bool
  bool=file%overwrite
ENDFUNCTION isOverwrite_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Open the file referenced by a Fortran file object.
!> @param file Fortran file object
!>
!> The various options for the open statement are assigned by the init routine.
!>
SUBROUTINE open_fortran_file(file)
  CHARACTER(LEN=*),PARAMETER :: myName='OPEN_FORTRAN_FILE'
  CLASS(FortranFileType),INTENT(INOUT) :: file
  CHARACTER(LEN=7) :: statusvar
  CHARACTER(LEN=10) :: accessvar
  CHARACTER(LEN=11) :: formvar
  CHARACTER(LEN=9) :: actionvar
  CHARACTER(LEN=3) :: padvar
  CHARACTER(LEN=256) :: iomsg
  INTEGER(SIK) :: reclval

  !Get the appropriate clause values for the OPEN statement
  IF(file%initstat) THEN
    IF(file%isOpen()) THEN
      WRITE(emesg,'(a,i4,a)') 'Cannot open file (UNIT=', &
          file%unitno,') File is already open!'
      CALL file%e%raiseError(modName//'::'//myName//' - '//emesg)
    ELSE
      !STATUS clause value
      IF(.NOT.file%isNew()) THEN
        statusvar='OLD'
      ELSE
        IF(file%overwrite) THEN
          statusvar='REPLACE'
        ELSE
          statusvar='NEW'
        ENDIF
      ENDIF
      !FORM clause value
      IF(file%isFormatted()) THEN
        formvar='FORMATTED'
      ELSE
        formvar='UNFORMATTED'
      ENDIF
      !ACCESS clause value
      IF(file%isDirect()) THEN
        accessvar='DIRECT'
        reclval=file%reclval
      ELSE
        IF(file%isStream()) THEN
          accessvar='STREAM'
        ELSE
          accessvar='SEQUENTIAL'
        ENDIF
        reclval=0
      ENDIF
      !ACTION clause value
      IF(file%isRead() .AND. .NOT.file%isWrite()) THEN
        actionvar='READ'
      ELSEIF(.NOT.file%isRead() .AND. file%isWrite()) THEN
        actionvar='WRITE'
      ELSEIF(file%isRead() .AND. file%isWrite()) THEN
        actionvar='READWRITE'
      ENDIF
      !PAD clause value
      IF(file%padstat) THEN
        padvar='YES'
      ELSE
        padvar='NO'
      ENDIF

      !The POSITION clause is illegal to use in the OPEN statement if
      !the file is DIRECT access.
      !The PAD clause is illegal to use in the OPEN statement if the file
      !is UNFORMATTED.
      IF(file%isDirect()) THEN
        IF(file%isFormatted()) THEN
          !Omit the POSITION clause, and include the PAD clause
          OPEN(UNIT=file%unitno,STATUS=TRIM(statusvar),PAD=TRIM(padvar), &
              ACCESS=TRIM(accessvar),FORM=TRIM(formvar),RECL=reclval, &
              ACTION=TRIM(actionvar),FILE=TRIM(file%getFilePath())// &
              TRIM(file%getFileName())//TRIM(file%getFileExt()), &
              IOSTAT=ioerr,IOMSG=iomsg)
        ELSE
          !Omit the POSITION clause, and the PAD clause
          OPEN(UNIT=file%unitno,STATUS=TRIM(statusvar),RECL=reclval, &
              ACCESS=TRIM(accessvar),FORM=TRIM(formvar),IOSTAT=ioerr, &
              ACTION=TRIM(actionvar),FILE=TRIM(file%getFilePath())// &
              TRIM(file%getFileName())//TRIM(file%getFileExt()),IOMSG=iomsg)
        ENDIF
      ELSE
        IF(file%isFormatted()) THEN
          !Include the POSITION clause, and the PAD clause
          OPEN(UNIT=file%unitno,STATUS=TRIM(statusvar),PAD=TRIM(padvar), &
              ACCESS=TRIM(accessvar),FORM=TRIM(formvar),IOSTAT=ioerr, &
              POSITION=TRIM(file%posopt),ACTION=TRIM(actionvar), &
              FILE=TRIM(file%getFilePath())//TRIM(file%getFileName())// &
              TRIM(file%getFileExt()),IOMSG=iomsg)
        ELSE
          !Include the POSITION clause, omit the PAD clause
          OPEN(UNIT=file%unitno,STATUS=TRIM(statusvar), &
              ACCESS=TRIM(accessvar),FORM=TRIM(formvar),IOSTAT=ioerr, &
              POSITION=TRIM(file%posopt),ACTION=TRIM(actionvar), &
              FILE=TRIM(file%getFilePath())//TRIM(file%getFileName())// &
              TRIM(file%getFileExt()),IOMSG=iomsg)
        ENDIF
      ENDIF

      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4,a,i4)') 'Error opening file "'// &
            TRIM(file%getFilePath())//TRIM(file%getFileName())// &
            TRIM(file%getFileExt())//'" (UNIT=',file%unitno, &
            ') IOSTAT=',ioerr
        CALL file%e%raiseError(modName//'::'//myName//' - '//emesg &
            //' IOMSG="'//TRIM(iomsg)//'"')
      ELSE
        CALL file%setOpenStat(.TRUE.)
        CALL file%setEOFStat(.FALSE.)
      ENDIF
    ENDIF
  ELSE
    CALL file%e%raiseError(modName//'::'//myName//' - '// &
        'Cannot open file! Object has not been initialized!')
  ENDIF
ENDSUBROUTINE open_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Close the file referenced by a fortran file object.
!> @param file Fortran file object
!>
!> File will not be deleted when closed.
!>
SUBROUTINE close_fortran_file(file)
  CHARACTER(LEN=*),PARAMETER :: myName='CLOSE_FORTRAN_FILE'
  CLASS(FortranFileType),INTENT(INOUT) :: file

  IF(file%initstat) THEN
    IF(file%isOpen()) THEN
      CLOSE(UNIT=file%unitno,STATUS='KEEP',IOSTAT=ioerr)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4,a,i4)') 'Error closing file (UNIT=', &
            file%unitno,') IOSTAT=',ioerr
        CALL file%e%raiseError(modName//'::'//myName//' - '//emesg)
      ELSE
        CALL file%setOpenStat(.FALSE.)
      ENDIF
    ELSE
      WRITE(emesg,'(a,i4,a)') 'Cannot close file (UNIT=', &
          file%unitno,') File is not open!'
      CALL file%e%raiseDebug(modName//'::'//myName//' - '//emesg)
    ENDIF
  ELSE
    CALL file%e%raiseDebug(modName//'::'//myName//' - '// &
        'Cannot close file! File object has not been initialized!')
  ENDIF
ENDSUBROUTINE close_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Delete a file referenced by the fortran file object.
!> @param file Fortran file object
!>
!> Tries to delete the file regardless fo whether or not it is open.
!>
SUBROUTINE delete_fortran_file(file)
  CHARACTER(LEN=*),PARAMETER :: myName='DELETE_FORTRAN_FILE'
  CLASS(FortranFileType),INTENT(INOUT) :: file

  IF(file%initstat) THEN
    IF(file%isOpen()) THEN
      CLOSE(UNIT=file%unitno,STATUS='DELETE',IOSTAT=ioerr)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4,a,i4)') 'Error deleting file (UNIT=', &
            file%unitno,') IOSTAT=',ioerr
        CALL file%e%raiseError(modName//'::'//myName//' - '//emesg)
      ELSE
        CALL file%setOpenStat(.FALSE.)
      ENDIF
    ELSE
      OPEN(UNIT=file%unitno,FILE=TRIM(file%getFilePath())// &
          TRIM(file%getFileName())//TRIM(file%getFileExt()), &
          IOSTAT=ioerr)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4,a,i4)') 'Error deleting file (UNIT=', &
            file%unitno,') IOSTAT=',ioerr
        CALL file%e%raiseError(modName//'::'//myName//' - '//emesg)
      ENDIF
      CLOSE(UNIT=file%unitno,STATUS='DELETE',IOSTAT=ioerr)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4,a,i4)') 'Error deleting file (UNIT=', &
            file%unitno,') IOSTAT=',ioerr
        CALL file%e%raiseError(modName//'::'//myName//' - '//emesg)
      ELSE
        CALL file%setOpenStat(.FALSE.)
      ENDIF
    ENDIF
  ELSE
    CALL file%e%raiseDebug(modName//'::'//myName//' - '// &
        'Cannot delete file! File object has not been initialized!')
  ENDIF
ENDSUBROUTINE delete_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Use rewind on a fortran file object.
!> @param file Fortran file object
!>
SUBROUTINE rewind_fortran_file(file)
  CHARACTER(LEN=*),PARAMETER :: myName='REWIND_FORTRAN_FILE'
  CLASS(FortranFileType),INTENT(INOUT) :: file

  IF(file%initstat) THEN
    IF(file%isOpen()) THEN
      REWIND(UNIT=file%unitno,IOSTAT=ioerr)
      CALL file%setEOFstat(.FALSE.)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4,a,i4)') 'Error rewinding file (UNIT=', &
            file%unitno,') IOSTAT=',ioerr
        CALL file%e%raiseError(modName//'::'//myName//' - '//emesg)
      ENDIF
    ELSE
      WRITE(emesg,'(a,i4,a)') 'Cannot rewind file (UNIT=',file%unitno, &
          '). File not is not open!'
      CALL file%e%raiseDebug(modName//'::'//myName//' - '//emesg)
    ENDIF
  ELSE
    CALL file%e%raiseDebug(modName//'::'//myName//' - '// &
        'Cannot rewind file! File object has not been initialized!')
  ENDIF
ENDSUBROUTINE rewind_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Use backspace on a fortran file object.
!> @param file Fortran file object
!>
SUBROUTINE backspace_fortran_file(file)
  CHARACTER(LEN=*),PARAMETER :: myName='BACKSPACE_FORTRAN_FILE'
  CLASS(FortranFileType),INTENT(INOUT) :: file

  IF(file%initstat) THEN
    IF(file%isOpen()) THEN
      BACKSPACE(UNIT=file%unitno,IOSTAT=ioerr)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4,a,i4)') 'Error backspacing file (UNIT=', &
            file%unitno,') IOSTAT=',ioerr
        CALL file%e%raiseError(modName//'::'//myName//' - '//emesg)
      ELSE
        IF(file%isEOF()) CALL file%setEOFstat(.FALSE.)
      ENDIF
    ELSE
      WRITE(emesg,'(a,i4,a)') 'Cannot backspace file (UNIT=',file%unitno, &
          '). File not is not open!'
      CALL file%e%raiseDebug(modName//'::'//myName//' - '//emesg)
    ENDIF
  ELSE
    CALL file%e%raiseDebug(modName//'::'// myName//' - '// &
        'Cannot backspace file! File object has not been initialized!')
  ENDIF
ENDSUBROUTINE backspace_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Returns the value of file%initstat
!> @param file Fortran file object
!>
PURE FUNCTION isInit_fortran_file(file) RESULT(bool)
  CLASS(FortranFileType),INTENT(IN) :: file
  LOGICAL(SBK) :: bool
  bool=file%initstat
ENDFUNCTION isInit_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Sets the status option for the file for the next call to open.
!> @param file Fortran file object
!> @param status the value of the status option
!>
!> The value of status is the same as that defined in the Fortran intrinsic OPEN
!> statment ("OLD", "NEW", "UKNOWN", "REPLACE", "SCRATCH"). The latter three
!> are all treated as replace. These correspond to the fopen implementation
!> and how the value of STATUS is derived from the newstat and overwrite
!> attribute.
!>
SUBROUTINE setStatus_fortran_file(file,status)
  CHARACTER(LEN=*),PARAMETER :: myName='setStatus_fortran_file'
  CLASS(FortranFileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: status
  CHARACTER(LEN=LEN(status)) :: new_status
  IF(file%initstat) THEN
    IF(.NOT.file%isOpen()) THEN
      new_status=status
      CALL toUPPER(new_status)
      SELECT CASE(new_status)
      CASE('OLD') !File already exists
        file%newstat=.FALSE.
        file%overwrite=.FALSE.
      CASE('NEW') !File does not exist and will be created
        file%newstat=.TRUE.
        file%overwrite=.FALSE.
      CASE('SCRATCH','REPLACE','UNKNOWN')
        file%newstat=.TRUE.
        file%overwrite=.TRUE.
      CASE DEFAULT
        CALL file%e%raiseError(modName//'::'//myName//' - Illegal '// &
            'value ('//status//') for input argument STATUS!')
      ENDSELECT
    ELSE
      CALL file%e%raiseError(modName//'::'//myName//' - File status '// &
          'cannot be changed while file is open!')
    ENDIF
  ELSE
    CALL file%e%raiseError(modName//'::'//myName//' - File status '// &
        'cannot be changed on uninitialized file!')
  ENDIF
ENDSUBROUTINE setStatus_fortran_file
!
!------------------------------------------------------------------------------
!> @brief This subroutine writes a 1-D array of strings to an output file as
!>        lines.
!> @param file The fortran file where the lines are written.
!> @param lines The 1-D array of strings to write.
!>
SUBROUTINE write_str_1a_fortran_file(file,lines)
  CLASS(FortranFileType),INTENT(INOUT) :: file
  TYPE(StringType),INTENT(IN) :: lines(:)
  INTEGER(SIK) :: i

  REQUIRE(file%initstat)
  REQUIRE(file%isOpen())
  !Loop over the lines, write them
  DO i=1,SIZE(lines)
    WRITE(file%getUnitNo(),'(3x,a)') CHAR(lines(i))
  ENDDO
ENDSUBROUTINE write_str_1a_fortran_file
!
!-------------------------------------------------------------------------------
!> @brief Returns a unit number that is presently not in use.
!> @param istt optional input for where to start searching for an unused unit
!>        number
!> @returns newlun a unit number that is not currently in use by this process
!>
FUNCTION FortranFile_get_new_unit(istt) RESULT(newlun)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: istt
  INTEGER(SIK) :: newlun,isafe
  LOGICAL(SBK) :: ostat

  newlun=MAX(MAX(OUTPUT_UNIT,ERROR_UNIT),INPUT_UNIT)+1
  IF(PRESENT(istt)) newlun=istt

  INQUIRE(UNIT=newlun,OPENED=ostat)
  isafe=0
  DO WHILE(ostat)
    newlun=newlun+1
    isafe=isafe+1
    INQUIRE(UNIT=newlun,OPENED=ostat)

    !Catch to prevent an infinite loop
    !Return a bad value as apparently all unit numbers
    !are in use.
    IF(isafe == 100000) THEN
      newlun=-666
      EXIT
    ENDIF
  ENDDO
ENDFUNCTION FortranFile_get_new_unit
!
ENDMODULE FileType_Fortran
