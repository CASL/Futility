!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module for I/O defines the derived type for a Log File object.
!>
!> The log file type is an extension of the Fortran file type. Specifically it
!> is a write-only text file that overwrites any existing file when opened. The
!> purpose of the log file is to provide a log of the execution of a program or
!> subprogram. It provides additional methods for @ref FileType_Log::
!> message_log_file "message" which writes a message to the log file and
!> optionally to the prompt. The @ref FileType_Log::echo_log_file "setEcho"
!> method can be used to control echoeing of messages to the prompt, similarly
!> the @ref FileType_Log::message_log_file "message" method has an optional
!> input to over-ride the value of @c %echostat. This module is considered to be
!> an I/O utility module so it's public members should be accessed through @ref
!> IOutil "IOutil". This module should not be used directly except when it is
!> needed by another I/O utility module. This module is tested by
!> @c testIOutil.f90 and the coverage report can be found at the @ref
!> CodeCoverageReports "Code Coverage Reports" page. An example of how to use
!> the log file type is provided below and in the test.
!>
!> @par EXAMPLES
!> @code
!> PROGRAM FileExample
!>
!> USE IOutil
!> IMPLICIT NONE
!>
!> TYPE(LogFileType) :: ErrorLog
!>
!> !Initialize the log file for errors
!> CALL ErrorLog%initialize(UNIT=50,FILE='test.log')
!>
!> !Open the file
!> CALL ErrorLog%fopen()
!>
!> !Log an error message with a time stamp and echo it to the prompt
!> CALL ErrorLog%message('This is the error message',.TRUE.,.TRUE.)
!>
!> !Delete the log file
!> CALL ErrorLog%fdelete()
!>
!> ENDPROGRAM
!> @endcode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FileType_Log

USE ISO_FORTRAN_ENV
USE IntrType
USE ExceptionHandler
USE Times
USE IO_Strings
USE FileType_Fortran
IMPLICIT NONE
PRIVATE

!List of Public Members
PUBLIC :: LogFileType

!> Module name for error messages
CHARACTER(LEN=*),PARAMETER :: modName='FILETYPE_LOG'
!> Scratch variable for exception messages of this module
CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: emesg
!> Scratch variable for IOSTAT values
INTEGER(SIK) :: ioerr

!> @brief Derived type object for a log file, it is an extension of the @ref
!> FileType_Fortran "FortranFileType" object.
!>
!> This type provides two new attributes to the @ref FileType_Fortran
!> "FortranFileType" for an echo status and a timer. It overwrites the methods
!> for @ref FileType_Log::init_log_file "initialize" and @ref
!> FileType_Log::clear_log_file "clear", and provides @ref
!> FileType_Log::message_log_file "message", @ref FileType_Log::echo_log_file
!> "setEcho", and @ref FileType_Log::isecho_log_file "isEcho".
TYPE,EXTENDS(FortranFileType) :: LogFileType
  !> Control variable for echoing messages to standard output.
  LOGICAL(SBK),PRIVATE :: echostat=.FALSE.
  !> A timer object for the log file to record elapsed time.
  TYPE(TimerType) :: timer
!
!List of type bound procedures (methods) for the Log File type
  CONTAINS
    !> @copybrief FileType_Log::init_log_file
    !> @copydetails FileType_Log::init_log_file
    PROCEDURE,PASS :: initialize => init_log_file
    !> @copybrief FileType_Log::clear_log_file
    !> @copydetails FileType_Log::clear_log_file
    PROCEDURE,PASS :: clear => clear_log_file
    !> @copybrief FileType_Log::message_log_file
    !> @copydetails FileType_Log::message_log_file
    PROCEDURE,PASS :: message => message_log_file
    !> @copybrief FileType_Log::echo_log_file
    !> @copydetails FileType_Log::echo_log_file
    PROCEDURE,PASS :: setEcho => echo_log_file
    !> @copybrief FileType_Log::isecho_log_file
    !> @copydetails FileType_Log::isecho_log_file
    PROCEDURE,PASS :: isEcho => isecho_log_file
ENDTYPE LogFileType

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the log file object.
!> @param fileobj log file object.
!> @param unit Unit number to use for the file.
!> @param status Optional input is not used by this routine.
!> @param access Optional input is not used by this routine.
!> @param form Optional input is not used by this routine.
!> @param position Optional input is not used by this routine.
!> @param action Optional input is not used by this routine.
!> @param pad Optional input is not used by this routine.
!> @param recl Optional input is not used by this routine.
!>
SUBROUTINE init_log_file(fileobj,unit,file,status,access,form, &
                             position,action,pad,recl)
  CHARACTER(LEN=*),PARAMETER :: myName='init_log_file'
  CLASS(LogFileType),INTENT(INOUT) :: fileobj
  INTEGER(SIK),OPTIONAL,INTENT(IN) :: unit
  CHARACTER(LEN=*),INTENT(IN) :: file
  CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: status
  CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: access
  CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: form
  CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: position
  CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: action
  CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: pad
  INTEGER(SIK),OPTIONAL,INTENT(IN) :: recl
  CHARACTER(LEN=LEN(file)) :: fname

  fname=''
  IF(PRESENT(status)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
    ' - Optional input "STATUS" is being ignored. Value is "REPLACE".')
  IF(PRESENT(access)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
    ' - Optional input "ACCESS" is being ignored. Value is "SEQUENTIAL".')
  IF(PRESENT(form)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
    ' - Optional input "FORM" is being ignored. Value is "FORMATTED".')
  IF(PRESENT(action)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
    ' - Optional input "ACTION" is being ignored. Value is "WRITE".')
  IF(PRESENT(pad)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
    ' - Optional input "PAD" is being ignored. Value is "YES".')
  IF(PRESENT(position)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
    ' - Optional input "POSITION" is being ignored. Value is "ASIS".')
  IF(PRESENT(recl)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
    ' - Optional input "RECL" is being ignored. File is "SEQUENTIAL".')

  !Initialize the timer for the log file
  CALL fileobj%timer%setTimerHiResMode(.TRUE.)
  CALL getFileName(file,fname,fileobj%e)
  CALL fileobj%timer%setTimerName('Log_'//TRIM(fname)//'_Timer')

  !Initialize the rest of the log file
  CALL init_fortran_file(fileobj,unit,file,'REPLACE','SEQUENTIAL', &
    'FORMATTED','ASIS','WRITE')
ENDSUBROUTINE init_log_file
!
!-------------------------------------------------------------------------------
!> @brief Clears the log file object and resets its state to the unitialized
!> state.
!> @param file log file object.
!>
SUBROUTINE clear_log_file(file,ldel)
  CLASS(LogFileType),INTENT(INOUT) :: file
  LOGICAL(SBK),OPTIONAL,INTENT(IN) :: ldel
  LOGICAL(SBK) :: bool
  bool=.FALSE.
  CALL file%timer%ResetTimer()
  file%echostat=.FALSE.
  IF(PRESENT(ldel)) bool=ldel
  CALL clear_fortran_file(file,bool)
ENDSUBROUTINE clear_log_file
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of the echo status for the log messages
!> @param file log file object.
!> @param bool the logical value to set for the echo status
!>
!> The echo status means that messages written be the log file will be echoed to
!> standard output (e.g. the prompt).
!>
SUBROUTINE echo_log_file(file,bool)
  CLASS(LogFileType),INTENT(INOUT) :: file
  LOGICAL(SBK),INTENT(IN) :: bool
  file%echostat=bool
ENDSUBROUTINE echo_log_file
!
!-------------------------------------------------------------------------------
!> @brief Returns the echo status of the log file object
!> @param file log file object.
!> @returns bool the logical value of the echo status
!>
PURE FUNCTION isecho_log_file(file) RESULT(bool)
  CLASS(LogFileType),INTENT(IN) :: file
  LOGICAL(SBK) :: bool
  bool=file%echostat
ENDFUNCTION isecho_log_file
!
!-------------------------------------------------------------------------------
!> @brief Writes a message to the log file and optionally the prompt
!> @param file log file object.
!> @param mesg the meaasage to print to the log file
!> @param timestamp optional logical of whether or not to prepend the elapsed
!>        time to @c mesg
!> @param echo optional logical of whether or not to echo the message to the
!>        prompt as well. This overrides the echo status of the file object.
!>
SUBROUTINE message_log_file(file,mesg,timestamp,echo)
  CHARACTER(LEN=*),PARAMETER :: myName='MESSAGE_LOG_FILE'
  CLASS(LogFileType),INTENT(INOUT) :: file
  CHARACTER(LEN=*),INTENT(IN) :: mesg
  LOGICAL(SBK),OPTIONAL,INTENT(IN) :: timestamp
  LOGICAL(SBK),OPTIONAL,INTENT(IN) :: echo
  LOGICAL(SBK) :: echostat
  LOGICAL(SBK) :: tstat

  echostat=file%echostat
  IF(PRESENT(echo)) echostat=echo
  tstat=.FALSE.
  IF(PRESENT(timestamp)) tstat=timestamp
  !Timestamp the message
  IF(tstat) THEN
    !Write message to log file
    CALL file%timer%toc()
    IF(file%isOpen()) THEN
      WRITE(file%getUnitNo(),'(2x,a)',IOSTAT=ioerr) &
        TRIM(file%timer%getTimeHHMMSS())//' '//TRIM(mesg)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4)') 'Error writing message to log file! IOSTAT=' &
          ,ioerr
        CALL file%e%raiseError(modName//'::'//myName//' - '//emesg)
      ENDIF
    ENDIF
    !Write message to prompt
    IF(echostat) THEN
      WRITE(OUTPUT_UNIT,'(2x,a)',IOSTAT=ioerr)  &
        TRIM(file%timer%getTimeHHMMSS())//' '//TRIM(mesg)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4)') 'Error writing message to prompt! IOSTAT=' &
          ,ioerr
        CALL file%e%raiseError(modName//'::'//myName//' - '//emesg)
      ENDIF
    ENDIF
    CALL file%timer%tic()
  ELSE
    !Write message to log file
    IF(file%isOpen()) THEN
      WRITE(file%getUnitNo(),'(12x,a)',IOSTAT=ioerr) TRIM(mesg)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4)') 'Error writing message to log file! IOSTAT=' &
          ,ioerr
        CALL file%e%raiseError(modName//'::'//myName//' - '//emesg)
      ENDIF
    ENDIF
    !Write message to prompt
    IF(echostat) THEN
      WRITE(OUTPUT_UNIT,'(12x,a)',IOSTAT=ioerr) TRIM(mesg)
      IF(ioerr /= 0) THEN
        WRITE(emesg,'(a,i4)') 'Error writing message to prompt! IOSTAT=' &
          ,ioerr
        CALL file%e%raiseError(modName//'::'//myName//' - '//emesg)
      ENDIF
    ENDIF
  ENDIF
  !Flush output buffer
  IF(file%isOpen()) FLUSH(file%getUnitNo())
  IF(echostat) FLUSH(OUTPUT_UNIT)
ENDSUBROUTINE message_log_file
!
ENDMODULE FileType_Log
