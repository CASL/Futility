!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module for defining exception types used by the
!>  ExceptionHandler
!>
!> @author William Gurecky
!>   @date 12/16/2019
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ExceptionTypes
  USE ISO_FORTRAN_ENV
  USE IntrType

  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: ExceptionTypeBase
  PUBLIC :: ExceptionTypeError
  PUBLIC :: ExceptionTypeWarning
  PUBLIC :: ExceptionTypeFatal
  PUBLIC :: ExceptionTypeInformation
  PUBLIC :: EXCEPTION_MAX_MESG_LENGTH

  !> The maximum size of an exception message
  INTEGER(SIK),PARAMETER :: EXCEPTION_MAX_MESG_LENGTH=512

  TYPE, ABSTRACT :: ExceptionTypeBase
    !> Exception identifier
    INTEGER(SIK),PRIVATE :: tag=-1_SIK
    !> Counter for number of times this exception was raised
    INTEGER(SIK),PRIVATE :: counter=0_SIK
    !> Counter for number of times this exception was raised
    LOGICAL(SBK),PRIVATE :: stopmode=.FALSE.
!
!List of type bound procedures (methods) for the Exception Handler object
    CONTAINS
      !> @copybrief ExceptionTypes::onRaise_ExceptionTypeBase
      !> @copydetails ExceptionTypes::onRaise_ExceptionTypeBase
      PROCEDURE,PASS :: onRaise => onRaise_ExceptionTypeBase
      !> @copybrief ExceptionTypes::setStopMode_ExceptionTypeBase
      !> @copydetails ExceptionTypes::setStopMode_ExceptionTypeBase
      PROCEDURE,PASS :: setStopMode => setStopMode_ExceptionTypeBase
      !> @copybrief ExceptionTypes::resetCounter_ExceptionTypeBase
      !> @copydetails ExceptionTypes::resetCounter_ExceptionTypeBase
      PROCEDURE,PASS :: resetCounter => resetCounter_ExceptionTypeBase
      !> @copybrief ExceptionTypes::getCounter_ExceptionTypeBase
      !> @copydetails ExceptionTypes::getCounter_ExceptionTypeBase
      PROCEDURE,PASS :: getCounter => getCounter_ExceptionTypeBase
      !> @copybrief ExceptionTypes::logMessage_ExceptionTypeBase
      !> @copydetails ExceptionTypes::logMessage_ExceptionTypeBase
      PROCEDURE,PASS :: logMessage => logMessage_ExceptionTypeBase
      !>
      !> abstract interface functions (absintfc)
      !>
      !> @copybrief ExceptionTypes::et_genprefix_absintfc
      !> @copydetails ExceptionTypes::et_genprefix_absintfc
      PROCEDURE(et_genprefix_absintfc),DEFERRED,PASS genPrefix

      !> @
  ENDTYPE ExceptionTypeBase

!> Concrete type for Error exception
  TYPE,EXTENDS(ExceptionTypeBase) :: ExceptionTypeError
    INTEGER(SIK),PRIVATE :: tag=4_SIK
    CONTAINS
      PROCEDURE,PASS :: genPrefix => genPrefix_ExceptionTypeError
  ENDTYPE ExceptionTypeError

!> Concrete type for Warning exception
  TYPE,EXTENDS(ExceptionTypeBase) :: ExceptionTypeWarning
    INTEGER(SIK),PRIVATE :: tag=2_SIK
    CONTAINS
      PROCEDURE,PASS :: genPrefix => genPrefix_ExceptionTypeWarning
  ENDTYPE ExceptionTypeWarning

!> Concrete type for Fatal exception
  TYPE,EXTENDS(ExceptionTypeBase) :: ExceptionTypeFatal
    INTEGER(SIK),PRIVATE :: tag=5_SIK
    LOGICAL(SBK),PRIVATE :: stopmode=.TRUE.
    CONTAINS
      PROCEDURE,PASS :: genPrefix => genPrefix_ExceptionTypeFatal
  ENDTYPE ExceptionTypeFatal

!> Concrete type for Debug exception
  TYPE,EXTENDS(ExceptionTypeBase) :: ExceptionTypeDebug
    INTEGER(SIK),PRIVATE :: tag=3_SIK
    CONTAINS
      PROCEDURE,PASS :: genPrefix => genPrefix_ExceptionTypeDebug
  ENDTYPE ExceptionTypeDebug

!> Concrete type for Information exception
  TYPE,EXTENDS(ExceptionTypeBase) :: ExceptionTypeInformation
    INTEGER(SIK),PRIVATE :: tag=1_SIK
    CONTAINS
      PROCEDURE,PASS :: genPrefix => genPrefix_ExceptionTypeInformation
  ENDTYPE ExceptionTypeInformation
!
ABSTRACT INTERFACE
  !> @brief Abstract interface for logging error msg
  SUBROUTINE et_genprefix_absintfc(this,prefix)
    IMPORT :: ExceptionTypeBase
    CLASS(ExceptionTypeBase),INTENT(IN)
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH),INTENT(INOUT) :: prefix
  ENDSUBROUTINE et_message_absintfc
ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Executed on raise
!>
    SUBROUTINE onRaise_ExceptionTypeBase(this,isQuiet,isLogActive,logUnit,mesg)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this
      INTEGER(SIK),INTENT(IN) :: eCode
      LOGICAL(SBK),INTENT(INOUT) :: isQuiet
      LOGICAL(SBK),INTENT(IN) :: isLogActive
      INTEGER(SIK),INTENT(IN) :: logUnit
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH),INTENT(INOUT) :: mesg

      this%counter = this%counter + 1_SIK
      CALL this%logMessage(isQuiet,isLogActive,logUnit,mesg)
      CALL exceptionStop(this%stopmode)

    ENDSUBROUTINE onRaise_ExceptionTypeBase
!
!-------------------------------------------------------------------------------
!> @brief Set the stopmode
!>
    SUBROUTINE setStopMode_ExceptionTypeBase(this,stopmode)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this
      INTEGER(SIK),INTENT(IN) :: stopmode

      this%stopmode = stopmode

    ENDSUBROUTINE setStopMode_ExceptionTypeBase
!
!-------------------------------------------------------------------------------
!> @brief Resets the error counter
!>
    SUBROUTINE resetCounter_ExceptionTypeBase(this)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this

      this%counter = 0_SIK

    ENDSUBROUTINE resetCounter_ExceptionTypeBase
!
!-------------------------------------------------------------------------------
!> @brief Get the current counter value
!>
    SUBROUTINE getCounter_ExceptionTypeBase(this,counterOut)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this
      INTEGER(SIK),INTENT(OUT) :: counterOut

      counterOut = this%counter

    ENDSUBROUTINE getCounter_ExceptionTypeBase
!
!-------------------------------------------------------------------------------
!> @brief Logs mesg to file
!>
    SUBROUTINE logMessage_ExceptionTypeBase(this,isQuiet,isLogActive,logUnit)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this
      INTEGER(SIK),INTENT(IN) :: eCode
      LOGICAL(SBK),INTENT(INOUT) :: isQuiet
      LOGICAL(SBK),INTENT(IN) :: isLogActive
      INTEGER(SIK),INTENT(IN) :: logUnit
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH),INTENT(INOUT) :: mesg
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: prefix
      INTEGER(SIK) :: ioerr1,ioerr2,prefixLen

      CALL this%genPrefix(prefix)

      !Write to the default standard error output
      IF(.NOT.isQuiet) THEN
        WRITE(ERROR_UNIT,'(a)') TRIM(prefix)
        WRITE(ERROR_UNIT,'(6x,a)') TRIM(mesg)
        FLUSH(ERROR_UNIT)
      ENDIF

      !Write to the log file
      IF(isLogActive) THEN
        WRITE(logUnit,'(a)',IOSTAT=ioerr1) TRIM(prefix)
        WRITE(logUnit,'(6x,a)',IOSTAT=ioerr2) TRIM(mesg)
        FLUSH(logUnit)

        !Additional error message if problem writing to log file
        IF(ioerr1 /= 0 .OR. ioerr2 /= 0) THEN
          WRITE(ERROR_UNIT,'(a)') '#### EXCEPTION_INFORMATION ####'
          IF(isQuiet) THEN
            !If quiet mode, then print the message that failed.
            WRITE(ERROR_UNIT,'(6x,a)') 'Problem writing to log file.'
            WRITE(ERROR_UNIT,'(6x,a)') 'Original Message: "'// &
                                       TRIM(prefix)//' - '//TRIM(mesg)//'"'
          ELSE
            !Message was already printed.
            WRITE(ERROR_UNIT,'(6x,a)') 'Problem writing above message '// &
                                       'to log file.'
          ENDIF
        ENDIF
      ENDIF

      !Set the message to be included as one line back to exception object
      prefixLen = LEN_TRIM(prefix)+3
      WRITE(mesg,'(a)') TRIM(prefix)//' - '//TRIM(mesg(1:EXCEPTION_MAX_MESG_LENGTH-prefixLen))
!
!-------------------------------------------------------------------------------
!> @brief Stops execution based on value of stopmode
!> @param stopmode boolean to indicate whether or not to stop execution
!>
    SUBROUTINE exceptionStop(stopmode)
#ifdef HAVE_MPI
      INCLUDE 'mpif.h'
      INTEGER(SIK) :: ierr
#endif
      LOGICAL(SBK),INTENT(IN) :: stopmode
#ifdef HAVE_MPI
      IF(stopmode) CALL MPI_Abort(MPI_COMM_WORLD,666,ierr)
#else
      IF(stopmode) THEN
        STOP 666
      ENDIF
#endif
    ENDSUBROUTINE exceptionStop
!
!-------------------------------------------------------------------------------
!> @brief Prefix mesg for type ExceptionTypeError
!>
    SUBROUTINE genPrefix_ExceptionTypeError(this,prefix)
      CLASS(ExceptionTypeError),INTENT(INOUT) :: this
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH),INTENT(INOUT) :: prefix

      WRITE(prefix,'(a)')  '#### EXCEPTION_ERROR ####'

    ENDSUBROUTINE genPrefix_ExceptionTypeError
!
!-------------------------------------------------------------------------------
!> @brief Prefix mesg for type ExceptionTypeWarning
!>
    SUBROUTINE genPrefix_ExceptionTypeWarning(this,prefix)
      CLASS(ExceptionTypeWarning),INTENT(INOUT) :: this
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH),INTENT(INOUT) :: prefix

      WRITE(prefix,'(a)')  '#### EXCEPTION_WARNING ####'

    ENDSUBROUTINE genPrefix_ExceptionTypeWarning
!
!-------------------------------------------------------------------------------
!> @brief Prefix mesg for type ExceptionTypeInformation
!>
    SUBROUTINE genPrefix_ExceptionTypeInformation(this,prefix)
      CLASS(ExceptionTypeInformation),INTENT(INOUT) :: this
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH),INTENT(INOUT) :: prefix

      WRITE(prefix,'(a)')  '#### EXCEPTION_INFORMATION ####'

    ENDSUBROUTINE genPrefix_ExceptionTypeInformation
!
!-------------------------------------------------------------------------------
!> @brief Prefix mesg for type ExceptionTypeFatal
!>
    SUBROUTINE genPrefix_ExceptionTypeFatal(this,prefix)
      CLASS(ExceptionTypeFatal),INTENT(INOUT) :: this
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH),INTENT(INOUT) :: prefix

      WRITE(prefix,'(a)')  '#### EXCEPTION_FATAL ####'

    ENDSUBROUTINE genPrefix_ExceptionTypeFatal
!
!-------------------------------------------------------------------------------
!> @brief Prefix mesg for type ExceptionTypeDebug
!>
    SUBROUTINE genPrefix_ExceptionTypeDebug(this,prefix)
      CLASS(ExceptionTypeDebug),INTENT(INOUT) :: this
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH),INTENT(INOUT) :: prefix

      WRITE(prefix,'(a)')  '#### EXCEPTION_DEBUG ####'

    ENDSUBROUTINE genPrefix_ExceptionTypeDebug
!
ENDMODULE ExceptionTypes
