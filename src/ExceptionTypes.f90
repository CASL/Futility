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
#include "Futility_DBC.h"
  USE Futility_DBC
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
  PUBLIC :: ExceptionTypeDebug
  PUBLIC :: ExceptionTypeInformation
  PUBLIC :: EXCEPTION_MAX_MESG_LEN

  !> The maximum size of an exception message
  INTEGER(SIK),PARAMETER :: EXCEPTION_MAX_MESG_LEN=512

  TYPE, ABSTRACT :: ExceptionTypeBase
    !> Initialization status
    LOGICAL(SBK),PUBLIC :: isInit=.FALSE.
    !> Integer tag identifying the exception type
    INTEGER(SIK),PRIVATE :: tag=-1
    !> Counter for number of times this exception was raised
    INTEGER(SIK),PRIVATE :: counter=0
    !> Stopmode
    LOGICAL(SBK),PRIVATE :: stopmode=.FALSE.
    !> Quiet
    LOGICAL(SBK),PRIVATE :: quiet=.FALSE.
    !> Verbose
    LOGICAL(SBK),PRIVATE :: verbose=.FALSE.
!
!List of type bound procedures (methods) for the Exception Handler object
    CONTAINS
      !> @copybrief ExceptionTypes::init_ExceptionTypeBase
      !> @copydetails ExceptionTypes::init_ExceptionTypeBase
      PROCEDURE,PASS :: init => init_ExceptionTypeBase
      !> @copybrief ExceptionTypes::init_ExceptionTypeBase
      !> @copydetails ExceptionTypes::init_ExceptionTypeBase
      PROCEDURE,PASS,NON_OVERRIDABLE :: initBase => init_ExceptionTypeBase
      !> @copybrief ExceptionTypes::onRaise_ExceptionTypeBase
      !> @copydetails ExceptionTypes::onRaise_ExceptionTypeBase
      PROCEDURE,PASS :: onRaise => onRaise_ExceptionTypeBase
      !> @copybrief ExceptionTypes::setStopMode_ExceptionTypeBase
      !> @copydetails ExceptionTypes::setStopMode_ExceptionTypeBase
      PROCEDURE,PASS :: setStopMode => setStopMode_ExceptionTypeBase
      !> @copybrief ExceptionTypes::setQuietMode_ExceptionTypeBase
      !> @copydetails ExceptionTypes::setQuietMode_ExceptionTypeBase
      PROCEDURE,PASS :: setQuietMode => setQuietMode_ExceptionTypeBase
      !> @copybrief ExceptionTypes::setVerboseMode_ExceptionTypeBase
      !> @copydetails ExceptionTypes::setVerboseMode_ExceptionTypeBase
      PROCEDURE,PASS :: setVerboseMode => setVerboseMode_ExceptionTypeBase
      !> @copybrief ExceptionTypes::resetCounter_ExceptionTypeBase
      !> @copydetails ExceptionTypes::resetCounter_ExceptionTypeBase
      PROCEDURE,PASS :: resetCounter => resetCounter_ExceptionTypeBase
      !> @copybrief ExceptionTypes::getCounter_ExceptionTypeBase
      !> @copydetails ExceptionTypes::getCounter_ExceptionTypeBase
      PROCEDURE,PASS :: getCounter => getCounter_ExceptionTypeBase
      !> @copybrief ExceptionTypes::logMessage_ExceptionTypeBase
      !> @copydetails ExceptionTypes::logMessage_ExceptionTypeBase
      PROCEDURE,PASS :: logMessage => logMessage_ExceptionTypeBase
      !> @copybrief ExceptionTypes::getTag_ExceptionTypeBase
      !> @copydetails ExceptionTypes::getTag_ExceptionTypeBase
      PROCEDURE,PASS :: getTag => getTag_ExceptionTypeBase
      !>
      !> abstract interface functions (absintfc)
      !>
      !> @copybrief ExceptionTypes::et_genprefix_absintfc
      !> @copydetails ExceptionTypes::et_genprefix_absintfc
      PROCEDURE(et_genprefix_absintfc),DEFERRED,PASS :: genPrefix

      !> @
  ENDTYPE ExceptionTypeBase

!> Concrete type for Information exception
  TYPE,EXTENDS(ExceptionTypeBase) :: ExceptionTypeInformation
    CONTAINS
      PROCEDURE,PASS :: genPrefix => genPrefix_ExceptionTypeInformation
  ENDTYPE ExceptionTypeInformation

!> Concrete type for Warning exception
  TYPE,EXTENDS(ExceptionTypeBase) :: ExceptionTypeWarning
    CONTAINS
      PROCEDURE,PASS :: genPrefix => genPrefix_ExceptionTypeWarning
  ENDTYPE ExceptionTypeWarning

!> Concrete type for Debug exception
  TYPE,EXTENDS(ExceptionTypeBase) :: ExceptionTypeDebug
    CONTAINS
      PROCEDURE,PASS :: genPrefix => genPrefix_ExceptionTypeDebug
  ENDTYPE ExceptionTypeDebug

!> Concrete type for Error exception
  TYPE,EXTENDS(ExceptionTypeBase) :: ExceptionTypeError
    CONTAINS
      PROCEDURE,PASS :: genPrefix => genPrefix_ExceptionTypeError
  ENDTYPE ExceptionTypeError

!> Concrete type for Fatal exception
  TYPE,EXTENDS(ExceptionTypeBase) :: ExceptionTypeFatal
    CONTAINS
      PROCEDURE,PASS :: genPrefix => genPrefix_ExceptionTypeFatal
  ENDTYPE ExceptionTypeFatal
!
ABSTRACT INTERFACE
  !> @brief Abstract interface for appending prefix to error msg
  SUBROUTINE et_genprefix_absintfc(this,prefix)
    IMPORT :: ExceptionTypeBase
    IMPORT :: EXCEPTION_MAX_MESG_LEN
    CLASS(ExceptionTypeBase),INTENT(INOUT) :: this
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LEN),INTENT(INOUT) :: prefix
  ENDSUBROUTINE et_genprefix_absintfc
ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initialize exception
!>
    SUBROUTINE init_ExceptionTypeBase(this,tag)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this
      INTEGER(SIK),INTENT(IN) :: tag
      REQUIRE(.NOT. this%isInit)

      this%tag = tag
      this%isInit = .TRUE.

    ENDSUBROUTINE init_ExceptionTypeBase
!
!-------------------------------------------------------------------------------
!> @brief Executed on raise
!>
    SUBROUTINE onRaise_ExceptionTypeBase(this,isLogActive,logUnit,mesg)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this
      LOGICAL(SBK),INTENT(IN) :: isLogActive
      INTEGER(SIK),INTENT(IN) :: logUnit
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LEN),INTENT(INOUT) :: mesg

      REQUIRE(this%isInit)

      this%counter = this%counter + 1
      CALL this%logMessage(this%quiet,isLogActive,logUnit,mesg)
      CALL exceptionStop(this%stopmode)

    ENDSUBROUTINE onRaise_ExceptionTypeBase
!
!-------------------------------------------------------------------------------
!> @brief Set the stopmode
!>
    SUBROUTINE setStopMode_ExceptionTypeBase(this,stopmode)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this
      LOGICAL(SBK),INTENT(IN) :: stopmode

      this%stopmode = stopmode

    ENDSUBROUTINE setStopMode_ExceptionTypeBase
!
!-------------------------------------------------------------------------------
!> @brief Set the quiet mode
!>
    SUBROUTINE setQuietMode_ExceptionTypeBase(this,quiet)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this
      LOGICAL(SBK),INTENT(IN) :: quiet

      this%quiet = quiet

    ENDSUBROUTINE setQuietMode_ExceptionTypeBase
!
!-------------------------------------------------------------------------------
!> @brief Set the quiet mode
!>
    SUBROUTINE setVerboseMode_ExceptionTypeBase(this,verbose)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this
      LOGICAL(SBK),INTENT(IN) :: verbose

      this%verbose = verbose

    ENDSUBROUTINE setVerboseMode_ExceptionTypeBase
!
!-------------------------------------------------------------------------------
!> @brief Resets the error counter
!>
    SUBROUTINE resetCounter_ExceptionTypeBase(this)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this

      this%counter = 0

    ENDSUBROUTINE resetCounter_ExceptionTypeBase
!
!-------------------------------------------------------------------------------
!> @brief Get the current counter value
!>
    FUNCTION getCounter_ExceptionTypeBase(this) RESULT(counterOut)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this
      INTEGER(SIK) :: counterOut

      REQUIRE(this%isInit)

      counterOut = this%counter

    ENDFUNCTION getCounter_ExceptionTypeBase
!
!-------------------------------------------------------------------------------
!> @brief Logs mesg to file
!>
    SUBROUTINE logMessage_ExceptionTypeBase(this,isQuiet,isLogActive,logUnit,mesg)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this
      LOGICAL(SBK),INTENT(IN) :: isQuiet
      LOGICAL(SBK),INTENT(IN) :: isLogActive
      INTEGER(SIK),INTENT(IN) :: logUnit
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LEN),INTENT(INOUT) :: mesg
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LEN) :: prefix
      INTEGER(SIK) :: ioerr1,ioerr2,prefixLen

      REQUIRE(this%isInit)
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
      WRITE(mesg,'(a)') TRIM(prefix)//' - '//TRIM(mesg(1:EXCEPTION_MAX_MESG_LEN-prefixLen))
    ENDSUBROUTINE logMessage_ExceptionTypeBase
!
!-------------------------------------------------------------------------------
!> @brief Get tag
!>
    FUNCTION getTag_ExceptionTypeBase(this) RESULT(tag)
      CLASS(ExceptionTypeBase),INTENT(INOUT) :: this
      INTEGER(SIK) :: tag

      REQUIRE(this%isInit)

      tag = this%tag

    ENDFUNCTION getTag_ExceptionTypeBase
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
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LEN),INTENT(INOUT) :: prefix

      WRITE(prefix,'(a)')  '#### EXCEPTION_ERROR ####'

    ENDSUBROUTINE genPrefix_ExceptionTypeError
!
!-------------------------------------------------------------------------------
!> @brief Prefix mesg for type ExceptionTypeWarning
!>
    SUBROUTINE genPrefix_ExceptionTypeWarning(this,prefix)
      CLASS(ExceptionTypeWarning),INTENT(INOUT) :: this
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LEN),INTENT(INOUT) :: prefix

      WRITE(prefix,'(a)')  '#### EXCEPTION_WARNING ####'

    ENDSUBROUTINE genPrefix_ExceptionTypeWarning
!
!-------------------------------------------------------------------------------
!> @brief Prefix mesg for type ExceptionTypeInformation
!>
    SUBROUTINE genPrefix_ExceptionTypeInformation(this,prefix)
      CLASS(ExceptionTypeInformation),INTENT(INOUT) :: this
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LEN),INTENT(INOUT) :: prefix

      WRITE(prefix,'(a)')  '#### EXCEPTION_INFORMATION ####'

    ENDSUBROUTINE genPrefix_ExceptionTypeInformation
!
!-------------------------------------------------------------------------------
!> @brief Prefix mesg for type ExceptionTypeFatal
!>
    SUBROUTINE genPrefix_ExceptionTypeFatal(this,prefix)
      CLASS(ExceptionTypeFatal),INTENT(INOUT) :: this
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LEN),INTENT(INOUT) :: prefix

      WRITE(prefix,'(a)')  '#### EXCEPTION_FATAL ####'

    ENDSUBROUTINE genPrefix_ExceptionTypeFatal
!
!-------------------------------------------------------------------------------
!> @brief Prefix mesg for type ExceptionTypeDebug
!>
    SUBROUTINE genPrefix_ExceptionTypeDebug(this,prefix)
      CLASS(ExceptionTypeDebug),INTENT(INOUT) :: this
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LEN),INTENT(INOUT) :: prefix

      WRITE(prefix,'(a)')  '#### EXCEPTION_DEBUG ####'

    ENDSUBROUTINE genPrefix_ExceptionTypeDebug
!
ENDMODULE ExceptionTypes
