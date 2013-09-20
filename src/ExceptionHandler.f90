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
!> @brief Utility module for simple error/exception handling.
!>
!> This module provides a Fortran 2003 object for basic exception handling
!> and error reporting. It defines 5 types of exceptions. The defined exceptions
!> and their definitions are:
!>  - @b INFORMATION: this is an exception just for reporting information. This
!>    may be useful if additional information about another exception needs to
!>    be reported. If the log file is active then information exceptions are
!>    not printed to standard error.
!>  - @b WARNING: this exception is for providing a warning to the user.
!>    Warnings cannot stop execution. They can be reported to the log file or
!>    standard error.
!>  - @b ERROR: this exception is for reporting errors. If @c stopOnError is
!>    false then execution will continue, otherwise it will be halted.
!>  - @b FATAL @b ERROR: this exception is for reporting fatal errors. It is
!>    assumed that the program cannot recover from a fatal error so this
!>    exception will always halt program execution.
!>  - @b FAILURE: this exception is for reporting a failure. It's exact purpose
!>    is not yet defined, so its more of a placeholder at present. One
!>    possibility might be for unit testing to report a test condition failure.
!>
!> The exception handler object default is to report exceptions to standard
!> error and stop execution when an error is reported. No default log file
!> is assumed, so a log file is not active by default also. This module is
!> tested using @c testExceptionHandler.f90. An example of how to use this
!> object is given below, the unit test also shows how to use the object.
!> Code coverage documentation can be found on the @ref CodeCoverageReports
!> page.
!>
!> @note
!>  - The exception handler type defined here is extendable. Some extended
!>    types might include one for operating in MPI, one for OpenMP, or adding
!>    other exception types.
!>  - This module uses some design features and naming conventions of the
!>    open source FLIBS module m_exception.f90 written by Michael Baudin.
!>
!> @par Module Dependencies
!>  - @ref ISO_FORTRAN_ENV - Intrinsic Fortran 2003 module
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>
!> @par EXAMPLES
!> @code
!> PROGRAM ExampleExceptions
!>   USE ExceptionHandler
!>   ! ... other modules ...
!>   IMPLICIT NONE
!>
!>   ! ... other variables ...
!>   TYPE(ExceptionHandlerType) :: e
!>
!>   !Set up a log file to report exceptions too.
!>   CALL e%setLogFileUnit(23)
!>   OPEN(UNIT=e%getLogFileUnit(),FILE='Exception.log', &
!>        ACCESS='SEQUENTIAL',FORM='FORMATTED')
!>   CALL e%setLogFileActive(.TRUE.)
!>
!>   !Suppress reporting of exceptions to standard error
!>   CALL e%setQuietMode(.TRUE.)
!>
!>   !Some code that doesn't execute exactly correctly
!>   CALL e%raiseWarning('There was a problem, but things should work out!')
!>
!>   CALL localsubroutine
!>
!>   CALL e%raiseFatalError('Code execution always stops after a fatal error!')
!>
!>   CONTAINS
!>     SUBROUTINE localsubroutine
!>        !This exception handler is separate from the one defined previously
!>        TYPE(ExceptionHandlerType) :: etmp
!>        INTEGER,DIMENSION(EXCEPTION_SIZE) :: counter
!>        INTEGER :: nerr
!>
!>        counter=e%getCounterAll()
!>        WRITE(*,*) 'e counter = ',counter
!>
!>        !Reset the exception counters in e
!>        CALL e%initCounter()
!>        counter=e%getCounterAll()
!>        WRITE(*,*) 'e counter = ',counter
!>
!>        !code will continue even when an error is raised
!>        !note etmp is not quiet and message is printed to prompt
!>        CALL etmp%setStopOnError(.FALSE.)
!>        CALL etmp%raiseError('Oops! There was an error!')
!>
!>        nerr=etmp%getCounter(EXCEPTION_ERROR)
!>        WRITE(*,*) 'etmp nerrors = ',nerr
!>     END SUBROUTINE
!>   !
!> END PROGRAM
!> @endcode
!>
!> @author Brendan Kochunas
!>   @date 06/20/2011
!>
!> @par Revisions:
!> (08/02/2011) - Brendan Kochunas
!>   - Changed getCounter to getCounterAll.
!>   - Added a new getCounter where the index of the exception type is passed
!>     as input.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ExceptionHandler
!
  USE ISO_FORTRAN_ENV
  USE IntrType

  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: EXCEPTION_OK
  PUBLIC :: EXCEPTION_INFORMATION
  PUBLIC :: EXCEPTION_WARNING
  PUBLIC :: EXCEPTION_DEBUG_WARNING
  PUBLIC :: EXCEPTION_ERROR
  PUBLIC :: EXCEPTION_FATAL_ERROR
  PUBLIC :: EXCEPTION_FAILURE
  PUBLIC :: EXCEPTION_SIZE
  PUBLIC :: EXCEPTION_MAX_MESG_LENGTH
  PUBLIC :: ExceptionHandlerType
  PUBLIC :: eDefault

  !> An enumeration for defining an OK condition
  INTEGER(SIK),PARAMETER :: EXCEPTION_OK=0
  !> An enumeration for defining an INFORMATION exception
  INTEGER(SIK),PARAMETER :: EXCEPTION_INFORMATION=1
  !> An enumeration for defining an WARNING exception
  INTEGER(SIK),PARAMETER :: EXCEPTION_WARNING=2
  !> An enumeration for defining an ERROR exception
  INTEGER(SIK),PARAMETER :: EXCEPTION_ERROR=3
  !> An enumeration for defining an FATAL ERROR exception
  INTEGER(SIK),PARAMETER :: EXCEPTION_FATAL_ERROR=4
  !> An enumeration for defining an FAILURE exception
  INTEGER(SIK),PARAMETER :: EXCEPTION_FAILURE=5
  !> An enumeration for defining a DEBUG WARNING exception
  INTEGER(SIK),PARAMETER :: EXCEPTION_DEBUG_WARNING=6
  !> The number of exception types
  INTEGER(SIK),PARAMETER :: EXCEPTION_SIZE=5
  !> The maximum size of an exception message
  INTEGER(SIK),PARAMETER :: EXCEPTION_MAX_MESG_LENGTH=512

  !> @brief Definition for a basic Exception Handler object.
  !>
  !> All the attributes are private. The type bound procedures (methods)
  !> provide interfaces to all the attributes.
  TYPE :: ExceptionHandlerType
    !> Defines whether or not to stop executaion when an error is raised
    LOGICAL(SBK),PRIVATE :: stopOnError=.TRUE.
    !> Defines whether or not to report exceptions to a log file
    LOGICAL(SBK),PRIVATE :: logFileActive=.FALSE.
    !> Defines whether or not to report exceptions to standard error
    LOGICAL(SBK),PRIVATE :: quiet=.FALSE.
    !> Defines whether or not to debug warnings are enabled
    LOGICAL(SBK),PRIVATE :: debug=.TRUE.
    !> The output unit identifier for the log file
    INTEGER(SIK),PRIVATE :: logFileUnit=0_SIK
    !> The number of INFORMATION exceptions that have been raised
    INTEGER(SIK),PRIVATE :: nInfo=0
    !> The number of WARNING exceptions that have been raised
    INTEGER(SIK),PRIVATE :: nWarn=0
    !> The number of ERROR exceptions that have been raised
    INTEGER(SIK),PRIVATE :: nError=0
    !> The number of FATAL ERROR exceptions that have been raised
    INTEGER(SIK),PRIVATE :: nFatal=0
    !> The number of FAILURE exceptions that have been raised
    INTEGER(SIK),PRIVATE :: nFail=0
    !> The last exception message that was reported
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH),PRIVATE :: lastMesg=''
    !> Surrogate exception handler to which most functions are delegated.
    TYPE(ExceptionHandlerType),POINTER :: surrogate => NULL()
!
!List of type bound procedures (methods) for the Exception Handler object
    CONTAINS
      !> @copybrief ExceptionHandler::initCounter
      !> @copydetails ExceptionHandler::initCounter
      PROCEDURE,PASS :: initCounter
      !> @copybrief ExceptionHandler::getCounterAll
      !> @copydetails ExceptionHandler::getCounterAll
      PROCEDURE,PASS :: getCounterAll
      !> @copybrief ExceptionHandler::getCounter
      !> @copydetails ExceptionHandler::getCounter
      PROCEDURE,PASS :: getCounter
      !> @copybrief ExceptionHandler::getLastMessage
      !> @copydetails ExceptionHandler::getLastMessage
      PROCEDURE,PASS :: getLastMessage
      !> @copybrief ExceptionHandler::setQuietMode
      !> @copydetails ExceptionHandler::setQuietMode
      PROCEDURE,PASS :: setQuietMode
      !> @copybrief ExceptionHandler::isQuietMode
      !> @copydetails ExceptionHandler::isQuietMode
      PROCEDURE,PASS :: isQuietMode
      !> @copybrief ExceptionHandler::setDebugMode
      !> @copydetails ExceptionHandler::setDebugMode
      PROCEDURE,PASS :: setDebugMode
      !> @copybrief ExceptionHandler::isDebugMode
      !> @copydetails ExceptionHandler::isDebugMode
      PROCEDURE,PASS :: isDebugMode
      !> @copybrief ExceptionHandler::setLogFileUnit
      !> @copydetails ExceptionHandler::setLogFileUnit
      PROCEDURE,PASS :: setLogFileUnit
      !> @copybrief ExceptionHandler::getLogFileUnit
      !> @copydetails ExceptionHandler::getLogFileUnit
      PROCEDURE,PASS :: getLogFileUnit
      !> @copybrief ExceptionHandler::setLogActive
      !> @copydetails ExceptionHandler::setLogActive
      PROCEDURE,PASS :: setLogActive
      !> @copybrief ExceptionHandler::isLogActive
      !> @copydetails ExceptionHandler::isLogActive
      PROCEDURE,PASS :: isLogActive
      !> @copybrief ExceptionHandler::checkLogFileOK
      !> @copydetails ExceptionHandler::checkLogFileOK
      PROCEDURE,PASS :: checkLogFileOK
      !> @copybrief ExceptionHandler::setStopOnError
      !> @copydetails ExceptionHandler::setStopOnError
      PROCEDURE,PASS :: setStopOnError
      !> @copybrief ExceptionHandler::isStopOnError
      !> @copydetails ExceptionHandler::isStopOnError
      PROCEDURE,PASS :: isStopOnError
      !> @copybrief ExceptionHandler::raiseInformation
      !> @copydetails ExceptionHandler::raiseInformation
      PROCEDURE,PASS :: raiseInformation
      !> @copybrief ExceptionHandler::raiseWarning
      !> @copydetails ExceptionHandler::raiseWarning
      PROCEDURE,PASS :: raiseWarning
      !> @copybrief ExceptionHandler::raiseDebugWarning
      !> @copydetails ExceptionHandler::raiseDebugWarning
      PROCEDURE,PASS :: raiseDebugWarning
      !> @copybrief ExceptionHandler::raiseError
      !> @copydetails ExceptionHandler::raiseError
      PROCEDURE,PASS :: raiseError
      !> @copybrief ExceptionHandler::raiseFatalError
      !> @copydetails ExceptionHandler::raiseFatalError
      PROCEDURE,PASS :: raiseFatalError
      !> @copybrief ExceptionHandler::raiseFailure
      !> @copydetails ExceptionHandler::raiseFailure
      PROCEDURE,PASS :: raiseFailure
  ENDTYPE ExceptionHandlerType

  !> @brief Default target for other exception handlers
  !>
  !> Singleton object. May or may not be used.
  TYPE(ExceptionHandlerType),TARGET,SAVE :: eDefault
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initialize the exception counters for an exception object.
!> @param e the exception object
    SUBROUTINE initCounter(e)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e

      IF(ASSOCIATED(e%surrogate)) THEN
        CALL e%surrogate%initCounter()
      ELSE
        e%nInfo=0
        e%nWarn=0
        e%nError=0
        e%nFatal=0
        e%nFail=0
        e%lastMesg=''
      ENDIF

    ENDSUBROUTINE initCounter
!
!-------------------------------------------------------------------------------
!> @brief Get the counters for the exception object.
!> @param e the exception object
!> @returns counter array with counter values
    PURE FUNCTION getCounterAll(e) RESULT(counter)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      INTEGER(SIK) :: counter(EXCEPTION_SIZE)

      IF(ASSOCIATED(e%surrogate)) THEN
        counter=e%surrogate%getCounterAll()
      ELSE
        counter(EXCEPTION_INFORMATION)=e%nInfo
        counter(EXCEPTION_WARNING)=e%nWarn
        counter(EXCEPTION_ERROR)=e%nError
        counter(EXCEPTION_FATAL_ERROR)=e%nFatal
        counter(EXCEPTION_FAILURE)=e%nFail
      ENDIF
    ENDFUNCTION getCounterAll
!
!-------------------------------------------------------------------------------
!> @brief Get a count of one exception type for the exception object.
!> @param e the exception object
!> @param i the index of the exception type
!> @returns count the number of a certain exception type
    PURE FUNCTION getCounter(e,i) RESULT(count)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK) :: count
      IF(ASSOCIATED(e%surrogate)) THEN
        count=e%surrogate%getCounter(i)
      ELSE
        count=-1
        SELECTCASE(i)
          CASE(EXCEPTION_INFORMATION)
            count=e%nInfo
          CASE(EXCEPTION_WARNING)
            count=e%nWarn
          CASE(EXCEPTION_ERROR)
            count=e%nError
          CASE(EXCEPTION_FATAL_ERROR)
            count=e%nFatal
          CASE(EXCEPTION_FAILURE)
            count=e%nFail
        ENDSELECT
      ENDIF
    ENDFUNCTION getCounter
!
!-------------------------------------------------------------------------------
!> @brief Gets the last exception message.
!> @param e the exception object
    PURE FUNCTION getLastMessage(e) RESULT(lastMesg)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: lastMesg
      IF(ASSOCIATED(e%surrogate)) THEN
        lastMesg=e%surrogate%getLastMessage()
      ELSE
        lastMesg=e%lastMesg
      ENDIF
    ENDFUNCTION getLastMessage
!
!-------------------------------------------------------------------------------
!> @brief Set the output unit for the exception log file.
!> @param e the exception object
!>
!> This routine checks for a valid unit number.
    SUBROUTINE setLogFileUnit(e,unit)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      INTEGER(SIK),INTENT(IN) :: unit
      LOGICAL(SBK) :: tmpquiet

      IF(ASSOCIATED(e%surrogate)) THEN
        NULLIFY(e%surrogate)
      ENDIF

      !Try to set the log file unit number. Check that it is a valid
      !value. If not display a warning.
      IF(unit /= OUTPUT_UNIT .AND. unit /= ERROR_UNIT .AND. unit > 0) THEN
        e%logFileUnit=unit
      ELSE
        tmpquiet=e%quiet
        e%quiet=.FALSE.
        CALL e%raiseWarning('Illegal unit number for log file. '// &
                            'Log file unit not set.')
        e%quiet=tmpquiet
      ENDIF
    ENDSUBROUTINE setLogFileUnit
!
!-------------------------------------------------------------------------------
!> @brief Get the unit number for the exception log file.
!> @param e the exception object
    FUNCTION getLogFileUnit(e) RESULT(unit)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      INTEGER(SIK) :: unit

      IF(ASSOCIATED(e%surrogate)) THEN
        unit=e%surrogate%getLogFileUnit()
      ELSE
        unit=e%logFileUnit
      ENDIF
    ENDFUNCTION getLogFileUnit
!
!-------------------------------------------------------------------------------
!> @brief Set the exception log file reporting to true or false.
!> @param e the exception object
!> @param isactive boolean for the log file reporting mode
!>
!> This routine has some error handling so the log file cannot be set to active
!> if the log file unit is not open for writing as a sequential formatted file.
    SUBROUTINE setLogActive(e,isactive)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      LOGICAL(SBK),INTENT(IN) :: isactive

      IF(ASSOCIATED(e%surrogate)) THEN
        NULLIFY(e%surrogate)
      ENDIF

      IF(isactive) THEN
        CALL e%checkLogFileOK()
      ELSE
        e%logFileActive=.FALSE.
      ENDIF
    ENDSUBROUTINE setLogActive
!
!-------------------------------------------------------------------------------
!> @brief Get the exception log file reporting mode.
!> @param e the exception object
!> @returns isactive boolean for the log file reporting mode
    FUNCTION isLogActive(e) RESULT(isactive)
      CLASS(ExceptionHandlerType) :: e
      LOGICAL(SBK) :: isactive

      IF(ASSOCIATED(e%surrogate)) THEN
        isactive=e%surrogate%isLogActive()
      ELSE
        isactive=.FALSE.
        IF(e%logFileActive) CALL e%checkLogFileOK
        isactive=e%logFileActive
      ENDIF

    ENDFUNCTION isLogActive
!
!-------------------------------------------------------------------------------
!> @brief Subroutine checks the status of the log file to see if it is a valid
!> unit
!> @param e the exception object
!>
!> A valid unit for the log file must be an open file that is formatted and has
!> write access and is sequential. For each of these properties that is not met
!> a warning is output to standard error and log file active status is set to
!> false.
    SUBROUTINE checkLogFileOK(e)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      LOGICAL(SBK) :: tmpquiet,isOpen
      INTEGER(SIK) :: nWarnOld
      CHARACTER(LEN=10) :: fpropstr

      IF(ASSOCIATED(e%surrogate)) THEN
        CALL e%surrogate%checkLogFileOK()
      ELSE
        !Since the state of the log file can change (e.g. closed) check it's
        !integrity
        nWarnOld=e%nWarn

        !Test if the file is open
        INQUIRE(UNIT=e%logFileUnit,OPENED=isOpen)
        IF(.NOT.isOpen) THEN
          tmpquiet=e%quiet
          e%quiet=.FALSE.
          CALL e%raiseWarning('Log file is not open! '// &
                              'Log file status is inactive.')
          e%logFileActive=.FALSE.
          e%quiet=tmpquiet
        ENDIF

        !Test is the file is a formatted file
        INQUIRE(UNIT=e%logFileUnit,FORM=fpropstr)
        IF(TRIM(fpropstr) /= 'FORMATTED') THEN
          tmpquiet=e%quiet
          e%quiet=.FALSE.
          CALL e%raiseWarning('Log file is not a formatted file! '// &
                              'Log file status is inactive.')
          e%logFileActive=.FALSE.
          e%quiet=tmpquiet
        ENDIF

        !Test if the file is sequential
        INQUIRE(UNIT=e%logFileUnit,ACCESS=fpropstr)
        IF(TRIM(fpropstr) /= 'SEQUENTIAL') THEN
          tmpquiet=e%quiet
          e%quiet=.FALSE.
          CALL e%raiseWarning('Log file is not a sequential file! '// &
                              'Log file status is inactive.')
          e%logFileActive=.FALSE.
          e%quiet=tmpquiet
        ENDIF

        !Test if the file has been opened for writing
        INQUIRE(UNIT=e%logFileUnit,ACTION=fpropstr)
        IF(.NOT.(TRIM(fpropstr) == 'WRITE' .OR. &
                 TRIM(fpropstr) == 'READWRITE')) THEN
          tmpquiet=e%quiet
          e%quiet=.FALSE.
          CALL e%raiseWarning('Log file is not open for writing! '// &
                              'Log file status is inactive.')
          e%logFileActive=.FALSE.
          e%quiet=tmpquiet
        ENDIF

        !If none of the checks produced a new warning then the log file check
        !passes the return value can be set to .TRUE. otherwise it is .FALSE.
        IF(nWarnOld == e%nWarn) THEN
          e%logFileActive=.TRUE.
        ELSE
          e%logFileActive=.FALSE.
        ENDIF
      ENDIF
    ENDSUBROUTINE checkLogFileOK
!
!-------------------------------------------------------------------------------
!> @brief Suppress/Unsupress exception reporting to standard error.
!> @param e the exception object
!> @param bool the boolean for quiet mode
    SUBROUTINE setQuietMode(e,bool)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      LOGICAL(SBK),INTENT(IN) :: bool

      IF(ASSOCIATED(e%surrogate)) THEN
        NULLIFY(e%surrogate)
      ENDIF

      e%quiet=bool
    ENDSUBROUTINE setQuietMode
!
!-------------------------------------------------------------------------------
!> @brief Get the status of the quiet mode. Whether or not exception reporting
!> to standard error is suppressed.
!> @param e the exception object
!> @returns bool indicates whether or not exception reporting is suppressed
    PURE FUNCTION isQuietMode(e) RESULT(bool)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      LOGICAL(SBK) :: bool

      IF(ASSOCIATED(e%surrogate)) THEN
        bool=e%surrogate%isQuietMode()
      ELSE
        bool=e%quiet
      ENDIF
    ENDFUNCTION isQuietMode
!
!-------------------------------------------------------------------------------
!> @brief Suppress/Unsupress exception reporting for debug warnings
!> @param e the exception object
!> @param bool the boolean for debug mode
    SUBROUTINE setDebugMode(e,bool)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      LOGICAL(SBK),INTENT(IN) :: bool

      IF(ASSOCIATED(e%surrogate)) THEN
        NULLIFY(e%surrogate)
      ENDIF
      e%debug=bool
    ENDSUBROUTINE setDebugMode
!
!-------------------------------------------------------------------------------
!> @brief Get the status of the debug mode. Whether or not exception reporting
!> of debug warnings are suppressed
!> @param e the exception object
!> @returns bool indicates whether or not debug reporting is suppressed
    PURE FUNCTION isDebugMode(e) RESULT(bool)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      LOGICAL(SBK) :: bool

      IF(ASSOCIATED(e%surrogate)) THEN
        bool=e%surrogate%isDebugMode()
      ELSE
        bool=e%debug
      ENDIF
    ENDFUNCTION isDebugMode
!
!-------------------------------------------------------------------------------
!> @brief Set the exception handler to stop when an error is raised.
!> @param e the exception object
!> @param bool the value for stopping on when an error is raised
    SUBROUTINE setStopOnError(e,bool)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      LOGICAL(SBK),INTENT(IN) :: bool

      IF(ASSOCIATED(e%surrogate)) THEN
        NULLIFY(e%surrogate)
      ENDIF

      e%stopOnError=bool
    ENDSUBROUTINE setStopOnError
!
!-------------------------------------------------------------------------------
!> @brief Get the status for stopping when an error is raised.
!> @param e the exception object
!> @returns bool indicates if the exception object will stop on an error
    PURE FUNCTION isStopOnError(e) RESULT(bool)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      LOGICAL(SBK) :: bool

      IF(ASSOCIATED(e%surrogate)) THEN
        bool=e%surrogate%isStopOnError()
      ELSE
        bool=e%stopOnError
      ENDIF
    ENDFUNCTION isStopOnError
!
!-------------------------------------------------------------------------------
!> @brief Raise an information exception in the exception handler.
!> @param e the exception object
!> @param mesg an informative message about the exception that was raised
!>
!> The routine adds one to the appropriate exception counter, reports the
!> exception message and then potentially stops execution.
    SUBROUTINE raiseInformation(e,mesg)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      CHARACTER(LEN=*),INTENT(IN) :: mesg

      IF(ASSOCIATED(e%surrogate)) THEN
        CALL e%surrogate%raiseInformation(mesg)
      ELSE
        e%nInfo=e%nInfo+1
        e%lastMesg=mesg
        CALL exceptionMessage(EXCEPTION_INFORMATION,e%quiet,e%logFileActive, &
                              e%logFileUnit,e%lastMesg)
      ENDIF
    ENDSUBROUTINE raiseInformation
!
!-------------------------------------------------------------------------------
!> @brief Raise a warning exception in the exception handler.
!> @param e the exception object
!> @param mesg an informative message about the exception that was raised
!>
!> The routine adds one to the appropriate exception counter, reports the
!> exception message and then potentially stops execution.
    SUBROUTINE raiseWarning(e,mesg)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      CHARACTER(LEN=*),INTENT(IN) :: mesg

      IF(ASSOCIATED(e%surrogate)) THEN
        CALL e%surrogate%raiseWarning(mesg)
      ELSE
        e%nWarn=e%nWarn+1
        e%lastMesg=mesg
        CALL exceptionMessage(EXCEPTION_WARNING,e%quiet,e%logFileActive, &
                              e%logFileUnit,e%lastMesg)
      ENDIF
    ENDSUBROUTINE raiseWarning
!
!-------------------------------------------------------------------------------
!> @brief Raise a debug warning exception in the exception handler.
!> @param e the exception object
!> @param mesg an informative message about the exception that was raised
!>
!> The routine throws a warning if the debug output is enabled, otherwise
!> remains silent.
    SUBROUTINE raiseDebugWarning(e,mesg)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      CHARACTER(LEN=*),INTENT(IN) :: mesg

      IF(ASSOCIATED(e%surrogate)) THEN
        CALL e%surrogate%raiseDebugWarning(mesg)
      ELSE
        IF(e%debug) CALL e%raiseWarning(mesg)
      ENDIF

    ENDSUBROUTINE raiseDebugWarning
!
!-------------------------------------------------------------------------------
!> @brief Raise an error exception in the exception handler.
!> @param e the exception object
!> @param mesg an informative message about the exception that was raised
!>
!> The routine adds one to the appropriate exception counter, reports the
!> exception message and then potentially stops execution.
    SUBROUTINE raiseError(e,mesg)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      CHARACTER(LEN=*),INTENT(IN) :: mesg

      IF(ASSOCIATED(e%surrogate)) THEN
        CALL e%surrogate%raiseError(mesg)
      ELSE
        e%nError=e%nError+1
        e%lastMesg=mesg
        CALL exceptionMessage(EXCEPTION_ERROR,e%quiet,e%logFileActive, &
                              e%logFileUnit,e%lastMesg)
        CALL exceptionStop(e%stopOnError)
      ENDIF
    ENDSUBROUTINE raiseError
!
!-------------------------------------------------------------------------------
!> @brief Raise a fatal error exception in the exception handler.
!> @param e the exception object
!> @param mesg an informative message about the exception that was raised
!>
!> The routine adds one to the appropriate exception counter, reports the
!> exception message and then potentially stops execution.
!> @note Fatal errors always halt execution.
    SUBROUTINE raiseFatalError(e,mesg)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      CHARACTER(LEN=*),INTENT(IN) :: mesg

      IF(ASSOCIATED(e%surrogate)) THEN
        CALL e%surrogate%raiseFatalError(mesg)
      ELSE
        e%nFatal=e%nFatal+1
        e%lastMesg=mesg
        CALL exceptionMessage(EXCEPTION_FATAL_ERROR,e%quiet,e%logFileActive, &
                              e%logFileUnit,e%lastMesg)
        CALL exceptionStop(.TRUE.)
      ENDIF
    ENDSUBROUTINE raiseFatalError
!
!-------------------------------------------------------------------------------
!> @brief Raise a failure exception in the exception handler.
!> @param e the exception object
!> @param mesg an informative message about the exception that was raised
!>
!> The routine adds one to the appropriate exception counter, reports the
!> exception message and then potentially stops execution.
    SUBROUTINE raiseFailure(e,mesg)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      CHARACTER(LEN=*),INTENT(IN) :: mesg

      IF(ASSOCIATED(e%surrogate)) THEN
        CALL e%surrogate%raiseFailure(mesg)
      ELSE
        e%nFail=e%nFail+1
        e%lastMesg=mesg
        CALL exceptionMessage(EXCEPTION_FAILURE,e%quiet,e%logFileActive, &
                              e%logFileUnit,e%lastMesg)
        CALL exceptionStop(e%stopOnError)
      ENDIF
    ENDSUBROUTINE raiseFailure
!
!-------------------------------------------------------------------------------
!> @brief A private routine for printing an exception message.
!> @param eCode the exception code
!> @param isQuiet whether or not the message will be output to standard error
!> @param isLogActive whether or not the message will be output to the exception log file
!> @param logUnit the output unit number for the exception log file
!> @param mesg an informative message about the exception
    SUBROUTINE exceptionMessage(eCode,isQuiet,isLogActive,logUnit,mesg)
      INTEGER(SIK),INTENT(IN) :: eCode
      LOGICAL(SBK),INTENT(INOUT) :: isQuiet
      LOGICAL(SBK),INTENT(IN) :: isLogActive
      INTEGER(SIK),INTENT(IN) :: logUnit
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH),INTENT(INOUT) :: mesg
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: prefix
      INTEGER(SIK) :: ioerr1,ioerr2

      !Set the appropriate prefix and printing options
      SELECT CASE(eCode)
        CASE(EXCEPTION_INFORMATION)
          prefix=''
          WRITE(mesg,'(a)')  'EXCEPTION_INFORMATION: '//TRIM(mesg)
          IF(isLogActive) isQuiet=.TRUE.
        CASE(EXCEPTION_WARNING)
          WRITE(prefix,'(a)')  '#### EXCEPTION_WARNING ####'
        CASE(EXCEPTION_ERROR)
          WRITE(prefix,'(a)')  '#### EXCEPTION_ERROR ####'
        CASE(EXCEPTION_FATAL_ERROR)
          WRITE(prefix,'(a)')  '#### EXCEPTION_FATAL_ERROR ####'
          isQuiet=.FALSE.
        CASE(EXCEPTION_FAILURE)
          WRITE(prefix,'(a)')  '#### EXCEPTION_FAILURE ####'
        CASE DEFAULT
          WRITE(prefix,'(a)')  'Illegal value for exception code.'
      ENDSELECT

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
      WRITE(mesg,'(a)') TRIM(prefix)//' - '//TRIM(mesg)
    ENDSUBROUTINE exceptionMessage
!
!-------------------------------------------------------------------------------
!> @brief Stops execution based on value of stopmode
!> @param stopmode boolean to indicate whether or not to stop execution
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
ENDMODULE ExceptionHandler
