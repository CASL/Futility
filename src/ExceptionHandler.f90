!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
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
!>   CALL e%setLogActive(.TRUE.)
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
  USE ISO_FORTRAN_ENV
  USE IntrType

  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: EXCEPTION_OK
  PUBLIC :: EXCEPTION_INFORMATION
  PUBLIC :: EXCEPTION_WARNING
  PUBLIC :: EXCEPTION_DEBUG
  PUBLIC :: EXCEPTION_ERROR
  PUBLIC :: EXCEPTION_FATAL_ERROR
  PUBLIC :: EXCEPTION_SIZE
  PUBLIC :: EXCEPTION_MAX_MESG_LENGTH
  PUBLIC :: ExceptionHandlerType
  PUBLIC :: ASSIGNMENT(=)

  !> An enumeration for defining an OK condition
  INTEGER(SIK),PARAMETER :: EXCEPTION_OK=0
  !> An enumeration for defining an INFORMATION exception
  INTEGER(SIK),PARAMETER :: EXCEPTION_INFORMATION=1
  !> An enumeration for defining an WARNING exception
  INTEGER(SIK),PARAMETER :: EXCEPTION_WARNING=2
  !> An enumeration for defining a DEBUG WARNING exception
  INTEGER(SIK),PARAMETER :: EXCEPTION_DEBUG=3
  !> An enumeration for defining an ERROR exception
  INTEGER(SIK),PARAMETER :: EXCEPTION_ERROR=4
  !> An enumeration for defining an FATAL ERROR exception
  INTEGER(SIK),PARAMETER :: EXCEPTION_FATAL_ERROR=5
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
    !> The output unit identifier for the log file
    INTEGER(SIK),PRIVATE :: logFileUnit=666
    !> The number of INFORMATION exceptions that have been raised
    INTEGER(SIK),PRIVATE :: nInfo=0
    !> The number of WARNING exceptions that have been raised
    INTEGER(SIK),PRIVATE :: nWarn=0
    !> The number of DEBUG exceptions that have been raised
    INTEGER(SIK),PRIVATE :: nDebug=0
    !> The number of ERROR exceptions that have been raised
    INTEGER(SIK),PRIVATE :: nError=0
    !> The number of FATAL ERROR exceptions that have been raised
    INTEGER(SIK),PRIVATE :: nFatal=0
    !> Defines whether or not to report exceptions to standard error
    LOGICAL(SBK),PRIVATE :: quiet(EXCEPTION_SIZE-1)= &
      (/.FALSE.,.FALSE.,.TRUE.,.FALSE./)
    !> Logical array that allows for selective verbosity of exception types
    LOGICAL(SBK),PRIVATE :: verbose(EXCEPTION_SIZE-1)= &
      (/.TRUE.,.TRUE.,.FALSE.,.TRUE./)
    !> The last exception message that was reported
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH),PRIVATE :: lastMesg=''
    !> Surrogate exception handler to which most functions are delegated.
    TYPE(ExceptionHandlerType),POINTER,PRIVATE :: surrogate => NULL()
!
!List of type bound procedures (methods) for the Exception Handler object
    CONTAINS
      !> @copybrief ExceptionHandler::addSurrogate
      !> @copydetails ExceptionHandler::addSurrogate
      PROCEDURE,PASS :: addSurrogate
      !> @copybrief ExceptionHandler::getSurrogate
      !> @copydetails ExceptionHandler::getSurrogate
      PROCEDURE,PASS :: getSurrogate
      !> @copybrief ExceptionHandler::initCounter
      !> @copydetails ExceptionHandler::initCounter
      PROCEDURE,PASS :: initCounter
      !> @copybrief ExceptionHandler::reset
      !> @copydetails ExceptionHandler::reset
      PROCEDURE,PASS :: reset
      !> @copybrief ExceptionHandler::getCounterAll
      !> @copydetails ExceptionHandler::getCounterAll
      PROCEDURE,PASS :: getCounterAll
      !> @copybrief ExceptionHandler::getCounter
      !> @copydetails ExceptionHandler::getCounter
      PROCEDURE,PASS :: getCounter
      !> @copybrief ExceptionHandler::setCounter_all
      !> @copydetails ExceptionHandler::setCounter_all
      PROCEDURE,PASS,PRIVATE :: setCounter_all
      !> @copybrief ExceptionHandler::setCounter_eCode
      !> @copydetails ExceptionHandler::setCounter_eCode
      PROCEDURE,PASS,PRIVATE :: setCounter_eCode
      !>
      GENERIC :: setCounter => setCounter_all,setCounter_eCode
      !> @copybrief ExceptionHandler::getLastMessage
      !> @copydetails ExceptionHandler::getLastMessage
      PROCEDURE,PASS :: getLastMessage
      !> @copybrief ExceptionHandler::setQuietMode_all
      !> @copydetails ExceptionHandler::setQuietMode_all
      PROCEDURE,PASS,PRIVATE :: setQuietMode_all
      !> @copybrief ExceptionHandler::setQuietMode_eCode
      !> @copydetails ExceptionHandler::setQuietMode_eCode
      PROCEDURE,PASS,PRIVATE :: setQuietMode_eCode
      !> @copybrief ExceptionHandler::setQuietMode_array
      !> @copydetails ExceptionHandler::setQuietMode_array
      PROCEDURE,PASS,PRIVATE :: setQuietMode_array
      !>
      GENERIC :: setQuietMode => setQuietMode_all,setQuietMode_eCode, &
                                 setQuietMode_array
      !> @copybrief ExceptionHandler::isQuietMode_all
      !> @copydetails ExceptionHandler::isQuietMode_all
      PROCEDURE,PASS,PRIVATE :: isQuietMode_all
      !> @copybrief ExceptionHandler::isQuietMode_eCode
      !> @copydetails ExceptionHandler::isQuietMode_eCode
      PROCEDURE,PASS,PRIVATE :: isQuietMode_eCode
      !>
      GENERIC :: isQuietMode => isQuietMode_all,isQuietMode_eCode
      !> @copybrief ExceptionHandler::setVerboseMode_all
      !> @copydetails ExceptionHandler::setVerboseMode_all
      PROCEDURE,PASS,PRIVATE :: setVerboseMode_all
      !> @copybrief ExceptionHandler::setVerboseMode_eCode
      !> @copydetails ExceptionHandler::setVerboseMode_eCode
      PROCEDURE,PASS,PRIVATE :: setVerboseMode_eCode
      !> @copybrief ExceptionHandler::setVerboseMode_array
      !> @copydetails ExceptionHandler::setVerboseMode_array
      PROCEDURE,PASS,PRIVATE :: setVerboseMode_array
      !>
      GENERIC :: setVerboseMode => setVerboseMode_all,setVerboseMode_eCode, &
                                 setVerboseMode_array
      !> @copybrief ExceptionHandler::isVerboseMode_all
      !> @copydetails ExceptionHandler::isVerboseMode_all
      PROCEDURE,PASS,PRIVATE :: isVerboseMode_all
      !> @copybrief ExceptionHandler::isVerboseMode_eCode
      !> @copydetails ExceptionHandler::isVerboseMode_eCode
      PROCEDURE,PASS,PRIVATE :: isVerboseMode_eCode
      !>
      GENERIC :: isVerboseMode => isVerboseMode_all,isVerboseMode_eCode
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
      !> @copybrief ExceptionHandler::raiseDebug
      !> @copydetails ExceptionHandler::raiseDebug
      PROCEDURE,PASS :: raiseDebug
      !> @copybrief ExceptionHandler::raiseError
      !> @copydetails ExceptionHandler::raiseError
      PROCEDURE,PASS :: raiseError
      !> @copybrief ExceptionHandler::raiseFatalError
      !> @copydetails ExceptionHandler::raiseFatalError
      PROCEDURE,PASS :: raiseFatalError
  ENDTYPE ExceptionHandlerType

  INTERFACE ASSIGNMENT(=)
    !> @copybrief ExceptionHandler::assign_etype
    !> @copydoc ExceptionHandler::assign_etype
    MODULE PROCEDURE assign_etype
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Overloads assignment operator
!> @param e right hand side of assignment operator
!> @param e2 left hand side of assignment operator
!>
!> Performs a deep copy of all attributes except the surrogate which is
!> pointer associated.
!>
    SUBROUTINE assign_etype(e,e2)
      TYPE(ExceptionHandlerType),INTENT(OUT) :: e
      TYPE(ExceptionHandlerType),INTENT(IN) :: e2
      e%nInfo=e2%nInfo
      e%nWarn=e2%nWarn
      e%nDebug=e2%nDebug
      e%nError=e2%nError
      e%nFatal=e2%nFatal
      e%lastMesg=e2%lastMesg
      e%logFileUnit=e2%logFileUnit
      e%stopOnError=e2%stopOnError
      e%logFileActive=e2%logFileActive
      e%quiet=e2%quiet
      e%verbose=e2%verbose
      e%surrogate => e2%surrogate
    ENDSUBROUTINE assign_etype
!
!-------------------------------------------------------------------------------
!> @brief Routine to add a surrogate handler for a given handler
!> @param e the exception handler
!> @param e2 the surrogate handler
!>
!> This defers all calls of an exception handler to it's surrogate. Association
!> with the surrogate is broken any time a "set" method is called.
!>
    SUBROUTINE addSurrogate(e,e2)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      TYPE(ExceptionHandlerType),TARGET,INTENT(IN) :: e2
      e%surrogate => e2
      IF(ASSOCIATED(e2%surrogate)) e%surrogate => e2%surrogate
    ENDSUBROUTINE addSurrogate
!
!-------------------------------------------------------------------------------
!> @brief Returns a pointer to an exception handler's surrogate
!> @param e the exception handler
!> @param e2 the surrogate handler
!>
    SUBROUTINE getSurrogate(e,e2)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      TYPE(ExceptionHandlerType),POINTER,INTENT(OUT) :: e2
       e2 => e%surrogate
    ENDSUBROUTINE getSurrogate
!
!-------------------------------------------------------------------------------
!> @brief Initialize the exception counters for an exception object.
!> @param e the exception object
!>
    PURE SUBROUTINE initCounter(e)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)
      e%nInfo=0
      e%nWarn=0
      e%nError=0
      e%nFatal=0
      e%nDebug=0
      e%lastMesg=''
    ENDSUBROUTINE initCounter
!
!-------------------------------------------------------------------------------
!> @brief Resets an exception handler to its default state.
!> @param e the exception object
!>
    PURE SUBROUTINE reset(e)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      NULLIFY(e%surrogate)
      e%nInfo=0
      e%nWarn=0
      e%nError=0
      e%nFatal=0
      e%nDebug=0
      e%lastMesg=''
      e%logFileUnit=666
      e%stopOnError=.TRUE.
      e%logFileActive=.FALSE.
      e%quiet=(/.FALSE.,.FALSE.,.TRUE.,.FALSE./)
      e%verbose=(/.TRUE.,.TRUE.,.FALSE.,.TRUE./)
    ENDSUBROUTINE reset
!
!-------------------------------------------------------------------------------
!> @brief Get the counters for the exception object.
!> @param e the exception object
!> @returns counter array with counter values
!>
    PURE FUNCTION getCounterAll(e) RESULT(counter)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      INTEGER(SIK) :: counter(EXCEPTION_SIZE)

      IF(ASSOCIATED(e%surrogate)) THEN
        counter(EXCEPTION_INFORMATION)=e%surrogate%nInfo
        counter(EXCEPTION_WARNING)=e%surrogate%nWarn
        counter(EXCEPTION_ERROR)=e%surrogate%nError
        counter(EXCEPTION_FATAL_ERROR)=e%surrogate%nFatal
        counter(EXCEPTION_DEBUG)=e%surrogate%nDebug
      ELSE
        counter(EXCEPTION_INFORMATION)=e%nInfo
        counter(EXCEPTION_WARNING)=e%nWarn
        counter(EXCEPTION_ERROR)=e%nError
        counter(EXCEPTION_FATAL_ERROR)=e%nFatal
        counter(EXCEPTION_DEBUG)=e%nDebug
      ENDIF
    ENDFUNCTION getCounterAll
!
!-------------------------------------------------------------------------------
!> @brief Get a count of one exception type for the exception object.
!> @param e the exception object
!> @param i the index of the exception type
!> @returns count the number of a certain exception type
!>
    PURE FUNCTION getCounter(e,i) RESULT(count)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK) :: count
      count=-1
      IF(ASSOCIATED(e%surrogate)) THEN
        SELECTCASE(i)
          CASE(EXCEPTION_INFORMATION)
            count=e%surrogate%nInfo
          CASE(EXCEPTION_WARNING)
            count=e%surrogate%nWarn
          CASE(EXCEPTION_DEBUG)
            count=e%surrogate%nDebug
          CASE(EXCEPTION_ERROR)
            count=e%surrogate%nError
          CASE(EXCEPTION_FATAL_ERROR)
            count=e%surrogate%nFatal
        ENDSELECT
      ELSE
        SELECTCASE(i)
          CASE(EXCEPTION_INFORMATION)
            count=e%nInfo
          CASE(EXCEPTION_WARNING)
            count=e%nWarn
          CASE(EXCEPTION_DEBUG)
            count=e%nDebug
          CASE(EXCEPTION_ERROR)
            count=e%nError
          CASE(EXCEPTION_FATAL_ERROR)
            count=e%nFatal
        ENDSELECT
      ENDIF
    ENDFUNCTION getCounter
!
!-------------------------------------------------------------------------------
!> @brief Set the counters for the exception object.
!> @param e the exception object
!> @param counter array with counter values
!>
    SUBROUTINE setCounter_all(e,counter)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      INTEGER(SIK),INTENT(IN) :: counter(EXCEPTION_SIZE)
      INTEGER(SIK) :: lcounter(EXCEPTION_SIZE)

      lcounter=counter
      WHERE(counter < 0) lcounter=0
      IF(ASSOCIATED(e%surrogate)) THEN
        e%surrogate%nInfo=lcounter(EXCEPTION_INFORMATION)
        e%surrogate%nWarn=lcounter(EXCEPTION_WARNING)
        e%surrogate%nError=lcounter(EXCEPTION_ERROR)
        e%surrogate%nFatal=lcounter(EXCEPTION_FATAL_ERROR)
        e%surrogate%nDebug=lcounter(EXCEPTION_DEBUG)
      ELSE
        e%nInfo=lcounter(EXCEPTION_INFORMATION)
        e%nWarn=lcounter(EXCEPTION_WARNING)
        e%nError=lcounter(EXCEPTION_ERROR)
        e%nFatal=lcounter(EXCEPTION_FATAL_ERROR)
        e%nDebug=lcounter(EXCEPTION_DEBUG)
      ENDIF
    ENDSUBROUTINE setCounter_all
!
!-------------------------------------------------------------------------------
!> @brief Set a count of one exception type for the exception object.
!> @param e the exception object
!> @param i the index of the exception type
!> @param count the number of a certain exception type
!>
    SUBROUTINE setCounter_eCode(e,i,count)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: count
      INTEGER(SIK) :: lcount
      lcount=0
      IF(count > 0) lcount=count
      IF(ASSOCIATED(e%surrogate)) THEN
        SELECTCASE(i)
          CASE(EXCEPTION_INFORMATION)
            e%surrogate%nInfo=lcount
          CASE(EXCEPTION_WARNING)
            e%surrogate%nWarn=lcount
          CASE(EXCEPTION_DEBUG)
            e%surrogate%nDebug=lcount
          CASE(EXCEPTION_ERROR)
            e%surrogate%nError=lcount
          CASE(EXCEPTION_FATAL_ERROR)
            e%surrogate%nFatal=lcount
        ENDSELECT
      ELSE
        SELECTCASE(i)
          CASE(EXCEPTION_INFORMATION)
            e%nInfo=lcount
          CASE(EXCEPTION_WARNING)
            e%nWarn=lcount
          CASE(EXCEPTION_DEBUG)
            e%nDebug=lcount
          CASE(EXCEPTION_ERROR)
            e%nError=lcount
          CASE(EXCEPTION_FATAL_ERROR)
            e%nFatal=lcount
        ENDSELECT
      ENDIF
    ENDSUBROUTINE setCounter_eCode
!
!-------------------------------------------------------------------------------
!> @brief Gets the last exception message.
!> @param e the exception object
!>
    PURE FUNCTION getLastMessage(e) RESULT(lastMesg)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: lastMesg
      lastMesg=e%lastMesg
      IF(ASSOCIATED(e%surrogate)) lastMesg=e%surrogate%lastMesg
    ENDFUNCTION getLastMessage
!
!-------------------------------------------------------------------------------
!> @brief Set the output unit for the exception log file.
!> @param e the exception object
!>
!> This routine checks for a valid unit number.
!>
    RECURSIVE SUBROUTINE setLogFileUnit(e,unit)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      INTEGER(SIK),INTENT(IN) :: unit
      LOGICAL(SBK) :: tmpQuiet
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)

      !Try to set the log file unit number. Check that it is a valid
      !value. If not display a warning.
      IF(unit /= OUTPUT_UNIT .AND. unit /= ERROR_UNIT .AND. unit > 0) THEN
        e%logFileUnit=unit
      ELSE
        e%lastMesg='Illegal unit number for log file. '// &
                    'Log file unit not set.'
        tmpQuiet=.FALSE.
        e%nWarn=e%nWarn+1
        CALL exceptionMessage(EXCEPTION_WARNING,tmpQuiet,.FALSE., &
          ERROR_UNIT,e%lastMesg)
      ENDIF
    ENDSUBROUTINE setLogFileUnit
!
!-------------------------------------------------------------------------------
!> @brief Get the unit number for the exception log file.
!> @param e the exception object
!>
    PURE FUNCTION getLogFileUnit(e) RESULT(unit)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      INTEGER(SIK) :: unit
      unit=e%logFileUnit
      IF(ASSOCIATED(e%surrogate)) unit=e%surrogate%logFileUnit
    ENDFUNCTION getLogFileUnit
!
!-------------------------------------------------------------------------------
!> @brief Set the exception log file reporting to true or false.
!> @param e the exception object
!> @param isactive boolean for the log file reporting mode
!>
!> This routine has some error handling so the log file cannot be set to active
!> if the log file unit is not open for writing as a sequential formatted file.
!>
    RECURSIVE SUBROUTINE setLogActive(e,isactive)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      LOGICAL(SBK),INTENT(IN) :: isactive
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)
      IF(isactive) THEN
        CALL e%checkLogFileOK()
        e%logFileActive=.TRUE.
      ELSE
        e%logFileActive=.FALSE.
      ENDIF
    ENDSUBROUTINE setLogActive
!
!-------------------------------------------------------------------------------
!> @brief Get the exception log file reporting mode.
!> @param e the exception object
!> @returns isactive boolean for the log file reporting mode
!>
    FUNCTION isLogActive(e) RESULT(isactive)
      CLASS(ExceptionHandlerType) :: e
      LOGICAL(SBK) :: isactive
      isactive=.FALSE.
      IF(ASSOCIATED(e%surrogate)) THEN
        IF(e%surrogate%logFileActive) CALL e%surrogate%checkLogFileOK()
        isactive=e%surrogate%logFileActive
      ELSE
        IF(e%logFileActive) CALL e%checkLogFileOK()
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
!>
    RECURSIVE SUBROUTINE checkLogFileOK(e)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      LOGICAL(SBK) :: isOpen
      INTEGER(SIK) :: nDebugOld
      CHARACTER(LEN=10) :: fprop

      IF(ASSOCIATED(e%surrogate)) THEN
        CALL checkLogFileOK(e%surrogate)
      ELSE
        !Since the state of the log file can change (e.g. closed) check it's
        !integrity
        nDebugOld=e%nDebug
        e%logFileActive=.FALSE.

        !Test if the file is open
        INQUIRE(UNIT=e%logFileUnit,OPENED=isOpen)
        IF(.NOT.isOpen) CALL raiseDebug(e,'Log file is not open! '// &
          'Log file status is inactive.')

        !Test if the file is a formatted file
        INQUIRE(UNIT=e%logFileUnit,FORM=fprop)
        IF(TRIM(fprop) /= 'FORMATTED') CALL raiseDebug(e, &
          'Log file is not a formatted file! Log file status is inactive.')

        !Test if the file is sequential
        INQUIRE(UNIT=e%logFileUnit,ACCESS=fprop)
        IF(TRIM(fprop) /= 'SEQUENTIAL') CALL raiseDebug(e, &
          'Log file is not a sequential file! Log file status is inactive.')

        !Test if the file has been opened for writing
        INQUIRE(UNIT=e%logFileUnit,ACTION=fprop)
        IF(.NOT.(TRIM(fprop) == 'WRITE' .OR. TRIM(fprop) == 'READWRITE')) &
          CALL raiseDebug(e,'Log file is not open for writing! '// &
            'Log file status is inactive.')

        !If none of the checks produced a new warning then the log file check
        !passes the return value can be set to .TRUE. otherwise it is .FALSE.
        IF(nDebugOld == e%nDebug) e%logFileActive=.TRUE.
      ENDIF
    ENDSUBROUTINE checkLogFileOK
!
!-------------------------------------------------------------------------------
!> @brief Suppress/Unsupress exception reporting to standard error.
!> @param e the exception object
!> @param bool the boolean for quiet mode
!>
    PURE SUBROUTINE setQuietMode_all(e,bool)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      LOGICAL(SBK),INTENT(IN) :: bool
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)
      e%quiet=bool
    ENDSUBROUTINE setQuietMode_all
!
!-------------------------------------------------------------------------------
!> @brief Suppress/Unsupress which exceptions will be reported to standard
!>        error.
!> @param e the exception object
!> @param int the enumeration corresponding to the exception type
!> @param bool the optional logical whether to turn the exception reporting on
!>        or off
!>
    PURE SUBROUTINE setQuietMode_eCode(e,eCode,bool)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      INTEGER(SIK),INTENT(IN) :: eCode
      LOGICAL(SBK),INTENT(IN) :: bool
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)
      IF(EXCEPTION_OK < eCode .AND. eCode < EXCEPTION_SIZE) &
        e%quiet(eCode)=bool
    ENDSUBROUTINE setQuietMode_eCode
!
!-------------------------------------------------------------------------------
!> @brief Suppress/Unsupress which exceptions will be reported to standard
!>        error.
!> @param e the exception object
!> @param int the enumeration corresponding to the exception type
!> @param bool the optional logical whether to turn the exception reporting on
!>        or off
!>
    PURE SUBROUTINE setQuietMode_array(e,bool)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      LOGICAL(SBK),INTENT(IN) :: bool(:)
      INTEGER(SIK) :: n
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)
      n=MIN(EXCEPTION_SIZE-1,SIZE(bool))
      e%quiet(1:n)=bool(1:n)
    ENDSUBROUTINE setQuietMode_array
!
!-------------------------------------------------------------------------------
!> @brief Get the status of the quiet mode. Whether or not exception reporting
!>        to standard error is suppressed.
!> @param e the exception object
!> @returns bool indicates whether or not exception reporting is suppressed
!>
    PURE FUNCTION isQuietMode_all(e) RESULT(bool)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      LOGICAL(SBK) :: bool
      bool=ALL(e%quiet)
      IF(ASSOCIATED(e%surrogate)) bool=ALL(e%surrogate%quiet)
    ENDFUNCTION isQuietMode_all
!
!-------------------------------------------------------------------------------
!> @brief Get the status of the quiet mode. Whether or not exception reporting
!>        to standard error is suppressed.
!> @param e the exception object
!> @returns bool indicates whether or not exception reporting is suppressed
!>
    PURE FUNCTION isQuietMode_eCode(e,eCode) RESULT(bool)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      INTEGER(SIK),INTENT(IN) :: eCode
      LOGICAL(SBK) :: bool
      bool=.FALSE.
      IF((EXCEPTION_OK < eCode) .AND. (eCode <= EXCEPTION_SIZE-1)) THEN
        bool=e%quiet(eCode)
        IF(ASSOCIATED(e%surrogate)) bool=e%surrogate%quiet(eCode)
      ENDIF
    ENDFUNCTION isQuietMode_eCode
!
!-------------------------------------------------------------------------------
!> @brief Suppress/Unsupress exception reporting to the log file.
!> @param e the exception object
!> @param bool the boolean for quiet mode
!>
    PURE SUBROUTINE setVerboseMode_all(e,bool)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      LOGICAL(SBK),INTENT(IN) :: bool
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)
      e%verbose=bool
    ENDSUBROUTINE setVerboseMode_all
!
!-------------------------------------------------------------------------------
!> @brief Suppress/Unsupress which exceptions will be reported to the log file.
!> @param e the exception object
!> @param int the enumeration corresponding to the exception type
!> @param bool the optional logical whether to turn the exception reporting on
!>        or off
!>
    PURE SUBROUTINE setVerboseMode_eCode(e,eCode,bool)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      INTEGER(SIK),INTENT(IN) :: eCode
      LOGICAL(SBK),INTENT(IN) :: bool
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)
      IF(EXCEPTION_OK < eCode .AND. eCode < EXCEPTION_SIZE) &
        e%verbose(eCode)=bool
    ENDSUBROUTINE setVerboseMode_eCode
!
!-------------------------------------------------------------------------------
!> @brief Suppress/Unsupress which exceptions will be reported to the log file.
!> @param e the exception object
!> @param int the enumeration corresponding to the exception type
!> @param bool the optional logical whether to turn the exception reporting on
!>        or off
!>
    PURE SUBROUTINE setVerboseMode_array(e,bool)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      LOGICAL(SBK),INTENT(IN) :: bool(:)
      INTEGER(SIK) :: n
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)
      n=MIN(EXCEPTION_SIZE-1,SIZE(bool))
      e%verbose(1:n)=bool(1:n)
    ENDSUBROUTINE setVerboseMode_array
!
!-------------------------------------------------------------------------------
!> @brief Get the status of the quiet mode. Whether or not exception reporting
!>        to the log file is suppressed.
!> @param e the exception object
!> @returns bool indicates whether or not exception reporting is suppressed
!>
    PURE FUNCTION isVerboseMode_all(e) RESULT(bool)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      LOGICAL(SBK) :: bool
      bool=ALL(e%verbose)
      IF(ASSOCIATED(e%surrogate)) bool=ALL(e%surrogate%verbose)
    ENDFUNCTION isVerboseMode_all
!
!-------------------------------------------------------------------------------
!> @brief Get the status of the quiet mode. Whether or not exception reporting
!>        to the log file is suppressed.
!> @param e the exception object
!> @returns bool indicates whether or not exception reporting is suppressed
!>
    PURE FUNCTION isVerboseMode_eCode(e,eCode) RESULT(bool)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      INTEGER(SIK),INTENT(IN) :: eCode
      LOGICAL(SBK) :: bool
      bool=.FALSE.
      IF((EXCEPTION_OK < eCode) .AND. (eCode <= EXCEPTION_SIZE-1)) THEN
        bool=e%verbose(eCode)
        IF(ASSOCIATED(e%surrogate)) bool=e%surrogate%verbose(eCode)
      ENDIF
    ENDFUNCTION isVerboseMode_eCode
!
!-------------------------------------------------------------------------------
!> @brief Set the exception handler to stop when an error is raised.
!> @param e the exception object
!> @param bool the value for stopping on when an error is raised
!>
    PURE SUBROUTINE setStopOnError(e,bool)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      LOGICAL(SBK),INTENT(IN) :: bool
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)
      e%stopOnError=bool
    ENDSUBROUTINE setStopOnError
!
!-------------------------------------------------------------------------------
!> @brief Get the status for stopping when an error is raised.
!> @param e the exception object
!> @returns bool indicates if the exception object will stop on an error
!>
    PURE FUNCTION isStopOnError(e) RESULT(bool)
      CLASS(ExceptionHandlerType),INTENT(IN) :: e
      LOGICAL(SBK) :: bool
      bool=e%stopOnError
      IF(ASSOCIATED(e%surrogate)) bool=e%surrogate%stopOnError
    ENDFUNCTION isStopOnError
!
!-------------------------------------------------------------------------------
!> @brief Raise an information exception in the exception handler.
!> @param e the exception object
!> @param mesg an informative message about the exception that was raised
!>
!> The routine adds one to the appropriate exception counter, reports the
!> exception message and then potentially stops execution.
!>
    SUBROUTINE raiseInformation(e,mesg)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      CHARACTER(LEN=*),INTENT(IN) :: mesg
      LOGICAL(SBK) :: toLog
      IF(ASSOCIATED(e%surrogate)) THEN
        e%surrogate%nInfo=e%surrogate%nInfo+1
        e%surrogate%lastMesg=mesg
        toLog=(e%surrogate%logFileActive .AND. &
          e%surrogate%verbose(EXCEPTION_INFORMATION))
        CALL exceptionMessage(EXCEPTION_INFORMATION, &
          e%surrogate%quiet(EXCEPTION_INFORMATION),toLog, &
          e%surrogate%logFileUnit,e%surrogate%lastMesg)
      ELSE
        e%nInfo=e%nInfo+1
        e%lastMesg=mesg
        toLog=(e%logFileActive .AND. e%verbose(EXCEPTION_INFORMATION))
        CALL exceptionMessage(EXCEPTION_INFORMATION, &
          e%quiet(EXCEPTION_INFORMATION),toLog,e%logFileUnit,e%lastMesg)
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
!>
    SUBROUTINE raiseWarning(e,mesg)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      CHARACTER(LEN=*),INTENT(IN) :: mesg
      LOGICAL(SBK) :: toLog
      IF(ASSOCIATED(e%surrogate)) THEN
        e%surrogate%nWarn=e%surrogate%nWarn+1
        e%surrogate%lastMesg=mesg
        toLog=(e%surrogate%logFileActive .AND. &
          e%surrogate%verbose(EXCEPTION_WARNING))
        CALL exceptionMessage(EXCEPTION_WARNING, &
          e%surrogate%quiet(EXCEPTION_WARNING),toLog, &
          e%surrogate%logFileUnit,e%surrogate%lastMesg)
      ELSE
        e%nWarn=e%nWarn+1
        e%lastMesg=mesg
        toLog=(e%logFileActive .AND. e%verbose(EXCEPTION_WARNING))
        CALL exceptionMessage(EXCEPTION_WARNING,e%quiet(EXCEPTION_WARNING), &
          toLog,e%logFileUnit,e%lastMesg)
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
!>
    SUBROUTINE raiseDebug(e,mesg)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      CHARACTER(LEN=*),INTENT(IN) :: mesg
      LOGICAL(SBK) :: toLog
      IF(ASSOCIATED(e%surrogate)) THEN
        e%surrogate%nDebug=e%surrogate%nDebug+1
        e%surrogate%lastMesg=mesg
        toLog=(e%surrogate%logFileActive .AND. &
          e%surrogate%verbose(EXCEPTION_DEBUG))
        CALL exceptionMessage(EXCEPTION_DEBUG, &
          e%surrogate%quiet(EXCEPTION_DEBUG),toLog, &
          e%surrogate%logFileUnit,e%surrogate%lastMesg)
      ELSE
        e%nDebug=e%nDebug+1
        e%lastMesg=mesg
        toLog=(e%logFileActive .AND. e%verbose(EXCEPTION_DEBUG))
        CALL exceptionMessage(EXCEPTION_DEBUG,e%quiet(EXCEPTION_DEBUG),toLog, &
          e%logFileUnit,e%lastMesg)
      ENDIF
    ENDSUBROUTINE raiseDebug
!
!-------------------------------------------------------------------------------
!> @brief Raise an error exception in the exception handler.
!> @param e the exception object
!> @param mesg an informative message about the exception that was raised
!>
!> The routine adds one to the appropriate exception counter, reports the
!> exception message and then potentially stops execution.
!>
    SUBROUTINE raiseError(e,mesg)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      CHARACTER(LEN=*),INTENT(IN) :: mesg
      LOGICAL(SBK) :: toLog
      IF(ASSOCIATED(e%surrogate)) THEN
        e%surrogate%nError=e%surrogate%nError+1
        e%surrogate%lastMesg=mesg
        toLog=(e%surrogate%logFileActive .AND. &
          e%surrogate%verbose(EXCEPTION_ERROR))
        CALL exceptionMessage(EXCEPTION_ERROR, &
          e%surrogate%quiet(EXCEPTION_ERROR),toLog, &
          e%surrogate%logFileUnit,e%surrogate%lastMesg)
        CALL exceptionStop(e%surrogate%stopOnError)
      ELSE
        e%nError=e%nError+1
        e%lastMesg=mesg
        toLog=(e%logFileActive .AND. e%verbose(EXCEPTION_ERROR))
        CALL exceptionMessage(EXCEPTION_ERROR,e%quiet(EXCEPTION_ERROR),toLog, &
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
!>
    SUBROUTINE raiseFatalError(e,mesg)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      CHARACTER(LEN=*),INTENT(IN) :: mesg
      LOGICAL(SBK) :: tmpQuiet
      tmpQuiet=.FALSE.
      IF(ASSOCIATED(e%surrogate)) THEN
        e%surrogate%nFatal=e%surrogate%nFatal+1
        e%surrogate%lastMesg=mesg
        CALL exceptionMessage(EXCEPTION_FATAL_ERROR,tmpQuiet, &
          e%surrogate%logFileActive,e%surrogate%logFileUnit, &
          e%surrogate%lastMesg)
      ELSE
        e%nFatal=e%nFatal+1
        e%lastMesg=mesg
        CALL exceptionMessage(EXCEPTION_FATAL_ERROR,tmpQuiet,e%logFileActive, &
          e%logFileUnit,e%lastMesg)
      ENDIF
      CALL exceptionStop(.TRUE.)
    ENDSUBROUTINE raiseFatalError
!
!-------------------------------------------------------------------------------
!> @brief A private routine for printing an exception message.
!> @param eCode the exception code
!> @param isQuiet whether or not the message will be output to standard error
!> @param isLogActive whether or not the message will be output to the exception log file
!> @param logUnit the output unit number for the exception log file
!> @param mesg an informative message about the exception
!>
    SUBROUTINE exceptionMessage(eCode,isQuiet,isLogActive,logUnit,mesg)
      INTEGER(SIK),INTENT(IN) :: eCode
      LOGICAL(SBK),INTENT(INOUT) :: isQuiet
      LOGICAL(SBK),INTENT(IN) :: isLogActive
      INTEGER(SIK),INTENT(IN) :: logUnit
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH),INTENT(INOUT) :: mesg
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: prefix
      INTEGER(SIK) :: ioerr1,ioerr2,prefixLen

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
        CASE(EXCEPTION_DEBUG)
          WRITE(prefix,'(a)')  '#### EXCEPTION_DEBUG_MESG ####'
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
      prefixLen = LEN_TRIM(prefix)+3
      WRITE(mesg,'(a)') TRIM(prefix)//' - '//TRIM(mesg(1:EXCEPTION_MAX_MESG_LENGTH-prefixLen))
    ENDSUBROUTINE exceptionMessage
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
!> @brief Copies the attributes from the surrogate to the passed exception
!>        handler and nullifies the surrogate.
!> @param stopmode boolean to indicate whether or not to stop execution
!>
    PURE SUBROUTINE copyFromSurrogate(e)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      TYPE(ExceptionHandlerType),POINTER :: tmpE
      tmpE => e%surrogate
      NULLIFY(e%surrogate)
      e%stopOnError=tmpE%stopOnError
      e%logFileActive=tmpE%logFileActive
      e%quiet=tmpE%quiet
      e%logFileUnit=tmpE%logFileUnit
      e%nInfo=tmpE%nInfo
      e%nWarn=tmpE%nWarn
      e%nError=tmpE%nError
      e%nFatal=tmpE%nFatal
      e%nDebug=tmpE%nDebug
      e%verbose=tmpE%verbose
      e%lastMesg=tmpE%lastMesg
      NULLIFY(tmpE)
    ENDSUBROUTINE copyFromSurrogate
!
ENDMODULE ExceptionHandler
