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
#include "Futility_DBC.h"
  USE Futility_DBC
  USE ISO_FORTRAN_ENV
  USE IntrType
  USE ExceptionTypes

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

  !> @brief Contains pointer to an ExceptionType object
  TYPE,PUBLIC :: ExceptionContainer
    CLASS(ExceptionTypeBase),POINTER :: expobj
  ENDTYPE ExceptionContainer

  !> @brief Definition for a basic Exception Handler object.
  !>
  !> All the attributes are private. The type bound procedures (methods)
  !> provide interfaces to all the attributes.
  TYPE :: ExceptionHandlerType
    !> True if ExceptionHandler is initilized
    LOGICAL(SBK),PUBLIC :: isInit=.FALSE.
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
    !> Exception registry
    TYPE(ExceptionContainer),ALLOCATABLE,PRIVATE :: exceptionRegistry(:)
!
!List of type bound procedures (methods) for the Exception Handler object
    CONTAINS
      !> @copybrief ExceptionHandler::init_ExceptionHandlerType
      !> @copydoc ExceptionHandler::init_ExceptionHandlerType
      PROCEDURE,PASS :: init => init_ExceptionHandlerType
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
      !>
      !> @copybrief ExceptionHandler::raiseRuntimeError
      !> @copydetails ExceptionHandler::raiseRuntimeError
      PROCEDURE,PASS :: raiseRuntimeError
      !> @copybrief ExceptionHandler::registerException
      !> @copydetails ExceptionHandler::registerException
      PROCEDURE,PASS :: registerException
      !> @copybrief ExceptionHandler::getTagList
      !> @copydetails ExceptionHandler::getTagList
      PROCEDURE,PASS :: getTagList
      !> @copybrief ExceptionHandler::getCountByTag
      !> @copydetails ExceptionHandler::getCountByTag
      PROCEDURE,PASS :: getCountByTag
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
!> @brief ExceptionHandlerType custom initilization.
!>
!> Creates a default exception registry.
!>
  SUBROUTINE init_ExceptionHandlerType(e)
    CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
    TYPE(ExceptionTypeFatal),TARGET,SAVE :: myFatal
    TYPE(ExceptionTypeDebug),TARGET,SAVE :: myDebug
    TYPE(ExceptionTypeInformation),TARGET,SAVE :: myInformation
    TYPE(ExceptionTypeError),TARGET,SAVE :: myError
    TYPE(ExceptionTypeWarning),TARGET,SAVE :: myWarning

    REQUIRE(.NOT. e%isInit)
    REQUIRE(.NOT. ALLOCATED(e%exceptionRegistry))

    CALL myInformation%init(EXCEPTION_INFORMATION)
    CALL myWarning%init(EXCEPTION_WARNING)
    CALL myDebug%init(EXCEPTION_DEBUG)
    CALL myError%init(EXCEPTION_ERROR)
    CALL myFatal%init(EXCEPTION_FATAL_ERROR)
    CALL myFatal%setStopMode(.TRUE.)

    ! create the default registry
    ALLOCATE(e%exceptionRegistry(EXCEPTION_SIZE))
    e%exceptionRegistry(EXCEPTION_ERROR)%expobj &
      => myError
    e%exceptionRegistry(EXCEPTION_WARNING)%expobj &
      => myWarning
    e%exceptionRegistry(EXCEPTION_DEBUG)%expobj &
      => myDebug
    e%exceptionRegistry(EXCEPTION_FATAL_ERROR)%expobj &
      => myFatal
    e%exceptionRegistry(EXCEPTION_INFORMATION)%expobj &
      => myInformation

    CALL e%reset()
    e%isInit = .TRUE.

  ENDSUBROUTINE init_ExceptionHandlerType
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
      e%isInit=e2%isInit
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
      IF(.NOT. ALLOCATED(e%exceptionRegistry) &
         .AND. ALLOCATED(e2%exceptionRegistry)) THEN
        ALLOCATE(e%exceptionRegistry(SIZE(e2%exceptionRegistry)))
        e%exceptionRegistry=e2%exceptionRegistry
      ENDIF
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
      INTEGER(SIK) :: i
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)
      IF(ALLOCATED(e%exceptionRegistry)) THEN
        DO i=1,SIZE(e%exceptionRegistry)
          CALL e%exceptionRegistry(i)%expobj%resetCounter()
        ENDDO
      ENDIF
      e%lastMesg=''
    ENDSUBROUTINE initCounter
!
!-------------------------------------------------------------------------------
!> @brief Resets an exception handler to its default state.
!> @param e the exception object
!>
    PURE SUBROUTINE reset(e)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      INTEGER(SIK) :: i
      NULLIFY(e%surrogate)
      e%lastMesg=''
      e%logFileUnit=666
      e%stopOnError=.TRUE.
      e%logFileActive=.FALSE.
      e%quiet=(/.FALSE.,.FALSE.,.TRUE.,.FALSE./)
      e%verbose=(/.TRUE.,.TRUE.,.FALSE.,.TRUE./)
      IF(ALLOCATED(e%exceptionRegistry)) THEN
        DO i=1,SIZE(e%exceptionRegistry)-1
          CALL e%exceptionRegistry(i)%expobj%resetCounter()
          CALL e%exceptionRegistry(i)%expobj%setQuietMode(e%quiet(i))
          CALL e%exceptionRegistry(i)%expobj%setVerboseMode(e%verbose(i))
        ENDDO
      ENDIF

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
        counter(EXCEPTION_INFORMATION)=&
          e%surrogate%exceptionRegistry(EXCEPTION_INFORMATION)%expobj%getCounter()
        counter(EXCEPTION_WARNING)=&
          e%surrogate%exceptionRegistry(EXCEPTION_WARNING)%expobj%getCounter()
        counter(EXCEPTION_ERROR)=&
          e%surrogate%exceptionRegistry(EXCEPTION_ERROR)%expobj%getCounter()
        counter(EXCEPTION_FATAL_ERROR)=&
          e%surrogate%exceptionRegistry(EXCEPTION_FATAL_ERROR)%expobj%getCounter()
        counter(EXCEPTION_DEBUG)=&
          e%surrogate%exceptionRegistry(EXCEPTION_DEBUG)%expobj%getCounter()
      ELSE
        counter(EXCEPTION_INFORMATION)=&
          e%exceptionRegistry(EXCEPTION_INFORMATION)%expobj%getCounter()
        counter(EXCEPTION_WARNING)=&
          e%exceptionRegistry(EXCEPTION_WARNING)%expobj%getCounter()
        counter(EXCEPTION_ERROR)=&
          e%exceptionRegistry(EXCEPTION_ERROR)%expobj%getCounter()
        counter(EXCEPTION_FATAL_ERROR)=&
          e%exceptionRegistry(EXCEPTION_FATAL_ERROR)%expobj%getCounter()
        counter(EXCEPTION_DEBUG)=&
          e%exceptionRegistry(EXCEPTION_DEBUG)%expobj%getCounter()
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
      IF(i > EXCEPTION_SIZE) RETURN
      IF(i < 1) RETURN
      IF(ASSOCIATED(e%surrogate)) THEN
        count=e%surrogate%exceptionRegistry(i)%expobj%getCounter()
      ELSE
        count=e%exceptionRegistry(i)%expobj%getCounter()
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
        CALL e%surrogate%exceptionRegistry(EXCEPTION_INFORMATION)%expobj%setCounter( &
          lcounter(EXCEPTION_INFORMATION))
        CALL e%surrogate%exceptionRegistry(EXCEPTION_WARNING)%expobj%setCounter( &
          lcounter(EXCEPTION_WARNING))
        CALL e%surrogate%exceptionRegistry(EXCEPTION_ERROR)%expobj%setCounter( &
          lcounter(EXCEPTION_ERROR))
        CALL e%surrogate%exceptionRegistry(EXCEPTION_FATAL_ERROR)%expobj%setCounter( &
          lcounter(EXCEPTION_FATAL_ERROR))
        CALL e%surrogate%exceptionRegistry(EXCEPTION_DEBUG)%expobj%setCounter( &
          lcounter(EXCEPTION_DEBUG))
      ELSE
        CALL e%exceptionRegistry(EXCEPTION_INFORMATION)%expobj%setCounter( &
          lcounter(EXCEPTION_INFORMATION))
        CALL e%exceptionRegistry(EXCEPTION_WARNING)%expobj%setCounter( &
          lcounter(EXCEPTION_WARNING))
        CALL e%exceptionRegistry(EXCEPTION_ERROR)%expobj%setCounter( &
          lcounter(EXCEPTION_ERROR))
        CALL e%exceptionRegistry(EXCEPTION_FATAL_ERROR)%expobj%setCounter( &
          lcounter(EXCEPTION_FATAL_ERROR))
        CALL e%exceptionRegistry(EXCEPTION_DEBUG)%expobj%setCounter( &
          lcounter(EXCEPTION_DEBUG))
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
        CALL e%surrogate%exceptionRegistry(i)%expobj%setCounter(lcount)
      ELSE
        CALL e%exceptionRegistry(i)%expobj%setCounter(lcount)
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
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)

      !Try to set the log file unit number. Check that it is a valid
      !value. If not display a warning.
      IF(unit /= OUTPUT_UNIT .AND. unit /= ERROR_UNIT .AND. unit > 0) THEN
        e%logFileUnit=unit
      ELSE
        e%lastMesg='Illegal unit number for log file. '// &
                    'Log file unit not set.'
        CALL e%raiseWarning(e%lastMesg)
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
        nDebugOld=e%exceptionRegistry(EXCEPTION_DEBUG)%expobj%getCounter()
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
        IF(nDebugOld == e%exceptionRegistry(EXCEPTION_DEBUG)%expobj%getCounter())&
          e%logFileActive=.TRUE.
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
      INTEGER(SIK) :: i
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)
      e%quiet=bool
      IF(ALLOCATED(e%exceptionRegistry)) THEN
        DO i=1,SIZE(e%exceptionRegistry)
          CALL e%exceptionRegistry(i)%expobj%setQuietMode(bool)
        ENDDO
      ENDIF
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
      IF(EXCEPTION_OK < eCode .AND. eCode < EXCEPTION_SIZE) &
        CALL e%exceptionRegistry(eCode)%expobj%setQuietMode(bool)
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
      INTEGER(SIK) :: n, i
      IF(ASSOCIATED(e%surrogate)) CALL copyFromSurrogate(e)
      n=MIN(EXCEPTION_SIZE-1,SIZE(bool))
      e%quiet(1:n)=bool(1:n)
      IF(ALLOCATED(e%exceptionRegistry)) THEN
        DO i=1,SIZE(e%exceptionRegistry)-1
          CALL e%exceptionRegistry(i)%expobj%setQuietMode(e%quiet(i))
        ENDDO
      ENDIF
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
      CALL e%exceptionRegistry(EXCEPTION_ERROR)%expobj%setStopMode(bool)
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
!> @brief Raise a runtime error
!> @param e the exception object
!> @param tag  tag of error to raise
!> @param mesg an informative message about the exception that was raised
!>
!> This routine raises a user defined exception
!>
  SUBROUTINE raiseRuntimeError(e,tag,mesg)
    CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
    INTEGER(SIK),INTENT(IN) :: tag
    CHARACTER(LEN=*),INTENT(IN) :: mesg
    INTEGER(SIK) :: nRe, i, tmpTag

    IF(ASSOCIATED(e%surrogate)) THEN
      e%surrogate%lastMesg=mesg
      nRe = SIZE(e%surrogate%exceptionRegistry)
      DO i=1,nRe
        tmpTag = e%surrogate%exceptionRegistry(i)%expobj%getTag()
        IF (tmpTag == tag) THEN
          CALL e%surrogate%exceptionRegistry(i)%expobj%onRaise( &
          e%surrogate%logFileActive, e%surrogate%logFileUnit, &
          e%surrogate%lastMesg)
        ENDIF
      ENDDO
    ELSE
      e%lastMesg=mesg
      nRe = SIZE(e%exceptionRegistry)
      DO i=1,nRe
        tmpTag = e%exceptionRegistry(i)%expobj%getTag()
        IF (tmpTag == tag) THEN
          CALL e%exceptionRegistry(i)%expobj%onRaise( &
          e%logFileActive, e%logFileUnit, e%lastMesg)
        ENDIF
      ENDDO
    ENDIF

  ENDSUBROUTINE raiseRuntimeError
!
!-------------------------------------------------------------------------------
!> @brief Register a new exception type with the exception handler
!> @param e the exception object
!> @param mesg an informative message about the exception that was raised
!> @param userException the class of error to raise
!>
!> This routine registers a new exception
!>
  SUBROUTINE registerException(e,userException)
    CLASS(ExceptionHandlerType),TARGET,INTENT(INOUT) :: e
    CLASS(ExceptionTypeBase),TARGET,INTENT(IN) :: userException
    TYPE(ExceptionContainer),ALLOCATABLE,DIMENSION(:) :: tmpReg
    INTEGER(SIK) :: nCurrentReg
    INTEGER(SIK),ALLOCATABLE :: currentTags(:)
    INTEGER(SIK),ALLOCATABLE :: currentCounts(:)
    TYPE(ExceptionHandlerType),POINTER :: target_e

    IF(ASSOCIATED(e%surrogate)) THEN
      target_e = e%surrogate
    ELSE
      target_e => e
    ENDIF

    IF(ALLOCATED(target_e%exceptionRegistry)) THEN
      CALL target_e%getTagList(currentTags, currentCounts)
      IF(ANY(currentTags == userException%getTag())) THEN
        CALL target_e%raiseFatalError("Cannot register userException with existing tag.")
        RETURN
      ENDIF
    ENDIF

    ! Append new exception type to registry if not already present
    IF(.NOT. ALLOCATED(target_e%exceptionRegistry)) THEN
      ALLOCATE(target_e%exceptionRegistry(1))
      target_e%exceptionRegistry(1)%expobj => userException
    ELSE
      nCurrentReg = SIZE(target_e%exceptionRegistry)
      ALLOCATE(tmpReg(nCurrentReg))
      tmpReg = target_e%exceptionRegistry
      DEALLOCATE(target_e%exceptionRegistry)
      ALLOCATE(target_e%exceptionRegistry(nCurrentReg+1))
      target_e%exceptionRegistry(1:nCurrentReg) = tmpReg
      target_e%exceptionRegistry(nCurrentReg+1)%expobj => userException
      DEALLOCATE(tmpReg)
    ENDIF

    NULLIFY(target_e)

  ENDSUBROUTINE registerException
!
!-------------------------------------------------------------------------------
!> @brief Get a list of tags and error counts
!> @param e the exception object
!> @param tags a list of tags
!> @param counts a list of counts associated with each tag
!>
!> This routine yeilds a tag list to be used by client code to handle errors
!> on a per tag basis
!>
  SUBROUTINE getTagList(e,tags,counts)
    CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
    INTEGER(SIK),ALLOCATABLE,INTENT(INOUT) :: tags(:)
    INTEGER(SIK),ALLOCATABLE,INTENT(INOUT) :: counts(:)
    INTEGER(SIK) :: nRe, i

    IF(ASSOCIATED(e%surrogate)) THEN
      REQUIRE(ALLOCATED(e%surrogate%exceptionRegistry))
      nRe = SIZE(e%surrogate%exceptionRegistry)
      ALLOCATE(tags(nRe))
      ALLOCATE(counts(nRe))
      DO i=1,nRe
        tags(i) = e%surrogate%exceptionRegistry(i)%expobj%getTag()
        counts(i) = e%surrogate%exceptionRegistry(i)%expobj%getCounter()
      ENDDO
    ELSE
      REQUIRE(ALLOCATED(e%exceptionRegistry))
      nRe = SIZE(e%exceptionRegistry)
      ALLOCATE(tags(nRe))
      ALLOCATE(counts(nRe))
      DO i=1,nRe
        tags(i) = e%exceptionRegistry(i)%expobj%getTag()
        counts(i) = e%exceptionRegistry(i)%expobj%getCounter()
      ENDDO
    ENDIF

  ENDSUBROUTINE getTagList
!
!-------------------------------------------------------------------------------
!> @brief Get count of error matching a tag
!> @param e the exception object
!> @param tag the tag of interest
!>
!> This routine yeilds the number of errors raised matching a tag
!>
    FUNCTION getCountByTag(e,tag) RESULT(ntag)
      CLASS(ExceptionHandlerType),INTENT(INOUT) :: e
      INTEGER(SIK),INTENT(IN) :: tag
      INTEGER(SIK):: tagIdx, ntag, i
      INTEGER(SIK),ALLOCATABLE :: tags(:)
      INTEGER(SIK),ALLOCATABLE :: counts(:)

      IF(ASSOCIATED(e%surrogate)) THEN
        IF(.NOT. ALLOCATED(e%surrogate%exceptionRegistry)) THEN
          ntag = 0
          RETURN
        ENDIF
        CALL e%surrogate%getTagList(tags, counts)
      ELSE
        IF(.NOT. ALLOCATED(e%exceptionRegistry)) THEN
          ntag = 0
          RETURN
        ENDIF
        CALL e%getTagList(tags, counts)
      ENDIF

      ! tagIdx = FINDLOC(tags, tag)
      tagIdx = 0
      DO i=1,SIZE(tags)
        IF(tags(i) == tag) tagIdx = i
      ENDDO
      IF(tagIdx == 0) THEN
        ntag = 0
      ELSE
        ntag = counts(tagIdx)
      ENDIF

    ENDFUNCTION getCountByTag
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
      REQUIRE(e%isInit)
      IF(ASSOCIATED(e%surrogate)) THEN
        CALL e%surrogate%raiseRuntimeError(EXCEPTION_INFORMATION,mesg)
      ELSE
        CALL e%raiseRuntimeError(EXCEPTION_INFORMATION,mesg)
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
      REQUIRE(e%isInit)
      IF(ASSOCIATED(e%surrogate)) THEN
        CALL e%surrogate%raiseRuntimeError(EXCEPTION_WARNING,mesg)
      ELSE
        CALL e%raiseRuntimeError(EXCEPTION_WARNING,mesg)
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
      REQUIRE(e%isInit)
      IF(ASSOCIATED(e%surrogate)) THEN
        CALL e%surrogate%raiseRuntimeError(EXCEPTION_DEBUG,mesg)
      ELSE
        CALL e%raiseRuntimeError(EXCEPTION_DEBUG,mesg)
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
      REQUIRE(e%isInit)
      IF(ASSOCIATED(e%surrogate)) THEN
        CALL e%surrogate%raiseRuntimeError(EXCEPTION_ERROR,mesg)
      ELSE
        CALL e%raiseRuntimeError(EXCEPTION_ERROR,mesg)
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
      REQUIRE(e%isInit)
      IF(ASSOCIATED(e%surrogate)) THEN
        CALL e%surrogate%raiseRuntimeError(EXCEPTION_FATAL_ERROR,mesg)
      ELSE
        CALL e%raiseRuntimeError(EXCEPTION_FATAL_ERROR,mesg)
      ENDIF
    ENDSUBROUTINE raiseFatalError
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
      IF(ALLOCATED(tmpE%exceptionRegistry)) THEN
        ALLOCATE(e%exceptionRegistry(SIZE(tmpE%exceptionRegistry)))
        e%exceptionRegistry=tmpE%exceptionRegistry
      ENDIF
      NULLIFY(tmpE)
    ENDSUBROUTINE copyFromSurrogate
!
ENDMODULE ExceptionHandler
