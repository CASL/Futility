!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module provides a command line processor type object.
!>
!> This module will process the command line and store the arguments, or it may
!> be given a string representing the command line that will be processed.
!> It stores these options as strings which can be retrieved.
!> This module is tested using @c testCmdLineProc.f90. An
!> example of how to use this object is given below, the unit test also shows
!> how to use the object. Code coverage documentation can be found on the @ref
!> CodeCoverageReports page.
!>
!> The general way in which it this object is used, is the developer must first
!> configure the object. This configuration involves setting the name of the
!> executable, defining all the input options, then defining the usage. The
!> usage should reflect how a developer supplied routine will process the
!> command line. The other piece that is needed by the command line processor
!> is an external routine with a defined interface that processes the command
!> line. The example below shows how this is done.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "Exceptionhandler": @copybrief ExceptionHandler
!>  - @ref IOutil "IOutil": @copybrief IOutil
!>
!> @par EXAMPLE
!> @code
!> PROGRAM CmdLineExample
!>
!>   USE CommandLineProcessor
!>   IMPLICIT NONE
!>
!>   TYPE(CmdLineProcType) :: CLP
!>
!>   !Configure exception handler of CLP
!>   CALL CLP%exception%setStopOnError(.FALSE.)
!>   CALL CLP%exception%setQuietMode(.TRUE.)
!>
!>   !Configure the command line processor (CLP)
!>   CALL CLP%setExecName('CmdLineExample.exe')
!>   CALL CLP%setNumOpts(3)
!>   CALL CLP%defineOpt(1,'-help','Display this help message')
!>   CALL CLP%defineOpt(2,' input_file','Name of input file')
!>   CALL CLP%defineOpt(3,'output_file','Name of output file')
!>   CALL CLP%defineUsage('[[-help] | [input_file] [output_file]]')
!>   CALL CLP%DisplayHelp()
!>
!>   !Process command line
!>   CALL CLP%setCmdLine()
!>   CALL CLP%ProcCmdLine(ExampleProcSub)
!>
!>   CONTAINS
!>     !Interface of this subroutine is fixed, it can be defined
!>     !anywhere in the code though, and it is to be passed
!>     !as an input argument when calling CLP%ProcCmdLine()
!>     SUBROUTINE ExampleProcSub(uclp)
!>
!>       TYPE(CmdLineProcType) :: uclp
!>       CHARACTER(LEN=MAX_ARG_STRING_LENGTH) :: arg
!>
!>       !The usage is defined here, not by the string passed to %defineUsage
!>       !In this example the useage is '-help' or the input file name or
!>       !the input file name and output file name.
!>
!>       SELECT CASE(uclp%getNargs())
!>         CASE(0)
!>           !use the default input file name
!>         CASE(1)
!>           CALL uclp%getCmdArg(1,arg)
!>           IF(TRIM(arg) == '-help')
!>             uclp%DisplayHelp
!>             STOP
!>           ELSE
!>             !process arg as input file name
!>           ENDIF
!>         CASE(2)
!>           CALL uclp%getCmdArg(1,arg)
!>           IF(TRIM(arg) == '-help')
!>             uclp%DisplayHelp
!>             CALL uclp%exception%raiseFatalError('Incorrect usage.')
!>           ELSE
!>             !process arg as input file name
!>           ENDIF
!>           CALL uclp%getCmdArg(2,arg)
!>           IF(TRIM(arg) == '-help')
!>             uclp%DisplayHelp
!>             CALL uclp%exception%raiseFatalError('Incorrect usage.')
!>           ELSE
!>             !process arg as output file name
!>           ENDIF
!>         CASE DEFAULT
!>           CALL uclp%DisplayHelp()
!>           CALL uclp%exception%raiseFatalError('Incorrect number of input arguments')
!>       END SELECT
!>
!>   END SUBROUTINE
!>
!> END PROGRAM
!> @endcode
!>
!> @author Brendan Kochunas
!>   @date 05/12/2011
!>
!> @par Revisions:
!> (07/05/2011) - Brendan Kochunas
!>   - Modified to be a derived type with type bound procedures
!>   - Updated unit test and documentation
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE CommandLineProcessor
  USE ISO_FORTRAN_ENV
  USE IntrType
  USE Strings
  USE ParameterLists
  USE ExceptionHandler
  USE IO_Strings
  USE IOutil

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: CmdLineProcType

  !> Name of the module to be used in exception reporting
  CHARACTER(LEN=*),PARAMETER :: modName='COMMANDLINEPROCESSOR'

  !> @brief Derived type for a command line option
  !>
  !> This is used to only for displaying help information
  TYPE :: CmdLineOptType
    !> The name of the option
    TYPE(StringType) :: name
    !> The description of the option
    TYPE(StringType) :: description
  ENDTYPE CmdLineOptType

  !> Derived type for a command line processor object
  TYPE :: CmdLineProcType
    !> Name of executable
    TYPE(StringType),PRIVATE :: execname
    !> String describing usage
    TYPE(StringType),PRIVATE :: usage
    !> Original command line
    TYPE(StringType) :: cmdline
    !> Number of allowable command line options
    INTEGER(SIK),PRIVATE :: nopts=0
    !> Number of command line arguments
    INTEGER(SIK),PRIVATE :: narg=0
    !> Character array with command line arguments
    TYPE(StringType),POINTER,PRIVATE :: CmdLineArgs(:)=>NULL()
    !> List of command line options for help message
    TYPE(CmdLineOptType),POINTER,PRIVATE :: opts(:)=>NULL()
    !> Exception Handler for the command line processor
    TYPE(ExceptionHandlerType) :: e
    CONTAINS
      !> @copybrief CommandLineProcessor::setExecName
      !> @copydetails CommandLineProcessor::setExecName
      PROCEDURE,PASS :: setExecName
      !> @copybrief CommandLineProcessor::getExecName
      !> @copydetails CommandLineProcessor::getExecName
      PROCEDURE,PASS :: getExecName
      !> @copybrief CommandLineProcessor::defineUsage
      !> @copydetails CommandLineProcessor::defineUsage
      PROCEDURE,PASS :: defineUsage
      !> @copybrief CommandLineProcessor::setNumOpts
      !> @copydetails CommandLineProcessor::setNumOpts
      PROCEDURE,PASS :: setNumOpts
      !> @copybrief CommandLineProcessor::getNumOpts
      !> @copydetails CommandLineProcessor::getNumOpts
      PROCEDURE,PASS :: getNumOpts
      !> @copybrief CommandLineProcessor::getOptName
      !> @copydetails CommandLineProcessor::getOptName
      PROCEDURE,PASS :: getOptName
      !> @copybrief CommandLineProcessor::defineOpt
      !> @copydetails CommandLineProcessor::defineOpt
      PROCEDURE,PASS :: defineOpt
      !> @copybrief CommandLineProcessor::clearOpts
      !> @copydetails CommandLineProcessor::clearOpts
      PROCEDURE,PASS :: clearOpts
      !> @copybrief CommandLineProcessor::setCmdLine
      !> @copydetails CommandLineProcessor::setCmdLine
      PROCEDURE,PASS :: setCmdLine
      !> @copybrief CommandLineProcessor::getNargs
      !> @copydetails CommandLineProcessor::getNargs
      PROCEDURE,PASS :: getNargs
      !> @copybrief CommandLineProcessor::getCmdArg_char
      !> @copydetails CommandLineProcessor::getCmdArg_char
      PROCEDURE,PRIVATE,PASS :: getCmdArg_char
      !> @copybrief CommandLineProcessor::getCmdArg_string
      !> @copydetails CommandLineProcessor::getCmdArg_string
      PROCEDURE,PRIVATE,PASS :: getCmdArg_string
      !> Generic type bound interface for all @c getCmdArg routines
      GENERIC :: getCmdArg => getCmdArg_char,getCmdArg_string
      !> @copybrief CommandLineProcessor::clearCmdLine
      !> @copydetails CommandLineProcessor::clearCmdLine
      PROCEDURE,PASS :: clearCmdLine
      !> @copybrief CommandLineProcessor::DisplayHelp
      !> @copydetails CommandLineProcessor::DisplayHelp
      PROCEDURE,PASS :: DisplayHelp
      !> @copybrief CommandLineProcessor::ProcCmdLineArgs
      !> @copydetails CommandLineProcessor::ProcCmdLineArgs
      PROCEDURE,PASS :: ProcCmdLineArgs
      !> @copybrief CommandLineProcessor::clear_CLP
      !> @copydetails CommandLineProcessor::clear_CLP
      PROCEDURE,PASS :: clear => clear_CLP
  ENDTYPE CmdLineProcType
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Sets the name of the executable to use for the command line
!> processor
!> @param clp command line processor object
!> @param execname executable name
!>
!> Raises a warning if the name is too long and will be truncated.
!>
    PURE SUBROUTINE setExecName(clp,execname)
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      CHARACTER(LEN=*),INTENT(IN) :: execname

      clp%execname=TRIM(execname)
    ENDSUBROUTINE setExecName
!
!-------------------------------------------------------------------------------
!> @brief Returns the name of the executable defined for the command line
!> processor
!> @param clp command line processor object
!> @returns execname executable name
!>
    PURE FUNCTION getExecName(clp) RESULT(execname)
      CLASS(CmdLineProcType),INTENT(IN) :: clp
      CHARACTER(LEN=clp%execname%n) :: execname
      execname=clp%execname
    ENDFUNCTION getExecName
!
!-------------------------------------------------------------------------------
!> @brief Define the string for describing the usage of the executable from the
!> command line.
!> @param clp command line processor object
!> @param usagestr the string with the usage syntax
!>
    PURE SUBROUTINE defineUsage(clp,usagestr)
      CHARACTER(LEN=*),PARAMETER :: myName='defineUsage'
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      CHARACTER(LEN=*),INTENT(IN) :: usagestr

      clp%usage=TRIM(usagestr)
    ENDSUBROUTINE defineUsage
!
!-------------------------------------------------------------------------------
!> @brief Set the number of possible command line options
!> @param clp command line processor object
!> @param n the number of command line options accepted by the executable
!>
    SUBROUTINE setNumOpts(clp,n)
      CHARACTER(LEN=*),PARAMETER :: myName='setNumOpts'
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      INTEGER(SIK),INTENT(IN) :: n

      IF(n < 1) THEN
        CALL clp%e%raiseError(modName//'::'//myName// &
          ' - illegal input, number of options is less than 1!')
      ELSE
        IF(clp%nopts /= 0) THEN
          CALL clp%e%raiseDebug(modName//'::'//myName// &
            ' - number of command line options already set!')
        ELSE
          clp%nopts=n
          ALLOCATE(clp%opts(1:n))
        ENDIF
      ENDIF
    ENDSUBROUTINE setNumOpts
!
!-------------------------------------------------------------------------------
!> @brief Get the number of command line options that have been set
!> @param clp command line processor object
!> @returns n the number of command line options accepted by the executable
!>
    PURE FUNCTION getNumOpts(clp) RESULT(n)
      CLASS(CmdLineProcType),INTENT(IN) :: clp
      INTEGER(SIK) :: n
      n=clp%nopts
    ENDFUNCTION getNumOpts
!
!-------------------------------------------------------------------------------
!> @brief Get the name of a particular command line option
!> @param clp command line processor object
!> @param optName the name of the command line option
!>
    PURE SUBROUTINE getOptName(clp,iopt,optName)
      CLASS(CmdLineProcType),INTENT(IN) :: clp
      INTEGER(SIK),INTENT(IN) :: iopt
      TYPE(StringType),INTENT(INOUT) :: optName
      optName=''
      IF(0 < iopt .AND. iopt <= clp%nopts) &
        optName=TRIM(ADJUSTL(clp%opts(iopt)%name))
    ENDSUBROUTINE getOptName
!
!-------------------------------------------------------------------------------
!> @brief Define the names and descriptions for each command line option
!> for the help message.
!> @param clp command line processor object
!> @param iopt the index of the option to define
!> @param name string with the name of the command line option
!> @param description string describing the command line option
!>
    SUBROUTINE defineOpt(clp,iopt,name,description)
      CHARACTER(LEN=*),PARAMETER :: myName='defineOpt'
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      INTEGER(SIK),INTENT(IN) :: iopt
      CHARACTER(LEN=*),INTENT(IN) :: name,description
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: emsg

      IF(0 < iopt .AND. iopt <= clp%nopts) THEN
        !Warn for bad input
        IF(LEN_TRIM(name) == 0) &
          CALL clp%e%raiseDebug(modName//'::'//myName// &
            ' - option name is empty!')
        IF(LEN_TRIM(description) == 0) &
          CALL clp%e%raiseDebug(modName//'::'//myName// &
            ' - option description is empty!')
        !Set option name and description for help message
        clp%opts(iopt)%name=TRIM(name)
        clp%opts(iopt)%description=TRIM(description)
      ELSE
        !Report error for illegal value of iopt
        WRITE(emsg,'(2(a,i4),a)') modName//'::'//myName//' - Option ',iopt, &
          ' is less than 0 or greater than ',clp%nopts,'.'
        CALL clp%e%raiseError(TRIM(emsg))
      ENDIF
    ENDSUBROUTINE defineOpt
!
!-------------------------------------------------------------------------------
!> @brief Clear the options defined for the command line processor.
!> @param clp command line processor object
!>
    PURE SUBROUTINE clearOpts(clp)
      CHARACTER(LEN=*),PARAMETER :: myName='clearOpts'
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      INTEGER(SIK) :: iopt
      clp%usage=''
      IF(ASSOCIATED(clp%opts)) THEN
        DO iopt=SIZE(clp%opts),1,-1
          clp%opts(iopt)%description=''
          clp%opts(iopt)%name=''
        ENDDO
        DEALLOCATE(clp%opts)
        clp%nopts=0
      ENDIF
    ENDSUBROUTINE clearOpts
!
!-------------------------------------------------------------------------------
!> @brief Processes the command line and stores the arguments in CmdLineArgs
!> @param clp command line processor object
!> @param iline (OPTIONAL) a string containing the command line arguments to
!>              process
!>
!> The optional input argument iline is provided should the code that this module
!> is a part of be linked as a library into another executable that processes a
!> different command line.
!>
    SUBROUTINE setCmdLine(clp,iline)
      CHARACTER(LEN=*),PARAMETER :: myName='setCmdLine'
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: iline
      TYPE(StringType) :: cmdline,opt
      INTEGER(SIK) :: iarg,ierr,cmdlinelength

      IF(.NOT.ASSOCIATED(clp%CmdLineArgs)) THEN
        IF(PRESENT(iline)) THEN

          cmdline=TRIM(iline)
          clp%cmdline=cmdline
          clp%narg=nFields(cmdline)
          !Only store command line arguments if there were any.
          IF(clp%narg > 0) THEN
            ALLOCATE(clp%CmdLineArgs(1:clp%narg))
            DO iarg=1,clp%narg
              CALL getField(iarg,cmdline,opt)
              clp%CmdLineArgs(iarg)=opt
            ENDDO
          ENDIF
        ELSE
        CALL GET_COMMAND(cmdline,cmdlinelength,ierr)
          IF(ierr /= 0) CALL clp%e%raiseError(modName//'::'//myName// &
              ' - problem getting command line.')
          clp%narg=nFields(cmdline)-1
          clp%cmdline=cmdline
          !Only store command line arguments if there were any.
          IF(clp%narg > 0) THEN
            ALLOCATE(clp%CmdLineArgs(1:clp%narg))
            DO iarg=2,clp%narg+1
              CALL getField(iarg,cmdline,opt)
              clp%CmdLineArgs(iarg-1)=TRIM(opt)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE setCmdLine
!
!-------------------------------------------------------------------------------
!> @brief Clears the command line information that has been processed by the
!> object.
!> @param clp command line processor object
!>
    PURE SUBROUTINE clearCmdLine(clp)
      CHARACTER(LEN=*),PARAMETER :: myName='clearCmdLine'
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      INTEGER(SIK) :: iarg
      clp%cmdline=''
      IF(ASSOCIATED(clp%CmdLineArgs)) THEN
        DO iarg=SIZE(clp%CmdLineArgs),1,-1
          clp%CmdLineArgs(iarg)=''
        ENDDO
        DEALLOCATE(clp%CmdLineArgs)
        clp%narg=0
      ENDIF
    ENDSUBROUTINE clearCmdLine
!
!-------------------------------------------------------------------------------
!> @brief Get a command line argument
!> @param clp command line processor object
!> @param iarg input, the ith command line argument
!> @param argout output, a string with the ith output argument
!>
    PURE SUBROUTINE getCmdArg_string(clp,iarg,argout)
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      INTEGER(SIK),INTENT(IN) :: iarg
      TYPE(StringType),INTENT(OUT) :: argout

      argout=''
      IF(0 < iarg .AND. iarg <= clp%narg) argout=TRIM(clp%CmdLineArgs(iarg))
    ENDSUBROUTINE getCmdArg_string
!
!-------------------------------------------------------------------------------
!> @brief Get a command line argument
!> @param clp command line processor object
!> @param iarg input, the ith command line argument
!> @param argout output, a string with the ith output argument
!>
    PURE SUBROUTINE getCmdArg_char(clp,iarg,argout)
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      INTEGER(SIK),INTENT(IN) :: iarg
      CHARACTER(LEN=*),INTENT(OUT) :: argout
      TYPE(StringType) :: tmpStr
      CALL getCmdArg_string(clp,iarg,tmpStr)
      argout=tmpStr
    ENDSUBROUTINE getCmdArg_char
!
!-------------------------------------------------------------------------------
!> @brief Get the number command line arguments
!> @param clp command line processor object
!>
    PURE FUNCTION getNargs(clp) RESULT(narg)
      CLASS(CmdLineProcType),INTENT(IN) :: clp
      INTEGER(SIK) :: narg
      narg=clp%narg
    ENDFUNCTION getNargs
!
!-------------------------------------------------------------------------------
!> @brief This routine controls execution based on command line arguments
!> @param clp command line processor object
!> @param userSubroutine the name of the subroutine defined outside the module
!> @param plist optional parameter list to process arguments into
!> that processes the command line arguments.
!>
    SUBROUTINE ProcCmdLineArgs(clp,userSubroutine,plist)
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      TYPE(ParamType),INTENT(INOUT),OPTIONAL :: plist
      !> This is the definition of the interface to the user subroutine
      !> defined elsewhere.
      INTERFACE
        SUBROUTINE userSubroutine(uclp,plist)
          IMPORT :: CmdLineProcType,ParamType
          CLASS(CmdLineProcType),INTENT(INOUT) :: uclp
          TYPE(ParamType),INTENT(INOUT),OPTIONAL :: plist
        ENDSUBROUTINE
      ENDINTERFACE

      !Call the other subroutine that was passed in.
      CALL userSubroutine(clp,plist)

    ENDSUBROUTINE ProcCmdLineArgs
!
!-------------------------------------------------------------------------------
!> @brief Displays a help message to the prompt
!> @param clp command line processor object
!>
    SUBROUTINE DisplayHelp(clp)
      CLASS(CmdLineProcType),INTENT(IN) :: clp
      INTEGER(SIK) :: iopt

      IF(LEN_TRIM(clp%execname) > 0 .AND. LEN_TRIM(clp%usage) > 0) THEN
        WRITE(OUTPUT_UNIT,*)
        WRITE(OUTPUT_UNIT,*)
        WRITE(OUTPUT_UNIT,'(a)') ' Usage for '//TRIM(clp%execname)//' is...'
        WRITE(OUTPUT_UNIT,*)
        WRITE(OUTPUT_UNIT,'(4x,a)') TRIM(clp%execname)//'  '//TRIM(clp%usage)
        DO iopt=1,clp%nopts
          WRITE(OUTPUT_UNIT,*)
          WRITE(OUTPUT_UNIT,'(6x,a)') TRIM(clp%opts(iopt)%name)//' '// &
                                      TRIM(clp%opts(iopt)%description)
        ENDDO
        WRITE(OUTPUT_UNIT,*)
      ENDIF
    ENDSUBROUTINE DisplayHelp
!
!-------------------------------------------------------------------------------
!> @brief Clears the command line processor object
!> @param clp command line processor object
!>
    PURE SUBROUTINE clear_CLP(clp)
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp

      clp%execname=''
      CALL clearCmdLine(clp)
      CALL clearOpts(clp)
    ENDSUBROUTINE clear_CLP
!
ENDMODULE CommandLineProcessor
