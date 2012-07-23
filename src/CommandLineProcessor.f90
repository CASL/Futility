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
  USE ExceptionHandler
  USE IOutil
  
  IMPLICIT NONE
  PRIVATE
  
  PUBLIC :: MAX_ARG_STRING_LENGTH
  PUBLIC :: MAX_EXECNAME_LENGTH
  PUBLIC :: MAX_CMD_LINE_OPT_NAME
  PUBLIC :: MAX_CMD_LINE_OPT_DESC
  PUBLIC :: CmdLineProcType
  
  !> Name of the module to be used in exception reporting
  CHARACTER(LEN=*),PARAMETER :: modName='COMMANDLINEPROCESSOR'
  
  !> Maximum length for any one argument on the command line
  INTEGER(SIK),PARAMETER :: MAX_ARG_STRING_LENGTH=1024
  !> Maximum length for the name of the executable to be used by the command
  !> line processor.
  INTEGER(SIK),PARAMETER :: MAX_EXECNAME_LENGTH=32
  !> Maximum length for the name of a command line option
  INTEGER(SIK),PARAMETER :: MAX_CMD_LINE_OPT_NAME=32
  !> Maximum length for the description of a command line option
  INTEGER(SIK),PARAMETER :: MAX_CMD_LINE_OPT_DESC=1024
  
  !> @brief Derived type for a command line option
  !>
  !> This is used to only for displaying help information
  TYPE :: CmdLineOptType
    !> The name of the option
    CHARACTER(LEN=MAX_CMD_LINE_OPT_NAME) :: name=''
    !> The description of the option
    CHARACTER(LEN=MAX_CMD_LINE_OPT_DESC) :: description=''
  ENDTYPE CmdLineOptType
  
  !> Derived type for a command line processor object
  TYPE :: CmdLineProcType
    !> Name of executable
    CHARACTER(LEN=MAX_EXECNAME_LENGTH),PRIVATE :: execname=''
    !> String describing usage
    CHARACTER(LEN=MAX_ARG_STRING_LENGTH),PRIVATE :: usage=''
    !> Number of allowable command line options
    INTEGER(SIK),PRIVATE :: nopts=0
    !> Number of command line arguments
    INTEGER(SIK),PRIVATE :: narg=0
    !> Character array with command line arguments
    CHARACTER(LEN=MAX_ARG_STRING_LENGTH),POINTER,PRIVATE :: CmdLineArgs(:)=>NULL()
    !> List of command line options for help message
    TYPE(CmdLineOptType),POINTER,PRIVATE :: opts(:)=>NULL()
    !> Exception Handler for the command line processor
    TYPE(ExceptionHandlerType),POINTER :: e
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
      !> @copybrief CommandLineProcessor::getCmdArg
      !> @copydetails CommandLineProcessor::getCmdArg
      PROCEDURE,PASS :: getCmdArg
      !> @copybrief CommandLineProcessor::clearCmdLine
      !> @copydetails CommandLineProcessor::clearCmdLine
      PROCEDURE,PASS :: clearCmdLine
      !> @copybrief CommandLineProcessor::DisplayHelp
      !> @copydetails CommandLineProcessor::DisplayHelp
      PROCEDURE,PASS :: DisplayHelp
      !> @copybrief CommandLineProcessor::ProcCmdLineArgs
      !> @copydetails CommandLineProcessor::ProcCmdLineArgs
      PROCEDURE,PASS :: ProcCmdLineArgs
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
    SUBROUTINE setExecName(clp,execname)
      CHARACTER(LEN=*),PARAMETER :: myName='setExecName'
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      CHARACTER(LEN=*) :: execname
      TYPE(ExceptionHandlerType) :: e
      
      IF(LEN_TRIM(execname) > MAX_EXECNAME_LENGTH) THEN
        IF(ASSOCIATED(clp%e)) THEN
          CALL clp%e%raiseWarning(modName//'::'//myName// &
            ' - name is going to be truncated.')
        ELSE
          CALL e%raiseWarning(modName//'::'//myName// &
            ' - name is going to be truncated.')
        ENDIF
      ENDIF
      clp%execname=TRIM(execname)
    ENDSUBROUTINE setExecName
!
!-------------------------------------------------------------------------------
!> @brief Returns the name of the executable defined for the command line
!> processor
!> @param clp command line processor object
!> @returns execname executable name
!>
    FUNCTION getExecName(clp) RESULT(execname)
      CLASS(CmdLineProcType),INTENT(IN) :: clp
      CHARACTER(LEN=MAX_EXECNAME_LENGTH) :: execname
      execname=clp%execname
    ENDFUNCTION getExecName
!
!-------------------------------------------------------------------------------
!> @brief Define the string for describing the usage of the executable from the
!> command line.
!> @param clp command line processor object
!> @param usagestr the string with the usage syntax
!>
    SUBROUTINE defineUsage(clp,usagestr)
      CHARACTER(LEN=*),PARAMETER :: myName='defineUsage'
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      CHARACTER(LEN=*),INTENT(IN) :: usagestr
      TYPE(ExceptionHandlerType) :: e
      
      IF(LEN_TRIM(usagestr) > MAX_ARG_STRING_LENGTH) THEN
        IF(ASSOCIATED(clp%e)) THEN
          CALL clp%e%raiseWarning(modName//'::'//myName// &
            ' - usage string is going to be truncated.')
        ELSE
          CALL e%raiseWarning(modName//'::'//myName// &
            ' - usage string is going to be truncated.')
        ENDIF
      ENDIF
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
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(clp%e)) THEN
        ALLOCATE(clp%e)
        localalloc=.TRUE.
      ENDIF
      IF(n < 1) THEN
        CALL clp%e%raiseError(modName//'::'//myName// &
          ' - illegal input, number of options is less than 1!')
      ELSE
        IF(clp%nopts /= 0) THEN
          CALL clp%e%raiseWarning(modName//'::'//myName// &
            ' - number of command line options already set!')
        ELSE
          clp%nopts=n
          ALLOCATE(clp%opts(1:n))
        ENDIF
      ENDIF
      IF(localalloc) DEALLOCATE(clp%e)
    ENDSUBROUTINE setNumOpts
!
!-------------------------------------------------------------------------------
!> @brief Get the number of command line options that have been set
!> @param clp command line processor object
!> @returns n the number of command line options accepted by the executable
!>
    FUNCTION getNumOpts(clp) RESULT(n)
      CLASS(CmdLineProcType),INTENT(IN) :: clp
      INTEGER(SIK) :: n
      n=clp%nopts
    ENDFUNCTION getNumOpts
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
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(clp%e))THEN
        ALLOCATE(clp%e)
        localalloc=.TRUE.
      ENDIf
      IF(0 < iopt .AND. iopt <= clp%nopts) THEN
        !Warn for bad input
        IF(LEN_TRIM(name) > MAX_CMD_LINE_OPT_NAME) &
          CALL clp%e%raiseWarning(modName//'::'//myName// &
            ' - option name is going to be truncated.')
        IF(LEN_TRIM(name) == 0) &
          CALL clp%e%raiseWarning(modName//'::'//myName// &
            ' - option name is empty!')
        IF(LEN_TRIM(description) > MAX_CMD_LINE_OPT_DESC) &
          CALL clp%e%raiseWarning(modName//'::'//myName// &
            ' - option description is going to be truncated.')
        IF(LEN_TRIM(description) == 0) &
          CALL clp%e%raiseWarning(modName//'::'//myName// &
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
      IF(localalloc) DEALLOCATE(clp%e)
    ENDSUBROUTINE defineOpt
!
!-------------------------------------------------------------------------------
!> @brief Clear the options defined for the command line processor.
!> @param clp command line processor object
!>
    SUBROUTINE clearOpts(clp)
      CHARACTER(LEN=*),PARAMETER :: myName='clearOpts'
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      TYPE(ExceptionHandlerType) :: e
      IF(ASSOCIATED(clp%opts)) THEN
        DEALLOCATE(clp%opts)
        clp%nopts=0
      ELSE
        IF(ASSOCIATED(clp%e)) THEN
          CALL clp%e%raiseWarning(modName//'::'//myName// &
            ' - command line options already clear.')
        ELSE
          CALL e%raiseWarning(modName//'::'//myName// &
            ' - command line options already clear.')
        ENDIF
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
      CHARACTER(LEN=MAX_ARG_STRING_LENGTH) :: cmdline
      CHARACTER(LEN=MAX_ARG_STRING_LENGTH) :: opt=''
      INTEGER(SIK) :: iarg,ierr,cmdlinelength
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(clp%e))THEN
        ALLOCATE(clp%e)
        localalloc=.TRUE.
      ENDIf
      IF(.NOT.ASSOCIATED(clp%CmdLineArgs)) THEN
        IF(PRESENT(iline)) THEN
          IF(LEN_TRIM(iline) > MAX_ARG_STRING_LENGTH) &
            CALL clp%e%raiseError(modName//'::'//myName// &
              ' - input command line is being truncated.')
          cmdline=TRIM(iline)
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
          IF(cmdlinelength > MAX_ARG_STRING_LENGTH) CALL clp%e%raiseError( &
            modName//'::'//myName//' - command line is too long to store!')
          clp%narg=nFields(cmdline)-1
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
      IF(localalloc) DEALLOCATE(clp%e)
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
      IF(ASSOCIATED(clp%CmdLineArgs)) THEN
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
    SUBROUTINE getCmdArg(clp,iarg,argout)
      CHARACTER(LEN=*),PARAMETER :: myName='getCmdArg'
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      INTEGER(SIK),INTENT(IN) :: iarg
      CHARACTER(LEN=*),INTENT(OUT) :: argout
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: emsg
      TYPE(ExceptionHandlerType) :: e
      
      argout=''
      IF(0 < iarg .AND. iarg <= clp%narg) THEN
        argout=TRIM(clp%CmdLineArgs(iarg))
      ELSE
        WRITE(emsg,'(2(a,i4),a)') modName//'::'//myName//' - Argument ',iarg, &
          ' is less than 0 or greater than ',clp%narg,'.'
        IF(ASSOCIATED(clp%e)) THEN
          CALL clp%e%raiseError(TRIM(emsg))
        ELSE
          CALL e%raiseError(TRIM(emsg))
        ENDIF
      ENDIF
    ENDSUBROUTINE getCmdArg
!
!-------------------------------------------------------------------------------
!> @brief Get the number command line arguments
!> @param clp command line processor object
!>
    FUNCTION getNargs(clp) RESULT(narg)
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      INTEGER(SIK) :: narg
      narg=clp%narg
    ENDFUNCTION getNargs
!
!-------------------------------------------------------------------------------
!> @brief This routine controls execution based on command line arguments
!> @param clp command line processor object
!> @param userSubroutine the name of the subroutine defined outside the module
!> that processes the command line arguments.
!>
    SUBROUTINE ProcCmdLineArgs(clp,userSubroutine)
      CLASS(CmdLineProcType),INTENT(INOUT) :: clp
      !> This is the definition of the interface to the user subroutine
      !> defined elsewhere.
      INTERFACE
        SUBROUTINE userSubroutine(uclp)
          IMPORT :: CmdLineProcType
          CLASS(CmdLineProcType),INTENT(INOUT) :: uclp
        ENDSUBROUTINE
      ENDINTERFACE
      
      !Call the other subroutine that was passed in.
      CALL userSubroutine(clp)
      
    ENDSUBROUTINE ProcCmdLineArgs
!
!-------------------------------------------------------------------------------
!> @brief Displays a help message to the prompt
!> @param clp command line processor object
!>
    SUBROUTINE DisplayHelp(clp)
      CLASS(CmdLineProcType),INTENT(IN) :: clp
      INTEGER(SIK) :: iopt
      
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
    ENDSUBROUTINE DisplayHelp
!
ENDMODULE CommandLineProcessor
