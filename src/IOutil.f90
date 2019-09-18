!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Global module for I/O utility modules, collecting all
!> public members of other I/O type modules. This is the module that should be
!> used elsewhere in the code.
!>
!> This module and its dependent modules are tested using @c testIOutil.f90.
!> The dependent modules are listed below. Examples on how to use this
!> module can be found in the examples of each of the dependent modules. This
!> is the module that should be used elsewhere when generic I/O functionality is
!> required. If implementing a new capability for I/O then the modules used by
!> this module should be used and the new module should be used here.
!>
!> @par
!> The functionality presently, only encompasses Fortran file types. The file
!> type object is abstracted in such a way that it should be relatively painless
!> to add new types of files. One example of a new file  type may be a SILO
!> database or HDF5 file.
!>
!> @par Module Dependencies
!>  - @ref IO_Strings "IO_Strings": @copybrief IO_Strings
!>  - @ref FileType_Base "FileType_Base": @copybrief FileType_Base
!>  - @ref FileType_Fortran "FileType_Fortran": @copybrief FileType_Fortran
!>  - @ref FileType_Log "FileType_Log": @copybrief FileType_Log
!>  - @ref FileType_Input "FileType_Input": @copybrief FileType_Input
!>
!> @author Brendan Kochunas
!>   @date 05/10/2011
!>
!> @par Revisions
!> (07/15/2011) - Brendan Kochunas
!>   - Changed overall design moving functions in IOutil into new modules whose
!>     capabalities are collected here. The reason for this was to provide
!>     better modularity, and better "hooks" for introducing new features later.
!> @par
!> (08/21/2011) - Brendan Kochunas
!>   - Added "input file" type
!> @todo
!>   - Design and add an "output file" abstract type
!>   - Finish documentation
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE IOutil
  USE IntrType
  USE Strings
  USE IO_Strings

  IMPLICIT NONE
  PRIVATE

  !Run-time Environment
  PUBLIC :: GET_COMMAND
  PUBLIC :: GET_ENVIRONMENT_VARIABLE
  PUBLIC :: GET_CURRENT_DIRECTORY
  PUBLIC :: MAKE_DIRECTORY

  !Needed for getPWD_c
  INCLUDE 'getSysProcInfo_F.h'

  !> @brief Generic interface for overloading intrinsic
  !> @ref GET_ENVIRONMENT_VARIABLE.
  INTERFACE GET_ENVIRONMENT_VARIABLE
    !> @copybrief IO_Strings::get_environment_variable_string
    !> @copydetails IO_Strings::get_environment_variable_string
    MODULE PROCEDURE get_environment_variable_string
  ENDINTERFACE GET_ENVIRONMENT_VARIABLE

  !> @brief Generic interface for overloading intrinsic
  !> @ref GET_COMMAND.
  INTERFACE GET_COMMAND
    !> @copybrief IO_Strings::get_command_string
    !> @copydetails IO_Strings::get_command_string
    MODULE PROCEDURE get_command_string
  ENDINTERFACE GET_COMMAND

  !> @brief Generic interface for overloading procedure
  !> @ref GET_CURRENT_DIRECTORY.
  INTERFACE GET_CURRENT_DIRECTORY
    MODULE PROCEDURE GET_CURRENT_DIRECTORY_string
    MODULE PROCEDURE GET_CURRENT_DIRECTORY_char
  ENDINTERFACE GET_CURRENT_DIRECTORY

  !> @brief Generic interface for overloading procedure
  !> @ref MAKE_DIRECTORY.
  INTERFACE MAKE_DIRECTORY
    MODULE PROCEDURE MAKE_DIRECTORY_string
    MODULE PROCEDURE MAKE_DIRECTORY_char
  ENDINTERFACE MAKE_DIRECTORY
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Obtain the entire command initiating the program.
!> @param command   StringType containing the value of the environment variable.
!>                  if there was a problem getting the value it is blank.
!> @param length    Optional, the length of the environment variable's value.
!> @param status    Optional, integer. 0 if variable exists and either has no
!>                  value or its value was successfully assigned to @c value.
!>
!> This routine is created to overload the intrinsic @c GET_ENVIRONMENT_VARIABLE
!> such that a variable length string may be returned and the client code does
!> not need to worry about encountering a value larger than the size of value.
!>
    SUBROUTINE GET_COMMAND_string(command,length,status)
      TYPE(StringType),INTENT(OUT),OPTIONAL :: command
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: length
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: status
      CHARACTER(LEN=1) :: tmpVal
      INTEGER(SIK) :: len,stat

#ifdef __GFORTRAN__
      !Without this defined when building with GNU this routine will
      !segfault from a stack overflow.
      !
      !If this line is included when building with Intel it causes a link
      !error in testIOutil.
      INTRINSIC GET_COMMAND
#endif

      len=0
      CALL GET_COMMAND(COMMAND=tmpVal,LENGTH=len)
      IF(PRESENT(command)) THEN
        command=''
        CALL GET_COMMAND_internal(command,len,stat)
      ELSE
        CALL GET_COMMAND(LENGTH=len,STATUS=stat)
      ENDIF
      IF(PRESENT(length)) length=len
      IF(PRESENT(status)) status=stat
    ENDSUBROUTINE GET_COMMAND_string
!
!-------------------------------------------------------------------------------
!> @brief Routine to be used only by @c get_command_string
!> @param str the command line value stored as a string
!> @param nchar the known length of the command line
!>
    SUBROUTINE GET_COMMAND_internal(str,nchar,stat)
      TYPE(StringType),INTENT(INOUT) :: str
      INTEGER(SIK),INTENT(IN) :: nchar
      INTEGER(SIK),INTENT(OUT) :: stat
      CHARACTER(LEN=nchar) :: tmpChar
      CALL GET_COMMAND(COMMAND=tmpChar,STATUS=stat)
      IF(stat == 0) str=tmpChar
    ENDSUBROUTINE GET_COMMAND_internal
!
!-------------------------------------------------------------------------------
!> @brief Get the value of an environment variable
!> @param name      The name of the environment variable
!> @param value     StringType containing the value of the environment variable.
!>                  if there was a problem getting the value it is blank.
!> @param length    Optional, the length of the environment variable's value.
!> @param status    Optional, integer. 0 if variable exists and either has no
!>                  value or its value was successfully assigned to @c value.
!> @param trim_name Optional, logical on whether or not to trim trailing blanks
!>                  from @c name when checking the environment for the variable.
!>
!> This routine is created to overload the intrinsic @c GET_ENVIRONMENT_VARIABLE
!> such that a variable length string may be returned and the client code does
!> not need to worry about encountering a value larger than the size of value.
!>
    SUBROUTINE GET_ENVIRONMENT_VARIABLE_string(name,value,length,status, &
      trim_name)
      CHARACTER(LEN=*),INTENT(IN) :: name
      TYPE(StringType),INTENT(INOUT),OPTIONAL :: value
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: length
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: status
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: trim_name
      CHARACTER(LEN=1) :: tmpVal
      LOGICAL(SBK) :: trm_nm
      INTEGER(SIK) :: len,stat
      INTRINSIC GET_ENVIRONMENT_VARIABLE

      trm_nm=.TRUE.
      len=0
      IF(PRESENT(value)) THEN
        value=''
        CALL GET_ENVIRONMENT_VARIABLE(NAME=name,VALUE=tmpVal,LENGTH=len, &
          STATUS=stat)
        IF(len > 0) THEN
          !Variable exists and has a value
          IF(PRESENT(trim_name)) trm_nm=trim_name
          CALL GET_ENVIRONMENT_VARIABLE_internal(value,name,len,trm_nm,stat)
        ENDIF
      ELSE
        CALL GET_ENVIRONMENT_VARIABLE(NAME=name,LENGTH=len,STATUS=stat)
      ENDIF
      IF(PRESENT(length)) length=len
      IF(PRESENT(status)) status=stat
    ENDSUBROUTINE GET_ENVIRONMENT_VARIABLE_string
!
!-------------------------------------------------------------------------------
!> @brief Routine to be used only by @c get_environment_variable_string
!> @param str the environment variable value stored as a string
!> @param varname the name of the environment variable
!> @param nchar the known length of the environment variable's value
!> @param ltrim logical on whether or not the name is to be trimmed.
!>
    SUBROUTINE GET_ENVIRONMENT_VARIABLE_internal(str,varname,nchar,ltrim,stat)
      TYPE(StringType),INTENT(INOUT) :: str
      CHARACTER(LEN=*),INTENT(IN) :: varname
      INTEGER(SIK),INTENT(IN) :: nchar
      LOGICAL(SBK),INTENT(IN) :: ltrim
      INTEGER(SIK),INTENT(OUT) :: stat
      CHARACTER(LEN=nchar) :: tmpChar

      CALL GET_ENVIRONMENT_VARIABLE(NAME=varname,VALUE=tmpChar, &
        STATUS=stat,TRIM_NAME=ltrim)
      IF(stat == 0) str=tmpChar
    ENDSUBROUTINE GET_ENVIRONMENT_VARIABLE_internal
!
!-------------------------------------------------------------------------------
    SUBROUTINE GET_CURRENT_DIRECTORY_char(dir,length,status)
      CHARACTER(LEN=:),ALLOCATABLE,INTENT(OUT) :: dir
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: length
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: status
      TYPE(StringType) :: tmp
      CALL GET_CURRENT_DIRECTORY_string(tmp,length,status)
      dir=CHAR(tmp)
    ENDSUBROUTINE GET_CURRENT_DIRECTORY_char
!
!-------------------------------------------------------------------------------
    SUBROUTINE GET_CURRENT_DIRECTORY_string(dir,length,status)
      TYPE(StringType),INTENT(OUT),OPTIONAL :: dir
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: length
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: status
      CHARACTER(LEN=1) :: tmp
      INTEGER(SIK) :: stat,len

      len=0
      CALL getPWD_c(tmp,1,stat)
      IF(stat > 0) len=stat
      stat=MIN(0,stat)
      IF(PRESENT(dir)) CALL GET_CURRENT_DIRECTORY_internal(dir,len,stat)
      IF(PRESENT(length)) length=len-1
      IF(PRESENT(status)) status=stat
    ENDSUBROUTINE GET_CURRENT_DIRECTORY_string
!
!-------------------------------------------------------------------------------
    SUBROUTINE GET_CURRENT_DIRECTORY_internal(dir,length,status)
      TYPE(StringType),INTENT(OUT) :: dir
      INTEGER(SIK),INTENT(IN) :: length
      INTEGER(SIK),INTENT(OUT) :: status
      CHARACTER(LEN=length) :: tmp
      CALL getPWD_c(tmp,length,status)
      IF(status == 0) dir=tmp(1:length-1) !Remove the C_NULL_CHAR
    ENDSUBROUTINE GET_CURRENT_DIRECTORY_internal
!
!-------------------------------------------------------------------------------
    SUBROUTINE MAKE_DIRECTORY_string(dir,status)
      TYPE(StringType),INTENT(IN) :: dir
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: status
      CALL MAKE_DIRECTORY_char(CHAR(dir),status)
    ENDSUBROUTINE MAKE_DIRECTORY_string
!
!-------------------------------------------------------------------------------
    SUBROUTINE MAKE_DIRECTORY_char(dir,status)
#ifdef __INTEL_COMPILER
      USE IFPORT !For SYSTEM function
#endif
      CHARACTER(LEN=*),INTENT(IN) :: dir
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: status
      CHARACTER(LEN=LEN(dir)) :: tmp
      INTEGER(SIK) :: s

      tmp=ADJUSTL(dir)
      CALL SlashRep(tmp)
#ifdef WIN32
      s=SYSTEM('if not exist "'//TRIM(dir)//'" mkdir "'//TRIM(dir)//'"')
#else
      s=SYSTEM('mkdir -p "'//TRIM(dir)//'"')
#endif
      IF(PRESENT(status)) status=s
    ENDSUBROUTINE MAKE_DIRECTORY_char
!
ENDMODULE IOutil
