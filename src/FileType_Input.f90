!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module for I/O defines the derived type for an Input File
!> object.
!>
!> The input file type is an extension of the Fortran file type. Specifically it
!> is an existing read-only text file. The purpose of the input file is provide
!> input to the program to be read at run time. It provides additional methods
!> for @ref FileType_Input::read_oneline "fgetl()" which gets one line of text
!> from the file. There are also methods to allow one to set a file to echo
!> the input that's read to another file. It also provides the maximum length
!> of one line of text from the input file. This module is considered to be
!> an I/O utility module so it's public members should be accessed through @ref
!> IOutil "IOutil". This module should not be used directly except when it is
!> needed by another I/O utility module. This module is tested by
!> @c testIOutil.f90 and the coverage report can be found at the @ref
!> CodeCoverageReports "Code Coverage Reports" page. An example of how to use
!> the input file type is provided below and in the test.
!>
!> @par Module Dependencies
!>  - ISO_FORTRAN_ENV: Intrinsic Fortran 2003 module providing some useful I/O
!>                     variables.
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref FileType_Fortran "FileType_Fortran": @copybrief FileType_Fortran
!>
!> @par EXAMPLES
!> @code
!> PROGRAM InpFileExample
!>
!> USE IOutil
!> IMPLICIT NONE
!>
!> TYPE(InputFileType) :: inpfile
!> TYPE(StringType) :: oneline
!>
!> !Initialize the log file for errors
!> CALL inpfileg%initialize(UNIT=50,FILE='test.inp')
!>
!> !Open the file
!> CALL inpfile%fopen()
!>
!> !Set an output file for echoing the input
!> CALL inpfile%setEchoUnit(25)
!> CALL inpfile%setEchoStat(.TRUE.)
!>
!> !Get a line of text from the input file
!> CALL inpfile%fgetl(oneline)
!>
!> ! ... routines to parse one line of input ...
!>
!> !Delete the file and clear the input file object
!> CALL inpfile%clear(.TRUE.)
!>
!> ENDPROGRAM
!> @endcode
!>
!> @author Brendan Kochunas
!>   @date 08/21/2011
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FileType_Input

  USE ISO_FORTRAN_ENV
  USE IntrType
  USE Strings
  USE FileType_Fortran
  IMPLICIT NONE
  PRIVATE

  !List of Public Members
  PUBLIC :: InputFileType

  !> Module name for error messages
  CHARACTER(LEN=*),PARAMETER :: modName='FILETYPE_INPUT'
  !> Scratch variable for IOSTAT values
  INTEGER(SIK) :: ioerr

  !> @brief Derived type object for an input file, it is an extension of the
  !> @ref FileType_Fortran "FortranFileType" object.
  !>
  !> This type provides two new attributes to the @ref FileType_Fortran
  !> "FortranFileType" for an echo status and an echo unit. It overwrites the
  !> methods for @ref FileType_Input::init_inp_file "initialize" and @ref
  !> FileType_Input::clear_inp_file "clear", and provides @ref
  !> FileType_Input::read_oneline "fgetl", @ref FileType_Input::echo_inp_file
  !> "setEchoStat", and @ref FileType_Input::isecho_inp_file "getEchoStat".
  TYPE,EXTENDS(FortranFileType) :: InputFileType
    !> Control variable for echoing messages to standard output.
    LOGICAL(SBK),PRIVATE :: echostat=.FALSE.
    !> Unit number for the file to echo result of fgetl to.
    INTEGER(SIK),PRIVATE :: echounit=-1
    !> The first character of oneline
    CHARACTER(LEN=1),PRIVATE :: probe=''
    !> The first character of the line before the line that was just read
    CHARACTER(LEN=1),PRIVATE :: lastprobe=''
!
!List of type bound procedures (methods) for the Input File type
    CONTAINS
      !> @copybrief FileType_Input::init_inp_file
      !> @copydetails FileType_Input::init_inp_file
      PROCEDURE,PASS :: initialize => init_inp_file
      !> @copybrief FileType_Input::clear_inp_file
      !> @copydetails FileType_Input::clear_inp_file
      PROCEDURE,PASS :: clear => clear_inp_file
      !> @copybrief FileType_Input::read_oneline_inp_file
      !> @copydetails FileType_Input::read_oneline_inp_file
      PROCEDURE,PASS :: fgetl => read_oneline_inp_file
      !> @copybrief FileType_Input::echo_inp_file
      !> @copydetails FileType_Input::echo_inp_file
      PROCEDURE,PASS :: setEchoStat => echo_inp_file
      !> @copybrief FileType_Input::isecho_inp_file
      !> @copydetails FileType_Input::isecho_inp_file
      PROCEDURE,PASS :: getEchoStat => isecho_inp_file
      !> @copybrief FileType_Input::setEchoUnit_inp_file
      !> @copydetails FileType_Input::setEchoUnit_inp_file
      PROCEDURE,PASS :: setEchoUnit => setEchoUnit_inp_file
      !> @copybrief FileType_Input::getEchoUnit_inp_file
      !> @copydetails FileType_Input::getEchoUnit_inp_file
      PROCEDURE,PASS :: getEchoUnit => getEchoUnit_inp_file
      !> @copybrief FileType_Input::getProbe_inp_file
      !> @copydetails FileType_Input::getProbe_inp_file
      PROCEDURE,PASS :: getProbe => getProbe_inp_file
      !> @copybrief FileType_Input::rewind_inp_file
      !> @copydetails FileType_Input::rewind_inp_file
      PROCEDURE,PASS :: frewind => rewind_inp_file
      !> @copybrief FileType_Input::backspace_inp_file
      !> @copydetails FileType_Input::backspace_inp_file
      PROCEDURE,PASS :: fbackspace => backspace_inp_file
  ENDTYPE InputFileType

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the input file object.
!> @param fileobj input file object.
!> @param unit Unit number to use for the file.
!> @param file Full path name of the file.
!> @param status Optional input is not used by this routine.
!> @param access Optional input is not used by this routine.
!> @param form Optional input is not used by this routine.
!> @param position Optional input is not used by this routine.
!> @param action Optional input is not used by this routine.
!> @param pad Optional input is not used by this routine.
!> @param recl Optional input is not used by this routine.
!>
    SUBROUTINE init_inp_file(fileobj,unit,file,status,access,form, &
                                 position,action,pad,recl)
      CHARACTER(LEN=*),PARAMETER :: myName='init_inp_file'
      CLASS(InputFileType),INTENT(INOUT) :: fileobj
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: unit
      CHARACTER(LEN=*),INTENT(IN) :: file
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: status
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: access
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: form
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: position
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: action
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: pad
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: recl

      IF(PRESENT(status)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
        ' - Optional input "STATUS" is being ignored. Value is "OLD".')
      IF(PRESENT(access)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
        ' - Optional input "ACCESS" is being ignored. Value is "SEQUENTIAL".')
      IF(PRESENT(form)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
        ' - Optional input "FORM" is being ignored. Value is "FORMATTED".')
      IF(PRESENT(action)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
        ' - Optional input "ACTION" is being ignored. Value is "READ".')
      IF(PRESENT(pad)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
        ' - Optional input "PAD" is being ignored. Value is "YES".')
      IF(PRESENT(position)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
        ' - Optional input "POSITION" is being ignored. Value is "REWIND".')
      IF(PRESENT(recl)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
        ' - Optional input "RECL" is being ignored. File is "SEQUENTIAL".')

      !Initialize the input file
      CALL init_fortran_file(fileobj,unit,file,'OLD','SEQUENTIAL', &
        'FORMATTED','REWIND','READ')
    ENDSUBROUTINE init_inp_file
!
!-------------------------------------------------------------------------------
!> @brief Rewinds an Input file
!> @param file input file object.
!>
!> This is needed to reset probe.
!>
    SUBROUTINE rewind_inp_file(file)
      CLASS(InputFileType),INTENT(INOUT) :: file
      file%probe=''
      file%lastprobe=''
      CALL rewind_fortran_file(file)
    ENDSUBROUTINE rewind_inp_file
!
!-------------------------------------------------------------------------------
!> @brief Backspaces an Input file
!> @param file input file object.
!>
!> This is needed to maintain the correct value for probe.
!>
    SUBROUTINE backspace_inp_file(file)
      CLASS(InputFileType),INTENT(INOUT) :: file
      CALL backspace_fortran_file(file)
      file%probe=file%lastprobe
    ENDSUBROUTINE backspace_inp_file
!
!-------------------------------------------------------------------------------
!> @brief Clears the log file object and resets its state to the unitialized
!> state.
!> @param file input file object.
!>
    SUBROUTINE clear_inp_file(file,ldel)
      CLASS(InputFileType),INTENT(INOUT) :: file
      LOGICAL(SBK),OPTIONAL,INTENT(IN) :: ldel
      LOGICAL(SBK) :: bool
      file%echounit=-1
      file%echostat=.FALSE.
      file%probe=''
      file%lastprobe=''
      bool=.FALSE.
      IF(PRESENT(ldel)) bool=ldel
      CALL clear_fortran_file(file,bool)
    ENDSUBROUTINE clear_inp_file
!
!-------------------------------------------------------------------------------
!> @brief Returns one line of text from the input file.
!> @param file input file object
!> @returns oneline StringType object storing a line of text from the input file.
!>
    SUBROUTINE read_oneline_inp_file(file,oneline)
      CHARACTER(LEN=*),PARAMETER :: myName='READ_ONELINE_INP_FILE'
      CLASS(InputFileType),INTENT(INOUT) :: file
      TYPE(StringType),INTENT(OUT) :: oneline
      CHARACTER(LEN=256) :: buffer
      CHARACTER(LEN=4) :: sioerr,sunit
      INTEGER(SIK) :: buffer_size,eioerr

      ioerr=0
      IF(file%isOpen() .AND. .NOT.file%isEOF()) THEN
        DO WHILE(ioerr /= IOSTAT_EOR .AND. ioerr /= IOSTAT_END)
          !Repeatedly read chunks of current input file line into buffer
          READ(UNIT=file%getUnitNo(),FMT='(a)',SIZE=buffer_size,ADVANCE='NO', &
            IOSTAT=ioerr) buffer
          IF(ioerr == IOSTAT_END) THEN
            !End of file
            CALL file%setEOFstat(.TRUE.)
          ELSEIF(ioerr == IOSTAT_EOR) THEN
            !Done reading line. Append last buffer to oneline.
            oneline=oneline//TRIM(buffer)
            file%lastprobe=file%probe
            IF(file%echostat) THEN
              WRITE(UNIT=file%echounit,FMT='(a)',IOSTAT=eioerr) TRIM(oneline)
              IF(eioerr /= 0) THEN
                WRITE(sioerr,'(i4)') eioerr; sioerr=ADJUSTL(sioerr)
                WRITE(sunit,'(i4)') file%echounit; sunit=ADJUSTL(sunit)
                CALL file%e%raiseError(modName//'::'//myName// &
                  ' - Error echoing oneline to UNIT='//TRIM(sunit) //' (IOSTAT='// &
                    TRIM(sioerr)//')!')
              ENDIF
            ENDIF
            oneline=TRIM(oneline)
          ELSEIF(ioerr < IOSTAT_EOR) THEN
            !Error reading line from input file
            WRITE(sioerr,'(i4)') ioerr; sioerr=ADJUSTL(sioerr)
            CALL file%e%raiseError(modName//'::'//myName// &
              ' - Error reading one line from input file (IOSTAT='// &
                TRIM(sioerr)//')!')
          ELSE
            !Still reading current line. Append buffer to oneline
            oneline=oneline//buffer
          ENDIF
        ENDDO
      ENDIF
      IF(ALLOCATED(oneline%s)) THEN
        file%probe=oneline%s(1)
      ELSE
        file%probe=''
        oneline=' '
      ENDIF
    ENDSUBROUTINE read_oneline_inp_file
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of the echo status for when a line is read
!> @param file input file object
!> @param bool the logical value to set for the echo status
!>
!> The echo status means that lines read will be echoed to a specified file
!> unit.
!>
    SUBROUTINE echo_inp_file(file,bool)
      CLASS(InputFileType),INTENT(INOUT) :: file
      LOGICAL(SBK),INTENT(IN) :: bool
      file%echostat=bool
    ENDSUBROUTINE echo_inp_file
!
!-------------------------------------------------------------------------------
!> @brief Returns the echo status of the input file object
!> @param file input file object
!> @returns bool the logical value of the echo status
!>
    PURE FUNCTION isecho_inp_file(file) RESULT(bool)
      CLASS(InputFileType),INTENT(IN) :: file
      LOGICAL(SBK) :: bool
      bool=file%echostat
    ENDFUNCTION isecho_inp_file
!
!-------------------------------------------------------------------------------
!> @brief Sets the value of the echounit attribute of the inputu file type
!> object.
!> @param file the input file object
!> @param iunit the unit number to use for the echo file.
!>
    SUBROUTINE setEchoUnit_inp_file(file,iunit)
      CHARACTER(LEN=*),PARAMETER :: myName='SETECHOUNIT_INP_FILE'
      CLASS(InputFileType),INTENT(INOUT) :: file
      INTEGER(SIK),INTENT(IN) :: iunit
      IF(0 < iunit .AND. iunit /= OUTPUT_UNIT .AND. iunit /= ERROR_UNIT) THEN
        file%echounit=iunit
      ELSE
        CALL file%e%raiseError('Incorrect input to '//modName//'::'// &
          myName//' - Illegal value for unit number!')
      ENDIF
    ENDSUBROUTINE setEchoUnit_inp_file
!
!-------------------------------------------------------------------------------
!> @brief Returns the value of the echounit attribute of the input file type
!> object.
!> @param file the input file object
!> @returns iunit the value of file%echounit
!>
    PURE FUNCTION getEchoUnit_inp_file(file) RESULT(iunit)
      CLASS(InputFileType),INTENT(IN) :: file
      INTEGER(SIK) :: iunit
      iunit=file%echounit
    ENDFUNCTION getEchoUnit_inp_file
!
!-------------------------------------------------------------------------------
!> @brief Returns the value of the PROBE attribute of the input file type
!> object.
!> @param file the input file object
!> @returns c the value of file%probe
!>
    PURE FUNCTION getProbe_inp_file(file) RESULT(c)
      CLASS(InputFileType),INTENT(IN) :: file
      CHARACTER(LEN=1) :: c

      c=file%probe

    ENDFUNCTION getProbe_inp_file
!
ENDMODULE FileType_Input
