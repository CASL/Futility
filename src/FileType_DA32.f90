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
!> @brief Moduled defines a direct access file type with 32-bit records.
!>
!> The implementation actually uses a programmable record size. Unless this file
!> is changed the actual record size of the file used in the implementation is
!> 1 KB (or 256 words, 1 word == 32-bits). The methods of the file however, 
!> allow the client code to read individual words within the 1 KB records. The
!> reason 1 KB records are used in this implementation is because it allows a
!> maximum addressable file of up to 2 TB. As one may guess, changing the 
!> WORDSREC parameter will change the maximum file size for all DA32 file types
!> and the maximum addressable space.
!>
!> Another subtlety to this implementation arises because the client may access
!> individual elements in the 1 KB blocks, writing to these blocks also requires
!> reading what is currently in the block. This can therefore create a possible
!> race condition if two processes try to write/modify the same 1 KB block.
!>
!> In general the DA32FileType provides 3 interfaces. One to initialize the
!> file, one to read data, and one to write data. The read/write interfaces
!> basically include 2 inputs, and 2 optional outputs. The input is the record
!> (as measured in 32-bit increments from 0) to begin reading or writing data,
!> and the variable that has data to be written or space for data to be read.
!> The output includes the IOSTAT value as would be returned from the READ and
!> WRITE intrinsics, and the number of records from the specified starting
!> address that were read from or written to. The read/write interfaces support
!> the following intrinsic data type and kinds:
!>  - Character arrays of default kind; CHARACTER(LEN=*)
!>  - Scalar or arrays of any dimension of logicals
!>  - Scalar or arrays of any dimension of 32-bit integers
!>  - Scalar or arrays of any dimension of 64-bit integers (longs)
!>  - Scalar or arrays of any dimension of 32-bit reals
!>  - Scalar or arrays of any dimension of 64-bit reals (doubles)
!>
!> When reading and writing array data the passed size of the dummy argument
!> determines the amount of data read from or written to the file. There are
!> no additional inputs for the amount of data to read or write.
!>
!> This implementation also makes use of r/w buffers to minimize the number
!> actual I/O statements that are executed. The size of the buffer should be
!> an integer multiple of WORDSREC, and unless it is changed the buffer size
!> is the size of 1 KB (the same as a record in the file).
!>
!> @author Brendan Kochunas
!>   @date 01/15/2014
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FileType_DA32
  USE ISO_FORTRAN_ENV
  USE IntrType
  USE ExceptionHandler
  USE IO_Strings
  USE FileType_Fortran
  
  IMPLICIT NONE
  PRIVATE
  
  !List of Public Members
  PUBLIC :: DA32FileType
  PUBLIC :: RECL32
  PUBLIC :: RECLSZ
  PUBLIC :: RECL_UNIT
  
  !> Module name for error messages
  CHARACTER(LEN=*),PARAMETER :: modName='FileType_DA32'
  
#ifdef __GFORTRAN__
  !> The units of the record length value
  CHARACTER(LEN=*),PARAMETER :: RECL_UNIT='BYTE'
  !> The size of a 32-bit record
  INTEGER(SIK),PARAMETER :: RECL32=4
#else
  !> The units of the record length value
  CHARACTER(LEN=*),PARAMETER :: RECL_UNIT='WORD'
  !> The size of a 32-bit record
  INTEGER(SIK),PARAMETER :: RECL32=1
#endif
  !> Number of words in a record
  INTEGER(SIK),PARAMETER :: WORDSREC=256
  !> Record size used in the operations of this implementation
  INTEGER(SIK),PARAMETER :: RECLSZ=WORDSREC*RECL32

  !> Size of buffers (in # of records) to use when reading/writing array data.
  !> KEEP AS A MULTIPLE OF 2!
  INTEGER(SIK),PARAMETER :: NBUF=WORDSREC
  !> Number of characters that can fit into a 32-bit record
  INTEGER(SIK),PARAMETER :: CH2REC=32/CHARACTER_STORAGE_SIZE
  !> The buffer size in characters.
  INTEGER(SIK),PARAMETER :: NBUFCH=NBUF*CH2REC
  !> The buffer size in number of doubles
  INTEGER(SIK),PARAMETER :: NBUFD=NBUF/2

  !> @brief Derived type object for Fortran file that is unformatted and
  !>        direct access with 32-bit records
  !>
  !> This is an extension of the abstract @ref FileType_Fortran 
  !> "FortranFileType". It has no additional attributes. It provides 2
  !> additional public methods which are generic interfaces for reading and
  !> writing data to the file.
  TYPE,EXTENDS(FortranFileType) :: DA32FileType
!
!List of type bound procedures (methods) for the Fortran File type
    CONTAINS
      !> @copybrief FileType_DA32::maxRecVal
      !> @copydetails FileType_DA32::maxRecVal
      PROCEDURE,NOPASS :: maxRecVal
      !> @copybrief FileType_DA32::getPad2NextBlk
      !> @copydetails FileType_DA32::getPad2NextBlk
      PROCEDURE,NOPASS :: getPad2NextBlk
      !> @copybrief FileType_DA32::init_DA32_file
      !> @copydetails FileType_DA32::init_DA32_file
      PROCEDURE,PASS :: initialize => init_DA32_file
      !> @copybrief FileType_DA32::getNextRec_DA32_file
      !> @copydetails FileType_DA32::getNextRec_DA32_file
      PROCEDURE,PASS :: getNextRec => getNextRec_DA32_file
      !> @copybrief FileType_DA32::readdat_char
      !> @copydetails FileType_DA32::readdat_char
      PROCEDURE,PASS,PRIVATE :: readdat_char
      !> @copybrief FileType_DA32::readdat_sbk0
      !> @copydetails FileType_DA32::readdat_sbk0
      PROCEDURE,PASS,PRIVATE :: readdat_sbk0
      !> @copybrief FileType_DA32::readdat_snk0
      !> @copydetails FileType_DA32::readdat_snk0
      PROCEDURE,PASS,PRIVATE :: readdat_snk0
      !> @copybrief FileType_DA32::readdat_slk0
      !> @copydetails FileType_DA32::readdat_slk0
      PROCEDURE,PASS,PRIVATE :: readdat_slk0
      !> @copybrief FileType_DA32::readdat_ssk0
      !> @copydetails FileType_DA32::readdat_ssk0
      PROCEDURE,PASS,PRIVATE :: readdat_ssk0
      !> @copybrief FileType_DA32::readdat_sdk0
      !> @copydetails FileType_DA32::readdat_sdk0
      PROCEDURE,PASS,PRIVATE :: readdat_sdk0
      !> @copybrief FileType_DA32::readdat_sbk1
      !> @copydetails FileType_DA32::readdat_sbk1
      PROCEDURE,PASS,PRIVATE :: readdat_sbk1
      !> @copybrief FileType_DA32::readdat_snk1
      !> @copydetails FileType_DA32::readdat_snk1
      PROCEDURE,PASS,PRIVATE :: readdat_snk1
      !> @copybrief FileType_DA32::readdat_slk1
      !> @copydetails FileType_DA32::readdat_slk1
      PROCEDURE,PASS,PRIVATE :: readdat_slk1
      !> @copybrief FileType_DA32::readdat_ssk1
      !> @copydetails FileType_DA32::readdat_ssk1
      PROCEDURE,PASS,PRIVATE :: readdat_ssk1
      !> @copybrief FileType_DA32::readdat_sdk1
      !> @copydetails FileType_DA32::readdat_sdk1
      PROCEDURE,PASS,PRIVATE :: readdat_sdk1
      !> @copybrief FileType_DA32::readdat_sbk2
      !> @copydetails FileType_DA32::readdat_sbk2
      PROCEDURE,PASS,PRIVATE :: readdat_sbk2
      !> @copybrief FileType_DA32::readdat_snk2
      !> @copydetails FileType_DA32::readdat_snk2
      PROCEDURE,PASS,PRIVATE :: readdat_snk2
      !> @copybrief FileType_DA32::readdat_slk2
      !> @copydetails FileType_DA32::readdat_slk2
      PROCEDURE,PASS,PRIVATE :: readdat_slk2
      !> @copybrief FileType_DA32::readdat_ssk2
      !> @copydetails FileType_DA32::readdat_ssk2
      PROCEDURE,PASS,PRIVATE :: readdat_ssk2
      !> @copybrief FileType_DA32::readdat_sdk2
      !> @copydetails FileType_DA32::readdat_sdk2
      PROCEDURE,PASS,PRIVATE :: readdat_sdk2
      !> @copybrief FileType_DA32::readdat_sbk3
      !> @copydetails FileType_DA32::readdat_sbk3
      PROCEDURE,PASS,PRIVATE :: readdat_sbk3
      !> @copybrief FileType_DA32::readdat_snk3
      !> @copydetails FileType_DA32::readdat_snk3
      PROCEDURE,PASS,PRIVATE :: readdat_snk3
      !> @copybrief FileType_DA32::readdat_slk3
      !> @copydetails FileType_DA32::readdat_slk3
      PROCEDURE,PASS,PRIVATE :: readdat_slk3
      !> @copybrief FileType_DA32::readdat_ssk3
      !> @copydetails FileType_DA32::readdat_ssk3
      PROCEDURE,PASS,PRIVATE :: readdat_ssk3
      !> @copybrief FileType_DA32::readdat_sdk3
      !> @copydetails FileType_DA32::readdat_sdk3
      PROCEDURE,PASS,PRIVATE :: readdat_sdk3
      !> @copybrief FileType_DA32::readdat_sbk4
      !> @copydetails FileType_DA32::readdat_sbk4
      PROCEDURE,PASS,PRIVATE :: readdat_sbk4
      !> @copybrief FileType_DA32::readdat_snk4
      !> @copydetails FileType_DA32::readdat_snk4
      PROCEDURE,PASS,PRIVATE :: readdat_snk4
      !> @copybrief FileType_DA32::readdat_slk4
      !> @copydetails FileType_DA32::readdat_slk4
      PROCEDURE,PASS,PRIVATE :: readdat_slk4
      !> @copybrief FileType_DA32::readdat_ssk4
      !> @copydetails FileType_DA32::readdat_ssk4
      PROCEDURE,PASS,PRIVATE :: readdat_ssk4
      !> @copybrief FileType_DA32::readdat_sdk4
      !> @copydetails FileType_DA32::readdat_sdk4
      PROCEDURE,PASS,PRIVATE :: readdat_sdk4
      !> @copybrief FileType_DA32::readdat_sbk5
      !> @copydetails FileType_DA32::readdat_sbk5
      PROCEDURE,PASS,PRIVATE :: readdat_sbk5
      !> @copybrief FileType_DA32::readdat_snk5
      !> @copydetails FileType_DA32::readdat_snk5
      PROCEDURE,PASS,PRIVATE :: readdat_snk5
      !> @copybrief FileType_DA32::readdat_slk5
      !> @copydetails FileType_DA32::readdat_slk5
      PROCEDURE,PASS,PRIVATE :: readdat_slk5
      !> @copybrief FileType_DA32::readdat_ssk5
      !> @copydetails FileType_DA32::readdat_ssk5
      PROCEDURE,PASS,PRIVATE :: readdat_ssk5
      !> @copybrief FileType_DA32::readdat_sdk5
      !> @copydetails FileType_DA32::readdat_sdk5
      PROCEDURE,PASS,PRIVATE :: readdat_sdk5
      !> @copybrief FileType_DA32::readdat_sbk6
      !> @copydetails FileType_DA32::readdat_sbk6
      PROCEDURE,PASS,PRIVATE :: readdat_sbk6
      !> @copybrief FileType_DA32::readdat_snk6
      !> @copydetails FileType_DA32::readdat_snk6
      PROCEDURE,PASS,PRIVATE :: readdat_snk6
      !> @copybrief FileType_DA32::readdat_slk6
      !> @copydetails FileType_DA32::readdat_slk6
      PROCEDURE,PASS,PRIVATE :: readdat_slk6
      !> @copybrief FileType_DA32::readdat_ssk6
      !> @copydetails FileType_DA32::readdat_ssk6
      PROCEDURE,PASS,PRIVATE :: readdat_ssk6
      !> @copybrief FileType_DA32::readdat_sdk6
      !> @copydetails FileType_DA32::readdat_sdk6
      PROCEDURE,PASS,PRIVATE :: readdat_sdk6
      !> @copybrief FileType_DA32::readdat_sbk4
      !> @copydetails FileType_DA32::readdat_sbk4
      PROCEDURE,PASS,PRIVATE :: readdat_sbk7
      !> @copybrief FileType_DA32::readdat_snk7
      !> @copydetails FileType_DA32::readdat_snk7
      PROCEDURE,PASS,PRIVATE :: readdat_snk7
      !> @copybrief FileType_DA32::readdat_slk7
      !> @copydetails FileType_DA32::readdat_slk7
      PROCEDURE,PASS,PRIVATE :: readdat_slk7
      !> @copybrief FileType_DA32::readdat_ssk7
      !> @copydetails FileType_DA32::readdat_ssk7
      PROCEDURE,PASS,PRIVATE :: readdat_ssk7
      !> @copybrief FileType_DA32::readdat_sdk7
      !> @copydetails FileType_DA32::readdat_sdk7
      PROCEDURE,PASS,PRIVATE :: readdat_sdk7
      !> Generic interface for reading data of an intrinsic type and kind
      GENERIC :: readdat => readdat_char,readdat_sbk0,readdat_snk0, &
                            readdat_slk0,readdat_ssk0,readdat_sdk0, &
                            readdat_sbk1,readdat_snk1,readdat_slk1, &
                            readdat_ssk1,readdat_sdk1,readdat_sbk2, &
                            readdat_snk2,readdat_slk2,readdat_ssk2, &
                            readdat_sdk2,readdat_sbk3,readdat_snk3, &
                            readdat_slk3,readdat_ssk3,readdat_sdk3, &
                            readdat_sbk4,readdat_snk4,readdat_slk4, &
                            readdat_ssk4,readdat_sdk4,readdat_sbk5, &
                            readdat_snk5,readdat_slk5,readdat_ssk5, &
                            readdat_sdk5,readdat_sbk6,readdat_snk6, &
                            readdat_slk6,readdat_ssk6,readdat_sdk6, &
                            readdat_sbk7,readdat_snk7,readdat_slk7, &
                            readdat_ssk7,readdat_sdk7
      !> @copybrief FileType_DA32::writedat_char
      !> @copydetails FileType_DA32::writedat_char
      PROCEDURE,PASS,PRIVATE :: writedat_char
      !> @copybrief FileType_DA32::writedat_sbk0
      !> @copydetails FileType_DA32::writedat_sbk0
      PROCEDURE,PASS,PRIVATE :: writedat_sbk0
      !> @copybrief FileType_DA32::writedat_snk0
      !> @copydetails FileType_DA32::writedat_snk0
      PROCEDURE,PASS,PRIVATE :: writedat_snk0
      !> @copybrief FileType_DA32::writedat_slk0
      !> @copydetails FileType_DA32::writedat_slk0
      PROCEDURE,PASS,PRIVATE :: writedat_slk0
      !> @copybrief FileType_DA32::writedat_ssk0
      !> @copydetails FileType_DA32::writedat_ssk0
      PROCEDURE,PASS,PRIVATE :: writedat_ssk0
      !> @copybrief FileType_DA32::writedat_sdk0
      !> @copydetails FileType_DA32::writedat_sdk0
      PROCEDURE,PASS,PRIVATE :: writedat_sdk0
      !> @copybrief FileType_DA32::writedat_sbk1
      !> @copydetails FileType_DA32::writedat_sbk1
      PROCEDURE,PASS,PRIVATE :: writedat_sbk1
      !> @copybrief FileType_DA32::writedat_snk1
      !> @copydetails FileType_DA32::writedat_snk1
      PROCEDURE,PASS,PRIVATE :: writedat_snk1
      !> @copybrief FileType_DA32::writedat_slk1
      !> @copydetails FileType_DA32::writedat_slk1
      PROCEDURE,PASS,PRIVATE :: writedat_slk1
      !> @copybrief FileType_DA32::writedat_ssk1
      !> @copydetails FileType_DA32::writedat_ssk1
      PROCEDURE,PASS,PRIVATE :: writedat_ssk1
      !> @copybrief FileType_DA32::writedat_sdk1
      !> @copydetails FileType_DA32::writedat_sdk1
      PROCEDURE,PASS,PRIVATE :: writedat_sdk1
      !> @copybrief FileType_DA32::writedat_sbk2
      !> @copydetails FileType_DA32::writedat_sbk2
      PROCEDURE,PASS,PRIVATE :: writedat_sbk2
      !> @copybrief FileType_DA32::writedat_snk2
      !> @copydetails FileType_DA32::writedat_snk2
      PROCEDURE,PASS,PRIVATE :: writedat_snk2
      !> @copybrief FileType_DA32::writedat_slk2
      !> @copydetails FileType_DA32::writedat_slk2
      PROCEDURE,PASS,PRIVATE :: writedat_slk2
      !> @copybrief FileType_DA32::writedat_ssk2
      !> @copydetails FileType_DA32::writedat_ssk2
      PROCEDURE,PASS,PRIVATE :: writedat_ssk2
      !> @copybrief FileType_DA32::writedat_sdk2
      !> @copydetails FileType_DA32::writedat_sdk2
      PROCEDURE,PASS,PRIVATE :: writedat_sdk2
      !> @copybrief FileType_DA32::writedat_sbk3
      !> @copydetails FileType_DA32::writedat_sbk3
      PROCEDURE,PASS,PRIVATE :: writedat_sbk3
      !> @copybrief FileType_DA32::writedat_snk3
      !> @copydetails FileType_DA32::writedat_snk3
      PROCEDURE,PASS,PRIVATE :: writedat_snk3
      !> @copybrief FileType_DA32::writedat_slk3
      !> @copydetails FileType_DA32::writedat_slk3
      PROCEDURE,PASS,PRIVATE :: writedat_slk3
      !> @copybrief FileType_DA32::writedat_ssk3
      !> @copydetails FileType_DA32::writedat_ssk3
      PROCEDURE,PASS,PRIVATE :: writedat_ssk3
      !> @copybrief FileType_DA32::writedat_sdk3
      !> @copydetails FileType_DA32::writedat_sdk3
      PROCEDURE,PASS,PRIVATE :: writedat_sdk3
      !> @copybrief FileType_DA32::writedat_sbk4
      !> @copydetails FileType_DA32::writedat_sbk4
      PROCEDURE,PASS,PRIVATE :: writedat_sbk4
      !> @copybrief FileType_DA32::writedat_snk4
      !> @copydetails FileType_DA32::writedat_snk4
      PROCEDURE,PASS,PRIVATE :: writedat_snk4
      !> @copybrief FileType_DA32::writedat_slk4
      !> @copydetails FileType_DA32::writedat_slk4
      PROCEDURE,PASS,PRIVATE :: writedat_slk4
      !> @copybrief FileType_DA32::writedat_ssk4
      !> @copydetails FileType_DA32::writedat_ssk4
      PROCEDURE,PASS,PRIVATE :: writedat_ssk4
      !> @copybrief FileType_DA32::writedat_sdk4
      !> @copydetails FileType_DA32::writedat_sdk4
      PROCEDURE,PASS,PRIVATE :: writedat_sdk4
      !> @copybrief FileType_DA32::writedat_sbk5
      !> @copydetails FileType_DA32::writedat_sbk5
      PROCEDURE,PASS,PRIVATE :: writedat_sbk5
      !> @copybrief FileType_DA32::writedat_snk5
      !> @copydetails FileType_DA32::writedat_snk5
      PROCEDURE,PASS,PRIVATE :: writedat_snk5
      !> @copybrief FileType_DA32::writedat_slk5
      !> @copydetails FileType_DA32::writedat_slk5
      PROCEDURE,PASS,PRIVATE :: writedat_slk5
      !> @copybrief FileType_DA32::writedat_ssk5
      !> @copydetails FileType_DA32::writedat_ssk5
      PROCEDURE,PASS,PRIVATE :: writedat_ssk5
      !> @copybrief FileType_DA32::writedat_sdk5
      !> @copydetails FileType_DA32::writedat_sdk5
      PROCEDURE,PASS,PRIVATE :: writedat_sdk5
      !> @copybrief FileType_DA32::writedat_sbk6
      !> @copydetails FileType_DA32::writedat_sbk6
      PROCEDURE,PASS,PRIVATE :: writedat_sbk6
      !> @copybrief FileType_DA32::writedat_snk6
      !> @copydetails FileType_DA32::writedat_snk6
      PROCEDURE,PASS,PRIVATE :: writedat_snk6
      !> @copybrief FileType_DA32::writedat_slk6
      !> @copydetails FileType_DA32::writedat_slk6
      PROCEDURE,PASS,PRIVATE :: writedat_slk6
      !> @copybrief FileType_DA32::writedat_ssk6
      !> @copydetails FileType_DA32::writedat_ssk6
      PROCEDURE,PASS,PRIVATE :: writedat_ssk6
      !> @copybrief FileType_DA32::writedat_sdk6
      !> @copydetails FileType_DA32::writedat_sdk6
      PROCEDURE,PASS,PRIVATE :: writedat_sdk6
      !> @copybrief FileType_DA32::writedat_sbk4
      !> @copydetails FileType_DA32::writedat_sbk4
      PROCEDURE,PASS,PRIVATE :: writedat_sbk7
      !> @copybrief FileType_DA32::writedat_snk7
      !> @copydetails FileType_DA32::writedat_snk7
      PROCEDURE,PASS,PRIVATE :: writedat_snk7
      !> @copybrief FileType_DA32::writedat_slk7
      !> @copydetails FileType_DA32::writedat_slk7
      PROCEDURE,PASS,PRIVATE :: writedat_slk7
      !> @copybrief FileType_DA32::writedat_ssk7
      !> @copydetails FileType_DA32::writedat_ssk7
      PROCEDURE,PASS,PRIVATE :: writedat_ssk7
      !> @copybrief FileType_DA32::writedat_sdk7
      !> @copydetails FileType_DA32::writedat_sdk7
      PROCEDURE,PASS,PRIVATE :: writedat_sdk7
      !> Generic interface for writing data of an intrinsic type and kind
      GENERIC :: writedat => writedat_char,writedat_sbk0,writedat_snk0, &
                             writedat_slk0,writedat_ssk0,writedat_sdk0, &
                             writedat_sbk1,writedat_snk1,writedat_slk1, &
                             writedat_ssk1,writedat_sdk1,writedat_sbk2, &
                             writedat_snk2,writedat_slk2,writedat_ssk2, &
                             writedat_sdk2,writedat_sbk3,writedat_snk3, &
                             writedat_slk3,writedat_ssk3,writedat_sdk3, &
                             writedat_sbk4,writedat_snk4,writedat_slk4, &
                             writedat_ssk4,writedat_sdk4,writedat_sbk5, &
                             writedat_snk5,writedat_slk5,writedat_ssk5, &
                             writedat_sdk5,writedat_sbk6,writedat_snk6, &
                             writedat_slk6,writedat_ssk6,writedat_sdk6, &
                             writedat_sbk7,writedat_snk7,writedat_slk7, &
                             writedat_ssk7,writedat_sdk7
  ENDTYPE DA32FileType
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Returns the value of the maximum addressable record (or word) in any
!>        DA32FileType
!> @returns val the maximum record value
!>
    PURE FUNCTION maxRecVal() RESULT(val)
      INTEGER(SLK) :: val
      val=INT(HUGE(1_SNK),SLK)*INT(WORDSREC,SLK)
    ENDFUNCTION maxRecVal
!
!-------------------------------------------------------------------------------
!> @brief Returns the value of the maximum addressable record (or word) in any
!>        DA32FileType
!> @param irec a positive integer (64-bit)
!> @returns pad the number of records to the first record of the next block
!>
!> e.g. irec+pad is the first record of the next block of the file.
!>
    PURE FUNCTION getPad2NextBlk(irec) RESULT(pad)
      INTEGER(SLK),INTENT(IN) :: irec
      INTEGER(SIK) :: pad
      pad=-1
      IF(irec > 0) THEN
        pad=WORDSREC-INT(MOD(irec-1,INT(WORDSREC,SLK)),SIK)
        IF(pad == INT(WORDSREC,SLK)) pad=0
      ENDIF
    ENDFUNCTION getPad2NextBlk
!
!-------------------------------------------------------------------------------
!> @brief Initializes a binary direct access file object.
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
!> The record size of the underlying implementation can be arbitrary. Here it
!> is chosen to be 1KB which allows for a 2TB file using 32-bit integers for
!> the records.
!>
!> Bottom line is the other interfaces allow the client code to reference
!> data within the file by 
!>
    SUBROUTINE init_DA32_file(fileobj,unit,file,status,access,form, &
                              position,action,pad,recl)
      CHARACTER(LEN=*),PARAMETER :: myName='init_DA32_file'
      CLASS(DA32FileType),INTENT(INOUT) :: fileobj
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: unit
      CHARACTER(LEN=*),INTENT(IN) :: file
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: status
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: access
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: form
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: position
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: action
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: pad
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: recl
      CHARACTER(LEN=9) :: actionval
      
      IF(PRESENT(access)) CALL fileobj%e%raiseWarning(modName//'::'//myName// &
        ' - Optional argument "ACCESS='''//TRIM(ADJUSTL(access))// &
          '''" is being ignored. DA32 File type is direct access.')
      
      IF(PRESENT(form)) CALL fileobj%e%raiseWarning(modName//'::'//myName// &
        ' - Optional argument "FORM='''//TRIM(ADJUSTL(form))// &
          '''" is being ignored. DA32 File type is unformatted.')
      
      IF(PRESENT(recl)) CALL fileobj%e%raiseWarning(modName//'::'//myName// &
        ' - Optional argument "RECL" is being ignored. '// &
          'DA32 File type has 1 kb records.')
      
      IF(PRESENT(action)) THEN
        actionval=action
        CALL toUPPER(actionval)
        IF(TRIM(actionval) == 'WRITE') actionval='READWRITE'
      ELSE
        actionval='READWRITE'
      ENDIF
      
      !Initialize the Direct Acces File using 1KB records. This allows for a
      !maximum file size of 2 TB.
      CALL init_fortran_file(fileobj,unit,file,status,'DIRECT', &
        'UNFORMATTED',position,actionval,pad,RECLSZ)
    ENDSUBROUTINE init_DA32_file
!
!-------------------------------------------------------------------------------
!> @brief
!> 
    FUNCTION getNextRec_DA32_file(thisDA32) RESULT(next_rec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK) :: next_rec
      next_rec=1
      IF(thisDA32%isInit()) THEN
        IF(thisDA32%isOpen()) THEN
          INQUIRE(thisDA32%getUnitNo(),NEXTREC=next_rec)
          next_rec=next_rec*INT(WORDSREC,SLK)
        ENDIF
      ENDIF
    ENDFUNCTION getNextRec_DA32_file
!
!-------------------------------------------------------------------------------
!> @brief Reads a stream of records from a direct access file into a buffer
!> @param thisDA32 the DA32 file type object to read data from
!> @param rec the record index to begin reading from
!> @param dat the data buffer to read data into
!> @param ioerr the IOSTAT value from the last read statement
!> @param nrec the number of records read into the buffer
!>
!> The buffer is a 32-bit integer array. All other read routines eventually 
!> call this routine.
!>
    SUBROUTINE readdat_basic(funit,iaddr,dat,ioerr,nword)
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SLK),INTENT(IN) :: iaddr
      INTEGER(SIK),INTENT(OUT) :: ioerr
      INTEGER(SLK),INTENT(INOUT) :: nword
      INTEGER(SNK),INTENT(OUT) :: dat(*)
      INTEGER(SLK) :: i,irec,istt,istp,iword,nrec,nread
      INTEGER(SNK) :: bufdat(WORDSREC)
      
      ioerr=0
      nread=0
      irec=(iaddr-1)/WORDSREC+1
      iword=MOD(iaddr-1_SLK,INT(WORDSREC,SLK))+1
      nrec=nword/WORDSREC
      IF(MOD(nword,INT(WORDSREC,SLK)) > 0) nrec=nrec+1
      IF(WORDSREC-iword+1 < nword) nrec=nrec+1
      
      READ(UNIT=funit,REC=irec,IOSTAT=ioerr) bufdat
      IF(ioerr == 0) THEN
        istt=1
        istp=MIN(nword,WORDSREC-iword+1)
        dat(istt:istp)=bufdat(iword:iword+istp-istt)
        nread=nread+istp-istt+1
        DO i=1,nrec-1
          READ(UNIT=funit,REC=irec+i,IOSTAT=ioerr) bufdat
          IF(ioerr /= 0) EXIT
          istt=istp+1
          istp=MIN(istt+WORDSREC-1,nword)
          dat(istt:istp)=bufdat(1:istp-istt+1)
          nread=nread+istp-istt+1
        ENDDO
      ENDIF
      nword=nread
    ENDSUBROUTINE readdat_basic
!
!-------------------------------------------------------------------------------
!> @brief Writes a stream of records from a buffer.
!> @param thisDA32 the DA32 file type object to write data from
!> @param rec the record index to begin writing at
!> @param dat the data buffer to write data from
!> @param ioerr the IOSTAT value from the last write statement
!> @param nrec the number of records written to the file
!>
!> The buffer is a 32-bit integer array. All other write routines eventually 
!> call this routine.
!>
    SUBROUTINE writedat_basic(funit,iaddr,dat,ioerr,nword)
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SLK),INTENT(IN) :: iaddr
      INTEGER(SIK),INTENT(OUT) :: ioerr
      INTEGER(SLK),INTENT(INOUT) :: nword
      INTEGER(SNK),INTENT(IN) :: dat(*)
      INTEGER(SLK) :: i,irec,istt,istp,iword,nrec,nwrite
      INTEGER(SNK) :: bufdat(WORDSREC)
            
      ioerr=0
      nwrite=0
      irec=(iaddr-1)/WORDSREC+1
      iword=MOD(iaddr-1_SLK,INT(WORDSREC,SLK))+1
      nrec=nword/WORDSREC
      IF(MOD(nword,INT(WORDSREC,SLK)) > 0) nrec=nrec+1
      IF(WORDSREC-iword+1 < nword) nrec=nrec+1
      
      bufdat=0
      IF(irec <= FileRecSize(funit)) &
        READ(UNIT=funit,REC=irec,IOSTAT=ioerr) bufdat
      IF(ioerr == 0) THEN
        istt=1
        istp=MIN(nword,INT(WORDSREC-iword+1,SLK))
        bufdat(iword:iword+istp-istt)=dat(istt:istp)
        WRITE(UNIT=funit,REC=irec,IOSTAT=ioerr) bufdat
        DO i=1,nrec-1
          IF(ioerr /= 0) EXIT
          nwrite=nwrite+istp-istt+1
          istt=istp+1
          istp=MIN(istt+WORDSREC-1_SLK,nword)
          IF(irec+i < FileRecSize(funit)) THEN
            READ(UNIT=funit,REC=irec+i,IOSTAT=ioerr) bufdat
            IF(ioerr /= 0) EXIT
          ELSE
            bufdat(istp-istt+1:WORDSREC)=0
          ENDIF
          bufdat(1:istp-istt+1)=dat(istt:istp)
          WRITE(UNIT=funit,REC=irec+i,IOSTAT=ioerr) bufdat
        ENDDO
        IF(ioerr == 0) THEN
          nwrite=nwrite+istp-istt+1
          FLUSH(funit)
        ENDIF
      ENDIF
      nword=nwrite
    ENDSUBROUTINE writedat_basic
!
!-------------------------------------------------------------------------------
!> @brief Writes character data to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_char(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      CHARACTER(LEN=*),INTENT(IN) :: dat
      
      INTEGER(SNK) :: tmpdat(NBUF)
      INTEGER(SIK) :: nlen,ioerr,istt,istp
      INTEGER(SLK) :: n,irec
      
      ioerr=IOSTAT_END
      irec=rec
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        nlen=LEN(dat)
        istt=1
        istp=MIN(nlen,NBUFCH)
        ioerr=0
        DO WHILE(nlen > 0)
          tmpdat=TRANSFER(dat(istt:istp),tmpdat)
          n=istp-istt+1
          n=n/CH2REC
          IF(MOD(istp-istt+1,CH2REC) > 0) n=n+1
          CALL writedat_basic(thisDA32%getUnitNo(),irec,tmpdat,ioerr,n)
          IF(ioerr /= 0) EXIT
          irec=irec+n
          nlen=nlen-NBUFCH
          istt=istp+1
          istp=MIN(nlen,NBUFCH)+istt-1
        ENDDO
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_char
!
!-------------------------------------------------------------------------------
!> @brief Writes a scalar logical to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sbk0(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(IN) :: dat
      
      INTEGER(SNK) :: tmpdat(1)
      INTEGER(SIK) :: ioerr
      INTEGER(SLK) :: n
      
      ioerr=IOSTAT_END
      n=0
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        n=1
        tmpdat=TRANSFER(dat,tmpdat)
        CALL writedat_basic(thisDA32%getUnitNo(),rec,tmpdat,ioerr,n)
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=n
    ENDSUBROUTINE writedat_sbk0
!
!-------------------------------------------------------------------------------
!> @brief Writes a scalar 32-bit integer to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_snk0(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(IN) :: dat
      
      INTEGER(SNK) :: tmpdat(1)
      INTEGER(SIK) :: ioerr
      INTEGER(SLK) :: n
      
      ioerr=IOSTAT_END
      n=0
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        n=1
        tmpdat=dat
        CALL writedat_basic(thisDA32%getUnitNo(),rec,tmpdat,ioerr,n)
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=n
    ENDSUBROUTINE writedat_snk0
!
!-------------------------------------------------------------------------------
!> @brief Writes a scalar 64-bit integer to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_slk0(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(IN) :: dat
      
      INTEGER(SNK) :: tmpdat(2)
      INTEGER(SIK) :: ioerr
      INTEGER(SLK) :: n
      
      ioerr=IOSTAT_END
      n=0
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        n=2
        tmpdat=TRANSFER(dat,tmpdat)
        CALL writedat_basic(thisDA32%getUnitNo(),rec,tmpdat,ioerr,n)
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=n
    ENDSUBROUTINE writedat_slk0
!
!-------------------------------------------------------------------------------
!> @brief Writes a scalar 32-bit real to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_ssk0(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(IN) :: dat
      
      INTEGER(SNK) :: tmpdat(1)
      INTEGER(SIK) :: ioerr
      INTEGER(SLK) :: n
      
      ioerr=IOSTAT_END
      n=0
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        n=1
        tmpdat=TRANSFER(dat,tmpdat)
        CALL writedat_basic(thisDA32%getUnitNo(),rec,tmpdat,ioerr,n)
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=n
    ENDSUBROUTINE writedat_ssk0
!
!-------------------------------------------------------------------------------
!> @brief Writes a scalar 64-bit real to a DA32 File starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sdk0(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(IN) :: dat
      
      INTEGER(SNK) :: tmpdat(2)
      INTEGER(SIK) :: ioerr
      INTEGER(SLK) :: n
      
      ioerr=IOSTAT_END
      n=0
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        n=2
        tmpdat=TRANSFER(dat,tmpdat)
        CALL writedat_basic(thisDA32%getUnitNo(),rec,tmpdat,ioerr,n)
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=n
    ENDSUBROUTINE writedat_sdk0
!
!-------------------------------------------------------------------------------
!> @brief Writes a 1-D array of logicals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sbk1(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(IN) :: dat(:)
      
      INTEGER(SNK) :: tmpdat(NBUF)
      INTEGER(SIK) :: ndat,ioerr,istt,istp
      INTEGER(SLK) :: n,irec
      
      ioerr=IOSTAT_END
      irec=rec
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        ndat=SIZE(dat)
        istt=1
        istp=MIN(ndat,NBUF)
        ioerr=0
        DO WHILE(ndat > 0)
          tmpdat=TRANSFER(dat(istt:istp),tmpdat)
          n=istp-istt+1
          CALL writedat_basic(thisDA32%getUnitNo(),irec,tmpdat,ioerr,n)
          IF(ioerr /= 0) EXIT
          irec=irec+n
          ndat=ndat-NBUF
          istt=istp+1
          istp=MIN(ndat,NBUF)+istt-1
        ENDDO
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sbk1
!
!-------------------------------------------------------------------------------
!> @brief Writes a 1-D array of 32-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_snk1(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(IN) :: dat(:)
      
      INTEGER(SNK) :: tmpdat(NBUF)
      INTEGER(SIK) :: ndat,ioerr,istt,istp
      INTEGER(SLK) :: n,irec
      
      ioerr=IOSTAT_END
      irec=rec
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        ndat=SIZE(dat)
        istt=1
        istp=MIN(ndat,NBUF)
        ioerr=0
        DO WHILE(ndat > 0)
          tmpdat(1:istp-istt+1)=dat(istt:istp)
          n=istp-istt+1
          CALL writedat_basic(thisDA32%getUnitNo(),irec,tmpdat,ioerr,n)
          IF(ioerr /= 0) EXIT
          irec=irec+n
          ndat=ndat-NBUF
          istt=istp+1
          istp=MIN(ndat,NBUF)+istt-1
        ENDDO
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_snk1
!
!-------------------------------------------------------------------------------
!> @brief Writes a 1-D array of 64-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_slk1(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(IN) :: dat(:)
      
      INTEGER(SNK) :: tmpdat(NBUF)
      INTEGER(SIK) :: ndat,ioerr,istt,istp
      INTEGER(SLK) :: n,irec
      
      ioerr=IOSTAT_END
      irec=rec
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        ndat=SIZE(dat)
        istt=1
        istp=MIN(ndat,NBUFD)
        ioerr=0
        DO WHILE(ndat > 0)
          tmpdat=TRANSFER(dat(istt:istp),tmpdat)
          n=istp-istt+1
          n=n*2
          CALL writedat_basic(thisDA32%getUnitNo(),irec,tmpdat,ioerr,n)
          IF(ioerr /= 0) EXIT
          irec=irec+n
          ndat=ndat-NBUFD
          istt=istp+1
          istp=MIN(ndat,NBUFD)+istt-1
        ENDDO
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_slk1
!
!-------------------------------------------------------------------------------
!> @brief Writes a 1-D array of 32-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_ssk1(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(IN) :: dat(:)
      
      INTEGER(SNK) :: tmpdat(NBUF)
      INTEGER(SIK) :: ndat,ioerr,istt,istp
      INTEGER(SLK) :: n,irec
      
      ioerr=IOSTAT_END
      irec=rec
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        ndat=SIZE(dat)
        istt=1
        istp=MIN(ndat,NBUF)
        ioerr=0
        DO WHILE(ndat > 0)
          tmpdat=TRANSFER(dat(istt:istp),tmpdat)
          n=istp-istt+1
          CALL writedat_basic(thisDA32%getUnitNo(),irec,tmpdat,ioerr,n)
          IF(ioerr /= 0) EXIT
          irec=irec+n
          ndat=ndat-NBUF
          istt=istp+1
          istp=MIN(ndat,NBUF)+istt-1
        ENDDO
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_ssk1
!
!-------------------------------------------------------------------------------
!> @brief Writes a 1-D array of 64-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sdk1(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(IN) :: dat(:)
      
      INTEGER(SNK) :: tmpdat(NBUF)
      INTEGER(SIK) :: ndat,ioerr,istt,istp
      INTEGER(SLK) :: n,irec
      
      ioerr=IOSTAT_END
      irec=rec
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        ndat=SIZE(dat)
        istt=1
        istp=MIN(ndat,NBUFD)
        ioerr=0
        DO WHILE(ndat > 0)
          tmpdat=TRANSFER(dat(istt:istp),tmpdat)
          n=istp-istt+1
          n=n*2
          CALL writedat_basic(thisDA32%getUnitNo(),irec,tmpdat,ioerr,n)
          IF(ioerr /= 0) EXIT
          irec=irec+n
          ndat=ndat-NBUFD
          istt=istp+1
          istp=MIN(ndat,NBUFD)+istt-1
        ENDDO
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sdk1
!
!-------------------------------------------------------------------------------
!> @brief Writes a 2-D array of logicals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sbk2(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(IN) :: dat(:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=2)
        CALL writedat_sbk1(thisDA32,irec,dat(:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sbk2
!
!-------------------------------------------------------------------------------
!> @brief Writes a 2-D array of 32-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_snk2(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(IN) :: dat(:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=2)
        CALL writedat_snk1(thisDA32,irec,dat(:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_snk2
!
!-------------------------------------------------------------------------------
!> @brief Writes a 2-D array of 64-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_slk2(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(IN) :: dat(:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=2)
        CALL writedat_slk1(thisDA32,irec,dat(:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_slk2
!
!-------------------------------------------------------------------------------
!> @brief Writes a 2-D array of 32-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_ssk2(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(IN) :: dat(:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=2)
        CALL writedat_ssk1(thisDA32,irec,dat(:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_ssk2
!
!-------------------------------------------------------------------------------
!> @brief Writes a 2-D array of 64-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sdk2(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(IN) :: dat(:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=2)
        CALL writedat_sdk1(thisDA32,irec,dat(:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sdk2
!
!-------------------------------------------------------------------------------
!> @brief Writes a 3-D array of logicals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sbk3(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(IN) :: dat(:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=3)
        CALL writedat_sbk2(thisDA32,irec,dat(:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sbk3
!
!-------------------------------------------------------------------------------
!> @brief Writes a 3-D array of 32-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_snk3(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(IN) :: dat(:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=3)
        CALL writedat_snk2(thisDA32,irec,dat(:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_snk3
!
!-------------------------------------------------------------------------------
!> @brief Writes a 3-D array of 64-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_slk3(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(IN) :: dat(:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=3)
        CALL writedat_slk2(thisDA32,irec,dat(:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_slk3
!
!-------------------------------------------------------------------------------
!> @brief Writes a 3-D array of 32-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_ssk3(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(IN) :: dat(:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=3)
        CALL writedat_ssk2(thisDA32,irec,dat(:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_ssk3
!
!-------------------------------------------------------------------------------
!> @brief Writes a 3-D array of 64-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sdk3(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(IN) :: dat(:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=3)
        CALL writedat_sdk2(thisDA32,irec,dat(:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sdk3
!
!-------------------------------------------------------------------------------
!> @brief Writes a 4-D array of logicals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sbk4(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(IN) :: dat(:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=4)
        CALL writedat_sbk3(thisDA32,irec,dat(:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sbk4
!
!-------------------------------------------------------------------------------
!> @brief Writes a 4-D array of 32-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_snk4(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(IN) :: dat(:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=4)
        CALL writedat_snk3(thisDA32,irec,dat(:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_snk4
!
!-------------------------------------------------------------------------------
!> @brief Writes a 4-D array of 64-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_slk4(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(IN) :: dat(:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=4)
        CALL writedat_slk3(thisDA32,irec,dat(:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_slk4
!
!-------------------------------------------------------------------------------
!> @brief Writes a 4-D array of 32-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_ssk4(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(IN) :: dat(:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=4)
        CALL writedat_ssk3(thisDA32,irec,dat(:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_ssk4
!
!-------------------------------------------------------------------------------
!> @brief Writes a 4-D array of 64-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sdk4(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(IN) :: dat(:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=4)
        CALL writedat_sdk3(thisDA32,irec,dat(:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sdk4
!
!-------------------------------------------------------------------------------
!> @brief Writes a 5-D array of logicals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sbk5(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(IN) :: dat(:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=5)
        CALL writedat_sbk4(thisDA32,irec,dat(:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sbk5
!
!-------------------------------------------------------------------------------
!> @brief Writes a 5-D array of 32-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_snk5(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(IN) :: dat(:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=5)
        CALL writedat_snk4(thisDA32,irec,dat(:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_snk5
!
!-------------------------------------------------------------------------------
!> @brief Writes a 5-D array of 64-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_slk5(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(IN) :: dat(:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=5)
        CALL writedat_slk4(thisDA32,irec,dat(:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_slk5
!
!-------------------------------------------------------------------------------
!> @brief Writes a 5-D array of 32-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_ssk5(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(IN) :: dat(:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=5)
        CALL writedat_ssk4(thisDA32,irec,dat(:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_ssk5
!
!-------------------------------------------------------------------------------
!> @brief Writes a 5-D array of 64-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sdk5(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(IN) :: dat(:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=5)
        CALL writedat_sdk4(thisDA32,irec,dat(:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sdk5
!
!-------------------------------------------------------------------------------
!> @brief Writes a 6-D array of logicals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sbk6(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(IN) :: dat(:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=6)
        CALL writedat_sbk5(thisDA32,irec,dat(:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sbk6
!
!-------------------------------------------------------------------------------
!> @brief Writes a 6-D array of 32-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_snk6(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(IN) :: dat(:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=6)
        CALL writedat_snk5(thisDA32,irec,dat(:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_snk6
!
!-------------------------------------------------------------------------------
!> @brief Writes a 6-D array of 64-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_slk6(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(IN) :: dat(:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=6)
        CALL writedat_slk5(thisDA32,irec,dat(:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_slk6
!
!-------------------------------------------------------------------------------
!> @brief Writes a 6-D array of 32-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_ssk6(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(IN) :: dat(:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=6)
        CALL writedat_ssk5(thisDA32,irec,dat(:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_ssk6
!
!-------------------------------------------------------------------------------
!> @brief Writes a 6-D array of 64-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sdk6(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(IN) :: dat(:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=6)
        CALL writedat_sdk5(thisDA32,irec,dat(:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sdk6
!
!-------------------------------------------------------------------------------
!> @brief Writes a 7-D array of logicals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sbk7(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(IN) :: dat(:,:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=7)
        CALL writedat_sbk6(thisDA32,irec,dat(:,:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sbk7
!
!-------------------------------------------------------------------------------
!> @brief Writes a 7-D array of 32-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_snk7(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(IN) :: dat(:,:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=7)
        CALL writedat_snk6(thisDA32,irec,dat(:,:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_snk7
!
!-------------------------------------------------------------------------------
!> @brief Writes a 7-D array of 64-bit integers to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_slk7(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(IN) :: dat(:,:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=7)
        CALL writedat_slk6(thisDA32,irec,dat(:,:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_slk7
!
!-------------------------------------------------------------------------------
!> @brief Writes a 7-D array of 32-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_ssk7(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(IN) :: dat(:,:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=7)
        CALL writedat_ssk6(thisDA32,irec,dat(:,:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_ssk7
!
!-------------------------------------------------------------------------------
!> @brief Writes a 7-D array of 64-bit reals to a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start writing
!> @param dat the data to be written
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records occupied by dat
!>        in the file
!>
    SUBROUTINE writedat_sdk7(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(INOUT) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(IN) :: dat(:,:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=7)
        CALL writedat_sdk6(thisDA32,irec,dat(:,:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE writedat_sdk7
!
!-------------------------------------------------------------------------------
!> @brief Reads character data from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_char(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      CHARACTER(LEN=*),INTENT(OUT) :: dat
      
      INTEGER(SNK) :: tmpdat(NBUF)
      INTEGER(SIK) :: nlen,ioerr,istt,istp,i
      INTEGER(SLK) :: irec,n
      
      ioerr=IOSTAT_END
      irec=rec
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        nlen=LEN(dat)
        istt=1
        istp=MIN(nlen,NBUFCH)
        ioerr=0
        DO WHILE(nlen > 0)
          n=istp-istt+1
          n=n/CH2REC
          IF(MOD(istp-istt+1,CH2REC) > 0) n=n+1
          CALL readdat_basic(thisDA32%getUnitNo(),irec,tmpdat,ioerr,n)
          IF(ioerr /= 0) EXIT
          dat(istt:istp)=TRANSFER(tmpdat(1:n),dat)
          irec=irec+n
          nlen=nlen-NBUFCH
          istt=istp+1
          istp=MIN(nlen,NBUFCH)+istt-1
        ENDDO
        
        !The transfer intrinsic generates 0 (or C NULL CHARS) for records
        !that were not complete. This replaces the trailing NULL chars with
        !spaces to facilitate the use of TRIM on the returned character string.
        IF(ioerr == 0) THEN
          DO i=1,LEN(dat)
            IF(ICHAR(dat(i:i)) == 0) dat(i:i)=ACHAR(32)
          ENDDO
        ENDIF
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_char
!
!-------------------------------------------------------------------------------
!> @brief Reads a scalar logical from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sbk0(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(OUT) :: dat
      
      INTEGER(SNK) :: tmpdat(1)
      INTEGER(SIK) :: ioerr
      INTEGER(SLK) :: n
      
      n=1
      CALL readdat_basic(thisDA32%getUnitNo(),rec,tmpdat,ioerr,n)
      dat=TRANSFER(tmpdat,dat)
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=n
    ENDSUBROUTINE readdat_sbk0
!
!-------------------------------------------------------------------------------
!> @brief Reads a scalar 32-bit integer from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_snk0(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(OUT) :: dat
      
      INTEGER(SNK) :: tmpdat(1)
      INTEGER(SIK) :: ioerr
      INTEGER(SLK) :: n
      
      n=1
      CALL readdat_basic(thisDA32%getUnitNo(),rec,tmpdat,ioerr,n)
      dat=tmpdat(1)
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=n
    ENDSUBROUTINE readdat_snk0
!
!-------------------------------------------------------------------------------
!> @brief Reads a scalar 64-bit integer from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_slk0(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(OUT) :: dat
      
      INTEGER(SNK) :: tmpdat(2)
      INTEGER(SIK) :: ioerr
      INTEGER(SLK) :: n
      
      n=2
      CALL readdat_basic(thisDA32%getUnitNo(),rec,tmpdat,ioerr,n)
      dat=TRANSFER(tmpdat,dat)
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=n
    ENDSUBROUTINE readdat_slk0
!
!-------------------------------------------------------------------------------
!> @brief Reads a scalar 32-bit real from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_ssk0(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(OUT) :: dat
      
      INTEGER(SNK) :: tmpdat(1)
      INTEGER(SIK) :: ioerr
      INTEGER(SLK) :: n
      
      n=1
      CALL readdat_basic(thisDA32%getUnitNo(),rec,tmpdat,ioerr,n)
      dat=TRANSFER(tmpdat,dat)
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=n
    ENDSUBROUTINE readdat_ssk0
!
!-------------------------------------------------------------------------------
!> @brief Reads a scalar 64-bit real from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sdk0(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(OUT) :: dat
      
      INTEGER(SNK) :: tmpdat(2)
      INTEGER(SIK) :: ioerr
      INTEGER(SLK) :: n
      
      n=2
      CALL readdat_basic(thisDA32%getUnitNo(),rec,tmpdat,ioerr,n)
      dat=TRANSFER(tmpdat,dat)
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=n
    ENDSUBROUTINE readdat_sdk0
!
!-------------------------------------------------------------------------------
!> @brief Reads a 1-D logical array from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sbk1(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(INOUT) :: dat(:)
      
      INTEGER(SNK) :: tmpdat(NBUF)
      INTEGER(SIK) :: ndat,ioerr,istt,istp
      INTEGER(SLK) :: n,irec
      
      ioerr=IOSTAT_END
      irec=rec
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        ndat=SIZE(dat)
        istt=1
        istp=MIN(ndat,NBUF)
        ioerr=0
        DO WHILE(ndat > 0)
          n=istp-istt+1
          CALL readdat_basic(thisDA32%getUnitNo(),irec,tmpdat,ioerr,n)
          IF(ioerr /= 0) EXIT
          dat(istt:istp)=TRANSFER(tmpdat(1:n),dat)
          irec=irec+n
          ndat=ndat-NBUF
          istt=istp+1
          istp=MIN(ndat,NBUF)+istt-1
        ENDDO
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sbk1
!
!-------------------------------------------------------------------------------
!> @brief Reads a 1-D array of 32-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_snk1(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(INOUT) :: dat(:)
      
      INTEGER(SNK) :: tmpdat(NBUF)
      INTEGER(SIK) :: ndat,ioerr,istt,istp
      INTEGER(SLK) :: n,irec
      
      ioerr=IOSTAT_END
      irec=rec
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        ndat=SIZE(dat)
        istt=1
        istp=MIN(ndat,NBUF)
        ioerr=0
        DO WHILE(ndat > 0)
          n=istp-istt+1
          CALL readdat_basic(thisDA32%getUnitNo(),irec,tmpdat,ioerr,n)
          IF(ioerr /= 0) EXIT
          dat(istt:istp)=tmpdat(1:n)
          irec=irec+n
          ndat=ndat-NBUF
          istt=istp+1
          istp=MIN(ndat,NBUF)+istt-1
        ENDDO
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_snk1
!
!-------------------------------------------------------------------------------
!> @brief Reads a 1-D array of 64-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_slk1(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(INOUT) :: dat(:)
      
      INTEGER(SNK) :: tmpdat(NBUF)
      INTEGER(SIK) :: ndat,ioerr,istt,istp
      INTEGER(SLK) :: n,irec
      
      ioerr=IOSTAT_END
      irec=rec
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        ndat=SIZE(dat)
        istt=1
        istp=MIN(ndat,NBUFD)
        ioerr=0
        DO WHILE(ndat > 0)
          n=istp-istt+1
          n=n*2
          CALL readdat_basic(thisDA32%getUnitNo(),irec,tmpdat,ioerr,n)
          IF(ioerr /= 0) EXIT
          dat(istt:istp)=TRANSFER(tmpdat(1:n),dat)
          irec=irec+n
          ndat=ndat-NBUFD
          istt=istp+1
          istp=MIN(ndat,NBUFD)+istt-1
        ENDDO
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_slk1
!
!-------------------------------------------------------------------------------
!> @brief Reads a 1-D array of 32-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_ssk1(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(INOUT) :: dat(:)
      
      INTEGER(SNK) :: tmpdat(NBUF)
      INTEGER(SIK) :: ndat,ioerr,istt,istp
      INTEGER(SLK) :: n,irec
      
      ioerr=IOSTAT_END
      irec=rec
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        ndat=SIZE(dat)
        istt=1
        istp=MIN(ndat,NBUF)
        ioerr=0
        DO WHILE(ndat > 0)
          n=istp-istt+1
          CALL readdat_basic(thisDA32%getUnitNo(),irec,tmpdat,ioerr,n)
          IF(ioerr /= 0) EXIT
          dat(istt:istp)=TRANSFER(tmpdat(1:n),dat)
          irec=irec+n
          ndat=ndat-NBUF
          istt=istp+1
          istp=MIN(ndat,NBUF)+istt-1
        ENDDO
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_ssk1
!
!-------------------------------------------------------------------------------
!> @brief Reads a 1-D array of 64-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sdk1(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(INOUT) :: dat(:)
      
      INTEGER(SNK) :: tmpdat(NBUF)
      INTEGER(SIK) :: ndat,ioerr,istt,istp
      INTEGER(SLK) :: n,irec
      
      ioerr=IOSTAT_END
      irec=rec
      IF(thisDA32%isInit() .AND. thisDA32%isOpen()) THEN
        ndat=SIZE(dat)
        istt=1
        istp=MIN(ndat,NBUFD)
        ioerr=0
        DO WHILE(ndat > 0)
          n=istp-istt+1
          n=n*2
          CALL readdat_basic(thisDA32%getUnitNo(),irec,tmpdat,ioerr,n)
          IF(ioerr /= 0) EXIT
          dat(istt:istp)=TRANSFER(tmpdat(1:n),dat)
          irec=irec+n
          ndat=ndat-NBUFD
          istt=istp+1
          istp=MIN(ndat,NBUFD)+istt-1
        ENDDO
      ENDIF
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sdk1
!
!-------------------------------------------------------------------------------
!> @brief Reads a 2-D logical array from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sbk2(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(INOUT) :: dat(:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=2)
        CALL readdat_sbk1(thisDA32,irec,dat(:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sbk2
!
!-------------------------------------------------------------------------------
!> @brief Reads a 2-D array of 32-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_snk2(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(INOUT) :: dat(:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=2)
        CALL readdat_snk1(thisDA32,irec,dat(:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_snk2
!
!-------------------------------------------------------------------------------
!> @brief Reads a 2-D array of 64-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_slk2(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(INOUT) :: dat(:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=2)
        CALL readdat_slk1(thisDA32,irec,dat(:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_slk2
!
!-------------------------------------------------------------------------------
!> @brief Reads a 2-D array of 32-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_ssk2(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(INOUT) :: dat(:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=2)
        CALL readdat_ssk1(thisDA32,irec,dat(:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_ssk2
!
!-------------------------------------------------------------------------------
!> @brief Reads a 2-D array of 64-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sdk2(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(INOUT) :: dat(:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=2)
        CALL readdat_sdk1(thisDA32,irec,dat(:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sdk2
!
!-------------------------------------------------------------------------------
!> @brief Reads a 3-D logical array from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sbk3(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(INOUT) :: dat(:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=3)
        CALL readdat_sbk2(thisDA32,irec,dat(:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sbk3
!
!-------------------------------------------------------------------------------
!> @brief Reads a 3-D array of 32-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_snk3(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(INOUT) :: dat(:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=3)
        CALL readdat_snk2(thisDA32,irec,dat(:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_snk3
!
!-------------------------------------------------------------------------------
!> @brief Reads a 3-D array of 64-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_slk3(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(INOUT) :: dat(:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=3)
        CALL readdat_slk2(thisDA32,irec,dat(:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_slk3
!
!-------------------------------------------------------------------------------
!> @brief Reads a 3-D array of 32-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_ssk3(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(INOUT) :: dat(:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=3)
        CALL readdat_ssk2(thisDA32,irec,dat(:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_ssk3
!
!-------------------------------------------------------------------------------
!> @brief Reads a 3-D array of 64-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sdk3(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(INOUT) :: dat(:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=3)
        CALL readdat_sdk2(thisDA32,irec,dat(:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sdk3
!
!-------------------------------------------------------------------------------
!> @brief Reads a 4-D logical array from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sbk4(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(INOUT) :: dat(:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=4)
        CALL readdat_sbk3(thisDA32,irec,dat(:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sbk4
!
!-------------------------------------------------------------------------------
!> @brief Reads a 4-D array of 32-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_snk4(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(INOUT) :: dat(:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=4)
        CALL readdat_snk3(thisDA32,irec,dat(:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_snk4
!
!-------------------------------------------------------------------------------
!> @brief Reads a 4-D array of 64-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_slk4(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(INOUT) :: dat(:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=4)
        CALL readdat_slk3(thisDA32,irec,dat(:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_slk4
!
!-------------------------------------------------------------------------------
!> @brief Reads a 4-D array of 32-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_ssk4(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(INOUT) :: dat(:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=4)
        CALL readdat_ssk3(thisDA32,irec,dat(:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_ssk4
!
!-------------------------------------------------------------------------------
!> @brief Reads a 4-D array of 64-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sdk4(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(INOUT) :: dat(:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=4)
        CALL readdat_sdk3(thisDA32,irec,dat(:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sdk4
!
!-------------------------------------------------------------------------------
!> @brief Reads a 5-D logical array from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sbk5(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(INOUT) :: dat(:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=5)
        CALL readdat_sbk4(thisDA32,irec,dat(:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sbk5
!
!-------------------------------------------------------------------------------
!> @brief Reads a 5-D array of 32-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_snk5(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(INOUT) :: dat(:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=5)
        CALL readdat_snk4(thisDA32,irec,dat(:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_snk5
!
!-------------------------------------------------------------------------------
!> @brief Reads a 5-D array of 64-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_slk5(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(INOUT) :: dat(:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=5)
        CALL readdat_slk4(thisDA32,irec,dat(:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_slk5
!
!-------------------------------------------------------------------------------
!> @brief Reads a 5-D array of 32-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_ssk5(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(INOUT) :: dat(:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=5)
        CALL readdat_ssk4(thisDA32,irec,dat(:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_ssk5
!
!-------------------------------------------------------------------------------
!> @brief Reads a 5-D array of 64-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sdk5(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(INOUT) :: dat(:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=5)
        CALL readdat_sdk4(thisDA32,irec,dat(:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sdk5
!
!-------------------------------------------------------------------------------
!> @brief Reads a 6-D logical array from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sbk6(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(INOUT) :: dat(:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=6)
        CALL readdat_sbk5(thisDA32,irec,dat(:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sbk6
!
!-------------------------------------------------------------------------------
!> @brief Reads a 6-D array of 32-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_snk6(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(INOUT) :: dat(:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=6)
        CALL readdat_snk5(thisDA32,irec,dat(:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_snk6
!
!-------------------------------------------------------------------------------
!> @brief Reads a 6-D array of 64-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_slk6(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(INOUT) :: dat(:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=6)
        CALL readdat_slk5(thisDA32,irec,dat(:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_slk6
!
!-------------------------------------------------------------------------------
!> @brief Reads a 6-D array of 32-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_ssk6(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(INOUT) :: dat(:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=6)
        CALL readdat_ssk5(thisDA32,irec,dat(:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_ssk6
!
!-------------------------------------------------------------------------------
!> @brief Reads a 6-D array of 64-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sdk6(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(INOUT) :: dat(:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=6)
        CALL readdat_sdk5(thisDA32,irec,dat(:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sdk6
!
!-------------------------------------------------------------------------------
!> @brief Reads a 7-D logical array from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sbk7(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      LOGICAL(SBK),INTENT(INOUT) :: dat(:,:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=7)
        CALL readdat_sbk6(thisDA32,irec,dat(:,:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sbk7
!
!-------------------------------------------------------------------------------
!> @brief Reads a 7-D array of 32-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_snk7(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SNK),INTENT(INOUT) :: dat(:,:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=7)
        CALL readdat_snk6(thisDA32,irec,dat(:,:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_snk7
!
!-------------------------------------------------------------------------------
!> @brief Reads a 7-D array of 64-bit integers from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_slk7(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      INTEGER(SLK),INTENT(INOUT) :: dat(:,:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=7)
        CALL readdat_slk6(thisDA32,irec,dat(:,:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_slk7
!
!-------------------------------------------------------------------------------
!> @brief Reads a 7-D array of 32-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_ssk7(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SSK),INTENT(INOUT) :: dat(:,:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=7)
        CALL readdat_ssk6(thisDA32,irec,dat(:,:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_ssk7
!
!-------------------------------------------------------------------------------
!> @brief Reads a 7-D array of 64-bit reals from a DA32 file starting at REC.
!> @param thisDA32 the DA32 file type object
!> @param rec the record address in the file to start reading
!> @param dat the data to be read
!> @param iostat optional return value with the IOSTAT value
!> @param nrec optional return value for the number of records read from the
!>        file
!>
    SUBROUTINE readdat_sdk7(thisDA32,rec,dat,iostat,nrec)
      CLASS(DA32FileType),INTENT(IN) :: thisDA32
      INTEGER(SLK),INTENT(IN) :: rec
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: iostat
      INTEGER(SLK),INTENT(OUT),OPTIONAL :: nrec
      REAL(SDK),INTENT(INOUT) :: dat(:,:,:,:,:,:,:)
      INTEGER(SIK) :: i,ioerr
      INTEGER(SLK) :: irec,n
      
      irec=rec
      ioerr=0
      DO i=1,SIZE(dat,DIM=7)
        CALL readdat_sdk6(thisDA32,irec,dat(:,:,:,:,:,:,i),ioerr,n)
        IF(ioerr /= 0) EXIT
        irec=irec+n
      ENDDO
      IF(PRESENT(iostat)) iostat=ioerr
      IF(PRESENT(nrec)) nrec=irec-rec
    ENDSUBROUTINE readdat_sdk7
!
!-------------------------------------------------------------------------------
!> @brief Internal routine to determine the maximum number of records in the 
!>        file on disk.
!> @param funit the unit number to check
!> @returns fsize the size in number of records of the file
!> 
    FUNCTION FileRecSize(funit) RESULT(fsize)
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK) :: fsize
      INQUIRE(UNIT=funit,SIZE=fsize)
      IF(fsize > 0) fsize=fsize*FILE_STORAGE_SIZE/(32*WORDSREC)
    ENDFUNCTION FileRecSize
!
ENDMODULE FileType_DA32
