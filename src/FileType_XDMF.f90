!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief 
!>
!> 
!> 
!> 
!> 
!> 
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FileType_XDMF
USE ISO_FORTRAN_ENV
USE IntrType
USE Strings
USE IO_Strings
USE FileType_Base

IMPLICIT NONE
PRIVATE

PUBLIC :: XDMFFileType

!> The XDMF File type
TYPE,EXTENDS(BaseFileType) :: XDMFFileType
  !> Logical indicating if file was initialized
  LOGICAL(SBK) :: isInit=.FALSE.
  !> Parameter list for XML light data
  TYPE(ParamType) :: xmlPL
  !> HDF5 File for heavy data
!
!List of type bound procedures
  CONTAINS
    !> @copybrief FileType_XML::init_XDMFFileType
    !> @copydoc FileType_XML::init_XDMFFileType
    PROCEDURE,PASS :: init => init_XDMFFileType
    !> @copybrief FileType_XML::clear_XDMFFileType
    !> @copydoc FileType_XML::clear_XDMFFileType
    PROCEDURE,PASS :: clear => clear_XDMFFileType
    !> @copybrief FileType_XML::fopen_XDMFFileType
    !> @copydoc FileType_XML::fopen_XDMFFileType
    PROCEDURE,PASS :: fopen => fopen_XDMFFileType
    !> @copybrief FileType_XML::fclose_XDMFFileType
    !> @copydoc FileType_XML::fclose_XDMFFileType
    PROCEDURE,PASS :: fclose => fclose_XDMFFileType
    !> @copybrief FileType_XML::fdelete_XDMFFileType
    !> @copydoc FileType_XML::fdelete_XDMFFileType
    PROCEDURE,PASS :: fdelete => fdelete_XDMFFileType
    !> @copybrief FileType_XML::importFromDisk_XDMFFileType
    !> @copydoc FileType_XML::importFromDisk_XDMFFileType
    PROCEDURE,PASS :: importFromDisk => importFromDisk_XDMFFileType
ENDTYPE XDMFFileType

CHARACTER(LEN=*),PARAMETER :: modName='FILETYPE_XDMF'
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes an XDMF file type
!> @param thisXDMFFile the XDML file type object
!> @param fname the name of the file to process
!> @param lread whether or not the file will be opened for reading or writing
!>
SUBROUTINE init_XDMFFileType(thisXDMFFile,fname,lread)
  CHARACTER(LEN=*),PARAMETER :: myName='init_XDMFFileType'
  CLASS(XDMFFileType),INTENT(INOUT) :: thisXDMFFile
  CHARACTER(LEN=*) :: fname
  LOGICAL(SBK) :: lread
ENDSUBROUTINE init_XDMFFileType
!
!-------------------------------------------------------------------------------
!> @brief Clears the XDMF File object
!> @param thisXDMFFile the XDMF file object
!>
SUBROUTINE clear_XDMFFileType(thisXDMFFile)
  CLASS(XDMFFileType),INTENT(INOUT) :: thisXDMFFile
ENDSUBROUTINE clear_XDMFFileType
!
!-------------------------------------------------------------------------------
!> @brief Opens the XDMF file type for I/O
!> @param file the XDMF file type object
!>
!> @todo fix how the unit number is set
!>
SUBROUTINE fopen_XDMFFileType(file)
  CHARACTER(LEN=*),PARAMETER :: myName='fopen_XDMFFileType'
  CLASS(XDMFFileType),INTENT(INOUT) :: file
  LOGICAL(SBK) :: lopen
  INTEGER(SIK) :: funit,ierr
  TYPE(StringType) :: fname

ENDSUBROUTINE fopen_XDMFFileType
!
!-------------------------------------------------------------------------------
!> @brief Closes the XDMF file object
!> @param file the XDMF file object
!>
SUBROUTINE fclose_XDMFFileType(file)
  CHARACTER(LEN=*),PARAMETER :: myName='fclose_XDMFFileType'
  CLASS(XDMFFileType),INTENT(INOUT) :: file
  INTEGER(SIK) :: ierr
ENDSUBROUTINE fclose_XDMFFileType
!
!-------------------------------------------------------------------------------
!> @brief Deletes the XDMF file from disk
!> @param file the XDMF file object
!>
SUBROUTINE fdelete_XDMFFileType(file)
  CHARACTER(LEN=*),PARAMETER :: myName='fdelete_XDMFFileType'
  CLASS(XDMFFileType),INTENT(INOUT) :: file
  INTEGER(SIK) :: ierr
ENDSUBROUTINE fdelete_XDMFFileType
!
!-------------------------------------------------------------------------------
!> @brief Loads an XDMF file from disk into the XDMF File type object in memory
!> @param thisXDMFFile the XDMF file to populate from disk
!> @param fname the name of the file to process
!>
SUBROUTINE importFromDisk_XDMFFileType(thisXDMFFile,fname)
  CHARACTER(LEN=*),PARAMETER :: myName='importFromDisk_XDMFFileType'
  CLASS(XDMFFileType),INTENT(INOUT) :: thisXDMFFile
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: fname

ENDSUBROUTINE importFromDisk_XDMFFileType
!
ENDMODULE FileType_XDMF
