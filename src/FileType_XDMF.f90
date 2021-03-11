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
#include "Futility_DBC.h"
USE ExceptionHandler
USE Futility_DBC
USE ISO_FORTRAN_ENV
USE IntrType
USE Strings
!USE IO_Strings
USE FileType_Base
USE FileType_XML
USE FileType_HDF5
USE VTKFiles
USE ParameterLists

IMPLICIT NONE
PRIVATE

PUBLIC :: XDMFFileType
PUBLIC :: XDMFMeshType

!> The module name
CHARACTER(LEN=*),PARAMETER :: modName='FILETYPE_XDMF'

!> Exception handler for the module
TYPE(ExceptionHandlerType),SAVE :: eXDMF

TYPE :: XDMFCell
  !> The cell type id followed by the vertex ids
  INTEGER(SIK), ALLOCATABLE :: vertex_list(:)
ENDTYPE XDMFCell

TYPE :: XDMFCellSet
  CHARACTER(LEN=64) :: name=''
  !> The cell ids
  INTEGER(SIK), ALLOCATABLE :: cell_list(:)
ENDTYPE XDMFCellSet

!> Mesh to hold XDMF Data
TYPE :: XDMFMeshType
  !> The name of the set
  CHARACTER(LEN=64) :: name=''
  !> Looks like:
  !> x1, x2, x3, ..., xn
  !> y1, y2, y3, ..., yn
  !> z1, z2, z3, ..., zn
  !> Therefore vertices will be of shape (3, N)
  REAL(SRK), ALLOCATABLE :: vertices(:, :)
  TYPE(XDMFCell), ALLOCATABLE :: cells(:)
  TYPE(XDMFCellSet), ALLOCATABLE :: cell_sets(:)

ENDTYPE XDMFMeshType

!> The XDMF File type
TYPE,EXTENDS(BaseFileType) :: XDMFFileType
!
!List of type bound procedures
  CONTAINS
    !> @copybrief FileType_XML::fopen_XDMFFileType
    !> @copydoc FileType_XML::fopen_XDMFFileType
    PROCEDURE,PASS :: fopen => fopen_XDMFFileType
    !> @copybrief FileType_XML::fclose_XDMFFileType
    !> @copydoc FileType_XML::fclose_XDMFFileType
    PROCEDURE,PASS :: fclose => fclose_XDMFFileType
    !> @copybrief FileType_XML::fdelete_XDMFFileType
    !> @copydoc FileType_XML::fdelete_XDMFFileType
    PROCEDURE,PASS :: fdelete => fdelete_XDMFFileType
ENDTYPE XDMFFileType

!
!===============================================================================
CONTAINS
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
ENDMODULE FileType_XDMF
