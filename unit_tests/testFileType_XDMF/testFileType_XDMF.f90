!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testXDMFFileType
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE Strings
USE FileType_XDMF
IMPLICIT NONE

TYPE(XDMFFileType) :: testXDMFFile
TYPE(XDMFMeshType) :: mesh
TYPE(StringType) :: fname

CREATE_TEST('XDMF TYPE')
fname='gridmesh_two_pins.xdmf'
mesh = testXDMFFile%importFromDisk(fname)
REGISTER_SUBTEST('XML',testXML)


FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!
SUBROUTINE testXML()
  LOGICAL(SBK) :: bool
  ASSERT(bool, 'test')

ENDSUBROUTINE testXML
ENDPROGRAM testXDMFFileType
