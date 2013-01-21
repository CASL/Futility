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
#include "UnitTest.h"

PROGRAM testHDF5
  USE UnitTest
  USE IntrType
  USE FileType_HDF5
  
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE
  
  CREATE_TEST("HDF File Type")

  REGISTER_SUBTEST("HDF5FileType",testHDF5FileType)

  FINALIZE_TEST()


!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileType()
      TYPE(HDF5FileType) :: h5
      REAL(SRK),ALLOCATABLE :: testR1(:),testR2(:,:)

      ALLOCATE(testR1(10))
      ALLOCATE(testR2(10,10))
    
      ! Create a RW access file. Existing file overwritten
      CALL h5%init('test.h5',.TRUE.)
#ifdef HAVE_HDF5
      ASSERT(h5%isinit,'HDF5 file type not properly initialized!')
#else
      ASSERT(.TRUE.,'HDF5 file type not properly initialized!')
#endif
      CALL h5%write('test AR1',testR1)
      CALL h5%write('test AR2',testR2)

      CALL h5%clear()
      ASSERT(.NOT.h5%isinit,'HDF5 object not properly cleared!')

    ENDSUBROUTINE testHDF5FileType

ENDPROGRAM testHDF5
