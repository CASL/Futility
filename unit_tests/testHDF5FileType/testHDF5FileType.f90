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
  USE IO_Strings
  
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE
  
  CREATE_TEST("HDF File Type")

#ifdef HAVE_HDF5
  REGISTER_SUBTEST("HDF5FileType Write",testHDF5FileTypeWrite)
  REGISTER_SUBTEST("HDF5FileType Read",testHDF5FileTypeRead)
#else
  REGISTER_SUBTEST("HDF5 Not Present",testHDF5_wo)
#endif

  FINALIZE_TEST()

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeWrite()
      TYPE(HDF5FileType) :: h5
      REAL(SRK),ALLOCATABLE :: testR1(:),testR2(:,:)
      LOGICAL(SBK),ALLOCATABLE :: testl1(:)
      INTEGER(SIK) :: i

      ALLOCATE(testR1(10))
      ALLOCATE(testR2(10,10))
      ALLOCATE(testL1(10))

      testL1=.FALSE.
      DO i=1,10,2
        testL1(i)=.TRUE.
      ENDDO
    
      ! Create a RW access file. Existing file overwritten
      CALL h5%init('test.h5','NEW')
#ifdef HAVE_HDF5
      ASSERT(h5%isinit,'HDF5 file type not properly initialized!')
#else
      ASSERT(.TRUE.,'HDF5 not present')
#endif
      CALL h5%mkdir('groupA')
      CALL h5%write('groupA->memAR1',testR1)
      CALL h5%write('groupA->memAL1',testL1)
      CALL h5%write('test AR2',testR2)

      CALL h5%clear()
      ASSERT(.NOT.h5%isinit,'HDF5 object not properly cleared!')

    ENDSUBROUTINE testHDF5FileTypeWrite
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeRead()
      TYPE(HDF5FileType) :: h5
      REAL(SRK),ALLOCATABLE :: testR1(:)
      CHARACTER(LEN=80),ALLOCATABLE :: sets(:)
      INTEGER(SIK) :: i

      CALL h5%init('readtest.h5','READ')

      CALL h5%ls('groupA',sets)
      DO i=1,SIZE(sets)
        WRITE(*,*)sets(i)
      ENDDO

      !ALLOCATE(testR1(100))


      ! Read a dataset (real-1)
      CALL h5%read('groupA->A2->dset4',testR1)

      WRITE(*,*)testR1

      CALL h5%clear()
    ENDSUBROUTINE testHDF5FileTypeRead
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5_wo()
      TYPE(HDF5FileType) :: h5
      REAL(SRK),ALLOCATABLE :: testR1(:)

      ALLOCATE(testR1(100))

      CALL h5%init('test2.h5','NEW')

      

    ENDSUBROUTINE testHDF5_wo
ENDPROGRAM testHDF5
