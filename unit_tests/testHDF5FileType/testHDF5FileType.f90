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
  USE ISO_FORTRAN_ENV
  USE ParallelEnv
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE FileType_HDF5
  USE IO_Strings
  
  IMPLICIT NONE
  
#ifdef HAVE_MPI
  INCLUDE 'mpif.h'
#endif

  TYPE(MPI_EnvType) :: testMPI
  INTEGER(SIK) :: mpierr,mysize,myrank
  
#ifdef HAVE_MPI
  CALL testMPI%init(mpierr)
  IF(testMPI%rank /= 0) THEN
    utest_master=.FALSE.
  ENDIF
#endif

  CREATE_TEST("HDF File Type")

#ifdef HAVE_HDF5
  REGISTER_SUBTEST("HDF5FileType Write",testHDF5FileTypeWrite)
  REGISTER_SUBTEST("HDF5FileType Read",testHDF5FileTypeRead)
#ifdef HAVE_MPI
  REGISTER_SUBTEST("HDF5FileType Parallel",testHDF5Parallel)
#endif
#else
  REGISTER_SUBTEST("HDF5 Not Present",testHDF5_wo)
#endif

  FINALIZE_TEST()
#ifdef HAVE_MPI
  CALL testMPI%finalize()
  CALL testMPI%clear()
#endif

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

      testR2=1

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
      CALL h5%write('test AR2',testR2,shape(testR2))

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

      ALLOCATE(testR1(10))

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
!
!-------------------------------------------------------------------------------

   SUBROUTINE testHDF5Parallel

      TYPE(HDF5FileType) :: h5
      REAL(SRK),ALLOCATABLE :: testR2(:,:)
      INTEGER(SIK),PARAMETER :: dim1=16
      INTEGER(SIK),PARAMETER :: dim2=16
      INTEGER(SIK) :: rank,l1,u1,l2,u2,nproc,mpierr
      INTEGER(SIK) :: npp1,npp2,ndir1,ndir2

      rank=testMPI%rank
      nproc=testMPI%nproc
      selectcase(nproc)
      CASE(4)
        npp1=8
        npp2=8
        ndir1=2
        ndir2=2
      CASE(8)
        npp1=8
        npp2=4
        ndir1=2
        ndir2=4
      CASE(16)
        npp1=4
        npp2=4
        ndir1=4
        ndir2=4
      CASE default
        stop
      ENDSELECT
    
      ! Allocate the local part of the array. This won't be the entire array,
      ! but just the local domain of the global array.
      l1=(MOD(rank,ndir1))*npp1+1
      u1=l1+npp1-1
      l2=(rank/ndir1)*npp2+1
      u2=l2+npp2-1
      ALLOCATE(testR2(l1:u1,l2:u2))

      WRITE(*,*)rank,l1,l2

      testR2=rank+1

      CALL h5%init('parallelout.h5','NEW')

      CALL h5%write('testD2',testR2,(/dim1,dim2/))

      CALL h5%clear()

    ENDSUBROUTINE testHDF5Parallel
ENDPROGRAM testHDF5
