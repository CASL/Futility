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
  USE Strings
  
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

#ifdef MPACT_HAVE_HDF5
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
      REAL(SDK),ALLOCATABLE :: testD1(:),testD2(:,:),testD3(:,:,:)
      REAL(SSK),ALLOCATABLE :: testR1(:),testR2(:,:),testR3(:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: testL1(:),testL2(:,:),testL3(:,:,:)
      INTEGER(SIK),ALLOCATABLE :: testI1(:),testI2(:,:),testI3(:,:,:)
      TYPE(StringType) :: testC
      TYPE(StringType),ALLOCATABLE :: testC1(:),testC2(:,:)
      INTEGER(SIK) :: i,j,k

      ALLOCATE(testD1(10))
      ALLOCATE(testD2(5,5))
      ALLOCATE(testD3(3,3,3))
      ALLOCATE(testR1(10))
      ALLOCATE(testR2(5,5))
      ALLOCATE(testR3(3,3,3))
      ALLOCATE(testL1(10))
      ALLOCATE(testL2(5,5))
      ALLOCATE(testL3(3,3,3))
      ALLOCATE(testI1(10))
      ALLOCATE(testI2(5,5))
      ALLOCATE(testI3(3,3,3))
!      ALLOCATE(testC1(3))
!      ALLOCATE(testC2(2,2))

      testD1=[(i,i=1,SIZE(testD1))]
      testD2=RESHAPE([(i,i=1,SIZE(testD2))],SHAPE(testD2))
      testD3=RESHAPE([(i,i=1,SIZE(testD3))],SHAPE(testD3))
      testR1=[(i,i=1,SIZE(testR1))]
      testR2=RESHAPE([(i,i=1,SIZE(testR2))],SHAPE(testR2))
      testR3=RESHAPE([(i,i=1,SIZE(testR3))],SHAPE(testR3))
      testI1=[(i,i=1,SIZE(testI1))]
      testI2=RESHAPE([(i,i=1,SIZE(testI2))],SHAPE(testI2))
      testI3=RESHAPE([(i,i=1,SIZE(testI3))],SHAPE(testI3))
      testL1(:)=.TRUE.; testL2(:,:)=.TRUE.; testL3(:,:,:)=.TRUE.
      DO i=1,3,2
        testL1(i)=.FALSE.
        DO j=1,3,3
          testL2(i,j)=.FALSE.
          DO k=1,3
            testL3(i,j,k)=.FALSE.
          ENDDO
        ENDDO
      ENDDO
      testC='String Test'
!      testC1(1)='String 1';testC1(2)='String 2';testC1(3)='String 3'
!      testC2(1,1)='String 1,1';testC2(1,2)='String1,2';testC2(2,1)='String 2,1'
!      testC2(2,2)='String2,2';
    
      ! Create a RW access file. Existing file overwritten
      CALL h5%init('writetest.h5','NEW')
      
#ifdef MPACT_HAVE_HDF5
      ASSERT(h5%isinit,'HDF5 file type not properly initialized!')
#else
      ASSERT(.TRUE.,'HDF5 not present')
#endif
      CALL h5%mkdir('groupD')
      CALL h5%write('groupD->memD1',testD1,SHAPE(testD1))
      CALL h5%write('groupD->memD2',testD2,SHAPE(testD2))
      CALL h5%write('groupD->memD3',testD3,SHAPE(testD3))
      CALL h5%mkdir('groupR')
      CALL h5%write('groupR->memR1',testR1,SHAPE(testR1))
      CALL h5%write('groupR->memR2',testR2,SHAPE(testR2))
      CALL h5%write('groupR->memR3',testR3,SHAPE(testR3))
      CALL h5%mkdir('groupI')
      CALL h5%write('groupI->memI1',testI1)
      CALL h5%write('groupI->memI2',testI2,SHAPE(testI2))
      CALL h5%write('groupI->memI3',testI3,SHAPE(testI3))
      CALL h5%mkdir('groupL')
      CALL h5%write('groupL->memL1',testL1)
      CALL h5%write('groupL->memL2',testL2,SHAPE(testL2))
      CALL h5%write('groupL->memL3',testL3,SHAPE(testL3))
      CALL h5%mkdir('groupC')
      CALL h5%write('groupC->memC',testC)

      CALL h5%clear()
      ASSERT(.NOT.h5%isinit,'HDF5 object not properly cleared!')

    ENDSUBROUTINE testHDF5FileTypeWrite
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeRead()
      TYPE(HDF5FileType) :: h5
      REAL(SDK),ALLOCATABLE :: testD1(:),testD2(:,:),testD3(:,:,:)
      REAL(SSK),ALLOCATABLE :: testR1(:),testR2(:,:),testR3(:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: testL1(:),testL2(:,:),testL3(:,:,:)
      INTEGER(SIK),ALLOCATABLE :: testI1(:),testI2(:,:),testI3(:,:,:)
      CHARACTER(LEN=80),ALLOCATABLE :: sets(:)
      INTEGER(SIK) :: i
      CHARACTER(LEN=80),PARAMETER :: FMT_data_r='(5f9.5)',FMT_data_i='(5i9)', &
              FMT_data_l='(5l9)'

      CALL h5%init('readtest.h5','READ')
      OPEN(UNIT=1,FILE='readtest.out')

      CALL h5%ls('groupD',sets)
      DO i=1,SIZE(sets)
        WRITE(1,*)sets(i)
      ENDDO

      ALLOCATE(testD1(10))
      ALLOCATE(testD2(5,5))
      ALLOCATE(testD3(3,3,3))
      ALLOCATE(testR1(10))
      ALLOCATE(testR2(5,5))
      ALLOCATE(testR3(3,3,3))
      ALLOCATE(testL1(10))
      ALLOCATE(testL2(5,5))
      ALLOCATE(testL3(3,3,3))
      ALLOCATE(testI1(10))
      ALLOCATE(testI2(5,5))
      ALLOCATE(testI3(3,3,3))

      ! Read a dataset (real-1)
      CALL h5%read('groupD->memD1',testD1)
      CALL h5%read('groupR->memR1',testR1)
      CALL h5%read('groupI->memI1',testI1)
      CALL h5%read('groupL->memL1',testL1)
      CALL h5%read('groupD->memD2',testD2)
      CALL h5%read('groupR->memR2',testR2)
      CALL h5%read('groupI->memI2',testI2)
      CALL h5%read('groupL->memL2',testL2)
      CALL h5%read('groupD->memD3',testD3)
      CALL h5%read('groupR->memR3',testR3)
      CALL h5%read('groupI->memI3',testI3)
      CALL h5%read('groupL->memL3',testL3)
      
      WRITE(1,*) 'testD1:'; WRITE(1,FMT=FMT_data_r)testD1
      WRITE(1,*) 'testD2:'; WRITE(1,FMT=FMT_data_r)testD2
      WRITE(1,*) 'testD3:'; WRITE(1,FMT=FMT_data_r)testD3
      WRITE(1,*) 'testR1:'; WRITE(1,FMT=FMT_data_r)testR1
      WRITE(1,*) 'testR2:'; WRITE(1,FMT=FMT_data_r)testR2
      WRITE(1,*) 'testR3:'; WRITE(1,FMT=FMT_data_r)testR3
      WRITE(1,*) 'testI1:'; WRITE(1,FMT=FMT_data_i)testI1
      WRITE(1,*) 'testI2:'; WRITE(1,FMT=FMT_data_i)testI2
      WRITE(1,*) 'testI3:'; WRITE(1,FMT=FMT_data_i)testI3
      WRITE(1,*) 'testL1:'; WRITE(1,FMT=FMT_data_l)testL1
      WRITE(1,*) 'testL2:'; WRITE(1,FMT=FMT_data_l)testL2
      WRITE(1,*) 'testL3:'; WRITE(1,FMT=FMT_data_l)testL3
      CLOSE(UNIT=1)

      CALL h5%clear()
      ASSERT(.NOT.h5%isinit, 'HDF5 object not properly cleared!')
      
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
