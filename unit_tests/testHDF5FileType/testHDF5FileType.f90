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
  USE Strings
  USE ExceptionHandler
  USE Strings
  USE FileType_HDF5
  USE IO_Strings
  USE ParameterLists
  
  IMPLICIT NONE
  
#ifdef HAVE_MPI
  INCLUDE 'mpif.h'
#endif

  TYPE(MPI_EnvType) :: testMPI
  INTEGER(SIK) :: mpierr,mysize,myrank
  
#ifdef HAVE_MPI
  CALL testMPI%init(mpierr)
  IF(testMPI%rank /=0)THEN
    utest_master=.FALSE.
  ENDIF
#endif

  CREATE_TEST("HDF File Type")

#ifdef MPACT_HAVE_HDF5
  REGISTER_SUBTEST("HDF5FileType Delete",testHDF5FileTypeCreateDelete)
  REGISTER_SUBTEST("HDF5FileType Write",testHDF5FileTypeWrite)
  REGISTER_SUBTEST("HDF5FileType Read",testHDF5FileTypeRead)
#ifdef HAVE_MPI
!  REGISTER_SUBTEST("HDF5FileType Parallel",testHDF5Parallel)
#endif
#else
  REGISTER_SUBTEST("HDF5 Not Present",testHDF5_wo)
#endif

  FINALIZE_TEST()
!#ifdef HAVE_MPI
!  CALL testMPI%finalize()
!  CALL testMPI%clear()
!#endif

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeCreateDelete()
      TYPE(HDF5FileType) :: h5
      LOGICAL(SBK) :: exists
      
      CALL h5%init('createdeletetest.h5','NEW')
      ASSERT(h5%isinit,'HDF5 object no properly initialized')
      IF(h5%isinit) exists=.TRUE.
      
      CALL h5%fdelete()
      INQUIRE(FILE='createdeletetest.h5',EXIST=exists)
      ASSERT(.NOT.exists,'HDF5 object not properly deleted.')
      
    ENDSUBROUTINE testHDF5FileTypeCreateDelete
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeWrite()
      TYPE(HDF5FileType) :: h5
      REAL(SDK),ALLOCATABLE :: testD1(:),testD2(:,:),testD3(:,:,:)
      REAL(SSK),ALLOCATABLE :: testS1(:),testS2(:,:),testS3(:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: testB1(:),testB2(:,:),testB3(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: testL1(:),testL2(:,:),testL3(:,:,:)
      INTEGER(SNK),ALLOCATABLE :: testN1(:),testN2(:,:),testN3(:,:,:)
      REAL(SDK) :: testD0
      REAL(SSK) :: testS0
      INTEGER(SLK) :: testL0
      INTEGER(SNK) :: testN0
      LOGICAL(SBK) :: testB0
      TYPE(StringType) :: testC0
      TYPE(StringType),ALLOCATABLE :: testC1(:),testC2(:,:),testC3(:,:,:)
      CHARACTER(LEN=12) :: helper_string
      INTEGER(SIK) :: i,j,k

      ALLOCATE(testD1(10))
      ALLOCATE(testD2(4,5))
      ALLOCATE(testD3(3,4,5))
      ALLOCATE(testS1(10))
      ALLOCATE(testS2(4,5))
      ALLOCATE(testS3(3,4,5))
      ALLOCATE(testB1(6))
      ALLOCATE(testB2(6,5))
      ALLOCATE(testB3(6,5,2))
      ALLOCATE(testL1(10))
      ALLOCATE(testL2(4,5))
      ALLOCATE(testL3(3,4,5))
      ALLOCATE(testN1(10))
      ALLOCATE(testN2(4,5))
      ALLOCATE(testN3(3,4,5))
      ALLOCATE(testC1(3))
      ALLOCATE(testC2(2,3))
      ALLOCATE(testC3(3,4,5))

      testD0=42.123456789_SDK
      testD1=[(i*1.0000000001_SDK,i=1,SIZE(testD1))]
      testD2=RESHAPE([(i*1.0000000001_SDK,i=1,SIZE(testD2))],SHAPE(testD2))
      testD3=RESHAPE([(i*1.0000000001_SDK,i=1,SIZE(testD3))],SHAPE(testD3))
      testS0=42.000_SSK
      testS1=[(i,i=1,SIZE(testS1))]
      testS2=RESHAPE([(i,i=1,SIZE(testS2))],SHAPE(testS2))
      testS3=RESHAPE([(i,i=1,SIZE(testS3))],SHAPE(testS3))
      testL0=123456789123456789_SLK
      testL1=[(i+1000000000,i=1,SIZE(testL1))]
      testL2=RESHAPE([(i+1000000000,i=1,SIZE(testL2))],SHAPE(testL2))
      testL3=RESHAPE([(i+1000000000,i=1,SIZE(testL3))],SHAPE(testL3))
      testN0=42_SNK
      testN1=[(i,i=1,SIZE(testN1))]
      testN2=RESHAPE([(i,i=1,SIZE(testN2))],SHAPE(testN2))
      testN3=RESHAPE([(i,i=1,SIZE(testN3))],SHAPE(testN3))
      testB0=.FALSE.
      testB1(:)=.TRUE.; testB2(:,:)=.TRUE.; testB3(:,:,:)=.TRUE.
      DO i=1,SIZE(testB1),2
        testB1(i)=.FALSE.
        DO j=1,SIZE(testB2,DIM=2),3
          testB2(i,j)=.FALSE.
          DO k=1,SIZE(testB3,DIM=3)
            testB3(i,j,k)=.FALSE.
          ENDDO
        ENDDO
      ENDDO
      testC0='Rank-0 (Not-an-array) String Test'
      DO i=1,SIZE(testC1)
        WRITE(helper_string,FMT="(a7,i0)") 'String ',i
        testC1(i)=helper_string
      ENDDO
      DO i=1,SIZE(testC2,1)
        DO j=1,SIZE(TestC2,2)
          WRITE(helper_string,FMT="(a7,i0,a1,i0)") 'String ',i,',',j
          testC2(i,j)=helper_string
        ENDDO
      ENDDO
      testC2(1,1)='String 1,1';testC2(1,2)='String 1,2';testC2(2,1)='String 2,1'
      testC2(2,2)='String 2,2';
      DO i=1,SIZE(testC3,1)
        DO j=1,SIZE(testC3,2)
          DO k=1,SIZE(testC3,3)
            WRITE(helper_string,FMT="(a7,i0,a1,i0,a1,i0)") 'String ',i,&
                    &",",j,",",k
            testC3(i,j,k)=helper_string
          ENDDO
        ENDDO
      ENDDO
    
      ! Create a RW access file. Existing file overwritten
      CALL h5%init('writetest.h5','NEW')
      
#ifdef MPACT_HAVE_HDF5
      ASSERT(h5%isinit,'HDF5 file type not properly initialized!')
#else
      ASSERT(.TRUE.,'HDF5 not present')
#endif
      CALL h5%mkdir('groupR')
      CALL h5%fwrite('groupR->memD0',testD0)
      CALL h5%fwrite('groupR->memD1',testD1,SHAPE(testD1))
      CALL h5%fwrite('groupR->memD2',testD2,SHAPE(testD2))
      CALL h5%fwrite('groupR->memD3',testD3,SHAPE(testD3))
      CALL h5%fwrite('groupR->memS0',testS0)
      CALL h5%fwrite('groupR->memS1',testS1,SHAPE(testS1))
      CALL h5%fwrite('groupR->memS2',testS2,SHAPE(testS2))
      CALL h5%fwrite('groupR->memS3',testS3,SHAPE(testS3))
      CALL h5%mkdir('groupI')
      CALL h5%fwrite('groupI->memL0',testL0)
      CALL h5%fwrite('groupI->memL1',testL1,SHAPE(testL1))
      CALL h5%fwrite('groupI->memL2',testL2,SHAPE(testL2))
      CALL h5%fwrite('groupI->memL3',testL3,SHAPE(testL3))
      CALL h5%fwrite('groupI->memN0',testN0)
      CALL h5%fwrite('groupI->memN1',testN1,SHAPE(testN1))
      CALL h5%fwrite('groupI->memN2',testN2,SHAPE(testN2))
      CALL h5%fwrite('groupI->memN3',testN3,SHAPE(testN3))
      CALL h5%mkdir('groupB')
      CALL h5%fwrite('groupB->memB0',testB0)
      CALL h5%fwrite('groupB->memB1',testB1,SHAPE(testB1))
      CALL h5%fwrite('groupB->memB2',testB2,SHAPE(testB2))
      CALL h5%fwrite('groupB->memB3',testB3,SHAPE(testB3))
      CALL h5%mkdir('groupC')
      CALL h5%fwrite('groupC->memC0',testC0)
      CALL h5%fwrite('groupC->memC1',testC1,SHAPE(testC1))
      CALL h5%fwrite('groupC->memC2',testC2,SHAPE(testC2))
      CALL h5%fwrite('groupC->memC3',testC3,SHAPE(testC3))

      DEALLOCATE(testD1,testD2,testD3,testS1,testS2,testS3,testL1,testL2, &
            testL3,testB1,testB2,testB3,testC1,testC2,testC3,testN1,testN2, &
            testN3)

      CALL h5%clear()
      ASSERT(.NOT.h5%isinit,'HDF5 object not properly cleared!')

    ENDSUBROUTINE testHDF5FileTypeWrite
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeRead()
      TYPE(HDF5FileType) :: h5
      REAL(SDK),ALLOCATABLE :: testD1(:),testD2(:,:),testD3(:,:,:)
      REAL(SSK),ALLOCATABLE :: testS1(:),testS2(:,:),testS3(:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: testB1(:),testB2(:,:),testB3(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: testL1(:),testL2(:,:),testL3(:,:,:)
      INTEGER(SNK),ALLOCATABLE :: testN1(:),testN2(:,:),testN3(:,:,:)
      TYPE(StringType),ALLOCATABLE :: testC1(:),testC2(:,:),testC3(:,:,:)
      REAL(SDK) :: testD0
      REAL(SSK) :: testS0
      INTEGER(SLK) :: testL0
      INTEGER(SNK) :: testN0
      LOGICAL(SBK) :: testB0
      TYPE(StringType) :: testC0
      CHARACTER(LEN=80),ALLOCATABLE :: sets(:)
      INTEGER(SIK) :: i,j,k
      CHARACTER(LEN=80),PARAMETER :: FMT_data_r='(5f15.11)',FMT_data_i='(5i15)'&
              ,FMT_data_l='(5l15)'

      CALL h5%init('readtest.h5','READ')
      OPEN(UNIT=1,FILE='readtest.out')

      CALL h5%ls('groupR',sets)
      DO i=1,SIZE(sets)
        WRITE(1,*)sets(i)
      ENDDO

      ! Read a dataset (real-1)
      CALL h5%fread('groupR->memD0',testD0)
      CALL h5%fread('groupR->memD1',testD1)
      CALL h5%fread('groupR->memD2',testD2)
      CALL h5%fread('groupR->memD3',testD3)
      CALL h5%fread('groupR->memS0',testS0)
      CALL h5%fread('groupR->memS1',testS1)
      CALL h5%fread('groupR->memS2',testS2)
      CALL h5%fread('groupR->memS3',testS3)
      CALL h5%fread('groupI->memN0',testN0)
      CALL h5%fread('groupI->memN1',testN1)
      CALL h5%fread('groupI->memN2',testN2)
      CALL h5%fread('groupI->memN3',testN3)
      CALL h5%fread('groupI->memL0',testL0)
      CALL h5%fread('groupI->memL1',testL1)
      CALL h5%fread('groupI->memL2',testL2)
      CALL h5%fread('groupI->memL3',testL3)
      CALL h5%fread('groupB->memB0',testB0)
      CALL h5%fread('groupB->memB1',testB1)
      CALL h5%fread('groupB->memB2',testB2)
      CALL h5%fread('groupB->memB3',testB3)
      CALL h5%fread('groupC->memC0',testC0)
      CALL h5%fread('groupC->memC1',testC1)
      CALL h5%fread('groupC->memC2',testC2)
      CALL h5%fread('groupC->memC3',testC3)
      
      WRITE(1,*) 'testD0:'; WRITE(1,FMT=FMT_data_r)testD0
      WRITE(1,*) 'testD1:'; WRITE(1,FMT=FMT_data_r)testD1
      WRITE(1,*) 'testD2:'; WRITE(1,FMT=FMT_data_r)testD2
      WRITE(1,*) 'testD3:'; WRITE(1,FMT=FMT_data_r)testD3
      WRITE(1,*) 'testS0:'; WRITE(1,FMT=FMT_data_r)testS0
      WRITE(1,*) 'testS1:'; WRITE(1,FMT=FMT_data_r)testS1
      WRITE(1,*) 'testS2:'; WRITE(1,FMT=FMT_data_r)testS2
      WRITE(1,*) 'testS3:'; WRITE(1,FMT=FMT_data_r)testS3
      WRITE(1,*) 'testN0:'; WRITE(1,FMT=FMT_data_i)testN0
      WRITE(1,*) 'testN1:'; WRITE(1,FMT=FMT_data_i)testN1
      WRITE(1,*) 'testN2:'; WRITE(1,FMT=FMT_data_i)testN2
      WRITE(1,*) 'testN3:'; WRITE(1,FMT=FMT_data_i)testN3
      WRITE(1,*) 'testL0:'; WRITE(1,*)testL0
      WRITE(1,*) 'testL1:'; WRITE(1,FMT=FMT_data_i)testL1
      WRITE(1,*) 'testL2:'; WRITE(1,FMT=FMT_data_i)testL2
      WRITE(1,*) 'testL3:'; WRITE(1,FMT=FMT_data_i)testL3
      WRITE(1,*) 'testB0:'; WRITE(1,FMT=FMT_data_l)testB0
      WRITE(1,*) 'testB1:'; WRITE(1,FMT=FMT_data_l)testB1
      WRITE(1,*) 'testB2:'; WRITE(1,FMT=FMT_data_l)testB2
      WRITE(1,*) 'testB3:'; WRITE(1,FMT=FMT_data_l)testB3
      WRITE(1,*) 'testC0:'; WRITE(1,FMT=*)CHAR(testC0)
      WRITE(1,*) 'testC1:'; 
      DO i=1,SIZE(testC1)
        WRITE(1,*)CHAR(testC1(i))
      ENDDO
      WRITE(1,*) 'testC2:';
      DO i=1,SIZE(testC2,1)
        DO j=1,SIZE(testC2,2)
          WRITE(1,*)CHAR(testC2(i,j))
        ENDDO
      ENDDO
      WRITE(1,*) 'testC3:';
      DO i=1,SIZE(testC3,1)
        DO j=1,SIZE(testC3,2)
          DO k=1,SIZE(testC3,3)
            WRITE(1,*)CHAR(testC3(i,j,k))
          ENDDO
        ENDDO
      ENDDO
      CLOSE(UNIT=1)

      DEALLOCATE(testD1,testD2,testD3,testS1,testS2,testS3,testL1,testL2, &
            testL3,testB1,testB2,testB3,testC1,testC2,testC3,testN1,testN2, &
            testN3)

      CALL h5%clear()
      ASSERT(.NOT.h5%isinit, 'HDF5 object not properly cleared!')
      
    ENDSUBROUTINE testHDF5FileTypeRead
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5_wo()
      TYPE(HDF5FileType) :: h5

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

      CALL h5%fwrite('testD2',testR2,(/dim1,dim2/))

      CALL h5%clear()

    ENDSUBROUTINE testHDF5Parallel
ENDPROGRAM testHDF5
