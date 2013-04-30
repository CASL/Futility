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
      testL0=12345678912345678_SLK
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
      LOGICAL(SBK) :: checkread

!  Set up reference solutions (Copied from write routine)
      REAL(SDK),ALLOCATABLE :: refD1(:),refD2(:,:),refD3(:,:,:)
      REAL(SSK),ALLOCATABLE :: refS1(:),refS2(:,:),refS3(:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: refB1(:),refB2(:,:),refB3(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: refL1(:),refL2(:,:),refL3(:,:,:)
      INTEGER(SNK),ALLOCATABLE :: refN1(:),refN2(:,:),refN3(:,:,:)
      REAL(SDK) :: refD0
      REAL(SSK) :: refS0
      INTEGER(SLK) :: refL0
      INTEGER(SNK) :: refN0
      LOGICAL(SBK) :: refB0
      TYPE(StringType) :: refC0
      TYPE(StringType),ALLOCATABLE :: refC1(:),refC2(:,:),refC3(:,:,:)
      CHARACTER(LEN=12) :: helper_string

      ALLOCATE(refD1(10))
      ALLOCATE(refD2(4,5))
      ALLOCATE(refD3(3,4,5))
      ALLOCATE(refS1(10))
      ALLOCATE(refS2(4,5))
      ALLOCATE(refS3(3,4,5))
      ALLOCATE(refB1(6))
      ALLOCATE(refB2(6,5))
      ALLOCATE(refB3(6,5,2))
      ALLOCATE(refL1(10))
      ALLOCATE(refL2(4,5))
      ALLOCATE(refL3(3,4,5))
      ALLOCATE(refN1(10))
      ALLOCATE(refN2(4,5))
      ALLOCATE(refN3(3,4,5))
      ALLOCATE(refC1(3))
      ALLOCATE(refC2(2,3))
      ALLOCATE(refC3(3,4,5))

      refD0=42.123456789_SDK
      refD1=[(i*1.0000000001_SDK,i=1,SIZE(refD1))]
      refD2=RESHAPE([(i*1.0000000001_SDK,i=1,SIZE(refD2))],SHAPE(refD2))
      refD3=RESHAPE([(i*1.0000000001_SDK,i=1,SIZE(refD3))],SHAPE(refD3))
      refS0=42.000_SSK
      refS1=[(i,i=1,SIZE(refS1))]
      refS2=RESHAPE([(i,i=1,SIZE(refS2))],SHAPE(refS2))
      refS3=RESHAPE([(i,i=1,SIZE(refS3))],SHAPE(refS3))
      refL0=12345678912345678_SLK
      refL1=[(i+1000000000,i=1,SIZE(refL1))]
      refL2=RESHAPE([(i+1000000000,i=1,SIZE(refL2))],SHAPE(refL2))
      refL3=RESHAPE([(i+1000000000,i=1,SIZE(refL3))],SHAPE(refL3))
      refN0=42_SNK
      refN1=[(i,i=1,SIZE(refN1))]
      refN2=RESHAPE([(i,i=1,SIZE(refN2))],SHAPE(refN2))
      refN3=RESHAPE([(i,i=1,SIZE(refN3))],SHAPE(refN3))
      refB0=.FALSE.
      refB1(:)=.TRUE.; refB2(:,:)=.TRUE.; refB3(:,:,:)=.TRUE.
      DO i=1,SIZE(refB1),2
        refB1(i)=.FALSE.
        DO j=1,SIZE(refB2,DIM=2),3
          refB2(i,j)=.FALSE.
          DO k=1,SIZE(refB3,DIM=3)
            refB3(i,j,k)=.FALSE.
          ENDDO
        ENDDO
      ENDDO
      refC0='Rank-0 (Not-an-array) String Test'
      DO i=1,SIZE(refC1)
        WRITE(helper_string,FMT="(a7,i0)") 'String ',i
        refC1(i)=helper_string
      ENDDO
      DO i=1,SIZE(refC2,1)
        DO j=1,SIZE(refC2,2)
          WRITE(helper_string,FMT="(a7,i0,a1,i0)") 'String ',i,',',j
          refC2(i,j)=helper_string
        ENDDO
      ENDDO
      refC2(1,1)='String 1,1';refC2(1,2)='String 1,2';refC2(2,1)='String 2,1'
      refC2(2,2)='String 2,2';
      DO i=1,SIZE(refC3,1)
        DO j=1,SIZE(refC3,2)
          DO k=1,SIZE(refC3,3)
            WRITE(helper_string,FMT="(a7,i0,a1,i0,a1,i0)") 'String ',i,&
                    &",",j,",",k
            refC3(i,j,k)=helper_string
          ENDDO
        ENDDO
      ENDDO

!  Begin test
      CALL h5%init('readtest.h5','READ')

      CALL h5%ls('groupR',sets)
!      WRITE(readtest,*) TRIM(ADJUSTL(sets(1)))
!      DO i=2,SIZE(sets)
!        WRITE(readtest,*) TRIM(readtest)//sets(i)
!      ENDDO

      SET_PREFIX("hdf5%read()")
      ! Read a dataset (real-1)
      CALL h5%fread('groupR->memD0',testD0)
      ASSERT(testD0==refD0,'D0 Failure')
      CALL h5%fread('groupR->memD1',testD1)
      ASSERT(ALL(testD1==refD1),'D1 Failure')
      CALL h5%fread('groupR->memD2',testD2)
      ASSERT(ALL(testD2==refD2),'D2 Failure')
      CALL h5%fread('groupR->memD3',testD3)
      ASSERT(ALL(testD3==refD3),'D3 Failure')
      CALL h5%fread('groupR->memS0',testS0)
      ASSERT(testS0==refS0,'S0 Failure')
      CALL h5%fread('groupR->memS1',testS1)
      ASSERT(ALL(testS1==refS1),'S1 Failure')
      CALL h5%fread('groupR->memS2',testS2)
      ASSERT(ALL(testS2==refS2),'S2 Failure')
      CALL h5%fread('groupR->memS3',testS3)
      ASSERT(ALL(testS3==refS3),'S3 Failure')
      CALL h5%fread('groupI->memN0',testN0)
      ASSERT(testN0==refN0,'N0 Failure')
      CALL h5%fread('groupI->memN1',testN1)
      ASSERT(ALL(testN1==refN1),'N1 Failure')
      CALL h5%fread('groupI->memN2',testN2)
      ASSERT(ALL(testN2==refN2),'N2 Failure')
      CALL h5%fread('groupI->memN3',testN3)
      ASSERT(ALL(testN3==refN3),'N3 Failure')
      CALL h5%fread('groupI->memL0',testL0)
      ASSERT(testL0==refL0,'L0 Failure')
      FINFO()testL0,refL0
      CALL h5%fread('groupI->memL1',testL1)
      ASSERT(ALL(testL1==refL1),'L1 Failure')
      CALL h5%fread('groupI->memL2',testL2)
      ASSERT(ALL(testL2==refL2),'L2 Failure')
      CALL h5%fread('groupI->memL3',testL3)
      ASSERT(ALL(testL3==refL3),'L3 Failure')
      CALL h5%fread('groupB->memB0',testB0)
      ASSERT(testB0.EQV.refB0,'B0 Failure')
      CALL h5%fread('groupB->memB1',testB1)
      ASSERT(ALL(testB1.EQV.refB1),'B1 Failure')
      CALL h5%fread('groupB->memB2',testB2)
      ASSERT(ALL(testB2.EQV.refB2),'B2 Failure')
      CALL h5%fread('groupB->memB3',testB3)
      ASSERT(ALL(testB3.EQV.refB3),'B3 Failure')
      CALL h5%fread('groupC->memC0',testC0)
      ASSERT(testC0==refC0,'C0 Failure')
      CALL h5%fread('groupC->memC1',testC1)
      checkread=.TRUE.
      DO i=1,SIZE(refC1)
        IF(testC1(i)/=refC1(i)) checkread=.FALSE.
      ENDDO
      ASSERT(checkread,'C1 Failure')
      CALL h5%fread('groupC->memC2',testC2)
      checkread=.TRUE.
      DO i=1,SIZE(refC2,1)
        DO j=1,SIZE(refC2,2)
          IF(testC2(i,j)/=refC2(i,j)) checkread=.FALSE.
        ENDDO
      ENDDO
      ASSERT(checkread,'C2 Failure')
      CALL h5%fread('groupC->memC3',testC3)
      checkread=.TRUE.
      DO i=1,SIZE(refC3,1)
        DO j=1,SIZE(refC3,2)
          DO k=1,SIZE(refC3,3)
            IF(testC3(i,j,k)/=refC3(i,j,k)) checkread=.FALSE.
          ENDDO
        ENDDO
      ENDDO
      ASSERT(checkread,'C3 Failure')

!      WRITE(readtest,*) TRIM(readtest),'testD0:'; WRITE(readtest,FMT=*)TRIM(readtest),testD0
!      WRITE(readtest,*) TRIM(readtest),'testD1:'; WRITE(readtest,FMT=*)TRIM(readtest),testD1
!      WRITE(readtest,*) TRIM(readtest),'testD2:'; WRITE(readtest,FMT=*)TRIM(readtest),testD2
!      WRITE(readtest,*) TRIM(readtest),'testD3:'; WRITE(readtest,FMT=*)TRIM(readtest),testD3
!      WRITE(readtest,*) TRIM(readtest),'testS0:'; WRITE(readtest,FMT=*)TRIM(readtest),testS0
!      WRITE(readtest,*) TRIM(readtest),'testS1:'; WRITE(readtest,FMT=*)TRIM(readtest),testS1
!      WRITE(readtest,*) TRIM(readtest),'testS2:'; WRITE(readtest,FMT=*)TRIM(readtest),testS2
!      WRITE(readtest,*) TRIM(readtest),'testS3:'; WRITE(readtest,FMT=*)TRIM(readtest),testS3
!      WRITE(readtest,*) TRIM(readtest),'testN0:'; WRITE(readtest,FMT=*)TRIM(readtest),testN0
!      WRITE(readtest,*) TRIM(readtest),'testN1:'; WRITE(readtest,FMT=*)TRIM(readtest),testN1
!      WRITE(readtest,*) TRIM(readtest),'testN2:'; WRITE(readtest,FMT=*)TRIM(readtest),testN2
!      WRITE(readtest,*) TRIM(readtest),'testN3:'; WRITE(readtest,FMT=*)TRIM(readtest),testN3
!      WRITE(readtest,*) TRIM(readtest),'testL0:'; WRITE(readtest,FMT=*)TRIM(readtest),testL0
!      WRITE(readtest,*) TRIM(readtest),'testL1:'; WRITE(readtest,FMT=*)TRIM(readtest),testL1
!      WRITE(readtest,*) TRIM(readtest),'testL2:'; WRITE(readtest,FMT=*)TRIM(readtest),testL2
!      WRITE(readtest,*) TRIM(readtest),'testL3:'; WRITE(readtest,FMT=*)TRIM(readtest),testL3
!      WRITE(readtest,*) TRIM(readtest),'testB0:'; WRITE(readtest,FMT=*)TRIM(readtest),testB0
!      WRITE(readtest,*) TRIM(readtest),'testB1:'; WRITE(readtest,FMT=*)TRIM(readtest),testB1
!      WRITE(readtest,*) TRIM(readtest),'testB2:'; WRITE(readtest,FMT=*)TRIM(readtest),testB2
!      WRITE(readtest,*) TRIM(readtest),'testB3:'; WRITE(readtest,FMT=*)TRIM(readtest),testB3
!      WRITE(readtest,*) TRIM(readtest),'testC0:'; WRITE(1,FMT=*)TRIM(readtest)//TRIM(testC0)
!      WRITE(readtest,*) TRIM(readtest),'testC1:';
!      DO i=1,SIZE(testC1)
!        WRITE(readtest,*)TRIM(readtest),CHAR(testC1(i))
!      ENDDO
!      WRITE(readtest,*) TRIM(readtest),'testC2:';
!      DO i=1,SIZE(testC2,1)
!        DO j=1,SIZE(testC2,2)
!          WRITE(readtest,*)TRIM(readtest),CHAR(testC2(i,j))
!        ENDDO
!      ENDDO
!      WRITE(readtest,*) TRIM(readtest),'testC3:';
!      DO i=1,SIZE(testC3,1)
!        DO j=1,SIZE(testC3,2)
!          DO k=1,SIZE(testC3,3)
!            WRITE(readtest,*)TRIM(readtest),CHAR(testC3(i,j,k))
!          ENDDO
!        ENDDO
!      ENDDO

      DEALLOCATE(testD1,testD2,testD3,testS1,testS2,testS3,testL1,testL2, &
            testL3,testB1,testB2,testB3,testC1,testC2,testC3,testN1,testN2, &
            testN3,refD1,refD2,refD3,refS1,refS2,refS3,refL1,refL2, &
            refL3,refB1,refB2,refB3,refC1,refC2,refC3,refN1,refN2, &
            refN3)

      CALL h5%clear()
      ASSERT(.NOT.h5%isinit, 'HDF5 object not properly cleared!')

 !     CALL testHDF5_read_check(readcheck)
 !     checkread=.TRUE.
 !     DO i=1,LEN(readtest)
 !       IF (readtest(i:i) /= readcheck(i:i)) THEN
 !         checkread=.FALSE.
 !         WRITE(*,*) i, readtest(i:i), readcheck(i:i)
 !       ENDIF
 !     ENDDO
 !
 !     ASSERT(checkread, 'HDF5 file not properly read!')

    ENDSUBROUTINE testHDF5FileTypeRead
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5_wo()
      TYPE(HDF5FileType) :: h5

      CALL h5%init('test2.h5','NEW')
      CALL h5%clear()
    ENDSUBROUTINE testHDF5_wo
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5_read_check(readtest)
      CHARACTER(LEN=*), INTENT(INOUT) :: readtest

      WRITE(readtest,FMT=*) '                                          '// &
      '                                                                  '// &
      '            memD0memD1memD2memD3memS0memS1memS2memS3testD0:   42.1'// &
      '23456789000002testD1:   1.0000000001000000        2.00000000020000'// &
      '00        3.0000000003000000        4.0000000004000000        5.00'// &
      '00000005000000        6.0000000006000000        7.0000000007000001'// &
      '        8.0000000008000001        9.0000000009000001        10.000'// &
      '000001000000testD2:   1.0000000001000000        2.0000000002000000'// &
      '        3.0000000003000000        4.0000000004000000        5.0000'// &
      '000005000000        6.0000000006000000        7.0000000007000001  '// &
      '      8.0000000008000001        9.0000000009000001        10.00000'// &
      '0001000000        11.000000001100000        12.000000001200000    '// &
      '    13.000000001300000        14.000000001400000        15.0000000'// &
      '01500000        16.000000001600000        17.000000001700002      '// &
      '  18.000000001800000        19.000000001899998        20.000000002'// &
      '000000testD3:   1.0000000001000000        2.0000000002000000      '// &
      '  3.0000000003000000        4.0000000004000000        5.0000000005'// &
      '000000        6.0000000006000000        7.0000000007000001        '// &
      '8.0000000008000001        9.0000000009000001        10.00000000100'// &
      '0000        11.000000001100000        12.000000001200000        13'// &
      '.000000001300000        14.000000001400000        15.0000000015000'// &
      '00        16.000000001600000        17.000000001700002        18.0'// &
      '00000001800000        19.000000001899998        20.000000002000000'// &
      '        21.000000002100002        22.000000002200000        23.000'// &
      '000002299998        24.000000002400000        25.000000002500002  '// &
      '      26.000000002600000        27.000000002699998        28.00000'// &
      '0002800000        29.000000002900002        30.000000003000000    '// &
      '    31.000000003099998        32.000000003200000        33.0000000'// &
      '03300002        34.000000003400004        35.000000003499999      '// &
      '  36.000000003600000        37.000000003700002        38.000000003'// &
      '799997        39.000000003899999        40.000000004000000        '// &
      '41.000000004100002        42.000000004200004        43.00000000429'// &
      '9999        44.000000004400000        45.000000004500002        46'// &
      '.000000004599997        47.000000004699999        48.0000000048000'// &
      '00        49.000000004900002        50.000000005000004        51.0'// &
      '00000005099999        52.000000005200000        53.000000005300002'// &
      '        54.000000005399997        55.000000005499999        56.000'// &
      '000005600000        57.000000005700002        58.000000005800004  '// &
      '      59.000000005899999        60.000000006000000testS0:   42.000'// &
      '0000testS1:   1.00000000       2.00000000       3.00000000       4'// &
      '.00000000       5.00000000       6.00000000       7.00000000      '// &
      ' 8.00000000       9.00000000       10.0000000testS2:   1.00000000 '// &
      '      2.00000000       3.00000000       4.00000000       5.0000000'// &
      '0       6.00000000       7.00000000       8.00000000       9.00000'// &
      '000       10.0000000       11.0000000       12.0000000       13.00'// &
      '00000       14.0000000       15.0000000       16.0000000       17.'// &
      '0000000       18.0000000       19.0000000       20.0000000testS3: '// &
      '  1.00000000       2.00000000       3.00000000       4.00000000   '// &
      '    5.00000000       6.00000000       7.00000000       8.00000000 '// &
      '      9.00000000       10.0000000       11.0000000       12.000000'// &
      '0       13.0000000       14.0000000       15.0000000       16.0000'// &
      '000       17.0000000       18.0000000       19.0000000       20.00'// &
      '00000       21.0000000       22.0000000       23.0000000       24.'// &
      '0000000       25.0000000       26.0000000       27.0000000       2'// &
      '8.0000000       29.0000000       30.0000000       31.0000000      '// &
      ' 32.0000000       33.0000000       34.0000000       35.0000000    '// &
      '   36.0000000       37.0000000       38.0000000       39.0000000  '// &
      '     40.0000000       41.0000000       42.0000000       43.0000000'// &
      '       44.0000000       45.0000000       46.0000000       47.00000'// &
      '00       48.0000000       49.0000000       50.0000000       51.000'// &
      '0000       52.0000000       53.0000000       54.0000000       55.0'// &
      '000000       56.0000000       57.0000000       58.0000000       59'// &
      '.0000000       60.0000000testN0:          42testN1:           1   '// &
      '        2           3           4           5           6         '// &
      '  7           8           9          10testN2:           1        '// &
      '   2           3           4           5           6           7  '// &
      '         8           9          10          11          12        '// &
      '  13          14          15          16          17          18  '// &
      '        19          20testN3:           1           2           3 '// &
      '          4           5           6           7           8       '// &
      '    9          10          11          12          13          14 '// &
      '         15          16          17          18          19       '// &
      '   20          21          22          23          24          25 '// &
      '         26          27          28          29          30       '// &
      '   31          32          33          34          35          36 '// &
      '         37          38          39          40          41       '// &
      '   42          43          44          45          46          47 '// &
      '         48          49          50          51          52       '// &
      '   53          54          55          56          57          58 '// &
      '         59          60testL0:   123456789123456784testL1:        '// &
      '   1000000001           1000000002           1000000003           '// &
      '1000000004           1000000005           1000000006           100'// &
      '0000007           1000000008           1000000009           100000'// &
      '0010testL2:           1000000001           1000000002           10'// &
      '00000003           1000000004           1000000005           10000'// &
      '00006           1000000007           1000000008           10000000'// &
      '09           1000000010           1000000011           1000000012 '// &
      '          1000000013           1000000014           1000000015    '// &
      '       1000000016           1000000017           1000000018       '// &
      '    1000000019           1000000020testL3:           1000000001   '// &
      '        1000000002           1000000003           1000000004      '// &
      '     1000000005           1000000006           1000000007         '// &
      '  1000000008           1000000009           1000000010           1'// &
      '000000011           1000000012           1000000013           1000'// &
      '000014           1000000015           1000000016           1000000'// &
      '017           1000000018           1000000019           1000000020'// &
      '           1000000021           1000000022           1000000023   '// &
      '        1000000024           1000000025           1000000026      '// &
      '     1000000027           1000000028           1000000029         '// &
      '  1000000030           1000000031           1000000032           1'// &
      '000000033           1000000034           1000000035           1000'// &
      '000036           1000000037           1000000038           1000000'// &
      '039           1000000040           1000000041           1000000042'// &
      '           1000000043           1000000044           1000000045   '// &
      '        1000000046           1000000047           1000000048      '// &
      '     1000000049           1000000050           1000000051         '// &
      '  1000000052           1000000053           1000000054           1'// &
      '000000055           1000000056           1000000057           1000'// &
      '000058           1000000059           1000000060testB0: FtestB1: F'// &
      ' T F T F TtestB2: F T F T F T T T T T T T T T T T T T F T F T F T '// &
      'T T T T T TtestB3: F T F T F T T T T T T T T T T T T T F T F T F T'// &
      ' T T T T T T F T F T F T T T T T T T T T T T T T F T F T F T T T T'// &
      ' T T TtestC0:testC1:String 1String 2String 3testC2:String 1,1Strin'// &
      'g 1,2String 1,3String 2,1String 2,2String 2,3testC3:String 1,1,1St'// &
      'ring 1,1,2String 1,1,3String 1,1,4String 1,1,5String 1,2,1String 1'// &
      ',2,2String 1,2,3String 1,2,4String 1,2,5String 1,3,1String 1,3,2St'// &
      'ring 1,3,3String 1,3,4String 1,3,5String 1,4,1String 1,4,2String 1'// &
      ',4,3String 1,4,4String 1,4,5String 2,1,1String 2,1,2String 2,1,3St'// &
      'ring 2,1,4String 2,1,5String 2,2,1String 2,2,2String 2,2,3String 2'// &
      ',2,4String 2,2,5String 2,3,1String 2,3,2String 2,3,3String 2,3,4St'// &
      'ring 2,3,5String 2,4,1String 2,4,2String 2,4,3String 2,4,4String 2'// &
      ',4,5String 3,1,1String 3,1,2String 3,1,3String 3,1,4String 3,1,5St'// &
      'ring 3,2,1String 3,2,2String 3,2,3String 3,2,4String 3,2,5String 3'// &
      ',3,1String 3,3,2String 3,3,3String 3,3,4String 3,3,5String 3,4,1St'// &
      'ring 3,4,2String 3,4,3String 3,4,4String 3,4,5'!                    '// &
!      '                                                                  '// &
!      '                                                                  '// &
!      '                                                                  '// &
!      '                                                                  '// &
!      '                                                                  '// &
!      '                                                                  '// &
!      '                                            '
    END SUBROUTINE testHDF5_read_check
!'
!-------------------------------------------------------------------------------

ENDPROGRAM testHDF5
