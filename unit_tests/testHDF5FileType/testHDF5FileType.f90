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
PROGRAM testHDF5
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE IntrType
  USE Strings
  USE ExceptionHandler
  USE ParallelEnv
  USE FileType_HDF5
  
  IMPLICIT NONE
  
  TYPE(MPI_EnvType) :: testMPI
  INTEGER(SIK) :: mpierr,mysize,myrank
  !  Reference solutions
  REAL(SDK),ALLOCATABLE :: refD1(:),refD2(:,:),refD3(:,:,:),refD4(:,:,:,:)
  REAL(SSK),ALLOCATABLE :: refS1(:),refS2(:,:),refS3(:,:,:),refS4(:,:,:,:)
  LOGICAL(SBK),ALLOCATABLE :: refB1(:),refB2(:,:),refB3(:,:,:)
  INTEGER(SLK),ALLOCATABLE :: refL1(:),refL2(:,:),refL3(:,:,:)
  INTEGER(SNK),ALLOCATABLE :: refN1(:),refN2(:,:),refN3(:,:,:)
  REAL(SDK) :: refD0
  REAL(SSK) :: refS0
  INTEGER(SLK) :: refL0
  INTEGER(SNK) :: refN0
  LOGICAL(SBK) :: refB0
  TYPE(StringType) :: refST0
  TYPE(StringType),ALLOCATABLE :: refST1(:),refST2(:,:),refST3(:,:,:)
  CHARACTER(LEN=32) :: refC1
  CHARACTER(LEN=12) :: helper_string
  LOGICAL(SBK) :: exists
  TYPE(StringType),ALLOCATABLE :: refsets(:)

  CALL testMPI%init(PE_COMM_WORLD)
  IF(testMPI%rank /= 0)  utest_master=.FALSE.

  CREATE_TEST("HDF File Type")
  
  CALL testHDF5FileTypeSetup()
  REGISTER_SUBTEST("HDF5FileType Initialization Checks",testHDF5FileTypeCreateDelete)
  REGISTER_SUBTEST("HDF5FileType Error Checks",testHDF5FileTypeErrorCheck)
  REGISTER_SUBTEST("HDF5FileType Read",testHDF5FileTypeRead)
!  IF(utest_nfail==0) THEN
    REGISTER_SUBTEST("HDF5FileType Write",testHDF5FileTypeWrite)
!  ELSE
!    WRITE(*,*) '-----------------------------------------------------'// &
!        '--------------------'
!    WRITE(*,*) "HDF5FileType Read subtest failed: HDF5FileType Write "// &
!        "subtest will not run"
!    WRITE(*,*) '-----------------------------------------------------'// &
!        '--------------------'
!  ENDIF
  DEALLOCATE(refD1,refD2,refD3,refD4,refS1,refS2,refS3,refS4,refB1,refB2,refB3,&
      refL1,refL2,refL3,refN1,refN2,refN3,refST1,refST2,refST3,refsets)

  FINALIZE_TEST()
  CALL testMPI%clear()
  CALL testMPI%finalize()

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeSetup()
      INTEGER(SIK) :: i,j,k

      ALLOCATE(refD1(10))
      ALLOCATE(refD2(4,5))
      ALLOCATE(refD3(3,4,5))
      ALLOCATE(refD4(3,4,5,6))
      ALLOCATE(refS1(10))
      ALLOCATE(refS2(4,5))
      ALLOCATE(refS3(3,4,5))
      ALLOCATE(refS4(3,4,5,6))
      ALLOCATE(refB1(6))
      ALLOCATE(refB2(6,5))
      ALLOCATE(refB3(6,5,2))
      ALLOCATE(refL1(10))
      ALLOCATE(refL2(4,5))
      ALLOCATE(refL3(3,4,5))
      ALLOCATE(refN1(10))
      ALLOCATE(refN2(4,5))
      ALLOCATE(refN3(3,4,5))
      ALLOCATE(refST1(3))
      ALLOCATE(refST2(2,3))
      ALLOCATE(refST3(3,4,5))
      ALLOCATE(refsets(10))

      refD0=42.123456789_SDK
      refD1=[(i*1.0000000001_SDK,i=1,SIZE(refD1))]
      refD2=RESHAPE([(i*1.0000000001_SDK,i=1,SIZE(refD2))],SHAPE(refD2))
      refD3=RESHAPE([(i*1.0000000001_SDK,i=1,SIZE(refD3))],SHAPE(refD3))
      refD4=RESHAPE([(i*1.0000000001_SDK,i=1,SIZE(refD4))],SHAPE(refD4))
      refS0=42.000_SSK
      refS1=[(i,i=1,SIZE(refS1))]
      refS2=RESHAPE([(i,i=1,SIZE(refS2))],SHAPE(refS2))
      refS3=RESHAPE([(i,i=1,SIZE(refS3))],SHAPE(refS3))
      refS4=RESHAPE([(i,i=1,SIZE(refS4))],SHAPE(refS4))
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
      refST0='Rank-0 (Not-an-array) String Test'
      DO i=1,SIZE(refST1)
        WRITE(helper_string,FMT="(a7,i0)") 'String ',i
        refST1(i)=helper_string
      ENDDO
      DO i=1,SIZE(refST2,1)
        DO j=1,SIZE(refST2,2)
          WRITE(helper_string,FMT="(a7,i0,a1,i0)") 'String ',i,',',j
          refST2(i,j)=helper_string
        ENDDO
      ENDDO
      refST2(1,1)='String 1,1';refST2(1,2)='String 1,2';refST2(2,1)='String 2,1'
      refST2(2,2)='String 2,2';
      DO i=1,SIZE(refST3,1)
        DO j=1,SIZE(refST3,2)
          DO k=1,SIZE(refST3,3)
            WRITE(helper_string,FMT="(a7,i0,a1,i0,a1,i0)") 'String ',i,&
                    &",",j,",",k
            refST3(i,j,k)=helper_string
          ENDDO
        ENDDO
      ENDDO
      refC1='MPACT string test'; refC1=ADJUSTL(refC1)
      
      refsets(1)='memD0'
      refsets(2)='memD1'
      refsets(3)='memD2'
      refsets(4)='memD3'
      refsets(5)='memD4'
      refsets(6)='memS0'
      refsets(7)='memS1'
      refsets(8)='memS2'
      refsets(9)='memS3'
      refsets(10)='memS4'
    
    ENDSUBROUTINE testHDF5FileTypeSetup
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeCreateDelete()
      TYPE(HDF5FileType) :: h5

      CALL h5%init('createdeletetest.h5','NEW')
      ASSERT(h5%isinit,'HDF5 object no properly initialized')
      IF(h5%isinit) exists=.TRUE.

      CALL h5%fclose()
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR) == 0,'hdf5_filetype%fclose')
      CALL h5%clear()
      ASSERT(.NOT.h5%isinit,'HDF5 object not properly cleared.')
      CALL h5%init('createdeletetest.h5','WRITE')
      ASSERT(h5%isWrite(),'HDF object %isWrite() should be .TRUE.')
      CALL h5%mkdir('testGroup')
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR) == 0,'hdf5_filetype%init ''WRITE''')
      CALL h5%clear()
      CALL h5%init('createdeletetest.h5','READ')
      ASSERT(.NOT.(h5%isWrite()),'HDF object %isWrite() should be .FALSE.')
      CALL h5%e%setStopOnError(.FALSE.)
      CALL h5%mkdir('testGroup')
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR) == 1,'hdf5_filetype%init ''READ''')
      FINFO() h5%e%getCounter(EXCEPTION_ERROR)
      CALL h5%fdelete()
      INQUIRE(FILE='createdeletetest.h5',EXIST=exists)
      ASSERT(.NOT.h5%isinit,'HDF5 object still initialized after deletion.')
      ASSERT(.NOT.exists,'HDF5 object not properly deleted without being cleared.')
      
      CALL h5%init('createdeletetest.h5','NEW')
      IF(h5%isinit) exists=.TRUE.
      CALL h5%fdelete()
      INQUIRE(FILE='createdeletetest.h5',EXIST=exists)
      ASSERT(.NOT.exists,'HDF5 object not properly deleted after begin cleared.')

    ENDSUBROUTINE testHDF5FileTypeCreateDelete
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeErrorCheck()
      TYPE(HDF5FileType) :: h5
      CHARACTER(LEN=32),ALLOCATABLE :: tmpchar(:)
      REAL(SDK),POINTER :: d4ptr(:,:,:,:)

      CALL h5%fwrite('groupR->memD0',refD0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==1,'%write_d0 %isinit check')
      CALL h5%fwrite('groupR->memD1',refD1,SHAPE(refD1))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==2,'%write_d1 %isinit check')
      CALL h5%fwrite('groupR->memD2',refD2,SHAPE(refD2))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==3,'%write_d2 %isinit check')
      CALL h5%fwrite('groupR->memD3',refD3,SHAPE(refD3))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==4,'%write_d3 %isinit check')
      CALL h5%fwrite('groupR->memD4',refD4,SHAPE(refD4))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==5,'%write_d4 %isinit check')
      CALL h5%fwrite('groupR->memS0',refS0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==6,'%write_s0 %isinit check')
      CALL h5%fwrite('groupR->memS1',refS1,SHAPE(refS1))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==7,'%write_s1 %isinit check')
      CALL h5%fwrite('groupR->memS2',refS2,SHAPE(refS2))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==8,'%write_s2 %isinit check')
      CALL h5%fwrite('groupR->memS3',refS3,SHAPE(refS3))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==9,'%write_s3 %isinit check')
      CALL h5%fwrite('groupR->memS4',refS4,SHAPE(refS4))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==10,'%write_s4 %isinit check')
      CALL h5%fwrite('groupI->memL0',refL0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==11,'%write_l0 %isinit check')
      CALL h5%fwrite('groupI->memL1',refL1,SHAPE(refL1))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==12,'%write_l1 %isinit check')
      CALL h5%fwrite('groupI->memL2',refL2,SHAPE(refL2))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==13,'%write_l2 %isinit check')
      CALL h5%fwrite('groupI->memL3',refL3,SHAPE(refL3))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==14,'%write_l3 %isinit check')
      CALL h5%fwrite('groupI->memN0',refN0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==15,'%write_n0 %isinit check')
      CALL h5%fwrite('groupI->memN1',refN1,SHAPE(refN1))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==16,'%write_n1 %isinit check')
      CALL h5%fwrite('groupI->memN2',refN2,SHAPE(refN2))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==17,'%write_n2 %isinit check')
      CALL h5%fwrite('groupI->memN3',refN3,SHAPE(refN3))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==18,'%write_n3 %isinit check')
      CALL h5%fwrite('groupB->memB0',refB0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==19,'%write_b0 %isinit check')
      CALL h5%fwrite('groupB->memB1',refB1,SHAPE(refB1))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==20,'%write_b1 %isinit check')
      CALL h5%fwrite('groupB->memB2',refB2,SHAPE(refB2))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==21,'%write_b2 %isinit check')
      CALL h5%fwrite('groupB->memB3',refB3,SHAPE(refB3))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==22,'%write_b3 %isinit check')
      CALL h5%fwrite('groupST->memST0',refST0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==23,'%write_st0 %isinit check')
      CALL h5%fwrite('groupST->memST1',refST1,SHAPE(refST1))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==24,'%write_st1 %isinit check')
      CALL h5%fwrite('groupST->memST2',refST2,SHAPE(refST2))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==25,'%write_st2 %isinit check')
      CALL h5%fwrite('groupST->memST3',refST3,SHAPE(refST3))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==26,'%write_st3 %isinit check')
      CALL h5%fwrite('groupC->memC1',refC1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==27,'%write_c1 %isinit check')
      CALL h5%mkdir('groupI')
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==28,'%mkdir %isinit check')
      CALL h5%ls('groupR',tmpchar)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==29,'%ls %isinit check')

      
      CALL h5%fread('groupR->memD0',refD0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==30,'%write_d0 %isinit check')
      CALL h5%fread('groupR->memD1',refD1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==31,'%write_d1 %isinit check')
      CALL h5%fread('groupR->memD2',refD2)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==32,'%write_d2 %isinit check')
      CALL h5%fread('groupR->memD3',refD3)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==33,'%write_d3 %isinit check')
      CALL h5%fread('groupR->memD4',refD4)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==34,'%write_d4 %isinit check')
      CALL h5%freadp('groupR->memD4',d4ptr)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==35,'%write_d4 %isinit check')
      CALL h5%fread('groupR->memS0',refS0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==36,'%write_s0 %isinit check')
      CALL h5%fread('groupR->memS1',refS1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==37,'%write_s1 %isinit check')
      CALL h5%fread('groupR->memS2',refS2)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==38,'%write_s2 %isinit check')
      CALL h5%fread('groupR->memS3',refS3)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==39,'%write_s3 %isinit check')
      CALL h5%fread('groupR->memS4',refS4)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==40,'%write_s4 %isinit check')
      CALL h5%fread('groupI->memL0',refL0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==41,'%write_l0 %isinit check')
      CALL h5%fread('groupI->memL1',refL1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==42,'%write_l1 %isinit check')
      CALL h5%fread('groupI->memL2',refL2)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==43,'%write_l2 %isinit check')
      CALL h5%fread('groupI->memL3',refL3)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==44,'%write_l3 %isinit check')
      CALL h5%fread('groupI->memN0',refN0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==45,'%write_n0 %isinit check')
      CALL h5%fread('groupI->memN1',refN1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==46,'%write_n1 %isinit check')
      CALL h5%fread('groupI->memN2',refN2)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==47,'%write_n2 %isinit check')
      CALL h5%fread('groupI->memN3',refN3)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==48,'%write_n3 %isinit check')
      CALL h5%fread('groupB->memB0',refB0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==49,'%write_b0 %isinit check')
      CALL h5%fread('groupB->memB1',refB1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==50,'%write_b1 %isinit check')
      CALL h5%fread('groupB->memB2',refB2)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==51,'%write_b2 %isinit check')
      CALL h5%fread('groupB->memB3',refB3)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==52,'%write_b3 %isinit check')
      CALL h5%fread('groupST->memST0',refST0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==53,'%write_st0 %isinit check')
      CALL h5%fread('groupST->memST1',refST1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==54,'%write_st1 %isinit check')
      CALL h5%fread('groupST->memST2',refST2)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==55,'%write_st2 %isinit check')
      CALL h5%fread('groupST->memST3',refST3)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==56,'%write_st3 %isinit check')



    ENDSUBROUTINE testHDF5FileTypeErrorCheck
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeWrite()
      TYPE(HDF5FileType) :: h5
      REAL(SDK),ALLOCATABLE :: testD1(:),testD2(:,:),testD3(:,:,:),testD4(:,:,:,:)
      REAL(SSK),ALLOCATABLE :: testS1(:),testS2(:,:),testS3(:,:,:),testS4(:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: testB1(:),testB2(:,:),testB3(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: testL1(:),testL2(:,:),testL3(:,:,:)
      INTEGER(SNK),ALLOCATABLE :: testN1(:),testN2(:,:),testN3(:,:,:)
      TYPE(StringType),ALLOCATABLE :: testST1(:),testST2(:,:),testST3(:,:,:)
      REAL(SDK) :: testD0
      REAL(SSK) :: testS0
      INTEGER(SLK) :: testL0
      INTEGER(SNK) :: testN0
      LOGICAL(SBK) :: testB0
      CHARACTER(LEN=32) :: testC1
      TYPE(StringType) :: testST0
      INTEGER(SIK) :: i,j,k
      LOGICAL(SBK) :: checkwrite

      COMPONENT_TEST('%fwrite with gdims')
      ! Create a RW access file. Existing file overwritten
      CALL h5%init('writetest.h5','NEW')

      ! Test writing with the gdims arguments
      CALL h5%mkdir('groupR')
      CALL h5%fwrite('groupR->memD0',refD0)
      CALL h5%fwrite('groupR->memD1',refD1,SHAPE(refD1))
      CALL h5%fwrite('groupR->memD2',refD2,SHAPE(refD2))
      CALL h5%fwrite('groupR->memD3',refD3,SHAPE(refD3))
      CALL h5%fwrite('groupR->memD4',refD4,SHAPE(refD4))
      CALL h5%fwrite('groupR->memS0',refS0)
      CALL h5%fwrite('groupR->memS1',refS1,SHAPE(refS1))
      CALL h5%fwrite('groupR->memS2',refS2,SHAPE(refS2))
      CALL h5%fwrite('groupR->memS3',refS3,SHAPE(refS3))
      CALL h5%fwrite('groupR->memS4',refS4,SHAPE(refS4))
      CALL h5%mkdir('groupI')
      CALL h5%fwrite('groupI->memL0',refL0)
      CALL h5%fwrite('groupI->memL1',refL1,SHAPE(refL1))
      CALL h5%fwrite('groupI->memL2',refL2,SHAPE(refL2))
      CALL h5%fwrite('groupI->memL3',refL3,SHAPE(refL3))
      CALL h5%fwrite('groupI->memN0',refN0)
      CALL h5%fwrite('groupI->memN1',refN1,SHAPE(refN1))
      CALL h5%fwrite('groupI->memN2',refN2,SHAPE(refN2))
      CALL h5%fwrite('groupI->memN3',refN3,SHAPE(refN3))
      CALL h5%mkdir('groupB')
      CALL h5%fwrite('groupB->memB0',refB0)
      CALL h5%fwrite('groupB->memB1',refB1,SHAPE(refB1))
      CALL h5%fwrite('groupB->memB2',refB2,SHAPE(refB2))
      CALL h5%fwrite('groupB->memB3',refB3,SHAPE(refB3))
      CALL h5%mkdir('groupST')
      CALL h5%fwrite('groupST->memST0',refST0)
      CALL h5%fwrite('groupST->memST1',refST1,SHAPE(refST1))
      CALL h5%fwrite('groupST->memST2',refST2,SHAPE(refST2))
      CALL h5%fwrite('groupST->memST3',refST3,SHAPE(refST3))
      CALL h5%mkdir('groupC')
      CALL h5%fwrite('groupC->memC1',refC1,LEN(refC1))
      
      CALL h5%fread('groupR->memD0',testD0)
      ASSERT(testD0==refD0,'D0 Write Failure with gdims_in')
      CALL h5%fread('groupR->memD1',testD1)
      ASSERT(ALL(testD1==refD1),'D1 Write Failure with gdims_in')
      CALL h5%fread('groupR->memD2',testD2)
      ASSERT(ALL(testD2==refD2),'D2 Write Failure with gdims_in')
      CALL h5%fread('groupR->memD3',testD3)
      ASSERT(ALL(testD3==refD3),'D3 Write Failure with gdims_in')
      CALL h5%fread('groupR->memD4',testD4)
      ASSERT(ALL(testD4==refD4),'D4 Write Failure with gdims_in')
      CALL h5%fread('groupR->memS0',testS0)
      ASSERT(testS0==refS0,'S0 Write Failure with gdims_in')
      CALL h5%fread('groupR->memS1',testS1)
      ASSERT(ALL(testS1==refS1),'S1 Write Failure with gdims_in')
      CALL h5%fread('groupR->memS2',testS2)
      ASSERT(ALL(testS2==refS2),'S2 Write Failure with gdims_in')
      CALL h5%fread('groupR->memS3',testS3)
      ASSERT(ALL(testS3==refS3),'S3 Write Failure with gdims_in')
      CALL h5%fread('groupR->memS4',testS4)
      ASSERT(ALL(testS4==refS4),'S4 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN0',testN0)
      ASSERT(testN0==refN0,'N0 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN1',testN1)
      ASSERT(ALL(testN1==refN1),'N1 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN2',testN2)
      ASSERT(ALL(testN2==refN2),'N2 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN3',testN3)
      ASSERT(ALL(testN3==refN3),'N3 Write Failure with gdims_in')
      CALL h5%fread('groupI->memL0',testL0)
      ASSERT(testL0==refL0,'L0 Write Failure with gdims_in')
      FINFO()testL0,refL0
      CALL h5%fread('groupI->memL1',testL1)
      ASSERT(ALL(testL1==refL1),'L1 Write Failure with gdims_in')
      CALL h5%fread('groupI->memL2',testL2)
      ASSERT(ALL(testL2==refL2),'L2 Write Failure with gdims_in')
      CALL h5%fread('groupI->memL3',testL3)
      ASSERT(ALL(testL3==refL3),'L3 Write Failure with gdims_in')
      CALL h5%fread('groupB->memB0',testB0)
      ASSERT(testB0.EQV.refB0,'B0 Write Failure with gdims_in')
      CALL h5%fread('groupB->memB1',testB1)
      ASSERT(ALL(testB1.EQV.refB1),'B1 Write Failure with gdims_in')
      CALL h5%fread('groupB->memB2',testB2)
      ASSERT(ALL(testB2.EQV.refB2),'B2 Write Failure with gdims_in')
      CALL h5%fread('groupB->memB3',testB3)
      ASSERT(ALL(testB3.EQV.refB3),'B3 Write Failure with gdims_in')
      CALL h5%fread('groupST->memST0',testST0)
      ASSERT(testST0==refST0,'ST0 Write Failure with gdims_in')
      CALL h5%fread('groupST->memST1',testST1)
      checkwrite=.TRUE.
      DO i=1,SIZE(refST1)
        IF(testST1(i)/=refST1(i)) checkwrite=.FALSE.
      ENDDO
      ASSERT(checkwrite,'ST1 Write Failure with gdims_in')
      CALL h5%fread('groupST->memST2',testST2)
      checkwrite=.TRUE.
      DO i=1,SIZE(refST2,1)
        DO j=1,SIZE(refST2,2)
          IF(testST2(i,j)/=refST2(i,j)) checkwrite=.FALSE.
        ENDDO
      ENDDO
      ASSERT(checkwrite,'ST2 Write Failure with gdims_in')
      CALL h5%fread('groupST->memST3',testST3)
      checkwrite=.TRUE.
      DO i=1,SIZE(refST3,1)
        DO j=1,SIZE(refST3,2)
          DO k=1,SIZE(refST3,3)
            IF(testST3(i,j,k)/=refST3(i,j,k)) checkwrite=.FALSE.
          ENDDO
        ENDDO
      ENDDO
      ASSERT(checkwrite,'ST3 Write Failure with gdims_in')      
      CALL h5%fread('groupC->memC1',testC1)
      ASSERT(TRIM(testC1)==TRIM(refC1),'C1 Write Failure with gdims_in')
      FINFO() ':'//testC1//':  :'//refC1//':'

      i=h5%ngrp('groupR')
      ASSERT(i == 10,'ngrp_HDF5FileType')
      FINFO() i

      ! Delete the file
      CALL h5%fdelete()

      COMPONENT_TEST('%fwrite without gdims')
      CALL h5%init('writetest.h5','NEW')

      ! Test writing without the gdims arguments
      CALL h5%mkdir('groupR')
      CALL h5%mkdir('groupI')
      CALL h5%mkdir('groupB')
      CALL h5%mkdir('groupST')
      CALL h5%mkdir('groupC')
      CALL h5%fwrite('groupR->memD0',refD0)
      CALL h5%fwrite('groupR->memD1',refD1)
      CALL h5%fwrite('groupR->memD2',refD2)
      CALL h5%fwrite('groupR->memD3',refD3)
      CALL h5%fwrite('groupR->memD4',refD4)
      CALL h5%fwrite('groupR->memS0',refS0)
      CALL h5%fwrite('groupR->memS1',refS1)
      CALL h5%fwrite('groupR->memS2',refS2)
      CALL h5%fwrite('groupR->memS3',refS3)
      CALL h5%fwrite('groupR->memS4',refS4)
      CALL h5%fwrite('groupI->memL0',refL0)
      CALL h5%fwrite('groupI->memL1',refL1)
      CALL h5%fwrite('groupI->memL2',refL2)
      CALL h5%fwrite('groupI->memL3',refL3)
      CALL h5%fwrite('groupI->memN0',refN0)
      CALL h5%fwrite('groupI->memN1',refN1)
      CALL h5%fwrite('groupI->memN2',refN2)
      CALL h5%fwrite('groupI->memN3',refN3)
      CALL h5%fwrite('groupB->memB0',refB0)
      CALL h5%fwrite('groupB->memB1',refB1)
      CALL h5%fwrite('groupB->memB2',refB2)
      CALL h5%fwrite('groupB->memB3',refB3)
      CALL h5%fwrite('groupST->memST0',refST0)
      CALL h5%fwrite('groupST->memST1',refST1)
      CALL h5%fwrite('groupST->memST2',refST2)
      CALL h5%fwrite('groupST->memST3',refST3)
      CALL h5%fwrite('groupC->memC1',refC1)
      
      CALL h5%fread('groupR->memD0',testD0)
      ASSERT(testD0==refD0,'D0 Write Failure')
      CALL h5%fread('groupR->memD1',testD1)
      ASSERT(ALL(testD1==refD1),'D1 Write Failure')
      CALL h5%fread('groupR->memD2',testD2)
      ASSERT(ALL(testD2==refD2),'D2 Write Failure')
      CALL h5%fread('groupR->memD3',testD3)
      ASSERT(ALL(testD3==refD3),'D3 Write Failure')
      CALL h5%fread('groupR->memD4',testD4)
      ASSERT(ALL(testD4==refD4),'D4 Write Failure')
      CALL h5%fread('groupR->memS0',testS0)
      ASSERT(testS0==refS0,'S0 Write Failure')
      CALL h5%fread('groupR->memS1',testS1)
      ASSERT(ALL(testS1==refS1),'S1 Write Failure')
      CALL h5%fread('groupR->memS2',testS2)
      ASSERT(ALL(testS2==refS2),'S2 Write Failure')
      CALL h5%fread('groupR->memS3',testS3)
      ASSERT(ALL(testS3==refS3),'S3 Write Failure')
      CALL h5%fread('groupR->memS4',testS4)
      ASSERT(ALL(testS4==refS4),'S4 Write Failure')
      CALL h5%fread('groupI->memN0',testN0)
      ASSERT(testN0==refN0,'N0 Write Failure')
      CALL h5%fread('groupI->memN1',testN1)
      ASSERT(ALL(testN1==refN1),'N1 Write Failure')
      CALL h5%fread('groupI->memN2',testN2)
      ASSERT(ALL(testN2==refN2),'N2 Write Failure')
      CALL h5%fread('groupI->memN3',testN3)
      ASSERT(ALL(testN3==refN3),'N3 Write Failure')
      CALL h5%fread('groupI->memL0',testL0)
      ASSERT(testL0==refL0,'L0 Write Failure')
      FINFO()testL0,refL0
      CALL h5%fread('groupI->memL1',testL1)
      ASSERT(ALL(testL1==refL1),'L1 Write Failure')
      CALL h5%fread('groupI->memL2',testL2)
      ASSERT(ALL(testL2==refL2),'L2 Write Failure')
      CALL h5%fread('groupI->memL3',testL3)
      ASSERT(ALL(testL3==refL3),'L3 Write Failure')
      CALL h5%fread('groupB->memB0',testB0)
      ASSERT(testB0.EQV.refB0,'B0 Write Failure')
      CALL h5%fread('groupB->memB1',testB1)
      ASSERT(ALL(testB1.EQV.refB1),'B1 Write Failure')
      CALL h5%fread('groupB->memB2',testB2)
      ASSERT(ALL(testB2.EQV.refB2),'B2 Write Failure')
      CALL h5%fread('groupB->memB3',testB3)
      ASSERT(ALL(testB3.EQV.refB3),'B3 Write Failure')
      CALL h5%fread('groupST->memST0',testST0)
      ASSERT(testST0==refST0,'ST0 Write Failure')
      FINFO() ':'//CHAR(testST0)//':  :'//CHAR(refST0)//':'
      CALL h5%fread('groupST->memST1',testST1)
      checkwrite=.TRUE.
      DO i=1,SIZE(refST1)
        IF(testST1(i)/=refST1(i)) checkwrite=.FALSE.
      ENDDO
      ASSERT(checkwrite,'ST1 Write Failure')
      CALL h5%fread('groupST->memST2',testST2)
      checkwrite=.TRUE.
      DO i=1,SIZE(refST2,1)
        DO j=1,SIZE(refST2,2)
          IF(testST2(i,j)/=refST2(i,j)) checkwrite=.FALSE.
        ENDDO
      ENDDO
      ASSERT(checkwrite,'ST2 Write Failure')
      CALL h5%fread('groupST->memST3',testST3)
      checkwrite=.TRUE.
      DO i=1,SIZE(refST3,1)
        DO j=1,SIZE(refST3,2)
          DO k=1,SIZE(refST3,3)
            IF(testST3(i,j,k)/=refST3(i,j,k)) checkwrite=.FALSE.
          ENDDO
        ENDDO
      ENDDO
      ASSERT(checkwrite,'ST3 Write Failure')      
      CALL h5%fread('groupC->memC1',testC1)
      ASSERT(testC1==refC1,'C1 Write Failure')
      
      i=h5%ngrp('groupR')
      ASSERT(i == 10,'ngrp_HDF5FileType')
      FINFO() i
 
      !CALL h5%fdelete()
      CALL h5%fclose()
      INQUIRE(FILE='writetest.h5',EXIST=exists)
!      ASSERT(.NOT.exists,'HDF5 object not properly deleted!')
    ENDSUBROUTINE testHDF5FileTypeWrite
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeRead()
      TYPE(HDF5FileType) :: h5
      REAL(SDK),ALLOCATABLE :: testD1(:),testD2(:,:),testD3(:,:,:),testD4(:,:,:,:)
      REAL(SDK),POINTER :: testDP4(:,:,:,:)
      REAL(SSK),ALLOCATABLE :: testS1(:),testS2(:,:),testS3(:,:,:),testS4(:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: testB1(:),testB2(:,:),testB3(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: testL1(:),testL2(:,:),testL3(:,:,:)
      INTEGER(SNK),ALLOCATABLE :: testN1(:),testN2(:,:),testN3(:,:,:)
      TYPE(StringType),ALLOCATABLE :: testST1(:),testST2(:,:),testST3(:,:,:)
      REAL(SDK) :: testD0
      REAL(SSK) :: testS0
      INTEGER(SLK) :: testL0
      INTEGER(SNK) :: testN0
      LOGICAL(SBK) :: testB0
      CHARACTER(LEN=32) :: testC1
      TYPE(StringType) :: testST0
      CHARACTER(LEN=80),ALLOCATABLE :: sets(:)
      INTEGER(SIK) :: i,j,k
      LOGICAL(SBK) :: checkread

!  Begin test
      CALL h5%init('readtest.h5','READ')

      CALL h5%ls('groupR',sets)
      DO i=1,SIZE(sets)
        ASSERT(TRIM(refsets(i))==TRIM(sets(i)),refsets(i)//' List Failure')
      ENDDO

      SET_PREFIX("hdf5%read()")
      ! Read a dataset (real-1)
      CALL h5%fread('groupR->memD0',testD0)
      ASSERT(testD0==refD0,'D0 Read Failure')
      CALL h5%fread('groupR->memD1',testD1)
      ASSERT(ALL(testD1==refD1),'D1 Read Failure')
      CALL h5%fread('groupR->memD2',testD2)
      ASSERT(ALL(testD2==refD2),'D2 Read Failure')
      CALL h5%fread('groupR->memD3',testD3)
      ASSERT(ALL(testD3==refD3),'D3 Read Failure')
      CALL h5%fread('groupR->memD4',testD4)
      ASSERT(ALL(testD4==refD4),'D4 Read Failure')
      CALL h5%freadp('groupR->memD4',testDP4)
      ASSERT(ALL(testDP4==refD4),'DP4 Read Failure')
      CALL h5%fread('groupR->memS0',testS0)
      ASSERT(testS0==refS0,'S0 Read Failure')
      CALL h5%fread('groupR->memS1',testS1)
      ASSERT(ALL(testS1==refS1),'S1 Read Failure')
      CALL h5%fread('groupR->memS2',testS2)
      ASSERT(ALL(testS2==refS2),'S2 Read Failure')
      CALL h5%fread('groupR->memS3',testS3)
      ASSERT(ALL(testS3==refS3),'S3 Read Failure')
      CALL h5%fread('groupR->memS4',testS4)
      ASSERT(ALL(testS4==refS4),'S4 Read Failure')
      CALL h5%fread('groupI->memN0',testN0)
      ASSERT(testN0==refN0,'N0 Read Failure')
      CALL h5%fread('groupI->memN1',testN1)
      ASSERT(ALL(testN1==refN1),'N1 Read Failure')
      CALL h5%fread('groupI->memN2',testN2)
      ASSERT(ALL(testN2==refN2),'N2 Read Failure')
      CALL h5%fread('groupI->memN3',testN3)
      ASSERT(ALL(testN3==refN3),'N3 Read Failure')
      CALL h5%fread('groupI->memL0',testL0)
      ASSERT(testL0==refL0,'L0 Read Failure')
      FINFO()testL0,refL0
      CALL h5%fread('groupI->memL1',testL1)
      ASSERT(ALL(testL1==refL1),'L1 Read Failure')
      CALL h5%fread('groupI->memL2',testL2)
      ASSERT(ALL(testL2==refL2),'L2 Read Failure')
      CALL h5%fread('groupI->memL3',testL3)
      ASSERT(ALL(testL3==refL3),'L3 Read Failure')
      CALL h5%fread('groupB->memB0',testB0)
      ASSERT(testB0.EQV.refB0,'B0 Read Failure')
      CALL h5%fread('groupB->memB1',testB1)
      ASSERT(ALL(testB1.EQV.refB1),'B1 Read Failure')
      CALL h5%fread('groupB->memB2',testB2)
      ASSERT(ALL(testB2.EQV.refB2),'B2 Read Failure')
      CALL h5%fread('groupB->memB3',testB3)
      ASSERT(ALL(testB3.EQV.refB3),'B3 Read Failure')
      CALL h5%fread('groupST->memST0',testST0)
      ASSERT(testST0==refST0,'ST0 Read Failure')
      CALL h5%fread('groupST->memST1',testST1)
      checkread=.TRUE.
      DO i=1,SIZE(refST1)
        IF(testST1(i)/=refST1(i)) checkread=.FALSE.
      ENDDO
      ASSERT(checkread,'ST1 Read Failure')
      CALL h5%fread('groupST->memST2',testST2)
      checkread=.TRUE.
      DO i=1,SIZE(refST2,1)
        DO j=1,SIZE(refST2,2)
          IF(testST2(i,j)/=refST2(i,j)) checkread=.FALSE.
        ENDDO
      ENDDO
      ASSERT(checkread,'ST2 Read Failure')
      CALL h5%fread('groupST->memST3',testST3)
      checkread=.TRUE.
      DO i=1,SIZE(refST3,1)
        DO j=1,SIZE(refST3,2)
          DO k=1,SIZE(refST3,3)
            IF(testST3(i,j,k)/=refST3(i,j,k)) checkread=.FALSE.
          ENDDO
        ENDDO
      ENDDO
      ASSERT(checkread,'ST3 Read Failure')
      CALL h5%fread('groupC->memC1',testC1)
      ASSERT(testC1 == refC1,'C1 Read Failure')

      DEALLOCATE(testD1,testD2,testD3,testS1,testS2,testS3,testL1,testL2, &
            testL3,testB1,testB2,testB3,testST1,testST2,testST3,testN1,testN2, &
            testN3,testD4,testS4,testDP4)

      CALL h5%clear()
      ASSERT(.NOT.h5%isinit, 'HDF5 object not properly cleared!')

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

ENDPROGRAM testHDF5
