!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testHDF5
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE IntrType
  USE Strings
  USE ExceptionHandler
  USE ParameterLists
  USE ParallelEnv
  USE FileType_HDF5

  IMPLICIT NONE

  !TYPE(ExceptionHandlerType),TARGET,SAVE :: e
  TYPE(MPI_EnvType) :: testMPI
  !  Reference solutions
  REAL(SDK),ALLOCATABLE :: refD1(:),refD2(:,:),refD3(:,:,:),refD4(:,:,:,:)
  REAL(SDK),ALLOCATABLE :: refD5(:,:,:,:,:),refD6(:,:,:,:,:,:),refD7(:,:,:,:,:,:,:)
  REAL(SSK),ALLOCATABLE :: refS1(:),refS2(:,:),refS3(:,:,:),refS4(:,:,:,:)
  REAL(SSK),ALLOCATABLE :: refS5(:,:,:,:,:),refS6(:,:,:,:,:,:),refS7(:,:,:,:,:,:,:)
  LOGICAL(SBK),ALLOCATABLE :: refB1(:),refB2(:,:),refB3(:,:,:)
  INTEGER(SLK),ALLOCATABLE :: refL1(:),refL2(:,:),refL3(:,:,:),refL4(:,:,:,:)
  INTEGER(SLK),ALLOCATABLE :: refL5(:,:,:,:,:),refL6(:,:,:,:,:,:),refL7(:,:,:,:,:,:,:)
  INTEGER(SNK),ALLOCATABLE :: refN1(:),refN2(:,:),refN3(:,:,:),refN4(:,:,:,:)
  INTEGER(SNK),ALLOCATABLE :: refN5(:,:,:,:,:),refN6(:,:,:,:,:,:),refN7(:,:,:,:,:,:,:)
  REAL(SDK) :: refD0
  REAL(SSK) :: refS0
  INTEGER(SLK) :: refL0
  INTEGER(SNK) :: refN0
  LOGICAL(SBK) :: refB0
  TYPE(StringType) :: refST0
  TYPE(StringType),ALLOCATABLE :: refST1(:),refSTC1(:),refST0CA(:),refST2(:,:),refST3(:,:,:)
  CHARACTER(LEN=32) :: refC1
  CHARACTER(LEN=12) :: helper_string
  LOGICAL(SBK) :: exists
  TYPE(StringType),ALLOCATABLE :: refsets(:)
  TYPE(HDF5FileType) :: h5file

  CALL testMPI%init(PE_COMM_WORLD)
  IF(testMPI%rank /= 0)  utest_master=.FALSE.

  CREATE_TEST("HDF File Type")

  CALL testHDF5FileTypeSetup()
  CALL HDF5Open()
  REGISTER_SUBTEST("HDF5FileType Uninit Checks",testHDF5FileTypeUninit)
  REGISTER_SUBTEST("HDF5FileType Initialization Checks",testHDF5FileTypeCreateDelete)
  REGISTER_SUBTEST("HDF5FileType Error Checks",testHDF5FileTypeErrorCheck)
  REGISTER_SUBTEST("HDF5FileType Read",testHDF5FileTypeRead)
  IF(utest_nfail == 0) THEN
    REGISTER_SUBTEST("HDF5FileType Write",testHDF5FileTypeWrite)
    REGISTER_SUBTEST("HDF5 File Compression",testCompress)
  ELSE
    WRITE(*,*) '-----------------------------------------------------'// &
        '--------------------'
    WRITE(*,*) "HDF5FileType Read subtest failed: HDF5FileType Write "// &
        "subtest will not run"
    WRITE(*,*) '-----------------------------------------------------'// &
        '--------------------'
  ENDIF
  CALL h5file%init('readtest.h5','READ')
  CALL h5file%clear(.TRUE.)
  DEALLOCATE(refD1,refD2,refD3,refD4,refD5,refD6,refD7,refS1,refS2,refS3,refS4, &
    refS5,refS6,refS7,refB1,refB2,refB3,refL1,refL2,refL3,refL4,refL5,refL6, &
      refL7,refN1,refN2,refN3,refN4,refN5,refN6,refN7,refST1,refST2,refST3,refsets)

  CALL HDF5Close()
  FINALIZE_TEST()
  CALL testMPI%clear()
  CALL testMPI%finalize()

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeSetup()
      TYPE(HDF5FileType) :: h5
      INTEGER(SIK) :: i,j,k

      ALLOCATE(refD1(10))
      ALLOCATE(refD2(4,5))
      ALLOCATE(refD3(3,4,5))
      ALLOCATE(refD4(3,4,5,6))
      ALLOCATE(refD5(3,4,5,6,7))
      ALLOCATE(refD6(3,4,5,6,7,8))
      ALLOCATE(refD7(3,4,5,6,7,8,9))
      ALLOCATE(refS1(10))
      ALLOCATE(refS2(4,5))
      ALLOCATE(refS3(3,4,5))
      ALLOCATE(refS4(3,4,5,6))
      ALLOCATE(refS5(3,4,5,6,7))
      ALLOCATE(refS6(3,4,5,6,7,8))
      ALLOCATE(refS7(3,4,5,6,7,8,9))
      ALLOCATE(refB1(6))
      ALLOCATE(refB2(6,5))
      ALLOCATE(refB3(6,5,2))
      ALLOCATE(refL1(10))
      ALLOCATE(refL2(4,5))
      ALLOCATE(refL3(3,4,5))
      ALLOCATE(refL4(3,4,5,6))
      ALLOCATE(refL5(3,4,5,6,7))
      ALLOCATE(refL6(3,4,5,6,7,8))
      ALLOCATE(refL7(3,4,5,6,7,8,9))
      ALLOCATE(refN1(10))
      ALLOCATE(refN2(4,5))
      ALLOCATE(refN3(3,4,5))
      ALLOCATE(refN4(3,4,5,6))
      ALLOCATE(refN5(3,4,5,6,7))
      ALLOCATE(refN6(3,4,5,6,7,8))
      ALLOCATE(refN7(3,4,5,6,7,8,9))
      ALLOCATE(refST1(3))
      ALLOCATE(refST2(2,3))
      ALLOCATE(refST3(3,4,5))
      ALLOCATE(refSTC1(32))
      ALLOCATE(refST0CA(33))
      ALLOCATE(refsets(16))

      refD0=42.123456789_SDK
      refD1=[(i*1.0000000001_SDK,i=1,SIZE(refD1))]
      refD2=RESHAPE([(i*1.0000000001_SDK,i=1,SIZE(refD2))],SHAPE(refD2))
      refD3=RESHAPE([(i*1.0000000001_SDK,i=1,SIZE(refD3))],SHAPE(refD3))
      refD4=RESHAPE([(i*1.0000000001_SDK,i=1,SIZE(refD4))],SHAPE(refD4))
      refD5=RESHAPE([(i*1.0000000001_SDK,i=1,SIZE(refD5))],SHAPE(refD5))
      refD6=RESHAPE([(i*1.0000000001_SDK,i=1,SIZE(refD6))],SHAPE(refD6))
      refD7=RESHAPE([(i*1.0000000001_SDK,i=1,SIZE(refD7))],SHAPE(refD7))
      refS0=42.000_SSK
      refS1=[(i,i=1,SIZE(refS1))]
      refS2=RESHAPE([(i,i=1,SIZE(refS2))],SHAPE(refS2))
      refS3=RESHAPE([(i,i=1,SIZE(refS3))],SHAPE(refS3))
      refS4=RESHAPE([(i,i=1,SIZE(refS4))],SHAPE(refS4))
      refS5=RESHAPE([(i,i=1,SIZE(refS5))],SHAPE(refS5))
      refS6=RESHAPE([(i,i=1,SIZE(refS6))],SHAPE(refS6))
      refS7=RESHAPE([(i,i=1,SIZE(refS7))],SHAPE(refS7))
      refL0=12345678912345678_SLK
      refL1=[(i+1000000000,i=1,SIZE(refL1))]
      refL2=RESHAPE([(i+1000000000,i=1,SIZE(refL2))],SHAPE(refL2))
      refL3=RESHAPE([(i+1000000000,i=1,SIZE(refL3))],SHAPE(refL3))
      refL4=RESHAPE([(i+1000000000,i=1,SIZE(refL4))],SHAPE(refL4))
      refL5=RESHAPE([(i+1000000000,i=1,SIZE(refL5))],SHAPE(refL5))
      refL6=RESHAPE([(i+1000000000,i=1,SIZE(refL6))],SHAPE(refL6))
      refL7=RESHAPE([(i+1000000000,i=1,SIZE(refL7))],SHAPE(refL7))
      refN0=42_SNK
      refN1=[(i,i=1,SIZE(refN1))]
      refN2=RESHAPE([(i,i=1,SIZE(refN2))],SHAPE(refN2))
      refN3=RESHAPE([(i,i=1,SIZE(refN3))],SHAPE(refN3))
      refN4=RESHAPE([(i,i=1,SIZE(refN4))],SHAPE(refN4))
      refN5=RESHAPE([(i,i=1,SIZE(refN5))],SHAPE(refN5))
      refN6=RESHAPE([(i,i=1,SIZE(refN6))],SHAPE(refN6))
      refN7=RESHAPE([(i,i=1,SIZE(refN7))],SHAPE(refN7))
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
       refST0CA(1)='R'; refST0CA(2)='a'; refST0CA(3)='n'; refST0CA(4)='k'; refST0CA(5)='-'
       refST0CA(6)='0'; refST0CA(7)=' '; refST0CA(8)='('; refST0CA(9)='N';refST0CA(10)='o'
      refST0CA(11)='t';refST0CA(12)='-';refST0CA(13)='a';refST0CA(14)='n';refST0CA(15)='-'
      refST0CA(16)='a';refST0CA(17)='r';refST0CA(18)='r';refST0CA(19)='a';refST0CA(20)='y'
      refST0CA(21)=')';refST0CA(22)=' ';refST0CA(23)='S';refST0CA(24)='t';refST0CA(25)='r'
      refST0CA(26)='i';refST0CA(27)='n';refST0CA(28)='g';refST0CA(29)=' ';refST0CA(30)='T'
      refST0CA(31)='e';refST0CA(32)='s';refST0CA(33)='t'
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
      refSTC1(1)='M';refSTC1(2)='P';refSTC1(3)='A';refSTC1(4)='C';refSTC1(5)='T'
      refSTC1(6)=' ';refSTC1(7)='s';refSTC1(8)='t';refSTC1(9)='r';refSTC1(10)='i'
      refSTC1(11)='n';refSTC1(12)='g';refSTC1(13)=' ';refSTC1(14)='t';refSTC1(15)='e'
      refSTC1(16)='s';refSTC1(17)='t';refSTC1(18)=' ';refSTC1(19)=' ';refSTC1(20)=' '
      refSTC1(21)=' ';refSTC1(22)=' ';refSTC1(23)=' ';refSTC1(24)=' ';refSTC1(25)=' '
      refSTC1(26)=' ';refSTC1(27)=' ';refSTC1(28)=' ';refSTC1(29)=' ';refSTC1(30)=' '
      refSTC1(31)=' ';refSTC1(32)=' '


      refsets(1)='memD0'
      refsets(2)='memD1'
      refsets(3)='memD2'
      refsets(4)='memD3'
      refsets(5)='memD4'
      refsets(6)='memD5'
      refsets(7)='memD6'
      refsets(8)='memD7'
      refsets(9)='memS0'
      refsets(10)='memS1'
      refsets(11)='memS2'
      refsets(12)='memS3'
      refsets(13)='memS4'
      refsets(14)='memS5'
      refsets(15)='memS6'
      refsets(16)='memS7'

      !write the reference file for the read side
      CALL h5%init('readtest.h5','NEW')
      CALL h5%fopen()
      CALL h5%mkdir('groupB')
      CALL h5%fwrite('groupB->memB0',refB0)
      CALL h5%fwrite('groupB->memB1',refB1)
      CALL h5%fwrite('groupB->memB2',refB2)
      CALL h5%fwrite('groupB->memB3',refB3)
      CALL h5%mkdir('groupC')
      CALL h5%mkdir('groupC->anotherGroup')
      CALL h5%mkdir('groupC->anotherGroup->moreGroups')
      CALL h5%mkdir('groupC->anotherGroup->moreGroups->almostLastGroup')
      CALL h5%mkdir('groupC->anotherGroup->moreGroups->LastGroup')
      CALL h5%fwrite('groupC->anotherGroup->memC1',refC1)
      CALL h5%fwrite('groupC->memC1',refC1)
      CALL h5%createHardLink('groupC->anotherGroup->memC1', &
        'groupC->anotherGroup->moreGroups->almostLastGroup->memC2')
      CALL h5%mkdir('groupI')
      CALL h5%fwrite('groupI->memL0',refL0)
      CALL h5%fwrite('groupI->memL1',refL1)
      CALL h5%fwrite('groupI->memL2',refL2)
      CALL h5%fwrite('groupI->memL3',refL3)
      CALL h5%fwrite('groupI->memL4',refL4)
      CALL h5%fwrite('groupI->memL5',refL5)
      CALL h5%fwrite('groupI->memL6',refL6)
      CALL h5%fwrite('groupI->memL7',refL7)
      CALL h5%fwrite('groupI->memN0',refN0)
      CALL h5%fwrite('groupI->memN1',refN1)
      CALL h5%fwrite('groupI->memN2',refN2)
      CALL h5%fwrite('groupI->memN3',refN3)
      CALL h5%fwrite('groupI->memN4',refN4)
      CALL h5%fwrite('groupI->memN5',refN5)
      CALL h5%fwrite('groupI->memN6',refN6)
      CALL h5%fwrite('groupI->memN7',refN7)
      CALL h5%mkdir('groupR')
      CALL h5%fwrite('groupR->memD0',refD0)
      CALL h5%fwrite('groupR->memD1',refD1)
      CALL h5%fwrite('groupR->memD2',refD2)
      CALL h5%fwrite('groupR->memD3',refD3)
      CALL h5%fwrite('groupR->memD4',refD4)
      CALL h5%fwrite('groupR->memD5',refD5)
      CALL h5%fwrite('groupR->memD6',refD6)
      CALL h5%fwrite('groupR->memD7',refD7)
      CALL h5%fwrite('groupR->memS0',refS0)
      CALL h5%fwrite('groupR->memS1',refS1)
      CALL h5%fwrite('groupR->memS2',refS2)
      CALL h5%fwrite('groupR->memS3',refS3)
      CALL h5%fwrite('groupR->memS4',refS4)
      CALL h5%fwrite('groupR->memS5',refS5)
      CALL h5%fwrite('groupR->memS6',refS6)
      CALL h5%fwrite('groupR->memS7',refS7)
      CALL h5%mkdir('groupST')
      CALL h5%fwrite('groupST->memCA0',CHAR(refST0))
      CALL h5%fwrite('groupST->memCA1',refST1)
      CALL h5%fwrite('groupST->memCA2',refST2)
      CALL h5%fwrite('groupST->memCA3',refST3)
      CALL h5%fwrite('groupST->memST0',refST0)
      CALL h5%fwrite('groupST->memST1',refST1)
      CALL h5%fwrite('groupST->memST2',refST2)
      CALL h5%fwrite('groupST->memST3',refST3)
      CALL h5%fclose()

    ENDSUBROUTINE testHDF5FileTypeSetup
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeUninit()
      TYPE(HDF5FileType) :: h5
      ASSERT(.NOT.h5%isinit,'%isinit')
      ASSERT(.NOT.h5%isNew(),'%newstat')
      ASSERT(h5%fullname%n == 0,'%fullname')
      ASSERT(h5%getUnitNo() == -1,'%unitno')
      ASSERT(h5%file_id == 0,'%file_id')
    ENDSUBROUTINE testHDF5FileTypeUninit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeCreateDelete()
      TYPE(HDF5FileType) :: h5

      CALL h5%e%setQuietMode(.TRUE.)
      CALL h5%init('createdeletetest.h5','NEW')

      ASSERT(h5%isinit,'HDF5 object no properly initialized')
      CALL h5%fopen()
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR) == 0,'hdf5_filetype%fopen')
      CALL h5%fclose()
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR) == 0,'hdf5_filetype%fclose')
      CALL h5%clear()
      ASSERT(.NOT.h5%isinit,'HDF5 object not properly cleared.')

      CALL h5%init('createdeletetest.h5','WRITE')
      CALL h5%fopen()
      ASSERT(h5%isWrite(),'HDF object %isWrite() should be .TRUE.')
      CALL h5%mkdir('testGroup')
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR) == 0,'hdf5_filetype%init ''WRITE''')
      CALL h5%clear()

      CALL h5%init('createdeletetest.h5','READ')
      CALL h5%fopen()
      ASSERT(.NOT.(h5%isWrite()),'HDF object %isWrite() should be .FALSE.')
      CALL h5%e%setStopOnError(.FALSE.)
      CALL h5%mkdir('testGroup')
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR) == 1,'hdf5_filetype%init ''READ''')
      FINFO() h5%e%getCounter(EXCEPTION_ERROR)
      CALL h5%fdelete()
      ASSERT(h5%isinit,'HDF5 object still initialized after deletion.')
      INQUIRE(FILE='createdeletetest.h5',EXIST=exists)
      ASSERT(.NOT.exists,'HDF5 object not properly deleted without being cleared.')
      CALL h5%clear()
      ASSERT(.NOT.h5%isinit,'HDF5 object not properly cleared.')

      CALL h5%init('createdeletetest.h5','NEW')
      CALL h5%fopen()
      IF(h5%isinit) exists=.TRUE.
      CALL h5%clear(LDEL=.TRUE.)
      INQUIRE(FILE='createdeletetest.h5',EXIST=exists)
      ASSERT(.NOT.exists,'HDF5 object not properly deleted after begin cleared.')

    ENDSUBROUTINE testHDF5FileTypeCreateDelete
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeErrorCheck()
      TYPE(HDF5FileType) :: h5
      REAL(SDK),POINTER :: d4ptr(:,:,:,:)
      d4ptr => NULL()

      CALL h5%e%setQuietMode(.TRUE.)
      !Adding the *2 for now to get the test passing.  Since the simplification,
      !the pre and post routines have two %isinit checks.  All these checks are
      !mostly redundant now though, one check checks the same code as all of them now.
      CALL h5%fwrite('groupR->memD0',refD0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==1*2,'%write_d0 %isinit check')
      CALL h5%fwrite('groupR->memD1',refD1,SHAPE(refD1))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==2*2,'%write_d1 %isinit check')
      CALL h5%fwrite('groupR->memD2',refD2,SHAPE(refD2))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==3*2,'%write_d2 %isinit check')
      CALL h5%fwrite('groupR->memD3',refD3,SHAPE(refD3))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==4*2,'%write_d3 %isinit check')
      CALL h5%fwrite('groupR->memD4',refD4,SHAPE(refD4))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==5*2,'%write_d4 %isinit check')
      CALL h5%fwrite('groupR->memD5',refD5,SHAPE(refD5))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==6*2,'%write_d5 %isinit check')
      CALL h5%fwrite('groupR->memD6',refD6,SHAPE(refD6))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==7*2,'%write_d6 %isinit check')
      CALL h5%fwrite('groupR->memD7',refD7,SHAPE(refD7))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==8*2,'%write_d7 %isinit check')
      CALL h5%fwrite('groupR->memS0',refS0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==9*2,'%write_s0 %isinit check')
      CALL h5%fwrite('groupR->memS1',refS1,SHAPE(refS1))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==10*2,'%write_s1 %isinit check')
      CALL h5%fwrite('groupR->memS2',refS2,SHAPE(refS2))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==11*2,'%write_s2 %isinit check')
      CALL h5%fwrite('groupR->memS3',refS3,SHAPE(refS3))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==12*2,'%write_s3 %isinit check')
      CALL h5%fwrite('groupR->memS4',refS4,SHAPE(refS4))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==13*2,'%write_s4 %isinit check')
      CALL h5%fwrite('groupR->memS5',refS5,SHAPE(refS5))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==14*2,'%write_s5 %isinit check')
      CALL h5%fwrite('groupR->memS6',refS6,SHAPE(refS6))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==15*2,'%write_s6 %isinit check')
      CALL h5%fwrite('groupR->memS7',refS7,SHAPE(refS7))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==16*2,'%write_s7 %isinit check')
      CALL h5%fwrite('groupI->memL0',refL0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==17*2,'%write_l0 %isinit check')
      CALL h5%fwrite('groupI->memL1',refL1,SHAPE(refL1))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==18*2,'%write_l1 %isinit check')
      CALL h5%fwrite('groupI->memL2',refL2,SHAPE(refL2))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==19*2,'%write_l2 %isinit check')
      CALL h5%fwrite('groupI->memL3',refL3,SHAPE(refL3))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==20*2,'%write_l3 %isinit check')
      CALL h5%fwrite('groupI->memL4',refL4,SHAPE(refL4))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==21*2,'%write_l4 %isinit check')
      CALL h5%fwrite('groupI->memL5',refL5,SHAPE(refL5))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==22*2,'%write_l5 %isinit check')
      CALL h5%fwrite('groupI->memL6',refL6,SHAPE(refL6))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==23*2,'%write_l6 %isinit check')
      CALL h5%fwrite('groupI->memL7',refL7,SHAPE(refL7))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==24*2,'%write_l7 %isinit check')
      CALL h5%fwrite('groupI->memN0',refN0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==25*2,'%write_n0 %isinit check')
      CALL h5%fwrite('groupI->memN1',refN1,SHAPE(refN1))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==26*2,'%write_n1 %isinit check')
      CALL h5%fwrite('groupI->memN2',refN2,SHAPE(refN2))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==27*2,'%write_n2 %isinit check')
      CALL h5%fwrite('groupI->memN3',refN3,SHAPE(refN3))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==28*2,'%write_n3 %isinit check')
      CALL h5%fwrite('groupI->memN4',refN4,SHAPE(refN4))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==29*2,'%write_n4 %isinit check')
      CALL h5%fwrite('groupI->memN5',refN5,SHAPE(refN5))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==30*2,'%write_n5 %isinit check')
      CALL h5%fwrite('groupI->memN6',refN6,SHAPE(refN6))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==31*2,'%write_n6 %isinit check')
      CALL h5%fwrite('groupI->memN7',refN7,SHAPE(refN7))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==32*2,'%write_n7 %isinit check')
      CALL h5%fwrite('groupB->memB0',refB0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==33*2,'%write_b0 %isinit check')
      CALL h5%fwrite('groupB->memB1',refB1,SHAPE(refB1))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==34*2,'%write_b1 %isinit check')
      CALL h5%fwrite('groupB->memB2',refB2,SHAPE(refB2))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==35*2,'%write_b2 %isinit check')
      CALL h5%fwrite('groupB->memB3',refB3,SHAPE(refB3))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==36*2,'%write_b3 %isinit check')
      CALL h5%fwrite('groupST->memST0',refST0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==37*2,'%write_st0 %isinit check')
      CALL h5%fwrite('groupST->memST1',refST1,SHAPE(refST1))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==38*2,'%write_st1 %isinit check')
      CALL h5%fwrite('groupST->memST2',refST2,SHAPE(refST2))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==39*2,'%write_st2 %isinit check')
      CALL h5%fwrite('groupST->memST3',refST3,SHAPE(refST3))
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==40*2,'%write_st3 %isinit check')
      CALL h5%fwrite('groupC->memC1',refC1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==41*2,'%write_c1 %isinit check')
      CALL h5%mkdir('groupI')
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==42*2-1,'%mkdir %isinit check')
      CALL h5%ls('groupR',refST1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==42*2,'%ls %isinit check')

      CALL h5%fread('groupR->memD0',refD0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==43*2,'%read_d0 %isinit check')
      CALL h5%fread('groupR->memD1',refD1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==44*2,'%read_d1 %isinit check')
      CALL h5%fread('groupR->memD2',refD2)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==45*2,'%read_d2 %isinit check')
      CALL h5%fread('groupR->memD3',refD3)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==46*2,'%read_d3 %isinit check')
      CALL h5%fread('groupR->memD4',refD4)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==47*2,'%read_d4 %isinit check')
      CALL h5%freadp('groupR->memD4',d4ptr)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==48*2,'%read_d4 %isinit check')
      CALL h5%fread('groupR->memS0',refS0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==49*2,'%read_s0 %isinit check')
      CALL h5%fread('groupR->memS1',refS1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==50*2,'%read_s1 %isinit check')
      CALL h5%fread('groupR->memS2',refS2)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==51*2,'%read_s2 %isinit check')
      CALL h5%fread('groupR->memS3',refS3)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==52*2,'%read_s3 %isinit check')
      CALL h5%fread('groupR->memS4',refS4)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==53*2,'%read_s4 %isinit check')
      CALL h5%fread('groupI->memL0',refL0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==54*2,'%read_l0 %isinit check')
      CALL h5%fread('groupI->memL1',refL1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==55*2,'%read_l1 %isinit check')
      CALL h5%fread('groupI->memL2',refL2)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==56*2,'%read_l2 %isinit check')
      CALL h5%fread('groupI->memL3',refL3)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==57*2,'%read_l3 %isinit check')
      CALL h5%fread('groupI->memN0',refN0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==58*2,'%read_n0 %isinit check')
      CALL h5%fread('groupI->memN1',refN1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==59*2,'%read_n1 %isinit check')
      CALL h5%fread('groupI->memN2',refN2)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==60*2,'%read_n2 %isinit check')
      CALL h5%fread('groupI->memN3',refN3)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==61*2,'%read_n3 %isinit check')
      CALL h5%fread('groupB->memB0',refB0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==62*2,'%read_b0 %isinit check')
      CALL h5%fread('groupB->memB1',refB1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==63*2,'%read_b1 %isinit check')
      CALL h5%fread('groupB->memB2',refB2)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==64*2,'%read_b2 %isinit check')
      CALL h5%fread('groupB->memB3',refB3)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==65*2,'%read_b3 %isinit check')
      CALL h5%fread('groupST->memST0',refST0)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==66*2,'%read_st0 %isinit check')
      CALL h5%fread('groupST->memST1',refST1)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==67*2,'%read_st1 %isinit check')
      CALL h5%fread('groupST->memST2',refST2)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==68*2,'%read_st2 %isinit check')
      CALL h5%fread('groupST->memST3',refST3)
      ASSERT(h5%e%getCounter(EXCEPTION_ERROR)==69*2,'%read_st3 %isinit check')

      CALL h5%clear(.TRUE.)
    ENDSUBROUTINE testHDF5FileTypeErrorCheck
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeWrite()
      TYPE(HDF5FileType) :: h5
      REAL(SDK),ALLOCATABLE :: testD1(:),testD2(:,:),testD3(:,:,:),testD4(:,:,:,:)
      REAL(SDK),ALLOCATABLE :: testD5(:,:,:,:,:),testD6(:,:,:,:,:,:),testD7(:,:,:,:,:,:,:)
      REAL(SSK),ALLOCATABLE :: testS1(:),testS2(:,:),testS3(:,:,:),testS4(:,:,:,:)
      REAL(SSK),ALLOCATABLE :: testS5(:,:,:,:,:),testS6(:,:,:,:,:,:),testS7(:,:,:,:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: testB1(:),testB2(:,:),testB3(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: testL1(:),testL2(:,:),testL3(:,:,:),testL4(:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: testL5(:,:,:,:,:),testL6(:,:,:,:,:,:),testL7(:,:,:,:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: testN1(:),testN2(:,:),testN3(:,:,:),testN4(:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: testN5(:,:,:,:,:),testN6(:,:,:,:,:,:),testN7(:,:,:,:,:,:,:)
      TYPE(StringType),ALLOCATABLE :: testST1(:),testST2(:,:),testST3(:,:,:)
      REAL(SDK) :: testD0
      REAL(SSK) :: testS0
      INTEGER(SLK) :: testL0
      INTEGER(SNK) :: testN0
      LOGICAL(SBK) :: testB0,bool
      CHARACTER(LEN=32) :: testC1,testC2
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
      TYPE(StringType) :: testST0
      INTEGER(SIK) :: i,j,k
      LOGICAL(SBK) :: checkwrite

      COMPONENT_TEST('%fwrite with gdims')
      ! Create a RW access file. Existing file overwritten
      CALL h5%init('writetest.h5','NEW')
      CALL h5%fopen()

      ! Test writing with the gdims arguments
      CALL h5%mkdir('groupR')
      CALL h5%fwrite('groupR->memD0',refD0)
      CALL h5%fwrite('groupR->memD1',refD1,SHAPE(refD1))
      CALL h5%fwrite('groupR->memD2',refD2,SHAPE(refD2))
      CALL h5%fwrite('groupR->memD3',refD3,SHAPE(refD3))
      CALL h5%fwrite('groupR->memD4',refD4,SHAPE(refD4))
      CALL h5%fwrite('groupR->memD5',refD5,SHAPE(refD5))
      CALL h5%fwrite('groupR->memD6',refD6,SHAPE(refD6))
      CALL h5%fwrite('groupR->memD7',refD7,SHAPE(refD7))
      CALL h5%fwrite('groupR->memS0',refS0)
      CALL h5%fwrite('groupR->memS1',refS1,SHAPE(refS1))
      CALL h5%fwrite('groupR->memS2',refS2,SHAPE(refS2))
      CALL h5%fwrite('groupR->memS3',refS3,SHAPE(refS3))
      CALL h5%fwrite('groupR->memS4',refS4,SHAPE(refS4))
      CALL h5%fwrite('groupR->memS5',refS5,SHAPE(refS5))
      CALL h5%fwrite('groupR->memS6',refS6,SHAPE(refS6))
      CALL h5%fwrite('groupR->memS7',refS7,SHAPE(refS7))
      CALL h5%mkdir('groupI')
      CALL h5%fwrite('groupI->memL0',refL0)
      CALL h5%fwrite('groupI->memL1',refL1,SHAPE(refL1))
      CALL h5%fwrite('groupI->memL2',refL2,SHAPE(refL2))
      CALL h5%fwrite('groupI->memL3',refL3,SHAPE(refL3))
      CALL h5%fwrite('groupI->memL4',refL4,SHAPE(refL4))
      CALL h5%fwrite('groupI->memL5',refL5,SHAPE(refL5))
      CALL h5%fwrite('groupI->memL6',refL6,SHAPE(refL6))
      CALL h5%fwrite('groupI->memL7',refL7,SHAPE(refL7))
      CALL h5%fwrite('groupI->memN0',refN0)
      CALL h5%fwrite('groupI->memN1',refN1,SHAPE(refN1))
      CALL h5%fwrite('groupI->memN2',refN2,SHAPE(refN2))
      CALL h5%fwrite('groupI->memN3',refN3,SHAPE(refN3))
      CALL h5%fwrite('groupI->memN4',refN4,SHAPE(refN4))
      CALL h5%fwrite('groupI->memN5',refN5,SHAPE(refN5))
      CALL h5%fwrite('groupI->memN6',refN6,SHAPE(refN6))
      CALL h5%fwrite('groupI->memN7',refN7,SHAPE(refN7))
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
      CALL h5%fread('groupR->memD5',testD5)
      ASSERT(ALL(testD5==refD5),'D5 Write Failure with gdims_in')
      CALL h5%fread('groupR->memD6',testD6)
      ASSERT(ALL(testD6==refD6),'D6 Write Failure with gdims_in')
      CALL h5%fread('groupR->memD7',testD7)
      ASSERT(ALL(testD7==refD7),'D7 Write Failure with gdims_in')
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
      CALL h5%fread('groupR->memS5',testS5)
      ASSERT(ALL(testS5==refS5),'S5 Write Failure with gdims_in')
      CALL h5%fread('groupR->memS6',testS6)
      ASSERT(ALL(testS6==refS6),'S6 Write Failure with gdims_in')
      CALL h5%fread('groupR->memS7',testS7)
      ASSERT(ALL(testS7==refS7),'S7 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN0',testN0)
      ASSERT(testN0==refN0,'N0 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN1',testN1)
      ASSERT(ALL(testN1==refN1),'N1 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN2',testN2)
      ASSERT(ALL(testN2==refN2),'N2 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN3',testN3)
      ASSERT(ALL(testN3==refN3),'N3 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN4',testN4)
      ASSERT(ALL(testN4==refN4),'N4 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN5',testN5)
      ASSERT(ALL(testN5==refN5),'N5 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN6',testN6)
      ASSERT(ALL(testN6==refN6),'N6 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN7',testN7)
      ASSERT(ALL(testN7==refN7),'N7 Write Failure with gdims_in')
      CALL h5%fread('groupI->memL0',testL0)
      ASSERT(testL0==refL0,'L0 Write Failure with gdims_in')
      FINFO()testL0,refL0
      CALL h5%fread('groupI->memL1',testL1)
      ASSERT(ALL(testL1==refL1),'L1 Write Failure with gdims_in')
      CALL h5%fread('groupI->memL2',testL2)
      ASSERT(ALL(testL2==refL2),'L2 Write Failure with gdims_in')
      CALL h5%fread('groupI->memL3',testL3)
      ASSERT(ALL(testL3==refL3),'L3 Write Failure with gdims_in')
      CALL h5%fread('groupI->memL4',testL4)
      ASSERT(ALL(testL4==refL4),'L4 Write Failure with gdims_in')
      CALL h5%fread('groupI->memL5',testL5)
      ASSERT(ALL(testL5==refL5),'L5 Write Failure with gdims_in')
      CALL h5%fread('groupI->memL6',testL6)
      ASSERT(ALL(testL6==refL6),'L6 Write Failure with gdims_in')
      CALL h5%fread('groupI->memL7',testL7)
      ASSERT(ALL(testL7==refL7),'L7 Write Failure with gdims_in')
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
      FINFO() ":"//testST0//":   :"//refST0//":"
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
      ASSERT(i == 16,'ngrp_HDF5FileType')
      FINFO() i

      ! Delete the file
      CALL h5%clear(.TRUE.)

      COMPONENT_TEST('%fwrite without gdims')
      CALL h5%init('writetest.h5','NEW')
      CALL h5%fopen()

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
      CALL h5%fwrite('groupR->memD5',refD5)
      CALL h5%fwrite('groupR->memD6',refD6)
      CALL h5%fwrite('groupR->memD7',refD7)
      CALL h5%fwrite('groupR->memS0',refS0)
      CALL h5%fwrite('groupR->memS1',refS1)
      CALL h5%fwrite('groupR->memS2',refS2)
      CALL h5%fwrite('groupR->memS3',refS3)
      CALL h5%fwrite('groupR->memS4',refS4)
      CALL h5%fwrite('groupR->memS5',refS5)
      CALL h5%fwrite('groupR->memS6',refS6)
      CALL h5%fwrite('groupR->memS7',refS7)
      CALL h5%fwrite('groupI->memL0',refL0)
      CALL h5%fwrite('groupI->memL1',refL1)
      CALL h5%fwrite('groupI->memL2',refL2)
      CALL h5%fwrite('groupI->memL3',refL3)
      CALL h5%fwrite('groupI->memL4',refL4)
      CALL h5%fwrite('groupI->memL5',refL5)
      CALL h5%fwrite('groupI->memL6',refL6)
      CALL h5%fwrite('groupI->memL7',refL7)
      CALL h5%fwrite('groupI->memN0',refN0)
      CALL h5%fwrite('groupI->memN1',refN1)
      CALL h5%fwrite('groupI->memN2',refN2)
      CALL h5%fwrite('groupI->memN3',refN3)
      CALL h5%fwrite('groupI->memN4',refN4)
      CALL h5%fwrite('groupI->memN5',refN5)
      CALL h5%fwrite('groupI->memN6',refN6)
      CALL h5%fwrite('groupI->memN7',refN7)
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
      CALL h5%fread('groupR->memD5',testD5)
      ASSERT(ALL(testD5==refD5),'D5 Write Failure')
      CALL h5%fread('groupR->memD6',testD6)
      ASSERT(ALL(testD6==refD6),'D6 Write Failure')
      CALL h5%fread('groupR->memD7',testD7)
      ASSERT(ALL(testD7==refD7),'D7 Write Failure')
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
      CALL h5%fread('groupR->memS5',testS5)
      ASSERT(ALL(testS5==refS5),'S5 Write Failure')
      CALL h5%fread('groupR->memS6',testS6)
      ASSERT(ALL(testS6==refS6),'S6 Write Failure')
      CALL h5%fread('groupR->memS7',testS7)
      ASSERT(ALL(testS7==refS7),'S7 Write Failure')
      CALL h5%fread('groupI->memN0',testN0)
      ASSERT(testN0==refN0,'N0 Write Failure')
      CALL h5%fread('groupI->memN1',testN1)
      ASSERT(ALL(testN1==refN1),'N1 Write Failure')
      CALL h5%fread('groupI->memN2',testN2)
      ASSERT(ALL(testN2==refN2),'N2 Write Failure')
      CALL h5%fread('groupI->memN3',testN3)
      ASSERT(ALL(testN3==refN3),'N3 Write Failure')
      CALL h5%fread('groupI->memN4',testN4)
      ASSERT(ALL(testN4==refN4),'N4 Write Failure')
      CALL h5%fread('groupI->memN5',testN5)
      ASSERT(ALL(testN5==refN5),'N5 Write Failure')
      CALL h5%fread('groupI->memN6',testN6)
      ASSERT(ALL(testN6==refN6),'N6 Write Failure')
      CALL h5%fread('groupI->memN7',testN7)
      ASSERT(ALL(testN7==refN7),'N7 Write Failure')
      CALL h5%fread('groupI->memL0',testL0)
      ASSERT(testL0==refL0,'L0 Write Failure')
      FINFO()testL0,refL0
      CALL h5%fread('groupI->memL1',testL1)
      ASSERT(ALL(testL1==refL1),'L1 Write Failure')
      CALL h5%fread('groupI->memL2',testL2)
      ASSERT(ALL(testL2==refL2),'L2 Write Failure')
      CALL h5%fread('groupI->memL3',testL3)
      ASSERT(ALL(testL3==refL3),'L3 Write Failure')
      CALL h5%fread('groupI->memL4',testL4)
      ASSERT(ALL(testL4==refL4),'L4 Write Failure')
      CALL h5%fread('groupI->memL5',testL5)
      ASSERT(ALL(testL5==refL5),'L5 Write Failure')
      CALL h5%fread('groupI->memL6',testL6)
      ASSERT(ALL(testL6==refL6),'L6 Write Failure')
      CALL h5%fread('groupI->memL7',testL7)
      ASSERT(ALL(testL7==refL7),'L7 Write Failure')
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
      FINFO() "testC1=",testC1,":refC1=",refC1,":"
      i=h5%ngrp('groupR')
      ASSERT(i == 16,'ngrp_HDF5FileType')
      FINFO() i

      COMPONENT_TEST('%pathExists')
      !Add some sub-groups
      CALL h5%mkdir('groupC->anotherGroup')
      CALL h5%mkdir('groupC->anotherGroup->moreGroups')
      CALL h5%mkdir('groupC->anotherGroup->moreGroups->almostLastGroup')
      CALL h5%mkdir('groupC->anotherGroup->moreGroups->lastGroup')
      !Check valid paths
      ASSERT(h5%pathExists('groupC'),'%pathExists(groupC)')
      ASSERT(h5%pathExists('groupC->anotherGroup'),'%pathExists(groupC->anotherGroup)')
      bool=h5%pathExists('groupC->anotherGroup->moreGroups')
      ASSERT(bool,'%pathExists(groupC->anotherGroup->moreGroups)')
      bool=h5%pathExists('groupC->anotherGroup->moreGroups->almostLastGroup')
      ASSERT(bool,'%pathExists(groupC->anotherGroup->moreGroups->almostLastGroup)')
      bool=h5%pathExists('groupC->anotherGroup->moreGroups->lastGroup')
      ASSERT(bool,'%pathExists(groupC->anotherGroup->moreGroups->lastGroup)')
      ASSERT(h5%pathExists('groupC->memC1'),'%pathExists(groupC->memC1)')

      COMPONENT_TEST('%isgrp')
      ASSERT(h5%isGroup('groupC'),'/groupC')
      ASSERT(.NOT.h5%isGroup('groupC->memC1'),'/groupC/memC1')
      ASSERT(h5%isGroup('groupC->anotherGroup'),'/groupC/anotherGroup')


      !Check for invalid paths
      ASSERT(.NOT.h5%pathExists('groupBlah'),'%pathExists(groupBlah)')
      ASSERT(.NOT.h5%pathExists('groupC->blahGroup'),'%pathExists(groupC->blahGroup)')
      bool=.NOT.h5%pathExists('groupC->anotherGroup->moreBlahGroups')
      ASSERT(bool,'%pathExists(groupC->anotherGroup->moreBlahGroups)')
      ASSERT(.NOT.h5%pathExists('groupC->memCBlah'),'%pathExists(groupC->memCBlah)')

      COMPONENT_TEST('%mkAllDir')
      ASSERT(.NOT.h5%pathExists('groupC->A group'),'no A group')
      ASSERT(.NOT.h5%pathExists('groupC->A group->leaf group'),'no leaf group')
      CALL h5%mkAllDir('groupC->A group->leaf group')
      ASSERT(h5%pathExists('groupC->A group'),'A group')
      ASSERT(h5%pathExists('groupC->A group->leaf group'),'leaf group')

      COMPONENT_TEST('%createHardLink')
      CALL h5%fwrite('groupC->anotherGroup->memC1',refC1)
      CALL h5%createHardLink('groupC->anotherGroup->memC1', &
        'groupC->anotherGroup->moreGroups->almostLastGroup->memC2')
      CALL h5%fread('groupC->anotherGroup->memC1',testC1)
      CALL h5%fread('groupC->anotherGroup->moreGroups->almostLastGroup->memC2',testC2)
      ASSERT(testC1 == testC2,'Valid Hardlink')
      CALL h5%e%setQuietMode(.TRUE.)
      CALL h5%e%setStopOnError(.FALSE.)
      CALL h5%createHardLink('groupThatDoesNotExist->memC1','groupC->anotherGroup->moreGroups')
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::createHardLink_HDF5FileType '// &
        '- Target of new link must exist in file!'
      ASSERT(TRIM(msg) == TRIM(refmsg),'Non-existant Target Link')
      FINFO() "   msg="//TRIM(msg)
      FINFO() "refmsg="//TRIM(refmsg)
      CALL h5%createHardLink('groupC->memC1','groupC->anotherGroup->moreGroups')
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::createHardLink_HDF5FileType '// &
        '- Location of new link already exists!'
      ASSERT(TRIM(msg) == TRIM(refmsg),'Pre-existing New Link')
      FINFO() "   msg="//TRIM(msg)
      FINFO() "refmsg="//TRIM(refmsg)

      CALL h5%e%setQuietMode(.FALSE.)
      CALL h5%e%setStopOnError(.TRUE.)

      !CALL h5%clear(.TRUE.)
      CALL h5%clear()
      !CALL h5%fdelete()
      !INQUIRE(FILE='writetest.h5',EXIST=exists)
      !ASSERT(.NOT.exists,'HDF5 object not properly deleted!')
    ENDSUBROUTINE testHDF5FileTypeWrite
!
!-------------------------------------------------------------------------------
    SUBROUTINE testCompress()
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
      CALL h5%init('writetest_compress.h5','NEW',.TRUE.)
      CALL h5%fopen()

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
      FINFO() ":"//testST0//":   :"//refST0//":"
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
      CALL h5%clear(.TRUE.)

      COMPONENT_TEST('%fwrite without gdims')
      CALL h5%init('writetest_compress.h5','NEW',.TRUE.)
      CALL h5%fopen()

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
      FINFO() "testC1=",testC1,":refC1=",refC1,":"
      i=h5%ngrp('groupR')
      ASSERT(i == 10,'ngrp_HDF5FileType')
      FINFO() i

      !Clear variables
      CALL h5%clear(.TRUE.)

    ENDSUBROUTINE testCompress
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeRead()
      TYPE(HDF5FileType) :: h5
      REAL(SDK),ALLOCATABLE :: testD1(:),testD2(:,:),testD3(:,:,:),testD4(:,:,:,:)
      REAL(SDK),ALLOCATABLE :: testD5(:,:,:,:,:),testD6(:,:,:,:,:,:),testD7(:,:,:,:,:,:,:)
      REAL(SDK),POINTER :: testDP4(:,:,:,:)
      REAL(SSK),ALLOCATABLE :: testS1(:),testS2(:,:),testS3(:,:,:),testS4(:,:,:,:)
      REAL(SSK),ALLOCATABLE :: testS5(:,:,:,:,:),testS6(:,:,:,:,:,:),testS7(:,:,:,:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: testB1(:),testB2(:,:),testB3(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: testL1(:),testL2(:,:),testL3(:,:,:),testL4(:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: testL5(:,:,:,:,:),testL6(:,:,:,:,:,:),testL7(:,:,:,:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: testN1(:),testN2(:,:),testN3(:,:,:),testN4(:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: testN5(:,:,:,:,:),testN6(:,:,:,:,:,:),testN7(:,:,:,:,:,:,:)
      TYPE(StringType),ALLOCATABLE :: testST1(:),testST2(:,:),testST3(:,:,:)
      REAL(SDK) :: testD0
      REAL(SSK) :: testS0
      INTEGER(SLK) :: testL0
      INTEGER(SNK) :: testN0
      LOGICAL(SBK) :: testB0
      CHARACTER(LEN=32) :: testC1
      TYPE(StringType) :: testST0
      INTEGER(SIK) :: i,j,k
      LOGICAL(SBK) :: checkread
      TYPE(ParamType) :: tmpPL
      testDP4 => NULL()

!  Begin test
      CALL h5%init('readtest.h5','READ')
      CALL h5%fopen()
      !CALL h5%e%setQuietMode('.FALSE.')

      CALL h5%ls('groupR',testST1)
      DO i=1,SIZE(testST1)
        ASSERT(TRIM(refsets(i))==TRIM(testST1(i)),refsets(i)//' List Failure')
        FINFO() refsets(i)//':'//testST1(i)
      ENDDO

      COMPONENT_TEST('%fread values')
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
      CALL h5%fread('groupR->memD5',testD5)
      ASSERT(ALL(testD5==refD5),'D5 Read Failure')
      CALL h5%fread('groupR->memD6',testD6)
      ASSERT(ALL(testD6==refD6),'D6 Read Failure')
      CALL h5%fread('groupR->memD7',testD7)
      ASSERT(ALL(testD7==refD7),'D7 Read Failure')
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
      CALL h5%fread('groupR->memS5',testS5)
      ASSERT(ALL(testS5==refS5),'S5 Read Failure')
      CALL h5%fread('groupR->memS6',testS6)
      ASSERT(ALL(testS6==refS6),'S6 Read Failure')
      CALL h5%fread('groupR->memS7',testS7)
      ASSERT(ALL(testS7==refS7),'S7 Read Failure')
      CALL h5%fread('groupI->memN0',testN0)
      ASSERT(testN0==refN0,'N0 Read Failure')
      CALL h5%fread('groupI->memN1',testN1)
      ASSERT(ALL(testN1==refN1),'N1 Read Failure')
      CALL h5%fread('groupI->memN2',testN2)
      ASSERT(ALL(testN2==refN2),'N2 Read Failure')
      CALL h5%fread('groupI->memN3',testN3)
      ASSERT(ALL(testN3==refN3),'N3 Read Failure')
      CALL h5%fread('groupI->memN4',testN4)
      ASSERT(ALL(testN4==refN4),'N4 Read Failure')
      CALL h5%fread('groupI->memN5',testN5)
      ASSERT(ALL(testN5==refN5),'N5 Read Failure')
      CALL h5%fread('groupI->memN6',testN6)
      ASSERT(ALL(testN6==refN6),'N6 Read Failure')
      CALL h5%fread('groupI->memN7',testN7)
      ASSERT(ALL(testN7==refN7),'N7 Read Failure')
      CALL h5%fread('groupI->memL0',testL0)
      ASSERT(testL0==refL0,'L0 Read Failure')
      FINFO()testL0,refL0
      CALL h5%fread('groupI->memL1',testL1)
      ASSERT(ALL(testL1==refL1),'L1 Read Failure')
      CALL h5%fread('groupI->memL2',testL2)
      ASSERT(ALL(testL2==refL2),'L2 Read Failure')
      CALL h5%fread('groupI->memL3',testL3)
      ASSERT(ALL(testL3==refL3),'L3 Read Failure')
      CALL h5%fread('groupI->memL4',testL4)
      ASSERT(ALL(testL4==refL4),'L4 Read Failure')
      CALL h5%fread('groupI->memL5',testL5)
      ASSERT(ALL(testL5==refL5),'L5 Read Failure')
      CALL h5%fread('groupI->memL6',testL6)
      ASSERT(ALL(testL6==refL6),'L6 Read Failure')
      CALL h5%fread('groupI->memL7',testL7)
      ASSERT(ALL(testL7==refL7),'L7 Read Failure')
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
      FINFO() "test: "//CHAR(testST0), "ref: "//CHAR(refST0)
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
      CALL h5%fread('groupST->memCA0',testST0)
      ASSERT(testST0==refST0,'CA0 Read Failure')
      FINFO() "test: "//CHAR(testST0), "ref: "//CHAR(refST0)
      CALL h5%fread('groupST->memCA1',testST1)
      checkread=.TRUE.
      DO i=1,SIZE(refST1)
        IF(testST1(i)/=refST1(i)) checkread=.FALSE.
      ENDDO
      ASSERT(checkread,'CA1 Read Failure')
      CALL h5%fread('groupST->memCA2',testST2)
      checkread=.TRUE.
      DO i=1,SIZE(refST2,1)
        DO j=1,SIZE(refST2,2)
          IF(testST2(i,j)/=refST2(i,j)) checkread=.FALSE.
        ENDDO
      ENDDO
      ASSERT(checkread,'CA2 Read Failure')
      CALL h5%fread('groupST->memCA3',testST3)
      checkread=.TRUE.
      DO i=1,SIZE(refST3,1)
        DO j=1,SIZE(refST3,2)
          DO k=1,SIZE(refST3,3)
            IF(testST3(i,j,k)/=refST3(i,j,k)) checkread=.FALSE.
          ENDDO
        ENDDO
      ENDDO
      ASSERT(checkread,'CA3 Read Failure')
      CALL h5%fread('groupC->memC1',testC1)
      ASSERT(testC1 == refC1,'C1 Read Failure')

      COMPONENT_TEST('%fread to parameter list')
      CALL h5%fread('groupC',tmpPL)
      !Character arrays
      CALL tmpPL%get('groupC->anotherGroup->memC1',testST1)
      DO i=1,SIZE(testST1)
        ASSERTFAIL(SIZE(testST1) == SIZE(refSTC1),'SIZE(')
        ASSERT(testST1(i) == refSTC1(i),'C1 Read PL Failure')
        FINFO() i,CHAR(testST1(i))
        FINFO() i,CHAR(refSTC1(i))
      ENDDO
      CALL tmpPL%get('groupC->anotherGroup->moreGroups->almostLastGroup->memC2',testST1)
      DO i=1,SIZE(testST1)
        ASSERTFAIL(SIZE(testST1) == SIZE(refSTC1),'SIZE(')
        ASSERT(testST1(i) == refSTC1(i),'C1 Read PL Failure')
        FINFO() i,CHAR(testST1(i))
        FINFO() i,CHAR(refSTC1(i))
      ENDDO
      CALL tmpPL%get('groupC->memC1',testST1)
      DO i=1,SIZE(testST1)
        ASSERTFAIL(SIZE(testST1) == SIZE(refSTC1),'SIZE(')
        ASSERT(testST1(i) == refSTC1(i),'C1 Read PL Failure')
        FINFO() i,CHAR(testST1(i))
        FINFO() i,CHAR(refSTC1(i))
      ENDDO
      !Logicals
      CALL tmpPL%clear()
      CALL h5%fread('groupB',tmpPL)
      CALL tmpPL%get('groupB->memB0',testB0)
      ASSERT(.NOT.testB0,'B0 read PL Failure')
      CALL tmpPL%get('groupB->memB1',testB1)
      testB0=.NOT.testB1(1) .AND. testB1(2) .AND. .NOT. testB1(3) .AND. &
        testB1(4) .AND. .NOT.testB1(5) .AND. testB1(6)
      ASSERT(testB0,'B1 read PL Failure')
      !Integers
      CALL tmpPL%clear()
      CALL h5%fread('groupI',tmpPL)
      CALL tmpPL%get('groupI->memL0',testL0)
      ASSERT(testL0 == refL0,'L0 read PL Failure')
      CALL tmpPL%get('groupI->memL1',testL1)
      ASSERT(ALL(testL1 == refL1),'L1 read PL Failure')
      CALL tmpPL%get('groupI->memL2',testL2)
      ASSERT(ALL(testL2 == refL2),'L2 read PL Failure')
      CALL tmpPL%get('groupI->memL3',testL3)
      ASSERT(ALL(testL3 == refL3),'L3 read PL Failure')
      CALL tmpPL%get('groupI->memN0',testN0)
      ASSERT(testN0 == refN0,'N0 read PL Failure')
      CALL tmpPL%get('groupI->memN1',testN1)
      ASSERT(ALL(testN1 == refN1),'N1 read PL Failure')
      CALL tmpPL%get('groupI->memN2',testN2)
      ASSERT(ALL(testN2 == refN2),'N2 read PL Failure')
      CALL tmpPL%get('groupI->memN3',testN3)
      ASSERT(ALL(testN3 == refN3),'N3 read PL Failure')
      !Reals
      CALL tmpPL%clear()
      CALL h5%fread('groupR',tmpPL)
      CALL tmpPL%get('groupR->memD0',testD0)
      ASSERT(testD0 == refD0,'D0 read PL Failure')
      CALL tmpPL%get('groupR->memD1',testD1)
      ASSERT(ALL(testD1 == refD1),'D1 read PL Failure')
      CALL tmpPL%get('groupR->memD2',testD2)
      ASSERT(ALL(testD2 == refD2),'D2 read PL Failure')
      CALL tmpPL%get('groupR->memD3',testD3)
      ASSERT(ALL(testD3 == refD3),'D3 read PL Failure')
      CALL tmpPL%get('groupR->memS0',testS0)
      ASSERT(testS0 == refS0,'S0 read PL Failure')
      CALL tmpPL%get('groupR->memS1',testS1)
      ASSERT(ALL(testS1 == refS1),'S1 read PL Failure')
      CALL tmpPL%get('groupR->memS2',testS2)
      ASSERT(ALL(testS2 == refS2),'S2 read PL Failure')
      CALL tmpPL%get('groupR->memS3',testS3)
      ASSERT(ALL(testS3 == refS3),'S3 read PL Failure')
      !Strings
      CALL tmpPL%clear()
      CALL tmpPL%clear()
      CALL h5%fread('groupST',tmpPL)
      CALL tmpPL%get('groupST->memST0',testST0)
      ASSERT(testST0 == refST0,'ST0 read PL Failure')
      CALL tmpPL%get('groupST->memST1',testST1)
      ASSERTFAIL(SIZE(testST1) == SIZE(refST1),'ST1 Sizes')
      DO i=1,SIZE(testST1)
        ASSERT(testST1(i) == refST1(i),'ST1 read PL Failure')
      ENDDO
      CALL tmpPL%get('groupST->memST2',testST2)
      testB0=(SIZE(testST2,DIM=1) == SIZE(refST2,DIM=1)) .AND. &
        (SIZE(testST2,DIM=2) == SIZE(refST2,DIM=2))
      ASSERTFAIL(testB0,'ST2 Sizes')
      DO j=1,SIZE(testST2,DIM=2)
        DO i=1,SIZE(testST2,DIM=1)
          ASSERT(testST2(i,j) == refST2(i,j),'ST2 read PL Failure')
         ENDDO
       ENDDO
      !Character array, read in as a 1-D string array
      CALL tmpPL%get('groupST->memCA0',testST1)
      ASSERTFAIL(SIZE(testST1) == SIZE(refST0CA),'ST1 Sizes')
      DO i=1,SIZE(testST1)
        ASSERT(testST1(i) == refST0CA(i),'ST1 read PL Failure')
        FINFO() i,CHAR(testST1(i))//' | '//CHAR(refST0CA(i))
      ENDDO
      CALL tmpPL%clear()

      DEALLOCATE(testD1,testD2,testD3,testD4,testD5,testD6,testD7,testS1,testS2,&
        testS3,testS4,testS5,testS6,testS7,testL1,testL2,testL3,testL4,testL5, &
        testL6,testL7,testB1,testB2,testB3,testST1,testST2,testST3,testN1,testN2, &
        testN3,testN4,testN5,testN6,testN7,testDP4)

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
