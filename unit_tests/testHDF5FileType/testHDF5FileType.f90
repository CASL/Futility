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
  USE ISO_C_BINDING
  USE TAU_Stubs
  USE UnitTest
  USE IntrType
  USE Strings
  USE ExceptionHandler
  USE ParameterLists
  USE Times
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
  TYPE(StringType) :: refST0,refCNCHAR0
  TYPE(StringType),ALLOCATABLE :: refST1(:),refSTC1(:),refST0CA(:),refST2(:,:),refST3(:,:,:)
  TYPE(StringType),ALLOCATABLE :: refCNCHAR1(:),refCNCHAR2(:,:),refCNCHAR3(:,:,:)
  CHARACTER(LEN=32) :: refC1,integer_name,real_name,string_name,char_name
  CHARACTER(LEN=12) :: helper_string
  LOGICAL(SBK) :: exists
  TYPE(StringType),ALLOCATABLE :: refsets(:)
  TYPE(HDF5FileType) :: h5file

  CALL testMPI%init(PE_COMM_WORLD)
  IF(testMPI%rank /= 0)  utest_master=.FALSE.

  CREATE_TEST("HDFFILETYPE")

  CALL testHDF5FileTypeSetup()
  CALL HDF5Open()
  REGISTER_SUBTEST("Uninit",testHDF5FileTypeUninit)
  REGISTER_SUBTEST("Init",testHDF5FileTypeCreateDelete)
  REGISTER_SUBTEST("Error Checks",testHDF5FileTypeErrorCheck)
  REGISTER_SUBTEST("%fread",testHDF5FileTypeRead)
  IF(utest_nfail == 0) THEN
    REGISTER_SUBTEST("%fwrite",testHDF5FileTypeWrite)
    REGISTER_SUBTEST("%overwrite",testHDF5FileTypeOverwrite)
    REGISTER_SUBTEST("File Compression",testCompress)
    REGISTER_SUBTEST("%isCompressed",testIsCompressed)
    REGISTER_SUBTEST("%getChunkSize",testGetChunkSize)
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
      refL7,refN1,refN2,refN3,refN4,refN5,refN6,refN7,refST1,refST2,refST3,refsets, &
      refCNCHAR1,refCNCHAR2,refCNCHAR3,refSTC1,refST0CA)

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
      ALLOCATE(refCNCHAR1(1))
      ALLOCATE(refCNCHAR2(1,1))
      ALLOCATE(refCNCHAR3(1,1,1))
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
      refCNCHAR0=C_NULL_CHAR
      refCNCHAR1=C_NULL_CHAR
      refCNCHAR2=C_NULL_CHAR
      DO k=1,SIZE(refCNCHAR3,DIM=3)
        refCNCHAR3(:,:,k)=C_NULL_CHAR
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

      !Set Attribute write values
      string_name='string'
      char_name='char'
      integer_name='int'
      real_name='real'

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
      CALL h5%fwrite('groupST->CNULLCHAR0',refCNCHAR0)
      CALL h5%fwrite('groupST->CNULLCHAR1',refCNCHAR1)
      CALL h5%fwrite('groupST->CNULLCHAR2',refCNCHAR2)
      CALL h5%fwrite('groupST->CNULLCHAR3',refCNCHAR3)
      CALL h5%fwrite('groupST->memST1',refST1)
      CALL h5%fwrite('groupST->memST2',refST2)
      CALL h5%fwrite('groupST->memST3',refST3)

      !Write Attribute Values
      CALL h5%write_attribute('groupB->memB0',integer_name,refN0)
      CALL h5%write_attribute('groupB->memB1',string_name,refST0)
      CALL h5%write_attribute('groupB->memB1',char_name,refC1)
      CALL h5%write_attribute('groupB->memB2',real_name,refD0)

      CALL h5%fclose()

    ENDSUBROUTINE testHDF5FileTypeSetup
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeUninit()
      TYPE(HDF5FileType) :: h5
      ASSERT(.NOT.h5%isinit,'%isinit')
      ASSERT(.NOT.h5%isNew(),'%newstat')
      ASSERT_EQ(LEN(h5%fullname),0,'%fullname')
      ASSERT_EQ(h5%getUnitNo(),-1,'%unitno')
      ASSERT_EQ(h5%file_id,0,'%file_id')
    ENDSUBROUTINE testHDF5FileTypeUninit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5FileTypeCreateDelete()
      TYPE(HDF5FileType) :: h5

      CALL h5%e%setQuietMode(.TRUE.)
      CALL h5%init('createdeletetest.h5','NEW')

      ASSERT(h5%isinit,'HDF5 object no properly initialized')
      CALL h5%fopen()
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),0,'hdf5_filetype%fopen')
      CALL h5%fclose()
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),0,'hdf5_filetype%fclose')
      CALL h5%clear()
      ASSERT(.NOT.h5%isinit,'HDF5 object not properly cleared.')

      CALL h5%init('createdeletetest.h5','WRITE')
      CALL h5%fopen()
      ASSERT(h5%isWrite(),'HDF object %isWrite() should be .TRUE.')
      CALL h5%mkdir('testGroup')
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),0,'hdf5_filetype%init ''WRITE''')
      CALL h5%clear()

      CALL h5%init('createdeletetest.h5','READ')
      CALL h5%fopen()
      ASSERT(.NOT.(h5%isWrite()),'HDF object %isWrite() should be .FALSE.')
      CALL h5%e%setStopOnError(.FALSE.)
      CALL h5%mkdir('testGroup')
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),1,'hdf5_filetype%init ''READ''')
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
      INTEGER(SIK) :: tmpi
      REAL(SDK),POINTER :: d4ptr(:,:,:,:)
      d4ptr => NULL()

      CALL h5%e%setQuietMode(.TRUE.)
      !Adding the *2 for now to get the test passing.  Since the simplification,
      !the pre and post routines have two %isinit checks.  All these checks are
      !mostly redundant now though, one check checks the same code as all of them now.
      CALL h5%fwrite('groupR->memD0',refD0)
      tmpi=1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_d0 %isinit check')
      CALL h5%fwrite('groupR->memD1',refD1,SHAPE(refD1))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_d1 %isinit check')
      CALL h5%fwrite('groupR->memD2',refD2,SHAPE(refD2))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_d2 %isinit check')
      CALL h5%fwrite('groupR->memD3',refD3,SHAPE(refD3))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_d3 %isinit check')
      CALL h5%fwrite('groupR->memD4',refD4,SHAPE(refD4))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_d4 %isinit check')
      CALL h5%fwrite('groupR->memD5',refD5,SHAPE(refD5))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_d5 %isinit check')
      CALL h5%fwrite('groupR->memD6',refD6,SHAPE(refD6))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_d6 %isinit check')
      CALL h5%fwrite('groupR->memD7',refD7,SHAPE(refD7))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_d7 %isinit check')
      CALL h5%fwrite('groupR->memS0',refS0)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_s0 %isinit check')
      CALL h5%fwrite('groupR->memS1',refS1,SHAPE(refS1))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_s1 %isinit check')
      CALL h5%fwrite('groupR->memS2',refS2,SHAPE(refS2))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_s2 %isinit check')
      CALL h5%fwrite('groupR->memS3',refS3,SHAPE(refS3))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_s3 %isinit check')
      CALL h5%fwrite('groupR->memS4',refS4,SHAPE(refS4))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_s4 %isinit check')
      CALL h5%fwrite('groupR->memS5',refS5,SHAPE(refS5))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_s5 %isinit check')
      CALL h5%fwrite('groupR->memS6',refS6,SHAPE(refS6))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_s6 %isinit check')
      CALL h5%fwrite('groupR->memS7',refS7,SHAPE(refS7))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_s7 %isinit check')
      CALL h5%fwrite('groupI->memL0',refL0)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_l0 %isinit check')
      CALL h5%fwrite('groupI->memL1',refL1,SHAPE(refL1))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_l1 %isinit check')
      CALL h5%fwrite('groupI->memL2',refL2,SHAPE(refL2))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_l2 %isinit check')
      CALL h5%fwrite('groupI->memL3',refL3,SHAPE(refL3))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_l3 %isinit check')
      CALL h5%fwrite('groupI->memL4',refL4,SHAPE(refL4))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_l4 %isinit check')
      CALL h5%fwrite('groupI->memL5',refL5,SHAPE(refL5))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_l5 %isinit check')
      CALL h5%fwrite('groupI->memL6',refL6,SHAPE(refL6))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_l6 %isinit check')
      CALL h5%fwrite('groupI->memL7',refL7,SHAPE(refL7))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_l7 %isinit check')
      CALL h5%fwrite('groupI->memN0',refN0)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_n0 %isinit check')
      CALL h5%fwrite('groupI->memN1',refN1,SHAPE(refN1))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_n1 %isinit check')
      CALL h5%fwrite('groupI->memN2',refN2,SHAPE(refN2))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_n2 %isinit check')
      CALL h5%fwrite('groupI->memN3',refN3,SHAPE(refN3))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_n3 %isinit check')
      CALL h5%fwrite('groupI->memN4',refN4,SHAPE(refN4))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_n4 %isinit check')
      CALL h5%fwrite('groupI->memN5',refN5,SHAPE(refN5))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_n5 %isinit check')
      CALL h5%fwrite('groupI->memN6',refN6,SHAPE(refN6))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_n6 %isinit check')
      CALL h5%fwrite('groupI->memN7',refN7,SHAPE(refN7))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_n7 %isinit check')
      CALL h5%fwrite('groupB->memB0',refB0)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_b0 %isinit check')
      CALL h5%fwrite('groupB->memB1',refB1,SHAPE(refB1))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_b1 %isinit check')
      CALL h5%fwrite('groupB->memB2',refB2,SHAPE(refB2))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_b2 %isinit check')
      CALL h5%fwrite('groupB->memB3',refB3,SHAPE(refB3))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_b3 %isinit check')
      CALL h5%fwrite('groupST->CNULLCHAR0',refCNCHAR0)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_st0 %isinit check')
      CALL h5%fwrite('groupST->CNULLCHAR1',refCNCHAR1)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_st1 %isinit check')
      CALL h5%fwrite('groupST->CNULLCHAR2',refCNCHAR2)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_st2 %isinit check')
      CALL h5%fwrite('groupST->CNULLCHAR3',refCNCHAR3)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_st3 %isinit check')
      CALL h5%fwrite('groupST->memST0',refST0)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_st0 %isinit check')
      CALL h5%fwrite('groupST->memST1',refST1,SHAPE(refST1))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_st1 %isinit check')
      CALL h5%fwrite('groupST->memST2',refST2,SHAPE(refST2))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_st2 %isinit check')
      CALL h5%fwrite('groupST->memST3',refST3,SHAPE(refST3))
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_st3 %isinit check')
      CALL h5%fwrite('groupC->memC1',refC1)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%write_c1 %isinit check')
      CALL h5%mkdir('groupI')
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2-1,'%mkdir %isinit check')
      CALL h5%ls('groupR',refST1)
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%ls %isinit check')

      CALL h5%fread('groupR->memD0',refD0)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_d0 %isinit check')
      CALL h5%fread('groupR->memD1',refD1)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_d1 %isinit check')
      CALL h5%fread('groupR->memD2',refD2)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_d2 %isinit check')
      CALL h5%fread('groupR->memD3',refD3)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_d3 %isinit check')
      CALL h5%fread('groupR->memD4',refD4)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_d4 %isinit check')
      CALL h5%freadp('groupR->memD4',d4ptr)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_d4 %isinit check')
      CALL h5%fread('groupR->memS0',refS0)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_s0 %isinit check')
      CALL h5%fread('groupR->memS1',refS1)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_s1 %isinit check')
      CALL h5%fread('groupR->memS2',refS2)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_s2 %isinit check')
      CALL h5%fread('groupR->memS3',refS3)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_s3 %isinit check')
      CALL h5%fread('groupR->memS4',refS4)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_s4 %isinit check')
      CALL h5%fread('groupI->memL0',refL0)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_l0 %isinit check')
      CALL h5%fread('groupI->memL1',refL1)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_l1 %isinit check')
      CALL h5%fread('groupI->memL2',refL2)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_l2 %isinit check')
      CALL h5%fread('groupI->memL3',refL3)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_l3 %isinit check')
      CALL h5%fread('groupI->memN0',refN0)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_n0 %isinit check')
      CALL h5%fread('groupI->memN1',refN1)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_n1 %isinit check')
      CALL h5%fread('groupI->memN2',refN2)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_n2 %isinit check')
      CALL h5%fread('groupI->memN3',refN3)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_n3 %isinit check')
      CALL h5%fread('groupB->memB0',refB0)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_b0 %isinit check')
      CALL h5%fread('groupB->memB1',refB1)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_b1 %isinit check')
      CALL h5%fread('groupB->memB2',refB2)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_b2 %isinit check')
      CALL h5%fread('groupB->memB3',refB3)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_b3 %isinit check')
      CALL h5%fread('groupST->memST0',refST0)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_st0 %isinit check')
      CALL h5%fread('groupST->memST1',refST1)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_st1 %isinit check')
      CALL h5%fread('groupST->memST2',refST2)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_st2 %isinit check')
      CALL h5%fread('groupST->memST3',refST3)
      tmpi=tmpi+1
      ASSERT_EQ(h5%e%getCounter(EXCEPTION_ERROR),tmpi*2,'%read_st3 %isinit check')

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
      CALL h5%mkdir('groupCNCHAR')
      CALL h5%fwrite('groupCNCHAR->CNCHAR0',refCNCHAR0)
      CALL h5%fwrite('groupCNCHAR->CNCHAR1',refCNCHAR1,SHAPE(refCNCHAR1))
      CALL h5%fwrite('groupCNCHAR->CNCHAR2',refCNCHAR2,SHAPE(refCNCHAR2))
      CALL h5%fwrite('groupCNCHAR->CNCHAR3',refCNCHAR3,SHAPE(refCNCHAR3))
      CALL h5%mkdir('groupST')
      CALL h5%fwrite('groupST->memST0',refST0)
      CALL h5%fwrite('groupST->memST1',refST1,SHAPE(refST1))
      CALL h5%fwrite('groupST->memST2',refST2,SHAPE(refST2))
      CALL h5%fwrite('groupST->memST3',refST3,SHAPE(refST3))
      CALL h5%mkdir('groupC')
      CALL h5%fwrite('groupC->memC1',refC1,LEN(refC1))

      CALL h5%fread('groupR->memD0',testD0)
      ASSERT_EQ(testD0,refD0,'D0 Write Failure with gdims_in')
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
      ASSERT_EQ(testS0,refS0,'S0 Write Failure with gdims_in')
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
      ASSERT_EQ(testN0,refN0,'N0 Write Failure with gdims_in')
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
      ASSERT_EQ(testL0,refL0,'L0 Write Failure with gdims_in')
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
      CALL h5%fread('groupCNCHAR->CNCHAR0',testST0)
      ASSERT_EQ(CHAR(testST0),'','CNCHAR0 is non empty! Write Failure with gdims_in')
      CALL h5%fread('groupCNCHAR->CNCHAR1',testST1)
      checkwrite=.TRUE.
      DO i=1,SIZE(refCNCHAR1)
        IF(testST1(i)/='') checkwrite=.FALSE.
      ENDDO
      ASSERT(checkwrite,'CNCHAR1 is non empty! Write Failure with gdims_in')
      CALL h5%fread('groupCNCHAR->CNCHAR2',testST2)
      checkwrite=.TRUE.
      DO i=1,SIZE(refCNCHAR2,1)
        DO j=1,SIZE(refCNCHAR2,2)
          IF(testST2(i,j)/='') checkwrite=.FALSE.
        ENDDO
      ENDDO
      ASSERT(checkwrite,'CNCHAR2 is non empty! Write Failure with gdims_in')
      CALL h5%fread('groupCNCHAR->CNCHAR3',testST3)
      checkwrite=.TRUE.
      DO i=1,SIZE(refCNCHAR3,1)
        DO j=1,SIZE(refCNCHAR3,2)
          DO k=1,SIZE(refCNCHAR3,3)
            IF(testST3(i,j,k)/='') checkwrite=.FALSE.
          ENDDO
        ENDDO
      ENDDO
      ASSERT(checkwrite,'CNCHAR3 is non empty! Write Failure with gdims_in')

      CALL h5%fread('groupST->memST0',testST0)
      ASSERT_EQ(CHAR(testST0),CHAR(refST0),'ST0 Write Failure with gdims_in')
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
      ASSERT_EQ(TRIM(testC1),TRIM(refC1),'C1 Write Failure with gdims_in')

      i=h5%ngrp('groupR')
      ASSERT_EQ(i,16,'ngrp_HDF5FileType')

      ! Delete the file
      CALL h5%clear(.TRUE.)

      COMPONENT_TEST('%fwrite without gdims')
      CALL h5%init('writetest.h5','NEW')
      CALL h5%fopen()

      ! Test writing without the gdims arguments
      CALL h5%mkdir('groupR')
      CALL h5%mkdir('groupI')
      CALL h5%mkdir('groupB')
      CALL h5%mkdir('groupCNCHAR')
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
      CALL h5%fwrite('groupCNCHAR->CNCHAR0',refCNCHAR0)
      CALL h5%fwrite('groupCNCHAR->CNCHAR1',refCNCHAR1)
      CALL h5%fwrite('groupCNCHAR->CNCHAR2',refCNCHAR2)
      CALL h5%fwrite('groupCNCHAR->CNCHAR3',refCNCHAR3)
      CALL h5%fwrite('groupST->memST0',refST0)
      CALL h5%fwrite('groupST->memST1',refST1)
      CALL h5%fwrite('groupST->memST2',refST2)
      CALL h5%fwrite('groupST->memST3',refST3)
      CALL h5%fwrite('groupC->memC1',refC1)

      COMPONENT_TEST('%write_attribute')
      !WRITE ATTRIBUTESr
      CALL h5%write_attribute('groupB->memB0',integer_name,refN0)
      CALL h5%write_attribute('groupB->memB1',string_name,refST0)
      CALL h5%write_attribute('groupB->memB1',char_name,refC1)
      CALL h5%write_attribute('groupB->memB2',real_name,refD0)

      !READ ATTRIBUTES
      CALL h5%read_attribute('groupB->memB0',integer_name,testN0)
      ASSERT_EQ(refN0,testN0,'integer read fail')
      CALL h5%read_attribute('groupB->memB1',string_name,testST0)
      ASSERT_EQ(CHAR(refST0),CHAR(testST0),'string read fail')
      CALL h5%read_attribute('groupB->memB1',char_name,testC1)
      ASSERT_EQ(TRIM(refC1),TRIM(testC1),'character read fail')
      CALL h5%read_attribute('groupB->memB2',real_name,testD0)
      ASSERT_SOFTEQ(refD0,testD0,1.E-12_SRK,'l')

      CALL h5%fread('groupR->memD0',testD0)
      ASSERT_EQ(testD0,refD0,'D0 Write Failure')
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
      ASSERT_EQ(testS0,refS0,'S0 Write Failure')
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
      ASSERT_EQ(testN0,refN0,'N0 Write Failure')
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
      ASSERT_EQ(testL0,refL0,'L0 Write Failure')
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
      CALL h5%fread('groupCNCHAR->CNCHAR0',testST0)
      ASSERT_EQ(CHAR(testST0),'','CNCHAR0 is non empty! Write Failure')
      CALL h5%fread('groupCNCHAR->CNCHAR1',testST1)
      checkwrite=.TRUE.
      DO i=1,SIZE(refCNCHAR1)
        IF(testST1(i)/='') checkwrite=.FALSE.
      ENDDO
      ASSERT(checkwrite,'CNCHAR1 is non empty! Write Failure')
      CALL h5%fread('groupCNCHAR->CNCHAR2',testST2)
      checkwrite=.TRUE.
      DO i=1,SIZE(refCNCHAR2,1)
        DO j=1,SIZE(refCNCHAR2,2)
          IF(testST2(i,j)/='') checkwrite=.FALSE.
        ENDDO
      ENDDO
      ASSERT(checkwrite,'CNCHAR2 is non empty! Write Failure')
      CALL h5%fread('groupCNCHAR->CNCHAR3',testST3)
      checkwrite=.TRUE.
      DO i=1,SIZE(refCNCHAR3,1)
        DO j=1,SIZE(refCNCHAR3,2)
          DO k=1,SIZE(refCNCHAR3,3)
            IF(testST3(i,j,k)/='') checkwrite=.FALSE.
          ENDDO
        ENDDO
      ENDDO
      ASSERT(checkwrite,'CNCHAR3 is non empty! Write Failure')

      CALL h5%fread('groupST->memST0',testST0)
      ASSERT_EQ(CHAR(testST0),CHAR(refST0),'ST0 Write Failure')
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
      ASSERT_EQ(TRIM(testC1),TRIM(refC1),'C1 Write Failure')
      i=h5%ngrp('groupR')
      ASSERT_EQ(i,16,'ngrp_HDF5FileType')


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
      ASSERT_EQ(testC1,testC2,'Valid Hardlink')
      CALL h5%e%setQuietMode(.TRUE.)
      CALL h5%e%setStopOnError(.FALSE.)
      CALL h5%createHardLink('groupThatDoesNotExist->memC1','groupC->anotherGroup->moreGroups')
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::createHardLink_HDF5FileType '// &
        '- Target of new link must exist in file!'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Non-existant Target Link')
      CALL h5%createHardLink('groupC->memC1','groupC->anotherGroup->moreGroups')
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::createHardLink_HDF5FileType '// &
        '- Location of new link already exists!'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Pre-existing New Link')

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
!> @brief Demonstrate that user can overwrite HDF5 file datasets
    SUBROUTINE testHDF5FileTypeOverwrite()
      TYPE(HDF5FileType) :: h5
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
      REAL(SDK),ALLOCATABLE :: testD1(:),testD2(:,:),testD3(:,:,:),testD4(:,:,:,:)
      REAL(SDK),ALLOCATABLE :: testD5(:,:,:,:,:),testD6(:,:,:,:,:,:),testD7(:,:,:,:,:,:,:)
      REAL(SSK),ALLOCATABLE :: testS1(:),testS2(:,:),testS3(:,:,:),testS4(:,:,:,:)
      REAL(SSK),ALLOCATABLE :: testS5(:,:,:,:,:),testS6(:,:,:,:,:,:),testS7(:,:,:,:,:,:,:)
      REAL(SSK),ALLOCATABLE :: newS1(:),newS2(:,:),newS3(:,:,:),newS4(:,:,:,:)
      REAL(SSK),ALLOCATABLE :: newS5(:,:,:,:,:),newS6(:,:,:,:,:,:),newS7(:,:,:,:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: testB1(:),testB2(:,:),testB3(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: testL1(:),testL2(:,:),testL3(:,:,:),testL4(:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: testL5(:,:,:,:,:),testL6(:,:,:,:,:,:),testL7(:,:,:,:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: testN1(:),testN2(:,:),testN3(:,:,:),testN4(:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: testN5(:,:,:,:,:),testN6(:,:,:,:,:,:),testN7(:,:,:,:,:,:,:)
      REAL(SDK),ALLOCATABLE :: newD1(:),newD2(:,:),newD3(:,:,:),newD4(:,:,:,:)
      REAL(SDK),ALLOCATABLE :: newD5(:,:,:,:,:),newD6(:,:,:,:,:,:),newD7(:,:,:,:,:,:,:)
      LOGICAL(SBK),ALLOCATABLE :: newB1(:),newB2(:,:),newB3(:,:,:)
      INTEGER(SLK),ALLOCATABLE :: newL1(:),newL2(:,:),newL3(:,:,:),newL4(:,:,:,:)
      INTEGER(SLK),ALLOCATABLE :: newL5(:,:,:,:,:),newL6(:,:,:,:,:,:),newL7(:,:,:,:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: newN1(:),newN2(:,:),newN3(:,:,:),newN4(:,:,:,:)
      INTEGER(SNK),ALLOCATABLE :: newN5(:,:,:,:,:),newN6(:,:,:,:,:,:),newN7(:,:,:,:,:,:,:)
      TYPE(StringType),ALLOCATABLE :: testST1(:),testST2(:,:),testST3(:,:,:)
      TYPE(StringType),ALLOCATABLE :: newST1(:),newST2(:,:),newST3(:,:,:)
      TYPE(StringType) :: testST0, newST0
      REAL(SDK) :: testD0,newD0
      REAL(SSK) :: testS0,newS0
      INTEGER(SLK) :: testL0
      INTEGER(SNK) :: testN0, newN0, i, j, k
      ! Picking a number that happens to work for the test.  The conversion between SLK and SDK
      ! is losing precision and leading to a different value being written than indicated in some
      ! cases.  The array values need to be smaller because of a conversion to SNK
      ! in the procedure truncating the value.
      INTEGER(SLK),PARAMETER :: newL0=12345678912345672_SLK, arrVal=2000000000_SLK
      LOGICAL(SBK) :: testB0,newB0
      CHARACTER(LEN=32) :: testC1,newC1
      REAL(SRK), PARAMETER :: rtol=1.0E-6_SRK
      REAL(SSK), PARAMETER :: rtols=1.0E-6_SSK

      COMPONENT_TEST('%set to overwrite and overwrite data')
      ! Open as new, write initial data, then overwrite it.
      CALL h5%init('writetest.h5','NEW')
      CALL h5%setOverwriteStat(.TRUE.)
      CALL h5%fopen()

      ! Write data first time
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
      CALL h5%mkdir('groupCNCHAR')
      CALL h5%fwrite('groupCNCHAR->CNCHAR0',refCNCHAR0)
      CALL h5%fwrite('groupCNCHAR->CNCHAR1',refCNCHAR1,SHAPE(refCNCHAR1))
      CALL h5%fwrite('groupCNCHAR->CNCHAR2',refCNCHAR2,SHAPE(refCNCHAR2))
      CALL h5%fwrite('groupCNCHAR->CNCHAR3',refCNCHAR3,SHAPE(refCNCHAR3))
      CALL h5%mkdir('groupST')
      CALL h5%fwrite('groupST->memST0',refST0)
      CALL h5%fwrite('groupST->memST1',refST1,SHAPE(refST1))
      CALL h5%fwrite('groupST->memST2',refST2,SHAPE(refST2))
      CALL h5%fwrite('groupST->memST3',refST3,SHAPE(refST3))
      CALL h5%mkdir('groupC')
      CALL h5%fwrite('groupC->memC1',refC1,LEN(refC1))

      ! Write data second time, overwriting existing
      ! Overwite with data that is different and check that the new data was written
      newD0=refD0+1.0_SDK
      newD1=refD1+1.0_SDK
      newD2=refD2+1.0_SDK
      newD3=refD3+1.0_SDK
      newD4=refD4+1.0_SDK
      newD5=refD5+1.0_SDK
      newD6=refD6+1.0_SDK
      newD7=refD7+1.0_SDK
      newS0=refS0+1.0_SSK
      newS1=refS1+1.0_SSK
      newS2=refS2+1.0_SSK
      newS3=refS3+1.0_SSK
      newS4=refS4+1.0_SSK
      newS5=refS5+1.0_SSK
      newS6=refS6+1.0_SSK
      newS7=refS7+1.0_SSK
      newL1=refL1
      newL1=arrVal
      newL2=refL2
      newL2=arrVal
      newL3=refL3
      newL3=arrVal
      newL4=refL4
      newL4=arrVal
      newL5=refL5
      newL5=arrVal
      newL6=refL6
      newL6=arrVal
      newL7=refL7
      newL7=arrVal
      newN0=refN0+1_SNK
      newN1=refN1+1_SNK
      newN2=refN2+1_SNK
      newN3=refN3+1_SNK
      newN4=refN4+1_SNK
      newN5=refN5+1_SNK
      newN6=refN6+1_SNK
      newN7=refN7+1_SNK
      newB0=.NOT.refB0
      newB1=.NOT.refB1
      newB2=.NOT.refB2
      newB3=.NOT.refB3
      newST0='new test'
      ALLOCATE(newST1(SIZE(refST1)))
      DO i=1,SIZE(newST1)
        newST1(i)=newST0
      ENDDO
      ALLOCATE(newST2(SIZE(refST2,1),SIZE(refST2,2)))
      DO i=1,SIZE(newST2,1)
        DO j=1,SIZE(newST2,2)
          newST2(i,j)=newST0
        ENDDO
      ENDDO
      ALLOCATE(newST3(SIZE(refST3,1),SIZE(refST3,2),SIZE(refST3,3)))
      DO i=1,SIZE(newST3,1)
        DO j=1,SIZE(newST3,2)
          DO k=1,SIZE(newST3,3)
            newST3(i,j,k)=newST0
          ENDDO
        ENDDO
      ENDDO
      newC1='new test'

      msg = 'Overwritten data not correct'
      CALL h5%mkdir('groupR')
      CALL h5%fwrite('groupR->memD0',newD0)
      CALL h5%fread ('groupR->memD0',testD0)
      ASSERT_SOFTEQ(newD0,testD0,rtol,msg)

      CALL h5%fwrite('groupR->memD1',newD1,SHAPE(newD1))
      CALL h5%fread ('groupR->memD1',testD1)
      ASSERT_SOFTEQ(newD1(1),testD1(1),rtol,msg)

      CALL h5%fwrite('groupR->memD2',newD2,SHAPE(newD2))
      CALL h5%fread ('groupR->memD2',testD2)
      ASSERT_SOFTEQ(newD2(1,1),testD2(1,1),rtol,msg)

      CALL h5%fwrite('groupR->memD3',newD3,SHAPE(newD3))
      CALL h5%fread ('groupR->memD3',testD3)
      ASSERT_SOFTEQ(newD3(1,1,1),testD3(1,1,1),rtol,msg)

      CALL h5%fwrite('groupR->memD4',newD4,SHAPE(newD4))
      CALL h5%fread ('groupR->memD4',testD4)
      ASSERT_SOFTEQ(newD4(1,1,1,1),testD4(1,1,1,1),rtol,msg)

      CALL h5%fwrite('groupR->memD5',newD5,SHAPE(newD5))
      CALL h5%fread ('groupR->memD5',testD5)
      ASSERT_SOFTEQ(newD5(1,1,1,1,1),testD5(1,1,1,1,1),rtol,msg)

      CALL h5%fwrite('groupR->memD6',newD6,SHAPE(newD6))
      CALL h5%fread ('groupR->memD6',testD6)
      ASSERT_SOFTEQ(newD6(1,1,1,1,1,1),testD6(1,1,1,1,1,1),rtol,msg)

      CALL h5%fwrite('groupR->memD7',newD7,SHAPE(newD7))
      CALL h5%fread ('groupR->memD7',testD7)
      ASSERT_SOFTEQ(newD7(1,1,1,1,1,1,1),testD7(1,1,1,1,1,1,1),rtol,msg)

      CALL h5%fwrite('groupR->memS0',newS0)
      CALL h5%fread ('groupR->memS0',testS0)
      ASSERT_SOFTEQ(newS0,testS0,rtols,msg)

      CALL h5%fwrite('groupR->memS1',newS1,SHAPE(newS1))
      CALL h5%fread ('groupR->memS1',testS1)
      ASSERT_SOFTEQ(newS1(1),testS1(1),rtols,msg)

      CALL h5%fwrite('groupR->memS2',newS2,SHAPE(newS2))
      CALL h5%fread ('groupR->memS2',testS2)
      ASSERT_SOFTEQ(newS2(1,1),testS2(1,1),rtols,msg)

      CALL h5%fwrite('groupR->memS3',newS3,SHAPE(newS3))
      CALL h5%fread ('groupR->memS3',testS3)
      ASSERT_SOFTEQ(newS3(1,1,1),testS3(1,1,1),rtols,msg)

      CALL h5%fwrite('groupR->memS4',newS4,SHAPE(newS4))
      CALL h5%fread ('groupR->memS4',testS4)
      ASSERT_SOFTEQ(newS4(1,1,1,1),testS4(1,1,1,1),rtols,msg)

      CALL h5%fwrite('groupR->memS5',newS5,SHAPE(newS5))
      CALL h5%fread ('groupR->memS5',testS5)
      ASSERT_SOFTEQ(newS5(1,1,1,1,1),testS5(1,1,1,1,1),rtols,msg)

      CALL h5%fwrite('groupR->memS6',newS6,SHAPE(newS6))
      CALL h5%fread ('groupR->memS6',testS6)
      ASSERT_SOFTEQ(newS6(1,1,1,1,1,1),testS6(1,1,1,1,1,1),rtols,msg)

      CALL h5%fwrite('groupR->memS7',newS7,SHAPE(newS7))
      CALL h5%fread ('groupR->memS7',testS7)
      ASSERT_SOFTEQ(newS7(1,1,1,1,1,1,1),testS7(1,1,1,1,1,1,1),rtols,msg)

      CALL h5%mkdir('groupI')
      CALL h5%fwrite('groupI->memL0',newL0)
      CALL h5%fread ('groupI->memL0',testL0)
      ASSERT_EQ(newL0,testL0,msg)

      CALL h5%fwrite('groupI->memL1',newL1,SHAPE(newL1))
      CALL h5%fread ('groupI->memL1',testL1)
      ASSERT(ALL(arrVal==testL1),msg)

      CALL h5%fwrite('groupI->memL2',newL2,SHAPE(newL2))
      CALL h5%fread ('groupI->memL2',testL2)
      ASSERT(ALL(arrVal==testL2),msg)

      CALL h5%fwrite('groupI->memL3',newL3,SHAPE(newL3))
      CALL h5%fread ('groupI->memL3',testL3)
      ASSERT(ALL(arrVal==testL3),msg)

      CALL h5%fwrite('groupI->memL4',newL4,SHAPE(newL4))
      CALL h5%fread ('groupI->memL4',testL4)
      ASSERT(ALL(arrVal==testL4),msg)

      CALL h5%fwrite('groupI->memL5',newL5,SHAPE(newL5))
      CALL h5%fread ('groupI->memL5',testL5)
      ASSERT(ALL(arrVal==testL5),msg)

      CALL h5%fwrite('groupI->memL6',newL6,SHAPE(newL6))
      CALL h5%fread ('groupI->memL6',testL6)
      ASSERT(ALL(arrVal==testL6),msg)

      CALL h5%fwrite('groupI->memL7',newL7,SHAPE(newL7))
      CALL h5%fread ('groupI->memL7',testL7)
      ASSERT(ALL(arrVal==testL7),msg)

      CALL h5%fwrite('groupI->memN0',newN0)
      CALL h5%fread ('groupI->memN0',testN0)
      ASSERT_EQ(newN0,testN0,msg)

      CALL h5%fwrite('groupI->memN1',newN1,SHAPE(newN1))
      CALL h5%fread ('groupI->memN1',testN1)
      ASSERT(newN1(1)==testN1(1),msg)

      CALL h5%fwrite('groupI->memN2',newN2,SHAPE(newN2))
      CALL h5%fread ('groupI->memN2',testN2)
      ASSERT(newN2(1,1)==testN2(1,1),msg)

      CALL h5%fwrite('groupI->memN3',newN3,SHAPE(newN3))
      CALL h5%fread ('groupI->memN3',testN3)
      ASSERT(newN3(1,1,1)==testN3(1,1,1),msg)

      CALL h5%fwrite('groupI->memN4',newN4,SHAPE(newN4))
      CALL h5%fread ('groupI->memN4',testN4)
      ASSERT(newN4(1,1,1,1)==testN4(1,1,1,1),msg)

      CALL h5%fwrite('groupI->memN5',newN5,SHAPE(newN5))
      CALL h5%fread ('groupI->memN5',testN5)
      ASSERT(newN5(1,1,1,1,1)==testN5(1,1,1,1,1),msg)

      CALL h5%fwrite('groupI->memN6',newN6,SHAPE(newN6))
      CALL h5%fread ('groupI->memN6',testN6)
      ASSERT(newN6(1,1,1,1,1,1)==testN6(1,1,1,1,1,1),msg)

      CALL h5%fwrite('groupI->memN7',newN7,SHAPE(newN7))
      CALL h5%fread ('groupI->memN7',testN7)
      ASSERT(newN7(1,1,1,1,1,1,1)==testN7(1,1,1,1,1,1,1),msg)

      CALL h5%mkdir('groupB')
      CALL h5%fwrite('groupB->memB0',newB0)
      CALL h5%fread ('groupB->memB0',testB0)
      ASSERT(testB0.EQV.newB0,msg)

      CALL h5%fwrite('groupB->memB1',newB1,SHAPE(newB1))
      CALL h5%fread ('groupB->memB1',testB1)
      ASSERT(testB1(1).EQV.newB1(1),msg)

      CALL h5%fwrite('groupB->memB2',newB2,SHAPE(newB2))
      CALL h5%fread ('groupB->memB2',testB2)
      ASSERT(testB2(1,1).EQV.newB2(1,1),msg)

      CALL h5%fwrite('groupB->memB3',newB3,SHAPE(newB3))
      CALL h5%fread ('groupB->memB3',testB3)
      ASSERT(testB3(1,1,1).EQV.newB3(1,1,1),msg)

      CALL h5%mkdir('groupST')
      CALL h5%fwrite('groupST->memST0',newST0)
      CALL h5%fread ('groupST->memST0',testST0)
      ASSERT_EQ(CHAR(newST0),CHAR(testST0),msg)

      CALL h5%fwrite('groupST->memST1',newST1,SHAPE(newST1))
      CALL h5%fread ('groupST->memST1',testST1)
      ASSERT(ALL(newST1==testST1),msg)

      CALL h5%fwrite('groupST->memST2',newST2,SHAPE(newST2))
      CALL h5%fread ('groupST->memST2',testST2)
      ASSERT(ALL(newST2==testST2),msg)

      CALL h5%fwrite('groupST->memST3',newST3,SHAPE(newST3))
      CALL h5%fread ('groupST->memST3',testST3)
      ASSERT(ALL(newST3==testST3),msg)

      !CALL h5%mkdir('groupC')
      CALL h5%fwrite('groupC->memC1',newC1,LEN(newC1))
      CALL h5%fread ('groupC->memC1',testC1)
      ASSERT_EQ(testC1,newC1,msg)


      CALL h5%fclose()
      CALL h5%clear()


      ! The file was previously status 'NEW' and the setOverwrite method
      ! call was made to make the file overwritable.
      ! This time, open the existing file with status 'OVERWRITE'.
      ! Overwrite the data again with different data and make sure it
      ! changed by reading it back in and comparing.
      COMPONENT_TEST('%Test file opened as OVERWRITE works')
      CALL h5%init('writetest.h5','OVERWRITE')
      CALL h5%fopen()

      newD0=refD0
      newD1=refD1
      newD2=refD2
      newD3=refD3
      newD4=refD4
      newD5=refD5
      newD6=refD6
      newD7=refD7
      newS0=refS0
      newS1=refS1
      newS2=refS2
      newS3=refS3
      newS4=refS4
      newS5=refS5
      newS6=refS6
      newS7=refS7
      newL1=arrVal+10_SLK
      newL2=arrVal+10_SLK
      newL3=arrVal+10_SLK
      newL4=arrVal+10_SLK
      newL5=arrVal+10_SLK
      newL6=arrVal+10_SLK
      newL7=arrVal+10_SLK
      newN0=refN0
      newN1=refN1
      newN2=refN2
      newN3=refN3
      newN4=refN4
      newN5=refN5
      newN6=refN6
      newN7=refN7
      newB0=refB0
      newB1=refB1
      newB2=refB2
      newB3=refB3
      newST0='ref val'
      DO i=1,SIZE(newST1)
        newST1(i)=newST0
      ENDDO
      DO i=1,SIZE(newST2,1)
        DO j=1,SIZE(newST2,2)
          newST2(i,j)=newST0
        ENDDO
      ENDDO
      DO i=1,SIZE(newST3,1)
        DO j=1,SIZE(newST3,2)
          DO k=1,SIZE(newST3,3)
            newST3(i,j,k)=newST0
          ENDDO
        ENDDO
      ENDDO
      newC1=refC1

      CALL h5%mkdir('groupR')
      CALL h5%fwrite('groupR->memD0',newD0)
      CALL h5%fread ('groupR->memD0',testD0)
      ASSERT_SOFTEQ(newD0,testD0,rtol,msg)

      CALL h5%fwrite('groupR->memD1',newD1,SHAPE(newD1))
      CALL h5%fread ('groupR->memD1',testD1)
      ASSERT_SOFTEQ(newD1(1),testD1(1),rtol,msg)

      CALL h5%fwrite('groupR->memD2',newD2,SHAPE(newD2))
      CALL h5%fread ('groupR->memD2',testD2)
      ASSERT_SOFTEQ(newD2(1,1),testD2(1,1),rtol,msg)

      CALL h5%fwrite('groupR->memD3',newD3,SHAPE(newD3))
      CALL h5%fread ('groupR->memD3',testD3)
      ASSERT_SOFTEQ(newD3(1,1,1),testD3(1,1,1),rtol,msg)

      CALL h5%fwrite('groupR->memD4',newD4,SHAPE(newD4))
      CALL h5%fread ('groupR->memD4',testD4)
      ASSERT_SOFTEQ(newD4(1,1,1,1),testD4(1,1,1,1),rtol,msg)

      CALL h5%fwrite('groupR->memD5',newD5,SHAPE(newD5))
      CALL h5%fread ('groupR->memD5',testD5)
      ASSERT_SOFTEQ(newD5(1,1,1,1,1),testD5(1,1,1,1,1),rtol,msg)

      CALL h5%fwrite('groupR->memD6',newD6,SHAPE(newD6))
      CALL h5%fread ('groupR->memD6',testD6)
      ASSERT_SOFTEQ(newD6(1,1,1,1,1,1),testD6(1,1,1,1,1,1),rtol,msg)

      CALL h5%fwrite('groupR->memD7',newD7,SHAPE(newD7))
      CALL h5%fread ('groupR->memD7',testD7)
      ASSERT_SOFTEQ(newD7(1,1,1,1,1,1,1),testD7(1,1,1,1,1,1,1),rtol,msg)

      CALL h5%fwrite('groupR->memS0',newS0)
      CALL h5%fread ('groupR->memS0',testS0)
      ASSERT_SOFTEQ(newS0,testS0,rtols,msg)

      CALL h5%fwrite('groupR->memS1',newS1,SHAPE(newS1))
      CALL h5%fread ('groupR->memS1',testS1)
      ASSERT_SOFTEQ(newS1(1),testS1(1),rtols,msg)

      CALL h5%fwrite('groupR->memS2',newS2,SHAPE(newS2))
      CALL h5%fread ('groupR->memS2',testS2)
      ASSERT_SOFTEQ(newS2(1,1),testS2(1,1),rtols,msg)

      CALL h5%fwrite('groupR->memS3',newS3,SHAPE(newS3))
      CALL h5%fread ('groupR->memS3',testS3)
      ASSERT_SOFTEQ(newS3(1,1,1),testS3(1,1,1),rtols,msg)

      CALL h5%fwrite('groupR->memS4',newS4,SHAPE(newS4))
      CALL h5%fread ('groupR->memS4',testS4)
      ASSERT_SOFTEQ(newS4(1,1,1,1),testS4(1,1,1,1),rtols,msg)

      CALL h5%fwrite('groupR->memS5',newS5,SHAPE(newS5))
      CALL h5%fread ('groupR->memS5',testS5)
      ASSERT_SOFTEQ(newS5(1,1,1,1,1),testS5(1,1,1,1,1),rtols,msg)

      CALL h5%fwrite('groupR->memS6',newS6,SHAPE(newS6))
      CALL h5%fread ('groupR->memS6',testS6)
      ASSERT_SOFTEQ(newS6(1,1,1,1,1,1),testS6(1,1,1,1,1,1),rtols,msg)

      CALL h5%fwrite('groupR->memS7',newS7,SHAPE(newS7))
      CALL h5%fread ('groupR->memS7',testS7)
      ASSERT_SOFTEQ(newS7(1,1,1,1,1,1,1),testS7(1,1,1,1,1,1,1),rtols,msg)

      CALL h5%mkdir('groupI')
      CALL h5%fwrite('groupI->memL0',newL0)
      CALL h5%fread ('groupI->memL0',testL0)
      ASSERT_EQ(newL0,testL0,msg)

      CALL h5%fwrite('groupI->memL1',newL1,SHAPE(newL1))
      CALL h5%fread ('groupI->memL1',testL1)
      ASSERT(ALL(newL1==testL1),msg)

      CALL h5%fwrite('groupI->memL2',newL2,SHAPE(newL2))
      CALL h5%fread ('groupI->memL2',testL2)
      ASSERT(ALL(newL2==testL2),msg)

      CALL h5%fwrite('groupI->memL3',newL3,SHAPE(newL3))
      CALL h5%fread ('groupI->memL3',testL3)
      ASSERT(ALL(newL3==testL3),msg)

      CALL h5%fwrite('groupI->memL4',newL4,SHAPE(newL4))
      CALL h5%fread ('groupI->memL4',testL4)
      ASSERT(ALL(newL4==testL4),msg)

      CALL h5%fwrite('groupI->memL5',newL5,SHAPE(newL5))
      CALL h5%fread ('groupI->memL5',testL5)
      ASSERT(ALL(newL5==testL5),msg)

      CALL h5%fwrite('groupI->memL6',newL6,SHAPE(newL6))
      CALL h5%fread ('groupI->memL6',testL6)
      ASSERT(ALL(newL6==testL6),msg)

      CALL h5%fwrite('groupI->memL7',newL7,SHAPE(newL7))
      CALL h5%fread ('groupI->memL7',testL7)
      ASSERT(ALL(newL7==testL7),msg)

      CALL h5%fwrite('groupI->memN0',newN0)
      CALL h5%fread ('groupI->memN0',testN0)
      ASSERT_EQ(newN0,testN0,msg)

      CALL h5%fwrite('groupI->memN1',newN1,SHAPE(newN1))
      CALL h5%fread ('groupI->memN1',testN1)
      ASSERT(newN1(1)==testN1(1),msg)

      CALL h5%fwrite('groupI->memN2',newN2,SHAPE(newN2))
      CALL h5%fread ('groupI->memN2',testN2)
      ASSERT(newN2(1,1)==testN2(1,1),msg)

      CALL h5%fwrite('groupI->memN3',newN3,SHAPE(newN3))
      CALL h5%fread ('groupI->memN3',testN3)
      ASSERT(newN3(1,1,1)==testN3(1,1,1),msg)

      CALL h5%fwrite('groupI->memN4',newN4,SHAPE(newN4))
      CALL h5%fread ('groupI->memN4',testN4)
      ASSERT(newN4(1,1,1,1)==testN4(1,1,1,1),msg)

      CALL h5%fwrite('groupI->memN5',newN5,SHAPE(newN5))
      CALL h5%fread ('groupI->memN5',testN5)
      ASSERT(newN5(1,1,1,1,1)==testN5(1,1,1,1,1),msg)

      CALL h5%fwrite('groupI->memN6',newN6,SHAPE(newN6))
      CALL h5%fread ('groupI->memN6',testN6)
      ASSERT(newN6(1,1,1,1,1,1)==testN6(1,1,1,1,1,1),msg)

      CALL h5%fwrite('groupI->memN7',newN7,SHAPE(newN7))
      CALL h5%fread ('groupI->memN7',testN7)
      ASSERT(newN7(1,1,1,1,1,1,1)==testN7(1,1,1,1,1,1,1),msg)

      CALL h5%mkdir('groupB')
      CALL h5%fwrite('groupB->memB0',newB0)
      CALL h5%fread ('groupB->memB0',testB0)
      ASSERT(testB0.EQV.newB0,msg)

      CALL h5%fwrite('groupB->memB1',newB1,SHAPE(newB1))
      CALL h5%fread ('groupB->memB1',testB1)
      ASSERT(testB1(1).EQV.newB1(1),msg)

      CALL h5%fwrite('groupB->memB2',newB2,SHAPE(newB2))
      CALL h5%fread ('groupB->memB2',testB2)
      ASSERT(testB2(1,1).EQV.newB2(1,1),msg)

      CALL h5%fwrite('groupB->memB3',newB3,SHAPE(newB3))
      CALL h5%fread ('groupB->memB3',testB3)
      ASSERT(testB3(1,1,1).EQV.newB3(1,1,1),msg)

      CALL h5%mkdir('groupST')
      CALL h5%fwrite('groupST->memST0',newST0)
      CALL h5%fread ('groupST->memST0',testST0)
      ASSERT_EQ(CHAR(newST0),CHAR(testST0),msg)

      CALL h5%fwrite('groupST->memST1',newST1,SHAPE(newST1))
      CALL h5%fread ('groupST->memST1',testST1)
      ASSERT_EQ(CHAR(newST1(1)),CHAR(testST1(1)),msg)

      CALL h5%fwrite('groupST->memST2',newST2,SHAPE(newST2))
      CALL h5%fread ('groupST->memST2',testST2)
      ASSERT(ALL(newST2==testST2),msg)

      CALL h5%fwrite('groupST->memST3',newST3,SHAPE(newST3))
      CALL h5%fread ('groupST->memST3',testST3)
      ASSERT(ALL(newST3==testST3),msg)

      !CALL h5%mkdir('groupC')
      CALL h5%fwrite('groupC->memC1',newC1,LEN(newC1))
      CALL h5%fread ('groupC->memC1',testC1)
      ASSERT_EQ(testC1,newC1,msg)


      ! Set the overwrite status back to .FALSE. and try to overwrite
      ! again.  Make sure the correct exception is thrown for each attempt.
      COMPONENT_TEST('%Test no overwrite')
      CALL h5%setOverwriteStat(.FALSE.)

      CALL h5%e%setStopOnError(.FALSE.)
      CALL h5%e%setQuietMode(.TRUE.)

      ! Quiet HDF5 down
      CALL HDF5Quiet(.TRUE.)

      CALL h5%mkdir('groupR')
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_DEBUG_MESG #### - FileType_HDF5::mkdir_HDF5FileType '// &
        '- Failed to create HDF5 group.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memD0',refD0)
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memD1',refD1,SHAPE(refD1))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memD2',refD2,SHAPE(refD2))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memD3',refD3,SHAPE(refD3))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memD4',refD4,SHAPE(refD4))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memD5',refD5,SHAPE(refD5))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memD6',refD6,SHAPE(refD6))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memD7',refD7,SHAPE(refD7))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memS0',refS0)
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memS1',refS1,SHAPE(refS1))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memS2',refS2,SHAPE(refS2))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memS3',refS3,SHAPE(refS3))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memS4',refS4,SHAPE(refS4))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memS5',refS5,SHAPE(refS5))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memS6',refS6,SHAPE(refS6))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupR->memS7',refS7,SHAPE(refS7))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%mkdir('groupI')
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_DEBUG_MESG #### - FileType_HDF5::mkdir_HDF5FileType '// &
        '- Failed to create HDF5 group.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memL0',refL0)
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memL1',refL1,SHAPE(refL1))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memL2',refL2,SHAPE(refL2))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memL3',refL3,SHAPE(refL3))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memL4',refL4,SHAPE(refL4))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memL5',refL5,SHAPE(refL5))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memL6',refL6,SHAPE(refL6))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memL7',refL7,SHAPE(refL7))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memN0',refN0)
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memN1',refN1,SHAPE(refN1))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memN2',refN2,SHAPE(refN2))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memN3',refN3,SHAPE(refN3))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memN4',refN4,SHAPE(refN4))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memN5',refN5,SHAPE(refN5))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memN6',refN6,SHAPE(refN6))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupI->memN7',refN7,SHAPE(refN7))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%mkdir('groupB')
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_DEBUG_MESG #### - FileType_HDF5::mkdir_HDF5FileType '// &
        '- Failed to create HDF5 group.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupB->memB0',refB0)
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupB->memB1',refB1,SHAPE(refB1))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupB->memB2',refB2,SHAPE(refB2))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupB->memB3',refB3,SHAPE(refB3))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%mkdir('groupCNCHAR')
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_DEBUG_MESG #### - FileType_HDF5::mkdir_HDF5FileType '// &
        '- Failed to create HDF5 group.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupCNCHAR->CNCHAR0',refCNCHAR0)
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupCNCHAR->CNCHAR1',refCNCHAR1,SHAPE(refCNCHAR1))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupCNCHAR->CNCHAR2',refCNCHAR2,SHAPE(refCNCHAR2))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupCNCHAR->CNCHAR3',refCNCHAR3,SHAPE(refCNCHAR3))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%mkdir('groupST')
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_DEBUG_MESG #### - FileType_HDF5::mkdir_HDF5FileType '// &
        '- Failed to create HDF5 group.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupST->memST0',refST0)
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupST->memST1',refST1,SHAPE(refST1))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupST->memST2',refST2,SHAPE(refST2))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fwrite('groupST->memST3',refST3,SHAPE(refST3))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%mkdir('groupC')
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_DEBUG_MESG #### - FileType_HDF5::mkdir_HDF5FileType '// &
        '- Failed to create HDF5 group.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      ! Turn exceptions back on for last one to make sure this function works
      call HDF5Quiet(.FALSE.)

      CALL h5%fwrite('groupC->memC1',refC1,LEN(refC1))
      msg=h5%e%getLastMessage()
      refmsg='#### EXCEPTION_ERROR #### - FileType_HDF5::postWrite '// &
        '- Could not close the dataset.'
      ASSERT_EQ(TRIM(msg),TRIM(refmsg),'Failure to prevent overwrite.')
      CALL h5%e%initCounter()

      CALL h5%fclose()
      CALL h5%clear(.TRUE.)


    END SUBROUTINE
!
!-------------------------------------------------------------------------------
    SUBROUTINE testCompress()
      TYPE(HDF5FileType) :: h5
      INTEGER(SIK),PARAMETER :: largeN=16777216 !128MB
      REAL(SDK),ALLOCATABLE :: largeArray(:)
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
      TYPE(TimerType) :: t

      COMPONENT_TEST('%fwrite with gdims')
      ! Create a RW access file. Existing file overwritten
      CALL h5%init('writetest_compress.h5','NEW',5)
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
      ASSERT_EQ(testD0,refD0,'D0 Write Failure with gdims_in')
      CALL h5%fread('groupR->memD1',testD1)
      ASSERT(ALL(testD1==refD1),'D1 Write Failure with gdims_in')
      CALL h5%fread('groupR->memD2',testD2)
      ASSERT(ALL(testD2==refD2),'D2 Write Failure with gdims_in')
      CALL h5%fread('groupR->memD3',testD3)
      ASSERT(ALL(testD3==refD3),'D3 Write Failure with gdims_in')
      CALL h5%fread('groupR->memD4',testD4)
      ASSERT(ALL(testD4==refD4),'D4 Write Failure with gdims_in')
      CALL h5%fread('groupR->memS0',testS0)
      ASSERT_EQ(testS0,refS0,'S0 Write Failure with gdims_in')
      CALL h5%fread('groupR->memS1',testS1)
      ASSERT(ALL(testS1==refS1),'S1 Write Failure with gdims_in')
      CALL h5%fread('groupR->memS2',testS2)
      ASSERT(ALL(testS2==refS2),'S2 Write Failure with gdims_in')
      CALL h5%fread('groupR->memS3',testS3)
      ASSERT(ALL(testS3==refS3),'S3 Write Failure with gdims_in')
      CALL h5%fread('groupR->memS4',testS4)
      ASSERT(ALL(testS4==refS4),'S4 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN0',testN0)
      ASSERT_EQ(testN0,refN0,'N0 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN1',testN1)
      ASSERT(ALL(testN1==refN1),'N1 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN2',testN2)
      ASSERT(ALL(testN2==refN2),'N2 Write Failure with gdims_in')
      CALL h5%fread('groupI->memN3',testN3)
      ASSERT(ALL(testN3==refN3),'N3 Write Failure with gdims_in')
      CALL h5%fread('groupI->memL0',testL0)
      ASSERT_EQ(testL0,refL0,'L0 Write Failure with gdims_in')
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
      ASSERT_EQ(CHAR(testST0),CHAR(refST0),'ST0 Write Failure with gdims_in')
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
      ASSERT_EQ(TRIM(testC1),TRIM(refC1),'C1 Write Failure with gdims_in')

      i=h5%ngrp('groupR')
      ASSERT_EQ(i,10,'ngrp_HDF5FileType')

      ! Delete the file
      CALL h5%clear(.TRUE.)

      COMPONENT_TEST('%fwrite without gdims')
      CALL h5%init('writetest_compress.h5','NEW',5)
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
      ASSERT_EQ(testD0,refD0,'D0 Write Failure')
      CALL h5%fread('groupR->memD1',testD1)
      ASSERT(ALL(testD1==refD1),'D1 Write Failure')
      CALL h5%fread('groupR->memD2',testD2)
      ASSERT(ALL(testD2==refD2),'D2 Write Failure')
      CALL h5%fread('groupR->memD3',testD3)
      ASSERT(ALL(testD3==refD3),'D3 Write Failure')
      CALL h5%fread('groupR->memD4',testD4)
      ASSERT(ALL(testD4==refD4),'D4 Write Failure')
      CALL h5%fread('groupR->memS0',testS0)
      ASSERT_EQ(testS0,refS0,'S0 Write Failure')
      CALL h5%fread('groupR->memS1',testS1)
      ASSERT(ALL(testS1==refS1),'S1 Write Failure')
      CALL h5%fread('groupR->memS2',testS2)
      ASSERT(ALL(testS2==refS2),'S2 Write Failure')
      CALL h5%fread('groupR->memS3',testS3)
      ASSERT(ALL(testS3==refS3),'S3 Write Failure')
      CALL h5%fread('groupR->memS4',testS4)
      ASSERT(ALL(testS4==refS4),'S4 Write Failure')
      CALL h5%fread('groupI->memN0',testN0)
      ASSERT_EQ(testN0,refN0,'N0 Write Failure')
      CALL h5%fread('groupI->memN1',testN1)
      ASSERT(ALL(testN1==refN1),'N1 Write Failure')
      CALL h5%fread('groupI->memN2',testN2)
      ASSERT(ALL(testN2==refN2),'N2 Write Failure')
      CALL h5%fread('groupI->memN3',testN3)
      ASSERT(ALL(testN3==refN3),'N3 Write Failure')
      CALL h5%fread('groupI->memL0',testL0)
      ASSERT_EQ(testL0,refL0,'L0 Write Failure')
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
      ASSERT_EQ(CHAR(testST0),CHAR(refST0),'ST0 Write Failure')
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
      ASSERT_EQ(testC1,refC1,'C1 Write Failure')
      i=h5%ngrp('groupR')
      ASSERT_EQ(i,10,'ngrp_HDF5FileType')

      !Clear variables
      CALL h5%clear(.TRUE.)


      COMPONENT_TEST('Large Array')
      CALL h5%init('largeData.h5','NEW',5)
      CALL h5%fopen()
      CALL h5%mkdir('large')
      CALL TAUSTUB_CHECK_MEMORY()
      ALLOCATE(largeArray(largeN))
      DO i=1,largeN
        largeArray(i)=REAL(i,SDK)*0.0001_SDK
      ENDDO
      CALL TAUSTUB_CHECK_MEMORY()
      CALL t%tic()
      CALL h5%fwrite('large->array',largeArray)
      CALL t%toc()
      WRITE(*,*) "Time to write 128 MB with 4 MB chunks="//t%getTimeHHMMSS()
      CALL TAUSTUB_CHECK_MEMORY()
      ASSERT(h5%pathExists('large->array'),'largeArray')

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
        ASSERT_EQ(TRIM(refsets(i)),TRIM(testST1(i)),refsets(i)//' List Failure')
      ENDDO

      COMPONENT_TEST('%fread values')
      ! Read a dataset (real-1)
      CALL h5%fread('groupR->memD0',testD0)
      ASSERT_EQ(testD0,refD0,'D0 Read Failure')
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
      ASSERT_EQ(testS0,refS0,'S0 Read Failure')
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
      ASSERT_EQ(testN0,refN0,'N0 Read Failure')
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
      ASSERT_EQ(testL0,refL0,'L0 Read Failure')
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
      ASSERT_EQ(CHAR(testST0),CHAR(refST0),'ST0 Read Failure')
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
      ASSERT_EQ(CHAR(testST0),CHAR(refST0),'CA0 Read Failure')
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
      ASSERT_EQ(testC1,refC1,'C1 Read Failure')

      !Read test attributes
      COMPONENT_TEST('%read_attributes')
      !READ ATTRIBUTES
      CALL h5%read_attribute('groupB->memB0',integer_name,testN0)
      ASSERT_EQ(refN0,testN0,'integer read fail')
      CALL h5%read_attribute('groupB->memB1',string_name,testST0)
      ASSERT_EQ(CHAR(refST0),CHAR(testST0),'string read fail')
      CALL h5%read_attribute('groupB->memB1',char_name,testC1)
      ASSERT_EQ(refC1,testC1,'string read fail')
      CALL h5%read_attribute('groupB->memB2',real_name,testD0)
      ASSERT_EQ(refD0,testD0,'real_read fail')

      COMPONENT_TEST('%fread to parameter list')
      CALL h5%fread('groupC',tmpPL)
      !Character arrays
      CALL tmpPL%get('groupC->anotherGroup->memC1',testST1)
      DO i=1,SIZE(testST1)
        ASSERTFAIL(SIZE(testST1) == SIZE(refSTC1),'SIZE(')
        ASSERT_EQ(CHAR(testST1(i)),CHAR(refSTC1(i)),'C1 Read PL Failure')
      ENDDO
      CALL tmpPL%get('groupC->anotherGroup->moreGroups->almostLastGroup->memC2',testST1)
      DO i=1,SIZE(testST1)
        ASSERTFAIL(SIZE(testST1) == SIZE(refSTC1),'SIZE(')
        ASSERT_EQ(CHAR(testST1(i)),CHAR(refSTC1(i)),'C1 Read PL Failure')
      ENDDO
      CALL tmpPL%get('groupC->memC1',testST1)
      DO i=1,SIZE(testST1)
        ASSERTFAIL(SIZE(testST1) == SIZE(refSTC1),'SIZE(')
        ASSERT_EQ(CHAR(testST1(i)),CHAR(refSTC1(i)),'C1 Read PL Failure')
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
      ASSERT_EQ(testL0,refL0,'L0 read PL Failure')
      CALL tmpPL%get('groupI->memL1',testL1)
      ASSERT(ALL(testL1 == refL1),'L1 read PL Failure')
      CALL tmpPL%get('groupI->memL2',testL2)
      ASSERT(ALL(testL2 == refL2),'L2 read PL Failure')
      CALL tmpPL%get('groupI->memL3',testL3)
      ASSERT(ALL(testL3 == refL3),'L3 read PL Failure')
      CALL tmpPL%get('groupI->memN0',testN0)
      ASSERT_EQ(testN0,refN0,'N0 read PL Failure')
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
      ASSERT_EQ(testD0,refD0,'D0 read PL Failure')
      CALL tmpPL%get('groupR->memD1',testD1)
      ASSERT(ALL(testD1 == refD1),'D1 read PL Failure')
      CALL tmpPL%get('groupR->memD2',testD2)
      ASSERT(ALL(testD2 == refD2),'D2 read PL Failure')
      CALL tmpPL%get('groupR->memD3',testD3)
      ASSERT(ALL(testD3 == refD3),'D3 read PL Failure')
      CALL tmpPL%get('groupR->memS0',testS0)
      ASSERT_EQ(testS0,refS0,'S0 read PL Failure')
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
      ASSERT_EQ(CHAR(testST0),CHAR(refST0),'ST0 read PL Failure')
      CALL tmpPL%get('groupST->memST1',testST1)
      ASSERTFAIL(SIZE(testST1) == SIZE(refST1),'ST1 Sizes')
      DO i=1,SIZE(testST1)
        ASSERT_EQ(CHAR(testST1(i)),CHAR(refST1(i)),'ST1 read PL Failure')
      ENDDO
      CALL tmpPL%get('groupST->memST2',testST2)
      testB0=(SIZE(testST2,DIM=1) == SIZE(refST2,DIM=1)) .AND. &
        (SIZE(testST2,DIM=2) == SIZE(refST2,DIM=2))
      ASSERTFAIL(testB0,'ST2 Sizes')
      DO j=1,SIZE(testST2,DIM=2)
        DO i=1,SIZE(testST2,DIM=1)
          ASSERT_EQ(CHAR(testST2(i,j)),CHAR(refST2(i,j)),'ST2 read PL Failure')
         ENDDO
       ENDDO
      !Character array, read in as a 1-D string array
      CALL tmpPL%get('groupST->memCA0',testST1)
      ASSERTFAIL(SIZE(testST1) == SIZE(refST0CA),'ST1 Sizes')
      DO i=1,SIZE(testST1)
        ASSERT_EQ(CHAR(testST1(i)),CHAR(refST0CA(i)),'ST1 read PL Failure')
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
    SUBROUTINE testIsCompressed()
      TYPE(HDF5FileType) :: h5
      INTEGER(SLK),ALLOCATABLE :: tmpA(:)
      ALLOCATE(tmpA(2097152))
      tmpA=-1

      COMPONENT_TEST('Uncompressed')
      CALL h5%init('tmpIsCompressed.h5','NEW',-1)
      CALL h5%fopen()
      CALL h5%mkdir('groupR')
      CALL h5%fwrite('groupR->memD1',refD1)
      CALL h5%fwrite('groupR->memD2',refD2)
      CALL h5%fwrite('groupR->memD3',refD3)
      CALL h5%fwrite('groupR->memD4',refD4)

      ASSERT(.NOT.h5%isCompressed(),'file query')
      ASSERT(.NOT.h5%isCompressed('groupR->memD2'),'groupR->memD2')

      CALL h5%clear(.TRUE.)

      COMPONENT_TEST('Compressed')
      CALL h5%init('tmpIsCompressed.h5','NEW',5)
      CALL h5%fopen()
      CALL h5%mkdir('groupR')
      CALL h5%fwrite('groupR->memD1',refD1)
      CALL h5%fwrite('groupR->tmpA',tmpA)

      ASSERT(h5%isCompressed(),'file query')
      ASSERT(.NOT.h5%isCompressed('groupR->memD1'),'groupR->memD1')
      ASSERT(h5%isCompressed('groupR->tmpA'),'groupR->tmpA')


      DEALLOCATE(tmpA)
      CALL h5%clear(.TRUE.)
    ENDSUBROUTINE testIsCompressed
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetChunkSize()
      TYPE(HDF5FileType) :: h5
      INTEGER(SLK),ALLOCATABLE :: cdims(:),tmpA(:),tmpA2(:,:)

      ALLOCATE(tmpA(2097152))
      tmpA=-1
      ALLOCATE(tmpA2(2,1048576))
      tmpA2=-1

      COMPONENT_TEST('Non-zero chunks')
      CALL h5%init('tmpGetChunkSize.h5','NEW',5)
      CALL h5%fopen()
      CALL h5%mkdir('groupR')
      CALL h5%fwrite('groupR->tmpA',tmpA)
      CALL h5%fwrite('groupR->tmpA2',tmpA2)
      CALL h5%getChunkSize('groupR->tmpA',cdims)
      ASSERT_EQ(cdims(1),131072,'memD1')
      CALL h5%getChunkSize('groupR->tmpA2',cdims)
      ASSERT(ALL(cdims == (/2,131072/)),'memD2')

      CALL h5%clear(.TRUE.)

      COMPONENT_TEST('No chunking')
      CALL h5%init('tmpGetChunkSize.h5','NEW',-1)
      CALL h5%fopen()
      CALL h5%mkdir('groupR')
      CALL h5%fwrite('groupR->memD1',refD1)
      CALL h5%fwrite('groupR->memD2',refD2)
      CALL h5%fwrite('groupR->memD3',refD3)
      CALL h5%fwrite('groupR->memD4',refD4)

      CALL h5%getChunkSize('groupR->memD1',cdims)
      ASSERT(.NOT.ALLOCATED(cdims),'memD1')
      CALL h5%getChunkSize('groupR->memD2',cdims)
      ASSERT(.NOT.ALLOCATED(cdims),'memD2')
      CALL  h5%getChunkSize('groupR->memS2',cdims)
      ASSERT(.NOT.ALLOCATED(cdims),'memS2')

      DEALLOCATE(tmpA)
      DEALLOCATE(tmpA2)
      CALL h5%clear(.TRUE.)
    ENDSUBROUTINE testGetChunkSize
!
ENDPROGRAM testHDF5
