!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testParallelEnv
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
!$ USE OMP_LIB
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE ParallelEnv
  USE Strings

  IMPLICIT NONE

#ifdef HAVE_MPI
  INCLUDE 'mpif.h'
  INTEGER :: mpierr, tmp
#endif

  TYPE(ParallelEnvType) :: testPE,testPE2

  INTEGER :: myrank,mysize,stt,stp

#ifdef HAVE_MPI
  CALL MPI_Init(mpierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD,myrank,mpierr)
  CALL MPI_Comm_size(MPI_COMM_WORLD,mysize,mpierr)
  tmp=0
#else
  myrank=0
  mysize=1
#endif
  CALL eParEnv%setQuietMode(.TRUE.)
  CALL eParEnv%setStopOnError(.FALSE.)

  CREATE_TEST('PARALLEL ENVIRONMENT')

  REGISTER_SUBTEST('PARAMETERS',testParams)
  REGISTER_SUBTEST('OMP_ENVTYPE',testOMPEnv)
  REGISTER_SUBTEST('MPI_ENVTYPE',testMPIEnv)
  REGISTER_SUBTEST('PARALLEL_ENVTYPE',testPE_Env)

  FINALIZE_TEST()
  CALL testPE%world%finalize()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testParams()
      INFO(0) 'PE_COMM_WORLD=',PE_COMM_WORLD
      INFO(0) 'PE_COMM_SELF=',PE_COMM_SELF
      INFO(0) 'PE_COMM_NULL=',PE_COMM_NULL
      INFO(0) 'PE_COMM_DEFAULT=',PE_COMM_DEFAULT
#ifdef HAVE_MPI
      ASSERT(PE_COMM_WORLD == MPI_COMM_WORLD,'PE_COMM_WORLD')
      ASSERT(PE_COMM_SELF == MPI_COMM_SELF,'PE_COMM_SELF')
      ASSERT(PE_COMM_NULL == MPI_COMM_NULL,'PE_COMM_NULL')
      ASSERT(PE_COMM_DEFAULT == PE_COMM_WORLD,'PE_COMM_DEFAULT')
#endif
    ENDSUBROUTINE testParams
!
!-------------------------------------------------------------------------------
    SUBROUTINE testOMPEnv()
      TYPE(OMP_EnvType) :: testOMP
!$    INTEGER :: n_warn

      COMPONENT_TEST('Uninit.')
      ASSERT(testOMP%nproc == -1,'%nproc')
      ASSERT(testOMP%rank == -1,'%rank')
      ASSERT(.NOT.testOMP%master,'%master')
      ASSERT(.NOT.testOMP%isInit(),'%isInit()')

      COMPONENT_TEST('%init()')
      CALL testOMP%init()
      ASSERT(testOMP%nproc == 1,'%nproc')
      ASSERT(testOMP%rank == 0,'%rank')
      ASSERT(testOMP%master,'%master')
      ASSERT(testOMP%isInit(),'%isInit()')

      COMPONENT_TEST('%clear()')
      CALL testOMP%clear()
      ASSERT(testOMP%nproc == -1,'%nproc')
      ASSERT(testOMP%rank == -1,'%rank')
      ASSERT(.NOT.testOMP%master,'%master')
      ASSERT(.NOT.testOMP%isInit(),'%isInit()')

!$    COMPONENT_TEST('With OpenMP')
!$    CALL testOMP%init(1)
!$    ASSERT(testOMP%nproc == 1,'%nproc')
!$    ASSERT(testOMP%rank == 0,'%rank')
!$    ASSERT(testOMP%master,'%master')
!$    ASSERT(testOMP%isInit(),'%isInit()')
!$    CALL testOMP%clear()
!$
!$    COMPONENT_TEST('Too many threads')
!$    n_warn = eParEnv%getCounter(EXCEPTION_WARNING)
!$    CALL testOMP%init(omp_get_num_procs()+1)
!$    ASSERT(eParEnv%getCounter(EXCEPTION_WARNING) > n_warn,'no warning')
!$    ASSERT(testOMP%rank == 0,'%rank')
!$    ASSERT(testOMP%master,'%master')
!$    ASSERT(testOMP%isInit(),'%isInit()')
!$    CALL testOMP%clear()
    ENDSUBROUTINE testOMPEnv
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMPIEnv()
      INTEGER(SIK) :: ip,jp,tag
      INTEGER(SLK) :: sbuf(2)
      INTEGER(SIK) :: sbuf_SIK(2)
      REAL(SRK) :: sbuf_SRK(2)
      INTEGER(SIK),ALLOCATABLE :: ranks_SIK(:),ranks2_SIK(:,:)
      INTEGER(SIK),ALLOCATABLE :: testIDX(:),testWGT(:)
      INTEGER(SLK),ALLOCATABLE :: ranks(:),ranks2(:,:)
      LOGICAL(SBK) :: bool,bool1d(10),bool2d(2,5),bool3d(2,5,2),bool4d(2,5,2,5)
      TYPE(MPI_EnvType) :: testMPI,testMPI2
      CHARACTER(LEN=8) :: tmpChar


      COMPONENT_TEST('%isInit()')
      ASSERT(testMPI%nproc == -1,'%nproc')
      ASSERT(testMPI%rank == -1,'%rank')
      ASSERT(.NOT.testMPI%master,'%master')
      ASSERT(.NOT.testMPI%isInit(),'%isInit()')

      COMPONENT_TEST('%init(PE_COMM_WORLD)')
      CALL testMPI%init(PE_COMM_WORLD)
      ASSERT(testMPI%comm /= PE_COMM_WORLD,'%comm world')
      ASSERT(testMPI%comm /= PE_COMM_SELF,'%comm self')
      ASSERT(testMPI%comm /= PE_COMM_NULL,'%comm null')
      ASSERT(testMPI%rank == myrank,'%rank')
      ASSERT(testMPI%nproc > 0,'%nproc')
      ASSERT(testMPI%isInit(),'%isInit()')

      COMPONENT_TEST('%init(PE_COMM_SELF)')
      CALL testMPI2%init(PE_COMM_SELF)
      ASSERT(testMPI2%comm /= PE_COMM_WORLD,'%comm world')
      ASSERT(testMPI2%comm /= PE_COMM_SELF,'%comm self')
      ASSERT(testMPI2%comm /= PE_COMM_NULL,'%comm null')
      ASSERT(testMPI2%rank == 0,'%rank')
      ASSERT(testMPI2%nproc == 1,'%nproc')
      ASSERT(testMPI2%master,'%master')
      ASSERT(testMPI2%isInit(),'%isInit()')

      COMPONENT_TEST('%clear()')
      CALL testMPI2%clear()
      ASSERT(testMPI2%nproc == -1,'%nproc')
      ASSERT(testMPI2%rank == -1,'%rank')
      ASSERT(.NOT.testMPI2%master,'%master')
      ASSERT(.NOT.testMPI2%isInit(),'%isInit()')

      COMPONENT_TEST('%barrier()')
      CALL testMPI%barrier()

      COMPONENT_TEST('%partition')
      CALL testMPI2%init(testMPI%comm)
      testMPI2%nproc=7
      testMPI2%rank=0
      CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
      ASSERT(stt == 2,'istt (0,7)')
      ASSERT(stp == 5,'istp (0,7)')
      testMPI2%rank=1
      CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
      ASSERT(stt == 6,'istt (1,7)')
      ASSERT(stp == 9,'istp (1,7)')
      testMPI2%rank=2
      CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
      ASSERT(stt == 10,'istt (2,7)')
      ASSERT(stp == 13,'istp (2,7)')
      testMPI2%rank=3
      CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
      ASSERT(stt == 14,'istt (3,7)')
      ASSERT(stp == 16,'istp (3,7)')
      testMPI2%rank=4
      CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
      ASSERT(stt == 17,'istt (4,7)')
      ASSERT(stp == 19,'istp (4,7)')
      testMPI2%rank=5
      CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
      ASSERT(stt == 20,'istt (5,7)')
      ASSERT(stp == 22,'istp (5,7)')
      testMPI2%rank=6
      CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
      ASSERT(stt == 23,'istt (6,7)')
      ASSERT(stp == 25,'istp (6,7)')

      testMPI2%nproc=10
      ALLOCATE(testWGT(40))
      testWGT=(/936,936,936,936,1722,1722,1722,1722,1722,1722,1722,1722,1916, &
                1916,1916,1916,1944,1944,1944,1944,1916,1916,1916,1916,1571, &
                1571,1571,1571,4122,4122,4122,4122,4266,4266,4266,4266,1850, &
                1850,1850,1850/)
      CALL testMPI2%partition(IWGT=testWGT,IPART=0,IDXMAP=testIDX)
      ASSERT(ALL(testIDX == (/11,23,33/)),'testIDX 0')
      CALL testMPI2%partition(IWGT=testWGT,IPART=1,IDXMAP=testIDX)
      ASSERT(ALL(testIDX == (/12,24,34/)),'testIDX 1')
      CALL testMPI2%partition(IWGT=testWGT,IPART=2,IDXMAP=testIDX)
      ASSERT(ALL(testIDX == (/3,9,35,37/)),'testIDX 2')
      CALL testMPI2%partition(IWGT=testWGT,IPART=3,IDXMAP=testIDX)
      ASSERT(ALL(testIDX == (/4,10,36,38/)),'testIDX 3')
      CALL testMPI2%partition(IWGT=testWGT,IPART=4,IDXMAP=testIDX)
      ASSERT(ALL(testIDX == (/5,15,27,29/)),'testIDX 4')
      CALL testMPI2%partition(IWGT=testWGT,IPART=5,IDXMAP=testIDX)
      ASSERT(ALL(testIDX == (/6,16,28,30/)),'testIDX 5')
      CALL testMPI2%partition(IWGT=testWGT,IDXMAP=testIDX)
      ASSERT(ALL(testIDX == (/1,7,21,31/)),'testIDX 6')
      CALL testMPI2%partition(IWGT=testWGT,IPART=7,IDXMAP=testIDX)
      ASSERT(ALL(testIDX == (/2,8,22,32/)),'testIDX 7')
      CALL testMPI2%partition(IWGT=testWGT,IPART=8,IDXMAP=testIDX)
      ASSERT(ALL(testIDX == (/13,17,19,25,39/)),'testIDX 8')
      CALL testMPI2%partition(IWGT=testWGT,N1=1,N2=40,IPART=9,IDXMAP=testIDX)
      ASSERT(ALL(testIDX == (/14,18,20,26,40/)),'testIDX 9')

      !Error Checking
      CALL testMPI2%partition(IWGT=testWGT,N1=1,N2=40,IPART=100,IDXMAP=testIDX)
      CALL testMPI2%partition(IWGT=testWGT,N1=41,N2=40,IDXMAP=testIDX)
      CALL testMPI2%clear()
      CALL testMPI2%partition(IWGT=testWGT,N1=1,N2=40,IDXMAP=testIDX)

      CALL testMPI%clear()

      COMPONENT_TEST('%gather')
      !Need to test error conditions and
      !cases where SIZE(recvbuf) > SIZE(sbuf)*nproc
      CALL testMPI%init(PE_COMM_WORLD)
      ALLOCATE(ranks(testMPI%nproc))
      ALLOCATE(ranks2(2,testMPI%nproc))
      ranks=-1
      ranks2=-1
      CALL testMPI%gather(INT(testMPI%rank,SLK),ranks)
      sbuf=(/testMPI%rank,-testMPI%rank/)
      CALL testMPI%gather(sbuf,ranks2)
      IF(testMPI%rank == 0) THEN
        DO ip=1,testMPI%nproc
          ASSERT(ranks(ip) == ip-1,'master ranks(ip)')
          FINFO() ip-1,ranks(ip)
          ASSERT(ALL(ranks2(:,ip) == (/ip-1,-ip+1/)),'master ranks2(ip)')
          FINFO() ip-1,ranks2(:,ip)
        ENDDO
      ELSE
        ASSERT(ALL(ranks == -1),'non-master ranks')
        ASSERT(ALL(ranks2 == -1),'non-master ranks2')
      ENDIF
      DO ip=1,testMPI%nproc-1
        ranks=-1
        ranks2=-1
        CALL testMPI%gather(INT(testMPI%rank,SLK),ranks,ip)
        CALL testMPI%gather(sbuf,ranks2,ip)
        IF(testMPI%rank == ip) THEN
          DO jp=1,testMPI%nproc
            ASSERT(ranks(jp) == jp-1,'master ranks(jp)')
            FINFO() ip,jp-1,ranks(jp)
            ASSERT(ALL(ranks2(:,jp) == (/jp-1,-jp+1/)),'master ranks2(jp)')
            FINFO() ip,jp-1,ranks2(:,jp)
          ENDDO
        ELSE
          ASSERT(ALL(ranks == -1),'non-ip ranks')
          ASSERT(ALL(ranks2 == -1),'non-ip ranks2')
        ENDIF
      ENDDO

      !Simple extension, testing gather on SIK
      ALLOCATE(ranks_SIK(testMPI%nproc))
      ALLOCATE(ranks2_SIK(2,testMPI%nproc))
      ranks_SIK=-1
      ranks2_SIK=-1
      CALL testMPI%gather(testMPI%rank,ranks_SIK)
      sbuf_SIK=(/testMPI%rank,-testMPI%rank/)
      CALL testMPI%gather(sbuf_SIK,ranks2_SIK)
      IF(testMPI%rank == 0) THEN
        DO ip=1,testMPI%nproc
          ASSERT(ranks_SIK(ip) == ip-1,'master ranks_SIK(ip)')
          FINFO() ip-1,ranks_SIK(ip)
          ASSERT(ALL(ranks2_SIK(:,ip) == (/ip-1,-ip+1/)),'master ranks2_SIK(ip)')
          FINFO() ip-1,ranks2_SIK(:,ip)
        ENDDO
      ELSE
        ASSERT(ALL(ranks_SIK == -1),'non-master ranks_SIK')
        ASSERT(ALL(ranks2_SIK == -1),'non-master ranks2_SIK')
      ENDIF
      DO ip=1,testMPI%nproc-1
        ranks_SIK=-1
        ranks2_SIK=-1
        CALL testMPI%gather(testMPI%rank,ranks_SIK,ip)
        CALL testMPI%gather(sbuf_SIK,ranks2_SIK,ip)
        IF(testMPI%rank == ip) THEN
          DO jp=1,testMPI%nproc
            ASSERT(ranks_SIK(jp) == jp-1,'master ranks_SIK(jp)')
            FINFO() ip,jp-1,ranks_SIK(jp)
            ASSERT(ALL(ranks2_SIK(:,jp) == (/jp-1,-jp+1/)),'master ranks2_SIK(jp)')
            FINFO() ip,jp-1,ranks2_SIK(:,jp)
          ENDDO
        ELSE
          ASSERT(ALL(ranks_SIK == -1),'non-ip ranks_SIK')
          ASSERT(ALL(ranks2_SIK == -1),'non-ip ranks2_SIK')
        ENDIF
      ENDDO

      COMPONENT_TEST('%send/%recv')
      sbuf_SIK = 0
      IF(testMPI%nproc > 1) THEN
        IF(testMPI%rank == 0) THEN
          !Send it out as the largest possible integers
          sbuf_SIK = (/HUGE(ip),-HUGE(ip)/)
          tag=1
          CALL testMPI%send(sbuf_SIK,size(sbuf_SIK),1,tag)
          !Get it back as zeros
          tag=2
          CALL testMPI%recv(sbuf_SIK,SIZE(sbuf_SIK),1,tag)
          ASSERT(ALL(sbuf_SIK == 0_SIK),'master recv')
          tag=3
          tmpChar ='testChar'
          CALL testMPI%send(tmpChar,1,tag)
          !Send it out as the largest possible real
          sbuf_SRK = (/HUGE(sbuf_SRK(1)),-HUGE(sbuf_SRK(1))/)
          tag=4
          CALL testMPI%send(sbuf_SRK,size(sbuf_SRK),1,tag)
          !Get it back as zeros
          tag=5
          CALL testMPI%recv(sbuf_SRK,SIZE(sbuf_SRK),1,tag)
          ASSERT(ALL(sbuf_SRK == 0_SIK),'master recv')
        ELSEIF(testMPI%rank ==1) THEN
          !Recieve as largest possible integers
          tag=1
          CALL testMPI%recv(sbuf_SIK,SIZE(sbuf_SIK),0,tag)
          ASSERT_EQ(sbuf_SIK(1),HUGE(ip),'subordinate recv positive')
          ASSERT_EQ(sbuf_SIK(2),-HUGE(ip),'subordinate recv negative')
          !Send as zero
          sbuf_SIK = 0
          tag=2
          CALL testMPI%send(sbuf_SIK,SIZE(sbuf_SIK),0,tag)
          tag=3
          CALL testMPI%recv(tmpChar,0,tag)
          ASSERT(tmpChar == 'testChar','CHARACTER check')
          !Recieve as largest possible integers
          tag=4
          CALL testMPI%recv(sbuf_SRK,SIZE(sbuf_SRK),0,tag)
          ASSERT_EQ(sbuf_SRK(1),HUGE(sbuf_SRK(1)),'subordinate recv positive')
          ASSERT_EQ(sbuf_SRK(2),-HUGE(sbuf_SRK(2)),'subordinate recv negative')
          !Send as zero
          sbuf_SRK = 0
          tag=5
          CALL testMPI%send(sbuf_SRK,SIZE(sbuf_SRK),0,tag)
        ENDIF
      ENDIF

      COMPONENT_TEST('%trueForAll')
      CALL testMPI%clear()
      CALL testMPI%init()
      bool=.TRUE.
      CALL testMPI%trueForAll(bool)
      ASSERT(bool,'trueForAll - True')
      bool=.FALSE.
      CALL testMPI%trueForAll(bool)
      ASSERT(.NOT.bool,'trueForAll - False')
      IF(testMPI%nproc > 1) THEN
        IF(testMPI%rank == 0) THEN
          bool=.TRUE.
        ELSE
          bool=.FALSE.
        ENDIF
        CALL testMPI%trueForAll(bool)
        ASSERT(.NOT.bool,'trueForAll - Parallel')

        bool1d=.FALSE.
        bool2d=.FALSE.
        bool3d=.FALSE.
        bool4d=.FALSE.
        IF(testMPI%rank == 0) THEN
          bool1d(1:5)=.TRUE.
          bool2d(1,:)=.TRUE.
          bool3d(1,:,:)=.TRUE.
          bool4d(1,:,2,:)=.TRUE.
        ELSE
          bool1d(1:4)=.TRUE.
          bool2d(2,:)=.TRUE.
          bool3d(1,2,:)=.TRUE.
          bool4d(:,:,2,:)=.TRUE.
        ENDIF
        CALL testMPI%trueForAll(bool1d)
        ASSERT_EQ(COUNT(bool1d),4,'trueForAll, 1D, count')
        ASSERT(ALL(bool1d(1:4)),'trueForAll, 1D')
        CALL testMPI%trueForAll(bool2d)
        ASSERT_EQ(COUNT(bool2d),0,'trueForAll, 1D, count')
        ASSERT(.NOT.ANY(bool2d),'trueForAll, 1D')
        CALL testMPI%trueForAll(bool3d)
        ASSERT_EQ(COUNT(bool3d),SIZE(bool3d,DIM=3),'trueForAll, 1D, count')
        ASSERT(ALL(bool3d(1,2,:)),'trueForAll, 1D')
        CALL testMPI%trueForAll(bool4d)
        ASSERT_EQ(COUNT(bool4d),SIZE(bool4d(1,:,2,:)),'trueForAll, 1D, count')
        ASSERT(ALL(bool4d(1,:,2,:)),'trueForAll, 1D')
      ENDIF

      COMPONENT_TEST('%trueForAny')
      CALL testMPI%clear()
      CALL testMPI%init()
      bool=.TRUE.
      CALL testMPI%trueForAny(bool)
      ASSERT(bool,'trueForAny - True')
      bool=.FALSE.
      CALL testMPI%trueForAny(bool)
      ASSERT(.NOT.bool,'trueForAny - False')
      IF(testMPI%nproc > 1) THEN
        IF(testMPI%rank == 0) THEN
          bool=.TRUE.
        ELSE
          bool=.FALSE.
        ENDIF
        CALL testMPI%trueForAny(bool)
        ASSERT(bool,'trueForAny - Parallel')

        bool1d=.FALSE.
        bool2d=.FALSE.
        bool3d=.FALSE.
        bool4d=.FALSE.
        IF(testMPI%rank == 0) THEN
          bool1d(1:5)=.TRUE.
          bool2d(1,:)=.TRUE.
          bool3d(1,:,:)=.TRUE.
          bool4d(:,:,1,:)=.TRUE.
        ELSE
          bool1d(1:4)=.TRUE.
          bool2d(2,:)=.TRUE.
          bool3d(1,2,:)=.TRUE.
          bool4d(:,:,2,:)=.TRUE.
        ENDIF
        CALL testMPI%trueForAny(bool1d)
        ASSERT_EQ(COUNT(bool1d),5,'trueForAny, 1D, count')
        ASSERT(ALL(bool1d(1:5)),'trueForAny, 1D')
        CALL testMPI%trueForAny(bool2d)
        ASSERT_EQ(COUNT(bool2d),SIZE(bool2d(1:2,:)),'trueForAny, 1D, count')
        ASSERT(ALL(bool2d(1:2,:)),'trueForAny, 1D')
        CALL testMPI%trueForAny(bool3d)
        ASSERT_EQ(COUNT(bool3d),SIZE(bool3d(1,:,:)),'trueForAny, 1D, count')
        ASSERT(ALL(bool3d(1,:,:)),'trueForAny, 1D')
        CALL testMPI%trueForAny(bool4d)
        ASSERT_EQ(COUNT(bool4d),SIZE(bool4d(:,:,1:2,:)),'trueForAny, 1D, count')
        ASSERT(ALL(bool4d(:,:,1:2,:)),'trueForAny, 1D')
      ENDIF

      CALL testMPI%clear()
    ENDSUBROUTINE testMPIEnv
!
!-------------------------------------------------------------------------------
    SUBROUTINE testPE_Env()
      CALL eParEnv%setStopOnError(.FALSE.)
      CALL eParEnv%setQuietMode(.TRUE.)

      !Test every line of isInit
      COMPONENT_TEST('%isInit()')
      ASSERT(.NOT.testPE%isInit(),'%isInit() 1')
      CALL testPE%world%init(PE_COMM_SELF)
      ASSERT(.NOT.testPE%isInit(),'%isInit() 2')
      ALLOCATE(testPE%space)
      ASSERT(.NOT.testPE%isInit(),'%isInit() 3')
      CALL testPE%space%init(PE_COMM_SELF)
      ASSERT(.NOT.testPE%isInit(),'%isInit() 4')
      ALLOCATE(testPE%angle)
      ASSERT(.NOT.testPE%isInit(),'%isInit() 5')
      CALL testPE%angle%init(PE_COMM_SELF)
      ASSERT(.NOT.testPE%isInit(),'%isInit() 6')
      ALLOCATE(testPE%energy)
      ASSERT(.NOT.testPE%isInit(),'%isInit() 7')
      CALL testPE%energy%init(PE_COMM_SELF)
      ALLOCATE(testPE%ray)
      ASSERT(.NOT.testPE%isInit(),'%isInit() 8')
      CALL testPE%ray%init()
      ASSERT(testPE%isInit(),'%isInit() 9')

      COMPONENT_TEST('%clear()')
      CALL testPE%clear()
      ASSERT(.NOT.ASSOCIATED(testPE%energy),'%energy')
      ASSERT(.NOT.ASSOCIATED(testPE%space),'%space')
      ASSERT(.NOT.ASSOCIATED(testPE%angle),'%angle')
      ASSERT(.NOT.ASSOCIATED(testPE%ray),'%ray')
      ASSERT(.NOT.testPE%world%isInit(),'%isInit()')

      COMPONENT_TEST('%initialize(...)')
#ifdef HAVE_MPI
      CALL testPE%initialize(PE_COMM_WORLD,2,1,1,1)
#else
      CALL testPE%initialize(PE_COMM_WORLD,1,1,1,1)
#endif
      ASSERT(testPE%world%comm /= PE_COMM_WORLD,'%world%comm world')
      ASSERT(testPE%world%comm /= PE_COMM_SELF,'%world%comm self')
      ASSERT(testPE%world%comm /= PE_COMM_NULL,'%world%comm null')
      ASSERT(testPE%world%nproc == mysize,'%world%nproc')
      ASSERT(testPE%world%rank == myrank,'%world%rank')
      ASSERT(testPE%world%master == (myrank == 0),'%world%master')
      ASSERT(testPE%world%isInit(),'%isInit()')
      ASSERTFAIL(ASSOCIATED(testPE%energy),'%energy')
      ASSERT(testPE%energy%comm /= PE_COMM_WORLD,'%energy%comm world')
      ASSERT(testPE%energy%comm /= PE_COMM_SELF,'%energy%comm self')
      ASSERT(testPE%energy%comm /= PE_COMM_NULL,'%energy%comm null')
      ASSERT(testPE%energy%nproc == 1,'%energy%nproc')
      ASSERT(testPE%energy%rank == 0,'%energy%rank')
      ASSERT(testPE%energy%master,'%energy%master')
      ASSERT(testPE%energy%isInit(),'%isInit()')
      ASSERTFAIL(ASSOCIATED(testPE%space),'%space')
      ASSERT(testPE%space%comm /= PE_COMM_WORLD,'%space%comm world')
      ASSERT(testPE%space%comm /= PE_COMM_SELF,'%space%comm self')
      ASSERT(testPE%space%comm /= PE_COMM_NULL,'%space%comm null')
      ASSERT(testPE%space%nproc == mysize,'%space%nproc')
      ASSERT(testPE%space%rank == myrank,'%space%rank')
      ASSERT(testPE%space%master == (myrank == 0),'%space%master')
      ASSERT(testPE%space%isInit(),'%isInit()')
      ASSERTFAIL(ASSOCIATED(testPE%angle),'%angle')
      ASSERT(testPE%angle%comm /= PE_COMM_WORLD,'%angle%comm world')
      ASSERT(testPE%angle%comm /= PE_COMM_SELF,'%angle%comm self')
      ASSERT(testPE%angle%comm /= PE_COMM_NULL,'%angle%comm null')
      ASSERT(testPE%angle%nproc == 1,'%angle%nproc')
      ASSERT(testPE%angle%rank == 0,'%angle%rank')
      ASSERT(testPE%angle%master,'%angle%master')
      ASSERT(testPE%angle%isInit(),'%isInit()')
      ASSERTFAIL(ASSOCIATED(testPE%ray),'%ray')
      ASSERT(testPE%ray%nproc == 1,'%ray%nproc')
      ASSERT(testPE%ray%rank == 0,'%ray%rank')
      ASSERT(testPE%ray%master,'%ray%master')
      ASSERT(testPE%ray%isInit(),'%isInit()')
      
      COMPONENT_TEST('deviceID')
      ASSERT_EQ(testPE%getDeviceID(),-1,'%deviceID')
      CALL testPE%setDeviceID(2)
      ASSERT_EQ(testPE%getDeviceID(),2,'%deviceID')

      COMPONENT_TEST('OPERATOR(=)')
      testPE2=testPE
      ASSERT(testPE2%world%comm /= testPE%world%comm,'world%comm')
      ASSERT(testPE2%world%nproc == testPE%world%nproc,'%world%nproc')
      ASSERT(testPE2%world%rank == testPE%world%rank,'%world%rank')
      ASSERT(testPE2%world%master .EQV. testPE%world%master,'%world%master')
      ASSERT(testPE2%world%isInit() .EQV. testPE%world%isInit(),'%isInit()')
      ASSERTFAIL(ASSOCIATED(testPE2%energy),'%energy')
      ASSERT(.NOT.ASSOCIATED(testPE2%energy,testPE%energy),'%energy')
      ASSERT(testPE2%energy%comm /= PE_COMM_WORLD,'%energy%comm world')
      ASSERT(testPE2%energy%comm /= PE_COMM_SELF,'%energy%comm self')
      ASSERT(testPE2%energy%comm /= PE_COMM_NULL,'%energy%comm null')
      ASSERT(testPE2%energy%comm /= testPE%energy%comm,'%energy%comm')
      ASSERT(testPE2%energy%nproc == testPE%energy%nproc,'%energy%nproc')
      ASSERT(testPE2%energy%rank == testPE%energy%rank,'%energy%rank')
      ASSERT(testPE2%energy%master .EQV. testPE%energy%master,'%energy%master')
      ASSERT(testPE2%energy%isInit() .EQV. testPE%energy%isInit(),'%isInit()')
      ASSERTFAIL(ASSOCIATED(testPE2%space),'%space')
      ASSERT(.NOT.ASSOCIATED(testPE2%space,testPE%space),'%space')
      ASSERT(testPE2%space%comm /= PE_COMM_WORLD,'%space%comm world')
      ASSERT(testPE2%space%comm /= PE_COMM_SELF,'%space%comm self')
      ASSERT(testPE2%space%comm /= PE_COMM_NULL,'%space%comm null')
      ASSERT(testPE2%space%comm /= testPE%space%comm,'%space%comm')
      ASSERT(testPE2%space%nproc == testPE%space%nproc,'%space%nproc')
      ASSERT(testPE2%space%rank == testPE%space%rank,'%space%rank')
      ASSERT(testPE2%space%master .EQV. testPE%space%master,'%space%master')
      ASSERT(testPE2%space%isInit() .EQV. testPE%space%isInit(),'%isInit()')
      ASSERTFAIL(ASSOCIATED(testPE2%angle),'%angle')
      ASSERT(.NOT.ASSOCIATED(testPE2%angle,testPE%angle),'%angle')
      ASSERT(testPE2%angle%comm /= PE_COMM_WORLD,'%angle%comm world')
      ASSERT(testPE2%angle%comm /= PE_COMM_SELF,'%angle%comm self')
      ASSERT(testPE2%angle%comm /= PE_COMM_NULL,'%angle%comm null')
      ASSERT(testPE2%angle%comm /= testPE%angle%comm,'%angle%comm')
      ASSERT(testPE2%angle%nproc == testPE%angle%nproc,'%angle%nproc')
      ASSERT(testPE2%angle%rank == testPE%angle%rank,'%angle%rank')
      ASSERT(testPE2%angle%master .EQV. testPE%angle%master,'%angle%master')
      ASSERT(testPE2%angle%isInit() .EQV. testPE%angle%isInit(),'%isInit()')
      ASSERTFAIL(ASSOCIATED(testPE2%ray),'%ray')
      ASSERT(.NOT.ASSOCIATED(testPE2%ray,testPE%ray),'%ray')
      ASSERT(testPE2%ray%nproc == testPE%ray%nproc,'%ray%nproc')
      ASSERT(testPE2%ray%rank == testPE%ray%rank,'%ray%rank')
      ASSERT(testPE2%ray%master .EQV. testPE%ray%master,'%ray%master')
      ASSERT(testPE2%ray%isInit() .EQV. testPE%ray%isInit(),'%isInit()')
      CALL testPE2%clear()
      ASSERT(testPE%world%comm /= PE_COMM_WORLD,'%world%comm world')
      ASSERT(testPE%world%comm /= PE_COMM_SELF,'%world%comm self')
      ASSERT(testPE%world%comm /= PE_COMM_NULL,'%world%comm null')
      ASSERT(testPE%world%nproc == mysize,'%world%nproc')
      ASSERT(testPE%world%rank == myrank,'%world%rank')
      ASSERT(testPE%world%master == (myrank == 0),'%world%master')
      ASSERT(testPE%world%isInit(),'%isInit()')
      ASSERTFAIL(ASSOCIATED(testPE%energy),'%energy')
      ASSERT(testPE%energy%comm /= PE_COMM_WORLD,'%energy%comm world')
      ASSERT(testPE%energy%comm /= PE_COMM_SELF,'%energy%comm self')
      ASSERT(testPE%energy%comm /= PE_COMM_NULL,'%energy%comm null')
      ASSERT(testPE%energy%nproc == 1,'%energy%nproc')
      ASSERT(testPE%energy%rank == 0,'%energy%rank')
      ASSERT(testPE%energy%master,'%energy%master')
      ASSERT(testPE%energy%isInit(),'%isInit()')
      ASSERTFAIL(ASSOCIATED(testPE%space),'%space')
      ASSERT(testPE%space%comm /= PE_COMM_WORLD,'%space%comm world')
      ASSERT(testPE%space%comm /= PE_COMM_SELF,'%space%comm self')
      ASSERT(testPE%space%comm /= PE_COMM_NULL,'%space%comm null')
      ASSERT(testPE%space%nproc == mysize,'%space%nproc')
      ASSERT(testPE%space%rank == myrank,'%space%rank')
      ASSERT(testPE%space%master == (myrank == 0),'%space%master')
      ASSERT(testPE%space%isInit(),'%isInit()')
      ASSERTFAIL(ASSOCIATED(testPE%angle),'%angle')
      ASSERT(testPE%angle%comm /= PE_COMM_WORLD,'%angle%comm world')
      ASSERT(testPE%angle%comm /= PE_COMM_SELF,'%angle%comm self')
      ASSERT(testPE%angle%comm /= PE_COMM_NULL,'%angle%comm null')
      ASSERT(testPE%angle%nproc == 1,'%angle%nproc')
      ASSERT(testPE%angle%rank == 0,'%angle%rank')
      ASSERT(testPE%angle%master,'%angle%master')
      ASSERT(testPE%angle%isInit(),'%isInit()')
      ASSERTFAIL(ASSOCIATED(testPE%ray),'%ray')
      ASSERT(testPE%ray%nproc == 1,'%ray%nproc')
      ASSERT(testPE%ray%rank == 0,'%ray%rank')
      ASSERT(testPE%ray%master,'%ray%master')
      ASSERT(testPE%ray%isInit(),'%isInit()')
      CALL testPE%clear()

      !Error Checking coverage
      CALL testPE%initialize(PE_COMM_WORLD,0,0,0,0)
!
!Not sure if we want to every resurrect this stuff.
!
  !CALL testPE%initialize(PE_COMM_WORLD,0,0,0,0)
  !CALL testPE%init(MPI_COMM_WORLD,mysize,1,1,1)
  !CALL testPE%world%barrier()
  !WRITE(OUTPUT_UNIT,*) myrank,testPE%space%rank,testPE%energy%rank,testPE%angle%rank
  !FLUSH(OUTPUT_UNIT)
  !CALL testPE%world%barrier()
!  CALL MPI_Reduce(testPE%world%rank,tmp,1,MPI_INTEGER,MPI_SUM,0,testPE%space%comm,mpierr)
!  CALL testPE%world%barrier()
!  IF(testPE%space%master) WRITE(OUTPUT_UNIT,*) 'SPACE REDUCE Check:',myrank,tmp
!  FLUSH(OUTPUT_UNIT)
!  CALL testPE%world%barrier()
!  CALL MPI_Reduce(testPE%world%rank,tmp,1,MPI_INTEGER,MPI_SUM,0,testPE%energy%comm,mpierr)
!  CALL testPE%world%barrier()
!  IF(testPE%energy%master) WRITE(OUTPUT_UNIT,*) 'ENERGY REDUCE Check:',myrank,tmp
!  FLUSH(OUTPUT_UNIT)
!  CALL testPE%world%barrier()
!  IF(.NOT.testPE%world%isInit() .OR. testPE%world%rank /= myrank .OR. &
!     testPE%world%nproc == -1 .OR. .NOT.testPE%space%isInit() .OR. &
!     testPE%space%nproc /= testPE%world%nproc .OR. testPE%space%rank /= myrank &
!     .OR. .NOT.testPE%energy%isInit() .OR. testPE%energy%nproc /= 1 .OR. &
!     testPE%energy%rank /= 0 .OR. .NOT.testPE%energy%master .OR. &
!     .NOT.testPE%angle%isInit() .OR. testPE%angle%nproc /= 1 .OR. &
!     testPE%angle%rank /= 0 .OR. .NOT.testPE%angle%master .OR. &
!     testPE%ray%nproc /= 1 .OR. .NOT.testPE%ray%master) THEN
!    WRITE(OUTPUT_UNIT,*) 'CALL testPE%init FAILED!',myrank
!    CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
!  ELSE
!    CALL testPE%world%barrier()
!    WRITE(OUTPUT_UNIT,*) '  Passed: testPE%init(...)',myrank
!    FLUSH(OUTPUT_UNIT)
!    CALL testPE%world%barrier()
!  ENDIF
!  CALL testPE2%init(MPI_COMM_WORLD,1,testPE%world%nproc,1,1)
!  IF(.NOT.testPE2%world%isInit() .OR. testPE2%world%rank /= myrank .OR. &
!     testPE2%world%nproc == -1 .OR. .NOT.testPE2%energy%isInit() .OR. &
!     testPE2%energy%nproc /= testPE2%world%nproc .OR. testPE2%energy%rank /= myrank &
!     .OR. .NOT.testPE2%space%isInit() .OR. testPE2%space%nproc /= 1 .OR. &
!     testPE2%space%rank /= 0 .OR. .NOT.testPE2%space%master .OR. &
!     .NOT.testPE2%angle%isInit() .OR. testPE2%angle%nproc /= 1 .OR. &
!     testPE2%angle%rank /= 0 .OR. .NOT.testPE2%angle%master .OR. &
!     testPE2%ray%nproc /= 1 .OR. .NOT.testPE2%ray%master) THEN
!    WRITE(OUTPUT_UNIT,*) 'CALL testPE2%init FAILED!',myrank
!    CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
!  ELSE
!    CALL testPE2%world%barrier()
!    WRITE(OUTPUT_UNIT,*) '  Passed: testPE2%init(...)',myrank
!    FLUSH(OUTPUT_UNIT)
!    CALL testPE2%world%barrier()
!  ENDIF
!  CALL testPE3%init(MPI_COMM_WORLD,1,1,testPE%world%nproc,1)
!  IF(.NOT.testPE3%world%isInit() .OR. testPE3%world%rank /= myrank .OR. &
!     testPE3%world%nproc == -1 .OR. .NOT.testPE3%angle%isInit() .OR. &
!     testPE3%angle%nproc /= testPE3%world%nproc .OR. testPE3%angle%rank /= myrank &
!     .OR. .NOT.testPE3%space%isInit() .OR. testPE3%space%nproc /= 1 .OR. &
!     testPE3%space%rank /= 0 .OR. .NOT.testPE3%space%master .OR. &
!     .NOT.testPE3%energy%isInit() .OR. testPE3%energy%nproc /= 1 .OR. &
!     testPE3%energy%rank /= 0 .OR. .NOT.testPE3%energy%master .OR. &
!     testPE3%ray%nproc /= 1 .OR. .NOT.testPE3%ray%master) THEN
!    WRITE(OUTPUT_UNIT,*) 'CALL testPE3%init FAILED!',myrank
!    CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
!  ELSE
!    CALL testPE3%world%barrier()
!    WRITE(OUTPUT_UNIT,*) '  Passed: testPE3%init(...)',myrank
!    FLUSH(OUTPUT_UNIT)
!    CALL testPE3%world%barrier()
!  ENDIF
    ENDSUBROUTINE testPE_Env
!
ENDPROGRAM testParallelEnv
