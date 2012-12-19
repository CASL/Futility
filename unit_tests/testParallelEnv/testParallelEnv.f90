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
PROGRAM testParallelEnv
  
  USE ISO_FORTRAN_ENV
  USE IntrType
  USE ExceptionHandler
  USE ParallelEnv
  !$ USE OMP_LIB
  IMPLICIT NONE

#ifdef HAVE_MPI
  INCLUDE 'mpif.h'
#endif

  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(ParallelEnvType) :: testPE,testPE2,testPE3
  TYPE(MPI_EnvType) :: testMPI,testMPI2
  TYPE(OMP_EnvType) :: testOMP
  
  INTEGER :: mpierr,myrank,mysize,tmp,stt,stp
  INTEGER(SIK),ALLOCATABLE :: testIDX(:),testWGT(:)
  
#ifdef HAVE_MPI
  CALL MPI_Init(mpierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD,myrank,mpierr)
  CALL MPI_Comm_size(MPI_COMM_WORLD,mysize,mpierr)
  tmp=0
#else
  myrank=0
  mysize=1
#endif
  eParEnv => e
  CALL eParEnv%setQuietMode(.TRUE.)
  CALL eParEnv%setStopOnError(.FALSE.)
  
  IF(myrank == 0) THEN
    WRITE(OUTPUT_UNIT,*) '==================================================='
    WRITE(OUTPUT_UNIT,*) 'TESTING PARALLEL ENVIRONMENT...'
    WRITE(OUTPUT_UNIT,*) '==================================================='
    
    WRITE(OUTPUT_UNIT,*) 'TESTING OMP_ENVTYPE'
    IF(testOMP%isInit()) THEN
      WRITE(OUTPUT_UNIT,*) 'testOMP%isInit() FAILED!'
      STOP 666
    ENDIF
    CALL testOMP%init()
    IF(testOMP%nproc /= 1 .OR. testOMP%rank /= 0 .OR. &
        .NOT.testOMP%isInit() .OR. .NOT.testOMP%master) THEN
      WRITE(OUTPUT_UNIT,*) 'testOMP%init() FAILED!'
      STOP 666
    ELSE
      WRITE(OUTPUT_UNIT,*) '  Passed: testOMP%isInit() (NO OMP)'
      WRITE(OUTPUT_UNIT,*) '  Passed: testOMP%init() (NO OMP)'
    ENDIF
    CALL testOMP%clear()
    IF(testOMP%nproc /= -1 .OR. testOMP%rank /= -1 .OR. &
      testOMP%isInit() .OR. testOMP%master) THEN
      WRITE(OUTPUT_UNIT,*) 'testOMP%clear() FAILED!'
      STOP 666
    ELSE
      WRITE(*,*) '  Passed: testOMP%clear()'
    ENDIF
!$  CALL testOMP%init(1)
!$  IF(testOMP%nproc /= 1 .OR. testOMP%rank /= 0 .OR. &
!$     .NOT.testOMP%master) THEN
!$    WRITE(OUTPUT_UNIT,*) 'testOMP%init(1) FAILED!'
!$    STOP 666
!$  ELSE
!$    WRITE(OUTPUT_UNIT,*) '  Passed: testOMP%init(1)'
!$  ENDIF
!$  CALL testOMP%init(1000)
!$  IF(testOMP%nproc /= omp_get_max_threads() .OR. testOMP%rank /= 0 .OR. &
!$     .NOT.testOMP%master) THEN
!$    WRITE(OUTPUT_UNIT,*) 'testOMP%init(1000) FAILED!'
!$    STOP 666
!$  ELSE
!$    WRITE(OUTPUT_UNIT,*) '  Passed: testOMP%init(1000)'
!$  ENDIF
    WRITE(OUTPUT_UNIT,*) '---------------------------------------------------'
  ENDIF
  
  IF(myrank == 0) WRITE(OUTPUT_UNIT,*) 'TESTING MPI_ENVTYPE'
#ifdef HAVE_MPI
    FLUSH(OUTPUT_UNIT)
    CALL MPI_Barrier(MPI_COMM_WORLD,mpierr)
#endif
  IF(testMPI%isInit()) THEN
    WRITE(OUTPUT_UNIT,*) myrank,'CALL testMPI%isInit() FAILED!'
#ifdef HAVE_MPI
    FLUSH(OUTPUT_UNIT)
    CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
    STOP 666
#endif
  ELSE
    IF(myrank == 0) WRITE(OUTPUT_UNIT,*) '  Passed: CALL testMPI%isInit()'
#ifdef HAVE_MPI
    FLUSH(OUTPUT_UNIT)
    CALL MPI_Barrier(MPI_COMM_WORLD,mpierr)
#endif
  ENDIF
#ifdef HAVE_MPI
  CALL testMPI%init(MPI_COMM_WORLD)
  IF(testMPI%comm /= MPI_COMM_WORLD .OR. testMPI%rank /= myrank &
     .OR. testMPI%nproc == -1 .OR. .NOT.testMPI%isInit()) THEN
    WRITE(OUTPUT_UNIT,*) myrank,'CALL testMPI%init(MPI_COMM_WORLD) FAILED!'
    FLUSH(OUTPUT_UNIT)
    CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
  ELSE
    WRITE(OUTPUT_UNIT,*) '  Passed: CALL testMPI%init(MPI_COMM_WORLD)', &
      testMPI%rank
    FLUSH(OUTPUT_UNIT)
    CALL MPI_Barrier(MPI_COMM_WORLD,mpierr)
  ENDIF
#else
  WRITE(OUTPUT_UNIT,*) '  WARNING: EXECUTABLE NOT COMPILED FOR MPI'
  CALL testMPI%init(PE_COMM_SELF)
  IF(testMPI%comm /= 1 .OR. testMPI%nproc /= 1 .OR. testMPI%rank /= 0 &
     .OR. .NOT.testMPI%master .OR. .NOT.testMPI%isInit()) THEN
    WRITE(OUTPUT_UNIT,*) 'CALL testMPI%init(0) FAILED!'
    STOP 666
  ELSE
    WRITE(OUTPUT_UNIT,*) '  Passed: CALL testMPI%init(0)'
  ENDIF
#endif
  CALL testMPI%barrier()
  IF(myrank == 0) WRITE(OUTPUT_UNIT,*) '  Passed: CALL testMPI%barrier()'
  FLUSH(OUTPUT_UNIT)
  CALL testMPI%barrier()
  
  CALL testMPI2%init(testMPI%comm)
  IF(testMPI%master) THEN
    testMPI2%nproc=7
    testMPI2%rank=0
    CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
    IF(stt /= 2 .OR. stp /= 5) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    testMPI2%rank=1
    CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
    IF(stt /= 6 .OR. stp /= 9) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    testMPI2%rank=2
    CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
    IF(stt /= 10 .OR. stp /= 13) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    testMPI2%rank=3
    CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
    IF(stt /= 14 .OR. stp /= 16) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    testMPI2%rank=4
    CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
    IF(stt /= 17 .OR. stp /= 19) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    testMPI2%rank=5
    CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
    IF(stt /= 20 .OR. stp /= 22) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    testMPI2%rank=6
    CALL testMPI2%partition(N1=2,N2=25,ISTT=stt,ISTP=stp)
    IF(stt /= 23 .OR. stp /= 25) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    
    testMPI2%nproc=10
    ALLOCATE(testWGT(40))
    testWGT=(/936,936,936,936,1722,1722,1722,1722,1722,1722,1722,1722,1916, &
              1916,1916,1916,1944,1944,1944,1944,1916,1916,1916,1916,1571, &
              1571,1571,1571,4122,4122,4122,4122,4266,4266,4266,4266,1850, &
              1850,1850,1850/)
    eParEnv => NULL()
    CALL testMPI2%partition(IWGT=testWGT,IPART=0,IDXMAP=testIDX)
    eParEnv => e
    IF(ANY(testIDX /= (/11,23,33/))) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    CALL testMPI2%partition(IWGT=testWGT,IPART=1,IDXMAP=testIDX)
    IF(ANY(testIDX /= (/12,24,34/))) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    CALL testMPI2%partition(IWGT=testWGT,IPART=2,IDXMAP=testIDX)
    IF(ANY(testIDX /= (/3,9,35,37/))) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    CALL testMPI2%partition(IWGT=testWGT,IPART=3,IDXMAP=testIDX)
    IF(ANY(testIDX /= (/4,10,36,38/))) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    CALL testMPI2%partition(IWGT=testWGT,IPART=4,IDXMAP=testIDX)
    IF(ANY(testIDX /= (/5,15,27,29/))) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    CALL testMPI2%partition(IWGT=testWGT,IPART=5,IDXMAP=testIDX)
    IF(ANY(testIDX /= (/6,16,28,30/))) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    CALL testMPI2%partition(IWGT=testWGT,IDXMAP=testIDX)
    IF(ANY(testIDX /= (/1,7,21,31/))) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    CALL testMPI2%partition(IWGT=testWGT,IPART=7,IDXMAP=testIDX)
    IF(ANY(testIDX /= (/2,8,22,32/))) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    CALL testMPI2%partition(IWGT=testWGT,IPART=8,IDXMAP=testIDX)
    IF(ANY(testIDX /= (/13,17,19,25,39/))) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    CALL testMPI2%partition(IWGT=testWGT,N1=1,N2=40,IPART=9,IDXMAP=testIDX)
    IF(ANY(testIDX /= (/14,18,20,26,40/))) THEN
      WRITE(OUTPUT_UNIT,*) testMPI2%rank,'CALL testMPI2%partition(...) FAILED!'
#ifdef HAVE_MPI
      FLUSH(OUTPUT_UNIT)
      CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
      STOP 666
#endif
    ENDIF
    
    !Error Checking
    CALL testMPI2%partition(IWGT=testWGT,N1=1,N2=40,IPART=100,IDXMAP=testIDX)
    CALL testMPI2%partition(IWGT=testWGT,N1=41,N2=40,IDXMAP=testIDX)
    CALL testMPI2%clear()
    CALL testMPI2%partition(IWGT=testWGT,N1=1,N2=40,IDXMAP=testIDX)
    WRITE(*,*) '  Passed: testMPI2%partition(...)'
    FLUSH(OUTPUT_UNIT)
  ENDIF
  
  CALL testMPI%barrier()
  CALL testMPI%clear()
  IF(testMPI%comm /= -1 .OR. testMPI%nproc /= -1 .OR. testMPI%rank /= -1 &
     .OR. testMPI%master .OR. testMPI%isInit()) THEN
    WRITE(OUTPUT_UNIT,*) myrank,'CALL testMPI%clear() FAILED!'
#ifdef HAVE_MPI
    FLUSH(OUTPUT_UNIT)
    CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
    STOP 666
#endif
  ELSE
    WRITE(OUTPUT_UNIT,*) '  Passed: CALL testMPI%clear()',myrank
#ifdef HAVE_MPI
    FLUSH(OUTPUT_UNIT)
    CALL MPI_Barrier(MPI_COMM_WORLD,mpierr)
#endif
  ENDIF
  IF(myrank == 0) WRITE(OUTPUT_UNIT,*) '---------------------------------------------------'
  IF(myrank == 0) WRITE(OUTPUT_UNIT,*) 'TESTING PARENVTYPE'
  
#ifdef HAVE_MPI
  FLUSH(OUTPUT_UNIT)
  CALL MPI_Barrier(MPI_COMM_WORLD,mpierr)
  CALL eParEnv%setStopOnError(.FALSE.)
  CALL eParEnv%setQuietMode(.TRUE.)
  CALL testPE%initialize(MPI_COMM_WORLD,0,0,0,0)
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
#else
  !Test every line of isInit
  IF(testPE%isInit()) THEN
    WRITE(*,*) 'CALL testPE%isInit() world%isInit() FAILED!'
    STOP 666
  ENDIF
  CALL testPE%world%init(0)
  IF(testPE%isInit()) THEN
    WRITE(*,*) 'CALL testPE%isInit() ASSOCIATED(space) FAILED!'
    STOP 666
  ENDIF
  ALLOCATE(testPE%space)
  IF(testPE%isInit()) THEN
    WRITE(*,*) 'CALL testPE%isInit() space%isInit() FAILED!'
    STOP 666
  ENDIF
  CALL testPE%space%init(0)
  IF(testPE%isInit()) THEN
    WRITE(*,*) 'CALL testPE%isInit() ASSOCIATED(angle) FAILED!'
    STOP 666
  ENDIF
  ALLOCATE(testPE%angle)
  IF(testPE%isInit()) THEN
    WRITE(*,*) 'CALL testPE%isInit() angle%isInit() FAILED!'
    STOP 666
  ENDIF
  CALL testPE%angle%init(0)
  IF(testPE%isInit()) THEN
    WRITE(*,*) 'CALL testPE%isInit() ASSOCIATED(energy) FAILED!'
    STOP 666
  ENDIF
  ALLOCATE(testPE%energy)
  IF(testPE%isInit()) THEN
    WRITE(*,*) 'CALL testPE%isInit() energy%isInit() FAILED!'
    STOP 666
  ENDIF
  CALL testPE%energy%init(0)
  IF(testPE%isInit()) THEN
    WRITE(*,*) 'CALL testPE%isInit() ASSOCIATED(ray) FAILED!'
    STOP 666
  ENDIF
  ALLOCATE(testPE%ray)
  IF(testPE%isInit()) THEN
    WRITE(*,*) 'CALL testPE%isInit() ray%isInit() FAILED!'
    STOP 666
  ENDIF
  CALL testPE%ray%init()
  IF(.NOT.testPE%isInit()) THEN
    WRITE(*,*) 'CALL testPE%isInit() FAILED!'
    STOP 666
  ELSE
    WRITE(OUTPUT_UNIT,*) '  Passed: CALL testPE%isInit()'
  ENDIF
  CALL testPE%clear()
  
  CALL testPE%initialize(0,1,1,1,1)
  IF(.NOT.testPE%isInit() .OR. testPE%world%comm /= 1 .OR. &
     testPE%world%nproc /=1 .OR. testPE%world%rank /= 0 .OR. &
     .NOT.testPE%world%master .OR. .NOT.testPE%energy%master .OR. &
     .NOT.testPE%energy%isInit() .OR. testPE%energy%comm /= 1 .OR. &
     testPE%energy%nproc /=1 .OR. testPE%energy%rank /= 0 .OR.  &
     .NOT.testPE%space%isInit() .OR. testPE%space%comm /= 1 .OR. &
     testPE%space%nproc /=1 .OR. testPE%space%rank /= 0 .OR. &
     .NOT.testPE%space%master .OR. .NOT.testPE%angle%master .OR. &
     .NOT.testPE%angle%isInit() .OR. testPE%angle%comm /= 1 .OR. &
     testPE%angle%nproc /=1 .OR. testPE%angle%rank /= 0 .OR.  &
     testPE%ray%nproc /= 1 .OR. testPE%ray%rank /= 0 .OR. &
     .NOT.testPE%ray%master) THEN
    WRITE(OUTPUT_UNIT,*) 'CALL testPE%init() FAILED!'
    STOP 666
  ELSE
    WRITE(OUTPUT_UNIT,*) '  Passed: CALL testPE%init()'
  ENDIF
#endif
  CALL testPE%clear()
  IF(testPE%world%isInit() .OR. ASSOCIATED(testPE%space) .OR. &
     ASSOCIATED(testPE%angle) .OR. ASSOCIATED(testPE%ray)) THEN
    WRITE(OUTPUT_UNIT,*) 'CALL testPE%clear() FAILED!',myrank
#ifdef HAVE_MPI
    FLUSH(OUTPUT_UNIT)
    CALL MPI_Abort(MPI_COMM_WORLD,666,mpierr)
#else
    STOP 666
#endif
  ELSE
    WRITE(OUTPUT_UNIT,*) '  Passed: CALL testPE%clear()',myrank
#ifdef HAVE_MPI
    FLUSH(OUTPUT_UNIT)
    CALL MPI_Barrier(MPI_COMM_WORLD,mpierr)
#endif
  ENDIF

  IF(myrank == 0) THEN
    WRITE(*,*) '==================================================='
    WRITE(*,*) 'TESTING PARALLEL ENVIRONMENT PASSED!'
    WRITE(*,*) '==================================================='
  ENDIF
#ifdef HAVE_MPI
  FLUSH(OUTPUT_UNIT)
  CALL MPI_Barrier(MPI_COMM_WORLD,mpierr)
#endif
  CALL testMPI%finalize()
ENDPROGRAM testParallelEnv
