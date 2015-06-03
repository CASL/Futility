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
!> @brief Utility Module defines types describing the parallel run time
!> environment.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "Exceptionhandler": @copybrief ExceptionHandler
!>
!> @par EXAMPLE
!> @code
!>
!> @endcode
!>
!> @author Brendan Kochunas
!>   @date 08/24/2011
!>
!> @todo
!>  - initialization/clear routines for ParallelEnvType
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ParallelEnv

  USE IntrType
  USE ExceptionHandler
  USE BLAS
  USE Allocs
!$ USE OMP_LIB

  IMPLICIT NONE
  PRIVATE

#ifdef HAVE_MPI

#ifdef MPACT_HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
  PetscErrorCode  :: ierr
  PetscBool :: petsc_isinit
#else
  INCLUDE 'mpif.h'
#endif

  INTEGER,PARAMETER :: PE_COMM_SELF=MPI_COMM_SELF
  INTEGER,PARAMETER :: PE_COMM_WORLD=MPI_COMM_WORLD
  INTEGER,PARAMETER :: PE_COMM_NULL=MPI_COMM_NULL
  INTEGER,SAVE :: PE_COMM_DEFAULT=PE_COMM_WORLD
#else
  INTEGER,PARAMETER :: PE_COMM_SELF=1
  INTEGER,PARAMETER :: PE_COMM_WORLD=0
  INTEGER,PARAMETER :: PE_COMM_NULL=-1
  INTEGER,SAVE :: PE_COMM_DEFAULT=0
#endif

  PUBLIC :: PE_COMM_SELF
  PUBLIC :: PE_COMM_WORLD
  PUBLIC :: PE_COMM_NULL
  PUBLIC :: PE_COMM_DEFAULT
  PUBLIC :: MPI_EnvType
  PUBLIC :: OMP_EnvType
  PUBLIC :: ParallelEnvType
  PUBLIC :: eParEnv
  PUBLIC :: ASSIGNMENT(=)

  TYPE,ABSTRACT :: ParEnvType
    !> Logical with initialization status
    LOGICAL(SBK),PRIVATE :: initstat=.FALSE.
    !> Whether or not this processor is the master.
    LOGICAL(SBK) :: master=.FALSE.
    !> The number of processors in the communicator
    INTEGER(SIK) :: nproc=-1
    !> The rank of the processor within the communicator
    INTEGER(SIK) :: rank=-1
!
!List of type bound procedures (methods) for the object
    CONTAINS
      !> @copybrief ParallelEnv::getInitStat_ParEnvType
      !> @copydetails  ParallelEnv::getInitStat_ParEnvType
      PROCEDURE,PASS :: isInit => getInitStat_ParEnvType
      !> @copybrief ParallelEnv::partition_indices_ParEnvType
      !> @copydetails  ParallelEnv::partition_indices_ParEnvType
      PROCEDURE,PASS :: partitionIDX => partition_indices_ParEnvType
      !> @copybrief ParallelEnv::partition_greedy_ParEnvType
      !> @copydetails  ParallelEnv::partition_greedy_ParEnvType
      PROCEDURE,PASS :: partitionGreedy => partition_greedy_ParEnvType
      !>
      GENERIC :: partition => partitionIDX,partitionGreedy
      !>
      PROCEDURE(ParEnvType_init_absintfc),DEFERRED,PASS :: init
      !>
      PROCEDURE(ParEnvType_clear_absintfc),DEFERRED,PASS :: clear

  ENDTYPE ParEnvType

  ABSTRACT INTERFACE
    SUBROUTINE ParEnvType_init_absintfc(myPE,PEparam)
      IMPORT :: SIK,ParEnvType
      CLASS(ParEnvType),INTENT(INOUT) :: myPE
      INTEGER(SIK),INTENT(IN),OPTIONAL :: PEparam
    ENDSUBROUTINE ParEnvType_init_absintfc
  ENDINTERFACE

  ABSTRACT INTERFACE
    SUBROUTINE ParEnvType_clear_absintfc(myPE)
      IMPORT :: ParEnvType
      CLASS(ParEnvType),INTENT(INOUT) :: myPE
    ENDSUBROUTINE ParEnvType_clear_absintfc
  ENDINTERFACE

  !> Type describes basic information for MPI environment
  TYPE,EXTENDS(ParEnvType) :: MPI_EnvType
    !> Fortran integer ID for the communicator
    INTEGER(SIK) :: comm=-1
!
!List of type bound procedures (methods) for the object
    CONTAINS
      !> @copybrief ParallelEnv::init_MPI_Env_type
      !> @copydetails ParallelEnv::init_MPI_Env_type
      PROCEDURE,PASS :: init => init_MPI_Env_type
      !> @copybrief ParallelEnv::clear_MPI_Env_type
      !> @copydetails ParallelEnv::clear_MPI_Env_type
      PROCEDURE,PASS :: clear => clear_MPI_Env_type
      !> @copybrief ParallelEnv::barrier_MPI_Env_type
      !> @copydetails ParallelEnv::barrier_MPI_Env_type
      PROCEDURE,PASS :: barrier => barrier_MPI_Env_type
      !> @copybrief ParallelEnv::gather_SIK0_MPI_Env_type
      !> @copydetails ParallelEnv::gather_SIK0_MPI_Env_type
      PROCEDURE,PASS,PRIVATE :: gather_SIK0_MPI_Env_type
      !> @copybrief ParallelEnv::gather_SLK0_MPI_Env_type
      !> @copydetails ParallelEnv::gather_SLK0_MPI_Env_type
      PROCEDURE,PASS,PRIVATE :: gather_SLK0_MPI_Env_type
      !> @copybrief ParallelEnv::gather_SLK1_MPI_Env_type
      !> @copydetails ParallelEnv::gather_SLK1_MPI_Env_type
      PROCEDURE,PASS,PRIVATE :: gather_SLK1_MPI_Env_type
      !>
      GENERIC :: gather => gather_SIK0_MPI_Env_type, &
                           gather_SLK0_MPI_Env_type, &
                           gather_SLK1_MPI_Env_type
      !> @copybrief ParallelEnv::scatter_SLK0_MPI_Env_type
      !> @copydetails ParallelEnv::scatter_SLK0_MPI_Env_type
      PROCEDURE,PASS,PRIVATE :: scatter_SLK0_MPI_Env_type
      !> @copybrief ParallelEnv::scatter_SLK1_MPI_Env_type
      !> @copydetails ParallelEnv::scatter_SLK1_MPI_Env_type
      PROCEDURE,PASS,PRIVATE :: scatter_SLK1_MPI_Env_type
      !>
      GENERIC :: scatter => scatter_SLK0_MPI_Env_type, &
                            scatter_SLK1_MPI_Env_type
      !> @copybrief ParallelEnv::bcast_SLK0_MPI_Env_type
      !> @copydetails ParallelEnv::bcast_SLK0_MPI_Env_type
      PROCEDURE,PASS,PRIVATE :: bcast_SLK0_MPI_Env_type
      !> @copybrief ParallelEnv::bcast_SSK2_MPI_Env_type
      !> @copydetails ParallelEnv::bcast_SSK2_MPI_Env_type
      PROCEDURE,PASS,PRIVATE :: bcast_SSK2_MPI_Env_type
      !> @copybrief ParallelEnv::bcast_SDK1_MPI_Env_type
      !> @copydetails ParallelEnv::bcast_SDK1_MPI_Env_type
      PROCEDURE,PASS,PRIVATE :: bcast_SDK1_MPI_Env_type
      !> @copybrief ParallelEnv::bcast_SDK2_MPI_Env_type
      !> @copydetails ParallelEnv::bcast_SDK2_MPI_Env_type
      PROCEDURE,PASS,PRIVATE :: bcast_SDK2_MPI_Env_type
      !> @copybrief ParallelEnv::bcast_SDK3_MPI_Env_type
      !> @copydetails ParallelEnv::bcast_SDK3_MPI_Env_type
      PROCEDURE,PASS,PRIVATE :: bcast_SDK3_MPI_Env_type
      !> @copybrief ParallelEnv::bcast_SSK4_MPI_Env_type
      !> @copydetails ParallelEnv::bcast_SSK4_MPI_Env_type
      PROCEDURE,PASS,PRIVATE :: bcast_SSK4_MPI_Env_type
      !> @copybrief ParallelEnv::bcast_SDK4_MPI_Env_type
      !> @copydetails ParallelEnv::bcast_SDK4_MPI_Env_type
      PROCEDURE,PASS,PRIVATE :: bcast_SDK4_MPI_Env_type
      !>
      GENERIC :: bcast => bcast_SLK0_MPI_Env_type, &
                          bcast_SSK2_MPI_Env_type, &
                          bcast_SDK1_MPI_Env_type, &
                          bcast_SDK2_MPI_Env_type, &
                          bcast_SDK3_MPI_Env_type, &
                          bcast_SSK4_MPI_Env_type, &
                          bcast_SDK4_MPI_Env_type
      !> @copybrief ParallelEnv::allReduceR_MPI_Env_type
      !> @copydetails  ParallelEnv::allReduceR_MPI_Env_type
      PROCEDURE,PASS :: allReduce => allReduceR_MPI_Env_type
      !> @copybrief ParallelEnv::allReduceI_MPI_Env_type
      !> @copydetails  ParallelEnv::allReduceI_MPI_Env_type
      PROCEDURE,PASS :: allReduceI => allReduceI_MPI_Env_type
      !GENERIC :: allReduce => allReduceR_MPI_Env_type,allReduceI_MPI_Env_type
      !> @copybrief ParallelEnv::allReduceMaxR_MPI_Env_type
      !> @copydetails  ParallelEnv::allReduceMaxR_MPI_Env_type
      PROCEDURE,PASS :: allReduceMax => allReduceMaxR_MPI_Env_type
      !> @copybrief ParallelEnv::allReduceMaxI_MPI_Env_type
      !> @copydetails  ParallelEnv::allReduceMaxI_MPI_Env_type
      PROCEDURE,PASS :: allReduceMaxI => allReduceMaxI_MPI_Env_type
      !GENERIC :: allReduceMax => allReduceMaxR_MPI_Env_type,allReduceMaxI_MPI_Env_type
      !> @copybrief ParallelEnv::allReduceMinR_MPI_Env_type
      !> @copydetails  ParallelEnv::allReduceMinR_MPI_Env_type
      PROCEDURE,PASS :: allReduceMin => allReduceMinR_MPI_Env_type
      !> @copybrief ParallelEnv::allReduceMinI_MPI_Env_type
      !> @copydetails  ParallelEnv::allReduceMinI_MPI_Env_type
      PROCEDURE,PASS :: allReduceMinI => allReduceMinI_MPI_Env_type
      !GENERIC :: allReduceMin => allReduceMinR_MPI_Env_type,allReduceMinI_MPI_Env_type
      !> @copybrief ParallelEnv::trueForAll_MPI_Env_type
      !> @copydetails  ParallelEnv::trueForAll_MPI_Env_type
      PROCEDURE,PASS :: trueForAll => trueForAll_MPI_Env_type
      !> @copybrief ParallelEnv::finalize_MPI_Env_type
      !> @copydetails  ParallelEnv::finalize_MPI_Env_type
      PROCEDURE,NOPASS :: finalize => finalize_MPI_Env_type
  ENDTYPE MPI_EnvType

  !> Type describes basic information about OpenMP environment
  TYPE,EXTENDS(ParEnvType) :: OMP_EnvType
!
!List of type bound procedures (methods) for the object
    CONTAINS
      !> @copybrief ParallelEnv::init_OMP_Env_type
      !> @copydetails  ParallelEnv::init_OMP_Env_type
      PROCEDURE,PASS :: init => init_OMP_Env_type
      !> @copybrief ParallelEnv::clear_OMP_Env_type
      !> @copydetails  ParallelEnv::clear_OMP_Env_type
      PROCEDURE,PASS :: clear => clear_OMP_Env_type
  ENDTYPE OMP_EnvType

  !> Type describes parallel environment for neutron transport
  !>
  !> Fairly specific to MPACT
  TYPE :: ParallelEnvType
    !> The environment for the entire program world
    TYPE(MPI_EnvType) :: world
    !> The environment for the entire program world
    TYPE(MPI_EnvType) :: CartGridWorld
    !> The environment for the group owning each energy domain
    TYPE(MPI_EnvType),POINTER :: energy => NULL()
    !> The environment for the group owning each spatial domain
    TYPE(MPI_EnvType),POINTER :: space => NULL()
    !> The environment for the group owning each angle domain
    TYPE(MPI_EnvType),POINTER :: angle => NULL()
    !> The environment for the threads decomposing the rays.
    TYPE(OMP_EnvType),POINTER :: ray => NULL()
!
!List of type bound procedures (methods) for the object
    CONTAINS
      !> @copybrief ParallelEnv::isInit_ParEnvType
      !> @copydetails  ParallelEnv::isInit_ParEnvType
      PROCEDURE,PASS :: isInit => isInit_ParEnvType
      !> @copybrief ParallelEnv::init_ParEnvType
      !> @copydetails  ParallelEnv::init_ParEnvType
      PROCEDURE,PASS :: initialize => init_ParEnvType
      !> @copybrief ParallelEnv::clear_ParEnvType
      !> @copydetails  ParallelEnv::clear_ParEnvType
      PROCEDURE,PASS :: clear => clear_ParEnvType
  ENDTYPE ParallelEnvType

  !> @brief Overloads the assignment operator for the ParallelEnvType.
  !>
  INTERFACE ASSIGNMENT(=)
    !> @copybrief ParallelEnv::assign_ParEnvType
    !> @copydetails ParallelEnv::assign_ParEnvType
    MODULE PROCEDURE assign_ParEnvType
  ENDINTERFACE

  !> Private scratch variable for the mpierr
  INTEGER(SIK) :: mpierr

  INTEGER(SIK),SAVE :: MAX_PE_COMM_ID=1

  !> Module name
  CHARACTER(LEN=*),PARAMETER :: modName='PARALLELENV'

  !> Exception Handler for the module
  TYPE(ExceptionHandlerType),SAVE :: eParEnv
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Function returns initialization status of a ParEnvType @e myPE.
    PURE FUNCTION getInitStat_ParEnvType(myPE) RESULT(bool)
      CLASS(ParEnvType),INTENT(IN) :: myPE
      LOGICAL(SBK) :: bool
      bool=myPE%initstat
    ENDFUNCTION getInitStat_ParEnvType
!
!-------------------------------------------------------------------------------
!> @brief Partitions a continuous range of indices by attempting to evenly
!> divide them among processors
!> @param myPE the parallel environment object
!> @param n1 the starting index for the range of indices (optional)
!> @param n2 the stopping index for the range of indices (optional)
!> @param ipart the partition to obtain return @c istt and @c istp values for
!>        (optional). When not present the rank is used.
!> @param istt return value for processor dependent starting index
!> @param istp return value for processor dependent stopping index
!>
!> If the number of indices cannot be evenly distributed amongst processors
!> then the remainder of indices will be assigned to the processes with ranks
!> 0-nremainder
!>
    SUBROUTINE partition_indices_ParEnvType(myPE,n1,n2,ipart,istt,istp)
      CHARACTER(LEN=*),PARAMETER :: myName='partition_greedy_ParEnvType'
      CLASS(ParEnvType),INTENT(IN) :: myPE
      INTEGER(SIK),INTENT(IN) :: n1
      INTEGER(SIK),INTENT(IN) :: n2
      INTEGER(SIK),INTENT(IN),OPTIONAL :: ipart
      INTEGER(SIK),INTENT(OUT) :: istt
      INTEGER(SIK),INTENT(OUT) :: istp

      INTEGER(SIK) :: myrank,nproc
      INTEGER(SIK) :: nwork,nwork_per_proc,work_rem

      istt=1
      istp=0
      IF(myPE%initstat) THEN
        myrank=myPE%rank
        IF(PRESENT(ipart)) myrank=ipart
        nproc=myPE%nproc
        nwork=n2-n1+1
        IF(myrank < nwork) THEN
          !Evenly divide work on each process
          nwork_per_proc=nwork/nproc

          !Remainder of work (to be assigned to first work_rem processors)
          work_rem=MOD(nwork,nproc)

          !Starting index
          istt=myrank*nwork_per_proc+MIN(myrank,work_rem)+n1

          !Stopping index
          istp=istt+nwork_per_proc-1

          !Adjust for remainder of work
          IF(work_rem > myrank) istp=istp+1

          IF(istt > istp) istp=istt
        ENDIF
      ENDIF
    ENDSUBROUTINE partition_indices_ParEnvType
!
!-------------------------------------------------------------------------------
!> @brief Partition a continuous range of indices based on associated weights.
!> @param myPE the parallel environment object
!> @param n1 the starting index for the range of indices
!> @param n2 the stopping index for the range of indices
!> @param istt return value for processor dependent starting index
!> @param istp return value for processor dependent stopping index
!>
!> If the number of indices cannot be evenly distributed amongst processors
!> then the remainder of indices will be assigned to the processes with ranks
!> 0-nremainder
!>
    SUBROUTINE partition_greedy_ParEnvType(myPE,iwgt,n1,n2,ipart,idxmap)
      CHARACTER(LEN=*),PARAMETER :: myName='partition_greedy_ParEnvType'
      CLASS(ParEnvType),INTENT(IN) :: myPE
      INTEGER(SIK),INTENT(IN) :: iwgt(:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n1
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n2
      INTEGER(SIK),INTENT(IN),OPTIONAL :: ipart
      INTEGER(SIK),ALLOCATABLE,INTENT(INOUT) :: idxmap(:)
      INTEGER(SIK) :: i,j,k,n,iwt,idx,iproc,nidx,pid
      INTEGER(SIK),ALLOCATABLE :: wsum(:),sorted_idx(:,:),tmpwt(:),nwtproc(:)

      IF(PRESENT(n1)) THEN
        i=n1
      ELSE
        i=LBOUND(iwgt,DIM=1)
      ENDIF
      IF(PRESENT(n2)) THEN
        j=n2
      ELSE
        j=UBOUND(iwgt,DIM=1)
      ENDIF
      n=SIZE(iwgt)

      IF(myPE%initstat) THEN
        IF(PRESENT(ipart)) THEN
          pid=ipart
        ELSE
          pid=myPE%rank
        ENDIF
        IF(j >= i .AND. LBOUND(iwgt,DIM=1) <= i .AND. j <= UBOUND(iwgt,DIM=1)) THEN
          IF(0 <= pid .AND. pid < myPE%nproc .AND. myPE%nproc <= n) THEN
            IF(ALLOCATED(idxmap)) DEALLOCATE(idxmap)

            CALL dmallocA(wsum,myPE%nproc)
            CALL dmallocA(nwtproc,myPE%nproc)
            CALL dmallocA(sorted_idx,myPE%nproc,n)
            CALL dmallocA(tmpwt,n)
            tmpwt=iwgt
            wsum=0
            nwtproc=0
            sorted_idx=0

            !Assign the weights for each index into the "bin" (e.g. processor)
            !with the current lowest sum
            DO k=i,j
              !Value and location of maximum weight
              idx=MAXLOC(tmpwt(i:j),DIM=1)
              iwt=tmpwt(idx)


              !Location of minimum sum of weights per proc
              iproc=MINLOC(wsum,DIM=1)

              !Index map for sorted_idx
              nwtproc(iproc)=nwtproc(iproc)+1

              !Update sum and sorted values
              sorted_idx(iproc,nwtproc(iproc))=idx
              wsum(iproc)=wsum(iproc)+iwt
              tmpwt(idx)=0
            ENDDO


            !Assign results to output variable while sorting
            pid=pid+1
            nidx=nwtproc(pid)
            ALLOCATE(idxmap(1:nidx))
            DO k=nidx,1,-1
              idx=MAXLOC(sorted_idx(pid,1:nidx),DIM=1)
              idxmap(k)=sorted_idx(pid,idx)
              sorted_idx(pid,idx)=0
            ENDDO

            !Clear locals
            CALL demallocA(tmpwt)
            CALL demallocA(sorted_idx)
            CALL demallocA(nwtproc)
            CALL demallocA(wsum)
          ELSE
            CALL eParEnv%raiseError(modName//'::'//myName// &
              ' - Illegal value for ipart or too many processors!')
          ENDIF
        ELSE
          CALL eParEnv%raiseError(modName//'::'//myName// &
            ' - Error with dimensions of iwgt, n1, and n2!')
        ENDIF
      ELSE
        CALL eParEnv%raiseError(modName//'::'//myName// &
          ' - Parallel environment is not initialized!')
      ENDIF
    ENDSUBROUTINE partition_greedy_ParEnvType
!
!-------------------------------------------------------------------------------
!> @brief Initializes an MPI environment type object.
    SUBROUTINE init_MPI_Env_type(myPE,PEparam)
      CHARACTER(LEN=*),PARAMETER :: myName='init_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      INTEGER(SIK),INTENT(IN),OPTIONAL :: PEparam
      INTEGER(SIK) :: isinit,icomm
      LOGICAL(SBK) :: allpetsc
      LOGICAL(SBK),ALLOCATABLE :: allpetsc2(:)

      IF(.NOT.myPE%initstat) THEN
        icomm=PE_COMM_NULL
        IF(PRESENT(PEparam)) icomm=PEparam

#ifdef HAVE_MPI
        !Initialize MPI if it has not been initialized
        CALL MPI_Initialized(isinit,mpierr)
        IF(mpierr /= MPI_SUCCESS) CALL eParEnv%raiseError(modName//'::'// &
          myName//' - call to MPI_Initialized returned an error!')

        IF(isinit == 0) THEN
          CALL MPI_Init(mpierr)
          IF(mpierr /= MPI_SUCCESS) CALL eParEnv%raiseError(modName//'::'// &
            myName//' - call to MPI_Init returned an error!')
        ENDIF
#endif

        !Set the communicator
        !Default is comm world
        IF(icomm == PE_COMM_NULL) icomm=PE_COMM_DEFAULT
#ifdef HAVE_MPI
        !Create a duplicate of the passed communicator
        CALL MPI_Comm_dup(icomm,myPE%comm,mpierr)
#else
        !Increment communicator to simulate duplication
        MAX_PE_COMM_ID=MAX_PE_COMM_ID+1
        myPE%comm=MAX_PE_COMM_ID+1
#endif

        !Get Information about the communicator
#ifdef HAVE_MPI
        CALL MPI_Comm_size(myPE%comm,myPE%nproc,mpierr)
        IF(mpierr /= MPI_SUCCESS) CALL eParEnv%raiseError(modName//'::'// &
          myName//' - call to MPI_Comm_size returned an error!')
        CALL MPI_Comm_rank(myPE%comm,myPE%rank,mpierr)
        IF(mpierr /= MPI_SUCCESS) CALL eParEnv%raiseError(modName//'::'// &
          myName//' - call to MPI_Comm_rank returned an error!')
#else
        myPE%nproc=1
        myPE%rank=0
#endif
        IF(myPE%rank == 0) myPE%master=.TRUE.

#ifdef MPACT_HAVE_PETSC
        !check if PETSC has been initialized as well
        CALL PetscInitialized(petsc_isinit,ierr)

        !Insure that PETSc is or is not initialized on all the processes
        !in this communicator
        IF(myPE%nproc > 1) THEN
          ALLOCATE(allpetsc2(0:myPE%nproc-1))
          allpetsc2=.FALSE.
          allpetsc2(myPE%rank)=petsc_isinit
          CALL MPI_Gather(petsc_isinit,1,MPI_LOGICAL, &
                          allpetsc2(myPE%rank),1,MPI_LOGICAL,0,myPE%comm,mpierr)
          IF((ANY(allpetsc2) .AND. .NOT.ALL(allpetsc2)) .AND. myPE%master) THEN
            CALL eParEnv%raiseFatalError(modName//'::'//myName// &
              ' - Something is wrong with your application. '// &
                'PetscInitialized should return either .TRUE. or '// &
                  '.FALSE. for all processes in the communicator.')
          ENDIF
          DEALLOCATE(allpetsc2)
        ENDIF
        !Call PETSc Initialize
        IF(.NOT.petsc_isinit) CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif
        myPE%initstat=.TRUE.
      ENDIF
    ENDSUBROUTINE init_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> Clears an MPI environment type object.
!>
!> If the communicator is not MPI_COMM_WORLD then it is also freed.
    SUBROUTINE clear_MPI_Env_type(myPE)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE

      IF(myPE%initstat) THEN
#ifdef HAVE_MPI
        IF(myPE%comm /= MPI_COMM_WORLD .AND. myPE%comm /= MPI_COMM_SELF) THEN
          CALL MPI_Comm_free(myPE%comm,mpierr) !I think this is collective
          IF(mpierr /= MPI_SUCCESS) CALL eParEnv%raiseError(modName//'::'// &
            myName//' - call to MPI_Comm_free returned an error!')
        ENDIF
#endif
        myPE%comm=-1
        myPE%nproc=-1
        myPE%rank=-1
        myPE%master=.FALSE.
        myPE%initstat=.FALSE.
      ENDIF
    ENDSUBROUTINE clear_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Barrier
    SUBROUTINE barrier_MPI_Env_type(myPE)
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
#ifdef HAVE_MPI
      IF(myPE%initstat) CALL MPI_Barrier(myPE%comm,mpierr)
#endif
    ENDSUBROUTINE barrier_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Gather
    SUBROUTINE gather_SIK0_MPI_Env_type(myPE,sendbuf,recvbuf,root)
      CHARACTER(LEN=*),PARAMETER :: myName='gather_SIK0_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      INTEGER(SIK),INTENT(IN) :: sendbuf
      INTEGER(SIK),INTENT(INOUT) :: recvbuf(:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: root
      INTEGER(SIK) :: rank
      rank=0
      IF(PRESENT(root)) rank=root
      IF(0 <= rank .AND. rank <= myPE%nproc-1) THEN
        IF(rank == myPE%rank) THEN
          IF(SIZE(recvbuf) < myPE%nproc) &
            CALL eParEnv%raiseError(modName//'::'//myName// &
              ' - size of receive buffer is not large enough!')
        ENDIF
#ifdef HAVE_MPI
        CALL MPI_Gather(sendbuf,1,MPI_INTEGER,recvbuf,1,MPI_INTEGER, &
          rank,myPE%comm,mpierr)
#else
        recvbuf(1)=sendbuf
#endif
      ELSE
        CALL eParEnv%raiseError(modName//'::'//myName// &
          ' - Invalid rank!')
      ENDIF
    ENDSUBROUTINE gather_SIK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Gather
    SUBROUTINE gather_SLK0_MPI_Env_type(myPE,sendbuf,recvbuf,root)
      CHARACTER(LEN=*),PARAMETER :: myName='gather_SLK0_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      INTEGER(SLK),INTENT(IN) :: sendbuf
      INTEGER(SLK),INTENT(INOUT) :: recvbuf(:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: root
      INTEGER(SIK) :: rank
      rank=0
      IF(PRESENT(root)) rank=root
      IF(0 <= rank .AND. rank <= myPE%nproc-1) THEN
        IF(rank == myPE%rank) THEN
          IF(SIZE(recvbuf) < myPE%nproc) &
            CALL eParEnv%raiseError(modName//'::'//myName// &
              ' - size of receive buffer is not large enough!')
        ENDIF
#ifdef HAVE_MPI
        CALL MPI_Gather(sendbuf,1,MPI_INTEGER8,recvbuf,1,MPI_INTEGER8, &
          rank,myPE%comm,mpierr)
#else
        recvbuf(1)=sendbuf
#endif
      ELSE
        CALL eParEnv%raiseError(modName//'::'//myName// &
          ' - Invalid rank!')
      ENDIF
    ENDSUBROUTINE gather_SLK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Gather
    SUBROUTINE gather_SLK1_MPI_Env_type(myPE,sendbuf,recvbuf,root)
      CHARACTER(LEN=*),PARAMETER :: myName='gather_SLK1_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      INTEGER(SLK),INTENT(IN) :: sendbuf(:)
      INTEGER(SLK),INTENT(INOUT) :: recvbuf(:,:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: root
      INTEGER(SIK) :: rank,count,i,j,n
      rank=0
      IF(PRESENT(root)) rank=root
      count=SIZE(sendbuf)
      IF(0 <= rank .AND. rank <= myPE%nproc-1) THEN
        IF(rank == myPE%rank) THEN
          IF(SIZE(recvbuf) < myPE%nproc*count) &
            CALL eParEnv%raiseError(modName//'::'//myName// &
              ' - size of receive buffer is not large enough!')
        ENDIF
#ifdef HAVE_MPI
        CALL MPI_Gather(sendbuf,count,MPI_INTEGER8,recvbuf,count, &
          MPI_INTEGER8,rank,myPE%comm,mpierr)
#else
        DO n=1,count
          i=MOD(n-1,SIZE(recvbuf,DIM=1))+1
          j=(n-1)/SIZE(recvbuf,DIM=1)+1
          recvbuf(i,j)=sendbuf(n)
        ENDDO
#endif
      ELSE
        CALL eParEnv%raiseError(modName//'::'//myName// &
          ' - Invalid rank!')
      ENDIF
    ENDSUBROUTINE gather_SLK1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Scatter
    SUBROUTINE scatter_SLK0_MPI_Env_type(myPE,sendbuf,recvbuf,root)
      CHARACTER(LEN=*),PARAMETER :: myName='scatter_SLK0_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      INTEGER(SLK),INTENT(IN) :: sendbuf(:)
      INTEGER(SLK),INTENT(INOUT) :: recvbuf
      INTEGER(SIK),INTENT(IN),OPTIONAL :: root
      INTEGER(SIK) :: rank
      rank=0
      IF(PRESENT(root)) rank=root
      IF(0 <= rank .AND. rank <= myPE%nproc-1) THEN
        IF(rank == myPE%rank) THEN
          IF(SIZE(sendbuf) < myPE%nproc) &
            CALL eParEnv%raiseError(modName//'::'//myName// &
              ' - size of send buffer is not large enough!')
        ENDIF
#ifdef HAVE_MPI
        CALL MPI_Scatter(sendbuf,1,MPI_INTEGER8,recvbuf,1,MPI_INTEGER8, &
          rank,myPE%comm,mpierr)
#else
        recvbuf=sendbuf(1)
#endif
      ELSE
        CALL eParEnv%raiseError(modName//'::'//myName// &
          ' - Invalid rank!')
      ENDIF
    ENDSUBROUTINE scatter_SLK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Scatter
    SUBROUTINE scatter_SLK1_MPI_Env_type(myPE,sendbuf,recvbuf,root)
      CHARACTER(LEN=*),PARAMETER :: myName='scatter_SLK1_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      INTEGER(SLK),INTENT(IN) :: sendbuf(:,:)
      INTEGER(SLK),INTENT(INOUT) :: recvbuf(:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: root
      INTEGER(SIK) :: rank,count,i,j,n
      rank=0
      IF(PRESENT(root)) rank=root
      count=SIZE(recvbuf)
      IF(0 <= rank .AND. rank <= myPE%nproc-1) THEN
        IF(rank == myPE%rank) THEN
          IF(SIZE(sendbuf) < myPE%nproc*count) &
            CALL eParEnv%raiseError(modName//'::'//myName// &
              ' - size of send buffer is not large enough!')
        ENDIF
#ifdef HAVE_MPI
        CALL MPI_Scatter(sendbuf,count,MPI_INTEGER8,recvbuf,count, &
          MPI_INTEGER8,rank,myPE%comm,mpierr)
#else
        DO n=1,count
          i=MOD(n-1,SIZE(sendbuf,DIM=1))+1
          j=(n-1)/SIZE(sendbuf,DIM=1)+1
          recvbuf(n)=sendbuf(i,j)
        ENDDO
#endif
      ELSE
        CALL eParEnv%raiseError(modName//'::'//myName// &
          ' - Invalid rank!')
      ENDIF
    ENDSUBROUTINE scatter_SLK1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief
    SUBROUTINE bcast_SLK0_MPI_Env_type(myPE,buf,root)
      CHARACTER(LEN=*),PARAMETER :: myName='bcast_SLK0_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      INTEGER(SLK),INTENT(IN) :: buf
      INTEGER(SIK),INTENT(IN),OPTIONAL :: root
      INTEGER(SIK) :: rank
      rank=0
      IF(PRESENT(root)) rank=root
      IF(0 <= rank .AND. rank <= myPE%nproc-1) THEN
#ifdef HAVE_MPI
        CALL MPI_Bcast(buf,1,MPI_INTEGER8,rank,myPE%comm,mpierr)
#endif
      ELSE
        CALL eParEnv%raiseError(modName//'::'//myName// &
          ' - Invalid rank!')
      ENDIF
    ENDSUBROUTINE bcast_SLK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief
    SUBROUTINE bcast_SSK2_MPI_Env_type(myPE,buf,root)
      CHARACTER(LEN=*),PARAMETER :: myName='bcast_SSK2_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      REAL(SSK),INTENT(IN) :: buf(:,:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: root
      INTEGER(SIK) :: rank
      rank=0
      IF(PRESENT(root)) rank=root
      IF(0 <= rank .AND. rank <= myPE%nproc-1) THEN
#ifdef HAVE_MPI
        CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL4,rank,myPE%comm,mpierr)
#endif
      ELSE
        CALL eParEnv%raiseError(modName//'::'//myName// &
          ' - Invalid rank!')
      ENDIF
    ENDSUBROUTINE bcast_SSK2_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief
    SUBROUTINE bcast_SDK1_MPI_Env_type(myPE,buf,root)
      CHARACTER(LEN=*),PARAMETER :: myName='bcast_SDK1_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      REAL(SDK),INTENT(IN) :: buf(:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: root
      INTEGER(SIK) :: rank
      rank=0
      IF(PRESENT(root)) rank=root
      IF(0 <= rank .AND. rank <= myPE%nproc-1) THEN
#ifdef HAVE_MPI
        CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL8,rank,myPE%comm,mpierr)
#endif
      ELSE
        CALL eParEnv%raiseError(modName//'::'//myName// &
          ' - Invalid rank!')
      ENDIF
    ENDSUBROUTINE bcast_SDK1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief
    SUBROUTINE bcast_SDK2_MPI_Env_type(myPE,buf,root)
      CHARACTER(LEN=*),PARAMETER :: myName='bcast_SDK2_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      REAL(SDK),INTENT(IN) :: buf(:,:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: root
      INTEGER(SIK) :: rank
      rank=0
      IF(PRESENT(root)) rank=root
      IF(0 <= rank .AND. rank <= myPE%nproc-1) THEN
#ifdef HAVE_MPI
        CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL8,rank,myPE%comm,mpierr)
#endif
      ELSE
        CALL eParEnv%raiseError(modName//'::'//myName// &
          ' - Invalid rank!')
      ENDIF
    ENDSUBROUTINE bcast_SDK2_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief
    SUBROUTINE bcast_SDK3_MPI_Env_type(myPE,buf,root)
      CHARACTER(LEN=*),PARAMETER :: myName='bcast_SDK3_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      REAL(SDK),INTENT(IN) :: buf(:,:,:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: root
      INTEGER(SIK) :: rank
      rank=0
      IF(PRESENT(root)) rank=root
      IF(0 <= rank .AND. rank <= myPE%nproc-1) THEN
#ifdef HAVE_MPI
        CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL8,rank,myPE%comm,mpierr)
#endif
      ELSE
        CALL eParEnv%raiseError(modName//'::'//myName// &
          ' - Invalid rank!')
      ENDIF
    ENDSUBROUTINE bcast_SDK3_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief
    SUBROUTINE bcast_SSK4_MPI_Env_type(myPE,buf,root)
      CHARACTER(LEN=*),PARAMETER :: myName='bcast_SSK4_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      REAL(SSK),INTENT(IN) :: buf(:,:,:,:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: root
      INTEGER(SIK) :: rank
      rank=0
      IF(PRESENT(root)) rank=root
      IF(0 <= rank .AND. rank <= myPE%nproc-1) THEN
#ifdef HAVE_MPI
        CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL4,rank,myPE%comm,mpierr)
#endif
      ELSE
        CALL eParEnv%raiseError(modName//'::'//myName// &
          ' - Invalid rank!')
      ENDIF
    ENDSUBROUTINE bcast_SSK4_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief
    SUBROUTINE bcast_SDK4_MPI_Env_type(myPE,buf,root)
      CHARACTER(LEN=*),PARAMETER :: myName='bcast_SDK4_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      REAL(SDK),INTENT(IN) :: buf(:,:,:,:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: root
      INTEGER(SIK) :: rank
      rank=0
      IF(PRESENT(root)) rank=root
      IF(0 <= rank .AND. rank <= myPE%nproc-1) THEN
#ifdef HAVE_MPI
        CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL8,rank,myPE%comm,mpierr)
#endif
      ELSE
        CALL eParEnv%raiseError(modName//'::'//myName// &
          ' - Invalid rank!')
      ENDIF
    ENDSUBROUTINE bcast_SDK4_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a sum of operation
!> for a real array.
!> @param myPE the MPI parallel environment
!> @param n the number of data elements to communicate
!> @param x the partial sum to be returned as the total sum
!>
!> This routine only performs a sum operation and only for reals.
!>
    SUBROUTINE allReduceR_MPI_Env_type(myPE,n,x)
      CHARACTER(LEN=*),PARAMETER :: myName='allReduce_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(IN) :: myPE
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SRK),INTENT(INOUT) :: x(*)
#ifdef HAVE_MPI
!      REAL(SRK) :: rbuf(n)
      REAL(SRK),ALLOCATABLE :: rbuf(:)
      IF(myPE%initstat) THEN
        ALLOCATE(rbuf(n))
#ifdef DBL

        CALL MPI_Allreduce(x,rbuf,n,MPI_DOUBLE_PRECISION,MPI_SUM, &
          myPE%comm,mpierr)
#else
        CALL MPI_Allreduce(x,rbuf,n,MPI_SINGLE_PRECISION,MPI_SUM, &
          myPE%comm,mpierr)
#endif
        IF(mpierr /= MPI_SUCCESS) THEN
          CALL eParEnv%raiseError(modName//'::'// &
            myName//' - call to MPI_Allreduce returned an error!')
        ELSE
          !Copy the result to the output argument
          CALL BLAS_copy(n,rbuf,1,x,1)
        ENDIF
        DEALLOCATE(rbuf)
      ENDIF
#endif
    ENDSUBROUTINE allReduceR_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a max operation
!> for a real array.
!> @param myPE the MPI parallel environment
!> @param n the number of data elements to communicate
!> @param x the partial array to be returned as the max array
!>
!> This routine only performs a max operation and only for reals.
!>
    SUBROUTINE allReduceMaxR_MPI_Env_type(myPE,n,x)
      CHARACTER(LEN=*),PARAMETER :: myName='allReduce_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(IN) :: myPE
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SRK),INTENT(INOUT) :: x(*)
#ifdef HAVE_MPI
      REAL(SRK) :: rbuf(n)
      IF(myPE%initstat) THEN
#ifdef DBL
        CALL MPI_Allreduce(x,rbuf,n,MPI_DOUBLE_PRECISION,MPI_MAX, &
          myPE%comm,mpierr)
#else
        CALL MPI_Allreduce(x,rbuf,n,MPI_SINGLE_PRECISION,MPI_MAX, &
          myPE%comm,mpierr)
#endif
        IF(mpierr /= MPI_SUCCESS) THEN
          CALL eParEnv%raiseError(modName//'::'// &
            myName//' - call to MPI_AllreduceMax returned an error!')
        ELSE
          !Copy the result to the output argument
          CALL BLAS_copy(n,rbuf,1,x,1)
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE allReduceMaxR_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a min operation
!> for a real array.
!> @param myPE the MPI parallel environment
!> @param n the number of data elements to communicate
!> @param x the partial array to be returned as the min array
!>
!> This routine only performs a min operation and only for reals.
!>
    SUBROUTINE allReduceMinR_MPI_Env_type(myPE,n,x)
      CHARACTER(LEN=*),PARAMETER :: myName='allReduce_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(IN) :: myPE
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SRK),INTENT(INOUT) :: x(*)
#ifdef HAVE_MPI
      REAL(SRK) :: rbuf(n)
      IF(myPE%initstat) THEN
#ifdef DBL
        CALL MPI_Allreduce(x,rbuf,n,MPI_DOUBLE_PRECISION,MPI_MIN, &
          myPE%comm,mpierr)
#else
        CALL MPI_Allreduce(x,rbuf,n,MPI_SINGLE_PRECISION,MPI_MIN, &
          myPE%comm,mpierr)
#endif
        IF(mpierr /= MPI_SUCCESS) THEN
          CALL eParEnv%raiseError(modName//'::'// &
            myName//' - call to MPI_AllreduceMin returned an error!')
        ELSE
          !Copy the result to the output argument
          CALL BLAS_copy(n,rbuf,1,x,1)
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE allReduceMinR_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a sum of operation
!> for an integer array.
!> @param myPE the MPI parallel environment
!> @param n the number of data elements to communicate
!> @param x the partial sum to be returned as the total sum
!>
!> This routine only performs a sum operation and only for integers.
!>
    SUBROUTINE allReduceI_MPI_Env_type(myPE,n,x)
      CHARACTER(LEN=*),PARAMETER :: myName='allReduce_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(IN) :: myPE
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(INOUT) :: x(*)
#ifdef HAVE_MPI
      INTEGER(SIK) :: rbuf(n)
      IF(myPE%initstat) THEN
        CALL MPI_Allreduce(x,rbuf,n,MPI_INTEGER,MPI_SUM, &
          myPE%comm,mpierr)
        IF(mpierr /= MPI_SUCCESS) THEN
          CALL eParEnv%raiseError(modName//'::'// &
            myName//' - call to MPI_Allreduce returned an error!')
        ELSE
          !Copy the result to the output argument
          !CALL BLAS_copy(n,rbuf,1,x,1)
          x(1:n)=rbuf ! No BLAS_copy for integers
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE allReduceI_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a max operation
!> for an integer array.
!> @param myPE the MPI parallel environment
!> @param n the number of data elements to communicate
!> @param x the partial array to be returned as the max array
!>
!> This routine only performs a max operation and only for integers.
!>
    SUBROUTINE allReduceMaxI_MPI_Env_type(myPE,n,x)
      CHARACTER(LEN=*),PARAMETER :: myName='allReduce_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(IN) :: myPE
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(INOUT) :: x(*)
#ifdef HAVE_MPI
      INTEGER(SIK) :: rbuf(n)
      IF(myPE%initstat) THEN
        CALL MPI_Allreduce(x,rbuf,n,MPI_INTEGER,MPI_MAX, &
          myPE%comm,mpierr)
        IF(mpierr /= MPI_SUCCESS) THEN
          CALL eParEnv%raiseError(modName//'::'// &
            myName//' - call to MPI_AllreduceMax returned an error!')
        ELSE
          !Copy the result to the output argument
          !CALL BLAS_copy(n,rbuf,1,x,1)
          x(1:n)=rbuf ! No BLAS_copy for integers
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE allReduceMaxI_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a min operation
!> for an integer array.
!> @param myPE the MPI parallel environment
!> @param n the number of data elements to communicate
!> @param x the partial array to be returned as the min array
!>
!> This routine only performs a min operation and only for integers.
!>
    SUBROUTINE allReduceMinI_MPI_Env_type(myPE,n,x)
      CHARACTER(LEN=*),PARAMETER :: myName='allReduce_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(IN) :: myPE
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(INOUT) :: x(*)
#ifdef HAVE_MPI
      INTEGER(SIK) :: rbuf(n)
      IF(myPE%initstat) THEN
        CALL MPI_Allreduce(x,rbuf,n,MPI_INTEGER,MPI_MIN, &
          myPE%comm,mpierr)
        IF(mpierr /= MPI_SUCCESS) THEN
          CALL eParEnv%raiseError(modName//'::'// &
            myName//' - call to MPI_AllreduceMin returned an error!')
        ELSE
          !Copy the result to the output argument
          !CALL BLAS_copy(n,rbuf,1,x,1)
          x(1:n)=rbuf ! No BLAS_copy for integers
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE allReduceMinI_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a logical and
!> operation for a scalar logical.
!> @param myPE the MPI parallel environment
!> @param n the number of data elements to communicate
!> @param x the partial sum to be returned as the total sum
!>
!> This routine only performs a sum operation and only for reals.
!>
    SUBROUTINE trueForAll_MPI_Env_type(myPE,lstat)
      CHARACTER(LEN=*),PARAMETER :: myName='trueForAll_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      LOGICAL(SBK),INTENT(INOUT) :: lstat
#ifdef HAVE_MPI
      LOGICAL(SBK) :: lrbuf
      IF(myPE%initstat) THEN
        CALL MPI_Allreduce(lstat,lrbuf,1,MPI_LOGICAL,MPI_LAND, &
          myPE%comm,mpierr)
        IF(mpierr /= MPI_SUCCESS) THEN
          CALL eParEnv%raiseError(modName//'::'// &
            myName//' - call to MPI_Allreduce returned an error!')
        ELSE
          lstat=lrbuf
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE trueForAll_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Finalize
!>
    SUBROUTINE finalize_MPI_Env_type()
#ifdef HAVE_MPI
      CALL MPI_Finalize(mpierr)
#endif
    ENDSUBROUTINE finalize_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Function returns initialization status of an OMP_EnvType @e myPE.
!>
    PURE FUNCTION getInitStat_OMP_Env_type(myPE) RESULT(bool)
      CLASS(OMP_EnvType),INTENT(IN) :: myPE
      LOGICAL(SBK) :: bool
      bool=myPE%initstat
    ENDFUNCTION getInitStat_OMP_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Initializes an OpenMP environment type object.
!>
    SUBROUTINE init_OMP_Env_type(myPE,PEparam)
      CLASS(OMP_EnvType),INTENT(INOUT) :: myPE
      INTEGER(SIK),INTENT(IN),OPTIONAL :: PEparam
      myPE%nproc=1
      myPE%rank=0
      myPE%master=.TRUE.
!$    IF(PRESENT(PEparam)) THEN
!$      IF(PEparam > omp_get_max_threads()) THEN
!$        myPE%nproc=omp_get_max_threads()
!$      ELSEIF(PEparam == 0) THEN
!$        myPE%nproc=omp_get_num_threads()
!$      ELSE
!$        myPE%nproc=MAX(1,PEparam)
!$      ENDIF
!$    ENDIF
      myPE%initStat=.TRUE.
    ENDSUBROUTINE init_OMP_Env_type
!
!-------------------------------------------------------------------------------
!> Clears the OpenMP environment type object
!>
    SUBROUTINE clear_OMP_Env_type(myPE)
      CLASS(OMP_EnvType),INTENT(INOUT) :: myPE
      myPE%nproc=-1
      myPE%rank=-1
      myPE%master=.FALSE.
      myPE%initStat=.FALSE.
    ENDSUBROUTINE clear_OMP_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Initializes an OpenMP environment type object.
    SUBROUTINE init_ParEnvType(myPE,commWorld,nspace,nenergy,nangle,nthreads)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ParEnvType'
      LOGICAL(SBK),DIMENSION(3),PARAMETER :: isPeriodic=(/.FALSE.,.FALSE.,.FALSE./)
      CLASS(ParallelEnvType),INTENT(INOUT) :: myPE
      INTEGER(SIK),INTENT(IN) :: commWorld
      INTEGER(SIK),INTENT(IN) :: nspace
      INTEGER(SIK),INTENT(IN) :: nangle
      INTEGER(SIK),INTENT(IN) :: nenergy
      INTEGER(SIK),INTENT(IN) :: nthreads
      CHARACTER(LEN=12) :: smpierr, nproc, selproc
      INTEGER(SIK) :: nerror,tmpcomm,commDims(3)
      LOGICAL(SBK) :: activeCommDim(3)

      nerror=eParEnv%getCounter(EXCEPTION_ERROR)
      CALL myPE%world%init(commWorld)

      IF(nspace < 1) CALL eParEnv%raiseError(modName//'::'//myName// &
        ' - input nspace is less than 1!')
      IF(nangle < 1) CALL eParEnv%raiseError(modName//'::'//myName// &
        ' - input nangle is less than 1!')
      IF(nenergy < 1) CALL eParEnv%raiseError(modName//'::'//myName// &
        ' - input nenergy is less than 1!')
      IF(nthreads < 1) CALL eParEnv%raiseError(modName//'::'//myName// &
        ' - input nthreads is less than 1!')
      WRITE(nproc,'(i12)') myPE%world%nproc
      WRITE(selproc,'(i12)') nenergy*nspace*nangle
      IF(myPE%world%nproc < nenergy*nspace*nangle) &
        CALL eParEnv%raiseError(modName//'::'//myName//' - Number of '// &
          'available MPI processes ('//TRIM(ADJUSTL(nproc))//') is less than '// &
          'specified in the input ('//TRIM(ADJUSTL(selproc))//')!')
      IF(myPE%world%nproc > nenergy*nspace*nangle) &
        CALL eParEnv%raiseError(modName//'::'//myName//' - Number of '// &
          'available MPI processes ('//TRIM(ADJUSTL(nproc))//') is more than '// &
          'specified in the input ('//TRIM(ADJUSTL(selproc))//')!')

      IF(nerror == eParEnv%getCounter(EXCEPTION_ERROR)) THEN
        commDims(1)=nspace
        commDims(2)=nangle
        commDims(3)=nenergy

#ifdef HAVE_MPI
        !Create Virtual Cartesian Grid Topology from communicator
        CALL MPI_Cart_create(myPE%world%comm,3,commDims,isPeriodic,.TRUE., &
          tmpcomm,mpierr)

        IF(mpierr == MPI_SUCCESS .AND. tmpcomm /= MPI_COMM_NULL) THEN
          !Setup MPI Env object for the virtual topology
          CALL myPE%CartGridWorld%init(tmpcomm)

          !Setup Communicator for Spatial Decomposition
          activeCommDim=.FALSE.
          activeCommDim(1)=.TRUE.
          CALL MPI_Cart_sub(myPE%CartGridWorld%comm,activeCommDim,tmpcomm,mpierr)
          IF(mpierr == MPI_SUCCESS) THEN
            ALLOCATE(myPE%space)
            CALL myPE%space%init(tmpcomm)
          ELSE
            WRITE(smpierr,'(i12)') mpierr; smpierr=ADJUSTL(smpierr)
            CALL eParEnv%raiseError(modName//'::'//myName// &
              ' - Unexpected error creating MPI communicator for spatial '// &
                'decomp., mpierr='//TRIM(smpierr)//'!')
          ENDIF

          !Setup Communicator for Angular Decomposition
          activeCommDim(1)=.FALSE.
          activeCommDim(2)=.TRUE.
          CALL MPI_Cart_sub(myPE%CartGridWorld%comm,activeCommDim,tmpcomm,mpierr)
          IF(mpierr == MPI_SUCCESS) THEN
            ALLOCATE(myPE%angle)
            CALL myPE%angle%init(tmpcomm)
          ELSE
            WRITE(smpierr,'(i12)') mpierr; smpierr=ADJUSTL(smpierr)
            CALL eParEnv%raiseError(modName//'::'//myName// &
              ' - Unexpected error creating MPI communicator for angular '// &
                'decomp., mpierr='//TRIM(smpierr)//'!')
          ENDIF

          !Setup Communicator for Energy Decomposition
          activeCommDim(2)=.FALSE.
          activeCommDim(3)=.TRUE.
          CALL MPI_Cart_sub(myPE%CartGridWorld%comm,activeCommDim,tmpcomm,mpierr)
          IF(mpierr == MPI_SUCCESS) THEN
            ALLOCATE(myPE%energy)
            CALL myPE%energy%init(tmpcomm)
          ELSE
            WRITE(smpierr,'(i12)') mpierr; smpierr=ADJUSTL(smpierr)
            CALL eParEnv%raiseError(modName//'::'//myName// &
              ' - Unexpected error creating MPI communicator for energy '// &
                'decomp., mpierr='//TRIM(smpierr)//'!')
          ENDIF

          !Setup Ray decomposition
          ALLOCATE(myPE%ray); CALL myPE%ray%init(nthreads)
        ELSE
          WRITE(smpierr,'(i12)') mpierr; smpierr=ADJUSTL(smpierr)
          CALL eParEnv%raiseError(modName//'::'//myName// &
            ' - MPI error when trying to create cartesian grid  '// &
              'virtual topology, mpierr='//TRIM(smpierr)//'!')
        ENDIF
#else
        CALL myPE%world%init(commWorld)
        ALLOCATE(myPE%space); myPE%space=myPE%world
        ALLOCATE(myPE%angle); myPE%angle=myPE%world
        ALLOCATE(myPE%energy); myPE%energy=myPE%world
        ALLOCATE(myPE%ray); CALL myPE%ray%init(nthreads)
#endif
      ENDIF
    ENDSUBROUTINE init_ParEnvType
!
!-------------------------------------------------------------------------------
!> Clears the parallel environment type object
!>
   SUBROUTINE clear_ParEnvType(myPE)
      CLASS(ParallelEnvType),INTENT(INOUT) :: myPE

      IF(ASSOCIATED(myPE%ray)) DEALLOCATE(myPE%ray)
      IF(ASSOCIATED(myPE%energy)) THEN
        CALL myPE%energy%clear(); DEALLOCATE(myPE%energy)
      ENDIF
      IF(ASSOCIATED(myPE%angle)) THEN
        CALL myPE%angle%clear(); DEALLOCATE(myPE%angle)
      ENDIF
      IF(ASSOCIATED(myPE%space)) THEN
        CALL myPE%space%clear(); DEALLOCATE(myPE%space)
      ENDIF
      CALL myPE%CartGridWorld%clear()
      CALL myPE%world%clear()
    ENDSUBROUTINE clear_ParEnvType
!
!-------------------------------------------------------------------------------
!> Returns initialization status of the parallel environment type object
!>
    PURE FUNCTION isInit_ParEnvType(myPE) RESULT(initStat)
      CLASS(ParallelEnvType),INTENT(IN) :: myPE
      LOGICAL(SBK) :: initStat

      initStat=.FALSE.

      IF(.NOT.myPE%world%initStat) RETURN
      IF(.NOT.ASSOCIATED(myPE%space)) RETURN
      IF(.NOT.myPE%space%initStat) RETURN
      IF(.NOT.ASSOCIATED(myPE%angle)) RETURN
      IF(.NOT.myPE%angle%initStat) RETURN
      IF(.NOT.ASSOCIATED(myPE%energy)) RETURN
      IF(.NOT.myPE%energy%initStat) RETURN
      IF(.NOT.ASSOCIATED(myPE%ray)) RETURN
      IF(.NOT.myPE%ray%initStat) RETURN

      initStat=.TRUE.

    ENDFUNCTION isInit_ParEnvType
!
!-------------------------------------------------------------------------------
!> @brief Overloaded assignment for ParEnvType
!> @param pe1 the left hand side of assignment operator
!> @param pe2 the right hand side of assignment operator
!>
!> Performs a deep copy. This is to avoid undefined behavior of association
!> of pointer attributes.
!>
    SUBROUTINE assign_ParEnvType(pe1,pe2)
      TYPE(ParallelEnvType),INTENT(INOUT) :: pe1
      TYPE(ParallelEnvType),INTENT(IN) :: pe2

      CALL clear_ParEnvType(pe1)
      CALL pe1%world%init(pe2%world%comm)
      CALL pe1%CartGridWorld%init(pe2%CartGridWorld%comm)
      IF(ASSOCIATED(pe2%energy)) THEN
        ALLOCATE(pe1%energy)
        CALL pe1%energy%init(pe2%energy%comm)
      ENDIF
      IF(ASSOCIATED(pe2%space)) THEN
        ALLOCATE(pe1%space)
        CALL pe1%space%init(pe2%space%comm)
      ENDIF
      IF(ASSOCIATED(pe2%angle)) THEN
        ALLOCATE(pe1%angle)
        CALL pe1%angle%init(pe2%angle%comm)
      ENDIF
      IF(ASSOCIATED(pe2%ray)) THEN
        ALLOCATE(pe1%ray)
        pe1%ray=pe2%ray
      ENDIF
    ENDSUBROUTINE assign_ParEnvType
!
ENDMODULE ParallelEnv

