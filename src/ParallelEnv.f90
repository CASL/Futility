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
!$ USE OMP_LIB

  IMPLICIT NONE
  PRIVATE
  
#ifdef HAVE_MPI
  INCLUDE 'mpif.h' 
#endif

  PUBLIC :: MPI_EnvType
  PUBLIC :: OMP_EnvType
  PUBLIC :: ParallelEnvType
  PUBLIC :: eParEnv
  
  !> Type describes basic information for MPI environment
  TYPE :: MPI_EnvType
    !> Logical with initialization status
    LOGICAL(SBK),PRIVATE :: initstat=.FALSE.
    !> Fortran integer ID for the communicator
    INTEGER(SIK) :: comm=-1
    !> The number of processors in the communicator
    INTEGER(SIK) :: nproc=-1
    !> The rank of the processor within the communicator
    INTEGER(SIK) :: rank=-1
    !> Whether or not this processor is the master.
    LOGICAL(SBK) :: master=.FALSE.
!
!List of type bound procedures (methods) for the object
    CONTAINS
      !> @copybrief ParallelEnv::getInitStat_MPI_Env_type
      !> @copydetails  ParallelEnv::getInitStat_MPI_Env_type
      PROCEDURE,PASS :: isInit => getInitStat_MPI_Env_type
      !> @copybrief ParallelEnv::init_MPI_Env_type
      !> @copydetails  ParallelEnv::init_MPI_Env_type
      PROCEDURE,PASS :: initialize => init_MPI_Env_type
      !> @copybrief ParallelEnv::partition_indeces_MPI_Env_Type
      !> @copydetails  ParallelEnv::partition_indeces_MPI_Env_Type
      PROCEDURE,PASS :: partition => partition_indeces_MPI_Env_Type
      !> @copybrief ParallelEnv::clear_MPI_Env_type
      !> @copydetails  ParallelEnv::clear_MPI_Env_type
      PROCEDURE,PASS :: clear => clear_MPI_Env_type
      !> @copybrief ParallelEnv::barrier_MPI_Env_type
      !> @copydetails  ParallelEnv::barrier_MPI_Env_type
      PROCEDURE,PASS :: barrier => barrier_MPI_Env_type
      !> @copybrief ParallelEnv::allReduce_MPI_Env_type
      !> @copydetails  ParallelEnv::allReduce_MPI_Env_type
      PROCEDURE,PASS :: allReduce => allReduce_MPI_Env_type
      !> @copybrief ParallelEnv::finalize_MPI_Env_type
      !> @copydetails  ParallelEnv::finalize_MPI_Env_type
      PROCEDURE,NOPASS :: finalize => finalize_MPI_Env_type
  ENDTYPE MPI_EnvType

  !> Type describes basic information about OpenMP environment
  TYPE :: OMP_EnvType
    !> Logical with initialization status
    LOGICAL(SBK),PRIVATE :: initstat=.FALSE.
    !> The number of threads in the OpenMP section
    INTEGER(SIK) :: nthread=-1
    !> The rank of the thread
    INTEGER(SIK) :: rank=-1
    !> Whether or not the the thread is the master
    LOGICAL(SBK) :: master=.FALSE.
!
!List of type bound procedures (methods) for the object
    CONTAINS
      !> @copybrief ParallelEnv::getInitStat_OMP_Env_type
      !> @copydetails  ParallelEnv::getInitStat_OMP_Env_type
      PROCEDURE,PASS :: isInit => getInitStat_OMP_Env_type
      !> @copybrief ParallelEnv::init_OMP_Env_type
      !> @copydetails  ParallelEnv::init_OMP_Env_type
      PROCEDURE,PASS :: initialize => init_OMP_Env_type
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
  
  !> Private scratch variable for the mpierr
  INTEGER(SIK) :: mpierr
  
  !> Module name
  CHARACTER(LEN=11),PARAMETER :: modName='PARALLELENV'
  
  !> Exception Handler for the module
  TYPE(ExceptionHandlerType),POINTER,SAVE :: eParEnv => NULL()
!
!===============================================================================      
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Partitions a continuous range of indeces by attempting to evenly 
!> divide them among processors
!> @param myMPI the mpi environment object
!> @param n1 the starting index for the range of indeces
!> @param n2 the stopping index for the range of indeces
!> @param istt return value for processor dependent starting index
!> @param istp return value for processor dependent stopping index
!>
!> If the number of indeces cannot be evenly distributed amongst processors
!> then the remainder of indeces will be assigned to the processes with ranks
!> 0-nremainder
    PURE SUBROUTINE partition_indeces_MPI_Env_Type(myMPI,n1,n2,istt,istp,ipart)
      CLASS(MPI_EnvType),INTENT(IN) :: myMPI
      INTEGER(SIK),INTENT(IN) :: n1
      INTEGER(SIK),INTENT(IN) :: n2
      INTEGER(SIK),INTENT(OUT) :: istt
      INTEGER(SIK),INTENT(OUT) :: istp
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: ipart
      
      INTEGER(SIK) :: myrank,nproc
      INTEGER(SIK) :: nwork,nwork_per_proc,work_rem
      
      istt=1
      istp=0
      IF(myMPI%initstat) THEN
        myrank=myMPI%rank
        IF(PRESENT(ipart)) myrank=ipart
        nproc=myMPI%nproc
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
    ENDSUBROUTINE partition_indeces_MPI_Env_Type
!
!-------------------------------------------------------------------------------
!> @brief Function returns initialization status of an MPI_EnvType @e myPE.
    PURE FUNCTION getInitStat_MPI_Env_type(myPE) RESULT(bool)
      CLASS(MPI_EnvType),INTENT(IN) :: myPE
      LOGICAL(SBK) :: bool
      bool=myPE%initstat
    ENDFUNCTION getInitStat_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Initializes an MPI environment type object.
    SUBROUTINE init_MPI_Env_type(myPE,icomm)
      CHARACTER(LEN=17),PARAMETER :: myName='init_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      INTEGER(SIK),INTENT(IN) :: icomm
      INTEGER(SIK) :: isinit
      LOGICAL(SBK) :: localalloc
      
      IF(.NOT.myPE%initstat) THEN
        localalloc=.FALSE.
        IF(.NOT.ASSOCIATED(eParEnv)) THEN
          ALLOCATE(eParEnv)
          localalloc=.TRUE.
        ENDIF
        
#ifdef HAVE_MPI
        CALL MPI_Initialized(isinit,mpierr)
        IF(mpierr /= MPI_SUCCESS) CALL eParEnv%raiseError(modName//'::'// &
          myName//' - call to MPI_Initialized returned an error!')
#else
        isinit=0
#endif

        !Set the communicator
        IF(isinit == 0) THEN
#ifdef HAVE_MPI
          CALL MPI_Init(mpierr)
          IF(mpierr /= MPI_SUCCESS) CALL eParEnv%raiseError(modName//'::'// &
            myName//' - call to MPI_Init returned an error!')
          myPE%comm=MPI_COMM_WORLD
#else
          myPE%comm=1
#endif
        ELSE
          myPE%comm=icomm
        ENDIF
      
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
        IF(localalloc) DEALLOCATE(eParEnv)
        myPE%initstat=.TRUE.
      ENDIF
    ENDSUBROUTINE init_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> Clears an MPI environment type object.
!>
!> If the communicator is not MPI_COMM_WORLD then it is also freed.
    SUBROUTINE clear_MPI_Env_type(myPE)
      CHARACTER(LEN=18),PARAMETER :: myName='clear_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      LOGICAL(SBK) :: localalloc
      
      IF(myPE%initstat) THEN
#ifdef HAVE_MPI
        localalloc=.FALSE.
        IF(.NOT.ASSOCIATED(eParEnv)) THEN
          ALLOCATE(eParEnv)
          localalloc=.TRUE.
        ENDIF
        IF(myPE%comm /= MPI_COMM_WORLD) THEN
          CALL MPI_Comm_free(myPE%comm,mpierr) !I think this is collective
          IF(mpierr /= MPI_SUCCESS) CALL eParEnv%raiseError(modName//'::'// &
            myName//' - call to MPI_Comm_free returned an error!')
        ENDIF
        IF(localalloc) DEALLOCATE(eParEnv)
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
!> @brief Wrapper routine calls MPI_Allreduce
!> @param myPE the MPI parallel environment 
!> @param n the number of data elements to commincate
!> @param x the partial sum to be returned as the total sum
!>
!> This routine only performs a sum operation and only for reals.
!>
    SUBROUTINE allReduce_MPI_Env_type(myPE,n,x)
      CHARACTER(LEN=*),PARAMETER :: myName='allReduce_MPI_Env_type'
      CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SRK),INTENT(INOUT) :: x(*)
      REAL(SRK) :: rbuf(n)
#ifdef HAVE_MPI
      IF(myPE%initstat) THEN
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
      ENDIF
#endif
    ENDSUBROUTINE allReduce_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Finalize
    SUBROUTINE finalize_MPI_Env_type()
#ifdef HAVE_MPI
      CALL MPI_Finalize(mpierr)
#endif
    ENDSUBROUTINE finalize_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Function returns initialization status of an OMP_EnvType @e myPE.
    PURE FUNCTION getInitStat_OMP_Env_type(myPE) RESULT(bool)
      CLASS(OMP_EnvType),INTENT(IN) :: myPE
      LOGICAL(SBK) :: bool
      bool=myPE%initstat
    ENDFUNCTION getInitStat_OMP_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Initializes an OpenMP environment type object.
    SUBROUTINE init_OMP_Env_type(myPE,uthreads)
      CLASS(OMP_EnvType),INTENT(INOUT) :: myPE
      INTEGER(SIK),INTENT(IN),OPTIONAL :: uthreads
      myPE%nthread=1
      myPE%rank=0
      myPE%master=.TRUE.
!$    IF(PRESENT(uthreads)) THEN
!$      IF(uthreads > omp_get_max_threads()) THEN
!$        myPE%nthread=omp_get_max_threads()
!$      ELSE
!$        myPE%nthread=MAX(1,uthreads)
!$      ENDIF
!$    ENDIF
      myPE%initStat=.TRUE.
    ENDSUBROUTINE init_OMP_Env_type
!
!-------------------------------------------------------------------------------
!> Clears the OpenMP environment type object
    SUBROUTINE clear_OMP_Env_type(myPE)
      CLASS(OMP_EnvType),INTENT(INOUT) :: myPE
      myPE%nthread=-1
      myPE%rank=-1
      myPE%master=.FALSE.
      myPE%initStat=.FALSE.
    ENDSUBROUTINE clear_OMP_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Initializes an OpenMP environment type object.
    SUBROUTINE init_ParEnvType(myPE,icomm,nspace,nenergy,nangle,nthreads)
      CHARACTER(LEN=15),PARAMETER :: myName='init_ParEnvType'
      CLASS(ParallelEnvType),INTENT(INOUT) :: myPE
      INTEGER(SIK),INTENT(IN) :: icomm
      INTEGER(SIK),INTENT(IN) :: nspace
      INTEGER(SIK),INTENT(IN) :: nangle
      INTEGER(SIK),INTENT(IN) :: nenergy
      INTEGER(SIK),INTENT(IN) :: nthreads
      INTEGER(SIK) :: icolor,ikey,ngrp,subcomm,nerror
      LOGICAL(SBK) :: localalloc

#ifdef HAVE_MPI
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eParEnv)) THEN
        ALLOCATE(eParEnv)
        localalloc=.TRUE.
      ENDIF
      nerror=eParEnv%getCounter(EXCEPTION_ERROR)
      CALL myPE%world%initialize(icomm)
      IF(nspace < 1) CALL eParEnv%raiseError(modName//'::'//myName// &
        ' - input nspace is less than 1!')
      IF(nangle < 1) CALL eParEnv%raiseError(modName//'::'//myName// &
        ' - input nangle is less than 1!')
      IF(nenergy < 1) CALL eParEnv%raiseError(modName//'::'//myName// &
        ' - input nenergy is less than 1!')
      IF(nthreads < 1) CALL eParEnv%raiseError(modName//'::'//myName// &
        ' - input nthreads is less than 1!')
!      IF(nenergy*nspace*nangle /= myPE%world%nproc) &
!        CALL eParEnv%raiseError(modName//'::'//myName//' - Number of '// &
!          'processors specified does not equal number of available processors!')
!      IF(nspace > 0) THEN
!        IF(nenergy*nangle /= myPE%world%nproc/nspace) &
!          CALL eParEnv%raiseError(modName//'::'//myName//' - Number of '// &
!            'processors per spatial domain does not equal number of '// &
!              'available processors in a spatial domain!')
!      ENDIF
!      IF(nspace > 0 .AND. nangle > 0) THEN
!        IF(nangle /= myPE%world%nproc/(nspace*nenergy)) &
!           CALL eParEnv%raiseError(modName//'::'//myName//' - Number of '// &
!            'processors per energy domain does not equal number of '// & 
!              'available processors in an energy domain!')
!      ENDIF
      IF(nerror == eParEnv%getCounter(EXCEPTION_ERROR)) THEN
        !Setup Communicator for Spatial Decomposition
        IF(.NOT.ASSOCIATED(myPE%space)) ALLOCATE(myPE%space)
        IF(.NOT.myPE%space%initstat) THEN
          ngrp=myPE%world%nproc/nspace
          icolor=MOD(myPE%world%rank,ngrp)+1
          ikey=myPE%world%rank/ngrp+1
          CALL MPI_Comm_split(myPE%world%comm,icolor,ikey,subcomm,mpierr)
          IF(mpierr /= MPI_SUCCESS) CALL eParEnv%raiseError(modName//'::'//myName// &
            ' - MPI_Comm_split returned an error when creating space comm!')
          CALL myPE%space%initialize(subcomm)
        ENDIF
        
        !Setup Communicator for Energy Decomposition
        IF(.NOT.ASSOCIATED(myPE%energy)) ALLOCATE(myPE%energy)
        IF(.NOT.myPE%energy%initstat) THEN
          ngrp=myPE%world%nproc/nenergy
          icolor=MOD(myPE%world%rank,ngrp)+1
          ikey=myPE%world%rank/ngrp+1
          CALL MPI_Comm_split(myPE%world%comm,icolor,ikey,subcomm,mpierr)
          IF(mpierr /= MPI_SUCCESS) CALL eParEnv%raiseError(modName//'::'//myName// &
            ' - MPI_Comm_split returned an error when creating angle comm!')
          CALL myPE%energy%initialize(subcomm)
        ENDIF
        
        !Setup Communicator for Angular Decomposition
        IF(.NOT.ASSOCIATED(myPE%angle)) ALLOCATE(myPE%angle)
        IF(.NOT.myPE%angle%initstat) THEN
          !ngrp=myPE%world%nproc/nangle
          !icolor=MOD(myPE%world%rank,ngrp)+1
          !ikey=myPE%world%rank/ngrp+1
          icolor=myPE%space%rank
          ikey=MOD(myPE%world%rank,myPE%space%nproc)+1
          CALL MPI_Comm_split(myPE%world%comm,icolor,ikey,subcomm,mpierr)
          IF(mpierr /= MPI_SUCCESS) CALL eParEnv%raiseError(modName//'::'//myName// &
            ' - MPI_Comm_split returned an error when creating angle comm!')
          CALL myPE%angle%initialize(subcomm)
        ENDIF
        
        !Setup Ray decomposition
        IF(.NOT.ASSOCIATED(myPE%ray)) THEN
          ALLOCATE(myPE%ray); CALL myPE%ray%initialize(nthreads)
        ENDIF
      ENDIF
      IF(localalloc) DEALLOCATE(eParEnv)
#else
      CALL myPE%world%initialize(icomm)
      IF(.NOT.ASSOCIATED(myPE%space)) THEN
        ALLOCATE(myPE%space); myPE%space=myPE%world
      ENDIF
      IF(.NOT.ASSOCIATED(myPE%angle)) THEN
        ALLOCATE(myPE%angle); myPE%angle=myPE%world
      ENDIF
      IF(.NOT.ASSOCIATED(myPE%energy)) THEN
        ALLOCATE(myPE%energy); myPE%energy=myPE%world
      ENDIF
      IF(.NOT.ASSOCIATED(myPE%ray)) THEN
        ALLOCATE(myPE%ray); CALL myPE%ray%initialize(nthreads)
      ENDIF
#endif
    ENDSUBROUTINE init_ParEnvType
!
!-------------------------------------------------------------------------------
!> Clears the parallel environment type object
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
      CALL myPE%world%clear()
    ENDSUBROUTINE clear_ParEnvType
!
!-------------------------------------------------------------------------------
!> Returns initialization status of the parallel environment type object
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
ENDMODULE ParallelEnv
