!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility Module defines types describing the parallel run time
!> environment.
!>
!> @par EXAMPLE
!> @code
!>
!> @endcode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ParallelEnv
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE ExceptionHandler
USE BLAS
USE trilinos_interfaces
USE Allocs
!$ USE OMP_LIB

IMPLICIT NONE
PRIVATE

#ifdef HAVE_MPI

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
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

!$  INTEGER :: max_threads_requested=0

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

    !> @copybrief ParallelEnv::recv_CHAR_MPI_Env_type
    !> @copydetails ParallelEnv::recv_CHAR_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: recv_CHAR_MPI_Env_type
    !> @copybrief ParallelEnv::recv_REAL_MPI_Env_type
    !> @copydetails ParallelEnv::recv_REAL_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: recv_REAL_MPI_Env_type
    !> @copybrief ParallelEnv::recv_INT_MPI_Env_type
    !> @copydetails ParallelEnv::recv_INT_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: recv_INT_MPI_Env_type
    !> @copybrief ParallelEnv::recv_REAL1_MPI_Env_type
    !> @copydetails ParallelEnv::recv_REAL1_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: recv_REAL1_MPI_Env_type
    !> @copybrief ParallelEnv::recv_INT1_MPI_Env_type
    !> @copydetails ParallelEnv::recv_INT1_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: recv_INT1_MPI_Env_type
    GENERIC :: recv => recv_CHAR_MPI_Env_type, &
        recv_REAL_MPI_Env_type, &
        recv_INT_MPI_Env_type, &
        recv_REAL1_MPI_Env_type, &
        recv_INT1_MPI_Env_type

    !> @copybrief ParallelEnv::send_CHAR_MPI_Env_type
    !> @copydetails ParallelEnv::send_CHAR_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: send_CHAR_MPI_Env_type
    !> @copybrief ParallelEnv::send_REAL_MPI_Env_type
    !> @copydetails ParallelEnv::send_REAL_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: send_REAL_MPI_Env_type
    !> @copybrief ParallelEnv::send_INT_MPI_Env_type
    !> @copydetails ParallelEnv::send_INT_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: send_INT_MPI_Env_type
    !> @copybrief ParallelEnv::send_REAL1_MPI_Env_type
    !> @copydetails ParallelEnv::send_REAL1_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: send_REAL1_MPI_Env_type
    !> @copybrief ParallelEnv::send_INT1_MPI_Env_type
    !> @copydetails ParallelEnv::send_INT1_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: send_INT1_MPI_Env_type
    GENERIC :: send => send_CHAR_MPI_Env_type, &
        send_REAL_MPI_Env_type, &
        send_INT_MPI_Env_type, &
        send_REAL1_MPI_Env_type, &
        send_INT1_MPI_Env_type

    !> @copybrief ParallelEnv::gather_SIK0_MPI_Env_type
    !> @copydetails ParallelEnv::gather_SIK0_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: gather_SIK0_MPI_Env_type
    !> @copybrief ParallelEnv::gather_SIK1_MPI_Env_type
    !> @copydetails ParallelEnv::gather_SIK1_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: gather_SIK1_MPI_Env_type
    !> @copybrief ParallelEnv::gather_SLK0_MPI_Env_type
    !> @copydetails ParallelEnv::gather_SLK0_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: gather_SLK0_MPI_Env_type
    !> @copybrief ParallelEnv::gather_SLK1_MPI_Env_type
    !> @copydetails ParallelEnv::gather_SLK1_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: gather_SLK1_MPI_Env_type
    !>
    GENERIC :: gather => gather_SIK0_MPI_Env_type, &
        gather_SLK0_MPI_Env_type, &
        gather_SLK1_MPI_Env_type, &
        gather_SIK1_MPI_Env_type
    !> @copybrief ParallelEnv::scatter_SLK0_MPI_Env_type
    !> @copydetails ParallelEnv::scatter_SLK0_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: scatter_SLK0_MPI_Env_type
    !> @copybrief ParallelEnv::scatter_SLK1_MPI_Env_type
    !> @copydetails ParallelEnv::scatter_SLK1_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: scatter_SLK1_MPI_Env_type
    !>
    GENERIC :: scatter => scatter_SLK0_MPI_Env_type, &
        scatter_SLK1_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SNK0_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SNK0_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SNK0_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SNK1_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SNK1_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SNK1_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SLK0_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SLK0_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SLK0_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SLK1_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SLK1_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SLK1_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SSK1_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SSK1_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SSK1_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SSK0_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SSK0_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SSK0_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SSK2_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SSK2_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SSK2_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SSK3_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SSK3_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SSK3_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SSK4_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SSK4_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SSK4_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SDK0_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SDK0_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SDK0_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SDK1_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SDK1_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SDK1_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SDK2_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SDK2_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SDK2_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SDK3_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SDK3_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SDK3_MPI_Env_type
    !> @copybrief ParallelEnv::bcast_SDK4_MPI_Env_type
    !> @copydetails ParallelEnv::bcast_SDK4_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: bcast_SDK4_MPI_Env_type
    !>
    GENERIC :: bcast => bcast_SNK0_MPI_Env_type, &
        bcast_SNK1_MPI_Env_type, &
        bcast_SLK0_MPI_Env_type, &
        bcast_SLK1_MPI_Env_type, &
        bcast_SSK0_MPI_Env_type, &
        bcast_SSK1_MPI_Env_type, &
        bcast_SSK2_MPI_Env_type, &
        bcast_SSK3_MPI_Env_type, &
        bcast_SSK4_MPI_Env_type, &
        bcast_SDK0_MPI_Env_type, &
        bcast_SDK1_MPI_Env_type, &
        bcast_SDK2_MPI_Env_type, &
        bcast_SDK3_MPI_Env_type, &
        bcast_SDK4_MPI_Env_type
    !> @copybrief ParallelEnv::allReduceR_scalar_MPI_Env_type
    !> @copydetails  ParallelEnv::allReduceR_scalar_MPI_Env_type
    PROCEDURE,PASS :: allReduce_scalar => allReduceR_scalar_MPI_Env_type
    !> @copybrief ParallelEnv::allReduceR_array_MPI_Env_type
    !> @copydetails  ParallelEnv::allReduceR_array_MPI_Env_type
    PROCEDURE,PASS :: allReduce => allReduceR_array_MPI_Env_type
    !> @copybrief ParallelEnv::allReduceI_scalar_MPI_Env_type
    !> @copydetails  ParallelEnv::allReduceI_scalar_MPI_Env_type
    PROCEDURE,PASS :: allReduceI_scalar => allReduceI_scalar_MPI_Env_type
    !> @copybrief ParallelEnv::allReduceI_array_MPI_Env_type
    !> @copydetails  ParallelEnv::allReduceI_array_MPI_Env_type
    PROCEDURE,PASS :: allReduceI => allReduceI_array_MPI_Env_type
    !> @copybrief ParallelEnv::allReduceMaxR_scalar_MPI_Env_type
    !> @copydetails  ParallelEnv::allReduceMaxR_scalar_MPI_Env_type
    PROCEDURE,PASS :: allReduceMax_scalar => allReduceMaxR_scalar_MPI_Env_type
    !> @copybrief ParallelEnv::allReduceMaxR_array_MPI_Env_type
    !> @copydetails  ParallelEnv::allReduceMaxR_array_MPI_Env_type
    PROCEDURE,PASS :: allReduceMax => allReduceMaxR_array_MPI_Env_type
    !> @copybrief ParallelEnv::allReduceMaxI_scalar_MPI_Env_type
    !> @copydetails  ParallelEnv::allReduceMaxI_scalar_MPI_Env_type
    PROCEDURE,PASS :: allReduceMaxI_scalar => allReduceMaxI_scalar_MPI_Env_type
    !> @copybrief ParallelEnv::allReduceMaxI_array_MPI_Env_type
    !> @copydetails  ParallelEnv::allReduceMaxI_array_MPI_Env_type
    PROCEDURE,PASS :: allReduceMaxI => allReduceMaxI_array_MPI_Env_type
    !> @copybrief ParallelEnv::allReduceMinR_scalar_MPI_Env_type
    !> @copydetails  ParallelEnv::allReduceMinR_scalar_MPI_Env_type
    PROCEDURE,PASS :: allReduceMin_scalar => allReduceMinR_scalar_MPI_Env_type
    !> @copybrief ParallelEnv::allReduceMinR_array_MPI_Env_type
    !> @copydetails  ParallelEnv::allReduceMinR_array_MPI_Env_type
    PROCEDURE,PASS :: allReduceMin => allReduceMinR_array_MPI_Env_type
    !> @copybrief ParallelEnv::allReduceMinI_scalar_MPI_Env_type
    !> @copydetails  ParallelEnv::allReduceMinI_scalar_MPI_Env_type
    PROCEDURE,PASS :: allReduceMinI_scalar => allReduceMinI_scalar_MPI_Env_type
    !> @copybrief ParallelEnv::allReduceMinI_array_MPI_Env_type
    !> @copydetails  ParallelEnv::allReduceMinI_array_MPI_Env_type
    PROCEDURE,PASS :: allReduceMinI => allReduceMinI_array_MPI_Env_type
    !> @copybrief ParallelEnv::reduceMaxLoc_MPI_Env_type
    !> @copydetails  ParallelEnv::reduceMaxLoc_MPI_Env_type
    PROCEDURE,PASS :: reduceMaxLoc => reduceMaxLocR_MPI_Env_type
    !> @copybrief ParallelEnv::reduceMinLoc_MPI_Env_type
    !> @copydetails  ParallelEnv::reduceMinLoc_MPI_Env_type
    PROCEDURE,PASS :: reduceMinLoc => reduceMinLocR_MPI_Env_type
    !> @copybrief ParallelEnv::trueForAll_SBK0_MPI_Env_type
    !> @copydetails  ParallelEnv::trueForAll_SBK0_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: trueForAll_SBK0 => trueForAll_SBK0_MPI_Env_type
    !> @copybrief ParallelEnv::trueForAll_SBK1_MPI_Env_type
    !> @copydetails  ParallelEnv::trueForAll_SBK1_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: trueForAll_SBK1 => trueForAll_SBK1_MPI_Env_type
    !> @copybrief ParallelEnv::trueForAll_SBK2_MPI_Env_type
    !> @copydetails  ParallelEnv::trueForAll_SBK2_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: trueForAll_SBK2 => trueForAll_SBK2_MPI_Env_type
    !> @copybrief ParallelEnv::trueForAll_SBK3_MPI_Env_type
    !> @copydetails  ParallelEnv::trueForAll_SBK3_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: trueForAll_SBK3 => trueForAll_SBK3_MPI_Env_type
    !> @copybrief ParallelEnv::trueForAll_SBK4_MPI_Env_type
    !> @copydetails  ParallelEnv::trueForAll_SBK4_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: trueForAll_SBK4 => trueForAll_SBK4_MPI_Env_type
    GENERIC :: trueForAll => trueForAll_SBK0,trueForAll_SBK1, &
        trueForAll_SBK2,trueForAll_SBK3, &
        trueForAll_SBK4
    !> @copybrief ParallelEnv::trueForAny_SBK0_MPI_Env_type
    !> @copydetails  ParallelEnv::trueForAny_SBK0_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: trueForAny_SBK0 => trueForAny_SBK0_MPI_Env_type
    !> @copybrief ParallelEnv::trueForAny_SBK1_MPI_Env_type
    !> @copydetails  ParallelEnv::trueForAny_SBK1_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: trueForAny_SBK1 => trueForAny_SBK1_MPI_Env_type
    !> @copybrief ParallelEnv::trueForAny_SBK2_MPI_Env_type
    !> @copydetails  ParallelEnv::trueForAny_SBK2_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: trueForAny_SBK2 => trueForAny_SBK2_MPI_Env_type
    !> @copybrief ParallelEnv::trueForAny_SBK3_MPI_Env_type
    !> @copydetails  ParallelEnv::trueForAny_SBK3_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: trueForAny_SBK3 => trueForAny_SBK3_MPI_Env_type
    !> @copybrief ParallelEnv::trueForAny_SBK4_MPI_Env_type
    !> @copydetails  ParallelEnv::trueForAny_SBK4_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: trueForAny_SBK4 => trueForAny_SBK4_MPI_Env_type
    GENERIC :: trueForAny => trueForAny_SBK0,trueForAny_SBK1, &
        trueForAny_SBK2,trueForAny_SBK3, &
        trueForAny_SBK4
    !> @copybrief ParallelEnv::finalize_MPI_Env_type
    !> @copydetails  ParallelEnv::finalize_MPI_Env_type
    PROCEDURE,NOPASS :: finalize => finalize_MPI_Env_type
    !> @copybrief ParallelEnv::scanSum_SIK0_MPI_Env_type
    !> @copydetails ParallelEnv::scanSum_SIK0_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: scanSum_SIK0_MPI_Env_type
    !> @copybrief ParallelEnv::scanSum_SIK_MPI_Env_type
    !> @copydetails ParallelEnv::scanSum_SIK_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: scanSum_SIK_MPI_Env_type
    !> @copybrief ParallelEnv::scanSum_SRK0_MPI_Env_type
    !> @copydetails ParallelEnv::scanSum_SRK0_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: scanSum_SRK0_MPI_Env_type
    !> @copybrief ParallelEnv::scanSum_SRK_MPI_Env_type
    !> @copydetails ParallelEnv::scanSum_SRK_MPI_Env_type
    PROCEDURE,PASS,PRIVATE :: scanSum_SRK_MPI_Env_type
    !>
    GENERIC :: scanSum => scanSum_SIK0_MPI_Env_type, &
        scanSum_SIK_MPI_Env_type, &
        scanSum_SRK0_MPI_Env_type, &
        scanSum_SRK_MPI_Env_type
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
  !> The device ID for offloading to to external accelerators
  INTEGER(SIK),PRIVATE :: deviceID=-1
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
    !> @copybrief ParallelEnv::setDeviceID_ParEnvType
    !> @copydetails ParallelEnv::setDeviceID_ParEnvType
    PROCEDURE,PASS :: setDeviceID => setDeviceID_ParEnvType
    !> @copybrief ParallelEnv::getDeviceID_ParEnvType
    !> @copydetails ParallelEnv::getDeviceID_ParEnvType
    PROCEDURE,PASS :: getDeviceID => getDeviceID_ParEnvType
ENDTYPE ParallelEnvType

!> @brief Overloads the assignment operator for the ParallelEnvType.
!>
INTERFACE ASSIGNMENT(=)
  !> @copybrief ParallelEnv::assign_ParEnvType
  !> @copydetails ParallelEnv::assign_ParEnvType
  MODULE PROCEDURE assign_ParEnvType
ENDINTERFACE

#ifdef HAVE_MPI
!> Private scratch variable for the mpierr
INTEGER(SIK) :: mpierr
#else
INTEGER(SIK),SAVE :: MAX_PE_COMM_ID=1
#endif

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
#ifdef HAVE_MPI || FUTILITY_HAVE_PETSC
  CHARACTER(LEN=*),PARAMETER :: myName='init_MPI_Env_type'
#endif
  CLASS(MPI_EnvType),INTENT(INOUT) :: myPE
  INTEGER(SIK),INTENT(IN),OPTIONAL :: PEparam
  INTEGER(SIK) :: icomm
#ifdef HAVE_MPI
  INTEGER(SIK) :: isinit
#endif
#ifdef FUTILITY_HAVE_PETSC
  LOGICAL(SBK),ALLOCATABLE :: allpetsc2(:)
#endif

  REQUIRE(.NOT.myPE%initstat)
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

#ifdef FUTILITY_HAVE_PETSC
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
#ifdef FUTILITY_HAVE_SLEPC
  IF(.NOT.petsc_isinit) CALL SlepcInitialize(PETSC_NULL_CHARACTER,ierr)
#else
  IF(.NOT.petsc_isinit) CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif
#endif
  myPE%initstat=.TRUE.
ENDSUBROUTINE init_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> Clears an MPI environment type object.
!>
!> If the communicator is not MPI_COMM_WORLD then it is also freed.
SUBROUTINE clear_MPI_Env_type(myPE)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='clear_MPI_Env_type'
#endif
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
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
#ifdef HAVE_MPI
  IF(myPE%initstat) CALL MPI_Barrier(myPE%comm,mpierr)
#endif
ENDSUBROUTINE barrier_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Send for character array
!> @param myPE parallel environment where the communication originates
!> @param sendbuf the data which is to be sent
!> @param destProc the rank of the recieving proc in myPE
!> @param in_tag message id which can be provided to distiguish messages
SUBROUTINE send_CHAR_MPI_Env_type(myPE,sendbuf,destProc,tag)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  CHARACTER(LEN=*),INTENT(IN) :: sendbuf
  INTEGER(SIK),INTENT(IN) :: destProc
  INTEGER(SIK),INTENT(IN) :: tag
  !
#ifdef HAVE_MPI
  INTEGER(SIK) :: numChar
  numChar = LEN_TRIM(sendbuf)
  CALL MPI_send(sendBuf,numChar,MPI_CHARACTER,destProc,tag,myPE%comm,mpierr)
#endif
ENDSUBROUTINE send_CHAR_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Send for ints
!> @param myPE parallel environment where the communication originates
!> @param sendbuf the scalar which is to be sent
!> @param destProc the rank of the recieving proc in myPE
!> @param in_tag message id which can be provided to distiguish messages
SUBROUTINE send_INT_MPI_Env_type(myPE,sendbuf,destProc,in_tag)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: sendbuf
  INTEGER(SIK),INTENT(IN) :: destProc
  INTEGER(SIK),INTENT(IN),OPTIONAL :: in_tag
  !
#ifdef HAVE_MPI
  INTEGER(SIK) :: tag
  tag=1
  IF(PRESENT(in_tag)) tag=in_tag
#ifdef DBLINT
  CALL MPI_send(sendBuf,1,MPI_INTEGER8,destProc,tag,myPE%comm,mpierr)
#else
  CALL MPI_send(sendBuf,1,MPI_INTEGER,destProc,tag,myPE%comm,mpierr)
#endif
#endif
ENDSUBROUTINE send_INT_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Send for ints
!> @param myPE parallel environment where the communication originates
!> @param n the number of elements to be sent
!> @param sendbuf the data which is to be sent
!> @param destProc the rank of the recieving proc in myPE
!> @param in_tag message id which can be provided to distiguish messages
SUBROUTINE send_INT1_MPI_Env_type(myPE,sendbuf,n,destProc,in_tag)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: sendbuf(*)
  INTEGER(SIK),INTENT(IN) :: n
  INTEGER(SIK),INTENT(IN) :: destProc
  INTEGER(SIK),INTENT(IN),OPTIONAL :: in_tag
  !
#ifdef HAVE_MPI
  INTEGER(SIK) :: tag
  tag=1
  IF(PRESENT(in_tag)) tag=in_tag
#ifdef DBLINT
  CALL MPI_send(sendBuf,n,MPI_INTEGER8,destProc,tag,myPE%comm,mpierr)
#else
  CALL MPI_send(sendBuf,n,MPI_INTEGER,destProc,tag,myPE%comm,mpierr)
#endif
#endif
ENDSUBROUTINE send_INT1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Send for reals
!> @param myPE parallel environment where the communication originates
!> @param sendbuf the data which is to be sent
!> @param destProc the rank of the recieving proc in myPE
!> @param in_tag message id which can be provided to distiguish messages
SUBROUTINE send_REAL_MPI_Env_type(myPE,sendbuf,destProc,in_tag)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SRK),INTENT(IN) :: sendbuf
  INTEGER(SIK),INTENT(IN) :: destProc
  INTEGER(SIK),INTENT(IN),OPTIONAL :: in_tag
  !
#ifdef HAVE_MPI
  INTEGER(SIK) :: tag
  tag=1
  IF(PRESENT(in_tag)) tag=in_tag
#ifdef DBL
  CALL MPI_send(sendBuf,1,MPI_DOUBLE_PRECISION,destProc,tag,myPE%comm,mpierr)
#else
  CALL MPI_send(sendBuf,1,MPI_SINGLE_PRECISION,destProc,tag,myPE%comm,mpierr)
#endif
#endif
ENDSUBROUTINE send_REAL_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Send for reals
!> @param myPE parallel environment where the communication originates
!> @param n the number of elements to be sent
!> @param sendbuf the data which is to be sent
!> @param destProc the rank of the recieving proc in myPE
!> @param in_tag message id which can be provided to distiguish messages
SUBROUTINE send_REAL1_MPI_Env_type(myPE,sendbuf,n,destProc,in_tag)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SRK),INTENT(IN) :: sendbuf(*)
  INTEGER(SIK),INTENT(IN) :: n
  INTEGER(SIK),INTENT(IN) :: destProc
  INTEGER(SIK),INTENT(IN),OPTIONAL :: in_tag
  !
#ifdef HAVE_MPI
  INTEGER(SIK) :: tag
  tag=1
  IF(PRESENT(in_tag)) tag=in_tag
#ifdef DBL
  CALL MPI_send(sendBuf,n,MPI_DOUBLE_PRECISION,destProc,tag,myPE%comm,mpierr)
#else
  CALL MPI_send(sendBuf,n,MPI_SINGLE_PRECISION,destProc,tag,myPE%comm,mpierr)
#endif
#endif
ENDSUBROUTINE send_REAL1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_recv for characters
!> @param myPE parallel environment where the communication originates
!> @param sendbuf the data which is to be sent
!> @param destProc the rank of the recieving proc in myPE
!> @param in_tag message id which can be provided to distiguish messages
SUBROUTINE recv_CHAR_MPI_Env_type(myPE,recvbuf,srcProc,tag)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  CHARACTER(LEN=*),INTENT(OUT) :: recvbuf
  INTEGER(SIK),INTENT(IN) :: srcProc
  INTEGER(SIK),INTENT(IN) :: tag
  !
#ifdef HAVE_MPI
  INTEGER(SIK) :: numChar
  INTEGER :: stat(MPI_STATUS_SIZE)

  CALL MPI_Probe(srcProc,tag,myPE%comm,stat,mpierr)
  CALL MPI_Get_Count(stat,MPI_CHARACTER,numChar,mpierr)
  CALL MPI_recv(recvBuf,numChar,MPI_CHARACTER,srcProc,tag,myPE%comm,stat,mpierr)
#endif
ENDSUBROUTINE recv_CHAR_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_recv for reals
!> @param myPE parallel environment where the communication originates
!> @param sendbuf the scalar which is to be sent
!> @param destProc the rank of the recieving proc in myPE
!> @param in_tag message id which can be provided to distiguish messages
SUBROUTINE recv_REAL_MPI_Env_type(myPE,recvbuf,srcProc,in_tag)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SRK),INTENT(INOUT) :: recvbuf
  INTEGER(SIK),INTENT(IN) :: srcProc
  INTEGER(SIK),INTENT(IN),OPTIONAL :: in_tag
  !
#ifdef HAVE_MPI
  INTEGER :: stat(MPI_STATUS_SIZE)
  INTEGER(SIK) :: tag
  tag=1
  IF(PRESENT(in_tag)) tag=in_tag
#ifdef DBL
  CALL MPI_recv(recvBuf,1,MPI_DOUBLE_PRECISION,srcProc,tag,myPE%comm,stat,mpierr)
#else
  CALL MPI_recv(recvBuf,1,MPI_SINGLE_PRECISION,srcProc,tag,myPE%comm,stat,mpierr)
#endif
#endif
ENDSUBROUTINE recv_REAL_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_recv for reals
!> @param myPE parallel environment where the communication originates
!> @param sendbuf the data array which is to be sent
!> @param n the number of elements to be sent
!> @param destProc the rank of the recieving proc in myPE
!> @param in_tag message id which can be provided to distiguish messages
SUBROUTINE recv_REAL1_MPI_Env_type(myPE,recvbuf,n,srcProc,in_tag)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SRK),INTENT(INOUT) :: recvbuf(:)
  INTEGER(SIK),INTENT(IN) :: n
  INTEGER(SIK),INTENT(IN) :: srcProc
  INTEGER(SIK),INTENT(IN),OPTIONAL :: in_tag
  !
#ifdef HAVE_MPI
  INTEGER :: stat(MPI_STATUS_SIZE)
  INTEGER(SIK) :: tag
  tag=1
  IF(PRESENT(in_tag)) tag=in_tag
#ifdef DBL
  CALL MPI_recv(recvBuf,n,MPI_DOUBLE_PRECISION,srcProc,tag,myPE%comm,stat,mpierr)
#else
  CALL MPI_recv(recvBuf,n,MPI_SINGLE_PRECISION,srcProc,tag,myPE%comm,stat,mpierr)
#endif
#endif
ENDSUBROUTINE recv_REAL1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_recv for integers
!> @param myPE parallel environment where the communication originates
!> @param recvbuf the scalar which is to be sent
!> @param destProc the rank of the recieving proc in myPE
!> @param in_tag message id which can be provided to distiguish messages
SUBROUTINE recv_INT_MPI_Env_type(myPE,recvbuf,srcProc,in_tag)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(INOUT) :: recvbuf
  INTEGER(SIK),INTENT(IN) :: srcProc
  INTEGER(SIK),INTENT(IN),OPTIONAL :: in_tag
  !
#ifdef HAVE_MPI
  INTEGER :: stat(MPI_STATUS_SIZE)
  INTEGER(SIK) :: tag
  tag=1
  IF(PRESENT(in_tag)) tag=in_tag
#ifdef DBLINT
  CALL MPI_recv(recvBuf,1,MPI_INTEGER8,srcProc,tag,myPE%comm,stat,mpierr)
#else
  CALL MPI_recv(recvBuf,1,MPI_INTEGER,srcProc,tag,myPE%comm,stat,mpierr)
#endif
#endif
ENDSUBROUTINE recv_INT_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_recv for integers
!> @param myPE parallel environment where the communication originates
!> @param recvbuf the array which is to be sent
!> @param n the number of elements to be sent
!> @param destProc the rank of the recieving proc in myPE
!> @param in_tag message id which can be provided to distiguish messages
SUBROUTINE recv_INT1_MPI_Env_type(myPE,recvbuf,n,srcProc,in_tag)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(INOUT) :: recvbuf(:)
  INTEGER(SIK),INTENT(IN) :: n
  INTEGER(SIK),INTENT(IN) :: srcProc
  INTEGER(SIK),INTENT(IN),OPTIONAL :: in_tag
  !
#ifdef HAVE_MPI
  INTEGER :: stat(MPI_STATUS_SIZE)
  INTEGER(SIK) :: tag

  tag=1
  IF(PRESENT(in_tag)) tag=in_tag
#ifdef DBLINT
  CALL MPI_recv(recvBuf,n,MPI_INTEGER8,srcProc,tag,myPE%comm,stat,mpierr)
#else
  CALL MPI_recv(recvBuf,n,MPI_INTEGER,srcProc,tag,myPE%comm,stat,mpierr)
#endif
#endif
ENDSUBROUTINE recv_INT1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Gather for integers
!> @param myPE parallel environment where the communication originates
!> @param sendbuf the data which is to be sent
!> @param recvbuf the data which is to be sent
!> @param root the rank of the root process
SUBROUTINE gather_SIK0_MPI_Env_type(myPE,sendbuf,recvbuf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: sendbuf
  INTEGER(SIK),INTENT(INOUT) :: recvbuf(:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(0 <= rank)
  REQUIRE(rank < myPE%nproc)
  REQUIRE(SIZE(recvbuf) == myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Gather(sendbuf,1,MPI_INTEGER,recvbuf,1,MPI_INTEGER, &
      rank,myPE%comm,mpierr)
#else
  recvbuf(1)=sendbuf
#endif
ENDSUBROUTINE gather_SIK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Gather for an SIK array
!> @param myPE parallel environment where the communication originates
!> @param sendbuf the data which is to be sent
!> @param recvbuf the data which is to be sent
!> @param root the rank of the root process
SUBROUTINE gather_SIK1_MPI_Env_type(myPE,sendbuf,recvbuf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: sendbuf(:)
  INTEGER(SIK),INTENT(INOUT) :: recvbuf(:,:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank,count
#ifndef HAVE_MPI
  INTEGER(SIK)::i,j,n
#endif
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(0 <= rank)
  REQUIRE(rank < myPE%nproc)
  count=SIZE(sendbuf)
  REQUIRE(SIZE(recvbuf) == myPE%nproc*count)
#ifdef HAVE_MPI
#ifdef DBLINT
  !64 Bit integer
  CALL MPI_Gather(sendbuf,count,MPI_INTEGER8,recvbuf,count, &
      MPI_INTEGER8,rank,myPE%comm,mpierr)
#else
  !32 Bit integer
  CALL MPI_Gather(sendbuf,count,MPI_INTEGER,recvbuf,count, &
      MPI_INTEGER,rank,myPE%comm,mpierr)
#endif
#else
  DO n=1,count
    i=MOD(n-1,SIZE(recvbuf,DIM=1))+1
    j=(n-1)/SIZE(recvbuf,DIM=1)+1
    recvbuf(i,j)=sendbuf(n)
  ENDDO
#endif
ENDSUBROUTINE gather_SIK1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Gather
SUBROUTINE gather_SLK0_MPI_Env_type(myPE,sendbuf,recvbuf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SLK),INTENT(IN) :: sendbuf
  INTEGER(SLK),INTENT(INOUT) :: recvbuf(:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(0 <= rank)
  REQUIRE(rank < myPE%nproc)
  REQUIRE(SIZE(recvbuf) == myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Gather(sendbuf,1,MPI_INTEGER8,recvbuf,1,MPI_INTEGER8, &
      rank,myPE%comm,mpierr)
#else
  recvbuf(1)=sendbuf
#endif
ENDSUBROUTINE gather_SLK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Gather
SUBROUTINE gather_SLK1_MPI_Env_type(myPE,sendbuf,recvbuf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SLK),INTENT(IN) :: sendbuf(:)
  INTEGER(SLK),INTENT(INOUT) :: recvbuf(:,:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank,count
#ifndef HAVE_MPI
  INTEGER(SIK)::i,j,n
#endif
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(0 <= rank)
  REQUIRE(rank < myPE%nproc)
  count=SIZE(sendbuf)
  REQUIRE(SIZE(recvbuf) == myPE%nproc*count)
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
ENDSUBROUTINE gather_SLK1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Scatter
SUBROUTINE scatter_SLK0_MPI_Env_type(myPE,sendbuf,recvbuf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SLK),INTENT(IN) :: sendbuf(:)
  INTEGER(SLK),INTENT(INOUT) :: recvbuf
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(0 <= rank)
  REQUIRE(rank < myPE%nproc)
  REQUIRE(SIZE(sendbuf) == myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Scatter(sendbuf,1,MPI_INTEGER8,recvbuf,1,MPI_INTEGER8, &
      rank,myPE%comm,mpierr)
#else
  recvbuf=sendbuf(1)
#endif
ENDSUBROUTINE scatter_SLK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Scatter
SUBROUTINE scatter_SLK1_MPI_Env_type(myPE,sendbuf,recvbuf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SLK),INTENT(IN) :: sendbuf(:,:)
  INTEGER(SLK),INTENT(INOUT) :: recvbuf(:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank,count
#ifndef HAVE_MPI
  INTEGER(SIK)::i,j,n
#endif
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(0 <= rank)
  REQUIRE(rank < myPE%nproc)
  count=SIZE(recvbuf)
  REQUIRE(SIZE(sendbuf) == myPE%nproc*count)
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
ENDSUBROUTINE scatter_SLK1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for scalar short integer
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SNK0_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SNK),INTENT(IN) :: buf
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,1,MPI_INTEGER4,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SNK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for rank-1 short integer
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SNK1_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SNK),INTENT(IN) :: buf(:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,SIZE(buf),MPI_INTEGER4,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SNK1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for scalar long integer
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SLK0_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SLK),INTENT(IN) :: buf
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,1,MPI_INTEGER8,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SLK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for rank-1 long integer
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SLK1_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SLK),INTENT(IN) :: buf(:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,SIZE(buf),MPI_INTEGER8,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SLK1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for scalar single precision real
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SSK0_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SSK),INTENT(IN) :: buf
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,1,MPI_REAL4,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SSK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for rank-1 single precision real
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SSK1_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SSK),INTENT(IN) :: buf(:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL4,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SSK1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for rank-2 single precision real
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SSK2_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SSK),INTENT(IN) :: buf(:,:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL4,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SSK2_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for scalar double precision real
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SDK0_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SDK),INTENT(IN) :: buf
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,1,MPI_REAL8,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SDK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for rank-1 double precision real
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SDK1_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SDK),INTENT(IN) :: buf(:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL8,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SDK1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for rank-2 double precision real
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SDK2_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SDK),INTENT(IN) :: buf(:,:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL8,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SDK2_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for rank-3 single precision real
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SSK3_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SSK),INTENT(IN) :: buf(:,:,:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL4,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SSK3_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for rank-3 double precision real
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SDK3_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SDK),INTENT(IN) :: buf(:,:,:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL8,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SDK3_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for rank-4 single precision real
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SSK4_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SSK),INTENT(IN) :: buf(:,:,:,:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL4,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SSK4_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Performs broadcast for rank-4 double precision real
!> @param myPE the MPI_EnvType
!> @param buf the data to be broadcast
!> @param root the rank of the source process; optional
!>
!> If @c root is not present, it default to 0 (master).
!>
SUBROUTINE bcast_SDK4_MPI_Env_type(myPE,buf,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SDK),INTENT(IN) :: buf(:,:,:,:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
  INTEGER(SIK) :: rank
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
#ifdef HAVE_MPI
  CALL MPI_Bcast(buf,SIZE(buf),MPI_REAL8,rank,myPE%comm,mpierr)
#endif
ENDSUBROUTINE bcast_SDK4_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a sum of operation
!> for a real scalar.
!> @param myPE the MPI parallel environment
!> @param x the partial sum to be returned as the total sum
!>
!> This routine only performs a sum operation and only for reals.
!>
SUBROUTINE allReduceR_scalar_MPI_Env_type(myPE,x)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SRK),INTENT(INOUT) :: x
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='allReduceR_scalar_MPI_Env_type'
  REAL(SRK) :: rbuf
  REQUIRE(myPE%initstat)
#ifdef DBL
  CALL MPI_Allreduce(x,rbuf,1,MPI_DOUBLE_PRECISION,MPI_SUM, &
      myPE%comm,mpierr)
#else
  CALL MPI_Allreduce(x,rbuf,1,MPI_SINGLE_PRECISION,MPI_SUM, &
      myPE%comm,mpierr)
#endif
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_Allreduce returned an error!')
  ELSE
    x=rbuf
  ENDIF
#endif
ENDSUBROUTINE allReduceR_scalar_MPI_Env_type
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
SUBROUTINE allReduceR_array_MPI_Env_type(myPE,n,x)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: n
  REAL(SRK),INTENT(INOUT) :: x(*)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='allReduceR_array_MPI_Env_type'
  REAL(SRK),ALLOCATABLE :: rbuf(:)
  REQUIRE(myPE%initstat)
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
#endif
ENDSUBROUTINE allReduceR_array_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a max operation
!> for a real scalar.
!> @param myPE the MPI parallel environment
!> @param x the partial value to be returned as the max
!>
!> This routine only performs a max operation and only for reals.
!>
SUBROUTINE allReduceMaxR_scalar_MPI_Env_type(myPE,x)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SRK),INTENT(INOUT) :: x
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='allReduceMaxR_scalar_MPI_Env_type'
  REAL(SRK) :: rbuf
  REQUIRE(myPE%initstat)
#ifdef DBL
  CALL MPI_Allreduce(x,rbuf,1,MPI_DOUBLE_PRECISION,MPI_MAX,myPE%comm,mpierr)
#else
  CALL MPI_Allreduce(x,rbuf,n,MPI_SINGLE_PRECISION,MPI_MAX,myPE%comm,mpierr)
#endif
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_AllreduceMax returned an error!')
  ELSE
    x=rbuf
  ENDIF
#endif
ENDSUBROUTINE allReduceMaxR_scalar_MPI_Env_type
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
SUBROUTINE allReduceMaxR_array_MPI_Env_type(myPE,n,x)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: n
  REAL(SRK),INTENT(INOUT) :: x(*)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='allReduceR_array_MPI_Env_type'
  REAL(SRK) :: rbuf(n)
  REQUIRE(myPE%initstat)
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
#endif
ENDSUBROUTINE allReduceMaxR_array_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a min operation
!> for a real scalar.
!> @param myPE the MPI parallel environment
!> @param x the partial value to be returned as the min
!>
!> This routine only performs a min operation and only for reals.
!>
SUBROUTINE allReduceMinR_scalar_MPI_Env_type(myPE,x)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  REAL(SRK),INTENT(INOUT) :: x
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='allReduceMinR_scalar_MPI_Env_type'
  REAL(SRK) :: rbuf
  REQUIRE(myPE%initstat)
#ifdef DBL
  CALL MPI_Allreduce(x,rbuf,1,MPI_DOUBLE_PRECISION,MPI_MIN,myPE%comm,mpierr)
#else
  CALL MPI_Allreduce(x,rbuf,n,MPI_SINGLE_PRECISION,MPI_MIN,myPE%comm,mpierr)
#endif
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_AllreduceMin returned an error!')
  ELSE
    x=rbuf
  ENDIF
#endif
ENDSUBROUTINE allReduceMinR_scalar_MPI_Env_type
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
SUBROUTINE allReduceMinR_array_MPI_Env_type(myPE,n,x)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: n
  REAL(SRK),INTENT(INOUT) :: x(*)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='allReduceR_array_MPI_Env_type'
  REAL(SRK) :: rbuf(n)
  REQUIRE(myPE%initstat)
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
#endif
ENDSUBROUTINE allReduceMinR_array_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_reduce and performs a maxloc operation
!> for a real array.
!> @param myPE the MPI parallel environment
!> @param n the number of data elements to communicate
!> @param x the partial array to be returned as the max array
!> @param i the partial array of indices to be returned
!> @param root the process on which to store the reduction
!>
!> This routine only performs a maxloc operation and only for reals.
!>
SUBROUTINE reduceMaxLocR_MPI_Env_type(myPE,n,x,i,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: n
  REAL(SRK),INTENT(INOUT) :: x(*)
  INTEGER(SLK),INTENT(INOUT) :: i(*)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='reduceMaxLocR_MPI_Env_type'
  REAL(SRK) :: sbuf(2,n)
  REAL(SRK) :: rbuf(2,n)
  INTEGER(SIK) :: rank
  REQUIRE(myPE%initstat)
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
  sbuf(1,:)=x(1:n)
  sbuf(2,:)=i(1:n)
#ifdef DBL
  CALL MPI_Allreduce(sbuf,rbuf,n,MPI_2DOUBLE_PRECISION,MPI_MAXLOC, &
      myPE%comm,mpierr)
#else
  CALL MPI_Allreduce(sbuf,rbuf,n,MPI_2REAL,MPI_MAXLOC,myPE%comm,mpierr)
#endif
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_AllreduceMax returned an error!')
  ELSE
    !Copy the result to the output argument
    x(1:n)=rbuf(1,:)
    i(1:n)=INT(rbuf(2,:),SLK)
  ENDIF
#endif
ENDSUBROUTINE reduceMaxLocR_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_reduce and performs a minloc operation
!> for a real array.
!> @param myPE the MPI parallel environment
!> @param n the number of data elements to communicate
!> @param x the partial array to be returned as the min array
!> @param i the partial array of indices to be returned
!> @param root the process on which to store the reduction
!>
!> This routine only performs a min operation and only for reals.
!>
SUBROUTINE reduceMinLocR_MPI_Env_type(myPE,n,x,i,root)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: n
  REAL(SRK),INTENT(INOUT) :: x(*)
  INTEGER(SLK),INTENT(INOUT) :: i(*)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: root
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='reduceMinLocR_MPI_Env_type'
  REAL(SRK) :: sbuf(2,n)
  REAL(SRK) :: rbuf(2,n)
  INTEGER(SIK) :: rank
  REQUIRE(myPE%initstat)
  rank=0
  IF(PRESENT(root)) rank=root
  REQUIRE(rank >= 0)
  REQUIRE(rank < myPE%nproc)
  sbuf(1,:)=x(1:n)
  sbuf(2,:)=i(1:n)
#ifdef DBL
  CALL MPI_Allreduce(sbuf,rbuf,n,MPI_2DOUBLE_PRECISION,MPI_MINLOC, &
      myPE%comm,mpierr)
#else
  CALL MPI_Allreduce(sbuf,rbuf,n,MPI_2REAL,MPI_MINLOC,myPE%comm,mpierr)
#endif
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_AllreduceMin returned an error!')
  ELSE
    !Copy the result to the output argument
    x(1:n)=rbuf(1,:)
    i(1:n)=INT(rbuf(2,:),SLK)
  ENDIF
#endif
ENDSUBROUTINE reduceMinLocR_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a sum of operation
!> for an integer scalar.
!> @param myPE the MPI parallel environment
!> @param x the partial sum to be returned as the total sum
!>
!> This routine only performs a sum operation and only for integers.
!>
SUBROUTINE allReduceI_scalar_MPI_Env_type(myPE,x)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(INOUT) :: x
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='allReduceI_scalar_MPI_Env_type'
  INTEGER(SIK) :: rbuf
  REQUIRE(myPE%initstat)
  CALL MPI_Allreduce(x,rbuf,1,MPI_INTEGER,MPI_SUM,myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_Allreduce returned an error!')
  ELSE
    x=rbuf
  ENDIF
#endif
ENDSUBROUTINE allReduceI_scalar_MPI_Env_type
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
SUBROUTINE allReduceI_array_MPI_Env_type(myPE,n,x)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: n
  INTEGER(SIK),INTENT(INOUT) :: x(*)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='allReduceI_array_MPI_Env_type'
  INTEGER(SIK) :: rbuf(n)
  REQUIRE(myPE%initstat)
  CALL MPI_Allreduce(x,rbuf,n,MPI_INTEGER,MPI_SUM, &
      myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_Allreduce returned an error!')
  ELSE
    !Copy the result to the output argument
    x(1:n)=rbuf ! No BLAS_copy for integers
  ENDIF
#endif
ENDSUBROUTINE allReduceI_array_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a max operation
!> for an integer scalar.
!> @param myPE the MPI parallel environment
!> @param x the partial value to be returned as the max array
!>
!> This routine only performs a max operation and only for integers.
!>
SUBROUTINE allReduceMaxI_scalar_MPI_Env_type(myPE,x)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(INOUT) :: x
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='allReduceMaxI_scalar_MPI_Env_type'
  INTEGER(SIK) :: rbuf
  REQUIRE(myPE%initstat)
  CALL MPI_Allreduce(x,rbuf,1,MPI_INTEGER,MPI_MAX, &
      myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_AllreduceMax returned an error!')
  ELSE
    x=rbuf
  ENDIF
#endif
ENDSUBROUTINE allReduceMaxI_scalar_MPI_Env_type
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
SUBROUTINE allReduceMaxI_array_MPI_Env_type(myPE,n,x)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: n
  INTEGER(SIK),INTENT(INOUT) :: x(*)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='allReduceMaxI_array_MPI_Env_type'
  INTEGER(SIK) :: rbuf(n)
  REQUIRE(myPE%initstat)
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
#endif
ENDSUBROUTINE allReduceMaxI_array_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a min operation
!> for an integer scalar.
!> @param myPE the MPI parallel environment
!> @param x the partial value to be returned as the min array
!>
!> This routine only performs a min operation and only for integers.
!>
SUBROUTINE allReduceMinI_scalar_MPI_Env_type(myPE,x)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(INOUT) :: x
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='allReduceMinI_scalar_MPI_Env_type'
  INTEGER(SIK) :: rbuf
  REQUIRE(myPE%initstat)
  CALL MPI_Allreduce(x,rbuf,1,MPI_INTEGER,MPI_MIN, &
      myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_AllreduceMin returned an error!')
  ELSE
    x=rbuf
  ENDIF
#endif
ENDSUBROUTINE allReduceMinI_scalar_MPI_Env_type
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
SUBROUTINE allReduceMinI_array_MPI_Env_type(myPE,n,x)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: n
  INTEGER(SIK),INTENT(INOUT) :: x(*)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='allReduceMinI_array_MPI_Env_type'
  INTEGER(SIK) :: rbuf(n)
  REQUIRE(myPE%initstat)
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
#endif
ENDSUBROUTINE allReduceMinI_array_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a logical 'and'
!> operation for a scalar logical.
!> @param myPE the MPI parallel environment
!> @param lstat logical that is set to the status of the logical 'and' operation
!>
SUBROUTINE trueForAll_SBK0_MPI_Env_type(myPE,lstat)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  LOGICAL(SBK),INTENT(INOUT) :: lstat
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='trueForAll_SBK0_MPI_Env_type'
  LOGICAL(SBK) :: lrbuf
  REQUIRE(myPE%initstat)
  CALL MPI_Allreduce(lstat,lrbuf,1,MPI_LOGICAL,MPI_LAND, &
      myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_Allreduce returned an error!')
  ELSE
    lstat=lrbuf
  ENDIF
#endif
ENDSUBROUTINE trueForAll_SBK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a logical 'and'
!> operation for a 1D array logical.
!> @param myPE the MPI parallel environment
!> @param lstat logical that is set to the status of the logical 'and' operation
!>
SUBROUTINE trueForAll_SBK1_MPI_Env_type(myPE,lstat)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  LOGICAL(SBK),INTENT(INOUT) :: lstat(:)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='trueForAll_SBK1_MPI_Env_type'
  LOGICAL(SBK),ALLOCATABLE :: lrbuf(:)
  INTEGER(SIK) :: n1
  REQUIRE(myPE%initstat)
  n1=SIZE(lstat,DIM=1)
  ALLOCATE(lrbuf(n1))
  CALL MPI_Allreduce(lstat,lrbuf,n1,MPI_LOGICAL,MPI_LAND, &
      myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_Allreduce returned an error!')
  ELSE
    lstat(:)=lrbuf(:)
  ENDIF
  DEALLOCATE(lrbuf)
#endif
ENDSUBROUTINE trueForAll_SBK1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a logical 'and'
!> operation for a 2D array logical.
!> @param myPE the MPI parallel environment
!> @param lstat logical that is set to the status of the logical 'and' operation
!>
SUBROUTINE trueForAll_SBK2_MPI_Env_type(myPE,lstat)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  LOGICAL(SBK),INTENT(INOUT) :: lstat(:,:)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='trueForAll_SBK2_MPI_Env_type'
  LOGICAL(SBK),ALLOCATABLE :: lrbuf(:,:)
  INTEGER(SIK) :: n1,n2
  REQUIRE(myPE%initstat)
  n1=SIZE(lstat,DIM=1)
  n2=SIZE(lstat,DIM=2)
  ALLOCATE(lrbuf(n1,n2))
  CALL MPI_Allreduce(lstat,lrbuf,n1*n2,MPI_LOGICAL,MPI_LAND, &
      myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_Allreduce returned an error!')
  ELSE
    lstat(:,:)=lrbuf(:,:)
  ENDIF
  DEALLOCATE(lrbuf)
#endif
ENDSUBROUTINE trueForAll_SBK2_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a logical 'and'
!> operation for a 3D array logical.
!> @param myPE the MPI parallel environment
!> @param lstat logical that is set to the status of the logical 'and' operation
!>
SUBROUTINE trueForAll_SBK3_MPI_Env_type(myPE,lstat)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  LOGICAL(SBK),INTENT(INOUT) :: lstat(:,:,:)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='trueForAll_SBK3_MPI_Env_type'
  LOGICAL(SBK),ALLOCATABLE :: lrbuf(:,:,:)
  INTEGER(SIK) :: n1,n2,n3
  REQUIRE(myPE%initstat)
  n1=SIZE(lstat,DIM=1)
  n2=SIZE(lstat,DIM=2)
  n3=SIZE(lstat,DIM=3)
  ALLOCATE(lrbuf(n1,n2,n3))
  CALL MPI_Allreduce(lstat,lrbuf,n1*n2*n3,MPI_LOGICAL,MPI_LAND, &
      myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_Allreduce returned an error!')
  ELSE
    lstat(:,:,:)=lrbuf(:,:,:)
  ENDIF
  DEALLOCATE(lrbuf)
#endif
ENDSUBROUTINE trueForAll_SBK3_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a logical 'and'
!> operation for a 4D array logical.
!> @param myPE the MPI parallel environment
!> @param lstat logical that is set to the status of the logical 'and' operation
!>
SUBROUTINE trueForAll_SBK4_MPI_Env_type(myPE,lstat)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  LOGICAL(SBK),INTENT(INOUT) :: lstat(:,:,:,:)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='trueForAll_SBK4_MPI_Env_type'
  LOGICAL(SBK),ALLOCATABLE :: lrbuf(:,:,:,:)
  INTEGER(SIK) :: n1,n2,n3,n4
  REQUIRE(myPE%initstat)
  n1=SIZE(lstat,DIM=1)
  n2=SIZE(lstat,DIM=2)
  n3=SIZE(lstat,DIM=3)
  n4=SIZE(lstat,DIM=4)
  ALLOCATE(lrbuf(n1,n2,n3,n4))
  CALL MPI_Allreduce(lstat,lrbuf,n1*n2*n3*n4,MPI_LOGICAL,MPI_LAND, &
      myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_Allreduce returned an error!')
  ELSE
    lstat(:,:,:,:)=lrbuf(:,:,:,:)
  ENDIF
  DEALLOCATE(lrbuf)
#endif
ENDSUBROUTINE trueForAll_SBK4_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a logical 'or'
!> operation for a scalar logical.
!> @param myPE the MPI parallel environment
!> @param lstat logical that is set to the status of the logical 'or' operation
!>
SUBROUTINE trueForAny_SBK0_MPI_Env_type(myPE,lstat)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  LOGICAL(SBK),INTENT(INOUT) :: lstat
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='trueForAny_SBK0_MPI_Env_type'
  LOGICAL(SBK) :: lrbuf
  REQUIRE(myPE%initstat)
  CALL MPI_Allreduce(lstat,lrbuf,1,MPI_LOGICAL,MPI_LOR, &
      myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_Allreduce returned an error!')
  ELSE
    lstat=lrbuf
  ENDIF
#endif
ENDSUBROUTINE trueForAny_SBK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a logical 'or'
!> operation for a 1D array logical.
!> @param myPE the MPI parallel environment
!> @param lstat logical that is set to the status of the logical 'or' operation
!>
SUBROUTINE trueForAny_SBK1_MPI_Env_type(myPE,lstat)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  LOGICAL(SBK),INTENT(INOUT) :: lstat(:)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='trueForAny_SBK1_MPI_Env_type'
  LOGICAL(SBK),ALLOCATABLE :: lrbuf(:)
  INTEGER(SIK) :: n1
  REQUIRE(myPE%initstat)
  n1=SIZE(lstat,DIM=1)
  ALLOCATE(lrbuf(n1))
  CALL MPI_Allreduce(lstat,lrbuf,n1,MPI_LOGICAL,MPI_LOR, &
      myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_Allreduce returned an error!')
  ELSE
    lstat(:)=lrbuf(:)
  ENDIF
  DEALLOCATE(lrbuf)
#endif
ENDSUBROUTINE trueForAny_SBK1_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a logical 'or'
!> operation for a 2D array logical.
!> @param myPE the MPI parallel environment
!> @param lstat logical that is set to the status of the logical 'or' operation
!>
SUBROUTINE trueForAny_SBK2_MPI_Env_type(myPE,lstat)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  LOGICAL(SBK),INTENT(INOUT) :: lstat(:,:)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='trueForAny_SBK2_MPI_Env_type'
  LOGICAL(SBK),ALLOCATABLE :: lrbuf(:,:)
  INTEGER(SIK) :: n1,n2
  REQUIRE(myPE%initstat)
  n1=SIZE(lstat,DIM=1)
  n2=SIZE(lstat,DIM=2)
  ALLOCATE(lrbuf(n1,n2))
  CALL MPI_Allreduce(lstat,lrbuf,n1*n2,MPI_LOGICAL,MPI_LOR, &
      myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_Allreduce returned an error!')
  ELSE
    lstat(:,:)=lrbuf(:,:)
  ENDIF
  DEALLOCATE(lrbuf)
#endif
ENDSUBROUTINE trueForAny_SBK2_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a logical 'or'
!> operation for a 3D array logical.
!> @param myPE the MPI parallel environment
!> @param lstat logical that is set to the status of the logical 'or' operation
!>
SUBROUTINE trueForAny_SBK3_MPI_Env_type(myPE,lstat)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  LOGICAL(SBK),INTENT(INOUT) :: lstat(:,:,:)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='trueForAny_SBK3_MPI_Env_type'
  LOGICAL(SBK),ALLOCATABLE :: lrbuf(:,:,:)
  INTEGER(SIK) :: n1,n2,n3
  REQUIRE(myPE%initstat)
  n1=SIZE(lstat,DIM=1)
  n2=SIZE(lstat,DIM=2)
  n3=SIZE(lstat,DIM=3)
  ALLOCATE(lrbuf(n1,n2,n3))
  CALL MPI_Allreduce(lstat,lrbuf,n1*n2*n3,MPI_LOGICAL,MPI_LOR, &
      myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_Allreduce returned an error!')
  ELSE
    lstat(:,:,:)=lrbuf(:,:,:)
  ENDIF
  DEALLOCATE(lrbuf)
#endif
ENDSUBROUTINE trueForAny_SBK3_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_Allreduce and performs a logical 'or'
!> operation for a 4D array logical.
!> @param myPE the MPI parallel environment
!> @param lstat logical that is set to the status of the logical 'or' operation
!>
SUBROUTINE trueForAny_SBK4_MPI_Env_type(myPE,lstat)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  LOGICAL(SBK),INTENT(INOUT) :: lstat(:,:,:,:)
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='trueForAny_SBK4_MPI_Env_type'
  LOGICAL(SBK),ALLOCATABLE :: lrbuf(:,:,:,:)
  INTEGER(SIK) :: n1,n2,n3,n4
  REQUIRE(myPE%initstat)
  n1=SIZE(lstat,DIM=1)
  n2=SIZE(lstat,DIM=2)
  n3=SIZE(lstat,DIM=3)
  n4=SIZE(lstat,DIM=4)
  ALLOCATE(lrbuf(n1,n2,n3,n4))
  CALL MPI_Allreduce(lstat,lrbuf,n1*n2*n3*n4,MPI_LOGICAL,MPI_LOR, &
      myPE%comm,mpierr)
  IF(mpierr /= MPI_SUCCESS) THEN
    CALL eParEnv%raiseError(modName//'::'// &
        myName//' - call to MPI_Allreduce returned an error!')
  ELSE
    lstat(:,:,:,:)=lrbuf(:,:,:,:)
  ENDIF
  DEALLOCATE(lrbuf)
#endif
ENDSUBROUTINE trueForAny_SBK4_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_scan on MPI_INTEGER data for operation MPI_SUM
!> @param myPE the MPI parallel environment
!> @param buffer_in the input buffer
!> @param buffer_out the output buffer
!> @param n the size of the input and output buffers
!>
SUBROUTINE scanSum_SIK0_MPI_Env_type(myPE,n,buffer_in,buffer_out)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: n
  INTEGER(SIK),INTENT(IN) :: buffer_in
  INTEGER(SIK),INTENT(OUT) :: buffer_out
  !
  INTEGER(SIK) :: buf_in(1),buf_out(1)

  REQUIRE(myPE%initstat)

  buf_in(1)=buffer_in
  CALL myPE%scanSum_SIK_MPI_Env_type(1,buf_in,buf_out)
  buffer_out=buf_out(1)

ENDSUBROUTINE scanSum_SIK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_scan on MPI_INTEGER data for operation MPI_SUM
!> @param myPE the MPI parallel environment
!> @param buffer_in the input buffer
!> @param buffer_out the output buffer
!> @param n the size of the input and output buffers
!>
SUBROUTINE scanSum_SIK_MPI_Env_type(myPE,n,buffer_in,buffer_out)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: n
  INTEGER(SIK),INTENT(IN) :: buffer_in(*)
  INTEGER(SIK),INTENT(OUT) :: buffer_out(*)

  REQUIRE(myPE%initstat)

#ifdef HAVE_MPI
  CALL MPI_scan(buffer_in,buffer_out,n,MPI_INTEGER,MPI_SUM,myPE%comm,mpierr)
#else
  buffer_out(1:n)=buffer_in(1:n)
#endif

ENDSUBROUTINE scanSum_SIK_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_scan on MPI_REAL data for operation MPI_SUM
!> @param myPE the MPI parallel environment
!> @param buffer_in the input buffer
!> @param buffer_out the output buffer
!> @param n the size of the input and output buffers
!>
SUBROUTINE scanSum_SRK0_MPI_Env_type(myPE,n,buffer_in,buffer_out)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: n
  REAL(SRK),INTENT(IN) :: buffer_in
  REAL(SRK),INTENT(OUT) :: buffer_out
  !
  REAL(SRK) :: buf_in(1),buf_out(1)

  REQUIRE(myPE%initstat)

  buf_in(1)=buffer_in
  CALL myPE%scanSum_SRK_MPI_Env_type(1,buf_in,buf_out)
  buffer_out=buf_out(1)

ENDSUBROUTINE scanSum_SRK0_MPI_Env_type
!
!-------------------------------------------------------------------------------
!> @brief Wrapper routine calls MPI_scan on MPI_REAL data for operation MPI_SUM
!> @param myPE the MPI parallel environment
!> @param buffer_in the input buffer
!> @param buffer_out the output buffer
!> @param n the size of the input and output buffers
!>
SUBROUTINE scanSum_SRK_MPI_Env_type(myPE,n,buffer_in,buffer_out)
  CLASS(MPI_EnvType),INTENT(IN) :: myPE
  INTEGER(SIK),INTENT(IN) :: n
  REAL(SRK),INTENT(IN) :: buffer_in(*)
  REAL(SRK),INTENT(OUT) :: buffer_out(*)

  REQUIRE(myPE%initstat)

#ifdef HAVE_MPI
  CALL MPI_scan(buffer_in,buffer_out,n,MPI_REAL,MPI_SUM,myPE%comm,mpierr)
#else
  buffer_out(1:n)=buffer_in(1:n)
#endif

ENDSUBROUTINE scanSum_SRK_MPI_Env_type
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
!> This uses the optional passed parameter as the number of threads to use in
!> the constructed parallel environment. The following rules are enforced:
!>  - If the requested number of threads is greater than the physical number
!     of processors on the system, a warning is generated. The number of still
!     respected, however, since it may be desired to observe the effects of
!     using too many threads.
!>  - If the requested number of threads is less than the result of
!>    omp_get_max_threads(), omp_set_num_threads() will be called to set the
!>    maximum number of threads to the requested number. It should be noted that
!>    omp_get_max_threads() is not the number of physical cores present, but
!>    rather the number of threads that would be used if a parallel region were
!>    encountered that lacked a num_threads() clause. This is usually the value
!>    of the OMP_NUM_THREADS environment variable at the start of program
!>    execution.  This rule ensures that TPLs that employ OpenMP, but do not
!>    explicitly request a number of threads using a num_threads clause, will
!>    use the number of threads requested by the user, rather than the
!>    value stored in OMP_NUM_THREADS.
SUBROUTINE init_OMP_Env_type(myPE,PEparam)
  CLASS(OMP_EnvType),INTENT(INOUT) :: myPE
  INTEGER(SIK),INTENT(IN),OPTIONAL :: PEparam
  myPE%nproc=1
  myPE%rank=0
  myPE%master=.TRUE.
!$    IF(PRESENT(PEparam)) THEN
!$      IF(PEparam > omp_get_num_procs()) THEN
!$        CALL eParEnv%raiseWarning("More threads requested than actual cores")
!$        myPE%nproc=PEparam
!$      ELSEIF(PEparam == 0) THEN
!$        myPE%nproc=omp_get_max_threads()
!$      ELSE
!$        myPE%nproc=MAX(1,PEparam)
!$      ENDIF
!$    ENDIF
!$    IF(myPE%nproc > max_threads_requested) THEN
!$      max_threads_requested = myPE%nproc
!$      CALL omp_set_num_threads(myPE%nproc)
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
  CLASS(ParallelEnvType),INTENT(INOUT) :: myPE
  INTEGER(SIK),INTENT(IN) :: commWorld
  INTEGER(SIK),INTENT(IN) :: nspace
  INTEGER(SIK),INTENT(IN) :: nangle
  INTEGER(SIK),INTENT(IN) :: nenergy
  INTEGER(SIK),INTENT(IN) :: nthreads
  CHARACTER(LEN=12) ::  nproc, selproc
  INTEGER(SIK) :: nerror,commDims(3)
#ifdef HAVE_MPI
  LOGICAL(SBK),DIMENSION(3),PARAMETER :: isPeriodic=(/.FALSE.,.FALSE.,.FALSE./)
  INTEGER(SIK) :: tmpcomm
  LOGICAL(SBK) :: activeCommDim(3)
  CHARACTER(LEN=12) :: smpierr
#endif

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
  myPE%deviceID=-1

ENDSUBROUTINE clear_ParEnvType
!
!-------------------------------------------------------------------------------
!> @brief Sets the device ID for an external accelerator
!> @param this the parallel environment type
!> @param ID the device ID
!>
SUBROUTINE setDeviceID_ParEnvType(this,ID)
  CLASS(ParallelEnvType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: ID

  this%deviceID=ID

ENDSUBROUTINE setDeviceID_ParEnvType
!
!-------------------------------------------------------------------------------
!> @brief Retrieves the device ID for an external accelerator
!> @param this the parallel environment type
!> @param ID the device ID
!>
FUNCTION getDeviceID_ParEnvType(this) RESULT(ID)
  CLASS(ParallelEnvType),INTENT(INOUT) :: this
  INTEGER(SIK) :: ID

  ID=this%deviceID

ENDFUNCTION getDeviceID_ParEnvType
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

