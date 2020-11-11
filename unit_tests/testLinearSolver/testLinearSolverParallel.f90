!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testLinearSolverParallel
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE ExceptionHandler
USE ParameterLists
USE ParallelEnv
USE VectorTypes
USE MatrixTypes
USE PreconditionerTypes
USE LinearSolverTypes

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if (((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>6)) || (PETSC_VERSION_MAJOR>=4))
USE PETSCVEC
#endif
#endif

IMPLICIT NONE

!TYPE(ExceptionHandlerType),TARGET :: e
#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
#undef IS
PetscErrorCode  :: ierr
#else
#ifdef HAVE_MPI
#include <mpif.h>
INTEGER(SIK) :: ierr
#endif
#endif

#ifdef HAVE_MPI
CALL MPI_Init(ierr)
#endif
#ifdef FUTILITY_HAVE_PETSC
CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif
!Configure exception handler for test
!CALL e%setStopOnError(.FALSE.)
!CALL e%setQuietMode(.TRUE.)
!CALL eParams%addSurrogate(e)
!CALL eLinearSolverType%addSurrogate(e)
!CALL mpiTestEnv%init(PE_COMM_WORLD)

CREATE_TEST('Test Linear Solvers')

REGISTER_SUBTEST('testIterativeSolve_GMRES',testIterativeSolve_GMRES)

FINALIZE_TEST()

#ifdef FUTILITY_HAVE_PETSC
  CALL PetscFinalize(ierr)
#else
#ifdef HAVE_MPI
  CALL MPI_Finalize(ierr)
#endif
#endif
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testIterativeSolve_GMRES()
#ifdef HAVE_MPI
  CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
  TYPE(ParamType) :: pList
  REAL(SRK),ALLOCATABLE :: thisB(:),dummyvec(:)
  INTEGER(SIK) :: rank,mpierr

  ALLOCATE(LinearSolverType_Iterative :: thisLS)
  CALL MPI_Comm_rank(PE_COMM_WORLD,rank,mpierr)

  COMPONENT_TEST('DistributedBandedMatrixType')
  !With GMRES
  !The sparse matrix type

  ! initialize linear system
  CALL pList%clear()
  CALL pList%add('LinearSolverType->TPLType',LINSYS_NATIVE)
  CALL pList%add('LinearSolverType->solverMethod',GMRES)
  CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_WORLD)
  CALL pList%add('LinearSolverType->numberOMP',1)
  CALL pList%add('LinearSolverType->timerName','testTimer')
  CALL pList%add('LinearSolverType->matType',DISTRIBUTED_BANDED)
  CALL pList%add('LinearSolverType->A->MatrixType->n',9)
  CALL pList%add('LinearSolverType->A->MatrixType->m',9)
  CALL pList%add('LinearSolverType->A->MatrixType->nnz',33)
  CALL pList%add('LinearSolverType->x->VectorType->n',9)
  CALL pList%add('LinearSolverType->b->VectorType->n',9)
  CALL pList%add('LinearSolverType->PC->PreCondType->pcType','NOPC')
  CALL pList%validate(pList)
  CALL thisLS%init(pList)

  !A =  4    -1     0    -1     0     0     0     0     0
  !    -1     4    -1     0    -1     0     0     0     0
  !     0    -1     4     0     0    -1     0     0     0
  !    -1     0     0     4    -1     0    -1     0     0
  !     0    -1     0    -1     4    -1     0    -1     0
  !     0     0    -1     0    -1     4     0     0    -1
  !     0     0     0    -1     0     0     4    -1     0
  !     0     0     0     0    -1     0    -1     4    -1
  !     0     0     0     0     0    -1     0    -1     4
  SELECTTYPE(A => thisLS%A); TYPE IS(DistributedBandedMatrixType)
    CALL A%set(1,1, 4.0_SRK)
    CALL A%set(1,2,-1.0_SRK)
    CALL A%set(1,4,-1.0_SRK)
    CALL A%set(2,1,-1.0_SRK)
    CALL A%set(2,2, 4.0_SRK)
    CALL A%set(2,3,-1.0_SRK)
    CALL A%set(2,5,-1.0_SRK)
    CALL A%set(3,2,-1.0_SRK)
    CALL A%set(3,3, 4.0_SRK)
    CALL A%set(3,6,-1.0_SRK)
    CALL A%set(4,1,-1.0_SRK)
    CALL A%set(4,4, 4.0_SRK)
    CALL A%set(4,5,-1.0_SRK)
    CALL A%set(4,7,-1.0_SRK)
    CALL A%set(5,2,-1.0_SRK)
    CALL A%set(5,4,-1.0_SRK)
    CALL A%set(5,5, 4.0_SRK)
    CALL A%set(5,6,-1.0_SRK)
    CALL A%set(5,8,-1.0_SRK)
    CALL A%set(6,3,-1.0_SRK)
    CALL A%set(6,5,-1.0_SRK)
    CALL A%set(6,6, 4.0_SRK)
    CALL A%set(6,9,-1.0_SRK)
    CALL A%set(7,4,-1.0_SRK)
    CALL A%set(7,7, 4.0_SRK)
    CALL A%set(7,8,-1.0_SRK)
    CALL A%set(8,5,-1.0_SRK)
    CALL A%set(8,7,-1.0_SRK)
    CALL A%set(8,8, 4.0_SRK)
    CALL A%set(8,9,-1.0_SRK)
    CALL A%set(9,6,-1.0_SRK)
    CALL A%set(9,8,-1.0_SRK)
    CALL A%set(9,9, 4.0_SRK)
    CALL A%assemble()
  ENDSELECT

  ! Use default initial guess (all ones)
  SELECTTYPE(x => thisLS%x); TYPE IS(NativeDistributedVectorType)
    x%b = 1.0_SRK
  ENDSELECT

  SELECTTYPE(b => thisLS%b); TYPE IS(NativeDistributedVectorType)
    b%b = 1.0_SRK
  ENDSELECT

  !set iterations and convergence information and build/set M
  SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
    thisLS%hasX0 = .TRUE.
    CALL thisLS%setConv(2,1.0E-6_SRK,1.0E-6_SRK,20,4)
  ENDSELECT


  !solve it
  CALL thisLS%solve()

  !Store expected solution (from MATLAB) in B
  IF(rank == 0) THEN
    ALLOCATE(dummyvec(5))
    ALLOCATE(thisB(5))
    thisB(:) = (/0.6875_SRK,0.875_SRK,0.6875_SRK,0.875_SRK,1.125_SRK/)
  ELSE
    ALLOCATE(dummyvec(4))
    ALLOCATE(thisB(4))
    thisB(:)=(/0.875_SRK,0.6875_SRK,0.875_SRK,0.6875_SRK/)
  ENDIF
  CALL thisLS%X%get(dummyvec)
  ASSERT(ALL(SOFTEQ(thisB(:),dummyvec(:),1.0E-05_SRK)), 'CALL Iterative%solve() -GMRES FAILED!')

  DEALLOCATE(thisB)
  DEALLOCATE(dummyvec)
  CALL thisLS%clear()
  CALL plist%clear()

ENDSUBROUTINE testIterativeSolve_GMRES
#endif
ENDPROGRAM testLinearSolverParallel
