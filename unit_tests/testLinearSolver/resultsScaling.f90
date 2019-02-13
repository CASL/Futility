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
  USE BLAS
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes
  USE PreconditionerTypes
  USE LinearSolverTypes
  USE trilinos_interfaces
  USE Times
  USE Allocs
  USE Strings
  USE IOUtil

  IMPLICIT NONE

  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(MPI_EnvType) :: mpiTestEnv
  TYPE(ParamType) :: pList, optListLS, optListMat, vecPList

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
#undef IS
  PetscErrorCode  :: ierr

  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

  !> set up default parameter list
  CALL optListLS%clear()
  CALL optListLS%add('LinearSolverType->TPLType',NATIVE)
  CALL optListLS%add('LinearSolverType->solverMethod',1_SNK) ! GE or BICGSTAB
  CALL optListLS%add('LinearSolverType->MPI_Comm_ID',PE_COMM_WORLD)
  CALL optListLS%add('LinearSolverType->numberOMP',1_SNK)
  CALL optListLS%add('LinearSolverType->timerName','LinearSolver Timer')
  CALL optListLS%add('LinearSolverType->matType',SPARSE)
  ! set parameters for matrices
  CALL optListLS%add('LinearSolverType->A->MatrixType->n',-1_SNK)
  CALL optListLS%add('LinearSolverType->A->MatrixType->nnz',-1_SNK)
  CALL optListLS%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
  ! set parameters for vectors
  CALL optListLS%add('LinearSolverType->x->VectorType->n',-1_SNK)
  CALL optListLS%add('LinearSolverType->b->VectorType->n',-1_SNK)

  ! Set up vector parameter list
  CALL vecPList%add('VectorType -> n',2)
  CALL vecPList%add('VectorType -> MPI_Comm_ID',PE_COMM_WORLD)

  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL eParams%addSurrogate(e)
  CALL eLinearSolverType%addSurrogate(e)
  CALL mpiTestEnv%init(PE_COMM_WORLD)

  CREATE_TEST('Test Linear Solvers')

  REGISTER_SUBTEST('testIterativeSolve_GMRES',testIterativeSolve_GMRES)

  FINALIZE_TEST()

  CALL pList%clear()
  CALL vecPList%clear()
  CALL optListMat%clear()
  CALL optListLS%clear()

#ifdef FUTILITY_HAVE_PETSC
  CALL PetscFinalize(ierr)
#else
  CALL mpiTestEnv%finalize()
#endif
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIterativeSolve_GMRES()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      REAL(SRK),ALLOCATABLE :: thisB(:),dummyvec(:)
      REAL(SRK),POINTER :: initGuess(:)
      REAL(SRK) :: timetaken
      INTEGER(SIK) :: i,n,nnz,time1,time2,clock_rate,gIt,thisIters
      INTEGER(SIK) :: gridSizeVals(10)
      INTEGER(SIK) :: xCoord,yCoord,gridSize

      ALLOCATE(LinearSolverType_Iterative :: thisLS)

      COMPONENT_TEST('Scaling Problem Size')

      ! Desired problem sizes
      ! 16, 256, 1024, 2048, 4096, 8192
      ! 4x4,16x16,32x32, ?, 64x64, ?
      ! 4,  8,   10,   11,   12,   13

      gridSizeVals = (/4, 16, 32, 64, 96, 128, 160, 192, 224, 256/)

      IF (mpiTestEnv%rank == 0) THEN
        WRITE(*,*) "Matrix Size"," ","Solution Time"," ","Iteration Count"," ","Max error"
      END IF

      DO gIt = 1,SIZE(gridSizeVals)
        gridSize = gridSizeVals(gIt)
        n = gridSize*gridSize
        nnz = 5*(gridSize-2)*(gridSize-2) + 16*(gridSize-2) + 12

        CALL pList%clear()
        CALL pList%add('LinearSolverType->TPLType',NATIVE)
        CALL pList%add('LinearSolverType->solverMethod',GMRES)
        CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_WORLD)
        CALL pList%add('LinearSolverType->numberOMP',1)
        CALL pList%add('LinearSolverType->timerName','testTimer')
        CALL pList%add('LinearSolverType->matType',SPARSE)
        CALL pList%add('LinearSolverType->A->MatrixType->n',n)
        CALL pList%add('LinearSolverType->A->MatrixType->nnz',nnz)
        CALL pList%add('LinearSolverType->x->VectorType->n',n)
        CALL pList%add('LinearSolverType->b->VectorType->n',n)
        CALL pList%validate(pList,optListLS)
        CALL thisLS%init(pList)

        SELECT TYPE(A => thisLS%A); TYPE IS(SparseMatrixType)
          DO i=1,n
            yCoord = (i-1)/gridSize
            xCoord = MOD(i-1,gridSize)
            IF (yCoord > 0) THEN
              CALL A%setShape(i,i-gridSize,-1.0_SRK)
            END IF
            IF (xCoord > 0) THEN
              CALL A%setShape(i, i-1,-1.0_SRK)
            END IF
            CALL A%setShape(i,i,4.0_SRK)
            IF (xCoord < gridSize-1) THEN
              CALL A%setShape(i, i+1,-1.0_SRK)
            END IF
            IF (yCoord < gridSize-1) THEN
              CALL A%setShape(i,i+gridSize,-1.0_SRK)
            END IF
          END DO
        END SELECT

        ALLOCATE(initGuess(n))
        ALLOCATE(dummyvec(n))
        initGuess = 1.0_SRK
        SELECT TYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setX0(initGuess)
        END SELECT

        ALLOCATE(thisB(n))
        thisB = 1.0_SRK
        SELECT TYPE(b => thisLS%b); TYPE IS(RealVectorType)
          CALL b%set(thisB)
        END SELECT

        SELECT TYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setConv(2,1.0E-9_SRK,1.0E-9_SRK,1000000,MIN(n/2,30))
        ENDSELECT

        CALL SYSTEM_CLOCK(time1)
        CALL thisLS%solve()
        !call clock right after parallel part in the SOR iterations, this will give us how long the SOR iterations took to converge
        CALL SYSTEM_CLOCK(time2,clock_rate)
        !calculate time taken in seconds
        timetaken=(time2*1.0_SRK-time1*1.0_SRK)/(clock_rate*1.0_SRK)

        SELECT TYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          thisIters = thisLS%iters
        END SELECT

        SELECT TYPE(A => thisLS%A); TYPE IS(SparseMatrixType)
          SELECT TYPE(sol => thisLS%X); TYPE IS(RealVectorType)
            CALL BLAS_matvec(THISMATRIX=A,X=sol%b,BETA=0.0_SRK,Y=dummyvec)
          END SELECT
        END SELECT

        IF (mpiTestEnv%rank == 0) THEN
          WRITE(*,*) n,timetaken,thisIters,MAXVAL(ABS(dummyvec - 1.0_SRK))
        END IF

        DEALLOCATE(initGuess)
        DEALLOCATE(dummyvec)
        DEALLOCATE(thisB)
        CALL thisLS%clear()

      END DO

    ENDSUBROUTINE testIterativeSolve_GMRES

ENDPROGRAM testLinearSolverParallel
