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
      REAL(SRK),POINTER :: thisX(:)
      INTEGER(SIK) :: i
      LOGICAL(SBK) :: match, bool

      ALLOCATE(LinearSolverType_Iterative :: thisLS)

      COMPONENT_TEST('SparseMatrixType')
      !With GMRES
      !The sparse matrix type

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',GMRES)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_WORLD)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',9_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',33_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',9_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',9_SNK)
      CALL pList%validate(pList,optListLS)
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
      SELECTTYPE(A => thisLS%A); TYPE IS(SparseMatrixType)
          CALL A%setShape(1,1, 4.0_SRK)
          CALL A%setShape(1,2,-1.0_SRK)
          CALL A%setShape(1,4,-1.0_SRK)
          CALL A%setShape(2,1,-1.0_SRK)
          CALL A%setShape(2,2, 4.0_SRK)
          CALL A%setShape(2,3,-1.0_SRK)
          CALL A%setShape(2,5,-1.0_SRK)
          CALL A%setShape(3,2,-1.0_SRK)
          CALL A%setShape(3,3, 4.0_SRK)
          CALL A%setShape(3,6,-1.0_SRK)
          CALL A%setShape(4,1,-1.0_SRK)
          CALL A%setShape(4,4, 4.0_SRK)
          CALL A%setShape(4,5,-1.0_SRK)
          CALL A%setShape(4,7,-1.0_SRK)
          CALL A%setShape(5,2,-1.0_SRK)
          CALL A%setShape(5,4,-1.0_SRK)
          CALL A%setShape(5,5, 4.0_SRK)
          CALL A%setShape(5,6,-1.0_SRK)
          CALL A%setShape(5,8,-1.0_SRK)
          CALL A%setShape(6,3,-1.0_SRK)
          CALL A%setShape(6,5,-1.0_SRK)
          CALL A%setShape(6,6, 4.0_SRK)
          CALL A%setShape(6,9,-1.0_SRK)
          CALL A%setShape(7,4,-1.0_SRK)
          CALL A%setShape(7,7, 4.0_SRK)
          CALL A%setShape(7,8,-1.0_SRK)
          CALL A%setShape(8,5,-1.0_SRK)
          CALL A%setShape(8,7,-1.0_SRK)
          CALL A%setShape(8,8, 4.0_SRK)
          CALL A%setShape(8,9,-1.0_SRK)
          CALL A%setShape(9,6,-1.0_SRK)
          CALL A%setShape(9,8,-1.0_SRK)
          CALL A%setShape(9,9, 4.0_SRK)
      ENDSELECT

      ! build X0 and set it to a vector orthogonal to the solution
      ! This forces GMRES to take longer and thus require a restart,
      ! which tests this portion of the solver
      ALLOCATE(thisX(9))
      thisX=0.0_SRK
      thisX(1)=1.0_SRK
      thisX(9)=-1.0_SRK

      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setX0(thisX)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1.0E-9_SRK,1000_SIK,3_SIK)
      ENDSELECT


      !solve it
      CALL thisLS%solve()

      !Store expected solution (from MATLAB) in B
      ALLOCATE(thisB(9))
      thisB(1)=0.6875_SRK
      thisB(2)=0.875_SRK
      thisB(3)=0.6875_SRK
      thisB(4)=0.875_SRK
      thisB(5)=1.125_SRK
      thisB(6)=0.875_SRK
      thisB(7)=0.6875_SRK
      thisB(8)=0.875_SRK
      thisB(9)=0.6875_SRK
      !multiply by 10,000 so we can match first five places.
      thisB=10000.0_SRK*thisB
      match=.TRUE.
      DO i=1,SIZE(thisB)
        SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(X%n))
          CALL X%get(dummyvec)
          IF(NINT(thisB(i)) /= NINT(10000.0_SRK*dummyvec(i))) THEN
            match=.FALSE.
            EXIT
          ENDIF
        ENDSELECT
      ENDDO
      ASSERT(match,'CALL Iterative%solve() -GMRES FAILED!')

      DEALLOCATE(thisB)
      CALL thisLS%clear()
      DEALLOCATE(thisX)

    !test with A being densesquare
      COMPONENT_TEST('DenseRectMatrixType')
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',GMRES)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_WORLD)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',DENSESQUARE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',9_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->isSym',.TRUE.)
      CALL pList%add('LinearSolverType->x->VectorType->n',9_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',9_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      DO i=1,9
        SELECTTYPE(A => thisLS%A); TYPE IS(DenseSquareMatrixType)
          CALL A%set(i,i,4.0_SRK)
          IF((i < 9).AND.((i /= 3).AND.(i /= 6))) THEN
            CALL A%set(i,i+1,-1.0_SRK)
          ENDIF
          IF(i < 7) THEN
            CALL A%set(i,i+3,-1.0_SRK)
          ENDIF
        ENDSELECT
      ENDDO

      ! build X0 and set it to a vector orthogonal to the solution
      ! This forces GMRES to take longer and thus require a restart,
      ! which tests this portion of the solver
      ALLOCATE(thisX(9))
      thisX=0.0_SRK
      thisX(1)=1.0_SRK
      thisX(9)=-1.0_SRK

      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT

      !set b
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1.0E-9_SRK,1000_SIK,3_SIK)
      ENDSELECT

      !solve
      CALL thisLS%solve()
      ALLOCATE(thisB(9))
      thisB(1)=0.6875_SRK
      thisB(2)=0.875_SRK
      thisB(3)=0.6875_SRK
      thisB(4)=0.875_SRK
      thisB(5)=1.125_SRK
      thisB(6)=0.875_SRK
      thisB(7)=0.6875_SRK
      thisB(8)=0.875_SRK
      thisB(9)=0.6875_SRK
      thisB=thisB*10000._SRK
      match=.TRUE.
      DO i=1,SIZE(thisB)
        SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(X%n))
          CALL X%get(dummyvec)
          IF(NINT(thisB(i)) /= NINT(10000.0_SRK*dummyvec(i))) THEN
            match=.FALSE.
            EXIT
          ENDIF
        ENDSELECT
      ENDDO
      ASSERT(match, 'Iterative%solve() - GMRES')
      !test to see how it performs with an already decomposed M
      !reset X to 1.0s
      match=.TRUE.
      DO i=1,SIZE(thisB)
        SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
          CALL X%set(1.0_SRK)
          CALL thisLS%solve()
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(X%n))
          CALL X%get(dummyvec)
          IF(NINT(thisB(i)) /= NINT(10000.0_SRK*dummyvec(i))) THEN
            match=.FALSE.
            EXIT
          ENDIF
        ENDSELECT
      ENDDO
      ASSERT(match, 'Iterative%solve() - GMRES')
      CALL thisLS%A%clear()
      CALL thisLS%clear()

      ! TriDiagonal matrix, it will go to LU method
      COMPONENT_TEST('TriDiagMatrixType')

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',GMRES)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_WORLD)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',TRIDIAG)
      CALL pList%add('LinearSolverType->A->MatrixType->n',3_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->isSym',.TRUE.)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',3_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      SELECTTYPE(A => thisLS%A); TYPE IS(TriDiagMatrixType)
        CALL A%set(1,1, 4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2, 4._SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,3, 4._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT

      CALL thisLS%solve()

      bool = thisLS%info == 0
      ASSERT(bool, 'Iterative%solve() - GMRES')
      CALL thisLS%A%clear()
      CALL thisLS%clear()
      DEALLOCATE(thisX)

    !DenseRect matrix

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',GMRES)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_WORLD)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',DENSERECT)
      CALL pList%add('LinearSolverType->A->MatrixType->n',3_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->m',2_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',2_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',3_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      ! A=[1 1]  b=[1]
      !   [1 2]    [2]
      !   [1 3]    [2]
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseRectMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,2,1._SRK)
        CALL A%set(2,1,1._SRK)
        CALL A%set(2,2,2._SRK)
        CALL A%set(3,1,1._SRK)
        CALL A%set(3,2,3._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(2,2._SRK)
      ENDSELECT

      ! initialize X0
      ALLOCATE(thisX(2))
      thisX=1.0_SRK

      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1.0E-9_SRK,1000_SIK,30_SIK)
      ENDSELECT

      !Solve it
      CALL thisLS%solve()

      bool = thisLS%info == 0
      ASSERT(bool, 'Iterative%solve() -GMRES method')

      DEALLOCATE(thisB)
      DEALLOCATE(thisX)
      CALL thisLS%clear()

#ifdef FUTILITY_HAVE_PETSC
      !With GMRES
      !The sparse matrix type
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',PETSC)
      CALL pList%add('LinearSolverType->solverMethod',GMRES)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_WORLD)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',9_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->isSym',.TRUE.)
      CALL pList%add('LinearSolverType->x->VectorType->n',9_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',9_SNK)
      CALL pList%validate(pList,optListLS)
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
      SELECTTYPE(A => thisLS%A); TYPE IS(PETSCMatrixType)
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
      ENDSELECT

      ! build X0 and set it to a vector orthogonal to the solution
      ! This forces GMRES to take longer and thus require a restart,
      ! which tests this portion of the solver
      ALLOCATE(thisX(9))
      thisX=0.0_SRK
      thisX(1)=1.0_SRK
      thisX(9)=-1.0_SRK

      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setX0(thisX)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(PETScVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1.0E-9_SRK,1000_SIK,3_SIK)
      ENDSELECT

      !solve it
      CALL thisLS%solve()

      !Store expected solution (from MATLAB) in B
      ALLOCATE(thisB(9))
      thisB(1)=0.6875_SRK
      thisB(2)=0.875_SRK
      thisB(3)=0.6875_SRK
      thisB(4)=0.875_SRK
      thisB(5)=1.125_SRK
      thisB(6)=0.875_SRK
      thisB(7)=0.6875_SRK
      thisB(8)=0.875_SRK
      thisB(9)=0.6875_SRK
      !multiply by 10,000 so we can match first five places.
      thisB=10000.0_SRK*thisB
      match=.TRUE.
      DO i=1,SIZE(thisB)
        SELECTTYPE(X => thisLS%X); TYPE IS(PETScVectorType)
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(X%n))
          CALL X%get(dummyvec)
          IF(NINT(thisB(i)) /= NINT(10000.0_SRK*dummyvec(i))) THEN
            match=.FALSE.
            EXIT
          ENDIF
        ENDSELECT
      ENDDO
      ASSERT(match, 'PETScIterative%solve() -BICGSTAB')

      DEALLOCATE(thisX)
      DEALLOCATE(thisB)
      CALL thisLS%A%clear()
      CALL thisLS%clear()

    !test with A being densesquare

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',PETSC)
      CALL pList%add('LinearSolverType->solverMethod',GMRES)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_WORLD)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',DENSESQUARE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',9_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->isSym',.TRUE.)
      CALL pList%add('LinearSolverType->x->VectorType->n',9_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',9_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      DO i=1,9
        SELECTTYPE(A => thisLS%A); TYPE IS(PETScMatrixType)
          CALL A%set(i,i,4.0_SRK)
          IF((i < 9).AND.((i /= 3).AND.(i /= 6))) THEN
            CALL A%set(i,i+1,-1.0_SRK)
          ENDIF
          IF(i < 7) THEN
            CALL A%set(i,i+3,-1.0_SRK)
          ENDIF
        ENDSELECT
      ENDDO

      ! build X0 and set it to a vector orthogonal to the solution
      ! This forces GMRES to take longer and thus require a restart,
      ! which tests this portion of the solver
      ALLOCATE(thisX(9))
      thisX=0.0_SRK
      thisX(1)=1.0_SRK
      thisX(9)=-1.0_SRK
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(PETScVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1.0E-9_SRK,1000_SIK,3_SIK)
      ENDSELECT

      !solve
      CALL thisLS%solve()
      ALLOCATE(thisB(9))
      thisB(1)=0.6875_SRK
      thisB(2)=0.875_SRK
      thisB(3)=0.6875_SRK
      thisB(4)=0.875_SRK
      thisB(5)=1.125_SRK
      thisB(6)=0.875_SRK
      thisB(7)=0.6875_SRK
      thisB(8)=0.875_SRK
      thisB(9)=0.6875_SRK
      thisB=thisB*10000._SRK
      match=.TRUE.
      DO i=1,SIZE(thisB)
        SELECTTYPE(X => thisLS%X); TYPE IS(PETScVectorType)
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(X%n))
          CALL X%get(dummyvec)
          IF(NINT(thisB(i)) /= NINT(10000.0_SRK*dummyvec(i))) THEN
            match=.FALSE.
            EXIT
          ENDIF
        ENDSELECT
      ENDDO
      ASSERT(match, 'PETScIterative%solve() - GMRES')
      !test to see how it performs with an already decomposed M
      !reset X to 1.0s
      match=.TRUE.
      DO i=1,SIZE(thisB)
        SELECTTYPE(X => thisLS%X); TYPE IS(PETScVectorType)
          CALL X%set(1.0_SRK)
          CALL thisLS%solve()
          IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
          ALLOCATE(dummyvec(X%n))
          CALL X%get(dummyvec)
          IF(NINT(thisB(i)) /= NINT(10000.0_SRK*dummyvec(i))) THEN
            match=.FALSE.
            EXIT
          ENDIF
        ENDSELECT
      ENDDO
      ASSERT(match, 'PETScIterative%solve() - GMRES')
      CALL thisLS%A%clear()
      DEALLOCATE(thisX)

#endif

      CALL thisLS%clear()

    ENDSUBROUTINE testIterativeSolve_GMRES

ENDPROGRAM testLinearSolverParallel
