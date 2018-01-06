!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testLinearSolver
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
#include <finclude/petsc.h>
#undef IS
  PetscErrorCode  :: ierr

  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

  !> set up default parameter list
  CALL optListLS%clear()
  CALL optListLS%add('LinearSolverType->TPLType',NATIVE)
  CALL optListLS%add('LinearSolverType->solverMethod',1_SNK) ! GE or BICGSTAB
  CALL optListLS%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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
  CALL vecPList%add('VectorType -> MPI_Comm_ID',PE_COMM_SELF)

  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL eParams%addSurrogate(e)
  CALL eLinearSolverType%addSurrogate(e)
  CALL mpiTestEnv%init(PE_COMM_SELF)

  CREATE_TEST('Test Linear Solvers')

  REGISTER_SUBTEST('TESTING NORMS PROCEDURE',testNorms)
  REGISTER_SUBTEST('testClear',testClear)
  REGISTER_SUBTEST('testInit',testInit)
  REGISTER_SUBTEST('TestUpdatedA',testUpdatedA)
  REGISTER_SUBTEST('testDirectSolve',testDirectSolve)
  REGISTER_SUBTEST('testQRSolve',testQRSolve)
  REGISTER_SUBTEST('testIterativeOthers',testIterativeOthers)
  REGISTER_SUBTEST('testIterativeSolve_BICGSTAB',testIterativeSolve_BICGSTAB)
  REGISTER_SUBTEST('testiterativeSovle_CGNR',testIterativeSolve_CGNR)
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
    SUBROUTINE testClear()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      LOGICAL(SBK) :: bool
    !test Direct
      ALLOCATE(LinearSolverType_Direct :: thisLS)

      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)

        !first build one by hand to test clear
        thisLS%isInit=.TRUE.
        thisLS%solverMethod=BICGSTAB
        thisLS%info=2
        thisLS%isDecomposed=.TRUE.
        CALL thisLS%MPIparallelEnv%init(PE_COMM_SELF)
        CALL thisLS%OMPparallelEnv%init(1)

        ! initialize matrix A
        ALLOCATE(DenseSquareMatrixType :: thisLS%A)
        CALL pList%clear()
        CALL pList%add('MatrixType->n',2_SNK)
        CALL pList%add('MatrixType->isSym',.TRUE.)
        CALL pList%validate(pList,optListMat)
        CALL thisLS%A%init(pList) !2x2, symmetric

        ! initialize vector X
        CALL vecPList%set('VectorType -> n',2)
        ALLOCATE(RealVectorType :: thisLS%X)
        CALL thisLS%X%init(vecPList)

        ! initialize vector b
        ALLOCATE(RealVectorType :: thisLS%b)
        CALL thisLS%b%init(vecPList)

        ! allocate IPIV
        ALLOCATE(thisLS%IPIV(2))

        ! initialize matrix M
        ALLOCATE(DenseSquareMatrixType :: thisLS%M)
        CALL pList%clear()
        CALL pList%add('MatrixType->n',10_SNK)
        CALL pList%add('MatrixType->isSym',.TRUE.)
        CALL thisLS%M%init(pList)

      ENDSELECT

      !call clear
      CALL thisLS%clear()

      !check results
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        bool = .NOT.thisLS%isInit .AND. thisLS%solverMethod == -1                   &
          .AND. .NOT.thisLS%isDecomposed .AND. .NOT.ASSOCIATED(thisLS%A)            &
          .AND. .NOT.ASSOCIATED(thisLS%X) .AND. thisLS%info == 0                    &
          .AND. .NOT.ASSOCIATED(thisLS%b) .AND. .NOT.ALLOCATED(thisLS%IPIV)         &
          .AND. .NOT.ALLOCATED(thisLS%M) .AND. .NOT.thisLS%MPIparallelEnv%isInit()  &
          .AND. .NOT.thisLS%OMPparallelEnv%isInit()
        ASSERT(bool, 'Direct%clear(...)')
      ENDSELECT
      DEALLOCATE(thisLS)

    !test Iterative
      ALLOCATE(LinearSolverType_Iterative :: thisLS)
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        !first build one by hand to test
        thisLS%isInit=.TRUE.
        thisLS%TPLType=PETSC
        thisLS%solverMethod=1
        thisLS%info=2
        thisLS%normType=2
        thisLS%maxIters=2
        thisLS%iters=2
        thisLS%convTol=2._SRK
        thisLS%residual=2._SRK
        thisLS%isDecomposed=.TRUE.
        CALL thisLS%MPIparallelEnv%init(PE_COMM_SELF)
        CALL thisLS%OMPparallelEnv%init(1)
#ifdef FUTILITY_HAVE_PETSC
        CALL KSPCreate(thisLS%MPIparallelEnv%comm,thisLS%ksp,ierr)
#endif

        ! initialize matrix A
        ALLOCATE(DenseSquareMatrixType :: thisLS%A)
        CALL pList%clear()
        CALL pList%add('MatrixType->n',2_SNK)
        CALL pList%add('MatrixType->isSym',.TRUE.)
        CALL pList%validate(pList,optListMat)
        CALL thisLS%A%init(pList) !2x2, symmetric

        ! initialize preconditioner
        ALLOCATE(ILU_PreCondType :: thisLS%PreCondType)
        CALL thisLS%setupPC()

        ! initialize vector X
        CALL vecPList%set('VectorType -> n',2)
        ALLOCATE(RealVectorType :: thisLS%X)
        CALL thisLS%X%init(vecPList)

        ! initialize vector b
        ALLOCATE(RealVectorType :: thisLS%b)
        CALL thisLS%b%init(vecPList)

        ! initialize matrix M
        ALLOCATE(DenseSquareMatrixType :: thisLS%M)
        CALL pList%clear()
        CALL pList%add('MatrixType->n',10_SNK)
        CALL pList%add('MatrixType->isSym',.TRUE.)
        CALL thisLS%M%init(pList)
      ENDSELECT

      CALL thisLS%clear()

      !check results
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        bool=thisLS%isInit .OR.thisLS%solverMethod == 1                  &
            .OR. ALLOCATED(thisLS%M) .OR. ASSOCIATED(thisLS%A)           &
            .OR. ASSOCIATED(thisLS%X) .OR. thisLS%info /= 0               &
!            .OR. thisLS%MPIparallelEnv%isInit()                          &
!            .OR. thisLS%OMPparallelEnv%isInit()                          &
            .OR. thisLS%normType == 2 .OR. thisLS%maxIters == 2          &
            .OR. thisLS%iters == 2 .OR. thisLS%isDecomposed              &
            .OR. thisLS%residual == 2._SRK .OR. thisLS%convTol == 2._SRK &
            .OR. ALLOCATED(thisLS%PreCondType)
        ASSERT(.NOT.(bool),'CALL Iterative%clear() FAILED!')
      ENDSELECT
      CALL thisLS%clear()
      DEALLOCATE(thisLS)

    ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInit()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      INTEGER(SIK) :: nerrors1,nerrors2
      LOGICAL(SBK) :: bool
   !test Direct
      ALLOCATE(LinearSolverType_Direct :: thisLS)

      !Bad input
      CALL pList%clear()
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',-1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->n',1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',1_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',1_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL thisLS%clear()

      CALL pList%clear()
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->A->MatrixType->n',1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',1_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',1_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL thisLS%clear()

      CALL pList%clear()
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->A->MatrixType->n',1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',1_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',1_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL thisLS%clear()

      !first test a correct use case, with timer name
      CALL pList%clear()
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->A->MatrixType->n',1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',1_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',1_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        bool = (thisLS%isInit .OR. thisLS%solverMethod /= 1        &
           .OR. .NOT.thisLS%MPIparallelEnv%isInit()                &
           .OR. .NOT.thisLS%OMPparallelEnv%isInit()                &
           .OR. thisLS%SolveTime%getTimerName() /= 'testTimer')
        ASSERT(bool, 'Direct%init(...)')
      ENDSELECT
      CALL thisLS%clear()

      !first test a correct use case, with no timer name
      CALL pList%clear()
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->n',1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',1_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',1_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Direct)
        bool = (thisLS%isInit .AND. thisLS%solverMethod == 1 &
           .AND. thisLS%MPIparallelEnv%isInit()                &
           .AND. thisLS%OMPparallelEnv%isInit()                &
           .AND. thisLS%SolveTime%getTimerName() == 'LinearSolver Timer')
        ASSERT(bool, 'Direct%init(...)')
      ENDSELECT
      CALL thisLS%clear()
      DEALLOCATE(thisLS)

    !test Iterative
      ALLOCATE(LinearSolverType_Iterative :: thisLS)

      !Bad input
      CALL pList%clear()
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',-1_SNK)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->A->MatrixType->n',1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',1_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',1_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL thisLS%clear()

      CALL pList%clear()
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->A->MatrixType->n',1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',1_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',1_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL thisLS%clear()

      CALL pList%clear()
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->A->MatrixType->n',1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',1_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',1_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL thisLS%clear()

      !first test a correct use case, with timer name
      CALL pList%clear()
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->A->MatrixType->n',2_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',2_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',2_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',2_SNK)
      CALL pList%add('LinearSolverType->PCType','ILU')
      CALL pList%add('LinearSolverType->PCIters',-1)
      CALL pList%add('LinearSolverType->PCSetup',0)

      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      SELECTTYPE(thisLS); TYPE IS(LinearSolvertype_Iterative)

        bool = (thisLS%isInit .AND. thisLS%solverMethod == 1 &
           .AND. thisLS%MPIparallelEnv%isInit() &
           .AND. thisLS%OMPparallelEnv%isInit() &
           .AND. thisLS%SolveTime%getTimerName() == 'testTimer')
        ASSERT(bool, 'Iterative%init(...)')
          ! Check uninitialized A
        CALL thisLS%A%clear()
        nerrors1=e%getCounter(EXCEPTION_ERROR)
        CALL thisLS%setupPC()
        nerrors2=e%getCounter(EXCEPTION_ERROR)
        ASSERT(nerrors2 == nerrors1+1,'LS%setupPC PC%A%isInit check')
        FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1
        ! Check deallocated A
        DEALLOCATE(thisLS%A)
        nerrors1=e%getCounter(EXCEPTION_ERROR)
        CALL thisLS%setupPC()
        nerrors2=e%getCounter(EXCEPTION_ERROR)
        ASSERT(nerrors2 == nerrors1+1,'LS%setupPC ALLOCATED(PC%A) check')
        FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1
        ! Check uninitialized linear solver
        CALL thisLS%clear()
        nerrors1=e%getCounter(EXCEPTION_ERROR)
        CALL thisLS%setupPC()
        nerrors2=e%getCounter(EXCEPTION_ERROR)
        ASSERT(nerrors2 == nerrors1+1,'LS%setupPC ALLOCATED(PC%A) check')
        FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1
!        ENDIF
      ENDSELECT
      CALL thisLS%clear()

      !first test a correct use case, with no timer name
      CALL pList%clear()
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->n',2_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',2_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',2_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',2_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Iterative)
        bool = (thisLS%isInit .AND. thisLS%solverMethod == 1 &
           .AND. thisLS%MPIparallelEnv%isInit() &
           .AND. thisLS%OMPparallelEnv%isInit() &
           .AND. thisLS%SolveTime%getTimerName() == 'LinearSolver Timer')
        ASSERT(bool, 'Iterative%init(...)')
      ENDSELECT
      CALL thisLS%clear()
      DEALLOCATE(thisLS)

    ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testUpdatedA()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      LOGICAL(SBK) :: bool

    !test Direct
      ALLOCATE(LinearSolverType_Direct :: thisLS)

      ! initialize matrix M
      ALLOCATE(DenseSquareMatrixType :: thisLS%M)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL thisLS%M%init(pList)
      thisLS%isDecomposed=.TRUE.

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',LU)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->A->MatrixType->n',1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',1_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',1_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        ALLOCATE(thisLS%IPIV(10))
      ENDSELECT
      CALL thisLS%updatedA()
      !Check
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        bool = .NOT.thisLS%isDecomposed
        ASSERT(bool, 'Direct%updatedA()')
      ENDSELECT
      CALL thisLS%clear()
      DEALLOCATE(thisLS)

    !test Iterative
      ALLOCATE(LinearSolverType_Iterative :: thisLS)

      ! initialize matrix M
      ALLOCATE(DenseSquareMatrixType :: thisLS%M)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->isSym',.TRUE.)
      CALL thisLS%M%init(pList)
      thisLS%isDecomposed=.TRUE.

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->A->MatrixType->n',1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',1_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',1_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      CALL thisLS%updatedA()

      !Check
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        bool = .NOT.thisLS%isDecomposed
        ASSERT(bool, 'Iterative%updatedA()')
      ENDSELECT
      CALL thisLS%clear()
      DEALLOCATE(thisLS)

    ENDSUBROUTINE testUpdatedA
!
!-------------------------------------------------------------------------------
    SUBROUTINE testDirectSolve()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      REAL(SRK),ALLOCATABLE :: dummyvec(:)
      LOGICAL(SBK) :: bool

      ALLOCATE(LinearSolverType_Direct :: thisLS)

    !Test GE (Dense-Square)
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',GE)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',DENSESQUARE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',3_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',3_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[1 2 3]  b=[6]   x=[*]
      !  [1 3 2]    [6]     [*]
      !  [1 3 2]    [6]     [*]
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseSquareMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,2,2._SRK)
        CALL A%set(1,3,3._SRK)
        CALL A%set(2,1,1._SRK)
        CALL A%set(2,2,3._SRK)
        CALL A%set(2,3,2._SRK)
        CALL A%set(3,1,1._SRK)
        CALL A%set(3,2,3._SRK)
        CALL A%set(3,3,2._SRK)
      ENDSELECT
      ! set all values to 6
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(6._SRK)
      ENDSELECT
      CALL thisLS%solve()

      !Check the result
      ASSERT(thisLS%info == -1,'CALL Direct%solve() -GE method FAILED!')
      CALL thisLS%clear()

    ! Test LU (Dense-Square)

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',LU)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',DENSESQUARE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',4_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
      CALL pList%add('LinearSolverType->x->VectorType->n',4_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',4_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[1 2 3 4]  b=[10]   x=[1]
      !  [1 3 2 3]    [9]      [1]
      !  [3 2 3 1]    [9]      [1]
      !  [1 1 1 1]    [4]      [1]
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseSquareMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,2,2._SRK)
        CALL A%set(1,3,3._SRK)
        CALL A%set(1,4,4._SRK)
        CALL A%set(2,1,1._SRK)
        CALL A%set(2,2,3._SRK)
        CALL A%set(2,3,2._SRK)
        CALL A%set(2,4,3._SRK)
        CALL A%set(3,1,3._SRK)
        CALL A%set(3,2,2._SRK)
        CALL A%set(3,3,3._SRK)
        CALL A%set(3,4,1._SRK)
        CALL A%set(4,1,1._SRK)
        CALL A%set(4,2,1._SRK)
        CALL A%set(4,3,1._SRK)
        CALL A%set(4,4,1._SRK)
        CLASS DEFAULT
          ASSERT(.FALSE., "Wrong matrix TYPE")
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,10._SRK)
        CALL b%set(2,9._SRK)
        CALL b%set(3,9._SRK)
        CALL b%set(4,4._SRK)
        CLASS DEFAULT
          ASSERT(.FALSE., "Wrong matrix TYPE")
      ENDSELECT

      ! solve
      CALL thisLS%solve()

      !Check the result
      SELECTTYPE (X=>thisLS%X); TYPE IS(RealVectorType)
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = ((dummyvec(1) .APPROXEQ. 1._SRK) &
           .AND. (dummyvec(2) .APPROXEQ. 1._SRK) &
           .AND. (dummyvec(3) .APPROXEQ. 1._SRK) &
           .AND. (dummyvec(4) .APPROXEQ. 1._SRK) &
           .AND. (thisLS%info == 0) )
        ASSERT(bool, 'Direct%solve() -LU method')
      ENDSELECT
      CALL thisLS%clear()

    ! Test LU (Tridiagonal)

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',LU)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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
        CALL A%set(1,1,4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2,4._SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,3,4._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT

      CALL thisLS%solve()

      !Check M
      !M=[ 4   -1    0]
      !  [-.25 3.75 -1]
      !  [ 0   -.267  3.7333]
      SELECTTYPE(M => thisLS%M); TYPE IS(TriDiagMatrixType)
        bool = ((M%a(1,1) .APPROXEQ.  0.0_SRK)  &
           .AND. (M%a(1,2) .APPROXEQ. -0.25_SRK) &
           .AND. (M%a(1,3) .APPROXEQ. -0.266666666666666666_SRK) &
           .AND. (M%a(2,1) .APPROXEQ.  0.25_SRK) &
           .AND. (M%a(2,2) .APPROXEQ. (1.0_SRK/3.75_SRK)) &
           .AND. (M%a(2,3) .APPROXEQ. (1.0_SRK/3.7333333333333334_SRK)) &
           .AND. (M%a(3,1) .APPROXEQ. -1.0_SRK)  &
           .AND. (M%a(3,2) .APPROXEQ. -1.0_SRK)  &
           .AND. (M%a(3,3) .APPROXEQ.  0.0_SRK)  &
           .AND. thisLS%isDecomposed)
        ASSERT(bool, 'Direct%solve() -LU method')
      ENDSELECT

      !Check X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = ((dummyvec(1) .APPROXEQ. 0.46428571428571430_SRK) &
           .AND.  (dummyvec(2) .APPROXEQ. 0.85714285714285721_SRK) &
           .AND.  (dummyvec(3) .APPROXEQ. 0.96428571428571430_SRK) &
           .AND.   thisLS%info == 0)
        ASSERT(bool, 'Direct%solve() -LU method')
      ENDSELECT


      !Reset X, and solve it again
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        CALL X%set(1.0_SRK)
        CALL thisLS%solve()
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = ((dummyvec(1) .APPROXEQ. 0.46428571428571430_SRK) &
           .AND.  (dummyvec(2) .APPROXEQ. 0.85714285714285721_SRK) &
           .AND.  (dummyvec(3) .APPROXEQ. 0.96428571428571430_SRK) &
           .AND.   thisLS%isDecomposed)
        ASSERT(bool, 'Direct%solve() -LU method')
      ENDSELECT
      CALL thisLS%A%clear()
      CALL thisLS%clear()

      !Sparse matrix, just make sure it could go to CGNR and could
      ! get result, the details will be tested in CGNR
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',GE)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',9_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',33_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',9_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',9_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      ! initialize matrix A
      SELECTTYPE(A=>thisLS%A); TYPE IS(SparseMatrixType)
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

      ! initialize vector X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        CALL X%set(1.0_SRK)
      ENDSELECT

      ! initialize vector b
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      !solve it
      CALL thisLS%solve()

      bool = thisLS%info == 0
      ASSERT(bool, 'Direct%solve() -GE method')
      FINFO() thisLS%info
      CALL thisLS%clear()

      !DenseRect matrix, just make sure that it could go to CGNR.
      !The result will be checked later.
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',GE)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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
      ! initialize matrix A
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseRectMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,2,1._SRK)
        CALL A%set(2,1,1._SRK)
        CALL A%set(2,2,2._SRK)
        CALL A%set(3,1,1._SRK)
        CALL A%set(3,2,3._SRK)
      ENDSELECT

      ! initialize vector b
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,2._SRK)
      ENDSELECT

      ! initialize vector X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
      CALL X%set(1.0_SRK)
      ENDSELECT

      ! solve it
      CALL thisLS%solve()
      bool = thisLS%info == 0
      ASSERT(bool, 'Direct%solve() -GE method')
      CALL thisLS%clear()

    !Test LU (Dense square matrix and tridiagonal matrix
    !other matrix will raise error message)
      !Dense square
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',LU)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',DENSESQUARE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',3_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',3_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !Singular case
      !A=[1 0 1 ]  b=[4]    x=[*]
      !  [2 5 -2]    [6]      [*]
      !  [1 0 1 ]    [4]      [*]
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseSquareMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,2,0._SRK)
        CALL A%set(1,3,1._SRK)
        CALL A%set(2,1,2._SRK)
        CALL A%set(2,2,5._SRK)
        CALL A%set(2,3,-2._SRK)
        CALL A%set(3,1,1._SRK)
        CALL A%set(3,2,0._SRK)
        CALL A%set(3,3,1._SRK)
      ENDSELECT

      ! initialize vector b
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,4._SRK)
        CALL b%set(2,6._SRK)
        CALL b%set(3,4._SRK)
      ENDSELECT

      ! solve it
      CALL thisLS%solve()
      bool = thisLS%info == -1
      ASSERT(bool, 'Direct%solve() -LU method')
      CALL thisLS%clear()

      ! Normal Case (non-singular)
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',LU)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',DENSESQUARE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',3_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',3_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[1 0 1 ]  b=[4]    x=[1]
      !  [2 5 -2]    [6]      [2]
      !  [3 6 9 ]    [42]     [3]
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseSquareMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,2,0._SRK)
        CALL A%set(1,3,1._SRK)
        CALL A%set(2,1,2._SRK)
        CALL A%set(2,2,5._SRK)
        CALL A%set(2,3,-2._SRK)
        CALL A%set(3,1,3._SRK)
        CALL A%set(3,2,6._SRK)
        CALL A%set(3,3,9._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1, 4._SRK)
        CALL b%set(2, 6._SRK)
        CALL b%set(3,42._SRK)
      ENDSELECT

      CALL thisLS%solve()

      !Check M
      ! M=[3      6   9]
      !   [1/3    2  -2]
      !   [2/3 -1/2  -9]
      SELECTTYPE(M => thisLS%M); TYPE IS(DenseSquareMatrixType)
        bool = ((M%A(1,1) .APPROXEQ.   3._SRK) &
           .AND. (M%A(1,2) .APPROXEQ.   6._SRK) &
           .AND. (M%A(1,3) .APPROXEQ.   9._SRK) &
           .AND. (M%A(2,1) .APPROXEQ.   1._SRK/3._SRK) &
           .AND. (M%A(2,2) .APPROXEQ.  -2._SRK) &
           .AND. (M%A(2,3) .APPROXEQ.  -2._SRK) &
           .AND. (M%A(3,1) .APPROXEQ.   2._SRK/3._SRK) &
           .AND. (M%A(3,2) .APPROXEQ. -0.5_SRK) &
           .AND. (M%A(3,3) .APPROXEQ.  -9._SRK) )
        ASSERT(bool, 'Direct%solve() -LU method')
      ENDSELECT

      ! check IPIV: IPIV=[3 3 0]
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        bool = thisLS%IPIV(1) == 3 &
               .AND. thisLS%IPIV(2) == 3 &
               .AND. thisLS%IPIV(3) == 0
        ASSERT(bool, 'Direct%solve() -LU method')
      ENDSELECT

      ! check X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = ((dummyvec(1) .APPROXEQ. 1._SRK) &
           .AND. (dummyvec(2) .APPROXEQ. 2._SRK) &
           .AND. (dummyvec(3) .APPROXEQ. 3._SRK) &
           .AND. (thisLS%info == 0))
        ASSERT(bool, 'Direct%solve() -LU method')
      ENDSELECT

      ! reset right hand side and solve it again
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1, 4._SRK)
        CALL b%set(2,14._SRK)
        CALL b%set(3,30._SRK)
      ENDSELECT
      thisLS%isDecomposed=.FALSE.
      CALL thisLS%solve()

      ! check X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = ((dummyvec(1) .APPROXEQ. 3._SRK) &
           .AND. (dummyvec(2) .APPROXEQ. 2._SRK) &
           .AND. (dummyvec(3) .APPROXEQ. 1._SRK) &
           .AND. (thisLS%info == 0))
        ASSERT(bool, 'Direct%solve() -LU method')
      ENDSELECT
      CALL thisLS%clear()

      !Tridiagonal (singular case)
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',LU)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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
      !  [ 0  0  0]
      SELECTTYPE(A => thisLS%A); TYPE IS(TriDiagMatrixType)
        CALL A%set(1,1,4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2,4._SRK)
        CALL A%set(2,3,0._SRK)
        CALL A%set(3,3,0._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(1,2._SRK)
        CALL b%set(1,3._SRK)
      ENDSELECT

      CALL thisLS%solve()
      !Check
      bool = thisLS%info == -1
      ASSERT(bool, 'Direct%solve() -LU method')
      CALL thisLS%clear()

      ! Tridiagonal (non-singular case)
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',LU)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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
        CALL A%set(1,1,4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2,4._SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,3,4._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT

      CALL thisLS%solve()

      !Check M
      !M=[ 4   -1    0]
      !  [-.25 3.75 -1]
      !  [ 0   -.267  3.7333]
      SELECTTYPE(M => thisLS%M); TYPE IS(TriDiagMatrixType)
        bool = ((M%a(1,1) .APPROXEQ.  0.0_SRK)  &
           .AND. (M%a(1,2) .APPROXEQ. -.25_SRK)  &
           .AND. (M%a(1,3) .APPROXEQ. -0.266666666666666666_SRK) &
           .AND. (M%a(2,1) .APPROXEQ. 0.25_SRK)  &
           .AND. (M%a(2,2) .APPROXEQ. (1.0_SRK/3.75_SRK)) &
           .AND. (M%a(2,3) .APPROXEQ. (1.0_SRK/3.7333333333333334_SRK)) &
           .AND. (M%a(3,1) .APPROXEQ. -1.0_SRK)  &
           .AND. (M%a(3,2) .APPROXEQ. -1.0_SRK)  &
           .AND. (M%a(3,3) .APPROXEQ.  0.0_SRK)  &
           .AND. thisLS%isDecomposed)
        ASSERT(bool, 'Direct%solve() -LU method')
      ENDSELECT

      !Check X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = ((dummyvec(1) .APPROXEQ. 0.46428571428571430_SRK) &
           .AND.  (dummyvec(2) .APPROXEQ. 0.85714285714285721_SRK) &
           .AND.  (dummyvec(3) .APPROXEQ. 0.96428571428571430_SRK) &
           .AND.   thisLS%info == 0)
        ASSERT(bool, 'Direct%solve() -LU method')
      ENDSELECT

      !Reset X, and solve it again
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        CALL X%set(1.0_SRK)
        CALL thisLS%solve()
        CALL X%get(dummyvec)
        bool = ((dummyvec(1) .APPROXEQ. 0.46428571428571430_SRK) &
           .AND.  (dummyvec(2) .APPROXEQ. 0.85714285714285721_SRK) &
           .AND.  (dummyvec(3) .APPROXEQ. 0.96428571428571430_SRK) &
           .AND.   thisLS%isDecomposed)
        ASSERT(bool, 'Direct%solve() -LU method')
      ENDSELECT
      CALL thisLS%clear()

      !test solve for TriDiag with a non-diagonally dominant A
      !just make sure we get the output statement.
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',LU)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',TRIDIAG)
      CALL pList%add('LinearSolverType->A->MatrixType->n',3_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->isSym',.TRUE.)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',3_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[ O.5  -1    0]
      !  [-1   0.5   -1]
      !  [ 0    -1  0.5]
      SELECTTYPE(A => thisLS%A); TYPE IS(TriDiagMatrixType)
        CALL A%set(1,1,0.5_SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2,0.5_SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,3,0.5_SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT

      CALL thisLS%solve()
      CALL thisLS%clear()

      !Sparse matrix, just make sure it could go to CGNR and could
      ! get result, the details will be test in CGNR
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',GE)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',9_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',33_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',9_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',9_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

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

      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        CALL X%set(1.0_SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      ! solve it
      CALL thisLS%solve()

      bool = thisLS%info == 0
      ASSERT(bool, 'Direct%solve() -LU method')
      CALL thisLS%clear()

      !DenseRect matrix, just make sure that it could go to CGNR.
      !The result will be checked later.
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',LU)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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
        CALL b%set(3,2._SRK)
      ENDSELECT

      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        CALL X%set(1.0_SRK)
      ENDSELECT

      !Solve it
      CALL thisLS%solve()
      bool = thisLS%info == 0
      ASSERT(bool, 'Direct%solve() -LU method')
      CALL thisLS%clear()

#ifdef HAVE_PARDISO
      ! test with GE (doesn't affect actual solve)
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',PARDISO_MKL)
      CALL pList%add('LinearSolverType->solverMethod',GE)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',3_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',8_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',3_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[1 0 1 ]  b=[4]    x=[1]
      !  [2 5 -2]    [6]      [2]
      !  [3 6 9 ]    [42]     [3]
      SELECTTYPE(A => thisLS%A); TYPE IS(SparseMatrixType)
        CALL A%setShape(1,1,1._SRK)
        CALL A%setShape(1,3,1._SRK)
        CALL A%setShape(2,1,2._SRK)
        CALL A%setShape(2,2,5._SRK)
        CALL A%setShape(2,3,-2._SRK)
        CALL A%setShape(3,1,3._SRK)
        CALL A%setShape(3,2,6._SRK)
        CALL A%setShape(3,3,9._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1, 4._SRK)
        CALL b%set(2, 6._SRK)
        CALL b%set(3,42._SRK)
      ENDSELECT

      CALL thisLS%solve()

      ! check X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = (SOFTEQ(dummyvec(1),1._SRK,1E-14_SRK)  &
           .AND.  SOFTEQ(dummyvec(2),2._SRK,1E-14_SRK)  &
           .AND.  SOFTEQ(dummyvec(3),3._SRK,1E-14_SRK)) &
           .OR. thisLS%info /= 0
        ASSERT(bool, 'PARDISODirect%solve()')
      ENDSELECT
      CALL thisLS%clear()

      ! test with LU (doesn't affect actual solve)
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',PARDISO_MKL)
      CALL pList%add('LinearSolverType->solverMethod',LU)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',3_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',8_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',3_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[1 0 1 ]  b=[4]    x=[1]
      !  [2 5 -2]    [6]      [2]
      !  [3 6 9 ]    [42]     [3]
      SELECTTYPE(A => thisLS%A); TYPE IS(SparseMatrixType)
        CALL A%setShape(1,1,1._SRK)
        CALL A%setShape(1,3,1._SRK)
        CALL A%setShape(2,1,2._SRK)
        CALL A%setShape(2,2,5._SRK)
        CALL A%setShape(2,3,-2._SRK)
        CALL A%setShape(3,1,3._SRK)
        CALL A%setShape(3,2,6._SRK)
        CALL A%setShape(3,3,9._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1, 4._SRK)
        CALL b%set(2, 6._SRK)
        CALL b%set(3,42._SRK)
      ENDSELECT

      CALL thisLS%solve()

      ! check X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = (SOFTEQ(dummyvec(1),1._SRK,1E-14_SRK)  &
           .AND.  SOFTEQ(dummyvec(2),2._SRK,1E-14_SRK)  &
           .AND.  SOFTEQ(dummyvec(3),3._SRK,1E-14_SRK)) &
           .OR. thisLS%info /= 0
        ASSERT(bool, 'PARDISODirect%solve()')
      ENDSELECT
      CALL thisLS%clear()
#endif

#ifdef FUTILITY_HAVE_PETSC
      ! test with SuperLU through PETSc
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',PETSC)
      CALL pList%add('LinearSolverType->solverMethod',LU)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',3_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',8_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',3_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[1 0 1 ]  b=[4]    x=[1]
      !  [2 5 -2]    [6]      [2]
      !  [3 6 9 ]    [42]     [3]
      WRITE(*,*) 'set A'
      SELECTTYPE(A => thisLS%A); TYPE IS(PETScMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,3,1._SRK)
        CALL A%set(2,1,2._SRK)
        CALL A%set(2,2,5._SRK)
        CALL A%set(2,3,-2._SRK)
        CALL A%set(3,1,3._SRK)
        CALL A%set(3,2,6._SRK)
        CALL A%set(3,3,9._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(PETScVectorType)
        CALL b%set(1, 4._SRK)
        CALL b%set(2, 6._SRK)
        CALL b%set(3,42._SRK)
      ENDSELECT

      CALL thisLS%solve()

      ! check X
      SELECTTYPE(X => thisLS%X); TYPE IS(PETScVectorType)
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = (SOFTEQ(dummyvec(1),1._SRK,1E-14_SRK)  &
           .AND.  SOFTEQ(dummyvec(2),2._SRK,1E-14_SRK)  &
           .AND.  SOFTEQ(dummyvec(3),3._SRK,1E-14_SRK)) &
           .OR. thisLS%info /= 0
        ASSERT(bool, 'SuperLU Direct%solve()')
      ENDSELECT
      CALL thisLS%clear()
#endif

    !end test of direct solver
      CALL thisLS%clear()
      DEALLOCATE(thisLS)

    ENDSUBROUTINE testDirectSolve
!
!-------------------------------------------------------------------------------
    SUBROUTINE testQRSolve()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS

      ALLOCATE(LinearSolverType_Direct :: thisLS)

    !Test GE (Dense-Square)
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',QR)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',DENSERECT)
      CALL pList%add('LinearSolverType->A->MatrixType->n',5_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->m',3_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',5_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[1 -1   1]    b=[ 1]   x=[ *]
      !  [1 -.5  .25]    [.5]     [ *]
      !  [1  0   0]      [ 0]     [ *]
      !  [1  .5  .25]    [.5]
      !  [1  1   1]      [ 2]
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseRectMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(1,3,1._SRK)
        CALL A%set(2,1,1._SRK)
        CALL A%set(2,2,-0.5_SRK)
        CALL A%set(2,3,0.25_SRK)
        CALL A%set(3,1,1._SRK)
        CALL A%set(3,2,0._SRK)
        CALL A%set(3,3,0._SRK)
        CALL A%set(4,1,1._SRK)
        CALL A%set(4,2,0.5_SRK)
        CALL A%set(4,3,0.25_SRK)
        CALL A%set(5,1,1._SRK)
        CALL A%set(5,2,1._SRK)
        CALL A%set(5,3,1._SRK)
      ENDSELECT
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,0.5_SRK)
        CALL b%set(3,0._SRK)
        CALL b%set(4,0.5_SRK)
        CALL b%set(5,2._SRK)
      ENDSELECT
      CALL thisLS%solve()

      !Check the result
      ASSERT(thisLS%info == 0,'CALL Direct%solve() - QR method')
      SELECTTYPE(x => thisLS%x); TYPE IS(RealVectorType)
        ASSERT(x%b(1) .APPROXEQ. 0.085714285714285500000_SRK,'CALL Direct%solve() - QR method value 1')
        ASSERT(x%b(2) .APPROXEQ. 0.400000000000000000000_SRK,'CALL Direct%solve() - QR method value 2')
        ASSERT(x%b(3) .APPROXEQ. 1.428571428571430000000_SRK,'CALL Direct%solve() - QR method value 3')
      ENDSELECT
      CALL thisLS%clear()

    !end test of direct solver
      CALL thisLS%clear()
      DEALLOCATE(thisLS)

    ENDSUBROUTINE testQRSolve
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIterativeOthers()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      REAL(SRK),POINTER :: thisX2(:)
      REAL(SRK),ALLOCATABLE :: resid_soln(:),dummyvec(:)
      TYPE(RealVectorType) :: resid
      INTEGER(SIK) :: i
      LOGICAL(SBK) :: bool

#ifdef FUTILITY_HAVE_PETSC
      PetscReal :: rtol,abstol,dtol
      PetscInt  :: maxits,restart
      PetscErrorCode :: ierr
#endif

      ALLOCATE(LinearSolverType_Iterative :: thisLS)

    !Test setX0

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',2_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',2_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',2_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        CALL X%set(1,0._SRK)
        CALL X%set(2,0._SRK)
      ENDSELECT

      ! initialize X0
      ALLOCATE(thisX2(3))
      thisX2=(/1._SRK,2._SRK,3._SRK/)

      !test case that is expected to work, thisX has already been allocated
      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX2)
        bool = (ASSOCIATED(thisLS%X) .AND. ASSOCIATED(thisX2) &
           .AND.  thisLS%hasX0)
        ASSERT(bool, 'Iterative%setX0(...)')
      ENDSELECT
      DEALLOCATE(thisX2)
      CALL thisLS%clear()

#ifdef FUTILITY_HAVE_PETSC
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',PETSC)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',2_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',2_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',2_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      ! initialize vector X
      SELECTTYPE(X => thisLS%X); TYPE IS(PETScVectorType)
        CALL X%set(1,0._SRK)
        CALL X%set(2,0._SRK)
      ENDSELECT

      ! initialize X0
      ALLOCATE(thisX2(3))
      thisX2=(/1._SRK,2._SRK,3._SRK/)

      !test case that is expected to work, thisX has already been allocated
      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX2)
        bool = (ASSOCIATED(thisLS%X) .AND. ASSOCIATED(thisX2) &
           .AND.  thisLS%hasX0)
        ASSERT(bool, 'PETScIterative%setX0(...)')
      ENDSELECT
      DEALLOCATE(thisX2)
      CALL thisLS%clear()
#endif

    !Test setConv
      !Bad input
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',2_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',2_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',2_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Iterative)
        CALL thisLS%setConv(-2,-0.1_SRK,-1,-1)
        CALL thisLS%setConv(-2,1.1_SRK,-1,-1)
        !Check if default value is used
        bool = thisLS%maxIters == 1000_SIK .AND. thisLS%normType == 2_SIK &
          .AND. thisLS%convTol == 0.001_SRK .AND. thisLS%nRestart == 30_SIK
        ASSERT(bool, 'Iterative%setConv(...)')
        FINFO() thisLS%maxIters, thisLS%normType, thisLS%convTol, thisLS%nRestart
      ENDSELECT

      !Correct input
      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Iterative)
        CALL thisLS%setConv(1_SIK,0.01_SRK,100_SIK,10_SIK)
        bool = thisLS%maxIters == 100_SIK .AND. thisLS%normType == 1_SIK &
          .AND. thisLS%convTol == 0.01_SRK .AND. thisLS%nRestart == 10_SIK
        ASSERT(bool, 'Iterative%setConv(...)')
      ENDSELECT
      CALL thisLS%clear()

#ifdef FUTILITY_HAVE_PETSC
      !Test setConv
      !Bad input
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',PETSC)
      CALL pList%add('LinearSolverType->solverMethod',GMRES) ! GMRES to test nrestart
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',2_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',2_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',2_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Iterative)
        CALL thisLS%setConv(-2,-0.1_SRK,-1,-1)
        CALL thisLS%setConv(-2,1.1_SRK,-1,-1)
        !Check if default value is used
        CALL KSPGetTolerances(thisLS%ksp,rtol,abstol,dtol,maxits,ierr)
!        CALL KSPGMRESGetRestart(thisLS%ksp,restart,ierr)
        restart=30
        bool = maxits == 1000_SIK .AND. rtol == 0.001_SRK &
           .AND. abstol == 0.001_SRK .AND. restart == 30_SIK
        ASSERT(bool, 'PETScIterative%setConv(...)')
      ENDSELECT

      !Correct input
      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Iterative)
        CALL thisLS%setConv(1_SIK,0.01_SRK,100_SIK,10_SIK)
        CALL KSPGetTolerances(thisLS%ksp,rtol,abstol,dtol,maxits,ierr)
!        CALL KSPGMRESGetRestart(thisLS%ksp,restart,ierr)
        restart=10
        bool = maxits == 100_SIK .AND. rtol == 0.01_SRK &
           .AND. abstol == 0.01_SRK .AND. restart == 10_SIK
        ASSERT(bool, 'PETScIterative%setConv(...)')
      ENDSELECT
      CALL thisLS%clear()
#endif

    !Test getResidual
      !Bad input
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%getResidual(resid)

        CALL pList%clear()
        CALL pList%add('LinearSolverType->matType',SPARSE)
        CALL pList%add('LinearSolverType->TPLType',NATIVE)
        CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
        CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
        CALL pList%add('LinearSolverType->numberOMP',1_SNK)
        CALL pList%add('LinearSolverType->timerName','testTimer')
        CALL pList%add('LinearSolverType->matType',SPARSE)
        CALL pList%add('LinearSolverType->A->MatrixType->n',5_SNK)
        CALL pList%add('LinearSolverType->A->MatrixType->nnz',1_SNK)
        CALL pList%add('LinearSolverType->x->VectorType->n',5_SNK)
        CALL pList%add('LinearSolverType->b->VectorType->n',5_SNK)
        CALL pList%validate(pList,optListLS)
        CALL thisLS%init(pList)

        CALL thisLS%getResidual(resid)

        CALL vecPList%set('VectorType->n',5_SNK)
        CALL resid%init(vecPList)
        CALL thisLS%getResidual(resid)

        CALL thisLS%clear()
        CALL resid%clear()

      ENDSELECT

      !Correct input
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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

      ! initialize vector X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        CALL X%set(1._SRK)
      ENDSELECT

      ! initialize vector b
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(10._SRK)
      ENDSELECT

      CALL vecPList%set('VectorType->n',9_SNK)
      CALL resid%init(vecPList)
      ALLOCATE(resid_soln(9))
      resid_soln=(/-8._SRK,-9._SRK,-8._SRK,-9._SRK,-10._SRK, &
        -9._SRK,-8._SRK,-9._SRK,-8._SRK/)

      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%getResidual(resid)
      ENDSELECT

      DO i=1,resid%n
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(resid%n))
        CALL resid%get(dummyvec)
        bool = (dummyvec(i) .APPROXEQ. resid_soln(i))
        ASSERT(bool, 'Iterative%getResidual(...)')
      ENDDO

      CALL thisLS%clear()
      DEALLOCATE(resid_soln)
      DEALLOCATE(thisLS)

    ENDSUBROUTINE testIterativeOthers
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIterativeSolve_BICGSTAB()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      REAL(SRK),ALLOCATABLE :: thisB(:),dummyvec(:)
      REAL(SRK),POINTER :: thisX(:)
      INTEGER(SIK) :: i
      LOGICAL(SBK) :: match, bool

      ALLOCATE(LinearSolverType_Iterative :: thisLS)

    !With BiCGSTAB
      !The sparse matrix type

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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

      ! build X0 and set it to 1.0s
      ALLOCATE(thisX(9))
      thisX=1.0_SRK

      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK,30_SIK)
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
      ASSERT(match, 'Iterative%solve() -BICGSTAB')

      DEALLOCATE(thisB)
      CALL thisLS%A%clear()
      CALL thisLS%clear()
      DEALLOCATE(thisX)

    !test with A being densesquare

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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

      !build X0 and set it to 1.0s
      ALLOCATE(thisX(9))
      thisX=1.0_SRK
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT

      !set b
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK,30_SIK)
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
      ASSERT(match, 'Iterative%solve() - BiCGSTAB')
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
      ASSERT(match, 'Iterative%solve() - BiCGSTAB')
      CALL thisLS%clear()
      DEALLOCATE(thisB)

      ! TriDiagonal matrix, it will go to LU method

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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
      ASSERT(bool, 'Iterative%solve() - BiCGSTAB')
      CALL thisLS%clear()
      DEALLOCATE(thisX)

      !DenseRect matrix

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK,30_SIK)
      ENDSELECT

      !Solve it
      CALL thisLS%solve()

      bool = thisLS%info == 0
      ASSERT(bool, 'Iterative%solve() -BiCGSTAB method')

      DEALLOCATE(thisX)
      CALL thisLS%clear()

#ifdef FUTILITY_HAVE_PETSC
      !The PETSC sparse matrix type
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',PETSC)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',9_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
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
      SELECTTYPE(A => thisLS%A); TYPE IS(PETScMatrixType)
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

      ! build X0 and set it to 1.0s
      ALLOCATE(thisX(9))
      thisX=1.0_SRK

      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(PETScVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK,30_SIK)
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

      DEALLOCATE(thisB)
      DEALLOCATE(thisX)
      CALL thisLS%A%clear()
      CALL thisLS%clear()

      !test with A being densesquare
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',PETSC)
      CALL pList%add('LinearSolverType->solverMethod',BICGSTAB)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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

      !build X0 and set it to 1.0s
      ALLOCATE(thisX(9))
      thisX=1.0_SRK
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(PETScVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK,30_SIK)
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
      ASSERT(match, 'PETSCIterative%solve() -BiCGSTAB')
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
      ASSERT(match, 'PETSCIterative%solve() -BiCGSTAB')
      CALL thisLS%clear()
      DEALLOCATE(thisB)
      DEALLOCATE(thisX)
#endif

      DEALLOCATE(thisLS)

    ENDSUBROUTINE testIterativeSolve_BICGSTAB
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIterativeSolve_CGNR()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      REAL(SRK),ALLOCATABLE :: dummyvec(:)
      REAL(SRK),POINTER :: thisX(:)
      LOGICAL(SBK) :: bool

      ALLOCATE(LinearSolverType_Iterative :: thisLS)

      !test CGNR
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',CGNR)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',DENSERECT)
      CALL pList%add('LinearSolverType->A->MatrixType->n',2_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->m',3_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',2_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !underdetermined matrix: solver%info should return -1.
      ! A=[1 1 1]  b= [1]
      !   [1 2 3]     [3]
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseRectMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,2,1._SRK)
        CALL A%set(1,3,1._SRK)
        CALL A%set(2,1,1._SRK)
        CALL A%set(2,2,2._SRK)
        CALL A%set(2,3,3._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,3._SRK)
      ENDSELECT

      ! initialize X0
      ALLOCATE(thisX(3))
      thisX=1.0_SRK

      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK,30_SIK)
      ENDSELECT

      CALL thisLS%solve()

      bool = thisLS%info == -1
      ASSERT(bool, 'Iterative%solve() -CGNR')

      DEALLOCATE(thisX)
      CALL thisLS%clear()

      !normal or overdetermined matrix should give answers

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',CGNR)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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
        CALL b%set(3,2._SRK)
      ENDSELECT

      ! initialize X0
      ALLOCATE(thisX(2))
      thisX=1.0_SRK

      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT
      !set iterations and convergence information and
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setConv(2_SIK,1.0E-13_SRK,1000_SIK,30_SIK)
      ENDSELECT

      CALL thisLS%solve()

      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = (SOFTEQ(dummyvec(1),2._SRK/3._SRK,1.0E-13_SRK) &
           .AND. SOFTEQ(dummyvec(2),0.5_SRK,1.0E-13_SRK))
        ASSERT(bool, 'Iterative%solve()')
      ENDSELECT

      DEALLOCATE(thisX)
      CALL thisLS%clear()

      !DenseSquare matrix

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',CGNR)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',DENSESQUARE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',3_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->isSym',.TRUE.)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',3_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseSquareMatrixType)
        CALL A%set(1,1,4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2,4._SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,3,4._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT

      !set iterations and convergence information and
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setConv(2_SIK,1.0E-13_SRK,1000_SIK,30_SIK)
      ENDSELECT

      CALL thisLS%solve()

      !Check X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = ((dummyvec(1) .APPROXEQ. 0.46428571428571430_SRK) &
               .AND.  (dummyvec(2) .APPROXEQ. 0.85714285714285721_SRK) &
               .AND.  (dummyvec(3) .APPROXEQ. 0.96428571428571430_SRK) &
               .AND.   thisLS%info == 0)
        ASSERT(bool, 'Iterative%solve() -CGNR method')
      ENDSELECT

      CALL thisLS%clear()

      !Sparse matrix

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',CGNR)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',3_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',7_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',3_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      SELECTTYPE(A => thisLS%A); TYPE IS(SparseMatrixType)
        CALL A%setShape(1,1, 4._SRK)
        CALL A%setShape(1,2,-1._SRK)
        CALL A%setShape(2,1,-1._SRK)
        CALL A%setShape(2,2, 4._SRK)
        CALL A%setShape(2,3,-1._SRK)
        CALL A%setShape(3,2,-1._SRK)
        CALL A%setShape(3,3, 4._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT

      ! initialize X0
      ALLOCATE(thisX(3))
      thisX=0._SRK

      !set iterations and convergence information and
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
        CALL thisLS%setConv(2_SIK,1.0E-13_SRK,1000_SIK,30_SIK)
      ENDSELECT

      !Check X
      CALL thisLS%solve()
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = ((dummyvec(1) .APPROXEQ. 0.46428571428571430_SRK) &
           .AND.  (dummyvec(2) .APPROXEQ. 0.85714285714285721_SRK) &
           .AND.  (dummyvec(3) .APPROXEQ. 0.96428571428571430_SRK) &
           .AND.   thisLS%info == 0)
        ASSERT(bool, 'Iterative%solve() -CGNR method')
      ENDSELECT

      DEALLOCATE(thisX)
      CALL thisLS%clear()

      !TriDiagonal matrix, it will go to LU

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',NATIVE)
      CALL pList%add('LinearSolverType->solverMethod',CGNR)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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
        CALL A%set(1,1,4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2,4._SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,3,4._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT

      CALL thisLS%solve()

      bool = thisLS%info == 0
      ASSERT(bool, 'Iterative%solve() - BiCGSTAB')

      CALL thisLS%clear()


#ifdef FUTILITY_HAVE_PETSC
      ! DenseSquare matrix
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',PETSC)
      CALL pList%add('LinearSolverType->solverMethod',CGNR)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',DENSESQUARE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',3_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->isSym',.TRUE.)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',3_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      SELECTTYPE(A => thisLS%A); TYPE IS(PETScMatrixType)
        CALL A%set(1,1,4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2,4._SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,3,4._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(PETScVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT

      !set iterations and convergence information and
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setConv(2_SIK,1.0E-13_SRK,1000_SIK,30_SIK)
      ENDSELECT

      CALL thisLS%solve()

      !Check X
      SELECTTYPE(X => thisLS%X); TYPE IS(PETScVectorType)
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = ((dummyvec(1) .APPROXEQ. 0.46428571428571430_SRK) &
           .AND.  (dummyvec(2) .APPROXEQ. 0.85714285714285721_SRK) &
           .AND.  (dummyvec(3) .APPROXEQ. 0.96428571428571430_SRK) &
           .AND.   thisLS%info == 0)
        ASSERT(bool, 'PETScIterative%solve() -CGNR method')
      ENDSELECT

      CALL thisLS%clear()

      ! Sparse matrix
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',PETSC)
      CALL pList%add('LinearSolverType->solverMethod',CGNR)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',3_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->isSym',.TRUE.)
      CALL pList%add('LinearSolverType->x->VectorType->n',3_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',3_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      SELECTTYPE(A => thisLS%A); TYPE IS(PETScMatrixType)
        CALL A%set(1,1, 4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,1,-1._SRK)
        CALL A%set(2,2, 4._SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,2,-1._SRK)
        CALL A%set(3,3, 4._SRK)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(PETScVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT

      ! initialize X0
      ALLOCATE(thisX(3))
      thisX=0._SRK

      !set iterations and convergence information and
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
        CALL thisLS%setConv(2_SIK,1.0E-13_SRK,1000_SIK,30_SIK)
      ENDSELECT

      !Check X
      CALL thisLS%solve()
      SELECTTYPE(X => thisLS%X); TYPE IS(PETScVectorType)
        IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
        ALLOCATE(dummyvec(X%n))
        CALL X%get(dummyvec)
        bool = ((dummyvec(1) .APPROXEQ. 0.46428571428571430_SRK) &
           .AND.  (dummyvec(2) .APPROXEQ. 0.85714285714285721_SRK) &
           .AND.  (dummyvec(3) .APPROXEQ. 0.96428571428571430_SRK) &
           .AND.   thisLS%info == 0)
        ASSERT(bool, 'PETScIterative%solve() -CGNR method')
      ENDSELECT

      DEALLOCATE(thisX)
      CALL thisLS%clear()

#endif

      DEALLOCATE(thisLS)

    ENDSUBROUTINE testIterativeSolve_CGNR
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
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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

      ! build X0 and set it to 1.0s
      ALLOCATE(thisX(9))
      thisX=1.0_SRK

      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setX0(thisX)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK,30_SIK)
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
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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

      !build X0 and set it to 1.0s
      ALLOCATE(thisX(9))
      thisX=1.0_SRK
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT

      !set b
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK,30_SIK)
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
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK,30_SIK)
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
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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

      ! build X0 and set it to 1.0s
      ALLOCATE(thisX(9))
      thisX=1.0_SRK

      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setX0(thisX)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(PETScVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK,30_SIK)
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
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
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

      !build X0 and set it to 1.0s
      ALLOCATE(thisX(9))
      thisX=1.0_SRK
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT

      SELECTTYPE(b => thisLS%b); TYPE IS(PETScVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK,30_SIK)
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
!
!-------------------------------------------------------------------------------
    SUBROUTINE testNorms()
      INTEGER(SIK) :: normType
      REAL(SRK),DIMENSION(10) :: x
      REAL(SRK) :: norm
      LOGICAL(SBK) :: bool
      !set up x
      x=(/0._SRK,-1._SRK,2._SRK,-3._SRK,4._SRK, &
          -5._SRK,6._SRK,-7._SRK,8._SRK,-9._SRK/)
      !test normType 1
      normType=1 !taxicab norm, just the absolute sum of these.
      !expected answer = 45
      CALL LNorm(x,normType,norm)
      bool = (norm .APPROXEQ. 45._SRK)
      ASSERT(bool, 'LNorm() - 1-NORM')
      !test normType 2
      normType=2 !Euclidean norm
      !expected answer = 16.88194301613413218312
      CALL LNorm(x,normType,norm)
      bool = (norm .APPROXEQ. 16.88194301613413218312_SRK)
      ASSERT(bool, 'LNorm() - 2-NORM')
      !test normType -1
      normType=-1 !Infinite norm
      CALL LNorm(x,normType,norm)
      !expected answer = 9.0
      bool = (norm .APPROXEQ. 9.0_SRK)
      ASSERT(bool, 'LNorm() - INFINITE-NORM')
      FINFO() norm
      !test normType 3 (just some p-norm)
      normType=3 !L-norm, w/ L=3
      CALL LNorm(x,normType,norm)
      !expected answer = 12.65148997952623864269
      bool = (norm .APPROXEQ. 12.65148997952623864269_SRK)
      ASSERT(bool, 'LNorm() - L-NORM')
      FINFO() norm
      !test an invalid norm (<=-2)
      normType=-2
      CALL LNorm(x,normType,norm)
      !expected answer = 0.0
      bool = (norm == 0.0_SRK)
      ASSERT(bool, 'LNorm() - L-NORM')
      FINFO() norm
    ENDSUBROUTINE testNorms

ENDPROGRAM testLinearSolver
