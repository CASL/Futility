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
PROGRAM testLinearSolver
  
  USE IntrType
  USE ExceptionHandler
  USE LinearSolverTypes
  USE MatrixTypes
  USE ParallelEnv
  USE ParameterLists
  IMPLICIT NONE
  
  TYPE(MPI_EnvType) :: mpiTestEnv
  TYPE(ExceptionHandlerType),POINTER :: e
  TYPE(ParamType) :: pList
  
#ifdef HAVE_PETSC
#include <finclude/petsc.h>
#define IS IS !petscisdef.h defines the keyword IS, and it needs to be reset
  PetscErrorCode  :: ierr
#endif

  !Configure exception handler for test
  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  eLinearSolverType => e
  CALL mpiTestEnv%initialize(PE_COMM_SELF)
  
#ifdef HAVE_PETSC    
  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING LINEAR SOLVERS...'
  WRITE(*,*) '==================================================='
  
  WRITE(*,*) 'TESTING NORMS PROCEDURE'
  CALL testNorms()
  
  WRITE(*,*) 'TESTING LINEAR SOLVER TYPES'
  CALL testClear()
  CALL testInit()
  CALL testUpdatedA()
  CALL testDirectSolve()
  CALL testIterativeOthers()
  CALL testIterativeSolve()

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING LINEAR SOLVERS PASSED!'
  WRITE(*,*) '==================================================='
#ifdef HAVE_PETSC    
  CALL PetscFinalize(ierr)
#endif
  CALL mpiTestEnv%finalize()
  DEALLOCATE(e)
  
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testClear()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      TYPE(MPI_EnvType),POINTER :: MPIEnv
      TYPE(OMP_EnvType),POINTER :: OMPEnv
      CLASS(MatrixType),POINTER :: thisA
      REAL(SRK),POINTER :: thisX(:)

      !initialize supporting variables
      ALLOCATE(MPIEnv,OMPEnv)
      CALL MPIEnv%initialize(PE_COMM_SELF)
      CALL OMPEnv%initialize(1)

    !test Direct
      ALLOCATE(LinearSolverType_Direct :: thisLS)

      ALLOCATE(DenseSquareMatrixType :: thisA)
      CALL pList%clear()
      CALL pList%add('PL->n',2_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisA%init(pList) !2x2, symmetric
      ALLOCATE(thisX(2))
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        !first build one by hand to test clear
        thisLS%isInit=.TRUE.
        thisLS%solverMethod=1
        thisLS%MPIparallelEnv => MPIEnv
        thisLS%OMPparallelEnv => OMPEnv
        thisLS%A => thisA
        thisLS%X => thisX
        thisLS%info=2
        ALLOCATE(thisLS%b(2))
        ALLOCATE(thisLS%IPIV(2))
        ALLOCATE(DenseSquareMatrixType :: thisLS%M)
        CALL pList%clear()
        CALL pList%add('PL->n',10_SNK)
        CALL pList%add('PL->m',10_SNK)
        CALL thisLS%M%init(pList)
        thisLS%hasA=.TRUE.
        thisLS%hasB=.TRUE.
        thisLS%isDecomposed=.TRUE.
      ENDSELECT
      !call clear
      CALL thisLS%clear()
      !check results
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        IF(thisLS%isInit .OR. thisLS%solverMethod /= -1 &
          .OR. thisLS%isDecomposed .OR. ASSOCIATED(thisLS%A) &
          .OR. ASSOCIATED(thisLS%X) .OR. thisLS%hasA .OR. thisLS%info /= 0 &
          .OR. ALLOCATED(thisLS%b) .OR. thisLS%hasB .OR. ALLOCATED(thisLS%IPIV) &
          .OR. ALLOCATED(thisLS%M) .OR. ASSOCIATED(thisLS%MPIparallelEnv) & 
          .OR. ASSOCIATED(thisLS%OMPparallelEnv) ) THEN
          WRITE(*,*) 'CALL Direct%clear(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      WRITE(*,*) '  Passed: CALL Direct%clear()'
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX,thisLS)
    !test Iterative
      ALLOCATE(LinearSolverType_Iterative :: thisLS)
      ALLOCATE(DenseSquareMatrixType :: thisA)
      CALL pList%clear()
      CALL pList%add('PL->n',2_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisA%init(pList) !2x2, symmetric
      ALLOCATE(thisX(2))
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        !first build one by hand to test clear
        thisLS%isInit=.TRUE.
        thisLS%solverMethod=1
        thisLS%MPIparallelEnv => MPIEnv
        thisLS%OMPparallelEnv => OMPEnv
        thisLS%A => thisA
        thisLS%X => thisX
        thisLS%info=2
        thisLS%hasA=.TRUE.
        thisLS%hasB=.TRUE.
        thisLS%isDecomposed=.TRUE.
        ALLOCATE(thisLS%b(2))
        ALLOCATE(DenseSquareMatrixType :: thisLS%M)
        CALL pList%clear()
        CALL pList%add('PL->n',10_SNK)
        CALL pList%add('PL->m',10_SNK)
        CALL thisLS%M%init(pList)
        thisLS%normType=2
        thisLS%maxIters=2
        thisLS%iters=2
        thisLS%convTol=2._SRK
        thisLS%residual=2._SRK
      ENDSELECT
      CALL thisLS%clear()
      !check results
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        IF(thisLS%isInit .OR.thisLS%solverMethod == 1 &
          .OR. thisLS%isDecomposed .OR. ASSOCIATED(thisLS%A) &
          .OR. ASSOCIATED(thisLS%X) .OR. thisLS%hasA .OR. thisLS%hasB &
          .OR. thisLS%info /= 0 .OR. ASSOCIATED(thisLS%MPIparallelEnv) &
          .OR. ASSOCIATED(thisLS%OMPparallelEnv) .OR. ALLOCATED(thisLS%M) &
          .OR. thisLS%normType == 2 .OR. thisLS%maxIters == 2 &
          .OR. thisLS%iters == 2 .OR. thisLS%convTol == 2._SRK &
          .OR. thisLS%residual == 2._SRK ) THEN
          WRITE(*,*) 'CALL Iterative%clear() FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      WRITE(*,*) '  Passed: CALL Iterative%clear()'
      CALL thisLS%clear()
      DEALLOCATE(thisA,thisX,thisLS)
      CALL MPIEnv%clear()
      CALL OMPEnv%clear()
      DEALLOCATE(MPIEnv,OMPEnv)
    ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInit()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      TYPE(MPI_EnvType),POINTER :: MPIEnv,MPIEnv_uninit
      TYPE(OMP_EnvType),POINTER :: OMPEnv,OMPEnv_uninit

      !initialize supporting variables
      ALLOCATE(MPIEnv,OMPEnv)
      CALL MPIEnv%initialize(PE_COMM_SELF)
      CALL OMPEnv%initialize(1)
   !test Direct
      ALLOCATE(LinearSolverType_Direct :: thisLS)
      !Bad input
      MPIEnv_uninit => NULL()
      OMPEnv_uninit => NULL()
      CALL thisLS%init(-1,MPIEnv_uninit,OMPEnv_uninit)
      CALL thisLS%clear()
      CALL thisLS%init(1,MPIEnv_uninit,OMPEnv_uninit,'testTimer')
      CALL thisLS%clear()
      ALLOCATE(MPIEnv_uninit,OMPEnv_uninit)
      CALL thisLS%init(1,MPIEnv_uninit,OMPEnv_uninit,'testTimer')
      CALL thisLS%clear()
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      CALL thisLS%clear()
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      CALL thisLS%clear()

      !first test a correct use case, with timer name
      CALL thisLS%init(1,MPIEnv,OMPEnv,'testTimer')
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        IF(.NOT. (thisLS%isInit .AND. thisLS%solverMethod == 1 &
          .AND. ASSOCIATED(thisLS%MPIparallelEnv) &
            .AND. ASSOCIATED(thisLS%OMPparallelEnv) &
              .AND. thisLS%SolveTime%getTimerName() == 'testTimer')) THEN
          WRITE(*,*) 'CALL Direct%init(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      CALL thisLS%clear()
      !first test a correct use case, with no timer name
      CALL thisLS%init(1,MPIEnv,OMPEnv)
      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Direct)
        IF(.NOT. (thisLS%isInit .AND. thisLS%solverMethod == 1 &
          .AND. ASSOCIATED(thisLS%MPIparallelEnv) &
            .AND. ASSOCIATED(thisLS%OMPparallelEnv) &
              .AND. thisLS%SolveTime%getTimerName() == 'LinearSolver Timer')) THEN
          WRITE(*,*) 'CALL Direct%init(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      WRITE(*,*) '  Passed: CALL Direct%init(...)'
      CALL thisLS%clear()
      DEALLOCATE(thisLS,MPIEnv_uninit,OMPEnv_uninit)
    !test Iterative
      ALLOCATE(LinearSolverType_Iterative :: thisLS)
      !Bad input
      CALL thisLS%init(-1,MPIEnv_uninit,OMPEnv_uninit)
      CALL thisLS%init(1,MPIEnv_uninit,OMPEnv_uninit,'testTimer')
      ALLOCATE(MPIEnv_uninit,OMPEnv_uninit)
      CALL thisLS%init(1,MPIEnv_uninit,OMPEnv_uninit,'testTimer')
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')

      CALL thisLS%clear()
      !first test a correct use case, with timer name
      CALL thisLS%init(1,MPIEnv,OMPEnv,'testTimer')
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        IF(.NOT. (thisLS%isInit .AND. thisLS%solverMethod == 1 &
          .AND. ASSOCIATED(thisLS%MPIparallelEnv) &
            .AND. ASSOCIATED(thisLS%OMPparallelEnv) &
              .AND. thisLS%SolveTime%getTimerName() == 'testTimer')) THEN
          WRITE(*,*) 'CALL Iterative%init(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      CALL thisLS%clear()
      !first test a correct use case, with no timer name
      CALL thisLS%init(1,MPIEnv,OMPEnv)
      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Iterative)
        IF(.NOT. (thisLS%isInit .AND. thisLS%solverMethod == 1 &
          .AND. ASSOCIATED(thisLS%MPIparallelEnv) &
            .AND. ASSOCIATED(thisLS%OMPparallelEnv) &
              .AND. thisLS%SolveTime%getTimerName() == 'LinearSolver Timer')) THEN
          WRITE(*,*) 'CALL Iterative%init(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      WRITE(*,*) '  Passed: CALL Iterative%init(...)'
      CALL thisLS%clear()
      CALL MPIEnv%clear()
      CALL OMPEnv%clear()
      DEALLOCATE(thisLS,MPIEnv,OMPEnv,MPIEnv_uninit,OMPEnv_uninit)
    ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testUpdatedA()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      TYPE(MPI_EnvType),POINTER :: MPIEnv
      TYPE(OMP_EnvType),POINTER :: OMPEnv

      !initialize supporting variables
      ALLOCATE(MPIEnv,OMPEnv)
      CALL MPIEnv%initialize(PE_COMM_SELF)
      CALL OMPEnv%initialize(1)
    !test Direct
      ALLOCATE(LinearSolverType_Direct :: thisLS)
      CALL thisLS%init(1,MPIEnv,OMPEnv,'testTimer')
      ALLOCATE(DenseSquareMatrixType :: thisLS%M)
      CALL pList%clear()
      CALL pList%add('PL->n',10_SNK)
      CALL pList%add('PL->m',10_SNK)
      CALL thisLS%M%init(pList)
      thisLS%isDecomposed=.TRUE.
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        ALLOCATE(thisLS%IPIV(10))
      ENDSELECT
      CALL thisLS%updatedA()
      !Check
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        IF(ALLOCATED(thisLS%M) .OR. thisLS%isDecomposed &
          .OR. ALLOCATED(thisLS%IPIV)) THEN
          WRITE(*,*) 'CALL Direct%updatedA() FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      WRITE(*,*) '  Passed: CALL Direct%updatedA()'
      CALL thisLS%clear()
      DEALLOCATE(thisLS)
    !test Iterative
      ALLOCATE(LinearSolverType_Iterative :: thisLS)
      CALL thisLS%init(1,MPIEnv,OMPEnv,'testTimer')
      ALLOCATE(DenseSquareMatrixType :: thisLS%M)
      CALL pList%clear()
      CALL pList%add('PL->n',10_SNK)
      CALL pList%add('PL->m',10_SNK)
      CALL thisLS%M%init(pList)
      thisLS%isDecomposed=.TRUE.
      CALL thisLS%updatedA()
      !Check
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        IF(thisLS%isDecomposed .OR. ALLOCATED(thisLS%M)) THEN
          WRITE(*,*) 'CALL Iterative%updatedA() FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      WRITE(*,*) '  Passed: CALL Iterative%updatedA()'
      CALL thisLS%clear
      DEALLOCATE(thisLS)
      CALL MPIEnv%clear()
      CALL OMPEnv%clear()
      DEALLOCATE(MPIEnv,OMPEnv)
    ENDSUBROUTINE testUpdatedA
!
!-------------------------------------------------------------------------------
    SUBROUTINE testDirectSolve()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      TYPE(MPI_EnvType),POINTER :: MPIEnv
      TYPE(OMP_EnvType),POINTER :: OMPEnv
      CLASS(MatrixType),POINTER :: thisA
      REAL(SRK),POINTER :: thisX(:)

      !initialize supporting variables
      ALLOCATE(MPIEnv,OMPEnv)
      CALL MPIEnv%initialize(PE_COMM_SELF)
      CALL OMPEnv%initialize(1)
      ALLOCATE(LinearSolverType_Direct :: thisLS)
      !Bad input, this part is just tested here.
      !it is the same with iterative method.
      !%A,%X, %b not assigned
      !The size of %A and %X and %b not correct
      CALL thisLS%solve()
      CALL thisLS%init(1,MPIEnv,OMPEnv)
      CALL thisLS%solve()
      ALLOCATE(DenseSquareMatrixType :: thisA)
      thisLS%A => thisA
      CALL thisLS%solve()
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',0_SNK)
      CALL thisA%init(pList)
      ALLOCATE(thisX(4))
      thisLS%X => thisX
      CALL thisLS%solve()
      ALLOCATE(thisLS%b(6))
      CALL thisLS%solve()
      CALL thisA%clear()
      DEALLOCATE(thisA)
      ALLOCATE(DenseRectMatrixType :: thisA)
      thisLS%A => thisA
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',7_SNK)
      CALL thisA%init(pList)
      CALL thisLS%solve()
      CALL thisA%clear()
      DEALLOCATE(thisA)
      CALL thisLS%clear()
    !Test GE
      !Dense square
      CALL thisLS%init(1,MPIEnv,OMPEnv,'testTimer')
      ALLOCATE(DenseSquareMatrixType :: thisA)

      !Singular dense matrix
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',0_SNK)
      CALL thisA%init(pList)
      !A=[1 2 3]  b=[6]   x=[*]
      !  [1 3 2]    [6]     [*]
      !  [1 3 2]    [6]     [*]
      CALL thisA%set(1,1,1._SRK)
      CALL thisA%set(1,2,2._SRK)
      CALL thisA%set(1,3,3._SRK)
      CALL thisA%set(2,1,1._SRK)
      CALL thisA%set(2,2,3._SRK)
      CALL thisA%set(2,3,2._SRK)
      CALL thisA%set(3,1,1._SRK)
      CALL thisA%set(3,2,3._SRK)
      CALL thisA%set(3,3,2._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      !Set b
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/6._SRK,6._SRK,6._SRK/)
      thisLS%hasB=.TRUE.
      !Build x
      ALLOCATE(thisX(3))
      thisLS%X => thisX
      CALL thisLS%solve()
      !Check the result
      IF(thisLS%info /= -1 ) THEN
        WRITE(*,*) 'CALL Direct%solve() -GE method FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisX,thisA)
      CALL thisLS%clear()
      !Normal dense matrix
      CALL thisLS%init(1,MPIEnv,OMPEnv,'testTimer')
      !Set A
      ALLOCATE(DenseSquareMatrixType :: thisA)
      CALL pList%clear()
      CALL pList%add('PL->n',4_SNK)
      CALL pList%add('PL->m',0_SNK)
      CALL thisA%init(pList)
      !A=[1 2 3 4]  b=[10]   x=[1]
      !  [1 3 2 3]    [9]      [1]
      !  [3 2 3 1]    [9]      [1]
      !  [1 1 1 1]    [4]      [1]
      CALL thisA%set(1,1,1._SRK)
      CALL thisA%set(1,2,2._SRK)
      CALL thisA%set(1,3,3._SRK)
      CALL thisA%set(1,4,4._SRK)
      CALL thisA%set(2,1,1._SRK)
      CALL thisA%set(2,2,3._SRK)
      CALL thisA%set(2,3,2._SRK)
      CALL thisA%set(2,4,3._SRK)
      CALL thisA%set(3,1,3._SRK)
      CALL thisA%set(3,2,2._SRK)
      CALL thisA%set(3,3,3._SRK)
      CALL thisA%set(3,4,1._SRK)
      CALL thisA%set(4,1,1._SRK)
      CALL thisA%set(4,2,1._SRK)
      CALL thisA%set(4,3,1._SRK)
      CALL thisA%set(4,4,1._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      !Set b
      ALLOCATE(thisLS%b(4))
      thisLS%b=(/10._SRK,9._SRK,9._SRK,4._SRK/)
      thisLS%hasB=.TRUE.
      !Build x
      ALLOCATE(thisX(4))
      thisLS%X => thisX
      CALL thisLS%solve()
      !Check the result
      IF(.NOT.( (thisLS%X(1) .APPROXEQ. 1._SRK) &
         .AND.(thisLS%X(2) .APPROXEQ. 1._SRK) &
         .AND.(thisLS%X(3) .APPROXEQ. 1._SRK) &
         .AND.(thisLS%X(4) .APPROXEQ. 1._SRK) &
         .AND.(thisLS%info == 0) )) THEN
        WRITE(*,*) 'CALL Direct%solve() -GE method FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisX,thisA)
      CALL thisLS%clear()
      !Tridiagonal
      !Normal case
      CALL thisLS%clear()
      CALL thisLS%init(1,MPIEnv,OMPEnv,'testTimer')
      ALLOCATE(TriDiagMatrixType :: thisA)
      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisA%init(pList)
      CALL thisA%set(1,1,4._SRK)
      CALL thisA%set(1,2,-1._SRK)
      CALL thisA%set(2,2,4._SRK)
      CALL thisA%set(2,3,-1._SRK)
      CALL thisA%set(3,3,4._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/1._SRK,2._SRK,3._SRK/)
      thisLS%hasB=.TRUE.
      ALLOCATE(thisX(3))
      thisLS%X => thisX
      CALL thisLS%solve()
      !Check M
      !M=[ 4   -1    0]
      !  [-.25 3.75 -1]
      !  [ 0   -.267  3.7333]
      SELECTTYPE(M => thisLS%M); TYPE IS(TriDiagMatrixType)
        IF(.NOT.((M%a(1,1) .APPROXEQ. 0.0_SRK) &
          .AND. (M%a(1,2) .APPROXEQ. -.25_SRK) &
          .AND. (M%a(1,3) .APPROXEQ. -0.266666666666666666_SRK) &
          .AND. (M%a(2,1) .APPROXEQ. 0.25_SRK) &
          .AND. (M%a(2,2) .APPROXEQ. (1.0_SRK/3.75_SRK)) &
          .AND. (M%a(2,3) .APPROXEQ. (1.0_SRK/3.7333333333333334_SRK)) &
          .AND. (M%a(3,1) .APPROXEQ. -1.0_SRK) &
          .AND. (M%a(3,2) .APPROXEQ. -1.0_SRK) &
          .AND. (M%a(3,3) .APPROXEQ. 0.0_SRK) &
          .AND. thisLS%isDecomposed)) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      !Check X
      IF(.NOT. ((thisLS%X(1) .APPROXEQ. 0.46428571428571430_SRK) &
        .AND. (thisLS%X(2) .APPROXEQ. 0.85714285714285721_SRK) &
          .AND. (thisLS%X(3) .APPROXEQ. 0.96428571428571430_SRK) &
            .AND. thisLS%info == 0)) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      !Reset X, and solve it again
      thisLS%X=1.0_SRK
      CALL thisLS%solve()
      IF(.NOT. ((thisLS%X(1) .APPROXEQ. 0.46428571428571430_SRK) &
        .AND. (thisLS%X(2) .APPROXEQ. 0.85714285714285721_SRK) &
          .AND. (thisLS%X(3) .APPROXEQ. 0.96428571428571430_SRK) &
            .AND. thisLS%isDecomposed)) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      CALL thisLS%clear()
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX)
      !Sparse matrix, just make sure it could go to CGNR and could
      !get result, the details will be test in CGNR
      CALL thisLS%init(1,MPIEnv,OMPEnv)
      ALLOCATE(SparseMatrixType :: thisA)
      CALL pList%clear()
      CALL pList%add('PL->n',9_SNK)
      CALL pList%add('PL->m',33_SNK)
      CALL thisA%init(pList)
      SELECTTYPE(thisA); TYPE IS(SparseMatrixType)
        CALL thisA%setShape(1,1,4.0_SRK)
        CALL thisA%setShape(1,2,-1.0_SRK)
        CALL thisA%setShape(1,4,-1.0_SRK)
        CALL thisA%setShape(2,1,-1.0_SRK)
        CALL thisA%setShape(2,2,4.0_SRK)
        CALL thisA%setShape(2,3,-1.0_SRK)
        CALL thisA%setShape(2,5,-1.0_SRK)
        CALL thisA%setShape(3,2,-1.0_SRK)
        CALL thisA%setShape(3,3,4.0_SRK)
        CALL thisA%setShape(3,6,-1.0_SRK)
        CALL thisA%setShape(4,1,-1.0_SRK)
        CALL thisA%setShape(4,4,4.0_SRK)
        CALL thisA%setShape(4,5,-1.0_SRK)
        CALL thisA%setShape(4,7,-1.0_SRK)
        CALL thisA%setShape(5,2,-1.0_SRK)
        CALL thisA%setShape(5,4,-1.0_SRK)
        CALL thisA%setShape(5,5,4.0_SRK)
        CALL thisA%setShape(5,6,-1.0_SRK)
        CALL thisA%setShape(5,8,-1.0_SRK)
        CALL thisA%setShape(6,3,-1.0_SRK)
        CALL thisA%setShape(6,5,-1.0_SRK)
        CALL thisA%setShape(6,6,4.0_SRK)
        CALL thisA%setShape(6,9,-1.0_SRK)
        CALL thisA%setShape(7,4,-1.0_SRK)
        CALL thisA%setShape(7,7,4.0_SRK)
        CALL thisA%setShape(7,8,-1.0_SRK)
        CALL thisA%setShape(8,5,-1.0_SRK)
        CALL thisA%setShape(8,7,-1.0_SRK)
        CALL thisA%setShape(8,8,4.0_SRK)
        CALL thisA%setShape(8,9,-1.0_SRK)
        CALL thisA%setShape(9,6,-1.0_SRK)
        CALL thisA%setShape(9,8,-1.0_SRK)
        CALL thisA%setShape(9,9,4.0_SRK)
      ENDSELECT
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      !Allocate it not associate it to a pointer, To improve coverage
      ALLOCATE(thisLS%X(9))
      thisLS%X=1.0_SRK
      !build b and set it
      ALLOCATE(thisLS%b(9))
      thisLS%hasB=.TRUE.
      thisLS%b=1.0_SRK
      !solve it
      CALL thisLS%solve()
      IF(thisLS%info /= 0) THEN
        WRITE(*,*) 'CALL Direct%solve() -GE method FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisA)
      CALL thisLS%clear()
      !DenseRect matrix, just make sure that it could go to CGNR.
      !The result will be checked later.
      CALL thisLS%init(1,MPIEnv,OMPEnv,'testTimer')
      ! A=[1 1]  b=[1]
      !   [1 2]    [2]
      !   [1 3]    [2]
      ALLOCATE(DenseRectMatrixType :: thisA)
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',2_SNK)
      CALL thisA%init(pList)
      CALL thisA%set(1,1,1._SRK)
      CALL thisA%set(1,2,1._SRK)
      CALL thisA%set(2,1,1._SRK)
      CALL thisA%set(2,2,2._SRK)
      CALL thisA%set(3,1,1._SRK)
      CALL thisA%set(3,2,3._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      !set b
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/1._SRK,2._SRK,2._SRK/)
      thisLS%hasB=.TRUE.
      !set x
      ALLOCATE(thisX(2))
      thisX=1.0_SRK
      thisLS%X => thisX
      !Solve it
      CALL thisLS%solve()
      IF(thisLS%info /= 0) THEN
        WRITE(*,*) 'CALL Direct%solve() -GE method FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX)
      CALL thisLS%clear()

    !Test LU (Dense square matrix and tridiagonal matrix
    !other matrix will raise error message)
      !Dense square
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      !Singular case
      ALLOCATE(DenseSquareMatrixType :: thisA)
      !A=[1 0 1 ]  b=[4]    x=[*]
      !  [2 5 -2]    [6]      [*]
      !  [1 0 1 ]    [4]      [*]
      !Set A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',0_SNK)
      CALL thisA%init(pList)
      CALL thisA%set(1,1,1._SRK)
      CALL thisA%set(1,2,0._SRK)
      CALL thisA%set(1,3,1._SRK)
      CALL thisA%set(2,1,2._SRK)
      CALL thisA%set(2,2,5._SRK)
      CALL thisA%set(2,3,-2._SRK)
      CALL thisA%set(3,1,1._SRK)
      CALL thisA%set(3,2,0._SRK)
      CALL thisA%set(3,3,1._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      !Set b
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/4._SRK,6._SRK,4._SRK/)
      thisLS%hasB=.TRUE.
      !Build x
      ALLOCATE(thisX(3))
      thisLS%X => thisX
      
      CALL thisLS%solve()
      IF(thisLS%info /= -1) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisX,thisA)
      CALL thisLS%clear()
      !Normal case
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      ALLOCATE(DenseSquareMatrixType :: thisA)
      !A=[1 0 1 ]  b=[4]    x=[1]
      !  [2 5 -2]    [6]      [2]
      !  [3 6 9 ]    [42]     [3]
      !Set A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',0_SNK)
      CALL thisA%init(pList)
      CALL thisA%set(1,1,1._SRK)
      CALL thisA%set(1,2,0._SRK)
      CALL thisA%set(1,3,1._SRK)
      CALL thisA%set(2,1,2._SRK)
      CALL thisA%set(2,2,5._SRK)
      CALL thisA%set(2,3,-2._SRK)
      CALL thisA%set(3,1,3._SRK)
      CALL thisA%set(3,2,6._SRK)
      CALL thisA%set(3,3,9._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      !Set b
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/4._SRK,6._SRK,42._SRK/)
      thisLS%hasB=.TRUE.
      !Build x
      ALLOCATE(thisX(3))
      thisLS%X => thisX
      CALL thisLS%solve()
      !Check M
      ! M=[3      6   9]
      !   [1/3    2  -2]
      !   [2/3 -1/2  -9]
      SELECTTYPE(M => thisLS%M); TYPE IS(DenseSquareMatrixType)
        IF(.NOT.((M%A(1,1) .APPROXEQ. 3._SRK) &
          .AND.(M%A(1,2) .APPROXEQ. 6._SRK) &
          .AND.(M%A(1,3) .APPROXEQ. 9._SRK) &
          .AND.(M%A(2,1) .APPROXEQ. 1._SRK/3._SRK) &
          .AND.(M%A(2,2) .APPROXEQ. -2._SRK) &
          .AND.(M%A(2,3) .APPROXEQ. -2._SRK) &
          .AND.(M%A(3,1) .APPROXEQ. 2._SRK/3._SRK) &
          .AND.(M%A(3,2) .APPROXEQ. -0.5_SRK) &
          .AND.(M%A(3,3) .APPROXEQ. -9._SRK) )) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      !Check IPIV: IPIV=[3 3 0]
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        IF(thisLS%IPIV(1) /= 3 .OR. thisLS%IPIV(2) /= 3 &
          .OR. thisLS%IPIV(3) /= 0 ) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      !Check X
      IF(.NOT.((thisLS%X(1) .APPROXEQ. 1._SRK) &
         .AND. (thisLS%X(2) .APPROXEQ. 2._SRK) &
         .AND. (thisLS%X(3) .APPROXEQ. 3._SRK) &
         .AND. (thisLS%info == 0))) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      !Reset right hand side and solve it again
      thisLS%b=(/4._SRK,14._SRK,30._SRK/)
      thisLS%isDecomposed=.FALSE.
      thisLS%hasB=.TRUE.
      CALL thisLS%solve()
      IF(.NOT.((thisLS%X(1) .APPROXEQ. 3._SRK) &
         .AND. (thisLS%X(2) .APPROXEQ. 2._SRK) &
         .AND. (thisLS%X(3) .APPROXEQ. 1._SRK) &
         .AND. (thisLS%info == 0))) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisX,thisA)

      !Tridiagonal
      !Singular case
      CALL thisLS%clear()
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      ALLOCATE(TriDiagMatrixType :: thisA)
      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0  0  0]
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisA%init(pList)
      CALL thisA%set(1,1,4._SRK)
      CALL thisA%set(1,2,-1._SRK)
      CALL thisA%set(2,2,4._SRK)
      CALL thisA%set(2,3,0._SRK)
      CALL thisA%set(3,3,0._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/1._SRK,2._SRK,3._SRK/)
      thisLS%hasB=.TRUE.
      ALLOCATE(thisX(3))
      thisLS%X => thisX
      CALL thisLS%solve()
      !Check X
      IF(thisLS%info /= -1) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX)
      !Normal case
      CALL thisLS%clear()
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      ALLOCATE(TriDiagMatrixType :: thisA)
      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisA%init(pList)
      CALL thisA%set(1,1,4._SRK)
      CALL thisA%set(1,2,-1._SRK)
      CALL thisA%set(2,2,4._SRK)
      CALL thisA%set(2,3,-1._SRK)
      CALL thisA%set(3,3,4._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/1._SRK,2._SRK,3._SRK/)
      thisLS%hasB=.TRUE.
      ALLOCATE(thisX(3))
      thisLS%X => thisX
      CALL thisLS%solve()
      !Check M
      !M=[ 4   -1    0]
      !  [-.25 3.75 -1]
      !  [ 0   -.267  3.7333]
      SELECTTYPE(M => thisLS%M); TYPE IS(TriDiagMatrixType)
        IF(.NOT.((M%a(1,1) .APPROXEQ. 0.0_SRK) &
          .AND. (M%a(1,2) .APPROXEQ. -.25_SRK) &
          .AND. (M%a(1,3) .APPROXEQ. -0.266666666666666666_SRK) &
          .AND. (M%a(2,1) .APPROXEQ. 0.25_SRK) &
          .AND. (M%a(2,2) .APPROXEQ. (1.0_SRK/3.75_SRK)) &
          .AND. (M%a(2,3) .APPROXEQ. (1.0_SRK/3.7333333333333334_SRK)) &
          .AND. (M%a(3,1) .APPROXEQ. -1.0_SRK) &
          .AND. (M%a(3,2) .APPROXEQ. -1.0_SRK) &
          .AND. (M%a(3,3) .APPROXEQ. 0.0_SRK) &
          .AND. thisLS%isDecomposed)) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      !Check X
      IF(.NOT. ((thisLS%X(1) .APPROXEQ. 0.46428571428571430_SRK) &
        .AND. (thisLS%X(2) .APPROXEQ. 0.85714285714285721_SRK) &
          .AND. (thisLS%X(3) .APPROXEQ. 0.96428571428571430_SRK) &
            .AND. thisLS%info == 0)) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      !Reset X, and solve it again
      thisLS%X=1.0_SRK
      CALL thisLS%solve()
      IF(.NOT. ((thisLS%X(1) .APPROXEQ. 0.46428571428571430_SRK) &
        .AND. (thisLS%X(2) .APPROXEQ. 0.85714285714285721_SRK) &
          .AND. (thisLS%X(3) .APPROXEQ. 0.96428571428571430_SRK) &
            .AND. thisLS%isDecomposed)) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      CALL thisLS%clear()
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX)
      !test solve for TriDiag with a non-diagonally dominant A
      !just make sure we get the output statement.
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      ALLOCATE(TriDiagMatrixType :: thisA)
      !A=[ O.5  -1    0]
      !  [-1   0.5   -1]
      !  [ 0    -1  0.5]
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisA%init(pList)  !symmetric
      CALL thisA%set(1,1,0.5_SRK)
      CALL thisA%set(1,2,-1._SRK)
      CALL thisA%set(2,2,0.5_SRK)
      CALL thisA%set(2,3,-1._SRK)
      CALL thisA%set(3,3,0.5_SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/1._SRK,2._SRK,3._SRK/)
      thisLS%hasB=.TRUE.
      ALLOCATE(thisX(3))
      thisLS%X => thisX
      CALL thisLS%solve()
      CALL thisA%clear()
      CALL thisLS%clear()
      DEALLOCATE(thisA,thisX)

      !Sparse matrix, just make sure it could go to CGNR and could
      !get result, the details will be test in CGNR
      CALL thisLS%init(1,MPIEnv,OMPEnv)
      ALLOCATE(SparseMatrixType :: thisA)
      CALL pList%clear()
      CALL pList%add('PL->n',9_SNK)
      CALL pList%add('PL->m',33_SNK)
      CALL thisA%init(pList)
      SELECTTYPE(thisA); TYPE IS(SparseMatrixType)
        CALL thisA%setShape(1,1,4.0_SRK)
        CALL thisA%setShape(1,2,-1.0_SRK)
        CALL thisA%setShape(1,4,-1.0_SRK)
        CALL thisA%setShape(2,1,-1.0_SRK)
        CALL thisA%setShape(2,2,4.0_SRK)
        CALL thisA%setShape(2,3,-1.0_SRK)
        CALL thisA%setShape(2,5,-1.0_SRK)
        CALL thisA%setShape(3,2,-1.0_SRK)
        CALL thisA%setShape(3,3,4.0_SRK)
        CALL thisA%setShape(3,6,-1.0_SRK)
        CALL thisA%setShape(4,1,-1.0_SRK)
        CALL thisA%setShape(4,4,4.0_SRK)
        CALL thisA%setShape(4,5,-1.0_SRK)
        CALL thisA%setShape(4,7,-1.0_SRK)
        CALL thisA%setShape(5,2,-1.0_SRK)
        CALL thisA%setShape(5,4,-1.0_SRK)
        CALL thisA%setShape(5,5,4.0_SRK)
        CALL thisA%setShape(5,6,-1.0_SRK)
        CALL thisA%setShape(5,8,-1.0_SRK)
        CALL thisA%setShape(6,3,-1.0_SRK)
        CALL thisA%setShape(6,5,-1.0_SRK)
        CALL thisA%setShape(6,6,4.0_SRK)
        CALL thisA%setShape(6,9,-1.0_SRK)
        CALL thisA%setShape(7,4,-1.0_SRK)
        CALL thisA%setShape(7,7,4.0_SRK)
        CALL thisA%setShape(7,8,-1.0_SRK)
        CALL thisA%setShape(8,5,-1.0_SRK)
        CALL thisA%setShape(8,7,-1.0_SRK)
        CALL thisA%setShape(8,8,4.0_SRK)
        CALL thisA%setShape(8,9,-1.0_SRK)
        CALL thisA%setShape(9,6,-1.0_SRK)
        CALL thisA%setShape(9,8,-1.0_SRK)
        CALL thisA%setShape(9,9,4.0_SRK)
      ENDSELECT
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      ALLOCATE(thisLS%X(9))
      thisLS%X=1.0_SRK
      !build b and set it
      ALLOCATE(thisLS%b(9))
      thisLS%hasB=.TRUE.
      thisLS%b=1.0_SRK
      !solve it
      CALL thisLS%solve()
      IF(thisLS%info /= 0) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisA)
      CALL thisLS%clear()
      !DenseRect matrix, just make sure that it could go to CGNR.
      !The result will be checked later.
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      ! A=[1 1]  b=[1]
      !   [1 2]    [2]
      !   [1 3]    [2]
      ALLOCATE(DenseRectMatrixType :: thisA)
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',2_SNK)
      CALL thisA%init(pList)
      CALL thisA%set(1,1,1._SRK)
      CALL thisA%set(1,2,1._SRK)
      CALL thisA%set(2,1,1._SRK)
      CALL thisA%set(2,2,2._SRK)
      CALL thisA%set(3,1,1._SRK)
      CALL thisA%set(3,2,3._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      !set b
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/1._SRK,2._SRK,2._SRK/)
      thisLS%hasB=.TRUE.
      !set x
      ALLOCATE(thisX(2))
      thisX=1.0_SRK
      thisLS%X => thisX
      !Solve it
      CALL thisLS%solve()
      IF(thisLS%info /= 0) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL Direct%solve()'
    !end test of direct solver
      CALL thisLS%clear()
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX)
      DEALLOCATE(thisLS)
      CALL MPIEnv%clear()
      CALL OMPEnv%clear()
      DEALLOCATE(MPIEnv,OMPEnv)
    ENDSUBROUTINE testDirectSolve
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIterativeOthers()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      TYPE(MPI_EnvType),POINTER :: MPIEnv
      TYPE(OMP_EnvType),POINTER :: OMPEnv
      CLASS(MatrixType),POINTER :: thisA
      REAL(SRK),POINTER :: thisX(:),thisX2(:)
      REAL(SRK),ALLOCATABLE :: resid_soln(:),resid(:)

      !initialize supporting variables
      ALLOCATE(MPIEnv,OMPEnv)
      CALL MPIEnv%initialize(PE_COMM_SELF)
      CALL OMPEnv%initialize(1)
      ALLOCATE(LinearSolverType_Iterative :: thisLS)
    !Test setX0
      ALLOCATE(thisX(2))
      thisX=(/0._SRK,0._SRK/)
      ALLOCATE(thisX2(3))
      thisX2=(/1._SRK,2._SRK,3._SRK/)
      !test case that is expected to work, thisX has already been allocated
      CALL thisLS%init(1,MPIEnv,OMPEnv)
      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Iterative)
        thisLS%X => thisX
        CALL thisLS%setX0(thisX2)
        IF(.NOT. (ASSOCIATED(thisLS%X,thisX2) .AND. thisLS%hasX0)) THEN
          WRITE(*,*) 'CALL Iterative%setX0(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      CALL thisLS%clear()
      WRITE(*,*) '  Passed: CALL Iterative%setX0(...)'
      DEALLOCATE(thisX,thisX2)
    !Test setConv
      !Bad input
      CALL thisLS%init(1,MPIEnv,OMPEnv)
      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Iterative)
        CALL thisLS%setConv(-2,-0.1_SRK,-1)
        CALL thisLS%setConv(-2,1.1_SRK,-1)
        !Check if default value is used
        IF(thisLS%maxIters /= 1000_SIK .OR. thisLS%normType /= 2_SIK &
          .OR. thisLS%convTol /= 0.001_SRK) THEN
          WRITE(*,*) 'CALL Iterative%setConv(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      !Correct input
      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Iterative)
        CALL thisLS%setConv(1_SIK,0.01_SRK,100_SIK)
        IF(thisLS%maxIters /= 100_SIK .OR. thisLS%normType /= 1_SIK &
          .OR. thisLS%convTol /= 0.01_SRK) THEN
          WRITE(*,*) 'CALL Iterative%setConv(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      CALL thisLS%clear()
      WRITE(*,*) '  Passed: CALL Iterative%setConv(...)'
    !Test getResidual
      !Bad input
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%getResidual(resid)
        CALL thisLS%init(1,MPIEnv,OMPEnv)
        ALLOCATE(SparseMatrixType :: thisA)
        thisLS%A => thisA
        ALLOCATE(thisLS%b(4),thisX(4))
        thisLS%X => thisX
        CALL thisLS%getResidual(resid)

        ALLOCATE(resid(4))
        CALL thisLS%getResidual(resid)
        
        CALL thisLS%clear()
        CALL thisA%clear()
        DEALLOCATE(resid,thisX,thisA)
      ENDSELECT
      !Correct input
      CALL thisLS%init(1,MPIEnv,OMPEnv)
      ALLOCATE(SparseMatrixType :: thisA)
      CALL pList%clear()
      CALL pList%add('PL->n',9_SNK)
      CALL pList%add('PL->m',33_SNK)
      CALL thisA%init(pList)
      !A =  4    -1     0    -1     0     0     0     0     0
      !    -1     4    -1     0    -1     0     0     0     0
      !     0    -1     4     0     0    -1     0     0     0
      !    -1     0     0     4    -1     0    -1     0     0
      !     0    -1     0    -1     4    -1     0    -1     0
      !     0     0    -1     0    -1     4     0     0    -1
      !     0     0     0    -1     0     0     4    -1     0
      !     0     0     0     0    -1     0    -1     4    -1
      !     0     0     0     0     0    -1     0    -1     4
      SELECTTYPE(thisA); TYPE IS(SparseMatrixType)
        CALL thisA%setShape(1,1,4.0_SRK)
        CALL thisA%setShape(1,2,-1.0_SRK)
        CALL thisA%setShape(1,4,-1.0_SRK)
        CALL thisA%setShape(2,1,-1.0_SRK)
        CALL thisA%setShape(2,2,4.0_SRK)
        CALL thisA%setShape(2,3,-1.0_SRK)
        CALL thisA%setShape(2,5,-1.0_SRK)
        CALL thisA%setShape(3,2,-1.0_SRK)
        CALL thisA%setShape(3,3,4.0_SRK)
        CALL thisA%setShape(3,6,-1.0_SRK)
        CALL thisA%setShape(4,1,-1.0_SRK)
        CALL thisA%setShape(4,4,4.0_SRK)
        CALL thisA%setShape(4,5,-1.0_SRK)
        CALL thisA%setShape(4,7,-1.0_SRK)
        CALL thisA%setShape(5,2,-1.0_SRK)
        CALL thisA%setShape(5,4,-1.0_SRK)
        CALL thisA%setShape(5,5,4.0_SRK)
        CALL thisA%setShape(5,6,-1.0_SRK)
        CALL thisA%setShape(5,8,-1.0_SRK)
        CALL thisA%setShape(6,3,-1.0_SRK)
        CALL thisA%setShape(6,5,-1.0_SRK)
        CALL thisA%setShape(6,6,4.0_SRK)
        CALL thisA%setShape(6,9,-1.0_SRK)
        CALL thisA%setShape(7,4,-1.0_SRK)
        CALL thisA%setShape(7,7,4.0_SRK)
        CALL thisA%setShape(7,8,-1.0_SRK)
        CALL thisA%setShape(8,5,-1.0_SRK)
        CALL thisA%setShape(8,7,-1.0_SRK)
        CALL thisA%setShape(8,8,4.0_SRK)
        CALL thisA%setShape(8,9,-1.0_SRK)
        CALL thisA%setShape(9,6,-1.0_SRK)
        CALL thisA%setShape(9,8,-1.0_SRK)
        CALL thisA%setShape(9,9,4.0_SRK)
      ENDSELECT
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      ALLOCATE(thisX(9))
      thisX=1._SRK
      thisLS%X => thisX
      ALLOCATE(thisLS%b(9))
      thisLS%b=10._SRK
      thisLS%hasB=.TRUE.
      ALLOCATE(resid_soln(9))
      resid_soln=(/-8._SRK,-9._SRK,-8._SRK,-9._SRK,-10._SRK, &
        -9._SRK,-8._SRK,-9._SRK,-8._SRK/)
      ALLOCATE(resid(9))
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%getResidual(resid)
      ENDSELECT
      IF(ANY(.NOT.(resid .APPROXEQ. resid_soln))) THEN
        WRITE(*,*) 'CALL Iterative%getResidual(...) FAILED!'
        STOP 666
      ENDIF

      WRITE(*,*) '  Passed: CALL Iterative%getResidual(...)'
      DEALLOCATE(resid)
      DEALLOCATE(resid_soln)
      CALL thisLS%clear()
      CALL MPIEnv%clear()
      CALL OMPEnv%clear()
      DEALLOCATE(thisLS,MPIEnv,OMPEnv)
    ENDSUBROUTINE testIterativeOthers
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIterativeSolve()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      TYPE(MPI_EnvType),POINTER :: MPIEnv
      TYPE(OMP_EnvType),POINTER :: OMPEnv
      CLASS(MatrixType),POINTER :: thisA
      REAL(SRK),POINTER :: thisX(:)
      REAL(SRK),ALLOCATABLE :: thisB(:)
      INTEGER(SIK) :: i
      LOGICAL(SBK) :: match

      !initialize supporting variables
      ALLOCATE(MPIEnv,OMPEnv)
      CALL MPIEnv%initialize(PE_COMM_SELF)
      CALL OMPEnv%initialize(1)
      ALLOCATE(LinearSolverType_Iterative :: thisLS)

    !With BiCGSTAB
      !The sparse matrix type
      CALL thisLS%init(1,MPIEnv,OMPEnv,'testTimer') !1 for BiCGSTAB
      ALLOCATE(SparseMatrixType :: thisA)
      CALL thisA%clear()
      CALL pList%clear()
      CALL pList%add('PL->n',9_SNK)
      CALL pList%add('PL->m',33_SNK)
      CALL thisA%init(pList)
      !A =  4    -1     0    -1     0     0     0     0     0
      !    -1     4    -1     0    -1     0     0     0     0
      !     0    -1     4     0     0    -1     0     0     0
      !    -1     0     0     4    -1     0    -1     0     0
      !     0    -1     0    -1     4    -1     0    -1     0
      !     0     0    -1     0    -1     4     0     0    -1
      !     0     0     0    -1     0     0     4    -1     0
      !     0     0     0     0    -1     0    -1     4    -1
      !     0     0     0     0     0    -1     0    -1     4
      SELECTTYPE(thisA)
        TYPE IS(SparseMatrixType)
          CALL thisA%setShape(1,1,4.0_SRK)
          CALL thisA%setShape(1,2,-1.0_SRK)
          CALL thisA%setShape(1,4,-1.0_SRK)
          CALL thisA%setShape(2,1,-1.0_SRK)
          CALL thisA%setShape(2,2,4.0_SRK)
          CALL thisA%setShape(2,3,-1.0_SRK)
          CALL thisA%setShape(2,5,-1.0_SRK)
          CALL thisA%setShape(3,2,-1.0_SRK)
          CALL thisA%setShape(3,3,4.0_SRK)
          CALL thisA%setShape(3,6,-1.0_SRK)
          CALL thisA%setShape(4,1,-1.0_SRK)
          CALL thisA%setShape(4,4,4.0_SRK)
          CALL thisA%setShape(4,5,-1.0_SRK)
          CALL thisA%setShape(4,7,-1.0_SRK)
          CALL thisA%setShape(5,2,-1.0_SRK)
          CALL thisA%setShape(5,4,-1.0_SRK)
          CALL thisA%setShape(5,5,4.0_SRK)
          CALL thisA%setShape(5,6,-1.0_SRK)
          CALL thisA%setShape(5,8,-1.0_SRK)
          CALL thisA%setShape(6,3,-1.0_SRK)
          CALL thisA%setShape(6,5,-1.0_SRK)
          CALL thisA%setShape(6,6,4.0_SRK)
          CALL thisA%setShape(6,9,-1.0_SRK)
          CALL thisA%setShape(7,4,-1.0_SRK)
          CALL thisA%setShape(7,7,4.0_SRK)
          CALL thisA%setShape(7,8,-1.0_SRK)
          CALL thisA%setShape(8,5,-1.0_SRK)
          CALL thisA%setShape(8,7,-1.0_SRK)
          CALL thisA%setShape(8,8,4.0_SRK)
          CALL thisA%setShape(8,9,-1.0_SRK)
          CALL thisA%setShape(9,6,-1.0_SRK)
          CALL thisA%setShape(9,8,-1.0_SRK)
          CALL thisA%setShape(9,9,4.0_SRK)
      ENDSELECT
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      !build X0 and set it to 1.0s
      ALLOCATE(thisX(9))
      thisX=1.0_SRK
      selecttype(thisLS)
        TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setX0(thisX)
      ENDSELECT
      !build b and set it
      ALLOCATE(thisLS%b(9))
      thisLS%hasB=.TRUE.
      thisLS%b=1.0_SRK
      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK)
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
        IF(NINT(thisB(i)) /= NINT(10000.0_SRK*thisLS%X(i))) THEN
          match=.FALSE.
          EXIT
        ENDIF
      ENDDO
      IF(.NOT. match) THEN
        WRITE(*,*) 'CALL Iterative%solve() -BICGSTAB FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX,thisB)
      CALL thisLS%clear()
    !test with A being densesquare
      CALL thisLS%init(1,MPIEnv,OMPEnv,'testTimer') !1 for BiCGSTAB
      !build thisA
      ALLOCATE(DenseSquareMatrixType :: thisA)
      CALL pList%clear()
      CALL pList%add('PL->n',9_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisA%init(pList) !9x9, symmetric.
      DO i=1,9
        CALL thisA%set(i,i,4.0_SRK)
        IF((i < 9).AND.((i /= 3).AND.(i /= 6))) THEN
          CALL thisA%set(i,i+1,-1.0_SRK)
        ENDIF
        IF(i < 7) THEN
          CALL thisA%set(i,i+3,-1.0_SRK)
        ENDIF
      ENDDO
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      !build X0 and set it to 1.0s
      ALLOCATE(thisX(9))
      thisX=1.0_SRK
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT
      !build b and set it
      ALLOCATE(thisLS%b(9))
      thisLS%hasB=.TRUE.
      thisLS%b=1.0_SRK
      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK)
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
        IF(NINT(thisB(i)) /= NINT(10000.0_SRK*thisLS%X(i))) THEN
          match=.FALSE.
          EXIT
        ENDIF
      ENDDO
      IF(.NOT. match) THEN
        WRITE(*,*) 'CALL Iterative%solve() - BiCGSTAB FAILED!'
        STOP 666
      ENDIF
      !test to see how it performs with an already decomposed M
      !reset X to 1.0s
      thisX=1.0_SRK
      CALL thisLS%solve()
      !WRITE(*,'(9F10.4)') thisLS%x
      SELECTTYPE(thisLS)
        TYPE IS(LinearSolverType_Iterative)
          !WRITE(*,*) thisLS%iters
      ENDSELECT
      match=.TRUE.
      DO i=1,SIZE(thisB)
        IF(NINT(thisB(i)) /= NINT(10000.0_SRK*thisLS%X(i))) THEN
          match=.FALSE.
          EXIT
        ENDIF
      ENDDO
      IF(.NOT. match) THEN
        WRITE(*,*) 'CALL Iterative%solve() - BiCGSTAB FAILED!'
        STOP 666
      ENDIF
      CALL thisLS%clear()
      CALL thisA%clear()
      DEALLOCATE(thisA)
      !TriDiagonal matrix, it will go to LU method
      CALL thisLS%clear()
      CALL thisLS%init(1,MPIEnv,OMPEnv,'testTimer')
      ALLOCATE(TriDiagMatrixType :: thisA)
      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisA%init(pList)
      CALL thisA%set(1,1,4._SRK)
      CALL thisA%set(1,2,-1._SRK)
      CALL thisA%set(2,2,4._SRK)
      CALL thisA%set(2,3,-1._SRK)
      CALL thisA%set(3,3,4._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/1._SRK,2._SRK,3._SRK/)
      thisLS%hasB=.TRUE.
      ALLOCATE(thisX(3))
      thisLS%X => thisX
      CALL thisLS%solve()
      IF(thisLS%info /= 0) THEN
        WRITE(*,*) 'CALL Iterative%solve() - BiCGSTAB FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX)
      CALL thisLS%clear()

      !DenseRect matrix
      CALL thisLS%init(1,MPIEnv,OMPEnv,'testTimer')
      ! A=[1 1]  b=[1]
      !   [1 2]    [2]
      !   [1 3]    [2]
      ALLOCATE(DenseRectMatrixType :: thisA)
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',2_SNK)
      CALL thisA%init(pList)
      CALL thisA%set(1,1,1._SRK)
      CALL thisA%set(1,2,1._SRK)
      CALL thisA%set(2,1,1._SRK)
      CALL thisA%set(2,2,2._SRK)
      CALL thisA%set(3,1,1._SRK)
      CALL thisA%set(3,2,3._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      !set b
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/1._SRK,2._SRK,2._SRK/)
      thisLS%hasB=.TRUE.
      !set x
      ALLOCATE(thisX(2))
      thisX=1.0_SRK
      thisLS%X => thisX
      !set iterations and convergence information and build/set M
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
        CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK)
      ENDSELECT
      !Solve it
      CALL thisLS%solve()
      IF(thisLS%info /= 0) THEN
        WRITE(*,*) 'CALL Iterative%solve() -BiCGSTAB method FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX)
      CALL thisLS%clear()
      !test CGNR
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      !underdetermined matrix: solver%info should return -1.
      ! A=[1 1 1]  b= [1]
      !   [1 2 3]     [3]
      !set A
      ALLOCATE(DenseRectMatrixType :: thisA)
      CALL pList%clear()
      CALL pList%add('PL->n',2_SNK)
      CALL pList%add('PL->m',3_SNK)
      CALL thisA%init(pList)
      CALL thisA%set(1,1,1._SRK)
      CALL thisA%set(1,2,1._SRK)
      CALL thisA%set(1,3,1._SRK)
      CALL thisA%set(2,1,1._SRK)
      CALL thisA%set(2,2,2._SRK)
      CALL thisA%set(2,3,3._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      !set b
      ALLOCATE(thisLS%b(2))
      thisLS%b=(/1._SRK,3._SRK/)
      thisLS%hasB=.TRUE.
      !set x
      ALLOCATE(thisX(3))
      thisX=1.0_SRK
      thisLS%X => thisX
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK)
      ENDSELECT
      CALL thisLS%solve()
      IF(thisLS%info /= -1) THEN
        WRITE(*,*)'CALL Iterative%solve() -CGNR FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX)
      CALL thisLS%clear()
      !normal or overdetermined matrix should give answers
       CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      ! A=[1 1]  b=[1]
      !   [1 2]    [2]
      !   [1 3]    [2]
      ALLOCATE(DenseRectMatrixType :: thisA)
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',2_SNK)
      CALL thisA%init(pList)
      CALL thisA%set(1,1,1._SRK)
      CALL thisA%set(1,2,1._SRK)
      CALL thisA%set(2,1,1._SRK)
      CALL thisA%set(2,2,2._SRK)
      CALL thisA%set(3,1,1._SRK)
      CALL thisA%set(3,2,3._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      !set b
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/1._SRK,2._SRK,2._SRK/)
      thisLS%hasB=.TRUE.
      !set x
      ALLOCATE(thisX(2))
      thisX=1.0_SRK
      thisLS%X => thisX
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT
      !set iterations and convergence information and
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setConv(2_SIK,1.0E-13_SRK,1000_SIK)
      ENDSELECT
      CALL thisLS%solve()
      IF(.NOT.((thisLS%X(1) .APPROXEQ. 2._SRK/3._SRK) &
         .AND. (thisLS%X(2) .APPROXEQ. 0.5_SRK))) THEN
        WRITE(*,*)'CALL Iterative%solve() FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX)
      CALL thisLS%clear()
      !DenseSquare matrix
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      ALLOCATE(DenseSquareMatrixType :: thisA)
      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisA%init(pList)
      CALL thisA%set(1,1,4._SRK)
      CALL thisA%set(1,2,-1._SRK)
      CALL thisA%set(2,2,4._SRK)
      CALL thisA%set(2,3,-1._SRK)
      CALL thisA%set(3,3,4._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/1._SRK,2._SRK,3._SRK/)
      thisLS%hasB=.TRUE.
      ALLOCATE(thisX(3))
      thisLS%X => thisX
      !set iterations and convergence information and
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setConv(2_SIK,1.0E-13_SRK,1000_SIK)
      ENDSELECT
      CALL thisLS%solve()
      !Check X
      IF(.NOT. ((thisLS%X(1) .APPROXEQ. 0.46428571428571430_SRK) &
        .AND. (thisLS%X(2) .APPROXEQ. 0.85714285714285721_SRK) &
          .AND. (thisLS%X(3) .APPROXEQ. 0.96428571428571430_SRK) &
            .AND. thisLS%info == 0)) THEN
        WRITE(*,*) 'CALL Iterative%solve() -CGNR method FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX)
      CALL thisLS%clear()
      !Sparse matrix
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      ALLOCATE(SparseMatrixType :: thisA)
      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',7_SNK)
      CALL thisA%init(pList)
      SELECTTYPE(thisA); TYPE IS(SparseMatrixType)
        CALL thisA%setShape(1,1,4._SRK)
        CALL thisA%setShape(1,2,-1._SRK)
        CALL thisA%setShape(2,1,-1._SRK)
        CALL thisA%setShape(2,2,4._SRK)
        CALL thisA%setShape(2,3,-1._SRK)
        CALL thisA%setShape(3,2,-1._SRK)
        CALL thisA%setShape(3,3,4._SRK)
      ENDSELECT
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/1._SRK,2._SRK,3._SRK/)
      thisLS%hasB=.TRUE.
      ALLOCATE(thisX(3))
      thisX=0._SRK
      thisLS%X => thisX
      !set iterations and convergence information and
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
        CALL thisLS%setConv(2_SIK,1.0E-13_SRK,1000_SIK)
      ENDSELECT
      !Check X
      CALL thisLS%solve()
      IF(.NOT. ((thisLS%X(1) .APPROXEQ. 0.46428571428571430_SRK) &
        .AND. (thisLS%X(2) .APPROXEQ. 0.85714285714285721_SRK) &
          .AND. (thisLS%X(3) .APPROXEQ. 0.96428571428571430_SRK) &
            .AND. thisLS%info == 0)) THEN
        WRITE(*,*) 'CALL Iterative%solve() -CGNR method FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX)
      CALL thisLS%clear()
      !TriDiagonal matrix, it will go to LU
      CALL thisLS%init(2,MPIEnv,OMPEnv,'testTimer')
      ALLOCATE(TriDiagMatrixType :: thisA)
      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisA%init(pList)
      CALL thisA%set(1,1,4._SRK)
      CALL thisA%set(1,2,-1._SRK)
      CALL thisA%set(2,2,4._SRK)
      CALL thisA%set(2,3,-1._SRK)
      CALL thisA%set(3,3,4._SRK)
      thisLS%A => thisA
      thisLS%hasA=.TRUE.
      ALLOCATE(thisLS%b(3))
      thisLS%b=(/1._SRK,2._SRK,3._SRK/)
      thisLS%hasB=.TRUE.
      ALLOCATE(thisX(3))
      thisLS%X => thisX
      CALL thisLS%solve()
      IF(thisLS%info /= 0) THEN
        WRITE(*,*) 'CALL Iterative%solve() - BiCGSTAB FAILED!'
        STOP 666
      ENDIF
      CALL thisA%clear()
      DEALLOCATE(thisA,thisX)
      CALL thisLS%clear()
      WRITE(*,*)'  Passed: CALL Iterative%solver()'
      CALL MPIEnv%clear()
      CALL OMPEnv%clear()
      CALL thisLS%clear()
      DEALLOCATE(thisLS,MPIEnv,OMPEnv)
    ENDSUBROUTINE testIterativeSolve
!
!-------------------------------------------------------------------------------
    SUBROUTINE testNorms()
      INTEGER(SIK) :: normType
      REAL(SRK),DIMENSION(10) :: x
      REAL(SRK) :: norm
      !set up x
      x=(/0._SRK,-1._SRK,2._SRK,-3._SRK,4._SRK, & 
          -5._SRK,6._SRK,-7._SRK,8._SRK,-9._SRK/)
      !test normType 1
      normType=1 !taxicab norm, just the absolute sum of these.
      !expected answer = 45
      CALL LNorm(x,normType,norm)
      IF(.NOT. (norm .APPROXEQ. 45._SRK)) THEN
        WRITE(*,*) 'CALL LNorm() - 1-NORM FAILED!'
        STOP 666
      ENDIF
      !test normType 2
      normType=2 !Euclidean norm
      !expected answer = 16.88194301613413218312
      CALL LNorm(x,normType,norm)
      IF(.NOT. (norm .APPROXEQ. 16.88194301613413218312_SRK)) THEN
        WRITE(*,*) 'CALL LNorm() - 2-NORM FAILED!'
        STOP 666
      ENDIF
      !test normType -1
      normType=-1 !Infinite norm
      CALL LNorm(x,normType,norm)
      !expected answer = 9.0
      IF(.NOT. (norm .APPROXEQ. 9.0_SRK)) THEN
        WRITE(*,*) 'CALL LNorm() - INFINITE-NORM FAILED!'
        WRITE(*,*) norm
        STOP 666
      ENDIF
      !test normType 3 (just some p-norm)
      normType=3 !L-norm, w/ L=3
      CALL LNorm(x,normType,norm)
      !expected answer = 12.65148997952623864269
      IF(.NOT. (norm .APPROXEQ. 12.65148997952623864269_SRK)) THEN
        WRITE(*,*) 'CALL LNorm() - L-NORM FAILED!'
        WRITE(*,*) norm
        STOP 666
      ENDIF
      !test an invalid norm (<=-2)
      normType=-2
      CALL LNorm(x,normType,norm)
      !expected answer = 0.0
      IF(.NOT. (norm == 0.0_SRK)) THEN
        WRITE(*,*) 'CALL LNorm() - L-NORM FAILED!'
        WRITE(*,*) norm
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL LNORM(...)'
    ENDSUBROUTINE testNorms

ENDPROGRAM testLinearSolver
