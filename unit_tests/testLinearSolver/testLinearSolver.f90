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
  USE VectorTypes
  USE ParallelEnv
  USE ParameterLists
  IMPLICIT NONE
  
  TYPE(ExceptionHandlerType),POINTER :: e
  TYPE(MPI_EnvType) :: mpiTestEnv
  TYPE(ParamType) :: pList, optListLS
  
#ifdef HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
  PetscErrorCode  :: ierr
  
  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

  !> set up default parameter list
  CALL optListLS%clear()
  CALL optListLS%add('PL->matrixType',SPARSE)
  CALL optListLS%add('PL->TPLType',NATIVE)   
  CALL optListLS%add('PL->solverMethod',1_SNK) ! GE or BICGSTAB
  CALL optListLS%add('PL->numberMPI',PE_COMM_SELF)
  CALL optListLS%add('PL->numberOMP',1_SNK)
  CALL optListLS%add('PL->timerName','LinearSolver Timer')

  !Configure exception handler for test
  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  eLinearSolverType => e
  CALL mpiTestEnv%initialize(PE_COMM_SELF)

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
#else
  CALL mpiTestEnv%finalize()
#endif
  
  DEALLOCATE(e)
  
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testClear()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS

    !test Direct
      ALLOCATE(LinearSolverType_Direct :: thisLS)
      
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
      
        !first build one by hand to test clear
        thisLS%isInit=.TRUE.
        thisLS%solverMethod=BICGSTAB
        thisLS%info=2
        thisLS%isDecomposed=.TRUE.
        CALL thisLS%MPIparallelEnv%initialize(0)
        CALL thisLS%OMPparallelEnv%initialize(1)
        
        ! initialize matrix A
        ALLOCATE(DenseSquareMatrixType :: thisLS%A)
        CALL pList%clear()
        CALL pList%add('PL->n',2_SNK)
        CALL pList%add('PL->m',1_SNK)
        CALL thisLS%A%init(pList) !2x2, symmetric
        
        ! initialize vector X
        ALLOCATE(RealVectorType :: thisLS%X)
        CALL thisLS%X%init(2)
        
        ! initialize vector b
        ALLOCATE(RealVectorType :: thisLS%b)
        CALL thisLS%b%init(2)
        
        ! allocate IPIV
        ALLOCATE(thisLS%IPIV(2))
        
        ! initialize matrix M
        ALLOCATE(DenseSquareMatrixType :: thisLS%M)
        CALL pList%clear()
        CALL pList%add('PL->n',10_SNK)
        CALL pList%add('PL->m',10_SNK)
        CALL thisLS%M%init(pList)
        
      ENDSELECT
      
      !call clear
      CALL thisLS%clear()
      
      !check results
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        IF(thisLS%isInit .OR. thisLS%solverMethod /= -1                 &
          .OR. thisLS%isDecomposed .OR. ALLOCATED(thisLS%A)             &
          .OR. ALLOCATED(thisLS%X) .OR. thisLS%info /= 0                &
          .OR. ALLOCATED(thisLS%b) .OR. ALLOCATED(thisLS%IPIV)          &
          .OR. ALLOCATED(thisLS%M) .OR. thisLS%MPIparallelEnv%isInit()  & 
          .OR. thisLS%OMPparallelEnv%isInit()) THEN
          WRITE(*,*) 'CALL Direct%clear(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      WRITE(*,*) '  Passed: CALL Direct%clear()'
      DEALLOCATE(thisLS)
      
    !test Iterative
      ALLOCATE(LinearSolverType_Iterative :: thisLS)

      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        !first build one by hand to test
        thisLS%isInit=.TRUE.
        thisLS%solverMethod=1
        thisLS%info=2
        thisLS%normType=2
        thisLS%maxIters=2
        thisLS%iters=2
        thisLS%convTol=2._SRK
        thisLS%residual=2._SRK
        thisLS%isDecomposed=.TRUE.
        CALL thisLS%MPIparallelEnv%initialize(0)
        CALL thisLS%OMPparallelEnv%initialize(1)
        
        ! initialize matrix A
        ALLOCATE(DenseSquareMatrixType :: thisLS%A)
        CALL pList%clear()
        CALL pList%add('PL->n',2_SNK)
        CALL pList%add('PL->m',1_SNK)
        CALL thisLS%A%init(pList) !2x2, symmetric
        
        ! initialize vector X
        ALLOCATE(RealVectorType :: thisLS%X)
        CALL thisLS%X%init(2)
        
        ! initialize vector b
        ALLOCATE(RealVectorType :: thisLS%b)
        CALL thisLS%b%init(2)        
        
        ! initialize matrix M
        ALLOCATE(DenseSquareMatrixType :: thisLS%M)
        CALL pList%clear()
        CALL pList%add('PL->n',10_SNK)
        CALL pList%add('PL->m',10_SNK)
        CALL thisLS%M%init(pList)
      ENDSELECT
      
      CALL thisLS%clear()
      
      !check results
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        IF(thisLS%isInit .OR.thisLS%solverMethod == 1                  &
          .OR. ALLOCATED(thisLS%M) .OR. ALLOCATED(thisLS%A)            &
          .OR. ALLOCATED(thisLS%X) .OR. thisLS%info /= 0               &
          .OR. thisLS%MPIparallelEnv%isInit()                          &
          .OR. thisLS%OMPparallelEnv%isInit()                          &
          .OR. thisLS%normType == 2 .OR. thisLS%maxIters == 2          &
          .OR. thisLS%iters == 2 .OR. thisLS%isDecomposed              &
          .OR. thisLS%residual == 2._SRK .OR. thisLS%convTol == 2._SRK) THEN
          WRITE(*,*) 'CALL Iterative%clear() FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      CALL thisLS%clear()
      DEALLOCATE(thisLS)
      WRITE(*,*) '  Passed: CALL Iterative%clear()'
      
    ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInit()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      
   !test Direct
      ALLOCATE(LinearSolverType_Direct :: thisLS)
      
      !Bad input
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',-1_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL thisLS%clear()
      
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL thisLS%clear()
      
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL thisLS%clear()
      
      !first test a correct use case, with timer name
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        IF(.NOT. (thisLS%isInit .AND. thisLS%solverMethod == 1 &
           .AND. thisLS%MPIparallelEnv%isInit()                &
           .AND. thisLS%OMPparallelEnv%isInit()                &
           .AND. thisLS%SolveTime%getTimerName() == 'testTimer')) THEN
          WRITE(*,*) 'CALL Direct%init(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      CALL thisLS%clear()
      
      !first test a correct use case, with no timer name
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Direct)
        IF(.NOT. (thisLS%isInit .AND. thisLS%solverMethod == 1 &
           .AND. thisLS%MPIparallelEnv%isInit()                &
           .AND. thisLS%OMPparallelEnv%isInit()                &
           .AND. thisLS%SolveTime%getTimerName() == 'LinearSolver Timer')) THEN
          WRITE(*,*) 'CALL Direct%init(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      CALL thisLS%clear()
      DEALLOCATE(thisLS)
      WRITE(*,*) '  Passed: CALL Direct%init(...)'
      
    !test Iterative
      ALLOCATE(LinearSolverType_Iterative :: thisLS)
      
      !Bad input
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',-1_SNK)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL thisLS%clear()
      
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL thisLS%clear()
      
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL thisLS%clear()
      
      !first test a correct use case, with timer name
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        IF(.NOT. (thisLS%isInit .AND. thisLS%solverMethod == 1 &
           .AND. thisLS%MPIparallelEnv%isInit() &
           .AND. thisLS%OMPparallelEnv%isInit() &
           .AND. thisLS%SolveTime%getTimerName() == 'testTimer')) THEN
          WRITE(*,*) 'CALL Iterative%init(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      CALL thisLS%clear()
      
      !first test a correct use case, with no timer name
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      SELECTTYPE(thisLS); TYPE IS (LinearSolverType_Iterative)
        IF(.NOT. (thisLS%isInit .AND. thisLS%solverMethod == 1 &
           .AND. thisLS%MPIparallelEnv%isInit() &
           .AND. thisLS%OMPparallelEnv%isInit() &
           .AND. thisLS%SolveTime%getTimerName() == 'LinearSolver Timer')) THEN
          WRITE(*,*) 'CALL Iterative%init(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      CALL thisLS%clear()
      DEALLOCATE(thisLS)
      WRITE(*,*) '  Passed: CALL Iterative%init(...)'

    ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testUpdatedA()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS

    !test Direct
      ALLOCATE(LinearSolverType_Direct :: thisLS)
      
      ! initialize matrix M
      ALLOCATE(DenseSquareMatrixType :: thisLS%M)
      CALL pList%clear()
      CALL pList%add('PL->n',10_SNK)
      CALL pList%add('PL->m',10_SNK)
      CALL thisLS%M%init(pList)
      thisLS%isDecomposed=.TRUE.
      
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',LU)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
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
      
      ! initialize matrix M
      ALLOCATE(DenseSquareMatrixType :: thisLS%M)
      CALL pList%clear()
      CALL pList%add('PL->n',10_SNK)
      CALL pList%add('PL->m',10_SNK)
      CALL thisLS%M%init(pList)
      thisLS%isDecomposed=.TRUE.
      
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      CALL thisLS%updatedA()
      
      !Check
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        IF(thisLS%isDecomposed .OR. ALLOCATED(thisLS%M)) THEN
          WRITE(*,*) 'CALL Iterative%updatedA() FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      WRITE(*,*) '  Passed: CALL Iterative%updatedA()'
      CALL thisLS%clear
      DEALLOCATE(thisLS)
      
    ENDSUBROUTINE testUpdatedA
!
!-------------------------------------------------------------------------------
    SUBROUTINE testDirectSolve()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS

      ALLOCATE(LinearSolverType_Direct :: thisLS)
     
      ! Unspecified check
      
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',DENSESQUARE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',GE)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL thisLS%solve()
      
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',0_SNK)
      CALL thisLS%A%init(pList)
      
      ! initialize vector X
      CALL thisLS%X%init(4)
      CALL thisLS%solve()
      
      ! initialize vector b
      CALL thisLS%b%init(6)
      CALL thisLS%solve()
      
      CALL thisLS%A%clear()
      DEALLOCATE(thisLS%A)
      CALL thisLS%clear()
      
    !Test GE (Dense-Square)
      
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',DENSESQUARE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',GE)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',0_SNK)
      CALL thisLS%A%init(pList)
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
      ! initialize vector b
      CALL thisLS%b%init(3)
      ! set all values to 6
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(6._SRK)
      ENDSELECT
      ! initialize vector X
      CALL thisLS%X%init(3)
      CALL thisLS%solve()
      
      !Check the result
      IF(thisLS%info /= -1 ) THEN
        WRITE(*,*) 'CALL Direct%solve() -GE method FAILED!'
        STOP 666
      ENDIF
      CALL thisLS%A%clear()
      CALL thisLS%clear()
      
      ! Test LU (Dense-Square)
      
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',DENSESQUARE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',LU)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',4_SNK)
      CALL pList%add('PL->m',0_SNK)
      CALL thisLS%A%init(pList)
      
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
      ENDSELECT
      
      ! initialize vector b
      CALL thisLS%b%init(4)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
      CALL b%set(1,10._SRK)
      CALL b%set(2,9._SRK)
      CALL b%set(3,9._SRK)
      CALL b%set(4,4._SRK)
      ENDSELECT
      
      ! initialize vector X
      CALL thisLS%X%init(4)
      
      ! solve
      CALL thisLS%solve()
      
      !Check the result
      SELECTTYPE (X=>thisLS%X); TYPE IS(RealVectorType)
        IF(.NOT.((X%get(1) .APPROXEQ. 1._SRK) &
           .AND. (X%get(2) .APPROXEQ. 1._SRK) &
           .AND. (X%get(3) .APPROXEQ. 1._SRK) &
           .AND. (X%get(4) .APPROXEQ. 1._SRK) &
           .AND. (thisLS%info == 0) )) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      CALL thisLS%A%clear()
      CALL thisLS%clear()
      
      
      ! Test LU (Tridiagonal)

      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',TRIDIAG)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',LU)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      
      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisLS%A%init(pList)
      SELECTTYPE(A => thisLS%A); TYPE IS(TriDiagMatrixType)
        CALL A%set(1,1,4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2,4._SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,3,4._SRK)
      ENDSELECT
      
      ! initialize vector b
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT
      
      ! initialize vector X
      CALL thisLS%X%init(3)
      CALL thisLS%solve()

      !Check M
      !M=[ 4   -1    0]
      !  [-.25 3.75 -1]
      !  [ 0   -.267  3.7333]
      SELECTTYPE(M => thisLS%M); TYPE IS(TriDiagMatrixType)
        IF(.NOT.((M%a(1,1) .APPROXEQ.  0.0_SRK)  &
           .AND. (M%a(1,2) .APPROXEQ. -0.25_SRK) &
           .AND. (M%a(1,3) .APPROXEQ. -0.266666666666666666_SRK) &
           .AND. (M%a(2,1) .APPROXEQ.  0.25_SRK) &
           .AND. (M%a(2,2) .APPROXEQ. (1.0_SRK/3.75_SRK)) &
           .AND. (M%a(2,3) .APPROXEQ. (1.0_SRK/3.7333333333333334_SRK)) &
           .AND. (M%a(3,1) .APPROXEQ. -1.0_SRK)  &
           .AND. (M%a(3,2) .APPROXEQ. -1.0_SRK)  &
           .AND. (M%a(3,3) .APPROXEQ.  0.0_SRK)  &
           .AND. thisLS%isDecomposed)) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      
      !Check X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(.NOT. ((X%get(1) .APPROXEQ. 0.46428571428571430_SRK) &
           .AND.  (X%get(2) .APPROXEQ. 0.85714285714285721_SRK) &
           .AND.  (X%get(3) .APPROXEQ. 0.96428571428571430_SRK) &
           .AND.   thisLS%info == 0)) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT

      
      !Reset X, and solve it again
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        CALL X%set(1.0_SRK)
        CALL thisLS%solve()
        IF(.NOT. ((X%get(1) .APPROXEQ. 0.46428571428571430_SRK) &
           .AND.  (X%get(2) .APPROXEQ. 0.85714285714285721_SRK) &
           .AND.  (X%get(3) .APPROXEQ. 0.96428571428571430_SRK) &
           .AND.   thisLS%isDecomposed)) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      CALL thisLS%A%clear()
      CALL thisLS%clear()
      
      !Sparse matrix, just make sure it could go to CGNR and could
      ! get result, the details will be tested in CGNR
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',GE)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',9_SNK)
      CALL pList%add('PL->m',33_SNK)
      CALL thisLS%A%init(pList)
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
      CALL thisLS%X%init(9)
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        CALL X%set(1.0_SRK)
      ENDSELECT
      
      ! initialize vector b
      CALL thisLS%b%init(9)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT
      
      !solve it
      CALL thisLS%solve()
      IF(thisLS%info /= 0) THEN
        WRITE(*,*) 'CALL Direct%solve() -GE method FAILED!'
        STOP 666
      ENDIF
      CALL thisLS%clear()
      
      !DenseRect matrix, just make sure that it could go to CGNR.
      !The result will be checked later.
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',DENSERECT)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',GE)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      ! A=[1 1]  b=[1]
      !   [1 2]    [2]
      !   [1 3]    [2]
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',2_SNK)
      CALL thisLS%A%init(pList)
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseRectMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,2,1._SRK)
        CALL A%set(2,1,1._SRK)
        CALL A%set(2,2,2._SRK)
        CALL A%set(3,1,1._SRK)
        CALL A%set(3,2,3._SRK)
      ENDSELECT
      
      ! initialize vector b
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,2._SRK)
      ENDSELECT
      
      ! initialize vector X
      CALL thisLS%X%init(2)
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
      CALL X%set(1.0_SRK)
      ENDSELECT
      
      ! solve it
      CALL thisLS%solve()
      IF(thisLS%info /= 0) THEN
        WRITE(*,*) 'CALL Direct%solve() -GE method FAILED!'
        STOP 666
      ENDIF
      CALL thisLS%clear()

    !Test LU (Dense square matrix and tridiagonal matrix
    !other matrix will raise error message)
      !Dense square
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',DENSESQUARE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',LU)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      !Singular case
      !A=[1 0 1 ]  b=[4]    x=[*]
      !  [2 5 -2]    [6]      [*]
      !  [1 0 1 ]    [4]      [*]
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',0_SNK)
      CALL thisLS%A%init(pList)
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
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,4._SRK)
        CALL b%set(2,6._SRK)
        CALL b%set(3,4._SRK)
      ENDSELECT
      
      ! initialize vector X
      CALL thisLS%X%init(3)
      
      ! solve it
      CALL thisLS%solve()
      IF(thisLS%info /= -1) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      CALL thisLS%clear()
      
      ! Normal Case (non-singular)
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',DENSESQUARE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',LU)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      !A=[1 0 1 ]  b=[4]    x=[1]
      !  [2 5 -2]    [6]      [2]
      !  [3 6 9 ]    [42]     [3]
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',0_SNK)
      CALL thisLS%A%init(pList)
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
      
      ! initialize vector b
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1, 4._SRK)
        CALL b%set(2, 6._SRK)
        CALL b%set(3,42._SRK)
      ENDSELECT
      
      ! initialize vector X
      CALL thisLS%X%init(3)
      CALL thisLS%solve()
      
      !Check M
      ! M=[3      6   9]
      !   [1/3    2  -2]
      !   [2/3 -1/2  -9]
      SELECTTYPE(M => thisLS%M); TYPE IS(DenseSquareMatrixType)
        IF(.NOT.((M%A(1,1) .APPROXEQ.   3._SRK) &
           .AND. (M%A(1,2) .APPROXEQ.   6._SRK) &
           .AND. (M%A(1,3) .APPROXEQ.   9._SRK) &
           .AND. (M%A(2,1) .APPROXEQ.   1._SRK/3._SRK) &
           .AND. (M%A(2,2) .APPROXEQ.  -2._SRK) &
           .AND. (M%A(2,3) .APPROXEQ.  -2._SRK) &
           .AND. (M%A(3,1) .APPROXEQ.   2._SRK/3._SRK) &
           .AND. (M%A(3,2) .APPROXEQ. -0.5_SRK) &
           .AND. (M%A(3,3) .APPROXEQ.  -9._SRK) )) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      
      ! check IPIV: IPIV=[3 3 0]
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Direct)
        IF(    thisLS%IPIV(1) /= 3 &
          .OR. thisLS%IPIV(2) /= 3 &
          .OR. thisLS%IPIV(3) /= 0 ) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      
      ! check X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(.NOT.((X%get(1) .APPROXEQ. 1._SRK) &
           .AND. (X%get(2) .APPROXEQ. 2._SRK) &
           .AND. (X%get(3) .APPROXEQ. 3._SRK) &
           .AND. (thisLS%info == 0))) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
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
        IF(.NOT.((thisLS%X%get(1) .APPROXEQ. 3._SRK) &
           .AND. (thisLS%X%get(2) .APPROXEQ. 2._SRK) &
           .AND. (thisLS%X%get(3) .APPROXEQ. 1._SRK) &
           .AND. (thisLS%info == 0))) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      CALL thisLS%clear()

      !Tridiagonal (singular case)
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',TRIDIAG)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',LU)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0  0  0]
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisLS%A%init(pList)
      SELECTTYPE(A => thisLS%A); TYPE IS(TriDiagMatrixType)
        CALL A%set(1,1,4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2,4._SRK)
        CALL A%set(2,3,0._SRK)
        CALL A%set(3,3,0._SRK)
      ENDSELECT
 
      ! initialize vector b
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(1,2._SRK)
        CALL b%set(1,3._SRK)
      ENDSELECT
      
      ! initialize vector X
      CALL thisLS%X%init(3)
      
      CALL thisLS%solve()
      !Check 
      IF(thisLS%info /= -1) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      CALL thisLS%clear()
      
      ! Tridiagonal (non-singular case)
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',TRIDIAG)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',LU)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisLS%A%init(pList)
      SELECTTYPE(A => thisLS%A); TYPE IS(TriDiagMatrixType)
        CALL A%set(1,1,4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2,4._SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,3,4._SRK)
      ENDSELECT
      
      ! initialize vector b
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT
      
      ! initialize vector X
      CALL thisLS%X%init(3)
      CALL thisLS%solve()
      
      !Check M
      !M=[ 4   -1    0]
      !  [-.25 3.75 -1]
      !  [ 0   -.267  3.7333]
      SELECTTYPE(M => thisLS%M); TYPE IS(TriDiagMatrixType)
        IF(.NOT.((M%a(1,1) .APPROXEQ.  0.0_SRK)  &
           .AND. (M%a(1,2) .APPROXEQ. -.25_SRK)  &
           .AND. (M%a(1,3) .APPROXEQ. -0.266666666666666666_SRK) &
           .AND. (M%a(2,1) .APPROXEQ. 0.25_SRK)  &
           .AND. (M%a(2,2) .APPROXEQ. (1.0_SRK/3.75_SRK)) &
           .AND. (M%a(2,3) .APPROXEQ. (1.0_SRK/3.7333333333333334_SRK)) &
           .AND. (M%a(3,1) .APPROXEQ. -1.0_SRK)  &
           .AND. (M%a(3,2) .APPROXEQ. -1.0_SRK)  &
           .AND. (M%a(3,3) .APPROXEQ.  0.0_SRK)  &
           .AND. thisLS%isDecomposed)) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      
      !Check X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(.NOT. ((X%get(1) .APPROXEQ. 0.46428571428571430_SRK) &
           .AND.  (X%get(2) .APPROXEQ. 0.85714285714285721_SRK) &
           .AND.  (X%get(3) .APPROXEQ. 0.96428571428571430_SRK) &
           .AND.   thisLS%info == 0)) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      
      !Reset X, and solve it again
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        CALL X%set(1.0_SRK)
        CALL thisLS%solve()
        IF(.NOT. ((X%get(1) .APPROXEQ. 0.46428571428571430_SRK) &
           .AND.  (X%get(2) .APPROXEQ. 0.85714285714285721_SRK) &
           .AND.  (X%get(3) .APPROXEQ. 0.96428571428571430_SRK) &
           .AND.   thisLS%isDecomposed)) THEN
          WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      CALL thisLS%clear()

      !test solve for TriDiag with a non-diagonally dominant A
      !just make sure we get the output statement.
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',TRIDIAG)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',LU)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      !A=[ O.5  -1    0]
      !  [-1   0.5   -1]
      !  [ 0    -1  0.5]
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisLS%A%init(pList)  !symmetric
      SELECTTYPE(A => thisLS%A); TYPE IS(TriDiagMatrixType)
        CALL A%set(1,1,0.5_SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2,0.5_SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,3,0.5_SRK)
      ENDSELECT
      
      ! initialize vector b
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT
      
      ! initialize vector X
      CALL thisLS%X%init(3)
      
      CALL thisLS%solve()
      CALL thisLS%clear()
      
      !Sparse matrix, just make sure it could go to CGNR and could
      ! get result, the details will be test in CGNR
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',GE)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n', 9_SNK)
      CALL pList%add('PL->m',33_SNK)
      CALL thisLS%A%init(pList)
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
      CALL thisLS%X%init(9)
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        CALL X%set(1.0_SRK)
      ENDSELECT
      
      ! initialize vector b
      CALL thisLS%b%init(9)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT
      
      ! solve it
      CALL thisLS%solve()
      
      IF(thisLS%info /= 0) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      CALL thisLS%clear()
      
      !DenseRect matrix, just make sure that it could go to CGNR.
      !The result will be checked later.
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',DENSERECT)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',LU)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      ! A=[1 1]  b=[1]
      !   [1 2]    [2]
      !   [1 3]    [2]
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',2_SNK)
      CALL thisLS%A%init(pList)
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseRectMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,2,1._SRK)
        CALL A%set(2,1,1._SRK)
        CALL A%set(2,2,2._SRK)
        CALL A%set(3,1,1._SRK)
        CALL A%set(3,2,3._SRK)
      ENDSELECT
      
      ! initialize vector b
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,2._SRK)
      ENDSELECT
      ! initialize vector X
      CALL thisLS%X%init(2)
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        CALL X%set(1.0_SRK)
      ENDSELECT
      !Solve it
      CALL thisLS%solve()
      IF(thisLS%info /= 0) THEN
        WRITE(*,*) 'CALL Direct%solve() -LU method FAILED!'
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL Direct%solve()'
    !end test of direct solver
      CALL thisLS%clear()
      DEALLOCATE(thisLS)
      
    ENDSUBROUTINE testDirectSolve
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIterativeOthers()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      REAL(SRK),POINTER :: thisX(:),thisX2(:)
      REAL(SRK),ALLOCATABLE :: resid_soln(:)
      TYPE(RealVectorType) :: resid
      INTEGER(SIK) :: i

      ALLOCATE(LinearSolverType_Iterative :: thisLS)
      
    !Test setX0
    
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      ! initialize vector X
      CALL thisLS%X%init(2)
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
        IF(.NOT. (ALLOCATED(thisLS%X) .AND. ASSOCIATED(thisX2) &
           .AND.  thisLS%hasX0)) THEN
          WRITE(*,*) 'CALL Iterative%setX0(...) FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      DEALLOCATE(thisX2)
      CALL thisLS%clear()
      WRITE(*,*) '  Passed: CALL Iterative%setX0(...)'
      
    !Test setConv
      !Bad input
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
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
        
        CALL pList%clear()
        CALL pList%add('PL->matrixType',SPARSE)
        CALL pList%add('PL->TPLType',NATIVE)
        CALL pList%add('PL->solverMethod',BICGSTAB)
        CALL pList%add('PL->numberMPI',PE_COMM_SELF)
        CALL pList%add('PL->numberOMP',1_SNK)
        CALL pList%add('PL->timerName','testTimer')
        CALL pList%validate(pList,optListLS)
        CALL thisLS%init(pList)

        ! initialize vectors X and b
        CALL thisLS%b%init(4)
        CALL thisLS%X%init(4)
        CALL thisLS%getResidual(resid)

        CALL resid%init(4)
        CALL thisLS%getResidual(resid)
        
        CALL thisLS%clear()
        CALL resid%clear()
        
      ENDSELECT
      
      !Correct input
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      CALL pList%clear()
      CALL pList%add('PL->n',9_SNK)
      CALL pList%add('PL->m',33_SNK)
      CALL thisLS%A%init(pList)
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
      CALL thisLS%X%init(9)
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        CALL X%set(1._SRK)
      ENDSELECT

      ! initialize vector b
      CALL thisLS%b%init(9)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(10._SRK)
      ENDSELECT
      
      CALL resid%init(9)
      ALLOCATE(resid_soln(9))
      resid_soln=(/-8._SRK,-9._SRK,-8._SRK,-9._SRK,-10._SRK, &
        -9._SRK,-8._SRK,-9._SRK,-8._SRK/)
      
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%getResidual(resid)
      ENDSELECT
      
      DO i=1,resid%n
        IF(.NOT.(resid%get(i) .APPROXEQ. resid_soln(i))) THEN
          WRITE(*,*) 'CALL Iterative%getResidual(...) FAILED!'
          STOP 666
        ENDIF
      ENDDO

      CALL thisLS%clear()
      DEALLOCATE(resid_soln)      
      DEALLOCATE(thisLS)
      WRITE(*,*) '  Passed: CALL Iterative%getResidual(...)'
      
    ENDSUBROUTINE testIterativeOthers

!-------------------------------------------------------------------------------
    SUBROUTINE testIterativeSolve()
      CLASS(LinearSolverType_Base),ALLOCATABLE :: thisLS
      REAL(SRK),ALLOCATABLE :: thisB(:)
      REAL(SRK),POINTER :: thisX(:)
      INTEGER(SIK) :: i
      LOGICAL(SBK) :: match

      ALLOCATE(LinearSolverType_Iterative :: thisLS)

    !With BiCGSTAB
      !The sparse matrix type
      
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',9_SNK)
      CALL pList%add('PL->m',33_SNK)
      CALL thisLS%A%init(pList)
      
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
      
      ! initialize vector X
      CALL thisLS%X%init(9)
      
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setX0(thisX)
      ENDSELECT

      ! build b and set it
      CALL thisLS%b%init(9)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT

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
        SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
          IF(NINT(thisB(i)) /= NINT(10000.0_SRK*X%get(i))) THEN
            match=.FALSE.
            EXIT
          ENDIF
        ENDSELECT
      ENDDO
      IF(.NOT. match) THEN
        WRITE(*,*) 'CALL Iterative%solve() -BICGSTAB FAILED!'
        STOP 666
      ENDIF
      
      DEALLOCATE(thisB)
      CALL thisLS%A%clear()
      CALL thisLS%clear()
      
    !test with A being densesquare
    
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',DENSESQUARE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',9_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisLS%A%init(pList) !9x9, symmetric.
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
      
      !initialize vector X
      CALL thisLS%X%init(9)
      
      !build X0 and set it to 1.0s
      ALLOCATE(thisX(9))
      thisX=1.0_SRK
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT
      
      !build b and set it
      CALL thisLS%b%init(9)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1.0_SRK)
      ENDSELECT
      
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
        SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
          IF(NINT(thisB(i)) /= NINT(10000.0_SRK*X%get(i))) THEN
            match=.FALSE.
            EXIT
          ENDIF
        ENDSELECT
      ENDDO
      IF(.NOT. match) THEN
        WRITE(*,*) 'CALL Iterative%solve() - BiCGSTAB FAILED!'
        STOP 666
      ENDIF
      !test to see how it performs with an already decomposed M
      !reset X to 1.0s
      match=.TRUE.
      DO i=1,SIZE(thisB)
        SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
          CALL X%set(1.0_SRK)
          CALL thisLS%solve()
          IF(NINT(thisB(i)) /= NINT(10000.0_SRK*X%get(i))) THEN
            match=.FALSE.
            EXIT
          ENDIF
        ENDSELECT
      ENDDO
      IF(.NOT. match) THEN
        WRITE(*,*) 'CALL Iterative%solve() - BiCGSTAB FAILED!'
        STOP 666
      ENDIF
      CALL thisLS%A%clear()
      CALL thisLS%clear()
      
      ! TriDiagonal matrix, it will go to LU method
      
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',TRIDIAG)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      ! initialize the matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisLS%A%init(pList)
      SELECTTYPE(A => thisLS%A); TYPE IS(TriDiagMatrixType)
        CALL A%set(1,1, 4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2, 4._SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,3, 4._SRK)
      ENDSELECT
      
      ! initialize the vector b
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT
      
      ! initialize the vector X
      CALL thisLS%X%init(3)

      CALL thisLS%solve()
      
      IF(thisLS%info /= 0) THEN
        WRITE(*,*) 'CALL Iterative%solve() - BiCGSTAB FAILED!'
        STOP 666
      ENDIF
      CALL thisLS%A%clear()
      CALL thisLS%clear()
      DEALLOCATE(thisX)

      !DenseRect matrix
      
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',DENSERECT)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',BICGSTAB)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      ! A=[1 1]  b=[1]
      !   [1 2]    [2]
      !   [1 3]    [2]
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',2_SNK)
      CALL thisLS%A%init(pList)
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseRectMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,2,1._SRK)
        CALL A%set(2,1,1._SRK)
        CALL A%set(2,2,2._SRK)
        CALL A%set(3,1,1._SRK)
        CALL A%set(3,2,3._SRK)
      ENDSELECT
      
      ! initialize vector b
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(2,2._SRK)
      ENDSELECT
      
      ! initialize X0
      ALLOCATE(thisX(2))
      thisX=1.0_SRK
      
      ! initialize vector X
      CALL thisLS%X%init(2)

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
      
      DEALLOCATE(thisX)
      CALL thisLS%clear()
      
      !test CGNR
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',DENSERECT)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',CGNR)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      !underdetermined matrix: solver%info should return -1.
      ! A=[1 1 1]  b= [1]
      !   [1 2 3]     [3]
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',2_SNK)
      CALL pList%add('PL->m',3_SNK)
      CALL thisLS%A%init(pList)
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseRectMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,2,1._SRK)
        CALL A%set(1,3,1._SRK)
        CALL A%set(2,1,1._SRK)
        CALL A%set(2,2,2._SRK)
        CALL A%set(2,3,3._SRK)
      ENDSELECT

      ! initialize vector b
      CALL thisLS%b%init(2)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,3._SRK)
      ENDSELECT
      
      ! initialize X0
      ALLOCATE(thisX(3))
      thisX=1.0_SRK
      
      ! initialize vector X
      CALL thisLS%X%init(3)
      
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
      
      DEALLOCATE(thisX)
      CALL thisLS%clear()
      
      !normal or overdetermined matrix should give answers
      
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',DENSERECT)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',CGNR)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      ! A=[1 1]  b=[1]
      !   [1 2]    [2]
      !   [1 3]    [2]
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',2_SNK)
      CALL thisLS%A%init(pList)
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseRectMatrixType)
        CALL A%set(1,1,1._SRK)
        CALL A%set(1,2,1._SRK)
        CALL A%set(2,1,1._SRK)
        CALL A%set(2,2,2._SRK)
        CALL A%set(3,1,1._SRK)
        CALL A%set(3,2,3._SRK)
      ENDSELECT
      
      ! initialize vector b
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,2._SRK)
      ENDSELECT
 
      ! initialize X0
      ALLOCATE(thisX(2))
      thisX=1.0_SRK
      
      ! initialize vector X
      CALL thisLS%X%init(2)
      
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
      ENDSELECT
      !set iterations and convergence information and
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setConv(2_SIK,1.0E-13_SRK,1000_SIK)
      ENDSELECT
      
      CALL thisLS%solve()
      
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(.NOT.((X%get(1) .APPROXEQ. 2._SRK/3._SRK) &
           .AND. (X%get(2) .APPROXEQ. 0.5_SRK))) THEN
          WRITE(*,*)'CALL Iterative%solve() FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      
      DEALLOCATE(thisX)
      CALL thisLS%clear()
      
      !DenseSquare matrix
      
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',DENSESQUARE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',CGNR)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      
      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisLS%A%init(pList)
      SELECTTYPE(A => thisLS%A); TYPE IS(DenseSquareMatrixType)
        CALL A%set(1,1,4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2,4._SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,3,4._SRK)
      ENDSELECT

      ! initialize vector b
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT
      
      ! initialize vector X
      CALL thisLS%X%init(3)

      !set iterations and convergence information and
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
          CALL thisLS%setConv(2_SIK,1.0E-13_SRK,1000_SIK)
      ENDSELECT
      
      CALL thisLS%solve()
      
      !Check X
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(.NOT. ((X%get(1) .APPROXEQ. 0.46428571428571430_SRK) &
           .AND.  (X%get(2) .APPROXEQ. 0.85714285714285721_SRK) &
           .AND.  (X%get(3) .APPROXEQ. 0.96428571428571430_SRK) &
           .AND.   thisLS%info == 0)) THEN
          WRITE(*,*) 'CALL Iterative%solve() -CGNR method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT

      CALL thisLS%clear()

      !Sparse matrix
      
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',SPARSE)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',CGNR)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',7_SNK)
      CALL thisLS%A%init(pList)
      SELECTTYPE(A => thisLS%A); TYPE IS(SparseMatrixType)
        CALL A%setShape(1,1, 4._SRK)
        CALL A%setShape(1,2,-1._SRK)
        CALL A%setShape(2,1,-1._SRK)
        CALL A%setShape(2,2, 4._SRK)
        CALL A%setShape(2,3,-1._SRK)
        CALL A%setShape(3,2,-1._SRK)
        CALL A%setShape(3,3, 4._SRK)
      ENDSELECT
     
      ! initialize vector b
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT

      ! initialize X0
      ALLOCATE(thisX(3))
      thisX=0._SRK
      
      ! initialize vector X
      CALL thisLS%X%init(3)
      
      !set iterations and convergence information and
      SELECTTYPE(thisLS); TYPE IS(LinearSolverType_Iterative)
        CALL thisLS%setX0(thisX)
        CALL thisLS%setConv(2_SIK,1.0E-13_SRK,1000_SIK)
      ENDSELECT
      
      !Check X
      CALL thisLS%solve()
      SELECTTYPE(X => thisLS%X); TYPE IS(RealVectorType)
        IF(.NOT. ((X%get(1) .APPROXEQ. 0.46428571428571430_SRK) &
           .AND.  (X%get(2) .APPROXEQ. 0.85714285714285721_SRK) &
           .AND.  (X%get(3) .APPROXEQ. 0.96428571428571430_SRK) &
           .AND.   thisLS%info == 0)) THEN
          WRITE(*,*) 'CALL Iterative%solve() -CGNR method FAILED!'
          STOP 666
        ENDIF
      ENDSELECT
      
      DEALLOCATE(thisX)
      CALL thisLS%clear()
      
      !TriDiagonal matrix, it will go to LU
      
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('PL->matrixType',TRIDIAG)
      CALL pList%add('PL->TPLType',NATIVE)
      CALL pList%add('PL->solverMethod',CGNR)
      CALL pList%add('PL->numberMPI',PE_COMM_SELF)
      CALL pList%add('PL->numberOMP',1_SNK)
      CALL pList%add('PL->timerName','testTimer')
      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)

      !A=[ 4 -1  0]
      !  [-1  4 -1]
      !  [ 0 -1  4]
      ! initialize matrix A
      CALL pList%clear()
      CALL pList%add('PL->n',3_SNK)
      CALL pList%add('PL->m',1_SNK)
      CALL thisLS%A%init(pList)
      SELECTTYPE(A => thisLS%A); TYPE IS(TriDiagMatrixType)
        CALL A%set(1,1,4._SRK)
        CALL A%set(1,2,-1._SRK)
        CALL A%set(2,2,4._SRK)
        CALL A%set(2,3,-1._SRK)
        CALL A%set(3,3,4._SRK)
      ENDSELECT
      
      ! initialize vector b
      CALL thisLS%b%init(3)
      SELECTTYPE(b => thisLS%b); TYPE IS(RealVectorType)
        CALL b%set(1,1._SRK)
        CALL b%set(2,2._SRK)
        CALL b%set(3,3._SRK)
      ENDSELECT
      
      ! initialize vector X
      CALL thisLS%X%init(3)

      CALL thisLS%solve()

      IF(thisLS%info /= 0) THEN
        WRITE(*,*) 'CALL Iterative%solve() - BiCGSTAB FAILED!'
        STOP 666
      ENDIF
      
      CALL thisLS%clear()
      DEALLOCATE(thisLS)
      
      WRITE(*,*)'  Passed: CALL Iterative%solver()'
      
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
