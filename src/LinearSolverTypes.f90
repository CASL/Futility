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
!> @brief Module to provide the ability to solve a system of equations
!>
!> DESCRIPTION
!> ??? Make PURE SUBROUTINES WHERE NECESSARY
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref Allocs "Allocs": @copybrief Allocs
!>  - @ref ParallelEnv "ParallelEnv": @copybrief ParallelEnv
!>  - @ref TimerType "TimerType": @copybrief TimerType
!>
!> @par EXAMPLES
!> @code
!> 
!> @endcode
!>
!> @author Adam Nelson, Zhouyu Liu
!>   @date 03/28/2012
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE LinearSolverTypes

  USE IntrType
  USE ExceptionHandler
  USE Allocs
  USE ParallelEnv
  USE Times
  USE MatrixTypes
  USE ParameterLists
  USE BLAS
  IMPLICIT NONE
  
#ifdef HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
#endif

  PRIVATE
!
! List of public members
  PUBLIC :: eLinearSolverType
  PUBLIC :: LinearSolverType_Base
  PUBLIC :: LinearSolverType_Direct
  PUBLIC :: LinearSolverType_Iterative
  PUBLIC :: LNorm
  
  !> Number of direct solver solution methodologies - for error checking
  INTEGER(SIK),PARAMETER :: MAX_DIRECT_SOLVER_METHODS=2
  !> Number of iterative solver solution methodologies - for error checking
  INTEGER(SIK),PARAMETER :: MAX_IT_SOLVER_METHODS=3
  !> set enumeration scheme for TPLs
  INTEGER(SIK),PUBLIC :: PETSC=0,TRILINOS=1,MKL=2,NATIVE=3
  !> set enumeration scheme for iterative solver methods
  INTEGER(SIK),PUBLIC :: BICGSTAB=1,CGNR=2,GMRES=3
  !> set enumeration scheme for direct solver methods
  INTEGER(SIK),PUBLIC :: GE=1,LU=2

  
  !> @brief the base linear solver type
  TYPE,ABSTRACT :: LinearSolverType_Base
    !> Initialization status 
    LOGICAL(SBK) :: isInit=.FALSE.
    !> Integer flag for the solution methodology desired
    INTEGER(SIK) :: solverMethod=-1
    !> Integer flag for the solution methodology desired
    INTEGER(SIK) :: TPLType=-1
    !> Pointer to the distributed memory parallel environment
    TYPE(MPI_EnvType) :: MPIparallelEnv
    !> Pointer to the shared memory parallel environment
    TYPE(OMP_EnvType) :: OMPparallelEnv
    !> Initialization status of A
    LOGICAL(SBK) :: hasA=.FALSE.
    !> Initialization status of b
    LOGICAL(SBK) :: hasB=.FALSE.
    !> Pointer to the MatrixType A
    CLASS(MatrixType),ALLOCATABLE :: A
    !> Right-hand side vector, b
    CLASS(VectorType),ALLOCATABLE :: b
    !> Pointer to solution vector, x
    CLASS(VectorType),ALLOCATABLE :: X
    !> Timer to measure solution time
    TYPE(TimerType) :: SolveTime
    !> Status of the decomposition of A
    LOGICAL(SBK) :: isDecomposed=.FALSE.
    !> Storage of the decomposed Matrix, M
    CLASS(MatrixType),ALLOCATABLE :: M
    !> Return value of the linear solver
    !> 0 : Normal
    !> -1: Unsuccessful exit
    INTEGER(SIK) :: info

#ifdef HAVE_PETSC
    KSP :: ksp
#endif

  !
  !List of Type Bound Procedures
    CONTAINS
      !> Deferred routine for initializing the linear solver system
      PROCEDURE,PASS :: init => init_LinearSolverType_Base
      !> Deferred routine for clearing the linear solver
      PROCEDURE(int_linearsolver_sub),DEFERRED,PASS :: clear
      !> Deferred routine for solving the linear system
      PROCEDURE(int_linearsolver_sub),DEFERRED,PASS :: solve
      !> Routine for updating status of M and isDecomposed when A has been changed
      PROCEDURE,PASS :: updatedA
  ENDTYPE LinearSolverType_Base
  
  !> Explicitly defines the interface for the clear and solve routines
  ABSTRACT INTERFACE
    SUBROUTINE int_linearsolver_sub(solver)
      IMPORT :: LinearSolverType_Base
      CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver
    ENDSUBROUTINE int_linearsolver_sub
  ENDINTERFACE

  !> @brief The extended type for the Direct Linear Solver
  TYPE,EXTENDS(LinearSolverType_Base) :: LinearSolverType_Direct
    !> Storage of row exchanges
    INTEGER(SIK),ALLOCATABLE :: IPIV(:)
!
!List of Type Bound Procedures
    CONTAINS 
      !> @copybrief LinearSolverTypes::clear_LinearSolverType_Direct
      !> @copydetails LinearSolverTypes::clear_LinearSolverType_Direct
      PROCEDURE,PASS :: clear => clear_LinearSolverType_Direct
      !> @copybrief LinearSolverTypes::solve_LinearSolverType_Direct
      !> @copydetails LinearSolverTypes::solve_LinearSolverType_Direct
      PROCEDURE,PASS :: solve => solve_LinearSolverType_Direct
  ENDTYPE LinearSolverType_Direct
  
  !> @brief The extended type for the Iterative Linear Solver
  TYPE,EXTENDS(LinearSolverType_Base) :: LinearSolverType_Iterative
    !> Status of the presence of the initial guess, X0
    LOGICAL(SBK) :: hasX0=.FALSE.
    !> Type of norm to be used for convergence checks
    INTEGER(SIK) :: normType=2_SIK
    !> Maximum number of iterations to perform
    INTEGER(SIK) :: maxIters=1000_SIK
    !> Actual iterations performed
    INTEGER(SIK) :: iters=0_SIK
    !> Tolerance for successful convergence
    REAL(SRK) :: convTol=1.0E-5_SRK
    !> Actual residual converged to
    REAL(SRK) :: residual=0._SRK
!
!List of Type Bound Procedures
    CONTAINS 
      !> @copybrief LinearSolverTypes::clear_LinearSolverType_Iterative
      !> @copydetails LinearSolverTypes::clear_LinearSolverType_Iterative
      PROCEDURE,PASS :: clear => clear_LinearSolverType_Iterative
      !> @copybrief LinearSolverTypes::solve_LinearSolverType_Iterative
      !> @copydetails LinearSolverTypes::solve_LinearSolverType_Iterative
      PROCEDURE,PASS :: solve => solve_LinearSolverType_Iterative
      !> @copybrief LinearSolverTypes::getResidual_LinearSolverType_Iterative
      !> @copydetails LinearSolverTypes::getResidual_LinearSolverType_Iterative
      PROCEDURE,PASS :: getResidual => getResidual_LinearSolverType_Iterative
      !> @copybrief LinearSolverTypes::setConv_LinearSolverType_Iterative
      !> @copydetails LinearSolverTypes::setConv_LinearSolverType_Iterative
      PROCEDURE,PASS :: setConv => setConv_LinearSolverType_Iterative
      !> @copybrief LinearSolverTypes::setX0_LinearSolverType_Iterative
      !> @copydetails LinearSolverTypes::setX0_LinearSolverType_Iterative
      PROCEDURE,PASS :: setX0 => setX0_LinearSolverType_Iterative
  ENDTYPE LinearSolverType_Iterative
  
  !> Exception Handler for use in MatrixTypes
  TYPE(ExceptionHandlerType),POINTER,SAVE :: eLinearSolverType => NULL()
  
  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='LINEARSOLVERTYPES'
!
!===============================================================================
  CONTAINS
  
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
!> @param solverMethod The integer flag for which type of solution scheme to use
!> @param MPIparallelEnv The MPI environment description
!> @param OMPparallelEnv The OMP environment description
!> @param TimerName The name of the timer to be used for querying (optional)
!>
!> This routine initializes the data spaces for the direct linear solver. 
!>
    SUBROUTINE init_LinearSolverType_Base(solver,pList)
      CHARACTER(LEN=*),PARAMETER :: myName='init_LinearSolverType_Base'
      CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver
      TYPE(ParamType),INTENT(IN) :: pList
      
      INTEGER(SIK) :: matrixType, TPLType
      INTEGER(SIK) :: solverMethod
      INTEGER(SIK) :: numberMPI, numberOMP
      CHARACTER(LEN=256) :: timerName
      LOGICAL(SBK) :: localalloc
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
#endif

      !Pull data from the parameter list
      CALL pList%get('matrixType',matrixType)
      CALL pList%get('TPLType',TPLType)
      CALL pList%get('solverMethod',solverMethod)
      CALL pList%get('numberMPI',numberMPI)
      CALL pList%get('numberOMP',numberOMP)
      CALL pList%get('timerName',timerName)
      
      !Initialize parallel environments based on input
      IF (numberMPI>0) CALL solver%MPIparallelEnv%initialize(numberMPI)
      IF (numberOMP>0) CALL solver%OMPparallelEnv%initialize(numberOMP)

      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eLinearSolverType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eLinearSolverType)
      ENDIF
      
      IF(.NOT. solver%isInit) THEN
         
        ! go through solver hierarchy to determine TPLType
        IF (TPLType==0) THEN ! PETSc
#ifdef HAVE_PETSC
          ! switch matrixType if PETSc not available
          IF     (matrixType==0) THEN ! sparse
            matrixType=4 ! PETSc sparse
          ELSEIF (matrixType==2) THEN ! dense
            matrixType=5 ! PETSc dense
          ENDIF
#else
          ! we don't have PETSc, so switch to next option (Trilinos)
          ! maybe throw a notification
          TPLType=1
#endif
        ENDIF
        IF (TPLType==1) THEN ! Trilinos
#ifndef HAVE_TRILINOS
          ! we don't have Trilinos, so switch to next option (MKL)
          TPLType=2
#endif
        ENDIF
        IF (TPLType==2) THEN ! MKL
#ifndef HAVE_MKL
          ! we don't have MKL, so switch to next option (native)
          TPLType=3
#endif
        ENDIF

        ! allocate matrix (A)
        IF (.NOT.ALLOCATED(solver%A)) THEN
          IF     (matrixType==0) THEN ! Sparse
            ALLOCATE(SparseMatrixType :: solver%A)
          ELSEIF (matrixType==1) THEN ! TriDiag 
            ALLOCATE(TriDiagMatrixType :: solver%A)
          ELSEIF (matrixType==2) THEN ! DenseSquare
            ALLOCATE(DenseSquareMatrixType :: solver%A)
          ELSEIF (matrixType==3) THEN ! DenseRect
            ALLOCATE(DenseRectMatrixType :: solver%A)
          ELSEIF (matrixType==4) THEN ! PETSc Sparse
            ALLOCATE(PETScMatrixType :: solver%A)
            SELECTTYPE(A => solver%A); TYPE IS(PETScMatrixType)
              A%SparseDense=0  ! sparse
            ENDSELECT
          ELSEIF (matrixType==5) THEN ! PETSc Dense
            ALLOCATE(PETScMatrixType :: solver%A)
            SELECTTYPE(A => solver%A); TYPE IS(PETScMatrixType)
              A%SparseDense=1  ! dense
            ENDSELECT
          ENDIF
        ELSE
          ! throw exception
        ENDIF
        ! allocate vectors (X and b)
        IF (matrixType==4 .OR. matrixType==5) THEN
          ALLOCATE(PETScVectorType :: solver%X)
          ALLOCATE(PETScVectorType :: solver%b)
        ELSE
          ALLOCATE(RealVectorType :: solver%X)
          ALLOCATE(RealVectorType :: solver%b)
        ENDIF
      
        ! direct solver
        SELECTTYPE(solver); TYPE IS(LinearSolverType_Direct)
          IF((solverMethod > 0) .AND. &
             (solverMethod <= MAX_DIRECT_SOLVER_METHODS)) THEN         
            !assign values to solver
            CALL solver%SolveTime%setTimerName(timerName)   
            solver%solverMethod=solverMethod
            solver%TPLType=TPLType
            solver%isInit=.TRUE.
          ELSE
            CALL eLinearSolverType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - invalid value of solverMethod')
          ENDIF
        ENDSELECT
        
        ! iterative solver
        SELECTTYPE(solver); TYPE IS(LinearSolverType_Iterative)
          IF((solverMethod > 0) .AND. &
             (solverMethod <= MAX_IT_SOLVER_METHODS)) THEN         
              
#ifdef HAVE_PETSC
            IF (matrixType==4 .OR. matrixType==5) THEN ! PETSc
                !create and initialize KSP
                CALL KSPCreate(MPI_COMM_WORLD,solver%ksp,ierr)
                CALL KSPSetOperators(solver%ksp,solver%A,solver%A,DIFFERENT_NONZERO_PATTERN,ierr)
                CALL KSPSetFromOptions(solver%ksp,ierr)
                
                !set iterative solver type
                SELECTCASE(solverMethod)
                  CASE(1) ! BCGS
                    CALL KSPSetType(solver%ksp,KSPBCGS,ierr)
                  CASE(2) ! CGNR
                    CALL KSPSetType(solver%ksp,KSPCGNE,ierr)
                  CASE(3) ! GMRES
                    CALL KSPSetType(solver%ksp,KSPGMRES,ierr)
                ENDSELECT
            ENDIF
#endif
            !assign values to solver
            CALL solver%SolveTime%setTimerName(timerName)   
            solver%solverMethod=solverMethod
            solver%TPLType=TPLType
            solver%isInit=.TRUE.
          ELSE
            CALL eLinearSolverType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - invalid value of solverMethod')
          ENDIF
        ENDSELECT
      ELSE
        CALL eLinearSolverType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - LinearSolverType already initialized')
      ENDIF
      
      IF(localalloc) DEALLOCATE(eLinearSolverType)
      
    ENDSUBROUTINE init_LinearSolverType_Base
!
!-------------------------------------------------------------------------------
!> @brief Clears the Direct Linear Solver Type
!> @param solver The linear solver to act on
!>
!> This routine clears the data spaces for the direct linear solver. 
!>
    SUBROUTINE clear_LinearSolverType_Direct(solver)
      CLASS(LinearSolverType_Direct),INTENT(INOUT) :: solver

      solver%isInit=.FALSE.
      solver%solverMethod=-1
      solver%hasA=.FALSE.
      solver%hasB=.FALSE.
      solver%info=0
      CALL solver%MPIparallelEnv%clear()
      CALL solver%OMPparallelEnv%clear()
      IF(ALLOCATED(solver%A)) DEALLOCATE(solver%A)
      IF(ALLOCATED(solver%X)) DEALLOCATE(solver%X)
      IF(ALLOCATED(solver%b)) DEALLOCATE(solver%b)
      IF(ALLOCATED(solver%IPIV)) CALL demallocA(solver%IPIV)
      IF(ALLOCATED(solver%M)) THEN
        CALL solver%M%clear()
        DEALLOCATE(solver%M)
      ENDIF

      !No timer clear function-just call toc instead
      CALL solver%SolveTime%toc()
      solver%isDecomposed=.FALSE.
    ENDSUBROUTINE clear_LinearSolverType_Direct
!
!-------------------------------------------------------------------------------
!> @brief Clears the Iterative Linear Solver Type
!> @param solver The linear solver to act on
!>
!> This routine clears the data spaces for the iterative linear solver. 
!>
    SUBROUTINE clear_LinearSolverType_Iterative(solver)
      CLASS(LinearSolverType_Iterative),INTENT(INOUT) :: solver
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
#endif

      solver%isInit=.FALSE.
      solver%solverMethod=-1
      solver%hasA=.FALSE.
      solver%hasB=.FALSE.
      solver%info=0
      CALL solver%MPIparallelEnv%clear()
      CALL solver%OMPparallelEnv%clear()
      IF(ALLOCATED(solver%A)) DEALLOCATE(solver%A)
      IF(ALLOCATED(solver%X)) DEALLOCATE(solver%X)
      IF(ALLOCATED(solver%b)) DEALLOCATE(solver%b)
      IF(ALLOCATED(solver%M)) THEN
        CALL solver%M%clear()
        DEALLOCATE(solver%M)
      ENDIF
      
#ifdef HAVE_PETSC
      CALL KSPDestroy(solver%ksp,ierr)
#endif 

      !No timer clear function-just call toc instead
      CALL solver%SolveTime%toc()
      solver%isDecomposed=.FALSE.
      solver%hasX0=.FALSE.
      solver%normType=-1
      solver%maxIters=-1
      solver%iters=0
      solver%convTol=0._SRK
      solver%residual=0._SRK
    ENDSUBROUTINE clear_LinearSolverType_Iterative
!
!-------------------------------------------------------------------------------
!> @brief Tells the LinearSystem that A has been updated outside of solver
!> @param solver The linear solver to act on
!>
!> This routine tells the LinearSystem that A has been updated outside of solver
!>
    SUBROUTINE updatedA(solver)
      CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver
      solver%isDecomposed=.FALSE.
      SELECTTYPE(solver)
        TYPE IS(LinearSolverType_Direct)
          IF(ALLOCATED(solver%IPIV)) CALL demallocA(solver%IPIV)
      ENDSELECT
      IF(ALLOCATED(solver%M)) THEN
        CALL solver%M%clear()
        DEALLOCATE(solver%M)
      ENDIF
    ENDSUBROUTINE updatedA
!
!-------------------------------------------------------------------------------
!> @brief Solves the Linear System
!> @param solver The linear solver to act on
!>
!> This routine solves the linear system directly
!>
    SUBROUTINE solve_LinearSolverType_Direct(solver)
      CHARACTER(LEN=*),PARAMETER :: myName='solve_LinearSolverType_Direct'
      CLASS(LinearSolverType_Direct),INTENT(INOUT) :: solver
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eLinearSolverType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eLinearSolverType)
      ENDIF
      CALL solve_checkInput(solver)
      IF(solver%info == 0) THEN
        solver%info=-1
        CALL solver%SolveTime%tic()
        SELECTCASE(solver%solverMethod)
          CASE(1) !GE
            SELECTTYPE(A => solver%A)
              TYPE IS(DenseSquareMatrixType)
                CALL solveGE_DenseSquare(solver)

              TYPE IS(TriDiagMatrixType)
                IF(.NOT.solver%isDecomposed) &
                  CALL DecomposePLU_TriDiag(solver)
                CALL solvePLU_TriDiag(solver)

              CLASS DEFAULT
                !Should not use direct method, go to CGNR
                CALL solveCGNR(solver)
                IF(solver%info == 0) &
                  CALL eLinearSolverType%raiseWarning(modName//'::'// &
                    myName//'- GE method for dense rectangular system '// &
                      'and sparse system is not implemented, CGNR method '// &
                        'is used instead.')
            ENDSELECT
          CASE(2) !LU
            SELECTTYPE(A => solver%A)
              TYPE IS(DenseSquareMatrixType)
                IF(.NOT. solver%isDecomposed) &
                  CALL DecomposePLU_DenseSquare(solver)
                CALL solvePLU_DenseSquare(solver)

              TYPE IS(TriDiagMatrixType)
                IF(.NOT.solver%isDecomposed) &
                  CALL DecomposePLU_TriDiag(solver)
                CALL solvePLU_TriDiag(solver)

              CLASS DEFAULT
                !Should not use direct method, go to CGNR
                CALL solveCGNR(solver)
                IF(solver%info == 0) &
                  CALL eLinearSolverType%raiseWarning(modName//'::'// &
                    myName//'- LU method for dense rectangular system '// &
                      'and sparse system is not implemented, CGNR method '// &
                        'is used instead.')
            ENDSELECT
        ENDSELECT
        CALL solver%SolveTime%toc()
      ENDIF
      IF(localalloc) DEALLOCATE(eLinearSolverType)
    ENDSUBROUTINE solve_LinearSolverType_Direct
!
!-------------------------------------------------------------------------------
!> @brief Solves the Linear System
!> @param solver The linear solver to act on
!>
!> This routine solves the linear system iteratively
!>
    SUBROUTINE solve_LinearSolverType_Iterative(solver)
      CHARACTER(LEN=*),PARAMETER :: myName='solve_LinearSolverType_Iterative'
      CLASS(LinearSolverType_Iterative),INTENT(INOUT) :: solver
      LOGICAL(SBK) :: localalloc
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
#endif

      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eLinearSolverType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eLinearSolverType)
      ENDIF
      CALL solve_checkInput(solver)
      IF(solver%info == 0) THEN
        IF(.NOT. solver%hasX0) THEN
          SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
            CALL X%set(1.0_SRK)
          ENDSELECT
          solver%hasX0=.TRUE.
          CALL eLinearSolverType%raiseWarning(modName//'::'// &
            myName//'- Initial X0 is set to 1.')
        ENDIF
        CALL solver%SolveTime%tic()
        solver%info=-1
        SELECTCASE(solver%solverMethod)
          CASE(1) !BiCGSTAB
            !need two type structures to deal with DenseRectMatrixType
            SELECTTYPE(A=>solver%A)
              TYPE IS(DenseSquareMatrixType)
                IF(.NOT. solver%isDecomposed) &
                  CALL DecomposeBiCGSTAB_DenseSquare(solver)
                CALL solveBiCGSTAB(solver)

              TYPE IS(SparseMatrixType)
                CALL solveBiCGSTAB(solver)

              TYPE IS(TriDiagMatrixType)
                !If the coefficient matrix is a tridiagonal matrix, PLU method
                !will be used instead.
                IF(.NOT. solver%isDecomposed) CALL DecomposePLU_TriDiag(solver)
                CALL solvePLU_TriDiag(solver)

                IF(solver%info == 0) &
                  CALL eLinearSolverType%raiseWarning(modName//'::'// &
                    myName//'- BiCGSTAB method for tridiagonal system '// &
                      'is not implemented, GE method is used instead.')

              TYPE IS(DenseRectMatrixType)
                !If the coefficient matrix is a rectangular matrix, CGNR method
                !will be used instead.
                CALL solveCGNR(solver)

                IF(solver%info == 0) &
                  CALL eLinearSolverType%raiseWarning(modName//'::'// &
                    myName//'- BiCGSTAB method for dense rectangular system '// &
                      'is not implemented, CGNR method is used instead.')

#ifdef HAVE_PETSC                      
              TYPE IS(PETScMatrixType)
                ! assemble matrix if necessary
                IF (.NOT.(A%isAssembled)) THEN
                  CALL MatAssemblyBegin(A,ierr)
                  CALL MatAssemblyEnd(A,ierr)
                  A%isAssembled=.FALSE.
                ENDIF
                
                ! solve
                CALL KSPSolve(solver%ksp,solver%b,solver%x,ierr)
#endif
                
            ENDSELECT
          CASE(2) !CGNR
            SELECTTYPE(A=>solver%A)
              TYPE IS(TriDiagMatrixType)
                !If the coefficient matrix is tridiagonal PLU method will be
                !used instead.
                IF(.NOT.solver%isDecomposed) &
                  CALL DecomposePLU_TriDiag(solver)
                CALL solvePLU_TriDiag(solver)

                IF(solver%info == 0) &
                  CALL eLinearSolverType%raiseWarning(modName//'::'// &
                  myName//'- CGNR method for tridiagonal system '// &
                    'is not implemented, PLU method is used instead.')
              TYPE IS(SparseMatrixType)
                CALL solveBiCGSTAB(solver)
                IF(solver%info == 0) &
                  CALL eLinearSolverType%raiseWarning(modName//'::'// &
                  myName//'- CGNR method for sparse system '// &
                    'is not implemented, BiCGSTAB method is used instead.')
#ifdef HAVE_PETSC                    
              TYPE IS(PETScMatrixType)
                ! assemble matrix if necessary
                IF (.NOT.(A%isAssembled)) THEN
                  CALL MatAssemblyBegin(A,ierr)
                  CALL MatAssemblyEnd(A,ierr)
                  A%isAssembled=.FALSE.
                ENDIF
                
                ! solve
                CALL KSPSolve(solver%ksp,solver%b,solver%x,ierr)
#endif
              CLASS DEFAULT
                CALL solveCGNR(solver)
            
            ENDSELECT
            
          CASE(3) !GMRES
            SELECTTYPE(A=>solver%A)
#ifdef HAVE_PETSC                    
              TYPE IS(PETScMatrixType)
                ! assemble matrix if necessary
                IF (.NOT.(A%isAssembled)) THEN
                  CALL MatAssemblyBegin(A,ierr)
                  CALL MatAssemblyEnd(A,ierr)
                  A%isAssembled=.FALSE.
                ENDIF
                
                ! solve
                CALL KSPSolve(solver%ksp,solver%b,solver%x,ierr)
#endif  
            ENDSELECT
        ENDSELECT
        CALL solver%SolveTime%toc()
      ENDIF
      IF(localalloc) DEALLOCATE(eLinearSolverType)
    ENDSUBROUTINE solve_LinearSolverType_Iterative
!
!-------------------------------------------------------------------------------
!> @brief Check the information before solving.
!> @param solver The linear solver to act on
!>
!> The matrix and vectors are operated outside of linear solver object, so they
!> need to checked before solving. This subroutine checks these information, and
!> it will be used for both direct solve subroutine and iterative subroutine.
    SUBROUTINE solve_checkInput(solver)
      CHARACTER(LEN=*),PARAMETER :: myName='solve_checkInput'
      CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver

      LOGICAL(SBK) :: localalloc
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eLinearSolverType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eLinearSolverType)
      ENDIF
      solver%info=-1
      IF(solver%isInit) THEN
        IF(ALLOCATED(solver%A)) THEN
          IF(ALLOCATED(solver%X)) THEN
            IF(ALLOCATED(solver%b)) THEN
              SELECTTYPE(A=>solver%A)
                TYPE IS(DenseRectMatrixType)
                  IF(A%n /= solver%b%n .OR. A%m /= solver%X%n &
                    .OR. A%n < 1 .OR. A%m < 1) THEN
                    CALL eLinearSolverType%raiseError(ModName//'::'//myName// &
                      '  - The size of the matrix and vector do not comform!')
                  ELSE
                    solver%info=0
                  ENDIF
                CLASS DEFAULT
                  IF(A%n /= solver%b%n .OR. A%n /= solver%X%n &
                    .OR. A%n < 1) THEN
                    CALL eLinearSolverType%raiseError(ModName//'::'//myName// &
                      '  - The size of the matrix and vector do not comform!')
                  ELSE
                    solver%info=0
                  ENDIF
              ENDSELECT
            ELSE
              CALL eLinearSolverType%raiseError(ModName//'::'//myName// &
                '  - The right hand side has not been set!')
            ENDIF
          ELSE
            CALL eLinearSolverType%raiseError(ModName//'::'//myName// &
              '  - The unknows X has not been associated!')
          ENDIF
        ELSE
          CALL eLinearSolverType%raiseError(ModName//'::'//myName// &
            '  - The matrix A has not been associated!')
        ENDIF
      ELSE
        CALL eLinearSolverType%raiseError(ModName//'::'//myName// &
          '  - Linear solver object has not been initialized!')
      ENDIF
      IF(localalloc) DEALLOCATE(eLinearSolverType)
    ENDSUBROUTINE solve_checkInput
!
!-------------------------------------------------------------------------------
!> @brief Sets the initial guess for X0 for the iterative solver
!> @param solver The linear solver to act on
!> @param X0 A vector which contains the initial guess
!>
!> This subroutine sets the initial guess for the iterative solver. The 
!> vector X0 is passed as an argument to this routine, and the X pointer
!> is then set to point to it. 
!>
    SUBROUTINE setX0_LinearSolverType_Iterative(solver,X0)
      CLASS(LinearSolverType_Iterative),INTENT(INOUT) :: solver
      REAL(SRK),POINTER,INTENT(IN) :: X0(:)

      IF(solver%isInit) THEN
        SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
          X%b=X0
        ENDSELECT
        solver%hasX0=.TRUE.
      ENDIF
    ENDSUBROUTINE setX0_LinearSolverType_Iterative
!
!-------------------------------------------------------------------------------
!> @brief Sets the convergence criteria for the iterative solver
!> @param solver The linear solver to act on
!> @param normType An integer representing the convergence check norm
!> @param convTol A value representing the convergence behavior
!> @param maxIters The maximum number of iterations to perform
!>
!> This subroutine sets the convergence criterion for the iterative solver. 
!>
    SUBROUTINE setConv_LinearSolverType_Iterative(solver,normType_in,convTol_in, &
                                                  maxIters_in)
      CHARACTER(LEN=*),PARAMETER :: myName='setConv_LinearSolverType_Iterative'
      CLASS(LinearSolverType_Iterative),INTENT(INOUT) :: solver
      INTEGER(SIK),INTENT(IN) :: normType_in
      REAL(SRK),INTENT(IN) :: convTol_in
      INTEGER(SIK),INTENT(IN) :: maxIters_in
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      PetscInt  :: maxits
      PetscReal :: rtol,abstol,dtol=PETSC_DEFAULT_DOUBLE_PRECISION
#endif

      INTEGER(SIK) :: normType,maxIters
      REAL(SRK) :: convTol
      LOGICAL(SBK) :: localalloc
      
#ifdef HAVE_PETSC
      ! set variables
      maxits=maxIters_in
      rtol=convTol_in
      abstol=convTol_in
#endif

      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eLinearSolverType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eLinearSolverType)
      ENDIF
      
      !Input check
      normType=normType_in
      convTol=convTol_in
      maxIters=maxIters_in
      IF(normType <= -2) THEN
        CALL eLinearSolverType%raiseWarning(modName//'::'// &
          myName//' - Incorrect input, normType should not be less '// &
            'than -1. Default value is used!')
        normType=2
      ENDIF
      IF(convTol < 0._SRK .OR. convTol >= 1._SRK) THEN
        CALL eLinearSolverType%raiseWarning(modName//'::'// &
          myName//' - Incorrect input, convTol should be in '// &
            'the range of (0, 1). Default value is used!')
        convTol=0.001_SRK
      ENDIF
      IF(maxIters <= 1) THEN
        CALL eLinearSolverType%raiseWarning(modName//'::'// &
          myName//' - Incorrect input, maxIters should not be less '// &
            'than 1. Default value is used!')
        maxIters=1000
      ENDIF
      IF(solver%isInit) THEN
        solver%normType=normType
        solver%convTol=convTol
        solver%maxIters=maxIters
#ifdef HAVE_PETSC
        IF (solver%TPLType == PETSC) THEN
          CALL KSPSetTolerances(solver%ksp,rtol,abstol,dtol,maxits,ierr)
        ENDIF
#endif
      ENDIF
      IF(localalloc) DEALLOCATE(eLinearSolverType)
    ENDSUBROUTINE setConv_LinearSolverType_Iterative
!
!-------------------------------------------------------------------------------
!> @brief Gets the residual for the iterative solver
!> @param solver The linear solver to act on
!> @param resid A vector which will contain the residual
!>
!> This subroutine gets the residual after completion of the iterative solver 
!>   
    SUBROUTINE getResidual_LinearSolverType_Iterative(solver,resid)
      CLASS(LinearSolverType_Iterative),INTENT(INOUT) :: solver
      TYPE(RealVectorType),INTENT(OUT) :: resid
      !input check
      IF(solver%isInit .AND. ALLOCATED(solver%b) .AND. ALLOCATED(solver%A) &
        .AND. ALLOCATED(solver%X) .AND. resid%n > 0) THEN
        !Written assuming A is not decomposed.  Which is accurate, the correct solve
        !function will contain the decomposed A.
        IF(resid%n == solver%b%n) THEN
#ifdef HAVE_MKL
          !not yet implemented
          SELECTTYPE(b => solver%b); TYPE IS(RealVectorType)
            resid%b=-b%b
          ENDSELECT
          CALL BLAS_matvec(THISMATRIX=solver%A,X=solver%X,Y=resid)
#else
          !perform calculations using the BLAS system (intrinsic to MPACT or TPL, defined by #HAVE_BLAS)
          SELECTTYPE(b => solver%b); TYPE IS(RealVectorType)
            resid%b=-b%b
          ENDSELECT
          CALL BLAS_matvec(THISMATRIX=solver%A,X=solver%X,Y=resid)
#endif
        ENDIF
      ENDIF
    ENDSUBROUTINE getResidual_LinearSolverType_Iterative
!
!-------------------------------------------------------------------------------
!> @brief Decompose Dense  Linear System using the BiCGSTAB method
!> @param solver The linear solver to act on
!>
!> This subroutine solves the Iterative Linear System using the BiCGSTAB method
!>
    SUBROUTINE DecomposeBiCGSTAB_DenseSquare(solver)
      CLASS(LinearSolverType_Iterative),INTENT(INOUT) :: solver
      TYPE(ParamType) :: pList

      INTEGER(SIK) :: i
      solver%isDecomposed=.FALSE.
      IF(ALLOCATED(solver%M)) THEN
        CALL solver%M%clear()
        DEALLOCATE(solver%M)
      ENDIF
      ALLOCATE(DenseSquareMatrixType :: solver%M)
      CALL pList%add('PL->n',solver%A%n)
      CALL pList%add('PL->m',0_SNK)
      CALL solver%M%init(pList)
      DO i=1,solver%M%n
        CALL solver%M%set(i,i,1.0_SRK)
      ENDDO
      solver%isDecomposed=.TRUE.
    ENDSUBROUTINE DecomposeBiCGSTAB_DenseSquare
!
!-------------------------------------------------------------------------------
!> @brief Solves the Iterative Linear System using the BiCGSTAB method
!> @param solver The linear solver to act on
!>
!> This subroutine solves the Iterative Linear System using the BiCGSTAB method
!>
    SUBROUTINE solveBiCGSTAB(solver)
      CLASS(LinearSolverType_Iterative),INTENT(INOUT) :: solver

      REAL(SRK),PARAMETER :: one=1.0_SRK,zero=0.0_SRK
      REAL(SRK):: calpha,crho,comega,crhod,cbeta,pts,ptt
      TYPE(RealVectorType) :: vr,vr0,vs,vv,vp,vy,vz,vt
      INTEGER(SIK) :: i,n,iterations

      CALL vr%init(solver%A%n)
      CALL vr0%init(solver%A%n)
      CALL vs%init(solver%A%n)
      CALL vv%init(solver%A%n)
      CALL vp%init(solver%A%n)
      CALL vy%init(solver%A%n)
      CALL vz%init(solver%A%n)
      CALL vt%init(solver%A%n)
      
      n=solver%A%n
      calpha=one
      crho=one
      comega=one
      CALL vp%set(zero)
      ! temporarily USE p to store A*x to compute p
      CALL BLAS_matvec(THISMATRIX=solver%A,X=solver%X,Y=vp)
      ! r and r0
      SELECTTYPE(b => solver%b); TYPE IS(RealVectorType)
        vr0%b=b%b-vp%b
      ENDSELECT
      vr%b=vr0%b
      CALL vp%set(zero)
      CALL vv%set(zero)
      
      !get L_norm
      CALL LNorm(vr0%b,solver%normType,solver%residual)
      !Iterate on solution
      DO iterations=1_SIK,solver%maxIters
        crhod=crho
        crho=BLAS_dot(vr0,vr,n)
        cbeta=crho*calpha/(crhod*comega)
        vp%b=vr%b+cbeta*(vp%b-comega*vv%b)

        ! y_j=inv(M)*p_j, store in y
        CALL vy%set(zero)
        IF(ALLOCATED(solver%M)) THEN
          SELECTTYPE(M => solver%M); TYPE IS(DenseSquareMatrixType)
            CALL MinvMult_dense(M,vp%b,vy%b)
          ENDSELECT
        ELSE
          vy%b=vp%b
        ENDIF
        CALL vv%set(zero)
        CALL BLAS_matvec(THISMATRIX=solver%A,X=vy,Y=vv)
        calpha=crho/BLAS_dot(vr0,vv,n)

        vs%b=vr%b-calpha*vv%b
        CALL vz%set(zero)
        IF(ALLOCATED(solver%M)) THEN
          SELECTTYPE(M => solver%M); TYPE IS(DenseSquareMatrixType)
            CALL MinvMult_dense(M,vs%b,vz%b)
          ENDSELECT
        ELSE
          vz%b=vs%b
        ENDIF
        CALL vt%set(zero)
        CALL BLAS_matvec(THISMATRIX=solver%A,X=vz,Y=vt)
        comega=BLAS_dot(vs,vt)/BLAS_dot(vt,vt)
        SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
          X%b=X%b+calpha*vy%b+comega*vz%b
        ENDSELECT
        vr%b=vs%b-comega*vt%b
        !get L_norm
        CALL LNorm(vr%b,solver%normType,solver%residual)
        !check convergence
        IF(solver%residual<=solver%convTol) EXIT
      ENDDO
      solver%iters=iterations
      solver%info=0
    ENDSUBROUTINE solveBiCGSTAB

!-------------------------------------------------------------------------------
!> @brief Factorizes a sparse solver%A with ILU method and stores this in
!>  solver%M
!> @param solver The linear solver object
!>
!> This subroutine factorizes A with ILU method and stores the result in solver%M
!> 
    SUBROUTINE DecomposeILU_Sparse(solver)
      CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver

      INTEGER(SIK) :: i,j,ik,k,kk,ij,kj,j2
      INTEGER(SIK) :: uptr(solver%A%n)
      REAL(SRK) :: m_val

      IF(ALLOCATED(solver%M)) THEN
        CALL solver%M%clear()
        DEALLOCATE(solver%M)
      ENDIF
      ALLOCATE(SparseMatrixType :: solver%M)

      SELECTTYPE(M => solver%M); TYPE IS(SparseMatrixType)
        SELECTTYPE(A => solver%A); TYPE IS(SparseMatrixType)
          M=A
        ENDSELECT
      ENDSELECT

      solver%info=-1
      SELECTTYPE(M => solver%M); TYPE IS(SparseMatrixType)
        ! Find the indeces of M containing the diagonal terms
        DO i=1,M%n
          DO j=M%ia(i),M%ia(i+1)-1
            IF(i==M%ja(j)) THEN
              uptr(i)=j
              EXIT
            ENDIF
          ENDDO
        ENDDO
        !Compute the ILU of solver%M
        DO i=2,M%n
          DO ik=M%ia(i),uptr(i)-1
            k=M%ja(ik)
            kk=uptr(k)
            IF(kk == 0 .OR.(M%a(kk) .APPROXEQ. 0._SRK)) THEN
              CALL M%clear()
              DEALLOCATE(solver%M)
              RETURN
            ENDIF
            m_val=M%a(ik)/M%a(kk)
            M%a(ik)=m_val
            DO ij=ik+1,M%ia(i+1)-1
              j=M%ja(ij)
              kj=0_SIK
              DO j2=M%ia(k),M%ia(k+1)-1
                IF(j==M%ja(j2)) THEN
                  kj=j2
                  EXIT
                ENDIF
              ENDDO
              IF(kj /= 0_SIK) M%a(ij)=M%a(ij)-m_val*M%a(kj)
            ENDDO
          ENDDO
        ENDDO
        solver%info=0
        solver%isDecomposed=.TRUE.
      ENDSELECT
    ENDSUBROUTINE DecomposeILU_Sparse

!-------------------------------------------------------------------------------
!> @brief Factorizes a TriDiag solver%A with the PLU method and stores the
!> result in solver%M.
!> @param solver The LinearSolverType object
!>
!> This subroutine factorizes the TriDiagnal matrix solver%A is with PLU method 
!> and stores the result in solver%M. If the matrix is not diagonally dominant,
!> the solution might be not accurate; and a warnning will be given.
!> 
    SUBROUTINE DecomposePLU_TriDiag(solver)
      CHARACTER(LEN=*),PARAMETER :: myName='decomposePLU_TriDiag'
      CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver
      TYPE(ParamType) :: pList

      INTEGER(SIK) :: i
      REAL(SRK) :: t
      LOGICAL(SBK) :: diagDom,localalloc

      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eLinearSolverType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eLinearSolverType)
      ENDIF

      !Check if M is allocated.
      IF(ALLOCATED(solver%M)) THEN
        CALL solver%M%clear()
        DEALLOCATE(solver%M)
      ENDIF
      ALLOCATE(TriDiagMatrixType :: solver%M)

      solver%info=-1
      IF(solver%A%isInit) THEN
        SELECTTYPE(A => solver%A); TYPE IS(TriDiagMatrixType)
          !Test for diagonal dominance
          diagDom=.TRUE.
          IF(ABS(A%a(2,1))<ABS(A%a(3,1))) diagDom=.FALSE.
          DO i=2,A%n-1
            IF(ABS(A%a(2,i))<(ABS(A%a(1,i))+ABS(A%a(3,i)))) &
              diagDom=.FALSE.; EXIT
          ENDDO
          IF(ABS(A%a(2,A%n))<ABS(A%a(1,A%n))) diagDom=.FALSE.

          !If the first diagonal coefficient is zero, return
          IF(A%a(2,1) .APPROXEQ. 0._SRK) THEN
            CALL solver%M%clear()
            DEALLOCATE(solver%M)
            RETURN
          ENDIF

          CALL pList%add('PL->n',A%n)
          CALL pList%add('PL->m',0_SNK)
          CALL solver%M%init(pList)
          SELECTTYPE(M => solver%M); TYPE IS(TriDiagMatrixType)
            M%a(2,1)=1.0_SRK/A%a(2,1)
            DO i=1,A%n-1
              M%a(1,i+1)=A%a(1,i+1)*M%a(2,i)
              M%a(3,i)=A%a(3,i)
              t=A%a(2,i+1)-M%a(1,i+1)*M%a(3,i)
              !If failed, return.
              IF(t .APPROXEQ. 0._SRK) THEN
                CALL M%clear()
                DEALLOCATE(solver%M)
                RETURN
              ENDIF
              M%a(2,i+1)=1.0_SRK/t
            ENDDO
            solver%info=0
            solver%isDecomposed=.TRUE.
          ENDSELECT

          !Give the warning
          IF(.NOT. diagDom) CALL eLinearSolverType%raiseWarning(modName// &
            '::'//myName//'- Tri-diagonal Matrix not diagonally dominant, '// &
              'solution might be not accurate')
        ENDSELECT
      ENDIF
      IF(localalloc) DEALLOCATE(eLinearSolverType)
    ENDSUBROUTINE DecomposePLU_TriDiag


!-------------------------------------------------------------------------------
!> @brief Solves a sparse system using forward and backward substitution, given M
!> @param M The resultant ILU factorization of A, inverted.
!> @param b the RHS vector
!> @param x the output vector, x=inv(M)*b
!>
!> This subroutine applies the inverse of a matrix M to a vector b and returns x.
!> It assumes that M is stored in the Compressed Sparse Row (CSR) format, and
!> that M is actually stored as LU.
!> 
    SUBROUTINE MinvMult_Sparse(M,b,x)       
        TYPE(SparseMatrixType),INTENT(IN) :: M
        REAL(SRK),INTENT(IN) :: b(:)
        REAL(SRK),INTENT(OUT) :: x(:)
        INTEGER(SIK) :: i,j,k,d
        INTEGER(SIK),DIMENSION(M%n) :: uptr
        REAL(SRK),DIMENSION(M%n) :: y
        REAL(SRK) :: sum
        ! Solve Ly=b for y
        DO i=1,M%n
          sum=0.0_SRK
          DO k=M%ia(i),M%ia(i+1)-1
            j=M%ja(k)
            IF(i.eq.j) THEN
              uptr(i)=k
              EXIT
            ENDIF
            sum=sum+M%a(k)*y(j)
          ENDDO
          y(i)=b(i)-sum
        ENDDO
        ! Solve Ux=y for x
        DO i=M%n,1,-1
          sum=0.0_SRK
          d=uptr(i)
          DO k=d+1,M%ia(i+1)-1
            j=M%ja(k)
            sum=sum+M%a(k)*x(j)
          ENDDO
          x(i)=(y(i)-sum)/M%a(d)
        ENDDO
    ENDSUBROUTINE MinvMult_Sparse
!
!-------------------------------------------------------------------------------
!> @brief Wrapper to perform dense inv(M)*b=x
!> @param Minv The preconditioner of A
!> @param b the RHS vector
!> @param x the output vector, x=inv(M)*b
!>
!> This subroutine applies the inverse of a matrix M to a vector b and returns x.
!> Minv is stored as a dense matrix.
!> 
    SUBROUTINE MinvMult_Dense(Minv,b,x)
        TYPE(DenseSquareMatrixType),INTENT(IN) :: Minv
        REAL(SRK),INTENT(IN) :: b(:)
        REAL(SRK),INTENT(INOUT) :: x(:)
        x=0._SRK
        CALL BLAS_matvec(THISMATRIX=Minv,X=b,Y=x)
    ENDSUBROUTINE MinvMult_dense
!
!-------------------------------------------------------------------------------
!> @brief Solve a tridiagonal system on a tridiag matrix using G.E.
!> @param solver The LinearSolverType object, previously decomposed with PLU method.
!>
!> This routine assumes that the tridiagonal matrix has already been decomposed
!> in to its PLU parts, with LU stored in M.
!>
    SUBROUTINE solvePLU_TriDiag(solver)
      CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver
      INTEGER(SIK) :: n,i
      REAL(SRK) :: Xprev

      solver%info=-1
      IF(solver%isDecomposed) THEN
        SELECTTYPE(M => solver%M); TYPE IS(TriDiagMatrixType)
          SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
            n=M%n
            !LUx=b,Ux=y, Ly=b
            !find y (Ly=b), y is stored in X to save space
            CALL X%set(1,solver%b%get(1))
            DO i=2,n
              CALL X%set(i,solver%b%get(i)-M%a(1,i)*X%get(i-1))
            ENDDO
            !find x with backward substitution (Ux=y)
            CALL X%set(n,X%get(n)*M%a(2,n))
            Xprev=X%get(n)
            DO i=(n-1),1,-1
              CALL X%set(i,(X%get(i)-M%a(3,i)*Xprev)*M%a(2,i))
              Xprev=X%get(i)
            ENDDO
            solver%info=0
          ENDSELECT
        ENDSELECT
      ENDIF
    ENDSUBROUTINE solvePLU_TriDiag
!
!-------------------------------------------------------------------------------
!> @brief Solve a dense square system using Gaussian Elimination method
!> @param solver The LinearSolverType object
!>
!> This routine perform partial pivoting.
!>
    SUBROUTINE solveGE_DenseSquare(solver)
      CLASS(LinearSolverType_Direct),INTENT(INOUT) :: solver
      
      REAL(SRK) :: thisa(solver%A%n,solver%A%n)
      REAL(SRK) :: t,thisb(solver%A%n)
      INTEGER(SIK) :: N,i,irow,icol,IPIV(solver%A%n)

      SELECTTYPE(b => solver%b); TYPE IS(RealVectorType)
        thisb=b%b
      ENDSELECT
      SELECTTYPE(A => solver%A); TYPE IS(DenseSquareMatrixType)
        thisa=A%A
      ENDSELECT
      solver%info=-1
      N=solver%A%n
      
      DO i=1,N-1
      !For each variable find pivot row and perform forward substitution
        !Find the pivot row
        t=0._SRK
        DO irow=i,N
          IF(ABS(thisa(irow,i)) > t) THEN
            t=ABS(thisa(irow,i))
            IPIV(i)=irow
          ENDIF
        ENDDO
        !The return information
        IF(t == 0) RETURN
        !if it differs from the current row, interchange the two rows.
        IF(IPIV(i) /= i) THEN
          CALL BLAS_swap(N,thisa(IPIV(i):N,1),N,thisa(i:N,1),N)
          t=thisb(i);thisb(i)=thisb(IPIV(i));thisb(IPIV(i))=t
        ENDIF
        
        !Perform forward substitution
        DO irow=i+1,N
          thisa(irow,i)=thisa(irow,i)/thisa(i,i)
          CALL BLAS_axpy(N-i,-thisa(irow,i),thisa(i:N,i+1),N,thisa(irow:N,i+1),N)
          thisb(irow)=thisb(irow)-thisa(irow,i)*thisb(i)
        ENDDO
      ENDDO

      !Perform backward substitution
      IF(thisa(N,N) .APPROXEQ. 0._SRK) RETURN
      SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
        CALL X%set(N,thisb(N)/thisa(N,N))
      ENDSELECT
      DO irow=N-1,1,-1
        t=0._SRK
        DO icol=irow+1,N
          t=t+thisa(irow,icol)*solver%X%get(icol)
        ENDDO
         SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
           CALL X%set(irow,(thisb(irow)-t)/thisa(irow,irow))
         ENDSELECT
      ENDDO
      solver%info=0
    ENDSUBROUTINE solveGE_DenseSquare
!-------------------------------------------------------------------------------
!> @brief Decompose a dense square system into a upper triangular matrix and a 
!> lower triangular matrix.
!> @param solver The linear solver object
!>
!> This routine perform partial pivoting.
!>
    SUBROUTINE DecomposePLU_DenseSquare(solver)
      CLASS(LinearSolverType_Direct),INTENT(INOUT) :: solver
      TYPE(ParamType) :: pList

      REAL(SRK) :: t
      INTEGER(SIK) :: N,i,irow
      LOGICAL(SBK) :: localalloc

      localalloc=.FALSE.
      IF(.NOT.(ASSOCIATED(eLinearSolverType)))THEN
        ALLOCATE(eLinearSolverType)
        localalloc=.TRUE.
      ENDIF

      IF(ALLOCATED(solver%M)) THEN
        CALL solver%M%clear()
        DEALLOCATE(solver%M)
      ENDIF
      ALLOCATE(DenseSquareMatrixType :: solver%M)

      IF(ALLOCATED(solver%IPIV)) THEN
        CALL demallocA(solver%IPIV)
      ENDIF
      CALL dmallocA(solver%IPIV,solver%A%n)

      CALL pList%add('PL->n',solver%A%n)
      CALL pList%add('PL->m',0_SNK)
      CALL solver%M%init(pList)
      SELECTTYPE(M => solver%M); TYPE IS(DenseSquareMatrixType)
        SELECTTYPE(A => solver%A); TYPE IS(DenseSquareMatrixType)
          M=A
        ENDSELECT
      ENDSELECT

      solver%IPIV=0
      solver%info=-1
      SELECTTYPE(M => solver%M)
        TYPE IS(DenseSquareMatrixType)
          N=solver%A%n
          !For each variable find pivot row and perform forward substitution
          DO i=1,N-1
            !Find the pivot row
            t=0._SRK
            DO irow=i,N
              IF(ABS(M%A(irow,i)) > t) THEN
                t=ABS(M%A(irow,i))
                solver%IPIV(i)=irow
              ENDIF
            ENDDO

            IF(t .APPROXEQ. 0._SRK) RETURN
            !if it differs from the current row, interchange the two rows.
            IF(solver%IPIV(i) /= i) THEN
              CALL BLAS_swap(N,M%A(solver%IPIV(i):N,1),N,M%A(i:N,1),N)
            ENDIF

            !Perform forward substitution
            DO irow=i+1,N
              M%A(irow,i)=M%A(irow,i)/M%A(i,i)
              CALL BLAS_axpy(N-i,-M%A(irow,i),M%A(i:N,i+1),N,M%A(irow:N,i+1),N)
            ENDDO
          ENDDO
          IF(M%A(N,N) .APPROXEQ. 0._SRK) RETURN
          solver%info=0
          solver%isDecomposed=.TRUE.
      ENDSELECT
      IF(localalloc) DEALLOCATE(eLinearSolverType)
    ENDSUBROUTINE DecomposePLU_DenseSquare
!-------------------------------------------------------------------------------
!> @brief Solve dense square linear system by PLU method with decomposed matrix.
!> @param solver The linear solver object
!>
!> This routine works only when the matrix has been decomposed. If not, the
!> original value will not be changed, and solver%info returns -1.
!>
    SUBROUTINE SolvePLU_DenseSquare(solver)
      CLASS(LinearSolverType_Direct),INTENT(INOUT) :: solver
      
      REAL(SRK) :: t,thisb(solver%A%n),thisx(solver%A%n)
      INTEGER(SIK) :: N,irow,icol
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.(ASSOCIATED(eLinearSolverType)))THEN
        ALLOCATE(eLinearSolverType)
        localalloc=.TRUE.
      ENDIF
      
      solver%info=-1
      IF(solver%isDecomposed) THEN
        SELECTTYPE(b => solver%b); TYPE IS(RealVectorType)
          thisb=b%b
        ENDSELECT
        N=solver%A%n
        !Permutate right hand side
        DO irow=1,N
          IF(solver%IPIV(irow) /= 0) THEN
            t=thisb(irow)
            thisb(irow)=thisb(solver%IPIV(irow))
            thisb(solver%IPIV(irow))=t
          ENDIF
        ENDDO
        SELECTTYPE(M => solver%M)
          TYPE IS(DenseSquareMatrixType)
          !Forward subsitution
          thisx(1)=thisb(1)
          DO irow=2,N
            t=0._SRK
            DO icol=1,irow-1
              t=t+thisx(icol)*M%A(irow,icol)
            ENDDO
            thisx(irow)=thisb(irow)-t
          ENDDO
          !Backward subsitution
          thisb(N)=thisx(N)/M%A(N,N)
          DO irow=N-1,1,-1
            t=0._SRK
            DO icol=irow+1,N
              t=t+thisb(icol)*M%A(irow,icol)
            ENDDO
            thisb(irow)=(thisx(irow)-t)/M%A(irow,irow)
          ENDDO
        ENDSELECT
        SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
          X%b=thisb
        ENDSELECT
        solver%info=0
      ENDIF
      IF(localalloc) DEALLOCATE(eLinearSolverType)
    ENDSUBROUTINE SolvePLU_DenseSquare
!
!-------------------------------------------------------------------------------
!> @brief Solve the sparse linear system
!> @param solver The linear solver object
!>
!> This routine solves the sparse linear system by two method. If the MKL library
!> could be found, the PLU method will be called. If it is not found, hard coded
!> CGNR method will be used instead.
!>
    SUBROUTINE solvePLU_Sparse(solver)
      CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver
#ifdef HAVE_MKL
      !Not sure if this will actually work at link time, if it doesn't then
      !"REAL(KIND(0.0d0)),EXTERNAL :: " should work. But then the pure
      !attribute will need to be removed from all the routines.
      INTERFACE
        PURE SUBROUTINE dss_create(handle,opt)
!          CLASS(MKL_DSS_HANDLE),INTENT(OUT) :: handle
          INTEGER,OPTIONAL,INTENT(IN) :: handle
          INTEGER,OPTIONAL,INTENT(IN) :: opt
        ENDSUBROUTINE dss_create
        
        PURE SUBROUTINE dss_define_structure(handle,opt,rowIndex,nRows,nCols, &
          columns,nNonZeros)
!          CLASS(MKL_DSS_HANDLE),INTENT(OUT) :: handle
          INTEGER,OPTIONAL,INTENT(IN) :: handle
          INTEGER,OPTIONAL,INTENT(IN) :: opt
          INTEGER,INTENT(IN) :: rowIndex
          INTEGER,INTENT(IN) :: nRows
          INTEGER,INTENT(IN) :: nCols
          INTEGER,INTENT(IN) :: columns(*)
          INTEGER,INTENT(IN) :: nNonZeros
          ENDSUBROUTINE dss_define_structure
          
        PURE SUBROUTINE dss_reorder(handle,opt,perm)
!          CLASS(MKL_DSS_HANDLE),INTENT(OUT) :: handle
          INTEGER,OPTIONAL,INTENT(IN) :: handle
          INTEGER,OPTIONAL,INTENT(IN) :: opt
          INTEGER,INTENT(IN) :: perm(*)
        ENDSUBROUTINE dss_reorder
        
        PURE SUBROUTINE dss_factor_real(handle,opt,rValues)
!          CLASS(MKL_DSS_HANDLE),INTENT(OUT) :: handle
          INTEGER,OPTIONAL,INTENT(IN) :: handle
          INTEGER,OPTIONAL,INTENT(IN) :: opt
          REAL(KIND(0.0D0)),INTENT(IN) :: rValues(*)
        ENDSUBROUTINE dss_factor_real
        
        PURE SUBROUTINE dss_solve_real(handle,opt,rRhsValues,nRhs,rSolValues)
!          CLASS(MKL_DSS_HANDLE),INTENT(OUT) :: handle
          INTEGER,OPTIONAL,INTENT(IN) :: handle
          INTEGER,OPTIONAL,INTENT(IN) :: opt
          INTEGER,INTENT(IN) :: nRhs
          REAL(KIND(0.0D0)),INTENT(IN) :: rRhsValues(1,*)
          REAL(KIND(0.0D0)),INTENT(IN) :: rSolValues(1,*)
        ENDSUBROUTINE dss_solve_real
        
        PURE SUBROUTINE dss_delete(handle,opt)
!          CLASS(MKL_DSS_HANDLE),INTENT(OUT) :: handle
          INTEGER,OPTIONAL,INTENT(IN) :: handle
          INTEGER,OPTIONAL,INTENT(IN) :: opt
        ENDSUBROUTINE dss_delete
      ENDINTERFACE
#else
#endif
    ENDSUBROUTINE solvePLU_Sparse
!-------------------------------------------------------------------------------
!> @brief Solve the rectangular linear system by CGNR method
!> @param solver The linear solver object
!>
!> This routine solves the rectangular linear system by CGNR method. It only
!> works when the number of equations is larger than the number of the unknowns.
!> IF not solver%info will return -1.
!>
    SUBROUTINE solveCGNR(solver)
      CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver

      INTEGER(SIK) :: M,N,i,maxIters
      REAL(SRK) :: alpha,beta,error,z0_dot,z1_dot,convTol
      TYPE(RealVectorType) :: z,w,r,p,b
      
      N=solver%A%n
      M=N
      SELECTTYPE(A => solver%A); TYPE IS(DenseRectMatrixType)
        M=A%m
      ENDSELECT
      solver%info=-1
      convTol=1e-9
      maxIters=M
      SELECTTYPE(solver); TYPE IS(LinearSolverType_Iterative)
        maxIters=MIN(M,solver%maxIters)
        convTol=solver%convTol
      ENDSELECT
      IF(N >= M) THEN
        CALL r%init(N)
        CALL w%init(N)
        CALL b%init(N)
        CALL z%init(M)
        CALL p%init(M)
        CALL r%set(0._SRK)
        CALL z%set(0._SRK)
        
        SELECTTYPE(vecb => solver%b); TYPE IS(RealVectorType)
            b%b=vecb%b
        ENDSELECT
        
        CALL BLAS_matvec(THISMATRIX=solver%A,X=solver%X,Y=r)
        r%b=b%b-r%b
        
        CALL BLAS_matvec(THISMATRIX=solver%A,trans='t',X=r,Y=z)
        p%b=z%b
        z0_dot=BLAS_dot(z,z)
        DO i=1,maxIters
          CALL w%set(0._SRK)
          CALL BLAS_matvec(THISMATRIX=solver%A,X=p,Y=w)
          alpha=z0_dot/BLAS_dot(w,w)
          SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
            X%b=X%b+alpha*p%b
          ENDSELECT
          r%b=r%b-alpha*w%b
          error=BLAS_dot(r,r)
          IF(error < convTol) EXIT
          CALL z%set(0._SRK)
          CALL BLAS_matvec(THISMATRIX=solver%A,TRANS='t',X=r,Y=z)
          z1_dot=BLAS_dot(z,z)
          beta=z1_dot/z0_dot
          p%b=z%b+beta*p%b
          z0_dot=z1_dot
        ENDDO
        solver%info=0
        SELECTTYPE(solver); TYPE IS(LinearSolverType_Iterative)
          solver%iters=i
          solver%residual=error
        ENDSELECT
        CALL r%clear()
        CALL w%clear()
        CALL b%clear()
        CALL z%clear()
        CALL p%clear()
      ENDIF
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Find the L-norm of a given vector.
!> @param x The vector, a 1-D SRK array
!> @param L The type of norm to calculate (L-norm)
!>
!> This routine finds the L-norm of the inputted vector, it is a wrapper for the
!> BLAS routines.  The only thing here not in BLAS are the L- and infinite-norms.
!>
    PURE SUBROUTINE LNorm(x,L,norm)
      REAL(SRK),DIMENSION(:),INTENT(IN) :: x
      INTEGER(SIK),INTENT(IN) :: L
      REAL(SRK),INTENT(OUT) :: norm
      INTEGER(SIK) :: i
      SELECT CASE(L)
        CASE(-1)
          !signifier for infinite norm
          i=BLAS_iamax(x)
          norm=ABS(x(i))
        CASE(1)
          norm=BLAS_asum(x)
        CASE(2)
          !2-norm
          norm=BLAS_nrm2(x)
        CASE (: -2)
          !not possible.
          norm=0.0_SRK
        CASE DEFAULT
          !L-norm
          norm=0.0_SRK
          DO i=1,SIZE(x)
            norm=norm+ABS(x(i))**L
          ENDDO
          norm=norm**(1._SRK/L)
      ENDSELECT
    ENDSUBROUTINE LNorm

ENDMODULE LinearSolverTypes