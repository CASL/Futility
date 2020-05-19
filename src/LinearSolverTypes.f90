!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module provides a linear system type and methods to solve systems
!> of equations
!>
!> The linear solver/system type owns it's own matrices and vectors. It is
!> initialized with parameter lists. In general any third party library (TPL)
!> that may be interfaced with this module should be implemented such that it's
!> optional.
!>
!> For valid reference lists
!> see @ref MatrixTypes::LinearSolverTypes_Declare_ValidParams
!> "LinearSolverTypes_Declare_ValidParams".
!>
!> Currently supported TPLs include:
!>  - PETSc (with interfaces to KSP)
!>
!> Additional TPL support is planned for:
!>  - Trilinos
!>  - MKL
!>  - LAPACK
!>  - ScaLAPACK
!>
!> The solver methods include two classes: Direct and Iterative
!>  - Direct Methods
!>    - LU decomposition (with partial pivoting)
!>  - Iterative Methods
!>    - BiCGSTAB (with ILU preconditioning)
!>    - GMRES
!>    - CGNR
!>
!> @par EXAMPLES
!> @code
!>
!> @endcode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#ifdef HAVE_PARDISO
include 'mkl_pardiso.f90'
#endif
MODULE LinearSolverTypes
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE BLAS
USE trilinos_interfaces
USE Times
USE ExceptionHandler
USE Allocs
USE ParameterLists
USE ParallelEnv
USE VectorTypes
USE MatrixTypes
USE PreconditionerTypes
USE Strings
USE IOUtil
#ifdef HAVE_PARDISO
USE MKL_PARDISO
#endif
#ifdef FUTILITY_HAVE_Trilinos
USE ForTeuchos_ParameterList
#endif

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if (((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>6)) || (PETSC_VERSION_MAJOR>=4))
USE PETSCKSP
#endif
#endif

IMPLICIT NONE

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if (((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6)) || (PETSC_VERSION_MAJOR>=4))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
!petscisdef.h defines the keyword IS, and it needs to be reset
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
PUBLIC :: LinearSolverType_reqParams,LinearSolverType_optParams
PUBLIC :: LinearSolverType_Declare_ValidParams
PUBLIC :: LinearSolverType_Clear_ValidParams

!> set enumeration scheme for TPLs
!> Temporary, old parameter names to be kept until MPACT is updated
INTEGER(SIK),PARAMETER,PUBLIC :: PETSC=0,TRILINOS=1,PARDISO_MKL=2,MKL=3,NATIVE=4
INTEGER(SIK),PARAMETER,PUBLIC :: LINSYS_PETSC=0,LINSYSTRILINOS=1, &
    LINSYS_PARDISO_MKL=2,LINSYS_MKL=3,LINSYS_NATIVE=4
!> Number of iterative solver solution methodologies - for error checking
INTEGER(SIK),PARAMETER :: MAX_IT_SOLVER_METHODS=9
!> set enumeration scheme for iterative solver methods
!>   In PETSc, PCSOR is GS locally and Jacobi globally.
INTEGER(SIK),PARAMETER,PUBLIC :: BICGSTAB=1,CGNR=2,GMRES=3,MULTIGRID=4, &
                                 SOR=5,ILU=6,BJACOBI=7,JACOBI=8,GAUSS_SEIDEL=9
!> Number of direct solver solution methodologies - for error checking
INTEGER(SIK),PARAMETER :: MAX_DIRECT_SOLVER_METHODS=3
!> set enumeration scheme for direct solver methods
INTEGER(SIK),PARAMETER,PUBLIC :: GE=1,LU=2,QR=3


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
  !> Initialization status of X (only needed for PETSc)
  LOGICAL(SBK) :: hasX=.FALSE.
  !> Pointer to the MatrixType A
  CLASS(MatrixType),POINTER :: A => NULL()
  !> Right-hand side vector, b
  CLASS(VectorType),POINTER :: b => NULL()
  !> Pointer to solution vector, x
  CLASS(VectorType),POINTER :: X => NULL()
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

#ifdef FUTILITY_HAVE_PETSC
  KSP :: ksp
  PC :: pc
#endif
#ifdef FUTILITY_HAVE_Trilinos
  INTEGER(SIK) :: Belos_solver
  INTEGER(SIK) :: Belos_pc
  LOGICAL(SBK) :: belos_pc_set=.TRUE.
#endif
!
!List of Type Bound Procedures
  CONTAINS
    !> Deferred routine for initializing the linear solver system
    PROCEDURE,PASS :: init => init_LinearSolverType_Base
    !> Deferred routine for clearing the linear solver
    PROCEDURE(linearsolver_sub_absintfc),DEFERRED,PASS :: clear
    !> Deferred routine for solving the linear system
    PROCEDURE(linearsolver_sub_absintfc),DEFERRED,PASS :: solve
    !> Routine for updating status of M and isDecomposed when A has changed
    !> and associates A with KSP if necessary
    PROCEDURE,PASS :: updatedA
    !> Routine for determining residual vector
    PROCEDURE(linearsolver_getRes_absintfc),DEFERRED,PASS :: getResidualVec
    !> Routine for determining residual norm
    PROCEDURE(linearsolver_getResNorm_absintfc),DEFERRED,PASS :: getResidualNorm
    !> Generic for getResidual
    GENERIC :: getResidual => getResidualVec,getResidualNorm
ENDTYPE LinearSolverType_Base

!> Explicitly defines the interface for the clear and solve routines
ABSTRACT INTERFACE
  SUBROUTINE linearsolver_sub_absintfc(solver)
    IMPORT :: LinearSolverType_Base
    CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver
  ENDSUBROUTINE linearsolver_sub_absintfc
  SUBROUTINE linearsolver_getRes_absintfc(solver,resid,nIters)
    IMPORT :: LinearSolverType_Base,VectorType,SIK
    CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver
    CLASS(VectorType),INTENT(INOUT) :: resid
    INTEGER(SIK),INTENT(OUT),OPTIONAL :: nIters
  ENDSUBROUTINE linearsolver_getRes_absintfc
  SUBROUTINE linearsolver_getResNorm_absintfc(solver,resid,nIters)
    IMPORT :: LinearSolverType_Base,SRK,SIK
    CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver
    REAL(SRK),INTENT(OUT) :: resid
    INTEGER(SIK),INTENT(OUT),OPTIONAL :: nIters
  ENDSUBROUTINE linearsolver_getResNorm_absintfc
ENDINTERFACE
!> Explicitly defines the interface for the getresidual routine

!> @brief The extended type for the Direct Linear Solver
TYPE,EXTENDS(LinearSolverType_Base) :: LinearSolverType_Direct
  !> Storage of row exchanges
  INTEGER(SIK),ALLOCATABLE :: IPIV(:)
#ifdef HAVE_PARDISO
  TYPE(MKL_PARDISO_HANDLE),ALLOCATABLE:: pt(:)
  INTEGER(SIK),ALLOCATABLE :: iparm(:)
  INTEGER(SIK),ALLOCATABLE :: perm(:)
  INTEGER(SIK) :: phase
  INTEGER(SIK) :: mtype
#endif
!
!List of Type Bound Procedures
  CONTAINS
    !> @copybrief LinearSolverTypes::clear_LinearSolverType_Direct
    !> @copydetails LinearSolverTypes::clear_LinearSolverType_Direct
    PROCEDURE,PASS :: clear => clear_LinearSolverType_Direct
    !> @copybrief LinearSolverTypes::solve_LinearSolverType_Direct
    !> @copydetails LinearSolverTypes::solve_LinearSolverType_Direct
    PROCEDURE,PASS :: solve => solve_LinearSolverType_Direct
    !> @copybrief LinearSolverTypes::getResidual_LinearSolverType_Direct
    !> @copydetails LinearSolverTypes::getResidual_LinearSolverType_Direct
    PROCEDURE,PASS :: getResidualVec => getResidualVec_LinearSolverType_Direct
    PROCEDURE,PASS :: getResidualNorm => getResidualNorm_LinearSolverType_Direct
ENDTYPE LinearSolverType_Direct

!> @brief The extended type for the Iterative Linear Solver
TYPE,EXTENDS(LinearSolverType_Base) :: LinearSolverType_Iterative
  !> Status of the presence of the initial guess, X0
  LOGICAL(SBK) :: hasX0=.FALSE.
  !> Type of norm to be used for convergence checks
  INTEGER(SIK) :: normType=2_SIK
  !> Maximum number of iterations to perform
  INTEGER(SIK) :: maxIters=1000_SIK
  !> Number of iterations before restart for GMRES
  INTEGER(SIK) :: nRestart=30_SIK
  !> Actual iterations performed
  INTEGER(SIK) :: iters=0_SIK
  !> Relative tolerance for successful convergence
  REAL(SRK) :: relConvTol=1.0E-5_SRK
  !> Absolute tolerance for successful convergence
  REAL(SRK) :: absConvtol=1.0E-5_SRK
  !> Actual residual converged to
  REAL(SRK) :: residual=0._SRK
  !> Params for preconditioner, supplied at %init
  !> Stored here because PC is not init until %setupPC is called
  TYPE(ParamType) :: pcParams
  !> Preconditioner to be used by LinearSolverTyope
  CLASS(PreconditionerType),ALLOCATABLE :: PreCondType
  !> The type of preconditioner
  TYPE(StringType) :: PCTypeName
  !> Number of times to precondition the system
  INTEGER(SIK) :: pciters=0_SIK
  !> Frequency with which to re-setup preconditioner
  INTEGER(SIK) :: pcsetup=0_SIK
  !> Setup status of precondintioner
  LOGICAL(SBK) :: isPcSetup=.FALSE.
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
    PROCEDURE,PASS :: getResidualNorm => getResidualNorm_LinearSolverType_Iterative
    PROCEDURE,PASS :: getResidualVec => getResidualVec_LinearSolverType_Iterative
        !getResidualNorm_LinearSolverType_Iterative
    !> @copybrief LinearSolverTypes::getIterResidual_LinearSolverType_Iterative
    !> @copydetails LinearSolverTypes::getIterResidual_LinearSolverType_Iterative
    !PROCEDURE,PASS :: getIterResidual => getIterResidual_LinearSolverType_Iterative
    !> @copybrief LinearSolverTypes::setConv_LinearSolverType_Iterative
    !> @copydetails LinearSolverTypes::setConv_LinearSolverType_Iterative
    PROCEDURE,PASS :: setConv => setConv_LinearSolverType_Iterative
    !> @copybrief LinearSolverTypes::setX0_LinearSolverType_Iterative
    !> @copydetails LinearSolverTypes::setX0_LinearSolverType_Iterative
    PROCEDURE,PASS :: setX0 => setX0_LinearSolverType_Iterative
    !> @copybrief LinearSolverTypes::setup_PreCond_LinearSolverType_Iterative
    !> @copydetails LinearSolverTypes::setup_PreCond_LinearSolverType_Iterative
    PROCEDURE,PASS :: setupPC => setup_PreCond_LinearSolverType_Iterative
ENDTYPE LinearSolverType_Iterative

!> Logical flag to check whether the required and optional parameter lists
!> have been created yet for the Linear Solver Type.
LOGICAL(SBK),SAVE :: LinearSolverType_Paramsflag=.FALSE.

!> The parameter lists to use when validating a parameter list for
!> initialization for a Linear Solver Type.
TYPE(ParamType),PROTECTED,SAVE :: LinearSolverType_reqParams,LinearSolverType_optParams

!> Exception Handler for use in MatrixTypes
TYPE(ExceptionHandlerType),SAVE :: eLinearSolverType

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
SUBROUTINE init_LinearSolverType_Base(solver,Params,A)
  CHARACTER(LEN=*),PARAMETER :: myName='init_LinearSolverType_Base'
  CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver
  TYPE(ParamType),INTENT(IN) :: Params
  CLASS(MatrixType),POINTER,INTENT(INOUT),OPTIONAL :: A
  CLASS(ParamType),POINTER :: pListPtr
  TYPE(ParamType) :: validParams,matPList,vecxPList,vecbPList,pcPList
  ! local variables
  INTEGER(SIK) :: n
  INTEGER(SIK) :: ReqTPLType,TPLType,solverMethod,pciters,pcsetup
  INTEGER(SIK) :: matType,matEngine
  INTEGER(SIK) :: comm,numberOMP
  CHARACTER(LEN=256) :: ReqTPLTypeStr,TPLTypeStr
  CHARACTER(LEN=:),ALLOCATABLE :: timerName,PreCondType
#ifdef FUTILITY_HAVE_PETSC
  KSP :: ksp_temp
  PC :: pc
  PetscErrorCode  :: iperr
#endif
#if defined(FUTILITY_HAVE_PETSC) || defined(FUTILITY_HAVE_Trilinos)
  INTEGER(SIK) :: ierr
#endif
#ifdef FUTILITY_HAVE_Trilinos
  TYPE(ParamType) :: belosParams
  TYPE(ForTeuchos_ParameterList_ID) :: plID
#endif
  !Check to set up required and optional param lists.
  IF(.NOT.LinearSolverType_Paramsflag) CALL LinearSolverType_Declare_ValidParams()

  !Validate against the reqParams and OptParams
  validParams=Params
  CALL validParams%validate(LinearSolverType_reqParams)

  !Pull LS data from the parameter list
  TPLType=-1
  solverMethod=-1
  comm=-1
  numberOMP=-1
  matType=-1
  matEngine=-1
  pciters=-1
  pcsetup=-1
  CALL validParams%get('LinearSolverType->TPLType',TPLType)
  CALL validParams%get('LinearSolverType->solverMethod',solverMethod)
  CALL validParams%get('LinearSolverType->MPI_Comm_ID',comm)
  CALL validParams%get('LinearSolverType->numberOMP',numberOMP)
  CALL validParams%get('LinearSolverType->timerName',timerName)
  CALL validParams%get('LinearSolverType->matType',matType)
  IF(validParams%has('LinearSolverType->A->MatrixType->matType')) THEN
    CALL validParams%set('LinearSolverType->A->MatrixType->matType',matType)
  ELSE
    CALL validParams%add('LinearSolverType->A->MatrixType->matType',matType)
  ENDIF

  IF (matType == DISTRIBUTED_BANDED .OR. matType == DISTR_BLOCKBANDED) THEN
    CALL validParams%add('LinearSolverType->x->VectorType->vecType',DISTRIBUTED_NATIVE)
    CALL validParams%add('LinearSolverType->b->VectorType->vecType',DISTRIBUTED_NATIVE)
  ELSEIF (matType == BANDED) THEN
    CALL validParams%add('LinearSolverType->x->VectorType->vecType',REAL_NATIVE)
    CALL validParams%add('LinearSolverType->b->VectorType->vecType',REAL_NATIVE)
  ENDIF

  ! pull data for matrix and vector parameter lists
  CALL validParams%get('LinearSolverType->A->MatrixType',pListPtr)
  matPList=pListPtr
  CALL validParams%get('LinearSolverType->x->VectorType',pListPtr)
  vecxPList=pListPtr
  CALL validParams%get('LinearSolverType->b->VectorType',pListPtr)
  vecbPList=pListPtr
  ! Check for Preconditioner Data
  IF(validParams%has('LinearSolverType->PC->PreCondType')) THEN
    CALL validParams%get('LinearSolverType->PC->PreCondType',pListPtr)
    pcPList = pListPtr
    CALL validParams%get('LinearSolverType->PC->PreCondType->pcType',PreCondType)

    SELECTTYPE(solver); TYPE IS(LinearSolverType_Iterative)
      solver%pcIters = -1
      IF (validParams%has('LinearSolverType->PC->PreCondType->pcIters')) &
        CALL validParams%get('LinearSolverType->PC->PreCondType->pcIters',solver%pcIters)
      solver%pcSetup = 0
      IF (validParams%has('LinearSolverType->PC->PreCondType->pcSetup')) &
        CALL validParams%get('LinearSolverType->PC->PreCondType->pcSetup',solver%pcSetup)
    ENDSELECT
  ELSE
    CALL pcPList%clear()
    CALL pcPList%add('PreCondType->pcType','DEFAULT')
    SELECTTYPE(solver); TYPE IS(LinearSolverType_Iterative)
      PreCondType = 'DEFAULT'
      solver%pcIters = -1
      solver%pcSetup = 0
    ENDSELECT
  ENDIF

  !add mpi communicator to parameter lists
  CALL matPList%add('MatrixType->MPI_Comm_ID',comm)
  CALL vecxPList%add('VectorType->MPI_Comm_ID',comm)
  CALL vecbPList%add('VectorType->MPI_Comm_ID',comm)
  CALL pcPList%add('PreCondType->MPI_Comm_ID',comm)
  !pull size from source vector
  CALL validParams%get('LinearSolverType->b->VectorType->n',n)

  CALL validParams%clear()

  !Initialize parallel environments based on input
  IF(comm /= -1) CALL solver%MPIparallelEnv%init(comm)
  IF(numberOMP > 0) CALL solver%OMPparallelEnv%init(numberOMP)

  IF(.NOT.solver%isInit) THEN
    solver%info=0

    ReqTPLType=TPLType
    ! go through solver hierarchy to determine TPLType
    IF(TPLType == PETSC) THEN ! PETSc
#ifndef FUTILITY_HAVE_PETSC
      TPLType=TRILINOS
#endif
    ENDIF
    IF(TPLType == TRILINOS) THEN ! Trilinos
#ifndef FUTILITY_HAVE_Trilinos
      TPLType=PARDISO_MKL
#endif
    ENDIF
    IF(TPLType == PARDISO_MKL) THEN ! PARDISO
#ifndef HAVE_PARDISO
      TPLType=MKL
#endif
    ENDIF
    IF(TPLType == MKL) THEN ! MKL
#ifndef HAVE_MKL
      TPLType=LINSYS_NATIVE
#endif
    ENDIF

    ! get right string for requested tpl type
    SELECTCASE(ReqTPLType)
    CASE(PETSC)
      ReqTPLTypeStr='PETSC'
    CASE(TRILINOS)
      ReqTPLTypeStr='TRILINOS'
    CASE(PARDISO_MKL)
      ReqTPLTypeStr='PARDISO_MKL'
    CASE(MKL)
      ReqTPLTypeStr='MKL'
    CASE(LINSYS_NATIVE)
      ReqTPLTypeStr='NATIVE'
    ENDSELECT

    ! get right string for actual tpl type
    SELECTCASE(TPLType)
    CASE(PETSC)
      TPLTypeStr='PETSC'
      matEngine=VM_PETSC
    CASE(TRILINOS)
      TPLTypeStr='TRILINOS'
      matEngine=VM_TRILINOS
    CASE(PARDISO_MKL)
      TPLTypeStr='PARDISO_MKL'
      matEngine=VM_NATIVE
    CASE(MKL)
      TPLTypeStr='MKL'
      matEngine=VM_NATIVE
    CASE(LINSYS_NATIVE)
      TPLTypeStr='NATIVE'
      matEngine=VM_NATIVE
    ENDSELECT

    !print status of TPL post-heirarchy
    IF(ReqTPLType /= TPLType) THEN
      CALL eLinearSolverType%raiseDebug(modName//'::'// &
          myName//' - Requested TPL '//TRIM(ReqTPLTypeStr)// &
          ' is not enabled, will use '//TRIM(TPLTypeStr)//' solvers instead.')
    ENDIF

    CALL matPList%add("MatrixType->engine",matEngine)
    CALL vecxPList%add('VectorType->engine',matEngine)
    CALL vecbPList%add('VectorType->engine',matEngine)
    ! allocate and initialize matrix (A)
    CALL MatrixFactory(solver%A, matPList)
#ifdef HAVE_MPI
    SELECT TYPE(A => solver%A); CLASS IS(DistributedBandedMatrixType)
      CALL vecxPList%add('VectorType->chunksize',A%blockSize)
      CALL vecbPList%add('VectorType->chunksize',A%blockSize)
    ENDSELECT
#endif
#ifdef HAVE_PARDISO
    SELECTTYPE(solver); TYPE IS(LinearSolverType_Direct)
      solver%mtype=11 ! real and nonsymmetric
    ENDSELECT
#endif
    IF(.NOT. ASSOCIATED(solver%A)) THEN
      CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
          "Failed to create matrix A")
    ENDIF

    IF(PRESENT(A)) A=>solver%A

    CALL VectorFactory(solver%X, vecxPlist)
    CALL VectorFactory(solver%b, vecbPlist)

    ! define other linear solver variables
    SELECTTYPE(solver)
    CLASS IS(LinearSolverType_Direct) ! direct solver
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
#ifdef HAVE_PARDISO
      IF(solver%TPLtype == PARDISO_MKL) THEN
        ALLOCATE(solver%iparm(64))
        ALLOCATE(solver%pt(64))
        DO i=1,64
          solver%iparm(i) = 0
          solver%pt(i)%dummy=0
        ENDDO
        solver%iparm(3)=numberOMP !number of threads

        ! allocate and initiailize solver%perm
        ALLOCATE(solver%perm(solver%A%n))
        solver%perm=1

        ! initialize phase
        solver%phase=13

        CALL PARDISOINIT(solver%pt,solver%mtype,solver%iparm)
      ENDIF
#endif
      IF(TPLType==PETSC) THEN
#ifdef FUTILITY_HAVE_PETSC
          !create and initialize KSP
          CALL KSPCreate(solver%MPIparallelEnv%comm,solver%ksp,iperr)
          CALL KSPSetType(solver%ksp,KSPPREONLY,iperr)
          CALL KSPSetFromOptions(solver%ksp,iperr)
          CALL solver%updatedA()

          !PC calls
          CALL KSPGetPC(solver%ksp,pc,ierr)
          CALL PCSetType(pc,PCLU,iperr)
#if (((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>6)) || (PETSC_VERSION_MAJOR>=4))
          CALL PCFactorSetMatSolverType(pc,MATSOLVERSUPERLU,iperr)
          CALL PCFactorSetUpMatSolverType(pc,iperr)
#else
          CALL PCFactorSetMatSolverPackage(pc,MATSOLVERSUPERLU_DIST,iperr)
          CALL PCFactorSetUpMatSolverPackage(pc,iperr)
#endif

#else
          CALL eLinearSolverType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - invalid value of solverMethod')
#endif
      ENDIF

    CLASS IS(LinearSolverType_Iterative) ! iterative solver
      IF((solverMethod > 0) .AND. &
          (solverMethod <= MAX_IT_SOLVER_METHODS)) THEN

        !only GMRES can handle when sparse LS of size 1
        IF(n==1 .AND. matType == SPARSE .AND. solverMethod/= GMRES) THEN
          solverMethod=GMRES
          CALL eLinearSolverType%raiseDebug(modName//'::'// &
              myName//' - Only GMRES can handle sparse systems of size 1.  '// &
              'Switching solver method to GMRES.')
        ENDIF

        solver%solverMethod=solverMethod
        solver%TPLType=TPLType

        solver%pcParams = pcPList

        IF(TPLType==PETSC) THEN
#ifdef FUTILITY_HAVE_PETSC
          !create and initialize KSP
          CALL KSPCreate(solver%MPIparallelEnv%comm,solver%ksp,iperr)

          !set iterative solver type
          SELECTCASE(solverMethod)
          CASE(BICGSTAB)
            CALL KSPSetType(solver%ksp,KSPBCGS,iperr)
          CASE(CGNR)
            CALL KSPSetType(solver%ksp,KSPCGNE,iperr)
          CASE(GMRES)
            CALL KSPSetType(solver%ksp,KSPGMRES,iperr)
          ENDSELECT

          CALL solver%updatedA()

          !Always use a nonzero initial guess:
          CALL KSPSetInitialGuessNonzero(solver%ksp,PETSC_TRUE,iperr)

          !set preconditioner
          IF((solver%solverMethod == GMRES) .OR. (solver%solverMethod == BICGSTAB)) THEN
            CALL KSPGetPC(solver%ksp,solver%pc,iperr)
            IF(TRIM(PreCondType)=='SOR') THEN
              CALL PCSetType(solver%pc,PCSOR,iperr)
            ELSEIF(TRIM(PreCondType)=='JACOBI') THEN
              CALL PCSetType(solver%pc,PCJACOBI,iperr)
            ELSEIF(TRIM(PreCondType)=='BJACOBI_ILU') THEN
              CALL PCSetType(solver%pc,PCBJACOBI,iperr)
#if (((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>6)) || (PETSC_VERSION_MAJOR>=4))
              CALL PetscOptionsSetValue(PETSC_NULL_OPTIONS,"-sub_ksp_type","preonly",iperr)
              CALL PetscOptionsSetValue(PETSC_NULL_OPTIONS,"-sub_pc_type","ilu",iperr)
#else
              CALL PetscOptionsSetValue("-sub_ksp_type","preonly",iperr)
              CALL PetscOptionsSetValue("-sub_pc_type","ilu",iperr)
#endif
              CALL PCSetFromOptions(solver%pc,iperr)
            ELSEIF(TRIM(PreCondType)=='EISENSTAT') THEN
              CALL PCSetType(solver%pc,PCEISENSTAT,iperr)
            ELSEIF(TRIM(PreCondType)=='MG') THEN
              !This is not actually a MG preconditioner since we are not
              ! providing it any geometric/interpolation/restriction
              ! information.  However, it does perform one Cholesky
              ! smoother step, which seems to be fairly effective for many
              ! problems.
              CALL PCSetType(solver%pc,PCMG,iperr)
            ELSEIF(TRIM(PreCondType)=='SHELL') THEN
              CALL PCSetType(solver%pc,PCSHELL,iperr)
              CALL PCShellSetSetup(solver%pc,PETSC_PCSHELL_setup_extern,iperr)
              CALL PCShellSetApply(solver%pc,PETSC_PCSHELL_apply_extern,iperr)
! Disabling nopc option for PETSC because it breaks a lot of things
!                  ELSEIF(TRIM(PreCondType)=='NOPC') THEN
!                    CALL PCSetType(solver%pc,PCNONE,iperr)
            ELSE   ! Regardless of what else is set, we'll use block-jacobi ILU
              CALL PCSetType(solver%pc,PCBJACOBI,iperr)
            ENDIF
          ENDIF
          CALL KSPSetFromOptions(solver%ksp,iperr)

#else
          CALL eLinearSolverType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - invalid value of solverMethod')
#endif
        ELSEIF(TPLType==TRILINOS) THEN
#ifdef FUTILITY_HAVE_Trilinos
          IF(solverMethod/=GMRES) &
              CALL eLinearSolverType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - Only GMRES solver is supported with Trilinos')

          ! PC option is hard-coded for now
          CALL belosParams%add('belos_options->pc_option', 2_SIK)
          plID = Teuchos_ParameterList_Create(ierr)
          CALL belosParams%toTeuchosPlist(plID)
          CALL Belos_Init(solver%Belos_solver)
          CALL Preconditioner_InitParams(solver%Belos_pc,plID)
          CALL Teuchos_ParameterList_Release(plID,ierr)

          SELECTTYPE(A=>solver%A); TYPE IS(TrilinosMatrixType)
            CALL Belos_SetMat(solver%Belos_solver,A%A)
          ENDSELECT
          SELECTTYPE(x=>solver%X); TYPE IS(TrilinosVectorType)
            CALL Belos_SetX(solver%Belos_solver,x%b)
          ENDSELECT
          SELECTTYPE(b=>solver%b); TYPE IS(TrilinosVectorType)
            CALL Belos_Setb(solver%Belos_solver,b%b)
          ENDSELECT
#else
          CALL eLinearSolverType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - invalid value of solverMethod')
#endif
        ENDIF

        !assign values to solver
        CALL solver%SolveTime%setTimerName(timerName)
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
  CALL vecbPList%clear()
  CALL vecxPList%clear()
  CALL matPList%clear()
  CALL pcPList%clear()
ENDSUBROUTINE init_LinearSolverType_Base
!
!-------------------------------------------------------------------------------
!> @brief Initializes preconditioner for Iteartive Linear Solver Type
!> @param solver The linear solver to act on
!> @param PreCondType The preconditioner method
!>
!> This routine sets up the precondtioner of type PreCondType
!>
SUBROUTINE setup_PreCond_LinearSolverType_Iterative(solver,params)
  CLASS(LinearSolverType_Iterative),INTENT(INOUT) :: solver
  TYPE(ParamType),INTENT(IN),OPTIONAL :: params
  CHARACTER(LEN=*),PARAMETER :: myName='setup_PreCond_LinearSolverType_Iterative'
  CHARACTER(LEN=:),ALLOCATABLE :: pcTypeName

  IF(solver%isinit) THEN
    IF(ASSOCIATED(solver%A)) THEN
      IF(solver%A%isInit) THEN
        IF (.NOT. ALLOCATED(solver%PreCondType)) THEN
          ! Set up PreconditionerType
          CALL solver%pcParams%get('PreCondType->pcType',pcTypeName)

          ! If pciters < 0 then preconditioning will always be used
          ! Otherwise, pciters will be decremented, and preconditioning will stop
          ! when pciters == 0

          ! If pcsetup < 0, the preconditioner will be set up every time.
          ! If pcsetup == 0, it will only get set up once.
          ! Otherwise, set it up every pcsetup iterations

          ! Mark pc as setup. Must be done here so NOPC is still marked as setup
          solver%isPcSetup = .TRUE.
          SELECT CASE(pcTypeName)
          CASE('NOPC')
            solver%PCTypeName='NOPC'
            RETURN
          CASE('DEFAULT')
            ALLOCATE(DistributedJacobi_PreCondType :: solver%PreCondType)
            solver%PCTypeName='DISTR_JACOBI'
          CASE('ILU')
            ALLOCATE(ILU_PreCondtype :: solver%PreCondType)
            solver%PCTypeName='ILU'
#ifdef HAVE_MPI
          CASE('DISTR_RSOR')
            ALLOCATE(DistributedRSOR_PreCondType :: solver%PreCondType)
            solver%PCTypeName='DISTR_RSOR'
          CASE('DISTR_JACOBI')
            ALLOCATE(DistributedJacobi_PreCondType :: solver%PreCondType)
            solver%PCTypeName='DISTR_JACOBI'
#endif
          ENDSELECT

          IF (solver%TPLType /= PETSC) THEN
            CALL solver%PreCondType%init(solver%A,solver%pcParams)
            CALL solver%PreCondType%setup()
          ENDIF
        ELSE
          IF (solver%TPLType /= PETSC) CALL solver%PreCondType%setup()
        ENDIF
        ! If pcsetup was 0, we've successfully set up the PC
        ! Make it huge so it is never set up again.
        IF (solver%pcsetup == 0) solver%pcsetup = HUGE(0)
      ELSE
        CALL eLinearSolverType%raiseError('Incorrect input to'//modName//'::'//myName// &
            ' - LinearSolverType matrix is not initialized. Preconditioner cannot be set up.')
      ENDIF
    ELSE
      CALL eLinearSolverType%raiseError('Incorrect input to'//modName//'::'//myName// &
          ' - LinearSolverType matrix is not allocated. Preconditioner cannot be set up.')
    ENDIF
  ELSE
    CALL eLinearSolverType%raiseError('Incorrect input to'//modName//'::'//myName// &
        ' - LinearSolverType is not initialized. Preconditioner cannot be set up.')
  ENDIF
ENDSUBROUTINE setup_PreCond_LinearSolverType_Iterative
!
!-------------------------------------------------------------------------------
!> @brief associates matrix with KSP (if PETSc), otherwise indicates that the
!>        matrix is not decomposed and needs to be refactored (for LU)
!> @param solver The linear solver to act on
!>
SUBROUTINE updatedA(solver)
  CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver
#ifdef FUTILITY_HAVE_PETSC
  PetscErrorCode  :: iperr
#endif

#ifdef FUTILITY_HAVE_PETSC
  SELECTTYPE(A=>solver%A); TYPE IS(PETScMatrixType)
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=5))
    CALL KSPSetOperators(solver%ksp,A%a,A%a,iperr)
#else
    CALL KSPSetOperators(solver%ksp,A%a,A%a, &
        DIFFERENT_NONZERO_PATTERN,iperr)
#endif
  ENDSELECT
#endif
  solver%isDecomposed=.FALSE.

ENDSUBROUTINE updatedA
!
!-------------------------------------------------------------------------------
!> @brief Method to compute/access residual vector and iteration count of system
!> @param solver The linear solver to act on
!> @param resid  The residual vector to return
!> @param nIters Number of iterations to return
!>
SUBROUTINE getResidualVec_LinearSolverType_Direct(solver,resid,nIters)
  CHARACTER(LEN=*),PARAMETER :: myName='getResidualVec_LinearSolverType_Direct'
  CLASS(LinearSolverType_Direct),INTENT(INOUT) :: solver
  CLASS(VectorType),INTENT(INOUT) :: resid
  INTEGER(SIK),INTENT(OUT),OPTIONAL :: nIters

  REQUIRE(solver%isInit)
  REQUIRE(resid%isInit)
  REQUIRE(ASSOCIATED(solver%b))
  REQUIRE(solver%b%isInit)
  REQUIRE(ASSOCIATED(solver%A))
  REQUIRE(solver%A%isInit)
  REQUIRE(ASSOCIATED(solver%x))
  REQUIRE(solver%x%isInit)

  IF (resid%isInit .AND. solver%isInit) THEN
    CALL BLAS_copy(solver%b,resid)
    CALL BLAS_matvec(thisMatrix=solver%A,alpha=-1.0_SRK,x=solver%x,beta=1.0_SRK,y=resid)
  ELSE
    CALL eLinearSolverType%raiseError('Incorrect call to '// &
       modName//'::'//myName//' - Residual or solver not initialized.')
  ENDIF

  ! Direct method; nIters has no meaning
  IF (PRESENT(nIters)) nIters = 0
ENDSUBROUTINE getResidualVec_LinearSolverType_Direct
!
!-------------------------------------------------------------------------------
!> @brief Method to compute/access residual norm and iteration count of system
!> @param solver The linear solver to act on
!> @param resid  The residual norm (scalar) to return
!> @param nIters Number of iterations to return
!>
SUBROUTINE getResidualNorm_LinearSolverType_Direct(solver,resid,nIters)
  CLASS(LinearSolverType_Direct),INTENT(INOUT) :: solver
  REAL(SRK),INTENT(OUT) :: resid
  INTEGER(SIK),INTENT(OUT),OPTIONAL :: nIters
  CLASS(VectorType),ALLOCATABLE :: u

  REQUIRE(solver%isInit)
  REQUIRE(ASSOCIATED(solver%b))
  REQUIRE(solver%b%isInit)
  REQUIRE(ASSOCIATED(solver%A))
  REQUIRE(solver%A%isInit)
  REQUIRE(ASSOCIATED(solver%x))
  REQUIRE(solver%x%isInit)

  CALL getResidualVec_LinearSolverType_Direct(solver,u)
  resid = BLAS_nrm2(u)

  ! Direct method; nIters has no meaning
  IF (PRESENT(nIters)) nIters = 0
ENDSUBROUTINE getResidualNorm_LinearSolverType_Direct
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
  solver%hasX=.FALSE.
  solver%info=0
  CALL solver%MPIparallelEnv%clear()
  CALL solver%OMPparallelEnv%clear()
#ifdef HAVE_PARDISO
  IF(ALLOCATED(solver%perm))  DEALLOCATE(solver%perm)
  IF(ALLOCATED(solver%iparm)) DEALLOCATE(solver%iparm)
  IF(ALLOCATED(solver%pt))    DEALLOCATE(solver%pt)
  solver%phase=-1
#endif
  IF(ASSOCIATED(solver%A)) THEN
    CALL solver%A%clear()
    DEALLOCATE(solver%A)
    NULLIFY(solver%A)
  ENDIF
  IF(ASSOCIATED(solver%X)) THEN
    CALL solver%X%clear()
    DEALLOCATE(solver%X)
    NULLIFY(solver%X)
  ENDIF
  IF(ASSOCIATED(solver%b)) THEN
    CALL solver%b%clear()
    DEALLOCATE(solver%b)
    NULLIFY(solver%b)
  ENDIF
  IF(ALLOCATED(solver%IPIV)) CALL demallocA(solver%IPIV)
  IF(ALLOCATED(solver%M)) THEN
    CALL solver%M%clear()
    DEALLOCATE(solver%M)
  ENDIF
  CALL solver%SolveTime%ResetTimer()
  solver%isDecomposed=.FALSE.
  IF(LinearSolverType_Paramsflag) CALL LinearSolverType_Clear_ValidParams()
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
#ifdef FUTILITY_HAVE_PETSC
  PetscErrorCode  :: ierr
  IF(solver%TPLType==PETSC .AND. solver%isInit) &
      CALL KSPDestroy(solver%ksp,ierr)
#endif

  CALL solver%MPIparallelEnv%clear()
  CALL solver%OMPparallelEnv%clear()
  IF(ASSOCIATED(solver%X)) THEN
    CALL solver%X%clear()
    DEALLOCATE(solver%X)
    NULLIFY(solver%X)
  ENDIF
  IF(ASSOCIATED(solver%b)) THEN
    CALL solver%b%clear()
    DEALLOCATE(solver%b)
    NULLIFY(solver%b)
  ENDIF
  IF(ALLOCATED(solver%PreCondType)) THEN
    CALL solver%PreCondType%clear()
    DEALLOCATE(solver%PrecondType)
  ENDIF
  IF(ASSOCIATED(solver%A)) THEN
    CALL solver%A%clear()
    DEALLOCATE(solver%A)
    NULLIFY(solver%A)
  ENDIF
  IF(ALLOCATED(solver%M)) THEN
    CALL solver%M%clear()
    DEALLOCATE(solver%M)
  ENDIF
  CALL solver%pcParams%clear()

  solver%isInit=.FALSE.
  solver%solverMethod=-1
  solver%hasX=.FALSE.
  solver%info=0
  solver%PCTypeName=''

  CALL solver%SolveTime%ResetTimer()
  solver%isDecomposed=.FALSE.
  solver%hasX0=.FALSE.
  solver%normType=-1
  solver%maxIters=-1
  solver%iters=0
  solver%relConvTol=0._SRK
  solver%absConvTol=0._SRK
  solver%residual=0._SRK
  solver%isPcSetup=.FALSE.
  IF(LinearSolverType_Paramsflag) CALL LinearSolverType_Clear_ValidParams()
ENDSUBROUTINE clear_LinearSolverType_Iterative
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
#ifdef HAVE_PARDISO
  INTEGER(SIK) :: msglvl=0,nrhs=1,maxfct=1,mnum=1,error=0
#endif
#ifdef FUTILITY_HAVE_PETSC
  PetscErrorCode :: iperr
#endif
  CALL solve_checkInput(solver)
  IF(solver%info == 0) THEN
    solver%info=-1
    CALL solver%SolveTime%tic()
    SELECTCASE(solver%solverMethod)
    CASE(GE)
      SELECTTYPE(A => solver%A)
      TYPE IS(DenseSquareMatrixType)
        CALL solveGE_DenseSquare(solver)

      TYPE IS(TriDiagMatrixType)
        IF(.NOT.solver%isDecomposed) &
            CALL DecomposePLU_TriDiag(solver)
        CALL solvePLU_TriDiag(solver)

      CLASS DEFAULT
        IF(solver%TPLtype==PARDISO_MKL) THEN
#ifdef HAVE_PARDISO
          SELECTTYPE(A => solver%A); TYPE IS(SparseMatrixType)
            SELECTTYPE(x => solver%x); TYPE IS(RealVectorType)
              SELECTTYPE(b => solver%b); TYPE IS(RealVectorType)
                CALL PARDISO(solver%pt,maxfct,mnum,solver%mtype, &
                    solver%phase,A%n,A%a,A%ia,A%ja,solver%perm,nrhs, &
                    solver%iparm,msglvl,b%b,x%b,error)
                 solver%phase=23
              ENDSELECT
            ENDSELECT
          ENDSELECT
#else
          CALL eLinearSolverType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - PARDISO not enabled.')
#endif
        ELSE
          !Should not use direct method, go to CGNR
          CALL solveCGNR(solver)
          IF(solver%info == 0) &
              CALL eLinearSolverType%raiseDebug(modName//'::'// &
              myName//'- GE method for dense rectangular system '// &
              'and sparse system is not implemented, CGNR method '// &
              'is used instead.')
        ENDIF
      ENDSELECT
    CASE(LU)
      SELECTTYPE(A => solver%A)
      TYPE IS(DenseSquareMatrixType)
        IF(.NOT. solver%isDecomposed) CALL DecomposePLU_DenseSquare(solver)
        CALL solvePLU_DenseSquare(solver)

      TYPE IS(TriDiagMatrixType)
        IF(.NOT.solver%isDecomposed) CALL DecomposePLU_TriDiag(solver)
        CALL solvePLU_TriDiag(solver)
#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScMatrixType)
        IF(solver%TPLtype==PETSC) THEN
          ! assemble matrix if necessary
          IF(.NOT.(A%isAssembled)) CALL A%assemble()

          ! assemble source vector if necessary
          SELECTTYPE(b=>solver%b); TYPE IS(PETScVectorType)
            IF(.NOT.(b%isAssembled)) CALL b%assemble()
          ENDSELECT

          ! assemble solution vector if necessary
          SELECTTYPE(X=>solver%X); TYPE IS(PETScVectorType)
            IF(.NOT.(X%isAssembled)) CALL X%assemble()
          ENDSELECT

          SELECTTYPE(A => solver%A); TYPE IS(PETScMatrixType)
            SELECTTYPE(x => solver%x); TYPE IS(PETScVectorType)
              SELECTTYPE(b => solver%b); TYPE IS(PETScVectorType)
                CALL KSPSolve(solver%ksp,b%b,x%b,iperr)
              ENDSELECT
            ENDSELECT
          ENDSELECT
        ENDIF
#endif
      CLASS DEFAULT
        IF(solver%TPLtype==PARDISO_MKL) THEN
#ifdef HAVE_PARDISO
          SELECTTYPE(A => solver%A); TYPE IS(SparseMatrixType)
            SELECTTYPE(x => solver%x); TYPE IS(RealVectorType)
              SELECTTYPE(b => solver%b); TYPE IS(RealVectorType)
                CALL PARDISO(solver%pt,maxfct,mnum,solver%mtype, &
                    solver%phase,A%n,A%a,A%ia,A%ja,solver%perm,nrhs, &
                    solver%iparm,msglvl,b%b,x%b,error)
                 solver%phase=23
              ENDSELECT
            ENDSELECT
          ENDSELECT
#else
          CALL eLinearSolverType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - PARDISO not enabled.')
#endif
        ELSE
          !Should not use direct method, go to CGNR
          CALL solveCGNR(solver)
          IF(solver%info == 0) &
              CALL eLinearSolverType%raiseDebug(modName//'::'// &
              myName//'- LU method for dense rectangular system '// &
              'and sparse system is not implemented, CGNR method '// &
              'is used instead.')
        ENDIF
      ENDSELECT
    CASE(QR)
      SELECTTYPE(A => solver%A)
      TYPE IS(DenseRectMatrixType)
        CALL solveQR_Dense(solver)

      CLASS DEFAULT
        CALL eLinearSolverType%raiseError('Incorrect call to '// &
            modName//'::'//myName//' - QR only supported for DensRet Matrix.')
      ENDSELECT
    ENDSELECT
    CALL solver%SolveTime%toc()
  ENDIF
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
#ifdef FUTILITY_HAVE_PETSC
  PetscErrorCode  :: ierr
#endif
  CALL solve_checkInput(solver)
  IF(solver%info == 0) THEN
    IF(.NOT. solver%hasX0) THEN
      SELECTTYPE(X => solver%X); CLASS IS(NativeVectorType)
        SELECTTYPE(b => solver%b); CLASS IS(NativeVectorType)
          x%b(:) = b%b(:)
          CALL eLinearSolverType%raiseDebug(modName//'::'// &
              myName//'- Initial X0 is set to b.')
        CLASS DEFAULT
          x%b(:) = 1.0_SRK
          CALL eLinearSolverType%raiseDebug(modName//'::'// &
              myName//'- Initial X0 is set to 1.')
        ENDSELECT
      ENDSELECT
      solver%hasX0=.TRUE.
    ENDIF
    CALL solver%SolveTime%tic()
    solver%info=-1
    SELECTCASE(solver%solverMethod)
    CASE(BICGSTAB)
      !need two type structures to deal with DenseRectMatrixType
      SELECTTYPE(A=>solver%A)
      TYPE IS(DenseSquareMatrixType)
        IF(.NOT. solver%isDecomposed) &
            CALL DecomposeBiCGSTAB_DenseSquare(solver)
        CALL solveBiCGSTAB(solver)

      TYPE IS(SparseMatrixType)
        !CALL DecomposeILU_Sparse(solver)
        CALL solveBiCGSTAB(solver)

      TYPE IS(TriDiagMatrixType)
        !If the coefficient matrix is a tridiagonal matrix, PLU method
        !will be used instead.
        IF(.NOT. solver%isDecomposed) CALL DecomposePLU_TriDiag(solver)
        CALL solvePLU_TriDiag(solver)

        IF(solver%info == 0) &
            CALL eLinearSolverType%raiseDebug(modName//'::'// &
            myName//'- BiCGSTAB method for tridiagonal system '// &
            'is not implemented, GE method is used instead.')

      TYPE IS(DenseRectMatrixType)
        !If the coefficient matrix is a rectangular matrix, CGNR method
        !will be used instead.
        CALL solveCGNR(solver)

        IF(solver%info == 0) &
            CALL eLinearSolverType%raiseDebug(modName//'::'// &
            myName//'- BiCGSTAB method for dense rectangular system '// &
            'is not implemented, CGNR method is used instead.')

#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScMatrixType)
        ! assemble matrix if necessary
        IF(.NOT.(A%isAssembled)) CALL A%assemble()

        ! assemble source vector if necessary
        SELECTTYPE(b=>solver%b); TYPE IS(PETScVectorType)
          IF(.NOT.(b%isAssembled)) CALL b%assemble()
        ENDSELECT

        ! assemble solution vector if necessary
        SELECTTYPE(X=>solver%X); TYPE IS(PETScVectorType)
          IF(.NOT.(X%isAssembled)) CALL X%assemble()
        ENDSELECT

        ! solve
        SELECTTYPE(b=>solver%b); TYPE IS(PETScVectorType)
          SELECTTYPE(X=>solver%X); TYPE IS(PETScVectorType)
            CALL KSPSolve(solver%ksp,b%b,x%b,ierr)
            IF(ierr==0) solver%info=0
          ENDSELECT
        ENDSELECT
#endif
      ENDSELECT
    CASE(CGNR)
      SELECTTYPE(A=>solver%A)
      TYPE IS(TriDiagMatrixType)
        !If the coefficient matrix is tridiagonal PLU method will be
        !used instead.
        IF(.NOT.solver%isDecomposed) &
            CALL DecomposePLU_TriDiag(solver)
        CALL solvePLU_TriDiag(solver)

        IF(solver%info == 0) &
            CALL eLinearSolverType%raiseDebug(modName//'::'// &
            myName//'- CGNR method for tridiagonal system '// &
            'is not implemented, PLU method is used instead.')
      TYPE IS(SparseMatrixType)
        CALL solveBiCGSTAB(solver)
        IF(solver%info == 0) &
            CALL eLinearSolverType%raiseDebug(modName//'::'// &
            myName//'- CGNR method for sparse system '// &
            'is not implemented, BiCGSTAB method is used instead.')

#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScMatrixType)
        ! assemble matrix if necessary
        IF(.NOT.(A%isAssembled)) CALL A%assemble()

        ! assemble source vector if necessary
        SELECTTYPE(b=>solver%b); TYPE IS(PETScVectorType)
          IF(.NOT.(b%isAssembled)) CALL b%assemble()
        ENDSELECT

        ! assemble solution vector if necessary
        SELECTTYPE(X=>solver%X); TYPE IS(PETScVectorType)
          IF(.NOT.(X%isAssembled)) CALL X%assemble()
        ENDSELECT

        ! solve
        SELECTTYPE(b=>solver%b); TYPE IS(PETScVectorType)
          SELECTTYPE(X=>solver%X); TYPE IS(PETScVectorType)
            CALL KSPSolve(solver%ksp,b%b,x%b,ierr)
            IF(ierr==0) solver%info=0
          ENDSELECT
        ENDSELECT
#endif
      CLASS DEFAULT
        CALL solveCGNR(solver)

      ENDSELECT

    CASE(GMRES)
      SELECTTYPE(A=>solver%A)
      TYPE IS(TriDiagMatrixType)
        !If the coefficient matrix is tridiagonal PLU method will be
        !used instead.
        IF(.NOT.solver%isDecomposed) &
            CALL DecomposePLU_TriDiag(solver)
        CALL solvePLU_TriDiag(solver)

        IF(solver%info == 0) &
            CALL eLinearSolverType%raiseDebug(modName//'::'// &
            myName//'- GMRES method for tridiagonal system '// &
            'is not implemented, PLU method is used instead.')
      TYPE IS(DenseRectMatrixType)
        !If the coefficient matrix is a rectangular matrix, CGNR method
        !will be used instead.
        CALL solveCGNR(solver)

        IF(solver%info == 0) &
            CALL eLinearSolverType%raiseDebug(modName//'::'// &
            myName//'- GMRES method for dense rectangular system '// &
            'is not implemented, CGNR method is used instead.')

#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScMatrixType)
        ! assemble matrix if necessary
        IF(.NOT.(A%isAssembled)) CALL A%assemble()

        ! assemble source vector if necessary
        SELECTTYPE(b=>solver%b); TYPE IS(PETScVectorType)
          IF(.NOT.(b%isAssembled)) CALL b%assemble()
        ENDSELECT

        ! assemble solution vector if necessary
        SELECTTYPE(X=>solver%X); TYPE IS(PETScVectorType)
          IF(.NOT.(X%isAssembled)) CALL X%assemble()
        ENDSELECT

        ! solve
        SELECTTYPE(b=>solver%b); TYPE IS(PETScVectorType)
          SELECTTYPE(X=>solver%X); TYPE IS(PETScVectorType)
            CALL KSPSolve(solver%ksp,b%b,x%b,ierr)

            IF(ierr==0) solver%info=0
          ENDSELECT
        ENDSELECT
#endif
#ifdef FUTILITY_HAVE_Trilinos
      TYPE IS(TrilinosMatrixType)
        ! assemble matrix if necessary
        IF(.NOT.(A%isAssembled)) CALL A%assemble()

        IF(solver%belos_pc_set) CALL Preconditioner_Setup(solver%Belos_pc,A%A)
        solver%belos_pc_set=.FALSE.

        ! assemble source vector if necessary
        SELECTTYPE(b=>solver%b); TYPE IS(TrilinosVectorType)
          IF(.NOT.(b%isAssembled)) CALL b%assemble()
        ENDSELECT

        ! assemble solution vector if necessary
        SELECTTYPE(X=>solver%X); TYPE IS(TrilinosVectorType)
          IF(.NOT.(X%isAssembled)) CALL X%assemble()
        ENDSELECT

        ! solve
        CALL Belos_solve(solver%Belos_solver)
#endif
      CLASS DEFAULT
        ! Ensure setupPrecond has been called
        IF (.NOT. solver%isPcSetup) CALL solver%setupPC()
        IF(solver%PCTypeName /= 'NOPC' .AND. solver%pciters/=0) THEN
          ! Decrementing handled by calling function
          CALL solveGMRES(solver,solver%PreCondType)
        ELSE
          CALL solveGMRES(solver)
        ENDIF
      ENDSELECT
    CASE(MULTIGRID)
#ifdef FUTILITY_HAVE_PETSC
      SELECTTYPE(A=>solver%A); TYPE IS(PETScMatrixType)
        ! assemble matrix if necessary
        IF(.NOT.(A%isAssembled)) CALL A%assemble()
        SELECTTYPE(b=>solver%b); TYPE IS(PETScVectorType)
          ! assemble source vector if necessary
          IF(.NOT.(b%isAssembled)) CALL b%assemble()
          SELECTTYPE(X=>solver%X); TYPE IS(PETScVectorType)
            ! assemble solution vector if necessary
            IF(.NOT.(X%isAssembled)) CALL X%assemble()
            ! solve
            CALL KSPSolve(solver%ksp,b%b,x%b,ierr)
            IF(ierr==0) solver%info=0
          ENDSELECT
        ENDSELECT
      ENDSELECT
#endif
    ENDSELECT
    CALL solver%SolveTime%toc()
  ENDIF
ENDSUBROUTINE solve_LinearSolverType_Iterative
!
!-------------------------------------------------------------------------------
!> @brief Check the information before solving.
!> @param solver The linear solver to act on
!>
!> The matrix and vectors are operated outside of linear solver object, so they
!> need to checked before solving. This subroutine checks these information, and
!> it will be used for both direct solve subroutine and iterative subroutine.
!>
SUBROUTINE solve_checkInput(solver)
  CHARACTER(LEN=*),PARAMETER :: myName='solve_checkInput'
  CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver

  solver%info=-1
  IF(.NOT. solver%isInit) THEN
    CALL eLinearSolverType%raiseError(ModName//'::'//myName// &
        '  - Linear solver object has not been initialized!')
    RETURN
  ENDIF
  IF(.NOT. ASSOCIATED(solver%A)) THEN
    CALL eLinearSolverType%raiseError(ModName//'::'//myName// &
        '  - The matrix A has not been associated!')
    RETURN
  ENDIF
  IF(.NOT. ASSOCIATED(solver%X)) THEN
    CALL eLinearSolverType%raiseError(ModName//'::'//myName// &
        '  - The unknowns X has not been associated!')
    RETURN
  ENDIF
  IF(.NOT. ASSOCIATED(solver%b)) THEN
    CALL eLinearSolverType%raiseError(ModName//'::'//myName// &
        '  - The right hand side has not been set!')
    RETURN
  ENDIF

  SELECTTYPE(A=>solver%A)
  TYPE IS(DenseRectMatrixType)
    IF(A%n /= solver%b%n .OR. A%m /= solver%X%n &
        .OR. A%n < 1 .OR. A%m < 1) THEN
        CALL eLinearSolverType%raiseError(ModName//'::'//myName// &
        '  - The size of the matrix and vector do not conform!')
    ELSE
      solver%info=0
    ENDIF
  CLASS DEFAULT
    IF(A%n /= solver%b%n .OR. A%n /= solver%X%n &
        .OR. A%n < 1) THEN
        CALL eLinearSolverType%raiseError(ModName//'::'//myName// &
        '  - The size of the matrix and vector do not conform!')
    ELSE
      solver%info=0
    ENDIF
  ENDSELECT
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
  INTEGER(SIK) :: i

  IF(solver%isInit) THEN
    SELECTTYPE(X => solver%X)
    CLASS IS(NativeVectorType)
      X%b=X0
    CLASS IS(DistributedVectorType)
      DO i=1,solver%X%n
        CALL X%set(i,X0(i))
      ENDDO
      CALL X%assemble()
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
!> @param nRestart The number of iterations before GMRES restarts
!>
!> This subroutine sets the convergence criterion for the iterative solver.
!>
SUBROUTINE setConv_LinearSolverType_Iterative(solver,normType_in,  &
    relConvTol_in,absConvTol_in,maxIters_in,nRestart_in,dTol_in)
  CHARACTER(LEN=*),PARAMETER :: myName='setConv_LinearSolverType_Iterative'
  CLASS(LinearSolverType_Iterative),INTENT(INOUT) :: solver
  INTEGER(SIK),INTENT(IN) :: normType_in
  REAL(SRK),INTENT(IN) :: relConvTol_in
  REAL(SRK),INTENT(IN) :: absConvTol_in
  INTEGER(SIK),INTENT(IN) :: maxIters_in
  INTEGER(SIK),INTENT(IN),OPTIONAL :: nRestart_in
  REAL(SRK),INTENT(IN),OPTIONAL :: dTol_in
#ifdef FUTILITY_HAVE_PETSC
  PetscErrorCode  :: ierr
  PetscInt  :: maxits,nrst
  PetscReal :: rtol,abstol
  PetscReal :: dtol
#endif

  INTEGER(SIK) :: normType,maxIters,nRestart
  REAL(SRK) :: relConvTol,absConvTol

  !Input check
  normType=normType_in
  relConvTol=relConvTol_in
  absConvTol=absConvTol_in
  maxIters=maxIters_in
  nRestart=-1_SRK
  IF(PRESENT(nRestart_in)) nRestart=nRestart_in
#ifdef FUTILITY_HAVE_PETSC
  IF(PRESENT(dTol_in)) THEN
    dtol=dTol_in
  ELSE
    dtol=1E30_SRK
  ENDIF
#endif
  IF(normType <= -2) THEN
    CALL eLinearSolverType%raiseDebug(modName//'::'// &
        myName//' - Incorrect input, normType should not be less '// &
        'than -1. Default value is used!')
    normType=2
  ENDIF
  IF(relConvTol < 0._SRK .OR. relConvTol >= 1._SRK) THEN
    CALL eLinearSolverType%raiseDebug(modName//'::'// &
        myName//' - Incorrect input, relConvTol should be in '// &
        'the range of (0, 1). Default value is used!')
    relConvTol=0.001_SRK
  ENDIF
  IF(absConvTol < 0._SRK .OR. absConvTol >= 1._SRK) THEN
    CALL eLinearSolverType%raiseDebug(modName//'::'// &
        myName//' - Incorrect input, absConvTol should be in '// &
        'the range of (0, 1). Default value is used!')
    absConvTol=0.001_SRK
  ENDIF
  IF(maxIters <= 1) THEN
    CALL eLinearSolverType%raiseDebug(modName//'::'// &
        myName//' - Incorrect input, maxIters should not be less '// &
        'than or equal to 1. Default value is used!')
    maxIters=1000
  ENDIF
  IF(nRestart <= 1 .OR. .NOT.PRESENT(nRestart_in)) THEN
    CALL eLinearSolverType%raiseDebug(modName//'::'// &
        myName//' - Incorrect input, nRestart should not be less '// &
        'than or equal to 1. Default value is used!')
    nRestart=30
  ENDIF
  IF(solver%isInit) THEN
    solver%normType=normType
    solver%relConvTol=relConvTol
    solver%absConvTol=absConvTol
    solver%maxIters=maxIters
    solver%nRestart=nRestart

    IF(solver%TPLType == PETSC) THEN
#ifdef FUTILITY_HAVE_PETSC
      rtol=relConvTol
      abstol=absConvTol
      maxits=maxIters
      nrst=nRestart
      CALL KSPSetTolerances(solver%ksp,rtol,abstol,dtol,maxits,ierr)
      IF(PRESENT(nRestart_in)) CALL KSPGMRESSetRestart(solver%ksp,nrst,ierr)
#else
      CALL eLinearSolverType%raiseFatalError('Incorrect call to '// &
          modName//'::'//myName//' - PETSc not enabled.  You will'// &
          'need to recompile with PETSc enabled to use this feature.')
#endif
    ELSEIF(solver%TPLType == TRILINOS) THEN
#ifdef FUTILITY_HAVE_Trilinos
      CALL Belos_SetConvCrit(solver%Belos_solver,solver%absConvTol,solver%maxIters)
#else
      CALL eLinearSolverType%raiseFatalError('Incorrect call to '// &
          modName//'::'//myName//' - Trilinos not enabled.  You will'// &
          'need to recompile with Trilinos enabled to use this feature.')
#endif
    ENDIF
  ENDIF
ENDSUBROUTINE setConv_LinearSolverType_Iterative

!-------------------------------------------------------------------------------
!> @brief Gets the residual for the iterative solver
!> @param solver The linear solver to act on
!> @param resid  The residual vector
!> @param nIters The iteration count to return
!>
SUBROUTINE getResidualVec_LinearSolverType_Iterative(solver,resid,nIters)
  CHARACTER(LEN=*),PARAMETER :: myName='getResidualVec_LinearSolverType_Iterative'
  CLASS(LinearSolverType_Iterative),INTENT(INOUT) :: solver
  CLASS(VectorType),INTENT(INOUT) :: resid
  INTEGER(SIK),INTENT(OUT),OPTIONAL :: nIters
  !input check
  REQUIRE(resid%isInit)
  REQUIRE(solver%isInit)
  REQUIRE(ASSOCIATED(solver%b))
  REQUIRE(solver%b%isInit)
  REQUIRE(ASSOCIATED(solver%A))
  REQUIRE(solver%A%isInit)
  REQUIRE(ASSOCIATED(solver%x))
  REQUIRE(solver%x%isInit)
  REQUIRE(resid%isInit)
  !Written assuming A is not decomposed.  Which is accurate, the correct
  !solve function will contain the decomposed A.

  !perform calculations using the BLAS system (intrinsic to Futility or
  !TPL, defined by #HAVE_BLAS)
  ! MKL May need something special but is not implemented at the moment.
  IF (resid%isInit .AND. solver%isInit) THEN
    CALL BLAS_copy(solver%b,resid)
    CALL BLAS_matvec(thisMatrix=solver%A,alpha=-1.0_SRK,x=solver%x,beta=1.0_SRK,y=resid)
    IF (PRESENT(nIters)) nIters = solver%iters
  ELSE
    CALL eLinearSolverType%raiseError('Incorrect call to '// &
       modName//'::'//myName//' - Residual or solver not initialized.')
  ENDIF

ENDSUBROUTINE getResidualVec_LinearSolverType_Iterative
!
!-------------------------------------------------------------------------------
!> @brief Gets the residual norm for the iterative solver
!> @param solver The linear solver to act on
!> @param resid  The (preconditioned) residual norm
!> @param nIters The iteration count to return
!>
SUBROUTINE getResidualNorm_LinearSolverType_Iterative(solver,resid,nIters)
  CLASS(LinearSolverType_Iterative),INTENT(INOUT) :: solver
  REAL(SRK),INTENT(OUT) :: resid
  INTEGER(SIK),INTENT(OUT),OPTIONAL :: nIters
#ifdef FUTILITY_HAVE_PETSC
  INTEGER(SIK) :: ierr
#endif

  REQUIRE(solver%isInit)
  REQUIRE(ASSOCIATED(solver%b))
  REQUIRE(solver%b%isInit)
  REQUIRE(ASSOCIATED(solver%A))
  REQUIRE(solver%A%isInit)
  REQUIRE(ASSOCIATED(solver%x))
  REQUIRE(solver%x%isInit)

  IF(solver%TPLType == PETSC) THEN
#ifdef FUTILITY_HAVE_PETSC
    IF (PRESENT(nIters)) CALL KSPGetIterationNumber(solver%ksp,niters,ierr)
    CALL KSPGetResidualNorm(solver%ksp,resid,ierr)
#endif
  ELSEIF(solver%TPLType == TRILINOS) THEN
#ifdef FUTILITY_HAVE_Trilinos
    IF (PRESENT(nIters)) CALL Belos_GetIterationCount(solver%Belos_solver,niters)
    CALL Belos_GetResid(solver%Belos_solver,resid)
#endif
  ELSE
    IF (PRESENT(nIters)) nIters=solver%iters
    resid=solver%residual
  ENDIF
ENDSUBROUTINE getResidualNorm_LinearSolverType_Iterative
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
  CALL pList%add('MatrixType->n',solver%A%n)
  CALL pList%add('MatrixType->isSym',.FALSE.)
  CALL solver%M%init(pList)
  CALL pList%clear()
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
  REAL(SRK):: calpha,crho,comega,crhod,cbeta
  TYPE(RealVectorType) :: vr,vr0,vs,vv,vp,vy,vz,vt
  INTEGER(SIK) :: n,iterations
  TYPE(ParamType) :: pList

  n=0

  !Get the n value and set the parameter list
  CALL pList%add('VectorType -> n',solver%A%n)
  CALL vr%init(pList)
  CALL vr0%init(pList)
  CALL vs%init(pList)
  CALL vv%init(pList)
  CALL vp%init(pList)
  CALL vy%init(pList)
  CALL vz%init(pList)
  CALL vt%init(pList)
  CALL pList%clear()

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
      SELECTTYPE(M => solver%M)
      TYPE IS(DenseSquareMatrixType); CALL MinvMult_dense(M,vp%b,vy%b)
      !TYPE IS(SparseMatrixType); CALL MinvMult_Sparse(M,vp%b,vy%b)
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
    IF(BLAS_dot(vt,vt) /= 0) THEN
      comega=BLAS_dot(vs,vt)/BLAS_dot(vt,vt)
    ELSE
      comega=0._SRK
    ENDIF
    SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
      X%b=X%b+calpha*vy%b+comega*vz%b
    ENDSELECT
    vr%b=vs%b-comega*vt%b
    !get L_norm
    CALL LNorm(vr%b,solver%normType,solver%residual)
    !check convergence
    IF(solver%residual <= solver%absConvTol) EXIT
  ENDDO
  solver%iters=iterations
  solver%info=0
ENDSUBROUTINE solveBiCGSTAB

!
!-------------------------------------------------------------------------------
!> @brief Control program for restarted GMRES solver. Handles restarts,
!> parallelism, and a few other things. Solver body is located in
!> solveGMRES_partial
!> @param thisLS The linear solver to act on
!> @param thisPC The preconditioner object to use on the system
!>
!> This subroutine solves the Iterative Linear System using the restarted GMRES method
!>
SUBROUTINE solveGMRES(thisLS,thisPC)
  CLASS(LinearSolverType_Iterative),INTENT(INOUT) :: thisLS
  CLASS(PreconditionerType),INTENT(INOUT),OPTIONAL :: thisPC
  INTEGER(SIK) :: nIters,outerIt
  REAL(SRK) :: tol,norm_b
  CLASS(VectorType),ALLOCATABLE :: u

  IF (thisLS%nRestart > thisLS%A%n) thisLS%nRestart = thisLS%A%n
  thisLS%iters = 0
  norm_b = BLAS_nrm2(thisLS%b)
  IF (norm_b .APPROXEQ. 0.0_SRK) THEN
    CALL thisLS%X%set(0.0_SRK)
    thisLS%residual = 0.0_SRK
    thisLS%iters = 1
    RETURN
  ENDIF

  CALL VectorResemble(u,thisLS%x)
  CALL thisLS%getResidual(u)
  thisLS%residual = BLAS_nrm2(u)
  tol = MAX(thisLS%absConvtol,thisLS%relConvTol*thisLS%residual)
  IF (thisLS%residual .APPROXLE. tol) THEN
    thisLS%iters = 1
    RETURN
  ENDIF

  SELECT TYPE(u); CLASS IS(NativeVectorType)
    DO outerIt=1,CEILING(thisLS%maxIters*1.0/thisLS%nRestart)
      IF(PRESENT(thisPC)) THEN
        CALL solveGMRES_partial(thisLS,u,norm_b,tol,nIters,thisPC)
      ELSE
        CALL solveGMRES_partial(thisLS,u,norm_b,tol,nIters)
      ENDIF
      thisLS%iters = thisLS%iters + nIters
      IF (thisLS%residual .APPROXLE. tol) THEN
        EXIT
      ENDIF
      CALL thisLS%getResidual(u)
      IF(PRESENT(thisPC)) CALL thisPC%apply(u)
      thisLS%residual = BLAS_nrm2(u)
    ENDDO
  CLASS DEFAULT
    CALL eLinearSolverType%raiseError('Incorrect call to '// &
       modName//'::solveGMRES'//' - Native solver does not support this vector type.')
  ENDSELECT
  CALL u%clear()
  DEALLOCATE(u)
ENDSUBROUTINE solveGMRES

!
!-------------------------------------------------------------------------------
!> @brief GMRES core solver routine. No restart logic.
!> @param thisLS The linear solver to act on
!> @param u      The solution vector to work with
!> @param norm_b Norm of RHS, passed as argument so it doesn't need to be
!         recomputed
!> @param tol    the most restrictive tolerance
!> @param nIters the running iteration count
!> @param thisPC The preconditioner object to use on the system
!>
SUBROUTINE solveGMRES_partial(thisLS,u,norm_b,tol,nIters,thisPC)
  CLASS(LinearSolverType_Iterative),INTENT(INOUT) :: thisLS
  CLASS(NativeVectorType),INTENT(INOUT) :: u
  REAL(SRK),INTENT(IN) :: norm_b
  REAL(SRK),INTENT(IN) :: tol
  INTEGER(SIK),INTENT(OUT) :: nIters
  CLASS(PreconditionerType),INTENT(INOUT),OPTIONAL :: thisPC

  ! Array of orthogonal basis vectors
  CLASS(NativeVectorType),ALLOCATABLE :: V(:)
  CLASS(VectorType),ALLOCATABLE :: Vy ! Generic vector container
  REAL(SRK),ALLOCATABLE :: R(:,:) ! Array of basis vector coeffs for sol.
  REAL(SRK),ALLOCATABLE :: givens_sin(:),givens_cos(:),f(:)
  TYPE(ParamType) :: vecPlist
  REAL(SRK) :: divTmp,temp
  INTEGER(SIK) :: krylovIdx,i
  INTEGER(SIK),ALLOCATABLE :: orthogReq(:)

  CALL vecPlist%clear()
  CALL VectorResemble(Vy,thisLS%x,vecPlist)
  CALL vecPlist%clear()

  SELECT TYPE(x => thisLS%X)
  TYPE IS(RealVectorType)
    CALL vecPlist%add('VectorType -> n',thisLS%A%n)
#ifdef HAVE_MPI
  TYPE IS(NativeDistributedVectorType)
    CALL vecPlist%add('VectorType -> n',thisLS%A%n)
    CALL vecPlist%add('VectorType -> MPI_Comm_ID',thisLS%MPIparallelEnv%comm)
    CALL vecPlist%add('VectorType -> chunkSize',x%chunkSize)
    CALL vecPlist%add('VectorType -> nlocal',SIZE(x%b))
#endif
  CLASS DEFAULT
    CALL eLinearSolverType%raiseError('Incorrect call to '// &
       modName//'::solveGMRES_partial'//' - Native solver does not support this vector type.')
  ENDSELECT

  ! Allocate Data storage arrays
  SELECT TYPE(x => thisLS%X)
  TYPE IS(RealVectorType)
    ALLOCATE(RealVectorType :: V(thisLS%nRestart+1))
#ifdef HAVE_MPI
  TYPE IS(NativeDistributedVectorType)
    ALLOCATE(NativeDistributedVectorType :: V(thisLS%nRestart+1))
#endif
  ENDSELECT

  ALLOCATE(R(thisLS%nRestart+1,thisLS%nRestart))
  ALLOCATE(givens_cos(thisLS%nRestart))
  ALLOCATE(givens_sin(thisLS%nRestart))
  ALLOCATE(f(thisLS%nRestart+1))
  ALLOCATE(orthogReq(thisLS%nRestart))

  ! Initialize relevant quantities (u assumed to contain vector of precond. resid
  divTmp = 1.0_SRK/thisLS%residual
  R(:,:) = 0.0_SRK
  givens_sin = 0.0_SRK
  givens_cos = 0.0_SRK
  CALL V(1)%init(vecPlist)
  V(1)%b = u%b*divTmp
  f(1) = thisLS%residual

  DO krylovIdx = 1,thisLS%nRestart
    ! Use next space of V as temporary storage:
    CALL V(krylovIdx+1)%init(vecPlist)

    ! Apply PC in-place and multiply in to u for orthogonalization
    IF (PRESENT(thisPC)) THEN
      u%b(:) = V(krylovIdx)%b(:)
      CALL thisPC%apply(u)
      CALL BLAS_matvec(THISMATRIX=thisLS%A,X=u,y=V(krylovIdx+1),BETA=0.0_SRK)
    ELSE
      CALL BLAS_matvec(THISMATRIX=thisLS%A,X=V(krylovIdx),Y=V(krylovIdx+1),BETA=0.0_SRK)
    ENDIF

    ! Create orthogonal basis
    CALL arnoldi(thisLS,V,krylovIdx,R(1:krylovIdx+1,krylovIdx),orthogReq)

    ! Perform Givens rotation
    DO i=1,krylovIdx-1
      temp = givens_cos(i)*R(i,krylovIdx) + givens_sin(i)*R(i+1,krylovIdx)
      R(i+1,krylovIdx) = givens_cos(i)*R(i+1,krylovIdx) - givens_sin(i)*R(i,krylovIdx)
      R(i,krylovIdx) = temp
    ENDDO

    IF (R(krylovIdx,krylovIdx) .APPROXLE. 0.0_SRK) THEN
      givens_cos(krylovIdx) = 0.0_SRK
      givens_sin(krylovIdx) = 1.0_SRK
    ELSE
      givens_cos(krylovIdx) = ABS(R(krylovIdx,krylovIdx))/ &
          SQRT(R(krylovIdx,krylovIdx)*R(krylovIdx,krylovIdx) &
          + R(krylovIdx+1,krylovIdx)*R(krylovIdx+1,krylovIdx))
      givens_sin(krylovIdx) = givens_cos(krylovIdx)*R(krylovIdx+1,krylovIdx)/ &
          R(krylovIdx,krylovIdx)
    ENDIF
    R(krylovIdx,krylovIdx) = givens_cos(krylovIdx)*R(krylovIdx,krylovIdx) &
        + givens_sin(krylovIdx)*R(krylovIdx+1,krylovIdx)

    f(krylovIdx+1) = -givens_sin(krylovIdx)*f(krylovIdx)
    f(krylovIdx)   =  givens_cos(krylovIdx)*f(krylovIdx)

    IF ((ABS(f(krylovIdx+1)) .APPROXLE. tol*norm_b) .OR. (krylovIdx == thisLS%nRestart)) THEN
      CALL BLAS_matvec('U','N','N',R(1:krylovIdx,1:krylovIdx),f(1:krylovIdx))
      CALL Vy%set(0.0_SRK)
      DO i=1,krylovIdx
        CALL BLAS_axpy(V(i),Vy,f(i))
      ENDDO
      IF (PRESENT(thisPC)) CALL thisPC%apply(Vy)
      CALL BLAS_axpy(Vy,thisLS%x)
      EXIT
    ENDIF
  ENDDO

  thisLS%residual = ABS(f(krylovIdx+1)/norm_b)
  nIters = krylovIdx

  DEALLOCATE(R)
  DEALLOCATE(givens_cos)
  DEALLOCATE(givens_sin)
  DEALLOCATE(f)
  DO i=1,SIZE(V)
    IF (V(i)%isInit) CALL V(i)%clear()
  ENDDO
  DEALLOCATE(V)
  CALL Vy%clear()
  DEALLOCATE(Vy)
  DEALLOCATE(orthogReq)

ENDSUBROUTINE solveGMRES_partial

!
!-------------------------------------------------------------------------------
!> @brief Helper routine for GMRES. Performs Arnoldi iteration by by
!         orthogonalizing the next krylov-space vector
!> @param thisLS The linear solver to act on
!> @param V      Orthogonal basis of the Krylov space
!> @param k      The current basis index
!> @param h      Entries of the upper triangular R-matrix
!> @param orthogReq MPI requests for distributed dot product
!>
SUBROUTINE arnoldi(thisLS,V,k,h,orthogReq)
  CLASS(LinearSolverType_Iterative) thisLS
  CLASS(NativeVectorType),INTENT(INOUT) :: V(:)
  INTEGER(SIK),INTENT(IN) :: k
  REAL(SRK),INTENT(INOUT) :: h(:)
  INTEGER(SIK),ALLOCATABLE,INTENT(IN) :: orthogReq(:)
  INTEGER(SIK) :: orthogIdx
#ifdef HAVE_MPI
  INTEGER(SIK) :: mpierr
#endif

  ! Perform distributed dot, masking communication
  DO orthogIdx=1,k
    h(orthogIdx) = BLAS_dot(V(orthogIdx)%b,V(k+1)%b)
#ifdef HAVE_MPI
    CALL MPI_IAllReduce(MPI_IN_PLACE,h(orthogIdx),1,MPI_DOUBLE_PRECISION, &
        MPI_SUM,thisLS%MPIParallelEnv%comm,orthogReq(orthogIdx),mpierr)
#endif
  ENDDO
  ! Subtract vector components
  DO orthogIdx=1,k
#ifdef HAVE_MPI
    CALL MPI_Wait(orthogReq(orthogIdx),MPI_STATUS_IGNORE,mpierr)
#endif
    CALL BLAS_axpy(V(orthogIdx),V(k+1),-h(orthogIdx))
  ENDDO
  h(k+1)=BLAS_nrm2(V(k+1))
  V(k+1)%b = V(k+1)%b/h(k+1)
ENDSUBROUTINE arnoldi

!
!-------------------------------------------------------------------------------
!> @brief Factorizes a sparse solver%A with ILU method and stores this in
!>  solver%M
!> @param solver The linear solver object
!>
!> This subroutine factorizes A with ILU method and stores result in solver%M
!>
SUBROUTINE DecomposeILU_Sparse(solver)
  CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver

  INTEGER(SIK) :: i,j,ik,k,kk,ij,kj,j2
  INTEGER(SIK) :: uptr(solver%A%n)
  REAL(SRK) :: m_val

  IF(.NOT. ALLOCATED(solver%M)) THEN
    ALLOCATE(SparseMatrixType :: solver%M)
  ENDIF

  SELECTTYPE(M => solver%M); TYPE IS(SparseMatrixType)
    SELECTTYPE(A => solver%A); TYPE IS(SparseMatrixType)
      M=A
    ENDSELECT
  ENDSELECT

  solver%info=-1
  solver%isDecomposed=.FALSE.
  SELECTTYPE(M => solver%M); TYPE IS(SparseMatrixType)
    ! Find the indices of M containing the diagonal terms
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
        IF(kk == 0 .OR.(M%a(kk) .APPROXEQA. 0._SRK)) THEN
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
  LOGICAL(SBK) :: diagDom

  !Check if M is allocated.
  IF(.NOT. ALLOCATED(solver%M) .AND. solver%A%isInit) THEN
    ALLOCATE(TriDiagMatrixType :: solver%M)
    SELECTTYPE(A => solver%A); TYPE IS(TriDiagMatrixType)
      CALL pList%add('MatrixType->n',A%n)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL solver%M%init(pList)
      CALL pList%clear()
    ENDSELECT
  ENDIF

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
      IF(A%a(2,1) .APPROXEQA. 0._SRK) THEN
        RETURN
      ENDIF

      SELECTTYPE(M => solver%M); TYPE IS(TriDiagMatrixType)
        M%a(2,1)=1.0_SRK/A%a(2,1)
        DO i=1,A%n-1
          M%a(1,i+1)=A%a(1,i+1)*M%a(2,i)
          M%a(3,i)=A%a(3,i)
          t=A%a(2,i+1)-M%a(1,i+1)*M%a(3,i)
          !If failed, return.
          IF(t .APPROXEQA. 0._SRK) THEN
            RETURN
          ENDIF
          M%a(2,i+1)=1.0_SRK/t
        ENDDO
        solver%info=0
        solver%isDecomposed=.TRUE.
      ENDSELECT

      !Give the warning
      IF(.NOT. diagDom) CALL eLinearSolverType%raiseDebug(modName// &
          '::'//myName//'- Tri-diagonal Matrix not diagonally dominant, '// &
          'solution might be not accurate')
    ENDSELECT
  ENDIF
ENDSUBROUTINE DecomposePLU_TriDiag


!-------------------------------------------------------------------------------
!> @brief Solves a sparse system using forward and backward substitution
!> @param M The resultant ILU factorization of A, inverted.
!> @param b the RHS vector
!> @param x the output vector, x=inv(M)*b
!>
!> This subroutine applies the inverse of a matrix M to a vector b and returns
!> x. It assumes that M is stored in the Compressed Sparse Row (CSR) format, and
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
!> This subroutine applies the inverse of a matrix M to a vector b and returns
!> x. Minv is stored as a dense matrix.
!>
SUBROUTINE MinvMult_Dense(Minv,b,x)
    TYPE(DenseSquareMatrixType),INTENT(INOUT) :: Minv
    REAL(SRK),INTENT(IN) :: b(:)
    REAL(SRK),INTENT(INOUT) :: x(:)
    x=0._SRK
    CALL BLAS_matvec(THISMATRIX=Minv,X=b,Y=x)
ENDSUBROUTINE MinvMult_dense
!
!-------------------------------------------------------------------------------
!> @brief Solve a tridiagonal system on a tridiag matrix using G.E.
!> @param solver The LinearSolverType object, previously decomposed with PLU
!> method.
!>
!> This routine assumes that the tridiagonal matrix has already been decomposed
!> in to its PLU parts, with LU stored in M.
!>
SUBROUTINE solvePLU_TriDiag(solver)
  CLASS(LinearSolverType_Base),INTENT(INOUT) :: solver
  INTEGER(SIK) :: n,i
  REAL(SRK) :: Xprev,tmpxval,tmpbval

  solver%info=-1
  IF(solver%isDecomposed) THEN
    SELECTTYPE(M => solver%M); TYPE IS(TriDiagMatrixType)
      SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
        SELECTTYPE(b => solver%b); TYPE IS(RealVectorType)
          n=M%n
          !LUx=b,Ux=y, Ly=b
          !find y (Ly=b), y is stored in X to save space
          CALL b%get(1,tmpbval)
          CALL X%set(1,tmpbval)
          DO i=2,n
            CALL X%get((i-1),tmpxval)
            CALL b%get(i,tmpbval)
            CALL X%set(i,tmpbval-M%a(1,i)*tmpxval)
          ENDDO
          !find x with backward substitution (Ux=y)
          CALL X%get(n,tmpxval)
          CALL X%set(n,tmpxval*M%a(2,n))
          CALL X%get(n,Xprev)
          DO i=(n-1),1,-1
            CALL X%get(i,tmpxval)
            CALL X%set(i,(tmpxval-M%a(3,i)*Xprev)*M%a(2,i))
            CALL X%get(i,Xprev)
          ENDDO
          solver%info=0
        ENDSELECT
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
  REAL(SRK) :: t,thisb(solver%A%n),tmpxval
  INTEGER(SIK) :: N,i,irow,icol,IPIV(solver%A%n)

  N=0

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
      CALL BLAS_axpy(N-i,-thisa(irow,i),thisa(i:N,i+1),N, &
          thisa(irow:N,i+1),N)
      thisb(irow)=thisb(irow)-thisa(irow,i)*thisb(i)
    ENDDO
  ENDDO

  !Perform backward substitution
  IF(thisa(N,N) .APPROXEQA. 0._SRK) RETURN
  SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
    CALL X%set(N,thisb(N)/thisa(N,N))
  ENDSELECT
  DO irow=N-1,1,-1
    t=0._SRK
    DO icol=irow+1,N
      SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
        CALL X%get(icol,tmpxval)
        t=t+thisa(irow,icol)*tmpxval
       ENDSELECT
    ENDDO
     SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
       CALL X%set(irow,(thisb(irow)-t)/thisa(irow,irow))
     ENDSELECT
  ENDDO
  solver%info=0
ENDSUBROUTINE solveGE_DenseSquare
!
!-------------------------------------------------------------------------------
!> @brief Decompose a dense square system into a upper triangular matrix and a
!> lower triangular matrix.
!> @param solver The linear solver object
!>
!> This routine perform partial pivoting.
!>
SUBROUTINE DecomposePLU_DenseSquare(solver)
  CLASS(LinearSolverType_Direct),INTENT(INOUT) :: solver
  INTEGER(SIK) :: N,i,irow
  REAL(SRK) :: t
  TYPE(ParamType) :: pList

  IF(.NOT. ALLOCATED(solver%M) .AND. solver%A%isInit) THEN
    ALLOCATE(DenseSquareMatrixType :: solver%M)
    CALL pList%add('MatrixType->n',solver%A%n)
    CALL pList%add('MatrixType->isSym',.FALSE.)
    CALL solver%M%init(pList)
    CALL pList%clear()
  ENDIF

  IF(.NOT. ALLOCATED(solver%IPIV) .AND. solver%A%isInit) THEN
    CALL dmallocA(solver%IPIV,solver%A%n)
  ENDIF

  SELECTTYPE(M => solver%M); TYPE IS(DenseSquareMatrixType)
    SELECTTYPE(A => solver%A); TYPE IS(DenseSquareMatrixType)
      M=A
    ENDSELECT
  ENDSELECT

  solver%IPIV=0
  solver%info=-1
  solver%isDecomposed=.FALSE.
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

      IF(t .APPROXEQA. 0._SRK) RETURN
      !if it differs from the current row, interchange the two rows.
      IF(solver%IPIV(i) /= i) THEN
        CALL BLAS_swap(N,M%A(solver%IPIV(i):N,1),N,M%A(i:N,1),N)
      ENDIF

      !Perform forward substitution
      DO irow=i+1,N
        t=1.0_SRK/M%A(i,i)
        M%A(irow,i)=M%A(irow,i)*t
        CALL BLAS_axpy(N-i,-M%A(irow,i),M%A(i:N,i+1),N,M%A(irow:N,i+1),N)
      ENDDO
    ENDDO
    IF(M%A(N,N) .APPROXEQA. 0._SRK) RETURN
    solver%info=0
    solver%isDecomposed=.TRUE.
  ENDSELECT
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
ENDSUBROUTINE SolvePLU_DenseSquare
!
!-------------------------------------------------------------------------------
!> @brief Solve the sparse linear system
!> @param solver The linear solver object
!>
!> This routine solves the sparse linear system by two method. If the MKL
!> library could be found, the PLU method will be called. If it is not found,
!> hard coded CGNR method will be used instead.
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
  TYPE(ParamType) :: pList,pList2

  N=0
  N=solver%A%n
  M=N
  !Set parameter list for vector
  CALL pList%add('VectorType -> n',solver%A%n)
  CALL pList2%add('VectorType -> n',solver%A%n)

  SELECTTYPE(A => solver%A); TYPE IS(DenseRectMatrixType)
    M=A%m
    CALL pList2%set('VectorType -> n',M)
  ENDSELECT
  solver%info=-1
  convTol=1e-9
  maxIters=M
  SELECTTYPE(solver); TYPE IS(LinearSolverType_Iterative)
    maxIters=MIN(M,solver%maxIters)
    convTol=solver%absConvTol
  ENDSELECT
  IF(N >= M) THEN
    CALL r%init(pList)
    CALL w%init(pList)
    CALL b%init(pList)
    CALL z%init(pList2)
    CALL p%init(pList2)
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
  CALL pList%clear()
  CALL pList2%clear()
ENDSUBROUTINE solveCGNR
!
!-------------------------------------------------------------------------------
!> @brief Solve a dense rectangular system using QR factorization
!> @param solver The LinearSolverType object
!>
!> This routine solves the rectangular linear system by QR method. It only
!> works when the number of equations is larger than the number of the unknowns.
!> IF not solver%info will return -1.
!>
SUBROUTINE solveQR_Dense(solver)
  CLASS(LinearSolverType_Direct),INTENT(INOUT) :: solver

  REAL(SRK),ALLOCATABLE :: thisa(:,:)
  REAL(SRK),ALLOCATABLE :: thisb(:),u(:)
  INTEGER(SIK) :: M,N,i,j,icol,irow
  REAL(SRK) :: alpha, beta,t,tmpxval

  M=0
  N=0
  SELECTTYPE(A => solver%A); TYPE IS(DenseRectMatrixType)
    M=A%m
    N=A%n
    ALLOCATE(thisa(N,M))
    thisa=A%A
  ENDSELECT

  ALLOCATE(thisb(N))
  ALLOCATE(u(N))
  SELECTTYPE(b => solver%b); TYPE IS(RealVectorType)
    thisb=b%b
  ENDSELECT

  solver%info=-1

  DO i=1,M
    u(:)=0.0_SRK
    u(i:N)=thisA(i:N,i)
    alpha=BLAS_nrm2(N,u)*u(i)/ABS(u(i))
    u(i)=u(i)+alpha
    !reuse alpha for <u,u>
    alpha=BLAS_dot(N,u,u)
    beta=-2.0_SRK*BLAS_dot(N,u,thisb)/alpha
    CALL BLAS_axpy(N,beta,u,thisb)
    DO j=i,M
      beta=-2.0_SRK*BLAS_dot(N,u,thisA(:,j))/alpha
      CALL BLAS_axpy(N,beta,u,thisA(:,j))
    ENDDO
  ENDDO

  !Perform backward substitution
  IF(thisa(M,M) .APPROXEQA. 0._SRK) RETURN
  SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
    CALL X%set(M,thisb(M)/thisa(M,M))
  ENDSELECT
  DO irow=M-1,1,-1
    t=0._SRK
    DO icol=irow+1,M
      SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
        CALL X%get(icol,tmpxval)
        t=t+thisa(irow,icol)*tmpxval
       ENDSELECT
    ENDDO
     SELECTTYPE(X => solver%X); TYPE IS(RealVectorType)
       CALL X%set(irow,(thisb(irow)-t)/thisa(irow,irow))
     ENDSELECT
  ENDDO
  solver%info=0
ENDSUBROUTINE solveQR_Dense
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
  CASE(: -2)
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
!
!-------------------------------------------------------------------------------
!> @brief Subroutine that sets up the default parameter lists for the
!>        LinearSolverType.
!> The required parameters for the Linear Solver Type are:
!>        'LinearSolverType->TPLType',SIK
!>        'LinearSolverType->solverMethod',SIK
!>        'LinearSolverType->MPI_COMM_ID',SIK
!>        'LinearSolverType->numberOMP',SIK
!>        'LinearSolverType->timerName',CHAR(LEN=256)
!>        'LinearSolverType->matType',SIK
!>        'LinearSolverType->A->MatrixType',Parameter List for MatrixType
!>        'LinearSolverType->x->VectorType',Parameter List for VectorType
!>        'LinearSolverType->b->VectorType',Parameter List for VectorType
!> The optional parameters for the PETSc Matrix Type do not exist.
!>
SUBROUTINE LinearSolverType_Declare_ValidParams()
  INTEGER(SIK) :: n,TPLType,solverMethod,MPI_COMM_ID,numberOMP,matType
  CHARACTER(LEN=256) :: timerName

  !Setup the required and optional parameter lists
  n=1_SIK
  matType=1_SIK
  TPLType=1_SIK
  solverMethod=1_SIK
  MPI_COMM_ID=1_SIK
  numberOMP=1_SIK
  timerName='Some LinearSolver Timer'

  CALL LinearSolverType_reqParams%clear()
  CALL LinearSolverType_reqParams%add('LinearSolverType->TPLType',TPLType)
  CALL LinearSolverType_reqParams%add('LinearSolverType->solverMethod',solverMethod)
  CALL LinearSolverType_reqParams%add('LinearSolverType->MPI_COMM_ID',MPI_COMM_ID)
  CALL LinearSolverType_reqParams%add('LinearSolverType->numberOMP',numberOMP)
  CALL LinearSolverType_reqParams%add('LinearSolverType->timerName',timerName)
  CALL LinearSolverType_reqParams%add('LinearSolverType->matType',matType)
  CALL LinearSolverType_reqParams%add('LinearSolverType->A->MatrixType->n',n)
  CALL LinearSolverType_reqParams%remove('LinearSolverType->A->MatrixType->n')
  CALL LinearSolverType_reqParams%add('LinearSolverType->b->VectorType->n',n)
  CALL LinearSolverType_reqParams%remove('LinearSolverType->b->VectorType->n')
  CALL LinearSolverType_reqParams%add('LinearSolverType->x->VectorType->n',n)
  CALL LinearSolverType_reqParams%remove('LinearSolverType->x->VectorType->n')

  !There are no optional parameters at this time.

  !Set flag to true since the defaults have been set for this type.
  LinearSolverType_Paramsflag=.TRUE.
ENDSUBROUTINE LinearSolverType_Declare_ValidParams
!
!-------------------------------------------------------------------------------
!> @brief Subroutine that clears the default parameter lists for the
!>        LinearSolverType.
!>
SUBROUTINE LinearSolverType_Clear_ValidParams()

  !Set flag to true since the defaults have been set for this type.
  LinearSolverType_Paramsflag=.FALSE.

  CALL LinearSolverType_reqParams%clear()

  !There are no optional parameters at this time.

ENDSUBROUTINE LinearSolverType_Clear_ValidParams
!
ENDMODULE LinearSolverTypes
