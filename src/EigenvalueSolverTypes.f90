!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!  This manuscript has been authored by UT-Battelle, LLC, under Contract       !
!  No. DE-AC0500OR22725 with the U.S. Department of Energy. The United States  !
!  Government retains and the publisher, by accepting the article for          !
!  publication, acknowledges that the United States Government retains a       !
!  non-exclusive, paid-up, irrevocable, world-wide license to publish or       !
!  reproduce the published form of this manuscript, or allow others to do so,  !
!  for the United States Government purposes.                                  !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module provides a eigenvalue system type and methods to solve systems
!> of equations
!>
!> Currently supported TPLs include:
!>  - SLEPc (with interfaces to EPS)
!>
!> Additional TPL support is planned for:
!>  - Trilinos
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref BLAS "BLAS": @copybrief BLAS
!>  - @ref Times "Times": @copybrief Times
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref Allocs "Allocs": @copybrief Allocs
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref ParallelEnv "ParallelEnv": @copybrief ParallelEnv
!>  - @ref VectorTypes "VectorTypes": @copybrief VectorTypes
!>  - @ref MatrixTypes "MatrixTypes": @copybrief MatrixTypes
!>  - @ref LinearSolverTypes "LinearSolverTypes": @copybrief LinearSolverTypes
!>
!> @par EXAMPLES
!> @code
!>
!> @endcode
!>
!> @author Ben Collins
!>   @date 02/14/2016
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE EigenvalueSolverTypes
  USE ISO_FORTRAN_ENV
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
#ifdef MPACT_HAVE_ForTeuchos
  USE ForTeuchos_ParameterList
#endif
  IMPLICIT NONE

#ifdef MPACT_HAVE_PETSC
#include <finclude/petsc.h>
#include <petscversion.h>
!petscisdef.h defines the keyword IS, and it needs to be reset
#ifdef MPACT_HAVE_SLEPC
#include <finclude/slepcsys.h>
#include <finclude/slepceps.h>
#endif
#undef IS
#endif

  PRIVATE
!
! List of public members
  PUBLIC :: eEigenvalueSolverType
  PUBLIC :: EigenvalueSolverType_Base
  PUBLIC :: EigenvalueSolverType_SLEPc
  PUBLIC :: EigenvalueSolverType_Anasazi
  PUBLIC :: EigenvalueSolverType_SNES

  PUBLIC :: PETSC_SHELL_EVS
#ifdef MPACT_HAVE_PETSC
  PUBLIC :: PETSC_SHELL_CALCRESID_extern
#endif

  !> set enumeration scheme for TPLs
  INTEGER(SIK),PARAMETER,PUBLIC :: SLEPC=0,ANASAZI=1,NATIVE=4
  !> set enumeration scheme for solver methods
  INTEGER(SIK),PARAMETER,PUBLIC :: POWER_IT=0,JD=1,GD=2,ARNOLDI=3,NONLIN=4

  !> @brief the base eigenvalue solver type
  TYPE,ABSTRACT :: EigenvalueSolverType_Base
    !> Initialization status
    LOGICAL(SBK) :: isInit=.FALSE.
    !> Integer flag for the solution methodology desired
    INTEGER(SIK) :: solverMethod=-1
    !> Integer flag for the solution methodology desired
    INTEGER(SIK) :: TPLType=-1
    !> Pointer to the distributed memory parallel environment
    TYPE(MPI_EnvType),POINTER :: MPIparallelEnv => NULL()
    !> Pointer to the shared memory parallel environment TODO: eventually
    TYPE(OMP_EnvType) :: OMPparallelEnv
    !> size of eigenvalue system
    INTEGER(SIK) :: n=-1
    !> Maximum number of iterations
    INTEGER(SIK) :: maxit=-1
    !> Stopping tolerance
    REAL(SRK) :: tol
    !> eigenvalue of the system
    REAL(SRK) :: k
    !> update pc
    LOGICAL(SBK) :: updatePC=.TRUE.
    !> pc setup
    LOGICAL(SBK) :: setupPC=.TRUE.
    !> Pointer to the MatrixType A
    CLASS(MatrixType),POINTER :: A => NULL()
    !> Pointer to the MatrixType B
    CLASS(MatrixType),POINTER :: B => NULL()
    !> Pointer to solution vector, x
    CLASS(VectorType),POINTER :: X
    !> Timer to measure solution time
    TYPE(TimerType) :: SolveTime
  !
  !List of Type Bound Procedures
    CONTAINS
      !> Deferred routine for initializing the eigenvalue solver system
      PROCEDURE(evsolver_init_sub_absintfc),DEFERRED,PASS :: init
      !> Deferred routine for clearing the eigenvalue solver
      PROCEDURE(evsolver_sub_absintfc),DEFERRED,PASS :: clear
      !> Deferred routine for solving the eigenvalue system
      PROCEDURE(evsolver_sub_absintfc),DEFERRED,PASS :: solve
      !> Routine for setting A and B for eigenvalue solver system
      PROCEDURE,PASS :: setMat => setMat_EigenvalueSolverType_Base
      !> @copybrief EigenvalueSolverTypes::getResidual_EigenvalueSolverType_Base
      !> @copydetails EigenvalueSolverTypes::getResidual_EigenvalueSolverType_Base
      PROCEDURE,PASS :: getResidual => getResidual_EigenvalueSolverType_Base
      !> @copybrief EigenvalueSolverTypes::setConv_EigenvalueSolverType_Base
      !> @copydetails EigenvalueSolverTypes::setConv_EigenvalueSolverType_Base
      PROCEDURE,PASS :: setConv => setConv_EigenvalueSolverType_Base
      !> @copybrief EigenvalueSolverTypes::setX0_EigenvalueSolverType_Base
      !> @copydetails EigenvalueSolverTypes::setX0_EigenvalueSolverType_Base
      PROCEDURE,PASS :: setX0 => setX0_EigenvalueSolverType_Base
  ENDTYPE EigenvalueSolverType_Base

  !> Explicitly defines the interface for the clear and solve routines
  ABSTRACT INTERFACE
    SUBROUTINE evsolver_init_sub_absintfc(solver,MPIEnv,Params)
      IMPORT :: EigenvalueSolverType_Base, ParamType, MPI_EnvType
      CLASS(EigenvalueSolverType_Base),INTENT(INOUT) :: solver
      TYPE(MPI_EnvType),INTENT(IN),TARGET :: MPIEnv
      TYPE(ParamType),INTENT(IN) :: Params
    ENDSUBROUTINE evsolver_init_sub_absintfc

    SUBROUTINE evsolver_sub_absintfc(solver)
      IMPORT :: EigenvalueSolverType_Base
      CLASS(EigenvalueSolverType_Base),INTENT(INOUT) :: solver
    ENDSUBROUTINE evsolver_sub_absintfc
  ENDINTERFACE

  !> @brief The extended type for the SLEPc Eigenvalue Solvers
  TYPE,EXTENDS(EigenvalueSolverType_Base) :: EigenvalueSolverType_SLEPc
    !>
    LOGICAL(SBK) :: clops=.FALSE.
#ifdef MPACT_HAVE_SLEPC
    !> SLEPc Eigenvalue Solver type
    EPS :: eps
#endif
    !> store vector for imaginary term required by SLEPc
    TYPE(PETScVectorType) :: xi
!
!List of Type Bound Procedures
    CONTAINS
      !> @copybrief EigenvalueSolverTypes::init_EigenvalueSolverType_SLEPc
      !> @copydetails EigenvalueSolverTypes::init_EigenvalueSolverType_SLEPc
      PROCEDURE,PASS :: init => init_EigenvalueSolverType_SLEPc
      !> @copybrief EigenvalueSolverTypes::clear_EigenvalueSolverType_SLEPc
      !> @copydetails EigenvalueSolverTypes::clear_EigenvalueSolverType_SLEPc
      PROCEDURE,PASS :: clear => clear_EigenvalueSolverType_SLEPc
      !> @copybrief EigenvalueSolverTypes::solve_EigenvalueSolverType_SLEPc
      !> @copydetails EigenvalueSolverTypes::solve_EigenvalueSolverType_SLEPc
      PROCEDURE,PASS :: solve => solve_EigenvalueSolverType_SLEPc
  ENDTYPE EigenvalueSolverType_SLEPc

  !> @brief The extended type for the SLEPc Eigenvalue Solvers
  TYPE,EXTENDS(EigenvalueSolverType_Base) :: EigenvalueSolverType_Anasazi
    !>
    !> Anasazi Eigenvalue Solver type
    INTEGER(SIK) :: eig
    !> Anasazi Eigenvalue Preconditioner type
    INTEGER(SIK) :: pc
    !> store vector for scaling fission source
    TYPE(TrilinosVectorType) :: x_scale
!
!List of Type Bound Procedures
    CONTAINS
      !> @copybrief EigenvalueSolverTypes::init_EigenvalueSolverType_Anasazi
      !> @copydetails EigenvalueSolverTypes::init_EigenvalueSolverType_Anasazi
      PROCEDURE,PASS :: init => init_EigenvalueSolverType_Anasazi
      !> @copybrief EigenvalueSolverTypes::clear_EigenvalueSolverType_Anasazi
      !> @copydetails EigenvalueSolverTypes::clear_EigenvalueSolverType_Anasazi
      PROCEDURE,PASS :: clear => clear_EigenvalueSolverType_Anasazi
      !> @copybrief EigenvalueSolverTypes::solve_EigenvalueSolverType_Anasazi
      !> @copydetails EigenvalueSolverTypes::solve_EigenvalueSolverType_Anasazi
      PROCEDURE,PASS :: solve => solve_EigenvalueSolverType_Anasazi
  ENDTYPE EigenvalueSolverType_Anasazi

  !> @brief The extended type for the SLEPc Eigenvalue Solvers
  TYPE,EXTENDS(EigenvalueSolverType_Base) :: EigenvalueSolverType_SNES
    !>
#ifdef MPACT_HAVE_PETSC
!Rondom stuff you need for SNES... like SNES :: snes
#endif
!
!List of Type Bound Procedures
    CONTAINS
      !> @copybrief EigenvalueSolverTypes::init_EigenvalueSolverType_SNES
      !> @copydetails EigenvalueSolverTypes::init_EigenvalueSolverType_SNES
      PROCEDURE,PASS :: init => init_EigenvalueSolverType_SNES
      !> @copybrief EigenvalueSolverTypes::clear_EigenvalueSolverType_SNES
      !> @copydetails EigenvalueSolverTypes::clear_EigenvalueSolverType_SNES
      PROCEDURE,PASS :: clear => clear_EigenvalueSolverType_SNES
      !> @copybrief EigenvalueSolverTypes::solve_EigenvalueSolverType_SNES
      !> @copydetails EigenvalueSolverTypes::solve_EigenvalueSolverType_SNES
      PROCEDURE,PASS :: solve => solve_EigenvalueSolverType_SNES
      !> @copybrief EigenvalueSolverTypes::calcResid_EigenvalueSolverType_SNES
      !> @copydetails EigenvalueSolverTypes::calcResid_EigenvalueSolverType_SNES
      PROCEDURE,PASS :: calcResid => calcResid_EigenvalueSolverType_SNES
  ENDTYPE EigenvalueSolverType_SNES


  !> SNES Resid calculator
  CLASS(EigenvalueSolverType_Base),POINTER :: PETSC_SHELL_EVS => NULL()



  !> Logical flag to check whether the required and optional parameter lists
  !> have been created yet for the Eigenvalue Solver Type.
  LOGICAL(SBK),SAVE :: EigenvalueSolverType_Paramsflag=.FALSE.

  !> The parameter lists to use when validating a parameter list for
  !> initialization for a Eigenvalue Solver Type.
  TYPE(ParamType),PROTECTED,SAVE :: EigenvalueSolverType_reqParams,EigenvalueSolverType_optParams

  !> Exception Handler for use in MatrixTypes
  TYPE(ExceptionHandlerType),SAVE :: eEigenvalueSolverType

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='EIGENVALUESOLVERTYPES'

#ifdef MPACT_HAVE_SLEPC
      PetscErrorCode ierr
#endif

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Eigenvalue Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The eigen solver to act on
!> @param solverMethod The integer flag for which type of solution scheme to use
!> @param MPIparallelEnv The MPI environment description
!> @param OMPparallelEnv The OMP environment description
!> @param TimerName The name of the timer to be used for querying (optional)
!>
!> This routine initializes the data spaces for the SLEPc eigenvalue solver.
!>
    SUBROUTINE init_EigenvalueSolverType_SLEPc(solver,MPIEnv,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_EigenvalueSolverType_SLEPc'
      CLASS(EigenvalueSolverType_SLEPc),INTENT(INOUT) :: solver
      TYPE(MPI_EnvType),INTENT(IN),TARGET :: MPIEnv
      TYPE(ParamType),INTENT(IN) :: Params
      TYPE(ParamType) :: validParams, tmpPL
      INTEGER(SIK) :: n,nlocal,solvertype,maxit
      REAL(SRK) :: tol
      LOGICAL(SBK) :: clops
      TYPE(STRINGType) :: pctype
#ifdef MPACT_HAVE_SLEPC
      ST :: st
      KSP :: ksp
      PC :: pc
#endif
      !Check to set up required and optional param lists.
      !IF(.NOT.EigenType_Paramsflag) CALL EigenType_Declare_ValidParams()

      IF(.NOT. MPIEnv%isInit()) THEN
        CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
          modName//'::'//myName//' - MPI Environment is not initialized!')
      ELSE
        solver%MPIparallelEnv => MPIEnv
      ENDIF
      !Validate against the reqParams and OptParams
      validParams=Params
      !CALL validParams%validate(EigenType_reqParams)

      n=0
      nlocal=0
      solvertype=-1
      !Pull Data from Parameter List
      CALL validParams%get('EigenvalueSolverType->n',n)
      CALL validParams%get('EigenvalueSolverType->nlocal',nlocal)
      CALL validParams%get('EigenvalueSolverType->solver',solvertype)
      CALL validParams%get('EigenvalueSolverType->preconditioner',pctype)
      CALL validParams%get('EigenvalueSolverType->tolerance',tol)
      CALL validParams%get('EigenvalueSolverType->max_iterations',maxit)
      IF(validParams%has('EigenvalueSolverType->SLEPc->cmdline_options')) &
        CALL validParams%get('EigenvalueSolverType->SLEPc->cmdline_options',clops)

      IF(.NOT. solver%isInit) THEN
        IF(n < 1) THEN
          CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (n) must be '// &
              'greater than 0!')
        ELSE
          solver%n=n
        ENDIF

        IF((nlocal < 1) .AND. (nlocal > n)) THEN
          CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (nlocal) must be '// &
              'greater than 0 and less than or equal to (n)!')
        ENDIF

        IF(tol<=0.0_SRK) THEN
          CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Tolerance must be '// &
              'greater than 0!')
        ELSE
          solver%tol=tol
        ENDIF

        IF(maxit < 1) THEN
          CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Maximum Iterations must be '// &
              'greater than 0!')
        ELSE
          solver%maxit=maxit
        ENDIF
#ifdef MPACT_HAVE_SLEPC
        CALL EPSCreate(solver%MPIparallelEnv%comm,solver%eps,ierr)
        CALL EPSSetProblemType(solver%eps,EPS_GNHEP,ierr)
        IF(ierr/=0) &
          CALL eEigenvalueSolverType%raiseError(modName//'::'//myName// &
            ' - SLEPc failed to initialize.')
        SELECTCASE(solvertype)
          CASE(POWER_IT)
            CALL EPSSetType(solver%eps,EPSPOWER,ierr)
          CASE(JD)
            CALL EPSSetType(solver%eps,EPSJD,ierr)
            CALL EPSSetWhichEigenpairs(solver%eps,EPS_LARGEST_REAL,ierr)
          CASE(GD)
            CALL EPSSetType(solver%eps,EPSGD,ierr)
            CALL EPSSetWhichEigenpairs(solver%eps,EPS_LARGEST_REAL,ierr)
          CASE(ARNOLDI)
            CALL EPSSetType(solver%eps,EPSARNOLDI,ierr)
            CALL EPSSetWhichEigenpairs(solver%eps,EPS_LARGEST_REAL,ierr)
          CASE DEFAULT
            CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
              modName//'::'//myName//' - Unknow solver type.')
        ENDSELECT
        IF(ierr/=0) &
            CALL eEigenvalueSolverType%raiseError(modName//'::'//myName// &
              ' - SLEPc failed to set solver type')
        CALL EPSSetTolerances(solver%eps,solver%tol,solver%maxit,ierr)
        IF(ierr/=0) &
            CALL eEigenvalueSolverType%raiseError(modName//'::'//myName// &
              ' - SLEPc failed to set solver type')

        !TODO: Need to set PC type
        CALL EPSGetST(solver%eps,st,ierr)
        !get KSP
        CALL STGetKSP(st,ksp,ierr)
        !CALL KSPSetType(ksp,KSPBCGS,ierr)
        !ksp pc
        CALL KSPGetPC(ksp,pc,ierr)
        !CALL PCSetType(pc,PCHYPRE,ierr)
        !CALL PCHYPRESetType(pc,"pilut",ierr)
        CALL PCSetType(pc,PCBJACOBI,ierr)
        !CALL PCSetReusePreconditioner(pc,PETSC_TRUE,ierr)
        !set KSP
#else
        CALL eEigenvalueSolverType%raiseError(modName//'::'//myName// &
          ' - SLEPc is not present in build')
#endif
        solver%SolverMethod=solvertype

        ALLOCATE(PETScVectorType :: solver%X)
        CALL tmpPL%clear()
        CALL tmpPL%add('VectorType->n',n)
        CALL tmpPL%add('VectorType->MPI_Comm_ID',solver%MPIparallelEnv%comm)
        CALL tmpPL%add('VectorType->nlocal',nlocal)
        CALL solver%X%init(tmpPL)
        CALL solver%xi%init(tmpPL)

        solver%TPLType=SLEPC
        solver%isInit=.TRUE.
      ELSE
        CALL eEigenvalueSolverType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - EigenvalueSolverType already initialized')
      ENDIF
      CALL validParams%clear()

    ENDSUBROUTINE init_EigenvalueSolverType_SLEPc

!
!-------------------------------------------------------------------------------
!> @brief Initializes the Eigenvalue Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The eigen solver to act on
!> @param solverMethod The integer flag for which type of solution scheme to use
!> @param MPIparallelEnv The MPI environment description
!> @param OMPparallelEnv The OMP environment description
!> @param TimerName The name of the timer to be used for querying (optional)
!>
!> This routine initializes the data spaces for the Anasazi eigenvalue solver.
!>
!> Custom options may be specified for the Anasazi solver and its preconditioner
!> through the "anasazi_options" and "pc_options" sections in the parameter
!> list, respectively. The entries in these lists should be valid options to
!> their respective Trilinos classes, as they are converted to Teuchos parameter
!> lists and handed to Trilinos.
    SUBROUTINE init_EigenvalueSolverType_Anasazi(solver,MPIEnv,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_EigenvalueSolverType_Anasazi'
      CLASS(EigenvalueSolverType_Anasazi),INTENT(INOUT) :: solver
      TYPE(MPI_EnvType),INTENT(IN),TARGET :: MPIEnv
      TYPE(ParamType),INTENT(IN) :: Params
      TYPE(ParamType) :: validParams, tmpPL
#ifdef MPACT_HAVE_Trilinos
      INTEGER(SIK) :: n,nlocal,solvertype,maxit
      REAL(SRK) :: tol
      TYPE(STRINGType) :: pctype
      INTEGER(C_INT) :: ierr
      CLASS(ParamType),POINTER :: anasaziParams, pcParams
      TYPE(ForTeuchos_ParameterList_ID) :: plID
      !Check to set up required and optional param lists.
      !IF(.NOT.EigenType_Paramsflag) CALL EigenType_Declare_ValidParams()

      IF(.NOT. MPIEnv%isInit()) THEN
        CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
          modName//'::'//myName//' - MPI Environment is not initialized!')
      ELSE
        solver%MPIparallelEnv => MPIEnv
      ENDIF
      !Validate against the reqParams and OptParams
      validParams=Params
      !CALL validParams%validate(EigenType_reqParams)

      n=0
      nlocal=0
      solvertype=-1
      !Pull Data from Parameter List
      CALL validParams%get('EigenvalueSolverType->n',n)
      CALL validParams%get('EigenvalueSolverType->nlocal',nlocal)
      CALL validParams%get('EigenvalueSolverType->solver',solvertype)
      CALL validParams%get('EigenvalueSolverType->preconditioner',pctype)
      CALL validParams%get('EigenvalueSolverType->tolerance',tol)
      CALL validParams%get('EigenvalueSolverType->max_iterations',maxit)

      IF(.NOT. solver%isInit) THEN
        IF(n < 1) THEN
          CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (n) must be '// &
              'greater than 0!')
        ELSE
          solver%n=n
        ENDIF

        IF((nlocal < 1) .AND. (nlocal > n)) THEN
          CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (nlocal) must be '// &
              'greater than 0 and less than or equal to (n)!')
        ENDIF

        IF(tol<=0.0_SRK) THEN
          CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Tolerance must be '// &
              'greater than 0!')
        ELSE
          solver%tol=tol
        ENDIF

        IF(maxit < 1) THEN
          CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Maximum Iterations must be '// &
              'greater than 0!')
        ELSE
          solver%maxit=maxit
        ENDIF
        IF(Params%has('EigenvalueSolverType->anasazi_options')) THEN
          CALL Params%get('EigenvalueSolverType->anasazi_options', anasaziParams)
          plID = Teuchos_ParameterList_Create(ierr)
          CALL anasaziParams%toTeuchosPlist(plID)
          CALL Anasazi_Init_Params(solver%eig, plID)
          CALL Teuchos_ParameterList_Release(plID, ierr)
        ELSE
          CALL Anasazi_Init(solver%eig)
        ENDIF
        IF(solvertype/=GD) THEN
          CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
              modName//'::'//myName//' - Only Generalized Davidson works with Anasazi.')
        ENDIF

        !Need to set PC type
        IF(Params%has('EigenvalueSolverType->pc_options')) THEN
          CALL Params%get('EigenvalueSolverType->pc_options', pcParams)
          ! Make sure that a pc_option is defined, if not, set to 2
          IF(.NOT.pcParams%has('pc_options->pc_option')) THEN
            CALL pcParams%add('pc_options->pc_option',2_SIK)
          ENDIF

          plID = Teuchos_ParameterList_Create(ierr)
          CALL pcParams%toTeuchosPlist(plID)
          CALL Preconditioner_InitParams(solver%pc,plID)
          CALL Teuchos_ParameterList_Release(plID, ierr)
        ELSE
          CALL Preconditioner_Init(solver%pc,2)
        ENDIF


        solver%SolverMethod=solvertype

        ALLOCATE(TrilinosVectorType :: solver%X)
        CALL tmpPL%clear()
        CALL tmpPL%add('VectorType->n',n)
        CALL tmpPL%add('VectorType->MPI_Comm_ID',solver%MPIparallelEnv%comm)
        CALL tmpPL%add('VectorType->nlocal',nlocal)
        CALL solver%X%init(tmpPL)
        CALL solver%X_scale%init(tmpPL)
        SELECTTYPE(x=>solver%X); TYPE IS(TrilinosVectorType)
          CALL Anasazi_SetX(solver%eig,x%b)
        ENDSELECT

        solver%TPLType=Anasazi
        solver%isInit=.TRUE.
      ELSE
        CALL eEigenvalueSolverType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - EigenvalueSolverType already initialized')
      ENDIF
      CALL validParams%clear()

#else
      CALL eEigenvalueSolverType%raiseError(modName//'::'//myName// &
        ' - Anasazi (Trilinos) is not present in build')
#endif
    ENDSUBROUTINE init_EigenvalueSolverType_Anasazi

!
!-------------------------------------------------------------------------------
!> @brief Initializes the Eigenvalue Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The eigen solver to act on
!> @param MPIparallelEnv The MPI environment description
!> @param OMPparallelEnv The OMP environment description
!> @param TimerName The name of the timer to be used for querying (optional)
!>
!> This routine initializes the data spaces for the SNES eigenvalue solver.
!>
    SUBROUTINE init_EigenvalueSolverType_SNES(solver,MPIEnv,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_EigenvalueSolverType_SNES'
      CLASS(EigenvalueSolverType_SNES),INTENT(INOUT) :: solver
      TYPE(MPI_EnvType),INTENT(IN),TARGET :: MPIEnv
      TYPE(ParamType),INTENT(IN) :: Params
      TYPE(ParamType) :: validParams, tmpPL
      INTEGER(SIK) :: n,nlocal,solvertype,maxit
      REAL(SRK) :: tol
      TYPE(STRINGType) :: pctype
      !Check to set up required and optional param lists.
      !IF(.NOT.EigenType_Paramsflag) CALL EigenType_Declare_ValidParams()

      IF(.NOT. MPIEnv%isInit()) THEN
        CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
          modName//'::'//myName//' - MPI Environment is not initialized!')
      ELSE
        solver%MPIparallelEnv => MPIEnv
      ENDIF
      !Validate against the reqParams and OptParams
      validParams=Params
      !CALL validParams%validate(EigenType_reqParams)

      n=0
      nlocal=0
      solvertype=-1
      !Pull Data from Parameter List
      CALL validParams%get('EigenvalueSolverType->n',n)
      CALL validParams%get('EigenvalueSolverType->nlocal',nlocal)
      CALL validParams%get('EigenvalueSolverType->solver',solvertype)
      CALL validParams%get('EigenvalueSolverType->preconditioner',pctype)
      CALL validParams%get('EigenvalueSolverType->tolerance',tol)
      CALL validParams%get('EigenvalueSolverType->max_iterations',maxit)

      IF(.NOT. solver%isInit) THEN
        IF(n < 1) THEN
          CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (n) must be '// &
              'greater than 0!')
        ELSE
          solver%n=n
        ENDIF

        IF((nlocal < 1) .AND. (nlocal > n)) THEN
          CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (nlocal) must be '// &
              'greater than 0 and less than or equal to (n)!')
        ENDIF

        IF(tol<=0.0_SRK) THEN
          CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Tolerance must be '// &
              'greater than 0!')
        ELSE
          solver%tol=tol
        ENDIF

        IF(maxit < 1) THEN
          CALL eEigenvalueSolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Maximum Iterations must be '// &
              'greater than 0!')
        ELSE
          solver%maxit=maxit
        ENDIF

#ifdef MPACT_HAVE_PETSC
        !TODO: SNES Init

        !Need to set PC type
        !TODO PC Init

#else
        CALL eEigenvalueSolverType%raiseError(modName//'::'//myName// &
          ' - SNES (PETSc) is not present in build')
#endif
        solver%SolverMethod=solvertype

        ALLOCATE(PetscVectorType :: solver%X)
        CALL tmpPL%clear()
        CALL tmpPL%add('VectorType->n',n)
        CALL tmpPL%add('VectorType->MPI_Comm_ID',solver%MPIparallelEnv%comm)
        CALL tmpPL%add('VectorType->nlocal',nlocal)
        CALL solver%X%init(tmpPL)
        !TODO  Make your own tmp vector:
        !   CALL solver%X_scale%init(tmpPL)

        solver%TPLType=SLEPC
        solver%isInit=.TRUE.
      ELSE
        CALL eEigenvalueSolverType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - EigenvalueSolverType already initialized')
      ENDIF
      CALL validParams%clear()

    ENDSUBROUTINE init_EigenvalueSolverType_SNES

    SUBROUTINE setMat_EigenvalueSolverType_Base(solver,A,B)
      CLASS(EigenvalueSolverType_Base),INTENT(INOUT) :: solver
      CLASS(MatrixType),INTENT(IN),TARGET :: A
      CLASS(MatrixType),INTENT(IN),TARGET :: B

      solver%A=>A
      solver%B=>B
    ENDSUBROUTINE setMat_EigenvalueSolverType_Base

    SUBROUTINE getResidual_EigenvalueSolverType_Base(solver,resid,its)
      CLASS(EigenvalueSolverType_Base),INTENT(INOUT) :: solver
      REAL(SRK),INTENT(OUT) :: resid
      INTEGER(SIK),INTENT(OUT) :: its

      SELECTTYPE(solver); TYPE IS(EigenvalueSolverType_SLEPc)
#ifdef MPACT_HAVE_SLEPC
        CALL EPSComputeRelativeError(solver%eps,0,resid,ierr)
        CALL EPSGetIterationNumber(solver%eps,its,ierr)
#endif
      TYPE IS(EigenvalueSolverType_Anasazi)
#ifdef MPACT_HAVE_Trilinos
        CALL Anasazi_GetResid(solver%eig,resid)
        CALL Anasazi_GetIterationCount(solver%eig,its)
#endif
      ENDSELECT
    ENDSUBROUTINE getResidual_EigenvalueSolverType_Base

    !> @param convTol A value representing the convergence behavior
    !> @param maxIters The maximum number of iterations to perform
    SUBROUTINE setConv_EigenvalueSolverType_Base(solver,convTol,maxIters)
      CLASS(EigenvalueSolverType_Base),INTENT(INOUT) :: solver
      REAL(SRK),INTENT(IN) :: convTol
      INTEGER(SIK),INTENT(IN) :: maxIters

      solver%tol=convTol
      solver%maxit=maxIters
    ENDSUBROUTINE setConv_EigenvalueSolverType_Base


    SUBROUTINE setX0_EigenvalueSolverType_Base(solver,x0)
      CLASS(EigenvalueSolverType_Base),INTENT(INOUT) :: solver
      CLASS(VectorType),INTENT(INOUT) :: x0

      SELECTTYPE(solver)
        TYPE IS(EigenvalueSolverType_SLEPc)
          SELECTTYPE(x0); TYPE IS(PETScVectorType)
#ifdef MPACT_HAVE_SLEPC
            CALL EPSSetInitialSpace(solver%eps,1,x0%b,ierr)
#endif
          ENDSELECT
        !Anasazi doesn't need this, blas copy will handle it
      ENDSELECT
      CALL BLAS_copy(THISVECTOR=x0,NEWVECTOR=solver%X)
    ENDSUBROUTINE setX0_EigenvalueSolverType_Base
!
!-------------------------------------------------------------------------------
!> @brief Clears the SLEPc Eigen Solver Type
!> @param solver The eigen solver to act on
!>
!> This routine clears the data spaces
!>
    SUBROUTINE clear_EigenvalueSolverType_SLEPc(solver)
      CLASS(EigenvalueSolverType_SLEPc),INTENT(INOUT) :: solver

      solver%solverMethod=-1
      solver%TPLType=-1
      NULLIFY(solver%MPIparallelEnv)
      IF(solver%OMPparallelEnv%isInit()) CALL solver%OMPparallelEnv%clear
      solver%n=-1
      solver%maxit=-1
      solver%tol=0.0_SRK
      solver%k=0.0_SRK
      NULLIFY(solver%A)
      NULLIFY(solver%B)
      IF(solver%X%isInit) CALL solver%X%clear()
      IF(solver%xi%isInit) CALL solver%xi%clear()
#ifdef MPACT_HAVE_SLEPC
      CALL EPSDestroy(solver%eps,ierr)
#endif
      solver%isInit=.FALSE.
    ENDSUBROUTINE clear_EigenvalueSolverType_SLEPc
!
!-------------------------------------------------------------------------------
!> @brief Clears the SLEPc Eigen Solver Type
!> @param solver The eigen solver to act on
!>
!> This routine clears the data spaces
!>
    SUBROUTINE clear_EigenvalueSolverType_Anasazi(solver)
      CLASS(EigenvalueSolverType_Anasazi),INTENT(INOUT) :: solver

      solver%solverMethod=-1
      solver%TPLType=-1
      NULLIFY(solver%MPIparallelEnv)
      IF(solver%OMPparallelEnv%isInit()) CALL solver%OMPparallelEnv%clear
      solver%n=-1
      solver%maxit=-1
      solver%tol=0.0_SRK
      solver%k=0.0_SRK
      NULLIFY(solver%A)
      NULLIFY(solver%B)
      CALL solver%x_scale%clear()
      IF(solver%X%isInit) CALL solver%X%clear()
#ifdef MPACT_HAVE_Trilinos
      CALL Preconditioner_Destroy(solver%pc)
      CALL Anasazi_Destroy(solver%eig)
#endif
      solver%isInit=.FALSE.
    ENDSUBROUTINE clear_EigenvalueSolverType_Anasazi
!
!-------------------------------------------------------------------------------
!> @brief Clears the SLEPc Eigen Solver Type
!> @param solver The eigen solver to act on
!>
!> This routine clears the data spaces
!>
    SUBROUTINE clear_EigenvalueSolverType_SNES(solver)
      CLASS(EigenvalueSolverType_SNES),INTENT(INOUT) :: solver

      solver%solverMethod=-1
      solver%TPLType=-1
      NULLIFY(solver%MPIparallelEnv)
      IF(solver%OMPparallelEnv%isInit()) CALL solver%OMPparallelEnv%clear
      solver%n=-1
      solver%maxit=-1
      solver%tol=0.0_SRK
      solver%k=0.0_SRK
      NULLIFY(solver%A)
      NULLIFY(solver%B)
      IF(solver%X%isInit) CALL solver%X%clear()
      !IF(solver%xi%isInit) CALL solver%xi%clear()
#ifdef MPACT_HAVE_PETSC
      !CALL SNESDestroy(solver%snes,ierr)
#endif
      !TODO: add anything else allocated in SNES
      solver%isInit=.FALSE.
    ENDSUBROUTINE clear_EigenvalueSolverType_SNES
!
!-------------------------------------------------------------------------------
!> @brief Clears the SLEPc Eigen Solver Type
!> @param solver The eigen solver to act on
!>
!> This routine clears the data spaces
!>
    SUBROUTINE solve_EigenvalueSolverType_SLEPc(solver)
      CLASS(EigenvalueSolverType_SLEPc),INTENT(INOUT) :: solver
#ifdef MPACT_HAVE_SLEPC
      PetscScalar    kr, ki
      REAL(SRK) :: tmp(2)

      SELECTTYPE(A=>solver%A); TYPE IS(PETScMatrixType)
        SELECTTYPE(B=>solver%B); TYPE IS(PETScMatrixType)
          IF (.NOT.(A%isAssembled)) CALL A%assemble()
          IF (.NOT.(B%isAssembled)) CALL B%assemble()
          CALL EPSSetOperators(solver%eps,A%A,B%A,ierr)
        ENDSELECT
      ENDSELECT

      CALL EPSSetTolerances(solver%eps,solver%tol,solver%maxit,ierr)
      CALL EPSSolve(solver%eps,ierr)

      CALL EPSGetEigenvalue(solver%eps,0,kr,ki,ierr)
      solver%k=REAL(kr,SRK)

      SELECTTYPE(x=>solver%x); TYPE IS(PETScVectorType)
        CALL EPSGetEigenvector(solver%eps,0,x%b,solver%xi%b,ierr)
      ENDSELECT

      !repurposing ki and xi since it isn't needed
      CALL solver%xi%set(0.0_SRK)
      CALL BLAS_matvec(THISMATRIX=solver%A,X=solver%x,Y=solver%xi)
      CALL VecSum(solver%xi%b,ki,ierr)
      CALL BLAS_scal(THISVECTOR=solver%X,A=1.0_SRK/REAL(ki,SRK))
#endif
    ENDSUBROUTINE solve_EigenvalueSolverType_SLEPc
!
!-------------------------------------------------------------------------------
!> @brief Solves the Anasazi Eigen Solver Type
!> @param solver The eigen solver to act on
!>
!> This routine solves the eigenvalue system
!>
    SUBROUTINE solve_EigenvalueSolverType_Anasazi(solver)
      CLASS(EigenvalueSolverType_Anasazi),INTENT(INOUT) :: solver
#ifdef MPACT_HAVE_Trilinos
      REAL(SRK) :: factor
      REAL(SRK) :: tmp(2)
      SELECTTYPE(A=>solver%A); TYPE IS(TrilinosMatrixType)
        SELECTTYPE(B=>solver%B); TYPE IS(TrilinosMatrixType)
          IF (.NOT.(A%isAssembled)) CALL A%assemble()
          IF (.NOT.(B%isAssembled)) CALL B%assemble()
          IF(solver%setupPC) THEN
            CALL Preconditioner_Setup(solver%pc,B%A)
            solver%setupPC=.FALSE.
            solver%updatePC=.FALSE.
          ELSEIF(solver%updatePC) THEN
            !CALL Preconditioner_Reset(solver%pc,B%A)
            solver%updatePC=.FALSE.
          ENDIF

          CALL Anasazi_SetMat(solver%eig,A%A,B%A)
          !IF(solver%tmpcnt==2) THEN
          !  CALL ForPETRA_MatEdit(B%A,"M.mtx"//C_NULL_CHAR);
          !  CALL ForPETRA_MatEdit(A%A,"F.mtx"//C_NULL_CHAR);
          !ENDIF
        ENDSELECT
      ENDSELECT

      !TODO: set tolerance
      CALL Anasazi_SetPC(solver%eig,solver%pc)
      CALL Anasazi_Solve(solver%eig)
      CALL Anasazi_GetEigenvalue(solver%eig,solver%k)

      !renormalize
      CALL solver%x_scale%set(0.0_SRK)
      CALL BLAS_matvec(THISMATRIX=solver%A,X=solver%x,Y=solver%x_scale)
      factor=BLAS_asum(solver%x_scale)/REAL(solver%n,SRK)
      CALL BLAS_scal(THISVECTOR=solver%X,A=1.0_SRK/factor)
#endif
    ENDSUBROUTINE solve_EigenvalueSolverType_Anasazi
!
!-------------------------------------------------------------------------------
!> @brief Clears the SNES Eigen Solver Type
!> @param solver The eigen solver to act on
!>
!> This routine clears the data spaces
!>
    SUBROUTINE solve_EigenvalueSolverType_SNES(solver)
      CLASS(EigenvalueSolverType_SNES),INTENT(INOUT) :: solver
#ifdef MPACT_HAVE_PETSC
      REAL(SRK) :: tmp(2)

      SELECTTYPE(A=>solver%A); TYPE IS(PETScMatrixType)
        SELECTTYPE(B=>solver%B); TYPE IS(PETScMatrixType)
          IF (.NOT.(A%isAssembled)) CALL A%assemble()
          IF (.NOT.(B%isAssembled)) CALL B%assemble()
        ENDSELECT
      ENDSELECT

      !CALL SNESSetTolerances(solver%eps,solver%tol,solver%maxit,ierr)
      !CALL tell SNES to use PETSC_SHELL_CALCRESID_extern
      !CALL SNESSolve(solver%eps,ierr)

      !CALL SNESGetEigenvalue(solver%eps,0,kr,ki,ierr)

      !repurposing ki and xi since it isn't needed
      !TODO make your own coefficients
      !CALL solver%xi%set(0.0_SRK)
      !CALL BLAS_matvec(THISMATRIX=solver%A,X=solver%x,Y=solver%xi)
      !CALL VecSum(solver%xi%b,ki,ierr)
      !CALL BLAS_scal(THISVECTOR=solver%X,A=1.0_SRK/REAL(ki,SRK))
#endif
    ENDSUBROUTINE solve_EigenvalueSolverType_SNES
!
!-------------------------------------------------------------------------------
!> @brief Clears the SNES Eigen Solver Type
!> @param solver The eigen solver to act on
!>
!> This routine clears the data spaces
!>
    SUBROUTINE calcResid_EigenvalueSolverType_SNES(solver,v)
      CLASS(EigenvalueSolverType_SNES),INTENT(INOUT) :: solver
      CLASS(PETScVectorType),INTENT(INOUT) :: v
#ifdef MPACT_HAVE_PETSC
      !TODO  v = Bv/sum(Bv)-Av
#endif
    ENDSUBROUTINE calcResid_EigenvalueSolverType_SNES

#ifdef MPACT_HAVE_PETSC
    SUBROUTINE PETSC_SHELL_CALCRESID_extern(snes,x,f,dummy,err)
      SNES :: snes
      Vec :: x
      Vec :: f
      INTEGER :: dummy
      PetscErrorCode :: err

      PetscErrorCode :: ierr
      INTEGER(SIK) :: n
      TYPE(PETScVectorType) :: v

      err=-1
      IF(ASSOCIATED(PETSC_SHELL_EVS) .AND. PETSC_SHELL_EVS%isInit) THEN
        n=0
        CALL VecGetSize(x,n,ierr)
        IF(ierr==0) CALL VecCopy(x,f,ierr)

        v%isInit=.TRUE.
        v%n=n
        v%b=f

        err=ierr
        SELECTTYPE(myEVS=>PETSC_SHELL_EVS); CLASS IS(EigenvalueSolverType_SNES)
          IF(err==0) CALL myEVS%calcResid(v)
        ENDSELECT
      ENDIF

    ENDSUBROUTINE PETSC_SHELL_CALCRESID_extern
#endif

!TEST CODE:  DO NOT CALL
    SUBROUTINE testJFNKNOX()
      !create x and F (need to be saved in global scope of module)
      !  use PE_COMM_SELF for comm
      !initialize JFNK
      !  CALL JFNK_Init(id,C_FUNLOC(testcalcResid),idx,idF)
      !CALL solve
      !destroy JFNK
    ENDSUBROUTINE testJFNKNOX

    SUBROUTINE testcalcResid() BIND(C)
      !Do matvec on dummy J
      !F=J*x
    ENDSUBROUTINE testcalcResid
!
ENDMODULE EigenvalueSolverTypes
