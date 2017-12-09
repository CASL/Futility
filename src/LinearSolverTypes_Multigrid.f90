!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module provides a linear system type and methods to solve systems
!> of equations via a multigrid method
!>
!> For valid reference lists
!> see @ref MatrixTypes::LinearSolverTypes_Declare_ValidParams
!> "LinearSolverTypes_Declare_ValidParams".
!>
!> Currently supported TPLs include:
!>  - PETSc (with interfaces to KSP)
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
!> @author Ben C. Yee
!>   @date 08/22/2017
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE LinearSolverTypes_Multigrid
  USE IntrType
  USE ExceptionHandler
  USE ParameterLists
  USE VectorTypes
  USE MatrixTypes
  USE LinearSolverTypes
  USE MultigridMesh
  USE SmootherTypes
  IMPLICIT NONE
  PRIVATE

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
!petscisdef.h defines the keyword IS, and it needs to be reset
#undef IS
#endif
!
! List of public members
  PUBLIC :: LinearSolverType_Multigrid

  INTEGER(SIK),PARAMETER,PUBLIC :: MAX_MG_LEVELS=8_SIK

  !!Public enumerations from LSTypes
  PUBLIC :: PETSC
  PUBLIC :: MULTIGRID
  !Krylov smoother/solver options:
  PUBLIC :: BICGSTAB,GMRES
  !Fixed point smoother options:
  PUBLIC :: SOR,BJACOBI,JACOBI,CBJ
  !Direct solver options:
  PUBLIC :: LU

  !> @brief The extended type for the Iterative Linear Solver
  TYPE,EXTENDS(LinearSolverType_Iterative) :: LinearSolverType_Multigrid
    !> Number of grids:
    INTEGER(SIK) :: nLevels=1_SIK
    !> Whether or not the restriciton, interpolation, and smoothing is ready:
    LOGICAL(SBK) :: isMultigridSetup=.FALSE.
    !> Size of each grid. level_info(:,level) = (/num_eqns,npts/)
    INTEGER(SIK),ALLOCATABLE :: level_info(:,:)
    !> Size of each grid locally. level_info(:,level) = (/num_eqns_local,npts_local/)
    INTEGER(SIK),ALLOCATABLE :: level_info_local(:,:)
#ifdef FUTILITY_HAVE_PETSC
    !> Array of PETSc interpolation matrices
    TYPE(PETScMatrixType),ALLOCATABLE :: interpMats_PETSc(:)
#endif

    CONTAINS
      !> @copybrief LinearSolverType_Multigrid::init_LinearSolverType_Multigrid
      !> @copydetails LinearSolverType_Multigrid::init_LinearSolverType_Multigrid
      PROCEDURE,PASS :: init => init_LinearSolverType_Multigrid
      !> @copybrief LinearSolverType_Multigrid::preAllocPETScInterpMat_LinearSolverType_Multigrid
      !> @copydetails LinearSolverType_Multigrid::preAllocPETScInterpMat_LinearSolverType_Multigrid
      PROCEDURE,PASS :: preAllocPETScInterpMat => &
                          preAllocPETScInterpMat_LinearSolverType_Multigrid
      !> @copybrief LinearSolverType_Multigrid::setupPETScMG_LinearSolverType_Multigrid
      !> @copydetails LinearSolverType_Multigrid::setupPETScMG_LinearSolverType_Multigrid
      PROCEDURE,PASS :: setupPETScMG => setupPETScMG_LinearSolverType_Multigrid
      !> @copybrief LinearSolverType_Multigrid::fillInterpMats_LinearSolverType_Multigrid
      !> @copydetails LinearSolverType_Multigrid::fillInterpMats_LinearSolverType_Multigrid
      PROCEDURE,PASS :: fillInterpMats => fillInterpMats_LinearSolverType_Multigrid
      !> @copybrief  LinearSolverType_Multigrid::solve_LinearSolverType_Multigrid
      !> @copydetails LinearSolverType_Multigrid::solve_LinearSolverType_Multigrid
      PROCEDURE,PASS :: solve => solve_LinearSolverType_Multigrid
      !> @copybrief  LinearSolverType_Multigrid::clear_LinearSolverType_Multigrid
      !> @copydetails LinearSolverType_Multigrid::clear_LinearSolverType_Multigrid
      PROCEDURE,PASS :: clear => clear_LinearSolverType_Multigrid
      !> @copybrief  LinearSolverType_Multigrid::setSmoother_LinearSolverType_Multigrid
      !> @copydetails LinearSolverType_Multigrid::setSmoother_LinearSolverType_Multigrid
      PROCEDURE,PASS :: setSmoother => setSmoother_LinearSolverType_Multigrid
  ENDTYPE LinearSolverType_Multigrid

  !> Logical flag to check whether the required and optional parameter lists
  !> have been created yet for the Linear Solver Type.
  LOGICAL(SBK),SAVE :: LinearSolverType_Paramsflag=.FALSE.

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='LINEARSOLVERTYPES_MULTIGRID'
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the multigrid Linear Solver Type with a parameter list
!>
!> @param solver The linear solver to act on
!> @param Params the parameter list
!> @param A The A operator in Ax=b
!>
    SUBROUTINE init_LinearSolverType_Multigrid(solver,Params,A)
      CHARACTER(LEN=*),PARAMETER :: myName='init_LinearSolverType_Multigrid'
      CLASS(LinearSolverType_Multigrid),INTENT(INOUT) :: solver
      TYPE(ParamType),INTENT(IN) :: Params
      CLASS(MatrixType),POINTER,INTENT(INOUT),OPTIONAL :: A
      CLASS(ParamType),POINTER :: pListPtr
      TYPE(ParamType) :: validParams,matPList,vecxPList,vecbPList
      ! local variables
      INTEGER(SIK) :: n,num_eqns,nx,ny,nz,nlocal
      INTEGER(SIK) :: iLevel
      INTEGER(SIK) :: TPLType
      INTEGER(SIK) :: matType,matEngine
      INTEGER(SIK) :: MPI_Comm_ID,numberOMP
      LOGICAL(SBK) :: manuallySetLevelInfo
      CHARACTER(LEN=256) :: timerName
#ifdef FUTILITY_HAVE_PETSC
      PetscErrorCode :: iperr
#else
      CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
        "For now, LinearSolverType_Multigrid requires PETSc enabled.")
#endif
      !Check to set up required and optional param lists.
      IF(.NOT.LinearSolverType_Paramsflag) CALL LinearSolverType_Declare_ValidParams()

      !Validate against the reqParams and OptParams
      validParams=Params
      CALL validParams%validate(LinearSolverType_reqParams)

      !Pull LS data from the parameter list
      TPLType=-1
      MPI_Comm_ID=-1
      matType=-1
      matEngine=-1
      timerName=''
      numberOMP=-1
      CALL validParams%get('LinearSolverType->TPLType',TPLType)
      CALL validParams%get('LinearSolverType->MPI_Comm_ID',MPI_Comm_ID)
      CALL validParams%get('LinearSolverType->timerName',timerName)
      CALL validParams%get('LinearSolverType->matType',matType)
      CALL validParams%add('LinearSolverType->A->MatrixType->matType',matType)
      CALL validParams%get('LinearSolverType->numberOMP',numberOMP)
      ! pull data for matrix and vector parameter lists
      CALL validParams%get('LinearSolverType->A->MatrixType',pListPtr)
      matPList=pListPtr
      CALL validParams%get('LinearSolverType->x->VectorType',pListPtr)
      vecxPList=pListPtr
      CALL validParams%get('LinearSolverType->b->VectorType',pListPtr)
      vecbPList=pListPtr
      !add mpi communicator to parameter lists
      CALL matPList%add('MatrixType->MPI_Comm_ID',MPI_Comm_ID)
      CALL vecxPList%add('VectorType->MPI_Comm_ID',MPI_Comm_ID)
      CALL vecbPList%add('VectorType->MPI_Comm_ID',MPI_Comm_ID)
      !pull size from source vector
      CALL validParams%get('LinearSolverType->b->VectorType->n',n)
      CALL validParams%get('LinearSolverType->b->VectorType->nlocal',nlocal)

      CALL validParams%clear()

      !Initialize parallel environments based on input
      IF(MPI_Comm_ID /= -1) CALL solver%MPIparallelEnv%init(MPI_Comm_ID)
      IF(numberOMP > 0) CALL solver%OMPparallelEnv%init(numberOMP)

      IF(TPLType /= PETSC) THEN
        CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
          "For now, LinearSolverType_Multigrid only works with PETSC.")
      ENDIF

      solver%solverMethod=MULTIGRID

      IF(.NOT.solver%isInit) THEN
        solver%info=0
        IF(TPLType == PETSC) THEN
#ifdef FUTILITY_HAVE_PETSC
          solver%TPLType=PETSC
          matEngine=VM_PETSC

          !Should be irrelevant if using PETSc:
          solver%PCTypeName='NOPC'
          solver%pciters=0
          solver%pcsetup=0

          CALL matPList%add("MatrixType->engine",matEngine)
          ! allocate and initialize matrix (A)
          CALL MatrixFactory(solver%A, matPList)
          IF(PRESENT(A)) A=>solver%A

          CALL vecxPList%add('VectorType->engine',matEngine)
          CALL VectorFactory(solver%X, vecxPlist)
          CALL vecbPList%add('VectorType->engine',matEngine)
          CALL VectorFactory(solver%b, vecbPlist)


          CALL KSPCreate(solver%MPIparallelEnv%comm,solver%ksp,iperr)
          SELECTTYPE(A=>solver%A); TYPE IS(PETScMatrixType)
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=5))
            CALL KSPSetOperators(solver%ksp,A%a,A%a,iperr)
#else
            CALL KSPSetOperators(solver%ksp,A%a,A%a, &
              DIFFERENT_NONZERO_PATTERN,iperr)
#endif
          ENDSELECT
#endif

        ENDIF

        !assign values to solver
        CALL solver%SolveTime%setTimerName(timerName)
        solver%isInit=.TRUE.
        solver%isMultigridSetup=.FALSE.

        !Figure out coarsening scheme:
        manuallySetLevelInfo=.TRUE.
        IF(Params%has('LinearSolverType->Multigrid->manuallySetLevelInfo')) THEN
          CALL Params%get('LinearSolverType->Multigrid->manuallySetLevelInfo', &
                          manuallySetLevelInfo)
        ENDIF
        IF(manuallySetLevelInfo) THEN
          IF(.NOT.(Params%has('LinearSolverType->Multigrid->nLevels'))) &
            CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
                   'Number of levels (nLevels) not provided!')
          IF(.NOT.(Params%has('LinearSolverType->Multigrid->level_info_local'))) &
            CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
                   'level_info_local not provided!')
          IF(.NOT.(Params%has('LinearSolverType->Multigrid->level_info'))) &
            CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
                   'level_info not provided!')

          CALL Params%get('LinearSolverType->Multigrid->nLevels',solver%nLevels)
          ALLOCATE(solver%level_info_local(2,solver%nLevels))
          CALL Params%get('LinearSolverType->Multigrid->level_info_local', &
                              solver%level_info_local)
          ALLOCATE(solver%level_info(2,solver%nLevels))
          CALL Params%get('LinearSolverType->Multigrid->level_info', &
                              solver%level_info)
        ELSE
          !The code in this else statement is mostly for testing.  It's an
          !  example of how to define the levels for a simple 1D problem on 1
          !  processor.  For any realistic (2D/3D or parallel) problem,
          !  level_info_local and level_info should be provided to linear
          !  solver.  It is too difficult to generalize this for all types of
          !  problems.
          CALL Params%get('LinearSolverType->Multigrid->nx_local',nx)
          CALL Params%get('LinearSolverType->Multigrid->ny_local',ny)
          CALL Params%get('LinearSolverType->Multigrid->nz_local',nz)
          CALL Params%get('LinearSolverType->Multigrid->num_eqns',num_eqns)

          !Number of levels required to reduce down to ~5*num_eqns unknowns per processor:
          solver%nLevels=FLOOR(log(MAX(nx-1,ny-1,nz-1)/5.0_SRK)/log(2.0_SRK))+1
          solver%nLevels=MIN(solver%nLevels,MAX_MG_LEVELS)
          solver%nLevels=MAX(1,solver%nLevels)
          IF(solver%nLevels < 2) &
              CALL eLinearSolverType%raiseDebug(modName//"::"//myName//" - "// &
                     'The grid is too small to coarsen, using multigrid with '// &
                     ' only 1 level!')
          ALLOCATE(solver%level_info_local(2,solver%nLevels))
          solver%level_info_local(:,solver%nLevels)=(/num_eqns,nx*ny*nz/)
          DO iLevel=solver%nLevels-1,1,-1
          !Setup the interpolation operator:
            nx=(nx+1)/2
            ny=(ny+1)/2
            nz=(nz+1)/2
            solver%level_info_local(:,iLevel)=(/num_eqns,nx*ny*nz/)
          ENDDO !iLevel
          ALLOCATE(solver%level_info(2,solver%nLevels))
          solver%level_info=solver%level_info_local
        ENDIF !manuallySetLevelInfo

        !Sanity check:
        IF(PRODUCT(solver%level_info(:,solver%nLevels)) /= n) THEN
          CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
                 'number of unknowns (n) does not match provided '// &
                 'npts,num_eqns')
        ENDIF

#ifdef FUTILITY_HAVE_PETSC
        ALLOCATE(solver%interpMats_PETSc(solver%nLevels-1))
#endif
      ELSE
        CALL eLinearSolverType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - LinearSolverType already initialized')
      ENDIF
      CALL vecbPList%clear()
      CALL vecxPList%clear()
      CALL matPList%clear()
    ENDSUBROUTINE init_LinearSolverType_Multigrid
!
!-------------------------------------------------------------------------------
!> @brief Initialize and preallocate memory for PETSc interpolation matrices
!>
!> @param solver The linear solver to act on
!> @param iLevel The matrix we are allocating interpolates from grid iLevel-1
!>        to grid iLevel
!> @param dnnz dnnz(i) must provide the number of nonzero columns local to the
!>             processor in local row i
!> @param onnz_in onnz_in(i) should provide the number of nonzero columns
!>             external to the processor in local row i
!>
    SUBROUTINE preAllocPETScInterpMat_LinearSolverType_Multigrid(solver, &
       iLevel,dnnz,onnz_in)
      CHARACTER(LEN=*),PARAMETER :: myName='preAllocPETScInterpMat_LinearSolverType_Multigrid'
      CLASS(LinearSolverType_Multigrid),INTENT(INOUT) :: solver
      INTEGER(SIK),INTENT(IN) :: iLevel,dnnz(:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: onnz_in(:)
      INTEGER(SIK),ALLOCATABLE :: onnz(:)
      INTEGER(SIK) :: npts,num_eqns,n
      INTEGER(SIK) :: npts_old,num_eqns_old,n_old

      TYPE(ParamType) :: matPList
      CLASS(MatrixType),POINTER :: interpmat => NULL()

      ALLOCATE(onnz(SIZE(dnnz)))
      IF(PRESENT(onnz_in)) THEN
        onnz=onnz_in
      ELSE
        onnz=0_SIK
      ENDIF

#ifdef FUTILITY_HAVE_PETSC
      IF(solver%isInit) THEN
        num_eqns=solver%level_info(1,iLevel)
        npts=solver%level_info(2,iLevel)
        n=npts*num_eqns

        num_eqns_old=solver%level_info(1,iLevel+1)
        npts_old=solver%level_info(2,iLevel+1)
        n_old=npts_old*num_eqns_old

        CALL matPList%clear()
        CALL matPList%add('MatrixType->matType',SPARSE)
        CALL matPList%add('MatrixType->engine',VM_PETSC)
        CALL matPList%add('MatrixType->MPI_Comm_ID',solver%MPIparallelEnv%comm)
        CALL matPList%add('MatrixType->isSym',.FALSE.)
        CALL matPList%add('MatrixType->n',n_old)
        CALL matPList%add('MatrixType->m',n)

        num_eqns=solver%level_info_local(1,iLevel)
        npts=solver%level_info_local(2,iLevel)
        n=npts*num_eqns
        num_eqns_old=solver%level_info_local(1,iLevel+1)
        npts_old=solver%level_info_local(2,iLevel+1)
        n_old=npts_old*num_eqns_old
        CALL matPList%add('MatrixType->nlocal',n_old)
        CALL matPList%add('MatrixType->mlocal',n)

        CALL matPList%add('MatrixType->onnz',onnz)
        CALL matPList%add('MatrixType->dnnz',dnnz)
        !CALL matPList%add('MatrixType->nnz',SUM(dnnz)+SUM(onnz))
        CALL MatrixFactory(interpmat,matPList)

        !Store this matrix object:
        SELECTTYPE(interpmat); TYPE IS(PETScMatrixType)
          solver%interpMats_PETSc(iLevel)=interpmat
        ENDSELECT

        NULLIFY(interpmat)
      ELSE
        CALL eLinearSolverType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - LinearSolverType must be initialized')
      ENDIF
#else
      CALL eLinearSolverType%raiseError('Incorrect call to '// &
        modName//'::'//myName//' - This subroutine can only be called if '// &
        'PETSc is not enabled.')
#endif

      CALL matPList%clear()
      DEALLOCATE(onnz)

    ENDSUBROUTINE preAllocPETScInterpMat_LinearSolverType_Multigrid
!
!-------------------------------------------------------------------------------
!> @brief Allocate and fill in the PETSc interpolation matrices from a multigrid
!>          mesh hierachy object
!>
!> @param solver The linear solver to act on
!> @param myMMeshes Multigrid mesh hierachy with information regarding weights
!>          and neighbors for each mesh
!> @param preallocated Whether the interpolation matrices are already allocated
!>          This is false by default
!>
    SUBROUTINE fillInterpMats_LinearSolverType_Multigrid(solver,myMMeshes,preallocated)
      CHARACTER(LEN=*),PARAMETER :: myName='fillInterpMats_LinearSolverType_Multigrid'
      CLASS(LinearSolverType_Multigrid),INTENT(INOUT) :: solver
      TYPE(MultigridMeshStructureType),INTENT(IN) :: myMMeshes
      LOGICAL(SBK),OPTIONAL :: preallocated

      !Need to increase size of indices to 48 if 3-D is allowed:
      INTEGER(SIK) :: iLevel,ip,i,row,col,ieqn,indices(8),nindices
      REAL(SRK),ALLOCATABLE :: wts(:,:)

#ifdef FUTILITY_HAVE_PETSC
      IF(solver%TPLType /= PETSC) &
        CALL eLinearSolverType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - This subroutine does not have a '// &
          'non-PETSc implementation yet.')

      IF(solver%nLevels /= myMMeshes%nLevels) &
        CALL eLinearSolverType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - Mismatch in grid and solver nLevels.')

      DO iLevel=solver%nLevels,2,-1
        !Allocate the interpolation matrix:
        IF(.NOT.PRESENT(preallocated) .OR. .NOT.preallocated) THEN
          CALL solver%preAllocPETScInterpMat(iLevel-1, &
                  2**myMMeshes%meshes(iLevel)%interpDegrees)
          !Note that if there is interpolation across processors, this does not
          !  work
        ENDIF
        ALLOCATE(wts(myMMeshes%meshes(iLevel)%num_eqns,8))
        !Create the interpolation operator:
        DO ip=myMMeshes%meshes(iLevel)%istt,myMMeshes%meshes(iLevel)%istp
          CALL getFinalWtsAndIndices(myMMeshes%meshes(iLevel), &
            myMMeshes%meshes(iLevel)%num_eqns,ip,indices,wts,nindices)
          DO ieqn=1,myMMeshes%meshes(iLevel)%num_eqns
            row=(ip-1)*myMMeshes%meshes(iLevel)%num_eqns+ieqn
            DO i=1,nindices
              col=(indices(i)-1)*myMMeshes%meshes(iLevel)%num_eqns+ieqn
              CALL solver%interpMats_PETSc(iLevel-1)%set(row,col,wts(ieqn,i))
            ENDDO
          ENDDO
        ENDDO
        DEALLOCATE(wts)
      ENDDO !iLevel
#else
      CALL eLinearSolverType%raiseError('Incorrect call to '// &
        modName//'::'//myName//' - This subroutine does not have a non-PETSc'// &
        'implementation yet.')
#endif
    ENDSUBROUTINE fillInterpMats_LinearSolverType_Multigrid
!
!-------------------------------------------------------------------------------
!> @brief Setup the PCMG environment in PETSc, finalize the interpolation operators
!>
!> @param solver The linear solver to act on
!> @param Params the parameter list
!>
    SUBROUTINE setupPETScMG_LinearSolverType_Multigrid(solver,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='setupPETScMG_LinearSolverType_Multigrid'
      CLASS(LinearSolverType_Multigrid),INTENT(INOUT) :: solver
      TYPE(ParamType),INTENT(IN) :: Params
      INTEGER(SIK) :: iLevel
      INTEGER(SIK),ALLOCATABLE :: smootherMethod_list(:)
      INTEGER(SIK) :: num_mg_coarse_its
#ifdef FUTILITY_HAVE_PETSC
      KSP :: ksp_temp
      PetscErrorCode  :: iperr

      IF(solver%TPLType /= PETSC) &
        CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
          "This subroutine should only be called with PETSc.")

      IF(solver%isMultigridSetup) &
          CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
                 'Multigrid linear system is already setup!')

      ALLOCATE(smootherMethod_list(solver%nLevels))
      IF(Params%has('LinearSolverType->smootherMethod_list')) THEN
        CALL Params%get('LinearSolverType->smootherMethod_list', &
                smootherMethod_list)
      ELSE
        smootherMethod_list(1)=GMRES
        smootherMethod_list(2:solver%nLevels)=SOR
      ENDIF

      !KSPRICHARDSON+PCMG = Multigrid linear solver, not multigrid precon.
      CALL KSPSetType(solver%ksp,KSPRICHARDSON,iperr)
      CALL KSPGetPC(solver%ksp,solver%pc,iperr)
      CALL PCSetType(solver%pc,PCMG,iperr)

      !For now, only Galerkin coarse grid operators are supported.
      !  Galerkin means A_c = R*A*I
      CALL PCMGSetGalerkin(solver%pc,PETSC_TRUE,iperr)
      CALL KSPSetInitialGuessNonzero(solver%ksp,PETSC_TRUE,iperr)

      !Set # of levels:
      CALL PCMGSetLevels(solver%pc,solver%nLevels,PETSC_NULL_OBJECT,iperr)

      !Need a smoother on all levels except the coarsest:
      DO iLevel=solver%nLevels-1,1,-1
        CALL solver%setSmoother(smootherMethod_list(iLevel+1),iLevel)

        !Set the interpolation operator:
        CALL solver%interpMats_PETSc(iLevel)%assemble()
        CALL PCMGSetInterpolation(solver%pc,iLevel,solver%interpMats_PETSc(iLevel)%a,iperr)
      ENDDO

      iLevel=0
      CALL solver%setSmoother(smootherMethod_list(iLevel+1),iLevel)
      IF(smootherMethod_list(iLevel+1) == LU) THEN
        num_mg_coarse_its=1
      ELSE IF(Params%has('LinearSolverType->num_mg_coarse_its')) THEN
        CALL Params%get('LinearSolverType->num_mg_coarse_its', &
                num_mg_coarse_its)
      ELSE
        !Some reasonable number of GMRES iterations:
        num_mg_coarse_its=CEILING(SQRT(PRODUCT(solver%level_info(:,1))+0._SRK))
        WRITE(*,*) "level_info = ", solver%level_info
        WRITE(*,*) "num_mg_coarse_its = ", num_mg_coarse_its
      ENDIF
      !None of the tolerances actually matter since PCMG doesn't check smoothers
      !  for convergence.  It just runs until the maximum number of iterations.
      CALL PCMGGetSmoother(solver%pc,0,ksp_temp,iperr)
      CALL KSPSetTolerances(ksp_temp,cg_tol,cg_tol,1.E3_SRK,cg_solver_its,iperr)
      CALL KSPGMRESSetRestart(ksp_temp,cg_solver_its,iperr)

      !Not sure if the tolerance actually matters on the outer level.  This
      !  call is mostly to set a limit on the number of MG V-cycles performed.
      CALL KSPSetTolerances(solver%ksp,1.E-10_SRK,1.E-10_SRK,1.E3_SRK,10_SIK,iperr)

      !Set cycle type to V:
      CALL PCMGSetCycleType(solver%pc,PC_MG_CYCLE_V,iperr)

      CALL PetscOptionsSetValue("-pc_mg_log",PETSC_NULL_CHARACTER,iperr)
      CALL PCSetFromOptions(solver%pc,iperr)

      solver%isMultigridSetup=.TRUE.
#else
      CALL eLinearSolverType%raiseError('Incorrect call to '// &
        modName//'::'//myName//' - This subroutine can only be called if '// &
        'PETSc is not enabled.')
#endif
    ENDSUBROUTINE setupPETScMG_LinearSolverType_Multigrid
!
!-------------------------------------------------------------------------------
!> @brief Define smoother options
!>
!> @param solver The linear solver to act on
!> @param smoother The type to set the smoother to
!> @param iLevel level index, in PETSc notation where 0=coarsest
!>
    SUBROUTINE setSmoother_LinearSolverType_Multigrid(solver,smoother,iLevel)
      CHARACTER(LEN=*),PARAMETER :: myName='setSmoother_LinearSolverType_Multigrid'
      CLASS(LinearSolverType_Multigrid),INTENT(INOUT) :: solver
      INTEGER(SIK),INTENT(IN) :: smoother
      INTEGER(SIK),INTENT(IN),OPTIONAL :: iLevel
      INTEGER(SIK) :: i,istt,istp

#ifdef FUTILITY_HAVE_PETSC
      KSP :: ksp_temp
      PC :: pc_temp
      PetscErrorCode  :: iperr
#endif

#ifdef FUTILITY_HAVE_PETSC
      IF(solver%TPLType /= PETSC) &
        CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
          "This subroutine should only be called with PETSc.")

      !TODO when a non-petsc version is implemented, add a variable to track
      !  type of solver being used (e.g., petsc vs non-petsc)

      !By default, all levels will be set to the specified smoother except
      !  the coarsest (i=0)
      istt=1
      istp=solver%nLevels-1
      IF(PRESENT(iLevel)) THEN
        istt=iLevel
        istp=iLevel
        IF(iLevel >= solver%nLevels) THEN
          CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
            "iLevel must be strictly smaller than the number of levels!")
        ENDIF
      ENDIF

      DO i=istt,istp
        CALL PCMGGetSmoother(solver%pc,i,ksp_temp,iperr)

        IF(smoother == CBJ) THEN
          IF(isSmootherListInit) THEN
            CALL smootherManager_setKSP(i+1,ksp_temp)
          ELSE
            CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
              "Smoother list must be initialized before any smoothers can be"// &
              " set to CBJ!")
          ENDIF
        ELSEIF(smoother == SOR) THEN
          CALL KSPSetType(ksp_temp,KSPRICHARDSON,iperr)
          CALL KSPGetPC(ksp_temp,pc_temp,iperr)
          CALL PCSetType(pc_temp,PCSOR,iperr)
        ELSEIF(smoother == GMRES) THEN
          CALL KSPSetType(ksp_temp,KSPGMRES,iperr)
          CALL KSPGetPC(ksp_temp,pc_temp,iperr)
          CALL PCSetType(pc_temp,PCBJACOBI,iperr)
        ELSEIF(smoother == BICGSTAB) THEN
          CALL KSPSetType(ksp_temp,KSPBCGS,iperr)
          CALL KSPGetPC(ksp_temp,pc_temp,iperr)
          CALL PCSetType(pc_temp,PCBJACOBI,iperr)
        ELSEIF(smoother == LU) THEN
        !We might want to loosen this to allow SUPERLU_MT (superLU for shared
        !  memory architectures)
#ifndef PETSC_HAVE_SUPERLU_DIST
          IF(solver%MPIparallelEnv%nproc > 1) &
            CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
              "Cannot use LU in parallel in PETSc without SUPERLU_DIST!")
#endif
          !Only for the coarsest level!
          IF(istt > 0) &
            CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
              "LU should only be used on the coarsest level!")
          CALL KSPSetType(ksp_temp,KSPPREONLY,iperr)
          CALL KSPGetPC(ksp_temp,pc_temp,iperr)
          CALL PCSetType(pc_temp,PCLU,iperr)
          IF(solver%MPIparallelEnv%nproc > 1) &
            CALL PCFactorSetMatSolverPackage(pc_temp,MATSOLVERSUPERLU_DIST,iperr)
        ELSEIF(smoother == BJACOBI) THEN
          CALL KSPSetType(ksp_temp,KSPRICHARDSON,iperr)
          CALL KSPGetPC(ksp_temp,pc_temp,iperr)
          CALL PCSetType(pc_temp,PCBJACOBI,iperr)
        ELSEIF(smoother == JACOBI) THEN
          CALL KSPSetType(ksp_temp,KSPRICHARDSON,iperr)
          CALL KSPGetPC(ksp_temp,pc_temp,iperr)
          CALL PCSetType(pc_temp,PCJACOBI,iperr)
        ELSE
          CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
            "Unrecognized smoother option!")
        ENDIF

        !On all levels except the finest, the initial guess should be zero
        !  since it is an error equation.
        IF(i == solver%nLevels-1) THEN
          CALL KSPSetInitialGuessNonzero(ksp_temp,PETSC_TRUE,iperr)
        ELSE
          CALL KSPSetInitialGuessNonzero(ksp_temp,PETSC_FALSE,iperr)
        ENDIF
      ENDDO
#endif

    ENDSUBROUTINE setSmoother_LinearSolverType_Multigrid
!
!-------------------------------------------------------------------------------
!> @brief Solves the Linear System with multigrid
!> @param solver The linear solver to act on
!>
!>
    SUBROUTINE solve_LinearSolverType_Multigrid(solver)
      CHARACTER(LEN=*),PARAMETER :: myName='solve_LinearSolverType_Multigrid'
      CLASS(LinearSolverType_Multigrid),INTENT(INOUT) :: solver

      IF(.NOT. solver%isMultigridSetup) &
        CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
          "Multigrid needs to be setup before it can be used to solve!")

      CALL solver%LinearSolverType_Iterative%solve()

    ENDSUBROUTINE solve_LinearSolverType_Multigrid
!
!-------------------------------------------------------------------------------
!> @brief Clears the Multigrid Linear Solver Type
!> @param solver The linear solver to act on
!>
!> This routine clears the data spaces for the iterative linear solver.
!>
    SUBROUTINE clear_LinearSolverType_Multigrid(solver)
      CLASS(LinearSolverType_Multigrid),INTENT(INOUT) :: solver

      INTEGER(SIK) :: iLevel

#ifdef FUTILITY_HAVE_PETSC
      IF(solver%isMultigridSetup) THEN
        DO iLevel=1,solver%nLevels-1
          CALL solver%interpMats_PETSc(iLevel)%clear()
        ENDDO
      ENDIF

      IF(ALLOCATED(solver%interpMats_PETSc)) DEALLOCATE(solver%interpMats_PETSc)
#endif

      solver%isMultigridSetup=.FALSE.
      IF(ALLOCATED(solver%level_info)) DEALLOCATE(solver%level_info)
      IF(ALLOCATED(solver%level_info_local)) DEALLOCATE(solver%level_info_local)
      solver%nLevels=1_SIK

      CALL solver%LinearSolverType_Iterative%clear()

    ENDSUBROUTINE clear_LinearSolverType_Multigrid
!
!-------------------------------------------------------------------------------
!> @brief Extracts the weights and neighbor indices for a given point on a mesh,
!>        and combines indices in preparation for assembly/filling of interp.
!>        matrix
!> @param myMesh MultigridMeshType object corresponding to the current level
!> @param ip Current point
!> @param indices Indices to be returned
!> @param wts Weights to be returned
!>
    SUBROUTINE getFinalWtsAndIndices(myMesh,num_eqns,ip,indices,wts,nindices)
      CLASS(MultigridMeshType),INTENT(IN) :: myMesh
      INTEGER(SIK),INTENT(IN) :: num_eqns,ip
      INTEGER(SIK),INTENT(INOUT) :: indices(:)
      REAL(SRK),INTENT(INOUT) :: wts(:,:)
      INTEGER(SIK),INTENT(OUT) :: nindices

      INTEGER(SIK) :: i,ind
      REAL(SRK) :: wt2(num_eqns),tmpwts(num_eqns,8)
      INTEGER(SIK) :: tmpindices(8),counter

      counter=1
      wt2=1.0_SRK
      CALL collectWtsAndIndices(myMesh,num_eqns,ip,tmpindices,tmpwts, &
                                 myMesh%interpDegrees(ip),counter,wt2)
      counter=counter-1

      indices=0_SIK
      nindices=1
      !i=1:
      indices(1)=tmpindices(1)
      wts(:,1)=tmpwts(:,1)
      !i>1:
      DO i=2,counter
        IF(tmpindices(i) < 1) CYCLE
        ind=MINLOC(indices(1:nindices),DIM=1,MASK=(indices(1:nindices)==tmpindices(i)))
        IF(ind == 0) THEN
          nindices=nindices+1
          indices(nindices)=tmpindices(i)
          wts(:,nindices)=tmpwts(:,i)
        ELSE
          wts(:,ind)=wts(:,ind)+tmpwts(:,i)
        ENDIF
      ENDDO

    ENDSUBROUTINE getFinalWtsAndIndices
!
!-------------------------------------------------------------------------------
!> @brief Extracts the weights and neighbor indices for a given point on a mesh
!>        by recursively interpolating until it reaches points with interpdegree
!>        equal to 0
!> @param myMesh MultigridMeshType object corresponding to the current level
!> @param ip Current point
!> @param indices Indices to be returned
!> @param wts Weights to be returned
!> @param interpdegree interpolation degree of the current point
!> @param counter current index on the indices and wts arrays
!> @param parentwt weight from parent, to be multiplied with new weights
!>
    RECURSIVE SUBROUTINE collectWtsAndIndices(myMesh,num_eqns,ip, &
                           indices,wts,interpdegree,counter,parentwt)
      CLASS(MultigridMeshType),INTENT(IN) :: myMesh
      INTEGER(SIK),INTENT(IN) :: num_eqns,ip,interpdegree
      INTEGER(SIK),INTENT(INOUT) :: indices(:),counter
      REAL(SRK),INTENT(IN) :: parentwt(:)
      REAL(SRK),INTENT(INOUT) :: wts(:,:)

      REAL(SRK) :: parentwt2(num_eqns)
      INTEGER(SIK) :: ipn,ichild

      IF(interpdegree == 0) THEN
        indices(counter)=myMesh%mmData(ip)%childIndices(1)
        wts(:,counter)=parentwt
        counter=counter+1
      ELSE
        DO ichild=1,2*interpdegree
          ipn=myMesh%mmData(ip)%childIndices(ichild)
          IF(ipn < 1) CYCLE
          parentwt2=myMesh%mmData(ip)%childWeights(:,ichild)*parentwt
          CALL collectWtsAndIndices(myMesh,num_eqns,ipn,indices,wts, &
                 myMesh%interpDegrees(ipn),counter,parentwt2)
        ENDDO
      ENDIF

    ENDSUBROUTINE

ENDMODULE LinearSolverTypes_Multigrid
