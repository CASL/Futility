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
  USE LinearSolverTypes
  IMPLICIT NONE

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

  INTEGER(SIK),PARAMETER :: max_levels=8_SIK

  !> @brief The extended type for the Iterative Linear Solver
  TYPE,EXTENDS(LinearSolverType_Iterative) :: LinearSolverType_Multigrid
    !> Number of grids:
    INTEGER(SIK) :: nLevels=1_SIK
    !> Whether or not the restriciton, interpolation, and smoothing is ready:
    LOGICAL(SBK) :: isMultigridSetup=.FALSE.
    !> Size of each grid level_info(level,:) = (/num_eqns,nx,ny,nz/)
    INTEGER(SIK),ALLOCATABLE :: level_info(:,:)
#ifdef FUTILITY_HAVE_PETSC
    !> Array of pointers to petsc interpolation matrices
    TYPE(PETScMatrixType),ALLOCATABLE :: interpMats(:)
#endif

    CONTAINS
      !> @copybrief TODO
      !> @copydetails TODO
      PROCEDURE,PASS :: init => init_LinearSolverType_Multigrid
      !> @copybrief TODO
      !> @copydetails TODO
      PROCEDURE,PASS :: setupInterpMats => setupInterpMats_LinearSolverType_Multigrid
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
      INTEGER(SIK) :: n
      INTEGER(SIK) :: TPLType
      INTEGER(SIK) :: matType,matEngine
      INTEGER(SIK) :: MPI_Comm_ID,numberOMP
      CHARACTER(LEN=256) :: timerName
#ifdef FUTILITY_HAVE_PETSC
      KSP :: ksp_temp
      PetscErrorCode  :: iperr
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

      CALL validParams%clear()

      !Initialize parallel environments based on input
      IF(MPI_Comm_ID /= -1) CALL solver%MPIparallelEnv%init(MPI_Comm_ID)
      IF(numberOMP > 0) CALL solver%OMPparallelEnv%init(numberOMP)

      IF(TPLType /= PETSC) THEN
        CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
          "For now, LinearSolverType_Multigrid only works with PETSC.")
      ENDIF

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

          solver%solverMethod=MULTIGRID

          CALL KSPCreate(solver%MPIparallelEnv%comm,solver%ksp,iperr)
          SELECTTYPE(A=>solver%A); TYPE IS(PETScMatrixType)
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=5))
            CALL KSPSetOperators(solver%ksp,A%a,A%a,iperr)
#else
            CALL KSPSetOperators(solver%ksp,A%a,A%a, &
              DIFFERENT_NONZERO_PATTERN,iperr)
#endif
          ENDSELECT

          !KSPRICHARDSON+PCMG = Multigrid linear solver, not multigrid precon.
          CALL KSPSetType(solver%ksp,KSPRICHARDSON,iperr)
          CALL KSPGetPC(solver%ksp,solver%pc,iperr)
          CALL PCSetType(solver%pc,PCMG,iperr)

          !For now, only Galerkin coarse grid operators are supported.
          !  Galerkin means A_c = R*A*I
          CALL PCMGSetGalerkin(solver%pc,PETSC_TRUE,iperr)

          !ZZZZ why doesnt this work for multigrid?
          !CALL KSPSetInitialGuessNonzero(solver%ksp,PETSC_TRUE,iperr)
#endif
        ENDIF

        !assign values to solver
        CALL solver%SolveTime%setTimerName(timerName)
        solver%isInit=.TRUE.
        solver%isMultigridSetup=.FALSE.

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
!> @brief Setup the interpolation and restriction matrices.
!>
!> @param solver The linear solver to act on
!> @param Params the parameter list
!>
    SUBROUTINE setupInterpMats_LinearSolverType_Multigrid(solver,Params,weights)
      CHARACTER(LEN=*),PARAMETER :: myName='init_LinearSolverType_Multigrid'
      CLASS(LinearSolverType_Multigrid),INTENT(INOUT) :: solver
      TYPE(ParamType),INTENT(IN) :: Params
      INTEGER(SIK) :: n,n_old,i,iLevel,nnz,MPI_Comm_ID
      !Number of coupled equations (in neutronics, # of groups)
      INTEGER(SIK) :: num_eqns
      !Number of dimensions
      INTEGER(SIK) :: num_dims
      !  Right now, this only works for 3-,5-, and 7-point cnetered finite
      !    difference stencils
      INTEGER(SIK) :: nx,ny,nz,nx_old,ny_old,nz_old
      INTEGER(SIK) :: inx,iny,inz,inum_eqn

      REAL(SRK),INTENT(IN),OPTIONAL :: weights(:,:,:,:)
      CLASS(MatrixType),POINTER :: interpmat => NULL()
      TYPE(ParamType) :: matPList
#ifdef FUTILITY_HAVE_PETSC
      INTEGER(SIK),ALLOCATABLE :: dnnz(:),onnz(:) !ZZZZ

      KSP :: ksp_temp
      PC :: pc_temp
      PetscErrorCode  :: iperr
      Mat :: mat_temp
#endif

      IF(solver%isMultigridSetup) &
          CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
                 'Multigrid linear system is already setup!')

      !Determine the weight type:
      CALL Params%get('LinearSolverType->Multigrid->nx',nx)
      CALL Params%get('LinearSolverType->Multigrid->ny',ny)
      CALL Params%get('LinearSolverType->Multigrid->nz',nz)
      CALL Params%get('LinearSolverType->MPI_Comm_ID',MPI_Comm_ID)
      num_dims=1
      IF(ny > 1) num_dims=num_dims+1
      IF(nz > 1) num_dims=num_dims+1
      CALL Params%get('LinearSolverType->Multigrid->num_eqns',num_eqns)
      CALL Params%get('LinearSolverType->b->VectorType->n',n)
      IF(nx*ny*nz*num_eqns /= n) THEN
          CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
                 'number of unknowns (n) does not match provided '// &
                 'nx,ny,nz,num_eqns')
      ENDIF

      !Number of levels required to reduce down to ~5*num_eqns unknowns per processor:
      solver%nLevels=FLOOR(log(MAX(nx-1,ny-1,nz-1)/ &
            solver%MPIParallelEnv%nproc/5.0_SRK)/log(2.0_SRK))+1
      solver%nLevels=MIN(solver%nLevels,max_levels)

      IF(solver%nLevels < 2) &
          CALL eLinearSolverType%raiseDebug(modName//"::"//myName//" - "// &
                 'The grid is too small to coarsen, using multigrid with '// &
                 ' only 1 level!')

      IF(solver%TPLType == PETSC) THEN
#ifdef FUTILITY_HAVE_PETSC

        !Set # of levels:
        CALL PCMGSetLevels(solver%pc,solver%nLevels,PETSC_NULL_OBJECT,iperr) !TODO use some sort of mpi thing here?
        ALLOCATE(solver%interpMats(solver%nLevels-1))

        CALL matPList%clear()
        CALL matPList%add('MatrixType->matType',SPARSE)
        CALL matPList%add('MatrixType->engine',VM_PETSC)
        CALL matPList%add('MatrixType->n',n)
        CALL matPList%add('MatrixType->m',n)
        CALL matPList%add('MatrixType->nlocal',n)
        CALL matPList%add('MatrixType->mlocal',n)
        ALLOCATE(dnnz(n))
        ALLOCATE(onnz(n))
        CALL matPList%add('MatrixType->onnz',onnz)
        CALL matPList%add('MatrixType->dnnz',dnnz)
        DEALLOCATE(dnnz)
        DEALLOCATE(onnz)
        CALL matPList%add('MatrixType->nnz',1)
        CALL matPList%add('MatrixType->MPI_Comm_ID',MPI_Comm_ID)
        CALL matPList%add('MatrixType->isSym',.FALSE.)
        nx_old=nx
        ny_old=ny
        nz_old=nz
        ALLOCATE(solver%level_info(solver%nLevels,4))
        solver%level_info(solver%nLevels,:)=(/num_eqns,nx,ny,nz/)
        DO iLevel=solver%nLevels-1,1,-1
          !Set the smoother:
          CALL PCMGGetSmoother(solver%pc,iLevel,ksp_temp,iperr)
          CALL KSPSetType(ksp_temp,KSPRICHARDSON,iperr)
          CALL KSPGetPC(ksp_temp,pc_temp,iperr)
          !TODO use PCBJACOBI and set block size
          CALL PCSetType(pc_temp,PCJACOBI,iperr)

          !Create the interpolation operator:
          nx_old=nx
          ny_old=ny
          nz_old=nz
          n_old=n
          IF(nx > 1) nx=nx/2+1
          IF(ny > 1) ny=ny/2+1
          IF(nz > 1) nz=nz/2+1
          n=nx*ny*nz*num_eqns
          CALL matPList%set('MatrixType->n',n_old)
          CALL matPList%set('MatrixType->m',n)
          CALL matPList%set('MatrixType->nlocal',n_old)
          CALL matPList%set('MatrixType->mlocal',n) !ZZZZ fix for parallel
          nnz=n
          IF(num_dims == 3) THEN
            nnz=nnz+((nx_old*ny_old*nz_old*num_eqns)-n)*6
          ELSEIF(num_dims == 2) THEN
            nnz=nnz+((nx_old*ny_old*nz_old*num_eqns)-n)*4 !ZZZZ what about the 2->1 part
          ELSE
            nnz=nnz+((nx_old*num_eqns)-n)*2 !ZZZZ what about the 2->1 part
          ENDIF
          CALL matPList%set('MatrixType->nnz',nnz)
          ALLOCATE(dnnz(n_old))
          ALLOCATE(onnz(n_old))
          onnz=0_SIK
          dnnz=0_SIK

          !To be used by the interp/restrict functions:
          solver%level_info(iLevel,:)=(/num_eqns,nx,ny,nz/)
          !Initialize interp, then set its values ZZZZ
          CALL MatrixFactory(interpmat,matPList)
          IF(.NOT. PRESENT(weights)) THEN !uniform weights will be used
            inx=1
            iny=1
            inz=1
            inum_eqn=1
            DO i=1,n_old
              IF(num_dims > 2) THEN
                !TODO
              ELSEIF(num_dims > 1) THEN
                !TODO !ZZZZ don't forget about the 2->1 part
              ELSE
                IF(XOR(MOD(inx,2)==1, (inx>nx/2 .AND. MOD(nx,2) == 0))) THEN
                  CALL interpmat%set(i,((inx+1)/2-1)*num_eqns+inum_eqn,1.0_SRK)
                  dnnz(i)=dnnz(i)+1
                ELSE
                  CALL interpmat%set(i,(inx/2-1)*num_eqns+inum_eqn,0.5_SRK)
                  CALL interpmat%set(i,(inx/2)*num_eqns+inum_eqn,0.5_SRK)
                  dnnz(i)=dnnz(i)+2
                ENDIF
              ENDIF

              !Track where we are:
              inum_eqn=inum_eqn+1
              IF(inum_eqn > num_eqns) THEN
                inum_eqn=1
                inx=inx+1
              ENDIF
              IF(inx>nx_old) THEN
                inx=1
                iny=iny+1
              ENDIF
              IF(iny>ny_old) THEN
                iny=1
                inz=inz+1
              ENDIF
            ENDDO
          ELSE
            !TODO
            CALL eLinearSolverType%raiseError(modName//"::"//myName//" - "// &
              "Nonuniform multigrid weights not implemented yet.")
          ENDIF
          CALL matPList%add('MatrixType->onnz',onnz)
          CALL matPList%add('MatrixType->dnnz',dnnz)
          DEALLOCATE(dnnz)
          DEALLOCATE(onnz)

          IF(ny == 1 .AND. ny_old > 1) num_dims=num_dims-1
          IF(nz == 1 .AND. nz_old > 1) num_dims=num_dims-1

          !Set the interpolation operator:
          SELECTTYPE(interpmat); TYPE IS(PETScMatrixType)
            CALL interpmat%assemble()
            CALL PCMGSetInterpolation(solver%pc,iLevel,interpmat%a,iperr)
            solver%interpMats(iLevel)=interpmat
          ENDSELECT
          DEALLOCATE(interpmat)
        ENDDO
        !Set coarsest smoother options:
        CALL PCMGGetSmoother(solver%pc,0,ksp_temp,iperr)
        CALL KSPSetType(ksp_temp,KSPGMRES,iperr)
        CALL KSPGetPC(ksp_temp,pc_temp,iperr)
        CALL PCSetType(pc_temp,PCBJACOBI,iperr)
        CALL KSPSetInitialGuessNonzero(ksp_temp,PETSC_TRUE,iperr)
#endif
      ENDIF
      solver%isMultigridSetup=.TRUE.

    ENDSUBROUTINE setupInterpMats_LinearSolverType_Multigrid
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
      PetscErrorCode :: iperr

      IF(solver%isMultigridSetup) THEN
        DO iLevel=1,solver%nLevels-1
          !CALL MatDestroy(solver%PetscInterpMats(iLevel),iperr) !ZZZZ
        ENDDO
      ENDIF
#endif

      solver%isMultigridSetup=.FALSE.
      IF(ALLOCATED(solver%level_info)) DEALLOCATE(solver%level_info)
      IF(ALLOCATED(solver%interpmats)) DEALLOCATE(solver%interpmats)
      solver%nLevels=1_SIK

      CALL solver%LinearSolverType_Iterative%clear()

    ENDSUBROUTINE clear_LinearSolverType_Multigrid

ENDMODULE LinearSolverTypes_Multigrid
