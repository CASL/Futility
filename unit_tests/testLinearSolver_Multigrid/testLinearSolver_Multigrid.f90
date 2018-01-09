!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testLinearSolver_Multigrid
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes
  USE LinearSolverTypes
  USE LinearSolverTypes_Multigrid
  USE MultigridMesh
  USE SmootherTypes

  IMPLICIT NONE

  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(MPI_EnvType) :: mpiTestEnv
  TYPE(ParamType) :: pList, optListLS, optListMat, vecPList

#ifdef FUTILITY_HAVE_PETSC
#include <finclude/petsc.h>
#include <petscversion.h>
#undef IS
  PetscErrorCode  :: ierr

  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

#ifdef HAVE_MPI
  CALL mpiTestEnv%init(MPI_COMM_WORLD)
#endif

  !> set up default parameter list
  CALL optListLS%clear()
  CALL optListLS%add('LinearSolverType->TPLType',NATIVE)
  CALL optListLS%add('LinearSolverType->solverMethod',1_SNK) ! GE or BICGSTAB
  CALL optListLS%add('LinearSolverType->MPI_Comm_ID',mpiTestEnv%comm)
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
  CALL vecPList%add('VectorType -> MPI_Comm_ID',mpiTestEnv%comm)

  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL eParams%addSurrogate(e)
  CALL eLinearSolverType%addSurrogate(e)

  CREATE_TEST('Test Linear Solvers')

  REGISTER_SUBTEST('testClear',testClear)
  REGISTER_SUBTEST('testInit',testInit)
#ifdef FUTILITY_HAVE_PETSC
  REGISTER_SUBTEST('testPreAllocPETScInterpMat',testPreAllocPETScInterpMat)
  REGISTER_SUBTEST('testFillInterpMats',testFillInterpMats)
  REGISTER_SUBTEST('testSetupPETScMG',testSetupPETScMG)
#endif
  REGISTER_SUBTEST('testIterativeSolve_Multigrid',testIterativeSolve_Multigrid)
  REGISTER_SUBTEST('testSetSmoother',testSetSmoother)

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
      TYPE(LinearSolverType_Multigrid) :: thisLS
      LOGICAL(SBK) :: bool

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

      CALL thisLS%clear()

      !check results
      bool=thisLS%isInit .OR.thisLS%solverMethod == 1                     &
          .OR. ALLOCATED(thisLS%M) .OR. ASSOCIATED(thisLS%A)              &
          .OR. ASSOCIATED(thisLS%X) .OR. thisLS%info /= 0                 &
          .OR. thisLS%normType == 2 .OR. thisLS%maxIters == 2             &
          .OR. thisLS%iters == 2 .OR. thisLS%isDecomposed                 &
          .OR. thisLS%residual == 2._SRK .OR. thisLS%convTol == 2._SRK    &
          .OR. ALLOCATED(thisLS%PreCondType) .OR. thisLS%nLevels /= 1 &
          .OR. ALLOCATED(thisLS%level_info)                               &
          .OR. ALLOCATED(thisLS%level_info_local)                         &
#ifdef FUTILITY_HAVE_PETSC
          .OR. ALLOCATED(thisLS%interpMats_PETSc)                         &
#endif
          .OR. thisLS%isMultigridSetup
      ASSERT(.NOT.(bool),'CALL Multigrid%clear() FAILED!')
      CALL thisLS%clear()

    ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInit()
      TYPE(LinearSolverType_Multigrid) :: thisLS
      INTEGER(SIK) :: ref_level_info(2,4)
      LOGICAL(SBK) :: bool

      CALL pList%clear()
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->TPLType',PETSC)
      CALL pList%add('LinearSolverType->solverMethod',MULTIGRID)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->n',65_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nlocal',65_SNK)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',193_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->n',65_SNK)
      CALL pList%add('LinearSolverType->x->VectorType->nlocal',65_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->n',65_SNK)
      CALL pList%add('LinearSolverType->b->VectorType->nlocal',65_SNK)
      CALL pList%add('LinearSolverType->PCType','PCMG')
      CALL pList%add('LinearSolverType->PCIters',-1)
      CALL pList%add('LinearSolverType->PCSetup',0)
      CALL pList%add('LinearSolverType->Multigrid->nx_local',65)
      CALL pList%add('LinearSolverType->Multigrid->ny_local',1)
      CALL pList%add('LinearSolverType->Multigrid->nz_local',1)
      CALL pList%add('LinearSolverType->Multigrid->num_eqns',1)
      CALL pList%add('LinearSolverType->Multigrid->manuallySetLevelInfo',.FALSE.)

      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      bool = (thisLS%isInit .AND. thisLS%solverMethod == MULTIGRID &
         .AND. thisLS%MPIparallelEnv%isInit() &
         .AND. thisLS%OMPparallelEnv%isInit() )
      ASSERT(bool, 'Iterative%init(...)')

      ref_level_info = RESHAPE((/1,9, &
                                 1,17, &
                                 1,33, &
                                 1,65/),SHAPE(ref_level_info))
      ASSERT(thisLS%nLevels == 4,'Check number of multigrid levels')
      ASSERT(ALL(ref_level_info == thisLS%level_info),'Check grid sizes on each level.')

      CALL thisLS%clear()

    ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
#ifdef FUTILITY_HAVE_PETSC
    SUBROUTINE testPreAllocPETScInterpMat()
      TYPE(LinearSolverType_Multigrid) :: thisLS
      INTEGER(SIK),PARAMETER :: ref_sizes(4)=(/9,17,33,65/)
      INTEGER(SIK) :: iLevel,m,n
      PetscErrorCode :: iperr

      IF(mpiTestEnv%master) THEN
        CALL init_MultigridLS(thisLS)
        CALL preAllocInterpMatrices_1D1G(thisLS)
        DO iLevel=1,thisLS%nLevels-1
          CALL MatGetSize(thisLS%interpMats_PETSc(iLevel)%a,m,n,iperr)
          ASSERT(m == ref_sizes(iLevel+1),'Incorrect # of rows for interp. mat.')
          FINFO() 'Interpolation from',iLevel,'to',iLevel+1
          ASSERT(n == ref_sizes(iLevel),'Incorrect # of cols for interp. mat.')
          FINFO() 'Interpolation from',iLevel,'to',iLevel+1
        ENDDO
        CALL thisLS%clear()
      ELSE
        !This is needed because the wrapper assumes that if one processor has
        ! >0 asserts, then all procs have >0 asserts.  Without this, it hangs
        ASSERT(.TRUE.,'blank assert')
      ENDIF
      CALL mpiTestEnv%barrier()

    ENDSUBROUTINE testPreAllocPETScInterpMat
#endif
!
!-------------------------------------------------------------------------------
#ifdef FUTILITY_HAVE_PETSC
    SUBROUTINE testFillInterpMats()
      TYPE(LinearSolverType_Multigrid) :: thisLS
      TYPE(MultigridMeshStructureType),POINTER :: myMMeshes => NULL()
      TYPE(InterpWeightsStructureType),POINTER :: myWtStructure => NULL()

      INTEGER(SIK) :: iLevel,ix,ncol,nx
      INTEGER(SIK),ALLOCATABLE :: cols(:),tmpint(:)
      REAL(SRK),ALLOCATABLE :: vals(:)
      CHARACTER(LEN=2) :: tmpchar
      TYPE(ParamType) :: smootherParams

      LOGICAL(SBK) :: boolcols,boolvals,tmpbool

      PetscErrorCode :: iperr

      CALL init_MultigridLS(thisLS)

      ALLOCATE(myMMeshes)
      CALL myMMeshes%init(thisLS%nLevels)
      DO iLevel=thisLS%nLevels,1,-1
        nx=thisLS%level_info(2,iLevel)
        myMMeshes%meshes(iLevel)%istt=1
        myMMeshes%meshes(iLevel)%istp=nx
        myMMeshes%meshes(iLevel)%nPointsLocal=nx
        ALLOCATE(myMMeshes%meshes(iLevel)%mmData(1:nx))
        ALLOCATE(myMMeshes%meshes(iLevel)%interpDegrees(1:nx))
        DO ix=1,nx
          IF(MOD(ix,2) == 1) THEN
            myMMeshes%meshes(iLevel)%interpDegrees(ix)=0
            ALLOCATE(myMMeshes%meshes(iLevel)%mmData(ix)%childIndices(1))
            myMMeshes%meshes(iLevel)%mmData(ix)%childIndices(1)=(ix+1)/2
          ELSE
            myMMeshes%meshes(iLevel)%interpDegrees(ix)=1
            ALLOCATE(myMMeshes%meshes(iLevel)%mmData(ix)%childIndices(2))
            myMMeshes%meshes(iLevel)%mmData(ix)%childIndices(1)=ix+1
            myMMeshes%meshes(iLevel)%mmData(ix)%childIndices(2)=ix-1
          ENDIF
        ENDDO
      ENDDO
      ALLOCATE(myWtStructure)
      CALL myWtStructure%init(myMMeshes,1)
      CALL thisLS%fillInterpMats(myMMeshes,myWtStructure)
      ALLOCATE(tmpint(thisLS%nLevels))
      tmpint=mpiTestEnv%comm
      CALL smootherParams%add('SmootherType->MPI_Comm_ID_list',tmpint)
      tmpint=2
      CALL smootherParams%add('SmootherType->num_colors_list',tmpint)
      CALL smootherManager_initFromMMeshes(smootherParams,myMMeshes)
      CALL thisLS%fillInterpMats(myMMeshes)
      !Also test out the CBJ smoothers:
      tmpint(1)=GMRES
      tmpint(2:thisLS%nLevels)=CBJ
      CALL pList%add('LinearSolverType->smootherMethod_list',tmpint)
      DEALLOCATE(tmpint)
      CALL thisLS%setupPETScMG(pList)

      ASSERT(isSmootherListInit,'smoother list initialized')

      ALLOCATE(vals(2),cols(2))
      cols=0
      DO iLevel=1,thisLS%nLevels-1
        boolcols=.TRUE.
        boolvals=.TRUE.
        nx=thisLS%level_info(2,iLevel)
        DO ix=1,nx
          vals=0.0_SRK
          CALL MatGetRow(thisLS%interpMats_PETSc(iLevel)%a,ix-1,ncol,cols,vals,iperr)
          IF(MOD(ix,2) == 1) THEN
            boolcols=(cols(1)==(ix-1)/2)
            boolvals=vals(1)==1.0_SRK
          ELSE
            boolcols=(cols(1)==ix/2-1) .AND. (cols(2)==ix/2)
            boolvals=ALL(vals(1:2)==0.5_SRK)
          ENDIF
          CALL MatRestoreRow(thisLS%interpMats_PETSc(iLevel)%a,1_SIK,ncol,cols,vals,iperr)
          IF(.NOT. boolcols .OR. .NOT. boolvals .OR. iperr /= 0 ) EXIT
        ENDDO
        WRITE(tmpchar,'(I2)') iLevel
        ASSERT(boolcols,'Check interpolation matrix col indices for grid '//tmpchar)
        ASSERT(boolvals,'Check interpolation matrix entries for grid '//tmpchar)
        ASSERT(iperr == 0,'Error obtaining matrix entries for grid '//tmpchar)

        IF(iLevel > 1) THEN
          WRITE(tmpchar,'(I2)') iLevel+1
          tmpbool= smootherList(iLevel+1)%smoother%smootherMethod == CBJ .AND. &
                    smootherList(iLevel+1)%smoother%isInit
          ASSERT(tmpbool,'smoother initialized to correct smoother method for grid '//tmpchar)
          SELECTTYPE(smoother=>smootherList(iLevel+1)%smoother)
            TYPE IS(SmootherType_PETSc_CBJ)
              tmpbool=smoother%colorManager%hasAllColorsDefined .AND. &
                        smoother%isKSPSetup
              ASSERT(tmpbool,'smoother has all colors defined and ksp set up for grid '//tmpchar)
          ENDSELECT
        ENDIF

      ENDDO
      CALL myWtStructure%clear()
      CALL myMMeshes%clear()
      DEALLOCATE(vals,cols,myWtStructure,myMMeshes)

    ENDSUBROUTINE testFillInterpMats
#endif
!
!-------------------------------------------------------------------------------
#ifdef FUTILITY_HAVE_PETSC
    SUBROUTINE testSetupPETScMG()
      TYPE(LinearSolverType_Multigrid) :: thisLS
      PC :: pc_temp
      KSPType :: myksptype
      PCType :: mypctype
      PetscErrorCode :: iperr

      CALL init_MultigridLS(thisLS)
      CALL preAllocInterpMatrices_1D1G(thisLS)
      CALL setupInterpMatrices_1D1G(thisLS)
      CALL thisLS%setupPETScMG(pList)
      ASSERT(thisLS%isMultigridSetup,'LS%isMultigridSetup')

      CALL KSPGetType(thisLS%ksp,myksptype,iperr)
      ASSERT(myksptype == KSPRICHARDSON,'KSP type must be richardson for MG solver in PETSc')
      CALL KSPGetPC(thisLS%ksp,pc_temp,iperr)
      CALL PCGetType(pc_temp,mypctype,iperr)
      ASSERT(mypctype == PCMG,'PC type should be Multigrid!')

      CALL thisLS%clear()
      CALL mpiTestEnv%barrier()

    ENDSUBROUTINE testSetupPETScMG
#endif
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIterativeSolve_Multigrid()
      TYPE(LinearSolverType_Multigrid) :: thisLS
      REAL(SRK),ALLOCATABLE :: soln(:)
      REAL(SRK),POINTER :: x(:)
      LOGICAL(SBK) :: match
      INTEGER(SIK) :: istt,istp

      INTEGER(SIK),PARAMETER :: n=65_SNK
      INTEGER(SIK),PARAMETER :: n_2G=260_SNK

      INTEGER(SIK) :: level_info(2,4),level_info_local(2,4)

#ifdef FUTILITY_HAVE_PETSC
      !================ 1G Problem ============================
#ifdef HAVE_MPI
      IF(mpiTestEnv%master) THEN
#endif
        CALL init_MultigridLS(thisLS)

        ! Create solution:
        ALLOCATE(soln(n))
        soln=1.0_SRK
        soln(n/3)=2.0_SRK
        CALL setupLinearProblem_1D1G(thisLS,soln)

        CALL preAllocInterpMatrices_1D1G(thisLS)
        CALL setupInterpMatrices_1D1G(thisLS)
        CALL thisLS%setupPETScMG(pList)

        ! build x0
        ALLOCATE(x(n))
        x(1:(n-1)/2)=0.5_SRK
        x((n+1)/2:n)=1.1_SRK
        CALL thisLS%setX0(x)

        !set iterations and convergence information and build/set M
        CALL thisLS%setConv(2,1.0E-9_SRK,1000,30)

        !solve it
        CALL thisLS%solve()

        SELECTTYPE(LS_x => thisLS%X); TYPE IS(PETScVectorType)
          CALL LS_x%getAll(x)
        ENDSELECT
        match=ALL(ABS(x-soln) < 1.0E-6_SRK)
        ASSERT(match, 'PETScIterative%solve() - 1G,1D Multigrid')

        DEALLOCATE(soln)
        DEALLOCATE(x)
        CALL thisLS%A%clear()
        CALL thisLS%clear()
#ifdef HAVE_MPI
      ENDIF
      CALL mpiTestEnv%barrier()
#endif
      !================ 1G Problem ============================
      !
      !
      !================ 2G,2-proc Problem ============================
#ifdef HAVE_MPI
      IF(mpiTestEnv%nproc == 2) THEN
        level_info=RESHAPE((/2,18,2,34,2,66,2,130/),(/2,4/))
        level_info_local=RESHAPE((/2,9,2,17,2,33,2,65/),(/2,4/))
        CALL init_MultigridLS(thisLS,num_eqns_in=2,nx_in=130, &
                                nprocs_in=2,level_info=level_info, &
                                level_info_local=level_info_local,nlevels=4)

        ! Create solution:
        ALLOCATE(soln(n_2G))
        soln=1.0_SRK
        soln(n_2G/3)=2.0_SRK
        CALL setupLinearProblem_1D2G_2proc(thisLS,soln)

        CALL preAllocInterpMatrices_1D2G_2proc(thisLS)
        CALL setupInterpMatrices_1D2G_2proc(thisLS)
        CALL thisLS%setupPETScMG(pList)

        ! build x0
        ALLOCATE(x(n_2G))
        x(1:n_2G/2)=1.1_SRK
        x(n_2G/2+1:n_2G)=0.5_SRK
        IF(mpiTestEnv%master) THEN
          istt=1
          istp=n_2G/2
        ELSE
          istt=n_2G/2+1
          istp=n_2G
        ENDIF
        SELECTTYPE(LS_x => thisLS%X); TYPE IS(PETScVectorType)
          CALL LS_x%setRange_array(istt,istp,x(istt:istp))
        ENDSELECT

        !set iterations and convergence information and build/set M
        CALL thisLS%setConv(2,1.0E-9_SRK,1000,30)

        !solve it
        CALL mpiTestEnv%barrier()
        CALL thisLS%solve()

        x=0.0_SRK
        SELECTTYPE(LS_x => thisLS%X); TYPE IS(PETScVectorType)
          CALL LS_x%getRange(istt,istp,x(istt:istp))
        ENDSELECT
        match=ALL(ABS(x(istt:istp)-soln(istt:istp)) < 1.0E-6_SRK)
        ASSERT(match, 'PETScIterative%solve() - 2G,1D Multigrid')

        DEALLOCATE(soln)
        DEALLOCATE(x)
        CALL thisLS%A%clear()
        CALL thisLS%clear()
      ENDIF
#endif
      !================ 2G,2-proc Problem ============================
#endif
      !This is needed because if one or more procs have at least one assert
      ! statement, the unit test code assumes all procs have at least one
      ! assert statement.
      ASSERT(.TRUE.,'Dummy assert to make sure no procs are hanging.')

    ENDSUBROUTINE testIterativeSolve_Multigrid
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSetSmoother
      TYPE(LinearSolverType_Multigrid) :: thisLS
      LOGICAL(SBK) :: tmpbool
      INTEGER(SIK) :: iLevel
#ifdef FUTILITY_HAVE_PETSC
      KSP :: ksp_temp
      PC :: pc_temp
      KSPType :: myksptype
      PCType :: mypctype
      PetscErrorCode :: iperr

      CALL init_MultigridLS(thisLS)
      CALL preAllocInterpMatrices_1D1G(thisLS)
      CALL setupInterpMatrices_1D1G(thisLS)
      CALL thisLS%setupPETScMG(pList)

      CALL thisLS%setSmoother(SOR,0)
      CALL PCMGGetSmoother(thisLS%pc,0,ksp_temp,iperr)
      CALL KSPGetPC(ksp_temp,pc_temp,iperr)
      CALL KSPGetType(ksp_temp,myksptype,iperr)
      CALL PCGetType(pc_temp,mypctype,iperr)
      tmpbool=(myksptype == KSPRICHARDSON) .AND. (mypctype == PCSOR)
      ASSERT(tmpbool,'Set smoother for coarsest level.')

      CALL thisLS%setSmoother(GMRES)
      tmpbool=.TRUE.
      DO iLevel=1,thisLS%nLevels-1
        CALL PCMGGetSmoother(thisLS%pc,iLevel,ksp_temp,iperr)
        CALL KSPGetPC(ksp_temp,pc_temp,iperr)
        CALL KSPGetType(ksp_temp,myksptype,iperr)
        CALL PCGetType(pc_temp,mypctype,iperr)
        tmpbool=tmpbool .AND. (myksptype == KSPGMRES)
      ENDDO
      ASSERT(tmpbool,'Set smoother for all but the coarsest level.')

      CALL thisLS%setSmoother(SOR,1)
      CALL PCMGGetSmoother(thisLS%pc,1,ksp_temp,iperr)
      CALL KSPGetPC(ksp_temp,pc_temp,iperr)
      CALL KSPGetType(ksp_temp,myksptype,iperr)
      CALL PCGetType(pc_temp,mypctype,iperr)
      tmpbool=(myksptype == KSPRICHARDSON) .AND. (mypctype == PCSOR)
      DO iLevel=2,thisLS%nLevels-1
        CALL PCMGGetSmoother(thisLS%pc,iLevel,ksp_temp,iperr)
        CALL KSPGetPC(ksp_temp,pc_temp,iperr)
        CALL KSPGetType(ksp_temp,myksptype,iperr)
        CALL PCGetType(pc_temp,mypctype,iperr)
        tmpbool=tmpbool .AND. (myksptype == KSPGMRES)
      ENDDO
      ASSERT(tmpbool,'Set smoother level 1 only.')

      CALL thisLS%clear()
#endif

    ENDSUBROUTINE testSetSmoother
!
!-------------------------------------------------------------------------------
    SUBROUTINE init_MultigridLS(thisLS,num_eqns_in,nx_in,ny_in,nz_in,nprocs_in, &
                                level_info,level_info_local,nlevels)
      TYPE(LinearSolverType_Multigrid),INTENT(INOUT) :: thisLS
      INTEGER(SIK),INTENT(IN),OPTIONAL :: nx_in,ny_in,nz_in,num_eqns_in,nprocs_in
      INTEGER(SIK),INTENT(IN),OPTIONAL :: level_info(:,:),level_info_local(:,:)
      INTEGER(SIK),INTENT(IN),OPTIONAL :: nlevels
      INTEGER(SIK) :: num_eqns,nx,ny,nz,nprocs,n,nlocal
      INTEGER(SIK),ALLOCATABLE :: dnnz(:),onnz(:)

      nprocs=1
      num_eqns=1
      nx=65
      ny=1
      nz=1
      IF(PRESENT(num_eqns_in)) num_eqns=num_eqns_in
      IF(PRESENT(nx_in)) nx=nx_in
      IF(PRESENT(ny_in)) ny=ny_in
      IF(PRESENT(nz_in)) nz=nz_in
      IF(PRESENT(nprocs_in)) nprocs=nprocs_in

      n=num_eqns*nx*ny*nz
      nlocal=n/nprocs

      !The PETSC sparse matrix type
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',PETSC)
      CALL pList%add('LinearSolverType->solverMethod',MULTIGRID)
      IF(nprocs > 1) THEN
        CALL pList%add('LinearSolverType->MPI_Comm_ID',mpiTestEnv%comm)
      ELSE
        CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      ENDIF
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->blockSize',num_eqns)
      CALL pList%add('LinearSolverType->A->MatrixType->n',n)
      CALL pList%add('LinearSolverType->A->MatrixType->nlocal',nlocal)
      ALLOCATE(dnnz(nlocal),onnz(nlocal))
      onnz=0
      dnnz(1:num_eqns)=1+num_eqns
      dnnz(num_eqns+1:nlocal-num_eqns)=2+num_eqns
      dnnz(nlocal-num_eqns+1:nlocal)=1+num_eqns
      IF(nprocs > 1) THEN
        IF(mpiTestEnv%master) THEN
          onnz(nlocal-num_eqns+1:nlocal)=1
        ELSE
          onnz(1:num_eqns)=1
        ENDIF
      ENDIF
      CALL pList%add('LinearSolverType->A->MatrixType->dnnz',dnnz)
      CALL pList%add('LinearSolverType->A->MatrixType->onnz',onnz)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',SUM(dnnz)+SUM(onnz))
      DEALLOCATE(dnnz,onnz)
      CALL pList%add('LinearSolverType->x->VectorType->n',n)
      CALL pList%add('LinearSolverType->x->VectorType->nlocal',nlocal)
      CALL pList%add('LinearSolverType->b->VectorType->n',n)
      CALL pList%add('LinearSolverType->b->VectorType->nlocal',nlocal)
      CALL pList%validate(pList,optListLS)

      !Geometry dimensions for multigrid:
      CALL pList%add('LinearSolverType->Multigrid->nx_local',nx/nprocs)
      CALL pList%add('LinearSolverType->Multigrid->ny_local',ny)
      CALL pList%add('LinearSolverType->Multigrid->nz_local',nz)
      CALL pList%add('LinearSolverType->Multigrid->num_eqns',num_eqns)
      IF(PRESENT(nlevels)) &
        CALL pList%add('LinearSolverType->Multigrid->nlevels',nlevels)
      IF(PRESENT(level_info)) &
        CALL pList%add('LinearSolverType->Multigrid->level_info',level_info)
      IF(PRESENT(level_info_local)) &
        CALL pList%add('LinearSolverType->Multigrid->level_info_local', &
                                                           level_info_local)
      IF(.NOT.(PRESENT(nlevels) .AND. PRESENT(level_info) .AND. &
          PRESENT(level_info_local))) &
        CALL pList%add('LinearSolverType->Multigrid->manuallySetLevelInfo',.FALSE.)

      CALL thisLS%init(pList)

    ENDSUBROUTINE init_MultigridLS
!
!-------------------------------------------------------------------------------
    SUBROUTINE preAllocInterpMatrices_1D1G(thisLS)
      TYPE(LinearSolverType_Multigrid),INTENT(INOUT) :: thisLS
      INTEGER(SIK) :: iLevel, inx, nx, nx_old
      INTEGER(SIK),ALLOCATABLE :: dnnz(:),onnz(:)

#ifdef FUTILITY_HAVE_PETSC
      nx=thisLS%level_info(2,thisLS%nLevels)
      DO iLevel=thisLS%nLevels-1,1,-1
        !Create the interpolation operator:
        nx_old=nx
        nx=thisLS%level_info(2,iLevel)

        ALLOCATE(dnnz(nx_old),onnz(nx_old))
        onnz=0
        DO inx=1,nx_old
          IF(XOR(MOD(inx,2)==1, (inx>nx/2 .AND. MOD(nx,2) == 0))) THEN
            dnnz(inx)=1
          ELSE
            dnnz(inx)=2
          ENDIF
        ENDDO

        CALL thisLS%preAllocPETScInterpMat(iLevel,dnnz,onnz)
        DEALLOCATE(dnnz,onnz)

      ENDDO
#endif
    ENDSUBROUTINE preAllocInterpMatrices_1D1G
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupInterpMatrices_1D1G(thisLS)
      TYPE(LinearSolverType_Multigrid),INTENT(INOUT) :: thisLS
      INTEGER(SIK) :: iLevel, inx, nx, nx_old

#ifdef FUTILITY_HAVE_PETSC
      nx=thisLS%level_info(2,thisLS%nLevels)
      DO iLevel=thisLS%nLevels-1,1,-1
        !Create the interpolation operator:
        nx_old=nx
        nx=thisLS%level_info(2,iLevel)

        DO inx=1,nx_old
          IF(XOR(MOD(inx,2)==1, (inx>nx/2 .AND. MOD(nx,2) == 0))) THEN
            CALL thisLS%interpMats_PETSc(iLevel)%set(inx,(inx+1)/2,1.0_SRK)
          ELSE
            CALL thisLS%interpMats_PETSc(iLevel)%set(inx,inx/2,0.5_SRK)
            CALL thisLS%interpMats_PETSc(iLevel)%set(inx,inx/2+1,0.5_SRK)
          ENDIF
        ENDDO

      ENDDO
#endif
    ENDSUBROUTINE setupInterpMatrices_1D1G
!
!-------------------------------------------------------------------------------
    SUBROUTINE preAllocInterpMatrices_1D2G_2proc(thisLS)
      TYPE(LinearSolverType_Multigrid),INTENT(INOUT) :: thisLS
      INTEGER(SIK) :: iLevel,inx,nx,nx_old,ieqn,row
      INTEGER(SIK),ALLOCATABLE :: dnnz(:),onnz(:)

#ifdef FUTILITY_HAVE_PETSC
      nx=thisLS%level_info_local(2,thisLS%nLevels)
      DO iLevel=thisLS%nLevels-1,1,-1
        !Create the interpolation operator:
        nx_old=nx
        nx=thisLS%level_info_local(2,iLevel)

        ALLOCATE(dnnz(nx_old*2),onnz(nx_old*2))
        onnz=0
        DO inx=1,nx_old
          DO ieqn=1,2
            row=(inx-1)*2+ieqn
            IF(XOR(MOD(inx,2)==1, (inx>nx/2 .AND. MOD(nx,2) == 0))) THEN
              dnnz(row)=1
            ELSE
              dnnz(row)=2
            ENDIF
          ENDDO
        ENDDO

        CALL thisLS%preAllocPETScInterpMat(iLevel,dnnz,onnz)
        DEALLOCATE(dnnz,onnz)

      ENDDO
#endif
    ENDSUBROUTINE preAllocInterpMatrices_1D2G_2proc
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupInterpMatrices_1D2G_2proc(thisLS)
      TYPE(LinearSolverType_Multigrid),INTENT(INOUT) :: thisLS
      INTEGER(SIK) :: iLevel,inx,nx,nx_old,ieqn,row
      INTEGER(SIK) :: inx_c,col
      INTEGER(SIK) :: offset(2)

#ifdef FUTILITY_HAVE_PETSC
      nx=thisLS%level_info_local(2,thisLS%nLevels)
      DO iLevel=thisLS%nLevels-1,1,-1
        !Create the interpolation operator:
        nx_old=nx
        nx=thisLS%level_info_local(2,iLevel)
        IF(mpiTestEnv%master) THEN
          offset=0
        ELSE
          offset(1)=nx_old*2
          offset(2)=nx*2
        ENDIF

        DO inx=1,nx_old
          DO ieqn=1,2
            row=(inx-1)*2+ieqn
            IF(XOR(MOD(inx,2)==1, (inx>nx/2 .AND. MOD(nx,2) == 0))) THEN
              col=(inx-1)+ieqn
              CALL thisLS%interpMats_PETSc(iLevel)%set(row+offset(1),col+offset(2),1.0_SRK)
            ELSE
              inx_c=inx/2
              col=(inx_c-1)*2+ieqn
              CALL thisLS%interpMats_PETSc(iLevel)% &
                            set(row+offset(1),col+offset(2),0.5_SRK)
              col=inx_c*2+ieqn
              CALL thisLS%interpMats_PETSc(iLevel)% &
                            set(row+offset(1),col+offset(2),0.5_SRK)
            ENDIF
          ENDDO
        ENDDO

      ENDDO
#endif
    ENDSUBROUTINE setupInterpMatrices_1D2G_2proc
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupLinearProblem_1D1G(thisLS,soln)
      TYPE(LinearSolverType_Multigrid),INTENT(INOUT) :: thisLS
      REAL(SRK),INTENT(IN) :: soln(:)
      REAL(SRK),ALLOCATABLE :: b(:)
      REAL(SRK),ALLOCATABLE :: A_temp(:,:)
      INTEGER(SIK) :: n
      INTEGER(SIK) :: i

#ifdef FUTILITY_HAVE_PETSC
      n=SIZE(soln)

      !A is a tridiagonal system with -1 on the offdiagonals, and
      !  2.5 on the diagonals.
      ALLOCATE(A_temp(n,n))
      A_temp=0.0_SRK
      SELECTTYPE(A => thisLS%A); TYPE IS(PETScMatrixType)
      DO i=1,n
        IF(i > 1) THEN
          CALL A%set(i,i-1,-1.0_SRK)
          A_temp(i,i-1)=-1.0_SRK
        ENDIF
        CALL A%set(i,i,2.5_SRK)
        A_temp(i,i)=2.5_SRK
        IF(i < n) THEN
          CALL A%set(i,i+1,-1.0_SRK)
          A_temp(i,i+1)=-1.0_SRK
        ENDIF
      ENDDO
      ENDSELECT

      ALLOCATE(b(n))
      b=MATMUL(A_temp,soln)
      DEALLOCATE(A_temp)

      SELECTTYPE(LS_b => thisLS%b); TYPE IS(PETScVectorType)
        CALL LS_b%setAll_array(b)
      ENDSELECT
      DEALLOCATE(b)
#endif

    ENDSUBROUTINE setupLinearProblem_1D1G
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupLinearProblem_1D2G_2proc(thisLS,soln)
      TYPE(LinearSolverType_Multigrid),INTENT(INOUT) :: thisLS
      REAL(SRK),INTENT(IN) :: soln(:)
      REAL(SRK),ALLOCATABLE :: b(:)
      REAL(SRK),ALLOCATABLE :: A_temp(:,:)
      INTEGER(SIK) :: n,nstart,nend
      INTEGER(SIK) :: i,grp
      INTEGER(SIK) :: row,col

#ifdef FUTILITY_HAVE_PETSC
#ifdef HAVE_MPI
      n=SIZE(soln)/2

      !2G homogeneous diffusion problem:
      ALLOCATE(A_temp(2*n,2*n))
      A_temp=0.0_SRK
      SELECTTYPE(A => thisLS%A); TYPE IS(PETScMatrixType)
        IF(mpiTestEnv%master) THEN
          nstart=1
          nend=n/2
        ELSE
          nstart=n/2+1
          nend=n
        ENDIF
        DO i=nstart,nend
          DO grp=1,2
            row=(i-1)*2+grp
            IF(i > nstart) THEN
              col=(i-2)*2+grp
              CALL A%set(row,col,-1.0_SRK)
              A_temp(row,col)=-1.0_SRK
            ENDIF
            IF(i < nend) THEN
              col=i*2+grp
              CALL A%set(row,col,-1.0_SRK)
              A_temp(row,col)=-1.0_SRK
            ENDIF
          ENDDO
          row=(i-1)*2+1
          CALL A%set(row,row,2.1_SRK)
          A_temp(row,row)=2.1_SRK
          CALL A%set(row+1,row+1,2.2_SRK)
          A_temp(row+1,row+1)=2.2_SRK
          !Upscatter:
          CALL A%set(row,row+1,-0.01_SRK)
          A_temp(row,row+1)=-0.01_SRK
          !Downscatter:
          CALL A%set(row+1,row,-0.5_SRK)
          A_temp(row+1,row)=-0.5_SRK
        ENDDO
      ENDSELECT

      CALL mpiTestEnv%allReduce(4*n*n,A_temp)

      ALLOCATE(b(2*n))
      b=MATMUL(A_temp,soln)
      DEALLOCATE(A_temp)

      SELECTTYPE(LS_b => thisLS%b); TYPE IS(PETScVectorType)
        CALL LS_b%setRange_array((nstart-1)*2+1,(nend-1)*2+2, &
                                  b((nstart-1)*2+1:(nend-1)*2+2))
      ENDSELECT
      DEALLOCATE(b)
#endif
#endif

    ENDSUBROUTINE setupLinearProblem_1D2G_2proc

ENDPROGRAM testLinearSolver_Multigrid
