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
  USE PreconditionerTypes
  USE LinearSolverTypes
  USE LinearSolverTypes_Multigrid

  IMPLICIT NONE

  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(MPI_EnvType) :: mpiTestEnv
  TYPE(ParamType) :: pList, optListLS, optListMat, vecPList

#ifdef FUTILITY_HAVE_PETSC
#include <finclude/petscdef.h>
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

  REGISTER_SUBTEST('testClear',testClear)
  REGISTER_SUBTEST('testInit',testInit)
  REGISTER_SUBTEST('testPreAllocPETScInterpMat',testPreAllocPETScInterpMat)
  REGISTER_SUBTEST('testSetupPETScMG',testSetupPETScMG)
  REGISTER_SUBTEST('testIterativeSolve_Multigrid',testIterativeSolve_Multigrid)

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

      ! initialize preconditioner
      ALLOCATE(ILU_PreCondType :: thisLS%PreCondType)
      CALL thisLS%setupPC() !ZZZZ

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
      bool=thisLS%isInit .OR.thisLS%solverMethod == 1                  &
          .OR. ALLOCATED(thisLS%M) .OR. ASSOCIATED(thisLS%A)           &
          .OR. ASSOCIATED(thisLS%X) .OR. thisLS%info /= 0               &
          .OR. thisLS%normType == 2 .OR. thisLS%maxIters == 2          &
          .OR. thisLS%iters == 2 .OR. thisLS%isDecomposed              &
          .OR. thisLS%residual == 2._SRK .OR. thisLS%convTol == 2._SRK &
          .OR. ALLOCATED(thisLS%PreCondType)
      ASSERT(.NOT.(bool),'CALL Multigrid%clear() FAILED!')
      CALL thisLS%clear()

    ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInit()
      TYPE(LinearSolverType_Multigrid) :: thisLS
      INTEGER(SIK) :: ref_level_info(4,4)
      INTEGER(SIK) :: nerrors1,nerrors2
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
      CALL pList%add('LinearSolverType->Multigrid->nx',65_SIK)
      CALL pList%add('LinearSolverType->Multigrid->nx',65_SIK)
      CALL pList%add('LinearSolverType->Multigrid->ny',1_SIK)
      CALL pList%add('LinearSolverType->Multigrid->nz',1_SIK)
      CALL pList%add('LinearSolverType->Multigrid->num_eqns',1_SIK)

      CALL pList%validate(pList,optListLS)
      CALL thisLS%init(pList)
      bool = (thisLS%isInit .AND. thisLS%solverMethod == MULTIGRID &
         .AND. thisLS%MPIparallelEnv%isInit() &
         .AND. thisLS%OMPparallelEnv%isInit() )
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

      CALL thisLS%init(pList)
      ref_level_info = RESHAPE((/1,9,1,1, &
                                 1,17,1,1, &
                                 1,33,1,1, &
                                 1,65,1,1/),SHAPE(ref_level_info))
      ASSERT(thisLS%nLevels == 4,'Check number of multigrid levels')
      ASSERT(ALL(ref_level_info == thisLS%level_info),'Check grid sizes on each level.')

      CALL thisLS%clear()
      ! Check uninitialized linear solver
      nerrors1=e%getCounter(EXCEPTION_ERROR)
      CALL thisLS%setupPC()
      nerrors2=e%getCounter(EXCEPTION_ERROR)
      ASSERT(nerrors2 == nerrors1+1,'LS%setupPC ALLOCATED(PC%A) check')
      FINFO() 'Result:',nerrors2,'Solution:',nerrors1+1
!      ENDIF
      CALL thisLS%clear()

    ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testPreAllocPETScInterpMat()
      TYPE(LinearSolverType_Multigrid) :: thisLS
#ifdef FUTILITY_HAVE_PETSC
      INTEGER(SIK),PARAMETER :: ref_sizes(4)=(/9,17,33,65/)
      INTEGER(SIK) :: iLevel,m,n
      PetscErrorCode :: iperr
#endif

      CALL init_MultigridLS(thisLS)
      CALL preAllocInterpMatrices_1D1G(thisLS)
#ifdef FUTILITY_HAVE_PETSC
      DO iLevel=1,thisLS%nLevels-1
        CALL MatGetSize(thisLS%interpMats(iLevel)%a,m,n,iperr)
        ASSERT(m == ref_sizes(iLevel+1),'Incorrect # of rows for interp. mat.')
        FINFO() 'Interpolation from',iLevel,'to',iLevel+1
        ASSERT(n == ref_sizes(iLevel),'Incorrect # of cols for interp. mat.')
        FINFO() 'Interpolation from',iLevel,'to',iLevel+1
      ENDDO
#endif
      CALL thisLS%clear()

    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSetupPETScMG()
      TYPE(LinearSolverType_Multigrid) :: thisLS
#ifdef FUTILITY_HAVE_PETSC
      KSP :: ksp_temp
      PC :: pc_temp
      KSPType :: ksptype
      PCType :: pctype
      PetscErrorCode :: iperr
#endif

      CALL init_MultigridLS(thisLS)
      CALL preAllocInterpMatrices_1D1G(thisLS)
      CALL setupInterpMatrices_1D1G(thisLS)
      CALL thisLS%setupPETScMG(pList)
      ASSERT(thisLS%isMultigridSetup,'LS%isMultigridSetup')

#ifdef FUTILITY_HAVE_PETSC
      CALL KSPGetType(thisLS%ksp,ksptype,iperr)
      ASSERT(ksptype == KSPRICHARDSON,'KSP type must be richardson for MG solver in PETSc')
      CALL KSPGetPC(thisLS%ksp,pc_temp,iperr)
      CALL PCGetType(pc_temp,pctype,iperr)
      ASSERT(pctype == PCMG,'PC type should be Multigrid!')
#endif

      CALL thisLS%clear()

    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIterativeSolve_Multigrid()
      TYPE(LinearSolverType_Multigrid) :: thisLS
      REAL(SRK),ALLOCATABLE :: soln(:),b(:)
      REAL(SRK),ALLOCATABLE :: A_temp(:,:)
      REAL(SRK),POINTER :: x(:)
      LOGICAL(SBK) :: match
      INTEGER(SIK) :: i

      INTEGER(SIK),PARAMETER :: n=65_SNK
      INTEGER(SIK),PARAMETER :: n_2G=130_SNK

#ifdef FUTILITY_HAVE_PETSC
      !================ 1G Problem ============================
      CALL init_MultigridLS(thisLS)
      CALL preAllocInterpMatrices_1D1G(thisLS)
      CALL setupInterpMatrices_1D1G(thisLS)
      CALL thisLS%setupPETScMG(pList)

      ! Create solution:
      ALLOCATE(soln(n))
      soln=1.0_SRK
      soln(n/3)=2.0_SRK
      CALL setupLinearProblem(thisLS,soln)

      CALL thisLS%setupPETScMG(pList)

      ! build x0
      ALLOCATE(x(n))
      x(1:(n-1)/2)=0.5_SRK
      x((n+1)/2:n)=1.1_SRK
      CALL thisLS%setX0(x)

      !set iterations and convergence information and build/set M
      CALL thisLS%setConv(2_SIK,1.0E-9_SRK,1000_SIK,30_SIK)

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
      !================ 1G Problem ============================
      !
      !
#endif

    ENDSUBROUTINE testIterativeSolve_Multigrid
!
!-------------------------------------------------------------------------------
    SUBROUTINE init_MultigridLS(thisLS,num_eqns_in,nx_in,ny_in,nz_in)
      TYPE(LinearSolverType_Multigrid),INTENT(INOUT) :: thisLS
      INTEGER(SIK),INTENT(IN),OPTIONAL :: nx_in,ny_in,nz_in,num_eqns_in
      INTEGER(SIK) :: num_eqns,nx,ny,nz,n
      INTEGER(SIK),ALLOCATABLE :: dnnz(:),onnz(:)

      num_eqns=1_SIK
      nx=65_SIK
      ny=1_SIK
      nz=1_SIK
      IF(PRESENT(num_eqns_in)) num_eqns=num_eqns_in
      IF(PRESENT(nx_in)) nx=nx_in
      IF(PRESENT(ny_in)) ny=ny_in
      IF(PRESENT(nz_in)) nz=nz_in

      n=num_eqns*nx*ny*nz

      !The PETSC sparse matrix type
      ! initialize linear system
      CALL pList%clear()
      CALL pList%add('LinearSolverType->TPLType',PETSC)
      CALL pList%add('LinearSolverType->solverMethod',MULTIGRID)
      CALL pList%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL pList%add('LinearSolverType->numberOMP',1_SNK)
      CALL pList%add('LinearSolverType->timerName','testTimer')
      CALL pList%add('LinearSolverType->matType',SPARSE)
      CALL pList%add('LinearSolverType->A->MatrixType->n',n)
      CALL pList%add('LinearSolverType->A->MatrixType->nlocal',n)
      ALLOCATE(dnnz(n),onnz(n))
      dnnz(1:num_eqns)=1_SIK+num_eqns
      dnnz(num_eqns+1:n-num_eqns)=2_SIK+num_eqns
      dnnz(n-num_eqns+1:n)=1_SIK+num_eqns
      onnz=0_SIK
      CALL pList%add('LinearSolverType->A->MatrixType->dnnz',dnnz)
      CALL pList%add('LinearSolverType->A->MatrixType->onnz',onnz)
      CALL pList%add('LinearSolverType->A->MatrixType->nnz',SUM(dnnz)+SUM(onnz))
      DEALLOCATE(dnnz,onnz)
      CALL pList%add('LinearSolverType->x->VectorType->n',n)
      CALL pList%add('LinearSolverType->b->VectorType->n',n)
      CALL pList%validate(pList,optListLS)

      !Geometry dimensions for multigrid:
      CALL pList%add('LinearSolverType->Multigrid->nx',nx)
      CALL pList%add('LinearSolverType->Multigrid->ny',ny)
      CALL pList%add('LinearSolverType->Multigrid->nz',nz)
      CALL pList%add('LinearSolverType->Multigrid->num_eqns',num_eqns)

      CALL thisLS%init(pList)

    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
    SUBROUTINE preAllocInterpMatrices_1D1G(thisLS)
      TYPE(LinearSolverType_Multigrid),INTENT(INOUT) :: thisLS
      INTEGER(SIK) :: iLevel, inx, nx, nx_old
      INTEGER(SIK),ALLOCATABLE :: dnnz(:),onnz(:)

      nx=thisLS%level_info(2,thisLS%nLevels)
      DO iLevel=thisLS%nLevels-1,1,-1
        !Create the interpolation operator:
        nx_old=nx
        nx=thisLS%level_info(2,iLevel)

        ALLOCATE(dnnz(nx_old),onnz(nx_old))
        onnz=0_SIK
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
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupInterpMatrices_1D1G(thisLS)
      TYPE(LinearSolverType_Multigrid),INTENT(INOUT) :: thisLS
      INTEGER(SIK) :: iLevel, inx, nx, nx_old

      DO iLevel=thisLS%nLevels-1,1,-1
        !Create the interpolation operator:
        nx_old=nx
        nx=thisLS%level_info(2,iLevel)

        DO inx=1,nx_old
          IF(XOR(MOD(inx,2)==1, (inx>nx/2 .AND. MOD(nx,2) == 0))) THEN
            CALL thisLS%interpMats(iLevel)%set(inx,(inx+1)/2,1.0_SRK)
          ELSE
            CALL thisLS%interpMats(iLevel)%set(inx,inx/2,0.5_SRK)
            CALL thisLS%interpMats(iLevel)%set(inx,inx/2+1,0.5_SRK)
          ENDIF
        ENDDO

      ENDDO
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
    SUBROUTINE setupLinearProblem(thisLS,soln)
      TYPE(LinearSolverType_Multigrid),INTENT(INOUT) :: thisLS
      REAL(SRK),INTENT(IN) :: soln(:)
      REAL(SRK),ALLOCATABLE :: b(:)
      REAL(SRK),ALLOCATABLE :: A_temp(:,:)
      INTEGER(SIK) :: n
      INTEGER(SIK) :: i

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

    ENDSUBROUTINE

ENDPROGRAM testLinearSolver_Multigrid
