!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testSmoother
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE ParameterLists
  USE ParallelEnv
  USE SmootherTypes
  USE ISO_C_BINDING

  IMPLICIT NONE

  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(MPI_EnvType) :: mpiTestEnv
  TYPE(ParamType) :: params,params_2proc
  INTEGER(SIK) :: istt(1),istp(1),blk_size(1),num_colors(1)
  INTEGER(SIK) :: MPI_Comm_ID(1),solverMethod(1)

#ifdef FUTILITY_HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
  PetscErrorCode  :: ierr

  KSP :: ksp
  PetscErrorCode :: iperr

  INTERFACE
    SUBROUTINE PCShellGetContext(mypc,ctx_ptr,iperr)
      USE ISO_C_BINDING
      PC :: mypc
      TYPE(C_PTR) :: ctx_ptr
      PetscErrorCode :: iperr
    ENDSUBROUTINE PCShellGetContext
  ENDINTERFACE

  INTERFACE
    SUBROUTINE VecGetArrayReadF90(x,xx_v,iperr)
      Vec :: x
      PetscScalar, POINTER :: xx_v(:)
      PetscErrorCode :: iperr
    ENDSUBROUTINE VecGetArrayReadF90
  ENDINTERFACE

  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#else
  INTEGER(SIK) :: ksp=-1_SIK
#endif

#ifdef HAVE_MPI
  CALL mpiTestEnv%init(MPI_COMM_WORLD)
#endif

  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL eParams%addSurrogate(e)
  CALL eSmootherType%addSurrogate(e)

  istt=1
  istp=65
  blk_size=2_SIK
  num_colors=2_SIK
  num_smoothers=1_SIK
  solverMethod=CBJ
  MPI_Comm_ID=mpiTestEnv%comm
  CALL params%add('SmootherType->num_smoothers',1_SIK)
  CALL params%add('SmootherType->istt_list',istt)
  CALL params%add('SmootherType->istp_list',istp)
  CALL params%add('SmootherType->num_colors_list',num_colors)
  CALL params%add('SmootherType->blk_size_list',blk_size)
  CALL params%add('SmootherType->solverMethod_list',solverMethod)
  CALL params%add('SmootherType->MPI_Comm_ID_list',MPI_Comm_ID)
  CALL params_2proc%clear()
  params_2proc=params
  IF(.NOT. mpiTestEnv%master) THEN
    CALL params_2proc%set('SmootherType->istt',istt+istp)
    CALL params_2proc%set('SmootherType->istp',istp+istp)
  ENDIF
#ifdef FUTILITY_HAVE_PETSC
  CALL KSPCreate(mpiTestEnv%comm,ksp,iperr)
#endif

  CREATE_TEST('Test SmootherType_PETSc_CBJ')
  REGISTER_SUBTEST('testClear',testClear)
  REGISTER_SUBTEST('testSetKSP',testSetKSP)
  REGISTER_SUBTEST('testInit',testInit)
  REGISTER_SUBTEST('testDefineColor',testDefineColor)
  REGISTER_SUBTEST('testDefineAllColors',testDefineAllColors)
  REGISTER_SUBTEST('testSmooth',testSmooth_PETSc_CBJ)
  FINALIZE_TEST()

  CALL params%clear()
#ifdef FUTILITY_HAVE_PETSC
  CALL KSPDestroy(ksp,iperr)
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
      num_smoothers=1_SIK
      ALLOCATE(smootherList(num_smoothers))
      ALLOCATE(SmootherType_PETSc_CBJ ::smootherList(1)%smoother)

      isSmootherListInit=.TRUE.
      CALL smootherManager_clear()

      ASSERT(.NOT. ALLOCATED(smootherList),'smootherList deallocated')
      ASSERT(.NOT. isSmootherListInit, 'smootherList not initialized')
      ASSERT(num_smoothers == 0_SIK,'no smoothers exist')
    ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSetKSP()
#ifdef FUTILITY_HAVE_PETSC
      LOGICAL(SBK) :: tmpbool
      CHARACTER(LEN=12) :: pcname
      PC :: pc
      KSPType :: myksptype
      PCType :: mypctype
      TYPE(C_PTR) :: ctx_ptr
      PetscInt,POINTER :: ctx(:)

      CALL KSPSetType(ksp,KSPGMRES,iperr)
      CALL KSPGetPC(ksp,pc,iperr)
      CALL PCSetType(pc,PCJACOBI,iperr)

      num_smoothers=1_SIK
      ALLOCATE(smootherList(num_smoothers))
      ALLOCATE(ctxList(num_smoothers))
      ctxList(1)%ctx=1
      ALLOCATE(SmootherType_PETSc_CBJ ::smootherList(1)%smoother)
      isSmootherListInit=.TRUE.

      CALL SmootherManager_setKSP(1,ksp)

      SELECTTYPE(smoother => smootherList(1)%smoother)
        TYPE IS(SmootherType_PETSc_CBJ)
          CALL KSPGetType(smoother%ksp,myksptype,iperr)
          CALL PCGetType(smoother%pc,mypctype,iperr)
          tmpbool=iperr == 0_SIK .AND. &
                  myksptype == KSPRICHARDSON .AND. &
                  mypctype == PCSHELL
          ASSERT(tmpbool,"smoother's copy of ksp and pc set to the correct types in PETSc")
      ENDSELECT

      !Get context returns a pointer to a pointer for some maddening reason:
      CALL PCShellGetContext(pc,ctx_ptr,iperr)
      CALL C_F_POINTER(ctx_ptr,ctx,(/1/))
      ASSERT(ctx(1) == 1_SIK,'Correct context')

      CALL PCShellGetName(pc,pcname,iperr)
      ASSERT(pcname=="CBJ PC    1",'Correct PC name')

      CALL KSPGetType(ksp,myksptype,iperr)
      CALL PCGetType(pc,mypctype,iperr)
      tmpbool=iperr == 0_SIK .AND. &
              myksptype == KSPRICHARDSON .AND. &
              mypctype == PCSHELL
      ASSERT(tmpbool,'ksp and pc set to the correct types in PETSc')

      CALL smootherManager_clear()
#endif
    ENDSUBROUTINE testSetKSP
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInit()
      LOGICAL(SBK) :: tmpbool

      CALL smootherManager_Init(params)

      SELECTTYPE(smoother => smootherList(1)%smoother)
        TYPE IS(SmootherType_PETSc_CBJ)
          tmpbool=ALLOCATED(smoother%colorManager%color_ids) .AND. &
                    LBOUND(smoother%colorManager%color_ids,DIM=1) == istt(1) .AND. &
                    UBOUND(smoother%colorManager%color_ids,DIM=1) == istp(1)
          ASSERT(tmpbool,'color_ids allocated with correct bounds')

          tmpbool=smoother%colorManager%num_colors == num_colors(1) .AND. &
                    smoother%istt == istt(1) .AND. &
                    smoother%istp == istp(1) .AND. &
                    smoother%blk_size == blk_size(1) .AND. &
                    smoother%TPLType == PETSc .AND. &
                    smoother%smootherMethod == CBJ .AND. &
                    smoother%blockMethod == LU
          ASSERT(tmpbool,'smoother parameters are the correct value')

          tmpbool=smoother%MPIparallelEnv%isInit()
          ASSERT(tmpbool,'smoother ParEnv initialized.')

          ASSERT(smoother%isInit,'Smoother  initialized')
      ENDSELECT

      CALL smootherManager_clear()

    ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testDefineColor
      INTEGER(SIK),PARAMETER :: num_indices=25_SIK
      INTEGER(SIK) :: index_list(num_indices)
      INTEGER(SIK) :: icolor,i
      LOGICAL(SBK) :: tmpbool

      CALL smootherManager_Init(params)

      SELECTTYPE(smoother => smootherList(1)%smoother)
        TYPE IS(SmootherType_PETSc_CBJ)
          ASSOCIATE(manager => smoother%colorManager)
            DO i=1,num_indices
              index_list(i)=2*i-1
            ENDDO
            !Mess up the color ids intentionally:
            manager%color_ids=1_SIK

            icolor=2_SIK
            CALL smootherManager_defineColor(1_SIK,icolor,index_list)

            ASSERT(manager%colors(icolor)%num_indices == num_indices,'Correct # of indices')
            ASSERT(ALL(manager%colors(icolor)%index_list == index_list),'Correct index list')
            tmpbool=.TRUE.
            DO i=1,num_indices
              IF(manager%color_ids(index_list(i)) /= icolor) THEN
                tmpbool=.FALSE.
                EXIT
              ENDIF
            ENDDO
            ASSERT(tmpbool,'Correct color ids for color 2')
          ENDASSOCIATE
      ENDSELECT

      CALL smootherManager_Clear()

    ENDSUBROUTINE testDefineColor
!
!-------------------------------------------------------------------------------
    SUBROUTINE testDefineAllColors
      LOGICAL(SBK) :: tmpbool
      INTEGER(SIK) :: i
      INTEGER(SBK),ALLOCATABLE :: color_ids(:)

      CALL smootherManager_Init(params)

      ALLOCATE(color_ids(istt(1):istp(1)))
      color_ids=2
      DO i=istt(1),istp(1),2
        color_ids(i)=1
      ENDDO
      CALL smootherManager_defineAllColors(1_SIK,color_ids)

      SELECTTYPE(smoother => smootherList(1)%smoother)
        TYPE IS(SmootherType_PETSc_CBJ)
          ASSOCIATE(manager => smoother%colorManager)

            tmpbool=ALL(manager%color_ids == color_ids)
            ASSERT(tmpbool,'Smoother%color_ids correctly set')
            tmpbool=manager%colors(1)%num_indices == 33_SIK .AND. &
                      manager%colors(2)%num_indices == 32_SIK
            ASSERT(tmpbool,'Correct number of red and black indices')
          ENDASSOCIATE
      ENDSELECT

      CALL smootherManager_clear()

    ENDSUBROUTINE testDefineAllColors
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSmooth_PETSc_CBJ
#ifdef FUTILITY_HAVE_PETSC
#ifdef HAVE_MPI
      !Test smoother on 2-proc,2-group problem
      INTEGER(SIK),PARAMETER :: nlocal=65_SIK
      INTEGER(SIK),PARAMETER :: num_eqns=2_SIK
      INTEGER(SIK) :: n
      INTEGER(SIK) :: nstart,nend
      INTEGER(SIK) :: i,ieqn
      INTEGER(SIK) :: row,col
      INTEGER(SIK) :: dnnz(nlocal*num_eqns),onnz(nlocal*num_eqns)

      Mat :: A_petsc
      Vec :: b_petsc,x_petsc
      PetscErrorCode :: iperr
      PetscScalar,POINTER :: xx_v(:)

      n=nlocal*mpiTestEnv%nproc

      dnnz=num_eqns+2
      dnnz(1:num_eqns)=num_eqns+1
      dnnz((nlocal-1)*num_eqns+1:nlocal*num_eqns)=num_eqns+1
      onnz=0_SIK
      nstart=mpiTestEnv%rank*nlocal+1
      nend=(mpiTestEnv%rank+1)*nlocal
      IF(mpiTestEnv%rank < mpiTestEnv%nproc-1) THEN
        onnz((nend-1)*num_eqns+1:nend*num_eqns)=1_SIK
      ENDIF
      IF(mpiTestEnv%rank > 0) THEN
        onnz(1:num_eqns)=1_SIK
      ENDIF

      CALL MatCreate(MPI_COMM_WORLD,A_petsc,iperr)
      CALL VecCreate(MPI_COMM_WORLD,b_petsc,iperr)
      CALL VecCreate(MPI_COMM_WORLD,x_petsc,iperr)
      CALL MatSetSizes(A_petsc,nlocal*num_eqns,nlocal*num_eqns, &
                        n*num_eqns,n*num_eqns,iperr)
      CALL VecSetSizes(b_petsc,nlocal*num_eqns,n*num_eqns,iperr)
      CALL VecSetSizes(x_petsc,nlocal*num_eqns,n*num_eqns,iperr)
      CALL VecSetType(b_petsc,VECMPI,iperr)
      CALL MatSetType(A_petsc,MATMPIAIJ,ierr)
      CALL VecSetType(x_petsc,VECMPI,iperr)
      CALL MatMPIAIJSetPreallocation(A_petsc,0,dnnz,0,onnz,iperr)
      CALL VecSet(b_petsc,0.0_SRK,iperr)
      CALL VecAssemblyBegin(b_petsc,iperr)


      !2G homogeneous diffusion problem:
      DO i=nstart,nend
        DO ieqn=1,num_eqns
          row=(i-1)*num_eqns+ieqn
          IF(i > nstart) THEN
            col=(i-2)*num_eqns+ieqn
            CALL MatSetValue(A_petsc,row-1,col-1,-1.0_SRK,INSERT_VALUES,iperr)
          ENDIF
          IF(i < nend) THEN
            col=i*num_eqns+ieqn
            CALL MatSetValue(A_petsc,row-1,col-1,-1.0_SRK,INSERT_VALUES,iperr)
          ENDIF
        ENDDO
        row=(i-1)*num_eqns+1
        CALL MatSetValue(A_petsc,row-1,row-1,2.1_SRK,INSERT_VALUES,iperr)
        CALL MatSetValue(A_petsc,row,row,2.2_SRK,INSERT_VALUES,iperr)
        !Upscatter:
        CALL MatSetValue(A_petsc,row-1,row,-0.01_SRK,INSERT_VALUES,iperr)
        !Downscatter:
        CALL MatSetValue(A_petsc,row,row-1,-0.5_SRK,INSERT_VALUES,iperr)
        IF(MOD(i,2) == 0_SIK) THEN
          CALL VecSetValue(x_petsc,row,1.0_SRK,INSERT_VALUES,iperr)
        ELSE
          CALL VecSetValue(x_petsc,row,0.0_SRK,INSERT_VALUES,iperr)
        ENDIF
      ENDDO

      CALL VecAssemblyBegin(x_petsc,iperr)
      CALL MatAssemblyBegin(A_petsc,MAT_FINAL_ASSEMBLY,iperr)

      CALL VecAssemblyEnd(b_petsc,iperr)
      CALL VecAssemblyEnd(x_petsc,iperr)
      CALL MatAssemblyEnd(A_petsc,MAT_FINAL_ASSEMBLY,iperr)

      CALL smootherManager_Init(params_2proc)

      CALL smootherManager_setKSP(1,ksp)

      CALL KSPSetOperators(ksp,A_petsc,A_petsc,iperr)

      CALL KSPSetInitialGuessNonzero(ksp,PETSC_TRUE,iperr)
      CALL KSPSetTolerances(ksp,1E-10_SRK,1E-10_SRK,1E2_SRK,1_SIK,iperr)
      CALL KSPSolve(ksp,b_petsc,x_petsc,iperr)
      ASSERT(iperr == 0_SIK,'No KSPSolve Errors')

      CALL VecGetArrayReadF90(x_petsc,xx_v,iperr)
      ASSERT(ALL(xx_v .APPROXEQ. 0.0_SRK),'CBJ converged oscillating error in 1 it.')
      CALL VecRestoreArrayReadF90(x_petsc,xx_v,iperr)

      CALL smootherManager_clear()

      CALL MatDestroy(A_petsc,iperr)
      CALL VecDestroy(b_petsc,iperr)
      CALL VecDestroy(x_petsc,iperr)
#endif
#endif

    ENDSUBROUTINE testSmooth_PETSc_CBJ

ENDPROGRAM testSmoother
