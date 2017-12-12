!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module provides a smoother type for smoothing a system of equations.
!>        It is intended as for support multigrid methods, though the smoothers
!>        can technically be used as standalone solvers in principle.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref ParallelEnv "ParallelEnv": @copybrief ParallelEnv
!>
!> @par EXAMPLES
!> @code
!>
!> @endcode
!>
!> @author Ben C. Yee
!>   @date 09/21/2017
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE SmootherTypes
  USE IntrType !ZZZZ remove unnecessary uses
  USE BLAS
  USE trilinos_interfaces
  USE Times
  USE ExceptionHandler
  USE ParameterLists
  USE ParallelEnv
  USE LinearSolverTypes
  USE MatrixTypes
  USE VectorTypes
  USE MultigridMesh
  USE ISO_C_BINDING
  IMPLICIT NONE

#ifdef FUTILITY_HAVE_PETSC
#include <finclude/petsc.h>
!petscisdef.h defines the keyword IS, and it needs to be reset
#undef IS
#endif

  PRIVATE

  INTEGER(SIK),PARAMETER,PUBLIC :: MAX_SMOOTHERS_PER_LIST=8_SIK
!
! List of public members
  PUBLIC :: eSmootherType
  PUBLIC :: num_smoother_lists
  PUBLIC :: smootherListCollection
  !Public smoother list manager functions:
  PUBLIC :: smootherManager_clear
  PUBLIC :: smootherManager_setKSP
  PUBLIC :: smootherManager_init
  PUBLIC :: smootherManager_initFromMMeshes
  PUBLIC :: smootherManager_defineColor
  PUBLIC :: smootherManager_defineAllColors
  PUBLIC :: smootherManager_requestAllBlocksUpdate
#ifdef UNIT_TEST
  PUBLIC :: SmootherType_PETSc_CBJ
#ifdef FUTILITY_HAVE_PETSC
  PUBLIC :: PCSetUp_CBJ
  PUBLIC :: PCApply_CBJ
#endif
#endif

  !> TPL types from LinearSolverTypes:
  PUBLIC :: PETSC,NATIVE
  !> Smoother types, from LinearSolverTypes:
  PUBLIC :: CBJ
  !> Block solver types, from LinearSolverTypes:
  PUBLIC :: LU,SOR,ILU

  !> @brief the base linear smoother type
  TYPE,ABSTRACT :: SmootherType_Base
    !> Initialization status
    LOGICAL(SBK) :: isInit=.FALSE.
    !> Integer flag for the solution methodology desired
    INTEGER(SIK) :: smootherMethod=-1
    !> Integer flag for the solution methodology desired
    INTEGER(SIK) :: blockMethod=-1
    !> Integer flag for the solution methodology desired
    INTEGER(SIK) :: TPLType=-1
    !> Pointer to an MPI parallel environment
    TYPE(MPI_EnvType) :: MPIparallelEnv
    !> Has initial guess?
    LOGICAL(SBK) :: hasX0=.FALSE.
    !> Current local residual norm
    REAL(SRK) :: localResidual=0.0_SRK
    !> Current global residual norm
    REAL(SRK) :: globalResidual=0.0_SRK
    !> Starting local index:
    INTEGER(SIK) :: istt=-1_SIK
    !> End local index:
    INTEGER(SIK) :: istp=-1_SIK
    !> Block size (number of unknowns per point):
    INTEGER(SIK) :: blk_size=-1_SIK
    !> Solvers for all the blocks:
    CLASS(LinearSolverType_Base),ALLOCATABLE :: blockSolvers(:)

  !
  !List of Type Bound Procedures
    CONTAINS
      !> Deferred routine for clearing the smoother
      PROCEDURE(smootherClear_sub_absintfc),DEFERRED,PASS :: clear
      !> Deferred routine for initializing the smoother
      PROCEDURE(smootherInit_sub_absintfc),DEFERRED,PASS :: init
  ENDTYPE SmootherType_Base

  TYPE,ABSTRACT,EXTENDS(SmootherType_Base) :: SmootherType_PETSc
#ifdef FUTILITY_HAVE_PETSC
    !> Pointer to PETSc KSP object, should type KSPRICHARDSON
    KSP :: ksp
    !> Pointer to PETSc PC object corresponding to ksp, should type PCSHELL
    PC :: pc
#else
    !> Dummy attribute to make sure Futility compiles when PETSc is not present
    INTEGER(SIK) :: ksp=-1_SIK
    !> Dummy attribute to make sure Futility compiles when PETSc is not present
    INTEGER(SIK) :: pc=-1_SIK
#endif
    !> Whether or not the KSP has been initialized/set up:
    LOGICAL(SBK) :: isKSPSetup=.FALSE.
  ENDTYPE SmootherType_PETSc

  !Handy structure to store list of indices (which can vary in length from
  !  color to color)
  TYPE :: IndexList
    !Number of indices for this color:
    INTEGER(SIK) :: num_indices=-1_SIK
    !List of indices for this color:
    INTEGER(SIK),ALLOCATABLE :: index_list(:)
  ENDTYPE IndexList

  !Handy type to handle the coloring of indices
  TYPE :: ColorManagerType
    !> Number of colors:
    INTEGER(SIK) :: num_colors=-1_SIK
    !> List of indices for each color (1:num_colors)
    TYPE(IndexList),ALLOCATABLE :: colors(:)
    !> Whether or not each of the color index lists have been set (1:num_colors)
    LOGICAL(SBK),ALLOCATABLE :: hasColorDefined(:)
    !> Whether or not all the color index lists have been set
    LOGICAL(SBK) :: hasAllColorsDefined=.FALSE.
    !> Color ID of each point (istt:istp)
    INTEGER(SIK),ALLOCATABLE :: color_ids(:)
    !> Whether the arrays above have been allocated:
    LOGICAL(SBK) :: isInit=.FALSE.
    !> Starting local index:
    INTEGER(SIK) :: istt=-1_SIK
    !> End local index:
    INTEGER(SIK) :: istp=-1_SIK

    !
    !List of Type Bound Procedures
      CONTAINS
      !> @copybrief SmootherTypes::init_ColorManagerType
      !> @copydetails SmootherTypes::init_ColorManagerType
      PROCEDURE,PASS :: init => init_ColorManagerType
      !> @copybrief SmootherTypes::clear_ColorManagerType
      !> @copydetails SmootherTypes::clear_ColorManagerType
      PROCEDURE,PASS :: clear => clear_ColorManagerType
  ENDTYPE ColorManagerType

  !Colored block Jacobi smoother:
  TYPE,EXTENDS(SmootherType_PETSc) :: SmootherType_PETSc_CBJ
    !> A type for managing the coloring scheme:
    TYPE(ColorManagerType) :: colorManager
    !> Current color being solved:
    INTEGER(SIK) :: icolor=-1_SIK
    !> Whether or not block calculations need to be reperformed:
    LOGICAL(SBK) :: blocksNeedUpdate=.TRUE.

  !
  !List of Type Bound Procedures
    CONTAINS
      !> @copybrief SmootherTypes::init_SmootherType_PETSc_CBJ
      !> @copydetails SmootherTypes::init_SmootherType_PETSc_CBJ
      PROCEDURE,PASS :: init => init_SmootherType_PETSc_CBJ
      !> @copybrief SmootherTypes::clear_SmootherType_PETSc_CBJ
      !> @copydetails SmootherTypes::clear_SmootherType_PETSc_CBJ
      PROCEDURE,PASS :: clear => clear_SmootherType_PETSc_CBJ
  ENDTYPE SmootherType_PETSc_CBJ

  !> Exception Handler for use in SmootherTypes
  TYPE(ExceptionHandlerType),SAVE :: eSmootherType

  !PETSC INTERFACES
#ifdef FUTILITY_HAVE_PETSC
  INTERFACE
    SUBROUTINE PCShellSetContext(mypc,ctx,iperr)
      PC :: mypc
      PetscInt :: ctx(1)
      PetscErrorCode :: iperr
    ENDSUBROUTINE PCShellSetContext
  ENDINTERFACE

  INTERFACE
    SUBROUTINE PCShellGetContext(mypc,ctx_ptr,iperr)
      USE ISO_C_BINDING
      PC :: mypc
      TYPE(C_PTR) :: ctx_ptr
      PetscErrorCode :: iperr
    ENDSUBROUTINE PCShellGetContext
  ENDINTERFACE

  INTERFACE
    SUBROUTINE MatSeqAIJGetArrayF90(A,xx_v,iperr)
      Mat :: A
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6) && (PETSC_VERSION_SUBMINOR>=4))
      PetscReal, POINTER :: xx_v(:)
#else
      PetscReal, POINTER :: xx_v(:,:)
#endif
      PetscErrorCode :: iperr
    ENDSUBROUTINE MatSeqAIJGetArrayF90
  ENDINTERFACE

  INTERFACE
    SUBROUTINE MatSeqAIJRestoreArrayF90(A,xx_v,iperr)
      Mat :: A
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6) && (PETSC_VERSION_SUBMINOR>=4))
      PetscReal, POINTER :: xx_v(:)
#else
      PetscReal, POINTER :: xx_v(:,:)
#endif
      PetscErrorCode :: iperr
    ENDSUBROUTINE MatSeqAIJRestoreArrayF90
  ENDINTERFACE

  INTERFACE
    SUBROUTINE MatGetRowIJF90(A,shift,symmetric,inodecompressed,n,ia,ja,done,iperr)
      Mat :: A
      PetscInt :: shift,n
      PetscBool :: symmetric,inodecompressed,done
      PetscInt,POINTER :: ia(:),ja(:)
      PetscErrorCode :: iperr
    ENDSUBROUTINE MatGetRowIJF90
  ENDINTERFACE

  INTERFACE
    SUBROUTINE MatRestoreRowIJF90(A,shift,symmetric,inodecompressed,n,ia,ja,done,iperr)
      Mat :: A
      PetscInt :: shift,n
      PetscBool :: symmetric,inodecompressed,done
      PetscInt,POINTER :: ia(:),ja(:)
      PetscErrorCode :: iperr
    ENDSUBROUTINE MatRestoreRowIJF90
  ENDINTERFACE

  INTERFACE
    SUBROUTINE VecGetArrayReadF90(x,xx_v,iperr)
      Vec :: x
      PetscReal, POINTER :: xx_v(:)
      PetscErrorCode :: iperr
    ENDSUBROUTINE VecGetArrayReadF90
  ENDINTERFACE

  INTERFACE
    SUBROUTINE VecRestoreArrayReadF90(x,xx_v,iperr)
      Vec :: x
      PetscReal, POINTER :: xx_v(:)
      PetscErrorCode :: iperr
    ENDSUBROUTINE VecRestoreArrayReadF90
  ENDINTERFACE

  INTERFACE
    SUBROUTINE VecGetArrayF90(x,xx_v,iperr)
      Vec :: x
      PetscReal, POINTER :: xx_v(:)
      PetscErrorCode :: iperr
    ENDSUBROUTINE VecGetArrayF90
  ENDINTERFACE

  INTERFACE
    SUBROUTINE VecRestoreArrayF90(x,xx_v,iperr)
      Vec :: x
      PetscReal, POINTER :: xx_v(:)
      PetscErrorCode :: iperr
    ENDSUBROUTINE VecRestoreArrayF90
  ENDINTERFACE
#endif

  !> Explicitly defines the interface for the clear routines
  ABSTRACT INTERFACE
    SUBROUTINE smootherClear_sub_absintfc(smoother)
      IMPORT :: SmootherType_Base
      CLASS(SmootherType_Base),INTENT(INOUT) :: smoother
    ENDSUBROUTINE smootherClear_sub_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the init routines
  ABSTRACT INTERFACE
    SUBROUTINE smootherInit_sub_absintfc(smoother,params)
      IMPORT :: SmootherType_Base,ParamType
      CLASS(SmootherType_Base),INTENT(INOUT) :: smoother
      TYPE(ParamType),INTENT(IN) :: params
    ENDSUBROUTINE smootherInit_sub_absintfc
  ENDINTERFACE

  !> Abstract smoother instance:
  !>  This is needed so smootherList can have different smoother types
  TYPE :: SmootherInstanceType
    CLASS(SmootherType_Base),ALLOCATABLE :: smoother
  ENDTYPE SmootherInstanceType
  !> This is needed to have a ctxList of pointers to PetscInt arrays
  TYPE :: ctxInstanceType
#ifdef FUTILITY_HAVE_PETSC
    PetscInt :: ctx(1)
#else
    INTEGER(SIK) :: ctx(1)
#endif
  ENDTYPE ctxInstanceType

  !> List of smoothers.
  !>  This is needed so we can have multiple lists of smoothers.
  TYPE :: SmootherListType
    !> Whether or not the smoother list has been initialized:
    !>   There is only one copy of the smoother list.
    LOGICAL(SBK) :: isSmootherListInit=.FALSE.
    !> Number of smoothers in the smoother list
    INTEGER(SIK) :: num_smoothers=0_SIK
    !> List of abstract smoothers:
    TYPE(SmootherInstanceType),ALLOCATABLE :: smootherList(:)
    !> ctxList to keep track of which smoother is which
    TYPE(ctxInstanceType),ALLOCATABLE :: ctxList(:)
  ENDTYPE SmootherListType

  !Number of smoother lists:
  INTEGER(SIK),SAVE :: num_smoother_lists=0_SIK
  !Collection of smootherList objects
  TYPE(SmootherListType),ALLOCATABLE,SAVE :: smootherListCollection(:)

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='SMOOTHERTYPES'
!
!------------------------------------------------------------------------------
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the SmootherType for an CBJ PETSc smoother
!>
!> @param smoother The smoother object to act on
!> @param ksp The PETSc ksp object to act on
!> @param params Parameter list, which must contain num_colors, istt, istp,
!>        blk_size, and MPI_Comm_ID
!>
    SUBROUTINE init_SmootherType_PETSc_CBJ(smoother,params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_SmootherType_PETSc_CBJ'
      CLASS(SmootherType_PETSc_CBJ),INTENT(INOUT) :: smoother
      TYPE(ParamType),INTENT(IN) :: params
      LOGICAL(SBK) :: tmpbool
      INTEGER(SIK) :: MPI_Comm_ID
#ifndef FUTILITY_HAVE_PETSC
      CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
          "This type should only be used with PETSc enabled!")
#endif
      !Check parameter list:
      tmpbool=params%has('SmootherType->num_colors') .AND. &
              params%has('SmootherType->istt') .AND. &
              params%has('SmootherType->istp') .AND. &
              params%has('SmootherType->blk_size') .AND. &
              params%has('SmootherType->blockMethod') .AND. &
              params%has('SmootherType->MPI_Comm_ID')
      IF(.NOT. tmpbool) &
        CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
            "Missing a parameter from the parameter list!")

      !Extract param list info:
      CALL smoother%colorManager%init(params)
      smoother%icolor=1_SIK

      CALL params%get('SmootherType->istt',smoother%istt)
      CALL params%get('SmootherType->istp',smoother%istp)
      CALL params%get('SmootherType->blk_size',smoother%blk_size)
      CALL params%get('SmootherType->MPI_Comm_ID',MPI_Comm_ID)

      !MPI_Comm_ID=-1
      IF(MPI_Comm_ID /= -1) THEN
        CALL smoother%MPIparallelEnv%init(MPI_Comm_ID)
      ELSE
#ifdef HAVE_MPI
        CALL smoother%MPIparallelEnv%init(MPI_COMM_WORLD)
#else
        CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
          "MPI_Comm_ID is nonnegative but MPI is not enabled")
#endif
      ENDIF

      smoother%smootherMethod=CBJ
      smoother%TPLType=PETSc
      CALL params%get('SmootherType->blockMethod',smoother%blockMethod)
      IF(smoother%blockMethod == -1_SIK) smoother%blockMethod=LU
      smoother%hasX0=.FALSE.
      smoother%isKSPSetup=.FALSE.
      smoother%blocksNeedUpdate=.TRUE.

      smoother%isInit=.TRUE.

    ENDSUBROUTINE init_SmootherType_PETSc_CBJ
!
!-------------------------------------------------------------------------------
!> @brief Clears the SmootherType for an CBJ PETSc smoother
!> @param smoother The smoother object to act on
!>
    SUBROUTINE clear_SmootherType_PETSc_CBJ(smoother)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_SmootherType_PETSc_CBJ'
      CLASS(SmootherType_PETSc_CBJ),INTENT(INOUT) :: smoother

      INTEGER(SIK) :: i

      CALL smoother%colorManager%clear()
      CALL smoother%MPIparallelEnv%clear()
      IF(ALLOCATED(smoother%blockSolvers)) THEN
        DO i=smoother%istt,smoother%istp
          CALL smoother%blockSolvers(i)%clear()
        ENDDO
        DEALLOCATE(smoother%blockSolvers)
      ENDIF
      smoother%isKSPSetup=.FALSE.
      smoother%isInit=.FALSE.
      smoother%blocksNeedUpdate=.TRUE.

    ENDSUBROUTINE clear_SmootherType_PETSc_CBJ
!
!-------------------------------------------------------------------------------
!> @brief Initializes the color manager type
!> @param manager The color manager object to act on
!>
    SUBROUTINE init_ColorManagerType(manager,params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ColorManagerType'
      CLASS(ColorManagerType),INTENT(INOUT) :: manager
      TYPE(ParamType),INTENT(IN) :: params

      CALL params%get('SmootherType->istt',manager%istt)
      CALL params%get('SmootherType->istp',manager%istp)
      CALL params%get('SmootherType->num_colors',manager%num_colors)

      !Create the color index lists:
      ALLOCATE(manager%colors(manager%num_colors))
      ALLOCATE(manager%hasColorDefined(manager%num_colors))
      manager%hasColorDefined=.FALSE.
      manager%hasAllColorsDefined=.FALSE.
      !Create the color ID list:
      ALLOCATE(manager%color_ids(manager%istt:manager%istp))
      manager%isInit=.TRUE.

    ENDSUBROUTINE init_ColorManagerType
!
!-------------------------------------------------------------------------------
!> @brief Clears the color manager type
!> @param manager The color manager object to act on
!>
    SUBROUTINE clear_ColorManagerType(manager)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_ColorManagerType'
      CLASS(ColorManagerType),INTENT(INOUT) :: manager
      INTEGER(SIK) :: icolor

      IF(ALLOCATED(manager%colors)) THEN
        DO icolor=1,manager%num_colors
          IF(manager%hasColorDefined(icolor)) THEN
            DEALLOCATE(manager%colors(icolor)%index_list)
            manager%hasColorDefined(icolor)=.FALSE.
          ENDIF
        ENDDO
        DEALLOCATE(manager%colors)
        DEALLOCATE(manager%hasColorDefined)
        manager%hasAllColorsDefined=.FALSE.
      ENDIF
      IF(ALLOCATED(manager%color_ids)) DEALLOCATE(manager%color_ids)
      manager%isInit=.FALSE.

    ENDSUBROUTINE clear_ColorManagerType
!
!-------------------------------------------------------------------------------
!> @brief Clears all the smoother lists
!>
    SUBROUTINE smootherManager_clear
      CHARACTER(LEN=*),PARAMETER :: myName='smootherManager_clear'

      INTEGER(SIK) :: ismoother,iList

      DO iList=1,num_smoother_lists
        IF(ALLOCATED(smootherListCollection(iList)%smootherList)) THEN
          DO ismoother=1,smootherListCollection(iList)%num_smoothers
            IF(ALLOCATED(smootherListCollection(iList)% &
                smootherList(ismoother)%smoother)) THEN
              CALL smootherListCollection(iList)%smootherList(ismoother)% &
                  smoother%clear()
              DEALLOCATE(smootherListCollection(iList)% &
                  smootherList(ismoother)%smoother)
            ENDIF
          ENDDO
          DEALLOCATE(smootherListCollection(iList)%smootherList)
        ENDIF
        IF(ALLOCATED(smootherListCollection(iList)%ctxList)) &
          DEALLOCATE(smootherListCollection(iList)%ctxList)
        smootherListCollection(iList)%isSmootherListInit=.FALSE.
        smootherListCollection(iList)%num_smoothers=0
      ENDDO

      IF(ALLOCATED(smootherListCollection)) DEALLOCATE(smootherListCollection)
      num_smoother_lists=0

    ENDSUBROUTINE smootherManager_clear
!
!-------------------------------------------------------------------------------
!> @brief Sets the ksp object for a given smoother
!>
!> @param ismoother Index of the smoother in the smootherList
!> @param iList Index of the smootherList in smootherListCollection
!> @param ksp Ksp object
!>
    SUBROUTINE smootherManager_setKSP(ismoother,iList,ksp)
      CHARACTER(LEN=*),PARAMETER :: myName='smootherManager_setKSP'
      INTEGER(SIK),INTENT(IN) :: ismoother,iList
      CHARACTER(LEN=11) :: pcname
      CHARACTER(LEN=5) :: pcnumber
#ifdef FUTILITY_HAVE_PETSC
      KSP,INTENT(IN) :: ksp
      PetscErrorCode :: iperr
#else
      INTEGER(SIK),INTENT(IN) :: ksp
      CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
          "This subroutine is only for PETSc smoothers!")
#endif
      IF(.NOT. smootherListCollection(iList)%isSmootherListInit) &
        CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
            "Smoother list has not been initialized!")

      IF(ismoother < 1_SIK .OR. &
          ismoother > smootherListCollection(iList)%num_smoothers) &
        CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
            "Invalid smoother ID!")

      SELECTTYPE(smoother => smootherListCollection(iList)% &
          smootherList(ismoother)%smoother)
        TYPE IS(SmootherType_PETSc_CBJ)
          smoother%ksp=ksp
#ifdef FUTILITY_HAVE_PETSC
          CALL KSPSetType(smoother%ksp,KSPRICHARDSON,iperr)
          CALL KSPGetPC(smoother%ksp,smoother%pc,iperr)
          CALL PCSetType(smoother%pc,PCSHELL,iperr)
          WRITE(pcnumber,'(i5)') ismoother
          pcname="CBJ PC"//pcnumber
          CALL PCShellSetName(smoother%pc,pcname,iperr)
          CALL PCShellSetSetUp(smoother%pc,PCSetUp_CBJ,iperr)
          CALL PCShellSetContext(smoother%pc, &
                  smootherListCollection(iList)%ctxList(ismoother)%ctx,iperr)
          CALL PCShellSetApply(smoother%pc,PCApply_CBJ,iperr)
          !ZZZZ move this to LS_MG or add num_inners here
          IF(smoother%colorManager%hasAllColorsDefined) THEN
            CALL KSPSetTolerances(smoother%ksp,0.0_SRK,0.0_SRK,1E8_SRK, &
                                  smoother%colorManager%num_colors,iperr)
          ENDIF
#endif
          smoother%isKSPSetup=.TRUE.
        CLASS IS(SmootherType_PETSc)
          CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
              "This subroutine has only been implemented for CBJ type PETSc"// &
               "smoothers!")
        CLASS DEFAULT
          CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
              "This subroutine is only for PETSc smoothers!")
      ENDSELECT

    ENDSUBROUTINE smootherManager_setKSP
!
!-------------------------------------------------------------------------------
!> @brief Initializes the smootherList
!>
!> @param params Parameter list with details for each smoother
!> @param iList Index for the list this subroutine is initializing
!>
    SUBROUTINE smootherManager_init(params,iList)
      CHARACTER(LEN=*),PARAMETER :: myName='smootherManager_init'
      TYPE(ParamType),INTENT(IN) :: params
      INTEGER(SIK),INTENT(IN) :: iList

      LOGICAL(SBK) :: tmpbool
      INTEGER(SIK) :: ismoother
      INTEGER(SIK) :: num_smoothers

      INTEGER(SIK),ALLOCATABLE :: istt_list(:),istp_list(:)
      INTEGER(SIK),ALLOCATABLE :: blk_size_list(:),num_colors_list(:)
      INTEGER(SIK),ALLOCATABLE :: smootherMethod_list(:),blockMethod_list(:)
      INTEGER(SIK),ALLOCATABLE :: MPI_Comm_ID_list(:)
      TYPE(ParamType) :: params_out

      !Check parameter list:
      tmpbool=params%has('SmootherType->num_smoothers') .AND. &
              params%has('SmootherType->istt_list') .AND. &
              params%has('SmootherType->istp_list') .AND. &
              params%has('SmootherType->blk_size_list') .AND. &
              params%has('SmootherType->smootherMethod_list') .AND. &
              params%has('SmootherType->MPI_Comm_ID_list')
      IF(.NOT. tmpbool) &
        CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
            "Missing a required parameter from the parameter list!")

      !Extract param list info:
      CALL params%get('SmootherType->num_smoothers',num_smoothers)
      smootherListCollection(iList)%num_smoothers=num_smoothers
      ALLOCATE(istt_list(num_smoothers))
      ALLOCATE(istp_list(num_smoothers))
      ALLOCATE(blk_size_list(num_smoothers))
      ALLOCATE(smootherMethod_list(num_smoothers))
      ALLOCATE(blockMethod_list(num_smoothers))
      ALLOCATE(num_colors_list(num_smoothers))
      ALLOCATE(MPI_Comm_ID_list(num_smoothers))
      CALL params%get('SmootherType->istt_list',istt_list)
      CALL params%get('SmootherType->istp_list',istp_list)
      CALL params%get('SmootherType->blk_size_list',blk_size_list)
      CALL params%get('SmootherType->smootherMethod_list',smootherMethod_list)
      IF(params%has('SmootherType->blockMethod_list')) THEN
        CALL params%get('SmootherType->blockMethod_list',blockMethod_list)
      ELSE
        blockMethod_list=-1_SIK
      ENDIF
      IF(params%has('SmootherType->num_colors_list')) THEN
        CALL params%get('SmootherType->num_colors_list',num_colors_list)
      ELSE
        num_colors_list=1_SIK
      ENDIF
      CALL params%get('SmootherType->MPI_Comm_ID_list',MPI_Comm_ID_list)

      ALLOCATE(smootherListCollection(iList)%smootherList(num_smoothers))
      ALLOCATE(smootherListCollection(iList)%ctxList(num_smoothers))
      DO ismoother=1,num_smoothers
        smootherListCollection(iList)%ctxList(ismoother)%ctx(1)= &
            iList*MAX_SMOOTHERS_PER_LIST+ismoother
        CALL params_out%clear()
        CALL params_out%add('SmootherType->istt',istt_list(ismoother))
        CALL params_out%add('SmootherType->istp',istp_list(ismoother))
        CALL params_out%add('SmootherType->blk_size',blk_size_list(ismoother))
        CALL params_out%add('SmootherType->smootherMethod', &
                            smootherMethod_list(ismoother))
        CALL params_out%add('SmootherType->blockMethod', &
                            blockMethod_list(ismoother))
        CALL params_out%add('SmootherType->num_colors', &
                            num_colors_list(ismoother))
        CALL params_out%add('SmootherType->MPI_Comm_ID', &
                            MPI_Comm_ID_list(ismoother))
        SELECTCASE(smootherMethod_list(ismoother))
          CASE(CBJ)
            ALLOCATE(SmootherType_PETSc_CBJ :: &
              smootherListCollection(iList)%smootherList(ismoother)%smoother)
          CASE DEFAULT
            CALL eSmootherType%raiseDebug(modName//"::"//myName//" - "// &
                "Unknown smoother method, not allocating smoother for this"// &
                " level!")
        ENDSELECT
        IF(ALLOCATED(smootherListCollection(iList)% &
                     smootherList(ismoother)%smoother)) &
          CALL smootherListCollection(iList)%smootherList(ismoother)% &
            smoother%init(params_out)
      ENDDO

      smootherListCollection(iList)%isSmootherListInit=.TRUE.

      DEALLOCATE(istt_list)
      DEALLOCATE(istp_list)
      DEALLOCATE(blk_size_list)
      DEALLOCATE(smootherMethod_list)
      DEALLOCATE(blockMethod_list)
      DEALLOCATE(num_colors_list)
      DEALLOCATE(MPI_Comm_ID_list)

    ENDSUBROUTINE smootherManager_init
!
!-------------------------------------------------------------------------------
!> @brief Initializes the smootherList from a multigrid mesh structure object
!>
!> @param params Parameter list with details for each smoother
!> @param myMMeshes MultigridMeshStructure object to be used to initialize
!>                  smootherList
!> @param iList Index of the smootherList to be initialized
!>
    SUBROUTINE smootherManager_initFromMMeshes(params,myMMeshes,iList)
      CHARACTER(LEN=*),PARAMETER :: myName='smootherManager_initFromMMeshes'
      TYPE(ParamType),INTENT(IN) :: params
      TYPE(MultigridMeshStructureType),INTENT(IN) :: myMMeshes
      INTEGER(SIK),INTENT(IN) :: iList

      INTEGER(SIK) :: iLevel,num_colors,num_smoothers
      TYPE(ParamType) :: params_out

      INTEGER(SIK),ALLOCATABLE :: istt_list(:),istp_list(:)
      INTEGER(SIK),ALLOCATABLE :: blk_size_list(:),num_colors_list(:)
      INTEGER(SIK),ALLOCATABLE :: smootherMethod_list(:),blockMethod_list(:)
      INTEGER(SIK),ALLOCATABLE :: MPI_Comm_ID_list(:),color_ids(:)

      !Check parameter list:
      IF(.NOT. params%has('SmootherType->MPI_Comm_ID_list')) &
        CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
            "Missing MPI_Comm_ID_list from the parameter list!")
      IF(.NOT. params%has('SmootherType->blk_size_list')) &
        CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
            "Missing blk_size_list from the parameter list!")

      !Fill out params_out using params and myMMeshes:
      CALL params_out%clear()
      IF(params%has('SmootherType->num_smoothers')) THEN
        CALL params%edit(0)
        CALL params%get('SmootherType->num_smoothers',num_smoothers)
      ELSE
        num_smoothers=myMMeshes%nLevels
      ENDIF
      smootherListCollection(iList)%num_smoothers=num_smoothers
      CALL params_out%add('SmootherType->num_smoothers',num_smoothers)
      ALLOCATE(istt_list(num_smoothers))
      ALLOCATE(istp_list(num_smoothers))
      ALLOCATE(smootherMethod_list(num_smoothers))
      DO iLevel=1,myMMeshes%nLevels
        istt_list(iLevel)=myMMeshes%meshes(iLevel)%istt
        istp_list(iLevel)=myMMeshes%meshes(iLevel)%istp
      ENDDO
      CALL params_out%add('SmootherType->istt_list',istt_list)
      CALL params_out%add('SmootherType->istp_list',istp_list)
      IF(params%has('SmootherType->smootherMethod_list')) THEN
        CALL params%get('SmootherType->smootherMethod_list',smootherMethod_list)
      ELSE
        smootherMethod_list(1)=GMRES
        smootherMethod_list(2:num_smoothers)=CBJ
      ENDIF
      CALL params_out%add('SmootherType->smootherMethod_list', &
                          smootherMethod_list)
      DEALLOCATE(istt_list)
      DEALLOCATE(istp_list)
      DEALLOCATE(smootherMethod_list)

      ALLOCATE(blk_size_list(num_smoothers))
      CALL params%get('SmootherType->blk_size_list',blk_size_list)
      CALL params_out%add('SmootherType->blk_size_list',blk_size_list)
      DEALLOCATE(blk_size_list)

      IF(params%has('SmootherType->blockMethod_list')) THEN
        ALLOCATE(blockMethod_list(num_smoothers))
        CALL params%get('SmootherType->blockMethod_list',blockMethod_list)
        CALL params_out%add('SmootherType->blockMethod_list',blockMethod_list)
        DEALLOCATE(blockMethod_list)
      ENDIF
      IF(params%has('SmootherType->num_colors_list')) THEN
        ALLOCATE(num_colors_list(num_smoothers))
        CALL params%get('SmootherType->num_colors_list',num_colors_list)
        CALL params_out%add('SmootherType->num_colors_list',num_colors_list)
        DEALLOCATE(num_colors_list)
      ENDIF

      ALLOCATE(MPI_Comm_ID_list(num_smoothers))
      CALL params%get('SmootherType->MPI_Comm_ID_list',MPI_Comm_ID_list)
      CALL params_out%add('SmootherType->MPI_Comm_ID_list',MPI_Comm_ID_list)
      DEALLOCATE(MPI_Comm_ID_list)

      CALL smootherManager_init(params_out,iList)

      !Define red-black coloring for colored smoothers:
      DO iLevel=1,myMMeshes%nLevels
        IF(ALLOCATED(smootherListCollection(iList)% &
                     smootherList(ilevel)%smoother)) THEN
          SELECTTYPE(smoother => smootherListCollection(iList)% &
                                 smootherList(iLevel)%smoother)
            TYPE IS(SmootherType_PETSc_CBJ)
              num_colors=smoother%colorManager%num_colors
              ALLOCATE(color_ids(myMMeshes%meshes(iLevel)%istt: &
                                 myMMeshes%meshes(iLevel)%istp))
              color_ids=MOD(myMMeshes%meshes(iLevel)%interpDegrees, &
                            num_colors)+1
              CALL smootherManager_defineAllColors(iList,iLevel,color_ids)
              DEALLOCATE(color_ids)
          ENDSELECT
        ENDIF
      ENDDO
      CALL params_out%clear()

    ENDSUBROUTINE smootherManager_initFromMMeshes
!
!-------------------------------------------------------------------------------
!> @brief Fill out an index list for a particular color
!>
!> @param iList smootherListCollection index of the smootherList
!> @param ismoother smootherList index of the smoother
!> @param icolor Color being defined
!> @param index_list list of indices for color icolor
!>
    SUBROUTINE smootherManager_defineColor(iList,ismoother,icolor,index_list)
      CHARACTER(LEN=*),PARAMETER :: myName='smootherManager_defineColor'
      INTEGER(SIK),INTENT(IN) :: ismoother
      INTEGER(SIK),INTENT(IN) :: iList
      INTEGER(SIK),INTENT(IN) :: icolor
      INTEGER(SIK),INTENT(IN) :: index_list(:)

      INTEGER(SIK) :: i,num_indices

#ifdef FUTILITY_HAVE_PETSC
      PetscErrorCode :: iperr
#endif

      IF(.NOT. smootherListCollection(iList)%isSmootherListInit) &
        CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
            "Smoother list has not been initialized!")

      IF(ismoother < 1_SIK .OR. &
         ismoother > smootherListCollection(iList)%num_smoothers) &
        CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
            "Invalid smoother ID!")

      SELECTTYPE(smoother => smootherListCollection(iList)% &
                             smootherList(ismoother)%smoother)
        TYPE IS(SmootherType_PETSc_CBJ)
          ASSOCIATE(manager=>smoother%colorManager)
            IF(.NOT. manager%isInit) &
              CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
                  "Color manager must be initialized first!")

            num_indices=SIZE(index_list)
            manager%colors(icolor)%num_indices=num_indices
            ALLOCATE(manager%colors(icolor)%index_list(num_indices))
            manager%colors(icolor)%index_list=index_list
            DO i=1,num_indices
               manager%color_ids(index_list(i))=icolor
            ENDDO
            manager%hasColorDefined(icolor)=.TRUE.
            manager%hasAllColorsDefined=ALL(manager%hasColorDefined)

#ifdef FUTILITY_HAVE_PETSC
            IF(manager%hasAllColorsDefined .AND. smoother%isKSPSetup) THEN
              CALL KSPSetTolerances(smoother%ksp,0.0_SRK,0.0_SRK,1E8_SRK, &
                                 smoother%colorManager%num_colors,iperr)
            ENDIF
#endif

          ENDASSOCIATE
        CLASS DEFAULT
          CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
              "This smoother type does not have support for coloring!")
      ENDSELECT

    ENDSUBROUTINE smootherManager_defineColor
!
!-------------------------------------------------------------------------------
!> @brief Provide the color_ids to fill out the index_lists for all the colors
!>
!> @param iList smootherListCollection index of the smootherList
!> @param ismoother smootherList index of the smoother
!> @param color_ids List of colors for each point, must have bounds
!>         solver%istt:solver%istp
!>
    SUBROUTINE smootherManager_defineAllColors(iList,ismoother,color_ids)
      CHARACTER(LEN=*),PARAMETER :: myName='defineAllColors_ColorManagerType'
      INTEGER(SIK),INTENT(IN) :: iList
      INTEGER(SIK),INTENT(IN) :: ismoother
      INTEGER(SIK),INTENT(IN),ALLOCATABLE :: color_ids(:)

      INTEGER(SIK) :: icolor,i
      INTEGER(SIK),ALLOCATABLE :: tmpints(:)
#ifdef FUTILITY_HAVE_PETSC
      PetscErrorCode :: iperr
#endif

      IF(.NOT. smootherListCollection(iList)%isSmootherListInit) &
        CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
            "Smoother list has not been initialized!")

      IF(ismoother < 1_SIK .OR. &
          ismoother > smootherListCollection(iList)%num_smoothers) &
        CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
            "Invalid smoother ID!")

      SELECTTYPE(smoother => smootherListCollection(iList)% &
                             smootherList(ismoother)%smoother)
        TYPE IS(SmootherType_PETSc_CBJ)
          ASSOCIATE(manager=>smoother%colorManager)
            manager%color_ids=color_ids

            !Get the number of indices for each color:
            DO icolor=1,manager%num_colors
              manager%colors(icolor)%num_indices=0
            ENDDO
            DO i=manager%istt,manager%istp
              icolor=color_ids(i)
              manager%colors(icolor)%num_indices= &
                 manager%colors(icolor)%num_indices+1
            ENDDO

            !Allocate based on # of indices for each color:
            DO icolor=1,manager%num_colors
              ALLOCATE(manager%colors(icolor)% &
                       index_list(manager%istp-manager%istt+1))
            ENDDO

            !Fill out index lists for each color:
            ALLOCATE(tmpints(manager%num_colors))
            tmpints=0_SIK
            DO i=manager%istt,manager%istp
              icolor=color_ids(i)
              tmpints(icolor)=tmpints(icolor)+1
              manager%colors(icolor)%index_list(tmpints(icolor))=i
            ENDDO
            DEALLOCATE(tmpints)
            manager%hasColorDefined=.TRUE.
            manager%hasAllColorsDefined=.TRUE.

#ifdef FUTILITY_HAVE_PETSC
            IF(smoother%isKSPSetup) THEN
              !TODO: more smoother steps per iteration
              !TODO need to put this in a different place...
              !     KSP is not always set up by this point
              CALL KSPSetTolerances(smoother%ksp,0.0_SRK,0.0_SRK,1E8_SRK, &
                                    smoother%colorManager%num_colors,iperr)
            ENDIF
#endif
          ENDASSOCIATE
        CLASS DEFAULT
          CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
              "This smoother type does not have support for coloring!")
      ENDSELECT

    ENDSUBROUTINE smootherManager_defineAllColors

!
!-------------------------------------------------------------------------------
!> @brief Request that the LU factorization be updated.  This needs to be
!>        called every time the diagonal blocks of a matrix are changed.
!>
!> @param iList smootherListCollection index of the smootherList
!>
    SUBROUTINE smootherManager_requestAllBlocksUpdate(iList)
      CHARACTER(LEN=*),PARAMETER :: myName='smootherManager_requestAllBlocksUpdate'
      INTEGER(SIK),INTENT(IN) :: iList
      INTEGER(SIK) :: ismoother

      IF(.NOT. smootherListCollection(iList)%isSmootherListInit) &
        CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
            "Smoother list has not been initialized!")

      DO ismoother=1,smootherListCollection(iList)%num_smoothers
        IF(ALLOCATED(smootherListCollection(iList)% &
                     smootherList(ismoother)%smoother)) THEN
          SELECTTYPE(smoother => smootherListCollection(iList)% &
                                 smootherList(ismoother)%smoother)
            TYPE IS(SmootherType_PETSc_CBJ)
              smoother%blocksNeedUpdate=.TRUE.
          ENDSELECT
        ENDIF
      ENDDO

    ENDSUBROUTINE smootherManager_requestAllBlocksUpdate
!
!-------------------------------------------------------------------------------
!> @brief PETSc PCSetup function for PCSHELL for the colored block Jacobi
!>        scheme
!>
!> @param smoother Smoother object which owns the SHELL
!> @param pc PETSc PC context
!> @param iperr PetscErrorCode
!>
#ifdef FUTILITY_HAVE_PETSC
    SUBROUTINE PCSetup_CBJ(pc,iperr)
      CHARACTER(LEN=*),PARAMETER :: myName='PCSetup_CBJ'
      PC,INTENT(INOUT) :: pc
      PetscErrorCode,INTENT(INOUT) :: iperr

      INTEGER(SIK) :: listID,smootherID,nnz
      INTEGER(SIK) :: localrowstart,localrowend,localcolind,rowstart
      INTEGER(SIK) :: blockrowind,blockcolind
      TYPE(C_PTR) :: ctx_ptr
      PetscInt,POINTER :: ctx(:),ia(:),ja(:)
      PetscInt :: numrows
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6) && (PETSC_VERSION_SUBMINOR>=4))
      PetscReal,POINTER :: matvals(:)
#else
      PetscReal,POINTER :: matvals(:,:)
#endif
      TYPE(ParamType) :: params
      Mat :: Amat,Pmat
      Mat :: localmat
      MatType :: globalmattype
      PetscBool :: done
      !Iteration variables:
      INTEGER(SIK) :: i,j,localrowind

      !Get the smoother ID:
      CALL PCShellGetContext(pc,ctx_ptr,iperr)
      CALL C_F_POINTER(ctx_ptr,ctx,(/1/))
      listID=ctx(1)/MAX_SMOOTHERS_PER_LIST
      smootherID=MOD(ctx(1),MAX_SMOOTHERS_PER_LIST)

      SELECTTYPE(smoother => smootherListCollection(listID)% &
          smootherList(smootherID)%smoother)
        TYPE IS(SmootherType_PETSc_CBJ)
          IF(.NOT. smoother%isInit) &
            CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
                "Smoother must be initialized first!")
          IF(.NOT. smoother%colorManager%hasAllColorsDefined) &
            CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
                "Smoother's color manager must have its colors defined first!")

          IF(smoother%blocksNeedUpdate) THEN
            IF(.NOT. ALLOCATED(smoother%blockSolvers)) THEN
              ALLOCATE(LinearSolverType_Direct :: &
                        smoother%blockSolvers(smoother%istt:smoother%istp))
            ENDIF
            CALL params%clear()
            CALL params%add('LinearSolverType->TPLType',NATIVE)
            CALL params%add('LinearSolverType->solverMethod',LU)
            !This part is serial, so we don't need an MPI communicator
            CALL params%add('LinearSolverType->MPI_Comm_ID',-1_SIK)
            CALL params%add('LinearSolverType->timerName', &
                            'LU solver for smoother blocks')
            CALL params%add('LinearSolverType->numberOMP',1_SIK)
            CALL params%add('LinearSolverType->matType',DENSESQUARE)
            CALL params%add('LinearSolverType->A->MatrixType->n', &
                            smoother%blk_size)
            CALL params%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
            CALL params%add('LinearSolverType->x->VectorType->n', &
                            smoother%blk_size)
            CALL params%add('LinearSolverType->b->VectorType->n', &
                            smoother%blk_size)
            !Get the block matrices:
            CALL PCGetOperators(pc,Amat,Pmat,iperr)
            CALL MatGetType(Pmat,globalmattype,iperr)
            IF(globalmattype == MATMPIAIJ) THEN
              CALL MatMPIAIJGetLocalMat(Pmat,MAT_INITIAL_MATRIX,localmat,iperr)
            ELSE
              localmat=Pmat
            ENDIF
            CALL MatSeqAIJGetArrayF90(localmat,matvals,iperr)
            CALL MatGetRowIJF90(localmat,1_SIK,PETSC_FALSE,PETSC_FALSE, &
                                  numrows,ia,ja,done,iperr)
            nnz=ia(numrows+1)-1
            IF(.NOT. done) &
              CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
                  "Unable to retrieve row and column indices!")

            !Global --> global indices, includes all procs, 1:total prob size
            !Local --> local to processor, 1:local prob size
            !Block --> local to block, 1:blk_size
            rowstart=(smoother%istt-1)*smoother%blk_size+1
            !rowend=smoother%istp*smoother%blk_size
            DO i=smoother%istt,smoother%istp
            !Loop over all local blocks
              IF(.NOT. smoother%blockSolvers(i)%isInit) &
                CALL smoother%blockSolvers(i)%init(params)
              localrowstart=(i-smoother%istt)*smoother%blk_size+1
              localrowend=(i-smoother%istt+1)*smoother%blk_size
              SELECTTYPE(A => smoother%blockSolvers(i)%A)
                TYPE IS(DenseSquareMatrixType)
                  A%a=0.0_SRK
                  DO localrowind=localrowstart,localrowend
                  !Loop over rows corresponding to block i
                    blockrowind=localrowind-localrowstart+1
                    DO j=ia(localrowind),ia(localrowind+1)-1
                    !Loop over nonzero entries of row localrowind
                      localcolind=ja(j)-rowstart+1
                      !If it is not in the diagonal block, continue
                      IF(localcolind < localrowstart .OR. &
                         localcolind > localrowend) CYCLE
                      !Otherwise, store the value in a densesquarematrix:
                      blockcolind=localcolind-localrowstart+1
                      A%a(blockrowind,blockcolind)= &
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6) && (PETSC_VERSION_SUBMINOR>=4))
                        matvals(j)
#else
                        matvals(MOD(j-1,numrows)+1,(j-1)/numrows+1)
#endif
                    ENDDO !j=i
                  ENDDO !localrowind
              ENDSELECT !smoother%blockSolvers(i)%A
            ENDDO !i=istt,istp
            CALL MatRestoreRowIJF90(localmat,1_SIK,PETSC_FALSE,PETSC_FALSE, &
                                  numrows,ia,ja,done,iperr)
            IF(.NOT. done) &
              CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
                  "Unable to restore row and column indices!")
            CALL MatSeqAIJRestoreArrayF90(localmat,matvals,iperr)
            CALL params%clear()
            smoother%blocksNeedUpdate=.FALSE.
          ENDIF
        CLASS DEFAULT
          CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
              "This subroutine is only for CBJ smoothers!")
      ENDSELECT


      iperr=0_SIK
    ENDSUBROUTINE PCSetup_CBJ
#endif
!
!-------------------------------------------------------------------------------
!> @brief PETSc PCApply function for PCSHELL for the colored block Jacobi
!>        scheme
!>
!> @param smoother Smoother object which owns the SHELL
!> @param pc PETSc PC context
!> @param iperr PetscErrorCode
!>
#ifdef FUTILITY_HAVE_PETSC
    SUBROUTINE PCApply_CBJ(pc,xin,xout,iperr)
      CHARACTER(LEN=*),PARAMETER :: myName='PCApply_CBJ'
      PC,INTENT(INOUT) :: pc
      Vec,INTENT(INOUT) :: xin,xout
      PetscErrorCode,INTENT(INOUT) :: iperr

      INTEGER(SIK) :: listID,smootherID,nlocal
      TYPE(C_PTR) :: ctx_ptr
      PetscInt,POINTER :: ctx(:)
      PetscReal,POINTER :: xin_vals(:),xout_vals(:)

      INTEGER(SIK) :: localrowstart,localrowend
      INTEGER(SIK) :: i

      !Get the smoother ID:
      CALL PCShellGetContext(pc,ctx_ptr,iperr)
      CALL C_F_POINTER(ctx_ptr,ctx,(/1/))
      listID=ctx(1)/MAX_SMOOTHERS_PER_LIST
      smootherID=MOD(ctx(1),MAX_SMOOTHERS_PER_LIST)

      SELECTTYPE(smoother => smootherListCollection(listID)% &
          smootherList(smootherID)%smoother)
        TYPE IS(SmootherType_PETSc_CBJ)
          IF(.NOT. smoother%isInit) &
            CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
                "Smoother must be initialized first!")
          IF(.NOT. smoother%colorManager%hasAllColorsDefined) &
            CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
                "Smoother's color manager must have its colors defined first!")

          nlocal=smoother%blk_size*(smoother%istp-smoother%istt+1)

#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
          CALL VecGetArrayReadF90(xin,xin_vals,iperr)
#else
          CALL VecGetArrayF90(xin,xin_vals,iperr)
#endif
          CALL VecGetArrayF90(xout,xout_vals,iperr)
          !TODO rewrite LSTypes to allow for option to not store b or x
          ! for each block.  We only need one instance of b or x at a time
          ! we want to do threaded parallelism
          ASSOCIATE(manager=>smoother%colorManager)
            DO i=smoother%istt,smoother%istp
            !Loop over all local blocks of color icolor
              localrowstart=(i-smoother%istt)*smoother%blk_size+1
              localrowend=(i-smoother%istt+1)*smoother%blk_size
              IF(manager%color_ids(i) == smoother%icolor) THEN
                SELECTTYPE(x => smoother%blockSolvers(i)%x)
                TYPE IS(RealVectorType)
                SELECTTYPE(b => smoother%blockSolvers(i)%b)
                TYPE IS(RealVectorType)
                  b%b=xin_vals(localrowstart:localrowend)
                  CALL smoother%blockSolvers(i)%solve()
                  xout_vals(localrowstart:localrowend)=x%b
                ENDSELECT
                ENDSELECT
              ELSE
                xout_vals(localrowstart:localrowend)=0.0_SRK
              ENDIF
            ENDDO
            !Increment the color
            smoother%icolor=smoother%icolor+1
            IF(smoother%icolor > manager%num_colors) smoother%icolor=1_SIK
          ENDASSOCIATE
          CALL VecRestoreArrayF90(xout,xout_vals,iperr)
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
          CALL VecRestoreArrayReadF90(xin,xin_vals,iperr)
#else
          CALL VecRestoreArrayF90(xin,xin_vals,iperr)
#endif
        CLASS DEFAULT
          CALL eSmootherType%raiseError(modName//"::"//myName//" - "// &
              "This subroutine is only for CBJ smoothers!")
      ENDSELECT

      iperr=0_SIK
    ENDSUBROUTINE PCApply_CBJ
#endif

ENDMODULE SmootherTypes
