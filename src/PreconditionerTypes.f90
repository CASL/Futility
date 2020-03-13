!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module provides a preconditioner type and methods to set up different
!> preconditioners
!>
!> The precontioner type owns its own matrix. It is initialized with a matrix.
!> In general any third party library (TPL) that may be interfaced with this
!> module should be implemented such that it's optional.
!>
!> Currently supported TPLs include:
!>  -
!>
!> @par EXAMPLES
!> @code
!>
!> @endcode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE PreconditionerTypes
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE BLAS
USE ExceptionHandler
USE Allocs
USE ParameterLists
USE ParallelEnv
USE VectorTypes
USE MatrixTypes
USE Constants_Conversion

IMPLICIT NONE
PRIVATE

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
#undef IS
#else
#ifdef HAVE_MPI
#include <mpif.h>
#endif
#endif

!
! List of Public members
PUBLIC :: PreconditionerType
PUBLIC :: DistributedPrecond
PUBLIC :: LU_PreCondType
PUBLIC :: ILU_PreCondType
PUBLIC :: SOR_PreCondType
PUBLIC :: RSOR_PreCondType
PUBLIC :: Jacobi_PreCondType
#ifdef HAVE_MPI
PUBLIC :: DistributedSOR_PreCondType
PUBLIC :: DistributedRSOR_PreCondType
PUBLIC :: DistributedJacobi_PreCondType
#endif
PUBLIC :: ePreCondType

#ifdef FUTILITY_HAVE_PETSC
PUBLIC :: PETSC_PCSHELL_SETUP_extern
PUBLIC :: PETSC_PCSHELL_APPLY_extern
#endif

PUBLIC :: PETSC_PCSHELL_PC

!> @brief The base preconditioner type
TYPE,ABSTRACT :: PreConditionerType
  !> Initialization status
  LOGICAL(SBK) :: isInit=.FALSE.
  !> Pointer to matrix being preconditioned
  CLASS(MatrixType),POINTER :: A=>NULL()
!
!List of Type Bound Procedures
  CONTAINS
    !> Deferred routine for initializing the preconditioner
    PROCEDURE(precond_init_absintfc),DEFERRED,PASS :: init
    !> Deferred routine for clearing the preconditioner
    PROCEDURE(precond_absintfc),DEFERRED,PASS :: clear
    !> Deferred routine for seting up the preconditioner
    PROCEDURE(precond_absintfc),DEFERRED,PASS :: setup
    !> Deferred routine for applying the preconditioner
    PROCEDURE(precond_apply_absintfc),DEFERRED,PASS :: apply
ENDTYPE PreConditionerType

!> @brief The extended type for distributed preconditioners
TYPE,ABSTRACT,EXTENDS(PreConditionerType) :: DistributedPrecond
  !> MPI comm ID
  INTEGER(SIK) :: comm=-1
ENDTYPE DistributedPrecond

!> @brief The extended type for LU based preconditioners
TYPE,ABSTRACT,EXTENDS(PreConditionerType) :: LU_PreCondType
  !> L matrix
  CLASS(MatrixType),ALLOCATABLE :: L
  !> U matrix
  CLASS(MatrixType),ALLOCATABLE :: U
  !> Combined LU matrix
  CLASS(MatrixType),ALLOCATABLE :: LU
!
!List of Type Bound Procedures
  CONTAINS
    !> @copybrief MatrixTypes::init_LU_PreCondType
    !> @copydetails MatrixTypes::init_LU_PreCondType
    PROCEDURE,PASS :: init => init_LU_PreCondType
    !> @copybrief MatrixTypes::clear_LU_PreCondType
    !> @copydetails MatrixTypes::clear_LU_PreCondType
    PROCEDURE,PASS :: clear => clear_LU_PreCondType
    !> Deferred routine for seting up the preconditioner
    PROCEDURE(precond_LU_absintfc),DEFERRED,PASS :: setup
    !> Deferred routine for applying the preconditioner
    PROCEDURE(precond_applyLU_absintfc),DEFERRED,PASS :: apply
ENDTYPE LU_PreCondType

!> @brief The extended type for the ILU preconditioner
TYPE,EXTENDS(LU_PreCondType) :: ILU_PreCondType
!
!List of Type Bound Procedures
  CONTAINS
    !> @copybrief MatrixTypes::setup_ILU_PreCondType
    !> @copydetails MatrixTypes::setup_ILU_PreCondType
    PROCEDURE,PASS :: setup => setup_ILU_PreCondType
    !> @copybrief MatrixTypes::apply_ILU_PreCondType
    !> @copydetails MatrixTypes::apply_ILU_PreCondType
    PROCEDURE,PASS :: apply => apply_ILU_PreCondType
ENDTYPE ILU_PreCondType

!> @brief The extended type for SOR based preconditioners
TYPE,ABSTRACT,EXTENDS(PreConditionerType) :: SOR_PreCondType
  !> Size of the diagonal blocks
  INTEGER(SIK) :: blockSize
  !> Number of diagonal blocks in matrix
  INTEGER(SIK) :: numBlocks
  !> Omega factor for sor
  REAL(SRK) :: omega
  !> Array of LU matrices for each diagonal block, will be dense!
  CLASS(MatrixType),ALLOCATABLE :: LU(:)
  !> Lower and upper portions of matrix with diagonal blocks removed
  CLASS(MatrixType),ALLOCATABLE :: LpU
  !
  !List of Type Bound Procedures
  CONTAINS
    !> @copybrief MatrixTypes::init_SOR_PreCondType
    !> @copydetails MatrixTypes::init_SOR_PreCondType
    PROCEDURE,PASS :: init => init_SOR_PreCondType
    !> @copybrief MatrixTypes::clear_SOR_PreCondType
    !> @copydetails MatrixTypes::clear_SOR_PreCondType
    PROCEDURE,PASS :: clear => clear_SOR_PreCondType
    !> Deferred routine for setting up the preconditioner
    PROCEDURE(precond_SOR_absintfc),DEFERRED,PASS :: setup
    !> Deferred routine for applying the preconditioner
    PROCEDURE(precond_applySOR_absintfc),DEFERRED,PASS :: apply
ENDTYPE SOR_PreCondType

!> @brief The extended type for the RSOR preconditioner
TYPE,EXTENDS(SOR_PreCondType) :: RSOR_PreCondType
!
!List of Type Bound Procedures
  CONTAINS
    !> @copybrief MatrixTypes::setup_RSOR_PreCondType
    !> @copydetails MatrixTypes::setup_RSOR_PreCondType
    PROCEDURE,PASS :: setup => setup_RSOR_PreCondType
    !> @copybrief MatrixTypes::apply_RSOR_PreCondType
    !> @copydetails MatrixTypes::apply_RSOR_PreCondType
    PROCEDURE,PASS :: apply => apply_RSOR_PreCondType
ENDTYPE RSOR_PreCondType

#ifdef HAVE_MPI
!> @brief The extended type for distributed SOR based preconditioners
TYPE,ABSTRACT,EXTENDS(DistributedPrecond) :: DistributedSOR_PreCondType
  !> Size of the diagonal blocks
  INTEGER(SIK) :: blockSize
  !> Number of blocks assigned to a processor
  INTEGER(SIK) :: nLocalBlocks
  !> First block assigned to a processor
  INTEGER(SIK) :: blockOffset
  !> Omega factor for sor
  REAL(SRK) :: omega
  !> Array of LU matrices for each diagonal block, will be dense!
  CLASS(MatrixType),ALLOCATABLE :: LU(:)
  !> Lower and upper portions of matrix with diagonal blocks removed
  CLASS(MatrixType),POINTER :: LpU
  !
  !List of Type Bound Procedures
  CONTAINS
    !> @copybrief MatrixTypes::init_DistributedSOR_PreCondType
    !> @copydetails MatrixTypes::init_DistributedSOR_PreCondType
    PROCEDURE,PASS :: init => init_DistributedSOR_PreCondType
    !> @copybrief MatrixTypes::clear_DistributedSOR_PreCondType
    !> @copydetails MatrixTypes::clear_DistributedSOR_PreCondType
    PROCEDURE,PASS :: clear => clear_DistributedSOR_PreCondType
    !> Deferred routine for setting up the preconditioner
    PROCEDURE(precond_DistributedSOR_absintfc),DEFERRED,PASS :: setup
    !> Deferred routine for applying the preconditioner
    PROCEDURE(precond_applyDistributedSOR_absintfc),DEFERRED,PASS :: apply
ENDTYPE DistributedSOR_PreCondType

!> @brief The extended type for the distributed RSOR preconditioner
TYPE,EXTENDS(DistributedSOR_PreCondType) :: DistributedRSOR_PreCondType
  !
  !List of Type Bound Procedures
  CONTAINS
    !> @copybrief MatrixTypes::setup_DistributedRSOR_PreCondType
    !> @copydetails MatrixTypes::setup_DistributedRSOR_PreCondType
    PROCEDURE,PASS :: setup => setup_DistributedRSOR_PreCondType
    !> @copybrief MatrixTypes::apply_DistributedRSOR_PreCondType
    !> @copydetails MatrixTypes::apply_DistributedRSOR_PreCondType
    PROCEDURE,PASS :: apply => apply_DistributedRSOR_PreCondType
ENDTYPE DistributedRSOR_PreCondType
#endif

!> @brief The extended type for the Jacobi preconditioner. Intended
!>        for serial matrices
TYPE,EXTENDS(PreConditionerType) :: Jacobi_PreCondType
  !> Inverse diagonal elements
  REAL(SRK),ALLOCATABLE :: invDiag(:)
  CONTAINS
    !> @copybrief PreconditionerTypes::init_Jacobi_PreCondType
    !> @copydetails PreconditionerTypes::init_Jacobi_PreCondType
    PROCEDURE,PASS :: init => init_Jacobi_PreCondType
    !> @copybrief PreconditionerTypes::clear_Jacobi_PreCondType
    !> @copydetails PreconditionerTypes::clear_Jacobi_PreCondType
    PROCEDURE,PASS :: clear => clear_Jacobi_PreCondType
    !> @copybrief PreconditionerTypes::setup_Jacobi_PreCondType
    !> @copydetails PreconditionerTypes::setup_Jacobi_PreCondType
    PROCEDURE,PASS :: setup => setup_Jacobi_PreCondType
    !> @copybrief PreconditionerTypes::apply_Jacobi_PreCondType
    !> @copydetails PreconditionerTypes::apply_Jacobi_PreCondType
    PROCEDURE,PASS :: apply => apply_Jacobi_PreCondType
ENDTYPE Jacobi_PreCondType

#ifdef HAVE_MPI
!> @brief The extended type for the Jacobi preconditioner. Intended
!>        for serial matrices
TYPE,EXTENDS(DistributedPreCond) :: DistributedJacobi_PreCondType
  TYPE(NativeDistributedVectorType) :: invDiag
  CONTAINS
    !> @copybrief PreconditionerTypes::init_DistributedJacobi_PreCondType
    !> @copydetails PreconditionerTypes::init_DistributedJacobi_PreCondType
    PROCEDURE,PASS :: init => init_DistributedJacobi_PreCondType
    !> @copybrief PreconditionerTypes::clear_DistributedJacobi_PreCondType
    !> @copydetails PreconditionerTypes::clear_DistributedJacobi_PreCondType
    PROCEDURE,PASS :: clear => clear_DistributedJacobi_PreCondType
    !> @copybrief PreconditionerTypes::setup_DistributedJacobi_PreCondType
    !> @copydetails PreconditionerTypes::setup_DistributedJacobi_PreCondType
    PROCEDURE,PASS :: setup => setup_DistributedJacobi_PreCondType
    !> @copybrief PreconditionerTypes::apply_DistributedJacobi_PreCondType
    !> @copydetails PreconditionerTypes::apply_DistributedJacobi_PreCondType
    PROCEDURE,PASS :: apply => apply_DistributedJacobi_PreCondType

ENDTYPE DistributedJacobi_PrecondType
#endif
!
!List of Abstract Interfaces
!> @brief The interface to preconditioner initialization routines
ABSTRACT INTERFACE
  SUBROUTINE precond_init_absintfc(thisPC,A,params)
    !notice you need to import all necessary types for abstract interfaces
    IMPORT :: PreconditionerType,Matrixtype,ParamType
    CLASS(PreconditionerType),INTENT(INOUT) :: thisPC
    CLASS(MatrixType),TARGET,INTENT(IN) :: A
    ! paramtype is a custom type that can take any intrinsic variable type
    !including multiple variables
    TYPE(ParamType),INTENT(IN),OPTIONAL :: params
  ENDSUBROUTINE precond_init_absintfc
ENDINTERFACE

!> @brief The interface to preconditioner application routines
ABSTRACT INTERFACE
  SUBROUTINE precond_apply_absintfc(thisPC,v)
    IMPORT :: PreconditionerType,VectorType
    CLASS(PreconditionerType),INTENT(INOUT) :: thisPC
    CLASS(VectorType),INTENT(INOUT) :: v
  ENDSUBROUTINE precond_apply_absintfc

  SUBROUTINE precond_applyLU_absintfc(thisPC,v)
    IMPORT :: LU_PreCondType,VectorType
    CLASS(LU_PreCondType),INTENT(INOUT) :: thisPC
    CLASS(VectorType),INTENT(INOUT) :: v
  ENDSUBROUTINE precond_applyLU_absintfc

  SUBROUTINE precond_applySOR_absintfc(thisPC,v)
    IMPORT :: SOR_PreCondType,VectorType
    CLASS(SOR_PreCondType),INTENT(INOUT) :: thisPC
    CLASS(VectorType),INTENT(INOUT) :: v
  ENDSUBROUTINE precond_applySOR_absintfc

#ifdef HAVE_MPI
  SUBROUTINE precond_applyDistributedSOR_absintfc(thisPC,v)
    IMPORT :: DistributedSOR_PreCondType,VectorType
    CLASS(DistributedSOR_PreCondType),INTENT(INOUT) :: thisPC
    CLASS(VectorType),INTENT(INOUT) :: v
  ENDSUBROUTINE precond_applyDistributedSOR_absintfc
#endif
ENDINTERFACE

!> @brief The interface to preconditioner setup/clear routines
ABSTRACT INTERFACE
  SUBROUTINE precond_absintfc(thisPC)
    IMPORT :: PreconditionerType
    CLASS(PreconditionerType),INTENT(INOUT) :: thisPC
  ENDSUBROUTINE precond_absintfc

  SUBROUTINE precond_LU_absintfc(thisPC)
    IMPORT :: LU_PreCondType
    CLASS(LU_PreCondType),INTENT(INOUT) :: thisPC
  ENDSUBROUTINE precond_LU_absintfc

  SUBROUTINE precond_SOR_absintfc(thisPC)
    IMPORT :: SOR_PreCondType
    CLASS(SOR_PreCondType),INTENT(INOUT) :: thisPC
  ENDSUBROUTINE precond_SOR_absintfc

#ifdef HAVE_MPI
  SUBROUTINE precond_DistributedSOR_absintfc(thisPC)
    IMPORT :: DistributedSOR_PreCondType
    CLASS(DistributedSOR_PreCondType),INTENT(INOUT) :: thisPC
  ENDSUBROUTINE precond_DistributedSOR_absintfc
#endif
ENDINTERFACE

CLASS(PreConditionerType),POINTER :: PETSC_PCSHELL_PC => NULL()

!> set enumeration scheme for BILU preconditioners
INTEGER(SIK),PARAMETER,PUBLIC :: BILU=0,BILUSGS=1

!> Name of module
CHARACTER(LEN=*),PARAMETER :: modName='PreconditionerTypes'

TYPE(ExceptionHandlerType),SAVE :: ePreCondType
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the LU Preconditioner Type with a parameter list
!> @param params The parameter list
!> @param thisPC The preconditioner to act on
!> @param A The matrix to precondition
!>
SUBROUTINE init_LU_PreCondtype(thisPC,A,params)
  CHARACTER(LEN=*),PARAMETER :: myName='init_LU_PreCondType'
  CLASS(LU_PrecondType),INTENT(INOUT) :: thisPC
  CLASS(MatrixType),ALLOCATABLE,TARGET,INTENT(IN) :: A
  TYPE(ParamType),INTENT(IN),OPTIONAL :: params

  IF(thisPC%isinit) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - Preconditioner is already initialized!')
    RETURN
  ENDIF

  IF(.NOT.(ALLOCATED(A))) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - Matrix being used for LU Preconditioner is not allocated!')
    RETURN
  ENDIF
  IF(.NOT.(A%isInit)) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - Matrix being used for LU Preconditioner is not initialized!')
    RETURN
  ENDIF
  thisPC%A => A

  ! This might not be necessary here, but not sure
  SELECT TYPE(mat => thisPC%A)
  TYPE IS(DenseSquareMatrixType)
    ALLOCATE(DenseSquareMatrixType :: thisPC%LU)

    ! Assign A to LU
    SELECT TYPE(LU => thisPC%LU); TYPE IS(DenseSquareMatrixType)
      LU=mat
    ENDSELECT

    IF(thisPC%LU%isInit) THEN
      thisPC%isInit=.TRUE.
    ELSE
      CALL ePreCondtype%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - In LU Preconditioner initialization, LU was not properly initialized!')
    ENDIF
  TYPE IS(SparseMatrixType)
    SELECT TYPE(thisPC)
    TYPE IS(ILU_PrecondType)
      ALLOCATE(SparseMatrixType :: thisPC%LU)
      ! Assign A to LU
      SELECT TYPE(LU => thisPC%LU); TYPE IS(SparseMatrixType)
        LU=mat
      ENDSELECT

      IF(thisPC%LU%isInit) THEN
        thisPC%isInit=.TRUE.
      ELSE
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
            ' = In LU Preconditioner initialization, LU was not properly initialized!')
      ENDIF
    ENDSELECT
  CLASS DEFAULT
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - LU Preconditioners are not supported for input matrix type!')
  ENDSELECT
ENDSUBROUTINE init_LU_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Clears the LU Preconditioner Type with a parameter list
!> @param thisPC The preconditioner to act on
!>
SUBROUTINE clear_LU_PreCondtype(thisPC)
  CLASS(LU_PrecondType),INTENT(INOUT) :: thisPC

  IF(ASSOCIATED(thisPC%A)) NULLIFY(thisPC%A)
  IF(ALLOCATED(thisPC%LU)) THEN
    CALL thisPC%LU%clear()
    DEALLOCATE(thisPC%LU)
  ENDIF
  thisPC%isInit=.FALSE.
ENDSUBROUTINE clear_LU_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Applies the ILU Preconditioner Type with a parameter list
!> @param thisPC The preconditioner to act on
!> @param v The matrix to apply the preconditioner to
!>
SUBROUTINE apply_ILU_PreCondType(thisPC,v)
  CLASS(ILU_PrecondType),INTENT(INOUT) :: thisPC
  CLASS(Vectortype),ALLOCATABLE,INTENT(INOUT) :: v
  CHARACTER(LEN=*),PARAMETER :: myName='apply_ILU_PreCondType'

  IF(.NOT.(thisPC%isInit)) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - Preconditioner is not initialized.')
  ELSEIF(.NOT.(ALLOCATED(v))) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - VectorType is not allocated.')
  ELSE
    IF(.NOT.(v%isInit)) THEN
      CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - VectorType is not initialized.')
    ELSE
      SELECT TYPE(v)
      CLASS IS(RealVectorType)
        CALL BLAS_matvec(THISMATRIX=thisPC%LU,X=v,Y=v,UPLO='L',DIAG='U',TRANS='N')
        CALL BLAS_matvec(THISMATRIX=thisPC%LU,X=v,Y=v,UPLO='U',DIAG='N',TRANS='N')
      CLASS DEFAULT
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
            ' - Vector type is not support by this PreconditionerType.')
      ENDSELECT
    ENDIF
  ENDIF
ENDSUBROUTINE apply_ILU_PreCondType
!
!-------------------------------------------------------------------------------
!> @brief Sets up the ILU Preconditioner Type with a parameter list
!> @param thisPC The preconditioner to act on
!>
SUBROUTINE setup_ILU_PreCondtype(thisPC)
  CLASS(ILU_PrecondType),INTENT(INOUT) :: thisPC

  CHARACTER(LEN=*),PARAMETER :: myName='setup_ILU_PreCondType'
  INTEGER(SIK) :: row,col,col2,j
  REAL(SRK) :: val1,val2,val3

  IF(.NOT.(thisPC%isinit)) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - Preconditioner is not initialized!')
  ELSEIF(.NOT.(ALLOCATED(thisPC%LU))) THEN
      CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Matrix being used for LU Preconditioner is not allocated!')
  ELSE
    IF(.NOT.(thisPC%LU%isInit)) THEN
      CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Matrix being used for LU Preconditioner is not initialized!')
    ELSE
      SELECT TYPE(LU => thisPC%LU)
      CLASS IS(DenseSquareMatrixType)
        ! LU Decomposition - No fill-in
        j=LU%n
        DO row=2,j
          DO col=1,row-1
            CALL LU%get(row,col,val1)
            IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) THEN
              CALL LU%get(col,col,val2)
              val2=val1/val2
              CALL LU%set(row,col,val2)
              DO col2=col+1,j
                CALL LU%get(col,col2,val3)
                IF(.NOT.(val3 .APPROXEQA. 0.0_SRK)) THEN
                  IF(col2 < row) THEN
                    CALL LU%get(row,col2,val1)
                    IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) CALL LU%set(row,col2,val1-val2*val3)
                  ELSE
                    CALL LU%get(row,col2,val1)
                    IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) CALL LU%set(row,col2,val1-val2*val3)
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      CLASS IS(SparseMatrixType)
        ! ILU Decomposition - No fill-in
        DO row=2,SIZE(LU%ia)-1
          DO col=LU%ia(row),LU%ia(row+1)-1
            IF(LU%ja(col) >= row) EXIT
            CALL LU%get(row,LU%ja(col),val1)
            IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) THEN
              CALL LU%get(LU%ja(col),LU%ja(col),val2)
              val2=val1/val2
              CALL LU%set(row,LU%ja(col),val2)
              DO col2=col+1,LU%ia(row+1)-1
                CALL LU%get(LU%ja(col),LU%ja(col2),val3)
                IF(.NOT.(val3 .APPROXEQA. 0.0_SRK)) THEN
                  CALL LU%get(row,LU%ja(col2),val1)
                  IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) CALL LU%set(row,LU%ja(col2),val1-val2*val3)
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ENDSELECT
    ENDIF
  ENDIF
ENDSUBROUTINE setup_ILU_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Intializes up the SOR Preconditioner Type with a parameter list
!> @param thisPC The preconditioner to act on
!> @param A The matrix to precondition
!> @param params The parameter list
!>
SUBROUTINE init_SOR_PreCondtype(thisPC,A,params)
  CHARACTER(LEN=*),PARAMETER :: myName='init_RSOR_PreCondType'
  CLASS(SOR_PrecondType),INTENT(INOUT) :: thisPC
  CLASS(MatrixType),ALLOCATABLE,TARGET,INTENT(IN) :: A
  TYPE(ParamType),INTENT(IN),OPTIONAL :: params
  TYPE(ParamType) :: PListMat_LU
  INTEGER(SIK) :: k

  !required statuses
  REQUIRE(.NOT. thisPC%isinit)
  REQUIRE(ALLOCATED(A))
  REQUIRE(A%isInit)

  thisPC%A => A

  !gets the number of blocks from the parameter list
  CALL params%get('PCType->numBlocks',thisPC%numBlocks)
  CALL params%get('PCType->omega',thisPC%omega)

  !makes sure that the number of blocks is valid
  REQUIRE(thisPC%numBlocks > 0)

  !makes sure that the number of blocks is valid
  REQUIRE(MOD(thisPC%A%n,thisPC%numBlocks) == 0)

  !makes sure that the value of omega is valid
  REQUIRE(thisPC%omega <= 2)
  REQUIRE(thisPC%omega >= 0)

  !calculate block size
  thisPC%blockSize=thisPC%A%n/thisPC%numBlocks

  !makes a lu matrix for each diagonal block in an array
  ALLOCATE(DenseSquareMatrixType :: thisPC%LU(thisPC%numBlocks))

  !initializes those matrices
  CALL PListMat_LU%add('MatrixType->n',thisPC%blockSize)
  CALL PListMat_LU%add('MatrixType->isSym',.FALSE.)
  DO k=1,thisPC%numBlocks
    CALL thisPC%LU(k)%init(PListMat_LU)
  ENDDO
  CALL PListMat_LU%clear()

  SELECT TYPE(mat => thisPC%A)
  TYPE IS(DenseSquareMatrixType)
    ALLOCATE(DenseSquareMatrixType :: thisPC%LpU)
    ! Assign A to LpU
    SELECT TYPE(LpU => thisPC%LpU); TYPE IS(DenseSquareMatrixType)
      LpU=mat
    ENDSELECT
    thisPC%isInit=.TRUE.

  TYPE IS(SparseMatrixType)
    ALLOCATE(SparseMatrixType :: thisPC%LpU)
    ! Assign A to LpU
    SELECT TYPE(LpU => thisPC%LpU); TYPE IS(SparseMatrixType)
      LpU=mat
    ENDSELECT
    thisPC%isInit=.TRUE.

  TYPE IS(BandedMatrixType)
    ALLOCATE(BandedMatrixType :: thisPC%LpU)
    ! Assign A to LpU
    SELECT TYPE(LpU => thisPC%LpU); TYPE IS(BandedMatrixType)
      LpU=mat
    ENDSELECT
    thisPC%isInit=.TRUE.

  CLASS DEFAULT
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - RSOR Preconditioners are not supported for input matrix type!')
  ENDSELECT
ENDSUBROUTINE init_SOR_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Clears up the SOR Preconditioner Type with a parameter list
!> @param thisPC The preconditioner to act on
!>
SUBROUTINE clear_SOR_PreCondtype(thisPC)
  CLASS(SOR_PrecondType),INTENT(INOUT) :: thisPC
  INTEGER(SIK) :: i

  IF(ASSOCIATED(thisPC%A)) NULLIFY(thisPC%A)
  IF(ALLOCATED(thisPC%LpU)) THEN
    CALL thisPC%LpU%clear()
    DEALLOCATE(thisPC%LpU)
  ENDIF
  IF(ALLOCATED(thisPC%LU)) THEN
    !gotta loop through, clear only works on a single matrix
    DO i=1,thisPC%numBlocks
      CALL thisPC%LU(i)%clear()
    ENDDO
    DEALLOCATE(thisPC%LU)
  ENDIF
  thisPC%isInit=.FALSE.
ENDSUBROUTINE clear_SOR_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Sets up the RSOR Preconditioner Type with a parameter list
!> @param thisPC The preconditioner to act on
!>
SUBROUTINE setup_RSOR_PreCondtype(thisPC)
  CLASS(RSOR_PrecondType),INTENT(INOUT) :: thisPC
  INTEGER(SIK) :: k,i,j
  REAL(SRK) :: tempreal

  !make sure everything is initialized and allocated
  REQUIRE(thisPC%isinit)

  ! make sure each LU block is initialized
  DO k=1,thisPC%numBlocks
    REQUIRE(thisPC%LU(k)%isInit)
  ENDDO

  !setup the Upper and Lower portion of the diagonal
  DO k=1,thisPC%numBlocks
    DO i=1,thisPC%blockSize
      DO j=1,thisPC%blockSize
        CALL thisPC%A%get((k-1)*thisPC%blockSize+i,(k-1)*thisPC%blockSize+j,tempreal)
        CALL thisPC%LU(k)%set(i,j,tempreal)
        IF(tempreal /= 0.0_SRK)THEN
          CALL thisPC%LpU%set((k-1)*thisPC%blockSize+i,(k-1)*thisPC%blockSize+j,0.0_SRK)
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  !do LU factorization on the diagonal blocks
  DO k=1,thisPC%numBlocks
    SELECT TYPE(mat => thisPC%LU(k))
    CLASS IS(DenseSquareMatrixType)
      CALL doolittle_LU_RSOR(mat)
    ENDSELECT
  ENDDO
ENDSUBROUTINE setup_RSOR_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Applies up the RSOR Preconditioner Type with a parameter list
!> @param thisPC The preconditioner to act on
!> @param v The vector to apply the preconditioner to to act on
!>
SUBROUTINE apply_RSOR_PreCondType(thisPC,v)
  CLASS(RSOR_PrecondType),INTENT(INOUT) :: thisPC
  CLASS(Vectortype),ALLOCATABLE,INTENT(INOUT) :: v
  CHARACTER(LEN=*),PARAMETER :: myName='apply_RSOR_PreCondType'
  TYPE(RealVectorType) :: w
  TYPE(ParamType) :: PListVec_RSOR
  INTEGER(SIK) :: k,lowIdx,highIdx

  REQUIRE(thisPC%isInit)
  REQUIRE(ALLOCATED(v))
  REQUIRE(v%isInit)

  CALL PListVec_RSOR%add('VectorType->n',thisPC%A%n)
  CALL w%init(PListVec_RSOR)
  CALL PListVec_RSOR%clear()

  SELECT TYPE(v)
  CLASS IS(RealVectorType)
    w%b = v%b
    DO k=1,thisPC%numBlocks
      SELECT TYPE(mat => thisPC%LU(k))
      CLASS IS(DenseSquareMatrixType)
        lowIdx = (k-1)*thisPC%blockSize + 1
        highIdx = lowIdx + thisPC%blockSize-1
        ! This is not a matvec call, but a call to the triangular solver
        ! dtrsv_all
        CALL BLAS_matvec('L','N','U',mat%A,w%b(lowIdx:highIdx))
        CALL BLAS_matvec('U','N','N',mat%A,w%b(lowIdx:highIdx))
      ENDSELECT
    ENDDO

    CALL BLAS_matvec(THISMATRIX=thisPC%LpU,X=w,Y=v,BETA=1.0_SRK,&
        ALPHA=-thisPC%omega)

    !solves the L and U problems for each block
    DO k=1,thisPC%numBlocks
      SELECT TYPE(mat => thisPC%LU(k))
      CLASS IS(DenseSquareMatrixType)
        lowIdx = (k-1)*thisPC%blockSize + 1
        highIdx = lowIdx + thisPC%blockSize-1
        CALL BLAS_matvec('L','N','U',mat%A,v%b(lowIdx:highIdx))
        CALL BLAS_matvec('U','N','N',mat%A,v%b(lowIdx:highIdx))
      ENDSELECT
    ENDDO

    CLASS DEFAULT
      CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Vector type is not support by this PreconditionerType.')
  ENDSELECT
ENDSUBROUTINE apply_RSOR_PreCondType

#ifdef HAVE_MPI
!
!-------------------------------------------------------------------------------
!> @brief Intializes up the Distributed SOR Preconditioner Type with a parameter
!> list
!> @param thisPC The preconditioner to act on
!> @param A The matrix to precondition
!> @param params The parameter list
!>
SUBROUTINE init_DistributedSOR_PreCondtype(thisPC,A,params)
  CHARACTER(LEN=*),PARAMETER :: myName='init_RSOR_PreCondType'
  CLASS(DistributedSOR_PrecondType),INTENT(INOUT) :: thisPC
  CLASS(MatrixType),ALLOCATABLE,TARGET,INTENT(IN) :: A
  TYPE(ParamType),INTENT(IN),OPTIONAL :: params
  TYPE(ParamType) :: PListMat_LU
  INTEGER(SIK) :: k,mpierr,rank

  REQUIRE(.NOT. thisPC%isinit)
  REQUIRE(ALLOCATED(A))
  REQUIRE(A%isInit)

  thisPC%A => A

  !get omega from the parameter list
  thisPC%omega = 0.0_SRK
  IF (params%has('PreCondType->omega')) &
    CALL params%get('PreCondType->omega',thisPC%omega)

  !this preconditioner will only support the distr. banded matrices
  SELECT TYPE(A)
  CLASS IS(DistributedBandedMatrixType)
    CALL MPI_Comm_Rank(A%comm,rank,mpierr)
    thisPC%blockSize = A%blockSize
    thisPC%nLocalBlocks = (A%iOffsets(rank+2) - A%iOffsets(rank+1))/A%blockSize
    thisPC%blockOffset = A%iOffsets(rank+1)/A%blockSize
    thisPC%comm = A%comm
  CLASS DEFAULT
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - RSOR Preconditioners are not supported for input matrix type!')
  ENDSELECT

  !makes sure that the value of omega is valid
  REQUIRE(thisPC%omega <= 2)
  REQUIRE(thisPC%omega >= 0)

  !makes a lu matrix for each diagonal block in an array
  ALLOCATE(DenseSquareMatrixType :: thisPC%LU(thisPC%nLocalBlocks))
  !initializes those matrices
  CALL PListMat_LU%add('MatrixType->n',thisPC%blockSize)
  CALL PListMat_LU%add('MatrixType->isSym',.FALSE.)
  DO k=1,thisPC%nLocalBlocks
    CALL thisPC%LU(k)%init(PListMat_LU)
  ENDDO
  CALL PListMat_LU%clear()

  SELECT TYPE(mat => thisPC%A)
  TYPE IS(DistributedBandedMatrixType)
    ALLOCATE(DistributedBandedMatrixType :: thisPC%LpU)
    ! Assign A to LpU
    SELECT TYPE(LpU => thisPC%LpU); TYPE IS(DistributedBandedMatrixType)
      LpU = mat
    ENDSELECT
    thisPC%isInit=.TRUE.

  TYPE IS(DistributedBlockBandedMatrixType)
    thisPC%LpU => mat
    ThisPC%isInit=.TRUE.
  ENDSELECT
ENDSUBROUTINE init_DistributedSOR_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Clears the Distributed SOR Preconditioner Type with a parameter list
!> @param thisPC The preconditioner to act on
!>
SUBROUTINE clear_DistributedSOR_PreCondtype(thisPC)
  CLASS(DistributedSOR_PrecondType),INTENT(INOUT) :: thisPC
  INTEGER(SIK) :: i

  IF(ASSOCIATED(thisPC%A)) NULLIFY(thisPC%A)
  IF(ASSOCIATED(thisPC%LpU)) THEN
    SELECT TYPE(LpU => thisPC%LpU)
    TYPE IS(DistributedBlockBandedMatrixType)
      NULLIFY(thisPC%LpU)
    CLASS DEFAULT
      CALL LpU%clear()
      DEALLOCATE(LpU)
    ENDSELECT
  ENDIF
  IF(ALLOCATED(thisPC%LU)) THEN
    !gotta loop through, clear only works on a single matrix
    DO i=1,thisPC%nLocalBlocks
      CALL thisPC%LU(i)%clear()
    ENDDO
    DEALLOCATE(thisPC%LU)
  ENDIF
  thisPC%isInit=.FALSE.
ENDSUBROUTINE clear_DistributedSOR_PreCondtype

!
!-------------------------------------------------------------------------------
!> @brief Sets up the Distributed RSOR Preconditioner Type with a parameter list
!> @param thisPC The preconditioner to act on
!>
SUBROUTINE setup_DistributedRSOR_PreCondtype(thisPC)
  CLASS(DistributedRSOR_PrecondType),INTENT(INOUT) :: thisPC
  INTEGER(SIK) :: k,i,j
  REAL(SRK) :: tempreal

  !make sure everything is initialized and allocated
  REQUIRE(thisPC%isinit)
  REQUIRE(ASSOCIATED(thisPC%LpU))

  ! make sure each LU block is initialized
  DO k=1,thisPC%nLocalBlocks
    REQUIRE(thisPC%LU(k)%isInit)
  ENDDO

  ! If the matrix A is block-banded we have the blocks readily available
  ! and can do LU factorization from these into the LU(:) containers
  ! Otherwise, we need to get them first, then do LU in place

  SELECT TYPE(A => thisPC%A)
  TYPE IS(DistributedBlockBandedMatrixType)
    !do LU factorization on the diagonal blocks
    DO k=1,thisPC%nLocalBlocks
      SELECT TYPE(LU => thisPC%LU(k)); TYPE IS(DenseSquareMatrixType)
        CALL doolittle_LU_RSOR(A%blocks(k),LU)
      ENDSELECT
    ENDDO
    ! blocks do not need to be set to zero because we can set the block mask
  CLASS DEFAULT
    !setup the Upper and Lower portion of the diagonal
    DO k=1,thisPC%nLocalBlocks
      DO i=1,thisPC%blockSize
        DO j=1,thisPC%blockSize
          CALL A%get((k+thisPC%blockOffset-1)*thisPC%blockSize+i, &
              (k+thisPC%blockOffset-1)*thisPC%blockSize+j,tempreal)
          CALL thisPC%LU(k)%set(i,j,tempreal)
          IF(tempreal /= 0.0_SRK)THEN
            CALL thispC%LpU%set((k+thisPC%blockOffset-1)*thisPC%blockSize+i, &
                (k+thisPC%blockOffset-1)*thisPC%blockSize+j,0.0_SRK)
          ENDIF
        ENDDO
      ENDDO
    ENDDO
    !do LU factorization on the diagonal blocks
    DO k=1,thisPC%nLocalBlocks
      SELECT TYPE(mat => thisPC%LU(k))
      CLASS IS(DenseSquareMatrixType)
        CALL doolittle_LU_RSOR(mat)
      ENDSELECT
    ENDDO
  ENDSELECT
ENDSUBROUTINE setup_DistributedRSOR_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Applies up the RSOR Preconditioner Type with a parameter list
!> @param thisPC The preconditioner to act on
!> @param v The vector to apply the preconditioner to to act on
!>
SUBROUTINE apply_DistributedRSOR_PreCondType(thisPC,v)
  CLASS(DistributedRSOR_PrecondType),INTENT(INOUT) :: thisPC
  CLASS(Vectortype),ALLOCATABLE,INTENT(INOUT) :: v
  CHARACTER(LEN=*),PARAMETER :: myName='apply_DistributedRSOR_PreCondType'
  TYPE(NativeDistributedVectorType) :: w
  TYPE(ParamType) :: PListVec_RSOR
  INTEGER(SIK) :: k,lowIdx,highIdx

  REQUIRE(thisPC%isInit)
  REQUIRE(ALLOCATED(v))
  REQUIRE(v%isInit)

  SELECT TYPE(v)
  CLASS IS(NativeDistributedVectorType)
    CALL PListVec_RSOR%add('VectorType->n',thisPC%A%n)
    CALL PListVec_RSOR%add('VectorType->chunkSize',thisPC%blockSize)
    CALL PListVec_RSOR%add('VectorType->MPI_Comm_ID',thisPC%comm)
    CALL PListVec_RSOR%add('VectorType->nlocal',SIZE(v%b))
    CALL w%init(PListVec_RSOR)
    CALL PListVec_RSOR%clear()
    w%b=v%b

    DO k=1,thisPC%nLocalBlocks
      SELECT TYPE(mat => thisPC%LU(k))
      CLASS IS(DenseSquareMatrixType)
        lowIdx = (k - 1)*thisPC%blockSize + LBOUND(w%b,1)
        highIdx = lowIdx + thisPC%blockSize-1
        ! This is not a matvec call, but a call to the triangular solver
        ! dtrsv_all
        CALL BLAS_matvec('L','N','U',mat%A,w%b(lowIdx:highIdx))
        CALL BLAS_matvec('U','N','N',mat%A,w%b(lowIdx:highIdx))
      ENDSELECT
    ENDDO

    SELECT TYPE(LpU => thisPC%LpU); TYPE IS(DistributedBlockBandedMatrixType)
      CALL LpU%setBlockMask(.TRUE.)
    ENDSELECT

    CALL BLAS_matvec(THISMATRIX=thisPC%LpU,X=w,Y=v,BETA=1.0_SRK,&
                      ALPHA=-thisPC%omega)

    SELECT TYPE(LpU => thisPC%LpU); TYPE IS(DistributedBlockBandedMatrixType)
      CALL LpU%setBlockMask(.FALSE.)
    ENDSELECT

    DO k=1,thisPC%nLocalBlocks
      SELECT TYPE(mat => thisPC%LU(k))
      CLASS IS(DenseSquareMatrixType)
        lowIdx = (k - 1)*thisPC%blockSize + LBOUND(w%b,1)
        highIdx = lowIdx + thisPC%blockSize-1
        CALL BLAS_matvec('L','N','U',mat%A,v%b(lowIdx:highIdx))
        CALL BLAS_matvec('U','N','N',mat%A,v%b(lowIdx:highIdx))
      ENDSELECT
    ENDDO

  CLASS DEFAULT
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
      ' - Vector type is not support by this PreconditionerType.')
  ENDSELECT
ENDSUBROUTINE apply_DistributedRSOR_PreCondType
#endif
!
!-------------------------------------------------------------------------------
!> @brief Does LU factorization on a matrix using the Doolittle Algorithm with
!> parameters
!> @param thisLU The matrix to perform the factorization on. On completion of
!> the factorization this matrix then holds both the L and U matrices. The L
!> matrix has 1s on the diagonal, so the diagonal of the matrix coming out has
!> the diagonals of of the U matrix
!> @param dest Optional parameter; if omitted, factorization is done in-place
!> if present, LU factorization is dumped there
!>
SUBROUTINE doolittle_LU_RSOR(thisLU,dest)
  CLASS(DenseSquareMatrixType),INTENT(INOUT) :: thisLU
  CLASS(DenseSquareMatrixType),OPTIONAL,INTENT(INOUT) :: dest
  INTEGER(SIK) :: k,i,j
  REAL(SRK) :: Ltemp(thisLU%n,thisLU%n),Utemp(thisLU%n,thisLU%n)

  !these need to be 0 at start since they accumulate
  Utemp(:,:)=0.0_SRK
  Ltemp(:,:)=0.0_SRK
  !this does the actual LU decomposition on each block
  DO i=1,thisLU%n
    DO j=1,thisLU%n
      CALL thisLU%get(i,j,Utemp(i,j))
      DO k=1,i-1
        Utemp(i,j)=Utemp(i,j)-Ltemp(i,k)*Utemp(k,j)
      ENDDO
    ENDDO
    DO j=i+1,thisLU%n
      CALL thisLU%get(j,i,Ltemp(j,i))
      DO k=1,i-1
        Ltemp(j,i)=Ltemp(j,i)-Ltemp(j,k)*Utemp(k,i)
      ENDDO
      Ltemp(j,i)=Ltemp(j,i)/Utemp(i,i)
    ENDDO
    Ltemp(i,i)=0
  ENDDO
  !set the block now to the new L in the lower and U in the upper
  !L is always 1 on diagonals, so set diagonals to U values
  DO i=1,thisLU%n
    DO j=i,thisLU%n
      IF (PRESENT(dest)) THEN
        CALL dest%set(i,j,Utemp(i,j))
      ELSE
        CALL thisLU%set(i,j,Utemp(i,j))
      ENDIF
    ENDDO
  ENDDO
  DO i=2,thisLU%n
    DO j=1,i-1
      IF (PRESENT(dest)) THEN
        CALL dest%set(i,j,Ltemp(i,j))
      ELSE
        CALL thisLU%set(i,j,Ltemp(i,j))
      ENDIF
    ENDDO
  ENDDO
ENDSUBROUTINE doolittle_LU_RSOR

!
!-------------------------------------------------------------------------------
!> @brief Initialize serial jacobi preconditioner
!> @param params The parameter list
!> @param thisPC The preconditioner to act on
!> @param A The matrix to precondition
SUBROUTINE init_Jacobi_PreCondType(thisPC,A,params)
  CHARACTER(LEN=*),PARAMETER :: myName='init_Jacobi_PreCondType'
  CLASS(Jacobi_PrecondType),INTENT(INOUT) :: thisPC
  CLASS(MatrixType),ALLOCATABLE,TARGET,INTENT(IN) :: A
  TYPE(ParamType),INTENT(IN),OPTIONAL :: params

  IF(thisPC%isinit) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - Preconditioner is already initialized!')
    RETURN
  ENDIF

  IF(.NOT.(ALLOCATED(A))) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - Matrix being used for LU Preconditioner is not allocated!')
    RETURN
  ENDIF
  IF(.NOT.(A%isInit)) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - Matrix being used for LU Preconditioner is not initialized!')
    RETURN
  ENDIF
  thisPC%A => A
  ALLOCATE(thisPC%invDiag(A%n))

  thisPC%isInit=.TRUE.

ENDSUBROUTINE init_Jacobi_PreCondType

#ifdef HAVE_MPI
!
!-------------------------------------------------------------------------------
!> @brief Initialize distr jacobi preconditioner
!> @param params The parameter list
!> @param thisPC The preconditioner to act on
!> @param A The matrix to precondition
SUBROUTINE init_DistributedJacobi_PreCondType(thisPC,A,params)
  CHARACTER(LEN=*),PARAMETER :: myName='init_DistributedJacobi_PreCondType'
  CLASS(DistributedJacobi_PrecondType),INTENT(INOUT) :: thisPC
  CLASS(MatrixType),ALLOCATABLE,TARGET,INTENT(IN) :: A
  TYPE(ParamType),INTENT(IN),OPTIONAL :: params
  INTEGER(SIK) :: rank,mpierr
  TYPE(ParamType) :: vecPL

  IF(thisPC%isinit) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - Preconditioner is already initialized!')
    RETURN
  ENDIF

  IF(.NOT.(ALLOCATED(A))) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - Matrix being used for LU Preconditioner is not allocated!')
    RETURN
  ENDIF
  IF(.NOT.(A%isInit)) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - Matrix being used for LU Preconditioner is not initialized!')
    RETURN
  ENDIF
  thisPC%A => A

  CALL params%get('PCType->MPI_Comm_ID',thisPC%comm)

  CALL vecPL%clear()
  CALL vecPL%add('VectorType->n',A%n)
  CALL vecPL%add('VectorType->MPI_Comm_ID',thisPC%comm)

  CALL MPI_Comm_Rank(thisPC%comm,rank,mpierr)
  SELECT TYPE(A); CLASS IS(DistributedBandedMatrixType)
    CALL vecPL%add('VectorType->chunkSize',A%blockSize)
    CALL thisPC%invDiag%init(vecPL)
    CALL vecPL%clear()
  ENDSELECT
  thisPC%isInit=.TRUE.

ENDSUBROUTINE init_DistributedJacobi_PreCondType
#endif
!
!-------------------------------------------------------------------------------
!> @brief Setup serial jacobi preconditioner
!> @param thisPC The preconditioner to act on
SUBROUTINE setup_Jacobi_PreCondType(thisPC)
  CLASS(Jacobi_PrecondType),INTENT(INOUT) :: thisPC
  INTEGER(SIK) :: i

  REQUIRE(thisPC%isInit)
  DO i=1,SIZE(thisPC%invDiag)
    CALL thisPC%A%get(i,i,thisPC%invDiag(i))
    thisPC%invDiag(i)=1.0_SRK/thisPC%invDiag(i)
  ENDDO
ENDSUBROUTINE setup_Jacobi_PreCondType

#ifdef HAVE_MPI
!
!-------------------------------------------------------------------------------
!> @brief Setup distr jacobi preconditioner
!> @param thisPC The preconditioner to act on
SUBROUTINE setup_DistributedJacobi_PreCondType(thisPC)
  CLASS(DistributedJacobi_PrecondType),INTENT(INOUT) :: thisPC
  INTEGER(SIK) :: i

  REQUIRE(thisPC%isInit)
  DO i=LBOUND(thisPC%invDiag%b,1),UBOUND(thisPC%invDiag%b,1)
    CALL thisPC%A%get(i,i,thisPC%invDiag%b(i))
    thisPC%invDiag%b(i)=1.0_SRK/thisPC%invDiag%b(i)
  ENDDO
ENDSUBROUTINE setup_DistributedJacobi_PreCondType
#endif
!
!-------------------------------------------------------------------------------
!> @brief Applies the Jacobi Preconditioner Type
!> @param thisPC The preconditioner to act on
!> @param v The matrix to apply the preconditioner to
SUBROUTINE apply_Jacobi_PreCondType(thisPC,v)
  CLASS(Jacobi_PrecondType),INTENT(INOUT) :: thisPC
  CLASS(Vectortype),ALLOCATABLE,INTENT(INOUT) :: v
  CHARACTER(LEN=*),PARAMETER :: myName='apply_Jacobi_PreCondType'

  IF(.NOT.(thisPC%isInit)) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - Preconditioner is not initialized.')
  ELSEIF(.NOT.(ALLOCATED(v))) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - VectorType is not allocated.')
  ELSEIF(.NOT.(v%isInit)) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - VectorType is not initialized.')
  ELSE
    SELECT TYPE(v)
    CLASS IS(RealVectorType)
      v%b(:) = thisPC%invDiag(:)*v%b(:)
    CLASS DEFAULT
      CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Vector type is not support by this PreconditionerType.')
    ENDSELECT
  ENDIF

ENDSUBROUTINE apply_Jacobi_PreCondType

#ifdef HAVE_MPI
!
!-------------------------------------------------------------------------------
!> @brief Applies the Jacobi Preconditioner Type
!> @param thisPC The preconditioner to act on
!> @param v The matrix to apply the preconditioner to
SUBROUTINE apply_DistributedJacobi_PreCondType(thisPC,v)
  CLASS(DistributedJacobi_PrecondType),INTENT(INOUT) :: thisPC
  CLASS(Vectortype),ALLOCATABLE,INTENT(INOUT) :: v
  CHARACTER(LEN=*),PARAMETER :: myName='apply_Jacobi_PreCondType'

  IF(.NOT.(thisPC%isInit)) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - Preconditioner is not initialized.')
  ELSEIF(.NOT.(ALLOCATED(v))) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - VectorType is not allocated.')
  ELSEIF(.NOT.(v%isInit)) THEN
    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
        ' - VectorType is not initialized.')
  ELSE
    SELECT TYPE(v)
    CLASS IS(NativeVectorType)
      v%b(:) = thisPC%invDiag%b(:)*v%b(:)
    CLASS DEFAULT
      CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Vector type is not support by this PreconditionerType.')
    ENDSELECT
  ENDIF

ENDSUBROUTINE apply_DistributedJacobi_PreCondType
#endif
!
!-------------------------------------------------------------------------------
!> @brief Clear serial jacobi preconditioner
!> @param thisPC The preconditioner to act on
SUBROUTINE clear_Jacobi_PreCondType(thisPC)
  CLASS(Jacobi_PrecondType),INTENT(INOUT) :: thisPC

  IF(ASSOCIATED(thisPC%A)) NULLIFY(thisPC%A)
  IF(ALLOCATED(thisPC%invDiag))  DEALLOCATE(thisPC%invDiag)

ENDSUBROUTINE clear_Jacobi_PreCondType

#ifdef HAVE_MPI
!
!-------------------------------------------------------------------------------
!> @brief Clear distr jacobi preconditioner
!> @param thisPC The preconditioner to act on
SUBROUTINE clear_DistributedJacobi_PreCondType(thisPC)
  CLASS(DistributedJacobi_PrecondType),INTENT(INOUT) :: thisPC

  IF(ASSOCIATED(thisPC%A)) NULLIFY(thisPC%A)
  CALL thisPC%invDiag%clear()
  thisPC%isInit = .FALSE.
  thisPC%comm = MPI_COMM_NULL

ENDSUBROUTINE clear_DistributedJacobi_PreCondType
#endif

!
!-------------------------------------------------------------------------------
!> @brief Returns the matrix multiplication where the diagonal matrix is on the left
!>
!> @params A is the full matrix
!> @params b is the diagonal of a matrix
!>
SUBROUTINE dmatmul_left(b,A,P)
  REAL(SRK),INTENT(IN) :: b(:)
  REAL(SRK),INTENT(IN) :: A(:,:)
  REAL(SRK),INTENT(INOUT) :: P(:,:)

  INTEGER(SIK) :: row,col,N

  N=SIZE(P(:,1))
  DO row=1,N
    DO col=1,N
      P(row,col)=A(row,col)*b(row)
    ENDDO
  ENDDO

ENDSUBROUTINE dmatmul_left
!
!-------------------------------------------------------------------------------
!> @brief Returns the matrix multiplication where the diagonal matrix is on the right
!>
!> @params A is the full matrix
!> @params b is the diagonal of a matrix
!>
SUBROUTINE dmatmul_right(A,b,P)
  REAL(SRK),INTENT(IN) :: A(:,:)
  REAL(SRK),INTENT(IN) :: b(:)
  REAL(SRK),INTENT(INOUT) :: P(:,:)

  INTEGER(SIK) :: row,col,N

  N=SIZE(P(:,1))
  DO row=1,N
    DO col=1,N
      P(row,col)=A(row,col)*b(col)
    ENDDO
  ENDDO

ENDSUBROUTINE dmatmul_right
!
#ifdef FUTILITY_HAVE_PETSC
SUBROUTINE PETSC_PCSHELL_SETUP_extern(pc,err)
  PC :: pc
  PetscErrorCode :: err

  err=0
  IF(ASSOCIATED(PETSC_PCSHELL_PC) .AND. PETSC_PCSHELL_PC%isInit) CALL PETSC_PCSHELL_PC%setup()

ENDSUBROUTINE PETSC_PCSHELL_SETUP_extern

SUBROUTINE PETSC_PCSHELL_APPLY_extern(pc,xin,xout,err)
  PC :: pc
  Vec :: xin
  Vec :: xout
  PetscErrorCode :: err

  PetscErrorCode :: ierr
  INTEGER(SIK) :: n
  TYPE(PETScVectorType) :: v

  err=-1
  IF(ASSOCIATED(PETSC_PCSHELL_PC) .AND. PETSC_PCSHELL_PC%isInit) THEN
    n=0
    CALL VecGetSize(xin,n,ierr)
    IF(ierr==0) CALL VecCopy(xin,xout,ierr)

    v%isInit=.TRUE.
    v%n=n
    v%b=xout

    err=ierr
    IF(err==0) CALL PETSC_PCSHELL_PC%apply(v)
  ENDIF

ENDSUBROUTINE PETSC_PCSHELL_APPLY_extern
#endif
!
ENDMODULE PreconditionerTypes
