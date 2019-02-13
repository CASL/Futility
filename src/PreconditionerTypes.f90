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
!>
!> @par EXAMPLES
!> @code
!>
!> @endcode
!>
!> @author Aaron Graham, Andrew Gerlach
!>   @date 11/01/2013
!>
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

#ifdef HAVE_MPI
#include <mpif.h>
#endif

  PRIVATE

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
#undef IS
#endif

  !
  ! List of Public members
  PUBLIC :: PreconditionerType
  PUBLIC :: DistributedPrecond
  PUBLIC :: LU_PreCondType
  PUBLIC :: ILU_PreCondType
  PUBLIC :: SOR_PreCondType
  PUBLIC :: RSOR_PreCondType
  PUBLIC :: DistributedSOR_PreCondType
  PUBLIC :: DistributedRSOR_PreCondType
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

  !> @brief The extended type for distributed SOR based preconditioners
  TYPE,ABSTRACT,EXTENDS(DistributedPrecond) :: DistributedSOR_PreCondType
    !> Size of the diagonal blocks
    INTEGER(SIK) :: blockSize
    !> Number of diagonal blocks in matrix
    INTEGER(SIK) :: numBlocks
    !> Number of blocks assigned to a processor
    INTEGER(SIK) :: myNumBlocks
    !> First block assigned to a processor
    INTEGER(SIK) :: myFirstBlock
    !> Omega factor for sor
    REAL(SRK) :: omega
    !> Array of LU matrices for each diagonal block, will be dense!
    CLASS(MatrixType),ALLOCATABLE :: LU(:)
    !> Lower and upper portions of matrix with diagonal blocks removed
    CLASS(MatrixType),ALLOCATABLE :: LpU
    !> Number of elements belonging to each processor
    INTEGER(SIK),ALLOCATABLE :: psize(:)
    !> Offset for elements belonging to each processor
    INTEGER(SIK),ALLOCATABLE :: pdispl(:)
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
!
!List of Abstract Interfaces
  !> Explicitly defines the interface for the init routine of all preconditioner types
  !> with parameter list
  ABSTRACT INTERFACE
    SUBROUTINE precond_init_absintfc(thisPC,A,params)
        !notice you need to import all necessary types for abstract interfaces
      IMPORT :: PreconditionerType,Matrixtype,ParamType
      CLASS(PreconditionerType),INTENT(INOUT) :: thisPC
      CLASS(MatrixType),TARGET,INTENT(IN),OPTIONAL :: A
      ! paramtype is a custom type that can take any intrinsic variable type
      !including multiple variables
      TYPE(ParamType),INTENT(IN),OPTIONAL :: params
    ENDSUBROUTINE precond_init_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the apply routine of all preconditioner types
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

    SUBROUTINE precond_applyDistributedSOR_absintfc(thisPC,v)
      IMPORT :: DistributedSOR_PreCondType,VectorType
      CLASS(DistributedSOR_PreCondType),INTENT(INOUT) :: thisPC
      CLASS(VectorType),INTENT(INOUT) :: v
    ENDSUBROUTINE precond_applyDistributedSOR_absintfc
  ENDINTERFACE
  
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

    SUBROUTINE precond_DistributedSOR_absintfc(thisPC)
      IMPORT :: DistributedSOR_PreCondType
      CLASS(DistributedSOR_PreCondType),INTENT(INOUT) :: thisPC
    ENDSUBROUTINE precond_DistributedSOR_absintfc
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
      CLASS(MatrixType),ALLOCATABLE,TARGET,INTENT(IN),OPTIONAL :: A
      TYPE(ParamType),INTENT(IN),OPTIONAL :: params

      IF(thisPC%isinit) THEN
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Preconditioner is already initialized!')
        RETURN
      ENDIF
      
      IF(.NOT. PRESENT(A) .OR. .NOT.(ALLOCATED(A))) THEN
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
      SELECTTYPE(mat => thisPC%A)
        TYPE IS(DenseSquareMatrixType)
          ALLOCATE(DenseSquareMatrixType :: thisPC%LU)

          ! Assign A to LU
          SELECTTYPE(LU => thisPC%LU); TYPE IS(DenseSquareMatrixType)
            LU=mat
          ENDSELECT

          IF(thisPC%LU%isInit) THEN
            thisPC%isInit=.TRUE.
          ELSE
            CALL ePreCondtype%raiseError('Incorrect input to '//modName//'::'//myName// &
              ' - In LU Preconditioner initialization, LU was not properly initialized!')
          ENDIF
        TYPE IS(SparseMatrixType)
          SELECTTYPE(thisPC)
            TYPE IS(ILU_PrecondType)
              ALLOCATE(SparseMatrixType :: thisPC%LU)
              ! Assign A to LU
              SELECTTYPE(LU => thisPC%LU); TYPE IS(SparseMatrixType)
                LU=mat
              ENDSELECT

              IF(thisPC%LU%isInit) THEN
                thisPC%isInit=.TRUE.
              ELSE
                CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                  ' = In LU Preconditioner initialization, LU was not properly initialized!')
              ENDIF
          ENDSELECT
          ! This doesnt appear to work. It initializes L and U, which never get
          ! used
        ! CLASS IS(PETScMatrixType)
        !   !allocate L and U
        !   ALLOCATE(PETScMatrixType :: thisPC%L)
        !   ALLOCATE(PETScMatrixType :: thisPC%U)
        !
        !   !initialize L and U
        !   SELECTTYPE(U => thisPC%U); TYPE IS(PETScMatrixType)
        !     SELECTTYPE(L => thisPC%L); TYPE IS(PETScMatrixType)
        !       ! Initialize L and U (add preallocation eventually)
        !       CALL PL%add('MatrixType->matType',SPARSE)
        !       CALL PL%add('MatrixType->n',mat%n)
        !       CALL PL%add('MatrixType->isSym',mat%isSymmetric)
        !       CALL PL%add('MatrixType->MPI_Comm_ID',mat%comm)
        !       CALL U%init(PL)
        !       CALL L%init(PL)
        !       CALL PL%clear()
        !     ENDSELECT
        !   ENDSELECT
        !
        !   thisPC%isInit=.TRUE.
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
          SELECTTYPE(v)
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
          SELECTTYPE(LU => thisPC%LU)
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
      CLASS(MatrixType),ALLOCATABLE,TARGET,INTENT(IN),OPTIONAL :: A
      TYPE(ParamType),INTENT(IN),OPTIONAL :: params
      TYPE(ParamType)::PListMat_LU
      INTEGER(SIK)::k
      
      !required statuses
      REQUIRE(.NOT. thisPC%isinit)
      REQUIRE(PRESENT(A))
      REQUIRE(ALLOCATED(A))
      REQUIRE(A%isInit)
      
      thisPC%A => A
      
      !gets the number of blocks from the parameter list
      CALL params%get('PCType->numBlocks',thisPC%numBlocks)
      CALL params%get('PCType->omega',thisPC%omega)
      
      !makes sure that the number of blocks is valid
      REQUIRE(thisPC%numBlocks .GT. 0)
      
      !makes sure that the number of blocks is valid
      REQUIRE(MOD(thisPC%A%n,thisPC%numBlocks) .EQ. 0)
      
      !makes sure that the value of omega is valid
      REQUIRE(thisPC%omega .LE. 2)
      REQUIRE(thisPC%omega .GE. 0)
      
      !calculate block size
      thisPC%blockSize=thisPC%A%n/thisPC%numBlocks
      
      !makes a lu matrix for each diagonal block in an array
      ALLOCATE(DenseSquareMatrixType :: thisPC%LU(thisPC%numBlocks))
      
      !initializes those matrices
      CALL PListMat_LU%add('MatrixType->n',thisPC%blockSize)
      CALL PListMat_LU%add('MatrixType->isSym',.FALSE.)
      DO k=1,thisPC%numBlocks
        CALL thisPC%LU(k)%init(PListMat_LU)
      END DO
      
      SELECTTYPE(mat => thisPC%A)
        TYPE IS(DenseSquareMatrixType)
          ALLOCATE(DenseSquareMatrixType :: thisPC%LpU)
          ! Assign A to LpU
          SELECTTYPE(LpU => thisPC%LpU); TYPE IS(DenseSquareMatrixType)
            LpU=mat
          ENDSELECT
          REQUIRE(thisPC%LpU%isInit)
          thisPC%isInit=.TRUE.
            
        TYPE IS(SparseMatrixType)
            ALLOCATE(SparseMatrixType :: thisPC%LpU)
            ! Assign A to LpU
            SELECTTYPE(LpU => thisPC%LpU); TYPE IS(SparseMatrixType)
                LpU=mat
            ENDSELECT
            REQUIRE(thisPC%LpU%isInit)
            thisPC%isInit=.TRUE.
            
        TYPE IS(BandedMatrixType)
            ALLOCATE(BandedMatrixType :: thisPC%LpU)
            ! Assign A to LpU
            SELECTTYPE(LpU => thisPC%LpU); TYPE IS(BandedMatrixType)
                LpU=mat
            ENDSELECT
            REQUIRE(thisPC%LpU%isInit)
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
      INTEGER(SIK)::i

      IF(ASSOCIATED(thisPC%A)) NULLIFY(thisPC%A)
      IF(ALLOCATED(thisPC%LpU)) THEN
        CALL thisPC%LpU%clear()
        DEALLOCATE(thisPC%LpU)
      ENDIF
      IF(ALLOCATED(thisPC%LU)) THEN
        !gotta loop through, clear only works on a single matrix
        DO i=1,thisPC%numBlocks
            CALL thisPC%LU(i)%clear()
        END DO
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
      CHARACTER(LEN=*),PARAMETER :: myName='setup_RSOR_PreCondType'
      INTEGER(SIK)::k,i,j
      REAL(SRK)::tempreal

      !make sure everything is initialized and allocated
      REQUIRE(thisPC%isinit)
      REQUIRE(ALLOCATED(thisPC%LpU))
      REQUIRE(thisPC%LpU%isInit)
      
      ! make sure each LU block is initialized
      DO k=1,thisPC%numBlocks
        REQUIRE(thisPC%LU(k)%isInit)
      END DO
      
      !setup the Upper and Lower portion of the diagonal 
      DO k=1,thisPC%numBlocks
          DO i=1,thisPC%blockSize
              DO j=1,thisPC%blockSize
                  CALL thisPC%A%get((k-1)*thisPC%blockSize+i,(k-1)*thisPC%blockSize+j,tempreal)
                  CALL thisPC%LU(k)%set(i,j,tempreal)
                  IF(tempreal .NE. 0.0_SRK)THEN
                      CALL thisPC%LpU%set((k-1)*thisPC%blockSize+i,(k-1)*thisPC%blockSize+j,0.0_SRK)
                  END IF
              END DO
          END DO
      END DO
      !do LU factorization on the diagonal blocks
      DO k=1,thisPC%numBlocks
        SELECTTYPE(mat => thisPC%LU(k))
          CLASS IS(DenseSquareMatrixType)
            CALL doolittle_LU_RSOR(mat)
        ENDSELECT
      END DO
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
      TYPE(RealVectorType)::w(4),tempw
      TYPE(ParamType)::PListVec_RSOR
      INTEGER(SIK)::k,i
      REAL(SRK)::tmpreal

      REQUIRE(thisPC%isInit)
      REQUIRE(ALLOCATED(v))
      REQUIRE(v%isInit)
      
      CALL PListVec_RSOR%add('VectorType->n',thisPC%A%n)
      CALL PListVec_RSOR%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL w(1)%init(PListVec_RSOR)
      CALL w(2)%init(PListVec_RSOR)
      CALL w(3)%init(PListVec_RSOR)
      CALL w(4)%init(PListVec_RSOR)
      CALL tempw%init(PListVec_RSOR)
      SELECTTYPE(v)
        CLASS IS(RealVectorType)
            w(3)%b=v%b
            
            !solves the L and U problems for each block
            DO k=1,thisPC%numBlocks
              SELECTTYPE(mat => thisPC%LU(k))
                CLASS IS(DenseSquareMatrixType)
                  CALL RSORsolveL(mat,v,w(1),k)
                  CALL RSORsolveU(mat,w(1),w(2),k)
              ENDSELECT
            END DO
            
            !multiply
            SELECTTYPE(LpU => thisPC%LpU)
                CLASS IS(BandedMatrixType)
                    CALL LpU%matvec(w(2)%b,tempw%b)
                    w(3)%b=w(3)%b-thisPC%omega*tempw%b
                CLASS DEFAULT
                    CALL BLAS_matvec(THISMATRIX=LpU,X=w(2),Y=w(3),&
                        &BETA=1.0_SRK,TRANS='N',ALPHA=-thisPC%omega)
            ENDSELECT
            
            
            !solves the L and U problems for each block
            DO k=1,thisPC%numBlocks
              SELECTTYPE(mat => thisPC%LU(k))
                CLASS IS(DenseSquareMatrixType)
                  CALL RSORsolveL(mat,w(3),w(4),k)
                  CALL RSORsolveU(mat,w(4),v,k)
              ENDSELECT
            END DO
            
        CLASS DEFAULT
          CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
            ' - Vector type is not support by this PreconditionerType.')
      ENDSELECT
    ENDSUBROUTINE apply_RSOR_PreCondType
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
      CLASS(MatrixType),ALLOCATABLE,TARGET,INTENT(IN),OPTIONAL :: A
      TYPE(ParamType),INTENT(IN),OPTIONAL :: params
      TYPE(ParamType)::PListMat_LU
      INTEGER(SIK)::k,mpierr,rank,nproc,extrablocks,stdblocks,i
#ifdef HAVE_MPI
      
      REQUIRE(.NOT. thisPC%isinit)
      REQUIRE(PRESENT(A))
      REQUIRE(ALLOCATED(A))
      REQUIRE(A%isInit)
      
      thisPC%A => A
      
      !gets the number of blocks from the parameter list
      CALL params%get('PCType->numBlocks',thisPC%numBlocks)
      CALL params%get('PCType->omega',thisPC%omega)
      CALL params%get('PCType->comm',thisPC%comm)
      
      
      !makes sure that the number of blocks is valid
      REQUIRE(thisPC%numBlocks .GT. 0)
      REQUIRE(MOD(thisPC%A%n,thisPC%numBlocks) .EQ. 0)
      
      !makes sure that the value of omega is valid
      REQUIRE(thisPC%omega .LE. 2)
      REQUIRE(thisPC%omega .GE. 0)
      
      !calculate block size
      thisPC%blockSize=thisPC%A%n/thisPC%numBlocks
      
      !calculate how many blocks this processor gets and which ones
      CALL MPI_Comm_rank(thisPC%comm,rank,mpierr)
      CALL MPI_Comm_size(thisPC%comm,nproc,mpierr)
      ALLOCATE(thisPC%psize(nproc),thisPC%pdispl(nproc))
      thisPC%myNumBlocks=INT(thisPC%numBlocks/nproc)
      stdblocks=INT(thisPC%numBlocks/nproc)
      extrablocks=MOD(thisPC%numBlocks,nproc)
      IF(rank+1 .LE. extrablocks)thisPC%myNumBlocks=thisPC%myNumBlocks+1
      thisPC%myFirstBlock=thisPC%myNumBlocks*rank+1
      IF(rank+1 .GT. extrablocks)thisPC%myFirstBlock=thisPC%myFirstBlock+extrablocks
      
      DO i=1,extrablocks
        thisPC%psize(i)=(stdblocks+1)*thisPC%blockSize
        thisPC%pdispl(i)=(i-1)*thisPC%psize(i)
      END DO
      
      DO i=extrablocks+1,nproc
        thisPC%psize(i)=stdblocks*thisPC%blockSize
        thisPC%pdispl(i)=(i-1)*thisPC%psize(i)+extrablocks*thisPC%blockSize
      END DO
      !makes a lu matrix for each diagonal block in an array
      ALLOCATE(DenseSquareMatrixType :: thisPC%LU(thisPC%myNumBlocks))
      !initializes those matrices
      CALL PListMat_LU%add('MatrixType->n',thisPC%blockSize)
      CALL PListMat_LU%add('MatrixType->isSym',.FALSE.)
      DO k=1,thisPC%myNumBlocks
        CALL thisPC%LU(k)%init(PListMat_LU)
      END DO
      
      SELECTTYPE(mat => thisPC%A)
        TYPE IS(DistributedBandedMatrixType)
          ALLOCATE(DistributedBandedMatrixType :: thisPC%LpU)
          ! Assign A to LpU
          SELECTTYPE(LpU => thisPC%LpU); TYPE IS(DistributedBandedMatrixType)
              LpU=mat
          ENDSELECT
          REQUIRE(thisPC%LpU%isInit)
          thisPC%isInit=.TRUE.
          
        CLASS DEFAULT
          CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
            ' - RSOR Preconditioners are not supported for input matrix type!')
      ENDSELECT
#endif
    ENDSUBROUTINE init_DistributedSOR_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Clears the Distributed SOR Preconditioner Type with a parameter list
!> @param thisPC The preconditioner to act on
!>
    SUBROUTINE clear_DistributedSOR_PreCondtype(thisPC)
      CLASS(DistributedSOR_PrecondType),INTENT(INOUT) :: thisPC
      INTEGER(SIK)::i
#ifdef HAVE_MPI

      IF(ASSOCIATED(thisPC%A)) NULLIFY(thisPC%A)
      IF(ALLOCATED(thisPC%LpU)) THEN
        CALL thisPC%LpU%clear()
        DEALLOCATE(thisPC%LpU)
      ENDIF
      IF(ALLOCATED(thisPC%LU)) THEN
        !gotta loop through, clear only works on a single matrix
        DO i=1,thisPC%myNumBlocks
            CALL thisPC%LU(i)%clear()
        END DO
        DEALLOCATE(thisPC%LU)
      ENDIF
      thisPC%isInit=.FALSE.
#endif
    ENDSUBROUTINE clear_DistributedSOR_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Sets up the Distributed RSOR Preconditioner Type with a parameter list
!> @param thisPC The preconditioner to act on
!>
    SUBROUTINE setup_DistributedRSOR_PreCondtype(thisPC)
      CLASS(DistributedRSOR_PrecondType),INTENT(INOUT) :: thisPC
      CHARACTER(LEN=*),PARAMETER :: myName='setup_RSOR_PreCondType'
      INTEGER(SIK)::k,i,j
      REAL(SRK)::tempreal

      !make sure everything is initialized and allocated
      REQUIRE(thisPC%isinit)
      REQUIRE(ALLOCATED(thisPC%LpU))
      REQUIRE(thisPC%LpU%isInit)
      
      ! make sure each LU block is initialized
      DO k=1,thisPC%myNumBlocks
        REQUIRE(thisPC%LU(k)%isInit)
      END DO
      
      !setup the Upper and Lower portion of the diagonal 
      DO k=1,thisPC%numBlocks
        IF(k .GE. thisPC%myFirstBlock .AND. k .LE. thisPC%myFirstBlock+thisPC%myNumBlocks-1)THEN
          DO i=1,thisPC%blockSize
              DO j=1,thisPC%blockSize
                  CALL thisPC%A%get((k-1)*thisPC%blockSize+i,(k-1)*thisPC%blockSize+j,tempreal)
                  CALL thisPC%LU(k-thisPC%myFirstBlock+1)%set(i,j,tempreal)
                  IF(tempreal .NE. 0.0_SRK)THEN
                      CALL thisPC%LpU%set((k-1)*thisPC%blockSize+i,(k-1)*thisPC%blockSize+j,0.0_SRK)
                  END IF
              END DO
          END DO
        ELSE
          DO i=1,thisPC%blockSize
              DO j=1,thisPC%blockSize
                  CALL thisPC%A%get((k-1)*thisPC%blockSize+i,(k-1)*thisPC%blockSize+j,tempreal)
                  IF(tempreal .NE. 0.0_SRK)THEN
                      CALL thisPC%LpU%set((k-1)*thisPC%blockSize+i,(k-1)*thisPC%blockSize+j,0.0_SRK)
                  END IF
              END DO
          END DO
        END IF
      END DO
      !do LU factorization on the diagonal blocks
      DO k=1,thisPC%myNumBlocks
        SELECTTYPE(mat => thisPC%LU(k))
          CLASS IS(DenseSquareMatrixType)
            CALL doolittle_LU_RSOR(mat)
        ENDSELECT
      END DO
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
      CHARACTER(LEN=*),PARAMETER :: myName='apply_RSOR_PreCondType'
      TYPE(RealVectorType)::w(4),tempw
      TYPE(ParamType)::PListVec_RSOR
      INTEGER(SIK)::k,i,mpierr
      REAL(SRK)::tmpreal
      REAL(SRK)::tmpreal1,tmpreal2
#ifdef HAVE_MPI

      REQUIRE(thisPC%isInit)
      REQUIRE(ALLOCATED(v))
      REQUIRE(v%isInit)
      
      CALL PListVec_RSOR%add('VectorType->n',thisPC%A%n)
      CALL PListVec_RSOR%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
      CALL w(1)%init(PListVec_RSOR)
      CALL w(2)%init(PListVec_RSOR)
      CALL w(3)%init(PListVec_RSOR)
      CALL w(4)%init(PListVec_RSOR)
      CALL tempw%init(PListVec_RSOR)
      SELECTTYPE(v)
        CLASS IS(RealVectorType)
            w(3)%b=v%b
            
            DO k=thisPC%myFirstBlock,thisPC%myFirstBlock+thisPC%myNumBlocks-1
              SELECTTYPE(mat => thisPC%LU(k-thisPC%myFirstBlock+1))
                CLASS IS(DenseSquareMatrixType)
                  CALL RSORsolveL(mat,v,w(1),k)
                  CALL RSORsolveU(mat,w(1),w(2),k)
              ENDSELECT
            END DO
            
            CALL MPI_Allgatherv(MPI_IN_PLACE,thisPC%myNumBlocks*thisPC%blockSize&
              ,MPI_DOUBLE_PRECISION,w(2)%b(1),thisPC%psize,thisPC%pdispl,MPI_DOUBLE_PRECISION,&
              thisPC%comm,mpierr)
              
            REQUIRE(mpierr .EQ. 0)
            
            SELECTTYPE(LpU => thisPC%LpU)
                CLASS IS(DistributedBandedMatrixType)
                    CALL LpU%matvec(w(2)%b,tempw%b)
                    w(3)%b=w(3)%b-thisPC%omega*tempw%b
                CLASS DEFAULT
                    CALL BLAS_matvec(THISMATRIX=LpU,X=w(2),Y=w(3),&
                        &BETA=1.0_SRK,TRANS='N',ALPHA=-thisPC%omega)
            ENDSELECT
            
            DO k=thisPC%myFirstBlock,thisPC%myFirstBlock+thisPC%myNumBlocks-1
              SELECTTYPE(mat => thisPC%LU(k-thisPC%myFirstBlock+1))
                CLASS IS(DenseSquareMatrixType)
                  CALL RSORsolveL(mat,w(3),w(4),k)
                  CALL RSORsolveU(mat,w(4),v,k)
              ENDSELECT
            END DO
            
            CALL MPI_Allgatherv(MPI_IN_PLACE,thisPC%myNumBlocks*thisPC%blockSize&
              ,MPI_DOUBLE_PRECISION,v%b(1),thisPC%psize,thisPC%pdispl,MPI_DOUBLE_PRECISION,&
              thisPC%comm,mpierr)
              
            REQUIRE(mpierr .EQ. 0)
            
        CLASS DEFAULT
          CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
            ' - Vector type is not support by this PreconditionerType.')
      ENDSELECT
#endif
    ENDSUBROUTINE apply_DistributedRSOR_PreCondType
!
!-------------------------------------------------------------------------------
!> @brief Does LU factorization on a matrix using the Doolittle Algorithm with 
!> parameters
!> @param thisLU The matrix to perform the factorization on. On completion of 
!> the factorization this matrix then holds both the L and U matrices. The L 
!> matrix has 1s on the diagonal, so the diagonal of the matrix coming out has
!> the diagonals of of the U matrix
!>
    SUBROUTINE doolittle_LU_RSOR(thisLU)
      CLASS(DenseSquareMatrixType),INTENT(INOUT) :: thisLU
      CHARACTER(LEN=*),PARAMETER :: myName='doolittle_LU_RSOR'
      INTEGER(SIK)::k,i,j,l
      REAL(SRK)::Ltemp(thisLU%n,thisLU%n),Utemp(thisLU%n,thisLU%n)
      
      !these need to be 0 at start since they accumulate
      Utemp(:,:)=0
      Ltemp(:,:)=0
      !this does the actual LU decomposition on each block
      DO i=1,thisLU%n
        DO j=1,thisLU%n
            CALL thisLU%get(i,j,Utemp(i,j))
            DO k=1,i-1
                Utemp(i,j)=Utemp(i,j)-Ltemp(i,k)*Utemp(k,j)
            END DO
        END DO
        DO j=i+1,thisLU%n
            CALL thisLU%get(j,i,Ltemp(j,i))
            DO k=1,i-1
                Ltemp(j,i)=Ltemp(j,i)-Ltemp(j,k)*Utemp(k,i)
            END DO
            Ltemp(j,i)=Ltemp(j,i)/Utemp(i,i)
        END DO
        Ltemp(i,i)=0
      END DO
      !set the block now to the new L in the lower and U in the upper
      !L is always 1 on diagonals, so set diagonals to U values
      DO i=1,thisLU%n
        DO j=i,thisLU%n
            CALL thisLU%set(i,j,Utemp(i,j))
        END DO
      END DO
      DO i=2,thisLU%n
        DO j=1,i-1
            CALL thisLU%set(i,j,Ltemp(i,j))
        END DO
      END DO
    ENDSUBROUTINE doolittle_LU_RSOR
!
!-------------------------------------------------------------------------------
!> @brief Solve a problem of Lx=b with parameters
!> @param thisLU The L matrix.
!> @param b The right side of the system
!> @param x The vector being solved for 
!> @param k The offset for the portion of the vector being solved for
!>
    SUBROUTINE RSORsolveL(thisLU,b,x,k)
      CLASS(DenseSquareMatrixType),INTENT(INOUT) :: thisLU
      CHARACTER(LEN=*),PARAMETER :: myName='RSORsolveL'
      TYPE(RealVectorType),INTENT(INOUT)::b
      TYPE(RealVectorType),INTENT(INOUT)::x
      INTEGER(SIK),INTENT(IN)::k
      INTEGER(SIK)::i,j
      REAL(SRK)::tempreal(3)
      
      CALL x%setrange_scalar((k-1)*thisLU%n+1,k*thisLU%n,0.0_SRK)
      DO i=1,thisLU%n
          CALL b%getone((k-1)*thisLU%n+i,tempreal(1))
          CALL x%setone((k-1)*thisLU%n+i,tempreal(1))
          DO j=1,i-1
              CALL x%getone((k-1)*thisLU%n+i,tempreal(1))
              CALL x%getone((k-1)*thisLU%n+j,tempreal(2))
              CALL thisLU%get(i,j,tempreal(3))
              CALL x%setone((k-1)*thisLU%n+i,tempreal(1)-tempreal(2)*tempreal(3))
          END DO
      END DO
      
    ENDSUBROUTINE RSORsolveL
!
!-------------------------------------------------------------------------------
!> @brief Solve a problem of Ux=b with parameters
!> @param thisLU The U matrix.
!> @param b The right side of the system
!> @param x The vector being solved for
!> @param k The offset for the portion of the vector being solved for
!>
    SUBROUTINE RSORsolveU(thisLU,b,x,k)
      CLASS(DenseSquareMatrixType),INTENT(INOUT) :: thisLU
      CHARACTER(LEN=*),PARAMETER :: myName='RSORsolveU'
      TYPE(RealVectorType),INTENT(INOUT)::b
      TYPE(RealVectorType),INTENT(INOUT)::x
      INTEGER(SIK),INTENT(IN)::k
      INTEGER(SIK)::i,j
      REAL(SRK)::tempreal(3)
      
      CALL x%setrange_scalar((k-1)*thisLU%n+1,k*thisLU%n,0.0_SRK)
      DO i=thisLU%n,1,-1
          CALL b%getone((k-1)*thisLU%n+i,tempreal(1))
          CALL x%setone((k-1)*thisLU%n+i,tempreal(1))
          DO j=thisLU%n,i+1,-1
              CALL x%getone((k-1)*thisLU%n+i,tempreal(1))
              CALL x%getone((k-1)*thisLU%n+j,tempreal(2))
              CALL thisLU%get(i,j,tempreal(3))
              CALL x%setone((k-1)*thisLU%n+i,tempreal(1)-tempreal(2)*tempreal(3))
          END DO
          CALL x%getone((k-1)*thisLU%n+i,tempreal(1))
          CALL thisLU%get(i,i,tempreal(2))
          CALL x%setone((k-1)*thisLU%n+i,tempreal(1)/tempreal(2))
      END DO
        
    ENDSUBROUTINE RSORsolveU
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
