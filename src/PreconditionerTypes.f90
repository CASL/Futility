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
#endif

  !
  ! List of Public members
  PUBLIC :: PreconditionerType
  PUBLIC :: LU_PreCondType
  PUBLIC :: ILU_PreCondType
  PUBLIC :: SOR_PreCondType
  PUBLIC :: RSOR_PreCondType
  PUBLIC :: ePreCondType

#ifdef FUTILITY_HAVE_PETSC
  PUBLIC :: PETSC_PCSHELL_SETUP_extern
  PUBLIC :: PETSC_PCSHELL_APPLY_extern
#endif

  PUBLIC :: PETSC_PCSHELL_PC

  TYPE,ABSTRACT :: PreConditionerType
    LOGICAL(SBK) :: isInit=.FALSE.
    CLASS(MatrixType),POINTER :: A=>NULL()

    CONTAINS
      PROCEDURE(precond_init_absintfc),DEFERRED,PASS :: init
      PROCEDURE(precond_absintfc),DEFERRED,PASS :: clear
      PROCEDURE(precond_absintfc),DEFERRED,PASS :: setup
      PROCEDURE(precond_apply_absintfc),DEFERRED,PASS :: apply
  ENDTYPE PreConditionerType

  TYPE,ABSTRACT,EXTENDS(PreConditionerType) :: LU_PreCondType
    CLASS(MatrixType),ALLOCATABLE :: L
    CLASS(MatrixType),ALLOCATABLE :: U
    CLASS(MatrixType),ALLOCATABLE :: LU

    CONTAINS
      PROCEDURE,PASS :: init => init_LU_PreCondType
      PROCEDURE,PASS :: clear => clear_LU_PreCondType
      PROCEDURE(precond_LU_absintfc),DEFERRED,PASS :: setup
      PROCEDURE(precond_applyLU_absintfc),DEFERRED,PASS :: apply
  ENDTYPE LU_PreCondType

  TYPE,EXTENDS(LU_PreCondType) :: ILU_PreCondType
    CONTAINS
      PROCEDURE,PASS :: setup => setup_ILU_PreCondType
      PROCEDURE,PASS :: apply => apply_ILU_PreCondType
  ENDTYPE ILU_PreCondType

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  TYPE,ABSTRACT,EXTENDS(PreConditionerType) :: SOR_PreCondType
    !size of the diagonal blocks
    INTEGER(SIK) :: blocksize
    !number of diagonal blocks in matrix
    INTEGER(SIK) :: numblocks
    !omega factor for sor
    REAL(SRK) :: omega
    !array of LU matrices for each diagonal block, will be dense!
    CLASS(MatrixType),ALLOCATABLE :: LU(:)
    !lower and upper portions of matrix with diagonal blocks removed
    CLASS(MatrixType),ALLOCATABLE :: LpU

    CONTAINS
        !initialize procedure
      PROCEDURE,PASS :: init => init_SOR_PreCondType
      !clear procedure
      PROCEDURE,PASS :: clear => clear_SOR_PreCondType
      PROCEDURE(precond_SOR_absintfc),DEFERRED,PASS :: setup
      PROCEDURE(precond_applySOR_absintfc),DEFERRED,PASS :: apply
  ENDTYPE SOR_PreCondType

  TYPE,EXTENDS(SOR_PreCondType) :: RSOR_PreCondType
    CONTAINS
      !setup procedure
      PROCEDURE,PASS :: setup => setup_RSOR_PreCondType
      !application procedure
      PROCEDURE,PASS :: apply => apply_RSOR_PreCondType
  ENDTYPE RSOR_PreCondType
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
  ENDINTERFACE

  CLASS(PreConditionerType),POINTER :: PETSC_PCSHELL_PC => NULL()

  !> set enumeration scheme for BILU preconditioners
  INTEGER(SIK),PARAMETER,PUBLIC :: BILU=0,BILUSGS=1

  CHARACTER(LEN=*),PARAMETER :: modName='PreconditionerTypes'

  TYPE(ExceptionHandlerType),SAVE :: ePreCondType
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
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
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
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
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
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
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!-------------------------------------------------------------------------------
! RSOR preconditioner initializer
    SUBROUTINE init_SOR_PreCondtype(thisPC,A,params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_RSOR_PreCondType'
      CLASS(SOR_PrecondType),INTENT(INOUT) :: thisPC
      CLASS(MatrixType),ALLOCATABLE,TARGET,INTENT(IN),OPTIONAL :: A
      TYPE(ParamType),INTENT(IN),OPTIONAL :: params
      TYPE(ParamType)::PListMat_LU
      INTEGER(SIK)::k

      IF(thisPC%isinit) THEN
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Preconditioner is already initialized!')
        RETURN
      ENDIF
      
      IF(.NOT. PRESENT(A) .OR. .NOT.(ALLOCATED(A))) THEN
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Matrix being used for RSOR Preconditioner is not allocated!')
        RETURN
      ENDIF
      IF(.NOT.(A%isInit)) THEN
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Matrix being used for RSOR Preconditioner is not initialized!')
        RETURN
      ENDIF
      
      thisPC%A => A
      
      !gets the number of blocks from the parameter list
      CALL params%get('PCType->numblocks',thisPC%numblocks)
      CALL params%get('PCType->omega',thisPC%omega)
      
      !makes sure that the number of blocks is valid
      IF(MOD(thisPC%A%n,thisPC%numblocks) .NE. 0)THEN
          CALL ePreCondtype%raiseError('Incorrect input to '//modName//'::'//myName// &
                  ' - Matrix size not divisible by number of blocks!')
      END IF
      
      !calculate block size
      thisPC%blocksize=thisPC%A%n/thisPC%numblocks
      
      !makes a lu matrix for each diagonal block in an array
      ALLOCATE(DenseSquareMatrixType :: thisPC%LU(thisPC%numblocks))
      !initializes those matrices
      CALL PListMat_LU%add('MatrixType->n',thisPC%blocksize)
      CALL PListMat_LU%add('MatrixType->isSym',.FALSE.)
      DO k=1,thisPC%numblocks
        CALL thisPC%LU(k)%init(PListMat_LU)
      END DO

      ! This might not be necessary here, but not sure
      SELECTTYPE(mat => thisPC%A)
        TYPE IS(DenseSquareMatrixType)
          ALLOCATE(DenseSquareMatrixType :: thisPC%LpU)
      
          ! Assign A to LpU
          SELECTTYPE(LpU => thisPC%LpU); TYPE IS(DenseSquareMatrixType)
            LpU=mat
          ENDSELECT
      
          IF(thisPC%LpU%isInit) THEN
            thisPC%isInit=.TRUE.
          ELSE
            CALL ePreCondtype%raiseError('Incorrect input to '//modName//'::'//myName// &
              ' - In RSOR Preconditioner initialization, RSOR was not properly initialized!')
          ENDIF
        CLASS DEFAULT
          CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
            ' - RSOR Preconditioners are not supported for input matrix type!')
      ENDSELECT
    ENDSUBROUTINE init_SOR_PreCondtype
!
!-------------------------------------------------------------------------------
! clears the rsor preconditioner
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
        DO i=1,thisPC%numblocks
            CALL thisPC%LU(i)%clear()
        END DO
        DEALLOCATE(thisPC%LU)
      ENDIF
      thisPC%isInit=.FALSE.
    ENDSUBROUTINE clear_SOR_PreCondtype
!
!-------------------------------------------------------------------------------
! Sets up RSOR preconditioner
    SUBROUTINE setup_RSOR_PreCondtype(thisPC)
      CLASS(RSOR_PrecondType),INTENT(INOUT) :: thisPC
      CHARACTER(LEN=*),PARAMETER :: myName='setup_RSOR_PreCondType'
      INTEGER(SIK)::k,i,j
      REAL(SRK)::tempreal

      !make sure everything is initialized and allocated
      IF(.NOT.(thisPC%isinit)) THEN
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Preconditioner is not initialized!')
      ELSEIF(.NOT.(ALLOCATED(thisPC%LpU))) THEN
          CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
            ' - Big Upper and Lower being used for RSOR Preconditioner is not allocated!')
      ELSEIF(.NOT.(thisPC%LpU%isInit)) THEN
          CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
            ' - Big Upper and Lower being used for RSOR Preconditioner is not initialized!')
      ELSE
          ! make sure each LU block is initialized
          DO k=1,thisPC%numblocks
            IF(.NOT.(thisPC%LU(k)%isInit)) THEN
              WRITE(*,*)'For Block',k
              CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                ' - A LU matrix being used for RSOR Preconditioner is not initialized!')
            END IF
          END DO
          
          !setup the Upper and Lower portion of the diagonal 
          SELECTTYPE(LpU => thisPC%LpU)
            CLASS IS(DenseSquareMatrixType)
                !basically just remove the diagonal values AND set the LU blocks
                DO k=1,thisPC%numblocks
                    DO i=1,thisPC%blocksize
                        DO j=1,thisPC%blocksize
                            CALL LpU%get((k-1)*thisPC%blocksize+i,(k-1)*thisPC%blocksize+j,tempreal)
                            CALL thisPC%LU(k)%set(i,j,tempreal)
                            CALL LpU%set((k-1)*thisPC%blocksize+i,(k-1)*thisPC%blocksize+j,0.0_SRK)
                        END DO
                    END DO
                END DO
          ENDSELECT
          !do LU factorization on the diagonal blocks
          CALL doolittle_LU_RSOR(thisPC)
      ENDIF
    ENDSUBROUTINE setup_RSOR_PreCondtype
!
!-------------------------------------------------------------------------------
! no real comments yet
    SUBROUTINE apply_RSOR_PreCondType(thisPC,v)
      CLASS(RSOR_PrecondType),INTENT(INOUT) :: thisPC
      CLASS(Vectortype),ALLOCATABLE,INTENT(INOUT) :: v
      CHARACTER(LEN=*),PARAMETER :: myName='apply_RSOR_PreCondType'
      TYPE(RealVectorType)::w(4)
      TYPE(ParamType)::PListVec_RSOR
      INTEGER(SIK)::k,i
      REAL(SRK)::tmpreal

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
          CALL PListVec_RSOR%add('VectorType->n',thisPC%A%n)
          CALL PListVec_RSOR%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
          CALL w(1)%init(PListVec_RSOR)
          CALL w(2)%init(PListVec_RSOR)
          CALL w(3)%init(PListVec_RSOR)
          CALL w(4)%init(PListVec_RSOR)
          SELECTTYPE(v)
            CLASS IS(RealVectorType)
                w(3)%b=v%b
                CALL RSORsolveL(thisPC,v,w(1))
                CALL RSORsolveU(thisPC,w(1),w(2))
                CALL BLAS_matvec(THISMATRIX=thisPC%LpU,X=w(2),Y=w(3),&
                    &BETA=1.0_SRK,TRANS='N',ALPHA=-thisPC%omega)
                CALL RSORsolveL(thisPC,w(3),w(4))
                CALL RSORsolveU(thisPC,w(4),v)
            CLASS DEFAULT
              CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                ' - Vector type is not support by this PreconditionerType.')
          ENDSELECT
        ENDIF
      ENDIF
    ENDSUBROUTINE apply_RSOR_PreCondType
!
!-------------------------------------------------------------------------------
! Doolittle algorithm for LU decomposition
! Does it for all diagonal blocks
    SUBROUTINE doolittle_LU_RSOR(thisPC)
      CLASS(RSOR_PrecondType),INTENT(INOUT) :: thisPC
      CHARACTER(LEN=*),PARAMETER :: myName='doolittle_LU_RSOR'
      INTEGER(SIK)::k,i,j,l
      REAL(SRK)::Ltemp(thisPC%blocksize,thisPC%blocksize),Utemp(thisPC%blocksize,thisPC%blocksize)
      
      !loop over all diagonal blocks
      DO l=1,thisPC%numblocks
          !these need to be 0 at start since the accumulate
          Utemp(:,:)=0
          Ltemp(:,:)=0
          !this does the actual LU decomposition on each block
          DO i=1,thisPC%blocksize
            DO j=1,thisPC%blocksize
                CALL thisPC%LU(l)%get(i,j,Utemp(i,j))
                DO k=1,i-1
                    Utemp(i,j)=Utemp(i,j)-Ltemp(i,k)*Utemp(k,j)
                END DO
            END DO
            DO j=i+1,thisPC%blocksize
                CALL thisPC%LU(l)%get(j,i,Ltemp(j,i))
                DO k=1,i-1
                    Ltemp(j,i)=Ltemp(j,i)-Ltemp(j,k)*Utemp(k,i)
                END DO
                Ltemp(j,i)=Ltemp(j,i)/Utemp(i,i)
            END DO
            Ltemp(i,i)=0
          END DO
          !set the block now to the new L in the lower and U in the upper
          !L is always 1 on diagonals, so set diagonals to U values
          DO i=1,thisPC%blocksize
            DO j=i,thisPC%blocksize
                CALL thisPC%LU(l)%set(i,j,Utemp(i,j))
            END DO
          END DO
          DO i=2,thisPC%blocksize
            DO j=1,i-1
                CALL thisPC%LU(l)%set(i,j,Ltemp(i,j))
            END DO
          END DO
      END DO
    ENDSUBROUTINE doolittle_LU_RSOR
!
!-------------------------------------------------------------------------------
! Solving L matrix system
    SUBROUTINE RSORsolveL(thisPC,b,x)
      CLASS(RSOR_PrecondType),INTENT(INOUT) :: thisPC
      CHARACTER(LEN=*),PARAMETER :: myName='RSORsolveL'
      TYPE(RealVectorType),INTENT(INOUT)::b
      TYPE(RealVectorType),INTENT(INOUT)::x
      INTEGER(SIK)::i,j,k
      REAL(SRK)::tempreal(3)
      
      DO k=1,thisPC%numblocks
        CALL x%setrange_scalar((k-1)*thisPC%blocksize+1,k*thisPC%blocksize,0.0_SRK)
        DO i=1,thisPC%blocksize
            CALL b%getone((k-1)*thisPC%blocksize+i,tempreal(1))
            CALL x%setone((k-1)*thisPC%blocksize+i,tempreal(1))
            DO j=1,i-1
                CALL x%getone((k-1)*thisPC%blocksize+i,tempreal(1))
                CALL x%getone((k-1)*thisPC%blocksize+j,tempreal(2))
                CALL thisPC%LU(k)%get(i,j,tempreal(3))
                CALL x%setone((k-1)*thisPC%blocksize+i,tempreal(1)-tempreal(2)*tempreal(3))
            END DO
        END DO
      END DO
    ENDSUBROUTINE RSORsolveL
!
!-------------------------------------------------------------------------------
! Solves U matrix system
    SUBROUTINE RSORsolveU(thisPC,b,x)
      CLASS(RSOR_PrecondType),INTENT(INOUT) :: thisPC
      CHARACTER(LEN=*),PARAMETER :: myName='RSORsolveU'
      TYPE(RealVectorType),INTENT(INOUT)::b
      TYPE(RealVectorType),INTENT(INOUT)::x
      INTEGER(SIK)::i,j,k
      REAL(SRK)::tempreal(3)
      
      DO k=1,thisPC%numblocks
        CALL x%setrange_scalar((k-1)*thisPC%blocksize+1,k*thisPC%blocksize,0.0_SRK)
        DO i=thisPC%blocksize,1,-1
            CALL b%getone((k-1)*thisPC%blocksize+i,tempreal(1))
            CALL x%setone((k-1)*thisPC%blocksize+i,tempreal(1))
            DO j=thisPC%blocksize,i+1,-1
                CALL x%getone((k-1)*thisPC%blocksize+i,tempreal(1))
                CALL x%getone((k-1)*thisPC%blocksize+j,tempreal(2))
                CALL thisPC%LU(k)%get(i,j,tempreal(3))
                CALL x%setone((k-1)*thisPC%blocksize+i,tempreal(1)-tempreal(2)*tempreal(3))
            END DO
            CALL x%getone((k-1)*thisPC%blocksize+i,tempreal(1))
            CALL thisPC%LU(k)%get(i,i,tempreal(2))
            CALL x%setone((k-1)*thisPC%blocksize+i,tempreal(1)/tempreal(2))
        END DO
      END DO
    ENDSUBROUTINE RSORsolveU
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
