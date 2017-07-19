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

  IMPLICIT NONE
  PRIVATE

#ifdef FUTILITY_HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
#endif

  !
  ! List of Public members
  PUBLIC :: PreconditionerType
  PUBLIC :: LU_PreCondType
  PUBLIC :: ILU_PreCondType
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

  ABSTRACT INTERFACE
    SUBROUTINE precond_init_absintfc(thisPC,A)
      IMPORT :: PreconditionerType,Matrixtype
      CLASS(PreconditionerType),INTENT(INOUT) :: thisPC
      CLASS(MatrixType),TARGET,INTENT(IN),OPTIONAL :: A
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
    SUBROUTINE init_LU_PreCondtype(thisPC,A)
      CHARACTER(LEN=*),PARAMETER :: myName='init_LU_PreCondType'
      CLASS(LU_PrecondType),INTENT(INOUT) :: thisPC
      CLASS(MatrixType),ALLOCATABLE,TARGET,INTENT(IN),OPTIONAL :: A
      INTEGER(SIK) :: col,row,j,nU,nL,nnzU,nnzL
      INTEGER(SIK) :: X
      REAL(SRK) :: val
      TYPE(ParamType) :: PL

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
