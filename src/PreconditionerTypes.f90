
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                              Copyright (C) 2012                              !
!                   The Regents of the University of Michigan                  !
!              MPACT Development Group and Prof. Thomas J. Downar              !
!                             All rights reserved.                             !
!                                                                              !
! Copyright is reserved to the University of Michigan for purposes of          !
! controlled dissemination, commercialization through formal licensing, or     !
! other disposition. The University of Michigan nor any of their employees,    !
! makes any warranty, express or implied, or assumes any liability or          !
! responsibility for the accuracy, completeness, or usefulness of any          !
! information, apparatus, product, or process disclosed, or represents that    !
! its use would not infringe privately owned rights. Reference herein to any   !
! specific commercial products, process, or service by trade name, trademark,  !
! manufacturer, or otherwise, does not necessarily constitute or imply its     !
! endorsement, recommendation, or favoring by the University of Michigan.      !
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

  !
  ! List of Public members
  PUBLIC :: PreconditionerType
  PUBLIC :: LU_PreCondType
  PUBLIC :: ILU_PreCondType
  PUBLIC :: BILU_PreCondType
  PUBLIC :: ePreCondType


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
    CLASS(MatrixType),POINTER :: L
    CLASS(MatrixType),POINTER :: U

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
  
  TYPE,EXTENDS(LU_PreCondType) :: BILU_PreCondType
    INTEGER(SIK) :: nPlane
    INTEGER(SIK) :: nPin
    INTEGER(SIK) :: nGrp
    INTEGER(SIK) :: BILUType  !for 2D, 3D and other variants
    REAL(SRK),ALLOCATABLE :: F0(:,:,:)
    REAL(SRK),ALLOCATABLE :: E(:,:)
    REAL(SRK),ALLOCATABLE :: W(:,:) 
    REAL(SRK),ALLOCATABLE :: N(:,:) 
    REAL(SRK),ALLOCATABLE :: S(:,:) 
    CONTAINS
      PROCEDURE,PASS :: setup => setup_BILU_PreCondType
      PROCEDURE,PASS :: apply => apply_BILU_PreCondType
  ENDTYPE BILU_PreCondType

  ABSTRACT INTERFACE
    SUBROUTINE precond_init_absintfc(PC,A)
      IMPORT :: PreconditionerType,Matrixtype
      CLASS(PreconditionerType),INTENT(INOUT) :: PC
      CLASS(MatrixType),TARGET,INTENT(IN) :: A
    ENDSUBROUTINE precond_init_absintfc
  ENDINTERFACE

  ABSTRACT INTERFACE
    SUBROUTINE precond_apply_absintfc(PC,v)
      IMPORT :: PreconditionerType,VectorType
      CLASS(PreconditionerType),INTENT(INOUT) :: PC
      CLASS(VectorType),INTENT(INOUT) :: v
    ENDSUBROUTINE precond_apply_absintfc

    SUBROUTINE precond_applyLU_absintfc(PC,v)
      IMPORT :: LU_PreCondType,VectorType
      CLASS(LU_PreCondType),INTENT(INOUT) :: PC
      CLASS(VectorType),INTENT(INOUT) :: v
    ENDSUBROUTINE precond_applyLU_absintfc
  ENDINTERFACE

  ABSTRACT INTERFACE
    SUBROUTINE precond_absintfc(PC)
      IMPORT :: PreconditionerType
      CLASS(PreconditionerType),INTENT(INOUT) :: PC
    ENDSUBROUTINE precond_absintfc

    SUBROUTINE precond_LU_absintfc(PC)
      IMPORT :: LU_PreCondType
      CLASS(LU_PreCondType),INTENT(INOUT) :: PC
    ENDSUBROUTINE precond_LU_absintfc
  ENDINTERFACE

  CHARACTER(LEN=*),PARAMETER :: modName='PreconditionerTypes'

  TYPE(ExceptionHandlerType),POINTER,SAVE :: ePreCondType => NULL()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
    SUBROUTINE init_LU_PreCondtype(PC,A)
      CHARACTER(LEN=*),PARAMETER :: myName='init_LU_PreCondType'
      CLASS(LU_PrecondType),INTENT(INOUT) :: PC
      CLASS(MatrixType),TARGET,INTENT(IN) :: A

      TYPE(ParamType) :: PL
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: col,row,i,j,nU,nL,nnzU,nnzL
      INTEGER(SIK) :: X
      REAL(SRK) :: val

WRITE(*,*) '   --Initializing Preconditioner...'
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(ePreCondType)) THEN
        ALLOCATE(ePreCondType)
        localalloc=.TRUE.
      ENDIF

      IF(PC%isinit) THEN
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Preconditioner is already initialized!')
      ELSE
        IF(.NOT.(A%isInit)) THEN
          CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
            ' - Matrix being used for LU Preconditioner is not initialized!')
        ELSE
          PC%A => A

          !allocate additional storage for BILU3D
          SELECTTYPE(PC)
            TYPE IS(BILU_PrecondType)
              X=SQRT(REAL(pc%nPin))
              ALLOCATE(PC%F0(pc%nPlane*pc%nPin,pc%nGrp,pc%nGrp))
              ALLOCATE(PC%E(pc%nPlane*X,pc%nGrp*(X-1)))
              ALLOCATE(PC%W(pc%nPlane*X,pc%nGrp*(X-1)))
              ALLOCATE(PC%N(pc%nPlane,pc%nGrp*X*(x-1)))
              ALLOCATE(PC%S(pc%nPlane,pc%nGrp*X*(x-1)))
              PC%F0=0.0_SRK
              PC%E=0.0_SRK
              PC%W=0.0_SRK
              PC%N=0.0_SRK
              PC%S=0.0_SRK
          ENDSELECT

          ! This might not be necessary here, but not sure
          SELECTTYPE(mat => PC%A)
            CLASS IS(DenseSquareMatrixType)
              ALLOCATE(SparseMatrixType :: PC%L)
              ALLOCATE(SparseMatrixType :: PC%U)

              ! Loop over A to get initialization data for L and U
              nU=0; nL=0  !number of rows
              nnzU=0; nnzL=0 !number of non-zero elements
              DO row=1,mat%n
                DO col=1,mat%n
                  ! This may be redundant since mat is sparse, but be safe for now
                  CALL mat%get(row,col,val)
                  IF(.NOT.(val .APPROXEQA. 0.0_SRK)) THEN
                    IF(col == row) THEN
                      nnzU=nnzU+1 ! This location is in U
                    ELSEIF(col > row) THEN
                      nnzU=nnzU+1 !This location is in U
                    ELSE
                      nnzL=nnzL+1 !This location is in L
                    ENDIF
                  ENDIF
                ENDDO
                nnzL=nnzL+1
              ENDDO
                
              ! Initialize L and U
              nU=mat%n
              nL=mat%n
              CALL PL%add('MatrixType->n',nU)
              CALL PL%add('MatrixType->nnz',nnzU)
              CALL PC%U%init(PL)
              CALL PL%set('MatrixType->n',nL)
              CALL PL%set('MatrixType->nnz',nnzL)
              CALL PC%L%init(PL)
              CALL PL%clear()
                    
              SELECTTYPE(L => PC%L); TYPE IS(SparseMatrixType)
                SELECTTYPE(U => PC%U); TYPE IS(SparseMatrixType)
                  ! Set the shape
                  DO row=1,mat%n
                    DO col=1,mat%n
                      ! This may be redundant since mat is sparse, but be safe for now
                      CALL mat%get(row,col,val)
                      IF(.NOT.(val .APPROXEQA. 0.0_SRK)) THEN
                        IF(col >= row) THEN
                          CALL U%setShape(row,col,val)
                        ELSE
                          CALL L%setShape(row,col,val)
                        ENDIF
                      ENDIF
                    ENDDO
                    CALL L%setShape(row,row,1.0_SRK)
                  ENDDO
                ENDSELECT
              ENDSELECT
                
              IF(PC%L%isInit .AND. PC%U%isInit) THEN
                PC%isInit=.TRUE.
              ELSEIF(.NOT.(PC%L%isInit)) THEN
                CALL ePreCondtype%raiseError('Incorrect input to '//modName//'::'//myName// &
                  ' - In LU Preconditioner initialization, L was not properly initialized')
              ELSE
                CALL ePreCondtype%raiseError('Incorrect input to '//modName//'::'//myName// &
                  ' - In LU Preconditioner initialization, U was not properly initialized')
              ENDIF
            CLASS IS(SparseMatrixType)
              ALLOCATE(SparseMatrixType :: PC%L)
              ALLOCATE(SparseMatrixType :: PC%U)

              SELECTTYPE(PC)
                TYPE IS(ILU_PrecondType)
                  ! Loop over A to get initialization data for L and U
                  nU=0; nL=0  !number of rows
                  nnzU=0; nnzL=0 !number of non-zero elements
                  DO row=1,SIZE(mat%ia)-1
                    DO j=mat%ia(row),mat%ia(row+1)-1
                      col=mat%ja(j)
                      ! This may be redundant since mat is sparse, but be safe for now
                      CALL mat%get(row,col,val)
                      IF(.NOT.(val .APPROXEQA. 0.0_SRK)) THEN
                        IF(col == row) THEN
                          nnzU=nnzU+1
                          nnzL=nnzL+1
                        ELSEIF(col > row) THEN
                          nnzU=nnzU+1 !This location is in U
                        ELSE
                          nnzL=nnzL+1 !This location is in L
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDDO
                TYPE IS(BILU_PrecondType)
                  ! Loop over A to get initialization data for L and U
                  nnzU=0
                  nnzL=0
                  DO row=1,SIZE(mat%ia)-1
                    DO j=mat%ia(row),mat%ia(row+1)-1
                      col=mat%ja(j)
                      CALL mat%get(row,col,val)
                      IF(.NOT.(val .APPROXEQA. 0.0_SRK)) THEN
                        IF(col > row+PC%Ngrp*PC%Npin-1) THEN
                          nnzU=nnzU+1
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDDO
                  nnzL=mat%nnz-nnzU
                  nnzU=nnzU+mat%n
              ENDSELECT
                
              ! Initialize L and U
              nU=mat%n
              nL=mat%n
              CALL PL%add('MatrixType->n',nU)
              CALL PL%add('MatrixType->nnz',nnzU)
              CALL PC%U%init(PL)
              CALL PL%set('MatrixType->n',nL)
              CALL PL%set('MatrixType->nnz',nnzL)
              CALL PC%L%init(PL)
              CALL PL%clear()

              SELECTTYPE(PC)
                TYPE IS(ILU_PrecondType)  
                  SELECTTYPE(L => PC%L); TYPE IS(SparseMatrixType)
                    SELECTTYPE(U => PC%U); TYPE IS(SparseMatrixType)
                      ! Set the shape
                      DO row=1,SIZE(mat%ia)-1
                        DO j=mat%ia(row),mat%ia(row+1)-1
                          col=mat%ja(j)
                          ! This may be redundant since mat is sparse, but be safe for now
                          CALL mat%get(row,col,val)
                          IF(.NOT.(val .APPROXEQA. 0.0_SRK)) THEN
                            IF(col >= row) THEN
                              CALL U%setShape(row,col,val)
                            ELSE
                              CALL L%setShape(row,col,val)
                            ENDIF
                          ENDIF
                        ENDDO
                        CALL L%setShape(row,row,1.0_SRK)
                      ENDDO
                    ENDSELECT
                  ENDSELECT
                TYPE IS(BILU_PrecondType) 
                  SELECTTYPE(L => PC%L); TYPE IS(SparseMatrixType)
                    SELECTTYPE(U => PC%U); TYPE IS(SparseMatrixType)
                      ! Set the shape
                      DO row=1,SIZE(mat%ia)-1
                        DO j=mat%ia(row),mat%ia(row+1)-1
                          col=mat%ja(j)
                          ! This may be redundant since mat is sparse, but be safe for now
                          CALL mat%get(row,col,val)
                          IF(.NOT.(val .APPROXEQA. 0.0_SRK)) THEN
                            IF(col >= row+PC%Ngrp*PC%Npin-1) THEN
                              CALL U%setShape(row,col,0.0_SRK)
                            ELSEIF(row == col) THEN
                              CALL U%setShape(row,row,0.0_SRK)
                              CALL L%setShape(row,row,0.0_SRK)
                            ELSE
                              CALL L%setShape(row,col,0.0_SRK)
                            ENDIF
                          ENDIF
                        ENDDO
                      ENDDO
                    ENDSELECT
                  ENDSELECT 
              ENDSELECT
                
              IF(PC%L%isInit .AND. PC%U%isInit) THEN
                PC%isInit=.TRUE.
              ELSEIF(.NOT.(PC%L%isInit)) THEN
                CALL ePreCondtype%raiseError('Incorrect input to '//modName//'::'//myName// &
                  ' - In LU Preconditioner initialization, L was not properly initialized')
              ELSE
                CALL ePreCondtype%raiseError('Incorrect input to '//modName//'::'//myName// &
                  ' - In LU Preconditioner initialization, U was not properly initialized')
              ENDIF
            CLASS IS(PETScMatrixType)
              !allocate L and U
              ALLOCATE(PETScMatrixType :: PC%L)
              ALLOCATE(PETScMatrixType :: PC%U)
              
              !initialize L and U
              SELECTTYPE(U => PC%U); TYPE IS(PETScMatrixType)
                SELECTTYPE(L => PC%L); TYPE IS(PETScMatrixType)
                  ! Initialize L and U (add preallocation eventually)
                  CALL PL%add('MatrixType->matType',SPARSE)
                  CALL PL%add('MatrixType->n',mat%n)
                  CALL PL%add('MatrixType->isSym',mat%isSymmetric)
                  CALL PL%add('MatrixType->MPI_Comm_ID',mat%comm)  
                  CALL U%init(PL)
                  CALL L%init(PL)
                  CALL PL%clear()
                ENDSELECT
              ENDSELECT

              PC%isInit=.TRUE.
            CLASS DEFAULT
              CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                ' - LU Preconditioners are not supported for input matrix type!')
          ENDSELECT 

        ENDIF
      ENDIF
    ENDSUBROUTINE init_LU_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
    SUBROUTINE clear_LU_PreCondtype(PC)
      CLASS(LU_PrecondType),INTENT(INOUT) :: PC

      IF(ASSOCIATED(PC%A)) NULLIFY(PC%A)
      IF(ASSOCIATED(PC%U)) CALL PC%U%clear()
      DEALLOCATE(PC%U)
      IF(ASSOCIATED(PC%L)) CALL PC%L%clear()
      DEALLOCATE(PC%L)
      SELECTTYPE(PC); TYPE IS(BILU_PrecondType) 
        IF(ALLOCATED(PC%F0)) DEALLOCATE(PC%F0)
        IF(ALLOCATED(PC%E)) DEALLOCATE(PC%E)
        IF(ALLOCATED(PC%W)) DEALLOCATE(PC%W)
        IF(ALLOCATED(PC%N)) DEALLOCATE(PC%N)
        IF(ALLOCATED(PC%S)) DEALLOCATE(PC%S)
      ENDSELECT
      PC%isInit=.FALSE.

    ENDSUBROUTINE clear_LU_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
    SUBROUTINE apply_ILU_PreCondType(PC,v)
      CLASS(ILU_PrecondType),INTENT(INOUT) :: PC
      CLASS(Vectortype),INTENT(INOUT) :: v

      CHARACTER(LEN=*),PARAMETER :: myName='apply_ILU_PreCondType'
      LOGICAL(SBK) :: localalloc

      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(ePreCondType)) THEN
        ALLOCATE(ePreCondType)
        localalloc=.TRUE.
      ENDIF

      IF(.NOT.(PC%isInit)) THEN
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Preconditioner is not initialized.')
      ELSEIF(.NOT.(v%isInit)) THEN
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - VectorType is not initialized.')
      ELSE
        SELECTTYPE(v)
          CLASS IS(RealVectorType)
            SELECTTYPE(L => PC%L)
              CLASS IS(SparseMatrixType)
                SELECTTYPE(U => PC%U); TYPE IS(SparseMatrixType)
                  CALL BLAS_matvec(THISMATRIX=L,X=v,Y=v,UPLO='L',DIAG='T',TRANS='N')
                  CALL BLAS_matvec(THISMATRIX=U,X=v,Y=v,UPLO='U',DIAG='N',TRANS='N')
                ENDSELECT
              CLASS DEFAULT
                CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                  ' - Preconditioner Matrix type is not supported!')
            ENDSELECT
          CLASS DEFAULT
            CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
              ' - Vector type is not support by this PreconditionerType.')
        ENDSELECT
      ENDIF
    ENDSUBROUTINE apply_ILU_PreCondType
!-------------------------------------------------------------------------------
!> @brief Applies the Block LU Decomposition
!> @param PC the preconditioner
!> @param v the vector to operate on
!>
    SUBROUTINE apply_BILU_PreCondType(PC,v)
      CHARACTER(LEN=*),PARAMETER :: myName='apply_BILU_PreCondType'
      CLASS(BILU_PrecondType),INTENT(INOUT) :: PC
      CLASS(Vectortype),INTENT(INOUT) :: v

      LOGICAL(SBK) :: localalloc

      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(ePreCondType)) THEN
        ALLOCATE(ePreCondType)
        localalloc=.TRUE.
      ENDIF

      IF(.NOT.(PC%isInit)) THEN
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Preconditioner is not initialized.')
      ELSEIF(.NOT.(v%isInit)) THEN
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - VectorType is not initialized.')
      ELSE

        SELECTTYPE(v)
          CLASS IS(PETScVectorType)
            SELECTTYPE(L => PC%L)
              CLASS IS(PETScMatrixType)

              CLASS DEFAULT
                CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                  ' - Preconditioner Matrix type is not supported!')
            ENDSELECT
          CLASS DEFAULT
            CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
              ' - Vector type is not support by this PreconditionerType.')
        ENDSELECT
      ENDIF

    ENDSUBROUTINE apply_BILU_PreCondType
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
    SUBROUTINE setup_ILU_PreCondtype(PC)
      CLASS(ILU_PrecondType),INTENT(INOUT) :: PC

      CHARACTER(LEN=*),PARAMETER :: myName='setup_ILU_PreCondType'
      INTEGER(SIK) :: row,col,col2,i,j,k,nL,nU,nnzL,nnzU
      REAL(SRK) :: val1,val2,val3
      LOGICAL(SBK) :: localalloc
      TYPE(ParamType) :: PL

      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(ePreCondType)) THEN
        ALLOCATE(ePreCondType)
        localalloc=.TRUE.
      ENDIF

      IF(.NOT.(PC%isinit)) THEN
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Preconditioner is not initialized!')
      ELSE
        IF(.NOT.(PC%A%isInit)) THEN
          CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
            ' - Matrix being used for LU Preconditioner is not initialized!')
        ELSE
          SELECTTYPE(mat => PC%A)
            CLASS IS(DenseSquareMatrixtype)
              SELECTTYPE(U => PC%U); TYPE IS(SparseMatrixType)
                SELECTTYPE(L => PC%L); TYPE IS(SparseMatrixType)
                  IF(.NOT.(U%isinit)) THEN
                    CALL ePrecondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                      ' - in LU decomposition, U was not properly initialize!')
                  ELSEIF(.NOT.(L%isinit)) THEN
                    CALL ePrecondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                      ' - in LU decomposition, L was not properly initialize!')
                  ELSE
                    ! LU Decomposition
                    j=mat%n
                    ! Just do "simple" looping.  Since A is dense, naive looping through columns is about
                    ! as efficient as a more complicated sparse matrix looping scheme
                    DO row=2,j
                      DO col=1,row-1
                        CALL L%get(row,col,val1)
                        IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) THEN
                          CALL U%get(col,col,val2)
                          val2=val1/val2
                          CALL L%set(row,col,val2)
                          DO col2=col+1,j
                            CALL U%get(col,col2,val3)
                            IF(.NOT.(val3 .APPROXEQA. 0.0_SRK)) THEN
                              IF(col2 < row) THEN
                                CALL L%get(row,col2,val1)
                                IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) CALL L%set(row,col2,val1-val2*val3)
                              ELSE
                                CALL U%get(row,col2,val1)
                                IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) CALL U%set(row,col2,val1-val2*val3)
                              ENDIF
                            ENDIF
                          ENDDO
                        ENDIF
                      ENDDO
                    ENDDO
                  ENDIF
                ENDSELECT
              ENDSELECT
            CLASS IS(SparseMatrixType)
              SELECTTYPE(U => PC%U); TYPE IS(SparseMatrixType)
                SELECTTYPE(L => PC%L); TYPE IS(SparseMatrixType)
                  ! Make sure initialization worked
                  IF(.NOT.(U%isInit)) THEN
                    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                      ' - In LU decomposition, U was not properly initialized!')
                  ELSEIF(.NOT.(L%isInit)) THEN
                    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                      ' - In LU decomposition, L was not properly initialized!')
                  ! Now loop through A again and set values of L and U
                  ELSE
                    ! Now complete LU Decomposition
                    ! A is sparse, so use A%ia and A%ja to work through L and U
                    DO row=2,SIZE(mat%ia)-1
                      DO col=mat%ia(row),mat%ia(row+1)-1
                        IF(mat%ja(col) >= row) EXIT
                        CALL L%get(row,mat%ja(col),val1)
                        IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) THEN
                          CALL U%get(mat%ja(col),mat%ja(col),val2)
                          val2=val1/val2
                          CALL L%set(row,mat%ja(col),val2)
                          DO col2=col+1,mat%ia(row+1)-1
                            CALL U%get(mat%ja(col),mat%ja(col2),val3)
                            IF(.NOT.(val3 .APPROXEQA. 0.0_SRK)) THEN
                              IF(mat%ja(col2) < row) THEN
                                CALL L%get(row,mat%ja(col2),val1)
                                IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) CALL L%set(row,mat%ja(col2),val1-val2*val3)
                              ELSE
                                CALL U%get(row,mat%ja(col2),val1)
                                IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) CALL U%set(row,mat%ja(col2),val1-val2*val3)
                              ENDIF
                            ENDIF
                          ENDDO
                        ENDIF
                      ENDDO
                    ENDDO
                  ENDIF
                ENDSELECT
              ENDSELECT
            CLASS DEFAULT
              CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                ' - LU Preconditioners are not supported by input matrix type!')
          ENDSELECT 
        ENDIF
      ENDIF

      IF(localalloc) DEALLOCATE(ePreCondType)
    ENDSUBROUTINE setup_ILU_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Sets up the BILU Preconditioner
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
    SUBROUTINE setup_BILU_PreCondtype(PC)
      CHARACTER(LEN=*),PARAMETER :: myName='setup_BILU_PreCondType'
      CLASS(BILU_PrecondType),INTENT(INOUT) :: PC
      
      INTEGER(SIK) :: ix,iy,iz
      INTEGER(SIK) :: row,col
      INTEGER(SIK) :: d2,c2
      INTEGER(SIK) :: d0,c0
      INTEGER(SIK) :: d1,c1
      INTEGER(SIK) :: XY,X,Y,Z,G
      INTEGER(SIK) :: dim0D,dim1D,dim2D
      INTEGER(SIK) :: ind
      INTEGER(SIK) :: rowstt,colstt
      REAL(SRK) :: val
      LOGICAL(SBK) :: localalloc
      TYPE(ParamType) :: PL
      REAL(SRK),ALLOCATABLE :: invM2(:,:),invM1(:,:),invM0(:,:)
      REAL(SRK),ALLOCATABLE :: tmp2D(:,:),tmp1D(:,:),tmp0D(:,:)
      REAL(SRK),ALLOCATABLE :: L2(:,:),U2(:,:),F2(:,:)
      REAL(SRK),ALLOCATABLE :: L1(:,:),U1(:,:),F1(:,:)
      REAL(SRK),ALLOCATABLE :: T(:),B(:),S(:),N(:),E(:),W(:)
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(ePreCondType)) THEN
        ALLOCATE(ePreCondType)
        localalloc=.TRUE.
      ENDIF

      IF(.NOT.(PC%isinit)) THEN
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Preconditioner is not initialized!')
      ELSE
        IF(.NOT.(PC%A%isInit)) THEN
          CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
            ' - Matrix being used for LU Preconditioner is not initialized!')
        ELSE
          
          !dimension variables
          XY=pc%nPin
          X=SQRT(REAL(XY))
          Y=SQRT(REAL(XY))
          Z=pc%nPlane
          G=pc%nGrp
          dim0D=G
          dim1D=X*G
          dim2D=XY*G
          !variables for 3D
          ALLOCATE(invM2(dim2D,dim2D))
          ALLOCATE(T(dim2D))
          ALLOCATE(B(dim2D))
          ALLOCATE(tmp2D(dim2D,dim2D))
          T=0.0_SRK
          B=0.0_SRK
          tmp2D=0.0_SRK
          invM2=0.0_SRK
          !variables for 2D
          ALLOCATE(L2(dim2D,dim2D))
          ALLOCATE(U2(dim2D,dim2D))
          ALLOCATE(F2(dim2D,dim2D))
          ALLOCATE(S(dim1D))
          ALLOCATE(N(dim1D))
          ALLOCATE(invM1(dim1D,dim1D))
          ALLOCATE(tmp1D(dim1D,dim1D))
          L2=0.0_SRK
          U2=0.0_SRK
          F2=0.0_SRK
          S=0.0_SRK
          N=0.0_SRK
          tmp1D=0.0_SRK
          invM1=0.0_SRK
          !variables for 1D
          ALLOCATE(L1(dim1D,dim1D))
          ALLOCATE(U1(dim1D,dim1D))
          ALLOCATE(F1(dim1D,dim1D))
          ALLOCATE(E(dim0D))
          ALLOCATE(W(dim0D))
          ALLOCATE(tmp0D(dim0D,dim0D))
          ALLOCATE(invM0(dim0D,dim0D))
          L1=0.0_SRK
          U1=0.0_SRK
          F1=0.0_SRK
          E=0.0_SRK
          W=0.0_SRK
          tmp0D=0.0_SRK
          invM0=0.0_SRK
          
          DO iz=1,Z
            d2=(iz-2)*dim2D+1
            c2=(iz-1)*dim2D+1
            
            IF(iz==1) THEN
              !form diagonal block of L
              DO row=c2,c2+dim2D-1
                DO col=c2,c2+dim2D-1
                  CALL pc%A%get(row,col,val)
                  IF(val /= 0.0_SRK) CALL pc%L%set(row,col,val)
                ENDDO
              ENDDO
              !form diagonal block of U (Identity)
              DO row=c2,c2+dim2D-1
                CALL pc%U%set(row,row,1.0_SRK)
              ENDDO
            ELSE
              !pull T block into matrix
              DO row=d2,d2+dim2D-1
                CALL pc%A%get(row,c2+row-d2,val)
                T(row-d2+1)=val
              ENDDO
              !pull B block into matrix
              DO row=c2,c2+dim2D-1
                CALL pc%A%get(row,d2+row-c2,val)
                B(row-c2+1)=val
              ENDDO     
              !form diagonal block of L
              tmp2D=0.0_SRK
              CALL dmatmul_left(B,invM2,tmp2D)
              CALL dmatmul_right(tmp2D,T,tmp2D)
              !form diagonal block of L
              DO row=c2,c2+dim2D-1
                DO col=c2,c2+dim2D-1
                  CALL pc%A%get(row,col,val)
                  IF(val /= 0.0_SRK) &
                    CALL pc%L%set(row,col,val-tmp2D(row-c2+1,col-c2+1))
                ENDDO
              ENDDO
              !form lower block of L
              DO row=c2,c2+dim2D-1
                DO col=d2,d2+dim2D-1
                  CALL pc%A%get(row,col,val)
                  IF(val /= 0.0_SRK) &
                    CALL pc%L%set(row,col,val)
                ENDDO
              ENDDO
              !form diagonal block of U (Identity)
              DO row=c2,c2+dim2D-1
                CALL pc%U%set(row,row,1.0_SRK)
              ENDDO
              !form upper block of U
              CALL dmatmul_right(invM2,T,tmp2D)
              DO row=d2,d2+dim2D-1
                DO col=c2,c2+dim2D-1
                  CALL pc%A%get(row,col,val)
                  IF(val /= 0.0_SRK) &
                    CALL pc%U%set(row,col,tmp2D(row-d2+1,col-c2+1))
                ENDDO
              ENDDO

            ENDIF
            
            !pull block M into matrix
            tmp2D=0.0_SRK
            DO row=c2,c2+dim2D-1
              DO col=c2,c2+dim2D-1
                CALL pc%L%get(row,col,val)
                IF(val /= 0.0_SRK) &
                  tmp2D(row-c2+1,col-c2+1)=val
              ENDDO
            ENDDO
            
            !LU M_k (2)  ...tmp2D
            DO iy=1,Y
              d1=(iy-2)*dim1D+1
              c1=(iy-1)*dim1D+1
              
              IF(iy==1) THEN
                !set diagonal of L2
                DO row=c1,c1+dim1D-1
                  DO col=c1,c1+dim1D-1
                    val=tmp2D(row,col)
                    L2(row,col)=val
                  ENDDO
                ENDDO
                !set U to diagonal block
                DO row=c1,c1+dim1D-1
                  U2(row,row)=1.0_SRK
                ENDDO
              ELSE
                !fill S
                DO row=c1,c1+dim1D-1
                  S(row-c1+1)=tmp2D(row,d1+row-c1)
                ENDDO
                !fill N
                N=0.0_SRK
                DO row=d1,d1+dim1D-1
                  N(row-d1+1)=tmp2D(row,c1+row-d1)
                ENDDO
                !form diagonal block of L
                tmp1D=0.0_SRK
                CALL dmatmul_left(S,invM1,tmp1D)
                CALL dmatmul_right(tmp1D,N,tmp1D)
                DO row=c1,c1+dim1D-1
                  DO col=c1,c1+dim1D-1
                    IF(tmp2D(row,col) /= 0.0_SRK) &
                      L2(row,col)=tmp2D(row,col)-tmp1D(row-c1+1,col-c1+1)
                  ENDDO
                ENDDO
                !form lower block of L
                DO row=c1,c1+dim1D-1
                  L2(row,d1+row-c1)=S(row-c1+1)
                ENDDO
                !form upper block of U
                CALL dmatmul_right(invM1,N,tmp1D)
                DO row=d1,d1+dim1D-1
                  DO col=c1,c1+dim1D-1
                    IF(tmp2D(row,col) /= 0.0_SRK) &
                      U2(row,col)=tmp1D(row-d1+1,col-c1+1)
                  ENDDO
                ENDDO
                !form diagonal block of U
                CALL dmatmul_right(invM1,N,tmp1D)
                DO row=c1,c1+dim1D-1
                  U2(row,row)=1.0_SRK
                ENDDO
                
              ENDIF
              
              tmp1D=L2(c1:c1+dim1D-1,c1:c1+dim1D-1)
              
              DO ix=1,X
                             
                d0=(ix-2)*dim0D+1
                c0=(ix-1)*dim0D+1
              
                IF(ix==1) THEN
                  !set diagonal of L
                  DO row=c0,c0+dim0D-1
                    DO col=c0,c0+dim0D-1
                      L1(row,col)=tmp1D(row,col)
                    ENDDO
                  ENDDO
                  !set U to diagonal block
                  DO row=c0,c0+dim0D-1
                    U1(row,row)=1.0_SRK
                  ENDDO
                ELSE
                  !fill W
                  DO row=c0,c0+dim0D-1
                    W(row-c0+1)=tmp1D(row,d0+row-c0)
                  ENDDO
                  !fill E
                  DO row=d0,d0+dim0D-1
                    E(row-d0+1)=tmp1D(row,c0+row-d0)
                  ENDDO
                  !set diagonal of L
                  tmp0D=0.0_SRK
                  CALL dmatmul_left(W,invM0,tmp0D)
                  CALL dmatmul_right(tmp0D,E,tmp0D)
                  DO row=c0,c0+dim0D-1
                    DO col=c0,c0+dim0D-1
                      IF(tmp1D(row,col) /= 0.0_SRK) &
                        L1(row,col)=tmp1D(row,col)-tmp0D(row-c0+1,col-c0+1)
                    ENDDO
                  ENDDO
                  !set lower block of L
                  DO row=c0,c0+dim0D-1
                    L1(row,d0+row-c0)=W(row-c0+1)
                  ENDDO
                  !set upper block of U
                  CALL dmatmul_right(invM0,E,tmp0D)
                  DO row=d0,d0+dim0D-1
                    DO col=c0,c0+dim0D-1
                      IF(tmp1D(row,col) /= 0.0_SRK) &
                        U1(row,col)=tmp0D(row-d0+1,col-c0+1)
                    ENDDO
                  ENDDO               
                  !set diagonal block of U
                  DO row=c0,c0+dim0D-1
                    U1(row,row)=1.0_SRK
                  ENDDO
                ENDIF
                
                tmp0D=L1(c0:c0+dim0D-1,c0:c0+dim0D-1)
                
                !determine 0D inverse (always direct for 0D)
                CALL direct_inv(tmp0D,invM0)
                F1(c0:c0+dim0D-1,c0:c0+dim0D-1)=invM0
                ind=(iz-1)*XY+(iy-1)*Y+ix
                PC%F0(ind,:,:)=invM0

              ENDDO !ix
              
              !determine 1D inverse
              CALL ABI(L1,U1,F1,dim0D,invM1)
              F2(c1:c1+dim1D-1,c1:c1+dim1D-1)=invM1
              !store for apply
              ind=(iz-1)*Y+iy
              DO row=1,dim0D*(X-1)
                PC%E(ind,row)=tmp1D(row,dim0D+row)
              ENDDO
              DO row=1,dim0D*(X-1)
                PC%W(ind,row)=tmp1D(dim0D+row,row)
              ENDDO
                
            ENDDO !iy

            !determine 2D inverse
            CALL ABI(L2,U2,F2,dim1D,invM2)
            DO row=1,dim1D*(X-1)
              PC%N(iz,row)=tmp2D(row,dim1D+row)
            ENDDO
            DO row=1,dim1D*(X-1)
              PC%S(iz,row)=tmp2D(dim1D+row,row)
            ENDDO
            
          ENDDO !iz
          
        ENDIF
      ENDIF

      IF(localalloc) DEALLOCATE(ePreCondType)
    ENDSUBROUTINE setup_BILU_PreCondtype
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
!-------------------------------------------------------------------------------
!> @brief Returns the complete inverse of a matrix
!>
    SUBROUTINE direct_inv(inpA,invA)
      REAL(SRK),INTENT(IN) :: inpA(:,:)
      REAL(SRK),INTENT(INOUT) :: invA(:,:)
      
      REAL(SRK),ALLOCATABLE :: A(:,:)
      INTEGER(SIK) :: ix,iy
      REAL(SRK) :: tmp
      
      ALLOCATE(A(1:SIZE(inpA(:,1)),1:SIZE(inpA(1,:))))
      A=inpA
      invA=0.0_SRK
      
      !set inverse to identity
      DO ix=1,SIZE(invA(1,:))
        invA(ix,ix)=1.0_SRK
      ENDDO
      
      DO iy=1,SIZE(A(1,:))
        DO ix=1,SIZE(A(1,:))
          IF(ix /= iy) THEN
            IF(A(ix,iy) /= 0.0_SRK .AND. A(iy,iy) /= 0.0_SRK) THEN
              tmp=A(ix,iy)/A(iy,iy)
              A(ix,:)=A(ix,:)-tmp*A(iy,:)
              invA(ix,:)=invA(ix,:)-tmp*invA(iy,:)
            ENDIF
          ELSE
            IF(ix /= SIZE(A(1,:))) THEN
              A(ix,:)=A(ix,:)+A(ix+1,:)
              invA(ix,:)=invA(ix,:)+invA(ix+1,:)
            ENDIF
            tmp=1.0_SRK/A(ix,ix)
            A(ix,:)=A(ix,:)*tmp
            invA(ix,:)=invA(ix,:)*tmp
          ENDIF
        ENDDO
      ENDDO
      
      DEALLOCATE(A)
    
    ENDSUBROUTINE direct_inv
!
!-------------------------------------------------------------------------------
!> @brief Returns the approximate block inverse of a matrix
!>
    SUBROUTINE ABI(L,U,F,block_dim,invA)
      REAL(SRK),INTENT(IN) :: L(:,:)
      REAL(SRK),INTENT(IN) :: U(:,:)
      REAL(SRK),INTENT(IN) :: F(:,:)
      INTEGER(SIK),INTENT(IN) :: block_dim
      REAL(SRK),INTENT(INOUT),ALLOCATABLE :: invA(:,:)
      
      INTEGER(SIK) :: i,ix,X,row
      INTEGER(SIK) :: ic,iu,id
      REAL(SRK),ALLOCATABLE :: L0(:,:),tmpM(:,:),invDU0(:)
      
      IF(ALLOCATED(invA)) DEALLOCATE(invA)
      ALLOCATE(invA(SIZE(L(:,1)),SIZE(L(1,:))))
      invA=0.0_SRK
      
      ALLOCATE(L0(block_dim,block_dim))
      ALLOCATE(tmpM(block_dim,block_dim))
      ALLOCATE(invDU0(block_dim))
      L0=0.0_SRK
      tmpM=0.0_SRK
      invDU0=0.0_SRK
      
      X=SIZE(L(:,1))/block_dim
      DO i=X,1,-1
        ic=(i-1)*block_dim+1
        iu=(i)*block_dim+1
        id=(i-2)*block_dim+1
        
        IF(i==1) THEN
        
          DO ix=X,1,-1
            IF(ix == i) THEN
              !diagonal block term
              DO row=1,block_dim
                invDU0(row)=U(ic+row-1,iu+row-1)
              ENDDO
              CALL dmatmul_left(invDU0,invA(iu:iu+block_dim-1,ic:ic+block_dim-1),tmpM)
              invA(ic:ic+block_dim-1,ic:ic+block_dim-1) = &
                F(ic:ic+block_dim-1,ic:ic+block_dim-1)+tmpM
            ELSEIF(ix == i+1) THEN
              !upper block term
              L0=L(ic:ic+block_dim-1,iu:iu+block_dim-1)
              DO row=1,block_dim
                invDU0(row)=U(ic+row-1,iu+row-1)
              ENDDO
              CALL dmatmul_left(invDU0,invA(iu:iu+block_dim-1,iu:iu+block_dim-1),tmpM)
              invA(ic:ic+block_dim-1,iu:iu+block_dim-1) = -tmpM
            ENDIF
          ENDDO
          
        ELSEIF(i==X) THEN
          
          DO ix=X,1,-1
            IF(ix == i) THEN
              !diagonal block term
              invA(ic:ic+block_dim-1,ic:ic+block_dim-1) = &
                F(ic:ic+block_dim-1,ic:ic+block_dim-1)
            ELSEIF(ix == i-1) THEN
              !lower block term)
              L0=L(ic:ic+block_dim-1,id:id+block_dim-1)
              invA(ic:ic+block_dim-1,id:id+block_dim-1)= &
                -MATMUL(MATMUL(invA(ic:ic+block_dim-1,ic:ic+block_dim-1),L0), &
                F(id:id+block_dim-1,id:id+block_dim-1))
            ENDIF
          ENDDO
          
        ELSE
          
          DO ix=X,1,-1
            IF(ix == i) THEN
              !diagonal block term)
              DO row=1,block_dim
                invDU0(row)=U(ic+row-1,iu+row-1)
              ENDDO
              CALL dmatmul_left(invDU0,invA(iu:iu+block_dim-1,ic:ic+block_dim-1),tmpM)
              invA(ic:ic+block_dim-1,ic:ic+block_dim-1) = &
                F(ic:ic+block_dim-1,ic:ic+block_dim-1) + tmpM
            ELSEIF(ix == i-1) THEN
              !lower block term
              L0=L(ic:ic+block_dim-1,id:id+block_dim-1)
              invA(ic:ic+block_dim-1,id:id+block_dim-1)=-MATMUL(invA(ic:ic+block_dim-1,ic:ic+block_dim-1), &
                MATMUL(L0,F(id:id+block_dim-1,id:id+block_dim-1)))
            ELSEIF(ix == i+1) THEN
              !upper block term
              L0=L(ic:ic+block_dim-1,id:id+block_dim-1)
              DO row=1,block_dim
                invDU0(row)=U(ic+row-1,iu+row-1)
              ENDDO
              CALL dmatmul_left(invDU0,invA(iu:iu+block_dim-1,iu:iu+block_dim-1),tmpM)
              invA(ic:ic+block_dim-1,iu:iu+block_dim-1)=-tmpM
            ENDIF
          ENDDO
          
        ENDIF
        
      ENDDO
    
    ENDSUBROUTINE ABI
!
!-------------------------------------------------------------------------------
END MODULE
