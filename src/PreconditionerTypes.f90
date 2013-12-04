
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

      INTEGER(SIK) :: X,Y,Z
      INTEGER(SIK) :: i,j,k
      INTEGER(SIK) :: index1,index2,index3,index4
      REAL(SRK),ALLOCATABLE :: soln(:),tmpvec(:)

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
                ALLOCATE(soln(SIZE(v%b)))
                ALLOCATE(tmpvec(pc%nPin*pc%nGrp))
                ALLOCATE(tmpmat(pc%nPin*pc%nGrp,pc%nPin*pc%nGrp))
                X=SQRT(REAL(pc%nPin))
                Y=SQRT(REAL(pc%nPin))
                Z=pc%nPlane
                ! Solve goes here.
                ! Requires F0(i,j,k), EW(i,j,k), NS(j,k) and the original BT(k) 
                ! i = x, j = y, k = z
                ! To solve M^(3)*z=v

                ! FORWARD SOLVE

                ! Step 1: solve M_1^(2)*z~_1=v_1
                index1=1
                index2=pc%nPin*pc%nGrp
                CALL M2solve(PC,soln(index1:index2),v%b(index1:index2),1)

                ! Step 2: solve M_k^(2)*z~_k=v_k-B_k*v_(k-1)
                  ! Requires B_k
                DO k=2,Z ! k=planes
                  ! solve M_k^(2)*z~_k=v_k-B_k*z~_(k-1)
                  index3=index1
                  index4=index2
                  index1=index2+1
                  index2=index2+pc%nPin*pc%nGrp
                  tmpmat=pc%L(index1:index2,index1:index2) ! B_k
                  CALL BLAS_dmatvec(pc%nGrp,1.0_SRK,tmpmat,soln(index3:index4),1.0_SRK,tmpvec)
                  v%b(index1:index2)=v%b(index1:index2)-tmpvec
                  CALL M2solve(PC,soln(index1:index2),v%b(index1:index2),k)
                ENDDO

                ! BACKWARD SOLVE

                ! Step 1: z_Z=z~_Z

                ! Step 2: solve M_k^(2)*z~_k=v_k-B_k*v_(k-1)
                  ! Requires B_k
                DO k=(Z-1),1,-1 ! k=planes
                  ! solve M_k^(2)*z~_k=v_k-B_k*z~_(k-1)
                  index3=index1
                  index4=index2
                  index1=index1-pc%nPin*pc%nGrp
                  index2=index1-1
                  tmpmat=pc%U(index1:index2,index1:index2) ! B_k
                  CALL BLAS_dmatvec(pc%nGrp,1.0_SRK,tmpmat,soln(index3:index4),1.0_SRK,tmpvec)
                  CALL M2solve(PC,tmpvec,tmpvec,k)
                  soln(index1:index2)=soln(index1:index2)+tmpvec
                ENDDO


              CLASS IS(PETScMatrixType)
                CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                  ' - PETSc Preconditioner Matrix type is not supported!')
              CLASS DEFAULT
                CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                  ' - Preconditioner Matrix type is not supported!')
            ENDSELECT
          CLASS IS(PETScVectorType)
            CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
              ' - PETSc Vector type is not support by this PreconditionerType.')
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
      
      INTEGER(SIK) :: ipl,ix,iy,row,col
      INTEGER(SIK) :: d_stt,c_stt,b1
      INTEGER(SIK) :: d0_stt,c0_stt
      INTEGER(SIK) :: d1_stt,c1_stt
      INTEGER(SIK) :: local_r,local_c
      INTEGER(SIK) :: X,Y,Z
      INTEGER(SIK) :: tmpind1,tmpind2
      INTEGER(SIK) :: rowstt,colstt
      REAL(SRK) :: tmpval
      LOGICAL(SBK) :: localalloc
      TYPE(ParamType) :: PL
      REAL(SRK),ALLOCATABLE :: tmp2DU(:,:),tmp2DinvM(:,:)
      REAL(SRK),ALLOCATABLE :: tmpT(:,:),tmpB(:,:),tmpM(:,:)
      REAL(SRK),ALLOCATABLE :: L2(:,:),U2(:,:),F2(:,:),tmpS(:,:),tmpN(:,:)
      REAL(SRK),ALLOCATABLE :: tmp1DU(:,:),tmp1DinvM(:,:),tmp1D(:,:)
      REAL(SRK),ALLOCATABLE :: L1(:,:),U1(:,:),F1(:,:),tmpE(:,:),tmpW(:,:)
      REAL(SRK),ALLOCATABLE :: tmp0DU(:,:),tmp0D(:,:),tmp0DinvM(:,:)

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
          X=SQRT(REAL(pc%nPin))
          Y=SQRT(REAL(pc%nPin))
          Z=pc%nPlane
          !variables for 3D
          ALLOCATE(tmp2DU(pc%nPin*pc%nGrp,pc%nPin*pc%nGrp))
          ALLOCATE(tmp2DinvM(pc%nPin*pc%nGrp,pc%nPin*pc%nGrp))
          ALLOCATE(tmpT(pc%nPin*pc%nGrp,pc%nPin*pc%nGrp))
          ALLOCATE(tmpB(pc%nPin*pc%nGrp,pc%nPin*pc%nGrp))
          ALLOCATE(tmpM(pc%nPin*pc%nGrp,pc%nPin*pc%nGrp))
          tmp2DU=0.0_SRK
          tmp2DinvM=0.0_SRK
          tmpT=0.0_SRK
          tmpB=0.0_SRK
          tmpM=0.0_SRK
          !variables for 2D
          ALLOCATE(L2(pc%nPin*pc%nGrp,pc%nPin*pc%nGrp))
          ALLOCATE(U2(pc%nPin*pc%nGrp,pc%nPin*pc%nGrp))
          ALLOCATE(F2(pc%nPin*pc%nGrp,pc%nPin*pc%nGrp))
          ALLOCATE(tmpS(X*pc%nGrp,X*pc%nGrp))
          ALLOCATE(tmpN(X*pc%nGrp,X*pc%nGrp))
          ALLOCATE(tmp1DU(X*pc%nGrp,X*pc%nGrp))
          ALLOCATE(tmp1DinvM(X*pc%nGrp,X*pc%nGrp))
          ALLOCATE(tmp1D(X*pc%nGrp,X*pc%nGrp))
          L2=0.0_SRK
          U2=0.0_SRK
          F2=0.0_SRK
          tmpS=0.0_SRK
          tmpN=0.0_SRK
          tmp1DU=0.0_SRK
          tmp1DinvM=0.0_SRK
          tmp1D=0.0_SRK
          !variables for 1D
          ALLOCATE(L1(X*pc%nGrp,X*pc%nGrp))
          ALLOCATE(U1(X*pc%nGrp,X*pc%nGrp))
          ALLOCATE(F1(X*pc%nGrp,X*pc%nGrp))
          ALLOCATE(tmpE(pc%nGrp,pc%nGrp))
          ALLOCATE(tmpW(pc%nGrp,pc%nGrp))
          ALLOCATE(tmp0D(pc%nGrp,pc%nGrp))
          ALLOCATE(tmp0DinvM(pc%nGrp,pc%nGrp))
          ALLOCATE(tmp0DU(pc%nGrp,pc%nGrp))
          L1=0.0_SRK
          U1=0.0_SRK
          F1=0.0_SRK
          tmpE=0.0_SRK
          tmpW=0.0_SRK
          tmp0D=0.0_SRK
          tmp0DinvM=0.0_SRK
          
          DO ipl=1,Z
            d_stt=(ipl-2)*pc%nPin*pc%nGrp+1
            c_stt=(ipl-1)*pc%nPin*pc%nGrp+1
            
            IF(ipl==1) THEN
              !form diagonal block of L
              DO row=c_stt,c_stt+pc%nPin*pc%nGrp-1
                DO col=c_stt,c_stt+pc%nPin*pc%nGrp-1
                  CALL pc%A%get(row,col,tmpval)
                  IF(tmpval /= 0.0_SRK) CALL pc%L%set(row,col,tmpval)
                ENDDO
              ENDDO
              !form diagonal block of U (Identity)
              DO row=c_stt,c_stt+pc%nPin*pc%nGrp-1
                CALL pc%U%set(row,row,1.0_SRK)
              ENDDO
            ELSE
              !pull T block into matrix
              local_r=1
              DO row=d_stt,d_stt+pc%nPin*pc%nGrp-1
                local_c=1
                DO col=c_stt,c_stt+pc%nPin*pc%nGrp-1
                  CALL pc%A%get(row,col,tmpval)
                  IF(tmpval /= 0.0_SRK) THEN
                    tmpT(local_r,local_c)=tmpval
                  ENDIF
                  local_c=local_c+1
                ENDDO
                local_r=local_r+1
              ENDDO
              !pull B block into matrix
              local_r=1
              DO row=c_stt,c_stt+pc%nPin*pc%nGrp-1
                local_c=1
                DO col=d_stt,d_stt+pc%nPin*pc%nGrp-1
                  CALL pc%A%get(row,col,tmpval)
                  IF(tmpval /= 0.0_SRK) THEN
                    tmpB(local_r,local_c)=tmpval
                  ENDIF
                  local_c=local_c+1
                ENDDO
                local_r=local_r+1
              ENDDO     
              
              !form diagonal block of L
              tmp2DU=MATMUL(tmpB,tmp2DinvM)
              tmp2DU=MATMUL(tmp2DU,tmpT)
              !form diagonal block of L
              local_r=1
              DO row=c_stt,c_stt+pc%nPin*pc%nGrp-1
                local_c=1
                DO col=c_stt,c_stt+pc%nPin*pc%nGrp-1
                  CALL pc%A%get(row,col,tmpval)
                  !IF(tmpval /= 0.0_SRK) CALL pc%L%set(row,col,tmpval)    !if enforcing same structure
                  IF(tmpval /= 0.0_SRK) THEN !.OR. &
!                    tmp2DU(local_r,local_c) /= 0.0_SRK) THEN
                    CALL pc%L%set(row,col,tmpval-tmp2DU(local_r,local_c))
                  ENDIF
                  local_c=local_c+1
                ENDDO
                local_r=local_r+1
              ENDDO
                
              !form lower block of L
              DO row=c_stt,c_stt+pc%nPin*pc%nGrp-1
                DO col=d_stt,d_stt+pc%nPin*pc%nGrp-1
                  CALL pc%A%get(row,col,tmpval)
                  IF(tmpval /= 0.0_SRK) THEN
                    CALL pc%L%set(row,col,tmpval)
                  ENDIF
                ENDDO
              ENDDO
              !form diagonal block of U (Identity)
              DO row=c_stt,c_stt+pc%nPin*pc%nGrp-1
                CALL pc%U%set(row,row,1.0_SRK)
              ENDDO
              !form upper block of U
              tmp2DU=MATMUL(tmp2DinvM,tmpT)
              local_r=1
              DO row=d_stt,d_stt+pc%nPin*pc%nGrp-1
                local_c=1
                DO col=c_stt,c_stt+pc%nPin*pc%nGrp-1
                  CALL pc%A%get(row,col,tmpval)
                  !IF(tmpval /= 0.0_SRK) CALL pc%L%set(row,col,tmpval)    !if enforcing same structure
                  IF(tmpval /= 0.0_SRK) THEN !.OR. &
!                    tmp2DU(local_r,local_c) /= 0.0_SRK) THEN
                    CALL pc%U%set(row,col,tmp2DU(local_r,local_c))
                  ENDIF
                  local_c=local_c+1
                ENDDO
                local_r=local_r+1
              ENDDO
              
            ENDIF
            
            !pull block M into matrix
            local_r=1
            DO row=c_stt,c_stt+pc%nPin*pc%nGrp-1
              local_c=1
              DO col=c_stt,c_stt+pc%nPin*pc%nGrp-1
                CALL pc%L%get(row,col,tmpval)
                IF(tmpval /= 0.0_SRK) THEN
                  tmpM(local_r,local_c)=tmpval
                ENDIF
                local_c=local_c+1
              ENDDO
              local_r=local_r+1
            ENDDO
            
            !LU M_k (2)  ...tmpM
            DO iy=1,Y
              d1_stt=(iy-2)*pc%Ngrp*Y+1
              c1_stt=(iy-1)*pc%Ngrp*Y+1
              
              IF(iy==1) THEN
                !set diagonal of L2
                DO row=c1_stt,c1_stt+Y*pc%nGrp-1
                  DO col=c1_stt,c1_stt+Y*pc%nGrp-1
                    tmpval=tmpM(row,col)
                    L2(row,col)=tmpval
                  ENDDO
                ENDDO
                !set U to diagonal block
                DO row=c1_stt,c1_stt+Y*pc%nGrp-1
                  U2(row,row)=1.0_SRK
                ENDDO
              ELSE
                !fill tmpS (can probably condense)
                tmpS=0.0_SRK
                local_r=1
                DO row=c1_stt,c1_stt+Y*pc%nGrp-1
                  local_c=1
                  DO col=d1_stt,d1_stt+Y*pc%nGrp-1
                    tmpS(local_r,local_c)=tmpM(row,col)
                    local_c=local_c+1
                  ENDDO
                  local_r=local_r+1
                ENDDO
                !fill tmpN (can probably condense)
                tmpN=0.0_SRK
                local_r=1
                DO row=d1_stt,d1_stt+Y*pc%nGrp-1
                  local_c=1
                  DO col=c1_stt,c1_stt+Y*pc%nGrp-1
                    tmpN(local_r,local_c)=tmpM(row,col)
                    local_c=local_c+1
                  ENDDO
                  local_r=local_r+1
                ENDDO
                !form diagonal block of L (can probably condense)
                tmp1DU=MATMUL(tmpS,tmp1DinvM)
                tmp1DU=MATMUL(tmp1DU,tmpN)
                local_r=1
                DO row=c1_stt,c1_stt+Y*pc%nGrp-1
                  local_c=1
                  DO col=c1_stt,c1_stt+Y*pc%nGrp-1
                    !IF(tmpval /= 0.0_SRK) CALL pc%L%set(row,col,tmpval)    !if enforcing same structure
                    IF(tmpval /= 0.0_SRK .OR. tmp1DU(local_r,local_c) /= 0.0_SRK) THEN
                      L2(row,col)=tmpM(row,col)-tmp1DU(local_r,local_c)
                    ENDIF
                    local_c=local_c+1
                  ENDDO
                  local_r=local_r+1
                ENDDO
                !form lower block of L (can probably condense)
                local_r=1
                DO row=c1_stt,c1_stt+Y*pc%nGrp-1
                  local_c=1
                  DO col=d1_stt,d1_stt+Y*pc%nGrp-1
                    !IF(tmpval /= 0.0_SRK) CALL pc%L%set(row,col,tmpval)    !if enforcing same structure
                    IF(tmpS(local_r,local_c) /= 0.0_SRK) THEN
                      L2(row,col)=tmpS(local_r,local_c)
                    ENDIF
                    local_c=local_c+1
                  ENDDO
                  local_r=local_r+1
                ENDDO
                !form upper block of U (can probably condense)
                tmp1DU=MATMUL(tmp1DinvM,tmpN)
                local_r=1
                DO row=d1_stt,d1_stt+Y*pc%nGrp-1
                  local_c=1
                  DO col=c1_stt,c1_stt+Y*pc%nGrp-1
                    !IF(tmpval /= 0.0_SRK) CALL pc%L%set(row,col,tmpval)    !if enforcing same structure
                    IF(tmpval /= 0.0_SRK .OR. tmp1DU(local_r,local_c) /= 0.0_SRK) THEN
                      U2(row,col)=tmp1DU(local_r,local_c)
                    ENDIF
                    local_c=local_c+1
                  ENDDO
                  local_r=local_r+1
                ENDDO
                !form diagonal block of U
                tmp1DU=MATMUL(tmp1DinvM,tmpN)
                DO row=c1_stt,c1_stt+Y*pc%nGrp-1
                  U2(row,row)=1.0_SRK
                ENDDO
                
              ENDIF
              
              tmp1D=L2(c1_stt:c1_stt+Y*pc%nGrp-1,c1_stt:c1_stt+Y*pc%nGrp-1)
              
              DO ix=1,X
                             
                d0_stt=(ix-2)*pc%Ngrp+1
                c0_stt=(ix-1)*pc%Ngrp+1
              
                IF(ix==1) THEN
                  !set diagonal of L
                  DO row=c0_stt,c0_stt+pc%nGrp-1
                    DO col=c0_stt,c0_stt+pc%nGrp-1
                      L1(row,col)=tmp1D(row,col)
                    ENDDO
                  ENDDO
                  !set U to diagonal block
                  DO row=c0_stt,c0_stt+pc%nGrp-1
                    U1(row,row)=1.0_SRK
                  ENDDO
                ELSE
                  !fill tmpW
                  local_r=1
                  DO row=c0_stt,c0_stt+pc%nGrp-1
                    local_c=1
                    DO col=d0_stt,d0_stt+pc%nGrp-1
                      tmpW(local_r,local_c)=tmp1D(row,col)
                      local_c=local_c+1
                    ENDDO
                    local_r=local_r+1
                  ENDDO
                  !fill tmpE
                  local_r=1
                  DO row=d0_stt,d0_stt+pc%nGrp-1
                    local_c=1
                    DO col=c0_stt,c0_stt+pc%nGrp-1
                      tmpE(local_r,local_c)=tmp1D(row,col)
                      local_c=local_c+1
                    ENDDO
                    local_r=local_r+1
                  ENDDO
                  !set diagonal of L
                  tmp0DU=MATMUL(tmpW,tmp0DinvM)
                  tmp0DU=MATMUL(tmp0DU,tmpE)
                  local_r=1
                  DO row=c0_stt,c0_stt+pc%nGrp-1
                    local_c=1
                    DO col=c0_stt,c0_stt+pc%nGrp-1
                      L1(row,col)=tmp1D(row,col)-tmp0DU(local_r,local_c)
                      local_c=local_c+1
                    ENDDO
                    local_r=local_r+1
                  ENDDO
                  !set lower block of L
                  local_r=1
                  DO row=c0_stt,c0_stt+pc%nGrp-1
                    local_c=1
                    DO col=d0_stt,d0_stt+pc%nGrp-1
                      L1(row,col)=tmpW(local_r,local_c)
                      local_c=local_c+1
                    ENDDO
                    local_r=local_r+1
                  ENDDO
                  !set upper block of U
                  tmp0DU=MATMUL(tmp0DinvM,tmpE)
                  local_r=1
                  DO row=d0_stt,d0_stt+pc%nGrp-1
                    local_c=1
                    DO col=c0_stt,c0_stt+pc%nGrp-1
                      U1(row,col)=tmp0DU(local_r,local_c)
                      local_c=local_c+1
                    ENDDO
                    local_r=local_r+1
                  ENDDO               
                  !set diagonal block of U
                  DO row=c0_stt,c0_stt+pc%nGrp-1
                    U1(row,row)=1.0_SRK
                  ENDDO
                ENDIF
                
                tmp0D=L1(c0_stt:c0_stt+pc%nGrp-1,c0_stt:c0_stt+pc%nGrp-1)
                
                !determine 0D inverse (always direct for 0D)
                CALL direct_inv(tmp0D,tmp0DinvM)
                F1(c0_stt:c0_stt+pc%nGrp-1,c0_stt:c0_stt+pc%nGrp-1)=tmp0DinvM
                tmpind1=(ipl-1)*pc%nPin+(iy-1)*Y+ix
                PC%F0(tmpind1,:,:)=tmp0DinvM

              ENDDO
              
              !determine 1D inverse (eventually ABI)
              !CALL direct_inv(tmp1D,tmp1DinvM)
              CALL ABI(L1,U1,F1,pc%Ngrp,tmp1DinvM)
              F2(c1_stt:c1_stt+X*pc%nGrp-1,c1_stt:c1_stt+X*pc%nGrp-1)=tmp1DinvM
              !store for apply
              tmpind1=(ipl-1)*Y+iy
              rowstt=0
              colstt=pc%nGrp
              DO row=1,pc%nGrp*(X-1)
                PC%E(tmpind1,row)=tmp1D(rowstt+row,colstt+row)
              ENDDO
              rowstt=pc%nGrp
              colstt=0
              DO row=1,pc%nGrp*(X-1)
                PC%W(tmpind1,row)=tmp1D(rowstt+row,colstt+row)
              ENDDO
                
            ENDDO
            
            !determine 2D inverse (eventually ABI)
            !CALL direct_inv(tmpM,tmp2DinvM)
            CALL ABI(L2,U2,F2,X*pc%Ngrp,tmp2DinvM)
            rowstt=0
            colstt=X*pc%nGrp
            DO row=1,pc%nGrp*X*(X-1)
              PC%N(ipl,row)=tmpM(rowstt+row,colstt+row)
            ENDDO
            rowstt=X*pc%nGrp
            colstt=0
            DO row=1,pc%nGrp*X*(X-1)
              PC%S(ipl,row)=tmpM(rowstt+row,colstt+row)
            ENDDO
            
            
          ENDDO
          DEALLOCATE(tmp2DU,tmp2DinvM)
          
        ENDIF
      ENDIF

      IF(localalloc) DEALLOCATE(ePreCondType)
    ENDSUBROUTINE setup_BILU_PreCondtype
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
      
      INTEGER(SIK) :: i,ix,X
      INTEGER(SIK) :: ic,iu,id
      REAL(SRK),ALLOCATABLE :: L0(:,:),DinvU0(:,:)
      
      IF(ALLOCATED(invA)) DEALLOCATE(invA)
      ALLOCATE(invA(SIZE(L(:,1)),SIZE(L(1,:))))
      invA=0.0_SRK
      
      ALLOCATE(L0(block_dim,block_dim))
      ALLOCATE(DinvU0(block_dim,block_dim))
      L0=0.0_SRK
      DinvU0=0.0_SRK
      
      X=SIZE(L(:,1))/block_dim
      DO i=X,1,-1
        ic=(i-1)*block_dim+1
        iu=(i)*block_dim+1
        id=(i-2)*block_dim+1
        
        IF(i==1) THEN
        
          DO ix=X,1,-1
            IF(ix == i) THEN
              !diagonal block term
              DinvU0=U(ic:ic+block_dim-1,iu:iu+block_dim-1)
              invA(ic:ic+block_dim-1,ic:ic+block_dim-1) = &
                F(ic:ic+block_dim-1,ic:ic+block_dim-1)+MATMUL(DinvU0,invA(iu:iu+block_dim-1,ic:ic+block_dim-1))
            ELSEIF(ix == i+1) THEN
              !upper block term
              L0=L(ic:ic+block_dim-1,iu:iu+block_dim-1)
              DinvU0=U(ic:ic+block_dim-1,iu:iu+block_dim-1)
              invA(ic:ic+block_dim-1,iu:iu+block_dim-1) = &
                -MATMUL(DinvU0,invA(iu:iu+block_dim-1,iu:iu+block_dim-1))
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
              DinvU0=U(ic:ic+block_dim-1,iu:iu+block_dim-1)
              invA(ic:ic+block_dim-1,ic:ic+block_dim-1) = &
                F(ic:ic+block_dim-1,ic:ic+block_dim-1) + MATMUL(DinvU0,invA(iu:iu+block_dim-1,ic:ic+block_dim-1))
            ELSEIF(ix == i-1) THEN
              !lower block term
              L0=L(ic:ic+block_dim-1,id:id+block_dim-1)
              invA(ic:ic+block_dim-1,id:id+block_dim-1)=-MATMUL(invA(ic:ic+block_dim-1,ic:ic+block_dim-1), &
                MATMUL(L0,F(id:id+block_dim-1,id:id+block_dim-1)))
            ELSEIF(ix == i+1) THEN
              !upper block term
              L0=L(ic:ic+block_dim-1,id:id+block_dim-1)
              DinvU0=U(ic:ic+block_dim-1,iu:iu+block_dim-1)
              invA(ic:ic+block_dim-1,iu:iu+block_dim-1)=-MATMUL(DinvU0,invA(iu:iu+block_dim-1,iu:iu+block_dim-1))
            ENDIF
          ENDDO
          
        ENDIF
        
      ENDDO
    
    ENDSUBROUTINE ABI
!
!-------------------------------------------------------------------------------
!> @brief solves M_k^(2)*x=b
!> requires S~_(j,k), N~_(j,k), W--(i,j,k), E--(i,j,k), F0(i,j,k)
!>   all in PC
!>
    SUBROUTINE M2solve(PC,x,b,k)
      CHARACTER(LEN=*),PARAMETER :: myName='M2solve'
      CLASS(BILU_PrecondType),INTENT(IN) :: PC
      REAL(SRK),INTENT(INOUT) :: x(:) ! length = pc%npins*pc%nGrp
      REAL(SRK),INTENT(IN) :: b(:)    ! length = pc%npins*pc%nGrp
      INTEGER(SIK),INTENT(IN) :: k

      REAL(SRK),ALLOCATABLE :: tmpvec(:),tmpmat(:,:)
      INTEGER(SIK) :: i,j
      INTEGER(SIK) :: X,Y
      INTEGER(SIK) :: index1,index2,index3,index4

      X=SQRT(REAL(pc%nPin))
      Y=SQRT(REAL(pc%nPin))
      Z=pc%nPlane

      ! Step 1: FORWARD SOLVE
      ! j=1

      index1=1
      index2=X*pc%nGrp

      CALL M1solve(PC,x(index1:index2),b(index1:index2),1,k)

      ! j=2,Y
      DO j=2,Y
        index3=index1
        index4=index2
        index1=index2+1
        index2=index2+X*pc%nGrp

        tmpmat=S(j,k)
        CALL BLAS_matvec(X*pc%nGrp,1.0_SRK,tmpmat,x(index3:index4),1.0_SRK,tmpvec)
        b(index1:index2)=b(index1:index2)-tmpvec

        CALL M1solve(PC,x(index1:index2),b(index1:index2),j,k)

      ENDDO

      ! Step 2: BACKWARD SOLVE

      ! j=Y - no change necessary

      ! j=(Y-1),1
      DO j=(Y-1),1,-1
        index3=index1
        index4=index2
        index1=index1-X*pc%nGrp
        index2=index1-1

        tmpmat=N(j,k)
        CALL BLAS_matvec(X*pc%nGrp,1.0_SRK,tmpmat,x(index3:index4),1.0_SRK,tmpvec)

        CALL M1solve(PC,tmpvec,tmpvec,j,k)

        x(index1:index2)=x(index1:index2)+tmpvec

      ENDDO

    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief solves M_j,k^(1)*x=b
!> requires S~_(j,k), N~_(j,k), W--(i,j,k), E--(i,j,k), F0(i,j,k)
!>   all in PC
!>
    SUBROUTINE M1solve(PC,x,b,j,k)
      CHARACTER(LEN=*),PARAMETER :: myName='M2solve'
      CLASS(BILU_PrecondType),INTENT(IN) :: PC
      CLASS(Vectortype),INTENT(INOUT) :: x
      CLASS(Vectortype),INTENT(IN) :: b
      INTEGER(SIK),INTENT(IN) :: j
      INTEGER(SIK),INTENT(IN) :: k

      REAL(SRK) :: tmpmat(pc%nGrp,pc%nGrp),tmpvec(pc%nGrp),tmpvec2(pc%nGrp)
      INTEGER(SIK) :: i,ig,index1,index2
      INTEGER(SIK) :: X,Y,Z

      X=SQRT(REAL(pc%nPin))
      Y=SQRT(REAL(pc%nPin))
      Z=pc%nPlane

             ! earlier planes   earlier rows
      index1=(k-1)*Y*X       +  (j-1)*X
      index1g=index1*pc%nGrp

      ! For E/W
      index2=(k-1)*Y+(j-1)

      ! Step 1: FORWARD SOLVE (obtain x~)

      ! i=1
      tmpmat=pc%F0(index1+1,:,:)
      tmpvec=b((index1g+1):(index1g+pc%nGrp))
                     ! size    alpha   A
      CALL BLAS_matvec(pc%nGrp,1.0_SRK,tmpmat,tmpvec,1.0_SRK,x((index1g+1):(index1g+pc%nGrp)))

      ! rest
      DO i=2,X
        tmpmat=pc%F0(index1+i,:,:)
        tmpvec=b((index1g+1):(index1g+pc%nGrp))
        tmpvec=tmpvec-pc%W(index2,((i-1)*pc%nGrp+1):(i*pc%nGrp))*x((index1g+1):(index1g+pc%nGrp))
        index1g=index1g+pc%nGrp ! increase index1g by nGrp for each i
                       ! size    alpha   A
        CALL BLAS_matvec(pc%nGrp,1.0_SRK,tmpmat,tmpvec,1.0_SRK,x((index1g+1):(index1g+pc%nGrp)))
      ENDDO

      ! Step 2: BACKWARD SOLVE

      ! i=X
      ! Do nothing because x_x,j,k=x~_x

      ! rest
      DO i=(X-1),1,-1
        tmpmat=pc%F0(index1+i,:,:)
        tmpvec=pc%E(index2,((i-1)*pc%nGrp+1):(i*pc%nGrp))*x((index1g+1):(index1g+pc%nGrp))
        CALL BLAS_matvec(pc%nGrp,1.0_SRK,tmpmat,tmpvec,1.0_SRK,tmpvec2)
        index1g=index1g-pc%nGrp ! increase index1g by nGrp for each i
        x((index1g+1):(index1g+pc%nGrp))=x((index1g+1):(index1g+pc%nGrp))-tmpvec2
      ENDDO

    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
END MODULE
