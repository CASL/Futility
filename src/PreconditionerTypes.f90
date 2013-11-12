
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
      PROCEDURE,PASS :: apply => apply_LU_PreCondType
  ENDTYPE LU_PreCondType

  TYPE,EXTENDS(LU_PreCondType) :: ILU_PreCondType
    CONTAINS
      PROCEDURE,PASS :: setup => setup_ILU_PreCondType
  ENDTYPE ILU_PreCondType
  
  TYPE,EXTENDS(LU_PreCondType) :: BILU_PreCondType
    INTEGER(SIK) :: nPlane
    INTEGER(SIK) :: nPin
    INTEGER(SIK) :: nGrp
    INTEGER(SIK) :: BILUType  !for 2D, 3D and other variants
    CONTAINS
      PROCEDURE,PASS :: setup => setup_BILU_PreCondType
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

          ! This might not be necessary here, but not sure
          SELECTTYPE(mat => PC%A)
            CLASS IS(SparseMatrixType)
              ALLOCATE(SparseMatrixType :: PC%L)
              ALLOCATE(SparseMatrixType :: PC%U)
              PC%isInit=.TRUE.
            CLASS IS(PETScMatrixType)
              !allocate L and U
              ALLOCATE(PETScMatrixType :: PC%L)
              ALLOCATE(PETScMatrixType :: PC%U)
              
              !initialize L and U
              SELECTTYPE(U => PC%U); TYPE IS(PETScMatrixType)
                SELECTTYPE(L => PC%L); TYPE IS(PETScMatrixType)
                  !to be supported at some point soon
                  
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
      CALL PC%U%clear()
      CALL PC%L%clear()
      DEALLOCATE(PC%U)
      DEALLOCATE(PC%L)
      PC%isInit=.FALSE.

    ENDSUBROUTINE clear_LU_PreCondtype
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
    SUBROUTINE apply_LU_PreCondType(PC,v)
      CLASS(LU_PrecondType),INTENT(INOUT) :: PC
      CLASS(Vectortype),INTENT(INOUT) :: v

      CHARACTER(LEN=*),PARAMETER :: myName='apply_LU_PreCondType'
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
        ! Perform U^-1 * L^-1 * v
      ENDIF
    ENDSUBROUTINE apply_LU_PreCondType
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
          ! This might not be necessary here, but not sure
          SELECTTYPE(mat => PC%A)
            CLASS IS(SparseMatrixType)
              SELECTTYPE(U => PC%U); TYPE IS(SparseMatrixType)
                SELECTTYPE(L => PC%L); TYPE IS(SparseMatrixType)
                  ! Loop over A to get initialization data for L and U
                  j=0
                  nU=0; nL=0  !number of rows
                  nnzU=0; nnzL=0 !number of non-zero elements
                  DO row=1,SIZE(mat%ia)-1
                    DO i=1,mat%ia(row+1)-mat%ia(row)
                      j=j+1
                      col=mat%ja(j)
                      ! This may be redundant since mat is sparse, but be safe for now
                      CALL mat%get(row,col,val1)
                      IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) THEN
                        IF(col == row) THEN
                          nnzU=nnzU+1 ! This location is in U
                        ELSEIF(col > row) THEN
                          nnzU=nnzU+1 !This location is in U
                        ELSE
                          nnzL=nnzL+1 !This location is in L
                        ENDIF
                      ENDIF
                    ENDDO
                    nnzL=nnzL+1 ! Account for 1's on diagonal of L
                  ENDDO
                  
                  ! Initialize L and U
                  nU=mat%n
                  nL=mat%n
                  CALL PL%add('MatrixType->n',nU)
                  CALL PL%add('MatrixType->nnz',nnzU)
                  CALL U%init(PL)
                  CALL PL%set('MatrixType->n',nL)
                  CALL PL%set('MatrixType->nnz',nnzL)
                  CALL L%init(PL)
                  CALL PL%clear()
                    
                  ! Make sure initialization worked
                  IF(.NOT.(U%isInit)) THEN
                    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                      ' - In LU decomposition, U was not properly initialized!')
                  ELSEIF(.NOT.(L%isInit)) THEN
                    CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                      ' - In LU decomposition, L was not properly initialized!')
                  ! Now loop through A again and set values of L and U
                  ELSE
                    j=0
                    val1=0.0_SRK
                    val2=0.0_SRK
                    DO row=2,SIZE(mat%ia)-1
                      DO i=1,mat%ia(row+1)-mat%ia(row)
                        j=j+1
                        col=mat%ja(j)
                        IF(col > row-1) EXIT
                        CALL mat%get(row,col,val1)
                        CALL mat%get(col,col,val2)
                        val2=val1/val2
                        CALL L%setShape(row,col,val2)
                        DO k=i+1,mat%ia(row+1)-mat%ia(row)
                          col2=mat%ja(j-i+k)
                          CALL mat%get(row,col2,val1)
                          IF(.NOT.(val1 .APPROXEQA. 0.0_SRK)) THEN
                            IF(col2 < row) THEN
                              CALL L%get(col,col2,val3)
                              CALL L%setShape(row,col2,val1-val2*val3)
                            ELSE
                              CALL U%get(col,col2,val3)
                              CALL U%setShape(row,col2,val1-val2*val3)
                            ENDIF
                          ENDIF
                        ENDDO
                      ENDDO
                      CALL L%setShape(row,row,1.0_SRK)
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
      
      INTEGER(SIK) :: ipl,row,col
      INTEGER(SIK) :: d_stt,c_stt,u_stt,b1
      INTEGER(SIK) :: local_r,local_c
      REAL(SRK) :: tmpval
      LOGICAL(SBK) :: localalloc
      TYPE(ParamType) :: PL
      REAL(SRK),ALLOCATABLE :: tmp2DU(:,:),tmp2DinvM(:,:)
      REAL(SRK),ALLOCATABLE :: tmpT(:,:),tmpB(:,:),tmpM(:,:)

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
          
          WRITE(*,*) 'hi from setup',pc%nPlane,pc%nPin,pc%nGrp
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
          
          DO ipl=1,pc%nPlane
          
            d_stt=(ipl-2)*pc%nPin*pc%nGrp+1
            c_stt=(ipl-1)*pc%nPin*pc%nGrp+1
            
            IF(ipl==1) THEN
              !pull block M into matrix
              local_r=1
              DO row=c_stt,c_stt+pc%nPin*pc%nGrp-1
                local_c=1
                DO col=c_stt,c_stt+pc%nPin*pc%nGrp-1
                  CALL pc%A%get(row,col,tmpval)
                  IF(tmpval /= 0.0_SRK) THEN
                    tmpM(local_r,local_c)=tmpval
                  ENDIF
                  local_c=local_c+1
                ENDDO
                local_r=local_r+1
              ENDDO
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
              
              !determine inverse of M (either ABI or direct)
              CALL direct_inv(tmpM,tmp2DinvM)
              
!              !print some things for debugging
!              WRITE(*,*) "M: "
!              DO row=1,pc%nPin*pc%nGrp
!                WRITE(*,'(I4,9F15.5)') row,tmpM(row,:)
!              ENDDO
!              WRITE(*,*) "invM: "
!              DO row=1,pc%nPin*pc%nGrp
!                WRITE(*,'(I4,9F15.5)') row,tmp2DinvM(row,:)
!              ENDDO
!              WRITE(*,*) "B: "
!              DO row=1,pc%nPin*pc%nGrp
!                WRITE(*,'(I4,9F15.5)') row,tmpB(row,:)
!              ENDDO
!              WRITE(*,*) "T: "
!              DO row=1,pc%nPin*pc%nGrp
!                WRITE(*,'(I4,9F15.5)') row,tmpT(row,:)
!              ENDDO
              
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
                  IF(tmpval /= 0.0_SRK .OR. &
                    tmp2DU(local_r,local_c) /= 0.0_SRK) THEN
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
                  !IF(tmpval /= 0.0_SRK) CALL pc%L%set(row,col,tmpval)    !if enforcing same structure
                  IF(tmpval /= 0.0_SRK .OR. &
                    tmp2DU(local_r,local_c) /= 0.0_SRK) THEN
                    CALL pc%U%set(row,col,tmp2DU(local_r,local_c))
                  ENDIF
                  local_c=local_c+1
                ENDDO
                local_r=local_r+1
              ENDDO
              
            ENDIF
          
          ENDDO
          
          DEALLOCATE(tmp2DU,tmp2DinvM)
          
        ENDIF
      ENDIF

      IF(localalloc) DEALLOCATE(ePreCondType)
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Returns the inverse of a matrix
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
END MODULE
