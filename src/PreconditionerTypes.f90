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
  PUBLIC :: BILU_PreCondType
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
      ELSE
        IF(.NOT. PRESENT(A) .OR. .NOT.(ALLOCATED(A))) THEN
          CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
            ' - Matrix being used for LU Preconditioner is not allocated!')
        ELSE
          IF(.NOT.(A%isInit)) THEN
            CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
              ' - Matrix being used for LU Preconditioner is not initialized!')
          ELSE
            thisPC%A => A

              !allocate additional storage for BILU3D
              SELECTTYPE(thisPC)
                TYPE IS(BILU_PrecondType)
                  X=INT(SQRT(REAL(thisPC%nPin)))
                  ALLOCATE(thisPC%F0(thisPC%nPlane*thisPC%nPin,thisPC%nGrp,thisPC%nGrp))
                  ALLOCATE(thisPC%E(thisPC%nPlane*X,thisPC%nGrp*(X-1)))
                  ALLOCATE(thisPC%W(thisPC%nPlane*X,thisPC%nGrp*(X-1)))
                  ALLOCATE(thisPC%N(thisPC%nPlane,thisPC%nGrp*X*(x-1)))
                  ALLOCATE(thisPC%S(thisPC%nPlane,thisPC%nGrp*X*(x-1)))
                  thisPC%F0=0.0_SRK
                  thisPC%E=0.0_SRK
                  thisPC%W=0.0_SRK
                  thisPC%N=0.0_SRK
                  thisPC%S=0.0_SRK
              ENDSELECT

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
                    TYPE IS(BILU_PrecondType)
                      ALLOCATE(SparseMatrixType :: thisPC%L)
                      ALLOCATE(SparseMatrixType :: thisPC%U)

                      ! Loop over A to get initialization data for L and U
                      nnzU=0
                      nnzL=0
                      DO row=1,SIZE(mat%ia)-1
                        DO j=mat%ia(row),mat%ia(row+1)-1
                          col=mat%ja(j)
                          CALL mat%get(row,col,val)
                          IF(.NOT.(val .APPROXEQA. 0.0_SRK)) THEN
                            IF(col > row+thisPC%Ngrp*thisPC%Npin-1) THEN
                              nnzU=nnzU+1
                            ENDIF
                          ENDIF
                        ENDDO
                      ENDDO
                      nnzL=mat%nnz-nnzU
                      nnzU=nnzU+mat%n
                  ENDSELECT

                  SELECTTYPE(thisPC); TYPE IS(BILU_PrecondType)
                    ! Initialize L and U
                    nU=mat%n
                    nL=mat%n
                    CALL PL%add('MatrixType->n',nU)
                    CALL PL%add('MatrixType->nnz',nnzU)
                    CALL thisPC%U%init(PL)
                    CALL PL%set('MatrixType->n',nL)
                    CALL PL%set('MatrixType->nnz',nnzL)
                    CALL thisPC%L%init(PL)
                    CALL PL%clear()

                    SELECTTYPE(L => thisPC%L); TYPE IS(SparseMatrixType)
                      SELECTTYPE(U => thisPC%U); TYPE IS(SparseMatrixType)
                        ! Set the shape
                        DO row=1,SIZE(mat%ia)-1
                          DO j=mat%ia(row),mat%ia(row+1)-1
                            col=mat%ja(j)
                            ! This may be redundant since mat is sparse, but be safe for now
                            CALL mat%get(row,col,val)
                            IF(.NOT.(val .APPROXEQA. 0.0_SRK)) THEN
                              IF(col >= row+thisPC%Ngrp*thisPC%Npin-1) THEN
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
                    IF(thisPC%L%isInit .AND. thisPC%U%isInit) THEN
                      thisPC%isInit=.TRUE.
                    ELSEIF(.NOT.(thisPC%L%isInit)) THEN
                      CALL ePreCondtype%raiseError('Incorrect input to '//modName//'::'//myName// &
                        ' - In LU Preconditioner initialization, L was not properly initialized')
                    ELSE
                      CALL ePreCondtype%raiseError('Incorrect input to '//modName//'::'//myName// &
                        ' - In LU Preconditioner initialization, U was not properly initialized')
                    ENDIF
                  ENDSELECT
                CLASS IS(PETScMatrixType)
                  !allocate L and U
                  ALLOCATE(PETScMatrixType :: thisPC%L)
                  ALLOCATE(PETScMatrixType :: thisPC%U)

                  !initialize L and U
                  SELECTTYPE(U => thisPC%U); TYPE IS(PETScMatrixType)
                    SELECTTYPE(L => thisPC%L); TYPE IS(PETScMatrixType)
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

                  thisPC%isInit=.TRUE.
                CLASS DEFAULT
                  CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                    ' - LU Preconditioners are not supported for input matrix type!')
            ENDSELECT
          ENDIF
        ENDIF
      ENDIF
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
      SELECTTYPE(thisPC); TYPE IS(BILU_PrecondType)
        IF(ALLOCATED(thisPC%U)) THEN
          CALL thisPC%U%clear()
          DEALLOCATE(thisPC%U)
        ENDIF
        IF(ALLOCATED(thisPC%L)) THEN
          CALL thisPC%L%clear()
          DEALLOCATE(thisPC%L)
        ENDIF
        IF(ALLOCATED(thisPC%F0)) DEALLOCATE(thisPC%F0)
        IF(ALLOCATED(thisPC%E)) DEALLOCATE(thisPC%E)
        IF(ALLOCATED(thisPC%W)) DEALLOCATE(thisPC%W)
        IF(ALLOCATED(thisPC%N)) DEALLOCATE(thisPC%N)
        IF(ALLOCATED(thisPC%S)) DEALLOCATE(thisPC%S)
      ENDSELECT
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
!-------------------------------------------------------------------------------
!> @brief Applies the Block LU Decomposition
!> @param thisPC the preconditioner
!> @param v the vector to operate on
!>
    SUBROUTINE apply_BILU_PreCondType(thisPC,v)
      CHARACTER(LEN=*),PARAMETER :: myName='apply_BILU_PreCondType'
      CLASS(BILU_PrecondType),INTENT(INOUT) :: thisPC
      CLASS(Vectortype),INTENT(INOUT) :: v

      INTEGER(SIK) :: i,j,k,X,Y,Z
      INTEGER(SIK) :: iz,izg,izgp,izy,izyx,izyg,izygp,izyxg,izyxgp,iy,iyg,iym1g
      INTEGER(SIK) :: dzg,dyg,dxg,dz,dy
      REAL(SRK),ALLOCATABLE :: soln(:),soln_prevj(:),soln_prevk(:)
      REAL(SRK),ALLOCATABLE :: tmpTB(:)

      IF(.NOT.(thisPC%isInit)) THEN
        CALL ePreCondType%raiseError(modName//'::'//myName// &
          ' - Preconditioner is not initialized.')
      ELSEIF(.NOT.(v%isInit)) THEN
        CALL ePreCondType%raiseError(modName//'::'//myName// &
          ' - VectorType is not initialized.')
      ELSE
        SELECTTYPE(v)
          CLASS IS(RealVectorType)
            SELECTTYPE(L => thisPC%L)
              CLASS IS(SparseMatrixType)


                X=INT(SQRT(REAL(thisPC%nPin)))
                Y=INT(SQRT(REAL(thisPC%nPin)))
                Z=thisPC%nPlane

                ALLOCATE(soln(SIZE(v%b)))
                soln=0.0_SRK

                IF(.FALSE.) THEN

                CALL solve_M3(thisPC,soln,v%b)

                ELSE

                dz=X*Y
                dzg=dz*thisPC%nGrp
                dy=Y
                dyg=X*thisPC%nGrp
                dxg=thisPC%nGrp

                ALLOCATE(soln_prevj(dyg))
                ALLOCATE(soln_prevk(dzg))
                ALLOCATE(tmpTB(dzg))

                ! Step 1: FORWARD SOLVE

                izg=0
                DO k=1,Z
                  iz=k ! = k
                  izgp=izg
                  izg=(k-1)*dzg

                  IF(k > 1) THEN
                    ! Get bottom matrix B_k from the A matrix
!                    tmpTB=0.0_SRK
                    DO i=1,dzg
                                   ! Might need to flip these two.
                      CALL thisPC%A%get(izgp+i,izg+i,tmpTB(i))
                      tmpTB(i)=-1.0_SRK*tmpTB(i)
                    ENDDO
                    ! b=b-B_k*U(k-1)
                    CALL BLAS_axpy(tmpTB,soln((izgp+1):(izgp+dzg)),v%b((izg+1):(izg+dzg)))!soln_prevk)
                    !v%b((izg+1):(izg+dzg))=v%b((izg+1):(izg+dzg))-soln_prevk
                  ENDIF

                  ! INSERT M(2) SOLVE HERE
                  ! Step 1: FORWARD SOLVE
                  izyg=(k-1)*dzg
                  DO j=1,Y
                    iy=(k-1)*dy+j
                    izygp=izyg
                    izy=(k-1)*dz+(j-1)*dy ! which goes from j to j + X
                    izyg=(k-1)*dzg+(j-1)*dyg ! which goes from j to j + dyg

                    IF(j > 1) THEN
                      iym1g=(j-2)*dyg
                      CALL BLAS_axpy(-1.0_SRK*thisPC%S(k,(iym1g+1):(iym1g+dyg)), &
                        soln((izygp+1):(izygp+dyg)),v%b((izyg+1):(izyg+dyg)))
                    ENDIF

                    ! INSERT M(1) SOLVE HERE. Uses soln, v%b, and F0 and W.
                    ! Step 1: FORWARD SOLVE (obtain x~)

                    ! i=1, soln(1)=F0(1,j,k)*b(1,j,k)
                    i=1
                    izyx=izy+i
                    izyxg=izyg+(i-1)*dxg
                    ! soln should aready be zero
                    CALL BLAS_matvec(thisPC%F0(izyx,:,:),v%b((izyxg+1):(izyxg+dxg)), &
                      soln((izyxg+1):(izyxg+dxg)))

                    ! i=2,X
                    DO i=2,X
                      izyx=izy+i
                      izyxgp=izyxg
                      izyxg=izyg+(i-1)*dxg ! increase izyxg by nGrp for each i
                      ! Overwrite b because not used again.
                      ! b(i,j,k)-W(i,j,k)*soln(i-1)
                      CALL BLAS_axpy(-1.0_SRK*thisPC%W(iy,((i-2)*dxg+1):((i-1)*dxg)), &
                        soln((izyxgp+1):(izyxgp+dxg)),v%b((izyxg+1):(izyxg+dxg)))
                      ! F0(i,j,k)*(b(i,j,k)-W(i,j,k)*soln(i-1))
                      ! soln should be zero
                      CALL BLAS_matvec(thisPC%F0(izyx,:,:),v%b((izyxg+1):(izyxg+dxg)),soln((izyxg+1):(izyxg+dxg)))
                    ENDDO


                    ! Step 2: BACKWARD SOLVE

                    ! i=X
                    ! Do nothing (already solved)

                    ! rest
                    DO i=(X-1),1,-1
                      izyxgp=izyxg
                      izyx=izy+i
                      izyxg=izyg+(i-1)*dxg ! increase izyxg by nGrp for each i
                      ! Overwrite b once more because unused
                      v%b((izyxg+1):(izyxg+dxg))=0.0_SRK
                      ! E(i,j)*soln(i+1,j,k)
                      CALL BLAS_axpy(thisPC%E(iy,((i-1)*dxg+1):(i*dxg)), &
                        soln((izyxgp+1):(izyxgp+dxg)),v%b((izyxg+1):(izyxg+dxg)))
                      ! ans(i,j,k)=ans(i,j,k)-F0(i,j,k)*(E(i,j)*soln(i+1,j,k))
                      CALL BLAS_matvec(-1.0_SRK*thisPC%F0(izyx,:,:),v%b((izyxg+1):(izyxg+dxg)), &
                        soln((izyxg+1):(izyxg+dxg)))
                    ENDDO

                  ENDDO

                  ! Step 2: BACKWARD SOLVE

                  DO j=(Y-1),1,-1
                    iy=(k-1)*dy+j
                    izygp=izyg
                    izy=(k-1)*dz+(j-1)*dy ! which goes from j to j + X
                    izyg=(k-1)*dzg+(j-1)*dyg ! which goes from j to j + dyg
                    iyg=(j-1)*dyg

                    v%b((izyg+1):(izyg+dyg))=0.0_SRK
                    CALL BLAS_axpy(-1.0_SRK*thisPC%N(k,(iyg+1):(iyg+dyg)), &
                      soln((izygp+1):(izygp+dyg)),v%b((izyg+1):(izyg+dyg)))

                    soln_prevj=soln((izyg+1):(izyg+dyg))
                    soln((izyg+1):(izyg+dyg))=0.0_SRK

                    ! INSERT M(1) SOLVE HERE. Uses soln, v%b, and F0 and W.
                    ! Step 1: FORWARD SOLVE (obtain x~)

                    ! i=1, soln(1)=F0(1,j,k)*b(1,j,k)
                    i=1
                    izyx=izy+i
                    izyxg=izyg+(i-1)*dxg
                    ! soln should aready be zero
                    CALL BLAS_matvec(thisPC%F0(izyx,:,:),v%b((izyxg+1):(izyxg+dxg)), &
                      soln((izyxg+1):(izyxg+dxg)))

                    ! i=2,X
                    DO i=2,X
                      izyx=izy+i
                      izyxgp=izyxg
                      izyxg=izyg+(i-1)*dxg ! increase izyxg by nGrp for each i
                      ! Overwrite b because not used again.
                      ! b(i,j,k)-W(i,j,k)*soln(i-1)
                      CALL BLAS_axpy(-1.0_SRK*thisPC%W(iy,((i-2)*dxg+1):((i-1)*dxg)), &
                        soln((izyxgp+1):(izyxgp+dxg)),v%b((izyxg+1):(izyxg+dxg)))
                      ! F0(i,j,k)*(b(i,j,k)-W(i,j,k)*soln(i-1))
                      ! soln should be zero
                      CALL BLAS_matvec(thisPC%F0(izyx,:,:),v%b((izyxg+1):(izyxg+dxg)),soln((izyxg+1):(izyxg+dxg)))
                    ENDDO


                    ! Step 2: BACKWARD SOLVE

                    ! i=X
                    ! Do nothing (already solved)

                    ! rest
                    DO i=(X-1),1,-1
                      izyxgp=izyxg
                      izyx=izy+i
                      izyxg=izyg+(i-1)*dxg ! increase izyxg by nGrp for each i
                      ! Overwrite b once more because unused
                      v%b((izyxg+1):(izyxg+dxg))=0.0_SRK
                      ! E(i,j)*soln(i+1,j,k)
                      CALL BLAS_axpy(thisPC%E(iy,((i-1)*dxg+1):(i*dxg)), &
                        soln((izyxgp+1):(izyxgp+dxg)),v%b((izyxg+1):(izyxg+dxg)))
                      ! ans(i,j,k)=ans(i,j,k)-F0(i,j,k)*(E(i,j)*soln(i+1,j,k))
                      CALL BLAS_matvec(-1.0_SRK*thisPC%F0(izyx,:,:),v%b((izyxg+1):(izyxg+dxg)), &
                        soln((izyxg+1):(izyxg+dxg)))
                    ENDDO

                    soln((izyg+1):(izyg+dyg))=soln((izyg+1):(izyg+dyg))+soln_prevj

                  ENDDO

                ENDDO

                ! STEP 2: Solve Backwards

                DO k=(Z-1),1,-1
                  iz=iz-1
                  izgp=izg
                  izg=izg-dzg

!                  v%b((izg+1):(izg+dzg))=0.0_SRK
!                  CALL BLAS_axpy(-1.0_SRK*thisPC%T,soln((izgp+1):(izgp+dzg)), &
!                    v%b((izg+1):(izg+dzg)))
!                  tmpTB=0.0_SRK
                  DO i=1,dzg
                    CALL thisPC%A%get(izgp+i,izg+i,tmpTB(i))
                    tmpTB(i)=-1.0_SRK*tmpTB(i)
                  ENDDO
                  v%b((izg+1):(izg+dzg))=0.0_SRK
                  CALL BLAS_axpy(tmpTB,soln((izgp+1):(izgp+dzg)),v%b((izg+1):(izg+dzg)))
!                  CALL BLAS_matvec(thisPC%A((izgp+1):(izgp+dzg),(izg+1):(izg+dzg)),&
!                    soln((izgp+1):(izgp+dzg)),soln_prevk)

                  soln_prevk=soln((izg+1):(izg+dzg))
                  soln((izg+1):(izg+dzg))=0.0_SRK

                  ! INSERT M(2) SOLVE HERE
                  ! Step 1: FORWARD SOLVE
                  izyg=(k-1)*dzg
                  DO j=1,Y
                    iy=(k-1)*dy+j
                    izygp=izyg
                    izy=(k-1)*dz+(j-1)*dy ! which goes from j to j + X
                    izyg=(k-1)*dzg+(j-1)*dyg ! which goes from j to j + dyg

                    IF(j > 1) THEN
                      iym1g=(j-2)*dyg
                      CALL BLAS_axpy(-1.0_SRK*thisPC%S(k,(iym1g+1):(iym1g+dyg)), &
                        soln((izygp+1):(izygp+dyg)), &
                          v%b((izyg+1):(izyg+dyg)))
                    ENDIF

                    ! INSERT M(1) SOLVE HERE. Uses soln, v%b, and F0 and W.
                    ! Step 1: FORWARD SOLVE (obtain x~)

                    ! i=1, soln(1)=F0(1,j,k)*b(1,j,k)
                    i=1
                    izyx=izy+i
                    izyxg=izyg+(i-1)*dxg
                    ! soln should aready be zero
                    CALL BLAS_matvec(thisPC%F0(izyx,:,:),v%b((izyxg+1):(izyxg+dxg)), &
                      soln((izyxg+1):(izyxg+dxg)))

                    ! i=2,X
                    DO i=2,X
                      izyx=izy+i
                      izyxgp=izyxg
                      izyxg=izyg+(i-1)*dxg ! increase izyxg by nGrp for each i
                      ! Overwrite b because not used again.
                      ! b(i,j,k)-W(i,j,k)*soln(i-1)
                      CALL BLAS_axpy(-1.0_SRK*thisPC%W(iy,((i-2)*dxg+1):((i-1)*dxg)), &
                        soln((izyxgp+1):(izyxgp+dxg)),v%b((izyxg+1):(izyxg+dxg)))
                      ! F0(i,j,k)*(b(i,j,k)-W(i,j,k)*soln(i-1))
                      ! soln should be zero
                      CALL BLAS_matvec(thisPC%F0(izyx,:,:),v%b((izyxg+1):(izyxg+dxg)),soln((izyxg+1):(izyxg+dxg)))
                    ENDDO


                    ! Step 2: BACKWARD SOLVE

                    ! i=X
                    ! Do nothing (already solved)

                    ! rest
                    DO i=(X-1),1,-1
                      izyxgp=izyxg
                      izyx=izy+i
                      izyxg=izyg+(i-1)*dxg ! increase izyxg by nGrp for each i
                      ! Overwrite b once more because unused
                      v%b((izyxg+1):(izyxg+dxg))=0.0_SRK
                      ! E(i,j)*soln(i+1,j,k)
                      CALL BLAS_axpy(thisPC%E(iy,((i-1)*dxg+1):(i*dxg)), &
                        soln((izyxgp+1):(izyxgp+dxg)),v%b((izyxg+1):(izyxg+dxg)))
                      ! ans(i,j,k)=ans(i,j,k)-F0(i,j,k)*(E(i,j)*soln(i+1,j,k))
                      CALL BLAS_matvec(-1.0_SRK*thisPC%F0(izyx,:,:),v%b((izyxg+1):(izyxg+dxg)), &
                        soln((izyxg+1):(izyxg+dxg)))
                    ENDDO

                  ENDDO

                  ! Step 2: BACKWARD SOLVE

                  DO j=(Y-1),1,-1
                    iy=(k-1)*dy+j
                    izygp=izyg
                    izy=(k-1)*dz+(j-1)*dy ! which goes from j to j + X
                    izyg=(k-1)*dzg+(j-1)*dyg ! which goes from j to j + dyg
                    iyg=(j-1)*dyg

                    v%b((izyg+1):(izyg+dyg))=0.0_SRK
                    CALL BLAS_axpy(-1.0_SRK*thisPC%N(k,(iyg+1):(iyg+dyg)), &
                      soln((izygp+1):(izygp+dyg)),v%b((izyg+1):(izyg+dyg)))

                    soln_prevj=soln((izyg+1):(izyg+dyg))
                    soln((izyg+1):(izyg+dyg))=0.0_SRK

                    ! INSERT M(1) SOLVE HERE. Uses soln, v%b, and F0 and W.
                    ! Step 1: FORWARD SOLVE (obtain x~)

                    ! i=1, soln(1)=F0(1,j,k)*b(1,j,k)
                    i=1
                    izyx=izy+i
                    izyxg=izyg+(i-1)*dxg
                    ! soln should aready be zero
                    CALL BLAS_matvec(thisPC%F0(izyx,:,:),v%b((izyxg+1):(izyxg+dxg)), &
                      soln((izyxg+1):(izyxg+dxg)))

                    ! i=2,X
                    DO i=2,X
                      izyx=izy+i
                      izyxgp=izyxg
                      izyxg=izyg+(i-1)*dxg ! increase izyxg by nGrp for each i
                      ! Overwrite b because not used again.
                      ! b(i,j,k)-W(i,j,k)*soln(i-1)
                      CALL BLAS_axpy(-1.0_SRK*thisPC%W(iy,((i-2)*dxg+1):((i-1)*dxg)), &
                        soln((izyxgp+1):(izyxgp+dxg)),v%b((izyxg+1):(izyxg+dxg)))
                      ! F0(i,j,k)*(b(i,j,k)-W(i,j,k)*soln(i-1))
                      ! soln should be zero
                      CALL BLAS_matvec(thisPC%F0(izyx,:,:),v%b((izyxg+1):(izyxg+dxg)),soln((izyxg+1):(izyxg+dxg)))
                    ENDDO


                    ! Step 2: BACKWARD SOLVE

                    ! i=X
                    ! Do nothing (already solved)

                    ! rest
                    DO i=(X-1),1,-1
                      izyxgp=izyxg
                      izyx=izy+i
                      izyxg=izyg+(i-1)*dxg ! increase izyxg by nGrp for each i
                      ! Overwrite b once more because unused
                      v%b((izyxg+1):(izyxg+dxg))=0.0_SRK
                      ! E(i,j)*soln(i+1,j,k)
                      CALL BLAS_axpy(thisPC%E(iy,((i-1)*dxg+1):(i*dxg)), &
                        soln((izyxgp+1):(izyxgp+dxg)),v%b((izyxg+1):(izyxg+dxg)))
                      ! ans(i,j,k)=ans(i,j,k)-F0(i,j,k)*(E(i,j)*soln(i+1,j,k))
                      CALL BLAS_matvec(-1.0_SRK*thisPC%F0(izyx,:,:),v%b((izyxg+1):(izyxg+dxg)), &
                        soln((izyxg+1):(izyxg+dxg)))
                    ENDDO

                    soln((izyg+1):(izyg+dyg))=soln((izyg+1):(izyg+dyg))+soln_prevj

                  ENDDO

                  soln((izg+1):(izg+dzg))=soln((izg+1):(izg+dzg))+soln_prevk
                ENDDO

                ENDIF

                v%b=soln
                DO i=1,SIZE(soln)
!                  WRITE(*,*) soln(i)
                ENDDO
!                STOP 677

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
!> @brief Sets up the BILU Preconditioner
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
    SUBROUTINE setup_BILU_PreCondtype(thisPC)
      CHARACTER(LEN=*),PARAMETER :: myName='setup_BILU_PreCondType'
      CLASS(BILU_PrecondType),INTENT(INOUT) :: thisPC

      INTEGER(SIK) :: ix,iy,iz,row,col,ind
      INTEGER(SIK) :: c0,c1,c2,d0,d1,d2,dim0D,dim1D,dim2D
      INTEGER(SIK) :: XY,X,Y,Z,G
      REAL(SRK) :: val
      REAL(SRK),ALLOCATABLE :: invM2(:,:),invM1(:,:),invM0(:,:)
      REAL(SRK),ALLOCATABLE :: tmp2D(:,:),tmp1D(:,:),tmp0D(:,:)
      REAL(SRK),ALLOCATABLE :: L2(:,:),U2(:,:),F2(:,:)
      REAL(SRK),ALLOCATABLE :: L1(:,:),U1(:,:),F1(:,:)
      REAL(SRK),ALLOCATABLE :: T(:),B(:),S(:),N(:),E(:),W(:)

      IF(.NOT.(thisPC%isinit)) THEN
        CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Preconditioner is not initialized!')
      ELSE
        IF(.NOT.(thisPC%A%isInit)) THEN
          CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
            ' - Matrix being used for LU Preconditioner is not initialized!')
        ELSE

          SELECTTYPE(U => thisPC%U); TYPE IS(SparseMatrixType)
            SELECTTYPE(L => thisPC%L); TYPE IS(SparseMatrixType)
              IF(.NOT.(U%isinit)) THEN
                CALL ePrecondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                  ' - in LU decomposition, U was not properly initialize!')
              ELSEIF(.NOT.(L%isinit)) THEN
                CALL ePrecondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                  ' - in LU decomposition, L was not properly initialize!')
              ELSE

                !dimension variables
                XY=thisPC%nPin
                X=INT(SQRT(REAL(XY)))
                Y=INT(SQRT(REAL(XY)))
                Z=thisPC%nPlane
                G=thisPC%nGrp
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
                        CALL thisPC%A%get(row,col,val)
                        IF(val /= 0.0_SRK) CALL thisPC%L%set(row,col,val)
                      ENDDO
                    ENDDO
!                    !form diagonal block of U (Identity)
!                    DO row=c2,c2+dim2D-1
!                      CALL thisPC%U%set(row,row,1.0_SRK)
!                    ENDDO
                  ELSE
                    !pull T block into matrix
                    DO row=d2,d2+dim2D-1
                      CALL thisPC%A%get(row,c2+row-d2,val)
                      T(row-d2+1)=val
                    ENDDO
                    !pull B block into matrix
                    DO row=c2,c2+dim2D-1
                      CALL thisPC%A%get(row,d2+row-c2,val)
                      B(row-c2+1)=val
                    ENDDO
                    !form diagonal block of L
                    tmp2D=0.0_SRK
                    IF(thisPC%BILUType==BILU) THEN
                      CALL dmatmul_left(B,invM2,tmp2D)
                      CALL dmatmul_right(tmp2D,T,tmp2D)
                    ENDIF
                    !form diagonal block of L
                    DO row=c2,c2+dim2D-1
                      DO col=c2,c2+dim2D-1
                        CALL thisPC%A%get(row,col,val)
                        IF(val /= 0.0_SRK) THEN
                          IF(thisPC%BILUType==BILU) THEN
                            CALL thisPC%L%set(row,col,val-tmp2D(row-c2+1,col-c2+1))
                          ELSEIF(thisPC%BILUType==BILUSGS) THEN
                            CALL thisPC%L%set(row,col,val)
                          ENDIF
                        ENDIF
                      ENDDO
                    ENDDO
                    !form lower block of L
                    DO row=c2,c2+dim2D-1
                      DO col=d2,d2+dim2D-1
                        CALL thisPC%A%get(row,col,val)
                        IF(val /= 0.0_SRK) &
                          CALL thisPC%L%set(row,col,val)
                      ENDDO
                    ENDDO
!                    !form diagonal block of U (Identity)
!                    DO row=c2,c2+dim2D-1
!                      CALL thisPC%U%set(row,row,1.0_SRK)
!                    ENDDO
!                    !form upper block of U
!                    CALL dmatmul_right(invM2,T,tmp2D)
!                    DO row=d2,d2+dim2D-1
!                      DO col=c2,c2+dim2D-1
!                        CALL thisPC%A%get(row,col,val)
!                        IF(val /= 0.0_SRK) &
!                          CALL thisPC%U%set(row,col,tmp2D(row-d2+1,col-c2+1))
!                      ENDDO
!                    ENDDO

                  ENDIF

                  !pull block M into matrix
                  tmp2D=0.0_SRK
                  DO row=c2,c2+dim2D-1
                    DO col=c2,c2+dim2D-1
                      CALL thisPC%L%get(row,col,val)
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
                      thisPC%F0(ind,:,:)=invM0

                    ENDDO !ix

                    !determine 1D inverse
                    CALL ABI(L1,U1,F1,dim0D,invM1)
                    F2(c1:c1+dim1D-1,c1:c1+dim1D-1)=invM1
                    !store for apply
                    ind=(iz-1)*Y+iy
                    DO row=1,dim0D*(X-1)
                      thisPC%E(ind,row)=tmp1D(row,dim0D+row)
                    ENDDO
                    DO row=1,dim0D*(X-1)
                      thisPC%W(ind,row)=tmp1D(dim0D+row,row)
                    ENDDO

                  ENDDO !iy

                  !determine 2D inverse
                  IF(thisPC%BILUType==BILU) CALL ABI(L2,U2,F2,dim1D,invM2)
                  DO row=1,dim1D*(X-1)
                    thisPC%N(iz,row)=tmp2D(row,dim1D+row)
                  ENDDO
                  DO row=1,dim1D*(X-1)
                    thisPC%S(iz,row)=tmp2D(dim1D+row,row)
                  ENDDO

                ENDDO !iz
              ENDIF

            CLASS DEFAULT
                CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                  ' - LU Preconditioners are not supported by input matrix type!')
            ENDSELECT
          CLASS DEFAULT
              CALL ePreCondType%raiseError('Incorrect input to '//modName//'::'//myName// &
                ' - LU Preconditioners are not supported by input matrix type!')
          ENDSELECT

        ENDIF
      ENDIF
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
      REAL(SRK),ALLOCATABLE :: L0(:),tmpM(:,:),invDU0(:)

      ALLOCATE(tmpM(block_dim,block_dim))
      ALLOCATE(L0(block_dim))
      ALLOCATE(invDU0(block_dim))
      tmpM=0.0_SRK
      L0=0.0_SRK
      invDU0=0.0_SRK

      X=SIZE(L(:,1))/block_dim

      !last row
      i=X
      ic=(i-1)*block_dim+1
      iu=(i)*block_dim+1
      id=(i-2)*block_dim+1
      DO ix=X,1,-1
        IF(ix == i) THEN
          !diagonal block term
          invA(ic:ic+block_dim-1,ic:ic+block_dim-1) = &
            F(ic:ic+block_dim-1,ic:ic+block_dim-1)
        ELSEIF(ix == i-1) THEN
          !lower block term)
          DO row=1,block_dim
            L0(row)=L(ic+row-1,id+row-1)
          ENDDO
          CALL dmatmul_right(invA(ic:ic+block_dim-1,ic:ic+block_dim-1),L0,tmpM)
          invA(ic:ic+block_dim-1,id:id+block_dim-1)= &
            -MATMUL(tmpM,F(id:id+block_dim-1,id:id+block_dim-1))
        ENDIF
      ENDDO

      !interior rows
      DO i=X-1,2,-1
        ic=(i-1)*block_dim+1
        iu=(i)*block_dim+1
        id=(i-2)*block_dim+1

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
            DO row=1,block_dim
              L0(row)=L(ic+row-1,id+row-1)
            ENDDO
            CALL dmatmul_left(L0,F(id:id+block_dim-1,id:id+block_dim-1),tmpM)
            invA(ic:ic+block_dim-1,id:id+block_dim-1)=-MATMUL(invA(ic:ic+block_dim-1,ic:ic+block_dim-1),tmpM)
          ELSEIF(ix == i+1) THEN
            !upper block term
            DO row=1,block_dim
              invDU0(row)=U(ic+row-1,iu+row-1)
            ENDDO
            CALL dmatmul_left(invDU0,invA(iu:iu+block_dim-1,iu:iu+block_dim-1),tmpM)
            invA(ic:ic+block_dim-1,iu:iu+block_dim-1)=-tmpM
          ENDIF
        ENDDO !ix
      ENDDO !i

      !first row
      i=1
      ic=(i-1)*block_dim+1
      iu=(i)*block_dim+1
      id=(i-2)*block_dim+1
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
          DO row=1,block_dim
            invDU0(row)=U(ic+row-1,iu+row-1)
          ENDDO
          CALL dmatmul_left(invDU0,invA(iu:iu+block_dim-1,iu:iu+block_dim-1),tmpM)
          invA(ic:ic+block_dim-1,iu:iu+block_dim-1) = -tmpM
        ENDIF
      ENDDO

      DEALLOCATE(tmpM)
      DEALLOCATE(L0)
      DEALLOCATE(invDU0)
    ENDSUBROUTINE ABI
!
!-------------------------------------------------------------------------------
!> @brief
!>
    SUBROUTINE solve_M3(thisPC,v,b)
      CLASS(BILU_PrecondType),INTENT(INOUT) :: thisPC
      REAL(SRK),INTENT(INOUT) :: v(:)
      REAL(SRK),INTENT(IN) :: b(:)
      INTEGER(SIK) :: X,j,Y,k,Z,ng,index1,index2,index3
      REAL(SRK),ALLOCATABLE :: vt(:),bt(:),tmp(:),Bot(:,:),T(:,:)

      ng=thisPC%nGrp
      X=INT(SQRT(REAL(thisPC%nPin)))
      Y=X
      Z=thisPC%nPlane

      ALLOCATE(vt(SIZE(v)))
      ALLOCATE(bt(ng*X*Y))
      ALLOCATE(tmp(ng*X*Y))
      ALLOCATE(Bot(ng*X*Y,ng*X*Y))
      ALLOCATE(T(ng*X*Y,ng*X*Y))

      CALL solve_M2(thisPC,vt(1:(ng*X*Y)),b(1:(ng*X*Y)),1)

      DO k=2,Z
        index1=(k-1)*X*Y*ng
        index2=(k-2)*X*Y*ng
        Bot=0.0_SRK
        DO j=1,(X*Y*ng)
          CALL thisPC%A%get(j+(k-1)*X*Y*ng,j+(k-2)*X*Y*ng,Bot(j,j))
        ENDDO
        tmp=0.0_SRK
        CALL BLAS_matvec(Bot,vt((index2+1):(index2+X*Y*ng)),tmp)
        bt=b((index1+1):(index1+X*Y*ng))-tmp
        CALL solve_M2(thisPC,tmp,bt,k)
        vt((index1+1):(index1+X*Y*ng))=tmp
      ENDDO

      v(((Z-1)*Y*X*ng+1):(Z*Y*X*ng))=vt(((Z-1)*Y*X*ng+1):(Z*Y*X*ng))

      DO k=(Z-1),1,-1
        index1=(k-1)*Y*X*ng
        index2=(k)*Y*X*ng
        index3=(k-2)*Y*X*ng
        T=0.0_SRK
        DO j=1,(X*Y*ng)
          CALL thisPC%A%get(j+(k-1)*X*Y*ng,j+(k)*X*Y*ng,T(j,j))
        ENDDO
        T=-1.0*T
        bt=0.0_SRK
        CALL BLAS_matvec(T,v((index2+1):(index2+X*Y*ng)),bt)
        CALL solve_M2(thisPC,tmp,bt,k)
        v((index1+1):(index1+Y*X*ng))=vt((index1+1):(index1+Y*X*ng))+tmp
      ENDDO

    ENDSUBROUTINE solve_M3
!
!-------------------------------------------------------------------------------
!> @brief
!>
    SUBROUTINE solve_M2(thisPC,v,b,k)
      CLASS(BILU_PrecondType),INTENT(INOUT) :: thisPC
      REAL(SRK),INTENT(INOUT) :: v(:)
      REAL(SRK),INTENT(IN) :: b(:)
      INTEGER(SIK),INTENT(IN) :: k

      REAL(SRK),ALLOCATABLE :: vt(:),bt(:),tmp(:)

      INTEGER(SIK) :: X,j,Y,ng,index1,index2,index3

      ng=thisPC%nGrp
      X=INT(SQRT(REAL(thisPC%nPin)))
      Y=X

      ALLOCATE(vt(SIZE(v)))
      ALLOCATE(bt(ng*X))
      ALLOCATE(tmp(ng*X))

      CALL solve_M1(thisPC,vt(1:(ng*X)),b(1:(ng*X)),1,k)

      DO j=2,Y
        index1=(j-1)*X*ng
        index2=(j-2)*X*ng
        tmp=0.0_SRK
        CALL BLAS_axpy(thisPC%S(k,(index2+1):(index2+X*ng)),vt((index2+1):(index2+X*ng)),tmp)
        bt=b((index1+1):(index1+X*ng))-tmp
        CALL solve_M1(thisPC,tmp,bt,j,k)
        vt((index1+1):(index1+X*ng))=tmp
      ENDDO

      v(((Y-1)*X*ng+1):(Y*X*ng))=vt(((Y-1)*X*ng+1):(Y*X*ng))

      DO j=(Y-1),1,-1
        index1=(j-1)*X*ng
        index2=(j)*X*ng
        index3=(j-2)*X*ng
        bt=0.0_SRK
        CALL BLAS_axpy(thisPC%N(k,(index1+1):(index1+X*ng)),v((index2+1):(index2+X*ng)),bt)
        bt=-1.0_SRK*bt
        CALL solve_M1(thisPC,tmp,bt,j,k)
        v((index1+1):(index1+X*ng))=vt((index1+1):(index1+X*ng))+tmp
      ENDDO
    ENDSUBROUTINE solve_M2
!
!-------------------------------------------------------------------------------
!> @brief
!>
    SUBROUTINE solve_M1(thisPC,v,b,j,k)
      CLASS(BILU_PrecondType),INTENT(INOUT) :: thisPC
      REAL(SRK),INTENT(INOUT) :: v(:)
      REAL(SRK),INTENT(IN) :: b(:)
      INTEGER(SIK),INTENT(IN) :: j
      INTEGER(SIK),INTENT(IN) :: k

      REAL(SRK),ALLOCATABLE :: vt(:),bt(:),tmp(:)

      INTEGER(SIK) :: i,X,ng,index1,index2,index3

      ng=thisPC%nGrp
      X=INT(SQRT(REAL(thisPC%nPin)))

      ALLOCATE(vt(SIZE(v)))
      ALLOCATE(bt(ng))
      ALLOCATE(tmp(ng))

      index1=(k-1)*thisPC%nPin*ng+(j-1)*X*ng
      index2=(k-1)*thisPC%nPin+(j-1)*X
      index3=(k-1)*X+j

      i=1

      bt=b(1:ng)
      tmp=0.0_SRK
      CALL BLAS_matvec(thisPC%F0(index2+i,:,:),bt,tmp)
      vt(1:ng)=tmp
!      IF(vt(1) > 1e2_SRK) THEN
!        WRITE(*,*) "A",i,j,k,vt(1),thisPC%F0(index2+i,1,1),bt(1)
!      ENDIF

      DO i=2,X
        tmp=0.0_SRK
        CALL BLAS_axpy(thisPC%W(index3,((i-2)*ng+1):((i-1)*ng)),vt(((i-2)*ng+1):((i-1)*ng)),tmp)
        bt=b(((i-1)*ng+1):(i*ng))-tmp
        tmp=0.0_SRK
        CALL BLAS_matvec(thisPC%F0(index2+i,:,:),bt,tmp)
        vt(((i-1)*ng+1):(i*ng))=tmp
!        IF(tmp(1) > 1e2_SRK) THEN
!          WRITE(*,*) "A",i,j,k,tmp(1),thisPC%F0(index2+i,1,1),thisPC%W(index3,((i-2)*ng+1)),vt((i-1)*ng),bt(1)
!        ENDIF
      ENDDO

      v(((X-1)*ng+1):(X*ng))=vt(((X-1)*ng+1):(X*ng))

      DO i=(X-1),1,-1
        tmp=0.0_SRK
        CALL BLAS_axpy(thisPC%E(index3,((i-1)*ng+1):((i)*ng)),v((i*ng+1):((i+1)*ng)),tmp)
        bt=0.0_SRK
        CALL BLAS_matvec(thisPC%F0(index2+i,:,:),tmp,bt)
        v(((i-1)*ng+1):(i*ng))=vt(((i-1)*ng+1):(i*ng))-bt
!        IF(v(1) > 1e2_SRK) THEN
!          WRITE(*,*) "B",i,j,k,v(((i-1)*ng+1):(i*ng)),thisPC%F0(index2+i,1,1),thisPC%E(index3,((i-1)*ng+1))
!        ENDIF
      ENDDO
    ENDSUBROUTINE solve_M1
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
