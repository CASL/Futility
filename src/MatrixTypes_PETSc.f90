!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief PETSc implementations of MatrixTypes
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref MatrixTypes_Base "MatrixTypes_Base": @copybrief MatrixTypes_Base
!>  - @ref BLAS2 "BLAS2": @copybrief BLAS2
!>  - @ref BLAS3 "BLAS3": @copybrief BLAS3
!>  - @ref VectorTypes "VectorTypes": @copybrief VectorTypes
!>
!> @author Shane Stimpson
!>   @date 02/14/2012
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE MatrixTypes_PETSc
  USE IntrType
  USE ExceptionHandler
  USE ParameterLists
  USE MatrixTypes_Base
  USE BLAS2,           ONLY: BLAS2_matvec => BLAS_matvec
  USE BLAS3,           ONLY: BLAS3_matmult => BLAS_matmat
  USE VectorTypes

  IMPLICIT NONE

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
#undef IS

  PRIVATE
!
! List of public members
  PUBLIC :: PETScMatrixType
  PUBLIC :: matvec_PETSc
  PUBLIC :: matvec_PETScVector
  PUBLIC :: matmult_PETSc

  TYPE,EXTENDS(DistributedMatrixType) :: PETScMatrixType
    Mat :: A

    !> Number of rows for nonsquare matrices:
    INTEGER(SIK) :: m
!
!List of Type Bound Procedures
    CONTAINS
      !> @copybrief MatrixTypes::clear_PETScMatrixType
      !> @copydetails MatrixTypes::clear_PETScMatrixType
      PROCEDURE,PASS :: clear => clear_PETScMatrixType
      !> @copybrief MatrixTypes::init_PETScMatrixType
      !> @copydetails MatrixTypes::init_PETScMatrixType
      PROCEDURE,PASS :: init => init_PETScMatrixParam
      !> @copybrief MatrixTypes::set_PETScMatrixType
      !> @copydetails MatrixTypes::set_PETScMatrixType
      PROCEDURE,PASS :: set => set_PETScMatrixType
      !> @copybrief MatrixTypes::set_PETScMatrixType
      !> @copydetails MatrixTypes::set_PETScMatrixType
      PROCEDURE,PASS :: setShape => set_PETScMatrixType
      !> @copybrief MatrixTypes::get_PETScMatrixType
      !> @copydetails MatrixTypes::get_PETScMatrixType
      PROCEDURE,PASS :: get => get_PETScMatrixType
      !> @copybrief MatrixTypes::assemble_PETScMatrixType
      !> @copydetails MatrixTypes::assemble_PETScMatrixType
      PROCEDURE,PASS :: assemble => assemble_PETScMatrixType
      !> @copybrief MatrixTypes::transpose_PETScMatrixType
      !> @copydetails MatrixTypes::transpose_PETScMatrixType
      PROCEDURE,PASS :: transpose => transpose_PETScMatrixType
  ENDTYPE PETScMatrixType

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='MATRIXTYPES_PETSC'

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes PETSc Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param pList the parameter list
!>
    SUBROUTINE init_PETScMatrixParam(matrix,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_PETScMatrixParam'
      CLASS(PETScMatrixType),INTENT(INOUT) :: matrix
      CLASS(ParamType),INTENT(IN) :: Params
      TYPE(ParamType) :: validParams
      INTEGER(SIK) :: n, matType, MPI_COMM_ID, nlocal
      !Number of columns (global and local)
      INTEGER(SIK) :: m, mlocal
      INTEGER(SIK),ALLOCATABLE :: dnnz(:), onnz(:)
      LOGICAL(SBK) :: isSym

      PetscErrorCode  :: ierr

      !Check to set up required and optional param lists.
      IF(.NOT.MatrixType_Paramsflag) CALL MatrixTypes_Declare_ValidParams()
      !Validate against the reqParams and OptParams
      validParams=Params
      CALL validParams%validate(DistributedMatrixType_reqParams,DistributedMatrixType_optParams)

      ! Pull Data From Parameter List
      CALL validParams%get('MatrixType->n',n)
      isSym=.FALSE.
      IF(validParams%has('MatrixType->isSym')) &
        CALL validParams%get('MatrixType->isSym',isSym)
      CALL validParams%get('MatrixType->matType',matType)
      CALL validParams%get('MatrixType->MPI_COMM_ID',MPI_COMM_ID)
      CALL validParams%get('MatrixType->nlocal',nlocal)

      m=n
      mlocal=nlocal
      IF(.NOT. isSym) THEN
        IF(validParams%has('MatrixType->m')) &
          CALL validParams%get('MatrixType->m',m)
        IF(validParams%has('MatrixType->mlocal')) &
          CALL validParams%get('MatrixType->mlocal',mlocal)
      ENDIF

      ALLOCATE(dnnz(nlocal))
      ALLOCATE(onnz(nlocal))
      CALL validParams%get('MatrixType->dnnz',dnnz)
      CALL validParams%get('MatrixType->onnz',onnz)
      CALL validParams%clear()

      IF(.NOT. matrix%isInit) THEN
        IF(n < 1 .OR. m < 1) THEN
          CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of rows (n) and cols (m) '// &
              'must be greater than 0!')
        ELSE
          matrix%isInit=.TRUE.
          matrix%n=n
          matrix%m=m
          matrix%comm=MPI_COMM_ID
          matrix%isAssembled=.FALSE.
          matrix%nlocal=nlocal
          IF(isSym) THEN
            matrix%isSymmetric=.TRUE.
          ELSE
            matrix%isSymmetric=.FALSE.
          ENDIF
          IF(.NOT.matrix%isCreated) THEN
            CALL MatCreate(MPI_COMM_ID,matrix%a,ierr)
            matrix%isCreated=.TRUE.
          ENDIF
          IF(nlocal<0 .OR. mlocal<0) THEN
            CALL MatSetSizes(matrix%a,PETSC_DECIDE,PETSC_DECIDE,matrix%n,matrix%m,ierr)
          ELSE
            CALL MatSetSizes(matrix%a,nlocal,mlocal,matrix%n,matrix%m,ierr)
          ENDIF

          IF (matType == SPARSE) THEN
            CALL MatSetType(matrix%a,MATMPIAIJ,ierr)
          ELSEIF (matType == DENSESQUARE) THEN
            CALL MatSetType(matrix%a,MATMPIDENSE,ierr)
          ELSE
            CALL eMatrixType%raiseError('Invalid matrix type in '// &
              modName//'::'//myName//' - Only sparse and dense square '// &
              'matrices are available with PETSc.')
          ENDIF

          IF(MINVAL(dnnz) > 0_SIK .AND. MINVAL(onnz) >= 0_SIK) THEN
            CALL MatMPIAIJSetPreallocation(matrix%A,0,dnnz,0,onnz,ierr)
          ELSE
            CALL MatSetUp(matrix%a,ierr)
          ENDIF
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - MatrixType already initialized')
      ENDIF
    ENDSUBROUTINE init_PETScMatrixParam

!
!-------------------------------------------------------------------------------
!> @brief Clears the PETSc sparse matrix
!> @param matrix the matrix type to act on
!>
    SUBROUTINE clear_PETScMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_PETScMatrixType'
      CLASS(PETScMatrixType),INTENT(INOUT) :: matrix
      PetscErrorCode  :: ierr

      IF(matrix%isInit) CALL MatDestroy(matrix%a,ierr)
      matrix%isInit=.FALSE.
      matrix%n=0
      matrix%isAssembled=.FALSE.
      matrix%isCreated=.FALSE.
      matrix%isSymmetric=.FALSE.
    ENDSUBROUTINE clear_PETScMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the PETSc matrix
!> @param declares the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
    SUBROUTINE set_PETScMatrixType(matrix,i,j,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='set_PETScMatrixType'
      CLASS(PETScMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(IN) :: setval
      PetscErrorCode  :: ierr

      IF(matrix%isInit) THEN
        IF(((j <= matrix%n) .AND. (i <= matrix%n)) &
          .AND. ((j > 0) .AND. (i > 0))) THEN
          CALL MatSetValues(matrix%a,1,i-1,1,j-1,setval,INSERT_VALUES,ierr)
          IF(matrix%isSymmetric) THEN
            CALL MatSetValues(matrix%a,1,j-1,1,i-1,setval,INSERT_VALUES,ierr)
          ENDIF
          matrix%isAssembled=.FALSE.
        ENDIF
      ENDIF
    ENDSUBROUTINE set_PETScMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Gets the values in the PETSc matrix - presently untested
!> @param declare the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!>
!> This routine gets the values of the sparse matrix.  If the (i,j) location is
!> out of bounds, then -1051.0 (an arbitrarily chosen key) is returned.
!>
    SUBROUTINE get_PETScMatrixType(matrix,i,j,getval)
      CHARACTER(LEN=*),PARAMETER :: myName='get_PETScMatrixType'
      CLASS(PETScMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(INOUT) :: getval
      PetscErrorCode  :: ierr

      getval=0.0_SRK
      IF(matrix%isInit) THEN
        ! assemble matrix if necessary
        IF (.NOT.(matrix%isAssembled)) CALL matrix%assemble()

        IF((i <= matrix%n) .AND. (j <= matrix%n) .AND. ((j > 0) .AND. (i > 0))) THEN
          CALL MatGetValues(matrix%a,1,i-1,1,j-1,getval,ierr)
        ELSE
          getval=-1051._SRK
        ENDIF
      ENDIF
    ENDSUBROUTINE get_PETScMatrixtype
!
!-------------------------------------------------------------------------------
    SUBROUTINE assemble_PETScMatrixType(thisMatrix,ierr)
      CLASS(PETScMatrixType),INTENT(INOUT) :: thisMatrix
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      INTEGER(SIK) :: ierrc
      PetscErrorCode  :: iperr

      ierrc=0
      IF(.NOT.thisMatrix%isAssembled) THEN
        CALL MatAssemblyBegin(thisMatrix%a,MAT_FINAL_ASSEMBLY,iperr)
        IF(iperr == 0) CALL MatAssemblyEnd(thisMatrix%a,MAT_FINAL_ASSEMBLY,iperr)
        IF(iperr == 0) thisMatrix%isAssembled=.TRUE.
        ierrc=iperr
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE assemble_PETScMatrixType
!
!-------------------------------------------------------------------------------
!> @brief tranpose the matrix
!> @param matrix declare the matrix type to act on
!>
!>
    SUBROUTINE transpose_PETScMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='transpose_PETScMatrixType'
      CLASS(PETScMatrixType),INTENT(INOUT) :: matrix
      PetscErrorCode  :: iperr
      IF(.NOT.matrix%isAssembled) CALL matrix%assemble()
      !This is to avoid a deadlock in IBarrier in MPICH
      CALL PetscCommBuildTwoSidedSetType(matrix%comm, &
        PETSC_BUILDTWOSIDED_ALLREDUCE,iperr)
      CALL MatTranspose(matrix%a,MAT_REUSE_MATRIX,matrix%a,iperr)
    ENDSUBROUTINE transpose_PETScMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Subroutine provides an interface to matrix vector multiplication for
!> the MatrixType.
!> @param trans single character input indicating whether or not to use the
!>        transpose of @c A
!> @param thisMatrix derived matrix type.
!> @param alpha the scalar used to scale @c x
!> @param x the vector to multiply with @c A
!> @param beta the scalar used to scale @c y
!> @param y the vector to add to the product of @c A and @c x
!> @param uplo character indicating if @c thisMatrix is upper or lower triangular
!> @param diag character indicating if diagonal of @c thisMatrix should be treated
!> @param incx_in integer containing distance between elements in @c x
!>
    SUBROUTINE matvec_PETScVector(thisMatrix,trans,alpha,x,beta,y,uplo,diag,incx_in)
      CHARACTER(LEN=*),PARAMETER :: myName='matvec_PetscNative'
      TYPE(PETScMatrixType),INTENT(INOUT) :: thisMatrix
      CLASS(VectorType),INTENT(INOUT) :: x
      CHARACTER(LEN=1),OPTIONAL,INTENT(IN) :: trans
      REAL(SRK),INTENT(IN),OPTIONAL :: alpha
      REAL(SRK),INTENT(IN),OPTIONAL :: beta
      CLASS(VectorType),INTENT(INOUT) :: y
      CHARACTER(LEN=1),INTENT(IN),OPTIONAL :: uplo
      CHARACTER(LEN=1),INTENT(IN),OPTIONAL :: diag
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx_in
      CHARACTER(LEN=1) :: t,ul,d
      INTEGER(SIK) :: incx
      REAL(SRK) :: a,b
      REAL(SRK),ALLOCATABLE :: tmpmat(:,:),tmpvec(:),tmpy(:)
      INTEGER(SIK) :: i,j
      PetscErrorCode  :: iperr
      TYPE(PETScVectorType) :: dummy
      TYPE(ParamType) :: vecPList

      IF(.NOT. thisMatrix%isInit) THEN
        CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
          "Matrix not initialized.")
        RETURN
      ENDIF

      t='n'
      ul='n'
      d='n'
      incx=1_SIK
      a=1
      b=1
      IF(PRESENT(trans)) t=trans
      IF(PRESENT(uplo)) ul=uplo
      IF(PRESENT(diag)) d=diag
      IF(PRESENT(incx_in)) incx=incx_in
      IF(PRESENT(alpha)) a=alpha
      IF(PRESENT(beta))  b=beta

      SELECTTYPE(x); TYPE IS(RealVectorType)
        SELECTTYPE(y); TYPE IS(RealVectorType)
          ALLOCATE(tmpmat(thisMatrix%n,thisMatrix%n))
          ALLOCATE(tmpvec(x%n))
          ALLOCATE(tmpy(x%n))
          ! stuff into temporary matrix
          DO i=1,thisMatrix%n
            DO j=1,thisMatrix%n
              CALL thisMatrix%get(i,j,tmpmat(i,j))
            ENDDO
          ENDDO
          ! stuff into temporary vector
          CALL x%get(tmpvec)
          CALL y%get(tmpy)
          IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
              alpha,tmpmat,thisMatrix%n,tmpvec,1,beta,tmpy,1)
          ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
              alpha,tmpmat,thisMatrix%n,tmpvec,1,tmpy,1)
          ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
              tmpmat,thisMatrix%n,tmpvec,1,beta,tmpy,1)
          ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
              tmpmat,thisMatrix%n,tmpvec,1,tmpy,1)
          ENDIF
          ! set into return vector
          CALL y%set(tmpy)
        ENDSELECT
      TYPE IS(PETScVectorType) ! x
        SELECTTYPE(y); TYPE IS(PETScVectorType)
          CALL vecPList%add('VectorType -> n',y%n)
          CALL vecPList%add('VectorType -> MPI_Comm_ID',y%comm)
          CALL vecPList%add('VectorType -> nlocal',x%nlocal)
          CALL dummy%init(vecPList)
          IF(.NOT.x%isAssembled) CALL x%assemble()
          IF(.NOT.y%isAssembled) CALL y%assemble()
          IF(.NOT.thisMatrix%isAssembled) CALL thisMatrix%assemble()
          IF(t == 'n') THEN
            CALL MatMult(thisMatrix%a,x%b,dummy%b,iperr)
          ELSE
            CALL MatMultTranspose(thisMatrix%a,x%b,dummy%b,iperr)
          ENDIF
          CALL BLAS_scal(dummy,a)
          CALL BLAS_scal(y,b)
          CALL BLAS_axpy(dummy,y)
          CALL vecPList%clear()
          CALL dummy%clear()
        ENDSELECT
        CLASS DEFAULT
              CALL eMatrixType%raiseError('Incorrect call to '// &
                  modName//'::'//myName//' - This interface is not available.')
      ENDSELECT
    ENDSUBROUTINE matvec_PETScVector
!
!-------------------------------------------------------------------------------
!> @brief Subroutine provides an interface to matrix vector multiplication for
!> the PETSc matrix type and 1-D array vectors.
!> @param trans single character input indicating whether or not to use the
!>        transpose of @c A
!> @param thisMatrix derived matrix type.
!> @param alpha the scalar used to scale @c x
!> @param x the vector to multiply with @c A
!> @param beta the scalar used to scale @c y
!> @param y the vector to add to the product of @c A and @c x
!> @param uplo character indicating if @c thisMatrix is upper or lower triangular
!> @param diag character indicating if diagonal of @c thisMatrix should be treated
!> @param incx_in integer containing distance between elements in @c x
!>
    SUBROUTINE matvec_PETSc(thisMatrix,trans,alpha,x,beta,y,uplo,diag,incx_in)
      CHARACTER(LEN=*),PARAMETER :: myName='matvec_MatrixType'
      TYPE(PETScMatrixType),INTENT(INOUT) :: thisMatrix
      CHARACTER(LEN=1),OPTIONAL,INTENT(IN) :: trans
      REAL(SRK),INTENT(IN),OPTIONAL :: alpha
      REAL(SRK),INTENT(IN) :: x(:)
      REAL(SRK),INTENT(IN),OPTIONAL :: beta
      REAL(SRK),INTENT(INOUT) :: y(:)
      CHARACTER(LEN=1),INTENT(IN),OPTIONAL :: uplo
      CHARACTER(LEN=1),INTENT(IN),OPTIONAL :: diag
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx_in
      !
      REAL(SRK),ALLOCATABLE :: tmpmat(:,:)
      INTEGER(SIK) :: i,j
      CHARACTER(LEN=1) :: t,ul,d
      INTEGER(SIK) :: incx

      IF(thisMatrix%isInit) THEN
        t='n'
        ul='n'
        d='n'
        incx=1_SIK
        IF(PRESENT(trans)) t=trans
        IF(PRESENT(uplo)) ul=uplo
        IF(PRESENT(diag)) d=diag
        IF(PRESENT(incx_in)) incx=incx_in

            ALLOCATE(tmpmat(thisMatrix%n,thisMatrix%n))
            ! stuff into temporary matrix
            DO i=1,thisMatrix%n
              DO j=1,thisMatrix%n
                CALL thisMatrix%get(i,j,tmpmat(i,j))
              ENDDO
            ENDDO

            IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
              CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
                alpha,tmpmat,thisMatrix%n,x,1,beta,y,1)
            ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
              CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
                alpha,tmpmat,thisMatrix%n,x,1,y,1)
            ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
              CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
                tmpmat,thisMatrix%n,x,1,beta,y,1)
            ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
              CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
                tmpmat,thisMatrix%n,x,1,y,1)
            ENDIF
            DEALLOCATE(tmpmat)
      ENDIF
    ENDSUBROUTINE matvec_PETSc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine provides an interface to matrix matrix multiplication for
!> the PETScMatrixType. alpha*A*B+beta*C
!> @param A a derived matrix type to multiply by @c B
!> @param B a derived matrix type to multiply by @c A
!> @param C a derived matrix type to add to the product of @c A and @c B
!>          If C is not initialized when the subroutine is called, it will be
!>          initialized and set to all zeros
!> @param transA single character input indicating whether or not to use the
!>        transpose of @c A
!> @param alpha the scalar used to scale the product of @c A and @cB
!> @param transB single character input indicating whether or not to use the
!>        transpose of @c B
!> @param beta the scalar used to scale @c C
!>
!> NOTE: This interface is inefficient to the point of being not worth
!> providing. We may want to remove these BLAS interfaces to TPL-backed
!> matrices, or at the very least implement them using functionality of tha
!> backing TPLs
    SUBROUTINE matmult_PETSc(A,B,C,alpha,beta,transA,transB)
      CHARACTER(LEN=*),PARAMETER :: myName='matmult_MatrixType'
      TYPE(PETScMatrixType),INTENT(INOUT) :: A
      TYPE(PETScMatrixType),INTENT(INOUT) :: B
      TYPE(PETScMatrixType),INTENT(INOUT) :: C
      REAL(SRK),INTENT(IN),OPTIONAL :: alpha
      REAL(SRK),INTENT(IN),OPTIONAL :: beta
      CHARACTER(LEN=1),OPTIONAL,INTENT(IN) :: transA
      CHARACTER(LEN=1),OPTIONAL,INTENT(IN) :: transB
      !
      INTEGER(SIK) :: i,j
      REAL(SRK),ALLOCATABLE :: tmpA(:,:),tmpB(:,:),tmpC(:,:)
      CHARACTER(LEN=1) :: tA
      CHARACTER(LEN=1) :: tB

      IF(.NOT.(A%isInit .AND. B%isInit .AND. C%isInit)) THEN
        RETURN
      ENDIF

      tA='n'
      IF(PRESENT(transA)) THEN
        tA=transA
      ENDIF
      tB='n'
      IF(PRESENT(transB)) THEN
        tB=transB
      ENDIF

      ALLOCATE(tmpA(A%n,A%n))
      ALLOCATE(tmpB(B%n,B%n))
      ALLOCATE(tmpC(C%n,C%n))
      tmpA=0._SRK
      tmpB=0._SRK
      tmpC=0._SRK
      DO i=1,A%n
        DO j=1,A%n
          CALL A%get(i,j,tmpA(i,j))
        ENDDO
      ENDDO
      DO i=1,B%n
        DO j=1,B%n
          CALL B%get(i,j,tmpB(i,j))
        ENDDO
      ENDDO
      DO i=1,B%n
        DO j=1,B%n
          CALL C%get(i,j,tmpC(i,j))
        ENDDO
      ENDDO
      ! A: Square   B: Square  C: Square
      IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
        CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,alpha,tmpA,tmpB,beta,tmpC)
      ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
        CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,alpha,tmpA,tmpB,tmpC)
      ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
        CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,tmpA,tmpB,beta,tmpC)
      ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
        CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,tmpA,tmpB,tmpC)
      ENDIF

      ! put into return matrix
      DO i=1,C%n
        DO j=1,C%n
          CALL C%set(i,j,tmpC(i,j))
        ENDDO
      ENDDO

      DEALLOCATE(tmpA)
      DEALLOCATE(tmpB)
      DEALLOCATE(tmpC)
    ENDSUBROUTINE matmult_PETSc
#endif

ENDMODULE MatrixTypes_PETSc
