!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module for defining matrix types.
!>
!> The types of matrices defined in this module include a sparse matrix type,
!> square and rectangle dense matrix types, and a square tridiagonal matrix
!> type. Presently, the dense matrices are all assumed to be stored in a general
!> format as opposed to packed, banded, or upper/lower triangular formats.
!> Interfaces to BLAS routines for matrix-vector multiplication and
!> matrix-matrix multiplication are also added to the global generic interfaces
!> for @ref BLAS2::BLAS_matvec "BLAS_matvec" and BLAS_matmat interfaces.
!>
!> The objects are initialized with a parameter list. For valid reference lists
!> see @ref MatrixTypes::MatrixTypes_Declare_ValidParams
!> "MatrixTypes_Declare_ValidParams".
!>
!> NOTE: Sparse formats are CSR formats that match the 3-Array Variation Format
!> of the Intel MKL. However, filling these requires traversing the matrix
!> in row-major format (i.e., increment i in inner loop). This is more how a C
!> programmer would do it, not us Fortran folk.  Something to be aware of.
!>
!> TIBWSFB: This documentation doesnt match the current interface
!> @par EXAMPLES
!> @code
!> PROGRAM ExampleMatrix
!>   TYPE(SparseMatrixType) :: sparse
!>   CLASS(MatrixType),POINTER :: matrix_p => NULL()
!>   TYPE(ParamType) :: params
!>
!>   CALL params%add("MatrixType->n",3_SIK)
!>   CALL params%add("MatrixType->nnz",6_SIK)
!>
!>   CALL sparse%init(params)
!>   CALL params%clear()
!>   CALL sparse%setShape(1,1,1._SRK)
!>   CALL sparse%setShape(1,3,2._SRK)
!>   CALL sparse%setShape(2,3,3._SRK)
!>   CALL sparse%setShape(3,1,4._SRK)
!>   CALL sparse%setShape(3,2,5._SRK)
!>   CALL sparse%setShape(3,3,6._SRK)
!>   CALL sparse%set(1,1,10._SRK)
!>   CALL sparse%set(1,3,20._SRK)
!>   CALL sparse%set(2,3,30._SRK)
!>   CALL sparse%set(3,1,40._SRK)
!>   CALL sparse%set(3,2,50._SRK)
!>   CALL sparse%set(3,3,60._SRK)
!>   CALL sparse%clear()
!>
!>   ! Create a PETSc matrix using the factory
!>   CALL params%add("MatrixType->n",3_SIK)
!>   CALL params%add("MatrixType->nnz",6_SIK)
!>   CALL params%add("MatrixType->matType",SPARSE)
!>   CALL params%add("MatrixType->engine",VM_PETSC)
!>   CALL MatrixFactory(matrix_p,params)
!>
!>   ! Clean up
!>   CALL matrix_p%clear()
!>   DEALLOCATE(matrix_p)
!>
!> ENDPROGRAM ExampleMatrix
!> @endcode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE MatrixTypes
USE IntrType
USE ExceptionHandler
USE Allocs
USE BLAS2,           ONLY: BLAS2_matvec => BLAS_matvec
USE BLAS3,           ONLY: BLAS3_matmult => BLAS_matmat
USE ParameterLists
USE VectorTypes
USE MatrixTypes_Base
USE MatrixTypes_Native
USE MatrixTypes_Trilinos
USE MatrixTypes_PETSc
USE trilinos_interfaces
IMPLICIT NONE

#ifdef HAVE_MPI
#include <mpif.h>
#endif

PRIVATE
!
! List of public members
PUBLIC :: MatrixType
PUBLIC :: MatrixFactory
PUBLIC :: DistributedMatrixFactory
PUBLIC :: MatrixResemble
PUBLIC :: SquareMatrixType
PUBLIC :: RectMatrixType
PUBLIC :: DistributedMatrixType
! Matrix structure enumerations
PUBLIC :: SPARSE,DENSESQUARE,DENSERECT,TRIDIAG, &
    BANDED,DISTRIBUTED_BANDED,DISTR_BLOCKBANDED
! Matrix-Vector engine enumerations
PUBLIC :: VM_PETSC,VM_TRILINOS,VM_NATIVE
! Parameter list setup/teardown
PUBLIC :: MatrixTypes_Declare_ValidParams
PUBLIC :: MatrixTypes_Clear_ValidParams
! Native implementations
PUBLIC :: DenseSquareMatrixType
PUBLIC :: DenseRectMatrixType
PUBLIC :: TriDiagMatrixType
PUBLIC :: BandedMatrixType
PUBLIC :: DistributedBandedMatrixType
PUBLIC :: DistributedBlockBandedMatrixType
PUBLIC :: SparseMatrixType
INTEGER(SIK),PARAMETER,PUBLIC :: MATVEC_SLOTS=2
! PETSc implementations
#ifdef FUTILITY_HAVE_PETSC
PUBLIC :: PETScMatrixType
#endif
#ifdef FUTILITY_HAVE_Trilinos
PUBLIC :: TrilinosMatrixType
#endif

PUBLIC :: eMatrixType
PUBLIC :: BLAS_matvec
PUBLIC :: BLAS_matmult

!> @brief Adds to the @ref BLAS2::BLAS_matvec "BLAS_matvec" interface so that
!> the matrix types defined in this module are also supported.
INTERFACE BLAS_matvec
  !> @copybrief MatrixTypes::matvec_MatrixType
  !> @copydetails MatrixTypes::matvec_MatrixType
  MODULE PROCEDURE matvec_MatrixType
  !> @copybrief MatrixTypes::matvec_MatrixTypeVectorType
  !> @copydetails MatrixTypes::matvec_MatrixTypeVectorType
  MODULE PROCEDURE matvec_MatrixTypeVectorType
ENDINTERFACE BLAS_matvec

!> @brief Adds to the @ref BLAS3::BLAS_matmult "BLAS_matmat" interface so that
!> the matrix types defined in this module are also supported.
INTERFACE BLAS_matmult
  !> @copybrief MatrixTypes::matmult_MatrixType
  !> @copydetails MatrixTypes::matmult_MatrixType
  MODULE PROCEDURE matmult_MatrixType
ENDINTERFACE BLAS_matmult

!> @brief Adds strsv_all and dtrsv_all routines for sparse matrices to the
!> @ref BLAS2::BLAS_matvec "BLAS_matvec" interface
!  INTERFACE trsv_sparse
  !> @copybrief MatrixTypes::strsv_all_sparse
  !> @copydetails MatrixTypes::strsv_all_sparse
!    MODULE PROCEDURE strsv_all_sparse
  !> @copybrief MatrixTypes::dtrsv_all_sparse
  !> @copydetails MatrixTypes::dtrsv_all_sparse
!    MODULE PROCEDURE dtrsv_all_sparse
!  ENDINTERFACE trsv_sparse

!> Name of module
CHARACTER(LEN=*),PARAMETER :: modName='MATRIXTYPES'

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Abstract factory for all enabled MatrixTypes
!>
!> @param matrix a pointer to the base MatrixType that will store the
!> constructed matrix. Should be NULL
!> @param params a parameter list to use to determine the type of and construct
!> the matrix
!>
!> This is an abstract factory routine for the base MatrixTypes. It uses the
!> matType and engine parameters on the passed parameter list to determine which
!> type of MatrixType to which the passed pointer should be allocated. It then
!> uses the same parameter list to initialize the matrix.
!>
!> The valid matrix engines (if enabled at configure time) are MV_NATIVE,
!> MV_TRILINOS and MV_PETSC.
!> The valid matTypes are those enumerated in the MatrixTypes_Base module.
!>
!> The purpose of such a routine is to hide from the user which specific
!> MatrixTypes are available and reduce the need for SELECT TYPEing the matrices
!> later. This also hides the #ifdef guards needed to safely interact with each
!> of the optional engines.
SUBROUTINE MatrixFactory(matrix, params)
  CHARACTER(LEN=*),PARAMETER :: myName="MatrixFactory"
  CLASS(MatrixType),POINTER,INTENT(INOUT) :: matrix
  CLASS(ParamType),INTENT(IN) :: params
  !
  INTEGER(SIK) :: engine,matType
  CLASS(DistributedMatrixType),POINTER :: dist_p

  IF(ASSOCIATED(matrix)) THEN
    CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
        "Matrix pointer already allocated")
    RETURN
  ENDIF

  dist_p => NULL()

  ! Default to dative, dense matrix
  engine=VM_NATIVE
  matType=DENSESQUARE

  IF(params%has("MatrixType->engine")) THEN
    CALL params%get("MatrixType->engine", engine)
  ENDIF

  IF(params%has("MatrixType->matType")) THEN
    CALL params%get("MatrixType->matType", matType)
  ELSE
    CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
        "no matrix TYPE specified")
  ENDIF

  SELECTCASE(engine)
  CASE(VM_NATIVE)
    SELECTCASE(matType)
    CASE(DENSESQUARE)
      ALLOCATE(DenseSquareMatrixType :: matrix)
    CASE(DENSERECT)
      ALLOCATE(DenseRectMatrixType :: matrix)
    CASE(SPARSE)
      ALLOCATE(SparseMatrixType :: matrix)
    CASE(TRIDIAG)
      ALLOCATE(TriDiagMatrixType :: matrix)
    CASE(BANDED)
      ALLOCATE(BandedMatrixType :: matrix)
    CASE(DISTRIBUTED_BANDED)
      ALLOCATE(DistributedBandedMatrixType :: matrix)
    CASE(DISTR_BLOCKBANDED)
      ALLOCATE(DistributedBlockBandedMatrixType :: matrix)
    CASE DEFAULT
      CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
          "Unrecognized matrix structure requested")
    ENDSELECT
  CASE(VM_TRILINOS)
    CALL DistributedMatrixFactory(dist_p, params)
    matrix => dist_p
    RETURN
  CASE(VM_PETSC)
    CALL DistributedMatrixFactory(dist_p, params)
    matrix => dist_p
    RETURN
  CASE DEFAULT
    CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
        "Unsupported matrix engine requested.")
  ENDSELECT

  CALL matrix%init(params)
ENDSUBROUTINE MatrixFactory
!
!-------------------------------------------------------------------------------
!> @brief Abstract factory for all enabled DistributedMatrixTypes
!>
!> @param matrix a pointer to the DistributedMatrixType that will store the
!> constructed matrix. Should be NULL
!> @param params a parameter list to use to determine the type of and construct
!> the matrix
!>
!> This is similar to the MatrixType factory, but specifically interacts with
!> DistributedMatrixType pointers. This distinction in necessary for client code
!> that knows that it wants one of the DistributedMatrixTypes and therefore uses
!> a pointer to such.
SUBROUTINE DistributedMatrixFactory(matrix, params)
  CHARACTER(LEN=*),PARAMETER :: myName="DistributedMatrixFactory"
  CLASS(DistributedMatrixType),POINTER,INTENT(INOUT) :: matrix
  CLASS(ParamType),INTENT(IN) :: params
  !
  INTEGER(SIK) :: engine,matType

  IF(ASSOCIATED(matrix)) THEN
    CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
        "Matrix pointer already allocated")
    RETURN
  ENDIF

  matType=SPARSE

  IF(params%has("MatrixType->engine")) THEN
    CALL params%get("MatrixType->engine", engine)
  ENDIF

  IF(params%has("MatrixType->matType")) THEN
    CALL params%get("MatrixType->matType", matType)
  ENDIF

  SELECTCASE(engine)
  CASE(VM_TRILINOS)
#ifdef FUTILITY_HAVE_Trilinos
    IF(matType == SPARSE) THEN
      ALLOCATE(TrilinosMatrixType :: matrix)
    ELSE
      CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
          "Trilinos matrix should be sparse")
    ENDIF
#else
    CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
        "Futility was not compiled with Trilinos support. Recompile "// &
      "with Trilinos to create PETSc matrices.")
#endif
  CASE(VM_PETSC)
#ifdef FUTILITY_HAVE_PETSC
    IF(matType == SPARSE .OR. matType == DENSESQUARE) THEN
      ALLOCATE(PETScMatrixType :: matrix)
    ELSE
      CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
          "PETSc matrix should be sparse")
    ENDIF
#else
    CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
        "Futility was not compiled with PETSc support. Recompile with "// &
      "PETSc to create PETSc matrices.")
#endif
  CASE DEFAULT
    CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
        "Unsupported distributed matrix engine requested.")
  ENDSELECT

  CALL matrix%init(params)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Abstractly reproduce a new Matrix of the same type passed in
!>
!> @param dest the matrix pointer to allocate
!> @param source the matrix to use when determining the proper type for dest
!> @param params the parameter list to use when constructing the dest matrix
!>
!> This routine uses the matrix passed in as source to allocate a new matrix,
!> dest, of the same type. The passed parameter list is then used to initialize
!> the dest matrix.
!> Such a routine exists to make it easy for client code to create a new matrix
!> that will be compatible with an existing matrix, without needed to know which
!> type the existing matrix is. This cuts down on #ifdef guards and SELECT TYPEs
SUBROUTINE MatrixResemble(dest, source, params)
  CHARACTER(LEN=*),PARAMETER :: myName="MatrixResemble"
  CLASS(MatrixType),POINTER,INTENT(INOUT) :: dest
  CLASS(MatrixType),POINTER,INTENT(IN) :: source
  CLASS(ParamType),INTENT(INOUT) :: params

  IF(.NOT. ASSOCIATED(source)) THEN
    CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
        "Source matrix is not associated")
    RETURN
  ENDIF

  IF(ASSOCIATED(dest)) THEN
    CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
        "Destination pointer is already associated")
    RETURN
  ENDIF

  SELECTTYPE(source)
  TYPE IS(DenseSquareMatrixType)
    ALLOCATE(DenseSquareMatrixType :: dest)
  TYPE IS(DenseRectMatrixType)
    ALLOCATE(DenseRectMatrixType :: dest)
  TYPE IS(TriDiagMatrixType)
    ALLOCATE(TriDiagMatrixType :: dest)
  TYPE IS(BandedMatrixType)
    ALLOCATE(BandedMatrixType :: dest)
  TYPE IS(DistributedBandedMatrixType)
    ALLOCATE(DistributedBandedMatrixType :: dest)
  TYPE IS(DistributedBlockBandedMatrixType)
    ALLOCATE(DistributedBlockBandedMatrixType :: dest)
  TYPE IS(SparseMatrixType)
    ALLOCATE(SparseMatrixType :: dest)
#ifdef FUTILITY_HAVE_PETSC
  TYPE IS(PETScMatrixType)
    ALLOCATE(PETScMatrixType :: dest)
#endif
#ifdef FUTILITY_HAVE_Trilinos
  TYPE IS(TrilinosMatrixType)
    ALLOCATE(TrilinosMatrixType :: dest)
#endif
  CLASS DEFAULT
    CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
        "Unsupported source matrix type")
  ENDSELECT
  CALL dest%init(params)
ENDSUBROUTINE MatrixResemble
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
SUBROUTINE matvec_MatrixType(thisMatrix,trans,alpha,x,beta,y,uplo,diag,incx_in)
  CHARACTER(LEN=*),PARAMETER :: myName='matvec_MatrixType'
  CLASS(MatrixType),INTENT(INOUT) :: thisMatrix
  CHARACTER(LEN=1),OPTIONAL,INTENT(IN) :: trans
  REAL(SRK),INTENT(IN),OPTIONAL :: alpha
  REAL(SRK),INTENT(IN) :: x(:)
  REAL(SRK),INTENT(IN),OPTIONAL :: beta
  REAL(SRK),INTENT(INOUT) :: y(:)
  CHARACTER(LEN=1),INTENT(IN),OPTIONAL :: uplo
  CHARACTER(LEN=1),INTENT(IN),OPTIONAL :: diag
  INTEGER(SIK),INTENT(IN),OPTIONAL :: incx_in
  !
  CHARACTER(LEN=1) :: t,ul,d
  REAL(SRK) :: a,b
  INTEGER(SIK),ALLOCATABLE :: idxMult(:)
  INTEGER(SIK) :: bIdx,incx

  SELECTTYPE(mat => thisMatrix)
#ifdef FUTILITY_HAVE_PETSC
  TYPE IS(PETScMatrixType)
    CALL matvec_PETSc(mat,trans,alpha,x,beta,y,uplo,diag,incx_in)
    RETURN
#endif
  ENDSELECT

  IF(thisMatrix%isInit) THEN
    t='n'
    ul='n'
    d='n'
    incx=1_SIK
    IF(PRESENT(trans)) t=trans
    IF(PRESENT(uplo)) ul=uplo
    IF(PRESENT(diag)) d=diag
    IF(PRESENT(incx_in)) incx=incx_in

    SELECT TYPE(thisMatrix)
    TYPE IS(DenseSquareMatrixType)
      IF(ul /= 'n') THEN
        y=x
        CALL BLAS2_matvec(ul,t,d,thisMatrix%a,y,incx)
      ELSEIF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
        CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
            alpha,thisMatrix%a,thisMatrix%n,x,1,beta,y,1)
      ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
        CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
            alpha,thisMatrix%a,thisMatrix%n,x,1,y,1)
      ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
        CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
            thisMatrix%a,thisMatrix%n,x,1,beta,y,1)
      ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
        CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
            thisMatrix%a,thisMatrix%n,x,1,y,1)
      ENDIF
    TYPE IS(DenseRectMatrixType)
      IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
        CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%m, &
            alpha,thisMatrix%a,thisMatrix%n,x,1,beta,y,1)
      ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
        CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%m, &
            alpha,thisMatrix%a,thisMatrix%n,x,1,y,1)
      ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
        CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%m, &
            thisMatrix%a,thisMatrix%n,x,1,beta,y,1)
      ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
        CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%m, &
            thisMatrix%a,thisMatrix%n,x,1,y,1)
      ENDIF
    TYPE IS(SparseMatrixType)
      IF(ul /= 'n') THEN
        y=x
        CALL trsv_sparse(ul,t,d,thisMatrix%a,thisMatrix%ia,thisMatrix%ja,y,incx)
      ELSEIF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
        CALL BLAS2_matvec(thisMatrix%n,thisMatrix%nnz,thisMatrix%ia, &
            thisMatrix%ja,thisMatrix%a,alpha,x,beta,y)
      ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
        CALL BLAS2_matvec(thisMatrix%n,thisMatrix%nnz,thisMatrix%ia, &
            thisMatrix%ja,thisMatrix%a,alpha,x,y)
      ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
        CALL BLAS2_matvec(thisMatrix%n,thisMatrix%nnz,thisMatrix%ia, &
            thisMatrix%ja,thisMatrix%a,x,beta,y)
      ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
        CALL BLAS2_matvec(thisMatrix%n,thisMatrix%nnz,thisMatrix%ia, &
            thisMatrix%ja,thisMatrix%a,x,y)
      ENDIF
    TYPE IS(BandedMatrixType)
      IF(ul /= 'n' .OR. d /= 'n' .OR. t /= 'n' .OR. incx /= 1) THEN
        CALL eMatrixType%raiseError('Incorrect call to '// &
            modName//'::'//myName//' - This interface is not implemented.')
      ENDIF

      a=1.0_SRK
      b=1.0_SRK
      IF (PRESENT(alpha)) a=alpha
      IF (PRESENT(beta)) b=beta

      IF (b == 0.0_SRK) THEN
        y=0.0_SRK
      ELSEIF (b /= 1.0_SRK) THEN
        y=y*b
      ENDIF

      ! This can probably be optimized for the a /= 1 case
      IF (a == 1.0_SRK) THEN
        DO bIdx=1,SIZE(thisMatrix%bandIdx)
          idxMult=thisMatrix%bands(bIdx)%jIdx-thisMatrix%bandIdx(bIdx)
          y(idxMult)=y(idxMult)+thisMatrix%bands(bIdx)%elem*x(thisMatrix%bands(bIdx)%jIdx)
        ENDDO
      ELSEIF (a == -1.0_SRK) THEN
        DO bIdx=1,SIZE(thisMatrix%bandIdx)
          idxMult=thisMatrix%bands(bIdx)%jIdx-thisMatrix%bandIdx(bIdx)
          y(idxMult)=y(idxMult)-thisMatrix%bands(bIdx)%elem*x(thisMatrix%bands(bIdx)%jIdx)
        ENDDO
      ELSEIF (a /= 0.0_SRK) THEN
        DO bIdx=1,SIZE(thisMatrix%bandIdx)
          idxMult=thisMatrix%bands(bIdx)%jIdx-thisMatrix%bandIdx(bIdx)
          y(idxMult)=y(idxMult)+a*thisMatrix%bands(bIdx)%elem*x(thisMatrix%bands(bIdx)%jIdx)
        ENDDO
      ENDIF

    CLASS IS(DistributedBandedMatrixType)
      CALL eMatrixType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - This interface is not available.')
    CLASS DEFAULT
      CALL eMatrixType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - This interface is not available.')
    ENDSELECT
  ENDIF
ENDSUBROUTINE matvec_MatrixType
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
!> TODO: This is more of a mess than it needs to be, and should be resturctured
!> to match the matvec implementation. Split up the functionality and store
!> closer to their respective implementations.
SUBROUTINE matvec_MatrixTypeVectorType(thisMatrix,trans,alpha,x,beta,y,uplo,diag,incx_in)
  CHARACTER(LEN=*),PARAMETER :: myName='matvec_MatrixTypeVectorType'
  CLASS(MatrixType),INTENT(INOUT) :: thisMatrix
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

#ifdef FUTILITY_HAVE_PETSC
  SELECTTYPE(mat => thisMatrix); TYPE IS(PETScMatrixType)
    CALL matvec_PETScVector(mat,trans,alpha,x,beta,y,uplo,diag,incx_in)
    RETURN
  ENDSELECT
#endif

#ifdef FUTILITY_HAVE_Trilinos
  SELECTTYPE(mat => thisMatrix); TYPE IS(TrilinosMatrixType)
    CALL matvec_TrilinosVector(mat,trans,alpha,x,beta,y,uplo,diag,incx_in)
    RETURN
  ENDSELECT
#endif

  IF(thisMatrix%isInit) THEN
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

    SELECT TYPE(x)
    TYPE IS(RealVectorType)
      SELECT TYPE(y)
      TYPE IS(RealVectorType)
        SELECT TYPE(thisMatrix)
        TYPE IS(DenseSquareMatrixType)
          IF(ul /= 'n') THEN
            y%b=x%b
            CALL BLAS2_matvec(ul,t,d,thisMatrix%a,y%b,incx)
          ELSEIF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
                alpha,thisMatrix%a,thisMatrix%n,x%b,1,beta,y%b,1)
          ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
                alpha,thisMatrix%a,thisMatrix%n,x%b,1,y%b,1)
          ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
                thisMatrix%a,thisMatrix%n,x%b,1,beta,y%b,1)
          ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%n, &
                thisMatrix%a,thisMatrix%n,x%b,1,y%b,1)
          ENDIF
        TYPE IS(DenseRectMatrixType)
          IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%m, &
                alpha,thisMatrix%a,thisMatrix%n,x%b,1,beta,y%b,1)
          ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%m, &
                alpha,thisMatrix%a,thisMatrix%n,x%b,1,y%b,1)
          ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%m, &
                thisMatrix%a,thisMatrix%n,x%b,1,beta,y%b,1)
          ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS2_matvec(t,thisMatrix%n,thisMatrix%m, &
                thisMatrix%a,thisMatrix%n,x%b,1,y%b,1)
          ENDIF
        TYPE IS(SparseMatrixType)
          IF(ul /= 'n') THEN
            y%b=x%b
            CALL trsv_sparse(ul,t,d,thisMatrix%a,thisMatrix%ia,thisMatrix%ja,y%b,incx)
          ELSEIF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS2_matvec(thisMatrix%n,thisMatrix%nnz,thisMatrix%ia, &
                thisMatrix%ja,thisMatrix%a,alpha,x%b,beta,y%b)
          ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS2_matvec(thisMatrix%n,thisMatrix%nnz,thisMatrix%ia, &
                thisMatrix%ja,thisMatrix%a,alpha,x%b,y%b)
          ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS2_matvec(thisMatrix%n,thisMatrix%nnz,thisMatrix%ia, &
                thisMatrix%ja,thisMatrix%a,x%b,beta,y%b)
          ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS2_matvec(thisMatrix%n,thisMatrix%nnz,thisMatrix%ia, &
                thisMatrix%ja,thisMatrix%a,x%b,y%b)
          ENDIF
        TYPE IS(BandedMatrixType)
          CALL matvec_MatrixType(thisMatrix,trans=t,alpha=a,X=x%b,beta=b, &
              Y=y%b,uplo=ul,diag=d,incx_in=incx)
        CLASS IS(DistributedBandedMatrixType)
          CALL matvec_MatrixType(thisMatrix,trans=t,alpha=a,X=x%b,beta=b, &
              Y=y%b,uplo=ul,diag=d,incx_in=incx)
        CLASS DEFAULT
          CALL eMatrixType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - This interface is not available.')
        ENDSELECT
      CLASS DEFAULT
        CALL eMatrixType%raiseError('Incorrect call to '// &
            modName//'::'//myName//' - This interface is not available.')
      ENDSELECT
#ifdef HAVE_MPI
    TYPE IS(NativeDistributedVectorType)
      SELECT TYPE(y)
      TYPE IS(NativeDistributedVectorType)
        SELECT TYPE(thisMatrix)
        CLASS IS(DistributedBandedMatrixType)
          CALL matvec_DistrBandedMatrixType(thisMatrix,x%b,y%b,t,ul,d,incx,a,b)
        CLASS DEFAULT
          CALL eMatrixType%raiseError('Incorrect call to '//modName//'::'// &
              myName//' - This interface is not available.')
        ENDSELECT
      CLASS DEFAULT
        CALL eMatrixType%raiseError('Incorrect call to '// &
            modName//'::'//myName//' - This interface is not available.')
      ENDSELECT
#endif
    CLASS DEFAULT
      CALL eMatrixType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - This interface is not available.')
    ENDSELECT
  ENDIF
ENDSUBROUTINE matvec_MatrixTypeVectorType

!
!-------------------------------------------------------------------------------
!> @brief Parallel algorithm to perform a matvec operation with banded matrices.
!> @param thisMatrix derived matrix type.
!> @param x the vector to multiply with @c A
!> @param y the vector to add to the product of @c A and @c x
!> @param t single character input indicating whether or not to use the
!>        transpose of @c A
!> @param ul character indicating if @c thisMatrix is upper or lower triangular
!> @param d character indicating if diagonal of @c thisMatrix should be treated
!> @param incx integer containing distance between elements in @c x
!> @param a the scalar used to scale @c x
!> @param b the scalar used to scale @c y
!>
SUBROUTINE matvec_DistrBandedMatrixType(thisMatrix,x,y,t,ul,d,incx,a,b)
  CLASS(DistributedBandedMatrixType),INTENT(INOUT) :: thisMatrix
  REAL(SRK),INTENT(INOUT) :: x(:)
  CHARACTER(LEN=1),INTENT(IN) :: t
  REAL(SRK),INTENT(IN) :: a
  REAL(SRK),INTENT(IN) :: b
  REAL(SRK),INTENT(INOUT) :: y(:)
  CHARACTER(LEN=1),INTENT(IN) :: ul
  CHARACTER(LEN=1),INTENT(IN) :: d
  INTEGER(SIK),INTENT(IN) :: incx
  INTEGER(SIK) :: rank,k
  INTEGER(SIK),ALLOCATABLE :: recvIdx(:,:), sendIdx(:,:)
  REAL(SRK),ALLOCATABLE :: recvResult(:,:),sendResult(:,:),tmpProduct(:)
  INTEGER(SIK) :: sendRequests(MATVEC_SLOTS),sendIdxRequests(MATVEC_SLOTS)
  INTEGER(SIK) :: recvRequests(MATVEC_SLOTS),recvIdxRequests(MATVEC_SLOTS)
  INTEGER(SIK) :: lowIdx,highIdx,sendCounter,recvCounter
#ifdef HAVE_MPI
  ! Get rank
  INTEGER(SIK) :: ctRecv(MATVEC_SLOTS)
  INTEGER(SIK) :: i,l,idxTmp,cnt,ctDefault,mpierr
  CALL MPI_Comm_rank(thisMatrix%comm,rank,mpierr)
#else
  rank=0
#endif
  ! NOTE: incx,ul,d,t are NOT USED

  ! We will store our send/recv arrays in an array of length MATVEC_SLOTS
  ! This allows one to be used for computation and the rest for
  ! communication.
  ! Which one to write to will be determined by *Counter MOD MATVEC_SLOTS
  sendCounter=0
  recvCounter=0
  sendRequests=0
  sendIdxRequests=0
  recvRequests=0
  recvIdxRequests=0
  ! The recv/sendResult array will sometimes hold it's full length,
  ! and sometimes less.
  ! The variable "cnt" will be used to determine this at
  ! each iteration
  ALLOCATE(recvIdx(thisMatrix%iOffsets(2),MATVEC_SLOTS))
  ALLOCATE(recvResult(thisMatrix%iOffsets(2),MATVEC_SLOTS))
  ALLOCATE(sendIdx(thisMatrix%iOffsets(2),MATVEC_SLOTS))
  ALLOCATE(sendResult(thisMatrix%iOffsets(2),MATVEC_SLOTS))
  ALLOCATE(tmpProduct(thisMatrix%iOffsets(rank+2)- &
      thisMatrix%iOffsets(rank+1)))

  ! First, take care of locally held data.
  SELECT TYPE(thisMatrix)
  TYPE IS(DistributedBlockBandedMatrixType)
    IF (.NOT. thisMatrix%blockMask) THEN
      DO k=1,thisMatrix%nlocalBlocks
        lowIdx=(k-1)*thisMatrix%blockSize+1
        highIdx=lowIdx-1+thisMatrix%blockSize
        CALL matvec_MatrixType(THISMATRIX=thisMatrix%blocks(k),X=x(lowIdx:highIdx), &
            Y=tmpProduct(lowIdx:highIdx),ALPHA=1.0_SRK,BETA=0.0_SRK)
      ENDDO
      IF (thisMatrix%chunks(rank+1)%isInit) THEN
        CALL BLAS_matvec(THISMATRIX=thisMatrix%chunks(rank+1),X=x, &
            y=tmpProduct,ALPHA=1.0_SRK,BETA=1.0_SRK)
      ENDIF
    ELSE
      IF (thisMatrix%chunks(rank+1)%isInit) THEN
        CALL BLAS_matvec(THISMATRIX=thisMatrix%chunks(rank+1),X=x, &
            y=tmpProduct,ALPHA=1.0_SRK,BETA=0.0_SRK)
      ELSE
        tmpProduct=0.0_SRK
      ENDIF
    ENDIF
  TYPE IS(DistributedBandedMatrixType)
    IF (thisMatrix%chunks(rank+1)%isInit) THEN
      CALL BLAS_matvec(THISMATRIX=thisMatrix%chunks(rank+1),X=x, &
          y=tmpProduct,ALPHA=1.0_SRK,BETA=0.0_SRK)
    ELSE
      tmpProduct=0.0_SRK
    ENDIF
  CLASS DEFAULT
    tmpProduct = 0.0_SRK
  ENDSELECT

#ifdef HAVE_MPI
  ! On each rank, loop over the chunks held (top to bottom)
  DO i=1,SIZE(thisMatrix%iOffsets)-1
    ctDefault=thisMatrix%iOffsets(i+1)-thisMatrix%iOffsets(i)
    IF (rank+1 == i) THEN
      ! We will be receiving data from other processes
      ! Find which other chunks in this row we need to
      ! communicate with
      DO k=1,SIZE(thisMatrix%bandSizes,1)
        IF (ASSOCIATED(thisMatrix%bandSizes(k)%p)) THEN
          IF (thisMatrix%bandSizes(k)%p(1) < 0) THEN
            ! We are receiving a whole vector at once
            recvCounter=recvCounter+1
            idxTmp=MOD(recvCounter,MATVEC_SLOTS)+1
            ! If we've filled up the available storage, we need
            ! to wait for communication to finish up
            CALL pop_recv(tmpProduct,recvResult,recvIdx,ctRecv,idxTmp, &
                recvRequests, recvIdxRequests)
            ctRecv(MOD(recvCounter,MATVEC_SLOTS)+1)=ctDefault
            CALL MPI_IRecv(recvResult(1:ctDefault,idxTmp), ctDefault, &
                MPI_DOUBLE_PRECISION,k-1,0,thisMatrix%comm, &
                recvRequests(idxTmp),mpierr)
          ELSE
            ! We are receiving multiple sparse chunks
            DO l=1,SIZE(thisMatrix%bandSizes(k)%p)
              recvCounter=recvCounter+1
              idxTmp=MOD(recvCounter,MATVEC_SLOTS)+1
              ! If we've filled up the available storage, we need
              ! to wait for communication to finish up
              CALL pop_recv(tmpProduct,recvResult,recvIdx,ctRecv,idxTmp, &
                  recvRequests, recvIdxRequests)
              ctRecv(idxTmp)=-thisMatrix%bandSizes(k)%p(l)
              CALL MPI_IRecv(recvIdx(1:ctRecv(idxTmp),idxTmp),-ctRecv(idxTmp), &
                  MPI_INTEGER,k-1,0,thisMatrix%comm,recvIdxRequests(idxTmp), mpierr)
              CALL MPI_IRecv(recvResult(1:ctRecv(idxTmp),idxTmp),-ctRecv(idxTmp), &
                  MPI_DOUBLE_PRECISION,k-1,0,thisMatrix%comm,recvRequests(idxTmp), mpierr)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
      ! We've finished calling irecv. Wait for remaining
      ! requests to finish:
      DO idxTmp=1,MATVEC_SLOTS
        CALL pop_recv(tmpProduct, recvResult, recvIdx, ctRecv, idxTmp, &
            recvRequests, recvIdxRequests)
      ENDDO
    ELSE
      ! We will be sending data to some other process
      IF (thismatrix%chunks(i)%isInit) THEN
        ! Decide whether to send whole vector or multiple sparse
        IF (2*thisMatrix%chunks(i)%nnz/3 >= thisMatrix%chunks(i)%n) THEN
          sendCounter=sendCounter+1
          idxTmp=MOD(sendCounter,MATVEC_SLOTS)+1
          ! Check if we can safely write to sendRequests
          CALL pop_send(sendResult,sendIdx,idxTmp,sendRequests,sendIdxRequests)
          CALL BLAS_matvec(THISMATRIX=thisMatrix%chunks(i),X=x,&
              y=sendResult(1:ctDefault,idxTmp),ALPHA=1.0_SRK,BETA=0.0_SRK)
          CALL MPI_ISend(sendResult(1:ctDefault,idxTmp),ctDefault, &
              MPI_DOUBLE_PRECISION,i-1,0,thisMatrix%comm,sendRequests(idxTmp),mpierr)
        ELSE
          ! Send several sparse vectors
          DO l=1,SIZE(thisMatrix%chunks(i)%bandIdx)
            sendCounter=sendCounter+1
            idxTmp=MOD(sendCounter,MATVEC_SLOTS)+1
            CALL pop_send(sendResult,sendIdx,idxTmp,sendRequests,sendIdxRequests)

            ! compute destination indices
            ! perform special-case multiplication from single band here
            cnt=SIZE(thisMatrix%chunks(i)%bands(l)%jIdx)
            sendIdx(1:cnt,idxTmp)=thisMatrix%chunks(i)%bands(l)%jIdx &
                -thisMatrix%chunks(i)%bandIdx(l)
            CALL MPI_ISend(sendIdx(1:cnt,idxTmp),cnt,MPI_INTEGER,i-1,0, &
                thisMatrix%comm,sendIdxRequests(idxTmp), mpierr)
            sendResult(1:cnt,idxTmp)=thisMatrix%chunks(i)%bands(l)%elem &
                *x(thisMatrix%chunks(i)%bands(l)%jIdx)
            CALL MPI_ISend(sendResult(1:cnt,idxTmp),cnt,MPI_DOUBLE_PRECISION, &
                i-1,0,thisMatrix%comm,sendRequests(idxTmp), mpierr)
          ENDDO
        ENDIF
      ENDIF
    ENDIF
  ENDDO
  DO idxTmp=1,MATVEC_SLOTS
    CALL pop_send(sendResult,sendIdx,idxTmp,sendRequests,sendIdxRequests)
  ENDDO
#endif

  ! do y=alpha*reduce+beta*y
  ! We take many special cases in order to effectively use FMA instructions
  ! or reduce the required flops.
  IF (a == 1.0_SRK) THEN
    IF (b == 1.0_SRK) THEN
      y=tmpProduct+y
    ELSEIF (b == 0.0_SRK) THEN
      y=tmpProduct
    ELSEIF (b == -1.0_SRK) THEN
      y=tmpProduct-y
    ELSE
      y=tmpProduct+b*y
    ENDIF
  ELSEIF (a == 0.0_SRK) THEN
    IF (b == 0.0_SRK) THEN
      y=0.0_SRK
    ELSEIF (b == -1.0_SRK) THEN
      y=-y
    ELSEIF (b /= 1.0_SRK) THEN
      y=b*y
    ENDIF
  ELSEIF (a == -1.0_SRK) THEN
    IF (b == 1.0_SRK) THEN
      y=y-tmpProduct
    ELSEIF (b == 0.0_SRK) THEN
      y=-tmpProduct
    ELSEIF (b == -1.0_SRK) THEN
      y=-tmpProduct-y
    ELSE
      y=b*y-tmpProduct
    ENDIF
  ELSE
    IF (b == 1.0_SRK) THEN
      y=a*tmpProduct+y
    ELSEIF (b == 0.0_SRK) THEN
      y=a*tmpProduct
    ELSEIF (b == -1.0_SRK) THEN
      y=a*tmpProduct-y
    ELSE
      y=a*tmpProduct+b*y
    ENDIF
  ENDIF
ENDSUBROUTINE matvec_DistrBandedMatrixType

#ifdef HAVE_MPI
!
!-------------------------------------------------------------------------------
!> @brief Helper routine that handles the movement of data out of the
!>        recieving buffer. This buffer has multiple "slots" that are either
!>        being worked on directly or receiving data from MPI in background.
!> @param acc The accumulating vector
!> @param valBuf The recv buffer for values
!> @param ctBuf Count of elements in each buffer slot
!> @param idxBuf The recv buffer for indices
!> @param idx The buffer slot to pop
!> @param req The array of active requests for data
!> @param idxReq The array of requests for indices
!>
SUBROUTINE pop_recv(acc,valBuf,idxBuf,ctBuf,idx,req,idxReq)
  REAL(SRK), INTENT(INOUT) :: acc(:)
  REAL(SRK), INTENT(INOUT) :: valBuf(:,:)
  INTEGER(SIK), INTENT(INOUT) :: ctBuf(MATVEC_SLOTS)
  INTEGER(SIK), INTENT(INOUT) :: idxBuf(:,:)
  INTEGER(SIK), INTENT(IN) :: idx
  INTEGER(SIK), INTENT(INOUT) :: req(MATVEC_SLOTS)
  INTEGER(SIK), INTENT(INOUT) :: idxReq(MATVEC_SLOTS)
  INTEGER(SIK) :: mpierr

  IF (req(idx) == 0 .AND. idxReq(idx) == 0) RETURN

  CALL MPI_Wait(req(idx),MPI_STATUS_IGNORE,mpierr)
  IF (ctBuf(idx) > 0) THEN
    acc(1:ctBuf(idx))=acc(1:ctBuf(idx))+valBuf(1:ctBuf(idx),idx)
  ELSE
    CALL MPI_Wait(idxReq(idx),MPI_STATUS_IGNORE,mpierr)
    idxReq(idx)=0
    acc(idxBuf(1:-ctBuf(idx),idx))=acc(idxBuf(1:-ctBuf(idx),idx)) &
        +valBuf(1:-ctBuf(idx),idx)
  ENDIF
ENDSUBROUTINE pop_recv
!
!-------------------------------------------------------------------------------
!> @brief Helper routine that keeps of data in the send buffer while
!>        MPI is still working. This buffer has multiple "slots" that are
!>        either being worked on directly or contain data being sent by
!>        MPI in background.
!> @param valBuf The send buffer for values
!> @param idxBuf The send buffer for indices
!> @param idx The buffer slot to pop
!> @param req The array of requests for data
!> @param idxReq The array of requests for indices
!>
SUBROUTINE pop_send(valBuf, idxBuf, idx, req, idxReq)
  REAL(SRK), INTENT(INOUT) :: valBuf(:,:)
  !> The send buffer for indices
  INTEGER(SIK), INTENT(INOUT) :: idxBuf(:,:)
  !> The buffer slot to pop
  INTEGER(SIK), INTENT(IN) :: idx
  !> The array of requests for data
  INTEGER(SIK), INTENT(INOUT) :: req(MATVEC_SLOTS)
  !> The array of requests for indices
  INTEGER(SIK), INTENT(INOUT) :: idxReq(MATVEC_SLOTS)
  INTEGER(SIK) :: mpierr

  IF (req(idx) == 0 .AND. idxReq(idx) == 0) RETURN

  CALL MPI_Wait(req(idx),MPI_STATUS_IGNORE,mpierr)
  IF (idxReq(idx) /= 0) THEN
    CALL MPI_Wait(idxReq(idx),MPI_STATUS_IGNORE,mpierr)
    idxReq(idx)=0
  ENDIF
ENDSUBROUTINE pop_send
#endif
!
!-------------------------------------------------------------------------------
!> @brief Subroutine solves a sparse triangular matrix linear system.
!> @param uplo single character input indicating if an upper (U) or lower (L)
!>        maxtrix is stored in @c A
!> @param trans single character input indicating whether or not to use the
!>        transpose of @c A
!> @param diag single character input indicating whether or not a unity
!>        diagonal is used
!> @param a the double-precision matrix to multiply with @c x, stored in CSR format
!> @param ia the indices of the @c a for the first element in each row
!> @param ja the column indices for each element in @c a
!> @param x the double-precision vector to multiply with @c a
!> @param incx_in the increment to use when looping over elements in @c x
PURE SUBROUTINE trsv_sparse(uplo,trans,diag,a,ia,ja,x,incx_in)
  CHARACTER(LEN=1),INTENT(IN) :: uplo
  CHARACTER(LEN=1),INTENT(IN) :: trans
  CHARACTER(LEN=1),INTENT(IN) :: diag
  REAL(SRK),INTENT(IN) :: a(:)
  INTEGER(SIK),INTENT(IN) :: ia(:)
  INTEGER(SIK),INTENT(IN) :: ja(:)
  REAL(SRK),INTENT(INOUT) :: x(:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: incx_in

  LOGICAL(SBK) :: nounit
  INTEGER(SIK) :: i,ix,j,jx,kx,n,incx
  REAL(SRK),PARAMETER :: ZERO=0.0_SRK
  INTRINSIC MAX
  n=SIZE(ia)-1
  IF(PRESENT(incx_in)) THEN
    incx=incx_in
  ELSE
    incx=1_SIK ! This won't ever be used in the current interfaces
  ENDIF
  ! Check inputs
  IF((trans == 't' .OR. trans == 'T' .OR. trans == 'c' .OR. trans == 'C' .OR. &
      trans == 'n' .OR. trans == 'N') .AND. &
      (uplo == 'u' .OR. uplo == 'U' .OR. uplo == 'l' .OR. uplo == 'L') .AND. &
      (diag == 'u' .OR. diag == 'U' .OR. diag == 'n' .OR. diag == 'N')) THEN

    ! Check whether diagonal is treated as unity or not.
    nounit=.FALSE.
    IF (diag == 'n' .OR. diag == 'N') nounit=.TRUE.
    ! Determine how elements of x are stored, and find "first" element accordingly
    IF(incx <= 0_SIK) THEN
      kx=1-(n-1)*incx ! Elements stored in reverse order (highest index to lowest)
    ELSEIF(incx /= 1) THEN
      kx=1 ! Elements stored from lowest index to highest
    ENDIF

    ! Don't use transpose of matrix
    IF (trans == 'n' .OR. trans == 'N') THEN  ! Form  x := inv( A )*x.
      ! Multiply by upper triangular part of matrix
      IF (uplo == 'u' .OR. uplo == 'U') THEN
        ! Elements of x are stored in increments of 1
        IF(incx == 1) THEN
          DO i=n,1,-1
            DO j=ia(i+1)-1,ia(i),-1
              IF(ja(j) == i) EXIT
              x(i)=x(i)-a(j)*x(ja(j))
            ENDDO
            IF(nounit) x(i)=x(i)/a(j)
          ENDDO
        ! Elements are stored in reverse order and/or by increments other than 1
        ELSE
          ix=ABS(incx)*n-kx
          DO i=n,1,-1
            jx=ABS(incx)*n-kx
            DO j=ia(i+1)-1,ia(i),-1
              IF(ja(j) <= i) EXIT
              x(ix)=x(ix)-a(j)*x(jx)
              IF(j == ia(i+1)-1) THEN
                jx=jx-incx*(n-ja(j)+1)
              ELSE
                jx=jx-incx*(ja(j+1)-ja(j))
              ENDIF
            ENDDO
            IF(nounit) x(ix)=x(ix)/a(j)
            ix=ix-incx
          ENDDO
        ENDIF
      ! Multiply by lower triangular part of matrix
      ELSE
        ! Elements of x are stored in increments of 1
        IF(incx == 1) THEN
          DO i=1,n
            DO j=ia(i),ia(i+1)-1
              IF(ja(j) >= i) EXIT
              x(i)=x(i)-a(j)*x(ja(j))
              IF(j == SIZE(ja)) EXIT
            ENDDO
            IF((ja(j) == i) .AND. nounit) x(i)=x(i)/a(j)
          ENDDO
        ! Elements are stored in reverse order and/or by increments other than 1
        ELSE
          ix=kx
          DO i=1,n
            jx=kx
            DO j=ia(i),ia(i+1)-1
              IF(ja(j) >= i) EXIT
              IF(ja(j) > 1) THEN
                jx=jx+incx*(ja(j)-ja(j-1))
              ELSE
                jx=jx+incx*(ja(j)-1)
              ENDIF
              x(ix)=x(ix)-a(j)*x(jx)
              IF(j == SIZE(ja)) EXIT
            ENDDO
            IF(nounit) x(ix)=x(ix)/a(j)
            ix=ix+incx
          ENDDO
        ENDIF
      ENDIF
    ! Use transpose of matrix
    ELSE
      ! Multiply by upper trianbular part of matrix
      IF (uplo == 'u' .OR. uplo == 'U') THEN
        IF(incx == 1) THEN
          DO j=1,SIZE(ia)-1
            IF(.NOT.(x(j) .APPROXEQA. ZERO)) THEN
              IF (nounit) x(j)=x(j)/a(ia(j))
              DO i=ia(j)+1,ia(j+1)-1
                IF(ja(i) <= j) CYCLE
                x(ja(i))=x(ja(i))-x(j)*a(i)
              ENDDO
            ENDIF
          ENDDO
        ! Elements are stored in reverse order and/or by increments other than 1
        ELSE
          ix=kx
          DO j=1,SIZE(ia)-1
            IF(.NOT.(x(ix) .APPROXEQA. ZERO)) THEN
              IF(nounit) x(ix)=x(ix)/a(ia(j))
              jx=ix
              DO i=ia(j)+1,ia(j+1)-1
                IF(ja(i) <= j) CYCLE
                jx=jx+incx*(ja(i)-ja(i-1))
                x(jx)=x(jx)-x(ix)*a(i)
              ENDDO
            ENDIF
            ix=ix+incx
          ENDDO
        ENDIF
      ! Multiply by lower triangular part of matrix
      ELSE
        ! Elements of x are stored in increments of 1
        IF(incx == 1) THEN
          DO i=n,1,-1
            IF(.NOT.(x(i) .APPROXEQA. ZERO)) THEN
              IF(nounit) x(i)=x(i)/a(ia(i+1)-1)
              DO j=ia(i+1)-2,ia(i),-1
                IF(ja(j) > i) CYCLE
                x(ja(j))=x(ja(j))-a(j)*x(i)
              ENDDO
            ENDIF
          ENDDO
        ! Elements are stored in reverse order and/or by increments other than 1
        ELSE
          ix=ABS(incx)*n-kx
          DO i=n,1,-1
            IF(.NOT.(x(ix) .APPROXEQA. ZERO)) THEN
              DO j=ia(i+1)-1,ia(i),-1
                IF(ja(j) == i) EXIT
              ENDDO
              IF(nounit) x(ix)=x(ix)/a(j)
              jx=ABS(incx)*n-kx
              DO j=ia(i+1)-1,ia(i),-1
                IF(j == ia(i+1)-1) THEN
                  jx=jx-incx*(n-ja(j))
                ELSE
                  jx=jx-incx*(ja(j+1)-ja(j))
                ENDIF
                IF(ja(j) >= i) CYCLE
                x(jx)=x(jx)-a(j)*x(ix)
              ENDDO
            ENDIF
            ix=ix-incx
          ENDDO
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDSUBROUTINE trsv_sparse
!
!-------------------------------------------------------------------------------
!> @brief Subroutine provides an interface to matrix matrix multiplication for
!> the MatrixType. alpha*A*B+beta*C
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
SUBROUTINE matmult_MatrixType(A,B,C,alpha,beta,transA,transB)
  CLASS(MatrixType),INTENT(INOUT) :: A
  CLASS(MatrixType),INTENT(INOUT) :: B
  CLASS(MatrixType),INTENT(INOUT) :: C
  REAL(SRK),INTENT(IN),OPTIONAL :: alpha
  REAL(SRK),INTENT(IN),OPTIONAL :: beta
  CHARACTER(LEN=1),OPTIONAL,INTENT(IN) :: transA
  CHARACTER(LEN=1),OPTIONAL,INTENT(IN) :: transB
  CHARACTER(LEN=1) :: tA
  CHARACTER(LEN=1) :: tB
#ifdef FUTILITY_HAVE_PETSC
  CHARACTER(LEN=*),PARAMETER :: myName='matmult_MatrixType'
  SELECTTYPE(A)
  TYPE IS(PETScMatrixType)
    SELECTTYPE(B)
    TYPE IS(PETScMatrixType)
      SELECTTYPE(C)
      TYPE IS(PETScMatrixType)
        CALL matmult_PETSc(A,B,C,alpha,beta,transA,transB)
      CLASS DEFAULT
        CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
            "Interface not implemented")
      ENDSELECT
    CLASS DEFAULT
      CALL eMatrixType%raiseError(modName//"::"//myName//" - "// &
          "Interface not implemented")
    ENDSELECT
    RETURN
  ENDSELECT
#endif

  IF(A%isInit) THEN
    tA='n'
    IF(PRESENT(transA)) tA=transA
  ENDIF
  IF(B%isInit) THEN
    tB='n'
    IF(PRESENT(transB)) tB=transB
  ENDIF

  IF(A%isInit .AND. B%isInit .AND. C%isInit) THEN
    SELECTTYPE(A)
    TYPE IS(DenseSquareMatrixType)
      SELECTTYPE(B)
      TYPE IS(DenseSquareMatrixType)
        SELECTTYPE(C)
        TYPE IS(DenseSquareMatrixType)
          ! A: Square   B: Square  C: Square
          IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,alpha,A%a,B%a,beta,C%a)
          ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,alpha,A%a,B%a,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,A%a,B%a,beta,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,A%a,B%a,C%a)
          ENDIF
        TYPE IS(DenseRectMatrixType)
          ! A: Square   B: Square  C: Rect
          IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,alpha,A%a,B%a,beta,C%a)
          ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,alpha,A%a,B%a,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,A%a,B%a,beta,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,A%a,B%a,C%a)
          ENDIF
        TYPE IS(SparseMatrixType)
          ! NOT SUPPORTED
        ENDSELECT
      TYPE IS(DenseRectMatrixType)
        SELECTTYPE(C)
        TYPE IS(DenseSquareMatrixType)
          ! A: Square   B: Rect  C: Square
          IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,alpha,A%a,B%a,beta,C%a)
          ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,alpha,A%a,B%a,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,A%a,B%a,beta,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,A%a,B%a,C%a)
          ENDIF
        TYPE IS(DenseRectMatrixType)
          ! A: Square   B: Rect  C: Rect
          IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,alpha,A%a,B%a,beta,C%a)
          ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,alpha,A%a,B%a,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,A%a,B%a,beta,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,A%a,B%a,C%a)
          ENDIF
        TYPE IS(SparseMatrixType)
          ! NOT SUPPORTED
        ENDSELECT
      TYPE IS(SparseMatrixType)
        ! NOT SUPPORTED
      ENDSELECT
    TYPE IS(DenseRectMatrixType)
      SELECTTYPE(B)
      TYPE IS(DenseSquareMatrixType)
        SELECTTYPE(C)
        TYPE IS(DenseSquareMatrixType)
          ! A: Rect   B: Square  C: Square
          IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,alpha,A%a,B%a,beta,C%a)
          ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,alpha,A%a,B%a,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,A%a,B%a,beta,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,A%a,B%a,C%a)
          ENDIF
        TYPE IS(DenseRectMatrixType)
          ! A: Rect   B: Square  C: Rect
          IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,alpha,A%a,B%a,beta,C%a)
          ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,alpha,A%a,B%a,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,A%a,B%a,beta,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,A%a,B%a,C%a)
          ENDIF
        TYPE IS(SparseMatrixType)
          ! NOT SUPPORTED
        ENDSELECT
      TYPE IS(DenseRectMatrixType)
        SELECTTYPE(C)
        TYPE IS(DenseSquareMatrixType)
          ! A: Rect   B: Rect  C: Square
          IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,alpha,A%a,B%a,beta,C%a)
          ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,alpha,A%a,B%a,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,A%a,B%a,beta,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%n,B%n,A%a,B%a,C%a)
          ENDIF
        TYPE IS(DenseRectMatrixType)
          ! A: Rect   B: Rect  C: Rect
          IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,alpha,A%a,B%a,beta,C%a)
          ELSEIF(PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,alpha,A%a,B%a,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,A%a,B%a,beta,C%a)
          ELSEIF(.NOT.PRESENT(alpha) .AND. .NOT.PRESENT(beta)) THEN
            CALL BLAS3_matmult(tA,tB,C%n,C%m,B%n,A%a,B%a,C%a)
          ENDIF
        TYPE IS(SparseMatrixType)
          ! NOT SUPPORTED
        ENDSELECT
      TYPE IS(SparseMatrixType)
        ! NOT SUPPORTED
      ENDSELECT
    TYPE IS(SparseMatrixType)
      ! NOT SUPPORTED
    ENDSELECT
  ENDIF
ENDSUBROUTINE matmult_MatrixType
!
ENDMODULE MatrixTypes
