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
!> NOTE: Sparse formats are CSR formats that match the 3-Array Variation Format
!> of the Intel MKL. However, filling these requires traversing the matrix
!> in row-major format (i.e., increment i in inner loop). This is more how a C
!> programmer would do it, not us Fortran folk.  Something to be aware of.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref Allocs "Allocs": @copybrief Allocs
!>  - @ref BLAS2 "BLAS2": @copybrief BLAS2
!>  - @ref BLAS3 "BLAS3": @copybrief BLAS3
!>
!> @par EXAMPLES
!> @code
!> PROGRAM ExampleMatrix
!>   TYPE(SparseMatrixType) :: sparse
!>
!>   CALL sparse%init(3,6)
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
!> ENDPROGRAM ExampleMatrix
!> @endcode
!>
!> @author Adam Nelson and Brendan Kochunas
!>   @date 02/14/2012
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE MatrixTypes

  USE IntrType
  USE ExceptionHandler
  USE Allocs
  USE BLAS2,           ONLY: BLAS2_matvec => BLAS_matvec
  USE BLAS3,           ONLY: BLAS3_matmult => BLAS_matmat
  USE VectorTypes
  USE ParameterLists
  IMPLICIT NONE

#ifdef HAVE_PETSC
#include <finclude/petsc.h>
#define IS IS !petscisdef.h defines the keyword IS, and it needs to be reset
#endif

  PRIVATE
!
! List of public members
  PUBLIC :: eMatrixType
  PUBLIC :: MatrixType
  PUBLIC :: SquareMatrixType
  PUBLIC :: RectMatrixType
  PUBLIC :: DenseSquareMatrixType
  PUBLIC :: DenseRectMatrixType
  PUBLIC :: TriDiagMatrixType
  PUBLIC :: SparseMatrixType
  PUBLIC :: PETScMatrixType
  PUBLIC :: BLAS_matvec
  PUBLIC :: BLAS_matmult
  
  !> @brief the base matrix type
  TYPE,ABSTRACT :: MatrixType
    !> Initialization status 
    LOGICAL(SBK) :: isInit=.FALSE.
    !> Number of rows in the matrix
    INTEGER(SIK) :: n=0
!
!List of Type Bound Procedures
    CONTAINS
      !> Deferred routine for clearing the matrix
      PROCEDURE(int_matrix_sub),DEFERRED,PASS :: clear
      !> Deferred routine for initializing the matrix
      PROCEDURE(int_matrix_init_param_sub),DEFERRED,PASS :: init
      !> Deferred routine for setting matrix values
      PROCEDURE(int_matrix_set_sub),DEFERRED,PASS :: set
  ENDTYPE MatrixType    
!
!List of Abstract Interfaces
  !> Explicitly defines the interface for the clear routine of all matrix types
  ABSTRACT INTERFACE
    SUBROUTINE int_matrix_sub(matrix)
      IMPORT :: MatrixType
      CLASS(MatrixType),INTENT(INOUT) :: matrix
    ENDSUBROUTINE int_matrix_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the init routine of all matrix types
  !> with parameter list
  ABSTRACT INTERFACE
    SUBROUTINE int_matrix_init_param_sub(matrix,pList)
      IMPORT :: MatrixType,ParamType
      CLASS(MatrixType),INTENT(INOUT) :: matrix
      CLASS(ParamType),INTENT(IN) :: pList
    ENDSUBROUTINE int_matrix_init_param_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the set routine of all matrix types
  ABSTRACT INTERFACE
    SUBROUTINE int_matrix_set_sub(matrix,i,j,setval)
      IMPORT :: SIK,SRK,MatrixType
      CLASS(MatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(IN) :: setval
    ENDSUBROUTINE int_matrix_set_sub
  ENDINTERFACE
    
  !> @brief The extended type of matrices for square matrices
  TYPE,ABSTRACT,EXTENDS(MatrixType) :: SquareMatrixType
    !> Indicates whether or not the matrix is symmetric
    LOGICAL(SBK) :: isSymmetric=.FALSE.
  ENDTYPE SquareMatrixType
  
  !> @brief The extended type for rectangular matrices
  TYPE,ABSTRACT,EXTENDS(MatrixType) :: RectMatrixType
    !> The number of columns
    INTEGER(SIK) :: m=0
  ENDTYPE RectMatrixType
  
  !> @brief The extended type for PETSc matrices
  TYPE,EXTENDS(SquareMatrixType) :: PETScMatrixType
#ifdef HAVE_PETSC
    Mat :: A
#endif
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
      !> @copybrief MatrixTypes::get_PETScMatrixType
      !> @copydetails MatrixTypes::get_PETScMatrixType
      PROCEDURE,PASS :: get => get_PETScMatrixType
  ENDTYPE PETScMatrixType
  
  !> @brief The extended type for dense square matrices
  !> 
  !> This does not add a significant functionality over the intrinsic
  !> allocatable arrays that are part of the Fortran language. It
  !> is provided so as to be able to use the BLAS interfaces adapted
  !> for the matrix type and it also gains some value by having the
  !> isSymmetric and isInit attributes.
  TYPE,EXTENDS(SquareMatrixType) :: DenseSquareMatrixType
    !> The values of the matrix
    REAL(SRK),ALLOCATABLE :: a(:,:)
!
!List of Type Bound Procedures
    CONTAINS
      !> @copybrief MatrixTypes::clear_DenseSquareMatrixType
      !> @copydetails MatrixTypes::clear_DenseSquareMatrixType
      PROCEDURE,PASS :: clear => clear_DenseSquareMatrixType
      !> @copybrief MatrixTypes::clear_DenseSquareMatrixType
      !> @copydetails MatrixTypes::clear_DenseSquareMatrixType
      PROCEDURE,PASS :: init => init_DenseSquareMatrixParam
      !> @copybrief MatrixTypes::set_DenseSquareMatrixType
      !> @copydetails MatrixTypes::set_DenseSquareMatrixType
      PROCEDURE,PASS :: set => set_DenseSquareMatrixType
  ENDTYPE DenseSquareMatrixType
  
  !> @brief The extended type for dense rectangular matrices
  TYPE,EXTENDS(RectMatrixType) :: DenseRectMatrixType
    !> The values of the matrix
    REAL(SRK),ALLOCATABLE :: a(:,:)
!
!List of Type Bound Procedures
    CONTAINS 
      !> @copybrief MatrixTypes::clear_DenseRectMatrixType
      !> @copydetails MatrixTypes::clear_DenseRectMatrixType
      PROCEDURE,PASS :: clear => clear_DenseRectMatrixType
      !> @copybrief MatrixTypes::init_DenseRectMatrixType
      !> @copydetails MatrixTypes::init_DenseRectMatrixType
      PROCEDURE,PASS :: init => init_DenseRectMatrixParam
      !> @copybrief MatrixTypes::set_DenseRectMatrixType
      !> @copydetails MatrixTypes::set_DenseRectMatrixType
      PROCEDURE,PASS :: set => set_DenseRectMatrixType
  ENDTYPE DenseRectMatrixType
  
  !I think this may need to be revisited
  !> @brief The extended type for tri-diagonal square matrices
  TYPE,EXTENDS(SquareMatrixType) :: TriDiagMatrixType
    !> The values of the matrix
    REAL(SRK),ALLOCATABLE :: a(:,:) 
!
!List of Type Bound Procedures
    CONTAINS
      !> @copybrief MatrixTypes::clear_TriDiagMatrixType
      !> @copydetails MatrixTypes::clear_TriDiagMatrixType
      PROCEDURE,PASS :: clear => clear_TriDiagMatrixType
      !> @copybrief MatrixTypes::init_TriDiagMatrixType
      !> @copydetails MatrixTypes::init_TriDiagMatrixType
      PROCEDURE,PASS :: init => init_TriDiagMatrixParam
      !> @copybrief MatrixTypes::set_TriDiagMatrixType
      !> @copydetails MatrixTypes::set_TriDiagMatrixType
      PROCEDURE,PASS :: set => set_TriDiagMatrixType
  ENDTYPE TriDiagMatrixType
  
  !> @brief The basic sparse matrix type
  !> 
  !> Matrix uses compressed sparse row storage format, 
  !> as defined by Intel's MKL
  TYPE,EXTENDS(MatrixType) :: SparseMatrixType
    !> Number of non-zero entries in the matrix
    INTEGER(SIK) :: nnz=0
    !> The number of elements in each row
    INTEGER(SIK),ALLOCATABLE :: ia(:)
    !> The column indeces for each element
    INTEGER(SIK),ALLOCATABLE :: ja(:) !columns
    !> The values of the matrix
    REAL(SRK),ALLOCATABLE :: a(:) !values
    !> A counter for the current location in a(:) and ja(:)
    INTEGER(SIK) :: jCount=0 
    !> A variable to store the previous row entered in set_shape
    INTEGER(SIK) :: iPrev=0
    !> A variable to store the previous column entered in set_shape
    INTEGER(SIK) :: jPrev=0
!
!List of Type Bound Procedures
    CONTAINS 
      !> @copybrief MatrixTypes::clear_SparseMatrixType
      !> @copydetails MatrixTypes::clear_SparseMatrixType
      PROCEDURE,PASS :: clear => clear_SparseMatrixType
      !> @copybrief MatrixTypes::init_SparseMatrixType
      !> @copydetails MatrixTypes::init_SparseMatrixType
      PROCEDURE,PASS :: init => init_SparseMatrixParam
      !> @copybrief MatrixTypes::set_SparseMatrixType
      !> @copydetails MatrixTypes::set_SparseMatrixType
      PROCEDURE,PASS :: set => set_SparseMatrixType
      !> @copybrief MatrixTypes::set_shape_SparseMatrixType
      !> @copydetails MatrixTypes::set_shape_SparseMatrixType
      PROCEDURE,PASS :: setShape => set_shape_SparseMatrixType
      !> @copybrief MatrixTypes::get_SparseMatrixType
      !> @copydetails MatrixTypes::get_SparseMatrixType
      PROCEDURE,PASS :: get => get_SparseMatrixType
  ENDTYPE SparseMatrixType
  
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
  
  !> Exception Handler for use in MatrixTypes
  TYPE(ExceptionHandlerType),POINTER,SAVE :: eMatrixType => NULL()
  
  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='MATRIXTYPES'
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes Sparse Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param pList the parameter list
!>
    SUBROUTINE init_SparseMatrixParam(matrix,pList)
      CHARACTER(LEN=*),PARAMETER :: myName='init_SparseMatrixParam'
      CLASS(SparseMatrixType),INTENT(INOUT) :: matrix
      CLASS(ParamType),INTENT(IN) :: pList
      INTEGER(SIK) :: n, m, mattype
      LOGICAL(SBK) :: localalloc
      
      ! Pull Data From Parameter List
      CALL pList%get('n',n)
      CALL pList%get('m',m)
      mattype=0
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eMatrixType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eMatrixType)
      ENDIF

      IF(.NOT. matrix%isInit) THEN
        IF((n < 1).OR.(m < 1))  THEN
          CALL eMatrixType%raiseError('Incorrect input to '// &
          modName//'::'//myName//' - Input parameters must be '// &
            'greater than 1!')
        ELSE
          matrix%isInit=.TRUE.
          matrix%n=n
          matrix%nnz=m
          matrix%jCount=0
          matrix%iPrev=0
          matrix%jPrev=0
          !regardless of sparsity, SIZE(ia)=n+1
          CALL dmallocA(matrix%ia,n+1)
          CALL dmallocA(matrix%a,m)
          CALL dmallocA(matrix%ja,m)
          !last entry of ia is known in advanced
          !this is per the intel MKL format
          matrix%ia(n+1)=m+1
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - MatrixType already initialized')
      ENDIF

      IF(localalloc) DEALLOCATE(eMatrixType)
      
    ENDSUBROUTINE init_SparseMatrixParam
!
!-------------------------------------------------------------------------------
!> @brief Initializes Tridiagonal Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param pList the parameter list
!>
    SUBROUTINE init_TriDiagMatrixParam(matrix,pList)
      CHARACTER(LEN=*),PARAMETER :: myName='init_TriDiagMatrixParam'
      CLASS(TriDiagMatrixType),INTENT(INOUT) :: matrix
      CLASS(ParamType),INTENT(IN) :: pList
      INTEGER(SIK) :: n, m, mattype
      LOGICAL(SBK) :: localalloc
      
      ! Pull Data From Parameter List
      CALL pList%get('n',n)
      CALL pList%get('m',m)
      mattype=0
      
     !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eMatrixType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eMatrixType)
      ENDIF

      IF(.NOT. matrix%isInit) THEN
        IF(n < 1) THEN
          CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of rows (n) must be '// &
              'greater than 1!')
        ELSE
          matrix%isInit=.TRUE.
          matrix%n=n
          IF(m == 0) THEN
            matrix%isSymmetric=.FALSE.
          ELSE
            matrix%isSymmetric=.TRUE.
          ENDIF
          CALL dmallocA(matrix%a,3,n)
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - MatrixType already initialized')
      ENDIF
        
      IF(localalloc) DEALLOCATE(eMatrixType)
      
    ENDSUBROUTINE init_TriDiagMatrixParam
!
!-------------------------------------------------------------------------------
!> @brief Initializes Dense Rectangular Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param pList the parameter list
!>
    SUBROUTINE init_DenseRectMatrixParam(matrix,pList)
      CHARACTER(LEN=*),PARAMETER :: myName='init_DenseRectMatrixParam'
      CLASS(DenseRectMatrixType),INTENT(INOUT) :: matrix
      CLASS(ParamType),INTENT(IN) :: pList
      INTEGER(SIK) :: n, m, mattype
      LOGICAL(SBK) :: localalloc
      
      ! Pull Data From Parameter List
      CALL pList%get('n',n)
      CALL pList%get('m',m)
      mattype=0
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eMatrixType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eMatrixType)
      ENDIF
      
      IF(.NOT. matrix%isInit) THEN
        IF(n < 1) THEN
          CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of rows (n) must'// &
              ' be greater than 1!')
        ELSEIF(m < 1) THEN
          CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of columns (m) must'// &
              ' be greater than 1!')
        ELSE
          matrix%isInit=.TRUE.
          matrix%n=n
          matrix%m=m
          CALL dmallocA(matrix%a,n,m)
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - MatrixType already initialized')
      ENDIF

      IF(localalloc) DEALLOCATE(eMatrixType)
      
    ENDSUBROUTINE init_DenseRectMatrixParam
!
!-------------------------------------------------------------------------------
!> @brief Initializes Dense Square Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param pList the parameter list
!>
    SUBROUTINE init_DenseSquareMatrixParam(matrix,pList)
      CHARACTER(LEN=*),PARAMETER :: myName='init_DenseSquareMatrixParam'
      CLASS(DenseSquareMatrixType),INTENT(INOUT) :: matrix
      CLASS(ParamType),INTENT(IN) :: pList
      INTEGER(SIK) :: n, m, mattype
      LOGICAL(SBK) :: localalloc
      
      ! Pull Data From Parameter List
      CALL pList%get('n',n)
      CALL pList%get('m',m)
      mattype=0
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eMatrixType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eMatrixType)
      ENDIF
      
      IF(.NOT. matrix%isInit) THEN
        IF(n < 1) THEN
          CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of rows (n) must be '// &
              'greater than 1!')
        ELSE
          matrix%isInit=.TRUE.
          matrix%n=n
          IF(m == 0) THEN
            matrix%isSymmetric=.FALSE.
          ELSE
            matrix%isSymmetric=.TRUE.
          ENDIF
          CALL dmallocA(matrix%a,n,n)
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - MatrixType already initialized')
      ENDIF

      IF(localalloc) DEALLOCATE(eMatrixType)
      
    ENDSUBROUTINE init_DenseSquareMatrixParam
!
!-------------------------------------------------------------------------------
!> @brief Initializes PETSc Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param pList the parameter list
!>
    SUBROUTINE init_PETScMatrixParam(matrix,pList)
      CHARACTER(LEN=*),PARAMETER :: myName='init_PETScMatrixParam'
      CLASS(PETScMatrixType),INTENT(INOUT) :: matrix
      CLASS(ParamType),INTENT(IN) :: pList
      INTEGER(SIK) :: n, m, mattype
      LOGICAL(SBK) :: localalloc
      
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      
      ! Pull Data From Parameter List
      CALL pList%get('n',n)
      CALL pList%get('m',m)
      SELECTTYPE(matrix); TYPE IS(PETScMatrixType)
        CALL pList%get('mattype',mattype)
      ENDSELECT
      mattype=0
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eMatrixType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eMatrixType)
      ENDIF

      IF(.NOT. matrix%isInit) THEN
        IF(n < 1) THEN
          CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of rows (n) must be '// &
              'greater than 0!')
        ELSE
          matrix%isInit=.TRUE.
          matrix%n=n
          IF(m == 0) THEN
            matrix%isSymmetric=.FALSE.
          ELSE
            matrix%isSymmetric=.TRUE.
          ENDIF
          CALL MatCreate(MPI_COMM_WORLD,matrix%a,ierr)
          CALL MatSetSizes(matrix%a,PETSC_DECIDE,PETSC_DECIDE,matrix%n,matrix%n,ierr)
          IF (PRESENT(mattype)) THEN
            IF (mattype == 0) THEN     ! sparse matrix
              CALL MatSetType(matrix%a,MATMPIAIJ,ierr)
            ELSEIF (mattype == 1) THEN ! dense matrix
              CALL MatSetType(matrix%a,MATMPIDENSE,ierr)
            ENDIF
          ELSE
            CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Matrix type (mattype) must '// &
            'be provided!')
          ENDIF
          CALL MatSetUp(matrix%a,ierr)
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - MatrixType already initialized')
      ENDIF

      IF(localalloc) DEALLOCATE(eMatrixType)
#endif
      
    ENDSUBROUTINE init_PETScMatrixParam

!
!-------------------------------------------------------------------------------
!> @brief Initializes Sparse Matrix Type
!> @param matrix the matrix type to act on
!> @param n the number of rows
!> @param m the number of columns
!> @param mattype the matrix type (only used for PETSc matrices)
!> @param num_entries (optional) the number of non-zero elements in the matrix
!>
!> This routine initializes the data spaces for the sparse matrices. 
!> Knowing the number of values a priori significantly reduces CSR init
!> overhead because it removes unnecessary allocs, copies, and deallocations.
!> This parameter is listed as optional only for compliance with the interface
!> provided by MatrixTypes.  It will not run sucessfully without this argument.
!>
    SUBROUTINE init_SparseMatrixType(matrix,n,m,mattype)
      CHARACTER(LEN=*),PARAMETER :: myName='init_SparseMatrixType'
      CLASS(SparseMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: m
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: mattype
      LOGICAL(SBK) :: localalloc
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eMatrixType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eMatrixType)
      ENDIF
      IF(PRESENT(m)) THEN
        IF(.NOT. matrix%isInit) THEN
          IF((n < 1).OR.(m < 1))  THEN
            CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Input parameters must be '// &
              'greater than 1!')
          ELSE
            matrix%isInit=.TRUE.
            matrix%n=n
            matrix%nnz=m
            matrix%jCount=0
            matrix%iPrev=0
            matrix%jPrev=0
            !regardless of sparsity, SIZE(ia)=n+1
            CALL dmallocA(matrix%ia,n+1)
            CALL dmallocA(matrix%a,m)
            CALL dmallocA(matrix%ja,m)
            !last entry of ia is known in advanced
            !this is per the intel MKL format
            matrix%ia(n+1)=m+1
          ENDIF
        ELSE
          CALL eMatrixType%raiseError('Incorrect call to '// &
            modName//'::'//myName//' - MatrixType already initialized')
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (m) must be'// &
              ' provided!')
      ENDIF
      IF(localalloc) DEALLOCATE(eMatrixType)
    ENDSUBROUTINE init_SparseMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Initializes the dense square matrix
!> @param matrix the matrix type to act on
!> @param n the number of rows
!> @param m integer-based logical defining whether or not the matrix is symmetric
!> @param mattype the matrix type (only used for PETSc matrices)
!>
    SUBROUTINE init_DenseSquareMatrixType(matrix,n,m,mattype)
      CHARACTER(LEN=*),PARAMETER :: myName='init_DenseSquareMatrixType'
      CLASS(DenseSquareMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: m
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: mattype
      LOGICAL(SBK) :: localalloc
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eMatrixType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eMatrixType)
      ENDIF
      IF(PRESENT(m)) THEN
        IF(.NOT. matrix%isInit) THEN
          IF(n < 1) THEN
            CALL eMatrixType%raiseError('Incorrect input to '// &
              modName//'::'//myName//' - Number of rows (n) must be '// &
                'greater than 1!')
          ELSE
            matrix%isInit=.TRUE.
            matrix%n=n
            IF(m == 0) THEN
              matrix%isSymmetric=.FALSE.
            ELSE
              matrix%isSymmetric=.TRUE.
            ENDIF
            CALL dmallocA(matrix%a,n,n)
          ENDIF
        ELSE
          CALL eMatrixType%raiseError('Incorrect call to '// &
            modName//'::'//myName//' - MatrixType already initialized')
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - isSymmetric must be provided!')
      ENDIF
      IF(localalloc) DEALLOCATE(eMatrixType)
    ENDSUBROUTINE init_DenseSquareMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Initializes the tridiagonal square matrix
!> @param matrix the matrix type to act on
!> @param n the number of rows
!> @param m integer-based logical defining whether or not the matrix is symmetric
!> @param mattype the matrix type (only used for PETSc matrices)
!>
    SUBROUTINE init_TriDiagMatrixType(matrix,n,m,mattype)
      CHARACTER(LEN=*),PARAMETER :: myName='init_TriDiagMatrixType'
      CLASS(TriDiagMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: m
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: mattype
      LOGICAL(SBK) :: localalloc
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eMatrixType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eMatrixType)
      ENDIF
      IF(PRESENT(m)) THEN
        IF(.NOT. matrix%isInit) THEN
          IF(n < 1) THEN
            CALL eMatrixType%raiseError('Incorrect input to '// &
              modName//'::'//myName//' - Number of rows (n) must be '// &
                'greater than 1!')
          ELSE
            matrix%isInit=.TRUE.
            matrix%n=n
            IF(m == 0) THEN
              matrix%isSymmetric=.FALSE.
            ELSE
              matrix%isSymmetric=.TRUE.
            ENDIF
            CALL dmallocA(matrix%a,3,n)
          ENDIF
        ELSE
          CALL eMatrixType%raiseError('Incorrect call to '// &
            modName//'::'//myName//' - MatrixType already initialized')
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - isSymmetric must be provided!')
      ENDIF
      IF(localalloc) DEALLOCATE(eMatrixType)
    ENDSUBROUTINE init_TriDiagMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Initializes the dense rectangular matrix
!> @param matrix the matrix type to act on
!> @param n the number of rows
!> @param m the number of columns. Listed as optional, but is not
!> @param mattype the matrix type (only used for PETSc matrices)
!>
    SUBROUTINE init_DenseRectMatrixType(matrix,n,m,mattype)
      CHARACTER(LEN=*),PARAMETER :: myName='init_DenseRectMatrixType'
      CLASS(DenseRectMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: m
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: mattype
      LOGICAL(SBK) :: localalloc
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eMatrixType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eMatrixType)
      ENDIF
      IF(PRESENT(m)) THEN
        IF(.NOT. matrix%isInit) THEN
          IF(n < 1) THEN
            CALL eMatrixType%raiseError('Incorrect input to '// &
              modName//'::'//myName//' - Number of rows (n) must'// &
                ' be greater than 1!')
          ELSEIF(m < 1) THEN
            CALL eMatrixType%raiseError('Incorrect input to '// &
              modName//'::'//myName//' - Number of columns (m) must'// &
                ' be greater than 1!')
          ELSE
            matrix%isInit=.TRUE.
            matrix%n=n
            matrix%m=m
            CALL dmallocA(matrix%a,n,m)
          ENDIF
        ELSE
          CALL eMatrixType%raiseError('Incorrect call to '// &
            modName//'::'//myName//' - MatrixType already initialized')
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of columns (m) must be '// &
              'provided!')
      ENDIF
      IF(localalloc) DEALLOCATE(eMatrixType)
    ENDSUBROUTINE init_DenseRectMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Initializes the PETSc matrix
!> @param declares the matrix type to act on
!> @param n the number of rows
!> @param m integer-based logical defining whether or not the matrix is symmetric
!> @param mattype the matrix type (only used for PETSc matrices)
!>
    SUBROUTINE init_PETScMatrixType(matrix,n,m,mattype)
      CHARACTER(LEN=*),PARAMETER :: myName='init_PETScSparseMatrixType'
      CLASS(PETScMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: m
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: mattype
      LOGICAL(SBK) :: localalloc
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eMatrixType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eMatrixType)
      ENDIF
      IF(PRESENT(m)) THEN
        IF(.NOT. matrix%isInit) THEN
          IF(n < 1) THEN
            CALL eMatrixType%raiseError('Incorrect input to '// &
              modName//'::'//myName//' - Number of rows (n) must be '// &
                'greater than 0!')
          ELSE
            matrix%isInit=.TRUE.
            matrix%n=n
            IF(m == 0) THEN
              matrix%isSymmetric=.FALSE.
            ELSE
              matrix%isSymmetric=.TRUE.
            ENDIF
            CALL MatCreate(MPI_COMM_WORLD,matrix%a,ierr)
            CALL MatSetSizes(matrix%a,PETSC_DECIDE,PETSC_DECIDE,matrix%n,matrix%n,ierr)
            IF (PRESENT(mattype)) THEN
              IF (mattype == 0) THEN     ! sparse matrix
                CALL MatSetType(matrix%a,MATMPIAIJ,ierr)
              ELSEIF (mattype == 1) THEN ! dense matrix
                CALL MatSetType(matrix%a,MATMPIDENSE,ierr)
              ENDIF
            ELSE
              CALL eMatrixType%raiseError('Incorrect input to '// &
              modName//'::'//myName//' - Matrix type (mattype) must '// &
              'be provided!')
            ENDIF
            CALL MatSetUp(matrix%a,ierr)
          ENDIF
        ELSE
          CALL eMatrixType%raiseError('Incorrect call to '// &
            modName//'::'//myName//' - MatrixType already initialized')
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of columns (m) must be '// &
              'provided!')
      ENDIF
      IF(localalloc) DEALLOCATE(eMatrixType)
#endif
    ENDSUBROUTINE init_PETScMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Clears the sparse matrix
!> @param matrix the matrix type to act on
!>
    SUBROUTINE clear_SparseMatrixType(matrix)
      CLASS(SparseMatrixType),INTENT(INOUT) :: matrix
      matrix%isInit=.FALSE.
      matrix%n=0
      matrix%jCount=0
      matrix%iPrev=0
      matrix%jPrev=0
      matrix%nnz=0
      IF(ALLOCATED(matrix%ia)) CALL demallocA(matrix%ia)
      IF(ALLOCATED(matrix%ja)) CALL demallocA(matrix%ja)
      IF(ALLOCATED(matrix%a)) CALL demallocA(matrix%a)
    ENDSUBROUTINE clear_SparseMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Clears the dense square matrix
!> @param matrix the matrix type to act on
!>
    SUBROUTINE clear_DenseSquareMatrixType(matrix)
      CLASS(DenseSquareMatrixType),INTENT(INOUT) :: matrix
      matrix%isInit=.FALSE.
      matrix%n=0
      matrix%isSymmetric=.FALSE.
      IF(ALLOCATED(matrix%a)) CALL demallocA(matrix%a)
    ENDSUBROUTINE clear_DenseSquareMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Clears the tri-diagonal matrix
!> @param matrix the matrix type to act on
!>
    SUBROUTINE clear_TriDiagMatrixType(matrix)
      CLASS(TriDiagMatrixType),INTENT(INOUT) :: matrix
      matrix%isInit=.FALSE.
      matrix%n=0
      matrix%isSymmetric=.FALSE.
      IF(ALLOCATED(matrix%a)) CALL demallocA(matrix%a)
     ENDSUBROUTINE clear_TriDiagMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Clears the dense rectangular matrix
!> @param matrix the matrix type to act on
!>
    SUBROUTINE clear_DenseRectMatrixType(matrix)
      CLASS(DenseRectMatrixType),INTENT(INOUT) :: matrix
      matrix%isInit=.FALSE.
      matrix%n=0
      matrix%m=0
      IF(ALLOCATED(matrix%a)) CALL demallocA(matrix%a)
    ENDSUBROUTINE clear_DenseRectMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Clears the PETSc sparse matrix
!> @param matrix the matrix type to act on
!>
    SUBROUTINE clear_PETScMatrixType(matrix)
      CLASS(PETScMatrixType),INTENT(INOUT) :: matrix
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      matrix%isInit=.FALSE.
      matrix%n=0
      matrix%isSymmetric=.FALSE.
      CALL MatDestroy(matrix%a,ierr)
#endif
    ENDSUBROUTINE clear_PETScMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the sparse matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
!> This routine sets the values of the sparse matrix.  It can only be used
!> If setShape has previously been applied to the same sparse matrix.
!>
    SUBROUTINE set_SparseMatrixType(matrix,i,j,setval)
      CLASS(SparseMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK) :: ja_index
      LOGICAL(SBK) :: found_ja
      IF(matrix%isInit) THEN
        IF(((matrix%jCount > 0).AND.(i <= matrix%n)) &
            .AND. ((j > 0) .AND. (i > 0))) THEN
          !currently written assuming no all-zero rows.
          !pretty safe assumption.
          found_ja=.FALSE.
          DO ja_index=matrix%ia(i),matrix%ia(i+1)-1
            IF(matrix%ja(ja_index) == j) THEN
              found_ja=.TRUE.
              EXIT          
            ENDIF
          ENDDO
          IF(found_ja) matrix%a(ja_index)=setval
        ENDIF
      ENDIF
    ENDSUBROUTINE set_SparseMatrixtype
!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the dense square matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
    SUBROUTINE set_DenseSquareMatrixType(matrix,i,j,setval)
      CLASS(DenseSquareMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(IN) :: setval
      IF(matrix%isInit) THEN
        IF(((j <= matrix%n) .AND. (i <= matrix%n)) & 
          .AND. ((j > 0) .AND. (i > 0))) THEN
          matrix%a(i,j)=setval
          IF(matrix%isSymmetric) matrix%a(j,i)=setval
        ENDIF
      ENDIF
    ENDSUBROUTINE set_DenseSquareMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the tridiagonal matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
    SUBROUTINE set_TriDiagMatrixType(matrix,i,j,setval)
      CLASS(TriDiagMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(IN) :: setval
      IF(matrix%isInit) THEN
        IF(((j <= matrix%n) .AND. (i <= matrix%n)) &
            .AND. (i>=1) .AND. (j >= 1)) THEN
          !based on i,j, put in correct location
          IF((j == (i-1)).AND. (i > 1)) THEN !sub-diag
            matrix%a(1,i)=setval
            IF(matrix%isSymmetric) matrix%a(3,j)=setval
          ELSEIF((j == (i+1)) .AND. (i < matrix%n)) THEN !super-diag
            matrix%a(3,i)=setval
            IF(matrix%isSymmetric) matrix%a(1,j)=setval
          ELSEIF(i == j) THEN
            matrix%a(2,i)=setval
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE set_TriDiagMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the dense rectangular matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
    SUBROUTINE set_DenseRectMatrixType(matrix,i,j,setval)
      CLASS(DenseRectMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(IN) :: setval
      IF(matrix%isInit) THEN
        IF(((j <= matrix%m) .AND. (i <= matrix%n)) &
          .AND. ((j > 0) .AND. (i > 0))) matrix%a(i,j)=setval
      ENDIF
    ENDSUBROUTINE set_DenseRectMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values and the shape in the sparse matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
!> This is to be used the first time the sparse matrix values are being set.
!> This routine learns the shape of the CSR format matrix.
!> The matrix must be supplied in row-major order, any entries not in this form
!> will be ignored.
!> 
    SUBROUTINE set_shape_SparseMatrixType(matrix,i,j,setval)
      CLASS(SparseMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),OPTIONAL,INTENT(IN) :: setval
      LOGICAL(SBK) :: ijOK
      IF(matrix%isInit) THEN
        IF((i <= matrix%n) .AND. ((j > 0) .AND. (i > 0))) THEN
          !enforce entering values in row-major order
          !first check to see if this is a new row or not (ia(i)>0)
          !If it is, then we have to comprae new j with previous j
          ijOK=.FALSE.
          IF((matrix%jCount < matrix%nnz) & 
            .AND.((matrix%iPrev == i).AND.(matrix%jPrev < j))) THEN
            ijOK=.TRUE.
          ELSEIF(matrix%iPrev+1 == i) THEN
            ijOK=.TRUE.
          ENDIF
          IF(matrix%ia(i) == 0) matrix%ia(i)=matrix%jCount+1
          IF(ijOK) THEN
            matrix%iPrev=i
            matrix%jPrev=j
            matrix%jCount=matrix%jCount+1
            IF(PRESENT(setval)) matrix%a(matrix%jCount)=setval
            matrix%ja(matrix%jCount)=j
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE set_shape_SparseMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the PETSc matrix
!> @param declares the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
    SUBROUTINE set_PETScMatrixType(matrix,i,j,setval)
      CLASS(PETScMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(IN) :: setval
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      IF(matrix%isInit) THEN
        IF(((j <= matrix%n) .AND. (i <= matrix%n)) & 
          .AND. ((j > 0) .AND. (i > 0))) THEN
          CALL MatSetValues(matrix%a,1,i-1,1,j-1,setval,INSERT_VALUES,ierr)
          IF(matrix%isSymmetric) THEN
            CALL MatSetValues(matrix%a,1,j-1,1,i-1,setval,INSERT_VALUES,ierr)
          ENDIF
          CALL MatAssemblyBegin(matrix%a,ierr)
          CALL MatAssemblyEnd(matrix%a,ierr)
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE set_PETScMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Gets the values in the sparse matrix - presently untested
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!>
!> This routine gets the values of the sparse matrix.  If an (i,j) value
!> is not present, then 0.0 is returned.  If the (i,j) location is out of
!> bounds, then -1051.0 is returned (-1051.0 is an arbitrarily chosen key).
!>
    FUNCTION get_SparseMatrixType(matrix,i,j) RESULT(aij)
      CLASS(SparseMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      INTEGER(SIK) :: ja_index
      LOGICAL(SBK) :: found_ja
      REAL(SRK) :: aij
      
      aij=0.0_SRK
      IF(matrix%isInit) THEN
        IF(((matrix%jCount > 0).AND.(i <= matrix%n)) &
            .AND. ((j > 0) .AND. (i > 0))) THEN
          found_ja=.FALSE.
          DO ja_index=matrix%ia(i),matrix%ia(i+1)-1
            IF(matrix%ja(ja_index) == j) THEN
              found_ja=.TRUE.
              EXIT          
            ENDIF
          ENDDO
          IF(found_ja) aij=matrix%a(ja_index)
        ELSE
          aij=-1051._SRK
        ENDIF
      ENDIF
    ENDFUNCTION get_SparseMatrixtype
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
    FUNCTION get_PETScMatrixType(matrix,i,j) RESULT(aij)
      CLASS(PETScMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK) :: aij
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
#endif
  
      aij=0.0_SRK
      
#ifdef HAVE_PETSC
      IF(matrix%isInit) THEN
        IF((i <= matrix%n) .AND. ((j > 0) .AND. (i > 0))) THEN
          CALL MatGetValues(matrix%a,1,i-1,1,j-1,aij,ierr)
        ELSE
          aij=-1051._SRK
        ENDIF
      ENDIF
#endif
    ENDFUNCTION get_PETScMatrixtype
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
!>
    SUBROUTINE matvec_MatrixType(thisMatrix,trans,alpha,x,beta,y)
      CLASS(MatrixType),INTENT(IN) :: thisMatrix
      CHARACTER(LEN=1),OPTIONAL,INTENT(IN) :: trans
      REAL(SRK),INTENT(IN),OPTIONAL :: alpha
      REAL(SRK),INTENT(IN) :: x(:)
      REAL(SRK),INTENT(IN),OPTIONAL :: beta
      REAL(SRK),INTENT(INOUT) :: y(:)
      
      CHARACTER(LEN=1) :: t
      
      IF(thisMatrix%isInit) THEN
        t='n'
        IF(PRESENT(trans)) t=trans
        
        SELECTTYPE(thisMatrix)
          TYPE IS(DenseSquareMatrixType)
            IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
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
            IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
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
!>
    SUBROUTINE matvec_MatrixTypeVectorType(thisMatrix,trans,alpha,x,beta,y)
      CLASS(MatrixType),INTENT(IN) :: thisMatrix
      CLASS(VectorType),INTENT(IN) :: x
      CHARACTER(LEN=1),OPTIONAL,INTENT(IN) :: trans
      REAL(SRK),INTENT(IN),OPTIONAL :: alpha
      REAL(SRK),INTENT(IN),OPTIONAL :: beta
      CLASS(VectorType),INTENT(INOUT) :: y
      
      CHARACTER(LEN=1) :: t
      
      IF(thisMatrix%isInit) THEN
        t='n'
        IF(PRESENT(trans)) t=trans
        
        SELECTTYPE(x); TYPE IS(RealVectorType)
          SELECTTYPE(y); TYPE IS(RealVectorType)
            SELECTTYPE(thisMatrix)
              TYPE IS(DenseSquareMatrixType)
                IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
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
                IF(PRESENT(alpha) .AND. PRESENT(beta)) THEN
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
            ENDSELECT
          ENDSELECT
        ENDSELECT
      ENDIF

    ENDSUBROUTINE matvec_MatrixTypeVectorType
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
      CLASS(MatrixType),INTENT(IN) :: A
      CLASS(MatrixType),INTENT(IN) :: B
      CLASS(MatrixType),INTENT(INOUT) :: C
      REAL(SRK),INTENT(IN),OPTIONAL :: alpha
      REAL(SRK),INTENT(IN),OPTIONAL :: beta
      CHARACTER(LEN=1),OPTIONAL,INTENT(IN) :: transA
      CHARACTER(LEN=1),OPTIONAL,INTENT(IN) :: transB
      
      CHARACTER(LEN=1) :: tA
      CHARACTER(LEN=1) :: tB

      IF(A%isInit) THEN
        tA='n'
        IF(PRESENT(transA)) tA=transA
      ENDIF
      IF(B%isInit) THEN
        tB='n'
        IF(PRESENT(transB)) tB=transB
      ENDIF
!      IF((.NOT. C%isInit) .AND. A%isInit .AND. B%isInit) THEN
!        SELECTTYPE(C)
!          TYPE IS(DenseSquareMatrixType)
!            CALL C%init(A%n)
!            C%a=0.0_SRK
!          TYPE IS(DenseRectMatrixType)
!            SELECTTYPE(B)
!              TYPE IS(DenseSquareMatrixType)
!                CALL C%init(A%n,B%n)
!              TYPE IS(DenseRectMatrixType)
!                CALL C%init(A%n,B%m)
!            ENDSELECT
!            C%a=0.0_SRK
!        ENDSELECT
!      ENDIF
        
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
