!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Futility-native implementations of MatrixTypes
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref Allocs "Allocs": @copybrief Allocs
!>  - @ref MatrixTypes_Base "MatrixTypes_Base": @copybrief MatrixTypes_Base
!>
!> @author Adam Nelson and Brendan Kochunas
!>   @date 02/14/2012
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE MatrixTypes_Native
#include "Futility_DBC.h"
  USE Futility_DBC
  USE IntrType
  USE ExceptionHandler
  USE ParameterLists
  USE Allocs
  USE MatrixTypes_Base

  IMPLICIT NONE

  PRIVATE
!
! List of public members
  PUBLIC :: DenseSquareMatrixType
  PUBLIC :: DenseRectMatrixType
  PUBLIC :: TriDiagMatrixType
  PUBLIC :: BandedMatrixType
  PUBLIC :: SparseMatrixType

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
      !> @copybrief MatrixTypes::get_DenseSquareMatrixType
      !> @copydetails MatrixTypes::get_DenseSquareMatrixType
      PROCEDURE,PASS :: get => get_DenseSquareMatrixType
      !> @copybrief MatrixTypes::transpose_DenseSquareMatrixType
      !> @copydetails MatrixTypes::transpose_DenseSquareMatrixType
      PROCEDURE,PASS :: transpose => transpose_DenseSquareMatrixType
      !> @copybrief MatrixTypes::zeroentries_DenseSquareMatrixType
      !> @copydetails MatrixTypes::zeroentries_DenseSquareMatrixType
      PROCEDURE,PASS :: zeroentries => zeroentries_DenseSquareMatrixType
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
      !> @copybrief MatrixTypes::get_DenseRectMatrixType
      !> @copydetails MatrixTypes::get_DenseRectMatrixType
      PROCEDURE,PASS :: get => get_DenseRectMatrixType
      !> @copybrief MatrixTypes::transpose_DenseRectMatrixType
      !> @copydetails MatrixTypes::transpose_DenseRectMatrixType
      PROCEDURE,PASS :: transpose => transpose_DenseRectMatrixType
      !> @copybrief MatrixTypes::zeroentries_DenseRectMatrixType
      !> @copydetails MatrixTypes::zeroentries_DenseRectMatrixType
      PROCEDURE,PASS :: zeroentries => zeroentries_DenseRectMatrixType
  ENDTYPE DenseRectMatrixType

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
      !> @copybrief MatrixTypes::get_TriDiagMatrixType
      !> @copydetails MatrixTypes::get_TriDiagMatrixType
      PROCEDURE,PASS :: get => get_TriDiagMatrixType
      !> @copybrief MatrixTypes::transpose_TriDiagMatrixType
      !> @copydetails MatrixTypes::transpose_TriDiagMatrixType
      PROCEDURE,PASS :: transpose => transpose_TriDiagMatrixType
      !> @copybrief MatrixTypes::zeroentries_TriDiagMatrixType
      !> @copydetails MatrixTypes::zeroentries_TriDiagMatrixType
      PROCEDURE,PASS :: zeroentries => zeroentries_TriDiagMatrixType
  ENDTYPE TriDiagMatrixType
 
  !> @brief Type used to hold the bands in the banded type
  TYPE Band
    ! begin i,j index,  end i,j index, diagonal index relative to main
    INTEGER(SIK) :: ib, jb, ie, je, didx 
    REAL(SRK), ALLOCATABLE :: elem(:)
  ENDTYPE Band
 
  !> @brief The basic banded matrix type
  TYPE,EXTENDS(RectMatrixType) :: BandedMatrixType
    !> The number of elements of b
    INTEGER(SIK) :: bnum
    !> The bands of the matrix
    TYPE(Band),ALLOCATABLE :: b(:) 
!
!List of Type Bound Procedures
    CONTAINS
      !> @copybrief MatrixTypes::clear_BandedMatrixType
      !> @copydetails MatrixTypes::clear_BandedMatrixType
      PROCEDURE,PASS :: clear => clear_BandedMatrixType
      !> @copybrief MatrixTypes::init_BandedMatrixType
      !> @copydetails MatrixTypes::init_BandedMatrixType
      PROCEDURE,PASS :: init => init_BandedMatrixParam
      !> @copybrief MatrixTypes::set_BandedMatrixType
      !> @copydetails MatrixTypes::set_BandedMatrixType
      PROCEDURE,PASS :: set => set_BandedMatrixType
      !> @copybrief MatrixTypes::get_BandedMatrixType
      !> @copydetails MatrixTypes::get_BandedMatrixType
      PROCEDURE,PASS :: get => get_BandedMatrixType
      !> @copybrief MatrixTypes::transpose_BandedMatrixType
      !> @copydetails MatrixTypes::transpose_BandedMatrixType
      PROCEDURE,PASS :: transpose => transpose_BandedMatrixType
      !> @copybrief MatrixTypes::zeroentries_BandedMatrixType
      !> @copydetails MatrixTypes::zeroentries_BandedMatrixType
      PROCEDURE,PASS :: zeroentries => zeroentries_BandedMatrixType
  ENDTYPE BandedMatrixType
 
  !> @brief The basic sparse matrix type
  !>
  !> Matrix uses compressed sparse row storage format,
  !> as defined by Intel's MKL
  TYPE,EXTENDS(MatrixType) :: SparseMatrixType
    !> Number of non-zero entries in the matrix
    INTEGER(SIK) :: nnz=0
    !> The number of elements in each row
    INTEGER(SIK),ALLOCATABLE :: ia(:)
    !> The column indices for each element
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
      !> @copybrief MatrixTypes::setRow_SparseMatrixType
      !> @copydetails MatrixTypes::setRow_SparseMatrixType
      PROCEDURE,PASS :: setRow => setRow_SparseMatrixType
      !> @copybrief MatrixTypes::set_shape_SparseMatrixType
      !> @copydetails MatrixTypes::set_shape_SparseMatrixType
      PROCEDURE,PASS :: setShape => set_shape_SparseMatrixType
      !> @copybrief MatrixTypes::get_SparseMatrixType
      !> @copydetails MatrixTypes::get_SparseMatrixType
      PROCEDURE,PASS :: get => get_SparseMatrixType
      !> @copybrief MatrixTypes::transpose_SparseMatrixType
      !> @copydetails MatrixTypes::transpose_SparseMatrixType
      PROCEDURE,PASS :: transpose => transpose_SparseMatrixType
      !> @copybrief MatrixTypes::zeroentries_SparseMatrixType
      !> @copydetails MatrixTypes::zeroentries_SparseMatrixType
      PROCEDURE,PASS :: zeroentries => zeroentries_SparseMatrixType
  ENDTYPE SparseMatrixType

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='MATRIXTYPES_NATIVE'

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes Sparse Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param pList the parameter list
!>
    SUBROUTINE init_SparseMatrixParam(matrix,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_SparseMatrixParam'
      CLASS(SparseMatrixType),INTENT(INOUT) :: matrix
      CLASS(ParamType),INTENT(IN) :: Params
      TYPE(ParamType) :: validParams
      INTEGER(SIK) :: n,nnz

      !Check to set up required and optional param lists.
      IF(.NOT.MatrixType_Paramsflag) CALL MatrixTypes_Declare_ValidParams()

      !Validate against the reqParams and OptParams
      validParams=Params
      CALL validParams%validate(SparseMatrixType_reqParams)

      ! Pull Data From Parameter List
      CALL validParams%get('MatrixType->n',n)
      CALL validParams%get('MatrixType->nnz',nnz)
      CALL validParams%clear()

      IF(.NOT. matrix%isInit) THEN
        IF((n < 1).OR.(nnz < 1))  THEN
          CALL eMatrixType%raiseError('Incorrect   input to '// &
          modName//'::'//myName//' - Input parameters must be '// &
            'greater than 1!')
        ELSE
          matrix%isInit=.TRUE.
          matrix%n=n
          matrix%nnz=nnz
          matrix%jCount=0
          matrix%iPrev=0
          matrix%jPrev=0
          !regardless of sparsity, SIZE(ia)=n+1
          CALL dmallocA(matrix%ia,matrix%n+1)
          CALL dmallocA(matrix%a,matrix%nnz)
          CALL dmallocA(matrix%ja,matrix%nnz)
          !last entry of ia is known in advanced
          !this is per the intel MKL format
          matrix%ia(matrix%n+1)=matrix%nnz+1
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - MatrixType already initialized')
      ENDIF
    ENDSUBROUTINE init_SparseMatrixParam
!
!-------------------------------------------------------------------------------
!> @brief Initializes Tridiagonal Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param pList the parameter list
!>
    SUBROUTINE init_TriDiagMatrixParam(matrix,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_TriDiagMatrixParam'
      CLASS(TriDiagMatrixType),INTENT(INOUT) :: matrix
      CLASS(ParamType),INTENT(IN) :: Params
      TYPE(ParamType) :: validParams
      INTEGER(SIK) :: n
      LOGICAL(SBK) :: isSym

      !Check to set up required and optional param lists.
      IF(.NOT.MatrixType_Paramsflag) CALL MatrixTypes_Declare_ValidParams()

      !Validate against the reqParams and OptParams
      validParams=Params
      CALL validParams%validate(TriDiagMatrixType_reqParams)

      ! Pull Data From Parameter List
      CALL validParams%get('MatrixType->n',n)
      CALL validParams%get('MatrixType->isSym',isSym)
      CALL validParams%clear()

      IF(.NOT. matrix%isInit) THEN
        IF(n < 1) THEN
          CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of rows (n) must be '// &
              'greater than 1!')
        ELSE
          matrix%isInit=.TRUE.
          matrix%n=n
          IF(isSym) THEN
            matrix%isSymmetric=.TRUE.
          ELSE
            matrix%isSymmetric=.FALSE.
          ENDIF
          CALL dmallocA(matrix%a,3,n)
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - MatrixType already initialized')
      ENDIF
    ENDSUBROUTINE init_TriDiagMatrixParam
!
!-------------------------------------------------------------------------------
!> @brief Initializes Banded Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param pList the parameter list
!>
    SUBROUTINE init_BandedMatrixParam(matrix,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_BandedMatrixParam'
      CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
      CLASS(ParamType),INTENT(IN) :: Params
      TYPE(ParamType) :: validParams
      INTEGER(SIK) :: n, m, bnum

      !Check to set up required and optional param lists.
      IF(.NOT.MatrixType_Paramsflag) CALL MatrixTypes_Declare_ValidParams()

      !Validate against the reqParams and OptParams
      validParams=Params
      CALL validParams%validate(BandedMatrixType_reqParams)

      ! Pull Data From Parameter List
      CALL validParams%get('MatrixType->n',n)
      CALL validParams%get('MatrixType->m',m)
      CALL validParams%get('MatrixType->bnum',bnum)
      CALL validParams%clear()

      IF(.NOT. matrix%isInit) THEN
        IF(n < 1) THEN
          CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of rows (n) must'// &
              ' be greater than 1!')
        ELSEIF(m < 1) THEN
          CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of columns (m) must'// &
              ' be greater than 1!')
        ELSEIF(bnum < 0) THEN
          CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of band objects (bnum) must'// &
              ' be greater than 0!')
        ELSE
          matrix%isInit=.TRUE.
          matrix%n=n
          matrix%m=m
          ALLOCATE(matrix%b(bnum))
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - MatrixType already initialized')
      ENDIF
    ENDSUBROUTINE init_BandedMatrixParam
!
!-------------------------------------------------------------------------------
!> @brief Initializes Dense Rectangular Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param pList the parameter list
!>
    SUBROUTINE init_DenseRectMatrixParam(matrix,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_DenseRectMatrixParam'
      CLASS(DenseRectMatrixType),INTENT(INOUT) :: matrix
      CLASS(ParamType),INTENT(IN) :: Params
      TYPE(ParamType) :: validParams
      INTEGER(SIK) :: n,m

      !Check to set up required and optional param lists.
      IF(.NOT.MatrixType_Paramsflag) CALL MatrixTypes_Declare_ValidParams()

      !Validate against the reqParams and OptParams
      validParams=Params
      CALL validParams%validate(DenseRectMatrixType_reqParams)

      ! Pull Data From Parameter List
      CALL validParams%get('MatrixType->n',n)
      CALL validParams%get('MatrixType->m',m)
      CALL validParams%clear()

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
    ENDSUBROUTINE init_DenseRectMatrixParam
!
!-------------------------------------------------------------------------------
!> @brief Initializes Dense Square Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param pList the parameter list
!>
    SUBROUTINE init_DenseSquareMatrixParam(matrix,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_DenseSquareMatrixParam'
      CLASS(DenseSquareMatrixType),INTENT(INOUT) :: matrix
      CLASS(ParamType),INTENT(IN) :: Params
      TYPE(ParamType) :: validParams
      INTEGER(SIK) :: n
      LOGICAL(SBK) :: isSym

      !Check to set up required and optional param lists.
      IF(.NOT.MatrixType_Paramsflag) CALL MatrixTypes_Declare_ValidParams()

      !Validate against the reqParams and OptParams
      validParams=Params
      CALL validParams%validate(DenseSquareMatrixType_reqParams)

      ! Pull Data From Parameter List
      CALL validParams%get('MatrixType->n',n)
      CALL validParams%get('MatrixType->isSym',isSym)
      CALL validParams%clear()

      IF(.NOT. matrix%isInit) THEN
        IF(n < 1) THEN
          CALL eMatrixType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of rows (n) must be '// &
              'greater than 1!')
        ELSE
          matrix%isInit=.TRUE.
          matrix%n=n
          IF(isSym) THEN
            matrix%isSymmetric=.TRUE.
          ELSE
            matrix%isSymmetric=.FALSE.
          ENDIF
          CALL dmallocA(matrix%a,n,n)
        ENDIF
      ELSE
        CALL eMatrixType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - MatrixType already initialized')
      ENDIF
    ENDSUBROUTINE init_DenseSquareMatrixParam
!
!-------------------------------------------------------------------------------
!> @brief Clears the sparse matrix
!> @param matrix the matrix type to act on
!>
    SUBROUTINE clear_SparseMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_SparseMatrixType'
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
      IF(MatrixType_Paramsflag) CALL MatrixTypes_Clear_ValidParams()
    ENDSUBROUTINE clear_SparseMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Clears the dense square matrix
!> @param matrix the matrix type to act on
!>
    SUBROUTINE clear_DenseSquareMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_DenseSquareMatrixType'
      CLASS(DenseSquareMatrixType),INTENT(INOUT) :: matrix
      matrix%isInit=.FALSE.
      matrix%n=0
      matrix%isSymmetric=.FALSE.
      IF(ALLOCATED(matrix%a)) CALL demallocA(matrix%a)
      IF(MatrixType_Paramsflag) CALL MatrixTypes_Clear_ValidParams()
    ENDSUBROUTINE clear_DenseSquareMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Clears the tri-diagonal matrix
!> @param matrix the matrix type to act on
!>
    SUBROUTINE clear_TriDiagMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_TriDiagMatrixType'
      CLASS(TriDiagMatrixType),INTENT(INOUT) :: matrix
      matrix%isInit=.FALSE.
      matrix%n=0
      matrix%isSymmetric=.FALSE.
      IF(ALLOCATED(matrix%a)) CALL demallocA(matrix%a)
      IF(MatrixType_Paramsflag) CALL MatrixTypes_Clear_ValidParams()
     ENDSUBROUTINE clear_TriDiagMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Clears the banded matrix
!> @param matrix the matrix type to act on
!>
    SUBROUTINE clear_BandedMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_BandedMatrixType'
      CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK) :: i
      matrix%isInit=.FALSE.
      matrix%n=0
      matrix%m=0
      ! May want to add additional subroutine to Allocs.f90 so can use
      ! IF(ALLOCATED(matrix%b)) CALL demallocA(matrix%b)
      IF(ALLOCATED(matrix%b)) THEN
        DO i=1,matrix%bnum
          IF(ALLOCATED(matrix%b(i)%elem)) DEALLOCATE(matrix%b(i)%elem)
        ENDDO
        DEALLOCATE(matrix%b)
      ENDIF  
      matrix%bnum=0
      IF(MatrixType_Paramsflag) CALL MatrixTypes_Clear_ValidParams()
     ENDSUBROUTINE clear_BandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Clears the dense rectangular matrix
!> @param matrix the matrix type to act on
!>
    SUBROUTINE clear_DenseRectMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_DenseRectMatrixType'
      CLASS(DenseRectMatrixType),INTENT(INOUT) :: matrix
      matrix%isInit=.FALSE.
      matrix%n=0
      matrix%m=0
      IF(ALLOCATED(matrix%a)) CALL demallocA(matrix%a)
      IF(MatrixType_Paramsflag) CALL MatrixTypes_Clear_ValidParams()
    ENDSUBROUTINE clear_DenseRectMatrixType
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
      CHARACTER(LEN=*),PARAMETER :: myName='set_SparseMatrixType'
      CLASS(SparseMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK) :: ja_index
      LOGICAL(SBK) :: found_ja

      REQUIRE(matrix%isInit)
      REQUIRE(matrix%jCount>0)
      REQUIRE(j>0)
      REQUIRE(i>0)
      REQUIRE(i+1<=size(matrix%ia))
      REQUIRE(i<=matrix%n)

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
    ENDSUBROUTINE set_SparseMatrixtype
!
!-------------------------------------------------------------------------------
!> @brief Sets an entire row of values in the sparse matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j a list of j locations in the matrix
!> @param setval a list of values to set at the corresponding j locations
!>
!> This routine sets the values of an entire row in the sparse matrix.  It can only be used
!> If setShape has previously been applied to the same sparse matrix.  
!>
    SUBROUTINE setRow_SparseMatrixType(matrix,i,j,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='set_SparseMatrixType'
      CLASS(SparseMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j(:)
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK) :: ja_index,ja
      LOGICAL(SBK) :: found_ja

      REQUIRE(matrix%isInit)
      REQUIRE(((matrix%jCount > 0).AND.(i <= matrix%n)) .AND. ((ALL(j > 0)) .AND. (i > 0)))
      REQUIRE(SIZE(j)==SIZE(setval))
      !currently written assuming no all-zero rows.
      !pretty safe assumption.
      DO ja = 1, SIZE(j)
        found_ja=.FALSE.
        DO ja_index=matrix%ia(i),matrix%ia(i+1)-1
          IF(matrix%ja(ja_index) == j(ja)) THEN
            found_ja=.TRUE.
            EXIT
          ENDIF
        ENDDO
        REQUIRE(found_ja)
        matrix%a(ja_index)=setval(ja)
      ENDDO
    ENDSUBROUTINE setRow_SparseMatrixtype

!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the dense square matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
    SUBROUTINE set_DenseSquareMatrixType(matrix,i,j,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='set_DenseSquareMatrixType'
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
      CHARACTER(LEN=*),PARAMETER :: myName='set_TriDiagMatrixType'
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
!> @brief Sets the values in the banded matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
    SUBROUTINE set_BandedMatrixType(matrix,i,j,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='set_BandedMatrixType'
      CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(IN) :: setval
    ENDSUBROUTINE set_BandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the dense rectangular matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
    SUBROUTINE set_DenseRectMatrixType(matrix,i,j,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='set_DenseRectMatrixType'
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
      CHARACTER(LEN=*),PARAMETER :: myName='set_shape_SparseMatrixType'
      CLASS(SparseMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),OPTIONAL,INTENT(IN) :: setval

      REQUIRE(matrix%isInit)
      REQUIRE(i<=matrix%n) !row within bounds
      REQUIRE(j>0) !positive column
      REQUIRE(i>0) !positive row
      REQUIRE(matrix%jCount<matrix%nnz) !valid number of non-zero entries
      !check for increasing column order (or new row)
      REQUIRE((matrix%iPrev==i .AND. matrix%jPrev<j) .OR. (matrix%iPrev<i))

      IF(matrix%ia(i) == 0) matrix%ia(i)=matrix%jCount+1
      matrix%iPrev=i
      matrix%jPrev=j
      matrix%jCount=matrix%jCount+1
      IF(PRESENT(setval)) matrix%a(matrix%jCount)=setval
      matrix%ja(matrix%jCount)=j

    ENDSUBROUTINE set_shape_SparseMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Gets the values in the tridiagonal matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
    SUBROUTINE get_TriDiagMatrixType(matrix,i,j,getval)
      CHARACTER(LEN=*),PARAMETER :: myName='get_TriDiagMatrixType'
      CLASS(TriDiagMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(INOUT) :: getval
      IF(matrix%isInit) THEN
        IF(((j <= matrix%n) .AND. (i <= matrix%n)) &
            .AND. (i>=1) .AND. (j >= 1)) THEN
          !based on i,j, pull from correct location
          IF((j == (i-1)).AND. (i > 1)) THEN !sub-diag
            getval=matrix%a(1,i)
          ELSEIF((j == (i+1)) .AND. (i < matrix%n)) THEN !super-diag
            getval=matrix%a(3,i)
          ELSEIF(i == j) THEN
            getval=matrix%a(2,i)
          ENDIF
        ELSE
          getval=-1051._SRK
        ENDIF
      ENDIF
    ENDSUBROUTINE get_TriDiagMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Gets the values in the banded matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
    SUBROUTINE get_BandedMatrixType(matrix,i,j,getval)
      CHARACTER(LEN=*),PARAMETER :: myName='get_BandedMatrixType'
      CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(INOUT) :: getval
    ENDSUBROUTINE get_BandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Gets the values in the dense rectangular matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
    SUBROUTINE get_DenseRectMatrixType(matrix,i,j,getval)
      CHARACTER(LEN=*),PARAMETER :: myName='get_DenseRectMatrixType'
      CLASS(DenseRectMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(INOUT) :: getval
      IF(matrix%isInit) THEN
        IF(((j <= matrix%m) .AND. (i <= matrix%n)) .AND. ((j > 0) .AND. (i > 0))) THEN
          getval=matrix%a(i,j)
        ELSE
          getval=-1051._SRK
        ENDIF
      ENDIF
    ENDSUBROUTINE get_DenseRectMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Gets the values in the Dense Square matrix
!> @param declare the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!>
!> This routine gets the values of the sparse matrix.  If the (i,j) location is
!> out of bounds, then -1051.0 (an arbitrarily chosen key) is returned.
!>
    SUBROUTINE get_DenseSquareMatrixType(matrix,i,j,getval)
      CHARACTER(LEN=*),PARAMETER :: myName='get_DenseSquareMatrixType'
      CLASS(DenseSquareMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(INOUT) :: getval

      getval=0.0_SRK
      IF(matrix%isInit) THEN
        IF((i <= matrix%n) .AND. (j <= matrix%n) .AND. ((j > 0) .AND. (i > 0))) THEN
          getval=matrix%A(i,j)
        ELSE
          getval=-1051._SRK
        ENDIF
      ENDIF
    ENDSUBROUTINE get_DenseSquareMatrixtype
!
!-------------------------------------------------------------------------------
!> @brief Gets the values in the sparse matrix - presently untested
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!>
!> This routine gets the values of the sparse matrix.  If an (i,j) value
!> is not present, then 0.0 is returned.  If the (i,j) location is out of
!> bounds, fails if DBC is enabled.
!>
    SUBROUTINE get_SparseMatrixType(matrix,i,j,getval)
      CHARACTER(LEN=*),PARAMETER :: myName='get_SparseMatrixType'
      CLASS(SparseMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      INTEGER(SIK) :: ja_index
      LOGICAL(SBK) :: found_ja
      REAL(SRK),INTENT(INOUT) :: getval

      REQUIRE(matrix%isInit)
      REQUIRE(matrix%jCount>0)
      REQUIRE(i<=matrix%n)
      REQUIRE(j>0)
      REQUIRE(i>0)

      found_ja=.FALSE.
      DO ja_index=matrix%ia(i),matrix%ia(i+1)-1
        IF(matrix%ja(ja_index) == j) THEN
          found_ja=.TRUE.
          EXIT
        ENDIF
      ENDDO

      IF(found_ja) THEN
        getval=matrix%a(ja_index)
      ELSE
        getval=0.0_SRK
      ENDIF
    ENDSUBROUTINE get_SparseMatrixtype
!
!-------------------------------------------------------------------------------
!> @brief tranpose the matrix
!> @param matrix declare the matrix type to act on
!>
!>
    SUBROUTINE transpose_DenseSquareMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='transpose_DenseSquareMatrixType'
      CLASS(DenseSquareMatrixType),INTENT(INOUT) :: matrix
      CALL eMatrixType%raiseFatalError(modName//'::'//myName// &
        ' - routine is not implemented!')
    ENDSUBROUTINE transpose_DenseSquareMatrixType
!
!-------------------------------------------------------------------------------
!> @brief tranpose the matrix
!> @param matrix declare the matrix type to act on
!>
!>
    SUBROUTINE transpose_DenseRectMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='transpose_DenseRecMatrixType'
      CLASS(DenseRectMatrixType),INTENT(INOUT) :: matrix
      CALL eMatrixType%raiseFatalError(modName//'::'//myName// &
        ' - routine is not implemented!')
    ENDSUBROUTINE transpose_DenseRectMatrixType
!
!-------------------------------------------------------------------------------
!> @brief tranpose the matrix
!> @param matrix declare the matrix type to act on
!>
!>
    SUBROUTINE transpose_TriDiagMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='transpose_TriDiagMatrixType'
      CLASS(TriDiagMatrixType),INTENT(INOUT) :: matrix
      CALL eMatrixType%raiseFatalError(modName//'::'//myName// &
        ' - routine is not implemented!')
    ENDSUBROUTINE transpose_TriDiagMatrixType
!
!-------------------------------------------------------------------------------
!> @brief tranpose the matrix
!> @param matrix declare the matrix type to act on
!>
!>
    SUBROUTINE transpose_BandedMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='transpose_BandedMatrixType'
      CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
    ENDSUBROUTINE transpose_BandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief tranpose the matrix
!> @param matrix declare the matrix type to act on
!>
!>
    SUBROUTINE transpose_SparseMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='transpose_SparseMatrixType'
      CLASS(SparseMatrixType),INTENT(INOUT) :: matrix

      INTEGER(SIK),ALLOCATABLE :: tmp_ia(:),tmp_ja(:),A(:,:),tmp(:),Row(:,:),n_Row(:)
      REAL(SRK),ALLOCATABLE :: tmp_a(:)
      INTEGER(SIK) :: nnz,i,j,n,nnz_index,row_max,nnz_index_old,index_i
      n=Matrix%n
      nnz=Matrix%nnz
      ALLOCATE(tmp_ia(SIZE(Matrix%ia)))
      ALLOCATE(tmp_ja(SIZE(Matrix%ja)))
      ALLOCATE(tmp_a(SIZE(Matrix%a)))
      ALLOCATE(A(nnz,2))
      ALLOCATE(tmp(nnz))

      ! A is defined as A(nnz,1:2),first index is i, and second is j
      nnz_index=0
      DO i=1,n
        DO j=Matrix%ia(i),Matrix%ia(i+1)-1
          nnz_index=nnz_index+1
          A(nnz_index,1)=i
          A(nnz_index,2)=Matrix%ja(j)
        ENDDO
      ENDDO

      !Transpose A
      tmp=A(:,1)
      A(:,1)=A(:,2)
      A(:,2)=tmp

      ! reorder A by i and then j

      !sort A by the first index
      !Row(i,j), i is the ith row, j is the nnz index base on column in that row.

      ALLOCATE(n_Row(n))
      n_Row=0
      DO i=1,nnz
        index_i=A(i,1)
        n_Row(index_i)=n_Row(index_i)+1
      ENDDO
      row_max=MAXVAL(n_Row)

      ALLOCATE(Row(n,row_max))
      n_Row=0
      DO i=1,nnz
        index_i=A(i,1)
        n_Row(index_i)=n_Row(index_i)+1
        Row(index_i,n_Row(index_i))=i
      ENDDO

      !add new
      tmp_ia=0
      tmp_ja=0
      tmp_a=0.0_SRK

      tmp_ia(1)=1
      DO i=1,n
        tmp_ia(i+1)=tmp_ia(i)+n_Row(i)
      ENDDO

      nnz_index=0
      DO i=1,n
        DO j=1,n_Row(i)
          nnz_index=nnz_index+1
          nnz_index_old=Row(i,j)
          tmp_ja(nnz_index)=A(nnz_index_old,2)
          tmp_a(nnz_index)=Matrix%a(nnz_index_old)
        ENDDO
      ENDDO
      Matrix%a=tmp_a
      Matrix%ia=tmp_ia
      Matrix%ja=tmp_ja

      DEALLOCATE(tmp_ia)
      DEALLOCATE(tmp_ja)
      DEALLOCATE(tmp_a)
      DEALLOCATE(A)
      DEALLOCATE(tmp)
      DEALLOCATE(n_Row)
      DEALLOCATE(Row)
    ENDSUBROUTINE transpose_SparseMatrixType
!
!-------------------------------------------------------------------------------
!> @brief zero the matrix
!> @param matrix declare the matrix type to act on
!>
!>
    SUBROUTINE  zeroentries_SparseMatrixType(matrix)
    CHARACTER(LEN=*),PARAMETER :: myName='zeroentries_SparseMatrixType'
      CLASS(SparseMatrixType),INTENT(INOUT) :: matrix
      REQUIRE(matrix%isInit)
      matrix%a=0.0_SRK
    ENDSUBROUTINE zeroentries_SparseMatrixType
!
!-------------------------------------------------------------------------------
!> @brief zero the matrix
!> @param matrix declare the matrix type to act on
!>
!>
    SUBROUTINE  zeroentries_DenseSquareMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='zeroentries_DenseSquareMatrixType'
      CLASS(DenseSquareMatrixType),INTENT(INOUT) :: matrix
      REQUIRE(matrix%isInit)
      matrix%a=0.0_SRK
    ENDSUBROUTINE zeroentries_DenseSquareMatrixType
!
!-------------------------------------------------------------------------------
!> @brief zero the matrix
!> @param matrix declare the matrix type to act on
!>
!>
    SUBROUTINE  zeroentries_TriDiagMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='zeroentries_TriDiagMatrixType'
      CLASS(TriDiagMatrixType),INTENT(INOUT) :: matrix
      REQUIRE(matrix%isInit)
      matrix%a=0.0_SRK
    ENDSUBROUTINE zeroentries_TriDiagMatrixType
!
!-------------------------------------------------------------------------------
!> @brief zero the matrix
!> @param matrix declare the matrix type to act on
!>
!>
    SUBROUTINE  zeroentries_BandedMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='zeroentries_BandedMatrixType'
      CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK) :: i
      REQUIRE(matrix%isInit)
      DO i=1,matrix%bnum
        IF(ALLOCATED(matrix%b(i)%elem)) matrix%b(i)%elem=0.0_SRK
      ENDDO
    ENDSUBROUTINE zeroentries_BandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief zero the matrix
!> @param matrix declare the matrix type to act on
!>
!>
    SUBROUTINE  zeroentries_BandedMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='zeroentries_BandedMatrixType'
      CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
      REQUIRE(matrix%isInit)
    ENDSUBROUTINE zeroentries_BandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief zero the matrix
!> @param matrix declare the matrix type to act on
!>
!>
    SUBROUTINE  zeroentries_DenseRectMatrixType(matrix)
      CHARACTER(LEN=*),PARAMETER :: myName='zeroentries_DenseRectMatrixType'
      CLASS(DenseRectMatrixType),INTENT(INOUT) :: matrix
      REQUIRE(matrix%isInit)
      matrix%a=0.0_SRK
    ENDSUBROUTINE zeroentries_DenseRectMatrixType
!
ENDMODULE MatrixTypes_Native
