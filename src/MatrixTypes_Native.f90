!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Futility-native implementations of MatrixTypes
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE MatrixTypes_Native
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE ExceptionHandler
USE ParameterLists
USE Allocs
USE MatrixTypes_Base
USE ParallelEnv
USE Sorting

IMPLICIT NONE

#ifdef HAVE_MPI
#include <mpif.h>
#endif

PRIVATE
!
! List of public members
PUBLIC :: DenseSquareMatrixType
PUBLIC :: DenseRectMatrixType
PUBLIC :: TriDiagMatrixType
PUBLIC :: BandedMatrixType
PUBLIC :: DistributedBandedMatrixType
PUBLIC :: DistributedBlockBandedMatrixType
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
  ! jIdx stores j index of each element in band
  INTEGER(SIK), ALLOCATABLE :: jIdx(:)
  ! elem stores the data in the band (compressed)
  REAL(SRK), ALLOCATABLE :: elem(:)
ENDTYPE Band

!> @brief Integer array used for 'ragged' storage of band contents
TYPE IntPtr
  INTEGER(SIK),ALLOCATABLE :: p(:)
ENDTYPE IntPtr

!> @brief The basic banded matrix type
TYPE,EXTENDS(MatrixType) :: BandedMatrixType
  !> Map of band indices stored (-m to n)
  INTEGER(SIK),ALLOCATABLE :: bandIdx(:)
  !> The bands stored in the matrix
  TYPE(Band),ALLOCATABLE :: bands(:)
  !> Number of nonzero elements
  INTEGER(SIK) :: nnz
  !> Number of columns
  INTEGER(SIK) :: m
  !> Counter to keep track of added elements before assembly
  INTEGER(SIK) :: counter
  !> Temporary containers used before (and deallocated after) assembly
  INTEGER(SIK), ALLOCATABLE :: iTmp(:),jTmp(:)
  REAL(SRK),ALLOCATABLE :: elemTmp(:)
  !> Logical flag for whether the matrix is assembled
  LOGICAL(SBK) :: isAssembled
  !> Logical flag for whether bands are stored in ascending or desending
  !> order by index
  LOGICAL(SBK) :: isReversed

!
!List of Type Bound Procedures
  CONTAINS
    !> @copybrief MatrixTypes::clear_BandedMatrixType
    !> @copydetails MatrixTypes::clear_BandedMatrixType
    PROCEDURE,PASS :: clear => clear_BandedMatrixType
    !> @copybrief MatrixTypes::init_BandedMatrixType
    !> @copydetails MatrixTypes::init_BandedMatrixType
    PROCEDURE,PASS :: init => init_BandedMatrixParam
    !> @copybrief MatrixTypes::assemble_BandedMatrixType
    !> @copydetails MatrixTypes::assemble_BandedMatrixType
    PROCEDURE,PASS :: assemble => assemble_BandedMatrixType
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
    !> @copybrief MatrixTypes::binarySearch_BandedMatrixType
    !> @copydetails MatrixTypes::binarySearch_BandedMatrixType
    PROCEDURE,PASS,PRIVATE :: binarySearch => binarySearch_BandedMatrixType
ENDTYPE BandedMatrixType

!> @brief The distributed banded matrix type
TYPE,EXTENDS(DistributedMatrixType) :: DistributedBandedMatrixType
  !> Map of band indices stored (-m to n)
  INTEGER(SIK),ALLOCATABLE :: iOffsets(:),jOffsets(:)
  !> The column of banded matrix 'chunks' stored locally
  TYPE(BandedMatrixType),ALLOCATABLE :: chunks(:)
  !> Number of nonzero elements
  INTEGER(SIK) :: nnz
  !> Number of columns
  INTEGER(SIK) :: m
  !> Block size (smallest indivisble unit)
  INTEGER(SIK) :: blockSize
  !> Array of band sizes used to determine optimal communication
  TYPE(IntPtr), ALLOCATABLE :: bandSizes(:)
  !> Temporary containers used before (and deallocated after) assembly
  INTEGER(SIK), ALLOCATABLE :: iTmp(:),jTmp(:)
  REAL(SRK),ALLOCATABLE :: elemTmp(:)
!
!List of Type Bound Procedures
  CONTAINS
    !> @copybrief MatrixTypes::clear_DistributedBandedMatrixType
    !> @copydetails MatrixTypes::clear_DistributedBandedMatrixType
    PROCEDURE,PASS :: clear => clear_DistributedBandedMatrixType
    !> @copybrief MatrixTypes::init_DistributedBandedMatrixType
    !> @copydetails MatrixTypes::init_DistributedBandedMatrixType
    PROCEDURE,PASS :: init => init_DistributedBandedMatrixParam
    !> @copybrief MatrixTypes::assemble_DistributedBandedMatrixType
    !> @copydetails MatrixTypes::assemble_DistributedBandedMatrixType
    PROCEDURE,PASS :: assemble => assemble_DistributedBandedMatrixType
    !> @copybrief MatrixTypes::setrow_DistributedBandedMatrixType
    !> @copydetails MatrixTypes::setrow_DistributedBandedMatrixType
    PROCEDURE,PASS :: setrow => setrow_DistributedBandedMatrixType
    !> @copybrief MatrixTypes::set_DistributedBandedMatrixType
    !> @copydetails MatrixTypes::set_DistributedBandedMatrixType
    PROCEDURE,PASS :: set => set_DistributedBandedMatrixType
    !> @copybrief MatrixTypes::get_DistributedBandedMatrixType
    !> @copydetails MatrixTypes::get_DistributedBandedMatrixType
    PROCEDURE,PASS :: get => get_DistributedBandedMatrixType
    !> @copybrief MatrixTypes::transpose_DistributedBandedMatrixType
    !> @copydetails MatrixTypes::transpose_DistributedBandedMatrixType
    PROCEDURE,PASS :: transpose => transpose_DistributedBandedMatrixType
    !> @copybrief MatrixTypes::zeroentries_DistributedBandedMatrixType
    !> @copydetails MatrixTypes::zeroentries_DistributedBandedMatrixType
    PROCEDURE,PASS :: zeroentries => zeroentries_DistributedBandedMatrixType
ENDTYPE DistributedBandedMatrixType

!> @brief The block banded matrix type (designed for CMFD)
!> for now, the blocks will be assumed to be only on main diag
TYPE,EXTENDS(DistributedBandedMatrixType) :: DistributedBlockBandedMatrixType
  !> The block size is set by parent (DistrBanded::blockSize)
  !> The number of local blocks
  INTEGER(SIK) :: nlocalBlocks
  !> Block offset for this processor
  INTEGER(SIK) :: blockOffset
  !> Mask to effectively zero out block vals
  LOGICAL(SBK) :: blockMask
  !> The dense block container
  TYPE(DenseSquareMatrixType),ALLOCATABLE :: blocks(:)
!
!List of Type Bound Procedures
  CONTAINS
    !> @copybrief MatrixTypes::clear_DistributedBlockBandedMatrixType
    !> @copydetails MatrixTypes::clear_DistributedBlockBandedMatrixType
    PROCEDURE,PASS :: clear => clear_DistributedBlockBandedMatrixType
    !> @copybrief MatrixTypes::init_DistributedBlockBandedMatrixType
    !> @copydetails MatrixTypes::init_DistributedBlockBandedMatrixType
    PROCEDURE,PASS :: init => init_DistributedBlockBandedMatrixParam
    ! The assemble routine will remain unchanged
    ! The setRow routine will remain unchanged (unimplemented)
    !> @copybrief MatrixTypes::set_DistributedBlockBandedMatrixType
    !> @copydetails MatrixTypes::set_DistributedBlockBandedMatrixType
    PROCEDURE,PASS :: set => set_DistributedBlockBandedMatrixType
    !> @copybrief MatrixTypes::get_DistributedBlockBandedMatrixType
    !> @copydetails MatrixTypes::get_DistributedBlockBandedMatrixType
    PROCEDURE,PASS :: get => get_DistributedBlockBandedMatrixType
    ! the transpose routine will remain unchanged (unimplemented)
    !> @copybrief MatrixTypes::zeroentries_DistributedBlockBandedMatrixType
    !> @copydetails MatrixTypes::zeroentries_DistributedBlockBandedMatrixType
    PROCEDURE,PASS :: zeroentries => zeroentries_DistributedBlockBandedMatrixType
    !> @copybrief MatrixTypes::setBlockMask_DistributedBlockBandedMatrixType
    !> @copydetails MatrixTypes::setBlockMask_DistributedBlockBandedMatrixType
    PROCEDURE,PASS :: setBlockMask => setBlockMask_DistributedBlockBandedMatrixType
ENDTYPE DistributedBlockBandedMatrixType


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
!> @param Params the parameter list
!>
SUBROUTINE init_SparseMatrixParam(matrix,Params)
  CLASS(SparseMatrixType),INTENT(INOUT) :: matrix
  CLASS(ParamType),INTENT(IN) :: Params
  TYPE(ParamType) :: validParams
  INTEGER(SIK) :: n,nnz

  REQUIRE(.NOT.matrix%isInit)

  !Check to set up required and optional param lists.
  IF(.NOT.MatrixType_Paramsflag) CALL MatrixTypes_Declare_ValidParams()

  !Validate against the reqParams and OptParams
  validParams=Params
  CALL validParams%validate(SparseMatrixType_reqParams)

  ! Pull Data From Parameter List
  CALL validParams%get('MatrixType->n',n)
  CALL validParams%get('MatrixType->nnz',nnz)
  CALL validParams%clear()
  REQUIRE(n > 0)
  REQUIRE(nnz > 0)

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

ENDSUBROUTINE init_SparseMatrixParam
!
!-------------------------------------------------------------------------------
!> @brief Initializes Tridiagonal Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param Params the parameter list
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
!> @param Params the parameter list
!>
SUBROUTINE init_BandedMatrixParam(matrix,Params)
  CHARACTER(LEN=*),PARAMETER :: myName='init_BandedMatrixParam'
  CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
  CLASS(ParamType),INTENT(IN) :: Params
  TYPE(ParamType) :: validParams
  INTEGER(SIK) :: n,m,nnz

  !Check to set up required and optional param lists.
  IF(.NOT.MatrixType_Paramsflag) CALL MatrixTypes_Declare_ValidParams()

  !Validate against the reqParams and OptParams
  validParams=Params
  CALL validParams%validate(BandedMatrixType_reqParams)

  ! Pull Data From Parameter List
  CALL validParams%get('MatrixType->n',n)
  m=n
  IF (validParams%has('MatrixType->m')) CALL validParams%get('MatrixType->m',m)
  CALL validParams%get('MatrixType->nnz',nnz)
  CALL validParams%clear()

  IF(.NOT. matrix%isInit) THEN
    IF(n <= 1) THEN
      CALL eMatrixType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Number of rows (n) must be greater than 1!')
    ELSEIF(m <= 1) THEN
      CALL eMatrixType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Number of columns (m) must be greater than 1!')
    ELSEIF(nnz < 1) THEN
      CALL eMatrixType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Number of nonzero elements (nnz) must be greater than 0!')
    ELSE
      ALLOCATE(matrix%iTmp(nnz))
      ALLOCATE(matrix%jTmp(nnz))
      ALLOCATE(matrix%elemTmp(nnz))

      matrix%isInit=.TRUE.
      matrix%isAssembled=.FaLSE.
      matrix%isReversed=.FALSE.
      matrix%counter=0_SRK
      matrix%n=n
      matrix%m=m
      matrix%nnz=nnz
    ENDIF
  ELSE
    CALL eMatrixType%raiseError('Incorrect call to '// &
        modName//'::'//myName//' - MatrixType already initialized')
  ENDIF
ENDSUBROUTINE init_BandedMatrixParam
!
!-------------------------------------------------------------------------------
!> @brief Initializes Distributed Banded Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param Params the parameter list
!>
SUBROUTINE init_DistributedBandedMatrixParam(matrix,Params)
  CLASS(DistributedBandedMatrixType),INTENT(INOUT) :: matrix
  CLASS(ParamType),INTENT(IN) :: Params
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='init_DistributedBandedMatrixParam'
  TYPE(ParamType) :: validParams
  INTEGER(SIK) :: n,m,nnz,commID,rank,mpierr,nproc,i,blocksize,nlocal

  !Check to set up required and optional param lists.
  IF(.NOT.MatrixType_Paramsflag) CALL MatrixTypes_Declare_ValidParams()

  !Validate against the reqParams and OptParams
  validParams=Params
  CALL validParams%validate(DistributedBandedMatrixType_reqParams, &
      DistributedBandedMatrixType_optParams)

  ! Pull Data From Parameter List
  CALL validParams%get('MatrixType->n',n)
  CALL validParams%get('MatrixType->m',m)
  CALL validParams%get('MatrixType->MPI_Comm_ID',commID)
  CALL validParams%get('MatrixType->nnz',nnz)
  CALL validParams%get('MatrixType->blockSize',blockSize)
  CALL validParams%get('MatrixType->nlocal',nlocal)
  CALL validParams%clear()

  IF(.NOT. matrix%isInit) THEN
    IF(n <= 1) THEN
      CALL eMatrixType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Number of rows (n) must be greater than 1!')
    ELSEIF(m <= 1) THEN
      CALL eMatrixType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Number of columns (m) must be greater than 1!')
    ELSEIF(nnz <= 1) THEN
      CALL eMatrixType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - Number of nonzero entries (nnz) must be greater than 0!')
    ELSEIF(commID == MPI_COMM_NULL) THEN
      CALL eMatrixType%raiseError('Incorrect input to '//modName//'::'//myName// &
          ' - MPI communicator cannot have the same value as MPI_COMM_NULL')
    ELSE
      CALL MPI_Comm_rank(commID,rank,mpierr)
      CALL MPI_Comm_size(commID,nproc,mpierr)

      REQUIRE(MOD(n,blockSize)==0)
      REQUIRE(MOD(m,blockSize)==0)

      ALLOCATE(matrix%jOffsets(nproc+1))
      ALLOCATE(matrix%iOffsets(nproc+1))
      matrix%jOffsets(1) = 0
      matrix%iOffsets(1) = 0

      matrix%nlocal = 0
      n=n/blockSize
      m=m/blockSize
      ! We assume that the matrix is somewhat evenly balanced
      ! among its columns; that is, no one chunk of columns contains
      ! more than twice the average nonzero entries
      ALLOCATE(matrix%iTmp(2*nnz/nProc))
      ALLOCATE(matrix%jTmp(2*nnz/nProc))
      ALLOCATE(matrix%elemTmp(2*nnz/nProc))
      IF(nlocal < 0) THEN
        ! Automatic division of matrix by greedy partitioning of blocks
        DO i=2,nproc+1
          matrix%jOffsets(i)=matrix%jOffsets(i-1)+m/nproc
          IF(MOD(m,nproc) > i-2) THEN
            matrix%jOffsets(i)=matrix%jOffsets(i)+1
          ENDIF
        ENDDO

        DO i=2,nproc+1
          matrix%iOffsets(i)=matrix%iOffsets(i-1)+n/nproc
          IF(MOD(n,nproc) > i-2) THEN
            matrix%iOffsets(i)=matrix%iOffsets(i)+1
          ENDIF
        ENDDO
        ! Separate i/jOffsets are maintained in the case of a rectangular
        ! matrix
        matrix%iOffsets=matrix%iOffsets*blockSize
        matrix%jOffsets=matrix%jOffsets*blockSize
      ELSE
        ! Partitioning explicitly specified (square only)
        REQUIRE(MOD(nlocal,blockSize) == 0)
        REQUIRE(m == n)
        ! Gather all nlocal and sum going forward
        CALL MPI_AllGather(nlocal,1,MPI_INTEGER,matrix%iOffsets(2),1, &
            MPI_INTEGER,commID,mpierr)
        DO i=2,nproc+1
          matrix%iOffsets(i)=matrix%iOffsets(i)+matrix%iOffsets(i-1)
        ENDDO
        REQUIRE(matrix%iOffsets(SIZE(matrix%iOffsets)) == n*blockSize)
        matrix%jOffsets(:)=matrix%iOffsets(:)
      ENDIF

      matrix%isInit=.TRUE.
      matrix%isAssembled=.FALSE.
      matrix%n=n*blockSize
      matrix%m=m*blockSize
      matrix%nnz=nnz
      matrix%blockSize = blockSize
      matrix%comm=commID
      ALLOCATE(matrix%bandSizes(nProc))
    ENDIF
  ELSE
    CALL eMatrixType%raiseError('Incorrect call to '// &
        modName//'::'//myName//' - MatrixType already initialized')
  ENDIF
#endif
ENDSUBROUTINE init_DistributedBandedMatrixParam

!
!-------------------------------------------------------------------------------
!> @brief Initializes Distributed Block-Banded Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param Params the parameter list
!>
SUBROUTINE init_DistributedBlockBandedMatrixParam(matrix,Params)
  CLASS(DistributedBlockBandedMatrixType),INTENT(INOUT) :: matrix
  CLASS(ParamType),INTENT(IN) :: Params
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='init_DistributedBlockBandedMatrixParam'
  TYPE(ParamType) :: validParams,blockParams
  INTEGER(SIK) :: n,rank,mpierr,commID,nproc,i,blockSize,nlocal
  INTEGER(SIK),ALLOCATABLE :: blkOffsetTmp(:)

  IF(.NOT. matrix%isInit) THEN
    !Check to set up required and optional param lists.
    IF(.NOT. MatrixType_Paramsflag) CALL MatrixTypes_Declare_ValidParams()

    !Validate against the reqParams and OptParams
    validParams=Params
    CALL validParams%validate(DistributedBlockBandedMatrixType_reqParams, &
        DistributedBlockBandedMatrixType_optParams)

    ! Pull Data From Parameter List
    CALL validParams%get('MatrixType->n',n)
    CALL validParams%get('MatrixType->MPI_Comm_ID',commID)
    CALL validParams%get('MatrixType->blockSize',blockSize)
    CALL validParams%get('MatrixType->nlocal',nlocal)

    REQUIRE(.NOT. matrix%isInit)
    REQUIRE(blockSize > 0)
    REQUIRE(MOD(n,blockSize) == 0)

    matrix%blockMask=.FALSE.

    ! Allocate the blocks, then adjust the parameter list and proceed to
    ! %parent%

    ! Default to greedy partitioning, respecting blockSize
    CALL MPI_Comm_rank(commID,rank,mpierr)
    CALL MPI_Comm_size(commID,nproc,mpierr)
    REQUIRE(n/blockSize >= nproc)

    n=n/blockSize
    ! If nlocal < 0, default to greedy partitioning
    IF(nlocal < 0) THEN
      IF(rank < MOD(n,nproc)) THEN
        matrix%nlocalBlocks=n/nproc+1
        matrix%blockOffset=(rank)*(n/nproc+1)
      ELSE
        matrix%nlocalBlocks=n/nproc
        matrix%blockOffset=(rank)*(n/nproc)+MOD(n,nproc)
      ENDIF
    ELSE
      ALLOCATE(blkOffsetTmp(nproc))
      REQUIRE(MOD(nlocal,blockSize) == 0)
      matrix%nlocalBlocks=nlocal/blockSize
      CALL MPI_AllGather(matrix%nlocalBlocks,1,MPI_INTEGER,blkOffsetTmp,1, &
          MPI_INTEGER,commID,mpierr)
      matrix%blockOffset=SUM(blkOffsetTmp(1:rank))
      DEALLOCATE(blkOffsetTmp)
    ENDIF

    ALLOCATE(matrix%blocks(matrix%nlocalBlocks))
    CALL blockParams%clear()
    CALL blockParams%add('MatrixType->n',blockSize)
    CALL blockParams%add('MatrixType->isSym',.FALSE.)

    DO i=1,matrix%nlocalblocks
      CALL matrix%blocks(i)%init(blockParams)
    ENDDO
    CALL blockParams%clear()

    CALL validParams%add('MatrixType->m',n*blockSize)
    CALL matrix%DistributedBandedMatrixType%init(validParams)
    CALL validParams%clear()
  ELSE
    CALL eMatrixType%raiseError('Incorrect call to '// &
        modName//'::'//myName//' - MatrixType already initialized')
  ENDIF

#endif
ENDSUBROUTINE init_DistributedBlockBandedMatrixParam

!
!-------------------------------------------------------------------------------
!> @brief Assembles Serial Banded Matrix Type. This rearranges values set into
!>        matrix bands, and sets the structure to read-only
!> @param matrix the matrix type to act on
!>
SUBROUTINE assemble_BandedMatrixType(thisMatrix)
  CLASS(BandedMatrixType),INTENT(INOUT) :: thisMatrix
  INTEGER(SIK),ALLOCATABLE :: numOnDiag(:),idxOrig(:)
  INTEGER(SIK),ALLOCATABLE :: iSort(:),jSort(:)
  REAL(SRK),ALLOCATABLE :: valSort(:)
  INTEGER(SIK) :: counter,nBands,i,j
  INTEGER(SLK) :: iLong,jLong,nLong,mLong
  INTEGER(SLK),ALLOCATABLE :: diagRank(:)

  REQUIRE(thisMatrix%isInit)

  ALLOCATE(diagRank(thisMatrix%counter))
  ALLOCATE(idxOrig(thisMatrix%counter))
  IF (thisMatrix%isAssembled) RETURN
  DO i=1,thisMatrix%counter
    iLong=INT(thisMatrix%iTmp(i),kind=SLK)
    jLong=INT(thisMatrix%jTmp(i),kind=SLK)
    nLong=INT(thisMatrix%n,kind=SLK)
    mLong=INT(thisMatrix%m,kind=SLK)
    idxOrig(i)=i
    ! The diagonal rank counts upwards as one moves southeast in the matrix,
    ! starting at a large negative number in the bottom left, and reaching
    ! zero near the middle
    IF(thisMatrix%n-thisMatrix%iTmp(i)+1+thisMatrix%jTmp(i) &
        <= MAX(thisMatrix%m,thisMatrix%n)) THEN
      diagRank(i)=-mLong*nLong/2+jLong-1+((nLong-iLong+jLong-1)*(nLong-iLong+jLong))/2
    ELSE
      diagRank(i) = nLong*mLong/2-(mLong-jLong+((iLong+mLong-jLong-1)*(iLong+mLong-jLong))/2)
    ENDIF
  ENDDO

  CALL sort(diagRank,idxOrig)
  ! Now update iTmp,jTmp,etc
  ALLOCATE(iSort(SIZE(idxOrig)))
  iSort = thisMatrix%iTmp(idxOrig)
  DEALLOCATE(thisMatrix%iTmp)
  jSort = thisMatrix%jTmp(idxOrig)
  DEALLOCATE(thisMatrix%jTmp)
  valSort = thisMatrix%elemTmp(idxOrig)
  DEALLOCATE(thisMatrix%elemTmp)

  ! Find number of elements in each band
  ALLOCATE(numOnDiag(thisMatrix%m+thisMatrix%n-1))
  numOnDiag=0_SIK
  DO i=1,SIZE(iSort)
    numOnDiag(jSort(i)-iSort(i)+thisMatrix%n)=1+numOnDiag(jSort(i)-iSort(i)+thisMatrix%n)
  ENDDO
  ! Now count up the number of bands necessary
  nBands=0
  DO i=1,SIZE(numOnDiag)
    IF(numOnDiag(i)/=0) THEN
      nBands=nBands+1
    ENDIF
  ENDDO
  ALLOCATE(thisMatrix%bandIdx(nBands))
  ALLOCATE(thisMatrix%bands(nBands))
  counter=1
  ! Allocate the bands
  DO i=1,SIZE(numOnDiag)
    IF(numOnDiag(i)/=0) THEN
      ALLOCATE(thisMatrix%bands(counter)%jIdx(numOnDiag(i)))
      ALLOCATE(thisMatrix%bands(counter)%elem(numOnDiag(i)))
      thisMatrix%bandIdx(counter)=i-thisMatrix%n
      counter=counter+1
    ENDIF
  ENDDO
  counter=1
  ! Set the bands
  DO i=1,SIZE(thisMatrix%bandIdx)
    DO j=1,SIZE(thisMatrix%bands(i)%jIdx)
      thisMatrix%bands(i)%jIdx(j)=jSort(counter)
      thisMatrix%bands(i)%elem(j)=valSort(counter)
      counter=counter+1
    ENDDO
  ENDDO
  thisMatrix%isAssembled=.TRUE.
ENDSUBROUTINE assemble_BandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Assembles Distributed Banded Matrix Type. This sets some values and
!>        assembles all chunks, setting the structure to read-only
!> @param thisMatrix the matrix type to act on
!> @param ierr optional error code return (not used for native types)
!>
SUBROUTINE assemble_DistributedBandedMatrixType(thisMatrix,ierr)
  CLASS(DistributedBandedMatrixType),INTENT(INOUT) :: thisMatrix
  INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef HAVE_MPI
  TYPE(ParamType) :: bandedPList
  INTEGER(SIK) :: mpierr, rank, nproc, i, j, nBandLocal
  INTEGER(SIK),ALLOCATABLE :: nnzPerChunk(:),nband(:),bandSizeTmp(:)

  REQUIRE(thisMatrix%isInit)
  CALL MPI_Comm_rank(thisMatrix%comm,rank,mpierr)
  CALL MPI_Comm_size(thisMatrix%comm,nproc,mpierr)
  ALLOCATE(nnzPerChunk(nProc))
  ALLOCATE(nBand(nProc))
  nnzPerChunk=0
  DO i=1,thisMatrix%nLocal
    DO j=1,nproc
      IF(thisMatrix%iTmp(i) > thisMatrix%iOffsets(j) .AND. &
          thisMatrix%iTmp(i) <= thisMatrix%iOffsets(j+1)) THEN
        nnzPerChunk(j)=nnzPerChunk(j)+1
        EXIT
      ENDIF
    ENDDO
  ENDDO
  CALL bandedPList%clear()

  ALLOCATE(thisMatrix%chunks(nProc))
  DO i=1,nProc
    IF(nnzPerChunk(i) > 0) THEN
      CALL bandedPList%add('MatrixType->nnz',nnzPerChunk(i))
      CALL bandedPList%add('MatrixType->n',thisMatrix%iOffsets(i+1) &
          -thisMatrix%iOffsets(i))
      CALL bandedPList%add('MatrixType->m',thisMatrix%jOffsets(rank+2) &
          -thisMatrix%jOffsets(rank+1))
      CALL thisMatrix%chunks(i)%init(bandedPList)

      DO j=1,thisMatrix%nLocal
        IF(thisMatrix%iTmp(j) > thisMatrix%iOffsets(i) .AND. &
            thisMatrix%iTmp(j) <= thisMatrix%iOffsets(i+1)) THEN
          CALL thisMatrix%chunks(i)%set(thisMatrix%iTmp(j)-thisMatrix%iOffsets(i), &
              thisMatrix%jTmp(j)-thisMatrix%jOffsets(rank+1),thisMatrix%elemTmp(j))
        ENDIF
      ENDDO
      CALL thisMatrix%chunks(i)%assemble()
      CALL bandedPList%clear()
    ENDIF
    ! Set nbandlocal (to be gathered into nband
    IF(thisMatrix%chunks(i)%isInit) THEN
      IF(2*thisMatrix%chunks(i)%nnz/3 < thisMatrix%chunks(i)%n) THEN
        nBandLocal=SIZE(thisMatrix%chunks(i)%bandIdx)
      ELSE
        nBandLocal=-1
      ENDIF
    ELSE
      nBandLocal=0
    ENDIF

    ! Gather to rank i-1 into nBand(:)
    CALL MPI_Gather(nBandLocal,1,MPI_INTEGER,nband(1),1,MPI_INTEGER,i-1, &
        thisMatrix%comm,mpierr)
    ! Have rank i-1 allocate
    IF(rank == i-1) THEN
      DO j=1,nProc
        IF(j /= i) THEN
          IF(nBand(j) > 0) THEN
            ALLOCATE(thisMatrix%bandSizes(j)%p(nBand(j)))
            CALL MPI_Recv(thisMatrix%bandSizes(j)%p(1),nBand(j),MPI_INTEGER, &
                j-1, 0, thisMatrix%comm,MPI_STATUS_IGNORE, mpierr)
          ELSE
            IF(nBand(j) /= 0) THEN
              ALLOCATE(thisMatrix%bandSizes(j)%p(1))
              thisMatrix%bandSizes(j)%p(1)=-1
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    ELSE
      IF(nBandLocal > 0) THEN
        ALLOCATE(bandSizeTmp(nBandLocal))
        DO j=1,nBandLocal
          bandSizeTmp(j)=SIZE(thisMatrix%chunks(i)%bands(j)%elem)
        ENDDO
        CALL MPI_Send(bandSizeTmp(:),nBandLocal,MPI_INTEGER,i-1,0, &
            thisMatrix%comm,mpierr)
        DEALLOCATE(bandSizeTmp)
      ENDIF
    ENDIF
  ENDDO

  thisMatrix%isAssembled = .TRUE.
  DEALLOCATE(thisMatrix%iTmp)
  DEALLOCATE(thisMatrix%jTmp)
  DEALLOCATE(thisMatrix%elemTmp)
#endif
ENDSUBROUTINE assemble_DistributedBandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Initializes Dense Rectangular Matrix Type with a Parameter List
!> @param matrix the matrix type to act on
!> @param Params the parameter list
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
!> @param Params the parameter list
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
  CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
  INTEGER(SIK) :: i

  matrix%isInit=.FALSE.
  matrix%isAssembled=.FALSE.
  matrix%n=0
  matrix%m=0
  matrix%counter=0
  IF(ALLOCATED(matrix%bands)) THEN
    DO i=1,SIZE(matrix%bandIdx)
      IF(ALLOCATED(matrix%bands(i)%elem)) DEALLOCATE(matrix%bands(i)%elem)
    ENDDO
    DEALLOCATE(matrix%bands)
  ENDIF
  IF(ALLOCATED(matrix%bandIdx)) DEALLOCATE(matrix%bandIdx)
  IF(ALLOCATED(matrix%iTmp)) DEALLOCATE(matrix%iTmp)
  IF(ALLOCATED(matrix%jTmp)) DEALLOCATE(matrix%jTmp)
  IF(ALLOCATED(matrix%elemTmp)) DEALLOCATE(matrix%elemTmp)
  matrix%nnz=0
  IF(MatrixType_Paramsflag) CALL MatrixTypes_Clear_ValidParams()
 ENDSUBROUTINE clear_BandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Clears the distributed banded matrix
!> @param matrix the matrix type to act on
!>
SUBROUTINE clear_DistributedBandedMatrixType(matrix)
  CLASS(DistributedBandedMatrixType),INTENT(INOUT) :: matrix
#ifdef HAVE_MPI
  INTEGER(SIK) :: i

  matrix%isInit=.FALSE.
  matrix%isCreated=.FALSE.
  matrix%isAssembled=.FALSE.
  matrix%comm=MPI_COMM_NULL
  matrix%n=0
  matrix%m=0
  matrix%nnz=0
  matrix%nLocal=0
  IF(ALLOCATED(matrix%chunks)) THEN
    DO i=1,SIZE(matrix%chunks)
      CALL matrix%chunks(i)%clear()
    ENDDO
    DEALLOCATE(matrix%chunks)
  ENDIF
  IF(ALLOCATED(matrix%bandSizes)) THEN
    DO i=1,SIZE(matrix%bandSizes)
      IF(ALLOCATED(matrix%bandSizes(i)%p)) DEALLOCATE(matrix%bandSizes(i)%p)
    ENDDO
    DEALLOCATE(matrix%bandSizes)
  ENDIF
  IF(ALLOCATED(matrix%iOffsets)) DEALLOCATE(matrix%iOffsets)
  IF(ALLOCATED(matrix%jOffsets)) DEALLOCATE(matrix%jOffsets)
  IF(ALLOCATED(matrix%iTmp)) DEALLOCATE(matrix%iTmp)
  IF(ALLOCATED(matrix%jTmp)) DEALLOCATE(matrix%jTmp)
  IF(ALLOCATED(matrix%elemTmp)) DEALLOCATE(matrix%elemTmp)
  IF(MatrixType_Paramsflag) CALL MatrixTypes_Clear_ValidParams()
#endif
 ENDSUBROUTINE clear_DistributedBandedMatrixType

!
!-------------------------------------------------------------------------------
!> @brief Clears the distributed block banded matrix
!> @param matrix the matrix type to act on
!>
SUBROUTINE clear_DistributedBlockBandedMatrixType(matrix)
  CLASS(DistributedBlockBandedMatrixType),INTENT(INOUT) :: matrix
#ifdef HAVE_MPI
  INTEGER(SIK) :: i
  matrix%blockMask=.FALSE.
  matrix%blockSize=0
  matrix%nLocalBlocks=0
  matrix%blockOffset=0
  matrix%isInit=.FALSE.
  matrix%isCreated=.FALSE.
  matrix%isAssembled=.FALSE.

  DO i=1,SIZE(matrix%blocks)
    CALL matrix%blocks(i)%clear()
  ENDDO
  DEALLOCATE(matrix%blocks)

  CALL matrix%DistributedBandedMatrixType%clear()
#endif
 ENDSUBROUTINE clear_DistributedBlockBandedMatrixType
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
!> @brief Sets the values in the distributed banded matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
SUBROUTINE set_DistributedBandedMatrixType(matrix,i,j,setval)
  CLASS(DistributedBandedMatrixType),INTENT(INOUT) :: matrix
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK),INTENT(IN) :: j
  REAL(SRK),INTENT(IN) :: setval
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='set_DistributedBandedMatrixType'
  INTEGER(SIK) :: rank,nproc,mpierr,k
  LOGICAL(SBK) :: flag
  CHARACTER(LEN=64) :: msg

  flag=.FALSE.
  REQUIRE(matrix%isInit)
  IF(.NOT. matrix%isAssembled) THEN
    ! If the matrix is not yet assembled, we are adding to the temporary
    ! containers i/j/elemTmp
    IF((j <= matrix%m) .AND. (i <= matrix%n) .AND. (i >= 1) .AND. (j >= 1)) THEN
      CALL MPI_Comm_rank(matrix%comm,rank,mpierr)
      CALL MPI_Comm_size(matrix%comm,nproc,mpierr)

      IF(j > matrix%jOffsets(rank+1) .AND. j <= matrix%jOffsets(rank+2)) THEN
        matrix%nLocal=matrix%nLocal+1
        IF(matrix%nLocal > 2*matrix%nnz/nproc) THEN
          CALL eMatrixType%raiseError('Matrix Element imbalance '// &
              modName//'::'//myName//' - Check nnz or use a different type')
        ENDIF
        matrix%iTmp(matrix%nLocal)=i
        matrix%jTmp(matrix%nLocal)=j
        matrix%elemTmp(matrix%nLocal)=setval
        flag=.TRUE.
      ENDIF
    ENDIF
  ELSE
    ! If the matrix is assmbled then we find the corresponding chunk and
    ! call set there
    CALL MPI_Comm_rank(matrix%comm,rank,mpierr)
    IF(j > matrix%jOffsets(rank+1) .AND. j <= matrix%jOffsets(rank+2)) THEN
      DO k=1,SIZE(matrix%jOffsets)-1
        IF(i > matrix%iOffsets(k) .AND. i <= matrix%iOffsets(k+1)) THEN
          IF(matrix%chunks(k)%isInit) THEN
            CALL matrix%chunks(k)%set(i-matrix%iOffsets(k), &
                j-matrix%jOffsets(rank+1),setval)
            flag=.TRUE.
            EXIT
          ENDIF
        ENDIF
      ENDDO
    ENDIF
  ENDIF
  IF(.NOT. flag) THEN
    WRITE(msg,'(I2,A,I9,A,I9)') rank," at ",i,',',j
    CALL eMatrixType%raiseWarning('Invalid matrix set in '// &
        modName//'::'//myName//' for rank '//msg)
  ENDIF
#endif
ENDSUBROUTINE set_DistributedBandedMatrixType

!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the distributed block banded matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
SUBROUTINE set_DistributedBlockBandedMatrixType(matrix,i,j,setval)
  CLASS(DistributedBlockBandedMatrixType),INTENT(INOUT) :: matrix
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK),INTENT(IN) :: j
  REAL(SRK),INTENT(IN) :: setval
#ifdef HAVE_MPI
  CHARACTER(LEN=*),PARAMETER :: myName='set_DistributedBlockBandedMatrixType'
  INTEGER(SIK) :: rank,mpierr,offset
  LOGICAL(SBK) :: flag
  CHARACTER(LEN=64) :: msg
  REQUIRE(matrix%isInit)

  flag=.FALSE.
  CALL MPI_Comm_Rank(matrix%comm,rank,mpierr)
  ! Check if element is in a block; if not call parent
  IF((i-1)/matrix%blockSize /= (j-1)/matrix%blockSize) THEN
    CALL matrix%DistributedBandedMatrixType%set(i,j,setval)
    flag = .TRUE.
  ELSE
    IF((j <= matrix%m) .AND. (i <= matrix%n) .AND. (i >= 1) .AND. (j >= 1)) THEN
      IF(.NOT. matrix%blockMask) THEN
        offset=((i-1)/matrix%blockSize)*matrix%blockSize
        IF(j > matrix%jOffsets(rank+1) .AND. (j <= matrix%jOffsets(rank+2))) THEN
          CALL matrix%blocks((j-1)/matrix%blockSize-matrix%blockOffset+1)%set( &
              i-offset,j-offset,setval)
          flag=.TRUE.
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  IF(.NOT. flag) THEN
    WRITE(msg,'(I2,A,I9,A,I9)') rank," at ",i,',',j
    CALL eMatrixType%raiseWarning('Invalid matrix set in '// &
        modName//'::'//myName//' for rank '//msg)
  ENDIF
#endif
ENDSUBROUTINE set_DistributedBlockBandedMatrixType

!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the distributed banded matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
SUBROUTINE setrow_DistributedBandedMatrixType(matrix,i,j,setval)
  CHARACTER(LEN=*),PARAMETER :: myName='setrow_DistributedBandedMatrixType'
  CLASS(DistributedBandedMatrixType),INTENT(INOUT) :: matrix
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK),INTENT(IN) :: j(:)
  REAL(SRK),INTENT(IN) :: setval(:)
  CALL eMatrixType%raiseFatalError(modName//'::'//myName// &
      ' - routine is not implemented!')
ENDSUBROUTINE setrow_DistributedBandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the banded matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
SUBROUTINE set_BandedMatrixType(matrix,i,j,setval)
  CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK),INTENT(IN) :: j
  REAL(SRK),INTENT(IN) :: setval
  INTEGER(SIK) :: bandLoc, elemIdx

  REQUIRE(matrix%isInit)
  IF((j <= matrix%m) .AND. (i <= matrix%n) .AND. (i >= 1) .AND. (j >= 1)) THEN
    IF(.NOT. matrix%isAssembled) THEN
      ! If it is not assembled, add to tmp variables
      REQUIRE(matrix%counter < matrix%nnz)
      matrix%counter=matrix%counter+1
      matrix%iTmp(matrix%counter)=i
      matrix%jTmp(matrix%counter)=j
      matrix%elemTmp(matrix%counter)=setval
    ELSE
      ! Otherwise, perform binary search to set in existing structure
      CALL binarySearch_BandedMatrixType(matrix,i,j,bandLoc,elemIdx)
      IF(bandLoc >= 0 .AND. elemIdx >= 0) THEN
        matrix%bands(bandLoc)%elem(elemIdx)=setVal
      ENDIF
    ENDIF
  ENDIF
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
!> @brief Gets the values in the distributed banded matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
SUBROUTINE get_DistributedBandedMatrixType(matrix,i,j,getval)
  CLASS(DistributedBandedMatrixType),INTENT(INOUT) :: matrix
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK),INTENT(IN) :: j
  REAL(SRK),INTENT(INOUT) :: getval
#ifdef HAVE_MPI
  INTEGER(SIK) :: k,mpierr,rank

  REQUIRE(matrix%isInit)
  REQUIRE(matrix%isAssembled)
  getval=0.0_SRK
  CALL MPI_Comm_rank(matrix%comm,rank,mpierr)
  IF(j > matrix%jOffsets(rank+1) .AND. j <= matrix%jOffsets(rank+2)) THEN
    DO k=1,SIZE(matrix%jOffsets)-1
      IF(i > matrix%iOffsets(k) .AND. i <= matrix%iOffsets(k+1)) THEN
        IF(matrix%chunks(k)%isInit) THEN
          CALL matrix%chunks(k)%get(i-matrix%iOffsets(k), &
              j-matrix%jOffsets(rank+1),getval)
          EXIT
        ENDIF
      ENDIF
    ENDDO
  ENDIF
#endif
ENDSUBROUTINE get_DistributedBandedMatrixType

!
!-------------------------------------------------------------------------------
!> @brief Gets the values in the distributed block banded matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
SUBROUTINE get_DistributedBlockBandedMatrixType(matrix,i,j,getval)
  CLASS(DistributedBlockBandedMatrixType),INTENT(INOUT) :: matrix
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK),INTENT(IN) :: j
  REAL(SRK),INTENT(INOUT) :: getval
#ifdef HAVE_MPI
  INTEGER(SIK) :: rank,mpierr,offset

  REQUIRE(matrix%isInit)
  REQUIRE(matrix%isAssembled)
  CALL MPI_Comm_Rank(matrix%comm,rank,mpierr)
  IF((i-1)/matrix%blockSize /= (j-1)/matrix%blockSize) THEN
    CALL matrix%DistributedBandedMatrixType%get(i,j,getVal)
  ELSEIF(.NOT. matrix%blockMask) THEN
    offset=((i-1)/matrix%blockSize)*matrix%blockSize
    IF(j > matrix%jOffsets(rank+1) .AND. j <= matrix%jOffsets(rank+2)) THEN
      getval=matrix%blocks((i-1)/matrix%blockSize-matrix%blockOffset+1)%A( &
          i-offset,j-offset)
    ENDIF
  ELSE
    getval=0.0_SRK
  ENDIF
#endif
ENDSUBROUTINE get_DistributedBlockBandedMatrixType

!
!-------------------------------------------------------------------------------
!> @brief Gets the values in the banded matrix
!> @param matrix the matrix type to act on
!> @param i the ith location in the matrix
!> @param j the jth location in the matrix
!> @param setval the value to be set
!>
SUBROUTINE get_BandedMatrixType(matrix,i,j,getval)
  CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK),INTENT(IN) :: j
  REAL(SRK),INTENT(INOUT) :: getval
  INTEGER(SIK) :: bandLoc,elemIdx

  REQUIRE(matrix%isInit)
  REQUIRE(matrix%isAssembled)
  CALL binarySearch_BandedMatrixType(matrix,i,j,bandLoc,elemIdx)
  getval=0.0_SRK
  IF(bandLoc >= 0 .AND. elemIdx >= 0) THEN
    getval=matrix%bands(bandLoc)%elem(elemIdx)
  ENDIF

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
SUBROUTINE transpose_DistributedBandedMatrixType(matrix)
  CHARACTER(LEN=*),PARAMETER :: myName='transpose_DistributedBandedMatrixType'
  CLASS(DistributedBandedMatrixType),INTENT(INOUT) :: matrix

  CALL eMatrixType%raiseFatalError(modName//'::'//myName// &
      ' - routine is not implemented!')
ENDSUBROUTINE transpose_DistributedBandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief tranpose the matrix
!> @param matrix declare the matrix type to act on
!>
!>
SUBROUTINE transpose_BandedMatrixType(matrix)
  CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
  INTEGER(SIK) :: mTmp,i

  REQUIRE(matrix%isInit)
  REQUIRE(matrix%isAssembled)

  mTmp=matrix%n
  matrix%n=matrix%m
  matrix%m=mTmp

  DO i=1,SIZE(matrix%bandIdx)
    matrix%bands(i)%jIdx=matrix%bands(i)%jIdx-matrix%bandIdx(i)
  ENDDO
  matrix%bandIdx=-matrix%bandIdx
  matrix%isReversed=.NOT.matrix%isReversed

ENDSUBROUTINE transpose_BandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief tranpose the matrix
!> @param matrix declare the matrix type to act on
!>
!>
SUBROUTINE transpose_SparseMatrixType(matrix)
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
SUBROUTINE zeroentries_DistributedBandedMatrixType(matrix)
  CLASS(DistributedBandedMatrixType),INTENT(INOUT) :: matrix
  INTEGER(SIK) :: i

  REQUIRE(matrix%isInit)
  REQUIRE(matrix%isAssembled)
  DO i=1,SIZE(matrix%iOffsets)-1
    IF(matrix%chunks(i)%isInit) THEN
      CALL matrix%chunks(i)%zeroentries()
    ENDIF
  ENDDO

ENDSUBROUTINE zeroentries_DistributedBandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief zero the matrix
!> @param matrix declare the matrix type to act on
!>
!>
SUBROUTINE  zeroentries_DistributedBlockBandedMatrixType(matrix)
  CLASS(DistributedBlockBandedMatrixType),INTENT(INOUT) :: matrix
  INTEGER(SIK) :: i

  REQUIRE(matrix%isInit)
  REQUIRE(matrix%isAssembled)
  DO i=1,SIZE(matrix%blocks)
    CALL matrix%blocks(i)%zeroEntries()
  ENDDO
  CALL matrix%DistributedBandedMatrixType%zeroEntries()

ENDSUBROUTINE zeroentries_DistributedBlockBandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief zero the matrix
!> @param matrix declare the matrix type to act on
!>
!>
SUBROUTINE  zeroentries_BandedMatrixType(matrix)
  CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
  INTEGER(SIK) :: i

  REQUIRE(matrix%isInit)
  REQUIRE(matrix%isAssembled)
  DO i=1,SIZE(matrix%bandIdx)
    IF(ALLOCATED(matrix%bands(i)%elem)) matrix%bands(i)%elem=0.0_SRK
  ENDDO

ENDSUBROUTINE zeroentries_BandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief zero the matrix
!> @param matrix declare the matrix type to act on
!>
!>
SUBROUTINE  zeroentries_DenseRectMatrixType(matrix)
  CLASS(DenseRectMatrixType),INTENT(INOUT) :: matrix
  REQUIRE(matrix%isInit)
  matrix%a=0.0_SRK
ENDSUBROUTINE zeroentries_DenseRectMatrixType
!
!-------------------------------------------------------------------------------
!> @brief Set the block mask for distribed block-banded matrix
!> setting to True will make all values in the block appear to be zero
!> without actually setting them. Setting to false restores these values.
!> @param matrix declare the matrix type to act on
!>
!>
SUBROUTINE setBlockMask_DistributedBlockBandedMatrixType(matrix,maskVal)
  CLASS(DistributedBlockBandedMatrixType),INTENT(INOUT) :: matrix
  LOGICAL(SBK),INTENT(IN) :: maskVal

  REQUIRE(matrix%isInit)
  matrix%blockMask=maskVal
ENDSUBROUTINE setBlockMask_DistributedBlockBandedMatrixType
!
!-------------------------------------------------------------------------------
!> @brief performs binary search for (i,j) in the matrix
!> @param matrix declare the matrix type to act on
!> @param i the desired row index
!> @param j the desired col index
!> @param bandLoc the index of the band (i,j) is located in (negative -> not there)
!> @param elemIdx the index of the elem(:) array (i,j) is located at (negative -> not there)
!>
SUBROUTINE binarySearch_BandedMatrixType(matrix, i, j, bandLoc, elemIdx)
  CLASS(BandedMatrixType),INTENT(INOUT) :: matrix
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK),INTENT(IN) :: j
  INTEGER(SIK),INTENT(OUT) :: bandLoc
  INTEGER(SIK),INTENT(OUT) :: elemIdx
  INTEGER(SIK) :: bIdx,lo,hi,mid
  LOGICAL(SBK) :: found

  REQUIRE(matrix%isInit)
  REQUIRE(matrix%isAssembled)
  REQUIRE((j <= matrix%m) .AND. (i <= matrix%n) .AND. (i>=1) .AND. (j >= 1))

  found=.FALSE.
  !Check if band is contained (binary search)
  bIdx=j-i
  IF(matrix%isReversed) THEN
    bIdx=-bIdx
    matrix%bandIdx=-matrix%bandIdx
  ENDIF

  lo=1
  hi=SIZE(matrix%bandIdx)
  IF(bIdx < matrix%bandIdx(1) .OR. bIdx > matrix%bandIdx(hi)) THEN
    bandloc=-1
  ELSE
    DO WHILE (lo <= hi .AND. .NOT. found)
      mid=(hi+lo)/2
      IF(bIdx < matrix%bandIdx(mid)) THEN
        hi=mid-1
      ELSE IF(bIdx > matrix%bandIdx(mid)) THEN
        lo=mid+1
      ELSE
        found=.TRUE.
        EXIT
      ENDIF
    ENDDO
    IF(found) THEN
      bandLoc=mid
      lo=1
      hi=SIZE(matrix%bands(bandLoc)%elem)
      found=.FALSE.
      IF(j < matrix%bands(bandLoc)%jIdx(lo) .OR. &
          j > matrix%bands(bandLoc)%jIdx(hi)) THEN
        elemIdx=-1
      ELSE
        DO WHILE (lo <= hi .AND. .NOT. found)
          mid=(hi+lo)/2
          IF(j < matrix%bands(bandLoc)%jIdx(mid)) THEN
            hi=mid-1
          ELSE IF(j > matrix%bands(bandLoc)%jIdx(mid)) THEN
            lo=mid+1
          ELSE
            found=.TRUE.
            EXIT
          ENDIF
        ENDDO
      ENDIF
      elemIdx=-1
      IF(found) THEN
        elemIdx=mid
      ENDIF
    ELSE
      bandLoc=-1
    ENDIF
  ENDIF
  IF(matrix%isReversed) THEN
    matrix%bandIdx=-matrix%bandIdx
  ENDIF
ENDSUBROUTINE binarySearch_BandedMatrixType

ENDMODULE MatrixTypes_Native
