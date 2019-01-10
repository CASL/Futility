!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Base abstraction for the MatrixTypes
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>
!> @author Adam Nelson and Brendan Kochunas
!>   @date 02/14/2012
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE MatrixTypes_Base
  USE IntrType
  USE ParameterLists
  USE ExceptionHandler

  IMPLICIT NONE
  PRIVATE

!
! List of public members
  PUBLIC :: eMatrixType
  PUBLIC :: MatrixType
  PUBLIC :: SquareMatrixType
  PUBLIC :: RectMatrixType
  PUBLIC :: DistributedMatrixType

  PUBLIC :: MatrixType_Paramsflag
  !> set enumeration scheme for matrix types
  INTEGER(SIK),PARAMETER,PUBLIC :: SPARSE=0,TRIDIAG=1,DENSESQUARE=2,DENSERECT=3
  INTEGER(SIK),PARAMETER,PUBLIC :: BANDED=4
  PUBLIC :: SparseMatrixType_reqParams,SparseMatrixType_optParams
  PUBLIC :: TriDiagMatrixType_reqParams,TriDiagMatrixType_optParams
  PUBLIC :: BandedMatrixType_reqParams,BandedMatrixType_optParams
  PUBLIC :: DistributedBandedMatrixType_reqParams, &
    DistributedBandedMatrixType_optParams
  PUBLIC :: DenseRectMatrixType_reqParams,DenseRectMatrixType_optParams
  PUBLIC :: DenseSquareMatrixType_reqParams,DenseSquareMatrixType_optParams
  PUBLIC :: DistributedMatrixType_reqParams,DistributedMatrixType_optParams
  PUBLIC :: MatrixTypes_Declare_ValidParams
  PUBLIC :: MatrixTypes_Clear_ValidParams

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
      PROCEDURE(matrix_sub_absintfc),DEFERRED,PASS :: clear
      !> Deferred routine for zeroing all the entries of the matrix
      PROCEDURE(matrix_sub_absintfc),DEFERRED,PASS :: zeroentries
      !> Deferred routine for initializing the matrix
      PROCEDURE(matrix_init_param_sub_absintfc),DEFERRED,PASS :: init
      !> Deferred routine for setting matrix values
      PROCEDURE(matrix_set_sub_absintfc),DEFERRED,PASS :: set
      !> Deferred routine for getting a matrix value
      PROCEDURE(matrix_get_sub_absintfc),DEFERRED,PASS :: get
      !> Deferred routine for getting a matrix value
      PROCEDURE(matrix_transpose_sub_absintfc),DEFERRED,PASS :: transpose
  ENDTYPE MatrixType
!
!List of Abstract Interfaces
  !> Explicitly defines the interface for the clear routine of all matrix types
  ABSTRACT INTERFACE
    SUBROUTINE matrix_sub_absintfc(matrix)
      IMPORT :: MatrixType
      CLASS(MatrixType),INTENT(INOUT) :: matrix
    ENDSUBROUTINE matrix_sub_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the init routine of all matrix types
  !> with parameter list
  ABSTRACT INTERFACE
    SUBROUTINE matrix_init_param_sub_absintfc(matrix,Params)
      IMPORT :: MatrixType,ParamType
      CLASS(MatrixType),INTENT(INOUT) :: matrix
      CLASS(ParamType),INTENT(IN) :: Params
    ENDSUBROUTINE matrix_init_param_sub_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the set routine of all matrix types
  ABSTRACT INTERFACE
    SUBROUTINE matrix_set_sub_absintfc(matrix,i,j,setval)
      IMPORT :: SIK,SRK,MatrixType
      CLASS(MatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(IN) :: setval
    ENDSUBROUTINE matrix_set_sub_absintfc
  ENDINTERFACE


  !> Explicitly defines the interface for the get routine of all matrix types
  ABSTRACT INTERFACE
    SUBROUTINE matrix_get_sub_absintfc(matrix,i,j,getval)
      IMPORT :: SIK,SRK,MatrixType
      CLASS(MatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: j
      REAL(SRK),INTENT(INOUT) :: getval
    ENDSUBROUTINE matrix_get_sub_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the transpose routine of all matrix types
  ABSTRACT INTERFACE
    SUBROUTINE matrix_transpose_sub_absintfc(matrix)
      IMPORT :: MatrixType
      CLASS(MatrixType),INTENT(INOUT) :: matrix
    ENDSUBROUTINE matrix_transpose_sub_absintfc
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

  !> @brief The extended type for distributed matrices (e.g. PETSc, Trilinos)
  TYPE,ABSTRACT,EXTENDS(SquareMatrixType) :: DistributedMatrixType

    !> creation status
    LOGICAL(SBK) :: isCreated=.FALSE.
    !> assembly status
    LOGICAL(SBK) :: isAssembled=.FALSE.
    !> MPI comm ID
    INTEGER(SIK) :: comm=-1
    !> number of local values
    INTEGER(SIK) :: nlocal=0
!
!List of Type Bound Procedures
    CONTAINS
      !> Deferred routine for assembling a matrix
      PROCEDURE(distmatrix_assemble_absintfc),DEFERRED,PASS :: assemble
      !> Deferred routine for setting a row at a time
      PROCEDURE(distmatrix_setRow_absintfc),DEFERRED,PASS :: setRow
  ENDTYPE DistributedMatrixType

  !> Explicitly defines the interface for assembling a distributed matrix
  ABSTRACT INTERFACE
    SUBROUTINE distmatrix_assemble_absintfc(thisMatrix,ierr)
      IMPORT :: SIK,DistributedMatrixType
      CLASS(DistributedMatrixType),INTENT(INOUT) :: thisMatrix
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE distmatrix_assemble_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for setting row of a distributed matrix
  ABSTRACT INTERFACE
    SUBROUTINE distmatrix_setRow_absintfc(matrix,i,j,setval)
      IMPORT :: SIK,SRK,DistributedMatrixType
      CLASS(DistributedMatrixType),INTENT(INOUT) :: matrix
      INTEGER(SIK),INTENT(IN) :: i, j(:)
      REAL(SRK),INTENT(IN) :: setval(:)
    ENDSUBROUTINE distmatrix_setRow_absintfc
  ENDINTERFACE

  !> The parameter lists to use when validating a parameter list for
  !> initialization for a Sparse Matrix Type.
  TYPE(ParamType),PROTECTED,SAVE :: SparseMatrixType_reqParams, SparseMatrixType_optParams

  !> The parameter lists to use when validating a parameter list for
  !> initialization for a Tri-Diagonal Matrix Type.
  TYPE(ParamType),PROTECTED,SAVE :: TriDiagMatrixType_reqParams, TriDiagMatrixType_optParams

  !> The parameter lists to use when validating a parameter list for
  !> initialization for a Banded Matrix Type.
  TYPE(ParamType),PROTECTED,SAVE :: BandedMatrixType_reqParams, BandedMatrixType_optParams

  !> The parameter lists to use when validating a parameter list for
  !> initialization for a Dense Rectangular Matrix Type.
  TYPE(ParamType),PROTECTED,SAVE :: DenseRectMatrixType_reqParams, DenseRectMatrixType_optParams

  !> The parameter lists to use when validating a parameter list for
  !> initialization for a Dense Square Matrix Type.
  TYPE(ParamType),PROTECTED,SAVE :: DenseSquareMatrixType_reqParams, DenseSquareMatrixType_optParams

  !> initialization for a Distributed Matrix Type.
  TYPE(ParamType),PROTECTED,SAVE :: DistributedMatrixType_reqParams, DistributedMatrixType_optParams

!> The parameter lists to use when validating a parameter list for
  !> initialization for a Banded Matrix Type.
  TYPE(ParamType),PROTECTED,SAVE :: DistributedBandedMatrixType_reqParams, DistributedBandedMatrixType_optParams

  !> Logical flag to check whether the required and optional parameter lists
  !> have been created yet for the Matrix Types.
  LOGICAL(SBK),SAVE :: MatrixType_Paramsflag=.FALSE.

  !> Exception Handler for use in MatrixTypes
  TYPE(ExceptionHandlerType),SAVE :: eMatrixType

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='MATRIXTYPES_BASE'

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Subroutine that sets up the default parameter lists for the all
!>        MatrixTypes including Sparse, Tri-Diagonal, Dense Rectangular, Dense
!>        Square, and PETSc.
!> The required parameters for the Sparse Matrix Type are:
!>        'MatrixType->n',SIK
!>        'MatrixType->nnz',SIK
!> The optional parameters for the Sparse Matrix Type do not exist.
!> The required parameters for the Tri-Diagonal Matrix Type are:
!>        'MatrixType->n',SIK
!>        'MatrixType->isSym',SBK
!> The optional parameters for the Tri-Diagonal Matrix Type do not exist.
!> The required parameters for the Dense Retangular Matrix Type are:
!>        'MatrixType->n',SIK
!>        'MatrixType->m',SIK
!> The optional parameters for the Dense Retangular Matrix Type do not exist.
!> The required parameters for the Dense Square Matrix Type are:
!>        'MatrixType->n',SIK
!>        'MatrixType->isSym',SBK
!> The optional parameters for the Dense Square Matrix Type do not exist.
!> The required parameters for the PETSc Matrix Type are:
!>        'MatrixType->n',SIK
!>        'MatrixType->isSym',SBK
!>        'MatrixType->matType',SIK
!>        'MatrixType->MPI_COMM_ID',SIK
!> The optional parameters for the PETSc Matrix Type do not exist.
!>
    SUBROUTINE MatrixTypes_Declare_ValidParams()
      INTEGER(SIK) :: n,m,nnz,dnnz(1),onnz(1),matType,MPI_COMM_ID,nlocal,nband
      INTEGER(SIK) :: bandi(1), bandj(1), bandl(1)
      LOGICAL(SBK) :: isSym

      !Setup the required and optional parameter lists
      n=1
      m=1
      nnz=1
      dnnz=-1
      onnz=-1
      isSym=.FALSE.
      matType=1
      MPI_COMM_ID=1
      nlocal=-1
      nband=1
      bandi(1)=1
      bandj(1)=1
      bandl(1)=1
      !Sparse Matrix Type - Required
      CALL SparseMatrixType_reqParams%add('MatrixType->n',n)
      CALL SparseMatrixType_reqParams%add('MatrixType->nnz',nnz)
      !Tri-Diagonal Matrix Type - Required
      CALL TriDiagMatrixType_reqParams%add('MatrixType->n',n)
      CALL TriDiagMatrixType_reqParams%add('MatrixType->isSym',isSym)
      !Banded Matrix Type - Required
      CALL BandedMatrixType_reqParams%add('MatrixType->n',n)
      CALL BandedMatrixType_reqParams%add('MatrixType->m',m)
      CALL BandedMatrixType_reqParams%add('MatrixType->nband',nband)
      CALL BandedMatrixType_reqParams%add('bandi',bandi)
      CALL BandedMatrixType_reqParams%add('bandj',bandj)
      CALL BandedMatrixType_reqParams%add('bandl',bandl)
      !Dense Rectangular Matrix Type - Required
      CALL DenseRectMatrixType_reqParams%add('MatrixType->n',n)
      CALL DenseRectMatrixType_reqParams%add('MatrixType->m',m)
      !Dense Square Matrix Type - Required
      CALL DenseSquareMatrixType_reqParams%add('MatrixType->n',n)
      CALL DenseSquareMatrixType_reqParams%add('MatrixType->isSym',isSym)
      !Distributed Matrix Type - Required
      CALL DistributedMatrixType_reqParams%add('MatrixType->n',n)
      CALL DistributedMatrixType_reqParams%add('MatrixType->isSym',isSym)
      CALL DistributedMatrixType_reqParams%add('MatrixType->matType',matType)
      CALL DistributedMatrixType_reqParams%add('MatrixType->MPI_COMM_ID',MPI_COMM_ID)
      !Distributed Banded Matrix Type - Required
      CALL DistributedBandedMatrixType_reqParams%add('MatrixType->n',n)
      CALL DistributedBandedMatrixType_reqParams%add('MatrixType->m',m)
      CALL DistributedBandedMatrixType_reqParams%add('MatrixType->isSym',isSym)
      CALL DistributedBandedMatrixType_reqParams%add('MatrixType->matType',matType)
      CALL DistributedBandedMatrixType_reqParams%add('MatrixType->MPI_COMM_ID',MPI_COMM_ID)
      CALL DistributedBandedMatrixType_reqParams%add('MatrixType->nband',nband)
      CALL DistributedBandedMatrixType_reqParams%add('bandi',bandi)
      CALL DistributedBandedMatrixType_reqParams%add('bandj',bandj)
      CALL DistributedBandedMatrixType_reqParams%add('bandl',bandl)


      !There are no optional parameters at this time.
      CALL DistributedMatrixType_optParams%add('MatrixType->nlocal',nlocal)
      CALL DistributedMatrixType_optParams%add('MatrixType->dnnz',dnnz)
      CALL DistributedMatrixType_optParams%add('MatrixType->onnz',onnz)

      !Set flag to true since the defaults have been set for this type.
      MatrixType_Paramsflag=.TRUE.
    ENDSUBROUTINE MatrixTypes_Declare_ValidParams
!
!-------------------------------------------------------------------------------
!> @brief Subroutine that clears the default parameter lists for the all
!>        MatrixTypes including Sparse, Tri-Diagonal, Dense Rectangular, Dense
!>        Square, and PETSc.
!>
    SUBROUTINE MatrixTypes_Clear_ValidParams()

      !Set flag to true since the defaults have been set for this type.
      MatrixType_Paramsflag=.FALSE.

      !Sparse Matrix Type
      CALL SparseMatrixType_reqParams%clear()
      !Tri-Diagonal Matrix Type
      CALL TriDiagMatrixType_reqParams%clear()
      !Banded Matrix Type
      CALL BandedMatrixType_reqParams%clear()      
      !Dense Rectangular Matrix Type
      CALL DenseRectMatrixType_reqParams%clear()
      !Dense Square Matrix Type
      CALL DenseSquareMatrixType_reqParams%clear()
      !Distributed Matrix Type
      CALL DistributedMatrixType_reqParams%clear()

      !There are no optional parameters at this time.
      CALL DistributedMatrixType_optParams%clear()
    ENDSUBROUTINE MatrixTypes_Clear_ValidParams

ENDMODULE MatrixTypes_Base
