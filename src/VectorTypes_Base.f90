!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief The base abstraction for the VectorType.
!>
!> The types of vectors defined in this module include a real vector type,
!> and PETSc vector type.
!>
!> The objects are initialized with a parameter list. For valid reference lists
!> see @ref VectorTypes::VectorTypes_Declare_ValidParams
!> "VectorTypes_Declare_ValidParams".
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>
!> TIBWSFB: This documentation does not match the actual interface
!>
!> @par EXAMPLES
!> @code
!> PROGRAM ExampleVector
!>   TYPE(RealVectorType) :: vector
!>
!>   CALL vector%init(36)
!>   CALL vector%set(1,10._SRK)
!>   value=vector%get(1)
!>   CALL vector%clear()
!> ENDPROGRAM ExampleVector
!> @endcode
!>
!> @author Shane Stimpson
!>   @date 08/20/2012
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE VectorTypes_Base
  USE IntrType
  USE ParameterLists
  USE ExceptionHandler

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: eVectorType
  PUBLIC :: VectorType
  PUBLIC :: DistributedVectorType
  PUBLIC :: RealVectorType_reqParams,RealVectorType_optParams
  PUBLIC :: DistributedVectorType_reqParams,DistributedVectorType_optParams
  PUBLIC :: NativeDistributedVectorType_reqParams,NativeDistributedVectorType_optParams
  PUBLIC :: VectorType_Paramsflag
  INTEGER(SIK),PARAMETER,PUBLIC :: REAL_NATIVE=0,DISTRIBUTED_NATIVE=1
  PUBLIC :: VectorType_Declare_ValidParams
  PUBLIC :: VectorType_Clear_ValidParams


!> @brief the base vector type
  TYPE,ABSTRACT :: VectorType
    !> Initialization status
    LOGICAL(SBK) :: isInit=.FALSE.
    !> Number of values in the vector
    INTEGER(SIK) :: n=0
!
!List of Type Bound Procedures
    CONTAINS
      !> Deferred routine for clearing the vector
      PROCEDURE(vector_clear_absintfc),DEFERRED,PASS :: clear
      !> Deferred routine for initializing the vector
      PROCEDURE(vector_init_absintfc),DEFERRED,PASS :: init
      !> Deferred routine for setting one vector value
      PROCEDURE(vector_init_setOne_absintfc),DEFERRED,PASS :: setOne
      !> Deferred routine for setting all vector values with scalar
      PROCEDURE(vector_setAll_scalar_absintfc),DEFERRED,PASS :: setAll_scalar
      !> Deferred routine for setting all vector values with array
      PROCEDURE(vector_setAll_array_absintfc),DEFERRED,PASS :: setAll_array
      !> Deferred routine for setting selected vector values with array
      PROCEDURE(vector_setSelected_absintfc),DEFERRED,PASS :: setSelected
      !> Deferred routine for setting all vector values with scalar
      PROCEDURE(vector_setRange_scalar_absintfc),DEFERRED,PASS :: setRange_scalar
      !> Deferred routine for setting all vector values with array
      PROCEDURE(vector_setRange_array_absintfc),DEFERRED,PASS :: setRange_array
      GENERIC :: set => setOne,setAll_scalar,setAll_array,setRange_scalar,setRange_array,setSelected
      !> Deferred routine for getting vector value
      PROCEDURE(vector_getOne_absintfc),DEFERRED,PASS :: getOne
      !> Deferred routine for getting all vector values
      PROCEDURE(vector_getAll_absintfc),DEFERRED,PASS :: getAll
      !> Deferred routine for getting all vector values, entire vector at once
      PROCEDURE(vector_getSelected_absintfc),DEFERRED,PASS :: getSelected
      !> Deferred routine for getting a range of vector values
      PROCEDURE(vector_getRange_absintfc),DEFERRED,PASS :: getRange
      GENERIC :: get => getOne,getAll,getSelected,getRange
  ENDTYPE VectorType
!
!List of Abstract Interfaces
  !> Explicitly defines the interface for the clear routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE vector_clear_absintfc(thisVector)
      IMPORT :: VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
    ENDSUBROUTINE vector_clear_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the init routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE vector_init_absintfc(thisVector,Params)
      IMPORT :: SIK,ParamType,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      TYPE(ParamType),INTENT(IN) :: Params
    ENDSUBROUTINE vector_init_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the set one routine of all vector
  !> types
  ABSTRACT INTERFACE
    SUBROUTINE vector_init_setOne_absintfc(thisVector,i,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_init_setOne_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the set all (scalar) routine of all
  !> vector types
  ABSTRACT INTERFACE
    SUBROUTINE vector_setAll_scalar_absintfc(thisVector,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_setAll_scalar_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the set selected (array) routine of all
  !> vector types
  ABSTRACT INTERFACE
    SUBROUTINE vector_setSelected_absintfc(thisVector,indices,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: indices(:)
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_setSelected_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the set all (array) routine of all
  !> vector types
  ABSTRACT INTERFACE
    SUBROUTINE vector_setAll_array_absintfc(thisVector,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_setAll_array_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the set range (scalar) routine of all
  !> vector types
  ABSTRACT INTERFACE
    SUBROUTINE vector_setRange_scalar_absintfc(thisVector,istt,istp,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_setRange_scalar_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the set range (array) routine of all
  !> vector types
  ABSTRACT INTERFACE
    SUBROUTINE vector_setRange_array_absintfc(thisVector,istt,istp,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_setRange_array_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the get (scalar) routine of all vector
  !> types
  ABSTRACT INTERFACE
    SUBROUTINE vector_getOne_absintfc(thisVector,i,getval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(OUT) :: getval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_getOne_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the get (scalar) routine of all vector
  !> types
  ABSTRACT INTERFACE
    SUBROUTINE vector_getAll_absintfc(thisVector,getval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(OUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_getAll_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the get (vector) routine of all vector
  !> types
  ABSTRACT INTERFACE
    SUBROUTINE vector_getSelected_absintfc(thisVector,indices,getval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN):: indices(:)
      REAL(SRK),INTENT(OUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_getSelected_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the get (scalar) routine of all vector
  !> types
  ABSTRACT INTERFACE
    SUBROUTINE vector_getRange_absintfc(thisVector,istt,istp,getval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      REAL(SRK),INTENT(OUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_getRange_absintfc
  ENDINTERFACE

  !> @brief The extended type for distributed vector
  !>
  !> This defines a common class for PETSc and Trilinos to inherit from that adds
  !> a minimal set of capability from the base vector type, mainly a concept of
  !> parallel environment and the assemble method.
  TYPE,ABSTRACT,EXTENDS(VectorType) :: DistributedVectorType
    !> creation status
    LOGICAL(SBK) :: isCreated=.FALSE.
    !> assembly status
    LOGICAL(SBK) :: isAssembled=.FALSE.
    !> MPI comm ID
    INTEGER(SIK) :: comm=-1
    !> total number of local values
    INTEGER(SIK) :: nlocal=-1
!
!List of Type Bound Procedures
    CONTAINS
      !> Deferred routine for assembling a vector
      PROCEDURE(distvector_assemble_absintfc),DEFERRED,PASS :: assemble
  ENDTYPE DistributedVectorType

  !> Explicitly defines the interface for assemblying a distributed vector type
  ABSTRACT INTERFACE
    SUBROUTINE distvector_assemble_absintfc(thisVector,ierr)
      IMPORT :: SIK,DistributedVectorType
      CLASS(DistributedVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE distvector_assemble_absintfc
  ENDINTERFACE

  !> Logical flag to check whether the required and optional parameter lists
  !> have been created yet for the Vector Types.
  LOGICAL(SBK),SAVE :: VectorType_Paramsflag=.FALSE.

  !> The parameter lists to use when validating a parameter list for
  !> initialization for the Real Vector Type.
  TYPE(ParamType),PROTECTED,SAVE :: RealVectorType_reqParams,RealVectorType_optParams,NativeDistributedVectorType_reqParams,NativeDistributedVectorType_optParams

  !> The parameter lists to use when validating a parameter list for
  !> initialization for the Distributed Vector Type.
  TYPE(ParamType),PROTECTED,SAVE :: DistributedVectorType_reqParams,DistributedVectorType_optParams

  !> Exception Handler for use in VectorTypes
  TYPE(ExceptionHandlerType),SAVE :: eVectorType
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Subroutine that sets up the default parameter lists for the
!>        VectorTypes, both Real and Distributed.
!> The required parameters for the Real Vector Type are:
!>        'VectorType->n',SIK
!> The optional parameters for the Real Vector Type do not exist.
!> The required parameters for the Distributed Vector Type are:
!>        'VectorType->n',SIK
!>        'VectorType->MPI_Comm_ID',SIK
!> The optional parameters for the Distributed Vector Type do not exist.
!>
    SUBROUTINE VectorType_Declare_ValidParams()
      INTEGER(SIK) :: n,MPI_Comm,nlocal,chunkSize

      !Setup the required and optional parameter lists
      n=1
      chunkSize =1
      MPI_Comm=1
      nlocal=-1
      !Real Vector Type - Required
      CALL RealVectorType_reqParams%add('VectorType->n',n)

      !Distributed Vector Type - Required
      CALL DistributedVectorType_reqParams%add('VectorType->n',n)
      CALL DistributedVectorType_reqParams%add('VectorType->MPI_Comm_ID',MPI_Comm)

      !There are no optional parameters at this time.
      CALL DistributedVectorType_optParams%add('VectorType->nlocal',nlocal)

      !Native Distributed vector - Required
      CALL NativeDistributedVectorType_reqParams%add('VectorType->n',n)
      CALL NativeDistributedVectorType_reqParams%add('VectorType->MPI_Comm_ID',MPI_Comm)

      CALL NativeDistributedVectorType_optParams%add('VectorType->chunkSize',chunkSize)
      CALL NativeDistributedVectorType_optParams%add('VectorType->nlocal',nlocal)

      !Set flag to true since the defaults have been set for this type.
      VectorType_Paramsflag=.TRUE.
    ENDSUBROUTINE VectorType_Declare_ValidParams
!
!-------------------------------------------------------------------------------
!> @brief Subroutine that clears the default parameter lists for the
!>        VectorTypes, both Real and Distributed.
!>
    SUBROUTINE VectorType_Clear_ValidParams()

      !Set flag to false since the defaults have been cleared for this type.
      VectorType_Paramsflag=.FALSE.

      !Real Vector Type - Required
      CALL RealVectorType_reqParams%clear()

      !Distributed Vector Type - Required
      CALL DistributedVectorType_reqParams%clear()
      !There are no optional parameters at this time.
      CALL DistributedVectorType_optParams%clear()

      CALL NativeDistributedVectorType_reqParams%clear()
      CALL NativeDistributedVectorType_optParams%clear()

    ENDSUBROUTINE VectorType_Clear_ValidParams


ENDMODULE
