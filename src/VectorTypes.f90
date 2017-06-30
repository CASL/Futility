!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module for defining vector types.
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
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref Allocs "Allocs": @copybrief Allocs
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
MODULE VectorTypes
  USE IntrType
  USE ExceptionHandler
  USE Allocs
  USE ParameterLists
  USE BLAS1,           ONLY: BLAS1_asum  => BLAS_asum,  &
                             BLAS1_axpy  => BLAS_axpy,  &
                             BLAS1_copy  => BLAS_copy,  &
                             BLAS1_dot   => BLAS_dot,   &
                             BLAS1_iamax => BLAS_iamax, &
                             BLAS1_iamin => BLAS_iamin, &
                             BLAS1_nrm2  => BLAS_nrm2,  &
                             BLAS1_scal  => BLAS_scal,  &
                             BLAS1_swap  => BLAS_swap
  USE trilinos_interfaces
  IMPLICIT NONE

#ifdef FUTILITY_HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
#endif

  PRIVATE
!
! List of public members
  PUBLIC :: eVectorType
  PUBLIC :: VectorType
  PUBLIC :: DistributedVectorType
  PUBLIC :: RealVectorType
  PUBLIC :: PETScVectorType
  PUBLIC :: TrilinosVectorType
  PUBLIC :: BLAS_asum
  PUBLIC :: BLAS_axpy
  PUBLIC :: BLAS_copy
  PUBLIC :: BLAS_dot
  PUBLIC :: BLAS_iamax
  PUBLIC :: BLAS_iamin
  PUBLIC :: BLAS_nrm2
  PUBLIC :: BLAS_scal
  PUBLIC :: BLAS_swap
  PUBLIC :: RealVectorType_reqParams,RealVectorType_optParams
  PUBLIC :: DistributedVectorType_reqParams,DistributedVectorType_optParams
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
      !> Deferred routine for setting all vector values with scalar
      PROCEDURE(vector_setRange_scalar_absintfc),DEFERRED,PASS :: setRange_scalar
      !> Deferred routine for setting all vector values with array
      PROCEDURE(vector_setRange_array_absintfc),DEFERRED,PASS :: setRange_array
      GENERIC :: set => setOne,setAll_scalar,setAll_array,setRange_scalar,setRange_array
      !> Deferred routine for getting vector value
      PROCEDURE(vector_getOne_absintfc),DEFERRED,PASS :: getOne
      !> Deferred routine for getting all vector values
      PROCEDURE(vector_getAll_absintfc),DEFERRED,PASS :: getAll
      !> Deferred routine for getting a range of vector values
      PROCEDURE(vector_getRange_absintfc),DEFERRED,PASS :: getRange
      GENERIC :: get => getOne,getAll,getRange
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

  !> Explicitly defines the interface for the set one routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE vector_init_setOne_absintfc(thisVector,i,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_init_setOne_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the set all (scalar) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE vector_setAll_scalar_absintfc(thisVector,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_setAll_scalar_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the set all (array) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE vector_setAll_array_absintfc(thisVector,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_setAll_array_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the set range (scalar) routine of all vector types
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

  !> Explicitly defines the interface for the set range (array) routine of all vector types
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

  !> Explicitly defines the interface for the get (scalar) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE vector_getOne_absintfc(thisVector,i,getval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(INOUT) :: getval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_getOne_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the get (scalar) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE vector_getAll_absintfc(thisVector,getval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(INOUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE vector_getAll_absintfc
  ENDINTERFACE

  !> Explicitly defines the interface for the get (scalar) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE vector_getRange_absintfc(thisVector,istt,istp,getval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      REAL(SRK),INTENT(INOUT) :: getval(:)
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

  !> @brief The extended type for real vector
  !>
  !> This does not add a significant functionality over the intrinsic
  !> allocatable arrays that are part of the Fortran language. It
  !> is provided so as to be able to use the BLAS interfaces adapted
  !> for the vector type and it also gains some value by having the
  !> isSymmetric and isInit attributes.
  TYPE,EXTENDS(VectorType) :: RealVectorType
    !> The values of the vector
    REAL(SRK),ALLOCATABLE :: b(:)
!
!List of Type Bound Procedures
    CONTAINS
      !> @copybrief VectorTypes::clear_RealVectorType
      !> @copydetails VectorTypes::clear_RealVectorType
      PROCEDURE,PASS :: clear => clear_RealVectorType
      !> @copybrief VectorTypes::init_RealVectorType
      !> @copydetails VectorTypes::init_RealVectorType
      PROCEDURE,PASS :: init => init_RealVectorType
      !> @copybrief VectorTypes::setOne_RealVectorType
      !> @copydetails VectorTypes::setOne_RealVectorType
      PROCEDURE,PASS :: setOne => setOne_RealVectorType
      !> @copybrief VectorTypes::setAll_scalar_RealVectorType
      !> @copydetails VectorTypes::setAll_scalar_RealVectorType
      PROCEDURE,PASS :: setAll_scalar => setAll_scalar_RealVectorType
      !> @copybrief VectorTypes::setAll_array_RealVectorType
      !> @copydetails VectorTypes::setAll_array_RealVectorType
      PROCEDURE,PASS :: setAll_array => setAll_array_RealVectorType
      !> @copybrief VectorTypes::setRange_scalar_RealVectorType
      !> @copydetails VectorTypes::setRange_scalar_RealVectorType
      PROCEDURE,PASS :: setRange_scalar => setRange_scalar_RealVectorType
      !> @copybrief VectorTypes::setRange_array_RealVectorType
      !> @copydetails VectorTypes::setRange_array_RealVectorType
      PROCEDURE,PASS :: setRange_array => setRange_array_RealVectorType
      !> @copybrief VectorTypes::getOne_RealVectorType
      !> @copydetails VectorTypes::getOne_RealVectorType
      PROCEDURE,PASS :: getOne => getOne_RealVectorType
      !> @copybrief VectorTypes::getAll_RealVectorType
      !> @copydetails VectorTypes::getAll_RealVectorType
      PROCEDURE,PASS :: getAll => getAll_RealVectorType
      !> @copybrief VectorTypes::getRange_RealVectorType
      !> @copydetails VectorTypes::getRange_RealVectorType
      PROCEDURE,PASS :: getRange => getRange_RealVectorType
  ENDTYPE RealVectorType


  !> @brief The extended type for PETSc vectors
  TYPE,EXTENDS(DistributedVectorType) :: PETScVectorType
    !> The values of the vector
#ifdef FUTILITY_HAVE_PETSC
    Vec :: b
#endif
!
!List of Type Bound Procedures
    CONTAINS
      !> @copybrief VectorTypes::clear_PETScVectorType
      !> @copydetails VectorTypes::clear_PETScVectorType
      PROCEDURE,PASS :: clear => clear_PETScVectorType
      !> @copybrief VectorTypes::init_PETScVectorType
      !> @copydetails VectorTypes::init_PETScVectorType
      PROCEDURE,PASS :: init => init_PETScVectorType
      !> @copybrief VectorTypes::setOne_PETScVectorType
      !> @copydetails VectorTypes::setOne_PETScVectorType
      PROCEDURE,PASS :: setOne => setOne_PETScVectorType
      !> @copybrief VectorTypes::setAll_scalar_PETScVectorType
      !> @copydetails VectorTypes::setAll_scalar_PETScVectorType
      PROCEDURE,PASS :: setAll_scalar => setAll_scalar_PETScVectorType
      !> @copybrief VectorTypes::setAll_array_PETScVectorType
      !> @copydetails VectorTypes::setAll_array_PETScVectorType
      PROCEDURE,PASS :: setAll_array => setAll_array_PETScVectorType
      !> @copybrief VectorTypes::setRange_scalar_PETScVectorType
      !> @copydetails VectorTypes::setRange_scalar_PETScVectorType
      PROCEDURE,PASS :: setRange_scalar => setRange_scalar_PETScVectorType
      !> @copybrief VectorTypes::setRange_array_PETScVectorType
      !> @copydetails VectorTypes::setRange_array_PETScVectorType
      PROCEDURE,PASS :: setRange_array => setRange_array_PETScVectorType
      !> @copybrief VectorTypes::getOne_PETScVectorType
      !> @copydetails VectorTypes::getOne_PETScVectorType
      PROCEDURE,PASS :: getOne => getOne_PETScVectorType
      !> @copybrief VectorTypes::getAll_PETScVectorType
      !> @copydetails VectorTypes::getAll_PETScVectorType
      PROCEDURE,PASS :: getAll => getAll_PETScVectorType
      !> @copybrief VectorTypes::getRange_PETScVectorType
      !> @copydetails VectorTypes::getRange_PETScVectorType
      PROCEDURE,PASS :: getRange => getRange_PETScVectorType
      !> @copybrief VectorTypes::assemble_PETScVectorType
      !> @copydetails VectorTypes::assemble_PETScVectorType
      PROCEDURE,PASS :: assemble => assemble_PETScVectorType
  ENDTYPE PETScVectorType

  !> @brief The extended type for Trilinos vectors
  TYPE,EXTENDS(DistributedVectorType) :: TrilinosVectorType
    !> The values of the vector
    INTEGER(SIK) :: b
!
!List of Type Bound Procedures
    CONTAINS
      !> @copybrief VectorTypes::clear_TrilinosVectorType
      !> @copydetails VectorTypes::clear_TrilinosVectorType
      PROCEDURE,PASS :: clear => clear_TrilinosVectorType
      !> @copybrief VectorTypes::init_TrilinosVectorType
      !> @copydetails VectorTypes::init_TrilinosVectorType
      PROCEDURE,PASS :: init => init_TrilinosVectorType
      !> @copybrief VectorTypes::setOne_TrilinosVectorType
      !> @copydetails VectorTypes::setOne_TrilinosVectorType
      PROCEDURE,PASS :: setOne => setOne_TrilinosVectorType
      !> @copybrief VectorTypes::setAll_scalar_TrilinosVectorType
      !> @copydetails VectorTypes::setAll_scalar_TrilinosVectorType
      PROCEDURE,PASS :: setAll_scalar => setAll_scalar_TrilinosVectorType
      !> @copybrief VectorTypes::setAll_array_TrilinosVectorType
      !> @copydetails VectorTypes::setAll_array_TrilinosVectorType
      PROCEDURE,PASS :: setAll_array => setAll_array_TrilinosVectorType
      !> @copybrief VectorTypes::setRange_scalar_TrilinosVectorType
      !> @copydetails VectorTypes::setRange_scalar_TrilinosVectorType
      PROCEDURE,PASS :: setRange_scalar => setRange_scalar_TrilinosVectorType
      !> @copybrief VectorTypes::setRange_array_TrilinosVectorType
      !> @copydetails VectorTypes::setRange_array_TrilinosVectorType
      PROCEDURE,PASS :: setRange_array => setRange_array_TrilinosVectorType
      !> @copybrief VectorTypes::getOne_TrilinosVectorType
      !> @copydetails VectorTypes::getOne_TrilinosVectorType
      PROCEDURE,PASS :: getOne => getOne_TrilinosVectorType
      !> @copybrief VectorTypes::getAll_TrilinosVectorType
      !> @copydetails VectorTypes::getAll_TrilinosVectorType
      PROCEDURE,PASS :: getAll => getAll_TrilinosVectorType
      !> @copybrief VectorTypes::getRange_TrilinosVectorType
      !> @copydetails VectorTypes::getRange_TrilinosVectorType
      PROCEDURE,PASS :: getRange => getRange_TrilinosVectorType
      !> @copybrief VectorTypes::assemble_TrilinosVectorType
      !> @copydetails VectorTypes::assemble_TrilinosVectorType
      PROCEDURE,PASS :: assemble => assemble_TrilinosVectorType
  ENDTYPE TrilinosVectorType

  !> @brief Adds to the @ref BLAS1::BLAS_asum "BLAS_asum" interface so that
  !> the vector types defined in this module are also supported.
  INTERFACE BLAS_asum
    !> @copybrief VectorTypes::asum_VectorType
    !> @copydetails VectorTypes::asum_VectorType
    MODULE PROCEDURE asum_VectorType
  ENDINTERFACE BLAS_asum

  !> @brief Adds to the @ref BLAS1::BLAS_axpy "BLAS_axpy" interface so that
  !> the vector types defined in this module are also supported.
  INTERFACE BLAS_axpy
    !> @copybrief VectorTypes::axpy_scalar_VectorType
    !> @copydetails VectorTypes::axpy_scalar_VectorType
    MODULE PROCEDURE axpy_scalar_VectorType
    !> @copybrief VectorTypes::axpy_vector_VectorType
    !> @copydetails VectorTypes::axpy_vector_VectorType
    MODULE PROCEDURE axpy_vector_VectorType
  ENDINTERFACE BLAS_axpy

  !> @brief Adds to the @ref BLAS1::BLAS_copy "BLAS_copy" interface so that
  !> the vector types defined in this module are also supported.
  INTERFACE BLAS_copy
    !> @copybrief VectorTypes::copy_VectorType
    !> @copydetails VectorTypes::copy_VectorType
    MODULE PROCEDURE copy_VectorType
  ENDINTERFACE BLAS_copy

  !> @brief Adds to the @ref BLAS1::BLAS_dot "BLAS_dot" interface so that
  !> the vector types defined in this module are also supported.
  INTERFACE BLAS_dot
    !> @copybrief VectorTypes::dot_VectorType
    !> @copydetails VectorTypes::dot_VectorType
    MODULE PROCEDURE dot_VectorType
  ENDINTERFACE BLAS_dot

  !> @brief Adds to the @ref BLAS1::BLAS_iamax "BLAS_iamax" interface so that
  !> the vector types defined in this module are also supported.
  INTERFACE BLAS_iamax
    !> @copybrief VectorTypes::iamax_VectorType
    !> @copydetails VectorTypes::iamax_VectorType
    MODULE PROCEDURE iamax_VectorType
  ENDINTERFACE BLAS_iamax

  !> @brief Adds to the @ref BLAS1::BLAS_iamin "BLAS_iamin" interface so that
  !> the vector types defined in this module are also supported.
  INTERFACE BLAS_iamin
    !> @copybrief VectorTypes::iamin_VectorType
    !> @copydetails VectorTypes::iamin_VectorType
    MODULE PROCEDURE iamin_VectorType
  ENDINTERFACE BLAS_iamin

  !> @brief Adds to the @ref BLAS1::BLAS_nrm2 "BLAS_nrm2" interface so that
  !> the vector types defined in this module are also supported.
  INTERFACE BLAS_nrm2
    !> @copybrief VectorTypes::nrm2_VectorType
    !> @copydetails VectorTypes::nrm2_VectorType
    MODULE PROCEDURE nrm2_VectorType
  ENDINTERFACE BLAS_nrm2

  !> @brief Adds to the @ref BLAS1::BLAS_scal "BLAS_scal" interface so that
  !> the vector types defined in this module are also supported.
  INTERFACE BLAS_scal
    !> @copybrief VectorTypes::scal_scalar_VectorType
    !> @copydetails VectorTypes::scal_scalar_VectorType
    MODULE PROCEDURE scal_scalar_VectorType
    !> @copybrief VectorTypes::scal_vector_VectorType
    !> @copydetails VectorTypes::scal_vector_VectorType
    MODULE PROCEDURE scal_vector_VectorType
  ENDINTERFACE BLAS_scal

  !> @brief Adds to the @ref BLAS1::BLAS_swap "BLAS_swap" interface so that
  !> the vector types defined in this module are also supported.
  INTERFACE BLAS_swap
    !> @copybrief VectorTypes::swap_VectorType
    !> @copydetails VectorTypes::swap_VectorType
    MODULE PROCEDURE swap_VectorType
  ENDINTERFACE BLAS_swap

  !> Logical flag to check whether the required and optional parameter lists
  !> have been created yet for the Vector Types.
  LOGICAL(SBK),SAVE :: VectorType_Paramsflag=.FALSE.

  !> The parameter lists to use when validating a parameter list for
  !> initialization for the Real Vector Type.
  TYPE(ParamType),PROTECTED,SAVE :: RealVectorType_reqParams,RealVectorType_optParams

  !> The parameter lists to use when validating a parameter list for
  !> initialization for the PETSc Vector Type.
  TYPE(ParamType),PROTECTED,SAVE :: DistributedVectorType_reqParams,DistributedVectorType_optParams

  !> Exception Handler for use in VectorTypes
  TYPE(ExceptionHandlerType),SAVE :: eVectorType

#ifdef FUTILITY_HAVE_PETSC
  !> Scratch variable for petsc error code.
  !> It is an integer type.
  PetscErrorCode  :: iperr
#endif
  INTEGER(SIK) :: ierrc

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='VECTORTYPES'

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the real vector
!> @param declares the vector type to act on
!> @param pList the number of rows
!>
    SUBROUTINE init_RealVectorType(thisVector,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_RealVectorType'
      CLASS(RealVectorType),INTENT(INOUT) :: thisVector
      TYPE(ParamType),INTENT(IN) :: Params
      TYPE(ParamType) :: validParams
      INTEGER(SIK) :: n

      !Check to set up required and optional param lists.
      IF(.NOT.VectorType_Paramsflag) CALL VectorType_Declare_ValidParams()

      !Validate against the reqParams and OptParams
      validParams=Params
      CALL validParams%validate(RealVectorType_reqParams)

      !Pull Data from Parameter List
      CALL Params%get('VectorType->n',n)

      IF(.NOT. thisVector%isInit) THEN
        IF(n < 1) THEN
          CALL eVectorType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (n) must be '// &
              'greater than 0!')
        ELSE
          thisVector%isInit=.TRUE.
          thisVector%n=n
          CALL dmallocA(thisVector%b,n)
        ENDIF
      ELSE
        CALL eVectorType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - VectorType already initialized')
      ENDIF
      CALL validParams%clear()
    ENDSUBROUTINE init_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Clears the real vector
!> @param declares the vector type to act on
!>
    SUBROUTINE clear_RealVectorType(thisVector)
      CLASS(RealVectorType),INTENT(INOUT) :: thisVector
      thisVector%isInit=.FALSE.
      thisVector%n=0
      IF(ALLOCATED(thisVector%b)) CALL demallocA(thisVector%b)
      IF(VectorType_Paramsflag) CALL VectorType_Clear_ValidParams()
    ENDSUBROUTINE clear_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets one value in the real vector
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!> @param setval the value to be set
!>
    SUBROUTINE setOne_RealVectorType(thisVector,i,setval,ierr)
      CLASS(RealVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF((i <= thisVector%n) .AND. (i > 0)) THEN
          ierrc=0
          thisVector%b(i)=setval
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE setOne_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all values in the real vector with a scalar value
!> @param declares the vector type to act on
!> @param setval the scalar value to be set
!>
    SUBROUTINE setAll_scalar_RealVectorType(thisVector,setval,ierr)
      CLASS(RealVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=0
        thisVector%b=setval
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE setAll_scalar_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all the values in the real vector
!> @param declare the vector type to act on
!> @param setval the array of values to be set
!>
    SUBROUTINE setAll_array_RealVectorType(thisVector,setval,ierr)
      CLASS(RealVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-3
        IF(SIZE(setval) == thisVector%n) THEN
          ierrc=0
          thisVector%b=setval
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE setAll_array_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the real vector with a scalar value
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE setRange_scalar_RealVectorType(thisVector,istt,istp,setval,ierr)
      CLASS(RealVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n) THEN
          ierrc=0
          thisVector%b(istt:istp)=setval
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE setRange_scalar_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the real vector with an array of values
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE setRange_array_RealVectorType(thisVector,istt,istp,setval,ierr)
      CLASS(RealVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n) THEN
          ierrc=-3
          IF(istp-istt+1 == SIZE(setval)) THEN
            ierrc=0
            thisVector%b(istt:istp)=setval
          ENDIF
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE setRange_array_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets one value in the real vector
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!>
    SUBROUTINE getOne_RealVectorType(thisVector,i,getval,ierr)
      CLASS(RealVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(INOUT) :: getval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF((i <= thisVector%n) .AND. (i > 0)) THEN
          ierrc=0
          getval=thisVector%b(i)
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE getOne_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets all values in the real vector
!> @param declares the vector type to act on
!>
    SUBROUTINE getAll_RealVectorType(thisVector,getval,ierr)
      CLASS(RealVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(INOUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-3
        IF(SIZE(getval) == thisVector%n) THEN
          ierrc=0
          getval=thisVector%b
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE getAll_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets a range of values in the real vector
!> @param declares the vector type to act on
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE getRange_RealVectorType(thisVector,istt,istp,getval,ierr)
      CLASS(RealVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      REAL(SRK),INTENT(INOUT):: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n) THEN
          ierrc=-3
          IF(istp-istt+1 == SIZE(getval)) THEN
            ierrc=0
            getval=thisVector%b(istt:istp)
          ENDIF
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE getRange_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Initializes the PETSc vector
!> @param declares the vector type to act on
!> @param n the number of rows
!>
    SUBROUTINE init_PETScVectorType(thisVector,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_PETScVectorType'
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector
      TYPE(ParamType),INTENT(IN) :: Params
      TYPE(ParamType) :: validParams
      INTEGER(SIK) :: n, MPI_Comm_ID, nlocal

      !Check to set up required and optional param lists.
      IF(.NOT.VectorType_Paramsflag) CALL VectorType_Declare_ValidParams()

      !Validate against the reqParams and OptParams
      validParams=Params
      CALL validParams%validate(DistributedVectorType_reqParams,DistributedVectorType_optParams)

      !Pull Data from Parameter List
      CALL validParams%get('VectorType->n',n)
      CALL validParams%get('VectorType->MPI_Comm_ID',MPI_Comm_ID)
      CALL validParams%get('VectorType->nlocal',nlocal)

#ifdef FUTILITY_HAVE_PETSC
      IF(.NOT. thisVector%isInit) THEN
        IF(n < 1) THEN
          CALL eVectorType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (n) must be '// &
              'greater than 0!')
        ELSE
          thisVector%isInit=.TRUE.
          thisVector%n=n
          thisVector%comm=MPI_Comm_ID
          thisVector%nlocal=nlocal
          IF(.NOT.thisVector%isCreated) THEN
            CALL VecCreate(thisVector%comm,thisVector%b,iperr)
            thisVector%isCreated=.TRUE.
          ENDIF
          IF(nlocal<0) THEN
            CALL VecSetSizes(thisVector%b,PETSC_DECIDE,thisVector%n,iperr)
          ELSE
            CALL VecSetSizes(thisVector%b,nlocal,thisVector%n,iperr)
          ENDIF
          CALL VecSetType(thisVector%b,VECMPI,iperr)
          CALL VecSetFromOptions(thisVector%b,iperr)
          CALL VecSet(thisVector%b,0._SRK,iperr)
          CALL thisVector%assemble(iperr)
        ENDIF
      ELSE
        CALL eVectorType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - VectorType already initialized')
      ENDIF
#else
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
#endif
      CALL validParams%clear()
    ENDSUBROUTINE init_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Clears the PETSc vector
!> @param declares the vector type to act on
!>
    SUBROUTINE clear_PETScVectorType(thisVector)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector

#ifdef FUTILITY_HAVE_PETSC
      IF(thisVector%isInit) CALL VecDestroy(thisVector%b,iperr)
      thisVector%isInit=.FALSE.
      thisVector%isAssembled=.FALSE.
      thisVector%isCreated=.FALSE.
      thisVector%n=0
      IF(VectorType_Paramsflag) CALL VectorType_Clear_ValidParams()
#else
      CHARACTER(LEN=*),PARAMETER :: myName='clear_PETScVectorType'
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
#endif
    ENDSUBROUTINE clear_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets one value in the real vector
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!> @param setval the value to be set
!>
    SUBROUTINE setOne_PETScVectorType(thisVector,i,setval,ierr)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_PETSC
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF((i <= thisVector%n) .AND. (i > 0)) THEN
          CALL VecSetValue(thisVector%b,i-1,setval,INSERT_VALUES,iperr)
          thisVector%isAssembled=.FALSE.
          ierrc=iperr
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='setOne_PETScVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
#endif
    ENDSUBROUTINE setOne_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all values in the PETSc vector with a scalar value
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!>
    SUBROUTINE setAll_scalar_PETScVectorType(thisVector,setval,ierr)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_PETSC
      ierrc=-1
      IF(thisVector%isInit) THEN
        CALL thisVector%assemble(iperr)
        IF(iperr == 0) CALL VecSet(thisVector%b,setval,iperr)
        thisVector%isAssembled=.FALSE.
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='setAll_scalar_PETScVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
#endif
    ENDSUBROUTINE setAll_scalar_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all the values in the PETSc vector with an array of values
!> @param declare the vector type to act on
!> @param setval the array of values to be set
!>
    SUBROUTINE setAll_array_PETScVectorType(thisVector,setval,ierr)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_PETSC
      INTEGER(SIK) :: i

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-3
        IF(SIZE(setval) == thisVector%n) THEN
          DO i=1,thisVector%n
            CALL VecSetValue(thisVector%b,i-1,setval(i),INSERT_VALUES,iperr)
          ENDDO
          thisVector%isAssembled=.FALSE.
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='setAll_array_PETScVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
#endif
    ENDSUBROUTINE setAll_array_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the PETSc vector with a scalar value
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE setRange_scalar_PETScVectorType(thisVector,istt,istp,setval,ierr)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_PETSC
      INTEGER(SIK) :: i

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n) THEN
          DO i=istt,istp
            CALL VecSetValue(thisVector%b,i-1,setval,INSERT_VALUES,iperr)
          ENDDO
          thisVector%isAssembled=.FALSE.
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='setRange_scalar_PETScVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
#endif
    ENDSUBROUTINE setRange_scalar_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the PETSc vector with an array of values
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE setRange_array_PETScVectorType(thisVector,istt,istp,setval,ierr)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_PETSC
      INTEGER(SIK) :: i

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n) THEN
          ierrc=-3
          IF(istp-istt+1 == SIZE(setval)) THEN
            DO i=istt,istp
              CALL VecSetValue(thisVector%b,i-1,setval(i-istt+1),INSERT_VALUES,iperr)
            ENDDO
            thisVector%isAssembled=.FALSE.
          ENDIF
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='setRange_array_PETScVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
#endif
    ENDSUBROUTINE setRange_array_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets one values in the PETSc vector
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!>
    SUBROUTINE getOne_PETScVectorType(thisVector,i,getval,ierr)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(INOUT) :: getval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_PETSC
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF((i <= thisVector%n) .AND. (i > 0)) THEN
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
          CALL VecGetValues(thisVector%b,1,i-1,getval,iperr)
          ierrc=iperr
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='getOne_PETScVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
#endif
    ENDSUBROUTINE getOne_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets all values in the PETSc vector
!> @param declares the vector type to act on
!>
    SUBROUTINE getAll_PETScVectorType(thisVector,getval,ierr)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(INOUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_PETSC
      INTEGER(SIK) :: i

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-3
        IF(SIZE(getval) == thisVector%n) THEN
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
          DO i=1,thisVector%n
            CALL VecGetValues(thisVector%b,1,i-1,getval(i),iperr)
          ENDDO
          ierrc=iperr
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='getAll_PETScVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
#endif
    ENDSUBROUTINE getAll_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets a range of  values in the PETSc vector
!> @param declares the vector type to act on
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE getRange_PETScVectorType(thisVector,istt,istp,getval,ierr)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      REAL(SRK),INTENT(INOUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_PETSC
      INTEGER(SIK) :: i

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n) THEN
          ierrc=-3
          IF(istp-istt+1 == SIZE(getval)) THEN
            IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
            DO i=istt,istp
              CALL VecGetValues(thisVector%b,1,i-1,getval(i-istt+1),iperr)
            ENDDO
            ierrc=iperr
          ENDIF
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='getRange_PETScVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
#endif
    ENDSUBROUTINE getRange_PETScVectorType
!
!-------------------------------------------------------------------------------
    SUBROUTINE assemble_PETScVectorType(thisVector,ierr)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_PETSC
      ierrc=0
      IF(.NOT.thisVector%isAssembled) THEN
        thisVector%isAssembled=.FALSE.
        CALL VecAssemblyBegin(thisVector%b,iperr)
        IF(iperr == 0) CALL VecAssemblyEnd(thisVector%b,iperr)
        IF(iperr == 0) thisVector%isAssembled=.TRUE.
        ierrc=iperr
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='assemble_PETScVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
#endif
    ENDSUBROUTINE assemble_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Trilinos vector
!> @param declares the vector type to act on
!> @param n the number of rows
!>
    SUBROUTINE init_TrilinosVectorType(thisVector,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_TrilinosVectorType'
      CLASS(TrilinosVectorType),INTENT(INOUT) :: thisVector
      TYPE(ParamType),INTENT(IN) :: Params
      TYPE(ParamType) :: validParams
      INTEGER(SIK) :: n, MPI_Comm_ID, nlocal

      !Check to set up required and optional param lists.
      IF(.NOT.VectorType_Paramsflag) CALL VectorType_Declare_ValidParams()

      !Validate against the reqParams and OptParams
      validParams=Params
      CALL validParams%validate(DistributedVectorType_reqParams,DistributedVectorType_optParams)

      !Pull Data from Parameter List
      CALL validParams%get('VectorType->n',n)
      CALL validParams%get('VectorType->MPI_Comm_ID',MPI_Comm_ID)
      CALL validParams%get('VectorType->nlocal',nlocal)

      IF(nlocal==-1) nlocal=n

#ifdef FUTILITY_HAVE_Trilinos
      IF(.NOT. thisVector%isInit) THEN
        IF(n < 1) THEN
          CALL eVectorType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (n) must be '// &
              'greater than 0!')
        ELSEIF(nlocal<0) THEN
          CALL eVectorType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of local values (nlocal) '// &
              'must be greater than 0!')
        ELSE
          thisVector%isInit=.TRUE.
          thisVector%n=n
          thisVector%comm=MPI_Comm_ID
          thisVector%nlocal=nlocal
          IF(.NOT.thisVector%isCreated) THEN
            CALL ForPETRA_VecInit(thisVector%b,n,nlocal,thisVector%comm)
            thisVector%isCreated=.TRUE.
          ENDIF
        ENDIF
      ELSE
        CALL eVectorType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - VectorType already initialized')
      ENDIF
#else
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - Trilinos not enabled.  You will'// &
         'need to recompile with Trilinos enabled to use this feature.')
#endif
      CALL validParams%clear()
    ENDSUBROUTINE init_TrilinosVectorType
!
!-------------------------------------------------------------------------------
!> @brief Clears the Trilinos vector
!> @param declares the vector type to act on
!>
    SUBROUTINE clear_TrilinosVectorType(thisVector)
      CLASS(TrilinosVectorType),INTENT(INOUT) :: thisVector

#ifdef FUTILITY_HAVE_Trilinos
      !IF(thisVector%isInit) CALL ForPETRA_VecDestroy(thisVector%b)
      thisVector%isInit=.FALSE.
      thisVector%isAssembled=.FALSE.
      thisVector%isCreated=.FALSE.
      thisVector%n=0
      CALL ForPETRA_VecDestroy(thisVector%b)
      IF(VectorType_Paramsflag) CALL VectorType_Clear_ValidParams()
#else
      CHARACTER(LEN=*),PARAMETER :: myName='clear_TrilinosVectorType'
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - Trilinos not enabled.  You will'// &
         'need to recompile with Trilinos enabled to use this feature.')
#endif
    ENDSUBROUTINE clear_TrilinosVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets one value in the real vector
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!> @param setval the value to be set
!>
    SUBROUTINE setOne_TrilinosVectorType(thisVector,i,setval,ierr)
      CLASS(TrilinosVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_Trilinos
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF((i <= thisVector%n) .AND. (i > 0)) THEN
          CALL ForPETRA_VecSet(thisVector%b,i,setval)
          ierrc=0
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='setOne_TrilinosVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - Trilinos not enabled.  You will'// &
         'need to recompile with Trilinos enabled to use this feature.')
#endif
    ENDSUBROUTINE setOne_TrilinosVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all values in the Trilinos vector with a scalar value
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!>
    SUBROUTINE setAll_scalar_TrilinosVectorType(thisVector,setval,ierr)
      CHARACTER(LEN=*),PARAMETER :: myName='setAll_scalar_TrilinosVectorType'
      CLASS(TrilinosVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_Trilinos
      ierrc=-1
      IF(thisVector%isInit) THEN
        CALL ForPETRA_VecSetAll(thisVector%b,setval)
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - Trilinos not enabled.  You will'// &
         'need to recompile with Trilinos enabled to use this feature.')
#endif
    ENDSUBROUTINE setAll_scalar_TrilinosVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all the values in the Trilinos vector with an array of values
!> @param declare the vector type to act on
!> @param setval the array of values to be set
!>
    SUBROUTINE setAll_array_TrilinosVectorType(thisVector,setval,ierr)
      CHARACTER(LEN=*),PARAMETER :: myName='setAll_array_TrilinosVectorType'
      CLASS(TrilinosVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_Trilinos

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-3
        IF(SIZE(setval) == thisVector%n) THEN
!TODO
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - This interface is not available.')
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - Trilinos not enabled.  You will'// &
         'need to recompile with Trilinos enabled to use this feature.')
#endif
    ENDSUBROUTINE setAll_array_TrilinosVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the Trilinos vector with a scalar value
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE setRange_scalar_TrilinosVectorType(thisVector,istt,istp,setval,ierr)
      CLASS(TrilinosVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_Trilinos
      INTEGER(SIK) :: i

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n) THEN
          DO i=istt,istp
            CALL ForPETRA_VecSet(thisVector%b,i,setval)
          ENDDO
          ierrc=0
          thisVector%isAssembled=.TRUE.
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='setRange_scalar_TrilinosVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - Trilinos not enabled.  You will'// &
         'need to recompile with Trilinos enabled to use this feature.')
#endif
    ENDSUBROUTINE setRange_scalar_TrilinosVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the Trilinos vector with an array of values
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE setRange_array_TrilinosVectorType(thisVector,istt,istp,setval,ierr)
      CLASS(TrilinosVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_Trilinos
      INTEGER(SIK) :: i

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n) THEN
          ierrc=-3
          IF(istp-istt+1 == SIZE(setval)) THEN
            DO i=istt,istp
              CALL ForPETRA_VecSet(thisVector%b,i,setval(i-istt+1))
            ENDDO
            thisVector%isAssembled=.TRUE.
            ierrc=0
          ENDIF
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='setRange_array_TrilinosVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - Trilinos not enabled.  You will'// &
         'need to recompile with Trilinos enabled to use this feature.')
#endif
    ENDSUBROUTINE setRange_array_TrilinosVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets one values in the Trilinos vector
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!>
    SUBROUTINE getOne_TrilinosVectorType(thisVector,i,getval,ierr)
      CLASS(TrilinosVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(INOUT) :: getval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_Trilinos
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF((i <= thisVector%n) .AND. (i > 0)) THEN
          CALL ForPETRA_VecGet(thisVector%b,i,getval)
          ierrc=0
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='getOne_TrilinosVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - Trilinos not enabled.  You will'// &
         'need to recompile with Trilinos enabled to use this feature.')
#endif
    ENDSUBROUTINE getOne_TrilinosVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets all values in the Trilinos vector
!> @param declares the vector type to act on
!>
    SUBROUTINE getAll_TrilinosVectorType(thisVector,getval,ierr)
      CHARACTER(LEN=*),PARAMETER :: myName='getAll_TrilinosVectorType'
      CLASS(TrilinosVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(INOUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_Trilinos

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-3
        IF(SIZE(getval) == thisVector%n) THEN
!TODO
          CALL eVectorType%raiseFatalError('Incorrect call to '// &
             modName//'::'//myName//' - This interface is not available.')
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - Trilinos not enabled.  You will'// &
         'need to recompile with Trilinos enabled to use this feature.')
#endif
    ENDSUBROUTINE getAll_TrilinosVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets a range of  values in the Trilinos vector
!> @param declares the vector type to act on
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE getRange_TrilinosVectorType(thisVector,istt,istp,getval,ierr)
      CLASS(TrilinosVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      REAL(SRK),INTENT(INOUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_Trilinos
      INTEGER(SIK) :: i

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n) THEN
          ierrc=-3
          IF(istp-istt+1 == SIZE(getval)) THEN
            DO i=istt,istp
              CALL ForPETRA_VecGet(thisVector%b,i,getval(i-istt+1))
            ENDDO
            ierrc=0
          ENDIF
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='getRange_TrilinosVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - Trilinos not enabled.  You will'// &
         'need to recompile with Trilinos enabled to use this feature.')
#endif
    ENDSUBROUTINE getRange_TrilinosVectorType
!
!-------------------------------------------------------------------------------
    SUBROUTINE assemble_TrilinosVectorType(thisVector,ierr)
      CLASS(TrilinosVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef FUTILITY_HAVE_Trilinos
      !Trilinos vectors don't need assembly
#else
      CHARACTER(LEN=*),PARAMETER :: myName='assemble_TrilinosVectorType'
      IF(PRESENT(ierr)) ierr=-1
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - Trilinos not enabled.  You will'// &
         'need to recompile with Trilinos enabled to use this feature.')
#endif
    ENDSUBROUTINE assemble_TrilinosVectorType
!
!-------------------------------------------------------------------------------
!> @brief Function provides an interface to vector absolute value summation
!> of a vector (x).
!> @param thisVector derived vector type
!> @param n the size of the vector @c x
!> @param incx the increment to use when looping over elements in @c x
!> @return r the sum of the absolute values of @c x
!>
    FUNCTION asum_VectorType(thisVector,n,incx) RESULT(r)
      CHARACTER(LEN=*),PARAMETER :: myName='asum_VectorType'
      CLASS(VectorType),INTENT(INOUT)     :: thisVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      REAL(SRK) :: r

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        IF(PRESENT(n) .AND. PRESENT(incx)) THEN
          r=BLAS1_asum(n,thisVector%b,incx)
        ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          r=BLAS1_asum(n,thisVector%b)
        ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
          r=BLAS1_asum(thisVector%b,incx)
        ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          r=BLAS1_asum(thisVector%b)
        ENDIF
      TYPE IS(PETScVectorType)
#ifdef FUTILITY_HAVE_PETSC
        IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
        IF(iperr == 0) CALL VecNorm(thisVector%b,NORM_1,r,iperr)
#else
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - PETSc not enabled.  You will'// &
           'need to recompile with PETSc enabled to use this feature.')
#endif
      TYPE IS(TrilinosVectorType)
#ifdef FUTILITY_HAVE_Trilinos
        CALL ForPETRA_VecSUM(thisVector%b,r)
#else
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - Trilinos not enabled.  You will'// &
           'need to recompile with Trilinos enabled to use this feature.')
#endif
      CLASS DEFAULT
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - This interface is not available.')
      ENDSELECT
    ENDFUNCTION asum_VectorType
!
!-------------------------------------------------------------------------------
!> @brief Subroutine provides an interface to compute the result of a vector (y)
!> plus a vector (x) times a scalar (a).
!> @param thisVector derived vector type
!> @param newVector resulting derived vector type
!> @param a the constant to multiply with @c x
!> @param n the size of the vectors @c x and @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param incy the increment to use when looping over elements in @c y
!>
    SUBROUTINE axpy_scalar_VectorType(thisVector,newVector,a,n,incx,incy)
      CHARACTER(LEN=*),PARAMETER :: myName='axpy_scalar_VectorType'
      CLASS(VectorType),INTENT(INOUT)  :: thisVector
      CLASS(VectorType),INTENT(INOUT)  :: newVector
      REAL(SRK),INTENT(IN),OPTIONAL :: a
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incy
      REAL(SRK) :: alpha

      alpha=1._SRK
      IF(PRESENT(a)) alpha=a

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(newVector); TYPE IS(RealVectorType)
          IF(PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
            CALL BLAS1_axpy(n,alpha,thisVector%b,incx,newVector%b,incy)
          ELSEIF(PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            CALL BLAS1_axpy(n,alpha,thisVector%b,newVector%b,incx)
          ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
            CALL BLAS1_axpy(n,alpha,thisVector%b,newVector%b,incy)
          ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            CALL BLAS1_axpy(n,alpha,thisVector%b,newVector%b)
          ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            CALL BLAS1_axpy(alpha,thisVector%b,newVector%b)
          ENDIF
        ENDSELECT
      TYPE IS(PETScVectorType)
        SELECTTYPE(newVector); TYPE IS(PETScVectorType)
#ifdef FUTILITY_HAVE_PETSC
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
          IF(iperr == 0) CALL VecAXPY(newVector%b,alpha,thisVector%b,iperr)
#else
          CALL eVectorType%raiseFatalError('Incorrect call to '// &
             modName//'::'//myName//' - PETSc not enabled.  You will'// &
             'need to recompile with PETSc enabled to use this feature.')
#endif
        ENDSELECT
      TYPE IS(TrilinosVectorType)
        SELECTTYPE(newVector); TYPE IS(TrilinosVectorType)
#ifdef FUTILITY_HAVE_Trilinos
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble()
          CALL ForPETRA_VecAXPY(newVector%b,thisVector%b,alpha,1.0_SRK)
#else
          CALL eVectorType%raiseFatalError('Incorrect call to '// &
             modName//'::'//myName//' - Trilinos not enabled.  You will'// &
             'need to recompile with Trilinos enabled to use this feature.')
#endif
        ENDSELECT
      CLASS DEFAULT
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - This interface is not available.')
      ENDSELECT
    ENDSUBROUTINE axpy_scalar_VectorType
!
!-------------------------------------------------------------------------------
!> @brief Subroutine provides an interface to compute the result of a vector (y)
!> plus a vector (x) times a vector (aVector).
!> @param thisVector derived vector type
!> @param newVector resulting derived vector type
!> @param a the constant to multiply with @c x
!> @param n the size of the vectors @c x and @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param incy the increment to use when looping over elements in @c y
!>
    SUBROUTINE axpy_vector_VectorType(thisVector,newVector,aVector,n,incx,incy)
      CHARACTER(LEN=*),PARAMETER :: myName='axpy_vector_VectorType'
      CLASS(VectorType),INTENT(INOUT)  :: thisVector
      CLASS(VectorType),INTENT(INOUT)  :: newVector
      CLASS(VectorType),INTENT(INOUT)  :: aVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incy
      REAL(SRK),ALLOCATABLE :: tmpthis(:),tmpnew(:),tmpa(:)

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(newVector); TYPE IS(RealVectorType)
          SELECTTYPE(aVector); TYPE IS(RealVectorType)
            ALLOCATE(tmpthis(thisVector%n))
            ALLOCATE(tmpnew(newVector%n))
            ALLOCATE(tmpa(aVector%n))
            CALL thisVector%get(tmpthis)
            CALL newVector%get(tmpnew)
            CALL aVector%get(tmpa)
          ENDSELECT
        ENDSELECT
      TYPE IS(PETScVectorType)
        SELECTTYPE(newVector); TYPE IS(PETScVectorType)
          SELECTTYPE(aVector); TYPE IS(PETScVectorType)
#ifdef FUTILITY_HAVE_PETSC
            ALLOCATE(tmpthis(thisVector%n))
            ALLOCATE(tmpnew(newVector%n))
            ALLOCATE(tmpa(aVector%n))
            CALL thisVector%get(tmpthis)
            CALL newVector%get(tmpnew)
            CALL aVector%get(tmpa)
#else
            CALL eVectorType%raiseFatalError('Incorrect call to '// &
               modName//'::'//myName//' - PETSc not enabled.  You will'// &
               'need to recompile with PETSc enabled to use this feature.')
#endif
          ENDSELECT
        ENDSELECT
      CLASS DEFAULT
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - This interface is not available.')
      ENDSELECT

      IF(PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
        CALL BLAS1_axpy(n,tmpa,tmpthis,incx,tmpnew,incy)
      ELSEIF(PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_axpy(n,tmpa,tmpthis,tmpnew,incx)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
        CALL BLAS1_axpy(n,tmpa,tmpthis,tmpnew,incy)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_axpy(n,tmpa,tmpthis,tmpnew)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_axpy(tmpa,tmpthis,tmpnew)
      ENDIF

      SELECTTYPE(newVector); TYPE IS(RealVectorType)
        CALL newVector%set(tmpnew)
      TYPE IS(PETScVectorType)
        CALL newVector%set(tmpnew)
      ENDSELECT

      DEALLOCATE(tmpthis)
      DEALLOCATE(tmpnew)
      DEALLOCATE(tmpa)
    ENDSUBROUTINE axpy_vector_VectorType
!
!-------------------------------------------------------------------------------
!> @brief Subroutine provides an interface to copy a vector (x) to another
!> vector (y).
!> @param x derived vector type
!> @param y resulting derived vector type
!> @param n the size of the vectors @c x and @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param incy the increment to use when looping over elements in @c y
!>
    SUBROUTINE copy_VectorType(thisVector,newVector,n,incx,incy)
      CHARACTER(LEN=*),PARAMETER :: myName='copy_VectorType'
      CLASS(VectorType),INTENT(IN)     :: thisVector
      CLASS(VectorType),INTENT(INOUT)  :: newVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incy

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(newVector); TYPE IS(RealVectorType)
          IF(PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
            CALL BLAS1_copy(n,thisVector%b,incx,newVector%b,incy)
          ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
            CALL BLAS1_copy(thisVector%b,incx,newVector%b,incy)
          ELSEIF(PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            CALL BLAS1_copy(n,thisVector%b,newVector%b,incx)
          ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
            CALL BLAS1_copy(n,thisVector%b,newVector%b,incy)
          ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            CALL BLAS1_copy(thisVector%b,newVector%b,incx)
          ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
            CALL BLAS1_copy(thisVector%b,newVector%b,incy)
          ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            CALL BLAS1_copy(n,thisVector%b,newVector%b)
          ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            CALL BLAS1_copy(thisVector%b,newVector%b)
          ENDIF
        ENDSELECT
      TYPE IS(PETScVectorType)
        SELECTTYPE(newVector); TYPE IS(PETScVectorType)
#ifdef FUTILITY_HAVE_PETSC
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
          IF(iperr == 0) CALL VecCopy(thisVector%b,newVector%b,iperr)
#else
          CALL eVectorType%raiseFatalError('Incorrect call to '// &
             modName//'::'//myName//' - PETSc not enabled.  You will'// &
             'need to recompile with PETSc enabled to use this feature.')
#endif
        ENDSELECT
      TYPE IS(TrilinosVectorType)
        SELECTTYPE(newVector); TYPE IS(TrilinosVectorType)
#ifdef FUTILITY_HAVE_Trilinos
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble()
          CALL ForPETRA_VecCopy(newVector%b,thisVector%b)
#else
          CALL eVectorType%raiseFatalError('Incorrect call to '// &
             modName//'::'//myName//' - Trilinos not enabled.  You will'// &
             'need to recompile with Trilinos enabled to use this feature.')
#endif
        ENDSELECT
      CLASS DEFAULT
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - This interface is not available.')
      ENDSELECT
    ENDSUBROUTINE copy_VectorType
!
!-------------------------------------------------------------------------------
!> @brief Subroutine provides an interface to compute dot product of two
!> vectors (x and y).
!> @param thisVector derived vector type.
!> @param thatVector derived vector type.
!> @param n the size of the vectors @c x and @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param incy the increment to use when looping over elements in @c y
!> @return r the dot product of @c x and @c y
!>
    FUNCTION dot_VectorType(thisVector,thatVector,n,incx,incy)  RESULT(r)
      CHARACTER(LEN=*),PARAMETER :: myName='dot_VectorType'
      CLASS(VectorType),INTENT(INOUT)     :: thisVector
      CLASS(VectorType),INTENT(INOUT)     :: thatVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incy
      REAL(SRK) :: r

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(thatVector); TYPE IS(RealVectorType)
          IF(PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
            r=BLAS1_dot(n,thisVector%b,incx,thatVector%b,incy)
          ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
            r=BLAS1_dot(thisVector%b,incx,thatVector%b,incy)
          ELSEIF(PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            r=BLAS1_dot(n,thisVector%b,thatVector%b,incx)
          ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
            r=BLAS1_dot(n,thisVector%b,thatVector%b,incy)
          ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            r=BLAS1_dot(n,thisVector%b,thatVector%b)
          ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            r=BLAS1_dot(thisVector%b,thatVector%b,incx)
          ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
            r=BLAS1_dot(thisVector%b,thatVector%b,incy)
          ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            r=BLAS1_dot(thisVector%b,thatVector%b)
          ENDIF
        ENDSELECT
      TYPE IS(PETScVectorType)
        SELECTTYPE(thatVector); TYPE IS(PETScVectorType)
#ifdef FUTILITY_HAVE_PETSC
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
          IF(iperr == 0) CALL VecTDot(thisVector%b,thatVector%b,r,iperr)
#else
          CALL eVectorType%raiseFatalError('Incorrect call to '// &
             modName//'::'//myName//' - PETSc not enabled.  You will'// &
             'need to recompile with PETSc enabled to use this feature.')
#endif
        ENDSELECT
      CLASS DEFAULT
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - This interface is not available.')
      ENDSELECT
    ENDFUNCTION dot_VectorType
!
!-------------------------------------------------------------------------------
!> @brief Subroutine provides an interface to compute the inex of the absolute
!> maximum of a vector (x).
!> @param thisVector derived vector type.
!> @param n the size of the vectors @c x
!> @param incx the increment to use when looping over elements in @c x
!> @return imax index of the absolute max of @c y
!>
    FUNCTION iamax_VectorType(thisVector,n,incx)  RESULT(imax)
      CHARACTER(LEN=*),PARAMETER :: myName='iamax_VectorType'
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      REAL(SRK),ALLOCATABLE :: tmpthis(:)
      INTEGER(SIK) :: imax

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        CALL thisVector%get(tmpthis)
      TYPE IS(PETScVectorType)
#ifdef FUTILITY_HAVE_PETSC
        ALLOCATE(tmpthis(thisVector%n))
        CALL thisVector%get(tmpthis)
#else
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - PETSc not enabled.  You will'// &
           'need to recompile with PETSc enabled to use this feature.')
#endif
      CLASS DEFAULT
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - This interface is not available.')
      ENDSELECT

      IF(PRESENT(n) .AND. PRESENT(incx)) THEN
        imax=BLAS1_iamax(n,tmpthis,incx)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
        imax=BLAS1_iamax(tmpthis,incx)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        imax=BLAS1_iamax(n,tmpthis)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        imax=BLAS1_iamax(tmpthis)
      ENDIF
      DEALLOCATE(tmpthis)
    ENDFUNCTION iamax_VectorType
!
!-------------------------------------------------------------------------------
!> @brief Subroutine provides an interface to compute the index of the absolute
!> minimum of a vector (x).
!> @param thisVector derived vector type.
!> @param n the size of the vectors @c x
!> @param incx the increment to use when looping over elements in @c x
!> @return imin index of the absolute min of @c x
!>

    FUNCTION iamin_VectorType(thisVector,n,incx)  RESULT(imin)
      CHARACTER(LEN=*),PARAMETER :: myName='iamin_VectorType'
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      REAL(SRK),ALLOCATABLE :: tmpthis(:)
      INTEGER(SIK) :: imin

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        CALL thisVector%get(tmpthis)
      TYPE IS(PETScVectorType)
#ifdef FUTILITY_HAVE_PETSC
        ALLOCATE(tmpthis(thisVector%n))
        CALL thisVector%get(tmpthis)
#else
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - PETSc not enabled.  You will'// &
           'need to recompile with PETSc enabled to use this feature.')
#endif
      CLASS DEFAULT
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - This interface is not available.')
      ENDSELECT

      IF(PRESENT(n) .AND. PRESENT(incx)) THEN
        imin=BLAS1_iamin(n,tmpthis,incx)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
        imin=BLAS1_iamin(tmpthis,incx)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        imin=BLAS1_iamin(n,tmpthis)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        imin=BLAS1_iamin(tmpthis)
      ENDIF
      DEALLOCATE(tmpthis)
    ENDFUNCTION iamin_VectorType
!
!-------------------------------------------------------------------------------
!> @brief Subroutine provides an interface to compute the 2-norm of a
!> vector (x).
!> @param thisVector derived vector type
!> @param n the size of the vectors @c x
!> @param incx the increment to use when looping over elements in @c x
!> @return norm2 the 2-norm of @c x
!>
    FUNCTION nrm2_VectorType(thisVector,n,incx)  RESULT(norm2)
      CHARACTER(LEN=*),PARAMETER :: myName='nrm2_VectorType'
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      REAL(SRK) :: norm2

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        IF(PRESENT(n) .AND. PRESENT(incx)) THEN
          norm2=BLAS1_nrm2(n,thisVector%b,incx)
        ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
          norm2=BLAS1_nrm2(thisVector%b,incx)
        ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          norm2=BLAS1_nrm2(n,thisVector%b)
        ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          norm2=BLAS1_nrm2(thisVector%b)
        ENDIF
      TYPE IS(PETScVectorType)
#ifdef FUTILITY_HAVE_PETSC
        IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
        IF(iperr == 0) CALL VecNorm(thisVector%b,NORM_2,norm2,iperr)
#else
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - PETSc not enabled.  You will'// &
           'need to recompile with PETSc enabled to use this feature.')
#endif
      TYPE IS(TrilinosVectorType)
#ifdef FUTILITY_HAVE_Trilinos
        IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble()
        CALL ForPETRA_VecNorm2(thisVector%b,norm2)
#else
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - Trilinos not enabled.  You will'// &
           'need to recompile with Trilinos enabled to use this feature.')
#endif
      CLASS DEFAULT
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - This interface is not available.')
      ENDSELECT
    ENDFUNCTION nrm2_VectorType
!
!-------------------------------------------------------------------------------
!> @brief Subroutine provides an interface to scale a vector (x) by a
!> scalar (a).
!> @param thisVector derived vector type
!> @param a the constant to multiply with @c x
!> @param n the size of the vectors @c x
!> @param incx the increment to use when looping over elements in @c x
!>
    SUBROUTINE scal_scalar_VectorType(thisVector,a,n,incx)
      CHARACTER(LEN=*),PARAMETER :: myName='scal_scalar_VectorType'
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: a
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        IF(PRESENT(n) .AND. PRESENT(incx)) THEN
          CALL BLAS1_scal(n,a,thisVector%b,incx)
        ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
          CALL BLAS1_scal(a,thisVector%b,incx)
        ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          CALL BLAS1_scal(n,a,thisVector%b)
        ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          CALL BLAS1_scal(a,thisVector%b)
        ENDIF
      TYPE IS(PETScVectorType)
#ifdef FUTILITY_HAVE_PETSC
        IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
        IF(iperr == 0) CALL VecScale(thisVector%b,a,iperr)
#else
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - PETSc not enabled.  You will'// &
           'need to recompile with PETSc enabled to use this feature.')
#endif
      TYPE IS(TrilinosVectorType)
#ifdef FUTILITY_HAVE_Trilinos
        CALL ForPETRA_VecScale(thisVector%b,a)
#else
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - Trilinos not enabled.  You will'// &
           'need to recompile with Trilinos enabled to use this feature.')
#endif
      CLASS DEFAULT
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - This interface is not available.')
      ENDSELECT
    ENDSUBROUTINE scal_scalar_VectorType
!
!-------------------------------------------------------------------------------
!> @brief Subroutine provides an interface to scale a vector (x) by another
!> vector (aVector).
!> @param thisVector derived vector type
!> @param a the constant to multiply with @c x
!> @param n the size of the vectors @c x
!> @param incx the increment to use when looping over elements in @c x
!>
    SUBROUTINE scal_vector_VectorType(thisVector,aVector,n,incx)
      CHARACTER(LEN=*),PARAMETER :: myName='scal_vector_VectorType'
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      CLASS(VectorType),INTENT(INOUT) :: aVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      REAL(SRK),ALLOCATABLE :: tmpthis(:),tmpa(:)

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(aVector); TYPE IS(RealVectorType)
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpa(aVector%n))
          CALL thisVector%get(tmpthis)
          CALL aVector%get(tmpa)
        ENDSELECT
      TYPE IS(PETScVectorType)
        SELECTTYPE(aVector); TYPE IS(PETScVectorType)
#ifdef FUTILITY_HAVE_PETSC
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpa(aVector%n))
          CALL thisVector%get(tmpthis)
          CALL aVector%get(tmpa)
#else
          CALL eVectorType%raiseFatalError('Incorrect call to '// &
             modName//'::'//myName//' - PETSc not enabled.  You will'// &
             'need to recompile with PETSc enabled to use this feature.')
#endif
        ENDSELECT
      CLASS DEFAULT
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - This interface is not available.')
      ENDSELECT

      IF(PRESENT(n) .AND. PRESENT(incx)) THEN
        CALL BLAS1_scal(n,tmpa,tmpthis,incx)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
        CALL BLAS1_scal(tmpa,tmpthis,incx)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        CALL BLAS1_scal(n,tmpa,tmpthis)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        CALL BLAS1_scal(tmpa,tmpthis)
      ENDIF

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        CALL thisVector%set(tmpthis)
      TYPE IS(PETScVectorType)
        CALL thisVector%set(tmpthis)
      ENDSELECT

      DEALLOCATE(tmpthis)
      DEALLOCATE(tmpa)
    ENDSUBROUTINE scal_vector_VectorType
!
!-------------------------------------------------------------------------------
!> @brief Subroutine swaps a vector @c x with a vector @c y
!> @param thisVector derived vector type
!> @param thatVector derived vector type
!> @param n the size of the vectors @c x and @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param incy the increment to use when looping over elements in @c y
!>
    SUBROUTINE swap_VectorType(thisVector,thatVector,n,incx,incy)
      CHARACTER(LEN=*),PARAMETER :: myName='swap_VectorType'
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      CLASS(VectorType),INTENT(INOUT) :: thatVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incy

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(thatVector); TYPE IS(RealVectorType)
          IF(PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
            CALL BLAS1_swap(n,thisVector%b,incx,thatVector%b,incy)
          ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
            CALL BLAS1_swap(thisVector%b,incx,thatVector%b,incy)
          ELSEIF(PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            CALL BLAS1_swap(n,thisVector%b,thatVector%b,incx)
          ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
            CALL BLAS1_swap(n,thisVector%b,thatVector%b,incy)
          ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            CALL BLAS1_swap(thisVector%b,thatVector%b,incx)
          ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
            CALL BLAS1_swap(thisVector%b,thatVector%b,incy)
          ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            CALL BLAS1_swap(n,thisVector%b,thatVector%b)
          ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            CALL BLAS1_swap(thisVector%b,thatVector%b)
          ENDIF
        ENDSELECT
      TYPE IS(PETScVectorType)
        SELECTTYPE(thatVector); TYPE IS(PETScVectorType)
#ifdef FUTILITY_HAVE_PETSC
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
          IF(.NOT.thatVector%isAssembled) CALL thatVector%assemble(iperr)
          IF(iperr == 0) CALL VecSwap(thisVector%b,thatVector%b,iperr)
#else
          CALL eVectorType%raiseFatalError('Incorrect call to '// &
             modName//'::'//myName//' - PETSc not enabled.  You will'// &
             'need to recompile with PETSc enabled to use this feature.')
#endif
        ENDSELECT
      CLASS DEFAULT
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - This interface is not available.')
      ENDSELECT
    ENDSUBROUTINE swap_VectorType
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
      INTEGER(SIK) :: n,MPI_Comm,nlocal

      !Setup the required and optional parameter lists
      n=1
      MPI_Comm=1
      nlocal=-1
      !Real Vector Type - Required
      CALL RealVectorType_reqParams%add('VectorType->n',n)

      !Distributed Vector Type - Required
      CALL DistributedVectorType_reqParams%add('VectorType->n',n)
      CALL DistributedVectorType_reqParams%add('VectorType->MPI_Comm_ID',MPI_Comm)

      !There are no optional parameters at this time.
      CALL DistributedVectorType_optParams%add('VectorType->nlocal',nlocal)

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

    ENDSUBROUTINE VectorType_Clear_ValidParams
!
ENDMODULE VectorTypes
