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
!> @brief Utility module for defining vector types.
!>
!> The types of vectors defined in this module include a real vector type,
!> and PETSc vector type. 
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
  IMPLICIT NONE

#ifdef HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
#endif

  PRIVATE
!
! List of public members
  PUBLIC :: eVectorType
  PUBLIC :: VectorType
  PUBLIC :: RealVectorType
  PUBLIC :: PETScVectorType
  PUBLIC :: BLAS_asum
  PUBLIC :: BLAS_axpy
  PUBLIC :: BLAS_copy
  PUBLIC :: BLAS_dot
  PUBLIC :: BLAS_iamax
  PUBLIC :: BLAS_iamin
  PUBLIC :: BLAS_nrm2
  PUBLIC :: BLAS_scal
  PUBLIC :: BLAS_swap
  
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
      PROCEDURE(int_vector_clear_sub),DEFERRED,PASS :: clear
      !> Deferred routine for initializing the vector
      PROCEDURE(int_vector_init_sub),DEFERRED,PASS :: init
      !> Deferred routine for setting one vector value
      PROCEDURE(int_vector_setOne_sub),DEFERRED,PASS :: setOne
      !> Deferred routine for setting all vector values with scalar
      PROCEDURE(int_vector_setAll_scalar_sub),DEFERRED,PASS :: setAll_scalar
      !> Deferred routine for setting all vector values with array
      PROCEDURE(int_vector_setAll_array_sub),DEFERRED,PASS :: setAll_array
      !> Deferred routine for setting all vector values with scalar
      PROCEDURE(int_vector_setRange_scalar_sub),DEFERRED,PASS :: setRange_scalar
      !> Deferred routine for setting all vector values with array
      PROCEDURE(int_vector_setRange_array_sub),DEFERRED,PASS :: setRange_array
      GENERIC :: set => setOne,setAll_scalar,setAll_array,setRange_scalar,setRange_array
      !> Deferred routine for getting vector value
      PROCEDURE(int_vector_getOne_sub),DEFERRED,PASS :: getOne
      !> Deferred routine for getting all vector values
      PROCEDURE(int_vector_getAll_sub),DEFERRED,PASS :: getAll
      !> Deferred routine for getting a range of vector values
      PROCEDURE(int_vector_getRange_sub),DEFERRED,PASS :: getRange
      GENERIC :: get => getOne,getAll,getRange
  ENDTYPE VectorType    
!
!List of Abstract Interfaces
  !> Explicitly defines the interface for the clear routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_clear_sub(thisVector)
      IMPORT :: VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
    ENDSUBROUTINE int_vector_clear_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the init routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_init_sub(thisVector,Params)
      IMPORT :: SIK,ParamType,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      TYPE(ParamType),INTENT(IN) :: Params
    ENDSUBROUTINE int_vector_init_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the set one routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_setOne_sub(thisVector,i,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE int_vector_setOne_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the set all (scalar) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_setAll_scalar_sub(thisVector,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE int_vector_setAll_scalar_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the set all (array) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_setAll_array_sub(thisVector,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE int_vector_setAll_array_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the set range (scalar) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_setRange_scalar_sub(thisVector,istt,istp,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE int_vector_setRange_scalar_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the set range (array) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_setRange_array_sub(thisVector,istt,istp,setval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE int_vector_setRange_array_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the get (scalar) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_getOne_sub(thisVector,i,getval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(INOUT) :: getval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE int_vector_getOne_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the get (scalar) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_getAll_sub(thisVector,getval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(INOUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE int_vector_getAll_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the get (scalar) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_getRange_sub(thisVector,istt,istp,getval,ierr)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      REAL(SRK),INTENT(INOUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
    ENDSUBROUTINE int_vector_getRange_sub
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
  TYPE,EXTENDS(VectorType) :: PETScVectorType
    !> The values of the vector
#ifdef HAVE_PETSC
    Vec :: b
#endif
    !> creation status
    LOGICAL(SBK) :: isCreated=.FALSE.
    !> assembly status
    LOGICAL(SBK) :: isAssembled=.FALSE.
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
  
  !> The parameter lists to use when validating a parameter list for
  !> initialization
  TYPE(ParamType),SAVE :: reqParamsVectorType,optParamsVectorType
  
  !> Exception Handler for use in VectorTypes
  TYPE(ExceptionHandlerType),POINTER,SAVE :: eVectorType => NULL()

#ifdef HAVE_PETSC
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
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: n
      
      !Validate Input Parameter List 
      validParams=Params
      CALL validRealVTParams(validParams)
      
      !Pull Data from Parameter List
      CALL Params%get('VectorType->n',n)
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      
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
      IF(localalloc) DEALLOCATE(eVectorType)
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
      LOGICAL(SBK) :: localalloc
      INTEGER(SIK) :: n, MPI_Comm_ID
      
      !Validate Input Parameter List 
      validParams=Params
      CALL validPETScVTParams(validParams)
      
      !Pull Data from Parameter List
      CALL Params%get('n',n)
      CALL Params%get('MPI_Comm_ID',MPI_Comm_ID)
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF

#ifdef HAVE_PETSC
      IF(.NOT. thisVector%isInit) THEN
        IF(n < 1) THEN
          CALL eVectorType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (n) must be '// &
              'greater than 0!')
        ELSE
          thisVector%isInit=.TRUE.
          thisVector%n=n
          IF(.NOT.thisVector%isCreated) THEN
            CALL VecCreate(MPI_Comm_ID,thisVector%b,iperr)
            thisVector%isCreated=.TRUE.
          ENDIF
          CALL VecSetSizes(thisVector%b,PETSC_DECIDE,thisVector%n,iperr)
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
      IF(localalloc) DEALLOCATE(eVectorType)
    ENDSUBROUTINE init_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Clears the PETSc vector
!> @param declares the vector type to act on
!>
    SUBROUTINE clear_PETScVectorType(thisVector)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector
      
#ifdef HAVE_PETSC
      thisVector%isInit=.FALSE.
      thisVector%isAssembled=.FALSE.
      thisVector%isCreated=.FALSE.
      thisVector%n=0

      CALL VecDestroy(thisVector%b,iperr)
#else
      CHARACTER(LEN=*),PARAMETER :: myName='setOne_PETScVectorType'
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
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

#ifdef HAVE_PETSC
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF((i <= thisVector%n) .AND. (i > 0)) THEN
          CALL VecSetValue(thisVector%b,i-1,setval,INSERT_VALUES,iperr)
          thisVector%isAssembled=.FALSE.
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='setOne_PETScVectorType'
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
      IF(localalloc) DEALLOCATE(eVectorType)
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

#ifdef HAVE_PETSC
      ierrc=-1
      IF(thisVector%isInit) THEN
        CALL thisVector%assemble(iperr)
        IF(iperr == 0) CALL VecSet(thisVector%b,setval,iperr)
        thisVector%isAssembled=.FALSE.
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='setAll_scalar_PETScVectorType'
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
      IF(localalloc) DEALLOCATE(eVectorType)
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
      
#ifdef HAVE_PETSC
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
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
      IF(localalloc) DEALLOCATE(eVectorType)
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
      
#ifdef HAVE_PETSC
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
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
      IF(localalloc) DEALLOCATE(eVectorType)
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

#ifdef HAVE_PETSC
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
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
      IF(localalloc) DEALLOCATE(eVectorType)
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
      
#ifdef HAVE_PETSC
      ierrc=-1
      IF(thisVector%isInit) THEN
        !
        !Commenting out for now to let iperr get returned for unassembled case
        !IF(.NOT.(thisVector%isAssembled)) THEN
        !  CALL VecAssemblyBegin(thisVector%b,iperr)
        !  CALL VecAssemblyEnd(thisVector%b,iperr)
        !  thisVector%isAssembled=.TRUE.
        !ENDIF
        
        ierrc=-2
        IF((i <= thisVector%n) .AND. (i > 0)) THEN
          CALL VecGetValues(thisVector%b,1,i-1,getval,iperr)
          ierrc=iperr
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='getOne_PETScVectorType'
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
      IF(localalloc) DEALLOCATE(eVectorType)
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

#ifdef HAVE_PETSC
      INTEGER(SIK) :: i
      
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-3
        IF(SIZE(getval) == thisVector%n) THEN
          !
          !Commenting out for now to let iperr get returned for unassembled case
          !IF(.NOT.(thisVector%isAssembled)) THEN
          !  CALL VecAssemblyBegin(thisVector%b,iperr)
          !  CALL VecAssemblyEnd(thisVector%b,iperr)
          !  thisVector%isAssembled=.TRUE.
          !ENDIF
          
          DO i=1,thisVector%n
            CALL VecGetValues(thisVector%b,1,i-1,getval(i),iperr)
          ENDDO
          ierrc=iperr
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
#else
      CHARACTER(LEN=*),PARAMETER :: myName='getAll_PETScVectorType'
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
      IF(localalloc) DEALLOCATE(eVectorType)
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
      
#ifdef HAVE_PETSC
      INTEGER(SIK) :: i
      
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n) THEN
          ierrc=-3
          IF(istp-istt+1 == SIZE(getval)) THEN
            !
            !Commenting out for now to let iperr get returned for unassembled case
            !IF(.NOT.(thisVector%isAssembled)) THEN
            !  CALL VecAssemblyBegin(thisVector%b,iperr)
            !  CALL VecAssemblyEnd(thisVector%b,iperr)
            !  thisVector%isAssembled=.TRUE.
            !ENDIF
            
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
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
      IF(localalloc) DEALLOCATE(eVectorType)
#endif
    ENDSUBROUTINE getRange_PETScVectorType
!
!-------------------------------------------------------------------------------
    SUBROUTINE assemble_PETScVectorType(thisVector,ierr)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
#ifdef HAVE_PETSC
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
      LOGICAL(SBK) :: localalloc
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      CALL eVectorType%raiseFatalError('Incorrect call to '// &
         modName//'::'//myName//' - PETSc not enabled.  You will'// &
         'need to recompile with PETSc enabled to use this feature.')
      IF(localalloc) DEALLOCATE(eVectorType)
#endif
    ENDSUBROUTINE assemble_PETScVectorType
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
      LOGICAL(SBK) :: localalloc

      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF

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
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
#ifdef HAVE_PETSC
        CALL VecNorm(thisVector%b,NORM_1,r,iperr)
#else
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - PETSc not enabled.  You will'// &
           'need to recompile with PETSc enabled to use this feature.')
#endif
      ENDSELECT
      
      IF(localalloc) DEALLOCATE(eVectorType)
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
      REAL(SRK) :: alpha=1.
      LOGICAL(SBK) :: localalloc
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      
      IF (PRESENT(a)) alpha=a
      
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
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        SELECTTYPE(newVector); TYPE IS(PETScVectorType)
#ifdef HAVE_PETSC
          CALL VecAXPY(newVector%b,alpha,thisVector%b,iperr)
#else
          CALL eVectorType%raiseFatalError('Incorrect call to '// &
             modName//'::'//myName//' - PETSc not enabled.  You will'// &
             'need to recompile with PETSc enabled to use this feature.')
#endif
        ENDSELECT
      ENDSELECT
      
      IF(localalloc) DEALLOCATE(eVectorType)
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
      LOGICAL(SBK) :: localalloc
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      
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
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        SELECTTYPE(newVector); TYPE IS(PETScVectorType)
          SELECTTYPE(aVector); TYPE IS(PETScVectorType)
#ifdef HAVE_PETSC
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
      ENDSELECT
      
      SELECTTYPE(newVector); TYPE IS(PETScVectorType)
        CALL newVector%set(tmpnew)
      ENDSELECT
      
      DEALLOCATE(tmpthis)
      DEALLOCATE(tmpnew)
      DEALLOCATE(tmpa)
      IF(localalloc) DEALLOCATE(eVectorType)
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
      CLASS(VectorType),INTENT(INOUT)  :: thisVector
      CLASS(VectorType),INTENT(INOUT)  :: newVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incy
      LOGICAL(SBK) :: localalloc
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      
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
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        SELECTTYPE(newVector); TYPE IS(PETScVectorType)
#ifdef HAVE_PETSC
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
          IF(iperr == 0) CALL VecCopy(thisVector%b,newVector%b,iperr)
#else
          CALL eVectorType%raiseFatalError('Incorrect call to '// &
             modName//'::'//myName//' - PETSc not enabled.  You will'// &
             'need to recompile with PETSc enabled to use this feature.')
#endif
        ENDSELECT
      ENDSELECT
      
      IF(localalloc) DEALLOCATE(eVectorType)
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
      LOGICAL(SBK) :: localalloc
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      
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
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        SELECTTYPE(thatVector); TYPE IS(PETScVectorType)
#ifdef HAVE_PETSC
          CALL VecTDot(thisVector%b,thatVector%b,r,iperr)
#else
          CALL eVectorType%raiseFatalError('Incorrect call to '// &
             modName//'::'//myName//' - PETSc not enabled.  You will'// &
             'need to recompile with PETSc enabled to use this feature.')
#endif
        ENDSELECT
      ENDSELECT
      
      IF(localalloc) DEALLOCATE(eVectorType)
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
      LOGICAL(SBK) :: localalloc
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        CALL thisVector%get(tmpthis)
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
#ifdef HAVE_PETSC
        ALLOCATE(tmpthis(thisVector%n))
        CALL thisVector%get(tmpthis)
#else
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - PETSc not enabled.  You will'// &
           'need to recompile with PETSc enabled to use this feature.')
#endif
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
      IF(localalloc) DEALLOCATE(eVectorType)
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
      LOGICAL(SBK) :: localalloc
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        CALL thisVector%get(tmpthis)
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
#ifdef HAVE_PETSC
        ALLOCATE(tmpthis(thisVector%n))
        CALL thisVector%get(tmpthis)
#else
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - PETSc not enabled.  You will'// &
           'need to recompile with PETSc enabled to use this feature.')
#endif
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
      IF(localalloc) DEALLOCATE(eVectorType)
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
      LOGICAL(SBK) :: localalloc
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      
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
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
#ifdef HAVE_PETSC
        CALL VecNorm(thisVector%b,NORM_2,norm2,iperr)
#else
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - PETSc not enabled.  You will'// &
           'need to recompile with PETSc enabled to use this feature.')
#endif
      ENDSELECT

      IF(localalloc) DEALLOCATE(eVectorType)
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
      LOGICAL(SBK) :: localalloc
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      
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
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
#ifdef HAVE_PETSC
        CALL thisVector%assemble(iperr)
        IF(iperr == 0) CALL VecScale(thisVector%b,a,iperr)
#else
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - PETSc not enabled.  You will'// &
           'need to recompile with PETSc enabled to use this feature.')
#endif
      ENDSELECT
      
      IF(localalloc) DEALLOCATE(eVectorType)
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
      LOGICAL(SBK) :: localalloc
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(aVector); TYPE IS(RealVectorType)
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpa(aVector%n))
          CALL thisVector%get(tmpthis)
          CALL aVector%get(tmpa)
        ENDSELECT
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        SELECTTYPE(aVector); TYPE IS(PETScVectorType)
#ifdef HAVE_PETSC
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
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        CALL thisVector%set(tmpthis)
      ENDSELECT
      
      DEALLOCATE(tmpthis)
      DEALLOCATE(tmpa)
      IF(localalloc) DEALLOCATE(eVectorType)
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
      LOGICAL(SBK) :: localalloc
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      
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
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        SELECTTYPE(thatVector); TYPE IS(PETScVectorType)
#ifdef HAVE_PETSC
          CALL thisVector%assemble(iperr)
          IF(iperr == 0) CALL thatVector%assemble(iperr)
          IF(iperr == 0) CALL VecSwap(thisVector%b,thatVector%b,iperr)
#else
          CALL eVectorType%raiseFatalError('Incorrect call to '// &
             modName//'::'//myName//' - PETSc not enabled.  You will'// &
             'need to recompile with PETSc enabled to use this feature.')
#endif
        ENDSELECT
      ENDSELECT

      IF(localalloc) DEALLOCATE(eVectorType)
    ENDSUBROUTINE swap_VectorType  
!
!-------------------------------------------------------------------------------
!> @brief Subroutine that sets up the default parameter lists for the Real 
!>        Vector Type and then validates them against the input parameter list.
!> @param thisParams The parameter list that was input to the initialization
!>        routine.
!> The required parameters for the Real Vector Type are:
!>        '...->n',SIK
!> The optional parameters for the Real Vector Type do not exist.
!>
    SUBROUTINE validRealVTParams(thisParams)
      TYPE(ParamType),INTENT(INOUT) :: thisParams
      INTEGER(SIK) :: n
      
      !Setup the required and optional parameter lists
      n=1_SIK
      CALL reqParamsVectorType%add('VectorType->n',n)
      
      !There are no optional parameters at this time.
      !Validate against the reqParams and OptParams
      CALL thisParams%validate(reqParamsVectorType)
      CALL reqParamsVectorType%clear()
    ENDSUBROUTINE validRealVTParams
!
!-------------------------------------------------------------------------------
!> @brief Subroutine that sets up the default parameter lists for the PETSc
!>        Vector Type and then validates them against the input parameter list.
!> @param thisParams The parameter list that was input to the initialization
!>        routine.
!> The required parameters for the PETSc Vector Type are:
!>        '...->n',SIK
!>        '...->MPI_Comm_ID',SIK
!> The optional parameters for the PETSc Vector Type do not exist.
!>
    SUBROUTINE validPETScVTParams(thisParams)
      TYPE(ParamType),INTENT(INOUT) :: thisParams
      INTEGER(SIK) :: n,MPI_Comm
      
      !Setup the required and optional parameter lists
      n=1_SIK
      MPI_Comm=1_SIK
      CALL reqParamsVectorType%clear()
      CALL reqParamsVectorType%add('VectorType->n',n)
      CALL reqParamsVectorType%add('VectorType->MPI_Comm_ID',MPI_Comm)
      
      !There are no optional parameters at this time.
      !Validate against the reqParams and OptParams
      CALL thisParams%validate(reqParamsVectorType)
      CALL reqParamsVectorType%clear()
    ENDSUBROUTINE validPETScVTParams
!
ENDMODULE VectorTypes
