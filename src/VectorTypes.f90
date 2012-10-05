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
    SUBROUTINE int_vector_clear_sub(vector)
      IMPORT :: VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
    ENDSUBROUTINE int_vector_clear_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the init routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_init_sub(vector,n)
      IMPORT :: SIK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: n
    ENDSUBROUTINE int_vector_init_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the set one routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_setOne_sub(vector,i,setval)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
    ENDSUBROUTINE int_vector_setOne_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the set all (scalar) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_setAll_scalar_sub(vector,setval)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      REAL(SRK),INTENT(IN) :: setval
    ENDSUBROUTINE int_vector_setAll_scalar_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the set all (array) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_setAll_array_sub(vector,setval)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      REAL(SRK),INTENT(IN) :: setval(:)
    ENDSUBROUTINE int_vector_setAll_array_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the set range (scalar) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_setRange_scalar_sub(vector,istt,istp,setval)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: istt,istp
      REAL(SRK),INTENT(IN) :: setval
    ENDSUBROUTINE int_vector_setRange_scalar_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the set range (array) routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_setRange_array_sub(vector,istt,istp,setval)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: istt,istp
      REAL(SRK),INTENT(IN) :: setval(:)
    ENDSUBROUTINE int_vector_setRange_array_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the get (scalar) routine of all vector types
  ABSTRACT INTERFACE
    FUNCTION int_vector_getOne_sub(vector,i) RESULT(getval)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK) :: getval
    ENDFUNCTION int_vector_getOne_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the get (scalar) routine of all vector types
  ABSTRACT INTERFACE
    FUNCTION int_vector_getAll_sub(vector) RESULT(getval)
      IMPORT :: SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      REAL(SRK),ALLOCATABLE :: getval(:)
    ENDFUNCTION int_vector_getAll_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the get (scalar) routine of all vector types
  ABSTRACT INTERFACE
    FUNCTION int_vector_getRange_sub(vector,istt,istp) RESULT(getval)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: istt,istp
      REAL(SRK),ALLOCATABLE :: getval(:)
    ENDFUNCTION int_vector_getRange_sub
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
  
  !> Exception Handler for use in VectorTypes
  TYPE(ExceptionHandlerType),POINTER,SAVE :: eVectorType => NULL()
  
  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='VECTORTYPES'
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the real vector
!> @param declares the vector type to act on
!> @param n the number of rows
!>
    SUBROUTINE init_RealVectorType(vector,n)
      CHARACTER(LEN=*),PARAMETER :: myName='init_RealVectorType'
      CLASS(RealVectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: n
      LOGICAL(SBK) :: localalloc
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      IF(.NOT. vector%isInit) THEN
        IF(n < 1) THEN
          CALL eVectorType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (n) must be '// &
              'greater than 0!')
        ELSE
          vector%isInit=.TRUE.
          vector%n=n
          CALL dmallocA(vector%b,n)
        ENDIF
      ELSE
        CALL eVectorType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - VectorType already initialized')
      ENDIF
      IF(localalloc) DEALLOCATE(eVectorType)
    ENDSUBROUTINE init_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Initializes the PETSc vector
!> @param declares the vector type to act on
!> @param n the number of rows
!>
    SUBROUTINE init_PETScVectorType(vector,n)
      CHARACTER(LEN=*),PARAMETER :: myName='init_PETScVectorType'
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: n
      LOGICAL(SBK) :: localalloc
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      
      !Error checking of subroutine input
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eVectorType)) THEN
        localalloc=.TRUE.
        ALLOCATE(eVectorType)
      ENDIF
      IF(.NOT. vector%isInit) THEN
        IF(n < 1) THEN
          CALL eVectorType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (n) must be '// &
              'greater than 0!')
        ELSE
          vector%isInit=.TRUE.
          vector%n=n
          ! will need to change MPI_COMM_WORLD
          IF(.NOT.vector%isCreated) THEN
            CALL VecCreate(MPI_COMM_WORLD,vector%b,ierr)
            vector%isCreated=.TRUE.
          ENDIF
          CALL VecSetSizes(vector%b,PETSC_DECIDE,vector%n,ierr)
          CALL VecSetType(vector%b,VECMPI,ierr)
          CALL VecSetFromOptions(vector%b,ierr)
        ENDIF
      ELSE
        CALL eVectorType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - VectorType already initialized')
      ENDIF
      IF(localalloc) DEALLOCATE(eVectorType)
#else
      CALL eVectorType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - PETSc not enabled')
#endif
    ENDSUBROUTINE init_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Clears the real vector
!> @param declares the vector type to act on
!>
    SUBROUTINE clear_RealVectorType(vector)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_RealVectorType'
      CLASS(RealVectorType),INTENT(INOUT) :: vector
      vector%isInit=.FALSE.
      vector%n=0
      IF(ALLOCATED(vector%b)) CALL demallocA(vector%b)
    ENDSUBROUTINE clear_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Clears the PETSc vector
!> @param declares the vector type to act on
!>
    SUBROUTINE clear_PETScVectorType(vector)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_PETScVectorType'
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      vector%isInit=.FALSE.
      vector%isAssembled=.FALSE.
      vector%isCreated=.FALSE.
      vector%n=0
      CALL VecDestroy(vector%b,ierr)
#else
      CALL eVectorType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - PETSc not enabled')
#endif
    ENDSUBROUTINE clear_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets one value in the real vector
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!> @param setval the value to be set
!>
    SUBROUTINE setOne_RealVectorType(vector,i,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='setOne_RealVectorType'
      CLASS(RealVectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
      IF(vector%isInit) THEN
        IF((i <= vector%n) .AND. (i > 0)) THEN
          vector%b(i)=setval
        ENDIF
      ENDIF
    ENDSUBROUTINE setOne_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets one value in the real vector
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!> @param setval the value to be set
!>
    SUBROUTINE setOne_PETScVectorType(vector,i,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='setOne_PETScVectorType'
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      IF(vector%isInit) THEN
        IF((i <= vector%n) .AND. (i > 0)) THEN
          CALL VecSetValue(vector%b,i-1,setval,INSERT_VALUES,ierr)
          CALL VecAssemblyBegin(vector%b,ierr)
          CALL VecAssemblyEnd(vector%b,ierr)
        ENDIF
      ENDIF
      vector%isAssembled=.FALSE.
#else
      CALL eVectorType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - PETSc not enabled')
#endif
    ENDSUBROUTINE setOne_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all values in the real vector with a scalar value
!> @param declares the vector type to act on
!> @param setval the scalar value to be set
!>
    SUBROUTINE setAll_scalar_RealVectorType(vector,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='setAll_scalar_RealVectorType'
      CLASS(RealVectorType),INTENT(INOUT) :: vector
      REAL(SRK),INTENT(IN) :: setval
      IF(vector%isInit) THEN
          vector%b=setval
      ENDIF
    ENDSUBROUTINE setAll_scalar_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all values in the PETSc vector with a scalar value
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!>
    SUBROUTINE setAll_scalar_PETScVectorType(vector,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='setAll_scalar_PETScVectorType'
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK) :: i
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      IF(vector%isInit) THEN
        DO i=1,vector%n
          CALL VecSetValue(vector%b,i-1,setval,INSERT_VALUES,ierr)
        ENDDO
      ENDIF
      vector%isAssembled=.FALSE.
#else
      CALL eVectorType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - PETSc not enabled')
#endif
    ENDSUBROUTINE setAll_scalar_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all the values in the real vector
!> @param declare the vector type to act on
!> @param setval the array of values to be set
!>
    SUBROUTINE setAll_array_RealVectorType(vector,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='setAll_array_RealVectorType'
      CLASS(RealVectorType),INTENT(INOUT) :: vector
      REAL(SRK),INTENT(IN) :: setval(:)
      IF(vector%isInit .AND. SIZE(setval)==vector%n) THEN
        vector%b=setval
      ENDIF
    ENDSUBROUTINE setAll_array_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all the values in the PETSc vector with an array of values
!> @param declare the vector type to act on
!> @param setval the array of values to be set
!>
    SUBROUTINE setAll_array_PETScVectorType(vector,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='setAll_array_PETScVectorType'
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK) :: i
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      IF(vector%isInit .AND. SIZE(setval)==vector%n) THEN
        DO i=1,vector%n
          CALL VecSetValue(vector%b,i-1,setval(i),INSERT_VALUES,ierr)
        ENDDO
      ENDIF
      vector%isAssembled=.FALSE.
#else
      CALL eVectorType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - PETSc not enabled')
#endif
    ENDSUBROUTINE setAll_array_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the real vector with a scalar value 
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE setRange_scalar_RealVectorType(vector,istt,istp,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='setRange_scalar_RealVectorType'
      CLASS(RealVectorType),INTENT(INOUT) :: vector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(IN) :: istt,istp
      IF(vector%isInit) THEN
        vector%b(istt:istp)=setval
      ENDIF
    ENDSUBROUTINE setRange_scalar_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the PETSc vector with a scalar value 
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE setRange_scalar_PETScVectorType(vector,istt,istp,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='setRange_scalar_PETScVectorType'
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(IN) :: istt,istp
      INTEGER(SIK) :: i
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      IF(vector%isInit) THEN
        DO i=istt,istp
          CALL VecSetValue(vector%b,i-1,setval,INSERT_VALUES,ierr)
        ENDDO
      ENDIF
      vector%isAssembled=.FALSE.
#else
      CALL eVectorType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - PETSc not enabled')
#endif
    ENDSUBROUTINE setRange_scalar_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the real vector with an array of values 
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE setRange_array_RealVectorType(vector,istt,istp,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='setRange_array_RealVectorType'
      CLASS(RealVectorType),INTENT(INOUT) :: vector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(IN) :: istt,istp
      IF(vector%isInit) THEN
        IF((istt <= vector%n) .AND. (istt > 0) .AND. &
           (istp <= vector%n) .AND. (istp > 0)) THEN
          vector%b(istt:istp)=setval
        ENDIF
      ENDIF
    ENDSUBROUTINE setRange_array_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the PETSc vector with an array of values 
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE setRange_array_PETScVectorType(vector,istt,istp,setval)
      CHARACTER(LEN=*),PARAMETER :: myName='setRange_array_PETScVectorType'
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(IN) :: istt,istp
      INTEGER(SIK) :: i
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      IF(vector%isInit) THEN
        IF((istt <= vector%n) .AND. (istt > 0) .AND. &
           (istp <= vector%n) .AND. (istp > 0)) THEN
          DO i=istt,istp
            CALL VecSetValue(vector%b,i-1,setval(i-istt+1),INSERT_VALUES,ierr)
          ENDDO
        ENDIF
      ENDIF
      vector%isAssembled=.FALSE.
#else
      CALL eVectorType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - PETSc not enabled')
#endif
    ENDSUBROUTINE setRange_array_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets one value in the real vector
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!>
!> This routine gets the values of the real vector.  If the location is out of
!> bounds, then -1051.0 is returned (-1051.0 is an arbitrarily chosen key).
!>
    FUNCTION getOne_RealVectorType(vector,i) RESULT(getval)
      CHARACTER(LEN=*),PARAMETER :: myName='getOne_RealVectorType'
      CLASS(RealVectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK) :: getval
      
      getval=0.0_SRK
      IF(vector%isInit) THEN
        IF((i <= vector%n) .AND. (i > 0)) THEN
          getval=vector%b(i)
        ELSE
          getval=-1051._SRK
        ENDIF
      ENDIF
    ENDFUNCTION getOne_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets one values in the PETSc vector
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!>
!> This routine gets the values of the PETSc vector.  If the location is out of
!> bounds, then -1051.0 is returned (-1051.0 is an arbitrarily chosen key).
!>
    FUNCTION getOne_PETScVectorType(vector,i) RESULT(getval)
      CHARACTER(LEN=*),PARAMETER :: myName='getOne_PETScVectorType'
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK) :: getval
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr

      getval=0.0_SRK
      IF(vector%isInit) THEN
        ! assemble matrix if necessary
        IF (.NOT.(vector%isAssembled)) THEN
          CALL VecAssemblyBegin(vector%b,ierr)
          CALL VecAssemblyEnd(vector%b,ierr)
          vector%isAssembled=.TRUE.
        ENDIF
        IF((i <= vector%n) .AND. (i > 0)) THEN
          CALL VecGetValues(vector%b,1,i-1,getval,ierr)
        ELSE
          getval=-1051._SRK
        ENDIF
      ENDIF
#else
      CALL eVectorType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - PETSc not enabled')
#endif
    ENDFUNCTION getOne_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets all values in the real vector
!> @param declares the vector type to act on
!>
!> This routine gets the values of the real vector.  If the location is out of
!> bounds, then -1051.0 is returned (-1051.0 is an arbitrarily chosen key).
!>
    FUNCTION getAll_RealVectorType(vector) RESULT(getval)
      CHARACTER(LEN=*),PARAMETER :: myName='getAll_RealVectorType'
      CLASS(RealVectorType),INTENT(INOUT) :: vector
      REAL(SRK),ALLOCATABLE :: getval(:)
      
      IF(vector%isInit) THEN
        ALLOCATE(getval(vector%n))
        getval=vector%b
      ELSE
        ALLOCATE(getval(1))
        getval=-1051._SRK
      ENDIF
    ENDFUNCTION getAll_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets all values in the PETSc vector
!> @param declares the vector type to act on
!>
!> This routine gets the values of the PETSc vector.  If the location is out of
!> bounds, then -1051.0 is returned (-1051.0 is an arbitrarily chosen key).
!>
    FUNCTION getAll_PETScVectorType(vector) RESULT(getval)
      CHARACTER(LEN=*),PARAMETER :: myName='getAll_PETScVectorType'
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
      INTEGER(SIK) :: i
      REAL(SRK),ALLOCATABLE :: getval(:)
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      
      IF(vector%isInit) THEN
        ALLOCATE(getval(vector%n))
        ! assemble matrix if necessary
        IF (.NOT.(vector%isAssembled)) THEN
          CALL VecAssemblyBegin(vector%b,ierr)
          CALL VecAssemblyEnd(vector%b,ierr)
          vector%isAssembled=.TRUE.
        ENDIF
        DO i=1,vector%n
          CALL VecGetValues(vector%b,1,i-1,getval(i),ierr)
        ENDDO
      ELSE
        ALLOCATE(getval(1))
        getval=-1051._SRK
      ENDIF
#else
      CALL eVectorType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - PETSc not enabled')
#endif
    ENDFUNCTION getAll_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets a range of values in the real vector
!> @param declares the vector type to act on
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
!> This routine gets the values of the real vector.  If the location is out of
!> bounds, then -1051.0 is returned (-1051.0 is an arbitrarily chosen key).
!>
    FUNCTION getRange_RealVectorType(vector,istt,istp) RESULT(getval)
      CHARACTER(LEN=*),PARAMETER :: myName='getRange_RealVectorType'
      CLASS(RealVectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: istt,istp
      REAL(SRK),ALLOCATABLE :: getval(:)
      
      IF(vector%isInit) THEN
        IF((istt <= vector%n) .AND. (istt > 0) .AND. &
             (istp <= vector%n) .AND. (istp > 0)) THEN
          ALLOCATE(getval(istp-istt+1))
          getval=vector%b(istt:istp)
        ELSE
          ALLOCATE(getval(1))
          getval=-1051._SRK
        ENDIF
      ELSE
        IF((istt > 0) .AND. (istp > 0) .AND. istp>istt) THEN
          ALLOCATE(getval(istp-istt+1))
          getval=-1051._SRK
        ELSE
          ALLOCATE(getval(1))
          getval=-1051._SRK
        ENDIF
      ENDIF

    ENDFUNCTION getRange_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets a range of  values in the PETSc vector
!> @param declares the vector type to act on
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
!> This routine gets the values of the PETSc vector.  If the location is out of
!> bounds, then -1051.0 is returned (-1051.0 is an arbitrarily chosen key).
!>
    FUNCTION getRange_PETScVectorType(vector,istt,istp) RESULT(getval)
      CHARACTER(LEN=*),PARAMETER :: myName='getRange_PETScVectorType'
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: istt,istp
      INTEGER(SIK) :: i
      REAL(SRK),ALLOCATABLE :: getval(:)
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr

      IF((istt <= vector%n) .AND. (istt > 0) .AND. &
           (istp <= vector%n) .AND. (istp > 0)) THEN
        ALLOCATE(getval(istp-istt+1))
        getval=0.0_SRK
        ! assemble matrix if necessary
        IF (.NOT.(vector%isAssembled)) THEN
          CALL VecAssemblyBegin(vector%b,ierr)
          CALL VecAssemblyEnd(vector%b,ierr)
          vector%isAssembled=.TRUE.
        ENDIF
        IF(vector%isInit) THEN
          DO i=istt,istp
            CALL VecGetValues(vector%b,1,i-1,getval(i-istt+1),ierr)
          ENDDO
        ENDIF
      ELSE
        IF((istt > 0) .AND. (istp > 0) .AND. istp>istt) THEN
          ALLOCATE(getval(istp-istt+1))
          getval=-1051._SRK
        ELSE
          ALLOCATE(getval(1))
          getval=-1051._SRK
        ENDIF
      ENDIF
#else
      CALL eVectorType%raiseError('Incorrect call to '// &
              modName//'::'//myName//' - PETSc not enabled')
#endif
    ENDFUNCTION getRange_PETScVectorType
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
      CLASS(VectorType),INTENT(INOUT)     :: thisVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      REAL(SRK),ALLOCATABLE :: tmpthis(:)
      REAL(SRK) :: r

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        tmpthis=thisVector%get()
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        tmpthis=thisVector%get()
      ENDSELECT
      
      IF(PRESENT(n) .AND. PRESENT(incx)) THEN
        r = BLAS1_asum(n,tmpthis,incx)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        r = BLAS1_asum(n,tmpthis)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
        r = BLAS1_asum(tmpthis,incx)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        r = BLAS1_asum(tmpthis)
      ENDIF
      DEALLOCATE(tmpthis)
            
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
      CLASS(VectorType),INTENT(INOUT)  :: thisVector
      CLASS(VectorType),INTENT(INOUT)  :: newVector
      REAL(SRK),INTENT(IN),OPTIONAL :: a
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incy
      REAL(SRK),ALLOCATABLE :: tmpthis(:),tmpnew(:)
      REAL(SRK) :: alpha=1.
      
      IF (PRESENT(a)) alpha=a
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(newVector); TYPE IS(RealVectorType)
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpnew(newVector%n))
          tmpthis=thisVector%get()
          tmpnew=newVector%get()
        ENDSELECT
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        SELECTTYPE(newVector); TYPE IS(PETScVectorType)
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpnew(newVector%n))
          tmpthis=thisVector%get()
          tmpnew=newVector%get()
        ENDSELECT
      ENDSELECT
      
      IF(PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
        CALL BLAS1_axpy(n,alpha,tmpthis,incx,tmpnew,incy)
      ELSEIF(PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_axpy(n,alpha,tmpthis,tmpnew,incx)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
        CALL BLAS1_axpy(n,alpha,tmpthis,tmpnew,incy)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_axpy(n,alpha,tmpthis,tmpnew)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_axpy(alpha,tmpthis,tmpnew)
      ENDIF
      
      SELECTTYPE(newVector); TYPE IS(RealVectorType)
        CALL newVector%set(tmpnew)
      ENDSELECT
      
      SELECTTYPE(newVector); TYPE IS(PETScVectorType)
        CALL newVector%set(tmpnew)
      ENDSELECT
      
      DEALLOCATE(tmpthis)
      DEALLOCATE(tmpnew)
      
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
            tmpthis=thisVector%get()
            tmpnew=newVector%get()
            tmpa=aVector%get()
          ENDSELECT
        ENDSELECT
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        SELECTTYPE(newVector); TYPE IS(PETScVectorType)
          SELECTTYPE(aVector); TYPE IS(PETScVectorType)
            ALLOCATE(tmpthis(thisVector%n))
            ALLOCATE(tmpnew(newVector%n))
            ALLOCATE(tmpa(aVector%n))
            tmpthis=thisVector%get()
            tmpnew=newVector%get()
            tmpa=aVector%get()
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
      CLASS(VectorType),INTENT(INOUT)  :: thisVector
      CLASS(VectorType),INTENT(INOUT)  :: newVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incy
      REAL(SRK),ALLOCATABLE :: tmpthis(:),tmpnew(:)
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(newVector); TYPE IS(RealVectorType)
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpnew(newVector%n))
          tmpthis=thisVector%get()
          tmpnew=newVector%get()
        ENDSELECT
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        SELECTTYPE(newVector); TYPE IS(PETScVectorType)
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpnew(newVector%n))
          tmpthis=thisVector%get()
          tmpnew=newVector%get()
        ENDSELECT
      ENDSELECT

      IF(PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
        CALL BLAS1_copy(n,tmpthis,incx,tmpnew,incy)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
        CALL BLAS1_copy(tmpthis,incx,tmpnew,incy)
      ELSEIF(PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_copy(n,tmpthis,tmpnew,incx)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
        CALL BLAS1_copy(n,tmpthis,tmpnew,incy)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_copy(tmpthis,tmpnew,incx)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
        CALL BLAS1_copy(tmpthis,tmpnew,incy)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_copy(n,tmpthis,tmpnew)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_copy(tmpthis,tmpnew)
      ENDIF
      
      SELECTTYPE(newVector); TYPE IS(RealVectorType)
        CALL newVector%set(tmpnew)
      ENDSELECT
      
      SELECTTYPE(newVector); TYPE IS(PETScVectorType)
        CALL newVector%set(tmpnew)
      ENDSELECT
      
      DEALLOCATE(tmpthis)
      DEALLOCATE(tmpnew)

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
      CLASS(VectorType),INTENT(INOUT)     :: thisVector
      CLASS(VectorType),INTENT(INOUT)     :: thatVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incy
      REAL(SRK),ALLOCATABLE :: tmpthis(:),tmpthat(:)
      REAL(SRK) :: r
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(thatVector); TYPE IS(RealVectorType)
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpthat(thatVector%n))
          tmpthis=thisVector%get()
          tmpthat=thatVector%get()
        ENDSELECT
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        SELECTTYPE(thatVector); TYPE IS(PETScVectorType)
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpthat(thatVector%n))
          tmpthis=thisVector%get()
          tmpthat=thatVector%get()
        ENDSELECT
      ENDSELECT
      
      IF(PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
        r = BLAS1_dot(n,tmpthis,incx,tmpthat,incy)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
        r = BLAS1_dot(tmpthis,incx,tmpthat,incy)
      ELSEIF(PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        r = BLAS1_dot(n,tmpthis,tmpthat,incx)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
        r = BLAS1_dot(n,tmpthis,tmpthat,incy)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        r = BLAS1_dot(n,tmpthis,tmpthat)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        r = BLAS1_dot(tmpthis,tmpthat,incx)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
        r = BLAS1_dot(tmpthis,tmpthat,incy)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        r = BLAS1_dot(tmpthis,tmpthat)
      ENDIF
      DEALLOCATE(tmpthis)
      DEALLOCATE(tmpthat)

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
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      REAL(SRK),ALLOCATABLE :: tmpthis(:)
      INTEGER(SIK) :: imax
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        tmpthis=thisVector%get()
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        tmpthis=thisVector%get()
      ENDSELECT

      IF(PRESENT(n) .AND. PRESENT(incx)) THEN
        imax = BLAS1_iamax(n,tmpthis,incx)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
        imax = BLAS1_iamax(tmpthis,incx)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        imax = BLAS1_iamax(n,tmpthis)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        imax = BLAS1_iamax(tmpthis)
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
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      REAL(SRK),ALLOCATABLE :: tmpthis(:)
      INTEGER(SIK) :: imin
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        tmpthis=thisVector%get()
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        tmpthis=thisVector%get()
      ENDSELECT
      
      IF(PRESENT(n) .AND. PRESENT(incx)) THEN
        imin = BLAS1_iamin(n,tmpthis,incx)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
        imin = BLAS1_iamin(tmpthis,incx)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        imin = BLAS1_iamin(n,tmpthis)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        imin = BLAS1_iamin(tmpthis)
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
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      REAL(SRK),ALLOCATABLE :: tmpthis(:)
      REAL(SRK) :: norm2
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        tmpthis=thisVector%get()
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        tmpthis=thisVector%get()
      ENDSELECT
      
      IF(PRESENT(n) .AND. PRESENT(incx)) THEN
        norm2 = BLAS1_nrm2(n,tmpthis,incx)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
        norm2 = BLAS1_nrm2(tmpthis,incx)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        norm2 = BLAS1_nrm2(n,tmpthis)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        norm2 = BLAS1_nrm2(tmpthis)
      ENDIF
      DEALLOCATE(tmpthis)

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
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: a
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      REAL(SRK),ALLOCATABLE :: tmpthis(:)
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        tmpthis=thisVector%get()
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        tmpthis=thisVector%get()
      ENDSELECT
      
      IF(PRESENT(n) .AND. PRESENT(incx)) THEN
        CALL BLAS1_scal(n,a,tmpthis,incx)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
        CALL BLAS1_scal(a,tmpthis,incx)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        CALL BLAS1_scal(n,a,tmpthis)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
        CALL BLAS1_scal(a,tmpthis)
      ENDIF
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        CALL thisVector%set(tmpthis)
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        CALL thisVector%set(tmpthis)
      ENDSELECT
      
      DEALLOCATE(tmpthis)

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
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      CLASS(VectorType),INTENT(INOUT) :: aVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      REAL(SRK),ALLOCATABLE :: tmpthis(:),tmpa(:)
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(aVector); TYPE IS(RealVectorType)
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpa(aVector%n))
          tmpthis=thisVector%get()
          tmpa=aVector%get()
        ENDSELECT
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        SELECTTYPE(aVector); TYPE IS(PETScVectorType)
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpa(aVector%n))
          tmpthis=thisVector%get()
          tmpa=aVector%get()
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
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      CLASS(VectorType),INTENT(INOUT) :: thatVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incy
      REAL(SRK),ALLOCATABLE :: tmpthis(:),tmpthat(:)
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        SELECTTYPE(thatVector); TYPE IS(PETScVectorType)
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpthat(thatVector%n))
          tmpthis=thisVector%get()
          tmpthat=thatVector%get()
        ENDSELECT
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(thatVector); TYPE IS(RealVectorType)
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpthat(thatVector%n))
          tmpthis=thisVector%get()
          tmpthat=thatVector%get()
        ENDSELECT
      ENDSELECT
      
      IF(PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
        CALL BLAS1_swap(n,tmpthis,incx,tmpthat,incy)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
        CALL BLAS1_swap(tmpthis,incx,tmpthat,incy)
      ELSEIF(PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_swap(n,tmpthis,tmpthat,incx)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
        CALL BLAS1_swap(n,tmpthis,tmpthat,incy)
      ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_swap(tmpthis,tmpthat,incx)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
        CALL BLAS1_swap(tmpthis,tmpthat,incy)
      ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_swap(n,tmpthis,tmpthat)
      ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
        CALL BLAS1_swap(tmpthis,tmpthat)
      ENDIF
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(thatVector); TYPE IS(RealVectorType)
          CALL thisVector%set(tmpthis)
          CALL thatVector%set(tmpthat)
        ENDSELECT
      ENDSELECT
      
      SELECTTYPE(thisVector); TYPE IS(PETScVectorType)
        SELECTTYPE(thatVector); TYPE IS(PETScVectorType)
          CALL thisVector%set(tmpthis)
          CALL thatVector%set(tmpthat)
        ENDSELECT
      ENDSELECT
      
      DEALLOCATE(tmpthis)
      DEALLOCATE(tmpthat)

    ENDSUBROUTINE swap_VectorType  

ENDMODULE VectorTypes
