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
!>   CALL vector%clear()
!> ENDPROGRAM ExampleVector
!> @endcode
!>
!> @author Shane Stimpson (adapted from MatrixTypes)
!>   @date 08/20/2012
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE VectorTypes

  USE IntrType
  USE ExceptionHandler
  USE Allocs
  IMPLICIT NONE

#ifdef HAVE_PETSC
#include <finclude/petsc.h>
#define IS IS !petscisdef.h defines the keyword IS, and it needs to be reset
#endif

  PRIVATE
!
! List of public members
  PUBLIC :: eVectorType
  PUBLIC :: VectorType
  PUBLIC :: RealVectorType
  PUBLIC :: PETScVectorType
  
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
      PROCEDURE(int_vector_sub),DEFERRED,PASS :: clear
      !> Deferred routine for initializing the vector
      PROCEDURE(int_vector_init_sub),DEFERRED,PASS :: init
      !> Deferred routine for setting vector values
      PROCEDURE(int_vector_set_sub),DEFERRED,PASS :: set
  ENDTYPE VectorType    
!
!List of Abstract Interfaces
  !> Explicitly defines the interface for the clear routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_sub(vector)
      IMPORT :: VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
    ENDSUBROUTINE int_vector_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the init routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_init_sub(vector,n)
      IMPORT :: SIK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: n
    ENDSUBROUTINE int_vector_init_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the set routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_set_sub(vector,i,setval)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
    ENDSUBROUTINE int_vector_set_sub
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
      !> @copybrief VectorTypes::set_RealVectorType
      !> @copydetails VectorTypes::set_RealVectorType
      PROCEDURE,PASS :: set => set_RealVectorType
  ENDTYPE RealVectorType


  !> @brief The extended type for PETSc vectors
  TYPE,EXTENDS(VectorType) :: PETScVectorType
    !> The values of the vector
#ifdef HAVE_PETSC
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
      !> @copybrief VectorTypes::set_PETScVectorType
      !> @copydetails VectorTypes::set_PETScVectorType
      PROCEDURE,PASS :: set => set_PETScVectorType
      !> @copybrief VectorTypes::get_PETScVectorType
      !> @copydetails VectorTypes::get_PETScVectorType
      PROCEDURE,PASS :: get => get_PETScVectorType
  ENDTYPE PETScVectorType

  
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
!> @brief Initializes the real vector
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
          CALL VecCreate(MPI_COMM_WORLD,vector%b,ierr)
          CALL VecSetSizes(vector%b,PETSC_DECIDE,vector%n,ierr)
          CALL VecSetType(vector%b,VECMPI,ierr)
          CALL VecSetFromOptions(vector%b,ierr)
        ENDIF
      ELSE
        CALL eVectorType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - VectorType already initialized')
      ENDIF
      IF(localalloc) DEALLOCATE(eVectorType)
#endif
    ENDSUBROUTINE init_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Clears the real vector
!> @param declares the vector type to act on
!>
    SUBROUTINE clear_RealVectorType(vector)
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
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      vector%isInit=.FALSE.
      vector%n=0
      CALL VecDestroy(vector%b,ierr)
#endif
    ENDSUBROUTINE clear_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the real vector
!> @param declare the vector type to act on
!> @param i the ith location in the vector
!> @param setval the value to be set
!>
    SUBROUTINE set_RealVectorType(vector,i,setval)
      CLASS(RealVectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
      IF(vector%isInit) THEN
        IF((i <= vector%n) .AND. (i > 0)) THEN
          vector%b(i)=setval
        ENDIF
      ENDIF
    ENDSUBROUTINE set_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the real vector
!> @param declare the vector type to act on
!> @param i the ith location in the vector
!> @param setval the value to be set
!>
    SUBROUTINE set_PETScVectorType(vector,i,setval)
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      IF(vector%isInit) THEN
        IF((i <= vector%n) .AND. (i > 0)) THEN
          CALL VecSetValues(vector%b,1,i-1,setval,INSERT_VALUES,ierr)
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE set_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets the values in the PETSc vector - presently untested
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!>
!> This routine gets the values of the PETSc vector.  If the location is out of
!> bounds, then -1051.0 is returned (-1051.0 is an arbitrarily chosen key).
!>
    FUNCTION get_PETScVectorType(vector,i) RESULT(getval)
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK) :: getval
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      IF(vector%isInit) THEN
        IF((i <= vector%n) .AND. (i > 0)) THEN
          CALL VecGetValues(vector%b,1,i-1,getval,ierr)
        ELSE
          getval=-1051._SRK
        ENDIF
      ENDIF
#endif
    ENDFUNCTION get_PETScVectorType
!
ENDMODULE VectorTypes
