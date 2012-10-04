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
      PROCEDURE(int_vector_setone_sub),DEFERRED,PASS :: setone
      !> Deferred routine for setting all vector values
      PROCEDURE(int_vector_setall_sub),DEFERRED,PASS :: setall
      GENERIC :: set => setone,setall
      !> Deferred routine for getting vector values
      PROCEDURE(int_vector_get_sub),DEFERRED,PASS :: get
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
    SUBROUTINE int_vector_setone_sub(vector,i,setval)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
    ENDSUBROUTINE int_vector_setone_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the set all routine of all vector types
  ABSTRACT INTERFACE
    SUBROUTINE int_vector_setall_sub(vector,setval)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      REAL(SRK),INTENT(IN) :: setval
    ENDSUBROUTINE int_vector_setall_sub
  ENDINTERFACE
  
  !> Explicitly defines the interface for the get routine of all vector types
  ABSTRACT INTERFACE
    FUNCTION int_vector_get_sub(vector,i) RESULT(getval)
      IMPORT :: SIK,SRK,VectorType
      CLASS(VectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK) :: getval
    ENDFUNCTION int_vector_get_sub
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
      !> @copybrief VectorTypes::setone_RealVectorType
      !> @copydetails VectorTypes::setone_RealVectorType
      PROCEDURE,PASS :: setone => setone_RealVectorType
      !> @copybrief VectorTypes::setall_RealVectorType
      !> @copydetails VectorTypes::setall_RealVectorType
      PROCEDURE,PASS :: setall => setall_RealVectorType
      !> @copybrief VectorTypes::get_RealVectorType
      !> @copydetails VectorTypes::get_RealVectorType
      PROCEDURE,PASS :: get => get_RealVectorType
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
      !> @copybrief VectorTypes::setone_PETScVectorType
      !> @copydetails VectorTypes::setone_PETScVectorType
      PROCEDURE,PASS :: setone => setone_PETScVectorType
      !> @copybrief VectorTypes::setall_PETScVectorType
      !> @copydetails VectorTypes::setall_PETScVectorType
      PROCEDURE,PASS :: setall => setall_PETScVectorType
      !> @copybrief VectorTypes::get_PETScVectorType
      !> @copydetails VectorTypes::get_PETScVectorType
      PROCEDURE,PASS :: get => get_PETScVectorType
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
      vector%isAssembled=.FALSE.
      vector%isCreated=.FALSE.
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
    SUBROUTINE setone_RealVectorType(vector,i,setval)
      CLASS(RealVectorType),INTENT(INOUT) :: vector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
      IF(vector%isInit) THEN
        IF((i <= vector%n) .AND. (i > 0)) THEN
          vector%b(i)=setval
        ENDIF
      ENDIF
    ENDSUBROUTINE setone_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the real vector
!> @param declare the vector type to act on
!> @param i the ith location in the vector
!> @param setval the value to be set
!>
    SUBROUTINE setone_PETScVectorType(vector,i,setval)
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
#endif
    ENDSUBROUTINE setone_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the real vector
!> @param declare the vector type to act on
!> @param setval the value to be set
!>
    SUBROUTINE setall_RealVectorType(vector,setval)
      CLASS(RealVectorType),INTENT(INOUT) :: vector
      REAL(SRK),INTENT(IN) :: setval
      IF(vector%isInit) THEN
          vector%b=setval
      ENDIF
    ENDSUBROUTINE setall_RealVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values in the real vector
!> @param declare the vector type to act on
!> @param setval the value to be set
!>
    SUBROUTINE setall_PETScVectorType(vector,setval)
      CLASS(PETScVectorType),INTENT(INOUT) :: vector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK) :: i
#ifdef HAVE_PETSC
      PetscErrorCode  :: ierr
      IF(vector%isInit) THEN
        DO i=1,vector%n
          CALL VecSetValue(vector%b,i-1,setval,INSERT_VALUES,ierr)
        ENDDO
        CALL VecAssemblyBegin(vector%b,ierr)
        CALL VecAssemblyEnd(vector%b,ierr)
      ENDIF
#endif
    ENDSUBROUTINE setall_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets the values in the real vector
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!>
!> This routine gets the values of the real vector.  If the location is out of
!> bounds, then -1051.0 is returned (-1051.0 is an arbitrarily chosen key).
!>
    FUNCTION get_RealVectorType(vector,i) RESULT(getval)
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
    ENDFUNCTION get_RealVectorType
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
#endif
    ENDFUNCTION get_PETScVectorType
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
      CLASS(VectorType),INTENT(IN)     :: thisVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      REAL(SRK) :: r

      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        IF(PRESENT(n) .AND. PRESENT(incx)) THEN
          r = BLAS1_asum(n,thisVector%b,incx)
        ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          r = BLAS1_asum(n,thisVector%b)
        ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
          r = BLAS1_asum(thisVector%b,incx)
        ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          r = BLAS1_asum(thisVector%b)
        ENDIF
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
      CLASS(VectorType),INTENT(IN)     :: thisVector
      CLASS(VectorType),INTENT(INOUT)  :: newVector
      REAL(SRK),INTENT(IN),OPTIONAL :: a
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incy
      
      REAL(SRK) :: alpha=1.
      
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
      CLASS(VectorType),INTENT(IN)     :: thisVector
      CLASS(VectorType),INTENT(INOUT)  :: newVector
      CLASS(VectorType),INTENT(IN)     :: aVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incy
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(newVector); TYPE IS(RealVectorType)
          SELECTTYPE(aVector); TYPE IS(RealVectorType)
            IF(PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
              CALL BLAS1_axpy(n,aVector%b,thisVector%b,incx,newVector%b,incy)
            ELSEIF(PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
              CALL BLAS1_axpy(n,aVector%b,thisVector%b,newVector%b,incx)
            ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
              CALL BLAS1_axpy(n,aVector%b,thisVector%b,newVector%b,incy)
            ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
              CALL BLAS1_axpy(n,aVector%b,thisVector%b,newVector%b)
            ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
              CALL BLAS1_axpy(aVector%b,thisVector%b,newVector%b)
            ENDIF
          ENDSELECT
        ENDSELECT
      ENDSELECT

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
      CLASS(VectorType),INTENT(IN)     :: thisVector
      CLASS(VectorType),INTENT(IN)     :: thatVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incy
      REAL(SRK) :: r
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(thatVector); TYPE IS(RealVectorType)
          IF(PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
            r = BLAS1_dot(n,thisVector%b,incx,thatVector%b,incy)
          ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. PRESENT(incy)) THEN
            r = BLAS1_dot(thisVector%b,incx,thatVector%b,incy)
          ELSEIF(PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            r = BLAS1_dot(n,thisVector%b,thatVector%b,incx)
          ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
            r = BLAS1_dot(n,thisVector%b,thatVector%b,incy)
          ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            r = BLAS1_dot(n,thisVector%b,thatVector%b)
          ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            r = BLAS1_dot(thisVector%b,thatVector%b,incx)
          ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. PRESENT(incy)) THEN
            r = BLAS1_dot(thisVector%b,thatVector%b,incy)
          ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx) .AND. .NOT.PRESENT(incy)) THEN
            r = BLAS1_dot(thisVector%b,thatVector%b)
          ENDIF
        ENDSELECT
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
      CLASS(VectorType),INTENT(IN) :: thisVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK) :: imax
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        IF(PRESENT(n) .AND. PRESENT(incx)) THEN
          imax = BLAS1_iamax(n,thisVector%b,incx)
        ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
          imax = BLAS1_iamax(thisVector%b,incx)
        ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          imax = BLAS1_iamax(n,thisVector%b)
        ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          imax = BLAS1_iamax(thisVector%b)
        ENDIF
      ENDSELECT

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
      CLASS(VectorType),INTENT(IN) :: thisVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      INTEGER(SIK) :: imin
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        IF(PRESENT(n) .AND. PRESENT(incx)) THEN
          imin = BLAS1_iamin(n,thisVector%b,incx)
        ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
          imin = BLAS1_iamin(thisVector%b,incx)
        ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          imin = BLAS1_iamin(n,thisVector%b)
        ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          imin = BLAS1_iamin(thisVector%b)
        ENDIF
      ENDSELECT

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
      CLASS(VectorType),INTENT(IN) :: thisVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      REAL(SRK) :: norm2
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        IF(PRESENT(n) .AND. PRESENT(incx)) THEN
          norm2 = BLAS1_nrm2(n,thisVector%b,incx)
        ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
          norm2 = BLAS1_nrm2(thisVector%b,incx)
        ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          norm2 = BLAS1_nrm2(n,thisVector%b)
        ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
          norm2 = BLAS1_nrm2(thisVector%b)
        ENDIF
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
      CLASS(VectorType),INTENT(INOUT) :: thisVector
      CLASS(VectorType),INTENT(IN) :: aVector
      INTEGER(SIK),INTENT(IN),OPTIONAL :: n
      INTEGER(SIK),INTENT(IN),OPTIONAL :: incx
      
      SELECTTYPE(thisVector); TYPE IS(RealVectorType)
        SELECTTYPE(aVector); TYPE IS(RealVectorType)
          IF(PRESENT(n) .AND. PRESENT(incx)) THEN
            CALL BLAS1_scal(n,aVector%b,thisVector%b,incx)
          ELSEIF(.NOT.PRESENT(n) .AND. PRESENT(incx)) THEN
            CALL BLAS1_scal(aVector%b,thisVector%b,incx)
          ELSEIF(PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
            CALL BLAS1_scal(n,aVector%b,thisVector%b)
          ELSEIF(.NOT.PRESENT(n) .AND. .NOT.PRESENT(incx)) THEN
            CALL BLAS1_scal(aVector%b,thisVector%b)
          ENDIF
        ENDSELECT
      ENDSELECT

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

    ENDSUBROUTINE swap_VectorType  

ENDMODULE VectorTypes
