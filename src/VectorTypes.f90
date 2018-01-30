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
!> This module exposes all of the modules which provide VectorType
!> functionality, as enabled at configure time. This includes the Native vector
!> types, and optionally PETSc and/or Trilinos vector types.
!> It also provides abstract factory routines to aid in constructing VectorType
!> objects, as well as interfaces to the BLAS algorithms
!>
!> The objects are initialized with a parameter list. For valid reference lists
!> see @ref VectorTypes::VectorTypes_Declare_ValidParams
!> "VectorTypes_Declare_ValidParams".
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref Allocs "Allocs": @copybrief Allocs
!>  - @ref ParameterLists "ParamterLists": @copybrief ParamterLists
!>  - @ref VectorType_Base "VectorType_Base": @copybrief VectorType_Base
!>  - @ref VectorTypes_Native "VectorTypes_Native": @copybrief VectorTypes_Native
!>  - @ref VectorTypes_PETSc "VectorTypes_PETSc": @copybrief VectorTypes_PETSc
!>  - @ref VectorTypes_Trilinos "VectorTypes_Trilinos": @copybrief VectorTypes_Trilinos
!>  - @ref trilinos_interfaces "trilinos_interfaces": @copybrief trilinos_interfaces
!>  - @ref BLAS1 "BLAS1": @copybrief BLAS1
!>
!> @par EXAMPLES
!> @code
!> PROGRAM ExampleVector
!>   TYPE(RealVectorType) :: vector
!>   CLASS(VectorType),POINTER :: vec_p
!>   TYPE(ParamType) :: params
!>   
!>   CALL params%add("VectorType->n",36_SIK)
!>
!>   CALL vector%init(params)
!>   CALL params%clear()
!>   CALL vector%set(1,10._SRK)
!>   value=vector%get(1)
!>   CALL vector%clear()
!>
!>   ! Create a Trilinos vector using the abstract factory
!>   CALL params%add("VectorType->n",36_SIK)
!>   CALL params%add("VectorType->nlocal",6_SIK)
!>   CALL params%add("VectorType->engine",VM_TRILINOS)
!>   CALL params%add("VectorType->MPI_Comm_ID",MPI_COMM_WORLD)
!>
!>   CALL VectorFactory(vec_p,params)
!>
!>   ! Clean up
!>   CALL vec_p%clear()
!>   DEALLOCATE(vec_p)
!>
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
  USE VectorTypes_Base
  USE VectorTypes_Native
  USE VectorTypes_PETSc
  USE VectorTypes_Trilinos
  USE trilinos_interfaces
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

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
#undef IS
#endif

  PRIVATE
!
! List of public members
  PUBLIC :: eVectorType
  PUBLIC :: VectorFactory
  PUBLIC :: VectorResemble
  PUBLIC :: VectorType
  PUBLIC :: DistributedVectorType
  PUBLIC :: RealVectorType
#ifdef FUTILITY_HAVE_PETSC
  PUBLIC :: PETScVectorType
#endif
#ifdef FUTILITY_HAVE_Trilinos
  PUBLIC :: TrilinosVectorType
#endif
  !> Enumerated matrix-vector engines
  INTEGER(SIK),PARAMETER,PUBLIC :: VM_PETSC=0,VM_TRILINOS=1,VM_NATIVE=2
  PUBLIC :: VectorType_Declare_ValidParams
  PUBLIC :: VectorType_Clear_ValidParams
  PUBLIC :: BLAS_asum
  PUBLIC :: BLAS_axpy
  PUBLIC :: BLAS_copy
  PUBLIC :: BLAS_dot
  PUBLIC :: BLAS_iamax
  PUBLIC :: BLAS_iamin
  PUBLIC :: BLAS_nrm2
  PUBLIC :: BLAS_scal
  PUBLIC :: BLAS_swap
  
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

#ifdef FUTILITY_HAVE_PETSC
  !> Scratch variable for petsc error code.
  !> It is an integer type.
  PetscErrorCode  :: iperr
#endif

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='VECTORTYPES'

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Abstract factory for all enabled VectorTypes
!>
!> @param vector the vector pointer to allocate and initialize. Should be NULL
!> @param params the parameter list to use in determining the type of and
!> initializing the vector.
!>
!> This is an abstract factory routine for the base VectorType. It will use the
!> 'engine' parameter in the passed parameter list to determine which type of
!> Vector to allocate the pointer to. The parameter list is then used to
!> initialize the vector.
    SUBROUTINE VectorFactory(vector, params)
      CHARACTER(LEN=*),PARAMETER :: myName="VectorFactory"
      CLASS(VectorType),POINTER,INTENT(INOUT) :: vector
      CLASS(ParamType),INTENT(IN) :: params
      !
      INTEGER(SIK) :: engine

      IF(ASSOCIATED(vector)) THEN
        CALL eVectorType%raiseError(modName//"::"//myName//" - "// &
          "Vector pointer is already allocated")
        RETURN
      ENDIF

      ! Default to the native engine
      engine=VM_NATIVE

      IF(params%has("VectorType->engine")) THEN
        CALL params%get("VectorType->engine",engine)
      ENDIF

      SELECTCASE(engine)
        CASE(VM_NATIVE)
          ALLOCATE(RealVectorType :: vector)
        CASE(VM_PETSC)
#ifdef FUTILITY_HAVE_PETSC
          ALLOCATE(PETScVectorType :: vector)
#endif
        CASE(VM_TRILINOS)
#ifdef FUTILITY_HAVE_Trilinos
          ALLOCATE(TrilinosVectorType :: vector)
#endif
        CASE DEFAULT
          CALL eVectorType%raiseError(modName//"::"//myName//" - "// &
            "Unrecognized vector engine requested")
      ENDSELECT

      CALL vector%init(params)

    ENDSUBROUTINE VectorFactory
!
!-------------------------------------------------------------------------------
!> @brief Create a new vector of compatible size and type to the input vector
!>
!> @param dest the destination VectorType pointer to allocate and construct.
!> This should be NULL
!> @param source the vector type to use in determining the type and parameters
!> of the dest vector
!> @param params the parameters to use in overriding settings from the source
!> vector
!>
!> For now, the source vector shall be initialized, though in the future it
!> might be nice to support uninitialized vectors.
!> Unlike the corresponding routine for MatrixTypes, this routine will attempt
!> to adopt parameters from the source vector to initialize the dest vector.
!> This is only done for required parameters that are not provided in the passed
!> parameter list. Providing the parameters on the parameter list will override
!> the corresponding parameters from the source matrix.
    SUBROUTINE VectorResemble(dest, source, params)
      CHARACTER(LEN=*),PARAMETER :: myName="VectorResemble"
      CLASS(VectorType),POINTER,INTENT(INOUT) :: dest
      CLASS(VectorType),POINTER,INTENT(IN) :: source
      CLASS(ParamType),INTENT(INOUT) :: params

      IF(.NOT. ASSOCIATED(source)) THEN
        CALL eVectorType%raiseError(modName//"::"//myName//" - "// &
          "Source vector is not associated")
        RETURN
      ENDIF

      IF(.NOT. source%isInit) THEN
        CALL eVectorType%raiseError(modName//"::"//myName//" - "// &
          "Source vector is not initialized")
      ENDIF

      IF(ASSOCIATED(dest)) THEN
        CALL eVectorType%raiseError(modName//"::"//myName//" - "// &
          "Destination vector is already associated")
        RETURN
      ENDIF

      IF(.NOT. params%has("VectorType->n")) THEN
        CALL params%add("VectorType->n",source%n)
      ENDIF
      SELECTTYPE(source); CLASS IS(DistributedVectorType)
        IF(.NOT. params%has("VectorType->nlocal")) THEN
          CALL params%add("VectorType->nlocal",source%nlocal)
        ENDIF
        IF(.NOT. params%has("VectorType->MPI_Comm_Id")) THEN
          CALL params%add("VectorType->MPI_Comm_Id",source%comm) 
        ENDIF
      ENDSELECT

      SELECTTYPE(source)
        TYPE IS(RealVectorType)
          ALLOCATE(RealVectorType :: dest)
#ifdef FUTILITY_HAVE_PETSC
        TYPE IS(PETScVectorType)
          ALLOCATE(PETScVectorType :: dest)
#endif
#ifdef FUTILITY_HAVE_Trilinos
        TYPE IS(TrilinosVectorType)
          ALLOCATE(TrilinosVectorType :: dest)
#endif
      ENDSELECT

      CALL dest%init(params)

      CALL params%clear()
    ENDSUBROUTINE VectorResemble
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
#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScVectorType)
        IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
        IF(iperr == 0) CALL VecNorm(thisVector%b,NORM_1,r,iperr)
#endif
#ifdef FUTILITY_HAVE_Trilinos
      TYPE IS(TrilinosVectorType)
        CALL ForPETRA_VecSUM(thisVector%b,r)
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
#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScVectorType)
        SELECTTYPE(newVector); TYPE IS(PETScVectorType)
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
          IF(iperr == 0) CALL VecAXPY(newVector%b,alpha,thisVector%b,iperr)
        ENDSELECT
#endif
#ifdef FUTILITY_HAVE_Trilinos
      TYPE IS(TrilinosVectorType)
        SELECTTYPE(newVector); TYPE IS(TrilinosVectorType)
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble()
          CALL ForPETRA_VecAXPY(newVector%b,thisVector%b,alpha,1.0_SRK)
        ENDSELECT
#endif
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
!> TIBWSFB: This is inefficient. It makes a copy of the vectors, which
!> is a pretty big waste. The one upside to this is the algorithm can be
!> implemented using a type-agnostic approach. But we still select type
!> everything for some reason. Worst of both worlds
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
#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScVectorType)
        SELECTTYPE(newVector); TYPE IS(PETScVectorType)
          SELECTTYPE(aVector); TYPE IS(PETScVectorType)
            ALLOCATE(tmpthis(thisVector%n))
            ALLOCATE(tmpnew(newVector%n))
            ALLOCATE(tmpa(aVector%n))
            CALL thisVector%get(tmpthis)
            CALL newVector%get(tmpnew)
            CALL aVector%get(tmpa)
          ENDSELECT
        ENDSELECT
#endif
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

      CALL newVector%set(tmpnew)

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
#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScVectorType)
        SELECTTYPE(newVector); TYPE IS(PETScVectorType)
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
          IF(iperr == 0) CALL VecCopy(thisVector%b,newVector%b,iperr)
        ENDSELECT
#endif
#ifdef FUTILITY_HAVE_Trilinos
      TYPE IS(TrilinosVectorType)
        SELECTTYPE(newVector); TYPE IS(TrilinosVectorType)
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble()
          CALL ForPETRA_VecCopy(newVector%b,thisVector%b)
        ENDSELECT
#endif
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
#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScVectorType)
        SELECTTYPE(thatVector); TYPE IS(PETScVectorType)
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
          IF(iperr == 0) CALL VecTDot(thisVector%b,thatVector%b,r,iperr)
        ENDSELECT
#endif
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
#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        CALL thisVector%get(tmpthis)
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
#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScVectorType)
        ALLOCATE(tmpthis(thisVector%n))
        CALL thisVector%get(tmpthis)
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
#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScVectorType)
        IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
        IF(iperr == 0) CALL VecNorm(thisVector%b,NORM_2,norm2,iperr)
#endif
#ifdef FUTILITY_HAVE_Trilinos
      TYPE IS(TrilinosVectorType)
        IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble()
        CALL ForPETRA_VecNorm2(thisVector%b,norm2)
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
#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScVectorType)
        IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
        IF(iperr == 0) CALL VecScale(thisVector%b,a,iperr)
#endif
#ifdef FUTILITY_HAVE_Trilinos
      TYPE IS(TrilinosVectorType)
        CALL ForPETRA_VecScale(thisVector%b,a)
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
#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScVectorType)
        SELECTTYPE(aVector); TYPE IS(PETScVectorType)
          ALLOCATE(tmpthis(thisVector%n))
          ALLOCATE(tmpa(aVector%n))
          CALL thisVector%get(tmpthis)
          CALL aVector%get(tmpa)
        ENDSELECT
#endif
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

      CALL thisVector%set(tmpthis)

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
#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScVectorType)
        SELECTTYPE(thatVector); TYPE IS(PETScVectorType)
          IF(.NOT.thisVector%isAssembled) CALL thisVector%assemble(iperr)
          IF(.NOT.thatVector%isAssembled) CALL thatVector%assemble(iperr)
          IF(iperr == 0) CALL VecSwap(thisVector%b,thatVector%b,iperr)
        ENDSELECT
#endif
      CLASS DEFAULT
        CALL eVectorType%raiseFatalError('Incorrect call to '// &
           modName//'::'//myName//' - This interface is not available.')
      ENDSELECT
    ENDSUBROUTINE swap_VectorType
!
ENDMODULE VectorTypes
