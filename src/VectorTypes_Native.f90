!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Futility-native implementations of VectorType
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref Allocs "Allocs": @copybrief Allocs
!>  - @ref VectorTypes_Base "VectorTypes_Base": @copybrief VectorTypes_Base
!>
!> @author Shane Stimpson
!>   @date 08/20/2012
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE VectorTypes_Native
  USE IntrType
  USE ExceptionHandler
#include "Futility_DBC.h"
  USE Futility_DBC
  USE ParameterLists
  USE Allocs
  USE VectorTypes_Base

  IMPLICIT NONE

  PRIVATE

!
! List of public members
  PUBLIC :: RealVectorType

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
      !> @copybrief VectorTypes::getSelected_RealVectorType
      !> @copydetails VectorTypes::getSelected_RealVectorType
      PROCEDURE,PASS :: getSelected => getSelected_RealVectorType
      !> @copybrief VectorTypes::getRange_RealVectorType
      !> @copydetails VectorTypes::getRange_RealVectorType
      PROCEDURE,PASS :: getRange => getRange_RealVectorType
  ENDTYPE RealVectorType

  INTEGER(SIK) :: ierrc

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='VECTORTYPES_NATIVE'
  
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
!> Works on serial vectors only, meaning the indices used to extract data from the vector
!> are 1->N, where N is the size of the vector.
!> @param declares the vector type to act on
!> @param getval Correctly sized array that will be filled with contents of this vector
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
!> @brief Gets a list of selected values from the vector
!> @param declares the vector type to act on
!> @param indices A list of the indices in the vector
!> @param getval Correctly sized array that will be filled with contents of this vector
    SUBROUTINE getSelected_RealVectorType(thisVector,indices,getval,ierr)
      CLASS(RealVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: indices(:)
      REAL(SRK),INTENT(INOUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      REQUIRE(thisVector%isInit)
      REQUIRE(SIZE(indices) == SIZE(getval))
      REQUIRE(SIZE(indices) <= thisVector%n)
      ierrc=0
      getval=thisVector%b(indices)
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE getSelected_RealVectorType

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


ENDMODULE VectorTypes_Native
