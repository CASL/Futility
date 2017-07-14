!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief VectorType implementation for the PETSc TPL
!>
!> @author Shane Stimpson
!>   @date 08/20/2012
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE VectorTypes_PETSc
  USE IntrType
  USE ExceptionHandler
  USE ParameterLists
  USE VectorTypes_Base

  IMPLICIT NONE

#ifdef FUTILITY_HAVE_PETSC

#include <finclude/petsc.h>
#undef IS

  PRIVATE
!
! List of public members
  PUBLIC :: PETScVectorType
  
  !> @brief The extended type for PETSc vectors
  TYPE,EXTENDS(DistributedVectorType) :: PETScVectorType
    !> The values of the vector
    Vec :: b
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

  !> Scratch variable for petsc error code.
  !> It is an integer type.
  PetscErrorCode  :: iperr

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='VECTORTYPES_PETSC'

!
!===============================================================================
  CONTAINS
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
      CALL validParams%clear()
    ENDSUBROUTINE init_PETScVectorType
!
!-------------------------------------------------------------------------------
!> @brief Clears the PETSc vector
!> @param declares the vector type to act on
!>
    SUBROUTINE clear_PETScVectorType(thisVector)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector

      IF(thisVector%isInit) CALL VecDestroy(thisVector%b,iperr)
      thisVector%isInit=.FALSE.
      thisVector%isAssembled=.FALSE.
      thisVector%isCreated=.FALSE.
      thisVector%n=0
      IF(VectorType_Paramsflag) CALL VectorType_Clear_ValidParams()
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
      !
      INTEGER(SIK) :: ierrc
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
      !
      INTEGER(SIK) :: ierrc
      ierrc=-1
      IF(thisVector%isInit) THEN
        CALL thisVector%assemble(iperr)
        IF(iperr == 0) CALL VecSet(thisVector%b,setval,iperr)
        thisVector%isAssembled=.FALSE.
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
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
      INTEGER(SIK) :: i
      !
      INTEGER(SIK) :: ierrc

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
      INTEGER(SIK) :: i
      !
      INTEGER(SIK) :: ierrc

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
      INTEGER(SIK) :: i
      !
      INTEGER(SIK) :: ierrc

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
      !
      INTEGER(SIK) :: ierrc
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
      INTEGER(SIK) :: i
      !
      INTEGER(SIK) :: ierrc

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
      INTEGER(SIK) :: i
      !
      INTEGER(SIK) :: ierrc

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
    ENDSUBROUTINE getRange_PETScVectorType
!
!-------------------------------------------------------------------------------
    SUBROUTINE assemble_PETScVectorType(thisVector,ierr)
      CLASS(PETScVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      !
      INTEGER(SIK) :: ierrc
      ierrc=0
      IF(.NOT.thisVector%isAssembled) THEN
        thisVector%isAssembled=.FALSE.
        CALL VecAssemblyBegin(thisVector%b,iperr)
        IF(iperr == 0) CALL VecAssemblyEnd(thisVector%b,iperr)
        IF(iperr == 0) thisVector%isAssembled=.TRUE.
        ierrc=iperr
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE assemble_PETScVectorType

#endif
ENDMODULE
