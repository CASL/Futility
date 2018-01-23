!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Trilinos implementations of VectorType
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref VectorTypes_Base "VectorTypes_Base": @copybrief VectorTypes_Base
!>  - @ref trilinos_interfaces "trilinos_interfaces": @copybrief trilinos_interfaces
!>
!> @author Benjamin Collins
!>   @date 07/17/2017
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE VectorTypes_Trilinos
  USE ISO_C_BINDING
  USE IntrType
  USE ExceptionHandler
  USE ParameterLists
  USE VectorTypes_Base
#ifdef FUTILITY_HAVE_ForTrilinos
#include "ForTrilinosTpetra_config.hpp"
#include "ForTrilinos.h"
  USE forteuchos
  USE fortpetra

  IMPLICIT NONE

  PRIVATE
!
! List of public members
  PUBLIC :: TrilinosVectorType

  !> @brief The extended type for Trilinos vectors
  TYPE,EXTENDS(DistributedVectorType) :: TrilinosVectorType
    !> mpi communicator
    TYPE(TeuchosComm) :: Tcomm
    !> Parallel distribution map
    TYPE(TpetraMap) :: map
    !> Vector
    TYPE(TpetraMultiVector) :: b
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

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='VECTORTYPES_TRILINOS'
!
!===============================================================================
  CONTAINS
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
#ifdef HAVE_MPI
            CALL thisVector%Tcomm%create(MPI_Comm_ID)
#else
            CALL thisVector%Tcomm%create()
#endif
            CALL thisVector%map%create(INT(n,C_LONG),INT(nlocal,C_LONG),thisVector%Tcomm)
            CALL thisVector%b%create(thisVector%map,1_C_LONG)
            thisVector%isCreated=.TRUE.
          ENDIF
        ENDIF
      ELSE
        CALL eVectorType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - VectorType already initialized')
      ENDIF
      CALL validParams%clear()
    ENDSUBROUTINE init_TrilinosVectorType
!
!-------------------------------------------------------------------------------
!> @brief Clears the Trilinos vector
!> @param declares the vector type to act on
!>
    SUBROUTINE clear_TrilinosVectorType(thisVector)
      CLASS(TrilinosVectorType),INTENT(INOUT) :: thisVector

      !IF(thisVector%isInit) CALL ForPETRA_VecDestroy(thisVector%b)
      thisVector%isInit=.FALSE.
      thisVector%isAssembled=.FALSE.
      thisVector%isCreated=.FALSE.
      thisVector%n=0
      CALL thisVector%b%release()
      CALL thisVector%map%release()
      CALL thisVector%Tcomm%release()
      IF(VectorType_Paramsflag) CALL VectorType_Clear_ValidParams()
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
      !
      INTEGER(SIK) :: ierrc

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF((i <= thisVector%n) .AND. (i > 0)) THEN
          CALL thisVector%b%replaceGlobalValue(INT(i,C_LONG),1_C_LONG,setval)
          ierrc=0
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
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
      !
      INTEGER(SIK) :: ierrc

      ierrc=-1
      IF(thisVector%isInit) THEN
        CALL thisVector%b%putScalar(setval)
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
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
      !
      INTEGER(SIK) :: ierrc

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
      !
      INTEGER(SIK) :: ierrc
      INTEGER(SIK) :: i

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n) THEN
          DO i=istt,istp
            CALL thisVector%b%replaceGlobalValue(INT(i,C_LONG),1_C_LONG,setval)
          ENDDO
          ierrc=0
          thisVector%isAssembled=.TRUE.
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
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
      !
      INTEGER(SIK) :: ierrc
      INTEGER(SIK) :: i

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n) THEN
          ierrc=-3
          IF(istp-istt+1 == SIZE(setval)) THEN
            DO i=istt,istp
              CALL thisVector%b%replaceGlobalValue(INT(i,C_LONG),1_C_LONG,setval(i-istt+1))
            ENDDO
            thisVector%isAssembled=.TRUE.
            ierrc=0
          ENDIF
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
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

      REAL(C_DOUBLE),DIMENSION(:),POINTER :: tmpval(:)=>NULL()
      INTEGER(C_INT) :: localID
      INTEGER(SIK) :: ierrc

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF((i <= thisVector%n) .AND. (i > 0)) THEN
          localID=thisVector%map%getLocalElement(INT(i,C_LONG_LONG))
          tmpval=>thisVector%b%getData(INT(1,C_SIZE_T))
          getval=tmpval(localID)
          ierrc=0
        ENDIF
      ENDIF

      IF(PRESENT(ierr)) ierr=ierrc
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
      !
      INTEGER(SIK) :: ierrc

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
      !
      REAL(C_DOUBLE),DIMENSION(:),POINTER :: tmpval(:)=>NULL()
      INTEGER(C_INT) :: localID
      INTEGER(SIK) :: ierrc
      INTEGER(SIK) :: i

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n) THEN
          ierrc=-3
          IF(istp-istt+1 == SIZE(getval)) THEN
            tmpval=>thisVector%b%getData(INT(1,C_SIZE_T))
            DO i=istt,istp
              localID=thisVector%map%getLocalElement(INT(i,C_LONG_LONG))
              getval(i-istt+1)=tmpval(localID)
            ENDDO
            ierrc=0
          ENDIF
        ENDIF
      ENDIF

      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE getRange_TrilinosVectorType
!
!-------------------------------------------------------------------------------
    SUBROUTINE assemble_TrilinosVectorType(thisVector,ierr)
      CLASS(TrilinosVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      !Trilinos vectors don't need assembly
    ENDSUBROUTINE assemble_TrilinosVectorType
#endif
ENDMODULE VectorTypes_Trilinos
