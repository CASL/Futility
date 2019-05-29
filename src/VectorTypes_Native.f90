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
  USE ParallelEnv

  IMPLICIT NONE

  PRIVATE

!
! List of public members
  PUBLIC :: NativeVectorType
  PUBLIC :: RealVectorType
  PUBLIC :: NativeDistributedVectorType

  TYPE,ABSTRACT,EXTENDS(VectorType) :: NativeVectorType
    REAL(SRK),ALLOCATABLE :: b(:)
  ENDTYPE NativeVectorType

  !> @brief The extended type for real vector
  !>
  !> This does not add a significant functionality over the intrinsic
  !> allocatable arrays that are part of the Fortran language. It
  !> is provided so as to be able to use the BLAS interfaces adapted
  !> for the vector type and it also gains some value by having the
  !> isSymmetric and isInit attributes.
  TYPE,EXTENDS(NativeVectorType) :: RealVectorType
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
      !> @copybrief VectorTypes::setSelected_RealVectorType
      !> @copydetails VectorTypes::setSelected_RealVectorType
      PROCEDURE,PASS :: setSelected => setSelected_RealVectorType
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

  TYPE,EXTENDS(NativeVectorType) :: NativeDistributedVectorType
    INTEGER(SIK) :: offset=0
    !> MPI comm ID
    INTEGER(SIK) :: comm=-1
    !> total number of local values
    INTEGER(SIK) :: nlocal=-1

    !TYPE(MPI_EnvType) :: commType

    !!! TODO: Edit vectortypes_base appropriately
    !
    !List of Type Bound Procedures
    CONTAINS
      !> @copybrief VectorTypes::clear_NativeDistributedVectorType
      !> @copydetails VectorTypes::clear_NativeDistributedVectorType
      PROCEDURE,PASS :: clear => clear_NativeDistributedVectorType
      !> @copybrief VectorTypes::init_NativeDistributedVectorType
      !> @copydetails VectorTypes::init_NativeDistributedVectorType
      PROCEDURE,PASS :: init => init_NativeDistributedVectorType
      !> @copybrief VectorTypes::setOne_NativeDistributedVectorType
      !> @copydetails VectorTypes::setOne_NativeDistributedVectorType
      PROCEDURE,PASS :: setOne => setOne_NativeDistributedVectorType
      !> @copybrief VectorTypes::setAll_scalar_NativeDistributedVectorType
      !> @copydetails VectorTypes::setAll_scalar_NativeDistributedVectorType
      PROCEDURE,PASS :: setAll_scalar => setAll_scalar_NativeDistributedVectorType
      !> @copybrief VectorTypes::setAll_array_NativeDistributedVectorType
      !> @copydetails VectorTypes::setAll_array_NativeDistributedVectorType
      PROCEDURE,PASS :: setAll_array => setAll_array_NativeDistributedVectorType
      !> @copybrief VectorTypes::setSelected_NativeDistributedVectorType
      !> @copydetails VectorTypes::setSelected_NativeDistributedVectorType
      PROCEDURE,PASS :: setSelected => setSelected_NativeDistributedVectorType
      !> @copybrief VectorTypes::setRange_scalar_NativeDistributedVectorType
      !> @copydetails VectorTypes::setRange_scalar_NativeDistributedVectorType
      PROCEDURE,PASS :: setRange_scalar => setRange_scalar_NativeDistributedVectorType
      !> @copybrief VectorTypes::setRange_array_NativeDistributedVectorType
      !> @copydetails VectorTypes::setRange_array_NativeDistributedVectorType
      PROCEDURE,PASS :: setRange_array => setRange_array_NativeDistributedVectorType
      !> @copybrief VectorTypes::getOne_NativeDistributedVectorType
      !> @copydetails VectorTypes::getOne_NativeDistributedVectorType
      PROCEDURE,PASS :: getOne => getOne_NativeDistributedVectorType
      !> @copybrief VectorTypes::getAll_NativeDistributedVectorType
      !> @copydetails VectorTypes::getAll_NativeDistributedVectorType
      PROCEDURE,PASS :: getAll => getAll_NativeDistributedVectorType
      !> @copybrief VectorTypes::getSelected_NativeDistributedVectorType
      !> @copydetails VectorTypes::getSelected_NativeDistributedVectorType
      PROCEDURE,PASS :: getSelected => getSelected_NativeDistributedVectorType
      !> @copybrief VectorTypes::getRange_NativeDistributedVectorType
      !> @copydetails VectorTypes::getRange_NativeDistributedVectorType
      PROCEDURE,PASS :: getRange => getRange_NativeDistributedVectorType
      !> @copybrief VectorTypes::assemble_PETScVectorType
      !> @copydetails VectorTypes::assemble_PETScVectorType
      PROCEDURE,PASS :: assemble => assemble_NativeDistributedVectorType
      ! TODO: Add descr.
      !PROCEDURE,PASS :: inLocalMem => inLocalMem_NativeDistributedVectorType
  ENDTYPE NativeDistributedVectorType


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
!> @brief Sets selected values in the real vector
!> @param declare the vector type to act on
!> @param indices a list of indices in the vector to set
!> @param setval the array of values to be set (must be same size as indices)
!>
    SUBROUTINE setSelected_RealVectorType(thisVector,indices,setval,ierr)
      CLASS(RealVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: indices(:)
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      REQUIRE(thisVector%isInit)
      REQUIRE(SIZE(setval) == SIZE(indices))
      ierrc=0
      thisVector%b(indices)=setval
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE setSelected_RealVectorType

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
      REAL(SRK),INTENT(OUT) :: getval
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
      REAL(SRK),INTENT(OUT) :: getval(:)
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
      REAL(SRK),INTENT(OUT) :: getval(:)
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
      REAL(SRK),INTENT(OUT):: getval(:)
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

    SUBROUTINE init_NativeDistributedVectorType(thisVector,Params)
      CHARACTER(LEN=*),PARAMETER :: myName='init_NativeDistributedVectorType'
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
      TYPE(ParamType),INTENT(IN) :: Params
      TYPE(ParamType) :: validParams
      INTEGER(SIK) :: n, nlocal, comm, nProc,rank,mpierr
      !TYPE(MPI_EnvType) :: commType

      !Check to set up required and optional param lists.
      IF(.NOT.VectorType_Paramsflag) CALL VectorType_Declare_ValidParams()

      !Validate against the reqParams and OptParams
      validParams=Params
      CALL validParams%validate(DistributedVectorType_reqParams,DistributedVectorType_optParams)

      !Pull Data from Parameter List
      CALL validParams%get('VectorType->n',n)
      CALL validParams%get('VectorType->MPI_Comm_ID',comm)
      !CALL validParams%get('VectorType->nlocal',nlocal)

      REQUIRE(.NOT. thisVector%isInit)
      REQUIRE(n > 1)

      thisVector%isInit=.TRUE.
      thisVector%n=n
      !CALL commType%init(comm)
      thisVector%comm = comm
      !thisVector%commType = commType

      CALL MPI_Comm_rank(comm,rank,mpierr)
      CALL MPI_Comm_size(comm,nproc,mpierr)
      ! Default to greedy partitioning
      IF(rank < MOD(n,nproc)) THEN
        thisVector%offset = (rank)*(n/nproc + 1)
        thisVector%nlocal = n/nproc + 1
      ELSE
        thisVector%offset = (rank)*(n/nproc) + MOD(n,nproc)
        thisVector%nlocal = n/nproc
      ENDIF
      ALLOCATE(thisVector%b(thisVector%nlocal))

      CALL validParams%clear()
    ENDSUBROUTINE init_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Clears the PETSc vector
!> @param declares the vector type to act on
!>
    SUBROUTINE clear_NativeDistributedVectorType(thisVector)
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector

      thisVector%isInit=.FALSE.
      thisVector%n=0
      IF(ALLOCATED(thisVector%b)) CALL demallocA(thisVector%b)

    ENDSUBROUTINE clear_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets one value in the real vector
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!> @param setval the value to be set
!>
    SUBROUTINE setOne_NativeDistributedVectorType(thisVector,i,setval,ierr)
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      !
      INTEGER(SIK) :: ierrc
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF((i <= thisVector%offset + thisVector%nlocal) .AND. (i > thisVector%offset)) THEN
          thisVector%b(i - thisVector%offset) = setval
          ierrc=0
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE setOne_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all values in the PETSc vector with a scalar value
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!>
    SUBROUTINE setAll_scalar_NativeDistributedVectorType(thisVector,setval,ierr)
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      !
      INTEGER(SIK) :: ierrc
      ierrc=-1
      IF(thisVector%isInit) THEN
        thisVector%b = setval
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE setAll_scalar_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all the values in the PETSc vector with an array of values
!> @param declare the vector type to act on
!> @param setval the array of values to be set
!>
    SUBROUTINE setAll_array_NativeDistributedVectorType(thisVector,setval,ierr)
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      INTEGER(SIK) :: i
      !
      INTEGER(SIK) :: ierrc

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-3
        IF(SIZE(setval) == thisVector%n) THEN
          thisVector%b = setval((thisVector%offset+1):(thisVector%offset + thisVector%nlocal))
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE setAll_array_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets selected values in the PETSc vector with an array of values
!> @param declare the vector type to act on
!> @param indices a list of indices (global if parallel) for which data is being set
!> @param setval the array of values to be set (same size as indices)
!>
    SUBROUTINE setSelected_NativeDistributedVectorType(thisVector,indices,setval,ierr)
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: indices(:)
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      INTEGER(SIK) :: ierrc,i

      REQUIRE(thisVector%isInit)
      REQUIRE(SIZE(setval) == SIZE(indices))

      DO i=1,SIZE(setval)
        CALL setOne_NativeDistributedVectorType(thisVector,indices(i),setval(i),ierrc)
      END DO

      IF(PRESENT(ierr)) ierr=ierrc

    END SUBROUTINE setSelected_NativeDistributedVectorType

!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the PETSc vector with a scalar value
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE setRange_scalar_NativeDistributedVectorType(thisVector,istt,istp,setval,ierr)
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      INTEGER(SIK) :: i
      !
      INTEGER(SIK) :: ierrc

      REQUIRE(thisVector%isInit)
      REQUIRE(0 < istt .AND. istp <= thisVector%n)

      IF(istt <= (thisVector%offset + thisVector%nlocal)  .OR. istp > thisVector%offset) THEN
        thisVector%b(MAX(istt,thisVector%offset+1):MIN(istp,thisVector%offset+thisVector%nlocal)) = setval
      ENDIF

      IF(PRESENT(ierr)) ierr=ierrc
    END SUBROUTINE setRange_scalar_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the PETSc vector with an array of values
!> @param declare the vector type to act on
!> @param setval the scalar value to be set
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!>
    SUBROUTINE setRange_array_NativeDistributedVectorType(thisVector,istt,istp,setval,ierr)
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(IN) :: setval(:)
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      INTEGER(SIK) :: low,high
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
            IF(istt <= (thisVector%offset + thisVector%nlocal)  .OR. istp > thisVector%offset) THEN
              low = MAX(istt,thisVector%offset+1)
              high = MIN(istp,thisVector%offset+thisVector%nlocal)
              thisVector%b((low - thisVector%offset):(high - thisVector%offset)) = setval((low - istt + 1):(high - istt + 1))
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE setRange_array_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets one values in the PETSc vector
!> @param declares the vector type to act on
!> @param i the ith location in the vector
!>
    SUBROUTINE getOne_NativeDistributedVectorType(thisVector,i,getval,ierr)
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      REAL(SRK),INTENT(OUT) :: getval
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      !
      INTEGER(SIK) :: ierrc
      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-2
        IF((i <= thisVector%offset + thisVector%nlocal) .AND. (i > thisVector%offset)) THEN
          getVal = thisVector%b(i - thisVector%offset)
          ierrc=0
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE getOne_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets all values in the PETSc vector
!> For parallel vectors, will only return the data owned by this domain.  Use getSelected to
!> specify which data to get.
!> @param declares the vector type to act on
!> @param getval Correctly sized array that will be filled with contents of this vector
    SUBROUTINE getAll_NativeDistributedVectorType(thisVector,getval,ierr)
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
      REAL(SRK),INTENT(OUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      !
      INTEGER(SIK) :: ierrc

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc=-3
        IF(SIZE(getval) == thisVector%n) THEN
          getval = thisVector%b
          ierrc=0
        ENDIF
      ENDIF
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE getAll_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets selected values in the PETSc vector
!> @param declares the vector type to act on
!> @param indices A list of indices at which to get vector values.  For parallel vectors,
!>        you must use the global indices.
!> @param getval Correctly sized array that will be filled with contents of this vector.
!>        Must be the same size as indices.
    SUBROUTINE getSelected_NativeDistributedVectorType(thisVector,indices,getval,ierr)
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: indices(:)
      REAL(SRK),INTENT(OUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      LOGICAL(SBK) :: inMemFlag
      !
      INTEGER(SIK) :: ierrc,i

      REQUIRE(thisVector%isInit)
      REQUIRE(SIZE(getval) == SIZE(indices))
      REQUIRE(SIZE(indices) <= thisVector%n)

      DO i=1,SIZE(indices)
        CALL inLocalMem_single_NativeDistributedVectorType(thisVector,indices(i),inMemFlag,ierrc)
        IF(inMemFlag) THEN
          getval(i) = thisVector%b(indices(i) - thisVector%offset)
        END IF
      END DO

      ierrc=0
      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE getSelected_NativeDistributedVectorType

!
!-------------------------------------------------------------------------------
!> @brief Gets a range of  values in the PETSc vector
!> @param declares the vector type to act on
!> @param istt the starting point of the range (Use global indices for parallel vectors)
!> @param istp the stopping point in the range (Use global indices for parallel vectors)
!>
    SUBROUTINE getRange_NativeDistributedVectorType(thisVector,istt,istp,getval,ierr)
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: istt
      INTEGER(SIK),INTENT(IN) :: istp
      REAL(SRK),INTENT(OUT) :: getval(:)
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      INTEGER(SIK) :: i, srcLow, srcHigh, destLow, destHigh
      INTEGER(SIK) :: ierrc


      REQUIRE(thisVector%isInit)
      REQUIRE(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n)
      REQUIRE(istp - istt+1 == SIZE(getVal))

      srcLow = MAX(istt,thisVector%offset+1) - thisVector%offset
      srcHigh = MIN(istp,thisVector%offset+thisVector%nlocal) - thisVector%offset
      IF(istt > thisVector%offset) THEN
        destLow = 1
      ELSE
        destLow = thisVector%offset - istt + 2
      ENDIF
      IF(istp <= thisVector%offset + thisVector%nlocal) THEN
        destHigh = istp - istt + 1
      ELSE
        destHigh = istp - istt + 1 - (istp - thisVector%offset - thisVector%nlocal)
      ENDIF

      getval(destLow:destHigh) = thisVector%b(srcLow:srcHigh)

      ierrc = 0

      IF(PRESENT(ierr)) ierr=ierrc
    ENDSUBROUTINE getRange_NativeDistributedVectorType

    SUBROUTINE inLocalMem_single_NativeDistributedVectorType(thisVector,i,ret,ierr)
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(IN) :: i
      LOGICAL(SBK),INTENT(OUT) :: ret
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
      INTEGER(SIK) :: ierrc

      ierrc=-1
      IF(thisVector%isInit) THEN
        ierrc = 0
        IF(i > thisVector%offset .AND. i <= thisVector%offset + thisVector%nlocal) THEN
          ret = .TRUE.
        ELSE
          ret = .FALSE.
        END IF
      END IF
      IF(PRESENT(ierr)) ierr = ierrc
    ENDSUBROUTINE inLocalMem_single_NativeDistributedVectorType

    SUBROUTINE assemble_NativeDistributedVectorType(thisVector,ierr)
      CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr

    ENDSUBROUTINE
END MODULE VectorTypes_Native
