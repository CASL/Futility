!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Futility-native implementations of VectorType
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

#ifdef HAVE_MPI
#include <mpif.h>
#endif

PRIVATE

!
! List of public members
PUBLIC :: NativeVectorType
PUBLIC :: RealVectorType
#ifdef HAVE_MPI
PUBLIC :: NativeDistributedVectorType
#endif

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

#ifdef HAVE_MPI
TYPE,EXTENDS(NativeVectorType) :: NativeDistributedVectorType
  !> MPI comm ID
  INTEGER(SIK) :: comm=-1
  !> Chunk size (minimum # of elements to subdivide to)
  INTEGER(SIK) :: chunkSize=1

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
ENDTYPE NativeDistributedVectorType
#endif

  INTEGER(SIK) :: ierrc

!> Name of module
CHARACTER(LEN=*),PARAMETER :: modName='VECTORTYPES_NATIVE'

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the real vector with a parameter list
!> @param thisVector the vector type to initialize
!> @param Params the parameterlist to use
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
!> @brief Clears a real vector
!> @param thisVector the vector type to act on
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
!> @brief Sets a scalar value in the real vector
!> @param thisVector the vector type to act on
!> @param i the location in the vector to set
!> @param setval the value to be set
!> @param ierr optional error code return (not used for native types)
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
!> @brief Sets all values in the real vector to a scalar value
!> @param thisVector the vector type to act on
!> @param setval the scalar value to be set
!> @param ierr optional error code return (not used for native types)
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
!> @brief Sets all the values in the real vector to an array of values
!> @param thisVector the vector type to act on
!> @param setval the array of values to be set
!> @param ierr optional error code return (not used for native types)
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
!> @param thisVector the vector type to act on
!> @param indices a list of indices in the vector to set
!> @param setval the array of values to be set (must be same size as indices)
!> @param ierr optional error code return (not used for native types)
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
!> @brief Sets a range of values in the real vector to a scalar value
!> @param thisVector the vector type to act on
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!> @param setval the scalar value to be set
!> @param ierr optional error code return (not used for native types)
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
!> @param thisVector the vector type to act on
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!> @param setval the array of values to set to
!> @param ierr optional error code return (not used for native types)
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
!> @param thisVector the vector type to act on
!> @param i the vector location to access
!> @param getval the variable to store the vector element
!> @param ierr optional error code return (not used for native types)
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
!> @param thisVector the vector type to act on
!> @param getval Correctly sized array that will be filled with vector contents
!> @param ierr optional error code return (not used for native types)
!>
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
!> @param thisVector the vector type to act on
!> @param indices A list of the indices to access
!> @param getval Correctly sized array that will be filled with vector contents
!> @param ierr optional error code return (not used for native types)
!>
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
!> @param getval Correctly sized array to be filled with contents in range
!> @param ierr optional error code return (not used for native types)
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

#ifdef HAVE_MPI
!
!-------------------------------------------------------------------------------
!> @brief Initializes the native distriubted vector with a parameterlist
!> @param thisVector the vector to initialize
!> @param Params the parameterlist
!>
SUBROUTINE init_NativeDistributedVectorType(thisVector,Params)
  CHARACTER(LEN=*),PARAMETER :: myName='init_NativeDistributedVectorType'
  CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
  TYPE(ParamType),INTENT(IN) :: Params
  TYPE(ParamType) :: validParams
  INTEGER(SIK) :: n, chunksize, comm, nProc,rank,mpierr,offset,nlocal
  INTEGER(SIK),ALLOCATABLE :: offsets(:)

  IF (.NOT. thisVector%isInit) THEN
    !Check to set up required and optional param lists.
    IF(.NOT. VectorType_Paramsflag) CALL VectorType_Declare_ValidParams()

    !Validate against the reqParams and OptParams
    validParams=Params
    CALL validParams%validate(NativeDistributedVectorType_reqParams, &
        NativeDistributedVectorType_optParams)

    !Pull Data from Parameter List
    CALL validParams%get('VectorType->n',n)
    CALL validParams%get('VectorType->MPI_Comm_ID',comm)
    CALL validParams%get('VectorType->nlocal',nlocal)

    chunksize=1
    IF (validParams%has('VectorType->chunkSize')) THEN
      CALL validParams%get('VectorType->chunkSize',chunkSize)
    ENDIF

    REQUIRE(.NOT. thisVector%isInit)
    REQUIRE(n >= 1)
    REQUIRE(chunkSize > 0)
    REQUIRE(MOD(n,chunkSize) == 0)

    thisVector%isInit=.TRUE.
    thisVector%n=n
    thisVector%chunkSize=chunkSize
    thisVector%comm=comm

    CALL MPI_Comm_rank(comm,rank,mpierr)
    CALL MPI_Comm_size(comm,nproc,mpierr)

    ! Default to greedy partitioning, respecting chunk size
    IF (nlocal < 0) THEN
      n = n/chunkSize
      IF (rank < MOD(n,nproc)) THEN
        offset=(rank)*(n/nproc+1)
        nlocal=n/nproc+1
      ELSE
        offset=(rank)*(n/nproc)+MOD(n,nproc)
        nlocal=n/nproc
      ENDIF
      offset=offset*chunkSize
      nlocal=nlocal*chunkSize
    ELSE
      REQUIRE(MOD(nlocal,chunksize) == 0)
      ALLOCATE(offsets(nproc))
      CALL MPI_AllGather(nlocal,1,MPI_INTEGER,offsets(1),1,MPI_INTEGER,comm,mpierr)
      offset=SUM(offsets(1:rank))
    ENDIF

    ALLOCATE(thisVector%b((offset+1):(offset+nlocal)))
    thisVector%b=0.0_SRK

    CALL validParams%clear()
  ELSE
    CALL eVectorType%raiseError('Incorrect call to '// &
        modName//'::'//myName//' - VectorType already initialized')
  ENDIF
    
ENDSUBROUTINE init_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Clears the native distributed vector
!> @param thisVector the vector type to act on
!>
SUBROUTINE clear_NativeDistributedVectorType(thisVector)
  CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector

  thisVector%isInit=.FALSE.
  thisVector%n=0
  thisVector%comm=-1
  thisVector%chunksize=1
  IF (ALLOCATED(thisVector%b)) CALL demallocA(thisVector%b)

ENDSUBROUTINE clear_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets one value in the native distributed vector
!>        will fail if value not in range
!> @param thisVector the vector type to act on
!> @param i the location in the vector to set
!> @param setval the value to be set
!> @param ierr optional error code return (not used for native types)
!>
SUBROUTINE setOne_NativeDistributedVectorType(thisVector,i,setval,ierr)
  CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
  INTEGER(SIK),INTENT(IN) :: i
  REAL(SRK),INTENT(IN) :: setval
  INTEGER(SIK),INTENT(OUT),OPTIONAL ::ierr

  REQUIRE(thisVector%isInit)
  REQUIRE(i >= LBOUND(thisVector%b,1) .AND. i <= UBOUND(thisVector%b,1))
  thisVector%b(i)=setval

ENDSUBROUTINE setOne_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all values in the native distributed vector
!>        to a specified scalar value
!> @param thisVector the vector type to act on
!> @param setval the scalar value to be set
!> @param ierr optional error code return (not used for native types)
!>
SUBROUTINE setAll_scalar_NativeDistributedVectorType(thisVector,setval,ierr)
  CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
  REAL(SRK),INTENT(IN) :: setval
  INTEGER(SIK),INTENT(OUT),OPTIONAL ::ierr

  REQUIRE(thisVector%isInit)
  thisVector%b=setval

ENDSUBROUTINE setAll_scalar_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets all the values in the native distributed vector with
!>        an array of values.
!> @param thisVector the vector type to act on
!> @param setval the array of values to be set
!> @param ierr optional error code return (not used for native types)
!>
SUBROUTINE setAll_array_NativeDistributedVectorType(thisVector,setval,ierr)
  CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
  REAL(SRK),INTENT(IN) :: setval(:)
  INTEGER(SIK),INTENT(OUT),OPTIONAL ::ierr

  REQUIRE(thisVector%isInit)
  REQUIRE(SIZE(setval) == SIZE(thisVector%b) .OR. SIZE(setval)==thisVector%n)

  IF (SIZE(setval) == thisVector%n) THEN
    thisVector%b(:)=setval(LBOUND(thisVector%b,1):UBOUND(thisVector%b,1))
  ELSE
    thisVector%b=setval
  ENDIF

ENDSUBROUTINE setAll_array_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets selected values in the native distributed vector to an array
!> @param thisVector the vector type to act on
!> @param indices a list of global indices data to be set
!> @param setval the array of values to be set (same size as indices)
!> @param ierr optional error code return (not used for native types)
!>
SUBROUTINE setSelected_NativeDistributedVectorType(thisVector,indices,setval,ierr)
  CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
  INTEGER(SIK),INTENT(IN) :: indices(:)
  REAL(SRK),INTENT(IN) :: setval(:)
  INTEGER(SIK),INTENT(OUT),OPTIONAL ::ierr

  REQUIRE(thisVector%isInit)
  REQUIRE(SIZE(setval) == SIZE(indices))
  REQUIRE(ALL(indices>0) .AND. ALL(indices <= thisVector%n))

  WHERE (indices <= UBOUND(thisVector%b,1) .AND. indices >= LBOUND(thisVector%b,1)) &
   thisVector%b(indices)=setval

ENDSUBROUTINE setSelected_NativeDistributedVectorType

!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the native distributed vector to a scalar value
!> @param thisVector the vector type to act on
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!> @param setval the scalar value to be set
!> @param ierr optional error code return (not used for native types)
!>
SUBROUTINE setRange_scalar_NativeDistributedVectorType(thisVector,istt,istp,setval,ierr)
  CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
  REAL(SRK),INTENT(IN) :: setval
  INTEGER(SIK),INTENT(IN) :: istt
  INTEGER(SIK),INTENT(IN) :: istp
  INTEGER(SIK),INTENT(OUT),OPTIONAL ::ierr

  REQUIRE(thisVector%isInit)
  REQUIRE(istt <= istp)
  REQUIRE(istt > 0 .AND. istp <=thisVector%n)

  IF(istt <= UBOUND(thisVector%b,1) .OR. istp >= LBOUND(thisVector%b,1)) &
      thisVector%b(MAX(istt,UBOUND(thisVector%b,1)):MIN(istp,LBOUND(thisVector%b,1))) &
      = setval

ENDSUBROUTINE setRange_scalar_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Sets a range of values in the native distributed vector to
!>        an array of values
!> @param thisVector the vector type to act on
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!> @param setval the array of values to be set
!> @param ierr optional error code return (not used for native types)
!>
SUBROUTINE setRange_array_NativeDistributedVectorType(thisVector,istt,istp,setval,ierr)
  CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
  REAL(SRK),INTENT(IN) :: setval(:)
  INTEGER(SIK),INTENT(IN) :: istt
  INTEGER(SIK),INTENT(IN) :: istp
  INTEGER(SIK),INTENT(OUT),OPTIONAL ::ierr
  INTEGER(SIK) :: lowSrc,highSrc,lowDest,highDest

  REQUIRE(thisVector%isInit)
  REQUIRE(istt > 0 .AND. istp <=thisVector%n)
  REQUIRE(istt <= istp)
  REQUIRE(SIZE(setval)==istp-istt+1)

  IF(istt <= UBOUND(thisVector%b,1) .OR. istp >= LBOUND(thisVector%b,1)) THEN
    lowSrc=MAX(1,LBOUND(thisVector%b,1)-istt)
    highSrc=SIZE(setval)-MAX(0,istp-UBOUND(thisVector%b,1))
    lowDest=MAX(istt,LBOUND(thisVector%b,1))
    highDest=MIN(istp,UBOUND(thisVector%b,1))
    thisVector%b(lowDest:highDest)=setval(lowSrc:highSrc)
  ENDIF

ENDSUBROUTINE setRange_array_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Accesses a scalar from the native distributed vector
!> @param thisVector the vector type to act on
!> @param i the location in the vector to access
!> @param getval the variable to store output
!> @param ierr optional error code return (not used for native types)
!>
SUBROUTINE getOne_NativeDistributedVectorType(thisVector,i,getval,ierr)
  CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
  INTEGER(SIK),INTENT(IN) :: i
  REAL(SRK),INTENT(OUT) :: getval
  INTEGER(SIK),INTENT(OUT),OPTIONAL ::ierr

  REQUIRE(thisVector%isInit)
  REQUIRE(i >= LBOUND(thisVector%b,1) .AND. i <= UBOUND(thisVector%b,1))

  getval=thisVector%b(i)

ENDSUBROUTINE getOne_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets all values in the native distributed vector. Will only return
!>        data owned by this domain.
!> @param thisVector the vector type to act on
!> @param getval Array to be filled with contents of this vector. Must be
!>        correctly sized
!> @param ierr optional error code return (not used for native types)
!>
SUBROUTINE getAll_NativeDistributedVectorType(thisVector,getval,ierr)
  CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
  REAL(SRK),INTENT(OUT) :: getval(:)
  INTEGER(SIK),INTENT(OUT),OPTIONAL ::ierr

  REQUIRE(thisVector%isInit)
  REQUIRE(SIZE(getVal) == SIZE(thisVector%b))

  getval=thisVector%b(:)

ENDSUBROUTINE getAll_NativeDistributedVectorType
!
!-------------------------------------------------------------------------------
!> @brief Gets selected values in the native distributed vector
!> @param thisVector the vector type to act on
!> @param indices A list of global indices at which to get vector values.
!> @param getval Array to be filled with requested data. Must be
!>        correctly sized.
!> @param ierr optional error code return (not used for native types)
!>
SUBROUTINE getSelected_NativeDistributedVectorType(thisVector,indices,getval,ierr)
  CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
  INTEGER(SIK),INTENT(IN) :: indices(:)
  REAL(SRK),INTENT(OUT) :: getval(:)
  INTEGER(SIK),INTENT(OUT),OPTIONAL ::ierr
  INTEGER(SIK) :: i

  REQUIRE(thisVector%isInit)
  REQUIRE(SIZE(getval) == SIZE(indices))

  DO i=1,SIZE(indices)
    IF (indices(i) >= LBOUND(thisVector%b,1) .AND. indices(i) <= UBOUND(thisVector%b,1)) &
        getval(i) = thisVector%b(indices(i))
  ENDDO

ENDSUBROUTINE getSelected_NativeDistributedVectorType

!
!-------------------------------------------------------------------------------
!> @brief Gets a range of values in the native distributed vector
!> @param thisVector the vector type to act on
!> @param istt the starting point of the range
!> @param istp the stopping point in the range
!> @param getval the vector to output values to
!> @param ierr optional error code return (not used for native types)
!>
SUBROUTINE getRange_NativeDistributedVectorType(thisVector,istt,istp,getval,ierr)
  CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
  INTEGER(SIK),INTENT(IN) :: istt
  INTEGER(SIK),INTENT(IN) :: istp
  REAL(SRK),INTENT(OUT) :: getval(:)
  INTEGER(SIK),INTENT(OUT),OPTIONAL ::ierr
  INTEGER(SIK) :: lowDest, highDest, lowSrc, highSrc

  REQUIRE(thisVector%isInit)
  REQUIRE(0 < istt .AND. istt <= istp .AND. istp <= thisVector%n)
  REQUIRE(istp-istt+1 == SIZE(getVal))

  IF(istt <= UBOUND(thisVector%b,1) .OR. istp >= LBOUND(thisVector%b,1)) THEN
    lowDest=MAX(1,LBOUND(thisVector%b,1)-istt)
    highDest=SIZE(getval)-MAX(0,istp-UBOUND(thisVector%b,1))
    lowSrc=MAX(istt,LBOUND(thisVector%b,1))
    highSrc=MIN(istp,UBOUND(thisVector%b,1))
    getval(lowDest:highDest)=thisVector%b(lowSrc:highSrc)
  ENDIF

ENDSUBROUTINE getRange_NativeDistributedVectorType

SUBROUTINE assemble_NativeDistributedVectorType(thisVector,ierr)
  CLASS(NativeDistributedVectorType),INTENT(INOUT) :: thisVector
  INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierr
  ! This routine does not need to do anything
ENDSUBROUTINE
#endif

ENDMODULE VectorTypes_Native
