!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testVectorTypesParallel
#include "UnitTest.h"
USE ISO_C_BINDING
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE ExceptionHandler
USE trilinos_interfaces
USE ParameterLists
USE ParallelEnv
USE VectorTypes

IMPLICIT NONE

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
#undef IS
PetscErrorCode  :: ierr
#else
#ifdef HAVE_MPI
#include <mpif.h>
INTEGER :: ierr
#endif
#endif
TYPE(ExceptionHandlerType),POINTER :: e

CREATE_TEST('Test Vector Types Parallel')

!Configure exception handler for test
ALLOCATE(e)
CALL e%setStopOnError(.FALSE.)
CALL e%setQuietMode(.TRUE.)
CALL eVectorType%addSurrogate(e)

#ifdef HAVE_MPI
CALL MPI_Init(ierr)
#endif
#ifdef FUTILITY_HAVE_PETSC
CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

REGISTER_SUBTEST("Vector Types",testVector)

#ifdef FUTILITY_HAVE_PETSC
CALL PetscFinalize(ierr)
#endif
#ifdef HAVE_MPI
CALL MPI_Finalize(ierr)
#endif
FINALIZE_TEST()

CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testVector()
#if defined(FUTILITY_HAVE_PETSC) && defined(HAVE_MPI)
  CLASS(VectorType),ALLOCATABLE :: thisVector
  TYPE(ParamType) :: pList
  LOGICAL(SBK) :: bool
  INTEGER(SIK) :: rank, nproc, mpierr, i
  REAL(SRK) :: val
  REAL(SRK),ALLOCATABLE :: getval(:)
  CALL MPI_Comm_rank(MPI_COMM_WORLD,rank,mpierr)
  CALL MPI_Comm_size(MPI_COMM_WORLD,nproc,mpierr)
  ASSERT(nproc==2, 'nproc valid')
  !Perform test of init function
  !first check intended init path (m provided)
  CALL pList%clear()
  CALL pList%add('VectorType->n',20)
  CALL pList%add('VectorType->MPI_Comm_ID',MPI_COMM_WORLD)
  CALL pList%add('VectorType->nlocal',10)
  ALLOCATE(PETScVectorType :: thisVector)
  CALL thisVector%init(pList)
  SELECTTYPE(thisVector)
    TYPE IS(PETScVectorType)
      !check for success
      bool = thisVector%isInit.AND.thisVector%n == 20
      ASSERT(bool, 'petscvec%init(...)')
      CALL VecGetSize(thisVector%b,i,ierr)
      ASSERT(i == 20, 'petscvec%init(...)')
  ENDSELECT

  ! Test setting/getting single elements at a time
  CALL thisVector%set(1,1._SRK)
  CALL thisVector%set(2,2._SRK)
  CALL thisVector%set(3,3._SRK)
  CALL thisVector%set(18,18._SRK)
  CALL thisVector%set(19,19._SRK)
  CALL thisVector%set(20,20._SRK)

  IF(rank==0) THEN
    CALL thisVector%get(1,val)
    ASSERT(val==1._SRK, "getOne")
    CALL thisVector%get(2,val)
    ASSERT(val==2._SRK, "getOne")
    CALL thisVector%get(3,val)
    ASSERT(val==3._SRK, "getOne")
  ELSE
    CALL thisVector%get(18,val)
    ASSERT(val==18._SRK, "getOne")
    CALL thisVector%get(19,val)
    ASSERT(val==19._SRK, "getOne")
    CALL thisVector%get(20,val)
    ASSERT(val==20._SRK, "getOne")
  ENDIF

  ! Test setting/getting selected elements all at once
  CALL thisVector%set([(REAL(i, SRK), i=1, 20)])
  ALLOCATE(getval(10))
  IF(rank==0) THEN
     CALL thisVector%get([(i, i=1, 10)], getval)
     ASSERT(ALL(getval==[(REAL(i, SRK), i=1, 10)]), 'getAll')
  ELSE
     CALL thisVector%get([(i, i=11, 20)], getval)
     ASSERT(ALL(getval==[(REAL(i, SRK), i=11, 20)]), 'getAll')
  ENDIF


  ! Use getAll with same data
  CALL thisVector%get(getval)
  IF(rank==0) THEN
     ASSERT(ALL(getval==[(REAL(i, SRK), i=1, 10)]), 'getAll')
  ELSE
     ASSERT(ALL(getval==[(REAL(i, SRK), i=11, 20)]), 'getAll')
  ENDIF

  ! Test setSelected by overwiting a few entries
  CALL thisVector%set([1, 5, 15, 20], [100._SRK, 105._SRK, 115._SRK, 120._SRK])
  DEALLOCATE(getval)
  ALLOCATE(getval(2))
  IF(rank==0) THEN
     CALL thisVector%get([1, 5], getval)
     ASSERT(getval(1)==100._SRK, 'set/getSelected')
     ASSERT(getval(2)==105._SRK, 'set/getSelected')
  ELSE
     CALL thisVector%get([15, 20], getval)
     ASSERT(getval(1)==115._SRK, 'set/getSelected')
     ASSERT(getval(2)==120._SRK, 'set/getSelected')
  ENDIF

  ! Test setting/getting elemnts by range
  CALL thisVector%set(1, 3, [1._SRK, 2._SRK, 3._SRK])
  CALL thisVector%set(18, 20, [18._SRK, 19._SRK, 20._SRK])
  DEALLOCATE(getval)
  ALLOCATE(getval(3))
  IF(rank==0) THEN
     CALL thisVector%get(1, 3, getval)
     ASSERT(ALL(getval==[1._SRK, 2._SRK, 3._SRK]), 'getRange')
  ELSE
     CALL thisVector%get(18, 20, getval)
     ASSERT(ALL(getval==[18._SRK, 19._SRK, 20._SRK]), 'getRange')
  ENDIF

  DEALLOCATE(thisVector)
  CALL pList%clear()

#endif
ENDSUBROUTINE testVector
ENDPROGRAM testVectorTypesParallel
