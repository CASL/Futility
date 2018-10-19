!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testMatrixTypesParallel
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE ISO_C_BINDING
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE trilinos_interfaces
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes


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
  TYPE(ExceptionHandlerType),TARGET :: e

  CREATE_TEST('Test Matrix Types Parallel')


#ifdef HAVE_MPI
  CALL MPI_Init(ierr)
#endif
#ifdef FUTILITY_HAVE_PETSC
  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL eParams%addSurrogate(e)
  CALL eMatrixType%addSurrogate(e)

  REGISTER_SUBTEST("Matrix Types",testMatrix)


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
    SUBROUTINE testMatrix()
#ifdef HAVE_MPI
      TYPE(ParamType) :: pList
      INTEGER(SIK) :: rank, nproc, mpierr
#if defined(FUTILITY_HAVE_PETSC) || defined(FUTILITY_HAVE_Trilinos)
      CLASS(DistributedMatrixType),ALLOCATABLE :: thisMatrix
      REAL(SRK) :: val
#endif

      CALL MPI_Comm_rank(MPI_COMM_WORLD,rank,mpierr)
      CALL MPI_Comm_size(MPI_COMM_WORLD,nproc,mpierr)

      ASSERT(nproc==2, 'nproc valid')

      ! Create matrix that looks like this
      ! 1 0 2 0
      ! 0 3 0 4
      ! 0 0 5 0
      ! 6 7 0 0
      ! Rank 0 owns rows 1 and 2
      ! Rank 1 owns rows 3 and 4

      CALL pList%add('MatrixType->n',4_SNK)
      CALL pList%add('MatrixType->nLocal',2_SNK)
      CALL pList%add('MatrixType->nnz',2_SNK)
      CALL pList%add('MatrixType->onz',2_SNK)
      CALL pList%add('MatrixType->matType',SPARSE)
      CALL pList%add('MatrixType->isSym',.FALSE.)
      CALL pList%add('MatrixType->MPI_COMM_ID',MPI_COMM_WORLD)

#ifdef FUTILITY_HAVE_PETSC
      ALLOCATE(PETScMatrixType::thisMatrix)

      CALL thisMatrix%init(pList)


      !Perform test of init function
      ASSERT(thisMatrix%isInit, "init")
      ASSERT(thisMatrix%n==4, "global size")

      ! Test setting/getting single elements at a time
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,3,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      CALL thisMatrix%set(2,4,4._SRK)
      CALL thisMatrix%set(3,3,5._SRK)
      CALL thisMatrix%set(4,1,6._SRK)
      CALL thisMatrix%set(4,2,7._SRK)

      IF(rank==0) THEN
        CALL thisMatrix%get(1,1,val)
        ASSERT(val .APPROXEQ. 1._SRK, "getOne")
        CALL thisMatrix%get(1,3,val)
        ASSERT(val .APPROXEQ. 2._SRK, "getOne")
        CALL thisMatrix%get(2,2,val)
        ASSERT(val .APPROXEQ. 3._SRK, "getOne")
        CALL thisMatrix%get(2,4,val)
        ASSERT(val .APPROXEQ. 4._SRK, "getOne")
      ELSE
        CALL thisMatrix%get(3,3,val)
        ASSERT(val .APPROXEQ. 5._SRK, "getOne")
        CALL thisMatrix%get(4,1,val)
        ASSERT(val .APPROXEQ. 6._SRK, "getOne")
        CALL thisMatrix%get(4,2,val)
        ASSERT(val .APPROXEQ. 7._SRK, "getOne")
      ENDIF

      ! Test setting row all at once
      CALL thisMatrix%setRow(1,[1, 3],[10._SRK, 11._SRK])
      CALL thisMatrix%setRow(2,[2, 4],[12._SRK, 13._SRK])
      CALL thisMatrix%setRow(3,[3],[14._SRK])
      CALL thisMatrix%setRow(4,[1, 2],[15._SRK, 16._SRK])

      IF(rank==0) THEN
        CALL thisMatrix%get(1,1,val)
        ASSERT(val .APPROXEQ. 10._SRK, "getOne")
        CALL thisMatrix%get(1,3,val)
        ASSERT(val .APPROXEQ. 11._SRK, "getOne")
        CALL thisMatrix%get(2,2,val)
        ASSERT(val .APPROXEQ. 12._SRK, "getOne")
        CALL thisMatrix%get(2,4,val)
        ASSERT(val .APPROXEQ. 13._SRK, "getOne")
      ELSE
        CALL thisMatrix%get(3,3,val)
        ASSERT(val .APPROXEQ. 14._SRK, "getOne")
        CALL thisMatrix%get(4,1,val)
        ASSERT(val .APPROXEQ. 15._SRK, "getOne")
        CALL thisMatrix%get(4,2,val)
        ASSERT(val .APPROXEQ. 16._SRK, "getOne")
      ENDIF

      CALL thisMatrix%clear()
      DEALLOCATE(thisMatrix)
#endif
      CALL pList%clear()

#endif
    ENDSUBROUTINE testMatrix
ENDPROGRAM testMatrixTypesParallel
