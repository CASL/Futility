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
      IMPLICIT NONE
      
      TYPE(ParamType) :: pList,optListMat
      INTEGER(SIK) :: rank,nproc,mpierr
      CLASS(DistributedMatrixType),ALLOCATABLE :: thisMatrix
      REAL(SRK) :: val
      LOGICAL(SBK) :: bool


      CALL MPI_Comm_rank(MPI_COMM_WORLD,rank,mpierr)
      CALL MPI_Comm_size(MPI_COMM_WORLD,nproc,mpierr)

      ASSERT(nproc==2, 'nproc valid')
!Test for distributed banded matrices
      ALLOCATE(DistributedBandedMatrixType :: thisMatrix)
      SELECTTYPE(thisMatrix)
        TYPE IS(DistributedBandedMatrixType)
        !test clear
        !make matrix w/out using untested init
        thisMatrix%isInit=.TRUE.
        thisMatrix%n=10
        thisMatrix%m=15
        thisMatrix%nband=4
        thisMatrix%myband=4
        thisMatrix%isCreated=.TRUE.
        thisMatrix%isAssembled=.TRUE.
        thisMatrix%comm=33
        ALLOCATE(thisMatrix%b(4))
        ALLOCATE(thisMatrix%b(2)%elem(5))
      ENDSELECT
      CALL thisMatrix%clear()
      SELECTTYPE(thisMatrix)
        TYPE IS(DistributedBandedMatrixType)
          bool = (.NOT.(thisMatrix%isInit).AND.(thisMatrix%n == 0)) &
              .AND.((thisMatrix%m == 0) & 
              .AND.(thisMatrix%nband == 0) & 
              .AND.(thisMatrix%myband == 0) &
              .AND.(thisMatrix%isCreated == .FALSE.) &
              .AND.(thisMatrix%isAssembled == .FALSE.) &
              .AND.(thisMatrix%comm == MPI_COMM_NULL) &
              .AND.(thisMatrix%m == 0) &
              .AND.(.NOT.ALLOCATED(thisMatrix%b)))
          ASSERT(bool, 'DistributedBandedMatrixType%clear()')
      ENDSELECT
      !check init
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',15_SNK)
      CALL pList%add('MatrixType->nband',3_SNK)
      CALL pList%add('bandi',(/1_SIK,1_SIK,1_SIK/))
      CALL pList%add('bandj',(/1_SIK,2_SIK,3_SIK/))
      CALL pList%add('bandl',(/4_SIK,3_SIK,2_SIK/))
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(DistributedBandedMatrixType)
          bool = (( thisMatrix%isInit).AND.(thisMatrix%n == 10)) &
              .AND.((thisMatrix%m == 15).AND.(thisMatrix%nband == 3))
          ASSERT(bool, 'banded%init(...)')
          bool = ((SIZE(thisMatrix%b) == 3).AND. &
                  (SIZE(thisMatrix%b(2)%elem) == 3) .AND. &
                  (thisMatrix%b(2)%ib == 1) .AND. &
                  (thisMatrix%b(2)%jb == 2) .AND. &
                  (thisMatrix%b(2)%ie == 3) .AND. &
                  (thisMatrix%b(2)%je == 4) .AND. &
                  (thisMatrix%b(2)%didx == 1))
          ASSERT(bool, 'banded%init(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      !test with double init (isInit==true on 2nd try)
      SELECTTYPE(thisMatrix)
        TYPE IS(DistributedBandedMatrixType); thisMatrix%m=1
      ENDSELECT
      CALL thisMatrix%init(pList)
      SELECTTYPE(thisMatrix)
        TYPE IS(DistributedBandedMatrixType)
          bool = thisMatrix%m == 1
          ASSERT(bool, 'banded%init(...)')
      ENDSELECT
      CALL thisMatrix%clear()
      WRITE(*,*) rank, "MADE IT TO 4"
      !test with n<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->m',10_SNK)
      CALL pList%add('MatrixType->nband',3_SNK)
      CALL pList%add('bandi',(/1_SIK,1_SIK,1_SIK/))
      CALL pList%add('bandj',(/1_SIK,2_SIK,3_SIK/))
      CALL pList%add('bandl',(/4_SIK,3_SIK,2_SIK/))
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'banded%init(...)')
      CALL thisMatrix%clear()
      WRITE(*,*) rank, "MADE IT TO 5"
      !test with m<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',-1_SNK)
      CALL pList%add('MatrixType->nband',3_SNK)
      CALL pList%add('bandi',(/1_SIK,1_SIK,1_SIK/))
      CALL pList%add('bandj',(/1_SIK,2_SIK,3_SIK/))
      CALL pList%add('bandl',(/4_SIK,3_SIK,2_SIK/))
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'banded%init(...)')
      CALL thisMatrix%clear()
      !test with nband<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',15_SNK)
      CALL pList%add('MatrixType->nband',-1_SNK)
      CALL pList%add('bandi',(/1_SIK,1_SIK,1_SIK/))
      CALL pList%add('bandj',(/1_SIK,2_SIK,3_SIK/))
      CALL pList%add('bandl',(/4_SIK,3_SIK,2_SIK/))
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'banded%init(...)')
      CALL thisMatrix%clear()
      !test with SIZE(bandi)/=SIZE(bandl) 
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',15_SNK)
      CALL pList%add('MatrixType->nband',3_SNK)
      CALL pList%add('bandi',(/1_SIK,1_SIK,1_SIK/))
      CALL pList%add('bandj',(/1_SIK,2_SIK,3_SIK/))
      CALL pList%add('bandl',(/4_SIK,3_SIK/))
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'banded%init(...)')
      CALL thisMatrix%clear()
      !test nband /= SIZE(bandi)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',15_SNK)
      CALL pList%add('MatrixType->nband',16_SNK)
      CALL pList%add('bandi',(/1_SIK,1_SIK,1_SIK/))
      CALL pList%add('bandj',(/1_SIK,2_SIK,3_SIK/))
      CALL pList%add('bandl',(/4_SIK,3_SIK,2_SIK/))
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'banded%init(...)')
      CALL thisMatrix%clear()
      !test out of bounds array element (i,j) not in (1:n,1:m)
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',15_SNK)
      CALL pList%add('MatrixType->nband',3_SNK)
      CALL pList%add('bandi',(/1_SIK,1_SIK,21_SIK/))
      CALL pList%add('bandj',(/1_SIK,2_SIK,3_SIK/))
      CALL pList%add('bandl',(/4_SIK,3_SIK,2_SIK/))
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'banded%init(...)')
      CALL thisMatrix%clear()
      !test multiple bands containing same array element
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',15_SNK)
      CALL pList%add('MatrixType->nband',3_SNK)
      CALL pList%add('bandi',(/1_SIK,2_SIK,1_SIK/))
      CALL pList%add('bandj',(/1_SIK,2_SIK,3_SIK/))
      CALL pList%add('bandl',(/4_SIK,3_SIK,2_SIK/))
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'banded%init(...)')
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL banded%init(...)'

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
