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

      TYPE(ParamType) :: pList,optListMat,distVecPList
      INTEGER(SIK) :: rank,nproc,mpierr,i,j
      CLASS(DistributedMatrixType),ALLOCATABLE :: thisMatrix
      TYPE(NativeDistributedVectorType),ALLOCATABLE :: distrVec1, distrVec2
      REAL(SRK),ALLOCATABLE :: dummyvec(:),dummyvec2(:)
      REAL(SRK) :: val
      LOGICAL(SBK) :: bool


      CALL MPI_Comm_rank(PE_COMM_WORLD,rank,mpierr)
      CALL MPI_Comm_size(PE_COMM_WORLD,nproc,mpierr)

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
        thisMatrix%nnz=4
        thisMatrix%isCreated=.TRUE.
        thisMatrix%isAssembled=.FALSE.
        thisMatrix%comm=33
        ALLOCATE(thisMatrix%chunks(2))
        ALLOCATE(thisMatrix%iOffsets(3))
        ALLOCATE(thisMatrix%jOffsets(3))
      ENDSELECT
      CALL thisMatrix%clear()
      SELECT TYPE(thisMatrix)
        TYPE IS(DistributedBandedMatrixType)
          bool = (.NOT.(thisMatrix%isInit).AND.(thisMatrix%n == 0)) &
              .AND.(thisMatrix%m == 0) &
              .AND.(thisMatrix%nLocal == 0) &
              .AND.(thisMatrix%isCreated == .FALSE.) &
              .AND.(thisMatrix%isAssembled == .FALSE.) &
              .AND.(thisMatrix%comm == MPI_COMM_NULL) &
              .AND.(thisMatrix%m == 0) &
              .AND.(.NOT.ALLOCATED(thisMatrix%chunks)) &
              .AND.(.NOT.ALLOCATED(thisMatrix%jOffsets)) &
              .AND.(.NOT.ALLOCATED(thisMatrix%iOffsets))
          ASSERT(bool, 'DistributedBandedMatrixType%clear()')
      END SELECT
      !check init
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',15_SNK)
      CALL pList%add('MatrixType->nnz',9_SNK)
      CALL pList%add('MatrixType->MPI_COMM_ID',PE_COMM_WORLD)

      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      SELECT TYPE(thisMatrix)
        TYPE IS(DistributedBandedMatrixType)
          bool = (( thisMatrix%isInit).AND.(thisMatrix%n == 10)) &
              .AND.((thisMatrix%m == 15).AND.(thisMatrix%nnz == 9))
          ASSERT(bool, 'banded%init(...)')
          IF(rank == 0) THEN
            bool = (ALLOCATED(thisMatrix%iTmp) .AND. &
                    ALLOCATED(thisMatrix%jTmp) .AND. &
                    ALLOCATED(thisMatrix%elemTmp) .AND. &
                    ALL(thisMatrix%jOffsets .EQ. (/0_SIK,8_SIK,15_SIK/)) .AND. &
                    ALL(thisMatrix%iOffsets .EQ. (/0_SIK,5_SIK,10_SIK/)) .AND. &
                    thisMatrix%nLocal == 0 .AND. &
                    .NOT.ALLOCATED(thisMatrix%chunks))
            ASSERT(bool, 'banded%init')
          ELSE
            bool = (ALLOCATED(thisMatrix%iTmp) .AND. &
                    ALLOCATED(thisMatrix%jTmp) .AND. &
                    ALLOCATED(thisMatrix%elemTmp) .AND. &
                    thisMatrix%nLocal == 0 .AND. &
                    ALL(thisMatrix%jOffsets .EQ. (/0_SIK,8_SIK,15_SIK/)) .AND. &
                    ALL(thisMatrix%iOffsets .EQ. (/0_SIK,5_SIK,10_SIK/)) .AND. &
                    .NOT.ALLOCATED(thisMatrix%chunks))
            ASSERT(bool, 'banded%init')
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      !test with double init (isInit==true on 2nd try)
      WRITE(*,*) "Beginning double init"
      CALL thisMatrix%init(pList)
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
      !test with n<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',-1_SNK)
      CALL pList%add('MatrixType->m',10_SNK)
      CALL pList%add('MatrixType->nnz',9_SNK)
      CALL pList%add('MatrixType->MPI_COMM_ID',PE_COMM_WORLD)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'banded%init(...)')
      CALL thisMatrix%clear()
      !test with m<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',-1_SNK)
      CALL pList%add('MatrixType->nnz',3_SNK)
      CALL pList%add('MatrixType->MPI_COMM_ID',PE_COMM_WORLD)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'banded%init(...)')
      CALL thisMatrix%clear()
      !test with nnz<1
      CALL pList%clear()
      CALL pList%add('MatrixType->n',10_SNK)
      CALL pList%add('MatrixType->m',15_SNK)
      CALL pList%add('MatrixType->nnz',-1_SNK)
      CALL pList%add('MatrixType->MPI_COMM_ID',PE_COMM_WORLD)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList) !expect exception
      bool = .NOT.thisMatrix%isInit
      ASSERT(bool, 'banded%init(...)')
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL banded%init(...)'

      !check set
      !test normal use case (split diagonal)
      !want to build:
      ![3 0 7 0 0]
      ![0 4 0 8 0]
      ![1 0 5 0 9]
      ![0 2 0 6 0]
      CALL pList%clear()
      CALL pList%add('MatrixType->n',4_SNK)
      CALL pList%add('MatrixType->m',5_SNK)
      CALL pList%add('MatrixType->nnz',9_SNK)
      CALL pList%add('MatrixType->MPI_COMM_ID',PE_COMM_WORLD)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      CALL thisMatrix%set(3,1,1._SRK)
      CALL thisMatrix%set(4,2,2._SRK)
      CALL thisMatrix%set(1,1,3._SRK)
      CALL thisMatrix%set(2,2,4._SRK)
      CALL thisMatrix%set(3,3,5._SRK)
      CALL thisMatrix%set(4,4,6._SRK)
      CALL thisMatrix%set(1,3,7._SRK)
      CALL thisMatrix%set(2,4,8._SRK)
      CALL thisMatrix%set(3,5,9._SRK)
      CALL thisMatrix%assemble()
      SELECTTYPE(thisMatrix)
        TYPE IS(DistributedBandedMatrixType)
          IF(rank == 0) THEN
            bool = SIZE(thisMatrix%chunks) == 2
            bool = SIZE(thisMatrix%chunks(1)%bandIdx) == 2
            bool = bool .AND. SIZE(thisMatrix%chunks(1)%bands(1)%elem) == 2
            bool = bool .AND. thisMatrix%chunks(1)%bands(1)%elem(1) == 3
            bool = bool .AND. thisMatrix%chunks(1)%bands(1)%elem(2) == 4
            bool = bool .AND. SIZE(thisMatrix%chunks(1)%bands(2)%elem) == 1
            bool = bool .AND. thisMatrix%chunks(1)%bands(2)%elem(1) == 7
            bool = SIZE(thisMatrix%chunks(2)%bandIdx) == 2
            bool = bool .AND. SIZE(thisMatrix%chunks(2)%bands(1)%elem) == 2
            bool = bool .AND. thisMatrix%chunks(2)%bands(1)%elem(1) == 1
            bool = bool .AND. thisMatrix%chunks(2)%bands(1)%elem(2) == 2
            bool = bool .AND. SIZE(thisMatrix%chunks(2)%bands(2)%elem) == 1
            bool = bool .AND. thisMatrix%chunks(2)%bands(2)%elem(1) == 5
            ASSERT(bool, 'banded%set(...)')
          ELSE
            bool = SIZE(thisMatrix%chunks) == 2
            bool = SIZE(thisMatrix%chunks(1)%bandIdx) == 1
            bool = bool .AND. SIZE(thisMatrix%chunks(1)%bands(1)%elem) == 1
            bool = bool .AND. thisMatrix%chunks(1)%bands(1)%elem(1) == 8
            bool = SIZE(thisMatrix%chunks(2)%bandIdx) == 2
            bool = bool .AND. SIZE(thisMatrix%chunks(2)%bands(1)%elem) == 1
            bool = bool .AND. thisMatrix%chunks(2)%bands(1)%elem(1) == 6
            bool = bool .AND. SIZE(thisMatrix%chunks(2)%bands(2)%elem) == 1
            bool = bool .AND. thisMatrix%chunks(2)%bands(2)%elem(1) == 9
            ASSERT(bool, 'banded%set(...)')
          ENDIF
      ENDSELECT
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL banded%set(...)'
      !check get functionality
      ![3 0 7 0 0]
      ![0 4 0 8 0]
      ![1 0 5 0 9]
      ![0 2 0 6 0]
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',4_SNK)
      CALL pList%add('MatrixType->m',5_SNK)
      CALL pList%add('MatrixType->nnz',9_SNK)
      CALL pList%add('MatrixType->MPI_COMM_ID',PE_COMM_WORLD)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      CALL thisMatrix%set(3,1,1._SRK)
      CALL thisMatrix%set(4,2,2._SRK)
      CALL thisMatrix%set(1,1,3._SRK)
      CALL thisMatrix%set(2,2,4._SRK)
      CALL thisMatrix%set(3,3,5._SRK)
      CALL thisMatrix%set(4,4,6._SRK)
      CALL thisMatrix%set(1,3,7._SRK)
      CALL thisMatrix%set(2,4,8._SRK)
      CALL thisMatrix%set(3,5,9._SRK)
      CALL thisMatrix%assemble()
      IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
      ALLOCATE(dummyvec(13))
      dummyvec=0
      CALL thisMatrix%get(1,1,dummyvec(1))
      CALL thisMatrix%get(1,2,dummyvec(2))
      CALL thisMatrix%get(1,3,dummyvec(3))
      CALL thisMatrix%get(2,2,dummyvec(4))
      CALL thisMatrix%get(2,4,dummyvec(5))
      CALL thisMatrix%get(2,5,dummyvec(6))
      CALL thisMatrix%get(3,1,dummyvec(7))
      CALL thisMatrix%get(3,2,dummyvec(8))
      CALL thisMatrix%get(3,3,dummyvec(9))
      CALL thisMatrix%get(3,5,dummyvec(10))
      CALL thisMatrix%get(4,2,dummyvec(11))
      CALL thisMatrix%get(4,4,dummyvec(12))
      CALL thisMatrix%get(4,5,dummyvec(13))

      bool = .TRUE.
      bool = bool .AND. dummyvec(1) == 3._SRK
      bool = bool .AND. dummyvec(2) == 0._SRK
      bool = bool .AND. dummyvec(3) == 7._SRK
      bool = bool .AND. dummyvec(4) == 4._SRK
      bool = bool .AND. dummyvec(5) == 8._SRK
      bool = bool .AND. dummyvec(6) == 0._SRK
      bool = bool .AND. dummyvec(7) == 1._SRK
      bool = bool .AND. dummyvec(8) == 0._SRK
      bool = bool .AND. dummyvec(9) == 5._SRK
      bool = bool .AND. dummyvec(10) == 9._SRK
      bool = bool .AND. dummyvec(11) == 2._SRK
      bool = bool .AND. dummyvec(12) == 6._SRK
      bool = bool .AND. dummyvec(13) == 0._SRK
      ASSERT(bool, 'banded%get(...)')

      IF(ALLOCATED(dummyvec)) DEALLOCATE(dummyvec)
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL banded%get(...)'
      !check transpose functionality
      ![1 2 0 0 0]
      ![0 3 4 0 0]
      ![0 0 5 6 0]
      ![0 0 0 7 0]
      !with main diagonal split [1,3],[5,7]
      !to
      ![1 0 0 0]
      ![2 3 0 0]
      ![0 4 5 0]
      ![0 0 6 7]
      ![0 0 0 0]
      ! CALL thisMatrix%clear()
      ! CALL pList%clear()
      ! CALL pList%add('MatrixType->n',4_SNK)
      ! CALL pList%add('MatrixType->m',5_SNK)
      ! CALL pList%add('MatrixType->nnz',7_SNK)
      ! CALL pList%add('MatrixType->MPI_COMM_ID',PE_COMM_WORLD)
      ! CALL pList%validate(pList,optListMat)
      ! CALL thisMatrix%init(pList)
      ! CALL thisMatrix%set(1,1,1._SRK)
      ! CALL thisMatrix%set(1,2,2._SRK)
      ! CALL thisMatrix%set(2,2,3._SRK)
      ! CALL thisMatrix%set(2,3,4._SRK)
      ! CALL thisMatrix%set(3,3,5._SRK)
      ! CALL thisMatrix%set(3,4,6._SRK)
      ! CALL thisMatrix%set(4,4,7._SRK)
      ! CALL thisMatrix%assemble()
      ! CALL thisMatrix%transpose()
      !
      ! ALLOCATE(dummyvec(7))
      ! CALL thisMatrix%get(1,1,dummyvec(1))
      ! CALL thisMatrix%get(2,1,dummyvec(2))
      ! CALL thisMatrix%get(2,2,dummyvec(3))
      ! CALL thisMatrix%get(3,2,dummyvec(4))
      ! CALL thisMatrix%get(3,3,dummyvec(5))
      ! CALL thisMatrix%get(4,3,dummyvec(6))
      ! CALL thisMatrix%get(4,4,dummyvec(7))
      !
      ! bool = .TRUE.
      ! bool = bool .AND. dummyvec(1) == 1
      ! bool = bool .AND. dummyvec(2) == 2
      ! bool = bool .AND. dummyvec(3) == 3
      ! bool = bool .AND. dummyvec(4) == 4
      ! bool = bool .AND. dummyvec(5) == 5
      ! bool = bool .AND. dummyvec(6) == 6
      ! bool = bool .AND. dummyvec(7) == 7
      ! ASSERT(bool,"banded%transpose()")
      !
      ! CALL thisMatrix%clear()
      ! WRITE(*,*) '  Passed: CALL banded%transpose(...)'

      !check zero_entries functionality
      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',4_SNK)
      CALL pList%add('MatrixType->m',5_SNK)
      CALL pList%add('MatrixType->nnz',7_SNK)
      CALL pList%add('MatrixType->MPI_COMM_ID',PE_COMM_WORLD)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      CALL thisMatrix%set(2,3,4._SRK)
      CALL thisMatrix%set(3,3,5._SRK)
      CALL thisMatrix%set(3,4,6._SRK)
      CALL thisMatrix%set(4,4,7._SRK)
      CALL thisMatrix%assemble()
      CALL thisMatrix%zeroentries()
      SELECTTYPE(thisMatrix)
        TYPE IS(DistributedBandedMatrixType)
          DO i=1,SIZE(thisMatrix%chunks(1)%bands)
            ! The rank1 chunk1 will not be initialized, so skip
            IF(rank==0) THEN
              DO j=1,SIZE(thisMatrix%chunks(1)%bands(i)%elem)
                bool=(thisMatrix%chunks(1)%bands(i)%elem(j) .APPROXEQ. 0.0_SRK)
                ASSERT(bool,"banded%zero()")
              ENDDO
            END IF
          ENDDO
          DO i=1,SIZE(thisMatrix%chunks(2)%bands)
            DO j=1,SIZE(thisMatrix%chunks(2)%bands(i)%elem)
              bool=(thisMatrix%chunks(2)%bands(i)%elem(j) .APPROXEQ. 0.0_SRK)
              ASSERT(bool,"banded%zero()")
            ENDDO
          ENDDO
      ENDSELECT
      CALL thisMatrix%clear()
      WRITE(*,*) '  Passed: CALL banded%zero(...)'

      !check matvec functionality
      ![1 2 0 0]
      ![0 3 0 0]
      ![0 0 5 6]
      ![0 9 0 7]

      CALL thisMatrix%clear()
      CALL pList%clear()
      CALL pList%add('MatrixType->n',4_SNK)
      CALL pList%add('MatrixType->m',4_SNK)
      CALL pList%add('MatrixType->nnz',7_SNK)
      CALL pList%add('MatrixType->MPI_COMM_ID',PE_COMM_WORLD)
      CALL pList%validate(pList,optListMat)
      CALL thisMatrix%init(pList)
      CALL thisMatrix%set(1,1,1._SRK)
      CALL thisMatrix%set(1,2,2._SRK)
      CALL thisMatrix%set(2,2,3._SRK)
      CALL thisMatrix%set(3,3,5._SRK)
      CALL thisMatrix%set(3,4,6._SRK)
      CALL thisMatrix%set(4,4,7._SRK)
      CALL thisMatrix%set(4,2,9._SRK)
      CALL thisMatrix%assemble()
      IF(ALLOCATED(distrVec1)) DEALLOCATE(distrVec1)
      IF(ALLOCATED(distrVec2)) DEALLOCATE(distrVec2)
      ALLOCATE(NativeDistributedVectorType :: distrVec1)
      ALLOCATE(NativeDistributedVectorType :: distrVec2)

      CALL distVecPList%clear()
      CALL distVecPList%add('VectorType->n',4)
      CALL distVecPList%add('VectorType->MPI_Comm_ID',PE_COMM_WORLD)
      CALL distrVec1%init(distVecPList)
      CALL distrVec2%init(distVecPList)

      ! Check zero vector
      distrVec1%b = 0.0_SRK
      distrVec2%b = 0.0_SRK

      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=distrVec1,Y=distrVec2)
      DO i=1,2
        bool = ALL(ABS(distrVec2%b) < 1E-6)
        ASSERT(bool, 'banded%matvec(...)')
      ENDDO
      ! Check for non-trivial vector
      IF (rank == 0) THEN
        distrVec1%b = (/1._SRK,2._SRK/)
      ELSE
        distrVec1%b = (/3._SRK,4._SRK/)
      END IF
      CALL SLEEP(2)
      WRITE(*,*) "------------------------------------------------"
      CALL BLAS_matvec(THISMATRIX=thisMatrix,X=distrVec1,Y=distrVec2,alpha=1.0_SRK,beta=0.0_SRK)
      WRITE(*,*) distrVec2%b
      IF (rank == 0) THEN
        bool = distrVec2%b(1) == 5._SRK
        ASSERT(bool, 'banded%matvec(...)')
        bool = distrVec2%b(2) == 6._SRK
        ASSERT(bool, 'banded%matvec(...)')
      ELSE
        bool = distrVec2%b(1) == 39._SRK
        ASSERT(bool, 'banded%matvec(...)')
        bool = distrVec2%b(2) == 46._SRK
        ASSERT(bool, 'banded%matvec(...)')
      END IF

      WRITE(*,*) '  Passed: CALL banded%matvec(...)'
      DEALLOCATE(thisMatrix)

      ! Create matrix that looks like this
      ! 1 0 2 0
      ! 0 3 0 4
      ! 0 0 5 0
      ! 6 7 0 0
      ! Rank 0 owns rows 1 and 2
      ! Rank 1 owns rows 3 and 4

      CALL pList%clear()
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
