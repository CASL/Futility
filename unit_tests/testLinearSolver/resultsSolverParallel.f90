!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM resultsSolverParallel
#include "Futility_DBC.h"
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE ISO_C_BINDING
  USE BLAS
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE Futility_DBC
  USE trilinos_interfaces
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes
  USE Times
  USE FileType_HDF5
  USE Strings
  USE LinearSolverTypes

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

!#define PETSC_ALL

#ifdef PETSC_ALL
#define PETSC_NOPC
#define PETSC_PREC
#endif

  TYPE(LinearSolverType_Iterative) :: petscLS, petscLS_prec, nativeLS, nativeLS_prec
  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(ParamType) :: petscPlist,nativePlist,petscVec,nativeVec,rsorPlist
  REAL(SRK) :: tBandPrec,tBandNopc,tPetscPrec,tPetscNopc

  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL eParams%addSurrogate(e)
  CALL eMatrixType%addSurrogate(e)

#ifdef FUTILITY_HAVE_PETSC
  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
#else
#ifdef HAVE_MPI
  CALL MPI_Init(ierr)
#endif
#endif

  CALL setup()

  CREATE_TEST('Solver Time (Un-preconditioned)')
#ifdef PETSC_NOPC
  REGISTER_SUBTEST('Petsc',timePetscSolve)
#endif
  REGISTER_SUBTEST('Banded',timeBandedSolve)
  FINALIZE_TEST()

  CREATE_TEST('Solver Time (Preconditioned)')
#ifdef PETSC_PREC
  REGISTER_SUBTEST('Petsc',timePetscSolve_prec)
#endif
  REGISTER_SUBTEST('Banded',timeBandedSolve_prec)
  FINALIZE_TEST()

  CREATE_TEST('Solutions Match')
  REGISTER_SUBTEST('match',checkResult)

  CALL petscPList%clear()
  CALL nativePlist%clear()
  CALL petscVec%clear()
  CALL nativeVec%clear()

#ifdef FUTILITY_HAVE_PETSC
  CALL PetscFinalize(ierr)
#endif

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------

    SUBROUTINE setup()
      INTEGER(SIK) :: n,nnz,i,j,rank,mpierr,ios,lowIdx,highIdx,nnz_banded,iperr
      REAL(SRK) :: tmpreal
      CHARACTER(200)::tempcharacter,dirname
      TYPE(TimerType) :: bandAssemble,petscAssemble
      TYPE(StringType),ALLOCATABLE :: contents(:)
      TYPE(HDF5FileType) :: inputData
      TYPE(PETScVectorType) :: dummy
      INTEGER(SIK),ALLOCATABLE :: matrix_row(:),matrix_col(:),src_idx(:),sol_idx(:)
      REAL(SRK),ALLOCATABLE :: matrix_val(:),src_val(:),sol_val(:)


      CALL MPI_Comm_rank(PE_COMM_WORLD,rank,mpierr)

      n = 148716
      nnz = 6015708
      nnz_banded = 583848

      CALL petscPlist%clear()
      CALL petscPlist%add('LinearSolverType->TPLType',PETSC)
      CALL petscPlist%add('LinearSolverType->solverMethod',GMRES)
      CALL petscPlist%add('LinearSolverType->MPI_Comm_ID',PE_COMM_WORLD)
      CALL petscPlist%add('LinearSolverType->numberOMP',1_SNK)
      CALL petscPlist%add('LinearSolverType->timerName','testTimer')
      CALL petscPlist%add('LinearSolverType->matType',SPARSE)
      CALL petscPlist%add('LinearSolverType->A->MatrixType->n',n)
      CALL petscPlist%add('LinearSolverType->A->MatrixType->nlocal',-1_SNK)
      CALL petscPlist%add('LinearSolverType->A->MatrixType->nnz',nnz)
      CALL petscPlist%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
      CALL petscPlist%add('LinearSolverType->x->vectorType->n',n)
      CALL petscPlist%add('LinearSolverType->x->vectorType->nlocal',-1_SNK)
      CALL petscPlist%add('LinearSolverType->b->VectorType->n',n)
      CALL petscPlist%add('LinearSolverType->b->VectorType->nlocal',-1_SNK)

      CALL nativePlist%clear()
      CALL nativePlist%add('LinearSolverType->TPLType',NATIVE)
      CALL nativePlist%add('LinearSolverType->solverMethod',GMRES)
      CALL nativePlist%add('LinearSolverType->MPI_Comm_ID',PE_COMM_WORLD)
      CALL nativePlist%add('LinearSolverType->numberOMP',1_SNK)
      CALL nativePlist%add('LinearSolverType->timerName','testTimer')
      CALL nativePlist%add('LinearSolverType->matType',DISTR_BLOCKBANDED)
      CALL nativePlist%add('LinearSolverType->A->MatrixType->n',n)
      CALL nativePlist%add('LinearSolverType->A->MatrixType->blockSize',51_SIK)
      CALL nativePlist%add('LinearSolverType->A->MatrixType->nnz',nnz_banded)
      CALL nativePlist%add('LinearSolverType->x->VectorType->n',n)
      CALL nativePlist%add('LinearSolverType->x->VectorType->chunkSize',51_SIK)
      CALL nativePlist%add('LinearSolverType->b->VectorType->n',n)
      CALL nativePlist%add('LinearSolverType->b->VectorType->chunkSize',51_SIK)

      CALL petscVec%clear()
      CALL petscVec%add('VectorType->n',n)
      CALL petscVec%add('VectorType->nlocal',-1)
      CALL petscVec%add('VectorType->MPI_Comm_ID',PE_COMM_WORLD)

      CALL nativeVec%clear()
      CALL nativeVec%add('VectorType->n',n)
      CALL nativeVec%add('VectorType->chunkSize',51_SIK)
      CALL nativeVec%add('VectorType->MPI_Comm_ID',PE_COMM_WORLD)

      CALL rsorPlist%clear()
      CALL rsorPlist%add('PCType->numBlocks',n/51)
      CALL rsorPlist%add('PCType->MPI_Comm_ID',PE_COMM_WORLD)
      CALL rsorPlist%add('PCType->omega',0.9_SRK)

      CALL petscPlist%validate(petscPlist)
      CALL petscLS%init(petscPlist)

      CALL nativePlist%validate(nativePlist)
      CALL nativeLS%init(nativePlist)

      CALL petscPlist%add('LinearSolverType->PCType','BJACOBI_ILU')
      CALL petscPlist%validate(petscPlist)
      CALL petscLS_prec%init(petscPlist)

      CALL nativePlist%add('LinearSolverType->PCType','DISTR_RSOR')
      CALL nativePlist%validate(nativePlist)
      CALL nativeLS_prec%init(nativePlist)


      WRITE(dirname,'(2A)'),'/home/mkbz/Research/largeCMFD/it_end.hdf5'
      CALL inputData%init(dirname,'READ')
      CALL inputData%fopen()

      CALL inputData%fread('cmfd_matrix/rows',matrix_row)
      CALL inputData%fread('cmfd_matrix/cols',matrix_col)
      CALL inputData%fread('cmfd_matrix/vals',matrix_val)

      CALL inputData%fread('sol_vec/idx',sol_idx)
      CALL inputData%fread('sol_vec/val',sol_val)

      CALL inputData%fread('src_vec/idx',src_idx)
      CALL inputData%fread('src_vec/val',src_val)

      DO i=1,SIZE(matrix_row)
        CALL nativeLS%A%set(matrix_row(i),matrix_col(i),matrix_val(i))
        CALL nativeLS_prec%A%set(matrix_row(i),matrix_col(i),matrix_val(i))
      END DO

#ifdef PETSC_NOPC
      SELECT TYPE(A => petscLS%A); TYPE IS(PetscMatrixType)
      highIdx = 0
      DO WHILE (highIdx < SIZE(matrix_row))
        lowIdx = highIdx + 1
        highIdx = lowIdx
        DO WHILE (matrix_row(highIdx) == matrix_row(lowIdx))
          highIdx = highIdx + 1
          IF (highIdx == SIZE(matrix_row)) EXIT
        END DO
        IF (highIdx == SIZE(matrix_row)) THEN
          CALL A%setRow(matrix_row(lowIdx),matrix_col(lowIdx:highIdx),matrix_val(lowIdx:highIdx))
          EXIT
        ENDIF
        highIdx = highIdx - 1
        CALL A%setRow(matrix_row(lowIdx),matrix_col(lowIdx:highIdx),matrix_val(lowIdx:highIdx))
      END DO
      END SELECT
#endif
#ifdef PETSC_PREC
      SELECT TYPE(A => petscLS_prec%A); TYPE IS(PetscMatrixType)
      highIdx = 0
      DO WHILE (highIdx < SIZE(matrix_row))
        lowIdx = highIdx + 1
        highIdx = lowIdx
        DO WHILE (matrix_row(highIdx) == matrix_row(lowIdx))
          highIdx = highIdx + 1
          IF (highIdx == SIZE(matrix_row)) EXIT
        END DO
        IF (highIdx == SIZE(matrix_row)) THEN
          CALL A%setRow(matrix_row(lowIdx),matrix_col(lowIdx:highIdx),matrix_val(lowIdx:highIdx))
          EXIT
        ENDIF
        highIdx = highIdx - 1
        CALL A%setRow(matrix_row(lowIdx),matrix_col(lowIdx:highIdx),matrix_val(lowIdx:highIdx))
      END DO
      ENDSELECT
#endif

      CALL dummy%init(petscVec)
      CALL VecGetOwnershipRange(dummy%b,lowIdx,highIdx,iperr)
      lowIdx = lowIdx+1

      DO i=1,SIZE(src_idx)
        CALL nativeLS%b%set(src_idx(i),src_val(i))
        CALL nativeLS_prec%b%set(src_idx(i),src_val(i))
        IF (src_idx(i) > lowIdx .AND. src_idx(i) <= highIdx) THEN
          CALL petscLS%b%set(src_idx(i),src_val(i))
          CALL petscLS_prec%b%set(src_idx(i),src_val(i))
        ENDIF
      END DO

      SELECT TYPE(b => petscLS%b); TYPE IS(PetscVectorType)
        CALL b%assemble()
      END SELECT
      SELECT TYPE(b => petscLS_prec%b); TYPE IS(PetscVectorType)
        CALL b%assemble()
      END SELECT


      SELECT TYPE(xNative => nativeLS%X); TYPE IS(NativeDistributedVectorType)
      SELECT TYPE(xNative_prec => nativeLS_prec%X); TYPE IS(NativeDistributedVectorType)
      SELECT TYPE(xPetsc => petscLS%X); TYPE IS(PetscVectorType)
      SELECT TYPE(xPetsc_prec => petscLS_prec%X); TYPE IS(PetscVectorType)
        DO i=1,SIZE(sol_idx)
          IF (sol_idx(i) > lowIdx .AND. sol_idx(i) <= highIdx) THEN
            CALL xNative%set(sol_idx(i),sol_val(i))
            CALL xNative_prec%set(sol_idx(i),sol_val(i))
            CALL xPetsc%set(sol_idx(i),sol_val(i))
            CALL xPetsc_prec%set(sol_idx(i),sol_val(i))
          END IF
        END DO
        CALL xPetsc%assemble()
        CALL xPetsc_prec%assemble()
      END SELECT
      END SELECT
      END SELECT
      END SELECT

      CALL inputData%clear()

      CALL nativeLS%setConv(2,1.0e-6_SRK,1.0e-6_SRK,1000000_SIK,30_SIK)
      CALL petscLS%setConv(2,1.0e-6_SRK,1.0e-6_SRK,1000000_SIK,30_SIK)
      CALL nativeLS_prec%setConv(2,1.0e-6_SRK,1.0e-6_SRK,1000000_SIK,30_SIK)
      CALL petscLS_prec%setConv(2,1.0e-6_SRK,1.0e-6_SRK,1000000_SIK,30_SIK)

      nativeLS%hasX0 = .TRUE.
      nativeLS_prec%hasX0 = .TRUE.
      petscLS%hasX0 = .TRUE.
      petscLS_prec%hasX0 = .TRUE.

      CALL nativeLS_prec%setupPC(rsorPlist)
      CALL SLEEP(2)

    END SUBROUTINE setup

    SUBROUTINE timeBandedSolve()

      REAL(SRK) :: timetaken,tmpreal
      INTEGER(SIK) :: i,j,mpierr
      TYPE(TimerType) :: bandSolve

      CALL bandSolve%setTimerName('Banded Solver Time')

      SELECT TYPE(A => nativeLS%A); TYPE IS(DistributedBlockBandedMatrixType)
        IF (.NOT. A%isAssembled) CALL A%assemble()
      END SELECT

      CALL MPI_Barrier(PE_COMM_WORLD,mpierr)
      CALL bandSolve%tic()
      CALL nativeLS%solve()
      CALL bandSolve%toc()

      ! report total time
      tBandNopc = bandSolve%getTimeReal()

      CALL SLEEP(2)

    END SUBROUTINE timeBandedSolve

    SUBROUTINE timePetscSolve()
      INTEGER(SIK) :: i,n,rank,nproc,ios,j,mpierr
      REAL(SRK) :: timeTaken,tmpreal,dummyvec(4)
      TYPE(TimerType) :: petscSolve

      n = 148716
      CALL MPI_Comm_rank(PE_COMM_WORLD,rank,mpierr)
      CALL MPI_Comm_size(PE_COMM_WORLD,nproc,mpierr)
      CALL petscSolve%setTimerName('Petsc Solver Time')

      SELECT TYPE(A => petscLS%A); TYPE IS(PetscMatrixType)
        IF (.NOT. A%isAssembled) CALL A%assemble()
      END SELECT

      ! Get clock
      CALL MPI_Barrier(PE_COMM_WORLD,mpierr)
      CALL petscSolve%tic()
      CALL petscLS%solve()
      CALL petscSolve%toc()
      ! report total time
      tPetscNopc = petscSolve%getTimeReal()
      CALL SLEEP(2)

    END SUBROUTINE timePetscSolve

    SUBROUTINE timeBandedSolve_prec()

      REAL(SRK) :: timetaken,tmpreal
      INTEGER(SIK) :: i,j,mpierr
      TYPE(TimerType) :: bandSolve

      CALL bandSolve%setTimerName('Preconditioned Banded Solver Time')

      SELECT TYPE(A => nativeLS_prec%A); TYPE IS(DistributedBlockBandedMatrixType)
        IF (.NOT. A%isAssembled) CALL A%assemble()
      END SELECT

      CALL MPI_Barrier(PE_COMM_WORLD,mpierr)
      CALL bandSolve%tic()
      CALL nativeLS_prec%solve()
      CALL bandSolve%toc()

      ! report total time
      tBandPrec = bandSolve%getTimeReal()
      CALL SLEEP(2)

    END SUBROUTINE timeBandedSolve_prec


    SUBROUTINE timePetscSolve_prec()
      INTEGER(SIK) :: i,n,rank,nproc,ios,j,mpierr
      REAL(SRK) :: timeTaken,tmpreal,dummyvec(4)
      TYPE(TimerType) :: petscSolve

      n = 148716
      CALL MPI_Comm_rank(PE_COMM_WORLD,rank,mpierr)
      CALL MPI_Comm_size(PE_COMM_WORLD,nproc,mpierr)
      CALL petscSolve%setTimerName('Preconditioned Petsc Solver Time')

      SELECT TYPE(A => petscLS_prec%A); TYPE IS(PetscMatrixType)
        IF (.NOT. A%isAssembled) CALL A%assemble()
      END SELECT

      ! Get clock
      CALL MPI_Barrier(PE_COMM_WORLD,mpierr)
      CALL petscSolve%tic()
      CALL petscLS_prec%solve()
      CALL petscSolve%toc()
      ! report total time
      tPetscPrec = petscSolve%getTimeReal()
      CALL SLEEP(2)

    END SUBROUTINE timePetscSolve_prec

    SUBROUTINE checkResult()
      REAL(SRK),ALLOCATABLE :: dummyvec(:),dummyvec2(:)
      REAL(SRK) :: resid_norm,rn1,rn2
      TYPE(NativeDistributedVectorType) :: resid1, resid2
      INTEGER(SIK) :: i,rank,nproc,mpierr,lowIdx,highIdx,iperr,iters
      INTEGER(SIK),ALLOCATABLE :: nlocal(:),offset(:)
      LOGICAL(SBK) :: file_present

      CALL MPI_Comm_rank(PE_COMM_WORLD,rank,mpierr)
      CALL MPI_Comm_size(PE_COMM_WORLD,nproc,mpierr)
      IF (rank==0) THEN
        INQUIRE(file="bandNopc_solver_results.txt", exist=file_present)
        IF (file_present) THEN
          OPEN(12, FILE="bandNopc_solver_results.txt", STATUS="old", POSITION="append", ACTION="write")
        ELSE
          OPEN(12, FILE="bandNopc_solver_results.txt", STATUS="new", ACTION="write")
        END IF
        INQUIRE(file="bandPrec_solver_results.txt", exist=file_present)
        IF (file_present) THEN
          OPEN(13, FILE="bandPrec_solver_results.txt", STATUS="old", POSITION="append", ACTION="write")
        ELSE
          OPEN(13, FILE="bandPrec_solver_results.txt", STATUS="new", ACTION="write")
        END IF
      END IF
      ALLOCATE(dummyvec(nativeLS%X%n))

      CALL resid1%init(nativeVec)
      CALL resid2%init(nativeVec)
      CALL nativeLS%getResidual(resid1)
      CALL nativeLS_prec%getResidual(resid2)

      rn1 = BLAS_dot(resid1%b,resid1%b)
      rn2 = BLAS_dot(resid2%b,resid2%b)
      CALL nativeLS%MPIParallelEnv%allReduce_scalar(rn1)
      CALL nativeLS_prec%MPIParallelEnv%allReduce_scalar(rn2)
      rn1 = SQRT(rn1)
      rn2 = SQRT(rn2)

      IF (rank==0) THEN
        WRITE(12,*) nproc,tBandNopc,rn1,nativeLS%iters
        WRITE(13,*) nproc,tBandPrec,rn2,nativeLS_prec%iters
        CLOSE(12)
        CLOSE(13)
      ENDIF

      ASSERT(rn1 < 5.0e-6, 'Native GMRES-NOPC residual check')
      ASSERT(rn2 < 5.0e-6, 'Native GMRES-PREC residual check')


      ! SELECT TYPE(x => nativeLS%X); TYPE IS(NativeDistributedVectorType)
      !   SELECT TYPE(xPrec => nativeLS_prec%X); TYPE IS(NativeDistributedVectorType)
      !     ASSERT(ALL(ABS(x%b - xPrec%b) < 1.0e-3),'banded nopc/prec compare')
      !   ENDSELECT
      ! ENDSELECT

      ALLOCATE(nlocal(nproc))
      ALLOCATE(offset(nproc))
      SELECT TYPE(x => nativeLS%X); TYPE IS(NativeDistributedVectorType)
        CALL MPI_Allgather(x%nlocal,1,MPI_INTEGER,nlocal,1,MPI_INTEGER,x%comm,mpierr)
        CALL MPI_Allgather(x%offset,1,MPI_INTEGER,offset,1,MPI_INTEGER,x%comm,mpierr)
        CALL MPI_Allgatherv(x%b,x%nlocal,MPI_DOUBLE_PRECISION,dummyvec,nlocal,offset,MPI_DOUBLE_PRECISION,&
        x%comm,mpierr)
      END SELECT
#ifdef PETSC_NOPC
      IF (rank==0) THEN
        INQUIRE(file="petscNopc_solver_results.txt", exist=file_present)
        IF (file_present) THEN
          OPEN(14, FILE="petscNopc_solver_results.txt", STATUS="old", POSITION="append", ACTION="write")
        ELSE
          OPEN(14, FILE="petscNopc_solver_results.txt", STATUS="new", ACTION="write")
        END IF
      END IF
      SELECT TYPE(x => petscLS%X); TYPE IS(PetscVectorType)
        CALL VecGetOwnershipRange(x%b,lowIdx,highIdx,iperr)
        lowIdx = lowIdx + 1

        ALLOCATE(dummyvec2(highIdx - lowIdx+1))

        DO i=lowIdx,highIdx
          CALL x%get(i,dummyvec2(i-lowIdx+1))
        ENDDO
      END SELECT

      !ASSERT(ALL(ABS(dummyvec2 - dummyvec(lowIdx:highIdx)) < 1.0e-3),'petsc nopc / band nopc compare')
      !IF (.NOT. ALL(ABS(dummyvec2 - dummyvec(lowIdx:highIdx)) < 1.0e-3)) THEN
        !WRITE(*,*) MAXVAL(ABS(dummyvec2 - dummyvec(lowIdx:highIdx))),MAXVAL(dummyvec2)
      !END IF

      CALL KSPGetResidualNorm(petscLS%ksp,resid_norm,iperr)
      CALL KSPGetIterationNumber(petscLS%ksp,iters,iperr)

      ASSERT(resid_norm < 5.0e-6, "Petsc NOPC residual check")

      IF (rank==0) THEN
        WRITE(14,*) nproc,tPetscNopc,resid_norm,iters
        CLOSE(14)
      ENDIF
      DEALLOCATE(dummyvec2)
#endif
#ifdef PETSC_PREC
      IF (rank==0) THEN
        INQUIRE(file="petscPrec_solver_results.txt", exist=file_present)
        IF (file_present) THEN
          OPEN(15, FILE="petscPrec_solver_results.txt", STATUS="old", POSITION="append", ACTION="write")
        ELSE
          OPEN(15, FILE="petscPrec_solver_results.txt", STATUS="new", ACTION="write")
        END IF
      END IF
      SELECT TYPE(x => petscLS_prec%X); TYPE IS(PetscVectorType)
        CALL VecGetOwnershipRange(x%b,lowIdx,highIdx,iperr)
        lowIdx = lowIdx + 1

        ALLOCATE(dummyvec2(highIdx - lowIdx+1))

        DO i=lowIdx,highIdx
          CALL x%get(i,dummyvec2(i-lowIdx+1))
        ENDDO
      END SELECT

      CALL KSPGetResidualNorm(petscLS_prec%ksp,resid_norm,iperr)
      CALL KSPGetIterationNumber(petscLS_prec%ksp,iters,iperr)

      !ASSERT(ALL(ABS(dummyvec2 - dummyvec(lowIdx:highIdx)) < 5.0e-3),'petsc prec / band nopc compare')
      !IF (.NOT. ALL(ABS(dummyvec2 - dummyvec(lowIdx:highIdx)) < 5.0e-3)) THEN
        !WRITE(*,*) MAXVAL(ABS(dummyvec2 - dummyvec(lowIdx:highIdx))),MAXVAL(dummyvec2)
      !END IF
      ASSERT(resid_norm < 5.0e-6, "Petsc PREC residual check")

      IF (rank==0) THEN
        WRITE(15,*) nproc,tPetscPrec,resid_norm,iters
        CLOSE(15)
      ENDIF

      DEALLOCATE(dummyvec2)
#endif

    END SUBROUTINE checkResult

ENDPROGRAM resultsSolverParallel
