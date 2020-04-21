!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testParTPLPETSC

#ifdef FUTILITY_HAVE_PETSC
#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>6))
USE PETSCSYS
USE PETSCVEC
USE PETSCMAT
USE PETSCKSP
USE PETSCPC
#endif
#endif

IMPLICIT NONE

#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR==3) && (PETSC_VERSION_MINOR==6))
#include <petsc/finclude/petsc.h>
#else
#include <finclude/petsc.h>
#endif
#undef IS

!define precision kinds
INTEGER,PARAMETER :: N_INT_ORDER=8
INTEGER,PARAMETER :: N_LONG_ORDER=18
INTEGER,PARAMETER :: N_SGL_DIGITS=6
INTEGER,PARAMETER :: N_DBL_DIGITS=15
INTEGER,PARAMETER :: SNK=SELECTED_INT_KIND(N_INT_ORDER)
INTEGER,PARAMETER :: SLK=SELECTED_INT_KIND(N_LONG_ORDER)
INTEGER,PARAMETER :: SSK=SELECTED_REAL_KIND(N_SGL_DIGITS)
INTEGER,PARAMETER :: SDK=SELECTED_REAL_KIND(N_DBL_DIGITS)
#ifdef DBL
INTEGER,PARAMETER :: SRK=SDK
#else
INTEGER,PARAMETER :: SRK=SSK
#endif
#ifdef DBLINT
INTEGER,PARAMETER :: SIK=SLK
#else
INTEGER,PARAMETER :: SIK=SNK
#endif

!define error code
INTEGER(SIK) :: ierr
INTEGER(SIK) :: rank

CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)
CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
IF(rank == 0) WRITE(*,*) '==================================================='
IF(rank == 0) WRITE(*,*) 'TESTING PETSC TPL...'
IF(rank == 0) WRITE(*,*) '==================================================='

#ifdef FUTILITY_HAVE_PETSC
CALL testPETSC_KSP()
CALL testPETSC_KSP_SuperLU()
#endif

CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
IF(rank == 0) WRITE(*,*) '==================================================='
IF(rank == 0) WRITE(*,*) 'TESTING PETSC TPL PASSED!'
IF(rank == 0) WRITE(*,*) '==================================================='

CALL PetscFinalize(ierr)

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testPETSC_KSP
  Vec :: x,b
  Mat :: A
  KSP :: ksp
  PetscReal :: rtol,abstol,dtol
  PetscInt  :: maxits,restart
  REAL(SRK) :: getval(1)

  !test KSPCreate
  CALL KSPCreate(MPI_COMM_WORLD,ksp,ierr)
  IF(ierr /= 0) THEN
    WRITE(*,*) 'CALL KSPCreate(MPI_COMM_WORLD,ksp,ierr) FAILED!'
    STOP 666
  ENDIF
  CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
  IF(rank == 0) WRITE(*,*) '  Passed: CALL KSPCreate(...)'

  !test KSPSetType
  CALL KSPSetType(ksp,KSPGMRES,ierr)
  IF(ierr /= 0) THEN
    WRITE(*,*) 'CALL KSPSetType(solver%ksp,KSPGMRES,ierr) FAILED!'
    STOP 666
  ENDIF
  CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
  IF(rank == 0) WRITE(*,*) '  Passed: CALL KSPSetType(...)'

  !create matrix and vectors for solver
  ! A = [ 2 -1  0  0    b=[1.0
  !      -1  2 -1  0       1.2
  !       0 -1  2 -1       1.1
  !       0  0 -1  2]      0.8]
  !setup A
  CALL MatCreate(MPI_COMM_WORLD,A,ierr)
  CALL MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,4,4,ierr)
  CALL MatSetType(A,MATMPIDENSE,ierr)
  CALL MatSetUp(A,ierr)
  CALL MatSetValues(A,1,0,1,0,2.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,0,1,1,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,1,1,0,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,1,1,1,2.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,1,1,2,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,2,1,1,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,2,1,2,2.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,2,1,3,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,3,1,2,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,3,1,3,2.0_SRK,INSERT_VALUES,ierr)
  CALL MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
  CALL MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)
  !setup b
  CALL VecCreate(MPI_COMM_WORLD,b,ierr)
  CALL VecSetSizes(b,PETSC_DECIDE,4,ierr)
  CALL VecSetType(b,VECMPI,ierr)
  CALL VecSetFromOptions(b,ierr)
  CALL VecSetValue(b,0,1.0_SRK,INSERT_VALUES,ierr)
  CALL VecSetValue(b,1,1.2_SRK,INSERT_VALUES,ierr)
  CALL VecSetValue(b,2,1.1_SRK,INSERT_VALUES,ierr)
  CALL VecSetValue(b,3,0.8_SRK,INSERT_VALUES,ierr)
  CALL VecAssemblyBegin(b,ierr)
  CALL VecAssemblyEnd(b,ierr)
  !setup x
  CALL VecCreate(MPI_COMM_WORLD,x,ierr)
  CALL VecSetSizes(x,PETSC_DECIDE,4,ierr)
  CALL VecSetType(x,VECMPI,ierr)
  CALL VecSetFromOptions(x,ierr)
  CALL VecAssemblyBegin(x,ierr)
  CALL VecAssemblyEnd(x,ierr)

  !test KSPSetOperators
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=5))
  CALL KSPSetOperators(ksp,A,A,ierr)
#else
  CALL KSPSetOperators(ksp,A,A,DIFFERENT_NONZERO_PATTERN,ierr)
#endif

  IF(ierr /= 0) THEN
    WRITE(*,*) 'CALL KSPSetOperators(ksp,A,A,DIFFERENT_NONZERO_PATTERN,ierr) FAILED!'
    STOP 666
  ENDIF
  CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
  IF(rank == 0) WRITE(*,*) '  Passed: CALL KSPSetOperators(...)'

  !test KSPSetFromOptions
  CALL KSPSetFromOptions(ksp,ierr)
  IF(ierr /= 0) THEN
    WRITE(*,*) 'CALL KSPSetFromOptions(ksp,ierr) FAILED!'
    STOP 666
  ENDIF
  CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
  IF(rank == 0) WRITE(*,*) '  Passed: CALL KSPSetFromOptions(...)'

  !test KSPSetTolerances (will need to test more)
  rtol=1E-12
  abstol=1E-12
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=5))
  dtol=PETSC_DEFAULT_REAL
#else
  dtol=PETSC_DEFAULT_DOUBLE_PRECISION
#endif
  maxits=1000
  CALL KSPSetTolerances(ksp,rtol,abstol,dtol,maxits,ierr)
  IF(ierr /= 0) THEN
    WRITE(*,*) 'CALL KSPSetTolerances(ksp,rtol,abstol,dtol,maxits,ierr) FAILED!'
    STOP 666
  ENDIF
  CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
  IF(rank == 0) WRITE(*,*) '  Passed: CALL KSPSetTolerances(...)'

  !test KSPGMRESSetRestart
  restart=50
  CALL KSPGMRESSetRestart(ksp,restart,ierr)
  IF(ierr /= 0) THEN
    WRITE(*,*) 'CALL KSPGMRESSetRestart(ksp,restart,ierr) FAILED!'
    STOP 666
  ENDIF
  CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
  IF(rank == 0) WRITE(*,*) '  Passed: CALL KSPGMRESSetRestart(...)'

  !test KSPSolve
  CALL KSPSolve(ksp,b,x,ierr)
  IF(rank == 0) THEN
    CALL VecGetValues(x,1,(/0/),getval,ierr)
    IF(ABS(getval(1)-2.12_SRK)>1E-13 .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,0,getval,ierr) [KSPSolve] FAILED!'
      STOP 666
    ENDIF
  ENDIF
  IF(rank == 1) THEN
    CALL VecGetValues(x,1,(/1/),getval,ierr)
    IF(ABS(getval(1)-3.24_SRK)>1E-13 .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,1,getval,ierr) [KSPSolve] FAILED!'
      STOP 666
    ENDIF
  ENDIF
  IF(rank == 2) THEN
    CALL VecGetValues(x,1,(/2/),getval,ierr)
    IF(ABS(getval(1)-3.16_SRK)>1E-13 .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,2,getval,ierr) [KSPSolve] FAILED!'
      STOP 666
    ENDIF
  ENDIF
  IF(rank == 3) THEN
    CALL VecGetValues(x,1,(/3/),getval,ierr)
    IF(ABS(getval(1)-1.98_SRK)>1E-13 .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,2,getval,ierr) [KSPSolve] FAILED!'
      STOP 666
    ENDIF
  ENDIF
  CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
  IF(rank == 0) WRITE(*,*) '  Passed: CALL KSPSolve(...)'

  CALL KSPDestroy(ksp,ierr)
  IF(ierr /= 0) THEN
    WRITE(*,*) 'CALL KSPDestroy(ksp,ierr) FAILED!'
    STOP 666
  ENDIF
  CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
  IF(rank == 0) WRITE(*,*) '  Passed: CALL KSPDestroy(...)'
  CALL MatDestroy(A,ierr)
  CALL VecDestroy(b,ierr)
  CALL VecDestroy(x,ierr)
ENDSUBROUTINE testPETSC_KSP
!
!-------------------------------------------------------------------------------
SUBROUTINE testPETSC_KSP_SuperLU
  Vec :: x,b
  Mat :: A,F
  KSP :: ksp
  PC  :: pc
  PetscReal :: rtol,abstol,dtol
  PetscInt  :: maxits,restart
  REAL(SRK) :: getval(1)

  IF(rank == 0) WRITE(*,*)
  IF(rank == 0) WRITE(*,*) 'Testing with SuperLU'

  !create matrix and vectors for solver
  ! A = [ 2 -1  0  0    b=[1.0
  !      -1  2 -1  0       1.2
  !       0 -1  2 -1       1.1
  !       0  0 -1  2]      0.8]
  !setup A
  CALL MatCreate(MPI_COMM_WORLD,A,ierr)
  CALL MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,4,4,ierr)
  CALL MatSetType(A,MATMPIAIJ,ierr)
  CALL MatSetUp(A,ierr)
  CALL MatSetValues(A,1,0,1,0,2.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,0,1,1,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,1,1,0,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,1,1,1,2.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,1,1,2,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,2,1,1,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,2,1,2,2.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,2,1,3,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,3,1,2,-1.0_SRK,INSERT_VALUES,ierr)
  CALL MatSetValues(A,1,3,1,3,2.0_SRK,INSERT_VALUES,ierr)
  CALL MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
  CALL MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)
  !setup b
  CALL VecCreate(MPI_COMM_WORLD,b,ierr)
  CALL VecSetSizes(b,PETSC_DECIDE,4,ierr)
  CALL VecSetType(b,VECMPI,ierr)
  CALL VecSetFromOptions(b,ierr)
  CALL VecSetValue(b,0,1.0_SRK,INSERT_VALUES,ierr)
  CALL VecSetValue(b,1,1.2_SRK,INSERT_VALUES,ierr)
  CALL VecSetValue(b,2,1.1_SRK,INSERT_VALUES,ierr)
  CALL VecSetValue(b,3,0.8_SRK,INSERT_VALUES,ierr)
  CALL VecAssemblyBegin(b,ierr)
  CALL VecAssemblyEnd(b,ierr)
  !setup x
  CALL VecCreate(MPI_COMM_WORLD,x,ierr)
  CALL VecSetSizes(x,PETSC_DECIDE,4,ierr)
  CALL VecSetType(x,VECMPI,ierr)
  CALL VecSetFromOptions(x,ierr)
  CALL VecAssemblyBegin(x,ierr)
  CALL VecAssemblyEnd(x,ierr)

  !KSP calls
  CALL KSPCreate(MPI_COMM_WORLD,ksp,ierr)
  CALL KSPSetType(ksp,KSPPREONLY,ierr)
  CALL KSPGetPC(ksp,pc,ierr)
  CALL KSPSetFromOptions(ksp,ierr)
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=5))
  CALL KSPSetOperators(ksp,A,A,ierr)
#else
  CALL KSPSetOperators(ksp,A,A,DIFFERENT_NONZERO_PATTERN,ierr)
#endif

  !PC calls
  CALL PCSetType(pc,PCLU,ierr)
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>6))
  CALL PCFactorSetMatSolverType(pc,MATSOLVERSUPERLU_DIST,ierr)
  CALL PCFactorSetUpMatSolverType(pc,ierr)
#else
  CALL PCFactorSetMatSolverPackage(pc,MATSOLVERSUPERLU_DIST,ierr)
  CALL PCFactorSetUpMatSolverPackage(pc,ierr)
#endif
  CALL PCFactorGetMatrix(pc,F,ierr)

  !test KSPSolve
  CALL KSPSolve(ksp,b,x,ierr)
  IF(rank == 0) THEN
    CALL VecGetValues(x,1,(/0/),getval,ierr)
    IF(ABS(getval(1)-2.12_SRK)>1E-13 .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,0,getval,ierr) [KSPSolve] FAILED!'
      STOP 666
    ENDIF
  ENDIF
  IF(rank == 1) THEN
    CALL VecGetValues(x,1,(/1/),getval,ierr)
    IF(ABS(getval(1)-3.24_SRK)>1E-13 .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,1,getval,ierr) [KSPSolve] FAILED!'
      STOP 666
    ENDIF
  ENDIF
  IF(rank == 2) THEN
    CALL VecGetValues(x,1,(/2/),getval,ierr)
    IF(ABS(getval(1)-3.16_SRK)>1E-13 .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,2,getval,ierr) [KSPSolve] FAILED!'
      STOP 666
    ENDIF
  ENDIF
  IF(rank == 3) THEN
    CALL VecGetValues(x,1,(/3/),getval,ierr)
    IF(ABS(getval(1)-1.98_SRK)>1E-13 .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,2,getval,ierr) [KSPSolve] FAILED!'
      STOP 666
    ENDIF
  ENDIF
  CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
  IF(rank == 0) WRITE(*,*) '  Passed: CALL KSPSolve(...)'

  CALL KSPDestroy(ksp,ierr)
  CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
  CALL MatDestroy(A,ierr)
  CALL VecDestroy(b,ierr)
  CALL VecDestroy(x,ierr)
ENDSUBROUTINE testPETSC_KSP_SuperLU
!
ENDPROGRAM testParTPLPETSC
