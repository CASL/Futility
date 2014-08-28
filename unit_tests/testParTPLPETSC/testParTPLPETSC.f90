!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                              Copyright (C) 2012                              !
!                   The Regents of the University of Michigan                  !
!              MPACT Development Group and Prof. Thomas J. Downar              !
!                             All rights reserved.                             !
!                                                                              !
! Copyright is reserved to the University of Michigan for purposes of          !
! controlled dissemination, commercialization through formal licensing, or     !
! other disposition. The University of Michigan nor any of their employees,    !
! makes any warranty, express or implied, or assumes any liability or          !
! responsibility for the accuracy, completeness, or usefulness of any          !
! information, apparatus, product, or process disclosed, or represents that    !
! its use would not infringe privately owned rights. Reference herein to any   !
! specific commercial products, process, or service by trade name, trademark,  !
! manufacturer, or otherwise, does not necessarily constitute or imply its     !
! endorsement, recommendation, or favoring by the University of Michigan.      !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testParTPLPETSC

  IMPLICIT NONE

#include <finclude/petsc.h>
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

#ifdef MPACT_HAVE_PETSC
  CALL testPETSC_KSP()
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
    REAL(SRK) :: getval

    !test KSPCreate
    CALL KSPCreate(MPI_COMM_WORLD,ksp,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL KSPCreate(MPI_COMM_WORLD,ksp,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
    IF(rank == 0) WRITE(*,*) '  Passed: CALL KSPCreate(...)'

    !test KSPCreateType
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
    CALL KSPSetOperators(ksp,A,A,DIFFERENT_NONZERO_PATTERN,ierr)
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
      CALL VecGetValues(x,1,0,getval,ierr)
      IF(ABS(getval-2.12_SRK)>1E-13 .OR. ierr /= 0) THEN
        WRITE(*,*) 'CALL VecGetValues(x,1,0,getval,ierr) [KSPSolve] FAILED!'
        STOP 666
      ENDIF
    ENDIF
    IF(rank == 1) THEN
      CALL VecGetValues(x,1,1,getval,ierr)
      IF(ABS(getval-3.24_SRK)>1E-13 .OR. ierr /= 0) THEN
        WRITE(*,*) 'CALL VecGetValues(x,1,1,getval,ierr) [KSPSolve] FAILED!'
        STOP 666
      ENDIF
    ENDIF
    IF(rank == 2) THEN
      CALL VecGetValues(x,1,2,getval,ierr)
      IF(ABS(getval-3.16_SRK)>1E-13 .OR. ierr /= 0) THEN
        WRITE(*,*) 'CALL VecGetValues(x,1,2,getval,ierr) [KSPSolve] FAILED!'
        STOP 666
      ENDIF
    ENDIF
    IF(rank == 3) THEN
      CALL VecGetValues(x,1,3,getval,ierr)
      IF(ABS(getval-1.98_SRK)>1E-13 .OR. ierr /= 0) THEN
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

  ENDSUBROUTINE testPETSC_KSP


ENDPROGRAM testParTPLPETSC