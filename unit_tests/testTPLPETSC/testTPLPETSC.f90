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
PROGRAM testTPLPETSC
#include "UnitTest.h"
USE UnitTest

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
  INTEGER,PARAMETER :: SBK=KIND(.TRUE.)
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
  PetscErrorCode  :: ierr

  CREATE_TEST('Test PETSC TPL')
  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING PETSC TPL...'
  WRITE(*,*) '==================================================='

#ifdef MPACT_HAVE_PETSC
  CALL testPETSC_VEC()
  CALL testPETSC_MAT()
  CALL testPETSC_KSP()
#else
  WRITE(*,*) ' PETSC not enabled!'
#endif

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING PETSC TPL PASSED!'
  WRITE(*,*) '==================================================='

  CALL PetscFinalize(ierr)
  FINALIZE_TEST()

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
  SUBROUTINE testPETSC_VEC
    Vec :: b,x
    REAL(SRK) :: getval
    LOGICAL(SBK) :: bool

    !test VecCreate
    CALL VecCreate(MPI_COMM_WORLD,b,ierr)
    ASSERT(ierr == 0, 'VecCreate(MPI_COMM_WORLD,b,ierr)')
    CALL VecCreate(MPI_COMM_WORLD,x,ierr)
    ASSERT(ierr == 0, 'VecCreate(MPI_COMM_WORLD,x,ierr)')
    WRITE(*,*) '  Passed: CALL VecCreate(...)'

    !test VecSetSizes
    CALL VecSetSizes(b,PETSC_DECIDE,3,ierr)
    ASSERT(ierr == 0, 'VecSetSizes(b,PETSC_DECIDE,3,ierr)')
    CALL VecSetSizes(x,PETSC_DECIDE,3,ierr)
    ASSERT(ierr == 0, 'VecSetSizes(x,PETSC_DECIDE,3,ierr)')
    WRITE(*,*) '  Passed: CALL VecSetSizes(...)'

    !test VecSetType
    CALL VecSetType(b,VECMPI,ierr)
    ASSERT(ierr == 0, 'VecSetType(b,VECMPI,ierr)')
    CALL VecSetType(x,VECMPI,ierr)
    ASSERT(ierr == 0, 'VecSetType(x,VECMPI,ierr)')
    WRITE(*,*) '  Passed: CALL VecSetType(...)'

    !test VecSetFromOptions
    CALL VecSetFromOptions(b,ierr)
    ASSERT(ierr == 0, 'VecSetFromOptions(b,ierr)')
    CALL VecSetFromOptions(x,ierr)
    ASSERT(ierr == 0, 'VecSetFromOptions(x,ierr)')
    WRITE(*,*) '  Passed: CALL VecSetFromOptions(...)'

    !first we will build a vector with the following values:
    ! b = [5
    !      7
    !      2]
    !
    !test VecSetValue
    CALL VecSetValue(b,0,5.0_SRK,INSERT_VALUES,ierr)
    ASSERT(ierr == 0, 'VecSetValue(b,0,5.0_SRK,INSERT_VALUES,ierr)')
    CALL VecSetValue(b,1,7.0_SRK,INSERT_VALUES,ierr)
    ASSERT(ierr == 0, 'VecSetValue(b,1,7.0_SRK,INSERT_VALUES,ierr)')
    CALL VecSetValue(b,2,2.0_SRK,INSERT_VALUES,ierr)
    ASSERT(ierr == 0, 'VecSetValue(b,2,2.0_SRK,INSERT_VALUES,ierr)')
    WRITE(*,*) '  Passed: CALL VecSetValues(...)'

    !test VecAssembly
    CALL VecAssemblyBegin(b,ierr)
    ASSERT(ierr == 0, 'VecAssemblyBegin(b,ierr)')
    CALL VecAssemblyEnd(b,ierr)
    ASSERT(ierr == 0, 'VecAssemblyEnd(b,ierr)')
    WRITE(*,*) '  Passed: CALL VecAssembly...(...)'

    !test VecGetValues
    CALL VecGetValues(b,1,0,getval,ierr)
    bool = .NOT.(getval /= 5.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,0,getval,ierr) [VecSetValue]')
    CALL VecGetValues(b,1,1,getval,ierr)
    bool = .NOT.(getval /= 7.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,1,getval,ierr) [VecSetValue]')
    CALL VecGetValues(b,1,2,getval,ierr)
    bool = .NOT.(getval /= 2.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,2,getval,ierr) [VecSetValue]')

    !now we will test with all values set to 6:
    CALL VecSet(b,6.0_SRK,ierr)
    ASSERT(ierr == 0, 'VecSet(b,6.0_SRK,ierr)')
    CALL VecAssemblyBegin(b,ierr)
    CALL VecAssemblyEnd(b,ierr)
    CALL VecGetValues(b,1,0,getval,ierr)
    bool = .NOT.(getval /= 6.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,0,getval,ierr) [VecSet]')
    CALL VecGetValues(b,1,1,getval,ierr)
    bool = .NOT.(getval /= 6.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,1,getval,ierr) [VecSet]')
    CALL VecGetValues(b,1,2,getval,ierr)
    bool = .NOT.(getval /= 6.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,2,getval,ierr) [VecSet]')
    WRITE(*,*) '  Passed: CALL VecSet(...)'

    !we will now set up the following vectors to test the BLAS-related functions:
    ! b = [4      x = [2
    !      8           9
    !      3]          5]

    !setup b
    CALL VecSetValue(b,0,4.0_SRK,INSERT_VALUES,ierr)
    CALL VecSetValue(b,1,8.0_SRK,INSERT_VALUES,ierr)
    CALL VecSetValue(b,2,3.0_SRK,INSERT_VALUES,ierr)
    CALL VecAssemblyBegin(b,ierr)
    CALL VecAssemblyEnd(b,ierr)
    !setup x
    CALL VecSetValue(x,0,2.0_SRK,INSERT_VALUES,ierr)
    CALL VecSetValue(x,1,9.0_SRK,INSERT_VALUES,ierr)
    CALL VecSetValue(x,2,5.0_SRK,INSERT_VALUES,ierr)
    CALL VecAssemblyBegin(x,ierr)
    CALL VecAssemblyEnd(x,ierr)

    !test VecNorm
    CALL VecNorm(b,NORM_1,getval,ierr)
    bool = .NOT.(getval /= 15.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecNorm(b,NORM_1,getval,ierr)')
    CALL VecNorm(x,NORM_1,getval,ierr)
    bool = .NOT.(getval /= 16.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecNorm(x,NORM_1,getval,ierr)')
    CALL VecNorm(b,NORM_2,getval,ierr)
    bool = .NOT.(getval /= 9.4339811320566032_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecNorm(b,NORM_2,getval,ierr)')
    CALL VecNorm(x,NORM_2,getval,ierr)
    bool = .NOT.(getval /= 10.488088481701515_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecNorm(x,NORM_2,getval,ierr)')
    WRITE(*,*) '  Passed: CALL VecNorm(...)'

    CALL VecAXPY(b,3.0_SRK,x,ierr)
    CALL VecGetValues(b,1,0,getval,ierr)
    bool = .NOT.(getval /= 10.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,0,getval,ierr) [VecAXPY]')
    CALL VecGetValues(b,1,1,getval,ierr)
    bool = .NOT.(getval /= 35.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,1,getval,ierr) [VecAXPY]')
    CALL VecGetValues(b,1,2,getval,ierr)
    bool = .NOT.(getval /= 18.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,2,getval,ierr) [VecAXPY]')
    WRITE(*,*) '  Passed: CALL VecAXPY(...)'

    !test VecCopy by copying b into x
    CALL VecCopy(b,x,ierr)
    CALL VecGetValues(x,1,0,getval,ierr)
    bool = .NOT.(getval /= 10.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(x,1,0,getval,ierr) [VecCopy]')
    CALL VecGetValues(x,1,1,getval,ierr)
    bool = .NOT.(getval /= 35.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(x,1,1,getval,ierr) [VecCopy]')
    CALL VecGetValues(x,1,2,getval,ierr)
    bool = .NOT.(getval /= 18.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(x,1,2,getval,ierr) [VecCopy]')
    WRITE(*,*) '  Passed: CALL VecCopy(...)'

    !reset b and x
    CALL VecSetValue(b,0,4.0_SRK,INSERT_VALUES,ierr)
    CALL VecSetValue(b,1,8.0_SRK,INSERT_VALUES,ierr)
    CALL VecSetValue(b,2,3.0_SRK,INSERT_VALUES,ierr)
    CALL VecAssemblyBegin(b,ierr)
    CALL VecAssemblyEnd(b,ierr)
    CALL VecSetValue(x,0,2.0_SRK,INSERT_VALUES,ierr)
    CALL VecSetValue(x,1,9.0_SRK,INSERT_VALUES,ierr)
    CALL VecSetValue(x,2,5.0_SRK,INSERT_VALUES,ierr)
    CALL VecAssemblyBegin(x,ierr)
    CALL VecAssemblyEnd(x,ierr)

    !test VecTDot
    CALL VecTDot(b,x,getval,ierr)
    bool = .NOT.(getval /= 95.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecTDot(b,x,getval,ierr)')
    WRITE(*,*) '  Passed: CALL VecTDot(...)'

    !test VecScale
    CALL VecScale(b,4.0_SRK,ierr)
    CALL VecGetValues(b,1,0,getval,ierr)
    bool = .NOT.(getval /= 16.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,0,getval,ierr) [VecScale]')
    CALL VecGetValues(b,1,1,getval,ierr)
    bool = .NOT.(getval /= 32.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,1,getval,ierr) [VecScale]')
    CALL VecGetValues(b,1,2,getval,ierr)
    bool = .NOT.(getval /= 12.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,2,getval,ierr) [VecScale]')
    WRITE(*,*) '  Passed: CALL VecScale(...)'

    !test VecSwap
    CALL VecSwap(b,x,ierr)
    CALL VecGetValues(b,1,0,getval,ierr)
    bool = .NOT.(getval /= 2.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,0,getval,ierr) [VecSwap]')
    CALL VecGetValues(b,1,1,getval,ierr)
    bool = .NOT.(getval /= 9.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,1,getval,ierr) [VecSwap]')
    CALL VecGetValues(b,1,2,getval,ierr)
    bool = .NOT.(getval /= 5.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,2,getval,ierr) [VecSwap]')
    CALL VecGetValues(x,1,0,getval,ierr)
    bool = .NOT.(getval /= 16.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(x,1,0,getval,ierr) [VecSwap]')
    CALL VecGetValues(x,1,1,getval,ierr)
    bool = .NOT.(getval /= 32.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(x,1,1,getval,ierr) [VecSwap]')
    CALL VecGetValues(x,1,2,getval,ierr)
    bool = .NOT.(getval /= 12.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(x,1,2,getval,ierr) [VecSwap]')

    !test VecDestroy
    CALL VecDestroy(b,ierr)
    ASSERT(ierr == 0, 'VecDestroy(b,ierr)')
    WRITE(*,*) '  Passed: CALL VecDestroy(...)'
    CALL VecDestroy(x,ierr)
    ASSERT(ierr == 0, 'VecDestroy(b,ierr)')
    WRITE(*,*) '  Passed: CALL VecDestroy(...)'


  ENDSUBROUTINE testPETSC_VEC
!
!-------------------------------------------------------------------------------
  SUBROUTINE testPETSC_MAT
    Vec :: b,x
    Mat :: A
    REAL(SRK) :: getval
    LOGICAL :: bool

    !test MatCreate
    CALL MatCreate(MPI_COMM_WORLD,A,ierr)
    ASSERT(ierr == 0, 'MatCreate(MPI_COMM_WORLD,A,ierr)')
    WRITE(*,*) '  Passed: CALL MatCreate(...)'

    !test MatSetSizes
    CALL MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,3,3,ierr)
    ASSERT(ierr == 0, 'MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,3,3,ierr)')
    WRITE(*,*) '  Passed: CALL MatSetSizes(...)'

    !test MatSetType
    CALL MatSetType(A,MATMPIDENSE,ierr)
    ASSERT(ierr == 0, 'MatSetType(A,MATMPIDENSE,ierr)')
    CALL MatSetType(A,MATMPIAIJ,ierr)
    ASSERT(ierr == 0, 'MatSetType(A,MATMPIAIJ,ierr)')
    WRITE(*,*) '  Passed: CALL MatSetType(...)'

    !test MatSetUp
    CALL MatSetUp(A,ierr)
    ASSERT(ierr == 0, 'MatSetUp(A,ierr)')
    WRITE(*,*) '  Passed: CALL MatSetUp(...)'

    !using the 3x3 matrix that has been set up, insert the following values:
    ! A = [1 3 5
    !      6 2 8
    !      5 2 4]
    !test MatSetValues
    CALL MatSetValues(A,1,0,1,0,1.0_SRK,INSERT_VALUES,ierr)
    ASSERT(ierr == 0, 'MatSetValues(A,1,0,1,0,1.0_SRK,INSERT_VALUES,ierr)')
    CALL MatSetValues(A,1,0,1,1,3.0_SRK,INSERT_VALUES,ierr)
    ASSERT(ierr == 0, 'MatSetValues(A,1,0,1,1,3.0_SRK,INSERT_VALUES,ierr)')
    CALL MatSetValues(A,1,0,1,2,5.0_SRK,INSERT_VALUES,ierr)
    ASSERT(ierr == 0, 'MatSetValues(A,1,0,1,2,5.0_SRK,INSERT_VALUES,ierr)')
    CALL MatSetValues(A,1,1,1,0,6.0_SRK,INSERT_VALUES,ierr)
    ASSERT(ierr == 0, 'MatSetValues(A,1,1,1,0,6.0_SRK,INSERT_VALUES,ierr)')
    CALL MatSetValues(A,1,1,1,1,2.0_SRK,INSERT_VALUES,ierr)
    ASSERT(ierr == 0, 'MatSetValues(A,1,1,1,1,2.0_SRK,INSERT_VALUES,ierr)')
    CALL MatSetValues(A,1,1,1,2,8.0_SRK,INSERT_VALUES,ierr)
    ASSERT(ierr == 0, 'MatSetValues(A,1,1,1,2,8.0_SRK,INSERT_VALUES,ierr)')
    CALL MatSetValues(A,1,2,1,0,5.0_SRK,INSERT_VALUES,ierr)
    ASSERT(ierr == 0, 'MatSetValues(A,1,2,1,0,5.0_SRK,INSERT_VALUES,ierr)')
    CALL MatSetValues(A,1,2,1,1,2.0_SRK,INSERT_VALUES,ierr)
    ASSERT(ierr == 0, 'MatSetValues(A,1,2,1,1,2.0_SRK,INSERT_VALUES,ierr)')
    CALL MatSetValues(A,1,2,1,2,4.0_SRK,INSERT_VALUES,ierr)
    ASSERT(ierr == 0, 'MatSetValues(A,1,2,1,2,4.0_SRK,INSERT_VALUES,ierr)')
    WRITE(*,*) '  Passed: CALL MatSetValues(...)'

    !test MatAssembly
    CALL MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
    ASSERT(ierr == 0, 'MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)')
    CALL MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)
    ASSERT(ierr == 0, 'MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)')
    WRITE(*,*) '  Passed: CALL MatAssembly...(...)'

    !test MatGetValues
    CALL MatGetValues(A,1,0,1,0,getval,ierr)
    bool = .NOT.(getval /= 1.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'MatGetValues(A,1,1,1,1,getval,ierr)')
    CALL MatGetValues(A,1,0,1,1,getval,ierr)
    bool = .NOT.(getval /= 3.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'MatGetValues(A,1,1,1,2,getval,ierr)')
    CALL MatGetValues(A,1,0,1,2,getval,ierr)
    bool = .NOT.(getval /= 5.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'MatGetValues(A,1,1,1,3,getval,ierr)')
    CALL MatGetValues(A,1,1,1,0,getval,ierr)
    bool = .NOT.(getval /= 6.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'MatGetValues(A,1,2,1,1,getval,ierr)')
    CALL MatGetValues(A,1,1,1,1,getval,ierr)
    bool = .NOT.(getval /= 2.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'MatGetValues(A,1,2,1,2,getval,ierr)')
    CALL MatGetValues(A,1,1,1,2,getval,ierr)
    bool = .NOT.(getval /= 8.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'MatGetValues(A,1,2,1,3,getval,ierr)')
    CALL MatGetValues(A,1,2,1,0,getval,ierr)
    bool = .NOT.(getval /= 5.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'MatGetValues(A,1,3,1,1,getval,ierr)')
    CALL MatGetValues(A,1,2,1,1,getval,ierr)
    bool = .NOT.(getval /= 2.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'MatGetValues(A,1,3,1,2,getval,ierr)')
    CALL MatGetValues(A,1,2,1,2,getval,ierr)
    bool = .NOT.(getval /= 4.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'MatGetValues(A,1,3,1,3,getval,ierr)')
    WRITE(*,*) '  Passed: CALL MatGetValues(...)'

    !to test the following two interface, we need to set up two vectors:
    !setup b
    CALL VecCreate(MPI_COMM_WORLD,b,ierr)
    CALL VecSetSizes(b,PETSC_DECIDE,3,ierr)
    CALL VecSetType(b,VECMPI,ierr)
    CALL VecSetFromOptions(b,ierr)
    !setup x
    CALL VecCreate(MPI_COMM_WORLD,x,ierr)
    CALL VecSetSizes(x,PETSC_DECIDE,3,ierr)
    CALL VecSetType(x,VECMPI,ierr)
    CALL VecSetFromOptions(x,ierr)
    CALL VecSetValue(x,0,6.0_SRK,INSERT_VALUES,ierr)
    CALL VecSetValue(x,1,2.0_SRK,INSERT_VALUES,ierr)
    CALL VecSetValue(x,2,4.0_SRK,INSERT_VALUES,ierr)

    !test MatMult
    ! A = [1 3 5    x=[6
    !      6 2 8       2
    !      5 2 4]      4]
    CALL MatMult(A,x,b,ierr)
    CALL VecGetValues(b,1,0,getval,ierr)
    bool = .NOT.(getval /= 32.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,0,getval,ierr)  [MatMult]')
    CALL VecGetValues(b,1,1,getval,ierr)
    bool = .NOT.(getval /= 72.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,1,getval,ierr) [MatMult]')
    CALL VecGetValues(b,1,2,getval,ierr)
    bool = .NOT.(getval /= 50.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,2,getval,ierr) [MatMult]')
    WRITE(*,*) '  Passed: CALL MatMult(...)'

    !test MatMultTransport
    CALL MatMultTranspose(A,x,b,ierr)
    CALL VecGetValues(b,1,0,getval,ierr)
    bool = .NOT.(getval /= 38.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,0,getval,ierr) [MatMultTranspose]')
    CALL VecGetValues(b,1,1,getval,ierr)
    bool = .NOT.(getval /= 30.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,1,getval,ierr) [MatMultTranspose]')
    CALL VecGetValues(b,1,2,getval,ierr)
    bool = .NOT.(getval /= 62.0_SRK .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(b,1,2,getval,ierr) [MatMultTranspose]')
    WRITE(*,*) '  Passed: CALL MatMultTranspose(...)'

    !test MatDestroy
    CALL MatDestroy(A,ierr)
    IF(ierr /= 0) WRITE(*,*) 'CALL MatDestroy(A,ierr) FAILED!'
    WRITE(*,*) '  Passed: CALL MatDestroy(...)'
    CALL VecDestroy(b,ierr)
    CALL VecDestroy(x,ierr)

  ENDSUBROUTINE testPETSC_MAT
!
!-------------------------------------------------------------------------------
  SUBROUTINE testPETSC_KSP
    Vec :: x,b
    Mat :: A
    KSP :: ksp
    PetscReal :: rtol,abstol,dtol
    PetscInt  :: maxits,restart
    REAL(SRK) :: getval
    LOGICAL :: bool

    !test KSPCreate
    CALL KSPCreate(MPI_COMM_WORLD,ksp,ierr)
    ASSERT(ierr == 0, 'KSPCreate(MPI_COMM_WORLD,ksp,ierr)')
    WRITE(*,*) '  Passed: CALL KSPCreate(...)'

    !test KSPCreateType
    CALL KSPSetType(ksp,KSPBCGS,ierr)
    ASSERT(ierr == 0, 'KSPSetType(solver%ksp,KSPBCGS,ierr)')
    CALL KSPSetType(ksp,KSPCGNE,ierr)
    ASSERT(ierr == 0, 'KSPSetType(solver%ksp,KSPCGNE,ierr)')
    CALL KSPSetType(ksp,KSPGMRES,ierr)
    ASSERT(ierr == 0, 'KSPSetType(solver%ksp,KSPGMRES,ierr)')
    WRITE(*,*) '  Passed: CALL KSPSetType(...)'

    !create matrix and vectors for solver
    ! A = [9 3 5    b=[6
    !      6 8 2       2
    !      5 2 4]      4]
    !setup A
    CALL MatCreate(MPI_COMM_WORLD,A,ierr)
    CALL MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,3,3,ierr)
    CALL MatSetType(A,MATMPIDENSE,ierr)
    CALL MatSetUp(A,ierr)
    CALL MatSetValues(A,1,0,1,0,9.0_SRK,INSERT_VALUES,ierr)
    CALL MatSetValues(A,1,0,1,1,3.0_SRK,INSERT_VALUES,ierr)
    CALL MatSetValues(A,1,0,1,2,5.0_SRK,INSERT_VALUES,ierr)
    CALL MatSetValues(A,1,1,1,0,6.0_SRK,INSERT_VALUES,ierr)
    CALL MatSetValues(A,1,1,1,1,8.0_SRK,INSERT_VALUES,ierr)
    CALL MatSetValues(A,1,1,1,2,2.0_SRK,INSERT_VALUES,ierr)
    CALL MatSetValues(A,1,2,1,0,5.0_SRK,INSERT_VALUES,ierr)
    CALL MatSetValues(A,1,2,1,1,2.0_SRK,INSERT_VALUES,ierr)
    CALL MatSetValues(A,1,2,1,2,4.0_SRK,INSERT_VALUES,ierr)
    CALL MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
    CALL MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)
    !setup b
    CALL VecCreate(MPI_COMM_WORLD,b,ierr)
    CALL VecSetSizes(b,PETSC_DECIDE,3,ierr)
    CALL VecSetType(b,VECMPI,ierr)
    CALL VecSetFromOptions(b,ierr)
    CALL VecSetValue(b,0,6.0_SRK,INSERT_VALUES,ierr)
    CALL VecSetValue(b,1,2.0_SRK,INSERT_VALUES,ierr)
    CALL VecSetValue(b,2,4.0_SRK,INSERT_VALUES,ierr)
    CALL VecAssemblyBegin(b,ierr)
    CALL VecAssemblyEnd(b,ierr)
    !setup x
    CALL VecCreate(MPI_COMM_WORLD,x,ierr)
    CALL VecSetSizes(x,PETSC_DECIDE,3,ierr)
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
    ASSERT(ierr == 0, 'KSPSetOperators(ksp,A,A,DIFFERENT_NONZERO_PATTERN,ierr)')
    WRITE(*,*) '  Passed: CALL KSPSetOperators(...)'

    !test KSPSetFromOptions
    CALL KSPSetFromOptions(ksp,ierr)
    ASSERT(ierr == 0, 'KSPSetFromOptions(ksp,ierr)')
    WRITE(*,*) '  Passed: CALL KSPSetFromOptions(...)'

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
    ASSERT(ierr == 0, 'KSPSetTolerances(ksp,rtol,abstol,dtol,maxits,ierr)')
    WRITE(*,*) '  Passed: CALL KSPSetTolerances(...)'

    !test KSPGMRESSetRestart
    restart=50
    CALL KSPGMRESSetRestart(ksp,restart,ierr)
    ASSERT(ierr == 0, 'KSPGMRESSetRestart(ksp,restart,ierr)')
    WRITE(*,*) '  Passed: CALL KSPGMRESSetRestart(...)'

    !test KSPSolve
    CALL KSPSolve(ksp,b,x,ierr)
    CALL VecGetValues(x,1,0,getval,ierr)
    bool = .NOT.(ABS(getval-0.4_SRK)>1E-13 .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(x,1,0,getval,ierr) [KSPSolve]')
    CALL VecGetValues(x,1,1,getval,ierr)
    bool = .NOT.(ABS(getval+0.2_SRK)>1E-13 .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(x,1,1,getval,ierr) [KSPSolve]')
    CALL VecGetValues(x,1,2,getval,ierr)
    bool = .NOT.(ABS(getval-0.6_SRK)>1E-13 .OR. ierr /= 0)
    ASSERT(bool, 'VecGetValues(x,1,2,getval,ierr) [KSPSolve]')
    WRITE(*,*) '  Passed: CALL KSPSolve(...)'

    CALL KSPDestroy(ksp,ierr)
    ASSERT(ierr == 0, 'KSPDestroy(ksp,ierr)')
    WRITE(*,*) '  Passed: CALL KSPDestroy(...)'
    CALL MatDestroy(A,ierr)
    CALL VecDestroy(b,ierr)
    CALL VecDestroy(x,ierr)
  ENDSUBROUTINE testPETSC_KSP


ENDPROGRAM testTPLPETSC
