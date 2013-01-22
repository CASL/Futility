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
  PetscErrorCode  :: ierr

  CALL PetscInitialize(PETSC_NULL_CHARACTER,ierr)

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING PETSC TPL...'
  WRITE(*,*) '==================================================='

#ifdef HAVE_PETSC
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

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
  SUBROUTINE testPETSC_VEC
    Vec :: b,x
    REAL(SRK) :: getval

    !test VecCreate
    CALL VecCreate(MPI_COMM_WORLD,b,ierr)
    IF(ierr /= 0) THEN 
      WRITE(*,*) 'CALL VecCreate(MPI_COMM_WORLD,b,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL VecCreate(MPI_COMM_WORLD,x,ierr)
    IF(ierr /= 0) THEN 
      WRITE(*,*) 'CALL VecCreate(MPI_COMM_WORLD,x,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL VecCreate(...)'
    
    !test VecSetSizes
    CALL VecSetSizes(b,PETSC_DECIDE,3,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL VecSetSizes(b,PETSC_DECIDE,3,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL VecSetSizes(x,PETSC_DECIDE,3,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL VecSetSizes(x,PETSC_DECIDE,3,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL VecSetSizes(...)'
    
    !test VecSetType
    CALL VecSetType(b,VECMPI,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL VecSetType(b,VECMPI,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL VecSetType(x,VECMPI,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL VecSetType(x,VECMPI,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL VecSetType(...)'
    
    !test VecSetFromOptions
    CALL VecSetFromOptions(b,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL VecSetFromOptions(b,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL VecSetFromOptions(x,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL VecSetFromOptions(x,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL VecSetFromOptions(...)'
    
    !first we will build a vector with the following values:
    ! b = [5
    !      7
    !      2]
    !
    !test VecSetValue
    CALL VecSetValue(b,0,5.0_SRK,INSERT_VALUES,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL VecSetValue(b,0,5.0_SRK,INSERT_VALUES,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL VecSetValue(b,1,7.0_SRK,INSERT_VALUES,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL VecSetValue(b,1,7.0_SRK,INSERT_VALUES,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL VecSetValue(b,2,2.0_SRK,INSERT_VALUES,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL VecSetValue(b,2,2.0_SRK,INSERT_VALUES,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL VecSetValues(...)'
    
    !test VecAssembly
    CALL VecAssemblyBegin(b,ierr)
    IF(ierr /= 0) THEN 
      WRITE(*,*) 'CALL VecAssemblyBegin(b,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL VecAssemblyEnd(b,ierr)
    IF(ierr /= 0) THEN 
      WRITE(*,*) 'CALL VecAssemblyEnd(b,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL VecAssembly...(...)'
    
    !test VecGetValues
    CALL VecGetValues(b,1,0,getval,ierr)
    IF(getval /= 5.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,0,getval,ierr) [VecSetValue] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,1,getval,ierr)
    IF(getval /= 7.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,1,getval,ierr) [VecSetValue] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,2,getval,ierr)
    IF(getval /= 2.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,2,getval,ierr) [VecSetValue] FAILED!'
      STOP 666
    ENDIF
    
    !now we will test with all values set to 6:
    CALL VecSet(b,6.0_SRK,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL VecSet(b,6.0_SRK,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL VecAssemblyBegin(b,ierr)
    CALL VecAssemblyEnd(b,ierr)
    CALL VecGetValues(b,1,0,getval,ierr)
    IF(getval /= 6.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,0,getval,ierr) [VecSet] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,1,getval,ierr)
    IF(getval /= 6.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,1,getval,ierr) [VecSet] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,2,getval,ierr)
    IF(getval /= 6.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,2,getval,ierr) [VecSet] FAILED!'
      STOP 666
    ENDIF
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
    IF(getval /= 15.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecNorm(b,NORM_1,getval,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL VecNorm(x,NORM_1,getval,ierr)
    IF(getval /= 16.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecNorm(x,NORM_1,getval,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL VecNorm(b,NORM_2,getval,ierr)
    IF(getval /= 9.4339811320566032_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecNorm(b,NORM_2,getval,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL VecNorm(x,NORM_2,getval,ierr)
    IF(getval /= 10.488088481701515_SRK .OR. ierr /= 0) THEN 
      WRITE(*,*) 'CALL VecNorm(x,NORM_2,getval,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL VecNorm(...)'
    
    CALL VecAXPY(b,3.0_SRK,x,ierr)
    CALL VecGetValues(b,1,0,getval,ierr)
    IF(getval /= 10.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,0,getval,ierr) [VecAXPY] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,1,getval,ierr)
    IF(getval /= 35.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,1,getval,ierr) [VecAXPY] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,2,getval,ierr)
    IF(getval /= 18.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,2,getval,ierr) [VecAXPY] FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL VecAXPY(...)'
    
    !test VecCopy by copying b into x
    CALL VecCopy(b,x,ierr)
    CALL VecGetValues(x,1,0,getval,ierr)
    IF(getval /= 10.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,0,getval,ierr) [VecCopy] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(x,1,1,getval,ierr)
    IF(getval /= 35.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,1,getval,ierr) [VecCopy] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(x,1,2,getval,ierr)
    IF(getval /= 18.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,2,getval,ierr) [VecCopy] FAILED!'
      STOP 666
    ENDIF
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
    IF(getval /= 95.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecTDot(b,x,getval,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL VecTDot(...)'
    
    !test VecScale
    CALL VecScale(b,4.0_SRK,ierr)
    CALL VecGetValues(b,1,0,getval,ierr)
    IF(getval /= 16.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,0,getval,ierr) [VecScale] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,1,getval,ierr)
    IF(getval /= 32.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,1,getval,ierr) [VecScale] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,2,getval,ierr)
    IF(getval /= 12.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,2,getval,ierr) [VecScale] FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL VecScale(...)'
    
    !test VecSwap
    CALL VecSwap(b,x,ierr)
    CALL VecGetValues(b,1,0,getval,ierr)
    IF(getval /= 2.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,0,getval,ierr) [VecSwap] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,1,getval,ierr)
    IF(getval /= 9.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,1,getval,ierr) [VecSwap] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,2,getval,ierr)
    IF(getval /= 5.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,2,getval,ierr) [VecSwap] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(x,1,0,getval,ierr)
    IF(getval /= 16.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,0,getval,ierr) [VecSwap] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(x,1,1,getval,ierr)
    IF(getval /= 32.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,1,getval,ierr) [VecSwap] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(x,1,2,getval,ierr)
    IF(getval /= 12.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(x,1,2,getval,ierr) [VecSwap] FAILED!'
      STOP 666
    ENDIF
    
    !test VecDestroy
    CALL VecDestroy(b,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL VecDestroy(b,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL VecDestroy(...)'
  
  ENDSUBROUTINE testPETSC_VEC
!
!-------------------------------------------------------------------------------
  SUBROUTINE testPETSC_MAT
    Vec :: b,x
    Mat :: A
    REAL(SRK) :: getval
    
    !test MatCreate
    CALL MatCreate(MPI_COMM_WORLD,A,ierr)
    IF(ierr /= 0) THEN 
      WRITE(*,*) 'CALL MatCreate(MPI_COMM_WORLD,A,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL MatCreate(...)'
    
    !test MatSetSizes
    CALL MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,3,3,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,3,3,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL MatSetSizes(...)'
    
    !test MatSetType
    CALL MatSetType(A,MATMPIDENSE,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatSetType(A,MATMPIDENSE,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatSetType(A,MATMPIAIJ,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatSetType(A,MATMPIAIJ,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL MatSetType(...)'
    
    !test MatSetUp
    CALL MatSetUp(A,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatSetUp(A,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL MatSetUp(...)'
    
    !using the 3x3 matrix that has been set up, insert the following values:
    ! A = [1 3 5
    !      6 2 8
    !      5 2 4]
    !test MatSetValues
    CALL MatSetValues(A,1,0,1,0,1.0_SRK,INSERT_VALUES,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatSetValues(A,1,0,1,0,1.0_SRK,INSERT_VALUES,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatSetValues(A,1,0,1,1,3.0_SRK,INSERT_VALUES,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatSetValues(A,1,0,1,1,3.0_SRK,INSERT_VALUES,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatSetValues(A,1,0,1,2,5.0_SRK,INSERT_VALUES,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatSetValues(A,1,0,1,2,5.0_SRK,INSERT_VALUES,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatSetValues(A,1,1,1,0,6.0_SRK,INSERT_VALUES,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatSetValues(A,1,1,1,0,6.0_SRK,INSERT_VALUES,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatSetValues(A,1,1,1,1,2.0_SRK,INSERT_VALUES,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatSetValues(A,1,1,1,1,2.0_SRK,INSERT_VALUES,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatSetValues(A,1,1,1,2,8.0_SRK,INSERT_VALUES,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatSetValues(A,1,1,1,2,8.0_SRK,INSERT_VALUES,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatSetValues(A,1,2,1,0,5.0_SRK,INSERT_VALUES,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatSetValues(A,1,2,1,0,5.0_SRK,INSERT_VALUES,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatSetValues(A,1,2,1,1,2.0_SRK,INSERT_VALUES,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatSetValues(A,1,2,1,1,2.0_SRK,INSERT_VALUES,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatSetValues(A,1,2,1,2,4.0_SRK,INSERT_VALUES,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatSetValues(A,1,2,1,2,4.0_SRK,INSERT_VALUES,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL MatSetValues(...)'
    
    !test MatAssembly
    CALL MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)
    IF(ierr /= 0) THEN 
      WRITE(*,*) 'CALL MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL MatAssembly...(...)'
    
    !test MatGetValues
    CALL MatGetValues(A,1,0,1,0,getval,ierr)
    IF(getval /= 1.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL MatGetValues(A,1,1,1,1,getval,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatGetValues(A,1,0,1,1,getval,ierr)
    IF(getval /= 3.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL MatGetValues(A,1,1,1,2,getval,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatGetValues(A,1,0,1,2,getval,ierr)
    IF(getval /= 5.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL MatGetValues(A,1,1,1,3,getval,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatGetValues(A,1,1,1,0,getval,ierr)
    IF(getval /= 6.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL MatGetValues(A,1,2,1,1,getval,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatGetValues(A,1,1,1,1,getval,ierr)     
    IF(getval /= 2.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL MatGetValues(A,1,2,1,2,getval,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatGetValues(A,1,1,1,2,getval,ierr)
    IF(getval /= 8.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL MatGetValues(A,1,2,1,3,getval,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatGetValues(A,1,2,1,0,getval,ierr)
    IF(getval /= 5.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL MatGetValues(A,1,3,1,1,getval,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatGetValues(A,1,2,1,1,getval,ierr)
    IF(getval /= 2.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL MatGetValues(A,1,3,1,2,getval,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL MatGetValues(A,1,2,1,2,getval,ierr)
    IF(getval /= 4.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL MatGetValues(A,1,3,1,3,getval,ierr) FAILED!'
      STOP 666
    ENDIF
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
    IF(getval /= 32.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,0,getval,ierr) [MatMult] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,1,getval,ierr)
    IF(getval /= 72.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,1,getval,ierr) [MatMult] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,2,getval,ierr)
    IF(getval /= 50.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,2,getval,ierr) [MatMult] FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL MatMult(...)'
    
    !test MatMultTransport
    CALL MatMultTranspose(A,x,b,ierr)
    CALL VecGetValues(b,1,0,getval,ierr)
    IF(getval /= 38.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,0,getval,ierr) [MatMultTranspose] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,1,getval,ierr)
    IF(getval /= 30.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,1,getval,ierr) [MatMultTranspose] FAILED!'
      STOP 666
    ENDIF
    CALL VecGetValues(b,1,2,getval,ierr)
    IF(getval /= 62.0_SRK .OR. ierr /= 0) THEN
      WRITE(*,*) 'CALL VecGetValues(b,1,2,getval,ierr) [MatMultTranspose] FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL MatMultTranspose(...)'
    
    !test MatDestroy
    CALL MatDestroy(A,ierr)
    IF(ierr /= 0) WRITE(*,*) 'CALL MatDestroy(A,ierr) FAILED!'
    WRITE(*,*) '  Passed: CALL MatDestroy(...)'

  ENDSUBROUTINE testPETSC_MAT
!
!-------------------------------------------------------------------------------
  SUBROUTINE testPETSC_KSP
    Vec :: x,b
    Mat :: A
    KSP :: ksp
    PetscReal :: rtol=1E-8,abstol=1E-8,dtol=1E-1
    PetscInt  :: maxits=1000,restart=50
    REAL(SRK) :: getval
   
    !test KSPCreate
    CALL KSPCreate(MPI_COMM_WORLD,ksp,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL KSPCreate(MPI_COMM_WORLD,ksp,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL KSPCreate(...)'
    
    !test KSPCreateType
    CALL KSPSetType(ksp,KSPBCGS,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL KSPSetType(solver%ksp,KSPBCGS,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL KSPSetType(ksp,KSPCGNE,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL KSPSetType(solver%ksp,KSPCGNE,ierr) FAILED!'
      STOP 666
    ENDIF
    CALL KSPSetType(ksp,KSPGMRES,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL KSPSetType(solver%ksp,KSPGMRES,ierr) FAILED!'
      STOP 666
    ENDIF
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
    CALL KSPSetOperators(ksp,A,A,DIFFERENT_NONZERO_PATTERN,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL KSPSetOperators(ksp,A,A,DIFFERENT_NONZERO_PATTERN,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL KSPSetOperators(...)'
    
    !test KSPSetFromOptions
    CALL KSPSetFromOptions(ksp,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL KSPSetFromOptions(ksp,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL KSPSetFromOptions(...)'
    
!    test KSPSetTolerances (will need to test more)
!    CALL KSPSetTolerances(ksp,rtol,abstol,dtol,maxits,ierr)
!    IF(ierr /= 0) THEN
!      WRITE(*,*) 'CALL KSPSetTolerances(ksp,rtol,abstol,dtol,maxits,ierr) FAILED!'
!      STOP 666
!    ENDIF
!    WRITE(*,*) '  Passed: CALL KSPSetTolerances(...)'
    
    !test KSPGMRESSetRestart
    CALL KSPGMRESSetRestart(ksp,restart,ierr)
    IF(ierr /= 0) THEN
      WRITE(*,*) 'CALL KSPGMRESSetRestart(ksp,restart,ierr) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL KSPGMRESSetRestart(...)'
    
!    !test KSPSolve
!    CALL KSPSolve(ksp,b,x,ierr)
!    CALL VecGetValues(x,1,0,getval,ierr)
!    IF(getval /= 0.4_SRK .OR. ierr /= 0) THEN
!      WRITE(*,*) getval
!      WRITE(*,*) 'CALL VecGetValues(x,1,0,getval,ierr) [KSPSolve] FAILED!'
!      STOP 666
!    ENDIF
!    CALL VecGetValues(x,1,1,getval,ierr)
!    IF(getval /= -0.2_SRK .OR. ierr /= 0) THEN
!      WRITE(*,*) getval
!      WRITE(*,*) 'CALL VecGetValues(x,1,1,getval,ierr) [KSPSolve] FAILED!'
!      STOP 666
!    ENDIF
!    CALL VecGetValues(x,1,2,getval,ierr)
!    IF(getval /= 0.6_SRK .OR. ierr /= 0) THEN
!      WRITE(*,*) getval
!      WRITE(*,*) 'CALL VecGetValues(x,1,2,getval,ierr) [KSPSolve] FAILED!'
!      STOP 666
!    ENDIF
!    WRITE(*,*) '  Passed: CALL KSPSolve(...)'
!    
!    CALL KSPDestroy(ksp,ierr)
!    IF(ierr /= 0) THEN
!      WRITE(*,*) 'CALL KSPDestroy(ksp,ierr) FAILED!'
!      STOP 666
!    ENDIF
!    WRITE(*,*) '  Passed: CALL KSPDestroy(...)'
  
  ENDSUBROUTINE testPETSC_KSP


ENDPROGRAM testTPLPETSC