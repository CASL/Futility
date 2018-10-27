!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testTPLSLEPC

  IMPLICIT NONE

#include <petscversion.h>
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
#include <petsc/finclude/petsc.h>
#include <slepc/finclude/slepc.h>
#else
#include <finclude/petsc.h>
#include <finclude/slepc.h>
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

  INTEGER(SIK) :: rank

  !define error code
  PetscErrorCode  :: ierr

  CALL SlepcInitialize(PETSC_NULL_CHARACTER,ierr)
  CALL MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING SLEPC TPL...'
  WRITE(*,*) '==================================================='

#ifdef FUTILITY_HAVE_SLEPC
  CALL testEX1F()
#else
  WRITE(*,*) ' SLEPC not enabled!'
#endif

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING SLEPC TPL PASSED!'
  WRITE(*,*) '==================================================='

  CALL SlepcFinalize(ierr)

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
  SUBROUTINE testEX1F()
    INTEGER(SIK) :: i,n,Istart,Iend
    INTEGER(SIK) :: i1,i2,i3
    INTEGER(SIK) :: nev,maxit,its,nconv
    INTEGER(SIK) :: col(3)
    REAL(SRK) :: tol,error
    REAL(SRK) :: kr,ki
    REAL(SRK) :: value(3)
    REAL(SRK) :: refeig(3),referr(3)
    Mat A
    EPS eps
    EPSType tname

    !set reference values
    refeig(1)=3.996206657474086_SRK
    refeig(2)=3.984841019343870_SRK
    refeig(3)=3.965946199367805_SRK
    referr(1)=4.303631495901956E-10_SRK
    referr(2)=2.086312056401549E-09_SRK
    referr(3)=9.984036738039173E-09_SRK

! This test problem was adapted from an example at:
! http://www.grycap.upv.es/slepc/documentation/current/src/eps/examples/tutorials/ex1f.F
! which has more information on this page
! http://www.grycap.upv.es/slepc/handson/handson1.html

    n = 50
    IF(rank == 0) THEN
      WRITE(*,'(A)') ' Running test problem EX1F...'
      WRITE(*,'(A,I3)') '    1-D Laplacian Eigenproblem, n =',n
    ENDIF

! Setup A
    CALL MatCreate(PETSC_COMM_WORLD,A,ierr)
    CALL MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,n,n,ierr)
    CALL MatSetFromOptions(A,ierr)
    CALL MatSetUp(A,ierr)

    i1 = 1
    i2 = 2
    i3 = 3
    call MatGetOwnershipRange(A,Istart,Iend,ierr)
    IF(Istart == 0) THEN
      i = 0
      col(1) = 0
      col(2) = 1
      value(1) =  2.0_SRK
      value(2) = -1.0_SRK
      call MatSetValues(A,i1,i,i2,col,value,INSERT_VALUES,ierr)
      Istart = Istart+1
    ENDIF
    IF(Iend == n) THEN
      i = n-1
      col(1) = n-2
      col(2) = n-1
      value(1) = -1.0_SRK
      value(2) =  2.0_SRK
      CALL MatSetValues(A,i1,i,i2,col,value,INSERT_VALUES,ierr)
      Iend = Iend-1
    ENDIF
    value(1) = -1.0_SRK
    value(2) =  2.0_SRK
    value(3) = -1.0_SRK
    DO i=Istart,Iend-1
      col(1) = i-1
      col(2) = i
      col(3) = i+1
      CALL MatSetValues(A,i1,i,i3,col,value,INSERT_VALUES,ierr)
    ENDDO

    CALL MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
    CALL MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)

! Create eigensolver and print basic info

    ! create eigensolver
    CALL EPSCreate(PETSC_COMM_WORLD,eps,ierr)

    ! Set operators. In this case, it is a standard eigenvalue problem
    CALL EPSSetOperators(eps,A,PETSC_NULL_OBJECT,ierr)
    CALL EPSSetProblemType(eps,EPS_HEP,ierr)

    ! Set solver parameters at runtime
    CALL EPSSetFromOptions(eps,ierr)

! Solve the eigensystem
    CALL EPSSolve(eps,ierr)
    CALL EPSGetIterationNumber(eps,its,ierr)
    IF(rank == 0) THEN
      WRITE(*,'(A,I4)') '    Number of iterations of the method:',its
    ENDIF

    ! Optional: Get some information from the solver and display it
    call EPSGetType(eps,tname,ierr)
    IF(rank == 0) THEN
      WRITE(*,'(A,A)') '    Solution method: ',tname
    ENDIF
    CALL EPSGetDimensions(eps,nev,PETSC_NULL_INTEGER,PETSC_NULL_INTEGER,ierr)
    IF(rank == 0) THEN
      WRITE(*,'(A,I2)') '    Number of requested eigenvalues:',nev
    ENDIF
    CALL EPSGetTolerances(eps,tol,maxit,ierr)
    IF(rank == 0) THEN
      WRITE(*,'(A,E10.4,A,I4)') '    Stopping condition: tol= ',tol,', maxit=',maxit
    ENDIF

! Display solution and clean up
    ! Get number of converged eigenpairs
    CALL EPSGetConverged(eps,nconv,ierr)
    IF(rank == 0) THEN
      WRITE(*,'(A,I2)') '    Number of converged eigenpairs:',nconv
    ENDIF

    ! Display eigenvalues and relative errors
    IF (nconv > 0) THEN
      IF(rank .eq. 0) THEN
        WRITE(*,*) '            k          ||Ax-kx||/||kx||'
        WRITE(*,*) '    ----------------- ------------------'
      ENDIF
      DO i=0,nconv-1
        ! Get converged eigenpairs: i-th eigenvalue is stored in kr
        ! (real part) and ki (imaginary part)
        CALL EPSGetEigenpair(eps,i,kr,ki,PETSC_NULL_OBJECT,PETSC_NULL_OBJECT,ierr)

        ! Compute the relative error associated to each eigenpair
#if ((PETSC_VERSION_MAJOR>=3) && (PETSC_VERSION_MINOR>=6))
        CALL EPSComputeError(eps,i,EPS_ERROR_RELATIVE,error,ierr)
#else
        CALL EPSComputeRelativeError(eps,i,error,ierr)
#endif
        IF(rank == 0) THEN
          WRITE(*,'(A,F20.15,A,ES25.15)') '       ',PetscRealPart(kr),'       ',error
          !check eigenvalue
          IF(ABS(PetscRealPart(kr)-refeig(i+1)) > tol) THEN
            WRITE(*,*) 'Eigenvalue',i+1,' incorrect'
            WRITE(*,*) PetscRealPart(kr),refeig(i+1)
            STOP 666
          ENDIF
          !check error
          IF(ABS(error-referr(i+1)) > tol) THEN
            WRITE(*,*) 'Eigenvalue',i+1,' incorrect'
            WRITE(*,*) PetscRealPart(kr),refeig(i+1)
            STOP 666
          ENDIF
        ENDIF

      ENDDO
      IF(rank == 0) WRITE(*,*)
    ENDIF

    ! destroy eigensolver and matrix
    CALL EPSDestroy(eps,ierr)
    CALL MatDestroy(A,ierr)

  ENDSUBROUTINE testEX1F

ENDPROGRAM testTPLSLEPC

