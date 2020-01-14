!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
#ifdef HAVE_PARDISO
include 'mkl_pardiso.f90'
#endif
PROGRAM testTPLPARDISO

#ifdef HAVE_PARDISO
USE MKL_PARDISO
#endif

IMPLICIT NONE

! define precision kinds
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

WRITE(*,*) '==================================================='
WRITE(*,*) 'TESTING TPLS...'
WRITE(*,*) '==================================================='

#ifdef HAVE_PARDISO
CALL testPARDISOINIT()
CALL testPARDISO()
#else
WRITE(*,*) ' PARDISO not enabled!'
#endif

WRITE(*,*) '==================================================='
WRITE(*,*) 'TESTING TPLS PASSED!'
WRITE(*,*) '==================================================='

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testPARDISOINIT()
#ifdef HAVE_PARDISO
INTEGER(SIK) :: i

TYPE(MKL_PARDISO_HANDLE):: pt(64)
INTEGER(SIK) :: mtype
INTEGER(SIK) :: iparm(64)

DO i=1,64
  iparm(i) = 0
  pt(i)%dummy=0
ENDDO
iparm(3)=1 ! nthreads

CALL PARDISOINIT(pt,mtype,iparm)

WRITE(*,*) '  Passed: CALL PARDISOINIT()'
#endif
ENDSUBROUTINE testPARDISOINIT

!
!-------------------------------------------------------------------------------
SUBROUTINE testPARDISO()
#ifdef HAVE_PARDISO
INTEGER(SIK) :: i

TYPE(MKL_PARDISO_HANDLE):: pt(64)
INTEGER(SIK) :: n,nnz
INTEGER(SIK) :: maxfct,mnum,mtype,phase,nrhs,msglvl,error
INTEGER(SIK) :: iparm(64)
INTEGER(SIK),ALLOCATABLE :: perm(:)
! variables for matrix A
INTEGER,ALLOCATABLE :: ia(:)
INTEGER,ALLOCATABLE :: ja(:)
REAL(SRK),ALLOCATABLE :: a(:)
! variables for solution (x) and source (b) vectors
REAL(SRK),ALLOCATABLE :: x(:)
REAL(SRK),ALLOCATABLE :: b(:)
! dummy variables for releasing memory
INTEGER(SIK) :: idum(1)
REAL(SRK) :: ddum(1)

DO i=1,64
  iparm(i)=0
  pt(i)%dummy=0
ENDDO

iparm(1)=1   ! no solver default
iparm(2)=2   ! fill-in reordering from METIS
iparm(3)=1   ! number of threads
iparm(4)=0   ! no iterative-direct algorithm
iparm(5)=0   ! no user fill-in reducing permutation
iparm(6)=0   ! =0 solution on the first n compoments of x
iparm(8)=9   ! numbers of iterative refinement steps
iparm(10)=13 ! perturbe the pivot elements with 1E-13
iparm(11)=1  ! use nonsymmetric permutation and scaling MPS
iparm(13)=0  ! maximum weighted matching algorithm is switched-off (default for symmetric). Try iparm(13) = 1 in case of inappropriate accuracy
iparm(14)=0  ! Output: number of perturbed pivots
iparm(18)=-1 ! Output: number of nonzeros in the factor LU
iparm(19)=-1 ! Output: Mflops for LU factorization
iparm(20)=0  ! Output: Numbers of CG Iterations

n=8          ! A is an nxn matrix
nnz=18       ! number of non-zero elements in A
nrhs=1       ! only one system, so only one right hand side
maxfct=1
mnum=1
mtype=-2     ! symmetric, indefinite
phase=13
msglvl=0     ! no output
error=0      ! initialize error value

! allocate vectors for matrix A
ALLOCATE(ia(n+1))
ALLOCATE(ja(nnz))
ALLOCATE(a(nnz))

! fill vectors for matrix A
ia = (/ 1, 5, 8, 10, 12, 15, 17, 18, 19 /)
ja = (/ 1,    3,       6, 7,    &
           2, 3,    5,          &
              3,             8, &
                 4,       7,    &
                    5, 6, 7,    &
                       6,    8, &
                          7,    &
                             8 /)
a = (/ 7.0_SRK,          1.0_SRK,                   2.0_SRK, 7.0_SRK,          &
               -4.0_SRK, 8.0_SRK,          2.0_SRK,                            &
                         1.0_SRK,                                     5.0_SRK, &
                                  7.0_SRK,                   9.0_SRK,          &
                                           5.0_SRK, 1.0_SRK, 5.0_SRK,          &
                                                   -1.0_SRK,          5.0_SRK, &
                                                            11.0_SRK,          &
                                                                      5.0_SRK /)

! allocate vectors for x and b
ALLOCATE(b(n))
ALLOCATE(x(n))
b=1.0_SRK

CALL PARDISOINIT(pt,mtype,iparm)

! allocate and initialize perm
ALLOCATE(perm(n))
perm=1

CALL PARDISO(pt,maxfct,mnum,mtype,phase,n,a,ia,ja,perm,nrhs,iparm,msglvl,b,x,error)
IF (error /= 0) WRITE(*,*) 'CALL PARDISO() -solve FAILED!'

! test solution
!WRITE(*,*) x

! terminate and release memory
phase=-1
CALL PARDISO(pt,maxfct,mnum,mtype,phase,n,a,ia,ja,perm,nrhs,iparm,msglvl,b,x,error)
IF (error /= 0) WRITE(*,*) 'CALL PARDISO() -release_memory FAILED!'

WRITE(*,*) '  Passed: CALL PARDISO()'

#endif
ENDSUBROUTINE testPARDISO

ENDPROGRAM