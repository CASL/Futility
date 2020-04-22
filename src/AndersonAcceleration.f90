!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module that encompasses the native implementation of
!>        Anderson Acceleration in Futility.
MODULE AndersonAccelerationTypes
USE IntrType
USE BLAS
USE Times
USE ExceptionHandler
USE ParameterLists
USE ParallelEnv
USE VectorTypes

IMPLICIT NONE

PRIVATE

!List of public members
PUBLIC :: eAndersonAccelerationType
PUBLIC :: AndersonAccelerationType

!> @brief the native Anderson Accelerator Type
TYPE :: AndersonAccelerationType
  !> Initialization status
  LOGICAL(SBK) :: isInit=.FALSE.
  !> Pointer to the distributed memory parallel environment
  TYPE(MPI_EnvType),POINTER :: MPIparallelEnv => NULL()
  !> Size of solution vector/matrix
  INTEGER(SIK) :: N=-1
  !> Current iteration count
  INTEGER(SIK) :: s=0
  !> Iteration depth of anderson solver
  INTEGER(SIK) :: depth=-1
  !> Starting iteration for Anderson
  INTEGER(SIK) :: start=1
  !> Value of mixing parameter
  REAL(SRK) :: beta=0.0_SRK
  !> Initial iterates
  REAL(SRK),ALLOCATABLE :: x(:,:)
  !> Gx vectors:
  REAL(SRK),ALLOCATABLE :: Gx(:,:)
  !> Difference vectors r=Gx-x
  REAL(SRK),ALLOCATABLE :: r(:,:)
  !> Anderson coefficient matrix
  REAL(SRK),ALLOCATABLE :: A(:,:)
  !> Anderson coefficients
  REAL(SRK),ALLOCATABLE :: alpha(:)
  !> Anderson right hand side
  REAL(SRK),ALLOCATABLE :: b(:)
  !> Timer to measure solution time
  TYPE(TimerType) :: SolveTime
!
!List of Type Bound Procedures
  CONTAINS
    !> @copybrief AndersonAccelerationType::init_AndersonAccelerationType
    !> @copydetails AndersonAccelerationType::init_AndersonAccelerationType
    PROCEDURE,PASS :: init => init_AndersonAccelerationType
    !> @copybrief AndersonAccelerationType::clear_AndersonAccelerationType
    !> @copydetails AndersonAccelerationType::clear_AndersonAccelerationType
    PROCEDURE,PASS :: clear => clear_AndersonAccelerationType
    !> @copybrief AndersonAccelerationType::step_AndersonAccelerationType
    !> @copydetails NativesAndersonAccelerationType::step_AndersonAccelerationType
    PROCEDURE,PASS :: step => step_AndersonAccelerationType
    !> @copybrief AndersonAccelerationType::set_AndersonAccelerationType
    !> @copydetails AndersonAccelerationType::set_AndersonAccelerationType
    PROCEDURE,PASS :: set => set_AndersonAccelerationType
ENDTYPE AndersonAccelerationType

!> Exception Handler for use in Anderson Acceleration
TYPE(ExceptionHandlerType),SAVE :: eAndersonAccelerationType

!> Name of module
CHARACTER(LEN=*),PARAMETER :: modName='AndersonAccelerationTypes'

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Anderson Acceleration Type with a parameter list
!> @param pList the parameter list
!> @param solver The anderson acceleration solver to act on
!> @param MPIEnv The MPI environment description
!> @param Params The anderson options parameter list
!>
SUBROUTINE init_AndersonAccelerationType(solver,MPIEnv,Params)
  CHARACTER(LEN=*),PARAMETER :: myName='init_AndersonAccelerationType'
  CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver
  TYPE(MPI_EnvType),INTENT(IN),TARGET :: MPIEnv
  TYPE(ParamType),INTENT(IN) :: Params

  INTEGER(SIK) :: i,M

  !Check to ensure parallel environment is initialized
  IF(.NOT. MPIEnv%isInit()) THEN
    CALL eAndersonAccelerationType%raiseError('Incorrect input to '// &
        modName//'::'//myName//' - MPI Environment is not initialized!')
  ELSE
    solver%MPIparallelEnv => MPIEnv
  ENDIF

  !Pull Data from Parameter List
  CALL Params%get('AndersonAccelerationType->N',solver%N)
  CALL Params%get('AndersonAccelerationType->depth',solver%depth)
  CALL Params%get('AndersonAccelerationType->beta',solver%beta)
  CALL Params%get('AndersonAccelerationType->start',solver%start)

  IF(.NOT. solver%isInit) THEN
    IF(solver%N < 1) THEN
      CALL eAndersonAccelerationType%raiseError('Incorrect input to '//modName// &
          '::'//myName//' - Number of unkowns (N) must be greater than 0!')
    ENDIF

    !Allocate member arrays
    M=solver%depth+1
    ALLOCATE(solver%x(solver%N,M),solver%Gx(solver%N,M),solver%r(solver%N,M), &
        solver%A(solver%depth,solver%depth),solver%alpha(M),solver%b(solver%depth))
    solver%x(:,:)=0.0_SRK
    solver%Gx(:,:)=0.0_SRK
    solver%r(:,:)=0.0_SRK
    solver%A(:,:)=0.0_SRK
    DO i=1,solver%depth
      solver%A(i,i)=1.0_SRK
    ENDDO
    solver%alpha(:)=0.0_SRK
    solver%b(:)=0.0_SRK
    solver%s=0
    solver%isInit=.TRUE.
  ELSE
    CALL eAndersonAccelerationType%raiseError('Incorrect call to '//modName// &
        '::'//myName//' - TrilinosAndersonAccelerationType already initialized')
  ENDIF

ENDSUBROUTINE init_AndersonAccelerationType
!
!-------------------------------------------------------------------------------
!> @brief Clears the Anderson Acceleration Type
!> @param solver The solver to act on
!>
SUBROUTINE clear_AndersonAccelerationType(solver)
  CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver

  IF(solver%isInit) THEN
    NULLIFY(solver%MPIparallelEnv)
    solver%s=0
    solver%N=-1
    solver%depth=-1
    solver%start=1
    solver%depth=-1
    solver%beta=0.0_SRK
    DEALLOCATE(solver%x)
    DEALLOCATE(solver%Gx)
    DEALLOCATE(solver%r)
    DEALLOCATE(solver%A)
    DEALLOCATE(solver%alpha)
    DEALLOCATE(solver%b)
    solver%isInit=.FALSE.
  ENDIF

ENDSUBROUTINE clear_AndersonAccelerationType
!
!-------------------------------------------------------------------------------
!> @brief Single step of Anderson acceleration acting on solution vectors
!> @param solver Anderson solver to take step with
!> @param New_Gx new solution iterate
!>
!> This routine takes a single anderson acceleration step
!>
FUNCTION step_AndersonAccelerationType(solver,New_Gx) RESULT(x_tmp)
  CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver
  REAL(SRK),INTENT(IN) :: New_Gx(:)

  INTEGER(SIK) :: i,j,k,depth_s
  REAL(SRK) :: x_tmp(solver%N)

  !Update iteration counter
  solver%s=solver%s+1

  IF(solver%s >= solver%start) THEN
    depth_s=MIN(solver%depth,solver%s-solver%start)
    !Push back vectors to make room for new ones
    DO i=depth_s,1,-1
      solver%Gx(:,i+1)=solver%Gx(:,i)
      solver%r(:,i+1)=solver%r(:,i)
    ENDDO

    !Set new vectors based on input
    solver%Gx(:,1)=New_Gx(:)
    solver%r(:,1)=solver%Gx(:,1)-solver%x(:,1)

    !Get fit coefficients
    IF(depth_s == 1) THEN
      solver%A(1,1)=0.0_SRK
      solver%b(1)=0.0_SRK
      DO j=1,solver%N
        solver%A(1,1)=solver%A(1,1)+(solver%r(j,2)-solver%r(j,1))**2_SIK
        solver%b(1)=solver%b(1)+solver%r(j,2)*(solver%r(j,2)-solver%r(j,1))
      ENDDO
      solver%alpha(1)=solver%b(1)/solver%A(1,1)
    ELSEIF(depth_s > 1) THEN
      !Construct coefficient matrix, right hand side, and solve for fit coefficients
      DO i=1,depth_s
        solver%b(i)=0.0_SRK
        DO j=1,solver%N
          solver%b(i)=solver%b(i)+solver%r(j,depth_s+1)*(solver%r(j,depth_s+1)-solver%r(j,i))
        ENDDO
        DO k=1,i
          solver%A(k,i)=0.0_SRK
          DO j=1,solver%N
            solver%A(k,i)=solver%A(k,i)+(solver%r(j,depth_s+1)-solver%r(j,k))* &
                                        (solver%r(j,depth_s+1)-solver%r(j,i))
          ENDDO
          IF(i /= k)solver%A(i,k)=solver%A(k,i)
        ENDDO
      ENDDO
      solver%alpha(1:depth_s)=GECP(solver%A(1:depth_s,1:depth_s),solver%b(1:depth_s))
    ENDIF
    !Back out "0th" alpha
    solver%alpha(depth_s+1)=0.0_SRK
    DO i=1,depth_s
      solver%alpha(depth_s+1)=solver%alpha(depth_s+1)+solver%alpha(i)
    ENDDO
    solver%alpha(depth_s+1)=1.0_SRK-solver%alpha(depth_s+1)

    !Get accelerated solution
    x_tmp(:)=0.0_SRK
    DO i=1,depth_s+1
      DO j=1,solver%N
        x_tmp(j)=x_tmp(j)+solver%alpha(i)*(solver%x(j,i)+solver%beta*solver%r(j,i))
      ENDDO
    ENDDO

    !Push back solution vectors and load in newest accelerated as next initial iterate
    DO i=MIN(depth_s+1,solver%depth),1,-1
      solver%x(:,i+1)=solver%x(:,i)
    ENDDO
    solver%x(:,1)=x_tmp(:)
  ENDIF

ENDFUNCTION step_AndersonAccelerationType
!
!-------------------------------------------------------------------------------
!> @brief Set the initial iterate for the Anderson acceleration solver
!> @param solver The anderson solver to act on
!> @param x initial iterate
!>
SUBROUTINE set_AndersonAccelerationType(solver,x)
  CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver
  REAL(SRK),INTENT(IN) :: x(:)

  solver%x(:,1)=x(:)

ENDSUBROUTINE set_AndersonAccelerationType
!
!-------------------------------------------------------------------------------
!> @brief Function which solves the system Ax=b by Gaussian elimination with
!>        full(complete) pivoting. NOTE that A is destroyed in the process.
!> @param A coefficient matrix in Ax=b
!> @param b right hand side of problem Ax=b
!> @returns x the solution vector of the problem Ax=b
!
FUNCTION GECP(A,b) RESULT(x)
  REAL(SRK),INTENT(INOUT) :: A(:,:)
  REAL(SRK),INTENT(INOUT) :: b(:)

  REAL(SRK) :: x(SIZE(b)),z(SIZE(b)),absmax,temp
  INTEGER(SIK)::i,j,k,l,N,k_max,l_max,P(SIZE(b))
  N=SIZE(b)

  !Begin LU factorization using A as a frame:
  DO j=1,N
    !Find possible swap element:
    absmax=A(j,j)
    k_max=j
    l_max=j
    DO l=j,N
      DO k=j,N
        temp=DABS(A(k,l))
        IF(temp > absmax)THEN
          absmax=temp
          k_max=k
          l_max=l
        ENDIF
      ENDDO!l
    ENDDO!k

    !Swap the row containing the maximum element to the current row if needed:
    IF(k_max /= j)THEN
      DO l=1,N
        temp=A(j,l)
        A(j,l)=A(k_max,l)
        A(k_max,l)=temp
      ENDDO!l
      !Swap entries of the b vector if needed:
      temp=b(j)
      b(j)=b(k_max)
      b(k_max)=temp
    ENDIF

    !Swap the column containing the maximum element to the current column if needed:
    IF(l_max .NE. j)THEN
      DO k=1,N
        temp=A(k,j)
        A(k,j)=A(k,l_max)
        A(k,l_max)=temp
      ENDDO!k
      !Record permutations of the A matrix and b vector:
      P(j)=l_max
    ELSE
      P(j)=j
    ENDIF

    !Record the multipliers in the stricktly unit lower triangle part of A, and perform the
    !"elimination" of all forward matrix entries below the primary diagonal:
    DO i=J+1,N
      A(i,j)=A(i,j)/A(j,j)
    ENDDO
    DO l=j+1,N
      DO i=j+1,N
        A(i,l)=A(i,l)-A(i,j)*A(j,l)
      ENDDO!l
    ENDDO!i
  ENDDO!j
  !Construction of L and U in the frame of A is complete.

  !Begin solve:
  !Forward Sweep: This solves the problem Lz=b going to z=L_inv*b:
  DO i=1,N
    z(i)=b(i)
    DO j=1,i-1
      z(i)=z(i)-A(i,j)*z(j)
    ENDDO!j
  ENDDO!i

  !Backward Sweep: This solves the problem Ux=z going to x=U_inv*z:
  DO i=N,1,-1
    x(i)=z(i)
    DO j=i+1,N
      x(i)=x(i)-A(i,j)*x(j)
    ENDDO!j
    x(i)=x(i)/A(i,i)
  ENDDO!i

  !Unswap solution vector x:
  DO i=N-1,1,-1
    temp=x(i)
    x(i)=x(P(i))
    x(P(i))=temp
  ENDDO!i

END FUNCTION GECP
!
ENDMODULE AndersonAccelerationTypes
