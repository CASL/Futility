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
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE FutilityComputingEnvironmentModule
USE ParameterLists
USE ParallelEnv
USE VectorTypes
USE MatrixTypes
USE LinearSolverTypes

IMPLICIT NONE

PRIVATE

!List of public members
PUBLIC :: AndersonAccelerationType

!> @brief the native Anderson Accelerator Type
TYPE :: AndersonAccelerationType
  !> Initialization status
  LOGICAL(SBK) :: isInit=.FALSE.
  !> Size of solution vector/matrix
  INTEGER(SIK) :: n=-1
  !> Current iteration count
  INTEGER(SIK) :: s=0
  !> Iteration depth of Anderson solver
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
  !> Anderson coefficients
  REAL(SRK),ALLOCATABLE :: alpha(:)
  !> Linear solver
  TYPE(LinearSolverType_Direct) :: LS
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
    !> @copydetails AndersonAccelerationType::step_AndersonAccelerationType
    PROCEDURE,PASS :: step => step_AndersonAccelerationType
    !> @copybrief AndersonAccelerationType::reset_AndersonAccelerationType
    !> @copydetails AndersonAccelerationType::reset_AndersonAccelerationType
    PROCEDURE,PASS :: reset => reset_AndersonAccelerationType
ENDTYPE AndersonAccelerationType

!> Name of module
CHARACTER(LEN=*),PARAMETER :: modName='AndersonAccelerationTypes'

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Anderson Acceleration Type with an input parameter list
!> @param pList the parameter list
!> @param solver The Anderson Acceleration solver to act on
!> @param Params The Anderson options parameter list
!> @param ce The computing environment to use for the calculation
!>
SUBROUTINE init_AndersonAccelerationType(solver,ce,Params)
  CHARACTER(LEN=*),PARAMETER :: myName='init_AndersonAccelerationType'
  CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver
  TYPE(ParamType),INTENT(IN) :: Params
  TYPE(FutilityComputingEnvironment),INTENT(IN) :: ce

  TYPE(ParamType) :: LSparams
  INTEGER(SIK) :: i,j,m

  REQUIRE(Params%has('AndersonAccelerationType->n'))
  REQUIRE(Params%has('AndersonAccelerationType->depth'))
  REQUIRE(Params%has('AndersonAccelerationType->beta'))
  REQUIRE(Params%has('AndersonAccelerationType->start'))

  !Pull Data from Parameter List
  CALL Params%get('AndersonAccelerationType->n',solver%n)
  CALL Params%get('AndersonAccelerationType->depth',solver%depth)
  CALL Params%get('AndersonAccelerationType->beta',solver%beta)
  CALL Params%get('AndersonAccelerationType->start',solver%start)

  IF(.NOT. solver%isInit) THEN
    IF(solver%n < 1) CALL ce%exceptHandler%raiseError('Incorrect input to '//modName// &
        '::'//myName//' - Number of unkowns (n) must be greater than 0!')

    IF(solver%depth < 0) CALL ce%exceptHandler%raiseError('Incorrect input to '//modName// &
        '::'//myName//' - Anderson depth must be greater than or equal to 0!')

    IF(solver%start < 1) CALL ce%exceptHandler%raiseError('Incorrect input to '//modName// &
        '::'//myName//' - Anderson starting point must be greater than 1!')

    IF(solver%beta <= 0.0_SRK .OR. solver%beta > 1.0_SRK) THEN
      CALL ce%exceptHandler%raiseError('Incorrect input to '//modName// &
          '::'//myName//' - Anderson mixing parameter must be between 0.0 and 1.0!')
    ENDIF

    !Setup linear solver
    IF(solver%depth > 1) THEN
      CALL LSparams%add('LinearSolverType->TPLType',NATIVE)
      CALL LSparams%add('LinearSolverType->solverMethod',GE)
      CALL LSparams%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
      CALL LSparams%add('LinearSolverType->timerName','AndersonTimer')
      CALL LSparams%add('LinearSolverType->numberOMP',1_SNK)
      CALL LSparams%add('LinearSolverType->matType',DENSESQUARE)
      CALL LSparams%add('LinearSolverType->A->MatrixType->isSym',.TRUE.)
      CALL LSparams%add('LinearSolverType->A->MatrixType->n',solver%depth)
      CALL LSparams%add('LinearSolverType->x->VectorType->n',solver%depth)
      CALL LSparams%add('LinearSolverType->b->VectorType->n',solver%depth)
      CALL solver%LS%init(LSparams)
      DO j=1,solver%depth
        DO i=j,solver%depth
          IF(i == j) THEN
            CALL solver%LS%A%set(i,j,1.0_SRK)
          ELSE
            CALL solver%LS%A%set(i,j,0.0_SRK)
          ENDIF
        ENDDO
        CALL solver%LS%b%set(j,0.0_SRK)
      ENDDO
    ENDIF

    !Allocate member arrays
    m=solver%depth+1
    ALLOCATE(solver%x(solver%n,m),solver%Gx(solver%n,m),solver%r(solver%n,m),solver%alpha(m))
    solver%x(:,:)=0.0_SRK
    solver%Gx(:,:)=0.0_SRK
    solver%r(:,:)=0.0_SRK
    solver%alpha(:)=0.0_SRK
    solver%s=0
    solver%isInit=.TRUE.
  ELSE
    CALL ce%exceptHandler%raiseError('Incorrect call to '//modName// &
        '::'//myName//' - AndersonAccelerationType already initialized')
  ENDIF

ENDSUBROUTINE init_AndersonAccelerationType
!
!-------------------------------------------------------------------------------
!> @brief Clears the Anderson Acceleration Type
!> @param solver The Anderson solver to act on
!>
SUBROUTINE clear_AndersonAccelerationType(solver)
  CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver

  IF(solver%isInit) THEN
    solver%s=0
    solver%n=-1
    solver%depth=-1
    solver%start=0
    solver%depth=-1
    solver%beta=0.0_SRK
    DEALLOCATE(solver%x)
    DEALLOCATE(solver%Gx)
    DEALLOCATE(solver%r)
    DEALLOCATE(solver%alpha)
    CALL solver%LS%clear()
    solver%isInit=.FALSE.
  ENDIF

ENDSUBROUTINE clear_AndersonAccelerationType
!
!-------------------------------------------------------------------------------
!> @brief Performs a single step of Anderson Acceleration acting on the input solution vector.
!>        If depth is set to zero, or if the iteration count is below the Anderson starting
!>        point the behavior is to under-relax the solution using the mixing parameter (beta)
!>        as the under-relaxation factor.
!> @param solver Anderson solver to take step with
!> @param x_new New solution iterate and under-relaxed / accelerated return array
!>
SUBROUTINE step_AndersonAccelerationType(solver,x_new)
  CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver
  REAL(SRK),INTENT(INOUT) :: x_new(:)

  INTEGER(SIK) :: i,j,k,depth_s
  REAL(SRK) :: tmpA,tmpb,tmp

  REQUIRE(SIZE(x_new) == solver%n)

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
    solver%Gx(:,1)=x_new(:)
    solver%r(:,1)=solver%Gx(:,1)-solver%x(:,1)

    !Get fit coefficients
    IF(depth_s == 1) THEN
      !Depth 1 is an especially simple case, where alpha can be calculated with a simple formula
      tmpA=0.0_SRK
      tmpb=0.0_SRK
      DO j=1,solver%n
        tmp=solver%r(j,2)-solver%r(j,1)
        tmpA=tmpA+tmp*tmp
        tmpb=tmpb+solver%r(j,2)*tmp
      ENDDO
      solver%alpha(1)=tmpb/tmpA
    ELSEIF(depth_s > 1) THEN
      !Construct coefficient matrix, right hand side, and solve for fit coefficients
      DO i=1,depth_s
        DO k=1,i
          tmpA=0.0_SRK
          DO j=1,solver%n
            tmpA=tmpA+(solver%r(j,depth_s+1)-solver%r(j,k))*(solver%r(j,depth_s+1)-solver%r(j,i))
          ENDDO
          CALL solver%LS%A%set(k,i,tmpA)
        ENDDO
        tmpb=0.0_SRK
        DO j=1,solver%n
          tmpb=tmpb+solver%r(j,depth_s+1)*(solver%r(j,depth_s+1)-solver%r(j,i))
        ENDDO
        CALL solver%LS%b%set(i,tmpb)
      ENDDO
      CALL solver%LS%solve()
      DO i=1,depth_s
        CALL solver%LS%x%get(i,solver%alpha(i))
      ENDDO
    ENDIF
    !Back out "0th" alpha
    solver%alpha(depth_s+1)=1.0_SRK
    DO i=1,depth_s
      solver%alpha(depth_s+1)=solver%alpha(depth_s+1)-solver%alpha(i)
    ENDDO

    !Get accelerated solution
    x_new(:)=0.0_SRK
    DO i=1,depth_s+1
      DO j=1,solver%n
        x_new(j)=x_new(j)+solver%alpha(i)*(solver%x(j,i)+solver%beta*solver%r(j,i))
      ENDDO
    ENDDO

    !Push back solution vectors and load in newest accelerated as next initial iterate
    DO i=MIN(depth_s+1,solver%depth),1,-1
      solver%x(:,i+1)=solver%x(:,i)
    ENDDO
    solver%x(:,1)=x_new(:)
  ELSE
    !Get under-relaxed solution using mixing parameter
    DO j=1,solver%n
      x_new(j)=(1.0_SRK-solver%beta)*solver%x(j,1)+solver%beta*x_new(j)
      solver%x(j,1)=x_new(j)
    ENDDO
  ENDIF

ENDSUBROUTINE step_AndersonAccelerationType
!
!-------------------------------------------------------------------------------
!> @brief Set or reset the initial iterate and linear solver state for the Anderson solver
!> @param solver The Anderson solver to act on
!> @param x Initial iterate solve is starting from
!>
SUBROUTINE reset_AndersonAccelerationType(solver,x)
  CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver
  REAL(SRK),INTENT(IN) :: x(:)

  INTEGER(SIK) :: i,j

  REQUIRE(SIZE(x) == solver%n)

  solver%s=0_SIK
  solver%x(:,1)=x(:)
  IF(solver%LS%isinit) THEN
    DO j=1,solver%depth
      DO i=j,solver%depth
        IF(i == j) THEN
          CALL solver%LS%A%set(i,j,1.0_SRK)
        ELSE
          CALL solver%LS%A%set(i,j,0.0_SRK)
        ENDIF
      ENDDO
      CALL solver%LS%b%set(j,0.0_SRK)
    ENDDO
  ENDIF

ENDSUBROUTINE reset_AndersonAccelerationType
!
ENDMODULE AndersonAccelerationTypes