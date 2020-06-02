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
  CLASS(VectorType),ALLOCATABLE :: x(:)
  !> Gx vectors:
  CLASS(VectorType),ALLOCATABLE :: Gx(:)
  !> Difference vectors r=Gx-x
  CLASS(VectorType),ALLOCATABLE :: r(:)
  !> Intermediate calculation vector
  CLASS(VectorType),ALLOCATABLE :: tmpvec
  !> Anderson coefficients
  REAL(SRK),ALLOCATABLE :: alpha(:)
  !> Linear solver
  TYPE(LinearSolverType_Direct) :: LS
  !> Futility computing environment
  TYPE(FutilityComputingEnvironment),POINTER :: ce => NULL()
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
  TYPE(FutilityComputingEnvironment),TARGET,INTENT(IN) :: ce
  TYPE(ParamType),INTENT(IN) :: Params

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

    !Allocate/create member arrays and vectors that can be done now and associate pointers
    m=solver%depth+1
    ALLOCATE(solver%alpha(m))
    solver%alpha(:)=0.0_SRK
    solver%s=0
    solver%ce => ce
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

  INTEGER(SIK) :: i

  IF(solver%isInit) THEN
    IF(ALLOCATED(solver%x)) THEN
      DO i=1,solver%depth+1
        CALL solver%x(i)%clear()
        CALL solver%Gx(i)%clear()
        CALL solver%r(i)%clear()
      ENDDO
      CALL solver%tmpvec%clear()
      DEALLOCATE(solver%x)
      DEALLOCATE(solver%Gx)
      DEALLOCATE(solver%r)
      DEALLOCATE(solver%tmpvec)
    ENDIF
    DEALLOCATE(solver%alpha)
    CALL solver%LS%clear()
    solver%s=0
    solver%n=-1
    solver%depth=-1
    solver%start=0
    solver%depth=-1
    solver%beta=0.0_SRK
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
!> @param x_new New solution iterate and under-relaxed / accelerated return vector
!>
SUBROUTINE step_AndersonAccelerationType(solver,x_new)
  CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver
  CLASS(VectorType),INTENT(INOUT) :: x_new

  INTEGER(SIK) :: i,k,depth_s
  REAL(SRK) :: tmpA,tmpb

  REQUIRE(x_new%n == solver%n)

  !Update iteration counter
  solver%s=solver%s+1

  IF(solver%s >= solver%start) THEN
    depth_s=MIN(solver%depth,solver%s-solver%start)
    !Push back vectors to make room for new ones
    DO i=depth_s,1,-1
      CALL BLAS_copy(solver%Gx(i),solver%Gx(i+1))
      CALL BLAS_copy(solver%r(i),solver%r(i+1))
    ENDDO

    !Set new vectors based on input
    CALL BLAS_copy(x_new,solver%Gx(1))
    CALL BLAS_copy(solver%Gx(1),solver%r(1))
    CALL BLAS_axpy(solver%x(1),solver%r(1),-1.0_SRK)

    !Get fit coefficients
    IF(depth_s == 1) THEN
      !Depth 1 is an especially simple case, where alpha can be calculated with a simple formula
      CALL BLAS_copy(solver%r(2),solver%tmpvec)
      CALL BLAS_axpy(solver%r(1),solver%tmpvec,-1.0_SRK)
      tmpA=BLAS_dot(solver%tmpvec,solver%tmpvec)
      tmpb=BLAS_dot(solver%r(2),solver%tmpvec)
      solver%alpha(1)=tmpb/tmpA
    ELSEIF(depth_s > 1) THEN
      !Construct coefficient matrix, right hand side, and solve for fit coefficients
      DO i=1,depth_s
        CALL BLAS_copy(solver%r(i),x_new)
        CALL BLAS_axpy(solver%r(depth_s+1),x_new,-1.0_SRK)
        tmpb=BLAS_dot(solver%r(depth_s+1),x_new)
        CALL solver%LS%b%set(i,tmpb)
        DO k=1,i
          CALL BLAS_copy(solver%r(depth_s+1),solver%tmpvec)
          CALL BLAS_axpy(solver%r(k),solver%tmpvec,-1.0_SRK)
          tmpA=BLAS_dot(solver%tmpvec,x_new)
          CALL solver%LS%A%set(k,i,tmpA)
        ENDDO
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

    !Ensure Anderson coefficient solve did not fail due to bad input vector
    IF(ISNAN(solver%alpha(depth_s+1)) .OR. (solver%alpha(1) .APPROXEQ. 0.0_SRK)) THEN
      !Bad Anderson coefficient solve, revert to under-relaxation and reset Anderson solver
      CALL x_new%set(0.0_SRK)
      CALL BLAS_axpy(solver%Gx(1),x_new,solver%beta)
      CALL BLAS_axpy(solver%x(1),x_new,1.0_SRK-solver%beta)
      CALL solver%reset(x_new)
    ELSE
      !Get accelerated solution
      CALL x_new%set(0.0_SRK)
      DO i=1,depth_s+1
        CALL BLAS_copy(solver%x(i),solver%tmpvec)
        CALL BLAS_axpy(solver%r(i),solver%tmpvec,solver%beta)
        CALL BLAS_axpy(solver%tmpvec,x_new,solver%alpha(i))
      ENDDO

      !Push back solution vectors and load in newest accelerated as next initial iterate
      DO i=MIN(depth_s+1,solver%depth),1,-1
        CALL BLAS_copy(solver%x(i),solver%x(i+1))
      ENDDO
      CALL BLAS_copy(x_new,solver%x(1))
    ENDIF
  ELSE
    !Get under-relaxed solution using mixing parameter
    CALL BLAS_scal(x_new,solver%beta)
    CALL BLAS_axpy(solver%x(1),x_new,1.0_SRK-solver%beta)
    CALL BLAS_copy(x_new,solver%x(1))
  ENDIF

ENDSUBROUTINE step_AndersonAccelerationType
!
!-------------------------------------------------------------------------------
!> @brief Set or reset the initial iterate and linear solver state for the Anderson solver
!> @param solver The Anderson solver to act on
!> @param x Initial iterate solve is starting from
!>
SUBROUTINE reset_AndersonAccelerationType(solver,x)
  CHARACTER(LEN=*),PARAMETER :: myName='reset_AndersonAccelerationType'
  CLASS(AndersonAccelerationType),INTENT(INOUT) :: solver
  CLASS(VectorType),INTENT(INOUT) :: x

  TYPE(ParamType) :: vecparams
  INTEGER(SIK) :: i,j,m

  REQUIRE(x%n == solver%n)

  !If this is the first call to set/reset must actually create vectors of needed type
  IF(solver%s == 0) THEN
    IF(.NOT.ALLOCATED(solver%x)) THEN
      m=solver%depth+1
      CALL vecparams%add("VectorType->n",solver%n)
      SELECT TYPE(x);TYPE IS(RealVectorType)
        ALLOCATE(RealVectorType :: solver%x(m),solver%Gx(m),solver%r(m),solver%tmpvec)
!!!TODO: Uncomment this once interfaces to needed BLAS routines have been created
! #ifdef HAVE_MPI
!      TYPE IS(NativeDistributedVectorType)
!        ALLOCATE(NativeDistributedVectorType :: solver%x(m),solver%Gx(m),solver%r(m),solver%tmpvec)
!        CALL vecparams%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
!        CALL vecparams%add('VectorType->chunkSize',x%chunkSize)
! #endif
#ifdef FUTILITY_HAVE_PETSC
      TYPE IS(PETScVectorType)
        ALLOCATE(PETScVectorType :: solver%x(m),solver%Gx(m),solver%r(m),solver%tmpvec)
        CALL vecparams%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
        CALL vecparams%add('VectorType->nlocal',x%nlocal)
#endif
!!!TODO: Uncomment this once interfaces to needed BLAS routines have been created
! #ifdef FUTILITY_HAVE_Trilinos
!      TYPE IS(TrilinosVectorType)
!        ALLOCATE(TrilinosVectorType :: solver%x(m),solver%Gx(m),solver%r(m),solver%tmpvec)
!        CALL vecparams%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
!        CALL vecparams%add('VectorType->nlocal',x%nlocal)
! #endif
      CLASS DEFAULT
        CALL solver%ce%exceptHandler%raiseError('Incorrect call to '//modName// &
            '::'//myName//' - Input vector type not supported!')
      ENDSELECT
      DO i=1,m
        CALL solver%x(i)%init(vecparams)
        CALL solver%Gx(i)%init(vecparams)
        CALL solver%r(i)%init(vecparams)
      ENDDO
      CALL solver%tmpvec%init(vecparams)
    ELSE
      CALL solver%ce%exceptHandler%raiseError('Incorrect call to '//modName// &
          '::'//myName//' - At least one step required before reseting!')
    ENDIF
  ENDIF

  !Reset iteration counter
  solver%s=0_SIK

  !Grab initial iterate to initiate Anderson from
  CALL BLAS_copy(x,solver%x(1))

  !Reset Anderson coefficient matrix
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