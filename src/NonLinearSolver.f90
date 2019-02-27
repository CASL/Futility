!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module provides a non-linear solver class
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE NonLinearSolverModule
USE ISO_C_BINDING
USE IntrType
USE BLAS
USE ParameterLists
USE VectorTypes
USE MatrixTypes

IMPLICIT NONE
PRIVATE

!> Non-linear solver TPL enumerations
INTEGER(SIK),PARAMETER,PUBLIC :: NLSOLVER_TPL_NATIVE=1
!> Non-linear solver method enumerations
INTEGER(SIK),PARAMETER,PUBLIC :: NLSOLVER_METHOD_BISECTION=1
INTEGER(SIK),PARAMETER,PUBLIC :: NLSOLVER_METHOD_NEWTON=2
!> Default solver tolerance (internal use only)
REAL(SRK),PARAMETER :: NLSOLVER_TOL_DEFAULT=1.0e-8_SRK

!> Abstract type for the non-linear solver interface.  Defines procedures
!> for evaluating the function and its jacobian.  The client should extend
!> this class for its specific applications.
TYPE,ABSTRACT :: NonLinearSolverInterface_Base
  PRIVATE
  CONTAINS
    PROCEDURE(nonlinearsolver_eval_absintfc),DEFERRED,PASS :: eval
    PROCEDURE(nonlinearsolver_jacobian_absintfc),DEFERRED,PASS :: jacobian
ENDTYPE NonLinearSolverInterface_Base

!>
TYPE,ABSTRACT :: NonLinearSolver_Base
  PRIVATE
  LOGICAL(SBK) :: isInit=.FALSE.
  INTEGER(SIK) :: solverMethod=-1
  INTEGER(SIK) :: TPLType=-1
  INTEGER(SIK) :: n=0
  REAL(SRK) :: tol=NLSOLVER_TOL_DEFAULT
  REAL(SRK) :: bounds(2)=(/HUGE(1.0_SRK),-HUGE(1.0_SRK)/)
  CLASS(NonLinearSolverInterface_Base),POINTER :: f => NULL()
  CONTAINS
    PROCEDURE,PASS :: init => init_NonLinearSolverBase
    PROCEDURE,PASS,NON_OVERRIDABLE :: initBase => init_NonLinearSolverBase
    PROCEDURE,PASS :: clear => clear_NonLinearSolverBase
    PROCEDURE,PASS,NON_OVERRIDABLE :: clearBase => clear_NonLinearSolverBase
    PROCEDURE(nonlinearsolver_step_absintfc),DEFERRED,PASS :: step
ENDTYPE NonLinearSolver_Base

ABSTRACT INTERFACE
  SUBROUTINE nonlinearsolver_eval_absintfc(this)
  ENDSUBROUTINE nonlinearsolver_eval_absintfc

  SUBROUTINE nonlinearsolver_step_absintfc(this)
  ENDSUBROUTINE nonlinearsolver_step_absintfc
ENDINTERFACE

TYPE,ABSTRACT :: NonLinearSolver_Native
  PRIVATE
  CONTAINS
    PROCEDURE,PASS :: init => init_NonLinearSolverNative
    PROCEDURE,PASS :: clear => clear_NonLinearSolverNative
    PROCEDURE,PASS :: step => step_NonLinearSolverNative
ENDTYPE NonLinearSolver_Native

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE init_NonLinearSolverBase(this)
  CLASS(NonLinearSolver_Base),INTENT(INOUT) :: this
  CLASS(NonLinearSolverInterface_Base),INTENT(INOUT),POINTER :: f
  TYPE(ParamType),INTENT(IN) :: Params

  REQUIRE(Params%has('NonLinearSolver -> n'))
  REQUIRE(Params%has('NonLinearSolver -> method'))
  REQUIRE(Params%has('NonLinearSolver -> tolerance'))
  REQUIRE(Params%has('NonLinearSolver -> bounds'))
  CALL Params%get('NonLinearSolver -> bounds',bounds)
  REQUIRE(SIZE(bounds) == 2)
  REQUIRE(bounds(1) < bounds(2))

  CALL Params%get('NonLinearSolver -> n',solver%n)
  CALL Params%get('NonLinearSolver -> method',this%solverMethod)
  CALL Params%get('NonLinearSolver -> tolerance',this%tol)
  this%bounds(1:2)=bounds(1:2)
  REQUIRE(solver%n > 0)
  REQUIRE(ANY(this%solverMethod == VALID_NLSOLVER_METHODS))
  REQUIRE(this%tol > ZERO)

  this%f => f
  f => NULL()

ENDSUBROUTINE init_NonLinearSolverBase
!
!-------------------------------------------------------------------------------
SUBROUTINE clear_NonLinearSolverBase(this)
  CLASS(NonLinearSolver_Base),INTENT(INOUT) :: this
ENDMODULE NonLinearSolverModule
!
!-------------------------------------------------------------------------------
SUBROUTINE init_NonLinearSolverNative(this,f,plist)
  CLASS(NonLinearSolver_Native),INTENT(INOUT) :: this
  CLASS(NonLinearSolverInterface_Base),INTENT(INOUT),POINTER :: f
  TYPE(ParamType),INTENT(IN) :: Params
  !
  TYPE(ParamType) :: linSysPlist

  CALL this%initBase(f,plist)

  IF(this%solverMethod == NLSOLVER_METHOD_BISECTION) THEN
    !Need an exception handler in here
    STOP 888
  ENDIF
  this%TPLType=NLSOLVER_TPL_NATIVE

  CALL linSysPlist%add('LinearSolverType->TPLType',NATIVE)
  CALL linSysPlist%add('LinearSolverType->solverMethod',GE)
  CALL linSysPlist%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
  CALL linSysPlist%add('LinearSolverType->numberOMP',1_SNK)
  CALL linSysPlist%add('LinearSolverType->timerName','ODEtimer')
  CALL linSysPlist%add('LinearSolverType->matType',DENSESQUARE)
  CALL linSysPlist%add('LinearSolverType->A->MatrixType->n',n)
  CALL linSysPlist%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
  CALL linSysPlist%add('LinearSolverType->x->VectorType->n',n)
  CALL linSysPlist%add('LinearSolverType->b->VectorType->n',n)
  CALL this%linSys%init(linSysPlist)
  CALL linSysPlist%clear()

ENDSUBROUTINE init_NonLinearSolverNative
!
!-------------------------------------------------------------------------------
SUBROUTINE clear_NonLinearSolverNative(this)
  CLASS(NonLinearSolver_Native),INTENT(INOUT) :: this
ENDSUBROUTINE clear_NonLinearSolverNative
!
!-------------------------------------------------------------------------------
SUBROUTINE step_NonLinearSolverNative(this,x)
  CLASS(NonLinearSolver_Native),INTENT(IN) :: this
  CLASS(VectorType),INTENT(INOUT) :: x
  !
  CLASS(RealVectorType),ALLOCATABLE :: y,delx

  IF(this%solverMethod == NLSOLVER_METHOD_NEWTON) THEN
    ALLOCATE(delx,y,SOURCE=x)
    y = this%f%eval(x) !Get the solution at the intial guess
    !Solve until all values are under the tolerance
    !  This solves the linear system Jacobian * (x_n+1 - x_n) = -F(x_n)
    !  for the (x_n+1 - x_n) term, then adds that term to x_n to obtain
    !  x_n+1 and update the function evaluation
    DO WHILE(ALL(y < this%tol))
      this%linSys%b = y !Set the RHS to the initial value
      CALL this%f%jacobian(x,this%linSys%A) !Approximate jacobian and store in linSys LHS
      CALL this%linSys%solve() !Solve the linSys
      delx = this%linSys%x !Retrieve the linSys solution
      x = x + delx !Add the linSys solution to x0 to get the new position
      y = this%f%eval(x) !Update the solution
    ENDDO
  ENDIF

ENDSUBROUTINE step_NonLinearSolverNative
!
ENDMODULE NonLinearSolverModule