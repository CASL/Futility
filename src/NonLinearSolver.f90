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
#include "Futility_DBC.h"
USE Futility_DBC
USE ISO_C_BINDING
USE IntrType
USE Constants_Conversion
USE BLAS
USE ParameterLists
USE ParallelEnv
USE FutilityComputingEnvironmentModule
USE VectorTypes
USE MatrixTypes
USE LinearSolverTypes

IMPLICIT NONE
PRIVATE

PUBLIC :: NonLinearSolverInterface_Base
PUBLIC :: NonLinearSolver_Base
PUBLIC :: NonLinearSolver_Native
#ifdef UNIT_TEST
PUBLIC :: testSolverMethod
PUBLIC :: testTPLType
PUBLIC :: testN
PUBLIC :: testIterations
PUBLIC :: testTol
PUBLIC :: testLinSys
PUBLIC :: testFunc
PUBLIC :: testCE
#endif

!> Non-linear solver TPL enumerations
INTEGER(SIK),PARAMETER,PUBLIC :: NLSOLVER_TPL_NATIVE=1
!> Non-linear solver method enumerations
INTEGER(SIK),PARAMETER,PUBLIC :: NLSOLVER_METHOD_BISECTION=1
INTEGER(SIK),PARAMETER,PUBLIC :: NLSOLVER_METHOD_NEWTON=2
INTEGER(SIK),PARAMETER :: VALID_NLSOLVER_METHODS(2)=(/NLSOLVER_METHOD_BISECTION, &
    NLSOLVER_METHOD_NEWTON/)

!> Abstract type for the non-linear solver interface.  Defines procedures
!> for evaluating the function and its jacobian.  The client should extend
!> this class for its specific applications.
TYPE,ABSTRACT :: NonLinearSolverInterface_Base
  PRIVATE
  CONTAINS
    !> @copybrief NonLinearSolverModule::clearInterface_Base
    !> @copydetails NonLinearSolverModule::clearInterface_Base
    PROCEDURE,PASS :: clear => clearInterface_Base
    !> @copybrief NonLinearSolverModule::nonlinearsolver_eval_absintfc
    !> @copydetails NonLinearSolverModule::nonlinearsolver_eval_absintfc
    PROCEDURE(nonlinearsolver_eval_absintfc),DEFERRED,PASS :: eval
    !> @copybrief NonLinearSolverModule::estimate_jacobian
    !> @copydetails NonLinearSolverModule::estimate_jacobian
    PROCEDURE,PASS :: jacobian => estimate_jacobian
    !> @copybrief NonLinearSolverModule::checkBounds_Base
    !> @copydetails NonLinearSolverModule::checkBounds_Base
    PROCEDURE,PASS :: checkBounds => checkBounds_Base
ENDTYPE NonLinearSolverInterface_Base

!> Abstract type for non-linear solver object.  Defines base initialization
!> and clear procedures and the interface for a solve procedure.
TYPE,ABSTRACT :: NonLinearSolver_Base
  PRIVATE
  !> Initialization status
  LOGICAL(SBK),PUBLIC :: isInit=.FALSE.
  !> Enumeration for the method this solver will use
  INTEGER(SIK) :: solverMethod=-1
  !> Enumeration for the TPL this solver will use
  INTEGER(SIK) :: TPLType=-1
  !> Enumeration for the number of unknowns in the system
  INTEGER(SIK) :: n=-1
  !> Number of iterations that were used to solve the system
  INTEGER(SIK) :: iterations=-1
  !> The maximum number of iterations allowed for the solve
  INTEGER(SIK) :: maxIterations=10000
  !> The target tolerance for the system residual, checked by the @checkBounds
  !> method on @c func
  REAL(SRK) :: tol=1.0E-5_SRK
  !> The linear system used to solve each iteration
  CLASS(LinearSolverType_Base),ALLOCATABLE :: linSys
  !> The function wrapper used to evaluate the function and jacobian, as well as
  !> check the convergence of the solution
  CLASS(NonLinearSolverInterface_Base),POINTER :: func => NULL()
  !> Pointer to the computing environment
  TYPE(FutilityComputingEnvironment),POINTER :: ce => NULL()
  CONTAINS
    !> @copybrief NonLinearSolverModule::init_NonLinearSolverBase
    !> @copydetails NonLinearSolverModule::init_NonLinearSolverBase
    PROCEDURE,PASS :: init => init_NonLinearSolverBase
    !> @copybrief NonLinearSolverModule::init_NonLinearSolverBase
    !> @copydetails NonLinearSolverModule::init_NonLinearSolverBase
    PROCEDURE,PASS,NON_OVERRIDABLE :: initBase => init_NonLinearSolverBase
    !> @copybrief NonLinearSolverModule::clear_NonLinearSolverBase
    !> @copydetails NonLinearSolverModule::clear_NonLinearSolverBase
    PROCEDURE,PASS :: clear => clear_NonLinearSolverBase
    !> @copybrief NonLinearSolverModule::clear_NonLinearSolverBase
    !> @copydetails NonLinearSolverModule::clear_NonLinearSolverBase
    PROCEDURE,PASS,NON_OVERRIDABLE :: clearBase => clear_NonLinearSolverBase
    !> @copybrief NonLinearSolverModule::nonlinearsolver_solve_absintfc
    !> @copydetails NonLinearSolverModule::nonlinearsolver_solve_absintfc
    PROCEDURE(nonlinearsolver_solve_absintfc),DEFERRED,PASS :: solve
    !> @copybrief NonLinearSolverModule::continueSolve
    !> @copydetails NonLinearSolverModule::continueSolve
    PROCEDURE,PASS,PRIVATE :: continueSolve
ENDTYPE NonLinearSolver_Base

!> Concrete type for non-linear solver object that uses native vector and
!> matrix objects and native solvers.  Defines concrete implementations
!> required to perform a solve given an abstract function interface.
TYPE,EXTENDS(NonLinearSolver_Base) :: NonLinearSolver_Native
  PRIVATE
  CONTAINS
    !> @copybrief NonLinearSolverModule::init_NonLinearSolverNative
    !> @copydetails NonLinearSolverModule::init_NonLinearSolverNative
    PROCEDURE,PASS :: init => init_NonLinearSolverNative
    !> @copybrief NonLinearSolverModule::clear_NonLinearSolverNative
    !> @copydetails NonLinearSolverModule::clear_NonLinearSolverNative
    PROCEDURE,PASS :: clear => clear_NonLinearSolverNative
    !> @copybrief NonLinearSolverModule::solve_NonLinearSolverNative
    !> @copydetails NonLinearSolverModule::solve_NonLinearSolverNative
    PROCEDURE,PASS :: solve => solve_NonLinearSolverNative
ENDTYPE NonLinearSolver_Native

ABSTRACT INTERFACE
  !> @brief Interface for the @c eval method of a @c NonLinearSolverInterface_Base
  !>        extension
  !> @param this the @c NonLinearSolverInterface_Base object
  !> @param x the @c VectorType to evaluate the function at
  !> @param y the @c VectorType containing the evaluation of the function at @c x
  !>
  SUBROUTINE nonlinearsolver_eval_absintfc(this,x,y)
    IMPORT :: NonLinearSolverInterface_Base,VectorType
    CLASS(NonLinearSolverInterface_Base),INTENT(IN) :: this
    CLASS(VectorType),INTENT(INOUT) :: x
    CLASS(VectorType),INTENT(INOUT) :: y
  ENDSUBROUTINE nonlinearsolver_eval_absintfc

  !> @brief Interface for the @c solve method of a @c NonLinearSolver extension
  !> @param this the @c NonLinearSolver object
  !> @param x the @c VectorType containing the initial guess for the solution
  !>        of the root-finding procedure
  !>
  SUBROUTINE nonlinearsolver_solve_absintfc(this,x)
    IMPORT :: NonLinearSolver_Base,VectorType
    CLASS(NonLinearSolver_Base),INTENT(INOUT) :: this
    CLASS(VectorType),INTENT(INOUT) :: x
  ENDSUBROUTINE nonlinearsolver_solve_absintfc
ENDINTERFACE

!> Module name
CHARACTER(LEN=*),PARAMETER :: modName='NonLinearSolver'

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Defines the interface for clearing a @c NonLinearSolverInterface_Base
!> @param this the object to clear
!>
SUBROUTINE clearInterface_Base(this)
  CLASS(NonLinearSolverInterface_Base),INTENT(INOUT) :: this

  !Does nothing.  This is implemented in case extensions of the interface implement
  !it so we can clear the extensions of this class when clearing the solver itself.

ENDSUBROUTINE clearInterface_Base
!
!-------------------------------------------------------------------------------
!> @brief Checks the bounds of a solution approximation
!> @param this the @c NonLinearSolverInterface_Base object
!> @param x the current solution approximation to check
!> @returns keepSolving a logical indicating whether the solve should continue
!>          or not
!>
!> This procedure defines a default behavior: nothing is checked and the solve will
!> always be told to continue.  Extensions of the @c NonLinearSolverInterface_Base
!> object can implement more advanced techniques.
!>
FUNCTION checkBounds_Base(this,x) RESULT(keepSolving)
  CLASS(NonLinearSolverInterface_Base),INTENT(INOUT) :: this
  CLASS(VectorType),INTENT(INOUT) :: x
  LOGICAL(SBK) :: keepSolving

  REQUIRE(x%isInit)

  keepSolving=.TRUE.

ENDFUNCTION checkBounds_Base
!
!-------------------------------------------------------------------------------
!> @brief Estimates the jacobian of a function
!> @param this the @c NonLinearSolverInterface_Base object
!> @param x the position at which to estimate the jacobian
!> @param J the resulting matrix containing the estimated jacobian
!>
!> This routine evaluates the function at @c x and @c x + @c perturb*x, where
!> @c perturb is a parameter set to 1.000001
!>
SUBROUTINE estimate_jacobian(this,x,J)
  CLASS(NonLinearSolverInterface_Base),INTENT(IN) :: this
  CLASS(VectorType),INTENT(INOUT) :: x
  CLASS(MatrixType),INTENT(INOUT) :: J
  !
  REAL(SRK),PARAMETER :: perturb=1.000001_SRK
  INTEGER(SIK) :: n,m
  REAL(SRK) :: x_real,delx_real,invDelx_real,y_x_real,y_delx_real
  CLASS(VectorType),ALLOCATABLE :: delx,y_x,y_delx

  REQUIRE(x%isInit)
  REQUIRE(J%isInit)
  REQUIRE(x%n > 0)
  REQUIRE(J%n == x%n)

  ALLOCATE(delx,SOURCE=x)
  ALLOCATE(y_x,SOURCE=x)
  ALLOCATE(y_delx,SOURCE=x)

  CALL this%eval(x,y_x)
  DO n=1,x%n
    !Copy the diffusion coefficients and perturb them
    CALL BLAS_copy(x,delx)
    CALL delx%get(n,x_real)
    IF(x_real .APPROXEQ. ZERO) THEN
      delx_real=ONE-perturb
    ELSE
      delx_real=x_real*perturb
    ENDIF
    CALL delx%set(n,delx_real)
    invDelx_real=ONE/(delx_real-x_real)

    !Evaluate the function and store jacobian entries
    CALL this%eval(delx,y_delx)
    DO m=1,x%n
      CALL y_x%get(m,y_x_real)
      CALL y_delx%get(m,y_delx_real)
      CALL J%set(m,n,invDelx_real*(y_delx_real-y_x_real))
    ENDDO !m
  ENDDO !n

ENDSUBROUTINE estimate_jacobian
!
!-------------------------------------------------------------------------------
!> @brief Defines the interface for initializing a @c NonLinearSolver_Base object
!> @param this the object to initialize
!> @param ce the computing environment
!> @param f the @c NonLinearSolverInterface_Base object to use for function evals
!> @param plist the parameter list to use for initialization
!>
!> This procedure takes ownership of @c f and clears the input pointer.  It does
!> not take ownership of @c ce.
!>
SUBROUTINE init_NonLinearSolverBase(this,ce,f,plist)
  CLASS(NonLinearSolver_Base),INTENT(INOUT) :: this
  TYPE(FutilityComputingEnvironment),TARGET,INTENT(IN) :: ce
  CLASS(NonLinearSolverInterface_Base),INTENT(INOUT),TARGET :: f
  TYPE(ParamType),INTENT(IN) :: plist

  REQUIRE(.NOT.this%isInit)
  REQUIRE(plist%has('NonLinearSolver -> n'))
  REQUIRE(plist%has('NonLinearSolver -> method'))

  CALL plist%get('NonLinearSolver -> n',this%n)
  REQUIRE(this%n > 0)
  CALL plist%get('NonLinearSolver -> method',this%solverMethod)
  REQUIRE(ANY(this%solverMethod == VALID_NLSOLVER_METHODS))
  IF(plist%has('NonLinearSolver -> tolerance')) THEN
    CALL plist%get('NonLinearSolver -> tolerance',this%tol)
    REQUIRE(this%tol > ZERO)
  ENDIF
  IF(plist%has('NonLinearSolver -> maxIterations')) THEN
    CALL plist%get('NonLinearSolver -> maxIterations',this%maxIterations)
    REQUIRE(this%maxIterations >= 0)
  ENDIF

  this%ce => ce
  this%func => f

ENDSUBROUTINE init_NonLinearSolverBase
!
!-------------------------------------------------------------------------------
!> @brief Defines the interface for clearing a @c NonLinearSolver_Base object
!> @param this the object to clear
!>
!> This routine clears data on the @c func component and nullifies the pointer
!>
SUBROUTINE clear_NonLinearSolverBase(this)
  CLASS(NonLinearSolver_Base),INTENT(INOUT) :: this

  this%solverMethod=-1
  this%TPLType=-1
  this%n=-1
  this%iterations=-1
  this%maxIterations=10000
  this%tol=1.0E-5_SRK
  CALL this%linSys%clear()
  DEALLOCATE(this%linSys)
  this%ce => NULL()
  IF(ASSOCIATED(this%func)) THEN
    CALL this%func%clear()
  ENDIF
  this%func => NULL()
  this%isInit=.FALSE.

ENDSUBROUTINE clear_NonLinearSolverBase
!
!-------------------------------------------------------------------------------
!> @brief Defines the interface for initializing a @c NonLinearSolver_Native object
!> @param this the object to initialize
!> @param ce the computing environment
!> @param f the @c NonLinearSolverInterface_Base object to use for function evals
!> @param plist the parameter list to use for initialization
!>
!> This procedure takes ownership of @c f and clears the input pointer.  It does
!> not take ownership of @c ce.
!>
SUBROUTINE init_NonLinearSolverNative(this,ce,f,plist)
  CLASS(NonLinearSolver_Native),INTENT(INOUT) :: this
  TYPE(FutilityComputingEnvironment),TARGET,INTENT(IN) :: ce
  CLASS(NonLinearSolverInterface_Base),INTENT(INOUT),TARGET :: f
  TYPE(ParamType),INTENT(IN) :: plist
  !
  CHARACTER(LEN=*),PARAMETER :: myName='init_NonLinearSolverNative'
  TYPE(ParamType) :: linSysPlist

  REQUIRE(.NOT.this%isInit)

  CALL this%initBase(ce,f,plist)

  IF(this%solverMethod == NLSOLVER_METHOD_BISECTION) THEN
    CALL ce%exceptHandler%raiseFatalError(modName//'::'//myName//' - Bisection method '// &
        'is not yet supported by the native NonLinearSolver object!')
  ENDIF
  this%TPLType=NLSOLVER_TPL_NATIVE

  CALL linSysPlist%add('LinearSolverType->TPLType',LINSYS_NATIVE)
  CALL linSysPlist%add('LinearSolverType->solverMethod',GE)
  CALL linSysPlist%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
  CALL linSysPlist%add('LinearSolverType->numberOMP',1_SNK)
  CALL linSysPlist%add('LinearSolverType->timerName','ODEtimer')
  CALL linSysPlist%add('LinearSolverType->matType',DENSESQUARE)
  CALL linSysPlist%add('LinearSolverType->A->MatrixType->n',this%n)
  CALL linSysPlist%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
  CALL linSysPlist%add('LinearSolverType->x->VectorType->n',this%n)
  CALL linSysPlist%add('LinearSolverType->b->VectorType->n',this%n)
  ALLOCATE(LinearSolverType_Direct :: this%linSys)
  CALL this%linSys%init(linSysPlist)
  CALL linSysPlist%clear()

  this%isInit=.TRUE.

ENDSUBROUTINE init_NonLinearSolverNative
!
!-------------------------------------------------------------------------------
!> @brief Defines the interface for clearing a @c NonLinearSolver_Native object
!> @param this the object to clear
!>
!> This routine clears data on the @c func component and then deallocates the
!> pointer
!>
SUBROUTINE clear_NonLinearSolverNative(this)
  CLASS(NonLinearSolver_Native),INTENT(INOUT) :: this

  CALL this%clearBase()

ENDSUBROUTINE clear_NonLinearSolverNative
!
!-------------------------------------------------------------------------------
!> @brief Solves a @c NonLinearSolver_Native object
!> @param this the @c NonLinearSolver_Native object to solve
!> @param x @c VectorType containing the initial guess; will be modified to store
!>        final solution
!>
SUBROUTINE solve_NonLinearSolverNative(this,x)
  CLASS(NonLinearSolver_Native),INTENT(INOUT) :: this
  CLASS(VectorType),INTENT(INOUT) :: x
  !
  LOGICAL(SBK) :: lsolve
  CLASS(VectorType),ALLOCATABLE :: y

  REQUIRE(this%isInit)
  REQUIRE(x%isInit)

  IF(this%solverMethod == NLSOLVER_METHOD_NEWTON) THEN
    !Set up the initial solve
    ALLOCATE(y,SOURCE=x)
    CALL this%func%eval(x,y) !Get the solution at the intial guess

    !Check that we actually need to do the solve (initial guess could be correct
    !or user could have set matIterations == 0)
    this%iterations=0
    lsolve=this%continueSolve(y)

    !Solve until all values are under the tolerance
    !  This solves the linear system Jacobian * (x_n+1 - x_n) = -F(x_n)
    !  for the (x_n+1 - x_n) term, then adds that term to x_n to obtain
    !  x_n+1 and update the function evaluation
    DO WHILE(lsolve)
      this%iterations=this%iterations+1
      !Set the RHS to the negative of the previous function evaluation
      CALL this%linSys%b%set(0.0_SRK)
      CALL BLAS_axpy(y,this%linSys%b,-1.0_SRK)
      !Approximate jacobian and store in linSys LHS
      CALL this%func%jacobian(x,this%linSys%A)
      !Solve the linSys
      CALL this%linSys%solve()
      !Add the linSys solution to x to get the new position
      CALL BLAS_axpy(this%linSys%x,x)

      !Enforce all parts of the solution to be in-bounds and allow the function
      !evaluator to stop the solve if certain conditions are met
      lsolve=this%func%checkBounds(x)

      !Update the solution
      CALL this%func%eval(x,y)

      !Check the stop conditions
      lsolve = lsolve .AND. this%continueSolve(y)
    ENDDO
  ENDIF

ENDSUBROUTINE solve_NonLinearSolverNative
!
!-------------------------------------------------------------------------------
!> @brief Checks stop criteria on a @c NonLinearSolver_Native object
!> @param this the @c NonLinearSolver_Native object being solved
!> @param y the current solution to check
!> @returns keepSolving logical indicating if the solver should keep solving
!>
!> If the absolute value of each element of @c y is less than the solver's
!> tolerance or if the maximum number of iterations has been reached, then
!> @c keepSolving will be set to false.  Otherwise, it will be set to true.
!>
FUNCTION continueSolve(this,y) RESULT(keepSolving)
  CLASS(NonLinearSolver_Base),INTENT(IN) :: this
  CLASS(VectorType),INTENT(INOUT) :: y
  LOGICAL(SBK) :: keepSolving
  !
  REAL(SRK),ALLOCATABLE :: vec2real(:)

  ALLOCATE(vec2real(this%n))
  CALL y%get(vec2real)
  keepSolving=.TRUE.

  !Convergence has been reached
  IF(ALL(ABS(vec2real) < this%tol)) THEN
    keepSolving=.FALSE.
  ENDIF
  !Max iterations reached
  IF(this%iterations >= this%maxIterations) THEN
    keepSolving=.FALSE.
  ENDIF

ENDFUNCTION continueSolve
!
!-------------------------------------------------------------------------------
#ifdef UNIT_TEST
!> List of test procedures to allow access to PRIVATE components of module-defined
!> extended types.
FUNCTION testSolverMethod(this) RESULT(val)
  CLASS(NonLinearSolver_Base),INTENT(IN) :: this
  INTEGER(SIK) :: val
  val=this%solverMethod
ENDFUNCTION testSolverMethod
FUNCTION testTPLType(this) RESULT(val)
  CLASS(NonLinearSolver_Base),INTENT(IN) :: this
  INTEGER(SIK) :: val
  val=this%TPLType
ENDFUNCTION testTPLType
FUNCTION testN(this) RESULT(val)
  CLASS(NonLinearSolver_Base),INTENT(IN) :: this
  INTEGER(SIK) :: val
  val=this%n
ENDFUNCTION testN
FUNCTION testIterations(this) RESULT(val)
  CLASS(NonLinearSolver_Base),INTENT(IN) :: this
  INTEGER(SIK) :: val
  val=this%iterations
ENDFUNCTION testIterations
FUNCTION testTol(this) RESULT(val)
  CLASS(NonLinearSolver_Base),INTENT(IN) :: this
  REAL(SRK) :: val
  val=this%tol
ENDFUNCTION testTol
SUBROUTINE testLinSys(this,val)
  CLASS(NonLinearSolver_Base),INTENT(IN) :: this
  CLASS(LinearSolverType_Base),ALLOCATABLE,INTENT(OUT) :: val
  IF(ALLOCATED(this%linSys)) THEN
    ALLOCATE(val,SOURCE=this%linSys)
  ENDIF
ENDSUBROUTINE testLinSys
FUNCTION testFunc(this) RESULT(val)
  CLASS(NonLinearSolver_Base),INTENT(IN) :: this
  CLASS(NonLinearSolverInterface_Base),POINTER :: val
  val => this%func
ENDFUNCTION testFunc
FUNCTION testCE(this) RESULT(val)
  CLASS(NonLinearSolver_Base),INTENT(IN) :: this
  TYPE(FutilityComputingEnvironment),POINTER :: val
  val => this%ce
ENDFUNCTION testCE
#endif
!
ENDMODULE NonLinearSolverModule
