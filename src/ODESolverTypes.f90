!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!  This manuscript has been authored by UT-Battelle, LLC, under Contract       !
!  No. DE-AC0500OR22725 with the U.S. Department of Energy. The United States  !
!  Government retains and the publisher, by accepting the article for          !
!  publication, acknowledges that the United States Government retains a       !
!  non-exclusive, paid-up, irrevocable, world-wide license to publish or       !
!  reproduce the published form of this manuscript, or allow others to do so,  !
!  for the United States Government purposes.                                  !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module provides a eigenvalue system type and methods to solve systems
!> of equations
!>
!> Currently supported TPLs include:
!>
!> Additional TPL support is planned for:
!>  - PETSc (with interfaces to TS)
!>  - Trilinos (Rythmos)
!>  - Sundials (CVODE)
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref BLAS "BLAS": @copybrief BLAS
!>  - @ref Times "Times": @copybrief Times
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref Allocs "Allocs": @copybrief Allocs
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref ParallelEnv "ParallelEnv": @copybrief ParallelEnv
!>  - @ref VectorTypes "VectorTypes": @copybrief VectorTypes
!>  - @ref MatrixTypes "MatrixTypes": @copybrief MatrixTypes
!>  - @ref LinearSolverTypes "LinearSolverTypes": @copybrief LinearSolverTypes
!>
!> @par EXAMPLES
!> @code
!>
!> @endcode
!>
!> @author Ben Collins
!>   @date 12/02/2016
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ODESolverTypes
  USE IntrType
  USE BLAS
  !USE trilinos_interfaces
  USE Times
  USE ExceptionHandler
  USE Allocs
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes
  USE LinearSolverTypes
  USE Strings
  IMPLICIT NONE

#ifdef MPACT_HAVE_PETSC
#include <finclude/petsc.h>
#include <petscversion.h>
!petscisdef.h defines the keyword IS, and it needs to be reset
#undef IS
#endif

  PRIVATE
!
! List of public members
  PUBLIC :: eODESolverType
  PUBLIC :: ODESolverType_Base
  PUBLIC :: ODESolverType_Native

  !> set enumeration scheme for TPLs
  INTEGER(SIK),PARAMETER,PUBLIC :: ODE_NATIVE=1
  !> set enumeration scheme for solver methods
  INTEGER(SIK),PARAMETER,PUBLIC :: THETA_METHOD=0,BDF_METHOD=1

  !> @brief the base ode solver type
  TYPE,ABSTRACT :: ODESolverType_Base
    !> Initialization status
    LOGICAL(SBK) :: isInit=.FALSE.
    !> Integer flag for the solution methodology desired
    INTEGER(SIK) :: solverMethod=-1
    !> Integer flag for the solution methodology desired
    INTEGER(SIK) :: TPLType=-1
    !> size of ODE system
    INTEGER(SIK) :: n=-1
    !> acceptable tolerance in solve
    REAL(SRK) :: tol=1.0e-6_SRK
    !> Timer to measure solution time
    TYPE(TimerType) :: SolveTime
    !>
    PROCEDURE(odesolver_f_absintfc),POINTER,NOPASS :: f => NULL()
  !
  !List of Type Bound Procedures
    CONTAINS
      !> Deferred routine for initializing the ode solver
      PROCEDURE(odesolver_init_sub_absintfc),DEFERRED,PASS :: init
      !> Deferred routine for clearing the ode solver
      PROCEDURE(odesolver_sub_absintfc),DEFERRED,PASS :: clear
      !> Deferred routine for solving the ode
      PROCEDURE(odesolver_step_sub_absintfc),DEFERRED,PASS :: step
  ENDTYPE ODESolverType_Base

  !> Explicitly defines the interface for the clear and step routines
  ABSTRACT INTERFACE
      SUBROUTINE odesolver_f_absintfc(t,y,ydot)
        IMPORT :: SRK,VectorType
        REAL(SRK),INTENT(IN) :: t
        CLASS(VectorType),INTENT(IN) :: y
        CLASS(VectorType),INTENT(INOUT) :: ydot
    ENDSUBROUTINE odesolver_f_absintfc

    SUBROUTINE odesolver_init_sub_absintfc(solver,Params,f)
      IMPORT :: ODESolverType_Base, ParamType
      CLASS(ODESolverType_Base),INTENT(INOUT) :: solver
      TYPE(ParamType),INTENT(IN) :: Params
      PROCEDURE(odesolver_f_absintfc) :: f
    ENDSUBROUTINE odesolver_init_sub_absintfc

    SUBROUTINE odesolver_step_sub_absintfc(solver,t0,y0,tf,yf)
      IMPORT :: ODESolverType_Base, VectorType, SRK
      CLASS(ODESolverType_Base),INTENT(INOUT) :: solver
      REAL(SRK),INTENT(IN) :: t0
      CLASS(VectorType),INTENT(IN) :: y0
      REAL(SRK),INTENT(IN) :: tf
      CLASS(VectorType),INTENT(INOUT) :: yf
    ENDSUBROUTINE odesolver_step_sub_absintfc

    SUBROUTINE odesolver_sub_absintfc(solver)
      IMPORT :: ODESolverType_Base
      CLASS(ODESolverType_Base),INTENT(INOUT) :: solver
    ENDSUBROUTINE odesolver_sub_absintfc
  ENDINTERFACE

  !> @brief the native ode solver type
  TYPE,EXTENDS(ODESolverType_Base) :: ODESolverType_Native
    !> value of theta
    REAL(SRK) :: theta=0.5_SRK
    !> order of BDF method
    INTEGER(SIK) :: BDForder=5
    !> size of substep
    REAL(SRK) :: substep_size=0.1_SRK
    !> Linear solver for implicit solve
    TYPE(LinearSolverType_Direct) :: myLS
  !
  !List of Type Bound Procedures
    CONTAINS
      !> @copybrief ODESolverTypes::init_ODESolverType_Native
      !> @copydetails ODESolverTypes::init_ODESolverType_Native
      PROCEDURE,PASS :: init => init_ODESolverType_Native
      !> @copybrief ODESolverTypes::clear_ODESolverType_Native
      !> @copydetails ODESolverTypes::clear_ODESolverType_Native
      PROCEDURE,PASS :: clear => clear_ODESolverType_Native
      !> @copybrief ODESolverTypes::step_ODESolverType_Native
      !> @copydetails ODESolverTypes::step_ODESolverType_Native
      PROCEDURE,PASS :: step => step_ODESolverType_Native
  ENDTYPE ODESolverType_Native

  !> Exception Handler for use in MatrixTypes
  TYPE(ExceptionHandlerType),SAVE :: eODESolverType

  !> Name of module
  CHARACTER(LEN=*),PARAMETER :: modName='ODESOLVERTYPES'

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the ODE Solver Type with a parameter list
!>
!> @param solver The ode solver to act on
!> @param Params A parameter list with input options
!>
!> This routine initializes the data spaces for the native ODE solver interface.
!>
    SUBROUTINE init_ODESolverType_Native(solver,Params,f)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ODESolverType_Native'
      CLASS(ODESolverType_Native),INTENT(INOUT) :: solver
      TYPE(ParamType),INTENT(IN) :: Params
      PROCEDURE(odesolver_f_absintfc) :: f

      INTEGER(SIK) :: n, solvetype, bdf_order
      REAL(SRK) :: theta, tol, substep

      theta=-100.0_SRK
      bdf_order=-1
      CALL Params%get('ODESolverType->n',n)
      CALL Params%get('ODESolverType->solver',solvetype)
      CALL Params%get('ODESolverType->tolerance',tol)
      CALL Params%get('ODESolverType->substep_size',substep)

      IF(.NOT. solver%isInit) THEN
        IF(n < 1) THEN
          CALL eODESolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (n) must be '// &
              'greater than 0!')
        ELSE
          solver%n=n
        ENDIF

        IF(solvetype == THETA_METHOD) THEN
          solver%solverMethod=solvetype
          IF(Params%has('ODESolverType->theta')) THEN
            CALL Params%get('ODESolverType->theta',theta)
            IF(theta<0.0_SRK .OR. theta>1.0_SRK) THEN
              CALL eODESolverType%raiseError('Incorrect input to '// &
                modName//'::'//myName//' - Theta must be between 0 and 1')
            ELSE
              solver%theta=theta
            ENDIF
          ENDIF
        ELSEIF(solvetype == BDF_METHOD) THEN
          solver%solverMethod=solvetype
          IF(Params%has('ODESolverType->bdf_order')) THEN
            CALL Params%get('ODESolverType->bdf_order',bdf_order)
            IF(bdf_order<1 .OR. bdf_order>5) THEN
              CALL eODESolverType%raiseError('Incorrect input to '// &
                modName//'::'//myName//' - BDF order must be between 1 and 5')
            ELSE
              solver%BDForder=bdf_order
            ENDIF

          ENDIF
        ELSE
          CALL eODESolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Unknown Solver Type')
        ENDIF

        IF(tol<=0.0_SRK) THEN
          CALL eODESolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Tolerance must be '// &
              'greater than 0!')
        ELSE
          solver%tol=tol
        ENDIF

        IF(substep <=0.0_SRK) THEN
          CALL eODESolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - substep size must be  '// &
              'greater than 0!')
        ELSE
          solver%substep_size=substep
        ENDIF

        ! Probably should initialize some stuff, linear solver

        solver%TPLType=ODE_NATIVE
        solver%f=>f
        solver%isInit=.TRUE.
      ELSE
        CALL eODESolverType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - ODESolverType already initialized')
      ENDIF
    ENDSUBROUTINE init_ODESolverType_Native
!
!-------------------------------------------------------------------------------
!> @brief Clears the Native ODE Solver Type
!> @param solver The ode solver to act on
!>
!> This routine clears the data spaces
!>
    SUBROUTINE clear_ODESolverType_Native(solver)
      CLASS(ODESolverType_Native),INTENT(INOUT) :: solver

      solver%solverMethod=-1
      solver%TPLType=-1
      solver%n=-1
      solver%tol=1.0e-6_SRK
      solver%theta=0.5_SRK
      solver%BDForder=5
      solver%substep_size=0.1_SRK

      !**** Need to clear linear solver
      solver%isInit=.FALSE.
    ENDSUBROUTINE clear_ODESolverType_Native
!
!-------------------------------------------------------------------------------
!> @brief Performs time step for the Native ODE Solver Type
!> @param solver The ode solver to act on
!> @param t0 The initial time of the solve
!> @param y0 The initial condition y(t0)
!> @param tf The final time of the solve
!> @param yf The final value of y(tf)
!>
!> This routine performs a solve using the native solver
!>
    SUBROUTINE step_ODESolverType_Native(solver,t0,y0,tf,yf)
      CLASS(ODESolverType_Native),INTENT(INOUT) :: solver
      REAL(SRK),INTENT(IN) :: t0
      CLASS(VectorType),INTENT(IN) :: y0
      REAL(SRK),INTENT(IN) :: tf
      CLASS(VectorType),INTENT(INOUT) :: yf

      INTEGER(SIK) :: i,nstep
      REAL(SRK) :: t,dt
      CLASS(VectorType),ALLOCATABLE :: ydot

      ALLOCATE(ydot,SOURCE=y0)
      CALL BLAS_copy(y0,yf)
      nstep=CEILING((tf-t0)/solver%substep_size)
      dt=(tf-t0)/REAL(nstep,SRK)
      t=t0

      IF(solver%solverMethod==THETA_METHOD .AND. solver%theta==0.0_SRK) THEN
        !forward Euler
        DO i=1,nstep
          t=t+dt
          CALL f(t,yf,ydot)
          CALL BLAS_axpy(ydot,yf,dt)
        ENDDO
      ELSE
        !setup RHS
        !setup LHS matrix
        !solve nonlinear system
        !save data if needed for next iteration
      ENDIF
      DEALLOCATE(ydot)
    ENDSUBROUTINE step_ODESolverType_Native
!
ENDMODULE ODESolverTypes
