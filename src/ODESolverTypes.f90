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
  PUBLIC :: ODESolverInterface_Base
  PUBLIC :: ODESolverType_Base
  PUBLIC :: ODESolverType_Native

  !> set enumeration scheme for TPLs
  INTEGER(SIK),PARAMETER,PUBLIC :: ODE_NATIVE=1
  !> set enumeration scheme for solver methods
  INTEGER(SIK),PARAMETER,PUBLIC :: THETA_METHOD=0,BDF_METHOD=1
  !> bdf constants for LHS
  REAL(SRK),PARAMETER :: beta_bdf(5)=(/1.0_SRK,2.0_SRK/3.0_SRK,6.0_SRK/11.0_SRK, &
    12.0_SRK/25.0_SRK,60.0_SRK/137.0_SRK/)
  !> bdf constants for RHS
  REAL(SRK),PARAMETER :: alpha_bdf(15)=(/                             -1.0_SRK,&
                                              -4.0_SRK/3.0_SRK,1.0_SRK/3.0_SRK,&
                         -18.0_SRK/11.0_SRK,9.0_SRK/11.0_SRK,-2.0_SRK/11.0_SRK,&
      -48.0_SRK/25.0_SRK,36.0_SRK/25.0_SRK,-16.0_SRK/25.0_SRK,3.0_SRK/25.0_SRK,&
    -300.0_SRK/137.0_SRK,300.0_SRK/137.0_SRK,-200.0_SRK/137.0_SRK,             &
       75.0_SRK/137.0_SRK,-12.0_SRK/137.0_SRK/)
  !> indices for RHS bdf constants
  INTEGER(SIK),PARAMETER :: alpha_index(6)=(/1,2,4,7,11,16/)

  TYPE,ABSTRACT :: ODESolverInterface_Base
    CONTAINS
      !> Deferred routine for initializing the ode solver
      PROCEDURE(odesolver_f_sub_absintfc),DEFERRED,PASS :: eval
  ENDTYPE ODESolverInterface_Base

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
    REAL(SRK) :: tol=1.0e-8_SRK
    !> Timer to measure solution time
    TYPE(TimerType) :: SolveTime
    !>
    CLASS(ODESolverInterface_Base),POINTER :: f
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
    SUBROUTINE odesolver_f_sub_absintfc(self,t,y,ydot)
      IMPORT :: ODESolverInterface_Base,SRK,VectorType
      CLASS(ODESolverInterface_Base),INTENT(INOUT) :: self
      REAL(SRK),INTENT(IN) :: t
      CLASS(VectorType),INTENT(INOUT) :: y
      CLASS(VectorType),INTENT(INOUT) :: ydot
    ENDSUBROUTINE odesolver_f_sub_absintfc

    SUBROUTINE odesolver_init_sub_absintfc(solver,Params,f)
      IMPORT :: ODESolverType_Base, ODESolverInterface_Base, ParamType
      CLASS(ODESolverType_Base),INTENT(INOUT) :: solver
      TYPE(ParamType),INTENT(IN) :: Params
      CLASS(ODESolverInterface_Base),POINTER :: f
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
      CLASS(ODESolverInterface_Base),POINTER :: f

      INTEGER(SIK) :: n, solvetype, bdf_order
      REAL(SRK) :: theta, tol, substep
      TYPE(ParamType) :: tmpPL

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

        CALL tmpPL%add('LinearSolverType->TPLType',NATIVE)
        CALL tmpPL%add('LinearSolverType->solverMethod',GE)
        CALL tmpPL%add('LinearSolverType->MPI_Comm_ID',PE_COMM_SELF)
        CALL tmpPL%add('LinearSolverType->numberOMP',1_SNK)
        CALL tmpPL%add('LinearSolverType->timerName','ODEtimer')
        CALL tmpPL%add('LinearSolverType->matType',DENSESQUARE)
        CALL tmpPL%add('LinearSolverType->A->MatrixType->n',n)
        CALL tmpPL%add('LinearSolverType->A->MatrixType->isSym',.FALSE.)
        CALL tmpPL%add('LinearSolverType->x->VectorType->n',n)
        CALL tmpPL%add('LinearSolverType->b->VectorType->n',n)
        CALL solver%myLS%init(tmpPL)

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
      solver%tol=1.0e-8_SRK
      solver%theta=0.5_SRK
      solver%BDForder=5
      solver%substep_size=0.1_SRK

      CALL solver%myLS%clear()
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

      INTEGER(SIK) :: i,j,k,nstep,ist,ord
      REAL(SRK) :: t,dt,beta,resid
      CLASS(VectorType),ALLOCATABLE :: ydot
      CLASS(VectorType),ALLOCATABLE :: rhs
      CLASS(VectorType),ALLOCATABLE :: bdf_hist(:)

      ALLOCATE(ydot,SOURCE=y0)
      CALL BLAS_copy(y0,yf)
      nstep=CEILING((tf-t0)/solver%substep_size)
      dt=(tf-t0)/REAL(nstep,SRK)
      t=t0

      IF(solver%solverMethod==THETA_METHOD .AND. solver%theta==0.0_SRK) THEN
        !forward Euler
        DO i=1,nstep
          CALL solver%f%eval(t,yf,ydot)
          CALL BLAS_axpy(ydot,yf,dt)
          t=t+dt
        ENDDO
      ELSE
        ALLOCATE(rhs,SOURCE=y0)
        IF(solver%solverMethod==BDF_METHOD) THEN
          ALLOCATE(bdf_hist(solver%BDForder),SOURCE=rhs)
          DO i=1,solver%BDForder
            CALL bdf_hist(i)%set(0.0_SRK)
          ENDDO
          ist=1
          CALL BLAS_copy(y0,bdf_hist(ist))
        ENDIF
        DO i=1,nstep
          CALL solver%f%eval(t,yf,ydot)
          IF(solver%solverMethod==THETA_METHOD) THEN
            !setup RHS
            CALL BLAS_copy(yf,rhs)
            CALL BLAS_axpy(ydot,rhs,dt*(1.0_SRK-solver%theta))
            beta=solver%theta
          ELSEIF(solver%solverMethod==BDF_METHOD) THEN
            !setup RHS
            ord=MIN(i,solver%BDForder)
            CALL BLAS_copy(bdf_hist(ist),rhs)
            CALL BLAS_scal(rhs,alpha_bdf(alpha_index(ord)))
            DO j=1,ord-1
              CALL BLAS_axpy(bdf_hist(ABS(MOD(ist-j-1+solver%BDForder,solver%BDForder))+1), &
                              rhs,alpha_bdf(alpha_index(ord)+j))
            ENDDO
            CALL BLAS_scal(rhs,-1.0_SRK)
            beta=beta_bdf(ord)
          ENDIF
          !setup LHS matrix
          IF (MOD(i-1,20)==0) THEN
            CALL estimate_jacobian(solver%f,t,yf,beta*dt,solver%myLS%A)
          ENDIF

          !Initial guess is forward euler
          CALL BLAS_axpy(ydot,yf,dt)
          !solve nonlinear system
          resid=2*solver%tol
          j=1
          DO WHILE (resid>solver%tol)
            CALL solver%f%eval(t+dt,yf,ydot)
            CALL BLAS_copy(rhs,solver%myLS%b)
            CALL BLAS_axpy(yf,solver%myLS%b,-1.0_SRK)
            CALL BLAS_axpy(ydot,solver%myLS%b,dt*beta)
            CALL BLAS_scal(solver%myLS%b,1.0_SRK)
            CALL solver%myLS%solve()
            CALL BLAS_axpy(solver%myLS%x,yf,1.0_SRK)
            resid=BLAS_nrm2(solver%myLS%x)
            j=j+1
          ENDDO
          IF(solver%solverMethod==BDF_METHOD) THEN
            !save data if needed for next iteration
            ist=MOD(ist,solver%BDForder)+1
            CALL BLAS_copy(yf,bdf_hist(ist))
          ENDIF
          t=t+dt
        ENDDO
        CALL rhs%clear()
        DEALLOCATE(rhs)
      ENDIF
      DEALLOCATE(ydot)
    ENDSUBROUTINE step_ODESolverType_Native

    SUBROUTINE estimate_jacobian(f,t,y,const,A)
      CLASS(ODESolverInterface_Base),INTENT(INOUT) :: f
      REAL(SRK),INTENT(IN) :: t
      CLASS(VectorType),INTENT(INOUT) :: y
      REAL(SRK),INTENT(IN) :: const
      CLASS(MatrixType),INTENT(INOUT) :: A

      INTEGER(SIK) :: i,j
      REAL(SRK) :: tmp,tmp2,f1,f2
      REAL(SRK) :: dy=1.000001_SRK
      REAL(SRK) :: dytol=1.0E-12_SRK
      CLASS(VectorType),ALLOCATABLE :: ytmp,f0,ftmp

      ALLOCATE(f0,SOURCE=y)
      ALLOCATE(ytmp,SOURCE=y)
      ALLOCATE(ftmp,SOURCE=y)

      CALL f%eval(t,y,f0)
      DO i=1,y%n
        CALL BLAS_copy(y,ytmp)
        CALL ytmp%get(i,tmp)
        IF(ABS(tmp)<dytol) THEN
          tmp2=SIGN(dytol,tmp)
        ELSE
          tmp2=tmp*dy
        ENDIF
        CALL ytmp%set(i,tmp2)
        CALL f%eval(t,ytmp,ftmp)
        DO j=1,y%n
          CALL f0%get(j,f1)
          CALL ftmp%get(j,f2)
          IF(i==j) THEN
            CALL A%set(i,j,1.0_SRK-const*(f2-f1)/(tmp2-tmp))
          ELSE
            CALL A%set(i,j,-const*(f2-f1)/(tmp2-tmp))
          ENDIF
        ENDDO
      ENDDO
    ENDSUBROUTINE estimate_jacobian
!
ENDMODULE ODESolverTypes
