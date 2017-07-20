!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
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
  USE ISO_C_BINDING
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

#ifdef FUTILITY_HAVE_PETSC
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
  PUBLIC :: ODESolverType_Sundials
  PUBLIC :: SUNDIALS_ODE_INTERFACE
  PUBLIC :: SUNDIALS_y
  PUBLIC :: SUNDIALS_ydot

  !> set enumeration scheme for TPLs
  INTEGER(SIK),PARAMETER,PUBLIC :: ODE_NATIVE=1,ODE_SUNDIALS=2
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

  CLASS(ODESolverInterface_Base),POINTER :: SUNDIALS_ODE_INTERFACE => NULL()
  TYPE(RealVectorType),SAVE :: SUNDIALS_y
  TYPE(RealVectorType),SAVE :: SUNDIALS_ydot
  LOGICAL(SLK),SAVE :: SUNDIALS_isInit=.FALSE.
  INTEGER(SIK),SAVE :: SUNDIALS_N=0
  INTEGER(C_LONG),SAVE :: SUNDIALS_iout(25)
  INTEGER(C_LONG),SAVE :: SUNDIALS_ipar(1)
  REAL(C_DOUBLE),SAVE :: SUNDIALS_rout(10)
  REAL(C_DOUBLE),SAVE :: SUNDIALS_rpar(1)

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
      CLASS(VectorType),INTENT(INOUT) :: y0
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

  !> @brief the sundials ode solver type
  TYPE,EXTENDS(ODESolverType_Base) :: ODESolverType_Sundials
    !> order of BDF method
    INTEGER(SIK) :: BDForder=5
    !> temporary allocatable array
    REAL(C_DOUBLE),ALLOCATABLE :: ytmp(:)
  !
  !List of Type Bound Procedures
    CONTAINS
      !> @copybrief ODESolverTypes::init_ODESolverType_Sundials
      !> @copydetails ODESolverTypes::init_ODESolverType_Sundials
      PROCEDURE,PASS :: init => init_ODESolverType_Sundials
      !> @copybrief ODESolverTypes::clear_ODESolverType_Sundials
      !> @copydetails ODESolverTypes::clear_ODESolverType_Sundials
      PROCEDURE,PASS :: clear => clear_ODESolverType_Sundials
      !> @copybrief ODESolverTypes::step_ODESolverType_Sundials
      !> @copydetails ODESolverTypes::step_ODESolverType_Sundials
      PROCEDURE,PASS :: step => step_ODESolverType_Sundials
  ENDTYPE ODESolverType_Sundials

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
!> @param f ODE Solver Abstract Interface
!>
!> This routine initializes the data spaces for the Sundials ODE solver interface.
!>
    SUBROUTINE init_ODESolverType_Sundials(solver,Params,f)
      CHARACTER(LEN=*),PARAMETER :: myName='init_ODESolverType_Sundials'
      CLASS(ODESolverType_Sundials),INTENT(INOUT) :: solver
      TYPE(ParamType),INTENT(IN) :: Params
      CLASS(ODESolverInterface_Base),POINTER :: f

      INTEGER(SIK) :: n, solvetype, bdf_order
      REAL(SRK) :: tol
#ifdef FUTILITY_HAVE_SUNDIALS
      INTEGER(SIK) :: ierr
      TYPE(ParamType) :: pList
#endif

      CALL Params%get('ODESolverType->n',n)
      CALL Params%get('ODESolverType->solver',solvetype)
      CALL Params%get('ODESolverType->tolerance',tol)

      IF(.NOT. solver%isInit) THEN
        IF(n < 1) THEN
          CALL eODESolverType%raiseError('Incorrect input to '// &
            modName//'::'//myName//' - Number of values (n) must be '// &
              'greater than 0!')
        ELSEIF(SUNDIALS_N > 0 .AND. SUNDIALS_N /= n) THEN
          CALL eODESolverType%raiseError('Unable to initialize '// &
            modName//'::'//myName//' - Another ODE SUNDIALS ODE Solver has'// &
              'already been created with a different n.')
        ELSE
          solver%n=n
          SUNDIALS_N=n
        ENDIF

        IF(solvetype == THETA_METHOD) THEN
              CALL eODESolverType%raiseError('Incorrect input to '// &
                modName//'::'//myName//' - Theta method is not supported with Sundials')
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

        solver%TPLType=ODE_SUNDIALS

        ALLOCATE(solver%ytmp(solver%n))
#ifdef FUTILITY_HAVE_SUNDIALS
        IF(.NOT. SUNDIALS_isInit) THEN
          CALL FNVINITS(1, INT(solver%n,C_LONG), ierr)
          ! IOUT, ROUT are likely unused, IPAR(1) should be n, RPAR is unused
          SUNDIALS_ipar(1)=solver%n
          SUNDIALS_rpar(1)=0.0_SRK
          !Calling malloc and assuming t=0 for in init.  This way we can call init then clear without a segfault
          CALL FCVMALLOC(0.0_SRK,solver%ytmp, 2, 2, 1, solver%tol, 1.0E-12_C_DOUBLE,SUNDIALS_IOUT, SUNDIALS_ROUT, &
                      SUNDIALS_IPAR, SUNDIALS_RPAR, ierr)
          CALL FCVDENSE(INT(solver%n,C_LONG),ierr)
          SUNDIALS_isInit=.TRUE.
        ENDIF
        solver%f=>f
        SUNDIALS_ODE_INTERFACE=>f
        CALL plist%clear()
        CALL plist%add('VectorType -> n',solver%n)
        IF(.NOT. SUNDIALS_y%isInit) CALL SUNDIALS_y%init(plist)
        IF(.NOT. SUNDIALS_ydot%isInit) CALL SUNDIALS_ydot%init(plist)
        CALL plist%clear()
        solver%isInit=.TRUE.
#else
        CALL eODESolverType%raiseError('Error in '// &
          modName//'::'//myName//' - Sundials interface is not available')
#endif
      ELSE
        CALL eODESolverType%raiseError('Incorrect call to '// &
          modName//'::'//myName//' - ODESolverType already initialized')
      ENDIF
    ENDSUBROUTINE init_ODESolverType_Sundials
!
!-------------------------------------------------------------------------------
!> @brief Clears the Sundials ODE Solver Type
!> @param solver The ode solver to act on
!>
!> This routine clears the data spaces
!>
    SUBROUTINE clear_ODESolverType_Sundials(solver)
      CLASS(ODESolverType_Sundials),INTENT(INOUT) :: solver

      solver%solverMethod=-1
      solver%TPLType=-1
      solver%n=-1
      solver%tol=1.0e-8_SRK
      solver%BDForder=5
      IF(ALLOCATED(solver%ytmp)) DEALLOCATE(solver%ytmp)
      IF(SUNDIALS_y%isInit) CALL SUNDIALS_y%clear()
      IF(SUNDIALS_ydot%isInit)CALL SUNDIALS_ydot%clear()
#ifdef FUTILITY_HAVE_SUNDIALS
      !If sundials FNVINITS and FCVMALLOC isn't called, FCVFEE segfaults
      IF(SUNDIALS_isInit) CALL FCVFREE()
#endif
      SUNDIALS_isInit=.FALSE.
      SUNDIALS_ODE_INTERFACE=>NULL()
      SUNDIALS_N=0

      solver%isInit=.FALSE.
    ENDSUBROUTINE clear_ODESolverType_Sundials
!
!-------------------------------------------------------------------------------
!> @brief Performs time step for the Sundials ODE Solver Type
!> @param solver The ode solver to act on
!> @param t0 The initial time of the solve
!> @param y0 The initial condition y(t0)
!> @param tf The final time of the solve
!> @param yf The final value of y(tf)
!>
!> This routine performs a solve using the sundials solver
!>(
    SUBROUTINE step_ODESolverType_Sundials(solver,t0,y0,tf,yf)
      CLASS(ODESolverType_Sundials),INTENT(INOUT) :: solver
      REAL(SRK),INTENT(IN) :: t0
      CLASS(VectorType),INTENT(INOUT) :: y0
      REAL(SRK),INTENT(IN) :: tf
      CLASS(VectorType),INTENT(INOUT) :: yf
#ifdef FUTILITY_HAVE_SUNDIALS
      INTEGER(SIK) :: ierr
      REAL(SRK) :: ttmp
#endif

      CALL y0%get(solver%ytmp)
#ifdef FUTILITY_HAVE_SUNDIALS

      !pull data out of y0 into y
      CALL FCVREINIT(t0,solver%ytmp, 1, solver%tol, 1.0E-12_C_DOUBLE, ierr)

      SUNDIALS_ODE_INTERFACE=>solver%f
      !put data in U into yf
      CALL FCVODE(REAL(TF,C_DOUBLE),REAL(ttmp,C_DOUBLE),solver%ytmp,INT(1,C_INT), ierr)
      CALL yf%set(solver%ytmp)
#endif
    ENDSUBROUTINE step_ODESolverType_Sundials
!
!-------------------------------------------------------------------------------
!> @brief Initializes the ODE Solver Type with a parameter list
!>
!> @param solver The ode solver to act on
!> @param Params A parameter list with input options
!> @param f ODE Solver Abstract Interface
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
      CLASS(VectorType),INTENT(INOUT) :: y0
      REAL(SRK),INTENT(IN) :: tf
      CLASS(VectorType),INTENT(INOUT) :: yf

      INTEGER(SIK) :: i,j,nstep,ist,N,ntmp
      REAL(SRK) :: t,dt,beta,m
      CLASS(VectorType),ALLOCATABLE :: ydot
      CLASS(VectorType),ALLOCATABLE :: rhs
      CLASS(VectorType),ALLOCATABLE :: bdf_hist(:,:)

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
      ELSEIF(solver%solverMethod==THETA_METHOD) THEN
        ALLOCATE(rhs,SOURCE=y0)
        DO i=1,nstep
          CALL solver%f%eval(t,yf,ydot)
          !setup RHS
          CALL BLAS_copy(yf,rhs)
          CALL BLAS_axpy(ydot,rhs,dt*(1.0_SRK-solver%theta))
          beta=solver%theta
          CALL solve_implicit(solver%f,solver%myLS,t,dt,yf,ydot,rhs,beta,solver%tol,MOD(i-1,20)==0)
        ENDDO
        CALL rhs%clear()
        DEALLOCATE(rhs)
      ELSE
        ALLOCATE(bdf_hist(solver%BDForder,solver%BDForder),SOURCE=y0)

        ist=1
        DO i=1,solver%BDForder
          DO j=1,solver%BDForder
            CALL bdf_hist(i,j)%setAll_scalar(0.0_SRK)
          ENDDO
          CALL BLAS_copy(y0,bdf_hist(ist,i))
        ENDDO
        !Presolve
        N=10 ! note that this needs to be sufficiently big in order to the algorithm below to
        m=1.0_SRK/REAL(N,SRK)     ! work, must be BDForder-1 but 10 represents the order of magnitude
        DO i=1,solver%BDForder-1
          ist=i
          ntmp=N-i+1
          DO j=1,i
            IF(ntmp>0) THEN
              CALL solve_bdf(solver%f,solver%myLS,i,ntmp,t,dt*m**(solver%BDForder-i),yf, &
                    ydot,solver%tol,ist,bdf_hist,.TRUE.)
              CALL BLAS_copy(yf,bdf_hist(j+1,i+1))
            ENDIF
            ntmp=N
          ENDDO
        ENDDO
        ist=solver%BDForder
        CALL solve_bdf(solver%f,solver%myLS,solver%BDForder,nstep-solver%BDForder+1,t,dt, &
                    yf,ydot,solver%tol,ist,bdf_hist)
        DO i=1,solver%BDForder
          DO j=1,solver%BDForder
            CALL bdf_hist(i,j)%clear()
          ENDDO
        ENDDO
        DEALLOCATE(bdf_hist)
      ENDIF
      CALL ydot%clear()
      DEALLOCATE(ydot)
    ENDSUBROUTINE step_ODESolverType_Native
!
!-------------------------------------------------------------------------------
!> @brief BDF iteration
!>
!> @param f the function interface which the Jacobian is based
!> @param myLS the linear solver type used for the Newton iteration
!> @param ord
!> @param nstep
!> @param t current time
!> @param dt current timestep
!> @param yf the vector which contains the final solution
!> @param ydot the vector which contains the solution derivative
!> @param tol the tolerance for the 2-norm which determines how tight to converge the implicit solve
!> @param ist the start index into the history variable
!> @param bdf_hist 2D array of the historical data.  The second index corresponds to the bdf primer
!>     hierarchy and should only be accessed as bdf_hist(:,ord)
!> @param updateJ_in an optional logical to update the jacobian of the matrix
!>
!> This routine performs the BDF solve for n steps.  THis routine is seperate in support of the priming
!> methodology that was implmeneted which introduces substeps for lower order methods to generate historical
!> data needed by the higher order methods.
!>
    SUBROUTINE solve_bdf(f,myLS,ord,nstep,t,dt,yf,ydot,tol,ist,bdf_hist,updateJ_in)
      CLASS(ODESolverInterface_Base),INTENT(INOUT) :: f
      TYPE(LinearSolverType_Direct),INTENT(INOUT) :: myLS
      INTEGER(SIK),INTENT(IN) :: ord
      INTEGER(SIK),INTENT(IN) :: nstep
      REAL(SRK),INTENT(INOUT) :: t
      REAL(SRK),INTENT(IN) :: dt
      CLASS(VectorType),INTENT(INOUT) :: yf
      CLASS(VectorType),INTENT(INOUT) :: ydot
      REAL(SRK),INTENT(IN) :: tol
      INTEGER(SIK),INTENT(INOUT) :: ist
      CLASS(VectorType),INTENT(INOUT) :: bdf_hist(:,:)
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: updateJ_in

      INTEGER(SIK) :: i,j
      CLASS(VectorType),ALLOCATABLE :: rhs
      LOGICAL(SBK) :: updateJ
      ALLOCATE(rhs,SOURCE=yf)

      DO i=1,nstep
        IF(PRESENT(updateJ_in)) THEN
          updateJ=.FALSE.
          IF(i==1) updateJ=updateJ_in
        ELSE
          updateJ=(MOD(i-1,20)==0)
        ENDIF
        CALL f%eval(t,yf,ydot)
        !setup RHS
        CALL BLAS_copy(bdf_hist(ist,ord),rhs)

        CALL BLAS_scal(rhs,alpha_bdf(alpha_index(ord)))
        DO j=1,ord-1
          CALL BLAS_axpy(bdf_hist(ABS(MOD(ist-j-1+ord,ord))+1,ord), &
                          rhs,alpha_bdf(alpha_index(ord)+j))
        ENDDO
        CALL BLAS_scal(rhs,-1.0_SRK)

        CALL solve_implicit(f,myLS,t,dt,yf,ydot,rhs,beta_bdf(ord),tol,updateJ)

        !save data if needed for next iteration
        ist=MOD(ist,ord)+1
        CALL BLAS_copy(yf,bdf_hist(ist,ord))
        t=t+dt
      ENDDO
      CALL rhs%clear()
      DEALLOCATE(rhs)
    ENDSUBROUTINE solve_bdf
!
!-------------------------------------------------------------------------------
!> @brief Iterative solve to get the implict term
!>
!> @param f the function interface which the Jacobian is based
!> @param myLS the linear solver type used for the Newton iteration
!> @param t current time
!> @param dt current timestep
!> @param yf the vector which contains the final solution
!> @param ydot the vector which contains the solution derivative
!> @param rhs the vector which contains the rhs of the solution
!> @param beta the constant multipier to scale the implicit term
!> @param tol the tolerance for the 2-norm which determines how tight to converge the implicit solve
!> @param updateJ an optional logical to update the jacobian of the matrix
!>
!> This routine performs a newton iteration to converge for the implict component of the soluion.
!> This routine is used for both theta method (theta/=0) and all BDF methods
!>
    SUBROUTINE solve_implicit(f,myLS,t,dt,yf,ydot,rhs,beta,tol,updateJ)
      CLASS(ODESolverInterface_Base),INTENT(INOUT) :: f
      TYPE(LinearSolverType_Direct),INTENT(INOUT) :: myLS
      REAL(SRK),INTENT(IN) :: t
      REAL(SRK),INTENT(IN) :: dt
      CLASS(VectorType),INTENT(INOUT) :: yf
      CLASS(VectorType),INTENT(INOUT) :: ydot
      CLASS(VectorType),INTENT(IN) :: rhs
      REAL(SRK),INTENT(IN) :: beta
      REAL(SRK),INTENT(IN) :: tol
      LOGICAL(SBK),INTENT(IN) :: updateJ

      INTEGER(SIK) :: j
      REAL(SRK) :: resid

      !setup LHS matrix
      IF (updateJ) THEN
        CALL estimate_jacobian(f,t,yf,beta*dt,myLS%A)
      ENDIF

      !Initial guess is forward euler
      CALL BLAS_axpy(ydot,yf,dt)
      !solve nonlinear system
      resid=2*tol
      j=1
      DO WHILE (resid>tol)
        CALL f%eval(t+dt,yf,ydot)
        CALL BLAS_copy(rhs,myLS%b)
        CALL BLAS_axpy(yf,myLS%b,-1.0_SRK)
        CALL BLAS_axpy(ydot,myLS%b,dt*beta)
        CALL BLAS_scal(myLS%b,1.0_SRK)
        CALL myLS%solve()
        CALL BLAS_axpy(myLS%x,yf,1.0_SRK)
        resid=BLAS_nrm2(myLS%x)
        j=j+1
      ENDDO
    ENDSUBROUTINE solve_implicit
!
!-------------------------------------------------------------------------------
!> @brief Estimate the Jacobian of the system
!>
!> @param f the function interface which the Jacobian is based
!> @param t current time
!> @param y the vector to form Jacobian around
!> @param const the constant multipier in front of the Jacobian
!> @param A the matrix type in which the Jacobian is stored
!>
!> This routine approximates A = I-const*J where J is the Jacobian, df/dy
!> which is formed using a finite difference approximation.
!>
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
        CALL ytmp%setOne(i,tmp2)
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
      CALL f0%clear()
      CALL ytmp%clear()
      CALL ftmp%clear()
      DEALLOCATE(f0)
      DEALLOCATE(ytmp)
      DEALLOCATE(ftmp)
    ENDSUBROUTINE estimate_jacobian
!
ENDMODULE ODESolverTypes

!
!-------------------------------------------------------------------------------
!> @brief FCVFUN interface for FCVODE to define y'(t)=f(t,y(t))
!> @param t The current time of the solve
!> @param y The current condition y(t)
!> @param ydot The calculated derivative at time t
!> @param ipar An integer array defined above - only ipar(1) will be defined as the number of unknowns
!> @param rpar A real array defined above - unused
!> @param ierr return value back to ODE solver
!>
!> This routine performs a solve using the sundials solver
!>
SUBROUTINE FCVFUN(t,y,ydot,ipar,rpar,ierr)
  USE ISO_C_BINDING
  USE IntrType
  USE VectorTypes
  USE ODESolverTypes
  REAL(C_DOUBLE),INTENT(IN) :: t
  REAL(C_DOUBLE),INTENT(IN) :: y(*)
  REAL(C_DOUBLE),INTENT(INOUT) :: ydot(*)
  INTEGER(C_LONG),INTENT(IN) :: ipar(1)
  REAL(C_DOUBLE),INTENT(IN) :: rpar(1)
  INTEGER(C_INT),INTENT(INOUT) :: ierr

  INTEGER(SIK) :: n

  n=ipar(1)
  SUNDIALS_y%b(1:n)=y(1:n)

  CALL SUNDIALS_ODE_INTERFACE%eval(t,SUNDIALS_y,SUNDIALS_ydot)
  !convert v_ydot back to
  ydot(1:n)=SUNDIALS_ydot%b(1:n)
  ierr=0
ENDSUBROUTINE FCVFUN
