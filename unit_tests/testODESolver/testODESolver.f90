!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!  This manuscript has been authored by UT-Battelle, LLC, under Contract       !
!  No. DE-AC0500OR22725 with the U.S. Department of Energy. The United States  !
!  Government retains and the publisher, by accepting the article for          !
!  publication, acknowledges that the United States Government retains a       !
!  non-exclusive, paid-up, irrevocable, world-wide license to publish or       !
!  reproduce the published form of this manuscript, or allow others to do so,  !
!  for the United States Government purposes.                                  !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE testODEInterface
  USE IntrType
  USE VectorTypes
  USE ODESolverTypes
  TYPE,EXTENDS(ODESolverInterface_Base) :: ODESolverInterface_Test
    CONTAINS
      !> Deferred routine for initializing the ode solver
      PROCEDURE,PASS :: eval => eval
  ENDTYPE ODESolverInterface_Test
  CONTAINS
    SUBROUTINE eval(self,t,y,ydot)
      CLASS(ODESolverInterface_Test),INTENT(INOUT) :: self
      REAL(SRK),INTENT(IN) :: t
      CLASS(VectorType),INTENT(INOUT) :: y
      CLASS(VectorType),INTENT(INOUT) :: ydot

      REAL(SRK) :: tmp(3)
      CALL ydot%set(0.0_SRK)
      CALL y%get(tmp)
      !CALL ydot%set(1,3.2_SRK*tmp(1)-2.4_SRK*tmp(2)+5.0_SRK*tmp(3))
      !CALL ydot%set(2,tmp(1)-10.0_SRK*tmp(3))
      !CALL ydot%set(3,-0.3_SRK*SUM(tmp))
      CALL ydot%set(1,3.2_SRK)
      CALL ydot%set(2,0.0_SRK)
      CALL ydot%set(3,-0.3_SRK)
    ENDSUBROUTINE eval
ENDMODULE testODEInterface

PROGRAM testODESolver
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE trilinos_interfaces
  USE ParameterLists
  USE VectorTypes
  USE MatrixTypes
  USE ODESolverTypes
  USE testODEInterface

  IMPLICIT NONE

  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(ParamType) :: pList
  CLASS(ODESolverType_Base),POINTER :: testODE
  CLASS(ODESolverInterface_Base),POINTER :: f

#ifdef MPACT_HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
  PetscErrorCode  :: ierr
  CALL PETScInitialize(PETSC_NULL_CHARACTER,ierr)
#endif
#ifdef MPACT_HAVE_Trilinos
        CALL MPACT_Trilinos_Init()
#endif

  !> set up default parameter list
  CALL pList%clear()
  CALL pList%add('ODESolverType->n',3_SIK)
  CALL pList%add('ODESolverType->solver',BDF_METHOD)
  CALL pList%add('ODESolverType->theta',0.0_SRK)
  CALL pList%add('ODESolverType->bdf_order',3_SIK)
  CALL pList%add('ODESolverType->tolerance',1.0e-5_SRK)
  CALL pList%add('ODESolverType->substep_size',0.01_SRK)

  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.FALSE.)
  CALL eParams%addSurrogate(e)
  CALL eODESolverType%addSurrogate(e)

  CREATE_TEST('Test ODE Solvers')

  ALLOCATE(ODESolverType_Native :: testODE)
  ALLOCATE(ODESolverInterface_Test:: f)

  REGISTER_SUBTEST('testInit_Native',testInit_Native)
  REGISTER_SUBTEST('testStep_Native_theta=0.0',testStep_Native)
  SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
    testODE%theta=0.5_SRK
  ENDSELECT
  REGISTER_SUBTEST('testStep_Native_theta=0.5',testStep_Native)
  SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
    testODE%theta=1.0_SRK
  ENDSELECT
  REGISTER_SUBTEST('testStep_Native_theta=1.0',testStep_Native)
  REGISTER_SUBTEST('testClear_Native',testClear_Native)
  FINALIZE_TEST()

  CALL pList%clear()

  DEALLOCATE(testODE)

#ifdef MPACT_HAVE_Trilinos
  CALL MPACT_Trilinos_Finalize()
#endif
#ifdef MPACT_HAVE_PETSC
  CALL PetscFinalize(ierr)
#endif
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInit_Native()
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        CALL testODE%init(pList,f)
        ASSERT(testODE%isInit,'%isInit')
        ASSERT(testODE%n==3,'%n')
        ASSERT(testODE%solverMethod==BDF_METHOD,'%solverMethod')
        ASSERT(testODE%theta/=0.0_SRK,'%theta') ! this shoudln't get set since method is bdf
        ASSERT(testODE%BDForder==3,'%BDForder')
        ASSERT(testODE%tol==1.0E-5_SRK,'%tol')
        ASSERT(testODE%substep_size==0.01_SRK,'%substep_size')
        ASSERT(ASSOCIATED(testODE%f),'%f')
        ASSERT(testODE%myLS%isInit,'%myLS%isInit')

        testODE%isInit=.FALSE.
        testODE%BDForder=5_SIK
        CALL testODE%myLS%clear()
        CALL pList%set('ODESolverType->solver',THETA_METHOD)

        CALL testODE%init(pList,f)
        ASSERT(testODE%isInit,'%isInit')
        ASSERT(testODE%n==3,'%n')
        ASSERT(testODE%solverMethod==THETA_METHOD,'%solverMethod')
        ASSERT(testODE%theta==0.0_SRK,'%theta')
        ASSERT(testODE%BDForder/=3,'%BDForder') ! this shoudln't get set since method is theta
        ASSERT(testODE%tol==1.0E-5_SRK,'%tol')
        ASSERT(testODE%substep_size==0.01_SRK,'%substep_size')
        ASSERT(ASSOCIATED(testODE%f),'%f')
        ASSERT(testODE%myLS%isInit,'%myLS%isInit')
      ENDSELECT

    ENDSUBROUTINE testInit_Native
!
!-------------------------------------------------------------------------------
    SUBROUTINE testStep_Native()
      REAL(SRK) :: tmpval
      TYPE(ParamType) :: tmpPL
      TYPE(RealVectorType) :: y0, yf

      CALL tmpPL%add('VectorType->n',3_SIK)
      CALL y0%init(tmpPL)
      CALL yf%init(tmpPL)

      CALL y0%set(3.0_SRK)
      CALL yf%set(0.0_SRK)

      CALL testODE%step(0.0_SRK,y0,3.5_SRK,yf)

      CALL yf%get(1,tmpval)
      ASSERT(SOFTEQ(tmpval,14.2_SRK,1.0E-12_SRK),'yf(1)')
      FINFO() tmpval
      CALL yf%get(2,tmpval)
      ASSERT(SOFTEQ(tmpval,3.00_SRK,1.0E-12_SRK),'yf(2)')
      FINFO() tmpval
      CALL yf%get(3,tmpval)
      ASSERT(SOFTEQ(tmpval,1.95_SRK,1.0E-12_SRK),'yf(3)')
      FINFO() tmpval

    ENDSUBROUTINE testStep_Native
!
!-------------------------------------------------------------------------------
    SUBROUTINE testClear_Native()
      CALL testODE%clear()
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        ASSERT(testODE%solverMethod==-1,'%solverMethod')
        ASSERT(testODE%TPLType==-1,'%TPLType')
        ASSERT(testODE%n==-1,'%n')
        ASSERT(testODE%tol==1.0e-6_SRK,'%tol')
        ASSERT(testODE%theta==0.5_SRK,'%theta')
        ASSERT(testODE%BDForder==5,'%BDForder')
        ASSERT(testODE%substep_size==0.1_SRK,'%substep_size')
        ASSERT(.NOT. testODE%myLS%isInit,'%myLS%isInit')
        ASSERT(.NOT. testODE%isInit,'%isInit')
      ENDSELECT
    ENDSUBROUTINE testClear_Native
!
!-------------------------------------------------------------------------------
!
ENDPROGRAM testODESolver

  SUBROUTINE f(t,y,ydot)
    USE IntrType
    USE VectorTypes
    REAL(SRK),INTENT(IN) :: t
    CLASS(VectorType),INTENT(IN) :: y
    CLASS(VectorType),INTENT(INOUT) :: ydot
WRITE(*,*) "in f"
WRITE(*,*) t
WRITE(*,*) y%isInit
WRITE(*,*) ydot%isInit
    CALL ydot%set(0.0_SRK)
    CALL ydot%set(1,3.2_SRK)
    CALL ydot%set(3,-0.3_SRK)
  ENDSUBROUTINE f