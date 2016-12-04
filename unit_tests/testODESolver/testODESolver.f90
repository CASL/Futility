!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!  This manuscript has been authored by UT-Battelle, LLC, under Contract       !
!  No. DE-AC0500OR22725 with the U.S. Department of Energy. The United States  !
!  Government retains and the publisher, by accepting the article for          !
!  publication, acknowledges that the United States Government retains a       !
!  non-exclusive, paid-up, irrevocable, world-wide license to publish or       !
!  reproduce the published form of this manuscript, or allow others to do so,  !
!  for the United States Government purposes.                                  !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
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

  IMPLICIT NONE

  TYPE(ExceptionHandlerType),TARGET :: e
  TYPE(ParamType) :: pList
  CLASS(ODESolverType_Base),POINTER :: testODE

  INTERFACE
    SUBROUTINE f(t,y,ydot)
      IMPORT :: SRK,VectorType
      REAL(SRK),INTENT(IN) :: t
      CLASS(VectorType),INTENT(IN) :: y
      CLASS(VectorType),INTENT(INOUT) :: ydot
    ENDSUBROUTINE f
  ENDINTERFACE

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
  CALL pList%add('ODESolverType->n',2_SIK)
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

  REGISTER_SUBTEST('testInit_Native',testInit_Native)
  REGISTER_SUBTEST('testStep_Native',testStep_Native)
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
        ASSERT(testODE%n==2,'%n')
        ASSERT(testODE%solverMethod==BDF_METHOD,'%solverMethod')
        ASSERT(testODE%theta/=0.0_SRK,'%theta') ! this shoudln't get set since method is bdf
        ASSERT(testODE%BDForder==3,'%BDForder')
        ASSERT(testODE%tol==1.0E-5_SRK,'%tol')
        ASSERT(testODE%substep_size==0.01_SRK,'%substep_size')
        ASSERT(ASSOCIATED(testODE%f),'%f')

        testODE%isInit=.FALSE.
        testODE%BDForder=5_SIK
        CALL pList%set('ODESolverType->solver',THETA_METHOD)

        CALL testODE%init(pList,f)
        ASSERT(testODE%isInit,'%isInit')
        ASSERT(testODE%n==2,'%n')
        ASSERT(testODE%solverMethod==THETA_METHOD,'%solverMethod')
        ASSERT(testODE%theta==0.0_SRK,'%theta')
        ASSERT(testODE%BDForder/=3,'%BDForder') ! this shoudln't get set since method is theta
        ASSERT(testODE%tol==1.0E-5_SRK,'%tol')
        ASSERT(testODE%substep_size==0.01_SRK,'%substep_size')
        ASSERT(ASSOCIATED(testODE%f),'%f')
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
        ASSERT(testODE%isInit==.FALSE.,'%isInit')
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

    CALL ydot%set(0.0_SRK)
    CALL ydot%set(1,3.2_SRK)
    CALL ydot%set(3,-0.3_SRK)
  ENDSUBROUTINE f