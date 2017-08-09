!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE testODEInterface
  USE IntrType
  USE VectorTypes
  USE ODESolverTypes
  TYPE,EXTENDS(ODESolverInterface_Base) :: ODESolverInterface_TestLinear
    CONTAINS
      !> Deferred routine for initializing the ode solver
      PROCEDURE,PASS :: eval => eval_linear
  ENDTYPE ODESolverInterface_TestLinear
  TYPE,EXTENDS(ODESolverInterface_Base) :: ODESolverInterface_TestExponential
    CONTAINS
      !> Deferred routine for initializing the ode solver
      PROCEDURE,PASS :: eval => eval_exp
  ENDTYPE ODESolverInterface_TestExponential
  TYPE,EXTENDS(ODESolverInterface_Base) :: ODESolverInterface_TestNonLinear
    CONTAINS
      !> Deferred routine for initializing the ode solver
      PROCEDURE,PASS :: eval => eval_nonlinear
  ENDTYPE ODESolverInterface_TestNonLinear
  CONTAINS
    SUBROUTINE eval_linear(self,t,y,ydot)
      CLASS(ODESolverInterface_TestLinear),INTENT(INOUT) :: self
      REAL(SRK),INTENT(IN) :: t
      CLASS(VectorType),INTENT(INOUT) :: y
      CLASS(VectorType),INTENT(INOUT) :: ydot

      REAL(SRK) :: tmp(3)
      CALL ydot%set(0.0_SRK)
      CALL y%get(tmp)
      CALL ydot%set(1,3.2_SRK)
      CALL ydot%set(2,0.0_SRK)
      CALL ydot%set(3,-0.3_SRK)
    ENDSUBROUTINE eval_linear
    !
    SUBROUTINE eval_exp(self,t,y,ydot)
      CLASS(ODESolverInterface_TestExponential),INTENT(INOUT) :: self
      REAL(SRK),INTENT(IN) :: t
      CLASS(VectorType),INTENT(INOUT) :: y
      CLASS(VectorType),INTENT(INOUT) :: ydot

      REAL(SRK) :: tmp(3)
      CALL ydot%set(0.0_SRK)
      CALL y%get(tmp)
      CALL ydot%set(1,3.2_SRK*tmp(1))
      CALL ydot%set(2,tmp(1))
      CALL ydot%set(3,-0.03_SRK*tmp(3))
    ENDSUBROUTINE eval_exp
    !
    SUBROUTINE eval_nonlinear(self,t,y,ydot)
      CLASS(ODESolverInterface_TestNonLinear),INTENT(INOUT) :: self
      REAL(SRK),INTENT(IN) :: t
      CLASS(VectorType),INTENT(INOUT) :: y
      CLASS(VectorType),INTENT(INOUT) :: ydot

      REAL(SRK) :: tmp(3)
      CALL ydot%set(0.0_SRK)
      CALL y%get(tmp)
      CALL ydot%set(1,1.0E-3_SRK*(tmp(3)-tmp(3)*tmp(2)*tmp(2)))
      CALL ydot%set(2,1.0E-3_SRK*(1.053-tmp(1)))
      CALL ydot%set(3,1.0E-3_SRK*(3.089-tmp(2)))
    ENDSUBROUTINE eval_nonlinear
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

#ifdef FUTILITY_HAVE_PETSC
#include <finclude/petsc.h>
#undef IS
  PetscErrorCode  :: ierr
  CALL PETScInitialize(PETSC_NULL_CHARACTER,ierr)
#endif

  !> set up default parameter list
  CALL pList%clear()
  CALL pList%add('ODESolverType->n',3_SIK)
  CALL pList%add('ODESolverType->solver',BDF_METHOD)
  CALL pList%add('ODESolverType->theta',0.0_SRK)
  CALL pList%add('ODESolverType->bdf_order',3_SIK)
  CALL pList%add('ODESolverType->tolerance',1.0e-9_SRK)
  CALL pList%add('ODESolverType->substep_size',0.01_SRK)

  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.FALSE.)
  CALL eParams%addSurrogate(e)
  CALL eODESolverType%addSurrogate(e)

  CREATE_TEST('Test ODE Solvers')

  ALLOCATE(ODESolverType_Native :: testODE)
  ALLOCATE(ODESolverInterface_TestLinear :: f)

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

  SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
    testODE%solverMethod=BDF_METHOD
    testODE%BDForder=1
  ENDSELECT
  REGISTER_SUBTEST('testStep_Native_BDF-1',testStep_Native)
  SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
    testODE%BDForder=2
  ENDSELECT
  REGISTER_SUBTEST('testStep_Native_BDF-2',testStep_Native)

  SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
    testODE%BDForder=3
  ENDSELECT
  REGISTER_SUBTEST('testStep_Native_BDF-3',testStep_Native)

  SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
    testODE%BDForder=4
  ENDSELECT
  REGISTER_SUBTEST('testStep_Native_BDF-4',testStep_Native)

  SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
    testODE%BDForder=5
  ENDSELECT
  REGISTER_SUBTEST('testStep_Native_BDF-5',testStep_Native)

  SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
    testODE%solverMethod=THETA_METHOD
  ENDSELECT
  DEALLOCATE(f)
  ALLOCATE(ODESolverInterface_TestExponential :: f)
  testODE%f=>f

  REGISTER_SUBTEST('testStep_Native_exp',testStep_Native_exp)

  DEALLOCATE(f)
  ALLOCATE(ODESolverInterface_TestNonLinear :: f)
  testODE%f=>f

  REGISTER_SUBTEST('testStep_Native_nonlinear',testStep_Native_nonlinear)

  REGISTER_SUBTEST('testClear_Native',testClear_Native)

! Sundials tests
  DEALLOCATE(testODE)
  DEALLOCATE(f)
  ALLOCATE(ODESolverType_Sundials :: testODE)
  ALLOCATE(ODESolverInterface_TestLinear :: f)

  CALL pList%set('ODESolverType->solver',BDF_METHOD)
  CALL pList%set('ODESolverType->bdf_order',3_SIK)
  CALL pList%set('ODESolverType->tolerance',1.0e-9_SRK)

  REGISTER_SUBTEST('testInit_Sundials',testInit_Sundials)
#ifdef FUTILITY_HAVE_SUNDIALS
  REGISTER_SUBTEST('testStep_Sundials_Linear',testStep_Sundials_Linear)
  DEALLOCATE(f)
  ALLOCATE(ODESolverInterface_TestExponential :: f)
  testODE%f=>f
  REGISTER_SUBTEST('testStep_Sundials_Exp',testStep_Sundials_Exp)
  DEALLOCATE(f)
  ALLOCATE(ODESolverInterface_TestNonLinear :: f)
  testODE%f=>f
  REGISTER_SUBTEST('testStep_Sundials_NonLinear',testStep_Sundials_NonLinear)
#endif
  REGISTER_SUBTEST('testClear_Sundials',testClear_Sundials)

  FINALIZE_TEST()

  CALL pList%clear()
  DEALLOCATE(testODE)
  DEALLOCATE(f)

#ifdef FUTILITY_HAVE_PETSC
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
        ASSERT(testODE%tol==1.0E-9_SRK,'%tol')
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
        ASSERT(testODE%tol==1.0E-9_SRK,'%tol')
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
    SUBROUTINE testStep_Native_exp()
      REAL(SRK) :: ref(3)

      ref(1)=3.0_SRK*EXP(3.2_SRK*3.5_SRK)
      ref(2)=3.0_SRK*(EXP(3.2_SRK*3.5_SRK)-1.0_SRK)/3.2_SRK+3.0_SRK
      ref(3)=3.0_SRK*EXP(-0.03_SRK*3.5_SRK)
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%theta=0.0_SRK
      ENDSELECT
      CALL testOrderConv(1.0_SRK,0.1_SRK,ref,'theta=0.0')
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%theta=0.5_SRK
      ENDSELECT
      CALL testOrderConv(2.0_SRK,0.1_SRK,ref,'theta=0.5')
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%theta=1.0_SRK
      ENDSELECT
      CALL testOrderConv(1.0_SRK,0.1_SRK,ref,'theta=1.0')

      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%solverMethod=BDF_METHOD
        testODE%BDForder=1
      ENDSELECT
      CALL testOrderConv(1.0_SRK,0.1_SRK,ref,'bdf-1')
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%BDForder=2
      ENDSELECT
      CALL testOrderConv(2.0_SRK,0.1_SRK,ref,'bdf-2')
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%BDForder=3
      ENDSELECT
      CALL testOrderConv(3.0_SRK,0.2_SRK,ref,'bdf-3')
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%BDForder=4
      ENDSELECT
      CALL testOrderConv(4.0_SRK,0.2_SRK,ref,'bdf-4')
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%BDForder=5
      ENDSELECT
      CALL testOrderConv(5.0_SRK,0.2_SRK,ref,'bdf-5')
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%solverMethod=THETA_METHOD
      ENDSELECT

    ENDSUBROUTINE testStep_Native_exp
!
!-------------------------------------------------------------------------------
    SUBROUTINE testStep_Native_nonlinear()
      REAL(SRK) :: ref(3)

      !This reference was generated with 1e-6 substep size
      ref=(/2.9162069601728970_SRK,2.9933322574820909_SRK,3.0003232540397478_SRK/)
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%theta=0.0_SRK
      ENDSELECT
      CALL testOrderConv(1.0_SRK,0.1_SRK,ref,'theta=0.0')
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%theta=0.5_SRK
      ENDSELECT
      CALL testOrderConv(2.0_SRK,0.1_SRK,ref,'theta=0.5')
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%theta=1.0_SRK
      ENDSELECT
      CALL testOrderConv(1.0_SRK,0.1_SRK,ref,'theta=1.0')

      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%solverMethod=BDF_METHOD
        testODE%BDForder=1
      ENDSELECT
      CALL testOrderConv(1.0_SRK,0.1_SRK,ref,'bdf-1')
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%BDForder=2
      ENDSELECT
      CALL testOrderConv(2.0_SRK,0.1_SRK,ref,'bdf-2')
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%BDForder=3
      ENDSELECT
      CALL testOrderConv(3.0_SRK,0.2_SRK,ref,'bdf-3')
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%BDForder=4
      ENDSELECT
      CALL testOrderConv(4.0_SRK,0.2_SRK,ref,'bdf-4')
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        testODE%BDForder=5
      ENDSELECT
      CALL testOrderConv(5.0_SRK,0.2_SRK,ref,'bdf-5')

    ENDSUBROUTINE testStep_Native_nonlinear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testClear_Native()
      CALL testODE%clear()
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
        ASSERT(testODE%solverMethod==-1,'%solverMethod')
        ASSERT(testODE%TPLType==-1,'%TPLType')
        ASSERT(testODE%n==-1,'%n')
        ASSERT(testODE%tol==1.0e-8_SRK,'%tol')
        ASSERT(testODE%theta==0.5_SRK,'%theta')
        ASSERT(testODE%BDForder==5,'%BDForder')
        ASSERT(testODE%substep_size==0.1_SRK,'%substep_size')
        ASSERT(.NOT. testODE%myLS%isInit,'%myLS%isInit')
        ASSERT(.NOT. testODE%isInit,'%isInit')
      ENDSELECT
    ENDSUBROUTINE testClear_Native
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInit_Sundials()
      INTEGER(SIK) :: errcnt
      errcnt=eODESolverType%getCounter(EXCEPTION_ERROR)
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Sundials)
        CALL testODE%init(pList,f)
        ASSERT(testODE%n==3,'%n')
        ASSERT(testODE%solverMethod==BDF_METHOD,'%solverMethod')
        ASSERT(testODE%BDForder==3,'%BDForder')
        ASSERT(testODE%tol==1.0E-9_SRK,'%tol')
        ASSERT(ALLOCATED(testODE%ytmp),'%ytmp Allocated')
#ifdef FUTILITY_HAVE_SUNDIALS
        ASSERT(testODE%isInit,'%isInit')
        ASSERT(ASSOCIATED(testODE%f),'%f')
        ASSERT(ASSOCIATED(EXTERNAL_ODE_INTERFACE),'EXTERNAL_ODE_INTERFACE')
#else
        ASSERT(eODESolverType%getCounter(EXCEPTION_ERROR)-errcnt == 1,'ERROR')
#endif
      ENDSELECT

    ENDSUBROUTINE testInit_Sundials
!
!-------------------------------------------------------------------------------
    SUBROUTINE testStep_Sundials_Linear()
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
    ENDSUBROUTINE testStep_Sundials_Linear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testStep_Sundials_Exp()
      REAL(SRK) :: tmpval
      TYPE(ParamType) :: tmpPL
      TYPE(RealVectorType) :: y0, yf
      REAL(SRK) :: ref(3)

      ref(1)=3.0_SRK*EXP(3.2_SRK*3.5_SRK)
      ref(2)=3.0_SRK*(EXP(3.2_SRK*3.5_SRK)-1.0_SRK)/3.2_SRK+3.0_SRK
      ref(3)=3.0_SRK*EXP(-0.03_SRK*3.5_SRK)

      CALL tmpPL%add('VectorType->n',3_SIK)
      CALL y0%init(tmpPL)
      CALL yf%init(tmpPL)

      CALL y0%set(3.0_SRK)
      CALL yf%set(0.0_SRK)

      CALL testODE%step(0.0_SRK,y0,3.5_SRK,yf)
      CALL yf%get(1,tmpval)
      ASSERT(SOFTEQR(tmpval,ref(1),1.0E-6_SRK),'yf(1)')
      FINFO() tmpval, ref(1), tmpval/ref(1)-1.0_SRK
      CALL yf%get(2,tmpval)
      ASSERT(SOFTEQR(tmpval,ref(2),1.0E-6_SRK),'yf(2)')
      FINFO() tmpval, ref(2), tmpval/ref(2)-1.0_SRK
      CALL yf%get(3,tmpval)
      ASSERT(SOFTEQR(tmpval,ref(3),1.0E-6_SRK),'yf(3)')
      FINFO() tmpval, ref(3), tmpval/ref(3)-1.0_SRK

    ENDSUBROUTINE testStep_Sundials_Exp
!
!-------------------------------------------------------------------------------
    SUBROUTINE testStep_Sundials_NonLinear()
      REAL(SRK) :: tmpval
      TYPE(ParamType) :: tmpPL
      TYPE(RealVectorType) :: y0, yf
      REAL(SRK) :: ref(3)

      ref=(/2.9162069601728970_SRK,2.9933322574820909_SRK,3.0003232540397478_SRK/)

      CALL tmpPL%add('VectorType->n',3_SIK)
      CALL y0%init(tmpPL)
      CALL yf%init(tmpPL)

      CALL y0%set(3.0_SRK)
      CALL yf%set(0.0_SRK)

      CALL testODE%step(0.0_SRK,y0,3.5_SRK,yf)

      CALL yf%get(1,tmpval)
      ASSERT(SOFTEQR(tmpval,ref(1),2.0E-8_SRK),'yf(1)')
      FINFO() tmpval, ref(1), tmpval/ref(1)-1.0_SRK
      CALL yf%get(2,tmpval)
      ASSERT(SOFTEQR(tmpval,ref(2),2.0E-8_SRK),'yf(2)')
      FINFO() tmpval, ref(2), tmpval/ref(2)-1.0_SRK
      CALL yf%get(3,tmpval)
      ASSERT(SOFTEQR(tmpval,ref(3),2.0E-8_SRK),'yf(3)')
      FINFO() tmpval, ref(3), tmpval/ref(3)-1.0_SRK

    ENDSUBROUTINE testStep_Sundials_NonLinear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testClear_Sundials()
      CALL testODE%clear()
      SELECTTYPE(testODE); TYPE IS(ODESolverType_Sundials)
        ASSERT(testODE%solverMethod==-1,'%solverMethod')
        ASSERT(testODE%TPLType==-1,'%TPLType')
        ASSERT(testODE%n==-1,'%n')
        ASSERT(testODE%tol==1.0e-8_SRK,'%tol')
        ASSERT(testODE%BDForder==5,'%BDForder')
        ASSERT(.NOT. ALLOCATED(testODE%ytmp),'%ytmp NOT Allocated')
        ASSERT(.NOT. ASSOCIATED(EXTERNAL_ODE_INTERFACE),'SUNDIALS_ODE_INTERFACE')
        ASSERT(.NOT. testODE%isInit,'%isInit')
      ENDSELECT
    ENDSUBROUTINE testClear_Sundials
!
!-------------------------------------------------------------------------------
    SUBROUTINE testOrderConv(exp_order,tol,ref,tag)
      REAL(SRK),INTENT(IN) :: exp_order
      REAL(SRK),INTENT(IN) :: tol
      REAL(SRK),INTENT(IN) :: ref(:)
      CHARACTER(LEN=*),INTENT(IN) :: tag

      INTEGER(SIK) :: i,j
      REAL(SRK) :: substep
      REAL(SRK) :: tmp(3,4)
      TYPE(ParamType) :: tmpPL
      TYPE(RealVectorType) :: y0, yf
      LOGICAL(SBK) :: bool

      CALL tmpPL%add('VectorType->n',3_SIK)
      CALL y0%init(tmpPL)
      CALL yf%init(tmpPL)

      substep=1.0E-2_SRK
      DO i=1,4
        CALL y0%set(3.0_SRK)
        CALL yf%set(0.0_SRK)
        SELECTTYPE(testODE); TYPE IS(ODESolverType_Native)
          testODE%substep_size=substep
        ENDSELECT

        CALL testODE%step(0.0_SRK,y0,3.5_SRK,yf)

        CALL yf%get(tmp(:,i))
        DO j=1,3
          tmp(j,i)=tmp(j,i)/ref(j)-1.0_SRK
        ENDDO
        substep=substep*0.1_SRK
      ENDDO

      DO i=1,3 ! reduction order
        DO j=1,3 ! solution index
          bool=(ABS(exp_order-LOG10(tmp(j,i)/tmp(j,i+1)))<=tol .OR. ABS(tmp(j,i))<=1.0E-8)
          ASSERT(bool,'Order Convergence: '//TRIM(tag))
          FINFO() tmp(j,:)
          FINFO() i,j,exp_order,LOG10(tmp(j,i)/tmp(j,i+1)),ABS(exp_order-LOG(tmp(j,i)/tmp(j,i+1)))
        ENDDO
      ENDDO


    ENDSUBROUTINE testOrderConv
!
!-------------------------------------------------------------------------------
!
ENDPROGRAM testODESolver
