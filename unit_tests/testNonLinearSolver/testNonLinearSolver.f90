!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE testNonLinearSolverInterface
USE IntrType
USE VectorTypes
USE MatrixTypes
USE NonLinearSolverModule

IMPLICIT NONE
PRIVATE

PUBLIC :: NonLinearSolverInterface_Test

!Sets up a test interface to find the roots of the following system of equations:
!  2*x1 + x1*x2 = 2
!  2*x2 - x1*x2**2 = 2
!The root occurs at x1 = 0.5, x2 = 2
TYPE,EXTENDS(NonLinearSolverInterface_Base) :: NonLinearSolverInterface_Test
  CONTAINS
    PROCEDURE,PASS :: eval => eval_test
ENDTYPE NonLinearSolverInterface_Test

CONTAINS
SUBROUTINE eval_test(this,x,y)
  CLASS(NonLinearSolverInterface_Test),INTENT(IN) :: this
  CLASS(VectorType),INTENT(INOUT) :: x
  CLASS(VectorType),INTENT(INOUT) :: y
  !
  REAL(SRK) :: xreal(2)

  CALL x%getAll(xreal)
  CALL y%set((/2.0_SRK*xreal(1) + xreal(1)*xreal(2) - 2.0_SRK, & ! 2*x1 + x1*x2 -2
      2.0_SRK*xreal(2) - xreal(1)*xreal(2)**2.0_SRK - 2.0_SRK/)) ! 2*x2 - x1*x2**2 -2

ENDSUBROUTINE eval_test
ENDMODULE testNonLinearSolverInterface
!
PROGRAM testNonLinearSolver
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE ParameterLists
USE FutilityComputingEnvironmentModule
USE VectorTypes
USE MatrixTypes
USE LinearSolverTypes
USE NonLinearSolverModule
USE testNonLinearSolverInterface

IMPLICIT NONE

TYPE(FutilityComputingEnvironment),TARGET :: ce
TYPE(NonLinearSolverInterface_Test),TARGET :: ftest
TYPE(NonLinearSolver_Native) :: nativeSolver

CALL setupTest()
CREATE_TEST("testNonLinearSolver")

REGISTER_SUBTEST('Native, Newton''s Method',testNativeNewton)

FINALIZE_TEST()
CALL clearTest()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testNativeNewton()
  REAL(SRK) :: refsol(2)=(/0.5_SRK,2.0_SRK/),testsol(2)
  TYPE(ParamType) :: plist
  TYPE(RealVectorType) :: x
  TYPE(LinearSolverType_Direct) :: refLS
  CLASS(LinearSolverType_Base),ALLOCATABLE :: testLS

  CALL plist%add('NonLinearSolver -> n',2)
  CALL plist%add('NonLinearSolver -> method',NLSOLVER_METHOD_NEWTON)
  CALL plist%add('NonLinearSolver -> tolerance',1.0E-14_SRK)
  CALL nativeSolver%init(ce,ftest,plist)
  CALL plist%clear()

  COMPONENT_TEST('InitBase')
  ASSERT_EQ(testSolverMethod(nativeSolver),NLSOLVER_METHOD_NEWTON,'%solverMethod')
  ASSERT_EQ(testN(nativeSolver),2,'%n')
  ASSERT_EQ(testIterations(nativeSolver),-1,'%iterations')
  ASSERT_EQ(testTol(nativeSolver),1.0E-14_SRK,'%tol')
  ASSERT(ASSOCIATED(testCE(nativeSolver)),'%ce')
  ASSERT(ASSOCIATED(testFunc(nativeSolver)),'%func')

  COMPONENT_TEST('Init')
  ASSERT_EQ(testTPLType(nativeSolver),NLSOLVER_TPL_NATIVE,'%TPLType')
  CALL testLinSys(nativeSolver,testLS)
  ASSERT(testLS%isInit,'%linSys')
  ASSERT(SAME_TYPE_AS(refLS,testLS),'%linSys dynamic type')
  DEALLOCATE(testLS)
  ASSERT(nativeSolver%isInit,'%isInit')

  CALL plist%add('VectorType -> n',2)
  CALL x%init(plist)
  CALL plist%clear()

  COMPONENT_TEST('Solve')
  CALL x%set((/0.0_SRK,0.0_SRK/))
  CALL nativeSolver%solve(x)
  CALL x%get(testsol)
  ASSERT_APPROXEQ(testsol(1),refsol(1),'solution(1)')
  ASSERT_APPROXEQ(testsol(2),refsol(2),'solution(2)')
  ASSERT_EQ(testIterations(nativeSolver),7,'%iterations')
  CALL x%clear()

  CALL nativeSolver%clear()

  COMPONENT_TEST('ClearBase')
  ASSERT_EQ(testSolverMethod(nativeSolver),-1,'%solverMethod')
  ASSERT_EQ(testTPLType(nativeSolver),-1,'%TPLType')
  ASSERT_EQ(testN(nativeSolver),-1,'%n')
  ASSERT_EQ(testIterations(nativeSolver),-1,'%iterations')
  ASSERT_EQ(testTol(nativeSolver),1.0E-5_SRK,'%tol')
  CALL testLinSys(nativeSolver,testLS)
  ASSERT(.NOT.ALLOCATED(testLS),'%linSys')
  ASSERT(.NOT.ASSOCIATED(testCE(nativeSolver)),'%ce')
  ASSERT(.NOT.ASSOCIATED(testFunc(nativeSolver)),'ASSOCIATED %func')
  ASSERT(.NOT.nativeSolver%isInit,'%isInit')

ENDSUBROUTINE testNativeNewton
!
!-------------------------------------------------------------------------------
SUBROUTINE setupTest()

  ALLOCATE(ce%exceptHandler)
  ALLOCATE(ce%parEnv)

ENDSUBROUTINE setupTest
!
!-------------------------------------------------------------------------------
SUBROUTINE clearTest()

  DEALLOCATE(ce%exceptHandler)
  CALL ce%parEnv%world%finalize()
  DEALLOCATE(ce%parEnv)

ENDSUBROUTINE clearTest
!
ENDPROGRAM testNonLinearSolver