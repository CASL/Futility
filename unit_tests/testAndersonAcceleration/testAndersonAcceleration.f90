!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testAndersonAcceleration
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE ExceptionHandler
USE ParameterLists
USE FutilityComputingEnvironmentModule
USE AndersonAccelerationTypes

IMPLICIT NONE

TYPE(FutilityComputingEnvironment),TARGET :: ce
TYPE(ExceptionHandlerType),TARGET :: exceptHandler
TYPE(ParamType) :: pList
TYPE(AndersonAccelerationType) :: testAndAcc

!> set up default parameter list
CALL pList%clear()
CALL pList%add('AndersonAccelerationType->N',10000_SIK)
CALL pList%add('AndersonAccelerationType->depth',10_SIK)
CALL pList%add('AndersonAccelerationType->beta',0.5_SRK)
CALL pList%add('AndersonAccelerationType->start',1)

!Configure exception handler for test
ce%exceptHandler => exceptHandler
CALL ce%exceptHandler%setStopOnError(.FALSE.)
CALL ce%exceptHandler%setQuietMode(.FALSE.)

CREATE_TEST('Test Anderson Acceleration Solver')

REGISTER_SUBTEST('testInit',testInit)
REGISTER_SUBTEST('testClear',testClear)
REGISTER_SUBTEST('testStep',testStep)

FINALIZE_TEST()

CALL pList%clear()
CALL ce%clear()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testInit()

  CALL testAndAcc%init(ce,pList)
  ASSERT(testAndAcc%isInit,'%isInit')
  ASSERT_EQ(testAndAcc%s,0,'%iter')
  ASSERT_EQ(testAndAcc%N,10000,'%N')
  ASSERT_EQ(testAndAcc%depth,10,'%depth')
  ASSERT_EQ(testAndAcc%beta,0.5_SRK,'%beta')
  ASSERT(ALLOCATED(testAndAcc%x),'%x not allocated')
  ASSERT_EQ(SIZE(testAndAcc%x,1),testAndAcc%N,'%x wrong size')
  ASSERT_EQ(SIZE(testAndAcc%x,2),testAndAcc%depth+1,'%x wrong size')
  ASSERT(ALLOCATED(testAndAcc%Gx),'%Gx not allocated')
  ASSERT_EQ(SIZE(testAndAcc%Gx,1),testAndAcc%N,'%Gx wrong size')
  ASSERT_EQ(SIZE(testAndAcc%Gx,2),testAndAcc%depth+1,'%Gx wrong size')
  ASSERT(ALLOCATED(testAndAcc%r),'%r not allocated')
  ASSERT_EQ(SIZE(testAndAcc%r,1),testAndAcc%N,'%r wrong size')
  ASSERT_EQ(SIZE(testAndAcc%r,2),testAndAcc%depth+1,'%r wrong size')
  ASSERT(ALLOCATED(testAndAcc%alpha),'%alpha not allocated')
  ASSERT_EQ(SIZE(testAndAcc%alpha,1),testAndAcc%depth+1,'%alpha wrong size')
  ASSERT(testAndAcc%LS%isInit,'%Linear solver not initialized')

ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
SUBROUTINE testStep()
  INTEGER(SIK) :: i
  REAL(SRK) :: UnAccErr(10),AccErr(10),mySol(10000),exSol(10000),inSol(10000),R(10000),Norm

  !Init Anderson
  CALL testAndAcc%init(ce,pList)

  !Exact chosen solution. Operator G will simply be taking a weighted average between
  !the current iterate and the exact following a randomly generated "convergence rate".
  !The solution vector will be randomly distributed E (80,120), with the initial guess
  !being somewhat similar.
  DO i=1,SIZE(exSol)
    CALL RANDOM_NUMBER(exSol(i))
    exSol(i)=40.0_SRK*exSol(i)+80.0_SRK
    CALL RANDOM_NUMBER(inSol(i))
    inSol(i)=40.0_SRK*inSol(i)+80.0_SRK
  ENDDO
  mySol(:)=inSol(:)
  Norm=1.0_SRK/NORM2(exSol(:))

  !Generate a fast element wise "convergence rate"
  DO i=1,SIZE(R)
    CALL RANDOM_NUMBER(R(i))
    R(i)=0.4_SRK+0.2_SRK*(R(i)-0.5_SRK)
  ENDDO

  !Get relative norms of the error between iterates and exact for unaccelerated problem
  DO i=1,10
    mySol=Weight_Avg(mySol,exSol,R)
    UnAccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
  ENDDO

  !Reset mySol to solve with Anderson now
  mySol(:)=inSol(:)

  !Set initial iterate for Anderson
  COMPONENT_TEST('Anderson_Set')
  CALL testAndAcc%reset(mySol)
  ASSERT(ALL(mySol(:)==testAndAcc%x(:,1)),'Anderson set failed')

  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  COMPONENT_TEST('Fast depth=10 beta=0.5')
  DO i=1,10
    mySol=Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    AccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
    IF(i > 1) ASSERT_LT(AccErr(i),UnAccErr(i),'Anderson too Slow')
  ENDDO

  COMPONENT_TEST('Fast depth=2 beta=0.8')
  !Reset Anderson and change options
  testAndAcc%depth=2_SIK
  testAndAcc%beta=0.8_SRK
  mySol(:)=inSol(:)
  CALL testAndAcc%reset(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    mySol=Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    AccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
    IF(i > 1) ASSERT_LT(AccErr(i),UnAccErr(i),'Anderson too Slow')
  ENDDO

  !Generate a slow element wise "convergence rate"
  DO i=1,SIZE(R)
    CALL RANDOM_NUMBER(R(i))
    R(i)=0.899999_SRK+0.2_SRK*(R(i)-0.5_SRK)
  ENDDO

  !Get relative norms of the error between iterates and exact for unaccelerated problem
  mySol(:)=inSol(:)
  DO i=1,10
    mySol=Weight_Avg(mySol,exSol,R)
    UnAccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
  ENDDO

  !Reset mySol to solve with Anderson now
  mySol(:)=inSol(:)

  COMPONENT_TEST('Slow depth=2 beta=0.8')
  !Clear and reinilialize Anderson
  CALL testAndAcc%reset(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    mySol=Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    AccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
    IF(i > 1) ASSERT_LT(AccErr(i),UnAccErr(i),'Anderson too Slow')
  ENDDO

  COMPONENT_TEST('Slow depth=1 beta=0.2')
  !Clear and reinilialize Anderson
  testAndAcc%depth=1_SIK
  testAndAcc%beta=0.2_SRK
  mySol(:)=inSol(:)
  CALL testAndAcc%reset(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    mySol=Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    AccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
    IF(i > 1) ASSERT_LT(AccErr(i),UnAccErr(i),'Anderson too Slow')
  ENDDO

  COMPONENT_TEST('Slow depth=10 beta=1.0')
  !Clear and reinilialize Anderson
  testAndAcc%depth=10_SIK
  testAndAcc%beta=1.0_SRK
  mySol(:)=inSol(:)
  CALL testAndAcc%reset(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    mySol=Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    AccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
    IF(i > 1) ASSERT_LT(AccErr(i),UnAccErr(i),'Anderson too Slow')
  ENDDO

  !Esure rate of convergence is correct:
  ASSERT_APPROXEQ(AccErr(10)/AccErr(9),0.90109849144468168_SRK,'Wrong Anderson Conv. Rate')

  COMPONENT_TEST('Slow depth=0 beta=0.5')
  !Clear and reinilialize Anderson (purposeful complete reinitialization for testing purposes)
  CALL testAndAcc%clear()
  CALL pList%set('AndersonAccelerationType->depth',0_SIK)
  CALL pList%set('AndersonAccelerationType->beta',0.5_SRK)
  CALL testAndAcc%init(ce,pList)
  mySol(:)=inSol(:)
  CALL testAndAcc%reset(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    mySol=Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    AccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
    ASSERT_GT(AccErr(i),UnAccErr(i),'Anderson too Fast')
    IF(i > 1) ASSERT_LT(AccErr(i),AccErr(i-1),'Anderson too Slow')
  ENDDO

  COMPONENT_TEST('Slow depth=1 beta=1.0 Start=4')
  CALL testAndAcc%clear()
  CALL pList%set('AndersonAccelerationType->depth',1_SIK)
  CALL pList%set('AndersonAccelerationType->beta',1.0_SRK)
  CALL pList%set('AndersonAccelerationType->start',4_SIK)
  CALL testAndAcc%init(ce,pList)
  mySol(:)=inSol(:)
  CALL testAndAcc%reset(mySol)
  DO i=1,10
    mySol=Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    AccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
    IF(i <= testAndAcc%start) ASSERT_APPROXEQ(AccErr(i),UnAccErr(i),'Improper Delayed Start')
    IF(i > testAndAcc%start) ASSERT_LT(AccErr(i),UnAccErr(i),'Anderson too Slow'//CHAR(i))
  ENDDO

ENDSUBROUTINE testStep
!
!-------------------------------------------------------------------------------
FUNCTION Weight_Avg(x1,x2,R) RESULT(x_avg)
  REAL(SRK),INTENT(IN) :: x1(:)
  REAL(SRK),INTENT(IN) :: x2(:)
  REAL(SRK),INTENT(IN) :: R(:)

  REAL(SRK) :: x_avg(SIZE(x1))
  INTEGER(SIK) :: i

  DO i=1,SIZE(x1)
    x_avg(i)=R(i)*x1(i)+(1_SRK-R(i))*x2(i)
  ENDDO

ENDFUNCTION
!
!-------------------------------------------------------------------------------
SUBROUTINE testClear()

  CALL testAndAcc%clear()
  ASSERT(.NOT.testAndAcc%isInit,'%isInit')
  ASSERT_EQ(testAndAcc%s,0,'%s')
  ASSERT_EQ(testAndAcc%N,-1,'%N')
  ASSERT_EQ(testAndAcc%depth,-1,'%depth')
  ASSERT_EQ(testAndAcc%start,0,'%start')
  ASSERT_EQ(testAndAcc%beta,0.0_SRK,'%beta')
  ASSERT(.NOT.ALLOCATED(testAndAcc%x),'%x)')
  ASSERT(.NOT.ALLOCATED(testAndAcc%Gx),'%Gx)')
  ASSERT(.NOT.ALLOCATED(testAndAcc%r),'%r)')
  ASSERT(.NOT.testAndAcc%LS%isInit,'%LS')

ENDSUBROUTINE testClear
!
ENDPROGRAM testAndersonAcceleration
