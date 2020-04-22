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
USE ParallelEnv
USE VectorTypes
USE AndersonAccelerationTypes

IMPLICIT NONE

TYPE(ExceptionHandlerType),TARGET :: e
TYPE(MPI_EnvType) :: mpiTestEnv
TYPE(ParamType) :: pList,optList
TYPE(AndersonAccelerationType) :: testAndAcc

!> set up default parameter list
CALL optList%clear()
CALL optList%add('AndersonAccelerationType->N',10000_SIK)
CALL optList%add('AndersonAccelerationType->depth',10_SIK)
CALL optList%add('AndersonAccelerationType->beta',0.5_SRK)
CALL optList%add('AndersonAccelerationType->start',1)

!Configure exception handler for test
CALL e%setStopOnError(.FALSE.)
CALL e%setQuietMode(.TRUE.)
CALL eParams%addSurrogate(e)
CALL eAndersonAccelerationType%addSurrogate(e)
CALL mpiTestEnv%init(PE_COMM_SELF)

CREATE_TEST('Test Anderson Acceleration Solver')

REGISTER_SUBTEST('testInit',testInit)
REGISTER_SUBTEST('testClear',testClear)
REGISTER_SUBTEST('testStep',testStep)

FINALIZE_TEST()

CALL pList%clear()
CALL optList%clear()

CALL mpiTestEnv%finalize()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testInit()

  CALL testAndAcc%init(mpiTestEnv,optList)
  ASSERT(testAndAcc%isInit,'%isInit')
  ASSERT(testAndAcc%s==0,'%iter')
  ASSERT(testAndAcc%N==10000,'%N')
  ASSERT(testAndAcc%depth==10,'%depth')
  ASSERT(testAndAcc%beta==0.5_SRK,'%beta')
  ASSERT(ASSOCIATED(testAndAcc%MPIparallelEnv),'%MPIenv')
  ASSERT(ALLOCATED(testAndAcc%x),'%x not allocated')
  ASSERT(SIZE(testAndAcc%x,1) == testAndAcc%N,'%x wrong size')
  ASSERT(SIZE(testAndAcc%x,2) == testAndAcc%depth+1,'%x wrong size')
  ASSERT(ALLOCATED(testAndAcc%Gx),'%Gx not allocated')
  ASSERT(SIZE(testAndAcc%Gx,1) == testAndAcc%N,'%Gx wrong size')
  ASSERT(SIZE(testAndAcc%Gx,2) == testAndAcc%depth+1,'%Gx wrong size')
  ASSERT(ALLOCATED(testAndAcc%r),'%r not allocated')
  ASSERT(SIZE(testAndAcc%r,1) == testAndAcc%N,'%r wrong size')
  ASSERT(SIZE(testAndAcc%r,2) == testAndAcc%depth+1,'%r wrong size')
  ASSERT(ALLOCATED(testAndAcc%A),'%A not allocated')
  ASSERT(SIZE(testAndAcc%A,1) == testAndAcc%depth,'%A wrong size')
  ASSERT(SIZE(testAndAcc%A,2) == testAndAcc%depth,'%A wrong size')
  ASSERT(ALLOCATED(testAndAcc%alpha),'%alpha not allocated')
  ASSERT(SIZE(testAndAcc%alpha,1) == testAndAcc%depth+1,'%alpha wrong size')

ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
SUBROUTINE testStep()
  INTEGER(SIK) :: i
  REAL(SRK) :: UnAccErr(10),AccErr(10),mySol(10000),exSol(10000),inSol(10000),R(10000),Norm

  !Init Anderson
  CALL testAndAcc%init(mpiTestEnv,optList)

  !Exact chosen solution. Operator G will simply be taking a weighted average between
  !the current iterate and the exact following a randomly generated "convergence rate".
  !The solution vector will be randomly distributed E (80,120), with the initial guess
  !being similar.
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
  CALL testAndAcc%set(mySol)
  ASSERT(ALL(mySol(:) == testAndAcc%x(:,1)),'Anderson set failed')

  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  COMPONENT_TEST('Fast depth=10 beta=0.5')
  DO i=1,10
    mySol=testAndAcc%step(Weight_Avg(mySol,exSol,R))
    AccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
    IF(i > 1) ASSERT(AccErr(i) < UnAccErr(i),'Anderson too Slow')
  ENDDO

  COMPONENT_TEST('Fast depth=2 beta=0.8')
  !Clear and reinilialize Anderson
  CALL testAndAcc%clear()
  CALL optList%set('AndersonAccelerationType->depth',2_SIK)
  CALL optList%set('AndersonAccelerationType->beta',0.8_SRK)
  CALL testAndAcc%init(mpiTestEnv,optList)
  mySol(:)=inSol(:)
  CALL testAndAcc%set(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    mySol=testAndAcc%step(Weight_Avg(mySol,exSol,R))
    AccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
    IF(i > 1) ASSERT(AccErr(i) < UnAccErr(i),'Anderson too Slow')
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
  CALL testAndAcc%clear()
  CALL testAndAcc%init(mpiTestEnv,optList)
  CALL testAndAcc%set(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    mySol=testAndAcc%step(Weight_Avg(mySol,exSol,R))
    AccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
    IF(i > 1) ASSERT(AccErr(i) < UnAccErr(i),'Anderson too Slow')
  ENDDO

  COMPONENT_TEST('Slow depth=1 beta=0.2')
  !Clear and reinilialize Anderson
  CALL testAndAcc%clear()
  CALL optList%set('AndersonAccelerationType->depth',1_SIK)
  CALL optList%set('AndersonAccelerationType->beta',0.2_SRK)
  CALL testAndAcc%init(mpiTestEnv,optList)
  mySol(:)=inSol(:)
  CALL testAndAcc%set(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    mySol=testAndAcc%step(Weight_Avg(mySol,exSol,R))
    AccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
    IF(i > 1) ASSERT(AccErr(i) < UnAccErr(i),'Anderson too Slow')
  ENDDO

  COMPONENT_TEST('Slow depth=10 beta=1.0')
  !Clear and reinilialize Anderson
  CALL testAndAcc%clear()
  CALL optList%set('AndersonAccelerationType->depth',10_SIK)
  CALL optList%set('AndersonAccelerationType->beta',1.0_SRK)
  CALL testAndAcc%init(mpiTestEnv,optList)
  mySol(:)=inSol(:)
  CALL testAndAcc%set(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    mySol=testAndAcc%step(Weight_Avg(mySol,exSol,R))
    AccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
    IF(i > 1) ASSERT(AccErr(i) < UnAccErr(i),'Anderson too Slow')
  ENDDO

  !Esure rate of convergence is correct:
  ASSERT(AccErr(10)/AccErr(9) .APPROXEQ. 0.90109868784461988_SRK,'Wrong Anderson Conv. Rate')

  COMPONENT_TEST('Slow depth=0 beta=0.5')
  !Clear and reinilialize Anderson
  CALL testAndAcc%clear()
  CALL optList%set('AndersonAccelerationType->depth',0_SIK)
  CALL optList%set('AndersonAccelerationType->beta',0.5_SRK)
  CALL testAndAcc%init(mpiTestEnv,optList)
  mySol(:)=inSol(:)
  CALL testAndAcc%set(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    mySol=testAndAcc%step(Weight_Avg(mySol,exSol,R))
    AccErr(i)=NORM2(mySol(:)-exSol(:))*Norm
    ASSERT(AccErr(i) > UnAccErr(i),'Anderson too Fast')
    IF(i > 1) ASSERT(AccErr(i) < AccErr(i-1),'Anderson too Slow')
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
  ASSERT(.NOT. testAndAcc%isInit,'%isInit')
  ASSERT(.NOT. ASSOCIATED(testAndAcc%MPIparallelEnv),'%MPIenv')
  ASSERT(testAndAcc%s==0,'%s')
  ASSERT(testAndAcc%N==-1,'%N')
  ASSERT(testAndAcc%depth==-1,'%depth')
  ASSERT(testAndAcc%start==1,'%start')
  ASSERT(testAndAcc%beta==0.0_SRK,'%beta')
  ASSERT(.NOT.ALLOCATED(testAndAcc%x),'%x)')
  ASSERT(.NOT.ALLOCATED(testAndAcc%Gx),'%Gx)')
  ASSERT(.NOT.ALLOCATED(testAndAcc%r),'%r)')
  ASSERT(.NOT.ALLOCATED(testAndAcc%A),'%A)')
  ASSERT(.NOT.ALLOCATED(testAndAcc%alpha),'%alpha)')
  ASSERT(.NOT.ALLOCATED(testAndAcc%b),'%b)')

ENDSUBROUTINE testClear
!
ENDPROGRAM testAndersonAcceleration
