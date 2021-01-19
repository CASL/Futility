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
USE ParallelEnv
USE AndersonAccelerationModule
USE VectorTypes
USE StochasticSampling

IMPLICIT NONE

TYPE(FutilityComputingEnvironment),TARGET :: ce
TYPE(ExceptionHandlerType),TARGET :: exceptHandler
TYPE(AndersonAccelerationType) :: testAndAcc
CLASS(VectorType),ALLOCATABLE :: mySol,exSol,inSol,tmpvec
TYPE(StochasticSamplingType) :: NativeRNG
TYPE(ParamType) :: pList,vecparams
REAL(SRK) :: tmp1,tmp2

ALLOCATE(RealVectorType :: mySol,exSol,inSol,tmpvec)
CALL vecparams%add("VectorType->n",10000_SIK)
CALL mySol%init(vecparams)
CALL exSol%init(vecparams)
CALL inSol%init(vecparams)
CALL tmpvec%init(vecparams)

!Setup Anderson parameter list
CALL pList%add('SolutionAcceleration->num_unknowns',10000_SIK)
CALL pList%add('SolutionAcceleration->anderson_depth',10_SIK)
CALL pList%add('SolutionAcceleration->anderson_mixing_parameter',0.5_SRK)
CALL pList%add('SolutionAcceleration->starting_iteration',1)

!Configure exception handler for test
ce%exceptHandler => exceptHandler
CALL ce%exceptHandler%setStopOnError(.FALSE.)
CALL ce%exceptHandler%setQuietMode(.FALSE.)

CREATE_TEST('Test Anderson Acceleration Solver')

CALL NativeRNG%init(RNG_MCNP_STD)
REGISTER_SUBTEST('Defaults',testDefaults)
REGISTER_SUBTEST('testInit(Real)',testInit)
REGISTER_SUBTEST('testClear(Real)',testClear)
REGISTER_SUBTEST('testReset(Real)',testReset)
REGISTER_SUBTEST('testStep(Real)',testStep)

#ifdef FUTILITY_HAVE_PETSC
!Reset everthing to test with different vector type
!Setup Anderson parameter list
CALL pList%clear()
CALL pList%add('SolutionAcceleration->num_unknowns',10000_SIK)
CALL pList%add('SolutionAcceleration->anderson_depth',10_SIK)
CALL pList%add('SolutionAcceleration->anderson_mixing_parameter',0.5_SRK)
CALL pList%add('SolutionAcceleration->starting_iteration',1)
CALL NativeRNG%clear()
CALL NativeRNG%init(RNG_MCNP_STD)
CALL mySol%clear()
CALL exSol%clear()
CALL inSol%clear()
CALL tmpvec%clear()
DEALLOCATE(mySol,exSol,inSol,tmpvec)
ALLOCATE(PETScVectorType :: mySol,exSol,inSol,tmpvec)
CALL vecparams%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
CALL vecparams%add('VectorType->nlocal',10000_SIK)
CALL mySol%init(vecparams)
CALL exSol%init(vecparams)
CALL inSol%init(vecparams)
CALL tmpvec%init(vecparams)

REGISTER_SUBTEST('testInit(Distrib)',testInit)
REGISTER_SUBTEST('testClear(Distrib)',testClear)
REGISTER_SUBTEST('testReset(Distrib)',testReset)
REGISTER_SUBTEST('testStep(Distrib)',testStep)
#endif
!!!TODO: When support for TrilinosVectors and NativeDistributed are added, add testing for them

CALL mySol%clear()
CALL exSol%clear()
CALL inSol%clear()
CALL tmpvec%clear()
DEALLOCATE(mySol,exSol,inSol,tmpvec)
CALL NativeRNG%clear()
CALL pList%clear()
CALL vecparams%clear()

FINALIZE_TEST()

CALL ce%clear()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testDefaults()

  ASSERT_EQ(testAndAcc%s,0,'%iter')
  ASSERT_EQ(testAndAcc%depth,1,'%depth')
  ASSERT_EQ(testAndAcc%start,1,'%start')
  ASSERT_EQ(testAndAcc%beta,0.5_SRK,'%beta')

ENDSUBROUTINE testDefaults
!
!-------------------------------------------------------------------------------
SUBROUTINE testInit()

  CALL testAndAcc%init(ce,pList)
  ASSERT(testAndAcc%isInit,'%isInit')
  ASSERT_EQ(testAndAcc%s,0,'%iter')
  ASSERT_EQ(testAndAcc%N,10000,'%N')
  ASSERT_EQ(testAndAcc%depth,10,'%depth')
  ASSERT_EQ(testAndAcc%start,1,'%start')
  ASSERT_EQ(testAndAcc%beta,0.5_SRK,'%beta')
  ASSERT(ALLOCATED(testAndAcc%alpha),'%alpha not allocated')
  ASSERT_EQ(SIZE(testAndAcc%alpha,1),testAndAcc%depth+1,'%alpha wrong size')
  ASSERT(testAndAcc%LS%isInit,'%Linear solver not initialized')

ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
SUBROUTINE testClear()

  CALL testAndAcc%clear()
  ASSERT(.NOT.testAndAcc%isInit,'%isInit')
  ASSERT_EQ(testAndAcc%s,0,'%s')
  ASSERT_EQ(testAndAcc%N,-1,'%N')
  ASSERT_EQ(testAndAcc%depth,1,'%depth')
  ASSERT_EQ(testAndAcc%start,1,'%start')
  ASSERT_EQ(testAndAcc%beta,0.5_SRK,'%beta')
  ASSERT(.NOT.ALLOCATED(testAndAcc%x),'%x)')
  ASSERT(.NOT.ALLOCATED(testAndAcc%Gx),'%Gx)')
  ASSERT(.NOT.ALLOCATED(testAndAcc%r),'%r)')
  ASSERT(.NOT.ALLOCATED(testAndAcc%alpha),'%alpha')
  ASSERT(.NOT.testAndAcc%LS%isInit,'%LS')

ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
SUBROUTINE testReset()
  INTEGER(SIK) :: i,j
  LOGICAL(SBK) :: bool

  !Init Anderson
  CALL testAndAcc%init(ce,pList)

  !Test initial set
  COMPONENT_TEST('Initial set')

  CALL mySol%set(1.0_SRK)
  CALL testAndAcc%reset(mySol)

  ASSERT(ALLOCATED(testAndAcc%x),'%x not allocated')
  ASSERT_EQ(SIZE(testAndAcc%x),testAndAcc%depth+1,'%x wrong number of vectors')
  ASSERT_EQ(testAndAcc%x(1)%n,testAndAcc%N,'%x wrong vector size')
  ASSERT(ALLOCATED(testAndAcc%Gx),'%Gx not allocated')
  ASSERT_EQ(SIZE(testAndAcc%Gx),testAndAcc%depth+1,'%Gx wrong number of vectors')
  ASSERT_EQ(testAndAcc%Gx(1)%n,testAndAcc%N,'%Gx wrong vector size')
  ASSERT(ALLOCATED(testAndAcc%r),'%r not allocated')
  ASSERT_EQ(SIZE(testAndAcc%r),testAndAcc%depth+1,'%r wrong number of vectors')
  ASSERT_EQ(testAndAcc%r(1)%n,testAndAcc%N,'%r wrong vector size')
  bool=.TRUE.
  DO i=1,mySol%n
    CALL mySol%get(i,tmp1)
    CALL testAndAcc%x(1)%get(i,tmp2)
    IF(tmp1 /= tmp2) bool=.FALSE.
  ENDDO
  ASSERT(bool,'Anderson reset failed to set vector')

  !Test subsequent reset
  COMPONENT_TEST('Subsequent Reset')

  CALL mySol%set(0.0_SRK)
  testAndAcc%s=2_SIK
  DO j=1,testAndAcc%depth
    DO i=1,testAndAcc%depth
      CALL testAndAcc%LS%A%set(i,j,2.0_SRK)
    ENDDO
    CALL testAndAcc%LS%b%set(j,2.0_SRK)
  ENDDO
  CALL testAndAcc%reset(mySol)

  bool=.TRUE.
  DO i=1,mySol%n
    CALL mySol%get(i,tmp1)
    CALL testAndAcc%x(1)%get(i,tmp2)
    IF(tmp1 /= tmp2) bool=.FALSE.
  ENDDO
  ASSERT(bool,'Anderson reset failed to reset vector')

  bool=.TRUE.
  DO j=1,testAndAcc%depth
    DO i=1,testAndAcc%depth
      CALL testAndAcc%LS%A%get(i,j,tmp1)
      IF(i == j) THEN
        IF(tmp1 /= 1.0_SRK) bool=.FALSE.
      ELSE
        IF(tmp1 /= 0.0_SRK) bool=.FALSE.
      ENDIF
    ENDDO
    CALL testAndAcc%LS%b%get(j,tmp1)
    IF(tmp1 /= 0.0_SRK) bool=.FALSE.
  ENDDO
  ASSERT(bool,'Anderson reset failed to reset linear solver')
  ASSERT_EQ(testAndAcc%s,0_SIK,'%s incorrect iteration count')

  CALL testAndAcc%clear()

ENDSUBROUTINE testReset
!
!-------------------------------------------------------------------------------
SUBROUTINE testStep()
  INTEGER(SIK) :: i
  REAL(SRK) :: UnAccErr(10),AccErr(10),R(10000),rando,Norm

  !Init Anderson
  CALL testAndAcc%init(ce,pList)

  !Exact chosen solution. Operator G will simply be taking a weighted average between
  !the current iterate and the exact following a randomly generated "convergence rate".
  !The solution vector will be randomly distributed E (80,120), with the initial guess
  !being somewhat similar.
  DO i=1,exSol%n
    rando=NativeRNG%rng()
    CALL exSol%set(i,40.0_SRK*rando+80.0_SRK)
    rando=NativeRNG%rng()
    CALL inSol%set(i,40.0_SRK*rando+80.0_SRK)
  ENDDO
  CALL BLAS_copy(inSol,mySol)
  Norm=1.0_SRK/BLAS_nrm2(exSol)

  !Generate a fast element wise "convergence rate"
  DO i=1,SIZE(R)
    R(i)=NativeRNG%rng()
    R(i)=0.4_SRK+0.2_SRK*(R(i)-0.5_SRK)
  ENDDO

  !Get relative norms of the error between iterates and exact for unaccelerated problem
  DO i=1,10
    CALL Weight_Avg(mySol,exSol,R)
    CALL BLAS_copy(mySol,tmpvec)
    CALL BLAS_axpy(exSol,tmpvec,-1.0_SRK)
    UnAccErr(i)=BLAS_nrm2(tmpvec)*Norm
  ENDDO

  !Reset mySol to solve with Anderson now
  CALL BLAS_copy(inSol,mySol)

  !Set Anderson solver
  CALL testAndAcc%reset(mySol)

  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  COMPONENT_TEST('Fast depth=10 beta=0.5')
  DO i=1,10
    CALL Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    CALL BLAS_copy(mySol,tmpvec)
    CALL BLAS_axpy(exSol,tmpvec,-1.0_SRK)
    AccErr(i)=BLAS_nrm2(tmpvec)*Norm
    IF(i > 1) ASSERT_LT(AccErr(i),UnAccErr(i),'Anderson too Slow')
  ENDDO

  COMPONENT_TEST('Fast depth=2 beta=0.8')
  !Reset Anderson and change options
  testAndAcc%depth=2_SIK
  testAndAcc%beta=0.8_SRK
  CALL BLAS_copy(inSol,mySol)
  CALL testAndAcc%reset(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    CALL Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    CALL BLAS_copy(mySol,tmpvec)
    CALL BLAS_axpy(exSol,tmpvec,-1.0_SRK)
    AccErr(i)=BLAS_nrm2(tmpvec)*Norm
    IF(i > 1) ASSERT_LT(AccErr(i),UnAccErr(i),'Anderson too Slow')
  ENDDO

  !Generate a slow element wise "convergence rate"
  DO i=1,SIZE(R)
    R(i)=NativeRNG%rng()
    R(i)=0.899999_SRK+0.2_SRK*(R(i)-0.5_SRK)
  ENDDO

  !Get relative norms of the error between iterates and exact for unaccelerated problem
  CALL BLAS_copy(inSol,mySol)
  DO i=1,10
    CALL Weight_Avg(mySol,exSol,R)
    CALL BLAS_copy(mySol,tmpvec)
    CALL BLAS_axpy(exSol,tmpvec,-1.0_SRK)
    UnAccErr(i)=BLAS_nrm2(tmpvec)*Norm
  ENDDO

  !Reset mySol to solve with Anderson now
  CALL BLAS_copy(inSol,mySol)

  COMPONENT_TEST('Slow depth=2 beta=0.8')
  !Reset Anderson
  CALL testAndAcc%reset(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    CALL Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    CALL BLAS_copy(mySol,tmpvec)
    CALL BLAS_axpy(exSol,tmpvec,-1.0_SRK)
    AccErr(i)=BLAS_nrm2(tmpvec)*Norm
    IF(i > 1) ASSERT_LT(AccErr(i),UnAccErr(i),'Anderson too Slow')
  ENDDO

  COMPONENT_TEST('Slow depth=1 beta=0.2')
  !Reset Anderson and change options
  testAndAcc%depth=1_SIK
  testAndAcc%beta=0.2_SRK
  CALL BLAS_copy(inSol,mySol)
  CALL testAndAcc%reset(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    CALL Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    CALL BLAS_copy(mySol,tmpvec)
    CALL BLAS_axpy(exSol,tmpvec,-1.0_SRK)
    AccErr(i)=BLAS_nrm2(tmpvec)*Norm
    IF(i > 1) ASSERT_LT(AccErr(i),UnAccErr(i),'Anderson too Slow')
  ENDDO

  COMPONENT_TEST('Slow depth=10 beta=1.0')
  !Reset Anderson and change options
  testAndAcc%depth=10_SIK
  testAndAcc%beta=1.0_SRK
  CALL BLAS_copy(inSol,mySol)
  CALL testAndAcc%reset(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    CALL Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    CALL BLAS_copy(mySol,tmpvec)
    CALL BLAS_axpy(exSol,tmpvec,-1.0_SRK)
    AccErr(i)=BLAS_nrm2(tmpvec)*Norm
    IF(i > 1) ASSERT_LT(AccErr(i),UnAccErr(i),'Anderson too Slow')
  ENDDO

  !Ensure rate of convergence is correct:
  !The rate here will need to be updated if any change is made to the Futility Stochastic
  !Sampling random number generator.       this number:
  ASSERT_APPROXEQ(AccErr(10)/AccErr(9),0.88883407949028004_SRK,'Wrong Anderson Conv. Rate')

  COMPONENT_TEST('Slow depth=0 beta=0.5')
  !Clear and reinilialize Anderson (purposeful complete reinitialization for testing purposes)
  CALL testAndAcc%clear()
  CALL pList%set('SolutionAcceleration->anderson_depth',0_SIK)
  CALL pList%set('SolutionAcceleration->anderson_mixing_parameter',0.5_SRK)
  CALL testAndAcc%init(ce,pList)
  CALL BLAS_copy(inSol,mySol)
  CALL testAndAcc%reset(mySol)
  !Get relative norms of the error between iterates and exact for accelerated problem
  !and compare to ensure that anderson is beating nonaccelerated
  DO i=1,10
    CALL Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    CALL BLAS_copy(mySol,tmpvec)
    CALL BLAS_axpy(exSol,tmpvec,-1.0_SRK)
    AccErr(i)=BLAS_nrm2(tmpvec)*Norm
    ASSERT_GT(AccErr(i),UnAccErr(i),'Anderson too Fast')
    IF(i > 1) ASSERT_LT(AccErr(i),AccErr(i-1),'Anderson too Slow')
  ENDDO

  COMPONENT_TEST('Slow depth=1 beta=1.0 Start=4')
  !Clear and reinilialize Anderson
  CALL testAndAcc%clear()
  CALL pList%set('SolutionAcceleration->anderson_depth',1_SIK)
  CALL pList%set('SolutionAcceleration->anderson_mixing_parameter',1.0_SRK)
  CALL pList%set('SolutionAcceleration->starting_iteration',4_SIK)
  CALL testAndAcc%init(ce,pList)
  CALL BLAS_copy(inSol,mySol)
  CALL testAndAcc%reset(mySol)
  DO i=1,10
    CALL Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    CALL BLAS_copy(mySol,tmpvec)
    CALL BLAS_axpy(exSol,tmpvec,-1.0_SRK)
    AccErr(i)=BLAS_nrm2(tmpvec)*Norm
    IF(i <= testAndAcc%start) ASSERT_APPROXEQ(AccErr(i),UnAccErr(i),'Improper Delayed Start')
    IF(i > testAndAcc%start) ASSERT_LT(AccErr(i),UnAccErr(i),'Anderson too Slow'//CHAR(i))
  ENDDO

  COMPONENT_TEST('Slow depth=1 beta=1.0 Failure Rescue')
  !Reset Anderson, this time a duplicate iterate will be handed in
  testAndAcc%start=1
  CALL BLAS_copy(inSol,mySol)
  CALL testAndAcc%reset(mySol)
  DO i=1,10
    IF(i /= 6) CALL Weight_Avg(mySol,exSol,R)
    CALL testAndAcc%step(mySol)
    CALL BLAS_copy(mySol,tmpvec)
    CALL BLAS_axpy(exSol,tmpvec,-1.0_SRK)
    AccErr(i)=BLAS_nrm2(tmpvec)*Norm
    IF(i > 1) ASSERT_LT(AccErr(i),UnAccErr(i),'Anderson too Slow'//CHAR(i))
  ENDDO

  !Final clear
  CALL testAndAcc%clear()

ENDSUBROUTINE testStep
!
!-------------------------------------------------------------------------------
SUBROUTINE Weight_Avg(x1,x2,R)
  CLASS(VectorType),INTENT(INOUT) :: x1
  CLASS(VectorType),INTENT(INOUT) :: x2
  REAL(SRK),INTENT(IN) :: R(:)

  INTEGER(SIK) :: i
  REAL(SRK) :: tmp1,tmp2

  DO i=1,x1%n
    CALL x1%get(i,tmp1)
    CALL x2%get(i,tmp2)
    tmp1=R(i)*tmp1+(1_SRK-R(i))*tmp2
    CALL x1%set(i,tmp1)
  ENDDO

ENDSUBROUTINE
!
ENDPROGRAM testAndersonAcceleration
