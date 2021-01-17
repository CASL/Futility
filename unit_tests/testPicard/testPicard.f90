!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testPicard
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE ExceptionHandler
USE ParameterLists
USE FutilityComputingEnvironmentModule
USE ParallelEnv
USE SolutionAccelerationModule
USE VectorTypes

IMPLICIT NONE

TYPE(FutilityComputingEnvironment),TARGET :: ce
TYPE(ExceptionHandlerType),TARGET :: exceptHandler
TYPE(ParallelEnvType),TARGET :: pe
TYPE(RelaxedPicardType) :: testRlxPicard
TYPE(ModifiedPicardType) :: testModPicard
CLASS(VectorType),ALLOCATABLE :: x
TYPE(ParamType) :: pList,vecparams

ALLOCATE(RealVectorType :: x)
CALL vecparams%add("VectorType->n",2_SIK)
CALL x%init(vecparams)

!Setup parameter list
CALL pList%add('SolutionAccelerationType->N',2_SIK)
CALL pList%add('RelaxedPicardType->alpha',0.6_SRK)
CALL pList%add('SolutionAccelerationType->start',3)

!Configure exception handler for test
ce%exceptHandler => exceptHandler
CALL ce%exceptHandler%setStopOnError(.FALSE.)
CALL ce%exceptHandler%setQuietMode(.FALSE.)

ce%parEnv => pe
CALL pe%initialize(PE_COMM_WORLD,1,1,1,1)

CREATE_TEST('Test Picard Solvers')

REGISTER_SUBTEST('testInit Relaxed',testInitRlx)
REGISTER_SUBTEST('testClear Relaxed',testClearRlx)
REGISTER_SUBTEST('testSetInitial Relaxed',testSetRlx)
REGISTER_SUBTEST('testReset Relaxed',testResetRlx)
REGISTER_SUBTEST('testStep Relaxed',testStepRlx)

REGISTER_SUBTEST('testInit Modified',testInitMod)
REGISTER_SUBTEST('testClear Modified',testClearMod)
REGISTER_SUBTEST('testSetInitial Modified',testSetMod)
REGISTER_SUBTEST('testReset Modified',testResetMod)
REGISTER_SUBTEST('testStep Modified',testStepMod)

#ifdef FUTILITY_HAVE_PETSC
!Reset everthing to test with different vector type
!Setup Acceleration parameter list
CALL pList%clear()
CALL pList%add('SolutionAccelerationType->N',2_SIK)
CALL pList%add('RelaxedPicardType->alpha',0.6_SRK)
CALL pList%add('SolutionAccelerationType->start',3)
CALL x%clear()
DEALLOCATE(x)
ALLOCATE(PETScVectorType :: x)
CALL vecparams%add('VectorType->MPI_Comm_ID',PE_COMM_SELF)
CALL vecparams%add('VectorType->nlocal',2_SIK)
CALL x%init(vecparams)

REGISTER_SUBTEST('testInit Relaxed (Distrib)',testInitRlx)
REGISTER_SUBTEST('testClear Relaxed (Distrib)',testClearRlx)
REGISTER_SUBTEST('testSetInitial Relaxed (Distrib)',testSetRlx)
REGISTER_SUBTEST('testReset Relaxed (Distrib)',testResetRlx)
REGISTER_SUBTEST('testStep Relaxed (Distrib)',testStepRlx)

REGISTER_SUBTEST('testInit Modified (Distrib)',testInitMod)
REGISTER_SUBTEST('testClear Modified (Distrib)',testClearMod)
REGISTER_SUBTEST('testSetInitial Modified (Distrib)',testSetMod)
REGISTER_SUBTEST('testReset Modified (Distrib)',testResetMod)
REGISTER_SUBTEST('testStep Modified (Distrib)',testStepMod)
#endif
!!!TODO: When support for TrilinosVectors and NativeDistributed are added, add testing for them

CALL x%clear()
DEALLOCATE(x)
CALL pList%clear()
CALL vecparams%clear()

FINALIZE_TEST()
CALL pe%clear()
CALL ce%clear()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testInitRlx()

  CALL testRlxPicard%init(ce,pList)
  ASSERT(testRlxPicard%isInit,'%isInit')
  ASSERT_EQ(testRlxPicard%s,0,'%iter')
  ASSERT_EQ(testRlxPicard%N,2,'%N')
  ASSERT_EQ(testRlxPicard%start,3,'%start')
  ASSERT_EQ(testRlxPicard%alpha,0.6_SRK,'%alpha')

ENDSUBROUTINE testInitRlx
!
!-------------------------------------------------------------------------------
SUBROUTINE testClearRlx()

  CALL testRlxPicard%clear()
  ASSERT(.NOT.testRlxPicard%isInit,'%isInit')
  ASSERT_EQ(testRlxPicard%s,0,'%s')
  ASSERT_EQ(testRlxPicard%N,-1,'%N')
  ASSERT_EQ(testRlxPicard%start,1,'%start')
  ASSERT_EQ(testRlxPicard%alpha,1.0_SRK,'%alpha')
  ASSERT(.NOT.ALLOCATED(testRlxPicard%x),'%x)')

ENDSUBROUTINE testClearRlx
!
!-------------------------------------------------------------------------------
SUBROUTINE testSetRlx()
  INTEGER(SIK) :: i
  REAL(SRK) :: tmp1, tmp2
  LOGICAL(SBK) :: bool

  CALL testRlxPicard%init(ce,pList)

  !Test initial set
  COMPONENT_TEST('Initial set')

  CALL x%set(1,1.0_SRK)
  CALL x%set(2,2.0_SRK)
  CALL testRlxPicard%setInitial(x)

  ASSERT(ALLOCATED(testRlxPicard%x),'%x not allocated')
  ASSERT_EQ(SIZE(testRlxPicard%x),1,'%x wrong number of vectors')
  ASSERT_EQ(testRlxPicard%x(1)%n,testRlxPicard%N,'%x wrong vector size')
  bool=.TRUE.
  DO i=1,x%n
    CALL x%get(i,tmp1)
    CALL testRlxPicard%x(1)%get(i,tmp2)
    IF(tmp1 /= tmp2) bool=.FALSE.
  ENDDO
  ASSERT(bool,'SetInitial failed to set vector')

  CALL testRlxPicard%clear()

ENDSUBROUTINE testSetRlx
!
!-------------------------------------------------------------------------------
SUBROUTINE testResetRlx()
  INTEGER(SIK) :: i
  REAL(SRK) :: tmp1, tmp2
  LOGICAL(SBK) :: bool

  CALL testRlxPicard%init(ce,pList)

  !Test initial set
  COMPONENT_TEST('Initial set')

  CALL x%set(1,1.0_SRK)
  CALL x%set(2,2.0_SRK)
  CALL testRlxPicard%reset(x)

  ASSERT(ALLOCATED(testRlxPicard%x),'%x not allocated')
  ASSERT_EQ(SIZE(testRlxPicard%x),1,'%x wrong number of vectors')
  ASSERT_EQ(testRlxPicard%x(1)%n,testRlxPicard%N,'%x wrong vector size')
  bool=.TRUE.
  DO i=1,x%n
    CALL x%get(i,tmp1)
    CALL testRlxPicard%x(1)%get(i,tmp2)
    IF(tmp1 /= tmp2) bool=.FALSE.
  ENDDO
  ASSERT(bool,'Reset failed to set vector')

  !Test subsequent reset
  COMPONENT_TEST('Subsequent Reset')

  CALL x%set(0.0_SRK)
  testRlxPicard%s=3_SIK
  CALL testRlxPicard%reset(x)

  bool=.TRUE.
  DO i=1,x%n
    CALL x%get(i,tmp1)
    CALL testRlxPicard%x(1)%get(i,tmp2)
    IF(tmp1 /= tmp2) bool=.FALSE.
  ENDDO
  ASSERT(bool,'Reset failed to reset vector')
  ASSERT_EQ(testRlxPicard%s,0_SIK,'%s incorrect iteration count')

  CALL testRlxPicard%clear()

ENDSUBROUTINE testResetRlx
!
!-------------------------------------------------------------------------------
SUBROUTINE testStepRlx()
  REAL(SRK) :: outval(2), solval(2), val(2)

  CALL testRlxPicard%init(ce,pList)

  val=(/1.0_SRK, 2.0_SRK/)
  CALL x%set(val)
  CALL testRlxPicard%setInitial(x)

  val=(/2.0_SRK, 3.0_SRK/)
  solval=val
  CALL x%set(val)
  CALL testRlxPicard%step(x)
  CALL x%get(outval)
  ASSERT_EQ(testRlxPicard%s,1,"%s")
  ASSERT(ALL(outval .APPROXEQ. solval),"return value step 1")
  FINFO() outval, ' ~= ', solval

  val=(/3.0_SRK, 4.0_SRK/)
  solval=val
  CALL x%set(val)
  CALL testRlxPicard%step(x)
  CALL x%get(outval)
  ASSERT_EQ(testRlxPicard%s,2,"%s")
  ASSERT(ALL(outval .APPROXEQ. solval),"return value step 2")
  FINFO() outval, ' ~= ', solval

  val=(/4.0_SRK, 5.0_SRK/)
  solval=(/3.6_SRK, 4.6_SRK/)
  CALL x%set(val)
  CALL testRlxPicard%step(x)
  CALL x%get(outval)
  ASSERT_EQ(testRlxPicard%s,3,"%s")
  ASSERT(ALL(outval .APPROXEQ. solval),"return value step 3")
  FINFO() outval, ' ~= ', solval

  val=(/5.0_SRK, 6.0_SRK/)
  solval=(/4.44_SRK, 5.44_SRK/)
  CALL x%set(val)
  CALL testRlxPicard%step(x)
  CALL x%get(outval)
  ASSERT_EQ(testRlxPicard%s,4,"%s")
  ASSERT(ALL(outval .APPROXEQ. solval),"return value step 4")
  FINFO() outval, ' ~= ', solval

  val=(/7.0_SRK, 8.0_SRK/)
  solval=(/5.976_SRK, 6.976_SRK/)
  CALL x%set(val)
  CALL testRlxPicard%step(x)
  CALL x%get(outval)
  ASSERT_EQ(testRlxPicard%s,5,"%s")
  ASSERT(ALL(outval .APPROXEQ. solval),"return value step 5")
  FINFO() outval, ' ~= ', solval

  !Final clear
  CALL testRlxPicard%clear()

ENDSUBROUTINE testStepRlx
!
!-------------------------------------------------------------------------------
SUBROUTINE testInitMod()

  CALL testModPicard%init(ce,pList)
  ASSERT(testModPicard%isInit,'%isInit')
  ASSERT_EQ(testModPicard%s,0,'%iter')
  ASSERT_EQ(testModPicard%N,2,'%N')
  ASSERT_EQ(testModPicard%start,3,'%start')
  ASSERT_EQ(testModPicard%alpha,0.6_SRK,'%alpha')

ENDSUBROUTINE testInitMod
!
!-------------------------------------------------------------------------------
SUBROUTINE testClearMod()

  CALL testModPicard%clear()
  ASSERT(.NOT.testModPicard%isInit,'%isInit')
  ASSERT_EQ(testModPicard%s,0,'%s')
  ASSERT_EQ(testModPicard%N,-1,'%N')
  ASSERT_EQ(testModPicard%start,1,'%start')
  ASSERT_EQ(testModPicard%alpha,1.0_SRK,'%alpha')
  ASSERT(.NOT.ALLOCATED(testModPicard%x),'%x)')
  ASSERT(.NOT.ALLOCATED(testModPicard%tmpvec),'%tmpvec)')

ENDSUBROUTINE testClearMod
!
!-------------------------------------------------------------------------------
SUBROUTINE testSetMod()
  INTEGER(SIK) :: i
  REAL(SRK) :: tmp1, tmp2
  LOGICAL(SBK) :: bool

  CALL testModPicard%init(ce,pList)

  !Test initial set
  COMPONENT_TEST('Initial set')

  CALL x%set(1,1.0_SRK)
  CALL x%set(2,2.0_SRK)
  CALL testModPicard%setInitial(x)

  ASSERT(ALLOCATED(testModPicard%x),'%x not allocated')
  ASSERT_EQ(SIZE(testModPicard%x),1,'%x wrong number of vectors')
  ASSERT_EQ(testModPicard%x(1)%n,testModPicard%N,'%x wrong vector size')
  bool=.TRUE.
  DO i=1,x%n
    CALL x%get(i,tmp1)
    CALL testModPicard%x(1)%get(i,tmp2)
    IF(tmp1 /= tmp2) bool=.FALSE.
  ENDDO
  ASSERT(bool,'SetInitial failed to set vector')
  ASSERT(ALLOCATED(testModPicard%tmpvec),'%tmpvec not allocated')
  ASSERT_EQ(testModPicard%tmpvec%n,testModPicard%N,'%tmpvec wrong vector size')

  CALL testModPicard%clear()

ENDSUBROUTINE testSetMod
!
!-------------------------------------------------------------------------------
SUBROUTINE testResetMod()
  INTEGER(SIK) :: i
  REAL(SRK) :: tmp1, tmp2
  LOGICAL(SBK) :: bool

  CALL testModPicard%init(ce,pList)

  !Test initial set
  COMPONENT_TEST('Initial set')

  CALL x%set(1,1.0_SRK)
  CALL x%set(2,2.0_SRK)
  CALL testModPicard%reset(x)

  ASSERT(ALLOCATED(testModPicard%x),'%x not allocated')
  ASSERT_EQ(SIZE(testModPicard%x),1,'%x wrong number of vectors')
  ASSERT_EQ(testModPicard%x(1)%n,testModPicard%N,'%x wrong vector size')
  bool=.TRUE.
  DO i=1,x%n
    CALL x%get(i,tmp1)
    CALL testModPicard%x(1)%get(i,tmp2)
    IF(tmp1 /= tmp2) bool=.FALSE.
  ENDDO
  ASSERT(bool,'Reset failed to set vector')
  ASSERT(ALLOCATED(testModPicard%tmpvec),'%tmpvec not allocated')
  ASSERT_EQ(testModPicard%tmpvec%n,testModPicard%N,'%tmpvec wrong vector size')

  !Test subsequent reset
  COMPONENT_TEST('Subsequent Reset')

  CALL x%set(0.0_SRK)
  testModPicard%s=3_SIK
  CALL testModPicard%reset(x)

  bool=.TRUE.
  DO i=1,x%n
    CALL x%get(i,tmp1)
    CALL testModPicard%x(1)%get(i,tmp2)
    IF(tmp1 /= tmp2) bool=.FALSE.
  ENDDO
  ASSERT(bool,'Reset failed to reset vector')
  ASSERT(ALLOCATED(testModPicard%x),'%x not allocated')
  ASSERT_EQ(SIZE(testModPicard%x),1,'%x wrong number of vectors')
  ASSERT_EQ(testModPicard%x(1)%n,testModPicard%N,'%x wrong vector size')
  ASSERT(ALLOCATED(testModPicard%tmpvec),'%tmpvec not allocated')
  ASSERT_EQ(testModPicard%tmpvec%n,testModPicard%N,'%tmpvec wrong vector size')

  ASSERT_EQ(testModPicard%s,0_SIK,'%s incorrect iteration count')

  CALL testModPicard%clear()

ENDSUBROUTINE testResetMod
!
!-------------------------------------------------------------------------------
SUBROUTINE testStepMod()
  REAL(SRK) :: outval(2), solval(2), val(2)

  CALL testModPicard%init(ce,pList)

  val=(/1.0_SRK, 2.0_SRK/)
  CALL x%set(val)
  CALL testModPicard%setInitial(x)

  val=(/2.0_SRK, 3.0_SRK/)
  solval=val
  CALL x%set(val)
  CALL testModPicard%step(x)
  CALL x%get(outval)
  ASSERT_EQ(testModPicard%s,1,"%s")
  ASSERT(ALL(outval .APPROXEQ. solval),"return value step 1")
  FINFO() outval, ' ~= ', solval

  val=(/3.0_SRK, 4.0_SRK/)
  solval=val
  CALL x%set(val)
  CALL testModPicard%step(x)
  CALL x%get(outval)
  ASSERT_EQ(testModPicard%s,2,"%s")
  ASSERT(ALL(outval .APPROXEQ. solval),"return value step 2")
  FINFO() outval, ' ~= ', solval

  val=(/4.0_SRK, 5.0_SRK/)
  solval=(/3.6_SRK, 4.6_SRK/)
  CALL x%set(val)
  CALL testModPicard%step(x)
  CALL x%get(outval)
  ASSERT_EQ(testModPicard%s,3,"%s")
  ASSERT(ALL(outval .APPROXEQ. solval),"return value step 3")
  FINFO() outval, ' ~= ', solval

  val=(/5.0_SRK, 6.0_SRK/)
  solval=(/4.6_SRK, 5.6_SRK/)
  CALL x%set(val)
  CALL testModPicard%step(x)
  CALL x%get(outval)
  ASSERT_EQ(testModPicard%s,4,"%s")
  ASSERT(ALL(outval .APPROXEQ. solval),"return value step 4")
  FINFO() outval, ' ~= ', solval

  val=(/7.0_SRK, 8.0_SRK/)
  solval=(/6.2_SRK, 7.2_SRK/)
  CALL x%set(val)
  CALL testModPicard%step(x)
  CALL x%get(outval)
  ASSERT_EQ(testModPicard%s,5,"%s")
  ASSERT(ALL(outval .APPROXEQ. solval),"return value step 5")
  FINFO() outval, ' ~= ', solval

  !Final clear
  CALL testModPicard%clear()

ENDSUBROUTINE testStepMod
!
ENDPROGRAM testPicard
