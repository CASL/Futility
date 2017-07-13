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
  TYPE(ParamType) :: pList, optList
  TYPE(AndersonAccelerationType) :: testAndAcc


  !> set up default parameter list
  CALL optList%clear()
  CALL optList%add('AndersonAccelerationType->n',5_SIK)
  CALL optList%add('AndersonAccelerationType->nlocal',5_SIK)
  CALL optList%add('AndersonAccelerationType->depth',2_SIK)
  CALL optList%add('AndersonAccelerationType->beta',0.5_SRK)

  !Configure exception handler for test
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL eParams%addSurrogate(e)
  CALL eAndersonAccelerationType%addSurrogate(e)
  CALL mpiTestEnv%init(PE_COMM_SELF)

  CREATE_TEST('Test Anderson Acceleration Solver')

  REGISTER_SUBTEST('testInit',testInit)
  REGISTER_SUBTEST('testStep',testStep)
  !REGISTER_SUBTEST('testReset',testReset)
  REGISTER_SUBTEST('testClear',testClear)
  REGISTER_SUBTEST('testStep beta=1',testStep_beta_1)
  !It appears anderson(0) doesn't work in trilinos which is too bad
  REGISTER_SUBTEST('testStep depth=0',testStep_depth_0)

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
      ASSERT(testAndAcc%iter==0,'%iter')
      ASSERT(testAndAcc%n==5,'%n')
      ASSERT(testAndAcc%depth==2,'%depth')
      ASSERT(testAndAcc%beta==0.5_SRK,'%beta')
      ASSERT(ASSOCIATED(testAndAcc%MPIparallelEnv),'%MPIenv')
#ifdef FUTILITY_HAVE_Trilinos
      !Can't test this without trilinos
      ASSERT(testAndAcc%X%isInit ,'%x')
#endif
    ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testStep()

#ifdef FUTILITY_HAVE_Trilinos
      INTEGER(SIK) :: i
      REAL(SRK) :: tmp
      !First call to step resets the solution
      CALL testAndAcc%X%set(1.0_SRK)
      CALL testAndAcc%step()
      ASSERT(testAndAcc%iter==1,'%iter')

      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        ASSERT(tmp==1.0_SRK,'%step(0)')
      ENDDO

      CALL testAndAcc%X%set(1,0.975_SRK)
      CALL testAndAcc%X%set(2,1.473_SRK)
      CALL testAndAcc%X%set(3,1.758_SRK)
      CALL testAndAcc%X%set(4,4.189_SRK)
      CALL testAndAcc%X%set(5,5.249_SRK)

      CALL testAndAcc%step()

      ! This one is easy, its just the average of the inputs and 1
      CALL testAndAcc%X%get(1,tmp)
      ASSERT(tmp==0.9875_SRK,'%step(1)')
      CALL testAndAcc%X%get(2,tmp)
      ASSERT(tmp==1.2365_SRK,'%step(1)')
      CALL testAndAcc%X%get(3,tmp)
      ASSERT(tmp==1.3790_SRK,'%step(1)')
      CALL testAndAcc%X%get(4,tmp)
      ASSERT(tmp==2.5945_SRK,'%step(1)')
      CALL testAndAcc%X%get(5,tmp)
      ASSERT(tmp==3.1245_SRK,'%step(1)')

      CALL testAndAcc%X%set(1,0.980_SRK)
      CALL testAndAcc%X%set(2,1.580_SRK)
      CALL testAndAcc%X%set(3,2.012_SRK)
      CALL testAndAcc%X%set(4,4.141_SRK)
      CALL testAndAcc%X%set(5,5.198_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(2)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,0.987_SRK)
      CALL testAndAcc%X%set(2,1.735_SRK)
      CALL testAndAcc%X%set(3,2.374_SRK)
      CALL testAndAcc%X%set(4,4.095_SRK)
      CALL testAndAcc%X%set(5,5.125_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(3)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,0.992_SRK)
      CALL testAndAcc%X%set(2,1.840_SRK)
      CALL testAndAcc%X%set(3,2.629_SRK)
      CALL testAndAcc%X%set(4,4.057_SRK)
      CALL testAndAcc%X%set(5,5.074_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(4)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,0.995_SRK)
      CALL testAndAcc%X%set(2,1.897_SRK)
      CALL testAndAcc%X%set(3,2.754_SRK)
      CALL testAndAcc%X%set(4,4.039_SRK)
      CALL testAndAcc%X%set(5,5.049_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(5)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,0.998_SRK)
      CALL testAndAcc%X%set(2,1.948_SRK)
      CALL testAndAcc%X%set(3,2.876_SRK)
      CALL testAndAcc%X%set(4,4.020_SRK)
      CALL testAndAcc%X%set(5,5.025_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(6)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,0.999_SRK)
      CALL testAndAcc%X%set(2,1.967_SRK)
      CALL testAndAcc%X%set(3,2.930_SRK)
      CALL testAndAcc%X%set(4,4.012_SRK)
      CALL testAndAcc%X%set(5,5.014_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(7)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,0.999_SRK)
      CALL testAndAcc%X%set(2,1.990_SRK)
      CALL testAndAcc%X%set(3,2.979_SRK)
      CALL testAndAcc%X%set(4,4.004_SRK)
      CALL testAndAcc%X%set(5,5.005_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(8)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,1.993_SRK)
      CALL testAndAcc%X%set(3,2.990_SRK)
      CALL testAndAcc%X%set(4,4.002_SRK)
      CALL testAndAcc%X%set(5,5.001_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(9)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,1.996_SRK)
      CALL testAndAcc%X%set(3,2.988_SRK)
      CALL testAndAcc%X%set(4,4.002_SRK)
      CALL testAndAcc%X%set(5,5.001_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(10)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,2.000_SRK)
      CALL testAndAcc%X%set(3,2.995_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,5.001_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(11)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,1.998_SRK)
      CALL testAndAcc%X%set(3,3.003_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,4.999_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(12)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,2.000_SRK)
      CALL testAndAcc%X%set(3,3.000_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,5.000_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(13)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,2.000_SRK)
      CALL testAndAcc%X%set(3,3.000_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,5.000_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(14)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,2.000_SRK)
      CALL testAndAcc%X%set(3,3.000_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,5.000_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(15)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,2.000_SRK)
      CALL testAndAcc%X%set(3,3.000_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,5.000_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(16)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,2.000_SRK)
      CALL testAndAcc%X%set(3,3.000_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,5.000_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(17)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,2.000_SRK)
      CALL testAndAcc%X%set(3,3.000_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,5.000_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(18)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,2.000_SRK)
      CALL testAndAcc%X%set(3,3.000_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,5.000_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(19)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,2.000_SRK)
      CALL testAndAcc%X%set(3,3.000_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,5.000_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(20)"
      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        ASSERT(SOFTEQ(tmp,REAL(i,SRK),1.0E-6_SRK),'%step(20)')
        FINFO() i, tmp-REAL(i,SRK)
      ENDDO
#endif
    ENDSUBROUTINE testStep
!
!-------------------------------------------------------------------------------
    SUBROUTINE testStep_beta_1()
#ifdef FUTILITY_HAVE_Trilinos
      INTEGER(SIK) :: i
      REAL(SRK) :: tmp
#endif

      CALL optList%set('AndersonAccelerationType->beta',1.0_SRK)
      CALL testAndAcc%init(mpiTestEnv,optList)
#ifdef FUTILITY_HAVE_Trilinos
      CALL testAndAcc%X%set(1.0_SRK)
      CALL testAndAcc%step()

      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        WRITE(*,*) i, tmp
      ENDDO

      CALL testAndAcc%X%set(1,0.975_SRK)
      CALL testAndAcc%X%set(2,1.473_SRK)
      CALL testAndAcc%X%set(3,1.758_SRK)
      CALL testAndAcc%X%set(4,4.189_SRK)
      CALL testAndAcc%X%set(5,5.249_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(1)"

      CALL testAndAcc%X%set(1,0.980_SRK)
      CALL testAndAcc%X%set(2,1.580_SRK)
      CALL testAndAcc%X%set(3,2.012_SRK)
      CALL testAndAcc%X%set(4,4.141_SRK)
      CALL testAndAcc%X%set(5,5.198_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(2)"

      CALL testAndAcc%X%set(1,0.987_SRK)
      CALL testAndAcc%X%set(2,1.735_SRK)
      CALL testAndAcc%X%set(3,2.374_SRK)
      CALL testAndAcc%X%set(4,4.095_SRK)
      CALL testAndAcc%X%set(5,5.125_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(3)"

      CALL testAndAcc%X%set(1,0.992_SRK)
      CALL testAndAcc%X%set(2,1.840_SRK)
      CALL testAndAcc%X%set(3,2.629_SRK)
      CALL testAndAcc%X%set(4,4.057_SRK)
      CALL testAndAcc%X%set(5,5.074_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(4)"

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,2.000_SRK)
      CALL testAndAcc%X%set(3,3.000_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,5.000_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(5)"

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,2.000_SRK)
      CALL testAndAcc%X%set(3,3.000_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,5.000_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(6)"

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,2.000_SRK)
      CALL testAndAcc%X%set(3,3.000_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,5.000_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(7)"

      CALL testAndAcc%X%set(1,1.000_SRK)
      CALL testAndAcc%X%set(2,2.000_SRK)
      CALL testAndAcc%X%set(3,3.000_SRK)
      CALL testAndAcc%X%set(4,4.000_SRK)
      CALL testAndAcc%X%set(5,5.000_SRK)

      CALL testAndAcc%step()

      DO i=1,5
        CALL testAndAcc%X%get(i,tmp)
        ASSERT(SOFTEQ(tmp,REAL(i,SRK),1.0E-6_SRK),'%step(20)')
        FINFO() i, tmp-REAL(i,SRK)
      ENDDO
#endif
      CALL testAndAcc%clear()
    ENDSUBROUTINE testStep_beta_1
!
!-------------------------------------------------------------------------------
    SUBROUTINE testStep_depth_0()
#ifdef FUTILITY_HAVE_Trilinos
      REAL(SRK) :: tmp
#endif

      CALL optList%set('AndersonAccelerationType->beta',0.5_SRK)
      CALL optList%set('AndersonAccelerationType->depth',0_SIK)
      CALL testAndAcc%init(mpiTestEnv,optList)
#ifdef FUTILITY_HAVE_Trilinos
      CALL testAndAcc%X%set(1.0_SRK)
      CALL testAndAcc%step()

      CALL testAndAcc%X%set(1,0.975_SRK)
      CALL testAndAcc%X%set(2,1.473_SRK)
      CALL testAndAcc%X%set(3,1.758_SRK)
      CALL testAndAcc%X%set(4,4.189_SRK)
      CALL testAndAcc%X%set(5,5.249_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(1)"
      CALL testAndAcc%X%get(1,tmp)
      ASSERT(SOFTEQ(tmp,0.9875_SRK,1.0E-12_SRK),'%step(1)')
      CALL testAndAcc%X%get(2,tmp)
      ASSERT(SOFTEQ(tmp,1.2365_SRK,1.0E-12_SRK),'%step(1)')
      CALL testAndAcc%X%get(3,tmp)
      ASSERT(SOFTEQ(tmp,1.3790_SRK,1.0E-12_SRK),'%step(1)')
      CALL testAndAcc%X%get(4,tmp)
      ASSERT(SOFTEQ(tmp,2.5945_SRK,1.0E-12_SRK),'%step(1)')
      CALL testAndAcc%X%get(5,tmp)
      ASSERT(SOFTEQ(tmp,3.1245_SRK,1.0E-12_SRK),'%step(1)')

      CALL testAndAcc%X%set(1,0.980_SRK)
      CALL testAndAcc%X%set(2,1.580_SRK)
      CALL testAndAcc%X%set(3,2.012_SRK)
      CALL testAndAcc%X%set(4,4.141_SRK)
      CALL testAndAcc%X%set(5,5.198_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(2)"
      CALL testAndAcc%X%get(1,tmp)
      ASSERT(SOFTEQ(tmp,0.98375000_SRK,1.0E-12_SRK),'%step(2)')
      CALL testAndAcc%X%get(2,tmp)
      ASSERT(SOFTEQ(tmp,1.40825000_SRK,1.0E-12_SRK),'%step(2)')
      CALL testAndAcc%X%get(3,tmp)
      ASSERT(SOFTEQ(tmp,1.69550000_SRK,1.0E-12_SRK),'%step(2)')
      CALL testAndAcc%X%get(4,tmp)
      ASSERT(SOFTEQ(tmp,3.36775000_SRK,1.0E-12_SRK),'%step(2)')
      CALL testAndAcc%X%get(5,tmp)
      ASSERT(SOFTEQ(tmp,4.16125000_SRK,1.0E-12_SRK),'%step(2)')

      CALL testAndAcc%X%set(1,0.987_SRK)
      CALL testAndAcc%X%set(2,1.735_SRK)
      CALL testAndAcc%X%set(3,2.374_SRK)
      CALL testAndAcc%X%set(4,4.095_SRK)
      CALL testAndAcc%X%set(5,5.125_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(3)"
      CALL testAndAcc%X%get(1,tmp)
      ASSERT(SOFTEQ(tmp,0.98537500_SRK,1.0E-12_SRK),'%step(3)')
      CALL testAndAcc%X%get(2,tmp)
      ASSERT(SOFTEQ(tmp,1.57162500_SRK,1.0E-12_SRK),'%step(3)')
      CALL testAndAcc%X%get(3,tmp)
      ASSERT(SOFTEQ(tmp,2.03475000_SRK,1.0E-12_SRK),'%step(3)')
      CALL testAndAcc%X%get(4,tmp)
      ASSERT(SOFTEQ(tmp,3.73137500_SRK,1.0E-12_SRK),'%step(3)')
      CALL testAndAcc%X%get(5,tmp)
      ASSERT(SOFTEQ(tmp,4.64312500_SRK,1.0E-12_SRK),'%step(3)')

      CALL testAndAcc%X%set(1,0.992_SRK)
      CALL testAndAcc%X%set(2,1.840_SRK)
      CALL testAndAcc%X%set(3,2.629_SRK)
      CALL testAndAcc%X%set(4,4.057_SRK)
      CALL testAndAcc%X%set(5,5.074_SRK)

      CALL testAndAcc%step()

      WRITE(*,*) "step(4)"
      CALL testAndAcc%X%get(1,tmp)
      ASSERT(SOFTEQ(tmp,0.98868750_SRK,1.0E-12_SRK),'%step(4)')
      CALL testAndAcc%X%get(2,tmp)
      ASSERT(SOFTEQ(tmp,1.70581250_SRK,1.0E-12_SRK),'%step(4)')
      CALL testAndAcc%X%get(3,tmp)
      ASSERT(SOFTEQ(tmp,2.33187500_SRK,1.0E-12_SRK),'%step(4)')
      CALL testAndAcc%X%get(4,tmp)
      ASSERT(SOFTEQ(tmp,3.89418750_SRK,1.0E-12_SRK),'%step(4)')
      CALL testAndAcc%X%get(5,tmp)
      ASSERT(SOFTEQ(tmp,4.85856250_SRK,1.0E-12_SRK),'%step(4)')
#endif
      CALL testAndAcc%clear()
    ENDSUBROUTINE testStep_depth_0
!
!-------------------------------------------------------------------------------
    SUBROUTINE testClear()
      CALL testAndAcc%clear()
      ASSERT(.NOT. testAndAcc%isInit,'%isInit')
      ASSERT(.NOT. ASSOCIATED(testAndAcc%MPIparallelEnv),'%MPIenv')
      ASSERT(.NOT. testAndAcc%OMPparallelEnv%isInit(),'%OMPenv')
      ASSERT(testAndAcc%n==-1,'%n')
      ASSERT(testAndAcc%depth==-1,'%depth')
      ASSERT(testAndAcc%beta==0.0_SRK,'%beta')
      IF(ASSOCIATED(testAndAcc%X)) THEN
        ASSERT(.NOT. (testAndAcc%X%isInit) ,'%x')
      ENDIF
    ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
!
ENDPROGRAM testAndersonAcceleration
