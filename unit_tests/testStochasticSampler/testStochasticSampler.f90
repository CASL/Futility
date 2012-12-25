!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                              Copyright (C) 2012                              !
!                   The Regents of the University of Michigan                  !
!              MPACT Development Group and Prof. Thomas J. Downar              !
!                             All rights reserved.                             !
!                                                                              !
! Copyright is reserved to the University of Michigan for purposes of          !
! controlled dissemination, commercialization through formal licensing, or     !
! other disposition. The University of Michigan nor any of their employees,    !
! makes any warranty, express or implied, or assumes any liability or          !
! responsibility for the accuracy, completeness, or usefulness of any          !
! information, apparatus, product, or process disclosed, or represents that    !
! its use would not infringe privately owned rights. Reference herein to any   !
! specific commercial products, process, or service by trade name, trademark,  !
! manufacturer, or otherwise, DOes not necessarily constitute or imply its     !
! enDOrsement, recommendation, or favoring by the University of Michigan.      !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testStochasticSampler
#include "UnitTest.h"
  USE UnitTest    
  USE IntrType
  USE ExceptionHandler
  USE ParallelEnv
  USE StochasticSampling
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE

  TYPE(StochasticSamplingType) :: myRNG
  TYPE(ExceptionHandlerType),POINTER :: e => NULL()
  TYPE(MPI_EnvType) :: MPIEnv
  TYPE(OMP_EnvType) :: OMPEnv
  
  INTERFACE
    FUNCTION linear(x)
      IMPORT :: SDK
      REAL(SDK),INTENT(IN) :: x
      REAL(SDK) :: linear
    ENDFUNCTION
  ENDINTERFACE

  INTERFACE
    FUNCTION lineararg(x,arg)
      IMPORT :: SDK
      REAL(SDK),INTENT(IN) :: x
      REAL(SDK),INTENT(IN) :: arg(:)
      REAL(SDK) :: lineararg
    ENDFUNCTION
  ENDINTERFACE

  INTERFACE
    FUNCTION quadfunc(x)
      IMPORT :: SDK
      REAL(SDK),INTENT(IN) :: x
      REAL(SDK) :: quadfunc
    ENDFUNCTION
  ENDINTERFACE
  
  CREATE_TEST("StochasticSampler")

  REGISTER_SUBTEST('Initialize',TestInit)
  REGISTER_SUBTEST('RNG Performance',TestRNG)
  REGISTER_SUBTEST('Uniform Dist',TestUniform)
  REGISTER_SUBTEST('Exp Dist',TestExp)
  REGISTER_SUBTEST('Normal Dist',TestNormal)
  REGISTER_SUBTEST('Maxwellian Dist',TestMaxwellian)
  REGISTER_SUBTEST('Watt Dist',TestWatt)
  REGISTER_SUBTEST('Evap Dist',TestEvap)
  REGISTER_SUBTEST('Norm Histogram',TestNormHist)
  REGISTER_SUBTEST('Unnorm Hist',TestUnnormHist)
  REGISTER_SUBTEST('Cont Hist',TestNormContHist)
  REGISTER_SUBTEST('UnnormCont Hist',TestUnnormContHist)
  REGISTER_SUBTEST('Norm PWL Dist',TestNormPWLinear)
  REGISTER_SUBTEST('Unnorm PWL Dist',TestUnnormPWLinear)
  REGISTER_SUBTEST('Rejection',TestRejection)
  REGISTER_SUBTEST('Rejct w Arg',TestRejectionArg)
  REGISTER_SUBTEST('Clear',TestClear)

  CALL MPIEnv%finalize()
  CALL myRNG%clear()
  
  FINALIZE_TEST() 
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestInit
      TYPE(StochasticSamplingType) :: myRNG2
      TYPE(StochasticSamplingType) :: myRNG3
      INTEGER :: i
      INTEGER(SLK) :: firstten(11)
      REAL(SDK) :: x
      
      ALLOCATE(e)
      CALL e%setQuietMode(.TRUE.)
      eStochasticSampler => e
      ! Test Manager Init
      CALL myRNG%init(3)
      SET_PREFIX('RNG Init')
      ! Test Initialize
      ASSERT(myRNG%isInit,'isInit')
      ASSERT(myRNG%counter==0,'counter')
      ASSERT(myRNG%RNmult==2806196910506780709_SLK,'mult')
      ASSERT(myRNG%RNadd==1_SLK,'add')
      ASSERT(myRNG%RNmask==2_SLK**63-1_SLK,'mask')
      ASSERT(myRNG%RNmod==2_SLK**63,'mod')
      ASSERT(myRNG%RNnorm==1.0_SDK/2.0_SDK**63,'norm')

      firstten(1)=1_SLK
      firstten(2)=2806196910506780710_SLK
      firstten(3)=6924308458965941631_SLK
      firstten(4)=7093833571386932060_SLK
      firstten(5)=4133560638274335821_SLK
      firstten(6)= 678653069250352930_SLK
      firstten(7)= 682142755826100203_SLK
      firstten(8)=8445772606699617528_SLK
      firstten(9)=4656810573858967513_SLK
      firstten(10)=5251754594274751070_SLK
      firstten(11)=2039497501229545367_SLK
      
      DO i=1,11
        ASSERT(myRNG%RNseed==firstten(i),'RNG did not reproduce first 10 random numbers')
        FINFO() 'Failed at random number: ', i
        x=myRNG%rng()
      ENDDO
      
      DO i=1,100
        x=myRNG%rng()
      ENDDO
      
      CALL myRNG2%init(3,skip=myRNG%counter)
      ASSERT(myRNG%rng()==myRNG2%rng(),'RNG did not skip ahead properly.')
      FINFO() "RNG 1:         ", myRNG%rng()
      FINFO() "RNG 2 Skipped: ", myRNG2%rng()
      CALL myRNG2%clear()

      !  Set up case wehre parallel env is not initialized
      CALL myRNG2%init(3,MPIparallelEnv=MPIEnv,OMPparallelEnv=OMPEnv)
      CALL myRNG3%init(3)
      ASSERT(myRNG2%RNseed==myRNG3%RNseed,'RNG did not handle uninitialized parallel env properly.')
      CALL myRNG2%clear()
      CALL myRNG3%clear()
      
      
      ! Set up parallel environment for initialization test
      !   Initialize null MPI env then sets it to appear as 100 processors and of rank 22
      CALL MPIEnv%init(PE_COMM_SELF)
      MPIEnv%nproc=100
      MPIEnv%rank=22
      
      CALL myRNG2%init(3,MPIparallelEnv=MPIEnv)
      
      ! Set up parallel environment for initialization test
      !   Initialize null MPI env then sets it to appear as 10 processors and of rank 2
      MPIEnv%nproc=10
      MPIEnv%rank=2
      ! Set up parallel environment for initialization test
      !   Initialize null MPI env then sets it to appear as 10 processors and of rank 2
      CALL OMPEnv%init(1)
      OMPEnv%nproc=10
      OMPEnv%rank=2
      CALL myRNG3%init(3,MPIparallelEnv=MPIEnv,OMPparallelEnv=OMPEnv)
      
      ASSERT(myRNG2%RNseed==myRNG3%RNseed,'RNG Parallel Decomposition did not initialize properly.')
      
      CALL myRNG2%clear()
      CALL myRNG3%clear()
    ENDSUBROUTINE TestInit
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestClear
      CALL myRNG%clear()
      
      SET_PREFIX('RNG clear')
      ASSERT(.NOT. myRNG%isInit,'isInit')
      ASSERT(myRNG%RNseed==-1,'RNseed')
      ASSERT(myRNG%RNseed==-1,'RNseed')
      ASSERT(myRNG%RNmult==-1,'RNmult')
      ASSERT(myRNG%RNadd==-1,'RNadd')
      ASSERT(myRNG%RNmask==-1,'RNmask')
      ASSERT(myRNG%RNmod==-1,'RNmod')
      ASSERT(myRNG%RNnorm==-1,'RNnorm')
      ASSERT(myRNG%counter==0,'counter')

    ENDSUBROUTINE TestClear
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestRNG
      INTEGER(SLK) :: i,n, inicount
      REAL(SDK) :: x, mean, stdev, truemean, truestd, tol
      
      n=1e8
      
      inicount=myRNG%counter
      mean=0.0
      stdev=0.0
      truemean=0.5_SDK
      truestd=1.0_SDK/SQRT(12.0_SDK)
      tol=5.0_SDK/SQRT(REAL(n,SDK))

      DO i=1,n
        x=myRNG%rng()
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==n,'Check counter increment.')      
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)
 
    ENDSUBROUTINE TestRNG
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestUniform
      INTEGER(SLK) :: i,n,inicount
      REAL(SDK) :: x,mean,stdev, truemean, truestd, tol

      n=1e6

      inicount=myRNG%counter
      mean=0.0
      stdev=0.0
      truemean=0.5_SDK
      truestd=1.0_SDK/SQRT(12.0_SDK)
      tol=5.0_SDK/SQRT(REAL(n,SDK))

      DO i=1,n
        x=myRNG%rng()
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

      inicount=myRNG%counter
      mean=0.0
      stdev=0.0
      truemean=(9.0_SDK+ (-8.0_SDK))/2.0_SDK
      truestd=(9.0_SDK- (-8.0_SDK))/SQRT(12.0)

      DO i=1,n
        x=myRNG%uniform(-8.0_SDK,9.0_SDK)
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

    ENDSUBROUTINE TestUniform
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestExp
      INTEGER(SLK) :: i,n,inicount
      REAL(SDK) :: x,mean,stdev, truemean, truestd, tol
      n=1e6

      inicount=myRNG%counter
      mean=0.0
      stdev=0.0
      truemean=1.0_SDK/2.0_SDK
      truestd=1.0_SDK/2.0_SDK
      tol=5.0_SDK/SQRT(REAL(n,SDK))

      DO i=1,n
        x=myRNG%exponential(2.0_SDK)
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

    ENDSUBROUTINE TestExp
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestNormal
      INTEGER(SLK) :: i,n,inicount
      REAL(SDK) :: x,mean,stdev, truemean, truestd, tol
      n=1e6

      COMPONENT_TEST('Normal Distribution')
      inicount=myRNG%counter
      mean=0.0
      stdev=0.0
      truemean=2.0_SDK
      truestd=0.5_SDK
      tol=5.0_SDK/SQRT(REAL(n,SDK))

      DO i=1,n
        x=myRNG%normal(2.0_SDK,0.5_SDK)
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==2*n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)
    
      COMPONENT_TEST('Log-Normal Distribution')
      inicount=myRNG%counter
      mean=0.0
      stdev=0.0
      truemean=EXP(2.0_SDK+0.5_SDK**2/2.0_SDK)
      truestd=SQRT((EXP(0.5_SDK**2)-1.0_SDK)*EXP(2.0_SDK*2.0_SDK+0.5_SDK**2))

      DO i=1,n
        x=myRNG%lognormal(2.0_SDK,0.5_SDK)
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==2*n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

    ENDSUBROUTINE TestNormal
!
!-------------------------------------------------------------------------------   
    SUBROUTINE TestMaxwellian
      INTEGER(SLK) :: i,n,inicount
      REAL(SDK) :: x,mean,stdev,T, truemean, truestd, tol
      n=1e6

      inicount=myRNG%counter
      T=600.0_SDK
      mean=0.0
      stdev=0.0
      truemean=3.0_SDK/2.0_SDK*T
      truestd=SQRT(3.0_SDK/2.0_SDK)*T
      tol=5.0_SDK/SQRT(REAL(n,SDK))

      DO i=1,n
        x=myRNG%maxwellian(T)
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==3*n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)/truemean<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)/truestd<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

    ENDSUBROUTINE TestMaxwellian
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestWatt
      INTEGER(SLK) :: i,n, inicount
      REAL(SDK) :: x, mean, stdev, a, b, truemean, truestd, tol
      
      n=1e6

      inicount=myRNG%counter
      a=2.0_SDK
      b=0.5_SDK
      truemean=a**2.5*SQRT(b)*(6.0_SDK+a*b)/(4.0_SDK*SQRT(a**3*b))
      truestd=SQRT((a**2*(-36.0*SQRT(a**3*b)+a*SQRT(b)*(12.0+a*b)* &
            (4.0*SQRT(a)+a**1.5*b-SQRT(b)*SQRT(a**3*b))))/SQRT(a**3*b))/(2.0*SQRT(2.0))
      tol=5.0_SDK/SQRT(REAL(n,SDK))
      
      mean=0.0
      stdev=0.0
      DO i=1,n
        x=myRNG%watt(a,b)
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==4*n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

    ENDSUBROUTINE TestWatt
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestEvap
      INTEGER(SLK) :: i,n, inicount
      REAL(SDK) :: x, theta, mean, stdev, truemean, truestd, tol
      
      n=1e6

      inicount=myRNG%counter
      theta=0.8_SDK
      truemean=8.0_SDK/5.0_SDK
      truestd=SQRT(32.0_SDK)/5.0_SDK
      tol=5.0_SDK/SQRT(REAL(n,SDK))
      
      mean=0.0
      stdev=0.0
      DO i=1,n
        x=myRNG%evaporation(theta)
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==2*n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

    ENDSUBROUTINE TestEvap
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestNormHist
      INTEGER(SLK) :: i,j,n, inicount
      REAL(SDK) :: mean, stdev, truemean, truestd, tol
      REAL(SRK),ALLOCATABLE :: y(:), iii(:)
      ALLOCATE(y(5))
      ALLOCATE(iii(5))
      n=1e6
  
      inicount=myRNG%counter
      y=(/ 0.2, 0.4, 0.1, 0.05, 0.25 /)
      mean=0.0_SDK
      stdev=0.0_SDK
      iii=0.0_SDK
      truemean=2.75_SRK
      truestd=SQRT(9.75_SRK-2.75_SRK**2)
      tol=5.0_SDK/SQRT(REAL(n,SDK))
      
      DO i=1,n
        j=myRNG%histogram(y)
        iii(j)=iii(j)+1
        mean=mean+REAL(j,SDK)/REAL(n,SDK)
        stdev=stdev+REAL(j,SDK)**2/REAL(n,SDK)
      ENDDO

      DO i=1,5
        ASSERT(ABS(REAL(iii(i))/REAL(n,SDK)-y(i))<tol,'RNG histogram bin test.')
        FINFO() i,REAL(iii(i))/REAL(n,SDK),y(i)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

      DEALLOCATE(y)
      DEALLOCATE(iii)
    ENDSUBROUTINE TestNormHist
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestUnnormHist
      INTEGER(SLK) :: i,j,n, inicount
      REAL(SDK) :: mean, stdev, truemean, truestd, tol
      REAL(SRK),ALLOCATABLE :: y(:), iii(:)
      ALLOCATE(y(5))
      ALLOCATE(iii(5))
      n=1e6
  
      inicount=myRNG%counter
      y=(/ 2.0, 4.0, 1.0, 0.5, 2.5 /)
      iii=0
      mean=0.0_SDK
      stdev=0.0_SDK
      truemean=2.75_SRK
      truestd=SQRT(9.75_SRK-2.75_SRK**2)
      tol=5.0_SDK/SQRT(REAL(n,SDK))
      
      DO i=1,n
        j=myRNG%unnormhistogram(y)
        iii(j)=iii(j)+1
        mean=mean+REAL(j,SDK)/REAL(n,SDK)
        stdev=stdev+REAL(j,SDK)**2/REAL(n,SDK)
      ENDDO

      DO i=1,5
        ASSERT(ABS(REAL(iii(i))/REAL(n,SDK)-y(i)/SUM(y))<tol,'RNG histogram bin test.')
        FINFO() i,REAL(iii(i))/REAL(n,SDK),y(i)/SUM(y)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

      DEALLOCATE(y)
      DEALLOCATE(iii)
    ENDSUBROUTINE TestUnnormHist
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestNormContHist
      INTEGER(SLK) :: i,n, inicount
      REAL(SDK) :: x, mean, stdev, truemean, truestd, tol
      REAL(SRK),ALLOCATABLE :: y(:), z(:)
      ALLOCATE(y(5))
      ALLOCATE(z(6))
      n=1e6

      inicount=myRNG%counter
      y=(/ 0.2, 0.4, 0.1, 0.05, 0.25 /)
      z=(/ 1.0, 2.0, 3.0, 4.0, 5.0, 7.0 /)
      mean=0.0_SDK
      stdev=0.0_SDK
      truemean=3.375_SRK
      truestd=SQRT(43.0_SRK/3.0_SRK-3.375_SRK**2)
      tol=5.0_SDK/SQRT(REAL(n,SDK))
      
      DO i=1,n
        x=myRNG%conthistogram(y,z)
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==2*n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

      DEALLOCATE(y)
      DEALLOCATE(z)
    ENDSUBROUTINE TestNormContHist    
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestUnnormContHist
      INTEGER(SLK) :: i,n, inicount
      REAL(SDK) :: x, mean, stdev, truemean, truestd, tol
      REAL(SRK),ALLOCATABLE :: y(:), z(:)
      ALLOCATE(y(5))
      ALLOCATE(z(6))
      n=1e6

      inicount=myRNG%counter
      y=(/ 2.0, 4.0, 1.0, 0.5, 2.5 /)
      z=(/ 1.0, 2.0, 3.0, 4.0, 5.0, 7.0 /)
      mean=0.0_SDK
      stdev=0.0_SDK
      truemean=3.375_SRK
      truestd=SQRT(43.0_SRK/3.0_SRK-3.375_SRK**2)
      tol=5.0_SDK/SQRT(REAL(n,SDK))
      
      DO i=1,n
        x=myRNG%unnormconthistogram(y,z)
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==2*n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

      DEALLOCATE(y)
      DEALLOCATE(z)
    ENDSUBROUTINE TestUnnormContHist  
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestNormPWLinear
      INTEGER(SLK) :: i,n, inicount
      REAL(SDK) :: x, mean, stdev, truemean, truestd, tol
      REAL(SRK),ALLOCATABLE :: y(:), z(:)
      ALLOCATE(y(3))
      ALLOCATE(z(3))
      n=1e6

      inicount=myRNG%counter
      y=(/ 0.0, 1.0, 0.0 /)
      z=(/ 0.0, 1.0, 2.0 /)
      mean=0.0
      stdev=0.0
      truemean=1.0_SDK
      truestd=1.0_SDK/SQRT(6.0_SDK)
      tol=5.0_SDK/SQRT(REAL(n,SDK))
      
      DO i=1,n
        x=myRNG%pwlinear(y,z)
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==2*n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

      DEALLOCATE(y)
      DEALLOCATE(z)
    ENDSUBROUTINE TestNormPWLinear     
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestUnnormPWLinear
      INTEGER(SLK) :: i,n, inicount
      REAL(SDK) :: x, mean, stdev, truemean, truestd, tol
      REAL(SRK),ALLOCATABLE :: y(:), z(:)
      ALLOCATE(y(3))
      ALLOCATE(z(3))
      n=1e6

      inicount=myRNG%counter
      y=(/ 0.0, 2.0, 0.0 /)
      z=(/ 0.0, 1.0, 3.0 /)
      mean=0.0
      stdev=0.0
      truemean=4.0_SDK/3.0_SDK
      truestd=SQRT(7.0_SDK)/SQRT(18.0_SDK)
      tol=5.0_SDK/SQRT(REAL(n,SDK))
      
      DO i=1,n
        x=myRNG%unnormpwlinear(y,z)
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount==2*n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

      DEALLOCATE(y)
      DEALLOCATE(z)
    ENDSUBROUTINE TestUnnormPWLinear    
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestRejection
      INTEGER(SLK) :: i,n, inicount
      REAL(SDK) :: x, mean, stdev, truemean, truestd, tol
      REAL(SRK),ALLOCATABLE :: y(:), z(:)
      ALLOCATE(y(3))
      ALLOCATE(z(3))
      n=1e6

      inicount=myRNG%counter
      y=(/ 0.0, 2.0, 0.0 /)
      z=(/ 0.0, 1.0, 3.0 /)
      mean=0.0
      stdev=0.0
      truemean=10.0_SDK/3.0_SDK
      truestd=5.0_SDK/SQRT(18.0_SDK)
      tol=5.0_SDK/SQRT(REAL(n,SDK))
      
      DO i=1,n
        x=myRNG%rejection(linear,0.0_SDK,5.0_SDK,7.0_SDK)
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount>n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

    ENDSUBROUTINE TestRejection
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestRejectionArg
      INTEGER(SLK) :: i,n, inicount
      REAL(SDK) :: x, mean, stdev, truemean, truestd, tol
      REAL(SRK),ALLOCATABLE :: y(:), z(:)
      ALLOCATE(y(3))
      ALLOCATE(z(3))
      n=1e6

      inicount=myRNG%counter
      y=(/ 0.0, 2.0, 0.0 /)
      z=(/ 0.0, 1.0, 3.0 /)
      mean=0.0
      stdev=0.0
      truemean=11.0_SDK/9.0_SDK
      truestd=SQRT(23.0_SDK)/9.0_SDK
      tol=5.0_SDK/SQRT(REAL(n,SDK))
      
      DO i=1,n
        x=myRNG%rejectionarg(lineararg,0.0_SDK,2.0_SDK,7.0_SDK,(/2.0_SDK,1.0_SDK/))
        mean=mean+x/REAL(n,SDK)
        stdev=stdev+x**2/REAL(n,SDK)
      ENDDO
      stdev=SQRT(stdev-mean**2)

      ASSERT(myRNG%counter-inicount>n,'Check counter increment.')
      ASSERT(ABS(mean-truemean)<tol,'RNG mean does not meet criteria')
      FINFO() mean,truemean,ABS(mean-truemean)
      ASSERT(ABS(stdev-truestd)<tol,'RNG standard deviation does not meet criteria')
      FINFO() stdev,truestd,ABS(stdev-truestd)

    ENDSUBROUTINE TestRejectionArg
!
ENDPROGRAM testStochasticSampler
!
!-------------------------------------------------------------------------------
FUNCTION linear(x) RESULT(y)
  USE IntrType
  REAL(SDK),INTENT(IN) :: x
  REAL(SDK) :: y
      
  y=x
ENDFUNCTION
!
!-------------------------------------------------------------------------------
FUNCTION lineararg(x,arg) RESULT(y)
  USE IntrType
  REAL(SDK),INTENT(IN) :: x
  REAL(SDK),INTENT(IN) :: arg(:)
  REAL(SDK) :: y
  y=arg(1)*x+arg(2)
ENDFUNCTION lineararg
!
!-------------------------------------------------------------------------------
FUNCTION quadfunc(x) RESULT(y)
  USE IntrType
  REAL(SDK),INTENT(IN) :: x
  REAL(SDK) :: y
      
  y=x**2+x+1.0_SDK
ENDFUNCTION
