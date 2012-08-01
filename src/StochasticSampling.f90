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
! manufacturer, or otherwise, does not necessarily constitute or imply its     !
! endorsement, recommendation, or favoring by the University of Michigan.      !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module for performing stochastic sampling.
!>
!> This package contains a Linear Congruential Random Number Generator that is
!> contained in MCNP.  The LCRNG format is common and could easily be extended
!> to have other LCRNGs.  The LCRNG is initialized by providing the routine with
!> an initial seed.  In addition to the LCRNG, several statistical distribution
!> functions have been created to allow for the sampling of non-uniform
!> distrubitions.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>
!> @par EXAMPLES
!> @code
!> PROGRAM ExampleRNG
!>   TYPE(StochasticSamplingType) :: myRNG
!>   INTEGER(SIK) :: randint
!>   REAL(SDK) :: randvar
!>
!>   CALL myRNG%init(5_SLK**19)
!>   randvar = myRNG%rng()
!>   randvar = myRNG%uniform(-1.0,1.0)
!>   randvar = myRNG%exponential(0.75)
!>   randvar = myRNG%normal(0.0,1.0)
!>   randint = myRNG%histogram( (/ 0.2, 0.5, 0.1, 0.3 /) )
!> ENDPROGRAM ExampleRNG
!> @endcode
!>
!> @author Ben Collins
!>   @date 04/26/2012
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE StochasticSampling

  USE IntrType
  USE ExceptionHandler
  IMPLICIT NONE
  PRIVATE
!
! List of public members
  PUBLIC :: StochasticManagerType
  PUBLIC :: StochasticSamplingType
  
  !> Maximum length for the name of a random number generator
  INTEGER(SIK),PARAMETER :: MAX_RNG_NAME_LEN=8
  
  !> Pi
  REAL(SRK),PARAMETER,PRIVATE :: PI=3.141592653589793_SRK
  
  !> Add description
  TYPE :: RNGdataType
    !> Random Number Data
    INTEGER(SLK) :: RNmult=-1
    !> Random Number Additive Term
    INTEGER(SLK) :: RNadd=-1
    !> log2 of Modulus, must be < 64
    INTEGER(SIK) :: RNlog2mod=-1
    !> Random Number Stride
    INTEGER(SLK) :: RNstride=-1
    !> Random Number Initial Seed
    INTEGER(SLK) :: RNseed0=-1
    !> Random Number Name
    CHARACTER(LEN=MAX_RNG_NAME_LEN) :: name=''
  ENDTYPE
  
  !> Add description
  INTEGER(SIK),PRIVATE,PARAMETER :: nRN=4
  
  !> Add description
  TYPE(RNGdataType),PRIVATE,PARAMETER :: generators(nRN)=(/ &
           RNGdataType(              5_SLK**19, 0_SLK, 48, 152917_SLK, 5_SLK**19, 'mcnp std'),  &
           RNGdataType(9219741426499971445_SLK, 1_SLK, 63, 152917_SLK, 1_SLK,     'LEcuyer1'),  &
           RNGdataType(2806196910506780709_SLK, 1_SLK, 63, 152917_SLK, 1_SLK,     'LEcuyer2'),  &
           RNGdataType(3249286849523012805_SLK, 1_SLK, 63, 152917_SLK, 1_SLK,     'LEcuyer3') /)
  !                           mult              add  log2mod  stride   seed0        name
    
  !> Add description
  TYPE :: StochasticSamplingType
    !> Initialization status 
    LOGICAL(SBK) :: isInit=.FALSE.
    !> Random Number Seed
    INTEGER(SLK) :: RNseed=-1
    !> Random Number Data
    INTEGER(SLK) :: RNmult=-1
    !> Random Number Additive Term
    INTEGER(SLK) :: RNadd=-1
    !> Random Number Mask
    INTEGER(SLK) :: RNmask=-1
    !> Random Number Mod
    INTEGER(SLK) :: RNmod=-1
    !> Random Number Normilization
    REAL(SDK) :: RNnorm=-1
    !> Random Number Count
    INTEGER(SLK) :: counter=0
!
!List of type bound procedures
    CONTAINS
      !> @copybrief StochasticSampling::init_Sampler
      !> @copydetails StochasticSampling::init_Sampler
      PROCEDURE,PASS :: init => init_Sampler
      !> @copybrief StochasticSampling::clear_Sampler
      !> @copydetails StochasticSampling::clear_Sampler
      PROCEDURE,PASS :: clear => clear_Sampler
      !> @copybrief StochasticSampling::rng_Sampler
      !> @copydetails StochasticSampling::rng_Sampler
      PROCEDURE,PASS :: rng => rng_Sampler
      !> @copybrief StochasticSampling::unif_Sampler
      !> @copydetails StochasticSampling::unif_Sampler
      PROCEDURE,PASS :: uniform => unif_Sampler
      !> @copybrief StochasticSampling::exp_Sampler
      !> @copydetails StochasticSampling::exp_Sampler
      PROCEDURE,PASS :: exponential => exp_Sampler
      !> @copybrief StochasticSampling::norm_Sampler
      !> @copydetails StochasticSampling::norm_Sampler
      PROCEDURE,PASS :: normal => norm_Sampler
      !> @copybrief StochasticSampling::lognorm_Sampler
      !> @copydetails StochasticSampling::lognorm_Sampler
      PROCEDURE,PASS :: lognormal => lognorm_Sampler
      !> @copybrief StochasticSampling::maxw_Sampler
      !> @copydetails StochasticSampling::maxw_Sampler
      PROCEDURE,PASS :: maxwellian => maxw_Sampler
      !> @copybrief StochasticSampling::watt_Sampler
      !> @copydetails StochasticSampling::watt_Sampler
      PROCEDURE,PASS :: watt => watt_Sampler
      !> @copybrief StochasticSampling::evap_Sampler
      !> @copydetails StochasticSampling::evap_Sampler
      PROCEDURE,PASS :: evaporation => evap_Sampler
!  Kalbach-mann not yet implemented
!      !> @copybrief StochasticSampling::kalb_Sampler
!      !> @copydetails StochasticSampling::kalb_Sampler
!      PROCEDURE,PASS :: kalbachmann => kalb_Sampler
!
      !> @copybrief StochasticSampling::nhist_Sampler
      !> @copydetails StochasticSampling::nhist_Sampler
      PROCEDURE,PASS :: histogram => nhist_Sampler
      !> @copybrief StochasticSampling::uhist_Sampler
      !> @copydetails StochasticSampling::uhist_Sampler
      PROCEDURE,PASS :: unnormhistogram => uhist_Sampler
      !> @copybrief StochasticSampling::nchist_Sampler
      !> @copydetails StochasticSampling::nchist_Sampler
      PROCEDURE,PASS :: conthistogram => nchist_Sampler
      !> @copybrief StochasticSampling::uchist_Sampler
      !> @copydetails StochasticSampling::uchist_Sampler
      PROCEDURE,PASS :: unnormconthistogram => uchist_Sampler
      !> @copybrief StochasticSampling::npwl_Sampler
      !> @copydetails StochasticSampling::npwl_Sampler
      PROCEDURE,PASS :: pwlinear => npwl_Sampler
      !> @copybrief StochasticSampling::upwl_Sampler
      !> @copydetails StochasticSampling::upwl_Sampler
      PROCEDURE,PASS :: unnormpwlinear => upwl_Sampler
      !> @copybrief StochasticSampling::reject_Sampler
      !> @copydetails StochasticSampling::reject_Sampler
      PROCEDURE,PASS :: rejection => reject_Sampler
      !> @copybrief StochasticSampling::rejectarg_Sampler
      !> @copydetails StochasticSampling::rejectarg_Sampler
      PROCEDURE,PASS :: rejectionarg => rejectarg_Sampler
      !> @copybrief StochasticSampling::pwlreject_Sampler
      !> @copydetails StochasticSampling::pwlreject_Sampler
      PROCEDURE,PASS :: pwlrejection => pwlreject_Sampler
  ENDTYPE StochasticSamplingType
  
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Constructor for a stochastic sampler
!> @param sampler the variable to initialize
!> @param seed0 is the initial seed
!>
!> There are essentially only a few ways to use this routine. If it is not
!> called in this way then the routine will return the sampler uninitialized.
!>
!> @code
!> TYPE(StochasticSampler) :: sampler
!> CALL sampler%initialize(19073486328125_SLK)
!> @endcode
!>
    PURE SUBROUTINE init_Sampler(sampler,RNGdata,seed0,skip)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      TYPE(RNGdataType),INTENT(IN) :: RNGdata
      INTEGER(SLK),INTENT(IN),OPTIONAL :: seed0
      INTEGER(SLK),INTENT(IN),OPTIONAL :: skip
      
      sampler%RNseed=RNGdata%RNseed0
      ! Add checks for constraints on seed0
      IF(PRESENT(seed0)) sampler%RNseed=seed0
      
      IF(PRESENT(skip)) sampler%RNseed=RNskip(RNGdata,sampler%RNseed,skip)
      
      sampler%RNmult=RNGdata%RNmult
      sampler%RNadd=RNGdata%RNadd
      sampler%RNmask=2_SLK**RNGdata%RNlog2mod-1_SLK
      sampler%RNmod=2_SLK**RNGdata%RNlog2mod
      sampler%RNnorm=1.0_SDK/2.0_SDK**RNGdata%RNlog2mod
      
      sampler%isInit=.TRUE.
      sampler%counter=0
    ENDSUBROUTINE init_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine clears the data in a stochastic sampler type variable
!> @param sampler the type variable to clear
!>
    ELEMENTAL SUBROUTINE clear_Sampler(sampler)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      sampler%RNseed=-1
      sampler%isInit=.FALSE.
    ENDSUBROUTINE clear_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the next random number for the stochastic sampler
!> @param sampler the type variable to sample from
!>
    FUNCTION rng_Sampler(sampler) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK) :: rang
      sampler%RNseed=IAND(IAND(sampler%RNmult*sampler%RNseed,sampler%RNmask)+ &
        sampler%RNadd,sampler%RNmask)
      rang=sampler%RNseed*sampler%RNnorm
      sampler%counter=sampler%counter+1
    ENDFUNCTION rng_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from a uniform distribution between a 
!> and b
!> @param sampler the type variable to sample from
!> @param xmin the minimum value in the uniform distribution
!> @param xmax the maximum value in the uniform distribution
!>
    FUNCTION unif_Sampler(sampler,xmin,xmax) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: xmin
      REAL(SDK),INTENT(IN) :: xmax
      REAL(SDK) :: rang
      rang=xmin+(xmax-xmin)*sampler%rng()
    ENDFUNCTION unif_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from an exponential distribution with 
!> the coefficient a
!> @param sampler the type variable to sample from
!> @param a is the coefficient of the exponential f(x)=a*EXP(-a*x)
!>
    FUNCTION exp_Sampler(sampler,a) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: a
      REAL(SDK) :: rang

      rang=-LOG(sampler%rng())/a
    ENDFUNCTION exp_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from a normal distribution with 
!> the mean value xbar and standard deviation sigma
!> @param sampler the type variable to sample from
!> @param mu is the mean value
!> @param sigma is the standard deviation
!>
    FUNCTION norm_Sampler(sampler,mu,sigma) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: mu
      REAL(SDK),INTENT(IN) :: sigma
      REAL(SDK) :: rn1,rn2
      REAL(SDK) :: rang
      rn1=-LOG(sampler%rng())
      rn2=COS(2.0_SDK*PI*sampler%rng())
      rang=mu+sigma*SQRT(2.0_SDK*rn1)*rn2
    ENDFUNCTION norm_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from a log-normal distribution with 
!> the mean value xbar and standard deviation sigma
!> @param sampler the type variable to sample from
!> @param mu is the mean value
!> @param sigma is the standard deviation
!>
    FUNCTION lognorm_Sampler(sampler,mu,sigma) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: mu
      REAL(SDK),INTENT(IN) :: sigma
      REAL(SDK) :: rang
      rang=EXP(sampler%normal(mu,sigma))
    ENDFUNCTION lognorm_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from a Maxwellian distribution with 
!> the temperature T
!> @param sampler the type variable to sample from
!> @param T is the temperature
!>
    FUNCTION maxw_Sampler(sampler,T) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: T
      REAL(SDK) :: rn1, rn2, rn3
      REAL(SDK) :: rang
      
      rn1=-LOG(sampler%rng())
      rn2=-LOG(sampler%rng())
      rn3=COS(0.5_SDK*PI*sampler%rng())**2
      rang=T*(rn1+rn2*rn3)
    ENDFUNCTION maxw_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from a Watt fission distribution with 
!> the coefficients a and b
!> @param sampler the type variable to sample from
!> @param a is the coefficient of the Watt fission spectrum
!> @param b is the coefficient of the Watt fission spectrum
!>
    FUNCTION watt_Sampler(sampler,a,b) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: a
      REAL(SDK),INTENT(IN) :: b
      REAL(SDK) :: w, a2b
      REAL(SDK) :: rang
      a2b=a**2*b
      w=sampler%maxwellian(a)
      rang=w+0.25_SDK*a2b+sampler%uniform(-1.0_SDK,1.0_SDK)*SQRT(a2b*w)
    ENDFUNCTION watt_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from a Evaporation distribution with 
!> the coefficient theta
!> @param sampler the type variable to sample from
!> @param thata is the coefficient of the Evaporation spectrum
!>
    FUNCTION evap_Sampler(sampler,theta) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: theta
      REAL(SDK) :: rn1
      REAL(SDK) :: rang
      rn1=sampler%rng()
      rang=-theta*LOG(rn1*sampler%rng())
    ENDFUNCTION evap_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from a Discrete Histogram with
!>                components y
!> @param sampler the type variable to sample from
!> @param y is a vector of the values of the histogram
!>
    FUNCTION nhist_Sampler(sampler,y) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: y(:)
      REAL(SDK) :: rn1, sum
      INTEGER(SIK) :: rang
      INTEGER(SIK) :: n
      
      n=SIZE(y,DIM=1)
      rn1=sampler%rng()
      sum=0.0_SDK
      DO rang=1,n
        sum=sum+y(rang)
        IF (rn1 <sum) EXIT
      ENDDO
    ENDFUNCTION nhist_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from an unnormalized discrete
!>        histogram with components y
!> @param sampler the type variable to sample from
!> @param y is a vector of the values of the histogram
!>
    FUNCTION uhist_Sampler(sampler,y) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: y(:)
      INTEGER(SIK) :: rang
      rang=sampler%histogram(y/SUM(y))
    ENDFUNCTION uhist_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from a normalized continuous
!>        histogram with components y and bounds x
!> @param sampler the type variable to sample from
!> @param y is a vector of the values of the histogram
!> @param x is a vector of the bounds of y
!>
    FUNCTION nchist_Sampler(sampler,y,x) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: y(:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK) :: rang
      INTEGER(SIK) :: interval
      interval=sampler%histogram(y)
      rang=sampler%uniform(x(interval),x(interval+1))
    ENDFUNCTION nchist_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from an unnormalized continuous
!>        histogram with components y and bounds x
!> @param sampler the type variable to sample from
!> @param y is a vector of the values of the histogram
!> @param x is a vector of the bounds of y
!>
    FUNCTION uchist_Sampler(sampler,y,x) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: y(:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK) :: rang
      
      rang=sampler%conthistogram(y/sum(y),x)
      
    ENDFUNCTION uchist_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from a normalized piece-wise linear
!>        distribution with points x and y
!> @param sampler the type variable to sample from
!> @param y is a vector of the y values of the piece-wise linear function
!> @param x is a vector of the x values of the piece-wise linear function
!>
    FUNCTION npwl_Sampler(sampler,y,x) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: y(:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK) :: rang
      REAL(SDK) :: rn1, rn2, sum
      INTEGER(SIK) :: i,n
      
      n=SIZE(x,DIM=1)-1
      
      rn1=sampler%rng()
      rn2=sampler%rng()
      
      sum=0.0_SDK
      DO i=1,n
        sum=sum+(y(i)+y(i+1))/2*(x(i+1)-x(i))
        IF (rn1 <sum) EXIT
      ENDDO
      
      IF (rn1 < y(i)/(y(i+1)+y(i))) THEN
        rang=x(i+1) - (x(i+1)-x(i))*SQRT(rn2)
      ELSE
        rang=x(i)+(x(i+1)-x(i))*SQRT(rn2)
      ENDIF
    ENDFUNCTION npwl_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from a unnormalized piece-wise linear
!>        distribution with points x and y
!> @param sampler the type variable to sample from
!> @param y is a vector of the y values of the piece-wise linear function
!> @param x is a vector of the x values of the piece-wise linear function
!>
    FUNCTION upwl_Sampler(sampler,y,x) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: y(:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK) :: rang
      REAL(SDK) :: sum
      INTEGER(SIK) :: i,n
      
      n=SIZE(y,DIM=1)-1

      sum=0.0_SDK
      DO i=1,n
        sum=sum+(y(i)+y(i+1))/2*(x(i+1)-x(i))
      ENDDO
      rang=sampler%pwlinear(y/sum,x)
    ENDFUNCTION upwl_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from an arbitrary function func using
!>        rejection sampling
!> @param sampler the type variable to sample from
!> @param func is the function which is sampled
!> @param xmin is the minimum value of x
!> @param xmax is the maximum value of x
!> @param ymax is the maximum value of func in the range xmin to xmax
!>
    FUNCTION reject_Sampler(sampler,func,xmin,xmax,ymax) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: xmin
      REAL(SDK),INTENT(IN) :: xmax
      REAL(SDK),INTENT(IN) :: ymax
      REAL(SDK) :: rang
      
      INTERFACE
        FUNCTION func(x)
          IMPORT :: SDK
          REAL(SDK),INTENT(IN) :: x
          REAL(SDK) :: func
        ENDFUNCTION
      ENDINTERFACE

      DO
        rang=sampler%uniform(xmin,xmax)
        IF (sampler%uniform(0.0_SDK,ymax)<=func(rang)) RETURN
      ENDDO
    ENDFUNCTION reject_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from an arbitrary function func with
!>        extra argument vector arg using rejection sampling
!> @param sampler the type variable to sample from
!> @param func is the function which is sampled
!> @param xmin is the minimum value of x
!> @param xmax is the maximum value of x
!> @param ymax is the maximum value of func in the range xmin to xmax
!> @param arg is a vector of arguments that gets passed to func
!>
    FUNCTION rejectarg_Sampler(sampler,func,xmin,xmax,ymax,arg) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: xmin
      REAL(SDK),INTENT(IN) :: xmax
      REAL(SDK),INTENT(IN) :: ymax
      REAL(SDK),INTENT(IN) :: arg(:)
      REAL(SDK) :: rang
      
      INTERFACE
        FUNCTION func(x,arg)
          IMPORT :: SDK
          REAL(SDK),INTENT(IN) :: x
          REAL(SDK),INTENT(IN) :: arg(:)
          REAL(SDK) :: func
        ENDFUNCTION
      ENDINTERFACE

      DO
        rang=sampler%uniform(xmin,xmax)
        IF (sampler%uniform(0.0_SDK,ymax)<=func(rang,arg)) RETURN
      ENDDO
    ENDFUNCTION rejectarg_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from an arbitrary function func using
!>        rejection sampling which is bound by a piece-wise linear function
!> @param sampler the type variable to sample from
!> @param func is the function which is sampled
!> @param yval is the y components of a piece-wise linear function bounding func
!> @param xval is the x components of a piece-wise linear function bounding func
!> @param c is an optional scalar which scales the piece-wise linear fucntion
!>          bounding func.  It is important to note that if c is present the pwl
!>          function is assumed to be normalized, if it is not present the pwl
!>          function is not scaled and used as is
!>
    FUNCTION pwlreject_Sampler(sampler,func,yval,xval,c) RESULT(rang)
      CLASS(StochasticSamplingType),INTENT(INOUT) :: sampler
      REAL(SDK),INTENT(IN) :: yval(:)
      REAL(SDK),INTENT(IN) :: xval(:)
      REAL(SDK),INTENT(IN),OPTIONAL :: c
      REAL(SDK) :: rang
      REAL(SDK) :: g, mult
      REAL(SDK),ALLOCATABLE :: yscaled(:)
      INTEGER(SIK) :: i,n
      
      INTERFACE
        FUNCTION func(x)
          IMPORT :: SDK
          REAL(SDK),INTENT(IN) :: x
          REAL(SDK) :: func
        ENDFUNCTION
      ENDINTERFACE
      
      n=SIZE(xval,DIM=1)-1
      ALLOCATE(yscaled(n+1))
      
      IF (PRESENT(c)) THEN
        mult=c
        yscaled=yval
      ELSE
        mult=0.0_SDK
        DO i=1,n
          mult=mult+(yval(i)+yval(i+1))/2*(xval(i+1)-xval(i))
        ENDDO
        yscaled=yval/mult
      ENDIF

      DO
        rang=sampler%pwlinear(yscaled,xval)
        DO i=1,n
          IF (xval(i) <= rang) then
            g=(yscaled(i)-yscaled(i-1))/(xval(i)-xval(i-1))*(rang-xval(i-1))+yscaled(i-1)
            EXIT
          ENDIF
        ENDDO
        IF (sampler%uniform(0.0_SDK,mult*g)<=func(rang)) RETURN
      ENDDO
    ENDFUNCTION pwlreject_Sampler
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a random number from an arbitrary function func using
!>        rejection sampling which is bound by a piece-wise linear function
!> @param sampler the type variable to sample from
!> @param func is the function which is sampled
!> @param yval is the y components of a piece-wise linear function bounding func
!> @param xval is the x components of a piece-wise linear function bounding func
!> @param c is an optional scalar which scales the piece-wise linear fucntion
!>          bounding func.  It is important to note that if c is present the pwl
!>          function is assumed to be normalized, if it is not present the pwl
!>          function is not scaled and used as is
!>
    PURE FUNCTION RNskip(RNGdata,seed0,skip) RESULT(newseed)
      TYPE(RNGdataType),INTENT(IN) :: RNGdata
      INTEGER(SLK),INTENT(IN) :: seed0
      INTEGER(SLK),INTENT(IN) :: skip
      ! Local Variables
      INTEGER(SLK) :: newseed
      INTEGER(SLK) :: nskip, gen, g, inc, c, gp, rn, period, mask
      
      mask=ISHFT(NOT(0_SLK),RNGdata%RNlog2mod-64)
      IF( RNGdata%RNadd==0 ) THEN
        period=ISHFT(1_SLK,RNGdata%RNlog2mod-2)
      ELSE
        period=ISHFT(1_SLK,RNGdata%RNlog2mod)
      ENDIF
      
      nskip=skip
      DO WHILE (nskip<0_SLK)
        nskip=nskip+period
      ENDDO
      
      nskip=IAND(nskip,mask)
      gen=1
      g=RNGdata%RNmult
      inc=0
      c=RNGdata%RNadd
      DO WHILE(nskip>0_SLK)
        IF(BTEST(nskip,0))  THEN
          gen=IAND(gen*g,mask)
          inc=IAND(inc*g,mask)
          inc=IAND(inc+c,mask)
        ENDIF
        gp=IAND(g+1,mask)
        g=IAND(g*g,mask)
        c=IAND(gp*c,mask)
        nskip=ISHFT(nskip,-1)
      ENDDO
      rn=IAND(gen*seed0,mask)
      rn=IAND(rn+inc,mask)
      newseed=rn
    ENDFUNCTION RNskip
!
ENDMODULE StochasticSampling
