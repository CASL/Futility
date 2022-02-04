!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief The global module for collecting all public members of
!> extended math routines.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ExtendedMath

USE IntrType
USE Constants_Conversion
IMPLICIT NONE
PRIVATE

PUBLIC :: GAMMAF
PUBLIC :: Rational_Fraction
PUBLIC :: GreatestCommonDivisor
PUBLIC :: LeastCommonMultiple
PUBLIC :: ATAN2PI
PUBLIC :: RotateQtrClockwise
PUBLIC :: ThirdOrderBickley

INTERFACE LeastCommonMultiple
  MODULE PROCEDURE LeastCommonMultiple_scalar
  MODULE PROCEDURE LeastCommonMultiple_A1
ENDINTERFACE LeastCommonMultiple

CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Computes the GAMMA function
!> @param z a double precision real to compute the value of gamma for
!> @returns g the value of the GAMMA function at z
!>
!> This routine uses the Lanczos approximation to compute the gamma function
!> The coefficients @c p are the coefficients used by the GNU Scientific
!> Library. It is a double precision function and should be accurate to 15
!> digits of precision. It agrees with the intrinsic function supplied by the
!> Intel compiler.
!>
  PURE RECURSIVE FUNCTION GAMMAF(z) RESULT(g)
    INTEGER(SIK),PARAMETER :: cg=7
    REAL(SDK),DIMENSION(0:8),PARAMETER :: p=(/0.99999999999980993_SDK, &
        676.5203681218851_SDK,-1259.1392167224028_SDK,771.32342877765313_SDK, &
        -176.61502916214059_SDK,12.507343278686905_SDK, &
        -0.13857109526572012_SDK,9.9843695780195716e-6_SDK, &
        1.5056327351493116e-7_SDK/)
    REAL(SDK),INTENT(IN) :: z
    REAL(SDK) :: g
    INTEGER(SIK) :: i
    REAL(SDK) :: t,w,x

    x=z
    IF(x < 0.5_SDK) THEN
      g=PI/(SIN(PI*x)*GAMMAF(1.0_SDK-x))
    ELSE
      x=x-1.0_SDK
      t=p(0)
      DO i=1,cg+1
        t=t+p(i)/(x+REAL(i,SDK))
      ENDDO
      w=x+REAL(cg,SDK)+0.5_SDK
      g=SQRT(2.0_SDK*PI)*(w**(x+0.5_SDK))*EXP(-w)*t
    ENDIF
  ENDFUNCTION GAMMAF
!
!-------------------------------------------------------------------------------
!> @brief Computes a Rational Fraction less than a tolerance
!> @param targval a real which is the target value of the rational fraction
!> @param tol a real which is the tolerance for the rational fraction
!> @param num an integer which is the numerator of the rational fraction
!> @param denom an integer which is the denomenator of the rational fraction
!>
!> This routine calculates the rational fractions of a target value until the
!> fraction is less than a tolererance from the original value.  The rational
!> fraction calculation will stop after 50 iterations, which is an arbitrary
!> value that could easily be extended if tighter accuracy is required.  The
!> coeffiecients are stored such that:
!>    approx = c(0) + 1 / (c(1) + 1 / (c(2) + 1 / (c(3) +...)))
!>
SUBROUTINE Rational_Fraction(targval,tol,num,denom)
  REAL(SRK),INTENT(IN) :: targval
  REAL(SRK),INTENT(IN) :: tol
  INTEGER(SIK),INTENT(OUT) :: num
  INTEGER(SIK),INTENT(OUT) :: denom

  INTEGER(SIK) :: lev
  INTEGER(SIK) :: coef(50)
  REAL(SRK) :: targ

  coef=0

  targ=targval

  coef(1)=FLOOR(targ)
  targ=1.0_SRK/(targ-REAL(coef(1),SRK))
  DO lev=2,SIZE(coef)
    coef(lev)=FLOOR(targ)
    targ=1.0_SRK/(targ-REAL(coef(lev),SRK))
    num=coef(lev)
    denom=1
    CALL simplify_rat_frac(coef,lev,num,denom)
    IF(ABS(REAL(num,SRK)/REAL(denom,SRK)/targval-1.0_SRK) < tol) EXIT
  ENDDO
ENDSUBROUTINE Rational_Fraction
!
!-------------------------------------------------------------------------------
!> @brief Simplifies the rational fraction to a simple fraction
!> @param coef rational fraction coefficients in extended form
!> @param lev the index of coef of the maximum non-zero location
!> @param num an integer which is the numerator of the rational fraction
!> @param denom an integer which is the denomenator of the rational fraction
!>
!> This routine takes the exapanded coefficients of the rational fraction and
!> simplifies it to a single fraction:
!>    num / denom = c(0) + 1 / (c(1) + 1 / (c(2) + 1 / (c(3) +...)))
!>
RECURSIVE SUBROUTINE simplify_rat_frac(coef,lev,num,denom)
  INTEGER(SIK),INTENT(IN) :: coef(*)
  INTEGER(SIK),INTENT(IN) :: lev
  INTEGER(SIK),INTENT(INOUT) :: num
  INTEGER(SIK),INTENT(INOUT) :: denom

  INTEGER(SIK) :: tmpnum

  tmpnum=num
  num=coef(lev-1)*num+denom
  denom=tmpnum
  IF(lev > 2) THEN
    CALL simplify_rat_frac(coef,lev-1,num,denom)
  ENDIF
ENDSUBROUTINE simplify_rat_frac
!
!-------------------------------------------------------------------------------
!> @brief Calculates the Greatest Common Divisor
!> @param a1 an integer
!> @param a2 an integer
!> @returns b the greatest common divisor of a1 and a2
!>
!> This routine uses the Euclidean method to calculate the greatest common
!> divisor between two positive numbers.  If a negative number is input, the
!> resulting value is 0.
!>
ELEMENTAL FUNCTION GreatestCommonDivisor(a1,a2) RESULT(b)
  INTEGER(SIK),INTENT(IN) :: a1
  INTEGER(SIK),INTENT(IN) :: a2

  INTEGER(SIK) :: a,b,c

  a=0; b=0; c=0

  IF(a1 > 0 .AND. a2 > 0) THEN
    IF(a1 < a2) THEN
      a=a2
      b=a1
    ELSE
      a=a1
      b=a2
    ENDIF

    DO
      c=MOD(a,b)
      IF(c == 0) EXIT
      a=b
      b=c
    ENDDO
  ENDIF
ENDFUNCTION GreatestCommonDivisor
!
!-------------------------------------------------------------------------------
!> @brief solves for the least common multiple of an array of integers
!> @param u the array of integers to consider
!> @returns lcm the least common multiple of the array
!>
PURE FUNCTION LeastCommonMultiple_A1(u) RESULT(lcm)
  INTEGER(SIK),INTENT(IN) :: u(:)

  INTEGER(SIK) :: lcm
  INTEGER(SIK) :: i

  lcm = 0
  IF(SIZE(u) == 0) RETURN
  IF(ANY(u == 0)) RETURN

  lcm = abs(u(1))
  DO i=2,SIZE(u)
    !In all likelihood there exists an 'a' and 'b' which will cause overflow
    lcm = LeastCommonMultiple(lcm,u(i))
  ENDDO

ENDFUNCTION LeastCommonMultiple_A1
!
!-------------------------------------------------------------------------------
!> @brief solves for the least common multiple of a pair of integers
!> @param u the first integer to consider
!> @param v the second integer to consider
!> @returns lcm the least common multiple of the array
!>
ELEMENTAL FUNCTION LeastCommonMultiple_scalar(u,v) RESULT(lcm)
  INTEGER(SIK),INTENT(IN) :: u
  INTEGER(SIK),INTENT(IN) :: v

  INTEGER(SIK) :: lcm
  INTEGER(SIK) :: a,b

  lcm = 0
  IF(u == 0 .OR. v == 0) RETURN
  a = ABS(u)
  b = ABS(v)

  IF(a == b) THEN
    lcm = a
  ELSE
    !In all likelihood there exists an 'a' and 'b' which will cause overflow
    lcm = (a/GreatestCommonDivisor(a,b)) * b
  ENDIF
ENDFUNCTION LeastCommonMultiple_scalar
!
!-------------------------------------------------------------------------------
!> @brief Calculates the angle from the origin to (x,y) from the +(x-axis) on
!> [0,2pi)
!> @param x the x-coordinate
!> @param y the y-coordinate
!> @returns theta the angle formed by the point (x,y) w.r.t. the positive x-axis
!> on [0,2PI)
!>
!> This routine calls ATAN2 and casts the result to exist on [0,2pi). It is
!> useful, since we often need to sort points by angle in the entire unit circle
!> and the discontinuity at pi that is generated by the native ATAN2 function is
!> inconvenient.
!>
ELEMENTAL FUNCTION ATAN2PI(x,y) RESULT(theta)
  REAL(SRK),INTENT(IN) :: x
  REAL(SRK),INTENT(IN) :: y
  REAL(SRK) :: theta,xx,yy
  xx=x
  IF(x .APPROXEQA. 0.0_SRK) xx=0.0_SRK
  yy=y
  IF(y .APPROXEQA. 0.0_SRK) yy=0.0_SRK
  theta=ATAN2(yy,xx)
  IF(.NOT.(theta .APPROXGE. 0.0_SRK)) theta=TWOPI+theta
ENDFUNCTION ATAN2PI
!
!-------------------------------------------------------------------------------
!> @brief This routine will rotate the input x and y coordinates @nrot number of
!>        clockwise quarter rotations.
!> @param x the x-coordinate
!> @param y the y-coordinate
!> @param nrot the number of clockwise quarter rotations
!>
!> @note: Values from [-3,-1] will be handled as well, even though they
!>        represent counter clockwise rotations. If nrot == 0, no rotations are
!>        performed. The results of these three rotations were taken from the
!>        rotation matrix:
!>   [cos(theta)  -sin(theta)  *  [x
!>    sin(theta)   cos(theta)]     y] where theta is degrees, CW.
!>
ELEMENTAL SUBROUTINE RotateQtrClockwise(x,y,nrot)
  REAL(SRK),INTENT(INOUT) :: x
  REAL(SRK),INTENT(INOUT) :: y
  INTEGER(SIK),INTENT(IN) :: nrot
  REAL(SRK) :: tmp

  SELECTCASE(MOD(nrot,4))
  CASE(1,-3) !90  CW/270 CCW
    tmp=y
    y=-x
    x=tmp
  CASE(2,-2) !180 CW/180 CCW
    x=-x
    y=-y
  CASE(3,-1) !270 CW/90  CCW
    tmp=x
    x=-y
    y=tmp
  ENDSELECT
ENDSUBROUTINE RotateQtrClockwise
!
!-------------------------------------------------------------------------------
!> @brief It calculates third order Bickley function
!> @param x The argument of the third order Bickley function
!> @returns y The result of the third order Bickley function
!>
PURE FUNCTION ThirdOrderBickley(x) RESULT(y)
  REAL(SRK),INTENT(IN) :: x
  REAL(SRK) :: y

  !local variables
  REAL(SRK) :: xx

  xx=ABS(x)
  IF(xx < 0.05_SRK) THEN !Most common case
    y=(0.7266088_SRK*xx-0.9990226_SRK)*xx+0.7853961_SRK
  ELSEIF(xx > 9.0_SRK) THEN !Second most common case
    y=0.0_SRK
  ELSEIF(xx < 0.10_SRK) THEN
    y=(0.6466375_SRK*xx-0.9912340_SRK)*xx+0.7852024_SRK
  ELSEIF(xx < 0.15_SRK) THEN
    y=(0.5856605_SRK*xx-0.9791293_SRK)*xx+0.7845986_SRK
  ELSEIF(xx < 0.20_SRK) THEN
    y=(0.5346648_SRK*xx-0.9638914_SRK)*xx+0.7834577_SRK
  ELSEIF(xx < 0.25_SRK) THEN
    y=(0.4907827_SRK*xx-0.9463843_SRK)*xx+0.7817094_SRK
  ELSEIF(xx < 0.30_SRK) THEN
    y=(0.4521752_SRK*xx-0.9271152_SRK)*xx+0.7793031_SRK
  ELSEIF(xx < 0.35_SRK) THEN
    y=(0.4177388_SRK*xx-0.9064822_SRK)*xx+0.7762107_SRK
  ELSEIF(xx < 0.40_SRK) THEN
    y=(0.3869945_SRK*xx-0.8849865_SRK)*xx+0.7724519_SRK
  ELSEIF(xx < 0.45_SRK) THEN
    y=(0.3590753_SRK*xx-0.8626685_SRK)*xx+0.7679903_SRK
  ELSEIF(xx < 0.50_SRK) THEN
    y=(0.3338676_SRK*xx-0.8400133_SRK)*xx+0.7628988_SRK
  ELSEIF(xx < 0.60_SRK) THEN
    y=(0.2998569_SRK*xx-0.8054172_SRK)*xx+0.7540982_SRK
  ELSEIF(xx < 0.70_SRK) THEN
    y=(0.2609154_SRK*xx-0.7587821_SRK)*xx+0.7401279_SRK
  ELSEIF(xx < 0.80_SRK) THEN
    y=(0.2278226_SRK*xx-0.7125290_SRK)*xx+0.7239594_SRK
  ELSEIF(xx < 0.90_SRK) THEN
    y=(0.1994999_SRK*xx-0.6672761_SRK)*xx+0.7058777_SRK
  ELSEIF(xx < 1.00_SRK) THEN
    y=(0.1751248_SRK*xx-0.6234536_SRK)*xx+0.6861762_SRK
  ELSEIF(xx < 1.40_SRK) THEN
    y=((-0.05337485_SRK*xx+0.3203223_SRK)*xx-0.7538355_SRK)*xx+0.7247294_SRK
  ELSEIF(xx < 1.80_SRK) THEN
    y=((-0.03146833_SRK*xx+0.2295280_SRK)*xx-0.6279752_SRK)*xx+0.6663720_SRK
  ELSEIF(xx < 2.20_SRK) THEN
    y=((-0.01906198_SRK*xx+0.1631667_SRK)*xx-0.5094124_SRK)*xx+0.5956163_SRK
  ELSEIF(xx < 2.60_SRK) THEN
    y=((-0.01174752_SRK*xx+0.1152418_SRK)*xx-0.4046007_SRK)*xx+0.5191031_SRK
  ELSEIF(xx < 3.0_SRK) THEN
    y=((-0.007328415_SRK*xx+0.08097913_SRK)*xx-0.3159648_SRK)*xx+0.4425954_SRK
  ELSEIF(xx < 3.40_SRK) THEN
    y=((-0.004617254_SRK*xx+0.05669960_SRK)*xx-0.2434341_SRK)*xx+0.3703178_SRK
  ELSEIF(xx < 3.80_SRK) THEN
    y=(0.007923547_SRK*xx-0.07158569_SRK)*xx+0.1684022_SRK
  ELSEIF(xx < 4.20_SRK) THEN
    y=(0.005095111_SRK*xx-0.05016344_SRK)*xx+0.1278307_SRK
  ELSEIF(xx < 4.60_SRK) THEN
    y=(0.003286040_SRK*xx-0.03501524_SRK)*xx+0.09611422_SRK
  ELSEIF(xx < 5.00_SRK) THEN
    y=(0.002126242_SRK*xx-0.02437465_SRK)*xx+0.07170491_SRK
  ELSEIF(xx < 5.80_SRK) THEN
    y=(0.001123687_SRK*xx-0.01425519_SRK)*xx+0.04616317_SRK
  ELSEIF(xx < 6.60_SRK) THEN
    y=(4.762937e-4_SRK*xx-6.810124e-3_SRK)*xx+0.02475115_SRK
  ELSEIF(xx < 7.40_SRK) THEN
    y=(2.031843e-4_SRK*xx-3.232035e-3_SRK)*xx+0.01302864_SRK
  ELSEIF(xx < 8.20_SRK) THEN
    y=(8.701440e-5_SRK*xx-1.524126e-3_SRK)*xx+6.749972e-3_SRK
  ELSE !Range 8.2 <= xx <= 9.0, but the > 9.0 portion was already handled
    y=(3.742673e-5_SRK*xx-7.157367e-4_SRK)*xx+3.454768e-3_SRK
  ENDIF

ENDFUNCTION ThirdOrderBickley
!
ENDMODULE ExtendedMath
