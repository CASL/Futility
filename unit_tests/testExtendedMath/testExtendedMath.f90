!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testExtendedMath
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE Constants_Conversion
USE ExtendedMath

IMPLICIT NONE

CREATE_TEST('EXTENDED MATH')

REGISTER_SUBTEST('Gamma Function',testGammaF)
REGISTER_SUBTEST('Rational Fraction',testRatFrac)
REGISTER_SUBTEST('GCD',testGCD)
REGISTER_SUBTEST('GCD',testLCM)
REGISTER_SUBTEST('ATAN2PI',testATAN2PI)

FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testGammaF()

  ASSERT_SOFTEQ(GAMMAF(0.25_SRK),3.6256099082219083119_SRK,1.0E-12_SRK, 'GammaF 0.25')
  ASSERT_SOFTEQ(GAMMAF(0.5_SRK), 1.7724538509055160273_SRK,1.0E-12_SRK, 'GammaF 0.5')
  ASSERT_SOFTEQ(GAMMAF(0.75_SRK),1.2254167024651776451_SRK,1.0E-12_SRK, 'GammaF 0.75')
  ASSERT_SOFTEQ(GAMMAF(3.5_SRK), 3.3233509704478425512_SRK,1.0E-12_SRK, 'GammaF 3.5')
  ASSERT_SOFTEQ(GAMMAF(5.00_SRK),24.00000000000000000_SRK ,1.0E-12_SRK, 'GammaF 5.0')
  ASSERT_SOFTEQ(GAMMAF(7.00_SRK),720.00000000000000000_SRK,1.0E-10_SRK, 'GammaF 7.0')
ENDSUBROUTINE testGammaF
!
!-------------------------------------------------------------------------------
SUBROUTINE testRatFrac()
  INTEGER(SIK) :: i,N,D
  REAL(SRK) :: dif,tol

  CALL Rational_Fraction(1.5_SRK,1.0E-12_SRK,N,D)
  ASSERT_EQ(N , 3, 'RatFrac 1.5 num')
  ASSERT_EQ(D , 2, 'RatFrac 1.5 denom')

  CALL Rational_Fraction(12.8125_SRK,1.0E-12_SRK,N,D)
  ASSERT_EQ(N , 205, 'RatFrac 12.8125 num')
  ASSERT_EQ(D , 16, 'RatFrac 12.8125 denom')

  ! Values of coefficients checked against wikipedia:
  ! http://en.wikipedia.org/wiki/Continued_fraction
  !  pi: [3;7,15,1,292,1,1,1,2,1,3,1,…].
  ! by truncating at 1e-12, the series should be [3;7,15,1,292,1,1,1,2,1]
  ! which is 1146408 / 364913 from hand calc
  CALL Rational_Fraction(3.141592653589793_SRK,1.0E-12_SRK,N,D)
  ASSERT_EQ(N , 1146408, 'RatFrac PI num')
  ASSERT_EQ(D , 364913, 'RatFrac PI denom')

  ! Values of coefficients checked againt wikipedia:
  !  sqrt(19): [4;2,1,3,1,2,8,2,1,3,1,2,8,…]  series repeats every 6
  ! by truncating to 1e-8 the series should be [4;2,1,3,1,2,8,2,1,3]
  ! which is  16311 / 3742 from hand calc
  CALL Rational_Fraction(SQRT(19.0_SRK),1.0E-8_SRK,N,D)
  ASSERT_EQ(N , 16311, 'RatFrac SQRT(19) num')
  ASSERT_EQ(D , 3742, 'RatFrac SQRT(19) denom')

  ! Checking a range of tolerances to ensure it is working correctly
  !  sqrt(2): [1;2,2,2,...] according to wikipedia.  2 the repeats forever
  !  This is also the slowest converging rational fraction
  DO i=1,12
    tol=10.0_SRK**(-REAL(i,SRK))
    CALL Rational_Fraction(SQRT(2.0_SRK),tol,N,D)
    dif=ABS(REAL(N,SRK)/REAL(D,SRK)/SQRT(2.0_SRK) - 1.0_SRK)
    ASSERT_LE(dif,tol, 'tolerance check')
  ENDDO

ENDSUBROUTINE testRatFrac
!
!-------------------------------------------------------------------------------
SUBROUTINE testGCD()
  ! test error conditions (passing negative or 0 values)
  ASSERT_EQ(GreatestCommonDivisor(-1,5),0, 'GCD errors')
  ASSERT_EQ(GreatestCommonDivisor(5,-5),0, 'GCD errors')
  ASSERT_EQ(GreatestCommonDivisor(-1,-5),0, 'GCD errors')
  ASSERT_EQ(GreatestCommonDivisor(0,5),0, 'GCD errors')
  ASSERT_EQ(GreatestCommonDivisor(5,0),0, 'GCD errors')

  ASSERT_EQ(GreatestCommonDivisor(2,3),1, 'GCD 2,3')
  ASSERT_EQ(GreatestCommonDivisor(3,2),1, 'GCD 3,2')  ! ensure both directions give same answer
  ASSERT_EQ(GreatestCommonDivisor(201,3),3, 'GCD 201,3')
  ASSERT_EQ(GreatestCommonDivisor(33,198),33, 'GCD 33,198')
  ASSERT_EQ(GreatestCommonDivisor(6248,3784),88, 'GCD 6248,3784')
ENDSUBROUTINE testGCD
!
!-------------------------------------------------------------------------------
SUBROUTINE testLCM()
  ASSERT_EQ(LeastCommonMultiple(-1,5),5, 'LCM')
  ASSERT_EQ(LeastCommonMultiple(5,-5),5, 'LCM')
  ASSERT_EQ(LeastCommonMultiple(-1,-5),5, 'LCM')
  ASSERT_EQ(LeastCommonMultiple(0,5),0, 'LCM')
  ASSERT_EQ(LeastCommonMultiple(5,0),0, 'LCM')

  ASSERT_EQ(LeastCommonMultiple(2,3),6, 'LCM')
  ASSERT_EQ(LeastCommonMultiple(3,2),6, 'LCM')
  ASSERT_EQ(LeastCommonMultiple(201,3),201, 'LCM')
  ASSERT_EQ(LeastCommonMultiple(33,198),198, 'LCM')
  ASSERT_EQ(LeastCommonMultiple(8,12),24, 'LCM')
  ASSERT_EQ(LeastCommonMultiple(8,36),72, 'LCM')

  ASSERT_EQ(LeastCommonMultiple((/8,12,36/)),LeastCommonMultiple(8,36),"LCM array")
ENDSUBROUTINE testLCM
!
!-------------------------------------------------------------------------------
SUBROUTINE testATAN2PI()
  ASSERT_APPROXEQ(ATAN2PI(0.0_SRK,0.0_SRK) , 0.0_SRK,'(0,0)')

  ASSERT_APPROXEQ(ATAN2PI(1.0_SRK,0.0_SRK) , 0.0_SRK,'(1,0)')
  ASSERT_APPROXEQ(ATAN2PI(0.0_SRK,1.0_SRK) , PI*0.5_SRK,'(0,1)')
  ASSERT_APPROXEQ(ATAN2PI(-1.0_SRK,0.0_SRK) , PI,'(-1,0)')
  ASSERT_APPROXEQ(ATAN2PI(0.0_SRK,-1.0_SRK) , PI*1.5_SRK,'(0,-1)')
  ASSERT_APPROXEQ(ATAN2PI(1.0_SRK,1.0_SRK) , PI*0.25_SRK,'(1,1)')
  ASSERT_APPROXEQ(ATAN2PI(-1.0_SRK,1.0_SRK) , PI*0.75_SRK,'(-1,1)')
  ASSERT_APPROXEQ(ATAN2PI(-1.0_SRK,-1.0_SRK) , PI*1.25_SRK,'(-1,-1)')
  ASSERT_APPROXEQ(ATAN2PI(1.0_SRK,-1.0_SRK) , PI*1.75_SRK,'(1,-1)')

  ASSERT_APPROXEQ(ATAN2PI(1.0_SRK,-EPSREAL) , 0.0_SRK,'(1.0,-EPS)')
  ASSERT_APPROXEQ(ATAN2PI(1.0_SRK,EPSREAL) , 0.0_SRK,'(1.0,+EPS)')
  ASSERT_APPROXEQ(ATAN2PI(-EPSREAL,1.0_SRK) , PI*0.5_SRK,'(-EPS,1.0)')
  ASSERT_APPROXEQ(ATAN2PI(EPSREAL,1.0_SRK) , PI*0.5_SRK,'(+EPS,1.0)')
  ASSERT_APPROXEQ(ATAN2PI(EPSREAL,EPSREAL) , 0.0_SRK,'(+EPS,+EPS)')
  ASSERT_APPROXEQ(ATAN2PI(-EPSREAL,EPSREAL) , 0.0_SRK,'(-EPS,+EPS)')
  ASSERT_APPROXEQ(ATAN2PI(EPSREAL,-EPSREAL) , 0.0_SRK,'(+EPS,-EPS)')
  ASSERT_APPROXEQ(ATAN2PI(-EPSREAL,-EPSREAL) , 0.0_SRK,'(-EPS,-EPS)')
ENDSUBROUTINE testATAN2PI
!
!-------------------------------------------------------------------------------
FUNCTION oldATAN2PI(x,y) RESULT(theta)
  REAL(SRK),INTENT(IN) :: x,y
  REAL(SRK) :: theta
  theta=atan2(y,x)
  IF(theta < 0) theta=2.0_SRK*PI+theta
ENDFUNCTION oldATAN2PI
!
ENDPROGRAM testExtendedMath
