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
REGISTER_SUBTEST('RotateQtrClockwise',testRotateQtrClockwise)
REGISTER_SUBTEST("testThirdOrderBickley", testThirdOrderBickley)

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
  ASSERT_EQ(LeastCommonMultiple(36,8),72, 'LCM')

  ASSERT_EQ(LeastCommonMultiple((/8,12,36/)),72,"LCM array")
  ASSERT_EQ(LeastCommonMultiple((/3,25,28/)),2100,"LCM array")
  ASSERT_EQ(LeastCommonMultiple((/12,15,75,10/)),300,"LCM array")
  ASSERT_EQ(LeastCommonMultiple((/100,1000,50,200,25/)),1000,"LCM array")
  ASSERT_EQ(LeastCommonMultiple((/100,1000,50,200,24/)),3000,"LCM array")
  ASSERT_EQ(LeastCommonMultiple((/198,150,24,205,87,200,154/)),164795400,"LCM array")
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
SUBROUTINE testRotateQtrClockwise()
  REAL(SRK) :: x,y

  !Null point, no rotation
  x=0.0_SRK; y=0.0_SRK
  CALL RotateQtrClockwise(x,y,0)
  ASSERT_APPROXEQ(x, 0.0_SRK,'x=0.0 rot 0')
  ASSERT_APPROXEQ(y, 0.0_SRK,'y=0.0 rot 0')

  !No rotation
  x=1.0_SRK; y=0.0_SRK
  CALL RotateQtrClockwise(x,y,0)
  ASSERT_APPROXEQ(x, 1.0_SRK,'x=1.0 rot 0')
  ASSERT_APPROXEQ(y, 0.0_SRK,'y=0.0 rot 0')

  !Rotate 90 degrees CW
  x=1.0_SRK; y=0.0_SRK
  CALL RotateQtrClockwise(x,y,1)
  ASSERT_APPROXEQ(x, 0.0_SRK,'x=0.0 rot 1')
  ASSERT_APPROXEQ(y, -1.0_SRK,'y=-1.0 rot 1')

  !Rotate 270 degrees CCW
  x=1.0_SRK; y=0.0_SRK
  CALL RotateQtrClockwise(x,y,-3)
  ASSERT_APPROXEQ(x, 0.0_SRK,'x=0.0 rot -3')
  ASSERT_APPROXEQ(y, -1.0_SRK,'y=-1.0 rot -3')

  !Rotate 180 degrees CW
  x=1.0_SRK; y=0.0_SRK
  CALL RotateQtrClockwise(x,y,2)
  ASSERT_APPROXEQ(x, -1.0_SRK,'x=-1.0 rot 2')
  ASSERT_APPROXEQ(y, 0.0_SRK,'y=0.0 rot 2')

  !Rotate 180 degrees CCW
  x=1.0_SRK; y=0.0_SRK
  CALL RotateQtrClockwise(x,y,-2)
  ASSERT_APPROXEQ(x, -1.0_SRK,'x=-1.0 rot -2')
  ASSERT_APPROXEQ(y, 0.0_SRK,'y=0.0 rot -2')

  !Rotate 270 degrees CW
  x=1.0_SRK; y=0.0_SRK
  CALL RotateQtrClockwise(x,y,3)
  ASSERT_APPROXEQ(x, 0.0_SRK,'x=0.0 rot 3')
  ASSERT_APPROXEQ(y, 1.0_SRK,'y=1.0 rot 3')

  !Rotate 90 degrees CCW
  x=1.0_SRK; y=0.0_SRK
  CALL RotateQtrClockwise(x,y,-1)
  ASSERT_APPROXEQ(x, 0.0_SRK,'x=0.0 rot -1')
  ASSERT_APPROXEQ(y, 1.0_SRK,'y=1.0 rot -1')

  !No rotation
  x=1.0_SRK; y=1.0_SRK
  CALL RotateQtrClockwise(x,y,0)
  ASSERT_APPROXEQ(x, 1.0_SRK,'x=1.0 rot 0')
  ASSERT_APPROXEQ(y, 1.0_SRK,'y=1.0 rot 0')

  !Rotate 90 degrees CW
  x=1.0_SRK; y=1.0_SRK
  CALL RotateQtrClockwise(x,y,1)
  ASSERT_APPROXEQ(x, 1.0_SRK,'x=1.0 rot 1')
  ASSERT_APPROXEQ(y, -1.0_SRK,'y=-1.0 rot 1')

  !Rotate 270 degrees CCW
  x=1.0_SRK; y=1.0_SRK
  CALL RotateQtrClockwise(x,y,-3)
  ASSERT_APPROXEQ(x, 1.0_SRK,'x=1.0 rot -3')
  ASSERT_APPROXEQ(y, -1.0_SRK,'y=-1.0 rot -3')

  !Rotate 180 degrees CW
  x=1.0_SRK; y=1.0_SRK
  CALL RotateQtrClockwise(x,y,2)
  ASSERT_APPROXEQ(x, -1.0_SRK,'x=-1.0 rot 2')
  ASSERT_APPROXEQ(y, -1.0_SRK,'y=-1.0 rot 2')

  !Rotate 180 degrees CCW
  x=1.0_SRK; y=1.0_SRK
  CALL RotateQtrClockwise(x,y,-2)
  ASSERT_APPROXEQ(x, -1.0_SRK,'x=-1.0 rot -2')
  ASSERT_APPROXEQ(y, -1.0_SRK,'y=-1.0 rot -2')

  !Rotate 270 degrees CW
  x=1.0_SRK; y=1.0_SRK
  CALL RotateQtrClockwise(x,y,3)
  ASSERT_APPROXEQ(x, -1.0_SRK,'x=-1.0 rot 3')
  ASSERT_APPROXEQ(y, 1.0_SRK,'y=1.0 rot 3')

  !Rotate 90 degrees CCW
  x=1.0_SRK; y=1.0_SRK
  CALL RotateQtrClockwise(x,y,-1)
  ASSERT_APPROXEQ(x, -1.0_SRK,'x=-1.0 rot -1')
  ASSERT_APPROXEQ(y, 1.0_SRK,'y=1.0 rot -1')
ENDSUBROUTINE testRotateQtrClockwise
!
!-------------------------------------------------------------------------------
FUNCTION oldATAN2PI(x,y) RESULT(theta)
  REAL(SRK),INTENT(IN) :: x,y
  REAL(SRK) :: theta
  theta=atan2(y,x)
  IF(theta < 0) theta=2.0_SRK*PI+theta
ENDFUNCTION oldATAN2PI
!
!-------------------------------------------------------------------------------
SUBROUTINE testThirdOrderBickley()

  ASSERT(SOFTEQ(ThirdOrderBickley(0.025_SRK),0.760874665500000_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.025)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.075_SRK),0.714497185937500_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.075)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.125_SRK),0.671358382812500_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.125)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.175_SRK),0.631150814500000_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.175)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.225_SRK),0.593618806687500_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.225)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.275_SRK),0.558542169500000_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.275)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.325_SRK),0.525727645750000_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.325)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.375_SRK),0.495003064062500_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.375)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.425_SRK),0.466214163562500_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.425)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.475_SRK),0.439221359750000_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.475)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.550_SRK),0.401825452250000_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.550)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.650_SRK),0.357156291500000_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.650)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.750_SRK),0.317712862500000_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.750)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.850_SRK),0.282831692750000_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.850)")
  ASSERT(SOFTEQ(ThirdOrderBickley(0.950_SRK),0.251945412000000_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(0.950)")
  ASSERT(SOFTEQ(ThirdOrderBickley(1.200_SRK),0.189159171200000_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(1.200)")
  ASSERT(SOFTEQ(ThirdOrderBickley(1.600_SRK),0.120309080320000_SRK     ,1.0e-12_SRK),"ThirdOrderBickley(1.600)")
  ASSERT(SOFTEQ(ThirdOrderBickley(2.000_SRK),7.696245999999995E-002_SRK,1.0e-12_SRK),"ThirdOrderBickley(2.000)")
  ASSERT(SOFTEQ(ThirdOrderBickley(2.400_SRK),4.945647152000016E-002_SRK,1.0e-12_SRK),"ThirdOrderBickley(2.400)")
  ASSERT(SOFTEQ(ThirdOrderBickley(2.800_SRK),3.189697312000006E-002_SRK,1.0e-12_SRK),"ThirdOrderBickley(2.800)")
  ASSERT(SOFTEQ(ThirdOrderBickley(3.200_SRK),2.063440492800001E-002_SRK,1.0e-12_SRK),"ThirdOrderBickley(3.200)")
  ASSERT(SOFTEQ(ThirdOrderBickley(3.600_SRK),1.338288511999999E-002_SRK,1.0e-12_SRK),"ThirdOrderBickley(3.600)")
  ASSERT(SOFTEQ(ThirdOrderBickley(4.000_SRK),8.698716000000009E-003_SRK,1.0e-12_SRK),"ThirdOrderBickley(4.000)")
  ASSERT(SOFTEQ(ThirdOrderBickley(4.400_SRK),5.664898399999979E-003_SRK,1.0e-12_SRK),"ThirdOrderBickley(4.400)")
  ASSERT(SOFTEQ(ThirdOrderBickley(4.800_SRK),3.695205679999994E-003_SRK,1.0e-12_SRK),"ThirdOrderBickley(4.800)")
  ASSERT(SOFTEQ(ThirdOrderBickley(5.400_SRK),1.951856920000009E-003_SRK,1.0e-12_SRK),"ThirdOrderBickley(2.4)")
  ASSERT(SOFTEQ(ThirdOrderBickley(6.200_SRK),8.371110279999996E-004_SRK,1.0e-12_SRK),"ThirdOrderBickley(2.8)")
  ASSERT(SOFTEQ(ThirdOrderBickley(7.000_SRK),3.604257000000000E-004_SRK,1.0e-12_SRK),"ThirdOrderBickley(3.2)")
  ASSERT(SOFTEQ(ThirdOrderBickley(7.800_SRK),1.557452960000002E-004_SRK,1.0e-12_SRK),"ThirdOrderBickley(3.6)")
  ASSERT(SOFTEQ(ThirdOrderBickley(8.600_SRK),6.751333079999975E-005_SRK,1.0e-12_SRK),"ThirdOrderBickley(4.0)")

!data from Wolfram Alpha 76 significant figures
!arg x     ThirdOrderBickley(x) result of the Bickley function of order three
!0.025     0.76087435435473089481415940877597033571853054821901991516400325474643284773336
!0.075     0.71449679782598526588821547470452465456072719612047630320980195234993342864256
!0.125     0.67135807577109342817946318906174187092787964671445454738446543799021539000978
!0.175     0.63115047862169504452063910448507825276853540965153164809546500702205218310449
!0.225     0.59361841911938945552648920262348309470941047039495859963279516413794004545305
!0.275     0.55854178728401589214049334376515267256572163215350210072437632033519341209916
!0.325     0.52572724070349487181924281894203869507925667656046621747294445654651987447062
!0.375     0.49500276632364780191816272471601602357514824260519750266189586060847625690883
!0.425     0.46621393979863575392216528721577559310239218091302181577479117780150980744750
!0.475     0.43922118686469524800396457044600487137488630880560313204930013463782482792567
!0.550     0.40182535282391698297458635029025432128965486542492333850466147236063725415729
!0.650     0.35715627270140456636611676768402689893215759424244353827223333051537222578432
!0.750     0.31771277656952221958158362255061189690906158712914606465171107896701959742579
!0.850     0.28283162553269040885783128737810736981905570575929112100383119692506037271062
!0.950     0.25194540869425338958124027964104324074752288880999752520768477667293821782424
!1.200     0.18916287763245597978706605859244691850573157613366372399064946356597412542183
!1.600     0.12031089184783858071717638522312464645541380493830736933748211196063082301064
!2.000     0.076963590311658422876818821208590636542634695527976466961577389725761406511519
!2.400     0.049457028031707628064213758059820561481339021775308114578972671841384222775386
!2.800     0.031897511889064199646903466297702116064408073380184568612045684723723587333865
!3.200     0.020634782845562449157993465589641699827465512521288053099219320252709113760499
!3.600     0.013382991329065580861327812619087558102372841626198681240934154676282966267723
!4.000     0.0086987886861997011003542329497674945951430192610029923790805463110000398579947
!4.400     0.0056648909039604982498722037905876778734163941370103855884253919179207346896954
!4.800     0.0036953063069731639358811565454225299312839796120822391929484878605617712940590
!5.400     0.0019521925233853647800369181229233195067620714546474160712559007214062890525572
!6.200     0.00083732548774562206899852981353429892451414336783756347601246685001654830130831
!7.000     0.00036061979247139406228437660036841834297077580646658610363966045026193115652369
!7.800     0.00015584424160148445631661363976906910135930089110821666602687874703788143473585
!8.600     0.000067544575386678789251476164807145393151299089940651099129594815501432040017895
ENDSUBROUTINE testThirdOrderBickley
!
ENDPROGRAM testExtendedMath
