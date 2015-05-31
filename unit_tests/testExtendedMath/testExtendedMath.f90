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
PROGRAM testExtendedMath
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE ExtendedMath

  IMPLICIT NONE

  CREATE_TEST('test ExtendedMath')

  REGISTER_SUBTEST('Test Gamma Function',testGammaF)
  REGISTER_SUBTEST('Test Rational Fractions',testRatFrac)
  REGISTER_SUBTEST('Test GCD',testGCD)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGammaF()

      ASSERT(SOFTEQ(GAMMAF(0.25_SRK),3.6256099082219083119_SRK,1.0E-12_SRK), 'GammaF 0.25')
      FINFO() GAMMAF(0.25_SRK)
      ASSERT(SOFTEQ(GAMMAF(0.5_SRK), 1.7724538509055160273_SRK,1.0E-12_SRK), 'GammaF 0.5')
      FINFO() GAMMAF(0.50_SRK)
      ASSERT(SOFTEQ(GAMMAF(0.75_SRK),1.2254167024651776451_SRK,1.0E-12_SRK), 'GammaF 0.75')
      FINFO() GAMMAF(0.75_SRK)
      ASSERT(SOFTEQ(GAMMAF(3.5_SRK), 3.3233509704478425512_SRK,1.0E-12_SRK), 'GammaF 3.5')
      FINFO() GAMMAF(3.50_SRK)
      ASSERT(SOFTEQ(GAMMAF(5.00_SRK),24.00000000000000000_SRK ,1.0E-12_SRK), 'GammaF 5.0')
      FINFO() GAMMAF(5.00_SRK)
      ASSERT(SOFTEQ(GAMMAF(7.00_SRK),720.00000000000000000_SRK,1.0E-10_SRK), 'GammaF 7.0')
      FINFO() GAMMAF(7.00_SRK)
    ENDSUBROUTINE testGammaF
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRatFrac()
      INTEGER(SIK) :: i, N, D
      REAL(SRK) :: dif, tol

      CALL Rational_Fraction(1.5_SRK,1.0E-12_SRK,N,D)
      ASSERT(N==3, 'RatFrac 1.5 num')
      ASSERT(D==2, 'RatFrac 1.5 denom')

      CALL Rational_Fraction(12.8125_SRK,1.0E-12_SRK,N,D)
      ASSERT(N==205, 'RatFrac 12.8125 num')
      ASSERT(D==16, 'RatFrac 12.8125 denom')

      ! Values of coefficients checked against wikipedia:
      ! http://en.wikipedia.org/wiki/Continued_fraction
      !  pi: [3;7,15,1,292,1,1,1,2,1,3,1,…].
      ! by truncating at 1e-12, the series should be [3;7,15,1,292,1,1,1,2,1]
      ! which is 1146408 / 364913 from hand calc
      CALL Rational_Fraction(3.141592653589793_SRK,1.0E-12_SRK,N,D)
      ASSERT(N==1146408, 'RatFrac PI num')
      ASSERT(D==364913, 'RatFrac PI denom')

      ! Values of coefficients checked againt wikipedia:
      !  sqrt(19): [4;2,1,3,1,2,8,2,1,3,1,2,8,…]  series repeats every 6
      ! by truncating to 1e-8 the series should be [4;2,1,3,1,2,8,2,1,3]
      ! which is  16311 / 3742 from hand calc
      CALL Rational_Fraction(SQRT(19.0_SRK),1.0E-8_SRK,N,D)
      ASSERT(N==16311, 'RatFrac SQRT(19) num')
      ASSERT(D==3742, 'RatFrac SQRT(19) denom')

      ! Checking a range of tolerances to ensure it is working correctly
      !  sqrt(2): [1;2,2,2,...] according to wikipedia.  2 the repeats forever
      !  This is also the slowest converging rational fraction
      DO i=1,12
        tol=10.0_SRK**(-REAL(i,SRK))
        CALL Rational_Fraction(SQRT(2.0_SRK),tol,N,D)
        dif=ABS(REAL(N,SRK)/REAL(D,SRK)/SQRT(2.0_SRK) - 1.0_SRK)
        ASSERT(dif<=tol, 'tolerance check')
        FINFO() i, N, D, ABS(SQRT(2.0_SRK)-REAL(N,SRK)/REAL(D,SRK)), tol
      ENDDO

    ENDSUBROUTINE testRatFrac
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGCD()
      ! test error conditions (passing negative or 0 values)
      ASSERT(GreatestCommonDivisor(-1,5)==0, 'GCD errors')
      ASSERT(GreatestCommonDivisor(5,-5)==0, 'GCD errors')
      ASSERT(GreatestCommonDivisor(-1,-5)==0, 'GCD errors')
      ASSERT(GreatestCommonDivisor(0,5)==0, 'GCD errors')
      ASSERT(GreatestCommonDivisor(5,0)==0, 'GCD errors')

      ASSERT(GreatestCommonDivisor(2,3)==1, 'GCD 2,3')
      ASSERT(GreatestCommonDivisor(3,2)==1, 'GCD 3,2')  ! ensure both directions give same answer
      ASSERT(GreatestCommonDivisor(201,3)==3, 'GCD 201,3')
      ASSERT(GreatestCommonDivisor(33,198)==33, 'GCD 33,198')
      ASSERT(GreatestCommonDivisor(6248,3784)==88, 'GCD 6248,3784')
    ENDSUBROUTINE testGCD
ENDPROGRAM testExtendedMath
