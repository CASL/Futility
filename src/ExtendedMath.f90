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
!> @brief The global module for collecting all public members of
!> extended math routines.
!>
!>
!> @author Ben Collins
!>    @date 05/30/2015
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ExtendedMath

  USE IntrType
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: GAMMAF
  PUBLIC :: Rational_Fraction
  PUBLIC :: GreatestCommonDivisor

  REAL(SRK),PARAMETER,PRIVATE :: PI=3.141592653589793_SRK

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
      IF(lev>2) THEN
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

      IF(a1>0 .AND. a2>0) THEN
        IF(a1 < a2) THEN
          a=a2
          b=a1
        ELSE
          a=a1
          b=a2
        ENDIF

        DO
          c=MOD(a,b)
          IF(c==0) EXIT
          a=b
          b=c
        ENDDO
      ENDIF
    ENDFUNCTION GreatestCommonDivisor

!
ENDMODULE ExtendedMath