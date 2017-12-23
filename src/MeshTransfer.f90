!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module implementing some basic sorting algorithms.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>
!> @author Ben Collins
!>    @date 12/22/2017
!>
!> @par Revisions:
!>   (07/06/2014) - Benjamin Collins
!>   - Added additional interface for qsort (QuickSort) to give better
!>           performance when needed
!>   (3/16/2016) - Dan Jabaay
!>   - Added sorting of one array based on the sorting of another.  This
!>     functionality will help with more applicable sorting cases.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE MeshTransfer
#include "DBC.h"
  USE DBC
  USE IntrType
  USE Strings
  IMPLICIT NONE

  PUBLIC :: LPPointVal
  PUBLIC :: LPIntegral
  PUBLIC :: ZPPointVal
  PUBLIC :: ZPIntegral

  REAL(SRK),PARAMETER :: ONE=1.0_SRK
  REAL(SRK),PARAMETER :: TWO=2.0_SRK

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Returns value of an arbitrary order Legendre Polynomial with coefficients coef
!> @param coef  The coefficients of the Legendre expansion
!> @param globX The value of x in the global coordinate
!> @param h     The height of the node, OPTIONAL (if not provided a height of 2 is assumed)
!> @param nodeX The center of the node, OPTIONAL (if not provided a position of 0 is assumed)
!>
!> This routine returns the value of an arbitrary order Legendre expansion at a
!> specific point for NEM and SANM nodal methods.
!>
    FUNCTION LPPointVal(coef,globX,h,nodeX) RESULT(y)
      REAL(SRK),DIMENSION(:),INTENT(IN) :: coef
      REAL(SRK),INTENT(IN) :: globX
      REAL(SRK),INTENT(IN),OPTIONAL :: h
      REAL(SRK),INTENT(IN),OPTIONAL :: nodeX
      REAL(SRK) :: y
      ! Local variables
      INTEGER(SIK) :: N,i
      REAL(SRK) :: w, x, Pn, Pnm1, Pnm2

      w=TWO
      x=globX

      IF(PRESENT(h)) w=h
      IF(PRESENT(nodeX)) x=globX-nodeX

      N=SIZE(coef)

      !project into local coordinate system
      x=TWO*x/w
      y=coef(1)
      Pnm2=ONE
      IF(N > 1) THEN
        y=y+coef(2)*x
        Pnm1=x
        DO i=2,N-1
          Pn=(TWO*REAL(i,SRK)-ONE)/REAL(i,SRK)*x*Pnm1-(REAL(i,SRK)-ONE)/REAL(i,SRK)*Pnm2
          y=y+coef(i+1)*Pn
          Pnm2=Pnm1
          Pnm1=Pn
        ENDDO
      ENDIF
    ENDFUNCTION LPPointVal
!
!-------------------------------------------------------------------------------
!> @brief Returns value of the integral of an arbitrary order Legendre Polynomial with coefficients coef
!> @param coef  The coefficients of the Legendre expansion
!> @param a     The beginning of the integral range
!> @param b     The end of the integral range
!> @param h     The height of the node, OPTIONAL (if not provided a height of 2 is assumed)
!> @param nodeX The center of the node, OPTIONAL (if not provided a position of 0 is assumed)
!>
!> This routine returns the value of the integral of an arbitrary order Legendre expansion on a
!> specific range a to b for NEM and SANM nodal methods.
!>
    FUNCTION LPIntegral(coef,a,b,h,nodeX) RESULT(y)
      REAL(SRK),DIMENSION(:),INTENT(IN) :: coef
      REAL(SRK),INTENT(IN) :: a
      REAL(SRK),INTENT(IN) :: b
      REAL(SRK),INTENT(IN),OPTIONAL :: h
      REAL(SRK),INTENT(IN),OPTIONAL :: nodeX
      REAL(SRK) :: y
      ! Local variables
      INTEGER(SIK) :: N,m
      REAL(SRK) :: w, xa, xb, Pna, Pnm1a, Pnm2a, Pnb, Pnm1b, Pnm2b
      REAL(SRK),ALLOCATABLE :: c(:)

      w=TWO
      xa=a
      xb=b

      IF(PRESENT(h)) w=h
      IF(PRESENT(nodeX)) xa=a-nodeX
      IF(PRESENT(nodeX)) xb=b-nodeX

      REQUIRE(ABS(xb-xa) <= w)

      N=SIZE(coef)

      ALLOCATE(c(0:N-1))
      c(:)=0.0_SRK
      c(0:N-1)=coef(1:N)

      !project into local coordinate system
      xa=TWO*xa/w
      xb=TWO*xb/w
      IF(N > 1) THEN
        Pnm1a=xa
        Pnm1b=xb
        Pnm2a=ONE
        Pnm2b=ONE
        !this is m=1
        y=c(0)*(xb-xa)
        DO m=2,N
          Pna=(TWO*REAL(m,SRK)-ONE)/REAL(m,SRK)*xa*Pnm1a-(REAL(m,SRK)-ONE)/REAL(m,SRK)*Pnm2a
          Pnb=(TWO*REAL(m,SRK)-ONE)/REAL(m,SRK)*xb*Pnm1b-(REAL(m,SRK)-ONE)/REAL(m,SRK)*Pnm2b
          y=y+c(m-1)/(TWO*REAL(m,SRK)-ONE)*(Pnb-Pna-Pnm2b+Pnm2a)
          Pnm2a=Pnm1a
          Pnm2b=Pnm1b
          Pnm1a=Pna
          Pnm1b=Pnb
        ENDDO
      ENDIF
      y=y/(xb-xa)
      DEALLOCATE(c)
    ENDFUNCTION LPIntegral
!
!-------------------------------------------------------------------------------
!> @brief Returns value of an arbitrary order Zernike Polynomial with coefficients coef
!> @param coef  The coefficients of the Zernike expansion
!> @param r     The value of r in the global coordinate
!> @param h     The height of the node, OPTIONAL (if not provided a height of 1 is assumed)
!>
!> This routine returns the value of an arbitrary order Zernike expansion at a
!> specific point.
!>
    FUNCTION ZPPointVal(coef,r,h) RESULT(y)
      REAL(SRK),DIMENSION(:),INTENT(IN) :: coef
      REAL(SRK),INTENT(IN) :: r
      REAL(SRK),INTENT(IN),OPTIONAL :: h
      REAL(SRK) :: y
      ! Local variables
      REAL(SRK) :: w

      w=ONE

      IF(PRESENT(h)) w=h

      y=LPPointVal(coef,TWO*(r/w)*(r/w)-ONE)
    ENDFUNCTION ZPPointVal
!
!-------------------------------------------------------------------------------
!> @brief Returns value of an arbitrary order Zernike Polynomial with coefficients coef
!> @param coef  The coefficients of the Zernike expansion
!> @param a     The beginning of the integral range
!> @param b     The end of the integral range
!> @param h     The height of the node, OPTIONAL (if not provided a height of 1 is assumed)
!>
!> This routine returns the value of an arbitrary order Zernike expansion at a
!> specific point.
!>
    FUNCTION ZPIntegral(coef,a,b,h) RESULT(y)
      REAL(SRK),DIMENSION(:),INTENT(IN) :: coef
      REAL(SRK),INTENT(IN) :: a
      REAL(SRK),INTENT(IN) :: b
      REAL(SRK),INTENT(IN),OPTIONAL :: h
      REAL(SRK) :: y
      ! Local variables
      REAL(SRK) :: w

      w=ONE

      IF(PRESENT(h)) w=h
      REQUIRE(ABS(b-a) <= w)

      y=LPIntegral(coef,TWO*(a/w)*(a/w)-ONE,TWO*(b/w)*(b/w)-ONE)
    ENDFUNCTION ZPIntegral
!
ENDMODULE MeshTransfer
