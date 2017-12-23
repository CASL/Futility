!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testMeshTransfer
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE Strings
  USE MeshTransfer

  IMPLICIT NONE
!
!Check the timer resolution
  CREATE_TEST('Mesh Transfer')

  REGISTER_SUBTEST('Test LP PointVal',TestLPPointVal)
  REGISTER_SUBTEST('Test LP Integral',TestLPIntegral)
  REGISTER_SUBTEST('Test ZP PointVal',TestZPPointVal)
  REGISTER_SUBTEST('Test ZP Integral',TestZPIntegral)

  FINALIZE_TEST()
!
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestLPPointVal()
      REAL(SRK),ALLOCATABLE :: c(:),sol(:),ref(:)
      REAL(SRK) :: x
      INTEGER(SIK) :: i

      ALLOCATE(c(8))
      DO i=1,8
        c(i)=1.0_SRK/REAL(i,SRK)
      ENDDO

      ALLOCATE(sol(11))

      ALLOCATE(ref(11))
      ref(1)=0.6345238095238095238095238095238095238095238095238095238095238095238_SRK
      ref(2)=0.7273055428571428571428571428571428571428571428571428571428571428571_SRK
      ref(3)=0.7314336761904761904761904761904761904761904761904761904761904761905_SRK
      ref(4)=0.7992556095238095238095238095238095238095238095238095238095238095238_SRK
      ref(5)=0.8436587428571428571428571428571428571428571428571428571428571428572_SRK
      ref(6)=0.8636904761904761904761904761904761904761904761904761904761904761905_SRK
      ref(7)=0.9327862095238095238095238095238095238095238095238095238095238095238_SRK
      ref(8)=1.0658213428571428571428571428571428571428571428571428571428571428571_SRK
      ref(9)=1.1812032761904761904761904761904761904761904761904761904761904761905_SRK
      ref(10)=1.3742194095238095238095238095238095238095238095238095238095238095238_SRK
      ref(11)=2.7178571428571428571428571428571428571428571428571428571428571428572_SRK

      DO i=-5,5
        x=REAL(i,SRK)/5.0_SRK
        ASSERT_APPROXEQ(LPPointVal(c,x),ref(i+6),'Legendre Polynomial Point values does not return reference')
      ENDDO
    ENDSUBROUTINE TestLPPointVal
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestLPIntegral()
      REAL(SRK),ALLOCATABLE :: c(:)
      REAL(SRK) :: ref
      INTEGER(SIK) :: i

      ALLOCATE(c(8))
      c(:)=0.0_SRK

      ! Test each moment individually
      c(1)=1.0_SRK
      ref=1.0000_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK), ref,"0th moment Integral")

      c(:)=0.0_SRK
      c(2)=1.0_SRK
      ref=-0.1_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"1st moment Integral")

      c(:)=0.0_SRK
      c(3)=1.0_SRK
      ref=-0.24_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"2nd moment Integral")

      c(:)=0.0_SRK
      c(4)=1.0_SRK
      ref=0.025_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"3rd moment Integral")

      c(:)=0.0_SRK
      c(5)=1.0_SRK
      ref=-0.0216_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"4th moment Integral")

      c(:)=0.0_SRK
      c(6)=1.0_SRK
      ref=0.04798_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"5th moment Integral")

      c(:)=0.0_SRK
      c(7)=1.0_SRK
      ref=0.034896_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"6th moment Integral")

      c(:)=0.0_SRK
      c(8)=1.0_SRK
      ref=-0.0159475_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"7th moment Integral")

      ! Test all the moments together
      DO i=1,8
        c(i)=1.0_SRK/REAL(i,SRK)
      ENDDO

      ref=0.88291837202380952380952380952380952380952380952380952380952380952380952380952_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,0.6_SRK),ref,"Mixed moments - unit width")
      ASSERT_APPROXEQ(LPIntegral(c,0.6_SRK,-0.8_SRK),ref,"Mixed moments - unit width reversed bounds")
      ! Scale solution up to a 20 cm node using equivalent bounds (ie same reference solution)
      ASSERT_APPROXEQ(LPIntegral(c,-8.0_SRK,6.0_SRK,h=20.0_SRK),ref,"Mixed moments - large width")

      ! Shift previous test over 20 cm
      ASSERT_APPROXEQ(LPIntegral(c,12.0_SRK,26.0_SRK,h=20.0_SRK,nodeX=20.0_SRK),ref,"Mixed moments - large width shifted")

      ref=1.0315685111002619311904027199489064514636993408203125000000000000000000000000_SRK
      ASSERT_APPROXEQ(LPIntegral(c,0.1_SRK,0.6_SRK),ref,"Mixed moments - positive bounds")

      ref=0.75727435035807388885586988180875778198242187500000000000000000000000000000000_SRK
      ASSERT_APPROXEQ(LPIntegral(c,-0.8_SRK,-0.3_SRK),ref,"Mixed moments - negative bounds")

    ENDSUBROUTINE TestLPIntegral
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestZPPointVal()
      REAL(SRK),ALLOCATABLE :: c(:),ref(:)
      REAL(SRK) :: x
      INTEGER(SIK) :: i

      ALLOCATE(c(8))
      DO i=1,8
        c(i)=1.0_SRK/REAL(i,SRK)
      ENDDO

      ALLOCATE(ref(11))
      ref(1)=0.634523809523809523809523809523809523809523809523809523809_SRK
      ref(2)=0.669168466491932857142857142857142857142857142857142857142_SRK
      ref(3)=0.723459283672502857142857142857142857142857142857142857142_SRK
      ref(4)=0.730170025011319523809523809523809523809523809523809523809_SRK
      ref(5)=0.718921933651382857142857142857142857142857142857142857142_SRK
      ref(6)=0.763206263950892857142857142857142857142857142857142857142_SRK
      ref(7)=0.831615464343649523809523809523809523809523809523809523809_SRK
      ref(8)=0.860670908609852857142857142857142857142857142857142857142_SRK
      ref(9)=0.981735365541302857142857142857142857142857142857142857142_SRK
      ref(10)=1.190718355576999523809523809523809523809523809523809523809_SRK
      ref(11)=2.717857142857142857142857142857142857142857142857142857142_SRK

      DO i=0,10
        x=REAL(i,SRK)/10.0_SRK
        ASSERT_APPROXEQ(ZPPointVal(c,x),ref(i+1),'Legendre Polynomial Point values does not return reference')
      ENDDO
    ENDSUBROUTINE TestZPPointVal
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestZPIntegral()
      REAL(SRK),ALLOCATABLE :: c(:)
      REAL(SRK) :: ref
      INTEGER(SIK) :: i

      ALLOCATE(c(8))
      c(:)=0.0_SRK

      ! Test each moment individually
      c(1)=1.0_SRK
      ref=1.0000_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK), ref,"0th moment Integral")

      c(:)=0.0_SRK
      c(2)=1.0_SRK
      ref=-0.55_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"1st moment Integral")

      c(:)=0.0_SRK
      c(3)=1.0_SRK
      ref=-0.0098_SRK
      ASSERT_SOFTEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,1.0E-15_SRK,"2nd moment Integral")

      c(:)=0.0_SRK
      c(4)=1.0_SRK
      ref=0.308825_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"3rd moment Integral")

      c(:)=0.0_SRK
      c(5)=1.0_SRK
      ref=-0.252552860_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"4th moment Integral")

      c(:)=0.0_SRK
      c(6)=1.0_SRK
      ref=0.0376286570_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"5th moment Integral")

      c(:)=0.0_SRK
      c(7)=1.0_SRK
      ref=0.0929795207320_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"6th moment Integral")

      c(:)=0.0_SRK
      c(8)=1.0_SRK
      ref=-0.072698399532550_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"7th moment Integral")

      ! Test all the moments together
      DO i=1,8
        c(i)=1.0_SRK/REAL(i,SRK)
      ENDDO

      ref=0.75889594290109791666666666666666666666666666666667_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.3_SRK,0.6_SRK),ref,"Mixed moments - unit width")
      ASSERT_APPROXEQ(ZPIntegral(c,0.6_SRK,0.3_SRK),ref,"Mixed moments - unit width reversed bounds")
      ! Scale solution up to a 20 cm node using equivalent bounds (ie same reference solution)
      ASSERT_APPROXEQ(ZPIntegral(c,6.0_SRK,12.0_SRK,h=20.0_SRK),ref,"Mixed moments - large width")

      ref=0.78953633305305698860168457031250000000000000000000_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.1_SRK,0.75_SRK),ref,"Mixed moments - positive bounds")


      ref=1.0000000000000000000_SRK
      ASSERT_APPROXEQ(ZPIntegral(c,0.0_SRK,1.00_SRK),ref,"Mixed moments - full integral")
    ENDSUBROUTINE TestZPIntegral
!
ENDPROGRAM testMeshTransfer
