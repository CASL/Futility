!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testIAPWSWaterProperties
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE IAPWSWaterPropertiesModule

IMPLICIT NONE

CREATE_TEST('IAPWS Water Properties')

REGISTER_SUBTEST('IAPWS Region 1',testReg1)
REGISTER_SUBTEST('IAPWS Region 2',testReg2)
REGISTER_SUBTEST('IAPWS Region 3',testReg3)
REGISTER_SUBTEST('IAPWS Region 4',testReg4)
REGISTER_SUBTEST('IAPWS Region 5',testReg5)

REGISTER_SUBTEST('Literature Viscosity',test_viscvt)
REGISTER_SUBTEST('Literature Thermal Conductivity',test_thconvt)

REGISTER_SUBTEST('NIST Saturated Region',test_SaturatedRegion)
REGISTER_SUBTEST('NIST Subcooled Region',test_SubcooledRegion)
REGISTER_SUBTEST('NIST Superheated Region 1',test_SuperheatedRegion1)
REGISTER_SUBTEST('NIST Supercritical Region',test_Superciritical)
REGISTER_SUBTEST('NIST Superheated Region 2',test_SuperheatedRegion2)

FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
! Taken from Table 5 and Table 7
SUBROUTINE testReg1()
  REAL(SRK) :: T, P, h, tol

  tol=1.0E-6_SRK

  T=300.0_SRK
  P=3.0_SRK
  ASSERT_SOFTEQR(vpt(P, T, 1), 0.100215168E-2_SRK, tol, 'vpt')
  ASSERT_SOFTEQR(vpt(P, T, 0), 0.100215168E-2_SRK, tol, 'vpt ireg=0')
  ASSERT_SOFTEQR(hpt(P, T, 1), 0.115331273E3_SRK, tol, 'hpt')
  ASSERT_SOFTEQR(hpt(P, T, 0), 0.115331273E3_SRK, tol, 'hpt ireg=0')
  ASSERT_SOFTEQR(cppt(P, T, 1), 0.417301218E1_SRK, tol, 'cppt')
  ASSERT_SOFTEQR(cppt(P, T, 0), 0.417301218E1_SRK, tol, 'cppt ireg=0')

  T=300.0_SRK
  P=80.0_SRK
  ASSERT_SOFTEQR(vpt(P, T, 1), 0.971180894E-3_SRK, tol, 'vpt')
  ASSERT_SOFTEQR(vpt(P, T, 0), 0.971180894E-3_SRK, tol, 'vpt ireg=0')
  ASSERT_SOFTEQR(hpt(P, T, 1), 0.184142828E3_SRK, tol, 'hpt')
  ASSERT_SOFTEQR(hpt(P, T, 0), 0.184142828E3_SRK, tol, 'hpt ireg=0')
  ASSERT_SOFTEQR(cppt(P, T, 1), 0.401008987E1_SRK, tol, 'cppt')
  ASSERT_SOFTEQR(cppt(P, T, 0), 0.401008987E1_SRK, tol, 'cppt ireg=0')

  T=500.0_SRK
  P=3.0_SRK
  ASSERT_SOFTEQR(vpt(P, T, 1), 0.120241800E-2_SRK, tol, 'vpt')
  ASSERT_SOFTEQR(vpt(P, T, 0), 0.120241800E-2_SRK, tol, 'vpt ireg=0')
  ASSERT_SOFTEQR(hpt(P, T, 1), 0.975542239E3_SRK, tol, 'hpt')
  ASSERT_SOFTEQR(hpt(P, T, 0), 0.975542239E3_SRK, tol, 'hpt ireg=0')
  ASSERT_SOFTEQR(cppt(P, T, 1), 0.465580682E1_SRK, tol, 'cppt')
  ASSERT_SOFTEQR(cppt(P, T, 0), 0.465580682E1_SRK, tol, 'cppt ireg=0')

  P=3.0_SRK
  h=500.0_SRK
  ASSERT_SOFTEQR(tph(P, h, 1), 0.391798509E3_SRK, tol, 'tph')
  ASSERT_SOFTEQR(tph(P, h, 0), 0.391798509E3_SRK, tol, 'tph ireg=0')

  P=80.0_SRK
  h=500.0_SRK
  ASSERT_SOFTEQR(tph(P, h, 1), 0.378108626E3_SRK, tol, 'tph')
  ASSERT_SOFTEQR(tph(P, h, 0), 0.378108626E3_SRK, tol, 'tph ireg=0')

  P=80.0_SRK
  h=1500.0_SRK
  ASSERT_SOFTEQR(tph(P, h, 1), 0.611041229E3_SRK, tol, 'tph')
  ASSERT_SOFTEQR(tph(P, h, 0), 0.611041229E3_SRK, tol, 'tph ireg=0')

ENDSUBROUTINE testReg1
!
!-------------------------------------------------------------------------------
! Taken from Table 15, Table 18 and Table 24
SUBROUTINE testReg2()
  REAL(SRK) :: T, P, h, tol

  tol=1.0E-6_SRK

  T=300.0_SRK
  P= 0.0035_SRK
  ASSERT_SOFTEQR(vpt(P, T, 2), 0.394913866E2_SRK, tol, 'vpt')
  ASSERT_SOFTEQR(vpt(P, T, 0), 0.394913866E2_SRK, tol, 'vpt ireg=0')
  ASSERT_SOFTEQR(hpt(P, T, 2), 0.254991145E4_SRK, tol, 'hpt')
  ASSERT_SOFTEQR(hpt(P, T, 0), 0.254991145E4_SRK, tol, 'hpt ireg=0')
  ASSERT_SOFTEQR(cppt(P, T, 2), 0.191300162E1_SRK, tol, 'cppt')
  ASSERT_SOFTEQR(cppt(P, T, 0), 0.191300162E1_SRK, tol, 'cppt ireg=0')

  T=700.0_SRK
  P=0.0035_SRK
  ASSERT_SOFTEQR(vpt(P, T, 2), 0.923015898E2_SRK, tol, 'vpt')
  ASSERT_SOFTEQR(vpt(P, T, 0), 0.923015898E2_SRK, tol, 'vpt ireg=0')
  ASSERT_SOFTEQR(hpt(P, T, 2), 0.333568375E4_SRK, tol, 'hpt')
  ASSERT_SOFTEQR(hpt(P, T, 0), 0.333568375E4_SRK, tol, 'hpt ireg=0')
  ASSERT_SOFTEQR(cppt(P, T, 2), 0.208141274E1_SRK, tol, 'cppt')
  ASSERT_SOFTEQR(cppt(P, T, 0), 0.208141274E1_SRK, tol, 'cppt ireg=0')

  T=700.0_SRK
  P=30.0_SRK
  ASSERT_SOFTEQR(vpt(P, T, 2), 0.542946619E-2_SRK, tol, 'vpt')
  ASSERT_SOFTEQR(vpt(P, T, 0), 0.542946619E-2_SRK, tol, 'vpt ireg=0')
  ASSERT_SOFTEQR(hpt(P, T, 2), 0.263149474E4_SRK, tol, 'hpt')
  ASSERT_SOFTEQR(hpt(P, T, 0), 0.263149474E4_SRK, tol, 'hpt ireg=0')
  ASSERT_SOFTEQR(cppt(P, T, 2), 0.103505092E2_SRK, tol, 'cppt')
  ASSERT_SOFTEQR(cppt(P, T, 0), 0.103505092E2_SRK, tol, 'cppt ireg=0')

! Currently the Metastable vapor range is not implemented so the tests in
! Table 18 are not valid.
!  T=450.0_SRK
!  P= 1.0_SRK
!  ASSERT_SOFTEQR(vpt(P, T, 2), 0.192516540_SRK, tol, 'vpt')
!  ASSERT_SOFTEQR(vpt(P, T, 0), 0.192516540_SRK, tol, 'vpt ireg=0')
!  ASSERT_SOFTEQR(hpt(P, T, 2), 0.276881115E4_SRK, tol, 'hpt')
!  ASSERT_SOFTEQR(hpt(P, T, 0), 0.276881115E4_SRK, tol, 'hpt ireg=0')
!  ASSERT_SOFTEQR(cppt(P, T, 2), 0.276349265E1_SRK, tol, 'cppt')
!  ASSERT_SOFTEQR(cppt(P, T, 0), 0.276349265E1_SRK, tol, 'cppt ireg=0')
!
!  T=440.0_SRK
!  P=1.0_SRK
!  ASSERT_SOFTEQR(vpt(P, T, 2), 0.186212297_SRK, tol, 'vpt')
!  ASSERT_SOFTEQR(vpt(P, T, 0), 0.186212297_SRK, tol, 'vpt ireg=0')
!  ASSERT_SOFTEQR(hpt(P, T, 2), 0.274015123E4_SRK, tol, 'hpt')
!  ASSERT_SOFTEQR(hpt(P, T, 0), 0.274015123E4_SRK, tol, 'hpt ireg=0')
!  ASSERT_SOFTEQR(cppt(P, T, 2), 0.298166443E1_SRK, tol, 'cppt')
!  ASSERT_SOFTEQR(cppt(P, T, 0), 0.298166443E1_SRK, tol, 'cppt ireg=0')
!
!  T=450.0_SRK
!  P=1.5_SRK
!  ASSERT_SOFTEQR(vpt(P, T, 2), 0.121685206_SRK, tol, 'vpt')
!  ASSERT_SOFTEQR(vpt(P, T, 0), 0.121685206_SRK, tol, 'vpt ireg=0')
!  ASSERT_SOFTEQR(hpt(P, T, 2), 0.272134539E4_SRK, tol, 'hpt')
!  ASSERT_SOFTEQR(hpt(P, T, 0), 0.272134539E4_SRK, tol, 'hpt ireg=0')
!  ASSERT_SOFTEQR(cppt(P, T, 2), 0.362795578E1_SRK, tol, 'cppt')
!  ASSERT_SOFTEQR(cppt(P, T, 0), 0.362795578E1_SRK, tol, 'cppt ireg=0')

  P=0.001_SRK
  h=3000.0_SRK
  ASSERT_SOFTEQR(tph(P, h, 2), 0.534433241E3_SRK, tol, 'tph')
  ASSERT_SOFTEQR(tph(P, h, 0), 0.534433241E3_SRK, tol, 'tph ireg=0')

  P=3.0_SRK
  h=3000.0_SRK
  ASSERT_SOFTEQR(tph(P, h, 2), 0.575373370E3_SRK, tol, 'tph')
  ASSERT_SOFTEQR(tph(P, h, 0), 0.575373370E3_SRK, tol, 'tph ireg=0')

  P=3.0_SRK
  h=4000.0_SRK
  ASSERT_SOFTEQR(tph(P, h, 2), 0.101077577E4_SRK, tol, 'tph')
  ASSERT_SOFTEQR(tph(P, h, 0), 0.101077577E4_SRK, tol, 'tph ireg=0')

  P=5.0_SRK
  h=3500.0_SRK
  ASSERT_SOFTEQR(tph(P, h, 2), 0.801299102E3_SRK, tol, 'tph')
  ASSERT_SOFTEQR(tph(P, h, 0), 0.801299102E3_SRK, tol, 'tph ireg=0')

  P=5.0_SRK
  h=4000.0_SRK
  ASSERT_SOFTEQR(tph(P, h, 2), 0.101531583E4_SRK, tol, 'tph')
  ASSERT_SOFTEQR(tph(P, h, 0), 0.101531583E4_SRK, tol, 'tph ireg=0')

  P=25.0_SRK
  h=3500.0_SRK
  ASSERT_SOFTEQR(tph(P, h, 2), 0.875279054E3_SRK, tol, 'tph')
  ASSERT_SOFTEQR(tph(P, h, 0), 0.875279054E3_SRK, tol, 'tph ireg=0')

  P=40.0_SRK
  h=2700.0_SRK
  ASSERT_SOFTEQR(tph(P, h, 2), 0.743056411E3_SRK, tol, 'tph')
  ASSERT_SOFTEQR(tph(P, h, 0), 0.743056411E3_SRK, tol, 'tph ireg=0')

  P=60.0_SRK
  h=2700.0_SRK
  ASSERT_SOFTEQR(tph(P, h, 2), 0.791137067E3_SRK, tol, 'tph')
  ASSERT_SOFTEQR(tph(P, h, 0), 0.791137067E3_SRK, tol, 'tph ireg=0')

  P=60.0_SRK
  h=3200.0_SRK
  ASSERT_SOFTEQR(tph(P, h, 2), 0.882756860E3_SRK, tol, 'tph')
  ASSERT_SOFTEQR(tph(P, h, 0), 0.882756860E3_SRK, tol, 'tph ireg=0')

ENDSUBROUTINE testReg2
!
!-------------------------------------------------------------------------------
! Taken from Table 33 inverted to use provided interface (table provides T, rho
! with P as solution variable)
SUBROUTINE testReg3()
  REAL(SRK) :: T, P, tol

  tol=1.0E-6_SRK

  T=650.0_SRK
  P= 0.255837018E2_SRK
  ASSERT_SOFTEQR(vpt(P, T, 3), 0.2E-2_SRK, tol, 'vpt')
  ASSERT_SOFTEQR(vpt(P, T, 0), 0.2E-2_SRK, tol, 'vpt ireg=0')
  ASSERT_SOFTEQR(hpt(P, T, 3), 0.186343019E4_SRK, tol, 'hpt')
  ASSERT_SOFTEQR(hpt(P, T, 0), 0.186343019E4_SRK, tol, 'hpt ireg=0')
  ASSERT_SOFTEQR(cppt(P, T, 3), 0.138935717E2_SRK, tol, 'cppt')
  ASSERT_SOFTEQR(cppt(P, T, 0), 0.138935717E2_SRK, tol, 'cppt ireg=0')

  T=650.0_SRK
  P=0.222930643E2_SRK
  ASSERT_SOFTEQR(vpt(P, T, 3), 0.5E-2_SRK, tol, 'vpt')
  ASSERT_SOFTEQR(vpt(P, T, 0), 0.5E-2_SRK, tol, 'vpt ireg=0')
  ASSERT_SOFTEQR(hpt(P, T, 3), 0.237512401E4_SRK, tol, 'hpt')
  ASSERT_SOFTEQR(hpt(P, T, 0), 0.237512401E4_SRK, tol, 'hpt ireg=0')
  ASSERT_SOFTEQR(cppt(P, T, 3), 0.446579342E2_SRK, tol, 'cppt')
  ASSERT_SOFTEQR(cppt(P, T, 0), 0.446579342E2_SRK, tol, 'cppt ireg=0')

  T=750.0_SRK
  P=0.783095639E2_SRK
  ASSERT_SOFTEQR(vpt(P, T, 3), 0.2E-2_SRK, tol, 'vpt')
  ASSERT_SOFTEQR(vpt(P, T, 0), 0.2E-2_SRK, tol, 'vpt ireg=0')
  ASSERT_SOFTEQR(hpt(P, T, 3), 0.225868845E4_SRK, tol, 'hpt')
  ASSERT_SOFTEQR(hpt(P, T, 0), 0.225868845E4_SRK, tol, 'hpt ireg=0')
  ASSERT_SOFTEQR(cppt(P, T, 3), 0.634165359E1_SRK, tol, 'cppt')
  ASSERT_SOFTEQR(cppt(P, T, 0), 0.634165359E1_SRK, tol, 'cppt ireg=0')

ENDSUBROUTINE testReg3
!
!-------------------------------------------------------------------------------
! Taken from Table 35 and Table 36
SUBROUTINE testReg4()
  REAL(SRK) :: tol

  tol=1.0E-6_SRK

  ASSERT_SOFTEQR(psattn(300.0_SRK), 0.353658941E-2_SRK, tol, 'psattn')
  ASSERT_SOFTEQR(psattn(500.0_SRK), 0.263889776E1_SRK,  tol, 'psattn')
  ASSERT_SOFTEQR(psattn(600.0_SRK), 0.123443146E2_SRK,  tol, 'psattn')

  ASSERT_SOFTEQR(tsatpn( 0.1_SRK), 0.372755919E3_SRK, tol, 'tsatpn')
  ASSERT_SOFTEQR(tsatpn( 1.0_SRK), 0.453035632E3_SRK, tol, 'tsatpn')
  ASSERT_SOFTEQR(tsatpn(10.0_SRK), 0.584149488E3_SRK, tol, 'tsatpn')

ENDSUBROUTINE testReg4
!
!-------------------------------------------------------------------------------
! Taken from Table 42
SUBROUTINE testReg5()
    REAL(SRK) :: T, P, tol

    tol=1.0E-6_SRK

    T=1500.0_SRK
    P=0.5_SRK
    ASSERT_SOFTEQR(vpt(P, T, 5), 0.138455090E1_SRK, tol, 'vpt')
    !ASSERT_SOFTEQR(vpt(P, T, 0), 0.138455090E1_SRK, tol, 'vpt ireg=0')
    ASSERT_SOFTEQR(hpt(P, T, 5), 0.521976855E4_SRK, tol, 'hpt')
    !ASSERT_SOFTEQR(hpt(P, T, 0), 0.521976855E4_SRK, tol, 'hpt ireg=0')
    ASSERT_SOFTEQR(cppt(P, T, 5), 0.261609445E1_SRK, tol, 'cppt')
    !ASSERT_SOFTEQR(cppt(P, T, 0), 0.261609445E1_SRK, tol, 'cppt ireg=0')

    T=1500.0_SRK
    P=30.0_SRK
    ASSERT_SOFTEQR(vpt(P, T, 5), 0.230761299E-1_SRK, tol, 'vpt')
    !ASSERT_SOFTEQR(vpt(P, T, 0), 0.230761299E-1_SRK, tol, 'vpt ireg=0')
    ASSERT_SOFTEQR(hpt(P, T, 5), 0.516723514E4_SRK, tol, 'hpt')
    !ASSERT_SOFTEQR(hpt(P, T, 0), 0.516723514E4_SRK, tol, 'hpt ireg=0')
    ASSERT_SOFTEQR(cppt(P, T, 5), 0.272724317E1_SRK, tol, 'cppt')
    !ASSERT_SOFTEQR(cppt(P, T, 0), 0.272724317E1_SRK, tol, 'cppt ireg=0')

    T=2000.0_SRK
    P=30.0_SRK
    ASSERT_SOFTEQR(vpt(P, T, 5), 0.311385219E-1_SRK, tol, 'vpt')
    !ASSERT_SOFTEQR(vpt(P, T, 0), 0.311385219E-1_SRK, tol, 'vpt ireg=0')
    ASSERT_SOFTEQR(hpt(P, T, 5), 0.657122604E4_SRK, tol, 'hpt')
    !ASSERT_SOFTEQR(hpt(P, T, 0), 0.657122604E4_SRK, tol, 'hpt ireg=0')
    ASSERT_SOFTEQR(cppt(P, T, 5), 0.288569882E1_SRK, tol, 'cppt')
    !ASSERT_SOFTEQR(cppt(P, T, 0), 0.288569882E1_SRK, tol, 'cppt ireg=0')

ENDSUBROUTINE testReg5
!
!-------------------------------------------------------------------------------
SUBROUTINE test_viscvt()
  ! Unit Test comparing function to LITERATURE values
  ! These reference values are provided in:
  ! J. Cooper, "The International Association for the Properties of Water and Steam", Berlin,
  ! Germany, 2008.
  ! For the verification of computer program implementations of the tables.
  ! See Table 4.
  REAL(SRK), PARAMETER, DIMENSION(11) :: & ! Temp. in [K], Density in [kg/m^3], and viscosity [uPa-s] (from literature)
      T = [298.15E0_SRK, 298.15E0_SRK, 373.15E0_SRK, 433.15E0_SRK, 433.15E0_SRK, 873.15E0_SRK, 873.15E0_SRK, 873.15E0_SRK, 1173.15E0_SRK, 1173.15E0_SRK, 1173.15E0_SRK], &
      Rho = [998.0E0_SRK, 1200.0E0_SRK, 1000.0E0_SRK, 1.0E0_SRK, 1000.0E0_SRK, 1.0E0_SRK, 100.0E0_SRK, 600.0E0_SRK, 1.0E0_SRK, 100.0E0_SRK, 400.0E0_SRK], &
      U_lit = [889.735100_SRK, 1437.649467_SRK, 307.883622_SRK, 14.538324_SRK, 217.685358_SRK, 32.619287_SRK, 35.802262_SRK, 77.430195_SRK, 44.217245_SRK, 47.640433_SRK, 64.154608_SRK]
  INTEGER(SIK) :: i
  REAL(SRK) :: tol=1.0E-6_SRK

  DO i = 1, SIZE(T)
    ASSERT_SOFTEQR(viscvt(1.0E0/Rho(i), T(i)), U_lit(i)*1.0E-6_SRK, tol, "viscosity")
  ENDDO
ENDSUBROUTINE test_viscvt
!
!-------------------------------------------------------------------------------
SUBROUTINE test_thconvt()
  ! Unit Test comparing function to LITERATURE values
  ! Values should be EXACT
  REAL(SRK), PARAMETER, DIMENSION(6) :: & ! Table 7,8,9 covering regions 1,2,and 3 respectively
      P = [20.00E0_SRK, 50.0E0_SRK, 0.3E0_SRK, 50.0E0_SRK, 22.064E0_SRK, 22.064E0_SRK], &
      T = [620.0E0_SRK, 620.0E0_SRK, 650.0E0_SRK, 800.0E0_SRK, 647.35E0_SRK, 647.35E0_SRK], &
      Rho = [0.613227777E3_SRK, 0.699226043E3_SRK, 0.100452141E1_SRK, 0.218030012E3_SRK, 222.0E0_SRK, 322.0E0_SRK], &
      K_lit = [0.481485195E3_SRK, 0.545038940E3_SRK, 0.522311024E2_SRK, 0.177709914E3_SRK, 0.366879411E3_SRK, 0.124182415E4_SRK]
  INTEGER(SIK) :: i
  REAL(SRK) :: tol=2.0E-5_SRK

  DO i = 1, SIZE(T)
    ASSERT_SOFTEQR(thconvt(P(i), 1.0E0_SRK/Rho(i), T(i), 0), k_lit(i)*1.0E-3_SRK, tol, 'th conductivity')
  ENDDO
ENDSUBROUTINE test_thconvt
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!                         NIST Subroutines
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! Gold Values from: NIST at  http://webbook.nist.gov/chemistry/fluid/
!-------------------------------------------------------------------------------
subroutine test_SaturatedRegion()
  ! IREG == 9
  ! Critical pressure at 22.064 MPa
  ! Critical temperature at 374 C
  ! The saturated equations are most accurate below 350 C.
  ! Between 350-550 [C], the B23 (F23 in if97.f) is used to estimate the saturation pressure.
  ! While the code is self consitent in its usage, external data tables due not agree very well
  ! with the equation between 350 [C] and 374 [C] . The NIST saturated data tables end at 374 [C]
  ! For example, F23 calculates Psat(365.75 [C])=18.407 [MPa] but NIST lists it as 20.0 [MPa]
  ! While the saturation pressure was a little off, the other material properties appeared to pass
  ! within tolerances.

  ! Gold Values
  REAL(SRK), PARAMETER, DIMENSION(5) :: &
      P = [0.01_SRK, 0.1_SRK, 1.00_SRK, 10.0_SRK, 16.0_SRK], &    ! Pressure [ MPa ]
      T = [318.956_SRK, 372.76_SRK, 453.03_SRK, 584.15_SRK, 620.5_SRK], &    ! Temperature [ C ]
      h1 = [191.81_SRK, 417.40_SRK, 762.52_SRK, 1408.1_SRK, 1649.7_SRK], &    ! Fluid Enthalpy [kJ/kg]
      h2 = [2583.9_SRK, 2674.95_SRK, 2777.1_SRK, 2725.5_SRK, 2580.8_SRK], &    ! Vapor Enthalpy [kJ/kg]
      cp1 = [4.1805_SRK, 4.2152_SRK, 4.4045_SRK, 6.1237_SRK, 9.4633_SRK], &    ! Fluid Specific Heat [kJ/kg-C]
      cp2 = [1.9400_SRK, 2.0784_SRK, 2.711_SRK, 7.1408_SRK, 15.198_SRK], &    ! Vapor Specific Heat [kJ/kg-C]
      k1 = [0.63838_SRK, 0.67897_SRK, 0.67337_SRK, 0.52683_SRK, 0.45303_SRK], &   ! Fluid Thermal Conductivity [W/m-K]
      k2 = [0.020037_SRK, 0.025053_SRK, 0.036427_SRK, 0.076543_SRK, 0.12804_SRK], &   ! Vapor Thermal Conductivity [W/m-K]
      v1 = [0.0010103_SRK, 0.0010432_SRK, 0.0011272_SRK, 0.0014526_SRK, 0.0017094_SRK], & ! Fluid Specific Volume m^3/kg
      v2 = [14.670_SRK, 1.6939000_SRK, 0.1943600_SRK, 0.0180300_SRK, 0.0093088_SRK], & ! Vapor Specific Volume m^3/kg
      u1 = [0.00058764_SRK, 0.00028291_SRK, 0.00015024_SRK, 8.1795E-05_SRK, 6.7146E-05_SRK], & ! Fluid Dynamic Viscosity Pa-s
      u2 = [1.0486E-05_SRK, 1.2256E-05_SRK, 1.5021E-05_SRK, 2.0267E-05_SRK, 2.3442E-05_SRK], & ! Vapor Dynamic Viscosity Pa-s
      st = [0.068643_SRK, 0.058988_SRK, 0.042217_SRK, 0.011865_SRK, 0.0041700_SRK] ! Fluid surface tension [N/m]
  ! Custom variables
  REAL(SRK) :: tol=1.0E-3
  INTEGER(SIK) :: i, ireg

  DO i = 1, SIZE(P)
    CALL regsopt(P(i), T(i), ireg)
    ASSERT_EQ(ireg, 9_SIK, 'regsopt')
    ASSERT_SOFTEQR(tph(P(i),h1(i), 0), T(i), tol, 'Tsat liq')
    ASSERT_SOFTEQR(tph(P(i),h2(i), 0), T(i), tol, 'Tsat vap')
    ASSERT_SOFTEQR(tsatpn(P(i)), T(i), tol, 'Tsat')
    ASSERT_SOFTEQR(hpt(P(i), T(i), 1), h1(i), tol, 'enthalpy liq')
    ASSERT_SOFTEQR(hpt(P(i), T(i), 2), h2(i), tol, 'enthalpy vap')
    ASSERT_SOFTEQR(vpt(P(i), T(i), 1), v1(i), tol, 'specific volume liq')
    ASSERT_SOFTEQR(vpt(P(i), T(i), 2), v2(i), tol, 'specific volume vap')
    ASSERT_SOFTEQR(cppt(P(i), T(i), 1), cp1(i), 1.0E-2_SRK,  'specific heat liq')
    ASSERT_SOFTEQR(cppt(P(i), T(i), 2), cp2(i), 1.0E-2_SRK,  'specific heat vap')
    ASSERT_SOFTEQR(thconvt(P(i), vpt(P(i), T(i), 1), T(i), 1), k1(i), 0.045_SRK, 'thermal conductivity liq')
    ASSERT_SOFTEQR(thconvt(P(i), vpt(P(i), T(i), 2), T(i), 2), k2(i), 0.045_SRK, 'thermal conductivity vap')
    ASSERT_SOFTEQR(viscvt(vpt(P(i), T(i), 1), T(i)), u1(i), 0.011_SRK, 'viscosity liq')
    ASSERT_SOFTEQR(viscvt(vpt(P(i), T(i), 2), T(i)), u2(i), 0.011_SRK, 'viscosity vap')
    ASSERT_SOFTEQR(surftt(T(i)), st(i), tol, 'surface tension')
  ENDDO
ENDSUBROUTINE test_SaturatedRegion
!
!-------------------------------------------------------------------------------
SUBROUTINE test_SubcooledRegion()
  ! IREG == 1
  ! T<350[C] P>P_sat
  ! Gold Values
  REAL(SRK), PARAMETER, DIMENSION(4) :: &
      P = [0.1_SRK, 0.1_SRK, 10.0_SRK, 10.0_SRK], & ! Pressure [ MPa ]
      T = [283.15_SRK, 363.15_SRK, 323.15_SRK, 573.15_SRK], & ! Temperature [ C ]
      h = [42.118_SRK, 377.06_SRK, 217.94_SRK, 1343.3_SRK], & ! Enthalpy [kJ/kg]
      cp = [4.1952_SRK, 4.2052_SRK, 4.1592_SRK, 5.6807_SRK], & ! Specific Heat [kJ/kg-C]
      k = [0.58005_SRK, 0.67527_SRK, 0.64819_SRK, 0.55067_SRK], & ! Thermal Conductivity [W/m-K]
      v = [0.0010003_SRK, 0.0010359_SRK, 0.0010078_SRK, 0.0013980_SRK], & ! Specific Volume [kg/m^3]
      u = [0.0013059_SRK, 0.00031441_SRK, 0.00054863_SRK, 8.6460E-05_SRK]   ! Dynamic Viscosity [Pa-S]
  INTEGER(SIK) :: i, ireg
  REAL(SRK) :: tol=1.0E-3_SRK

  DO i=1,SIZE(P)
    CALL regsopt(P(i), T(i), ireg)
    ASSERT_EQ(ireg, 1_SIK, 'regsopt')
    CALL regsph(P(i), h(i), ireg)
    ASSERT_EQ(ireg, 1_SIK, 'regsph')
    ASSERT_SOFTEQR(tph(P(i), h(i), 0), T(i), tol, 'Tsat')
    ASSERT_SOFTEQR(vpt(P(i), T(i), 0), v(i), tol, 'vpt')
    ASSERT_SOFTEQR(cppt(P(i), T(i), 0), cp(i), tol, 'cppt')
    ASSERT_SOFTEQR(thconvt(P(i), v(i), T(i), 0), k(i), tol*8.0_SRK, 'thconvt')
    ASSERT_SOFTEQR(viscvt(v(i), T(i), P(i), 0), u(i), tol, 'viscvt')
  ENDDO
ENDSUBROUTINE test_SubcooledRegion
!
!-------------------------------------------------------------------------------
SUBROUTINE test_SuperheatedRegion1()
  ! IREG == 2
  ! T<800[C] P>P_sat

  ! Gold Values
  REAL(SRK), PARAMETER, DIMENSION(6) :: &
      P = [0.01_SRK, 0.1_SRK, 1.0_SRK, 10.0_SRK, 10.0_SRK, 10.0_SRK], & ! Pressure [ MPa ]
      T = [373.15_SRK, 383.15_SRK, 523.15_SRK, 623.15_SRK, 873.15_SRK, 1073.15_SRK], & ! Temperature [ K ]
      h = [2687.5_SRK, 2696.3_SRK, 2943.1_SRK, 2924.0_SRK, 3625.8_SRK, 4114.5_SRK], & ! Enthalpy [kJ/kg]
      cp = [1.9058_SRK, 2.0415_SRK, 2.2106_SRK, 4.0117_SRK, 2.4576_SRK, 2.4571_SRK], &   ! Specific Heat [kJ/kg-C]
      k = [0.024009_SRK, 0.025767_SRK, 0.040516_SRK, 0.068090_SRK, 0.087139_SRK, 0.11317_SRK], & ! Thermal Conductivity [W/m-K]
      v = [17.196_SRK, 1.7447_SRK, 0.23275_SRK, 0.022440_SRK, 0.038378_SRK, 0.048629_SRK], & ! Specific Volume [kg/m^3]
      u = [1.2344E-05_SRK, 1.2644E-05_SRK, 1.8046E-05_SRK, 2.2151E-05_SRK, 3.3089E-05_SRK, 4.0884E-05_SRK]   ! Dynamic Viscosity [Pa-S]
  INTEGER(SIK) :: i, ireg
  REAL(SRK) :: tol=1.0E-3_SRK

  DO i=1,SIZE(P)
    CALL regsopt(P(i), T(i), ireg)
    ASSERT_EQ(ireg, 2, 'regsopt')
    CALL regsph(P(i), h(i), ireg)
    ASSERT_EQ(ireg, 2, 'regsph')
    ASSERT_SOFTEQR(tph(P(i), h(i), 0), T(i), tol, 'tph')
    ASSERT_SOFTEQR(hpt(P(i), T(i), 0), h(i), tol, 'hpt')
    ASSERT_SOFTEQR(vpt(P(i), T(i), 0), v(i), tol, 'vpt')
    ASSERT_SOFTEQR(cppt(P(i), T(i), 0), cp(i), tol, 'cppt')
    ASSERT_SOFTEQR(thconvt(P(i), v(i), T(i), 0), k(i), 0.015_SRK, 'thermal conductivity')
    ASSERT_SOFTEQR(viscvt(v(i), T(i)), u(i), 0.002_SRK, 'viscosity')
  ENDDO
ENDSUBROUTINE test_SuperheatedRegion1
!
!-------------------------------------------------------------------------------
SUBROUTINE test_Superciritical()
  ! IREG == 3
  ! 590[C]>T>350[C] P<P_sat
  ! P<100 MPa or
  ! Mostly Supercritical Values

  ! Gold Values
  REAL(SRK), PARAMETER, DIMENSION(4) :: &
      P = [20.0_SRK, 30.0_SRK, 100.0_SRK, 100.0_SRK], & ! Pressure [ MPa ]
      T = [633.15_SRK, 673.15_SRK, 748.15_SRK, 823.15_SRK], & ! Temperature [ C ]
      h = [1740.1_SRK, 2152.8_SRK, 2178.5_SRK, 2595.9_SRK], & ! Enthalpy [kJ/kg]
      cp = [11.475_SRK, 25.868_SRK, 5.4372_SRK, 5.5516_SRK], & ! Specific Heat [kJ/kg-C]
      v = [0.0018248_SRK, 0.0027978_SRK, 0.0017491_SRK, 0.0022495_SRK], & ! Specific Volume [m^3/kg]
      k = [0.43345_SRK, 0.33204_SRK, 0.43664_SRK, 0.31935_SRK], & ! Thermal Conductivity [W/m-K]
      u = [6.2803E-05_SRK, 4.3937E-05_SRK, 7.0322E-05_SRK, 5.9123E-05_SRK]   ! Dynamic Viscosity [Pa-S]
  INTEGER(SIK) :: i, ireg
  REAL(SRK) :: tol=1.0E-3_SRK

  DO i=1,SIZE(P)
    CALL regsopt(P(i), T(i), ireg)
    ASSERT_EQ(ireg, 3, 'regsopt')
    CALL regsph(P(i), h(i), ireg)
    ASSERT_EQ(ireg, 3, 'regsph')
    ASSERT_SOFTEQR(tph(P(i), h(i), 0), T(i), tol, 'tph')
    ASSERT_SOFTEQR(hpt(P(i), T(i), 0), h(i), tol, 'hpt')
    ASSERT_SOFTEQR(vpt(P(i), T(i), 0), v(i), tol, 'vpt')
    ASSERT_SOFTEQR(cppt(P(i), T(i), 0), cp(i), 0.005_SRK, 'cppt')
    ASSERT_SOFTEQR(thconvt(P(i), v(i), T(i), 0), k(i), 0.075_SRK, 'thermal conductivity')
    ASSERT_SOFTEQR(viscvt(v(i), T(i)), u(i), 0.007_SRK, 'viscosity')
    ASSERT_SOFTEQR(pvt3n(v(i), T(i)), P(i), tol, 'pvt')
  ENDDO
ENDSUBROUTINE test_Superciritical
!
!-------------------------------------------------------------------------------
SUBROUTINE test_SuperheatedRegion2()
  ! IREG == 5
  ! Originally: 2000[C]>T>800[C] P>P_sat P<10.0[MPa]
  ! Modified:    900[C]>T>800[C] P>P_sat P<10.0[MPa], dyn. visc. and therm. cond. are limiting factors
  ! is limited to T<1000.1[C]
  ! Gold Values
  REAL(SRK), PARAMETER, DIMENSION(6) :: &
      P = [0.1_SRK, 1.0_SRK, 10.0_SRK, 0.1_SRK, 1.0_SRK, 10.0_SRK], & ! Pressure [ MPa ]
      T = [1123.15_SRK, 1123.15_SRK, 1123.15_SRK, 1273.15_SRK, 1273.15_SRK, 1273.15_SRK], & ! Temperature [ K ]
      h = [4278.2_SRK, 4274.6_SRK, 4237.8_SRK, 4642.6_SRK, 4639.9_SRK, 4613.8_SRK], & ! Enthalpy [kJ/kg]
      cp = [2.3781_SRK, 2.3866_SRK, 2.4747_SRK, 2.4782_SRK, 2.4839_SRK, 2.5411_SRK], & ! Specific Heat [kJ/kg-C]
      k = [0.11446_SRK, 0.11486_SRK, 0.11980_SRK, 0.13633_SRK, 0.13655_SRK, 0.13972_SRK], & ! Thermal Conductivity [W/m-K]
      v = [5.1828_SRK, 0.51762_SRK, 0.051099_SRK, 5.8754_SRK, 0.58721_SRK, 0.058390_SRK], & ! Specific Volume [kg/m^3]
      u = [4.2243E-05_SRK, 4.2280E-05_SRK, 4.2733E-05_SRK, 4.7665E-05_SRK, 4.7696E-05_SRK, 4.8072E-05_SRK]   ! Dynamic Viscosity [Pa-S]
  INTEGER(SIK) :: i, ireg
  REAL(SRK) :: tol=1.0E-3_SRK

  ! Test only values below 900 C
  DO i=1,3 !SIZE(P)
    CALL regsopt(P(i), T(i), ireg)
    ASSERT_EQ(ireg, 5, 'regsopt')
    CALL regsph(P(i), h(i), ireg)
    ASSERT_EQ(ireg, 5, 'regsph')
    ASSERT_SOFTEQR(tph(P(i), h(i), 0), T(i), tol, 'tph')
    ASSERT_SOFTEQR(hpt(P(i), T(i), 0), h(i), tol, 'hpt')
    ASSERT_SOFTEQR(vpt(P(i), T(i), 0), v(i), tol, 'vpt')
    ASSERT_SOFTEQR(cppt(P(i), T(i), 0), cp(i), tol, 'cppt')
    ASSERT_SOFTEQR(thconvt(P(i), v(i), T(i), 0), k(i), 0.025_SRK, 'thermal conductivity')
    ASSERT_SOFTEQR(viscvt(v(i), T(i)), u(i), 0.011_SRK, 'viscosity')
  ENDDO
ENDSUBROUTINE test_SuperheatedRegion2
!
ENDPROGRAM testIAPWSWaterProperties