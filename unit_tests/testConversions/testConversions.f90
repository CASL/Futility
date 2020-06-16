!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testConversions
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE Constants_Conversion

IMPLICIT NONE

CREATE_TEST("Conversions")

REGISTER_SUBTEST('Temperatures',testTemperatures)

FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testTemperatures()
  REAL(SRK) :: in_temp(4),out_temp(4)

  COMPONENT_TEST('K_to_C')
  in_temp = (/0.0_SRK,273.15_SRK,565.0_SRK,1200.0_SRK/)
  out_temp = K_to_C(in_temp)
  ASSERT_APPROXEQ(out_temp(1),-273.15_SRK,'0 K_to_C (1D)')
  ASSERT_APPROXEQ(out_temp(2),0.0_SRK,'273.15 K_to_C (1D)')
  ASSERT_APPROXEQ(out_temp(3),291.85_SRK,'565 K_to_C (1D)')
  ASSERT_APPROXEQ(out_temp(4),926.85_SRK,'1200 K_to_C (1D)')
  ASSERT_APPROXEQ(K_to_C(0.0_SRK),-273.15_SRK,'0 K_to_C')
  ASSERT_APPROXEQ(K_to_C(273.15_SRK),0.0_SRK,'273.15 K_to_C')
  ASSERT_APPROXEQ(K_to_C(565.0_SRK),291.85_SRK,'565 K_to_C')
  ASSERT_APPROXEQ(K_to_C(1200.0_SRK),926.85_SRK,'1200 K_to_C')

  COMPONENT_TEST('C_to_K')
  in_temp = (/-273.15_SRK,0.0_SRK,291.85_SRK,926.85_SRK/)
  out_temp = C_to_K(in_temp)
  ASSERT_APPROXEQ(out_temp(1),0.0_SRK,'-273.15 C_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(2),273.15_SRK,'0 C_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(3),565.0_SRK,'291.85 C_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(4),1200.0_SRK,'926.85 C_to_K (1D)')
  ASSERT_APPROXEQ(C_to_K(-273.15_SRK),0.0_SRK,'-273.15 C_to_K')
  ASSERT_APPROXEQ(C_to_K(0.0_SRK),273.15_SRK,'0 C_to_K')
  ASSERT_APPROXEQ(C_to_K(291.85_SRK),565.0_SRK,'291.85 C_to_K')
  ASSERT_APPROXEQ(C_to_K(926.85_SRK),1200.0_SRK,'926.85 C_to_K')

  COMPONENT_TEST('F_to_C')
  in_temp = (/32.0_SRK,68.0_SRK,212.0_SRK,557.33_SRK/)
  out_temp = F_to_C(in_temp)
  ASSERT_APPROXEQ(out_temp(1),0.0_SRK,'32 F_to_C (1D)')
  ASSERT_APPROXEQ(out_temp(2),20.0_SRK,'68 F_to_C (1D)')
  ASSERT_APPROXEQ(out_temp(3),100.0_SRK,'212 F_to_C (1D)')
  ASSERT_APPROXEQ(out_temp(4),291.85_SRK,'557.33 F_to_C (1D)')
  ASSERT_APPROXEQ(F_to_C(32.0_SRK),0.0_SRK,'32 F_to_C')
  ASSERT_APPROXEQ(F_to_C(68.0_SRK),20.0_SRK,'68 F_to_C')
  ASSERT_APPROXEQ(F_to_C(212.0_SRK),100.0_SRK,'212 F_to_C')
  ASSERT_APPROXEQ(F_to_C(557.33_SRK),291.85_SRK,'557.33 F_to_C')

  COMPONENT_TEST('C_to_F')
  in_temp = (/0.0_SRK,20.0_SRK,100.0_SRK,291.85_SRK/)
  out_temp = C_to_F(in_temp)
  ASSERT_APPROXEQ(out_temp(1),32.0_SRK,'0 C_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(2),68.0_SRK,'20 C_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(3),212.0_SRK,'100 C_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(4),557.33_SRK,'291.85 C_to_F (1D)')
  ASSERT_APPROXEQ(C_to_F(0.0_SRK),32.0_SRK,'0 C_to_F')
  ASSERT_APPROXEQ(C_to_F(20.0_SRK),68.0_SRK,'20 C_to_F')
  ASSERT_APPROXEQ(C_to_F(100.0_SRK),212.0_SRK,'100 C_to_F')
  ASSERT_APPROXEQ(C_to_F(291.85_SRK),557.33_SRK,'291.85 C_to_F')

  COMPONENT_TEST('F_to_K')
  in_temp = (/32.0_SRK,68.0_SRK,212.0_SRK,557.33_SRK/)
  out_temp = F_to_K(in_temp)
  ASSERT_APPROXEQ(out_temp(1),273.15_SRK,'32 F_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(2),293.15_SRK,'68 F_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(3),373.15_SRK,'212 F_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(4),565.0_SRK,'557.33 F_to_K (1D)')
  ASSERT_APPROXEQ(F_to_K(32.0_SRK),273.15_SRK,'32 F_to_K')
  ASSERT_APPROXEQ(F_to_K(68.0_SRK),293.15_SRK,'68 F_to_K')
  ASSERT_APPROXEQ(F_to_K(212.0_SRK),373.15_SRK,'212 F_to_K')
  ASSERT_APPROXEQ(F_to_K(557.33_SRK),565.0_SRK,'557.33 F_to_K')

  COMPONENT_TEST('K_to_F')
  in_temp = (/273.15_SRK,293.15_SRK,373.15_SRK,565.0_SRK/)
  out_temp = K_to_F(in_temp)
  ASSERT_APPROXEQ(out_temp(1),32.0_SRK,'273.15 K_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(2),68.0_SRK,'293.15 K_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(3),212.0_SRK,'373.15 K_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(4),557.33_SRK,'565 K_to_F (1D)')
  ASSERT_APPROXEQ(K_to_F(273.15_SRK),32.0_SRK,'273.15 K_to_F')
  ASSERT_APPROXEQ(K_to_F(293.15_SRK),68.0_SRK,'293.15 K_to_F')
  ASSERT_APPROXEQ(K_to_F(373.15_SRK),212.0_SRK,'373.15 K_to_F')
  ASSERT_APPROXEQ(K_to_F(565.0_SRK),557.33_SRK,'565 K_to_F')

  COMPONENT_TEST('tempTo_C')
  !To Celsius from Kelvin
  in_temp = (/0.0_SRK,273.15_SRK,565.0_SRK,1200.0_SRK/)
  out_temp = tempTo_C(in_temp,'K')
  ASSERT_APPROXEQ(out_temp(1),-273.15_SRK,'0 K_to_C (1D)')
  ASSERT_APPROXEQ(out_temp(2),0.0_SRK,'273.15 K_to_C (1D)')
  ASSERT_APPROXEQ(out_temp(3),291.85_SRK,'565 K_to_C (1D)')
  ASSERT_APPROXEQ(out_temp(4),926.85_SRK,'1200 K_to_C (1D)')
  !To Celsius from Fahrenheit
  in_temp = (/32.0_SRK,68.0_SRK,212.0_SRK,557.33_SRK/)
  out_temp = tempTo_C(in_temp,'F')
  ASSERT_APPROXEQ(out_temp(1),0.0_SRK,'32 F_to_C (1D)')
  ASSERT_APPROXEQ(out_temp(2),20.0_SRK,'68 F_to_C (1D)')
  ASSERT_APPROXEQ(out_temp(3),100.0_SRK,'212 F_to_C (1D)')
  ASSERT_APPROXEQ(out_temp(4),291.85_SRK,'557.33 F_to_C (1D)')
  !To Celsius from Rankine
  in_temp = (/0.0_SRK,273.15_SRK,546.3_SRK,1475.01_SRK/)
  out_temp = tempTo_C(in_temp,'R')
  ASSERT_APPROXEQF(out_temp(1),-273.15_SRK,'0 R_to_C (1D)')
  ASSERT_APPROXEQF(out_temp(2),-121.4_SRK,'273.15 R_to_C (1D)')
  ASSERT_APPROXEQF(out_temp(3),30.35_SRK,'565 R_to_C (1D)')
  ASSERT_APPROXEQF(out_temp(4),546.3_SRK,'1200 R_to_C (1D)')

  COMPONENT_TEST('tempTo_K')
  !To Kelvin from Celsius
  in_temp = (/-273.15_SRK,0.0_SRK,291.85_SRK,926.85_SRK/)
  out_temp = tempTo_K(in_temp,'C')
  ASSERT_APPROXEQ(out_temp(1),0.0_SRK,'-273.15 C_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(2),273.15_SRK,'0 C_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(3),565.0_SRK,'291.85 C_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(4),1200.0_SRK,'926.85 C_to_K (1D)')
  !To Kelvin from Fahrenheit
  in_temp = (/32.0_SRK,68.0_SRK,212.0_SRK,557.33_SRK/)
  out_temp = tempTo_K(in_temp,'F')
  ASSERT_APPROXEQ(out_temp(1),273.15_SRK,'32 F_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(2),293.15_SRK,'68 F_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(3),373.15_SRK,'212 F_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(4),565.0_SRK,'557.33 F_to_K (1D)')
  !To Kelvin from Rankine
  in_temp = (/0.0_SRK,491.67_SRK,983.34_SRK,1475.01_SRK/)
  out_temp = tempTo_K(in_temp,'R')
  ASSERT_APPROXEQF(out_temp(1),0.0_SRK,'0 R_to_K (1D)')
  ASSERT_APPROXEQF(out_temp(2),273.15_SRK,'491.67 R_to_K (1D)')
  ASSERT_APPROXEQF(out_temp(3),546.3_SRK,'565 R_to_K (1D)')
  ASSERT_APPROXEQF(out_temp(4),819.45_SRK,'1200 R_to_K (1D)')

  COMPONENT_TEST('tempTo_F')
  !To Fahrenheit from Celsius
  in_temp = (/0.0_SRK,20.0_SRK,100.0_SRK,291.85_SRK/)
  out_temp = tempTo_F(in_temp,'C')
  ASSERT_APPROXEQ(out_temp(1),32.0_SRK,'0 C_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(2),68.0_SRK,'20 C_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(3),212.0_SRK,'100 C_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(4),557.33_SRK,'291.85 C_to_F (1D)')
  !To Fahrenheit from Kelvin
  in_temp = (/273.15_SRK,293.15_SRK,373.15_SRK,565.0_SRK/)
  out_temp = tempTo_F(in_temp,'K')
  ASSERT_APPROXEQ(out_temp(1),32.0_SRK,'273.15 K_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(2),68.0_SRK,'293.15 K_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(3),212.0_SRK,'373.15 K_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(4),557.33_SRK,'565 K_to_F (1D)')
  !To Fahrenheit from Rankine
  in_temp = (/0.0_SRK,459.67_SRK,919.34_SRK,1379.01_SRK/)
  out_temp = tempTo_F(in_temp,'R')
  ASSERT_APPROXEQF(out_temp(1),-459.67_SRK,'0 R_to_K (1D)')
  ASSERT_APPROXEQF(out_temp(2),0.0_SRK,'491.67 R_to_K (1D)')
  ASSERT_APPROXEQF(out_temp(3),459.67_SRK,'565 R_to_K (1D)')
  ASSERT_APPROXEQF(out_temp(4),919.34_SRK,'1200 R_to_K (1D)')

  COMPONENT_TEST('tempTo_R')
  !To Celsius from Rankine
  in_temp = (/-273.15_SRK,-121.4_SRK,30.35_SRK,546.3_SRK/)
  out_temp = tempTo_R(in_temp,'C')
  ASSERT_APPROXEQF(out_temp(1),0.0_SRK,'0 C_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(2),273.15_SRK,'273.15 C_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(3),546.3_SRK,'565 C_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(4),1475.01_SRK,'1200 C_to_R (1D)')
  !To Kelvin from Rankine
  in_temp = (/0.0_SRK,273.15_SRK,546.3_SRK,819.45_SRK/)
  out_temp = tempTo_R(in_temp,'K')
  ASSERT_APPROXEQF(out_temp(1),0.0_SRK,'0 K_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(2),491.67_SRK,'491.67 K_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(3),983.34_SRK,'565 K_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(4),1475.01_SRK,'1200 K_to_R (1D)')
  !To Rankine from Fahrenheit
  in_temp = (/-459.67_SRK,0.0_SRK,459.67_SRK,919.34_SRK/)
  out_temp = tempTo_R(in_temp,'F')
  ASSERT_APPROXEQF(out_temp(1),0.0_SRK,'0 F_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(2),459.67_SRK,'491.67 F_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(3),919.34_SRK,'565 F_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(4),1379.01_SRK,'1200 F_to_R (1D)')

  COMPONENT_TEST('convertTemp')
  !To Kelvin from Celsius
  in_temp = (/-273.15_SRK,0.0_SRK,291.85_SRK,926.85_SRK/)
  out_temp = convertTemp(in_temp,'C','K')
  ASSERT_APPROXEQ(out_temp(1),0.0_SRK,'-273.15 C_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(2),273.15_SRK,'0 C_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(3),565.0_SRK,'291.85 C_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(4),1200.0_SRK,'926.85 C_to_K (1D)')
  !To Kelvin from Fahrenheit
  in_temp = (/32.0_SRK,68.0_SRK,212.0_SRK,557.33_SRK/)
  out_temp = convertTemp(in_temp,'F','K')
  ASSERT_APPROXEQ(out_temp(1),273.15_SRK,'32 F_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(2),293.15_SRK,'68 F_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(3),373.15_SRK,'212 F_to_K (1D)')
  ASSERT_APPROXEQ(out_temp(4),565.0_SRK,'557.33 F_to_K (1D)')
  !To Kelvin from Rankine
  in_temp = (/0.0_SRK,491.67_SRK,983.34_SRK,1475.01_SRK/)
  out_temp = convertTemp(in_temp,'R','K')
  ASSERT_APPROXEQF(out_temp(1),0.0_SRK,'0 R_to_K (1D)')
  ASSERT_APPROXEQF(out_temp(2),273.15_SRK,'491.67 R_to_K (1D)')
  ASSERT_APPROXEQF(out_temp(3),546.3_SRK,'565 R_to_K (1D)')
  ASSERT_APPROXEQF(out_temp(4),819.45_SRK,'1200 R_to_K (1D)')
  !To Fahrenheit from Celsius
  in_temp = (/0.0_SRK,20.0_SRK,100.0_SRK,291.85_SRK/)
  out_temp = convertTemp(in_temp,'C','F')
  ASSERT_APPROXEQ(out_temp(1),32.0_SRK,'0 C_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(2),68.0_SRK,'20 C_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(3),212.0_SRK,'100 C_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(4),557.33_SRK,'291.85 C_to_F (1D)')
  !To Fahrenheit from Kelvin
  in_temp = (/273.15_SRK,293.15_SRK,373.15_SRK,565.0_SRK/)
  out_temp = convertTemp(in_temp,'K','F')
  ASSERT_APPROXEQ(out_temp(1),32.0_SRK,'273.15 K_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(2),68.0_SRK,'293.15 K_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(3),212.0_SRK,'373.15 K_to_F (1D)')
  ASSERT_APPROXEQ(out_temp(4),557.33_SRK,'565 K_to_F (1D)')
  !To Fahrenheit from Rankine
  in_temp = (/0.0_SRK,459.67_SRK,919.34_SRK,1379.01_SRK/)
  out_temp = convertTemp(in_temp,'R','F')
  ASSERT_APPROXEQF(out_temp(1),-459.67_SRK,'0 R_to_K (1D)')
  ASSERT_APPROXEQF(out_temp(2),0.0_SRK,'491.67 R_to_K (1D)')
  ASSERT_APPROXEQF(out_temp(3),459.67_SRK,'565 R_to_K (1D)')
  ASSERT_APPROXEQF(out_temp(4),919.34_SRK,'1200 R_to_K (1D)')
  !To Celsius from Rankine
  in_temp = (/-273.15_SRK,-121.4_SRK,30.35_SRK,546.3_SRK/)
  out_temp = convertTemp(in_temp,'C','R')
  ASSERT_APPROXEQF(out_temp(1),0.0_SRK,'0 C_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(2),273.15_SRK,'273.15 C_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(3),546.3_SRK,'565 C_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(4),1475.01_SRK,'1200 C_to_R (1D)')
  !To Kelvin from Rankine
  in_temp = (/0.0_SRK,273.15_SRK,546.3_SRK,819.45_SRK/)
  out_temp = convertTemp(in_temp,'K','R')
  ASSERT_APPROXEQF(out_temp(1),0.0_SRK,'0 K_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(2),491.67_SRK,'491.67 K_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(3),983.34_SRK,'565 K_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(4),1475.01_SRK,'1200 K_to_R (1D)')
  !To Rankine from Fahrenheit
  in_temp = (/-459.67_SRK,0.0_SRK,459.67_SRK,919.34_SRK/)
  out_temp = convertTemp(in_temp,'F','R')
  ASSERT_APPROXEQF(out_temp(1),0.0_SRK,'0 F_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(2),459.67_SRK,'491.67 F_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(3),919.34_SRK,'565 F_to_R (1D)')
  ASSERT_APPROXEQF(out_temp(4),1379.01_SRK,'1200 F_to_R (1D)')

ENDSUBROUTINE testTemperatures
!
ENDPROGRAM testConversions
