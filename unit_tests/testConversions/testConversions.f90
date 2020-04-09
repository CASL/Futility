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

  COMPONENT_TEST('K_to_C')
  ASSERT_EQ(K_to_C(0.0_SRK),-273.15_SRK,'0 K_to_C')
  ASSERT_EQ(K_to_C(273.15_SRK),0.0_SRK,'273.15 K_to_C')
  ASSERT_EQ(K_to_C(565.0_SRK),291.85_SRK,'565 K_to_C')
  ASSERT_EQ(K_to_C(1200.0_SRK),926.85_SRK,'1200 K_to_C')

  COMPONENT_TEST('C_to_K')
  ASSERT_EQ(C_to_K(-273.15_SRK),0.0_SRK,'-273.15 C_to_K')
  ASSERT_EQ(C_to_K(0.0_SRK),273.15_SRK,'0 C_to_K')
  ASSERT_EQ(C_to_K(291.85_SRK),565.0_SRK,'291.85 C_to_K')
  ASSERT_EQ(C_to_K(926.85_SRK),1200.0_SRK,'926.85 C_to_K')

  COMPONENT_TEST('F_to_C')
  ASSERT_EQ(F_to_C(32.0_SRK),0.0_SRK,'32 F_to_C')
  ASSERT_EQ(F_to_C(68.0_SRK),20.0_SRK,'68 F_to_C')
  ASSERT_EQ(F_to_C(212.0_SRK),100.0_SRK,'212 F_to_C')
  ASSERT_EQ(F_to_C(557.33_SRK),291.85_SRK,'557.33 F_to_C')

  COMPONENT_TEST('C_to_F')
  ASSERT_EQ(C_to_F(0.0_SRK),32.0_SRK,'0 C_to_F')
  ASSERT_EQ(C_to_F(20.0_SRK),68.0_SRK,'20 C_to_F')
  ASSERT_EQ(C_to_F(100.0_SRK),212.0_SRK,'100 C_to_F')
  ASSERT_EQ(C_to_F(291.85_SRK),557.33_SRK,'291.85 C_to_F')

  COMPONENT_TEST('F_to_K')
  ASSERT_EQ(F_to_K(32.0_SRK),273.15_SRK,'32 F_to_K')
  ASSERT_EQ(F_to_K(68.0_SRK),293.15_SRK,'68 F_to_K')
  ASSERT_EQ(F_to_K(212.0_SRK),373.15_SRK,'212 F_to_K')
  ASSERT_EQ(F_to_K(557.33_SRK),565.0_SRK,'557.33 F_to_K')

  COMPONENT_TEST('K_to_F')
  ASSERT_EQ(K_to_F(273.15_SRK),32.0_SRK,'273.15 K_to_F')
  ASSERT_EQ(K_to_F(293.15_SRK),68.0_SRK,'293.15 K_to_F')
  ASSERT_EQ(K_to_F(373.15_SRK),212.0_SRK,'373.15 K_to_F')
  ASSERT_EQ(K_to_F(565.0_SRK),557.33_SRK,'565 K_to_F')

ENDSUBROUTINE testTemperatures
!
ENDPROGRAM testConversions
