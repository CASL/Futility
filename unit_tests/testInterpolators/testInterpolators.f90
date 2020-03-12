!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testInterpolators
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE InterpolatorsModule

IMPLICIT NONE

REAL(SRK) :: label1(3),label2(3),label3(3),Interpolant
REAL(SRK) :: table_1D(3),table_2D(3,3),table_3D(3,3,3)
!
CREATE_TEST('Interpolators')
!
REGISTER_SUBTEST('1-D Interpolator',test1DInterp)
REGISTER_SUBTEST('2-D Interpolator',test2DInterp)
REGISTER_SUBTEST('3-D Interpolator',test3DInterp)

FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE test1DInterp()

  COMPONENT_TEST('1D Asc. In Range')
  label1(1)=1.0_SRK
  label1(2)=2.0_SRK
  label1(3)=3.0_SRK
  table_1D(1)=40.0_SRK
  table_1D(2)=50.0_SRK
  table_1D(3)=75.0_SRK
  Interpolant=Interp(label1,table_1D,1.6_SRK)
  ASSERT_SOFTEQ(Interpolant,46.0_SRK,1e-14_SRK,'Incorrect Interpolant')

  COMPONENT_TEST('1D Asc. Out Of Range')
  Interpolant=Interp(label1,table_1D,0.4_SRK)
  ASSERT_SOFTEQ(Interpolant,40.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,table_1D,3.6_SRK)
  ASSERT_SOFTEQ(Interpolant,75.0_SRK,1e-14_SRK,'Incorrect Interpolant')

  COMPONENT_TEST('Asc. Bullseye')
  Interpolant=Interp(label1,table_1D,2.0_SRK)
  ASSERT_SOFTEQ(Interpolant,50.0_SRK,1e-14_SRK,'Incorrect Interpolant')

  COMPONENT_TEST('1D Des. In Range')
  label1(1)=3.0_SRK
  label1(2)=2.0_SRK
  label1(3)=1.0_SRK
  table_1D(1)=75.0_SRK
  table_1D(2)=50.0_SRK
  table_1D(3)=40.0_SRK
  Interpolant=Interp(label1,table_1D,1.6_SRK)
  ASSERT_SOFTEQ(Interpolant,46.0_SRK,1e-14_SRK,'Incorrect Interpolant')

  COMPONENT_TEST('1D Des. Out Of Range')
  Interpolant=Interp(label1,table_1D,0.4_SRK)
  ASSERT_SOFTEQ(Interpolant,40.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,table_1D,3.6_SRK)
  ASSERT_SOFTEQ(Interpolant,75.0_SRK,1e-14_SRK,'Incorrect Interpolant')

  COMPONENT_TEST('Des. Bullseye')
  Interpolant=Interp(label1,table_1D,2.0_SRK)
  ASSERT_SOFTEQ(Interpolant,50.0_SRK,1e-14_SRK,'Incorrect Interpolant')

ENDSUBROUTINE test1DInterp

SUBROUTINE test2DInterp()

  COMPONENT_TEST('2D Asc. In Range')
  label1(1)=1.0_SRK
  label1(2)=2.0_SRK
  label1(3)=3.0_SRK
  label2(1)=1.0_SRK
  label2(2)=2.0_SRK
  label2(3)=3.0_SRK
  table_2D(1,1)=40.0_SRK
  table_2D(2,1)=50.0_SRK
  table_2D(3,1)=75.0_SRK
  table_2D(1,2)=60.0_SRK
  table_2D(2,2)=80.0_SRK
  table_2D(3,2)=90.0_SRK
  table_2D(1,3)=100.0_SRK
  table_2D(2,3)=110.0_SRK
  table_2D(3,3)=120.0_SRK
  Interpolant=Interp(label1,label2,table_2D,[1.6_SRK,2.4_SRK])
  ASSERT_SOFTEQ(Interpolant,85.6_SRK,1e-14_SRK,'Incorrect Interpolant')

  COMPONENT_TEST('2D Asc. Out Of Range')
  Interpolant=Interp(label1,label2,table_2D,[0.5_SRK,2.4_SRK])
  ASSERT_SOFTEQ(Interpolant,76.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,table_2D,[3.5_SRK,2.4_SRK])
  ASSERT_SOFTEQ(Interpolant,102.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,table_2D,[1.6_SRK,0.5_SRK])
  ASSERT_SOFTEQ(Interpolant,46.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,table_2D,[1.6_SRK,3.5_SRK])
  ASSERT_SOFTEQ(Interpolant,106.0_SRK,1e-14_SRK,'Incorrect Interpolant')

  COMPONENT_TEST('2D Des. In Range')
  label1(1)=3.0_SRK
  label1(2)=2.0_SRK
  label1(3)=1.0_SRK
  label2(1)=3.0_SRK
  label2(2)=2.0_SRK
  label2(3)=1.0_SRK
  table_2D(1,1)=120.0_SRK
  table_2D(2,1)=110.0_SRK
  table_2D(3,1)=100.0_SRK
  table_2D(1,2)=90.0_SRK
  table_2D(2,2)=80.0_SRK
  table_2D(3,2)=60.0_SRK
  table_2D(1,3)=75.0_SRK
  table_2D(2,3)=50.0_SRK
  table_2D(3,3)=40.0_SRK
  Interpolant=Interp(label1,label2,table_2D,[1.6_SRK,2.4_SRK])
  ASSERT_SOFTEQ(Interpolant,85.6_SRK,1e-14_SRK,'Incorrect Interpolant')

  COMPONENT_TEST('2D Des. Out Of Range')
  Interpolant=Interp(label1,label2,table_2D,[0.5_SRK,2.4_SRK])
  ASSERT_SOFTEQ(Interpolant,76.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,table_2D,[3.5_SRK,2.4_SRK])
  ASSERT_SOFTEQ(Interpolant,102.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,table_2D,[1.6_SRK,0.5_SRK])
  ASSERT_SOFTEQ(Interpolant,46.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,table_2D,[1.6_SRK,3.5_SRK])
  ASSERT_SOFTEQ(Interpolant,106.0_SRK,1e-14_SRK,'Incorrect Interpolant')

ENDSUBROUTINE test2DInterp

SUBROUTINE test3DInterp()

  COMPONENT_TEST('3D Asc. In Range')
  label1(1)=1.0_SRK
  label1(2)=2.0_SRK
  label1(3)=3.0_SRK
  label2(1)=1.0_SRK
  label2(2)=2.0_SRK
  label2(3)=3.0_SRK
  label3(1)=1.0_SRK
  label3(2)=2.0_SRK
  label3(3)=3.0_SRK
  table_3D(1,1,1)=40.0_SRK
  table_3D(2,1,1)=50.0_SRK
  table_3D(3,1,1)=75.0_SRK
  table_3D(1,2,1)=60.0_SRK
  table_3D(2,2,1)=80.0_SRK
  table_3D(3,2,1)=90.0_SRK
  table_3D(1,3,1)=100.0_SRK
  table_3D(2,3,1)=110.0_SRK
  table_3D(3,3,1)=120.0_SRK
  table_3D(1,1,2)=40.0_SRK
  table_3D(2,1,2)=50.0_SRK
  table_3D(3,1,2)=75.0_SRK
  table_3D(1,2,2)=60.0_SRK
  table_3D(2,2,2)=80.0_SRK
  table_3D(3,2,2)=90.0_SRK
  table_3D(1,3,2)=100.0_SRK
  table_3D(2,3,2)=110.0_SRK
  table_3D(3,3,2)=120.0_SRK
  table_3D(1,1,3)=40.0_SRK
  table_3D(2,1,3)=50.0_SRK
  table_3D(3,1,3)=75.0_SRK
  table_3D(1,2,3)=60.0_SRK
  table_3D(2,2,3)=80.0_SRK
  table_3D(3,2,3)=90.0_SRK
  table_3D(1,3,3)=100.0_SRK
  table_3D(2,3,3)=110.0_SRK
  table_3D(3,3,3)=120.0_SRK
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,2.4_SRK,1.0_SRK])
  ASSERT_SOFTEQ(Interpolant,85.6_SRK,1e-14_SRK,'Incorrect Interpolant')

  COMPONENT_TEST('3D Asc. Out Of Range')
  Interpolant=Interp(label1,label2,label3,table_3D,[0.5_SRK,2.4_SRK,1.0_SRK])
  ASSERT_SOFTEQ(Interpolant,76.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,label3,table_3D,[3.5_SRK,2.4_SRK,1.0_SRK])
  ASSERT_SOFTEQ(Interpolant,102.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,0.5_SRK,1.0_SRK])
  ASSERT_SOFTEQ(Interpolant,46.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,3.5_SRK,1.0_SRK])
  ASSERT_SOFTEQ(Interpolant,106.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,2.4_SRK,0.5_SRK])
  ASSERT_SOFTEQ(Interpolant,85.6_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,2.4_SRK,3.5_SRK])
  ASSERT_SOFTEQ(Interpolant,85.6_SRK,1e-14_SRK,'Incorrect Interpolant')
  !
  COMPONENT_TEST('3D Des. In Range')
  label1(1)=3.0_SRK
  label1(2)=2.0_SRK
  label1(3)=1.0_SRK
  label2(1)=3.0_SRK
  label2(2)=2.0_SRK
  label2(3)=1.0_SRK
  label3(1)=3.0_SRK
  label3(2)=2.0_SRK
  label3(3)=1.0_SRK
  table_3D(1,1,1)=120.0_SRK
  table_3D(2,1,1)=110.0_SRK
  table_3D(3,1,1)=100.0_SRK
  table_3D(1,2,1)=90.0_SRK
  table_3D(2,2,1)=80.0_SRK
  table_3D(3,2,1)=60.0_SRK
  table_3D(1,3,1)=75.0_SRK
  table_3D(2,3,1)=50.0_SRK
  table_3D(3,3,1)=40.0_SRK
  table_3D(1,1,2)=120.0_SRK
  table_3D(2,1,2)=110.0_SRK
  table_3D(3,1,2)=100.0_SRK
  table_3D(1,2,2)=90.0_SRK
  table_3D(2,2,2)=80.0_SRK
  table_3D(3,2,2)=60.0_SRK
  table_3D(1,3,2)=75.0_SRK
  table_3D(2,3,2)=50.0_SRK
  table_3D(3,3,2)=40.0_SRK
  table_3D(1,1,3)=120.0_SRK
  table_3D(2,1,3)=110.0_SRK
  table_3D(3,1,3)=100.0_SRK
  table_3D(1,2,3)=90.0_SRK
  table_3D(2,2,3)=80.0_SRK
  table_3D(3,2,3)=60.0_SRK
  table_3D(1,3,3)=75.0_SRK
  table_3D(2,3,3)=50.0_SRK
  table_3D(3,3,3)=40.0_SRK
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,2.4_SRK,1.0_SRK])
  ASSERT_SOFTEQ(Interpolant,85.6_SRK,1e-14_SRK,'Incorrect Interpolant')

  COMPONENT_TEST('3D Des. Out Of Range')
  Interpolant=Interp(label1,label2,label3,table_3D,[0.5_SRK,2.4_SRK,1.0_SRK])
  ASSERT_SOFTEQ(Interpolant,76.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,label3,table_3D,[3.5_SRK,2.4_SRK,1.0_SRK])
  ASSERT_SOFTEQ(Interpolant,102.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,0.5_SRK,1.0_SRK])
  ASSERT_SOFTEQ(Interpolant,46.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,3.5_SRK,1.0_SRK])
  ASSERT_SOFTEQ(Interpolant,106.0_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,2.4_SRK,0.5_SRK])
  ASSERT_SOFTEQ(Interpolant,85.6_SRK,1e-14_SRK,'Incorrect Interpolant')
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,2.4_SRK,3.5_SRK])
  ASSERT_SOFTEQ(Interpolant,85.6_SRK,1e-14_SRK,'Incorrect Interpolant')

ENDSUBROUTINE test3DInterp

ENDPROGRAM testInterpolators
