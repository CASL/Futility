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
  COMPONENT_TEST('1D Ascending In Range')
  label1(1)=1.0_SRK
  label1(2)=2.0_SRK
  label1(3)=3.0_SRK
  table_1D(1)=40.0_SRK
  table_1D(2)=50.0_SRK
  table_1D(3)=75.0_SRK
  Interpolant=Interp(label1,table_1D,1.6_SRK)
  ASSERT(Interpolant .APPROXEQR. 46.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",46.0_SRK

  COMPONENT_TEST('1D Ascending Out Of Range')
  Interpolant=Interp(label1,table_1D,0.4_SRK)
  ASSERT(Interpolant .APPROXEQR. 40.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",40.0_SRK
  Interpolant=Interp(label1,table_1D,3.6_SRK)
  ASSERT(Interpolant .APPROXEQR. 75.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",75.0_SRK

  COMPONENT_TEST('1D Descending In Range')
  label1(1)=3.0_SRK
  label1(2)=2.0_SRK
  label1(3)=1.0_SRK
  table_1D(1)=75.0_SRK
  table_1D(2)=50.0_SRK
  table_1D(3)=40.0_SRK
  Interpolant=Interp(label1,table_1D,1.6_SRK)
  ASSERT(Interpolant .APPROXEQR. 46.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",46.0_SRK

  COMPONENT_TEST('1D Descending Out Of Range')
  Interpolant=Interp(label1,table_1D,0.4_SRK)
  ASSERT(Interpolant .APPROXEQR. 40.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",40.0_SRK
  Interpolant=Interp(label1,table_1D,3.6_SRK)
  ASSERT(Interpolant .APPROXEQR. 75.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",75.0_SRK
ENDSUBROUTINE test1DInterp

SUBROUTINE test2DInterp()
  COMPONENT_TEST('2D Ascending In Range')
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
  ASSERT(Interpolant .APPROXEQR. 85.6_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",85.6_SRK

  COMPONENT_TEST('2D Ascending Out Of Range')
  Interpolant=Interp(label1,label2,table_2D,[0.5_SRK,2.4_SRK])
  ASSERT(Interpolant .APPROXEQR. 76.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",76.0_SRK
  Interpolant=Interp(label1,label2,table_2D,[3.5_SRK,2.4_SRK])
  ASSERT(Interpolant .APPROXEQR. 102.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",102.0_SRK
  Interpolant=Interp(label1,label2,table_2D,[1.6_SRK,0.5_SRK])
  ASSERT(Interpolant .APPROXEQR. 46.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",46.0_SRK
  Interpolant=Interp(label1,label2,table_2D,[1.6_SRK,3.5_SRK])
  ASSERT(Interpolant .APPROXEQR. 106.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",106.0_SRK

  COMPONENT_TEST('2D Descending In Range')
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
  ASSERT(Interpolant .APPROXEQR. 85.6_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",85.6_SRK

  COMPONENT_TEST('2D Descending Out Of Range')
  Interpolant=Interp(label1,label2,table_2D,[0.5_SRK,2.4_SRK])
  ASSERT(Interpolant .APPROXEQR. 76.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",76.0_SRK
  Interpolant=Interp(label1,label2,table_2D,[3.5_SRK,2.4_SRK])
  ASSERT(Interpolant .APPROXEQR. 102.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",102.0_SRK
  Interpolant=Interp(label1,label2,table_2D,[1.6_SRK,0.5_SRK])
  ASSERT(Interpolant .APPROXEQR. 46.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",46.0_SRK
  Interpolant=Interp(label1,label2,table_2D,[1.6_SRK,3.5_SRK])
  ASSERT(Interpolant .APPROXEQR. 106.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",106.0_SRK

ENDSUBROUTINE test2DInterp

SUBROUTINE test3DInterp()
  COMPONENT_TEST('3D Ascending In Range')
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
  ASSERT(Interpolant .APPROXEQR. 85.6_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",85.6_SRK

  COMPONENT_TEST('3D Ascending Out Of Range')
  Interpolant=Interp(label1,label2,label3,table_3D,[0.5_SRK,2.4_SRK,1.0_SRK])
  ASSERT(Interpolant .APPROXEQR. 76.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",76.0_SRK
  Interpolant=Interp(label1,label2,label3,table_3D,[3.5_SRK,2.4_SRK,1.0_SRK])
  ASSERT(Interpolant .APPROXEQR. 102.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",102.0_SRK
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,0.5_SRK,1.0_SRK])
  ASSERT(Interpolant .APPROXEQR. 46.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",46.0_SRK
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,3.5_SRK,1.0_SRK])
  ASSERT(Interpolant .APPROXEQR. 106.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",106.0_SRK
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,2.4_SRK,0.5_SRK])
  ASSERT(Interpolant .APPROXEQR. 85.6_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",85.6_SRK
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,2.4_SRK,3.5_SRK])
  ASSERT(Interpolant .APPROXEQR. 85.6_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",85.6_SRK
  !
  COMPONENT_TEST('3D Descending In Range')
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
  ASSERT(Interpolant .APPROXEQR. 85.6_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",85.6_SRK

  COMPONENT_TEST('3D Descending Out Of Range')
  Interpolant=Interp(label1,label2,label3,table_3D,[0.5_SRK,2.4_SRK,1.0_SRK])
  ASSERT(Interpolant .APPROXEQR. 76.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",76.0_SRK
  Interpolant=Interp(label1,label2,label3,table_3D,[3.5_SRK,2.4_SRK,1.0_SRK])
  ASSERT(Interpolant .APPROXEQR. 102.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",102.0_SRK
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,0.5_SRK,1.0_SRK])
  ASSERT(Interpolant .APPROXEQR. 46.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",46.0_SRK
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,3.5_SRK,1.0_SRK])
  ASSERT(Interpolant .APPROXEQR. 106.0_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",106.0_SRK
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,2.4_SRK,0.5_SRK])
  ASSERT(Interpolant .APPROXEQR. 85.6_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",85.6_SRK
  Interpolant=Interp(label1,label2,label3,table_3D,[1.6_SRK,2.4_SRK,3.5_SRK])
  ASSERT(Interpolant .APPROXEQR. 85.6_SRK,'Incorrect Interpolant')
  FINFO()"Interpolant=", Interpolant, "True value=",85.6_SRK

ENDSUBROUTINE test3DInterp

ENDPROGRAM testInterpolators
