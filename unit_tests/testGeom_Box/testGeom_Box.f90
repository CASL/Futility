!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testGeom_Box
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE IntrType
  USE Constants_Conversion
  USE ParameterLists
  USE Geom

  IMPLICIT NONE

  TYPE(PointType) :: point
  TYPE(PointType) :: points(2),points2(2),points3(2)
  TYPE(LineType) :: line1,lines(2)
  TYPE(OBBoxType) :: box,box2,boxs(2)
  INTEGER(SIK) :: i
  REAL(SRK) :: d
  REAL(SRK) :: e_2d(2),e_3d(3),u1_2d(2),u2_2d(2),u3_2d(2)
  REAL(SRK) :: u1_3d(3),u2_3d(3),u3_3d(3)
  LOGICAL(SBK) :: bool

  CREATE_TEST('Test Geom')
  CALL eParams%setQuietMode(.TRUE.)
  CALL eParams%setStopOnError(.FALSE.)

  REGISTER_SUBTEST('Test OB Box',TestOBBox)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestOBBox
      TYPE(ParamType) :: params
    !Test for clear
      COMPONENT_TEST('%clear()')
      CALL point%init(DIM=3,X=0.1_SRK,Y=0.2_SRK,Z=0.3_SRK)
      box%p0=point
!      ALLOCATE(box%u(3,3),box%e(3))
      box%u(1,:)=(/0.5_SRK,0.5_SRK,0._SRK/)
      box%u(2,:)=(/-0.5_SRK,0.5_SRK,0._SRK/)
      box%u(3,:)=(/0._SRK,0._SRK,1._SRK/)
      box%e=1._SRK
      CALL box%clear()

      bool = .NOT.(ANY(box%u /= 0.0_SRK) .OR. ANY(box%e /= 0.0_SRK) &
                   .OR. box%p0%dim /= 0 .OR. ALLOCATED(box%p0%coord))
      ASSERT(bool,'box%clear()')

    !Test for set
      COMPONENT_TEST('%set()')
      !input check
      CALL box%clear()
      CALL point%clear()
      CALL point%init(DIM=3,X=0.1_SRK,Y=0.2_SRK,Z=0.3_SRK)
      CALL box%set(point,(/1._SRK,1._SRK,1._SRK/),(/1._SRK,0._SRK,0._SRK/), &
        (/0._SRK,1._SRK,1._SRK/),(/0._SRK,0._SRK,0._SRK/))
      CALL box%set(point,(/1._SRK,1._SRK/),(/1._SRK,0._SRK/), &
        (/0._SRK,1._SRK/),(/0._SRK,1._SRK/))
      CALL box%set(point,(/1._SRK,1._SRK,1._SRK/),(/1._SRK,0._SRK,1._SRK/), &
        (/0._SRK,1._SRK,1._SRK/))
      CALL box%set(point,(/1._SRK,1._SRK/),(/1._SRK,0._SRK/),(/1._SRK,1._SRK/))
      CALL box%set(point,(/1._SRK,2._SRK/),(/0._SRK,0._SRK/),(/1._SRK,0._SRK/))
      CALL point%clear()
      CALL point%init(DIM=2,X=1._SRK,Y=1._SRK)
      CALL box%set(point,(/-1._SRK,2._SRK/),(/0._SRK,0._SRK/),(/1._SRK,0._SRK/))
      CALL box%set(point,(/1._SRK,-2._SRK/),(/0._SRK,0._SRK/),(/1._SRK,0._SRK/))
      !Test for set 2D
      CALL point%clear()
      CALL box%clear()
      CALL point%init(DIM=2,X=1._SRK,Y=1._SRK)
      e_2d=(/SQRT(2._SRK),2*SQRT(2._SRK)/)
      u1_2d=(/1._SRK,1._SRK/)
      u2_2d=(/-1._SRK,1._SRK/)
      u3_2d=(/-1._SRK,1._SRK/)
      CALL box%set(point,e_2d,u1_2d,u2_2d,u3_2d)
      bool = .NOT.(box%p0 /= point .OR. ANY(box%e(1:2) /= e_2d) &
                   .OR. .NOT.(ALL(box%u(1:2,1) .APPROXEQ. u1_2d/SQRT(2._SRK))) &
                   .OR. .NOT.(ALL(box%u(1:2,2) .APPROXEQ. u2_2d/SQRT(2._SRK))))
      ASSERT(bool, 'box%set(...)')
      !Test for set 3D
      CALL point%clear()
      CALL box%clear()
      CALL point%init(DIM=3,X=1._SRK,Y=1._SRK,Z=1._SRK)
      e_3d=(/SQRT(2._SRK),2*SQRT(2._SRK),1._SRK/)
      u1_3d=(/1._SRK,1._SRK,0._SRK/)
      u2_3d=(/-1._SRK,1._SRK,0._SRK/)
      u3_3d=(/0._SRK,0._SRK,2._SRK/)
      CALL box%set(point,e_3d,u1_3d,u2_3d,u3_3d)
      bool = .NOT.(box%p0 /= point .OR. ANY(box%e /= e_3d) &
                  .OR. ANY(.NOT.(box%u(:,1) .APPROXEQ. u1_3d/SQRT(2._SRK))) &
                  .OR. ANY(.NOT.(box%u(:,2) .APPROXEQ. u2_3d/SQRT(2._SRK))) &
                  .OR. ANY(.NOT.(box%u(:,3) .APPROXEQ. u3_3d*0.5_SRK)) )
      ASSERT(bool, 'box%set(...)')
      CALL box%clear()

    !Test for intersection
      COMPONENT_TEST('%intersectLine()')
      !Bad input
      CALL box%clear()
      CALL line1%clear()
      CALL point%clear()
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL point%init(DIM=2,X=1._SRK,Y=1._SRK)
      u1_2d=(/1._SRK,1._SRK/)
      u2_2d=(/-1._SRK,1._SRK/)
      CALL box%set(point,(/1._SRK,1._SRK/),u1_2d,u2_2d)

      CALL points(1)%init(COORD=(/0._SRK,0._SRK,0._SRK/))
      CALL points(2)%init(COORD=(/0._SRK,8._SRK,0._SRK/))
      CALL line1%set(points(1),points(2))

      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      !Test for intersection 2D
      CALL box%clear()
      CALL point%clear()
      CALL point%init(DIM=2,X=1._SRK,Y=1._SRK)
      u1_2d=(/1._SRK,1._SRK/)
      u2_2d=(/-1._SRK,1._SRK/)
      CALL box%set(point,(/SQRT(2._SRK),8._SRK/),u1_2d,u2_2d)

      !Normal
      CALL line1%clear()
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/0._SRK,0._SRK/))
      CALL points(2)%init(COORD=(/0._SRK,8._SRK/))
      CALL line1%set(points(1),points(2))

      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      bool = .NOT.(ANY(points2(1)%coord /= (/0._SRK,2._SRK/)) &
                   .OR. ANY(points2(2)%coord /= (/0._SRK,4._SRK/)))
      ASSERT(bool, 'box%intersectLine(...)')
      !Parallel
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/1._SRK,1._SRK/))
      CALL points(2)%init(COORD=(/2._SRK,2._SRK/))
      CALL line1%set(points(1),points(2))

      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      bool = .NOT.(ANY(points2(1)%coord /= (/1._SRK,1._SRK/)) &
                   .OR. ANY(points2(2)%coord /= (/2._SRK,2._SRK/)))
      ASSERT(bool, 'box%intersectLine(...)')

      !one point: overlap
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/2._SRK,0._SRK/))
      CALL points(2)%init(COORD=(/2._SRK,2._SRK/))
      CALL line1%set(points(1),points(2))

      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      bool = .NOT.(.NOT.(ANY(points2(1)%coord .APPROXEQ. (/2._SRK,2._SRK/))) &
                   .OR. .NOT.(ANY(points2(2)%coord .APPROXEQ. (/2._SRK,2._SRK/))))
      ASSERT(bool, 'box%intersectLine(...)')
      !one point

      !Now the intersection method of the line and box object takes the line as
      !an infinit line instead of a segment because of the requirement in modular
      !rays type. Maybe an option could be set to specify whether the line is
      !infinit or finit. So the test is left here to be used then.

      !CALL line1%clear()
      !CALL points(1)%clear()
      !CALL points(2)%clear()
      !CALL points(1)%init(COORD=(/0._SRK,0._SRK/))
      !CALL points(2)%init(COORD=(/0._SRK,3._SRK/))
      !CALL line1%set(points(1),points(2))
      !
      !CALL points2(1)%clear()
      !CALL points2(2)%clear()
      !CALL box%intersectLine(line1,points2(1),points2(2))
      !IF(ANY(points2(1)%coord /= (/0._SRK,2._SRK/)) &
      !  .OR. points2(2)%dim /= -3) THEN
      !  WRITE(*,*) 'CALL box%intersectLine(...) FAILED! 2D'
      !  STOP 666
      !ENDIF

      !No point
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/3._SRK,0._SRK/))
      CALL points(2)%init(COORD=(/3._SRK,2._SRK/))
      CALL line1%set(points(1),points(2))

      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      bool = .NOT.(points2(1)%dim /= -3 .OR. points2(2)%dim /= -3)
      ASSERT(bool, 'box%intersectLine(...)')

      !Test for intersection (3D normal box)
      CALL box%clear()
      CALL point%clear()
      CALL point%init(DIM=3,X=1._SRK,Y=1._SRK,Z=0._SRK)
      u1_3d=(/1._SRK,1._SRK,0._SRK/)
      u2_3d=(/-1._SRK,1._SRK,0._SRK/)
      u3_3d=(/0._SRK,0._SRK,1._SRK/)
      CALL box%set(point,(/SQRT(2._SRK),8._SRK,2._SRK/),u1_3d,u2_3d,u3_3d)

      !Normal
      CALL line1%clear()
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/0._SRK,0._SRK,1._SRK/))
      CALL points(2)%init(COORD=(/0._SRK,8._SRK,1._SRK/))
      CALL line1%set(points(2),points(1))

      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      bool = .NOT.(ANY(.NOT.(points2(2)%coord .APPROXEQ. (/0._SRK,2._SRK,1._SRK/))) &
                   .OR. ANY(.NOT.(points2(1)%coord .APPROXEQ. (/0._SRK,4._SRK,1._SRK/))))
      ASSERT(bool, 'box%intersectLine(...)')

      !Parallel
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/1._SRK,1._SRK,1._SRK/))
      CALL points(2)%init(COORD=(/2._SRK,2._SRK,1._SRK/))
      CALL line1%set(points(1),points(2))

      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      bool = .NOT.(ANY(points2(1)%coord /= (/1._SRK,1._SRK,1._SRK/)) &
              .OR. ANY(points2(2)%coord /= (/2._SRK,2._SRK,1._SRK/)))
      ASSERT(bool, 'box%intersectLine(...)')

      !one point: overlap
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/2._SRK,0._SRK,1._SRK/))
      CALL points(2)%init(COORD=(/2._SRK,2._SRK,1._SRK/))
      CALL line1%set(points(1),points(2))

      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      bool = .NOT.(.NOT.(ANY(points2(1)%coord .APPROXEQ. (/2._SRK,2._SRK,1._SRK/))) &
                   .OR. .NOT.(ANY(points2(2)%coord .APPROXEQ. (/2._SRK,2._SRK,1._SRK/))))
      ASSERT(bool, 'box%intersectLine(...)')

      !one point
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/0._SRK,0._SRK,1._SRK/))
      CALL points(2)%init(COORD=(/0._SRK,3._SRK,1._SRK/))
      CALL line1%set(points(1),points(2))

      !CALL points2(1)%clear()
      !CALL points2(2)%clear()
      !CALL box%intersectLine(line1,points2(1),points2(2))
      !IF(.NOT.(ANY(points2(1)%coord .APPROXEQ. (/0._SRK,2._SRK,1._SRK/))) &
      !  .OR. points2(2)%dim /= -3) THEN
      !  WRITE(*,*) 'CALL box%intersectLine(...) FAILED! 3D'
      !  STOP 666
      !ENDIF

      !Test for intersection (3D box: one extention is zero)
      CALL box%clear()
      CALL point%clear()
      CALL point%init(DIM=3,X=1._SRK,Y=1._SRK,Z=0._SRK)
      u1_3d=(/1._SRK,1._SRK,0._SRK/)
      u2_3d=(/-1._SRK,1._SRK,0._SRK/)
      u3_3d=(/0._SRK,0._SRK,1._SRK/)
      CALL box%set(point,(/SQRT(2._SRK),8._SRK,0._SRK/),u1_3d,u2_3d,u3_3d)

      !one point
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/0._SRK,3._SRK,0._SRK/))
      CALL points(2)%init(COORD=(/0._SRK,3._SRK,1._SRK/))
      CALL line1%set(points(1),points(2))

      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      bool = .NOT.(.NOT.(ANY(points2(1)%coord .APPROXEQ. (/0._SRK,3._SRK,0._SRK/))) &
                   .OR. .NOT.(ANY(points2(2)%coord .APPROXEQ. (/0._SRK,3._SRK,0._SRK/))))
      ASSERT(bool, 'box%intersectLine(...)')

      !No point
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/0._SRK,1._SRK,0._SRK/))
      CALL points(2)%init(COORD=(/0._SRK,1._SRK,1._SRK/))
      CALL line1%set(points(1),points(2))

      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      bool = .NOT.(points2(1)%dim /= -3 .AND. points2(2)%dim /= -3)
      ASSERT(bool, 'box%intersectLine(...)')

!
!Test hasPoint
      COMPONENT_TEST('%inside()')
      CALL box%clear()
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      u1_2d=(/1.0_SRK,0.0_SRK/)
      u2_2d=(/0.0_SRK,1.0_SRK/)
      CALL box%set(point,(/4.0_SRK,4.0_SRK/),u1_2d,u2_2d)
      CALL point%clear()
      CALL point%init(DIM=2,X=2.5_SRK,Y=1.6_SRK)
      ASSERT(box%inside(point),'%hasPoint')
      CALL point%clear()
      CALL point%init(DIM=2,X=-10.0_SRK,Y=-0.5_SRK)
      ASSERT(.NOT.box%inside(point),'%hasPoint')

      CALL box%clear()
      CALL point%clear()
      CALL point%init(DIM=2,X=0.5_SRK,Y=0.5_SRK)
      u1_2d=(/1.0_SRK,1.0_SRK/)
      u2_2d=(/-1.0_SRK,1.0_SRK/)
      CALL box%set(point,(/4.0_SRK,4.0_SRK/),u1_2d,u2_2d)
      CALL point%clear()
      CALL point%init(DIM=2,X=0.5_SRK,Y=1.6_SRK)
      ASSERT(box%inside(point),'%hasPoint')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-0.5_SRK)
      ASSERT(.NOT.box%inside(point),'%hasPoint')



      !Test for equivalence operation (implicitly tests assignment operation)
      COMPONENT_TEST('OPERATOR(==)')
      box2=box
      ASSERT(box == box2,'box equivalence')
      box2%u(:,1)=(/0.0_SRK,1.0_SRK,0.0_SRK/)
      ASSERT(.NOT.(box == box2),'box non-equivalence')
!
#ifdef __GFORTRAN__
      WRITE(*,*) 'ELEMENTAL METHODS FOR NON-SCALAR BASE OBJECTS NOT YET SUPPORTED BY COMPILER'
#else
    !Test for clear
      COMPONENT_TEST('Elemental %clear()')
      CALL boxs(1)%clear()
      CALL boxs(2)%clear()
      CALL point%clear()
      CALL point%init(DIM=2,X=1._SRK,Y=1._SRK)
      u1_2d=(/1._SRK,1._SRK/)
      u2_2d=(/-1._SRK,1._SRK/)
      CALL boxs(1)%set(point,(/SQRT(2._SRK),8._SRK/),u1_2d,u2_2d)
      boxs(2)=boxs(1)
      CALL boxs%clear()

      bool = .NOT.(ANY(boxs(1)%u /= 0.0_SRK).OR. ANY(boxs(1)%e /= 0.0_SRK) &
                   .OR. boxs(1)%p0%dim /= 0 .OR. ALLOCATED(boxs(1)%p0%coord) &
                   .OR. ANY(boxs(2)%u /= 0.0_SRK).OR. ANY(boxs(2)%e /= 0.0_SRK) &
                   .OR. boxs(2)%p0%dim /= 0 .OR. ALLOCATED(boxs(2)%p0%coord))
      ASSERT(bool, 'box%clear()')

    !Test for intersection
      COMPONENT_TEST('Elemental %intersectLine()')
      CALL boxs(1)%set(point,(/SQRT(2._SRK),8._SRK/),u1_2d,u2_2d)
      boxs(2)=boxs(1)

      CALL lines%clear()
      CALL points%clear()
      CALL points(1)%init(COORD=(/0._SRK,0._SRK/))
      CALL points(2)%init(COORD=(/0._SRK,8._SRK/))
      CALL lines(1)%set(points(1),points(2))
      lines(2)=lines(1)

      CALL points2%clear()
      CALL points3%clear()
      CALL boxs%intersectLine(lines,points2,points3)
      bool = .NOT.(.NOT.(ANY(points2(1)%coord .APPROXEQ. (/0._SRK,2._SRK/))) &
                   .OR. .NOT.(ANY(points3(1)%coord .APPROXEQ. (/0._SRK,4._SRK/))) &
                   .OR. .NOT.(ANY(points2(2)%coord .APPROXEQ. (/0._SRK,2._SRK/))) &
                   .OR. .NOT.(ANY(points3(2)%coord .APPROXEQ. (/0._SRK,4._SRK/))))
      ASSERT(bool, 'box%intersectLine(...)')
#endif
    ENDSUBROUTINE

ENDPROGRAM testGeom_Box
