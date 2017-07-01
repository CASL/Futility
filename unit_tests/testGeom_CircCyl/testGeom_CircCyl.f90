!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testGeom_CircCyl
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE IntrType
  USE Constants_Conversion
  USE ParameterLists
  USE Geom

  IMPLICIT NONE

  TYPE(PointType) :: point,point1,point2,point3,point4
  TYPE(LineType) :: line1
  TYPE(CircleType) :: circle1,circle2
  TYPE(CylinderType) :: cylinder1,cylinder2
  LOGICAL(SBK) :: bool

  CREATE_TEST('Test Geom')
  CALL eParams%setQuietMode(.TRUE.)
  CALL eParams%setStopOnError(.FALSE.)

  REGISTER_SUBTEST('Test Circle and Cylinder',TestCircle_and_Cylinder)
  REGISTER_SUBTEST('circle%onSurface',testOnSurface)
  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestCircle_and_Cylinder
!Test clear
      COMPONENT_TEST('Circle %clear()')
      CALL point%clear()
      CALL point2%clear()
      CALL point%init(DIM=2,X=0.1_SRK,Y=0.2_SRK)
      circle1%r=0.5_SRK
      circle1%c=point
      CALL circle1%clear()
      bool = .NOT.(circle1%r /= 0.0_SRK .OR. circle1%c%dim /= 0 .OR. &
                   ALLOCATED(circle1%c%coord))
      ASSERT(bool, 'circle1%clear()')
!
!Test set
      !Error check
      COMPONENT_TEST('Circle %set()')
      CALL circle1%set(point2,0.5_SRK)
      bool=.NOT.(circle1%r /= 0.0_SRK .OR. circle1%c%dim /= 0 .OR. &
                 ALLOCATED(circle1%c%coord))
      ASSERT(bool, 'circle1%set(...)')
      CALL circle1%set(point,-0.5_SRK)
      bool=.NOT.(circle1%r /= 0.0_SRK .OR. circle1%c%dim /= 0 .OR. &
                 ALLOCATED(circle1%c%coord))
      ASSERT(bool, 'circle1%set(...)')
      !Real case
      CALL circle1%set(point,0.5_SRK)
      bool=.NOT.(circle1%r /= 0.5_SRK .OR. circle1%c%dim /= 2 .OR. &
                 circle1%c%coord(1) /= 0.1_SRK .OR. circle1%c%coord(2) /= 0.2_SRK)
      ASSERT(bool, 'circle1%set(...)')
      CALL circle1%clear()
!
!Test intersect
      COMPONENT_TEST('Circle%intersectLine()')
      CALL point%clear()
      CALL circle1%clear()
      CALL line1%clear()
      CALL circle1%intersectLine(line1,point2,point3) !Test bad input

      !Reference circle for other tests
      CALL point%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL circle1%set(point,0.4225_SRK)

      !Test disjoint (pointing away)
      CALL line1%p1%init(DIM=2,X=0.3_SRK,Y=0.4_SRK)
      CALL line1%p2%init(DIM=2,X=0.4_SRK,Y=0.5_SRK)
      CALL circle1%intersectLine(line1,point2,point3)
      bool = .NOT.(point2%dim /= -2 .OR. point3%dim /= -2)
      ASSERT(bool, 'circle1%intersectLine(...) (disjoint')
      CALL line1%clear()

      !Test disjoint (ray misses)
      CALL line1%p1%init(DIM=2,X=-0.4_SRK,Y=0.4_SRK)
      CALL line1%p2%init(DIM=2,X=-0.1_SRK,Y=0.5_SRK)
      CALL circle1%intersectLine(line1,point2,point3)
      bool = .NOT.(point2%dim /= -2 .OR. point3%dim /= -2)
      ASSERT(bool, 'circle1%intersectLine(...) (disjoint 1)')
      CALL line1%clear()

      !Test tangent
      CALL line1%p1%init(DIM=2,X=0.4225_SRK,Y=0.4_SRK)
      CALL line1%p2%init(DIM=2,X=0.4225_SRK,Y=-0.5_SRK)
      CALL circle1%intersectLine(line1,point2,point3)
      bool = .NOT.(point2%dim /= -3 .OR. point3%dim /= -3)
      ASSERT(bool, 'circle1%intersectLine(...) (tangent)')
      CALL line1%clear()

      !Test totally inside
      CALL line1%p1%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL line1%p2%init(DIM=2,X=0.1_SRK,Y=0.1_SRK)
      CALL circle1%intersectLine(line1,point2,point3)
      bool = .NOT.(point2%dim /= 0 .OR. point3%dim /= 0)
      ASSERT(bool, 'circle1%intersectLine(...) (inside)')
      CALL line1%clear()

      !Test 1 point of intersection
      CALL line1%p1%init(DIM=2,X=-0.4_SRK,Y=-0.3_SRK)
      CALL line1%p2%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL circle1%intersectLine(line1,point2,point3)
      bool = .NOT.(ANY(.NOT.(point2%coord .APPROXEQ. (/-0.3380_SRK,-0.2535_SRK/))) &
                   .OR. point3%dim /= 0)
      ASSERT(bool, 'circle1%intersectLine(...) (1-point)')
      CALL line1%clear()

      !Test 2 points of intersection
      CALL line1%p1%init(DIM=2,X=-0.3_SRK,Y=-0.4_SRK)
      CALL line1%p2%init(DIM=2,X=0.4_SRK,Y=0.2_SRK)
      CALL circle1%intersectLine(line1,point2,point3)
      bool=ALL(point2%coord .APPROXEQ. (/-0.239446595040736_SRK,-0.348097081463488_SRK/)) &
        .AND. ALL(point3%coord .APPROXEQ. (/0.380623065628971_SRK,0.183391199110547_SRK/))
      ASSERT(bool,'circle1%intersectLine(...) (2-points)')
      CALL line1%clear()


      COMPONENT_TEST('Circle-Circle Intersect')
      !Test 1  (disjoint)
      CALL point%clear()
      CALL circle1%clear()
      CALL circle2%clear()
      CALL point%init(DIM=2,X=-1.0_SRK,Y=ZERO)
      CALL circle1%set(point,0.5_SRK)
      CALL point%clear()
      CALL point%init(DIM=2,X=1.0_SRK,Y=ZERO)
      CALL circle2%set(point,0.5_SRK)
      CALL circle1%intersectCircle(circle2,point2,point3)
      bool=(point2%dim == -2 .AND. point3%dim == -2)
      ASSERT(bool,'circle1%intersectCircle(...) (disjoint)')
      !Test 2 (tangent)
      CALL point%clear()
      CALL circle1%clear()
      CALL circle2%clear()
      CALL point%init(DIM=2,X=-1.0_SRK,Y=ZERO)
      CALL circle1%set(point,1.0_SRK)
      CALL point%clear()
      CALL point%init(DIM=2,X=1.0_SRK,Y=ZERO)
      CALL circle2%set(point,1.0_SRK)
      CALL circle1%intersectCircle(circle2,point2,point3)
      bool=(point2%dim == -3 .AND. point3%dim == -3)
      ASSERT(bool,'circle1%intersectCircle(...) (tangent)')
      !Test 3 (normal intersection)
      CALL point%clear()
      CALL circle1%clear()
      CALL circle2%clear()
      CALL point%init(DIM=2,X=ZERO,Y=ZERO)
      CALL circle1%set(point,1.0_SRK)
      CALL point%clear()
      CALL point%init(DIM=2,X=ZERO,Y=1.5_SRK)
      CALL circle2%set(point,1.0_SRK)
      CALL circle1%intersectCircle(circle2,point2,point3)
      bool=(point2%dim == 2 .AND. point3%dim == 2)
      ASSERT(bool,'circle1%intersectCircle(...) (Normal Intersect...on x-axis)')
      bool=(point2%coord(2)==point3%coord(2) .AND. point2%coord(1)==-point3%coord(1) &
            .AND. point2%coord(2)==0.75_SRK)
      ASSERT(bool,'circle1%intersectCircle(...) (Normal Intersect...on x-axis)')
      !Test 4 (normal intersection)
      CALL point%clear()
      CALL circle1%clear()
      CALL circle2%clear()
      CALL point%init(DIM=2,X=1.5_SRK,Y=1.5_SRK)
      CALL circle1%set(point,1.5_SRK)
      CALL point%clear()
      CALL point%init(DIM=2,X=2.5_SRK,Y=2.5_SRK)
      CALL circle2%set(point,SQRT(3.25_SRK-2.0_SRK*SQRT(2.0_SRK)))
      CALL circle1%intersectCircle(circle2,point2,point3)
      bool=(point2%dim == 2 .AND. point3%dim == 2)
      ASSERT(bool,'circle1%intersectCircle(...) (Normal Intersect)')
      bool=(point2%coord(1) == 2.0_SRK .AND. point3%coord(2) == 2.0_SRK)
      ASSERT(bool,'circle1%intersectCircle(...) (Normal Intersect)')
      !Test 5 (Overlapped)
      CALL point%clear()
      CALL circle1%clear()
      CALL circle2%clear()
      CALL point%init(DIM=2,X=ZERO,Y=ZERO)
      CALL circle1%set(point,1.5_SRK)
      CALL point%clear()
      CALL point%init(DIM=2,X=0.4_SRK,Y=0.25_SRK)
      CALL circle2%set(point,0.1_SRK)
      CALL circle1%intersectCircle(circle2,point2,point3)
      bool=(point2%dim == -4 .AND. point3%dim == -4)
      ASSERT(bool,'circle1%intersectCircle(...) (Overlapped)')
      !Test 6 (uninitialized circ)
      CALL point%clear()
      CALL circle1%clear()
      CALL circle2%clear()
      CALL point%init(DIM=2,X=0.4_SRK,Y=0.25_SRK)
      CALL circle2%set(point,0.1_SRK)
      CALL circle1%intersectCircle(circle2,point2,point3)
      bool=(point2%dim == -1 .AND. point3%dim == -1)
      ASSERT(bool,'circle1%intersectCircle(...) (uninitialized)')

      COMPONENT_TEST('Arc Intersect')
      CALL point%clear()
      CALL circle1%clear()
      CALL point%init(DIM=2,X=ZERO,Y=ZERO)
      CALL circle1%set(point,1.0_SRK,ANGSTT=ZERO,ANGSTP=HALFPI)
      !Test 1
      CALL line1%p1%init(DIM=2,X=-2.0_SRK,Y=1.5_SRK)
      CALL line1%p2%init(DIM=2,X=2.0_SRK,Y=-2.5_SRK)
      CALL circle1%intersectArcLine(line1,point1,point2,point3,point4)
      bool=(point1%dim == 0 .AND. .NOT.ALLOCATED(point1%coord) .AND. &
            point2%dim == 0 .AND. .NOT.ALLOCATED(point2%coord) .AND. &
            point3%dim == -3 .AND. ALLOCATED(point3%coord) .AND. &
            point4%dim == -3 .AND. ALLOCATED(point4%coord))
      ASSERT(bool,'arc test 1 (no points)')
      !Test 2
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,X=-2.0_SRK,Y=0.5_SRK)
      CALL line1%p2%init(DIM=2,X=2.0_SRK,Y=0.5_SRK)
      CALL circle1%intersectArcLine(line1,point1,point2,point3,point4)
      bool=(point1%dim == 0 .AND. .NOT.ALLOCATED(point1%coord) .AND. &
            point2%dim == 2 .AND. ALL(point2%coord .APPROXEQ. (/SQRT(0.75_SRK),0.5_SRK/)) .AND. &
            point3%dim == -3 .AND. .NOT.ALLOCATED(point3%coord) .AND. &
            point4%dim == 2 .AND. ALL(point4%coord .APPROXEQ. (/0.0_SRK,0.5_SRK/)))
      ASSERT(bool,'arc test 2 (arc and line)')
      !Test 3
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,Y=2.0_SRK,X=0.5_SRK)
      CALL line1%p2%init(DIM=2,Y=-2.0_SRK,X=0.5_SRK)
      CALL circle1%intersectArcLine(line1,point1,point2,point3,point4)
      bool=(point1%dim == 2 .AND. ALL(point1%coord .APPROXEQ. (/0.5_SRK,SQRT(0.75_SRK)/)) .AND. &
            point2%dim == 0 .AND. .NOT.ALLOCATED(point2%coord) .AND. &
            point3%dim == 2 .AND. ALL(point3%coord .APPROXEQ. (/0.5_SRK,0.0_SRK/)) .AND. &
            point4%dim == -3 .AND. .NOT.ALLOCATED(point4%coord))
      ASSERT(bool,'arc test 3 (arc and line)')
      !Test 4
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,X=-2.0_SRK,Y=2.5_SRK)
      CALL line1%p2%init(DIM=2,X=2.0_SRK,Y=-1.5_SRK)
      CALL circle1%intersectArcLine(line1,point1,point2,point3,point4)
      bool=(point1%dim == 0 .AND. .NOT.ALLOCATED(point1%coord) .AND. &
            point2%dim == 0 .AND. .NOT.ALLOCATED(point2%coord) .AND. &
            point3%dim == 2 .AND. ALL(point3%coord .APPROXEQ. (/0.5_SRK,0.0_SRK/)) .AND. &
            point4%dim == 2 .AND. ALL(point4%coord .APPROXEQ. (/0.0_SRK,0.5_SRK/)))
      ASSERT(bool,'arc test 4 (line and line)')
      !Test 5
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,X=0.5_SRK,Y=-0.1_SRK)
      CALL line1%p2%init(DIM=2,X=0.5_SRK,Y=0.1_SRK)
      CALL circle1%intersectArcLine(line1,point1,point2,point3,point4)
      bool=(point1%dim == 0 .AND. .NOT.ALLOCATED(point1%coord) .AND. &
            point2%dim == 0 .AND. .NOT.ALLOCATED(point2%coord) .AND. &
            point3%dim == 2 .AND. ALL(point3%coord .APPROXEQ. (/0.5_SRK,0.0_SRK/)) .AND. &
            point4%dim == -3 .AND. .NOT.ALLOCATED(point2%coord))
      ASSERT(bool,'arc test 5 (one line inside)')
      !Test 6
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,X=0.5_SRK,Y=-2.0_SRK)
      CALL line1%p2%init(DIM=2,X=0.5_SRK,Y=0.1_SRK)
      CALL circle1%intersectArcLine(line1,point1,point2,point3,point4)
      bool=(point1%dim == 0 .AND. .NOT.ALLOCATED(point1%coord) .AND. &
            point2%dim == 0 .AND. .NOT.ALLOCATED(point2%coord) .AND. &
            point3%dim == 2 .AND. ALL(point3%coord .APPROXEQ. (/0.5_SRK,0.0_SRK/)) .AND. &
            point4%dim == -3 .AND. .NOT.ALLOCATED(point2%coord))
      ASSERT(bool,'arc test 6 (one line over)')
      !Test 7
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,X=-2.0_SRK,Y=2.0_SRK)
      CALL line1%p2%init(DIM=2,X=2.0_SRK,Y=-2.0_SRK)
      CALL circle1%intersectArcLine(line1,point1,point2,point3,point4)
      bool=(point1%dim == 0 .AND. .NOT.ALLOCATED(point1%coord) .AND. &
            point2%dim == 0 .AND. .NOT.ALLOCATED(point2%coord) .AND. &
            point3%dim == -3 .AND. ALL(point3%coord .APPROXEQ. (/0.0_SRK,0.0_SRK/)) .AND. &
            point4%dim == -3 .AND. ALL(point4%coord .APPROXEQ. (/0.0_SRK,0.0_SRK/)))
      ASSERT(bool,'arc test 7 (origin corner)')
      !Test 8
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,X=2.0_SRK,Y=1.0_SRK)
      CALL line1%p2%init(DIM=2,X=-2.0_SRK,Y=-3.0_SRK)
      CALL circle1%intersectArcLine(line1,point1,point2,point3,point4)
      bool=(point1%dim == 0 .AND. .NOT.ALLOCATED(point1%coord) .AND. &
            point2%dim == 0 .AND. .NOT.ALLOCATED(point2%coord) .AND. &
            point3%dim == -3 .AND. ALL(point3%coord .APPROXEQ. (/1.0_SRK,0.0_SRK/)) .AND. &
            point4%dim == -3 .AND. ALL(point4%coord .APPROXEQ. (/0.0_SRK,-1.0_SRK/)))
      ASSERT(bool,'arc test 8 (arc corner)')
      !Test 9
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,X=-2.0_SRK,Y=2.5_SRK)
      CALL line1%p2%init(DIM=2,X=2.0_SRK,Y=-1.5_SRK)
      circle1%thetastt=HALFPI
      circle1%thetastp=TWOPI
      CALL circle1%intersectArcLine(line1,point1,point2,point3,point4)
      bool=(point1%dim == 2 .AND. ALL(point1%coord .APPROXEQ. (/-0.411437827766148_SRK,0.911437827766148_SRK/)) .AND. &
            point2%dim == 2 .AND. ALL(point2%coord .APPROXEQ. (/0.911437827766148_SRK,-0.411437827766148_SRK/)) .AND. &
            point3%dim == 2 .AND. ALL(point3%coord .APPROXEQ. (/0.0_SRK,0.5_SRK/)) .AND. &
            point4%dim == 2 .AND. ALL(point4%coord .APPROXEQ. (/0.5_SRK,0.0_SRK/)))
      ASSERT(bool,'arc test 9 (pacman)')
      !Test 10
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,X=0.0_SRK,Y=-0.1_SRK)
      CALL line1%p2%init(DIM=2,X=0.0_SRK,Y=2.0_SRK)
      CALL circle1%intersectArcLine(line1,point1,point2,point3,point4)
      bool=(point1%dim == 0 .AND. .NOT.ALLOCATED(point1%coord) .AND. &
            point2%dim == 0 .AND. .NOT.ALLOCATED(point2%coord) .AND. &
            point3%dim == -3 .AND. .NOT.ALLOCATED(point3%coord) .AND. &
            point4%dim == -3 .AND. ALL(point4%coord .APPROXEQ. (/0.0_SRK,0.0_SRK/)))
      ASSERT(bool,'arc test 10 (y-axis)')
      !Test 11
      circle1%thetastt=ZERO
      circle1%thetastp=HALFPI
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,X=-2.0_SRK,Y=-2.0_SRK)
      CALL line1%p2%init(DIM=2,X=2.0_SRK,Y=2.0_SRK)
      CALL circle1%intersectArcLine(line1,point1,point2,point3,point4)
      bool=(point1%dim == 0 .AND. .NOT.ALLOCATED(point1%coord) .AND. &
            point2%dim == 2 .AND. ALL(point2%coord .APPROXEQ. (/SQRT(0.5_SRK),SQRT(0.5_SRK)/)) .AND. &
            point3%dim == 2 .AND. ALL(point3%coord .APPROXEQ. (/0.0_SRK,0.0_SRK/)) .AND. &
            point4%dim == -3 .AND. ALL(point4%coord .APPROXEQ. (/0.0_SRK,0.0_SRK/)))
      ASSERT(bool,'arc test 11 (through origin)')
      !Test 12
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,X=-2.0_SRK,Y=1.5_SRK)
      CALL line1%p2%init(DIM=2,X=2.0_SRK,Y=-0.5_SRK)
      CALL circle1%intersectArcLine(line1,point1,point2,point3,point4)
      bool=(point1%dim == 0 .AND. .NOT.ALLOCATED(point1%coord) .AND. &
            point2%dim == 0 .AND. .NOT.ALLOCATED(point2%coord) .AND. &
            point3%dim == 2 .AND. ALL(point3%coord .APPROXEQ. (/1.0_SRK,0.0_SRK/)) .AND. &
            point4%dim == 2 .AND. ALL(point4%coord .APPROXEQ. (/0.0_SRK,0.5_SRK/)))
      ASSERT(bool,'arc test 12 (corner side)')
      !Test 13
      CALL point%clear()
      CALL circle1%clear()
      CALL line1%clear()
      CALL point%init(DIM=2,X=1.0_SRK,Y=1.0_SRK)
      CALL circle1%set(point,1.0_SRK,ANGSTT=TWOPI-QTRPI,ANGSTP=QTRPI)
      CALL line1%p1%init(DIM=2,X=1.8_SRK,Y=2.5_SRK)
      CALL line1%p2%init(DIM=2,X=1.8_SRK,Y=0.0_SRK)
      CALL circle1%intersectArcLine(line1,point1,point2,point3,point4)
      bool=(point1%dim == 2 .AND. ALL(point1%coord .APPROXEQA. (/1.8_SRK,1.6_SRK/)) .AND. &
            point2%dim == 2 .AND. ALL(point2%coord .APPROXEQA. (/1.8_SRK,0.4_SRK/)) .AND. &
            point3%dim == -3 .AND. ALLOCATED(point3%coord) .AND. &
            point4%dim == -3 .AND. ALLOCATED(point4%coord))
      ASSERT(bool,'arc test 13 (arc crosses theta=0 (thetastt > thetastp))')

!Test hasPoint
      COMPONENT_TEST('Circle %inside()')
      !Full circle
      CALL point%clear()
      CALL point%init(COORD=(/1.5_SRK,1.5_SRK/))
      CALL circle2%set(point,0.4225_SRK)
      CALL point%clear()
      CALL point%init(COORD=(/1.6_SRK,1.6_SRK/))
      ASSERT(circle2%inside(point),'Circle internal point, Q1')
      CALL point%clear()
      CALL point%init(COORD=(/1.4_SRK,1.6_SRK/))
      ASSERT(circle2%inside(point),'Circle internal point, Q2')
      CALL point%clear()
      CALL point%init(COORD=(/1.4_SRK,1.4_SRK/))
      ASSERT(circle2%inside(point),'Circle internal point, Q3')
      CALL point%clear()
      CALL point%init(COORD=(/1.6_SRK,1.4_SRK/))
      ASSERT(circle2%inside(point),'Circle internal point, Q4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle2%inside(point),'Circle external point')

      !Test half circle
      CALL point%clear()
      CALL point%init(COORD=(/-1.5_SRK,-1.5_SRK/))
      CALL circle2%set(point,0.4225_SRK,ANGSTT=ZERO,ANGSTP=PI)
      ASSERT(circle2%inside(circle2%c),'Circle Arc Centroid')
      CALL point%clear()
      CALL point%init(COORD=(/-1.4_SRK,-1.4_SRK/))
      ASSERT(circle2%inside(point),'Circle Arc internal point, Q1')
      CALL point%clear()
      CALL point%init(COORD=(/-1.6_SRK,-1.4_SRK/))
      ASSERT(circle2%inside(point),'Circle Arc internal point, Q2')
      CALL point%clear()
      CALL point%init(COORD=(/-1.6_SRK,-1.6_SRK/))
      ASSERT(.NOT.circle2%inside(point),'Circle Arc external point, Q3')
      CALL point%clear()
      CALL point%init(COORD=(/-1.4_SRK,-1.6_SRK/))
      ASSERT(.NOT.circle2%inside(point),'Circle Arc external point, Q4')

      !Test 1st and 2nd quadrant arc
      CALL point%clear()
      CALL point%init(COORD=(/-1.5_SRK,1.5_SRK/))
      CALL circle2%set(point,0.4225_SRK,ANGSTT=QTRPI,ANGSTP=HALFPI+QTRPI)
      ASSERT(circle2%inside(circle2%c),'Circle Arc Centroid')
      CALL point%clear()
      CALL point%init(COORD=(/-1.35_SRK,1.6_SRK/))
      ASSERT(.NOT.circle2%inside(point),'Circle Arc external point Q1')
      CALL point%clear()
      CALL point%init(COORD=(/-1.4_SRK,1.6_SRK/))
      ASSERT(circle2%inside(point),'Circle Arc internal point Q1')
      CALL point%clear()
      CALL point%init(COORD=(/-1.6_SRK,1.6_SRK/))
      ASSERT(circle2%inside(point),'Circle Arc internal point Q2')
      CALL point%clear()
      CALL point%init(COORD=(/-1.7_SRK,1.6_SRK/))
      ASSERT(.NOT.circle2%inside(point),'Circle Arc external point Q2')
      CALL point%clear()
      CALL point%init(COORD=(/-1.6_SRK,1.4_SRK/))
      ASSERT(.NOT.circle2%inside(point),'Circle Arc external point Q3')
      CALL point%clear()
      CALL point%init(COORD=(/-1.4_SRK,1.4_SRK/))
      ASSERT(.NOT.circle2%inside(point),'Circle Arc external point Q4')

      !Test "reflex" arc
      CALL point%clear()
      CALL point%init(COORD=(/1.5_SRK,-1.5_SRK/))
      CALL circle2%set(point,0.4225_SRK,ANGSTT=ZERO,ANGSTP=PI+HALFPI+QTRPI)
      ASSERT(circle2%inside(circle2%c),'Circle Arc Centroid')
      CALL point%clear()
      CALL point%init(COORD=(/1.6_SRK,-1.4_SRK/))
      ASSERT(circle2%inside(point),'Circle Arc internal point Q1')
      CALL point%clear()
      CALL point%init(COORD=(/1.45_SRK,-1.4_SRK/))
      ASSERT(circle2%inside(point),'Circle Arc internal point Q2')
      CALL point%clear()
      CALL point%init(COORD=(/1.4_SRK,-1.6_SRK/))
      ASSERT(circle2%inside(point),'Circle Arc internal point, Q3')
      CALL point%clear()
      CALL point%init(COORD=(/1.6_SRK,-1.6_SRK/))
      ASSERT(circle2%inside(point),'Circle Arc internal point on bndy, Q4')
      CALL point%clear()
      CALL point%init(COORD=(/1.7_SRK,-1.6_SRK/))
      ASSERT(.NOT.circle2%inside(point),'Circle Arc external point Q4')

      !Test 3rd and 4th quadrant arc
      CALL point%clear()
      CALL point%init(COORD=(/1.5_SRK,1.5_SRK/))
      CALL circle2%set(point,0.4225_SRK,ANGSTT=PI+QTRPI,ANGSTP=PI+HALFPI+QTRPI)
      ASSERT(circle2%inside(circle2%c),'Circle Arc Centroid')
      CALL point%clear()
      CALL point%init(COORD=(/1.6_SRK,1.6_SRK/))
      ASSERT(.NOT.circle2%inside(point),'Circle Arc external point Q1')
      CALL point%clear()
      CALL point%init(COORD=(/1.4_SRK,1.6_SRK/))
      ASSERT(.NOT.circle2%inside(point),'Circle Arc external point Q2')
      CALL point%clear()
      CALL point%init(COORD=(/1.3_SRK,1.4_SRK/))
      ASSERT(.NOT.circle2%inside(point),'Circle Arc external point Q3')
      CALL point%clear()
      CALL point%init(COORD=(/1.4_SRK,1.4_SRK/))
      ASSERT(circle2%inside(point),'Circle Arc internal point Q3')
      CALL point%clear()
      CALL point%init(COORD=(/1.5_SRK,1.4_SRK/))
      ASSERT(circle2%inside(point),'Circle Arc internal point Q3-Q4')
      CALL point%clear()
      CALL point%init(COORD=(/1.6_SRK,1.4_SRK/))
      ASSERT(circle2%inside(point),'Circle Arc internal point Q4')
      CALL point%clear()
      CALL point%init(COORD=(/1.7_SRK,1.4_SRK/))
      ASSERT(.NOT.circle2%inside(point),'Circle Arc external point Q4')

      !
      !Test for equivalence operation (implicitly tests assignment operation)
      COMPONENT_TEST('OPERATOR(==)')
      circle2=circle1
      ASSERT(circle1 == circle2,'circle equivalence')
      circle2%r=0.2_SRK
      ASSERT(.NOT.(circle1 == circle2),'circle non-equivalence')
!
!Test clear
      COMPONENT_TEST('Cylinder %clear()')
      CALL point3%clear()
      CALL point2%clear()
      CALL point2%init(DIM=3,X=0.1_SRK,Y=0.2_SRK,Z=0.3_SRK)
      CALL point3%init(DIM=3,X=0.1_SRK,Y=0.2_SRK,Z=1.3_SRK)
      cylinder1%axis%p1=point2
      cylinder1%axis%p2=point2
      cylinder1%r=1.0_SRK

      CALL cylinder1%clear()
      bool = .NOT.(cylinder1%r /= 0.0_SRK .OR. cylinder1%axis%p1%dim /= 0 .OR. &
                   ALLOCATED(cylinder1%axis%p1%coord) .OR. cylinder1%axis%p2%dim /= 0 .OR. &
                   ALLOCATED(cylinder1%axis%p2%coord))
      ASSERT(bool, 'cylinder1%clear()')
!
!Test set
      COMPONENT_TEST('Cylinder %set()')
      !Error check
      CALL cylinder1%set(point,point3,1.0_SRK)
      bool = .NOT.(cylinder1%r /= 0.0_SRK .OR. cylinder1%axis%p1%dim /= 0 .OR. &
                   ALLOCATED(cylinder1%axis%p1%coord) .OR. cylinder1%axis%p2%dim /= 0 .OR. &
                   ALLOCATED(cylinder1%axis%p2%coord))
      ASSERT(bool, 'cylinder1%set(...)')
      CALL cylinder1%set(point3,point3,1.0_SRK)
      bool = .NOT.(cylinder1%r /= 0.0_SRK .OR. cylinder1%axis%p1%dim /= 0 .OR. &
                   ALLOCATED(cylinder1%axis%p1%coord) .OR. cylinder1%axis%p2%dim /= 0 .OR. &
                   ALLOCATED(cylinder1%axis%p2%coord))
      ASSERT(bool, 'cylinder1%set(...)')
      CALL cylinder1%set(point2,point3,-1.0_SRK)
      bool = .NOT.(cylinder1%r /= 0.0_SRK .OR. cylinder1%axis%p1%dim /= 0 .OR. &
                   ALLOCATED(cylinder1%axis%p1%coord) .OR. cylinder1%axis%p2%dim /= 0 .OR. &
                   ALLOCATED(cylinder1%axis%p2%coord))
      ASSERT(bool, 'cylinder1%set(...)')
      !Real test
      CALL cylinder1%set(point2,point3,1.0_SRK)
      bool = .NOT.(cylinder1%r /= 1.0_SRK .OR. cylinder1%axis%p1%dim /= 3 .OR. &
                   ANY(cylinder1%axis%p1%coord /= (/0.1_SRK,0.2_SRK,0.3_SRK/)) .OR. &
                   cylinder1%axis%p2%dim /= 3 .OR. &
                   ANY(cylinder1%axis%p2%coord /= (/0.1_SRK,0.2_SRK,1.3_SRK/)))
      ASSERT(bool, 'cylinder1%set(...)')
      CALL cylinder1%clear()

!
!Test intersection
      COMPONENT_TEST('Cylinder %intersectLine()')
      !Test bad input
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(point2%dim /= -1 .OR. point3%dim /= -1)
      ASSERT(bool, 'cylinder1%intersectLine(...) (bad input)')

      !Reference cylinder for all cases.
      cylinder1%r=1.0_SRK
      CALL cylinder1%axis%p1%init(DIM=3,X=0.1_SRK,Y=-0.2_SRK,Z=0.0_SRK)
      CALL cylinder1%axis%p2%init(DIM=3,X=0.1_SRK,Y=-0.2_SRK,Z=1.0_SRK)
      cylinder1%thetastp=TWOPI
      CALL line1%clear()

      !Test totally "outside" P-surface
      CALL line1%p1%init(DIM=3,X=2.0_SRK,Y=2.0_SRK,Z=-0.5_SRK)
      CALL line1%p2%init(DIM=3,X=2.0_SRK,Y=2.0_SRK,Z=-1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(point2%dim /= -2 .OR. point3%dim /= -2)
      ASSERT(bool, 'cylinder1%intersectLine(...) (outside P)')
      CALL line1%clear()

      !Test totally "outside" Q-surface
      CALL line1%p1%init(DIM=3,X=2.0_SRK,Y=2.0_SRK,Z=10.5_SRK)
      CALL line1%p2%init(DIM=3,X=2.0_SRK,Y=2.0_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(point2%dim /= -3 .OR. point3%dim /= -3)
      ASSERT(bool, 'cylinder1%intersectLine(...) (outside Q)')
      CALL line1%clear()

      !Test parallel on cylinder surface
      CALL line1%p1%init(DIM=3,X=0.1_SRK,Y=-1.2_SRK,Z=-0.5_SRK)
      CALL line1%p2%init(DIM=3,X=0.1_SRK,Y=-1.2_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(point2%dim /= -4 .OR. point3%dim /= -4)
      ASSERT(bool, 'cylinder1%intersectLine(...) (parallel on surface)')
      CALL line1%clear()

      !Test parallel outside radius
      CALL line1%p1%init(DIM=3,X=2.0_SRK,Y=2.0_SRK,Z=-0.5_SRK)
      CALL line1%p2%init(DIM=3,X=2.0_SRK,Y=2.0_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(point2%dim /= -5 .OR. point3%dim /= -5)
      ASSERT(bool, 'cylinder1%intersectLine(...) (parallel outside)')
      CALL line1%clear()

      !Test parallel totally inside
      CALL line1%p1%init(DIM=3,X=0.0_SRK,Y=0.2_SRK,Z=0.2_SRK)
      CALL line1%p2%init(DIM=3,X=0.0_SRK,Y=0.2_SRK,Z=0.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(point2%dim /= -6 .OR. point3%dim /= -6)
      ASSERT(bool, 'cylinder1%intersectLine(...) (parallel inside)')
      CALL line1%clear()

      !Test parallel both intersections
      CALL line1%p1%init(DIM=3,X=0.0_SRK,Y=0.0_SRK,Z=-0.5_SRK)
      CALL line1%p2%init(DIM=3,X=0.0_SRK,Y=0.0_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(ANY(.NOT.(point2%coord .APPROXEQ. (/0.0_SRK,0.0_SRK,0.0_SRK/))) .OR. &
                   ANY(.NOT.(point3%coord .APPROXEQ. (/0.0_SRK,0.0_SRK,1.0_SRK/))))
      ASSERT(bool, 'cylinder1%intersectLine(...) (parallel inside)')
      CALL line1%clear()

      !Test not parallel and tangent
      CALL line1%p1%init(DIM=3,X=-1.1_SRK,Y=-1.2_SRK,Z=0.5_SRK)
      CALL line1%p2%init(DIM=3,X=1.1_SRK,Y=-1.2_SRK,Z=0.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(point2%dim /= -7 .OR. point3%dim /= -7)
      ASSERT(bool, 'cylinder1%intersectLine(...) (disjoint)')
      CALL line1%clear()

      !Test not parallel and disjoint
      CALL line1%p1%init(DIM=3,X=0.1_SRK,Y=2.0_SRK,Z=0.5_SRK)
      CALL line1%p2%init(DIM=3,X=1.1_SRK,Y=2.0_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(point2%dim /= -8 .OR. point3%dim /= -8)
      ASSERT(bool, 'cylinder1%intersectLine(...) (disjoint)')
      CALL line1%clear()

      !Test not parallel outside no intersection
      CALL line1%p1%init(DIM=3,X=0.1_SRK,Y=2.0_SRK,Z=-0.5_SRK)
      CALL line1%p2%init(DIM=3,X=0.1_SRK,Y=2.5_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(point2%dim /= 0 .OR. point3%dim /= 0)
      ASSERT(bool, 'cylinder1%intersectLine(...) (no intersection)')
      CALL line1%clear()

      !Test not parallel intersection with P-surface only
      CALL line1%p1%init(DIM=3,X=0.0_SRK,Y=0.0_SRK,Z=-0.5_SRK)
      CALL line1%p2%init(DIM=3,X=0.0_SRK,Y=0.5_SRK,Z=0.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(ANY(.NOT.(point2%coord .APPROXEQ. (/0.0_SRK,0.25_SRK,0.0_SRK/))) .OR. &
                   point3%dim /= 0)
      ASSERT(bool, 'cylinder1%intersectLine(...) (P-surface)')
      CALL line1%clear()

      !Test not parallel intersection with Q-surface only
      CALL line1%p1%init(DIM=3,X=0.0_SRK,Y=0.0_SRK,Z=0.5_SRK)
      CALL line1%p2%init(DIM=3,X=0.0_SRK,Y=0.5_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(ANY(.NOT.(point3%coord .APPROXEQ. (/0.0_SRK,0.25_SRK,1.0_SRK/))) .OR. &
                   point2%dim /= 0)
      ASSERT(bool, 'cylinder1%intersectLine(...) (Q-surface)')
      CALL line1%clear()

      !Test not parallel intersection with both PQ-surfaces
      CALL line1%p1%init(DIM=3,X=0.0_SRK,Y=0.5_SRK,Z=1.5_SRK)
      CALL line1%p2%init(DIM=3,X=0.0_SRK,Y=0.0_SRK,Z=-0.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(ANY(.NOT.(point3%coord .APPROXEQ. (/0.0_SRK,0.125_SRK,0.0_SRK/))) .OR. &
                   ANY(.NOT.(point2%coord .APPROXEQ. (/0.0_SRK,0.375_SRK,1.0_SRK/))))
      ASSERT(bool, 'cylinder1%intersectLine(...) (QP-surface)')
      CALL line1%clear()

      !Test not parallel intersection with cylinder once
      CALL line1%p1%init(DIM=3,X=0.0_SRK,Y=0.0_SRK,Z=0.5_SRK)
      CALL line1%p2%init(DIM=3,X=0.0_SRK,Y=1.5_SRK,Z=0.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(ANY(.NOT.(point3%coord .APPROXEQ. (/0.0_SRK,0.794987437106620_SRK,0.5_SRK/))) .OR. &
                   point2%dim /= 0)
      ASSERT(bool, 'cylinder1%intersectLine(...)')
      CALL line1%clear()

      !Test not parallel intersection with cylinder twice
      CALL line1%p1%init(DIM=3,X=0.0_SRK,Y=-1.5_SRK,Z=0.5_SRK)
      CALL line1%p2%init(DIM=3,X=0.0_SRK,Y=1.5_SRK,Z=0.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      bool = .NOT.(ANY(.NOT.(point3%coord .APPROXEQ. (/0.0_SRK,0.794987437106620_SRK,0.5_SRK/))) .OR. &
                   ANY(.NOT.(point2%coord .APPROXEQ. (/0.0_SRK,-1.194987437106620_SRK,0.5_SRK/))))
      ASSERT(bool, 'cylinder1%intersectLine(...) (cyl-2)')


      !Test missed intersection for slice of cylinder
      !cylinder1%thetastt=HALFPI
      !cylinder1%thetastp=PI
      !CALL line1%p1%init(DIM=3,X=-2.0_SRK,Y=-1.0_SRK,Z=0.44_SRK)
      !CALL line1%p2%init(DIM=3,X=2.0_SRK,Y=0.0_SRK,Z=0.55_SRK)
      !CALL cylinder1%intersectLine(line1,point2,point3)
      !bool = (point2%dim == 0) .AND. (point3%dim == 0)
      !ASSERT(bool,'cylinder1%intersectLine(...) (arc miss)')
      !FINFO() point2%dim, point2%coord
      !FINFO() point3%dim, point3%coord

      !Test for equivalence operation (implicitly tests assignment operation)
      COMPONENT_TEST('OPERATOR(==)')
      cylinder2=cylinder1
      ASSERT(cylinder1 == cylinder2,'cylinder equivalence')
      cylinder2%r=0.5_SRK
      ASSERT(.NOT.(cylinder1 == cylinder2),'cylinder non-equivalence')

#ifdef __GFORTRAN__
      WRITE(*,*) 'ELEMENTAL METHODS FOR NON-SCALAR BASE OBJECTS NOT YET SUPPORTED BY COMPILER'
#else
      WRITE(*,*) 'TESTING CIRCLETYPE (arrays)'
!
!Test clear
      COMPONENT_TEST('Elemental Circle %clear()')
      CALL point%clear()
      CALL point2%clear()
      CALL point%init(DIM=2,X=0.1_SRK,Y=0.2_SRK)
      circles(1)%r=0.5_SRK
      circles(1)%c=point
      circles(2)%r=0.5_SRK
      circles(2)%c=point
      CALL circles%clear()
      bool = .NOT.(circles(1)%r /= 0.0_SRK .OR. circles(1)%c%dim /= 0 .OR. &
                   ALLOCATED(circles(1)%c%coord) .OR. circles(2)%r /= 0.0_SRK .OR. &
                   circles(2)%c%dim /= 0 .OR. ALLOCATED(circles(2)%c%coord))
      ASSERT(bool, 'circles%clear()')
!
!Test set
      COMPONENT_TEST('Elemental Circle %set()')
      CALL point%init(DIM=2,X=0.1_SRK,Y=0.2_SRK)
!This no longer works with Intel 12
      CALL circles%set(point,(/0.5_SRK,0.6_SRK/))
!      CALL circles(1)%set(point,0.5_SRK)
!      CALL circles(2)%set(point,0.6_SRK)
      bool = .NOT.(circles(1)%r /= 0.5_SRK .OR. circles(2)%r /= 0.6_SRK .OR. &
                   ANY(circles(1)%c%coord /= (/0.1_SRK,0.2_SRK/)) .OR. &
                   ANY(circles(2)%c%coord /= (/0.1_SRK,0.2_SRK/)) .OR. &
                   circles(1)%c%dim /= 2 .OR. circles(2)%c%dim /= 2)
      ASSERT(bool, 'circles%set(...)')
!
!Test intersection
      COMPONENT_TEST('Elemental Circle %intersectLine()')
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,X=-1.0_SRK,Y=-1.0_SRK)
      CALL line1%p2%init(DIM=2,X=1.0_SRK,Y=1.0_SRK)

      CALL circles%intersectLine(line1,points,points2)
      bool = .NOT.(ANY(.NOT.(points(1)%coord .APPROXEQ. -0.2_SRK)) .OR. &
                   ANY(.NOT.(points2(1)%coord .APPROXEQ. 0.5_SRK)) .OR. &
                   ANY(.NOT.(points(2)%coord .APPROXEQ. -0.271307488658817_SRK)) .OR. &
                   ANY(.NOT.(points2(2)%coord .APPROXEQ. 0.571307488658817_SRK)))
      ASSERT(bool, 'circles%intersectLine(...)')
!
!Test Clear
      COMPONENT_TEST('Elemental Cylinder %clear()')
      CALL point3%clear()
      CALL point2%clear()
      CALL point2%init(DIM=3,X=0.1_SRK,Y=-0.2_SRK,Z=0.0_SRK)
      CALL point3%init(DIM=3,X=0.1_SRK,Y=-0.2_SRK,Z=1.0_SRK)
      cylinders(1)%axis%p1=point2
      cylinders(1)%axis%p2=point2
      cylinders(1)%r=1.0_SRK
      cylinders(2)=cylinders(1)

      CALL cylinders%clear()
      bool = .NOT.(cylinders(1)%r /= 0.0_SRK .OR. cylinders(1)%axis%p1%dim /= 0 .OR. &
                   ALLOCATED(cylinders(1)%axis%p1%coord) .OR. cylinders(1)%axis%p2%dim /= 0 .OR. &
                   ALLOCATED(cylinders(1)%axis%p2%coord) .OR. cylinders(2)%r /= 0.0_SRK .OR. &
                   cylinders(2)%axis%p1%dim /= 0 .OR. ALLOCATED(cylinders(2)%axis%p1%coord) .OR. &
                   cylinders(2)%axis%p2%dim /= 0 .OR. ALLOCATED(cylinders(2)%axis%p2%coord))
      ASSERT(bool, 'cylinders%clear()')
!
!Test set
      COMPONENT_TEST('Elemental Cylinder %set()')
!This no longer works with Intel 12
      CALL cylinders%set(point2,point3,(/1.0_SRK,1.5_SRK/))
!      CALL cylinders(1)%set(point2,point3,1.0_SRK)
!      CALL cylinders(2)%set(point2,point3,1.5_SRK)
      bool = .NOT.(cylinders(1)%r /= 1.0_SRK .OR. cylinders(1)%axis%p1%dim /= 3 .OR. &
                   ANY(cylinders(1)%axis%p1%coord /= (/0.1_SRK,-0.2_SRK,0.0_SRK/)) .OR. &
                   cylinders(1)%axis%p2%dim /= 3 .OR. &
                   ANY(cylinders(1)%axis%p2%coord /= (/0.1_SRK,-0.2_SRK,1.0_SRK/)) .OR. &
                   cylinders(2)%r /= 1.5_SRK .OR. cylinders(2)%axis%p1%dim /= 3 .OR. &
                   ANY(cylinders(2)%axis%p1%coord /= (/0.1_SRK,-0.2_SRK,0.0_SRK/)) .OR. &
                   cylinders(2)%axis%p2%dim /= 3 .OR. &
                   ANY(cylinders(2)%axis%p2%coord /= (/0.1_SRK,-0.2_SRK,1.0_SRK/)))
      ASSERT(bool, 'cylinders%set(...)')
!
!Test intersectLine
      COMPONENT_TEST('Elemental Cylinder %intersectLine()')
      CALL line1%clear()
      CALL line1%p1%init(DIM=3,X=0.0_SRK,Y=-2.5_SRK,Z=0.5_SRK)
      CALL line1%p2%init(DIM=3,X=0.0_SRK,Y=2.5_SRK,Z=0.5_SRK)
      CALL cylinders%intersectLine(line1,points2,points3)
      bool = .NOT.(ANY(.NOT.(points3(1)%coord .APPROXEQ. (/0.0_SRK,0.794987437106620_SRK,0.5_SRK/))) .OR. &
                   ANY(.NOT.(points2(1)%coord .APPROXEQ. (/0.0_SRK,-1.194987437106620_SRK,0.5_SRK/))) .OR. &
                   ANY(.NOT.(points3(2)%coord .APPROXEQ. (/0.0_SRK,1.296662954709577_SRK,0.5_SRK/))) .OR. &
                   ANY(.NOT.(points2(2)%coord .APPROXEQ. (/0.0_SRK,-1.696662954709577_SRK,0.5_SRK/))))
      ASSERT(bool, 'cylinders%intersectLine(...)')
#endif

    ENDSUBROUTINE TestCircle_and_Cylinder
!
!-------------------------------------------------------------------------------
    SUBROUTINE testOnSurface()
      TYPE(CircleType) :: circle

      COMPONENT_TEST('Undefined circle')
      CALL circle%clear()
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'origin')

      COMPONENT_TEST('Full Circle')
      CALL circle%set(point,1.0_SRK)
      ASSERT(.NOT.circle%onSurface(point),'inside')
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,2.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'outside')
      CALL point%clear()
      CALL point%init(COORD=(/1.0_SRK,0.0_SRK/))
      ASSERT(circle%onSurface(point),'0')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,1.0_SRK/))
      ASSERT(circle%onSurface(point),'PI/2')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'3*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/-1.0_SRK,0.0_SRK/))
      ASSERT(circle%onSurface(point),'PI')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'5*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,-1.0_SRK/))
      ASSERT(circle%onSurface(point),'6*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'7*PI/4')

      COMPONENT_TEST('Half Circle [0,PI]')
      circle%thetastt=0.0_SRK
      circle%thetastp=PI
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'inside')
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,2.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'outside')
      CALL point%clear()
      CALL point%init(COORD=(/1.0_SRK,0.0_SRK/))
      ASSERT(circle%onSurface(point),'0')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,1.0_SRK/))
      ASSERT(circle%onSurface(point),'PI/2')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'3*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/-1.0_SRK,0.0_SRK/))
      ASSERT(circle%onSurface(point),'PI')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(.NOT.circle%onSurface(point),'5*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,-1.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'6*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(.NOT.circle%onSurface(point),'7*PI/4')

      COMPONENT_TEST('Half Circle [PI,0]')
      circle%thetastt=PI
      circle%thetastp=0.0_SRK
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'inside')
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,2.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'outside')
      CALL point%clear()
      CALL point%init(COORD=(/1.0_SRK,0.0_SRK/))
      ASSERT(circle%onSurface(point),'0')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(.NOT.circle%onSurface(point),'PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,1.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'PI/2')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(.NOT.circle%onSurface(point),'3*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/-1.0_SRK,0.0_SRK/))
      ASSERT(circle%onSurface(point),'PI')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'5*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,-1.0_SRK/))
      ASSERT(circle%onSurface(point),'6*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'7*PI/4')

      COMPONENT_TEST('Half Circle [PI/2,3PI/2]')
      circle%thetastt=PI*0.5_SRK
      circle%thetastp=PI*1.5_SRK
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'inside')
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,2.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'outside')
      CALL point%clear()
      CALL point%init(COORD=(/1.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'0')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(.NOT.circle%onSurface(point),'PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,1.0_SRK/))
      ASSERT(circle%onSurface(point),'PI/2')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'3*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/-1.0_SRK,0.0_SRK/))
      ASSERT(circle%onSurface(point),'PI')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'5*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,-1.0_SRK/))
      ASSERT(circle%onSurface(point),'6*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(.NOT.circle%onSurface(point),'7*PI/4')

      COMPONENT_TEST('Half Circle [3PI/2,PI/2]')
      circle%thetastt=PI*1.5_SRK
      circle%thetastp=PI*0.5_SRK
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'inside')
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,2.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'outside')
      CALL point%clear()
      CALL point%init(COORD=(/1.0_SRK,0.0_SRK/))
      ASSERT(circle%onSurface(point),'0')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,1.0_SRK/))
      ASSERT(circle%onSurface(point),'PI/2')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(.NOT.circle%onSurface(point),'3*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/-1.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'PI')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(.NOT.circle%onSurface(point),'5*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,-1.0_SRK/))
      ASSERT(circle%onSurface(point),'6*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'7*PI/4')

      COMPONENT_TEST('Half Circle [PI/4,5PI/4]')
      circle%thetastt=PI*0.25_SRK
      circle%thetastp=PI*1.25_SRK
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'inside')
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,2.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'outside')
      CALL point%clear()
      CALL point%init(COORD=(/1.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'0')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,1.0_SRK/))
      ASSERT(circle%onSurface(point),'PI/2')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'3*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/-1.0_SRK,0.0_SRK/))
      ASSERT(circle%onSurface(point),'PI')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'5*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,-1.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'6*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(.NOT.circle%onSurface(point),'7*PI/4')

      COMPONENT_TEST('Half Circle [5PI/4,PI/4]')
      circle%thetastt=PI*1.25_SRK
      circle%thetastp=PI*0.25_SRK
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'inside')
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,2.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'outside')
      CALL point%clear()
      CALL point%init(COORD=(/1.0_SRK,0.0_SRK/))
      ASSERT(circle%onSurface(point),'0')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,1.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'PI/2')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(.NOT.circle%onSurface(point),'3*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/-1.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'PI')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'5*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,-1.0_SRK/))
      ASSERT(circle%onSurface(point),'6*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'7*PI/4')

      COMPONENT_TEST('Half Circle [3PI/4,7PI/4]')
      circle%thetastt=PI*0.75_SRK
      circle%thetastp=PI*1.75_SRK
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'inside')
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,2.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'outside')
      CALL point%clear()
      CALL point%init(COORD=(/1.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'0')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(.NOT.circle%onSurface(point),'PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,1.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'PI/2')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'3*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/-1.0_SRK,0.0_SRK/))
      ASSERT(circle%onSurface(point),'PI')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'5*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,-1.0_SRK/))
      ASSERT(circle%onSurface(point),'6*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'7*PI/4')

      COMPONENT_TEST('Half Circle [7PI/4,3PI/4]')
      circle%thetastt=PI*1.75_SRK
      circle%thetastp=PI*0.75_SRK
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'inside')
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,2.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'outside')
      CALL point%clear()
      CALL point%init(COORD=(/1.0_SRK,0.0_SRK/))
      ASSERT(circle%onSurface(point),'0')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,1.0_SRK/))
      ASSERT(circle%onSurface(point),'PI/2')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'3*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/-1.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'PI')
      CALL point%clear()
      CALL point%init(COORD=(/-SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(.NOT.circle%onSurface(point),'5*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,-1.0_SRK/))
      ASSERT(.NOT.circle%onSurface(point),'6*PI/4')
      CALL point%clear()
      CALL point%init(COORD=(/SQRT(0.5_SRK),-SQRT(0.5_SRK)/))
      ASSERT(circle%onSurface(point),'7*PI/4')

      CALL circle%clear()
      CALL point%clear()
    ENDSUBROUTINE testOnSurface
!
ENDPROGRAM testGeom_CircCyl
