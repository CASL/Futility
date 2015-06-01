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
PROGRAM testGeom
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV  
  USE UnitTest
  USE IntrType
  USE Constants_Conversion
  USE ParameterLists
  USE Geom
  
  IMPLICIT NONE
  
  TYPE(PointType) :: point,point2,point3,points(2),points2(2),points3(2)
  TYPE(LinkedListPointType),POINTER :: firstPoint,thisPoint
  TYPE(LineType) :: line1,line2,lines(2),dis,diss(2)
  TYPE(PlaneType) :: plane1,plane2,planes(2)
  TYPE(CircleType) :: circle1,circle2,circles(2)
  TYPE(CylinderType) :: cylinder1,cylinder2,cylinders(2)
  TYPE(OBBoxType) :: box,box2,boxs(2)
  INTEGER(SIK) :: ldim(2),i,ioerr
  REAL(SRK) :: d,s(2),mu1,mu2,mu1s(2),mu2s(2)
  REAL(SRK) :: e_2d(2),e_3d(3),u1_2d(2),u2_2d(2),u3_2d(2)
  REAL(SRK) :: u1_3d(3),u2_3d(3),u3_3d(3)
  CHARACTER(LEN=MAX_COORD_STR_LEN) :: tempstr
  LOGICAL(SBK) :: bool
  
  CREATE_TEST('Test Geom')
  CALL eParams%setQuietMode(.TRUE.)
  CALL eParams%setStopOnError(.FALSE.)
  
  REGISTER_SUBTEST('Test Points',TestPoints)
  REGISTER_SUBTEST('Test Lines',TestLine)
  REGISTER_SUBTEST('Test Plane',TestPlane)
  REGISTER_SUBTEST('Test Circle and Cylinder',TestCircle_and_Cylinder)
  REGISTER_SUBTEST('Test Box',TestBox)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestPoints
      
      !Initialize by hand
      COMPONENT_TEST('%clear()')
      !or point=PointType(2,(/0.5_SRK,0.3_SRK/))
      point%dim=1
      ALLOCATE(point%coord(1))
      point%coord(1)=0.5_SRK
!      
!Test clear routine
      CALL point%clear()
      bool = .NOT.(point%dim /= 0 .OR. ALLOCATED(point%coord))
      ASSERT(bool, 'point%clear()')
      WRITE(*,*) '  Passed: CALL point%clear()'
      CALL point%clear() !Test redundant call doesn't have an error
!      
!Test initialization
      COMPONENT_TEST('%init()')
      CALL point%init(DIM=1,X=0.5_SRK) !test 1-D
      bool = .NOT.(point%dim /= 1 .OR. point%coord(1) /= 0.5_SRK)
      ASSERT(bool, 'point%init(DIM=1,X=0.5)')
      
      !Test redundant call
      CALL point%init(DIM=2,X=0.6_SRK,Y=0.7_SRK)
      bool = .NOT.(point%dim /= 1 .OR. point%coord(1) /= 0.5_SRK)
      ASSERT(bool, 'point%init(DIM=1,X=0.5)')
      CALL point%clear()
      
      CALL point%init(DIM=2,X=0.2_SRK,Y=0.3_SRK) !test 2-D
      bool = .NOT.(point%dim /= 2 .OR. point%coord(1) /= 0.2_SRK .OR. &
              point%coord(2) /= 0.3_SRK)
      ASSERT(bool, 'point%init(DIM=2,X=0.2_SRK,Y=0.3_SRK)')
      CALL point%clear()
      
      CALL point%init(DIM=3,X=0.7_SRK,Y=0.8_SRK,Z=0.9_SRK) !test 3-D
      bool = .NOT.(point%dim /= 3 .OR. point%coord(1) /= 0.7_SRK .OR. &
                   point%coord(2) /= 0.8_SRK .OR. point%coord(3) /= 0.9_SRK)
      ASSERT(bool, 'point%init(DIM=3,X=0.7_SRK,Y=0.8_SRK,Z=0.9_SRK)')
      CALL point%clear()
      
      CALL point%init(COORD=(/1.7_SRK,1.8_SRK,1.9_SRK,1.0_SRK/)) !test N-D
      bool = .NOT.(point%dim /= 4 .OR. point%coord(1) /= 1.7_SRK .OR. &
                   point%coord(2) /= 1.8_SRK .OR. point%coord(3) /= 1.9_SRK .OR. &
                   point%coord(4) /= 1.0_SRK)
      ASSERT(bool, 'point%init(COORD=(/.../))')
!      
!Test getCoordString
      COMPONENT_TEST('%getCoordString()')
      bool = .NOT.(TRIM(point%getCoordString()) /= '( 1.70000000000000E+00, '// &
                        '1.80000000000000E+00, 1.90000000000000E+00, '// &
                        '1.00000000000000E+00)' .OR. TRIM(point%getCoordString( &
                        'f6.3')) /= '( 1.700, 1.800, 1.900, 1.000)')
      ASSERT(bool, 'point%getCoordString(...))')
#ifdef DEBUG
      bool = .NOT.(LEN_TRIM(point%getCoordString('i6')) /= 0 .OR. &
                   LEN_TRIM(point2%getCoordString()) /= 0)
#else
      bool = .NOT.(LEN_TRIM(point2%getCoordString()) /= 0)
#endif
      ASSERT(bool, 'point%getCoordString(...))')
!
!Test Distance
      COMPONENT_TEST('Distance()')
      CALL point2%init(DIM=1,X=0.5_SRK)
      CALL point3%init(DIM=1,X=0.1_SRK)
      d=Distance(point2,point3)
      ASSERT(d == 0.4_SRK, '1-D Distance(...)') !Check 1-D
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL point3%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      d=Distance(point2,point3)
      bool = d .APPROXEQ. 0.447213595499958_SRK
      ASSERT(bool, '2-D Distance(...)')
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL point3%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      d=Distance(point2,point3)
      bool = d .APPROXEQ. 0.748331477354788_SRK
      ASSERT(bool, '3-D Distance(...)')
      
      !Redundant call to test error check
      ASSERT(Distance(point2,point) == 0.0_SRK, 'Distance(point2,point)')
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      CALL point3%init(DIM=4,COORD=(/0.3_SRK,0.2_SRK,0.1_SRK,0.0_SRK/))
      d=Distance(point2,point3)
      bool = d .APPROXEQ. 1.09544511501033_SRK
      ASSERT(bool, 'N-D Distance(...)')
      CALL point2%clear()
      CALL point3%clear()
      d=Distance(point2,point3) !Test for empty points
!
!Test midpoint
      COMPONENT_TEST('midPoint()')
      CALL point2%init(DIM=1,X=0.5_SRK)
      CALL point3%init(DIM=1,X=0.1_SRK)
      point=midPoint(point2,point3)
      bool = point%coord(1) .APPROXEQ. 0.3_SRK
      ASSERT(bool, '1-D midPoint(...)')
      CALL point2%clear()
      CALL point3%clear()
      
      !Test 2-D
      CALL point2%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL point3%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      point=midPoint(point2,point3)
      bool = .NOT.(.NOT.(point%coord(1) .APPROXEQ. 0.4_SRK) .OR. &
                   .NOT.(point%coord(2) .APPROXEQ. 0.4_SRK))
      ASSERT(bool, '2-D midPoint(...)')
      CALL point2%clear()
      CALL point3%clear()
      
      !Test 3-D
      CALL point2%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL point3%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      point=midPoint(point2,point3)
      bool = .NOT.(.NOT.(point%coord(1) .APPROXEQ. 0.4_SRK) .OR. &
                   .NOT.(point%coord(2) .APPROXEQ. 0.4_SRK) .OR. &
                   .NOT.(point%coord(3) .APPROXEQ. 0.4_SRK))
      ASSERT(bool, '3-D midPoint(...)')
      CALL point2%clear()
      CALL point3%clear()
      
      !Test N-D
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      CALL point3%init(DIM=4,COORD=(/0.3_SRK,0.2_SRK,0.1_SRK,0.0_SRK/))
      point=midPoint(point2,point3) !Test for incorrect dimensions
      CALL point3%init(DIM=4,COORD=(/0.3_SRK,0.2_SRK,0.1_SRK,0.0_SRK/))
      point=midPoint(point2,point3)
      bool = .NOT.(.NOT.(point%coord(1) .APPROXEQ. 0.4_SRK) .OR. &
                   .NOT.(point%coord(2) .APPROXEQ. 0.4_SRK) .OR. &
                   .NOT.(point%coord(3) .APPROXEQ. 0.4_SRK) .OR. &
                   .NOT.(point%coord(4) .APPROXEQ. 0.4_SRK))
      ASSERT(bool, 'N-D midPoint(...)')
      CALL point2%clear()
      CALL point3%clear()
      point=midPoint(point2,point3) !Test for empty points
!
!Test Operators
      COMPONENT_TEST('OPERATOR(+)')
      CALL point2%init(DIM=1,X=0.5_SRK)
      CALL point3%init(DIM=1,X=0.1_SRK)
      point=point2+point3
      bool = .NOT.(point%dim /= 1 .OR. point%coord(1) /= 0.6_SRK)
      ASSERT(bool, '1-D PointType')
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL point3%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      point=point2+point3
      bool = .NOT.(point%dim /= 2 .OR. ANY(point%coord /= 0.8_SRK))
      ASSERT(bool, '2-D PointType OPERATOR(+)')
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL point3%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      point=point2+point3
      bool = .NOT.(point%dim /= 3 .OR. ANY(.NOT.(point%coord .APPROXEQ. 0.8_SRK)))
      ASSERT(bool, '3-D PointType OPERATOR(+)')
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      CALL point3%init(DIM=4,COORD=(/0.3_SRK,0.2_SRK,0.1_SRK,0.0_SRK/))
      point=point2+point3
      bool = .NOT.(point%dim /= 4 .OR. ANY(.NOT.(point%coord .APPROXEQ. 0.8_SRK)))
      ASSERT(bool, 'N-D PointType OPERATOR(+)')
      CALL point2%clear()
      CALL point3%clear()
      
      !Redundant calls for error checking
      point=point2+point3 !Empty case
      bool = .NOT.(point%dim /= 0 .OR. ALLOCATED(point%coord))
      ASSERT(bool, 'Empty PointType OPERATOR(+)')
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      point2=point2+point3 !mismatched dimensions case
      bool = .NOT.(point2%dim /= 0 .OR. ALLOCATED(point2%coord))
      ASSERT(bool, 'Mismatched PointType OPERATOR(+)')
      
      !Test subtraction
      COMPONENT_TEST('OPERATOR(-)')
      CALL point2%init(DIM=1,X=0.5_SRK)
      CALL point3%init(DIM=1,X=0.1_SRK)
      point=point2-point3
      bool = .NOT.(point%dim /= 1 .OR. point%coord(1) /= 0.4_SRK)
      ASSERT(bool, '1-D PointType OPERATOR(-)')
!      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL point3%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      point=point2-point3
      bool = .NOT.(point%dim /= 2 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.2_SRK) .OR. &
                   .NOT.(point%coord(2) .APPROXEQ. 0.4_SRK))
      ASSERT(bool, '2-D PointType OPERATOR(-)')
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL point3%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      point=point2-point3
      bool = .NOT.(point%dim /= 3 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.2_SRK) .OR. &
                   .NOT.(point%coord(2) .APPROXEQ. 0.4_SRK) .OR. &
                   .NOT.(point%coord(3) .APPROXEQ. 0.6_SRK))
      ASSERT(bool, '3-D PointType OPERATOR(-)')
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      CALL point3%init(DIM=4,COORD=(/0.3_SRK,0.2_SRK,0.1_SRK,0.0_SRK/))
      point=point2-point3
      bool = .NOT.(point%dim /= 4 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.2_SRK) .OR. &
                   .NOT.(point%coord(2) .APPROXEQ. 0.4_SRK) .OR. &
                   .NOT.(point%coord(3) .APPROXEQ. 0.6_SRK) .OR. &
                   .NOT.(point%coord(4) .APPROXEQ. 0.8_SRK))
      ASSERT(bool, 'N-D PointType OPERATOR(-)')
      CALL point2%clear()
      CALL point3%clear()
      
      !Redundant calls for error checking
      point=point2-point3 !Empty case
      bool = .NOT.(point%dim /= 0 .OR. ALLOCATED(point%coord))
      ASSERT(bool, 'Empty PointType OPERATOR(-)')
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      point2=point2-point3 !mismatched dimensions case
      bool = .NOT.(point2%dim /= 0 .OR. ALLOCATED(point2%coord))
      ASSERT(bool, 'Mismatched PointType OPERATOR(-)')
      
      COMPONENT_TEST('OPERATOR(==)')
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      CALL point3%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.80000000000002_SRK/))
      point=point2
      bool = .NOT.(.NOT.(point == point2) .OR. point == point3)
      ASSERT(bool, 'PointType OPERATOR(==)')
      
      COMPONENT_TEST('OPERATOR(/=)')
      bool = .NOT.((point /= point2) .OR. .NOT.(point /= point3))
      ASSERT(bool, 'PointType OPERATOR(/=)')
      
      COMPONENT_TEST('OPERATOR(.APPROXEQ.)')
      point2%coord(4)=0.80000000000001_SRK
      bool = .NOT.(.NOT.(point .APPROXEQA. point2) .OR. (point .APPROXEQA. point3))
      ASSERT(bool, 'PointType OPERATOR(.APPROXEQ.)')
      CALL point2%clear()
      CALL point3%clear()
      
#ifdef __GFORTRAN__
      WRITE(*,*) 'ELEMENTAL METHODS FOR NON-SCALAR BASE OBJECTS NOT YET SUPPORTED BY COMPILER'
#else
      CALL point%init(DIM=1,X=0.5_SRK)
      points=point
!      
!Test clear routine
      COMPONENT_TEST('Elemental %clear()')
      CALL points%clear()
      bool = .NOT.(ANY(points%dim /= 0) .OR. ALLOCATED(points(1)%coord) .OR. &
                   ALLOCATED(points(2)%coord))
      ASSERT(bool, 'point%clear()')
!
!Test distance routine
      COMPONENT_TEST('Elemental Distance()')
      CALL points(1)%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL points2(1)%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      CALL points(2)%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL points2(2)%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      s=Distance(points,points2)
      bool = .NOT.(.NOT.(s(1) .APPROXEQ. 0.748331477354788_SRK) .OR. &
                   .NOT.(s(2) .APPROXEQ. 0.447213595499958_SRK))
      ASSERT(bool, 'Distance(...)')
!
!Test midPoint routine
      COMPONENT_TEST('Elemental Midpoint()')
      CALL points(1)%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL points2(1)%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      CALL points(2)%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL points2(2)%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      points3=midPoint(points,points2)
      bool = .NOT.(ANY(.NOT.(points3(1)%coord .APPROXEQ. 0.4_SRK)) .OR. &
                   ANY(.NOT.(points3(2)%coord .APPROXEQ. 0.4_SRK)))
      ASSERT(bool, 'midPoint(...)')
#endif
!
!Test Operators
      !Addition
      COMPONENT_TEST('Elemental OPERATOR(+)')
      CALL points(1)%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL points2(1)%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      CALL points(2)%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL points2(2)%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      points3=points+points2
      bool = .NOT.(ANY(.NOT.(points3(1)%coord .APPROXEQ. 0.8_SRK)) .OR. &
                   ANY(.NOT.(points3(2)%coord .APPROXEQ. 0.8_SRK)))
      ASSERT(bool, 'PointType Array OPERATOR(+)')
      
      !Subtraction
      COMPONENT_TEST('Elemental OPERATOR(-)')
      points3=points-points2
      bool = .NOT.(.NOT.(points3(1)%coord(1) .APPROXEQ. 0.2_SRK) .OR. &
                   .NOT.(points3(1)%coord(2) .APPROXEQ. 0.4_SRK) .OR. &
                   .NOT.(points3(1)%coord(3) .APPROXEQ. 0.6_SRK) .OR. &
                   .NOT.(points3(2)%coord(1) .APPROXEQ. 0.2_SRK) .OR. &
                   .NOT.(points3(2)%coord(2) .APPROXEQ. 0.4_SRK))
      ASSERT(bool, 'PointType Array OPERATOR(-)')
      
      !Equal to
      COMPONENT_TEST('Elemental OPERATOR(==)')
      points3=points
      bool = .NOT.(ANY(.NOT.(points == points3)) .OR. ANY(points2 == points))
      ASSERT(bool, 'PointType Array OPERATOR(==)')
      
      !Not equal to
      COMPONENT_TEST('Elemental OPERATOR(/=)')
      bool = .NOT.(ANY(.NOT.(points2 /= points)) .OR. ANY(points /= points3))
      ASSERT(bool, 'PointType Array OPERATOR(/=)')
      
      !Approximately equal to
      COMPONENT_TEST('Elemental OPERATOR(.APPROXEQ.)')
      points3=points
      points2=points
      points2(1)%coord(1)=0.50000000000002_SRK
      points2(2)%coord(1)=0.50000000000002_SRK
      points3(1)%coord(1)=0.50000000000001_SRK
      points3(2)%coord(1)=0.50000000000001_SRK
      bool = .NOT.(ANY(.NOT.(points .APPROXEQA. points3)) .OR. ANY(points2 .APPROXEQA. points))
      ASSERT(bool, 'PointType Array OPERATOR(.APPROXEQ.)')
!
!Test linked list of points
      COMPONENT_TEST('LinkedListPointType %insert()')
      ALLOCATE(firstPoint)
      thisPoint => firstPoint
      DO i=1,5
        CALL thisPoint%p%init(DIM=3,X=REAL(i,SRK),Y=0.1_SRK,Z=0.2_SRK)
        thisPoint%sortval=REAL(i,SRK)
        ALLOCATE(thisPoint%next)
        thisPoint => thisPoint%next
      ENDDO
      NULLIFY(thisPoint)
      CALL firstPoint%insert(thisPoint)
      ALLOCATE(thisPoint)
      thisPoint%sortval=-1._SRK
      CALL firstPoint%insert(thisPoint)
      thisPoint%sortval=101._SRK
      CALL firstPoint%insert(thisPoint)
      thisPoint%sortval=1._SRK
      CALL firstPoint%insert(thisPoint)
      CALL thisPoint%p%init(DIM=3,X=firstPoint%p%coord(1)+EPSREAL, &
        Y=firstPoint%p%coord(2)+EPSREAL,Z=firstPoint%p%coord(3)-EPSREAL)
      thisPoint%sortval=1._SRK+Distance(firstPoint%p,thisPoint%p)
      CALL firstPoint%insert(thisPoint)
      bool = ASSOCIATED(thisPoint)
      ASSERT(bool, 'firstPoint%insert(thisPoint) for nearby point FAILED')
      thisPoint%sortval=1.5_SRK
      CALL firstPoint%insert(thisPoint)
      bool = .NOT.(ASSOCIATED(thisPoint) .OR. firstPoint%next%sortval /= 1.5_SRK)
      ASSERT(bool, 'firstPoint%insert(thisPoint)')

      COMPONENT_TEST('LinkedListPointType %clear()')
      CALL ClearLinkedListPointType(firstPoint)
      ASSERT(.NOT.ASSOCIATED(firstPoint), 'ClearLinkedListPointType(...)')
    ENDSUBROUTINE TestPoints
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestLine
      TYPE(ParamType) :: params
!The LineType constructor allocates sp and ep, care should
!be taken not to code memory leaks. This is why clearPoints() exists.
      COMPONENT_TEST('%clear()')
      !Initialize by hand
      CALL line1%p1%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL line1%p2%init(DIM=2,X=1.0_SRK,Y=1.0_SRK)
!
!Test clear
      CALL line1%clear()
      bool=.NOT.(line1%p1%dim /= 0 .OR. line1%p2%dim /= 0 .OR. &
                   ALLOCATED(line1%p1%coord) .OR. ALLOCATED(line1%p2%coord))
      ASSERT(bool, 'line1%clear()')
      
      !Redundant call to clear
      CALL line1%clear()
!      
!Test init
      COMPONENT_TEST('%init()')
      CALL point2%init(DIM=3,X=0.1_SRK,Y=0.2_SRK,Z=0.3_SRK)
      CALL point3%init(DIM=3,X=0.4_SRK,Y=0.5_SRK,Z=0.6_SRK)
      CALL line1%set(point2,point3)
      bool=.NOT.(line1%p1%dim /= 3 .OR. line1%p2%dim /= 3 .OR. &
                 line1%p1%coord(1) /= 0.1_SRK .OR. line1%p1%coord(2) /= 0.2_SRK .OR. &
                 line1%p1%coord(3) /= 0.3_SRK .OR. line1%p2%coord(1) /= 0.4_SRK .OR. &
                 line1%p2%coord(2) /= 0.5_SRK .OR. line1%p2%coord(3) /= 0.6_SRK)
      ASSERT(bool, 'line1%set(...)')
      CALL line1%clear()
      
      !Redundant call to test input error
      CALL line1%set(point,point2)
      bool = .NOT.(line1%p1%dim /= 0 .OR. line1%p2%dim /= 0 .OR. &
                   ALLOCATED(line1%p1%coord) .OR. ALLOCATED(line1%p2%coord))
      ASSERT(bool, 'line1%set(...)')
      CALL line1%clear()
      
      CALL newGeom(params,line1)
      bool=.NOT.(line1%p1%dim /= 0 .OR. line1%p2%dim /= 0 .OR. &
                 ALLOCATED(line1%p1%coord) .OR. ALLOCATED(line1%p2%coord))
      ASSERT(bool, 'CALL newGeom(params,line1) (empty params)')
      CALL params%add('LineGeom->StartPoint',1)
      CALL params%add('LineGeom->EndPoint',3)
      CALL newGeom(params,line1)
      bool=.NOT.(line1%p1%dim /= 0 .OR. line1%p2%dim /= 0 .OR. &
                 ALLOCATED(line1%p1%coord) .OR. ALLOCATED(line1%p2%coord))
      ASSERT(bool, 'CALL newGeom(params,line1) (badparams)')
      CALL params%clear()
      CALL params%add('LineGeom->StartPoint',point2%coord)
      CALL params%add('LineGeom->EndPoint',point3%coord)
      CALL newGeom(params,line1)
      bool=.NOT.(line1%p1%dim /= 3 .OR. line1%p2%dim /= 3 .OR. &
                 line1%p1%coord(1) /= 0.1_SRK .OR. line1%p1%coord(2) /= 0.2_SRK .OR. &
                 line1%p1%coord(3) /= 0.3_SRK .OR. line1%p2%coord(1) /= 0.4_SRK .OR. &
                 line1%p2%coord(2) /= 0.5_SRK .OR. line1%p2%coord(3) /= 0.6_SRK)
      ASSERT(bool, 'CALL newGeom(params,line1)')
      CALL params%clear()
      CALL line1%clear()
!
!Test getDim
      COMPONENT_TEST('%getDim()')
      CALL line1%set(point2,point3)
      ASSERT(line1%getDim() == 3, 'line1%getDim()')
      CALL line1%clear()
      
      !Redundant call to test input error
      ASSERT(line1%getDim() == 0, 'line1%getDim()')
!      
!Test length
      COMPONENT_TEST('%length()')
      CALL line1%set(point2,point3)
      d=line1%length()
      ASSERT(d .APPROXEQ. 0.519615242270663_SRK, 'line1%length()')
!
!Test midpoint
      COMPONENT_TEST('%midpoint()')
      point=line1%midpoint()
      bool = .NOT.(point%dim /= 3 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.25_SRK) .OR. &
                   .NOT.(point%coord(2) .APPROXEQ. 0.35_SRK) .OR. &
                   .NOT.(point%coord(3) .APPROXEQ. 0.45_SRK))
      ASSERT(bool, 'line1%midpoint()')
!
!Test intersect
      !2D
      COMPONENT_TEST('%intersectLine()')
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL line1%p2%init(DIM=2,X=0.0_SRK,Y=1.0_SRK)
      CALL line2%p1%init(DIM=2,X=1.0_SRK,Y=0.0_SRK)
      CALL line2%p2%init(DIM=2,X=1.0_SRK,Y=1.0_SRK)
      !Overlap
      point=line1%intersectLine(line1)
      ASSERT(point%dim == -2, 'line1%intersectLine(...)')
      !disjoint
      point=line1%intersectLine(line2)
      ASSERT(point%dim == -3, 'line1%intersectLine(...)')
      !Normal
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/0.0_SRK,1.0_SRK/))
      CALL line2%p1%init(COORD=(/-0.5_SRK,0.5_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.5_SRK/))
      point=line1%intersectLine(line2)
      bool = .NOT.(point%dim /= 2 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.0_SRK) .OR. &
                   .NOT.(point%coord(2) .APPROXEQ. 0.5_SRK))
      ASSERT(bool, 'line1%intersectLine(...)')
      !3D
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,1.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.0_SRK,1.0_SRK/))
      !Overlap
      point=line1%intersectLine(line1)
      ASSERT(point%dim == -2, 'line1%intersectLine(...)')
      !disjoint
      point=line1%intersectLine(line2)
      ASSERT(point%dim == -3, 'line1%intersectLine(...)')
      !Normal
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      CALL line2%p1%init(COORD=(/1.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line2%p2%init(COORD=(/0.0_SRK,1.0_SRK,1.0_SRK/))
      point=line1%intersectLine(line2)
      bool = .NOT.(point%dim /= 3 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.5_SRK) .OR. &
                   .NOT.(point%coord(2) .APPROXEQ. 0.5_SRK) .OR. &
                   .NOT.(point%coord(3) .APPROXEQ. 0.5_SRK))
      ASSERT(bool, 'line1%intersectLine(...)')

      !Redundant calls to test error checking.
      CALL line2%clear()
      point=line1%intersectLine(line2) !mismatched dimensions
      ASSERT(point%dim == -1, 'point=line1%intersectLine(line2)')
      CALL line1%clear()
      CALL line1%p1%init(DIM=1,X=0.5_SRK)
      CALL line1%p2%init(DIM=1,X=0.0_SRK)
      point=line1%intersectLine(line1)
      ASSERT(point%dim == -2, '1-D line1%intersectLine(...)')
      
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/0.0_SRK,1.0_SRK/))
      CALL line2%p1%init(COORD=(/-0.5_SRK,2.5_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,2.5_SRK/))
      point=line1%intersectLine(line2)
      ASSERT(point%dim == -3, 'line1%intersectLine(...)')
      
!Test for %distance2Line(...)
      COMPONENT_TEST('%distance2Line()')
      !Line dimension error check
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/0.0_SRK,1.0_SRK/))
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      bool = .NOT.(dis%p1%dim /= -1 .AND. dis%p2%dim /= -1)
      ASSERT(bool, 'line2%distance2Line(...)')
      !Line input check
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      bool = .NOT.(dis%p1%dim /= -1 .AND. dis%p2%dim /= -1)
      ASSERT(bool, 'line2%distance2Line(...)')
      !Overlap
      CALL line1%clear()
      CALL dis%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      bool = .NOT.(dis%p1%dim /= -2 .AND. dis%p2%dim /= -2)
      ASSERT(bool, 'line2%distance2Line(...)')
      !Parallel
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      bool = .NOT.(dis%p1%dim /= -3 .AND. dis%p2%dim /= -3)
      ASSERT(bool, 'line2%distance2Line(...)')
      !Normal with interscetion
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/0.0_SRK,0.0_SRK,1.0_SRK/))
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      bool = .NOT.(dis%getDim() /= 3 .OR. mu1 /= 0._SRK .OR. mu2 /= -1._SRK &
                   .OR. dis%p1 /= line2%p1 .OR. dis%length() > EPSREAL)
      ASSERT(bool, 'line2%distance2Line(...)')
      !Normal without interscetion
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/0.0_SRK,1.0_SRK,0.0_SRK/))
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      bool = .NOT.(dis%getDim() /= 3 .OR. mu1 /= 0._SRK .OR. mu2 /= 0._SRK &
                   .OR. dis%p1 /= line2%p1 .OR. dis%p2 /= line1%p1)
      ASSERT(bool, 'line2%distance2Line(...)')
      
!Test for %distance2Point(...)
      COMPONENT_TEST('%distance2Point()')
      CALL line1%clear()
      CALL point%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK/))
      CALL point%init(COORD=(/-0.5_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 0.25_SRK
      ASSERT(bool, '1-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 0.0_SRK
      ASSERT(bool, '1-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/1.5_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 0.25_SRK
      ASSERT(bool, '1-D line1%distance2Point(...)')
      
      CALL line1%clear()
      CALL point%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK/))
      CALL point%init(COORD=(/-0.5_SRK/))
      ASSERT(line1%distance2Point(point) == -1.0_SRK, '2-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK/))
      ASSERT(line1%distance2Point(point) .APPROXEQ. 0.0_SRK, '2-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.5_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 0.125_SRK
      ASSERT(bool, '2-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,0.5_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 1.25_SRK
      ASSERT(bool, '2-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/-0.5_SRK,-1.0_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 1.25_SRK
      ASSERT(bool, '2-D line1%distance2Point(...)')
      
      CALL line1%clear()
      CALL point%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 0.0_SRK
      ASSERT(bool, '3-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.0_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 0.1666666666666667_SRK
      ASSERT(bool, '3-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/1.0_SRK,1.0_SRK,2.0_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 1.0_SRK
      ASSERT(bool, '3-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 1.0_SRK
      ASSERT(bool, '3-D line1%distance2Point(...)')
      
!Test pointIsLeft
      COMPONENT_TEST('%pointIsLeft()')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,1.0_SRK,-1.0_SRK/))
      ASSERT(line1%pointIsLeft(point),'Point (0.0,1.0,-1.0)')
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,1.0_SRK,-10.0_SRK/))
      ASSERT(.NOT.line1%pointIsLeft(point),'Point (2.0,1.0,-10.0)')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      ASSERT(.NOT.line1%pointIsLeft(point),'Point (0.5,0.5,0.5)')
      
!Test pointIsLeft
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,1.0_SRK,-10.0_SRK/))
      COMPONENT_TEST('%pointIsRight()')
      ASSERT(line1%pointIsRight(point),'Point (2.0,1.0,-10.0)')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,1.0_SRK,-1.0_SRK/))
      ASSERT(.NOT.line1%pointIsRight(point),'Point (0.0,1.0,-1.0)')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      ASSERT(.NOT.line1%pointIsLeft(point),'Point (0.5,0.5,0.5)')
      
      !Test for equivalence operation
      COMPONENT_TEST('OPERATOR(==)')
      line2=line1
      ASSERT(line1 == line2,'3-D line equivalence')
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK/))
      line2=line1
      ASSERT(line1 == line2,'2-D line equivalence')
      CALL line2%clear()
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      ASSERT(.NOT.(line1 == line2),'2-D and 3-D line non-equivalence')
      
#ifdef __GFORTRAN__
      WRITE(*,*) 'ELEMENTAL METHODS FOR NON-SCALAR BASE OBJECTS NOT YET SUPPORTED BY COMPILER'
#else
      CALL points2%clear()
      CALL points3%clear()
      CALL points2(1)%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL points2(2)%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      points3=points2
      points3(1)%coord=points3(1)%coord+0.1_SRK
      points3(2)%coord=points3(2)%coord+0.1_SRK
!This no longer works with Intel 12
      COMPONENT_TEST('Elemental %set()')
      CALL lines%set(points2,points3)
!      CALL lines(1)%set(points2(1),points3(1))
!      CALL lines(2)%set(points2(2),points3(2))
      bool = .NOT.(lines(1)%p1%dim /= 3 .OR. lines(1)%p2%dim /= 3 .OR. & 
                  ANY(.NOT.(lines(1)%p1%coord .APPROXEQ. (/0.5_SRK,0.6_SRK,0.7_SRK/))) .OR. &
                  ANY(.NOT.(lines(1)%p2%coord .APPROXEQ. (/0.6_SRK,0.7_SRK,0.8_SRK/))) .OR. &
                  lines(2)%p1%dim /= 2 .OR. lines(2)%p2%dim /= 2 .OR. &
                  ANY(.NOT.(lines(2)%p1%coord .APPROXEQ. (/0.5_SRK,0.6_SRK/))) .OR. &
                  ANY(.NOT.(lines(2)%p2%coord .APPROXEQ. (/0.6_SRK,0.7_SRK/))))
      ASSERT(bool, 'lines%set(...)')
      
      COMPONENT_TEST('Elemental %getDim()')
      ldim=lines%getDim()
      bool = .NOT.(ldim(1) /= 3 .OR. ldim(2) /= 2)
      ASSERT(bool, 'lines%getDim()')
      
      COMPONENT_TEST('Elemental %length()')
      s=lines%length()
      bool = .NOT.(.NOT.(s(1) .APPROXEQ. 0.173205080756888_SRK) .OR. &
                   .NOT.(s(2) .APPROXEQ. 0.141421356237309_SRK))
      ASSERT(bool, 'lines%length()')
      
      COMPONENT_TEST('Elemental %midPoint()')
      points=lines%midPoint()
      bool = .NOT.(ANY(.NOT.(points(1)%coord .APPROXEQ. (/0.55_SRK,0.65_SRK,0.75_SRK/))) .OR. &
                   ANY(.NOT.(points(2)%coord .APPROXEQ. (/0.55_SRK,0.65_SRK/))))
      ASSERT(bool, 'lines%midPoint()')
      
      COMPONENT_TEST('Elemental %intersectLine()')
      points=lines%intersectLine(lines)
      bool = .NOT.(points(1)%dim /= -2 .OR. points(2)%dim /= -2)
      ASSERT(bool, 'lines%intersectLine(...)')
      
     !diss=lines%distance2Line(lines)
     ! IF(diss(1)%p(1)%dim /= -2 .OR. diss(2)%p(1)%dim /= -2) THEN
     !   WRITE(*,*) 'lines%distance2Line(...) FAILED!'
     !   STOP 666
     ! ELSE
     !   WRITE(*,*) '  Passed: lines%distance2Line(...)'
     ! ENDIF
     !
      COMPONENT_TEST('Elemental %distance2Point()')
      points=lines%midPoint()
      s=lines%distance2Point(points)
      bool = .NOT.(ANY(.NOT.(s .APPROXEQ. 0.0_SRK)))
      ASSERT(bool, 'lines%distance2Point(...)')
      
      COMPONENT_TEST('Elemental %clear()')
      CALL lines%clear()
      bool = .NOT.((lines(1)%p1%dim /= 0) .OR. (lines(1)%p2%dim /= 0) .OR. &
                   (lines(2)%p1%dim /= 0) .OR. (lines(2)%p2%dim /= 0) .OR. &
                   ALLOCATED(lines(1)%p1%coord) .OR. ALLOCATED(lines(1)%p2%coord) .OR. &
                   ALLOCATED(lines(2)%p1%coord) .OR. ALLOCATED(lines(2)%p2%coord))
      ASSERT(bool, 'lines%clear(...)')
#endif
    ENDSUBROUTINE TestLine
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestPlane
      REAL(SRK) :: n(3)
      TYPE(ParamType) :: params
!
!Test clear
      COMPONENT_TEST('%clear()')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      plane1%n=(/1.0_SRK,1.0_SRK,1.0_SRK/)
      plane1%v0=point2
      CALL plane1%clear()
      bool = .NOT.(ANY(plane1%n /= 0.0_SRK) .OR. plane1%v0%dim /= 0 .OR. &
                   ALLOCATED(plane1%v0%coord))
      ASSERT(bool, 'plane1%clear()')
      
      COMPONENT_TEST('%set()')
      n=(/1.0_SRK,1.0_SRK,1.0_SRK/)
      CALL plane1%set(n,point)
      bool = .NOT.(ANY(plane1%v0%coord /= 0.5_SRK) .OR. ANY(.NOT.(plane1%n .APPROXEQ. &
                   (/1._SRK/SQRT(3.0_SRK),1._SRK/SQRT(3.0_SRK),1._SRK/SQRT(3.0_SRK)/))))
      ASSERT(bool, 'plane1%set(...)')
      CALL plane1%clear()
      
      CALL newGeom(params,plane1)
      bool = .NOT.(ANY(plane1%n /= 0.0_SRK) .OR. plane1%v0%dim /= 0 .OR. &
                   ALLOCATED(plane1%v0%coord))
      ASSERT(bool, 'CALL newGeom(params,plane1) (empty params)')
      CALL params%add('PlaneGeom->Point',1)
      CALL params%add('PlaneGeom->NormalVector',3)
      CALL newGeom(params,plane1)
      bool = .NOT.(ANY(plane1%n /= 0.0_SRK) .OR. plane1%v0%dim /= 0 .OR. &
                   ALLOCATED(plane1%v0%coord))
      ASSERT(bool, 'CALL newGeom(params,plane1) (bad params)')
      CALL params%clear()
      CALL params%add('PlaneGeom->Point',point%coord)
      CALL params%add('PlaneGeom->NormalVector',n)
      CALL newGeom(params,plane1)
      bool = .NOT.(ANY(plane1%v0%coord /= 0.5_SRK) .OR. ANY(.NOT.(plane1%n .APPROXEQ. &
                   (/1._SRK/SQRT(3.0_SRK),1._SRK/SQRT(3.0_SRK),1._SRK/SQRT(3.0_SRK)/))))
      ASSERT(bool, 'CALL newGeom(params,plane1)')
      
      !Test disjoint-ness
      COMPONENT_TEST('%intersect()')
      CALL line1%clear()
      CALL line1%p1%init(COORD=(/0._SRK,0._SRK,0._SRK/))
      CALL line1%p2%init(COORD=(/0.1_SRK,0.1_SRK,0.1_SRK/))
      point2=plane1%intersectLine(line1)
      ASSERT(point2%dim == -3, 'plane%intersect(...)')
      
      !Test for collinearity
      CALL line1%clear()
      CALL line1%p1%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      CALL line1%p2%init(COORD=(/0.75_SRK,0.75_SRK,0._SRK/))
      point2=plane1%intersectLine(line1)
      ASSERT(point2%dim == -2, 'plane%intersect(...)')
      
      !Test for parallel
      CALL line1%clear()
      CALL line1%p1%init(COORD=(/0.4_SRK,0.4_SRK,0.4_SRK/))
      CALL line1%p2%init(COORD=(/0.65_SRK,0.65_SRK,-0.1_SRK/))
      point2=plane1%intersectLine(line1)
      ASSERT(point2%dim == -3, 'plane%intersect(...)')
      
      CALL line1%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      point2=plane1%intersectLine(line1)
      bool = .NOT.(point2%dim /= 3 .OR. ANY(.NOT.(point2%coord .APPROXEQ. 0.5_SRK)))
      ASSERT(bool, 'plane1%intersect(...)')

      !Test for bad input
      CALL line1%clear()
      point2=plane1%intersectLine(line1)
      ASSERT(point2%dim == -1, 'plane%intersect(...)')
      
      CALL line2%clear
      CALL line2%p1%init(COORD=(/-0.5_SRK,2.5_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,2.5_SRK/))
      point2=plane1%intersectLine(line2)
      ASSERT(point2%dim == -1, 'plane%intersect(...)')
      
      CALL plane1%clear()
      point2=plane1%intersectLine(line2)
      ASSERT(point2%dim == -1, 'plane%intersect(...)')
      
      !Test for equivalence operation
      COMPONENT_TEST('OPERATOR(==)')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      n=(/1.0_SRK,1.0_SRK,1.0_SRK/)
      CALL plane1%set(n,point)
      plane2=plane1
      ASSERT(plane1 == plane2,'plane equivalence')
      CALL plane2%set((/1.0_SRK,0.0_SRK,1.0_SRK/),point)
      ASSERT(.NOT.(plane1 == plane2),'plane non-equivalence')
      
#ifdef __GFORTRAN__
      WRITE(*,*) 'ELEMENTAL METHODS FOR NON-SCALAR BASE OBJECTS NOT YET SUPPORTED BY COMPILER'
#else      
      COMPONENT_TEST('Elemental %set()')
      CALL plane1%set((/1.0_SRK,1.0_SRK,1.0_SRK/),point)
      planes=plane1
      CALL line1%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      points=planes%intersectLine(line1)
      bool = .NOT.(points(1)%dim /= 3 .OR. ANY(.NOT.(points(1)%coord .APPROXEQ. 0.5_SRK)) .OR. &
                   points(2)%dim /= 3 .OR. ANY(.NOT.(points(2)%coord .APPROXEQ. 0.5_SRK)))
      ASSERT(bool, 'planes%intersect(...)')
      
      COMPONENT_TEST('Elemental %clear()')
      CALL planes%clear()
      bool = .NOT.(ANY(planes(1)%n /= 0.0_SRK) .OR. planes(1)%v0%dim /= 0 .OR. &
                   ALLOCATED(planes(1)%v0%coord) .OR. ANY(planes(2)%n /= 0.0_SRK) .OR. &
                   planes(2)%v0%dim /= 0 .OR. ALLOCATED(planes(2)%v0%coord))
      ASSERT(bool, 'planes%clear()')
#endif
    ENDSUBROUTINE TestPlane
!
!-------------------------------------------------------------------------------    
    SUBROUTINE TestCircle_and_Cylinder
      TYPE(ParamType) :: params
!
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
      
      CALL newGeom(params,circle1)
      bool=.NOT.(circle1%r /= 0.0_SRK .OR. circle1%c%dim /= 0 .OR. &
                 ALLOCATED(circle1%c%coord))
      ASSERT(bool, 'CALL newGeom(params,circle1) (empty params)')
      CALL params%add('CircleGeom -> Radius',1)
      CALL params%add('CircleGeom -> Centroid',2)
      CALL params%add('CircleGeom -> StartingAngle',.FALSE.)
      CALL params%add('CircleGeom -> StoppingAngle',(/0.0,0.0/))
      CALL newGeom(params,circle1)
      bool=.NOT.(circle1%r /= 0.0_SRK .OR. circle1%c%dim /= 0 .OR. &
                 ALLOCATED(circle1%c%coord))
      ASSERT(bool, 'CALL newGeom(params,circle1) (bad params)')
      CALL params%clear()
      CALL params%add('CircleGeom -> Radius',0.5_SRK)
      CALL params%add('CircleGeom -> Centroid',point%coord)
      CALL newGeom(params,circle1)
      bool=.NOT.(circle1%r /= 0.5_SRK .OR. circle1%c%dim /= 2 .OR. &
                 circle1%c%coord(1) /= 0.1_SRK .OR. circle1%c%coord(2) /= 0.2_SRK)
      ASSERT(bool, 'CALL newGeom(params,circle1)')
      CALL params%clear()
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
      
      !Test 1 point of intersection for arc
      CALL circle1%clear()
      CALL line1%p1%init(DIM=2,X=-0.4_SRK,Y=-0.3_SRK)
      CALL line1%p2%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL circle1%set(point,0.4225_SRK,ANGSTT=PI,ANGSTP=TWOPI)
      CALL circle1%intersectLine(line1,point2,point3)
      bool = ALL(point2%coord .APPROXEQ. (/-0.3380_SRK,-0.2535_SRK/)) &
        .AND. point3%dim == 0
      ASSERT(bool,'circle1%intersectLine(...) (arc 1-point)')
      CALL line1%clear()
      
      !Test 2 points of intersection for arc
      CALL circle1%clear()
      CALL line1%p1%init(DIM=2,X=-0.3_SRK,Y=0.4_SRK)
      CALL line1%p2%init(DIM=2,X=0.4_SRK,Y=0.2_SRK)
      CALL circle1%set(point,0.4225_SRK,ANGSTT=ZERO,ANGSTP=PI)
      CALL circle1%intersectLine(line1,point2,point3)
      bool = ALL(point2%coord .APPROXEQA. (/-0.200892135566299_SRK,0.371683467304657_SRK/)) &
        .AND. ALL(point3%coord .APPROXEQA. (/0.3669298714153550_SRK,0.209448608167041_SRK/))
      ASSERT(bool,'circle1%intersectLine(...) (arc 2-points)')
      
      !Test missed intersection for arc
      !CALL circle1%clear()
      !CALL point2%clear()
      !CALL point3%clear()
      !CALL line1%p1%init(DIM=2,X=-0.3_SRK,Y=0.4_SRK)
      !CALL line1%p2%init(DIM=2,X=0.4_SRK,Y=0.2_SRK)
      !CALL circle1%set(point,0.4225_SRK,ANGSTT=PI,ANGSTP=TWOPI)
      !CALL circle1%intersectLine(line1,point2,point3)
      !bool = (point2%dim == 0) .AND. (point3%dim == 0)
      !ASSERT(bool,'circle1%intersectLine(...) (arc miss)')
      !FINFO() point2%dim, point2%coord
      !FINFO() point3%dim, point3%coord
      
!Test hasPoint
      COMPONENT_TEST('Circle %hasPoint()')
      !Full circle
      CALL point%clear()
      CALL point%init(COORD=(/1.5_SRK,1.5_SRK/))
      CALL circle2%set(point,0.4225_SRK)
      CALL point%clear()
      CALL point%init(COORD=(/1.6_SRK,1.6_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle internal point, Q1')
      CALL point%clear()
      CALL point%init(COORD=(/1.4_SRK,1.6_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle internal point, Q2')
      CALL point%clear()
      CALL point%init(COORD=(/1.4_SRK,1.4_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle internal point, Q3')
      CALL point%clear()
      CALL point%init(COORD=(/1.6_SRK,1.4_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle internal point, Q4')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK/))
      ASSERT(.NOT.circle2%hasPoint(point),'Circle external point')
      
      !Test half circle
      CALL point%clear()
      CALL point%init(COORD=(/-1.5_SRK,-1.5_SRK/))
      CALL circle2%set(point,0.4225_SRK,ANGSTT=ZERO,ANGSTP=PI)
      ASSERT(circle2%hasPoint(circle2%c),'Circle Arc Centroid')
      CALL point%clear()
      CALL point%init(COORD=(/-1.4_SRK,-1.4_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle Arc internal point, Q1')
      CALL point%clear()
      CALL point%init(COORD=(/-1.6_SRK,-1.4_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle Arc internal point, Q2')
      CALL point%clear()
      CALL point%init(COORD=(/-1.6_SRK,-1.6_SRK/))
      ASSERT(.NOT.circle2%hasPoint(point),'Circle Arc external point, Q3')
      CALL point%clear()
      CALL point%init(COORD=(/-1.4_SRK,-1.6_SRK/))
      ASSERT(.NOT.circle2%hasPoint(point),'Circle Arc external point, Q4')
      
      !Test 1st and 2nd quadrant arc
      CALL point%clear()
      CALL point%init(COORD=(/-1.5_SRK,1.5_SRK/))
      CALL circle2%set(point,0.4225_SRK,ANGSTT=QTRPI,ANGSTP=HALFPI+QTRPI)
      ASSERT(circle2%hasPoint(circle2%c),'Circle Arc Centroid')
      CALL point%clear()
      CALL point%init(COORD=(/-1.35_SRK,1.6_SRK/))
      ASSERT(.NOT.circle2%hasPoint(point),'Circle Arc external point Q1')
      CALL point%clear()
      CALL point%init(COORD=(/-1.4_SRK,1.6_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle Arc internal point Q1')
      CALL point%clear()
      CALL point%init(COORD=(/-1.6_SRK,1.6_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle Arc internal point Q2')
      CALL point%clear()
      CALL point%init(COORD=(/-1.7_SRK,1.6_SRK/))
      ASSERT(.NOT.circle2%hasPoint(point),'Circle Arc external point Q2')
      CALL point%clear()
      CALL point%init(COORD=(/-1.6_SRK,1.4_SRK/))
      ASSERT(.NOT.circle2%hasPoint(point),'Circle Arc external point Q3')
      CALL point%clear()
      CALL point%init(COORD=(/-1.4_SRK,1.4_SRK/))
      ASSERT(.NOT.circle2%hasPoint(point),'Circle Arc external point Q4')
      
      !Test "reflex" arc
      CALL point%clear()
      CALL point%init(COORD=(/1.5_SRK,-1.5_SRK/))
      CALL circle2%set(point,0.4225_SRK,ANGSTT=ZERO,ANGSTP=PI+HALFPI+QTRPI)
      ASSERT(circle2%hasPoint(circle2%c),'Circle Arc Centroid')
      CALL point%clear()
      CALL point%init(COORD=(/1.6_SRK,-1.4_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle Arc internal point Q1')
      CALL point%clear()
      CALL point%init(COORD=(/1.45_SRK,-1.4_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle Arc internal point Q2')
      CALL point%clear()
      CALL point%init(COORD=(/1.4_SRK,-1.6_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle Arc internal point, Q3')
      CALL point%clear()
      CALL point%init(COORD=(/1.6_SRK,-1.6_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle Arc internal point on bndy, Q4')
      CALL point%clear()
      CALL point%init(COORD=(/1.7_SRK,-1.6_SRK/))
      ASSERT(.NOT.circle2%hasPoint(point),'Circle Arc external point Q4')
      
      !Test 3rd and 4th quadrant arc
      CALL point%clear()
      CALL point%init(COORD=(/1.5_SRK,1.5_SRK/))
      CALL circle2%set(point,0.4225_SRK,ANGSTT=PI+QTRPI,ANGSTP=PI+HALFPI+QTRPI)
      ASSERT(circle2%hasPoint(circle2%c),'Circle Arc Centroid')
      CALL point%clear()
      CALL point%init(COORD=(/1.6_SRK,1.6_SRK/))
      ASSERT(.NOT.circle2%hasPoint(point),'Circle Arc external point Q1')
      CALL point%clear()
      CALL point%init(COORD=(/1.4_SRK,1.6_SRK/))
      ASSERT(.NOT.circle2%hasPoint(point),'Circle Arc external point Q2')
      CALL point%clear()
      CALL point%init(COORD=(/1.3_SRK,1.4_SRK/))
      ASSERT(.NOT.circle2%hasPoint(point),'Circle Arc external point Q3')
      CALL point%clear()
      CALL point%init(COORD=(/1.4_SRK,1.4_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle Arc internal point Q3')
      CALL point%clear()
      CALL point%init(COORD=(/1.5_SRK,1.4_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle Arc internal point Q3-Q4')
      CALL point%clear()
      CALL point%init(COORD=(/1.6_SRK,1.4_SRK/))
      ASSERT(circle2%hasPoint(point),'Circle Arc internal point Q4')
      CALL point%clear()
      CALL point%init(COORD=(/1.7_SRK,1.4_SRK/))
      ASSERT(.NOT.circle2%hasPoint(point),'Circle Arc external point Q4')
      
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
      
      CALL newGeom(params,cylinder1)
      bool=.NOT.(cylinder1%r /= 0.0_SRK .OR. cylinder1%axis%p1%dim /= 0 .OR. &
                 ALLOCATED(cylinder1%axis%p1%coord) .OR. cylinder1%axis%p2%dim /= 0 .OR. &
                 ALLOCATED(cylinder1%axis%p2%coord))
      ASSERT(bool,'CALL newGeom(params,cylinder1) (empty params)')
      CALL params%add('CylinderGeom -> Radius',1)
      CALL params%add('CylinderGeom -> BottomCentroid',2)
      CALL params%add('CylinderGeom -> TopCentroid',3)
      CALL params%add('CylinderGeom -> StartingAngle',.FALSE.)
      CALL params%add('CylinderGeom -> StoppingAngle',(/0.0,0.0/))
      CALL newGeom(params,cylinder1)
      bool=.NOT.(cylinder1%r /= 0.0_SRK .OR. cylinder1%axis%p1%dim /= 0 .OR. &
                 ALLOCATED(cylinder1%axis%p1%coord) .OR. cylinder1%axis%p2%dim /= 0 .OR. &
                 ALLOCATED(cylinder1%axis%p2%coord))
      ASSERT(bool,'CALL newGeom(params,cylinder1) (bad params)')
      CALL params%clear()
      CALL params%add('CylinderGeom -> Radius',1.0_SRK)
      CALL params%add('CylinderGeom -> BottomCentroid',point2%coord)
      CALL params%add('CylinderGeom -> TopCentroid',point3%coord)
      CALL newGeom(params,cylinder1)
      bool=.NOT.(cylinder1%r /= 1.0_SRK .OR. cylinder1%axis%p1%dim /= 3 .OR. &
                 ANY(cylinder1%axis%p1%coord /= (/0.1_SRK,0.2_SRK,0.3_SRK/)) .OR. &
                 cylinder1%axis%p2%dim /= 3 .OR. &
                 ANY(cylinder1%axis%p2%coord /= (/0.1_SRK,0.2_SRK,1.3_SRK/)))
      ASSERT(bool,'CALL newGeom(params,cylinder1)')
      CALL params%clear()
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
    SUBROUTINE TestBox
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
      
      CALL newGeom(params,box)
      bool = .NOT.(ANY(box%u /= 0.0_SRK) .OR. ANY(box%e /= 0.0_SRK) &
                   .OR. box%p0%dim /= 0 .OR. ALLOCATED(box%p0%coord))
      ASSERT(bool, 'CALL newGeom(params,box) (empty params)')
      CALL params%add('BoxGeom -> CornerPoint',1)
      CALL params%add('BoxGeom -> Vector1',5)
      CALL params%add('BoxGeom -> Vector2',1)
      CALL params%add('BoxGeom -> ExtentVector',0)
      CALL newGeom(params,box)
      bool = .NOT.(ANY(box%u /= 0.0_SRK) .OR. ANY(box%e /= 0.0_SRK) &
                   .OR. box%p0%dim /= 0 .OR. ALLOCATED(box%p0%coord))
      ASSERT(bool, 'CALL newGeom(params,box) (bad params)')
      CALL params%clear()
      CALL params%add('BoxGeom -> CornerPoint',point%coord)
      CALL params%add('BoxGeom -> Vector1',u1_3d)
      CALL params%add('BoxGeom -> Vector2',u2_3d)
      CALL params%add('BoxGeom -> Vector3',u3_3d)
      CALL params%add('BoxGeom -> ExtentVector',e_3d)
      CALL newGeom(params,box)
      bool = .NOT.(box%p0 /= point .OR. ANY(box%e /= e_3d) &
                  .OR. ANY(.NOT.(box%u(:,1) .APPROXEQ. u1_3d/SQRT(2._SRK))) &
                  .OR. ANY(.NOT.(box%u(:,2) .APPROXEQ. u2_3d/SQRT(2._SRK))) &
                  .OR. ANY(.NOT.(box%u(:,3) .APPROXEQ. u3_3d*0.5_SRK)) )
      ASSERT(bool, 'CALL newGeom(params,box)')
      CALL params%clear()

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

ENDPROGRAM testGeom
