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
PROGRAM testGeom_Points
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV  
  USE UnitTest
  USE IntrType
  USE Constants_Conversion
  USE ParameterLists
  USE Geom_Points
  USE Geom
  
  IMPLICIT NONE
  
  TYPE(PointType) :: point,point2,point3
  TYPE(PointType) :: points(2),points2(2),points3(2)
  TYPE(LinkedListPointType),POINTER :: firstPoint,thisPoint
  INTEGER(SIK) :: i
  REAL(SRK) :: d,s(2)
  LOGICAL(SBK) :: bool
  
  CREATE_TEST('Test Geom')
  CALL eParams%setQuietMode(.TRUE.)
  CALL eParams%setStopOnError(.FALSE.)
  
  REGISTER_SUBTEST('Test Points',TestPoints)

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
!Test innerAngle
      COMPONENT_TEST('innerAngle()')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=2.0_SRK)
      CALL point2%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL point3%init(DIM=2,X=5.0_SRK,Y=0.0_SRK)
      ASSERT(innerAngle(point,point2,point3) .APPROXEQA. HALFPI,'90 right angle')
      CALL point%clear()
      CALL point2%clear()
      CALL point3%clear()
      CALL point%init(DIM=2,X=1.75_SRK,Y=2.134189828107240_SRK)
      CALL point2%init(DIM=2,X=0.25_SRK,Y=1.5_SRK)
      CALL point3%init(DIM=2,X=1.75_SRK,Y=0.865810171892757_SRK)
      ASSERT(innerAngle(point,point2,point3) .APPROXEQA. 0.8_SRK,'acute angle')
      CALL point%clear()
      CALL point2%clear()
      CALL point3%clear()
      CALL point%init(DIM=2,X=1.5_SRK+2.0_SRK*COS(QTRPI*0.25_SRK), &
        Y=0.5_SRK+2.0_SRK*SIN(QTRPI*0.25_SRK))
      CALL point2%init(DIM=2,X=1.5_SRK,Y=0.5_SRK)
      CALL point3%init(DIM=2,X=1.5_SRK+4.0_SRK*COS(QTRPI*0.25_SRK), &
        Y=0.5_SRK+4.0_SRK*SIN(-QTRPI*0.25_SRK))
      ASSERT(innerAngle(point,point2,point3) .APPROXEQA. QTRPI*0.5_SRK,'45 acute angle')
      CALL point%clear()
      CALL point2%clear()
      CALL point3%clear()
      CALL point%init(DIM=2,X=-40.0_SRK,Y=61.5_SRK)
      CALL point2%init(DIM=2,X=20.0_SRK,Y=1.5_SRK)
      CALL point3%init(DIM=2,X=60.0_SRK,Y=1.5_SRK)
      ASSERT(innerAngle(point,point2,point3) .APPROXEQA. PI-QTRPI,'135 obtuse angle')
      CALL point%clear()
      CALL point2%clear()
      CALL point3%clear()
      CALL point%init(DIM=2,X=-40.0_SRK,Y=0.5_SRK)
      CALL point2%init(DIM=2,X=20.0_SRK,Y=0.5_SRK)
      CALL point3%init(DIM=2,X=60.0_SRK,Y=0.5_SRK)
      ASSERT(innerAngle(point,point2,point3) .APPROXEQA. PI,'180 straight angle')
!
!Test outerAngle
      COMPONENT_TEST('outerAngle()')
      CALL point%clear()
      CALL point2%clear()
      CALL point3%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=2.0_SRK)
      CALL point2%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL point3%init(DIM=2,X=5.0_SRK,Y=0.0_SRK)
      ASSERT(outerAngle(point,point2,point3) .APPROXEQA. TWOPI-HALFPI,'270 right angle')
      CALL point%clear()
      CALL point2%clear()
      CALL point3%clear()
      CALL point%init(DIM=2,X=1.75_SRK,Y=2.134189828107240_SRK)
      CALL point2%init(DIM=2,X=0.25_SRK,Y=1.5_SRK)
      CALL point3%init(DIM=2,X=1.75_SRK,Y=0.865810171892757_SRK)
      ASSERT(outerAngle(point,point2,point3) .APPROXEQA. TWOPI-0.8_SRK,'acute angle')
      CALL point%clear()
      CALL point2%clear()
      CALL point3%clear()
      CALL point%init(DIM=2,X=1.5_SRK+2.0_SRK*COS(QTRPI*0.25_SRK), &
        Y=0.5_SRK+2.0_SRK*SIN(QTRPI*0.25_SRK))
      CALL point2%init(DIM=2,X=1.5_SRK,Y=0.5_SRK)
      CALL point3%init(DIM=2,X=1.5_SRK+4.0_SRK*COS(QTRPI*0.25_SRK), &
        Y=0.5_SRK+4.0_SRK*SIN(-QTRPI*0.25_SRK))
      ASSERT(outerAngle(point,point2,point3) .APPROXEQA. TWOPI-QTRPI*0.5_SRK,'315 acute angle')
      CALL point%clear()
      CALL point2%clear()
      CALL point3%clear()
      CALL point%init(DIM=2,X=-40.0_SRK,Y=61.5_SRK)
      CALL point2%init(DIM=2,X=20.0_SRK,Y=1.5_SRK)
      CALL point3%init(DIM=2,X=60.0_SRK,Y=1.5_SRK)
      ASSERT(outerAngle(point,point2,point3) .APPROXEQA. PI+QTRPI,'225 obtuse angle')
      CALL point%clear()
      CALL point2%clear()
      CALL point3%clear()
      CALL point%init(DIM=2,X=-40.0_SRK,Y=0.5_SRK)
      CALL point2%init(DIM=2,X=20.0_SRK,Y=0.5_SRK)
      CALL point3%init(DIM=2,X=60.0_SRK,Y=0.5_SRK)
      ASSERT(outerAngle(point,point2,point3) .APPROXEQA. PI,'180 straight angle')
      
!
!Test Operators
      COMPONENT_TEST('OPERATOR(+)')
      CALL point%clear()
      CALL point2%clear()
      CALL point3%clear()
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

ENDPROGRAM testGeom_Points
