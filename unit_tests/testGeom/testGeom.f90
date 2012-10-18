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
      
  USE IntrType
  USE Geom
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE
  
  TYPE(PointType) :: point,point2,point3,points(2),points2(2),points3(2)
  TYPE(LinkedListPointType),POINTER :: firstPoint,thisPoint
  TYPE(LineType) :: line1,line2,lines(2),dis,diss(2)
  TYPE(PlaneType) :: plane1,planes(2)
  TYPE(CircleType) :: circle1,circles(2)
  TYPE(CylinderType) :: cylinder1,cylinders(2)
  TYPE(OBBType) :: box,boxs(2)
  INTEGER(SIK) :: ldim(2),i,ioerr
  REAL(SRK) :: d,s(2),mu1,mu2,mu1s(2),mu2s(2)
  REAL(SRK) :: e_2d(2),e_3d(3),u1_2d(2),u2_2d(2),u3_2d(2)
  REAL(SRK) :: u1_3d(3),u2_3d(3),u3_3d(3)
  CHARACTER(LEN=MAX_COORD_STR_LEN) :: tempstr
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING GEOM...'
  WRITE(*,*) '==================================================='
  
  CALL TestPoints()
  CALL TestLine()
  CALL TestPlane()
  CALL TestCircle_and_Cylinder()
  CALL TestBox()
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING GEOM PASSED!'
  WRITE(*,*) '==================================================='
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestPoints
      WRITE(*,*) 'TESTING POINTTYPE (scalar)'
      
      !Initialize by hand
      !or point=PointType(2,(/0.5_SRK,0.3_SRK/))
      point%dim=1
      ALLOCATE(point%coord(1))
      point%coord(1)=0.5_SRK
!      
!Test clear routine
      CALL point%clear()
      IF(point%dim /= 0 .OR. ALLOCATED(point%coord)) THEN
        WRITE(*,*) 'CALL point%clear() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL point%clear()'
      ENDIF
      CALL point%clear() !Test redundant call doesn't have an error
!      
!Test initialization
      CALL point%init(DIM=1,X=0.5_SRK) !test 1-D
      IF(point%dim /= 1 .OR. point%coord(1) /= 0.5_SRK) THEN
        WRITE(*,*) 'CALL point%init(DIM=1,X=0.5) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL point%init(DIM=1,X=0.5)'
      ENDIF
      
      !Test redundant call
      CALL point%init(DIM=2,X=0.6_SRK,Y=0.7_SRK)
      IF(point%dim /= 1 .OR. point%coord(1) /= 0.5_SRK) THEN
        WRITE(*,*) 'CALL point%init(DIM=1,X=0.5) FAILED!'
        STOP 666
      ENDIF
      CALL point%clear()
      
      CALL point%init(DIM=2,X=0.2_SRK,Y=0.3_SRK) !test 2-D
      IF(point%dim /= 2 .OR. point%coord(1) /= 0.2_SRK .OR. &
        point%coord(2) /= 0.3_SRK) THEN
        WRITE(*,*) 'CALL point%init(DIM=2,X=0.2_SRK,Y=0.3_SRK) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL point%init(DIM=2,X=0.2_SRK,Y=0.3_SRK)'
      ENDIF
      CALL point%clear()
      
      CALL point%init(DIM=3,X=0.7_SRK,Y=0.8_SRK,Z=0.9_SRK) !test 3-D
      IF(point%dim /= 3 .OR. point%coord(1) /= 0.7_SRK .OR. &
        point%coord(2) /= 0.8_SRK .OR. point%coord(3) /= 0.9_SRK) THEN
        WRITE(*,*) 'CALL point%init(DIM=3,X=0.7_SRK,Y=0.8_SRK,Z=0.9_SRK) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL point%init(DIM=3,X=0.7_SRK,Y=0.8_SRK,Z=0.9_SRK)'
      ENDIF
      CALL point%clear()
      
      CALL point%init(COORD=(/1.7_SRK,1.8_SRK,1.9_SRK,1.0_SRK/)) !test N-D
      IF(point%dim /= 4 .OR. point%coord(1) /= 1.7_SRK .OR. &
        point%coord(2) /= 1.8_SRK .OR. point%coord(3) /= 1.9_SRK .OR. &
          point%coord(4) /= 1.0_SRK) THEN
        WRITE(*,*) 'CALL point%init(COORD=(/.../)) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL point%init(COORD=(/.../))'
      ENDIF
!      
!Test getCoordString
      IF(TRIM(point%getCoordString()) /= '( 1.70000000000000E+00, '// &
        '1.80000000000000E+00, 1.90000000000000E+00, '// &
          '1.00000000000000E+00)' .OR. TRIM(point%getCoordString( &
            'f6.3')) /= '( 1.700, 1.800, 1.900, 1.000)') THEN
        WRITE(*,*) 'CALL point%getCoordString(...)) FAILED!'
        STOP 666
      ENDIF
#ifdef DEBUG
      IF(LEN_TRIM(point%getCoordString('i6')) /= 0 .OR. &
           LEN_TRIM(point2%getCoordString()) /= 0) THEN
#else
      IF(LEN_TRIM(point2%getCoordString()) /= 0) THEN
#endif
        WRITE(*,*) 'CALL point%getCoordString(...)) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL point%getCoordString(...)'
      ENDIF
!
!Test Distance
      CALL point2%init(DIM=1,X=0.5_SRK)
      CALL point3%init(DIM=1,X=0.1_SRK)
      d=Distance(point2,point3)
      IF(d /= 0.4_SRK) THEN !Check 1-D
        WRITE(*,*) '1-D Distance(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 1-D Distance(...)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL point3%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      d=Distance(point2,point3)
      IF(.NOT.(d .APPROXEQ. 0.447213595499958_SRK)) THEN !Check 2-D
        WRITE(*,*) '2-D Distance(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 2-D Distance(...)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL point3%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      d=Distance(point2,point3)
      IF(.NOT.(d .APPROXEQ. 0.748331477354788_SRK)) THEN !Check 3-D
        WRITE(*,*) '3-D Distance(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 3-D Distance(...)'
      ENDIF
      
      !Redundant call to test error check
      IF(Distance(point2,point) /= 0.0_SRK) THEN
        WRITE(*,*) 'Distance(point2,point) FAILED!'
        STOP 666
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      CALL point3%init(DIM=4,COORD=(/0.3_SRK,0.2_SRK,0.1_SRK,0.0_SRK/))
      d=Distance(point2,point3)
      IF(.NOT.(d .APPROXEQ. 1.09544511501033_SRK)) THEN !Check 3-D
        WRITE(*,*) 'N-D Distance(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: N-D Distance(...)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      d=Distance(point2,point3) !Test for empty points
!
!Test midpoint
      CALL point2%init(DIM=1,X=0.5_SRK)
      CALL point3%init(DIM=1,X=0.1_SRK)
      point=midPoint(point2,point3)
      IF(.NOT.(point%coord(1) .APPROXEQ. 0.3_SRK)) THEN
        WRITE(*,*) '1-D midPoint(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 1-D midPoint(...)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      !Test 2-D
      CALL point2%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL point3%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      point=midPoint(point2,point3)
      IF(.NOT.(point%coord(1) .APPROXEQ. 0.4_SRK) .OR. &
        .NOT.(point%coord(2) .APPROXEQ. 0.4_SRK)) THEN
        WRITE(*,*) '2-D midPoint(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 2-D midPoint(...)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      !Test 3-D
      CALL point2%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL point3%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      point=midPoint(point2,point3)
      IF(.NOT.(point%coord(1) .APPROXEQ. 0.4_SRK) .OR. &
        .NOT.(point%coord(2) .APPROXEQ. 0.4_SRK) .OR. &
          .NOT.(point%coord(3) .APPROXEQ. 0.4_SRK)) THEN
        WRITE(*,*) '3-D midPoint(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 3-D midPoint(...)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      !Test N-D
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      CALL point3%init(DIM=4,COORD=(/0.3_SRK,0.2_SRK,0.1_SRK,0.0_SRK/))
      point=midPoint(point2,point3) !Test for incorrect dimensions
      CALL point3%init(DIM=4,COORD=(/0.3_SRK,0.2_SRK,0.1_SRK,0.0_SRK/))
      point=midPoint(point2,point3)
      IF(.NOT.(point%coord(1) .APPROXEQ. 0.4_SRK) .OR. &
        .NOT.(point%coord(2) .APPROXEQ. 0.4_SRK) .OR. &
          .NOT.(point%coord(3) .APPROXEQ. 0.4_SRK) .OR. &
            .NOT.(point%coord(4) .APPROXEQ. 0.4_SRK)) THEN
        WRITE(*,*) 'N-D midPoint(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: N-D midPoint(...)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      point=midPoint(point2,point3) !Test for empty points
!
!Test Operators
      CALL point2%init(DIM=1,X=0.5_SRK)
      CALL point3%init(DIM=1,X=0.1_SRK)
      point=point2+point3
      IF(point%dim /= 1 .OR. point%coord(1) /= 0.6_SRK) THEN
        WRITE(*,*) '1-D PointType OPERATOR(+) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 1-D PointType OPERATOR(+)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL point3%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      point=point2+point3
      IF(point%dim /= 2 .OR. ANY(point%coord /= 0.8_SRK)) THEN
        WRITE(*,*) '2-D PointType OPERATOR(+) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 2-D PointType OPERATOR(+)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL point3%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      point=point2+point3
      IF(point%dim /= 3 .OR. ANY(.NOT.(point%coord .APPROXEQ. 0.8_SRK))) THEN
        WRITE(*,*) '3-D PointType OPERATOR(+) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 3-D PointType OPERATOR(+)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      CALL point3%init(DIM=4,COORD=(/0.3_SRK,0.2_SRK,0.1_SRK,0.0_SRK/))
      point=point2+point3
      IF(point%dim /= 4 .OR. ANY(.NOT.(point%coord .APPROXEQ. 0.8_SRK))) THEN
        WRITE(*,*) 'N-D PointType OPERATOR(+) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: N-D PointType OPERATOR(+)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      !Redundant calls for error checking
      point=point2+point3 !Empty case
      IF(point%dim /= 0 .OR. ALLOCATED(point%coord)) THEN
        WRITE(*,*) 'Empty PointType OPERATOR(+) FAILED!'
        STOP 666
      ENDIF
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      point2=point2+point3 !mismatched dimensions case
      IF(point2%dim /= 0 .OR. ALLOCATED(point2%coord)) THEN
        WRITE(*,*) 'Mismatched PointType OPERATOR(+) FAILED!'
        STOP 666
      ENDIF
      
      !Test subtraction
      CALL point2%init(DIM=1,X=0.5_SRK)
      CALL point3%init(DIM=1,X=0.1_SRK)
      point=point2-point3
      IF(point%dim /= 1 .OR. point%coord(1) /= 0.4_SRK) THEN
        WRITE(*,*) '1-D PointType OPERATOR(-) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 1-D PointType OPERATOR(-)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL point3%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      point=point2-point3
      IF(point%dim /= 2 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.2_SRK) .OR. &
        .NOT.(point%coord(2) .APPROXEQ. 0.4_SRK)) THEN
        WRITE(*,*) '2-D PointType OPERATOR(-) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 2-D PointType OPERATOR(-)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL point3%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      point=point2-point3
      IF(point%dim /= 3 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.2_SRK) .OR. &
        .NOT.(point%coord(2) .APPROXEQ. 0.4_SRK) .OR. &
          .NOT.(point%coord(3) .APPROXEQ. 0.6_SRK)) THEN
        WRITE(*,*) '3-D PointType OPERATOR(-) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 3-D PointType OPERATOR(-)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      CALL point3%init(DIM=4,COORD=(/0.3_SRK,0.2_SRK,0.1_SRK,0.0_SRK/))
      point=point2-point3
      IF(point%dim /= 4 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.2_SRK) .OR. &
        .NOT.(point%coord(2) .APPROXEQ. 0.4_SRK) .OR. &
          .NOT.(point%coord(3) .APPROXEQ. 0.6_SRK) .OR. &
            .NOT.(point%coord(4) .APPROXEQ. 0.8_SRK)) THEN
        WRITE(*,*) 'N-D PointType OPERATOR(-) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: N-D PointType OPERATOR(-)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      !Redundant calls for error checking
      point=point2-point3 !Empty case
      IF(point%dim /= 0 .OR. ALLOCATED(point%coord)) THEN
        WRITE(*,*) 'Empty PointType OPERATOR(-) FAILED!'
        STOP 666
      ENDIF
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      point2=point2-point3 !mismatched dimensions case
      IF(point2%dim /= 0 .OR. ALLOCATED(point2%coord)) THEN
        WRITE(*,*) 'Mismatched PointType OPERATOR(-) FAILED!'
        STOP 666
      ENDIF
      
      CALL point2%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.8_SRK/))
      CALL point3%init(DIM=4,COORD=(/0.5_SRK,0.6_SRK,0.7_SRK,0.80000000000002_SRK/))
      point=point2
      IF(.NOT.(point == point2) .OR. point == point3) THEN
        WRITE(*,*) 'PointType OPERATOR(==) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: PointType OPERATOR(==)'
      ENDIF
      IF((point /= point2) .OR. .NOT.(point /= point3)) THEN
        WRITE(*,*) 'PointType OPERATOR(/=) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: PointType OPERATOR(/=)'
      ENDIF
      point2%coord(4)=0.80000000000001_SRK
      IF(.NOT.(point .APPROXEQA. point2) .OR. (point .APPROXEQA. point3)) THEN
        WRITE(*,*) 'PointType OPERATOR(.APPROXEQ.) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: PointType OPERATOR(.APPROXEQ.)'
      ENDIF
      CALL point2%clear()
      CALL point3%clear()
      
      WRITE(*,*) '---------------------------------------------------'
      WRITE(*,*) 'TESTING POINTTYPE (arrays)'
#ifdef __GFORTRAN__
      WRITE(*,*) 'ELEMENTAL METHODS FOR NON-SCALAR BASE OBJECTS NOT YET SUPPORTED BY COMPILER'
#else
      CALL point%init(DIM=1,X=0.5_SRK)
      points=point
!      
!Test clear routine
      CALL points%clear()
      IF(ANY(points%dim /= 0) .OR. ALLOCATED(points(1)%coord) .OR. &
        ALLOCATED(points(2)%coord)) THEN
        WRITE(*,*) 'CALL point%clear() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL points%clear()'
      ENDIF
!
!Test distance routine
      CALL points(1)%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL points2(1)%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      CALL points(2)%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL points2(2)%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      s=Distance(points,points2)
      IF(.NOT.(s(1) .APPROXEQ. 0.748331477354788_SRK) .OR. &
        .NOT.(s(2) .APPROXEQ. 0.447213595499958_SRK)) THEN
        WRITE(*,*) 'Distance(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: Distance(...)'
      ENDIF
!
!Test midPoint routine
      CALL points(1)%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL points2(1)%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      CALL points(2)%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL points2(2)%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      points3=midPoint(points,points2)
      IF(ANY(.NOT.(points3(1)%coord .APPROXEQ. 0.4_SRK)) .OR. &
        ANY(.NOT.(points3(2)%coord .APPROXEQ. 0.4_SRK))) THEN
        WRITE(*,*) 'midPoint(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: midPoint(...)'
      ENDIF
#endif
!
!Test Operators
      !Addition
      CALL points(1)%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL points2(1)%init(DIM=3,X=0.3_SRK,Y=0.2_SRK,Z=0.1_SRK)
      CALL points(2)%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      CALL points2(2)%init(DIM=2,X=0.3_SRK,Y=0.2_SRK)
      points3=points+points2
      IF(ANY(.NOT.(points3(1)%coord .APPROXEQ. 0.8_SRK)) .OR. &
        ANY(.NOT.(points3(2)%coord .APPROXEQ. 0.8_SRK))) THEN
        WRITE(*,*) 'PointType Array OPERATOR(+) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: PointType Array OPERATOR(+)'
      ENDIF
      
      !Subtraction
      points3=points-points2
      IF(.NOT.(points3(1)%coord(1) .APPROXEQ. 0.2_SRK) .OR. &
        .NOT.(points3(1)%coord(2) .APPROXEQ. 0.4_SRK) .OR. &
          .NOT.(points3(1)%coord(3) .APPROXEQ. 0.6_SRK) .OR. &
            .NOT.(points3(2)%coord(1) .APPROXEQ. 0.2_SRK) .OR. &
              .NOT.(points3(2)%coord(2) .APPROXEQ. 0.4_SRK)) THEN
        WRITE(*,*) 'PointType Array OPERATOR(-) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: PointType Array OPERATOR(-)'
      ENDIF
      
      !Equal to
      points3=points
      IF(ANY(.NOT.(points == points3)) .OR. ANY(points2 == points)) THEN
        WRITE(*,*) 'PointType Array OPERATOR(==) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: PointType Array OPERATOR(==)'
      ENDIF
      
      !Not equal to
      IF(ANY(.NOT.(points2 /= points)) .OR. ANY(points /= points3)) THEN
        WRITE(*,*) 'PointType Array OPERATOR(/=) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: PointType Array OPERATOR(/=)'
      ENDIF
      
      !Approximately equal to
      points3=points
      points2=points
      points2(1)%coord(1)=0.50000000000002_SRK
      points2(2)%coord(1)=0.50000000000002_SRK
      points3(1)%coord(1)=0.50000000000001_SRK
      points3(2)%coord(1)=0.50000000000001_SRK
      IF(ANY(.NOT.(points .APPROXEQA. points3)) .OR. ANY(points2 .APPROXEQA. points)) THEN
        WRITE(*,*) 'PointType Array OPERATOR(.APPROXEQ.) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: PointType Array OPERATOR(.APPROXEQ.)'
      ENDIF
      WRITE(*,*) '---------------------------------------------------'
!
!Test linked list of points
      WRITE(*,*) 'TESTING LINKEDLISTPOINTTYPE'
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
      IF(.NOT.ASSOCIATED(thisPoint)) THEN
        WRITE(*,*) 'CALL firstPoint%insert(thisPoint) for nearby point FAILED!'
        STOP 666
      ENDIF
      thisPoint%sortval=1.5_SRK
      CALL firstPoint%insert(thisPoint)
      IF(ASSOCIATED(thisPoint) .OR. firstPoint%next%sortval /= 1.5_SRK) THEN
        WRITE(*,*) 'CALL firstPoint%insert(thisPoint) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL firstPoint%insert(thisPoint)'
      ENDIF

      CALL ClearLinkedListPointType(firstPoint)
      IF(ASSOCIATED(firstPoint)) THEN
        WRITE(*,*) 'CALL ClearLinkedListPointType(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL ClearLinkedListPointType(...)'
      ENDIF
      WRITE(*,*) '---------------------------------------------------'
    ENDSUBROUTINE TestPoints
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestLine
!The LineType constructor allocates sp and ep, care should
!be taken not to code memory leaks. This is why clearPoints() exists.
      WRITE(*,*) 'TESTING LINETYPE (scalar)'
      
      !Initialize by hand
      CALL line1%p(1)%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL line1%p(2)%init(DIM=2,X=1.0_SRK,Y=1.0_SRK)
!
!Test clear
      CALL line1%clear()
      IF(line1%p(1)%dim /= 0 .OR. line1%p(2)%dim /= 0 .OR. &
        ALLOCATED(line1%p(1)%coord) .OR. ALLOCATED(line1%p(2)%coord)) THEN
        WRITE(*,*) 'CALL line1%clear() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL line1%clear()'
      ENDIF
      
      !Redundant call to clear
      CALL line1%clear()
!      
!Test init
      CALL point2%init(DIM=3,X=0.1_SRK,Y=0.2_SRK,Z=0.3_SRK)
      CALL point3%init(DIM=3,X=0.4_SRK,Y=0.5_SRK,Z=0.6_SRK)
      CALL line1%set(point2,point3)
      IF(line1%p(1)%dim /= 3 .OR. line1%p(2)%dim /= 3 .OR. &
        line1%p(1)%coord(1) /= 0.1_SRK .OR. line1%p(1)%coord(2) /= 0.2_SRK .OR. &
          line1%p(1)%coord(3) /= 0.3_SRK .OR. line1%p(2)%coord(1) /= 0.4_SRK .OR. &
            line1%p(2)%coord(2) /= 0.5_SRK .OR. line1%p(2)%coord(3) /= 0.6_SRK) THEN
        WRITE(*,*) 'CALL line1%set(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL line1%set(...)'
      ENDIF
      CALL line1%clear()
      
      !Redundant call to test input error
      CALL line1%set(point,point2)
      IF(line1%p(1)%dim /= 0 .OR. line1%p(2)%dim /= 0 .OR. &
        ALLOCATED(line1%p(1)%coord) .OR. ALLOCATED(line1%p(2)%coord)) THEN
        WRITE(*,*) 'CALL line1%set(...) FAILED!'
        STOP 666
      ENDIF
!
!Test getDim
      CALL line1%set(point2,point3)
      IF(line1%getDim() /= 3) THEN
        WRITE(*,*) 'line1%getDim() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: line1%getDim()'
      ENDIF
      CALL line1%clear()
      
      !Redundant call to test input error
      IF(line1%getDim() /= 0) THEN
        WRITE(*,*) 'line1%getDim() FAILED!'
        STOP 666
      ENDIF
!      
!Test length
      CALL line1%set(point2,point3)
      d=line1%length()
      IF(.NOT.(d .APPROXEQ. 0.519615242270663_SRK)) THEN
        WRITE(*,*) 'line1%length() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: line1%length()'
      ENDIF
!
!Test midpoint
      point=line1%midpoint()
      IF(point%dim /= 3 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.25_SRK) .OR. &
        .NOT.(point%coord(2) .APPROXEQ. 0.35_SRK) .OR. &
          .NOT.(point%coord(3) .APPROXEQ. 0.45_SRK)) THEN
        WRITE(*,*) 'line1%midpoint() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: line1%midpoint()'
      ENDIF
!
!Test intersect
      !2D
      CALL line1%clear()
      CALL line1%p(1)%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL line1%p(2)%init(DIM=2,X=0.0_SRK,Y=1.0_SRK)
      CALL line2%p(1)%init(DIM=2,X=1.0_SRK,Y=0.0_SRK)
      CALL line2%p(2)%init(DIM=2,X=1.0_SRK,Y=1.0_SRK)
      !Overlap
      point=line1%intersectLine(line1)
      IF(point%dim /= -2) THEN
        WRITE(*,*) 'line1%intersectLine(...) FAILED!'
        STOP 666
      ENDIF
      !disjoint
      point=line1%intersectLine(line2)
      IF(point%dim /= -3) THEN
        WRITE(*,*) 'line1%intersectLine(...) FAILED!'
        STOP 666
      ENDIF
      !Normal
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/0.0_SRK,1.0_SRK/))
      CALL line2%p(1)%init(COORD=(/-0.5_SRK,0.5_SRK/))
      CALL line2%p(2)%init(COORD=(/1.0_SRK,0.5_SRK/))
      point=line1%intersectLine(line2)
      IF(point%dim /= 2 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.0_SRK) .OR. &
        .NOT.(point%coord(2) .APPROXEQ. 0.5_SRK)) THEN
        WRITE(*,*) 'line1%intersectLine(...) FAILED!'
        STOP 666
      ENDIF
      !3D
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      CALL line2%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,1.0_SRK/))
      CALL line2%p(2)%init(COORD=(/1.0_SRK,0.0_SRK,1.0_SRK/))
      !Overlap
      point=line1%intersectLine(line1)
      IF(point%dim /= -2) THEN
        WRITE(*,*) 'line1%intersectLine(...) FAILED!'
        STOP 666
      ENDIF
      !disjoint
      point=line1%intersectLine(line2)
      IF(point%dim /= -3) THEN
        WRITE(*,*) 'line1%intersectLine(...) FAILED!'
        STOP 666
      ENDIF
      !Normal
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      CALL line2%p(1)%init(COORD=(/1.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line2%p(2)%init(COORD=(/0.0_SRK,1.0_SRK,1.0_SRK/))
      point=line1%intersectLine(line2)
      IF(point%dim /= 3 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.5_SRK) .OR. &
        .NOT.(point%coord(2) .APPROXEQ. 0.5_SRK) .OR. &
          .NOT.(point%coord(3) .APPROXEQ. 0.5_SRK)) THEN
        WRITE(*,*) 'line1%intersectLine(...) FAILED!'
        STOP 666
      ELSE 
        WRITE(*,*) '  Passed: line1%intersectLine(...)'
      ENDIF

      !Redundant calls to test error checking.
      CALL line2%clear()
      point=line1%intersectLine(line2) !mismatched dimensions
      IF(point%dim /= -1) THEN
        WRITE(*,*) 'point=line1%intersectLine(line2) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      CALL line1%p(1)%init(DIM=1,X=0.5_SRK)
      CALL line1%p(2)%init(DIM=1,X=0.0_SRK)
      point=line1%intersectLine(line1)
      IF(point%dim /= -2) THEN !1-D is collinear
        WRITE(*,*) '1-D line1%intersectLine(...) FAILED!'
        STOP 666
      ENDIF
      
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/0.0_SRK,1.0_SRK/))
      CALL line2%p(1)%init(COORD=(/-0.5_SRK,2.5_SRK/))
      CALL line2%p(2)%init(COORD=(/1.0_SRK,2.5_SRK/))
      point=line1%intersectLine(line2)
      IF(point%dim /= -3) THEN
        WRITE(*,*) 'line1%intersectLine(...) FAILED!'
        STOP 666
      ENDIF
      !Test for %distance2Line(...)
      !Line dimension error check
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/0.0_SRK,1.0_SRK/))
      CALL line2%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p(2)%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      IF(dis%p(1)%dim /= -1 .AND. dis%p(2)%dim /= -1) THEN
        WRITE(*,*) 'line2%distance2Line(...) FAILED!'
        STOP 666
      ENDIF
      !Line input check
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line2%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p(2)%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      IF(dis%p(1)%dim /= -1 .AND. dis%p(2)%dim /= -1) THEN
        WRITE(*,*) 'line2%distance2Line(...) FAILED!'
        STOP 666
      ENDIF
      !Overlap
      CALL line1%clear()
      CALL dis%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line1%p(2)%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      IF(dis%p(1)%dim /= -2 .AND. dis%p(2)%dim /= -2) THEN
        WRITE(*,*) 'line2%distance2Line(...) FAILED!'
        STOP 666
      ENDIF
      !Parallel
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/1.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line2%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p(2)%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      IF(dis%p(1)%dim /= -3 .AND. dis%p(2)%dim /= -3) THEN
        WRITE(*,*) 'line2%distance2Line(...) FAILED!'
        STOP 666
      ENDIF
      !Normal with interscetion
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/0.0_SRK,0.0_SRK,1.0_SRK/))
      CALL line2%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p(2)%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      IF(dis%getDim() /= 3 .OR. mu1 /= 0._SRK .OR. mu2 /= -1._SRK &
        .OR. dis%p(1) /= line2%p(1) .OR. dis%length() > EPSREAL) THEN
        WRITE(*,*) 'line2%distance2Line(...) FAILED!'
        STOP 666
      ENDIF
      !Normal without interscetion
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/0.0_SRK,1.0_SRK,0.0_SRK/))
      CALL line2%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p(2)%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      IF(dis%getDim() /= 3 .OR. mu1 /= 0._SRK .OR. mu2 /= 0._SRK &
        .OR. dis%p(1) /= line2%p(1) .OR. dis%p(2) /= line1%p(1)) THEN
        WRITE(*,*) 'line2%distance2Line(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: line2%distangce2Line(...)'
      ENDIF
      !Test for %distance2Point(...)
      CALL line1%clear()
      CALL point%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/1.0_SRK/))
      CALL point%init(COORD=(/-0.5_SRK/))
      IF(.NOT.(line1%distance2Point(point) .APPROXEQ. 0.25_SRK)) THEN
        WRITE(*,*) '1-D line1%distance2Point(...) FAILED!'
        STOP 666
      ENDIF
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK/))
      IF(.NOT.(line1%distance2Point(point) .APPROXEQ. 0.0_SRK)) THEN
        WRITE(*,*) '1-D line1%distance2Point(...) FAILED!'
        STOP 666
      ENDIF
      CALL point%clear()
      CALL point%init(COORD=(/1.5_SRK/))
      IF(.NOT.(line1%distance2Point(point) .APPROXEQ. 0.25_SRK)) THEN
        WRITE(*,*) '1-D line1%distance2Point(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 1-D line1%distance2Point(...)'
      ENDIF
      
      CALL line1%clear()
      CALL point%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/1.0_SRK,1.0_SRK/))
      CALL point%init(COORD=(/-0.5_SRK/))
      IF(line1%distance2Point(point) /= -1.0_SRK) THEN
        WRITE(*,*) '2-D line1%distance2Point(...) FAILED!'
        STOP 666
      ENDIF
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK/))
      IF(.NOT.(line1%distance2Point(point) .APPROXEQ. 0.0_SRK)) THEN
        WRITE(*,*) '2-D line1%distance2Point(...) FAILED!'
        STOP 666
      ENDIF
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.5_SRK/))
      IF(.NOT.(line1%distance2Point(point) .APPROXEQ. 0.125_SRK)) THEN
        WRITE(*,*) '2-D line1%distance2Point(...) FAILED!'
        STOP 666
      ENDIF
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,0.5_SRK/))
      IF(.NOT.(line1%distance2Point(point) .APPROXEQ. 1.25_SRK)) THEN
        WRITE(*,*) '2-D line1%distance2Point(...) FAILED!'
        STOP 666
      ENDIF
      CALL point%clear()
      CALL point%init(COORD=(/-0.5_SRK,-1.0_SRK/))
      IF(.NOT.(line1%distance2Point(point) .APPROXEQ. 1.25_SRK)) THEN
        WRITE(*,*) '2-D line1%distance2Point(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 2-D line1%distance2Point(...)'
      ENDIF
      
      CALL line1%clear()
      CALL point%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      IF(.NOT.(line1%distance2Point(point) .APPROXEQ. 0.0_SRK)) THEN
        WRITE(*,*) '3-D line1%distance2Point(...) FAILED!'
        STOP 666
      ENDIF
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.0_SRK/))
      IF(.NOT.(line1%distance2Point(point) .APPROXEQ. 0.1666666666666667_SRK)) THEN
        WRITE(*,*) '3-D line1%distance2Point(...) FAILED!'
        STOP 666
      ENDIF
      CALL point%clear()
      CALL point%init(COORD=(/1.0_SRK,1.0_SRK,2.0_SRK/))
      IF(.NOT.(line1%distance2Point(point) .APPROXEQ. 1.0_SRK)) THEN
        WRITE(*,*) '3-D line1%distance2Point(...) FAILED!'
        STOP 666
      ENDIF
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      IF(.NOT.(line1%distance2Point(point) .APPROXEQ. 1.0_SRK)) THEN
        WRITE(*,*) '3-D line1%distance2Point(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: 3-D line1%distance2Point(...)'
      ENDIF
      
      WRITE(*,*) '---------------------------------------------------'
      WRITE(*,*) 'TESTING LINETYPE (arrays)'
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
      CALL lines%set(points2,points3)
!      CALL lines(1)%set(points2(1),points3(1))
!      CALL lines(2)%set(points2(2),points3(2))
      IF(lines(1)%p(1)%dim /= 3 .OR. lines(1)%p(2)%dim /= 3 .OR. & 
        ANY(.NOT.(lines(1)%p(1)%coord .APPROXEQ. (/0.5_SRK,0.6_SRK,0.7_SRK/))) .OR. &
        ANY(.NOT.(lines(1)%p(2)%coord .APPROXEQ. (/0.6_SRK,0.7_SRK,0.8_SRK/))) .OR. &
        lines(2)%p(1)%dim /= 2 .OR. lines(2)%p(2)%dim /= 2 .OR. &
        ANY(.NOT.(lines(2)%p(1)%coord .APPROXEQ. (/0.5_SRK,0.6_SRK/))) .OR. &
        ANY(.NOT.(lines(2)%p(2)%coord .APPROXEQ. (/0.6_SRK,0.7_SRK/)))) THEN
        WRITE(*,*) 'CALL lines%set(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL lines%set(...)'
      ENDIF
      
      ldim=lines%getDim()
      IF(ldim(1) /= 3 .OR. ldim(2) /= 2) THEN
        WRITE(*,*) 'CALL lines%getDim() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL lines%getDim()'
      ENDIF
      
      s=lines%length()
      IF(.NOT.(s(1) .APPROXEQ. 0.173205080756888_SRK) .OR. &
        .NOT.(s(2) .APPROXEQ. 0.141421356237309_SRK)) THEN
        WRITE(*,*) 'lines%length() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: lines%length()'
      ENDIF
      
      points=lines%midPoint()
      IF(ANY(.NOT.(points(1)%coord .APPROXEQ. (/0.55_SRK,0.65_SRK,0.75_SRK/))) .OR. &
        ANY(.NOT.(points(2)%coord .APPROXEQ. (/0.55_SRK,0.65_SRK/)))) THEN
        WRITE(*,*) 'lines%midPoint() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: lines%midPoint()'
      ENDIF
      
      points=lines%intersectLine(lines)
      IF(points(1)%dim /= -2 .OR. points(2)%dim /= -2) THEN
        WRITE(*,*) 'lines%intersectLine(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: lines%intersectLine(...)'
      ENDIF
      
     !diss=lines%distance2Line(lines)
     ! IF(diss(1)%p(1)%dim /= -2 .OR. diss(2)%p(1)%dim /= -2) THEN
     !   WRITE(*,*) 'lines%distance2Line(...) FAILED!'
     !   STOP 666
     ! ELSE
     !   WRITE(*,*) '  Passed: lines%distance2Line(...)'
     ! ENDIF
     !
      points=lines%midPoint()
      s=lines%distance2Point(points)
      IF(ANY(.NOT.(s .APPROXEQ. 0.0_SRK))) THEN
        WRITE(*,*) 'lines%distance2Point(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: lines%distance2Point(...)'
      ENDIF
      
      CALL lines%clear()
      IF((lines(1)%p(1)%dim /= 0) .OR. (lines(1)%p(2)%dim /= 0) .OR. &
         (lines(2)%p(1)%dim /= 0) .OR. (lines(2)%p(2)%dim /= 0) .OR. &
         ALLOCATED(lines(1)%p(1)%coord) .OR. ALLOCATED(lines(1)%p(2)%coord) .OR. &
         ALLOCATED(lines(2)%p(1)%coord) .OR. ALLOCATED(lines(2)%p(2)%coord)) THEN
        WRITE(*,*) 'lines%clear(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: lines%clear(...)'
      ENDIF
#endif
      WRITE(*,*) '---------------------------------------------------'
    ENDSUBROUTINE TestLine
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestPlane
      REAL(SRK) :: n(3)
      WRITE(*,*) 'TESTING PLANETYPE (scalar)'
!
!Test clear
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      plane1%n=(/1.0_SRK,1.0_SRK,1.0_SRK/)
      plane1%v0=point2
      CALL plane1%clear()
      IF(ANY(plane1%n /= 0.0_SRK) .OR. plane1%v0%dim /= 0 .OR. &
        ALLOCATED(plane1%v0%coord)) THEN
        WRITE(*,*) 'CALL plane1%clear() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL plane1%clear()'
      ENDIF
      n=(/1.0_SRK,1.0_SRK,1.0_SRK/)
      CALL plane1%set(n,point)
      IF(ANY(plane1%v0%coord /= 0.5_SRK) .OR. ANY(.NOT.(plane1%n .APPROXEQ. &
         (/1._SRK/SQRT(3.0_SRK),1._SRK/SQRT(3.0_SRK),1._SRK/SQRT(3.0_SRK)/)))) THEN
        WRITE(*,*) 'CALL plane1%set(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL plane1%set(...)'
      ENDIF
      
      !Test disjoint-ness
      CALL line1%clear()
      CALL line1%p(1)%init(COORD=(/0._SRK,0._SRK,0._SRK/))
      CALL line1%p(2)%init(COORD=(/0.1_SRK,0.1_SRK,0.1_SRK/))
      point2=plane1%intersectLine(line1)
      IF(point2%dim /= -3) THEN
        WRITE(*,*) 'plane%intersect(...) FAILED!'
        STOP 666
      ENDIF
      
      !Test for collinearity
      CALL line1%clear()
      CALL line1%p(1)%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      CALL line1%p(2)%init(COORD=(/0.75_SRK,0.75_SRK,0._SRK/))
      point2=plane1%intersectLine(line1)
      IF(point2%dim /= -2) THEN
        WRITE(*,*) 'plane%intersect(...) FAILED!'
        STOP 666
      ENDIF
      
      !Test for parallel
      CALL line1%clear()
      CALL line1%p(1)%init(COORD=(/0.4_SRK,0.4_SRK,0.4_SRK/))
      CALL line1%p(2)%init(COORD=(/0.65_SRK,0.65_SRK,-0.1_SRK/))
      point2=plane1%intersectLine(line1)
      IF(point2%dim /= -3) THEN
        WRITE(*,*) 'plane%intersect(...) FAILED!'
        STOP 666
      ENDIF
      
      CALL line1%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      point2=plane1%intersectLine(line1)
      IF(point2%dim /= 3 .OR. ANY(.NOT.(point2%coord .APPROXEQ. 0.5_SRK))) THEN
        WRITE(*,*) 'plane1%intersect(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: plane1%intersect(...)'
      ENDIF

      !Test for bad input
      CALL line1%clear()
      point2=plane1%intersectLine(line1)
      IF(point2%dim /= -1) THEN
        WRITE(*,*) 'plane%intersect(...) FAILED!'
        STOP 666
      ENDIF
      
      CALL line2%clear
      CALL line2%p(1)%init(COORD=(/-0.5_SRK,2.5_SRK/))
      CALL line2%p(2)%init(COORD=(/1.0_SRK,2.5_SRK/))
      point2=plane1%intersectLine(line2)
      IF(point2%dim /= -1) THEN
        WRITE(*,*) 'plane%intersect(...) FAILED!'
        STOP 666
      ENDIF
      
      CALL plane1%clear()
      point2=plane1%intersectLine(line2)
      IF(point2%dim /= -1) THEN
        WRITE(*,*) 'plane%intersect(...) FAILED!'
        STOP 666
      ENDIF
      WRITE(*,*) '---------------------------------------------------'
      WRITE(*,*) 'TESTING PLANETYPE (arrays)'
#ifdef __GFORTRAN__
      WRITE(*,*) 'ELEMENTAL METHODS FOR NON-SCALAR BASE OBJECTS NOT YET SUPPORTED BY COMPILER'
#else      
      CALL plane1%set((/1.0_SRK,1.0_SRK,1.0_SRK/),point)
      planes=plane1
      CALL line1%clear()
      CALL line1%p(1)%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p(2)%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      points=planes%intersectLine(line1)
      IF(points(1)%dim /= 3 .OR. ANY(.NOT.(points(1)%coord .APPROXEQ. 0.5_SRK)) .OR. &
        points(2)%dim /= 3 .OR. ANY(.NOT.(points(2)%coord .APPROXEQ. 0.5_SRK))) THEN
        WRITE(*,*) 'planes%intersect(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: planes%intersect(...)'
      ENDIF
      
      CALL planes%clear()
      IF(ANY(planes(1)%n /= 0.0_SRK) .OR. planes(1)%v0%dim /= 0 .OR. &
        ALLOCATED(planes(1)%v0%coord) .OR. ANY(planes(2)%n /= 0.0_SRK) .OR. &
        planes(2)%v0%dim /= 0 .OR. ALLOCATED(planes(2)%v0%coord)) THEN
        WRITE(*,*) 'CALL planes%clear() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL planes%clear()'
      ENDIF
      WRITE(*,*) '---------------------------------------------------'
#endif
    ENDSUBROUTINE TestPlane
!
!-------------------------------------------------------------------------------    
    SUBROUTINE TestCircle_and_Cylinder
      WRITE(*,*) 'TESTING CIRCLETYPE (scalar)'
!
!Test clear
      CALL point%clear()
      CALL point2%clear()
      CALL point%init(DIM=2,X=0.1_SRK,Y=0.2_SRK)
      circle1%r=0.5_SRK
      circle1%c=point
      CALL circle1%clear()
      IF(circle1%r /= 0.0_SRK .OR. circle1%c%dim /= 0 .OR. &
        ALLOCATED(circle1%c%coord)) THEN
        WRITE(*,*) 'CALL circle1%clear() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL circle1%clear()'
      ENDIF
!
!Test set
      !Error check
      CALL circle1%set(point2,0.5_SRK)
      IF(circle1%r /= 0.0_SRK .OR. circle1%c%dim /= 0 .OR. &
        ALLOCATED(circle1%c%coord)) THEN
        WRITE(*,*) 'CALL circle1%set(...) FAILED!'
        STOP 666
      ENDIF
      CALL circle1%set(point,-0.5_SRK)
      IF(circle1%r /= 0.0_SRK .OR. circle1%c%dim /= 0 .OR. &
        ALLOCATED(circle1%c%coord)) THEN
        WRITE(*,*) 'CALL circle1%set(...) FAILED!'
        STOP 666
      ENDIF
      !Real case
      CALL circle1%set(point,0.5_SRK)
      IF(circle1%r /= 0.5_SRK .OR. circle1%c%dim /= 2 .OR. &
        circle1%c%coord(1) /= 0.1_SRK .OR. circle1%c%coord(2) /= 0.2_SRK) THEN
        WRITE(*,*) 'CALL circle1%set(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL circle1%set(...)'
      ENDIF
!
!Test intersect
      CALL point%clear()
      CALL circle1%clear()
      CALL line1%clear()
      CALL circle1%intersectLine(line1,point2,point3) !Test bad input
      
      !Reference circle for other tests
      CALL point%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL circle1%set(point,0.4225_SRK)
      
      !Test disjoint (pointing away)
      CALL line1%p(1)%init(DIM=2,X=0.3_SRK,Y=0.4_SRK)
      CALL line1%p(2)%init(DIM=2,X=0.4_SRK,Y=0.5_SRK)
      CALL circle1%intersectLine(line1,point2,point3)
      IF(point2%dim /= -2 .OR. point3%dim /= -2) THEN
        WRITE(*,*) 'CALL circle1%intersectLine(...) (disjoint 1) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test disjoint (ray misses)
      CALL line1%p(1)%init(DIM=2,X=-0.4_SRK,Y=0.4_SRK)
      CALL line1%p(2)%init(DIM=2,X=-0.1_SRK,Y=0.5_SRK)
      CALL circle1%intersectLine(line1,point2,point3)
      IF(point2%dim /= -2 .OR. point3%dim /= -2) THEN
        WRITE(*,*) 'CALL circle1%intersectLine(...) (disjoint 1) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test tangent
      CALL line1%p(1)%init(DIM=2,X=0.4225_SRK,Y=0.4_SRK)
      CALL line1%p(2)%init(DIM=2,X=0.4225_SRK,Y=-0.5_SRK)
      CALL circle1%intersectLine(line1,point2,point3)
      IF(point2%dim /= -3 .OR. point3%dim /= -3) THEN
        WRITE(*,*) 'CALL circle1%intersectLine(...) (tangent) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test totally inside
      CALL line1%p(1)%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL line1%p(2)%init(DIM=2,X=0.1_SRK,Y=0.1_SRK)
      CALL circle1%intersectLine(line1,point2,point3)
      IF(point2%dim /= 0 .OR. point3%dim /= 0) THEN
        WRITE(*,*) 'CALL circle1%intersectLine(...) (inside) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test 1 point of intersection
      CALL line1%p(1)%init(DIM=2,X=-0.4_SRK,Y=-0.3_SRK)
      CALL line1%p(2)%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL circle1%intersectLine(line1,point2,point3)
      IF(ANY(.NOT.(point2%coord .APPROXEQ. (/-0.3380_SRK,-0.2535_SRK/))) &
        .OR. point3%dim /= 0) THEN
        WRITE(*,*) 'CALL circle1%intersectLine(...) (1-point) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test 2 points of intersection
      CALL line1%p(1)%init(DIM=2,X=-0.3_SRK,Y=-0.4_SRK)
      CALL line1%p(2)%init(DIM=2,X=0.4_SRK,Y=0.2_SRK)
      CALL circle1%intersectLine(line1,point2,point3)
      IF(ANY(.NOT.(point2%coord .APPROXEQ. (/-0.239446595040736_SRK,-0.348097081463488_SRK/))) &
        .OR. ANY(.NOT.(point3%coord .APPROXEQ. (/0.380623065628971_SRK,0.183391199110547_SRK/)))) THEN
        WRITE(*,*) 'CALL circle1%intersectLine(...) (2-points) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL circle1%intersectLine(...)'
      ENDIF
      WRITE(*,*) '---------------------------------------------------'
      WRITE(*,*) 'TESTING CYLINDERTYPE (scalar)'
!
!Test clear
      CALL point3%clear()
      CALL point2%clear()
      CALL point2%init(DIM=3,X=0.1_SRK,Y=0.2_SRK,Z=0.3_SRK)
      CALL point3%init(DIM=3,X=0.1_SRK,Y=0.2_SRK,Z=1.3_SRK)
      cylinder1%axis%p(1)=point2
      cylinder1%axis%p(2)=point2
      cylinder1%r=1.0_SRK
      
      CALL cylinder1%clear()
      IF(cylinder1%r /= 0.0_SRK .OR. cylinder1%axis%p(1)%dim /= 0 .OR. &
        ALLOCATED(cylinder1%axis%p(1)%coord) .OR. cylinder1%axis%p(2)%dim /= 0 .OR. &
          ALLOCATED(cylinder1%axis%p(2)%coord)) THEN
        WRITE(*,*) 'CALL cylinder1%clear() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL cylinder1%clear()'
      ENDIF
!
!Test set
      !Error check
      CALL cylinder1%set(point,point3,1.0_SRK)
      IF(cylinder1%r /= 0.0_SRK .OR. cylinder1%axis%p(1)%dim /= 0 .OR. &
        ALLOCATED(cylinder1%axis%p(1)%coord) .OR. cylinder1%axis%p(2)%dim /= 0 .OR. &
          ALLOCATED(cylinder1%axis%p(2)%coord)) THEN
        WRITE(*,*) 'CALL cylinder1%set(...) FAILED!'
        STOP 666
      ENDIF
      CALL cylinder1%set(point3,point3,1.0_SRK)
      IF(cylinder1%r /= 0.0_SRK .OR. cylinder1%axis%p(1)%dim /= 0 .OR. &
        ALLOCATED(cylinder1%axis%p(1)%coord) .OR. cylinder1%axis%p(2)%dim /= 0 .OR. &
          ALLOCATED(cylinder1%axis%p(2)%coord)) THEN
        WRITE(*,*) 'CALL cylinder1%set(...) FAILED!'
        STOP 666
      ENDIF
      CALL cylinder1%set(point2,point3,-1.0_SRK)
      IF(cylinder1%r /= 0.0_SRK .OR. cylinder1%axis%p(1)%dim /= 0 .OR. &
        ALLOCATED(cylinder1%axis%p(1)%coord) .OR. cylinder1%axis%p(2)%dim /= 0 .OR. &
          ALLOCATED(cylinder1%axis%p(2)%coord)) THEN
        WRITE(*,*) 'CALL cylinder1%set(...) FAILED!'
        STOP 666
      ENDIF
      !Real test
      CALL cylinder1%set(point2,point3,1.0_SRK)
      IF(cylinder1%r /= 1.0_SRK .OR. cylinder1%axis%p(1)%dim /= 3 .OR. &
        ANY(cylinder1%axis%p(1)%coord /= (/0.1_SRK,0.2_SRK,0.3_SRK/)) .OR. &
          cylinder1%axis%p(2)%dim /= 3 .OR. &
            ANY(cylinder1%axis%p(2)%coord /= (/0.1_SRK,0.2_SRK,1.3_SRK/))) THEN
        WRITE(*,*) 'CALL cylinder1%set(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL cylinder1%set(...)'
      ENDIF
      CALL cylinder1%clear()
!
!Test intersection
      
      !Test bad input
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(point2%dim /= -1 .OR. point3%dim /= -1) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (bad input) FAILED!'
        STOP 666
      ENDIF
      
      !Reference cylinder for all cases.
      cylinder1%r=1.0_SRK
      CALL cylinder1%axis%p(1)%init(DIM=3,X=0.1_SRK,Y=-0.2_SRK,Z=0.0_SRK)
      CALL cylinder1%axis%p(2)%init(DIM=3,X=0.1_SRK,Y=-0.2_SRK,Z=1.0_SRK)
      CALL line1%clear()
      
      !Test totally "outside" P-surface
      CALL line1%p(1)%init(DIM=3,X=2.0_SRK,Y=2.0_SRK,Z=-0.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=2.0_SRK,Y=2.0_SRK,Z=-1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(point2%dim /= -2 .OR. point3%dim /= -2) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (outside P) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test totally "outside" Q-surface
      CALL line1%p(1)%init(DIM=3,X=2.0_SRK,Y=2.0_SRK,Z=10.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=2.0_SRK,Y=2.0_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(point2%dim /= -3 .OR. point3%dim /= -3) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (outside Q) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test parallel on cylinder surface
      CALL line1%p(1)%init(DIM=3,X=0.1_SRK,Y=-1.2_SRK,Z=-0.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=0.1_SRK,Y=-1.2_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(point2%dim /= -4 .OR. point3%dim /= -4) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (parallel on surface) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test parallel outside radius
      CALL line1%p(1)%init(DIM=3,X=2.0_SRK,Y=2.0_SRK,Z=-0.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=2.0_SRK,Y=2.0_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(point2%dim /= -5 .OR. point3%dim /= -5) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (parallel outside) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test parallel totally inside
      CALL line1%p(1)%init(DIM=3,X=0.0_SRK,Y=0.2_SRK,Z=0.2_SRK)
      CALL line1%p(2)%init(DIM=3,X=0.0_SRK,Y=0.2_SRK,Z=0.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(point2%dim /= -6 .OR. point3%dim /= -6) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (parallel inside) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test parallel both intersections
      CALL line1%p(1)%init(DIM=3,X=0.0_SRK,Y=0.0_SRK,Z=-0.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=0.0_SRK,Y=0.0_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(ANY(.NOT.(point2%coord .APPROXEQ. (/0.0_SRK,0.0_SRK,0.0_SRK/))) .OR. &
        ANY(.NOT.(point3%coord .APPROXEQ. (/0.0_SRK,0.0_SRK,1.0_SRK/)))) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (parallel inside) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test not parallel and tangent
      CALL line1%p(1)%init(DIM=3,X=-1.1_SRK,Y=-1.2_SRK,Z=0.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=1.1_SRK,Y=-1.2_SRK,Z=0.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(point2%dim /= -7 .OR. point3%dim /= -7) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (disjoint) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test not parallel and disjoint
      CALL line1%p(1)%init(DIM=3,X=0.1_SRK,Y=2.0_SRK,Z=0.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=1.1_SRK,Y=2.0_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(point2%dim /= -8 .OR. point3%dim /= -8) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (disjoint) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test not parallel outside no intersection
      CALL line1%p(1)%init(DIM=3,X=0.1_SRK,Y=2.0_SRK,Z=-0.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=0.1_SRK,Y=2.5_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(point2%dim /= 0 .OR. point3%dim /= 0) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (no intersection) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test not parallel intersection with P-surface only
      CALL line1%p(1)%init(DIM=3,X=0.0_SRK,Y=0.0_SRK,Z=-0.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=0.0_SRK,Y=0.5_SRK,Z=0.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(ANY(.NOT.(point2%coord .APPROXEQ. (/0.0_SRK,0.25_SRK,0.0_SRK/))) .OR. &
        point3%dim /= 0) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (P-surface) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test not parallel intersection with Q-surface only
      CALL line1%p(1)%init(DIM=3,X=0.0_SRK,Y=0.0_SRK,Z=0.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=0.0_SRK,Y=0.5_SRK,Z=1.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(ANY(.NOT.(point3%coord .APPROXEQ. (/0.0_SRK,0.25_SRK,1.0_SRK/))) .OR. &
        point2%dim /= 0) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (Q-surface) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test not parallel intersection with both PQ-surfaces
      CALL line1%p(1)%init(DIM=3,X=0.0_SRK,Y=0.5_SRK,Z=1.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=0.0_SRK,Y=0.0_SRK,Z=-0.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(ANY(.NOT.(point3%coord .APPROXEQ. (/0.0_SRK,0.125_SRK,0.0_SRK/))) .OR. &
        ANY(.NOT.(point2%coord .APPROXEQ. (/0.0_SRK,0.375_SRK,1.0_SRK/)))) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (QP-surface) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test not parallel intersection with cylinder once
      CALL line1%p(1)%init(DIM=3,X=0.0_SRK,Y=0.0_SRK,Z=0.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=0.0_SRK,Y=1.5_SRK,Z=0.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(ANY(.NOT.(point3%coord .APPROXEQ. (/0.0_SRK,0.794987437106620_SRK,0.5_SRK/))) .OR. &
        point2%dim /= 0) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (cyl-1) FAILED!'
        STOP 666
      ENDIF
      CALL line1%clear()
      
      !Test not parallel intersection with cylinder twice
      CALL line1%p(1)%init(DIM=3,X=0.0_SRK,Y=-1.5_SRK,Z=0.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=0.0_SRK,Y=1.5_SRK,Z=0.5_SRK)
      CALL cylinder1%intersectLine(line1,point2,point3)
      IF(ANY(.NOT.(point3%coord .APPROXEQ. (/0.0_SRK,0.794987437106620_SRK,0.5_SRK/))) .OR. &
        ANY(.NOT.(point2%coord .APPROXEQ. (/0.0_SRK,-1.194987437106620_SRK,0.5_SRK/)))) THEN
        WRITE(*,*) 'CALL cylinder1%intersectLine(...) (cyl-2) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL cylinder1%intersectLine(...)'
      ENDIF
      CALL line1%clear()
      WRITE(*,*) '---------------------------------------------------'
      
#ifdef __GFORTRAN__
      WRITE(*,*) 'ELEMENTAL METHODS FOR NON-SCALAR BASE OBJECTS NOT YET SUPPORTED BY COMPILER'
#else
      WRITE(*,*) 'TESTING CIRCLETYPE (arrays)'
!
!Test clear
      CALL point%clear()
      CALL point2%clear()
      CALL point%init(DIM=2,X=0.1_SRK,Y=0.2_SRK)
      circles(1)%r=0.5_SRK
      circles(1)%c=point
      circles(2)%r=0.5_SRK
      circles(2)%c=point
      CALL circles%clear()
      IF(circles(1)%r /= 0.0_SRK .OR. circles(1)%c%dim /= 0 .OR. &
        ALLOCATED(circles(1)%c%coord) .OR. circles(2)%r /= 0.0_SRK .OR. &
          circles(2)%c%dim /= 0 .OR. ALLOCATED(circles(2)%c%coord)) THEN
        WRITE(*,*) 'CALL circles%clear() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL circles%clear()'
      ENDIF
!
!Test set
      CALL point%init(DIM=2,X=0.1_SRK,Y=0.2_SRK)
!This no longer works with Intel 12
      CALL circles%set(point,(/0.5_SRK,0.6_SRK/))
!      CALL circles(1)%set(point,0.5_SRK)
!      CALL circles(2)%set(point,0.6_SRK)
      IF(circles(1)%r /= 0.5_SRK .OR. circles(2)%r /= 0.6_SRK .OR. &
        ANY(circles(1)%c%coord /= (/0.1_SRK,0.2_SRK/)) .OR. &
          ANY(circles(2)%c%coord /= (/0.1_SRK,0.2_SRK/)) .OR. &
            circles(1)%c%dim /= 2 .OR. circles(2)%c%dim /= 2) THEN
        WRITE(*,*) 'CALL circles%set(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL circles%set(...)'
      ENDIF
!
!Test intersection
      CALL line1%clear()
      CALL line1%p(1)%init(DIM=2,X=-1.0_SRK,Y=-1.0_SRK)
      CALL line1%p(2)%init(DIM=2,X=1.0_SRK,Y=1.0_SRK)
      CALL circles%intersectLine(line1,points,points2)
      IF(ANY(.NOT.(points(1)%coord .APPROXEQ. -0.2_SRK)) .OR. &
        ANY(.NOT.(points2(1)%coord .APPROXEQ. 0.5_SRK)) .OR. &
        ANY(.NOT.(points(2)%coord .APPROXEQ. -0.271307488658817_SRK)) .OR. &
        ANY(.NOT.(points2(2)%coord .APPROXEQ. 0.571307488658817_SRK))) THEN
        WRITE(*,*) 'CALL circles%intersectLine(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL circles%intersectLine(...)'
      ENDIF
      
      WRITE(*,*) '---------------------------------------------------'
      
      WRITE(*,*) 'TESTING CYLINDERTYPE (arrays)'
!
!Test Clear
      CALL point3%clear()
      CALL point2%clear()
      CALL point2%init(DIM=3,X=0.1_SRK,Y=-0.2_SRK,Z=0.0_SRK)
      CALL point3%init(DIM=3,X=0.1_SRK,Y=-0.2_SRK,Z=1.0_SRK)
      cylinders(1)%axis%p(1)=point2
      cylinders(1)%axis%p(2)=point2
      cylinders(1)%r=1.0_SRK
      cylinders(2)=cylinders(1)
      
      CALL cylinders%clear()
      IF(cylinders(1)%r /= 0.0_SRK .OR. cylinders(1)%axis%p(1)%dim /= 0 .OR. &
        ALLOCATED(cylinders(1)%axis%p(1)%coord) .OR. cylinders(1)%axis%p(2)%dim /= 0 .OR. &
          ALLOCATED(cylinders(1)%axis%p(2)%coord) .OR. cylinders(2)%r /= 0.0_SRK .OR. &
           cylinders(2)%axis%p(1)%dim /= 0 .OR. ALLOCATED(cylinders(2)%axis%p(1)%coord) .OR. &
             cylinders(2)%axis%p(2)%dim /= 0 .OR. ALLOCATED(cylinders(2)%axis%p(2)%coord)) THEN
        WRITE(*,*) 'CALL cylinders%clear() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL cylinders%clear()'
      ENDIF
!
!Test set
!This no longer works with Intel 12
      CALL cylinders%set(point2,point3,(/1.0_SRK,1.5_SRK/))
!      CALL cylinders(1)%set(point2,point3,1.0_SRK)
!      CALL cylinders(2)%set(point2,point3,1.5_SRK)
      IF(cylinders(1)%r /= 1.0_SRK .OR. cylinders(1)%axis%p(1)%dim /= 3 .OR. &
        ANY(cylinders(1)%axis%p(1)%coord /= (/0.1_SRK,-0.2_SRK,0.0_SRK/)) .OR. &
          cylinders(1)%axis%p(2)%dim /= 3 .OR. &
            ANY(cylinders(1)%axis%p(2)%coord /= (/0.1_SRK,-0.2_SRK,1.0_SRK/)) .OR. &
         cylinders(2)%r /= 1.5_SRK .OR. cylinders(2)%axis%p(1)%dim /= 3 .OR. &
        ANY(cylinders(2)%axis%p(1)%coord /= (/0.1_SRK,-0.2_SRK,0.0_SRK/)) .OR. &
          cylinders(2)%axis%p(2)%dim /= 3 .OR. &
            ANY(cylinders(2)%axis%p(2)%coord /= (/0.1_SRK,-0.2_SRK,1.0_SRK/))) THEN
        WRITE(*,*) 'CALL cylinders%set(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL cylinders%set(...)'
      ENDIF
!
!Test intersectLine
      CALL line1%clear()
      CALL line1%p(1)%init(DIM=3,X=0.0_SRK,Y=-2.5_SRK,Z=0.5_SRK)
      CALL line1%p(2)%init(DIM=3,X=0.0_SRK,Y=2.5_SRK,Z=0.5_SRK)
      CALL cylinders%intersectLine(line1,points2,points3)
      IF(ANY(.NOT.(points3(1)%coord .APPROXEQ. (/0.0_SRK,0.794987437106620_SRK,0.5_SRK/))) .OR. &
        ANY(.NOT.(points2(1)%coord .APPROXEQ. (/0.0_SRK,-1.194987437106620_SRK,0.5_SRK/))) .OR. &
        ANY(.NOT.(points3(2)%coord .APPROXEQ. (/0.0_SRK,1.296662954709577_SRK,0.5_SRK/))) .OR. &
        ANY(.NOT.(points2(2)%coord .APPROXEQ. (/0.0_SRK,-1.696662954709577_SRK,0.5_SRK/)))) THEN
        WRITE(*,*) 'CALL cylinders%intersectLine(...) FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL cylinders%intersectLine(...)'
      ENDIF
#endif
    ENDSUBROUTINE TestCircle_and_Cylinder
!
!-------------------------------------------------------------------------------    
    SUBROUTINE TestBox
      WRITE(*,*) '---------------------------------------------------'
      WRITE(*,*) 'TESTING OBBTYPE (scalar)'
    !Test for clear
      CALL point%init(DIM=3,X=0.1_SRK,Y=0.2_SRK,Z=0.3_SRK)
      box%p0=point
!      ALLOCATE(box%u(3,3),box%e(3))
      box%u(1,:)=(/0.5_SRK,0.5_SRK,0._SRK/)
      box%u(2,:)=(/-0.5_SRK,0.5_SRK,0._SRK/)
      box%u(3,:)=(/0._SRK,0._SRK,1._SRK/)
      box%e=1._SRK
      CALL box%clear()
      
      IF(ANY(box%u /= 0.0_SRK) .OR. ANY(box%e /= 0.0_SRK) &
        .OR. box%p0%dim /= 0 .OR. ALLOCATED(box%p0%coord) ) THEN
        WRITE(*,*) 'CALL box%clear() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL box%clear()'
      ENDIF
    !Test for set
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
      IF(box%p0 /= point .OR. ANY(box%e(1:2) /= e_2d) &
        .OR. .NOT.(ALL(box%u(1:2,1) .APPROXEQ. u1_2d/SQRT(2._SRK))) &
          .OR. .NOT.(ALL(box%u(1:2,2) .APPROXEQ. u2_2d/SQRT(2._SRK)))) THEN
        WRITE(*,*) 'CALL box%set(...) FAILED!'
        STOP 666
      ENDIF
      !Test for set 3D
      CALL point%clear()
      CALL box%clear()
      CALL point%init(DIM=3,X=1._SRK,Y=1._SRK,Z=1._SRK)
      e_3d=(/SQRT(2._SRK),2*SQRT(2._SRK),1._SRK/)
      u1_3d=(/1._SRK,1._SRK,0._SRK/)
      u2_3d=(/-1._SRK,1._SRK,0._SRK/)
      u3_3d=(/0._SRK,0._SRK,2._SRK/)
      CALL box%set(point,e_3d,u1_3d,u2_3d,u3_3d)
      IF(box%p0 /= point .OR. ANY(box%e /= e_3d) &
        .OR. ANY(.NOT.(box%u(:,1) .APPROXEQ. u1_3d/SQRT(2._SRK))) &
          .OR. ANY(.NOT.(box%u(:,2) .APPROXEQ. u2_3d/SQRT(2._SRK))) &
            .OR. ANY(.NOT.(box%u(:,3) .APPROXEQ. u3_3d*0.5_SRK)) ) THEN
        WRITE(*,*) 'CALL box%set(...) FAILED!'
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: Call box%set()'
    !Test for intersection
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
      IF(ANY(points2(1)%coord /= (/0._SRK,2._SRK/)) &
        .OR. ANY(points2(2)%coord /= (/0._SRK,4._SRK/))) THEN
        WRITE(*,*) 'CALL box%intersectLine(...) FAILED! 2D'
        STOP 666
      ENDIF
      !Parallel
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/1._SRK,1._SRK/))
      CALL points(2)%init(COORD=(/2._SRK,2._SRK/))
      CALL line1%set(points(1),points(2))
      
      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      IF(ANY(points2(1)%coord /= (/1._SRK,1._SRK/)) &
        .OR. ANY(points2(2)%coord /= (/2._SRK,2._SRK/))) THEN
        WRITE(*,*) 'CALL box%intersectLine(...) FAILED! 2D'
        STOP 666
      ENDIF

      !one point: overlap
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/2._SRK,0._SRK/))
      CALL points(2)%init(COORD=(/2._SRK,2._SRK/))
      CALL line1%set(points(1),points(2))
      
      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      IF(.NOT.(ANY(points2(1)%coord .APPROXEQ. (/2._SRK,2._SRK/))) &
        .OR. .NOT.(ANY(points2(2)%coord .APPROXEQ. (/2._SRK,2._SRK/)))) THEN
        WRITE(*,*) 'CALL box%intersectLine(...) FAILED! 2D'
        STOP 666
      ENDIF
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
      IF(points2(1)%dim /= -3 .OR. points2(2)%dim /= -3) THEN
        WRITE(*,*) 'CALL box%intersectLine(...) FAILED! 2D'
        STOP 666
      ENDIF
      
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
      IF(ANY(.NOT.(points2(2)%coord .APPROXEQ. (/0._SRK,2._SRK,1._SRK/))) &
        .OR. ANY(.NOT.(points2(1)%coord .APPROXEQ. (/0._SRK,4._SRK,1._SRK/)))) THEN
        WRITE(*,*) 'CALL box%intersectLine(...) FAILED! 3D'
        STOP 666
      ENDIF
      
      !Parallel
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/1._SRK,1._SRK,1._SRK/))
      CALL points(2)%init(COORD=(/2._SRK,2._SRK,1._SRK/))
      CALL line1%set(points(1),points(2))
      
      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      IF(ANY(points2(1)%coord /= (/1._SRK,1._SRK,1._SRK/)) &
        .OR. ANY(points2(2)%coord /= (/2._SRK,2._SRK,1._SRK/))) THEN
        WRITE(*,*) 'CALL box%intersectLine(...) FAILED! 3D'
        STOP 666
      ENDIF

      !one point: overlap
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/2._SRK,0._SRK,1._SRK/))
      CALL points(2)%init(COORD=(/2._SRK,2._SRK,1._SRK/))
      CALL line1%set(points(1),points(2))
      
      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      IF(.NOT.(ANY(points2(1)%coord .APPROXEQ. (/2._SRK,2._SRK,1._SRK/))) &
        .OR. .NOT.(ANY(points2(2)%coord .APPROXEQ. (/2._SRK,2._SRK,1._SRK/)))) THEN
        WRITE(*,*) 'CALL box%intersectLine(...) FAILED! 3D'
        STOP 666
      ENDIF
        
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
      IF(.NOT.(ANY(points2(1)%coord .APPROXEQ. (/0._SRK,3._SRK,0._SRK/))) &
        .OR. .NOT.(ANY(points2(2)%coord .APPROXEQ. (/0._SRK,3._SRK,0._SRK/)))) THEN
        WRITE(*,*) 'CALL box%intersectLine(...) FAILED! 3D'
        STOP 666
      ENDIF
      
      !No point
      CALL points(1)%clear()
      CALL points(2)%clear()
      CALL points(1)%init(COORD=(/0._SRK,1._SRK,0._SRK/))
      CALL points(2)%init(COORD=(/0._SRK,1._SRK,1._SRK/))
      CALL line1%set(points(1),points(2))
      
      CALL points2(1)%clear()
      CALL points2(2)%clear()
      CALL box%intersectLine(line1,points2(1),points2(2))
      IF(points2(1)%dim /= -3 .AND. points2(2)%dim /= -3) THEN
        WRITE(*,*) 'CALL box%intersectLine(...) FAILED! 3D'
        STOP 666
      ENDIF
      WRITE(*,*) '  Passed: CALL box%intersectLine()'

      WRITE(*,*) '---------------------------------------------------'
      WRITE(*,*) 'TESTING BOXTYPE (arrays)'
!
#ifdef __GFORTRAN__
      WRITE(*,*) 'ELEMENTAL METHODS FOR NON-SCALAR BASE OBJECTS NOT YET SUPPORTED BY COMPILER'
#else
    !Test for clear
      CALL boxs(1)%clear()
      CALL boxs(2)%clear()
      CALL point%clear()
      CALL point%init(DIM=2,X=1._SRK,Y=1._SRK)
      u1_2d=(/1._SRK,1._SRK/)
      u2_2d=(/-1._SRK,1._SRK/)
      CALL boxs(1)%set(point,(/SQRT(2._SRK),8._SRK/),u1_2d,u2_2d)
      boxs(2)=boxs(1)
      CALL boxs%clear()
      
      IF(ANY(boxs(1)%u /= 0.0_SRK).OR. ANY(boxs(1)%e /= 0.0_SRK) &
        .OR. boxs(1)%p0%dim /= 0 .OR. ALLOCATED(boxs(1)%p0%coord) &
        .OR. ANY(boxs(2)%u /= 0.0_SRK).OR. ANY(boxs(2)%e /= 0.0_SRK) &
        .OR. boxs(2)%p0%dim /= 0 .OR. ALLOCATED(boxs(2)%p0%coord)) THEN
        WRITE(*,*) 'CALL box%clear() FAILED!'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL box%clear()'
      ENDIF
    !Test for intersection
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
      IF(.NOT.(ANY(points2(1)%coord .APPROXEQ. (/0._SRK,2._SRK/))) &
        .OR. .NOT.(ANY(points3(1)%coord .APPROXEQ. (/0._SRK,4._SRK/))) &
        .OR. .NOT.(ANY(points2(2)%coord .APPROXEQ. (/0._SRK,2._SRK/))) &
        .OR. .NOT.(ANY(points3(2)%coord .APPROXEQ. (/0._SRK,4._SRK/)))) THEN
        WRITE(*,*) 'CALL box%intersectLine(...) FAILED! 2D'
        STOP 666
      ELSE
        WRITE(*,*) '  Passed: CALL box%intersectLine()'
      ENDIF
#endif
    ENDSUBROUTINE

ENDPROGRAM testGeom
