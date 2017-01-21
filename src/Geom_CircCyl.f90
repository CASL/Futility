!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module defining "circles" and "cylinders" for use in
!> geometry.
!>
!> This module provides a derived data types for a "circle" which must exist in
!> 2-D space and a "cylinder" which must exist in 3-D space. The circle is
!> defined by a point for the center of rotation and a radius. The cylinder is
!> defined by two points for the central axis of rotation and its total extent
!> (if it is to be finite) and a radius. The cylinder is assumed to be a right
!> cylinder. The module also provides methods for constructing and destructing
!> these types and routines for intersecting lines with their bodies.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref Geom_Points "Geom_Points": @copybrief Geom_Points
!>  - @ref Geom_Line "Geom_Line": @copybrief Geom_Line
!>
!> @author Brendan Kochunas
!>    @date 11/02/2011
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_CircCyl
  USE IntrType
  USE Constants_Conversion
  USE ExtendedMath
  USE Geom_Points
  USE Geom_Line

  IMPLICIT NONE
  PRIVATE !Default contents of module to private
!
! List of Public items
  PUBLIC :: CircleType
  PUBLIC :: CylinderType
  PUBLIC :: OPERATOR(==)

  !> @brief Type for a circle
  TYPE :: CircleType
    !> The point that defines the center of rotation of the circle.
    TYPE(PointType) :: c
    !> The radius of the circle
    REAL(SRK) :: r=0.0_SRK
    !> The starting theta angle
    REAL(SRK) :: thetastt=0.0_SRK
    !> The stopping theta angle
    REAL(SRK) :: thetastp=0.0_SRK
!
!List of type bound procedures
    CONTAINS
      !> @copybrief Geom_CircCyl::set_CircleType
      !> @copydetails Geom_CircCyl::set_CircleType
      PROCEDURE,PASS :: set => set_CircleType
      !> @copybrief Geom_CircCyl::clear_CircleType
      !> @copydetails Geom_CircCyl::clear_CircleType
      PROCEDURE,PASS :: clear => clear_CircleType
      !> @copybrief Geom_CircCyl::intersect_CircleType_and_LineType
      !> @copydetails Geom_CircCyl::intersect_CircleType_and_LineType
      PROCEDURE,PASS :: intersectLine => intersect_CircleType_and_LineType
      !> @copybrief Geom_CircCyl::intersect_CircleType_and_CircleType
      !> @copydetails Geom_CircCyl::intersect_CircleType_and_CircleType
      PROCEDURE,PASS :: intersectCircle => intersect_CircleType_and_CircleType
      !> @copybrief Geom_CircCyl::intersect_ArcCircleType_and_LineType
      !> @copydetails Geom_CircCyl::intersect_ArcCircleType_and_LineType
      PROCEDURE,PASS :: intersectArcLine => intersect_ArcCircleType_and_LineType
      !> @copybrief Geom_CircCyl::inside_CircleType
      !> @copydetails Geom_CircCyl::inside_CircleType
      PROCEDURE,PASS :: inside => inside_CircleType
      !> @copybrief Geom_CircCyl::onSurface_CircleType
      !> @copydetails Geom_CircCyl::onSurface_CircleType
      PROCEDURE,PASS :: onSurface => onSurface_CircleType
  ENDTYPE CircleType

  !> @brief Type for a cylinder
  TYPE :: CylinderType
    !> The line segment that defines the center of rotation of the cylinder and
    !> its extents (if finite).
    TYPE(LineType) :: axis
    !> The radius of the circle
    REAL(SRK) :: r=0.0_SRK
    !> The starting theta angle
    REAL(SRK) :: thetastt=0.0_SRK
    !> The stopping theta angle
    REAL(SRK) :: thetastp=0.0_SRK
!
!List of type bound procedures
    CONTAINS
      !> @copybrief Geom_CircCyl::set_CylinderType
      !> @copydetails Geom_CircCyl::set_CylinderType
      PROCEDURE,PASS :: set => set_CylinderType
      !> @copybrief Geom_CircCyl::clear_CylinderType
      !> @copydetails Geom_CircCyl::clear_CylinderType
      PROCEDURE,PASS :: clear => clear_CylinderType
      !> @copybrief Geom_CircCyl::intersect_CylinderType_and_LineType
      !> @copydetails Geom_CircCyl::intersect_CylinderType_and_LineType
      PROCEDURE,PASS :: intersectLine => intersect_CylinderType_and_LineType
  ENDTYPE CylinderType

  !> @brief Generic interface for 'is equal to' operator (==)
  !>
  !> Adds 'is equal to' capability for Circle types
  INTERFACE OPERATOR(==)
    !> @copybrief Geom_CircCyl::isequal_CircleType
    !> @copydetails Geom_CircCyl::isequal_CircleType
    MODULE PROCEDURE isequal_CircleType
    !> @copybrief Geom_CircCyl::isequal_CylinderType
    !> @copydetails Geom_CircCyl::isequal_CylinderType
    MODULE PROCEDURE isequal_CylinderType
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Sets the values for the CircleType object attributes
!> @param circ the CircleType object to set
!> @param p a point for the center of rotation (must be 2-D)
!> @param r the radius of the circle
    ELEMENTAL SUBROUTINE set_CircleType(circ,p,r,angstt,angstp)
      CLASS(CircleType),INTENT(INOUT) :: circ
      TYPE(PointType),INTENT(IN) :: p
      REAL(SRK),INTENT(IN) :: r
      REAL(SRK),INTENT(IN),OPTIONAL :: angstt
      REAL(SRK),INTENT(IN),OPTIONAL :: angstp
      REAL(SRK) :: dtheta
      CALL circ%clear()
      IF(p%dim == 2 .AND. r > ZERO) THEN
        circ%c=p
        circ%r=r
        circ%thetastt=ZERO
        circ%thetastp=TWOPI
        IF(PRESENT(angstt)) circ%thetastt=angstt
        IF(PRESENT(angstp)) circ%thetastp=angstp
        dtheta=circ%thetastp-circ%thetastt
        IF(dtheta .APPROXGE. TWOPI) THEN
          circ%thetastt=ZERO
          circ%thetastp=TWOPI
        ENDIF
        !circ%cosstt=COS(circ%thetastt)
        !circ%sinstt=SIN(circ%thetastt)
        !circ%cosstp=COS(circ%thetastp)
        !circ%sinstp=SIN(circ%thetastp)
      ENDIF
    ENDSUBROUTINE set_CircleType
!
!-------------------------------------------------------------------------------
!> @brief Clears a CircleType object's attributes
!> @param circ the CircleType object to clear
    ELEMENTAL SUBROUTINE clear_CircleType(circ)
      CLASS(CircleType),INTENT(INOUT) :: circ
      CALL circ%c%clear()
      circ%r=ZERO
      circ%thetastt=ZERO
      circ%thetastp=ZERO
    ENDSUBROUTINE clear_CircleType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values for the CylinderType object attributes
!> @param cyl the CylinderType object to set
!> @param p a point for the "bottom" of the central axis of rotation
!>        (must be 3-D)
!> @param q a point for the "top" of the central axis of rotation (must be 3-D)
!> @param r the radius of the cylinder
    ELEMENTAL SUBROUTINE set_CylinderType(cyl,p,q,r,angstt,angstp)
      CLASS(CylinderType),INTENT(INOUT) :: cyl
      TYPE(PointType),INTENT(IN) :: p
      TYPE(PointType),INTENT(IN) :: q
      REAL(SRK),INTENT(IN) :: r
      REAL(SRK),INTENT(IN),OPTIONAL :: angstt
      REAL(SRK),INTENT(IN),OPTIONAL :: angstp
      REAL(SRK) :: dtheta
      CALL cyl%clear()
      IF(p%dim == 3 .AND. .NOT.(p .APPROXEQA. q)  .AND. r > ZERO) THEN
        CALL cyl%axis%set(p,q)
        cyl%r=r
        cyl%thetastt=ZERO
        cyl%thetastp=TWOPI
        IF(PRESENT(angstt)) cyl%thetastt=angstt
        IF(PRESENT(angstp)) cyl%thetastp=angstp
        dtheta=cyl%thetastp-cyl%thetastt
        IF(dtheta .APPROXGE. TWOPI) THEN
          cyl%thetastt=ZERO
          cyl%thetastp=TWOPI
        ENDIF
      ENDIF
    ENDSUBROUTINE set_CylinderType
!
!-------------------------------------------------------------------------------
!> @brief Clears a CylinderType object's attributes
!> @param circ the CylinderType object to clear
    ELEMENTAL SUBROUTINE clear_CylinderType(cyl)
      CLASS(CylinderType),INTENT(INOUT) :: cyl
      CALL cyl%axis%clear()
      cyl%r=ZERO
      cyl%thetastt=ZERO
      cyl%thetastp=ZERO
    ENDSUBROUTINE clear_CylinderType
!
!-------------------------------------------------------------------------------
!> @brief Determines point(s) of intersection between a line and circle (if any)
!> @param circle the circle type to test for intersection
!> @param line the line type thats being tested against the circle
!> @param p1 the first point of intersection (if it exists)
!> @param p2 the second point of intersection (if it exists)
!> @note a return code is assigned to @c p1%dim and @c p2%dim indicating the type of
!> intersection. @n
!>  > 0: success; an intersection point was found @n
!> == 0: no intersection was found (intersection outside segment) @n
!>   -1: problem with dummy arguments passed to routine @n
!>   -2: line is not directed toward circle (disjoint) @n
!>   -3: line segment is tangent @n
    ELEMENTAL SUBROUTINE intersect_CircleType_and_LineType(circle,line,p1,p2)
      CLASS(CircleType),INTENT(IN) :: circle
      TYPE(LineType),INTENT(IN) :: line
      TYPE(PointType),INTENT(INOUT) :: p1
      TYPE(PointType),INTENT(INOUT) :: p2
      REAL(SRK) :: a,b,c,t1,t2,u(2),w(2),ra,discr

      CALL p1%clear()
      CALL p2%clear()
      p1%dim=-1
      p2%dim=-1
      IF(circle%c%dim == 2 .AND. line%p1%dim == 2 .AND. &
        line%p2%dim == 2 .AND. circle%r > 0.0_SRK) THEN
        u(1)=line%p2%coord(1)-line%p1%coord(1)  !dx
        u(2)=line%p2%coord(2)-line%p1%coord(2)  !dy
        w(1)=line%p1%coord(1)-circle%c%coord(1)
        w(2)=line%p1%coord(2)-circle%c%coord(2)
        b=w(1)*u(1)+w(2)*u(2)
        c=w(1)*w(1)+w(2)*w(2)-circle%r*circle%r
        IF(c > zero .AND. b > zero) THEN
          p1%dim=-2
          p2%dim=-2
        ELSE
          a=u(1)*u(1)+u(2)*u(2) !dr^2
          discr=b*b-a*c
          IF(discr < -EPSREAL) THEN
            !Disjoint
            p1%dim=-2
            p2%dim=-2
          ELSEIF(discr .APPROXEQA. zero) THEN
            !Tangent
            ra=one/a
            t1=-ra*b
            p1=line%p1
            p1%coord(1)=p1%coord(1)+u(1)*t1
            p1%coord(2)=p1%coord(2)+u(2)*t1
            p1%dim=-3
            p2%dim=-3
          ELSE
            p1%dim=0
            p2%dim=0
            ra=one/a
            discr=SQRT(discr)
            t1=(-b-discr)*ra
            t2=(-b+discr)*ra
            IF(ZERO < t1 .AND. t1 < ONE) THEN
              p1=line%p1
              p1%coord(1)=p1%coord(1)+u(1)*t1
              p1%coord(2)=p1%coord(2)+u(2)*t1
            ENDIF
            IF(ZERO < t2 .AND. t2 < ONE) THEN
              p2=line%p1
              p2%coord(1)=p2%coord(1)+u(1)*t2
              p2%coord(2)=p2%coord(2)+u(2)*t2
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE intersect_CircleType_and_LineType
!
!-------------------------------------------------------------------------------
!> @brief Determines point(s) of intersection between two circles (if any)
!> @param c1 first circle type to test for intersection
!> @param c2 second circle type to test for intersection
!> @param p1 first point of intersection if it exists
!> @param p2 second point of intersection if it exists
!> @note a return code is assigned to @c p1%dim and @c p2%dim indicating the type of
!> intersection. @n
!> ==  2: success; Two points of intersection  found @n
!> == -1: uninitialized circle
!> == -2: no intersection points found (disjoint) @n
!> == -3: The circles are tangent @n
!> == -4: The circles are overlapped @n
    ELEMENTAL SUBROUTINE intersect_CircleType_and_CircleType(c1,c2,p1,p2)
      CLASS(CircleType),INTENT(IN)  :: c1,c2
      TYPE(PointType),INTENT(INOUT) :: p1,p2
      TYPE(PointType)               :: v1
      REAL(SRK)                     :: d,s,m
      REAL(SRK)                     :: dx,dy,dx2,dy2,dr2
      REAL(SRK)                     :: t1,t2,a,b,c

      CALL p1%clear()
      CALL p1%clear()
      p1%dim=-1
      p2%dim=-1

      IF(c1%c%dim==2 .AND. c2%c%dim==2 .AND. &
        c1%r > 0.0_SRK .AND. c2%r > 0.0_SRK) THEN
        d=Distance(c1%c,c2%c)
        s=c1%r+c2%r
        IF(d > s) THEN
          p1%dim=-2
          p2%dim=-2
        ELSEIF(d .APPROXEQ. s) THEN
          !Tangent
          v1=c2%c-c1%c
          m=SQRT(v1%coord(1)*v1%coord(1)+v1%coord(2)*v1%coord(2))
          v1%coord(1)=v1%coord(1)/m
          v1%coord(2)=v1%coord(2)/m
          CALL p1%init(DIM=2,X=ZERO,Y=ZERO)
          p1%coord(1)=c1%c%coord(1)+v1%coord(1)*c1%r
          p1%coord(2)=c1%c%coord(2)+v1%coord(2)*c1%r
          p1%dim=-3
          p2=p1
        ELSE
          IF(c1%c == c2%c) THEN
            p1%dim=-4
            p2%dim=-4
          ELSE
            dx=c2%c%coord(1)-c1%c%coord(1)
            dy=c2%c%coord(2)-c1%c%coord(2)
            dr2=(c1%r*c1%r-c2%r*c2%r)
            dx2=(c1%c%coord(1)*c1%c%coord(1)-c2%c%coord(1)*c2%c%coord(1))
            dy2=(c1%c%coord(2)*c1%c%coord(2)-c2%c%coord(2)*c2%c%coord(2))
            IF(c1%c%coord(1) == c2%c%coord(1)) THEN
              t1=(dr2-dx2-dy2)/(2.0_SRK*dy)
              t2=dx/dy
              !Set up quadratic equation
              a=(1.0_SRK+t2*t2)
              b=(2.0_SRK*t2*c1%c%coord(2)-2.0_SRK*c1%c%coord(1)-2.0_SRK*t1*t2)
              c=(t1*t1+c1%c%coord(1)*c1%c%coord(1)+c1%c%coord(2)*c1%c%coord(2))
              c=c-2.0_SRK*t1*c1%c%coord(2)-c1%r*c1%r
              IF(4.0_SRK*a*c > b*b) THEN
                p1%dim=-4
                p2%dim=-4
              ELSE
                CALL p1%init(DIM=2,X=ZERO,Y=ZERO)
                p2=p1
                p1%coord(1)=(-b+SQRT(b*b-4.0_SRK*a*c))/(2.0_SRK*a)
                p1%coord(2)=t1-p1%coord(1)*t2
                p2%coord(1)=(-b-SQRT(b*b-4.0_SRK*a*c))/(2.0_SRK*a)
                p2%coord(2)=t1-p2%coord(1)*t2
              ENDIF
            ELSE
              t1=(dr2-dx2-dy2)/(2.0_SRK*dx)
              t2=dy/dx

              !Set up quadratic equation
              a=(1.0_SRK+t2*t2)
              b=(2.0_SRK*t2*c1%c%coord(1)-2.0_SRK*c1%c%coord(2)-2.0_SRK*t1*t2)
              c=(t1*t1+c1%c%coord(1)*c1%c%coord(1)+c1%c%coord(2)*c1%c%coord(2))
              c=c-2.0_SRK*t1*c1%c%coord(1)-c1%r*c1%r
              IF(4.0_SRK*a*c > b*b) THEN
                p1%dim=-4
                p2%dim=-4
              ELSE
                CALL p1%init(DIM=2,X=ZERO,Y=ZERO)
                p2=p1
                p1%coord(2)=(-b+SQRT(b*b-4.0_SRK*a*c))/(2.0_SRK*a)
                p1%coord(1)=t1-p1%coord(2)*t2
                p2%coord(2)=(-b-SQRT(b*b-4.0_SRK*a*c))/(2.0_SRK*a)
                p2%coord(1)=t1-p2%coord(2)*t2
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE intersect_CircleType_and_CircleType
!
!-------------------------------------------------------------------------------
!> @brief Determines point(s) of intersection between a line and circle (if any)
!> @param circle the circle type to test for intersection
!> @param line the line type thats being tested against the circle
!> @param p1 the first point of intersection (if it exists)
!> @param p2 the second point of intersection (if it exists)
!> @note a return code is assigned to @c p1%dim and @c p2%dim indicating the type of
!> intersection. @n
!>  > 0: success; an intersection point was found @n
!> == 0: no intersection was found (intersection outside segment) @n
!>   -1: problem with dummy arguments passed to routine @n
!>   -2: line is not directed toward circle (disjoint) @n
!>   -3: line segment is tangent @n
    ELEMENTAL SUBROUTINE intersect_ArcCircleType_and_LineType(circle,line,p1,p2,p3,p4)
      CLASS(CircleType),INTENT(IN) :: circle
      TYPE(LineType),INTENT(IN) :: line
      TYPE(PointType),INTENT(INOUT) :: p1
      TYPE(PointType),INTENT(INOUT) :: p2
      TYPE(PointType),INTENT(INOUT) :: p3
      TYPE(PointType),INTENT(INOUT) :: p4
      LOGICAL(SBK) :: arcTest
      REAL(SRK) :: a,b,c,t1,t2,u(2),w(2),ra,discr,theta,theta_shift,c0(2),c1(2),c2(2)
      TYPE(LineType) :: arcLine

      CALL p1%clear()
      CALL p2%clear()
      CALL p3%clear()
      CALL p4%clear()
      p1%dim=-1
      p2%dim=-1
      p3%dim=-1
      p4%dim=-1
      IF(circle%c%dim == 2 .AND. line%p1%dim == 2 .AND. &
        line%p2%dim == 2 .AND. circle%r > 0.0_SRK) THEN
        u(1)=line%p2%coord(1)-line%p1%coord(1)  !dx
        u(2)=line%p2%coord(2)-line%p1%coord(2)  !dy
        w(1)=line%p1%coord(1)-circle%c%coord(1)
        w(2)=line%p1%coord(2)-circle%c%coord(2)
        b=w(1)*u(1)+w(2)*u(2)
        c=w(1)*w(1)+w(2)*w(2)-circle%r*circle%r
        IF(c > zero .AND. b > zero) THEN
          p1%dim=-2
          p2%dim=-2
        ELSE
          a=u(1)*u(1)+u(2)*u(2) !dr^2
          discr=b*b-a*c
          arcTest=(circle%thetastt /= ZERO .OR. circle%thetastp /= TWOPI)
          IF(discr < -EPSREAL) THEN
            !Disjoint
            p1%dim=-2
            p2%dim=-2
          ELSEIF(discr .APPROXEQA. zero) THEN
            !Tangent
            p1%dim=-3
            p2%dim=-3
          ELSE
            p1%dim=0
            p2%dim=0
            ra=one/a
            discr=SQRT(discr)
            t1=(-b-discr)*ra
            t2=(-b+discr)*ra
            IF(ZERO < t1 .AND. t1 < ONE) THEN
              p1=line%p1
              p1%coord(1)=p1%coord(1)+u(1)*t1
              p1%coord(2)=p1%coord(2)+u(2)*t1
              IF(arcTest) THEN
                theta_shift=0.0_SRK
                theta=ATAN2PI((p1%coord(1)-circle%c%coord(1)),(p1%coord(2)-circle%c%coord(2)))
                IF(circle%thetastt > circle%thetastp) THEN
                  theta_shift=TWOPI
                  IF(p1%coord(2)-circle%c%coord(2) > 0.0_SRK) theta=theta+TWOPI
                ENDIF
                IF((theta .APPROXLE. circle%thetastt) .OR. &
                  (circle%thetastp+theta_shift .APPROXLE. theta)) CALL p1%clear()
              ENDIF
            ENDIF
            IF(ZERO < t2 .AND. t2 < ONE) THEN
              p2=line%p1
              p2%coord(1)=p2%coord(1)+u(1)*t2
              p2%coord(2)=p2%coord(2)+u(2)*t2
              IF(arcTest) THEN
                theta_shift=0.0_SRK
                theta=ATAN2PI((p2%coord(1)-circle%c%coord(1)),(p2%coord(2)-circle%c%coord(2)))
                IF(circle%thetastt > circle%thetastp) THEN
                  theta_shift=TWOPI
                  IF(p2%coord(2)-circle%c%coord(2) > 0.0_SRK) theta=theta+TWOPI
                ENDIF
                IF((theta .APPROXLE. circle%thetastt) .OR. &
                  (circle%thetastp+theta_shift .APPROXLE. theta)) CALL p2%clear()
              ENDIF
            ENDIF
          ENDIF
          IF(arcTest) THEN
            !Test for intersection with segments between center of rotation
            !and ends of arc
            arcLine%p1=circle%c
            c0=circle%c%coord
            c1(1)=c0(1)+circle%r*COS(circle%thetastt)
            c1(2)=c0(2)+circle%r*SIN(circle%thetastt)
            c2(1)=c0(1)+circle%r*COS(circle%thetastp)
            c2(2)=c0(2)+circle%r*SIN(circle%thetastp)
            CALL arcLine%p2%init(DIM=2,X=c1(1),Y=c1(2))
            p3=arcLine%intersect(line)
            arcLine%p2%coord=c2
            p4=arcLine%intersect(line)

            IF(p1%dim /= 2 .AND. p2%dim /= 2) THEN
              !No other intersections, therefore we check p3 and p4
              !to find out if either lies on a corner of the wedge
              IF(p3%dim == 2) THEN
                IF(p4%dim < 0) THEN
                  !The other surface is co-linear or disjoint
                  !so we mark this as co-linear or disjoint
                  IF(ALL(p3%coord .APPROXEQA. c0) .OR. &
                     ALL(p3%coord .APPROXEQA. c1) .OR. &
                     ALL(p3%coord .APPROXEQA. c2)) p3%dim=p4%dim
                ELSEIF(p4%dim == 2) THEN
                  IF(ALL(p3%coord .APPROXEQA. p4%coord)) THEN
                    p3%dim=-3
                    p4%dim=-3
                  ENDIF
                ENDIF
              ELSEIF(p3%dim < 0 .AND. p4%dim > 0) THEN
                !The other surface is co-linear or disjoint
                !so we mark this as co-linear or disjoint
                IF(ALL(p4%coord .APPROXEQA. c0) .OR. &
                   ALL(p4%coord .APPROXEQA. c1) .OR. &
                   ALL(p4%coord .APPROXEQA. c2)) p4%dim=p3%dim
              ENDIF
            ELSE
              IF(p3%dim == 2 .AND. p4%dim == 2) THEN
                IF(ALL(p3%coord .APPROXEQA. p4%coord)) p4%dim=-3
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE intersect_ArcCircleType_and_LineType
!
!-------------------------------------------------------------------------------
!> @brief Determines the points of intersection between a line and a cylinder
!> (if any).
!> @param cyl the cylinder object to test for intersection
!> @param line the line segment to test for intersection
!> @param p1 the first point of intersection (if it exists)
!> @param p2 the second point of intersection (if it exists)
!> @note a return code is assigned to @c p1%dim and @c p2%dim indicating the type of
!> intersection. @n
!>  > 0: success; an intersection point was found @n
!> == 0: no intersection was found @n
!>   -1: problem with dummy arguments passed to routine @n
!>   -2: the line segment lies totally outside the cylinders P-surface @n
!>   -3: the line segment lies totally outside the cylinders Q-surface @n
!>   -4: the segment lies on the cylinder's surface @n
!>   -5: the segment is parallel to the cylinder's axis and outisde it's radius @n
!>   -6: the segment is parallel to the cylinder's axis and totally inside it @n
!>   -7: the segment is tangent to the cylinder @n
!>   -8: the segment does not intersect the cylinder @n
!>
!> The algorithm for finding the intersection is similar to that outlined
!> in section 5.3.7 of "Real-Time Collision Detection" by Christer Ericson.
!> The problem for the intersection is stated as a quadratic equation which is
!> obtained by substituting the parametric equation of a line into the
!> parametric equation for a cylinder.
!>
!> For a segment defined by points A and B with orientation A->B, and a finite
!> cylinder defined by a central axis of rotation P->Q and radius R, the
!> equation is given as:
!>
!>  \f[ a t^2 + 2b t + c = 0 \f]
!>
!> with
!>
!>  \f[ a = \left(\vec{d}\cdot\vec{d}\right) \left(\vec{u}\cdot\vec{u}\right) - \left(\vec{u}\cdot\vec{d}\right)^2 \f]
!>  \f[ b = \left(\vec{d}\cdot\vec{d}\right) \left(\vec{w}\cdot\vec{u}\right) - \left(\vec{u}\cdot\vec{d}\right) \left(\vec{w}\cdot\vec{d}\right) \f]
!>  \f[ c = \left(\vec{d}\cdot\vec{d}\right) \left(\vec{w}\cdot\vec{w} - R^2\right) - \left(\vec{w}\cdot\vec{d}\right)^2 \f]
!>
!> and
!>
!>  \f[ \vec{u} = \vec{B}-\vec{A} \f]
!>  \f[ \vec{d} = \vec{Q}-\vec{P} \f]
!>  \f[ \vec{w} = \vec{A}-\vec{P} \f]
!>
!> One then solves the quadratic equation for the roots of \f$ t \f$
!> (where \f$ 0 <= t <= 1 \f$) and obtains the coordinates of the points as:
!>
!> \f[ \vec{L}(t)=\vec{A}+\vec{u} t \f]
!>
!> Intersections at the end-caps of the cylinder (called the P-surface and
!> Q-surface) have the values of t as:
!>
!> \f[ t_P = -\left(\vec{w}\cdot\vec{d}\right)/\left(\vec{u}\cdot\vec{d}\right) \f]
!> \f[ t_Q = \left(\vec{d}\cdot\vec{d}-\vec{w}\cdot\vec{d}\right)/\left(\vec{u}\cdot\vec{d}\right) \f]
!>
!> @TODO: Create sister routine to work with partial cylinders
!>
    ELEMENTAL SUBROUTINE intersect_CylinderType_and_LineType(cyl,line,p1,p2)
      CLASS(CylinderType),INTENT(IN) :: cyl
      TYPE(LineType),INTENT(IN) :: line
      TYPE(PointType),INTENT(INOUT) :: p1
      TYPE(PointType),INTENT(INOUT) :: p2
      LOGICAL(SBK) :: arcTest
      REAL(SRK) :: u(3),w(3),d(3)
      REAL(SRK) :: ud,uw,uu,wd,dd,t1,t2,tp,tq,a,b,c,k,g1,g2,ra,discr
      !REAL(SRK) :: x(2),theta

      CALL p1%clear()
      CALL p2%clear()
      p1%dim=-1
      p2%dim=-1
      !Check for valid input
      IF(cyl%axis%p1%dim == 3 .AND. cyl%axis%p2%dim == 3 .AND. &
        line%p1%dim == 3 .AND. line%p2%dim == 3 .AND. cyl%r > zero) THEN
        p1%dim=0
        p2%dim=0

        !For a line defined by A->B compute B-A as slope for parametric eqn's
        !P(t)=A+u*t for a point on segment A->B.
        u(1)=line%p2%coord(1)-line%p1%coord(1)
        u(2)=line%p2%coord(2)-line%p1%coord(2)
        u(3)=line%p2%coord(3)-line%p1%coord(3)

        !For a central axis of rotation defined by P->Q compute Q-P
        d(1)=cyl%axis%p2%coord(1)-cyl%axis%p1%coord(1)
        d(2)=cyl%axis%p2%coord(2)-cyl%axis%p1%coord(2)
        d(3)=cyl%axis%p2%coord(3)-cyl%axis%p1%coord(3)

        !Vector of P->A, used to project A->B onto cylinder-local coordinate
        w(1)=line%p1%coord(1)-cyl%axis%p1%coord(1)
        w(2)=line%p1%coord(2)-cyl%axis%p1%coord(2)
        w(3)=line%p1%coord(3)-cyl%axis%p1%coord(3)

        !Dot(u,d), length of A->B projected onto P->Q
        ud=u(1)*d(1)+u(2)*d(2)+u(3)*d(3)
        !Dot(w,d), length of P->A projected onto P->Q
        wd=w(1)*d(1)+w(2)*d(2)+w(3)*d(3)
        !Dot(d,d), length of P->Q
        dd=d(1)*d(1)+d(2)*d(2)+d(3)*d(3)

        IF(wd < -EPSREAL .AND. wd+ud < -EPSREAL) THEN
          !The line segment is totally outside the P-surface
          p1%dim=-2
          p2%dim=-2
        ELSEIF(wd > dd .AND. wd+ud > dd) THEN
          !The line segment is totally outside the Q-surface
          p1%dim=-3
          p2%dim=-3
        ELSE
          !At least one of the segment end points lies within interval spanned
          !by P->Q, or on opposite ends of P and Q

          !Dot(u,u), length of A->B
          uu=u(1)*u(1)+u(2)*u(2)+u(3)*u(3)
          !Dot(u,w), projection of A->B into direction perpendicular to P->Q
          uw=u(1)*w(1)+u(2)*w(2)+u(3)*w(3)

          a=dd*uu-ud*ud
          k=w(1)*w(1)+w(2)*w(2)+w(3)*w(3)-cyl%r*cyl%r
          c=dd*k-wd*wd

          IF(ABS(a) < EPSREAL) THEN
!Segment A->B is parallel to P->Q
            IF(c .APPROXEQA. zero) THEN
              !Segment is on the cylinder surface
              p1%dim=-4
              p2%dim=-4
            ELSEIF(c > zero) THEN
              !Segment is outside cylinder radius
              p1%dim=-5
              p2%dim=-5
            ELSE
              !Segment is inside cylinder, check end-caps
              IF(wd < zero .OR. wd+ud < zero) THEN !segment intersects P-surface
                tp=-wd/ud
                p1=line%p1
                p1%coord(1)=p1%coord(1)+tp*u(1)
                p1%coord(2)=p1%coord(2)+tp*u(2)
                p1%coord(3)=p1%coord(3)+tp*u(3)
              ENDIF
              IF(wd > dd .OR. wd+ud > dd) THEN !segment intersects Q-surface
                tq=(dd-wd)/ud
                p2=line%p1
                p2%coord(1)=p2%coord(1)+tq*u(1)
                p2%coord(2)=p2%coord(2)+tq*u(2)
                p2%coord(3)=p2%coord(3)+tq*u(3)
              ENDIF
              IF((zero < wd .AND. wd < dd) .AND. &
                (zero < wd+ud .AND. wd+ud < dd)) THEN
                !Segment is totally inside cylinder and parallel to P->Q
                p1%dim=-6
                p2%dim=-6
              ENDIF
            ENDIF
          ELSE
!Segment A->B is not parallel to P->Q
            b=dd*uw-ud*wd
            discr=b*b-a*c
            IF(discr .APPROXEQA. zero) THEN
              !Line is tangent
              p1%dim=-7
              p2%dim=-7
            ELSEIF(discr < zero) THEN
              !No real roots, no intersection
              p1%dim=-8
              p2%dim=-8
            ELSE
              !Solution to quadratic formula
              ra=1.0_SRK/a
              g1=-b*ra
              g2=SQRT(discr)*ra
              t1=g1-g2
              t2=g1+g2

              arcTest=(cyl%thetastt /= ZERO .OR. cyl%thetastp /= TWOPI)
              IF(zero < t1 .AND. t1 < one) THEN
                !Intersection with the side of a cylinder
                p1=line%p1
                p1%coord(1)=p1%coord(1)+t1*u(1)
                p1%coord(2)=p1%coord(2)+t1*u(2)
                p1%coord(3)=p1%coord(3)+t1*u(3)
              ELSE
                IF(wd+t1*ud < zero) THEN
                  tp=-wd/ud
                  IF(((k+tp*(2._SRK*uw+tp*uu)) < zero) .AND.  &
                    (zero < tp .AND. tp < one)) THEN
                    !Intersection with P-surface
                    p1=line%p1
                    p1%coord(1)=p1%coord(1)+tp*u(1)
                    p1%coord(2)=p1%coord(2)+tp*u(2)
                    p1%coord(3)=p1%coord(3)+tp*u(3)
                  ENDIF
                ELSEIF(wd+t1*ud > dd) THEN
                  tq=(dd-wd)/ud
                  IF(((k+dd-2._SRK*wd+tq*(2._SRK*(uw-ud)+tq*uu)) < zero) .AND. &
                    (zero < tq .AND. tq < one)) THEN
                    !Intersection with Q-surface
                    p1=line%p1
                    p1%coord(1)=p1%coord(1)+tq*u(1)
                    p1%coord(2)=p1%coord(2)+tq*u(2)
                    p1%coord(3)=p1%coord(3)+tq*u(3)
                  ENDIF
                ENDIF
              ENDIF

              IF(zero < t2 .AND. t2 < one) THEN
                !Intersection with the side of a cylinder
                p2=line%p1
                p2%coord(1)=p2%coord(1)+t2*u(1)
                p2%coord(2)=p2%coord(2)+t2*u(2)
                p2%coord(3)=p2%coord(3)+t2*u(3)
              ELSE
                IF(wd+t2*ud < zero) THEN
                  tp=-wd/ud
                  IF(((k+tp*(2._SRK*uw+tp*uu)) < zero) .AND.  &
                    (zero < tp .AND. tp < one)) THEN
                    !Intersection with P-surface
                    p2=line%p1
                    p2%coord(1)=p2%coord(1)+tp*u(1)
                    p2%coord(2)=p2%coord(2)+tp*u(2)
                    p2%coord(3)=p2%coord(3)+tp*u(3)
                  ENDIF
                ELSEIF(wd+t2*ud > dd) THEN
                  tq=(dd-wd)/ud
                  IF(((k+dd-2._SRK*wd+tq*(2._SRK*(uw-ud)+tq*uu)) < zero) .AND. &
                    (zero < tq .AND. tq < one)) THEN
                    !Intersection with Q-surface
                    p2=line%p1
                    p2%coord(1)=p2%coord(1)+tq*u(1)
                    p2%coord(2)=p2%coord(2)+tq*u(2)
                    p2%coord(3)=p2%coord(3)+tq*u(3)
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE intersect_CylinderType_and_LineType
!
!-------------------------------------------------------------------------------
!> @brief Determines whether a point lies within a circle type.
!> @param circle the circle type to test for intersection
!> @param point the first to query
    ELEMENTAL FUNCTION inside_CircleType(circle,point) RESULT(bool)
      CLASS(CircleType),INTENT(IN) :: circle
      TYPE(PointType),INTENT(IN) :: point
      LOGICAL(SBK) :: bool
      REAL(SRK) :: x,y,dtheta ,cosstt,cosstp,sinstt,sinstp
      bool=.FALSE.
      x=point%coord(1)-circle%c%coord(1)
      y=point%coord(2)-circle%c%coord(2)
      dtheta=circle%thetastp-circle%thetastt
      IF((x*x+y*y) .APPROXLE. circle%r*circle%r) THEN
        IF(dtheta .APPROXLE. PI)THEN
          cosstt=COS(circle%thetastt)
          cosstp=COS(circle%thetastp)
          sinstt=SIN(circle%thetastt)
          sinstp=SIN(circle%thetastp)
          IF(COS(circle%thetastt)*y .APPROXGE. SIN(circle%thetastt)*x) &
            bool=COS(circle%thetastp)*y .APPROXLE. SIN(circle%thetastp)*x
        ELSEIF(dtheta < TWOPI) THEN
          cosstt=COS(circle%thetastt)
          cosstp=COS(circle%thetastp)
          sinstt=SIN(circle%thetastt)
          sinstp=SIN(circle%thetastp)
          bool=(COS(circle%thetastt)*y .APPROXGE. SIN(circle%thetastt)*x) &
            .OR. (COS(circle%thetastp)*y .APPROXLE. SIN(circle%thetastp)*x)
        ELSE
          bool=.TRUE.
        ENDIF
      ENDIF
    ENDFUNCTION inside_CircleType
!
!-------------------------------------------------------------------------------
!> @brief Determines whether a point lies within a circle type.
!> @param circle the circle type to test for intersection
!> @param point the first to query
    ELEMENTAL FUNCTION onSurface_CircleType(circle,point) RESULT(bool)
      CLASS(CircleType),INTENT(IN) :: circle
      TYPE(PointType),INTENT(IN) :: point
      LOGICAL(SBK) :: bool
      REAL(SRK) :: x,y,theta,theta_shift
      bool=.FALSE.
      IF(point%dim == 2 .AND. circle%r > 0.0_SRK) THEN
        x=point%coord(1)-circle%c%coord(1)
        y=point%coord(2)-circle%c%coord(2)
        IF((x*x+y*y) .APPROXEQA. circle%r*circle%r) THEN
          theta=ATAN2PI(x,y)
          theta_shift=0.0_SRK
          IF(circle%thetastt > circle%thetastp) theta_shift=TWOPI
          IF(y > 0.0_SRK .OR. ((y .APPROXEQA. 0.0_SRK) .AND. &
            (x .APPROXGE. 0.0_SRK))) theta=theta+theta_shift
          bool=(circle%thetastt .APPROXLE. theta) .AND. &
               (theta .APPROXLE. circle%thetastp+theta_shift)
        ENDIF
      ENDIF
    ENDFUNCTION onSurface_CircleType
!
!-------------------------------------------------------------------------------
!> @brief Defines the 'is equal to' operation between two circles e.g. @c
!>        circ1 == circ2
!> @param p0 the first circle
!> @param p1 the second circle
!> @returns @c bool the boolean result of the operation
!>
!> Function is elemental so it can be used on an array of circles.
    ELEMENTAL FUNCTION isequal_CircleType(c0,c1) RESULT(bool)
      TYPE(CircleType),INTENT(IN) :: c0,c1
      LOGICAL(SBK) :: bool
      bool=.FALSE.
      IF((c0%r .APPROXEQA. c1%r) .AND. (c0%thetastt .APPROXEQA. c1%thetastt) .AND. &
        (c0%thetastp .APPROXEQA. c1%thetastp)) bool=(c0%c == c1%c)
    ENDFUNCTION isequal_CircleType
!
!-------------------------------------------------------------------------------
!> @brief Defines the 'is equal to' operation between two cylinders e.g. @c
!>        cyl1 == cyl2
!> @param p0 the first cylinder
!> @param p1 the second cylinder
!> @returns @c bool the boolean result of the operation
!>
!> Function is elemental so it can be used on an array of cylinders.
    ELEMENTAL FUNCTION isequal_CylinderType(c0,c1) RESULT(bool)
      TYPE(CylinderType),INTENT(IN) :: c0,c1
      LOGICAL(SBK) :: bool
      bool=.FALSE.
      IF((c0%r .APPROXEQA. c1%r) .AND. (c0%thetastt .APPROXEQA. c1%thetastt) .AND. &
        (c0%thetastp .APPROXEQA. c1%thetastp)) bool=(c0%axis == c1%axis)
    ENDFUNCTION isequal_CylinderType
!
ENDMODULE Geom_CircCyl
