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
  USE Geom_Points
  USE Geom_Line
  IMPLICIT NONE
  PRIVATE !Default contents of module to private
!
! List of Public items
  PUBLIC :: CircleType
  PUBLIC :: CylinderType

  !> @brief Type for a circle
  TYPE :: CircleType
    !> The point that defines the center of rotation of the circle.
    TYPE(PointType) :: c
    !> The radius of the circle
    REAL(SRK) :: r=0.0_SRK
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
  ENDTYPE CircleType
  
  !> @brief Type for a cylinder
  TYPE :: CylinderType
    !> The line segment that defines the center of rotation of the cylinder and
    !> its extents (if finite).
    TYPE(LineType) :: axis
    !> The radius of the circle
    REAL(SRK) :: r=0.0_SRK
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
  
  REAL(SRK),PARAMETER :: zero=0.0_SRK
  REAL(SRK),PARAMETER :: one=1.0_SRK
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Sets the values for the CircleType object attributes
!> @param circ the CircleType object to set
!> @param p a point for the center of rotation (must be 2-D)
!> @param r the radius of the circle
    ELEMENTAL SUBROUTINE set_CircleType(circ,p,r)
      CLASS(CircleType),INTENT(INOUT) :: circ
      TYPE(PointType),INTENT(IN) :: p
      REAL(SRK),INTENT(IN) :: r
      CALL circ%clear()
      IF(p%dim == 2 .AND. r > zero) THEN
        circ%c=p
        circ%r=r
      ENDIF
    ENDSUBROUTINE set_CircleType
!
!-------------------------------------------------------------------------------
!> @brief Clears a CircleType object's attributes
!> @param circ the CircleType object to clear
    ELEMENTAL SUBROUTINE clear_CircleType(circ)
      CLASS(CircleType),INTENT(INOUT) :: circ
      CALL circ%c%clear()
      circ%r=zero
    ENDSUBROUTINE clear_CircleType
!
!-------------------------------------------------------------------------------
!> @brief Sets the values for the CylinderType object attributes
!> @param cyl the CylinderType object to set
!> @param p a point for the "bottom" of the central axis of rotation
!>        (must be 3-D)
!> @param q a point for the "top" of the central axis of rotation (must be 3-D)
!> @param r the radius of the cylinder
    ELEMENTAL SUBROUTINE set_CylinderType(cyl,p,q,r)
      CLASS(CylinderType),INTENT(INOUT) :: cyl
      TYPE(PointType),INTENT(IN) :: p
      TYPE(PointType),INTENT(IN) :: q
      REAL(SRK),INTENT(IN) :: r
      CALL cyl%clear()
      IF(p%dim == 3 .AND. .NOT.(p .APPROXEQA. q)  .AND. r > zero) THEN
        CALL cyl%axis%set(p,q)
        cyl%r=r
      ENDIF
    ENDSUBROUTINE set_CylinderType
!
!-------------------------------------------------------------------------------
!> @brief Clears a CylinderType object's attributes
!> @param circ the CylinderType object to clear
    ELEMENTAL SUBROUTINE clear_CylinderType(cyl)
      CLASS(CylinderType),INTENT(INOUT) :: cyl
      CALL cyl%axis%clear()
      cyl%r=zero
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
      IF(circle%c%dim == 2 .AND. line%p(1)%dim == 2 .AND. &
        line%p(2)%dim == 2 .AND. circle%r > 0.0_SRK) THEN
        u(1)=line%p(2)%coord(1)-line%p(1)%coord(1)
        u(2)=line%p(2)%coord(2)-line%p(1)%coord(2)
        w(1)=line%p(1)%coord(1)-circle%c%coord(1)
        w(2)=line%p(1)%coord(2)-circle%c%coord(2)
        b=w(1)*u(1)+w(2)*u(2)
        c=w(1)*w(1)+w(2)*w(2)-circle%r*circle%r
        IF(c > zero .AND. b > zero) THEN
          p1%dim=-2
          p2%dim=-2
        ELSE
          a=u(1)*u(1)+u(2)*u(2)
          discr=b*b-a*c
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
            IF(zero < t1 .AND. t1 < one) THEN
              p1=line%p(1)
              p1%coord(1)=p1%coord(1)+u(1)*t1
              p1%coord(2)=p1%coord(2)+u(2)*t1
            ENDIF
            IF(zero < t2 .AND. t2 < one) THEN
              p2=line%p(1)
              p2%coord(1)=p2%coord(1)+u(1)*t2
              p2%coord(2)=p2%coord(2)+u(2)*t2
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE intersect_CircleType_and_LineType
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
    ELEMENTAL SUBROUTINE intersect_CylinderType_and_LineType(cyl,line,p1,p2)
      CLASS(CylinderType),INTENT(IN) :: cyl
      TYPE(LineType),INTENT(IN) :: line
      TYPE(PointType),INTENT(INOUT) :: p1
      TYPE(PointType),INTENT(INOUT) :: p2
      
      REAL(SRK) :: u(3),w(3),d(3)
      REAL(SRK) :: ud,uw,uu,wd,dd,t1,t2,tp,tq,a,b,c,k,g1,g2,ra,discr
      
      CALL p1%clear()
      CALL p2%clear()
      p1%dim=-1
      p2%dim=-1
      !Check for valid input
      IF(cyl%axis%p(1)%dim == 3 .AND. cyl%axis%p(2)%dim == 3 .AND. &
        line%p(1)%dim == 3 .AND. line%p(2)%dim == 3 .AND. cyl%r > zero) THEN
        p1%dim=0
        p2%dim=0
        
        !For a line defined by A->B compute B-A as slope for parametric eqn's
        !P(t)=A+u*t for a point on segment A->B.
        u(1)=line%p(2)%coord(1)-line%p(1)%coord(1)
        u(2)=line%p(2)%coord(2)-line%p(1)%coord(2)
        u(3)=line%p(2)%coord(3)-line%p(1)%coord(3)
        
        !For a central axis of rotation defined by P->Q compute Q-P
        d(1)=cyl%axis%p(2)%coord(1)-cyl%axis%p(1)%coord(1)
        d(2)=cyl%axis%p(2)%coord(2)-cyl%axis%p(1)%coord(2)
        d(3)=cyl%axis%p(2)%coord(3)-cyl%axis%p(1)%coord(3)
        
        !Vector of P->A, used to project A->B onto cylinder-local coordinate
        w(1)=line%p(1)%coord(1)-cyl%axis%p(1)%coord(1)
        w(2)=line%p(1)%coord(2)-cyl%axis%p(1)%coord(2)
        w(3)=line%p(1)%coord(3)-cyl%axis%p(1)%coord(3)
        
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
                p1=line%p(1)
                p1%coord(1)=p1%coord(1)+tp*u(1)
                p1%coord(2)=p1%coord(2)+tp*u(2)
                p1%coord(3)=p1%coord(3)+tp*u(3)
              ENDIF
              IF(wd > dd .OR. wd+ud > dd) THEN !segment intersects Q-surface
                tq=(dd-wd)/ud
                p2=line%p(1)
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
              
              IF(zero < t1 .AND. t1 < one) THEN
                !Intersection with the side of a cylinder
                p1=line%p(1)
                p1%coord(1)=p1%coord(1)+t1*u(1)
                p1%coord(2)=p1%coord(2)+t1*u(2)
                p1%coord(3)=p1%coord(3)+t1*u(3)
              ELSE
                IF(wd+t1*ud < zero) THEN
                  tp=-wd/ud
                  IF(((k+tp*(2._SRK*uw+tp*uu)) < zero) .AND.  &
                    (zero < tp .AND. tp < one)) THEN
                    !Intersection with P-surface
                    p1=line%p(1)
                    p1%coord(1)=p1%coord(1)+tp*u(1)
                    p1%coord(2)=p1%coord(2)+tp*u(2)
                    p1%coord(3)=p1%coord(3)+tp*u(3)
                  ENDIF
                ELSEIF(wd+t1*ud > dd) THEN
                  tq=(dd-wd)/ud
                  IF(((k+dd-2._SRK*wd+tq*(2._SRK*(uw-ud)+tq*uu)) < zero) .AND. &
                    (zero < tq .AND. tq < one)) THEN
                    !Intersection with Q-surface
                    p1=line%p(1)
                    p1%coord(1)=p1%coord(1)+tq*u(1)
                    p1%coord(2)=p1%coord(2)+tq*u(2)
                    p1%coord(3)=p1%coord(3)+tq*u(3)
                  ENDIF
                ENDIF
              ENDIF
              
              IF(zero < t2 .AND. t2 < one) THEN
                !Intersection with the side of a cylinder
                p2=line%p(1)
                p2%coord(1)=p2%coord(1)+t2*u(1)
                p2%coord(2)=p2%coord(2)+t2*u(2)
                p2%coord(3)=p2%coord(3)+t2*u(3)
              ELSE
                IF(wd+t2*ud < zero) THEN
                  tp=-wd/ud
                  IF(((k+tp*(2._SRK*uw+tp*uu)) < zero) .AND.  &
                    (zero < tp .AND. tp < one)) THEN
                    !Intersection with P-surface
                    p2=line%p(1)
                    p2%coord(1)=p2%coord(1)+tp*u(1)
                    p2%coord(2)=p2%coord(2)+tp*u(2)
                    p2%coord(3)=p2%coord(3)+tp*u(3)
                  ENDIF
                ELSEIF(wd+t2*ud > dd) THEN
                  tq=(dd-wd)/ud
                  IF(((k+dd-2._SRK*wd+tq*(2._SRK*(uw-ud)+tq*uu)) < zero) .AND. &
                    (zero < tq .AND. tq < one)) THEN
                    !Intersection with Q-surface
                    p2=line%p(1)
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
ENDMODULE Geom_CircCyl
