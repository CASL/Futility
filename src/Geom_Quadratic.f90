!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module defining quadratic functions for use in geometry.
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Quadratic
USE IntrType
USE Geom_Points

IMPLICIT NONE
PRIVATE
!
! List of Public items
PUBLIC :: QuadraticType

REAL(SDK), PARAMETER :: PI=3.14159265358979311599796346854
!> It is defined by ax^2 + bx + c.
!> There is also an x,y shift and a theta rotation parameter
!> to make the quadratic type general
TYPE :: QuadraticType
  REAL(SRK) :: a=0.0, b=0.0, theta=0.0, shift_x=0.0, shift_y=0.0
  TYPE(PointType) :: points(3)
!
!List of type bound procedures
  CONTAINS
    !> @copybrief Geom_Quadratic::init_QuadraticType
    !> @copydetails Geom_Quadratic::init_QuadraticType
    PROCEDURE,PASS :: set => init_QuadraticType
    !> @copybrief Geom_Quadratic::clear_QuadraticType
    !> @copydetails Geom_Quadratic::clear_QuadraticType
    PROCEDURE,PASS :: clear => clear_QuadraticType
!    !> @copybrief Geom_Quadratic::length_lineType
!    !> @copydetails Geom_Quadratic::length_lineType
!    PROCEDURE,PASS :: length => length_lineType
!    !> @copybrief Geom_Quadratic::intersect_QuadraticType_and_QuadraticType
!    !> @copydetails Geom_Quadratic::intersect_QuadraticType_and_QuadraticType
!    PROCEDURE,PASS :: intersectLine => intersect_QuadraticType_and_QuadraticType
!    !> @copybrief Geom_Quadratic::pointIsLeft_QuadraticType
!    !> @copydetails Geom_Quadratic::pointIsLeft_QuadraticType
!    PROCEDURE,PASS :: pointIsLeft => pointIsLeft_QuadraticType
ENDTYPE QuadraticType

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Constructor for QuadraticType
!> @param p1 Start point of the arc
!> @param p2 End point of the arc
!> @param p3 An additional point on the arc
!> @returns line the QuadraticType object
!>
!> If sp and ep are not of the same type line is returned as NULL.
SUBROUTINE init_QuadraticType(arc,p1,p2,p3)
  CLASS(QuadraticType),INTENT(INOUT) :: arc
  TYPE(PointType),INTENT(IN) :: p1,p2,p3
  TYPE(PointType) :: p0,p1s,p2s,p3s
  REAL(SDK) :: a, b, theta, rotation_mat(2,2)
  CALL arc%clear()
  IF(p1%dim == p2%dim .AND. p2%dim == p3%dim .AND.  p1%dim == 2) THEN
    arc%points = (/ p1, p2, p3/)
    ! Shift to origin
    p0 = p1
    p1s = p1 - p0
    p2s = p2 - p0
    p3s = p3 - p0
    arc%shift_x = p1%coord(1)
    arc%shift_y = p1%coord(2)

    ! rotate linear edge to become the x-axis
    IF( p2s%coord(1) .APPROXEQ. 0.0_SRK ) THEN
      IF( p2s%coord(2) >= 0.0_SRK ) THEN
        theta = PI/2.0_SDK
      ELSE
        theta = -PI/2.0_SDK
      ENDIF
    ELSE
      theta = ATAN(p2s%coord(2)/p2s%coord(1))
    ENDIF

    IF(p2s%coord(1) < 0.0) theta = theta + pi

    arc%theta = theta

    rotation_mat(1,:) = (/COS(theta), SIN(theta)/)
    rotation_mat(2,:) = (/-SIN(theta), COS(theta)/)
    p1s%coord = MATMUL(rotation_mat, p1s%coord)
    p2s%coord = MATMUL(rotation_mat, p2s%coord)
    p3s%coord = MATMUL(rotation_mat, p3s%coord)

    ! Get quadratic coefficients
    !   y = ax^2 + bx + c
    ! Since p1s%x = 0, p1s%y = 0 due to the shift to the origin,
    !   0 = 0 + 0 + c --> c = 0
    ! Due to the rotation to make the linear edge the x-axis, p2s%y = 0
    !   0 = ax_2^2 + bx_2 --> ax_2 + b = 0 --> b = -ax_2
    ! Lastly,
    !   y_3 = ax_3^2 + bx_3
    ! Using, b = -ax_2
    !   y_3 = ax_3(x_3  - x_2) --> a = y_3/x_3 1/(x_3 - x_2)
    ! Note if x_3 = 0 --> y_3 = 0 --> (x_1, y_1) = (x_3, y_3), which is invalid
    a = (p3s%coord(2)/p3s%coord(1))/(p3s%coord(1) - p2s%coord(1))
    b = -a*p2s%coord(1)
    arc%a = a
    arc%b = b
    CALL p0%clear()
    CALL p1s%clear()
    CALL p2s%clear()
    CALL p3s%clear()
  ENDIF
ENDSUBROUTINE init_QuadraticType
!
!-------------------------------------------------------------------------------
!> @brief Deallocates the @c sp and @c ep components of line
!> @param line line segment of type @c QuadraticType
ELEMENTAL SUBROUTINE clear_QuadraticType(arc)
  CLASS(QuadraticType),INTENT(INOUT) :: arc
  INTEGER(SIK) :: i
  DO i = 1,3
    CALL arc%points(i)%clear()
  ENDDO
  arc%a = 0.0
  arc%b = 0.0
  arc%theta = 0.0
  arc%shift_x = 0.0
  arc%shift_y = 0.0
ENDSUBROUTINE clear_QuadraticType
!
!-------------------------------------------------------------------------------
!> @brief Finds the intersection between 2 line segments (if it exists)
!> @param l1 the line to test for intersection
!> @param l2 the other line to test for intersection
!> @returns @c p the point of intersection
!> @note a return code is assigned to p%dim indicating the type of
!> intersection.
!>   -3: there is no intersection (disjoint)
!>   -2: the line segments overlap
!>   -1: problem with dummy arguments passed to routine
!>  > 0: success; an intersection point was found
!ELEMENTAL FUNCTION intersect_QuadraticType_and_QuadraticType(l1,l2) RESULT(p)
!  CLASS(QuadraticType),INTENT(IN) :: l1
!  TYPE(QuadraticType),INTENT(IN) :: l2
!  TYPE(PointType) :: p
!  p%dim=-1
!  IF(l1%p1%dim == l1%p2%dim .AND. l2%p1%dim == l2%p2%dim .AND. &
!      l1%p1%dim == l2%p1%dim .AND. l1%p1%dim > 0) THEN
!    SELECTCASE(l1%p1%dim)
!    CASE(1)
!      p%dim=-2 !Always collinear/overlap
!    CASE(2)
!      p=intersect_lines2D(l1%p1,l1%p2,l2%p1,l2%p2)
!    CASE(3)
!      p=intersect_lines3D(l1%p1,l1%p2,l2%p1,l2%p2)
!    ENDSELECT
!  ENDIF
!ENDFUNCTION intersect_QuadraticType_and_QuadraticType
!
!-------------------------------------------------------------------------------
!> @brief Finds the intersection between two 2-D line segments (if it exists)
!> @param s1p0 the line to test for intersection
!> @param s1p1 the line to test for intersection
!> @param s2p0 the line to test for intersection
!> @param s2p1 the line to test for intersection
!> @returns @c p the point of intersection (if it exists)
!> @note a return code is assigned to p%dim indicating the type of
!> intersection.
!>   -3: there is no intersection (disjoint)
!>   -2: the line segments overlap
!>  > 0: success; an intersection point was found
!ELEMENTAL FUNCTION intersect_lines2D(s1p0,s1p1,s2p0,s2p1) RESULT(p)
!  TYPE(PointType),INTENT(IN) :: s1p0,s1p1,s2p0,s2p1
!  TYPE(PointType) :: p
!  REAL(SRK) :: u(2),v(2),w(2),d,s,t
!  u(1)=s1p1%coord(1)-s1p0%coord(1)
!  u(2)=s1p1%coord(2)-s1p0%coord(2)
!  v(1)=s2p1%coord(1)-s2p0%coord(1)
!  v(2)=s2p1%coord(2)-s2p0%coord(2)
!  w(1)=s1p0%coord(1)-s2p0%coord(1)
!  w(2)=s1p0%coord(2)-s2p0%coord(2)
!  d=u(1)*v(2)-u(2)*v(1)
!  s=v(1)*w(2)-v(2)*w(1)
!  t=u(1)*w(2)-u(2)*w(1)
!  IF(ABS(d) < EPSREAL) THEN
!    !Segments are collinear
!    IF(s /= 0._SRK .OR. t /= 0._SRK) THEN
!      p%dim=-3 !parallel lines
!    ELSE
!      p%dim=-2 !overlap
!    ENDIF
!  ELSE
!    s=s/d
!    t=t/d
!    IF(((0._SRK .APPROXLE. s) .AND. (s .APPROXLE. 1.0_SRK)) .AND. &
!        ((0._SRK .APPROXLE. t) .AND. (t .APPROXLE. 1.0_SRK))) THEN
!      !Success, intersection point was found.
!      p=s1p0
!      p%coord(1)=p%coord(1)+s*u(1)
!      p%coord(2)=p%coord(2)+s*u(2)
!    ELSE
!      !would intersect if segments were infinite. Still store how close it
!      !was though. this is useful for calling routines that might need to
!      !know that the point was close (i.e. Coarse ray trace)
!      p=s1p0
!      p%dim=-3
!      p%coord(1)=p%coord(1)+s*u(1)
!      p%coord(2)=p%coord(2)+s*u(2)
!    ENDIF
!  ENDIF
!ENDFUNCTION intersect_lines2D
!
!-------------------------------------------------------------------------------
!> @brief Determines whether a point is to the left of the line.
!> @param line the line object
!> @param pt the point object
!> @returns @c bool the boolean result of the operation
!>
!> Function is elemental so it can be used on an array of lines.
!ELEMENTAL FUNCTION pointIsLeft_QuadraticType(line,pt) RESULT(bool)
!  CLASS(QuadraticType),INTENT(IN) :: line
!  TYPE(PointType),INTENT(IN) :: pt
!  LOGICAL(SBK) :: bool
!  bool=.FALSE.
!  IF((pt%dim > 1) .AND. (line%getDim() > 1)) &
!      bool=(line%p2%coord(1)-line%p1%coord(1))*(pt%coord(2)-line%p1%coord(2))- &
!      (line%p2%coord(2)-line%p1%coord(2))*(pt%coord(1)-line%p1%coord(1)) > 0.0_SRK
!ENDFUNCTION pointIsLeft_QuadraticType
!
ENDMODULE Geom_Quadratic
