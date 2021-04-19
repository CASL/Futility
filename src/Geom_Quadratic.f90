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
USE Geom_Line

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
    !> @copybrief Geom_Quadratic::area_QuadraticType
    !> @copydetails Geom_Quadratic::area_QuadraticType
    PROCEDURE,PASS :: area => area_QuadraticType
    !> @copybrief Geom_Quadratic::intersectLine_QuadraticType
    !> @copydetails Geom_Quadratic::intersectLine_QuadraticType
    PROCEDURE,PASS :: intersectLine => intersectLine_QuadraticType
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
!> @brief Clears and resets all values of the arc
!> @param arc the arc
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
!> @brief Computes the area of the quadratic arc
!> @param arc the arc
ELEMENTAL FUNCTION area_QuadraticType(arc) RESULT(area)
  CLASS(QuadraticType),INTENT(IN) :: arc
  REAL(SRK) :: d, area
  d = distance(arc%points(1), arc%points(2))
  area = arc%a*d**3/3.0_SRK + arc%b*d**2/2.0_SRK
ENDFUNCTION area_QuadraticType
!
!-------------------------------------------------------------------------------
!> @brief Finds the intersection between a line and the arc (if it exists)
!> @param line line to test for intersection
!> @returns points the points of intersection
!> @note If the line and quadratic arc are collinear, throws an error
PURE SUBROUTINE intersectLine_QuadraticType(arc, line, points)
  CLASS(QuadraticType),INTENT(IN) :: arc
  TYPE(LineType),INTENT(IN) :: line
  TYPE(PointType),ALLOCATABLE,INTENT(INOUT) :: points(:)

ENDSUBROUTINE intersectLine_QuadraticType
ENDMODULE Geom_Quadratic
