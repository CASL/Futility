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
MODULE Geom_QuadraticSegment
USE IntrType
USE Geom_Points
USE Geom_Line

IMPLICIT NONE
PRIVATE
!
! List of Public items
PUBLIC :: QuadraticSegment_2D
PUBLIC :: interpolate

TYPE :: QuadraticSegment_2D
  TYPE(PointType) :: points(3)
!
!List of type bound procedures
  CONTAINS
    !> @copybrief Geom_QuadraticSegment::init_QuadraticSegment_2D
    !> @copydetails Geom_QuadraticSegment::init_QuadraticSegment_2D
    PROCEDURE,PASS :: set => init_QuadraticSegment_2D
    !> @copybrief Geom_QuadraticSegment::clear_QuadraticSegment_2D
    !> @copydetails Geom_QuadraticSegment::clear_QuadraticSegment_2D
    PROCEDURE,PASS :: clear => clear_QuadraticSegment_2D
    !> @copybrief Geom_QuadraticSegment::intersectLine_QuadraticSegment_2D
    !> @copydetails Geom_QuadraticSegment::intersectLine_QuadraticSegment_2D
!    PROCEDURE,PASS :: intersectLine => intersectLine_QuadraticSegment_2D
ENDTYPE QuadraticSegment_2D

INTERFACE interpolate
  MODULE PROCEDURE interpolate_QuadraticSegment_2D
ENDINTERFACE interpolate




!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Constructor for QuadraticSegment_2D
!> @param p1 Start point of the quad
!> @param p2 End point of the quad
!> @param p3 An additional point on the quad
!>
ELEMENTAL SUBROUTINE init_QuadraticSegment_2D(quad,p1,p2,p3)
  CLASS(QuadraticSegment_2D),INTENT(INOUT) :: quad
  TYPE(PointType),INTENT(IN) :: p1,p2,p3
  CALL quad%clear()
  IF(p1%dim == p2%dim .AND. p2%dim == p3%dim .AND.  p1%dim == 2) THEN
    quad%points(1) = p1
    quad%points(2) = p2
    quad%points(3) = p3
  ENDIF
ENDSUBROUTINE init_QuadraticSegment_2D
!
!-------------------------------------------------------------------------------
!> @brief Clears and resets all values of the quad
!> @param quad the quad
ELEMENTAL SUBROUTINE clear_QuadraticSegment_2D(quad)
  CLASS(QuadraticSegment_2D),INTENT(INOUT) :: quad
  CALL quad%points(1)%clear()
  CALL quad%points(2)%clear()
  CALL quad%points(3)%clear()
ENDSUBROUTINE clear_QuadraticSegment_2D

ELEMENTAL FUNCTION interpolate_QuadraticSegment_2D(q, r, s) RESULT(p)
  CLASS(QuadraticSegment_2D),INTENT(IN) :: q
  REAL(SRK), INTENT(IN) :: r,s
  TYPE(PointType) :: p
  p = (4*r - 3)*q%points(1) + (4*r - 1)*q%points(2) + (4 - 8*r)*q%points(3)
ENDFUNCTION interpolate_QuadraticSegment_2D


















!!-------------------------------------------------------------------------------
!!> @brief Finds the intersection between a line and the quad (if it exists)
!!> @param line line to test for intersection
!!> @returns points the points of intersection
!!> @note a return code is assigned to p%dim indicating the type of
!!> intersection.
!!>   -3: there is no intersection (disjoint)
!!>   -2: the line segment and quad overlap
!!>   -1: problem with dummy arguments passed to routine
!!>    1: success; an 1 intersection point was found
!!>    2: success; 2 intersection points were found
!SUBROUTINE intersectLine_QuadraticSegment_2D(quad, line, points)
!  CLASS(QuadraticSegment_2D),INTENT(IN) :: quad
!  TYPE(LineType),INTENT(IN) :: line
!  TYPE(PointType),INTENT(INOUT) :: points(2)
!  TYPE(LineType) :: line_s
!  TYPE(PointType) :: p0
!  REAL(SDK) :: A,B,C,m,d,rotation_mat(2,2), xdomain, x, disc, xmin, xmax
!  REAL(SDK), PARAMETER :: vertical = 1.0E11_SDK
!
!!  WRITE(*,*) "intersectLine_QuadraticSegment_2D"
!!  WRITE(*,*) "My points:"
!!  WRITE(*,*) quad%points(1)%coord
!!  WRITE(*,*) quad%points(2)%coord
!!  WRITE(*,*) quad%points(3)%coord
!  IF(.NOT.ALLOCATED(points(1)%coord))THEN
!   CALL points(1)%init(DIM=2,X=-HUGE(0.0_SRK),Y=-HUGE(0.0_SRK))
!   CALL points(2)%init(DIM=2,X=-HUGE(0.0_SRK),Y=-HUGE(0.0_SRK))
!  ENDIF
!  points(:)%dim = -1
!  IF(line%p1%dim == line%p2%dim .AND. line%p1%dim == 2) THEN
!    ! Transform the line to the coordinates of the quad
!    ! Create a copy of the line.
!    line_s%p1 = line%p1
!    line_s%p2 = line%p2
!!    WRITE(*,*) "Line coords pre-transform: "
!!    WRITE(*,*) "  p1: ", line_s%p1%coord
!!    WRITE(*,*) "  p2: ", line_s%p2%coord
!!    WRITE(*,*) "shift: ", quad%shift_x, quad%shift_y
!!    WRITE(*,*) "theta/pi: ", quad%theta/PI 
!
!    ! Shift the line to the origin
!    CALL p0%init(DIM=2, X=quad%shift_x, Y=quad%shift_y)
!    line_s%p1 = line_s%p1 - p0
!    line_s%p2 = line_s%p2 - p0
!    ! Rotate the line
!    rotation_mat(1,:) = (/COS(quad%theta), SIN(quad%theta)/)
!    rotation_mat(2,:) = (/-SIN(quad%theta), COS(quad%theta)/)
!    line_s%p1%coord = MATMUL(rotation_mat, line_s%p1%coord)
!    line_s%p2%coord = MATMUL(rotation_mat, line_s%p2%coord)
!!    WRITE(*,*) "Line coords transformed: "
!!    WRITE(*,*) "  p1: ", line_s%p1%coord
!!    WRITE(*,*) "  p2: ", line_s%p2%coord
!    ! Get the
!    ! line: y = mx + d
!    ! quad: y = ax^2 + bx
!    ! solve for the x which satisfies both conditions
!    ! mx + d = ax^2 + bx
!    ! ax^2 + (b-m)x - d
!    ! A = a, B = b-m, C = -d
!    ! x = (-B pm sqrt(B^2 - 4AC))/2A
!    ! If B^2 < 4AC, there is no real solution
!    ! If B^2 == 4AC, there is one solution
!    ! If B^2 > 4AC, there are two solutions
!    ! These are solutions for the line, not the line segment.
!    ! x_soln must be in [line%p1%x, lines%p2%x], assuming p1%x <= p2%x
!    !
!    ! This is numerical instability hell. There has to be a better way
!    m = (line_s%p2%coord(2) - line_s%p1%coord(2))/(line_s%p2%coord(1) - line_s%p1%coord(1))
!!    WRITE(*,*) "m: ", m
!    d = line_s%p1%coord(2) - m*line_s%p1%coord(1)
!!    WRITE(*,*) "d: ", d
!    A = quad%a
!!    WRITE(*,*) "a, b: ", quad%a, quad%b
!    B = quad%b - m
!    C = -d
!!    WRITE(*,*) " A B C", A, B, C
!
!    ! If B^2 < 4AC, there is no real solution
!    IF(B*B < 4*A*C)THEN
!      points(:)%dim = -3
!      RETURN
!    ENDIF
!
!    ! Valid x for the quad is from 0.0 to the distance from quad p1 to p2
!    xdomain = distance(quad%points(1), quad%points(2))
!    ! Valid x for the line is from the min to max x of the segment
!    xmin = MIN(line_s%p1%coord(1), line_s%p2%coord(1)) ! valid domain of the seg
!    xmax = MAX(line_s%p1%coord(1), line_s%p2%coord(1))
!    ! If vertical line, just check if the point x are within the valid domain.
!    ! If so, take the intersection there.
!    IF(ABS(m) > vertical) THEN
!!      WRITE(*,*) "vertical"
!      x = line_s%p1%coord(1)
!      IF( 0.0 <= x .AND. x <= xdomain)THEN
!        points(:)%dim = 1
!        points(1)%coord(1) = x
!        points(1)%coord(2) = quad%a*x**2 + quad%b*x
!      ELSE
!        points(:)%dim = -3
!      ENDIF
!    ELSE ! non-vertical
!!      WRITE(*,*) "not vertical"
!      disc = SQRT(B*B - 4*A*C)
!      x = (-B + disc)/(2*A)
!!      WRITE(*,*) "x1: ", x
!      IF(0.0 <= x .AND. x <= xdomain .AND. xmin <= x .AND. x <= xmax) THEN
!!        WRITE(*,*) "valid x1"
!        points(:)%dim = 1
!        points(1)%coord(1) = x
!        points(1)%coord(2) = quad%a*x**2 + quad%b*x
!      ELSE
!!        WRITE(*,*) "invalid x1"
!        points(:)%dim = -3
!      ENDIF
!      ! If the possibility of a second solution exists, check it
!      IF( ABS(B*B - 4*A*C) > 1.0E-6) THEN
!        x = (-B - disc)/(2*A)
!!        WRITE(*,*) "x2: ", x
!        IF(0.0 <= x .AND. x <= xdomain .AND. xmin <= x .AND. x <= xmax) THEN
!!          WRITE(*,*) "valid x2"
!          IF(points(1)%dim == 1)THEN ! second intersection
!            points(:)%dim = 2
!            points(2)%coord(1) = x
!            points(2)%coord(2) = quad%a*x**2 + quad%b*x
!          ELSE
!            points(:)%dim = 1
!            points(1)%coord(1) = x
!            points(1)%coord(2) = quad%a*x**2 + quad%b*x
!          ENDIF
!        ENDIF
!      ENDIF
!    ENDIF
!
!    ! If intersection was found, transfrom back to original coords
!    IF(points(1)%dim > 0)THEN
!!      WRITE(*,*) "Intersection found"
!!      WRITE(*,*) "Number of intersections: ", points(1)%dim
!!      WRITE(*,*) "Coordinates prior to transform:"
!!      IF(points(1)%dim == 1) THEN
!!        WRITE(*,*) "x1, y1 ", points(1)%coord
!!      ELSE
!!        WRITE(*,*) "x1, y1 ", points(1)%coord
!!        WRITE(*,*) "x2, y2 ", points(2)%coord
!!      ENDIF
!      rotation_mat(1,:) = (/COS(quad%theta), -SIN(quad%theta)/)
!      rotation_mat(2,:) = (/SIN(quad%theta), COS(quad%theta)/)
!      IF(points(1)%dim == 2) THEN
!        points(2)%coord = MATMUL(rotation_mat, points(2)%coord)
!        points(2)%coord(1) = points(2)%coord(1) + quad%shift_x
!        points(2)%coord(2) = points(2)%coord(2) + quad%shift_y
!      ENDIF
!      points(1)%coord = MATMUL(rotation_mat, points(1)%coord)
!      points(1)%coord(1) = points(1)%coord(1) + quad%shift_x
!      points(1)%coord(2) = points(1)%coord(2) + quad%shift_y
!!      WRITE(*,*) "Coordinates post transform:"
!!      IF(points(1)%dim == 1) THEN
!!        WRITE(*,*) "x1, y1 ", points(1)%coord
!!      ELSE
!!        WRITE(*,*) "x1, y1 ", points(1)%coord
!!        WRITE(*,*) "x2, y2 ", points(2)%coord
!!      ENDIF
!    ENDIF
!  ENDIF
!ENDSUBROUTINE intersectLine_QuadraticSegment_2D
ENDMODULE Geom_QuadraticSegment
