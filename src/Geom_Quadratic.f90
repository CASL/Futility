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
USE ExceptionHandler
USE IntrType
USE Geom_Points
USE Geom_Line

IMPLICIT NONE
PRIVATE

! List of Public items
PUBLIC :: QuadraticType

CHARACTER(LEN=*),PARAMETER :: modName='Geom_Quadratic'
TYPE(ExceptionHandlerType),SAVE :: eGeomQuad

TYPE :: QuadraticType
  REAL(SRK) :: a=0.0, b=0.0
  TYPE(PointType) :: u, y
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
!    !> @copybrief Geom_Quadratic::intersectLine_QuadraticType
!    !> @copydetails Geom_Quadratic::intersectLine_QuadraticType
!    PROCEDURE,PASS :: intersectLine => intersectLine_QuadraticType
    !> @copybrief Geom_Quadratic::pointIsLeft_QuadraticType
    !> @copydetails Geom_Quadratic::pointIsLeft_QuadraticType
    PROCEDURE,PASS :: pointIsLeft => pointIsLeft_QuadraticType
ENDTYPE QuadraticType

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Constructor for QuadraticType
!> @param p1 Start point of the q
!> @param p2 End point of the q
!> @param p3 An additional point on the q
!> @returns line the QuadraticType object
!>
SUBROUTINE init_QuadraticType(q,p1,p2,p3)
  CLASS(QuadraticType),INTENT(INOUT) :: q
  TYPE(PointType),INTENT(IN) :: p1,p2,p3
  TYPE(PointType) :: u, v, v_x_u, v_x_u_x_u, p0, y_x_u
  INTEGER(SIK) :: d
  ! Using q(1) = x⃗₂ gives b = -a|u⃗|.
  ! Using q(t₃) = x⃗₃, the following steps may be used to derive a
  !   1) v⃗ = x⃗₃ - x⃗₁
  !   2) b = -a|u⃗|
  !   3) × u⃗ both sides, and u⃗ × u⃗ = 0⃗
  !   4) |t₃u⃗| = u⃗ ⋅v⃗/|u⃗|
  !   5) |u⃗|² = u⃗ ⋅u⃗
  !   6) v⃗ × u⃗ = -v⃗ × u⃗
  !   the result:
  !   
  !             (u⃗ ⋅ u⃗) (v⃗ × u⃗) ⋅ (v⃗ × u⃗)
  ! a = -------------------------------------------
  !     (u⃗ ⋅ v⃗)[(u⃗ ⋅ v⃗) - (u⃗ ⋅ u⃗)](ŷ × u⃗) ⋅ (v⃗ × u⃗)
  !   
  ! We can construct ŷ with
  !   
  !      -(v⃗ × u⃗) × u⃗
  ! ŷ =  -------------
  !      |(v⃗ × u⃗) × u⃗|
  IF((p1%dim /= 3) .OR. (p2%dim /= 3) .OR. (p1%dim /= 3)) THEN
    CALL eGeomQuad%raiseError(modName//'::init_QuadraticType'// & 
      ' Quadratic initialized with 2D point, but requires 3D points.')
  ENDIF
  CALL q%clear()
  q%points = (/ p1, p2, p3/)
  d = p1%dim
  CALL p0%init(DIM=3, X=0.0_SRK, Y=0.0_SRK, Z=0.0_SRK)
  u = p2 - p1
  v = p3 - p1
  v_x_u = cross(v,u)
  q%u = u
  IF (ALL(v_x_u%coord .APPROXEQ. 0.0_SRK)) THEN
    q%a = 0.0_SRK
    q%b = 0.0_SRK
    q%y = p0
  ELSE
    v_x_u_x_u = cross(v_x_u, u)
    q%y = v_x_u_x_u
    q%y%coord = -q%y%coord/norm(v_x_u_x_u)
    y_x_u = cross(q%y, u)
    q%a = DOT_PRODUCT(u%coord, u%coord)*DOT_PRODUCT(v_x_u%coord, v_x_u%coord)/(&
      DOT_PRODUCT(u%coord,v%coord)*&
      (DOT_PRODUCT(u%coord,v%coord) - DOT_PRODUCT(u%coord, u%coord))*&
      DOT_PRODUCT(y_x_u%coord, v_x_u%coord))
    q%b = -norm(u)*q%a
  ENDIF
ENDSUBROUTINE init_QuadraticType
!
!-------------------------------------------------------------------------------
!> @brief Clears and resets all values of the q
!> @param q the q
ELEMENTAL SUBROUTINE clear_QuadraticType(q)
  CLASS(QuadraticType),INTENT(INOUT) :: q
  INTEGER(SIK) :: i
  DO i = 1,3
    CALL q%points(i)%clear()
  ENDDO
  CALL q%u%clear()
  CALL q%y%clear()
  q%a = 0.0
  q%b = 0.0
ENDSUBROUTINE clear_QuadraticType

!
!-------------------------------------------------------------------------------
!> @brief Computes the area of the quadratic q
!> @param q the q
ELEMENTAL FUNCTION area_QuadraticType(q) RESULT(area)
  CLASS(QuadraticType),INTENT(IN) :: q
  REAL(SRK) :: d, area
  d = distance(q%points(1), q%points(2))
  area = q%a*d**3/3.0_SRK + q%b*d**2/2.0_SRK
ENDFUNCTION area_QuadraticType
!!!
!!!-------------------------------------------------------------------------------
!!!> @brief Finds the intersection between a line and the q (if it exists)
!!!> @param line line to test for intersection
!!!> @returns points the points of intersection
!!!> @note a return code is assigned to p%dim indicating the type of
!!!> intersection.
!!!>   -3: there is no intersection (disjoint)
!!!>   -2: the line segment and q overlap
!!!>   -1: problem with dummy arguments passed to routine
!!!>    1: success; an 1 intersection point was found
!!!>    2: success; 2 intersection points were found
!!SUBROUTINE intersectLine_QuadraticType(q, line, points)
!!  CLASS(QuadraticType),INTENT(IN) :: q
!!  TYPE(LineType),INTENT(IN) :: line
!!  TYPE(PointType),INTENT(INOUT) :: points(2)
!!  TYPE(LineType) :: line_s
!!  TYPE(PointType) :: p0
!!  REAL(SDK) :: A,B,C,m,d,rotation_mat(2,2), xdomain, x, disc, xmin, xmax
!!  REAL(SDK), PARAMETER :: vertical = 1.0E11_SDK
!!
!!!  WRITE(*,*) "intersectLine_QuadraticType"
!!!  WRITE(*,*) "My points:"
!!!  WRITE(*,*) q%points(1)%coord
!!!  WRITE(*,*) q%points(2)%coord
!!!  WRITE(*,*) q%points(3)%coord
!!  IF(.NOT.ALLOCATED(points(1)%coord))THEN
!!   CALL points(1)%init(DIM=2,X=-HUGE(0.0_SRK),Y=-HUGE(0.0_SRK))
!!   CALL points(2)%init(DIM=2,X=-HUGE(0.0_SRK),Y=-HUGE(0.0_SRK))
!!  ENDIF
!!  points(:)%dim = -1
!!  IF(line%p1%dim == line%p2%dim .AND. line%p1%dim == 2) THEN
!!    ! Transform the line to the coordinates of the q
!!    ! Create a copy of the line.
!!    line_s%p1 = line%p1
!!    line_s%p2 = line%p2
!!!    WRITE(*,*) "Line coords pre-transform: "
!!!    WRITE(*,*) "  p1: ", line_s%p1%coord
!!!    WRITE(*,*) "  p2: ", line_s%p2%coord
!!!    WRITE(*,*) "shift: ", q%shift_x, q%shift_y
!!!    WRITE(*,*) "theta/pi: ", q%theta/PI 
!!
!!    ! Shift the line to the origin
!!    CALL p0%init(DIM=2, X=q%shift_x, Y=q%shift_y)
!!    line_s%p1 = line_s%p1 - p0
!!    line_s%p2 = line_s%p2 - p0
!!    ! Rotate the line
!!    rotation_mat(1,:) = (/COS(q%theta), SIN(q%theta)/)
!!    rotation_mat(2,:) = (/-SIN(q%theta), COS(q%theta)/)
!!    line_s%p1%coord = MATMUL(rotation_mat, line_s%p1%coord)
!!    line_s%p2%coord = MATMUL(rotation_mat, line_s%p2%coord)
!!!    WRITE(*,*) "Line coords transformed: "
!!!    WRITE(*,*) "  p1: ", line_s%p1%coord
!!!    WRITE(*,*) "  p2: ", line_s%p2%coord
!!    ! Get the
!!    ! line: y = mx + d
!!    ! quad: y = ax^2 + bx
!!    ! solve for the x which satisfies both conditions
!!    ! mx + d = ax^2 + bx
!!    ! ax^2 + (b-m)x - d
!!    ! A = a, B = b-m, C = -d
!!    ! x = (-B pm sqrt(B^2 - 4AC))/2A
!!    ! If B^2 < 4AC, there is no real solution
!!    ! If B^2 == 4AC, there is one solution
!!    ! If B^2 > 4AC, there are two solutions
!!    ! These are solutions for the line, not the line segment.
!!    ! x_soln must be in [line%p1%x, lines%p2%x], assuming p1%x <= p2%x
!!    !
!!    ! This is numerical instability hell. There has to be a better way
!!    m = (line_s%p2%coord(2) - line_s%p1%coord(2))/(line_s%p2%coord(1) - line_s%p1%coord(1))
!!!    WRITE(*,*) "m: ", m
!!    d = line_s%p1%coord(2) - m*line_s%p1%coord(1)
!!!    WRITE(*,*) "d: ", d
!!    A = q%a
!!!    WRITE(*,*) "a, b: ", q%a, q%b
!!    B = q%b - m
!!    C = -d
!!!    WRITE(*,*) " A B C", A, B, C
!!
!!    ! If B^2 < 4AC, there is no real solution
!!    IF(B*B < 4*A*C)THEN
!!      points(:)%dim = -3
!!      RETURN
!!    ENDIF
!!
!!    ! Valid x for the q is from 0.0 to the distance from q p1 to p2
!!    xdomain = distance(q%points(1), q%points(2))
!!    ! Valid x for the line is from the min to max x of the segment
!!    xmin = MIN(line_s%p1%coord(1), line_s%p2%coord(1)) ! valid domain of the seg
!!    xmax = MAX(line_s%p1%coord(1), line_s%p2%coord(1))
!!    ! If vertical line, just check if the point x are within the valid domain.
!!    ! If so, take the intersection there.
!!    IF(ABS(m) > vertical) THEN
!!!      WRITE(*,*) "vertical"
!!      x = line_s%p1%coord(1)
!!      IF( 0.0 <= x .AND. x <= xdomain)THEN
!!        points(:)%dim = 1
!!        points(1)%coord(1) = x
!!        points(1)%coord(2) = q%a*x**2 + q%b*x
!!      ELSE
!!        points(:)%dim = -3
!!      ENDIF
!!    ELSE ! non-vertical
!!!      WRITE(*,*) "not vertical"
!!      disc = SQRT(B*B - 4*A*C)
!!      x = (-B + disc)/(2*A)
!!!      WRITE(*,*) "x1: ", x
!!      IF(0.0 <= x .AND. x <= xdomain .AND. xmin <= x .AND. x <= xmax) THEN
!!!        WRITE(*,*) "valid x1"
!!        points(:)%dim = 1
!!        points(1)%coord(1) = x
!!        points(1)%coord(2) = q%a*x**2 + q%b*x
!!      ELSE
!!!        WRITE(*,*) "invalid x1"
!!        points(:)%dim = -3
!!      ENDIF
!!      ! If the possibility of a second solution exists, check it
!!      IF( ABS(B*B - 4*A*C) > 1.0E-6) THEN
!!        x = (-B - disc)/(2*A)
!!!        WRITE(*,*) "x2: ", x
!!        IF(0.0 <= x .AND. x <= xdomain .AND. xmin <= x .AND. x <= xmax) THEN
!!!          WRITE(*,*) "valid x2"
!!          IF(points(1)%dim == 1)THEN ! second intersection
!!            points(:)%dim = 2
!!            points(2)%coord(1) = x
!!            points(2)%coord(2) = q%a*x**2 + q%b*x
!!          ELSE
!!            points(:)%dim = 1
!!            points(1)%coord(1) = x
!!            points(1)%coord(2) = q%a*x**2 + q%b*x
!!          ENDIF
!!        ENDIF
!!      ENDIF
!!    ENDIF
!!
!!    ! If intersection was found, transfrom back to original coords
!!    IF(points(1)%dim > 0)THEN
!!!      WRITE(*,*) "Intersection found"
!!!      WRITE(*,*) "Number of intersections: ", points(1)%dim
!!!      WRITE(*,*) "Coordinates prior to transform:"
!!!      IF(points(1)%dim == 1) THEN
!!!        WRITE(*,*) "x1, y1 ", points(1)%coord
!!!      ELSE
!!!        WRITE(*,*) "x1, y1 ", points(1)%coord
!!!        WRITE(*,*) "x2, y2 ", points(2)%coord
!!!      ENDIF
!!      rotation_mat(1,:) = (/COS(q%theta), -SIN(q%theta)/)
!!      rotation_mat(2,:) = (/SIN(q%theta), COS(q%theta)/)
!!      IF(points(1)%dim == 2) THEN
!!        points(2)%coord = MATMUL(rotation_mat, points(2)%coord)
!!        points(2)%coord(1) = points(2)%coord(1) + q%shift_x
!!        points(2)%coord(2) = points(2)%coord(2) + q%shift_y
!!      ENDIF
!!      points(1)%coord = MATMUL(rotation_mat, points(1)%coord)
!!      points(1)%coord(1) = points(1)%coord(1) + q%shift_x
!!      points(1)%coord(2) = points(1)%coord(2) + q%shift_y
!!!      WRITE(*,*) "Coordinates post transform:"
!!!      IF(points(1)%dim == 1) THEN
!!!        WRITE(*,*) "x1, y1 ", points(1)%coord
!!!      ELSE
!!!        WRITE(*,*) "x1, y1 ", points(1)%coord
!!!        WRITE(*,*) "x2, y2 ", points(2)%coord
!!!      ENDIF
!!    ENDIF
!!  ENDIF
!!ENDSUBROUTINE intersectLine_QuadraticType
!!!
!-------------------------------------------------------------------------------
!> @brief Determines whether a point is to the left of q
!> @param line the line object
!> @param pt the point object
!> @returns @c bool the boolean result of the operation
!>
! ELEMENTAL
FUNCTION pointIsLeft_QuadraticType(q,p,n_in) RESULT(bool)
  CLASS(QuadraticType),INTENT(IN) :: q
  TYPE(PointType),INTENT(IN) :: p
  TYPE(PointType),INTENT(IN), OPTIONAL :: n_in
  LOGICAL(SBK) :: bool
  TYPE(PointType) :: w, u, n
  TYPE(LineType) :: l
  REAL(SRK) :: detinv, A(3,3), B(3,3), x(3), t,s,v,smax
  ! Let w⃗ = p - q.x⃗₁, u⃗ = q.x⃗₂ - q.x⃗₁
  ! If p is in the plane of q, then w⃗ is a linear combination of u⃗ and ŷ. 
  ! w⃗ = tu⃗ + sŷ + vn̂, and v=0. Here we include n̂ to make a square system.
  ! Therefore, if A = [u⃗ ŷ n̂] and x = [t; s; v], then Ax=w⃗. 
  ! A is invertible since {u⃗, ŷ, n̂} are a basis for R³.
  ! If p is actually in the plane of q, then v = 0.
  ! If t ∉ (0, 1), then we can simply use the line is_left check.
  ! If t ∈ (0, 1), then we need to see if the point is within the quad area.
  ! If If 0 ≤ s ≤ a|u⃗/2|² + b|u⃗/2|, then the point is within the quad area.
  ! If the point is within the quad area, we need to reverse the linear result.
  IF(PRESENT(n_in))THEN
    n = n_in
  ELSE
    CALL n%init(DIM=3, COORD=(/0.0_SRK, 0.0_SRK, 1.0_SRK/))
  ENDIF
  w = p - q%points(1) 
  u = q%points(2) - q%points(1)
  A(1, :) = u%coord  
  A(2, :) = q%y%coord 
  A(3, :) = n%coord  

  ! Calculate the inverse determinant of the matrix
  detinv = 1/(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
            - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
            + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

  ! Calculate the inverse of the matrix
  B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
  B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
  B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
  B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
  B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
  B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
  B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
  B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
  B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))

  x = MATMUL(B, w%coord)
  t = x(1); s = abs(x(2)); v = x(3)
  CALL l%set(q%points(1), q%points(2))
  bool = l%pointIsLeft(p)
  smax = 0.25_SRK*q%a*norm(u)**2 + 0.5*q%b*norm(u)
  IF( (t <= 0) .OR. (1 <= t) .OR. (s == 0) .OR. (smax <= s) )THEN
    ! Point is outside quad area, just use line isLeft value
    bool = bool
  ELSE
    ! Point is inside quad area, return opposite of isLeft
    bool = .NOT.bool
  ENDIF
ENDFUNCTION pointIsLeft_QuadraticType
ENDMODULE Geom_Quadratic
