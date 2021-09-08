!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module defining a triangle for use in geometry.
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Triangle
USE IntrType
USE Geom_Points
USE Geom_Line

IMPLICIT NONE
PRIVATE
!
! List of Public items
PUBLIC :: Triangle_2D
PUBLIC :: area
PUBLIC :: interpolate
PUBLIC :: pointInside
!PUBLIC :: intersect

TYPE :: Triangle_2D
  TYPE(PointType) :: points(3)
!
!List of type bound procedures
  CONTAINS
    !> @copybrief Geom_Triangle::init_Triangle_2D
    !> @copydetails Geom_Triangle::init_Triangle_2D
    PROCEDURE,PASS :: set => init_Triangle_2D
    !> @copybrief Geom_Triangle::clear_Triangle_2D
    !> @copydetails Geom_Triangle::clear_Triangle_2D
    PROCEDURE,PASS :: clear => clear_Triangle_2D
ENDTYPE Triangle_2D

INTERFACE interpolate
  MODULE PROCEDURE interpolate_Triangle_2D
ENDINTERFACE interpolate

INTERFACE area
  MODULE PROCEDURE area_Triangle_2D
ENDINTERFACE area

INTERFACE pointInside
  MODULE PROCEDURE pointInside_Triangle_2D
ENDINTERFACE pointInside


!INTERFACE intersect
!  MODULE PROCEDURE intersectLine_Triangle_2D
!ENDINTERFACE intersect

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Constructor for Triangle_2D
!> @param p1 Start point of the tri
!> @param p2 End point of the tri
!> @param p3 An additional point on the tri
!>
ELEMENTAL SUBROUTINE init_Triangle_2D(tri,p1,p2,p3)
  CLASS(Triangle_2D),INTENT(INOUT) :: tri
  TYPE(PointType),INTENT(IN) :: p1,p2,p3
  CALL tri%clear()
  IF(p1%dim == p2%dim .AND. p2%dim == p3%dim .AND.  p1%dim == 2) THEN
    tri%points(1) = p1
    tri%points(2) = p2
    tri%points(3) = p3
  ENDIF
ENDSUBROUTINE init_Triangle_2D
!
!-------------------------------------------------------------------------------
!> @brief Clears and resets all values of the tri
!> @param tri the tri
ELEMENTAL SUBROUTINE clear_Triangle_2D(tri)
  CLASS(Triangle_2D),INTENT(INOUT) :: tri
  CALL tri%points(1)%clear()
  CALL tri%points(2)%clear()
  CALL tri%points(3)%clear()
ENDSUBROUTINE clear_Triangle_2D

ELEMENTAL FUNCTION interpolate_Triangle_2D(tri, r, s) RESULT(p)
  CLASS(Triangle_2D),INTENT(IN) :: tri
  REAL(SRK), INTENT(IN) :: r,s
  TYPE(PointType) :: p
  p = (1.0_SRK - r - s)*tri%points(1) + &
                      r*tri%points(2) + &
                      s*tri%points(3)
ENDFUNCTION interpolate_Triangle_2D

ELEMENTAL FUNCTION area_Triangle_2D(tri) RESULT(a)
  CLASS(Triangle_2D),INTENT(IN) :: tri
  TYPE(PointType) :: u,v
  REAL(SRK) :: a
  ! A = bh/2
  ! Let u⃗ = (v₂ - v₁), v⃗ = (v₃ - v₁)
  ! b = |u⃗|
  ! h = |sin(θ) v⃗|, where θ is the angle between u⃗ and v⃗
  ! u⃗ × v⃗ = |u⃗||v⃗| sin(θ), hence
  ! A = |u⃗ × v⃗|/2 = bh/2
  u = tri%points(2) - tri%points(1)
  v = tri%points(3) - tri%points(1)
  a = norm(cross(u, v))/2.0_SRK
ENDFUNCTION area_Triangle_2D

ELEMENTAL FUNCTION pointInside_Triangle_2D(tri, p) RESULT(bool)
  CLASS(Triangle_2D),INTENT(IN) :: tri
  TYPE(PointType),INTENT(IN) :: p
  REAL(SRK) :: A1, A2, A3, A
  LOGICAL(SBK) :: bool
  ! If the point is within the plane of the triangle, then the point is only within the triangle
  ! if the areas of the triangles formed by the point and each pair of two vertices sum to the 
  ! area of the triangle. Division by 2 is dropped, since it cancels
  ! If the vertices are A, B, and C, and the point is P, 
  ! P is inside ΔABC iff area(ΔABC) = area(ΔABP) + area(ΔBCP) + area(ΔACP)
  A1 = norm(cross((tri%points(1) - p), (tri%points(2) - p)))
  A2 = norm(cross((tri%points(2) - p), (tri%points(3) - p)))
  A3 = norm(cross((tri%points(3) - p), (tri%points(1) - p)))
  A  = norm(cross((tri%points(2) - tri%points(1)), (tri%points(3) - tri%points(1))))
  bool = (A1 + A2 + A3 .APPROXEQA. A)
ENDFUNCTION pointInside_Triangle_2D

!-------------------------------------------------------------------------------
!> @brief Finds the intersections between a line and the triratic segment (if it exists)
!> @param line line to test for intersection
!
! ELEMENTAL
!
!SUBROUTINE intersectLine_Triangle_2D(q, l, npoints, point1, point2)
!  CLASS(Triangle_2D),INTENT(IN) :: q
!  TYPE(LineType),INTENT(IN) :: l
!  INTEGER(SIK),INTENT(OUT) :: npoints
!  TYPE(PointType),INTENT(OUT) :: point1, point2
!  REAL(SRK) :: A, B, C, r, s, r1, r2, s1, s2
!  TYPE(PointType) :: A_vec, B_vec, C_vec, D_vec, E_vec, w_vec, l_s1, l_s2
!  npoints = 0
!  CALL point1%clear()
!  CALL point2%clear()
!  CALL point1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
!  CALL point2%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
!  D_vec = 2.0_SRK*(q%points(1) + q%points(2) - 2.0_SRK*q%points(3))
!  E_vec = 4.0_SRK*q%points(3) - 3.0_SRK*q%points(1) - q%points(2)
!  w_vec = l%p2 - l%p1
!  A_vec = cross(D_vec, w_vec)
!  B_vec = cross(E_vec, w_vec)
!  C_vec = cross((q%points(1) - l%p1), w_vec)
!  A = A_vec%coord(3)
!  B = B_vec%coord(3)
!  C = C_vec%coord(3)
!  IF (A < 1.0E-6_SRK) THEN
!    r = -C/B
!    point1 = interpolate(q,r)
!    s = (DOT_PRODUCT((point1 - l%p1), w_vec))/DOT_PRODUCT(w_vec, w_vec)
!    IF (0.0_SRK <= s .AND. s <= 1.0_SRK .AND. 0.0_SRK <= r .AND. r <= 1.0_SRK) THEN
!      npoints = 1
!    ENDIF
!  ELSE
!    r1 = (-B - SQRT(B**2 - 4*A*C))/(2*A)
!    r2 = (-B + SQRT(B**2 - 4*A*C))/(2*A)
!    point1 = interpolate(q,r1)
!    point2 = interpolate(q,r2)
!    s1 = (DOT_PRODUCT((point1 - l%p1), w_vec))/DOT_PRODUCT(w_vec, w_vec)
!    s2 = (DOT_PRODUCT((point2 - l%p1), w_vec))/DOT_PRODUCT(w_vec, w_vec)
!    l_s1 = (l%p2 - l%p1)*s1 + l%p1
!    l_s2 = (l%p2 - l%p1)*s2 + l%p1
!    ! Check points to see if they are valid intersections
!    IF (0.0_SRK <= s1 .AND. s1 <= 1.0_SRK .AND. &
!        0.0_SRK <= r1 .AND. r1 <= 1.0_SRK .AND. &
!        (point1 .APPROXEQA. l_s1)) THEN
!      npoints = 1
!    ENDIF
!    IF (0.0_SRK <= s2 .AND. s2 <= 1.0_SRK .AND. &
!        0.0_SRK <= r2 .AND. r2 <= 1.0_SRK .AND. &
!        (point2 .APPROXEQA. l_s2)) THEN
!      npoints = npoints + 1
!      IF (npoints == 1) THEN
!        point1 = point2
!      ENDIF
!    ENDIF
!  ENDIF
!  CALL A_vec%clear()
!  CALL B_vec%clear()
!  CALL C_vec%clear()
!  CALL D_vec%clear()
!  CALL E_vec%clear()
!  CALL w_vec%clear()
!  CALL l_s1%clear()
!  CALL l_s2%clear()
!ENDSUBROUTINE intersectLine_Triangle_2D
ENDMODULE Geom_Triangle
