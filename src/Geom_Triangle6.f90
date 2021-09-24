!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module defining a triangle with quadratic edges for 
!> use in geometry.
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Triangle6
USE IntrType
USE Geom_Points
USE Geom_Line
USE Geom_QuadraticSegment

IMPLICIT NONE
PRIVATE
!
! List of Public items
PUBLIC :: Triangle6_2D
PUBLIC :: interpolate
PUBLIC :: derivative
PUBLIC :: area
PUBLIC :: real_to_parametric
PUBLIC :: pointInside
PUBLIC :: intersect

TYPE :: Triangle6_2D
  ! The points are assumed to be ordered as follows
  ! p₁ = vertex A
  ! p₂ = vertex B
  ! p₃ = vertex C
  ! p₄ = point on the quadratic segment from A to B
  ! p₅ = point on the quadratic segment from B to C
  ! p₆ = point on the quadratic segment from C to A
  TYPE(PointType) :: points(6)
!
!List of type bound procedures
  CONTAINS
    !> @copybrief Geom_Triangle6::init_Triangle6_2D
    !> @copydetails Geom_Triangle6::init_Triangle6_2D
    PROCEDURE,PASS :: set => init_Triangle6_2D
    !> @copybrief Geom_Triangle6::clear_Triangle6_2D
    !> @copydetails Geom_Triangle6::clear_Triangle6_2D
    PROCEDURE,PASS :: clear => clear_Triangle6_2D
ENDTYPE Triangle6_2D

INTERFACE interpolate
  MODULE PROCEDURE interpolate_Triangle6_2D
ENDINTERFACE interpolate

INTERFACE derivative
  MODULE PROCEDURE derivative_Triangle6_2D
ENDINTERFACE derivative

INTERFACE area
  MODULE PROCEDURE area_Triangle6_2D
ENDINTERFACE area

INTERFACE real_to_parametric
  MODULE PROCEDURE real_to_parametric_Triangle6_2D
ENDINTERFACE real_to_parametric

INTERFACE pointInside
  MODULE PROCEDURE pointInside_Triangle6_2D
ENDINTERFACE pointInside

INTERFACE intersect
  MODULE PROCEDURE intersectLine_Triangle6_2D
ENDINTERFACE intersect

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Constructor for Triangle6_2D
!> @param p1 Start point of the tri
!> @param p2 End point of the tri
!> @param p3 An additional point on the tri
!>
PURE SUBROUTINE init_Triangle6_2D(tri,points)
  CLASS(Triangle6_2D),INTENT(INOUT) :: tri
  TYPE(PointType),INTENT(IN) :: points(6)
  CALL tri%clear()
  IF(points(1)%dim == points(2)%dim .AND. & 
     points(2)%dim == points(3)%dim .AND. & 
     points(3)%dim == points(4)%dim .AND. & 
     points(4)%dim == points(5)%dim .AND. & 
     points(5)%dim == points(6)%dim .AND. & 
     points(6)%dim == points(1)%dim .AND. & 
     points(1)%dim == 2) THEN
    tri%points(1) = points(1) 
    tri%points(2) = points(2)
    tri%points(3) = points(3)
    tri%points(4) = points(4)
    tri%points(5) = points(5)
    tri%points(6) = points(6)
  ENDIF
ENDSUBROUTINE init_Triangle6_2D
!
!-------------------------------------------------------------------------------
!> @brief Clears and resets all values of the tri
!> @param tri the tri
PURE SUBROUTINE clear_Triangle6_2D(tri)
  CLASS(Triangle6_2D),INTENT(INOUT) :: tri
  CALL tri%points(1)%clear()
  CALL tri%points(2)%clear()
  CALL tri%points(3)%clear()
  CALL tri%points(4)%clear()
  CALL tri%points(5)%clear()
  CALL tri%points(6)%clear()
ENDSUBROUTINE clear_Triangle6_2D

PURE FUNCTION interpolate_Triangle6_2D(tri, r, s) RESULT(p)
  CLASS(Triangle6_2D),INTENT(IN) :: tri
  REAL(SRK), INTENT(IN) :: r,s
  TYPE(PointType) :: p
  p = (1.0_SRK - r - s)*(2.0_SRK*(1.0_SRK - r - s) - 1.0_SRK)*tri%points(1) +& 
                                        r*(2.0_SRK*r-1.0_SRK)*tri%points(2) +&
                                        s*(2.0_SRK*s-1.0_SRK)*tri%points(3) +&
                                  4.0_SRK*r*(1.0_SRK - r - s)*tri%points(4) +&
                                                  4.0_SRK*r*s*tri%points(5) +&
                                  4.0_SRK*s*(1.0_SRK - r - s)*tri%points(6)

ENDFUNCTION interpolate_Triangle6_2D

PURE SUBROUTINE derivative_Triangle6_2D(tri, r, s, dr, ds)
  CLASS(Triangle6_2D),INTENT(IN) :: tri
  REAL(SRK), INTENT(IN) :: r,s
  TYPE(PointType), INTENT(INOUT) :: dr, ds
  ! ∂T/∂r = (4r + 4s - 3)p1 +
  !              (4r - 1)p2 +
  !         4(1 - 2r - s)p4 +
  !                  (4s)p5 +
  !                 (-4s)p6

  ! ∂T/∂s = (4r + 4s - 3)p1 +
  !              (4s - 1)p3 +
  !                 (-4r)p4 +
  !                  (4r)p5 +
  !         4(1 - r - 2s)p6

  dr = (4.0_SRK*r + 4.0_SRK*s - 3.0_SRK)*tri%points(1) +& 
                   (4.0_SRK*r - 1.0_SRK)*tri%points(2) +&
       4.0_SRK*(1.0_SRK - 2.0_SRK*r - s)*tri%points(4) +&
                               4.0_SRK*s*tri%points(5) +&
                            (-4.0_SRK*s)*tri%points(6)
                                      
  ds = (4.0_SRK*r + 4.0_SRK*s - 3.0_SRK)*tri%points(1) +& 
                   (4.0_SRK*s - 1.0_SRK)*tri%points(3) +&
                            (-4.0_SRK*r)*tri%points(4) +&
                               4.0_SRK*r*tri%points(5) +&
       4.0_SRK*(1.0_SRK - r - 2.0_SRK*s)*tri%points(6)
ENDSUBROUTINE derivative_Triangle6_2D


PURE FUNCTION area_Triangle6_2D(tri) RESULT(a)
  CLASS(Triangle6_2D),INTENT(IN) :: tri
  TYPE(PointType) :: dr, ds
  REAL(SRK) :: a
  REAL(SRK) :: w(12), r(12), s(12) 
  INTEGER(SIK) :: i
  ! P6. 0 negative weights, 0 points outside of the triangle
  w = (/0.058393137863189,& 
        0.058393137863189,&
        0.058393137863189,&
        0.025422453185104,&
        0.025422453185104,&
        0.025422453185104,&
        0.041425537809187,&
        0.041425537809187,&
        0.041425537809187,&
        0.041425537809187,&
        0.041425537809187,&
        0.041425537809187/)

  r = (/0.249286745170910,& 
        0.249286745170910,&
        0.501426509658179,&
        0.063089014491502,&
        0.063089014491502,&
        0.873821971016996,&
        0.310352451033785,&
        0.636502499121399,&
        0.053145049844816,&
        0.310352451033785,&
        0.636502499121399,&
        0.053145049844816/)

  s = (/0.249286745170910,& 
        0.501426509658179,&
        0.249286745170910,&
        0.063089014491502,&
        0.873821971016996,&
        0.063089014491502,&
        0.636502499121399,&
        0.053145049844816,&
        0.310352451033785,&
        0.053145049844816,&
        0.310352451033785,&
        0.636502499121399/)

  ! Numerical integration required. Gauss-Legendre quadrature over a triangle is used.
  ! Let T(r,s) be the interpolation function for tri6,
  !                             1 1-r                          N
  ! A = ∬ ||∂T/∂r × ∂T/∂s||dA = ∫  ∫ ||∂T/∂r × ∂T/∂s|| ds dr = ∑ wᵢ||∂T/∂r(rᵢ,sᵢ) × ∂T/∂s(rᵢ,sᵢ)||
  !      D                      0  0                          i=1
  !
  ! N is the number of points used in the quadrature. N = 12 is sufficient for 2D.
  a = 0.0_SRK
  DO i = 1, 12
    CALL derivative(tri, r(i), s(i), dr, ds)
    a = a + w(i)*norm(cross(dr, ds))
  ENDDO
ENDFUNCTION area_Triangle6_2D

PURE FUNCTION real_to_parametric_Triangle6_2D(tri, p) RESULT(p_out)
  CLASS(Triangle6_2D),INTENT(IN) :: tri
  TYPE(PointType),INTENT(IN) :: p
  TYPE(PointType) :: p_out, err1, err2, dr, ds
  REAL(SRK) :: r,s,detinv,J_inv(2,2),delta_rs(2)
  INTEGER(SIK) :: i, max_iters
  max_iters = 30
  r = 1.0_SRK/3.0_SRK
  s = 1.0_SRK/3.0_SRK
  err1 = p - interpolate(tri, r, s)
  DO i = 1, max_iters
    CALL derivative(tri, r, s, dr, ds)
    detinv = 1.0_SRK/(dr%coord(1)*ds%coord(2) - ds%coord(1)*dr%coord(2))
    J_inv(1,1) = +detinv * ds%coord(2)
    J_inv(2,1) = -detinv * dr%coord(2)
    J_inv(1,2) = -detinv * ds%coord(1)
    J_inv(2,2) = +detinv * dr%coord(1)
    delta_rs = MATMUL(J_inv, err1%coord)
    r = r + delta_rs(1)
    s = s + delta_rs(2)
    err2 = p - interpolate(tri, r, s)
    IF( norm(err2 - err1) < 1.0E-6_SRK ) THEN
      EXIT
    ENDIF
    err1 = err2
  ENDDO
  CALL p_out%init(DIM=2, X=r, Y=s)
ENDFUNCTION real_to_parametric_Triangle6_2D


PURE FUNCTION pointInside_Triangle6_2D(tri, p) RESULT(bool)
  CLASS(Triangle6_2D),INTENT(IN) :: tri
  TYPE(PointType),INTENT(IN) :: p
  TYPE(PointType) :: p_rs
  REAL(SRK) :: eps
  LOGICAL(SBK) :: bool
  ! Determine if the point is in the triangle using the Newton-Raphson method
  p_rs = real_to_parametric(tri, p)
  eps = 1E-6_SRK
  ! Check that the r coordinate and s coordinate are in [-ϵ,  1 + ϵ] and
  ! r + s ≤ 1 + ϵ
  ! These are the conditions for a valid point in the triangle ± some ϵ
  ! Also check that the point is close to what the interpolation function produces
  IF((-eps <= p_rs%coord(1)) .AND. (p_rs%coord(1) <= 1 + eps) .AND. &
     (-eps <= p_rs%coord(2)) .AND. (p_rs%coord(2) <= 1 + eps) .AND. &
     (p_rs%coord(1) + p_rs%coord(2) <= 1 + eps) .AND. &  
     (norm(p - interpolate(tri, p_rs%coord(1), p_rs%coord(2))) < 1.0E-4_SRK)) THEN 
    bool = .TRUE.
  ELSE
    bool = .FALSE.
  ENDIF
ENDFUNCTION pointInside_Triangle6_2D

!-------------------------------------------------------------------------------
!> @brief Finds the intersections between a line and the quadratic triangle (if it exists)
!> @param line line to test for intersection
!
PURE SUBROUTINE intersectLine_Triangle6_2D(tri, l, npoints, points)
  CLASS(Triangle6_2D),INTENT(IN) :: tri
  TYPE(LineType),INTENT(IN) :: l
  INTEGER(SIK),INTENT(OUT) :: npoints
  TYPE(PointType),INTENT(OUT) :: points(4)
  TYPE(PointType) :: intersection_points(8), ipoint1, ipoint2
  TYPE(QuadraticSegment_2D) :: edges(3)
  INTEGER(SIK) :: i, j, intersections, ipoints
  LOGICAL(SBK) :: duplicate
  ! Intersect all the edges
  CALL edges(1)%set(tri%points(1), tri%points(2), tri%points(4)) 
  CALL edges(2)%set(tri%points(2), tri%points(3), tri%points(5)) 
  CALL edges(3)%set(tri%points(3), tri%points(1), tri%points(6)) 
  intersections = 0
  npoints = 0
  DO i = 1,3
    CALL intersect(edges(i), l, ipoints, ipoint1, ipoint2)
    IF( ipoints == 1) THEN
      intersection_points(intersections + 1) = ipoint1
      intersections = intersections + 1
    ELSEIF( ipoints == 2) THEN
      intersection_points(intersections + 1) = ipoint1
      intersections = intersections + 1
      intersection_points(intersections + 1) = ipoint2
      intersections = intersections + 1
    ENDIF 
  ENDDO
  DO i = 1, intersections
    IF ( npoints == 0) THEN
      points(1) = intersection_points(1)
      npoints = 1
    ELSE
      duplicate = .FALSE.
      DO j = 1, npoints
        IF( intersection_points(i) .APPROXEQA. points(j) ) THEN
          duplicate = .TRUE. 
          EXIT
        ENDIF
      ENDDO
      IF( .NOT. duplicate) THEN
        npoints = npoints + 1
        points(npoints) = intersection_points(i)
      ENDIF
    ENDIF
  ENDDO
ENDSUBROUTINE intersectLine_Triangle6_2D
ENDMODULE Geom_Triangle6
