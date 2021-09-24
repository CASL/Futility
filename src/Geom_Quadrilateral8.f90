!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module defining a quadrilateral with quadratic edges for 
!> use in geometry.
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Quadrilateral8
USE IntrType
USE Geom_Points
USE Geom_Line
USE Geom_QuadraticSegment

IMPLICIT NONE
PRIVATE
!
! List of Public items
!
PUBLIC :: Quadrilateral8_2D
PUBLIC :: interpolate
PUBLIC :: derivative
PUBLIC :: area
PUBLIC :: real_to_parametric
PUBLIC :: pointInside
PUBLIC :: intersect

TYPE :: Quadrilateral8_2D
  ! The points are assumed to be ordered  in counter clockwise order as follows
  ! p₁ = vertex A
  ! p₂ = vertex B
  ! p₃ = vertex C
  ! p₄ = vertex D
  ! p₅ = point on the quadratic segment from A to B
  ! p₆ = point on the quadratic segment from B to C
  ! p₇ = point on the quadratic segment from C to D
  ! p₈ = point on the quadratic segment from D to A
  TYPE(PointType) :: points(8)
!
!List of type bound procedures
  CONTAINS
    !> @copybrief Geom_Quadrilateral8::init_Quadrilateral8_2D
    !> @copydetails Geom_Quadrilateral8::init_Quadrilateral8_2D
    PROCEDURE,PASS :: set => init_Quadrilateral8_2D
    !> @copybrief Geom_Quadrilateral8::clear_Quadrilateral8_2D
    !> @copydetails Geom_Quadrilateral8::clear_Quadrilateral8_2D
    PROCEDURE,PASS :: clear => clear_Quadrilateral8_2D
ENDTYPE Quadrilateral8_2D

INTERFACE interpolate
  MODULE PROCEDURE interpolate_Quadrilateral8_2D
ENDINTERFACE interpolate

INTERFACE derivative
  MODULE PROCEDURE derivative_Quadrilateral8_2D
ENDINTERFACE derivative

INTERFACE area
  MODULE PROCEDURE area_Quadrilateral8_2D
ENDINTERFACE area

INTERFACE real_to_parametric
  MODULE PROCEDURE real_to_parametric_Quadrilateral8_2D
ENDINTERFACE real_to_parametric

INTERFACE pointInside
  MODULE PROCEDURE pointInside_Quadrilateral8_2D
ENDINTERFACE pointInside

INTERFACE intersect
  MODULE PROCEDURE intersectLine_Quadrilateral8_2D
ENDINTERFACE intersect

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Constructor for Quadrilateral8_2D
!> @param p1 Start point of the quad
!> @param p2 End point of the quad
!> @param p3 An additional point on the quad
!>
PURE SUBROUTINE init_Quadrilateral8_2D(quad, points)
  CLASS(Quadrilateral8_2D),INTENT(INOUT) :: quad
  TYPE(PointType),INTENT(IN) :: points(8)
  INTEGER(SIK) :: i
  LOGICAL(SBK) :: bool
  CALL quad%clear()
  bool = .TRUE.
  DO i = 1,8
    IF( .NOT. points(i)%dim == 2 ) THEN
      bool = .FALSE.
    ENDIF
  ENDDO
  IF(bool) THEN
    DO i = 1,8
      quad%points(i) = points(i)
    ENDDO
  ENDIF
ENDSUBROUTINE init_Quadrilateral8_2D
!
!-------------------------------------------------------------------------------
!> @brief Clears and resets all values of the quad
!> @param quad the quad
ELEMENTAL SUBROUTINE clear_Quadrilateral8_2D(quad)
  CLASS(Quadrilateral8_2D),INTENT(INOUT) :: quad
  INTEGER(SIK) :: i
  DO i = 1,8
    CALL quad%points(i)%clear()
  ENDDO
ENDSUBROUTINE clear_Quadrilateral8_2D

ELEMENTAL FUNCTION interpolate_Quadrilateral8_2D(quad, r, s) RESULT(p)
  CLASS(Quadrilateral8_2D),INTENT(IN) :: quad
  REAL(SRK), INTENT(IN) :: r,s
  REAL(SRK) :: u, v
  TYPE(PointType) :: p
    ! See The Visualization Toolkit: An Object-Oriented Approach to 3D Graphics, 4th Edition
    ! Chapter 8, Advanced Data Representation, in the interpolation functions section
    u = 2.0_SRK*r - 1.0_SRK
    v = 2.0_SRK*s - 1.0_SRK 
    p = ((1.0_SRK - u)*(1.0_SRK - v)*(-u - v - 1.0_SRK)/4.0_SRK)*quad%points(1) + & 
        ((1.0_SRK + u)*(1.0_SRK - v)*( u - v - 1.0_SRK)/4.0_SRK)*quad%points(2) + &
        ((1.0_SRK + u)*(1.0_SRK + v)*( u + v - 1.0_SRK)/4.0_SRK)*quad%points(3) + &
        ((1.0_SRK - u)*(1.0_SRK + v)*(-u + v - 1.0_SRK)/4.0_SRK)*quad%points(4) + &
                        ((1.0_SRK - u**2)*(1.0_SRK - v)/2.0_SRK)*quad%points(5) + &
                        ((1.0_SRK - v**2)*(1.0_SRK + u)/2.0_SRK)*quad%points(6) + &
                        ((1.0_SRK - u**2)*(1.0_SRK + v)/2.0_SRK)*quad%points(7) + &
                        ((1.0_SRK - v**2)*(1.0_SRK - u)/2.0_SRK)*quad%points(8)
ENDFUNCTION interpolate_Quadrilateral8_2D

ELEMENTAL SUBROUTINE derivative_Quadrilateral8_2D(quad, r, s, dr, ds)
  CLASS(Quadrilateral8_2D),INTENT(IN) :: quad
  REAL(SRK), INTENT(IN) :: r,s
  TYPE(PointType), INTENT(INOUT) :: dr, ds
  REAL(SRK) :: u, v
  ! Chain rule
  ! ∂Q   ∂Q ∂ξ  ∂Q   ∂Q ∂η 
  ! -- = -- --, -- = -- --
  ! ∂r   ∂ξ ∂r  ∂s   ∂η ∂s
  ! ξ = 2T(r) - 1; η = 2T(s) - 1 
  ! ∂Q_∂ξ = (1 - η)*(2ξ + η)/4*p1 + 
  !         (1 - η)*(2ξ - η)/4*p2 +
  !         (1 + η)*(2ξ + η)/4*p3 +    
  !         (1 + η)*(2ξ - η)/4*p4 +    
  !                 -ξ*(1 - η)*p5 +    
  !                (1 - η^2)/2*p6 +    
  !                 -ξ*(1 + η)*p7 +    
  !               -(1 - η^2)/2*p8    

  ! ∂Q_∂η = (1 - ξ)*( ξ + 2η)/4*p1 + 
  !         (1 + ξ)*(-ξ + 2η)/4*p2 +
  !         (1 + ξ)*( ξ + 2η)/4*p3 +    
  !         (1 - ξ)*(-ξ + 2η)/4*p4 +    
  !                -(1 - ξ^2)/2*p5 +   
  !                  -η*(1 + ξ)*p6 +    
  !                 (1 - ξ^2)/2*p7 +    
  !                  -η*(1 - ξ)*p8
  u = 2.0_SRK*r - 1.0_SRK
  v = 2.0_SRK*s - 1.0_SRK 

  dr = (1.0_SRK - v)*(2.0_SRK*u + v)/4.0_SRK*quad%points(1) + & 
       (1.0_SRK - v)*(2.0_SRK*u - v)/4.0_SRK*quad%points(2) + &
       (1.0_SRK + v)*(2.0_SRK*u + v)/4.0_SRK*quad%points(3) + &   
       (1.0_SRK + v)*(2.0_SRK*u - v)/4.0_SRK*quad%points(4) + &   
                          (-u*(1.0_SRK - v))*quad%points(5) + &   
                    (1.0_SRK - v**2)/2.0_SRK*quad%points(6) + &   
                          (-u*(1.0_SRK + v))*quad%points(7) + &   
                 (-(1.0_SRK - v**2))/2.0_SRK*quad%points(8)    

   ds = (1.0_SRK - u)*( u + 2.0_SRK*v)/4.0_SRK*quad%points(1) + & 
        (1.0_SRK + u)*(-u + 2.0_SRK*v)/4.0_SRK*quad%points(2) + &
        (1.0_SRK + u)*( u + 2.0_SRK*v)/4.0_SRK*quad%points(3) + &    
        (1.0_SRK - u)*(-u + 2.0_SRK*v)/4.0_SRK*quad%points(4) + &    
                   (-(1.0_SRK - u**2)/2.0_SRK)*quad%points(5) + &   
                            (-v*(1.0_SRK + u))*quad%points(6) + &    
                (1.0_SRK - u**2.0_SRK)/2.0_SRK*quad%points(7) + &    
                            (-v*(1.0_SRK - u))*quad%points(8)

  ! Chain rule
  dr = 2.0_SRK*dr
  ds = 2.0_SRK*ds
ENDSUBROUTINE derivative_Quadrilateral8_2D

ELEMENTAL FUNCTION area_Quadrilateral8_2D(quad) RESULT(a)
  CLASS(Quadrilateral8_2D),INTENT(IN) :: quad
  TYPE(PointType) :: dr, ds
  REAL(SRK) :: a
  REAL(SRK) :: w(3), r(3)
  INTEGER(SIK) :: i, j
  w = (/0.2777777777777776,& 
        0.4444444444444444,&
        0.2777777777777776/)

  r = (/0.1127016653792583,& 
        0.5,&
        0.8872983346207417/)
  ! Numerical integration required. Gauss-Legendre quadrature over a quadrilateral is used.
  ! Let Q(r,s) be the interpolation function for quad8,
  !                             1  1                         
  ! A = ∬ ||∂Q/∂r × ∂Q/∂s||dA = ∫  ∫ ||∂Q/∂r × ∂Q/∂s|| ds dr 
  !      D                      0  0                         
  !   
  !       N   N  
  !   =   ∑   ∑  wᵢwⱼ||∂Q/∂r(rᵢ,sⱼ) × ∂Q/∂s(rᵢ,sⱼ)||
  !      i=1 j=1
  ! N is the square root of the number of points used in the quadrature.
  ! N = 3 is sufficient for 2D.
  a = 0.0_SRK
  DO i = 1, 3
    DO j = 1, 3
      CALL derivative(quad, r(i), r(j), dr, ds)
      a = a + w(i)*w(j)*norm(cross(dr, ds))
    ENDDO
  ENDDO
ENDFUNCTION area_Quadrilateral8_2D

ELEMENTAL FUNCTION real_to_parametric_Quadrilateral8_2D(quad, p) RESULT(p_out)
  CLASS(Quadrilateral8_2D),INTENT(IN) :: quad
  TYPE(PointType),INTENT(IN) :: p
  TYPE(PointType) :: p_out, err1, err2, dr, ds
  REAL(SRK) :: r,s,detinv,J_inv(2,2),delta_rs(2)
  INTEGER(SIK) :: i, max_iters
  max_iters = 30
  r = 1.0_SRK/3.0_SRK
  s = 1.0_SRK/3.0_SRK
  err1 = p - interpolate(quad, r, s)
  DO i = 1, max_iters
    CALL derivative(quad, r, s, dr, ds)
    detinv = 1.0_SRK/(dr%coord(1)*ds%coord(2) - ds%coord(1)*dr%coord(2))
    J_inv(1,1) = +detinv * ds%coord(2)
    J_inv(2,1) = -detinv * dr%coord(2)
    J_inv(1,2) = -detinv * ds%coord(1)
    J_inv(2,2) = +detinv * dr%coord(1)
    delta_rs = MATMUL(J_inv, err1%coord)
    r = r + delta_rs(1)
    s = s + delta_rs(2)
    err2 = p - interpolate(quad, r, s)
    IF( norm(err2 - err1) < 1.0E-6_SRK ) THEN
      EXIT
    ENDIF
    err1 = err2
  ENDDO
  CALL p_out%init(DIM=2, X=r, Y=s)
ENDFUNCTION real_to_parametric_Quadrilateral8_2D


ELEMENTAL FUNCTION pointInside_Quadrilateral8_2D(quad, p) RESULT(bool)
  CLASS(Quadrilateral8_2D),INTENT(IN) :: quad
  TYPE(PointType),INTENT(IN) :: p
  TYPE(PointType) :: p_rs
  REAL(SRK) :: eps
  LOGICAL(SBK) :: bool
  ! Determine if the point is in the quad8 using the Newton-Raphson method
  p_rs = real_to_parametric(quad, p)
  eps = 1E-6_SRK
  ! Check that the r coordinate and s coordinate are in [-ϵ,  1 + ϵ] and
  ! r + s ≤ 1 + ϵ
  ! These are the conditions for a valid point in the quadangle ± some ϵ
  ! Also check that the point is close to what the interpolation function produces
  IF((-eps <= p_rs%coord(1)) .AND. (p_rs%coord(1) <= 1 + eps) .AND. &
     (-eps <= p_rs%coord(2)) .AND. (p_rs%coord(2) <= 1 + eps) .AND. &
     (p_rs%coord(1) + p_rs%coord(2) <= 1 + eps) .AND. &  
     (norm(p - interpolate(quad, p_rs%coord(1), p_rs%coord(2))) < 1.0E-4_SRK)) THEN 
    bool = .TRUE.
  ELSE
    bool = .FALSE.
  ENDIF
ENDFUNCTION pointInside_Quadrilateral8_2D

!-------------------------------------------------------------------------------
!> @brief Finds the intersections between a line and the quadratic quad (if it exists)
!> @param line line to test for intersection
!
PURE SUBROUTINE intersectLine_Quadrilateral8_2D(quad, l, npoints, points)
  CLASS(Quadrilateral8_2D),INTENT(IN) :: quad
  TYPE(LineType),INTENT(IN) :: l
  INTEGER(SIK),INTENT(OUT) :: npoints
  TYPE(PointType),INTENT(OUT) :: points(4)
  TYPE(PointType) :: intersection_points(8), ipoint1, ipoint2
  TYPE(QuadraticSegment_2D) :: edges(4)
  INTEGER(SIK) :: i, j, intersections, ipoints
  LOGICAL(SBK) :: duplicate
  ! Intersect all the edges
  CALL edges(1)%set(quad%points(1), quad%points(2), quad%points(5)) 
  CALL edges(2)%set(quad%points(2), quad%points(3), quad%points(6)) 
  CALL edges(3)%set(quad%points(3), quad%points(4), quad%points(7)) 
  CALL edges(4)%set(quad%points(4), quad%points(1), quad%points(8)) 
  intersections = 0
  npoints = 0
  DO i = 1,4
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
ENDSUBROUTINE intersectLine_Quadrilateral8_2D
ENDMODULE Geom_Quadrilateral8
