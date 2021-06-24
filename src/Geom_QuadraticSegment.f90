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
PUBLIC :: arc_length
PUBLIC :: derivative
PUBLIC :: interpolate
PUBLIC :: intersect

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
ENDTYPE QuadraticSegment_2D

INTERFACE interpolate
  MODULE PROCEDURE interpolate_QuadraticSegment_2D
ENDINTERFACE interpolate

INTERFACE intersect
  MODULE PROCEDURE intersectLine_QuadraticSegment_2D
ENDINTERFACE intersect

INTERFACE derivative
  MODULE PROCEDURE derivative_QuadraticSegment_2D
ENDINTERFACE derivative

INTERFACE arc_length
  MODULE PROCEDURE arc_length_QuadraticSegment_2D
ENDINTERFACE arc_length
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

ELEMENTAL FUNCTION interpolate_QuadraticSegment_2D(q, r) RESULT(p)
  CLASS(QuadraticSegment_2D),INTENT(IN) :: q
  REAL(SRK), INTENT(IN) :: r
  TYPE(PointType) :: p
  p = (2.0_SRK*r - 1.0_SRK)*(r - 1.0_SRK)*q%points(1) + &
                  r*(2.0_SRK*r - 1.0_SRK)*q%points(2) + &
                  4.0_SRK*r*(1.0_SRK - r)*q%points(3)
ENDFUNCTION interpolate_QuadraticSegment_2D

ELEMENTAL FUNCTION derivative_QuadraticSegment_2D(q, r) RESULT(p)
  CLASS(QuadraticSegment_2D),INTENT(IN) :: q
  REAL(SRK), INTENT(IN) :: r
  TYPE(PointType) :: p
  p = (4.0_SRK*r - 3.0_SRK)*q%points(1) + &
      (4.0_SRK*r - 1.0_SRK)*q%points(2) + &
      (4.0_SRK - 8.0_SRK*r)*q%points(3)
ENDFUNCTION derivative_QuadraticSegment_2D

ELEMENTAL FUNCTION arc_length_QuadraticSegment_2D(q) RESULT(a)
  CLASS(QuadraticSegment_2D),INTENT(IN) :: q
  INTEGER(SIK) :: i
  TYPE(PointType) :: p
  REAL(SRK) :: a
  REAL(SRK) :: w(15), r(15) 
  w = (/ &
       0.015376620998058315,&
       0.03518302374405407,&
       0.053579610233586,&
       0.06978533896307715,&
       0.083134602908497,&
       0.0930805000077811,&
       0.0992157426635558,&
       0.10128912096278064,&
       0.0992157426635558,&
       0.0930805000077811,&
       0.083134602908497,&
       0.06978533896307715,&
       0.053579610233586,&
       0.03518302374405407,&
       0.015376620998058315/)
  r = (/&
       0.006003740989757311,&
       0.031363303799647024,&
       0.0758967082947864,&
       0.13779113431991497,&
       0.21451391369573058,&
       0.3029243264612183,&
       0.39940295300128276,&
       0.5,&
       0.6005970469987172,&
       0.6970756735387817,&
       0.7854860863042694,&
       0.862208865680085,&
       0.9241032917052137,&
       0.968636696200353,&
       0.9939962590102427/)
  a = 0.0_SRK
  DO i = 1,15
    p = derivative(q, r(i))
    a = a + w(i)*norm(p)
  ENDDO
ENDFUNCTION arc_length_QuadraticSegment_2D

!-------------------------------------------------------------------------------
!> @brief Finds the intersections between a line and the quadratic segment (if it exists)
!> @param line line to test for intersection
!
! ELEMENTAL
!
SUBROUTINE intersectLine_QuadraticSegment_2D(q, l, intersects, npoints, point1, point2)
  CLASS(QuadraticSegment_2D),INTENT(IN) :: q
  TYPE(LineType),INTENT(IN) :: l
  LOGICAL(SBK),INTENT(OUT) :: intersects
  INTEGER(SIK),INTENT(OUT) :: npoints
  TYPE(PointType),INTENT(OUT) :: point1, point2
  REAL(SRK) :: A, B, C, r, s, r1, r2, s1, s2
  TYPE(PointType) :: A_vec, B_vec, C_vec, D_vec, E_vec, w_vec, l_s1, l_s2
  ! q(r) = (2r-1)(r-1)x⃗₁ + r(2r-1)x⃗₂ + 4r(1-r)x⃗₃
  ! q(r) = 2r²(x⃗₁ + x⃗₂ - 2x⃗₃) + r(-3x⃗₁ - x⃗₂ + 4x⃗₃) + x⃗₁
  ! Let D⃗ = 2(x⃗₁ + x⃗₂ - 2x⃗₃), E⃗ = (-3x⃗₁ - x⃗₂ + 4x⃗₃), F⃗ = x₁
  ! q(r) = r²D⃗ + rE⃗ + F⃗
  ! l(s) = x⃗₄ + sw⃗
  ! If D⃗ × w⃗ ≠ 0
  !   x⃗₄ + sw⃗ = r²D⃗ + rE⃗ + F⃗
  !   sw⃗ = r²D⃗ + rE⃗ + (F⃗ - x⃗₄)
  !   0 = r²(D⃗ × w⃗) + r(E⃗ × w⃗) + (F⃗ - x⃗₄) × w⃗
  !   Let A = (D⃗ × w⃗), B = (E⃗ × w⃗), C = (F⃗ - x⃗₄) × w⃗
  !   0 = Ar² + Br + C
  !   r = (-B - √(B²-4AC))/2A, -B + √(B²-4AC))/2A)
  !   s = ((q(r) - p₄)⋅w⃗/(w⃗ ⋅ w⃗)
  !   r is invalid if:
  !     1) A = 0
  !     2) B² < 4AC
  !     3) r < 0 or 1 < r   (Curve intersects, segment doesn't)
  !   s is invalid if:
  !     1) s < 0 or 1 < s   (Line intersects, segment doesn't)
  ! If D⃗ × w⃗ = 0, we need to use line intersection instead.
  npoints = 0
  intersects = .FALSE.
  CALL point1%clear()
  CALL point2%clear()
  CALL point1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL point2%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  D_vec = 2.0_SRK*(q%points(1) + q%points(2) - 2.0_SRK*q%points(3))
  E_vec = 4.0_SRK*q%points(3) - 3.0_SRK*q%points(1) - q%points(2)
  w_vec = l%p2 - l%p1
  A_vec = cross(D_vec, w_vec)
  B_vec = cross(E_vec, w_vec)
  C_vec = cross((q%points(1) - l%p1), w_vec)
  A = A_vec%coord(3)
  B = B_vec%coord(3)
  C = C_vec%coord(3)
  IF (A < 1.0E-6_SRK) THEN
    r = -C/B
    point1 = interpolate(q,r)
    s = (DOT_PRODUCT((point1 - l%p1), w_vec))/DOT_PRODUCT(w_vec, w_vec)
    IF (0.0_SRK <= s .AND. s <= 1.0_SRK .AND. 0.0_SRK <= r .AND. r <= 1.0_SRK) THEN
      npoints = 1
    ENDIF
  ELSE
    r1 = (-B - SQRT(B**2 - 4*A*C))/(2*A)
    r2 = (-B + SQRT(B**2 - 4*A*C))/(2*A)
    point1 = interpolate(q,r1)
    point2 = interpolate(q,r2)
    s1 = (DOT_PRODUCT((point1 - l%p1), w_vec))/DOT_PRODUCT(w_vec, w_vec)
    s2 = (DOT_PRODUCT((point2 - l%p1), w_vec))/DOT_PRODUCT(w_vec, w_vec)
    l_s1 = (l%p2 - l%p1)*s1 + l%p1
    l_s2 = (l%p2 - l%p1)*s2 + l%p1
    ! Check points to see if they are valid intersections
    IF (0.0_SRK <= s1 .AND. s1 <= 1.0_SRK .AND. &
        0.0_SRK <= r1 .AND. r1 <= 1.0_SRK .AND. &
        (point1 .APPROXEQA. l_s1)) THEN
      npoints = 1
    ENDIF
    IF (0.0_SRK <= s2 .AND. s2 <= 1.0_SRK .AND. &
        0.0_SRK <= r2 .AND. r2 <= 1.0_SRK .AND. &
        (point2 .APPROXEQA. l_s2)) THEN
      npoints = npoints + 1
      IF (npoints == 1) THEN
        point1 = point2
      ENDIF
    ENDIF
  ENDIF
  intersects = npoints > 0
  CALL A_vec%clear()
  CALL B_vec%clear()
  CALL C_vec%clear()
  CALL D_vec%clear()
  CALL E_vec%clear()
  CALL w_vec%clear()
  CALL l_s1%clear()
  CALL l_s2%clear()
ENDSUBROUTINE intersectLine_QuadraticSegment_2D
ENDMODULE Geom_QuadraticSegment
