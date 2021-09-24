!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module defining a quadrilateral for use in geometry.
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Quadrilateral
USE IntrType
USE Geom_Points
USE Geom_Line
USE Geom_Triangle

IMPLICIT NONE
PRIVATE
!
! List of Public items
PUBLIC :: Quadrilateral_2D
PUBLIC :: interpolate
PUBLIC :: area
PUBLIC :: pointInside
PUBLIC :: intersect

! NOTE: Quadrilaterals are assumed to be convex. This is because quadrilaterals
! must be convex to be valid finite elements.
! https://math.stackexchange.com/questions/2430691/jacobian-determinant-for-bi-linear-quadrilaterals

TYPE :: Quadrilateral_2D
  ! Counter clockwise order
  TYPE(PointType) :: points(4)
!
!List of type bound procedures
  CONTAINS
    !> @copybrief Geom_Quadrilateral::init_Quadrilateral_2D
    !> @copydetails Geom_Quadrilateral::init_Quadrilateral_2D
    PROCEDURE,PASS :: set => init_Quadrilateral_2D
    !> @copybrief Geom_Quadrilateral::clear_Quadrilateral_2D
    !> @copydetails Geom_Quadrilateral::clear_Quadrilateral_2D
    PROCEDURE,PASS :: clear => clear_Quadrilateral_2D
ENDTYPE Quadrilateral_2D

INTERFACE interpolate
  MODULE PROCEDURE interpolate_Quadrilateral_2D
ENDINTERFACE interpolate

INTERFACE area
  MODULE PROCEDURE area_Quadrilateral_2D
ENDINTERFACE area

INTERFACE pointInside
  MODULE PROCEDURE pointInside_Quadrilateral_2D
ENDINTERFACE pointInside

INTERFACE intersect
  MODULE PROCEDURE intersectLine_Quadrilateral_2D
ENDINTERFACE intersect

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Constructor for Quadrilateral_2D
!>
PURE SUBROUTINE init_Quadrilateral_2D(quad,points)
  CLASS(Quadrilateral_2D),INTENT(INOUT) :: quad
  TYPE(PointType),INTENT(IN) :: points(4)
  CALL quad%clear()
  IF(points(1)%dim == points(2)%dim .AND. &
     points(2)%dim == points(3)%dim .AND. &
     points(3)%dim == points(4)%dim .AND. points(1)%dim == 2) THEN
    quad%points(1) = points(1)
    quad%points(2) = points(2)
    quad%points(3) = points(3)
    quad%points(4) = points(4)
  ENDIF
ENDSUBROUTINE init_Quadrilateral_2D
!
!-------------------------------------------------------------------------------
!> @brief Clears and resets all values of the quad
!> @param quad the quad
PURE SUBROUTINE clear_Quadrilateral_2D(quad)
  CLASS(Quadrilateral_2D),INTENT(INOUT) :: quad
  CALL quad%points(1)%clear()
  CALL quad%points(2)%clear()
  CALL quad%points(3)%clear()
  CALL quad%points(4)%clear()
ENDSUBROUTINE clear_Quadrilateral_2D

PURE FUNCTION interpolate_Quadrilateral_2D(quad, r, s) RESULT(p)
  CLASS(Quadrilateral_2D),INTENT(IN) :: quad
  REAL(SRK), INTENT(IN) :: r,s
  TYPE(PointType) :: p
  p = (1.0_SRK - r)*(1.0_SRK - s)*quad%points(1) + &
                  r*(1.0_SRK - s)*quad%points(2) + &
                              r*s*quad%points(3) + &
                  (1.0_SRK - r)*s*quad%points(4)
ENDFUNCTION interpolate_Quadrilateral_2D

PURE FUNCTION area_Quadrilateral_2D(quad) RESULT(a)
  CLASS(Quadrilateral_2D),INTENT(IN) :: quad
  TYPE(Triangle_2D) ::t1, t2 
  REAL(SRK) :: a
  CALL t1%set(quad%points(1:3))
  CALL t2%set(quad%points((/3, 4, 1/)))
  a = area(t1)
  a = a + area(t2)
ENDFUNCTION area_Quadrilateral_2D

PURE FUNCTION pointInside_Quadrilateral_2D(quad, p) RESULT(bool)
  CLASS(Quadrilateral_2D),INTENT(IN) :: quad
  TYPE(PointType),INTENT(IN) :: p
  TYPE(Triangle_2D) ::t1, t2 
  LOGICAL(SBK) :: bool, b1, b2
  CALL t1%set(quad%points(1:3))
  CALL t2%set(quad%points((/3,4,1/)))
  b1 = pointInside(t1, p)
  b2 = pointInside(t2, p)
  bool = (b1 .OR. b2)
ENDFUNCTION pointInside_Quadrilateral_2D

!-------------------------------------------------------------------------------
!> @brief Finds the intersections between a line and the quadrilateral (if it exists)
!> @param line line to test for intersection
!
PURE SUBROUTINE intersectLine_Quadrilateral_2D(quad, l, npoints, ipoints)
  CLASS(Quadrilateral_2D),INTENT(IN) :: quad
  TYPE(LineType),INTENT(IN) :: l
  INTEGER(SIK),INTENT(OUT) :: npoints
  TYPE(PointType),INTENT(OUT) :: ipoints(2)
  TYPE(PointType) :: points(4), p_intersect
  Type(LineType) :: lines(4)
  INTEGER(SIK) :: i, intersections
  LOGICAL(SBK) :: have_p1, have_p2
  ! Intersect all the edges
  CALL lines(1)%set(quad%points(1), quad%points(2)) 
  CALL lines(2)%set(quad%points(2), quad%points(3)) 
  CALL lines(3)%set(quad%points(3), quad%points(4)) 
  CALL lines(4)%set(quad%points(4), quad%points(1)) 
  intersections = 0
  npoints = 0
  DO i = 1,4
    p_intersect = l%intersect(lines(i))
    IF( p_intersect%dim == 2 ) THEN
      points(intersections + 1) = p_intersect
      intersections = intersections + 1
    ENDIF 
  ENDDO
  have_p1 = .FALSE.
  have_p2 = .FALSE.
  DO i = 1,intersections
    IF(.NOT. have_p1) THEN
      ipoints(1) = points(i)
      have_p1 = .TRUE.
      npoints = 1
    ELSEIF((.NOT. have_p2) .AND. (.NOT.(ipoints(1) .APPROXEQA. points(i)))) THEN 
      ipoints(2) = points(i)
      have_p2 = .TRUE.
      npoints = 2
    ENDIF
  ENDDO
ENDSUBROUTINE intersectLine_Quadrilateral_2D
ENDMODULE Geom_Quadrilateral
