!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testGeom_QuadraticSegment
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE Strings
USE Geom_QuadraticSegment
USE Geom_Points
USE Geom_Line

IMPLICIT NONE

CREATE_TEST('QUADRATIC TYPE')
REGISTER_SUBTEST('CLEAR', testClear)
REGISTER_SUBTEST('INIT', testInit)
REGISTER_SUBTEST('INTERPOLATE', testInterpolate)
REGISTER_SUBTEST('DERIVATIVE', testDerivative)
REGISTER_SUBTEST('ARC LENGTH', testArcLength)
REGISTER_SUBTEST('INTERSECT LINE', testIntersectLine)
FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testClear()
  TYPE(QuadraticSegment_2D) :: quad
  INTEGER(SIK) :: i
  DO i=1,3
    CALL quad%points(i)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  ENDDO

  CALL quad%clear()
  DO i=1,3
    ASSERT(quad%points(i)%dim == 0, "Value not cleared")
    ASSERT(.NOT. ALLOCATED(quad%points(i)%coord), "Value not cleared")
  ENDDO
ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
SUBROUTINE testInit()
  TYPE(QuadraticSegment_2D) :: quad
  TYPE(PointType) :: p1, p2, p3

  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL quad%set(p1, p2, p3)
  ASSERT(quad%points(1) == p1, "Point assigned incorrectly")
  ASSERT(quad%points(2) == p2, "Point assigned incorrectly")
  ASSERT(quad%points(3) == p3, "Point assigned incorrectly")
ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
SUBROUTINE testInterpolate()
  TYPE(QuadraticSegment_2D) :: q
  TYPE(PointType) :: p1, p2, p3, p

  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL q%set(p1, p2, p3)
  p = interpolate(q, 0.0_SRK)
  ASSERT( p == p1, "Wrong point")
  p = interpolate(q, 1.0_SRK)
  ASSERT( p == p2, "Wrong point")
  p = interpolate(q, 0.5_SRK)
  ASSERT( p == p3, "Wrong point")
  p = interpolate(q, 0.25_SRK)
  ASSERT( p%coord(1) .APPROXEQA. 0.5_SRK, "Wrong point")
  ASSERT( p%coord(2) .APPROXEQA. 0.75_SRK, "Wrong point")
ENDSUBROUTINE testInterpolate
!
!-------------------------------------------------------------------------------
SUBROUTINE testDerivative()
  TYPE(QuadraticSegment_2D) :: q
  TYPE(PointType) :: p1, p2, p3, dr, p_r

  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL q%set(p1, p2, p3)
  CALL derivative(q, 0.0_SRK, dr)
  CALL p_r%init(DIM=2, X=2.0_SRK, Y=4.0_SRK)
  ASSERT( p_r .APPROXEQA. dr, "Wrong point")
  CALL p_r%clear()
  CALL derivative(q, 0.25_SRK, dr)
  CALL p_r%init(DIM=2, X=2.0_SRK, Y=2.0_SRK)
  ASSERT( p_r .APPROXEQA. dr, "Wrong point")
ENDSUBROUTINE testDerivative
!
!-------------------------------------------------------------------------------
SUBROUTINE testArcLength()
  TYPE(QuadraticSegment_2D) :: q
  TYPE(PointType) :: p1, p2, p3
  REAL(SRK) :: a

  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL q%set(p1, p2, p3)
  a = arc_length(q)
  ASSERT( ABS(a - 2.0_SRK) < 1.0E-6_SRK, "arc_length")
  CALL p3%clear()

  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL q%set(p1, p2, p3)
  a = arc_length(q)
  ASSERT( a .APPROXEQA. 2.9578857151786138, "arc_length")
ENDSUBROUTINE testArcLength
!
!-------------------------------------------------------------------------------
SUBROUTINE testIntersectLine()
  TYPE(QuadraticSegment_2D) :: q
  TYPE(LineType) :: l
  TYPE(PointType) :: p1, p2, p3, p4, p5, ipoint1, ipoint2
  INTEGER(SIK) :: npoints
  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL p4%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL p5%init(DIM=2, X=1.0_SRK, Y=2.0_SRK)

  ! 1 intersection
  CALL q%set(p1, p2, p3)
  CALL l%set(p4, p5)
  CALL intersect(q, l, npoints, ipoint1, ipoint2)
  ASSERT(npoints == 1, "Intersects")
  ASSERT(ipoint1%coord(1) .APPROXEQ. 1.0_SRK, "intersection 1")
  ASSERT(ipoint1%coord(2) .APPROXEQ. 1.0_SRK, "intersection 1")

  ! 2 intersections
  CALL p4%clear()
  CALL p5%clear()
  CALL p4%init(DIM=2, X=0.0_SRK, Y=0.75_SRK)
  CALL p5%init(DIM=2, X=2.0_SRK, Y=0.75_SRK)
  CALL l%set(p4, p5)
  CALL intersect(q, l, npoints, ipoint1, ipoint2)
  ASSERT(npoints == 2, "Intersects")
  ASSERT(ipoint1%coord(1) .APPROXEQ. 0.5_SRK,  "intersection 1")
  ASSERT(ipoint1%coord(2) .APPROXEQ. 0.75_SRK, "intersection 1")
  ASSERT(ipoint2%coord(1) .APPROXEQ. 1.5_SRK,  "intersection 2")
  ASSERT(ipoint2%coord(2) .APPROXEQ. 0.75_SRK, "intersection 2")

  ! 0 intersections
  CALL p4%clear()
  CALL p5%clear()
  CALL p4%init(DIM=2, X=0.0_SRK, Y=3.0_SRK)
  CALL p5%init(DIM=2, X=2.0_SRK, Y=3.0_SRK)
  CALL l%set(p4, p5)
  CALL intersect(q, l, npoints, ipoint1, ipoint2)
  ASSERT(npoints == 0, "Intersects")

  CALL q%clear()
  CALL l%clear()
ENDSUBROUTINE testIntersectLine
ENDPROGRAM testGeom_QuadraticSegment
