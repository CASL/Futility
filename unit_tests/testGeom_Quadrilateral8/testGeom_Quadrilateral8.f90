!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testGeom_Quadrilateral8
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE Strings
USE Geom_Quadrilateral8
USE Geom_Points
USE Geom_Line
USE Geom_QuadraticSegment

IMPLICIT NONE

CREATE_TEST('QUADRILATERIAL8 TYPE')
REGISTER_SUBTEST('CLEAR', testClear)
REGISTER_SUBTEST('INIT', testInit)
REGISTER_SUBTEST('INTERPOLATE', testInterpolate)
REGISTER_SUBTEST('DERIVATIVE', testDerivative)
REGISTER_SUBTEST('AREA', testArea)
REGISTER_SUBTEST('REAL TO PARAMETRIC', testR2P)
REGISTER_SUBTEST('POINT INSIDE', testPointInside)
REGISTER_SUBTEST('INTERSECT LINE', testIntersectLine)
FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testClear()
  TYPE(Quadrilateral8_2D) :: quad
  INTEGER(SIK) :: i
  DO i=1,8
    CALL quad%points(i)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  ENDDO

  CALL quad%clear()
  DO i=1,8
    ASSERT(quad%points(i)%dim == 0, "Value not cleared")
    ASSERT(.NOT. ALLOCATED(quad%points(i)%coord), "Value not cleared")
  ENDDO
ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
SUBROUTINE testInit()
  TYPE(Quadrilateral8_2D) :: quad
  TYPE(PointType) :: points(8)
  INTEGER(SIK) :: i

  CALL points(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL points(2)%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL points(3)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL points(4)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL points(5)%init(DIM=2, X=0.5_SRK, Y=0.0_SRK)
  CALL points(6)%init(DIM=2, X=1.0_SRK, Y=0.5_SRK)
  CALL points(7)%init(DIM=2, X=0.5_SRK, Y=1.0_SRK)
  CALL points(8)%init(DIM=2, X=0.0_SRK, Y=0.5_SRK)
  CALL quad%set(points)
  DO i = 1, 8
    ASSERT(quad%points(i) == points(i), "Point assigned incorrectly")
  ENDDO
ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
SUBROUTINE testInterpolate()
  TYPE(Quadrilateral8_2D) :: quad
  TYPE(PointType) :: points(8), p
  CALL points(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL points(2)%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL points(3)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL points(4)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL points(5)%init(DIM=2, X=0.5_SRK, Y=0.0_SRK)
  CALL points(6)%init(DIM=2, X=1.0_SRK, Y=0.5_SRK)
  CALL points(7)%init(DIM=2, X=0.5_SRK, Y=1.0_SRK)
  CALL points(8)%init(DIM=2, X=0.0_SRK, Y=0.5_SRK)
  CALL quad%set(points)
  p = interpolate(quad, 0.0_SRK, 0.0_SRK)
  ASSERT( p == points(1), "Wrong point")

  p = interpolate(quad, 1.0_SRK, 0.0_SRK)
  ASSERT( p == points(2), "Wrong point")

  p = interpolate(quad, 0.0_SRK, 1.0_SRK)
  ASSERT( p == points(4), "Wrong point")

  p = interpolate(quad, 1.0_SRK, 1.0_SRK)
  ASSERT( p == points(3), "Wrong point")

  p = interpolate(quad, 0.5_SRK, 0.0_SRK)
  ASSERT( p == points(5), "Wrong point")
ENDSUBROUTINE testInterpolate
!
!-------------------------------------------------------------------------------
SUBROUTINE testDerivative()
  TYPE(Quadrilateral8_2D) :: quad
  TYPE(PointType) :: points(8), dr, ds, p_r, p_s
  CALL points(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL points(2)%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL points(3)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL points(4)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL points(5)%init(DIM=2, X=0.5_SRK, Y=0.0_SRK)
  CALL points(6)%init(DIM=2, X=1.0_SRK, Y=0.5_SRK)
  CALL points(7)%init(DIM=2, X=0.5_SRK, Y=1.0_SRK)
  CALL points(8)%init(DIM=2, X=0.0_SRK, Y=0.5_SRK)
  CALL quad%set(points)
  CALL p_r%init(DIM=2, X=1.0_SRK, Y=0.0_SRK) 
  CALL p_s%init(DIM=2, X=0.0_SRK, Y=1.0_SRK) 

  CALL derivative(quad, 1.0_SRK, 0.5_SRK, dr, ds)
  ASSERT( dr .APPROXEQA. p_r, "Wrong point")
  ASSERT( ds .APPROXEQA. p_s, "Wrong point")
ENDSUBROUTINE testDerivative
!
!-------------------------------------------------------------------------------
SUBROUTINE testArea()
  TYPE(Quadrilateral8_2D) :: quad
  TYPE(PointType) :: points(8)
  REAL(SRK) :: a
  INTEGER(SIK) :: i
  CALL points(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL points(2)%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL points(3)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL points(4)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL points(5)%init(DIM=2, X=0.5_SRK, Y=0.0_SRK)
  CALL points(6)%init(DIM=2, X=1.0_SRK, Y=0.5_SRK)
  CALL points(7)%init(DIM=2, X=0.5_SRK, Y=1.0_SRK)
  CALL points(8)%init(DIM=2, X=0.0_SRK, Y=0.5_SRK)
  CALL quad%set(points)
  a = area(quad)
  ASSERT( SOFTEQ(a, 1.0_SRK, 1.0E-6_SRK), "Wrong area")
  CALL quad%clear()
  DO i = 1, 8
    CALL points(i)%clear()
  ENDDO 

  CALL points(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL points(2)%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL points(3)%init(DIM=2, X=2.0_SRK, Y=3.0_SRK)
  CALL points(4)%init(DIM=2, X=0.0_SRK, Y=3.0_SRK)
  CALL points(5)%init(DIM=2, X=1.5_SRK, Y=0.5_SRK)
  CALL points(6)%init(DIM=2, X=2.5_SRK, Y=1.5_SRK)
  CALL points(7)%init(DIM=2, X=1.5_SRK, Y=2.5_SRK)
  CALL points(8)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL quad%set(points)
  a = area(quad)
  ASSERT( SOFTEQ(a, 17.0_SRK/3.0_SRK, 1.0E-6_SRK), "Wrong area")
ENDSUBROUTINE testArea
!
!-------------------------------------------------------------------------------
SUBROUTINE testR2P()
  TYPE(Quadrilateral8_2D) :: quad
  TYPE(PointType) :: points(8), p
  CALL points(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL points(2)%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL points(3)%init(DIM=2, X=2.0_SRK, Y=3.0_SRK)
  CALL points(4)%init(DIM=2, X=0.0_SRK, Y=3.0_SRK)
  CALL points(5)%init(DIM=2, X=1.5_SRK, Y=0.5_SRK)
  CALL points(6)%init(DIM=2, X=2.5_SRK, Y=1.5_SRK)
  CALL points(7)%init(DIM=2, X=1.5_SRK, Y=2.5_SRK)
  CALL points(8)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL quad%set(points)
  p = real_to_parametric(quad, points(1))
  ASSERT( SOFTEQ(p%coord(1), 0.0_SRK, 1.0E-6_SRK), "Wrong coordinate")
  ASSERT( SOFTEQ(p%coord(2), 0.0_SRK, 1.0E-6_SRK), "Wrong coordinate")
  p = real_to_parametric(quad, points(2))
  ASSERT( SOFTEQ(p%coord(1), 1.0_SRK, 1.0E-6_SRK), "Wrong coordinate")
  ASSERT( SOFTEQ(p%coord(2), 0.0_SRK, 1.0E-6_SRK), "Wrong coordinate")
  p = real_to_parametric(quad, points(5))
  ASSERT( SOFTEQ(p%coord(1), 0.5_SRK, 1.0E-6_SRK), "Wrong coordinate")
  ASSERT( SOFTEQ(p%coord(2), 0.0_SRK, 1.0E-6_SRK), "Wrong coordinate")
ENDSUBROUTINE testR2P
!
!-------------------------------------------------------------------------------
SUBROUTINE testPointInside()
  TYPE(Quadrilateral8_2D) :: quad
  TYPE(PointType) :: points(8), p
  LOGICAL(SBK) :: bool
  CALL points(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL points(2)%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL points(3)%init(DIM=2, X=2.0_SRK, Y=3.0_SRK)
  CALL points(4)%init(DIM=2, X=0.0_SRK, Y=3.0_SRK)
  CALL points(5)%init(DIM=2, X=1.5_SRK, Y=0.5_SRK)
  CALL points(6)%init(DIM=2, X=2.5_SRK, Y=1.5_SRK)
  CALL points(7)%init(DIM=2, X=1.5_SRK, Y=2.5_SRK)
  CALL points(8)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL quad%set(points)

  CALL p%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  bool = pointInside(quad, p)
  CALL p%clear()
  ASSERT( bool, "point inside")

  CALL p%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  bool = pointInside(quad, p)
  CALL p%clear()
  ASSERT( .NOT. bool, "point not inside")
ENDSUBROUTINE testPointInside
!
!-------------------------------------------------------------------------------
SUBROUTINE testIntersectLine()
  TYPE(Quadrilateral8_2D) :: quad
  TYPE(LineType) :: l
  TYPE(PointType) :: p1, p2, p3, p4, p5, p6, p7, p8, points(4)
  INTEGER(SIK) :: npoints
  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=2.0_SRK, Y=2.0_SRK)
  CALL p4%init(DIM=2, X=1.5_SRK, Y=0.25_SRK)
  CALL p5%init(DIM=2, X=3.0_SRK, Y=1.0_SRK)
  CALL p6%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL quad%set(p1, p2, p3, p4, p5, p6)

  CALL p7%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p8%init(DIM=2, X=4.0_SRK, Y=0.0_SRK)
  CALL l%set(p7, p8)
  npoints = -1
  CALL intersect(quad, l, npoints, points)
  ASSERT(npoints == 2, "Intersects")
  ASSERT(points(1)%coord(1) .APPROXEQA. 0.0_SRK, "intersection 1")
  ASSERT(points(1)%coord(2) .APPROXEQA. 0.0_SRK, "intersection 1")
  ASSERT(points(2)%coord(1) .APPROXEQA. 2.0_SRK, "intersection 2")
  ASSERT(points(2)%coord(2) .APPROXEQA. 0.0_SRK, "intersection 2")
  CALL l%clear() 
  CALL p7%clear()
  CALL p8%clear()

  CALL p7%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL p8%init(DIM=2, X=4.0_SRK, Y=1.0_SRK)
  CALL l%set(p7, p8)
  npoints = -1
  CALL intersect(quad, l, npoints, points)
  ASSERT(npoints == 2, "Intersects")
  ASSERT(points(1)%coord(1) .APPROXEQA. 3.0_SRK, "intersection 1")
  ASSERT(points(1)%coord(2) .APPROXEQA. 1.0_SRK, "intersection 1")
  ASSERT(points(2)%coord(1) .APPROXEQA. 1.0_SRK, "intersection 2")
  ASSERT(points(2)%coord(2) .APPROXEQA. 1.0_SRK, "intersection 2")
  CALL l%clear() 
  CALL p7%clear()
  CALL p8%clear()

  CALL p7%init(DIM=2, X=0.0_SRK, Y=0.1_SRK)
  CALL p8%init(DIM=2, X=4.0_SRK, Y=0.1_SRK)
  CALL l%set(p7, p8)
  npoints = -1
  CALL intersect(quad, l, npoints, points)
  ASSERT(npoints == 4, "Intersects")
  ASSERT(points(1)%coord(1) .APPROXEQA. 0.4254033307585166_SRK, "intersection 1")
  ASSERT(points(1)%coord(2) .APPROXEQA. 0.1_SRK, "intersection 1")
  ASSERT(points(2)%coord(1) .APPROXEQA. 1.9745966692414834_SRK, "intersection 2")
  ASSERT(points(2)%coord(2) .APPROXEQA. 0.1_SRK, "intersection 2")
  ASSERT(points(3)%coord(1) .APPROXEQA. 2.19_SRK, "intersection 3")
  ASSERT(points(3)%coord(2) .APPROXEQA. 0.1_SRK, "intersection 3")
  ASSERT(points(4)%coord(1) .APPROXEQA. 0.1_SRK, "intersection 4")
  ASSERT(points(4)%coord(2) .APPROXEQA. 0.1_SRK, "intersection 4")
  CALL l%clear() 
  CALL p7%clear()
  CALL p8%clear()
ENDSUBROUTINE testIntersectLine
ENDPROGRAM testGeom_Quadrilateral8
