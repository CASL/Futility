!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testGeom_Triangle6
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE Strings
USE Geom_Triangle6
USE Geom_Points
USE Geom_Line
USE Geom_QuadraticSegment

IMPLICIT NONE

CREATE_TEST('TRIANGLE TYPE')
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
  TYPE(Triangle6_2D) :: tri
  INTEGER(SIK) :: i
  DO i=1,6
    CALL tri%points(i)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  ENDDO

  CALL tri%clear()
  DO i=1,6
    ASSERT(tri%points(i)%dim == 0, "Value not cleared")
    ASSERT(.NOT. ALLOCATED(tri%points(i)%coord), "Value not cleared")
  ENDDO
ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
SUBROUTINE testInit()
  TYPE(Triangle6_2D) :: tri
  TYPE(PointType) :: tpoints(6)

  CALL tpoints(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL tpoints(2)%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL tpoints(3)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL tpoints(4)%init(DIM=2, X=0.5_SRK, Y=0.0_SRK)
  CALL tpoints(5)%init(DIM=2, X=0.5_SRK, Y=0.5_SRK)
  CALL tpoints(6)%init(DIM=2, X=0.0_SRK, Y=0.5_SRK)
  CALL tri%set(tpoints)
  ASSERT(tri%points(1) == tpoints(1), "Point assigned incorrectly")
  ASSERT(tri%points(2) == tpoints(2), "Point assigned incorrectly")
  ASSERT(tri%points(3) == tpoints(3), "Point assigned incorrectly")
  ASSERT(tri%points(4) == tpoints(4), "Point assigned incorrectly")
  ASSERT(tri%points(5) == tpoints(5), "Point assigned incorrectly")
  ASSERT(tri%points(6) == tpoints(6), "Point assigned incorrectly")
ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
SUBROUTINE testInterpolate()
  TYPE(Triangle6_2D) :: tri
  TYPE(PointType) :: tpoints(6), p
  CALL tpoints(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL tpoints(2)%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL tpoints(3)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL tpoints(4)%init(DIM=2, X=0.5_SRK, Y=0.0_SRK)
  CALL tpoints(5)%init(DIM=2, X=0.5_SRK, Y=0.5_SRK)
  CALL tpoints(6)%init(DIM=2, X=0.0_SRK, Y=0.5_SRK)
  CALL tri%set(tpoints)
  p = interpolate(tri, 0.0_SRK, 0.0_SRK)
  ASSERT( p == tpoints(1), "Wrong point")

  p = interpolate(tri, 1.0_SRK, 0.0_SRK)
  ASSERT( p == tpoints(2), "Wrong point")

  p = interpolate(tri, 0.0_SRK, 1.0_SRK)
  ASSERT( p == tpoints(3), "Wrong point")

  p = interpolate(tri, 0.5_SRK, 0.0_SRK)
  ASSERT( p == tpoints(4), "Wrong point")

  p = interpolate(tri, 0.5_SRK, 0.5_SRK)
  ASSERT( p == tpoints(5), "Wrong point")

  p = interpolate(tri, 0.0_SRK, 0.5_SRK)
  ASSERT( p == tpoints(6), "Wrong point")
ENDSUBROUTINE testInterpolate
!
!-------------------------------------------------------------------------------
SUBROUTINE testDerivative()
  TYPE(Triangle6_2D) :: tri
  TYPE(PointType) :: tpoints(6), dr, ds, p_r, p_s
  CALL tpoints(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL tpoints(2)%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL tpoints(3)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL tpoints(4)%init(DIM=2, X=0.5_SRK, Y=0.0_SRK)
  CALL tpoints(5)%init(DIM=2, X=0.5_SRK, Y=0.5_SRK)
  CALL tpoints(6)%init(DIM=2, X=0.0_SRK, Y=0.5_SRK)
  CALL p_r%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL p_s%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL tri%set(tpoints)

  CALL derivative(tri, 0.0_SRK, 0.0_SRK, dr, ds)
  ASSERT( dr .APPROXEQA. p_r, "Wrong point")
  ASSERT( ds .APPROXEQA. p_s, "Wrong point")
ENDSUBROUTINE testDerivative
!
!-------------------------------------------------------------------------------
SUBROUTINE testArea()
  TYPE(Triangle6_2D) :: tri
  TYPE(PointType) :: tpoints(6)
  REAL(SRK) :: a
  CALL tpoints(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL tpoints(2)%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL tpoints(3)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL tpoints(4)%init(DIM=2, X=0.5_SRK, Y=0.0_SRK)
  CALL tpoints(5)%init(DIM=2, X=0.5_SRK, Y=0.5_SRK)
  CALL tpoints(6)%init(DIM=2, X=0.0_SRK, Y=0.5_SRK)
  CALL tri%set(tpoints)
  a = area(tri)
  ASSERT( SOFTEQ(a, 0.5_SRK, 1.0E-6_SRK), "Wrong area")
  CALL tpoints(1)%clear()
  CALL tpoints(2)%clear()
  CALL tpoints(3)%clear()
  CALL tpoints(4)%clear()
  CALL tpoints(5)%clear()
  CALL tpoints(6)%clear()
  CALL tri%clear()
  CALL tpoints(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL tpoints(2)%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL tpoints(3)%init(DIM=2, X=2.0_SRK, Y=2.0_SRK)
  CALL tpoints(4)%init(DIM=2, X=1.5_SRK, Y=0.25_SRK)
  CALL tpoints(5)%init(DIM=2, X=3.0_SRK, Y=1.0_SRK)
  CALL tpoints(6)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL tri%set(tpoints)
  a = area(tri)
  ASSERT( SOFTEQ(a, 3.0_SRK, 1.0E-6_SRK), "Wrong area")
ENDSUBROUTINE testArea
!
!-------------------------------------------------------------------------------
SUBROUTINE testR2P()
  TYPE(Triangle6_2D) :: tri
  TYPE(PointType) :: tpoints(6), p
  CALL tpoints(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL tpoints(2)%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL tpoints(3)%init(DIM=2, X=2.0_SRK, Y=2.0_SRK)
  CALL tpoints(4)%init(DIM=2, X=1.5_SRK, Y=0.25_SRK)
  CALL tpoints(5)%init(DIM=2, X=3.0_SRK, Y=1.0_SRK)
  CALL tpoints(6)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL tri%set(tpoints)
  p = real_to_parametric(tri, tpoints(1))
  ASSERT( SOFTEQ(p%coord(1), 0.0_SRK, 1.0E-6_SRK), "Wrong coordinate")
  ASSERT( SOFTEQ(p%coord(2), 0.0_SRK, 1.0E-6_SRK), "Wrong coordinate")
  p = real_to_parametric(tri, tpoints(2))
  ASSERT( SOFTEQ(p%coord(1), 1.0_SRK, 1.0E-6_SRK), "Wrong coordinate")
  ASSERT( SOFTEQ(p%coord(2), 0.0_SRK, 1.0E-6_SRK), "Wrong coordinate")
  p = real_to_parametric(tri, tpoints(4))
  ASSERT( SOFTEQ(p%coord(1), 0.5_SRK, 1.0E-6_SRK), "Wrong coordinate")
  ASSERT( SOFTEQ(p%coord(2), 0.0_SRK, 1.0E-6_SRK), "Wrong coordinate")
ENDSUBROUTINE testR2P
!
!-------------------------------------------------------------------------------
SUBROUTINE testPointInside()
  TYPE(Triangle6_2D) :: tri
  TYPE(PointType) :: tpoints(6), p
  LOGICAL(SBK) :: bool
  CALL tpoints(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL tpoints(2)%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL tpoints(3)%init(DIM=2, X=2.0_SRK, Y=2.0_SRK)
  CALL tpoints(4)%init(DIM=2, X=1.5_SRK, Y=0.25_SRK)
  CALL tpoints(5)%init(DIM=2, X=3.0_SRK, Y=1.0_SRK)
  CALL tpoints(6)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL tri%set(tpoints)

  CALL p%init(DIM=2, X=1.0_SRK, Y=0.5_SRK)
  bool = pointInside(tri, p)
  CALL p%clear()
  ASSERT( bool, "point inside")

  CALL p%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  bool = pointInside(tri, p)
  CALL p%clear()
  ASSERT( .NOT. bool, "point not inside")
ENDSUBROUTINE testPointInside
!
!-------------------------------------------------------------------------------
SUBROUTINE testIntersectLine()
  TYPE(Triangle6_2D) :: tri
  TYPE(LineType) :: l
  TYPE(PointType) :: tpoints(6), p7, p8, points(4)
  INTEGER(SIK) :: npoints
  CALL tpoints(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL tpoints(2)%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL tpoints(3)%init(DIM=2, X=2.0_SRK, Y=2.0_SRK)
  CALL tpoints(4)%init(DIM=2, X=1.5_SRK, Y=0.25_SRK)
  CALL tpoints(5)%init(DIM=2, X=3.0_SRK, Y=1.0_SRK)
  CALL tpoints(6)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL tri%set(tpoints)

  CALL p7%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p8%init(DIM=2, X=4.0_SRK, Y=0.0_SRK)
  CALL l%set(p7, p8)
  npoints = -1
  CALL intersect(tri, l, npoints, points)
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
  CALL intersect(tri, l, npoints, points)
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
  CALL intersect(tri, l, npoints, points)
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
ENDPROGRAM testGeom_Triangle6
