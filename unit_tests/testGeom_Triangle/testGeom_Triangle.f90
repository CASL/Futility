!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testGeom_Triangle
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE Strings
USE Geom_Triangle
USE Geom_Points
USE Geom_Line

IMPLICIT NONE

CREATE_TEST('TRIANGLE TYPE')
REGISTER_SUBTEST('CLEAR', testClear)
REGISTER_SUBTEST('INIT', testInit)
REGISTER_SUBTEST('INTERPOLATE', testInterpolate)
REGISTER_SUBTEST('POINT INSIDE', testPointInside)
!REGISTER_SUBTEST('INTERSECT LINE', testIntersectLine)
FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testClear()
  TYPE(Triangle_2D) :: tri
  INTEGER(SIK) :: i
  DO i=1,3
    CALL tri%points(i)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  ENDDO

  CALL tri%clear()
  DO i=1,3
    ASSERT(tri%points(i)%dim == 0, "Value not cleared")
    ASSERT(.NOT. ALLOCATED(tri%points(i)%coord), "Value not cleared")
  ENDDO
ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
SUBROUTINE testInit()
  TYPE(Triangle_2D) :: tri
  TYPE(PointType) :: p1, p2, p3

  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  CALL tri%set(p1, p2, p3)
  ASSERT(tri%points(1) == p1, "Point assigned incorrectly")
  ASSERT(tri%points(2) == p2, "Point assigned incorrectly")
  ASSERT(tri%points(3) == p3, "Point assigned incorrectly")
ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
SUBROUTINE testInterpolate()
  TYPE(Triangle_2D) :: tri
  TYPE(PointType) :: p1, p2, p3, p

  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL tri%set(p1, p2, p3)
  p = interpolate(tri, 0.0_SRK, 0.0_SRK)
  ASSERT( p == p1, "Wrong point")
  p = interpolate(tri, 1.0_SRK, 0.0_SRK)
  ASSERT( p == p2, "Wrong point")
  p = interpolate(tri, 0.0_SRK, 1.0_SRK)
  ASSERT( p == p3, "Wrong point")
  p = interpolate(tri, 0.5_SRK, 0.5_SRK)
  ASSERT( p%coord(1) .APPROXEQA. 1.0_SRK, "Wrong point")
  ASSERT( p%coord(2) .APPROXEQA. 0.5_SRK, "Wrong point")
ENDSUBROUTINE testInterpolate
!
!-------------------------------------------------------------------------------
SUBROUTINE testArea()
  TYPE(Triangle_2D) :: tri
  TYPE(PointType) :: p1, p2, p3
  REAL(SRK) :: a

  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL tri%set(p1, p2, p3)
  a = area(tri)
  ASSERT( a .APPROXEQA. 0.5_SRK, "Wrong area")
ENDSUBROUTINE testArea
!
!-------------------------------------------------------------------------------
SUBROUTINE testPointInside()
  TYPE(Triangle_2D) :: tri
  TYPE(PointType) :: p1, p2, p3, p
  LOGICAL(SBK) :: bool

  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL tri%set(p1, p2, p3)

  CALL p%init(DIM=2, X=0.5_SRK, Y=0.1_SRK)
  bool = pointInside(tri, p)
  CALL p%clear()
  ASSERT( bool, "point inside")

  CALL p%init(DIM=2, X=0.5_SRK, Y=0.0_SRK)
  bool = pointInside(tri, p)
  CALL p%clear()
  ASSERT( bool, "point inside")

  CALL p%init(DIM=2, X=0.5_SRK, Y=-0.1_SRK)
  bool = pointInside(tri, p)
  CALL p%clear()
  ASSERT( .NOT. bool, "point inside")
ENDSUBROUTINE testPointInside
!
!-------------------------------------------------------------------------------
SUBROUTINE testIntersectLine()
  TYPE(Triangle_2D) :: tri
  TYPE(LineType) :: l
  TYPE(PointType) :: p1, p2, p3, p4, p5, ipoint1, ipoint2
  INTEGER(SIK) :: npoints
  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL p4%init(DIM=2, X=2.0_SRK, Y=1.0_SRK)
  CALL p5%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)

  ! 2 intersection
  CALL tri%set(p1, p2, p3)
  CALL l%set(p4, p5)
  npoints = -1
  CALL intersect(tri, l, npoints, ipoint1, ipoint2)
  ASSERT(npoints == 2, "Intersects")
  ASSERT(ipoint1%coord(1) .APPROXEQA. 0.0_SRK, "intersection 1")
  ASSERT(ipoint1%coord(2) .APPROXEQA. 0.0_SRK, "intersection 1")
  ASSERT(ipoint2%coord(1) .APPROXEQA. 1.0_SRK, "intersection 2")
  ASSERT(ipoint2%coord(2) .APPROXEQA. 0.5_SRK, "intersection 2")

  ! 1 intersection
  CALL l%clear() 
  CALL p4%clear()
  CALL p5%clear()
  CALL p4%init(DIM=2, X=-1.0_SRK, Y=1.0_SRK)
  CALL p5%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL l%set(p4, p5)
  npoints = -1
  CALL intersect(tri, l, npoints, ipoint1, ipoint2)
  ASSERT(npoints == 1, "Intersects")
  ASSERT(ipoint1%coord(1) .APPROXEQ. 0.0_SRK, "intersection 1")
  ASSERT(ipoint1%coord(2) .APPROXEQ. 0.0_SRK, "intersection 1")

  ! 0 intersection
  CALL l%clear() 
  CALL p4%clear()
  CALL p5%clear()
  CALL p4%init(DIM=2, X=1.0_SRK, Y=-1.0_SRK)
  CALL p5%init(DIM=2, X=0.0_SRK, Y=-1.0_SRK)
  CALL l%set(p4, p5)
  npoints = -1
  CALL intersect(tri, l, npoints, ipoint1, ipoint2)
  ASSERT(npoints == 0, "Intersects")
ENDSUBROUTINE testIntersectLine
ENDPROGRAM testGeom_Triangle
