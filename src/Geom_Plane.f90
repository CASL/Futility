!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module defining "planes" for use in geometry.
!>
!> This module provides a derived data types for a "plane" which must exist in
!> 3-D space. The plane is defined by a point and a normal vector. The point
!> also indicates the minimnum extents of the plane so it is semi-finite.
!> The module also provides methods for constructing a plane, deallocating the
!> plane, and getting the intersection of a plane and line.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Plane
USE IntrType
USE Geom_Points
USE Geom_Line

IMPLICIT NONE
PRIVATE !Default contents of module to private
!
! List of Public items
PUBLIC :: PlaneType
PUBLIC :: OPERATOR(==)

!> @brief Type for a Plane defined by a normal vector @c n and a point @c v0.
TYPE :: PlaneType
  !> The vector that is normal to the plane's surface
  REAL(SRK),DIMENSION(3) :: n=0._SRK
  !> The point that defines the plane, it must also have the minimum values
  !> of the plane in x, y, and z.
  TYPE(PointType) :: v0
!
!List of type bound procedures
  CONTAINS
    !> @copybrief GeomPlane::set_PlaneType
    !> @copydetails GeomPlane::set_PlaneType
    PROCEDURE,PASS :: set => set_PlaneType
    !> @copybrief GeomPlane::clear_PlaneType
    !> @copydetails GeomPlane::clear_PlaneType
    PROCEDURE,PASS :: clear => clear_PlaneType
    !> @copybrief GeomPlane::intersect_PlaneType_and_LineType
    !> @copydetails GeomPlane::intersect_PlaneType_and_LineType
    PROCEDURE,PASS :: intersectLine => intersect_PlaneType_and_LineType
ENDTYPE PlaneType

!> @brief Generic interface for 'is equal to' operator (==)
!>
!> Adds 'is equal to' capability for Plane types
INTERFACE OPERATOR(==)
  !> @copybrief Geom_Plane::isequal_PlaneType
  !> @copydetails Geom_Plane::isequal_PlaneType
  MODULE PROCEDURE isequal_PlaneType
ENDINTERFACE
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Sets value for PlaneType
!> @param plane the plane type to set the value of
!> @param n_in a 3-dimensional vector representing the direction normal to the
!>        plane's surface
!> @param v0 a point lying in the plane, also the minimum extent of the plane
!>
PURE SUBROUTINE set_PlaneType(plane,n_in,v0)
  CLASS(PlaneType),INTENT(INOUT) :: plane
  REAL(SRK),INTENT(IN) :: n_in(3)
  TYPE(PointType),INTENT(IN) :: v0
  REAL(SRK) :: magn,n(3)

  CALL clear_PlaneType(plane)
  !Make sure the point v0 has data
  IF(v0%dim == 3 .AND. ALLOCATED(v0%coord)) THEN
    !Make sure n is normalized to 1
    magn=SQRT(n_in(1)*n_in(1)+n_in(2)*n_in(2)+n_in(3)*n_in(3))
    n=n_in
    IF(magn > 1.0_SRK) n=n_in/magn

    plane%n=n
    plane%v0=v0
  ENDIF
ENDSUBROUTINE set_PlaneType
!
!-------------------------------------------------------------------------------
!> @brief Clears the PlaneType object
!> @param plane the plane to clear
ELEMENTAL SUBROUTINE clear_PlaneType(plane)
  CLASS(PlaneType),INTENT(INOUT) :: plane
  plane%n=0._SRK
  CALL plane%v0%clear()
ENDSUBROUTINE clear_PlaneType
!
!-------------------------------------------------------------------------------
!> @brief Finds the intersection between a plane and line segment (if it exists)
!> @param plane the plane to test for intersection
!> @param line the segment to test for intersection
!> @returns @c point the point of intersection (if it exists)
!> @note a return code is assigned to point%dim indicating the type of
!> intersection. @n
!>   -3: there is no intersection (disjoint) @n
!>   -2: the line segment lies within the plane @n
!>   -1: problem with dummy arguments passed to routine @n
!>  > 0: success; an intersection point was found @n
ELEMENTAL FUNCTION intersect_PlaneType_and_LineType(plane,line) RESULT(point)
  CLASS(PlaneType),INTENT(IN) :: plane
  TYPE(LineType),INTENT(IN) :: line
  TYPE(PointType) :: point
  REAL(SRK) :: d,n,s
  REAL(SRK),DIMENSION(3) :: u,w
  point%dim=-1
  !Verify dummy arguments have data
  IF((plane%v0%dim == 3 .AND. ALLOCATED(plane%v0%coord)) .AND. &
      (line%p1%dim == 3 .AND. ALLOCATED(line%p1%coord)) .AND. &
      (line%p2%dim == 3 .AND. ALLOCATED(line%p2%coord))) THEN
    u(1)=line%p2%coord(1)-line%p1%coord(1)
    u(2)=line%p2%coord(2)-line%p1%coord(2)
    u(3)=line%p2%coord(3)-line%p1%coord(3)
    w(1)=line%p1%coord(1)-plane%v0%coord(1)
    w(2)=line%p1%coord(2)-plane%v0%coord(2)
    w(3)=line%p1%coord(3)-plane%v0%coord(3)
    d=plane%n(1)*u(1)+plane%n(2)*u(2)+plane%n(3)*u(3)
    n=-(plane%n(1)*w(1)+plane%n(2)*w(2)+plane%n(3)*w(3))
    IF(ABS(d) < EPSREAL) THEN
      IF(n .APPROXEQA. 0._SRK) THEN
        point%dim=-2 !collinear
      ELSE
        point%dim=-3 !parallel
      ENDIF
    ELSE
      s=n/d
      IF(0._SRK <= s .AND. s <= 1._SRK) THEN
        !Compute the intersection
        point%dim=line%p1%dim
        ALLOCATE(point%coord(point%dim))
        point%coord(1)=line%p1%coord(1)+s*u(1)
        point%coord(2)=line%p1%coord(2)+s*u(2)
        point%coord(3)=line%p1%coord(3)+s*u(3)
      ELSE
        point%dim=-3 !would intersect if infinite segment length
      ENDIF
    ENDIF
  ENDIF
ENDFUNCTION intersect_PlaneType_and_LineType
!
!-------------------------------------------------------------------------------
!> @brief Defines the 'is equal to' operation between two planes e.g. @c
!>        plane1 == plane2
!> @param p0 the first plane
!> @param p1 the second plane
!> @returns @c bool the boolean result of the operation
!>
!> Function is elemental so it can be used on an array of planes.
ELEMENTAL FUNCTION isequal_PlaneType(p0,p1) RESULT(bool)
  TYPE(PlaneType),INTENT(IN) :: p0,p1
  LOGICAL(SBK) :: bool
  bool=.FALSE.
  IF(ALL(p0%n .APPROXEQA. p1%n)) bool=(p0%v0 == p1%v0)
ENDFUNCTION isequal_PlaneType
!
ENDMODULE Geom_Plane
