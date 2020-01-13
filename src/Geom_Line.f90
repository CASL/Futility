!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module defining "lines" for use in geometry.
!>
!> This module provides a derived data types for a "line" it is implicitly
!> for 1-D, 2-D, or 3-D space and this is determined by its members.
!> The line is defined by two points. The module also provides methods for
!> constructing the line, clearing its members, and getting the length of the
!> line, its midpoint, or an intersection with another line.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Line
USE IntrType
USE Geom_Points

IMPLICIT NONE
PRIVATE !Default contents of module to private
!
! List of Public items
PUBLIC :: LineType
PUBLIC :: OPERATOR(==)

!> @brief Type for a Line
!>
!> It is defined by 2 points and is implicitly 1-D, 2-D, or 3-D.
TYPE :: LineType
  !> The points definint the line segment p(1) is the starting point
  !> p(2) is the ending point
  TYPE(PointType) :: p1
  TYPE(PointType) :: p2
!
!List of type bound procedures
  CONTAINS
    !> @copybrief GeomLine::init_LineType
    !> @copydetails GeomLine::init_LineType
    PROCEDURE,PASS :: set => init_LineType
    !> @copybrief GeomLine::clear_LineType
    !> @copydetails GeomLine::clear_LineType
    PROCEDURE,PASS :: clear => clear_LineType
    !> @copybrief GeomLine::dim_LineType
    !> @copydetails GeomLine::dim_LineType
    PROCEDURE,PASS :: getDim => dim_LineType
    !> @copybrief GeomLine::length_lineType
    !> @copydetails GeomLine::length_lineType
    PROCEDURE,PASS :: length => length_lineType
    !> @copybrief GeomLine::midPointLine
    !> @copydetails GeomLine::midPointLine
    PROCEDURE,PASS :: midPoint => midPoint_LineType
    !> @copybrief GeomLine::intersect_LineType_and_LineType
    !> @copydetails GeomLine::intersect_LineType_and_LineType
    PROCEDURE,PASS :: intersect => intersect_LineType_and_LineType
    PROCEDURE,PASS :: intersectLine => intersect_LineType_and_LineType
    !> @copybrief GeomLine::distance_LineType_to_PointType
    !> @copydetails GeomLine::distance_LineType_to_PointType
    PROCEDURE,PASS :: distance2Point => distance_LineType_to_PointType
    !> @copybrief GeomLine::distance_LineType_to_LineType
    !> @copydetails GeomLine::distance_LineType_to_LineType
    PROCEDURE,PASS :: distance2Line => distance_LineType_to_LineType
    !> @copybrief GeomLine::pointIsLeft_LineType
    !> @copydetails GeomLine::pointIsLeft_LineType
    PROCEDURE,PASS :: pointIsLeft => pointIsLeft_LineType
    !> @copybrief GeomLine::pointIsRight_LineType
    !> @copydetails GeomLine::pointIsRight_LineType
    PROCEDURE,PASS :: pointIsRight => pointIsRight_LineType
ENDTYPE LineType

!> @brief Generic interface for 'is equal to' operator (==)
!>
!> Adds 'is equal to' capability for Line types
INTERFACE OPERATOR(==)
  !> @copybrief Geom_Line::isequal_LineType
  !> @copydetails Geom_Line::isequal_LineType
  MODULE PROCEDURE isequal_LineType
ENDINTERFACE
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Constructor for LineType
!> @param sp the starting point of the line
!> @param ep the end point of the line
!> @returns line the LineType object
!>
!> If sp and ep are not of the same type line is returned as NULL.
ELEMENTAL SUBROUTINE init_LineType(line,sp,ep)
  CLASS(LineType),INTENT(INOUT) :: line
  TYPE(PointType),INTENT(IN) :: sp,ep
  CALL line%clear()
  IF(sp%dim == ep%dim .AND. sp%dim > 0) THEN
    line%p1=sp
    line%p2=ep
  ENDIF
ENDSUBROUTINE init_LineType
!
!-------------------------------------------------------------------------------
!> @brief Deallocates the @c sp and @c ep components of line
!> @param line line segment of type @c LineType
ELEMENTAL SUBROUTINE clear_LineType(line)
  CLASS(LineType),INTENT(INOUT) :: line
  CALL line%p1%clear()
  CALL line%p2%clear()
ENDSUBROUTINE clear_LineType
!
!-------------------------------------------------------------------------------
!> @brief Returns the dimension of the LineType @e line
!> @param line line segment of type @c LineType
!> @returns @c n the dimension of LineType @c line
ELEMENTAL FUNCTION dim_LineType(line) RESULT(n)
  CLASS(LineType),INTENT(IN) :: line
  INTEGER(SIK) :: n
  n=0
  IF(line%p1%dim == line%p2%dim .AND. line%p1%dim > 0) n=line%p1%dim
ENDFUNCTION dim_LineType
!
!-------------------------------------------------------------------------------
!> @brief Computes the length of a line segment defined by 2 points
!> @param line line segment of type @c LineType
!> @returns @c length the length of @c line
ELEMENTAL FUNCTION length_LineType(line) RESULT(length)
  CLASS(LineType),INTENT(IN) :: line
  REAL(SRK) :: length
  length=Distance(line%p1,line%p2)
ENDFUNCTION length_LineType
!
!-------------------------------------------------------------------------------
!> @brief Computes the midpoint of line defined by two points
!> @param line line segment of type @c LineType
!> @returns @c p the midpoint of @c line
ELEMENTAL FUNCTION midPoint_LineType(line) RESULT(p)
  CLASS(LineType),INTENT(IN) :: line
  TYPE(PointType) :: p
  p=midPoint(line%p1,line%p2)
ENDFUNCTION midPoint_LineType
!
!-------------------------------------------------------------------------------
!> @brief Finds the intersection between 2 line segments (if it exists)
!> @param l1 the line to test for intersection
!> @param l2 the other line to test for intersection
!> @returns @c p the point of intersection
!> @note a return code is assigned to p%dim indicating the type of
!> intersection.
!>   -3: there is no intersection (disjoint)
!>   -2: the line segments overlap
!>   -1: problem with dummy arguments passed to routine
!>  > 0: success; an intersection point was found
ELEMENTAL FUNCTION intersect_LineType_and_LineType(l1,l2) RESULT(p)
  CLASS(LineType),INTENT(IN) :: l1
  TYPE(LineType),INTENT(IN) :: l2
  TYPE(PointType) :: p
  p%dim=-1
  IF(l1%p1%dim == l1%p2%dim .AND. l2%p1%dim == l2%p2%dim .AND. &
    l1%p1%dim == l2%p1%dim .AND. l1%p1%dim > 0) THEN
    SELECTCASE(l1%p1%dim)
      CASE(1)
        p%dim=-2 !Always collinear/overlap
      CASE(2)
        p=intersect_lines2D(l1%p1,l1%p2,l2%p1,l2%p2)
      CASE(3)
        p=intersect_lines3D(l1%p1,l1%p2,l2%p1,l2%p2)
    ENDSELECT
  ENDIF
ENDFUNCTION intersect_LineType_and_LineType
!
!-------------------------------------------------------------------------------
!> @brief Finds the intersection between two 2-D line segments (if it exists)
!> @param s1p0 the line to test for intersection
!> @param s1p1 the line to test for intersection
!> @param s2p0 the line to test for intersection
!> @param s2p1 the line to test for intersection
!> @returns @c p the point of intersection (if it exists)
!> @note a return code is assigned to p%dim indicating the type of
!> intersection.
!>   -3: there is no intersection (disjoint)
!>   -2: the line segments overlap
!>  > 0: success; an intersection point was found
ELEMENTAL FUNCTION intersect_lines2D(s1p0,s1p1,s2p0,s2p1) RESULT(p)
  TYPE(PointType),INTENT(IN) :: s1p0,s1p1,s2p0,s2p1
  TYPE(PointType) :: p
  REAL(SRK) :: u(2),v(2),w(2),d,s,t
  u(1)=s1p1%coord(1)-s1p0%coord(1)
  u(2)=s1p1%coord(2)-s1p0%coord(2)
  v(1)=s2p1%coord(1)-s2p0%coord(1)
  v(2)=s2p1%coord(2)-s2p0%coord(2)
  w(1)=s1p0%coord(1)-s2p0%coord(1)
  w(2)=s1p0%coord(2)-s2p0%coord(2)
  d=u(1)*v(2)-u(2)*v(1)
  s=v(1)*w(2)-v(2)*w(1)
  t=u(1)*w(2)-u(2)*w(1)
  IF(ABS(d) < EPSREAL) THEN
    !Segments are collinear
    IF(s /= 0._SRK .OR. t /= 0._SRK) THEN
      p%dim=-3 !parallel lines
    ELSE
      p%dim=-2 !overlap
    ENDIF
  ELSE
    s=s/d
    t=t/d
    IF(((0._SRK .APPROXLE. s) .AND. (s .APPROXLE. 1.0_SRK)) .AND. &
       ((0._SRK .APPROXLE. t) .AND. (t .APPROXLE. 1.0_SRK))) THEN
      !Success, intersection point was found.
      p=s1p0
      p%coord(1)=p%coord(1)+s*u(1)
      p%coord(2)=p%coord(2)+s*u(2)
    ELSE
      !would intersect if segments were infinite. Still store how close it
      !was though. this is useful for calling routines that might need to
      !know that the point was close (i.e. Coarse ray trace)
      p=s1p0
      p%dim=-3
      p%coord(1)=p%coord(1)+s*u(1)
      p%coord(2)=p%coord(2)+s*u(2)
    ENDIF
  ENDIF
ENDFUNCTION intersect_lines2D
!
!-------------------------------------------------------------------------------
!> @brief Finds the intersection between two 3-D line segments (if it exists)
!> @param s1p0 the line to test for intersection
!> @param s1p1 the line to test for intersection
!> @param s2p0 the line to test for intersection
!> @param s2p1 the line to test for intersection
!> @returns @c p the point of intersection (if it exists)
!> @note a return code is assigned to p%dim indicating the type of
!> intersection.
!>   -3: there is no intersection (disjoint)
!>   -2: the line segments overlap
!>  > 0: success; an intersection point was found
!>
ELEMENTAL FUNCTION intersect_lines3D(s1p0,s1p1,s2p0,s2p1) RESULT(p)
TYPE(PointType),INTENT(IN) :: s1p0,s1p1,s2p0,s2p1
  TYPE(PointType) :: p

  TYPE(LineType) :: s1,s2,dis
  REAL(SRK) :: mu1,mu2
  INTEGER(SIK) :: thisDim
  REAL(SRK) :: EPSREAL_LOCAL

  EPSREAL_LOCAL=EPSREAL*100._SRK
  p%dim=-3
  s1%p1=s1p0
  s1%p2=s1p1
  s2%p1=s2p0
  s2%p2=s2p1
  CALL distance_LineType_to_LineType(s1,s2,dis,mu1,mu2)
  thisDim=dis%p1%dim
  IF(thisDim > 0) THEN
    IF(0._SRK-EPSREAL_LOCAL <= mu1 .AND. mu1 <= 1._SRK+EPSREAL_LOCAL &
      .AND. 0._SRK-EPSREAL_LOCAL <= mu2 .AND. mu2 <= 1._SRK+EPSREAL_LOCAL &
        .AND. dis%length() < EPSREAL_LOCAL) THEN
      p=dis%p1
    ENDIF
  ELSEIF(thisDim == -2) THEN
    p%dim=-2
  ENDIF
  CALL s1%clear()
  CALL s2%clear()
  CALL dis%clear()
ENDFUNCTION intersect_lines3D
!
!-------------------------------------------------------------------------------
!> @brief Calculates the shortest line segment between two line segments
!>   s1 and s2 (if it exists).
!> @param s1 the first line segment to test
!> @param s2 the second line segment to test
!> @param mu1 the position of the start point of dis on the s1
!> @param mu2 the position of the end point of dis on the s2
!> @returns @c dis the shortest segment between s1 and s2 (if it exists)
!> @note a return code is assigned to dis%p1%dim and dis%p2%dim indicating
!>   the type of result.
!>   -3: the line segments are parallel
!>   -2: the line segments overlap
!>   -1: problem with dummy arguments passed to routine
!>  > 0: success; an shortest distance was found
!>
!> @note this routine only works in 3-D space.
!>
!> Calculates the line segment 'dis' that is the shortest route between
!> two line segments s1 and s2. Calculate also the values of mu1 and mu1 where
!>    dis%p1 = s1%p1 + mu1 (s1%p2 - s1%p1)
!>    dis%p2 = s2%p1 + mu2 (s2%p1 - s2%p1)
!>
ELEMENTAL SUBROUTINE distance_LineType_to_LineType(s1,s2,dis,mu1,mu2)
  CLASS(LineType),INTENT(IN) :: s1
  TYPE(LineType),INTENT(IN) :: s2
  TYPE(LineType),INTENT(OUT) :: dis
  REAL(SRK),OPTIONAL,INTENT(INOUT) :: mu1
  REAL(SRK),OPTIONAL,INTENT(INOUT) :: mu2
  TYPE(LineType) :: line
  REAL(SRK),DIMENSION(3) :: p13,p43,p21
  REAL(SRK) :: d1343,d4321,d1321,d4343,d2121
  REAL(SRK) :: denom,numer,d

  dis%p1%dim=-1
  dis%p2%dim=-1
  IF(s1%getdim() == 3 .AND. s2%getdim() == 3) THEN
    p21(1)=s1%p2%coord(1)-s1%p1%coord(1)
    p21(2)=s1%p2%coord(2)-s1%p1%coord(2)
    p21(3)=s1%p2%coord(3)-s1%p1%coord(3)
    p43(1)=s2%p2%coord(1)-s2%p1%coord(1)
    p43(2)=s2%p2%coord(2)-s2%p1%coord(2)
    p43(3)=s2%p2%coord(3)-s2%p1%coord(3)
    !Check the line segment
    IF((ABS(p21(1)) > EPSREAL .OR. ABS(p21(2)) > EPSREAL &
      .OR. ABS(p21(3)) > EPSREAL) .AND. (ABS(p43(1)) > EPSREAL &
        .OR. ABS(p43(2)) > EPSREAL .OR. ABS(p43(3)) > EPSREAL)) THEN
      p13(1)=s1%p1%coord(1)-s2%p1%coord(1)
      p13(2)=s1%p1%coord(2)-s2%p1%coord(2)
      p13(3)=s1%p1%coord(3)-s2%p1%coord(3)

      d1343=p13(1)*p43(1)+p13(2)*p43(2)+p13(3)*p43(3)
      d4321=p43(1)*p21(1)+p43(2)*p21(2)+p43(3)*p21(3)
      d1321=p13(1)*p21(1)+p13(2)*p21(2)+p13(3)*p21(3)
      d4343=p43(1)*p43(1)+p43(2)*p43(2)+p43(3)*p43(3)
      d2121=p21(1)*p21(1)+p21(2)*p21(2)+p21(3)*p21(3)
      denom=d2121*d4343-d4321*d4321
      IF(denom < EPSREAL) THEN
        dis%p1%dim=-3
        dis%p2%dim=-3
        d=ABS(p13(1)*p21(1)+p13(2)*p21(2)+p13(3)*p21(3))
        CALL line%set(s1%p1,s2%p1)
        IF(ABS(d-line%length()*s1%length()) < EPSREAL) THEN
          dis%p1%dim=-2
          dis%p2%dim=-2
        ENDIF
        CALL line%clear()
      ELSE
        numer=d1343*d4321-d1321*d4343
        mu1=numer/denom
        mu2=(d1343+d4321*mu1)/d4343
        dis%p1=s1%p1
        dis%p1%coord(1)=s1%p1%coord(1)+mu1*p21(1)
        dis%p1%coord(2)=s1%p1%coord(2)+mu1*p21(2)
        dis%p1%coord(3)=s1%p1%coord(3)+mu1*p21(3)
        dis%p2=s2%p1
        dis%p2%coord(1)=s2%p1%coord(1)+mu2*p43(1)
        dis%p2%coord(2)=s2%p1%coord(2)+mu2*p43(2)
        dis%p2%coord(3)=s2%p1%coord(3)+mu2*p43(3)
      ENDIF
    ENDIF
  ENDIF
ENDSUBROUTINE distance_LineType_to_LineType
!
!-------------------------------------------------------------------------------
!> @brief Finds the shortest squared distance between a point and a segment
!> @param line the LineType
!> @param p the PointType
!> @returns @c d2 the minimum squared distance between the point and the line
!> @note if @c d2<0, then there was a problem with the input arguments. If
!> @c d2==0 than the point is on the segment.
!>
ELEMENTAL FUNCTION distance_LineType_to_PointType(line,p) RESULT(d2)
  CLASS(LineType),INTENT(IN) :: line
  TYPE(PointType),INTENT(IN) :: p
  REAL(SRK) :: d2
  d2=-1._SRK
  IF(line%p1%dim == line%p2%dim .AND. line%p1%dim == p%dim) THEN
    SELECTCASE(p%dim)
      CASE(1)
        d2=distance1D_to_point(line%p1,line%p2,p)
      CASE(2)
        d2=distance2D_to_point(line%p1,line%p2,p)
      CASE(3)
        d2=distance3D_to_point(line%p1,line%p2,p)
    ENDSELECT
  ENDIF
ENDFUNCTION distance_LineType_to_PointType
!
!-------------------------------------------------------------------------------
!> @brief Finds the shortest squared distance between a point and a segment in
!> 1-D.
!> @param a start point of line segment
!> @param b end point of line segment
!> @param c the point to find the distance from line segment ab
!> @returns @c d2 the minimum squared distance between the point and the line
!>
ELEMENTAL FUNCTION distance1D_to_point(a,b,c) RESULT(d2)
  TYPE(PointType),INTENT(IN) :: a,b,c
  REAL(SRK) :: d2
  IF(c%coord(1) < a%coord(1)) THEN
    d2=(a%coord(1)-c%coord(1))*(a%coord(1)-c%coord(1))
  ELSEIF(c%coord(1) > b%coord(1)) THEN
    d2=(c%coord(1)-b%coord(1))*(c%coord(1)-b%coord(1))
  ELSE
    d2=0.0_SRK
  ENDIF
ENDFUNCTION distance1D_to_point
!
!-------------------------------------------------------------------------------
!> @brief Finds the shortest squared distance between a point and a segment in
!> 2-D.
!> @param a start point of line segment
!> @param b end point of line segment
!> @param c the point to find the distance from line segment ab
!> @returns @c d2 the minimum squared distance between the point and the line
!>
ELEMENTAL FUNCTION distance2D_to_point(a,b,c) RESULT(d2)
  TYPE(PointType),INTENT(IN) :: a,b,c
  REAL(SRK) :: d2
  REAL(SRK) :: e,f
  REAL(SRK) :: ab(2),ac(2),bc(2)

  ab(1)=b%coord(1)-a%coord(1)
  ab(2)=b%coord(2)-a%coord(2)
  ac(1)=c%coord(1)-a%coord(1)
  ac(2)=c%coord(2)-a%coord(2)

  e=ac(1)*ab(1)+ac(2)*ab(2)
  IF(e <= 0.0_SRK) THEN
    !c projects onto ab outside a
    d2=ac(1)*ac(1)+ac(2)*ac(2)
  ELSE
    f=ab(1)*ab(1)+ab(2)*ab(2)
    IF(e >= f) THEN
      !c projects onto ab outside b
      bc(1)=c%coord(1)-b%coord(1)
      bc(2)=c%coord(2)-b%coord(2)
      d2=bc(1)*bc(1)+bc(2)*bc(2)
    ELSE
      !c projects onto ab
      d2=(ac(1)*ac(1)+ac(2)*ac(2))-e*e/f
    ENDIF
  ENDIF
ENDFUNCTION distance2D_to_point
!
!-------------------------------------------------------------------------------
!> @brief Finds the shortest squared distance between a point and a segment in
!> 3-D.
!> @param a start point of line segment
!> @param b end point of line segment
!> @param c the point to find the distance from line segment ab
!> @returns @c d2 the minimum squared distance between the point and the line
!>
ELEMENTAL FUNCTION distance3D_to_point(a,b,c) RESULT(d2)
  TYPE(PointType),INTENT(IN) :: a,b,c
  REAL(SRK) :: d2
  REAL(SRK) :: e,f
  REAL(SRK) :: ab(3),ac(3),bc(3)

  ab(1)=b%coord(1)-a%coord(1)
  ab(2)=b%coord(2)-a%coord(2)
  ab(3)=b%coord(3)-a%coord(3)
  ac(1)=c%coord(1)-a%coord(1)
  ac(2)=c%coord(2)-a%coord(2)
  ac(3)=c%coord(3)-a%coord(3)

  e=ac(1)*ab(1)+ac(2)*ab(2)+ac(3)*ab(3)
  IF(e <= 0.0_SRK) THEN
    !c projects onto ab outside a
    d2=ac(1)*ac(1)+ac(2)*ac(2)+ac(3)*ac(3)
  ELSE
    f=ab(1)*ab(1)+ab(2)*ab(2)+ab(3)*ab(3)
    IF(e >= f) THEN
      !c projects onto ab outside b
      bc(1)=c%coord(1)-b%coord(1)
      bc(2)=c%coord(2)-b%coord(2)
      bc(3)=c%coord(3)-b%coord(3)
      d2=bc(1)*bc(1)+bc(2)*bc(2)+bc(3)*bc(3)
    ELSE
      !c projects onto ab
      d2=(ac(1)*ac(1)+ac(2)*ac(2)+ac(3)*ac(3))-e*e/f
    ENDIF
  ENDIF
ENDFUNCTION distance3D_to_point
!
!-------------------------------------------------------------------------------
!> @brief Determines whether a point is to the left of the line.
!> @param line the line object
!> @param pt the point object
!> @returns @c bool the boolean result of the operation
!>
!> Function is elemental so it can be used on an array of lines.
ELEMENTAL FUNCTION pointIsLeft_LineType(line,pt) RESULT(bool)
  CLASS(LineType),INTENT(IN) :: line
  TYPE(PointType),INTENT(IN) :: pt
  LOGICAL(SBK) :: bool
  bool=.FALSE.
  IF((pt%dim > 1) .AND. (line%getDim() > 1)) &
    bool=(line%p2%coord(1)-line%p1%coord(1))*(pt%coord(2)-line%p1%coord(2))- &
      (line%p2%coord(2)-line%p1%coord(2))*(pt%coord(1)-line%p1%coord(1)) > 0.0_SRK
ENDFUNCTION pointIsLeft_LineType
!
!-------------------------------------------------------------------------------
!> @brief Determines whether a point is to the right of the line.
!> @param line the line object
!> @param pt the point object
!> @returns @c bool the boolean result of the operation
!>
!> Function is elemental so it can be used on an array of lines.
ELEMENTAL FUNCTION pointIsRight_LineType(line,pt) RESULT(bool)
  CLASS(LineType),INTENT(IN) :: line
  TYPE(PointType),INTENT(IN) :: pt
  LOGICAL(SBK) :: bool
  bool=.FALSE.
  IF((pt%dim > 1) .AND. (line%getDim() > 1)) &
    bool=(line%p2%coord(1)-line%p1%coord(1))*(pt%coord(2)-line%p1%coord(2))- &
      (line%p2%coord(2)-line%p1%coord(2))*(pt%coord(1)-line%p1%coord(1)) < 0.0_SRK
ENDFUNCTION pointIsRight_LineType
!
!-------------------------------------------------------------------------------
!> @brief Defines the 'is equal to' operation between two lines e.g. @c
!>        l0 == l1
!> @param p0 the first line
!> @param p1 the second line
!> @returns @c bool the boolean result of the operation
!>
!> Function is elemental so it can be used on an array of lines.
ELEMENTAL FUNCTION isequal_LineType(l0,l1) RESULT(bool)
  TYPE(LineType),INTENT(IN) :: l0,l1
  LOGICAL(SBK) :: bool
  bool=.FALSE.
  IF(l0%p1 == l1%p1) bool=l0%p2 == l1%p2
ENDFUNCTION isequal_LineType
!
ENDMODULE Geom_Line
