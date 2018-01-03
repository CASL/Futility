!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module defining bounding "boxes" for use in
!> geometry.
!>
!> This module presently provides a derived data type for an oriented bounding
!> box (OBB). The OBB is defined by an origin point, an orientation matrix
!> indicating the directions and a vector indicating the extent of the box for
!> each dimension. It is inherently a 3-D object, but can represent a 2-D surface
!> in 3-D space or 2-D space. This is determined by the dimensionality of its
!> member starting point. The module also provides methods for constructing and
!> clearing the OBBoxType, and intersecting it with a line.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref Geom_Points "Geom_Points": @copybrief Geom_Points
!>  - @ref Geom_Line "Geom_Line": @copybrief Geom_Line
!>
!> @author Zhouyu Liu
!>    @date 03/19/2012
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Box
  USE IntrType
  USE Geom_Points
  USE Geom_Line
  USE Geom_Plane
  IMPLICIT NONE

  PRIVATE
!
!List of public items
  PUBLIC OBBoxType
  PUBLIC ABBoxType
  PUBLIC :: OPERATOR(==)

  !> @brief Type for a Box
  TYPE OBBoxType
    !> The start corner point
    TYPE(PointType) :: p0
    !> The vector that points in the direction of the first dimension
    REAL(SRK) :: u(3,3)=0.0_SRK
    !> The extent of every dimension
    REAL(SRK) :: e(3)=0.0_SRK
    !
    !List of type bound procedures
    CONTAINS
      !> @copybrief Geom_Box::set_OBBoxType
      !> @copydetail Geom_Box::set_OBBoxType
      PROCEDURE,PASS :: set => set_OBBoxType
      !> @copybrief Geom_Box::clear_OBBoxType
      !> @copydetail Geom_Box::clear_OBBoxType
      PROCEDURE,PASS :: clear => clear_OBBoxType
      !> @copybrief Geom_Box::intersect_OBBoxType_and_LineType
      !> @copydetail Geom_Box::intersect_OBBoxType_and_LineType
      PROCEDURE,PASS :: intersectLine => intersect_OBBoxType_and_LineType
      !> @copybrief Geom_Box::getLines_OBBoxType
      !> @copydetail Geom_Box::getLines_OBBoxType
      PROCEDURE,PASS :: getLines => getLines_OBBoxType
      !> @copybrief Geom_Box::getPlanes_OBBoxType
      !> @copydetail Geom_Box::getPlanes_OBBoxType
      PROCEDURE,PASS :: getPlanes => getPlanes_OBBoxType
      !> @copybrief Geom_Box::inside_OBBoxType
      !> @copydetail Geom_Box::inside_OBBoxType
      PROCEDURE,PASS :: inside => inside_OBBoxType
  ENDTYPE OBBoxType

  !> @breif Lightweight type for axis-aligned box
  !>
  !> Derived type for storing a simple axis-aligned box. This is used to bound
  !> each of the cuboids for the cartesian mesh overlay.
  TYPE :: ABBoxType
    !> Minimum x extent of the box
    REAL(SRK) :: xMin=0.0_SRK
    !> Maximum x extent of the box
    REAL(SRK) :: xMax=0.0_SRK
    !> Minimum y extent of the box
    REAL(SRK) :: yMin=0.0_SRK
    !> Maximum y extent of the box
    REAL(SRK) :: yMax=0.0_SRK
    !> Minimum z extent of the box
    REAL(SRK) :: zMin=0.0_SRK
    !> Maximum z extent of the box
    REAL(SRK) :: zMax=0.0_SRK
    !> Is the origin of our coordinate system inside of the box
    LOGICAL(SBK) :: isOrigin=.FALSE.
    !> Is the box 3D?
    LOGICAL(SBK) :: is3D=.FALSE.
    !> If the box has been assigned values
    LOGICAL(SBK) :: isSet=.FALSE.
!
!List of type bound procedure for the object
    CONTAINS
      !> @copybrief Geom_Box::inside_ABBoxType
      !> @copydetail Geom_Box::inside_ABBoxType
      PROCEDURE,PASS :: inside => inside_ABBoxType
      !> @copybrief Geom_Box::set_ABBoxType
      !> @copydetail Geom_Box::set_ABBoxType
      PROCEDURE,PASS :: set => set_ABBoxType
      !> @copybrief Geom_Box::clear_ABBoxType
      !> @copydetail Geom_Box::clear_ABBoxType
      PROCEDURE,PASS :: clear => clear_ABBoxType
  ENDTYPE ABBoxType

  !> @brief Generic interface for 'is equal to' operator (==)
  !>
  !> Adds 'is equal to' capability for OBBox types
  INTERFACE OPERATOR(==)
    !> @copybrief Geom_Box::isequal_OBBoxType
    !> @copydetails Geom_Box::isequal_OBBoxType
    MODULE PROCEDURE isequal_OBBoxType
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Clears an OBBoxType object's atrributes
!> @param thisOBB the OBBoxType object to clear
!>
    ELEMENTAL SUBROUTINE clear_OBBoxType(thisBox)
      CLASS(OBBoxType),INTENT(INOUT) :: thisBox
      CALL thisBox%p0%clear()
      thisBox%u=0.0_SRK
      thisBox%e=0.0_SRK
    ENDSUBROUTINE clear_OBBoxType
!
!-------------------------------------------------------------------------------
!> @brief Sets a OBBoxType object's atrributes
!> @param box the OBBoxType object to set
!> @param p0 the start corner point of the box
!> @param e_in the extent of every dimension
!> @param u1_in the vector that points to the first dimension
!> @param u2_in the vector that points to the second dimension
!> @param u3_in the vector that points to the third dimension (optional)
!>
!> If the box is in 2D space, the parameter "u3_in" will be useless.
!>
   PURE SUBROUTINE set_OBBoxType(thisBox,p0,e_in,u1_in,u2_in,u3_in)
      CLASS(OBBoxType),INTENT(INOUT) :: thisBox
      TYPE(PointType),INTENT(IN) :: p0
      REAL(SRK),INTENT(IN) :: e_in(:)
      REAL(SRK),INTENT(IN) :: u1_in(:)
      REAL(SRK),INTENT(IN) :: u2_in(:)
      REAL(SRK),INTENT(IN),OPTIONAL :: u3_in(:)

      REAL(SRK) :: u1(3),u2(3),u3(3)
      REAL(SRK) :: magn1,magn2,magn3,dotmp1,dotmp2,dotmp3,r
      INTEGER(SIK) :: ndim,nnon0

      CALL thisBox%clear()
      ndim=p0%dim

      IF(SIZE(e_in) >= ndim .AND. SIZE(u1_in) >= ndim .AND. SIZE(u2_in) >= ndim) THEN
        u1=0.0_SRK
        u2=0.0_SRK
        IF(ndim == 2) THEN
          IF(e_in(1) > 0.0_SRK .AND. e_in(2) > 0.0_SRK) THEN
            u1(1)=u1_in(1)
            u1(2)=u1_in(2)
            u2(1)=u2_in(1)
            u2(2)=u2_in(2)

            !Renormalize the vectors
            magn1=u1(1)*u1(1)+u1(2)*u1(2)
            magn2=u2(1)*u2(1)+u2(2)*u2(2)
            IF(.NOT.(magn1 .APPROXEQA. 1._SRK) .AND. magn1 /= 0.0_SRK) THEN
              r=1._SRK/SQRT(magn1)
              u1(1)=u1(1)*r
              u1(2)=u1(2)*r
            ENDIF
            IF(.NOT.(magn2 .APPROXEQA. 1._SRK) .AND. magn2 /= 0.0_SRK) THEN
              r=1._SRK/SQRT(magn2)
              u2(1)=u2(1)*r
              u2(2)=u2(2)*r
            ENDIF
            dotmp1=u1(1)*u2(1)+u1(2)*u2(2)

            !Check that the vector lengths are non-zero and at a right angle
            IF(magn1 > EPSREAL .AND. magn2 > EPSREAL .AND. ABS(dotmp1) < EPSREAL) THEN
              !Assign values to OBB
              thisBox%p0=p0
              thisBox%u(1,1)=u1(1)
              thisBox%u(2,1)=u1(2)
              thisBox%u(1,2)=u2(1)
              thisBox%u(2,2)=u2(2)
              thisBox%e(1)=e_in(1)
              thisBox%e(2)=e_in(2)
            ENDIF
          ENDIF
        ELSEIF(ndim == 3) THEN
          !OBB is 3-D

          !Insure that at least 2 of the extents are non-zero
          nnon0=0
          IF(e_in(1) > 0.0_SRK) nnon0=nnon0+1
          IF(e_in(2) > 0.0_SRK) nnon0=nnon0+1
          IF(e_in(3) > 0.0_SRK) nnon0=nnon0+1
          IF(nnon0 >= 2) THEN
            u1(1)=u1_in(1)
            u1(2)=u1_in(2)
            u1(3)=u1_in(3)
            u2(1)=u2_in(1)
            u2(2)=u2_in(2)
            u2(3)=u2_in(3)

            !Renormalize the vectors
            magn1=u1(1)*u1(1)+u1(2)*u1(2)+u1(3)*u1(3)
            magn2=u2(1)*u2(1)+u2(2)*u2(2)+u2(3)*u2(3)
            IF(.NOT.(magn1 .APPROXEQA. 1._SRK) .AND. magn1 /= 0.0_SRK) THEN
              r=1._SRK/SQRT(magn1)
              u1(1)=u1(1)*r
              u1(2)=u1(2)*r
              u1(3)=u1(3)*r
            ENDIF
            IF(.NOT.(magn2 .APPROXEQA. 1._SRK) .AND. magn2 /= 0.0_SRK) THEN
              r=1._SRK/SQRT(magn2)
              u2(1)=u2(1)*r
              u2(2)=u2(2)*r
              u2(3)=u2(3)*r
            ENDIF
            dotmp1=u1(1)*u2(1)+u1(2)*u2(2)+u1(3)*u2(3)

            magn3=1._SRK
            dotmp2=0._SRK
            dotmp3=0._SRK
            IF(PRESENT(u3_in)) THEN
              !Process u3_in
              u3(1)=u3_in(1)
              u3(2)=u3_in(2)
              u3(3)=u3_in(3)
              magn3=u3(1)*u3(1)+u3(2)*u3(2)+u3(3)*u3(3)
              IF(.NOT.(magn3 .APPROXEQA. 1._SRK) .AND. magn3 /= 0.0_SRK) THEN
                r=1._SRK/SQRT(magn3)
                u3(1)=u3(1)*r
                u3(2)=u3(2)*r
                u3(3)=u3(3)*r
              ENDIF
              dotmp2=u1(1)*u3(1)+u1(2)*u3(2)+u1(3)*u3(3)
              dotmp3=u3(1)*u2(1)+u3(2)*u2(2)+u3(3)*u2(3)
            ELSE
              !Compute u3 as the cross product of u1 and u2
              u3(1)=u1(2)*u2(3)-u1(3)*u2(2)
              u3(2)=u1(3)*u2(1)-u1(1)*u2(3)
              u3(3)=u1(1)*u2(2)-u1(2)*u2(1)
              magn3=u3(1)*u3(1)+u3(2)*u3(2)+u3(3)*u3(3)
              IF(.NOT.(magn3 .APPROXEQA. 1._SRK) .AND. magn3 /= 0.0_SRK) THEN
                r=1._SRK/SQRT(magn3)
                u3(1)=u3(1)*r
                u3(2)=u3(2)*r
                u3(3)=u3(3)*r
              ENDIF
            ENDIF

            !Check that the vector lengths are non-zero and at a right angle
            IF(magn1 /= 0._SRK .AND. magn2 /= 0._SRK &
              .AND. magn3 /= 0._SRK .AND. ABS(dotmp1) < EPSREAL &
                .AND. ABS(dotmp2) < EPSREAL .AND. ABS(dotmp3) < EPSREAL) THEN
              thisBox%p0=p0
              thisBox%u(1,1)=u1(1)
              thisBox%u(2,1)=u1(2)
              thisBox%u(3,1)=u1(3)
              thisBox%u(1,2)=u2(1)
              thisBox%u(2,2)=u2(2)
              thisBox%u(3,2)=u2(3)
              thisBox%u(1,3)=u3(1)
              thisBox%u(2,3)=u3(2)
              thisBox%u(3,3)=u3(3)
              thisBox%e(1)=e_in(1)
              thisBox%e(2)=e_in(2)
              thisBox%e(3)=e_in(3)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE set_OBBoxType
!
!-------------------------------------------------------------------------------
!> @brief Determine the point(s) of intersection between an OBB and line
!> segment (if it exists)
!> @param box the box to test to intersection
!> @param line the line segment to test to intersection
!> @param p1 the first intersection which is near to the segment starting point
!> @param p2 the second intersection which is far from the segment starting point
!> @returns @c p1 the line segment of intersection
!> @note a return code is assigned to p1%dim/p2%dim indicating the
!>   type of shortest route segment.
!>   -3: there is no intersection (disjoint) @n
!>   -1: problem with dummy arguments passed to routine @n
!>  > 1: success; an shortest distance was found @n
!>
!> If the two points overlap, it will successfully return two points. If only
!> one point is got, another point status is indicated by %dim.
!>
!> The algorithm for finding the intersection is similar to that outlined
!> in section 5.3.3 of "Real-Time Collision Detection" by Christer Ericson.
!> The problem for the intersection is stated as a ray and an axis-aligned
!> bounding box (AABB) and has been adapted for a segment and an OBB.
!>
    ELEMENTAL SUBROUTINE intersect_OBBoxType_and_LineType(thisBox,line,p1,p2)
      CLASS(OBBoxType),INTENT(IN) :: thisBox
      TYPE(LineType),INTENT(IN) :: line
      TYPE(PointType),INTENT(INOUT) :: p1,p2

      REAL(SRK) :: coord(3),dir(3),dir_world(3),coord_new(3)
      REAL(SRK) :: tmin,tmax,invdir,t1,t2,t
      INTEGER(SIK) :: i,ndim

      p1%dim=-1
      p2%dim=-1

      !Check the box and line
      ndim=thisBox%p0%dim
      IF(ndim == line%p1%dim .AND. ndim == line%p2%dim .AND. ndim > 1) THEN
        p1%dim=-3
        p2%dim=-3

        IF(ndim == 2) THEN
!2-D
          !Translate the coordinate into the OBB system.
          dir_world(1)=line%p2%coord(1)-line%p1%coord(1)
          dir_world(2)=line%p2%coord(2)-line%p1%coord(2)

          coord(1)=line%p1%coord(1)-thisBox%p0%coord(1)
          coord(2)=line%p1%coord(2)-thisBox%p0%coord(2)

          dir(1)=dir_world(1)*thisBox%u(1,1)+dir_world(2)*thisBox%u(2,1)
          dir(2)=dir_world(1)*thisBox%u(1,2)+dir_world(2)*thisBox%u(2,2)

          coord_new(1)=coord(1)*thisBox%u(1,1)+coord(2)*thisBox%u(2,1)
          coord_new(2)=coord(1)*thisBox%u(1,2)+coord(2)*thisBox%u(2,2)
        ELSEIF(ndim == 3) THEN
!3-D
          !Translate the coordinate into the OBB system.
          dir_world(1)=line%p2%coord(1)-line%p1%coord(1)
          dir_world(2)=line%p2%coord(2)-line%p1%coord(2)
          dir_world(3)=line%p2%coord(3)-line%p1%coord(3)

          coord(1)=line%p1%coord(1)-thisBox%p0%coord(1)
          coord(2)=line%p1%coord(2)-thisBox%p0%coord(2)
          coord(3)=line%p1%coord(3)-thisBox%p0%coord(3)

          dir(1)=dir_world(1)*thisBox%u(1,1)+dir_world(2)*thisBox%u(2,1)+ &
            dir_world(3)*thisBox%u(3,1)
          dir(2)=dir_world(1)*thisBox%u(1,2)+dir_world(2)*thisBox%u(2,2)+ &
            dir_world(3)*thisBox%u(3,2)
          dir(3)=dir_world(1)*thisBox%u(1,3)+dir_world(2)*thisBox%u(2,3)+ &
            dir_world(3)*thisBox%u(3,3)

          coord_new(1)=coord(1)*thisBox%u(1,1)+coord(2)*thisBox%u(2,1)+ &
            coord(3)*thisBox%u(3,1)
          coord_new(2)=coord(1)*thisBox%u(1,2)+coord(2)*thisBox%u(2,2)+ &
            coord(3)*thisBox%u(3,2)
          coord_new(3)=coord(1)*thisBox%u(1,3)+coord(2)*thisBox%u(2,3)+ &
            coord(3)*thisBox%u(3,3)
        ENDIF

        tmin=-1._SRK
        tmax=1.e30_SRK
        DO i=1,ndim
          IF(ABS(dir(i)) < EPSREAL) THEN
            !If segment is parallel to box, and if the coordinate for that
            !dimension is outside box then there is no intersection
            IF(coord_new(i) < 0._SRK-EPSREAL .OR. &
              coord_new(i) > thisBox%e(i)+EPSREAL) RETURN
          ELSE
            invdir=1._SRK/dir(i)
            t1=(-coord_new(i))*invdir
            t2=(thisBox%e(i)-coord_new(i))*invdir
            !Make t1 be the closer of the two intersections
            IF(t1 > t2) THEN
              t=t2
              t2=t1
              t1=t
            ENDIF
            IF(t1 > tmin) tmin=t1
            IF(t2 < tmax) tmax=t2

            !Exit if intersection with box becomes empty
            IF(tmin > tmax) RETURN
          ENDIF
        ENDDO

        !Segment intersects in all dimensions so compute the points
        !The first point lies on the segment
        p1=thisBox%p0
        p1%coord(1)=line%p1%coord(1)+dir_world(1)*tmin
        p1%coord(2)=line%p1%coord(2)+dir_world(2)*tmin
        IF(ndim == 3) p1%coord(3)=line%p1%coord(3)+dir_world(3)*tmin

        !The second point lies on the segment
        p2=thisBox%p0
        p2%coord(1)=line%p1%coord(1)+dir_world(1)*tmax
        p2%coord(2)=line%p1%coord(2)+dir_world(2)*tmax
        IF(ndim == 3) p2%coord(3)=line%p1%coord(3)+dir_world(3)*tmax
      ENDIF
    ENDSUBROUTINE intersect_OBBoxType_and_LineType
!
!-------------------------------------------------------------------------------
!> @brief Returns the lines the make up a OBBoxType object
!> @param box the OBBoxType object to decompose
!> @param lines the array of line types to be returned
!>
!> Only works for 2-D currently.
!>
   PURE SUBROUTINE getLines_OBBoxType(thisBox,lines)
     CLASS(OBBoxType),INTENT(INOUT) :: thisBox
     TYPE(LineType),INTENT(OUT),ALLOCATABLE :: lines(:)

     IF(thisBox%p0%dim == 2) THEN
       ALLOCATE(lines(4))
       lines(1)%p1=thisBox%p0
       lines(4)%p2=thisBox%p0
       CALL lines(1)%p2%init(DIM=2,X=thisBox%p0%coord(1)+thisBox%u(1,1)*thisBox%e(1), &
         Y=thisBox%p0%coord(2)+thisBox%u(2,1)*thisBox%e(1))
       lines(2)%p1=lines(1)%p2
       CALL lines(2)%p2%init(DIM=2,X=lines(2)%p1%coord(1)+thisBox%u(1,2)*thisBox%e(2), &
         Y=lines(2)%p1%coord(2)+thisBox%u(2,2)*thisBox%e(2))
       lines(3)%p1=lines(2)%p2
       CALL lines(3)%p2%init(DIM=2,X=thisBox%p0%coord(1)+thisBox%u(1,2)*thisBox%e(2), &
         Y=thisBox%p0%coord(2)+thisBox%u(2,2)*thisBox%e(2))
       lines(4)%p1=lines(3)%p2
     !ELSE
     ENDIF
   ENDSUBROUTINE getLines_OBBoxType
!
!-------------------------------------------------------------------------------
!> @brief Returns the lines the make up a OBBoxType object
!> @param box the OBBoxType object to decompose
!> @param lines the array of line types to be returned
!>
!> Only works for 3-D currently.
!>
   PURE SUBROUTINE getPlanes_OBBoxType(thisBox,planes)
     CLASS(OBBoxType),INTENT(INOUT) :: thisBox
     TYPE(PlaneType),INTENT(OUT),ALLOCATABLE :: planes(:)
     TYPE(PointType) :: tmpPt

     IF(thisBox%p0%dim == 3) THEN
       ALLOCATE(planes(6))
       planes(1)%v0=thisBox%p0
       planes(1)%n=thisBox%u(:,1)
       planes(2)%v0=thisBox%p0
       planes(1)%n=thisBox%u(:,2)
       planes(3)%v0=thisBox%p0
       planes(1)%n=thisBox%u(:,3)
       CALL tmpPt%init(X=thisBox%p0%coord(1)+thisBox%u(1,1)*thisBox%e(1)+ &
         thisBox%u(1,2)*thisBox%e(2)+thisBox%u(1,3)*thisBox%e(3), &
         Y=thisBox%p0%coord(2)+thisBox%u(2,1)*thisBox%e(1)+ &
         thisBox%u(2,2)*thisBox%e(2)+thisBox%u(2,3)*thisBox%e(3), &
         Z=thisBox%p0%coord(3)+thisBox%u(3,1)*thisBox%e(1)+ &
         thisBox%u(3,2)*thisBox%e(2)+thisBox%u(3,3)*thisBox%e(3))
       planes(4)%v0=tmpPt
       planes(4)%n=thisBox%u(:,1) !Does this need to be made negative?
       planes(5)%v0=tmpPt
       planes(4)%n=thisBox%u(:,2) !Does this need to be made negative?
       planes(6)%v0=tmpPt
       planes(4)%n=thisBox%u(:,3) !Does this need to be made negative?
     !ELSE
     ENDIF
   ENDSUBROUTINE getPlanes_OBBoxType
!
!-------------------------------------------------------------------------------
!> @brief Returns the lines the make up a OBBoxType object
!> @param box the OBBoxType object to decompose
!> @param lines the array of line types to be returned
!>
!> Only works for 2-D currently.
!>
   ELEMENTAL FUNCTION inside_OBBoxType(thisBox,point) RESULT(bool)
     CLASS(OBBoxType),INTENT(IN) :: thisBox
     TYPE(PointType),INTENT(IN) :: point
     LOGICAL(SBK) :: bool
     TYPE(PointType) :: px,py
     REAL(SRK) :: pxmag,pymag,pxproj,pyproj

     bool=.FALSE.
     IF(point%dim == 2) THEN
       CALL px%init(DIM=2,X=thisBox%u(1,1)*thisBox%e(1), &
         Y=thisBox%u(2,1)*thisBox%e(1))
       CALL py%init(DIM=2,X=thisBox%u(1,2)*thisBox%e(2), &
         Y=thisBox%u(2,2)*thisBox%e(2))
       pxmag=(px%coord(1)-thisBox%p0%coord(1))*(px%coord(1)-thisBox%p0%coord(1))+&
         (px%coord(2)-thisBox%p0%coord(2))*(px%coord(2)-thisBox%p0%coord(2))
       pymag=(py%coord(1)-thisBox%p0%coord(1))*(py%coord(1)-thisBox%p0%coord(1))+&
         (py%coord(2)-thisBox%p0%coord(2))*(py%coord(2)-thisBox%p0%coord(2))
       pxproj=(px%coord(1)-thisBox%p0%coord(1))*(point%coord(1)-thisBox%p0%coord(1))+ &
         (px%coord(2)-thisBox%p0%coord(2))*(point%coord(2)-thisBox%p0%coord(2))
       IF((0.0_SRK .APPROXLE. pxproj) .AND. (pxproj .APPROXLE. pxmag)) THEN
         pyproj=(py%coord(1)-thisBox%p0%coord(1))*(point%coord(1)-thisBox%p0%coord(1))+ &
         (py%coord(2)-thisBox%p0%coord(2))*(point%coord(2)-thisBox%p0%coord(2))
         bool=(0.0_SRK .APPROXLE. pyproj) .AND. (pyproj .APPROXLE. pymag)
       ENDIF
     ELSEIF(point%dim == 3) THEN
     ENDIF
   ENDFUNCTION inside_OBBoxType
!
!-------------------------------------------------------------------------------
!> @brief Defines the 'is equal to' operation between two OBBoxes e.g. @c b0==b1
!> @param p0 the first box
!> @param p1 the second box
!> @returns @c bool the boolean result of the operation
!>
!> Function is elemental so it can be used on an array of boxes.
    ELEMENTAL FUNCTION isequal_OBBoxType(b0,b1) RESULT(bool)
      TYPE(OBBoxType),INTENT(IN) :: b0,b1
      LOGICAL(SBK) :: bool
      bool=.FALSE.
      IF(b0%p0 == b1%p0 .AND. ALL(b0%u .APPROXEQA. b1%u)) &
        bool=ALL(b0%e.APPROXEQA. b1%e)
    ENDFUNCTION isequal_OBBoxType
!
!-------------------------------------------------------------------------------
!> @brief Constructor for the ABBoxType
!> @param thisABB the ABBoxType object to be initialized
!> @param xMin the minimum x-extent of the box
!> @param xMax the maximum x-extent of the box
!> @param yMin the minimum y-extent of the box
!> @param yMax the maximum y-extent of the box
!>
!> This routine sets the attributes of a ABBoxType object.
    ELEMENTAL SUBROUTINE set_ABBoxType(thisABB,xMin,xMax,yMin,yMax,zMin,zMax)
      CLASS(ABBoxType),INTENT(INOUT) :: thisABB
      REAL(SRK),INTENT(IN) :: xMin,xMax,yMin,yMax
      REAL(SRK),INTENT(IN),OPTIONAL :: zMin,zMax

      thisABB%xMin=xMin
      thisABB%xMax=xMax
      thisABB%yMin=yMin
      thisABB%yMax=yMax
      IF(PRESENT(zMin) .AND. PRESENT(zMax)) THEN
        thisABB%zMin=zMin
        thisABB%zMax=zMax
        thisABB%is3D=.TRUE.
      ELSE
        thisABB%is3D=.FALSE.
      ENDIF
      thisABB%isSet=.TRUE.
      thisABB%isOrigin=((xMin <= 0.0_SRK) .AND. (xMax >= 0.0_SRK) .AND. &
        (yMin <= 0.0_SRK) .AND. (yMax >= 0.0_SRK))

    ENDSUBROUTINE set_ABBoxType
!
!-------------------------------------------------------------------------------
!> @brief Clears the attributes of a ABBoxType object
!> @param thisABB the ABBoxType object to be cleared
!>
    ELEMENTAL SUBROUTINE clear_ABBoxType(thisABB)
      CLASS(ABBoxType),INTENT(INOUT) :: thisABB
      thisABB%xMin=0.0_SRK
      thisABB%xMax=0.0_SRK
      thisABB%yMin=0.0_SRK
      thisABB%yMax=0.0_SRK
      thisABB%zMin=0.0_SRK
      thisABB%zMax=0.0_SRK
      thisABB%is3D=.FALSE.
      thisABB%isSet=.FALSE.
      thisABB%isOrigin=.FALSE.
    ENDSUBROUTINE clear_ABBoxType
!
!-------------------------------------------------------------------------------
!> @brief Tests if a point is inside a box
!> @param thisABB the box
!> @param p the point
!> @returns inside .TRUE. if p is inside the box.
!>
    ELEMENTAL FUNCTION inside_ABBoxType(thisABB,p) RESULT(inside)
      CLASS(ABBoxType),INTENT(IN) :: thisABB
      TYPE(PointType),INTENT(IN) :: p
      REAL(SRK),PARAMETER :: fuzz=1e-6_SRK
      LOGICAL(SBK) :: inside

      inside=.FALSE.
      IF(p%dim == 2 .OR. (p%dim == 3 .AND. .NOT.thisABB%is3D)) THEN
        inside=((p%coord(1) > thisABB%xMin-fuzz) .AND. &
          (p%coord(1) < thisABB%xMax+fuzz) .AND. &
          (p%coord(2) > thisABB%yMin-fuzz) .AND. &
          (p%coord(2) < thisABB%yMax+fuzz))
      ELSEIF(p%dim == 3 .AND. thisABB%is3D) THEN
        inside=((p%coord(1) > thisABB%xMax-fuzz) .AND. &
          (p%coord(1) < thisABB%xMax+fuzz) .AND. &
          (p%coord(2) > thisABB%yMin-fuzz) .AND. &
          (p%coord(2) < thisABB%yMax+fuzz) .AND. &
          (p%coord(3) > thisABB%zMin-fuzz) .AND. &
          (p%coord(3) < thisABB%zMax+fuzz))
      ENDIF

    ENDFUNCTION inside_ABBoxType
!
ENDMODULE Geom_Box
