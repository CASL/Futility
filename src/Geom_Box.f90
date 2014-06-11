!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                              Copyright (C) 2012                              !
!                   The Regents of the University of Michigan                  !
!              MPACT Development Group and Prof. Thomas J. Downar              !
!                             All rights reserved.                             !
!                                                                              !
! Copyright is reserved to the University of Michigan for purposes of          !
! controlled dissemination, commercialization through formal licensing, or     !
! other disposition. The University of Michigan nor any of their employees,    !
! makes any warranty, express or implied, or assumes any liability or          !
! responsibility for the accuracy, completeness, or usefulness of any          !
! information, apparatus, product, or process disclosed, or represents that    !
! its use would not infringe privately owned rights. Reference herein to any   !
! specific commercial products, process, or service by trade name, trademark,  !
! manufacturer, or otherwise, does not necessarily constitute or imply its     !
! endorsement, recommendation, or favoring by the University of Michigan.      !
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
!> clearing the OBB, and intersecting it with a line.
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
  IMPLICIT NONE

  PRIVATE
!
!List of public items
  PUBLIC OBBType

  !> @brief Type for a Box
  TYPE OBBType
    !> The start corner point
    TYPE(PointType) :: p0
    !> The vector that points in the direction of the first dimension
    REAL(SRK) :: u(3,3)=0.0_SRK
    !> The extent of every dimension
    REAL(SRK) :: e(3)=0.0_SRK
    !
    !List of type bound procedures
    CONTAINS
      !> @copybrief Geom_Box::set_OBBType
      !> @copydetail Geom_Box::set_OBBType
      PROCEDURE,PASS :: set => set_OBBType
      !> @copybrief Geom_Box::clear_OBBType
      !> @copydetail Geom_Box::clear_OBBType
      PROCEDURE,PASS :: clear => clear_OBBType
      !> @copybrief Geom_Box::intersect_OBBType_and_LineType
      !> @copydetail Geom_Box::intersect_OBBType_and_LineType
      PROCEDURE,PASS :: intersectLine => intersect_OBBType_and_LineType
  ENDTYPE OBBType
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Clears an OBBType object's atrributes
!> @param thisOBB the OBBType object to clear
!>
    ELEMENTAL SUBROUTINE clear_OBBType(thisOBB)
      CLASS(OBBType),INTENT(INOUT) :: thisOBB
      CALL thisOBB%p0%clear()
      thisOBB%u=0.0_SRK
      thisOBB%e=0.0_SRK
    ENDSUBROUTINE clear_OBBType
!
!-------------------------------------------------------------------------------
!> @brief Sets a BoxType object's atrributes
!> @param box the BoxType object to set
!> @param p0 the start corner point of the box
!> @param e_in the extent of every dimension
!> @param u1_in the vector that points to the first dimention
!> @param u2_in the vector that points to the second dimention
!> @param u3_in the vector that points to the third dimention (optional)
!>
!> If the box is in 2D space, the parameter "u3_in" will be useless.
!>
   PURE SUBROUTINE set_OBBType(thisOBB,p0,e_in,u1_in,u2_in,u3_in)
      CLASS(OBBType),INTENT(INOUT) :: thisOBB
      TYPE(PointType),INTENT(IN) :: p0
      REAL(SRK),INTENT(IN) :: e_in(:)
      REAL(SRK),INTENT(IN) :: u1_in(:)
      REAL(SRK),INTENT(IN) :: u2_in(:)
      REAL(SRK),INTENT(IN),OPTIONAL :: u3_in(:)

      REAL(SRK) :: u1(3),u2(3),u3(3)
      REAL(SRK) :: magn1,magn2,magn3,dotmp1,dotmp2,dotmp3,r
      INTEGER(SIK) :: ndim,nnon0

      CALL thisOBB%clear()
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
              thisOBB%p0=p0
              thisOBB%u(1,1)=u1(1)
              thisOBB%u(2,1)=u1(2)
              thisOBB%u(1,2)=u2(1)
              thisOBB%u(2,2)=u2(2)
              thisOBB%e(1)=e_in(1)
              thisOBB%e(2)=e_in(2)
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
              thisOBB%p0=p0
              thisOBB%u(1,1)=u1(1)
              thisOBB%u(2,1)=u1(2)
              thisOBB%u(3,1)=u1(3)
              thisOBB%u(1,2)=u2(1)
              thisOBB%u(2,2)=u2(2)
              thisOBB%u(3,2)=u2(3)
              thisOBB%u(1,3)=u3(1)
              thisOBB%u(2,3)=u3(2)
              thisOBB%u(3,3)=u3(3)
              thisOBB%e(1)=e_in(1)
              thisOBB%e(2)=e_in(2)
              thisOBB%e(3)=e_in(3)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE set_OBBType
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
    ELEMENTAL SUBROUTINE intersect_OBBType_and_LineType(thisOBB,line,p1,p2)
      CLASS(OBBType),INTENT(IN) :: thisOBB
      TYPE(LineType),INTENT(IN) :: line
      TYPE(PointType),INTENT(INOUT) :: p1,p2

      REAL(SRK) :: coord(3),dir(3),dir_world(3),coord_new(3)
      REAL(SRK) :: tmin,tmax,invdir,t1,t2,t
      INTEGER(SIK) :: i,ndim

      p1%dim=-1
      p2%dim=-1

      !Check the box and line
      ndim=thisOBB%p0%dim
      IF(ndim == line%p(1)%dim .AND. ndim == line%p(2)%dim .AND. ndim > 1) THEN
        p1%dim=-3
        p2%dim=-3

        IF(ndim == 2) THEN
!2-D
          !Translate the coordinate into the OBB system.
          dir_world(1)=line%p(2)%coord(1)-line%p(1)%coord(1)
          dir_world(2)=line%p(2)%coord(2)-line%p(1)%coord(2)

          coord(1)=line%p(1)%coord(1)-thisOBB%p0%coord(1)
          coord(2)=line%p(1)%coord(2)-thisOBB%p0%coord(2)

          dir(1)=dir_world(1)*thisOBB%u(1,1)+dir_world(2)*thisOBB%u(2,1)
          dir(2)=dir_world(1)*thisOBB%u(1,2)+dir_world(2)*thisOBB%u(2,2)

          coord_new(1)=coord(1)*thisOBB%u(1,1)+coord(2)*thisOBB%u(2,1)
          coord_new(2)=coord(1)*thisOBB%u(1,2)+coord(2)*thisOBB%u(2,2)
        ELSEIF(ndim == 3) THEN
!3-D
          !Translate the coordinate into the OBB system.
          dir_world(1)=line%p(2)%coord(1)-line%p(1)%coord(1)
          dir_world(2)=line%p(2)%coord(2)-line%p(1)%coord(2)
          dir_world(3)=line%p(2)%coord(3)-line%p(1)%coord(3)

          coord(1)=line%p(1)%coord(1)-thisOBB%p0%coord(1)
          coord(2)=line%p(1)%coord(2)-thisOBB%p0%coord(2)
          coord(3)=line%p(1)%coord(3)-thisOBB%p0%coord(3)

          dir(1)=dir_world(1)*thisOBB%u(1,1)+dir_world(2)*thisOBB%u(2,1)+ &
            dir_world(3)*thisOBB%u(3,1)
          dir(2)=dir_world(1)*thisOBB%u(1,2)+dir_world(2)*thisOBB%u(2,2)+ &
            dir_world(3)*thisOBB%u(3,2)
          dir(3)=dir_world(1)*thisOBB%u(1,3)+dir_world(2)*thisOBB%u(2,3)+ &
            dir_world(3)*thisOBB%u(3,3)

          coord_new(1)=coord(1)*thisOBB%u(1,1)+coord(2)*thisOBB%u(2,1)+ &
            coord(3)*thisOBB%u(3,1)
          coord_new(2)=coord(1)*thisOBB%u(1,2)+coord(2)*thisOBB%u(2,2)+ &
            coord(3)*thisOBB%u(3,2)
          coord_new(3)=coord(1)*thisOBB%u(1,3)+coord(2)*thisOBB%u(2,3)+ &
            coord(3)*thisOBB%u(3,3)
        ENDIF

        tmin=-1._SRK
        tmax=1.e30_SRK
        DO i=1,ndim
          IF(ABS(dir(i)) < EPSREAL) THEN
            !If segment is parallel to box, and if the coordinate for that
            !dimension is outside box then there is no intersection
            IF(coord_new(i) < 0._SRK-EPSREAL .OR. &
              coord_new(i) > thisOBB%e(i)+EPSREAL) RETURN
          ELSE
            invdir=1._SRK/dir(i)
            t1=(-coord_new(i))*invdir
            t2=(thisOBB%e(i)-coord_new(i))*invdir
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
        p1=thisOBB%p0
        p1%coord(1)=line%p(1)%coord(1)+dir_world(1)*tmin
        p1%coord(2)=line%p(1)%coord(2)+dir_world(2)*tmin
        IF(ndim == 3) p1%coord(3)=line%p(1)%coord(3)+dir_world(3)*tmin

        !The second point lies on the segment
        p2=thisOBB%p0
        p2%coord(1)=line%p(1)%coord(1)+dir_world(1)*tmax
        p2%coord(2)=line%p(1)%coord(2)+dir_world(2)*tmax
        IF(ndim == 3) p2%coord(3)=line%p(1)%coord(3)+dir_world(3)*tmax
      ENDIF
    ENDSUBROUTINE intersect_OBBType_and_LineType
!
ENDMODULE Geom_Box
