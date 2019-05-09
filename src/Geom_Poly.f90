!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module defining a polygon type and a "polygonize"
!> method.
!>
!> This module provides a derived data types for a "circle" which must exist in
!> 2-D space and a "cylinder" which must exist in 3-D space. The circle is
!> defined by a point for the center of rotation and a radius. The cylinder is
!> defined by two points for the central axis of rotation and its total extent
!> (if it is to be finite) and a radius. The cylinder is assumed to be a right
!> cylinder. The module also provides methods for constructing and destructing
!> these types and routines for intersecting lines with their bodies.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref Geom_Points "Geom_Points": @copybrief Geom_Points
!>  - @ref Geom_Line "Geom_Line": @copybrief Geom_Line
!>  - @ref Geom_Line "Geom_Plane": @copybrief Geom_Plane
!>  - @ref Geom_Box "Geom_Box": @copybrief Geom_Box
!>  - @ref Geom_Line "Geom_CircCyl": @copybrief Geom_CircCyl
!>  - @ref Geom_Line "Geom_Graph": @copybrief Geom_Graph
!>
!> @author Brendan Kochunas and Dan Jabaay
!>    @date 06/06/2015
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Poly
  USE IntrType
  USE Allocs
  USE Constants_Conversion
  USE Geom_Graph
  USE Geom_Points
  USE Geom_Line
  USE Geom_Plane
  USE Geom_Box
  USE Geom_CircCyl
  USE Geom_Box

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: PolygonType
  PUBLIC :: Polygonize
  PUBLIC :: OPERATOR(==)

  !> @brief Type used to describe a polygon.
  !>
  TYPE :: PolygonType
    !> Bool for if the object is initialized
    LOGICAL :: isInit=.FALSE.
    !> Area of the polygon
    REAL(SRK) :: area=0.0_SRK
    !> The number of elements in the list of vertices
    INTEGER(SIK) :: nVert=0
    !> The number of elements in the list of edges
    INTEGER(SIK) :: nQuadEdge=0
    !> Centroid of the polygon
    TYPE(PointType) :: centroid
    !> The list of vertices SIZE(nvert)
    TYPE(PointType),ALLOCATABLE :: vert(:)
    !> The list of edges by the vert index SIZE (2,nvert)
    INTEGER(SIK),ALLOCATABLE :: edge(:,:)
    !> Map to move from the number of quad edges to the edge index SIZE(nQuadEdge)
    INTEGER(SIK),ALLOCATABLE :: quad2edge(:)
    !> The list of quadratic edges (circles) SIZE (3,nQuadEdge)
    REAL(SRK),ALLOCATABLE :: quadEdge(:,:)
    !>
    TYPE(PolygonType),POINTER :: nextPoly => NULL()
    !>
    TYPE(PolygonType),POINTER :: subRegions => NULL()
!
!List of type bound procedure for the object
    CONTAINS
      !> @copybrief Geom_Poly::set_PolygonType
      !> @copydetails Geom_Poly::set_PolygonType
      PROCEDURE,PASS :: set => set_PolygonType
      !> @copybrief Geom_Poly::clear_PolygonType
      !> @copydetails Geom_Poly::clear_PolygonType
      PROCEDURE,PASS :: clear => clear_PolygonType
      !> @copybrief Geom_Poly::point_inside_PolygonType
      !> @copydetails Geom_Poly::point_inside_PolygonType
      PROCEDURE,PASS :: pointInside => point_inside_PolygonType
      !> @copybrief Geom_Poly::polygon_inside_PolygonType
      !> @copydetails Geom_Poly::polygon_inside_PolygonType
      PROCEDURE,PASS :: boundsPoly => polygon_inside_PolygonType
      !> @copybrief Geom_Poly::onSurface_PolygonType
      !> @copydetails Geom_Poly::onSurface_PolygonType
      PROCEDURE,PASS :: onSurface => onSurface_PolygonType
      !> @copybrief Geom_Poly::doesLineIntersect_PolygonType
      !> @copydetails Geom_Poly::doesLineIntersect_PolygonType
      PROCEDURE,PASS :: doesLineIntersect => doesLineIntersect_PolygonType
      !> @copybrief Geom_Poly::doesPolyIntersect_PolygonType
      !> @copydetails Geom_Poly::doesPolyIntersect_PolygonType
      PROCEDURE,PASS :: doesPolyIntersect => doesPolyIntersect_PolygonType
      !> @copybrief Geom_Poly::isSimple_PolygonType
      !> @copydetails Geom_Poly::isSimple_PolygonType
      PROCEDURE,PASS :: isSimple => isSimple_PolygonType
      !> @copybrief Geom_Poly::isCircle_PolygonType
      !> @copydetails Geom_Poly::isCircle_PolygonType
      PROCEDURE,PASS :: isCircle => isCircle_PolygonType
      !> @copybrief Geom_Poly::isSector_PolygonType
      !> @copydetails Geom_Poly::isSector_PolygonType
      PROCEDURE,PASS :: isSector => isSector_PolygonType
      !> @copybrief Geom_Poly::intersectLine_PolygonType
      !> @copydetails Geom_Poly::intersectLine_PolygonType
      PROCEDURE,PASS :: intersectLine => intersectLine_PolygonType
      !> @copybrief Geom_Poly::intersectPoly_PolygonType
      !> @copydetails Geom_Poly::intersectPoly_PolygonType
      PROCEDURE,PASS :: intersectPoly => intersectPoly_PolygonType
      !> @copybrief Geom_Poly::getRadius_PolygonType
      !> @copydetails Geom_Poly::getRadius_PolygonType
      PROCEDURE,PASS :: getRadius => getRadius_PolygonType
      !> @copybrief Geom_Poly::getInnerRadius_PolygonType
      !> @copydetails Geom_Poly::getInnerRadius_PolygonType
      PROCEDURE,PASS :: getInnerRadius => getInnerRadius_PolygonType
      !> @copybrief Geom_Poly::getOuterRadius_PolygonType
      !> @copydetails Geom_Poly::getOuterRadius_PolygonType
      PROCEDURE,PASS :: getOuterRadius => getOuterRadius_PolygonType
      !> @copybrief Geom_Poly::subtractSubVolume_PolygonType
      !> @copydetails Geom_Poly::subtractSubVolume_PolygonType
      PROCEDURE,PASS :: subtractSubVolume => subtractSubVolume_PolygonType
      !> @copybrief Geom_Poly::generateGraph_PolygonType
      !> @copydetails Geom_Poly::generateGraph_PolygonType
      PROCEDURE,PASS :: generateGraph => generateGraph_PolygonType
  ENDTYPE PolygonType

  INTERFACE Polygonize
    MODULE PROCEDURE Polygonize_Circle
    MODULE PROCEDURE Polygonize_Cylinder
    MODULE PROCEDURE Polygonize_OBBox
    MODULE PROCEDURE Polygonize_ABBox
  ENDINTERFACE

  !No need to make this public... yet.
  !!> @brief
  !INTERFACE inside_PolygonType
  !  !> @copybrief Geom_Poly::polygon_inside_PolygonType
  !  !> @copydetails Geom_Poly::polygon_inside_PolygonType
  !  MODULE PROCEDURE polygon_inside_PolygonType
  !  !> @copybrief Geom_Poly::point_inside_PolygonType
  !  !> @copydetails Geom_Poly::point_inside_PolygonType
  !  MODULE PROCEDURE point_inside_PolygonType
  !ENDINTERFACE

  !> @brief
  INTERFACE OPERATOR(==)
    !> @copybrief Geom_Poly::isequal_PolygonType
    !> @copydetails Geom_Poly::isequal_PolygonType
    MODULE PROCEDURE isequal_PolygonType
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief This routine initialies the polygon type from a point graph type.
!> @param thisPoly The polygon type to be initialized.
!> @param thatGraph The graphtype from which to initialize the polygon.
!>
!> Note:  References for the calculation of the centroid include:
!> http://mathworld.wolfram.com/CircularSegment.html for the circular sector
!> portion, https://en.wikipedia.org/wiki/Centroid for Centroid of a polygon
!> specified by vertices, and
!> http://www.slideshare.net/coolzero2012/centroids-moments-of-inertia for
!> how to set up the super-position of these elements altogether.
!>
    SUBROUTINE set_PolygonType(thisPoly,thatGraph)
      CLASS(PolygonType),INTENT(INOUT) :: thisPoly
      TYPE(GraphType),INTENT(IN) :: thatGraph
      REAL(SRK),PARAMETER :: FOURTHIRD=4.0_SRK/3.0_SRK
      INTEGER(SIK) :: i,icw,iccw,icurr,inextold,inext,iedge,iquad
      REAL(SRK) :: R1,coeff,subarea,xcent,ycent,theta,halftheta,sinhalftheta,tsint,rcent
      TYPE(PointType) :: point
      TYPE(LineType) :: line
      TYPE(CircleType) :: circle

      !Check if thatGraph is closed (i.e. each vertex has only two neighbors)
      CALL clear_PolygonType(thisPoly)
      IF(ALLOCATED(thatGraph%vertices) .AND. thatGraph%isMinimumCycle()) THEN
        !Get the number of vertices and edges (equal, since it's a minimum cycle)
        thisPoly%nVert=thatGraph%nVert()
        ALLOCATE(thisPoly%vert(thisPoly%nVert))
        CALL dmallocA(thisPoly%edge,2,thisPoly%nVert)
        !Setup initial points and edge point
        !Need to use the edgeMatrix to find the neighboring points, can't assume ordered.
        CALL thisPoly%vert(1)%init(DIM=2,X=thatGraph%vertices(1,1), &
          Y=thatGraph%vertices(2,1))
        thisPoly%edge(1,1)=1
        !Loop over all vertices in the graphtype in CW ordering.
        !Logic here is for when we have our next point directly above the starting point.
        iccw=thatGraph%getCCWMostVert(0,1)
        icw=thatGraph%getCWMostVert(0,1)
        inext=iccw
        inextold=1
        icurr=1
        xcent=0.0_SRK; ycent=0.0_SRK
        DO i=2,thisPoly%nVert
          CALL thisPoly%vert(i)%init(DIM=2,X=thatGraph%vertices(1,inext), &
            Y=thatGraph%vertices(2,inext))
          !Using the CW-th point, the area calc will be negative.  Hence the ABS()
          subarea=thisPoly%vert(i-1)%coord(1)* &
            thisPoly%vert(i)%coord(2)-thisPoly%vert(i-1)%coord(2)* &
            thisPoly%vert(i)%coord(1)
          thisPoly%area=thisPoly%area+subarea
          !When calculating the weighted means, we subtract because we're going CW instead of CCW
          xcent=xcent-subarea*(thisPoly%vert(i-1)%coord(1)+thisPoly%vert(i)%coord(1))
          ycent=ycent-subarea*(thisPoly%vert(i-1)%coord(2)+thisPoly%vert(i)%coord(2))
          !Set the edge
          thisPoly%edge(2,i-1)=i
          thisPoly%edge(1,i)=i
          !Check if it's a quadratic edge to count
          IF(thatGraph%quadEdges(3,icurr,inext) > 0.0_SRK) thisPoly%nQuadEdge=thisPoly%nQuadEdge+1
          inextold=inext
          !Get the next vertex using the neighbor call
          inext=thatGraph%getCCWMostVert(icurr,inext)
          icurr=inextold
        ENDDO
        !Set last edge
        thisPoly%edge(2,thisPoly%nVert)=1
        subarea=thisPoly%vert(thisPoly%nVert)%coord(1)* &
          thisPoly%vert(1)%coord(2)-thisPoly%vert(thisPoly%nVert)%coord(2)* &
          thisPoly%vert(1)%coord(1)
        thisPoly%area=thisPoly%area+subarea
        !When calculating the weighted means, we subtract because we're going CW instead of CCW
        xcent=xcent-subarea*(thisPoly%vert(thisPoly%nVert)%coord(1)+thisPoly%vert(1)%coord(1))
        ycent=ycent-subarea*(thisPoly%vert(thisPoly%nVert)%coord(2)+thisPoly%vert(1)%coord(2))

        !Check if it's a quadratic edge to count
        IF(thatGraph%quadEdges(3,1,icurr) > 0.0_SRK) thisPoly%nQuadEdge=thisPoly%nQuadEdge+1
        !Last component of the area calc.
        thisPoly%area=ABS(thisPoly%area*0.5_SRK)
        xcent=xcent/6.0_SRK
        ycent=ycent/6.0_SRK
        !Setup the quadratic edges if necessary
        IF(thisPoly%nQuadEdge > 0) THEN
          CALL dmallocA(thisPoly%quadEdge,3,thisPoly%nQuadEdge)
          CALL dmallocA(thisPoly%quad2edge,thisPoly%nQuadEdge)
          thisPoly%quad2edge=0
          thisPoly%quadEdge=0.0_SRK
          !Loop over all vertices in the graphtype in CW ordering.
          !Logic here is for when we have our next point directly above the starting point.
          iccw=thatGraph%getCCWMostVert(0,1)
          icw=thatGraph%getCWMostVert(0,1)
          inext=iccw
          inextold=1
          icurr=1
          iquad=0
          DO i=2,thisPoly%nvert
            IF(thatGraph%quadEdges(3,icurr,inext) > 0.0_SRK) THEN
              iquad=iquad+1
              thisPoly%quadEdge(:,iquad)=thatGraph%quadEdges(:,icurr,inext)
              thisPoly%quad2edge(iquad)=i-1
            ENDIF
            inextold=inext
            inext=thatGraph%getCCWMostVert(icurr,inext)
            icurr=inextold
          ENDDO
          IF(thatGraph%quadEdges(3,1,icurr) > 0.0_SRK) THEN
            iquad=iquad+1
            thisPoly%quadEdge(:,iquad)=thatGraph%quadEdges(:,1,icurr)
            thisPoly%quad2edge(iquad)=thisPoly%nvert
          ENDIF

          !Now do the chord area checks, because they're all positive values.
          DO i=1,thisPoly%nQuadEdge
            IF(thisPoly%quadEdge(3,i) > 0.0_SRK) THEN
              CALL createArcFromQuad(thisPoly,i,circle)
              CALL point%init(DIM=2,X=thisPoly%quadEdge(1,i), &
                Y=thisPoly%quadEdge(2,i))
              !Setup line to test whether to add or subtract
              iedge=thisPoly%quad2edge(i)
              CALL line%set(thisPoly%vert(thisPoly%edge(1,iedge)), &
                thisPoly%vert(thisPoly%edge(2,iedge)))

              R1=thisPoly%quadEdge(3,i)

              !Calculate Chord Sector area (Arc area - triangle area)
              coeff=1.0_SRK
              IF(line%pointIsLeft(point)) coeff=-1.0_SRK
              theta=circle%thetastp-circle%thetastt !Guaranteed to be on (0,PI]
              tsint=theta-SIN(theta)
              halftheta=0.5_SRK*theta
              sinhalftheta=SIN(0.5_SRK*theta)

              !Compute area of circular segment
              subarea=coeff*R1*R1*0.5_SRK*tsint
              !Compute radial position of centroid in circular segment
              rcent=FOURTHIRD*R1*sinhalftheta*sinhalftheta*sinhalftheta/tsint

              !Add to sum of area
              thisPoly%area=thisPoly%area+subarea

              !Add to centroid numerator (area waited centroids)
              !convert radial centroid to x,y coordinates
              xcent=xcent+subarea*(rcent*COS(halftheta+circle%thetastt)+circle%c%coord(1))
              ycent=ycent+subarea*(rcent*SIN(halftheta+circle%thetastt)+circle%c%coord(2))

              !Clear things
              CALL circle%clear()
              CALL line%clear()
              CALL point%clear()
            ENDIF
          ENDDO
        ENDIF
        CALL thisPoly%centroid%init(DIM=2,X=xcent/thisPoly%area,Y=ycent/thisPoly%area)
        thisPoly%isinit=.TRUE.
      ENDIF
    ENDSUBROUTINE set_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief The routine clears all of the attributes and deallocates the arrays
!>        for the polygon type.
!> @param thisPolygon The routine to be cleared.
!>
    SUBROUTINE clear_PolygonType(thisPolygon)
      CLASS(PolygonType),INTENT(INOUT) :: thisPolygon
      INTEGER(SIK) :: i

      !Clear the vertex data
      DO i=thisPolygon%nVert,1,-1
        CALL thisPolygon%vert(i)%clear()
      ENDDO
      IF(ALLOCATED(thisPolygon%vert)) DEALLOCATE(thisPolygon%vert)
      IF(ALLOCATED(thisPolygon%edge)) CALL demallocA(thisPolygon%edge)
      IF(ALLOCATED(thisPolygon%quad2edge)) CALL demallocA(thisPolygon%quad2edge)
      CALL demallocA(thisPolygon%quadEdge)
      thisPolygon%nVert=0
      thisPolygon%nQuadEdge=0
      thisPolygon%area=0.0_SRK
      CALL thisPolygon%centroid%clear()
      thisPolygon%subregions => NULL()
      thisPolygon%nextPoly => NULL()
      thisPolygon%isinit=.FALSE.
    ENDSUBROUTINE clear_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
    ELEMENTAL FUNCTION isSimple_PolygonType(thisPoly) RESULT(bool)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      LOGICAL(SBK) :: bool
      bool=.NOT.ASSOCIATED(thisPoly%subRegions)
    ENDFUNCTION isSimple_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief Determines whether a polygon meets the criteria for being a circle.
!>        In short, the number of vertices (and edges implicitly) must equal
!>        the number of quadratic edges.  The quadratic edges must all be equal
!>        to one another as well.
!> @param thisPoly The polygon type to query
!> @param bool The logical result of the operation
!>
    ELEMENTAL FUNCTION isCircle_PolygonType(thisPoly) RESULT(bool)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i
      bool=.FALSE.
      IF(thisPoly%isinit .AND. (thisPoly%nVert == thisPoly%nQuadEdge)) THEN
        bool=.TRUE.
        DO i=2,thisPoly%nQuadEdge
          bool=bool .AND. ALL(thisPoly%quadEdge(:,i) .APPROXEQA. thisPoly%quadEdge(:,1))
        ENDDO
      ENDIF
    ENDFUNCTION isCircle_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief This routine will query a polygon type, check if it meets the criteria
!>        to be a circle, and if so it will return the radius for the circle.
!> @param thisPoly The polygon type from which to get the radius
!> @param r The radius if thisPoly is a circle.  It will be 0.0 for all other
!>        cases.
!>
    ELEMENTAL FUNCTION getRadius_PolygonType(thisPoly) RESULT(r)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      REAL(SRK) :: r
      r=0.0_SRK
      IF(thisPoly%isCircle()) r=thisPoly%quadEdge(3,1)
    ENDFUNCTION getRadius_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief Determines whether a polygon meets the criteria for being an arc
!>        sector. In short, the polygon must have at least 1 quadratic edge and
!>        exactly two non-quadratic edges. A circle is an arc sector.
!> @param thisPoly The polygon type to query
!> @param bool The logical result of the operation
!>
    ELEMENTAL FUNCTION isSector_PolygonType(thisPoly) RESULT(bool)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: nNonQuadEdges
      bool=.FALSE.
      IF(thisPoly%isinit) THEN
        IF(thisPoly%isCircle()) THEN
          bool=.TRUE.
        ELSE
          !nVert should be equal to the total number of edges
          nNonQuadEdges=thisPoly%nVert-thisPoly%nQuadEdge
          bool=(thisPoly%nQuadEdge >= 1) .AND. (nNonQuadEdges == 2)
        ENDIF
      ENDIF
    ENDFUNCTION isSector_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief This routine will query a polygon type, check if it meets the criteria
!>        to be an arc sector, and if so it will return the inner radius.
!> @param thisPoly The polygon type from which to get the inner radius
!> @param r The inner radius if thisPoly is an arc sector.  It will be 0.0 for
!>        all other cases.
!>
    ELEMENTAL FUNCTION getInnerRadius_PolygonType(thisPoly) RESULT(r)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      REAL(SRK) :: r
      INTEGER(SIK) :: i
      r=0.0_SRK
      IF(thisPoly%isCircle()) THEN
        r=0.0_SRK
      ELSEIF(thisPoly%isSector()) THEN
        r=HUGE(1.0_SRK)
        DO i=1,thisPoly%nQuadEdge
          IF(thisPoly%quadEdge(3,i) < r) r=thisPoly%quadEdge(3,i)
        ENDDO
      ENDIF
      IF(r .APPROXEQA. thisPoly%getOuterRadius()) r=0.0_SRK
    ENDFUNCTION getInnerRadius_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief This routine will query a polygon type, check if it meets the criteria
!>        to be an arc sector, and if so it will return the outer radius.
!> @param thisPoly The polygon type from which to get the outer radius
!> @param r The outer radius if thisPoly is an arc sector.  It will be 0.0 for
!>        all other cases.
!>
    ELEMENTAL FUNCTION getOuterRadius_PolygonType(thisPoly) RESULT(r)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      REAL(SRK) :: r
      INTEGER(SIK) :: i
      r=0.0_SRK
      IF(thisPoly%isCircle()) THEN
        r=thisPoly%getRadius()
      ELSEIF(thisPoly%isSector()) THEN
        DO i=1,thisPoly%nQuadEdge
          IF(thisPoly%quadEdge(3,i) > r) r=thisPoly%quadEdge(3,i)
        ENDDO
      ENDIF
    ENDFUNCTION getOuterRadius_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief This routine determines if a given point lies on the surface of a
!>        given polygon.
!> @param thisPoly The polygon type used in the query
!> @param point The point type to check if it lies on the surface of the
!>        polygontype
!> @param bool The logical result of this operation.  TRUE if the point is on
!>        the surface.
!>
    PURE RECURSIVE FUNCTION onSurface_PolygonType(thisPoly,point,incSubReg) RESULT(bool)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      TYPE(PointType),INTENT(IN) :: point
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: incSubReg
      LOGICAL(SBK) :: bool
      LOGICAL(SBK) :: isQuadEdge,includeSubRegions
      INTEGER(SIK) :: i,iedge
      REAL(SRK) :: d
      TYPE(PointType) :: centroid
      TYPE(LineType) :: line

      IF(thisPoly%isinit .AND. point%dim == 2) THEN
        bool=.FALSE.
        !Check if its one of the vertices
        DO i=1,thisPoly%nVert
          IF(ALL(point%coord .APPROXEQA. thisPoly%vert(i)%coord)) THEN
            bool=.TRUE.
            EXIT
          ENDIF
        ENDDO

        IF(.NOT.bool) THEN
          !Check straight edges.
          DO i=1,thisPoly%nVert
            isQuadEdge=.FALSE.
            IF(thisPoly%nQuadEdge > 0) THEN
              IF(ANY(thisPoly%quad2edge == i)) isQuadEdge=.TRUE.
            ENDIF
            IF(.NOT.isQuadEdge) THEN
              CALL line%set(thisPoly%vert(thisPoly%edge(1,i)), &
                thisPoly%vert(thisPoly%edge(2,i)))
              d=line%distance2Point(point)
              IF(d .APPROXEQA. 0.0_SRK) THEN
                bool=.TRUE.
                EXIT
              ENDIF
            ENDIF
          ENDDO
          CALL line%clear()
        ENDIF

        IF(.NOT.bool .AND. thisPoly%nQuadEdge > 0) THEN
          !Check quadratic edges
          DO i=1,thisPoly%nQuadEdge
            !First check if the point is on the surface of the circle
            IF(((point%coord(1)-thisPoly%quadEdge(1,i))* &
              (point%coord(1)-thisPoly%quadEdge(1,i))+ &
              (point%coord(2)-thisPoly%quadEdge(2,i))* &
              (point%coord(2)-thisPoly%quadEdge(2,i))) .APPROXEQA. &
                (thisPoly%quadEdge(3,i)*thisPoly%quadEdge(3,i))) THEN
              !Check if the point is within the arc range
              CALL centroid%init(DIM=2,X=thisPoly%quadEdge(1,i), &
                Y=thisPoly%quadEdge(2,i))
              iedge=thisPoly%quad2edge(i)
              CALL line%set(thisPoly%vert(thisPoly%edge(1,iedge)), &
                thisPoly%vert(thisPoly%edge(2,iedge)))
              IF(line%pointIsLeft(centroid)) THEN
                !Centroid is on the left, the valid portion of the
                !arc lies to the right of the line.
                bool=line%pointIsRight(point)
              ELSE
                !Centroid is on the right, the valid portion of the
                !arc lies to the left of the line.
                bool=line%pointIsLeft(point)
              ENDIF
              CALL centroid%clear()
              CALL line%clear()
              IF(bool) EXIT
            ENDIF
          ENDDO
        ENDIF
        IF(.NOT.bool) THEN
          includeSubRegions=.TRUE.
          IF(PRESENT(incSubReg)) includeSubRegions=incSubReg
          IF(ASSOCIATED(thisPoly%subregions) .AND. includeSubRegions) THEN
            bool=onSurface_PolygonType(thisPoly%subregions,point,.FALSE.)
          ELSEIF(ASSOCIATED(thisPoly%nextPoly) .AND. .NOT.includeSubRegions) THEN
            bool=onSurface_PolygonType(thisPoly%nextPoly,point,.FALSE.)
          ENDIF
        ENDIF
      ENDIF
    ENDFUNCTION onSurface_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief This routine determines whether a point lies within an arbitrary
!> polygon.  The processes by which this can be done are the ray tracing "count
!> number" approach, or the winding number approach.  After researching the
!> problem, it was determined that the winding number provides the best solution
!> in the case that the polygon is non-convex.  A paper was found by Alciatore
!> and Miranda describing a simple axis-crossing method.  This routine applies
!> the axis-crossing winding number from their paper:
!> http://www.engr.colostate.edu/~dga/dga/papers/point_in_polygon.pdf
!>
!> @param thisPoly The polygon type used in the query
!> @param point The point type to check if it lies inside the polygontype
!> @param isSub Optional logical of whether or not it is a recursive subregion check.
!> @param bool The logical result of this operation.  TRUE if the point is inside.
!>
    PURE RECURSIVE FUNCTION point_inside_PolygonType(thisPoly,point,isSub) RESULT(bool)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      TYPE(PointType),INTENT(IN) :: point
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: isSub
      LOGICAL(SBK) :: bool
      LOGICAL(SBK) :: inConvexCirc,inConcaveCirc,isSubReg,inPoly,lpix,lpnextx,onLine
      INTEGER(SIK) :: i,iedge
      REAL(SRK) :: r,w,wm,pi(2),pnext(2)
      TYPE(PointType) :: centroid
      TYPE(LineType) :: line
      TYPE(CircleType) :: circ

      isSubReg=.FALSE.
      IF(PRESENT(isSub)) isSubReg=isSub

      bool=.FALSE.
      IF(thisPoly%nVert > 0 .AND. point%dim == 2) THEN
        w=0.0_SRK
        DO i=1,thisPoly%nVert
          pi=thisPoly%vert(thisPoly%edge(1,i))%coord-point%coord
          pnext=thisPoly%vert(thisPoly%edge(2,i))%coord-point%coord
          lpix=(pi(2) .APPROXEQA. 0.0_SRK) .AND. .NOT.(pi(1) .APPROXLE. 0.0_SRK)
          lpnextx=(pnext(2) .APPROXEQA. 0.0_SRK) .AND. .NOT.(pnext(1) .APPROXLE. 0.0_SRK)
          IF(.NOT.(pi(2)*pnext(2) .APPROXGE. 0.0_SRK)) THEN
            r=pi(1)+pi(2)*(pnext(1)-pi(1))/(pi(2)-pnext(2))
            IF(.NOT.(r .APPROXLE. 0.0_SRK)) THEN !r > 0
              wm=REAL(ABS(TRANSFER(.NOT.(pi(2) .APPROXGE. 0.0_SRK),1)),SRK)
              w=w+2.0_SRK*wm-1.0_SRK
            ENDIF
          ELSEIF(lpix .AND. .NOT.lpnextx) THEN
            wm=REAL(ABS(TRANSFER(.NOT.(pnext(2) .APPROXLE. 0.0_SRK),1)),SRK)
            w=w+wm-0.5_SRK
          ELSEIF(lpnextx .AND. .NOT.lpix) THEN
            wm=REAL(ABS(TRANSFER(.NOT.(pi(2) .APPROXGE. 0.0_SRK),1)),SRK)
            w=w+wm-0.5_SRK
          ENDIF
        ENDDO
        inPoly=.NOT.(w .APPROXEQA. 0.0_SRK)

        !We need this statement until the winding algorithm can be fixed
        !for special edge cases.
        IF(.NOT.inPoly) inPoly=onSurface_PolygonType(thisPoly,point)

        !Now check the quadratic edges if there are any
        IF(thisPoly%nQuadEdge > 0) THEN
          inConvexCirc=.FALSE.
          inConcaveCirc=.FALSE.
          DO i=1,thisPoly%nQuadEdge
            iedge=thisPoly%quad2edge(i)
            CALL centroid%init(DIM=2,X=thisPoly%quadEdge(1,i),Y=thisPoly%quadEdge(2,i))
            CALL line%set(thisPoly%vert(thisPoly%edge(1,iedge)), &
              thisPoly%vert(thisPoly%edge(2,iedge)))
            CALL circ%set(centroid,thisPoly%quadEdge(3,i))
            !If it's approximately on the line consider it in the arc
            onLine=(line%distance2Point(point) .APPROXEQA. 0.0_SRK)
            !Check if the centroid is on the interior or exterior of the edge
            IF(line%pointIsRight(centroid) .AND. .NOT. inConvexCirc) THEN
              !Quad edge extends outside the polygon.
              !Check if the point is to the left of the edge and inside the circle
              inConvexCirc=(line%pointIsLeft(point) .OR. onLine) .AND. circ%inside(point)
            ELSEIF(line%pointIsLeft(centroid) .AND. .NOT. inConcaveCirc) THEN
              !Quad edge extends inside the polygon.
              !Check if the point is to the right of the edge and outside the circle
              inConcaveCirc=(line%pointIsRight(point) .OR. onLine) .AND. &
                (circ%inside(point) .AND. .NOT.circ%onSurface(point))
            ENDIF
            CALL circ%clear()
            CALL line%clear()
            CALL centroid%clear()
          ENDDO
          !Logic for the cases where there is inside and outside circles.
          inPoly=(inPoly .OR. inConvexCirc) .AND. .NOT.inConcaveCirc
        ENDIF

        IF(.NOT.isSubReg) THEN
          !Logic for inside the polygon and outside the subregions
          IF(inPoly .AND. .NOT.thisPoly%isSimple()) THEN
            inPoly=.NOT.point_inside_PolygonType(thisPoly%subregions,point,.TRUE.) &
              .OR. onSurface_PolygonType(thisPoly%subregions,point,.FALSE.)
          ENDIF
        ELSEIF(ASSOCIATED(thisPoly%nextPoly)) THEN
          inPoly=inPoly .OR. point_inside_PolygonType(thisPoly%nextPoly,point,.TRUE.)
        ENDIF
        bool=inPoly
      ENDIF
    ENDFUNCTION point_inside_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief This routine determines if one polygon is within another. The first
!>        polygon's surface boundaries are inclusive. So if the exact same
!>        polygon where given for both arguments, the result of the operation
!>        would be TRUE.
!>
!> @param thisPoly The polygon type used in the query
!> @param thatPoly The polygon type to check if it lies inside the polygontype
!> @param bool The logical result of this operation.  TRUE if the point is inside.
!>
    FUNCTION polygon_inside_PolygonType(thisPoly,thatPoly) RESULT(bool)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      TYPE(PolygonType),INTENT(IN) :: thatPoly
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,j,iedge
      TYPE(PointType) :: p1,p2
      TYPE(LineType) :: tmpLine
      TYPE(LineType),ALLOCATABLE :: Lines(:)
      TYPE(CircleType) :: tmpCirc
      TYPE(CircleType),ALLOCATABLE :: Circs(:)
      TYPE(PolygonType),POINTER :: iPoly,lastSubPoly

      bool=.FALSE.

      IF(thisPoly%isinit .AND. thatPoly%isinit) THEN
        bool=.TRUE.
        !1. make sure all vertices are inside bounding polygon
        DO i=1,thatPoly%nVert
          !Make sure all of those points are inside thisPoly!
          IF(.NOT.thisPoly%pointInside(thatPoly%vert(i))) THEN
            bool=.FALSE.
            EXIT
          ENDIF
        ENDDO
        !2. Check intersections between combinations of edges of two polygons (ignore intersections that are vertices)
        IF(bool) THEN
          ALLOCATE(Lines(thisPoly%nVert-thisPoly%nQuadEdge))
          IF(thisPoly%nQuadEdge > 0) ALLOCATE(Circs(thisPoly%nQuadEdge))
          j=1
          !Set up the lines for thisPoly
          DO i=1,thisPoly%nVert
            IF(thisPoly%nQuadEdge > 0) THEN
              IF(ALL(thisPoly%quad2edge /= i)) THEN
                CALL Lines(j)%set(thisPoly%vert(thisPoly%edge(1,i)), &
                  thisPoly%vert(thisPoly%edge(2,i)))
                j=j+1
              ENDIF
            ELSE
              CALL Lines(j)%set(thisPoly%vert(thisPoly%edge(1,i)), &
                thisPoly%vert(thisPoly%edge(2,i)))
              j=j+1
            ENDIF
          ENDDO
          !Set up circles/arcs
          DO i=1,thisPoly%nQuadEdge
            CALL createArcFromQuad(thisPoly,i,Circs(i))
          ENDDO
          !Test for line-line intersections
          Line: DO i=1,thisPoly%nVert-thisPoly%nQuadEdge
            !Loop over straight edges
            DO j=1,thatPoly%nVert
              CALL p1%clear()
              CALL p2%clear()
              IF(thatPoly%nQuadEdge > 0) THEN
                IF(ALL(thatPoly%quad2edge /= j)) THEN
                  CALL tmpLine%set(thatPoly%vert(thatPoly%edge(1,j)), &
                    thatPoly%vert(thatPoly%edge(2,j)))
                  p1=Lines(i)%intersectLine(tmpLine)
                ENDIF
              ELSE
                CALL tmpLine%set(thatPoly%vert(thatPoly%edge(1,j)), &
                  thatPoly%vert(thatPoly%edge(2,j)))
                p1=Lines(i)%intersectLine(tmpLine)
              ENDIF
              !Check if there was an intersection
              IF(p1%dim > 0) THEN
                !Clear the point if it happens to be one of the segment endpoints
                IF((p1 .APPROXEQA. Lines(i)%p1) .OR. &
                   (p1 .APPROXEQA. Lines(i)%p2) .OR. &
                   (p1 .APPROXEQA. tmpLine%p1) .OR. &
                   (p1 .APPROXEQA. tmpLine%p2)) &
                  CALL p1%clear()
              ENDIF
              IF(p1%dim > 0) THEN
                !Found an intersection! Polygon is outside!
                bool=.FALSE.
                EXIT Line
              ENDIF
              CALL tmpLine%clear()
            ENDDO
            !Check for line-circle intersections
            DO j=1,thatPoly%nQuadEdge
              CALL createArcFromQuad(thatPoly,j,tmpCirc)
              CALL tmpCirc%intersectLine(Lines(i),p1,p2)

              IF(p1%dim == -3) p1%dim=2 !Include tangent points

              !If line segment end points are on circle, intersections
              !are not returned, so we handle that special case here.
              IF(p1%dim == 0 .AND. tmpCirc%onSurface(Lines(i)%p1)) &
                p1=Lines(i)%p1
              IF(p2%dim == 0 .AND. tmpCirc%onSurface(Lines(i)%p2)) &
                p2=Lines(i)%p2

              iedge=thatPoly%quad2edge(j)
              !Exclude points not in the arc, or the endpoints of either the line or arc
              IF(.NOT.tmpCirc%onSurface(p1) .OR. &
                (Lines(i)%p1 .APPROXEQA. p1) .OR. &
                (Lines(i)%p2 .APPROXEQA. p1).OR. &
                (thatPoly%vert(thatPoly%edge(1,iedge)) .APPROXEQA. p1) .OR. &
                (thatPoly%vert(thatPoly%edge(2,iedge)) .APPROXEQA. p1)) CALL p1%clear()
              IF(.NOT.tmpCirc%onSurface(p2) .OR. &
                (Lines(i)%p1 .APPROXEQA. p2) .OR. &
                (Lines(i)%p2 .APPROXEQA. p2) .OR. &
                (thatPoly%vert(thatPoly%edge(1,iedge)) .APPROXEQA. p2) .OR. &
                (thatPoly%vert(thatPoly%edge(2,iedge)) .APPROXEQA. p2)) CALL p2%clear()

              IF((p1%dim > 0) .OR. (p2%dim > 0)) THEN
                bool=.FALSE.
                EXIT Line
              ENDIF
            ENDDO
          ENDDO Line
          IF(bool) THEN
            !Test for circle-line intersections
            Quad: DO i=1,thisPoly%nQuadEdge
              !Loop over straight edges
              DO j=1,thatPoly%nVert
                CALL p1%clear()
                CALL p2%clear()
                IF(thatPoly%nQuadEdge > 0) THEN
                  IF(ALL(thatPoly%quad2edge /= i)) THEN
                    CALL tmpLine%set(thatPoly%vert(thatPoly%edge(1,j)), &
                      thatPoly%vert(thatPoly%edge(2,j)))
                    CALL Circs(i)%intersectLine(tmpLine,p1,p2)
                  ELSE
                    CYCLE
                  ENDIF
                ELSE
                  CALL tmpLine%set(thatPoly%vert(thatPoly%edge(1,j)), &
                    thatPoly%vert(thatPoly%edge(2,j)))
                  CALL Circs(i)%intersectLine(tmpLine,p1,p2)
                ENDIF

                IF(p1%dim == -3) p1%dim=2 !Include tangent points

                !If line segment end points are on circle, intersections
                !are not returned, so we handle that special case here.
                IF(p1%dim == 0 .AND. Circs(i)%onSurface(tmpLine%p1)) &
                  p1=tmpLine%p1
                IF(p2%dim == 0 .AND. Circs(i)%onSurface(tmpLine%p2)) &
                  p2=tmpLine%p2

                iedge=thisPoly%quad2edge(i)
                !Exclude points not in the arc, or the endpoints of either the line or arc
                IF(.NOT.Circs(i)%onSurface(p1) .OR. &
                  (tmpLine%p1 .APPROXEQA. p1) .OR. &
                  (tmpLine%p2 .APPROXEQA. p1) .OR. &
                  (thisPoly%vert(thisPoly%edge(1,iedge)) .APPROXEQA. p1) .OR. &
                  (thisPoly%vert(thisPoly%edge(2,iedge)) .APPROXEQA. p1)) CALL p1%clear()
                IF(.NOT.Circs(i)%onSurface(p2) .OR. &
                  (tmpLine%p1 .APPROXEQA. p2) .OR. &
                  (tmpLine%p2 .APPROXEQA. p2) .OR. &
                  (thisPoly%vert(thisPoly%edge(1,iedge)) .APPROXEQA. p2) .OR. &
                  (thisPoly%vert(thisPoly%edge(2,iedge)) .APPROXEQA. p2)) CALL p2%clear()

                IF((p1%dim > 0) .OR. (p2%dim > 0)) THEN
                  bool=.FALSE.
                  EXIT Quad
                ENDIF
                CALL tmpLine%clear()
              ENDDO
              !Loop over quadratic edges
              DO j=1,thatPoly%nQuadEdge
                CALL createArcFromQuad(thatPoly,j,tmpCirc)
                CALL tmpCirc%intersectCircle(Circs(i),p1,p2)

                IF(p1%dim == -3) p1%dim=2 !Include tangent points

                iedge=thatPoly%quad2edge(j)
                !Exclude points not in the arc, or the endpoints
                IF(.NOT.tmpCirc%onSurface(p1) .OR. &
                  (thatPoly%vert(thatPoly%edge(1,iedge)) .APPROXEQA. p1) .OR. &
                  (thatPoly%vert(thatPoly%edge(2,iedge)) .APPROXEQA. p1)) CALL p1%clear()
                IF(.NOT.tmpCirc%onSurface(p2) .OR. &
                  (thatPoly%vert(thatPoly%edge(1,iedge)) .APPROXEQA. p2) .OR. &
                  (thatPoly%vert(thatPoly%edge(2,iedge)) .APPROXEQA. p2)) CALL p2%clear()

                IF((p1%dim > 0) .OR. (p2%dim > 0)) THEN
                  bool=.FALSE.
                  EXIT Quad
                ENDIF
                CALL tmpCirc%clear()
              ENDDO
            ENDDO Quad
          ENDIF
        ENDIF
        !3. make sure smaller polygon centroid is inside larger polygon
        IF(bool) THEN
          IF((.NOT. thisPoly%pointInside(thatPoly%centroid) .AND. &
           thatPoly%pointInside(thatPoly%centroid))) bool=.FALSE.
        ENDIF
        !4. Evaluate against subregions (for each subregion)
        IF(bool) THEN
          IF(ASSOCIATED(thisPoly%subregions)) THEN
            iPoly => NULL()
            lastSubPoly => thisPoly%subRegions
            sub: DO WHILE(ASSOCIATED(lastSubPoly))
              !  A. Check that all vertices of polygon are subregion%onSurface .EQV. subregion%inside
              DO i=1,thatPoly%nVert
                IF(lastSubPoly%onSurface(thatPoly%vert(i)) .NEQV. &
                    lastSubPoly%pointInside(thatPoly%vert(i))) THEN
                  bool=.FALSE.
                  EXIT sub
                ENDIF
              ENDDO
              !  B. Check that there are no intersections between edges of polygons (ignore intersections that are vertices)
              IF(lastSubPoly%doesPolyIntersect(thatPoly)) THEN
                bool=.FALSE.
                EXIT
              ENDIF
              !  C. make sure centroid is outside
              !IF(lastSubPoly%pointInside(thatPoly%centroid)) THEN
              !  bool=.FALSE.
              !  EXIT
              !ENDIF
              iPoly => lastSubPoly
              lastSubPoly => lastSubPoly%nextPoly
            ENDDO sub
          ENDIF
        ENDIF

        !Clear the first set of lines
        IF(ALLOCATED(Lines)) THEN
          DO i=1,SIZE(Lines)
            CALL Lines(i)%clear()
          ENDDO
          DEALLOCATE(Lines)
        ENDIF
        !Clear the first set of circles
        IF(ALLOCATED(Circs)) THEN
          DO i=1,SIZE(Circs)
            CALL Circs(i)%clear()
          ENDDO
          DEALLOCATE(Circs)
        ENDIF
      ENDIF
    ENDFUNCTION polygon_inside_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief Function to determine if a line intersects a polygon. This routine
!>        tests the lines intersecting with lines and lines intersecting with
!>        arc circles.  If any of these intersection routines return a point with
!>        a positive dimension variable, an intersection has occurred and will
!>        return TRUE.  If all of the intersection routines return 0 or an error
!>        code, the polygons are presumed to not intersect.
!>
!>        It should be noted that this routine presumes that a colinear
!>        intersection does not constitute an intersection.  This can be changed
!>        by modifying the logic to test a point returning from the intersection
!>        routines if it has a dimension matching the colinear error code.
!> @param thisPoly The first polygon type
!> @param line The line type to intersect with the polygon
!> @param bool The logical result of the operation
!>
    PURE FUNCTION doesLineIntersect_PolygonType(thisPolygon,line) RESULT(bool)
      CLASS(PolygonType),INTENT(IN) :: thisPolygon
      TYPE(LineType),INTENT(IN) :: line
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,j,ipoint
      REAL(SRK) :: x,y
      TYPE(PointType),ALLOCATABLE :: tmppoints(:)
      TYPE(LineType),ALLOCATABLE :: lines(:)
      TYPE(CircleType),ALLOCATABLE :: circles(:)

      bool=.FALSE.

      !Construct line and circle types locally
      ALLOCATE(lines(thisPolygon%nVert))
      IF(thisPolygon%nQuadEdge > 0) ALLOCATE(circles(thisPolygon%nQuadEdge))

      DO i=1,thisPolygon%nVert
        CALL lines(i)%set(thisPolygon%vert(thisPolygon%edge(1,i)), &
          thisPolygon%vert(thisPolygon%edge(2,i)))
      ENDDO
      DO i=1,thisPolygon%nQuadEdge
        CALL createArcFromQuad(thisPolygon,i,circles(i))
      ENDDO
      !Make tmppoints the max possible size
      ALLOCATE(tmppoints(thisPolygon%nVert+3*thisPolygon%nQuadEdge))

      !Test the line with all the edges,
      !Intersect circles first if necessary
      ipoint=1
      Quad: DO i=1,thisPolygon%nQuadEdge
        CALL circles(i)%intersectArcLine(line,tmppoints(ipoint), &
          tmppoints(ipoint+1),tmppoints(ipoint+2),tmppoints(ipoint+3))
        !Check if the points are on the circle
        DO j=0,3
          IF(tmppoints(ipoint+j)%DIM == 2) THEN
            !If it's not on the circle, clear it.
            x=tmppoints(ipoint+j)%coord(1)-circles(i)%c%coord(1)
            y=tmppoints(ipoint+j)%coord(2)-circles(i)%c%coord(2)
            IF(.NOT.(x*x+y*y .APPROXEQA. circles(i)%r*circles(i)%r)) &
              CALL tmppoints(ipoint+j)%clear()
            IF(tmppoints(ipoint+j)%dim > 0) THEN
              bool=.TRUE.
              EXIT Quad
            ENDIF
          ENDIF
        ENDDO
        ipoint=ipoint+4
      ENDDO Quad

      !Intersect the remaining lines if necessary
      IF(.NOT.bool) THEN
        IF(thisPolygon%nQuadEdge > 0) THEN
          DO i=1,SIZE(lines)
            !Intersect the line
            IF(ALL(i /= thisPolygon%quad2edge)) THEN
              tmppoints(ipoint)=lines(i)%intersectLine(line)
              IF(tmppoints(ipoint)%dim > 0) THEN
                bool=.TRUE.
                EXIT
              ENDIF
              ipoint=ipoint+1
            ENDIF
          ENDDO
        ELSE
          DO i=1,SIZE(lines)
            tmppoints(ipoint)=lines(i)%intersectLine(line)
            IF(tmppoints(ipoint)%dim > 0) THEN
              bool=.TRUE.
              EXIT
            ENDIF
            ipoint=ipoint+1
          ENDDO
        ENDIF
      ENDIF

      !Clear local types
      DO i=SIZE(tmppoints),1,-1
        CALL tmppoints(i)%clear()
      ENDDO
      DEALLOCATE(tmppoints)
      DO i=SIZE(lines),1,-1
        CALL lines(i)%clear()
      ENDDO
      DEALLOCATE(lines)
      IF(ALLOCATED(circles)) THEN
        DO i=SIZE(circles),1,-1
          CALL circles(i)%clear()
        ENDDO
        DEALLOCATE(circles)
      ENDIF
    ENDFUNCTION doesLineIntersect_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief Function to determine if two polygons intersect each other. This
!>        routine tests the lines intersecting with lines, lines intersecting
!>        with arc circles, arc circles intersecting with lines, and arc-circles
!>        intersecting with arc-circles.  If any of these intersection routines
!>        return a point with a positive dimension variable, an intersection
!>        has occurred and will return TRUE.  If all of the intersection routines
!>        return 0 or an error code, the polygons are presumed to not intersect.
!>
!>        It should be noted that this routine presumes that a tangential
!>        intersection does not constitute an intersection.  This can be changed
!>        by modifying the logic to test a point returning from the circle
!>        intersection routines if it has a dimension matching the tangent error
!>        code.
!> @param thisPoly The first polygon type
!> @param thatPoly The second polygon type to intersect with the first
!> @param bool The logical result of the operation
!>
    PURE FUNCTION doesPolyIntersect_PolygonType(thisPoly,thatPoly) RESULT(bool)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      TYPE(PolygonType),INTENT(IN) :: thatPoly
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,j,iedge
      TYPE(PointType) :: p1,p2
      TYPE(LineType) :: tmpLine
      TYPE(LineType),ALLOCATABLE :: Lines(:)
      TYPE(CircleType) :: tmpCirc
      TYPE(CircleType),ALLOCATABLE :: Circs(:)

      bool=.FALSE.

      IF(thisPoly%isinit .AND. thatPoly%isinit) THEN
        !2. Check intersections between combinations of edges of two polygons (ignore intersections that are vertices)
        ALLOCATE(Lines(thisPoly%nVert-thisPoly%nQuadEdge))
        IF(thisPoly%nQuadEdge > 0) ALLOCATE(Circs(thisPoly%nQuadEdge))
        j=1
        !Set up the lines for thisPoly
        DO i=1,thisPoly%nVert
          IF(thisPoly%nQuadEdge > 0) THEN
            IF(ALL(thisPoly%quad2edge /= i)) THEN
              CALL Lines(j)%set(thisPoly%vert(thisPoly%edge(1,i)), &
                thisPoly%vert(thisPoly%edge(2,i)))
              j=j+1
            ENDIF
          ELSE
            CALL Lines(j)%set(thisPoly%vert(thisPoly%edge(1,i)), &
              thisPoly%vert(thisPoly%edge(2,i)))
            j=j+1
          ENDIF
        ENDDO
        !Set up circles/arcs
        DO i=1,thisPoly%nQuadEdge
          CALL createArcFromQuad(thisPoly,i,Circs(i))
        ENDDO
        !Test for line-line intersections
        Line: DO i=1,thisPoly%nVert-thisPoly%nQuadEdge
          !Loop over straight edges
          DO j=1,thatPoly%nVert
            CALL p1%clear()
            CALL p2%clear()
            IF(thatPoly%nQuadEdge > 0) THEN
              IF(ALL(thatPoly%quad2edge /= j)) THEN
                CALL tmpLine%set(thatPoly%vert(thatPoly%edge(1,j)), &
                  thatPoly%vert(thatPoly%edge(2,j)))
                p1=Lines(i)%intersectLine(tmpLine)
              ENDIF
            ELSE
              CALL tmpLine%set(thatPoly%vert(thatPoly%edge(1,j)), &
                thatPoly%vert(thatPoly%edge(2,j)))
              p1=Lines(i)%intersectLine(tmpLine)
            ENDIF
            !Check if there was an intersection
            IF(p1%dim > 0) THEN
              !Clear the point if it happens to be one of the segment endpoints
              IF((p1 .APPROXEQA. Lines(i)%p1) .OR. &
                 (p1 .APPROXEQA. Lines(i)%p2) .OR. &
                 (p1 .APPROXEQA. tmpLine%p1) .OR. &
                 (p1 .APPROXEQA. tmpLine%p2)) &
                CALL p1%clear()
            ENDIF
            IF(p1%dim > 0) THEN
              !Found an intersection! Polygon is outside!
              bool=.TRUE.
              EXIT Line
            ENDIF
            CALL tmpLine%clear()
          ENDDO
          !Check for line-circle intersections
          DO j=1,thatPoly%nQuadEdge
            CALL createArcFromQuad(thatPoly,j,tmpCirc)
            CALL tmpCirc%intersectLine(Lines(i),p1,p2)

            IF(p1%dim == -3) p1%dim=2 !Include tangent points

            !If line segment end points are on circle, intersections
            !are not returned, so we handle that special case here.
            IF(p1%dim == 0 .AND. tmpCirc%onSurface(Lines(i)%p1)) &
              p1=Lines(i)%p1
            IF(p2%dim == 0 .AND. tmpCirc%onSurface(Lines(i)%p2)) &
              p2=Lines(i)%p2

            !Exclude points not in the arc, or the endpoints
            IF(.NOT.tmpCirc%onSurface(p1) .OR. &
              (Lines(i)%p1 .APPROXEQA. p1) .OR. &
              (Lines(i)%p2 .APPROXEQA. p1)) CALL p1%clear()
            IF(.NOT.tmpCirc%onSurface(p2) .OR. &
              (Lines(i)%p1 .APPROXEQA. p2) .OR. &
              (Lines(i)%p2 .APPROXEQA. p2)) CALL p2%clear()

            IF((p1%dim > 0) .OR. (p2%dim > 0)) THEN
              bool=.TRUE.
              EXIT Line
            ENDIF
          ENDDO
        ENDDO Line
        IF(.NOT.bool) THEN
          !Test for circle-line intersections
          Quad: DO i=1,thisPoly%nQuadEdge
            !Loop over straight edges
            DO j=1,thatPoly%nVert
              CALL p1%clear()
              CALL p2%clear()
              IF(thatPoly%nQuadEdge > 0) THEN
                IF(ALL(thatPoly%quad2edge /= j)) THEN
                  CALL tmpLine%set(thatPoly%vert(thatPoly%edge(1,j)), &
                    thatPoly%vert(thatPoly%edge(2,j)))
                  CALL Circs(i)%intersectLine(tmpLine,p1,p2)
                ELSE
                  CYCLE
                ENDIF
              ELSE
                CALL tmpLine%set(thatPoly%vert(thatPoly%edge(1,j)), &
                  thatPoly%vert(thatPoly%edge(2,j)))
                CALL Circs(i)%intersectLine(tmpLine,p1,p2)
              ENDIF


              IF(p1%dim == -3) p1%dim=2 !Include tangent points

              !If line segment end points are on circle, intersections
              !are not returned, so we handle that special case here.
              IF(p1%dim == 0 .AND. Circs(i)%onSurface(tmpLine%p1)) &
                p1=tmpLine%p1
              IF(p2%dim == 0 .AND. Circs(i)%onSurface(tmpLine%p2)) &
                p2=tmpLine%p2

              !Exclude points not in the arc, or the endpoints
              IF(.NOT.Circs(i)%onSurface(p1) .OR. &
                (tmpLine%p1 .APPROXEQA. p1) .OR. &
                (tmpLine%p2 .APPROXEQA. p1)) CALL p1%clear()
              IF(.NOT.Circs(i)%onSurface(p2) .OR. &
                (tmpLine%p1 .APPROXEQA. p2) .OR. &
                (tmpLine%p2 .APPROXEQA. p2)) CALL p2%clear()

              IF((p1%dim > 0) .OR. (p2%dim > 0)) THEN
                bool=.TRUE.
                EXIT Quad
              ENDIF
              CALL tmpLine%clear()
            ENDDO
            !Loop over quadratic edges
            DO j=1,thatPoly%nQuadEdge
              CALL createArcFromQuad(thatPoly,j,tmpCirc)
              CALL tmpCirc%intersectCircle(Circs(i),p1,p2)

              IF(p1%dim == -3) p1%dim=2 !Include tangent points

              iedge=thatPoly%quad2edge(j)
              !Exclude points not in the arc, or the endpoints
              IF(.NOT.tmpCirc%onSurface(p1) .OR. &
                (thatPoly%vert(thatPoly%edge(1,iedge)) .APPROXEQA. p1) .OR. &
                (thatPoly%vert(thatPoly%edge(2,iedge)) .APPROXEQA. p1)) CALL p1%clear()
              IF(.NOT.tmpCirc%onSurface(p2) .OR. &
                (thatPoly%vert(thatPoly%edge(1,iedge)) .APPROXEQA. p2) .OR. &
                (thatPoly%vert(thatPoly%edge(2,iedge)) .APPROXEQA. p2)) CALL p2%clear()

              IF((p1%dim > 0) .OR. (p2%dim > 0)) THEN
                bool=.TRUE.
                EXIT Quad
              ENDIF
              CALL tmpCirc%clear()
            ENDDO
          ENDDO Quad
        ENDIF
        !Not sure if this is needed currently.
        !4. Evaluate against subregions (for each subregion)
        !IF(.NOT.bool) THEN
        !  IF(ASSOCIATED(thisPoly%subregions)) THEN
        !    iPoly => NULL()
        !    lastSubPoly => thisPoly%subRegions
        !    DO WHILE(ASSOCIATED(lastSubPoly))
        !      !  B. Check that there are no intersections between edges of polygons (ignore intersections that are vertices)
        !      IF(doesPolyIntersect_PolygonType(lastSubPoly,thatPoly)) THEN
        !        bool=.TRUE.
        !        EXIT
        !      ENDIF
        !      iPoly => lastSubPoly
        !      lastSubPoly => lastSubPoly%nextPoly
        !    ENDDO sub
        !  ENDIF
        !ENDIF
      ENDIF

      !Clear the first set of lines
      IF(ALLOCATED(Lines)) THEN
        DO i=1,SIZE(Lines)
          CALL Lines(i)%clear()
        ENDDO
        DEALLOCATE(Lines)
      ENDIF
      !Clear the first set of circles
      IF(ALLOCATED(Circs)) THEN
        DO i=1,SIZE(Circs)
          CALL Circs(i)%clear()
        ENDDO
        DEALLOCATE(Circs)
      ENDIF
    ENDFUNCTION doesPolyIntersect_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
    PURE SUBROUTINE intersectLine_PolygonType(thisPolygon,line,points)
      CLASS(PolygonType),INTENT(IN) :: thisPolygon
      TYPE(LineType),INTENT(IN) :: line
      TYPE(PointType),ALLOCATABLE,INTENT(INOUT) :: points(:)
      INTEGER(SIK) :: i,j,ipoint,npoints,nlines,narcs,maxpoints
      TYPE(PointType) :: p1,p2
      TYPE(PointType),ALLOCATABLE :: tmppoints(:)
      TYPE(LineType),ALLOCATABLE :: lines(:)
      TYPE(CircleType),ALLOCATABLE :: circles(:)

      !Clear the output variable if it's allocated
      IF(ALLOCATED(points)) THEN
        DO i=1,SIZE(points)
          CALL points(i)%clear()
        ENDDO
        DEALLOCATE(points)
      ENDIF

      !Determine local array sizes
      nlines=thisPolygon%nVert-thisPolygon%nQuadEdge
      narcs=thisPolygon%nQuadEdge
      maxPoints=nlines+2*narcs

      IF(maxPoints > 0) THEN
        ALLOCATE(tmppoints(maxPoints))

        !Setup lines and circles
        IF(nlines > 0) ALLOCATE(lines(nlines))
        IF(narcs > 0) THEN
          ALLOCATE(circles(narcs))
          j=0
          DO i=1,thisPolygon%nVert
            IF(.NOT.ANY(thisPolygon%quad2edge == i)) THEN
              j=j+1
              CALL lines(j)%set(thisPolygon%vert(thisPolygon%edge(1,i)), &
                thisPolygon%vert(thisPolygon%edge(2,i)))
            ENDIF
          ENDDO
        ELSE
          DO i=1,thisPolygon%nVert
            CALL lines(i)%set(thisPolygon%vert(thisPolygon%edge(1,i)), &
              thisPolygon%vert(thisPolygon%edge(2,i)))
          ENDDO
        ENDIF
        DO i=1,narcs
          CALL createArcFromQuad(thisPolygon,i,circles(i))
        ENDDO

        !Test against lines
        npoints=0
        IF(nlines > 0) THEN
          DO i=1,nlines
            p1=lines(i)%intersectLine(line)
            IF(p1%dim == 2) THEN
              npoints=npoints+1
              tmpPoints(npoints)=p1
            ELSEIF(p1%dim == -2) THEN
              !Line overlaps edge of polygon, record 0 points
              npoints=0
              narcs=0 !Do not test arcs either
              EXIT
            ENDIF
            CALL p1%clear()
          ENDDO
        ENDIF

        !Test against arcs
        IF(narcs > 0) THEN
          DO i=1,narcs
            CALL circles(i)%intersectLine(line,p1,p2)
            IF(p1%dim == -3) p1%dim=2 !Include tangent points

            !If line segment end points are on circle, intersections
            !are not returned, so we handle that special case here.
            IF(p1%dim == 0 .AND. circles(i)%onSurface(line%p1)) &
              p1=line%p1
            IF(p2%dim == 0 .AND. circles(i)%onSurface(line%p2)) &
              p2=line%p2

            !Exclude points not in the arc
            IF(.NOT.circles(i)%onSurface(p1)) CALL p1%clear()
            IF(.NOT.circles(i)%onSurface(p2)) CALL p2%clear()

            IF(p1%dim == 2) THEN
              npoints=npoints+1
              tmpPoints(npoints)=p1
            ENDIF
            IF(p2%dim == 2) THEN
              npoints=npoints+1
              tmpPoints(npoints)=p2
            ENDIF
          ENDDO
        ENDIF

        IF(npoints > 0) THEN
          !Remove duplicate points
          DO i=1,npoints
            DO j=i+1,npoints
              IF(tmppoints(j) .APPROXEQA. tmppoints(i)) CALL tmppoints(j)%clear()
            ENDDO
          ENDDO

          ipoint=0
          DO i=1,npoints
            IF(tmpPoints(i)%dim == 2) ipoint=ipoint+1
          ENDDO

          !Set output variable
          ALLOCATE(points(ipoint))
          ipoint=0
          DO i=1,npoints
            IF(tmpPoints(i)%dim == 2) THEN
              ipoint=ipoint+1
              points(ipoint)=tmpPoints(i)
            ENDIF
          ENDDO
        ENDIF

        !Clear local types
        IF(ALLOCATED(tmpPoints)) THEN
          DO i=1,SIZE(tmpPoints)
            CALL tmpPoints(i)%clear()
          ENDDO
          DEALLOCATE(tmpPoints)
        ENDIF
        IF(ALLOCATED(lines)) THEN
          DO i=SIZE(lines),1,-1
            CALL lines(i)%clear()
          ENDDO
          DEALLOCATE(lines)
        ENDIF
        IF(ALLOCATED(circles)) THEN
          DO i=SIZE(circles),1,-1
            CALL circles(i)%clear()
          ENDDO
          DEALLOCATE(circles)
        ENDIF
      ENDIF
    ENDSUBROUTINE intersectLine_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
    PURE SUBROUTINE intersectPoly_PolygonType(thisPoly,thatPoly,points)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      TYPE(PolygonType),INTENT(IN) :: thatPoly
      TYPE(PointType),ALLOCATABLE,INTENT(INOUT) :: points(:)
      INTEGER(SIK) :: i,j,k,ipoint,npoints
      REAL(SRK) :: x,y
      TYPE(PointType),ALLOCATABLE :: tmppoints(:)
      TYPE(LineType),ALLOCATABLE :: theseLines(:),thoseLines(:)
      TYPE(CircleType),ALLOCATABLE :: theseCircs(:),thoseCircs(:)

      IF(thisPoly%isinit .AND. thatPoly%isinit) THEN
        !Set up all the lines for both polygons
        ALLOCATE(theseLines(thisPoly%nVert))
        ALLOCATE(thoseLines(thatPoly%nVert))
        IF(thisPoly%nQuadEdge > 0) ALLOCATE(theseCircs(thisPoly%nQuadEdge))
        IF(thatPoly%nQuadEdge > 0) ALLOCATE(thoseCircs(thatPoly%nQuadEdge))
        ALLOCATE(tmppoints(thisPoly%nVert*thatPoly%nVert+4*thisPoly%nQuadEdge*thatPoly%nVert+ &
          4*thatPoly%nQuadEdge*thisPoly%nVert+2*thisPoly%nQuadEdge*thatPoly%nQuadEdge))
        DO i=1,thisPoly%nVert
          IF(thisPoly%nQuadEdge > 0) THEN
            IF(ALL(thisPoly%quad2edge /= i)) CALL theseLines(i)%set(thisPoly%vert(thisPoly%edge(1,i)), &
              thisPoly%vert(thisPoly%edge(2,i)))
          ELSE
            CALL theseLines(i)%set(thisPoly%vert(thisPoly%edge(1,i)), &
              thisPoly%vert(thisPoly%edge(2,i)))
          ENDIF
        ENDDO
        DO i=1,thatPoly%nVert
          IF(thatPoly%nQuadEdge > 0) THEN
            IF(ALL(thatPoly%quad2edge /= i)) CALL thoseLines(i)%set(thatPoly%vert(thatPoly%edge(1,i)), &
              thatPoly%vert(thatPoly%edge(2,i)))
          ELSE
            CALL thoseLines(i)%set(thatPoly%vert(thatPoly%edge(1,i)), &
              thatPoly%vert(thatPoly%edge(2,i)))
          ENDIF
        ENDDO
        DO i=1,thisPoly%nQuadEdge
          CALL createArcFromQuad(thisPoly,i,theseCircs(i))
        ENDDO
        DO i=1,thatPoly%nQuadEdge
          CALL createArcFromQuad(thatPoly,i,thoseCircs(i))
        ENDDO

        !Nested do-loops for intersecting all lines with lines
        ipoint=1
        !TheseLines
        DO i=1,thisPoly%nVert
          !ThoseLines
          DO j=1,thatPoly%nVert
            IF((theseLines(i)%p1%dim == 2) .AND. (thoseLines(j)%p1%dim == 2)) THEN
              tmppoints(ipoint)=theseLines(i)%intersect(thoseLines(j))
              ipoint=ipoint+1
            ENDIF
          ENDDO
          !All lines with the those circles
          DO j=1,thatPoly%nQuadEdge
            CALL thoseCircs(j)%intersectArcLine(theseLines(i),tmppoints(ipoint), &
              tmppoints(ipoint+1),tmppoints(ipoint+2),tmppoints(ipoint+3))
            !Check if the points are on the circle
            DO k=0,3
              IF(tmppoints(ipoint+k)%DIM == 2) THEN
                !If it's not on the circle, clear it.
                x=tmppoints(ipoint+k)%coord(1)-thoseCircs(j)%c%coord(1)
                y=tmppoints(ipoint+k)%coord(2)-thoseCircs(j)%c%coord(2)
                IF(.NOT.(x*x+y*y .APPROXEQA. thoseCircs(j)%r*thoseCircs(j)%r)) &
                  CALL tmppoints(ipoint+k)%clear()
              ENDIF
            ENDDO
            ipoint=ipoint+4
          ENDDO
        ENDDO
        !All circles with the other circles  (May need to improve this intersection routine for arcs...
        !TheseCircs
        DO i=1,thisPoly%nQuadEdge
          DO j=1,thatPoly%nVert
            CALL theseCircs(i)%intersectArcLine(thoseLines(j),tmppoints(ipoint), &
              tmppoints(ipoint+1),tmppoints(ipoint+2),tmppoints(ipoint+3))
            !Check if the points are on the circle
            DO k=0,3
              IF(tmppoints(ipoint+k)%DIM == 2) THEN
                !If it's not on the circle, clear it.
                x=tmppoints(ipoint+k)%coord(1)-theseCircs(i)%c%coord(1)
                y=tmppoints(ipoint+k)%coord(2)-theseCircs(i)%c%coord(2)
                IF(.NOT.(x*x+y*y .APPROXEQA. theseCircs(i)%r*theseCircs(i)%r)) &
                  CALL tmppoints(ipoint+k)%clear()
              ENDIF
            ENDDO
            ipoint=ipoint+4
          ENDDO
          !ThoseCircs
          DO j=1,thatPoly%nQuadEdge
            CALL theseCircs(i)%intersectCircle(thoseCircs(j),tmppoints(ipoint), &
              tmppoints(ipoint+1))
            ipoint=ipoint+2
          ENDDO
        ENDDO
        !Clear the first set of lines
        DO i=1,thisPoly%nVert
          CALL theseLines(i)%clear()
        ENDDO
        !Clear the first set of circles
        DO i=1,thisPoly%nQuadEdge
          CALL theseCircs(i)%clear()
        ENDDO
        !Clear the rest of the circles
        DO j=1,thatPoly%nQuadEdge
          CALL thoseCircs(j)%clear()
        ENDDO
        !Clear the rest of the lines.
        DO j=1,thatPoly%nVert
          CALL thoseLines(j)%clear()
        ENDDO
        !How to handle subregion intersections?  Do we just eliminate the points that are %inside the subregion?

        !Get the reduced set of points that were actual intersections.
        !Eliminate duplicate points.
        DO i=1,SIZE(tmppoints)
          DO j=i+1,SIZE(tmppoints)
            IF(tmppoints(i) == tmppoints(j)) CALL tmppoints(j)%clear()
          ENDDO
        ENDDO
        npoints=0
        DO i=1,SIZE(tmppoints)
          IF(tmppoints(i)%dim == 2) npoints=npoints+1
        ENDDO
        IF(npoints > 0) THEN
          ALLOCATE(points(npoints))
          ipoint=1
          DO i=1,SIZE(tmppoints)
            IF(tmppoints(i)%dim == 2) THEN
              points(ipoint)=tmppoints(i)
              ipoint=ipoint+1
            ENDIF
            CALL tmppoints(i)%clear()
          ENDDO
        ELSE
          DO i=1,SIZE(tmppoints)
            CALL tmppoints(i)%clear()
          ENDDO
        ENDIF

        !Clear stuff
        DEALLOCATE(tmppoints)
        DEALLOCATE(theseLines,thoseLines)
        IF(ALLOCATED(theseCircs)) DEALLOCATE(theseCircs)
        IF(ALLOCATED(thoseCircs)) DEALLOCATE(thoseCircs)
      ENDIF
    ENDSUBROUTINE intersectPoly_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
    SUBROUTINE subtractSubVolume_PolygonType(thisPoly,subPoly)
      CLASS(PolygonType),INTENT(INOUT) :: thisPoly
      TYPE(PolygonType),TARGET,INTENT(INOUT) :: subPoly
      LOGICAL(SBK) :: trueInside,lintersect
      INTEGER(SIK) :: i
      REAL(SRK) :: cent(2),area
      TYPE(PolygonType),POINTER :: lastSubPoly,iPoly,prevPoly

      IF(thisPoly%isInit .AND. subPoly%isInit) THEN
        IF(thisPoly%boundsPoly(subPoly)) THEN
          !Make sure the vertices are not on the surface
          trueInside=.TRUE.
          DO i=1,subPoly%nVert
            IF(thisPoly%onSurface(subPoly%vert(i))) THEN
              trueInside=.FALSE.
              EXIT
            ENDIF
          ENDDO
          !If the subpoly is actually inside thisPoly
          IF(trueInside) THEN
            lintersect=.FALSE.
            IF(.NOT.ASSOCIATED(thisPoly%subRegions)) THEN
              thisPoly%subRegions => subPoly
            ELSE
              !Intersection check - if intersects a subregions, do not subtract
              iPoly => NULL()
              lastSubPoly => thisPoly%subRegions
              DO WHILE(ASSOCIATED(lastSubPoly))
                lintersect=lastSubPoly%doesPolyIntersect(subPoly)
                IF(lintersect) EXIT
                iPoly => lastSubPoly
                lastSubPoly => lastSubPoly%nextPoly
              ENDDO
              !Overlap check - if subPoly overlaps subregions, remove overlapped regions
              IF(.NOT.lintersect) THEN
                !iPoly => subPoly
                !iPoly%nextPoly => thisPoly%subRegions
                !thisPoly%subRegions => iPoly
                subPoly%nextPoly => thisPoly%subRegions
                thisPoly%subRegions => subPoly
                prevPoly => thisPoly%subRegions
                iPoly => prevPoly%nextPoly
                DO WHILE(ASSOCIATED(iPoly))
                  IF(subPoly%boundsPoly(iPoly)) THEN
                    area=thisPoly%area+iPoly%area
                    !add the centroid back to this Poly
                    thisPoly%centroid%coord(1)=(thisPoly%centroid%coord(1)* &
                      thisPoly%area+iPoly%area*iPoly%centroid%coord(1))/ &
                      area
                    thisPoly%centroid%coord(2)=(thisPoly%centroid%coord(2)* &
                      thisPoly%area+iPoly%area*iPoly%centroid%coord(2))/ &
                      area
                    !add the area back to thisPoly
                    thisPoly%area=area
                    prevPoly%nextPoly => iPoly%nextPoly
                    iPoly => prevPoly
                  ENDIF
                  prevPoly => iPoly
                  iPoly => prevPoly%nextPoly
                ENDDO
              ENDIF
            ENDIF
            IF(.NOT.lintersect) THEN
              cent(1)=thisPoly%centroid%coord(1)*thisPoly%area- &
                subPoly%centroid%coord(1)*subPoly%area
              cent(2)=thisPoly%centroid%coord(2)*thisPoly%area- &
                subPoly%centroid%coord(2)*subPoly%area
              thisPoly%area=thisPoly%area-subPoly%area
              CALL thisPoly%centroid%clear()
              CALL thisPoly%centroid%init(DIM=2,X=cent(1)/thisPoly%area, &
                Y=cent(2)/thisPoly%area)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE subtractSubVolume_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief This routine will take polygon type and return the graphtype object
!>        to which it corresponds.  That graphtype could then be used to
!>        initialize the same polygon.
!> @param thisPoly The polygon type object to be transcribed into a graph type
!> @param g The corresponding graphtype to thisPoly
!>
    RECURSIVE SUBROUTINE generateGraph_PolygonType(thisPoly,g,incSubReg)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      TYPE(GraphType),INTENT(INOUT) :: g
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: incSubReg

      LOGICAL(SBK) :: includeSubRegions
      LOGICAL(SBK),ALLOCATABLE :: isQuadEdge(:)
      INTEGER(SIK) :: i,j,v0,v1
      REAL(SRK) :: r,c(2)
      REAL(SRK),ALLOCATABLE :: verts(:,:)
      TYPE(GraphType) :: subGraph
      TYPE(PolygonType),POINTER :: iPoly

      CALL g%clear()
      IF(thisPoly%isInit) THEN
        ALLOCATE(verts(2,thisPoly%nVert))
        DO i=1,thisPoly%nVert
          verts(:,i)=thisPoly%vert(i)%coord(1:2)
          CALL g%insertVertex(verts(:,i))
        ENDDO
        ALLOCATE(isQuadEdge(thisPoly%nVert)); isQuadEdge=.FALSE.
        DO i=1,thisPoly%nQuadEdge
          j=thisPoly%quad2edge(i)
          isQuadEdge(j)=.TRUE.
          c=thisPoly%quadEdge(1:2,i)
          r=thisPoly%quadEdge(3,i)
          v0=thisPoly%edge(1,j)
          v1=thisPoly%edge(2,j)
          CALL g%defineQuadraticEdge(verts(:,v0),verts(:,v1),c,r)
        ENDDO
        DO i=1,thisPoly%nVert
          IF(.NOT.isQuadEdge(i)) THEN
            v0=thisPoly%edge(1,i)
            v1=thisPoly%edge(2,i)
            CALL g%defineEdge(verts(:,v0),verts(:,v1))
          ENDIF
        ENDDO

        !Add subregions to graph
        includeSubRegions=.TRUE.
        IF(PRESENT(incSubReg)) includeSubRegions=incSubReg
        IF(includeSubRegions) THEN
          iPoly => thisPoly%subregions
          DO WHILE(ASSOCIATED(iPoly))
            CALL generateGraph_PolygonType(iPoly,subGraph,.FALSE.)
            CALL g%combineGraph(subGraph)
            iPoly => iPoly%nextPoly
          ENDDO
        ENDIF
      ENDIF
    ENDSUBROUTINE generateGraph_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief This routine will return a polygon type for a given circular geometry.
!> @param circle The circle type to be turned into a polygon
!> @param polygon The polygon type that corresponds to the circle type.
!>
    SUBROUTINE Polygonize_Circle(circle,polygon)
      TYPE(CircleType),INTENT(IN) :: circle
      TYPE(PolygonType),INTENT(INOUT) :: polygon
      REAL(SRK) :: v0(2),v1(2),v2(2),v3(2),r,c(2)
      TYPE(GraphType) :: g

      CALL polygon%clear()
      IF(circle%r > 0.0_SRK .AND. circle%c%dim == 2) THEN
        r=circle%r
        c=circle%c%coord(1:2)
        IF((circle%thetastt .APPROXEQA. 0.0_SRK) .AND. &
           (circle%thetastp .APPROXEQA. TWOPI)) THEN

          v0(1)=c(1)-r; v0(2)=c(2)
          v1(1)=c(1); v1(2)=c(2)+r
          v2(1)=c(1)+r; v2(2)=c(2)
          v3(1)=c(1); v3(2)=c(2)-r

          CALL g%insertVertex(v0)
          CALL g%insertVertex(v1)
          CALL g%insertVertex(v2)
          CALL g%insertVertex(v3)
          CALL g%defineQuadraticEdge(v0,v1,c,r)
          CALL g%defineQuadraticEdge(v1,v2,c,r)
          CALL g%defineQuadraticEdge(v2,v3,c,r)
          CALL g%defineQuadraticEdge(v0,v3,c,r)
        ELSE
          v0(1)=c(1); v0(2)=c(2)
          v1(1)=c(1)+r*COS(circle%thetastt)
          v1(2)=c(2)+r*SIN(circle%thetastt)
          v2(1)=c(1)+r*COS(circle%thetastp)
          v2(2)=c(2)+r*SIN(circle%thetastp)

          CALL g%insertVertex(v0)
          CALL g%insertVertex(v1)
          CALL g%insertVertex(v2)
          CALL g%defineEdge(v0,v1)
          CALL g%defineEdge(v0,v2)
          CALL g%defineQuadraticEdge(v1,v2,c,r)
        ENDIF

        CALL polygon%set(g)
        CALL g%clear()
      ENDIF
    ENDSUBROUTINE Polygonize_Circle
!
!-------------------------------------------------------------------------------
!> @brief This routine will return a polygon type for a given cylindrical geometry.
!> This routine converts a 3D cylinder to a 2D polygon for visualization.
!> @param cylinder The cylinder type to be turned into a polygon
!> @param polygon The polygon type that corresponds to the cylinder type.
!>
    SUBROUTINE Polygonize_Cylinder(cylinder,polygon)
      TYPE(CylinderType),INTENT(IN) :: cylinder
      TYPE(PolygonType),INTENT(INOUT) :: polygon
      REAL(SRK) :: v0(2),v1(2),v2(2),v3(2),r,c(3)
      TYPE(GraphType) :: g

      CALL polygon%clear()
      IF(cylinder%r > 0.0_SRK .AND. cylinder%axis%p1%dim == 3) THEN
        r=cylinder%r
        c=cylinder%axis%p1%coord(1:3)
        IF((cylinder%thetastt .APPROXEQA. 0.0_SRK) .AND. &
           (cylinder%thetastp .APPROXEQA. TWOPI)) THEN

          v0(1)=c(1)-r; v0(2)=c(2)
          v1(1)=c(1); v1(2)=c(2)+r
          v2(1)=c(1)+r; v2(2)=c(2)
          v3(1)=c(1); v3(2)=c(2)-r

          CALL g%insertVertex(v0)
          CALL g%insertVertex(v1)
          CALL g%insertVertex(v2)
          CALL g%insertVertex(v3)
          CALL g%defineQuadraticEdge(v0,v1,c(1:2),r)
          CALL g%defineQuadraticEdge(v1,v2,c(1:2),r)
          CALL g%defineQuadraticEdge(v2,v3,c(1:2),r)
          CALL g%defineQuadraticEdge(v0,v3,c(1:2),r)
        ELSE
          v0(1)=c(1); v0(2)=c(2)
          v1(1)=c(1)+r*COS(cylinder%thetastt)
          v1(2)=c(2)+r*SIN(cylinder%thetastt)
          v2(1)=c(1)+r*COS(cylinder%thetastp)
          v2(2)=c(2)+r*SIN(cylinder%thetastp)

          CALL g%insertVertex(v0)
          CALL g%insertVertex(v1)
          CALL g%insertVertex(v2)
          CALL g%defineEdge(v0,v1)
          CALL g%defineEdge(v0,v2)
          CALL g%defineQuadraticEdge(v1,v2,c(1:2),r)
        ENDIF

        CALL polygon%set(g)
        CALL g%clear()
      ENDIF
    ENDSUBROUTINE Polygonize_Cylinder
!
!-------------------------------------------------------------------------------
!> @brief This routine will return a polygon type for a given OBBox  geometry.
!> @param obBox The OBBox type to be turned into a polygon
!> @param polygon The polygon type that corresponds to the OBBox type.
!>
!> Not sure if this is correct.
!>
    SUBROUTINE Polygonize_OBBox(obBox,polygon)
      TYPE(OBBoxType),INTENT(IN) :: obBox
      TYPE(PolygonType),INTENT(INOUT) :: polygon
      REAL(SRK) :: v0(2),v1(2),v2(2),v3(2)
      TYPE(GraphType) :: g

      CALL polygon%clear()
      IF(obBox%p0%dim > 1) THEN
        v0(1)=obBox%p0%coord(1); v0(2)=obBox%p0%coord(2)
        v1(1)=v0(1)+obBox%u(1,1)*obBox%e(1)
        v1(2)=v0(2)+obBox%u(2,1)*obBox%e(1)
        v2(1)=v0(1)+obBox%u(1,2)*obBox%e(2)
        v2(2)=v0(2)+obBox%u(2,2)*obBox%e(2)
        v3(1)=v0(1)+obBox%u(1,1)*obBox%e(1)+obBox%u(1,2)*obBox%e(2)
        v3(2)=v0(2)+obBox%u(2,1)*obBox%e(1)+obBox%u(2,2)*obBox%e(2)

        CALL g%insertVertex(v0)
        CALL g%insertVertex(v1)
        CALL g%insertVertex(v2)
        CALL g%insertVertex(v3)
        CALL g%defineEdge(v0,v1)
        CALL g%defineEdge(v0,v2)
        CALL g%defineEdge(v2,v3)
        CALL g%defineEdge(v1,v3)

        CALL polygon%set(g)
        CALL g%clear()
      ENDIF
    ENDSUBROUTINE Polygonize_OBBox
!
!-------------------------------------------------------------------------------
!> @brief This routine will return a polygon type for a given ABBox  geometry.
!> @param abBox The ABBox type to be turned into a polygon
!> @param polygon The polygon type that corresponds to the ABBox type.
!>
    SUBROUTINE Polygonize_ABBox(abBox,polygon)
      TYPE(ABBoxType),INTENT(IN) :: abBox
      TYPE(PolygonType),INTENT(INOUT) :: polygon
      REAL(SRK) :: v0(2),v1(2),v2(2),v3(2)
      TYPE(GraphType) :: g

      CALL polygon%clear()

      v0(1)=abBox%xMin; v0(2)=abBox%yMin
      v1(1)=abBox%xMin; v1(2)=abBox%yMax
      v2(1)=abBox%xMax; v2(2)=abBox%yMax
      v3(1)=abBox%xMax; v3(2)=abBox%yMin
      CALL g%insertVertex(v0)
      CALL g%insertVertex(v1)
      CALL g%insertVertex(v2)
      CALL g%insertVertex(v3)
      CALL g%defineEdge(v0,v1)
      CALL g%defineEdge(v1,v2)
      CALL g%defineEdge(v2,v3)
      CALL g%defineEdge(v3,v0)

      CALL polygon%set(g)
      CALL g%clear()
    ENDSUBROUTINE Polygonize_ABBox
!
!-------------------------------------------------------------------------------
!> @brief An overloaded operator routine to determine if two polygon types are
!>        equivalent.
!> @param p1 The first polygon type to compare
!> @param p2 The second polygon type to compare
!> @param bool The logical result of the operation
!>
    PURE RECURSIVE FUNCTION isequal_PolygonType(p1,p2) RESULT(bool)
      TYPE(PolygonType),INTENT(IN) :: p1
      TYPE(PolygonType),INTENT(IN) :: p2
      LOGICAL(SBK) :: bool

      bool=.FALSE.
      IF(p1%isinit .AND. p2%isinit) THEN
        IF((p1%area .APPROXEQA. p2%area) .AND. (p1%nVert == p2%nVert) .AND. &
          (p1%nQuadEdge == p2%nQuadEdge) .AND. (p1%centroid .APPROXEQA. p2%centroid) .AND. &
          (SIZE(p1%vert) == SIZE(p2%vert)) .AND. &
          (SIZE(p1%edge,DIM=1) == SIZE(p2%edge,DIM=1)) .AND. &
          (SIZE(p1%edge,DIM=2) == SIZE(p2%edge,DIM=2)) .AND. &
          (ASSOCIATED(p1%nextPoly) .EQV. ASSOCIATED(p2%nextPoly)) .AND. &
            (ASSOCIATED(p1%subRegions) .EQV. ASSOCIATED(p2%subRegions))) THEN
          !
          bool=ALL(p1%vert .APPROXEQA. p2%vert) .AND. ALL(p1%edge == p2%edge)
          bool=bool .AND. (ALLOCATED(p1%quad2edge) .EQV. ALLOCATED(p2%quad2edge))
          IF(bool .AND. ALLOCATED(p1%quad2edge)) THEN
            IF(SIZE(p1%quadEdge,DIM=1) == SIZE(p2%quadEdge,DIM=1) .AND. &
               SIZE(p1%quadEdge,DIM=2) == SIZE(p2%quadEdge,DIM=2)) THEN
              bool=bool .AND. ALL(p1%quad2edge == p2%quad2edge) .AND. &
                ALL(p1%quadEdge .APPROXEQA. p2%quadEdge)
            ELSE
              bool=.FALSE.
            ENDIF
          ENDIF
          IF(bool .AND. ASSOCIATED(p1%nextPoly)) THEN
            bool=isequal_PolygonType(p1%nextPoly,p2%nextPoly)
            IF(bool .AND. ASSOCIATED(p1%subRegions)) &
              bool=isequal_PolygonType(p1%subRegions,p2%subRegions)
          ENDIF
        ENDIF
      ENDIF
    ENDFUNCTION isequal_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief This is a local subroutine to create an arc-circle from a specified
!>        quadratic edge for a given polygon type
!> @param thisPoly The polygon type from which to create the arc-circle
!> @param iquad The desired quadratic edge index to convert to a circle type
!> @param circle The circle type created from the polygon's quadratic edge
!>
    PURE SUBROUTINE createArcFromQuad(thisPoly,iquad,circle)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      INTEGER(SIK),INTENT(IN) :: iquad
      TYPE(CircleType),INTENT(INOUT) :: circle
      INTEGER(SIK) :: iedge
      REAL(SRK) :: angstart,angstop
      TYPE(PointType) :: centroid,refpoint
      TYPE(LineType) :: edge

      IF((thisPoly%nQuadEdge > 0) .AND. (0 < iquad) .AND. &
          (iquad <= thisPoly%nQuadEdge)) THEN
        iedge=thisPoly%quad2edge(iquad)
        CALL edge%set(thisPoly%vert(thisPoly%edge(1,iedge)), &
          thisPoly%vert(thisPoly%edge(2,iedge)))
        CALL centroid%init(DIM=2,X=thisPoly%quadEdge(1,iquad), &
          Y=thisPoly%quadEdge(2,iquad))
        !Need to get the starting and stopping angles...
        refpoint=centroid
        refpoint%coord(1)=refpoint%coord(1)+1.0_SRK
        !Starting angle uses the line endpoint
        IF(edge%pointIsRight(centroid)) THEN
          !starting angle is in quadrant 3 or 4
          IF(.NOT.(edge%p2%coord(2) .APPROXGE. centroid%coord(2))) THEN
            angstart=outerAngle(edge%p2,centroid,refpoint)
          ELSE
            angstart=innerAngle(edge%p2,centroid,refpoint)
          ENDIF
          !stopping angle is in quadrant 3 or 4
          IF(.NOT.(edge%p1%coord(2) .APPROXGE. centroid%coord(2))) THEN
            angstop=outerAngle(edge%p1,centroid,refpoint)
          ELSE
            angstop=innerAngle(edge%p1,centroid,refpoint)
          ENDIF
        !Starting angle uses the line startpoint
        ELSE
          !starting angle is in quadrant 3 or 4
          IF(.NOT.(edge%p1%coord(2) .APPROXGE. centroid%coord(2))) THEN
            angstart=outerAngle(edge%p1,centroid,refpoint)
          ELSE
            angstart=innerAngle(edge%p1,centroid,refpoint)
          ENDIF
          !stopping angle is in quadrant 3 or 4
          IF(.NOT.(edge%p2%coord(2) .APPROXGE. centroid%coord(2))) THEN
            angstop=outerAngle(edge%p2,centroid,refpoint)
          ELSE
            angstop=innerAngle(edge%p2,centroid,refpoint)
          ENDIF
        ENDIF
        IF(angstop < angstart) angstop=angstop+TWOPI
        CALL circle%set(centroid,thisPoly%quadEdge(3,iquad), &
          ANGSTT=angstart,ANGSTP=angstop)
        CALL centroid%clear()
        CALL refpoint%clear()
        CALL edge%clear()
      ENDIF
    ENDSUBROUTINE createArcFromQuad
!
ENDMODULE Geom_Poly
