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
!>  - @ref Geom_Line "Geom_CircCyl": @copybrief Geom_CircCyl
!>  - @ref Geom_Line "Geom_Graph": @copybrief Geom_Graph
!>
!> @author Brendan Kochunas, Dan Jabaay
!>    @date 06/06/2015
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Poly
  USE IntrType
  USE Allocs
  USE Constants_Conversion
  USE Geom_Points
  USE Geom_Line
  USE Geom_Plane
  USE Geom_CircCyl
  USE Geom_Graph
  
  IMPLICIT NONE
  PRIVATE
  
  PUBLIC :: PolygonType

  !> @brief Type used to describe a polygon.
  !>
  !> Derived type that forms a linked list of PolygonVertexType objects to
  !> describe a polygon. The insert method assumes that the polygon is convex,
  !> and sorts the vertices by angle to force the polygon to be ordered in CCW
  !> order. This is needed for the algorithm that is used to calculate the area
  !> of the polygon. The finalize method points the last element to the first.
  !> This makes the area calculation a lot cleaner.
  TYPE :: PolygonType
    !> Bool for if the object is initialized
    LOGICAL :: isInit=.FALSE.
    !> Area of the polygon
    REAL(SRK) :: area=0.0_SRK
    !> The number of elements in the linked list of vertices
    INTEGER(SIK) :: nVert=0 
    !> The number of elements in the linked list of edges
    INTEGER(SIK) :: nEdge=0 
    !> The list of vertices
    TYPE(PointType),ALLOCATABLE :: vert(:)
    !> The list of edges by the vert index (SIZE 2,nvert)
    INTEGER(SIK),ALLOCATABLE :: edge(:,:)
    !> The list of quadratic edges (circles) (SIZE 3,nEdge)
    REAL(SRK),ALLOCATABLE :: quadEdge(:,:)
!
!List of type bound procedure for the object
    CONTAINS
      !> @copybrief Geom_Poly::set_PolygonType
      !> @copydetails Geom_Poly::set_PolygonType
      PROCEDURE,PASS :: set => set_PolygonType
      !> @copybrief Geom_Poly::clear_PolygonType
      !> @copydetails Geom_Poly::clear_PolygonType
      PROCEDURE,PASS :: clear => clear_PolygonType
      !> @copybrief Geom_Poly::inside_PolygonType
      !> @copydetails Geom_Poly::inside_PolygonType
      PROCEDURE,PASS :: inside => inside_PolygonType
      !> @copybrief Geom_Poly::intersect_PolygonType
      !> @copydetails Geom_Poly::intersect_PolygonType
      PROCEDURE,PASS :: intersectLine => intersectLine_PolygonType
!      !> Placeholder for insertion routine
!      PROCEDURE,PASS :: insert => insert_PolygonType
!      !> Placeholder for the finilize routine (points last element at first)
!      PROCEDURE,PASS :: finalize => finalize_PolygonType
!      !> Placeholder for interior point routine
!      PROCEDURE,PASS :: interior => interior_PolygonType
  ENDTYPE PolygonType
    
  INTERFACE Polygonize
    MODULE PROCEDURE Polygonize_Circle
    MODULE PROCEDURE Polygonize_OBBox2D
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
    SUBROUTINE set_PolygonType(thisPoly,thatGraph)
      CLASS(PolygonType) :: thisPoly
      TYPE(GraphType) :: thatGraph
      
      INTEGER(SIK) :: i,icurr,inextold,inext,nvert
      REAL(SRK) :: a,r,h,R1,coeff
      TYPE(PointType) :: point
      TYPE(LineType) :: line
      
      !Check if thatGraph is closed (i.e. each vertex has only two neighbors)
      !Pick the starting vertex, loop over all vertices until back to the original,
      !  calculating angles for each set of three. Increment nElem.
      IF(ALLOCATED(thatGraph%vertices)) THEN
        !IF(thatGraph%isMinSet()) THEN
          !Get the number of vertices and edges (equal, since it's a minimum cycle)
          thisPoly%nVert=thatGraph%nVert()
          thisPoly%nEdge=thatGraph%nVert()
          ALLOCATE(thisPoly%vert(thisPoly%nVert))
          ALLOCATE(thisPoly%edge(2,thisPoly%nEdge))
          CALL dmallocA(thisPoly%quadEdge,3,thisPoly%nEdge)
          thisPoly%quadEdge=0.0_SRK
          !Setup initial points
          
          !Need to use the edgeMatrix to find the neighboring points, can't assume ordered.
          CALL thisPoly%vert(1)%init(DIM=2,X=thatGraph%vertices(1,1), &
            Y=thatGraph%vertices(2,1))
          thisPoly%edge(1,1)=1
          !Loop over all vertices in the graphtype in CW ordering.
          inext=thatGraph%getCWMostVert(0,1)
          inextold=1
          icurr=1
          DO i=2,thisPoly%nVert
            !Get the next vertex using the neighbor call
            !Using the CCW-th point, the area calc will be positive.  CW would be negative.
            CALL thisPoly%vert(i)%init(DIM=2,X=thatGraph%vertices(1,inext), &
              Y=thatGraph%vertices(2,inext))
            thisPoly%area=thisPoly%area+thisPoly%vert(i-1)%coord(1)* &
              thisPoly%vert(i)%coord(2)-thisPoly%vert(i-1)%coord(2)* &
              thisPoly%vert(i)%coord(1)
            !Set the edge
            thisPoly%edge(2,i-1)=i
            thisPoly%edge(1,i)=i
            !Check if it's a quadratic edge and assign
            IF(thatGraph%quadEdges(3,icurr,inext) > 0.0_SRK) &
              thisPoly%quadEdge(:,i-1)=thatGraph%quadEdges(:,icurr,inext)
            inextold=inext
            inext=thatGraph%getCWMostVert(icurr,inext)
            icurr=inextold
          ENDDO
          !Set last edge
          thisPoly%edge(2,thisPoly%nVert)=1
          thisPoly%area=thisPoly%area+thisPoly%vert(thisPoly%nVert)%coord(1)* &
            thisPoly%vert(1)%coord(2)-thisPoly%vert(thisPoly%nVert)%coord(2)* &
            thisPoly%vert(1)%coord(1)
          !Check if it's a quadratic edge and assign
          IF(thatGraph%quadEdges(3,1,icurr) > 0.0_SRK) &
            thisPoly%quadEdge(:,thisPoly%nVert)=thatGraph%quadEdges(:,1,icurr)
          !Last component of the area calc.
          thisPoly%area=ABS(thisPoly%area*0.5_SRK)
          
          !Now do the chord area checks, because they're all positive values.
          DO i=1,thisPoly%nEdge
            coeff=1.0_SRK
            IF(thisPoly%quadEdge(3,i) > 0.0_SRK) THEN
              CALL point%init(DIM=2,X=thisPoly%quadEdge(1,i), &
                Y=thisPoly%quadEdge(2,i))
              !Setup line to test whether to add or subtract
              CALL line%set(thisPoly%vert(thisPoly%edge(1,i)), &
                thisPoly%vert(thisPoly%edge(2,i)))
              !Modify area - taken from Wolfram's circular segment calculation.
              R1=thisPoly%quadEdge(3,i)
              !Calculate "a"
              a=distance(thisPoly%vert(thisPoly%edge(1,i)), &
                thisPoly%vert(thisPoly%edge(2,i)))
              !Calculate "r"
              r=0.5_SRK*SQRT(4.0_SRK*R1*R1-a*a)
              !Calculate "h"
              h=R1-r
              !Calculate Chord Sector area (Arc area - triangle area)
              IF(line%pointIsLeft(point)) coeff=-1.0_SRK
              thisPoly%area=thisPoly%area+coeff*(R1*R1*ACOS((R1-h)/R1)- &
                (R1-h)*SQRT(2*R1*h-h*h))
            ENDIF
          ENDDO
          thisPoly%isinit=.TRUE.
        !ENDIF
      ENDIF
      
    ENDSUBROUTINE set_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
    SUBROUTINE clear_PolygonType(thisPorygon)
      CLASS(PolygonType),INTENT(INOUT) :: thisPorygon
      INTEGER(SIK) :: i
    
      !Clear the vertex data
      DO i=thisPorygon%nVert,1,-1
        CALL thisPorygon%vert(i)%clear()
      ENDDO
      IF(ALLOCATED(thisPorygon%vert)) DEALLOCATE(thisPorygon%vert)
      IF(ALLOCATED(thisPorygon%edge)) DEALLOCATE(thisPorygon%edge)
      CALL demallocA(thisPorygon%quadEdge)
      thisPorygon%nVert=0
      thisPorygon%nEdge=0
      thisPorygon%area=0.0_SRK
      thisPorygon%isinit=.FALSE.
    ENDSUBROUTINE clear_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief 
!> @param
!>
    ELEMENTAL FUNCTION inside_PolygonType(thisPolygon,point) RESULT(bool)
      CLASS(PolygonType),INTENT(IN) :: thisPolygon
      TYPE(PointType),INTENT(IN) :: point
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i
      
      !Traverse through all the edges, test the point to see if it is always
      !to the left (or right) of the lines since we will be ordering them.
      !Only applies to convex polygons.
      bool=.FALSE.
      IF(thisPolygon%isinit) THEN
        DO i=1,thisPolygon%nEdge
          !Create line
          !
        ENDDO
      ENDIF
    ENDFUNCTION inside_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
    ELEMENTAL SUBROUTINE intersectLine_PolygonType(thisPolygon,line,p1,p2)
      CLASS(PolygonType),INTENT(IN) :: thisPolygon
      TYPE(LineType),INTENT(IN) :: line
      TYPE(PointType),INTENT(INOUT) :: p1,p2
      
      !Test the line with all the edges, make sure they aren't co-linear
      !Get the intersections from the line_line_intersect routine.
      !Should only need two points for convex geometry polygons.
      !If one of the edges is quadratic, calculate the point from the coefficients
      
    ENDSUBROUTINE intersectLine_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
    SUBROUTINE Polygonize_Circle(circle,lines,polygons)
      TYPE(CircleType),INTENT(IN) :: circle
      TYPE(LineType),INTENT(IN) :: lines(:)
      TYPE(PolygonType),ALLOCATABLE,INTENT(INOUT) :: polygons(:)
      
      INTEGER(SIK) :: i,nlines,npoints,npoly
      INTEGER(SIK),ALLOCATABLE :: nsegP(:)
      REAL(SRK) :: thetap,d
      TYPE(LinkedListPointType),ALLOCATABLE :: geomObjIntersections(:)
      TYPE(GraphType) :: graph
      TYPE(GraphType),ALLOCATABLE :: cycles(:)
      
!      IF(ALLOCATED(polygons)) THEN
!        DO i=1,SIZE(polygons)
!!          CALL polygons(i)%clear()
!        ENDDO
!        DEALLOCATE(polygons)
!      ENDIF
!      nlines=SIZE(lines)
!!
!!Determine all intersections and construct as graph
!      ALLOCATE(geomObjIntersections(0:nlines))
!      !Determine line-circle intersections
!      DO i=1,SIZE(lines)
!        CALL circle%intersectLine(lines(i),p1,p2)
!        IF(p1%dim == 2) THEN
!          thetap=circle%calcThetaOfPoint(p1)
!          CALL graph%insertVertex(p1%coord(1:2))
!          CALL geomObjIntersections(0)%insertPoint(p1,thetap)
!        ENDIF
!        IF(p2%dim == 2) THEN
!          npoints=npoints+1
!          thetap=circle%calcThetaOfPoint(p2)
!          CALL graph%insertVertex(p2%coord(1:2))
!          CALL geomObjIntersections(0)%insertPoint(p2,thetap)
!        ENDIF
!      ENDDO
!      !Define edges in graph from intersections with circle
!      curPoint => geomObjIntersections(0)%firstPoint
!      CALL graph%defineQuadraticEdge(curPoint,geomObjIntersections(0)%lastPoint)
!      DO WHILE(.NOT.ASSOCIATED(firstPoint,geomObjIntersections(0)%lastPoint))
!        CALL graph%defineQuadraticEdge(curPoint,curPoint%nextPoint,circle%r,circle%c)
!        curPoint => curPoint%nextPoint
!      ENDDO
!
!      !Update graph for all line-line intersections
!      DO i=1,SIZE(lines)
!        DO j=i+1,SIZE(lines)
!          p1=lines(i)%intersectLine(lines(j))
!          IF(p1%dim == 2) THEN
!            IF(cirlc%inside(p1)) THEN
!              npoints=npoints+1
!              CALL graph%insertVertex(p1%coord(1:2))
!              d=lines(i)%distance2Point(p1)
!              CALL geomObjIntersections(i)%insertPoint(p1,d)
!            ENDIF
!          ENDIF
!          CALL p1%clear()
!        ENDDO
!      ENDDO
!      DO i=1,nlines
!        curPoint => geomObjIntersections(i)%firstPoint
!        DO WHILE(.NOT.ASSOCIATED(curPoint,geomObjIntersections(i)%lastPoint))
!          CALL graph%defineEdge(curPoint,curPoint%nextPoint)
!          curPoint => curPoint%nextPoint
!        ENDDO
!      ENDDO
!      
!      
!      CALL graph%getMCB(cycles)
!      npoly=SIZE(cycles)
!      ALLOCATE(polygons(npoly))
!      DO i=1,npoly
!        CALL init_PolygonFromCycle(polygons(i),cycles(i))
!      ENDDO
    ENDSUBROUTINE Polygonize_Circle
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
    SUBROUTINE Polygonize_OBBox2D(obBox,lines,polygons)
      TYPE(CircleType),INTENT(IN) :: obBox
      TYPE(LineType),INTENT(IN) :: lines(:)
      TYPE(PolygonType),ALLOCATABLE,INTENT(INOUT) :: polygons(:)
    ENDSUBROUTINE Polygonize_OBBox2D
!
ENDMODULE Geom_Poly
