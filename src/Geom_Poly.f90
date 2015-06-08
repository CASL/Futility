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
!>
!> @author Brendan Kochunas
!>    @date 06/06/2015
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Poly
  USE IntrType
  USE Constants_Conversion
  USE Geom_Points
  USE Geom_Line
  USE Geom_Plane
  USE Geom_CircCyl
  USE Geom_Box
  USE Geom_Graph
  
  IMPLICIT NONE
  PRIVATE
  
  !> @brief Type describing intersection between geometrical primitives
  !>
  !> Derived type to store an intersection between two surfaces/curves that
  !> potentially bound a region of the mesh.
  TYPE :: MesherIntersectType
    !> x coordinate of the intersection point
    REAL(SRK) :: x=0.0_SRK
    !> y coordinate of the intersection point
    REAL(SRK) :: y=0.0_SRK
    !> does it have a circle?
    LOGICAL :: circ=.FALSE.
    !> circle radius. The intersection can only involve one circle. This is a
    !> limitation which shouldnt cause any issues since out circles are always
    !> concentric.
    REAL(SRK) :: r=0.0_SRK
    !> If a value has been assigned to the intersection
    LOGICAL :: isSet=.FALSE.
!!
!!List of type bound procedure for the object
!    CONTAINS
!      !> Placeholder for the set routine
!      PROCEDURE,PASS :: set => set_MesherIntersectType
!      !> Placeholder for the clear routine
!      PROCEDURE,PASS :: clear => clear_MesherIntersectType
  ENDTYPE MesherIntersectType

  !> @brief Type used for storing a vertex in PolygonType
  !>
  !> Derived type to store the vertex in a polygon. The following derived type
  !> is a linked list of this type which stores all of the vertices in a whole
  !> polygon.
  TYPE :: PolygonVertexType
    !> An element in the linked list
    TYPE(MesherIntersectType) :: point
    !> Pointer to the next element in the linked list
    TYPE(PolygonVertexType),POINTER :: nextElem => NULL()
    !> this is the angle of the point w.r.t the centroid of the FSR. It is used
    !> to sort the points and ensure that the polygon is ordered CCW.
    REAL(SRK) :: theta
  ENDTYPE PolygonVertexType

  !> @brief Type used to describe a polygon.
  !>
  !> Derived type that forms a linked list of PolygonVertexType objects to
  !> describe a polygon. The insert method assumes that the polygon is convex,
  !> and sorts the vertices by angle to force the polygon to be ordered in CCW
  !> order. This is needed for the algorithm that is used to calculate the area
  !> of the polygon. The finalize method points the last element to the first.
  !> This makes the area calculation a lot cleaner.
  TYPE :: PolygonType
    !> The entry element of the list
    TYPE(PolygonVertexType),POINTER :: firstElem => NULL()
    !> Bounding box. ultimately all points must fall in this region
    TYPE(ABBoxType) :: box
    !> Minimum angular bound of the FSR of interest
    REAL(SRK) :: azMin=0.0_SRK
    !> Maximum angular bound of the FSR of interest
    REAL(SRK) :: azMax=0.0_SRK
    !> Minimum radial bound of the FSR of interest
    REAL(SRK) :: rMin=0.0_SRK
    !> Maximum radial bound of the FSR of interest
    REAL(SRK) :: rMax=0.0_SRK
    !> X coordinate of the reference point for angle-sorting.
    REAL(SRK) :: x=0.0_SRK
    !> Y coordinate of the reference point for angle-sorting.
    REAL(SRK) :: y=0.0_SRK
    !> The number of elements in the linked list of vertices
    INTEGER(SIK) :: nElem=0 
    !> Bool for if the object is initialized
    LOGICAL :: isInit=.FALSE.
!!
!!List of type bound procedure for the object
!    CONTAINS
!      !> Placeholder for set routine
!      PROCEDURE,PASS :: set    => set_PolygonType
!      !> Placeholder for clear routine
!      PROCEDURE,PASS :: clear  => clear_PolygonType
!      !> Placeholder for insertion routine
!      PROCEDURE,PASS :: insert => insert_PolygonType
!      !> Placeholder for the finilize routine (points last element at first)
!      PROCEDURE,PASS :: finalize => finalize_PolygonType
!      !> Placeholder for interior point routine
!      PROCEDURE,PASS :: interior => interior_PolygonType
!      !> Placeholder to the report routine
!      PROCEDURE,PASS :: report => report_PolygonType
!      !> Placeholder for the area calculation routine
!      PROCEDURE,PASS :: area => area_PolygonType
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
