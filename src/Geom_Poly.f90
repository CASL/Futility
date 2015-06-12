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
!> @author Brendan Kochunas and Dan Jabaay
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
  TYPE :: PolygonType
    !> Bool for if the object is initialized
    LOGICAL :: isInit=.FALSE.
    !> Area of the polygon
    REAL(SRK) :: area=0.0_SRK
    !> The number of elements in the list of vertices
    INTEGER(SIK) :: nVert=0 
    !> The number of elements in the list of edges
    INTEGER(SIK) :: nQuadEdge=0 
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
      !> @copybrief Geom_Poly::inside_PolygonType
      !> @copydetails Geom_Poly::inside_PolygonType
      PROCEDURE,PASS :: inside => inside_PolygonType
      !> @copybrief Geom_Poly::intersect_PolygonType
      !> @copydetails Geom_Poly::intersect_PolygonType
      PROCEDURE,PASS :: intersectLine => intersectLine_PolygonType
      !> @copybrief Geom_Poly::isSimple_PolygonType
      !> @copydetails Geom_Poly::isSimple_PolygonType
      PROCEDURE,PASS :: isSimple => isSimple_PolygonType
      !> @copybrief Geom_Poly::subtractSubVolume_PolygonType
      !> @copydetails Geom_Poly::subtractSubVolume_PolygonType
      PROCEDURE,PASS :: subtractSubVolume => subtractSubVolume_PolygonType
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
      
      INTEGER(SIK) :: i,icw,iccw,icurr,inextold,inext,iedge,iquad
      REAL(SRK) :: a,r,h,R1,coeff
      TYPE(PointType) :: point
      TYPE(LineType) :: line
      
      !Check if thatGraph is closed (i.e. each vertex has only two neighbors)
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
        IF(thatGraph%vertices(2,icw) > thatGraph%vertices(2,iccw)) THEN
          inext=icw
        ELSE
          inext=iccw
        ENDIF
        inextold=1
        icurr=1
        DO i=2,thisPoly%nVert
          CALL thisPoly%vert(i)%init(DIM=2,X=thatGraph%vertices(1,inext), &
            Y=thatGraph%vertices(2,inext))
          !Using the CW-th point, the area calc will be negative.  Hence the ABS()
          thisPoly%area=thisPoly%area+thisPoly%vert(i-1)%coord(1)* &
            thisPoly%vert(i)%coord(2)-thisPoly%vert(i-1)%coord(2)* &
            thisPoly%vert(i)%coord(1)
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
        thisPoly%area=thisPoly%area+thisPoly%vert(thisPoly%nVert)%coord(1)* &
          thisPoly%vert(1)%coord(2)-thisPoly%vert(thisPoly%nVert)%coord(2)* &
          thisPoly%vert(1)%coord(1)
        !Check if it's a quadratic edge to count
        IF(thatGraph%quadEdges(3,1,icurr) > 0.0_SRK) thisPoly%nQuadEdge=thisPoly%nQuadEdge+1
        !Last component of the area calc.
        thisPoly%area=ABS(thisPoly%area*0.5_SRK)
          
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
          IF(thatGraph%vertices(2,icw) > thatGraph%vertices(2,iccw)) THEN
            inext=icw
          ELSE
            inext=iccw
          ENDIF
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
            coeff=1.0_SRK
            IF(thisPoly%quadEdge(3,i) > 0.0_SRK) THEN
              CALL point%init(DIM=2,X=thisPoly%quadEdge(1,i), &
                Y=thisPoly%quadEdge(2,i))
              !Setup line to test whether to add or subtract
              iedge=thisPoly%quad2edge(i)
              CALL line%set(thisPoly%vert(thisPoly%edge(1,iedge)), &
                thisPoly%vert(thisPoly%edge(2,iedge)))
              !Modify area - taken from Wolfram's circular segment calculation.
              R1=thisPoly%quadEdge(3,i)
              !Calculate "a"
              a=distance(thisPoly%vert(thisPoly%edge(1,iedge)), &
                thisPoly%vert(thisPoly%edge(2,iedge)))
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
        ENDIF
        thisPoly%isinit=.TRUE.
      ENDIF
    ENDSUBROUTINE set_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
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
      thisPolygon%isinit=.FALSE.
    ENDSUBROUTINE clear_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief This routine determines whether a point lies within an arbitrary 
!> polygon.  The processes by which this can be done are the ray tracing "count
!> number" approach, or the winding number approach.  After researching the 
!> problem, it was determined that the winding number provides the best solution
!> in the case that the polygon is non-convex.  A paper was found by Hormann and 
!> Agathos describing several approaches.  This routine applies the winding 
!> number algorithm 6 from their paper:
!> http://www.inf.usi.ch/hormann/papers/Hormann.2001.TPI.pdf
!>
!> @param thisPoly The polygon type used in the query
!> @param point The point type to check if it lies inside the polygontype
!> @param bool The logical result of this operation.  TRUE if the point is inside.
!>
!> NOTES: Working on getting this functional with quadratic edges.
!>
    ELEMENTAL FUNCTION inside_PolygonType(thisPoly,point) RESULT(bool)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      TYPE(PointType),INTENT(IN) :: point
      LOGICAL(SBK) :: bool,inPoly,inConvexCirc,inConcaveCirc
      INTEGER(SIK) :: i,wn,istt,istp,iedge
      TYPE(PointType) :: centroid
      TYPE(LineType) :: line
      TYPE(CircleType) :: circ
      
      bool=.FALSE.
      wn=0
      DO i=1,thisPoly%nVert
        istt=thisPoly%edge(1,i)
        istp=thisPoly%edge(2,i)
        !Crossing
        IF((thisPoly%vert(istt)%coord(2) < point%coord(2)) /= &
          (thisPoly%vert(istp)%coord(2) < point%coord(2))) THEN
          !P_i(x) >= R(x)
          IF(thisPoly%vert(istt)%coord(1) .APPROXGE. point%coord(1)) THEN
            !P_i+1(x) > R(x)
            IF(thisPoly%vert(istp)%coord(1) > point%coord(1)) THEN
              IF(thisPoly%vert(istp)%coord(2) > thisPoly%vert(istt)%coord(2)) THEN
                wn=wn+1
              ELSE
                wn=wn-1
              ENDIF
            ELSE !det() > 0.0_SRK .EQV. P_i+1(y) > P_i(y), right_crossing
              !(P_i(x)-R(x))*(P_i+1(y)-R(y))-(P_i+1(x)-R(x))*(P_i(y)-R(y))
              IF(((thisPoly%vert(istt)%coord(1)-point%coord(1))* &
                (thisPoly%vert(istp)%coord(2)-point%coord(2))- &
                  (thisPoly%vert(istp)%coord(1)-point%coord(1))* &
                (thisPoly%vert(istt)%coord(2)-point%coord(2)) > 0.0_SRK) .EQV. &
                  (thisPoly%vert(istp)%coord(2) > point%coord(2))) THEN
                !modify wn
                IF(thisPoly%vert(istp)%coord(2) > thisPoly%vert(istt)%coord(2)) THEN
                  wn=wn+1
                ELSE
                  wn=wn-1
                ENDIF
              ENDIF
            ENDIF
          ELSE
            IF(thisPoly%vert(istp)%coord(1) > point%coord(1)) THEN
              !det() > 0.0_SRK .EQV. P_i+1(y) > P_i(y), right_crossing
              !(P_i(x)-R(x))*(P_i+1(y)-R(y))-(P_i+1(x)-R(x))*(P_i(y)-R(y))
              IF(((thisPoly%vert(istt)%coord(1)-point%coord(1))* &
                (thisPoly%vert(istp)%coord(2)-point%coord(2))- &
                  (thisPoly%vert(istp)%coord(1)-point%coord(1))* &
                (thisPoly%vert(istt)%coord(2)-point%coord(2)) > 0.0_SRK) .EQV. &
                  (thisPoly%vert(istp)%coord(2) > point%coord(2))) THEN
                !modify wn
                IF(thisPoly%vert(istp)%coord(2) > thisPoly%vert(istt)%coord(2)) THEN
                  wn=wn+1
                ELSE
                  wn=wn-1
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      inPoly=(wn /= 0)
      
      !Now check the quadratic edges if there are any
      inConvexCirc=.FALSE.
      inConcaveCirc=.FALSE.
      IF(thisPoly%nQuadEdge > 0) THEN
        DO i=1,thisPoly%nQuadEdge
          iedge=thisPoly%quad2edge(i)
          CALL centroid%init(DIM=2,X=thisPoly%quadEdge(1,i),Y=thisPoly%quadEdge(2,i))
          CALL line%set(thisPoly%vert(thisPoly%edge(1,iedge)), &
            thisPoly%vert(thisPoly%edge(2,iedge)))
          CALL circ%set(centroid,thisPoly%quadEdge(3,i))
          !Check if the centroid is on the interior or exterior of the edge
          IF(line%pointIsRight(centroid) .AND. .NOT. inConvexCirc) THEN
            !Quad edge extends outside the polygon.  
            !Check if the point is to the left of the edge and inside the circle
            inConvexCirc=line%pointIsLeft(point) .AND. circ%inside(point)
          ELSEIF(line%pointIsLeft(centroid) .AND. .NOT. inConcaveCirc) THEN
            !Quad edge extends inside the polygon.  
            !Check if the point is to the right of the edge and outside the circle
            inConcaveCirc=line%pointIsRight(point) .AND. circ%inside(point)
          ENDIF
          CALL circ%clear()
          CALL line%clear()
          CALL centroid%clear()
        ENDDO
        !Logic for the cases where there is inside and outside circles.
        bool=(inPoly .OR. inConvexCirc) .AND. .NOT. inConcaveCirc
      !If just straight edges
      ELSE
        bool=inPoly
      ENDIF
    ENDFUNCTION inside_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
    PURE SUBROUTINE intersectLine_PolygonType(thisPolygon,line,points)
      CLASS(PolygonType),INTENT(IN) :: thisPolygon
      TYPE(LineType),INTENT(IN) :: line
      TYPE(PointType),ALLOCATABLE,INTENT(INOUT) :: points(:)
      INTEGER(SIK) :: i,j,iedge,ipoint,npoints
      REAL(SRK) :: angstart,angstop,x,y
      TYPE(PointType) :: centroid,refpoint
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
      
      !Construct line and circle types locally
      ALLOCATE(lines(thisPolygon%nVert))
      IF(thisPolygon%nQuadEdge > 0) ALLOCATE(circles(thisPolygon%nQuadEdge))
      
      DO i=1,thisPolygon%nVert
        CALL lines(i)%set(thisPolygon%vert(thisPolygon%edge(1,i)), &
          thisPolygon%vert(thisPolygon%edge(2,i)))
      ENDDO
      DO i=1,thisPolygon%nQuadEdge
        CALL centroid%init(DIM=2,X=thisPolygon%quadEdge(1,i), &
          Y=thisPolygon%quadEdge(2,i))
        !Need to get the starting and stopping angles...
        refpoint=centroid
        refpoint%coord(1)=refpoint%coord(1)+1.0_SRK
        iedge=thisPolygon%quad2edge(i)
        !Starting angle uses the line endpoint
        IF(lines(iedge)%pointIsRight(centroid)) THEN
          !starting angle is in quadrant 3 or 4
          IF(lines(iedge)%p2%coord(2) .APPROXLE. centroid%coord(2)) THEN
            angstart=outerAngle(lines(iedge)%p2,centroid,refpoint)
          ELSE
            angstart=innerAngle(lines(iedge)%p2,centroid,refpoint)
          ENDIF
          !stopping angle is in quadrant 3 or 4
          IF(lines(iedge)%p1%coord(2) .APPROXLE. centroid%coord(2)) THEN
            angstop=outerAngle(lines(iedge)%p1,centroid,refpoint)
          ELSE
            angstop=innerAngle(lines(iedge)%p1,centroid,refpoint)
          ENDIF
        !Starting angle uses the line startpoint
        ELSE
          !starting angle is in quadrant 3 or 4
          IF(lines(iedge)%p1%coord(2) .APPROXLE. centroid%coord(2)) THEN
            angstart=outerAngle(lines(iedge)%p1,centroid,refpoint)
          ELSE
            angstart=innerAngle(lines(iedge)%p1,centroid,refpoint)
          ENDIF
          !stopping angle is in quadrant 3 or 4
          IF(lines(iedge)%p2%coord(2) .APPROXLE. centroid%coord(2)) THEN
            angstop=outerAngle(lines(iedge)%p2,centroid,refpoint)
          ELSE
            angstop=innerAngle(lines(iedge)%p2,centroid,refpoint)
          ENDIF
        ENDIF
        CALL circles(i)%set(centroid,thisPolygon%quadEdge(3,i), &
          ANGSTT=angstart,ANGSTP=angstop)
        CALL centroid%clear()
        CALL refpoint%clear()
      ENDDO
      !Make tmppoints the max possible size
      ALLOCATE(tmppoints(thisPolygon%nVert+3*thisPolygon%nQuadEdge))
      
      !Test the line with all the edges, 
      !Intersect circles first if necessary
      ipoint=1
      DO i=1,thisPolygon%nQuadEdge
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
          ENDIF
        ENDDO
        ipoint=ipoint+4
      ENDDO
      
      !Intersect the remaining lines if necessary
      IF(thisPolygon%nQuadEdge > 0) THEN
        DO i=1,SIZE(lines)
          !Intersect the line
          IF(ALL(i /= thisPolygon%quad2edge)) THEN 
            tmppoints(ipoint)=lines(i)%intersectLine(line)
            ipoint=ipoint+1
          ENDIF
        ENDDO
      ELSE
        DO i=1,SIZE(lines)
          tmppoints(ipoint)=lines(i)%intersectLine(line)
          ipoint=ipoint+1
        ENDDO
      ENDIF
      
      !Find all the good points
      npoints=0
      DO i=1,SIZE(tmppoints)
        IF(tmppoints(i)%dim == 2) npoints=npoints+1
      ENDDO
      IF(npoints > 0) THEN
        ALLOCATE(points(npoints))
      
        !Size the final points array and copy the good points
        ipoint=1
        DO i=1,SIZE(tmppoints)
          IF(tmppoints(i)%dim == 2) THEN
            points(ipoint)=tmppoints(i)
            ipoint=ipoint+1
          ENDIF
        ENDDO
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
    ENDSUBROUTINE intersectLine_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
    ELEMENTAL FUNCTION isSimple_PolygonType(thisPoly) RESULT(bool)
      CLASS(PolygonType),INTENT(IN) :: thisPoly
      LOGICAL(SBK) :: bool
      bool=ASSOCIATED(thisPoly%subRegions)
    ENDFUNCTION isSimple_PolygonType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
    SUBROUTINE subtractSubVolume_PolygonType(thisPoly,subPoly)
      CLASS(PolygonType),INTENT(INOUT) :: thisPoly
      TYPE(PolygonType),TARGET,INTENT(IN) :: subPoly
      LOGICAL(SBK) :: allIn
      INTEGER(SIK) :: i,v1,v2
      REAL(SRK) :: a(2),b(2),c(2),r,ac(2),bc(2),m(2),scal
      TYPE(PointType) :: arcMidPoint
      TYPE(PolygonType),POINTER :: lastSubPoly
      
      IF(thisPoly%isInit .AND. subPoly%isInit) THEN
        allIn=.TRUE.
        !Check that all vertices are inside the bounding polygon
        DO i=1,subPoly%nVert
          allIn=(allIn .AND. inside_PolygonType(thisPoly,subPoly%vert(i)))
        ENDDO
        
        !Check quadratic edges to make sure their outermost point is still
        !inside the bounding polygon
        IF(allIn) THEN
          DO i=1,subPoly%nQuadEdge
            v1=thisPoly%edge(1,thisPoly%quad2Edge(i))
            v2=thisPoly%edge(2,thisPoly%quad2Edge(i))
            a=thisPoly%vert(v1)%coord
            b=thisPoly%vert(v2)%coord
            c=thisPoly%quadEdge(1:2,i)
            r=thisPoly%quadEdge(3,i)
          
            m=a+b-2.0_SRK*c
            scal=r/SQRT(m(1)*m(1)+m(2)*m(2))
            m=m*scal
            m=m+c
            CALL arcMidPoint%init(COORD=m)
            allIn=(allIn .AND. inside_PolygonType(thisPoly,arcMidPoint))
          ENDDO
        ENDIF
        
        IF(allIn) THEN
          lastSubPoly => thisPoly%subRegions
          DO WHILE(ASSOCIATED(lastSubPoly))
            lastSubPoly => lastSubPoly%nextPoly
          ENDDO
          lastSubPoly => subPoly
          thisPoly%area=thisPoly%area-subPoly%area
        ENDIF
      ENDIF
    ENDSUBROUTINE subtractSubVolume_PolygonType
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
