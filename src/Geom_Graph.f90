!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module defining a graph type.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Graph
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE Constants_Conversion
USE ExtendedMath
USE Allocs
USE VTKFiles
USE Geom_Points
USE Geom_Line
USE Geom_CircCyl

IMPLICIT NONE
PRIVATE

PUBLIC :: GraphType
PUBLIC :: DAGraphType
PUBLIC :: OPERATOR(==)

!> @brief a Directed Acyclic Graph Type
TYPE :: DAGraphType
  !> The number of nodes on the graph
  INTEGER(SIK) :: n=0
  !> Nodes (Size n)
  INTEGER(SIK),ALLOCATABLE :: nodes(:)
  !> Edges of connectivity Size(n,n)
  INTEGER(SIK),ALLOCATABLE :: edgeMatrix(:,:)
!
!List of type bound procedures (methods) for the object
  CONTAINS
    !> @copybrief Geom_Graph::init_DAGraphType
    !> @copydetails Geom_Graph::init_DAGraphType
    PROCEDURE,PASS :: init => init_DAGraphType
    !> @copybrief Geom_Graph::clear_DAGraphType
    !> @copydetails Geom_Graph::clear_DAGraphType
    PROCEDURE,PASS :: clear => clear_DAGraphType
    !> @copybrief Geom_Graph::defineEdge_DAGraphType
    !> @copydetails Geom_Graph::defineEdge_DAGraphType
    PROCEDURE,PASS :: insertNode=> insertNode_DAGraphType
    !> @copybrief Geom_Graph::removeNode_DAGraphType
    !> @copydetails Geom_Graph::removeNode_DAGraphType
    PROCEDURE,PASS :: removeNode=> removeNode_DAGraphType
    !> @copybrief Geom_Graph::isStartNode_DAGraphType
    !> @copydetails Geom_Graph::isStartNode_DAGraphType
    PROCEDURE,PASS :: isStartNode=> isStartNode_DAGraphType
    !> @copybrief Geom_Graph::getNextStartNode_DAGraphType
    !> @copydetails Geom_Graph::getNextStartNode_DAGraphType
    PROCEDURE,PASS :: getNextStartNode=> getNextStartNode_DAGraphType
    !> @copybrief Geom_Graph::defineEdge_DAGraphType
    !> @copydetails Geom_Graph::defineEdge_DAGraphType
    PROCEDURE,PASS :: defineEdge => defineEdge_DAGraphType
    !> @copybrief Geom_Graph::removeEdge_DAGraphType
    !> @copydetails Geom_Graph::removeEdge_DAGraphType
    PROCEDURE,PASS :: removeEdge => removeEdge_DAGraphType
    !> @copybrief Geom_Graph::getIndex_DAGraphType
    !> @copydetails Geom_Graph::getIndex_DAGraphType
    PROCEDURE,PASS,PRIVATE :: getIndex => getIndex_DAGraphType
    !> @copybrief Geom_Graph::KATS_DAGraphType
    !> @copydetails Geom_Graph::KATS_DAGraphType
    PROCEDURE,PASS :: KATS => KATS_DAGraphType
ENDTYPE DAGraphType

!> @brief a Planar Graph
TYPE :: GraphType
  !Only allocated during execution of getMCB
  LOGICAL(SBK),ALLOCATABLE,PRIVATE :: isCycleEdge(:,:)
  !> A list of vertices in the graph.
  !> The list is sorted lexicographically
  !> e.g. V_i with (x_i,y_i) and V_i+1 with (x_i+1,y_i+1) then
  !> x_i < x_i+1 or if x_i == x_i+1 then y_i < y_i+1
  !> The insert vertex routine inserts the vertex in order. Duplicate points
  !> are not stored.
  REAL(SRK),ALLOCATABLE :: vertices(:,:)
  !> Matrix indicating connectivity of graph
  !> 0 means no connection, 1 means linear connection,
  !> -1 means quadratic connection. Diagonal is 0 and matrix is symmetric.
  !> size is (nvertices,nvertices)
  INTEGER(SIK),ALLOCATABLE :: edgeMatrix(:,:)
  !> Similar to edgeMatrix component except for entries with -1 it stores
  !> the center of rotation and radius to define the quadratic edge.
  !> size is (3,nvertices,nvertices)
  REAL(SRK),ALLOCATABLE :: quadEdges(:,:,:)
!
!List of type bound procedures (methods) for the object
  CONTAINS
    !> @copybrief Geom_Graph::nVert_graphType
    !> @copydetails Geom_Graph::nVert_graphType
    PROCEDURE,PASS :: nVert => nVert_graphType
    !> @copybrief Geom_Graph::nEdge_graphType
    !> @copydetails Geom_Graph::nEdge_graphType
    PROCEDURE,PASS :: nEdge => nEdge_graphType
    !> @copybrief Geom_Graph::getVertIndex_Real
    !> @copydetails Geom_Graph::getVertIndex_Real
    PROCEDURE,PASS,PRIVATE :: getVertIndex_Real
    !> @copybrief Geom_Graph::getVertIndex_Point
    !> @copydetails Geom_Graph::getVertIndex_Point
    PROCEDURE,PASS,PRIVATE :: getVertIndex_Point
    GENERIC :: getVertIndex => getVertIndex_Real,getVertIndex_Point
    !> @copybrief Geom_Graph::nAdjacent_graphType
    !> @copydetails Geom_Graph::nAdjacent_graphType
    PROCEDURE,PASS :: nAdjacent => nAdjacent_graphType
    !> @copybrief Geom_Graph::getAdjacentVert_graphType
    !> @copydetails Geom_Graph::getAdjacentVert_graphType
    PROCEDURE,PASS :: getAdjacentVert => getAdjacentVert_graphType
    !> @copybrief Geom_Graph::getCWMostVert_graphType
    !> @copydetails Geom_Graph::getCWMostVert_graphType
    PROCEDURE,PASS :: getCWMostVert => getCWMostVert_graphType
    !> @copybrief Geom_Graph::getCCWMostVert_graphType
    !> @copydetails Geom_Graph::getCCWMostVert_graphType
    PROCEDURE,PASS :: getCCWMostVert => getCCWMostVert_graphType
    !> @copybrief Geom_Graph::getMidPointOnEdge
    !> @copydetails Geom_Graph::getMidPointOnEdge
    PROCEDURE,PASS :: getMidPointOnEdge
    !> @copybrief Geom_Graph::isMinimumCycle_graphType
    !> @copydetails Geom_Graph::isMinimumCycle_graphType
    PROCEDURE,PASS :: isMinimumCycle => isMinimumCycle_graphType
    !> @copybrief Geom_Graph::insertVertex_graphType
    !> @copydetails Geom_Graph::insertVertex_graphType
    PROCEDURE,PASS :: insertVertex => insertVertex_graphType
    !> @copybrief Geom_Graph::defineLinearEdge_graphType
    !> @copydetails Geom_Graph::defineLinearEdge_graphType
    PROCEDURE,PASS :: defineLinearEdge => defineLinearEdge_graphType
    !> @copybrief Geom_Graph::defineQuadraticEdge
    !> @copydetails Geom_Graph::defineQuadraticEdge
    PROCEDURE,PASS :: defineQuadraticEdge
    !> @copybrief Geom_Graph::removeVertex_graphType
    !> @copydetails Geom_Graph::removeVertex_graphType
    PROCEDURE,PASS,PRIVATE :: removeVertex_graphType
    !> @copybrief Geom_Graph::removeVertex_idx_graphType
    !> @copydetails Geom_Graph::removeVertex_idx_graphType
    PROCEDURE,PASS,PRIVATE :: removeVertex_idx_graphType
    GENERIC :: removeVertex => removeVertex_graphType,removeVertex_idx_graphType
    !> @copybrief Geom_Graph::removeEdge_graphType
    !> @copydetails Geom_Graph::removeEdge_graphType
    PROCEDURE,PASS,PRIVATE :: removeEdge_graphType
    !> @copybrief Geom_Graph::removeVertex_IJ_graphType
    !> @copydetails Geom_Graph::removeVertex_IJ_graphType
    PROCEDURE,PASS,PRIVATE :: removeEdge_IJ_graphType
    GENERIC :: removeEdge => removeEdge_graphType,removeEdge_IJ_graphType
    !> @copybrief Geom_Graph::removeFilament_vertIdx_graphType
    !> @copydetails Geom_Graph::removeFilament_vertIdx_graphType
    PROCEDURE,PASS :: removeFilamentFromVert => removeFilament_vertIdx_graphType
    !> @copybrief Geom_Graph::getMCB_graphType
    !> @copydetails Geom_Graph::getMCB_graphType
    PROCEDURE,PASS :: getMCB => getMCB_graphType
    !> @copybrief Geom_Graph::editToVTK_graphType
    !> @copydetails Geom_Graph::editToVTK_graphType
    PROCEDURE,PASS :: editToVTK => editToVTK_graphType
    !> @copybrief Geom_Graph::combine_graphType
    !> @copydetails Geom_Graph::combine_graphType
    PROCEDURE,PASS :: combineGraph => combine_graphType
    !> @copybrief Geom_Graph::triangulateVerts_graphType
    !> @copydetails Geom_Graph::triangulateVerts_graphType
    PROCEDURE,PASS :: triangulateVerts => triangulateVerts_graphType
    !> @copybrief Geom_Graph::clear_graphType
    !> @copydetails Geom_Graph::clear_graphType
    PROCEDURE,PASS :: clear => clear_graphType
    !> @copybrief Geom_Graph::divideLinearEdge
    !> @copydetails Geom_Graph::divideLinearEdge
    PROCEDURE,PASS,PRIVATE :: divideLinearEdge
    !> @copybrief Geom_Graph::divideQuadEdge
    !> @copydetails Geom_Graph::divideQuadEdge
    PROCEDURE,PASS,PRIVATE :: divideQuadEdge
    GENERIC :: divideEdge => divideLinearEdge,divideQuadEdge
ENDTYPE GraphType

INTERFACE OPERATOR(==)
  !> @copybrief Geom_Graph::isequal_graphType
  !> @copydetails Geom_Graph::isequal_graphType
  MODULE PROCEDURE isequal_graphType
ENDINTERFACE
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Returns the number of vertexes in a graph object
!> @param thisGraph the graph to interrogate
!> @returns n the number of vertexes
!>
ELEMENTAL FUNCTION nVert_graphType(thisGraph) RESULT(n)
  CLASS(GraphType),INTENT(IN) :: thisGraph
  INTEGER(SIK) :: n
  n=0
  IF(ALLOCATED(thisGraph%vertices)) n=SIZE(thisGraph%vertices,DIM=2)
ENDFUNCTION nVert_graphType
!
!-------------------------------------------------------------------------------
!> @brief returnes the number of edges in a graph object
!> @param thisGraph the graph to interrogate
!> @returns n the number of edges
!>
ELEMENTAL FUNCTION nEdge_graphType(thisGraph) RESULT(n)
  CLASS(GraphType),INTENT(IN) :: thisGraph
  INTEGER(SIK) :: n
  n=0
  IF(ALLOCATED(thisGraph%edgeMatrix)) n=SUM(ABS(thisGraph%edgeMatrix))/2
ENDFUNCTION nEdge_graphType
!
!-------------------------------------------------------------------------------
!> @brief Returns the index of a vertex in a graph given the coordinates
!> @param thisGraph the graph to interrogate
!> @param coord the coordinates of the vertex
!> @returns idx the index of the vertex
!>
!> If the vertex could not be found, -1 is returned
!>
FUNCTION getVertIndex_Real(thisGraph,coord) RESULT(idx)
  CLASS(GraphType),INTENT(IN) :: thisGraph
  REAL(SRK),INTENT(IN) :: coord(2)
  INTEGER(SIK) :: idx
  INTEGER(SIK) :: i,j,n
  idx=-1
  n=nVert_graphType(thisGraph)
  DO i=1,n
    IF(coord(1) .APPROXEQA. thisGraph%vertices(1,i)) THEN
      IF(coord(2) .APPROXEQA. thisGraph%vertices(2,i)) THEN
        idx=i
      ELSE
        DO j=i+1,n
          IF(.NOT.(coord(1) .APPROXEQA. thisGraph%vertices(1,j))) EXIT
          IF(coord(2) .APPROXEQA. thisGraph%vertices(2,j)) THEN
            idx=j
            EXIT
          ENDIF
        ENDDO
      ENDIF
      EXIT
    ENDIF
  ENDDO
ENDFUNCTION getVertIndex_Real
!
!-------------------------------------------------------------------------------
!> @brief Returns the index of a vertex in a graph given the coordinates
!> @param thisGraph the graph to interrogate
!> @param point the point containing the coordinates of the vertex
!> @returns idx the index of the vertex
!>
!> If the vertex could not be found, -1 is returned
!>
FUNCTION getVertIndex_Point(thisGraph,point) RESULT(idx)
  CLASS(GraphType),INTENT(IN) :: thisGraph
  CLASS(PointType),INTENT(IN) :: point
  REAL(SRK) :: idx
  idx=thisGraph%getVertIndex_Real(point%coord)
ENDFUNCTION getVertIndex_Point
!
!-------------------------------------------------------------------------------
!> @brief Returns the number of points connected to a specified point
!> @param thisGraph the graph to interrogate
!> @param i the index of the vertex to interrogate
!> @returns n the number of connected points
!>
ELEMENTAL FUNCTION nAdjacent_graphType(thisGraph,i) RESULT(n)
  CLASS(GraphType),INTENT(IN) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK) :: n
  n=0
  IF(ALLOCATED(thisGraph%edgeMatrix)) THEN
    IF(0 < i .AND. i < SIZE(thisGraph%edgeMatrix,DIM=2)+1) &
        n=SUM(ABS(thisGraph%edgeMatrix(:,i)))
  ENDIF
ENDFUNCTION nAdjacent_graphType
!
!-------------------------------------------------------------------------------
!> @brief Returns a particular adjacent vertex
!> @param thisGraph the graph object to interrogate
!> @param v0 the index of the vertex to interrogate
!> @param i which adjacent vertex to return (from 1 to @c thisGraph%nAdjacent(v0))
!> @param v1 the graph vertex index for the @c i-th adjacent vertex to @c v0
!>
ELEMENTAL FUNCTION getAdjacentVert_graphType(thisGraph,v0,i) RESULT(v1)
  CLASS(GraphType),INTENT(IN) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: v0
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK) :: v1
  INTEGER(SIK) :: j,n,nVert

  v1=0
  nVert=nVert_graphType(thisGraph)
  IF(0 < v0 .AND. v0 < nVert+1) THEN
    IF(0 < i .AND. i < SUM(ABS(thisGraph%edgeMatrix(:,v0)))+1) THEN
      n=0
      DO j=1,nVert
        IF(thisGraph%edgeMatrix(j,v0) /= 0) n=n+1
        IF(n == i) THEN
          v1=j
          EXIT
        ENDIF
      ENDDO
    ENDIF
  ENDIF
ENDFUNCTION getAdjacentVert_graphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
ELEMENTAL FUNCTION getCWMostVert_graphType(thisGraph,v0,vCurr) RESULT(vNext)
  CLASS(GraphType),INTENT(IN) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: vCurr
  INTEGER(SIK),INTENT(IN) :: v0
  LOGICAL(SBK) :: isVCurrConvex,badEdge
  INTEGER(SIK) :: vNext,vPrev,vi,i,nVert,nAdj
  REAL(SRK) :: dcurr(2),dnext(2),di(2)

  vNext=0
  nVert=nVert_graphType(thisGraph)
  vPrev=v0
  IF(vPrev == vCurr) vPrev=0
  IF(0 < vCurr .AND. vCurr <= nVert .AND. 0 <= vPrev .AND. vPrev <= nVert) THEN
    badEdge=.FALSE.
    IF(vPrev > 0) badEdge=thisGraph%edgeMatrix(vCurr,vPrev) == 0

    IF(.NOT.badEdge) THEN
      nAdj=nAdjacent_graphType(thisGraph,vCurr)
      IF(nAdj == 1) THEN
        !Shortcut for 1 adjacent vert
        vNext=getAdjacentVert_graphType(thisGraph,vCurr,1)
        IF(vNext == vPrev) vNext=0
      ELSEIF(nAdj > 1) THEN
        !Get default vNext (first vertice found that is not vPrev)
        DO i=1,nAdj
          vi=getAdjacentVert_graphType(thisGraph,vCurr,i)
          IF(vi /= vPrev) THEN
            vNext=vi
            EXIT
          ENDIF
        ENDDO

        !Search other vertices
        dcurr=(/0.0_SRK,-1.0_SRK/)
        IF(vPrev > 0) dcurr=thisGraph%vertices(:,vCurr)- &
            thisGraph%vertices(:,vPrev)
        dnext=thisGraph%vertices(:,vNext)-thisGraph%vertices(:,vCurr)
        isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) .APPROXLE. 0.0_SRK)
        DO i=1,nAdj
          vi=getAdjacentVert_graphType(thisGraph,vCurr,i)
          IF(vi /= vPrev .AND. vi /= vNext) THEN
            di=thisGraph%vertices(:,vi)-thisGraph%vertices(:,vCurr)
            IF(isVCurrConvex) THEN
              IF(dcurr(1)*di(2)-dcurr(2)*di(1) < 0.0_SRK .OR. &
                  dnext(1)*di(2)-dnext(2)*di(1) < 0.0_SRK) THEN
                vNext=vi
                dnext=di
                isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) .APPROXLE. 0.0_SRK)
              ENDIF
            ELSE
              IF(dcurr(1)*di(2)-dcurr(2)*di(1) < 0.0_SRK .AND. &
                  dnext(1)*di(2)-dnext(2)*di(1) < 0.0_SRK) THEN
                vNext=vi
                dnext=di
                isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) .APPROXLE. 0.0_SRK)
              ENDIF
            ENDIF
          ENDIF
        ENDDO !End searching over vertices
      ENDIF
    ENDIF
  ENDIF
ENDFUNCTION getCWMostVert_graphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
ELEMENTAL FUNCTION getCCWMostVert_graphType(thisGraph,v0,vCurr) RESULT(vNext)
  CLASS(GraphType),INTENT(IN) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: vCurr
  INTEGER(SIK),INTENT(IN) :: v0
  LOGICAL(SBK) :: isVCurrConvex,badEdge
  INTEGER(SIK) :: vNext,vPrev,vi,i,nVert,nAdj
  REAL(SRK) :: dcurr(2),dnext(2),di(2)

  vNext=0
  nVert=nVert_graphType(thisGraph)
  vPrev=v0
  IF(vPrev == vCurr) vPrev=0
  IF(0 < vCurr .AND. vCurr <= nVert .AND. 0 <= vPrev .AND. vPrev <= nVert) THEN
    badEdge=.FALSE.
    IF(vPrev > 0) badEdge=thisGraph%edgeMatrix(vCurr,vPrev) == 0

    IF(.NOT.badEdge) THEN
      nAdj=nAdjacent_graphType(thisGraph,vCurr)
      IF(nAdj == 1) THEN
        !Shortcut for 1 adjacent vert
        vNext=getAdjacentVert_graphType(thisGraph,vCurr,1)
        IF(vNext == vPrev) vNext=0
      ELSEIF(nAdj > 1) THEN
        !Get default vNext (first vertice found that is not vPrev)
        DO i=1,nAdj
          vi=getAdjacentVert_graphType(thisGraph,vCurr,i)
          IF(vi /= vPrev) THEN
            vNext=vi
            EXIT
          ENDIF
        ENDDO

        !Search other vertices
        dcurr=(/0.0_SRK,-1.0_SRK/)
        IF(vPrev > 0) dcurr=thisGraph%vertices(:,vCurr)- &
            thisGraph%vertices(:,vPrev)
        dnext=thisGraph%vertices(:,vNext)-thisGraph%vertices(:,vCurr)
        isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) .APPROXLE. 0.0_SRK)
        DO i=1,nAdj
          vi=getAdjacentVert_graphType(thisGraph,vCurr,i)
          IF(vi /= vPrev .AND. vi /= vNext) THEN
            di=thisGraph%vertices(:,vi)-thisGraph%vertices(:,vCurr)
            IF(isVCurrConvex) THEN
              IF(dcurr(1)*di(2)-dcurr(2)*di(1) > 0.0_SRK .AND. &
                  dnext(1)*di(2)-dnext(2)*di(1) > 0.0_SRK) THEN
                vNext=vi
                dnext=di
                isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) .APPROXLE. 0.0_SRK)
              ENDIF
            ELSE
              IF(dcurr(1)*di(2)-dcurr(2)*di(1) > 0.0_SRK .OR. &
                  dnext(1)*di(2)-dnext(2)*di(1) > 0.0_SRK) THEN
                vNext=vi
                dnext=di
                isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) .APPROXLE. 0.0_SRK)
              ENDIF
            ENDIF
          ENDIF
        ENDDO !End searching over vertices
      ENDIF
    ENDIF
  ENDIF
ENDFUNCTION getCCWMostVert_graphType
!
!-------------------------------------------------------------------------------
!> @brief Determines if the graph is a minimum cycle
!> @param thisGraph the graph to interrogate
!> @returns bool logical indicating if the graph is a minimum cycle (true) or not (false)
!>
!>
ELEMENTAL FUNCTION isMinimumCycle_graphType(thisGraph) RESULT(bool)
  CLASS(GraphType),INTENT(IN) :: thisGraph
  LOGICAL(SBK) :: bool
  LOGICAL(SBK) :: isMinCyc
  INTEGER(SIK) :: i,n
  bool=.FALSE.
  n=nVert_graphType(thisGraph)
  IF(n > 2) THEN
    isMinCyc=.TRUE. !Assume true
    DO i=1,n !Verify all vertices have 2 neighbors
      IF(nAdjacent_graphType(thisGraph,i) /= 2) THEN
        isMinCyc=.FALSE.
        EXIT
      ENDIF
    ENDDO
    bool=isMinCyc
  ENDIF
ENDFUNCTION isMinimumCycle_graphType
!
!-------------------------------------------------------------------------------
!> @brief Inserts a new vertex into a graph object
!> @param thisGraph the graph to modify
!> @param coord the coordinates of the new vertex to add
!>
SUBROUTINE insertVertex_graphType(thisGraph,coord)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph
  REAL(SRK),INTENT(IN) :: coord(2)
  INTEGER(SIK) :: i,j,n,k,idx
  INTEGER(SIK),ALLOCATABLE :: tmpE(:,:)
  REAL(SRK),ALLOCATABLE :: tmpVertices(:,:),tmpQE(:,:,:)
  IF(ALLOCATED(thisGraph%vertices)) THEN
    n=SIZE(thisGraph%vertices,DIM=2)
    CALL dmallocA(tmpVertices,2,n+1)
    j=0
    k=0
    DO i=1,n
      IF(coord(1) .APPROXLE. thisGraph%vertices(1,i)) THEN
        IF(coord(1) .APPROXEQA. thisGraph%vertices(1,i)) THEN
          IF(coord(2) .APPROXEQA. thisGraph%vertices(2,i)) THEN
            idx=-1 !Duplicate vertex
            EXIT
          ELSEIF(coord(2) < thisGraph%vertices(2,i)) THEN
            idx=i !Before i
          ELSE
            !After i
            DO j=i+1,n
              !Find index for end of sequence with same x value
              IF(.NOT.(coord(1) .APPROXEQA. thisGraph%vertices(1,j))) EXIT
            ENDDO
            j=j-1

            !Search on y through sequence of same x
            DO k=i+1,j
              IF(coord(2) .APPROXEQA. thisGraph%vertices(2,k)) THEN
                idx=-1
                EXIT
              ELSEIF(coord(2) < thisGraph%vertices(2,k)) THEN
                idx=k
                EXIT
              ENDIF
            ENDDO
            IF(k == j+1) idx=k
          ENDIF
        ELSE
          idx=i !Before i
        ENDIF
        EXIT
      ENDIF
    ENDDO
    IF(j /= 0) i=j
    IF(i == n+1) idx=n+1 !Last point
    IF(idx > 0) THEN
      IF(idx > 1) tmpVertices(:,1:idx-1)=thisGraph%vertices(:,1:idx-1)
      tmpVertices(:,idx)=coord
      IF(idx <= n) tmpVertices(:,idx+1:n+1)=thisGraph%vertices(:,idx:n)
      CALL demallocA(thisGraph%vertices)
      CALL MOVE_ALLOC(tmpVertices,thisGraph%vertices)
      !Expand Edge Matrices
      CALL dmallocA(tmpE,n+1,n+1)
      CALL dmallocA(tmpQE,3,n+1,n+1)
      DO j=1,idx-1
        DO i=1,idx-1
          tmpE(i,j)=thisGraph%edgeMatrix(i,j)
          tmpE(j,i)=thisGraph%edgeMatrix(j,i)
          tmpQE(:,i,j)=thisGraph%quadEdges(:,i,j)
          tmpQE(:,j,i)=thisGraph%quadEdges(:,j,i)
        ENDDO
        DO i=idx+1,n+1
          tmpE(i,j)=thisGraph%edgeMatrix(i-1,j)
          tmpE(j,i)=thisGraph%edgeMatrix(j,i-1)
          tmpQE(:,i,j)=thisGraph%quadEdges(:,i-1,j)
          tmpQE(:,j,i)=thisGraph%quadEdges(:,j,i-1)
        ENDDO
      ENDDO
      DO j=idx+1,n+1
        DO i=1,idx-1
          tmpE(i,j)=thisGraph%edgeMatrix(i,j-1)
          tmpE(j,i)=thisGraph%edgeMatrix(j-1,i)
          tmpQE(:,i,j)=thisGraph%quadEdges(:,i,j-1)
          tmpQE(:,j,i)=thisGraph%quadEdges(:,j-1,i)
        ENDDO
        DO i=idx+1,n+1
          tmpE(i,j)=thisGraph%edgeMatrix(i-1,j-1)
          tmpE(j,i)=thisGraph%edgeMatrix(j-1,i-1)
          tmpQE(:,i,j)=thisGraph%quadEdges(:,i-1,j-1)
          tmpQE(:,j,i)=thisGraph%quadEdges(:,j-1,i-1)
        ENDDO
      ENDDO
      CALL demallocA(thisGraph%edgeMatrix)
      CALL MOVE_ALLOC(tmpE,thisGraph%edgeMatrix)
      CALL demallocA(thisGraph%quadEdges)
      CALL MOVE_ALLOC(tmpQE,thisGraph%quadEdges)
    ENDIF
  ELSE
    CALL dmallocA(thisGraph%vertices,2,1)
    thisGraph%vertices(:,1)=coord
    CALL dmallocA(thisGraph%edgeMatrix,1,1)
    CALL dmallocA(thisGraph%quadEdges,3,1,1)
  ENDIF
ENDSUBROUTINE insertVertex_graphType
!
!-------------------------------------------------------------------------------
!> @brief Defines a linear edge between 2 vertexes
!> @param thisGraph the graph to modify
!> @param coord1 the coordinates of the first vertex
!> @param coord2 the coordinates of the second vertex
!>
SUBROUTINE defineLinearEdge_graphType(thisGraph,coord1,coord2)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph
  REAL(SRK),INTENT(IN) :: coord1(2)
  REAL(SRK),INTENT(IN) :: coord2(2)
  !
  INTEGER(SIK) :: v1,v2
  TYPE(PointType) :: m

  v1=thisGraph%getVertIndex(coord1)
  v2=thisGraph%getVertIndex(coord2)
  IF(v1 > 0 .AND. v2 > 0 .AND. v1 /= v2) THEN
    IF(thisGraph%edgeMatrix(v1,v2) == -1) THEN
      CALL m%init(COORD=thisGraph%getMidPointOnEdge(v1,v2))
      CALL thisGraph%divideEdge(v1,v2,m)
    ELSE
      thisGraph%edgeMatrix(v1,v2)=1
      thisGraph%edgeMatrix(v2,v1)=1
    ENDIF
  ENDIF
ENDSUBROUTINE defineLinearEdge_graphType
!
!-------------------------------------------------------------------------------
!> @brief Defines a quadratic edge between 2 vertexes
!> @param thisGraph the graph to modify
!> @param coord1 the coordinates of the first vertex
!> @param coord2 the coordinates of the second vertex
!> @param c0 the centroid of the circle/arc
!> @param r the radius of the circle/arc
!>
SUBROUTINE defineQuadraticEdge(thisGraph,coord1,coord2,c0,r)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph
  REAL(SRK),INTENT(IN) :: coord1(2)
  REAL(SRK),INTENT(IN) :: coord2(2)
  REAL(SRK),INTENT(IN) :: c0(2)
  REAL(SRK),INTENT(IN) :: r

  INTEGER(SIK) :: v1,v2
  REAL(SRK) :: x1,y1,x2,y2,r1,r2,rsq,d
  TYPE(PointType) :: m

  !Check that coord1 and coord2 exist on circle
  IF(.NOT.(r .APPROXEQA. 0.0_SRK)) THEN
    x1=coord1(1)-c0(1)
    y1=coord1(2)-c0(2)
    r1=x1*x1+y1*y1
    x2=coord2(1)-c0(1)
    y2=coord2(2)-c0(2)
    r2=x2*x2+y2*y2
    rsq=r*r
    IF((rsq .APPROXEQA. r1) .AND. (rsq .APPROXEQA. r2)) THEN
      v1=thisGraph%getVertIndex(coord1)
      v2=thisGraph%getVertIndex(coord2)
      IF(v1 > 0 .AND. v2 > 0 .AND. v1 /= v2) THEN
        IF(thisGraph%edgeMatrix(v1,v2) == 1) THEN
          CALL m%init(COORD=thisGraph%getMidPointOnEdge(v1,v2))
          CALL thisGraph%divideEdge(v1,v2,m,r)
        ELSE
          !Update edge matrix
          thisGraph%edgeMatrix(v1,v2)=-1
          thisGraph%edgeMatrix(v2,v1)=-1

          !Store circle info in quadEdges
          thisGraph%quadEdges(1:2,v1,v2)=c0
          thisGraph%quadEdges(3,v1,v2)=ABS(r)

          !Check for semi-circle and determine which half of circle
          !connects the points
          r1=(x2-x1)
          r1=r1*r1
          r2=(y2-y1)
          r2=r2*r2
          d=SQRT(r1+r2)
          IF(d .APPROXEQA. 2.0_SRK*ABS(r)) &
              thisGraph%quadEdges(3,v1,v2)=r !sign of r indicates which half
                                            !of semi-circle, all other cases
                                            !traverse shorter arc between points
          thisGraph%quadEdges(:,v2,v1)=thisGraph%quadEdges(:,v1,v2)
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDSUBROUTINE defineQuadraticEdge
!
!-------------------------------------------------------------------------------
!> @brief Returns the midpoint of an edge
!> @param thisGraph the graph to interrogate
!> @param v1 the index of the edge's first endpoint
!> @param v2 the index of the edge's second endpoint
!> @returns m the midpoint
!>
FUNCTION getMidPointOnEdge(thisGraph,v1,v2) RESULT(m)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: v1
  INTEGER(SIK),INTENT(IN) :: v2
  REAL(SRK) :: m(2)

  INTEGER(SIK) :: n
  REAL(SRK) :: a(2),b(2),c(2),r,alp1,alp2,theta,scal

  REQUIRE(v1 > 0)
  REQUIRE(v2 > 0)
  REQUIRE(v1 <= thisGraph%nVert())
  REQUIRE(v2 <= thisGraph%nVert())

  m=-HUGE(m)
  n=thisGraph%nVert()+1
  IF(v1 > 0 .AND. v2 > 0 .AND. v1 < n .AND. v2 < n) THEN
    IF(thisGraph%edgeMatrix(v1,v2) == 1) THEN
      m=thisGraph%vertices(:,v1)+thisGraph%vertices(:,v2)
      m=0.5_SRK*m
    ELSEIF(thisGraph%edgeMatrix(v1,v2) == -1) THEN
      a=thisGraph%vertices(:,v1)
      b=thisGraph%vertices(:,v2)
      c=thisGraph%quadEdges(1:2,v1,v2)
      r=thisGraph%quadEdges(3,v1,v2)
      m=a+b-2.0_SRK*c
      scal=SQRT(m(1)*m(1)+m(2)*m(2))
      IF(scal .APPROXEQA. 0.0_SRK) THEN
        !Half circle. Lame. Find theta of midpoint
        alp1=ATAN2PI(a(1)-c(1),a(2)-c(2))
        alp2=ATAN2PI(b(1)-c(1),b(2)-c(2))
        theta=0.5_SRK*(alp1+alp2)

        !Adjust theta for the appropriate half of the circle
        IF(alp2 > alp1) theta=theta-PI
        IF(r < 0.0_SRK) theta=theta-PI

        m(1)=ABS(r)*COS(theta)
        m(2)=ABS(r)*SIN(theta)
      ELSE
        scal=ABS(r)/scal
        m=m*scal
      ENDIF
      m=m+c
    ENDIF
  ENDIF

ENDFUNCTION getMidPointOnEdge
!
!-------------------------------------------------------------------------------
!> @brief Removes a vertex from a graph object
!> @param thisGraph the graph to modify
!> @param v the coordinates of the vertex to remove
!>
SUBROUTINE removeVertex_graphType(thisGraph,v)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph
  REAL(SRK),INTENT(IN) :: v(2)
  INTEGER(SIK) :: i
  i=thisGraph%getVertIndex(v)
  CALL removeVertex_idx_graphType(thisGraph,i)
ENDSUBROUTINE removeVertex_graphType
!
!-------------------------------------------------------------------------------
!> @brief Removes a vertex from a graph object
!> @param thisGraph the graph to modify
!> @param idx the index of the vertex to remove
!>
SUBROUTINE removeVertex_idx_graphType(thisGraph,idx)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: idx
  INTEGER(SIK) :: i,j,n
  INTEGER(SIK),ALLOCATABLE :: tmpEdge(:,:)
  REAL(SRK),ALLOCATABLE :: tmpVert(:,:),tmpQE(:,:,:)

  n=nVert_graphType(thisGraph)
  IF(0 < idx .AND. idx <= n) THEN
    CALL dmallocA(tmpVert,2,n-1)
    CALL dmallocA(tmpEdge,n-1,n-1)
    CALL dmallocA(tmpQE,3,n-1,n-1)
    DO i=1,idx-1
      tmpVert(:,i)=thisGraph%vertices(:,i)
      DO j=1,idx-1
        tmpEdge(j,i)=thisGraph%edgeMatrix(j,i)
        tmpQE(:,j,i)=thisGraph%quadEdges(:,j,i)
      ENDDO
      DO j=idx+1,n
        tmpEdge(j-1,i)=thisGraph%edgeMatrix(j,i)
        tmpQE(:,j-1,i)=thisGraph%quadEdges(:,j,i)
      ENDDO
    ENDDO
    DO i=idx+1,n
      tmpVert(:,i-1)=thisGraph%vertices(:,i)
      DO j=1,idx-1
        tmpEdge(j,i-1)=thisGraph%edgeMatrix(j,i)
        tmpQE(:,j,i-1)=thisGraph%quadEdges(:,j,i)
      ENDDO
      DO j=idx+1,n
        tmpEdge(j-1,i-1)=thisGraph%edgeMatrix(j,i)
        tmpQE(:,j-1,i-1)=thisGraph%quadEdges(:,j,i)
      ENDDO
    ENDDO
    CALL clear_graphType(thisGraph)
    IF(ALLOCATED(tmpVert)) &
        CALL MOVE_ALLOC(tmpVert,thisGraph%vertices)
    IF(ALLOCATED(tmpEdge)) &
        CALL MOVE_ALLOC(tmpEdge,thisGraph%edgeMatrix)
    IF(ALLOCATED(tmpQE)) &
        CALL MOVE_ALLOC(tmpQE,thisGraph%quadEdges)
  ENDIF
ENDSUBROUTINE removeVertex_idx_graphType
!
!-------------------------------------------------------------------------------
!> @brief Removes an edge from a graph object
!> @param thisGraph the graph to modify
!> @param c1 the coordinates of the first edge vertex
!> @param c2 the coordinates of the second edge vertex
!>
SUBROUTINE removeEdge_graphType(thisGraph,c1,c2)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph
  REAL(SRK),INTENT(IN) :: c1(2)
  REAL(SRK),INTENT(IN) :: c2(2)
  INTEGER(SIK) :: v1,v2

  v1=thisGraph%getVertIndex(c1)
  v2=thisGraph%getVertIndex(c2)
  IF(v1 > 0 .AND. v2 > 0) THEN
    thisGraph%edgeMatrix(v1,v2)=0
    thisGraph%edgeMatrix(v2,v1)=0
    thisGraph%quadEdges(:,v1,v2)=0.0_SRK
    thisGraph%quadEdges(:,v2,v1)=0.0_SRK
  ENDIF
ENDSUBROUTINE removeEdge_graphType

!
!-------------------------------------------------------------------------------
!> @brief Removes an edge from a graph object
!> @param thisGraph the graph to modify
!> @param i the index of the first edge vertex
!> @param j the index of the second edge vertex
!>
ELEMENTAL SUBROUTINE removeEdge_IJ_graphType(thisGraph,i,j)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK),INTENT(IN) :: j
  INTEGER(SIK) :: n

  n=nVert_graphType(thisGraph)+1
  IF(i > 0 .AND. j > 0 .AND. i < n .AND. j < n) THEN
    thisGraph%edgeMatrix(i,j)=0
    thisGraph%edgeMatrix(j,i)=0
    thisGraph%quadEdges(:,i,j)=0.0_SRK
    thisGraph%quadEdges(:,j,i)=0.0_SRK
  ENDIF
ENDSUBROUTINE removeEdge_IJ_graphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
SUBROUTINE removeFilament_vertIdx_graphType(thisGraph,i0,i1)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: i0
  INTEGER(SIK),INTENT(IN) :: i1

  LOGICAL(SBK) :: isCycleEdge
  INTEGER(SIK) :: n,nAdj,v0,v1
  REAL(SRK) :: xy(2)

  n=nVert_graphType(thisGraph)
  IF(0 < i0 .AND. i0 <= n .AND. 0 < i1 .AND. i1 <= n) THEN
    v0=i0
    v1=i1
    isCycleEdge=.FALSE.
    IF(ALLOCATED(thisGraph%isCycleEdge)) THEN
      isCycleEdge=thisGraph%isCycleEdge(v0,v1)
    ENDIF
    IF(isCycleEdge) THEN
      nAdj=nAdjacent_graphType(thisGraph,v0)
      IF(nAdj > 2) THEN
        CALL removeEdge_IJ_graphType(thisGraph,v0,v1)
        v0=v1
        nAdj=nAdjacent_graphType(thisGraph,v0)
        IF(nAdj == 1) v1=getAdjacentVert_graphType(thisGraph,v0,1)
      ENDIF
      DO WHILE(nAdj == 1)
        v1=getAdjacentVert_graphType(thisGraph,v0,1)
        IF(thisGraph%isCycleEdge(v0,v1)) THEN
          xy=thisGraph%vertices(:,v1)
          CALL removeVertex_idx_graphType(thisGraph,v0)
          v0=thisGraph%getVertIndex(xy)
          nAdj=nAdjacent_graphType(thisGraph,v0)
        ELSE
          EXIT
        ENDIF
      ENDDO
      nAdj=nAdjacent_graphType(thisGraph,v0)
      IF(nAdj == 0) CALL removeVertex_idx_graphType(thisGraph,v0)
    ELSE
      nAdj=nAdjacent_graphType(thisGraph,v0)
      IF(nAdj > 2 .AND. v1 /= v0) THEN
        CALL removeEdge_IJ_graphType(thisGraph,v0,v1)
        v0=v1
        nAdj=nAdjacent_graphType(thisGraph,v0)
        IF(nAdj == 1) v1=getAdjacentVert_graphType(thisGraph,v0,1)
      ENDIF
      DO WHILE(nAdj == 1)
        v1=getAdjacentVert_graphType(thisGraph,v0,1)
        xy=thisGraph%vertices(:,v1)
        CALL removeVertex_idx_graphType(thisGraph,v0)
        v0=thisGraph%getVertIndex(xy)
        nAdj=nAdjacent_graphType(thisGraph,v0)
      ENDDO
      IF(nAdj == 0) CALL removeVertex_idx_graphType(thisGraph,v0)
    ENDIF
  ENDIF
ENDSUBROUTINE removeFilament_vertIdx_graphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
SUBROUTINE extractPrimitive_graphType(thisGraph,v0,subgraph)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK) :: v0
  CLASS(GraphType),INTENT(INOUT) :: subgraph

  LOGICAL(SBK),ALLOCATABLE :: visited(:)
  INTEGER(SIK) :: i,j,n,nAdj,vCurr,vNext,vPrev
  REAL(SRK) :: coord1(2),coord2(2),c0(2),r

  CALL clear_graphType(subGraph)
  n=nVert_graphType(thisGraph)
  ALLOCATE(visited(0:n)); visited=.FALSE.
  vNext=getCWMostVert_graphType(thisGraph,0,v0)
  vPrev=v0
  vCurr=vNext

  coord1=thisGraph%vertices(:,vPrev)
  CALL insertVertex_graphType(subgraph,coord1)
  DO WHILE(vCurr /= 0 .AND. vCurr /= v0 .AND. .NOT.visited(vCurr))
    coord1=thisGraph%vertices(:,vPrev)
    coord2=thisGraph%vertices(:,vCurr)
    CALL insertVertex_graphType(subgraph,coord2)
    IF(thisGraph%edgeMatrix(vPrev,vCurr) == 1) THEN
      CALL subgraph%defineLinearEdge(coord1,coord2)
    ELSEIF(thisGraph%edgeMatrix(vPrev,vCurr) == -1) THEN
      c0=thisGraph%quadEdges(1:2,vPrev,vCurr)
      r=thisGraph%quadEdges(3,vPrev,vCurr)
      CALL subgraph%defineQuadraticEdge(coord1,coord2,c0,r)
    ENDIF
    visited(vCurr)=.TRUE.
    vNext=getCCWMostVert_graphType(thisGraph,vPrev,vCurr)
    vPrev=vCurr
    vCurr=vNext
  ENDDO
  IF(vCurr == 0) THEN
    !Found a filament, not necessarily rooted at vPrev
    vCurr=getAdjacentVert_graphType(thisGraph,vPrev,1)
    CALL removeFilament_vertIdx_graphType(thisGraph,vPrev,vCurr)
  ELSEIF(vCurr == v0) THEN
    !Minimum Cycle Found!

    !Add Last/First point and last edge
    coord1=thisGraph%vertices(:,vPrev)
    coord2=thisGraph%vertices(:,vCurr)
    IF(thisGraph%edgeMatrix(vPrev,vCurr) == 1) THEN
      CALL subgraph%defineLinearEdge(coord1,coord2)
    ELSEIF(thisGraph%edgeMatrix(vPrev,vCurr) == -1) THEN
      c0=thisGraph%quadEdges(1:2,vPrev,vCurr)
      r=thisGraph%quadEdges(3,vPrev,vCurr)
      CALL subgraph%defineQuadraticEdge(coord1,coord2,c0,r)
    ENDIF

    n=nVert_graphType(subGraph)
    DO i=1,n
      j=i+1
      IF(i == n) j=1
      vCurr=thisGraph%getVertIndex(subgraph%vertices(:,i))
      vNext=thisGraph%getVertIndex(subgraph%vertices(:,j))
      thisGraph%isCycleEdge(vCurr,vNext)=.TRUE.
    ENDDO
    vCurr=v0
    vNext=getCWMostVert_graphType(thisGraph,0,v0)
    CALL removeEdge_IJ_graphType(thisGraph,vCurr,vNext)
    nAdj=nAdjacent_graphType(thisGraph,vCurr)
    IF(nAdj == 1) THEN
      CALL removeFilament_vertIdx_graphType(thisGraph,vCurr, &
          getAdjacentVert_graphType(thisGraph,vCurr,1))
    ENDIF
    nAdj=nAdjacent_graphType(thisGraph,vNext)
    IF(nAdj == 1) THEN
      CALL removeFilament_vertIdx_graphType(thisGraph,vNext, &
          getAdjacentVert_graphType(thisGraph,vNext,1))
    ENDIF
  ELSE
    !vCurr was visited earlier, but we do not have a minimal cycle
    !This  implies v0 is part of a filament. Therefore we locate
    !a direction away from the initial vNext.
    vCurr=v0
    vNext=getCWMostVert_graphType(thisGraph,0,v0)
    nAdj=nAdjacent_graphType(thisGraph,vCurr)
    DO WHILE(nAdj == 2)
      IF(getAdjacentVert_graphType(thisGraph,vCurr,1) /= vNext) THEN
        vNext=vCurr
        vCurr=getAdjacentVert_graphType(thisGraph,vCurr,1)
      ELSE
        vNext=vCurr
        vCurr=getAdjacentVert_graphType(thisGraph,vCurr,2)
      ENDIF
      nAdj=nAdjacent_graphType(thisGraph,vCurr)
    ENDDO
    CALL removeFilament_vertIdx_graphType(thisGraph,vCurr,vNext)
  ENDIF
ENDSUBROUTINE extractPrimitive_graphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
SUBROUTINE getMCB_graphType(thisGraph,cycles)
  CLASS(GraphType),INTENT(IN) :: thisGraph
  TYPE(GraphType),ALLOCATABLE :: cycles(:)
  INTEGER(SIK) :: i,n,nadj,ncycles
  TYPE(GraphType) :: g,primeGraph
  TYPE(GraphType),ALLOCATABLE :: tmpCycles(:)

  IF(ALLOCATED(cycles)) THEN
    DO i=1,SIZE(cycles)
      CALL cycles(i)%clear()
    ENDDO
    DEALLOCATE(cycles)
  ENDIF
  SELECTTYPE(thisGraph); TYPE IS(GraphType)
    g=thisGraph
  ENDSELECT
  n=thisGraph%nVert()
  ALLOCATE(g%isCycleEdge(n,n))
  g%isCycleEdge=.FALSE.
  ncycles=0
  DO WHILE(g%nVert() > 0)
    nadj=g%nAdjacent(1)
    IF(nadj == 0) THEN
      CALL g%removeVertex(1)
    ELSEIF(nadj == 1) THEN
      CALL g%removeFilamentFromVert(1,1)
    ELSE
      CALL extractPrimitive_graphType(g,1,primeGraph)
      IF(primeGraph%isMinimumCycle()) THEN
        !Found minimum cycle, so add it to basis
        ncycles=ncycles+1
        ALLOCATE(tmpCycles(ncycles))
        DO i=1,ncycles-1
          tmpCycles(i)=cycles(i)
          CALL cycles(i)%clear()
        ENDDO
        tmpCycles(ncycles)=primeGraph
        IF(ALLOCATED(cycles)) DEALLOCATE(cycles)
        CALL MOVE_ALLOC(tmpCycles,cycles)
        g%isCycleEdge=.FALSE.
      ENDIF
    ENDIF
  ENDDO
  DEALLOCATE(g%isCycleEdge)
ENDSUBROUTINE getMCB_graphType
!
!-------------------------------------------------------------------------------
!> @brief Edits a graph object to a VTK file
!> @param thisGraph the graph object to edit
!> @param fname the file name to write to
!> @param unitNo the unit number to use; optional
!>
SUBROUTINE editToVTK_graphType(thisGraph,fname,unitNo)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph
  CHARACTER(LEN=*),INTENT(IN) :: fname
  INTEGER(SIK),INTENT(IN),OPTIONAL :: unitNo

  INTEGER(SIK) :: i,j,nvert,nedge,n
  TYPE(VTKMeshType) :: vtkMesh
  TYPE(VTKLegFileType) :: vtkFile

  nvert=nVert_graphType(thisGraph)
  IF(nvert > 0) THEN
    vtkMesh%meshType=VTK_UNSTRUCTURED_GRID
    vtkMesh%dims=nvert
    vtkMesh%numPoints=nvert
    ALLOCATE(vtkMesh%x(nvert))
    ALLOCATE(vtkMesh%y(nvert))
    ALLOCATE(vtkMesh%z(nvert))
    DO i=1,nvert
      vtkMesh%x(i)=thisGraph%vertices(1,i)
      vtkMesh%y(i)=thisGraph%vertices(2,i)
      vtkMesh%z(i)=0.0_SRK
    ENDDO
    nedge=nEdge_graphType(thisGraph)
    !Set up cell list (edges only isoloated verteces are ommitted)
    vtkMesh%numCells=nedge
    ALLOCATE(vtkMesh%cellList(vtkMesh%numCells))
    DO i=1,vtkMesh%numCells
      vtkMesh%cellList(i)=VTK_LINE
    ENDDO

    !Set up node list
    IF(nedge > 0) THEN
      n=0
      ALLOCATE(vtkMesh%nodelist(2*nedge))
      DO i=1,nvert
        DO j=i+1,nvert
          IF(ABS(thisGraph%edgeMatrix(i,j)) == 1) THEN
            n=n+1
            vtkMesh%nodelist(n)=i-1
            n=n+1
            vtkMesh%nodelist(n)=j-1
          ENDIF
        ENDDO
      ENDDO
      vtkMesh%isInit=.TRUE.

      !Write data to file
      CALL vtkFile%initialize(UNIT=unitNo,FILE=TRIM(fname))
      CALL vtkFile%writeMesh(vtkMesh)
    ENDIF

    !Clear local objects
    CALL vtkFile%clear()
    CALL vtkMesh%clear()
  ENDIF
ENDSUBROUTINE editToVTK_graphType
!
!-------------------------------------------------------------------------------
!> @brief Clears a graph object
!> @param thisGraph the graph object to clear
!>
!>
!>
SUBROUTINE clear_graphType(thisGraph)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph
  CALL demallocA(thisGraph%vertices)
  CALL demallocA(thisGraph%edgeMatrix)
  CALL demallocA(thisGraph%quadEdges)
ENDSUBROUTINE clear_graphType
!
!-------------------------------------------------------------------------------
!> @brief Inserts all vertexes and edges from one graph into another
!> @param thisGraph the target graph to be modified
!> @param g the source graph whose pionts are being inserted
!>
SUBROUTINE combine_GraphType(thisGraph,g)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph
  TYPE(GraphType),INTENT(IN) :: g
  TYPE(GraphType) :: target,source,source_edge
  INTEGER(SIK) :: i,line_n,source_n,target_n,nAdj,target_edge_i1,target_edge_i2
  INTEGER(SIK) :: target_i1,target_i2,source_i1,source_i2,source_edge_i1,source_edge_i2
  INTEGER(SIK),ALLOCATABLE :: cwVerts(:)
  REAL(SRK) :: alp1,alp2,theta,theta_shift,r,x1,y1,r2
  REAL(SRK) :: source_v1(2),source_v2(2),target_v1(2),target_v2(2)
  REAL(SRK),ALLOCATABLE :: vTheta(:)
  TYPE(PointType) :: centroid,intersection1,intersection2
  TYPE(LineType) :: source_line,target_line
  TYPE(CircleType) :: source_arc,target_arc

  SELECTTYPE(thisGraph); TYPE IS(GraphType)
    target=thisGraph
  ENDSELECT
  source=g
  CALL source_line%p1%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
  CALL source_line%p2%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
  CALL target_line%p1%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
  CALL target_line%p2%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
  CALL centroid%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
! WRITE(*,*)
! DO i=1,source%nVert()
!   WRITE(*,*) 'Z:',i,':',source%vertices(:,i),':',source%edgeMatrix(:,i),':',source%quadEdges(:,:,i)
! ENDDO

  !We will combine the graphs by analyzing one source edge at a time
  DO WHILE(source%nEdge() > 0)
! WRITE(*,*)
! DO i=1,target%nVert()
!   WRITE(*,*) 'X:',i,':',target%vertices(:,i),':',target%edgeMatrix(:,i),':',target%quadEdges(:,:,i)
! ENDDO
    !Find the first remaining edge in the source graph and store the vertex indexes
    source_n=source%nVert()
    outer: DO source_i1=1,source_n
      DO source_i2=source_i1+1,source_n
        IF(source%edgeMatrix(source_i2,source_i1) /= 0) THEN
          EXIT outer
        ENDIF
      ENDDO
    ENDDO outer
    IF(source_i1 > source_n .OR. source_i2 > source_n) THEN
      source_i1=0
      source_i2=0
    ENDIF

    !This just means we found an edge in the source graph
    IF(source_i1 > 0 .AND. source_i2 > 0) THEN
      !Construct a small graph source_edge that just holds the line from source_i1 to source_i2
      !After we loop over all the edges of the target graph, we'll add the vertexes and
      !edges of source_edge to the target graph
      source_v1=source%vertices(:,source_i1)
      source_v2=source%vertices(:,source_i2)
      CALL source_edge%insertVertex(source_v1)
      CALL source_edge%insertVertex(source_v2)
      !If the source edge is quadratic, set up a circle object source_arc
      IF(source%edgeMatrix(source_i1,source_i2) == -1) THEN
        centroid%coord=source%quadEdges(1:2,source_i1,source_i2)
        r=source%quadEdges(3,source_i1,source_i2)
        alp1=ATAN2PI(source_v1(1)-centroid%coord(1),source_v1(2)-centroid%coord(2))
        alp2=ATAN2PI(source_v2(1)-centroid%coord(1),source_v2(2)-centroid%coord(2))

        !Set the source_edge edge
        CALL source_edge%defineQuadraticEdge(source_v1,source_v2,centroid%coord,r)

        !Ensure we are traversing the shorter arc on the circle
        IF(ABS(alp1-alp2) .APPROXEQA. PI) THEN
          !Semi-circle, for this case we must look at sign of r
          IF(r < 0.0_SRK) THEN
            CALL source_arc%set(centroid,ABS(r),alp1,alp2)
          ELSE
            CALL source_arc%set(centroid,ABS(r),alp2,alp1)
          ENDIF
        ELSE
          !Distance between alpha_1 and alpha_2 is < PI
          IF(source_v1(2) .APPROXEQA. centroid%coord(2)) THEN
            IF(source_v2(2) > source_v1(2)) THEN
              CALL source_arc%set(centroid,ABS(r),alp2,alp1)
            ELSE
              CALL source_arc%set(centroid,ABS(r),alp1,alp2)
            ENDIF
          ELSEIF(source_v1(2) < centroid%coord(2)) THEN
            CALL source_arc%set(centroid,ABS(r),alp1,alp2)
          ELSE
            CALL source_arc%set(centroid,ABS(r),alp2,alp1)
          ENDIF
        ENDIF

        !Compute theta shift so that thetastp > thetastt when crossing x+ axis
        theta_shift=0.0_SRK
        IF(source_arc%thetastt > source_arc%thetastp) theta_shift=TWOPI
      !If the source edge is linear, set up a line object
      ELSE
        source_line%p1%coord=source_v1
        source_line%p2%coord=source_v2
        !set the source_edge edge
        CALL source_edge%defineLinearEdge(source_v1,source_v2)
      ENDIF

      !Now loop over all edges in the target graph
      target_n=thisGraph%nVert()
      DO target_i1=1,target_n
        DO target_i2=target_i1+1,target_n
          IF(thisGraph%edgeMatrix(target_i2,target_i1) /= 0) THEN
            target_v1=thisGraph%vertices(:,target_i1)
            target_v2=thisGraph%vertices(:,target_i2)
          ENDIF
          !Linear edge on the target graph
          IF(thisGraph%edgeMatrix(target_i2,target_i1) == 1) THEN
            target_line%p1%dim=2; target_line%p2%dim=2
            target_line%p1%coord=target_v1
            target_line%p2%coord=target_v2
            !This is a line-line intersection
            IF(source_arc%r == 0.0_SRK) THEN
              intersection1=source_line%intersectLine(target_line)
              !An intersection was found, so we need to replace the current
              !edge with 2 new edges including the midpoint, then add the midpoint
              !to the source edge that will get added at the end
              IF(intersection1%dim == 2) THEN
                target_edge_i1=target%getVertIndex(target_v1)
                target_edge_i2=target%getVertIndex(target_v2)
! WRITE(*,*) 'Added line-line intersection1:',target_edge_i1,target_edge_i2,intersection1%coord,target%edgeMatrix
                CALL target%divideEdge(target_edge_i1,target_edge_i2,intersection1)
                source_edge_i1=source_edge%getVertIndex(source_v1)
                source_edge_i2=source_edge%getVertIndex(source_v2)
                CALL source_edge%divideEdge(source_edge_i1,source_edge_i2,intersection1)
              ENDIF
            !This is a circle-line intersection
            ELSE
              CALL source_arc%intersectLine(target_line,intersection1,intersection2)

              !Count tangent points
              IF(intersection1%dim == -3) intersection1%dim=2
              IF(intersection1%dim == 0 .AND. intersection2%dim == 2) THEN
                intersection1=intersection2
                CALL intersection2%clear()
              ENDIF

              !Check for intersections on ends of line segments
              IF(intersection1%dim == 0) THEN
                x1=target_v1(1)-source_arc%c%coord(1)
                y1=target_v1(2)-source_arc%c%coord(2)
                r2=x1*x1+y1*y1
                IF(r2 .APPROXEQA. source_arc%r*source_arc%r) THEN
                  intersection1=target_line%p1
                  target_line%p1%dim=0
                ELSE
                  x1=target_v2(1)-source_arc%c%coord(1)
                  y1=target_v2(2)-source_arc%c%coord(2)
                  r2=x1*x1+y1*y1
                  IF(r2 .APPROXEQA. source_arc%r*source_arc%r) THEN
                    intersection1=target_line%p2
                    target_line%p2%dim=0
                  ENDIF
                ENDIF
              ENDIF
              IF(target_line%p1%dim == 2 .AND. intersection2%dim == 0) THEN
                x1=target_v1(1)-source_arc%c%coord(1)
                y1=target_v1(2)-source_arc%c%coord(2)
                r2=x1*x1+y1*y1
                IF(r2 .APPROXEQA. source_arc%r*source_arc%r) THEN
                  intersection2=target_line%p1
                  target_line%p1%dim=0
                ENDIF
              ELSEIF(target_line%p2%dim == 2 .AND. intersection2%dim == 0) THEN
                x1=target_v2(1)-source_arc%c%coord(1)
                y1=target_v2(2)-source_arc%c%coord(2)
                r2=x1*x1+y1*y1
                IF(r2 .APPROXEQA. source_arc%r*source_arc%r) THEN
                  intersection2=target_line%p2
                  target_line%p2%dim=0
                ENDIF
              ENDIF

              !Filter points for interval of theta on arc. (might have bugs?)
              IF(intersection1%dim == 2) THEN
                theta=ATAN2PI(intersection1%coord(1)-source_arc%c%coord(1), &
                              intersection1%coord(2)-source_arc%c%coord(2))
                IF(intersection1%coord(2)-source_arc%c%coord(2) .APPROXGE. 0.0_SRK) &
                    theta=theta+theta_shift
                IF(.NOT.((source_arc%thetastt .APPROXLE. theta) .AND. &
                    (theta .APPROXLE. source_arc%thetastp+theta_shift))) CALL intersection1%clear()
              ENDIF
              IF(intersection2%dim == 2) THEN
                theta=ATAN2PI(intersection2%coord(1)-source_arc%c%coord(1), &
                              intersection2%coord(2)-source_arc%c%coord(2))
                IF(intersection2%coord(2)-source_arc%c%coord(2) .APPROXGE. 0.0_SRK) &
                    theta=theta+theta_shift
                IF(.NOT.((source_arc%thetastt .APPROXLE. theta) .AND. &
                    (theta .APPROXLE. source_arc%thetastp+theta_shift))) CALL intersection2%clear()
              ENDIF

              !The first intersection point has data, so divide based on that
              IF(intersection1%dim == 2) THEN
                target_edge_i1=target%getVertIndex(target_v1)
                target_edge_i2=target%getVertIndex(target_v2)
                CALL target%divideEdge(target_edge_i1,target_edge_i2,intersection1)
! WRITE(*,*) 'Added line-circle intersection1:',intersection1%coord,target%edgeMatrix
                source_edge_i1=source_edge%getVertIndex(source_v1)
                source_edge_i2=source_edge%getVertIndex(source_v2)
                CALL source_edge%divideEdge(source_edge_i1,source_edge_i2,intersection1,source_arc%r)
                !Update i1 and i2 to be from the point of intersection to the second point.
                !This is needed in case intersection2 also has data.  When adding a vertex,
                !there is no guarantee that the previous target_edge_i1 and target_edge_i2 are the
                !same, so we need to update those values
                IF(intersection2%dim == 2) THEN
                  target_edge_i1=target%getVertIndex(intersection1)
                  target_edge_i2=target%getVertIndex(target_v2)
                  source_edge_i1=source_edge%getVertIndex(intersection1)
                  source_edge_i2=source_edge%getVertIndex(source_v2)
                ENDIF
              !No data in intersection 1, but prepare indexes to divide the segments based
              !on intersection 2
              ELSEIF(intersection2%dim == 2) THEN
                target_edge_i1=target%getVertIndex(target_v1)
                target_edge_i2=target%getVertIndex(target_v2)
                source_edge_i1=source_edge%getVertIndex(source_v1)
                source_edge_i2=source_edge%getVertIndex(source_v2)
              ENDIF
              !The second intersection point has data, so divide
              IF(intersection2%dim == 2) THEN
                CALL target%divideEdge(target_edge_i1,target_edge_i2,intersection2)
! WRITE(*,*) 'Added line-circle intersection2:',intersection2%coord,target%edgeMatrix
                CALL source_edge%divideEdge(source_edge_i1,source_edge_i2,intersection2,source_arc%r)
              ENDIF
            ENDIF
          !Quadratic edge on the target graph
          ELSEIF(thisGraph%edgeMatrix(target_i2,target_i1) == -1) THEN
            centroid%coord=thisGraph%quadEdges(1:2,target_i1,target_i2)
            r=thisGraph%quadEdges(3,target_i1,target_i2)
            alp1=ATAN2PI(target_v1(1)-centroid%coord(1),target_v1(2)-centroid%coord(2))
            alp2=ATAN2PI(target_v2(1)-centroid%coord(1),target_v2(2)-centroid%coord(2))
            !Insure we are traversing the shorter arc on the circle
            IF(ABS(alp1-alp2) .APPROXEQA. PI) THEN
              !Semi-circle, for this case we must look at sign of r
              IF(r < 0.0_SRK) THEN
                CALL target_arc%set(centroid,ABS(r),alp1,alp2)
              ELSE
                CALL target_arc%set(centroid,ABS(r),alp2,alp1)
              ENDIF
            ELSE
              !Distance between alpha_1 and alpha_2 is < PI
              IF(target_v1(2) .APPROXEQA. centroid%coord(2)) THEN
                IF(target_v2(2) > target_v1(2)) THEN
                  CALL target_arc%set(centroid,ABS(r),alp2,alp1)
                ELSE
                  CALL target_arc%set(centroid,ABS(r),alp1,alp2)
                ENDIF
              ELSEIF(target_v1(2) < centroid%coord(2)) THEN
                CALL target_arc%set(centroid,ABS(r),alp1,alp2)
              ELSE
                CALL target_arc%set(centroid,ABS(r),alp2,alp1)
              ENDIF
            ENDIF
            !A line-circle intersection
            IF(source_arc%r == 0.0_SRK) THEN
              CALL target_arc%intersectLine(source_line,intersection1,intersection2)

              !Count tangent points
              IF(intersection1%dim == -3) intersection1%dim=2
              IF(intersection1%dim == 0 .AND. intersection2%dim == 2) THEN
                intersection1=intersection2
                CALL intersection2%clear()
              ENDIF

              !Check for intersections on ends of line segments
              IF(intersection1%dim == 0) THEN
                x1=source_v1(1)-target_arc%c%coord(1)
                y1=source_v1(2)-target_arc%c%coord(2)
                r2=x1*x1+y1*y1
                IF(r2 .APPROXEQA. target_arc%r*target_arc%r) THEN
                  intersection1=source_line%p1
                  source_line%p1%dim=0
                ELSE
                  x1=source_v2(1)-target_arc%c%coord(1)
                  y1=source_v2(2)-target_arc%c%coord(2)
                  r2=x1*x1+y1*y1
                  IF(r2 .APPROXEQA. target_arc%r*target_arc%r) THEN
                    intersection1=source_line%p2
                    source_line%p2%dim=0
                  ENDIF
                ENDIF
              ENDIF
              IF(source_line%p1%dim == 2 .AND. intersection2%dim == 0) THEN
                x1=source_v1(1)-target_arc%c%coord(1)
                y1=source_v1(2)-target_arc%c%coord(2)
                r2=x1*x1+y1*y1
                IF(r2 .APPROXEQA. target_arc%r*target_arc%r) THEN
                  intersection2=source_line%p1
                  source_line%p1%dim=0
                ENDIF
              ELSEIF(source_line%p2%dim == 2 .AND. intersection2%dim == 0) THEN
                x1=source_v2(1)-target_arc%c%coord(1)
                y1=source_v2(2)-target_arc%c%coord(2)
                r2=x1*x1+y1*y1
                IF(r2 .APPROXEQA. target_arc%r*target_arc%r) THEN
                  intersection2=source_line%p2
                  source_line%p2%dim=0
                ENDIF
              ENDIF

              !Filter points for interval of theta on arc. (might have bugs?)
              theta_shift=0.0_SRK
              IF(target_arc%thetastt > target_arc%thetastp) theta_shift=TWOPI
              IF(intersection1%dim == 2) THEN
                theta=ATAN2PI(intersection1%coord(1)-target_arc%c%coord(1), &
                              intersection1%coord(2)-target_arc%c%coord(2))
                IF(intersection1%coord(2)-target_arc%c%coord(2) .APPROXGE. 0.0_SRK) &
                    theta=theta+theta_shift
                IF(.NOT.((target_arc%thetastt .APPROXLE. theta) .AND. &
                    (theta .APPROXLE. target_arc%thetastp+theta_shift))) CALL intersection1%clear()
              ENDIF
              IF(intersection2%dim == 2) THEN
                theta=ATAN2PI(intersection2%coord(1)-target_arc%c%coord(1), &
                              intersection2%coord(2)-target_arc%c%coord(2))
                IF(intersection2%coord(2)-target_arc%c%coord(2) .APPROXGE. 0.0_SRK) &
                    theta=theta+theta_shift
                IF(.NOT.((target_arc%thetastt .APPROXLE. theta) .AND. &
                    (theta .APPROXLE. target_arc%thetastp+theta_shift))) CALL intersection2%clear()
              ENDIF

              !The first intersection point has data, so divide based on that
              IF(intersection1%dim == 2) THEN
                target_edge_i1=target%getVertIndex(target_v1)
                target_edge_i2=target%getVertIndex(target_v2)
                CALL target%divideEdge(target_edge_i1,target_edge_i2,intersection1,target_arc%r)
! WRITE(*,*) 'Added circle-line intersection1:',intersection1%coord,target%edgeMatrix
                source_edge_i1=source_edge%getVertIndex(source_v1)
                source_edge_i2=source_edge%getVertIndex(source_v2)
                CALL source_edge%divideEdge(source_edge_i1,source_edge_i2,intersection1)
                !Update i1 and i2 to be from the point of intersection to the second point.
                !This is needed in case intersection2 also has data.  When adding a vertex,
                !there is no guarantee that the previous target_edge_i1 and target_edge_i2 are the
                !same, so we need to update those values
                IF(intersection2%dim == 2) THEN
                  target_edge_i1=target%getVertIndex(intersection1)
                  target_edge_i2=target%getVertIndex(target_v2)
                  source_edge_i1=source_edge%getVertIndex(intersection1)
                  source_edge_i2=source_edge%getVertIndex(source_v2)
                ENDIF
              !No data in intersection 1, but prepare indexes to divide the segments based
              !on intersection 2
              ELSEIF(intersection2%dim == 2) THEN
                target_edge_i1=target%getVertIndex(target_v1)
                target_edge_i2=target%getVertIndex(target_v2)
                source_edge_i1=source_edge%getVertIndex(source_v1)
                source_edge_i2=source_edge%getVertIndex(source_v2)
              ENDIF
              !The second intersection point has data, so divide
              IF(intersection2%dim == 2) THEN
                CALL target%divideEdge(target_edge_i1,target_edge_i2,intersection2,target_arc%r)
! WRITE(*,*) 'Added circle-line intersection2:',intersection2%coord,target%edgeMatrix
                CALL source_edge%divideEdge(source_edge_i1,source_edge_i2,intersection2)
              ENDIF
            ELSE
              !circle-circle (F-this)
              !This really shouldn't be hard... as with line-circle intersection,
              !there can be either 1 or 2 points of intersection.  Once they're found,
              !the remaining logic to divide edges is the same.  We just need a
              !circle-circle intersection routine
            ENDIF
          ENDIF
        ENDDO !target_i2
      ENDDO !target_i1
      !A linear edge is being inserted
      IF(source_arc%r == 0.0_SRK) THEN
        CALL target%insertVertex(source_edge%vertices(:,1))
! WRITE(*,*) 'Added source edge vertex:',source_edge%vertices(:,1)
        DO i=2,source_edge%nVert()
          CALL target%insertVertex(source_edge%vertices(:,i))
! WRITE(*,*) 'Added source edge vertex:',source_edge%vertices(:,i)
          CALL target%defineLinearEdge(source_edge%vertices(:,i-1),source_edge%vertices(:,i))
! WRITE(*,*) 'Added source edge:',target%edgeMatrix
        ENDDO !i
      !A quadratic edge is being inserted
      ELSE
        !Sort vertices in clock-wise order.
        !^^^ But why?  Whatever order the vertexes are on the source_edge should be fine...
        line_n=source_edge%nVert()
        ALLOCATE(cwVerts(line_n)); cwVerts=0
        ALLOCATE(vTheta(line_n));
        DO i=1,line_n
          vTheta(i)=ATAN2PI(source_edge%vertices(1,i)-source_arc%c%coord(1), &
              source_edge%vertices(2,i)-source_arc%c%coord(2))
        ENDDO !i
        IF(source_arc%thetastt > source_arc%thetastp) THEN
          !This arc crosses the positive x-axis, shift some angles by 2*PI
          !so vertex theta's can be ordered sequentially.
          DO i=1,line_n
            IF((0.0_SRK .APPROXLE. vTheta(i)) .AND. vTheta(i) < source_arc%thetastt) &
                vTheta(i)=vTheta(i)+TWOPI
          ENDDO !i
        ENDIF
        DO i=1,line_n
          cwVerts(i)=MINLOC(vTheta,DIM=1)
          vTheta(cwVerts(i))=HUGE(vTheta(1))
        ENDDO !i

        !Add vertices in CW-order and define edges
        CALL target%insertVertex(source_edge%vertices(:,cwVerts(1)))
        DO i=2,source_edge%nVert()
          CALL target%insertVertex(source_edge%vertices(:,cwVerts(i)))
          CALL target%defineQuadraticEdge(source_edge%vertices(:,cwVerts(i-1)), &
              source_edge%vertices(:,cwVerts(i)),source_arc%c%coord,source_arc%r)
        ENDDO !i
        DEALLOCATE(vTheta,cwVerts)
      ENDIF
      SELECTTYPE(thisGraph); TYPE IS(GraphType)
        thisGraph=target
      ENDSELECT
      !CALL editToVTK_graphType(thisGraph,'tmpG.vtk')
      CALL source_arc%clear()
      CALL source_edge%clear()
    ENDIF

    !Remove the edge
    CALL source%removeEdge(source_i1,source_i2)
    !Remove any isoloated vertices
    nAdj=source%nAdjacent(source_i1)
    IF(nAdj == 0) THEN
      CALL source%removeVertex(source_i1)
    ENDIF
    source_i2=source%getVertIndex(source_v2)
    nAdj=source%nAdjacent(source_i2)
    IF(nAdj == 0) THEN
      CALL source%removeVertex(source_i2)
    ENDIF
  ENDDO

  CALL centroid%clear()
  CALL intersection1%clear()
  CALL intersection2%clear()
  CALL source_line%clear()
  CALL target_line%clear()
  CALL source_arc%clear()
  CALL source_edge%clear()
  CALL target%clear()
  CALL source%clear()

ENDSUBROUTINE combine_GraphType
!
!-------------------------------------------------------------------------------
!> @brief Recursively divides a linear edge between two vertexes given a midpoint
!> @param this the graph object
!> @param i1 the first vertex index
!> @param i2 the second vertex index
!> @param edge_midpoint the new midpoint to insert in the edge
!>
!> This routine inserts @c edge_midpoint as a new vertex in @c this.  It then
!> removes the edge from @c i1 to @c i2 and replaces it with two new edges:
!> one from @c i1 to @c edge_midpoint and one from @c edge_midpoint to @c i2.
!>
!> There are multiple corner cases that can occur if @c this already has a vertex
!> at the location specified by @c edge_midpoint.  First, the vertex may already
!> exist, edges to it may also exist from @c i1 and/or @c i2.  If those edges are
!> linear, then they are simply left in place.  If they are quadratic, then
!> a new midpoint is calculated between @c edge_midpoint and either @c i1 or @c i2.
!> This routine is then recursively called using that new midpoint to generate 2
!> line segments from @c i1 or @c i2 to @c edge_midpoint.  This allows the previously
!> existing quadratic edge to also remain in place.
!>
!> One assumption that's made is that @c edge_midpoint actually lies on the line
!> between @c i1 and @c i2.  If that's not actually the case, the edge will be
!> removed.  It's assumed that the client code is truly calculating a midpoint of
!> the existing edge.  If not, then this routine will likely be modifying the graph
!> more than expected.
!>
RECURSIVE SUBROUTINE divideLinearEdge(this,i1,i2,edge_midpoint)
  CLASS(GraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: i1
  INTEGER(SIK),INTENT(IN) :: i2
  TYPE(PointType),INTENT(IN) :: edge_midpoint
  !
  INTEGER(SIK) :: start_i,stop_i,new_i
  REAL(SRK) :: startpoint(2),endpoint(2)
  TYPE(PointType) :: new_midpoint

  REQUIRE(i1 > 0)
  REQUIRE(i2 > 0)
  REQUIRE(i1 <= this%nVert())
  REQUIRE(i2 <= this%nVert())
  REQUIRE(edge_midpoint%dim == 2)

  !Get the coordinates
  start_i=i1
  stop_i=i2
  startpoint=this%vertices(:,start_i)
  endpoint=this%vertices(:,stop_i)
  !Remove the old edge if it's linear
  IF(this%edgeMatrix(start_i,stop_i) == 1) THEN
    CALL this%removeEdge(start_i,stop_i)
  ENDIF
  !Add the midpoint vertex
  CALL this%insertVertex(edge_midpoint%coord)
  !Get the new vertex index so we can check that if any edges to it already existed
  start_i=this%getVertIndex(startpoint)
  stop_i=this%getVertIndex(endpoint)
  new_i=this%getVertIndex(edge_midpoint%coord)
! WRITE(*,*) 'divideLinearEdge:',start_i,stop_i,':',edge_midpoint%coord,':',new_i
! WRITE(*,*) 'lin edge check:',this%edgeMatrix(:,start_i)
! WRITE(*,*) 'lin edge check:',this%edgeMatrix(:,new_i)
! WRITE(*,*) 'lin edge check:',this%edgeMatrix(:,stop_i)

  !Check the start of the line first
  SELECTCASE(this%edgeMatrix(start_i,new_i))
  ! There's already a linear edge there, so just continue
  CASE(1)
    CONTINUE
  ! There's no edge here, so add it
  CASE(0)
    CALL this%defineLinearEdge(startpoint,edge_midpoint%coord)
  ! There's a quadratic edge, so we'll need to divide again to store both edges
  CASE(-1)
    CALL new_midpoint%init(COORD=this%getMidPointOnEdge(start_i,new_i))
    CALL this%divideEdge(start_i,new_i,new_midpoint)
    CALL new_midpoint%clear()
  ENDSELECT
  start_i=this%getVertIndex(startpoint)
  stop_i=this%getVertIndex(endpoint)

  !Now check the end of the line
  SELECTCASE(this%edgeMatrix(stop_i,new_i))
  ! There's already a linear edge there, so just continue
  CASE(1)
    CONTINUE
  ! There's no edge here, so add it
  CASE(0)
    CALL this%defineLinearEdge(edge_midpoint%coord,endpoint)
  ! There's a quadratic edge, so we'll need to divide again to store both edges
  CASE(-1)
    CALL new_midpoint%init(COORD=this%getMidPointOnEdge(new_i,stop_i))
    CALL this%divideLinearEdge(new_i,stop_i,new_midpoint)
  ENDSELECT

ENDSUBROUTINE divideLinearEdge
!
!-------------------------------------------------------------------------------
!> @brief Recursively divides a quadratic edge between two vertexes given a midpoint
!> @param this the graph object
!> @param i1 the first vertex index
!> @param i2 the second vertex index
!> @param edge_midpoint the new midpoint to insert in the edge
!>
!> This routine inserts @c edge_midpoint as a new vertex in @c this.  It then
!> removes the edge from @c i1 to @c i2 and replaces it with two new edges:
!> one from @c i1 to @c edge_midpoint and one from @c edge_midpoint to @c i2.
!>
!> There are multiple corner cases that can occur if @c this already has a vertex
!> at the location specified by @c edge_midpoint.  First, the vertex may already
!> exist, edges to it may also exist from @c i1 and/or @c i2.  If those edges are
!> quadratic with the same radius, then they are simply left in place.  If they are
!> quadratic with a different radius or if they are linear, then
!> a new midpoint is calculated between @c edge_midpoint and either @c i1 or @c i2.
!> This routine is then recursively called using that new midpoint to generate 2
!> quadratic edges from @c i1 or @c i2 to @c edge_midpoint.  This allows the previously
!> existing quadratic edge to also remain in place.
!>
!> One assumption that's made is that @c edge_midpoint actually lies on the arc
!> between @c i1 and @c i2.  If that's not actually the case, the edge will be
!> removed.  It's assumed that the client code is truly calculating a midpoint of
!> the existing arc.  If not, then this routine will likely be modifying the graph
!> more than expected.
!>
RECURSIVE SUBROUTINE divideQuadEdge(this,i1,i2,edge_midpoint,radius)
  CLASS(GraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: i1
  INTEGER(SIK),INTENT(IN) :: i2
  TYPE(PointType),INTENT(IN) :: edge_midpoint
  REAL(SRK),INTENT(IN) :: radius
  !
  INTEGER(SIK) :: start_i,stop_i,new_i
  REAL(SRK) :: startpoint(2),endpoint(2),centroid(2)
  TYPE(PointType) :: new_midpoint

  REQUIRE(i1 > 0)
  REQUIRE(i2 > 0)
  REQUIRE(i1 <= this%nVert())
  REQUIRE(i2 <= this%nVert())
  REQUIRE(edge_midpoint%dim == 2)
  REQUIRE(radius > 0.0_SRK)

  !Get the coordinates
  start_i=i1
  stop_i=i2
  startpoint=this%vertices(:,start_i)
  endpoint=this%vertices(:,stop_i)
  centroid=this%quadEdges(1:2,start_i,stop_i)
  !Remove the old edge if it's quadratic
! WRITE(*,*) 'edge check:',this%edgeMatrix(:,start_i),':',this%edgeMatrix(:,stop_i)
  IF(this%edgeMatrix(start_i,stop_i) == -1) THEN
    CALL this%removeEdge(start_i,stop_i)
  ENDIF
  ! WRITE(*,*) 'edge check:',this%edgeMatrix(:,start_i),':',this%edgeMatrix(:,stop_i)
  !Add the midpoint vertex
  CALL this%insertVertex(edge_midpoint%coord)
  !Get the new vertex index so we can check that if any edges to it already existed
  start_i=this%getVertIndex(startpoint)
  stop_i=this%getVertIndex(endpoint)
  new_i=this%getVertIndex(edge_midpoint%coord)
! WRITE(*,*) 'divideQuadEdge:',start_i,stop_i,':',edge_midpoint%coord,':',radius,':',new_i

  !Check the start of the arc first
  IF(start_i /= new_i) THEN
    SELECTCASE(this%edgeMatrix(start_i,new_i))
    ! There's already a quadratic edge here
    CASE(-1)
      !The radius is the same, so just leave this edge and continue
      IF(radius .APPROXEQA. this%quadEdges(3,start_i,new_i)) THEN
! WRITE(*,*) 'branch a1a'
        CONTINUE
      !A different radius, so divide again and keep both
      ELSE
! WRITE(*,*) 'branch a1b'
        CALL new_midpoint%init(COORD=this%getMidPointOnEdge(start_i,new_i))
        CALL this%divideQuadEdge(start_i,new_i,new_midpoint,radius)
        CALL new_midpoint%clear()
      ENDIF
    ! There's no edge here, so add it
    CASE(0)
! WRITE(*,*) 'branch a2'
      CALL this%defineQuadraticEdge(startpoint,edge_midpoint%coord,centroid,radius)
    ! There's a linear edge here, so divide again
    CASE(1)
! WRITE(*,*) 'branch a3'
      CALL new_midpoint%init(COORD=this%getMidPointOnEdge(start_i,new_i))
      CALL this%divideQuadEdge(start_i,new_i,new_midpoint,radius)
      CALL new_midpoint%clear()
    ENDSELECT
  ENDIF
  start_i=this%getVertIndex(startpoint)
  stop_i=this%getVertIndex(endpoint)
  ! WRITE(*,*) 'edge check:',this%edgeMatrix(:,start_i),':',this%edgeMatrix(:,stop_i)

  !Check the end of the arc
  IF(new_i /= stop_i) THEN
    SELECTCASE(this%edgeMatrix(new_i,stop_i))
    ! There's already a quadratic edge here
    CASE(-1)
      !The radius is the same, so just leave this edge and continue
      IF(radius .APPROXEQA. this%quadEdges(3,new_i,stop_i)) THEN
! WRITE(*,*) 'branch b1a'
        CONTINUE
      !A different radius, so divide again and keep both
      ELSE
! WRITE(*,*) 'branch b1b'
        CALL new_midpoint%init(COORD=this%getMidPointOnEdge(new_i,stop_i))
        CALL this%divideQuadEdge(new_i,stop_i,new_midpoint,radius)
      ENDIF
    ! There's no edge here, so add it
    CASE(0)
! WRITE(*,*) 'branch b2'
      CALL this%defineQuadraticEdge(edge_midpoint%coord,endpoint,centroid,radius)
    ! There's a linear edge here, so divide again
    CASE(1)
! WRITE(*,*) 'branch b3'
      CALL new_midpoint%init(COORD=this%getMidPointOnEdge(new_i,stop_i))
      CALL this%divideQuadEdge(new_i,stop_i,new_midpoint,radius)
    ENDSELECT
  ENDIF
start_i=this%getVertIndex(startpoint)
stop_i=this%getVertIndex(endpoint)
! WRITE(*,*) 'edge check:',this%edgeMatrix(:,start_i),':',this%edgeMatrix(:,stop_i)

ENDSUBROUTINE divideQuadEdge
!
!-------------------------------------------------------------------------------
!> @brief Takes cloud of points and computes Delaunay triangulation
!> @param thisGraph the graph containing the cloud of points
!>
SUBROUTINE triangulateVerts_graphType(thisGraph)
  CLASS(GraphType),INTENT(INOUT) :: thisGraph

  INTEGER(SIK) :: nVert,nTri,nEdges,i,j,k
  INTEGER(SIK),ALLOCATABLE :: v1(:),v2(:),v3(:)
  INTEGER(SIK),ALLOCATABLE :: polEdges(:,:)
  REAL(SRK) :: xMin,xMax,yMin,yMax,dx,dy,dm,xMid,yMid,superTri(2,3)
  REAL(SRK) :: x1,x2,x3,y1,y2,y3,r1,r2,r3,a,bx,by,c,rad
  REAL(SRK),ALLOCATABLE :: verts(:,:)
  LOGICAL(SBK), ALLOCATABLE :: complete(:)

  LOGICAL(SBK) :: incircum
  REAL(SRK) :: r,xc,yc

  thisGraph%edgeMatrix=0
  nVert=thisGraph%nVert()

  !Maximum of n-2 triangles...n+1 with super-triangle
  ALLOCATE(v1(6*nVert+6),v2(6*nVert+6),v3(6*nVert+6))
  v1=0
  v2=0
  v3=0
  ALLOCATE(polEdges(2,6*nVert+6))
  polEdges=0
  ALLOCATE(complete(3*nVert+2))
  complete=.FALSE.
  !Make a copy of vertices before adding in super-triangle
  ALLOCATE(verts(2,nVert))
  verts=thisGraph%vertices
  !Find the minimum and maximum x-values in the graph
  xMin=thisGraph%vertices(1,1)
  yMin=thisGraph%vertices(2,1)
  xMax=xMin
  yMax=yMin
  DO i=2,nVert
    xMin=MIN(xMin,thisGraph%vertices(1,i))
    xMax=MAX(xMax,thisGraph%vertices(1,i))
    yMin=MIN(yMin,thisGraph%vertices(2,i))
    yMax=MAX(yMax,thisGraph%vertices(2,i))
  ENDDO

  dx=xMax-xMin
  dy=yMax-yMin
  dm=MAX(dx,dy)
  xMid=(xMin+xMax)/2.0_SRK
  yMid=(yMin+yMax)/2.0_SRK

  !Add super triangle vertices(triangle contains all other points)
  superTri(:,1)=(/xMid-20.0_SRK*dm,yMid-dm/)
  superTri(:,2)=(/xMid,yMid+20.0_SRK*dm/)
  superTri(:,3)=(/xMid+20.0_SRK*dm,yMid-dm/)
  CALL thisGraph%insertVertex(superTri(:,1))
  CALL thisGraph%insertVertex(superTri(:,2))
  CALL thisGraph%insertVertex(superTri(:,3))

  v1(1)=thisGraph%getVertIndex(superTri(:,1))
  v2(1)=thisGraph%getVertIndex(superTri(:,2))
  v3(1)=thisGraph%getVertIndex(superTri(:,3))
  complete=.FALSE.
  nTri=1

  DO i=1,nVert
    nEdges=0
    j=0
    DO WHILE(j < nTri) !Loop until j>nTri
      j=j+1
      IF(.NOT. complete(j)) THEN
        !Check to see if it is inside the circumcircle
        !of triangle j
        x1=thisGraph%vertices(1,v1(j))
        x2=thisGraph%vertices(1,v2(j))
        x3=thisGraph%vertices(1,v3(j))
        y1=thisGraph%vertices(2,v1(j))
        y2=thisGraph%vertices(2,v2(j))
        y3=thisGraph%vertices(2,v3(j))

        r1=x1*x1+y1*y1
        r2=x2*x2+y2*y2
        r3=x3*x3+y3*y3

        a=x1*(y2-y3)-y1*(x2-x3)+(x2*y3-x3*y2)
        bx=r1*(y3-y2)+y1*(r2-r3)+(r3*y2-r2*y3)
        by=r1*(x2-x3)-x1*(r2-r3)+(r2*x3-r3*x2)
        c=r1*(x3*y2-x2*y3)+x1*(r2*y3-r3*y2)+y1*(r3*x2-r2*x3)

        xc=-bx/(2.0_SRK*a)
        yc=-by/(2.0_SRK*a)

        dx=x1-xc
        dy=y1-yc
        r=SQRT(dx*dx+dy*dy)

        dx=verts(1,i)-xc
        dy=verts(2,i)-yc

        rad=SQRT(dx*dx+dy*dy)
        incircum=.FALSE.
        IF(rad < r .AND. .NOT.(r .APPROXEQA. rad)) incircum=.TRUE.
        IF(verts(1,i) > xc+r) THEN
          !Sweeping left to right, this triangle
          !will never be looked at again
          complete(j)=.TRUE.
          CYCLE
        ENDIF
        IF(incircum) THEN
          polEdges(1,nEdges+1)=v1(j)
          polEdges(2,nEdges+1)=v2(j)
          polEdges(1,nEdges+2)=v2(j)
          polEdges(2,nEdges+2)=v3(j)
          polEdges(1,nEdges+3)=v3(j)
          polEdges(2,nEdges+3)=v1(j)

          v1(j)=v1(nTri)
          v2(j)=v2(nTri)
          v3(j)=v3(nTri)
          complete(j)=complete(nTri)
          nEdges=nEdges+3
          nTri=nTri-1
          j=j-1
        ENDIF
      ENDIF
    ENDDO

    !If any edges are repeated, they are not on the edge polygon
    !don't need to be considered
    DO j=1,nEdges-1
      IF(.NOT. ALL(polEdges(:,j) == 0)) THEN
        DO k=j+1,nEdges
          IF( .NOT. ALL(polEdges(:,k) == 0)) THEN
            IF(polEdges(1,j) == polEdges(2,k) &
                .AND. polEdges(2,j) == polEdges(1,k)) THEN
              polEdges(:,j)=0
              polEdges(:,k)=0
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDDO
    !Update triangle list
    DO j=1,nEdges
      IF(.NOT. ALL(polEdges(:,j) == 0)) THEN
        nTri=nTri+1
        v1(nTri)=polEdges(1,j)
        v2(nTri)=polEdges(2,j)
        v3(nTri)=thisGraph%getVertIndex(verts(:,i))
        complete(nTri)=.FALSE.
      ENDIF
    ENDDO
  ENDDO !End loop over each vertex
  !Create edges of final triangulation
  DO i=1,nTri
    CALL thisGraph%defineLinearEdge(thisGraph%vertices(:,v1(i)), &
        thisGraph%vertices(:,v2(i)))
    CALL thisGraph%defineLinearEdge(thisGraph%vertices(:,v2(i)), &
        thisGraph%vertices(:,v3(i)))
    CALL thisGraph%defineLinearEdge(thisGraph%vertices(:,v3(i)), &
        thisGraph%vertices(:,v1(i)))
  ENDDO
  !Remove superTriangle from arrays
  CALL thisGraph%removeVertex(superTri(:,1))
  CALL thisGraph%removeVertex(superTri(:,2))
  CALL thisGraph%removeVertex(superTri(:,3))
ENDSUBROUTINE triangulateVerts_graphType
!
!-------------------------------------------------------------------------------
!> @brief Checks for equality between 2 graph objects
!> @param g0 the first graph to compare
!> @param g1 the second graph to compare
!> @returns bool the result
!>
ELEMENTAL FUNCTION isequal_GraphType(g0,g1) RESULT(bool)
  TYPE(GraphType),INTENT(IN) :: g0
  TYPE(GraphType),INTENT(IN) :: g1
  LOGICAL(SBK) :: bool

  bool=.FALSE.
  IF((ALLOCATED(g0%isCycleEdge) .EQV. ALLOCATED(g1%isCycleEdge)) .AND. &
      (ALLOCATED(g0%vertices) .EQV. ALLOCATED(g1%vertices)) .AND. &
      (ALLOCATED(g0%edgeMatrix) .EQV. ALLOCATED(g1%edgeMatrix)) .AND. &
      (ALLOCATED(g0%quadEdges) .EQV. ALLOCATED(g1%quadEdges))) THEN
    IF(SIZE(g0%vertices,DIM=1) == SIZE(g1%vertices,DIM=1) .AND. &
        SIZE(g0%vertices,DIM=2) == SIZE(g1%vertices,DIM=2) .AND. &
        SIZE(g0%edgeMatrix,DIM=1) == SIZE(g1%edgeMatrix,DIM=1) .AND. &
        SIZE(g0%edgeMatrix,DIM=2) == SIZE(g1%edgeMatrix,DIM=2) .AND. &
        SIZE(g0%quadEdges,DIM=1) == SIZE(g1%quadEdges,DIM=1) .AND. &
        SIZE(g0%quadEdges,DIM=2) == SIZE(g1%quadEdges,DIM=2) .AND. &
        SIZE(g0%quadEdges,DIM=3) == SIZE(g1%quadEdges,DIM=3)) THEN
      bool=.TRUE.
      IF(ALLOCATED(g0%isCycleEdge)) THEN
        IF(SIZE(g0%isCycleEdge,DIM=1) == SIZE(g1%isCycleEdge,DIM=1) .AND. &
            SIZE(g0%isCycleEdge,DIM=2) == SIZE(g1%isCycleEdge,DIM=2)) THEN
          bool=.TRUE.
        ELSE
          bool=.FALSE.
        ENDIF
      ENDIF
      bool=bool .AND. ALL(g0%vertices .APPROXEQA. g1%vertices) .AND. &
          ALL(g0%edgeMatrix == g1%edgeMatrix) .AND. &
          ALL(g0%quadEdges .APPROXEQA. g1%quadEdges)
    ENDIF
  ENDIF
ENDFUNCTION isequal_GraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE init_DAGraphType(thisGraph,n,nodes)
  CLASS(DAGraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: n
  INTEGER(SIK),ALLOCATABLE,INTENT(IN) :: nodes(:)

  IF(.NOT. ALLOCATED(thisGraph%nodes) .AND. ALLOCATED(nodes)) THEN
    IF(n > 0) THEN
      IF(SIZE(nodes) == n) THEN
        thisGraph%n=n
        ALLOCATE(thisGraph%nodes(n))
        ALLOCATE(thisGraph%edgeMatrix(n,n))
        thisGraph%nodes=nodes
        thisGraph%edgeMatrix=0
      ENDIF
    ENDIF
  ENDIF
ENDSUBROUTINE init_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE clear_DAGraphType(thisGraph)
  CLASS(DAGraphType),INTENT(INOUT) :: thisGraph

  thisGraph%n=0
  IF(ALLOCATED(thisGraph%nodes)) DEALLOCATE(thisGraph%nodes)
  IF(ALLOCATED(thisGraph%edgeMatrix)) DEALLOCATE(thisGraph%edgeMatrix)
ENDSUBROUTINE clear_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE insertNode_DAGraphType(thisGraph,ID,ind)
  CLASS(DAGraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: ID
  INTEGER(SIK),INTENT(IN),OPTIONAL :: ind
  INTEGER(SIK) :: index
  INTEGER(SIK),ALLOCATABLE :: tmpNodes(:),tmpEdges(:,:)

  index=1
  IF(PRESENT(ind)) index=ind
  IF(ALLOCATED(thisGraph%edgeMatrix)) THEN
    IF(ALL(thisGraph%nodes /= ID)) THEN
      !Store values temporarily
      ALLOCATE(tmpNodes(thisGraph%n))
      ALLOCATE(tmpEdges(thisGraph%n,thisGraph%n))
      tmpNodes=thisGraph%nodes
      tmpEdges=thisGraph%edgeMatrix
      !Resize arrays on graph type
      DEALLOCATE(thisGraph%nodes)
      DEALLOCATE(thisGraph%edgeMatrix)
      thisGraph%n=thisGraph%n+1
      ALLOCATE(thisGraph%nodes(thisGraph%n))
      ALLOCATE(thisGraph%edgeMatrix(thisGraph%n,thisGraph%n))
      thisGraph%edgeMatrix=0
      !reassign old data and assign new node
      thisGraph%nodes(1:index-1)=tmpNodes(1:index-1)
      thisGraph%nodes(index)=ID
      thisGraph%nodes(index+1:thisGraph%n)=tmpNodes(index:)
      thisGraph%edgeMatrix(1:index-1,1:index-1)=tmpEdges(1:index-1,1:index-1)
      thisGraph%edgeMatrix(index+1:,1:index-1)=tmpEdges(index:,1:index-1)
      thisGraph%edgeMatrix(1:index-1,index+1:)=tmpEdges(1:index-1,index:)
      thisGraph%edgeMatrix(index+1:,index+1:)=tmpEdges(index:,index:)
      DEALLOCATE(tmpNodes)
      DEALLOCATE(tmpEdges)
    ENDIF
  ELSE
    ALLOCATE(thisGraph%edgeMatrix(1,1))
    ALLOCATE(thisGraph%nodes(1))
    thisGraph%edgeMatrix=0
    thisGraph%nodes=ID
    thisGraph%n=1
  ENDIF
ENDSUBROUTINE insertNode_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE removeNode_DAGraphType(thisGraph,ID,ind)
  CLASS(DAGraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK),INTENT(IN),OPTIONAL :: ID
  INTEGER(SIK),INTENT(IN),OPTIONAL :: ind
  INTEGER(SIK) :: i,index

  index=0
  IF(PRESENT(ID)) THEN
    !Find the index to remove
    DO i=1,thisGraph%n
      IF(ID == thisGraph%nodes(i)) THEN
        index=i
        EXIT
      ENDIF
    ENDDO
    !If the ID was found, remove the index.
    IF(index > 0) CALL removeNodeByIndex_DAGraphType(thisGraph,index)
  ELSEIF(PRESENT(ind)) THEN
    CALL removeNodeByIndex_DAGraphType(thisGraph,ind)
  ENDIF
ENDSUBROUTINE removeNode_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE removeNodeByIndex_DAGraphType(thisGraph,ind)
  CLASS(DAGraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: ind
  INTEGER(SIK) :: i
  INTEGER(SIK),ALLOCATABLE :: tmpN(:),tmpEdge(:,:)

  IF(ALLOCATED(thisGraph%edgeMatrix)) THEN
    IF((1 <= ind) .AND. (ind <= thisGraph%n)) THEN
      !Store values temporarily
      i=thisGraph%n
      ALLOCATE(tmpN(i))
      ALLOCATE(tmpEdge(i,i))
      tmpN=thisGraph%nodes
      tmpEdge=thisGraph%edgeMatrix
      !Resize arrays on graph type
      DEALLOCATE(thisGraph%nodes)
      DEALLOCATE(thisGraph%edgeMatrix)
      thisGraph%n=thisGraph%n-1
      IF(thisGraph%n > 0) THEN
        ALLOCATE(thisGraph%nodes(thisGraph%n))
        ALLOCATE(thisGraph%edgeMatrix(thisGraph%n,thisGraph%n))
        thisGraph%edgeMatrix=0
        !reassign old data and assign new node
        thisGraph%nodes(1:ind-1)=tmpN(1:ind-1)
        thisGraph%edgeMatrix(1:ind-1,1:ind-1)=tmpEdge(1:ind-1,1:ind-1)
        IF(ind <= thisGraph%n) THEN
          thisGraph%nodes(ind:thisGraph%n)=tmpN(ind+1:)
          thisGraph%edgeMatrix(ind:,1:ind-1)=tmpEdge(ind+1:,1:ind-1)
          thisGraph%edgeMatrix(1:ind-1,ind:)=tmpEdge(1:ind-1,ind+1:)
          thisGraph%edgeMatrix(ind:,ind:)=tmpEdge(ind+1:,ind+1:)
        ENDIF
      ENDIF
      !CALL demalloc(tmpint)
      DEALLOCATE(tmpEdge)
    ENDIF
  ENDIF
ENDSUBROUTINE removeNodeByIndex_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
FUNCTION isStartNode_DAGraphType(thisGraph,ID,ind) RESULT(bool)
  CLASS(DAGraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK),INTENT(IN),OPTIONAL :: ID
  INTEGER(SIK),INTENT(IN),OPTIONAL :: ind
  INTEGER(SIK) :: index
  LOGICAL(SBK) :: bool

  bool=.FALSE.
  IF(PRESENT(ID)) THEN
    index=thisGraph%getIndex(ID)
    IF(ALL(thisGraph%edgeMatrix(:,index) == 0)) bool=.TRUE.
  ELSEIF(PRESENT(ind)) THEN
    IF(ALL(thisGraph%edgeMatrix(:,ind) == 0)) bool=.TRUE.
  ENDIF
ENDFUNCTION isStartNode_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE getNextStartNode_DAGraphType(thisGraph,old,ID)
  CLASS(DAGraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK),INTENT(INOUT) :: old(:)
  INTEGER(SIK),INTENT(OUT) :: ID
  INTEGER(SIK) :: i

  ID=0
  DO i=1,thisGraph%n
    IF(thisGraph%isStartNode(IND=i) .AND. ALL(thisGraph%nodes(i) /= old)) THEN
      ID=thisGraph%nodes(i)
      old(i)=ID
      EXIT
    ENDIF
  ENDDO
ENDSUBROUTINE getNextStartNode_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE defineEdge_DAGraphType(thisGraph,fromID,toID)
  CLASS(DAGraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: fromID
  INTEGER(SIK),INTENT(IN) :: toID
  INTEGER(SIK) :: fromInd,toInd

  IF(ALLOCATED(thisGraph%edgeMatrix)) THEN
    fromInd=thisGraph%getIndex(fromID)
    toInd=thisGraph%getIndex(toID)
    IF((fromInd > 0) .AND. (toInd > 0)) thisGraph%edgeMatrix(fromInd,toInd)=1
  ENDIF
ENDSUBROUTINE defineEdge_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE removeEdge_DAGraphType(thisGraph,fromID,toID)
  CLASS(DAGraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: fromID
  INTEGER(SIK),INTENT(IN) :: toID
  INTEGER(SIK) :: fromInd,toInd

  IF(ALLOCATED(thisGraph%edgeMatrix)) THEN
    fromInd=thisGraph%getIndex(fromID)
    toInd=thisGraph%getIndex(toID)
    IF((fromInd > 0) .AND. (toInd > 0)) thisGraph%edgeMatrix(fromInd,toInd)=0
  ENDIF
ENDSUBROUTINE removeEdge_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
FUNCTION getIndex_DAGraphType(thisGraph,ID) RESULT(ind)
  CLASS(DAGraphType),INTENT(INOUT) :: thisGraph
  INTEGER(SIK),INTENT(IN) :: ID
  INTEGER(SIK) :: i,ind

  ind=0
  IF(ALLOCATED(thisGraph%edgeMatrix)) THEN
    DO i=1,thisGraph%n
      IF(thisGraph%nodes(i) == ID) THEN
        ind=i
        EXIT
      ENDIF
    ENDDO
  ENDIF
ENDFUNCTION getIndex_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief Kahn's Algorithm for Topological Sorting (KATS).
!> @param
!>
SUBROUTINE KATS_DAGraphType(thisGraph)
  CLASS(DAGraphType),INTENT(INOUT) :: thisGraph
  TYPE(DAGraphType) :: sortedGraph
  INTEGER(SIK) :: i,ID
  INTEGER(SIK),ALLOCATABLE :: oldIDs(:)

  ID=0
  ALLOCATE(oldIDs(thisGraph%n))
  oldIDs=0
  CALL thisGraph%getNextStartNode(oldIDs,ID)

  DO WHILE(ID /= 0)
    CALL sortedGraph%insertNode(ID,1)
    DO i=1,thisGraph%n
      CALL thisGraph%removeEdge(ID,thisGraph%nodes(i))
    ENDDO
    CALL thisGraph%getNextStartNode(oldIDs,ID)
  ENDDO
  IF(ALL(thisGraph%edgeMatrix == 0)) THEN
    SELECTTYPE(thisGraph); TYPE IS(DAGraphType)
      thisGraph=sortedGraph
    ENDSELECT
  ENDIF
  DEALLOCATE(oldIDs)
ENDSUBROUTINE KATS_DAGraphType
!
ENDMODULE Geom_Graph
