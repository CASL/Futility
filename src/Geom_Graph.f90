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
!PUBLIC :: OPERATOR(+)

INTEGER(SIK),PARAMETER :: GRAPH_NULL_EDGE=0
INTEGER(SIK),PARAMETER :: GRAPH_LINEAR_EDGE=1
INTEGER(SIK),PARAMETER :: GRAPH_QUADRATIC_EDGE=-1

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
  !> the center of rotation and radius to define the quadratic edge.  The
  !> radius can be stored as negative to indicate which semi-circle is being
  !> stored.
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
    !> @copybrief Geom_Graph::getVertIndex_point
    !> @copydetails Geom_Graph::getVertIndex_point
    PROCEDURE,PASS,PRIVATE :: getVertIndex_point
    !> @copybrief Geom_Graph::getVertIndex_coords
    !> @copydetails Geom_Graph::getVertIndex_coords
    PROCEDURE,PASS,PRIVATE :: getVertIndex_coords
    GENERIC :: getVertIndex => getVertIndex_point,getVertIndex_coords
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
    !> @copybrief Geom_Graph::getMidPointOnEdge_idx
    !> @copydetails Geom_Graph::getMidPointOnEdge_idx
    PROCEDURE,PASS,PRIVATE :: getMidPointOnEdge_idx
    !> @copybrief Geom_Graph::getMidPointOnEdge_point
    !> @copydetails Geom_Graph::getMidPointOnEdge_point
    PROCEDURE,PASS,PRIVATE :: getMidPointOnEdge_point
    GENERIC :: getMidPointOnEdge => getMidPointOnEdge_idx,getMidPointOnEdge_point
    !> @copybrief Geom_Graph::isMinimumCycle_graphType
    !> @copydetails Geom_Graph::isMinimumCycle_graphType
    PROCEDURE,PASS :: isMinimumCycle => isMinimumCycle_graphType
    !> @copybrief Geom_Graph::insertVertex_coords
    !> @copydetails Geom_Graph::insertVertex_coords
    PROCEDURE,PASS,PRIVATE :: insertVertex_coords
    !> @copybrief Geom_Graph::insertVertex_point
    !> @copydetails Geom_Graph::insertVertex_point
    PROCEDURE,PASS,PRIVATE :: insertVertex_point
    GENERIC :: insertVertex => insertVertex_coords,insertVertex_point
    !> @copybrief Geom_Graph::defineLinearEdge_coords
    !> @copydetails Geom_Graph::defineLinearEdge_coords
    PROCEDURE,PASS,PRIVATE :: defineLinearEdge_coords
    !> @copybrief Geom_Graph::defineLinearEdge_point
    !> @copydetails Geom_Graph::defineLinearEdge_point
    PROCEDURE,PASS,PRIVATE :: defineLinearEdge_point
    !> @copybrief Geom_Graph::defineQuadraticEdge_coords
    !> @copydetails Geom_Graph::defineQuadraticEdge_coords
    PROCEDURE,PASS,PRIVATE :: defineQuadraticEdge_coords
    !> @copybrief Geom_Graph::defineQuadraticEdge_point
    !> @copydetails Geom_Graph::defineQuadraticEdge_point
    PROCEDURE,PASS,PRIVATE :: defineQuadraticEdge_point
    GENERIC :: defineEdge => defineLinearEdge_coords,defineLinearEdge_point, &
        defineQuadraticEdge_coords,defineQuadraticEdge_point
    !> @copybrief Geom_Graph::removeVertex_point
    !> @copydetails Geom_Graph::removeVertex_point
    PROCEDURE,PASS,PRIVATE :: removeVertex_point
    !> @copybrief Geom_Graph::removeVertex_coords
    !> @copydetails Geom_Graph::removeVertex_coords
    PROCEDURE,PASS,PRIVATE :: removeVertex_coords
    !> @copybrief Geom_Graph::removeVertex_index
    !> @copydetails Geom_Graph::removeVertex_index
    PROCEDURE,PASS,PRIVATE :: removeVertex_index
    GENERIC :: removeVertex => removeVertex_point,removeVertex_coords,removeVertex_index
    !> @copybrief Geom_Graph::removeEdge_point
    !> @copydetails Geom_Graph::removeEdge_point
    PROCEDURE,PASS,PRIVATE :: removeEdge_point
    !> @copybrief Geom_Graph::removeEdge_coords
    !> @copydetails Geom_Graph::removeEdge_coords
    PROCEDURE,PASS,PRIVATE :: removeEdge_coords
    !> @copybrief Geom_Graph::removeEdge_index
    !> @copydetails Geom_Graph::removeEdge_index
    PROCEDURE,PASS,PRIVATE :: removeEdge_index
    GENERIC :: removeEdge => removeEdge_point,removeEdge_coords,removeEdge_index
    !> @copybrief Geom_Graph::divideEdge_scalar
    !> @copydetails Geom_Graph::divideEdge_scalar
    PROCEDURE,PASS,PRIVATE :: divideEdge_scalar
    !> @copybrief Geom_Graph::divideEdge_array
    !> @copydetails Geom_Graph::divideEdge_array
    PROCEDURE,PASS,PRIVATE :: divideEdge_array
    GENERIC :: divideEdge => divideEdge_scalar,divideEdge_array
    !> @copybrief Geom_Graph::divideLinearEdge
    !> @copydetails Geom_Graph::divideLinearEdge
    PROCEDURE,PASS :: divideLinearEdge
    !> @copybrief Geom_Graph::divideQuadraticEdge
    !> @copydetails Geom_Graph::divideQuadraticEdge
    PROCEDURE,PASS :: divideQuadraticEdge
    !> @copybrief Geom_Graph::applyLinearSplit
    !> @copydetails Geom_Graph::applyLinearSplit
    PROCEDURE,PASS,PRIVATE :: applyLinearSplit
    !> @copybrief Geom_Graph::applyQuadraticSplit
    !> @copydetails Geom_Graph::applyQuadraticSplit
    PROCEDURE,PASS,PRIVATE :: applyQuadraticSplit
    !> @copybrief Geom_Graph::removeFilamentFromVert
    !> @copydetails Geom_Graph::removeFilamentFromVert
    PROCEDURE,PASS :: removeFilamentFromVert
    !> @copybrief Geom_Graph::extractPrimitive
    !> @copydetails Geom_Graph::extractPrimitive
    PROCEDURE,PASS :: extractPrimitive
    !> @copybrief Geom_Graph::getMCB_graphType
    !> @copydetails Geom_Graph::getMCB_graphType
    PROCEDURE,PASS :: getMCB => getMCB_graphType
    !> @copybrief Geom_Graph::editToVTK_graphType
    !> @copydetails Geom_Graph::editToVTK_graphType
    PROCEDURE,PASS :: editToVTK => editToVTK_graphType
    !> @copybrief Geom_Graph::combine_graphType
    !> @copydetails Geom_Graph::combine_graphType
    PROCEDURE,PASS :: combineGraph => combine_GraphType
    !> @copybrief Geom_Graph::triangulateVerts_graphType
    !> @copydetails Geom_Graph::triangulateVerts_graphType
    PROCEDURE,PASS :: triangulateVerts => triangulateVerts_graphType
    !> @copybrief Geom_Graph::clear_graphType
    !> @copydetails Geom_Graph::clear_graphType
    PROCEDURE,PASS :: clear => clear_graphType
ENDTYPE GraphType

INTERFACE OPERATOR(==)
  !> @copybrief Geom_Graph::isequal_graphType
  !> @copydetails Geom_Graph::isequal_graphType
  MODULE PROCEDURE isequal_graphType
ENDINTERFACE

!INTERFACE OPERATOR(+)
!  !> @copybrief Geom_Graph::add_graphType
!  !> @copydetails Geom_Graph::add_graphType
!  MODULE PROCEDURE add_graphType
!ENDINTERFACE
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Returns the number of vertexes in a graph object
!> @param this the graph to interrogate
!> @returns n the number of vertexes
!>
ELEMENTAL FUNCTION nVert_graphType(this) RESULT(n)
  CLASS(GraphType),INTENT(IN) :: this
  INTEGER(SIK) :: n
  n=0
  IF(ALLOCATED(this%vertices)) n=SIZE(this%vertices,DIM=2)
ENDFUNCTION nVert_graphType
!
!-------------------------------------------------------------------------------
!> @brief returnes the number of edges in a graph object
!> @param this the graph to interrogate
!> @returns n the number of edges
!>
ELEMENTAL FUNCTION nEdge_graphType(this) RESULT(n)
  CLASS(GraphType),INTENT(IN) :: this
  INTEGER(SIK) :: n
  n=0
  IF(ALLOCATED(this%edgeMatrix)) n=SUM(ABS(this%edgeMatrix))/2
ENDFUNCTION nEdge_graphType
!
!-------------------------------------------------------------------------------
!> @brief Returns the index of a vertex in a graph given the coordinates
!> @param this the graph to interrogate
!> @param point the point containing the coordinates of the vertex
!> @returns idx the index of the vertex
!>
!> If the vertex could not be found, -1 is returned
!>
FUNCTION getVertIndex_point(this,point) RESULT(idx)
  CLASS(GraphType),INTENT(IN) :: this
  TYPE(PointType),INTENT(IN) :: point
  INTEGER(SIK) :: idx

  idx=this%getVertIndex(point%coord)

ENDFUNCTION getVertIndex_point
!
!-------------------------------------------------------------------------------
!> @brief Returns the index of a vertex in a graph given the coordinates
!> @param this the graph to interrogate
!> @param coord the coordinates of the vertex
!> @returns idx the index of the vertex
!>
!> If the vertex could not be found, -1 is returned
!>
FUNCTION getVertIndex_coords(this,coord) RESULT(idx)
  CLASS(GraphType),INTENT(IN) :: this
  REAL(SRK),INTENT(IN) :: coord(2)
  INTEGER(SIK) :: idx
  INTEGER(SIK) :: i,j,n
  idx=-1
  n=this%nVert()
  DO i=1,n
    IF(coord(1) .APPROXEQA. this%vertices(1,i)) THEN
      IF(coord(2) .APPROXEQA. this%vertices(2,i)) THEN
        idx=i
      ELSE
        DO j=i+1,n
          IF(.NOT.(coord(1) .APPROXEQA. this%vertices(1,j))) EXIT
          IF(coord(2) .APPROXEQA. this%vertices(2,j)) THEN
            idx=j
            EXIT
          ENDIF
        ENDDO
      ENDIF
      EXIT
    ENDIF
  ENDDO
ENDFUNCTION getVertIndex_coords
!
!-------------------------------------------------------------------------------
!> @brief Returns the number of points connected to a specified point
!> @param this the graph to interrogate
!> @param i the index of the vertex to interrogate
!> @returns n the number of connected points
!>
ELEMENTAL FUNCTION nAdjacent_graphType(this,i) RESULT(n)
  CLASS(GraphType),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK) :: n
  n=0
  IF(ALLOCATED(this%edgeMatrix)) THEN
    IF(0 < i .AND. i < SIZE(this%edgeMatrix,DIM=2)+1) &
        n=SUM(ABS(this%edgeMatrix(:,i)))
  ENDIF
ENDFUNCTION nAdjacent_graphType
!
!-------------------------------------------------------------------------------
!> @brief Returns a particular adjacent vertex
!> @param this the graph object to interrogate
!> @param v0 the index of the vertex to interrogate
!> @param i which adjacent vertex to return (from 1 to @c this%nAdjacent(v0))
!> @param v1 the graph vertex index for the @c i-th adjacent vertex to @c v0
!>
ELEMENTAL FUNCTION getAdjacentVert_graphType(this,v0,i) RESULT(v1)
  CLASS(GraphType),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: v0
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK) :: v1
  INTEGER(SIK) :: j,n,nVert

  v1=0
  nVert=this%nVert()
  IF(0 < v0 .AND. v0 < nVert+1) THEN
    IF(0 < i .AND. i < SUM(ABS(this%edgeMatrix(:,v0)))+1) THEN
      n=0
      DO j=1,nVert
        IF(this%edgeMatrix(j,v0) /= 0) n=n+1
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
ELEMENTAL FUNCTION getCWMostVert_graphType(this,v0,vCurr) RESULT(vNext)
  CLASS(GraphType),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: vCurr
  INTEGER(SIK),INTENT(IN) :: v0
  LOGICAL(SBK) :: isVCurrConvex,badEdge
  INTEGER(SIK) :: vNext,vPrev,vi,i,nVert,nAdj
  REAL(SRK) :: dcurr(2),dnext(2),di(2)

  vNext=0
  nVert=this%nVert()
  vPrev=v0
  IF(vPrev == vCurr) vPrev=0
  IF(0 < vCurr .AND. vCurr <= nVert .AND. 0 <= vPrev .AND. vPrev <= nVert) THEN
    badEdge=.FALSE.
    IF(vPrev > 0) badEdge=this%edgeMatrix(vCurr,vPrev) == 0

    IF(.NOT.badEdge) THEN
      nAdj=this%nAdjacent(vCurr)
      IF(nAdj == 1) THEN
        !Shortcut for 1 adjacent vert
        vNext=this%getAdjacentVert(vCurr,1)
        IF(vNext == vPrev) vNext=0
      ELSEIF(nAdj > 1) THEN
        !Get default vNext (first vertice found that is not vPrev)
        DO i=1,nAdj
          vi=this%getAdjacentVert(vCurr,i)
          IF(vi /= vPrev) THEN
            vNext=vi
            EXIT
          ENDIF
        ENDDO

        !Search other vertices
        dcurr=(/0.0_SRK,-1.0_SRK/)
        IF(vPrev > 0) dcurr=this%vertices(:,vCurr)- &
            this%vertices(:,vPrev)
        dnext=this%vertices(:,vNext)-this%vertices(:,vCurr)
        isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) .APPROXLE. 0.0_SRK)
        DO i=1,nAdj
          vi=this%getAdjacentVert(vCurr,i)
          IF(vi /= vPrev .AND. vi /= vNext) THEN
            di=this%vertices(:,vi)-this%vertices(:,vCurr)
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
ELEMENTAL FUNCTION getCCWMostVert_graphType(this,v0,vCurr) RESULT(vNext)
  CLASS(GraphType),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN) :: vCurr
  INTEGER(SIK),INTENT(IN) :: v0
  LOGICAL(SBK) :: isVCurrConvex,badEdge
  INTEGER(SIK) :: vNext,vPrev,vi,i,nVert,nAdj
  REAL(SRK) :: dcurr(2),dnext(2),di(2)

  vNext=0
  nVert=this%nVert()
  vPrev=v0
  IF(vPrev == vCurr) vPrev=0
  IF(0 < vCurr .AND. vCurr <= nVert .AND. 0 <= vPrev .AND. vPrev <= nVert) THEN
    badEdge=.FALSE.
    IF(vPrev > 0) badEdge=this%edgeMatrix(vCurr,vPrev) == 0

    IF(.NOT.badEdge) THEN
      nAdj=this%nAdjacent(vCurr)
      IF(nAdj == 1) THEN
        !Shortcut for 1 adjacent vert
        vNext=this%getAdjacentVert(vCurr,1)
        IF(vNext == vPrev) vNext=0
      ELSEIF(nAdj > 1) THEN
        !Get default vNext (first vertice found that is not vPrev)
        DO i=1,nAdj
          vi=this%getAdjacentVert(vCurr,i)
          IF(vi /= vPrev) THEN
            vNext=vi
            EXIT
          ENDIF
        ENDDO

        !Search other vertices
        dcurr=(/0.0_SRK,-1.0_SRK/)
        IF(vPrev > 0) dcurr=this%vertices(:,vCurr)- &
            this%vertices(:,vPrev)
        dnext=this%vertices(:,vNext)-this%vertices(:,vCurr)
        isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) .APPROXLE. 0.0_SRK)
        DO i=1,nAdj
          vi=this%getAdjacentVert(vCurr,i)
          IF(vi /= vPrev .AND. vi /= vNext) THEN
            di=this%vertices(:,vi)-this%vertices(:,vCurr)
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
!> @param this the graph to interrogate
!> @returns bool logical indicating if the graph is a minimum cycle (true) or not (false)
!>
ELEMENTAL FUNCTION isMinimumCycle_graphType(this) RESULT(bool)
  CLASS(GraphType),INTENT(IN) :: this
  LOGICAL(SBK) :: bool
  LOGICAL(SBK) :: isMinCyc
  INTEGER(SIK) :: i,n
  bool=.FALSE.
  n=this%nVert()
  IF(n > 2) THEN
    isMinCyc=.TRUE. !Assume true
    DO i=1,n !Verify all vertices have 2 neighbors
      IF(this%nAdjacent(i) /= 2) THEN
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
!> @param this the graph to modify
!> @param point the point containing coordinates of the new vertex to add
!>
SUBROUTINE insertVertex_point(this,point)
  CLASS(GraphType),INTENT(INOUT) :: this
  TYPE(PointType),INTENT(IN) :: point

  CALL this%insertVertex(point%coord)

ENDSUBROUTINE insertVertex_point
!
!-------------------------------------------------------------------------------
!> @brief Inserts a new vertex into a graph object
!> @param this the graph to modify
!> @param coord the coordinates of the new vertex to add
!>
SUBROUTINE insertVertex_coords(this,coord)
  CLASS(GraphType),INTENT(INOUT) :: this
  REAL(SRK),INTENT(IN) :: coord(2)
  INTEGER(SIK) :: i,j,n,k,idx
  INTEGER(SIK),ALLOCATABLE :: tmpE(:,:)
  REAL(SRK),ALLOCATABLE :: tmpVertices(:,:),tmpQE(:,:,:)
  IF(ALLOCATED(this%vertices)) THEN
    n=SIZE(this%vertices,DIM=2)
    CALL dmallocA(tmpVertices,2,n+1)
    j=0
    k=0
    DO i=1,n
      IF(coord(1) .APPROXLE. this%vertices(1,i)) THEN
        IF(coord(1) .APPROXEQA. this%vertices(1,i)) THEN
          IF(coord(2) .APPROXEQA. this%vertices(2,i)) THEN
            idx=-1 !Duplicate vertex
            EXIT
          ELSEIF(coord(2) < this%vertices(2,i)) THEN
            idx=i !Before i
          ELSE
            !After i
            DO j=i+1,n
              !Find index for end of sequence with same x value
              IF(.NOT.(coord(1) .APPROXEQA. this%vertices(1,j))) EXIT
            ENDDO
            j=j-1

            !Search on y through sequence of same x
            DO k=i+1,j
              IF(coord(2) .APPROXEQA. this%vertices(2,k)) THEN
                idx=-1
                EXIT
              ELSEIF(coord(2) < this%vertices(2,k)) THEN
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
      IF(idx > 1) tmpVertices(:,1:idx-1)=this%vertices(:,1:idx-1)
      tmpVertices(:,idx)=coord
      IF(idx <= n) tmpVertices(:,idx+1:n+1)=this%vertices(:,idx:n)
      CALL demallocA(this%vertices)
      CALL MOVE_ALLOC(tmpVertices,this%vertices)
      !Expand Edge Matrices
      CALL dmallocA(tmpE,n+1,n+1)
      CALL dmallocA(tmpQE,3,n+1,n+1)
      DO j=1,idx-1
        DO i=1,idx-1
          tmpE(i,j)=this%edgeMatrix(i,j)
          tmpE(j,i)=this%edgeMatrix(j,i)
          tmpQE(:,i,j)=this%quadEdges(:,i,j)
          tmpQE(:,j,i)=this%quadEdges(:,j,i)
        ENDDO
        DO i=idx+1,n+1
          tmpE(i,j)=this%edgeMatrix(i-1,j)
          tmpE(j,i)=this%edgeMatrix(j,i-1)
          tmpQE(:,i,j)=this%quadEdges(:,i-1,j)
          tmpQE(:,j,i)=this%quadEdges(:,j,i-1)
        ENDDO
      ENDDO
      DO j=idx+1,n+1
        DO i=1,idx-1
          tmpE(i,j)=this%edgeMatrix(i,j-1)
          tmpE(j,i)=this%edgeMatrix(j-1,i)
          tmpQE(:,i,j)=this%quadEdges(:,i,j-1)
          tmpQE(:,j,i)=this%quadEdges(:,j-1,i)
        ENDDO
        DO i=idx+1,n+1
          tmpE(i,j)=this%edgeMatrix(i-1,j-1)
          tmpE(j,i)=this%edgeMatrix(j-1,i-1)
          tmpQE(:,i,j)=this%quadEdges(:,i-1,j-1)
          tmpQE(:,j,i)=this%quadEdges(:,j-1,i-1)
        ENDDO
      ENDDO
      CALL demallocA(this%edgeMatrix)
      CALL MOVE_ALLOC(tmpE,this%edgeMatrix)
      CALL demallocA(this%quadEdges)
      CALL MOVE_ALLOC(tmpQE,this%quadEdges)
    ENDIF
  ELSE
    CALL dmallocA(this%vertices,2,1)
    this%vertices(:,1)=coord
    CALL dmallocA(this%edgeMatrix,1,1)
    CALL dmallocA(this%quadEdges,3,1,1)
  ENDIF
ENDSUBROUTINE insertVertex_coords
!
!-------------------------------------------------------------------------------
!> @brief Defines a linear edge between 2 vertexes
!> @param this the graph to modify
!> @param p1 the point containing the coordinates of the first vertex
!> @param p1 the point containing the coordinates of the second vertex
!>
SUBROUTINE defineLinearEdge_point(this,p1,p2)
  CLASS(GraphType),INTENT(INOUT) :: this
  TYPE(PointType),INTENT(IN) :: p1
  TYPE(PointType),INTENT(IN) :: p2
  CALL this%defineLinearEdge_coords(p1%coord,p2%coord)
ENDSUBROUTINE defineLinearEdge_point
!
!-------------------------------------------------------------------------------
!> @brief Defines a linear edge between 2 vertexes
!> @param this the graph to modify
!> @param coord1 the coordinates of the first vertex
!> @param coord2 the coordinates of the second vertex
!>
SUBROUTINE defineLinearEdge_coords(this,coord1,coord2)
  CLASS(GraphType),INTENT(INOUT) :: this
  REAL(SRK),INTENT(IN) :: coord1(2)
  REAL(SRK),INTENT(IN) :: coord2(2)
  INTEGER(SIK) :: v1,v2
  v1=this%getVertIndex(coord1)
  v2=this%getVertIndex(coord2)
  IF(v1 > 0 .AND. v2 > 0 .AND. v1 /= v2) THEN
    this%edgeMatrix(v1,v2)=1
    this%edgeMatrix(v2,v1)=1
  ENDIF
ENDSUBROUTINE defineLinearEdge_coords
!
!-------------------------------------------------------------------------------
!> @brief Defines a quadratic edge between 2 vertexes
!> @param this the graph to modify
!> @param p1 the point containing the coordinates of the first vertex
!> @param p2 the point containing the coordinates of the second vertex
!> @param c0 the centroid of the circle/arc
!> @param r the radius of the circle/arc
!>
SUBROUTINE defineQuadraticEdge_point(this,p1,p2,c0,r)
  CLASS(GraphType),INTENT(INOUT) :: this
  TYPE(PointType),INTENT(IN) :: p1
  TYPE(PointType),INTENT(IN) :: p2
  TYPE(PointType),INTENT(IN) :: c0
  REAL(SRK),INTENT(IN) :: r

  CALL this%defineQuadraticEdge_coords(p1%coord,p2%coord,c0%coord,r)

ENDSUBROUTINE defineQuadraticEdge_point
!
!-------------------------------------------------------------------------------
!> @brief Defines a quadratic edge between 2 vertexes
!> @param this the graph to modify
!> @param coord1 the coordinates of the first vertex
!> @param coord2 the coordinates of the second vertex
!> @param c0 the centroid of the circle/arc
!> @param r the radius of the circle/arc
!>
SUBROUTINE defineQuadraticEdge_coords(this,coord1,coord2,c0,r)
  CLASS(GraphType),INTENT(INOUT) :: this
  REAL(SRK),INTENT(IN) :: coord1(2)
  REAL(SRK),INTENT(IN) :: coord2(2)
  REAL(SRK),INTENT(IN) :: c0(2)
  REAL(SRK),INTENT(IN) :: r

  INTEGER(SIK) :: v1,v2
  REAL(SRK) :: x1,y1,x2,y2,r1,r2,rsq,d

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
      v1=this%getVertIndex(coord1)
      v2=this%getVertIndex(coord2)
      IF(v1 > 0 .AND. v2 > 0 .AND. v1 /= v2) THEN
        !Update edge matrix
        this%edgeMatrix(v1,v2)=-1
        this%edgeMatrix(v2,v1)=-1

        !Store circle info in quadEdges
        this%quadEdges(1:2,v1,v2)=c0
        this%quadEdges(3,v1,v2)=ABS(r)

        !Check for semi-circle and determine which half of circle
        !connects the points
        r1=(x2-x1)
        r1=r1*r1
        r2=(y2-y1)
        r2=r2*r2
        d=SQRT(r1+r2)
        IF(d .APPROXEQA. 2.0_SRK*ABS(r)) &
            this%quadEdges(3,v1,v2)=r !sign of r indicates which half
                                           !of semi-circle, all other cases
                                           !traverse shorter arc between points
        this%quadEdges(:,v2,v1)=this%quadEdges(:,v1,v2)
      ENDIF
    ENDIF
  ENDIF
ENDSUBROUTINE defineQuadraticEdge_coords
!
!-------------------------------------------------------------------------------
!> @brief Returns the midpoint of an edge
!> @param this the graph to interrogate
!> @param p1 the point with the coordinates of the edge's first endpoint
!> @param p2 the point with the coordinates of the edge's second endpoint
!> @returns m the midpoint
!>
SUBROUTINE getMidPointOnEdge_point(this,p1,p2,m)
  CLASS(GraphType),INTENT(INOUT) :: this
  TYPE(PointType),INTENT(IN) :: p1
  TYPE(PointType),INTENT(IN) :: p2
  TYPE(PointType),INTENT(OUT) :: m
  !
  REAL(SRK) :: coords(2)

  CALL m%clear()
  CALL this%getMidPointOnEdge(this%getVertIndex(p1),this%getVertIndex(p2),coords)
  CALL m%init(COORD=coords)

ENDSUBROUTINE getMidPointOnEdge_point
!
!-------------------------------------------------------------------------------
!> @brief Returns the midpoint of an edge
!> @param this the graph to interrogate
!> @param v1 the index of the edge's first endpoint
!> @param v2 the index of the edge's second endpoint
!> @returns m the midpoint
!>
SUBROUTINE getMidPointOnEdge_idx(this,v1,v2,m)
  CLASS(GraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: v1
  INTEGER(SIK),INTENT(IN) :: v2
  REAL(SRK),INTENT(INOUT) :: m(2)

  INTEGER(SIK) :: n
  REAL(SRK) :: a(2),b(2),c(2),r,alp1,alp2,theta,scal

  m=-HUGE(m)
  n=this%nVert()+1
  IF(v1 > 0 .AND. v2 > 0 .AND. v1 < n .AND. v2 < n) THEN
    IF(this%edgeMatrix(v1,v2) == 1) THEN
      m=this%vertices(:,v1)+this%vertices(:,v2)
      m=0.5_SRK*m
    ELSEIF(this%edgeMatrix(v1,v2) == -1) THEN
      a=this%vertices(:,v1)
      b=this%vertices(:,v2)
      c=this%quadEdges(1:2,v1,v2)
      r=this%quadEdges(3,v1,v2)
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
ENDSUBROUTINE getMidPointOnEdge_idx
!
!-------------------------------------------------------------------------------
!> @brief Removes a vertex from a graph object
!> @param this the graph to modify
!> @param p the point containing the coordinates of the vertex to remove
!>
SUBROUTINE removeVertex_point(this,p)
  CLASS(GraphType),INTENT(INOUT) :: this
  TYPE(PointType),INTENT(IN) :: p
  INTEGER(SIK) :: i
  i=this%getVertIndex(p%coord)
  CALL this%removeVertex(i)
ENDSUBROUTINE removeVertex_point
!
!-------------------------------------------------------------------------------
!> @brief Removes a vertex from a graph object
!> @param this the graph to modify
!> @param v the coordinates of the vertex to remove
!>
SUBROUTINE removeVertex_coords(this,v)
  CLASS(GraphType),INTENT(INOUT) :: this
  REAL(SRK),INTENT(IN) :: v(2)
  INTEGER(SIK) :: i
  i=this%getVertIndex(v)
  CALL this%removeVertex(i)
ENDSUBROUTINE removeVertex_coords
!
!-------------------------------------------------------------------------------
!> @brief Removes a vertex from a graph object
!> @param this the graph to modify
!> @param idx the index of the vertex to remove
!>
SUBROUTINE removeVertex_index(this,idx)
  CLASS(GraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: idx
  INTEGER(SIK) :: i,j,n
  INTEGER(SIK),ALLOCATABLE :: tmpEdge(:,:)
  REAL(SRK),ALLOCATABLE :: tmpVert(:,:),tmpQE(:,:,:)

  n=this%nVert()
  IF(0 < idx .AND. idx <= n) THEN
    CALL dmallocA(tmpVert,2,n-1)
    CALL dmallocA(tmpEdge,n-1,n-1)
    CALL dmallocA(tmpQE,3,n-1,n-1)
    DO i=1,idx-1
      tmpVert(:,i)=this%vertices(:,i)
      DO j=1,idx-1
        tmpEdge(j,i)=this%edgeMatrix(j,i)
        tmpQE(:,j,i)=this%quadEdges(:,j,i)
      ENDDO
      DO j=idx+1,n
        tmpEdge(j-1,i)=this%edgeMatrix(j,i)
        tmpQE(:,j-1,i)=this%quadEdges(:,j,i)
      ENDDO
    ENDDO
    DO i=idx+1,n
      tmpVert(:,i-1)=this%vertices(:,i)
      DO j=1,idx-1
        tmpEdge(j,i-1)=this%edgeMatrix(j,i)
        tmpQE(:,j,i-1)=this%quadEdges(:,j,i)
      ENDDO
      DO j=idx+1,n
        tmpEdge(j-1,i-1)=this%edgeMatrix(j,i)
        tmpQE(:,j-1,i-1)=this%quadEdges(:,j,i)
      ENDDO
    ENDDO
    CALL this%clear()
    IF(ALLOCATED(tmpVert)) &
        CALL MOVE_ALLOC(tmpVert,this%vertices)
    IF(ALLOCATED(tmpEdge)) &
        CALL MOVE_ALLOC(tmpEdge,this%edgeMatrix)
    IF(ALLOCATED(tmpQE)) &
        CALL MOVE_ALLOC(tmpQE,this%quadEdges)
  ENDIF
ENDSUBROUTINE removeVertex_index
!
!-------------------------------------------------------------------------------
!> @brief Removes an edge from a graph object
!> @param this the graph to modify
!> @param p1 the point containing the coordinates of the first edge vertex
!> @param p2 the point containing the coordinates of the second edge vertex
!>
SUBROUTINE removeEdge_point(this,p1,p2)
  CLASS(GraphType),INTENT(INOUT) :: this
  TYPE(PointType),INTENT(IN) :: p1
  TYPE(PointType),INTENT(IN) :: p2

  CALL this%removeEdge(this%getVertIndex(p1%coord),this%getVertIndex(p2%coord))

ENDSUBROUTINE removeEdge_point
!
!-------------------------------------------------------------------------------
!> @brief Removes an edge from a graph object
!> @param this the graph to modify
!> @param c1 the coordinates of the first edge vertex
!> @param c2 the coordinates of the second edge vertex
!>
SUBROUTINE removeEdge_coords(this,c1,c2)
  CLASS(GraphType),INTENT(INOUT) :: this
  REAL(SRK),INTENT(IN) :: c1(2)
  REAL(SRK),INTENT(IN) :: c2(2)

  CALL this%removeEdge(this%getVertIndex(c1),this%getVertIndex(c2))

ENDSUBROUTINE removeEdge_coords

!
!-------------------------------------------------------------------------------
!> @brief Removes an edge from a graph object
!> @param this the graph to modify
!> @param i the index of the first edge vertex
!> @param j the index of the second edge vertex
!>
SUBROUTINE removeEdge_index(this,i,j)
  CLASS(GraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: i
  INTEGER(SIK),INTENT(IN) :: j
  INTEGER(SIK) :: n

  n=this%nVert()+1
  IF(i > 0 .AND. j > 0 .AND. i < n .AND. j < n) THEN
    this%edgeMatrix(i,j)=0
    this%edgeMatrix(j,i)=0
    this%quadEdges(:,i,j)=0.0_SRK
    this%quadEdges(:,j,i)=0.0_SRK
  ENDIF
ENDSUBROUTINE removeEdge_index
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
SUBROUTINE removeFilamentFromVert(this,i0,i1)
  CLASS(GraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: i0
  INTEGER(SIK),INTENT(IN) :: i1

  LOGICAL(SBK) :: isCycleEdge
  INTEGER(SIK) :: n,nAdj,v0,v1
  REAL(SRK) :: xy(2)

  n=this%nVert()
  IF(0 < i0 .AND. i0 <= n .AND. 0 < i1 .AND. i1 <= n) THEN
    v0=i0
    v1=i1
    isCycleEdge=.FALSE.
    IF(ALLOCATED(this%isCycleEdge)) THEN
      isCycleEdge=this%isCycleEdge(v0,v1)
    ENDIF
    IF(isCycleEdge) THEN
      nAdj=this%nAdjacent(v0)
      IF(nAdj > 2) THEN
        CALL this%removeEdge(v0,v1)
        v0=v1
        nAdj=this%nAdjacent(v0)
        IF(nAdj == 1) v1=this%getAdjacentVert(v0,1)
      ENDIF
      DO WHILE(nAdj == 1)
        v1=this%getAdjacentVert(v0,1)
        IF(this%isCycleEdge(v0,v1)) THEN
          xy=this%vertices(:,v1)
          CALL this%removeVertex(v0)
          v0=this%getVertIndex(xy)
          nAdj=this%nAdjacent(v0)
        ELSE
          EXIT
        ENDIF
      ENDDO
      nAdj=this%nAdjacent(v0)
      IF(nAdj == 0) CALL this%removeVertex(v0)
    ELSE
      nAdj=this%nAdjacent(v0)
      IF(nAdj > 2 .AND. v1 /= v0) THEN
        CALL this%removeEdge(v0,v1)
        v0=v1
        nAdj=this%nAdjacent(v0)
        IF(nAdj == 1) v1=this%getAdjacentVert(v0,1)
      ENDIF
      DO WHILE(nAdj == 1)
        v1=this%getAdjacentVert(v0,1)
        xy=this%vertices(:,v1)
        CALL this%removeVertex(v0)
        v0=this%getVertIndex(xy)
        nAdj=this%nAdjacent(v0)
      ENDDO
      IF(nAdj == 0) CALL this%removeVertex(v0)
    ENDIF
  ENDIF
ENDSUBROUTINE removeFilamentFromVert
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
SUBROUTINE extractPrimitive(this,v0,subgraph)
  CLASS(GraphType),INTENT(INOUT) :: this
  INTEGER(SIK) :: v0
  CLASS(GraphType),INTENT(INOUT) :: subgraph

  LOGICAL(SBK),ALLOCATABLE :: visited(:)
  INTEGER(SIK) :: i,j,n,nAdj,vCurr,vNext,vPrev
  REAL(SRK) :: coord1(2),coord2(2),c0(2),r

  CALL subGraph%clear()
  n=this%nVert()
  ALLOCATE(visited(0:n)); visited=.FALSE.
  vNext=this%getCWMostVert(0,v0)
  vPrev=v0
  vCurr=vNext

  coord1=this%vertices(:,vPrev)
  CALL subgraph%insertVertex(coord1)
  DO WHILE(vCurr /= 0 .AND. vCurr /= v0 .AND. .NOT.visited(vCurr))
    coord1=this%vertices(:,vPrev)
    coord2=this%vertices(:,vCurr)
    CALL subgraph%insertVertex(coord2)
    IF(this%edgeMatrix(vPrev,vCurr) == 1) THEN
      CALL subgraph%defineEdge(coord1,coord2)
    ELSEIF(this%edgeMatrix(vPrev,vCurr) == -1) THEN
      c0=this%quadEdges(1:2,vPrev,vCurr)
      r=this%quadEdges(3,vPrev,vCurr)
      CALL subgraph%defineEdge(coord1,coord2,c0,r)
    ENDIF
    visited(vCurr)=.TRUE.
    vNext=this%getCCWMostVert(vPrev,vCurr)
    vPrev=vCurr
    vCurr=vNext
  ENDDO
  IF(vCurr == 0) THEN
    !Found a filament, not necessarily rooted at vPrev
    vCurr=this%getAdjacentVert(vPrev,1)
    CALL this%removeFilamentFromVert(vPrev,vCurr)
  ELSEIF(vCurr == v0) THEN
    !Minimum Cycle Found!

    !Add Last/First point and last edge
    coord1=this%vertices(:,vPrev)
    coord2=this%vertices(:,vCurr)
    IF(this%edgeMatrix(vPrev,vCurr) == 1) THEN
      CALL subgraph%defineEdge(coord1,coord2)
    ELSEIF(this%edgeMatrix(vPrev,vCurr) == -1) THEN
      c0=this%quadEdges(1:2,vPrev,vCurr)
      r=this%quadEdges(3,vPrev,vCurr)
      CALL subgraph%defineEdge(coord1,coord2,c0,r)
    ENDIF

    n=subGraph%nVert()
    DO i=1,n
      j=i+1
      IF(i == n) j=1
      vCurr=this%getVertIndex(subgraph%vertices(:,i))
      vNext=this%getVertIndex(subgraph%vertices(:,j))
      this%isCycleEdge(vCurr,vNext)=.TRUE.
    ENDDO
    vCurr=v0
    vNext=this%getCWMostVert(0,v0)
    CALL this%removeEdge(vCurr,vNext)
    nAdj=this%nAdjacent(vCurr)
    IF(nAdj == 1) THEN
      CALL this%removeFilamentFromVert(vCurr,this%getAdjacentVert(vCurr,1))
    ENDIF
    nAdj=this%nAdjacent(vNext)
    IF(nAdj == 1) THEN
      CALL this%removeFilamentFromVert(vNext,this%getAdjacentVert(vNext,1))
    ENDIF
  ELSE
    !vCurr was visited earlier, but we do not have a minimal cycle
    !This  implies v0 is part of a filament. Therefore we locate
    !a direction away from the initial vNext.
    vCurr=v0
    vNext=this%getCWMostVert(0,v0)
    nAdj=this%nAdjacent(vCurr)
    DO WHILE(nAdj == 2)
      IF(this%getAdjacentVert(vCurr,1) /= vNext) THEN
        vNext=vCurr
        vCurr=this%getAdjacentVert(vCurr,1)
      ELSE
        vNext=vCurr
        vCurr=this%getAdjacentVert(vCurr,2)
      ENDIF
      nAdj=this%nAdjacent(vCurr)
    ENDDO
    CALL this%removeFilamentFromVert(vCurr,vNext)
  ENDIF
ENDSUBROUTINE extractPrimitive
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
SUBROUTINE getMCB_graphType(this,cycles)
  CLASS(GraphType),INTENT(IN) :: this
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
  g=this
  n=this%nVert()
  CALL dmallocA(g%isCycleEdge,n,n)
  ncycles=0
  DO WHILE(g%nVert() > 0)
    nadj=g%nAdjacent(1)
    IF(nadj == 0) THEN
      CALL g%removeVertex(1)
    ELSEIF(nadj == 1) THEN
      CALL g%removeFilamentFromVert(1,1)
    ELSE
      CALL g%extractPrimitive(1,primeGraph)
      IF(isMinimumCycle_graphType(primeGraph)) THEN
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
  CALL demallocA(g%isCycleEdge)
ENDSUBROUTINE getMCB_graphType
!
!-------------------------------------------------------------------------------
!> @brief Edits a graph object to a VTK file
!> @param this the graph object to edit
!> @param fname the file name to write to
!> @param unitNo the unit number to use; optional
!>
SUBROUTINE editToVTK_graphType(this,fname,unitNo)
  CLASS(GraphType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: fname
  INTEGER(SIK),INTENT(IN),OPTIONAL :: unitNo

  INTEGER(SIK) :: i,j,nvert,nedge,n
  TYPE(VTKMeshType) :: vtkMesh
  TYPE(VTKLegFileType) :: vtkFile

  nvert=this%nVert()
  IF(nvert > 0) THEN
    vtkMesh%meshType=VTK_UNSTRUCTURED_GRID
    vtkMesh%dims=nvert
    vtkMesh%numPoints=nvert
    ALLOCATE(vtkMesh%x(nvert))
    ALLOCATE(vtkMesh%y(nvert))
    ALLOCATE(vtkMesh%z(nvert))
    DO i=1,nvert
      vtkMesh%x(i)=this%vertices(1,i)
      vtkMesh%y(i)=this%vertices(2,i)
      vtkMesh%z(i)=0.0_SRK
    ENDDO
    nedge=this%nEdge()
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
          IF(ABS(this%edgeMatrix(i,j)) == 1) THEN
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
!> @param this the graph object to clear
!>
SUBROUTINE clear_graphType(this)
  CLASS(GraphType),INTENT(INOUT) :: this
  CALL demallocA(this%vertices)
  CALL demallocA(this%edgeMatrix)
  CALL demallocA(this%quadEdges)
ENDSUBROUTINE clear_graphType
!
!-------------------------------------------------------------------------------
!> @brief Inserts all vertexes and edges from one graph into another
!> @param this the target graph to be modified
!> @param g the source graph whose pionts are being inserted
!>
SUBROUTINE combine_GraphType(this,g)
  CLASS(GraphType),INTENT(INOUT) :: this
  TYPE(GraphType),INTENT(IN) :: g
  !
  INTEGER(SIK) :: source_i1,source_i2,source_edge_type,edge_endpoint_i1,edge_endpoint_i2
  INTEGER(SIK) :: target_edge_type
  TYPE(PointType) :: source_p1,source_p2,target_p1,target_p2,centroid
  TYPE(PointType) :: intersect_p1,intersect_p2
  TYPE(PointType),ALLOCATABLE :: intersection_points(:)
  TYPE(LineType) :: source_line,target_line
  TYPE(CircleType) :: source_circle,target_circle
  TYPE(GraphType) :: source_edge,target,source

  target=this
  source=g

  DO WHILE(source%nEdge() > 0)
    !Find the next edge in the source graph
    find_source_edge: DO source_i1=1,source%nVert()
      DO source_i2=source_i1+1,source%nVert()
        IF(source%edgeMatrix(source_i1,source_i2) /= 0) THEN
          EXIT find_source_edge
        ENDIF
      ENDDO !source_i1
    ENDDO find_source_edge
    source_edge_type = source%edgeMatrix(source_i1,source_i2)

    !Get the coordinates for the source edge
    CALL source_p1%init(COORD=source%vertices(1:2,source_i1))
    CALL source_p2%init(COORD=source%vertices(1:2,source_i2))

    !Set up a separate graph that will just contain this edge
    CALL source_edge%insertVertex(source_p1)
    CALL source_edge%insertVertex(source_p2)
    SELECTCASE(source_edge_type)
    CASE(GRAPH_LINEAR_EDGE)
      CALL source_edge%defineEdge(source_p1,source_p2)
    CASE(GRAPH_QUADRATIC_EDGE)
      CALL centroid%init(COORD=source%quadEdges(1:2,source_i1,source_i2))
      CALL source_edge%defineEdge(source_p1,source_p2,centroid,source%quadEdges(3,source_i1,source_i2))
      CALL centroid%clear()
    ENDSELECT

    !Set up either a line or a circle object for the source edge
    SELECTCASE(source_edge_type)
    CASE(GRAPH_LINEAR_EDGE)
      CALL source_line%set(source_p1,source_p2)
    CASE(GRAPH_QUADRATIC_EDGE)
      CALL centroid%init(COORD=source%quadEdges(1:2,source_i1,source_i2))
      source_circle=generateCircle(source_p1,source_p2, &
          centroid,source%quadEdges(3,source_i1,source_i2))
      CALL centroid%clear()
    ENDSELECT

    !Loop over all target edges and search for intersections
    DO edge_endpoint_i1=1,this%nVert()
      DO edge_endpoint_i2=edge_endpoint_i1+1,this%nVert()
        target_edge_type=this%edgeMatrix(edge_endpoint_i1,edge_endpoint_i2)
        !If there's no edge, then just cycle to the next set of points
        IF(target_edge_type == GRAPH_NULL_EDGE) THEN
          CYCLE
        ENDIF

        !Get the coordinates for the target edge
        CALL target_p1%init(COORD=this%vertices(1:2,edge_endpoint_i1))
        CALL target_p2%init(COORD=this%vertices(1:2,edge_endpoint_i2))

        !Set up either a line or a circle object for the target edge
        SELECTCASE(target_edge_type)
        CASE(GRAPH_LINEAR_EDGE)
          CALL target_line%set(target_p1,target_p2)
        CASE(GRAPH_QUADRATIC_EDGE)
          CALL centroid%init(COORD=this%quadEdges(1:2,edge_endpoint_i1,edge_endpoint_i2))
          target_circle=generateCircle(target_p1,target_p2, &
              centroid,this%quadEdges(3,edge_endpoint_i1,edge_endpoint_i2))
          CALL centroid%clear()
        ENDSELECT

        !Now we need to get all the intersection points.  There can be either
        !0, 1, or 2 points of interection between the source and target edges:
        !  Source   Target   Intersections
        !    line     line     0, 1
        !    line   circle     0, 1, 2
        !  circle     line     0, 1, 2
        !  circle   circle     0, 1, 2
        SELECTCASE(source_edge_type)
        CASE(GRAPH_LINEAR_EDGE)
          SELECTCASE(target_edge_type)
          CASE(GRAPH_LINEAR_EDGE)
            intersect_p1 = source_line%intersect(target_line)
            SELECTCASE(intersect_p1%dim)
            CASE(2) !Intersection found
              ALLOCATE(intersection_points(1))
              intersection_points(1)=intersect_p1
            CASE DEFAULT !No intersection
              ALLOCATE(intersection_points(0))
            ENDSELECT
          CASE(GRAPH_QUADRATIC_EDGE)
            CALL target_circle%intersect(source_line,intersect_p1,intersect_p2)
            !Tangent line, so 1 point of intersection
            IF(intersect_p1%dim == -3 .AND. intersect_p2%dim == -3) THEN
              ALLOCATE(intersection_points(1))
              intersect_p1%dim=2
              intersection_points(1)=intersect_p1
            !Two intersections
            ELSEIF(intersect_p1%dim == 2 .AND. intersect_p2%dim == 2) THEN
              ALLOCATE(intersection_points(2))
              intersection_points(1)=intersect_p1
              intersection_points(2)=intersect_p2
            !Only the first point is an intersection
            ELSEIF(intersect_p1%dim == 2) THEN
              ALLOCATE(intersection_points(1))
              intersection_points(1)=intersect_p1
            !Only the second point is an intersection
            ELSEIF(intersect_p2%dim == 2) THEN
              ALLOCATE(intersection_points(1))
              intersection_points(1)=intersect_p2
            !Neither point is an intersection
            ELSE
              ALLOCATE(intersection_points(0))
            ENDIF
          ENDSELECT
        CASE(GRAPH_QUADRATIC_EDGE)
          SELECTCASE(target_edge_type)
          CASE(GRAPH_LINEAR_EDGE)
            CALL source_circle%intersect(target_line,intersect_p1,intersect_p2)
            !Tangent line, so 1 point of intersection
            IF(intersect_p1%dim == -3 .AND. intersect_p2%dim == -3) THEN
              ALLOCATE(intersection_points(1))
              intersect_p1%dim=2
              intersection_points(1)=intersect_p1
            !Two intersections
            ELSEIF(intersect_p1%dim == 2 .AND. intersect_p2%dim == 2) THEN
              ALLOCATE(intersection_points(2))
              intersection_points(1)=intersect_p1
              intersection_points(2)=intersect_p2
            !Only the first point is an intersection
            ELSEIF(intersect_p1%dim == 2) THEN
              ALLOCATE(intersection_points(1))
              intersection_points(1)=intersect_p1
            !Only the second point is an intersection
            ELSEIF(intersect_p2%dim == 2) THEN
              ALLOCATE(intersection_points(1))
              intersection_points(1)=intersect_p2
            !Neither point is an intersection
            ELSE
              ALLOCATE(intersection_points(0))
            ENDIF
          CASE(GRAPH_QUADRATIC_EDGE)
            CALL source_circle%intersect(target_circle,intersect_p1,intersect_p2)
            !Circles are tangent
            IF(intersect_p1%dim == -3 .AND. intersect_p2%dim == -3) THEN
              ALLOCATE(intersection_points(1))
              intersect_p1%dim=2
              intersection_points(1)=intersect_p1
            !Two intersections
            ELSEIF(intersect_p1%dim == 2 .AND. intersect_p2%dim == 2) THEN
              ALLOCATE(intersection_points(2))
              intersection_points(1)=intersect_p1
              intersection_points(2)=intersect_p2
            !No intersections found (or circles overlap or some other degerate case)
            ELSE
              ALLOCATE(intersection_points(0))
            ENDIF
          ENDSELECT
        ENDSELECT
        CALL intersect_p1%clear()
        CALL intersect_p2%clear()

        !Divide the target edge for each intersection
        CALL target%divideEdge(target_p1,target_p2,intersection_points)

        !Divide the source edge for each intersection
        CALL source_edge%divideEdge(source_p1,source_p2,intersection_points)

        DEALLOCATE(intersection_points)
      ENDDO !edge_endpoint_i2
    ENDDO !edge_endpoint_i1

    !Now we need to insert all the vertexes/edges from the source edge
    source_i1=1
    CALL target%insertVertex(source_edge%vertices(1:2,source_i1))
    DO source_i2=2,source_edge%nVert()
      CALL target%insertVertex(source_edge%vertices(1:2,source_i2))
      SELECTCASE(source_edge_type)
      CASE(GRAPH_LINEAR_EDGE)
        CALL target%defineEdge(source_p1,source_p2)
      CASE(GRAPH_QUADRATIC_EDGE)
        CALL target%defineEdge(source_p1,source_p2,source_circle%c, &
            source_circle%r)
      ENDSELECT
      source_i1=source_i2
    ENDDO !source_i2

    !Clear up objects
    CALL source_edge%clear()
    CALL source_line%clear()
    CALL source_circle%clear()
  ENDDO !WHILE(source%nEdge() > 0)

  !Copy the temporary graph into the input graph, then clean up
  SELECTTYPE(this); TYPE IS(GraphType)
    this=target
  ENDSELECT
  CALL target%clear()
  CALL source%clear()

ENDSUBROUTINE combine_GraphType
SUBROUTINE combine_GraphType_old(this,g)
  CLASS(GraphType),INTENT(INOUT) :: this
  TYPE(GraphType),INTENT(IN) :: g
  TYPE(GraphType) :: g0,g1,lineAB
  INTEGER(SIK) :: i,j,n,nAdj,v1,v2
  INTEGER(SIK),ALLOCATABLE :: cwVerts(:)
  REAL(SRK) :: alp1,alp2,theta,theta_shift,r,scal,x1,y1,r2
  REAL(SRK) :: a(2),b(2),c(2),d(2),m(2)
  REAL(SRK),ALLOCATABLE :: vTheta(:)
  TYPE(PointType) :: p0,p1,p2,p3,p4
  TYPE(LineType) :: l1,l2
  TYPE(CircleType) :: c1,c2

  g0 = this
  g1 = g
  CALL l1%p1%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
  CALL l1%p2%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
  CALL l2%p1%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
  CALL l2%p2%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
  CALL p0%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
  DO WHILE(g1%nEdge() > 0)
    n=g1%nVert()
    v1=0; v2=0
    outer: DO i=1,n
      DO j=i+1,n
        IF(g1%edgeMatrix(j,i) /= 0) THEN
          v1=i
          v2=j
          EXIT outer
        ENDIF
      ENDDO
    ENDDO outer

    IF(v1 > 0 .AND. v2 > 0) THEN
      a=g1%vertices(:,v1)
      b=g1%vertices(:,v2)
      CALL lineAB%insertVertex(a)
      CALL lineAB%insertVertex(b)
      IF(g1%edgeMatrix(v1,v2) == -1) THEN
        p0%coord=g1%quadEdges(1:2,v1,v2)
        r=g1%quadEdges(3,v1,v2)
        alp1=ATAN2PI(a(1)-p0%coord(1),a(2)-p0%coord(2))
        alp2=ATAN2PI(b(1)-p0%coord(1),b(2)-p0%coord(2))

        !Insure we are traversing the shorter arc on the circle
        IF(ABS(alp1-alp2) .APPROXEQA. PI) THEN
          !Semi-circle, for this case we must look at sign of r
          IF(r < 0.0_SRK) THEN
            CALL c1%set(p0,ABS(r),alp1,alp2)
          ELSE
            CALL c1%set(p0,ABS(r),alp2,alp1)
          ENDIF
        ELSE
          !Distance between alpha_1 and alpha_2 is < PI
          IF(a(2) .APPROXEQA. p0%coord(2)) THEN
            IF(b(2) > a(2)) THEN
              CALL c1%set(p0,ABS(r),alp2,alp1)
            ELSE
              CALL c1%set(p0,ABS(r),alp1,alp2)
            ENDIF
          ELSEIF(a(2) < p0%coord(2)) THEN
            CALL c1%set(p0,ABS(r),alp1,alp2)
          ELSE
            CALL c1%set(p0,ABS(r),alp2,alp1)
          ENDIF
        ENDIF

        !Compute arc midpoint
        m=a+b-2.0_SRK*p0%coord
        scal=SQRT(m(1)*m(1)+m(2)*m(2))
        IF(scal .APPROXEQA. 0.0_SRK) THEN
          !Half circle. Lame.
          theta=0.5_SRK*(alp1+alp2)
          !Adjust theta for the appropriate half of the circle
          IF(.NOT.((c1%thetastt .APPROXLE. theta) .AND. &
              (theta .APPROXLE. c1%thetastp))) theta=theta-PI
          m(1)=c1%r*COS(theta)
          m(2)=c1%r*SIN(theta)
        ELSE
          scal=ABS(r)/scal
          m=m*scal
        ENDIF
        m=m+c1%c%coord

        !Compute theta shift so that thetastp > thetastt when crossing x+ axis
        theta_shift=0.0_SRK
        IF(c1%thetastt > c1%thetastp) theta_shift=TWOPI
      ELSE
        l1%p1%coord=a
        l1%p2%coord=b
      ENDIF

      n=this%nVert()
      DO i=1,n
        DO j=i+1,n
          IF(this%edgeMatrix(j,i) /= 0) THEN
            c=this%vertices(:,i)
            d=this%vertices(:,j)
          ENDIF
          IF(this%edgeMatrix(j,i) == 1) THEN
            l2%p1%dim=2; l2%p2%dim=2
            l2%p1%coord=c
            l2%p2%coord=d
            IF(c1%r == 0.0_SRK) THEN
              p1=l1%intersect(l2)
              IF(p1%dim == 2) THEN
                CALL g0%removeEdge(c,d)
                CALL g0%insertVertex(p1%coord)
                CALL g0%defineEdge(c,p1%coord)
                CALL g0%defineEdge(d,p1%coord)
                CALL lineAB%insertVertex(p1%coord)
              ENDIF
            ELSE
              !circle-line
              CALL c1%intersect(l2,p1,p2)

              !Count tangent points
              IF(p1%dim == -3) p1%dim=2
              IF(p1%dim == 0 .AND. p2%dim == 2) THEN
                p1=p2
                CALL p2%clear()
              ENDIF

              !Check for intersections on ends of line segments
              IF(p1%dim == 0) THEN
                x1=c(1)-c1%c%coord(1)
                y1=c(2)-c1%c%coord(2)
                r2=x1*x1+y1*y1
                IF(r2 .APPROXEQA. c1%r*c1%r) THEN
                  p1=l2%p1
                  l2%p1%dim=0
                ELSE
                  x1=d(1)-c1%c%coord(1)
                  y1=d(2)-c1%c%coord(2)
                  r2=x1*x1+y1*y1
                  IF(r2 .APPROXEQA. c1%r*c1%r) THEN
                    p1=l2%p2
                    l2%p2%dim=0
                  ENDIF
                ENDIF
              ENDIF
              IF(l2%p1%dim == 2 .AND. p2%dim == 0) THEN
                x1=c(1)-c1%c%coord(1)
                y1=c(2)-c1%c%coord(2)
                r2=x1*x1+y1*y1
                IF(r2 .APPROXEQA. c1%r*c1%r) THEN
                  p2=l2%p1
                  l2%p1%dim=0
                ENDIF
              ELSEIF(l2%p2%dim == 2 .AND. p2%dim == 0) THEN
                x1=d(1)-c1%c%coord(1)
                y1=d(2)-c1%c%coord(2)
                r2=x1*x1+y1*y1
                IF(r2 .APPROXEQA. c1%r*c1%r) THEN
                  p2=l2%p2
                  l2%p2%dim=0
                ENDIF
              ENDIF

              !Filter points for interval of theta on arc. (might have bugs?)
              IF(p1%dim == 2) THEN
                theta=ATAN2PI(p1%coord(1)-c1%c%coord(1), &
                              p1%coord(2)-c1%c%coord(2))
                IF(p1%coord(2)-c1%c%coord(2) .APPROXGE. 0.0_SRK) &
                    theta=theta+theta_shift
                IF(.NOT.((c1%thetastt .APPROXLE. theta) .AND. &
                    (theta .APPROXLE. c1%thetastp+theta_shift))) CALL p1%clear()
              ENDIF
              IF(p2%dim == 2) THEN
                theta=ATAN2PI(p2%coord(1)-c1%c%coord(1), &
                              p2%coord(2)-c1%c%coord(2))
                IF(p2%coord(2)-c1%c%coord(2) .APPROXGE. 0.0_SRK) &
                    theta=theta+theta_shift
                IF(.NOT.((c1%thetastt .APPROXLE. theta) .AND. &
                    (theta .APPROXLE. c1%thetastp+theta_shift))) CALL p2%clear()
              ENDIF
              IF(p1%dim == 2 .AND. p2%dim == 2) THEN
                CALL g0%removeEdge(c,d)
                CALL g0%insertVertex(p1%coord)
                CALL g0%insertVertex(p2%coord)
                !Cord intersecting circle
                CALL g0%defineEdge(p1%coord,p2%coord)
                CALL g0%defineEdge(c,p1%coord) !is p1 always closer to c?
                CALL g0%defineEdge(d,p2%coord) !is p2 always closer to d?
                CALL lineAB%insertVertex(p1%coord)
                CALL lineAB%insertVertex(p2%coord)

                !Add midpoint of arc (keeps graph sane)
                !Need to store edge information here, because normal point sorting
                !on graph type does not implicitly keep points ordered for arcs
                CALL lineAB%insertVertex(m)
                CALL lineAB%defineEdge(m,p1%coord)
                CALL lineAB%defineEdge(m,p2%coord)
              ELSE
                IF(p1%dim /= 2 .AND. p2%dim == 2) p1=p2
                IF(p1%dim == 2) THEN
                  CALL g0%removeEdge(c,d)
                  CALL g0%insertVertex(p1%coord)
                  CALL g0%defineEdge(c,p1%coord)
                  CALL g0%defineEdge(d,p1%coord)
                  CALL lineAB%insertVertex(p1%coord)
                ENDIF
              ENDIF
            ENDIF
          ELSEIF(this%edgeMatrix(j,i) == -1) THEN
            p0%coord=this%quadEdges(1:2,i,j)
            r=this%quadEdges(3,i,j)
            alp1=ATAN2PI(c(1)-p0%coord(1),c(2)-p0%coord(2))
            alp2=ATAN2PI(d(1)-p0%coord(1),d(2)-p0%coord(2))
            !Insure we are traversing the shorter arc on the circle
            IF(ABS(alp1-alp2) .APPROXEQA. PI) THEN
              !Semi-circle, for this case we must look at sign of r
              IF(r < 0.0_SRK) THEN
                CALL c2%set(p0,ABS(r),alp1,alp2)
              ELSE
                CALL c2%set(p0,ABS(r),alp2,alp1)
              ENDIF
            ELSE
              !Distance between alpha_1 and alpha_2 is < PI
              IF(c(2) .APPROXEQA. p0%coord(2)) THEN
                IF(d(2) > c(2)) THEN
                  CALL c2%set(p0,ABS(r),alp2,alp1)
                ELSE
                  CALL c2%set(p0,ABS(r),alp1,alp2)
                ENDIF
              ELSEIF(c(2) < p0%coord(2)) THEN
                CALL c2%set(p0,ABS(r),alp1,alp2)
              ELSE
                CALL c2%set(p0,ABS(r),alp2,alp1)
              ENDIF
            ENDIF
            IF(c1%r == 0.0_SRK) THEN
              !line-circle
              CALL c2%intersect(l1,p1,p2)

              !Count tangent points
              IF(p1%dim == -3) p1%dim=2
              IF(p1%dim == 0 .AND. p2%dim == 2) THEN
                p1=p2
                CALL p2%clear()
              ENDIF

              !Check for intersections on ends of line segments
              IF(p1%dim == 0) THEN
                x1=a(1)-c2%c%coord(1)
                y1=a(2)-c2%c%coord(2)
                r2=x1*x1+y1*y1
                IF(r2 .APPROXEQA. c2%r*c2%r) THEN
                  p1=l1%p1
                  l1%p1%dim=0
                ELSE
                  x1=b(1)-c2%c%coord(1)
                  y1=b(2)-c2%c%coord(2)
                  r2=x1*x1+y1*y1
                  IF(r2 .APPROXEQA. c2%r*c2%r) THEN
                    p1=l1%p2
                    l1%p2%dim=0
                  ENDIF
                ENDIF
              ENDIF
              IF(l1%p1%dim == 2 .AND. p2%dim == 0) THEN
                x1=a(1)-c2%c%coord(1)
                y1=a(2)-c2%c%coord(2)
                r2=x1*x1+y1*y1
                IF(r2 .APPROXEQA. c2%r*c2%r) THEN
                  p2=l1%p1
                  l1%p1%dim=0
                ENDIF
              ELSEIF(l1%p2%dim == 2 .AND. p2%dim == 0) THEN
                x1=b(1)-c2%c%coord(1)
                y1=b(2)-c2%c%coord(2)
                r2=x1*x1+y1*y1
                IF(r2 .APPROXEQA. c2%r*c2%r) THEN
                  p2=l1%p2
                  l1%p2%dim=0
                ENDIF
              ENDIF

              !Filter points for interval of theta on arc. (might have bugs?)
              theta_shift=0.0_SRK
              IF(c2%thetastt > c2%thetastp) theta_shift=TWOPI
              IF(p1%dim == 2) THEN
                theta=ATAN2PI(p1%coord(1)-c2%c%coord(1), &
                              p1%coord(2)-c2%c%coord(2))
                IF(p1%coord(2)-c2%c%coord(2) .APPROXGE. 0.0_SRK) &
                    theta=theta+theta_shift
                IF(.NOT.((c2%thetastt .APPROXLE. theta) .AND. &
                    (theta .APPROXLE. c2%thetastp+theta_shift))) CALL p1%clear()
              ENDIF
              IF(p2%dim == 2) THEN
                theta=ATAN2PI(p2%coord(1)-c2%c%coord(1), &
                              p2%coord(2)-c2%c%coord(2))
                IF(p2%coord(2)-c2%c%coord(2) .APPROXGE. 0.0_SRK) &
                    theta=theta+theta_shift
                IF(.NOT.((c2%thetastt .APPROXLE. theta) .AND. &
                    (theta .APPROXLE. c2%thetastp+theta_shift))) CALL p2%clear()
              ENDIF
              IF(p1%dim == 2 .AND. p2%dim == 2) THEN
                !Compute arc midpoint
                m=c+d-2.0_SRK*p0%coord
                scal=SQRT(m(1)*m(1)+m(2)*m(2))
                IF(scal .APPROXEQA. 0.0_SRK) THEN
                  !Half circle. Lame.
                  theta=0.5_SRK*(alp1+alp2)
                  !Adjust theta for the appropriate half of the circle
                  IF(.NOT.((c2%thetastt .APPROXLE. theta) .AND. &
                      (theta .APPROXLE. c2%thetastp))) theta=theta-PI
                  m(1)=c2%r*COS(theta)
                  m(2)=c2%r*SIN(theta)
                ELSE
                  scal=ABS(r)/scal
                  m=m*scal
                ENDIF
                m=m+c2%c%coord

                CALL g0%removeEdge(c,d)
                CALL g0%insertVertex(p1%coord)
                CALL g0%insertVertex(p2%coord)

                !Add midpoint of arc (keeps graph sane)
                !p1 and p2 are connected by a straight point and an arc
                CALL g0%insertVertex(m)
                CALL g0%defineEdge(m,p1%coord,c2%c%coord,c2%r)
                CALL g0%defineEdge(m,p2%coord,c2%c%coord,c2%r)

                !Cord intersecting circle
                CALL g0%defineEdge(p1%coord,p2%coord)
                !is p1 always closer to c?
                CALL g0%defineEdge(c,p1%coord,c2%c%coord,c2%r)
                !is p2 always closer to d?
                CALL g0%defineEdge(d,p2%coord,c2%c%coord,c2%r)
                CALL lineAB%insertVertex(p1%coord)
                CALL lineAB%insertVertex(p2%coord)
              ELSE
                IF(p1%dim /= 2 .AND. p2%dim == 2) p1=p2
                IF(p1%dim == 2) THEN
                  CALL g0%removeEdge(c,d)
                  CALL g0%insertVertex(p1%coord)
                  CALL g0%defineEdge(c,p1%coord,c2%c%coord,c2%r)
                  CALL g0%defineEdge(d,p1%coord,c2%c%coord,c2%r)
                  CALL lineAB%insertVertex(p1%coord)
                ENDIF
              ENDIF
              l1%p1%dim=2; l1%p2%dim=2
            ELSE
              !circle-circle (F-this)

            ENDIF
          ENDIF
        ENDDO
      ENDDO
      IF(c1%r == 0.0_SRK) THEN
        CALL g0%insertVertex(lineAB%vertices(:,1))
        DO i=2,lineAB%nVert()
          CALL g0%insertVertex(lineAB%vertices(:,i))
          CALL g0%defineEdge(lineAB%vertices(:,i-1), &
              lineAB%vertices(:,i))
        ENDDO
      ELSE
        !Sort vertices in clock-wise order.
        n=lineAB%nVert()
        ALLOCATE(cwVerts(n)); cwVerts=0
        ALLOCATE(vTheta(n));
        DO i=1,n
          vTheta(i)=ATAN2PI(lineAB%vertices(1,i)-c1%c%coord(1), &
              lineAB%vertices(2,i)-c1%c%coord(2))
        ENDDO
        IF(c1%thetastt > c1%thetastp) THEN
          !This arc crosses the positive x-axis, shift some angles by 2*PI
          !so vertex theta's can be ordered sequentially.
          DO i=1,n
            IF((0.0_SRK .APPROXLE. vTheta(i)) .AND. vTheta(i) < c1%thetastt) &
                vTheta(i)=vTheta(i)+TWOPI
          ENDDO
        ENDIF
        DO i=1,n
          cwVerts(i)=MINLOC(vTheta,DIM=1)
          vTheta(cwVerts(i))=HUGE(vTheta(1))
        ENDDO

        !Add vertices in CW-order and define edges
        CALL g0%insertVertex(lineAB%vertices(:,cwVerts(1)))
        DO i=2,lineAB%nVert()
          CALL g0%insertVertex(lineAB%vertices(:,cwVerts(i)))
          CALL g0%defineEdge(lineAB%vertices(:,cwVerts(i-1)), &
              lineAB%vertices(:,cwVerts(i)),c1%c%coord,c1%r)
        ENDDO
        DEALLOCATE(vTheta,cwVerts)
      ENDIF
      SELECTTYPE(this); TYPE IS(GraphType)
        this = g0
      ENDSELECT
      !CALL editToVTK_graphType(this,'tmpG.vtk')
      CALL c1%clear()
      CALL lineAB%clear()
    ENDIF

    !Remove the edge
    CALL g1%removeEdge(v1,v2)
    !Remove any isoloated vertices
    nAdj=g1%nAdjacent(v1)
    IF(nAdj == 0) CALL g1%removeVertex(v1)
    v2=g1%getVertIndex(b)
    nAdj=g1%nAdjacent(v2)
    IF(nAdj == 0) CALL g1%removeVertex(v2)
  ENDDO
  CALL p0%clear()
  CALL p1%clear()
  CALL p2%clear()
  CALL p3%clear()
  CALL p4%clear()
  CALL l1%clear()
  CALL l2%clear()
  CALL c1%clear()
  CALL lineAB%clear()
  CALL g0%clear()
  CALL g1%clear()
ENDSUBROUTINE combine_GraphType_old
!
!-------------------------------------------------------------------------------
SUBROUTINE divideEdge_array(this,endpoint_p1,endpoint_p2,intersections)
  CLASS(GraphType),INTENT(INOUT) :: this
  TYPE(PointType),INTENT(IN) :: endpoint_p1
  TYPE(PointType),INTENT(IN) :: endpoint_p2
  TYPE(PointType),INTENT(IN) :: intersections(:)
  !
  INTEGER(SIK) :: i

  IF(SIZE(intersections) > 0) THEN
    CALL this%divideEdge_scalar(endpoint_p1,endpoint_p2,intersections(1))
  ENDIF
  DO i=2,SIZE(intersections)
    CALL this%divideEdge_scalar(intersections(i-1),endpoint_p2,intersections(i))
  ENDDO !i

ENDSUBROUTINE divideEdge_array
!
!-------------------------------------------------------------------------------
SUBROUTINE divideEdge_scalar(this,endpoint_p1,endpoint_p2,intersection)
  CLASS(GraphType),INTENT(INOUT) :: this
  TYPE(PointType),INTENT(IN) :: endpoint_p1
  TYPE(PointType),INTENT(IN) :: endpoint_p2
  TYPE(PointType),INTENT(IN) :: intersection
  !
  INTEGER(SIK) :: endpoint_i1,endpoint_i2

  endpoint_i1=this%getVertIndex(endpoint_p1)
  endpoint_i2=this%getVertIndex(endpoint_p2)

  SELECTCASE(this%edgeMatrix(endpoint_i1,endpoint_i2))
  !No edge, so do nothing
  CASE(GRAPH_NULL_EDGE)
    CONTINUE
  !Linear edge
  CASE(GRAPH_LINEAR_EDGE)
    CALL this%divideEdge(endpoint_p1,endpoint_p2,intersection)
  !Quadratic edge
  CASE(GRAPH_QUADRATIC_EDGE)
    CALL this%divideEdge(endpoint_p1,endpoint_p2,intersection)
  ENDSELECT

ENDSUBROUTINE divideEdge_scalar
!
!-------------------------------------------------------------------------------
SUBROUTINE divideLinearEdge(this,endpoint_p1,endpoint_p2,intersection)
  CLASS(GraphType),INTENT(INOUT) :: this
  TYPE(PointType),INTENT(IN) :: endpoint_p1
  TYPE(PointType),INTENT(IN) :: endpoint_p2
  TYPE(PointType),INTENT(IN) :: intersection
  !
  LOGICAL(SBK) :: lnewPoint
  INTEGER(SIK) :: endpoint_i1,endpoint_i2,intersection_index
  TYPE(PointType) :: new_midpoint

  REQUIRE(endpoint_p1%dim == 2)
  REQUIRE(endpoint_p2%dim == 2)
  REQUIRE(intersection%dim == 2)
  REQUIRE(this%edgeMatrix(this%getVertIndex(endpoint_p1),this%getVertIndex(endpoint_p2)) \
      == GRAPH_LINEAR_EDGE)

  !If the intersection point already exists, get the index
  intersection_index=this%getVertIndex(intersection)
  !If it wasn't found, add it and get the index
  IF(intersection_index == -1) THEN
    lnewPoint=.TRUE.
    CALL this%insertVertex(intersection)
    intersection_index=this%getVertIndex(intersection)
  ELSE
    lnewPoint=.FALSE.
  ENDIF

  !Get the endpoint indexes after inserting new points
  endpoint_i1=this%getVertIndex(endpoint_p1)
  endpoint_i2=this%getVertIndex(endpoint_p2)

  !If the point is new, we know there are no edges, so we can just easily add them
  IF(lnewPoint) THEN
    CALL this%applyLinearSplit(endpoint_i1,endpoint_i2,intersection_index)
  !The point existed before, so we need to do some special handling for pre-existing
  !edges
  ELSE
    !No quadratic edges, so the new linear edges can just be added like normal
    !If any linear edges already existed, then they'll just remain
    IF(ANY(this%edgeMatrix(endpoint_i1,intersection_index) == [GRAPH_NULL_EDGE,GRAPH_LINEAR_EDGE]) .AND. &
        ANY(this%edgeMatrix(intersection_index,endpoint_i2) == [GRAPH_NULL_EDGE,GRAPH_LINEAR_EDGE])) THEN
      CALL this%applyLinearSplit(endpoint_i1,endpoint_i2,intersection_index)
    !At least one edge is quadratic, so we have to do some extra work.  Basically, we'll insert
    !an addition vertex between the point of intersection and the endpoint, then draw 2 linear
    !edges to make the connection.  This prevents overwriting the quadratic edge(s) that already
    !existed
    ELSE
      !There's a quadratic edge here, so get the midpoint and divide again
      IF(this%edgeMatrix(endpoint_i1,intersection_index) == GRAPH_QUADRATIC_EDGE) THEN
        CALL this%getMidPointOnEdge(endpoint_p1,intersection,new_midpoint)
        CALL this%divideQuadraticEdge(endpoint_p1,intersection,new_midpoint)
        CALL new_midpoint%clear()
        CALL this%defineEdge(endpoint_p1,intersection)
        !Get the endpoint indexes after inserting new points
        endpoint_i1=this%getVertIndex(endpoint_p1)
        endpoint_i2=this%getVertIndex(endpoint_p2)
        intersection_index=this%getVertIndex(intersection)
        !No quadratic edge for this part, so just apply the split
      ELSE
        CALL this%defineEdge(endpoint_p1,intersection)
      ENDIF
      !There's a quadratic edge here, so get the midpoint and divide again
      IF(this%edgeMatrix(intersection_index,endpoint_i2) == GRAPH_QUADRATIC_EDGE) THEN
        CALL this%getMidPointOnEdge(intersection,endpoint_p2,new_midpoint)
        CALL this%divideQuadraticEdge(intersection,endpoint_p2,new_midpoint)
        CALL new_midpoint%clear()
        CALL this%defineEdge(endpoint_p1,intersection)
      !No quadratic edge for this part, so just apply the split
      ELSE
        CALL this%defineEdge(intersection,endpoint_p2)
      ENDIF
      CALL this%removeEdge(endpoint_i1,endpoint_i2)
    ENDIF
  ENDIF

ENDSUBROUTINE divideLinearEdge
!
!-------------------------------------------------------------------------------
SUBROUTINE applyLinearSplit(this,endpoint_i1,endpoint_i2,intersection)
  CLASS(GraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: endpoint_i1
  INTEGER(SIK),INTENT(IN) :: endpoint_i2
  INTEGER(SIK),INTENT(IN) :: intersection

  CALL this%removeEdge(endpoint_i1,endpoint_i2)
  CALL this%defineEdge(this%vertices(:,endpoint_i1),this%vertices(:,intersection))
  CALL this%defineEdge(this%vertices(:,intersection),this%vertices(:,endpoint_i2))

ENDSUBROUTINE applyLinearSplit
!
!-------------------------------------------------------------------------------
SUBROUTINE divideQuadraticEdge(this,endpoint_p1,endpoint_p2,intersection)
  CLASS(GraphType),INTENT(INOUT) :: this
  TYPE(PointType),INTENT(IN) :: endpoint_p1
  TYPE(PointType),INTENT(IN) :: endpoint_p2
  TYPE(PointType),INTENT(IN) :: intersection
  !
  LOGICAL(SBK) :: lnewPoint
  INTEGER(SIK) :: endpoint_i1,endpoint_i2,intersection_index
  REAL(SRK) :: radius
  TYPE(PointType) :: new_midpoint,centroid

  REQUIRE(endpoint_p1%dim == 2)
  REQUIRE(endpoint_p2%dim == 2)
  REQUIRE(intersection%dim == 2)
  REQUIRE(this%edgeMatrix(this%getVertIndex(endpoint_p1),this%getVertIndex(endpoint_p2)) == \
      GRAPH_QUADRATIC_EDGE)

  !If the intersection point already exists, get the index
  intersection_index=this%getVertIndex(intersection)
  !If it wasn't found, add it and get the index
  IF(intersection_index == -1) THEN
    lnewPoint=.TRUE.
    CALL this%insertVertex(intersection)
    intersection_index=this%getVertIndex(intersection)
  ELSE
    lnewPoint=.FALSE.
  ENDIF

  !Get the endpoint indexes after inserting new points
  endpoint_i1=this%getVertIndex(endpoint_p1)
  endpoint_i2=this%getVertIndex(endpoint_p2)

  !If the point is new, we know there are no edges, so we can just easily add them
  IF(lnewPoint) THEN
    CALL this%applyQuadraticSplit(endpoint_i1,endpoint_i2,intersection_index)
  !The point existed before, so we need to do some special handling for pre-existing
  !edges
  ELSE
    !No edges, so the new ones can just be added like normal
    IF(this%edgeMatrix(endpoint_i1,intersection_index) == GRAPH_NULL_EDGE .AND. &
        this%edgeMatrix(intersection_index,endpoint_i2) == GRAPH_NULL_EDGE) THEN
      CALL this%applyQuadraticSplit(endpoint_i1,endpoint_i2,intersection_index)
    !At least one part of the edge exists before, so we have to do some extra work.
    !If either pre-existing segment is also quadratic with the same centroid and radius,
    !then we can just leave it.  Otherwise, we'll need to split the segment again
    ELSE
      CALL centroid%init(COORD=this%quadEdges(1:2,endpoint_i1,endpoint_i2))
      radius=this%quadEdges(3,endpoint_i1,endpoint_i2)
      !Check for pre-existing edges from endpoint 1
      SELECTCASE(this%edgeMatrix(endpoint_i1,intersection_index))
      CASE(GRAPH_QUADRATIC_EDGE)
        !Parameters match, so just leave that edge alone
        IF(ALL(centroid%coord .APPROXEQA. this%quadEdges(1:2,endpoint_i1,intersection_index)) .AND. &
            (radius .APPROXEQA. this%quadEdges(3,endpoint_i1,intersection_index))) THEN
          CONTINUE
          !Parameters do not match, so split this new edge
        ELSE
          CALL this%getMidPointOnEdge(endpoint_p1,intersection,new_midpoint)
          CALL this%divideQuadraticEdge(endpoint_p1,intersection,new_midpoint)
          CALL this%defineEdge(endpoint_p1,intersection,centroid,radius)
        ENDIF
      CASE(GRAPH_LINEAR_EDGE)
        CALL this%divideLinearEdge(endpoint_p1,intersection,Midpoint(endpoint_p1,intersection))
        CALL this%defineEdge(endpoint_p1,intersection,centroid,radius)
      CASE(GRAPH_NULL_EDGE)
        CALL this%defineEdge(endpoint_p1,intersection,centroid,radius)
      ENDSELECT
      !Get the endpoint indexes after inserting new points
      endpoint_i1=this%getVertIndex(endpoint_p1)
      endpoint_i2=this%getVertIndex(endpoint_p2)
      intersection_index=this%getVertIndex(intersection)
      !Check for pre-existing edges from endpoint 2
      SELECTCASE(this%edgeMatrix(intersection_index,endpoint_i2))
      CASE(GRAPH_QUADRATIC_EDGE)
        !Parameters match, so just leave that edge alone
        IF(ALL(centroid%coord .APPROXEQA. this%quadEdges(1:2,intersection_index,endpoint_i2)) .AND. &
            (radius .APPROXEQA. this%quadEdges(3,intersection_index,endpoint_i2))) THEN
          CONTINUE
          !Parameters do not match, so split this new edge
        ELSE
          CALL this%getMidPointOnEdge(intersection,endpoint_p2,new_midpoint)
          CALL this%divideQuadraticEdge(intersection,endpoint_p2,new_midpoint)
          CALL this%defineEdge(intersection,endpoint_p2,centroid,radius)
        ENDIF
      CASE(GRAPH_LINEAR_EDGE)
        CALL this%divideLinearEdge(intersection,endpoint_p2,Midpoint(intersection,endpoint_p2))
        CALL this%defineEdge(intersection,endpoint_p2,centroid,radius)
      CASE(GRAPH_NULL_EDGE)
        CALL this%defineEdge(intersection,endpoint_p2,centroid,radius)
      ENDSELECT
      CALL this%removeEdge(endpoint_p1,endpoint_p2)
    ENDIF
  ENDIF

ENDSUBROUTINE divideQuadraticEdge
!
!-------------------------------------------------------------------------------
SUBROUTINE applyQuadraticSplit(this,endpoint_i1,endpoint_i2,intersection)
  CLASS(GraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: endpoint_i1
  INTEGER(SIK),INTENT(IN) :: endpoint_i2
  INTEGER(SIK),INTENT(IN) :: intersection
  !
  REAL(SRK) :: radius
  TYPE(PointType) :: centroid

  CALL centroid%init(COORD=this%quadEdges(1:2,endpoint_i1,endpoint_i2))
  radius=this%quadEdges(3,endpoint_i1,endpoint_i2)
  CALL this%removeEdge(endpoint_i1,endpoint_i2)
  CALL this%defineEdge(this%vertices(:,endpoint_i1),this%vertices(:,intersection), &
      centroid%coord,radius)
  CALL this%defineEdge(this%vertices(:,intersection),this%vertices(:,endpoint_i2), &
      centroid%coord,radius)
  CALL centroid%clear()

ENDSUBROUTINE applyQuadraticSplit
!
!-------------------------------------------------------------------------------
!> @brief Takes cloud of points and computes Delaunay triangulation
!> @param this the graph containing the cloud of points
!>
SUBROUTINE triangulateVerts_graphType(this)
  CLASS(GraphType),INTENT(INOUT) :: this

  INTEGER(SIK) :: nVert,nTri,nEdges,i,j,k
  INTEGER(SIK),ALLOCATABLE :: v1(:),v2(:),v3(:)
  INTEGER(SIK),ALLOCATABLE :: polEdges(:,:)
  REAL(SRK) :: xMin,xMax,yMin,yMax,dx,dy,dm,xMid,yMid,superTri(2,3)
  REAL(SRK) :: x1,x2,x3,y1,y2,y3,r1,r2,r3,a,bx,by,c,rad
  REAL(SRK),ALLOCATABLE :: verts(:,:)
  LOGICAL(SBK), ALLOCATABLE :: complete(:)

  LOGICAL(SBK) :: incircum
  REAL(SRK) :: r,xc,yc

  this%edgeMatrix=0
  nVert=this%nVert()

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
  verts=this%vertices
  !Find the minimum and maximum x-values in the graph
  xMin=this%vertices(1,1)
  yMin=this%vertices(2,1)
  xMax=xMin
  yMax=yMin
  DO i=2,nVert
    xMin=MIN(xMin,this%vertices(1,i))
    xMax=MAX(xMax,this%vertices(1,i))
    yMin=MIN(yMin,this%vertices(2,i))
    yMax=MAX(yMax,this%vertices(2,i))
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
  CALL this%insertVertex(superTri(:,1))
  CALL this%insertVertex(superTri(:,2))
  CALL this%insertVertex(superTri(:,3))

  v1(1)=this%getVertIndex(superTri(:,1))
  v2(1)=this%getVertIndex(superTri(:,2))
  v3(1)=this%getVertIndex(superTri(:,3))
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
        x1=this%vertices(1,v1(j))
        x2=this%vertices(1,v2(j))
        x3=this%vertices(1,v3(j))
        y1=this%vertices(2,v1(j))
        y2=this%vertices(2,v2(j))
        y3=this%vertices(2,v3(j))

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
        v3(nTri)=this%getVertIndex(verts(:,i))
        complete(nTri)=.FALSE.
      ENDIF
    ENDDO
  ENDDO !End loop over each vertex
  !Create edges of final triangulation
  DO i=1,nTri
    CALL this%defineEdge(this%vertices(:,v1(i)), &
        this%vertices(:,v2(i)))
    CALL this%defineEdge(this%vertices(:,v2(i)), &
        this%vertices(:,v3(i)))
    CALL this%defineEdge(this%vertices(:,v3(i)), &
        this%vertices(:,v1(i)))
  ENDDO
  !Remove superTriangle from arrays
  CALL this%removeVertex(superTri(:,1))
  CALL this%removeVertex(superTri(:,2))
  CALL this%removeVertex(superTri(:,3))
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
SUBROUTINE init_DAGraphType(this,n,nodes)
  CLASS(DAGraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: n
  INTEGER(SIK),ALLOCATABLE,INTENT(IN) :: nodes(:)

  IF(.NOT. ALLOCATED(this%nodes) .AND. ALLOCATED(nodes)) THEN
    IF(n > 0) THEN
      IF(SIZE(nodes) == n) THEN
        this%n=n
        ALLOCATE(this%nodes(n))
        ALLOCATE(this%edgeMatrix(n,n))
        this%nodes=nodes
        this%edgeMatrix=0
      ENDIF
    ENDIF
  ENDIF
ENDSUBROUTINE init_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE clear_DAGraphType(this)
  CLASS(DAGraphType),INTENT(INOUT) :: this

  this%n=0
  IF(ALLOCATED(this%nodes)) DEALLOCATE(this%nodes)
  IF(ALLOCATED(this%edgeMatrix)) DEALLOCATE(this%edgeMatrix)
ENDSUBROUTINE clear_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE insertNode_DAGraphType(this,ID,ind)
  CLASS(DAGraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: ID
  INTEGER(SIK),INTENT(IN),OPTIONAL :: ind
  INTEGER(SIK) :: index
  INTEGER(SIK),ALLOCATABLE :: tmpNodes(:),tmpEdges(:,:)

  index=1
  IF(PRESENT(ind)) index=ind
  IF(ALLOCATED(this%edgeMatrix)) THEN
    IF(ALL(this%nodes /= ID)) THEN
      !Store values temporarily
      ALLOCATE(tmpNodes(this%n))
      ALLOCATE(tmpEdges(this%n,this%n))
      tmpNodes=this%nodes
      tmpEdges=this%edgeMatrix
      !Resize arrays on graph type
      DEALLOCATE(this%nodes)
      DEALLOCATE(this%edgeMatrix)
      this%n=this%n+1
      ALLOCATE(this%nodes(this%n))
      ALLOCATE(this%edgeMatrix(this%n,this%n))
      this%edgeMatrix=0
      !reassign old data and assign new node
      this%nodes(1:index-1)=tmpNodes(1:index-1)
      this%nodes(index)=ID
      this%nodes(index+1:this%n)=tmpNodes(index:)
      this%edgeMatrix(1:index-1,1:index-1)=tmpEdges(1:index-1,1:index-1)
      this%edgeMatrix(index+1:,1:index-1)=tmpEdges(index:,1:index-1)
      this%edgeMatrix(1:index-1,index+1:)=tmpEdges(1:index-1,index:)
      this%edgeMatrix(index+1:,index+1:)=tmpEdges(index:,index:)
      DEALLOCATE(tmpNodes)
      DEALLOCATE(tmpEdges)
    ENDIF
  ELSE
    ALLOCATE(this%edgeMatrix(1,1))
    ALLOCATE(this%nodes(1))
    this%edgeMatrix=0
    this%nodes=ID
    this%n=1
  ENDIF
ENDSUBROUTINE insertNode_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE removeNode_DAGraphType(this,ID,ind)
  CLASS(DAGraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN),OPTIONAL :: ID
  INTEGER(SIK),INTENT(IN),OPTIONAL :: ind
  INTEGER(SIK) :: i,index

  index=0
  IF(PRESENT(ID)) THEN
    !Find the index to remove
    DO i=1,this%n
      IF(ID == this%nodes(i)) THEN
        index=i
        EXIT
      ENDIF
    ENDDO
    !If the ID was found, remove the index.
    IF(index > 0) CALL removeNodeByIndex_DAGraphType(this,index)
  ELSEIF(PRESENT(ind)) THEN
    CALL removeNodeByIndex_DAGraphType(this,ind)
  ENDIF
ENDSUBROUTINE removeNode_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE removeNodeByIndex_DAGraphType(this,ind)
  CLASS(DAGraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: ind
  INTEGER(SIK) :: i
  INTEGER(SIK),ALLOCATABLE :: tmpN(:),tmpEdge(:,:)

  IF(ALLOCATED(this%edgeMatrix)) THEN
    IF((1 <= ind) .AND. (ind <= this%n)) THEN
      !Store values temporarily
      i=this%n
      ALLOCATE(tmpN(i))
      ALLOCATE(tmpEdge(i,i))
      tmpN=this%nodes
      tmpEdge=this%edgeMatrix
      !Resize arrays on graph type
      DEALLOCATE(this%nodes)
      DEALLOCATE(this%edgeMatrix)
      this%n=this%n-1
      IF(this%n > 0) THEN
        ALLOCATE(this%nodes(this%n))
        ALLOCATE(this%edgeMatrix(this%n,this%n))
        this%edgeMatrix=0
        !reassign old data and assign new node
        this%nodes(1:ind-1)=tmpN(1:ind-1)
        this%edgeMatrix(1:ind-1,1:ind-1)=tmpEdge(1:ind-1,1:ind-1)
        IF(ind <= this%n) THEN
          this%nodes(ind:this%n)=tmpN(ind+1:)
          this%edgeMatrix(ind:,1:ind-1)=tmpEdge(ind+1:,1:ind-1)
          this%edgeMatrix(1:ind-1,ind:)=tmpEdge(1:ind-1,ind+1:)
          this%edgeMatrix(ind:,ind:)=tmpEdge(ind+1:,ind+1:)
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
FUNCTION isStartNode_DAGraphType(this,ID,ind) RESULT(bool)
  CLASS(DAGraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN),OPTIONAL :: ID
  INTEGER(SIK),INTENT(IN),OPTIONAL :: ind
  INTEGER(SIK) :: index
  LOGICAL(SBK) :: bool

  bool=.FALSE.
  IF(PRESENT(ID)) THEN
    index=this%getIndex(ID)
    IF(ALL(this%edgeMatrix(:,index) == 0)) bool=.TRUE.
  ELSEIF(PRESENT(ind)) THEN
    IF(ALL(this%edgeMatrix(:,ind) == 0)) bool=.TRUE.
  ENDIF
ENDFUNCTION isStartNode_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE getNextStartNode_DAGraphType(this,old,ID)
  CLASS(DAGraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(INOUT) :: old(:)
  INTEGER(SIK),INTENT(OUT) :: ID
  INTEGER(SIK) :: i

  ID=0
  DO i=1,this%n
    IF(this%isStartNode(IND=i) .AND. ALL(this%nodes(i) /= old)) THEN
      ID=this%nodes(i)
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
SUBROUTINE defineEdge_DAGraphType(this,fromID,toID)
  CLASS(DAGraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: fromID
  INTEGER(SIK),INTENT(IN) :: toID
  INTEGER(SIK) :: fromInd,toInd

  IF(ALLOCATED(this%edgeMatrix)) THEN
    fromInd=this%getIndex(fromID)
    toInd=this%getIndex(toID)
    IF((fromInd > 0) .AND. (toInd > 0)) this%edgeMatrix(fromInd,toInd)=1
  ENDIF
ENDSUBROUTINE defineEdge_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE removeEdge_DAGraphType(this,fromID,toID)
  CLASS(DAGraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: fromID
  INTEGER(SIK),INTENT(IN) :: toID
  INTEGER(SIK) :: fromInd,toInd

  IF(ALLOCATED(this%edgeMatrix)) THEN
    fromInd=this%getIndex(fromID)
    toInd=this%getIndex(toID)
    IF((fromInd > 0) .AND. (toInd > 0)) this%edgeMatrix(fromInd,toInd)=0
  ENDIF
ENDSUBROUTINE removeEdge_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
FUNCTION getIndex_DAGraphType(this,ID) RESULT(ind)
  CLASS(DAGraphType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: ID
  INTEGER(SIK) :: i,ind

  ind=0
  IF(ALLOCATED(this%edgeMatrix)) THEN
    DO i=1,this%n
      IF(this%nodes(i) == ID) THEN
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
SUBROUTINE KATS_DAGraphType(this)
  CLASS(DAGraphType),INTENT(INOUT) :: this
  TYPE(DAGraphType) :: sortedGraph
  INTEGER(SIK) :: i,ID
  INTEGER(SIK),ALLOCATABLE :: oldIDs(:)

  ID=0
  ALLOCATE(oldIDs(this%n))
  oldIDs=0
  CALL this%getNextStartNode(oldIDs,ID)

  DO WHILE(ID /= 0)
    CALL sortedGraph%insertNode(ID,1)
    DO i=1,this%n
      CALL this%removeEdge(ID,this%nodes(i))
    ENDDO
    CALL this%getNextStartNode(oldIDs,ID)
  ENDDO
  IF(ALL(this%edgeMatrix == 0)) THEN
    SELECTTYPE(this); TYPE IS(DAGraphtype)
      this=sortedGraph
    ENDSELECT
  ENDIF
  DEALLOCATE(oldIDs)
ENDSUBROUTINE KATS_DAGraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
SUBROUTINE assign_DAGraphType(g0,g1)
  CLASS(DAGraphType),INTENT(INOUT) :: g0
  CLASS(DAGraphType),INTENT(IN) :: g1
  CALL clear_DAGraphType(g0)
  IF(g1%n > 0) THEN
    g0%n=g1%n
    ALLOCATE(g0%nodes(g1%n))
    ALLOCATE(g0%edgeMatrix(g1%n,g1%n))
    g0%nodes=g1%nodes
    g0%edgeMatrix=g1%edgeMatrix
  ENDIF
ENDSUBROUTINE assign_DAGraphType
!
ENDMODULE Geom_Graph
