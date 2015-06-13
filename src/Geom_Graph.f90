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
!> @brief A Fortran 2003 module defining a graph type.
!>
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref Allocs "Allocs": @copybrief Allocs
!>
!> @author Brendan Kochunas
!>    @date 06/06/2015
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom_Graph
  USE IntrType
  USE Allocs
  USE VTKFiles

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: GraphType
  PUBLIC :: OPERATOR(+)

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
    CONTAINS
      !> @copybrief Geom_Graph::nVert_graphType
      !> @copydetails Geom_Graph::nVert_graphType
      PROCEDURE,PASS :: nVert => nVert_graphType
      !> @copybrief Geom_Graph::nEdge_graphType
      !> @copydetails Geom_Graph::nEdge_graphType
      PROCEDURE,PASS :: nEdge => nEdge_graphType
      !> @copybrief Geom_Graph::getVertIndex_graphType
      !> @copydetails Geom_Graph::getVertIndex_graphType
      PROCEDURE,PASS :: getVertIndex => getVertIndex_graphType
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
      !> @copybrief Geom_Graph::isMinimumCycle_graphType
      !> @copydetails Geom_Graph::isMinimumCycle_graphType
      PROCEDURE,PASS :: isMinimumCycle => isMinimumCycle_graphType
      !> @copybrief Geom_Graph::insertVertex_graphType
      !> @copydetails Geom_Graph::insertVertex_graphType
      PROCEDURE,PASS :: insertVertex => insertVertex_graphType
      !> @copybrief Geom_Graph::defineEdge_graphType
      !> @copydetails Geom_Graph::defineEdge_graphType
      PROCEDURE,PASS :: defineEdge => defineEdge_graphType
      !> @copybrief Geom_Graph::defineQuadEdge_graphType
      !> @copydetails Geom_Graph::defineQuadEdge_graphType
      PROCEDURE,PASS :: defineQuadraticEdge => defineQuadEdge_graphType
      !> @copybrief Geom_Graph::removeVertex_graphType
      !> @copydetails Geom_Graph::removeVertex_graphType
      PROCEDURE,PASS :: removeVertex => removeVertex_graphType
      !> @copybrief Geom_Graph::removeVertex_idx_graphType
      !> @copydetails Geom_Graph::removeVertex_idx_graphType
      PROCEDURE,PASS :: removeVertexI => removeVertex_idx_graphType
      !> @copybrief Geom_Graph::removeEdge_graphType
      !> @copydetails Geom_Graph::removeEdge_graphType
      PROCEDURE,PASS :: removeEdge => removeEdge_graphType
      !> @copybrief Geom_Graph::removeVertex_idx_graphType
      !> @copydetails Geom_Graph::removeVertex_idx_graphType
      PROCEDURE,PASS :: removeEdgeIJ => removeEdge_IJ_graphType
      !> @copybrief Geom_Graph::removeFilament_vertIdx_graphType
      !> @copydetails Geom_Graph::removeFilament_vertIdx_graphType
      PROCEDURE,PASS :: removeFilamentFromVert => removeFilament_vertIdx_graphType
      !> @copybrief Geom_Graph::getMCB_graphType
      !> @copydetails Geom_Graph::getMCB_graphType
      PROCEDURE,PASS :: getMCB => getMCB_graphType
      !> @copybrief Geom_Graph::editToVTK_graphType
      !> @copydetails Geom_Graph::editToVTK_graphType
      PROCEDURE,PASS :: editToVTK => editToVTK_graphType
      !> @copybrief Geom_Graph::clear_graphType
      !> @copydetails Geom_Graph::clear_graphType
      PROCEDURE,PASS :: clear => clear_graphType
  ENDTYPE GraphType

  INTERFACE OPERATOR(+)
    !> @copybrief Geom_Graph::add_graphType
    !> @copydetails Geom_Graph::add_graphType
    MODULE PROCEDURE add_graphType
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
    ELEMENTAL FUNCTION nVert_graphType(thisGraph) RESULT(n)
      CLASS(GraphType),INTENT(IN) :: thisGraph
      INTEGER(SIK) :: n
      n=0
      IF(ALLOCATED(thisGraph%vertices)) n=SIZE(thisGraph%vertices,DIM=2)
    ENDFUNCTION nVert_graphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
    ELEMENTAL FUNCTION nEdge_graphType(thisGraph) RESULT(n)
      CLASS(GraphType),INTENT(IN) :: thisGraph
      INTEGER(SIK) :: n
      n=0
      IF(ALLOCATED(thisGraph%edgeMatrix)) n=SUM(ABS(thisGraph%edgeMatrix))/2
    ENDFUNCTION nEdge_graphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
    PURE FUNCTION getVertIndex_graphType(thisGraph,coord) RESULT(idx)
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
              IF(coord(2) .APPROXEQA. thisGraph%vertices(2,j)) THEN
                idx=j
                EXIT
              ENDIF
            ENDDO
          ENDIF
          EXIT
        ENDIF
      ENDDO
    ENDFUNCTION getVertIndex_graphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
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
!> @brief
!> @param
!>
!>
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
            dcurr=thisGraph%vertices(:,vCurr)-(/0.0_SRK,-1.0_SRK/)
            IF(vPrev > 0) dcurr=thisGraph%vertices(:,vCurr)- &
              thisGraph%vertices(:,vPrev)
            dnext=thisGraph%vertices(:,vNext)-thisGraph%vertices(:,vCurr)
            isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) <= 0.0_SRK)
            DO i=1,nAdj
              vi=getAdjacentVert_graphType(thisGraph,vCurr,i)
              IF(vi /= vPrev .AND. vi /= vNext) THEN
                di=thisGraph%vertices(:,vi)-thisGraph%vertices(:,vCurr)
                IF(isVCurrConvex) THEN
                  IF(dcurr(1)*di(2)-dcurr(2)*di(1) < 0.0_SRK .OR. &
                      dnext(1)*di(2)-dnext(2)*di(1) < 0.0_SRK) THEN
                    vNext=vi
                    dnext=di
                    isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) <= 0.0_SRK)
                  ENDIF
                ELSE
                  IF(dcurr(1)*di(2)-dcurr(2)*di(1) < 0.0_SRK .AND. &
                      dnext(1)*di(2)-dnext(2)*di(1) < 0.0_SRK) THEN
                    vNext=vi
                    dnext=di
                    isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) <= 0.0_SRK)
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
            dcurr=thisGraph%vertices(:,vCurr)-(/0.0_SRK,-1.0_SRK/)
            IF(vPrev > 0) dcurr=thisGraph%vertices(:,vCurr)- &
              thisGraph%vertices(:,vPrev)
            dnext=thisGraph%vertices(:,vNext)-thisGraph%vertices(:,vCurr)
            isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) <= 0.0_SRK)
            DO i=1,nAdj
              vi=getAdjacentVert_graphType(thisGraph,vCurr,i)
              IF(vi /= vPrev .AND. vi /= vNext) THEN
                di=thisGraph%vertices(:,vi)-thisGraph%vertices(:,vCurr)
                IF(isVCurrConvex) THEN
                  IF(dcurr(1)*di(2)-dcurr(2)*di(1) > 0.0_SRK .AND. &
                      dnext(1)*di(2)-dnext(2)*di(1) > 0.0_SRK) THEN
                    vNext=vi
                    dnext=di
                    isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) <= 0.0_SRK)
                  ENDIF
                ELSE
                  IF(dcurr(1)*di(2)-dcurr(2)*di(1) > 0.0_SRK .OR. &
                      dnext(1)*di(2)-dnext(2)*di(1) > 0.0_SRK) THEN
                    vNext=vi
                    dnext=di
                    isVCurrConvex=(dnext(1)*dcurr(2)-dnext(2)*dcurr(1) <= 0.0_SRK)
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
!> @brief
!> @param
!>
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
!> @brief
!> @param
!>
!>
!>
    SUBROUTINE insertVertex_graphType(thisGraph,coord)
      CLASS(GraphType),INTENT(INOUT) :: thisGraph
      REAL(SRK),INTENT(IN) :: coord(2)
      INTEGER(SIK) :: i,j,n,k
      INTEGER(SIK),ALLOCATABLE :: tmpE(:,:)
      REAL(SRK),ALLOCATABLE :: tmpVertices(:,:),tmpQE(:,:,:)
      IF(ALLOCATED(thisGraph%vertices)) THEN
        n=SIZE(thisGraph%vertices,DIM=2)
        CALL dmallocA(tmpVertices,2,n+1)
        j=0
        DO i=1,n
          IF(coord(1) <= thisGraph%vertices(1,i)) THEN
            IF(coord(1) .APPROXEQA. thisGraph%vertices(1,i)) THEN
              IF(coord(2) .APPROXEQA. thisGraph%vertices(2,i)) THEN
                k=-1 !Duplicate vertex
              ELSEIF(coord(2) < thisGraph%vertices(2,i)) THEN                
                k=i !Before i
              ELSE
                !After i
                DO j=i+1,n
                  !Find index for end of sequence with same x value
                  IF(.NOT.(coord(1) .APPROXEQA. thisGraph%vertices(1,j))) EXIT
                ENDDO
                !Search on y through sequence of same x
                DO k=i+1,j-1
                  IF(coord(2) < thisGraph%vertices(2,k)) EXIT
                ENDDO
              ENDIF
            ELSE
              k=i !Before i
            ENDIF
            EXIT
          ENDIF
        ENDDO
        IF(j /= 0) i=j
        IF(i == n+1) THEN
          k=n+1 !Last point
          i=n
        ENDIF
        IF(k > 0) THEN
          IF(k > 1) tmpVertices(:,1:k-1)=thisGraph%vertices(:,1:i-1)
          tmpVertices(:,k)=coord
          tmpVertices(:,k+1:n+1)=thisGraph%vertices(:,i:n)
          CALL demallocA(thisGraph%vertices)
          CALL MOVE_ALLOC(tmpVertices,thisGraph%vertices)
          
          !Expand Edge Matrices
          CALL dmallocA(tmpE,n+1,n+1)
          CALL dmallocA(tmpQE,3,n+1,n+1)
          DO j=1,k-1
            DO i=1,k-1
              tmpE(i,j)=thisGraph%edgeMatrix(i,j)
              tmpE(j,i)=thisGraph%edgeMatrix(j,i)
              tmpQE(:,i,j)=thisGraph%quadEdges(:,i,j)
              tmpQE(:,j,i)=thisGraph%quadEdges(:,j,i)
            ENDDO
            DO i=k+1,n+1
              tmpE(i,j)=thisGraph%edgeMatrix(i-1,j)
              tmpE(j,i)=thisGraph%edgeMatrix(j,i-1)
              tmpQE(:,i,j)=thisGraph%quadEdges(:,i-1,j)
              tmpQE(:,j,i)=thisGraph%quadEdges(:,j,i-1)
            ENDDO
          ENDDO
          DO j=k+1,n+1
            DO i=1,k-1
              tmpE(i,j)=thisGraph%edgeMatrix(i,j-1)
              tmpE(j,i)=thisGraph%edgeMatrix(j-1,i)
              tmpQE(:,i,j)=thisGraph%quadEdges(:,i,j-1)
              tmpQE(:,j,i)=thisGraph%quadEdges(:,j-1,i)
            ENDDO
            DO i=k+1,n+1
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
!> @brief
!> @param
!>
!>
!>
    PURE SUBROUTINE defineEdge_graphType(thisGraph,coord1,coord2)
      CLASS(GraphType),INTENT(INOUT) :: thisGraph
      REAL(SRK),INTENT(IN) :: coord1(2)
      REAL(SRK),INTENT(IN) :: coord2(2)
      INTEGER(SIK) :: v1,v2
      v1=getVertIndex_graphType(thisGraph,coord1)
      v2=getVertIndex_graphType(thisGraph,coord2)
      IF(v1 > 0 .AND. v2 > 0 .AND. v1 /= v2) THEN
        thisGraph%edgeMatrix(v1,v2)=1
        thisGraph%edgeMatrix(v2,v1)=1
      ENDIF
    ENDSUBROUTINE defineEdge_graphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
    PURE SUBROUTINE defineQuadEdge_graphType(thisGraph,coord1,coord2,c0,r)
      CLASS(GraphType),INTENT(INOUT) :: thisGraph
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
          v1=getVertIndex_graphType(thisGraph,coord1)
          v2=getVertIndex_graphType(thisGraph,coord2)
          IF(v1 > 0 .AND. v2 > 0 .AND. v1 /= v2) THEN
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
    ENDSUBROUTINE defineQuadEdge_graphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
    SUBROUTINE removeVertex_graphType(thisGraph,v)
      CLASS(GraphType),INTENT(INOUT) :: thisGraph
      REAL(SRK),INTENT(IN) :: v(2)
      INTEGER(SIK) :: i
      i=getVertIndex_graphType(thisGraph,v)
      CALL removeVertex_idx_graphType(thisGraph,i)
    ENDSUBROUTINE removeVertex_graphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
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
        CALL MOVE_ALLOC(tmpVert,thisGraph%vertices)
        CALL MOVE_ALLOC(tmpEdge,thisGraph%edgeMatrix)
        CALL MOVE_ALLOC(tmpQE,thisGraph%quadEdges)
      ENDIF
    ENDSUBROUTINE removeVertex_idx_graphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
    PURE SUBROUTINE removeEdge_graphType(thisGraph,c1,c2)
      CLASS(GraphType),INTENT(INOUT) :: thisGraph
      REAL(SRK),INTENT(IN) :: c1(2)
      REAL(SRK),INTENT(IN) :: c2(2)
      INTEGER(SIK) :: v1,v2

      v1=getVertIndex_graphType(thisGraph,c1)
      v2=getVertIndex_graphType(thisGraph,c2)
      IF(v1 > 0 .AND. v2 > 0) THEN
        thisGraph%edgeMatrix(v1,v2)=0
        thisGraph%edgeMatrix(v2,v1)=0
        thisGraph%quadEdges(:,v1,v2)=0.0_SRK
        thisGraph%quadEdges(:,v2,v1)=0.0_SRK
      ENDIF
    ENDSUBROUTINE removeEdge_graphType
   
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
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
              v0=getVertIndex_graphType(thisGraph,xy)
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
            v0=getVertIndex_graphType(thisGraph,xy)
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
          CALL defineEdge_graphType(subgraph,coord1,coord2)
        ELSEIF(thisGraph%edgeMatrix(vPrev,vCurr) == -1) THEN
          c0=thisGraph%quadEdges(1:2,vPrev,vCurr)
          r=thisGraph%quadEdges(3,vPrev,vCurr)
          CALL defineQuadEdge_graphType(subgraph,coord1,coord2,c0,r)
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
          CALL defineEdge_graphType(subgraph,coord1,coord2)
        ELSEIF(thisGraph%edgeMatrix(vPrev,vCurr) == -1) THEN
          c0=thisGraph%quadEdges(1:2,vPrev,vCurr)
          r=thisGraph%quadEdges(3,vPrev,vCurr)
          CALL defineQuadEdge_graphType(subgraph,coord1,coord2,c0,r)
        ENDIF

        n=nVert_graphType(subGraph)
        DO i=1,n
          j=i+1
          IF(i == n) j=1
          vCurr=getVertIndex_graphType(thisGraph,subgraph%vertices(:,i))
          vNext=getVertIndex_graphType(thisGraph,subgraph%vertices(:,j))
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
            vCurr=getAdjacentVert_graphType(thisGraph,vCurr,1)
          ELSE
            vCurr=getAdjacentVert_graphType(thisGraph,vCurr,2)
          ENDIF
          vNext=vCurr
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
          CALL clear_graphType(cycles(i))
        ENDDO
        DEALLOCATE(cycles)
      ENDIF
      SELECTTYPE(thisGraph); TYPE IS(GraphType)
        g=thisGraph
      ENDSELECT
      n=nVert_graphType(thisGraph)
      CALL dmallocA(g%isCycleEdge,n,n)
      ncycles=0
      DO WHILE(nVert_graphType(g) > 0)
        CALL editToVTK_graphType(g,'testMCB.vtk')
        nadj=nAdjacent_graphType(g,1)
        IF(nadj == 0) THEN
          CALL removeVertex_idx_graphType(g,1)
        ELSEIF(nadj == 1) THEN
          CALL removeFilament_vertIdx_graphType(g,1,1)
        ELSE
          CALL extractPrimitive_graphType(g,1,primeGraph)
          IF(isMinimumCycle_graphType(primeGraph)) THEN
            !Found minimum cycle, so add it to basis
            ncycles=ncycles+1
            ALLOCATE(tmpCycles(ncycles))
            DO i=1,ncycles-1
              tmpCycles(i)=cycles(i)
              CALL clear_graphType(cycles(i))
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
!> @brief
!> @param
!>
!>
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
!> @brief
!> @param
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
!> @brief
!> @param
!>
!>
!>
    FUNCTION add_GraphType(g0,g1) RESULT(g)
      TYPE(GraphType),INTENT(IN) :: g0
      TYPE(GraphType),INTENT(IN) :: g1
      TYPE(GraphType) :: g
      INTEGER(SIK) :: i,j,n
      
      CALL clear_graphType(g)
      g=g0
      n=nVert_graphType(g1)
      DO i=1,n
        CALL insertVertex_graphType(g,g1%vertices(:,i))
      ENDDO
      DO j=1,n
        DO i=j+1,n
          IF(g1%edgeMatrix(i,j) == 1) THEN
            CALL defineEdge_graphType(g,g1%vertices(:,i),g1%vertices(:,j))
          ELSEIF(g1%edgeMatrix(i,j) == -1) THEN
            CALL defineQuadEdge_graphType(g,g1%vertices(:,i),g1%vertices(:,j), &
              g1%quadEdges(1:2,i,j),g1%quadEdges(3,i,j))
          ENDIF
        ENDDO
      ENDDO
    ENDFUNCTION add_GraphType
!
ENDMODULE Geom_Graph
