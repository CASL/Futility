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
  PUBLIC :: ASSIGNMENT(=)
  PUBLIC :: OPERATOR(==)
  !PUBLIC :: OPERATOR(+)

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
      !> @copybrief Geom_Graph::getMidPointOnEdge_graphType
      !> @copydetails Geom_Graph::getMidPointOnEdge_graphType
      PROCEDURE,PASS :: getMidPointOnEdge => getMidPointOnEdge_graphType
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
      !> @copybrief Geom_Graph::combine_graphType
      !> @copydetails Geom_Graph::combine_graphType
      PROCEDURE,PASS :: combineGraph => combine_graphType
      !> @copybrief Geom_Graph::triangulateVerts_graphType
      !> @copydetails Geom_Graph::triangulateVerts_graphType
      PROCEDURE,PASS :: triangulateVerts => triangulateVerts_graphType
      !> @copybrief Geom_Graph::clear_graphType
      !> @copydetails Geom_Graph::clear_graphType
      PROCEDURE,PASS :: clear => clear_graphType
  ENDTYPE GraphType

  INTERFACE ASSIGNMENT(=)
    !> @copybrief Geom_Graph::assign_graphType
    !> @copydetails Geom_Graph::assign_graphType
    MODULE PROCEDURE assign_graphType
  ENDINTERFACE

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
    PURE SUBROUTINE getMidPointOnEdge_graphType(thisGraph,v1,v2,m)
      CLASS(GraphType),INTENT(INOUT) :: thisGraph
      INTEGER(SIK),INTENT(IN) :: v1
      INTEGER(SIK),INTENT(IN) :: v2
      REAL(SRK),INTENT(INOUT) :: m(2)

      INTEGER(SIK) :: n
      REAL(SRK) :: a(2),b(2),c(2),r,alp1,alp2,theta,scal

      m=-HUGE(m)
      n=nVert_graphType(thisGraph)+1
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
    ENDSUBROUTINE getMidPointOnEdge_graphType
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
    SUBROUTINE combine_GraphType(thisGraph,g)
      CLASS(GraphType),INTENT(INOUT) :: thisGraph
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

      SELECTTYPE(thisGraph); TYPE IS(GraphType)
        CALL assign_GraphType(g0,thisGraph)
      ENDSELECT
      CALL assign_GraphType(g1,g)
      CALL l1%p1%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL l1%p2%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL l2%p1%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL l2%p2%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL p0%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      DO WHILE(nEdge_graphType(g1) > 0)
        n=nVert_graphType(g1)
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
          CALL insertVertex_graphType(lineAB,a)
          CALL insertVertex_graphType(lineAB,b)
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

          n=nVert_graphType(thisGraph)
          DO i=1,n
            DO j=i+1,n
              IF(thisGraph%edgeMatrix(j,i) /= 0) THEN
                c=thisGraph%vertices(:,i)
                d=thisGraph%vertices(:,j)
              ENDIF
              IF(thisGraph%edgeMatrix(j,i) == 1) THEN
                l2%p1%dim=2; l2%p2%dim=2
                l2%p1%coord=c
                l2%p2%coord=d
                IF(c1%r == 0.0_SRK) THEN
                  p1=l1%intersectLine(l2)
                  IF(p1%dim == 2) THEN
                    CALL removeEdge_graphType(g0,c,d)
                    CALL insertVertex_graphType(g0,p1%coord)
                    CALL defineEdge_graphType(g0,c,p1%coord)
                    CALL defineEdge_graphType(g0,d,p1%coord)
                    CALL insertVertex_graphType(lineAB,p1%coord)
                  ENDIF
                ELSE
                  !circle-line
                  CALL c1%intersectLine(l2,p1,p2)

                  !Count tangent points
                  IF(p1%dim == -3) p1%dim=2

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
                    CALL removeEdge_graphType(g0,c,d)
                    CALL insertVertex_graphType(g0,p1%coord)
                    CALL insertVertex_graphType(g0,p2%coord)
                    !Cord intersecting circle
                    CALL defineEdge_graphType(g0,p1%coord,p2%coord)
                    CALL defineEdge_graphType(g0,c,p1%coord) !is p1 always closer to c?
                    CALL defineEdge_graphType(g0,d,p2%coord) !is p2 always closer to d?
                    CALL insertVertex_graphType(lineAB,p1%coord)
                    CALL insertVertex_graphType(lineAB,p2%coord)

                    !Add midpoint of arc (keeps graph sane)
                    !Need to store edge information here, because normal point sorting
                    !on graph type does not implicitly keep points ordered for arcs
                    CALL insertVertex_graphType(lineAB,m)
                    CALL defineEdge_graphType(lineAB,m,p1%coord)
                    CALL defineEdge_graphType(lineAB,m,p2%coord)
                  ELSE
                    IF(p1%dim /= 2 .AND. p2%dim == 2) p1=p2
                    IF(p1%dim == 2) THEN
                      CALL removeEdge_graphType(g0,c,d)
                      CALL insertVertex_graphType(g0,p1%coord)
                      CALL defineEdge_graphType(g0,c,p1%coord)
                      CALL defineEdge_graphType(g0,d,p1%coord)
                      CALL insertVertex_graphType(lineAB,p1%coord)
                    ENDIF
                  ENDIF
                ENDIF
              ELSEIF(thisGraph%edgeMatrix(j,i) == -1) THEN
                p0%coord=thisGraph%quadEdges(1:2,i,j)
                r=thisGraph%quadEdges(3,i,j)
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
                  CALL c2%intersectLine(l1,p1,p2)

                  !Count tangent points
                  IF(p1%dim == -3) p1%dim=2

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
                    
                    CALL removeEdge_graphType(g0,c,d)
                    CALL insertVertex_graphType(g0,p1%coord)
                    CALL insertVertex_graphType(g0,p2%coord)
                    
                    !Add midpoint of arc (keeps graph sane)
                    !p1 and p2 are connected by a straight point and an arc
                    CALL insertVertex_graphType(g0,m)
                    CALL defineQuadEdge_graphType(g0,m,p1%coord,c2%c%coord,c2%r)
                    CALL defineQuadEdge_graphType(g0,m,p2%coord,c2%c%coord,c2%r)
                    
                    !Cord intersecting circle
                    CALL defineEdge_graphType(g0,p1%coord,p2%coord)
                    !is p1 always closer to c?
                    CALL defineQuadEdge_graphType(g0,c,p1%coord,c2%c%coord,c2%r)
                    !is p2 always closer to d?
                    CALL defineQuadEdge_graphType(g0,d,p2%coord,c2%c%coord,c2%r)
                    CALL insertVertex_graphType(lineAB,p1%coord)
                    CALL insertVertex_graphType(lineAB,p2%coord)
                  ELSE
                    IF(p1%dim /= 2 .AND. p2%dim == 2) p1=p2
                    IF(p1%dim == 2) THEN
                      CALL removeEdge_graphType(g0,c,d)
                      CALL insertVertex_graphType(g0,p1%coord)
                      CALL defineQuadEdge_graphType(g0,c,p1%coord,c2%c%coord,c2%r)
                      CALL defineQuadEdge_graphType(g0,d,p1%coord,c2%c%coord,c2%r)
                      CALL insertVertex_graphType(lineAB,p1%coord)
                    ENDIF
                  ENDIF
                ELSE
                  !circle-circle (F-this)

                ENDIF
              ENDIF
            ENDDO
          ENDDO
          IF(c1%r == 0.0_SRK) THEN
            CALL insertVertex_graphType(g0,lineAB%vertices(:,1))
            DO i=2,nVert_graphType(lineAB)
              CALL insertVertex_graphType(g0,lineAB%vertices(:,i))
              CALL defineEdge_graphType(g0,lineAB%vertices(:,i-1), &
                lineAB%vertices(:,i)) 
            ENDDO
          ELSE
            !Sort vertices in clock-wise order.
            n=nVert_graphType(lineAB)
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
            CALL insertVertex_graphType(g0,lineAB%vertices(:,cwVerts(1)))
            DO i=2,nVert_graphType(lineAB)
              CALL insertVertex_graphType(g0,lineAB%vertices(:,cwVerts(i)))
              CALL defineQuadEdge_graphType(g0,lineAB%vertices(:,cwVerts(i-1)), &
                lineAB%vertices(:,cwVerts(i)),c1%c%coord,c1%r)
            ENDDO
            DEALLOCATE(vTheta,cwVerts)
          ENDIF
          SELECTTYPE(thisGraph); TYPE IS(GraphType)
            CALL assign_GraphType(thisGraph,g0)
          ENDSELECT
          !CALL editToVTK_graphType(thisGraph,'tmpG.vtk')
          CALL c1%clear()
          CALL clear_graphType(lineAB)
        ENDIF

        !Remove the edge
        CALL removeEdge_IJ_graphType(g1,v1,v2)
        !Remove any isoloated vertices
        nAdj=nAdjacent_graphType(g1,v1)
        IF(nAdj == 0) CALL removeVertex_idx_graphType(g1,v1)
        v2=getVertIndex_graphType(g1,b)
        nAdj=nAdjacent_graphType(g1,v2)
        IF(nAdj == 0) CALL removeVertex_idx_graphType(g1,v2)
      ENDDO
      CALL p0%clear()
      CALL p1%clear()
      CALL p2%clear()
      CALL p3%clear()
      CALL p4%clear()
      CALL l1%clear()
      CALL l2%clear()
      CALL c1%clear()
      CALL clear_graphType(lineAB)
      CALL clear_graphType(g0)
      CALL clear_graphType(g1)
    ENDSUBROUTINE combine_GraphType
!
!-------------------------------------------------------------------------------
!> @brief Takes cloud of points and computes Delaunay triangulation
!> @param thisGraph the graph containing the cloud of points
!>
    SUBROUTINE triangulateVerts_graphType(thisGraph)
      CLASS(GraphType),INTENT(INOUT) :: thisGraph

      TYPE(PointType) :: p0,p1,PointI
      TYPE(CircleType) :: circumCirc
      INTEGER(SIK) :: nVert,nTri,nEdges,i,j,k
      INTEGER(SIK),ALLOCATABLE :: v1(:),v2(:),v3(:)
      INTEGER(SIK),ALLOCATABLE :: polEdges(:,:)
      REAL(SRK) :: xMin,xMax,yMin,yMax,dx,dy,dm,xMid,yMid,superTri(2,3)
      REAL(SRK) :: x1,x2,x3,y1,y2,y3,r1,r2,r3,x0,y0,a,bx,by,c,rad
      REAL(SRK),ALLOCATABLE :: verts(:,:)
      LOGICAL(SBK), ALLOCATABLE :: complete(:)

      LOGICAL(SBK) :: incircum
      REAL(SRK) :: drsqr, m1,m2,mx1,mx2,my1,my2,r,rsqr,xc,yc
      CHARACTER(len=22) :: tmpname
      
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
        CALL thisGraph%defineEdge(thisGraph%vertices(:,v1(i)), & 
          thisGraph%vertices(:,v2(i)))
        CALL thisGraph%defineEdge(thisGraph%vertices(:,v2(i)), & 
          thisGraph%vertices(:,v3(i)))
        CALL thisGraph%defineEdge(thisGraph%vertices(:,v3(i)), & 
          thisGraph%vertices(:,v1(i)))
      ENDDO
      !Remove superTriangle from arrays
      CALL thisGraph%removeVertex(superTri(:,1))
      CALL thisGraph%removeVertex(superTri(:,2))
      CALL thisGraph%removeVertex(superTri(:,3))
    ENDSUBROUTINE triangulateVerts_graphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
!>
    SUBROUTINE assign_GraphType(g0,g1)
      TYPE(GraphType),INTENT(INOUT) :: g0
      TYPE(GraphType),INTENT(IN) :: g1
      INTEGER(SIK) :: n
      CALL clear_graphType(g0)
      n=nVert_GraphType(g1)
      IF(n > 0) THEN
        CALL dmallocA(g0%vertices,2,n)
        CALL dmallocA(g0%edgeMatrix,n,n)
        CALL dmallocA(g0%quadEdges,3,n,n)
        g0%vertices=g1%vertices
        g0%edgeMatrix=g1%edgeMatrix
        g0%quadEdges=g1%quadEdges
      ENDIF
    ENDSUBROUTINE assign_GraphType
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
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
!!
!!-------------------------------------------------------------------------------
!!> @brief
!!> @param
!!>
!!>
!!>
!    FUNCTION add_GraphType(g0,g1) RESULT(g)
!      TYPE(GraphType),INTENT(IN) :: g0
!      TYPE(GraphType),INTENT(IN) :: g1
!      TYPE(GraphType) :: g
!      INTEGER(SIK) :: i,j,n
!      
!      CALL clear_graphType(g)
!      g=g0
!      n=nVert_graphType(g1)
!      DO i=1,n
!        CALL insertVertex_graphType(g,g1%vertices(:,i))
!      ENDDO
!      DO j=1,n
!        DO i=j+1,n
!          IF(g1%edgeMatrix(i,j) == 1) THEN
!            CALL defineEdge_graphType(g,g1%vertices(:,i),g1%vertices(:,j))
!          ELSEIF(g1%edgeMatrix(i,j) == -1) THEN
!            CALL defineQuadEdge_graphType(g,g1%vertices(:,i),g1%vertices(:,j), &
!              g1%quadEdges(1:2,i,j),g1%quadEdges(3,i,j))
!          ENDIF
!        ENDDO
!      ENDDO
!    ENDFUNCTION add_GraphType
!
ENDMODULE Geom_Graph
