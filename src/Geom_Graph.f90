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
  
  IMPLICIT NONE
  PRIVATE
  
  PUBLIC :: GraphType
  
  !> @brief a Planar Graph
  TYPE :: GraphType
    !>
    REAL(SRK),ALLOCATABLE :: vertices(:,:)
    !>
    INTEGER(SIK),ALLOCATABLE :: edgeMatrix(:,:)
    !>
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
      !> @copybrief Geom_Graph::insertVertex_graphType
      !> @copydetails Geom_Graph::insertVertex_graphType
      PROCEDURE,PASS :: insertVertex => insertVertex_graphType
      !> @copybrief Geom_Graph::defineEdge_graphType
      !> @copydetails Geom_Graph::defineEdge_graphType
      PROCEDURE,PASS :: defineEdge => defineEdge_graphType
      !> @copybrief Geom_Graph::defineQuadEdge_graphType
      !> @copydetails Geom_Graph::defineQuadEdge_graphType
      PROCEDURE,PASS :: defineQuadraticEdge => defineQuadEdge_graphType
      !> @copybrief Geom_Graph::getMCB_graphType
      !> @copydetails Geom_Graph::getMCB_graphType
      PROCEDURE,PASS :: getMCB => getMCB_graphType
      !> @copybrief Geom_Graph::clear_graphType
      !> @copydetails Geom_Graph::clear_graphType
      PROCEDURE,PASS :: clear => clear_graphType
  ENDTYPE GraphType
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
    SUBROUTINE defineEdge_graphType(thisGraph,coord1,coord2)
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
    SUBROUTINE defineQuadEdge_graphType(thisGraph,coord1,coord2,c0,r)
      CLASS(GraphType),INTENT(INOUT) :: thisGraph
      REAL(SRK),INTENT(IN) :: coord1(2)
      REAL(SRK),INTENT(IN) :: coord2(2)
      REAL(SRK),INTENT(IN) :: c0(2)
      REAL(SRK),INTENT(IN) :: r
      
      INTEGER(SIK) :: v1,v2
      REAL(SRK) :: x1,y1,x2,y2,r1,r2,rsq
      
      !Check that coord1 and coord2 exist on circle
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
          thisGraph%quadEdges(3,v1,v2)=r
          thisGraph%quadEdges(1:2,v2,v1)=c0
          thisGraph%quadEdges(3,v2,v1)=r
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
    SUBROUTINE getMCB_graphType(thisGraph,cycles)
      CLASS(GraphType),INTENT(IN) :: thisGraph
      TYPE(GraphType),ALLOCATABLE :: cycles(:)
    ENDSUBROUTINE getMCB_graphType
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
ENDMODULE Geom_Graph
