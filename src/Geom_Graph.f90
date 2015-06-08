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
MODULE Geom_Graph
  USE IntrType
  USE Allocs
  
  IMPLICIT NONE
  PRIVATE
  
  PUBLIC :: GraphType
  
  TYPE :: GraphType
    !>
    REAL(SRK),ALLOCATABLE :: vertices(:,:)
    !>
    INTEGER(SIK),ALLOCATABLE :: edgeMatrix(:,:)
    !>
    REAL(SRK),ALLOCATABLE :: quadEdges(:,:)
    CONTAINS
      !> @copybrief
      !> @copydetails
      PROCEDURE,PASS :: nVert => nVert_graphType
      !> @copybrief
      !> @copydetails
      PROCEDURE,PASS :: nEdge => nEdge_graphType
      !> @copybrief 
      !> @copydetails
      PROCEDURE,PASS :: insertVertex => insertVertex_graphType
      !> @copybrief 
      !> @copydetails
      PROCEDURE,PASS :: defineEdge => defineEdge_graphType
      !> @copybrief 
      !> @copydetails
      PROCEDURE,PASS :: defineQuadraticEdge => defineQuadEdge_graphType
      !> @copybrief 
      !> @copydetails
      PROCEDURE,PASS :: getMCB => getMCB_graphType
      !> @copybrief 
      !> @copydetails
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
    SUBROUTINE insertVertex_graphType(thisGraph,coord)
      CLASS(GraphType),INTENT(INOUT) :: thisGraph
      REAL(SRK),INTENT(IN) :: coord(2)
      INTEGER(SIK) :: i,j,n,k
      INTEGER(SIK),ALLOCATABLE :: tmpE(:,:)
      REAL(SRK),ALLOCATABLE :: tmpVertices(:,:)
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
          
          !Expand Edge Matrix
          CALL dmallocA(tmpE,n+1,n+1)
          DO j=1,k-1
            DO i=1,k-1
              tmpE(i,j)=thisGraph%edgeMatrix(i,j)
              tmpE(j,i)=thisGraph%edgeMatrix(j,i)
            ENDDO
            DO i=k+1,n+1
              tmpE(i,j)=thisGraph%edgeMatrix(i-1,j)
              tmpE(j,i)=thisGraph%edgeMatrix(j,i-1)
            ENDDO
          ENDDO
          DO j=k+1,n+1
            DO i=1,k-1
              tmpE(i,j)=thisGraph%edgeMatrix(i,j-1)
              tmpE(j,i)=thisGraph%edgeMatrix(j-1,i)
            ENDDO
            DO i=k+1,n+1
              tmpE(i,j)=thisGraph%edgeMatrix(i-1,j-1)
              tmpE(j,i)=thisGraph%edgeMatrix(j-1,i-1)
            ENDDO
          ENDDO
          CALL demallocA(thisGraph%edgeMatrix)
          CALL MOVE_ALLOC(tmpE,thisGraph%edgeMatrix)
        ENDIF
      ELSE
        CALL dmallocA(thisGraph%vertices,2,1)
        thisGraph%vertices(:,1)=coord
        CALL dmallocA(thisGraph%edgeMatrix,1,1)
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
      INTEGER(SIK) :: i,j,n,v1,v2
      
      IF(ALLOCATED(thisGraph%vertices) .AND. &
        .NOT.ALL(coord1 .APPROXEQA. coord2)) THEN
        n=SIZE(thisGraph%vertices,DIM=2)
        v1=0
        v2=0
        DO i=1,n
          IF(coord1(1) .APPROXEQA. thisGraph%vertices(1,i)) THEN
            IF(coord1(2) .APPROXEQA. thisGraph%vertices(2,i)) THEN
              v1=i
            ELSE
              DO j=i+1,n
                IF(coord1(2) .APPROXEQA. thisGraph%vertices(2,i)) THEN
                  v1=j
                  EXIT
                ENDIF
              ENDDO
            ENDIF
            EXIT
          ENDIF
        ENDDO
        DO i=1,n
          IF(coord2(1) .APPROXEQA. thisGraph%vertices(1,i)) THEN
            IF(coord2(2) .APPROXEQA. thisGraph%vertices(2,i)) THEN
              v2=i
            ELSE
              DO j=i+1,n
                IF(coord2(2) .APPROXEQA. thisGraph%vertices(2,i)) THEN
                  v2=j
                  EXIT
                ENDIF
              ENDDO
            ENDIF
            EXIT
          ENDIF
        ENDDO
        IF(v1 > 0 .AND. v2 > 0) THEN
          thisGraph%edgeMatrix(v1,v2)=1
          thisGraph%edgeMatrix(v2,v1)=1
        ENDIF
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
