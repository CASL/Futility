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
PROGRAM testGeom_Poly
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV  
  USE UnitTest
  USE IntrType
  USE Constants_Conversion
  USE ParameterLists
  USE Geom_Points
  USE Geom_Line
  USE Geom_Box
  USE Geom_Graph
  USE Geom_Poly
  
  IMPLICIT NONE
  
  LOGICAL(SBK) :: bool
  TYPE(GraphType) :: testGraph
  TYPE(PolygonType) :: testPolyType
  
  CREATE_TEST('Test Polygons')
  CALL eParams%setQuietMode(.TRUE.)
  CALL eParams%setStopOnError(.FALSE.)
  
  REGISTER_SUBTEST('Test Uninitialized',testUninit)
  REGISTER_SUBTEST('Test Clear',testClear)
  REGISTER_SUBTEST('Test Set',testSet)
  REGISTER_SUBTEST('Test Inside',testInside)
  REGISTER_SUBTEST('Test IntersectLine',testIntersectLine)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testUninit()
      ASSERT(.NOT.testPolyType%isinit,'%isinit')
      ASSERT(testPolyType%area .APPROXEQA. 0.0_SRK,'%area')
      ASSERT(testPolyType%nVert == 0,'%nVert')
      ASSERT(testPolyType%nQuadEdge == 0,'%nQuadEdge')
      ASSERT(.NOT.ALLOCATED(testPolyType%vert),'%vert')
      ASSERT(.NOT.ALLOCATED(testPolyType%edge),'%edge')
      ASSERT(.NOT.ALLOCATED(testPolyType%quad2edge),'%quad2edge')
      ASSERT(.NOT.ALLOCATED(testPolyType%quadEdge),'%quadEdge')
    ENDSUBROUTINE testUninit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testClear()
      !init by hand
      testPolyType%isinit=.TRUE.
      ALLOCATE(testPolyType%vert(3))
      CALL testPolyType%vert(1)%init(DIM=2,X=-1.5_SRK,Y=-1.5_SRK)
      CALL testPolyType%vert(2)%init(DIM=2,X=1.5_SRK,Y=-1.5_SRK)
      CALL testPolyType%vert(3)%init(DIM=2,X=0.0_SRK,Y=2.5_SRK)
      ALLOCATE(testPolyType%edge(2,3))
      testPolyType%edge(:,1)=(/1,2/)
      testPolyType%edge(:,2)=(/2,3/)
      testPolyType%edge(:,3)=(/3,1/)
      ALLOCATE(testPolyType%quadEdge(3,3))
      testPolyType%quadEdge=1.0_SRK
      testPolyType%nVert=3
      testPolyType%nQuadEdge=3
      testPolyType%area=6.0_SRK
      
      CALL testPolyType%clear()
      CALL testUninit()
      
      !Redundant Call
      CALL testPolyType%clear()
      
    ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSet()
      INTEGER(SIK) :: i,inext
      REAL(SRK) :: testCoord(2,9),c0(2),r
      
      !Setup test graph - isosceles triangle
      testCoord(:,1)=(/-1.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/1.0_SRK,-1.0_SRK/)
      testCoord(:,3)=(/0.0_SRK,2.0_SRK/)
      DO i=1,3
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,1))
      
      !inext=testGraph%getCCWMostVert(1,0) !3
      !inext=testGraph%getCCWMostVert(1,3) !2
      !inext=testGraph%getCCWMostVert(3,2) !1
      !inext=testGraph%getCCWMostVert(2,1) !3
      !inext=testGraph%getCWMostVert(0,1) !2
      !inext=testGraph%getCWMostVert(1,2) !3
      !inext=testGraph%getCWMostVert(2,3) !1
      !inext=testGraph%getCWMostVert(3,1) !2
      
      CALL testPolyType%set(testGraph)
      ASSERT(testPolyType%isinit,'triangle %isinit')
      bool=ALL(testPolyType%vert(1)%coord .APPROXEQA. testCoord(:,1))
      ASSERT(bool,'triangle %vert(1)')
      bool=ALL(testPolyType%vert(2)%coord .APPROXEQA. testCoord(:,3))
      ASSERT(bool,'triangle %vert(2)')
      bool=ALL(testPolyType%vert(3)%coord .APPROXEQA. testCoord(:,2))
      ASSERT(bool,'triangle %vert(3)')
      ASSERT(ALL(testPolyType%edge(:,1) == (/1,2/)),'triangle %edge(:,1)')
      ASSERT(ALL(testPolyType%edge(:,2) == (/2,3/)),'triangle %edge(:,2)')
      ASSERT(ALL(testPolyType%edge(:,3) == (/3,1/)),'triangle %edge(:,3)')
      ASSERT(testPolyType%area .APPROXEQA. 3.0_SRK,'%area')
      
      CALL testGraph%clear()
      CALL testPolyType%clear()
      !Setup test graph - rectangle
      testCoord(:,1)=(/-2.0_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-2.0_SRK,4.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,4.0_SRK/)
      testCoord(:,4)=(/1.0_SRK,-2.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      
      CALL testPolyType%set(testGraph)
      ASSERT(testPolyType%isinit,'rectangle %isinit')
      bool=ALL(testPolyType%vert(1)%coord .APPROXEQA. testCoord(:,1))
      ASSERT(bool,'rectangle %vert(1)')
      bool=ALL(testPolyType%vert(2)%coord .APPROXEQA. testCoord(:,2))
      ASSERT(bool,'rectangle %vert(2)')
      bool=ALL(testPolyType%vert(3)%coord .APPROXEQA. testCoord(:,3))
      ASSERT(bool,'rectangle %vert(3)')
      bool=ALL(testPolyType%vert(4)%coord .APPROXEQA. testCoord(:,4))
      ASSERT(bool,'rectangle %vert(4)')
      ASSERT(ALL(testPolyType%edge(:,1) == (/1,2/)),'rectangle %edge(:,1)')
      ASSERT(ALL(testPolyType%edge(:,2) == (/2,3/)),'rectangle %edge(:,2)')
      ASSERT(ALL(testPolyType%edge(:,3) == (/3,4/)),'rectangle %edge(:,3)')
      ASSERT(ALL(testPolyType%edge(:,4) == (/4,1/)),'rectangle %edge(:,4)')
      ASSERT(testPolyType%area .APPROXEQA. 18.0_SRK,'%area')
      
      CALL testGraph%clear()
      CALL testPolyType%clear()
      !Setup test graph - square with a full circle
      testCoord(:,1)=(/-2.0_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-2.0_SRK,2.0_SRK/)
      testCoord(:,3)=(/2.0_SRK,2.0_SRK/)
      testCoord(:,4)=(/2.0_SRK,-2.0_SRK/)
      c0=(/0.0_SRK,0.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testGraph%defineQuadraticEdge(testCoord(:,1), &
        testCoord(:,2),c0,SQRT(8.0_SRK))
      CALL testGraph%defineQuadraticEdge(testCoord(:,2), &
        testCoord(:,3),c0,SQRT(8.0_SRK))
      CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,4),c0,SQRT(8.0_SRK))
      CALL testGraph%defineQuadraticEdge(testCoord(:,4), &
        testCoord(:,1),c0,SQRT(8.0_SRK))
      
      CALL testPolyType%set(testGraph)
      ASSERT(testPolyType%isinit,'rectangle %isinit')
      bool=ALL(testPolyType%vert(1)%coord .APPROXEQA. testCoord(:,1))
      ASSERT(bool,'square-circ %vert(1)')
      bool=ALL(testPolyType%vert(2)%coord .APPROXEQA. testCoord(:,2))
      ASSERT(bool,'square-circ %vert(2)')
      bool=ALL(testPolyType%vert(3)%coord .APPROXEQA. testCoord(:,3))
      ASSERT(bool,'square-circ %vert(3)')
      bool=ALL(testPolyType%vert(4)%coord .APPROXEQA. testCoord(:,4))
      ASSERT(bool,'square-circ %vert(4)')
      ASSERT(ALL(testPolyType%edge(:,1) == (/1,2/)),'square-circ %edge(:,1)')
      ASSERT(ALL(testPolyType%edge(:,2) == (/2,3/)),'square-circ %edge(:,2)')
      ASSERT(ALL(testPolyType%edge(:,3) == (/3,4/)),'square-circ %edge(:,3)')
      ASSERT(ALL(testPolyType%edge(:,4) == (/4,1/)),'square-circ %edge(:,4)')
      bool=ALL(testPolyType%quadEdge(:,1) .APPROXEQA. &
        (/0.0_SRK,0.0_SRK, SQRT(8.0_SRK)/))
      ASSERT(bool,'square-circ %quadEdge(:,1)')
      bool=ALL(testPolyType%quadEdge(:,2) .APPROXEQA. &
        (/0.0_SRK,0.0_SRK, SQRT(8.0_SRK)/))
      ASSERT(bool,'square-circ %quadEdge(:,2)')
      bool=ALL(testPolyType%quadEdge(:,3) .APPROXEQA. &
        (/0.0_SRK,0.0_SRK, SQRT(8.0_SRK)/))
      ASSERT(bool,'square-circ %quadEdge(:,3)')
      bool=ALL(testPolyType%quadEdge(:,4) .APPROXEQA. &
        (/0.0_SRK,0.0_SRK, SQRT(8.0_SRK)/))
      ASSERT(bool,'square-circ %quadEdge(:,4)')
      ASSERT(testPolyType%area .APPROXEQA. 25.13274122871835_SRK,'%area')
      
      !CALL testGraph%clear()
      !CALL testPolyType%clear()
      !!Setup test graph - square with a half circle
      !testCoord(:,1)=(/-2.0_SRK,-2.0_SRK/)
      !testCoord(:,2)=(/-2.0_SRK,2.0_SRK/)
      !testCoord(:,3)=(/2.0_SRK,2.0_SRK/)
      !testCoord(:,4)=(/2.0_SRK,-2.0_SRK/)
      !c0(2)=(/2.0_SRK,0.0_SRK/)
      !DO i=1,4
      !  CALL testGraph%insertVertex(testCoord(:,i))
      !ENDDO
      !CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      !CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      !CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      !CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      !CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
      !  testCoord(:,4),c0,2.0_SRK)
      !
      !CALL testPolyType%set(testGraph)
      !ASSERT(testPolyType%isinit,'rectangle %isinit')
      !bool=ALL(testPolyType%vert(1)%coord .APPROXEQA. testCoord(:,1))
      !ASSERT(bool,'square-quad %vert(1)')
      !bool=ALL(testPolyType%vert(2)%coord .APPROXEQA. testCoord(:,2))
      !ASSERT(bool,'square-quad %vert(2)')
      !bool=ALL(testPolyType%vert(3)%coord .APPROXEQA. testCoord(:,3))
      !ASSERT(bool,'square-quad %vert(3)')
      !bool=ALL(testPolyType%vert(4)%coord .APPROXEQA. testCoord(:,4))
      !ASSERT(bool,'square-quad %vert(4)')
      !ASSERT(ALL(testPolyType%edge(:,1) == (/1,2/)),'square-quad %edge(:,1)')
      !ASSERT(ALL(testPolyType%edge(:,2) == (/2,3/)),'square-quad %edge(:,2)')
      !ASSERT(ALL(testPolyType%edge(:,3) == (/3,4/)),'square-quad %edge(:,3)')
      !ASSERT(ALL(testPolyType%edge(:,4) == (/4,1/)),'square-quad %edge(:,4)')
      !bool=ALL(testPolyType%quadEdge(:,3) .APPROXEQA. &
      !  (/2.0_SRK,0.0_SRK,2.0_SRK/))
      !ASSERT(bool,'square-quad %quadEdge(:,3)')
      !ASSERT(testPolyType%area .APPROXEQA. 16.0_SRK+TWOPI,'%area')
      
      CALL testGraph%clear()
      CALL testPolyType%clear()
      !Setup test graph - square with a circle to subtract
      testCoord(:,1)=(/-2.0_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-2.0_SRK,2.0_SRK/)
      testCoord(:,3)=(/2.0_SRK,2.0_SRK/)
      testCoord(:,4)=(/2.0_SRK,-2.0_SRK/)
      c0=(/4.0_SRK,0.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,4),c0,SQRT(8.0_SRK))
      
      CALL testPolyType%set(testGraph)
      ASSERT(testPolyType%isinit,'rectangle %isinit')
      bool=ALL(testPolyType%vert(1)%coord .APPROXEQA. testCoord(:,1))
      ASSERT(bool,'square-quad %vert(1)')
      bool=ALL(testPolyType%vert(2)%coord .APPROXEQA. testCoord(:,2))
      ASSERT(bool,'square-quad %vert(2)')
      bool=ALL(testPolyType%vert(3)%coord .APPROXEQA. testCoord(:,3))
      ASSERT(bool,'square-quad %vert(3)')
      bool=ALL(testPolyType%vert(4)%coord .APPROXEQA. testCoord(:,4))
      ASSERT(bool,'square-quad %vert(4)')
      ASSERT(ALL(testPolyType%edge(:,1) == (/1,2/)),'square-quad %edge(:,1)')
      ASSERT(ALL(testPolyType%edge(:,2) == (/2,3/)),'square-quad %edge(:,2)')
      ASSERT(ALL(testPolyType%edge(:,3) == (/3,4/)),'square-quad %edge(:,3)')
      ASSERT(ALL(testPolyType%edge(:,4) == (/4,1/)),'square-quad %edge(:,4)')
      bool=ALL(testPolyType%quadEdge(:,1) .APPROXEQA. &
        (/4.0_SRK,0.0_SRK,SQRT(8.0_SRK)/))
      ASSERT(bool,'square-quad %quadEdge(:,1)')
      ASSERT(testPolyType%area .APPROXEQA. 16.0_SRK-2.28318530717959_SRK,'%area')
      
      CALL testGraph%clear()
      CALL testPolyType%clear()
    ENDSUBROUTINE testSet
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInside()
      INTEGER(SIK) :: i,inext
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(PointType) :: point
      
      !Setup test graph - isosceles triangle
      testCoord(:,1)=(/-1.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/1.0_SRK,-1.0_SRK/)
      testCoord(:,3)=(/0.0_SRK,2.0_SRK/)
      DO i=1,3
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,1))
      
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isinit,'%isinit')
      CALL point%clear()
      CALL point%init(DIM=2,X=-0.5_SRK,Y=-0.5_SRK)
      ASSERT(testPolyType%inside(point),'low left triangle')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.5_SRK,Y=-0.5_SRK)
      ASSERT(testPolyType%inside(point),'low right triangle')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.2_SRK,Y=0.5_SRK)
      ASSERT(testPolyType%inside(point),'top right of triangle')
      CALL point%clear()
      CALL point%init(DIM=2,X=-0.2_SRK,Y=0.5_SRK)
      ASSERT(testPolyType%inside(point),'top left of triangle')
      CALL point%clear()
      CALL point%init(DIM=2,X=-5.0_SRK,Y=-5.0_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'outside triangle')
      CALL point%clear()
      CALL point%init(DIM=2,X=-0.5_SRK,Y=-1.0_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'on bottom edge triangle')
      
      !Setup test graph - quadralateral
      CALL testGraph%clear()
      CALL testPolyType%clear()
      testCoord(:,1)=(/-3.0_SRK,-3.0_SRK/)
      testCoord(:,2)=(/-1.0_SRK,2.0_SRK/)
      testCoord(:,3)=(/2.0_SRK,3.0_SRK/)
      testCoord(:,4)=(/1.0_SRK,-2.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      
      
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isinit,'%isinit')
      CALL point%clear()
      CALL point%init(DIM=2,X=1.0_SRK,Y=1.0_SRK)
      ASSERT(testPolyType%inside(point),'Q1 Quadralateral')
      CALL point%clear()
      CALL point%init(DIM=2,X=-1.0_SRK,Y=1.0_SRK)
      ASSERT(testPolyType%inside(point),'Q2 Quadralateral')
      CALL point%clear()
      CALL point%init(DIM=2,X=-1.0_SRK,Y=-1.0_SRK)
      ASSERT(testPolyType%inside(point),'Q3 Quadralateral')
      CALL point%clear()
      CALL point%init(DIM=2,X=1.0_SRK,Y=-1.0_SRK)
      ASSERT(testPolyType%inside(point),'Q4 Quadralateral')
      CALL point%clear()
      CALL point%init(DIM=2,X=2.0_SRK,Y=1.0_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'Outside Q1 Quadralateral')
      CALL point%clear()
      CALL point%init(DIM=2,X=-3.0_SRK,Y=1.0_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'Outside Q2 Quadralateral')
      CALL point%clear()
      CALL point%init(DIM=2,X=-2.5_SRK,Y=-0.25_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'Outside Q3 Quadralateral')
      CALL point%clear()
      CALL point%init(DIM=2,X=2.0_SRK,Y=-1.0_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'Outside Q4 Quadralateral')
      
      !Setup test graph - quadralateral with quadratic edge
      CALL testGraph%clear()
      CALL testPolyType%clear()
      testCoord(:,1)=(/-2.0_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-2.0_SRK,2.0_SRK/)
      testCoord(:,3)=(/2.0_SRK,2.0_SRK/)
      testCoord(:,4)=(/2.0_SRK,-2.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      c0=(/-1.0_SRK,0.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,1), &
        testCoord(:,2),c0,SQRT(5.0_SRK))
      c0=(/0.0_SRK,1.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,2), &
        testCoord(:,3),c0,SQRT(5.0_SRK))
      c0=(/4.0_SRK,0.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,4),c0,SQRT(8.0_SRK))
      c0=(/0.0_SRK,-4.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,4), &
        testCoord(:,1),c0,SQRT(8.0_SRK))
      
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isinit,'%isinit')
      CALL point%clear()
      CALL point%init(DIM=2,X=-5.0_SRK,Y=5.0_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'1:Outside Poly and West QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=-2.5_SRK,Y=0.0_SRK)
      ASSERT(testPolyType%inside(point),'2:Outside Poly, Inside West QuadEdge')
      CALL point%clear()
      CALL point%init(DIM=2,X=-1.5_SRK,Y=0.0_SRK)
      ASSERT(testPolyType%inside(point),'3:Inside Poly and West QuadEdge')
      CALL point%clear()
      CALL point%init(DIM=2,X=1.8_SRK,Y=0.0_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'4:Inside Poly and East QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=4.0_SRK,Y=1.0_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'5:Outside Poly and Inside East QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=-10.0_SRK,Y=-1.0_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'6:Outside Poly and East QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=-3.0_SRK,Y=2.5_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'7:Outside Poly and North QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=-1.99_SRK,Y=2.5_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'8:Outside Poly and North QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=2.5_SRK)
      ASSERT(testPolyType%inside(point),'9:Outside Poly and Inside North QuadEdge')
      CALL point%clear()
      CALL point%init(DIM=2,X=1.99_SRK,Y=2.5_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'10:Outside Poly and North QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=3.0_SRK,Y=2.5_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'11:Outside Poly and North QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=-3.0_SRK,Y=-1.9_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'12:Outside Poly and South QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-1.9_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'13:Inside Poly and South QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=3.0_SRK,Y=-1.9_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'14:Outside Poly and South QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=-2.0_SRK,Y=-2.5_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'15:Outside Poly and South QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-2.5_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'16:Outside Poly and South QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=2.0_SRK,Y=-2.5_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'17:Outside Poly and South QuadEdge (out)')
      
      CALL testGraph%clear()
      CALL testPolyType%clear()
      CALL point%clear()
    ENDSUBROUTINE testInside
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIntersectLine()
      INTEGER(SIK) :: i,inext
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(PointType) :: point1,point2
      TYPE(PointType),ALLOCATABLE :: points(:)
      TYPE(LineType) :: line
      
!Setup test graph - isosceles triangle
      testCoord(:,1)=(/-1.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/1.0_SRK,-1.0_SRK/)
      testCoord(:,3)=(/0.0_SRK,2.0_SRK/)
      DO i=1,3
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,1))
      
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isinit,'%isinit')
      CALL point1%init(DIM=2,X=-1.0_SRK,Y=0.0_SRK)
      CALL point2%init(DIM=2,X=1.0_SRK,Y=0.0_SRK)
      CALL line%set(point1,point2)
      CALL testPolyType%intersectLine(line,points)
      ASSERTFAIL(SIZE(points) == 2,'npoints for crossing whole geom')
      bool=(points(1)%coord(1) .APPROXEQA. -0.6666666666666667_SRK) .AND. &
        (points(1)%coord(2) .APPROXEQA. 0.0_SRK)
      ASSERT(bool,'point 1 intersection')
      bool=(points(2)%coord(1) .APPROXEQA. 0.6666666666666667_SRK) .AND. &
        (points(2)%coord(2) .APPROXEQA. 0.0_SRK)
      ASSERT(bool,'point 2 intersection')
      
      CALL point1%clear()
      CALL point2%clear()
      CALL line%clear()
      CALL point1%init(DIM=2,X=-1.0_SRK,Y=3.0_SRK)
      CALL point2%init(DIM=2,X=1.0_SRK,Y=3.0_SRK)
      CALL line%set(point1,point2)
      CALL testPolyType%intersectLine(line,points)
      ASSERT(.NOT.ALLOCATED(points),'npoints for miss case')
      
      CALL point1%clear()
      CALL point2%clear()
      CALL line%clear()
      CALL point1%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL point2%init(DIM=2,X=1.0_SRK,Y=0.0_SRK)
      CALL line%set(point1,point2)
      CALL testPolyType%intersectLine(line,points)
      ASSERTFAIL(SIZE(points) == 1,'npoints for one side crossing')
      bool=(points(1)%coord(1) .APPROXEQA. 0.66666666666666667_SRK) .AND. &
        (points(1)%coord(2) .APPROXEQA. 0.0_SRK)
      ASSERT(bool,'point 1 intersection')
      
!Setup test graph - quadralateral with quadratic edge
      CALL testGraph%clear()
      CALL testPolyType%clear()
      testCoord(:,1)=(/-2.0_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-2.0_SRK,2.0_SRK/)
      testCoord(:,3)=(/2.0_SRK,2.0_SRK/)
      testCoord(:,4)=(/2.0_SRK,-2.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      c0=(/-1.0_SRK,0.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,1), &
        testCoord(:,2),c0,SQRT(5.0_SRK))
      c0=(/0.0_SRK,1.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,2), &
        testCoord(:,3),c0,SQRT(5.0_SRK))
      c0=(/4.0_SRK,0.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,4),c0,SQRT(8.0_SRK))
      c0=(/0.0_SRK,-4.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,4), &
        testCoord(:,1),c0,SQRT(8.0_SRK))
      
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isinit,'%isinit')
      CALL point1%clear()
      CALL point2%clear()
      CALL line%clear()
      CALL point1%init(DIM=2,X=-4.0_SRK,Y=0.0_SRK)
      CALL point2%init(DIM=2,X=4.0_SRK,Y=0.0_SRK)
      CALL line%set(point1,point2)
      CALL testPolyType%intersectLine(line,points)
      ASSERTFAIL(SIZE(points) == 2,'npoints for crossing whole geom')
      bool=(points(1)%coord(1) .APPROXEQA. -1.0_SRK-SQRT(5.0_SRK)) .AND. &
        (points(1)%coord(2) .APPROXEQA. 0.0_SRK)
      ASSERT(bool,'point 1 intersection')
      bool=(points(2)%coord(1) .APPROXEQA. 4.0_SRK-SQRT(8.0_SRK)) .AND. &
        (points(2)%coord(2) .APPROXEQA. 0.0_SRK)
      ASSERT(bool,'point 2 intersection')
      
      CALL point1%clear()
      CALL point2%clear()
      CALL line%clear()
      CALL point1%init(DIM=2,X=-4.0_SRK,Y=-1.5_SRK)
      CALL point2%init(DIM=2,X=4.0_SRK,Y=-1.5_SRK)
      CALL line%set(point1,point2)
      CALL testPolyType%intersectLine(line,points)
      ASSERTFAIL(SIZE(points) == 4,'npoints for crossing whole geom')
      
    ENDSUBROUTINE testIntersectLine

!
ENDPROGRAM testGeom_Poly
