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
  USE Geom_CircCyl
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
  REGISTER_SUBTEST('Test Subtract Sub-Volume',testSubtractSubVol)
  REGISTER_SUBTEST('Test Inside',testInside)
  REGISTER_SUBTEST('Test IntersectLine',testIntersectLine)
  REGISTER_SUBTEST('Test IntersectPoly',testIntersectPoly)
  REGISTER_SUBTEST('Test Polygonize',testPolygonize)
  REGISTER_SUBTEST('Test GenerateGraph',testGenerateGraph)

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
    SUBROUTINE testSubtractSubVol()
      INTEGER(SIK) :: i,inext
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(PolygonType) :: testPoly2
      
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
      CALL testPoly2%set(testGraph)
      
      CALL testGraph%clear()
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
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      CALL testPoly2%subtractSubVolume(testPolyType)
      ASSERT(testPoly2%area .APPROXEQA. 13.0_SRK,'%area')
      
      
      CALL testPoly2%clear()
      CALL testPolyType%clear()
    ENDSUBROUTINE testSubtractSubVol
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInside()
      INTEGER(SIK) :: i,inext
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(PointType) :: point
      TYPE(PolygonType) :: testPoly2
      
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
      testCoord(:,1)=(/-3.0_SRK,3.0_SRK/)
      testCoord(:,2)=(/3.0_SRK,3.0_SRK/)
      testCoord(:,3)=(/3.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/2.0_SRK,2.0_SRK/)
      testCoord(:,5)=(/2.0_SRK,-2.0_SRK/)
      testCoord(:,6)=(/-2.0_SRK,-2.0_SRK/)
      testCoord(:,7)=(/-2.0_SRK,2.0_SRK/)
      DO i=1,7
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,7),testCoord(:,1))
      c0=(/0.0_SRK,1.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,1), &
        testCoord(:,2),c0,SQRT(13.0_SRK))
      c0=(/4.0_SRK,0.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,4), &
        testCoord(:,5),c0,SQRT(8.0_SRK))
      c0=(/0.0_SRK,-4.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,5), &
        testCoord(:,6),c0,SQRT(8.0_SRK))
      c0=(/-1.0_SRK,0.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,6), &
        testCoord(:,7),c0,SQRT(5.0_SRK))
      
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
      CALL point%init(DIM=2,X=-2.5_SRK,Y=2.5_SRK)
      ASSERT(testPolyType%inside(point),'8:inside Poly and North QuadEdge (on line, in)')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=3.5_SRK)
      ASSERT(testPolyType%inside(point),'9:Outside Poly and Inside North QuadEdge')
      CALL point%clear()
      CALL point%init(DIM=2,X=2.5_SRK,Y=2.5_SRK)
      ASSERT(testPolyType%inside(point),'10:Outside Poly and North QuadEdge')
      CALL point%clear()
      CALL point%init(DIM=2,X=4.0_SRK,Y=2.5_SRK)
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
      CALL point%clear()
      CALL point%init(DIM=2,X=2.9_SRK,Y=0.5_SRK)
      ASSERT(testPolyType%inside(point),'18:Inside Poly and East QuadEdge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=5.0_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'19:Outside Poly and North QuadEdge (out)')
      
      CALL testGraph%clear()
      !Setup test graph - square with a full circle
      testCoord(:,1)=(/-1.5_SRK,-1.5_SRK/)
      testCoord(:,2)=(/-1.5_SRK,1.5_SRK/)
      testCoord(:,3)=(/1.5_SRK,1.5_SRK/)
      testCoord(:,4)=(/1.5_SRK,-1.5_SRK/)
      c0=(/0.0_SRK,0.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testGraph%defineQuadraticEdge(testCoord(:,1), &
        testCoord(:,2),c0,SQRT(4.5_SRK))
      CALL testGraph%defineQuadraticEdge(testCoord(:,2), &
        testCoord(:,3),c0,SQRT(4.5_SRK))
      CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,4),c0,SQRT(4.5_SRK))
      CALL testGraph%defineQuadraticEdge(testCoord(:,4), &
        testCoord(:,1),c0,SQRT(4.5_SRK))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      CALL testPolyType%subtractSubVolume(testPoly2)
      CALL point%clear()
      CALL point%init(DIM=2,X=-1.0_SRK,Y=1.0_SRK)
      ASSERT(.NOT.testPolyType%inside(point),'20:Inside subvolume poly (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.5_SRK,Y=2.0_SRK)
      ASSERT(testPolyType%inside(point),'21:outside subvolume poly')
      
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
      bool=(points(1)%coord(1) .APPROXEQA. -2.65831239517770_SRK) .AND. &
        (points(1)%coord(2) .APPROXEQA. -1.5_SRK)
      ASSERT(bool,'point 1 on convex circle 1')
      bool=(points(2)%coord(1) .APPROXEQA. 1.602084238343640_SRK) .AND. &
        (points(2)%coord(2) .APPROXEQA. -1.5_SRK)
      ASSERT(bool,'point 1 on concave circle 3')
      bool=(points(3)%coord(1) .APPROXEQA. -1.32287565553230_SRK) .AND. &
        (points(3)%coord(2) .APPROXEQA. -1.5_SRK)
      ASSERT(bool,'point 1 on concave circle 4')
      bool=(points(4)%coord(1) .APPROXEQA. 1.32287565553230_SRK) .AND. &
        (points(4)%coord(2) .APPROXEQA. -1.5_SRK)
      ASSERT(bool,'point 2 on concave circle 4')
    ENDSUBROUTINE testIntersectLine
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIntersectPoly()
      INTEGER(SIK) :: i,inext
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(PointType) :: point1,point2
      TYPE(PointType),ALLOCATABLE :: points(:)
      TYPE(PolygonType) :: testPoly
      
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
      c0=(/4.0_SRK,0.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,4),c0,SQRT(8.0_SRK))
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isinit,'%isinit')
      CALL testGraph%clear()
      testCoord(:,1)=(/-3.5_SRK,0.0_SRK/)
      testCoord(:,2)=(/-1.0_SRK,2.5_SRK/)
      testCoord(:,3)=(/2.0_SRK,0.0_SRK/)
      DO i=1,3
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,1))
      c0=(/-0.75_SRK,3.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,1),c0,SQRT(16.5625_SRK))
      CALL testPoly%set(testGraph)
      ASSERTFAIL(testPoly%isinit,'%isinit')
      CALL testPolyType%intersectPoly(testPoly,points)
      ASSERTFAIL(SIZE(points) == 6,'SIZE(points)')
      bool=(points(1)%coord(1) .APPROXEQA. -1.281754163448150_SRK) .AND. &
        (points(1)%coord(2) .APPROXEQA. 2.218245836551850_SRK)
      ASSERT(bool,'%point(1)')
      bool=(points(2)%coord(1) .APPROXEQA. -3.218245836551850_SRK) .AND. &
        (points(2)%coord(2) .APPROXEQA. 0.2817541634481450_SRK)
      ASSERT(bool,'%point(2)')
      bool=(points(3)%coord(1) .APPROXEQA. 1.242847645548310_SRK) .AND. &
        (points(3)%coord(2) .APPROXEQA. 0.6309602953764110_SRK)
      ASSERT(bool,'%point(3)')
      bool=(points(4)%coord(1) .APPROXEQA. -0.4_SRK) .AND. &
        (points(4)%coord(2) .APPROXEQA. 2.0_SRK)
      ASSERT(bool,'%point(4)')
      bool=(points(5)%coord(1) .APPROXEQA. 4.0_SRK-SQRT(8.0_SRK)) .AND. &
        (points(5)%coord(2) .APPROXEQA. 0.0_SRK)      
      ASSERT(bool,'%point(5)')
      bool=(points(6)%coord(1) .APPROXEQA. -1.0*SQRT(5.0_SRK)) .AND. &
        (points(6)%coord(2) .APPROXEQA. 0.0_SRK)
      ASSERT(bool,'%point(6)')
      
    ENDSUBROUTINE testIntersectPoly
!
!-------------------------------------------------------------------------------
    SUBROUTINE testPolygonize()
      TYPE(PointType) :: testpoint
      TYPE(ABBoxType) :: testABbox
      TYPE(OBBoxType) :: testOBbox
      TYPE(CircleType) :: testcircle

      !polygonize an OB box
      CALL testpoint%init(DIM=2,X=1.0_SRK,Y=1.0_SRK)
      CALL testOBbox%set(testpoint,(/4.0_SRK*SQRT(2.0_SRK),2.0_SRK*SQRT(2.0_SRK)/), &
        (/-1.0_SRK,1.0_SRK/),(/1.0_SRK,1.0_SRK/))
      CALL Polygonize(testOBbox,testPolyType)
      ASSERTFAIL(testPolyType%isinit,'init polygon from box')
      ASSERT(testPolyType%nVert == 4,'%nVert')
      ASSERT(testPolyType%nQuadEdge == 0,'%nQuadEdge')
      bool=(testPolyType%vert(1)%coord(1) .APPROXEQA. -3.0_SRK) .AND. &
        (testPolyType%vert(1)%coord(2) .APPROXEQA. 5.0_SRK)
      ASSERT(bool,'%vert(1)')
      bool=(testPolyType%vert(2)%coord(1) .APPROXEQA. -1.0_SRK) .AND. &
        (testPolyType%vert(2)%coord(2) .APPROXEQA. 7.0_SRK)
      ASSERT(bool,'%vert(2)')
      bool=(testPolyType%vert(3)%coord(1) .APPROXEQA. 3.0_SRK) .AND. &
        (testPolyType%vert(3)%coord(2) .APPROXEQA. 3.0_SRK)
      ASSERT(bool,'%vert(3)')
      bool=(testPolyType%vert(4)%coord(1) .APPROXEQA. 1.0_SRK) .AND. &
        (testPolyType%vert(4)%coord(2) .APPROXEQA. 1.0_SRK)
      ASSERT(bool,'%vert(4)')
      CALL testpoint%clear()
      CALL testOBbox%clear()
      CALL testPolyType%clear()

      !polygonize an AB box
      CALL testABbox%set(-4.0_SRK,2.0_SRK,1.0_SRK,3.0_SRK)
      CALL Polygonize(testABbox,testPolyType)
      ASSERTFAIL(testPolyType%isinit,'init polygon from AB box')
      ASSERT(testPolyType%nVert == 4,'%nVert')
      ASSERT(testPolyType%nQuadEdge == 0,'%nQuadEdge')
      bool=(testPolyType%vert(1)%coord(1) .APPROXEQA. -4.0_SRK) .AND. &
        (testPolyType%vert(1)%coord(2) .APPROXEQA. 1.0_SRK)
      ASSERT(bool,'%vert(1)')
      bool=(testPolyType%vert(2)%coord(1) .APPROXEQA. -4.0_SRK) .AND. &
        (testPolyType%vert(2)%coord(2) .APPROXEQA. 3.0_SRK)
      ASSERT(bool,'%vert(2)')
      bool=(testPolyType%vert(3)%coord(1) .APPROXEQA. 2.0_SRK) .AND. &
        (testPolyType%vert(3)%coord(2) .APPROXEQA. 3.0_SRK)
      ASSERT(bool,'%vert(3)')
      bool=(testPolyType%vert(4)%coord(1) .APPROXEQA. 2.0_SRK) .AND. &
        (testPolyType%vert(4)%coord(2) .APPROXEQA. 1.0_SRK)
      ASSERT(bool,'%vert(4)')
      CALL testABbox%clear()
      CALL testPolyType%clear()

      !polygonize a circle
      CALL testpoint%init(DIM=2,X=1.0_SRK,Y=1.0_SRK)
      CALL testcircle%set(testpoint,2.0_SRK*SQRT(2.0_SRK))
      CALL Polygonize(testcircle,testPolyType)
      ASSERTFAIL(testPolyType%isinit,'init polygon from circle')
      ASSERT(testPolyType%nVert == 4,'%nVert')
      ASSERT(testPolyType%nQuadEdge == 4,'%nQuadEdge')
      bool=(testPolyType%quadEdge(1,1) .APPROXEQA. 1.0_SRK) .AND. &
        (testPolyType%quadEdge(2,1) .APPROXEQA. 1.0_SRK) .AND. &
        (testPolyType%quadEdge(3,1) .APPROXEQA. 2.0_SRK*SQRT(2.0_SRK))
      ASSERT(bool,'%quadEdge(1)')
      bool=(testPolyType%quadEdge(1,2) .APPROXEQA. 1.0_SRK) .AND. &
        (testPolyType%quadEdge(2,2) .APPROXEQA. 1.0_SRK) .AND. &
        (testPolyType%quadEdge(3,2) .APPROXEQA. 2.0_SRK*SQRT(2.0_SRK))
      ASSERT(bool,'%quadEdge(2)')
      bool=(testPolyType%quadEdge(1,3) .APPROXEQA. 1.0_SRK) .AND. &
        (testPolyType%quadEdge(2,3) .APPROXEQA. 1.0_SRK) .AND. &
        (testPolyType%quadEdge(3,3) .APPROXEQA. 2.0_SRK*SQRT(2.0_SRK))
      ASSERT(bool,'%quadEdge(3)')
      bool=(testPolyType%quadEdge(1,4) .APPROXEQA. 1.0_SRK) .AND. &
        (testPolyType%quadEdge(2,4) .APPROXEQA. 1.0_SRK) .AND. &
        (testPolyType%quadEdge(3,4) .APPROXEQA. 2.0_SRK*SQRT(2.0_SRK))
      ASSERT(bool,'%quadEdge(4)')
      CALL testpoint%clear()
      CALL testcircle%clear()
      CALL testPolyType%clear()

      !Polygonize an arc
      CALL testpoint%init(DIM=2,X=1.0_SRK,Y=1.0_SRK)
      CALL testcircle%set(testpoint,2.0_SRK*SQRT(2.0_SRK),ANGSTT=TWOPI-QTRPI,ANGSTP=QTRPI)
      CALL Polygonize(testcircle,testPolyType)
      ASSERTFAIL(testPolyType%isinit,'init polygon from arc circle')
      ASSERT(testPolyType%nVert == 3,'%nVert')
      ASSERT(testPolyType%nQuadEdge == 1,'%nQuadEdge')
      bool=(testPolyType%vert(1)%coord(1) .APPROXEQA. 1.0_SRK) .AND. &
        (testPolyType%vert(1)%coord(2) .APPROXEQA. 1.0_SRK)
      ASSERT(bool,'%vert(1)')
      bool=(testPolyType%vert(2)%coord(1) .APPROXEQA. 3.0_SRK) .AND. &
        (testPolyType%vert(2)%coord(2) .APPROXEQA. 3.0_SRK)
      ASSERT(bool,'%vert(2)')
      bool=(testPolyType%vert(3)%coord(1) .APPROXEQA. 3.0_SRK) .AND. &
        (testPolyType%vert(3)%coord(2) .APPROXEQA. -1.0_SRK)
      ASSERT(bool,'%vert(3)')
      bool=(testPolyType%quadEdge(1,1) .APPROXEQA. 1.0_SRK) .AND. &
        (testPolyType%quadEdge(2,1) .APPROXEQA. 1.0_SRK) .AND. &
        (testPolyType%quadEdge(3,1) .APPROXEQA. 2.0_SRK*SQRT(2.0_SRK))
      ASSERT(bool,'%quadEdge(1)')

      CALL testpoint%clear()
      CALL testcircle%clear()
      CALL testPolyType%clear()
    ENDSUBROUTINE testPolygonize
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGenerateGraph
      INTEGER(SIK) :: i,inext
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(PointType) :: point1,point2
      TYPE(GraphType) :: testGraph2
      
      CALL testGraph%clear()
      CALL testPolyType%clear()
      testCoord(:,1)=(/-3.0_SRK,3.0_SRK/)
      testCoord(:,2)=(/3.0_SRK,3.0_SRK/)
      testCoord(:,3)=(/3.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/2.0_SRK,2.0_SRK/)
      testCoord(:,5)=(/2.0_SRK,-2.0_SRK/)
      testCoord(:,6)=(/-2.0_SRK,-2.0_SRK/)
      testCoord(:,7)=(/-2.0_SRK,2.0_SRK/)
      DO i=1,7
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,7),testCoord(:,1))
      c0=(/0.0_SRK,1.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,1), &
        testCoord(:,2),c0,SQRT(13.0_SRK))
      c0=(/4.0_SRK,0.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,4), &
        testCoord(:,5),c0,SQRT(8.0_SRK))
      c0=(/0.0_SRK,-4.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,5), &
        testCoord(:,6),c0,SQRT(8.0_SRK))
      c0=(/-1.0_SRK,0.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,6), &
        testCoord(:,7),c0,SQRT(5.0_SRK))
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isinit,'%isinit')
      CALL testPolyType%generateGraph(testGraph2)
      ASSERT(testGraph == testGraph2,'Graph is equal')
      
    ENDSUBROUTINE testGenerateGraph
!
ENDPROGRAM testGeom_Poly
