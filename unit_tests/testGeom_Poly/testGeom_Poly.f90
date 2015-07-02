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
  REGISTER_SUBTEST('Test Point onSurface',testPointOnSurface)
  REGISTER_SUBTEST('Test Point Inside',testPointInside)
  REGISTER_SUBTEST('Test Poly Inside',testPolyInside)
  REGISTER_SUBTEST('Test Subtract Sub-Volume',testSubtractSubVol)
  REGISTER_SUBTEST('Test DoesLineIntersect',testDoesLineIntersect)
  REGISTER_SUBTEST('Test DoesPolyIntersect',testDoesPolyIntersect)
  REGISTER_SUBTEST('Test IntersectLine',testIntersectLine)
  !REGISTER_SUBTEST('Test IntersectPoly',testIntersectPoly)
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
      TYPE(PolygonType) :: refRect
      
      !Setup test graph - isosceles triangle
      testCoord(:,1)=(/-1.0_SRK,-2.0_SRK/)
      testCoord(:,2)=(/1.0_SRK,-2.0_SRK/)
      testCoord(:,3)=(/0.0_SRK,1.0_SRK/)
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
      bool=(testPolyType%centroid%coord(1) .APPROXEQA. 0.0_SRK) .AND. &
        (testPolyType%centroid%coord(2) .APPROXEQA. -1.0_SRK)
      ASSERT(bool,'%centroid')
      
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
      bool=(testPolyType%centroid%coord(1) .APPROXEQA. -0.5_SRK) .AND. &
        (testPolyType%centroid%coord(2) .APPROXEQA. 1.0_SRK)
      ASSERT(bool,'%centroid')

      refRect%isInit=.TRUE.
      refRect%area=6.0_SRK*3.0_SRK
      refRect%nVert=4
      ALLOCATE(refRect%vert(4))
      DO i=1,4
        CALL refRect%vert(i)%init(COORD=testCoord(:,i))
      ENDDO
      ALLOCATE(refRect%edge(2,4))
      refRect%edge(:,1)=(/1,2/)
      refRect%edge(:,2)=(/2,3/)
      refRect%edge(:,3)=(/3,4/)
      refRect%edge(:,4)=(/4,1/)
      refRect%nQuadEdge=0
      CALL refRect%centroid%init(COORD=(/-0.5_SRK,1.0_SRK/))
      bool=(testPolyType == refRect)
      ASSERT(bool,'== rectangle')
      
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
      bool=(testPolyType%centroid%coord(1) .APPROXEQA. 0.0_SRK) .AND. &
        (testPolyType%centroid%coord(2) .APPROXEQA. 0.0_SRK)
      ASSERT(bool,'%centroid')
      
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
      bool=(testPolyType%centroid%coord(1) .APPROXEQA. -0.388817189177665_SRK) .AND. &
        (testPolyType%centroid%coord(2) .APPROXEQA. 0.0_SRK)
      ASSERT(bool,'%centroid')
      FINFO() testPolyType%centroid%coord
      
      CALL testGraph%clear()
      CALL testPolyType%clear()

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
      CALL testGraph%defineQuadraticEdge(testCoord(:,1),testCoord(:,2), &
         (/0.0_SRK,0.0_SRK/),SQRT(8.0_SRK))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,4),c0,SQRT(8.0_SRK))
      
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isinit,'rectangle %isinit')
      bool=ALL(testPolyType%vert(1)%coord .APPROXEQA. testCoord(:,1))
      ASSERT(bool,'square-2quad %vert(1)')
      bool=ALL(testPolyType%vert(2)%coord .APPROXEQA. testCoord(:,2))
      ASSERT(bool,'square-2quad %vert(2)')
      bool=ALL(testPolyType%vert(3)%coord .APPROXEQA. testCoord(:,3))
      ASSERT(bool,'square-2quad %vert(3)')
      bool=ALL(testPolyType%vert(4)%coord .APPROXEQA. testCoord(:,4))
      ASSERT(bool,'square-2quad %vert(4)')
      ASSERT(ALL(testPolyType%edge(:,1) == (/1,2/)),'square-2quad %edge(:,1)')
      ASSERT(ALL(testPolyType%edge(:,2) == (/2,3/)),'square-2quad %edge(:,2)')
      ASSERT(ALL(testPolyType%edge(:,3) == (/3,4/)),'square-2quad %edge(:,3)')
      ASSERT(ALL(testPolyType%edge(:,4) == (/4,1/)),'square-2quad %edge(:,4)')
      bool=ALL(testPolyType%quadEdge(:,1) .APPROXEQA. &
        (/0.0_SRK,0.0_SRK,SQRT(8.0_SRK)/))
      ASSERT(bool,'square-2quad %quadEdge(:,1)')
      bool=ALL(testPolyType%quadEdge(:,2) .APPROXEQA. &
        (/4.0_SRK,0.0_SRK,SQRT(8.0_SRK)/))
      ASSERT(bool,'square-2quad %quadEdge(:,2)')
      ASSERT(testPolyType%area .APPROXEQA. 16.0_SRK,'%area')
!       bool=(testPolyType%centroid%coord(1) .APPROXEQA. -2.0_SRK*0.388817189177665_SRK) .AND. &
!         (testPolyType%centroid%coord(2) .APPROXEQA. 0.0_SRK)
!       ASSERT(bool,'%centroid')
!       FINFO() testPolyType%centroid%coord


    ENDSUBROUTINE testSet
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSubtractSubVol()
      INTEGER(SIK) :: i,inext
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(PolygonType) :: testPoly2,testPoly3,testPoly4
      
      !Case 1, subtract itself
      !Setup test graph - quadralateral
      CALL testGraph%clear()
      CALL testPolyType%clear()
      testCoord(:,1)=(/-2.0_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-2.0_SRK,3.0_SRK/)
      testCoord(:,3)=(/2.0_SRK,3.0_SRK/)
      testCoord(:,4)=(/2.0_SRK,-2.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isinit,'%isinit')
      CALL testPolyType%subtractSubVolume(testPolyType)
      ASSERT(.NOT.ASSOCIATED(testPolyType%subRegions),'%subRegions')
      ASSERT(testPolyType%area .APPROXEQA. 20.0_SRK,'%area')
      bool=ALL(testPolyType%centroid%coord .APPROXEQA. (/0.0_SRK,0.5_SRK/))
      ASSERT(bool,'%centroid')
      
      !Case 4, subtract a valid subregion
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
      CALL testPoly2%set(testGraph)
      
      !Triangle is fully inside quadralateral
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      CALL testPolyType%subtractSubVolume(testPoly2)
      !Test valid subtractSubVolume
      ASSERTFAIL(ASSOCIATED(testPolyType%subRegions),'%subRegions')
      ASSERT(testPolyType%subRegions == testPoly2,'equivalence of %subRegions')
      ASSERT(testPolyType%area .APPROXEQA. 17.0_SRK,'%area')
      ASSERT(ALL(testPolyType%centroid%coord .APPROXEQA. (/0.0_SRK,0.5882352941176471_SRK/)),'%centroid')
      testPolyType%area=testPolyType%area+testPolyType%subRegions%area
      testPolyType%centroid%coord(2)=0.5_SRK
      CALL testPolyType%subRegions%clear()
      testPolyType%subRegions => NULL()
      
      !Case 2, subtract a rectangle that shares an edge with the polygon
      CALL testPoly2%clear()
      CALL testGraph%clear()
      !Setup test graph - colinear rectangle
      testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
      testCoord(:,3)=(/2.0_SRK,1.0_SRK/)
      testCoord(:,4)=(/2.0_SRK,0.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPoly2%set(testGraph)
      !rectangle shares an edge with quadralateral
      ASSERTFAIL(testPolyType%isinit,'%isinit')
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      CALL testPolyType%subtractSubVolume(testPoly2)
      ASSERT(.NOT.ASSOCIATED(testPolyType%subRegions),'%subRegions')
      
      !Case 8, subtracting a rectangle that intersects with other subregions
      CALL testGraph%clear()
      !Setup test graph - rectangle
      testCoord(:,1)=(/-0.5_SRK,-0.5_SRK/)
      testCoord(:,2)=(/-0.5_SRK,2.5_SRK/)
      testCoord(:,3)=(/0.5_SRK,2.5_SRK/)
      testCoord(:,4)=(/0.5_SRK,-0.5_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPoly4%set(testGraph)
      CALL testPolyType%subtractSubVolume(testPoly4)
      bool=.NOT.ASSOCIATED(testPolyType%subRegions%nextPoly)
      ASSERT(bool,'%subRegions%nextPoly')
      
      !Case 6, subtracting a smaller rectangle within a larger subregions
      !Reinit base polygon
      CALL testGraph%clear()
      CALL testPoly2%clear()
      CALL testPoly3%clear()
      CALL testPoly4%clear()
      CALL testPolyType%clear()
      testCoord(:,1)=(/-2.0_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-2.0_SRK,3.0_SRK/)
      testCoord(:,3)=(/2.0_SRK,3.0_SRK/)
      testCoord(:,4)=(/2.0_SRK,-2.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isinit,'%isinit')
      CALL testGraph%clear()
      !Setup test graph - rectangle
      testCoord(:,1)=(/-1.0_SRK,1.0_SRK/)
      testCoord(:,2)=(/-1.0_SRK,2.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,2.0_SRK/)
      testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      CALL testPolyType%subtractSubVolume(testPoly2)
      ASSERT(ASSOCIATED(testPolyType%subRegions),'%subRegions')
      ASSERT(testPolyType%area .APPROXEQA. 18.0_SRK,'%area')
      bool=ALL(testPolyType%centroid%coord .APPROXEQA. (/0.0_SRK,0.3888888888888889_SRK/))
      ASSERT(bool,'%centroid')
      
      !Setup test graph - rectangle
      CALL testGraph%clear()
      testCoord(:,1)=(/-0.5_SRK,1.1_SRK/)
      testCoord(:,2)=(/-0.5_SRK,1.9_SRK/)
      testCoord(:,3)=(/0.5_SRK,1.9_SRK/)
      testCoord(:,4)=(/0.5_SRK,1.1_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPoly3%set(testGraph)
      ASSERTFAIL(testPoly3%isinit,'%isinit')
      CALL testPolyType%subtractSubVolume(testPoly3)
      ASSERT(.NOT.ASSOCIATED(testPolyType%subRegions%nextPoly),'%subRegions%nextPoly')
      
      !Case 9, subtracting a larger rectangle over a two smaller subregions
      CALL testPoly3%clear()
      CALL testGraph%clear()
      !Setup test graph - rectangle
      testCoord(:,1)=(/-0.5_SRK,-1.0_SRK/)
      testCoord(:,2)=(/-0.5_SRK,0.0_SRK/)
      testCoord(:,3)=(/0.5_SRK,0.0_SRK/)
      testCoord(:,4)=(/0.5_SRK,-1.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPoly3%set(testGraph)
      ASSERTFAIL(testPoly3%isinit,'%isinit')
      CALL testPolyType%subtractSubVolume(testPoly3)
      ASSERT(ASSOCIATED(testPolyType%subRegions),'%subRegions')
      ASSERT(ASSOCIATED(testPolyType%subRegions%nextPoly),'%subRegions%nextPoly')
      ASSERT(testPolyType%area .APPROXEQA. 17.0_SRK,'%area')
      bool=ALL(testPolyType%centroid%coord .APPROXEQA. (/0.0_SRK,0.4411764705882353_SRK/))
      ASSERT(bool,'%centroid')
      
      CALL testGraph%clear()
      !Setup test graph - rectangle
      testCoord(:,1)=(/-1.5_SRK,-1.25_SRK/)
      testCoord(:,2)=(/-1.5_SRK,2.75_SRK/)
      testCoord(:,3)=(/1.5_SRK,2.75_SRK/)
      testCoord(:,4)=(/1.5_SRK,-1.25_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPoly4%set(testGraph)
      ASSERTFAIL(testPoly4%isinit,'%isinit')
      CALL testPolyType%subtractSubVolume(testPoly4)
      ASSERT(ASSOCIATED(testPolyType%subRegions),'%subRegions')
      ASSERT(.NOT.ASSOCIATED(testPolyType%subRegions%nextPoly),'%subRegions%nextPoly')
      ASSERT(testPolyType%area .APPROXEQA. 8.0_SRK,'%area')
      bool=ALL(testPolyType%centroid%coord .APPROXEQA. (/0.0_SRK,0.125_SRK/))
      ASSERT(bool,'%centroid')
      
      CALL testPoly2%clear()
      CALL testPolyType%clear()
    ENDSUBROUTINE testSubtractSubVol
!
!-------------------------------------------------------------------------------
    SUBROUTINE testPointOnSurface()
      INTEGER(SIK) :: i,inext
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(PointType) :: point
      TYPE(PolygonType),TARGET :: testPoly2,testPoly3,testPoly4
      
      !Case 1, subtract itself
      !Setup test graph - quadralateral
      CALL testGraph%clear()
      CALL testPolyType%clear()
      testCoord(:,1)=(/-2.0_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-2.0_SRK,3.0_SRK/)
      testCoord(:,3)=(/2.0_SRK,3.0_SRK/)
      testCoord(:,4)=(/2.0_SRK,-2.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isinit,'%isinit')
      CALL testPolyType%subtractSubVolume(testPolyType)
      ASSERT(.NOT.ASSOCIATED(testPolyType%subRegions),'%subRegions')
      ASSERT(testPolyType%area .APPROXEQA. 20.0_SRK,'%area')
      bool=ALL(testPolyType%centroid%coord .APPROXEQA. (/0.0_SRK,0.5_SRK/))
      ASSERT(bool,'%centroid')
      
      !Case 4, subtract a valid subregion
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
      c0=(/0.0_SRK,1.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,1), &
        testCoord(:,2),c0,SQRT(5.0_SRK))
      CALL testPoly2%set(testGraph)
      
      !Triangle is fully inside quadralateral
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      testPolyType%subregions => testPoly2
      
      !Outer surfaces
      CALL point%clear()
      CALL point%init(DIM=2,X=-2.0_SRK,Y=0.0_SRK)
      ASSERT(testPolyType%onSurface(point),'West Outer Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=-2.1_SRK,Y=0.0_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Beyond West Outer Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=-1.9_SRK,Y=0.0_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Inside West Outer Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=3.0_SRK)
      ASSERT(testPolyType%onSurface(point),'North Outer Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=3.1_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Beyond North Outer Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=2.9_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Inside North Outer Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=2.0_SRK,Y=0.0_SRK)
      ASSERT(testPolyType%onSurface(point),'East Outer Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=2.1_SRK,Y=0.0_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Beyond East Outer Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=1.9_SRK,Y=0.0_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Inside East Outer Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-2.0_SRK)
      ASSERT(testPolyType%onSurface(point),'South Outer Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-2.1_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Beyond South Outer Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-1.9_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Inside South Outer Edge')
      !Outer corners
      ASSERT(testPolyType%onSurface(testPolyType%vert(1)),'South-West Corner')
      ASSERT(testPolyType%onSurface(testPolyType%vert(2)),'North-West Corner')
      ASSERT(testPolyType%onSurface(testPolyType%vert(3)),'North-East Corner')
      ASSERT(testPolyType%onSurface(testPolyType%vert(4)),'South-East Corner')
      
      !Sub-region tests.
      CALL point%clear()
      CALL point%init(DIM=2,X=-0.3333333333333333_SRK,Y=1.0_SRK)
      ASSERT(testPolyType%onSurface(point),'1st Inner Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=-0.3433333333333333_SRK,Y=1.0_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Beyond 1st Inner Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=-0.3233333333333333_SRK,Y=1.0_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Inside 1st Inner Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.3333333333333333_SRK,Y=1.0_SRK)
      ASSERT(testPolyType%onSurface(point),'2nd Inner Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.3433333333333333_SRK,Y=1.0_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Beyond 2nd Inner Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.3233333333333333_SRK,Y=1.0_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Inside 2nd Inner Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-SQRT(5.0_SRK)+1.0_SRK)
      ASSERT(testPolyType%onSurface(point),'3rd Inner Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-SQRT(5.0_SRK)+0.9_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Beyond 3rd Inner Edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-SQRT(5.0_SRK)+1.1_SRK)
      ASSERT(.NOT.testPolyType%onSurface(point),'Inside 3rd Inner Edge')
      !Inner corners
      ASSERT(testPolyType%onSurface(testPolyType%subregions%vert(1)),'1st Inner Corner')
      ASSERT(testPolyType%onSurface(testPolyType%subregions%vert(2)),'2nd Inner Corner')
      ASSERT(testPolyType%onSurface(testPolyType%subregions%vert(3)),'3rd Inner Corner')
    ENDSUBROUTINE testPointOnSurface
!
!-------------------------------------------------------------------------------
    SUBROUTINE testPointInside()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,inext
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(PointType) :: point
      TYPE(PolygonType),TARGET :: testPoly2,testPoly3
      
      COMPONENT_TEST('Triangle')
      CALL testPolyType%clear()
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
      ASSERT(testPolyType%pointInside(point),'low left')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.5_SRK,Y=-0.5_SRK)
      ASSERT(testPolyType%pointInside(point),'low right')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.2_SRK,Y=0.5_SRK)
      ASSERT(testPolyType%pointInside(point),'top right')
      CALL point%clear()
      CALL point%init(DIM=2,X=-0.2_SRK,Y=0.5_SRK)
      ASSERT(testPolyType%pointInside(point),'top left')
      CALL point%clear()
      CALL point%init(DIM=2,X=-5.0_SRK,Y=-5.0_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'outside')
      CALL point%clear()
      CALL point%init(DIM=2,X=-0.5_SRK,Y=-1.0_SRK)
      ASSERT(testPolyType%pointInside(point),'on bottom edge')
      CALL testGraph%clear()
      CALL testPolyType%clear()
      CALL point%clear()

      COMPONENT_TEST('Square')
      testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,1.0_SRK/)
      testCoord(:,4)=(/1.0_SRK,0.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,4))
      CALL testPolyType%set(testGraph)
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=0.5_SRK)
      ASSERT(testPolyType%pointInside(point),'left edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.5_SRK,Y=1.0_SRK)
      ASSERT(testPolyType%pointInside(point),'top edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=1.0_SRK,Y=0.5_SRK)
      ASSERT(testPolyType%pointInside(point),'right edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.5_SRK,Y=0.0_SRK)
      ASSERT(testPolyType%pointInside(point),'bottom edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=-1.0_SRK,Y=0.0_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'parallel -x outside')
      CALL point%clear()
      CALL point%init(DIM=2,X=2.0_SRK,Y=0.0_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'parallel +x outside')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-1.0_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'parallel -y outside')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=2.0_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'parallel +y outside')
      CALL testGraph%clear()
      CALL testPolyType%clear()

      COMPONENT_TEST('Quadrilateral')
      !Setup test graph - quadralateral
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
      ASSERT(testPolyType%pointInside(point),'Q1')
      CALL point%clear()
      CALL point%init(DIM=2,X=-1.0_SRK,Y=1.0_SRK)
      ASSERT(testPolyType%pointInside(point),'Q2')
      CALL point%clear()
      CALL point%init(DIM=2,X=-1.0_SRK,Y=-1.0_SRK)
      ASSERT(testPolyType%pointInside(point),'Q3')
      CALL point%clear()
      CALL point%init(DIM=2,X=1.0_SRK,Y=-1.0_SRK)
      ASSERT(testPolyType%pointInside(point),'Q4')
      CALL point%clear()
      CALL point%init(DIM=2,X=2.0_SRK,Y=1.0_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'Outside Q1')
      CALL point%clear()
      CALL point%init(DIM=2,X=-3.0_SRK,Y=1.0_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'Outside Q2')
      CALL point%clear()
      CALL point%init(DIM=2,X=-2.5_SRK,Y=-0.25_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'Outside Q3')
      CALL point%clear()
      CALL point%init(DIM=2,X=2.0_SRK,Y=-1.0_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'Outside Q4')
      CALL point%clear()
      CALL point%init(DIM=2,X=2.0_SRK,Y=3.0_SRK)
      ASSERT(testPolyType%pointInside(point),'on vertex')
      CALL point%clear()
      CALL point%init(DIM=2,X=-2.0_SRK,Y=-3.0_SRK)
      !MODIFY Polygon for this test (edge must be co-linear axis)
      testPolyType%vert(4)%coord(2)=-3.0_SRK
      ASSERT(testPolyType%pointInside(point),'on edge')
      CALL point%clear()
      CALL point%init(DIM=2,X=3.0_SRK,Y=3.0_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'outside bugfix')

      !Setup test graph - quadralateral with quadratic edge
      COMPONENT_TEST('Quad Edges')
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
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
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
      ASSERT(.NOT.testPolyType%pointInside(point),'1:Outside Poly and West QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=-2.5_SRK,Y=0.0_SRK)
      ASSERT(testPolyType%pointInside(point),'2:Outside Poly, Inside West QuadEdge')
      CALL point%clear()
      CALL point%init(DIM=2,X=-1.5_SRK,Y=0.0_SRK)
      ASSERT(testPolyType%pointInside(point),'3:Inside Poly and West QuadEdge')
      CALL point%clear()
      CALL point%init(DIM=2,X=1.8_SRK,Y=0.0_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'4:Inside Poly and East QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=4.0_SRK,Y=1.0_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'5:Outside Poly and Inside East QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=-10.0_SRK,Y=-1.0_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'6:Outside Poly and East QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=-3.0_SRK,Y=2.5_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'7:Outside Poly and North QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=-2.5_SRK,Y=2.5_SRK)
      ASSERT(testPolyType%pointInside(point),'8:inside Poly and North QuadEdge (on line, in)')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=3.5_SRK)
      ASSERT(testPolyType%pointInside(point),'9:Outside Poly and Inside North QuadEdge')
      CALL point%clear()
      CALL point%init(DIM=2,X=2.5_SRK,Y=2.5_SRK)
      ASSERT(testPolyType%pointInside(point),'10:Outside Poly and North QuadEdge')
      CALL point%clear()
      CALL point%init(DIM=2,X=4.0_SRK,Y=2.5_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'11:Outside Poly and North QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=-3.0_SRK,Y=-1.9_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'12:Outside Poly and South QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-1.9_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'13:Inside Poly and South QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=3.0_SRK,Y=-1.9_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'14:Outside Poly and South QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=-2.0_SRK,Y=-2.5_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'15:Outside Poly and South QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-2.5_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'16:Outside Poly and South QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=2.0_SRK,Y=-2.5_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'17:Outside Poly and South QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=2.9_SRK,Y=0.5_SRK)
      ASSERT(testPolyType%pointInside(point),'18:Inside Poly and East QuadEdge')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=5.0_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'19:Outside Poly and North QuadEdge (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-1.17157287525381_SRK)
      ASSERT(testPolyType%pointInside(point),'20:On bottom QuadEdge (in)')
      CALL point%clear()
      CALL point%init(DIM=2,X=-3.23606797749979_SRK,Y=0.0_SRK)
      ASSERT(testPolyType%pointInside(point),'21:On left QuadEdge (in)')
      CALL testGraph%clear()

      COMPONENT_TEST('Circular Hole')
      testCoord(:,1)=(/-1.0_SRK,-1.0_SRK/)/SQRT(2.0_SRK)
      testCoord(:,2)=(/-1.0_SRK,1.0_SRK/)/SQRT(2.0_SRK)
      testCoord(:,3)=(/1.0_SRK,1.0_SRK/)/SQRT(2.0_SRK)
      testCoord(:,4)=(/1.0_SRK,-1.0_SRK/)/SQRT(2.0_SRK)
      c0=(/0.0_SRK,0.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,1), &
        testCoord(:,2),c0,1.0_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,2), &
        testCoord(:,3),c0,1.0_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,4),c0,1.0_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,4), &
        testCoord(:,1),c0,1.0_SRK)
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      testPolyType%subRegions => testPoly2
      CALL point%clear()
      CALL point%init(DIM=2,X=-0.6_SRK,Y=0.6_SRK)
      ASSERT(.NOT.testPolyType%pointInside(point),'20:Inside subvolume poly (out)')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.5_SRK,Y=1.0_SRK)
      ASSERT(testPolyType%pointInside(point),'21:outside subvolume poly')
      !Sub-Region vertices
      CALL point%clear()
      CALL point%init(DIM=2,X=-1.0_SRK/SQRT(2.0_SRK),Y=-1.0_SRK/SQRT(2.0_SRK))
      ASSERT(testPolyType%pointInside(point),'22: SW vertex subvolume poly')
      CALL point%clear()
      CALL point%init(DIM=2,X=-1.0_SRK/SQRT(2.0_SRK),Y=1.0_SRK/SQRT(2.0_SRK))
      ASSERT(testPolyType%pointInside(point),'23: NW vertex subvolume poly')
      CALL point%clear()
      CALL point%init(DIM=2,X=1.0_SRK/SQRT(2.0_SRK),Y=1.0_SRK/SQRT(2.0_SRK))
      ASSERT(testPolyType%pointInside(point),'24: NE vertex subvolume poly')
      CALL point%clear()
      CALL point%init(DIM=2,X=1.0_SRK/SQRT(2.0_SRK),Y=-1.0_SRK/SQRT(2.0_SRK))
      ASSERT(testPolyType%pointInside(point),'25: SE vertex subvolume poly')
      CALL point%clear()
      CALL point%init(DIM=2,X=-1.0_SRK,Y=0.0_SRK)
      ASSERT(testPolyType%pointInside(point),'26: West Edge subvolume poly')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=1.0_SRK)
      ASSERT(testPolyType%pointInside(point),'27: North Edge subvolume poly')
      CALL point%clear()
      CALL point%init(DIM=2,X=1.0_SRK,Y=0.0_SRK)
      ASSERT(testPolyType%pointInside(point),'28: East Edge subvolume poly')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.0_SRK,Y=-1.0_SRK)
      ASSERT(testPolyType%pointInside(point),'29: South Edge subvolume poly')
      CALL testGraph%clear()
      CALL testPolyType%clear()
      CALL point%clear()

      COMPONENT_TEST('Circle 4p')
      testCoord(:,1)=(/-0.564_SRK,0.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.564_SRK/)
      testCoord(:,3)=(/0.564_SRK,0.0_SRK/)
      testCoord(:,4)=(/0.0_SRK,-0.564_SRK/)
      c0=(/6.943559581994459E-018_SRK,1.388711916398892E-017_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,1), &
        testCoord(:,2),c0,0.564_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,2), &
        testCoord(:,3),c0,0.564_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,4),c0,0.564_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,4), &
        testCoord(:,1),c0,0.564_SRK)
      CALL testPolyType%set(testGraph)
      CALL point%init(DIM=2,X=-0.264_SRK,Y=0.0_SRK)
      ASSERT(testPolyType%pointInside(point),'circle')
      CALL point%clear()
      CALL point%init(DIM=2,X=0.264_SRK,Y=0.0_SRK)
      ASSERT(testPolyType%pointInside(point),'circle')
      CALL testPolyType%clear()

      COMPONENT_TEST('Sub-Sub-Region')
      testCoord(:,1)=(/-0.8_SRK,-0.8_SRK/)
      testCoord(:,2)=(/-0.8_SRK,0.8_SRK/)
      testCoord(:,3)=(/0.8_SRK,0.8_SRK/)
      testCoord(:,4)=(/0.8_SRK,-0.8_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPolyType%clear()
      CALL testPolyType%set(testGraph)
      CALL testGraph%clear()
      testCoord(:,1)=(/-0.5_SRK,-0.5_SRK/)
      testCoord(:,2)=(/-0.5_SRK,0.5_SRK/)
      testCoord(:,3)=(/0.5_SRK,0.5_SRK/)
      testCoord(:,4)=(/0.5_SRK,-0.5_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPoly2%clear()
      CALL testPoly2%set(testGraph)
      CALL testGraph%clear()
      testCoord(:,1)=(/-0.3_SRK,-0.3_SRK/)
      testCoord(:,2)=(/-0.3_SRK,0.3_SRK/)
      testCoord(:,3)=(/0.3_SRK,0.3_SRK/)
      testCoord(:,4)=(/0.3_SRK,-0.3_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPoly3%clear()
      CALL testPoly3%set(testGraph)
      CALL testGraph%clear()

      testPolyType%subRegions => testPoly2
      testPoly2%subRegions => testPoly3

      CALL point%clear()
      CALL point%init(COORD=(/-0.3_SRK,0.0_SRK/))
      ASSERT(.NOT.testPolyType%pointInside(point),'on Sub-Sub-Region surface')
      point%coord=(/-0.4_SRK,0.0_SRK/)
      ASSERT(.NOT.testPolyType%pointInside(point),'in Sub-Region')
      point%coord=(/-0.2_SRK,0.0_SRK/)
      ASSERT(.NOT.testPolyType%pointInside(point),'in Sub-Sub-Region')

    ENDSUBROUTINE testPointInside
!
!-------------------------------------------------------------------------------
    SUBROUTINE testPolyInside()
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(PointType) :: point
      TYPE(PolygonType) :: testPoly2,testPoly3
      
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
      
      CALL testGraph%clear()
      testCoord(:,1)=(/-0.5_SRK,-0.5_SRK/)
      testCoord(:,2)=(/0.5_SRK,-0.5_SRK/)
      testCoord(:,3)=(/0.0_SRK,1.0_SRK/)
      DO i=1,3
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,1))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      ASSERT(testPolyType%boundsPoly(testPoly2),'Small triangle in bigger one')
      ASSERT(.NOT.testPoly2%boundsPoly(testPolyType),'Big triangle not in smaller one')
      CALL testPoly2%clear()
      
      CALL testGraph%clear()
      testCoord(:,1)=(/-1.0_SRK,2.0_SRK/)
      testCoord(:,2)=(/1.0_SRK,2.0_SRK/)
      testCoord(:,3)=(/0.0_SRK,-1.0_SRK/)
      DO i=1,3
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,1))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      ASSERT(.NOT.testPolyType%boundsPoly(testPoly2),'line-line intersection')
      ASSERT(.NOT.testPoly2%boundsPoly(testPolyType),'No points inside')
      CALL testPoly2%clear()
      
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
      ASSERT(.NOT.testPolyType%boundsPoly(testPoly2),'circle-circle intersection')
      ASSERT(.NOT.testPoly2%boundsPoly(testPolyType),'No points inside')
      CALL testPoly2%clear()
      
      CALL testGraph%clear()
      !Setup test graph - square with a east face circle
      testCoord(:,1)=(/2.1_SRK,2.1_SRK/)
      testCoord(:,2)=(/2.1_SRK,2.5_SRK/)
      testCoord(:,3)=(/2.9_SRK,2.5_SRK/)
      testCoord(:,4)=(/2.9_SRK,2.1_SRK/)
      c0=(/2.8_SRK,2.3_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,4),c0,SQRT(0.05_SRK))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      ASSERT(.NOT.testPolyType%boundsPoly(testPoly2),'circle-line intersection')
      ASSERT(.NOT.testPoly2%boundsPoly(testPolyType),'No points inside')
      CALL testPoly2%clear()
      
      CALL testGraph%clear()
      !Setup test graph - square
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
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      ASSERT(.NOT.testPolyType%boundsPoly(testPoly2),'line-circle intersection')
      ASSERT(.NOT.testPoly2%boundsPoly(testPolyType),'No points inside')
      CALL testPoly2%clear()
      
      CALL testGraph%clear()
      CALL testPolyType%clear()
      CALL point%clear()

      !Setup test graph -NW circle quadrant
      testCoord(:,1)=(/-0.5_SRK,0.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.5_SRK/)
      testCoord(:,3)=(/0.0_SRK,0.0_SRK/)
      DO i=1,3
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      c0=(/0.0_SRK,0.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,1),testCoord(:,2),c0,0.5_SRK)
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,1))
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isInit,'%isInit')
      !Setup smaller arc
      testCoord(:,1)=(/0.5_SRK*COS(7.0_SRK*PI/8.0_SRK),0.5_SRK*SIN(7.0_SRK*PI/8.0_SRK)/)
      testCoord(:,2)=(/0.5_SRK*COS(5.0_SRK*PI/8.0_SRK),0.5_SRK*SIN(5.0_SRK*PI/8.0_SRK)/)
      testCoord(:,3)=(/0.5_SRK*COS(5.0_SRK*PI/8.0_SRK),0.5_SRK*SIN(7.0_SRK*PI/8.0_SRK)/)
      CALL testGraph%clear()
      DO i=1,3
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      c0=(/0.0_SRK,0.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,1),testCoord(:,2),c0,0.5_SRK)
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,1))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isInit,'%isInit')
      CALL testGraph%clear()
      ASSERT(testPolyType%boundsPoly(testPoly2),'arc-arc coincident')
      ASSERT(.NOT. testPoly2%boundsPoly(testPolyType),'No points inside')
      !Setup triangle
      CALL testPoly2%clear()
      testCoord(:,1)=(/-0.15_SRK,0.15_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.25_SRK/)
      testCoord(:,3)=(/0.0_SRK,0.05_SRK/)
      CALL testGraph%clear()
      DO i=1,3
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,1))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isInit,'%isInit')
      ASSERT(testPolyType%boundsPoly(testPoly2),'edge-edge coincident')
      ASSERT(.NOT. testPoly2%boundsPoly(testPolyType),'no points inside')
      CALL testPolyType%clear()
      CALL testPoly2%clear()
      !Setup rectangle-1 quad edge
      testCoord(:,1)=(/-2.0_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-2.0_SRK,2.0_SRK/)
      testCoord(:,3)=(/2.0_SRK,2.0_SRK/)
      testCoord(:,4)=(/2.0_SRK,-2.0_SRK/)
      CALL testGraph%clear()
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      c0=(/4.0_SRK,0.0_SRK/)
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineQuadraticEdge(testCoord(:,3),testCoord(:,4),c0,SQRT(8.0_SRK))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isInit,'%isInit')
      !Setup triangle intersecting quadratic edge
      testCoord(:,1)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,2)=(/2.0_SRK,2.0_SRK/)
      testCoord(:,3)=(/2.0_SRK,-2.0_SRK/)
      CALL testGraph%clear()
      DO i=1,3
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,1))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isInit,'%isInit')
      ASSERT(.NOT. testPolyType%boundsPoly(testPoly2),'Intersect-arc')
      ASSERT(.NOT. testPoly2%boundsPoly(testPolyType),'Intersect-arc')
      CALL testPoly2%clear()

      !Setup triangle with all points on quadratic edge
      testCoord(:,1)=(/1.1715728752538097_SRK,0.0_SRK/)
      testCoord(:,2)=(/2.0_SRK,2.0_SRK/)
      testCoord(:,3)=(/2.0_SRK,-2.0_SRK/)
      CALL testGraph%clear()
      DO i=1,3
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,1))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isInit,'%isInit')
      ASSERT(.NOT. testPolyType%boundsPoly(testPoly2),'On quadratic edge')
      ASSERT(.NOT. testPoly2%boundsPoly(testPolyType),'No points inside')

      CALL testPoly2%clear()
      CALL testPolyType%clear()
      !setup rectangle
      testCoord(:,1)=(/-2.0_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-2.0_SRK,2.0_SRK/)
      testCoord(:,3)=(/2.0_SRK,2.0_SRK/)
      testCoord(:,4)=(/2.0_SRK,-2.0_SRK/)
      CALL testGraph%clear()
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPolyType%set(testGraph)
      !Setup arc segment
      c0=(/-3.0_SRK,-3.0_SRK/)
      testCoord(:,1)=(/-2.0_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-2.0_SRK,-1.5_SRK/)
      testCoord(:,3)=(/-1.5_SRK,-2.0_SRK/)
      CALL testGraph%clear()
       DO i=1,3
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,1.8027756377319946_SRK)
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,1))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isInit,'%isInit')
      ASSERT(testPolyType%boundsPoly(testPoly2),'2 coincident edges')
      ASSERT(.NOT. testPoly2%boundsPoly(testPolyType),'no points inside')
      CALL testPoly2%clear()
      testCoord(:,1)=(/-1.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/-1.0_SRK,1.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,1.0_SRK/)
      testCoord(:,4)=(/1.0_SRK,-1.0_SRK/)
      CALL testGraph%clear()
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isInit,'%isInit')
      ASSERTFAIL(testPolyType%boundsPoly(testPoly2),'smaller square')
      CALL testPolyType%subtractSubVolume(testPoly2)
      ASSERT(ASSOCIATED(testPolyType%subRegions),'Subregion subtracted')
      CALL testGraph%clear()
      testCoord(:,1)=(/-1.0_SRK,0.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/0.0_SRK,-1.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPoly3%set(testGraph)
      ASSERTFAIL(testPoly3%isInit,'%isInit')
      ASSERT(testPoly2%boundsPoly(testPoly3),'inside subregion')
      ASSERT(.NOT. testPolyType%boundsPoly(testPoly3),'inside subregion')
      ASSERT(.NOT. testPoly3%boundsPoly(testPolyType),'no points inside')
      CALL testPoly3%clear()

      !Circle box-intersecting subregion
      c0=(/-1.0_SRK,-1.0_SRK/)
      testCoord(:,1)=(/-1.25_SRK,-1.0_SRK/)
      testCoord(:,2)=(/-1.0_SRK,-0.75_SRK/)
      testCoord(:,3)=(/-0.75_SRK,-1.0_SRK/)
      testCoord(:,4)=(/-1.0_SRK,-1.25_SRK/)
      CALL testGraph%clear()
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,1),testCoord(:,2),c0,0.25_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,0.25_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,3),testCoord(:,4),c0,0.25_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,4),testCoord(:,1),c0,0.25_SRK)
      CALL testPoly3%set(testGraph)
      ASSERTFAIL(testPoly3%isInit,'%isInit')
      ASSERT(.NOT. testPolyType%boundsPoly(testPoly3),'verts on subRegions')
      ASSERT(.NOT. testPoly3%boundsPoly(testPolyType),'no points inside')
      CALL testPoly3%clear()

      !Circle-box intersecting subregions
      c0=(/-1.1_SRK,-1.1_SRK/)
      testCoord(:,1)=(/-1.35_SRK,-1.1_SRK/)
      testCoord(:,2)=(/-1.1_SRK,-0.85_SRK/)
      testCoord(:,3)=(/-0.85_SRK,-1.1_SRK/)
      testCoord(:,4)=(/-1.1_SRK,-1.35_SRK/)
      CALL testGraph%clear()
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,1),testCoord(:,2),c0,0.25_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,0.25_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,3),testCoord(:,4),c0,0.25_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,4),testCoord(:,1),c0,0.25_SRK)
      CALL testPoly3%set(testGraph)
      ASSERTFAIL(testPoly3%isInit,'%isInit')
      ASSERT(.NOT. testPolyType%boundsPoly(testPoly3),'intersects subRegions')
      ASSERT(.NOT. testPoly3%boundsPoly(testPolyType),'no points inside')

    ENDSUBROUTINE testPolyInside
!
!-------------------------------------------------------------------------------
    SUBROUTINE testDoesLineIntersect()
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(LineType) :: line
      
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

      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isinit,'%isinit')
      CALL line%p1%init(COORD=(/0.0_SRK,-2.5_SRK/))
      CALL line%p2%init(COORD=(/2.5_SRK,0.0_SRK/))
      ASSERT(testPolyType%doesLineIntersect(line),'intersect line')
      CALL line%clear()
      CALL line%p1%init(COORD=(/-2.1_SRK,-2.5_SRK/))
      CALL line%p2%init(COORD=(/-2.1_SRK,2.5_SRK/))
      ASSERT(testPolyType%doesLineIntersect(line),'intersect circle')
      CALL line%clear()
      CALL line%p1%init(COORD=(/-5.0_SRK,-2.5_SRK/))
      CALL line%p2%init(COORD=(/-5.0_SRK,2.5_SRK/))
      ASSERT(.NOT.testPolyType%doesLineIntersect(line),'non-intersection')
      CALL line%clear()
      
      CALL testGraph%clear()
      CALL testPolyType%clear()
    ENDSUBROUTINE testDoesLineIntersect
!
!-------------------------------------------------------------------------------
    SUBROUTINE testDoesPolyIntersect()
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(PolygonType) :: testPoly2
      
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
      CALL testPolyType%set(testGraph)
      ASSERTFAIL(testPolyType%isinit,'%isinit')
      
      CALL testGraph%clear()
      CALL testPoly2%clear()
      testCoord(:,1)=(/0.0_SRK,-4.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/4.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/4.0_SRK,-4.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      
      ASSERT(testPolyType%doesPolyIntersect(testPoly2),'intersect poly line-line')
      ASSERT(testPoly2%doesPolyIntersect(testPolyType),'reverse intersect poly line-line')
      
      CALL testGraph%clear()
      CALL testPoly2%clear()
      testCoord(:,1)=(/-3.2_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-3.2_SRK,2.0_SRK/)
      testCoord(:,3)=(/-2.2_SRK,2.0_SRK/)
      testCoord(:,4)=(/-2.2_SRK,-2.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      
      ASSERT(testPolyType%doesPolyIntersect(testPoly2),'intersect poly line-circle')
      ASSERT(testPoly2%doesPolyIntersect(testPolyType),'reverse intersect poly line-circle')
      
      CALL testGraph%clear()
      CALL testPoly2%clear()
      testCoord(:,1)=(/0.0_SRK,-5.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,-3.0_SRK/)
      testCoord(:,3)=(/4.0_SRK,-3.0_SRK/)
      testCoord(:,4)=(/4.0_SRK,-5.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      c0=(/2.0_SRK,-4.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,2), &
        testCoord(:,3),c0,SQRT(5.0_SRK))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      
      ASSERT(testPolyType%doesPolyIntersect(testPoly2),'intersect poly circle-line')
      ASSERT(testPoly2%doesPolyIntersect(testPolyType),'reverse intersect poly circle-line')
      
      CALL testGraph%clear()
      CALL testPoly2%clear()
      testCoord(:,1)=(/-3.2_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-3.2_SRK,2.0_SRK/)
      testCoord(:,3)=(/-2.2_SRK,2.0_SRK/)
      testCoord(:,4)=(/-2.2_SRK,-2.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      c0=(/-2.7_SRK,0.0_SRK/)
      CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,4),c0,SQRT(4.25_SRK))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      
      ASSERT(testPolyType%doesPolyIntersect(testPoly2),'intersect poly circle-circle')
      ASSERT(testPoly2%doesPolyIntersect(testPolyType),'reverse intersect poly circle-circle')
      
      CALL testGraph%clear()
      CALL testPoly2%clear()
      testCoord(:,1)=(/-32.0_SRK,-2.0_SRK/)
      testCoord(:,2)=(/-32.0_SRK,2.0_SRK/)
      testCoord(:,3)=(/-22.0_SRK,2.0_SRK/)
      testCoord(:,4)=(/-22.0_SRK,-2.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testPoly2%set(testGraph)
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      
      ASSERT(.NOT.testPolyType%doesPolyIntersect(testPoly2),'non-intersection')
      ASSERT(.NOT.testPoly2%doesPolyIntersect(testPolyType),'non-intersection')
      
      CALL testGraph%clear()
      CALL testPoly2%clear()
      CALL testPolyType%clear()
    ENDSUBROUTINE testDoesPolyIntersect
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

      CALL point1%clear()
      CALL point2%clear()
      CALL line%clear()
      CALL point1%init(DIM=2,X=-1.0_SRK,Y=-1.0_SRK)
      CALL point2%init(DIM=2,X=1.0_SRK,Y=0.0_SRK)
      CALL line%set(point1,point2)
      CALL testPolyType%intersectLine(line,points)
      ASSERTFAIL(SIZE(points) == 2,'npoints for vertex end point')
      bool=(points(1)%coord(1) .APPROXEQA. -1.0_SRK) .AND. &
        (points(1)%coord(2) .APPROXEQA. -1.0_SRK)
      ASSERT(bool,'point 1 intersection')

      CALL point1%clear()
      CALL point2%clear()
      CALL line%clear()
      CALL point1%init(DIM=2,X=0.0_SRK,Y=3.0_SRK)
      CALL point2%init(DIM=2,X=0.0_SRK,Y=-2.0_SRK)
      CALL line%set(point1,point2)
      CALL testPolyType%intersectLine(line,points)
      ASSERTFAIL(SIZE(points) == 2,'npoints for through vertex')
      bool=(points(1)%coord(1) .APPROXEQA. 0.0_SRK) .AND. &
        (points(1)%coord(2) .APPROXEQA. 2.0_SRK)
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
!
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
      TYPE(PolygonType) :: testPoly2
      
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
      
      !Setup subregion graph - square with a full circle
      CALL testGraph2%clear()
      testCoord(:,1)=(/-1.0_SRK,-1.0_SRK/)/SQRT(2.0_SRK)
      testCoord(:,2)=(/-1.0_SRK,1.0_SRK/)/SQRT(2.0_SRK)
      testCoord(:,3)=(/1.0_SRK,1.0_SRK/)/SQRT(2.0_SRK)
      testCoord(:,4)=(/1.0_SRK,-1.0_SRK/)/SQRT(2.0_SRK)
      c0=(/0.0_SRK,0.0_SRK/)
      DO i=1,4
        CALL testGraph2%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph2%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph2%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph2%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph2%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testGraph2%defineQuadraticEdge(testCoord(:,1), &
        testCoord(:,2),c0,1.0_SRK)
      CALL testGraph2%defineQuadraticEdge(testCoord(:,2), &
        testCoord(:,3),c0,1.0_SRK)
      CALL testGraph2%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,4),c0,1.0_SRK)
      CALL testGraph2%defineQuadraticEdge(testCoord(:,4), &
        testCoord(:,1),c0,1.0_SRK)
      CALL testPoly2%clear()
      CALL testPoly2%set(testGraph2)
      ASSERTFAIL(testPoly2%isinit,'%isinit')
      CALL testPolyType%subtractSubVolume(testPoly2)
      ASSERTFAIL(ASSOCIATED(testPolyType%subRegions),'%subRegions')
      
      !Add the subregion graph to the main reference graph
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      CALL testGraph%defineQuadraticEdge(testCoord(:,1), &
        testCoord(:,2),c0,1.0_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,2), &
        testCoord(:,3),c0,1.0_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,3), &
        testCoord(:,4),c0,1.0_SRK)
      CALL testGraph%defineQuadraticEdge(testCoord(:,4), &
        testCoord(:,1),c0,1.0_SRK)
      
      CALL testPolyType%generateGraph(testGraph2)
      ASSERT(testGraph == testGraph2,'Graph with sub-region is equal')
    ENDSUBROUTINE testGenerateGraph
!
ENDPROGRAM testGeom_Poly
