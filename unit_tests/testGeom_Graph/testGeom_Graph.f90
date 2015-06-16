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
PROGRAM testGeom_Graph
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV  
  USE UnitTest
  USE IntrType
  USE Allocs
  USE Geom_Graph
  
  IMPLICIT NONE
  
  TYPE(GraphType) :: testGraph,testGraph2
  
  CREATE_TEST('TEST GEOM_GRAPH')
  
  REGISTER_SUBTEST('Uninit',testUninit)
  REGISTER_SUBTEST('%nVert',testNVert)
  REGISTER_SUBTEST('%nEdge',testNEdge)
  REGISTER_SUBTEST('%clear',testClear)
  REGISTER_SUBTEST('%insertVertex',testInsertVertex)
  REGISTER_SUBTEST('%getVertIndex',testGetVertIndex)
  REGISTER_SUBTEST('%defineEdge',testDefineEdge)
  REGISTER_SUBTEST('%defineQuadEdge',testDefineQuadEdge)
  REGISTER_SUBTEST('%editToVTK',testVTK)
  REGISTER_SUBTEST('%nAdjacent',testNAdjacent)
  REGISTER_SUBTEST('%getAdjacentVert',testGetAdjVert)
  REGISTER_SUBTEST('%getCWMostVert',testCWVert)
  REGISTER_SUBTEST('%getCCWMostVert',testCCWVert)
  REGISTER_SUBTEST('%isMinimumCycle',testIsMinCyc)
  REGISTER_SUBTEST('%removeVertex',testRemVert)
  REGISTER_SUBTEST('%removeEdge',testRemEdge)
  REGISTER_SUBTEST('%removeFilament',testRemFil)
  REGISTER_SUBTEST('%getMCB',testGetMCB)
  REGISTER_SUBTEST('OPERATOR(==)',testIsEqual)
  REGISTER_SUBTEST('OPERATOR(+)',testAddition)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testUninit()
      ASSERT(.NOT.ALLOCATED(testGraph%vertices),'%vertices')
      ASSERT(.NOT.ALLOCATED(testGraph%edgeMatrix),'%edgeMatrix')
      ASSERT(.NOT.ALLOCATED(testGraph%quadEdges),'%quadEdges')
    ENDSUBROUTINE testUninit
!
!-------------------------------------------------------------------------------
    SUBROUTINE testClear()
      CALL dmallocA(testGraph%vertices,1,1)
      CALL testGraph%clear()
      CALL testUninit()
      CALL dmallocA(testGraph%edgeMatrix,1,1)
      CALL testGraph%clear()
      CALL testUninit()
      CALL dmallocA(testGraph%quadEdges,1,1,1)
      CALL testGraph%clear()
      CALL testUninit()
    ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testNVert()
      ASSERT(testGraph%nVert() == 0,'uninit')
      CALL dmallocA(testGraph%vertices,1,8)
      ASSERT(testGraph%nVert() == 8,'8')
      CALL testGraph%clear()
      ASSERT(testGraph%nVert() == 0,'post clear')
    ENDSUBROUTINE testNVert
!
!-------------------------------------------------------------------------------
    SUBROUTINE testNEdge()
      ASSERT(testGraph%nEdge() == 0,'uninit')
      CALL dmallocA(testGraph%edgeMatrix,3,3)
      ASSERT(testGraph%nEdge() == 0,'0')
      testGraph%EdgeMatrix(1,2)=1
      testGraph%EdgeMatrix(2,1)=1
      ASSERT(testGraph%nEdge() == 1,'0')
      testGraph%EdgeMatrix(3,2)=-1
      testGraph%EdgeMatrix(2,3)=-1
      ASSERT(testGraph%nEdge() == 2,'0')
      CALL testGraph%clear()
      ASSERT(testGraph%nEdge() == 0,'post clear')
    ENDSUBROUTINE testNEdge
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInsertVertex()
      LOGICAL(SBK) :: bool
      REAL(SRK) :: testCoord(2,6)

      testCoord(:,1)=(/1.0_SRK,2.0_SRK/)
      testCoord(:,2)=(/2.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/3.0_SRK,2.5_SRK/)
      testCoord(:,4)=(/3.0_SRK,3.0_SRK/)
      testCoord(:,5)=(/3.0_SRK,4.0_SRK/)
      testCoord(:,6)=(/4.0_SRK,1.0_SRK/)

      COMPONENT_TEST('First Vertex')
      CALL testGraph%insertVertex(testCoord(:,2))
      ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices)')
      ASSERT(SIZE(testGraph%vertices,DIM=1) == 2,'SIZE(%vertices,DIM=1)')
      ASSERT(SIZE(testGraph%vertices,DIM=2) == 1,'SIZE(%vertices,DIM=2)')
      bool=ALL(testGraph%vertices == testCoord(:,2:2))
      ASSERT(bool,'%vertices (values)')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
      ASSERT(bool,'%edgeMatrix square')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
      ASSERT(bool,'%edgeMatrix size vertices')

      COMPONENT_TEST('Last Vertex')
      CALL testGraph%insertVertex(testCoord(:,6))
      ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices)')
      ASSERT(SIZE(testGraph%vertices,DIM=1) == 2,'SIZE(%vertices,DIM=1)')
      ASSERT(SIZE(testGraph%vertices,DIM=2) == 2,'SIZE(%vertices,DIM=2)')
      bool=ALL(testGraph%vertices == testCoord(:,(/2,6/)))
      ASSERT(bool,'%vertices (values)')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
      ASSERT(bool,'%edgeMatrix square')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
      ASSERT(bool,'%edgeMatrix size vertices')

      COMPONENT_TEST('Beginning Vertex')
      CALL testGraph%insertVertex(testCoord(:,1))
      ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices)')
      ASSERT(SIZE(testGraph%vertices,DIM=1) == 2,'SIZE(%vertices,DIM=1)')
      ASSERT(SIZE(testGraph%vertices,DIM=2) == 3,'SIZE(%vertices,DIM=2)')
      bool=ALL(testGraph%vertices == testCoord(:,(/1,2,6/)))
      ASSERT(bool,'%vertices (values) 3')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
      ASSERT(bool,'%edgeMatrix square')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
      ASSERT(bool,'%edgeMatrix size vertices')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
      ASSERT(bool,'%edgeMatrix square')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
      ASSERT(bool,'%edgeMatrix size vertices')

      COMPONENT_TEST('Insertion Vertex')
      CALL testGraph%insertVertex(testCoord(:,4))
      ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices)')
      ASSERT(SIZE(testGraph%vertices,DIM=1) == 2,'SIZE(%vertices,DIM=1)')
      ASSERT(SIZE(testGraph%vertices,DIM=2) == 4,'SIZE(%vertices,DIM=2)')
      bool=ALL(testGraph%vertices == testCoord(:,(/1,2,4,6/)))
      ASSERT(bool,'%vertices (values)')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
      ASSERT(bool,'%edgeMatrix square')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
      ASSERT(bool,'%edgeMatrix size vertices')

      COMPONENT_TEST('Equal X before')
      CALL testGraph%insertVertex(testCoord(:,3))
      ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices)')
      ASSERT(SIZE(testGraph%vertices,DIM=1) == 2,'SIZE(%vertices,DIM=1)')
      ASSERT(SIZE(testGraph%vertices,DIM=2) == 5,'SIZE(%vertices,DIM=2)')
      bool=ALL(testGraph%vertices == testCoord(:,(/1,2,3,4,6/)))
      ASSERT(bool,'%vertices (values) 5')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
      ASSERT(bool,'%edgeMatrix square')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
      ASSERT(bool,'%edgeMatrix size vertices')

      COMPONENT_TEST('Equal X after')
      CALL testGraph%insertVertex(testCoord(:,5))
      ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices)')
      ASSERT(SIZE(testGraph%vertices,DIM=1) == 2,'SIZE(%vertices,DIM=1)')
      ASSERT(SIZE(testGraph%vertices,DIM=2) == 6,'SIZE(%vertices,DIM=2)')
      bool=ALL(testGraph%vertices == testCoord)
      ASSERT(bool,'%vertices (values)')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
      ASSERT(bool,'%edgeMatrix square')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
      ASSERT(bool,'%edgeMatrix size vertices')

      COMPONENT_TEST('Duplicate')
      CALL testGraph%insertVertex(testCoord(:,3))
      ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices)')
      ASSERT(SIZE(testGraph%vertices,DIM=1) == 2,'SIZE(%vertices,DIM=1)')
      ASSERT(SIZE(testGraph%vertices,DIM=2) == 6,'SIZE(%vertices,DIM=2)')
      bool=ALL(testGraph%vertices == testCoord)
      ASSERT(bool,'%vertices (values)')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
      ASSERT(bool,'%edgeMatrix square')
      bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
      ASSERT(bool,'%edgeMatrix size vertices')

      CALL testGraph%clear()
    ENDSUBROUTINE testInsertVertex
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetVertIndex()
      REAL(SRK) :: testCoord(2,6)

      testCoord(:,1)=(/1.0_SRK,2.0_SRK/)
      testCoord(:,2)=(/2.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/3.0_SRK,2.5_SRK/)
      testCoord(:,4)=(/3.0_SRK,3.0_SRK/)
      testCoord(:,5)=(/3.0_SRK,4.0_SRK/)
      testCoord(:,6)=(/4.0_SRK,1.0_SRK/)

      COMPONENT_TEST('Empty graph')
      ASSERT(testGraph%getVertIndex(testCoord(:,1)) == -1,'1')
      ASSERT(testGraph%getVertIndex(testCoord(:,2)) == -1,'2')
      ASSERT(testGraph%getVertIndex(testCoord(:,3)) == -1,'3')
      ASSERT(testGraph%getVertIndex(testCoord(:,4)) == -1,'4')
      ASSERT(testGraph%getVertIndex(testCoord(:,5)) == -1,'5')
      ASSERT(testGraph%getVertIndex(testCoord(:,6)) == -1,'6')

      COMPONENT_TEST('First vertex')
      CALL testGraph%insertVertex(testCoord(:,2))
      ASSERT(testGraph%getVertIndex(testCoord(:,1)) == -1,'1')
      ASSERT(testGraph%getVertIndex(testCoord(:,2)) ==  1,'2')
      ASSERT(testGraph%getVertIndex(testCoord(:,3)) == -1,'3')
      ASSERT(testGraph%getVertIndex(testCoord(:,4)) == -1,'4')
      ASSERT(testGraph%getVertIndex(testCoord(:,5)) == -1,'5')
      ASSERT(testGraph%getVertIndex(testCoord(:,6)) == -1,'6')

      COMPONENT_TEST('Last vertex')
      CALL testGraph%insertVertex(testCoord(:,6))
      ASSERT(testGraph%getVertIndex(testCoord(:,1)) == -1,'1')
      ASSERT(testGraph%getVertIndex(testCoord(:,2)) ==  1,'2')
      ASSERT(testGraph%getVertIndex(testCoord(:,3)) == -1,'3')
      ASSERT(testGraph%getVertIndex(testCoord(:,4)) == -1,'4')
      ASSERT(testGraph%getVertIndex(testCoord(:,5)) == -1,'5')
      ASSERT(testGraph%getVertIndex(testCoord(:,6)) ==  2,'6')

      COMPONENT_TEST('Beginning vertex')
      CALL testGraph%insertVertex(testCoord(:,1))
      ASSERT(testGraph%getVertIndex(testCoord(:,1)) ==  1,'1')
      ASSERT(testGraph%getVertIndex(testCoord(:,2)) ==  2,'2')
      ASSERT(testGraph%getVertIndex(testCoord(:,3)) == -1,'3')
      ASSERT(testGraph%getVertIndex(testCoord(:,4)) == -1,'4')
      ASSERT(testGraph%getVertIndex(testCoord(:,5)) == -1,'5')
      ASSERT(testGraph%getVertIndex(testCoord(:,6)) ==  3,'6')

      COMPONENT_TEST('Insertion vertex')
      CALL testGraph%insertVertex(testCoord(:,4))
      ASSERT(testGraph%getVertIndex(testCoord(:,1)) ==  1,'1')
      ASSERT(testGraph%getVertIndex(testCoord(:,2)) ==  2,'2')
      ASSERT(testGraph%getVertIndex(testCoord(:,3)) == -1,'3')
      ASSERT(testGraph%getVertIndex(testCoord(:,4)) ==  3,'4')
      ASSERT(testGraph%getVertIndex(testCoord(:,5)) == -1,'5')
      ASSERT(testGraph%getVertIndex(testCoord(:,6)) ==  4,'6')

      COMPONENT_TEST('Equal X before')
      CALL testGraph%insertVertex(testCoord(:,3))
      ASSERT(testGraph%getVertIndex(testCoord(:,1)) ==  1,'1')
      ASSERT(testGraph%getVertIndex(testCoord(:,2)) ==  2,'2')
      ASSERT(testGraph%getVertIndex(testCoord(:,3)) ==  3,'3')
      ASSERT(testGraph%getVertIndex(testCoord(:,4)) ==  4,'4')
      ASSERT(testGraph%getVertIndex(testCoord(:,5)) == -1,'5')
      ASSERT(testGraph%getVertIndex(testCoord(:,6)) ==  5,'6')

      COMPONENT_TEST('Equal X after')
      CALL testGraph%insertVertex(testCoord(:,5))
      ASSERT(testGraph%getVertIndex(testCoord(:,1)) ==  1,'1')
      ASSERT(testGraph%getVertIndex(testCoord(:,2)) ==  2,'2')
      ASSERT(testGraph%getVertIndex(testCoord(:,3)) ==  3,'3')
      ASSERT(testGraph%getVertIndex(testCoord(:,4)) ==  4,'4')
      ASSERT(testGraph%getVertIndex(testCoord(:,5)) ==  5,'5')
      ASSERT(testGraph%getVertIndex(testCoord(:,6)) ==  6,'6')

      CALL testGraph%clear()
    ENDSUBROUTINE testGetVertIndex
!
!-------------------------------------------------------------------------------
    SUBROUTINE testDefineEdge()
      REAL(SRK) :: testCoord(2,6)

      testCoord(:,1)=(/1.0_SRK,2.0_SRK/)
      testCoord(:,2)=(/2.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/3.0_SRK,2.5_SRK/)
      testCoord(:,4)=(/3.0_SRK,3.0_SRK/)
      testCoord(:,5)=(/3.0_SRK,4.0_SRK/)
      testCoord(:,6)=(/4.0_SRK,1.0_SRK/)

      !No vertices
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      ASSERT(.NOT.ALLOCATED(testGraph%edgeMatrix),'no vertices')

      !Same Point
      CALL testGraph%insertVertex(testCoord(:,1))
      CALL testGraph%insertVertex(testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,1))
      ASSERT(ALL(testGraph%edgeMatrix == 0),'same point')
      
      !Missing Point
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      ASSERT(ALL(testGraph%edgeMatrix == 0),'missing point 1')
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,1))
      ASSERT(ALL(testGraph%edgeMatrix == 0),'missing point 2')
      
      !Valid Point
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      ASSERT(testGraph%edgeMatrix(1,2) == 1,'edge 1')
      ASSERT(testGraph%nEdge() == 1,'nEdge')
      CALL symEdgeCheck()

      !Test Resize
      COMPONENT_TEST('Post Insertion')
      CALL testGraph%insertVertex(testCoord(:,2))
      ASSERT(testGraph%edgeMatrix(1,3) == 1,'edge 1')
      ASSERT(testGraph%nEdge() == 1,'nEdge')
      CALL symEdgeCheck()

      CALL testGraph%clear()
    ENDSUBROUTINE testDefineEdge
!
!-------------------------------------------------------------------------------
    SUBROUTINE testDefineQuadEdge()
      REAL(SRK),PARAMETER :: r=1.0_SRK,c0(2)=0.0_SRK  
      LOGICAL(SBK) :: bool
      REAL(SRK) :: testCoord(2,5)

      testCoord(:,1)=(/-1.0_SRK,0.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,-1.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/0.0_SRK,1.1_SRK/)
      testCoord(:,5)=(/-0.5_SRK,-0.5_SRK/)

      COMPONENT_TEST('No vertices')
      CALL testGraph%defineQuadraticEdge(testCoord(:,1),testCoord(:,2),c0,r)
      ASSERT(.NOT.ALLOCATED(testGraph%edgeMatrix),'no eMatrix')
      ASSERT(.NOT.ALLOCATED(testGraph%quadEdges),'no qEdges')
      
      COMPONENT_TEST('Same Point')
      CALL testGraph%insertVertex(testCoord(:,1))
      CALL testGraph%insertVertex(testCoord(:,2))
      CALL testGraph%defineQuadraticEdge(testCoord(:,1),testCoord(:,1),c0,r)
      ASSERT(ALL(testGraph%edgeMatrix == 0),'no eMatrix')
      ASSERT(ALL(testGraph%quadEdges == 0.0_SRK),'no qEdges')
      
      COMPONENT_TEST('Missing Points')
      CALL testGraph%defineQuadraticEdge(testCoord(:,3),testCoord(:,2),c0,r)
      ASSERT(ALL(testGraph%edgeMatrix == 0),'no eMatrix 1')
      ASSERT(ALL(testGraph%quadEdges == 0.0_SRK),'no qEdges 1')
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,r)
      ASSERT(ALL(testGraph%edgeMatrix == 0),'no eMatrix 2')
      ASSERT(ALL(testGraph%quadEdges == 0.0_SRK),'no qEdges 2')
      
      COMPONENT_TEST('Valid Edge')
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,1),c0,r)
      ASSERT(testGraph%nEdge() == 1,'nEdge')
      ASSERT(testGraph%edgeMatrix(1,2) == -1,'edge 1')
      bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 1')
      CALL symEdgeCheck()
      
      COMPONENT_TEST('Post vertex append')
      CALL testGraph%insertVertex(testCoord(:,3))
      ASSERT(testGraph%nEdge() == 1,'nEdge')
      ASSERT(testGraph%edgeMatrix(1,2) == -1,'edge 1')
      bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 1')
      CALL symEdgeCheck()
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,r)
      ASSERT(testGraph%nEdge() == 2,'nEdge')
      ASSERT(testGraph%edgeMatrix(1,2) == -1,'edge 1')
      bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 1')
      ASSERT(testGraph%edgeMatrix(2,3) == -1,'edge 2')
      bool=ALL(testGraph%quadEdges(:,2,3) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 2')
      CALL symEdgeCheck()

      COMPONENT_TEST('Post vertex insertion')
      CALL testGraph%insertVertex(testCoord(:,4))
      ASSERT(testGraph%nEdge() == 2,'nEdge')
      ASSERT(testGraph%edgeMatrix(1,2) == -1,'edge 1')
      bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 1')
      ASSERT(testGraph%edgeMatrix(2,4) == -1,'edge 2')
      bool=ALL(testGraph%quadEdges(:,2,4) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 2')
      CALL symEdgeCheck()
      
      COMPONENT_TEST('Mixed edges')
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      ASSERT(testGraph%nEdge() == 4,'nEdge')
      ASSERT(testGraph%edgeMatrix(1,2) == -1,'edge 1')
      bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 1')
      ASSERT(testGraph%edgeMatrix(2,4) == -1,'edge 2')
      bool=ALL(testGraph%quadEdges(:,2,4) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 2')
      ASSERT(testGraph%edgeMatrix(1,3) == 1,'edge 3')
      bool=ALL(testGraph%quadEdges(:,1,3) == 0.0_SRK)
      ASSERT(bool,'quadEdge 3')
      ASSERT(testGraph%edgeMatrix(3,4) == 1,'edge 4')
      bool=ALL(testGraph%quadEdges(:,3,4) == 0.0_SRK)
      ASSERT(bool,'quadEdge 4')
      CALL symEdgeCheck()

      COMPONENT_TEST('Mixed edge post insertion')
      CALL testGraph%insertVertex(testCoord(:,5))
      ASSERT(testGraph%nEdge() == 4,'nEdge')
      ASSERT(testGraph%edgeMatrix(1,3) == -1,'edge 1')
      bool=ALL(testGraph%quadEdges(:,1,3) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 1')
      ASSERT(testGraph%edgeMatrix(3,5) == -1,'edge 2')
      bool=ALL(testGraph%quadEdges(:,3,5) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 2')
      ASSERT(testGraph%edgeMatrix(1,4) == 1,'edge 3')
      bool=ALL(testGraph%quadEdges(:,1,4) == 0.0_SRK)
      ASSERT(bool,'quadEdge 3')
      ASSERT(testGraph%edgeMatrix(4,5) == 1,'edge 4')
      bool=ALL(testGraph%quadEdges(:,4,5) == 0.0_SRK)
      ASSERT(bool,'quadEdge 4')
      CALL symEdgeCheck()

      COMPONENT_TEST('Vertices not on circle')
      CALL testGraph%defineQuadraticEdge(testCoord(:,1),testCoord(:,5),c0,r)
      ASSERT(testGraph%nEdge() == 4,'nEdge')
      ASSERT(testGraph%edgeMatrix(1,3) == -1,'edge 1')
      bool=ALL(testGraph%quadEdges(:,1,3) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 1')
      ASSERT(testGraph%edgeMatrix(3,5) == -1,'edge 2')
      bool=ALL(testGraph%quadEdges(:,3,5) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 2')
      ASSERT(testGraph%edgeMatrix(1,4) == 1,'edge 3')
      bool=ALL(testGraph%quadEdges(:,1,4) == 0.0_SRK)
      ASSERT(bool,'quadEdge 3')
      ASSERT(testGraph%edgeMatrix(4,5) == 1,'edge 4')
      bool=ALL(testGraph%quadEdges(:,4,5) == 0.0_SRK)
      ASSERT(bool,'quadEdge 4')
      ASSERT(testGraph%edgeMatrix(1,2) == 0,'edge 5')
      bool=ALL(testGraph%quadEdges(:,1,2) == 0.0_SRK)
      ASSERT(bool,'quadEdge 5')
      CALL symEdgeCheck()

      COMPONENT_TEST('Circle inflection')
      CALL testGraph%clear()
      CALL testGraph%insertVertex(testCoord(:,1))
      CALL testGraph%insertVertex(testCoord(:,2))
      CALL testGraph%insertVertex(testCoord(:,3))
      CALL testGraph%defineQuadraticEdge(testCoord(:,1),testCoord(:,2),c0,-r)
      CALL testGraph%defineQuadraticEdge(testCoord(:,1),testCoord(:,3),c0,-r)
      ASSERT(testGraph%nEdge() == 2,'nEdge')
      ASSERT(testGraph%edgeMatrix(1,2) == -1,'edge 1')
      bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 1')
      ASSERT(testGraph%edgeMatrix(1,3) == -1,'edge 2')
      bool=ALL(testGraph%quadEdges(:,1,3) == (/c0(1),c0(2),-r/))
      ASSERT(bool,'quadEdge 2')
      CALL symEdgeCheck()

      CALL testGraph%clear()
    ENDSUBROUTINE testDefineQuadEdge
!
!-------------------------------------------------------------------------------
    SUBROUTINE testVTK()
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r
      
      !Setup test graph
      testCoord(:,1)=(/0.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/1.25_SRK,0.5_SRK/)
      testCoord(:,5)=(/1.50_SRK,-0.5_SRK/)
      testCoord(:,6)=(/1.75_SRK,-1.0_SRK/)
      testCoord(:,7)=(/2.00_SRK,-0.75_SRK/)
      testCoord(:,8)=(/2.25_SRK,-0.25_SRK/)
      testCoord(:,9)=(/3.00_SRK,-0.10_SRK/)
      c0=(/0.5_SRK,0.0_SRK/)
      r=0.5_SRK
      DO i=1,9
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,r)
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
      CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))
      
      CALL testGraph%editToVTK('testVTK.vtk')

      CALL testGraph%clear()
    ENDSUBROUTINE testVTK
!
!-------------------------------------------------------------------------------
    SUBROUTINE testNAdjacent()
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r
      
      !Setup test graph
      testCoord(:,1)=(/0.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/1.25_SRK,0.5_SRK/)
      testCoord(:,5)=(/1.50_SRK,-0.5_SRK/)
      testCoord(:,6)=(/1.51_SRK,-1.0_SRK/)
      testCoord(:,7)=(/1.52_SRK,-0.75_SRK/)
      testCoord(:,8)=(/1.53_SRK,-0.50_SRK/)
      testCoord(:,9)=(/1.54_SRK,0.50_SRK/)
      c0=(/0.5_SRK,0.0_SRK/)
      r=0.5_SRK
      DO i=1,9
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,r)
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
      CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))
      
      ASSERT(testGraph%nAdjacent(0) == 0,'0')
      ASSERT(testGraph%nAdjacent(10) == 0,'n+1')
      
      ASSERT(testGraph%nAdjacent(1) == 0,'v1')
      ASSERT(testGraph%nAdjacent(2) == 1,'v2')
      ASSERT(testGraph%nAdjacent(3) == 3,'v3')
      ASSERT(testGraph%nAdjacent(4) == 1,'v4')
      ASSERT(testGraph%nAdjacent(5) == 4,'v5')
      ASSERT(testGraph%nAdjacent(6) == 1,'v6')
      ASSERT(testGraph%nAdjacent(7) == 1,'v7')
      ASSERT(testGraph%nAdjacent(8) == 2,'v8')
      ASSERT(testGraph%nAdjacent(9) == 1,'v9')

      CALL testGraph%clear()
    ENDSUBROUTINE testNAdjacent
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetAdjVert()
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r
      
      !Setup test graph
      testCoord(:,1)=(/0.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/1.25_SRK,0.5_SRK/)
      testCoord(:,5)=(/1.50_SRK,-0.5_SRK/)
      testCoord(:,6)=(/1.51_SRK,-1.0_SRK/)
      testCoord(:,7)=(/1.52_SRK,-0.75_SRK/)
      testCoord(:,8)=(/1.53_SRK,-0.50_SRK/)
      testCoord(:,9)=(/1.54_SRK,0.50_SRK/)
      c0=(/0.5_SRK,0.0_SRK/)
      r=0.5_SRK
      DO i=1,9
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,r)
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
      CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))
      
      ASSERT(testGraph%getAdjacentVert(0,1) == 0,'0')
      ASSERT(testGraph%getAdjacentVert(10,1) == 0,'n+1')
      ASSERT(testGraph%getAdjacentVert(1,0) == 0,'edge 0')
      ASSERT(testGraph%getAdjacentVert(1,10) == 0,'edge 10')

      ASSERT(testGraph%getAdjacentVert(1,1) == 0,'(1,1)')
      ASSERT(testGraph%getAdjacentVert(2,1) == 3,'(2,1)')
      ASSERT(testGraph%getAdjacentVert(2,2) == 0,'(2,2)')
      ASSERT(testGraph%getAdjacentVert(3,1) == 2,'(3,1)')
      ASSERT(testGraph%getAdjacentVert(3,2) == 4,'(3,2)')
      ASSERT(testGraph%getAdjacentVert(3,3) == 5,'(3,3)')
      ASSERT(testGraph%getAdjacentVert(3,4) == 0,'(3,4)')
      ASSERT(testGraph%getAdjacentVert(4,1) == 3,'(4,1)')
      ASSERT(testGraph%getAdjacentVert(4,2) == 0,'(4,2)')
      ASSERT(testGraph%getAdjacentVert(5,1) == 3,'(5,1)')
      ASSERT(testGraph%getAdjacentVert(5,2) == 6,'(5,2)')
      ASSERT(testGraph%getAdjacentVert(5,3) == 7,'(5,3)')
      ASSERT(testGraph%getAdjacentVert(5,4) == 8,'(5,4)')
      ASSERT(testGraph%getAdjacentVert(5,5) == 0,'(5,5)')
      ASSERT(testGraph%getAdjacentVert(6,1) == 5,'(6,1)')
      ASSERT(testGraph%getAdjacentVert(6,2) == 0,'(6,2)')
      ASSERT(testGraph%getAdjacentVert(7,1) == 5,'(7,1)')
      ASSERT(testGraph%getAdjacentVert(7,2) == 0,'(7,2)')
      ASSERT(testGraph%getAdjacentVert(8,1) == 5,'(8,1)')
      ASSERT(testGraph%getAdjacentVert(8,2) == 9,'(8,2)')
      ASSERT(testGraph%getAdjacentVert(8,3) == 0,'(8,3)')
      ASSERT(testGraph%getAdjacentVert(9,1) == 8,'(9,1)')
      ASSERT(testGraph%getAdjacentVert(9,2) == 0,'(9,2)')

      CALL testGraph%clear()
    ENDSUBROUTINE testGetAdjVert
!
!-------------------------------------------------------------------------------
    SUBROUTINE testCWVert()
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r
      
      !Setup test graph
      testCoord(:,1)=(/0.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/1.25_SRK,0.5_SRK/)
      testCoord(:,5)=(/1.50_SRK,-0.5_SRK/)
      testCoord(:,6)=(/1.75_SRK,-1.0_SRK/)
      testCoord(:,7)=(/2.00_SRK,-0.75_SRK/)
      testCoord(:,8)=(/2.25_SRK,-0.25_SRK/)
      testCoord(:,9)=(/3.00_SRK,-0.10_SRK/)
      c0=(/0.5_SRK,0.0_SRK/)
      r=0.5_SRK
      DO i=1,9
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,r)
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
      CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))

      COMPONENT_TEST('Bad verts')
      ASSERT(testGraph%getCWMostVert(0,1) == 0,'(0,1)')
      ASSERT(testGraph%getCWMostVert(10,1) == 0,'(10,1)')
      ASSERT(testGraph%getCWMostVert(1,-1) == 0,'(1,-1)')
      ASSERT(testGraph%getCWMostVert(1,10) == 0,'(1,10)')

      COMPONENT_TEST('Isolated vert')
      ASSERT(testGraph%getCWMostVert(1,1) == 0,'')

      COMPONENT_TEST('End vert')
      ASSERT(testGraph%getCWMostVert(0,2) == 3,'(0,2)')
      ASSERT(testGraph%getCWMostVert(1,2) == 0,'(1,2)')
      ASSERT(testGraph%getCWMostVert(2,2) == 3,'(2,2)')
      ASSERT(testGraph%getCWMostVert(3,2) == 0,'(3,2)')

      COMPONENT_TEST('Branch vert')
      ASSERT(testGraph%getCWMostVert(0,3) == 5,'(0,3)')
      ASSERT(testGraph%getCWMostVert(1,3) == 0,'(1,3)')
      ASSERT(testGraph%getCWMostVert(2,3) == 5,'(2,3)')
      ASSERT(testGraph%getCWMostVert(3,3) == 5,'(3,3)')
      ASSERT(testGraph%getCWMostVert(4,3) == 2,'(4,3)')
      ASSERT(testGraph%getCWMostVert(5,3) == 4,'(5,3)')
      
      CALL testGraph%clear()
    ENDSUBROUTINE testCWVert
!
!-------------------------------------------------------------------------------
    SUBROUTINE testCCWVert()
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r
      
      !Setup test graph
      testCoord(:,1)=(/0.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/1.25_SRK,0.5_SRK/)
      testCoord(:,5)=(/1.50_SRK,-0.5_SRK/)
      testCoord(:,6)=(/1.75_SRK,-1.0_SRK/)
      testCoord(:,7)=(/2.00_SRK,-0.75_SRK/)
      testCoord(:,8)=(/2.25_SRK,-0.25_SRK/)
      testCoord(:,9)=(/3.00_SRK,-0.10_SRK/)
      c0=(/0.5_SRK,0.0_SRK/)
      r=0.5_SRK
      DO i=1,9
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,r)
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
      CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))

      COMPONENT_TEST('Bad verts')
      ASSERT(testGraph%getCCWMostVert(0,1) == 0,'(0,1)')
      ASSERT(testGraph%getCCWMostVert(10,1) == 0,'(10,1)')
      ASSERT(testGraph%getCCWMostVert(1,-1) == 0,'(1,-1)')
      ASSERT(testGraph%getCCWMostVert(1,10) == 0,'(1,10)')

      COMPONENT_TEST('Isolated vert')
      ASSERT(testGraph%getCCWMostVert(1,1) == 0,'')

      COMPONENT_TEST('End vert')
      ASSERT(testGraph%getCCWMostVert(0,2) == 3,'(0,2)')
      ASSERT(testGraph%getCCWMostVert(1,2) == 0,'(1,2)')
      ASSERT(testGraph%getCCWMostVert(2,2) == 3,'(2,2)')
      ASSERT(testGraph%getCCWMostVert(3,2) == 0,'(3,2)')

      COMPONENT_TEST('Branch vert')
      ASSERT(testGraph%getCCWMostVert(0,3) == 2,'(0,3)')
      ASSERT(testGraph%getCCWMostVert(1,3) == 0,'(1,3)')
      ASSERT(testGraph%getCCWMostVert(2,3) == 4,'(2,3)')
      ASSERT(testGraph%getCCWMostVert(3,3) == 2,'(3,3)')
      ASSERT(testGraph%getCCWMostVert(4,3) == 5,'(4,3)')
      ASSERT(testGraph%getCCWMostVert(5,3) == 2,'(5,3)')

      CALL testGraph%clear()
    ENDSUBROUTINE testCCWVert
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIsMinCyc()
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9)
      
      COMPONENT_TEST('Empty graph')
      ASSERT(.NOT.testGraph%isMinimumCycle(),'empty')
      
      COMPONENT_TEST('No edges')
      !Setup test graph 1 (no minimum cycle)
      testCoord(:,1)=(/0.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/1.25_SRK,0.5_SRK/)
      testCoord(:,5)=(/1.50_SRK,-0.5_SRK/)
      testCoord(:,6)=(/1.75_SRK,-1.0_SRK/)
      testCoord(:,7)=(/2.00_SRK,-0.75_SRK/)
      testCoord(:,8)=(/2.25_SRK,-0.25_SRK/)
      testCoord(:,9)=(/3.00_SRK,-0.10_SRK/)
      DO i=1,9
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      ASSERT(.NOT.testGraph%isMinimumCycle(),'iso')
      
      COMPONENT_TEST('Filament')
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
      ASSERT(.NOT.testGraph%isMinimumCycle(),'iso')
      
      COMPONENT_TEST('Cycle w/ filaments')
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
      CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))
      ASSERT(.NOT.testGraph%isMinimumCycle(),'iso')
      
      COMPONENT_TEST('Polygon')
      CALL testGraph%clear()
      !Setup test graph 2 (polygon)
      testCoord(:,1)=(/-1.0_SRK,0.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,-1.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/0.5_SRK,0.5_SRK/)
      testCoord(:,5)=(/0.0_SRK,1.0_SRK/)
      DO i=1,5
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,1))
      ASSERT(testGraph%isMinimumCycle(),'pentagon')
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,2))
      ASSERT(.NOT.testGraph%isMinimumCycle(),'two-cycle')
      
      CALL testGraph%clear()
    ENDSUBROUTINE testIsMinCyc
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRemVert()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r
      
      !Setup test graph
      testCoord(:,1)=(/0.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/1.25_SRK,0.5_SRK/)
      testCoord(:,5)=(/1.50_SRK,-0.5_SRK/)
      testCoord(:,6)=(/1.51_SRK,-1.0_SRK/)
      testCoord(:,7)=(/1.52_SRK,-0.75_SRK/)
      testCoord(:,8)=(/1.53_SRK,-0.50_SRK/)
      testCoord(:,9)=(/1.54_SRK,0.50_SRK/)
      c0=(/0.5_SRK,0.0_SRK/)
      r=0.5_SRK
      DO i=1,9
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,r)
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
      CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))
      
      COMPONENT_TEST('Bad Vertices')
      CALL testGraph%removeVertexI(0)
      ASSERT(testGraph%nVert() == 9,'nvert 0')
      ASSERT(testGraph%nEdge() == 7,'nedge 0')
      CALL testGraph%removeVertexI(10)
      ASSERT(testGraph%nVert() == 9,'nvert n+1')
      ASSERT(testGraph%nEdge() == 7,'nedge n+1')
      CALL testGraph%removeVertex(c0)
      ASSERT(testGraph%nVert() == 9,'nvert (1,1)')
      ASSERT(testGraph%nEdge() == 7,'nedge (1,1)')
      
      COMPONENT_TEST('Isolated vertex')
      CALL testGraph%removeVertexI(1)
      ASSERT(testGraph%nVert() == 8,'nvert')
      ASSERT(testGraph%nEdge() == 7,'nedge')
      ASSERT(testGraph%edgeMatrix(1,2) == -1,'E(1,2)')
      ASSERT(testGraph%edgeMatrix(2,3) == 1,'E(2,3)')
      ASSERT(testGraph%edgeMatrix(2,4) == 1,'E(2,4)')
      ASSERT(testGraph%edgeMatrix(4,5) == 1,'E(4,5)')
      ASSERT(testGraph%edgeMatrix(4,6) == 1,'E(4,6)')
      ASSERT(testGraph%edgeMatrix(4,7) == 1,'E(4,7)')
      ASSERT(testGraph%edgeMatrix(7,8) == 1,'E(7,8)')
      bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
      ASSERT(bool,'QE(1,2)')
      CALL symEdgeCheck()
      
      COMPONENT_TEST('V5')
      CALL testGraph%removeVertex(testCoord(:,5))
      ASSERT(testGraph%nVert() == 7,'nvert')
      ASSERT(testGraph%nEdge() == 3,'nedge')
      ASSERT(testGraph%edgeMatrix(1,2) == -1,'E(1,2)')
      ASSERT(testGraph%edgeMatrix(2,3) == 1,'E(2,3)')
      ASSERT(testGraph%edgeMatrix(6,7) == 1,'E(6,7)')
      bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
      ASSERT(bool,'QE(1,2)')
      CALL symEdgeCheck()

      COMPONENT_TEST('Endpoint')
      CALL testGraph%removeVertexI(1)
      ASSERT(testGraph%nVert() == 6,'nvert')
      ASSERT(testGraph%nEdge() == 2,'nedge')
      ASSERT(testGraph%edgeMatrix(1,2) == 1,'E(1,2)')
      ASSERT(testGraph%edgeMatrix(5,6) == 1,'E(5,6)')
      CALL symEdgeCheck()

      CALL testGraph%clear()
      CALL testGraph%removeVertexI(1)
    ENDSUBROUTINE testRemVert
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRemEdge()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r
      
      !Setup test graph
      testCoord(:,1)=(/0.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/1.25_SRK,0.5_SRK/)
      testCoord(:,5)=(/1.50_SRK,-0.5_SRK/)
      testCoord(:,6)=(/1.51_SRK,-1.0_SRK/)
      testCoord(:,7)=(/1.52_SRK,-0.75_SRK/)
      testCoord(:,8)=(/1.53_SRK,-0.50_SRK/)
      testCoord(:,9)=(/1.54_SRK,0.50_SRK/)
      c0=(/0.5_SRK,0.0_SRK/)
      r=0.5_SRK
      DO i=1,9
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,r)
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
      CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))
      
      COMPONENT_TEST('Bad Points')
      CALL testGraph%removeEdgeIJ(0,1)
      ASSERT(testGraph%nEdge() == 7,'nedge (0,1)')
      CALL testGraph%removeEdgeIJ(1,0)
      ASSERT(testGraph%nEdge() == 7,'nedge (1,0)')
      CALL testGraph%removeEdgeIJ(10,1)
      ASSERT(testGraph%nEdge() == 7,'nedge (10,1)')
      CALL testGraph%removeEdgeIJ(1,10)
      ASSERT(testGraph%nEdge() == 7,'nedge (1,10)')
      CALL testGraph%removeEdge(c0,testCoord(:,2))
      ASSERT(testGraph%nEdge() == 7,'nedge (c0,2)')
      CALL testGraph%removeEdge(testCoord(:,2),c0)
      ASSERT(testGraph%nEdge() == 7,'nedge (2,c0)')
      
      COMPONENT_TEST('Same Vertex')
      CALL testGraph%removeEdgeIJ(1,1)
      ASSERT(testGraph%nEdge() == 7,'nedge')
      ASSERT(testGraph%edgeMatrix(2,3) == -1,'E(2,3)')
      ASSERT(testGraph%edgeMatrix(3,4) == 1,'E(3,4)')
      ASSERT(testGraph%edgeMatrix(3,5) == 1,'E(3,5)')
      ASSERT(testGraph%edgeMatrix(5,6) == 1,'E(5,6)')
      ASSERT(testGraph%edgeMatrix(5,7) == 1,'E(5,7)')
      ASSERT(testGraph%edgeMatrix(5,8) == 1,'E(5,8)')
      ASSERT(testGraph%edgeMatrix(8,9) == 1,'E(8,9)')
      bool=ALL(testGraph%quadEdges(:,2,3) == (/c0(1),c0(2),r/))
      ASSERT(bool,'QE(2,3)')
      CALL symEdgeCheck()
      
      COMPONENT_TEST('Non-existent edge (IJ)')
      CALL testGraph%removeEdgeIJ(1,2)
      ASSERT(testGraph%nEdge() == 7,'nedge')
      ASSERT(testGraph%edgeMatrix(2,3) == -1,'E(2,3)')
      ASSERT(testGraph%edgeMatrix(3,4) == 1,'E(3,4)')
      ASSERT(testGraph%edgeMatrix(3,5) == 1,'E(3,5)')
      ASSERT(testGraph%edgeMatrix(5,6) == 1,'E(5,6)')
      ASSERT(testGraph%edgeMatrix(5,7) == 1,'E(5,7)')
      ASSERT(testGraph%edgeMatrix(5,8) == 1,'E(5,8)')
      ASSERT(testGraph%edgeMatrix(8,9) == 1,'E(8,9)')
      bool=ALL(testGraph%quadEdges(:,2,3) == (/c0(1),c0(2),r/))
      ASSERT(bool,'QE(2,3)')
      CALL symEdgeCheck()

      COMPONENT_TEST('Non-existent edge')
      CALL testGraph%removeEdge(testCoord(:,4),testCoord(:,5))
      ASSERT(testGraph%nEdge() == 7,'nedge')
      ASSERT(testGraph%edgeMatrix(2,3) == -1,'E(2,3)')
      ASSERT(testGraph%edgeMatrix(3,4) == 1,'E(3,4)')
      ASSERT(testGraph%edgeMatrix(3,5) == 1,'E(3,5)')
      ASSERT(testGraph%edgeMatrix(5,6) == 1,'E(5,6)')
      ASSERT(testGraph%edgeMatrix(5,7) == 1,'E(5,7)')
      ASSERT(testGraph%edgeMatrix(5,8) == 1,'E(5,8)')
      ASSERT(testGraph%edgeMatrix(8,9) == 1,'E(8,9)')
      bool=ALL(testGraph%quadEdges(:,2,3) == (/c0(1),c0(2),r/))
      ASSERT(bool,'QE(2,3)')
      CALL symEdgeCheck()
      
      COMPONENT_TEST('Valid Edge')
      CALL testGraph%removeEdge(testCoord(:,3),testCoord(:,4))
      ASSERT(testGraph%nEdge() == 6,'nedge')
      ASSERT(testGraph%edgeMatrix(2,3) == -1,'E(2,3)')
      ASSERT(testGraph%edgeMatrix(3,5) == 1,'E(3,5)')
      ASSERT(testGraph%edgeMatrix(5,6) == 1,'E(5,6)')
      ASSERT(testGraph%edgeMatrix(5,7) == 1,'E(5,7)')
      ASSERT(testGraph%edgeMatrix(5,8) == 1,'E(5,8)')
      ASSERT(testGraph%edgeMatrix(8,9) == 1,'E(8,9)')
      bool=ALL(testGraph%quadEdges(:,2,3) == (/c0(1),c0(2),r/))
      ASSERT(bool,'QE(2,3)')
      CALL symEdgeCheck()

      COMPONENT_TEST('Valid Edge (IJ)')
      CALL testGraph%removeEdgeIJ(2,3)
      ASSERT(testGraph%nEdge() == 5,'nedge')
      ASSERT(testGraph%edgeMatrix(3,5) == 1,'E(3,5)')
      ASSERT(testGraph%edgeMatrix(5,6) == 1,'E(5,6)')
      ASSERT(testGraph%edgeMatrix(5,7) == 1,'E(5,7)')
      ASSERT(testGraph%edgeMatrix(5,8) == 1,'E(5,8)')
      ASSERT(testGraph%edgeMatrix(8,9) == 1,'E(8,9)')
      CALL symEdgeCheck()

      CALL testGraph%clear()
      CALL testGraph%removeEdgeIJ(2,3)
    ENDSUBROUTINE testRemEdge
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRemFil()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r

      !Setup test graph
      testCoord(:,1)=(/0.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/1.25_SRK,0.5_SRK/)
      testCoord(:,5)=(/1.50_SRK,-0.5_SRK/)
      testCoord(:,6)=(/1.51_SRK,-1.0_SRK/)
      testCoord(:,7)=(/1.52_SRK,-0.75_SRK/)
      testCoord(:,8)=(/1.53_SRK,-0.50_SRK/)
      testCoord(:,9)=(/1.54_SRK,0.50_SRK/)
      c0=(/0.5_SRK,0.0_SRK/)
      r=0.5_SRK
      DO i=1,9
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,r)
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
      CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))
      CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
      CALL testGraph%editToVTK('testRemFil_0.vtk')

      COMPONENT_TEST('Branch point (no edge)')
      CALL testGraph%removeFilamentFromVert(5,5)
      CALL testGraph%editToVTK('testRemFil_1.vtk')
      ASSERT(testGraph%nVert() == 9,'nvert')
      ASSERT(testGraph%nEdge() == 8,'nedge')
      ASSERT(testGraph%edgeMatrix(2,3) == -1,'E(2,3)')
      ASSERT(testGraph%edgeMatrix(3,4) == 1,'E(3,4)')
      ASSERT(testGraph%edgeMatrix(3,5) == 1,'E(3,5)')
      ASSERT(testGraph%edgeMatrix(5,6) == 1,'E(5,6)')
      ASSERT(testGraph%edgeMatrix(5,7) == 1,'E(5,7)')
      ASSERT(testGraph%edgeMatrix(5,8) == 1,'E(5,8)')
      ASSERT(testGraph%edgeMatrix(6,7) == 1,'E(6,7)')
      ASSERT(testGraph%edgeMatrix(8,9) == 1,'E(8,9)')
      bool=ALL(testGraph%quadEdges(:,2,3) == (/c0(1),c0(2),r/))
      ASSERT(bool,'QE(2,3)')
      CALL symEdgeCheck()

      COMPONENT_TEST('Isolated point')
      CALL testGraph%removeFilamentFromVert(1,1)
      ASSERT(testGraph%nVert() == 8,'nvert')
      ASSERT(testGraph%nEdge() == 8,'nedge')
      ASSERT(testGraph%edgeMatrix(1,2) == -1,'E(2,3)')
      ASSERT(testGraph%edgeMatrix(2,3) == 1,'E(3,4)')
      ASSERT(testGraph%edgeMatrix(2,4) == 1,'E(3,5)')
      ASSERT(testGraph%edgeMatrix(4,5) == 1,'E(5,6)')
      ASSERT(testGraph%edgeMatrix(4,6) == 1,'E(5,7)')
      ASSERT(testGraph%edgeMatrix(4,7) == 1,'E(5,8)')
      ASSERT(testGraph%edgeMatrix(5,6) == 1,'E(6,7)')
      ASSERT(testGraph%edgeMatrix(7,8) == 1,'E(8,9)')
      bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
      ASSERT(bool,'QE(2,3)')
      CALL symEdgeCheck()

      COMPONENT_TEST('1 vert')
      CALL testGraph%removeFilamentFromVert(1,1)
      CALL testGraph%editToVTK('testRemFil_3.vtk')
      ASSERT(testGraph%nVert() == 7,'nvert')
      ASSERT(testGraph%nEdge() == 7,'nedge')
      ASSERT(testGraph%edgeMatrix(1,2) == 1,'E(3,4)')
      ASSERT(testGraph%edgeMatrix(1,3) == 1,'E(3,5)')
      ASSERT(testGraph%edgeMatrix(3,4) == 1,'E(5,6)')
      ASSERT(testGraph%edgeMatrix(3,5) == 1,'E(5,7)')
      ASSERT(testGraph%edgeMatrix(3,6) == 1,'E(5,8)')
      ASSERT(testGraph%edgeMatrix(4,5) == 1,'E(6,7)')
      ASSERT(testGraph%edgeMatrix(6,7) == 1,'E(8,9)')
      CALL symEdgeCheck()

      COMPONENT_TEST('3 verts')
      CALL testGraph%removeFilamentFromVert(7,7)
      CALL testGraph%editToVTK('testRemFil_4.vtk')
      ASSERT(testGraph%nVert() == 5,'nvert')
      ASSERT(testGraph%nEdge() == 5,'nedge')
      ASSERT(testGraph%edgeMatrix(1,2) == 1,'E(3,4)')
      ASSERT(testGraph%edgeMatrix(1,3) == 1,'E(3,5)')
      ASSERT(testGraph%edgeMatrix(3,4) == 1,'E(5,6)')
      ASSERT(testGraph%edgeMatrix(3,5) == 1,'E(5,7)')
      ASSERT(testGraph%edgeMatrix(4,5) == 1,'E(6,7)')
      CALL symEdgeCheck()

      !Not sure if this is a bug or not...
      COMPONENT_TEST('Branch point (w/ edge)')
      CALL testGraph%removeFilamentFromVert(1,1)
      CALL testGraph%editToVTK('testRemFil_5.vtk')
      ASSERT(testGraph%nVert() == 5,'nvert')
      ASSERT(testGraph%nEdge() == 5,'nedge')
      ASSERT(testGraph%edgeMatrix(1,2) == 1,'E(3,4)')
      ASSERT(testGraph%edgeMatrix(1,3) == 1,'E(3,5)')
      ASSERT(testGraph%edgeMatrix(3,4) == 1,'E(5,6)')
      ASSERT(testGraph%edgeMatrix(3,5) == 1,'E(5,7)')
      ASSERT(testGraph%edgeMatrix(4,5) == 1,'E(6,7)')
      CALL symEdgeCheck()
      
      CALL testGraph%clear()
    ENDSUBROUTINE testRemFil
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetMCB()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(GraphType),ALLOCATABLE :: cycles(:)
      
      COMPONENT_TEST('1 cycle + filaments')
      !Setup test graph
      testCoord(:,1)=(/0.0_SRK,-1.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/1.25_SRK,0.5_SRK/)
      testCoord(:,5)=(/1.50_SRK,-0.5_SRK/)
      testCoord(:,6)=(/1.75_SRK,-1.0_SRK/)
      testCoord(:,7)=(/2.00_SRK,-0.75_SRK/)
      testCoord(:,8)=(/2.25_SRK,-0.25_SRK/)
      testCoord(:,9)=(/3.00_SRK,-0.10_SRK/)
      c0=(/0.5_SRK,0.0_SRK/)
      r=0.5_SRK
      DO i=1,9
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,r)
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
      CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))

      CALL testGraph%getMCB(cycles)
      ASSERTFAIL(ALLOCATED(cycles),'allocated')
      ASSERT(SIZE(cycles) == 1,'size')
      ASSERT(cycles(1)%nVert() == 3,'cycle vert')
      bool=ALL(cycles(1)%vertices(:,1) == testCoord(:,5))
      ASSERT(bool,'vert 1')
      bool=ALL(cycles(1)%vertices(:,2) == testCoord(:,6))
      ASSERT(bool,'vert 2')
      bool=ALL(cycles(1)%vertices(:,3) == testCoord(:,7))
      ASSERT(bool,'vert 3')
      ASSERT(cycles(1)%edgeMatrix(1,2) == 1,'edge(1,2)')
      ASSERT(cycles(1)%edgeMatrix(1,3) == 1,'edge(1,3)')
      ASSERT(cycles(1)%edgeMatrix(2,3) == 1,'edge(2,3)')
      CALL testGraph%clear()

      COMPONENT_TEST('1 cycle')
      testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
      testCoord(:,5)=(/2.0_SRK,0.0_SRK/)
      testCoord(:,6)=(/2.0_SRK,1.0_SRK/)
      testCoord(:,7)=(/2.0_SRK,2.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,2))
      CALL testGraph%getMCB(cycles)
      ASSERTFAIL(ALLOCATED(cycles),'allocated')
      ASSERT(SIZE(cycles) == 1,'size')
      ASSERT(cycles(1)%nVert() == 4,'cycle vert')
      bool=ALL(cycles(1)%vertices(:,1) == testCoord(:,1))
      ASSERT(bool,'vert 1')
      bool=ALL(cycles(1)%vertices(:,2) == testCoord(:,2))
      ASSERT(bool,'vert 2')
      bool=ALL(cycles(1)%vertices(:,3) == testCoord(:,3))
      ASSERT(bool,'vert 3')
      bool=ALL(cycles(1)%vertices(:,4) == testCoord(:,4))
      ASSERT(bool,'vert 4')
      ASSERT(cycles(1)%edgeMatrix(1,2) == 1,'edge(1,2)')
      ASSERT(cycles(1)%edgeMatrix(1,3) == 1,'edge(1,3)')
      ASSERT(cycles(1)%edgeMatrix(3,4) == 1,'edge(3,4)')
      ASSERT(cycles(1)%edgeMatrix(2,4) == 1,'edge(2,4)')

      COMPONENT_TEST('2 cycles')
      CALL testGraph%insertVertex(testCoord(:,5))
      CALL testGraph%insertVertex(testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))      
      CALL testGraph%getMCB(cycles)
      ASSERTFAIL(ALLOCATED(cycles),'allocated')
      ASSERT(SIZE(cycles) == 2,'size')
      ASSERT(cycles(1)%nVert() == 4,'1 cycle vert')
      bool=ALL(cycles(1)%vertices(:,1) == testCoord(:,1))
      ASSERT(bool,'1 vert 1')
      bool=ALL(cycles(1)%vertices(:,2) == testCoord(:,2))
      ASSERT(bool,'1 vert 2')
      bool=ALL(cycles(1)%vertices(:,3) == testCoord(:,3))
      ASSERT(bool,'1 vert 3')
      bool=ALL(cycles(1)%vertices(:,4) == testCoord(:,4))
      ASSERT(bool,'1 vert 4')
      ASSERT(cycles(1)%edgeMatrix(1,2) == 1,'1 edge(1,2)')
      ASSERT(cycles(1)%edgeMatrix(1,3) == 1,'1 edge(1,3)')
      ASSERT(cycles(1)%edgeMatrix(3,4) == 1,'1 edge(3,4)')
      ASSERT(cycles(1)%edgeMatrix(2,4) == 1,'1 edge(2,4)')
      ASSERT(cycles(2)%nVert() == 4,'2 cycle vert')
      bool=ALL(cycles(2)%vertices(:,1) == testCoord(:,3))
      ASSERT(bool,'2 vert 1')
      bool=ALL(cycles(2)%vertices(:,2) == testCoord(:,4))
      ASSERT(bool,'2 vert 2')
      bool=ALL(cycles(2)%vertices(:,3) == testCoord(:,5))
      ASSERT(bool,'2 vert 3')
      bool=ALL(cycles(2)%vertices(:,4) == testCoord(:,6))
      ASSERT(bool,'2 vert 4')
      ASSERT(cycles(2)%edgeMatrix(1,2) == 1,'2 edge(1,2)')
      ASSERT(cycles(2)%edgeMatrix(1,3) == 1,'2 edge(1,3)')
      ASSERT(cycles(2)%edgeMatrix(3,4) == 1,'2 edge(3,4)')
      ASSERT(cycles(2)%edgeMatrix(2,4) == 1,'2 edge(2,4)')
      
      COMPONENT_TEST('2 cycles + filament')
      CALL testGraph%insertVertex(testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
      CALL testGraph%getMCB(cycles)
      ASSERTFAIL(ALLOCATED(cycles),'allocated')
      ASSERT(SIZE(cycles) == 2,'size')
      ASSERT(cycles(1)%nVert() == 4,'1 cycle vert')
      bool=ALL(cycles(1)%vertices(:,1) == testCoord(:,1))
      ASSERT(bool,'1 vert 1')
      bool=ALL(cycles(1)%vertices(:,2) == testCoord(:,2))
      ASSERT(bool,'1 vert 2')
      bool=ALL(cycles(1)%vertices(:,3) == testCoord(:,3))
      ASSERT(bool,'1 vert 3')
      bool=ALL(cycles(1)%vertices(:,4) == testCoord(:,4))
      ASSERT(bool,'1 vert 4')
      ASSERT(cycles(1)%edgeMatrix(1,2) == 1,'1 edge(1,2)')
      ASSERT(cycles(1)%edgeMatrix(1,3) == 1,'1 edge(1,3)')
      ASSERT(cycles(1)%edgeMatrix(3,4) == 1,'1 edge(3,4)')
      ASSERT(cycles(1)%edgeMatrix(2,4) == 1,'1 edge(2,4)')
      ASSERT(cycles(1)%nVert() == 4,'2 cycle vert')
      bool=ALL(cycles(2)%vertices(:,1) == testCoord(:,3))
      ASSERT(bool,'2 vert 1')
      bool=ALL(cycles(2)%vertices(:,2) == testCoord(:,4))
      ASSERT(bool,'2 vert 2')
      bool=ALL(cycles(2)%vertices(:,3) == testCoord(:,5))
      ASSERT(bool,'2 vert 3')
      bool=ALL(cycles(2)%vertices(:,4) == testCoord(:,6))
      ASSERT(bool,'2 vert 4')
      ASSERT(cycles(2)%edgeMatrix(1,2) == 1,'2 edge(1,2)')
      ASSERT(cycles(2)%edgeMatrix(1,3) == 1,'2 edge(1,3)')
      ASSERT(cycles(2)%edgeMatrix(3,4) == 1,'2 edge(3,4)')
      ASSERT(cycles(2)%edgeMatrix(2,4) == 1,'2 edge(2,4)')
      
      CALL testGraph%clear()
    ENDSUBROUTINE testGetMCB
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIsEqual()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r
      testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
      testCoord(:,5)=(/2.0_SRK,0.0_SRK/)
      testCoord(:,6)=(/2.0_SRK,1.0_SRK/)
      testCoord(:,7)=(/2.0_SRK,2.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,2))
      CALL testGraph%insertVertex(testCoord(:,5))
      CALL testGraph%insertVertex(testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,6))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
      CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))      
      CALL testGraph%insertVertex(testCoord(:,7))
      CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
      !Graph2
      DO i=1,4
        CALL testGraph2%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph2%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph2%defineEdge(testCoord(:,1),testCoord(:,3))
      CALL testGraph2%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph2%defineEdge(testCoord(:,4),testCoord(:,2))
      CALL testGraph2%insertVertex(testCoord(:,5))
      CALL testGraph2%insertVertex(testCoord(:,6))
      CALL testGraph2%defineEdge(testCoord(:,4),testCoord(:,6))
      CALL testGraph2%defineEdge(testCoord(:,3),testCoord(:,5))
      CALL testGraph2%defineEdge(testCoord(:,5),testCoord(:,6))      
      CALL testGraph2%insertVertex(testCoord(:,7))
      CALL testGraph2%defineEdge(testCoord(:,6),testCoord(:,7))
      ASSERT(testGraph == testGraph2,'Equal Graphs')
      testGraph2%vertices(1,1)=-1.0_SRK
      ASSERT(.NOT.(testGraph == testGraph2),'Non-Equal Graphs')
      
      CALL testGraph%clear()
      CALL testGraph2%clear()
    ENDSUBROUTINE testIsEqual
!
!-------------------------------------------------------------------------------
    SUBROUTINE testAddition()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i
      REAL(SRK) :: testCoord(2,9),c0(2),r
      TYPE(GraphType) :: testGraph3,testGraph4
      
      !Test graph1
      testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
      testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
      testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
      testCoord(:,5)=(/2.0_SRK,0.0_SRK/)
      testCoord(:,6)=(/2.0_SRK,1.0_SRK/)
      testCoord(:,7)=(/2.0_SRK,2.0_SRK/)
      DO i=1,4
        CALL testGraph%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,2))
      !test graph2
      CALL testGraph2%insertVertex(testCoord(:,1))
      CALL testGraph2%insertVertex(testCoord(:,5))
      CALL testGraph2%insertVertex(testCoord(:,6))
      CALL testGraph2%insertVertex(testCoord(:,7))
      CALL testGraph2%defineEdge(testCoord(:,7),testCoord(:,8))
      CALL testGraph2%defineEdge(testCoord(:,5),testCoord(:,6))
      CALL testGraph2%defineEdge(testCoord(:,6),testCoord(:,7))
      !Test graph4
      DO i=1,4
        CALL testGraph4%insertVertex(testCoord(:,i))
      ENDDO
      CALL testGraph4%insertVertex(testCoord(:,5))
      CALL testGraph4%insertVertex(testCoord(:,6))
      CALL testGraph4%insertVertex(testCoord(:,7))
      CALL testGraph4%defineEdge(testCoord(:,1),testCoord(:,2))
      CALL testGraph4%defineEdge(testCoord(:,1),testCoord(:,3))
      CALL testGraph4%defineEdge(testCoord(:,3),testCoord(:,4))
      CALL testGraph4%defineEdge(testCoord(:,4),testCoord(:,2))
      CALL testGraph4%defineEdge(testCoord(:,7),testCoord(:,1))
      CALL testGraph4%defineEdge(testCoord(:,5),testCoord(:,6))      
      CALL testGraph4%defineEdge(testCoord(:,6),testCoord(:,7))
      
      testGraph3=testGraph+testGraph2
      ASSERT(testGraph3 == testGraph4,'addition')
    ENDSUBROUTINE testAddition
!
!-------------------------------------------------------------------------------
    SUBROUTINE symEdgeCheck()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,j,n
      n=testGraph%nVert()
      DO i=1,n
        ASSERT(testGraph%edgeMatrix(i,i) == 0,'E(i,i)')
        bool=ALL(testGraph%quadEdges(:,i,i) == 0.0_SRK)
        ASSERT(bool,'QE(i,i)')
        DO j=i+1,n
          ASSERT(testGraph%edgeMatrix(i,j) == testGraph%edgeMatrix(j,i),'E symmetry')
          bool=ALL(testGraph%quadEdges(:,i,j) == testGraph%quadEdges(:,j,i))
          ASSERT(bool,'QE symmetry')
        ENDDO
      ENDDO
    ENDSUBROUTINE symEdgeCheck
!
ENDPROGRAM testGeom_Graph
