!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testGeom_Graph
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE Allocs
USE Geom_Graph

IMPLICIT NONE

TYPE(GraphType) :: testGraph
TYPE(DAGraphType) :: testDAGraph

CREATE_TEST('TEST GEOM_GRAPH')

REGISTER_SUBTEST('Uninit',testUninit)
REGISTER_SUBTEST('%nVert',testNVert)
REGISTER_SUBTEST('%nEdge',testNEdge)
REGISTER_SUBTEST('%clear',testClear)
REGISTER_SUBTEST('%insertVertex',testInsertVertex)
REGISTER_SUBTEST('%getVertIndex',testGetVertIndex)
REGISTER_SUBTEST('%defineEdge',testDefineEdge)
REGISTER_SUBTEST('%defineQuadEdge',testDefineQuadEdge)
REGISTER_SUBTEST('%getMidPointOnEdge',testGetMidPoint)
REGISTER_SUBTEST('ASSIGNMENT(=)',testAssign)
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
REGISTER_SUBTEST('%combineGraph',testCombine)
REGISTER_SUBTEST('%TriangulateVerts',testTriangulate)
REGISTER_SUBTEST('OPERATOR(==)',testIsEqual)
!REGISTER_SUBTEST('OPERATOR(+)',testAddition)
FINALIZE_TEST()

CREATE_TEST('TEST Directed Acyclic Graph')
REGISTER_SUBTEST('Uninit',testDAGUninit)
REGISTER_SUBTEST('%clear',testDAGClear)
REGISTER_SUBTEST('%init',testDAGinit)
REGISTER_SUBTEST('%defineEdge',testDAGDefineEdge)
REGISTER_SUBTEST('%removeEdge',testDAGRemoveEdge)
REGISTER_SUBTEST('%insertNode',testDAGinsertNode)
REGISTER_SUBTEST('%removeNode',testDAGremoveNode)
REGISTER_SUBTEST('%isStartNode',testDAGisStartNode)
REGISTER_SUBTEST('%testDAGgetNextStartNode',testDAGgetNextStartNode)
REGISTER_SUBTEST('%KATS',testDAGKATS)
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
  ASSERT_EQ(testGraph%nVert(),0,'uninit')
  CALL dmallocA(testGraph%vertices,1,8)
  ASSERT_EQ(testGraph%nVert(),8,'8')
  CALL testGraph%clear()
  ASSERT_EQ(testGraph%nVert(),0,'post clear')
ENDSUBROUTINE testNVert
!
!-------------------------------------------------------------------------------
SUBROUTINE testNEdge()
  ASSERT_EQ(testGraph%nEdge(),0,'uninit')
  CALL dmallocA(testGraph%edgeMatrix,3,3)
  ASSERT_EQ(testGraph%nEdge(),0,'0')
  testGraph%EdgeMatrix(1,2)=1
  testGraph%EdgeMatrix(2,1)=1
  ASSERT_EQ(testGraph%nEdge(),1,'0')
  testGraph%EdgeMatrix(3,2)=-1
  testGraph%EdgeMatrix(2,3)=-1
  ASSERT_EQ(testGraph%nEdge(),2,'0')
  CALL testGraph%clear()
  ASSERT_EQ(testGraph%nEdge(),0,'post clear')
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
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1)')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),1,'SIZE(%vertices,DIM=2)')
  bool=ALL(testGraph%vertices == testCoord(:,2:2))
  ASSERT(bool,'%vertices (values)')
  bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
  ASSERT(bool,'%edgeMatrix square')
  bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
  ASSERT(bool,'%edgeMatrix size vertices')

  COMPONENT_TEST('Last Vertex')
  CALL testGraph%insertVertex(testCoord(:,6))
  ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices)')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1)')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),2,'SIZE(%vertices,DIM=2)')
  bool=ALL(testGraph%vertices == testCoord(:,(/2,6/)))
  ASSERT(bool,'%vertices (values)')
  bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
  ASSERT(bool,'%edgeMatrix square')
  bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
  ASSERT(bool,'%edgeMatrix size vertices')

  COMPONENT_TEST('Beginning Vertex')
  CALL testGraph%insertVertex(testCoord(:,1))
  ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices)')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1)')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),3,'SIZE(%vertices,DIM=2)')
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
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1)')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),4,'SIZE(%vertices,DIM=2)')
  bool=ALL(testGraph%vertices == testCoord(:,(/1,2,4,6/)))
  ASSERT(bool,'%vertices (values)')
  bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
  ASSERT(bool,'%edgeMatrix square')
  bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
  ASSERT(bool,'%edgeMatrix size vertices')

  COMPONENT_TEST('Equal X before')
  CALL testGraph%insertVertex(testCoord(:,3))
  ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices)')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1)')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),5,'SIZE(%vertices,DIM=2)')
  bool=ALL(testGraph%vertices == testCoord(:,(/1,2,3,4,6/)))
  ASSERT(bool,'%vertices (values) 5')
  bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
  ASSERT(bool,'%edgeMatrix square')
  bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
  ASSERT(bool,'%edgeMatrix size vertices')

  COMPONENT_TEST('Equal X after')
  CALL testGraph%insertVertex(testCoord(:,5))
  ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices)')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1)')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),6,'SIZE(%vertices,DIM=2)')
  bool=ALL(testGraph%vertices == testCoord)
  ASSERT(bool,'%vertices (values)')
  bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
  ASSERT(bool,'%edgeMatrix square')
  bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
  ASSERT(bool,'%edgeMatrix size vertices')

  COMPONENT_TEST('Duplicate')
  CALL testGraph%insertVertex(testCoord(:,3))
  ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices)')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1)')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),6,'SIZE(%vertices,DIM=2)')
  bool=ALL(testGraph%vertices == testCoord)
  ASSERT(bool,'%vertices (values)')
  bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%edgeMatrix,DIM=2)
  ASSERT(bool,'%edgeMatrix square')
  bool=SIZE(testGraph%edgeMatrix,DIM=1) == SIZE(testGraph%vertices,DIM=2)
  ASSERT(bool,'%edgeMatrix size vertices')

  CALL testGraph%clear()

  COMPONENT_TEST('Vertical Lines')
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,0.5_SRK/)
  testCoord(:,3)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,5)=(/1.0_SRK,0.5_SRK/)
  testCoord(:,6)=(/1.0_SRK,1.0_SRK/)
  CALL testGraph%insertVertex(testCoord(:,1))
  ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices) 1')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1) 1')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),1,'SIZE(%vertices,DIM=2) 1')
  bool=ALL(testGraph%vertices(:,1) == testCoord(:,1))
  ASSERT(bool,'%vertices (values) 1')
  CALL testGraph%insertVertex(testCoord(:,3))
  ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices) 2')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1) 2')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),2,'SIZE(%vertices,DIM=2) 2')
  bool=ALL(testGraph%vertices(:,1) == testCoord(:,1)) .AND. &
      ALL(testGraph%vertices(:,2) == testCoord(:,3))
  ASSERT(bool,'%vertices (values) 2')
  CALL testGraph%insertVertex(testCoord(:,4))
  ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices) 3')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1) 3')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),3,'SIZE(%vertices,DIM=2) 3')
  bool=ALL(testGraph%vertices(:,1) == testCoord(:,1)) .AND. &
      ALL(testGraph%vertices(:,2) == testCoord(:,3)) .AND. &
      ALL(testGraph%vertices(:,3) == testCoord(:,4))
  ASSERT(bool,'%vertices (values) 3')
  CALL testGraph%insertVertex(testCoord(:,6))
  ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices) 4')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1) 4')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),4,'SIZE(%vertices,DIM=2) 4')
  bool=ALL(testGraph%vertices(:,1) == testCoord(:,1)) .AND. &
      ALL(testGraph%vertices(:,2) == testCoord(:,3)) .AND. &
      ALL(testGraph%vertices(:,3) == testCoord(:,4)) .AND. &
      ALL(testGraph%vertices(:,4) == testCoord(:,6))
  ASSERT(bool,'%vertices (values) 4')
  CALL testGraph%insertVertex(testCoord(:,2))
  ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices) 5')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1) 5')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),5,'SIZE(%vertices,DIM=2) 5')
  bool=ALL(testGraph%vertices(:,1) == testCoord(:,1)) .AND. &
      ALL(testGraph%vertices(:,2) == testCoord(:,2)) .AND. &
      ALL(testGraph%vertices(:,3) == testCoord(:,3)) .AND. &
      ALL(testGraph%vertices(:,4) == testCoord(:,4)) .AND. &
      ALL(testGraph%vertices(:,5) == testCoord(:,6))
  ASSERT(bool,'%vertices (values) 5')
  CALL testGraph%insertVertex(testCoord(:,5))
  ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices) 6')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1) 6')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),6,'SIZE(%vertices,DIM=2) 6')
  bool=ALL(testGraph%vertices == testCoord)
  ASSERT(bool,'%vertices (values) 6')
  CALL testGraph%insertVertex(testCoord(:,5))
  ASSERTFAIL(ALLOCATED(testGraph%vertices),'ALLOCATED(%vertices) 6')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=1),2,'SIZE(%vertices,DIM=1) 6')
  ASSERT_EQ(SIZE(testGraph%vertices,DIM=2),6,'SIZE(%vertices,DIM=2) 6')
  bool=ALL(testGraph%vertices == testCoord)
  ASSERT(bool,'%vertices (values) 6')
  CALL testGraph%clear()
ENDSUBROUTINE testInsertVertex
!
!-------------------------------------------------------------------------------
SUBROUTINE testGetVertIndex()
  REAL(SRK) :: testCoord(2,7)

  testCoord(:,1)=(/1.0_SRK,2.0_SRK/)
  testCoord(:,2)=(/2.0_SRK,0.0_SRK/)
  testCoord(:,3)=(/3.0_SRK,2.5_SRK/)
  testCoord(:,4)=(/3.0_SRK,3.0_SRK/)
  testCoord(:,5)=(/3.0_SRK,4.0_SRK/)
  testCoord(:,6)=(/4.0_SRK,1.0_SRK/)

  COMPONENT_TEST('Empty graph')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)),-1,'1')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,2)),-1,'2')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,3)),-1,'3')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,4)),-1,'4')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,5)),-1,'5')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,6)),-1,'6')

  COMPONENT_TEST('First vertex')
  CALL testGraph%insertVertex(testCoord(:,2))
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)),-1,'1')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,2)), 1,'2')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,3)),-1,'3')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,4)),-1,'4')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,5)),-1,'5')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,6)),-1,'6')

  COMPONENT_TEST('Last vertex')
  CALL testGraph%insertVertex(testCoord(:,6))
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)),-1,'1')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,2)), 1,'2')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,3)),-1,'3')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,4)),-1,'4')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,5)),-1,'5')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,6)), 2,'6')

  COMPONENT_TEST('Beginning vertex')
  CALL testGraph%insertVertex(testCoord(:,1))
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)), 1,'1')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,2)), 2,'2')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,3)),-1,'3')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,4)),-1,'4')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,5)),-1,'5')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,6)), 3,'6')

  COMPONENT_TEST('Insertion vertex')
  CALL testGraph%insertVertex(testCoord(:,4))
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)), 1,'1')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,2)), 2,'2')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,3)),-1,'3')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,4)), 3,'4')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,5)),-1,'5')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,6)), 4,'6')

  COMPONENT_TEST('Equal X before')
  CALL testGraph%insertVertex(testCoord(:,3))
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)), 1,'1')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,2)), 2,'2')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,3)), 3,'3')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,4)), 4,'4')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,5)),-1,'5')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,6)), 5,'6')

  COMPONENT_TEST('Equal X after')
  CALL testGraph%insertVertex(testCoord(:,5))
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)), 1,'1')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,2)), 2,'2')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,3)), 3,'3')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,4)), 4,'4')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,5)), 5,'5')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,6)), 6,'6')

  CALL testGraph%clear()

  COMPONENT_TEST('Vertical Lines')
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,0.5_SRK/)
  testCoord(:,3)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,5)=(/1.0_SRK,0.5_SRK/)
  testCoord(:,6)=(/1.0_SRK,1.0_SRK/)
  testCoord(:,7)=(/0.5_SRK,0.5_SRK/)
  CALL testGraph%insertVertex(testCoord(:,1))
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)), 1,'1 (1)')
  CALL testGraph%insertVertex(testCoord(:,3))
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)), 1,'1 (2)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,3)), 2,'2 (2)')
  CALL testGraph%insertVertex(testCoord(:,4))
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)), 1,'1 (3)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,3)), 2,'2 (3)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,4)), 3,'3 (3)')
  CALL testGraph%insertVertex(testCoord(:,6))
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)), 1,'1 (4)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,3)), 2,'2 (4)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,4)), 3,'3 (4)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,6)), 4,'4 (4)')
  CALL testGraph%insertVertex(testCoord(:,2))
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)), 1,'1 (5)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,2)), 2,'2 (5)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,3)), 3,'3 (5)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,4)), 4,'4 (5)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,6)), 5,'5 (5)')
  CALL testGraph%insertVertex(testCoord(:,5))
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)), 1,'1 (6)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,2)), 2,'2 (6)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,3)), 3,'3 (6)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,4)), 4,'4 (6)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,5)), 5,'5 (6)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,6)), 6,'6 (6)')
  CALL testGraph%insertVertex(testCoord(:,7))
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,1)), 1,'1 (7)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,2)), 2,'2 (7)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,3)), 3,'3 (7)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,7)), 4,'4 (7)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,4)), 5,'5 (7)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,5)), 6,'6 (7)')
  ASSERT_EQ(testGraph%getVertIndex(testCoord(:,6)), 7,'7 (7)')
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
  ASSERT_EQ(testGraph%edgeMatrix(1,2),1,'edge 1')
  ASSERT_EQ(testGraph%nEdge(),1,'nEdge')
  CALL symEdgeCheck()

  !Test Resize
  COMPONENT_TEST('Post Insertion')
  CALL testGraph%insertVertex(testCoord(:,2))
  ASSERT_EQ(testGraph%edgeMatrix(1,3),1,'edge 1')
  ASSERT_EQ(testGraph%nEdge(),1,'nEdge')
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
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2),c0,r)
  ASSERT(.NOT.ALLOCATED(testGraph%edgeMatrix),'no eMatrix')
  ASSERT(.NOT.ALLOCATED(testGraph%quadEdges),'no qEdges')

  COMPONENT_TEST('Same Point')
  CALL testGraph%insertVertex(testCoord(:,1))
  CALL testGraph%insertVertex(testCoord(:,2))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,1),c0,r)
  ASSERT(ALL(testGraph%edgeMatrix == 0),'no eMatrix')
  ASSERT(ALL(testGraph%quadEdges == 0.0_SRK),'no qEdges')

  COMPONENT_TEST('Missing Points')
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,2),c0,r)
  ASSERT(ALL(testGraph%edgeMatrix == 0),'no eMatrix 1')
  ASSERT(ALL(testGraph%quadEdges == 0.0_SRK),'no qEdges 1')
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c0,r)
  ASSERT(ALL(testGraph%edgeMatrix == 0),'no eMatrix 2')
  ASSERT(ALL(testGraph%quadEdges == 0.0_SRK),'no qEdges 2')

  COMPONENT_TEST('Valid Edge')
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,1),c0,r)
  ASSERT_EQ(testGraph%nEdge(),1,'nEdge')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),-1,'edge 1')
  bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
  ASSERT(bool,'quadEdge 1')
  CALL symEdgeCheck()

  COMPONENT_TEST('Post vertex append')
  CALL testGraph%insertVertex(testCoord(:,3))
  ASSERT_EQ(testGraph%nEdge(),1,'nEdge')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),-1,'edge 1')
  bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
  ASSERT(bool,'quadEdge 1')
  CALL symEdgeCheck()
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c0,r)
  ASSERT_EQ(testGraph%nEdge(),2,'nEdge')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),-1,'edge 1')
  bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
  ASSERT(bool,'quadEdge 1')
  ASSERT_EQ(testGraph%edgeMatrix(2,3),-1,'edge 2')
  bool=ALL(testGraph%quadEdges(:,2,3) == (/c0(1),c0(2),r/))
  ASSERT(bool,'quadEdge 2')
  CALL symEdgeCheck()

  COMPONENT_TEST('Post vertex insertion')
  CALL testGraph%insertVertex(testCoord(:,4))
  ASSERT_EQ(testGraph%nEdge(),2,'nEdge')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),-1,'edge 1')
  bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
  ASSERT(bool,'quadEdge 1')
  ASSERT_EQ(testGraph%edgeMatrix(2,4),-1,'edge 2')
  bool=ALL(testGraph%quadEdges(:,2,4) == (/c0(1),c0(2),r/))
  ASSERT(bool,'quadEdge 2')
  CALL symEdgeCheck()

  COMPONENT_TEST('Mixed edges')
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  ASSERT_EQ(testGraph%nEdge(),4,'nEdge')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),-1,'edge 1')
  bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
  ASSERT(bool,'quadEdge 1')
  ASSERT_EQ(testGraph%edgeMatrix(2,4),-1,'edge 2')
  bool=ALL(testGraph%quadEdges(:,2,4) == (/c0(1),c0(2),r/))
  ASSERT(bool,'quadEdge 2')
  ASSERT_EQ(testGraph%edgeMatrix(1,3),1,'edge 3')
  bool=ALL(testGraph%quadEdges(:,1,3) == 0.0_SRK)
  ASSERT(bool,'quadEdge 3')
  ASSERT_EQ(testGraph%edgeMatrix(3,4),1,'edge 4')
  bool=ALL(testGraph%quadEdges(:,3,4) == 0.0_SRK)
  ASSERT(bool,'quadEdge 4')
  CALL symEdgeCheck()

  COMPONENT_TEST('Mixed edge post insertion')
  CALL testGraph%insertVertex(testCoord(:,5))
  ASSERT_EQ(testGraph%nEdge(),4,'nEdge')
  ASSERT_EQ(testGraph%edgeMatrix(1,3),-1,'edge 1')
  bool=ALL(testGraph%quadEdges(:,1,3) == (/c0(1),c0(2),r/))
  ASSERT(bool,'quadEdge 1')
  ASSERT_EQ(testGraph%edgeMatrix(3,5),-1,'edge 2')
  bool=ALL(testGraph%quadEdges(:,3,5) == (/c0(1),c0(2),r/))
  ASSERT(bool,'quadEdge 2')
  ASSERT_EQ(testGraph%edgeMatrix(1,4),1,'edge 3')
  bool=ALL(testGraph%quadEdges(:,1,4) == 0.0_SRK)
  ASSERT(bool,'quadEdge 3')
  ASSERT_EQ(testGraph%edgeMatrix(4,5),1,'edge 4')
  bool=ALL(testGraph%quadEdges(:,4,5) == 0.0_SRK)
  ASSERT(bool,'quadEdge 4')
  CALL symEdgeCheck()

  COMPONENT_TEST('Vertices not on circle')
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,5),c0,r)
  ASSERT_EQ(testGraph%nEdge(),4,'nEdge')
  ASSERT_EQ(testGraph%edgeMatrix(1,3),-1,'edge 1')
  bool=ALL(testGraph%quadEdges(:,1,3) == (/c0(1),c0(2),r/))
  ASSERT(bool,'quadEdge 1')
  ASSERT_EQ(testGraph%edgeMatrix(3,5),-1,'edge 2')
  bool=ALL(testGraph%quadEdges(:,3,5) == (/c0(1),c0(2),r/))
  ASSERT(bool,'quadEdge 2')
  ASSERT_EQ(testGraph%edgeMatrix(1,4),1,'edge 3')
  bool=ALL(testGraph%quadEdges(:,1,4) == 0.0_SRK)
  ASSERT(bool,'quadEdge 3')
  ASSERT_EQ(testGraph%edgeMatrix(4,5),1,'edge 4')
  bool=ALL(testGraph%quadEdges(:,4,5) == 0.0_SRK)
  ASSERT(bool,'quadEdge 4')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),0,'edge 5')
  bool=ALL(testGraph%quadEdges(:,1,2) == 0.0_SRK)
  ASSERT(bool,'quadEdge 5')
  CALL symEdgeCheck()

  COMPONENT_TEST('Circle inflection')
  CALL testGraph%clear()
  CALL testGraph%insertVertex(testCoord(:,1))
  CALL testGraph%insertVertex(testCoord(:,2))
  CALL testGraph%insertVertex(testCoord(:,3))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2),c0,-r)
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3),c0,-r)
  ASSERT_EQ(testGraph%nEdge(),2,'nEdge')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),-1,'edge 1')
  bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
  ASSERT(bool,'quadEdge 1')
  ASSERT_EQ(testGraph%edgeMatrix(1,3),-1,'edge 2')
  bool=ALL(testGraph%quadEdges(:,1,3) == (/c0(1),c0(2),-r/))
  ASSERT(bool,'quadEdge 2')
  CALL symEdgeCheck()

  CALL testGraph%clear()
ENDSUBROUTINE testDefineQuadEdge
!
!-------------------------------------------------------------------------------
SUBROUTINE testGetMidPoint()
  INTEGER(SIK) :: i
  REAL(SRK) :: testCoord(2,4),c(2),r,m(2)

  COMPONENT_TEST('Straight Edge')
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  DO i=1,4
    CALL testGraph%insertVertex(testCoord(:,i))
  ENDDO
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))

  CALL testGraph%getMidPointOnEdge(1,2,m)
  ASSERT(ALL(m .APPROXEQ. (/0.0_SRK,0.5_SRK/)),'(1,2)')
  CALL testGraph%getMidPointOnEdge(1,3,m)
  ASSERT(ALL(m .APPROXEQ. (/0.5_SRK,0.0_SRK/)),'(1,3)')
  CALL testGraph%getMidPointOnEdge(1,4,m)
  ASSERT(ALL(m .APPROXEQ. -HUGE(r)),'(1,4)')
  CALL testGraph%getMidPointOnEdge(2,3,m)
  ASSERT(ALL(m .APPROXEQ. -HUGE(r)),'(2,3)')
  CALL testGraph%getMidPointOnEdge(2,4,m)
  ASSERT(ALL(m .APPROXEQ. (/0.5_SRK,1.0_SRK/)),'(2,4)')
  CALL testGraph%getMidPointOnEdge(3,4,m)
  ASSERT(ALL(m .APPROXEQ. (/1.0_SRK,0.5_SRK/)),'(3,4)')
  CALL testGraph%clear()

  COMPONENT_TEST('Quarter-Circle')
  testCoord(:,1)=(/-1.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,-1.0_SRK/)
  testCoord(:,3)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,0.0_SRK/)
  DO i=1,4
    CALL testGraph%insertVertex(testCoord(:,i))
  ENDDO
  c=0.0_SRK
  r=1.0_SRK
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2),c,r)
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3),c,r)
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,4),c,r)
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4),c,r)
  CALL testGraph%getMidPointOnEdge(1,2,m)
  ASSERT(ALL(m .APPROXEQ. (/-SQRT(0.5_SRK),-SQRT(0.5_SRK)/)),'(1,2)')
  CALL testGraph%getMidPointOnEdge(1,3,m)
  ASSERT(ALL(m .APPROXEQ. (/-SQRT(0.5_SRK),SQRT(0.5_SRK)/)),'(1,3)')
  CALL testGraph%getMidPointOnEdge(1,4,m)
  ASSERT(ALL(m .APPROXEQ. -HUGE(r)),'(1,4)')
  CALL testGraph%getMidPointOnEdge(2,3,m)
  ASSERT(ALL(m .APPROXEQ. -HUGE(r)),'(2,3)')
  CALL testGraph%getMidPointOnEdge(2,4,m)
  ASSERT(ALL(m .APPROXEQ. (/SQRT(0.5_SRK),-SQRT(0.5_SRK)/)),'(2,4)')
  CALL testGraph%getMidPointOnEdge(3,4,m)
  ASSERT(ALL(m .APPROXEQ. (/SQRT(0.5_SRK),SQRT(0.5_SRK)/)),'(3,4)')
  CALL testGraph%clear()

  COMPONENT_TEST('Half-Circle Top')
  testCoord(:,1)=(/-1.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,-1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  DO i=1,3
    CALL testGraph%insertVertex(testCoord(:,i))
  ENDDO
  c=0.0_SRK
  r=1.0_SRK
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2),c,r)
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3),c,r)
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c,r)
  CALL testGraph%getMidPointOnEdge(1,2,m)
  ASSERT(ALL(m .APPROXEQ. (/-SQRT(0.5_SRK),-SQRT(0.5_SRK)/)),'(1,2)')
  CALL testGraph%getMidPointOnEdge(1,3,m)
  ASSERT(ALL(m .APPROXEQ. (/0.0_SRK,1.0_SRK/)),'(1,3)')
  CALL testGraph%getMidPointOnEdge(2,3,m)
  ASSERT(ALL(m .APPROXEQ. (/SQRT(0.5_SRK),-SQRT(0.5_SRK)/)),'(2,3)')
  CALL testGraph%clear()

  COMPONENT_TEST('Half-Circle Left')
  testCoord(:,1)=(/0.0_SRK,-1.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  DO i=1,3
    CALL testGraph%insertVertex(testCoord(:,i))
  ENDDO
  c=0.0_SRK
  r=1.0_SRK
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2),c,r)
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3),c,r)
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c,r)
  CALL testGraph%getMidPointOnEdge(1,2,m)
  ASSERT(ALL(m .APPROXEQ. (/-1.0_SRK,0.0_SRK/)),'(1,2)')
  CALL testGraph%getMidPointOnEdge(1,3,m)
  ASSERT(ALL(m .APPROXEQ. (/SQRT(0.5_SRK),-SQRT(0.5_SRK)/)),'(1,3)')
  CALL testGraph%getMidPointOnEdge(2,3,m)
  ASSERT(ALL(m .APPROXEQ. (/SQRT(0.5_SRK),SQRT(0.5_SRK)/)),'(2,3)')
  CALL testGraph%clear()

  COMPONENT_TEST('Half-Circle Bottom')
  testCoord(:,1)=(/-1.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  DO i=1,3
    CALL testGraph%insertVertex(testCoord(:,i))
  ENDDO
  c=0.0_SRK
  r=1.0_SRK
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2),c,r)
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3),c,-r)
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c,r)
  CALL testGraph%getMidPointOnEdge(1,2,m)
  ASSERT(ALL(m .APPROXEQ. (/-SQRT(0.5_SRK),SQRT(0.5_SRK)/)),'(1,2)')
  CALL testGraph%getMidPointOnEdge(1,3,m)
  ASSERT(ALL(m .APPROXEQ. (/0.0_SRK,-1.0_SRK/)),'(1,3)')
  CALL testGraph%getMidPointOnEdge(2,3,m)
  ASSERT(ALL(m .APPROXEQ. (/SQRT(0.5_SRK),SQRT(0.5_SRK)/)),'(2,3)')
  CALL testGraph%clear()

  COMPONENT_TEST('Half-Circle Right')
  testCoord(:,1)=(/-1.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,-1.0_SRK/)
  testCoord(:,3)=(/0.0_SRK,1.0_SRK/)
  DO i=1,3
    CALL testGraph%insertVertex(testCoord(:,i))
  ENDDO
  c=0.0_SRK
  r=1.0_SRK
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2),c,r)
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3),c,r)
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c,-r)
  CALL testGraph%getMidPointOnEdge(1,2,m)
  ASSERT(ALL(m .APPROXEQ. (/-SQRT(0.5_SRK),-SQRT(0.5_SRK)/)),'(1,2)')
  CALL testGraph%getMidPointOnEdge(1,3,m)
  ASSERT(ALL(m .APPROXEQ. (/-SQRT(0.5_SRK),SQRT(0.5_SRK)/)),'(1,3)')
  CALL testGraph%getMidPointOnEdge(2,3,m)
  ASSERT(ALL(m .APPROXEQ. (/1.0_SRK,0.0_SRK/)),'(2,3)')
  CALL testGraph%clear()
ENDSUBROUTINE testGetMidPoint
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
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c0,r)
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
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c0,r)
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
  CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))

  ASSERT_EQ(testGraph%nAdjacent(0),0,'0')
  ASSERT_EQ(testGraph%nAdjacent(10),0,'n+1')

  ASSERT_EQ(testGraph%nAdjacent(1),0,'v1')
  ASSERT_EQ(testGraph%nAdjacent(2),1,'v2')
  ASSERT_EQ(testGraph%nAdjacent(3),3,'v3')
  ASSERT_EQ(testGraph%nAdjacent(4),1,'v4')
  ASSERT_EQ(testGraph%nAdjacent(5),4,'v5')
  ASSERT_EQ(testGraph%nAdjacent(6),1,'v6')
  ASSERT_EQ(testGraph%nAdjacent(7),1,'v7')
  ASSERT_EQ(testGraph%nAdjacent(8),2,'v8')
  ASSERT_EQ(testGraph%nAdjacent(9),1,'v9')

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
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c0,r)
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
  CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))

  ASSERT_EQ(testGraph%getAdjacentVert(0,1),0,'0')
  ASSERT_EQ(testGraph%getAdjacentVert(10,1),0,'n+1')
  ASSERT_EQ(testGraph%getAdjacentVert(1,0),0,'edge 0')
  ASSERT_EQ(testGraph%getAdjacentVert(1,10),0,'edge 10')

  ASSERT_EQ(testGraph%getAdjacentVert(1,1),0,'(1,1)')
  ASSERT_EQ(testGraph%getAdjacentVert(2,1),3,'(2,1)')
  ASSERT_EQ(testGraph%getAdjacentVert(2,2),0,'(2,2)')
  ASSERT_EQ(testGraph%getAdjacentVert(3,1),2,'(3,1)')
  ASSERT_EQ(testGraph%getAdjacentVert(3,2),4,'(3,2)')
  ASSERT_EQ(testGraph%getAdjacentVert(3,3),5,'(3,3)')
  ASSERT_EQ(testGraph%getAdjacentVert(3,4),0,'(3,4)')
  ASSERT_EQ(testGraph%getAdjacentVert(4,1),3,'(4,1)')
  ASSERT_EQ(testGraph%getAdjacentVert(4,2),0,'(4,2)')
  ASSERT_EQ(testGraph%getAdjacentVert(5,1),3,'(5,1)')
  ASSERT_EQ(testGraph%getAdjacentVert(5,2),6,'(5,2)')
  ASSERT_EQ(testGraph%getAdjacentVert(5,3),7,'(5,3)')
  ASSERT_EQ(testGraph%getAdjacentVert(5,4),8,'(5,4)')
  ASSERT_EQ(testGraph%getAdjacentVert(5,5),0,'(5,5)')
  ASSERT_EQ(testGraph%getAdjacentVert(6,1),5,'(6,1)')
  ASSERT_EQ(testGraph%getAdjacentVert(6,2),0,'(6,2)')
  ASSERT_EQ(testGraph%getAdjacentVert(7,1),5,'(7,1)')
  ASSERT_EQ(testGraph%getAdjacentVert(7,2),0,'(7,2)')
  ASSERT_EQ(testGraph%getAdjacentVert(8,1),5,'(8,1)')
  ASSERT_EQ(testGraph%getAdjacentVert(8,2),9,'(8,2)')
  ASSERT_EQ(testGraph%getAdjacentVert(8,3),0,'(8,3)')
  ASSERT_EQ(testGraph%getAdjacentVert(9,1),8,'(9,1)')
  ASSERT_EQ(testGraph%getAdjacentVert(9,2),0,'(9,2)')

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
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c0,r)
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
  CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
  CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))

  COMPONENT_TEST('Bad verts')
  ASSERT_EQ(testGraph%getCWMostVert(0,1),0,'(0,1)')
  ASSERT_EQ(testGraph%getCWMostVert(10,1),0,'(10,1)')
  ASSERT_EQ(testGraph%getCWMostVert(1,-1),0,'(1,-1)')
  ASSERT_EQ(testGraph%getCWMostVert(1,10),0,'(1,10)')

  COMPONENT_TEST('Isolated vert')
  ASSERT_EQ(testGraph%getCWMostVert(1,1),0,'')

  COMPONENT_TEST('End vert')
  ASSERT_EQ(testGraph%getCWMostVert(0,2),3,'(0,2)')
  ASSERT_EQ(testGraph%getCWMostVert(1,2),0,'(1,2)')
  ASSERT_EQ(testGraph%getCWMostVert(2,2),3,'(2,2)')
  ASSERT_EQ(testGraph%getCWMostVert(3,2),0,'(3,2)')

  COMPONENT_TEST('Branch vert')
  ASSERT_EQ(testGraph%getCWMostVert(0,3),2,'(0,3)')
  ASSERT_EQ(testGraph%getCWMostVert(1,3),0,'(1,3)')
  ASSERT_EQ(testGraph%getCWMostVert(2,3),5,'(2,3)')
  ASSERT_EQ(testGraph%getCWMostVert(3,3),2,'(3,3)')
  ASSERT_EQ(testGraph%getCWMostVert(4,3),2,'(4,3)')
  ASSERT_EQ(testGraph%getCWMostVert(5,3),4,'(5,3)')
  CALL testGraph%clear()

  COMPONENT_TEST('Two Point')
  testCoord(:,1)=(/-0.564_SRK,0.0_SRK/)
  testCoord(:,2)=(/-0.521068056_SRK,-0.215833456_SRK/)
  testCoord(:,3)=(/-0.521068056_SRK,0.215833456_SRK/)
  DO i=1,3
    CALL testGraph%insertVertex(testCoord(:,i))
  ENDDO
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
  ASSERT_EQ(testGraph%getCWMostVert(0,1),2,'(0,1)')
  ASSERT_EQ(testGraph%getCWMostVert(2,1),3,'(2,1)')
  ASSERT_EQ(testGraph%getCWMostVert(3,1),2,'(3,1)')

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
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c0,r)
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
  CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
  CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))

  COMPONENT_TEST('Bad verts')
  ASSERT_EQ(testGraph%getCCWMostVert(0,1),0,'(0,1)')
  ASSERT_EQ(testGraph%getCCWMostVert(10,1),0,'(10,1)')
  ASSERT_EQ(testGraph%getCCWMostVert(1,-1),0,'(1,-1)')
  ASSERT_EQ(testGraph%getCCWMostVert(1,10),0,'(1,10)')

  COMPONENT_TEST('Isolated vert')
  ASSERT_EQ(testGraph%getCCWMostVert(1,1),0,'')

  COMPONENT_TEST('End vert')
  ASSERT_EQ(testGraph%getCCWMostVert(0,2),3,'(0,2)')
  ASSERT_EQ(testGraph%getCCWMostVert(1,2),0,'(1,2)')
  ASSERT_EQ(testGraph%getCCWMostVert(2,2),3,'(2,2)')
  ASSERT_EQ(testGraph%getCCWMostVert(3,2),0,'(3,2)')

  COMPONENT_TEST('Branch vert')
  CALL testGraph%editToVTK('testCCW.vtk')
  ASSERT_EQ(testGraph%getCCWMostVert(0,3),4,'(0,3)')
  ASSERT_EQ(testGraph%getCCWMostVert(1,3),0,'(1,3)')
  ASSERT_EQ(testGraph%getCCWMostVert(2,3),4,'(2,3)')
  ASSERT_EQ(testGraph%getCCWMostVert(3,3),4,'(3,3)')
  ASSERT_EQ(testGraph%getCCWMostVert(4,3),5,'(4,3)')
  ASSERT_EQ(testGraph%getCCWMostVert(5,3),2,'(5,3)')

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
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c0,r)
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
  CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))

  COMPONENT_TEST('Bad Vertices')
  CALL testGraph%removeVertex(0)
  ASSERT_EQ(testGraph%nVert(),9,'nvert 0')
  ASSERT_EQ(testGraph%nEdge(),7,'nedge 0')
  CALL testGraph%removeVertex(10)
  ASSERT_EQ(testGraph%nVert(),9,'nvert n+1')
  ASSERT_EQ(testGraph%nEdge(),7,'nedge n+1')
  CALL testGraph%removeVertex(c0)
  ASSERT_EQ(testGraph%nVert(),9,'nvert (1,1)')
  ASSERT_EQ(testGraph%nEdge(),7,'nedge (1,1)')

  COMPONENT_TEST('Isolated vertex')
  CALL testGraph%removeVertex(1)
  ASSERT_EQ(testGraph%nVert(),8,'nvert')
  ASSERT_EQ(testGraph%nEdge(),7,'nedge')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),-1,'E(1,2)')
  ASSERT_EQ(testGraph%edgeMatrix(2,3),1,'E(2,3)')
  ASSERT_EQ(testGraph%edgeMatrix(2,4),1,'E(2,4)')
  ASSERT_EQ(testGraph%edgeMatrix(4,5),1,'E(4,5)')
  ASSERT_EQ(testGraph%edgeMatrix(4,6),1,'E(4,6)')
  ASSERT_EQ(testGraph%edgeMatrix(4,7),1,'E(4,7)')
  ASSERT_EQ(testGraph%edgeMatrix(7,8),1,'E(7,8)')
  bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
  ASSERT(bool,'QE(1,2)')
  CALL symEdgeCheck()

  COMPONENT_TEST('V5')
  CALL testGraph%removeVertex(testCoord(:,5))
  ASSERT_EQ(testGraph%nVert(),7,'nvert')
  ASSERT_EQ(testGraph%nEdge(),3,'nedge')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),-1,'E(1,2)')
  ASSERT_EQ(testGraph%edgeMatrix(2,3),1,'E(2,3)')
  ASSERT_EQ(testGraph%edgeMatrix(6,7),1,'E(6,7)')
  bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
  ASSERT(bool,'QE(1,2)')
  CALL symEdgeCheck()

  COMPONENT_TEST('Endpoint')
  CALL testGraph%removeVertex(1)
  ASSERT_EQ(testGraph%nVert(),6,'nvert')
  ASSERT_EQ(testGraph%nEdge(),2,'nedge')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),1,'E(1,2)')
  ASSERT_EQ(testGraph%edgeMatrix(5,6),1,'E(5,6)')
  CALL symEdgeCheck()

  CALL testGraph%clear()
  CALL testGraph%removeVertex(1)
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
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c0,r)
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
  CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))

  COMPONENT_TEST('Bad Points')
  CALL testGraph%removeEdge(0,1)
  ASSERT_EQ(testGraph%nEdge(),7,'nedge (0,1)')
  CALL testGraph%removeEdge(1,0)
  ASSERT_EQ(testGraph%nEdge(),7,'nedge (1,0)')
  CALL testGraph%removeEdge(10,1)
  ASSERT_EQ(testGraph%nEdge(),7,'nedge (10,1)')
  CALL testGraph%removeEdge(1,10)
  ASSERT_EQ(testGraph%nEdge(),7,'nedge (1,10)')
  CALL testGraph%removeEdge(c0,testCoord(:,2))
  ASSERT_EQ(testGraph%nEdge(),7,'nedge (c0,2)')
  CALL testGraph%removeEdge(testCoord(:,2),c0)
  ASSERT_EQ(testGraph%nEdge(),7,'nedge (2,c0)')

  COMPONENT_TEST('Same Vertex')
  CALL testGraph%removeEdge(1,1)
  ASSERT_EQ(testGraph%nEdge(),7,'nedge')
  ASSERT_EQ(testGraph%edgeMatrix(2,3),-1,'E(2,3)')
  ASSERT_EQ(testGraph%edgeMatrix(3,4),1,'E(3,4)')
  ASSERT_EQ(testGraph%edgeMatrix(3,5),1,'E(3,5)')
  ASSERT_EQ(testGraph%edgeMatrix(5,6),1,'E(5,6)')
  ASSERT_EQ(testGraph%edgeMatrix(5,7),1,'E(5,7)')
  ASSERT_EQ(testGraph%edgeMatrix(5,8),1,'E(5,8)')
  ASSERT_EQ(testGraph%edgeMatrix(8,9),1,'E(8,9)')
  bool=ALL(testGraph%quadEdges(:,2,3) == (/c0(1),c0(2),r/))
  ASSERT(bool,'QE(2,3)')
  CALL symEdgeCheck()

  COMPONENT_TEST('Non-existent edge (IJ)')
  CALL testGraph%removeEdge(1,2)
  ASSERT_EQ(testGraph%nEdge(),7,'nedge')
  ASSERT_EQ(testGraph%edgeMatrix(2,3),-1,'E(2,3)')
  ASSERT_EQ(testGraph%edgeMatrix(3,4),1,'E(3,4)')
  ASSERT_EQ(testGraph%edgeMatrix(3,5),1,'E(3,5)')
  ASSERT_EQ(testGraph%edgeMatrix(5,6),1,'E(5,6)')
  ASSERT_EQ(testGraph%edgeMatrix(5,7),1,'E(5,7)')
  ASSERT_EQ(testGraph%edgeMatrix(5,8),1,'E(5,8)')
  ASSERT_EQ(testGraph%edgeMatrix(8,9),1,'E(8,9)')
  bool=ALL(testGraph%quadEdges(:,2,3) == (/c0(1),c0(2),r/))
  ASSERT(bool,'QE(2,3)')
  CALL symEdgeCheck()

  COMPONENT_TEST('Non-existent edge')
  CALL testGraph%removeEdge(testCoord(:,4),testCoord(:,5))
  ASSERT_EQ(testGraph%nEdge(),7,'nedge')
  ASSERT_EQ(testGraph%edgeMatrix(2,3),-1,'E(2,3)')
  ASSERT_EQ(testGraph%edgeMatrix(3,4),1,'E(3,4)')
  ASSERT_EQ(testGraph%edgeMatrix(3,5),1,'E(3,5)')
  ASSERT_EQ(testGraph%edgeMatrix(5,6),1,'E(5,6)')
  ASSERT_EQ(testGraph%edgeMatrix(5,7),1,'E(5,7)')
  ASSERT_EQ(testGraph%edgeMatrix(5,8),1,'E(5,8)')
  ASSERT_EQ(testGraph%edgeMatrix(8,9),1,'E(8,9)')
  bool=ALL(testGraph%quadEdges(:,2,3) == (/c0(1),c0(2),r/))
  ASSERT(bool,'QE(2,3)')
  CALL symEdgeCheck()

  COMPONENT_TEST('Valid Edge')
  CALL testGraph%removeEdge(testCoord(:,3),testCoord(:,4))
  ASSERT_EQ(testGraph%nEdge(),6,'nedge')
  ASSERT_EQ(testGraph%edgeMatrix(2,3),-1,'E(2,3)')
  ASSERT_EQ(testGraph%edgeMatrix(3,5),1,'E(3,5)')
  ASSERT_EQ(testGraph%edgeMatrix(5,6),1,'E(5,6)')
  ASSERT_EQ(testGraph%edgeMatrix(5,7),1,'E(5,7)')
  ASSERT_EQ(testGraph%edgeMatrix(5,8),1,'E(5,8)')
  ASSERT_EQ(testGraph%edgeMatrix(8,9),1,'E(8,9)')
  bool=ALL(testGraph%quadEdges(:,2,3) == (/c0(1),c0(2),r/))
  ASSERT(bool,'QE(2,3)')
  CALL symEdgeCheck()

  COMPONENT_TEST('Valid Edge (IJ)')
  CALL testGraph%removeEdge(2,3)
  ASSERT_EQ(testGraph%nEdge(),5,'nedge')
  ASSERT_EQ(testGraph%edgeMatrix(3,5),1,'E(3,5)')
  ASSERT_EQ(testGraph%edgeMatrix(5,6),1,'E(5,6)')
  ASSERT_EQ(testGraph%edgeMatrix(5,7),1,'E(5,7)')
  ASSERT_EQ(testGraph%edgeMatrix(5,8),1,'E(5,8)')
  ASSERT_EQ(testGraph%edgeMatrix(8,9),1,'E(8,9)')
  CALL symEdgeCheck()

  CALL testGraph%clear()
  CALL testGraph%removeEdge(2,3)
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
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c0,r)
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
  ASSERT_EQ(testGraph%nVert(),9,'nvert')
  ASSERT_EQ(testGraph%nEdge(),8,'nedge')
  ASSERT_EQ(testGraph%edgeMatrix(2,3),-1,'E(2,3)')
  ASSERT_EQ(testGraph%edgeMatrix(3,4),1,'E(3,4)')
  ASSERT_EQ(testGraph%edgeMatrix(3,5),1,'E(3,5)')
  ASSERT_EQ(testGraph%edgeMatrix(5,6),1,'E(5,6)')
  ASSERT_EQ(testGraph%edgeMatrix(5,7),1,'E(5,7)')
  ASSERT_EQ(testGraph%edgeMatrix(5,8),1,'E(5,8)')
  ASSERT_EQ(testGraph%edgeMatrix(6,7),1,'E(6,7)')
  ASSERT_EQ(testGraph%edgeMatrix(8,9),1,'E(8,9)')
  bool=ALL(testGraph%quadEdges(:,2,3) == (/c0(1),c0(2),r/))
  ASSERT(bool,'QE(2,3)')
  CALL symEdgeCheck()

  COMPONENT_TEST('Isolated point')
  CALL testGraph%removeFilamentFromVert(1,1)
  ASSERT_EQ(testGraph%nVert(),8,'nvert')
  ASSERT_EQ(testGraph%nEdge(),8,'nedge')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),-1,'E(2,3)')
  ASSERT_EQ(testGraph%edgeMatrix(2,3),1,'E(3,4)')
  ASSERT_EQ(testGraph%edgeMatrix(2,4),1,'E(3,5)')
  ASSERT_EQ(testGraph%edgeMatrix(4,5),1,'E(5,6)')
  ASSERT_EQ(testGraph%edgeMatrix(4,6),1,'E(5,7)')
  ASSERT_EQ(testGraph%edgeMatrix(4,7),1,'E(5,8)')
  ASSERT_EQ(testGraph%edgeMatrix(5,6),1,'E(6,7)')
  ASSERT_EQ(testGraph%edgeMatrix(7,8),1,'E(8,9)')
  bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
  ASSERT(bool,'QE(2,3)')
  CALL symEdgeCheck()

  COMPONENT_TEST('1 vert')
  CALL testGraph%removeFilamentFromVert(1,1)
  CALL testGraph%editToVTK('testRemFil_3.vtk')
  ASSERT_EQ(testGraph%nVert(),7,'nvert')
  ASSERT_EQ(testGraph%nEdge(),7,'nedge')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),1,'E(3,4)')
  ASSERT_EQ(testGraph%edgeMatrix(1,3),1,'E(3,5)')
  ASSERT_EQ(testGraph%edgeMatrix(3,4),1,'E(5,6)')
  ASSERT_EQ(testGraph%edgeMatrix(3,5),1,'E(5,7)')
  ASSERT_EQ(testGraph%edgeMatrix(3,6),1,'E(5,8)')
  ASSERT_EQ(testGraph%edgeMatrix(4,5),1,'E(6,7)')
  ASSERT_EQ(testGraph%edgeMatrix(6,7),1,'E(8,9)')
  CALL symEdgeCheck()

  COMPONENT_TEST('3 verts')
  CALL testGraph%removeFilamentFromVert(7,7)
  CALL testGraph%editToVTK('testRemFil_4.vtk')
  ASSERT_EQ(testGraph%nVert(),5,'nvert')
  ASSERT_EQ(testGraph%nEdge(),5,'nedge')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),1,'E(3,4)')
  ASSERT_EQ(testGraph%edgeMatrix(1,3),1,'E(3,5)')
  ASSERT_EQ(testGraph%edgeMatrix(3,4),1,'E(5,6)')
  ASSERT_EQ(testGraph%edgeMatrix(3,5),1,'E(5,7)')
  ASSERT_EQ(testGraph%edgeMatrix(4,5),1,'E(6,7)')
  CALL symEdgeCheck()

  !Not sure if this is a bug or not...
  COMPONENT_TEST('Branch point (w/ edge)')
  CALL testGraph%removeFilamentFromVert(1,1)
  CALL testGraph%editToVTK('testRemFil_5.vtk')
  ASSERT_EQ(testGraph%nVert(),5,'nvert')
  ASSERT_EQ(testGraph%nEdge(),5,'nedge')
  ASSERT_EQ(testGraph%edgeMatrix(1,2),1,'E(3,4)')
  ASSERT_EQ(testGraph%edgeMatrix(1,3),1,'E(3,5)')
  ASSERT_EQ(testGraph%edgeMatrix(3,4),1,'E(5,6)')
  ASSERT_EQ(testGraph%edgeMatrix(3,5),1,'E(5,7)')
  ASSERT_EQ(testGraph%edgeMatrix(4,5),1,'E(6,7)')
  CALL symEdgeCheck()

  CALL testGraph%clear()
ENDSUBROUTINE testRemFil
!
!-------------------------------------------------------------------------------
SUBROUTINE testGetMCB()
  LOGICAL(SBK) :: bool
  INTEGER(SIK) :: i
  REAL(SRK) :: testCoord(2,18),c0(2),r
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
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,3),c0,r)
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,8))
  CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
  CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,9))

  CALL testGraph%getMCB(cycles)
  ASSERTFAIL(ALLOCATED(cycles),'allocated')
  ASSERT_EQ(SIZE(cycles),1,'size')
  ASSERT_EQ(cycles(1)%nVert(),3,'cycle vert')
  bool=ALL(cycles(1)%vertices(:,1) == testCoord(:,5))
  ASSERT(bool,'vert 1')
  bool=ALL(cycles(1)%vertices(:,2) == testCoord(:,6))
  ASSERT(bool,'vert 2')
  bool=ALL(cycles(1)%vertices(:,3) == testCoord(:,7))
  ASSERT(bool,'vert 3')
  ASSERT_EQ(cycles(1)%edgeMatrix(1,2),1,'edge(1,2)')
  ASSERT_EQ(cycles(1)%edgeMatrix(1,3),1,'edge(1,3)')
  ASSERT_EQ(cycles(1)%edgeMatrix(2,3),1,'edge(2,3)')
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
  ASSERT_EQ(SIZE(cycles),1,'size')
  ASSERT_EQ(cycles(1)%nVert(),4,'cycle vert')
  bool=ALL(cycles(1)%vertices(:,1) == testCoord(:,1))
  ASSERT(bool,'vert 1')
  bool=ALL(cycles(1)%vertices(:,2) == testCoord(:,2))
  ASSERT(bool,'vert 2')
  bool=ALL(cycles(1)%vertices(:,3) == testCoord(:,3))
  ASSERT(bool,'vert 3')
  bool=ALL(cycles(1)%vertices(:,4) == testCoord(:,4))
  ASSERT(bool,'vert 4')
  ASSERT_EQ(cycles(1)%edgeMatrix(1,2),1,'edge(1,2)')
  ASSERT_EQ(cycles(1)%edgeMatrix(1,3),1,'edge(1,3)')
  ASSERT_EQ(cycles(1)%edgeMatrix(3,4),1,'edge(3,4)')
  ASSERT_EQ(cycles(1)%edgeMatrix(2,4),1,'edge(2,4)')

  COMPONENT_TEST('2 cycles')
  CALL testGraph%insertVertex(testCoord(:,5))
  CALL testGraph%insertVertex(testCoord(:,6))
  CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,6))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,6))
  CALL testGraph%getMCB(cycles)
  ASSERTFAIL(ALLOCATED(cycles),'allocated')
  ASSERT_EQ(SIZE(cycles),2,'size')
  ASSERT_EQ(cycles(1)%nVert(),4,'1 cycle vert')
  bool=ALL(cycles(1)%vertices(:,1) == testCoord(:,1))
  ASSERT(bool,'1 vert 1')
  bool=ALL(cycles(1)%vertices(:,2) == testCoord(:,2))
  ASSERT(bool,'1 vert 2')
  bool=ALL(cycles(1)%vertices(:,3) == testCoord(:,3))
  ASSERT(bool,'1 vert 3')
  bool=ALL(cycles(1)%vertices(:,4) == testCoord(:,4))
  ASSERT(bool,'1 vert 4')
  ASSERT_EQ(cycles(1)%edgeMatrix(1,2),1,'1 edge(1,2)')
  ASSERT_EQ(cycles(1)%edgeMatrix(1,3),1,'1 edge(1,3)')
  ASSERT_EQ(cycles(1)%edgeMatrix(3,4),1,'1 edge(3,4)')
  ASSERT_EQ(cycles(1)%edgeMatrix(2,4),1,'1 edge(2,4)')
  ASSERT_EQ(cycles(2)%nVert(),4,'2 cycle vert')
  bool=ALL(cycles(2)%vertices(:,1) == testCoord(:,3))
  ASSERT(bool,'2 vert 1')
  bool=ALL(cycles(2)%vertices(:,2) == testCoord(:,4))
  ASSERT(bool,'2 vert 2')
  bool=ALL(cycles(2)%vertices(:,3) == testCoord(:,5))
  ASSERT(bool,'2 vert 3')
  bool=ALL(cycles(2)%vertices(:,4) == testCoord(:,6))
  ASSERT(bool,'2 vert 4')
  ASSERT_EQ(cycles(2)%edgeMatrix(1,2),1,'2 edge(1,2)')
  ASSERT_EQ(cycles(2)%edgeMatrix(1,3),1,'2 edge(1,3)')
  ASSERT_EQ(cycles(2)%edgeMatrix(3,4),1,'2 edge(3,4)')
  ASSERT_EQ(cycles(2)%edgeMatrix(2,4),1,'2 edge(2,4)')

  COMPONENT_TEST('2 cycles + filament')
  CALL testGraph%insertVertex(testCoord(:,7))
  CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,7))
  CALL testGraph%getMCB(cycles)
  ASSERTFAIL(ALLOCATED(cycles),'allocated')
  ASSERT_EQ(SIZE(cycles),2,'size')
  ASSERT_EQ(cycles(1)%nVert(),4,'1 cycle vert')
  bool=ALL(cycles(1)%vertices(:,1) == testCoord(:,1))
  ASSERT(bool,'1 vert 1')
  bool=ALL(cycles(1)%vertices(:,2) == testCoord(:,2))
  ASSERT(bool,'1 vert 2')
  bool=ALL(cycles(1)%vertices(:,3) == testCoord(:,3))
  ASSERT(bool,'1 vert 3')
  bool=ALL(cycles(1)%vertices(:,4) == testCoord(:,4))
  ASSERT(bool,'1 vert 4')
  ASSERT_EQ(cycles(1)%edgeMatrix(1,2),1,'1 edge(1,2)')
  ASSERT_EQ(cycles(1)%edgeMatrix(1,3),1,'1 edge(1,3)')
  ASSERT_EQ(cycles(1)%edgeMatrix(3,4),1,'1 edge(3,4)')
  ASSERT_EQ(cycles(1)%edgeMatrix(2,4),1,'1 edge(2,4)')
  ASSERT_EQ(cycles(1)%nVert(),4,'2 cycle vert')
  bool=ALL(cycles(2)%vertices(:,1) == testCoord(:,3))
  ASSERT(bool,'2 vert 1')
  bool=ALL(cycles(2)%vertices(:,2) == testCoord(:,4))
  ASSERT(bool,'2 vert 2')
  bool=ALL(cycles(2)%vertices(:,3) == testCoord(:,5))
  ASSERT(bool,'2 vert 3')
  bool=ALL(cycles(2)%vertices(:,4) == testCoord(:,6))
  ASSERT(bool,'2 vert 4')
  ASSERT_EQ(cycles(2)%edgeMatrix(1,2),1,'2 edge(1,2)')
  ASSERT_EQ(cycles(2)%edgeMatrix(1,3),1,'2 edge(1,3)')
  ASSERT_EQ(cycles(2)%edgeMatrix(3,4),1,'2 edge(3,4)')
  ASSERT_EQ(cycles(2)%edgeMatrix(2,4),1,'2 edge(2,4)')
  CALL testGraph%clear()

  COMPONENT_TEST('KeyHole')
  testCoord(:, 1)=(/-0.564_SRK,0.0_SRK/)
  testCoord(:, 2)=(/-0.521068056_SRK,-0.215833456_SRK/)
  testCoord(:, 3)=(/-0.521068056_SRK,0.215833456_SRK/)
  testCoord(:, 4)=(/-0.398808225_SRK,-0.398808225_SRK/)
  testCoord(:, 5)=(/-0.398808225_SRK,0.398808225_SRK/)
  testCoord(:, 6)=(/-0.215833456_SRK,-0.521068056_SRK/)
  testCoord(:, 7)=(/-0.215833456_SRK,0.521068056_SRK/)
  testCoord(:, 8)=(/0.0_SRK,-0.564_SRK/)
  testCoord(:, 9)=(/0.0_SRK,0.564_SRK/)
  testCoord(:,10)=(/0.215833456_SRK,-0.521068056_SRK/)
  testCoord(:,11)=(/0.215833456_SRK,0.521068056_SRK/)
  testCoord(:,12)=(/0.398808225_SRK,-0.398808225_SRK/)
  testCoord(:,13)=(/0.398808225_SRK,0.398808225_SRK/)
  testCoord(:,14)=(/0.521068056_SRK,-0.215833456_SRK/)
  testCoord(:,15)=(/0.521068056_SRK,0.215833456_SRK/)
  testCoord(:,16)=(/0.564_SRK,0.0_SRK/)
  testCoord(:,17)=(/0.8128_SRK,-0.8128_SRK/)
  testCoord(:,18)=(/0.8128_SRK,0.8128_SRK/)
  DO i=1,18
    CALL testGraph%insertVertex(testCoord(:,i))
  ENDDO
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,5))
  CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,6))
  CALL testGraph%defineEdge(testCoord(:,5),testCoord(:,7))
  CALL testGraph%defineEdge(testCoord(:,6),testCoord(:,8))
  CALL testGraph%defineEdge(testCoord(:,7),testCoord(:,9))
  CALL testGraph%defineEdge(testCoord(:,8),testCoord(:,10))
  CALL testGraph%defineEdge(testCoord(:,9),testCoord(:,11))
  CALL testGraph%defineEdge(testCoord(:,10),testCoord(:,12))
  CALL testGraph%defineEdge(testCoord(:,11),testCoord(:,13))
  CALL testGraph%defineEdge(testCoord(:,12),testCoord(:,14))
  CALL testGraph%defineEdge(testCoord(:,12),testCoord(:,17))
  CALL testGraph%defineEdge(testCoord(:,13),testCoord(:,15))
  CALL testGraph%defineEdge(testCoord(:,13),testCoord(:,18))
  CALL testGraph%defineEdge(testCoord(:,14),testCoord(:,16))
  CALL testGraph%defineEdge(testCoord(:,15),testCoord(:,16))
  CALL testGraph%defineEdge(testCoord(:,17),testCoord(:,18))
  CALL testGraph%getMCB(cycles)
  ASSERT_EQ(SIZE(cycles),2,'SIZE(cycles)')
  ASSERT_EQ(cycles(1)%nVert(),16,'c1 nVert')
  ASSERT(ALL(cycles(1)%vertices(:, 1) == testCoord(:, 1)),'c1 v1')
  ASSERT(ALL(cycles(1)%vertices(:, 2) == testCoord(:, 2)),'c1 v2')
  ASSERT(ALL(cycles(1)%vertices(:, 3) == testCoord(:, 3)),'c1 v3')
  ASSERT(ALL(cycles(1)%vertices(:, 4) == testCoord(:, 4)),'c1 v4')
  ASSERT(ALL(cycles(1)%vertices(:, 5) == testCoord(:, 5)),'c1 v5')
  ASSERT(ALL(cycles(1)%vertices(:, 6) == testCoord(:, 6)),'c1 v6')
  ASSERT(ALL(cycles(1)%vertices(:, 7) == testCoord(:, 7)),'c1 v7')
  ASSERT(ALL(cycles(1)%vertices(:, 8) == testCoord(:, 8)),'c1 v8')
  ASSERT(ALL(cycles(1)%vertices(:, 9) == testCoord(:, 9)),'c1 v9')
  ASSERT(ALL(cycles(1)%vertices(:,10) == testCoord(:,10)),'c1 v10')
  ASSERT(ALL(cycles(1)%vertices(:,11) == testCoord(:,11)),'c1 v11')
  ASSERT(ALL(cycles(1)%vertices(:,12) == testCoord(:,12)),'c1 v12')
  ASSERT(ALL(cycles(1)%vertices(:,13) == testCoord(:,13)),'c1 v13')
  ASSERT(ALL(cycles(1)%vertices(:,14) == testCoord(:,14)),'c1 v14')
  ASSERT(ALL(cycles(1)%vertices(:,15) == testCoord(:,15)),'c1 v15')
  ASSERT(ALL(cycles(1)%vertices(:,16) == testCoord(:,16)),'c1 v16')
  ASSERT_EQ(cycles(2)%nVert(),7,'c2 nVer')
  ASSERT(ALL(cycles(2)%vertices(:, 1) == testCoord(:,12)),'c2 v1')
  ASSERT(ALL(cycles(2)%vertices(:, 2) == testCoord(:,13)),'c2 v2')
  ASSERT(ALL(cycles(2)%vertices(:, 3) == testCoord(:,14)),'c2 v3')
  ASSERT(ALL(cycles(2)%vertices(:, 4) == testCoord(:,15)),'c2 v4')
  ASSERT(ALL(cycles(2)%vertices(:, 5) == testCoord(:,16)),'c2 v5')
  ASSERT(ALL(cycles(2)%vertices(:, 6) == testCoord(:,17)),'c2 v6')
  ASSERT(ALL(cycles(2)%vertices(:, 7) == testCoord(:,18)),'c2 v7')
  CALL testGraph%clear()
ENDSUBROUTINE testGetMCB
!
!-------------------------------------------------------------------------------
SUBROUTINE testCombine()
  LOGICAL(SBK) :: bool
  REAL(SRK) :: testCoord(2,4),c(2),r
  TYPE(GraphType) :: g2

  COMPONENT_TEST('Cartesian Grid in Box')
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  CALL testGraph%insertVertex(testCoord(:,1))
  CALL testGraph%insertVertex(testCoord(:,2))
  CALL testGraph%insertVertex(testCoord(:,3))
  CALL testGraph%insertVertex(testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))

  testCoord(:,1)=(/0.0_SRK,0.25_SRK/)
  testCoord(:,2)=(/1.0_SRK,0.25_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),6,'nvert 1')
  ASSERT_EQ(testGraph%nEdge(),7,'nedge 1')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/)) ,'vert 1 (1)')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,0.25_SRK/)),'vert 2 (1)')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.0_SRK,1.0_SRK/)) ,'vert 3 (1)')
  ASSERT(ALL(testGraph%vertices(:,4) == (/1.0_SRK,0.0_SRK/)) ,'vert 4 (1)')
  ASSERT(ALL(testGraph%vertices(:,5) == (/1.0_SRK,0.25_SRK/)),'vert 5 (1)')
  ASSERT(ALL(testGraph%vertices(:,6) == (/1.0_SRK,1.0_SRK/)) ,'vert 6 (1)')
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,1,0,0/))
  ASSERT(bool,'edges 1 (1)')
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,1,0,1,0/))
  ASSERT(bool,'edges 2 (1)')
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,1,0,0,0,1/))
  ASSERT(bool,'edges 3 (1)')
  bool=ALL(testGraph%edgeMatrix(:,4) == (/1,0,0,0,1,0/))
  ASSERT(bool,'edges 4 (1)')
  bool=ALL(testGraph%edgeMatrix(:,5) == (/0,1,0,1,0,1/))
  ASSERT(bool,'edges 5 (1)')
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,0,1,0,1,0/))
  ASSERT(bool,'edges 6 (1)')

  CALL g2%clear()
  testCoord(:,1)=(/0.0_SRK,0.75_SRK/)
  testCoord(:,2)=(/1.0_SRK,0.75_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),8,'nvert 2')
  ASSERT_EQ(testGraph%nEdge(),10,'nedge 2')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/)) ,'vert 1 (2)')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,0.25_SRK/)),'vert 2 (2)')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.0_SRK,0.75_SRK/)),'vert 3 (2)')
  ASSERT(ALL(testGraph%vertices(:,4) == (/0.0_SRK,1.0_SRK/)) ,'vert 4 (2)')
  ASSERT(ALL(testGraph%vertices(:,5) == (/1.0_SRK,0.0_SRK/)) ,'vert 5 (2)')
  ASSERT(ALL(testGraph%vertices(:,6) == (/1.0_SRK,0.25_SRK/)),'vert 6 (2)')
  ASSERT(ALL(testGraph%vertices(:,7) == (/1.0_SRK,0.75_SRK/)),'vert 7 (2)')
  ASSERT(ALL(testGraph%vertices(:,8) == (/1.0_SRK,1.0_SRK/)) ,'vert 8 (2)')
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,0,1,0,0,0/))
  ASSERT(bool,'edges 1 (2)')
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,1,0,0,1,0,0/))
  ASSERT(bool,'edges 2 (2)')
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,1,0,1,0,0,1,0/))
  ASSERT(bool,'edges 3 (2)')
  bool=ALL(testGraph%edgeMatrix(:,4) == (/0,0,1,0,0,0,0,1/))
  ASSERT(bool,'edges 4 (2)')
  bool=ALL(testGraph%edgeMatrix(:,5) == (/1,0,0,0,0,1,0,0/))
  ASSERT(bool,'edges 5 (2)')
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,1,0,0,1,0,1,0/))
  ASSERT(bool,'edges 6 (2)')
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,0,1,0,0,1,0,1/))
  ASSERT(bool,'edges 7 (2)')
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,0,0,1,0,0,1,0/))
  ASSERT(bool,'edges 8 (2)')

  CALL g2%clear()
  testCoord(:,1)=(/0.25_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.25_SRK,1.0_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),12,'nvert 3')
  ASSERT_EQ(testGraph%nEdge(),17,'nedge 3')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/))  ,'vert  1 (3)')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,0.25_SRK/)) ,'vert  2 (3)')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.0_SRK,0.75_SRK/)) ,'vert  3 (3)')
  ASSERT(ALL(testGraph%vertices(:,4) == (/0.0_SRK,1.0_SRK/))  ,'vert  4 (3)')
  ASSERT(ALL(testGraph%vertices(:,5) == (/0.25_SRK,0.0_SRK/)) ,'vert  5 (3)')
  ASSERT(ALL(testGraph%vertices(:,6) == (/0.25_SRK,0.25_SRK/)),'vert  6 (3)')
  ASSERT(ALL(testGraph%vertices(:,7) == (/0.25_SRK,0.75_SRK/)),'vert  7 (3)')
  ASSERT(ALL(testGraph%vertices(:,8) == (/0.25_SRK,1.0_SRK/)) ,'vert  8 (3)')
  ASSERT(ALL(testGraph%vertices(:,9) == (/1.0_SRK,0.0_SRK/))  ,'vert  9 (3)')
  ASSERT(ALL(testGraph%vertices(:,10) == (/1.0_SRK,0.25_SRK/)),'vert 10 (3)')
  ASSERT(ALL(testGraph%vertices(:,11) == (/1.0_SRK,0.75_SRK/)),'vert 11 (3)')
  ASSERT(ALL(testGraph%vertices(:,12) == (/1.0_SRK,1.0_SRK/)) ,'vert 12 (3)')
                                        ! 1 2 3 4 5 6 7 8 9 0 1 2
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,0,1,0,0,0,0,0,0,0/))
  ASSERT(bool,'edges 1 (3)')            ! 1 2 3 4 5 6 7 8 9 0 1 2
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,1,0,0,1,0,0,0,0,0,0/))
  ASSERT(bool,'edges 2 (3)')            ! 1 2 3 4 5 6 7 8 9 0 1 2
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,1,0,1,0,0,1,0,0,0,0,0/))
  ASSERT(bool,'edges 3 (3)')            ! 1 2 3 4 5 6 7 8 9 0 1 2
  bool=ALL(testGraph%edgeMatrix(:,4) == (/0,0,1,0,0,0,0,1,0,0,0,0/))
  ASSERT(bool,'edges 4 (3)')            ! 1 2 3 4 5 6 7 8 9 0 1 2
  bool=ALL(testGraph%edgeMatrix(:,5) == (/1,0,0,0,0,1,0,0,1,0,0,0/))
  ASSERT(bool,'edges 5 (3)')            ! 1 2 3 4 5 6 7 8 9 0 1 2
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,1,0,0,1,0,1,0,0,1,0,0/))
  ASSERT(bool,'edges 6 (3)')            ! 1 2 3 4 5 6 7 8 9 0 1 2
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,0,1,0,0,1,0,1,0,0,1,0/))
  ASSERT(bool,'edges 7 (3)')            ! 1 2 3 4 5 6 7 8 9 0 1 2
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,0,0,1,0,0,1,0,0,0,0,1/))
  ASSERT(bool,'edges 8 (3)')            ! 1 2 3 4 5 6 7 8 9 0 1 2
  bool=ALL(testGraph%edgeMatrix(:,9) == (/0,0,0,0,1,0,0,0,0,1,0,0/))
  ASSERT(bool,'edges 9 (3)')             ! 1 2 3 4 5 6 7 8 9 0 1 2
  bool=ALL(testGraph%edgeMatrix(:,10) == (/0,0,0,0,0,1,0,0,1,0,1,0/))
  ASSERT(bool,'edges 10 (3)')            ! 1 2 3 4 5 6 7 8 9 0 1 2
  bool=ALL(testGraph%edgeMatrix(:,11) == (/0,0,0,0,0,0,1,0,0,1,0,1/))
  ASSERT(bool,'edges 11 (3)')            ! 1 2 3 4 5 6 7 8 9 0 1 2
  bool=ALL(testGraph%edgeMatrix(:,12) == (/0,0,0,0,0,0,0,1,0,0,1,0/))
  ASSERT(bool,'edges 12 (3)')

  CALL g2%clear()
  testCoord(:,1)=(/0.75_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.75_SRK,1.0_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),16,'nvert 4')
  ASSERT_EQ(testGraph%nEdge(),24,'nedge 4')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/))   ,'vert  1 (4)')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,0.25_SRK/))  ,'vert  2 (4)')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.0_SRK,0.75_SRK/))  ,'vert  3 (4)')
  ASSERT(ALL(testGraph%vertices(:,4) == (/0.0_SRK,1.0_SRK/))   ,'vert  4 (4)')
  ASSERT(ALL(testGraph%vertices(:,5) == (/0.25_SRK,0.0_SRK/))  ,'vert  5 (4)')
  ASSERT(ALL(testGraph%vertices(:,6) == (/0.25_SRK,0.25_SRK/)) ,'vert  6 (4)')
  ASSERT(ALL(testGraph%vertices(:,7) == (/0.25_SRK,0.75_SRK/)) ,'vert  7 (4)')
  ASSERT(ALL(testGraph%vertices(:,8) == (/0.25_SRK,1.0_SRK/))  ,'vert  8 (4)')
  ASSERT(ALL(testGraph%vertices(:,9) == (/0.75_SRK,0.0_SRK/))  ,'vert  9 (4)')
  ASSERT(ALL(testGraph%vertices(:,10) == (/0.75_SRK,0.25_SRK/)),'vert 10 (4)')
  ASSERT(ALL(testGraph%vertices(:,11) == (/0.75_SRK,0.75_SRK/)),'vert 11 (4)')
  ASSERT(ALL(testGraph%vertices(:,12) == (/0.75_SRK,1.0_SRK/)) ,'vert 12 (4)')
  ASSERT(ALL(testGraph%vertices(:,13) == (/1.0_SRK,0.0_SRK/))  ,'vert 13 (4)')
  ASSERT(ALL(testGraph%vertices(:,14) == (/1.0_SRK,0.25_SRK/)) ,'vert 14 (4)')
  ASSERT(ALL(testGraph%vertices(:,15) == (/1.0_SRK,0.75_SRK/)) ,'vert 15 (4)')
  ASSERT(ALL(testGraph%vertices(:,16) == (/1.0_SRK,1.0_SRK/))  ,'vert 16 (4)')
                                        ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0/))
  ASSERT(bool,'edges 1 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0/))
  ASSERT(bool,'edges 2 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,1,0,1,0,0,1,0,0,0,0,0,0,0,0,0/))
  ASSERT(bool,'edges 3 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,4) == (/0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0/))
  ASSERT(bool,'edges 4 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,5) == (/1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0/))
  ASSERT(bool,'edges 5 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,1,0,0,1,0,1,0,0,1,0,0,0,0,0,0/))
  ASSERT(bool,'edges 6 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,0,1,0,0,1,0,1,0,0,1,0,0,0,0,0/))
  ASSERT(bool,'edges 7 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0/))
  ASSERT(bool,'edges 8 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,9) == (/0,0,0,0,1,0,0,0,0,1,0,0,1,0,0,0/))
  ASSERT(bool,'edges 9 (4)')             ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,10) == (/0,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0/))
  ASSERT(bool,'edges 10 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,11) == (/0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0/))
  ASSERT(bool,'edges 11 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,12) == (/0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1/))
  ASSERT(bool,'edges 12 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,13) == (/0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0/))
  ASSERT(bool,'edges 13 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,14) == (/0,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0/))
  ASSERT(bool,'edges 14 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,15) == (/0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,1/))
  ASSERT(bool,'edges 15 (4)')            ! 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
  bool=ALL(testGraph%edgeMatrix(:,16) == (/0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0/))
  ASSERT(bool,'edges 16 (4)')
  CALL g2%clear()
  CALL testGraph%clear()

  COMPONENT_TEST('Box-y-Box')
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  CALL testGraph%insertVertex(testCoord(:,1))
  CALL testGraph%insertVertex(testCoord(:,2))
  CALL testGraph%insertVertex(testCoord(:,3))
  CALL testGraph%insertVertex(testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
  testCoord(:,1)=(/0.5_SRK,0.25_SRK/)
  testCoord(:,2)=(/0.5_SRK,0.75_SRK/)
  testCoord(:,3)=(/1.5_SRK,0.25_SRK/)
  testCoord(:,4)=(/1.5_SRK,0.75_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%insertVertex(testCoord(:,3))
  CALL g2%insertVertex(testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3))
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),10,'nvert')
  ASSERT_EQ(testGraph%nEdge(),12,'nedge')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/))  ,'vert  1')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,1.0_SRK/))  ,'vert  2')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.5_SRK,0.25_SRK/)) ,'vert  3')
  ASSERT(ALL(testGraph%vertices(:,4) == (/0.5_SRK,0.75_SRK/)) ,'vert  4')
  ASSERT(ALL(testGraph%vertices(:,5) == (/1.0_SRK,0.0_SRK/))  ,'vert  5')
  ASSERT(ALL(testGraph%vertices(:,6) == (/1.0_SRK,0.25_SRK/)) ,'vert  6')
  ASSERT(ALL(testGraph%vertices(:,7) == (/1.0_SRK,0.75_SRK/)) ,'vert  7')
  ASSERT(ALL(testGraph%vertices(:,8) == (/1.0_SRK,1.0_SRK/))  ,'vert  8')
  ASSERT(ALL(testGraph%vertices(:,9) == (/1.5_SRK,0.25_SRK/)) ,'vert  9')
  ASSERT(ALL(testGraph%vertices(:,10) == (/1.5_SRK,0.75_SRK/)),'vert 10')
                                        ! 1 2 3 4 5 6 7 8 9 0
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,0,1,0,0,0,0,0/))
  ASSERT(bool,'edges 1')                ! 1 2 3 4 5 6 7 8 9 0
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,0,0,0,0,0,1,0,0/))
  ASSERT(bool,'edges 2')                ! 1 2 3 4 5 6 7 8 9 0
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,0,0,1,0,1,0,0,0,0/))
  ASSERT(bool,'edges 3')                ! 1 2 3 4 5 6 7 8 9 0
  bool=ALL(testGraph%edgeMatrix(:,4) == (/0,0,1,0,0,0,1,0,0,0/))
  ASSERT(bool,'edges 4')                ! 1 2 3 4 5 6 7 8 9 0
  bool=ALL(testGraph%edgeMatrix(:,5) == (/1,0,0,0,0,1,0,0,0,0/))
  ASSERT(bool,'edges 5')                ! 1 2 3 4 5 6 7 8 9 0
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,0,1,0,1,0,1,0,1,0/))
  ASSERT(bool,'edges 6')                ! 1 2 3 4 5 6 7 8 9 0
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,0,0,1,0,1,0,1,0,1/))
  ASSERT(bool,'edges 7')                ! 1 2 3 4 5 6 7 8 9 0
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,1,0,0,0,0,1,0,0,0/))
  ASSERT(bool,'edges 8')                ! 1 2 3 4 5 6 7 8 9 0
  bool=ALL(testGraph%edgeMatrix(:,9) == (/0,0,0,0,0,1,0,0,0,1/))
  ASSERT(bool,'edges 9')                 ! 1 2 3 4 5 6 7 8 9 0
  bool=ALL(testGraph%edgeMatrix(:,10) == (/0,0,0,0,0,0,1,0,1,0/))
  ASSERT(bool,'edges 10')
  CALL g2%clear()
  CALL testGraph%clear()

  COMPONENT_TEST('Circle in Box')
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  CALL testGraph%insertVertex(testCoord(:,1))
  CALL testGraph%insertVertex(testCoord(:,2))
  CALL testGraph%insertVertex(testCoord(:,3))
  CALL testGraph%insertVertex(testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
  testCoord(:,1)=(/0.25_SRK,0.5_SRK/)
  testCoord(:,2)=(/0.5_SRK,0.25_SRK/)
  testCoord(:,3)=(/0.5_SRK,0.75_SRK/)
  testCoord(:,4)=(/0.75_SRK,0.5_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%insertVertex(testCoord(:,3))
  CALL g2%insertVertex(testCoord(:,4))
  c=(/0.5_SRK,0.5_SRK/); r=0.25_SRK
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2),c,r)
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,4),c,r)
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,4),c,r)
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3),c,r)
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),8,'nvert')
  ASSERT_EQ(testGraph%nEdge(),8,'nedge')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/)) ,'vert 1')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,1.0_SRK/)) ,'vert 2')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.25_SRK,0.5_SRK/)),'vert 3')
  ASSERT(ALL(testGraph%vertices(:,4) == (/0.5_SRK,0.25_SRK/)),'vert 4')
  ASSERT(ALL(testGraph%vertices(:,5) == (/0.5_SRK,0.75_SRK/)),'vert 5')
  ASSERT(ALL(testGraph%vertices(:,6) == (/0.75_SRK,0.5_SRK/)),'vert 6')
  ASSERT(ALL(testGraph%vertices(:,7) == (/1.0_SRK,0.0_SRK/)) ,'vert 7')
  ASSERT(ALL(testGraph%vertices(:,8) == (/1.0_SRK,1.0_SRK/)) ,'vert 8')
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,0,0,0,1,0/))
  ASSERT(bool,'edges 1')
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,0,0,0,0,0,1/))
  ASSERT(bool,'edges 2')
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,0,0,-1,-1,0,0,0/))
  ASSERT(bool,'edges 3')
  bool=ALL(testGraph%edgeMatrix(:,4) == (/0,0,-1,0,0,-1,0,0/))
  ASSERT(bool,'edges 4')
  bool=ALL(testGraph%edgeMatrix(:,5) == (/0,0,-1,0,0,-1,0,0/))
  ASSERT(bool,'edges 5')
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,0,0,-1,-1,0,0,0/))
  ASSERT(bool,'edges 6')
  bool=ALL(testGraph%edgeMatrix(:,7) == (/1,0,0,0,0,0,0,1/))
  ASSERT(bool,'edges 7')
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,1,0,0,0,0,1,0/))
  ASSERT(bool,'edges 8')
  CALL g2%clear()
  CALL testGraph%clear()

  COMPONENT_TEST('Box-Circle (1-edge)')
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  CALL testGraph%insertVertex(testCoord(:,1))
  CALL testGraph%insertVertex(testCoord(:,2))
  CALL testGraph%insertVertex(testCoord(:,3))
  CALL testGraph%insertVertex(testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
  testCoord(:,1)=(/0.75_SRK,0.5_SRK/)
  testCoord(:,2)=(/1.0_SRK,0.25_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.75_SRK/)
  testCoord(:,4)=(/1.25_SRK,0.5_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%insertVertex(testCoord(:,3))
  CALL g2%insertVertex(testCoord(:,4))
  c=(/1.0_SRK,0.5_SRK/); r=0.25_SRK
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2),c,r)
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,4),c,r)
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,4),c,r)
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3),c,r)
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),8,'nvert')
  ASSERT_EQ(testGraph%nEdge(),10,'nedge')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/)) ,'vert 1')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,1.0_SRK/)) ,'vert 2')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.75_SRK,0.5_SRK/)),'vert 3')
  ASSERT(ALL(testGraph%vertices(:,4) == (/1.0_SRK,0.0_SRK/)), 'vert 4')
  ASSERT(ALL(testGraph%vertices(:,5) == (/1.0_SRK,0.25_SRK/)),'vert 5')
  ASSERT(ALL(testGraph%vertices(:,6) == (/1.0_SRK,0.75_SRK/)),'vert 6')
  ASSERT(ALL(testGraph%vertices(:,7) == (/1.0_SRK,1.0_SRK/)) ,'vert 7')
  ASSERT(ALL(testGraph%vertices(:,8) == (/1.25_SRK,0.5_SRK/)),'vert 8')
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,1,0,0,0,0/))
  ASSERT(bool,'edges 1')
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,0,0,0,0,1,0/))
  ASSERT(bool,'edges 2')
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,0,0,0,-1,-1,0,0/))
  ASSERT(bool,'edges 3')
  bool=ALL(testGraph%edgeMatrix(:,4) == (/1,0,0,0,1,0,0,0/))
  ASSERT(bool,'edges 4')
  bool=ALL(testGraph%edgeMatrix(:,5) == (/0,0,-1,1,0,1,0,-1/))
  ASSERT(bool,'edges 5')
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,0,-1,0,1,0,1,-1/))
  ASSERT(bool,'edges 6')
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,1,0,0,0,1,0,0/))
  ASSERT(bool,'edges 7')
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,0,0,0,-1,-1,0,0/))
  ASSERT(bool,'edges 8')

  !Perform the reverse
  COMPONENT_TEST('Circle-Box (1-edge)')
  testGraph=g2
  CALL g2%clear()
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%insertVertex(testCoord(:,3))
  CALL g2%insertVertex(testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3))
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),8,'nvert')
  ASSERT_EQ(testGraph%nEdge(),10,'nedge')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/)) ,'vert 1')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,1.0_SRK/)) ,'vert 2')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.75_SRK,0.5_SRK/)),'vert 3')
  ASSERT(ALL(testGraph%vertices(:,4) == (/1.0_SRK,0.0_SRK/)), 'vert 4')
  ASSERT(ALL(testGraph%vertices(:,5) == (/1.0_SRK,0.25_SRK/)),'vert 5')
  ASSERT(ALL(testGraph%vertices(:,6) == (/1.0_SRK,0.75_SRK/)),'vert 6')
  ASSERT(ALL(testGraph%vertices(:,7) == (/1.0_SRK,1.0_SRK/)) ,'vert 7')
  ASSERT(ALL(testGraph%vertices(:,8) == (/1.25_SRK,0.5_SRK/)),'vert 8')
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,1,0,0,0,0/))
  ASSERT(bool,'edges 1')
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,0,0,0,0,1,0/))
  ASSERT(bool,'edges 2')
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,0,0,0,-1,-1,0,0/))
  ASSERT(bool,'edges 3')
  bool=ALL(testGraph%edgeMatrix(:,4) == (/1,0,0,0,1,0,0,0/))
  ASSERT(bool,'edges 4')
  bool=ALL(testGraph%edgeMatrix(:,5) == (/0,0,-1,1,0,1,0,-1/))
  ASSERT(bool,'edges 5')
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,0,-1,0,1,0,1,-1/))
  ASSERT(bool,'edges 6')
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,1,0,0,0,1,0,0/))
  ASSERT(bool,'edges 7')
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,0,0,0,-1,-1,0,0/))
  ASSERT(bool,'edges 8')
  CALL g2%clear()
  CALL testGraph%clear()

  COMPONENT_TEST('Box-Circle (2-edge)')
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  CALL testGraph%insertVertex(testCoord(:,1))
  CALL testGraph%insertVertex(testCoord(:,2))
  CALL testGraph%insertVertex(testCoord(:,3))
  CALL testGraph%insertVertex(testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
  testCoord(:,1)=(/0.5_SRK,1.0_SRK/)
  testCoord(:,2)=(/1.0_SRK,0.5_SRK/)
  testCoord(:,3)=(/1.0_SRK,1.5_SRK/)
  testCoord(:,4)=(/1.5_SRK,1.0_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%insertVertex(testCoord(:,3))
  CALL g2%insertVertex(testCoord(:,4))
  c=(/1.0_SRK,1.0_SRK/); r=0.5_SRK
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2),c,r)
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,4),c,r)
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,4),c,r)
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3),c,r)
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),8,'nvert')
  ASSERT_EQ(testGraph%nEdge(),10,'nedge')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/)),'vert 1')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,1.0_SRK/)),'vert 2')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.5_SRK,1.0_SRK/)),'vert 3')
  ASSERT(ALL(testGraph%vertices(:,4) == (/1.0_SRK,0.0_SRK/)),'vert 4')
  ASSERT(ALL(testGraph%vertices(:,5) == (/1.0_SRK,0.5_SRK/)),'vert 5')
  ASSERT(ALL(testGraph%vertices(:,6) == (/1.0_SRK,1.0_SRK/)),'vert 6')
  ASSERT(ALL(testGraph%vertices(:,7) == (/1.0_SRK,1.5_SRK/)),'vert 7')
  ASSERT(ALL(testGraph%vertices(:,8) == (/1.5_SRK,1.0_SRK/)),'vert 8')
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,1,0,0,0,0/))
  ASSERT(bool,'edges 1')
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,1,0,0,0,0,0/))
  ASSERT(bool,'edges 2')
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,1,0,0,-1,1,-1,0/))
  ASSERT(bool,'edges 3')
  bool=ALL(testGraph%edgeMatrix(:,4) == (/1,0,0,0,1,0,0,0/))
  ASSERT(bool,'edges 4')
  bool=ALL(testGraph%edgeMatrix(:,5) == (/0,0,-1,1,0,1,0,-1/))
  ASSERT(bool,'edges 5')
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,0,1,0,1,0,0,0/))
  ASSERT(bool,'edges 6')
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,0,-1,0,0,0,0,-1/))
  ASSERT(bool,'edges 7')
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,0,0,0,-1,0,-1,0/))
  ASSERT(bool,'edges 8')

  !Perform Reverse
  COMPONENT_TEST('Circle-Box (2-edge)')
  testGraph=g2
  CALL g2%clear()
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%insertVertex(testCoord(:,3))
  CALL g2%insertVertex(testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3))
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),8,'nvert')
  ASSERT_EQ(testGraph%nEdge(),10,'nedge')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/)),'vert 1')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,1.0_SRK/)),'vert 2')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.5_SRK,1.0_SRK/)),'vert 3')
  ASSERT(ALL(testGraph%vertices(:,4) == (/1.0_SRK,0.0_SRK/)),'vert 4')
  ASSERT(ALL(testGraph%vertices(:,5) == (/1.0_SRK,0.5_SRK/)),'vert 5')
  ASSERT(ALL(testGraph%vertices(:,6) == (/1.0_SRK,1.0_SRK/)),'vert 6')
  ASSERT(ALL(testGraph%vertices(:,7) == (/1.0_SRK,1.5_SRK/)),'vert 7')
  ASSERT(ALL(testGraph%vertices(:,8) == (/1.5_SRK,1.0_SRK/)),'vert 8')
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,1,0,0,0,0/))
  ASSERT(bool,'edges 1')
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,1,0,0,0,0,0/))
  ASSERT(bool,'edges 2')
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,1,0,0,-1,1,-1,0/))
  ASSERT(bool,'edges 3')
  bool=ALL(testGraph%edgeMatrix(:,4) == (/1,0,0,0,1,0,0,0/))
  ASSERT(bool,'edges 4')
  bool=ALL(testGraph%edgeMatrix(:,5) == (/0,0,-1,1,0,1,0,-1/))
  ASSERT(bool,'edges 5')
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,0,1,0,1,0,0,0/))
  ASSERT(bool,'edges 6')
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,0,-1,0,0,0,0,-1/))
  ASSERT(bool,'edges 7')
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,0,0,0,-1,0,-1,0/))
  ASSERT(bool,'edges 8')
  CALL g2%clear()
  CALL testGraph%clear()

  COMPONENT_TEST('Box with inscribed Circle')
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  CALL testGraph%insertVertex(testCoord(:,1))
  CALL testGraph%insertVertex(testCoord(:,2))
  CALL testGraph%insertVertex(testCoord(:,3))
  CALL testGraph%insertVertex(testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
  testCoord(:,1)=(/0.0_SRK,0.5_SRK/)
  testCoord(:,2)=(/0.5_SRK,0.0_SRK/)
  testCoord(:,3)=(/0.5_SRK,1.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,0.5_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%insertVertex(testCoord(:,3))
  CALL g2%insertVertex(testCoord(:,4))
  c=(/0.5_SRK,0.5_SRK/); r=0.5_SRK
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2),c,r)
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,4),c,r)
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,4),c,r)
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3),c,r)
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),8,'nvert')
  ASSERT_EQ(testGraph%nEdge(),12,'nedge')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/)),'vert 1')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,0.5_SRK/)),'vert 2')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.0_SRK,1.0_SRK/)),'vert 3')
  ASSERT(ALL(testGraph%vertices(:,4) == (/0.5_SRK,0.0_SRK/)),'vert 4')
  ASSERT(ALL(testGraph%vertices(:,5) == (/0.5_SRK,1.0_SRK/)),'vert 5')
  ASSERT(ALL(testGraph%vertices(:,6) == (/1.0_SRK,0.0_SRK/)),'vert 6')
  ASSERT(ALL(testGraph%vertices(:,7) == (/1.0_SRK,0.5_SRK/)),'vert 7')
  ASSERT(ALL(testGraph%vertices(:,8) == (/1.0_SRK,1.0_SRK/)),'vert 8')
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,1,0,0,0,0/))
  ASSERT(bool,'edges 1')
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,1,-1,-1,0,0,0/))
  ASSERT(bool,'edges 2')
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,1,0,0,1,0,0,0/))
  ASSERT(bool,'edges 3')
  bool=ALL(testGraph%edgeMatrix(:,4) == (/1,-1,0,0,0,1,-1,0/))
  ASSERT(bool,'edges 4')
  bool=ALL(testGraph%edgeMatrix(:,5) == (/0,-1,1,0,0,0,-1,1/))
  ASSERT(bool,'edges 5')
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,0,0,1,0,0,1,0/))
  ASSERT(bool,'edges 6')
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,0,0,-1,-1,1,0,1/))
  ASSERT(bool,'edges 7')
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,0,0,0,1,0,1,0/))
  ASSERT(bool,'edges 8')

  !Perform reverse
  COMPONENT_TEST('Circle with Bounding Box')
  testGraph=g2
  CALL g2%clear()
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%insertVertex(testCoord(:,3))
  CALL g2%insertVertex(testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3))
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),8,'nvert')
  ASSERT_EQ(testGraph%nEdge(),12,'nedge')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/)),'vert 1')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,0.5_SRK/)),'vert 2')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.0_SRK,1.0_SRK/)),'vert 3')
  ASSERT(ALL(testGraph%vertices(:,4) == (/0.5_SRK,0.0_SRK/)),'vert 4')
  ASSERT(ALL(testGraph%vertices(:,5) == (/0.5_SRK,1.0_SRK/)),'vert 5')
  ASSERT(ALL(testGraph%vertices(:,6) == (/1.0_SRK,0.0_SRK/)),'vert 6')
  ASSERT(ALL(testGraph%vertices(:,7) == (/1.0_SRK,0.5_SRK/)),'vert 7')
  ASSERT(ALL(testGraph%vertices(:,8) == (/1.0_SRK,1.0_SRK/)),'vert 8')
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,1,0,0,0,0/))
  ASSERT(bool,'edges 1')
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,1,-1,-1,0,0,0/))
  ASSERT(bool,'edges 2')
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,1,0,0,1,0,0,0/))
  ASSERT(bool,'edges 3')
  bool=ALL(testGraph%edgeMatrix(:,4) == (/1,-1,0,0,0,1,-1,0/))
  ASSERT(bool,'edges 4')
  bool=ALL(testGraph%edgeMatrix(:,5) == (/0,-1,1,0,0,0,-1,1/))
  ASSERT(bool,'edges 5')
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,0,0,1,0,0,1,0/))
  ASSERT(bool,'edges 6')
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,0,0,-1,-1,1,0,1/))
  ASSERT(bool,'edges 7')
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,0,0,0,1,0,1,0/))
  ASSERT(bool,'edges 8')
  CALL g2%clear()
  CALL testGraph%clear()

  COMPONENT_TEST('Box with half-circle inside')
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  CALL testGraph%insertVertex(testCoord(:,1))
  CALL testGraph%insertVertex(testCoord(:,2))
  CALL testGraph%insertVertex(testCoord(:,3))
  CALL testGraph%insertVertex(testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
  testCoord(:,1)=(/1.0_SRK,0.25_SRK/)
  testCoord(:,2)=(/1.0_SRK,0.75_SRK/)
  testCoord(:,3)=(/1.25_SRK,0.5_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%insertVertex(testCoord(:,3))
  c=(/1.0_SRK,0.5_SRK/); r=0.25_SRK
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2),c,r)
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,3),c,r)
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3),c,r)
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),8,'nvert')
  ASSERT_EQ(testGraph%nEdge(),10,'nedge')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/)) ,'vert 1')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,1.0_SRK/)) ,'vert 2')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.75_SRK,0.5_SRK/)),'vert 3')
  ASSERT(ALL(testGraph%vertices(:,4) == (/1.0_SRK,0.0_SRK/)), 'vert 4')
  ASSERT(ALL(testGraph%vertices(:,5) == (/1.0_SRK,0.25_SRK/)),'vert 5')
  ASSERT(ALL(testGraph%vertices(:,6) == (/1.0_SRK,0.75_SRK/)),'vert 6')
  ASSERT(ALL(testGraph%vertices(:,7) == (/1.0_SRK,1.0_SRK/)) ,'vert 7')
  ASSERT(ALL(testGraph%vertices(:,8) == (/1.25_SRK,0.5_SRK/)),'vert 8')
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,1,0,0,0,0/))
  ASSERT(bool,'edges 1')
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,0,0,0,0,1,0/))
  ASSERT(bool,'edges 2')
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,0,0,0,-1,-1,0,0/))
  ASSERT(bool,'edges 3')
  bool=ALL(testGraph%edgeMatrix(:,4) == (/1,0,0,0,1,0,0,0/))
  ASSERT(bool,'edges 4')
  bool=ALL(testGraph%edgeMatrix(:,5) == (/0,0,-1,1,0,1,0,-1/))
  ASSERT(bool,'edges 5')
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,0,-1,0,1,0,1,-1/))
  ASSERT(bool,'edges 6')
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,1,0,0,0,1,0,0/))
  ASSERT(bool,'edges 7')
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,0,0,0,-1,-1,0,0/))
  ASSERT(bool,'edges 8')

  !Perform Reverse
  COMPONENT_TEST('half-circle inside with Box')
  testGraph=g2
  CALL g2%clear()
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%insertVertex(testCoord(:,3))
  CALL g2%insertVertex(testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3))
  CALL testGraph%combineGraph(g2)
  ASSERT_EQ(testGraph%nVert(),8,'nvert')
  ASSERT_EQ(testGraph%nEdge(),10,'nedge')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/)) ,'vert 1')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,1.0_SRK/)) ,'vert 2')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.75_SRK,0.5_SRK/)),'vert 3')
  ASSERT(ALL(testGraph%vertices(:,4) == (/1.0_SRK,0.0_SRK/)), 'vert 4')
  ASSERT(ALL(testGraph%vertices(:,5) == (/1.0_SRK,0.25_SRK/)),'vert 5')
  ASSERT(ALL(testGraph%vertices(:,6) == (/1.0_SRK,0.75_SRK/)),'vert 6')
  ASSERT(ALL(testGraph%vertices(:,7) == (/1.0_SRK,1.0_SRK/)) ,'vert 7')
  ASSERT(ALL(testGraph%vertices(:,8) == (/1.25_SRK,0.5_SRK/)),'vert 8')
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,1,0,0,0,0/))
  ASSERT(bool,'edges 1')
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,0,0,0,0,1,0/))
  ASSERT(bool,'edges 2')
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,0,0,0,-1,-1,0,0/))
  ASSERT(bool,'edges 3')
  bool=ALL(testGraph%edgeMatrix(:,4) == (/1,0,0,0,1,0,0,0/))
  ASSERT(bool,'edges 4')
  bool=ALL(testGraph%edgeMatrix(:,5) == (/0,0,-1,1,0,1,0,-1/))
  ASSERT(bool,'edges 5')
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,0,-1,0,1,0,1,-1/))
  ASSERT(bool,'edges 6')
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,1,0,0,0,1,0,0/))
  ASSERT(bool,'edges 7')
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,0,0,0,-1,-1,0,0/))
  ASSERT(bool,'edges 8')
  CALL g2%clear()
  CALL testGraph%clear()

  COMPONENT_TEST('Box with half-circle outside')
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  CALL testGraph%insertVertex(testCoord(:,1))
  CALL testGraph%insertVertex(testCoord(:,2))
  CALL testGraph%insertVertex(testCoord(:,3))
  CALL testGraph%insertVertex(testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL testGraph%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
  testCoord(:,1)=(/0.75_SRK,0.5_SRK/)
  testCoord(:,2)=(/1.0_SRK,0.25_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.75_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%insertVertex(testCoord(:,3))
  c=(/1.0_SRK,0.5_SRK/); r=0.25_SRK
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2),c,-r)
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,3),c,-r)
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3),c,-r)
  CALL testGraph%combineGraph(g2)
  CALL testGraph%editToVTK('box-circle.vtk')
  ASSERT_EQ(testGraph%nVert(),8,'nvert')
  ASSERT_EQ(testGraph%nEdge(),10,'nedge')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/)) ,'vert 1')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,1.0_SRK/)) ,'vert 2')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.75_SRK,0.5_SRK/)),'vert 3')
  ASSERT(ALL(testGraph%vertices(:,4) == (/1.0_SRK,0.0_SRK/)), 'vert 4')
  ASSERT(ALL(testGraph%vertices(:,5) == (/1.0_SRK,0.25_SRK/)),'vert 5')
  ASSERT(ALL(testGraph%vertices(:,6) == (/1.0_SRK,0.75_SRK/)),'vert 6')
  ASSERT(ALL(testGraph%vertices(:,7) == (/1.0_SRK,1.0_SRK/)) ,'vert 7')
  ASSERT(ALL(testGraph%vertices(:,8) == (/1.25_SRK,0.5_SRK/)),'vert 8')
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,1,0,0,0,0/))
  ASSERT(bool,'edges 1')
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,0,0,0,0,1,0/))
  ASSERT(bool,'edges 2')
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,0,0,0,-1,-1,0,0/))
  ASSERT(bool,'edges 3')
  bool=ALL(testGraph%edgeMatrix(:,4) == (/1,0,0,0,1,0,0,0/))
  ASSERT(bool,'edges 4')
  bool=ALL(testGraph%edgeMatrix(:,5) == (/0,0,-1,1,0,1,0,-1/))
  ASSERT(bool,'edges 5')
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,0,-1,0,1,0,1,-1/))
  ASSERT(bool,'edges 6')
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,1,0,0,0,1,0,0/))
  ASSERT(bool,'edges 7')
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,0,0,0,-1,-1,0,0/))
  ASSERT(bool,'edges 8')

  !Perform Reverse
  COMPONENT_TEST('half-circle outside with Box')
  testGraph=g2
  CALL g2%clear()
  testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
  testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
  testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
  testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
  CALL g2%insertVertex(testCoord(:,1))
  CALL g2%insertVertex(testCoord(:,2))
  CALL g2%insertVertex(testCoord(:,3))
  CALL g2%insertVertex(testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3))
  CALL testGraph%combineGraph(g2)
  CALL testGraph%editToVTK('circle-box.vtk')
  ASSERT_EQ(testGraph%nVert(),8,'nvert')
  ASSERT_EQ(testGraph%nEdge(),10,'nedge')
  ASSERT(ALL(testGraph%vertices(:,1) == (/0.0_SRK,0.0_SRK/)) ,'vert 1')
  ASSERT(ALL(testGraph%vertices(:,2) == (/0.0_SRK,1.0_SRK/)) ,'vert 2')
  ASSERT(ALL(testGraph%vertices(:,3) == (/0.75_SRK,0.5_SRK/)),'vert 3')
  ASSERT(ALL(testGraph%vertices(:,4) == (/1.0_SRK,0.0_SRK/)), 'vert 4')
  ASSERT(ALL(testGraph%vertices(:,5) == (/1.0_SRK,0.25_SRK/)),'vert 5')
  ASSERT(ALL(testGraph%vertices(:,6) == (/1.0_SRK,0.75_SRK/)),'vert 6')
  ASSERT(ALL(testGraph%vertices(:,7) == (/1.0_SRK,1.0_SRK/)) ,'vert 7')
  ASSERT(ALL(testGraph%vertices(:,8) == (/1.25_SRK,0.5_SRK/)),'vert 8')
  bool=ALL(testGraph%edgeMatrix(:,1) == (/0,1,0,1,0,0,0,0/))
  ASSERT(bool,'edges 1')
  bool=ALL(testGraph%edgeMatrix(:,2) == (/1,0,0,0,0,0,1,0/))
  ASSERT(bool,'edges 2')
  bool=ALL(testGraph%edgeMatrix(:,3) == (/0,0,0,0,-1,-1,0,0/))
  ASSERT(bool,'edges 3')
  bool=ALL(testGraph%edgeMatrix(:,4) == (/1,0,0,0,1,0,0,0/))
  ASSERT(bool,'edges 4')
  bool=ALL(testGraph%edgeMatrix(:,5) == (/0,0,-1,1,0,1,0,-1/))
  ASSERT(bool,'edges 5')
  bool=ALL(testGraph%edgeMatrix(:,6) == (/0,0,-1,0,1,0,1,-1/))
  ASSERT(bool,'edges 6')
  bool=ALL(testGraph%edgeMatrix(:,7) == (/0,1,0,0,0,1,0,0/))
  ASSERT(bool,'edges 7')
  bool=ALL(testGraph%edgeMatrix(:,8) == (/0,0,0,0,-1,-1,0,0/))
  ASSERT(bool,'edges 8')
  CALL g2%clear()
  CALL testGraph%clear()
!Test to cover a spacer grid bug for BWRs
  COMPONENT_TEST('BWR Spacer Grid')
  testCoord(:,1)=[-0.65000000000000002_SRK,0.63260303149822616_SRK]
  CALL g2%insertVertex(testCoord(:,1))
  testCoord(:,1)=[-0.65000000000000002_SRK,0.64999999999999991_SRK]
  CALL g2%insertVertex(testCoord(:,1))
  testCoord(:,1)=[0.14182486630031421_SRK,0.63260303149822616_SRK]
  CALL g2%insertVertex(testCoord(:,1))
  testCoord(:,1)=[0.14182486630031421_SRK,0.64999999999999991_SRK]
  CALL g2%insertVertex(testCoord(:,1))
  g2%edgeMatrix=RESHAPE(SOURCE=[0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0],SHAPE=[4,4])
  g2%quadEdges=0.0_SRK

  testCoord(:,1)=[-0.65000000000000002_SRK,-0.65000000000000002_SRK]
  CALL testGraph%insertVertex(testCoord(:,1))
  testCoord(:,1)=[-0.65000000000000002_SRK,0.65000000000000002_SRK]
  CALL testGraph%insertVertex(testCoord(:,1))
  testCoord(:,1)=[-0.11519999999999886_SRK,-0.11519999999999886_SRK]
  CALL testGraph%insertVertex(testCoord(:,1))
  testCoord(:,1)=[-0.11519999999999882_SRK,0.64999999999999991_SRK]
  CALL testGraph%insertVertex(testCoord(:,1))
  testCoord(:,1)=[0.14182486630031421_SRK,0.14182486630031421_SRK]
  CALL testGraph%insertVertex(testCoord(:,1))
  testCoord(:,1)=[0.14182486630031424_SRK,0.63260303149822628_SRK]
  CALL testGraph%insertVertex(testCoord(:,1))
  testCoord(:,1)=[0.14182486630031424_SRK,0.64999999999999991_SRK]
  CALL testGraph%insertVertex(testCoord(:,1))
  testCoord(:,1)=[0.47308564490390465_SRK,0.65000000000000002_SRK]
  CALL testGraph%insertVertex(testCoord(:,1))
  testCoord(:,1)=[0.63260303149822628_SRk,0.14182486630031421_SRK]
  CALL testGraph%insertVertex(testCoord(:,1))
  testCoord(:,1)=[0.65000000000000002_SRK,-0.65000000000000002_SRK]
  CALL testGraph%insertVertex(testCoord(:,1))
  testCoord(:,1)=[0.64999999999999991_SRK,-0.11519999999999886_SRK]
  CALL testGraph%insertVertex(testCoord(:,1))
  testCoord(:,1)=[0.64999999999999991_SRK,0.14182486630031421_SRK]
  CALL testGraph%insertVertex(testCoord(:,1))
  testCoord(:,1)=[0.65000000000000002_SRK,0.47308564490390459_SRK]
  CALL testGraph%insertVertex(testCoord(:,1))
  testCoord(:,1)=[0.65000000000000002_SRK,0.65000000000000002_SRk]
  CALL testGraph%insertVertex(testCoord(:,1))
  testGraph%edgeMatrix(:, 1)=[0,1,0,0,0,0,0,0,0,1,0,0,0,0]
  testGraph%edgeMatrix(:, 2)=[1,0,0,1,0,0,0,0,0,0,0,0,0,0]
  testGraph%edgeMatrix(:, 3)=[0,0,0,1,0,0,0,0,0,0,1,0,0,0]
  testGraph%edgeMatrix(:, 4)=[0,1,1,0,0,0,1,0,0,0,0,0,0,0]
  testGraph%edgeMatrix(:, 5)=[0,0,0,0,0,1,0,0,1,0,0,0,0,0]
  testGraph%edgeMatrix(:, 6)=[0,0,0,0,1,0,1,0,-1,0,0,0,0,0]
  testGraph%edgeMatrix(:, 7)=[0,0,0,1,0,1,0,1,0,0,0,-1,0,0]
  testGraph%edgeMatrix(:, 8)=[0,0,0,0,0,0,1,0,0,0,0,0,-1,1]
  testGraph%edgeMatrix(:, 9)=[0,0,0,0,1,-1,0,0,0,0,0,1,0,0]
  testGraph%edgeMatrix(:,10)=[1,0,0,0,0,0,0,0,0,0,1,0,0,0]
  testGraph%edgeMatrix(:,11)=[0,0,1,0,0,0,0,0,0,1,0,1,0,0]
  testGraph%edgeMatrix(:,12)=[0,0,0,0,0,0,-1,0,1,0,1,0,1,0]
  testGraph%edgeMatrix(:,13)=[0,0,0,0,0,0,0,-1,0,0,0,1,0,1]
  testGraph%edgeMatrix(:,14)=[0,0,0,0,0,0,0,1,0,0,0,0,1,0]
  testGraph%quadEdges=0.0_SRK
  testGraph%quadEdges(:,9,6)=[0.14182486630031421_SRK,0.14182486630031421_SRK,0.49077816519791206_SRK]
  testGraph%quadEdges(:,12,7)=[0.14182486630031421_SRK,0.14182486630031421_SRK,0.50817513369968581_SRK]
  testGraph%quadEdges(:,13,8)=[-0.11519999999999886_SRK,-0.11519999999999886_SRK,0.96519999999999995_SRK]
  testGraph%quadEdges(:,6,9)=[0.14182486630031421_SRK,0.14182486630031421_SRK,0.49077816519791206_SRK]
  testGraph%quadEdges(:,7,12)=[0.14182486630031421_SRK,0.14182486630031421_SRK,0.50817513369968581_SRK]
  testGraph%quadEdges(:,8,13)=[-0.11519999999999886_SRK,-0.11519999999999886_SRK,0.96519999999999995_SRK]

  CALL g2%editToVTK('bwr-grid-1.vtk')
  CALL testGraph%editToVTK('bwr-grid-2.vtk')
  CALL testGraph%combineGraph(g2)
  CALL testGraph%editToVTK('bwr-grid-combined.vtk')

  ASSERT_EQ(testGraph%nVert(),16,'%nVert()')
  ASSERT_APPROXEQ(testGraph%vertices(1,2),-0.65_SRK,'Vertex 2, x')
  ASSERT_APPROXEQ(testGraph%vertices(2,2),0.63260303149822628_SRK,'Vertex 2, y')
  ASSERT_APPROXEQ(testGraph%vertices(1,5),-0.11519999999999882_SRK,'Vertex 5, x')
  ASSERT_APPROXEQ(testGraph%vertices(2,5),0.63260303149822628_SRK,'Vertex 5, y')
  ASSERT(.NOT.ANY(testGraph%vertices(1:2,:) .APPROXEQ. 0.48885743497004952_SRK),'Bad point added!')
! DO i=1,testGraph%nVert()
! WRITE(*,*) testGraph%edgeMatrix(:,i)
! ENDDO
  CALL g2%clear()
  CALL testGraph%clear()

ENDSUBROUTINE testCombine
!
!-------------------------------------------------------------------------------
SUBROUTINE testTriangulate
  LOGICAL(SBK) :: bool
  INTEGER(SIK) :: i,j,n
  REAL(SRK) :: testCoord(2,9)
  TYPE(GraphType),ALLOCATABLE :: cycles(:)

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

  CALL testGraph%clear()
  DO i=1,9
    CALL testGraph%insertVertex(testCoord(:,i))
  ENDDO
  CALL testGraph%TriangulateVerts()
  CALL testGraph%editToVTK('Triangulate.vtk')
  n=0
  DO i=1,9
    DO j=i+1,9
      IF(testGraph%edgeMatrix(i,j) == 1) n=n+1
    ENDDO
  ENDDO
  bool=(n == 19)
  ASSERT(bool,'Wrong number of edges')
  CALL testGraph%getMCB(cycles)
  bool=(SIZE(cycles) == 11)
  ASSERT(bool,'Wrong number of triangles')
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
SUBROUTINE testAssign()
  INTEGER(SIK) :: i
  REAL(SRK) :: testCoord(2,9),c0(2),r
  TYPE(GraphType) :: g2

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
    CALL g2%insertVertex(testCoord(:,i))
  ENDDO
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,3),c0,r)
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,5))
  CALL g2%defineEdge(testCoord(:,5),testCoord(:,6))
  CALL g2%defineEdge(testCoord(:,5),testCoord(:,7))
  CALL g2%defineEdge(testCoord(:,5),testCoord(:,8))
  CALL g2%defineEdge(testCoord(:,6),testCoord(:,7))
  CALL g2%defineEdge(testCoord(:,8),testCoord(:,9))
  COMPONENT_TEST('Empty')
  testGraph=g2
  ASSERT_EQ(testGraph%nVert(),g2%nVert(),'nVert')
  ASSERT(ALL(testGraph%vertices == g2%vertices),'vertices')
  ASSERT(ALL(testGraph%edgeMatrix == g2%edgeMatrix),'edgeMatrix')
  ASSERT(ALL(testGraph%quadEdges == g2%quadEdges),'quadEdges')
  CALL g2%clear()

  COMPONENT_TEST('Overwrite')
  DO i=1,3
    CALL g2%insertVertex(testCoord(:,i))
  ENDDO
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3))
  CALL g2%defineEdge(testCoord(:,2),testCoord(:,3))
  testGraph=g2
  ASSERT(ALL(testGraph%vertices == g2%vertices),'vertices')
  ASSERT(ALL(testGraph%edgeMatrix == g2%edgeMatrix),'edgeMatrix')
  ASSERT(ALL(testGraph%quadEdges == g2%quadEdges),'quadEdges')
  CALL testGraph%clear()
  CALL g2%clear()
ENDSUBROUTINE testAssign
!
!-------------------------------------------------------------------------------
SUBROUTINE testIsEqual()
  INTEGER(SIK) :: i
  REAL(SRK) :: testCoord(2,9)
  TYPE(GraphType) :: g2

  CALL testGraph%clear()
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
  DO i=1,7
    CALL g2%insertVertex(testCoord(:,i))
  ENDDO
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,2))
  CALL g2%defineEdge(testCoord(:,1),testCoord(:,3))
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,4))
  CALL g2%defineEdge(testCoord(:,4),testCoord(:,2))
  CALL g2%defineEdge(testCoord(:,4),testCoord(:,6))
  CALL g2%defineEdge(testCoord(:,3),testCoord(:,5))
  CALL g2%defineEdge(testCoord(:,5),testCoord(:,6))
  CALL g2%defineEdge(testCoord(:,6),testCoord(:,7))
  ASSERT(testGraph == g2,'Equal Graphs')
  g2%vertices(1,1)=-1.0_SRK
  ASSERT(.NOT.(testGraph == g2),'Non-Equal Graphs')

  CALL testGraph%clear()
  CALL g2%clear()
ENDSUBROUTINE testIsEqual
!
!-------------------------------------------------------------------------------
SUBROUTINE symEdgeCheck()
  LOGICAL(SBK) :: bool
  INTEGER(SIK) :: i,j,n
  n=testGraph%nVert()
  DO i=1,n
    ASSERT_EQ(testGraph%edgeMatrix(i,i),0,'E(i,i)')
    bool=ALL(testGraph%quadEdges(:,i,i) == 0.0_SRK)
    ASSERT(bool,'QE(i,i)')
    DO j=i+1,n
      ASSERT_EQ(testGraph%edgeMatrix(i,j),testGraph%edgeMatrix(j,i),'E symmetry')
      bool=ALL(testGraph%quadEdges(:,i,j) == testGraph%quadEdges(:,j,i))
      ASSERT(bool,'QE symmetry')
    ENDDO
  ENDDO
ENDSUBROUTINE symEdgeCheck
!!
!!-------------------------------------------------------------------------------
!    SUBROUTINE testAddition()
!      LOGICAL(SBK) :: bool
!      INTEGER(SIK) :: i
!      REAL(SRK) :: testCoord(2,9),c0(2),r
!      TYPE(GraphType) :: testGraph2,testGraph3,testGraph4
!
!      !Test graph1
!      testCoord(:,1)=(/0.0_SRK,0.0_SRK/)
!      testCoord(:,2)=(/0.0_SRK,1.0_SRK/)
!      testCoord(:,3)=(/1.0_SRK,0.0_SRK/)
!      testCoord(:,4)=(/1.0_SRK,1.0_SRK/)
!      testCoord(:,5)=(/2.0_SRK,0.0_SRK/)
!      testCoord(:,6)=(/2.0_SRK,1.0_SRK/)
!      testCoord(:,7)=(/2.0_SRK,2.0_SRK/)
!      DO i=1,4
!        CALL testGraph%insertVertex(testCoord(:,i))
!      ENDDO
!      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,2))
!      CALL testGraph%defineEdge(testCoord(:,1),testCoord(:,3))
!      CALL testGraph%defineEdge(testCoord(:,3),testCoord(:,4))
!      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,2))
!      !test graph2
!      CALL testGraph2%insertVertex(testCoord(:,1))
!      CALL testGraph2%insertVertex(testCoord(:,5))
!      CALL testGraph2%insertVertex(testCoord(:,6))
!      CALL testGraph2%insertVertex(testCoord(:,7))
!      CALL testGraph2%defineEdge(testCoord(:,7),testCoord(:,8))
!      CALL testGraph2%defineEdge(testCoord(:,5),testCoord(:,6))
!      CALL testGraph2%defineEdge(testCoord(:,6),testCoord(:,7))
!      !Test graph4
!      DO i=1,4
!        CALL testGraph4%insertVertex(testCoord(:,i))
!      ENDDO
!      CALL testGraph4%insertVertex(testCoord(:,5))
!      CALL testGraph4%insertVertex(testCoord(:,6))
!      CALL testGraph4%insertVertex(testCoord(:,7))
!      CALL testGraph4%defineEdge(testCoord(:,1),testCoord(:,2))
!      CALL testGraph4%defineEdge(testCoord(:,1),testCoord(:,3))
!      CALL testGraph4%defineEdge(testCoord(:,3),testCoord(:,4))
!      CALL testGraph4%defineEdge(testCoord(:,4),testCoord(:,2))
!      CALL testGraph4%defineEdge(testCoord(:,7),testCoord(:,1))
!      CALL testGraph4%defineEdge(testCoord(:,5),testCoord(:,6))
!      CALL testGraph4%defineEdge(testCoord(:,6),testCoord(:,7))
!
!      testGraph3=testGraph+testGraph2
!      ASSERT(testGraph3 == testGraph4,'addition')
!    ENDSUBROUTINE testAddition
!
!-------------------------------------------------------------------------------
SUBROUTINE testDAGUninit()
  ASSERT_EQ(testDAGraph%n,0,'%n')
  ASSERT(.NOT.ALLOCATED(testDAGraph%nodes),'%nodes')
  ASSERT(.NOT.ALLOCATED(testDAGraph%edgeMatrix),'%edgeMatrix')
ENDSUBROUTINE testDAGUninit
!
!-------------------------------------------------------------------------------
SUBROUTINE testDAGClear()
  ALLOCATE(testDAGraph%nodes(1))
  ALLOCATE(testDAGraph%edgeMatrix(1,1))
  testDAGraph%n=1
  CALL testDAGraph%clear()
  CALL testDAGUninit()
ENDSUBROUTINE testDAGClear
!
!-------------------------------------------------------------------------------
SUBROUTINE testDAGInit()
  INTEGER(SIK),ALLOCATABLE :: nodes(:)

  CALL testDAGraph%init(0,nodes)
  CALL testDAGUninit()

  CALL testDAGraph%init(5,nodes)
  CALL testDAGUninit()

  ALLOCATE(nodes(5))
  nodes(1)=2; nodes(2)=3; nodes(3)=4; nodes(4)=5; nodes(5)=6

  CALL testDAGraph%init(4,nodes)
  CALL testDAGUninit()

  CALL testDAGraph%init(5,nodes)

  ASSERT_EQ(testDAGraph%n,5,'%n')
  ASSERT(ALL(testDAGraph%nodes == (/2,3,4,5,6/)),'%nodes')
  ASSERT(ALL(testDAGraph%edgeMatrix == 0),'%edgeMatrix')
  DEALLOCATE(nodes)
ENDSUBROUTINE testDAGInit
!
!-------------------------------------------------------------------------------
SUBROUTINE testDAGDefineEdge()
  INTEGER(SIK),ALLOCATABLE :: edges(:,:)

  ALLOCATE(edges(5,5))
  edges=0

  CALL testDAGraph%defineEdge(0,0)
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%null edge')

  CALL testDAGraph%defineEdge(2,3)
  edges(1,2)=1
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edge(1,2)')

  CALL testDAGraph%defineEdge(5,3)
  edges(4,2)=1
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edge(4,2)')

  CALL testDAGraph%defineEdge(2,4)
  edges(1,3)=1
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edge(1,3)')

  CALL testDAGraph%defineEdge(2,5)
  edges(1,4)=1
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edge(1,4)')

  CALL testDAGraph%defineEdge(3,4)
  edges(2,3)=1
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edge(2,3)')

  CALL testDAGraph%defineEdge(5,6)
  edges(4,5)=1
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edge(4,5)')

  DEALLOCATE(edges)
ENDSUBROUTINE testDAGDefineEdge
!
!-------------------------------------------------------------------------------
SUBROUTINE testDAGRemoveEdge()
  INTEGER(SIK),ALLOCATABLE :: edges(:,:)

  ALLOCATE(edges(5,5))
  edges=0
  edges(1,2)=1
  edges(4,2)=1
  edges(1,3)=1
  edges(1,4)=1
  edges(2,3)=1
  edges(4,5)=1

  CALL testDAGraph%removeEdge(2,3)
  edges(1,2)=0
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edge(1,2)')

  CALL testDAGraph%removeEdge(5,3)
  edges(4,2)=0
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edge(4,2)')

  CALL testDAGraph%removeEdge(2,4)
  edges(1,3)=0
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edge(1,3)')

  !Redundant call
  CALL testDAGraph%removeEdge(2,4)
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edge(1,3)')

  CALL testDAGraph%defineEdge(2,3)
  CALL testDAGraph%defineEdge(5,3)
  CALL testDAGraph%defineEdge(2,4)

  DEALLOCATE(edges)
ENDSUBROUTINE testDAGRemoveEdge
!
!-------------------------------------------------------------------------------
SUBROUTINE testDAGinsertNode()
  INTEGER(SIK),ALLOCATABLE :: edges(:,:)

  ALLOCATE(edges(6,6))
  edges=0
  edges(1,3)=1
  edges(1,4)=1
  edges(1,5)=1
  edges(3,4)=1
  edges(5,3)=1
  edges(5,6)=1
  CALL testDAGraph%insertNode(7,2)
  ASSERT_EQ(testDAGraph%n,6,'%n')
  ASSERT(ALL(testDAGraph%nodes == (/2,7,3,4,5,6/)),'%nodes')
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edgeMatrix')
  DEALLOCATE(edges)

  ALLOCATE(edges(7,7))
  edges=0
  edges(2,4)=1
  edges(2,5)=1
  edges(2,6)=1
  edges(4,5)=1
  edges(6,4)=1
  edges(6,7)=1
  CALL testDAGraph%insertNode(8)
  ASSERT_EQ(testDAGraph%n,7,'%n')
  ASSERT(ALL(testDAGraph%nodes == (/8,2,7,3,4,5,6/)),'%nodes')
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edgeMatrix')
  DEALLOCATE(edges)
ENDSUBROUTINE testDAGinsertNode
!
!-------------------------------------------------------------------------------
SUBROUTINE testDAGremoveNode()
  INTEGER(SIK),ALLOCATABLE :: edges(:,:)

  ALLOCATE(edges(6,6))
  edges=0
  edges(2,3)=1
  edges(2,4)=1
  edges(2,5)=1
  edges(3,4)=1
  edges(5,3)=1
  edges(5,6)=1
  CALL testDAGraph%removeNode(ID=7)
  ASSERT_EQ(testDAGraph%n,6,'%n')
  ASSERT(ALL(testDAGraph%nodes == (/8,2,3,4,5,6/)),'%nodes')
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edgeMatrix')
  DEALLOCATE(edges)

  ALLOCATE(edges(5,5))
  edges=0
  edges(1,2)=1
  edges(1,3)=1
  edges(1,4)=1
  edges(2,3)=1
  edges(4,2)=1
  edges(4,5)=1
  CALL testDAGraph%removeNode(IND=1)
  ASSERT_EQ(testDAGraph%n,5,'%n')
  ASSERT(ALL(testDAGraph%nodes == (/2,3,4,5,6/)),'%nodes')
  ASSERT(ALL(testDAGraph%edgeMatrix == edges),'%edgeMatrix')
  DEALLOCATE(edges)
ENDSUBROUTINE testDAGremoveNode
!
!-------------------------------------------------------------------------------
SUBROUTINE testDAGisStartNode()

  ASSERT(testDAGraph%isStartNode(ID=2),'%isStartNode(ID=2)')
  ASSERT(.NOT.testDAGraph%isStartNode(ID=3),'%isStartNode(ID=3)')
  ASSERT(.NOT.testDAGraph%isStartNode(ID=4),'%isStartNode(ID=4)')
  ASSERT(.NOT.testDAGraph%isStartNode(ID=5),'%isStartNode(ID=5)')
  ASSERT(.NOT.testDAGraph%isStartNode(ID=6),'%isStartNode(ID=6)')

  ASSERT(testDAGraph%isStartNode(IND=1),'%isStartNode(ID=1)')
  ASSERT(.NOT.testDAGraph%isStartNode(IND=2),'%isStartNode(ID=2)')
  ASSERT(.NOT.testDAGraph%isStartNode(IND=3),'%isStartNode(ID=3)')
  ASSERT(.NOT.testDAGraph%isStartNode(IND=4),'%isStartNode(ID=4)')
  ASSERT(.NOT.testDAGraph%isStartNode(IND=5),'%isStartNode(ID=5)')
ENDSUBROUTINE testDAGisStartNode
!
!-------------------------------------------------------------------------------
SUBROUTINE testDAGgetNextStartNode()
  INTEGER(SIK) :: ID
  INTEGER(SIK),ALLOCATABLE :: old(:)

  ALLOCATE(old(5))
  old=0
  CALL testDAGraph%getNextStartNode(old,ID)
  ASSERT_EQ(ID,2,'Start Node is 2')
  ASSERT(ALL(old == (/2,0,0,0,0/)),'old list')

  CALL testDAGraph%getNextStartNode(old,ID)
  ASSERT_EQ(ID,0,'No other start node')
  ASSERT(ALL(old == (/2,0,0,0,0/)),'old list')

ENDSUBROUTINE testDAGgetNextStartNode
!
!-------------------------------------------------------------------------------
SUBROUTINE testDAGKATS()
  INTEGER(SIK),ALLOCATABLE :: nodes(:)

  ALLOCATE(nodes(8))
  nodes=(/10,9,2,8,11,3,5,7/)
  CALL testDAGraph%clear()
  CALL testDAGraph%init(8,nodes)
  CALL testDAGraph%defineEdge(3,8)
  CALL testDAGraph%defineEdge(3,10)
  CALL testDAGraph%defineEdge(5,11)
  CALL testDAGraph%defineEdge(7,11)
  CALL testDAGraph%defineEdge(7,8)
  CALL testDAGraph%defineEdge(11,2)
  CALL testDAGraph%defineEdge(11,9)
  CALL testDAGraph%defineEdge(11,10)
  CALL testDAGraph%defineEdge(8,9)

  CALL testDAGraph%KATS()
  ASSERT(ALL(testDAGraph%nodes == (/2,9,10,11,8,7,5,3/)),'%sorted')
  DEALLOCATE(nodes)

ENDSUBROUTINE testDAGKATS
!
ENDPROGRAM testGeom_Graph
