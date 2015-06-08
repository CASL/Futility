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
  
  TYPE(GraphType) :: testGraph
  
  CREATE_TEST('TEST GEOM_GRAPH')
  
  REGISTER_SUBTEST('Uninit',testUninit)
  REGISTER_SUBTEST('%nVert',testNVert)
  REGISTER_SUBTEST('%nEdge',testNEdge)
  REGISTER_SUBTEST('%clear',testClear)
  REGISTER_SUBTEST('%insertVertex',testInsertVertex)
  REGISTER_SUBTEST('%defineEdge',testDefineEdge)
  REGISTER_SUBTEST('%defineQuadEdge',testDefineQuadEdge)
  REGISTER_SUBTEST('%getMCB',testGetMCB)

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
      CALL dmallocA(testGraph%quadEdges,1,1)
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
    SUBROUTINE testDefineEdge()
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,j,n
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
      n=testGraph%nVert()
      CALL testGraph%defineEdge(testCoord(:,4),testCoord(:,1))
      ASSERT(testGraph%edgeMatrix(1,2) == 1,'edge 1')
      DO i=1,n
        ASSERT(testGraph%edgeMatrix(i,i) == 0,'diagonal')
        DO j=i+1,n
          ASSERT(testGraph%edgeMatrix(i,j) == testGraph%edgeMatrix(j,i),'symmetry')
        ENDDO
      ENDDO
      
      !Test Resize
      COMPONENT_TEST('Post Insertion')
      CALL testGraph%insertVertex(testCoord(:,2))
      ASSERT(testGraph%edgeMatrix(1,3) == 1,'edge 1')
      DO i=1,n
        ASSERT(testGraph%edgeMatrix(i,i) == 0,'diagonal')
        DO j=i+1,n
          ASSERT(testGraph%edgeMatrix(i,j) == testGraph%edgeMatrix(j,i),'symmetry')
        ENDDO
      ENDDO
    ENDSUBROUTINE testDefineEdge
!
!-------------------------------------------------------------------------------
    SUBROUTINE testDefineQuadEdge()
      ASSERT(.FALSE.,'TEST NOT IMPLEMENTED!')
    ENDSUBROUTINE testDefineQuadEdge
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetMCB()
      ASSERT(.FALSE.,'TEST NOT IMPLEMENTED!')
    ENDSUBROUTINE testGetMCB
!
ENDPROGRAM testGeom_Graph
