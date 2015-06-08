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
  REGISTER_SUBTEST('%getVertIndex',testGetVertIndex)
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
      ASSERT(testGraph%nEdge() == 1,'nEdge')
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
      ASSERT(testGraph%nEdge() == 1,'nEdge')
      DO i=1,n
        ASSERT(testGraph%edgeMatrix(i,i) == 0,'diagonal')
        DO j=i+1,n
          ASSERT(testGraph%edgeMatrix(i,j) == testGraph%edgeMatrix(j,i),'symmetry')
        ENDDO
      ENDDO

      CALL testGraph%clear()
    ENDSUBROUTINE testDefineEdge
!
!-------------------------------------------------------------------------------
    SUBROUTINE testDefineQuadEdge()
      REAL(SRK),PARAMETER :: r=1.0_SRK,c0(2)=0.0_SRK  
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,j,n
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
      n=testGraph%nVert()
      DO i=1,n
        ASSERT(testGraph%edgeMatrix(i,i) == 0,'e diagonal')
        bool=ALL(testGraph%quadEdges(:,i,i) .APPROXEQ. 0.0_SRK)
        ASSERT(bool,'qe diagonal')
        DO j=i+1,n
          ASSERT(testGraph%edgeMatrix(i,j) == testGraph%edgeMatrix(j,i),'e symmetry')
          bool=ALL(testGraph%quadEdges(:,i,j) .APPROXEQ. testGraph%quadEdges(:,j,i))
          ASSERT(bool,'qe symmetry')
        ENDDO
      ENDDO
      
      COMPONENT_TEST('Post vertex append')
      CALL testGraph%insertVertex(testCoord(:,3))
      ASSERT(testGraph%nEdge() == 1,'nEdge')
      ASSERT(testGraph%edgeMatrix(1,2) == -1,'edge 1')
      bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 1')
      n=testGraph%nVert()
      DO i=1,n
        ASSERT(testGraph%edgeMatrix(i,i) == 0,'e diagonal')
        bool=ALL(testGraph%quadEdges(:,i,i) .APPROXEQ. 0.0_SRK)
        ASSERT(bool,'qe diagonal')
        DO j=i+1,n
          ASSERT(testGraph%edgeMatrix(i,j) == testGraph%edgeMatrix(j,i),'e symmetry')
          bool=ALL(testGraph%quadEdges(:,i,j) .APPROXEQ. testGraph%quadEdges(:,j,i))
          ASSERT(bool,'qe symmetry')
        ENDDO
      ENDDO
      CALL testGraph%defineQuadraticEdge(testCoord(:,2),testCoord(:,3),c0,r)
      ASSERT(testGraph%nEdge() == 2,'nEdge')
      ASSERT(testGraph%edgeMatrix(1,2) == -1,'edge 1')
      bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 1')
      ASSERT(testGraph%edgeMatrix(2,3) == -1,'edge 2')
      bool=ALL(testGraph%quadEdges(:,2,3) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 2')
      n=testGraph%nVert()
      DO i=1,n
        ASSERT(testGraph%edgeMatrix(i,i) == 0,'e diagonal')
        bool=ALL(testGraph%quadEdges(:,i,i) .APPROXEQ. 0.0_SRK)
        ASSERT(bool,'qe diagonal')
        DO j=i+1,n
          ASSERT(testGraph%edgeMatrix(i,j) == testGraph%edgeMatrix(j,i),'e symmetry')
          bool=ALL(testGraph%quadEdges(:,i,j) .APPROXEQ. testGraph%quadEdges(:,j,i))
          ASSERT(bool,'qe symmetry')
        ENDDO
      ENDDO

      COMPONENT_TEST('Post vertex insertion')
      CALL testGraph%insertVertex(testCoord(:,4))
      ASSERT(testGraph%nEdge() == 2,'nEdge')
      ASSERT(testGraph%edgeMatrix(1,2) == -1,'edge 1')
      bool=ALL(testGraph%quadEdges(:,1,2) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 1')
      ASSERT(testGraph%edgeMatrix(2,4) == -1,'edge 2')
      bool=ALL(testGraph%quadEdges(:,2,4) == (/c0(1),c0(2),r/))
      ASSERT(bool,'quadEdge 2')
      n=testGraph%nVert()
      DO i=1,n
        ASSERT(testGraph%edgeMatrix(i,i) == 0,'e diagonal')
        bool=ALL(testGraph%quadEdges(:,i,i) .APPROXEQ. 0.0_SRK)
        ASSERT(bool,'qe diagonal')
        DO j=i+1,n
          ASSERT(testGraph%edgeMatrix(i,j) == testGraph%edgeMatrix(j,i),'e symmetry')
          bool=ALL(testGraph%quadEdges(:,i,j) .APPROXEQ. testGraph%quadEdges(:,j,i))
          ASSERT(bool,'qe symmetry')
        ENDDO
      ENDDO
      
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
      n=testGraph%nVert()
      DO i=1,n
        ASSERT(testGraph%edgeMatrix(i,i) == 0,'e diagonal')
        bool=ALL(testGraph%quadEdges(:,i,i) .APPROXEQ. 0.0_SRK)
        ASSERT(bool,'qe diagonal')
        DO j=i+1,n
          ASSERT(testGraph%edgeMatrix(i,j) == testGraph%edgeMatrix(j,i),'e symmetry')
          bool=ALL(testGraph%quadEdges(:,i,j) .APPROXEQ. testGraph%quadEdges(:,j,i))
          ASSERT(bool,'qe symmetry')
        ENDDO
      ENDDO

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
      n=testGraph%nVert()
      DO i=1,n
        ASSERT(testGraph%edgeMatrix(i,i) == 0,'e diagonal')
        bool=ALL(testGraph%quadEdges(:,i,i) .APPROXEQ. 0.0_SRK)
        ASSERT(bool,'qe diagonal')
        DO j=i+1,n
          ASSERT(testGraph%edgeMatrix(i,j) == testGraph%edgeMatrix(j,i),'e symmetry')
          bool=ALL(testGraph%quadEdges(:,i,j) .APPROXEQ. testGraph%quadEdges(:,j,i))
          ASSERT(bool,'qe symmetry')
        ENDDO
      ENDDO

      CALL testGraph%clear()
    ENDSUBROUTINE testDefineQuadEdge
!
!-------------------------------------------------------------------------------
    SUBROUTINE testGetMCB()
      ASSERT(.FALSE.,'TEST NOT IMPLEMENTED!')
    ENDSUBROUTINE testGetMCB
!
ENDPROGRAM testGeom_Graph
