!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testVTUFiles
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE ExceptionHandler
  USE VTKFiles
  USE VTUFiles
  !
  IMPLICIT NONE
  !
  TYPE(ExceptionHandlerType),POINTER,SAVE :: e
  TYPE(VTKMeshType),SAVE :: testVTKMesh
  TYPE(VTKDataType),SAVE :: testVTKData
  TYPE(VTUXMLFileType),SAVE :: testVTUFile
  LOGICAL(SBK) :: bool
  !
  CREATE_TEST('Test VTU Files')
  !
  CALL SetupError()
  !
  REGISTER_SUBTEST('CLEAR',testClear)
  REGISTER_SUBTEST('INITIALIZE',testInitialize)
  REGISTER_SUBTEST('STRUCTURED POINTS',testFailSTRUCTURED_POINTS)
  REGISTER_SUBTEST('UNSTRUCTURED GRID',testUNSTRUCTURED_GRID)
  REGISTER_SUBTEST('MULTIFILE DATASET',testMultiVTU)
  !
  CALL DellocateAll()
  !
  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE SetupError()
      ALLOCATE(e)
      CALL e%setStopOnError(.FALSE.)
      CALL e%setQuietMode(.TRUE.)
      CALL testVTUFile%e%addSurrogate(e)
      CALL eVTK%addSurrogate(e)
    ENDSUBROUTINE SetupError
!
!-------------------------------------------------------------------------------
    SUBROUTINE testClear()
      !Test clear
      CALL testVTUFile%fopen()
      testVTUFile%hasMesh=.TRUE.
      !
      CALL testVTUFile%clear()
      ASSERT(.NOT.testVTUFile%isInit(),'testVTUFile%isInit()')
      ASSERT(.NOT.testVTUFile%isOpen(),'testVTUFile%isOpen()')
      ASSERT(.NOT.testVTUFile%hasMesh,'testVTUFile%hasMesh')
    ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInitialize()
      !Test initialize
      CALL testVTUFile%initialize(666,'testVTU0.vtu')
      ASSERT(testVTUFile%isInit(),'testVTUFile%isInit()')
      ASSERT(testVTUFile%isOpen(),'testVTUFile%isOpen()')
      ASSERT(.NOT.testVTUFile%hasMesh,'testVTUFile%hasMesh')
      ASSERT(testVTUFile%numDataSet==0,'testVTUFile%numDataSet')
      CALL testVTUFile%clear(.TRUE.)
    ENDSUBROUTINE testInitialize
!
!-------------------------------------------------------------------------------
    SUBROUTINE testFailSTRUCTURED_POINTS()
      !Test writeMesh for STRUCTURED_POINTS (should fail, not supported)
      CALL testVTUFile%e%addSurrogate(e)
      CALL testVTUFile%initialize(666,'testVTU1.vtu')
      CALL SetupTest1_Mesh()
      CALL testVTUFile%writeMesh(testVTKMesh) ! Try to write STRUCTURED_POINTS
      bool=.NOT.testVTUFile%hasMesh
      ASSERT(bool,'testVTUFile%writeMesh(...) STRUCTURED_POINTS')
      CALL testVTUFile%clear(.TRUE.)
    ENDSUBROUTINE testFailSTRUCTURED_POINTS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testUNSTRUCTURED_GRID()
      !Test writeMesh for UNSTRUCTURED_GRID
      CALL testVTUFile%initialize(666,'testVTU2.vtu')
      CALL SetupTest2_Mesh()
      CALL testVTUFile%writeMesh(testVTKMesh)
      bool=testVTUFile%hasMesh
      ASSERT(bool,'testVTUFile%writeMesh(...) UNSTRUCTURED_GRID')
      !
      !Write data on the mesh
      CALL SetupTest2_Data()
      CALL testVTUFile%writeScalarData(testVTKData)
      !
      !Test writeScalarData
      COMPONENT_TEST('write float')
      testVTKData%vtkDataFormat='float'
      testVTKData%varname='mat_float'
      CALL testVTUFile%writeScalarData(testVTKData)
      CALL testVTUFile%hasData('mat_float','float',bool)
      ASSERT(bool,'testVTUFile%hasData(...) FLOAT')
      !
      COMPONENT_TEST('write long')
      testVTKData%vtkDataFormat='long'
      testVTKData%varname='mat_long'
      CALL testVTUFile%writeScalarData(testVTKData) !repeat for long
      CALL testVTUFile%hasData('mat_long','long',bool)
      ASSERT(bool,'testVTUFile%hasData(...) LONG')
      !
      COMPONENT_TEST('write short')
      testVTKData%vtkDataFormat='short'
      testVTKData%varname='mat_short'
      CALL testVTUFile%writeScalarData(testVTKData) !repeat for short
      CALL testVTUFile%hasData('mat_short','short',bool)
      ASSERT(bool,'testVTUFile%hasData(...) SHORT')
      !
      COMPONENT_TEST('write int')
      testVTKData%vtkDataFormat='int'
      testVTKData%varname='mat_int'
      CALL testVTUFile%writeScalarData(testVTKData) !write as int
      CALL testVTUFile%hasData('mat_int','int',bool)
      ASSERT(bool,'testVTUFile%hasData(...) INT')
      !
      COMPONENT_TEST('write double')
      testVTKData%vtkDataFormat='double'
      testVTKData%varname='mat_double'
      CALL testVTUFile%writeScalarData(testVTKData) !write as double
      CALL testVTUFile%hasData('mat_double','double',bool)
      ASSERT(bool,'testVTUFile%hasData(...) DOUBLE')
      !
      CALL testVTUFile%close()
      CALL testVTUFile%clear()
    ENDSUBROUTINE testUNSTRUCTURED_GRID
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMultiVTU()
      !Test writeMesh for UNSTRUCTURED_GRID
      CALL testVTUFile%initialize(666,'testPVTU_0.vtu')
      CALL SetupTest2_Mesh()
      CALL testVTUFile%writeMesh(testVTKMesh)
      bool=testVTUFile%hasMesh
      ASSERT(bool,'testVTUFile%hasMesh')
      INQUIRE(FILE='testPVTU_0.vtu',EXIST=bool)
      ASSERT(bool,'FILE WRITTEN')
      !
      !Write data on the mesh
      CALL SetupTest2_Data()
      CALL testVTUFile%writeScalarData(testVTKData)
      ASSERT(testVTUFile%numDataSet==1,'testVTUFile%numDataSet==1')
      CALL testVTUFile%close()
      CALL testVTUFile%clear()
      !
      !Write to second file
      CALL testVTUFile%initialize(666,'testPVTU_1.vtu')
      !Translate mesh
      CALL testVTKMesh%translate(-4.0_SRK,-5.0_SRK,3.0_SRK)
      CALL testVTUFile%writeMesh(testVTKMesh)
      bool=testVTUFile%hasMesh
      ASSERT(bool,'testVTUFile%hasMesh')
      INQUIRE(FILE='testPVTU_1.vtu',EXIST=bool)
      ASSERT(bool,'FILE WRITTEN')
      !Write data on the mesh
      CALL testVTUFile%writeScalarData(testVTKData)
      ASSERT(testVTUFile%numDataSet==1,'testVTUFile%numDataSet==1')
      CALL testVTUFile%close()
      CALL testVTUFile%clear()
      !
      !Check that data is present
      CALL testVTUFile%hasData('material','int',bool)
      ASSERT(bool,'testVTUFile%hasData(...)')
      !
      !Write pvtu file
      CALL testVTUFile%writepvtu(666,'testPVTU',2,0)
      CALL testVTUFile%hasFile('testPVTU_0.vtu',bool)
      ASSERT(bool,'testVTUFile%hasFile(...) testPVTU_0.vtu')
      CALL testVTUFile%hasFile('testPVTU_1.vtu',bool)
      ASSERT(bool,'testVTUFile%hasFile(...) testPVTU_1.vtu')
      !
      !Ensure that data is now deallocated
      CALL testVTUFile%hasData('material','int',bool)
      ASSERT(.NOT.bool,'testVTUFile%hasData(...)')
      ASSERT(testVTUFile%numDataSet==0,'testVTUFile%numDataSet==0')
    ENDSUBROUTINE testMultiVTU
!
!-------------------------------------------------------------------------------
    SUBROUTINE SetupTest1_Mesh()
      !Initialize a VTK mesh by hand
      testVTKMesh%dims=(/33,33,5/)
      testVTKMesh%meshType=VTK_STRUCTURED_POINTS
      testVTKMesh%numCells=(testVTKMesh%dims(1)-1)*(testVTKMesh%dims(2)-1)* &
        (testVTKMesh%dims(3)-1)
      ALLOCATE(testVTKMesh%x(2))
      ALLOCATE(testVTKMesh%y(2))
      ALLOCATE(testVTKMesh%z(2))
      testVTKMesh%x(1)=0.0_SRK
      testVTKMesh%x(2)=0.25_SRK
      testVTKMesh%y(1)=0.0_SRK
      testVTKMesh%y(2)=0.25_SRK
      testVTKMesh%z(1)=0.0_SRK
      testVTKMesh%z(2)=2.0_SRK
      testVTKMesh%isInit=.TRUE.
    ENDSUBROUTINE SetupTest1_Mesh
!
!-------------------------------------------------------------------------------
    SUBROUTINE SetupTest2_Mesh()
      INTEGER(SIK) :: i,j
      !Initialize a VTK mesh by hand
      testVTKMesh%numPoints=800
      testVTKMesh%dims=testVTKMesh%numPoints
      testVTKMesh%meshType=VTK_UNSTRUCTURED_GRID
      testVTKMesh%numCells=100
      IF (ALLOCATED(testVTKMesh%x)) DEALLOCATE(testVTKMesh%x)
      IF (ALLOCATED(testVTKMesh%y)) DEALLOCATE(testVTKMesh%y)
      IF (ALLOCATED(testVTKMesh%z)) DEALLOCATE(testVTKMesh%z)
      IF (ALLOCATED(testVTKMesh%cellList)) DEALLOCATE(testVTKMesh%cellList)
      IF (ALLOCATED(testVTKMesh%nodeList)) DEALLOCATE(testVTKMesh%nodeList)
      ALLOCATE(testVTKMesh%x(testVTKMesh%numPoints))
      ALLOCATE(testVTKMesh%y(testVTKMesh%numPoints))
      ALLOCATE(testVTKMesh%z(testVTKMesh%numPoints))
      ALLOCATE(testVTKMesh%cellList(testVTKMesh%numCells))
      ALLOCATE(testVTKMesh%nodeList(800))
      OPEN(unit=555,file='mesh2Points.txt',FORM='FORMATTED', &
        ACCESS='SEQUENTIAL',STATUS='OLD',ACTION='READ')
      DO i=1,800
        READ(555,*) testVTKMesh%x(i),testVTKMesh%y(i),testVTKMesh%z(i)
      ENDDO
      CLOSE(555)
      !
      testVTKMesh%cellList=12
      j=0
      DO i=1,800
        testVTKMesh%nodeList(i)=i-1
      ENDDO
      testVTKMesh%isInit=.TRUE.
    ENDSUBROUTINE SetupTest2_Mesh
!
!-------------------------------------------------------------------------------
    SUBROUTINE SetupTest1_Data()
      INTEGER(SIK) :: i
      !
      !Initialize VTK data by hand
      testVTKData%varname='VTK_cellIndex_int'
      testVTKData%vtkDataFormat='int'
      testVTKData%dataSetType=VTK_DATA_SCALARS
      testVTKData%isCellData=.TRUE.
      testVTKData%isInit=.TRUE.
      IF(.NOT.ALLOCATED(testVTKData%dataList)) &
        ALLOCATE(testVTKData%dataList(testVTKMesh%numCells))
      DO i=1,testVTKMesh%numCells
        testVTKData%dataList(i)=REAL(i,SRK)
      ENDDO
    ENDSUBROUTINE SetupTest1_Data
!
!-------------------------------------------------------------------------------
    SUBROUTINE SetupTest2_Data()
      INTEGER(SIK) :: i
      !
      !Initialize VTK data by hand
      testVTKData%varname='material'
      testVTKData%vtkDataFormat='int'
      testVTKData%dataSetType=VTK_DATA_SCALARS
      testVTKData%isCellData=.TRUE.
      testVTKData%isInit=.TRUE.
      IF(ALLOCATED(testVTKData%dataList)) DEALLOCATE(testVTKData%dataList)
      ALLOCATE(testVTKData%dataList(testVTKMesh%numCells))
      OPEN(unit=555,file='mesh2Data.txt',FORM='FORMATTED', &
        ACCESS='SEQUENTIAL',STATUS='OLD',ACTION='READ')
      DO i=1,testVTKMesh%numCells
        READ(555,*) testVTKData%dataList(i)
      ENDDO
      CLOSE(555)
    ENDSUBROUTINE SetupTest2_Data
!
!-------------------------------------------------------------------------------
    SUBROUTINE DellocateAll()
      DEALLOCATE(e)
      CALL testVTUFile%clear()
      CALL testVTKMesh%clear()
      CALL testVTKData%clear()
    ENDSUBROUTINE DellocateAll
!
ENDPROGRAM testVTUFiles