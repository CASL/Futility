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

  IMPLICIT NONE

  TYPE(ExceptionHandlerType),POINTER,SAVE :: e
  TYPE(VTKMeshType),SAVE :: testVTKMesh
  TYPE(VTKDataType),SAVE :: testVTKData
  TYPE(VTUXMLFileType),SAVE :: testVTUFile
  TYPE(PVTUXMLFileType),SAVE :: testPVTUFile
  LOGICAL(SBK) :: bool

  CREATE_TEST('Test VTU Files')

  CALL SetupError

  REGISTER_SUBTEST('INITIALIZE',testInitialize)
  REGISTER_SUBTEST('STRUCTURED POINTS',testFailSTRUCTURED_POINTS)
  REGISTER_SUBTEST('UNSTRUCTURED GRID',testUNSTRUCTURED_GRID)
  REGISTER_SUBTEST('MULTIFILE DATASET',testMultiVTU)

  CALL DellocateAll

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE SetupError
      ALLOCATE(e)
      CALL e%setStopOnError(.FALSE.)
      CALL e%setQuietMode(.TRUE.)
      CALL testVTUFile%e%addSurrogate(e)
      CALL eVTK%addSurrogate(e)
    ENDSUBROUTINE SetupError
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInitialize
      !Test Initialize and clear
      CALL testVTUFile%clear
      CALL testVTUFile%initialize(666,'testVTU0.vtu')
      bool = testVTUFile%isInit() .AND. testVTUFile%isOpen()
      ASSERT(bool,'testVTUFile%initialize(...)')
      CALL testVTUFile%clear(.TRUE.)
    ENDSUBROUTINE testInitialize
!
!-------------------------------------------------------------------------------
    SUBROUTINE testFailSTRUCTURED_POINTS
      !Test writeMesh for STRUCTURED_POINTS (should fail, not supported)
      CALL testVTUFile%e%addSurrogate(e)
      CALL testVTUFile%initialize(666,'testVTU1.vtu')
      CALL SetupTest1_Mesh
      CALL testVTUFile%writeMesh(testVTKMesh) ! Try to write STRUCTURED_POINTS
      bool = .NOT.testVTUFile%hasMesh
      ASSERT(bool, 'testVTUFile%writeMesh(...) STRUCTURED_POINTS')
      CALL testVTUFile%clear(.TRUE.)
    ENDSUBROUTINE testFailSTRUCTURED_POINTS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testUNSTRUCTURED_GRID
      !Test writeMesh for UNSTRUCTURED_GRID
      CALL testVTUFile%initialize(666,'testVTU2.vtu')
      CALL SetupTest2_Mesh
      CALL testVTUFile%writeMesh(testVTKMesh)
      bool = testVTUFile%hasMesh
      ASSERT(bool, 'testVTUFile%writeMesh(...) UNSTRUCTURED_GRID')

      !Write data on the mesh
      CALL SetupTest2_Data
      CALL testVTUFile%writeScalarData(testVTKData)
      testVTKData%varname='mat_val'
      testVTKData%vtkDataFormat='float'
      CALL testVTUFile%writeScalarData(testVTKData)

      !Test writeScalarData
      testVTKData%vtkDataFormat='long'
      testVTKData%varname='mat_long'
      CALL testVTUFile%writeScalarData(testVTKData) !repeat for long
      testVTKData%vtkDataFormat='short'
      testVTKData%varname='mat_short'
      CALL testVTUFile%writeScalarData(testVTKData) !repeat for short
      testVTKData%vtkDataFormat='int'
      testVTKData%varname='mat_float'
      CALL testVTUFile%writeScalarData(testVTKData) !write as int
      testVTKData%vtkDataFormat='double'
      testVTKData%varname='mat_double'
      CALL testVTUFile%writeScalarData(testVTKData) !write as double
      CALL testVTUFile%close
      CALL testVTUFile%clear
    ENDSUBROUTINE testUNSTRUCTURED_GRID
!
!-------------------------------------------------------------------------------
    SUBROUTINE testMultiVTU
      !Test writeMesh for UNSTRUCTURED_GRID
      CALL testPVTUFile%clear
      CALL testPVTUFile%initialize(666,'testPVTU_0.vtu')
      CALL SetupTest2_Mesh
      CALL testPVTUFile%writeMesh(testVTKMesh)
      bool = testPVTUFile%hasMesh
      ASSERT(bool,'testPVTUFile%writeMesh(...) UNSTRUCTURED_GRID 0')
      !Write data on the mesh
      CALL SetupTest2_Data
      CALL testPVTUFile%writeScalarData(testVTKData)
      CALL testPVTUFile%close
      CALL testPVTUFile%clear

      !Write to second file
      CALL testPVTUFile%initialize(666,'testPVTU_1.vtu')
      !Translate mesh
      CALL testVTKMesh%translate(-4.0_SRK,-5.0_SRK,3.0_SRK)
      CALL testPVTUFile%writeMesh(testVTKMesh)
      bool = testPVTUFile%hasMesh
      ASSERT(bool,'testPVTUFile%writeMesh(...) UNSTRUCTURED_GRID 1')
      !Write data on the mesh
      CALL testPVTUFile%writeScalarData(testVTKData)
      CALL testPVTUFile%close
      CALL testPVTUFile%clear

      CALL testPVTUFile%writepvtu(666,'testPVTU',2)
    ENDSUBROUTINE testMultiVTU
!
!-------------------------------------------------------------------------------
    SUBROUTINE SetupTest1_Mesh
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
    SUBROUTINE SetupTest2_Mesh
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
      
      testVTKMesh%cellList=12
      j=0
      DO i=1,800
        testVTKMesh%nodeList(i)=i-1
      ENDDO
      testVTKMesh%isInit=.TRUE.
    ENDSUBROUTINE SetupTest2_Mesh
!
!-------------------------------------------------------------------------------
    SUBROUTINE SetupTest1_Data
      INTEGER(SIK) :: i
      
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
    SUBROUTINE SetupTest2_Data
      INTEGER(SIK) :: i
      
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
    SUBROUTINE DellocateAll
      DEALLOCATE(e)
      CALL testVTKMesh%clear
      CALL testVTKData%clear
    ENDSUBROUTINE DellocateAll
!
ENDPROGRAM testVTUFiles