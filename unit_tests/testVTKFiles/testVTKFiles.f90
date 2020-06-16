!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testVTKFiles
#include "UnitTest.h"
USE UnitTest
USE IntrType
USE ExceptionHandler
USE VTKFiles

IMPLICIT NONE

CHARACTER(LEN=257) :: longstring
TYPE(ExceptionHandlerType),POINTER,SAVE :: e
TYPE(VTKMeshType),SAVE :: testVTKMesh, testVTKMesh2
TYPE(VTKDataType),SAVE :: testVTKData
TYPE(VTKLegFileType),SAVE :: testVTKFile
REAL(SRK),ALLOCATABLE :: xref(:),yref(:),zref(:),cellRef(:),nodeRef(:)
LOGICAL(SBK),ALLOCATABLE :: testMask(:)
LOGICAL(SBK) :: bool
INTEGER(SIK) :: k,npartfail

CREATE_TEST('Test VTK Files')

ALLOCATE(e)
CALL e%setStopOnError(.FALSE.)
CALL e%setQuietMode(.TRUE.)

!Check clear
REGISTER_SUBTEST('Clear',testClear)
REGISTER_SUBTEST('Init',testInit)
REGISTER_SUBTEST('Write',testWrite)
REGISTER_SUBTEST('removeRedundantPoints',testRemoveRedundantPoints)
REGISTER_SUBTEST('meshConversion',testMeshConversion)
REGISTER_SUBTEST('meshTranslation',testMeshTranslation)
REGISTER_SUBTEST('meshAddition',testMeshAddition)
REGISTER_SUBTEST('cellRemoval',testCellRemoval)

!
DEALLOCATE(e)
CALL testVTKMesh%clear()
CALL testVTKMesh2%clear()
CALL testVTKData%clear()
CALL testVTKFile%clear()

FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testClear()

  CALL testVTKFile%clear()

ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
SUBROUTINE testInit()

  CALL testVTKFile%e%addSurrogate(e)
  CALL eVTK%addSurrogate(e)

  !Test Initialize and clear
  CALL testVTKFile%initialize(666,'testVTK0.vtk')
  bool = testVTKFile%isInit() .AND. testVTKFile%isOpen()
  ASSERT(bool, 'testVTKFile%initialize(...)')

  !Error check
  CALL testVTKFile%initialize(666,'testVTK1.vtk',STATUS='testVTK1')

  CALL testVTKFile%clear(.TRUE.)
  CALL testVTKFile%e%addSurrogate(e)
  bool = .NOT.testVTKFile%isInit()
  ASSERT(bool, 'testVTKFile%clear() 0')

  CALL testVTKFile%clear()
  CALL testVTKFile%e%addSurrogate(e)
  longstring='testVTK0.vtk'
  longstring(257:257)='r'
  CALL testVTKFile%initialize(666,'testVTK1.vtk',STATUS=longstring)
ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
SUBROUTINE testWrite()

  COMPONENT_TEST('writeMesh, STRUCTURED_POINTS')
  CALL testVTKFile%writeMesh(testVTKMesh) !write with uninitialized file
  CALL testVTKFile%initialize(666,'testVTK1.vtk',STATUS='testVTK1.vtk')
  CALL testVTKFile%writeMesh(testVTKMesh) !write with unitialized mesh
  CALL testVTKFile%fclose()
  CALL testVTKFile%writeMesh(testVTKMesh) !write with closed file
  CALL testVTKFile%clear()
  CALL testVTKFile%e%addSurrogate(e)
  CALL testVTKFile%initialize(666,'testVTK1.vtk',STATUS='testVTK1')
  CALL SetupTest1_Mesh()
  CALL testVTKFile%writeMesh(testVTKMesh)
  bool = testVTKFile%hasMesh
  ASSERT(bool, 'testVTKFile%writeMesh(...) STRUCTURED_POINTS')

  COMPONENT_TEST('writeScalarData')
  CALL SetupTest1_Data()
  CALL testVTKFile%writeScalarData(testVTKData) !Integer data
  testVTKData%vtkDataFormat='long'
  testVTKData%varname='VTK_cellIndex_long'
  CALL testVTKFile%writeScalarData(testVTKData) !repeat for long
  testVTKData%vtkDataFormat='short'
  testVTKData%varname='VTK_cellIndex_short'
  CALL testVTKFile%writeScalarData(testVTKData) !repeat for short
  testVTKData%vtkDataFormat='float'
  testVTKData%varname='VTK_cellIndex_float'
  CALL testVTKFile%writeScalarData(testVTKData) !write as float
  testVTKData%vtkDataFormat='double'
  testVTKData%varname='VTK_cellIndex_double'
  CALL testVTKFile%writeScalarData(testVTKData) !write as double

  COMPONENT_TEST('Error Checks')
  testVTKData%vtkDataFormat='char'
  CALL testVTKFile%writeScalarData(testVTKData) !bad data format
  testVTKData%dataSetType=VTK_DATA_TENSORS
  CALL testVTKFile%writeScalarData(testVTKData) !bad type of data set
  testVTKData%isInit=.FALSE.
  CALL testVTKFile%writeScalarData(testVTKData) !no data
  testVTKData%isInit=.TRUE.
  testVTKData%isCellData=.FALSE.
  CALL testVTKFile%writeScalarData(testVTKData) !non-cell data
  testVTKFile%hasMesh=.FALSE.
  CALL testVTKFile%writeScalarData(testVTKData) !no mesh yet
  testVTKFile%hasMesh=.TRUE.
  CALL testVTKFile%fclose()
  CALL testVTKFile%writeScalarData(testVTKData) !closed file

  COMPONENT_TEST('STRUCTURED_GRID')
  CALL testVTKFile%clear()
  CALL testVTKFile%initialize(666,'testVTK2.vtk',STATUS='testVTK2')
  CALL SetupTest2_Mesh()
  CALL testVTKFile%writeMesh(testVTKMesh)
  bool = testVTKFile%hasMesh
  ASSERT(bool, 'testVTKFile%writeMesh(...) STRUCTURED_GRID')
  !Write data on the mesh
  CALL SetupTest1_Data()
  CALL testVTKFile%writeScalarData(testVTKData)

  COMPONENT_TEST('RECTILINEAR_GRID')
  CALL testVTKFile%clear()
  CALL testVTKFile%initialize(666,'testVTK3.vtk',STATUS='testVTK3')
  CALL SetupTest2_Mesh()
  testVTKMesh%meshType=VTK_RECTILINEAR_GRID
  CALL testVTKFile%writeMesh(testVTKMesh)
  bool = testVTKFile%hasMesh
  ASSERT(bool, 'testVTKFile%writeMesh(...) RECTILINEAR_GRID')
  !Write data on the mesh
  testVTKData%vtkDataFormat='double'
  CALL testVTKFile%writeScalarData(testVTKData)

  COMPONENT_TEST('UNSTRUCTURED_GRID')
  CALL testVTKFile%clear()
  CALL testVTKFile%initialize(666,'testVTK4.vtk',STATUS='testVTK4')
  CALL SetupTest4_Mesh()
  CALL testVTKFile%writeMesh(testVTKMesh)
  bool = testVTKFile%hasMesh
  ASSERT(bool, 'testVTKFile%writeMesh(...) UNSTRUCTURED_GRID')
  !Write data on the mesh
  CALL SetupTest4_Data()
  CALL testVTKFile%writeScalarData(testVTKData)
  testVTKData%varname='mat_val'
  testVTKData%vtkDataFormat='float'
  CALL testVTKFile%writeScalarData(testVTKData)

ENDSUBROUTINE testWrite
!
!-------------------------------------------------------------------------------
SUBROUTINE testRemoveRedundantPoints()

  CALL testVTKFile%clear()
  CALL testVTKFile%initialize(666,'testVTK5pre.vtk',STATUS='testVTK5pre')
  CALL SetupTest5_Mesh()

  !Mesh written before point cleanup, for comparison
  CALL testVTKFile%writeMesh(testVTKMesh)
  CALL testVTKMesh%cleanupPoints
  ALLOCATE(xref(180))
  ALLOCATE(yref(180))
  ALLOCATE(zref(180))
  OPEN(unit=555,file='mesh5ref.txt',FORM='FORMATTED', &
      ACCESS='SEQUENTIAL',STATUS='OLD',ACTION='READ')
  DO k=1,180
    READ(555,*) xref(k),yref(k),zref(k)
  ENDDO
  CLOSE(555)
  CALL testVTKFile%clear()
  CALL testVTKFile%initialize(666,'testVTK5post.vtk',STATUS='testVTK5post')
  CALL testVTKFile%writeMesh(testVTKMesh)
  npartfail=0
  IF(ANY(.NOT.(testVTKMesh%x .APPROXEQ. xref)) .OR. &
      ANY(.NOT.(testVTKMesh%y .APPROXEQ. yref)) .OR. &
      ANY(.NOT.(testVTKMesh%z .APPROXEQ. zref))) THEN

    bool = ALL((testVTKMesh%x .APPROXEQA. xref)) .AND. &
           ALL((testVTKMesh%y .APPROXEQA. yref)) .AND. &
           ALL((testVTKMesh%z .APPROXEQA. zref))
    ASSERT(bool, 'testVTKMesh%cleanupPoints(...) UNSTRUCTURED_GRID')
    npartfail=npartfail+1
  ENDIF
  IF(npartfail > 0) THEN
    WRITE(*,*) 'WARNING: Partial failures n=',npartfail, &
        ' for "CALL testVTKMesh%cleanupPoints(...) UNSTRUCTURED_GRID"'
  ENDIF

  CALL testVTKFile%clear()
  DEALLOCATE(xref)
  DEALLOCATE(yref)
  DEALLOCATE(zref)
  CALL testVTKFile%initialize(666,'testVTK6pre.vtk',STATUS='testVTK6pre')
  CALL SetupTest6_Mesh()
  CALL testVTKFile%writeMesh(testVTKMesh)
  CALL testVTKMesh%cleanupPoints
  CALL testVTKFile%clear()
  CALL testVTKFile%initialize(666,'testVTK6post.vtk',STATUS='testVTK6post')
  CALL testVTKFile%writeMesh(testVTKMesh)
  ALLOCATE(xref(10))
  ALLOCATE(yref(10))
  ALLOCATE(zref(10))
  xref=(/ 0.0,0.0,0.0,0.0,0.5,0.5,1.0,1.0,1.0,1.0 /)
  yref=(/ 0.0,0.0,1.0,1.0,0.5,0.5,0.0,0.0,1.0,1.0 /)
  zref=(/ 0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0 /)
  bool = ALL((testVTKMesh%x .APPROXEQ. xref)) .AND. &
         ALL((testVTKMesh%y .APPROXEQ. yref)) .AND. &
         ALL((testVTKMesh%z .APPROXEQ. zref))
  ASSERT(bool, 'testVTKMesh%cleanupPoints(...) UNSTRUCTURED_GRID')

ENDSUBROUTINE testRemoveRedundantPoints
!
!-------------------------------------------------------------------------------
SUBROUTINE testMeshConversion()

  COMPONENT_TEST('Unstructured to unstructured')
  CALL testVTKFile%clear()
  DEALLOCATE(testVTKMesh%x)
  DEALLOCATE(testVTKMesh%y)
  DEALLOCATE(testVTKMesh%z)
  DEALLOCATE(testVTKMesh%cellList)
  DEALLOCATE(testVTKMesh%nodeList)
  CALL testVTKFile%initialize(666,'testVTK7.vtk',STATUS='testVTK7')
  CALL SetupTest1_Mesh()
  CALL testVTKMesh%convert(VTK_UNSTRUCTURED_GRID)
  CALL testVTKFile%writeMesh(testVTKMesh)
  bool = testVTKMesh%numPoints == 5445
  ASSERT(bool, 'testVTKMesh%convert(...) STRUCTURED_POINTS')

  COMPONENT_TEST('Rectilinear to unstructured')
  CALL testVTKFile%clear()
  DEALLOCATE(testVTKMesh%cellList)
  DEALLOCATE(testVTKMesh%nodeList)
  CALL testVTKFile%initialize(666,'testVTK9.vtk',STATUS='testVTK9')
  CALL SetupTest2_Mesh()
  testVTKMesh%meshType=VTK_RECTILINEAR_GRID
  CALL testVTKMesh%convert(VTK_UNSTRUCTURED_GRID)
  CALL testVTKFile%writeMesh(testVTKMesh)
  bool = testVTKMesh%numPoints == 5445
  ASSERT(bool, 'testVTKMesh%convert(...) RECTILINEAR_GRID')

ENDSUBROUTINE testMeshConversion
!
!-------------------------------------------------------------------------------
SUBROUTINE testMeshTranslation()

  COMPONENT_TEST('VTK_STRUCTURED_POINTS')
  CALL testVTKFile%clear()
  DEALLOCATE(testVTKMesh%x)
  DEALLOCATE(testVTKMesh%y)
  DEALLOCATE(testVTKMesh%z)
  DEALLOCATE(xref)
  DEALLOCATE(yref)
  DEALLOCATE(zref)
  DEALLOCATE(testVTKMesh%cellList)
  DEALLOCATE(testVTKMesh%nodeList)
  ALLOCATE(xref(2))
  ALLOCATE(yref(2))
  ALLOCATE(zref(2))
  testVTKMesh%meshType=-1
  CALL testVTKMesh%translate(1.0_SRK,1.0_SRK,3.0_SRK) !Error check
  CALL SetupTest1_Mesh()
  xref=testVTKMesh%x
  yref=testVTKMesh%y
  zref=testVTKMesh%z
  CALL testVTKMesh%translate(1.0_SRK,2.0_SRK,3.0_SRK)
  bool = ((testVTKMesh%x(1) .APPROXEQ. xref(1)+1.0_SRK) .OR. &
         (testVTKMesh%x(2) .APPROXEQ. xref(2))) .AND. &
         ((testVTKMesh%y(1) .APPROXEQ. yref(1)+2.0_SRK) .OR. &
         (testVTKMesh%y(2) .APPROXEQ. yref(2))) .AND. &
         ((testVTKMesh%z(1) .APPROXEQ. zref(1)+3.0_SRK) .OR. &
         (testVTKMesh%z(2) .APPROXEQ. zref(2)))
  ASSERT(bool, 'testVTKMesh%translate(...) STRUCTURED_POINTS')

  COMPONENT_TEST('VTK_STRUCTURED_GRID')
  DEALLOCATE(xref)
  DEALLOCATE(yref)
  DEALLOCATE(zref)
  CALL testVTKFile%clear()
  CALL testVTKFile%initialize(666,'testVTK11.vtk',STATUS='testVTK11')
  CALL SetupTest2_Mesh()
  ALLOCATE(xref(size(testVTKMesh%x)))
  ALLOCATE(yref(size(testVTKMesh%y)))
  ALLOCATE(zref(size(testVTKMesh%z)))
  xref=testVTKMesh%x
  yref=testVTKMesh%y
  zref=testVTKMesh%z
  CALL testVTKMesh%translate(1.0_SRK,2.0_SRK,3.0_SRK)
  bool = ALL((testVTKMesh%x .APPROXEQ. xref+1.0_SRK)) .AND. &
         ALL((testVTKMesh%y .APPROXEQ. yref+2.0_SRK)) .AND. &
         ALL((testVTKMesh%z .APPROXEQ. zref+3.0_SRK))
  ASSERT(bool, 'testVTKMesh%translate(...) STRUCTURED_GRID')

  COMPONENT_TEST('VTK_RECTILINEAR_GRID')
  DEALLOCATE(xref)
  DEALLOCATE(yref)
  DEALLOCATE(zref)
  CALL SetupTest2_Mesh()
  ALLOCATE(xref(size(testVTKMesh%x)))
  ALLOCATE(yref(size(testVTKMesh%y)))
  ALLOCATE(zref(size(testVTKMesh%z)))
  testVTKMesh%meshType=VTK_RECTILINEAR_GRID
  xref=testVTKMesh%x
  yref=testVTKMesh%y
  zref=testVTKMesh%z
  CALL testVTKMesh%translate(1.0_SRK,2.0_SRK,3.0_SRK)
  bool = ALL((testVTKMesh%x .APPROXEQ. xref+1.0_SRK)) .AND. &
         ALL((testVTKMesh%y .APPROXEQ. yref+2.0_SRK)) .AND. &
         ALL((testVTKMesh%z .APPROXEQ. zref+3.0_SRK))
  ASSERT(bool, 'testVTKMesh%translate(...) RECTILINEAR_GRID')

  COMPONENT_TEST('VTK_UNSTRUCTURED_GRID')
  DEALLOCATE(xref)
  DEALLOCATE(yref)
  DEALLOCATE(zref)
  CALL SetupTest4_Mesh()
  ALLOCATE(xref(size(testVTKMesh%x)))
  ALLOCATE(yref(size(testVTKMesh%y)))
  ALLOCATE(zref(size(testVTKMesh%z)))
  xref=testVTKMesh%x
  yref=testVTKMesh%y
  zref=testVTKMesh%z
  CALL testVTKMesh%translate(1.0_SRK,2.0_SRK,3.0_SRK)
  bool = ALL((testVTKMesh%x .APPROXEQ. xref+1.0_SRK)) .AND. &
         ALL((testVTKMesh%y .APPROXEQ. yref+2.0_SRK)) .AND. &
         ALL((testVTKMesh%z .APPROXEQ. zref+3.0_SRK))
  ASSERT(bool, 'testVTKMesh%translate(...) UNSTRUCTURED_GRID')

ENDSUBROUTINE testMeshTranslation
!
!-------------------------------------------------------------------------------
SUBROUTINE testMeshAddition()
  INTEGER(SIK) :: i
  TYPE(VTKMeshType),ALLOCATABLE :: mesh_array(:)

  COMPONENT_TEST('Binary')
  CALL testVTKFile%clear()
  DEALLOCATE(xref)
  DEALLOCATE(yref)
  DEALLOCATE(zref)
  CALL SetupTest14_Mesh()
  testVTKMesh=testVTKMesh+testVTKMesh2
  CALL testVTKFile%initialize(666,'testVTK14.vtk',STATUS='testVTK14')
  CALL testVTKFile%writeMesh(testVTKMesh)

  ALLOCATE(xref(30))
  ALLOCATE(yref(30))
  ALLOCATE(zref(30))
  ALLOCATE(cellRef(5))
  ALLOCATE(nodeRef(30))
  xref=(/0.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,0.0_SRK,0.5_SRK,0.0_SRK,0.0_SRK, &
      0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK,1.0_SRK,1.0_SRK,0.5_SRK,1.0_SRK,1.0_SRK, &
      0.5_SRK,0.0_SRK,0.0_SRK,0.0_SRK,1.0_SRK,1.0_SRK,1.0_SRK/)
  yref=(/0.0_SRK,0.0_SRK,0.5_SRK,0.0_SRK,0.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK, &
      0.5_SRK,1.0_SRK,1.0_SRK,0.5_SRK,1.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK, &
      0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK/)
  zref=(/0.0_SRK,0.0_SRK,0.0_SRK,1.0_SRK,1.0_SRK,1.0_SRK,0.0_SRK,0.0_SRK,0.0_SRK,1.0_SRK,1.0_SRK, &
      1.0_SRK,0.0_SRK,0.0_SRK,0.0_SRK,1.0_SRK,1.0_SRK,1.0_SRK,0.0_SRK,0.0_SRK,0.0_SRK,1.0_SRK,1.0_SRK, &
      1.0_SRK,1.0_SRK,1.0_SRK,1.5_SRK,1.0_SRK,1.0_SRK,1.5_SRK/)
  cellRef=(/13,13,13,13,13/)
  nodeRef=(/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29/)
  ASSERT(testVTKMesh%isInit,'%isInit')
  ASSERT_EQ(testVTKMesh%meshType,VTK_UNSTRUCTURED_GRID,'%meshType')
  ASSERT_EQ(testVTKMesh%numPoints,30,'%numPoints')
  ASSERT_EQ(testVTKMesh%numCells,5,'%numCells')
  DO i=1,30
    ASSERT_APPROXEQ(testVTKMesh%x(i),xref(i),'%x')
    ASSERT_APPROXEQ(testVTKMesh%y(i),yref(i),'%y')
    ASSERT_APPROXEQ(testVTKMesh%z(i),zref(i),'%z')
    ASSERT_EQ(testVTKMesh%nodeList(i),nodeRef(i),'%nodeList')
  ENDDO !i
  DO i=1,5
    ASSERT_EQ(testVTKMesh%cellList(i),cellRef(i),'%cellList')
  ENDDO !i

  DEALLOCATE(xref)
  DEALLOCATE(yref)
  DEALLOCATE(zref)
  DEALLOCATE(cellRef)
  DEALLOCATE(nodeRef)

  COMPONENT_TEST('Array')
  CALL testVTKFile%clear()
  ALLOCATE(mesh_array(5))
  DO i=1,5
    mesh_array(i)=testVTKMesh2
  ENDDO !i
  testVTKMesh=sumMeshes_VTKMesh(mesh_array)
  CALL testVTKFile%initialize(666,'testVTK14.vtk',STATUS='testVTK14')
  CALL testVTKFile%writeMesh(testVTKMesh)

  ALLOCATE(xref(30))
  ALLOCATE(yref(30))
  ALLOCATE(zref(30))
  ALLOCATE(cellRef(5))
  ALLOCATE(nodeRef(30))
  xref=(/0.0_SRK,0.0_SRK,0.0_SRK,1.0_SRK,1.0_SRK,1.0_SRK,0.0_SRK,0.0_SRK,0.0_SRK,1.0_SRK,1.0_SRK, &
      1.0_SRK,0.0_SRK,0.0_SRK,0.0_SRK,1.0_SRK,1.0_SRK,1.0_SRK,0.0_SRK,0.0_SRK,0.0_SRK,1.0_SRK,1.0_SRK, &
      1.0_SRK,0.0_SRK,0.0_SRK,0.0_SRK,1.0_SRK,1.0_SRK,1.0_SRK/)
  yref=(/0.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK, &
      0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK, &
      0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK,0.0_SRK,1.0_SRK,0.5_SRK/)
  zref=(/1.0_SRK,1.0_SRK,1.5_SRK,1.0_SRK,1.0_SRK,1.5_SRK,1.0_SRK,1.0_SRK,1.5_SRK,1.0_SRK,1.0_SRK, &
      1.5_SRK,1.0_SRK,1.0_SRK,1.5_SRK,1.0_SRK,1.0_SRK,1.5_SRK,1.0_SRK,1.0_SRK,1.5_SRK,1.0_SRK,1.0_SRK, &
      1.5_SRK,1.0_SRK,1.0_SRK,1.5_SRK,1.0_SRK,1.0_SRK,1.5_SRK/)
  nodeRef=(/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29/)
  cellRef=(/13,13,13,13,13/)
  ASSERT(testVTKMesh%isInit,'%isInit')
  ASSERT_EQ(testVTKMesh%meshType,VTK_UNSTRUCTURED_GRID,'%meshType')
  ASSERT_EQ(testVTKMesh%numPoints,30,'%numPoints')
  ASSERT_EQ(testVTKMesh%numCells,5,'%numCells')
  DO i=1,30
    ASSERT_APPROXEQ(testVTKMesh%x(i),xref(i),'%x')
    ASSERT_APPROXEQ(testVTKMesh%y(i),yref(i),'%y')
    ASSERT_APPROXEQ(testVTKMesh%z(i),zref(i),'%z')
    ASSERT_EQ(testVTKMesh%nodeList(i),nodeRef(i),'%nodeList')
  ENDDO !i
  DO i=1,5
    ASSERT_EQ(testVTKMesh%cellList(i),cellRef(i),'%cellList')
  ENDDO !i

  DEALLOCATE(xref)
  DEALLOCATE(yref)
  DEALLOCATE(zref)
  DEALLOCATE(cellRef)
  DEALLOCATE(nodeRef)
  DEALLOCATE(mesh_array)

ENDSUBROUTINE testMeshAddition
!
!-------------------------------------------------------------------------------
SUBROUTINE testCellRemoval()

  CALL testVTKFile%clear()
  CALL testVTKFile%initialize(666,'testVTK15.vtk',STATUS='testVTK15')
  CALL SetupTest6_Mesh()
  ALLOCATE(testMask(4))
  testMask=(/ .FALSE.,.TRUE.,.FALSE.,.FALSE. /)
  CALL testVTKMesh%removeCells(testMask)
  CALL testVTKFile%writeMesh(testVTKMesh)
  bool = testVTKMesh%numCells == 3
  ASSERT(bool, 'VTKMeshType removeCells')
  DEALLOCATE(testMask)

ENDSUBROUTINE testCellRemoval
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
  INTEGER(SIK) :: i
  REAL(SRK) :: x
  !Initialize a VTK mesh by hand
  testVTKMesh%dims=(/33,33,5/)
  testVTKMesh%meshType=VTK_STRUCTURED_GRID
  testVTKMesh%numCells=(testVTKMesh%dims(1)-1)*(testVTKMesh%dims(2)-1)* &
      (testVTKMesh%dims(3)-1)
  testVTKMesh%numPoints=testVTKMesh%dims(1)*testVTKMesh%dims(2)* &
      testVTKMesh%dims(3)
  DEALLOCATE(testVTKMesh%x)
  DEALLOCATE(testVTKMesh%y)
  DEALLOCATE(testVTKMesh%z)
  ALLOCATE(testVTKMesh%x(33))
  ALLOCATE(testVTKMesh%y(33))
  ALLOCATE(testVTKMesh%z(5))
  x=0.0_SRK
  DO i=1,testVTKMesh%dims(1)
    testVTKMesh%x(i)=x
    x=x+0.25_SRK
  ENDDO
  x=0.0_SRK
  DO i=1,testVTKMesh%dims(2)
    testVTKMesh%y(i)=x
    x=x+0.25_SRK
  ENDDO
  x=0.0_SRK
  DO i=1,testVTKMesh%dims(3)
    testVTKMesh%z(i)=x
    x=x+2.0_SRK
  ENDDO
  testVTKMesh%isInit=.TRUE.
ENDSUBROUTINE SetupTest2_Mesh

!-------------------------------------------------------------------------------
SUBROUTINE SetupTest4_Mesh()
  INTEGER(SIK) :: i,j
  !Initialize a VTK mesh by hand
  testVTKMesh%numPoints=800
  testVTKMesh%dims=testVTKMesh%numPoints
  testVTKMesh%meshType=VTK_UNSTRUCTURED_GRID
  testVTKMesh%numCells=100
  DEALLOCATE(testVTKMesh%x)
  DEALLOCATE(testVTKMesh%y)
  DEALLOCATE(testVTKMesh%z)
  ALLOCATE(testVTKMesh%x(testVTKMesh%numPoints))
  ALLOCATE(testVTKMesh%y(testVTKMesh%numPoints))
  ALLOCATE(testVTKMesh%z(testVTKMesh%numPoints))
  ALLOCATE(testVTKMesh%cellList(testVTKMesh%numCells))
  ALLOCATE(testVTKMesh%nodeList(800))
  OPEN(unit=555,file='mesh3Points.txt',FORM='FORMATTED', &
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
ENDSUBROUTINE SetupTest4_Mesh
!
!-------------------------------------------------------------------------------
SUBROUTINE SetupTest5_Mesh()
  INTEGER(SIK) :: i,j
  !Initialize a VTK mesh by hand
  testVTKMesh%numPoints=800
  testVTKMesh%dims=testVTKMesh%numPoints
  testVTKMesh%meshType=VTK_UNSTRUCTURED_GRID
  testVTKMesh%numCells=100
  DEALLOCATE(testVTKMesh%x)
  DEALLOCATE(testVTKMesh%y)
  DEALLOCATE(testVTKMesh%z)
  DEALLOCATE(testVTKMesh%cellList)
  DEALLOCATE(testVTKMesh%nodeList)
  ALLOCATE(testVTKMesh%x(testVTKMesh%numPoints))
  ALLOCATE(testVTKMesh%y(testVTKMesh%numPoints))
  ALLOCATE(testVTKMesh%z(testVTKMesh%numPoints))
  ALLOCATE(testVTKMesh%cellList(testVTKMesh%numCells))
  ALLOCATE(testVTKMesh%nodeList(800))
  OPEN(unit=555,file='mesh3Points.txt',FORM='FORMATTED', &
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
ENDSUBROUTINE SetupTest5_Mesh
!
!-------------------------------------------------------------------------------
SUBROUTINE SetupTest6_Mesh()
  !Initialize a VTK mesh by hand
  testVTKMesh%numPoints=24
  testVTKMesh%dims=testVTKMesh%numPoints
  testVTKMesh%meshType=VTK_UNSTRUCTURED_GRID
  testVTKMesh%numCells=4
  DEALLOCATE(testVTKMesh%x)
  DEALLOCATE(testVTKMesh%y)
  DEALLOCATE(testVTKMesh%z)
  DEALLOCATE(testVTKMesh%cellList)
  DEALLOCATE(testVTKMesh%nodeList)
  ALLOCATE(testVTKMesh%x(testVTKMesh%numPoints))
  ALLOCATE(testVTKMesh%y(testVTKMesh%numPoints))
  ALLOCATE(testVTKMesh%z(testVTKMesh%numPoints))
  ALLOCATE(testVTKMesh%cellList(testVTKMesh%numCells))
  ALLOCATE(testVTKMesh%nodeList(24))
  testVTKMesh%x=(/ 0.0,1.0,0.5,0.0,1.0,0.5,0.0,0.0,0.5,0.0,0.0,0.5, &
                   0.0,1.0,0.5,0.0,1.0,0.5,1.0,1.0,0.5,1.0,1.0,0.5 /)
  testVTKMesh%y=(/ 0.0,0.0,0.5,0.0,0.0,0.5,0.0,1.0,0.5,0.0,1.0,0.5, &
                   1.0,1.0,0.5,1.0,1.0,0.5,0.0,1.0,0.5,0.0,1.0,0.5 /)
  testVTKMesh%z=(/ 0.0,0.0,0.0,1.0,1.0,1.0,0.0,0.0,0.0,1.0,1.0,1.0, &
                   0.0,0.0,0.0,1.0,1.0,1.0,0.0,0.0,0.0,1.0,1.0,1.0 /)
  testVTKMesh%nodeList=(/ 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, &
      16,17,18,19,20,21,22,23 /)
  testVTKMesh%cellList=VTK_WEDGE
  testVTKMesh%isInit=.TRUE.
ENDSUBROUTINE SetupTest6_Mesh
!
!-------------------------------------------------------------------------------
SUBROUTINE SetupTest14_Mesh()
  CALL SetupTest6_Mesh()
  testVTKMesh2%numPoints=6
  testVTKMesh2%numCells=1
  ALLOCATE(testVTKMesh2%x(testVTKMesh2%numPoints))
  ALLOCATE(testVTKMesh2%y(testVTKMesh2%numPoints))
  ALLOCATE(testVTKMesh2%z(testVTKMesh2%numPoints))
  ALLOCATE(testVTKMesh2%cellList(testVTKMesh2%numCells))
  ALLOCATE(testVTKMesh2%nodeList(6))
  testVTKMesh2%x=(/ 0.0,0.0,0.0,1.0,1.0,1.0 /)
  testVTKMesh2%y=(/ 0.0,1.0,0.5,0.0,1.0,0.5 /)
  testVTKMesh2%z=(/ 0.0,0.0,0.5,0.0,0.0,0.5 /)
  testVTKMesh2%cellList=VTK_WEDGE
  testVTKMesh2%nodeList=(/ 0,1,2,3,4,5 /)
  testVTKMesh2%meshType=VTK_UNSTRUCTURED_GRID
  testVTKMesh2%isInit=.TRUE.
  CALL testVTKMesh2%translate(0.0_SRK,0.0_SRK,1.0_SRK)
ENDSUBROUTINE SetupTest14_Mesh
!
!-------------------------------------------------------------------------------
SUBROUTINE SetupTest1_Data()
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
SUBROUTINE SetupTest4_Data()
  INTEGER(SIK) :: i

  !Initialize VTK data by hand
  testVTKData%varname='material'
  testVTKData%vtkDataFormat='int'
  testVTKData%dataSetType=VTK_DATA_SCALARS
  testVTKData%isCellData=.TRUE.
  testVTKData%isInit=.TRUE.
  IF(ALLOCATED(testVTKData%dataList)) DEALLOCATE(testVTKData%dataList)
  ALLOCATE(testVTKData%dataList(testVTKMesh%numCells))
  OPEN(unit=555,file='mesh3Data.txt',FORM='FORMATTED', &
      ACCESS='SEQUENTIAL',STATUS='OLD',ACTION='READ')
  DO i=1,testVTKMesh%numCells
    READ(555,*) testVTKData%dataList(i)
  ENDDO
  CLOSE(555)
  ENDSUBROUTINE SetupTest4_Data
ENDPROGRAM testVTKFiles
