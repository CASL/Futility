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
  
  IMPLICIT NONE
  
  CHARACTER(LEN=257) :: longstring
  TYPE(ExceptionHandlerType),POINTER,SAVE :: e
  TYPE(VTKMeshType),SAVE :: testVTKMesh, testVTKMesh2
  TYPE(VTKDataType),SAVE :: testVTKData
  TYPE(VTUXMLFileType),SAVE :: testVTUFile
  TYPE(PVTUXMLFileType),SAVE :: testPVTUFile
  REAL(SRK),ALLOCATABLE :: xref(:),yref(:),zref(:)
  LOGICAL(SBK),ALLOCATABLE :: testMask(:)
  LOGICAL(SBK) :: bool
  INTEGER(SIK) :: k,npartfail

  CREATE_TEST('Test VTU Files')
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING VTUFILES...'
  WRITE(*,*) '==================================================='
  
  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  
  !Check clear
  CALL testVTUFile%clear()
  CALL testVTUFile%e%addSurrogate(e)
  CALL eVTK%addSurrogate(e)
  
!Test Initialize and clear
  CALL testVTUFile%initialize(666,'testVTU0.vtu')
  bool = testVTUFile%isInit() .AND. testVTUFile%isOpen()
  ASSERT(bool, 'testVTUFile%initialize(...)')
  WRITE(*,*) '  Passed: CALL testVTUFile%initialize(...)'
  
  !Error check
  CALL testVTUFile%initialize(666,'testVTU1.vtu',STATUS='testVTU1')
  
  CALL testVTUFile%clear(.TRUE.)
  CALL testVTUFile%e%addSurrogate(e)
  bool = .NOT.testVTUFile%isInit()
  ASSERT(bool, 'testVTUFile%clear()')
  WRITE(*,*) '  Passed: CALL testVTUFile%clear()'
  
  CALL testVTUFile%clear()
  CALL testVTUFile%e%addSurrogate(e)
  longstring='testVTU0.vtu'
  longstring(257:257)='r'
  CALL testVTUFile%initialize(666,'testVTU1.vtu',STATUS=longstring)
!
!Test writeMesh for STRUCTURED_POINTS (should fail, not supported)
  CALL testVTUFile%clear()
  CALL testVTUFile%e%addSurrogate(e)
  CALL testVTUFile%initialize(666,'testVTU1.vtu',STATUS='testVTU1')
  CALL SetupTest1_Mesh()
  CALL testVTUFile%writeMesh(testVTKMesh) ! Try to write STRUCTURED_POINTS
  bool = .NOT.testVTUFile%hasMesh
  ASSERT(bool, 'testVTUFile%writeMesh(...) STRUCTURED_POINTS')
  WRITE(*,*) '  Passed: CALL testVTUFile%writeMesh(...) STRUCTURED_POINTS'
!
!Test writeMesh for UNSTRUCTURED_GRID
  CALL testVTUFile%clear()
  CALL testVTUFile%initialize(666,'testVTU4.vtu',STATUS='testVTU4')
  CALL SetupTest4_Mesh()
  CALL testVTUFile%writeMesh(testVTKMesh)
  bool = testVTUFile%hasMesh
  ASSERT(bool, 'testVTUFile%writeMesh(...) UNSTRUCTURED_GRID')
  WRITE(*,*) '  Passed: CALL testVTUFile%writeMesh(...) UNSTRUCTURED_GRID'
  !Write data on the mesh
  CALL SetupTest4_Data()
  CALL testVTUFile%writeScalarData(testVTKData)
  testVTKData%varname='mat_val'
  testVTKData%vtkDataFormat='float'
  CALL testVTUFile%writeScalarData(testVTKData)
!
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
  CALL testVTUFile%close()
!
!Test removeRedundantPoints for VTK UNSTRUCTURED_GRID
  CALL testVTUFile%clear()
  CALL testVTUFile%initialize(666,'testVTU5pre.vtu',STATUS='testVTU5pre')
  CALL SetupTest5_Mesh()
  !Mesh written before point cleanup, for comparison
  CALL testVTUFile%writeMesh(testVTKMesh)
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
  CALL testVTUFile%close()
  CALL testVTUFile%clear()
  CALL testVTUFile%initialize(666,'testVTU5post.vtu',STATUS='testVTU5post')
  CALL testVTUFile%writeMesh(testVTKMesh)
  CALL testVTUFile%close()
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
  WRITE(*,*) '  Passed: CALL testVTKMesh%cleanupPoints(...) UNSTRUCTURED_GRID'
  !
  !Another test for redundant point removal
  CALL testVTUFile%clear()
  DEALLOCATE(xref)
  DEALLOCATE(yref)
  DEALLOCATE(zref)
  CALL testVTUFile%initialize(666,'testVTU6pre.vtu',STATUS='testVTU6pre')
  CALL SetupTest6_Mesh()
  CALL testVTUFile%writeMesh(testVTKMesh)
  CALL testVTKMesh%cleanupPoints
  CALL testVTUFile%close()
  CALL testVTUFile%clear()
  CALL testVTUFile%initialize(666,'testVTU6post.vtu',STATUS='testVTU6post')
  CALL testVTUFile%writeMesh(testVTKMesh)
  CALL testVTUFile%close()
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
  WRITE(*,*) '  Passed: CALL testVTKMesh%cleanupPoints(...) UNSTRUCTURED_GRID'
  !
  !Test mesh conversion - structured points to unstructured grid
  CALL testVTUFile%clear()
  DEALLOCATE(testVTKMesh%x)
  DEALLOCATE(testVTKMesh%y)
  DEALLOCATE(testVTKMesh%z)
  DEALLOCATE(testVTKMesh%cellList)
  DEALLOCATE(testVTKMesh%nodeList)
  CALL testVTUFile%initialize(666,'testVTU7.vtu',STATUS='testVTU7')
  CALL SetupTest1_Mesh()
  CALL testVTKMesh%convert(VTK_UNSTRUCTURED_GRID)
  CALL testVTUFile%writeMesh(testVTKMesh)
  CALL testVTUFile%close()
  bool = testVTKMesh%numPoints == 5445
  ASSERT(bool, 'testVTKMesh%convert(...) STRUCTURED_POINTS')
  WRITE(*,*) '  Passed: CALL testVTKMesh%convert(...) STRUCTURED_POINTS'
!
!Test mesh conversion - rectilinear grid to unstructured grid
  CALL testVTUFile%clear()
  DEALLOCATE(testVTKMesh%cellList)
  DEALLOCATE(testVTKMesh%nodeList)
  CALL testVTUFile%initialize(666,'testVTU9.vtu',STATUS='testVTU9')
  CALL SetupTest2_Mesh()
  testVTKMesh%meshType=VTK_RECTILINEAR_GRID
  CALL testVTKMesh%convert(VTK_UNSTRUCTURED_GRID)
  CALL testVTUFile%writeMesh(testVTKMesh)
  CALL testVTUFile%close()
  bool = testVTKMesh%numPoints == 5445
  ASSERT(bool, 'testVTKMesh%convert(...) RECTILINEAR_GRID')
  WRITE(*,*) '  Passed: CALL testVTKMesh%convert(...) RECTILINEAR_GRID'
!
!Test mesh translation - VTK_STRUCTURED_POINTS
  CALL testVTUFile%clear()
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
  WRITE(*,*) '  Passed: CALL testVTKMesh%translate(...) STRUCTURED_POINTS'
!
!Test mesh addition
  CALL testVTUFile%clear()
  DEALLOCATE(xref)
  DEALLOCATE(yref)
  DEALLOCATE(zref)
  CALL SetupTest14_Mesh()
  testVTKMesh=testVTKMesh+testVTKMesh2
  CALL testVTKMesh%cleanupPoints()
  CALL testVTUFile%initialize(666,'testVTU14.vtu',STATUS='testVTU14')
  CALL testVTUFile%writeMesh(testVTKMesh)
  CALL testVTUFile%close()
  ALLOCATE(xref(12))
  ALLOCATE(yref(12))
  ALLOCATE(zref(12))
  xref=(/ 0.0,0.0,0.0,0.0,0.0,0.5,0.5,1.0,1.0,1.0,1.0,1.0 /)
  yref=(/ 0.0,0.0,0.5,1.0,1.0,0.5,0.5,0.0,0.0,0.5,1.0,1.0 /)
  zref=(/ 0.0,1.0,1.5,0.0,1.0,0.0,1.0,0.0,1.0,1.5,0.0,1.0 /)
  bool = ALL((testVTKMesh%x .APPROXEQ. xref)) .AND. &
         ALL((testVTKMesh%y .APPROXEQ. yref)) .AND. &
         ALL((testVTKMesh%z .APPROXEQ. zref))
  ASSERT(bool, 'VTKMeshType OPERATOR(+)')
  WRITE(*,*) '  Passed: VTKMeshType OPERATOR(+)'
!
!Test cell removal
  CALL testVTUFile%clear()
  CALL testVTUFile%initialize(666,'testVTU15.vtu',STATUS='testVTU15')
  CALL SetupTest6_Mesh()
  ALLOCATE(testMask(4))
  testMask=(/ .FALSE.,.TRUE.,.FALSE.,.FALSE. /)
  CALL testVTKMesh%removeCells(testMask)
  CALL testVTUFile%writeMesh(testVTKMesh)
  CALL testVTUFile%close()
  bool = testVTKMesh%numCells == 3
  ASSERT(bool, 'VTKMeshType removeCells')
  WRITE(*,*) '  Passed: VTKMeshType removeCells'
!
  CALL testVTUFile%clear()
!Test writeMesh for UNSTRUCTURED_GRID
  CALL testPVTUFile%clear()
  CALL testPVTUFile%initialize(666,'testPVTU_0.vtu',STATUS='testPVTU_0')
  CALL SetupTest4_Mesh()
  CALL testPVTUFile%writeMesh(testVTKMesh)
  bool = testPVTUFile%hasMesh
  ASSERT(bool, 'testPVTUFile%writeMesh(...) UNSTRUCTURED_GRID')
  WRITE(*,*) '  Passed: CALL testPVTUFile%writeMesh(...) UNSTRUCTURED_GRID'
  !Write data on the mesh
  CALL SetupTest4_Data()
  CALL testPVTUFile%writeScalarData(testVTKData)
  CALL testPVTUFile%close()
  CALL testPVTUFile%clear()

  CALL testPVTUFile%initialize(666,'testPVTU_1.vtu',STATUS='testPVTU_1')
  CALL testVTKMesh%translate(-4.0_SRK,-5.0_SRK,3.0_SRK)
  CALL testPVTUFile%writeMesh(testVTKMesh)
  bool = testPVTUFile%hasMesh
  ASSERT(bool, 'testPVTUFile%writeMesh(...) UNSTRUCTURED_GRID')
  WRITE(*,*) '  Passed: CALL testPVTUFile%writeMesh(...) UNSTRUCTURED_GRID'
  !Write data on the mesh
  CALL testPVTUFile%writeScalarData(testVTKData)
  CALL testPVTUFile%close()

  CALL testPVTUFile%writepvtu(666,'testPVTU',2)
  CALL testPVTUFile%clear()
!
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING VTUFILES PASSED!'
  WRITE(*,*) '==================================================='
  DEALLOCATE(e)
  CALL testVTKMesh%clear()
  CALL testVTKMesh2%clear()
  CALL testVTKData%clear()

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
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
      IF (ALLOCATED(testVTKMesh%x)) DEALLOCATE(testVTKMesh%x)
      IF (ALLOCATED(testVTKMesh%y)) DEALLOCATE(testVTKMesh%y)
      IF (ALLOCATED(testVTKMesh%z)) DEALLOCATE(testVTKMesh%z)
      IF (ALLOCATED(testVTKMesh%cellList)) DEALLOCATE(testVTKMesh%cellList)
      IF (ALLOCATED(testVTKMesh%nodeList)) DEALLOCATE(testVTKMesh%nodeList)
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
ENDPROGRAM testVTUFiles