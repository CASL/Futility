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
  REAL(SRK),ALLOCATABLE :: xref(:),yref(:),zref(:)
  LOGICAL(SBK),ALLOCATABLE :: testMask(:)
  INTEGER(SIK) :: k,npartfail
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING VTKFILES...'
  WRITE(*,*) '==================================================='
  
  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  
  !Check clear
  CALL testVTKFile%clear()
  testVTKFile%e => e
  eVTK => e
  
!Test Initialize and clear
  CALL testVTKFile%initialize(666,'testVTK0.vtk')
  IF(.NOT.testVTKFile%isInit() .OR. .NOT.testVTKFile%isOpen()) THEN
    WRITE(*,*) 'CALL testVTKFile%initialize(...) 0 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testVTKFile%initialize(...) 0'
  ENDIF
  
  !Error check
  CALL testVTKFile%initialize(666,'testVTK1.vtk',STATUS='testVTK1')
  
  CALL testVTKFile%clear(.TRUE.)
  testVTKFile%e=>e
  IF(testVTKFile%isInit()) THEN
    WRITE(*,*) 'CALL testVTKFile%clear() 0 FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testVTKFile%clear() 0'
  ENDIF
  
  CALL testVTKFile%clear()
  testVTKFile%e=>e
  longstring='testVTK0.vtk'
  longstring(257:257)='r'
  CALL testVTKFile%initialize(666,'testVTK1.vtk',STATUS=longstring)
!
!Test writeMesh for STRUCTURED_POINTS
  CALL testVTKFile%writeMesh(testVTKMesh) !write with uninitialized file
  CALL testVTKFile%initialize(666,'testVTK1.vtk',STATUS='testVTK1.vtk')
  CALL testVTKFile%writeMesh(testVTKMesh) !write with unitialized mesh
  CALL testVTKFile%fclose()
  CALL testVTKFile%writeMesh(testVTKMesh) !write with closed file
  CALL testVTKFile%clear()
  testVTKFile%e=>e
  CALL testVTKFile%initialize(666,'testVTK1.vtk',STATUS='testVTK1')
  CALL SetupTest1_Mesh()
  CALL testVTKFile%writeMesh(testVTKMesh)
  IF(.NOT.testVTKFile%hasMesh) THEN
    WRITE(*,*) 'CALL testVTKFile%writeMesh(...) STRUCTURED_POINTS FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testVTKFile%writeMesh(...) STRUCTURED_POINTS'
  ENDIF
!
!Test writeScalarData
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
  
  !Error checking/coverage
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
  WRITE(*,*) '  Passed: CALL testVTKFile%writeScalarData(...) 1'
!
!Test writeMesh for STRUCTURED_GRID
  CALL testVTKFile%clear()
  CALL testVTKFile%initialize(666,'testVTK2.vtk',STATUS='testVTK2')
  CALL SetupTest2_Mesh()
  CALL testVTKFile%writeMesh(testVTKMesh)
  IF(.NOT.testVTKFile%hasMesh) THEN
    WRITE(*,*) 'CALL testVTKFile%writeMesh(...) STRUCTURED_GRID FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testVTKFile%writeMesh(...) STRUCTURED_GRID'
  ENDIF
  !Write data on the mesh
  CALL SetupTest1_Data()
  CALL testVTKFile%writeScalarData(testVTKData)
!
!Test writeMesh for RECTILINEAR_GRID
  CALL testVTKFile%clear()
  CALL testVTKFile%initialize(666,'testVTK3.vtk',STATUS='testVTK3')
  CALL SetupTest2_Mesh()
  testVTKMesh%meshType=VTK_RECTILINEAR_GRID
  CALL testVTKFile%writeMesh(testVTKMesh)
  IF(.NOT.testVTKFile%hasMesh) THEN
    WRITE(*,*) 'CALL testVTKFile%writeMesh(...) RECTILINEAR_GRID FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testVTKFile%writeMesh(...) RECTILINEAR_GRID'
  ENDIF
  !Write data on the mesh
  testVTKData%vtkDataFormat='double'
  CALL testVTKFile%writeScalarData(testVTKData)
!
!Test writeMesh for UNSTRUCTURED_GRID
  CALL testVTKFile%clear()
  CALL testVTKFile%initialize(666,'testVTK4.vtk',STATUS='testVTK4')
  CALL SetupTest4_Mesh()
  CALL testVTKFile%writeMesh(testVTKMesh)
  IF(.NOT.testVTKFile%hasMesh) THEN
    WRITE(*,*) 'CALL testVTKFile%writeMesh(...) UNSTRUCTURED_GRID FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testVTKFile%writeMesh(...) UNSTRUCTURED_GRID'
  ENDIF
  !Write data on the mesh
  CALL SetupTest4_Data()
  CALL testVTKFile%writeScalarData(testVTKData)
  testVTKData%varname='mat_val'
  testVTKData%vtkDataFormat='float'
  CALL testVTKFile%writeScalarData(testVTKData)
!
!Test removeRedundantPoints for VTK UNSTRUCTURED_GRID
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
    
    IF(ANY(.NOT.(testVTKMesh%x .APPROXEQA. xref)) .OR. &
     ANY(.NOT.(testVTKMesh%y .APPROXEQA. yref)) .OR. &
     ANY(.NOT.(testVTKMesh%z .APPROXEQA. zref))) THEN
      WRITE(*,*) 'CALL testVTKMesh%cleanupPoints(...) UNSTRUCTURED_GRID FAILED!'
      STOP 666
    ELSE
      npartfail=npartfail+1
    ENDIF
  ENDIF
  IF(npartfail > 0) THEN
    WRITE(*,*) 'WARNING: Partial failures n=',npartfail, &
      ' for "CALL testVTKMesh%cleanupPoints(...) UNSTRUCTURED_GRID"'
  ENDIF
  WRITE(*,*) '  Passed: CALL testVTKMesh%cleanupPoints(...) UNSTRUCTURED_GRID'
  !
  !Another test for redundant point removal
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
  IF(ANY(.NOT.(testVTKMesh%x .APPROXEQ. xref)) .OR. &
     ANY(.NOT.(testVTKMesh%y .APPROXEQ. yref)) .OR. &
     ANY(.NOT.(testVTKMesh%z .APPROXEQ. zref))) THEN
    WRITE(*,*) 'CALL testVTKMesh%cleanupPoints(...) UNSTRUCTURED_GRID FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testVTKMesh%cleanupPoints(...) UNSTRUCTURED_GRID'
  ENDIF
  !
  !Test mesh conversion - structured points to unstructured grid
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
  IF(testVTKMesh%numPoints /= 5445) THEN
    WRITE(*,*) 'CALL testVTKMesh%convert(...) STRUCTURED_POINTS FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testVTKMesh%convert(...) STRUCTURED_POINTS'
  ENDIF
!
!Test mesh conversion - structured grid to unstructured grid
!Not yet implemented; may be implemented later
  !CALL testVTKFile%clear()
  !DEALLOCATE(testVTKMesh%cellList)
  !DEALLOCATE(testVTKMesh%nodeList)
  !CALL testVTKFile%initialize(666,'testVTK8.vtk',STATUS='testVTK8')
  !CALL SetupTest2_Mesh()
  !CALL testVTKMesh%convert(VTK_UNSTRUCTURED_GRID)
  !CALL testVTKFile%writeMesh(testVTKMesh)
  !IF(testVTKMesh%numPoints /= 5445) THEN
  !  WRITE(*,*) 'CALL testVTKMesh%convert(...) STRUCTURED_GRID FAILED!'
  !  STOP 666
  !ELSE
  !  WRITE(*,*) '  Passed: CALL testVTKMesh%convert(...) STRUCTURED_GRID'
  !ENDIF
!
!Test mesh conversion - rectilinear grid to unstructured grid
  CALL testVTKFile%clear()
  DEALLOCATE(testVTKMesh%cellList)
  DEALLOCATE(testVTKMesh%nodeList)
  CALL testVTKFile%initialize(666,'testVTK9.vtk',STATUS='testVTK9')
  CALL SetupTest2_Mesh()
  testVTKMesh%meshType=VTK_RECTILINEAR_GRID
  CALL testVTKMesh%convert(VTK_UNSTRUCTURED_GRID)
  CALL testVTKFile%writeMesh(testVTKMesh)
  IF(testVTKMesh%numPoints /= 5445) THEN
    WRITE(*,*) 'CALL testVTKMesh%convert(...) RECTILINEAR_GRID FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testVTKMesh%convert(...) RECTILINEAR_GRID'
  ENDIF
!
!Test mesh translation - VTK_STRUCTURED_POINTS
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
  IF(.NOT.((testVTKMesh%x(1) .APPROXEQ. xref(1)+1.0_SRK) .AND. &
      (testVTKMesh%x(2) .APPROXEQ. xref(2))) .OR. &
     .NOT.((testVTKMesh%y(1) .APPROXEQ. yref(1)+2.0_SRK) .AND. &
      (testVTKMesh%y(2) .APPROXEQ. yref(2))) .OR. &
     .NOT.((testVTKMesh%z(1) .APPROXEQ. zref(1)+3.0_SRK) .AND. &
      (testVTKMesh%z(2) .APPROXEQ. zref(2)))) THEN
    WRITE(*,*) 'CALL testVTKMesh%translate(...) STRUCTURED_POINTS FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testVTKMesh%translate(...) STRUCTURED_POINTS'
  ENDIF
!
!Test mesh translation - VTK_STRUCTURED_GRID
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
  IF(ANY(.NOT.(testVTKMesh%x .APPROXEQ. xref+1.0_SRK)) .OR. &
     ANY(.NOT.(testVTKMesh%y .APPROXEQ. yref+2.0_SRK)) .OR. &
     ANY(.NOT.(testVTKMesh%z .APPROXEQ. zref+3.0_SRK))) THEN
    WRITE(*,*) 'CALL testVTKMesh%translate(...) STRUCTURED_GRID FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testVTKMesh%translate(...) STRUCTURED_GRID'
  ENDIF
!
!Test mesh translation - VTK_RECTILINEAR_GRID
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
  IF(ANY(.NOT.(testVTKMesh%x .APPROXEQ. xref+1.0_SRK)) .OR. &
     ANY(.NOT.(testVTKMesh%y .APPROXEQ. yref+2.0_SRK)) .OR. &
     ANY(.NOT.(testVTKMesh%z .APPROXEQ. zref+3.0_SRK))) THEN
    WRITE(*,*) 'CALL testVTKMesh%translate(...) RECTILINEAR_GRID FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testVTKMesh%translate(...) RECTILINEAR_GRID'
  ENDIF
!
!Test mesh translation - VTK_UNSTRUCTURED_GRID
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
  IF(ANY(.NOT.(testVTKMesh%x .APPROXEQ. xref+1.0_SRK)) .OR. &
     ANY(.NOT.(testVTKMesh%y .APPROXEQ. yref+2.0_SRK)) .OR. &
     ANY(.NOT.(testVTKMesh%z .APPROXEQ. zref+3.0_SRK))) THEN
    WRITE(*,*) 'CALL testVTKMesh%translate(...) UNSTRUCTURED_GRID FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL testVTKMesh%translate(...) UNSTRUCTURED_GRID'
  ENDIF
!
!Test mesh addition
  CALL testVTKFile%clear()
  DEALLOCATE(xref)
  DEALLOCATE(yref)
  DEALLOCATE(zref)
  CALL SetupTest14_Mesh()
  testVTKMesh=testVTKMesh+testVTKMesh2
  CALL testVTKMesh%cleanupPoints()
  CALL testVTKFile%initialize(666,'testVTK14.vtk',STATUS='testVTK14')
  CALL testVTKFile%writeMesh(testVTKMesh)
  ALLOCATE(xref(12))
  ALLOCATE(yref(12))
  ALLOCATE(zref(12))
  xref=(/ 0.0,0.0,0.0,0.0,0.0,0.5,0.5,1.0,1.0,1.0,1.0,1.0 /)
  yref=(/ 0.0,0.0,0.5,1.0,1.0,0.5,0.5,0.0,0.0,0.5,1.0,1.0 /)
  zref=(/ 0.0,1.0,1.5,0.0,1.0,0.0,1.0,0.0,1.0,1.5,0.0,1.0 /)
  IF(ANY(.NOT.(testVTKMesh%x .APPROXEQ. xref)) .OR. &
     ANY(.NOT.(testVTKMesh%y .APPROXEQ. yref)) .OR. &
     ANY(.NOT.(testVTKMesh%z .APPROXEQ. zref))) THEN
    WRITE(*,*) 'VTKMeshType OPERATOR(+) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: VTKMeshType OPERATOR(+)'
  ENDIF
!
!Test cell removal
  CALL testVTKFile%clear()
  CALL testVTKFile%initialize(666,'testVTK15.vtk',STATUS='testVTK15')
  CALL SetupTest6_Mesh()
  ALLOCATE(testMask(4))
  testMask=(/ .FALSE.,.TRUE.,.FALSE.,.FALSE. /)
  CALL testVTKMesh%removeCells(testMask)
  CALL testVTKFile%writeMesh(testVTKMesh)
  IF(testVTKMesh%numCells /= 3) THEN
    WRITE(*,*) 'VTKMeshType removeCells FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: VTKMeshType removeCells'
  ENDIF
!
!Check for memory leak
 !DO k=1,1000
 !  DEALLOCATE(testVTKMesh2%x)
 !  DEALLOCATE(testVTKMesh2%y)
 !  DEALLOCATE(testVTKMesh2%z)
 !  DEALLOCATE(testVTKMesh2%cellList)
 !  DEALLOCATE(testVTKMesh2%nodeList)
 !  CALL SetupTest14_Mesh()
 !  CALL testVTKMesh2%translate(1.0_SRK,0.0_SRK,0.0_SRK)
 !  testVTKMesh=testVTKMesh+testVTKMesh2
 !ENDDO
!
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING VTKFILES PASSED!'
  WRITE(*,*) '==================================================='
  DEALLOCATE(e)
  CALL testVTKMesh%clear()
  CALL testVTKMesh2%clear()
  CALL testVTKData%clear()
  CALL testVTKFile%clear()
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
