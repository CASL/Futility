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
!> @brief Utility module for working with VTK Files.
!>
!> This module provides derived types for a VTK File, a VTK mesh and VTK data.
!> It also provides enumerated constants that are defined by VTK. Currently, 
!> it only supports writing of VTK legacy files for all but FIELD and POLYGON
!> mesh. In terms of writing data for this mesh, it only supports writing of
!> scalar cell data.
!>
!> A few notes.
!>  - Interfaces for the VTKDataType and VTKMeshType have not been
!>    developed, so these types must be used "by-hand" at present.
!>  - The IOSTAT clause is not used for any of the WRITE statements, so its
!>    possible to get segfaults here, though hopefully that won't happen.
!>  - VTK legacy files only support 1 mesh
!>  - The VTK data that gets written to a file is not strictly enforced
!>    to be the same size as the mesh, as this is not required by VTK but
!>    can lead to abnormal behavior.
!>  - For the name of variables for VTK data, it appears that 'vtk' cannot
!>    appear in the variable name if displaying the data in VisIt
!>    seems like a stupid bug to me.
!>  - VisIt may not support displaying all cell types (e.g. the quadratic ones)
!>  - Tools that utilize VTK may not be fully compatible with VTK and therefore
!>    may not support all the features of this module.
!> 
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref FileType_Fortran "FileType_Fortran": @copybrief FileType_Fortran
!>
!> @par EXAMPLES
!> @code
!> @endcode
!>
!> @author Brendan Kochunas, Daniel Jabaay, Joe Lelli
!>   @date 02/02/2012
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE VTKFiles
  USE IntrType
  USE ExceptionHandler
  USE Allocs
  USE FileType_Fortran
  IMPLICIT NONE
  PRIVATE
  
  !List of Public Members
  PUBLIC :: VTK_STRUCTURED_POINTS
  PUBLIC :: VTK_STRUCTURED_GRID
  PUBLIC :: VTK_UNSTRUCTURED_GRID
  PUBLIC :: VTK_POLYDATA
  PUBLIC :: VTK_RECTILINEAR_GRID
  PUBLIC :: VTK_VERTEX
  PUBLIC :: VTK_POLY_VERTEX
  PUBLIC :: VTK_LINE
  PUBLIC :: VTK_POLY_LINE
  PUBLIC :: VTK_TRIANGLE
  PUBLIC :: VTK_TRIANGLE_STRIP
  PUBLIC :: VTK_POLYGON
  PUBLIC :: VTK_PIXEL
  PUBLIC :: VTK_QUAD
  PUBLIC :: VTK_TETRA
  PUBLIC :: VTK_VOXEL
  PUBLIC :: VTK_HEXAHEDRON
  PUBLIC :: VTK_WEDGE
  PUBLIC :: VTK_PYRAMID
  PUBLIC :: VTK_QUADRATIC_EDGE
  PUBLIC :: VTK_QUADRATIC_TRIANGLE
  PUBLIC :: VTK_QUADRATIC_QUAD
  PUBLIC :: VTK_QUADRATIC_TETRA
  PUBLIC :: VTK_QUADRATIC_HEXAHEDRON
  PUBLIC :: VTK_DATA_SCALARS
  PUBLIC :: VTK_DATA_COLOR_SCALARS
  PUBLIC :: VTK_DATA_LOOKUP_TABLE
  PUBLIC :: VTK_DATA_VECTORS
  PUBLIC :: VTK_DATA_NORMALS
  PUBLIC :: VTK_DATA_TEXTURE_COORDINATES
  PUBLIC :: VTK_DATA_TENSORS
  PUBLIC :: VTK_DATA_FIELD
  PUBLIC :: VTKMeshType
  PUBLIC :: VTKDataType
  PUBLIC :: VTKLegFileType
  PUBLIC :: VTUXMLFileType
  PUBLIC :: PVTUXMLFileType
  PUBLIC :: eVTK
  PUBLIC :: OPERATOR(+)
  
  !>Interface for mesh addition
  INTERFACE OPERATOR(+)
    MODULE PROCEDURE addMesh_VTKMesh
  ENDINTERFACE

  !> Module name for error messages
  CHARACTER(LEN=*),PARAMETER :: modName='VTKFILES'
  
  !> Enumeration for the VTK Dataset type STRUCTURED_POINTS
  INTEGER(SIK),PARAMETER :: VTK_STRUCTURED_POINTS=1
  !> Enumeration for the VTK Dataset type STRUCTURED_GRID
  INTEGER(SIK),PARAMETER :: VTK_STRUCTURED_GRID=2
  !> Enumeration for the VTK Dataset type UNSTRUCTURED_GRID
  INTEGER(SIK),PARAMETER :: VTK_UNSTRUCTURED_GRID=3
  !> Enumeration for the VTK Dataset type POLYDATA
  INTEGER(SIK),PARAMETER :: VTK_POLYDATA=4
  !> Enumeration for the VTK Dataset type RECTILINEAR_GRID
  INTEGER(SIK),PARAMETER :: VTK_RECTILINEAR_GRID=5
  !> Enumeration for the VTK Dataset type FIELD
  INTEGER(SIK),PARAMETER :: VTK_FIELD=6
  
  !> Enumeration for the VTK Cell Type VTK_VERTEX from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_VERTEX=1
  !> Enumeration for the VTK Cell Type VTK_POLY_VERTEX from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_POLY_VERTEX=2
  !> Enumeration for the VTK Cell Type VTK_LINE from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_LINE=3
  !> Enumeration for the VTK Cell Type VTK_POLY_LINE from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_POLY_LINE=4
  !> Enumeration for the VTK Cell Type VTK_TRIANGLE from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_TRIANGLE=5
  !> Enumeration for the VTK Cell Type VTK_TRIANGLE_STRIP from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_TRIANGLE_STRIP=6
  !> Enumeration for the VTK Cell Type VTK_POLYGON from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_POLYGON=7
  !> Enumeration for the VTK Cell Type VTK_PIXEL from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_PIXEL=8
  !> Enumeration for the VTK Cell Type VTK_QUAD from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_QUAD=9
  !> Enumeration for the VTK Cell Type VTK_TETRA from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_TETRA=10
  !> Enumeration for the VTK Cell Type VTK_VOXEL from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_VOXEL=11
  !> Enumeration for the VTK Cell Type VTK_HEXAHEDRON from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_HEXAHEDRON=12
  !> Enumeration for the VTK Cell Type VTK_WEDGE from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_WEDGE=13
  !> Enumeration for the VTK Cell Type VTK_PYRAMID from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_PYRAMID=14
  !> Enumeration for the VTK Cell Type VTK_QUADRATIC_EDGE from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_QUADRATIC_EDGE=21
  !> Enumeration for the VTK Cell Type VTK_QUADRATIC_TRIANGLE from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_QUADRATIC_TRIANGLE=22
  !> Enumeration for the VTK Cell Type VTK_QUADRATIC_QUAD from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_QUADRATIC_QUAD=23
  !> Enumeration for the VTK Cell Type VTK_QUADRATIC_TETRA from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_QUADRATIC_TETRA=24
  !> Enumeration for the VTK Cell Type VTK_QUADRATIC_HEXAHEDRON from VTK Standard
  INTEGER(SIK),PARAMETER :: VTK_QUADRATIC_HEXAHEDRON=25
  
  !> Enumeration for VTK SCALAR data attribute
  INTEGER(SIK),PARAMETER :: VTK_DATA_SCALARS=1
  !> Enumeration for VTK COLOR_SCALAR data attribute
  INTEGER(SIK),PARAMETER :: VTK_DATA_COLOR_SCALARS=2
  !> Enumeration for VTK LOOKUP_TABLE data attribute
  INTEGER(SIK),PARAMETER :: VTK_DATA_LOOKUP_TABLE=3
  !> Enumeration for VTK VECTORS data attribute
  INTEGER(SIK),PARAMETER :: VTK_DATA_VECTORS=4
  !> Enumeration for VTK NORMALS data attribute
  INTEGER(SIK),PARAMETER :: VTK_DATA_NORMALS=5
  !> Enumeration for VTK TEXTURE_COORDINATES data attribute
  INTEGER(SIK),PARAMETER :: VTK_DATA_TEXTURE_COORDINATES=6
  !> Enumeration for VTK TENSORS data attribute
  INTEGER(SIK),PARAMETER :: VTK_DATA_TENSORS=7
  !> Enumeration for VTK FIELD data attribute
  INTEGER(SIK),PARAMETER :: VTK_DATA_FIELD=8
  
  !> Type for representing a VTK mesh
  !>
  !> Depending on the meshType only certain attributes are meaningful.
  !> This should be documented here, for now read the code.
  !>
  !> Adding type-bound procedures for initializing and clearing the
  !> object should be considered.
  TYPE :: VTKMeshType
    !> Logical for whether or not the mesh has been written to the file
    LOGICAL(SBK) :: isInit=.FALSE.
    !> The type of the VTK mesh (e.g. structured, unstructured, etc.)
    !> Should be one of the enumerations provided by the module.
    INTEGER(SIK) :: meshType=0
    !> The number of cells in the mesh
    INTEGER(SIK) :: numCells=0
    !> The number of points or vertices in the mesh
    INTEGER(SIK) :: numPoints=0
    !> The dimensions nx, ny, nz for the DIMENSIONS data where present
    INTEGER(SIK) :: dims(3)=0
    !> The list of cell types; needed for unstructured grid types
    !> The cell types are provided as enumerations in this module.
    INTEGER(SIK),ALLOCATABLE :: cellList(:)
    !> The node list for each cell; needed for unstructured grid types
    INTEGER(SIK),ALLOCATABLE :: nodeList(:)
    !> The x coordinates of the vertices in the mesh
    REAL(SRK),ALLOCATABLE :: x(:)
    !> The y coordinates of the vertices in the mesh
    REAL(SRK),ALLOCATABLE :: y(:)
    !> The z coordinates of the vertices in the mesh
    REAL(SRK),ALLOCATABLE :: z(:)
!
!List of type-bound procedures (methods) for the VTK Mesh type
    CONTAINS
      !> @copybrief VTKFiles::removeRedundantPoints
      !> @copydetails VTKFiles::removeRedundantPoints
      PROCEDURE,PASS :: cleanupPoints => removeRedundantPts
      !> @copybrief VTKFiles::convert_VTKMeshType
      !> @copydetails VTKFiles::convert_VTKMeshType
      PROCEDURE,PASS :: convert => convert_VTKMeshType
      !> @copybrief VTKFiles::translate_VTKMeshType
      !> @copydetails VTKFiles::translate_VTKMeshType
      PROCEDURE,PASS :: translate => translate_VTKMeshType
      !> @copybrief VTKFiles::removeCells_VTKMeshType
      !> @copydetails VTKFiles::removeCells_VTKMeshType
      PROCEDURE,PASS :: removeCells => removeCells_VTKMeshType
      !> @copybrief VTKFiles::clear_VTKMeshType
      !> @copydetails VTKFiles::clear_VTKMeshType
      PROCEDURE,PASS :: clear => clear_VTKMeshType
  ENDTYPE VTKMeshType
  
  !> Type for representing data that can be written on a VTK mesh
  !>
  !> Adding type-bound procedures for initializing and clearing the
  !> object should be considered.
  TYPE :: VTKDataType
    !> Logical for whether or not the mesh has been written to the file
    LOGICAL(SBK) :: isInit=.FALSE.
    !> The name of the data set being written
    CHARACTER(LEN=64) :: varname=''
    !> The format type of the data (e.g. float, int, etc.)
    CHARACTER(LEN=14) :: vtkDataFormat=''
    !> Whether or not the data is cell data or point data
    LOGICAL(SBK) :: isCellData=.FALSE.
    !> The type of the data set (e.g. one of the parameters VTK_DATA_*)
    !> defined in this module.
    INTEGER(SIK) :: dataSetType=0
    !> The data values to be written
    REAL(SRK),ALLOCATABLE :: dataList(:)
!
!List of type-bound procedures (methods) for the VTK Mesh type
    CONTAINS
      !> @copybrief VTKFiles::clear_VTKDataType
      !> @copydetails VTKFiles::clear_VTKDataType
      PROCEDURE,PASS :: clear => clear_VTKDataType
  ENDTYPE VTKDataType
  
  
  !> @brief Derived type object for files definable by the legacy VTK format
  !>
  !> This is an extension of the @ref FileType_Fortran "FortranFileType".
  TYPE,EXTENDS(FortranFileType) :: VTKLegFileType
    !isInit inherited from FortranFileType
    !> Whether or not this object has a mesh added to it
    LOGICAL(SBK) :: hasMesh=.FALSE.
    !> The VTK version of this file
    REAL(SRK) :: version=3.0_SRK
    !> The VTK mesh in this file
    TYPE(VTKMeshType) :: mesh
!
!List of type bound procedures (methods) for the VTKLeg File type
    CONTAINS
      !> @copybrief VTKFiles::init_VTKLegFileType
      !> @copydetails VTKFiles::init_VTKLegFileType
      PROCEDURE,PASS :: initialize => init_VTKLegFileType
      !> @copybrief VTKFiles::clear_VTKLegFileType
      !> @copydetails VTKFiles::clear_VTKLegFileType
      PROCEDURE,PASS :: clear => clear_VTKLegFileType
      !> @copybrief VTKFiles::writeMesh_VTKLegFileType
      !> @copydetails VTKFiles::writeMesh_VTKLegFileType
      PROCEDURE,PASS :: writeMesh => writeMesh_VTKLegFileType
      !> @copybrief VTKFiles::writeScalarData_VTKLegFileType
      !> @copydetails VTKFiles::writeScalarData_VTKLegFileType
      PROCEDURE,PASS :: writeScalarData => writeScalarData_VTKLegFileType
  ENDTYPE VTKLegFileType


  !> @brief Derived type object for files definable by the VTU XML format
  !>
  !> This is an extension of the @ref FileType_Fortran "VTKLegFileType".
  TYPE,EXTENDS(VTKLegFileType) :: VTUXMLFileType
!
!List of type bound procedures (methods) for the VTU XML File type
    CONTAINS
      !> @copybrief VTUFiles::init_VTUXMLFileType
      !> @copydetails VTUFiles::init_VTUXMLFileType
      PROCEDURE,PASS :: initialize => init_VTUXMLFileType
      !> @copybrief VTUFiles::writeMesh_VTUXMLFileType
      !> @copydetails VTUFiles::writeMesh_VTUXMLFileType
      PROCEDURE,PASS :: writeMesh => writeMesh_VTUXMLFileType
      !> @copybrief VTUFiles::writeScalarData_VTUXMLFileType
      !> @copydetails VTUFiles::writeScalarData_VTUXMLFileType
      PROCEDURE,PASS :: writeScalarData => writeScalarData_VTUXMLFileType
      !> @copybrief VTUFiles::close_VTUXMLFileType
      !> @copydetails VTUFiles::close_VTUXMLFileType
      PROCEDURE,PASS :: close => close_VTUXMLFileType
  ENDTYPE VTUXMLFileType
  
  !> @brief Derived type object for files definable by the PVTU XML format
  !>
  !> This is an extension of the @ref FileType_Fortran "VTUXMLFileType".
  TYPE,EXTENDS(VTUXMLFileType) :: PVTUXMLFileType
  
  CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: fileList
  INTEGER(SIK) :: fileListLength = 0
  CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: varNameList
  INTEGER(SIK) :: varNameListLength = 0
  CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: dataFormatList
  INTEGER(SIK) :: dataFormatListLength = 0
!
!List of type bound procedures (methods) for the VTU XML File type
    CONTAINS
      !> @copybrief VTUFiles::init_PVTUXMLFileType
      !> @copydetails VTUFiles::init_PVTUXMLFileType
      PROCEDURE,PASS :: initialize => init_PVTUXMLFileType
      !> @copybrief VTUFiles::writeScalarData_PVTUXMLFileType
      !> @copydetails VTUFiles::writeScalarData_PVTUXMLFileType
      PROCEDURE,PASS :: writeScalarData => writeScalarData_PVTUXMLFileType
      !> @copybrief VTUFiles::writepvtu_PVTUXMLFileType
      !> @copydetails VTUFiles::writepvtu_PVTUXMLFileType
      PROCEDURE,PASS :: writepvtu => writepvtu_PVTUXMLFileType
  ENDTYPE PVTUXMLFileType

  !> @brief Generic interface for assignment operator (=)
  !>
  !> Adds assignment capability for VTKMeshType
  !> Main purpose is for valgrind errors in VERA builds.
  INTERFACE ASSIGNMENT(=)
    !> @copybrief VTKFiles::assign_VTKMeshType
    !> @copydetails VTKFiles::assign_VTKMeshType
    MODULE PROCEDURE assign_VTKMeshType
  ENDINTERFACE
  
  TYPE(ExceptionHandlerType),SAVE :: eVTK
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the VTKLegacy file object.
!> @param fileobj input file object.
!> @param unit Unit number to use for the file.
!> @param status optional, used here for name of header of VTK File
!>
!> The other input arguments are not currently supported. See 
!> @ref FileType_Fortran::init_fortran_file "init_fortran_file" for a complete
!> description of all the optional input arguments.
!> 
!> This routine initializes the file object through the Fortran file type
!> interface then writes the VTK header to the file.
!>
    SUBROUTINE init_VTKLegFileType(fileobj,unit,file,status,access,form, &
                                 position,action,pad,recl)
      CHARACTER(LEN=*),PARAMETER :: myName='init_VTKLegFileType'
      CLASS(VTKLegFileType),INTENT(INOUT) :: fileobj
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: unit
      CHARACTER(LEN=*),INTENT(IN) :: file
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: status
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: access
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: form
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: position
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: action
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: pad
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: recl
      
      CHARACTER(LEN=256) :: title
      
      IF(.NOT.fileobj%isInit()) THEN
        IF(PRESENT(access)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "ACCESS" is being ignored. Value is "SEQUENTIAL".')
        IF(PRESENT(form)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "FORM" is being ignored. Value is "FORMATTED".')
        IF(PRESENT(action)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "ACTION" is being ignored. Value is "WRITE".')
        IF(PRESENT(pad)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "PAD" is being ignored. Value is "YES".')
        IF(PRESENT(position)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "POSITION" is being ignored. Value is "APPEND".')
        IF(PRESENT(recl)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "RECL" is being ignored. File is "SEQUENTIAL".')
        
        !Initialize the file, only support ascii for now
        CALL init_fortran_file(fileobj,unit,file,'REPLACE','SEQUENTIAL', &
          'FORMATTED','APPEND','WRITE')
      
        !Open the file
        CALL fileobj%fopen()
      
        !Determine the file's title
        IF(PRESENT(status)) THEN
          IF(LEN_TRIM(status) > 256) THEN
            CALL fileobj%e%raiseDebug(modName//'::'//myName// &
              ' - File title name is being truncated!')
            title=status(1:256)
          ELSE
            title=TRIM(status)
          ENDIF
        ELSE
          title='vtk output'
        ENDIF
      
        !Write header to file, title and format information to file
        IF(fileobj%isOpen()) THEN
          WRITE(fileobj%getUnitNo(),'(a,f4.1)') '# vtk DataFile Version',fileobj%version
          WRITE(fileobj%getUnitNo(),'(a)') TRIM(title)
          WRITE(fileobj%getUnitNo(),'(a)') 'ASCII'
          !Change "new" status so that if file is closed and re-opened
          !during execution it will be appended file instead of replacing it.
          fileobj%newstat=.FALSE.
        ENDIF
      ELSE
        CALL fileobj%e%raiseError(modName//'::'//myName// &
          ' - VTK File is already initialized!')
      ENDIF
    ENDSUBROUTINE init_VTKLegFileType
!
!-------------------------------------------------------------------------------
!> @brief Clears the VTKLegacy file object.
!> @param file the VTK Legacy file object
!> @param ldel logical on whether or not to delete the file
!>
!> Clears the file object using the @ref FileType_Fortran::clear_fortran_file 
!> "clear_fortran_file" interface. This is why @c ldel is an argument. In
!> practice one probably does not want to delete these files before the program
!> exits.
!>
    SUBROUTINE clear_VTKLegFileType(file,ldel)
      CLASS(VTKLegFileType),INTENT(INOUT) :: file
      LOGICAL(SBK),OPTIONAL,INTENT(IN) :: ldel
      LOGICAL(SBK) :: bool
      IF(file%hasMesh) CALL clear_VTKMeshType(file%mesh)
      file%hasMesh=.FALSE.
      bool=.FALSE.
      IF(PRESENT(ldel)) bool=ldel
      CALL clear_fortran_file(file,bool)
    ENDSUBROUTINE clear_VTKLegFileType
!
!-------------------------------------------------------------------------------
!> @brief Clears the VTK Mesh object.
!> @param myVTKMesh the VTK mesh object
!>
    SUBROUTINE clear_VTKMeshType(myVTKMesh)
      CLASS(VTKMeshType),INTENT(INOUT) :: myVTKMesh
      myVTKMesh%isInit=.FALSE.
      myVTKMesh%meshType=0
      myVTKMesh%numCells=0
      myVTKMesh%numPoints=0
      myVTKMesh%dims=0
      IF(ALLOCATED(myVTKMesh%cellList)) DEALLOCATE(myVTKMesh%cellList)
      IF(ALLOCATED(myVTKMesh%nodeList)) DEALLOCATE(myVTKMesh%nodeList)
      IF(ALLOCATED(myVTKMesh%x)) DEALLOCATE(myVTKMesh%x)
      IF(ALLOCATED(myVTKMesh%y)) DEALLOCATE(myVTKMesh%y)
      IF(ALLOCATED(myVTKMesh%z)) DEALLOCATE(myVTKMesh%z)
    ENDSUBROUTINE clear_VTKMeshType
!
!-------------------------------------------------------------------------------
!> @brief Clears the VTK Data object.
!> @param myVTKData the VTK Data object
!>
    SUBROUTINE clear_VTKDataType(myVTKData)
      CLASS(VTKDataType),INTENT(INOUT) :: myVTKData
      
      myVTKData%isInit=.FALSE.
      myVTKData%varname=''
      myVTKData%vtkDataFormat=''
      myVTKData%isCellData=.FALSE.
      myVTKData%dataSetType=0
      IF(ALLOCATED(myVTKData%dataList)) DEALLOCATE(myVTKData%dataList)
    ENDSUBROUTINE clear_VTKDataType
!
!-------------------------------------------------------------------------------
!> @brief Assigns one VTK Mesh object to another.
!> @param thisVTKMeshType the VTK Data object being assigned to
!> @param thatVTKMeshType the VTK Data object being assigned from
!>
    SUBROUTINE assign_VTKMeshType(thisVTKMeshType,thatVTKMeshType)
      TYPE(VTKMeshType),INTENT(INOUT) :: thisVTKMeshType
      TYPE(VTKMeshType),INTENT(IN) :: thatVTKMeshType

      IF(thisVTKMeshType%isInit) CALL clear_VTKMeshType(thisVTKMeshType)
      IF(thatVTKMeshType%isInit) THEN
        thisVTKMeshType%meshType=thatVTKMeshType%meshType
        thisVTKMeshType%numcells=thatVTKMeshType%numcells
        thisVTKMeshType%numpoints=thatVTKMeshType%numpoints
        thisVTKMeshType%dims=thatVTKMeshType%dims
        IF(ALLOCATED(thatVTKMeshType%cellList)) THEN
          ALLOCATE(thisVTKMeshType%cellList(SIZE(thatVTKMeshType%cellList)))
          thisVTKMeshType%cellList=thatVTKMeshType%cellList
        ENDIF
        IF(ALLOCATED(thatVTKMeshType%nodeList)) THEN
          ALLOCATE(thisVTKMeshType%nodeList(SIZE(thatVTKMeshType%nodeList)))
          thisVTKMeshType%nodeList=thatVTKMeshType%nodeList
        ENDIF
        IF(ALLOCATED(thatVTKMeshType%x)) THEN
          ALLOCATE(thisVTKMeshType%x(SIZE(thatVTKMeshType%x)))
          thisVTKMeshType%x=thatVTKMeshType%x
        ENDIF
        IF(ALLOCATED(thatVTKMeshType%y)) THEN
          ALLOCATE(thisVTKMeshType%y(SIZE(thatVTKMeshType%y)))
          thisVTKMeshType%y=thatVTKMeshType%y
        ENDIF
        IF(ALLOCATED(thatVTKMeshType%z)) THEN
          ALLOCATE(thisVTKMeshType%z(SIZE(thatVTKMeshType%z)))
          thisVTKMeshType%z=thatVTKMeshType%z
        ENDIF
        thisVTKMeshType%isInit=thatVTKMeshType%isInit
      ENDIF

    ENDSUBROUTINE assign_VTKMeshType
!
!-------------------------------------------------------------------------------
!> @brief Writes a VTK mesh to VTK Legacy file
!> @param myVTKFile the VTK file to write the data to
!> @param vtkMesh the VTK mesh to write in the file
!> 
!> This routine supports writing of STRUCTURED_POINTS, STRUCTURED_GRIDS, 
!> RECTILINEAR_GRIDS, and UNSTRUCTURED_GRIDS. The other types of 
!> geometry/topology are not yet implemented. The display of this 
!> data was verified by loading and displaying it with VisIt. It should be noted
!> that other tools may have varying support for VTK legacy files.
!>
    SUBROUTINE writeMesh_VTKLegFileType(myVTKFile,vtkMesh)
      CHARACTER(LEN=*),PARAMETER :: myName='writeMesh_VTKLegFileType'
      CLASS(VTKLegFileType),INTENT(INOUT) :: myVTKFile
      TYPE(VTKMeshType),INTENT(IN) :: vtkMesh
      CHARACTER(LEN=256) :: aline,sint
      INTEGER(SIK) :: funit,i,j,k,n
      
      IF(.NOT.myVTKFile%hasMesh) THEN
        IF(myVTKFile%isOpen()) THEN
          IF(vtkMesh%isInit) THEN
            funit=myVTKFile%getUnitNo()
            myVTKFile%mesh=vtkMesh
            myVTKFile%hasMesh=.TRUE.
            SELECTCASE(vtkMesh%meshType)
              CASE(VTK_STRUCTURED_POINTS)
                !Write a mesh that is structured points
                WRITE(funit,'(a)') 'DATASET STRUCTURED_POINTS'
                aline='DIMENSIONS'
                DO i=1,3
                  WRITE(sint,'(i64)') myVTKFile%mesh%dims(i); sint=ADJUSTL(sint)
                  aline=TRIM(aline)//' '//TRIM(sint)
                ENDDO
                WRITE(funit,'(a)') TRIM(aline)
                WRITE(funit,'(a,3es17.8)') 'ORIGIN ', &
                  myVTKFile%mesh%x(1),myVTKFile%mesh%y(1),myVTKFile%mesh%z(1)
                WRITE(funit,'(a,3es17.8)') 'SPACING ', &
                  myVTKFile%mesh%x(2),myVTKFile%mesh%y(2),myVTKFile%mesh%z(2)
                WRITE(funit,'(a)') ''
              CASE(VTK_STRUCTURED_GRID)
                !Write a mesh that is a structured grid
                WRITE(funit,'(a)') 'DATASET STRUCTURED_GRID'
                aline='DIMENSIONS'
                DO i=1,3
                  WRITE(sint,'(i64)') myVTKFile%mesh%dims(i); sint=ADJUSTL(sint)
                  aline=TRIM(aline)//' '//TRIM(sint)
                ENDDO
                WRITE(funit,'(a)') TRIM(aline)
                WRITE(sint,'(i64)') myVTKFile%mesh%numPoints; sint=ADJUSTL(sint)
                WRITE(funit,'(a)') 'POINTS '//TRIM(sint)//' float'
                DO k=1,myVTKFile%mesh%dims(3)
                  DO j=1,myVTKFile%mesh%dims(2)
                    DO i=1,myVTKFile%mesh%dims(1)
                      WRITE(funit,'(3es17.8)') myVTKFile%mesh%x(i), &
                        myVTKFile%mesh%y(j),myVTKFile%mesh%z(k)
                    ENDDO
                  ENDDO
                ENDDO
                WRITE(funit,'(a)') ''
              CASE(VTK_RECTILINEAR_GRID)
                !Write a mesh that is a rectilinear grid
                WRITE(funit,'(a)') 'DATASET RECTILINEAR_GRID'
                aline='DIMENSIONS'
                DO i=1,3
                  WRITE(sint,'(i64)') myVTKFile%mesh%dims(i); sint=ADJUSTL(sint)
                  aline=TRIM(aline)//' '//TRIM(sint)
                ENDDO
                WRITE(funit,'(a)') TRIM(aline)
                
                !Write the x coordinates
                WRITE(sint,'(i64)') myVTKFile%mesh%dims(1); sint=ADJUSTL(sint)
                WRITE(funit,'(a)') 'X_COORDINATES '//TRIM(sint)//' float'
                DO i=1,myVTKFile%mesh%dims(1)-MOD(myVTKFile%mesh%dims(1),3),3
                  WRITE(funit,'(3es17.8)') myVTKFile%mesh%x(i), &
                        myVTKFile%mesh%x(i+1),myVTKFile%mesh%x(i+2)
                ENDDO
                IF(MOD(myVTKFile%mesh%dims(1),3) == 1) THEN
                  i=myVTKFile%mesh%dims(1)
                  WRITE(funit,'(3es17.8)') myVTKFile%mesh%x(i)
                ELSEIF(MOD(myVTKFile%mesh%dims(1),3) == 2) THEN
                  i=myVTKFile%mesh%dims(1)-1
                  WRITE(funit,'(3es17.8)') myVTKFile%mesh%x(i), &
                    myVTKFile%mesh%x(i+1)
                ENDIF
                
                !Write the y coordinates
                WRITE(sint,'(i64)') myVTKFile%mesh%dims(2); sint=ADJUSTL(sint)
                WRITE(funit,'(a)') 'Y_COORDINATES '//TRIM(sint)//' float'
                DO i=1,myVTKFile%mesh%dims(2)-MOD(myVTKFile%mesh%dims(2),3),3
                  WRITE(funit,'(3es17.8)') myVTKFile%mesh%y(i), &
                        myVTKFile%mesh%y(i+1),myVTKFile%mesh%y(i+2)
                ENDDO
                IF(MOD(myVTKFile%mesh%dims(2),3) == 1) THEN
                  i=myVTKFile%mesh%dims(2)
                  WRITE(funit,'(3es17.8)') myVTKFile%mesh%y(i)
                ELSEIF(MOD(myVTKFile%mesh%dims(2),3) == 2) THEN
                  i=myVTKFile%mesh%dims(2)-1
                  WRITE(funit,'(3es17.8)') myVTKFile%mesh%y(i), &
                    myVTKFile%mesh%y(i+1)
                ENDIF
                
                !Write the z coordinates
                WRITE(sint,'(i64)') myVTKFile%mesh%dims(3); sint=ADJUSTL(sint)
                WRITE(funit,'(a)') 'Z_COORDINATES '//TRIM(sint)//' float'
                DO i=1,myVTKFile%mesh%dims(3)-MOD(myVTKFile%mesh%dims(3),3),3
                  WRITE(funit,'(3es17.8)') myVTKFile%mesh%z(i),myVTKFile%mesh%z(i+1),myVTKFile%mesh%z(i+2)
                ENDDO
                IF(MOD(myVTKFile%mesh%dims(3),3) == 1) THEN
                  i=myVTKFile%mesh%dims(3)
                  WRITE(funit,'(3es17.8)') myVTKFile%mesh%z(i)
                ELSEIF(MOD(myVTKFile%mesh%dims(3),3) == 2) THEN
                  i=myVTKFile%mesh%dims(3)-1
                  WRITE(funit,'(3es17.8)') myVTKFile%mesh%z(i), &
                    myVTKFile%mesh%z(i+1)
                ENDIF
                WRITE(funit,'(a)') ''
              CASE(VTK_UNSTRUCTURED_GRID)
                !Write a mesh that is an unstructured grid
                !Clean-up redundant points
                CALL myVTKFile%mesh%cleanupPoints()
                WRITE(funit,'(a)') 'DATASET UNSTRUCTURED_GRID'
                WRITE(sint,'(i64)') myVTKFile%mesh%numPoints; sint=ADJUSTL(sint)
                WRITE(funit,'(a)') 'POINTS '//TRIM(sint)//' float'
                DO i=1,myVTKFile%mesh%numPoints
!                  WRITE(*,'(i0,a1,3es17.8)') i,':',myVTKFile%mesh%x(i), &
!                    myVTKFile%mesh%y(i),myVTKFile%mesh%z(i)
                  WRITE(funit,'(3es17.8)') myVTKFile%mesh%x(i), &
                    myVTKFile%mesh%y(i),myVTKFile%mesh%z(i)
                ENDDO
                WRITE(funit,'(a)') ''
                
                !Write the list of cell vertices
                aline='CELLS'
                WRITE(sint,'(i64)') myVTKFile%mesh%numCells; sint=ADJUSTL(sint)
                aline=TRIM(aline)//' '//TRIM(sint)
                WRITE(sint,'(i64)') SIZE(myVTKFile%mesh%nodeList)+ &
                  myVTKFile%mesh%numCells
                sint=ADJUSTL(sint)
                aline=TRIM(aline)//' '//TRIM(sint)
                WRITE(funit,'(a)') TRIM(aline)
                j=1
                DO i=1,myVTKFile%mesh%numCells
                  !Determine the next n nodes that make up this cell
                  SELECTCASE(myVTKFile%mesh%cellList(i))
                    CASE(VTK_VERTEX); n=1
                    CASE(VTK_LINE); n=2
                    CASE(VTK_TRIANGLE); n=3
                    CASE(VTK_QUAD); n=4
                    CASE(VTK_TETRA); n=4
                    CASE(VTK_HEXAHEDRON); n=8
                    CASE(VTK_VOXEL); n=8
                    CASE(VTK_WEDGE); n=6
                    CASE(VTK_PYRAMID); n=5
                    CASE(VTK_QUADRATIC_EDGE); n=3
                    CASE(VTK_QUADRATIC_TRIANGLE); n=6
                    CASE(VTK_QUADRATIC_QUAD); n=8
                    CASE(VTK_QUADRATIC_TETRA); n=10
                    CASE(VTK_QUADRATIC_HEXAHEDRON); n=20
                    CASE DEFAULT
                      CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                        ' - VTK cell type is not supported!')
                      n=0
                  ENDSELECT
                  IF(n > 0) THEN
                    WRITE(sint,'(i64)') n; sint=ADJUSTL(sint)
                    aline=TRIM(sint)
                    DO k=0,n-1
                      WRITE(sint,'(i64)') myVTKFile%mesh%nodeList(j+k)
                      sint=ADJUSTL(sint)
                      aline=TRIM(aline)//' '//TRIM(sint)
                    ENDDO
                    WRITE(funit,'(a)') TRIM(aline)
                    j=j+n
                  ENDIF
                ENDDO
                WRITE(funit,'(a)') ''
                
                !Write the list of cell types
                aline='CELL_TYPES'
                WRITE(sint,'(i64)') myVTKFile%mesh%numCells; sint=ADJUSTL(sint)
                aline=TRIM(aline)//' '//TRIM(sint)
                WRITE(funit,'(a)') TRIM(aline)
                WRITE(funit,*) myVTKFile%mesh%cellList
                WRITE(funit,'(a)') ''
                
              CASE(VTK_POLYDATA)
                CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                  ' - VTK DATASET "POLYDATA" not supported!')
              CASE DEFAULT
                CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                  ' - VTK DATASET type is not recognized!')
            ENDSELECT
            FLUSH(funit)
          ELSE
            CALL myVTKFile%e%raiseError(modName//'::'//myName// &
              ' - VTK Mesh is not initialized!')
          ENDIF
        ELSE
          CALL myVTKFile%e%raiseError(modName//'::'//myName// &
            ' - VTK File is not open for writing!')
        ENDIF
      ELSE
        CALL myVTKFile%e%raiseError(modName//'::'//myName// &
          ' - VTK File already has a mesh!')
      ENDIF
    ENDSUBROUTINE writeMesh_VTKLegFileType
!
!-------------------------------------------------------------------------------
!> @brief Writes scalar data for a VTK mesh to VTK Legacy file
!> @param myVTKFile the VTK file to write the data to
!> @param vtkData the VTK data to write to the file
!> 
!> Presently, this routine is only capable of writing scalar cell data.
!> The data can be written as integers, single precision or double precision.
!> Note that in the VTKDataType data is always stored as double precision.
!>
!> Currently no check is made if the amount of data differs from the number
!> of cells in the mesh that has already been written in the file. From what
!> I understand about VTK, it does not have to, and that a default lookup table
!> will be used to obtain missing values. Adding a warning message when the
!> amount of data is not consistent should be done.
!>
    SUBROUTINE writeScalarData_VTKLegFileType(myVTKFile,vtkData)
      CHARACTER(LEN=*),PARAMETER :: myName='writeScalarData_VTKLegFileType'
      CLASS(VTKLegFileType),INTENT(INOUT) :: myVTKFile
      TYPE(VTKDataType),INTENT(IN) :: vtkData
      INTEGER(SIK) :: funit,i,istp
      CHARACTER(LEN=256) :: aline,sint
      
      IF(myVTKFile%hasMesh) THEN
        IF(myVTKFile%isOpen()) THEN
          IF(vtkData%isInit) THEN
            IF(vtkData%isCellData) THEN
              IF(vtkData%dataSetType == VTK_DATA_SCALARS) THEN
                funit=myVTKFile%getUnitNo()
                aline='CELL_DATA'
                WRITE(sint,'(i64)') myVTKFile%mesh%numCells; sint=ADJUSTL(sint)
                aline=TRIM(aline)//' '//TRIM(sint)
                SELECTCASE(TRIM(vtkData%vtkDataFormat))
                  CASE('float')
                    WRITE(funit,'(a)') TRIM(aline)
                    WRITE(funit,'(a)') 'SCALARS '//TRIM(vtkData%varname)//' '// &
                      TRIM(vtkData%vtkDataFormat)
                    WRITE(funit,'(a)') 'LOOKUP_TABLE '//TRIM(vtkData%varname)//'_table'
                    DO i=1,SIZE(vtkData%datalist),5
                      istp=i+4
                      IF(istp > SIZE(vtkData%datalist)) istp=SIZE(vtkData%datalist)
                      WRITE(funit,'(5f15.6)') REAL(vtkData%datalist(i:istp),SSK)
                    ENDDO
                    WRITE(funit,'(a)') ''
                    WRITE(funit,'(a)') 'POINT_DATA 1'
                    WRITE(funit,'(a)') ''
                  CASE('double')
                    WRITE(funit,'(a)') TRIM(aline)
                    WRITE(funit,'(a)') 'SCALARS '//TRIM(vtkData%varname)//' '// &
                      TRIM(vtkData%vtkDataFormat)
                    WRITE(funit,'(a)') 'LOOKUP_TABLE '//TRIM(vtkData%varname)//'_table'
                    DO i=1,SIZE(vtkData%datalist),3
                      istp=i+2
                      IF(istp > SIZE(vtkData%datalist)) istp=SIZE(vtkData%datalist)
                      WRITE(funit,'(3es22.14)') vtkData%datalist(i:istp)
                    ENDDO
                    WRITE(funit,'(a)') ''
                    WRITE(funit,'(a)') 'POINT_DATA 1'
                    WRITE(funit,'(a)') ''
                  CASE('int','short','long')
                    WRITE(funit,'(a)') TRIM(aline)
                    WRITE(funit,'(a)') 'SCALARS '//TRIM(vtkData%varname)//' '// &
                      TRIM(vtkData%vtkDataFormat)
                    WRITE(funit,'(a)') 'LOOKUP_TABLE '//TRIM(vtkData%varname)//'_table'
                    DO i=1,SIZE(vtkData%datalist),6
                      istp=i+5
                      IF(istp > SIZE(vtkData%datalist)) istp=SIZE(vtkData%datalist)
                      WRITE(funit,'(6i12)') NINT(vtkData%datalist(i:istp),SIK)
                    ENDDO
                    WRITE(funit,'(a)') ''
                    WRITE(funit,'(a)') 'POINT_DATA 1'
                    WRITE(funit,'(a)') ''
                  CASE DEFAULT
                    CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                      ' - Writing of "'//TRIM(vtkData%vtkDataFormat)// &
                        '" data not yet supported!')
                ENDSELECT
                FLUSH(funit)
              ELSE
                CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                ' - dataSetType is not yet supported!')
              ENDIF
            ELSE
              CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                ' - Writing of point data is not yet supported!')
            ENDIF
          ELSE
            CALL myVTKFile%e%raiseError(modName//'::'//myName// &
              ' - VTK Data is not initialized!')
          ENDIF
        ELSE
          CALL myVTKFile%e%raiseError(modName//'::'//myName// &
            ' - VTK File is not open for writing!')
        ENDIF
      ELSE
        CALL myVTKFile%e%raiseError(modName//'::'//myName// &
          ' - VTK File has no mesh to write data for!')
      ENDIF
    ENDSUBROUTINE writeScalarData_VTKLegFileType
!
!-------------------------------------------------------------------------------
!> @brief Remove redundant (i.e. duplicate, trplicate, etc) points from a VTK
!>        unstructured grid
!> @param thisVTKMesh the VTK mesh with redundant points
!>
!> For some VTK unstructured grids (where all points and connectivity data are
!> defined explicitly), there are redundant points, i.e. points with the same
!> x,y,z coordinates as other points. This leads the resultant VTK file being
!> unnecessarily bloated. This routine sorts the point list, removes the
!> redundant points, reconstructs the point list, and alters the node list to
!> reflect the new point list.
!>
    SUBROUTINE removeRedundantPts(thisVTKMesh)
      CLASS(VTKMeshType),INTENT(INOUT) :: thisVTKMesh
      LOGICAL(SBK),ALLOCATABLE :: isRedundant(:)
      INTEGER(SIK),ALLOCATABLE :: newPtList(:),partialPtList(:)
      REAL(SRK),ALLOCATABLE :: newX(:),newY(:),newZ(:)
      INTEGER(SIK) :: uniqPoints
      INTEGER(SIK) :: i,j
      REAL(SRK) :: x,y,z
      
      !Allocate scratch arrays
      ALLOCATE(isRedundant(thisVTKMesh%numPoints))
      isRedundant=.FALSE.
      ALLOCATE(newPtList(thisVTKMesh%numPoints))
      ALLOCATE(newY(thisVTKMesh%numPoints))
      ALLOCATE(newZ(thisVTKMesh%numPoints))
      
      !Only unstructured grid is supported, because this is the only mesh type
      !likely to have redundant points
      IF(thisVTKMesh%meshType == VTK_UNSTRUCTURED_GRID) THEN
        !This will keep track of where the points are moved when they are sorted
        !This is necessary for updating the node list
        DO i=1,thisVTKMesh%numPoints
          newPtList(i)=i-1
        ENDDO
          
!Sort all the points along x
        CALL mergeSort(thisVTKMesh%numPoints,thisVTkMesh%x,newPtList)
          
        !Re-order y and z coordinates to match sorted x coordinates
        DO i=1,thisVTKMesh%numPoints
          newY(i)=thisVTKMesh%y(newPtList(i)+1)
          newZ(i)=thisVTKMesh%z(newPtList(i)+1)
        ENDDO
        thisVTKMesh%y=newY
        DEALLOCATE(newY)
!
!Sort the y-coordinates within each value of x
        i=1
        j=0
        DO WHILE(i < thisVTKMesh%numPoints)
          DO WHILE(thisVTKMesh%x(j+1) .APPROXEQA. thisVTKMesh%x(i))
            j=j+1
            IF(j == thisVTKMesh%numPoints) EXIT
          ENDDO
          ALLOCATE(newY(j-i+1))
          ALLOCATE(partialPtList(j-i+1))
          newY=thisVTKMesh%y(i:j)
          partialPtList=newPtList(i:j)
            
          CALL mergeSort(j-i+1,newY,partialPtList)
            
          newPtList(i:j)=partialPtList
          thisVTKMesh%y(i:j)=newY
          DEALLOCATE(newY)
          DEALLOCATE(partialPtList)
          i=j+1
          j=i
        ENDDO

        !Re-order z coordinates to match sorted x and y coordinates
        DO i=1,thisVTKMesh%numPoints
          newZ(i)=thisVTKMesh%z(newPtList(i)+1)
        ENDDO
        thisVTKMesh%z=newZ
        DEALLOCATE(newZ)
!          
!Sort z-coordinates for each all points with same x- and y-coordinates
        i=1
        j=0
        DO WHILE(i < thisVTKMesh%numPoints)
          DO WHILE((thisVTKMesh%x(j+1) .APPROXEQA. thisVTKMesh%x(i)) .AND. &
                    (thisVTKMesh%y(j+1) .APPROXEQA. thisVTKMesh%y(i)))
            j=j+1
            IF(j==thisVTKMesh%numPoints) EXIT
          ENDDO
          ALLOCATE(newZ(j-i+1))
          ALLOCATE(partialPtList(j-i+1))
          newZ=thisVTKMesh%z(i:j)
          partialPtList=newPtList(i:j)
          CALL mergeSort(j-i+1,newZ,partialPtList)
          newPtList(i:j)=partialPtList
          thisVTKMesh%z(i:j)=newZ
          DEALLOCATE(newZ)
          DEALLOCATE(partialPtList)
          i=j+1
          j=i
        ENDDO
          
          
        !invert newPtList so that the index is the old point number, 
        !and the output is the new
        ALLOCATE(partialPtList(thisVTKMesh%numPoints))
        DO i=1,thisVTKMesh%numPoints
          partialPtList(newPtList(i)+1)=i-1
        ENDDO
        newPtList=partialPtList
        DEALLOCATE(partialPtList)
          
        !Alter the node list to reflect the reordered point list
        DO i=1,size(ThisVTKMesh%nodeList)
          ThisVTKMesh%nodeList(i)=newPtList(thisVTKMesh%nodeList(i)+1)
        ENDDO
!          
!Remove the redundant points
        uniqPoints=0_SIK
        i=1
        DO WHILE(i <= thisVTKMesh%numPoints)
          x=thisVTKMesh%x(i)
          y=thisVTKMesh%y(i)
          z=thisVTKMesh%z(i)
          newPtList(i)=uniqPoints
          uniqPoints=uniqPoints+1
          i=i+1
          IF(i > thisVTKMesh%numPoints) EXIT
          DO WHILE((thisVTKMesh%x(i) .APPROXEQA. x) .AND. &
            (thisVTKMesh%y(i) .APPROXEQA. y))
            IF(ABS(z-thisVTKMesh%z(i)) <= 2._SRK*EPSREAL) THEN
              isRedundant(i)=.TRUE.
            ELSE
              z=thisVTKMesh%z(i)
              uniqPoints=uniqPoints+1
            ENDIF
            newPtList(i)=uniqPoints-1
            i=i+1
            IF(i > thisVTKMesh%numPoints) EXIT
          ENDDO
        ENDDO
          
        !Create new list of xyz coordinates
        ALLOCATE(newX(uniqPoints))
        ALLOCATE(newY(uniqPoints))
        ALLOCATE(newZ(uniqPoints))
        j=0
        DO i=1,thisVTKMesh%numPoints
          IF(.NOT.isRedundant(i)) THEN
            j=j+1
            newX(j)=thisVTKMesh%x(i)
            newY(j)=thisVTKMesh%y(i)
            newZ(j)=thisVTKMesh%z(i)
          ENDIF
        ENDDO
        CALL demallocA(thisVTKMesh%x)
        CALL demallocA(thisVTKMesh%y)
        CALL demallocA(thisVTKMesh%z)
        CALL dmallocA(thisVTKMesh%x,uniqPoints)
        CALL dmallocA(thisVTKMesh%y,uniqPoints)
        CALL dmallocA(thisVTKMesh%z,uniqPoints)
        thisVTKMesh%numPoints=uniqPoints
        thisVTKMesh%x=newX
        thisVTKMesh%y=newY
        thisVTKMesh%z=newZ
          
        !Update the node list to remove the redundant points
        DO i=1,SIZE(thisVTKMesh%nodeList)
          thisVTKMesh%nodeList(i)=newPtList(thisVTKMesh%nodeList(i)+1)
        ENDDO
          
        !Clean up scratch variables
        DEALLOCATE(newX)
        DEALLOCATE(newY)
        DEALLOCATE(newZ)
        DEALLOCATE(isRedundant)
        DEALLOCATE(newPtList)
      ENDIF
    ENDSUBROUTINE removeRedundantPts
!
!-------------------------------------------------------------------------------
!> @brief For use in the mergeSort subroutine. Creates one ordered array
!>        (in ascending order) from two ordered arrays. Also takes two linked
!>        arrays
!> @param left, right the two ordered arrays
!> @param new the resultant ordered array
!> @param lpoints, rpoints arrays of point indexes for the array being sorted.
!>        This information will be necessary in altering the node list
!> @param NL, NR the size of the left and right ordered arrays, respectively
!> @param N the size of the output array
!>
   PURE SUBROUTINE merger(nl,nr,n,lpoints,rpoints,left,right,new,points)
     INTEGER(SIK),INTENT(IN) :: nl
     INTEGER(SIK),INTENT(IN) :: nr
     INTEGER(SIK),INTENT(IN) :: n
     INTEGER(SIK),INTENT(IN) :: lpoints(nl)
     INTEGER(SIK),INTENT(IN) :: rpoints(nr)
     REAL(SRK),INTENT(IN) :: left(nl)
     REAL(SRK),INTENT(IN) :: right(nr)
     REAL(SRK),INTENT(OUT) :: new(n)
     INTEGER(SIK),INTENT(OUT) :: points(n)
     INTEGER(SIK) :: i,j,k
     
     i=1
     j=1
     DO k=1,n
       IF(i > nl) THEN
         new(k)=right(j)
         points(k)=rpoints(j)
         j=j+1
       ELSEIF(j > nr) THEN
         new(k)=left(i)
         points(k)=lpoints(i)
         i=i+1
       ELSEIF(left(i) < right(j)) THEN
         new(k)=left(i)
         points(k)=lpoints(i)
         i=i+1
       ELSE
         new(k)=right(j)
         points(k)=rpoints(j)
         j=j+1
       ENDIF
     ENDDO
   ENDSUBROUTINE merger
!
!-------------------------------------------------------------------------------
!> @brief Orders a numeric array, in ascending order, using the merge sort 
!>        algorithm
!> @param array numeric array to be sorted
!> @param N length of the numeric array
!> @param pointList list on point indexes, which is linked to the numeric array.
!>        This is needed to reorder the node list
!>
   RECURSIVE SUBROUTINE mergeSort(n,array,pointList)
     INTEGER(SIK),INTENT(IN) :: n
     REAL(SRK),INTENT(INOUT) :: array(n)
     INTEGER(SIK),INTENT(INOUT) :: pointList(n)
     REAL(SRK),ALLOCATABLE :: rightArray(:)
     REAL(SRK),ALLOCATABLE :: leftArray(:)
     INTEGER(SIK),ALLOCATABLE :: leftPoints(:),rightPoints(:)
     INTEGER(SIK) :: nr,nl
     
     CALL dmallocA(rightArray,n/2)
     CALL dmallocA(leftArray, n-(n/2))
     CALL dmallocA(leftPoints,n-(n/2))
     CALL dmallocA(rightPoints,n/2)
     IF(n > 1) THEN
       nr=n/2
       nl=n-nr
       leftArray=array(1:nl)
       rightArray=array(nl+1:n)
       leftPoints=pointList(1:nl)
       rightPoints=pointList(nl+1:n)
       CALL mergeSort(nl,leftArray,leftPoints)
       CALL mergeSort(nr,rightArray,rightPoints)
       CALL merger(nl,nr,n,leftPoints,rightPoints,leftArray,rightArray, &
         array,pointList)
     ENDIF
     CALL demallocA(rightArray)
     CALL demallocA(leftArray)
     CALL demallocA(leftPoints)
     CALL demallocA(rightPoints)
   ENDSUBROUTINE mergeSort
!
!-------------------------------------------------------------------------------
!> @brief converts VTKMesh object to a new type
!> @param thisVTKMesh VTK mesh object to be converted
!>        Currently, must be of type VTK_STRUCTURED_POINTS or
!>        VTK_RECTILINEAR_GRID
!> @param newVTKMeshType VTK mesh type to which the VTKMesh will be converted
!>        Currently, must be VTK_UNSTRUCTURED_POINTS
!>
  SUBROUTINE convert_VTKMeshType(thisVTKMesh,newVTKMeshType)
    CHARACTER(LEN=*),PARAMETER :: myName='convert_VTKMeshType'
    CLASS(VTKMeshType),INTENT(INOUT) :: thisVTKMesh
    INTEGER(SIK),INTENT(IN) :: newVTKMeshType
    REAL(SRK),ALLOCATABLE :: pointMap(:,:,:)
    REAL(SRK),ALLOCATABLE :: xList(:)
    REAL(SRK),ALLOCATABLE :: yList(:)
    REAL(SRK),ALLOCATABLE :: zList(:)
    INTEGER(SIK) :: i,j,k,n
    
    IF(thisVTKMesh%isinit) THEN
      SELECTCASE(newVTKMeshType)
        CASE(VTK_STRUCTURED_POINTS)
          !Will be implemented later, print warning
          CALL eVTK%raiseError(modName//'::'//myName// &
            ' - Conversion to type VTK_STRUCTURED_POINTS is not supported!')
        CASE(VTK_STRUCTURED_GRID)
          !Will be implemented later, print warning
          CALL eVTK%raiseError(modName//'::'//myName// &
            ' - Conversion to type VTK_STRUCTURED_GRID is not supported!')
        CASE(VTK_UNSTRUCTURED_GRID)
          SELECTCASE(thisVTKMesh%meshType)
            CASE(VTK_STRUCTURED_POINTS)
              thisVTKMesh%meshType=VTK_UNSTRUCTURED_GRID
              thisVTKMesh%numPoints=thisVTKMesh%dims(1)*thisVTKMesh%dims(2)* &
                                    thisVTKMesh%dims(3)
              CALL dmallocA(Xlist,thisVTKMesh%numPoints)
              CALL dmallocA(Ylist,thisVTKMesh%numPoints)
              CALL dmallocA(Zlist,thisVTKMesh%numPoints)
              CALL dmallocA(pointMap,thisVTKMesh%dims(1),thisVTKMesh%dims(2), &
                            thisVTKMesh%dims(3))
              n=0
              DO k=1,thisVTKMesh%dims(3)
                DO j=1,thisVTKMesh%dims(2)
                  DO i=1,thisVTKMesh%dims(1)
                    pointmap(i,j,k)=n
                    Xlist(n+1)=(i-1)*thisVTKMesh%x(2)+thisVTKMesh%x(1)
                    Ylist(n+1)=(j-1)*thisVTKMesh%y(2)+thisVTKMesh%y(1)
                    Zlist(n+1)=(k-1)*thisVTKMesh%z(2)+thisVTKMesh%z(1)
                    n=n+1
                  ENDDO
                ENDDO
              ENDDO
              CALL demallocA(thisVTKMesh%x)
              CALL demallocA(thisVTKMesh%y)
              CALL demallocA(thisVTKMesh%z)
              CALL dmallocA(thisVTKMesh%x,thisVTKMesh%numPoints)
              CALL dmallocA(thisVTKMesh%y,thisVTKMesh%numPoints)
              CALL dmallocA(thisVTKMesh%z,thisVTKMesh%numPoints)
              ThisVTKMesh%x=Xlist
              ThisVTKMesh%y=Ylist
              ThisVTKMesh%z=Zlist
              IF(thisVTKMesh%numCells == 0) THEN
                thisVTKMesh%numCells=(thisVTKMesh%dims(1)-1)* &
                (thisVTKMesh%dims(2)-1)*(thisVTKMesh%dims(3)-1)
              ENDIF
              CALL dmallocA(thisVTKMesh%cellList,thisVTKMesh%numCells)
              CALL dmallocA(thisVTKMesh%nodeList,thisVTKMesh%numCells*8)
              thisVTKMesh%cellList=VTK_HEXAHEDRON
              n=1
              DO k=1,thisVTKMesh%dims(3)-1
                DO j=1,thisVTKMesh%dims(2)-1
                  DO i=1,thisVTKMesh%dims(1)-1
                    thisVTKMesh%nodeList(n)=pointMap(i,j,k)
                    thisVTKMesh%nodeList(n+1)=pointMap(i+1,j,k)
                    thisVTKMesh%nodeList(n+2)=pointMap(i+1,j+1,k)
                    thisVTKMesh%nodeList(n+3)=pointMap(i,j+1,k)
                    thisVTKMesh%nodeList(n+4)=pointMap(i,j,k+1)
                    thisVTKMesh%nodeList(n+5)=pointMap(i+1,j,k+1)
                    thisVTKMesh%nodeList(n+6)=pointMap(i+1,j+1,k+1)
                    thisVTKMesh%nodeList(n+7)=pointMap(i,j+1,k+1)
                    n=n+8
                  ENDDO
                ENDDO
              ENDDO
              CALL demallocA(Xlist)
              CALL demallocA(Ylist)
              CALL demallocA(Zlist)
              CALL demallocA(pointMap)
              
            CASE(VTK_RECTILINEAR_GRID)
              thisVTKMesh%meshType=VTK_UNSTRUCTURED_GRID
              thisVTKMesh%numPoints=thisVTKMesh%dims(1)*thisVTKMesh%dims(2)* &
                                    thisVTKMesh%dims(3)
              CALL dmallocA(Xlist,thisVTKMesh%numPoints)
              CALL dmallocA(Ylist,thisVTKMesh%numPoints)
              CALL dmallocA(Zlist,thisVTKMesh%numPoints)
              CALL dmallocA(pointMap,thisVTKMesh%dims(1),thisVTKMesh%dims(2), &
                            thisVTKMesh%dims(3))
              n=0
              DO k=1,thisVTKMesh%dims(3)
                DO j=1,thisVTKMesh%dims(2)
                  DO i=1,thisVTKMesh%dims(1)
                    pointmap(i,j,k)=n
                    Xlist(n+1)=thisVTKMesh%x(i)
                    Ylist(n+1)=thisVTKMesh%y(j)
                    Zlist(n+1)=thisVTKMesh%z(k)
                    n=n+1
                  ENDDO
                ENDDO
              ENDDO
              CALL demallocA(thisVTKMesh%x)
              CALL demallocA(thisVTKMesh%y)
              CALL demallocA(thisVTKMesh%z)
              CALL dmallocA(thisVTKMesh%x,thisVTKMesh%numPoints)
              CALL dmallocA(thisVTKMesh%y,thisVTKMesh%numPoints)
              CALL dmallocA(thisVTKMesh%z,thisVTKMesh%numPoints)
              ThisVTKMesh%x=Xlist
              ThisVTKMesh%y=Ylist
              ThisVTKMesh%z=Zlist
              IF(thisVTKMesh%numCells == 0) THEN
                thisVTKMesh%numCells=(thisVTKMesh%dims(1)-1)* &
                (thisVTKMesh%dims(2)-1)*(thisVTKMesh%dims(3)-1)
              ENDIF
              CALL dmallocA(thisVTKMesh%cellList,thisVTKMesh%numCells)
              CALL dmallocA(thisVTKMesh%nodeList,thisVTKMesh%numCells*8)
              thisVTKMesh%cellList=VTK_HEXAHEDRON
              n=1
              DO k=1,thisVTKMesh%dims(3)-1
                DO j=1,thisVTKMesh%dims(2)-1
                  DO i=1,thisVTKMesh%dims(1)-1
                    thisVTKMesh%nodeList(n)=pointMap(i,j,k)
                    thisVTKMesh%nodeList(n+1)=pointMap(i+1,j,k)
                    thisVTKMesh%nodeList(n+2)=pointMap(i+1,j+1,k)
                    thisVTKMesh%nodeList(n+3)=pointMap(i,j+1,k)
                    thisVTKMesh%nodeList(n+4)=pointMap(i,j,k+1)
                    thisVTKMesh%nodeList(n+5)=pointMap(i+1,j,k+1)
                    thisVTKMesh%nodeList(n+6)=pointMap(i+1,j+1,k+1)
                    thisVTKMesh%nodeList(n+7)=pointMap(i,j+1,k+1)
                    n=n+8
                  ENDDO
                ENDDO
              ENDDO
              CALL demallocA(Xlist)
              CALL demallocA(Ylist)
              CALL demallocA(Zlist)
              CALL demallocA(pointMap)
            CASE(VTK_UNSTRUCTURED_GRID)
              RETURN !already an unstructured grid
            CASE DEFAULT
              !Print warning for VTKMesh object types not yet implemented
              CALL eVTK%raiseError(modName//'::'//myName// &
                ' - VTKMesh type not supported for conversion!')
          ENDSELECT
        CASE(VTK_POLYDATA)
          !Will be implemented later, print warning
          CALL eVTK%raiseError(modName//'::'//myName// &
            ' - Conversion to type VTK_POLYDATA is not supported!')
        CASE(VTK_RECTILINEAR_GRID)
          !Will be implemented later, print warning
          CALL eVTK%raiseError(modName//'::'//myName// &
            ' - Conversion to type VTK_RECTILINEAR_GRID is not supported!')
      ENDSELECT
    ENDIF
  ENDSUBROUTINE convert_VTKMeshType
!
!-------------------------------------------------------------------------------
!> @brief Translates (shifts) a VTKMesh object in the X, Y, and/or Z directions
!> @param thisVTKMesh VTK mesh object to be translated
!>        Currently, VTK_POLYDATA is unsupported
!> @param x_shift Translation distance in the X direction
!> @param y_shift Translation distance in the Y direction
!> @param z_shift Translation distance in the Z direction
!>
  SUBROUTINE translate_VTKMeshType(thisVTKMesh,x_shift,y_shift,z_shift)
    CHARACTER(LEN=*),PARAMETER :: myName='translate_VTKMeshType'
    CLASS(VTKMeshType),INTENT(INOUT) :: thisVTKMesh
    REAL(SRK),INTENT(IN) :: x_shift
    REAL(SRK),INTENT(IN) :: y_shift
    REAL(SRK),INTENT(IN) :: z_shift
    
    IF(thisVTKMesh%meshType == VTK_STRUCTURED_POINTS) THEN
      thisVTKMesh%x(1)=thisVTKMesh%x(1)+x_shift
      thisVTKMesh%y(1)=thisVTKMesh%y(1)+y_shift
      thisVTKMesh%z(1)=thisVTKMesh%z(1)+z_shift
    ELSEIF(thisVTKMesh%meshType == VTK_STRUCTURED_GRID .OR. &
           thisVTKMesh%meshType == VTK_RECTILINEAR_GRID .OR. &
           thisVTKMesh%meshType == VTK_UNSTRUCTURED_GRID) THEN
      thisVTKMesh%x=thisVTKMesh%x+x_shift
      thisVTKMesh%y=thisVTKMesh%y+y_shift
      thisVTKMesh%z=thisVTKMesh%z+z_shift
    ELSE
      CALL eVTK%raiseError(modName//'::'//myName// &
        ' - VTKMesh type not supported!')
    ENDIF
  ENDSUBROUTINE translate_VTKMeshType
!
!-------------------------------------------------------------------------------
!> @brief Removes cells from a VTK mesh object
!> @param thisVTKMesh The VTK mesh object to have cells removed
!> @param cellNotPresent Mask showing which cells are to be removed in the final
!>        mesh
!>
  SUBROUTINE removeCells_VTKMeshType(thisVTKMesh,cellNotPresent)
    CLASS(VTKMeshType),INTENT(INOUT) :: thisVTKMesh
    LOGICAL(SBK),ALLOCATABLE,INTENT(IN) :: cellNotPresent(:)
    CHARACTER(LEN=*),PARAMETER :: myName='removeCells_VTKMeshType'
    LOGICAL(SBK),ALLOCATABLE :: pointPresent(:),nodePresent(:)
    INTEGER(SIK),ALLOCATABLE :: scratchCellList(:),scratchNodeList(:)
    REAL(SRK),ALLOCATABLE :: scratchX(:),scratchY(:),scratchZ(:)
    INTEGER(SIK) :: i,j,k,n,p,q
    INTEGER(SIK),ALLOCATABLE :: pointMap(:)

    n=0
    q=0
    
    CALL thisVTKMesh%convert(VTK_UNSTRUCTURED_GRID)
    ALLOCATE(nodePresent(SIZE(thisVTKMesh%nodeList)))
    
    !Calculate the length of new cell list and node list
    DO i=1,thisVTKMesh%numCells
      IF(.NOT. cellNotPresent(i)) THEN
        n=n+1
      SELECTCASE(thisVTKMesh%cellList(i))
        CASE(VTK_VERTEX); q=q+1
        CASE(VTK_LINE); q=q+2
        CASE(VTK_TRIANGLE); q=q+3
        CASE(VTK_QUAD); q=q+4
        CASE(VTK_TETRA); q=q+4
        CASE(VTK_HEXAHEDRON); q=q+8
        CASE(VTK_VOXEL); q=q+8
        CASE(VTK_WEDGE); q=q+6
        CASE(VTK_PYRAMID); q=q+5
        CASE(VTK_QUADRATIC_EDGE); q=q+3
        CASE(VTK_QUADRATIC_TRIANGLE); q=q+6
        CASE(VTK_QUADRATIC_QUAD); q=q+8
        CASE(VTK_QUADRATIC_TETRA); q=q+10
        CASE(VTK_QUADRATIC_HEXAHEDRON); q=q+20
        CASE DEFAULT
            CALL eVTK%raiseError(modName//'::'//myName// &
            ' - VTK cell type is not supported!')
            n=0
      ENDSELECT
      ENDIF
    ENDDO
    
    !Fill scratch cell lists and node lists
    ALLOCATE(scratchCellList(n))
    ALLOCATE(scratchNodeList(q))
    p=1
    q=1
    k=1
    DO i=1,thisVTKMesh%numCells
      SELECTCASE(thisVTKMesh%cellList(i))
        CASE(VTK_VERTEX); n=1
        CASE(VTK_LINE); n=2
        CASE(VTK_TRIANGLE); n=3
        CASE(VTK_QUAD); n=4
        CASE(VTK_TETRA); n=4
        CASE(VTK_HEXAHEDRON); n=8
        CASE(VTK_VOXEL); n=8
        CASE(VTK_WEDGE); n=6
        CASE(VTK_PYRAMID); n=5
        CASE(VTK_QUADRATIC_EDGE); n=3
        CASE(VTK_QUADRATIC_TRIANGLE); n=6
        CASE(VTK_QUADRATIC_QUAD); n=8
        CASE(VTK_QUADRATIC_TETRA); n=10
        CASE(VTK_QUADRATIC_HEXAHEDRON); n=20
        CASE DEFAULT
            CALL eVTK%raiseError(modName//'::'//myName// &
            ' - VTK cell type is not supported!')
            n=0
      ENDSELECT
      IF(.NOT. cellNotPresent(i)) THEN
        scratchCellList(p)=thisVTKMesh%cellList(i)
        p=p+1
        DO j=q,q+n-1
          scratchNodeList(j)=thisVTKMesh%nodeList(k)
          k=k+1
        ENDDO
        q=q+n
      ELSE
        k=k+n
      ENDIF
    ENDDO
    thisVTKMesh%numCells=SIZE(scratchCellList)
    
    !Find which points are no longer used, remove them, and renumber the node
    !list
    ALLOCATE(pointPresent(thisVTKMesh%numPoints))
    pointPresent=.FALSE.
    DO i=1,SIZE(scratchNodeList)
      pointPresent(scratchNodeList(i)+1)=.TRUE.
    ENDDO
    ALLOCATE(pointMap(thisVTKMesh%numPoints))
    j=0
    DO i=1,thisVTKMesh%numPoints
      IF(pointPresent(i)) THEN
        pointMap(i)=j
        j=j+1
      ELSE
        pointMap(i)=-1_SIK
      ENDIF
    ENDDO
    DO i=1,SIZE(scratchNodeList)
      scratchNodeList(i)=pointMap(scratchNodeList(i)+1)
    ENDDO
    ALLOCATE(scratchX(j))
    ALLOCATE(scratchY(j))
    ALLOCATE(scratchZ(j))
    k=1
    DO i=1,thisVTKMesh%numPoints
      IF(pointPresent(i)) THEN
        scratchX(k)=thisVTKMesh%x(i)
        scratchY(k)=thisVTKMesh%y(i)
        scratchZ(k)=thisVTKMesh%z(i)
        k=k+1
      ENDIF
    ENDDO
    
    !Enter new point lists, cell list, and node list into the VTK mesh
    !object
    CALL demallocA(ThisVTKMesh%x)
    CALL demallocA(ThisVTKMesh%y)
    CALL demallocA(ThisVTKMesh%z)
    CALL demallocA(ThisVTKMesh%cellList)
    CALL demallocA(ThisVTKMesh%nodeList)
    thisVTKMesh%numPoints=j
    CALL dmallocA(ThisVTKMesh%x,ThisVTKMesh%numPoints)
    CALL dmallocA(ThisVTKMesh%y,ThisVTKMesh%numPoints)
    CALL dmallocA(ThisVTKMesh%z,ThisVTKMesh%numPoints)
    CALL dmallocA(ThisVTKMesh%cellList,ThisVTKMesh%numCells)
    CALL dmallocA(ThisVTKMesh%nodeList,SIZE(scratchNodeList))
    thisVTKMesh%x=scratchX
    thisVTKMesh%y=scratchY
    thisVTKMesh%z=scratchZ
    thisVTKMesh%cellList=scratchCellList
    thisVTKMesh%nodeList=scratchNodeList
    
    !Clean up scratch variables
    DEALLOCATE(scratchCellList)
    DEALLOCATE(scratchNodeList)
    DEALLOCATE(scratchX)
    DEALLOCATE(scratchY)
    DEALLOCATE(scratchZ)
    DEALLOCATE(pointPresent)
    DEALLOCATE(nodePresent)
    DEALLOCATE(pointMap)
  ENDSUBROUTINE removeCells_VTKMeshType    
!
!-------------------------------------------------------------------------------
!> @brief Adds two VTK mesh objects together so that a single mesh is formed
!> @param mesh1 The first mesh to be added (cells from this mesh will be first
!>              in the cell and node lists. Keep this in mind when assigning
!>              data to the VTK file)
!> @param mesh2 The second mesh to be added
!>
!> This routine may produce a memory leak for a=a+b, but I after some initial 
!> investigation it does not appear to.
!>
  FUNCTION addMesh_VTKMesh(mesh1,mesh2) RESULT(newMesh)
    TYPE(VTKMeshType),INTENT(IN) :: mesh1,mesh2
    TYPE(VTKMeshType) :: newMesh,mesh1conv,mesh2conv
    INTEGER(SIK) :: numPoints,numCells,numNodes
    
    !Convert input meshed to VTK_UNSTRUCTURED_GRID
    mesh1conv=mesh1
    mesh2conv=mesh2
    CALL mesh1conv%convert(VTK_UNSTRUCTURED_GRID)
    CALL mesh2conv%convert(VTK_UNSTRUCTURED_GRID)
    
    !Allocate new mesh arrays
    numPoints=mesh1conv%numPoints+mesh2conv%numPoints
    numCells=mesh1conv%numCells+mesh2conv%numCells
    numNodes=SIZE(mesh1conv%nodeList)+SIZE(mesh2conv%nodeList)
    CALL dmallocA(newMesh%x,numPoints)
    CALL dmallocA(newMesh%y,numPoints)
    CALL dmallocA(newMesh%z,numPoints)
    CALL dmallocA(newMesh%cellList,numCells)
    CALL dmallocA(newMesh%nodeList,numNodes)
    newMesh%numPoints=numPoints
    newMesh%numCells=numCells
    
    !Merge the two input meshes
    newMesh%meshType=VTK_UNSTRUCTURED_GRID
    newMesh%isInit=.TRUE.
    newMesh%x(1:mesh1conv%numPoints)=mesh1conv%x
    newMesh%x(mesh1conv%numPoints+1:)=mesh2conv%x
    newMesh%y(1:mesh1conv%numPoints)=mesh1conv%y
    newMesh%y(mesh1conv%numPoints+1:)=mesh2conv%y
    newMesh%z(1:mesh1conv%numPoints)=mesh1conv%z
    newMesh%z(mesh1conv%numPoints+1:)=mesh2conv%z
    newMesh%cellList(1:mesh1conv%numCells)=mesh1conv%cellList
    newMesh%cellList(mesh1conv%numCells+1:)=mesh2conv%cellList
    newMesh%nodeList(1:SIZE(mesh1conv%nodeList))=mesh1conv%nodeList
    newMesh%nodeList(SIZE(mesh1conv%nodeList)+1:)=mesh2conv%nodeList+ &
                                                  mesh1conv%numPoints
  ENDFUNCTION addMesh_VTKMesh
!
!-------------------------------------------------------------------------------
!> @brief Closes the VTU XML file object.
!> @param file the VTU XML file object
!> @param ldel logical on whether or not to delete the file
!>
!> Closes the file by adding standard XML structure closures.
!>
    SUBROUTINE close_VTUXMLFileType(myVTKFile)
      CHARACTER(LEN=*),PARAMETER :: myName='close_VTUXMLFileType'
      CLASS(VTUXMLFileType),INTENT(INOUT) :: myVTKFile
      INTEGER(SIK) :: funit

      funit=myVTKFile%getUnitNo()
      WRITE(funit,'(a)') '      </CellData>'
      WRITE(funit,'(a)') '    </Piece>'
      WRITE(funit,'(a)') '  </UnstructuredGrid>'
      WRITE(funit,'(a)') '</VTKFile>'
    ENDSUBROUTINE close_VTUXMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Initializes the VTU XML file object.
!> @param fileobj input file object.
!> @param unit Unit number to use for the file.
!> @param status optional, used here for name of header of VTK File
!>
!> The other input arguments are not currently supported. See 
!> @ref FileType_Fortran::init_fortran_file "init_fortran_file" for a complete
!> description of all the optional input arguments.
!> 
!> This routine initializes the file object through the Fortran file type
!> interface then writes the VTU header to the file.
!>
    SUBROUTINE init_VTUXMLFileType(fileobj,unit,file,status,access,form, &
                                 position,action,pad,recl)
      CHARACTER(LEN=*),PARAMETER :: myName='init_VTUXMLFileType'
      CLASS(VTUXMLFileType),INTENT(INOUT) :: fileobj
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: unit
      CHARACTER(LEN=*),INTENT(IN) :: file
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: status
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: access
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: form
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: position
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: action
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: pad
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: recl
      
      CHARACTER(LEN=256) :: title
      
      IF(.NOT.fileobj%isInit()) THEN
        IF(PRESENT(access)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "ACCESS" is being ignored. Value is "SEQUENTIAL".')
        IF(PRESENT(form)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "FORM" is being ignored. Value is "FORMATTED".')
        IF(PRESENT(action)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "ACTION" is being ignored. Value is "WRITE".')
        IF(PRESENT(pad)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "PAD" is being ignored. Value is "YES".')
        IF(PRESENT(position)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "POSITION" is being ignored. Value is "APPEND".')
        IF(PRESENT(recl)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "RECL" is being ignored. File is "SEQUENTIAL".')
        
        !Initialize the file, only support ascii for now
        CALL init_fortran_file(fileobj,unit,file,'REPLACE','SEQUENTIAL', &
          'FORMATTED','APPEND','WRITE')
      
        !Open the file
        CALL fileobj%fopen()
      
        !Determine the file's title
        IF(PRESENT(status)) THEN
          IF(LEN_TRIM(status) > 256) THEN
            CALL fileobj%e%raiseDebug(modName//'::'//myName// &
              ' - File title name is being truncated!')
            title=status(1:256)
          ELSE
            title=TRIM(status)
          ENDIF
        ELSE
          title='vtu output'
        ENDIF
      
        !Write header to file, title and format information to file
        IF(fileobj%isOpen()) THEN
          WRITE(fileobj%getUnitNo(),'(a)') '<?xml version="1.0"?>'
          WRITE(fileobj%getUnitNo(),'(a)') '<VTKFile type="UnstructuredGrid" version="0.1" byte_order="LittleEndian">'
          !Change "new" status so that if file is closed and re-opened
          !during execution it will be appended file instead of replacing it.
          fileobj%newstat=.FALSE.
        ENDIF
      ELSE
        CALL fileobj%e%raiseError(modName//'::'//myName// &
          ' - VTU File is already initialized!')
      ENDIF
    ENDSUBROUTINE init_VTUXMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Writes a VTU mesh to VTU file
!> @param myVTKFile the VTK file to write the data to
!> @param vtkMesh the VTK mesh to write in the file
!> 
!> This routine supports writing of UNSTRUCTURED_GRIDS. The other types of 
!> geometry/topology are not yet implemented. The display of this 
!> data was verified by loading and displaying it with VisIt.
!>
    SUBROUTINE writeMesh_VTUXMLFileType(myVTKFile,vtkMesh)
      CHARACTER(LEN=*),PARAMETER :: myName='writeMesh_VTUXMLFileType'
      CLASS(VTUXMLFileType),INTENT(INOUT) :: myVTKFile
      TYPE(VTKMeshType),INTENT(IN) :: vtkMesh
      CHARACTER(LEN=256) :: aline,sint
      INTEGER(SIK) :: funit,i,j,k,n
      INTEGER(SIK),DIMENSION(:),ALLOCATABLE :: offsets
      
      IF(.NOT.myVTKFile%hasMesh) THEN
        IF(myVTKFile%isOpen()) THEN
          IF(vtkMesh%isInit) THEN
            funit=myVTKFile%getUnitNo()
            SELECTCASE(vtkMesh%meshType)
              CASE(VTK_UNSTRUCTURED_GRID)
                myVTKFile%mesh=vtkMesh
                myVTKFile%hasMesh=.TRUE.
                !Write a mesh that is an unstructured grid
                !Clean-up redundant points
                CALL myVTKFile%mesh%cleanupPoints()
                WRITE(sint,'(i64)') myVTKFile%mesh%numPoints; sint=ADJUSTL(sint)
                WRITE(aline,'(i64)') myVTKFile%mesh%numCells; aline=ADJUSTL(aline)
                WRITE(funit,'(a)') '  <UnstructuredGrid>'
                WRITE(funit,'(a)') '    <Piece NumberOfPoints="'//TRIM(sint)//'" NumberOfCells="'//TRIM(aline)//'">'
                WRITE(funit,'(a)') '      <Points>'
                WRITE(funit,'(a)') '        <DataArray type="Float32" NumberOfComponents="3" format="ascii">'
                DO i=1,myVTKFile%mesh%numPoints
                  WRITE(funit,'(a,3es17.8)') '        ',myVTKFile%mesh%x(i), &
                    myVTKFile%mesh%y(i),myVTKFile%mesh%z(i)
                ENDDO
                WRITE(funit,'(a)') '        </DataArray>'
                WRITE(funit,'(a)') '      </Points>'
                
                !Write the list of cell vertices
                WRITE(funit,'(a)') '      <Cells>'
                WRITE(funit,'(a)') '        <DataArray type="Int32" Name="connectivity" format="ascii">'
                j=1
                ALLOCATE(offsets(myVTKFile%mesh%numCells))
                DO i=1,myVTKFile%mesh%numCells
                  !Determine the next n nodes that make up this cell
                  SELECTCASE(myVTKFile%mesh%cellList(i))
                    CASE(VTK_VERTEX); n=1
                    CASE(VTK_LINE); n=2
                    CASE(VTK_TRIANGLE); n=3
                    CASE(VTK_QUAD); n=4
                    CASE(VTK_TETRA); n=4
                    CASE(VTK_HEXAHEDRON); n=8
                    CASE(VTK_VOXEL); n=8
                    CASE(VTK_WEDGE); n=6
                    CASE(VTK_PYRAMID); n=5
                    CASE(VTK_QUADRATIC_EDGE); n=3
                    CASE(VTK_QUADRATIC_TRIANGLE); n=6
                    CASE(VTK_QUADRATIC_QUAD); n=8
                    CASE(VTK_QUADRATIC_TETRA); n=10
                    CASE(VTK_QUADRATIC_HEXAHEDRON); n=20
                    CASE DEFAULT
                      CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                        ' - VTK cell type is not supported!')
                      n=0
                  ENDSELECT
                  IF(n > 0) THEN
                    WRITE(sint,'(a)') '  '; sint=ADJUSTL(sint)
                    aline=TRIM(sint)
                    DO k=0,n-1
                      WRITE(sint,'(i64)') myVTKFile%mesh%nodeList(j+k)
                      sint=ADJUSTL(sint)
                      aline=TRIM(aline)//' '//TRIM(sint)
                    ENDDO
                    WRITE(funit,'(a)') '         '//TRIM(aline)
                    j=j+n
                    offsets(i) = j-1
                  ENDIF
                ENDDO
                WRITE(funit,'(a)') '        </DataArray>'

                ! Write the offsets
                WRITE(funit,'(a)') '        <DataArray type="Int32" Name="offsets" format="ascii">'
                WRITE(funit,*) offsets
                WRITE(funit,'(a)') '        </DataArray>'
                
                !Write the list of cell types
                WRITE(funit,'(a)') '        <DataArray type="UInt8" Name="types" format="ascii">'
                WRITE(funit,*) myVTKFile%mesh%cellList
                WRITE(funit,'(a)') '        </DataArray>'
                WRITE(funit,'(a)') '      </Cells>'
                WRITE(funit,'(a)') '      <CellData>'
                
              CASE(VTK_POLYDATA)
                CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                  ' - VTK DATASET "POLYDATA" not supported!')
              CASE DEFAULT
                CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                  ' - VTK DATASET type is not recognized!')
            ENDSELECT
            FLUSH(funit)
          ELSE
            CALL myVTKFile%e%raiseError(modName//'::'//myName// &
              ' - VTU Mesh is not initialized!')
          ENDIF
        ELSE
          CALL myVTKFile%e%raiseError(modName//'::'//myName// &
            ' - VTU File is not open for writing!')
        ENDIF
      ELSE
        CALL myVTKFile%e%raiseError(modName//'::'//myName// &
          ' - VTU File already has a mesh!')
      ENDIF
    ENDSUBROUTINE writeMesh_VTUXMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Writes scalar data for a VTK mesh to VTU XML file
!> @param myVTKFile the VTK file to write the data to
!> @param vtkData the VTK data to write to the file
!> 
!> Presently, this routine is only capable of writing scalar cell data.
!> The data can be written as integers, single precision or double precision.
!> Note that in the VTKDataType data is always stored as double precision.
!>
!> Currently no check is made if the amount of data differs from the number
!> of cells in the mesh that has already been written in the file. From what
!> I understand about VTK, it does not have to, and that a default lookup table
!> will be used to obtain missing values. Adding a warning message when the
!> amount of data is not consistent should be done.
!>
    SUBROUTINE writeScalarData_VTUXMLFileType(myVTKFile,vtkData)
      CHARACTER(LEN=*),PARAMETER :: myName='writeScalarData_VTUXMLFileType'
      CLASS(VTUXMLFileType),INTENT(INOUT) :: myVTKFile
      TYPE(VTKDataType),INTENT(IN) :: vtkData
      INTEGER(SIK) :: funit,i,istp
      CHARACTER(LEN=256) :: aline,sint
      
      IF(myVTKFile%hasMesh) THEN
        IF(myVTKFile%isOpen()) THEN
          IF(vtkData%isInit) THEN
            IF(vtkData%isCellData) THEN
              IF(vtkData%dataSetType == VTK_DATA_SCALARS) THEN
                funit=myVTKFile%getUnitNo()
                aline='CELL_DATA'
                WRITE(sint,'(i64)') myVTKFile%mesh%numCells; sint=ADJUSTL(sint)
                aline=TRIM(aline)//' '//TRIM(sint)
                SELECTCASE(TRIM(vtkData%vtkDataFormat))
                  CASE('float')
                    WRITE(funit,'(a)') '        <DataArray type="Float32"'// &
                      ' Name="'//TRIM(vtkData%varname)//'" format="ascii">'
                    DO i=1,SIZE(vtkData%datalist),5
                      istp=i+4
                      IF(istp > SIZE(vtkData%datalist)) istp=SIZE(vtkData%datalist)
                      WRITE(funit,'(5f15.6)') REAL(vtkData%datalist(i:istp),SSK)
                    ENDDO
                  CASE('double')
                    WRITE(funit,'(a)') '        <DataArray type="Float64"'// &
                      ' Name="'//TRIM(vtkData%varname)//'" format="ascii">'
                    DO i=1,SIZE(vtkData%datalist),3
                      istp=i+2
                      IF(istp > SIZE(vtkData%datalist)) istp=SIZE(vtkData%datalist)
                      WRITE(funit,'(3es22.14)') vtkData%datalist(i:istp)
                    ENDDO
                  CASE('int','short','long')
                    WRITE(funit,'(a)') '        <DataArray type="Int32"'// &
                      ' Name="'//TRIM(vtkData%varname)//'" format="ascii">'
                    DO i=1,SIZE(vtkData%datalist),6
                      istp=i+5
                      IF(istp > SIZE(vtkData%datalist)) istp=SIZE(vtkData%datalist)
                      WRITE(funit,'(6i12)') NINT(vtkData%datalist(i:istp),SIK)
                    ENDDO
                  CASE DEFAULT
                    CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                      ' - Writing of "'//TRIM(vtkData%vtkDataFormat)// &
                        '" data not yet supported!')
                ENDSELECT
                WRITE(funit,'(a)') '        </DataArray>'
                FLUSH(funit)
              ELSE
                CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                ' - dataSetType is not yet supported!')
              ENDIF
            ELSE
              CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                ' - Writing of point data is not yet supported!')
            ENDIF
          ELSE
            CALL myVTKFile%e%raiseError(modName//'::'//myName// &
              ' - VTK Data is not initialized!')
          ENDIF
        ELSE
          CALL myVTKFile%e%raiseError(modName//'::'//myName// &
            ' - VTK File is not open for writing!')
        ENDIF
      ELSE
        CALL myVTKFile%e%raiseError(modName//'::'//myName// &
          ' - VTK File has no mesh to write data for!')
      ENDIF
    ENDSUBROUTINE writeScalarData_VTUXMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Initializes the VTU XML file object.
!> @param fileobj input file object.
!> @param unit Unit number to use for the file.
!> @param status optional, used here for name of header of VTK File
!>
!> The other input arguments are not currently supported. See 
!> @ref FileType_Fortran::init_fortran_file "init_fortran_file" for a complete
!> description of all the optional input arguments.
!> 
!> This routine initializes the file object through the Fortran file type
!> interface then writes the VTU header to the file.
!>
    SUBROUTINE init_PVTUXMLFileType(fileobj,unit,file,status,access,form, &
                                 position,action,pad,recl)
      CHARACTER(LEN=*),PARAMETER :: myName='init_PVTUXMLFileType'
      CLASS(PVTUXMLFileType),INTENT(INOUT) :: fileobj
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: unit
      CHARACTER(LEN=*),INTENT(IN) :: file
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: status
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: access
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: form
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: position
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: action
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: pad
      INTEGER(SIK),OPTIONAL,INTENT(IN) :: recl
      
      CHARACTER(LEN=256) :: title
      CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: temp

      fileobj%fileListLength = fileobj%fileListLength + 1

      IF (.NOT.ALLOCATED(fileobj%fileList)) THEN
        ALLOCATE(fileobj%fileList(fileobj%fileListLength))
        fileobj%fileList = file
      ELSE
        ALLOCATE(temp(fileobj%fileListLength))
        temp(1:fileobj%fileListLength - 1) = fileobj%fileList
        temp(fileobj%fileListLength) = file
        DEALLOCATE(fileobj%fileList)
        ALLOCATE(fileobj%fileList(fileobj%fileListLength))
        fileobj%fileList = temp
        DEALLOCATE(temp)
      ENDIF
      
      IF(.NOT.fileobj%isInit()) THEN
        IF(PRESENT(access)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "ACCESS" is being ignored. Value is "SEQUENTIAL".')
        IF(PRESENT(form)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "FORM" is being ignored. Value is "FORMATTED".')
        IF(PRESENT(action)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "ACTION" is being ignored. Value is "WRITE".')
        IF(PRESENT(pad)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "PAD" is being ignored. Value is "YES".')
        IF(PRESENT(position)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "POSITION" is being ignored. Value is "APPEND".')
        IF(PRESENT(recl)) CALL fileobj%e%raiseDebug(modName//'::'//myName// &
          ' - Optional input "RECL" is being ignored. File is "SEQUENTIAL".')
        
        !Initialize the file, only support ascii for now
        CALL init_fortran_file(fileobj,unit,file,'REPLACE','SEQUENTIAL', &
          'FORMATTED','APPEND','WRITE')
      
        !Open the file
        CALL fileobj%fopen()
      
        !Determine the file's title
        IF(PRESENT(status)) THEN
          IF(LEN_TRIM(status) > 256) THEN
            CALL fileobj%e%raiseDebug(modName//'::'//myName// &
              ' - File title name is being truncated!')
            title=status(1:256)
          ELSE
            title=TRIM(status)
          ENDIF
        ELSE
          title='vtu output'
        ENDIF
      
        !Write header to file, title and format information to file
        IF(fileobj%isOpen()) THEN
          WRITE(fileobj%getUnitNo(),'(a)') '<?xml version="1.0"?>'
          WRITE(fileobj%getUnitNo(),'(a)') '<VTKFile type='// &
          '"UnstructuredGrid" version="0.1" byte_order="LittleEndian">'
          !Change "new" status so that if file is closed and re-opened
          !during execution it will be appended file instead of replacing it.
          fileobj%newstat=.FALSE.
        ENDIF
      ELSE
        CALL fileobj%e%raiseError(modName//'::'//myName// &
          ' - VTU File is already initialized!')
      ENDIF
    ENDSUBROUTINE init_PVTUXMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Writes scalar data for a VTK mesh to VTU XML file
!> @param myVTKFile the VTK file to write the data to
!> @param vtkData the VTK data to write to the file
!> 
!> Presently, this routine is only capable of writing scalar cell data.
!> The data can be written as integers, single precision or double precision.
!> Note that in the VTKDataType data is always stored as double precision.
!>
!> Currently no check is made if the amount of data differs from the number
!> of cells in the mesh that has already been written in the file. From what
!> I understand about VTK, it does not have to, and that a default lookup table
!> will be used to obtain missing values. Adding a warning message when the
!> amount of data is not consistent should be done.
!>
    SUBROUTINE writeScalarData_PVTUXMLFileType(myVTKFile,vtkData)
      CHARACTER(LEN=*),PARAMETER :: myName='writeScalarData_PVTUXMLFileType'
      CLASS(PVTUXMLFileType),INTENT(INOUT) :: myVTKFile
      TYPE(VTKDataType),INTENT(IN) :: vtkData
      INTEGER(SIK) :: funit,i,istp
      CHARACTER(LEN=256) :: aline,sint

      CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: temp

      myVTKFile%varNameListLength = myVTKFile%varNameListLength + 1

      IF (.NOT.ALLOCATED(myVTKFile%varNameList)) THEN
        ALLOCATE(myVTKFile%varNameList(myVTKFile%varNameListLength))
        myVTKFile%varNameList = vtkData%varname
      ELSE
        ALLOCATE(temp(myVTKFile%varNameListLength))
        temp(1:myVTKFile%varNameListLength - 1) = myVTKFile%varNameList
        temp(myVTKFile%varNameListLength) = vtkData%varname
        DEALLOCATE(myVTKFile%varNameList)
        ALLOCATE(myVTKFile%varNameList(myVTKFile%varNameListLength))
        myVTKFile%varNameList = temp
        DEALLOCATE(temp)
      ENDIF

      myVTKFile%dataFormatListLength = myVTKFile%dataFormatListLength + 1

      IF (.NOT.ALLOCATED(myVTKFile%dataFormatList)) THEN
        ALLOCATE(myVTKFile%dataFormatList(myVTKFile%dataFormatListLength))
        myVTKFile%dataFormatList = vtkData%vtkDataFormat
      ELSE
        ALLOCATE(temp(myVTKFile%dataFormatListLength))
        temp(1:myVTKFile%dataFormatListLength - 1) = myVTKFile%dataFormatList
        temp(myVTKFile%dataFormatListLength) = vtkData%vtkDataFormat
        DEALLOCATE(myVTKFile%dataFormatList)
        ALLOCATE(myVTKFile%dataFormatList(myVTKFile%dataFormatListLength))
        myVTKFile%dataFormatList = temp
        DEALLOCATE(temp)
      ENDIF
      
      IF(myVTKFile%hasMesh) THEN
        IF(myVTKFile%isOpen()) THEN
          IF(vtkData%isInit) THEN
            IF(vtkData%isCellData) THEN
              IF(vtkData%dataSetType == VTK_DATA_SCALARS) THEN
                funit=myVTKFile%getUnitNo()
                aline='CELL_DATA'
                WRITE(sint,'(i64)') myVTKFile%mesh%numCells; sint=ADJUSTL(sint)
                aline=TRIM(aline)//' '//TRIM(sint)
                SELECTCASE(TRIM(vtkData%vtkDataFormat))
                  CASE('float')
                    WRITE(funit,'(a)') '        <DataArray type="Float32"'// &
                      ' Name="'//TRIM(vtkData%varname)//'" format="ascii">'
                    DO i=1,SIZE(vtkData%datalist),5
                      istp=i+4
                      IF(istp > SIZE(vtkData%datalist)) istp=SIZE(vtkData%datalist)
                      WRITE(funit,'(5f15.6)') REAL(vtkData%datalist(i:istp),SSK)
                    ENDDO
                  CASE('double')
                    WRITE(funit,'(a)') '        <DataArray type="Float64"'// &
                      ' Name="'//TRIM(vtkData%varname)//'" format="ascii">'
                    DO i=1,SIZE(vtkData%datalist),3
                      istp=i+2
                      IF(istp > SIZE(vtkData%datalist)) istp=SIZE(vtkData%datalist)
                      WRITE(funit,'(3es22.14)') vtkData%datalist(i:istp)
                    ENDDO
                  CASE('int','short','long')
                    WRITE(funit,'(a)') '        <DataArray type="Int32"'// &
                      ' Name="'//TRIM(vtkData%varname)//'" format="ascii">'
                    DO i=1,SIZE(vtkData%datalist),6
                      istp=i+5
                      IF(istp > SIZE(vtkData%datalist)) istp=SIZE(vtkData%datalist)
                      WRITE(funit,'(6i12)') NINT(vtkData%datalist(i:istp),SIK)
                    ENDDO
                  CASE DEFAULT
                    CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                      ' - Writing of "'//TRIM(vtkData%vtkDataFormat)// &
                        '" data not yet supported!')
                ENDSELECT
                WRITE(funit,'(a)') '        </DataArray>'
                FLUSH(funit)
              ELSE
                CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                ' - dataSetType is not yet supported!')
              ENDIF
            ELSE
              CALL myVTKFile%e%raiseError(modName//'::'//myName// &
                ' - Writing of point data is not yet supported!')
            ENDIF
          ELSE
            CALL myVTKFile%e%raiseError(modName//'::'//myName// &
              ' - VTK Data is not initialized!')
          ENDIF
        ELSE
          CALL myVTKFile%e%raiseError(modName//'::'//myName// &
            ' - VTK File is not open for writing!')
        ENDIF
      ELSE
        CALL myVTKFile%e%raiseError(modName//'::'//myName// &
          ' - VTK File has no mesh to write data for!')
      ENDIF
    ENDSUBROUTINE writeScalarData_PVTUXMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Closes the VTU XML file object.
!> @param file the VTU XML file object
!> @param ldel logical on whether or not to delete the file
!>
!> Writes the pvtu file for visualizing in parallel.
!>
    SUBROUTINE writepvtu_PVTUXMLFileType(fileobj,funit,filen,procs)
      CHARACTER(LEN=*),PARAMETER :: myName='writepvtu_PVTUXMLFileType'
      CLASS(PVTUXMLFileType),INTENT(INOUT) :: fileobj
      CHARACTER(LEN=*),INTENT(IN) :: filen
      INTEGER(SIK),INTENT(IN) :: procs
      CHARACTER(LEN=128) :: sint
      INTEGER(SIK),INTENT(IN) :: funit
      INTEGER(SIK) :: i,j,iord
      CHARACTER(LEN=128) :: fname,fmtStr

      OPEN(unit=funit,file=TRIM(filen)//'.pvtu')

      WRITE(funit,'(a)') '<?xml version="1.0"?>'
      WRITE(funit,'(a)') '<VTKFile type="PUnstructuredGrid" version="0.1" byte_order="LittleEndian">'
      WRITE(funit,'(a)') '  <PUnstructuredGrid GhostLevel="0">'

      WRITE(funit,'(a)') '    <PPoints>'

      WRITE(funit,'(a)') '      <PDataArray type="Float32"'// &
        ' NumberOfComponents="3"/>'

      WRITE(funit,'(a)') '    </PPoints>'
      WRITE(funit,'(a)') '    <PCells>'
      WRITE(funit,'(a)') '      <PDataArray type="Int32" Name="connectivity"/>'
      WRITE(funit,'(a)') '      <PDataArray type="Int32" Name="offsets"/>'
      WRITE(funit,'(a)') '      <PDataArray type="UInt8" Name="types"/>'
      WRITE(funit,'(a)') '    </PCells>'

      WRITE(funit,'(a)') '    <PCellData Scalars="Data">'

      DO i=1,fileobj%varNameListLength
        SELECTCASE(TRIM(fileobj%dataFormatList(i)))
          CASE('float')
            WRITE(funit,'(a)') '      <PDataArray type="Float32"'// &
              ' Name="'//TRIM(fileobj%varNameList(i))//'"/>'
          CASE('double')
            WRITE(funit,'(a)') '      <PDataArray type="Float64"'// &
              ' Name="'//TRIM(fileobj%varNameList(i))//'"/>'
          CASE('int','short','long')
            WRITE(funit,'(a)') '      <PDataArray type="Int32"'// &
              ' Name="'//TRIM(fileobj%varNameList(i))//'"/>'
          CASE DEFAULT
            CALL fileobj%e%raiseError(modName//'::'//myName// &
              ' - Writing of "'//TRIM(fileobj%dataFormatList(i))// &
                '" data not yet supported!')
        ENDSELECT
      ENDDO

      WRITE(funit,'(a)') '    </PCellData>'

      DO i=1,procs
        iord=procs-1
        j=1
        DO WHILE(iord >= 10)
          j=j+1
          iord=iord/10
        ENDDO
        WRITE(fmtStr,'(a,i2.2,a,i2.2,a)') '(i',j,'.',j,')'; fmtSTR=ADJUSTL(fmtStr)
        WRITE(sint,FMT=TRIM(fmtStr)) i-1
        fname=TRIM(filen)//'_'//TRIM(sint)
        WRITE(funit,'(a)') '    <Piece Source="'//TRIM(fname)//'.vtu'//'"/>'
      ENDDO
      WRITE(funit,'(a)') '  </PUnstructuredGrid>'
      WRITE(funit,'(a)') '</VTKFile>'

      CLOSE(funit)
    ENDSUBROUTINE writepvtu_PVTUXMLFileType
!
ENDMODULE VTKFiles