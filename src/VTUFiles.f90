!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module for working with VTU Files.
!>
!> This module provides derived types for VTU Files, which are XML-based.
!> These files only support datasets of the UNSTRUCTURED_GRID type.
!>
!> A few notes.
!>  - To properly visualize output files in VisIt, open the .pvtu file rather
!>    than the "database".
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref FileType_Fortran "FileType_Fortran": @copybrief FileType_Fortran
!>  - @ref VTKFiles "VTKFiles": @copybrief VTKFiles
!>
!> @par EXAMPLES
!> @code
!> @endcode
!>
!> @author Zackary Dodson, Brendan Kochunas
!>   @date 05/10/2017
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE VTUFiles
  USE IntrType
  USE FileType_Fortran
  USE VTKFiles
  IMPLICIT NONE
  PRIVATE
  !
  !List of Public Members
  PUBLIC :: VTUXMLFileType
  !
  !> Module name for error messages
  CHARACTER(LEN=*),PARAMETER :: modName='VTUFILES'
  
  !> @brief Derived type object for files definable by the VTU XML format
  !>
  !> This is an extension of the @ref FileType_Fortran "VTKLegFileType".
  TYPE,EXTENDS(VTKLegFileType) :: VTUXMLFileType
  !
  CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: dataFormatList
  INTEGER(SIK) :: dataFormatListLength = 0
  CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: varNameList
  INTEGER(SIK) :: varNameListLength = 0
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
      !> @copybrief VTUFiles::hasData_VTUXMLFileType
      !> @copydetails VTUFiles::hasData_VTUXMLFileType
      PROCEDURE,PASS :: hasData => hasData_VTUXMLFileType
      !> @copybrief VTUFiles::close_VTUXMLFileType
      !> @copydetails VTUFiles::close_VTUXMLFileType
      PROCEDURE,PASS :: close => close_VTUXMLFileType
      !> @copybrief VTUFiles::writepvtu_PVTUXMLFileType
      !> @copydetails VTUFiles::writepvtu_PVTUXMLFileType
      PROCEDURE,PASS :: writepvtu => writepvtu_VTUXMLFileType
  ENDTYPE VTUXMLFileType
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the VTU XML file object.
!> @param fileobj input file object.
!> @param unit Unit number to use for the file.
!> @param status optional, not used for VTU files.
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
      !
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
        !
        !Initialize the file, only support ascii for now
        CALL init_fortran_file(fileobj,unit,file,'REPLACE','SEQUENTIAL', &
          'FORMATTED','APPEND','WRITE')
        !
        !Open the file
        CALL fileobj%fopen()
        !
        !Handle status argument, which is unused
        IF(PRESENT(status)) THEN
          CALL fileobj%e%raiseDebug(modName//'::'//myName// &
            ' - File header not used for VTU files!')
        ENDIF
        !
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
      !
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
                !
                ! Write the offsets
                WRITE(funit,'(a)') '        <DataArray type="Int32" Name="offsets" format="ascii">'
                WRITE(funit,*) offsets
                WRITE(funit,'(a)') '        </DataArray>'
                DEALLOCATE(offsets)
                !
                !Write the list of cell types
                WRITE(funit,'(a)') '        <DataArray type="UInt8" Name="types" format="ascii">'
                WRITE(funit,*) myVTKFile%mesh%cellList
                WRITE(funit,'(a)') '        </DataArray>'
                WRITE(funit,'(a)') '      </Cells>'
                WRITE(funit,'(a)') '      <CellData>'
                !
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
      CHARACTER(LEN=256),ALLOCATABLE,DIMENSION(:) :: temp
      !
      myVTKFile%varNameListLength=myVTKFile%varNameListLength+1
      IF (.NOT.ALLOCATED(myVTKFile%varNameList)) THEN
        ALLOCATE(myVTKFile%varNameList(myVTKFile%varNameListLength))
        myVTKFile%varNameList=vtkData%varname
      ELSE
        ALLOCATE(temp(myVTKFile%varNameListLength))
        temp(1:myVTKFile%varNameListLength-1)=myVTKFile%varNameList
        temp(myVTKFile%varNameListLength)=vtkData%varname
        DEALLOCATE(myVTKFile%varNameList)
        ALLOCATE(myVTKFile%varNameList(myVTKFile%varNameListLength))
        myVTKFile%varNameList=temp
        DEALLOCATE(temp)
      ENDIF
      !
      myVTKFile%dataFormatListLength=myVTKFile%dataFormatListLength+1
      IF (.NOT.ALLOCATED(myVTKFile%dataFormatList)) THEN
        ALLOCATE(myVTKFile%dataFormatList(myVTKFile%dataFormatListLength))
        myVTKFile%dataFormatList = vtkData%vtkDataFormat
      ELSE
        ALLOCATE(temp(myVTKFile%dataFormatListLength))
        temp(1:myVTKFile%dataFormatListLength-1)=myVTKFile%dataFormatList
        temp(myVTKFile%dataFormatListLength)=vtkData%vtkDataFormat
        DEALLOCATE(myVTKFile%dataFormatList)
        ALLOCATE(myVTKFile%dataFormatList(myVTKFile%dataFormatListLength))
        myVTKFile%dataFormatList=temp
        DEALLOCATE(temp)
      ENDIF
      !
      IF(myVTKFile%hasMesh) THEN
        IF(myVTKFile%isOpen()) THEN
          IF(vtkData%isInit) THEN
            IF(vtkData%isCellData) THEN
              IF(vtkData%dataSetType==VTK_DATA_SCALARS) THEN
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
!> @brief Checks if fileobj has a given data set.
!> @param fileobj input file object.
!> @param varName Name of the variable to check.
!> @param varFormat Data format of the data to check.
!> @param bool Whether or not the data is present.
!>
!> Checks if the VTU File has the specified data name and type.
!>
    SUBROUTINE hasData_VTUXMLFileType(fileobj,varName,varFormat,bool)
      CHARACTER(LEN=*),PARAMETER :: myName='hasData_VTUXMLFileType'
      CLASS(VTUXMLFileType),INTENT(INOUT) :: fileobj
      CHARACTER(LEN=*),INTENT(IN) :: varName,varFormat
      LOGICAL(SBK),INTENT(INOUT) :: bool
      INTEGER(SIK) :: i
      !
      bool=.FALSE.
      DO i=1,fileobj%varNameListLength
        IF (varName==fileobj%varNameList(i).AND. &
          varFormat==fileobj%dataFormatList(i)) bool=.TRUE.
      ENDDO
    ENDSUBROUTINE hasData_VTUXMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Closes the VTU XML file object.
!> @param myVTKFile the VTU XML file object
!>
!> Closes the file by adding standard XML structure closures.
!>
    SUBROUTINE close_VTUXMLFileType(myVTKFile)
      CHARACTER(LEN=*),PARAMETER :: myName='close_VTUXMLFileType'
      CLASS(VTUXMLFileType),INTENT(INOUT) :: myVTKFile
      INTEGER(SIK) :: funit
      !
      funit=myVTKFile%getUnitNo()
      WRITE(funit,'(a)') '      </CellData>'
      WRITE(funit,'(a)') '    </Piece>'
      WRITE(funit,'(a)') '  </UnstructuredGrid>'
      WRITE(funit,'(a)') '</VTKFile>'
    ENDSUBROUTINE close_VTUXMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Creates pvtu file for VTU XML file object.
!> @param fileobj input file object.
!> @param funit Unit number to use for the file.
!> @param filen Name of the pvtu file.
!> @param procs Number of processors in MPI environment.
!> @param rank MPI rank.
!>
!> Writes the pvtu file for visualizing in parallel (only if master rank and
!> the number of processors is greater than one).
!>
    SUBROUTINE writepvtu_VTUXMLFileType(fileobj,funit,filen,procs,rank)
      CHARACTER(LEN=*),PARAMETER :: myName='writepvtu_VTUXMLFileType'
      CLASS(VTUXMLFileType),INTENT(INOUT) :: fileobj
      CHARACTER(LEN=*),INTENT(IN) :: filen
      CHARACTER(LEN=128) :: sint
      INTEGER(SIK),INTENT(IN) :: funit,procs,rank
      INTEGER(SIK) :: i,j,iord
      CHARACTER(LEN=128) :: fname,fmtStr
      !
      IF((procs>1).AND.rank==0) THEN
        OPEN(unit=funit,file=TRIM(filen)//'.pvtu')
        !
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
        !
        WRITE(funit,'(a)') '    <PCellData Scalars="Data">'
        DO i=1,fileobj%dataFormatListLength
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
        !
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
      ENDIF
      !
      DEALLOCATE(fileobj%varNameList)
      DEALLOCATE(fileobj%dataFormatList)
    ENDSUBROUTINE writepvtu_VTUXMLFileType
!
ENDMODULE VTUFiles