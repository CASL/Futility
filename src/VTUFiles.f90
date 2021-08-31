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
!> @par EXAMPLES
!> @code
!> @endcode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE VTUFiles
USE IntrType
USE FileType_Fortran
USE VTKFiles
USE Strings
USE IO_Strings
IMPLICIT NONE
PRIVATE
!
!List of Public Members
PUBLIC :: VTUXMLFileType
!
!> Module name for error messages
CHARACTER(LEN=*),PARAMETER :: modName='VTUFILES'
!
!> @brief Derived type object for files definable by the VTU XML format
!>
!> This is an extension of the @ref FileType_Fortran "VTKLegFileType".
TYPE,EXTENDS(VTKLegFileType) :: VTUXMLFileType
!
TYPE(StringType),ALLOCATABLE,DIMENSION(:) :: dataFormatList,varNameList, &
    fileList
INTEGER(SIK) :: numDataSet=0,numFiles=0
!
!List of type bound procedures (methods) for the VTU XML File type
  CONTAINS
    !> @copybrief VTUFiles::init_VTUXMLFileType
    !> @copydetails VTUFiles::init_VTUXMLFileType
    PROCEDURE,PASS :: initialize => init_VTUXMLFileType
    !> @copybrief VTUXMLes::clear_VTUXMLFileType
    !> @copydetails VTUXMLes::clear_VTUXMLFileType
    PROCEDURE,PASS :: clear => clear_VTUXMLFileType
    !> @copybrief VTUFiles::writeMesh_VTUXMLFileType
    !> @copydetails VTUFiles::writeMesh_VTUXMLFileType
    PROCEDURE,PASS :: writeMesh => writeMesh_VTUXMLFileType
    !> @copybrief VTUFiles::writeScalarData_VTUXMLFileType
    !> @copydetails VTUFiles::writeScalarData_VTUXMLFileType
    PROCEDURE,PASS :: writeScalarData => writeScalarData_VTUXMLFileType
    !> @copybrief VTUFiles::hasData_VTUXMLFileType
    !> @copydetails VTUFiles::hasData_VTUXMLFileType
    PROCEDURE,PASS :: hasData => hasData_VTUXMLFileType
    !> @copybrief VTUFiles::hasFile_VTUXMLFileType
    !> @copydetails VTUFiles::hasFile_VTUXMLFileType
    PROCEDURE,PASS :: hasFile => hasFile_VTUXMLFileType
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
    IF(PRESENT(position)) CALL fileobj%e%raiseDebug(modName//'::'// &
        myName//' - Optional input "POSITION" is being ignored. Value '// &
        'is "APPEND".')
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
    fileobj%numDataSet=0
  ELSE
    CALL fileobj%e%raiseError(modName//'::'//myName// &
        ' - VTU File is already initialized!')
  ENDIF
ENDSUBROUTINE init_VTUXMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Clears the VTU XML file object.
!> @param file the VTU XML file object
!> @param ldel logical on whether or not to delete the file
!>
!> Clears the file object using the @ref FileType_Fortran::clear_fortran_file
!> "clear_fortran_file" interface. This is why @c ldel is an argument. In
!> practice one probably does not want to delete these files before the program
!> exits.
!>
SUBROUTINE clear_VTUXMLFileType(file,ldel)
  CLASS(VTUXMLFileType),INTENT(INOUT) :: file
  LOGICAL(SBK),OPTIONAL,INTENT(IN) :: ldel
  LOGICAL(SBK) :: bool
  IF(file%hasMesh) CALL file%mesh%clear()
  file%hasMesh=.FALSE.
  bool=.FALSE.
  IF(PRESENT(ldel)) bool=ldel
  CALL clear_fortran_file(file,bool)
  IF(ALLOCATED(file%fileList)) DEALLOCATE(file%fileList)
  file%numFiles=0
ENDSUBROUTINE clear_VTUXMLFileType
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
  TYPE(StringType) :: aline,sint
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
          sint=myVTKFile%mesh%numPoints
          aline=myVTKFile%mesh%numCells
          WRITE(funit,'(a)') '  <UnstructuredGrid>'
          IF(ALLOCATED(myVTKFile%mesh%fieldData)) THEN
            n=SIZE(myVTKFile%mesh%fieldData)
            WRITE(funit,'(a)') '    <FieldData>'
            WRITE(funit,'(a)') '      <DataArray type="Int32" Name="MaterialIds" NumberOfTuples="'// &
                str(n)//'" format="ascii">'
            i=0
            DO WHILE(i < n)
              i=i+1
              IF(MOD(i,10) == 1) WRITE(funit,'(a)',ADVANCE='NO') '       '
              WRITE(funit,'(a,i0)',ADVANCE='NO') " ",myVTKFile%mesh%fieldData(i)
              IF(MOD(i,10) == 0 .OR. i == n) WRITE(funit,'(a)',ADVANCE='YES') ""
            ENDDO
            WRITE(funit,'(a)') '      </DataArray>'
            WRITE(funit,'(a)') '    </FieldData>'
          ENDIF
          WRITE(funit,'(a)') '    <Piece NumberOfPoints="'//TRIM(sint)// &
              '" NumberOfCells="'//TRIM(aline)//'">'
          WRITE(funit,'(a)') '      <Points>'
          WRITE(funit,'(a)') '        <DataArray type="Float32"'// &
              ' NumberOfComponents="3" format="ascii">'
          DO i=1,myVTKFile%mesh%numPoints
            WRITE(funit,'(a,3es17.8)') '        ',myVTKFile%mesh%x(i), &
                myVTKFile%mesh%y(i),myVTKFile%mesh%z(i)
          ENDDO
          WRITE(funit,'(a)') '        </DataArray>'
          WRITE(funit,'(a)') '      </Points>'

          !Write the list of cell vertices
          WRITE(funit,'(a)') '      <Cells>'
          WRITE(funit,'(a)') '        <DataArray type="Int32" Name="'// &
              'connectivity" format="ascii">'
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
              aline='  '
              DO k=0,n-1
                sint=myVTKFile%mesh%nodeList(j+k)
                aline=aline//' '//sint
              ENDDO
              WRITE(funit,'(a)') '         '//TRIM(aline)
              j=j+n
              offsets(i)=j-1
            ENDIF
          ENDDO
          WRITE(funit,'(a)') '        </DataArray>'
          !
          ! Write the offsets
          WRITE(funit,'(a)') '        <DataArray type="Int32" Name="'// &
              'offsets" format="ascii">'
          WRITE(funit,*) offsets
          WRITE(funit,'(a)') '        </DataArray>'
          DEALLOCATE(offsets)
          !
          !Write the list of cell types
          WRITE(funit,'(a)') '        <DataArray type="UInt8" Name="'// &
              'types" format="ascii">'
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
  !
  !Increment data set number
  myVTKFile%numDataSet=myVTKFile%numDataSet+1
  !Append data set info to lists
  CALL str_append(myVTKFile%varNameList,vtkData%varname)
  CALL str_append(myVTKFile%dataFormatList,vtkData%vtkDataFormat)
  !
  IF(myVTKFile%hasMesh) THEN
    IF(myVTKFile%isOpen()) THEN
      IF(vtkData%isInit) THEN
        IF(vtkData%isCellData) THEN
          IF(vtkData%dataSetType==VTK_DATA_SCALARS) THEN
            funit=myVTKFile%getUnitNo()
            SELECTCASE(TRIM(vtkData%vtkDataFormat))
            CASE('float')
              WRITE(funit,'(a)') '        <DataArray type="Float32"'// &
                  ' Name="'//TRIM(vtkData%varname)//'" format="ascii">'
              DO i=1,SIZE(vtkData%datalist),5
                istp=i+4
                IF(istp > SIZE(vtkData%datalist)) istp= &
                    SIZE(vtkData%datalist)
                WRITE(funit,'(5f15.6)') REAL(vtkData%datalist(i:istp),SSK)
              ENDDO
            CASE('double')
              WRITE(funit,'(a)') '        <DataArray type="Float64"'// &
                  ' Name="'//TRIM(vtkData%varname)//'" format="ascii">'
              DO i=1,SIZE(vtkData%datalist),3
                istp=i+2
                IF(istp > SIZE(vtkData%datalist)) istp= &
                    SIZE(vtkData%datalist)
                WRITE(funit,'(3es22.14)') vtkData%datalist(i:istp)
              ENDDO
            CASE('int','short','long')
              WRITE(funit,'(a)') '        <DataArray type="Int32"'// &
                  ' Name="'//TRIM(vtkData%varname)//'" format="ascii">'
              DO i=1,SIZE(vtkData%datalist),6
                istp=i+5
                IF(istp > SIZE(vtkData%datalist)) istp= &
                    SIZE(vtkData%datalist)
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
  CLASS(VTUXMLFileType),INTENT(INOUT) :: fileobj
  CHARACTER(LEN=*),INTENT(IN) :: varName,varFormat
  LOGICAL(SBK),INTENT(INOUT) :: bool
  INTEGER(SIK) :: i
  !
  bool=.FALSE.
  IF(ALLOCATED(fileobj%varNameList)) THEN
    DO i=1,fileobj%numDataSet
      IF (varName == fileobj%varNameList(i).AND. &
          varFormat == fileobj%dataFormatList(i)) bool=.TRUE.
    ENDDO
  ENDIF
ENDSUBROUTINE hasData_VTUXMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Checks if fileobj has a given file.
!> @param fileobj input file object.
!> @param fname Name of the file to check.
!> @param bool Whether or not the data is present.
!>
!> Checks if the VTU File has the specified file (which is written to pvtu).
!>
SUBROUTINE hasFile_VTUXMLFileType(fileobj,fname,bool)
  CLASS(VTUXMLFileType),INTENT(INOUT) :: fileobj
  CHARACTER(LEN=*),INTENT(IN) :: fname
  LOGICAL(SBK),INTENT(INOUT) :: bool
  INTEGER(SIK) :: i
  !
  bool=.FALSE.
  IF(ALLOCATED(fileobj%fileList)) THEN
    DO i=1,fileobj%numFiles
      IF(fname == fileobj%fileList(i)) THEN
        bool=.TRUE.
        EXIT
      ENDIF
    ENDDO
  ENDIF
ENDSUBROUTINE hasFile_VTUXMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Closes the VTU XML file object.
!> @param myVTKFile the VTU XML file object
!>
!> Closes the file by adding standard XML structure closures.
!>
SUBROUTINE close_VTUXMLFileType(myVTKFile)
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
SUBROUTINE writepvtu_VTUXMLFileType(fileobj,funit,case,filen,procs,rank)
  CHARACTER(LEN=*),PARAMETER :: myName='writepvtu_VTUXMLFileType'
  CLASS(VTUXMLFileType),INTENT(INOUT) :: fileobj
  CHARACTER(LEN=*),INTENT(IN) :: case,filen
  CHARACTER(LEN=128) :: sint
  INTEGER(SIK),INTENT(IN) :: funit,procs,rank
  INTEGER(SIK) :: i,j,iord
  CHARACTER(LEN=128) :: fname,fmtStr
  !
  IF(rank == 0) THEN
    sint='fsr_'//TRIM(case)//'/'
    CALL SlashRep(sint)
    OPEN(unit=funit,file=TRIM(sint)//TRIM(filen)//'.pvtu')
    !
    WRITE(funit,'(a)') '<?xml version="1.0"?>'
    WRITE(funit,'(a)') '<VTKFile type="PUnstructuredGrid" version="'// &
        '0.1" byte_order="LittleEndian">'
    WRITE(funit,'(a)') '  <PUnstructuredGrid GhostLevel="0">'
    WRITE(funit,'(a)') '    <PPoints>'
    WRITE(funit,'(a)') '      <PDataArray type="Float32"'// &
        ' NumberOfComponents="3"/>'
    WRITE(funit,'(a)') '    </PPoints>'
    WRITE(funit,'(a)') '    <PCells>'
    WRITE(funit,'(a)') '      <PDataArray type="Int32" Name="'// &
        'connectivity"/>'
    WRITE(funit,'(a)') '      <PDataArray type="Int32" Name="offsets"/>'
    WRITE(funit,'(a)') '      <PDataArray type="UInt8" Name="types"/>'
    WRITE(funit,'(a)') '    </PCells>'
    !
    WRITE(funit,'(a)') '    <PCellData Scalars="Data">'
    DO i=1,fileobj%numDataSet
      SELECTCASE(TRIM(CHAR(fileobj%dataFormatList(i))))
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
    ALLOCATE(fileobj%fileList(procs))
    fileobj%numFiles=procs
    DO i=1,procs
      iord=procs-1
      j=1
      DO WHILE(iord >= 10)
        j=j+1
        iord=iord/10
      ENDDO
      WRITE(fmtStr,'(a,i2.2,a,i2.2,a)') '(i',j,'.',j,')'
      fmtSTR=ADJUSTL(fmtStr)
      WRITE(sint,FMT=TRIM(fmtStr)) i-1
      fname=TRIM(filen)//'_'//TRIM(sint)//'.vtu'
      fileobj%fileList(i)=fname
      sint='DOMAIN_'//TRIM(sint)//'/'
      fname=TRIM(sint)//TRIM(fname)
      WRITE(funit,'(a)') '    <Piece Source="'//TRIM(fname)//'"/>'
    ENDDO
    WRITE(funit,'(a)') '  </PUnstructuredGrid>'
    WRITE(funit,'(a)') '</VTKFile>'
    CLOSE(funit)
  ENDIF
  !
  DEALLOCATE(fileobj%varNameList)
  DEALLOCATE(fileobj%dataFormatList)
  fileobj%numDataSet=0
ENDSUBROUTINE writepvtu_VTUXMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Appends a StringType to an array of StringTypes.
!> @param str_list array of StringTypes
!> @param str StringType to be added to array
!>
SUBROUTINE str_append(str_list,str)
  TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: str_list(:)
  CHARACTER(LEN=*),INTENT(IN) :: str
  TYPE(StringType),ALLOCATABLE :: temp(:)

  IF(ALLOCATED(str_list)) THEN
    ALLOCATE(temp(SIZE(str_list)+1))
    temp(1:SIZE(str_list))=str_list
    temp(SIZE(temp))=str
    CALL MOVE_ALLOC(temp,str_list)
  ELSE
    ALLOCATE(str_list(1))
    str_list=str
  ENDIF
ENDSUBROUTINE str_append
!
ENDMODULE VTUFiles
