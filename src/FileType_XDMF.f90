!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief 
!>
!> 
!> 
!> 
!> 
!> 
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FileType_XDMF
#include "Futility_DBC.h"
USE ExceptionHandler
USE Futility_DBC
USE ISO_FORTRAN_ENV
USE IntrType
USE Strings
!USE IO_Strings
USE FileType_Base
USE FileType_XML
USE FileType_HDF5
USE VTKFiles
USE ParameterLists

IMPLICIT NONE
PRIVATE

PUBLIC :: XDMFFileType

!> The module name
CHARACTER(LEN=*),PARAMETER :: modName='FILETYPE_XDMF'

!> Exception handler for the module
TYPE(ExceptionHandlerType),SAVE :: eXDMF

!> The XDMF File type
TYPE,EXTENDS(BaseFileType) :: XDMFFileType
!
!List of type bound procedures
  CONTAINS
    PROCEDURE,PASS :: fopen => fopen_XDMFFileType
    !> @copybrief FileType_XML::fclose_XDMFFileType
    !> @copydoc FileType_XML::fclose_XDMFFileType
    PROCEDURE,PASS :: fclose => fclose_XDMFFileType
    !> @copybrief FileType_XML::fdelete_XDMFFileType
    !> @copydoc FileType_XML::fdelete_XDMFFileType
    PROCEDURE,PASS :: fdelete => fdelete_XDMFFileType
    !> @copybrief FileType_XML::importFromDisk_XDMFFileType
    !> @copydoc FileType_XML::importFromDisk_XDMFFileType
    PROCEDURE,PASS :: importFromDiskToVTK => importFromDiskToVTK_XDMFFileType
ENDTYPE XDMFFileType

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Opens the XDMF file type for I/O
!> @param file the XDMF file type object
!>
!> @todo fix how the unit number is set
!>
SUBROUTINE fopen_XDMFFileType(file)
  CHARACTER(LEN=*),PARAMETER :: myName='fopen_XDMFFileType'
  CLASS(XDMFFileType),INTENT(INOUT) :: file
  LOGICAL(SBK) :: lopen
  INTEGER(SIK) :: funit,ierr
  TYPE(StringType) :: fname

ENDSUBROUTINE fopen_XDMFFileType
!
!-------------------------------------------------------------------------------
!> @brief Closes the XDMF file object
!> @param file the XDMF file object
!>
SUBROUTINE fclose_XDMFFileType(file)
  CHARACTER(LEN=*),PARAMETER :: myName='fclose_XDMFFileType'
  CLASS(XDMFFileType),INTENT(INOUT) :: file
  INTEGER(SIK) :: ierr
ENDSUBROUTINE fclose_XDMFFileType
!
!-------------------------------------------------------------------------------
!> @brief Deletes the XDMF file from disk
!> @param file the XDMF file object
!>
SUBROUTINE fdelete_XDMFFileType(file)
  CHARACTER(LEN=*),PARAMETER :: myName='fdelete_XDMFFileType'
  CLASS(XDMFFileType),INTENT(INOUT) :: file
  INTEGER(SIK) :: ierr
ENDSUBROUTINE fdelete_XDMFFileType
!
!-------------------------------------------------------------------------------
SUBROUTINE importFromDiskToVTK_XDMFFileType(thisXDMFFile,strpath,vtkMesh)
  CHARACTER(LEN=*),PARAMETER :: myName='importFromDisk_XDMFFileType'
  CLASS(XDMFFileType),INTENT(INOUT) :: thisXDMFFile
  CLASS(StringType),INTENT(IN) :: strpath
  CLASS(VTKMeshType),INTENT(INOUT) :: vtkMesh
  TYPE(XMLFileType) :: xml
  TYPE(HDF5FileType) :: h5
  TYPE(XMLElementType),POINTER :: xmle, children(:),echildren(:)
  TYPE(StringType) :: strIn,strOut,elname,content,fname,group,toponame,dtype
  TYPE(StringType),ALLOCATABLE :: strArray(:),segments(:)
  INTEGER(SIK) :: i,k,vtkid,nperc,npercRef
  INTEGER(SIK),ALLOCATABLE :: dataShape(:)
  INTEGER(SLK) :: nnodes,j,ncells
  REAL(SSK),ALLOCATABLE :: vals4(:,:)
  REAl(SDK),ALLOCATABLE :: vals8(:,:)
  REAL(SNK),ALLOCATABLE :: ivals4(:,:)
  REAl(SLK),ALLOCATABLE :: ivals8(:,:)
  TYPE(ParamType) :: topolist 
!  TYPE(ParamType),POINTER :: pList(:)
  
  ! Setup param lists for cell type conversions
  ! id is xdmf topology id, n is number of nodes
  ! multiple names for same topology for interoperability
  CALL topolist%add('Topology->Triangle->id'            , 4)
  CALL topolist%add('Topology->Triangle->n'             , 3)
  CALL topolist%add('Topology->Triangle->vtkid'         , VTK_TRIANGLE)
  CALL topolist%add('Topology->Triangle_6->id'          ,24)
  CALL topolist%add('Topology->Triangle_6->n'           , 6)
  CALL topolist%add('Topology->Triangle_6->vtkid'       , VTK_QUADRATIC_TRIANGLE)
  CALL topolist%add('Topology->Tri_6->id'               ,24)
  CALL topolist%add('Topology->Tri_6->n'                , 6)
  CALL topolist%add('Topology->Tri_6->vtkid'            , VTK_QUADRATIC_TRIANGLE)
  CALL topolist%add('Topology->Quadrilateral->id'       , 5)
  CALL topolist%add('Topology->Quadrilateral->n'        , 4)
  CALL topolist%add('Topology->Quadrilateral->vtkid'    , VTK_QUAD)
  CALL topolist%add('Topology->Quadrilateral_8->id'     ,25)
  CALL topolist%add('Topology->Quadrilateral_8->n'      , 8)
  CALL topolist%add('Topology->Quadrilateral_8->vtkid'  , VTK_QUADRATIC_QUAD)
  CALL topolist%add('Topology->Quad_8->id'              ,25)
  CALL topolist%add('Topology->Quad_8->n'               , 8)
  CALL topolist%add('Topology->Quad_8->vtkid'           , VTK_QUADRATIC_QUAD)

  CALL topolist%add('XDMFID->4' ,'Triangle'       ) 
  CALL topolist%add('XDMFID->24','Triangle_6'     ) 
  CALL topolist%add('XDMFID->5' ,'Quadrilateral'  ) 
  CALL topolist%add('XDMFID->25','Quadrilateral_8') 
  
  ! XML
  CALL xml%importFromDisk(CHAR(strpath))
  xmle => xml%root
  REQUIRE(ASSOCIATED(xmle))
  REQUIRE(xmle%name%upper() == 'XDMF')
  ! Version
  strIn='Version'
  CALL xmle%getAttributeValue(strIn,strOut)
  IF(strOut /= '3.0') THEN
    CALL eXDMF%raiseError(modName//'::'//myName// &
      ' - Currently only supports XDMF version 3.0') 
  ENDIF
  ! Domain
  CALL xmle%getChildren(children)
  REQUIRE(SIZE(children) > 0)
  REQUIRE(children(1)%name%upper() == 'DOMAIN')
  IF(SIZE(children) > 1) THEN
     CALL eXDMF%raiseError(modName//'::'//myName// &     
       ' - Currently only supports single domain XDMF') 
  ENDIF
  ! Grid
  CALL children(1)%getChildren(children)
  REQUIRE(SIZE(children) > 0)
  REQUIRE(children(1)%name%upper() == 'GRID')
  IF(SIZE(children) > 1) THEN
     CALL eXDMF%raiseError(modName//'::'//myName// &     
       ' - Currently only supports single grid XDMF') 
  ENDIF                                                    
  ! Process mesh
  CALL children(1)%getChildren(children)
  REQUIRE(SIZE(children) > 0)
  DO i=1,SIZE(children)
    xmle=children(i)
    elname=xmle%name%upper()
    SELECTCASE(CHAR(elname))
    CASE('GEOMETRY')
      ! GeometryType
      strIn='GeometryType'
      CALL xmle%getAttributeValue(strIn,strOut)
      IF(strOut /= 'XYZ') THEN
        CALL eXDMF%raiseWarning(modName//'::'//myName// &
          ' - GeometryType only supports XYZ right now.')              
      ENDIF
      ! Format
      CALL xmle%getChildren(echildren)
      REQUIRE(SIZE(echildren) == 1)
      xmle=echildren(1)
      strIn='Format'
      CALL xmle%getAttributeValue(strIn,strOut)
      IF(strOut /= 'HDF') THEN
        CALL eXDMF%raiseWarning(modName//'::'//myName// &
          ' - only supports HDF5 geometry data right now.')              
      ENDIF
      ! Node Data
      strIn='Dimensions'
      CALL xmle%getAttributeValue(strIn,strOut)
      strArray=strOut%split()
      REQUIRE(strArray(2) == '3')
      nnodes=strArray(1)%stoi()
      !H5 File
      content=xmle%getContent()
      segments=content%split(':')
      fname=segments(1)
      group=segments(2)%substr(2,LEN(segments(2)))
      CALL h5%init(TRIM(fname),'READ')
      CALL h5%fopen()
      REQUIRE(h5%pathExists(CHAR(group)))
      dataShape=h5%getDataShape(CHAR(group))
      REQUIRE(dataShape(1) == 3)
      REQUIRE(dataShape(2) == nnodes)
      ! Data type
      dtype=h5%getDataType(CHAR(group))
      IF(dtype == 'SSK') THEN
        CALL h5%fread(CHAR(group),vals4)
      ELSE
        CALL h5%fread(CHAR(group),vals8)
      ENDIF
      !problem if SRK /= SDK? 
      vtkMesh%numPoints=nnodes
      ALLOCATE(vtkMesh%x(nnodes))
      ALLOCATE(vtkMesh%y(nnodes))
      ALLOCATE(vtkMesh%z(nnodes))
      IF(dtype == 'SSK') THEN
        DO j=1,nnodes
          vtkMesh%x(j)=vals4(1,j)
          vtkMesh%y(j)=vals4(2,j)
          vtkMesh%z(j)=vals4(3,j)
        ENDDO
        DEALLOCATE(vals4)
      ELSE
        DO j=1,nnodes
          vtkMesh%x(j)=vals8(1,j)
          vtkMesh%y(j)=vals8(2,j)
          vtkMesh%z(j)=vals8(3,j)
        ENDDO
        DEALLOCATE(vals8)
      ENDIF
    CASE('TOPOLOGY')
      vtkMesh%meshType=3 ! unstructured grid only
      ! Mixed or single type
      ! TopologyType
      strIn='TopologyType'
      CALL xmle%getAttributeValue(strIn,strOut)
      toponame=strOut
      IF(toponame%upper() == 'MIXED') THEN
      ! Mixed topology
        CALL eXDMF%raiseError(modName//'::'//myName// &
          ' - Mixed Topology not currently supported')              
      ELSE
      ! Single topology
        ! Topology
        IF(.NOT.topolist%has(CHAR(strOut))) CALL eXDMF%raiseError(modName// &
          '::'//myName//' - Topology type '//TRIM(strOut)//' currently supported')              
        ! ncells
        strIn='NumberOfElements'                        
        CALL xmle%getAttributeValue(strIn,strOut)
        ncells=strOut%stoi()
        ! nodes per cell
        strIn='NodesPerElement'                        
        CALL xmle%getAttributeValue(strIn,strOut)
        CALL topolist%get(CHAR(toponame)//'->n', npercRef)
        REQUIRE(strOut%stoi() == npercRef)
        nperc=strOut%stoi()
        ! Cell data
        CALL xmle%getChildren(echildren)
        REQUIRE(SIZE(echildren) == 1)
        xmle=echildren(1)
        strIn='Dimensions'
        CALL xmle%getAttributeValue(strIn,strOut)
        strArray=strOut%split()
        REQUIRE(strArray(2)%stoi() == npercRef)
        REQUIRE(strArray(1)%stoi() == ncells)
        strIn='DataType'
        CALL xmle%getAttributeValue(strIn,strOut)
        REQUIRE(strOut%upper() == 'INT' .OR. strOut%upper() == 'UINT')
        !H5 File
        content=xmle%getContent()
        segments=content%split(':')
        fname=segments(1)
        group=segments(2)%substr(2,LEN(segments(2)))
!        CALL h5%init(TRIM(fname),'READ')
!        CALL h5%fopen()
        REQUIRE(h5%pathExists(CHAR(group)))
        dataShape=h5%getDataShape(CHAR(group))
        REQUIRE(dataShape(1) == nperc)
        REQUIRE(dataShape(2) == ncells)
        ! Data type
        dtype=h5%getDataType(CHAR(group))
        IF(dtype == 'SNK') THEN
          CALL h5%fread(CHAR(group),ivals4)
        ELSE
          CALL h5%fread(CHAR(group),ivals8)
        ENDIF
        !problem if SIK /= SLK?
        vtkMesh%numCells=ncells
        ALLOCATE(vtkMesh%cellList(ncells))
        ALLOCATE(vtkMesh%nodeList(ncells*nperc))
        ! Convert XDMF topology id to VTK
        CALL topolist%get(CHAR(toponame)//'->vtkid', vtkid)
        IF(dtype == 'SNK') THEN
          DO j=1,ncells
            vtkMesh%cellList(j)=vtkid
            DO k=1,nperc
              vtkMesh%nodeList(nperc*(j-1)+k)=ivals4(k,j)
            ENDDO
          ENDDO
          DEALLOCATE(ivals4)
        ELSE
          DO j=1,ncells
            vtkMesh%cellList(j)=vtkid
            DO k=1,nperc
              vtkMesh%nodeList(nperc*(j-1)+k)=ivals8(k,j)
            ENDDO
          ENDDO
          DEALLOCATE(ivals8)
        ENDIF
      ENDIF                                                                   
    CASE DEFAULT
      CALL eXDMF%raiseWarning(modName//'::'//myName// &
      ' - Unsupported data in XDMF file '//CHAR(elname))      
    ENDSELECT
  ENDDO

  

ENDSUBROUTINE importFromDiskToVTK_XDMFFileType
!
ENDMODULE FileType_XDMF
