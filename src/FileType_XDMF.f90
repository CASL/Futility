!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module defines objects for representing an XDMF file as a hierarchical
!> mesh
!>
!> This module reads an XDMF file and stores the information in a hierarchical
!> mesh type. It can also write the hierarchical mesh to XDMF.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FileType_XDMF
#include "Futility_DBC.h"
USE ExceptionHandler
USE Futility_DBC
USE ISO_FORTRAN_ENV
USE IntrType
USE Strings
USE FileType_Base
USE FileType_XML
USE FileType_HDF5
USE ParameterLists

IMPLICIT NONE
PRIVATE

#ifdef FUTILITY_HAVE_HDF5
! Public members
PUBLIC :: XDMFFileType
PUBLIC :: XDMFMeshType
PUBLIC :: XDMFTopologyList
PUBLIC :: ASSIGNMENT(=)

!> The module name
CHARACTER(LEN=*),PARAMETER :: modName='FILETYPE_XDMF'

!> Exception handler for the module
TYPE(ExceptionHandlerType),SAVE :: eXDMF

!> Parameter list that holds XDMF topology names, ids, etc.
TYPE(ParamType),SAVE :: XDMFTopologyList

!> Type to hold the vertices that make up a mesh cell
TYPE :: XDMFCell
  !> The cell type id followed by the vertex ids
  !> XDMF ID, v1, v2, ..., v_n
  INTEGER(SLK), ALLOCATABLE :: vertex_list(:)
ENDTYPE XDMFCell

!> Type to hold a list of cell IDs that make up a named set.
TYPE :: XDMFCellSet
  !> The name of the set
  TYPE(StringType) :: name
  !> The cell IDs
  INTEGER(SLK), ALLOCATABLE :: cell_list(:)
ENDTYPE XDMFCellSet

!> Type to hold the XDMF mesh data
TYPE :: XDMFMeshType
  !> The name of the mesh
  TYPE(StringType) :: name
  !> If the mesh cells are all the same topology
  LOGICAL(SBK) :: singleTopology=.FALSE.
  !> The vertices that compose the mesh
  !> Looks like:
  !> x1, x2, x3, ..., xn
  !> y1, y2, y3, ..., yn
  !> z1, z2, z3, ..., zn
  !> Therefore vertices will be of shape (3, N)
  REAL(SDK), ALLOCATABLE :: vertices(:, :)
  !> The mesh cells
  TYPE(XDMFCell), ALLOCATABLE :: cells(:)
  !> Material for each mesh cell
  INTEGER(SNK), ALLOCATABLE :: material_ids(:)
  !> Named sets within the mesh
  TYPE(XDMFCellSet), ALLOCATABLE :: cell_sets(:)
  !> Child and parent meshes
  TYPE(XDMFMeshType), POINTER :: parent => NULL(), children(:) => NULL()
  CONTAINS
    !> @copybrief XDMFMeshType::clear_XDMFMeshType
    !> @copydoc XDMFMeshType::clear_XDMFMeshType
    PROCEDURE,PASS :: clear => clear_XDMFMeshType
ENDTYPE XDMFMeshType

!> The XDMF File type
TYPE,EXTENDS(BaseFileType) :: XDMFFileType
!
!List of type bound procedures
!
!Import and export are the only procedures used.
!open, close, and delete are members of the base type.
  CONTAINS
    !> @copybrief FileType_XDMF::fopen_XDMFFileType
    !> @copydoc FileType_XDMF::fopen_XDMFFileType
    PROCEDURE,PASS :: fopen => fopen_XDMFFileType
    !> @copybrief FileType_XDMF::fclose_XDMFFileType
    !> @copydoc FileType_XDMF::fclose_XDMFFileType
    PROCEDURE,PASS :: fclose => fclose_XDMFFileType
    !> @copybrief FileType_XDMF::fdelete_XDMFFileType
    !> @copydoc FileType_XDMF::fdelete_XDMFFileType
    PROCEDURE,PASS :: fdelete => fdelete_XDMFFileType
    !> @copybrief FileType_XDMF::importFromDisk_XDMFFileType
    !> @copydoc FileType_XDMF::importFromDisk_XDMFFileType
    PROCEDURE,PASS :: importFromDisk => importFromDisk_XDMFFileType
    !> @copybrief FileType_XDMF::exportToDisk_XDMFFileType
    !> @copydoc FileType_XDMF::exportToDisk_XDMFFileType
    PROCEDURE,PASS :: exportToDisk => exportToDisk_XDMFFileType
ENDTYPE XDMFFileType

!> @brief Interface for assignment operator (=)
INTERFACE ASSIGNMENT(=)
  !> @copybrief FileType_XDMF::assign_XDMFFileType
  !> @copydoc FileType_XDMF::assign_XDMFFileType
  MODULE PROCEDURE assign_XDMFMeshType
ENDINTERFACE
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Opens the XDMF file type for I/O
!> @param file the XDMF file type object
!>
SUBROUTINE fopen_XDMFFileType(file)
  CHARACTER(LEN=*),PARAMETER :: myName='fopen_XDMFFileType'
  CLASS(XDMFFileType),INTENT(INOUT) :: file
  CALL eXDMF%raiseError(modName//'::'//myName//' - Not implemented.')
ENDSUBROUTINE fopen_XDMFFileType
!
!-------------------------------------------------------------------------------
!> @brief Closes the XDMF file object
!> @param file the XDMF file object
!>
SUBROUTINE fclose_XDMFFileType(file)
  CHARACTER(LEN=*),PARAMETER :: myName='fclose_XDMFFileType'
  CLASS(XDMFFileType),INTENT(INOUT) :: file
  CALL eXDMF%raiseError(modName//'::'//myName//' - Not implemented.')
ENDSUBROUTINE fclose_XDMFFileType
!
!-------------------------------------------------------------------------------
!> @brief Deletes the XDMF file from disk
!> @param file the XDMF file object
!>
SUBROUTINE fdelete_XDMFFileType(file)
  CHARACTER(LEN=*),PARAMETER :: myName='fdelete_XDMFFileType'
  CLASS(XDMFFileType),INTENT(INOUT) :: file
  CALL eXDMF%raiseError(modName//'::'//myName//' - Not implemented.')
ENDSUBROUTINE fdelete_XDMFFileType
!
!-------------------------------------------------------------------------------
!> @brief Initializes the XDMFTopologyList
!>
SUBROUTINE init_XDMFTopologyList()
  ! Setup param list for cell type conversions
  ! id is XDMF topology id,
  ! n is number of vertices,
  ! multiple names for same topology for interoperability
  CALL XDMFTopologyList%add('Topology->Triangle->id'            , 4_SLK)
  CALL XDMFTopologyList%add('Topology->Triangle->n'             , 3_SLK)
  CALL XDMFTopologyList%add('Topology->Triangle_6->id'          ,36_SLK)
  CALL XDMFTopologyList%add('Topology->Triangle_6->n'           , 6_SLK)
  CALL XDMFTopologyList%add('Topology->Tri_6->id'               ,36_SLK)
  CALL XDMFTopologyList%add('Topology->Tri_6->n'                , 6_SLK)
  CALL XDMFTopologyList%add('Topology->Quadrilateral->id'       , 5_SLK)
  CALL XDMFTopologyList%add('Topology->Quadrilateral->n'        , 4_SLK)
  CALL XDMFTopologyList%add('Topology->Quadrilateral_8->id'     ,37_SLK)
  CALL XDMFTopologyList%add('Topology->Quadrilateral_8->n'      , 8_SLK)
  CALL XDMFTopologyList%add('Topology->Quad_8->id'              ,37_SLK)
  CALL XDMFTopologyList%add('Topology->Quad_8->n'               , 8_SLK)
  CALL XDMFTopologyList%add('XDMFID->4' ,'Triangle'       )
  CALL XDMFTopologyList%add('XDMFID->36','Triangle_6'     )
  CALL XDMFTopologyList%add('XDMFID->5' ,'Quadrilateral'  )
  CALL XDMFTopologyList%add('XDMFID->37','Quadrilateral_8')
ENDSUBROUTINE init_XDMFTopologyList
!
!-------------------------------------------------------------------------------
!> @brief Create the XDMF mesh object
!> @param mesh the parent mesh
!> @param xmle the child XML element
!> @param h5 the HDF5 file containing mesh data
!>
RECURSIVE SUBROUTINE create_XDMFMesh_from_file(mesh, xmle, h5)
  CHARACTER(LEN=*),PARAMETER :: myName='create_XDMFMesh_from_file'
  TYPE(XDMFMeshType),TARGET, INTENT(INOUT)  :: mesh
  TYPE(XMLElementType), INTENT(INOUT) :: xmle
  TYPE(HDF5FileType), INTENT(INOUT) :: h5
  TYPE(XMLElementType), POINTER :: xmle_children(:)
  TYPE(StringType) :: strIn, strOut
  INTEGER(SIK) :: i, grid_ctr, mesh_ctr

  ! If this xml element has children
  IF(xmle%hasChildren()) THEN
    ! Determine the number or XML children that are grids
    CALL xmle%getChildren(xmle_children)
    grid_ctr = 0
    DO i=1,SIZE(xmle_children)
      IF(xmle_children(i)%name%upper() == 'GRID') grid_ctr = grid_ctr + 1
    ENDDO

    ! If some children are grids, it is not a leaf
    IF(grid_ctr > 0) THEN
      ! Allocate children for current mesh and create entities
      ALLOCATE(mesh%children(grid_ctr))
      mesh_ctr=1
      ! Recursively create children of each grid
      DO i=1,SIZE(xmle_children)
        IF(xmle_children(i)%name%upper() == 'GRID') THEN
          strIn='Name'
          CALL xmle_children(i)%getAttributeValue(strIn,strOut)
          mesh%children(mesh_ctr)%name = strOut
          mesh%children(mesh_ctr)%parent => mesh
          CALL create_XDMFMesh_from_file(mesh%children(mesh_ctr), xmle_children(i), h5)
          mesh_ctr = mesh_ctr + 1
        ENDIF
      ENDDO
    ! If this mesh does not have grid children it is a leaf on the tree.
    ! Add vertices, cells, etc.
    ELSE
      CALL setup_leaf_XDMFMesh_from_file(mesh, xmle, h5)
    ENDIF
  ELSE
    CALL eXDMF%raiseError(modName//'::'//myName// &
      ' - Expected the XML element to have children.')
  ENDIF
ENDSUBROUTINE create_XDMFMesh_from_file
!
!-------------------------------------------------------------------------------
!> @brief Setup the leaf mesh objects which contain vertices, cells, etc.
!> @param mesh the parent mesh
!> @param xmle the child XML element
!> @param h5 the HDF5 file containing mesh data
!>
SUBROUTINE setup_leaf_XDMFMesh_from_file(mesh, xmle, h5)
  CHARACTER(LEN=*),PARAMETER :: myName='setup_leaf_XDMFMesh_from_file'
  TYPE(XDMFMeshType),INTENT(INOUT),TARGET  :: mesh
  TYPE(XMLElementType), INTENT(INOUT) :: xmle
  TYPE(HDF5FileType), INTENT(INOUT) :: h5
  TYPE(XMLElementType), POINTER :: xmle_children(:), ele_children(:)
  TYPE(StringType) :: elname, strIn, strOut, content, group, dtype, toponame, &
    xdmf_id_str
  TYPE(StringType),ALLOCATABLE :: strArray(:),segments(:)
  INTEGER(SLK) :: nverts, ncells, xdmf_id,vert_ctr,i,j
  INTEGER(SNK) :: ncell_sets
  INTEGER(SNK),ALLOCATABLE :: dshape(:)
  REAL(SSK),ALLOCATABLE :: vals4_2d(:,:)
  REAl(SDK),ALLOCATABLE :: vals8_2d(:,:)
  INTEGER(SNK),ALLOCATABLE :: ivals4_1d(:),ivals4_2d(:,:)
  INTEGER(SLK),ALLOCATABLE :: ivals8_1d(:),ivals8_2d(:,:)
  TYPE(XDMFCellSet), ALLOCATABLE :: cell_sets_temp(:)

  REQUIRE(xmle%hasChildren())
  CALL xmle%getChildren(xmle_children)
  ! Each XML element has a type of information.
  ! Handle each with a CASE
  DO i=1,SIZE(xmle_children)
    elname=xmle_children(i)%name%upper()
    SELECTCASE(ADJUSTL(elname))
      CASE("GEOMETRY")
        ! GeometryType
        strIn='GeometryType'
        CALL xmle_children(i)%getAttributeValue(strIn,strOut)
        IF(strOut /= 'XYZ') THEN
          CALL eXDMF%raiseWarning(modName//'::'//myName// &
            ' - GeometryType only supports XYZ right now.')
        ENDIF
        ! Format
        CALL xmle_children(i)%getChildren(ele_children)
        REQUIRE(SIZE(ele_children) == 1)
        strIn='Format'
        CALL ele_children(1)%getAttributeValue(strIn,strOut)
        IF(strOut /= 'HDF') THEN
          CALL eXDMF%raiseWarning(modName//'::'//myName// &
            ' - only supports HDF5 geometry data right now.')
        ENDIF
        ! Vertex Data
        strIn='Dimensions'
        CALL ele_children(1)%getAttributeValue(strIn,strOut)
        strArray=strOut%split()
        REQUIRE(strArray(2) == '3')
        nverts=strArray(1)%stoi()
        ! This is all awful string manipulation to get the h5 group
        content=ele_children(1)%getContent()
        segments=content%split(':')
        group=segments(2)%substr(2,LEN(segments(2)))
        REQUIRE(h5%pathExists(CHAR(group)))
        group = group%replace("/", "->")
        ! Data shape
        dshape=h5%getDataShape(CHAR(group))
        REQUIRE(dshape(1) == 3)
        REQUIRE(dshape(2) == nverts)
        ! Data type
        dtype=h5%getDataType(CHAR(group))
        IF(dtype == 'SSK') THEN
          CALL h5%fread(CHAR(group),vals4_2d)
        ELSE
          CALL h5%fread(CHAR(group),vals8_2d)
        ENDIF
        ALLOCATE(mesh%vertices(3,nverts))
        IF(dtype == 'SSK') THEN
          mesh%vertices=vals4_2d
          DEALLOCATE(vals4_2d)
        ELSE
          mesh%vertices=vals8_2d
          DEALLOCATE(vals8_2d)
        ENDIF
      CASE("TOPOLOGY")
        ! TopologyType
        strIn='TopologyType'
        CALL xmle_children(i)%getAttributeValue(strIn,toponame)
        IF(toponame%upper() == 'MIXED') THEN
          ! Mixed topology
          ! Format
          CALL xmle_children(i)%getChildren(ele_children)
          REQUIRE(SIZE(ele_children) == 1)
          strIn='Format'
          CALL ele_children(1)%getAttributeValue(strIn,strOut)
          IF(strOut /= 'HDF') THEN
            CALL eXDMF%raiseWarning(modName//'::'//myName// &
              ' - only supports HDF5 topology data right now.')
          ENDIF
          ! Topology Data
          strIn='NumberOfElements'
          CALL xmle_children(i)%getAttributeValue(strIn,strOut)
          ncells=strOut%stoi()
          ! This is all awful string manipulation to get the h5 group
          content=ele_children(1)%getContent()
          segments=content%split(':')
          group=segments(2)%substr(2,LEN(segments(2)))
          REQUIRE(h5%pathExists(CHAR(group)))
          group = group%replace("/", "->")
          ! Data shape
          dshape=h5%getDataShape(CHAR(group))
          REQUIRE(SIZE(dshape) == 1)
          ! Data type
          dtype=h5%getDataType(CHAR(group))
          IF(dtype == 'SNK') THEN
            CALL h5%fread(CHAR(group),ivals4_1d)
          ELSE
            CALL h5%fread(CHAR(group),ivals8_1d)
          ENDIF
          ALLOCATE(mesh%cells(ncells))
          vert_ctr = 1
          IF(dtype == 'SNK') THEN
            DO j=1,ncells
              xdmf_id = ivals4_1d(vert_ctr)
              xdmf_id_str = xdmf_id
              IF(.NOT.XDMFTopologyList%has('XDMFID->'//ADJUSTL(xdmf_id_str))) THEN
                CALL eXDMF%raiseError(modName//'::'//myName//&
                  ' - Topology type '//TRIM(xdmf_id_str)//' not supported')
              ELSE
                CALL XDMFTopologyList%get('XDMFID->'//ADJUSTL(xdmf_id_str), toponame)
                CALL XDMFTopologyList%get(ADJUSTL(toponame)//'->n', nverts)
              ENDIF
              ALLOCATE(mesh%cells(j)%vertex_list(nverts+1))
              mesh%cells(j)%vertex_list(1) = xdmf_id
              mesh%cells(j)%vertex_list(2:nverts+1) = ivals4_1d(vert_ctr:vert_ctr+nverts) + 1
              vert_ctr = vert_ctr + nverts
            ENDDO
            DEALLOCATE(ivals4_1d)
          ELSE
            DO j=1,ncells
              xdmf_id = ivals8_1d(vert_ctr)
              xdmf_id_str = xdmf_id
              IF(.NOT.XDMFTopologyList%has('XDMFID->'//ADJUSTL(xdmf_id_str))) THEN
                CALL eXDMF%raiseError(modName//'::'//myName//&
                  ' - Topology type '//TRIM(xdmf_id_str)//' not supported')
              ELSE
                CALL XDMFTopologyList%get('XDMFID->'//ADJUSTL(xdmf_id_str), toponame)
                CALL XDMFTopologyList%get(ADJUSTL(toponame)//'->n', nverts)
              ENDIF
              ALLOCATE(mesh%cells(j)%vertex_list(nverts+1))
              mesh%cells(j)%vertex_list(1) = xdmf_id
              mesh%cells(j)%vertex_list(2:nverts+1) = ivals8_1d(vert_ctr+1:vert_ctr+nverts) + 1
              vert_ctr = vert_ctr + nverts + 1
            ENDDO
            DEALLOCATE(ivals8_1d)
          ENDIF
        ELSE
          ! Single topology
          IF(.NOT.XDMFTopologyList%has(CHAR(toponame))) CALL eXDMF%raiseError(modName// &
            '::'//myName//' - Topology type '//TRIM(strOut)//' not supported')
          ! XDMF ID
          CALL XDMFTopologyList%get(CHAR(toponame)//'->id', xdmf_id)
          ! Format
          CALL xmle_children(i)%getChildren(ele_children)
          REQUIRE(SIZE(ele_children) == 1)
          strIn='Format'
          CALL ele_children(1)%getAttributeValue(strIn,strOut)
          IF(strOut /= 'HDF') THEN
            CALL eXDMF%raiseWarning(modName//'::'//myName// &
              ' - only supports HDF5 topology data right now.')
          ENDIF
          ! Topology Data
          strIn='NumberOfElements'
          CALL xmle_children(i)%getAttributeValue(strIn,strOut)
          ncells=strOut%stoi()
          strIn='NodesPerElement'
          CALL xmle_children(i)%getAttributeValue(strIn,strOut)
          nverts=strOut%stoi()
          ! This is all awful string manipulation to get the h5 group
          content=ele_children(1)%getContent()
          segments=content%split(':')
          group=segments(2)%substr(2,LEN(segments(2)))
          REQUIRE(h5%pathExists(CHAR(group)))
          group = group%replace("/", "->")
          ! Data shape
          dshape=h5%getDataShape(CHAR(group))
          REQUIRE(dshape(1) == nverts)
          REQUIRE(dshape(2) == ncells)
          ! Data type
          dtype=h5%getDataType(CHAR(group))
          IF(dtype == 'SNK') THEN
            CALL h5%fread(CHAR(group),ivals4_2d)
          ELSE
            CALL h5%fread(CHAR(group),ivals8_2d)
          ENDIF
          ALLOCATE(mesh%cells(ncells))
          IF(dtype == 'SNK') THEN
            DO j=1,ncells
              ALLOCATE(mesh%cells(j)%vertex_list(nverts + 1))
              mesh%cells(j)%vertex_list(1) = xdmf_id
              ! Account for 0based to 1based index switch
              mesh%cells(j)%vertex_list(2:) = ivals4_2d(:, j) + 1
            ENDDO
            DEALLOCATE(ivals4_2d)
          ELSE
            DO j=1,ncells
              ALLOCATE(mesh%cells(j)%vertex_list(nverts + 1))
              mesh%cells(j)%vertex_list(1) = xdmf_id
              ! Account for 0based to 1based index switch
              mesh%cells(j)%vertex_list(2:) = ivals8_2d(:, j) + 1
            ENDDO
            DEALLOCATE(ivals8_2d)
          ENDIF
          mesh%singleTopology = .TRUE.
        ENDIF
      CASE("ATTRIBUTE")
        strIn='Name'
        CALL xmle_children(i)%getAttributeValue(strIn,strOut)
        IF(strOut%upper() == 'MATERIALID') THEN
          ! Format
          CALL xmle_children(i)%getChildren(ele_children)
          REQUIRE(SIZE(ele_children) == 1)
          strIn='Format'
          CALL ele_children(1)%getAttributeValue(strIn,strOut)
          IF(strOut /= 'HDF') THEN
            CALL eXDMF%raiseWarning(modName//'::'//myName// &
              ' - only supports HDF5 material data right now.')
          ENDIF
          ! Material Data
          strIn='Dimensions'
          CALL ele_children(1)%getAttributeValue(strIn,strOut)
          ncells=strOut%stoi()
          REQUIRE(ALLOCATED(mesh%cells))
          REQUIRE(ncells == SIZE(mesh%cells))
          ! This is all awful string manipulation to get the h5 group
          content=ele_children(1)%getContent()
          segments=content%split(':')
          group=segments(2)%substr(2,LEN(segments(2)))
          REQUIRE(h5%pathExists(CHAR(group)))
          group = group%replace("/", "->")
          ! Data shape
          dshape=h5%getDataShape(CHAR(group))
          REQUIRE(SIZE(dshape) == 1)
          REQUIRE(dshape(1) == ncells)
          ! Data type
          dtype=h5%getDataType(CHAR(group))
          IF(dtype == 'SNK') THEN
            CALL h5%fread(CHAR(group),ivals4_1d)
          ELSE
            CALL h5%fread(CHAR(group),ivals8_1d)
          ENDIF
          ALLOCATE(mesh%material_ids(ncells))
          IF(dtype == 'SNK') THEN
            ! Account for 0based to 1based index switch
            mesh%material_ids = ivals4_1d + 1
            DEALLOCATE(ivals4_1d)
          ELSE
            ! Account for 0based to 1based index switch
            ! material ids will not exceed MAX(INTEGER(4)),
            ! so narrowing will not occur.
            mesh%material_ids = ivals8_1d + 1
            DEALLOCATE(ivals8_1d)
          ENDIF
        ELSE
          CALL eXDMF%raiseWarning(modName//'::'//myName//' - mesh attribute '//&
            TRIM(strOut)//' not supported')
        ENDIF
      CASE("SET")
        ! SetType
        strIn='SetType'
        CALL xmle_children(i)%getAttributeValue(strIn,strOut)
        IF(strOut /= 'Cell') THEN
          CALL eXDMF%raiseWarning(modName//'::'//myName// &
            ' - only supports SetType="Cell" right now.')
        ENDIF
        ! SetName
        strIn='Name'
        CALL xmle_children(i)%getAttributeValue(strIn,elname)
        ! Format
        CALL xmle_children(i)%getChildren(ele_children)
        REQUIRE(SIZE(ele_children) == 1)
        strIn='Format'
        CALL ele_children(1)%getAttributeValue(strIn,strOut)
        IF(strOut /= 'HDF') THEN
          CALL eXDMF%raiseWarning(modName//'::'//myName// &
            ' - only supports HDF5 cell set data right now.')
        ENDIF
        ! Cell Set Data
        strIn='Dimensions'
        CALL ele_children(1)%getAttributeValue(strIn,strOut)
        ncells=strOut%stoi()
        REQUIRE(ALLOCATED(mesh%cells))
        REQUIRE(ncells <= SIZE(mesh%cells))
        ! This is all awful string manipulation to get the h5 group
        content=ele_children(1)%getContent()
        segments=content%split(':')
        group=segments(2)%substr(2,LEN(segments(2)))
        REQUIRE(h5%pathExists(CHAR(group)))
        group = group%replace("/", "->")
        ! Data shape
        dshape=h5%getDataShape(CHAR(group))
        REQUIRE(SIZE(dshape) == 1)
        REQUIRE(dshape(1) == ncells)
        ! Data type
        dtype=h5%getDataType(CHAR(group))
        IF(dtype == 'SNK') THEN
          CALL h5%fread(CHAR(group),ivals4_1d)
        ELSE
          CALL h5%fread(CHAR(group),ivals8_1d)
        ENDIF
        ! Resize cell sets if needed
        ! This is expected to happen infrequently
        IF(ALLOCATED(mesh%cell_sets)) THEN
          ! Copy current sets to temp, deallocate current sets
          ncell_sets = SIZE(mesh%cell_sets)
          ALLOCATE(cell_sets_temp(ncell_sets))
          DO j=1, ncell_sets
            ALLOCATE(cell_sets_temp(j)%cell_list(SIZE(mesh%cell_sets(j)%cell_list)))
            cell_sets_temp(j)%cell_list = mesh%cell_sets(j)%cell_list
            cell_sets_temp(j)%name = mesh%cell_sets(j)%name
            DEALLOCATE(mesh%cell_sets(j)%cell_list)
          ENDDO
          DEALLOCATE(mesh%cell_sets)
          ! Reallocate cell sets to be on bigger and copy all old sets over
          ALLOCATE(mesh%cell_sets(ncell_sets+1))
          DO j=1, ncell_sets
            ALLOCATE(mesh%cell_sets(j)%cell_list(SIZE(cell_sets_temp(j)%cell_list)))
            mesh%cell_sets(j)%cell_list = cell_sets_temp(j)%cell_list
            mesh%cell_sets(j)%name = cell_sets_temp(j)%name
            DEALLOCATE(cell_sets_temp(j)%cell_list)
            CALL cell_sets_temp(j)%name%clear()
          ENDDO
          DEALLOCATE(cell_sets_temp)
        ELSE
          ncell_sets = 0
          ALLOCATE(mesh%cell_sets(1))
        ENDIF
        ! Add the one new cell set
        mesh%cell_sets(ncell_sets + 1)%name = elname
        ALLOCATE(mesh%cell_sets(ncell_sets + 1)%cell_list(ncells))
        IF(dtype == 'SNK') THEN
          ! Account for 0based to 1based index switch
          mesh%cell_sets(ncell_sets + 1)%cell_list = ivals4_1d + 1
          DEALLOCATE(ivals4_1d)
        ELSE
          mesh%cell_sets(ncell_sets + 1)%cell_list = ivals8_1d + 1
          DEALLOCATE(ivals8_1d)
        ENDIF

      CASE DEFAULT
        CALL eXDMF%raiseWarning(modName//'::'//myName// &
          ' - Unsupported data in XDMF file '//CHAR(elname))
    ENDSELECT
  ENDDO
ENDSUBROUTINE setup_leaf_XDMFMesh_from_file
!
!-------------------------------------------------------------------------------
!> @brief Imports the mesh data in the file to a mesh object.
!> @param thisXDMFFile the XDMF file type object
!> @param strpath the string holding the path to the XDMF file
!> @param mesh the XDMF mesh object
!>
SUBROUTINE importFromDisk_XDMFFileType(thisXDMFFile, strpath, mesh)
  CHARACTER(LEN=*),PARAMETER :: myName='importFromDisk_XDMFFileType'
  CLASS(XDMFFileType),INTENT(INOUT) :: thisXDMFFile
  TYPE(StringType),INTENT(INOUT) :: strpath
  TYPE(XDMFMeshType),INTENT(OUT),TARGET  :: mesh
  TYPE(XMLFileType) :: xml
  TYPE(HDF5FileType) :: h5
  TYPE(XMLElementType),POINTER :: xmle, children(:)
  TYPE(StringType) :: strIn, strOut
  INTEGER(SIK) :: i
  CHARACTER(LEN=200) :: charpath

  ! Initialize the XDMFTopologyList if it has not been
  IF(.NOT.XDMFTopologyList%has('Topology')) CALL init_XDMFTopologyList()

  !H5
  ! NOTE: it is assumed that the h5 and xml files have the same name.
  i = LEN_TRIM(strpath)
  charpath = CHAR(strpath)
  CALL h5%init(charpath(1:i-4)//"h5",'READ')
  CALL h5%fopen()

  !XML
  CALL xml%importFromDisk(ADJUSTL(strpath))
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

  ! Information
  ! NOTE: It is assumed that material information is before any grids
  ! and that all grids are contained in one overall grid.
  CALL children(1)%getChildren(children)
  IF (SIZE(children) == 2) THEN
    REQUIRE(children(1)%name%upper() == 'INFORMATION')
    REQUIRE(children(2)%name%upper() == 'GRID')
    i = 2
  ELSE IF(SIZE(children) == 1) THEN
    REQUIRE(children(1)%name%upper() == 'GRID')
    i = 1
  ELSE
    CALL eXDMF%raiseError(modName//'::'//myName// &
      ' - Expecting information and grid elements only.')
  ENDIF

  ! Init root mesh
  strIn="Name"
  CALL children(i)%getAttributeValue(strIn,strOut)
  mesh%name = strOut

  ! Create grids
  CALL create_XDMFMesh_from_file(mesh, children(i), h5)

ENDSUBROUTINE importFromDisk_XDMFFileType
!
!-------------------------------------------------------------------------------
!> @brief Clears the XDMF mesh
!> @param thismesh the XDMF mesh object being assigned to
!>
RECURSIVE SUBROUTINE clear_XDMFMeshType(thismesh)
  CLASS(XDMFMeshType), INTENT(INOUT) :: thismesh
  INTEGER(SNK) :: i

  CALL thismesh%name%clear()
  thismesh%singleTopology = .FALSE.
  IF(ASSOCIATED(thismesh%parent)) thismesh%parent => NULL()
  IF(ASSOCIATED(thismesh%children)) THEN
    DO i=1,SIZE(thismesh%children)
      CALL thismesh%children(i)%clear()
    ENDDO
    thismesh%children => NULL()
  ENDIF
  IF( ALLOCATED(thismesh%vertices) ) DEALLOCATE(thismesh%vertices)
  IF( ALLOCATED(thismesh%cells) ) THEN
    DO i=1, SIZE(thismesh%cells)
      DEALLOCATE(thismesh%cells(i)%vertex_list)
    ENDDO
    DEALLOCATE(thismesh%cells)
  ENDIF
  IF( ALLOCATED(thismesh%material_ids) ) DEALLOCATE(thismesh%material_ids)
  IF( ALLOCATED(thismesh%cell_sets) ) THEN
    DO i=1, SIZE(thismesh%cell_sets)
      DEALLOCATE(thismesh%cell_sets(i)%cell_list)
    ENDDO
    DEALLOCATE(thismesh%cell_sets)
  ENDIF
ENDSUBROUTINE clear_XDMFMeshType
!
!-------------------------------------------------------------------------------
!> @brief Assigns an XDMF mesh type to another
!> @param thismesh the XDMF mesh object being assigned to
!> @param thatmesh the XDMF mesh object being assigned from
!>
RECURSIVE SUBROUTINE assign_XDMFMeshType(thismesh, thatmesh)
  TYPE(XDMFMeshType), INTENT(INOUT) :: thismesh
  TYPE(XDMFMeshType), INTENT(IN) :: thatmesh
  INTEGER(SNK) :: i

  thismesh%name = thatmesh%name
  thismesh%singleTopology = thatmesh%singleTopology
  IF(ASSOCIATED(thatmesh%parent)) thismesh%parent => thatmesh%parent
  ! NOTE: Children cannot be recursively cleared without risk of
  ! modify other mesh objects due to the pointer to other meshes.
  ! Therefore, it is assumed that one will manually clear a mesh
  ! if the children are to be deleted.
  IF(ASSOCIATED(thatmesh%children)) THEN
    ALLOCATE(thismesh%children(SIZE(thatmesh%children)))
    thismesh%children => thatmesh%children
  ENDIF
  IF( ALLOCATED(thatmesh%vertices) ) THEN
    IF(ALLOCATED(thismesh%vertices)) DEALLOCATE(thismesh%vertices)
    ALLOCATE(thismesh%vertices(3, SIZE(thatmesh%vertices, DIM=2)))
    thismesh%vertices = thatmesh%vertices
  ENDIF
  IF( ALLOCATED(thatmesh%cells) ) THEN
    IF(ALLOCATED(thismesh%cells))THEN
      DO i=1, SIZE(thismesh%cells)
        DEALLOCATE(thismesh%cells(i)%vertex_list)
      ENDDO
      DEALLOCATE(thismesh%cells)
    ENDIF
    ALLOCATE(thismesh%cells(SIZE(thatmesh%cells)))
    DO i = 1, SIZE(thatmesh%cells)
      ALLOCATE(thismesh%cells(i)%vertex_list(SIZE(thatmesh%cells(i)%vertex_list)))
      thismesh%cells(i)%vertex_list = thatmesh%cells(i)%vertex_list
    ENDDO
  ENDIF
  IF( ALLOCATED(thatmesh%material_ids) )THEN
    IF(ALLOCATED(thismesh%material_ids)) DEALLOCATE(thismesh%material_ids)
    ALLOCATE(thismesh%material_ids(SIZE(thatmesh%material_ids)))
    thismesh%material_ids = thatmesh%material_ids
  ENDIF
  IF( ALLOCATED(thatmesh%cell_sets) ) THEN
    IF(ALLOCATED(thismesh%cell_sets))THEN
      DO i=1, SIZE(thismesh%cell_sets)
        DEALLOCATE(thismesh%cell_sets(i)%cell_list)
      ENDDO
      DEALLOCATE(thismesh%cell_sets)
    ENDIF
    ALLOCATE(thismesh%cell_sets(SIZE(thatmesh%cell_sets)))
    DO i = 1, SIZE(thatmesh%cell_sets)
      ALLOCATE(thismesh%cell_sets(i)%cell_list(SIZE(thatmesh%cell_sets(i)%cell_list)))
      thismesh%cell_sets(i)%cell_list = thatmesh%cell_sets(i)%cell_list
      thismesh%cell_sets(i)%name = thatmesh%cell_sets(i)%name
    ENDDO
  ENDIF
ENDSUBROUTINE assign_XDMFMeshType
!
!-------------------------------------------------------------------------------
!> @brief Create the xml hierarchy for the mesh
!> @param mesh the mesh
!> @param xmle the XML element
!> @param h5 the HDF5 file
!>
RECURSIVE SUBROUTINE export_leaf_XDMFFileType(mesh, xmle, strpath, h5)
  CHARACTER(LEN=*),PARAMETER :: myName='export_leaf_XDMFFileType'
  TYPE(XDMFMeshType),INTENT(IN)  :: mesh
  TYPE(XMLElementType),TARGET,INTENT(INOUT) :: xmle
  TYPE(StringType),INTENT(INOUT) :: strpath
  TYPE(HDF5FileType), INTENT(INOUT) :: h5
  TYPE(XMLElementType),POINTER :: current_xml, child_xml
  TYPE(StringType) :: str_name, str_value, str1, str2, toponame, xdmf_id_str 
  INTEGER(SNK) :: nchildren, child_ctr
  INTEGER(SLK) :: xdmf_id, nverts, ncells, i
  CHARACTER(LEN=200) :: charpath
  INTEGER(SLK), ALLOCATABLE :: vertex_list_2d(:, :), vertex_list_1d(:, :)

  ! Create HDF5 group
  CALL h5%mkdir(CHAR(mesh%name))

  ! Determine number of xmle children
  ! Geometry, Topology, Material, Cell sets
  ! Assumes mesh has geometry and topology
  nchildren = 2
  IF(ALLOCATED(mesh%material_ids)) nchildren = nchildren + 1
  IF(ALLOCATED(mesh%cell_sets)) nchildren = nchildren + SIZE(mesh%cell_sets)
  ALLOCATE(xmle%children(nchildren))
  child_ctr = 1

  ! GEOMETRY
  current_xml => xmle%children(1)
  str_name="Geometry"
  CALL current_xml%setName(str_name) 
  current_xml%nAttr=0
  current_xml%parent => xmle

  str_name= "GeometryType"
  str_value = "XYZ"
  CALL current_xml%setAttribute(str_name, str_value)

  ALLOCATE(current_xml%children(1))
  child_xml => current_xml%children(1)
  str_name="DataItem"
  CALL child_xml%setName(str_name) 
  child_xml%nAttr=0
  child_xml%parent => current_xml

  str_name= "DataType"
  str_value = "Float"
  CALL child_xml%setAttribute(str_name, str_value)

  nverts=SIZE(mesh%vertices, DIM=2)
  str_name="Dimensions"
  str1 = nverts
  str2 = "3"
  str_value = str1//" "//str2
  CALL child_xml%setAttribute(str_name, str_value)

  str_name= "Format"
  str_value = "HDF"
  CALL child_xml%setAttribute(str_name, str_value)

  str_name= "Precision"
  str_value = "8"
  CALL child_xml%setAttribute(str_name, str_value)

  i = LEN_TRIM(strpath)
  charpath = CHAR(strpath)
  child_xml%content = charpath(1:i-4)//"h5:/"//mesh%name//"/vertices"

  CALL h5%fwrite(CHAR(mesh%name)//'->vertices',mesh%vertices)

  ! TOPOLOGY
  current_xml => xmle%children(2)
  str_name="Topology"
  CALL current_xml%setName(str_name) 
  current_xml%nAttr=0
  current_xml%parent => xmle

  IF(mesh%singleTopology)THEN
    str_name= "TopologyType"
    xdmf_id = mesh%cells(1)%vertex_list(1)
    xdmf_id_str = xdmf_id
    CALL XDMFTopologyList%get('XDMFID->'//ADJUSTL(xdmf_id_str), toponame)
    CALL XDMFTopologyList%get(ADJUSTL(toponame)//'->n', nverts)
    str_value = toponame
    CALL current_xml%setAttribute(str_name, str_value)

    str_name= "NumberOfElements"
    ncells = SIZE(mesh%cells)
    str_value = ncells
    CALL current_xml%setAttribute(str_name, str_value)

    str_name= "NodesPerElement"
    str_value = nverts
    CALL current_xml%setAttribute(str_name, str_value)    

    ALLOCATE(current_xml%children(1))
    child_xml => current_xml%children(1)
    str_name="DataItem"
    CALL child_xml%setName(str_name) 
    child_xml%nAttr=0
    child_xml%parent => current_xml

    str_name= "DataType"
    str_value = "Int"
    CALL child_xml%setAttribute(str_name, str_value)

    str_name="Dimensions"
    str1 = ncells
    str2 = nverts
    str_value = str1//" "//str2
    CALL child_xml%setAttribute(str_name, str_value)

    str_name= "Format"
    str_value = "HDF"
    CALL child_xml%setAttribute(str_name, str_value)

    str_name= "Precision"
    str_value = "8"
    CALL child_xml%setAttribute(str_name, str_value)

    i = LEN_TRIM(strpath)
    charpath = CHAR(strpath)
    child_xml%content = charpath(1:i-4)//"h5:/"//mesh%name//"/cells"

    ALLOCATE(vertex_list_2d(nverts, ncells))
    DO i = 1, ncells
      ! Convert 1 based to 0 based index
      vertex_list_2d(:,i) = mesh%cells(i)%vertex_list(2:) - 1
    ENDDO

    CALL h5%fwrite(CHAR(mesh%name)//'->cells',vertex_list_2d)

    DEALLOCATE(vertex_list_2d)

!  ELSE
!    str_name= "TopologyType"
!    str_value = "Mixed"
!    CALL current_xml%setAttribute(str_name, str_value)
  ENDIF


ENDSUBROUTINE export_leaf_XDMFFileType
!
!-------------------------------------------------------------------------------
!> @brief Create the xml hierarchy for the mesh
!> @param mesh the mesh
!> @param xmle the XML element
!> @param h5 the HDF5 file
!>
RECURSIVE SUBROUTINE create_xml_hierarchy_XDMFFileType(mesh, xmle, strpath, h5)
  CHARACTER(LEN=*),PARAMETER :: myName='create_xml_hierarchy_XDMFFileType'
  TYPE(XDMFMeshType),INTENT(INOUT)  :: mesh
  TYPE(XMLElementType),TARGET,INTENT(INOUT) :: xmle
  TYPE(StringType),INTENT(INOUT) :: strpath
  TYPE(HDF5FileType), INTENT(INOUT) :: h5
  INTEGER(SNK) :: i
  TYPE(StringType) :: str_name, str_value

  WRITE(*,*) CHAR(mesh%name)
  ! If this mesh has children
  IF(ASSOCIATED(mesh%children)) THEN
    ! Add XML element children
    ALLOCATE(xmle%children(SIZE(mesh%children)))
    DO i=1,SIZE(mesh%children)
      ! Set attributes then recurse
      str_name="Grid"
      CALL xmle%children(i)%setName(str_name) 
      xmle%children(i)%nAttr=0
      xmle%children(i)%parent => xmle
      str_name='Name'
      str_value = mesh%children(i)%name
      CALL xmle%children(i)%setAttribute(str_name, str_value)
      str_name='GridType'
      IF(ASSOCIATED(mesh%children(i)%children))THEN
        str_value = 'Tree'
      ELSE
        str_value = 'Uniform'
      ENDIF
      CALL xmle%children(i)%setAttribute(str_name, str_value)

      CALL create_xml_hierarchy_XDMFFileType(mesh%children(i), xmle%children(i), strpath, h5) 
    ENDDO
  ELSE
    CALL export_leaf_XDMFFileType(mesh, xmle, strpath, h5)
  ENDIF
ENDSUBROUTINE create_xml_hierarchy_XDMFFileType
!
!-------------------------------------------------------------------------------
!> @brief Exports mesh data to an XDMF fiel.
!> @param thisXDMFFile the XDMF file type object
!> @param strpath the string holding the path to the XDMF file
!> @param mesh the XDMF mesh object
!>
SUBROUTINE exportToDisk_XDMFFileType(thisXDMFFile, strpath, mesh)
  CHARACTER(LEN=*),PARAMETER :: myName='exportToDisk_XDMFFileType'
  CLASS(XDMFFileType),INTENT(IN) :: thisXDMFFile
  TYPE(StringType),INTENT(INOUT) :: strpath
  TYPE(XDMFMeshType),INTENT(INOUT)  :: mesh
  TYPE(XMLFileType) :: xml
  TYPE(HDF5FileType) :: h5
  TYPE(XMLElementType),POINTER :: xmle 
  TYPE(StringType) :: str_name, str_value
  INTEGER(SNK) :: i
  CHARACTER(LEN=200) :: charpath

  ! Create HDF5 file
  i = LEN_TRIM(strpath)
  charpath = CHAR(strpath)
  CALL h5%init(charpath(1:i-4)//"h5",'NEW')
  CALL h5%fopen()

  ! Create XML file
  CALL xml%init(ADJUSTL(strpath),.FALSE.)
  xmle => xml%root
  REQUIRE(ASSOCIATED(xmle))
  !   Set Xdmf
  str_name='Xdmf'
  CALL xmle%setName(str_name)
  str_name='Version'
  str_value = '3.0'
  CALL xmle%setAttribute(str_name, str_value)
  !   Set Domain
  ALLOCATE(xmle%children(1))
  str_name='Domain'
  CALL xmle%children(1)%setName(str_name)
  xmle%children(1)%parent => xml%root
  xmle%children(1)%nAttr = 0
  ! Setup the grid that contains everything
  xmle => xmle%children(1)
  ALLOCATE(xmle%children(1))
  str_name="Grid"
  CALL xmle%children(1)%setName(str_name)
  xmle%children(1)%parent => xmle
  xmle%children(1)%nAttr = 0
  str_name='Name'
  str_value = mesh%name
  CALL xmle%children(1)%setAttribute(str_name, str_value)
  str_name='GridType'
  str_value = 'Tree'
  CALL xmle%children(1)%setAttribute(str_name, str_value)

  ! Recursively add xml elements for each grid. Only the leaves have vertices,
  ! so only the leaves have HDF5 groups/data.
  xmle => xmle%children(1)
  CALL create_xml_hierarchy_XDMFFileType(mesh, xmle, strpath, h5)

  ! Finish up
  CALL xml%exportToDisk(CHAR(strpath))
  CALL h5%fclose()

ENDSUBROUTINE exportToDisk_XDMFFileType
#endif
ENDMODULE FileType_XDMF
