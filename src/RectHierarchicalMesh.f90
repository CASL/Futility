!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module defines objects for representing an XDMF file as a
!> rectangularly partitioned hierarchical mesh
!>
!> This module reads an XDMF file and stores the information in a rectangularly 
!> partitioned hierarchical mesh type. It can also write the hierarchical mesh 
!> to XDMF.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE RectHierarchicalMesh
#include "Futility_DBC.h"
USE ExceptionHandler
USE Futility_DBC
USE ISO_FORTRAN_ENV
USE IntrType
USE Strings
USE FileType_XML
USE FileType_HDF5
USE ParameterLists
USE Sorting
USE Geom
USE XDMFMesh 

IMPLICIT NONE
PRIVATE

#ifdef FUTILITY_HAVE_HDF5
! Public members
PUBLIC :: RectHierarchicalMeshType
PUBLIC :: RectHierarchicalMeshTypePtrArry
PUBLIC :: ImportRectXDMFMesh
!PUBLIC :: ExportXDMFMesh
PUBLIC :: ASSIGNMENT(=)

!> The module name
CHARACTER(LEN=*),PARAMETER :: modName='RectHierarchicalMesh'
!
!> Exception handler for the module
TYPE(ExceptionHandlerType),SAVE :: eRHM
!
!> Type to hold the XDMF mesh data
TYPE :: RectHierarchicalMeshType
  !> The bounding box for the mesh
  TYPE(Quadrilateral_2D) :: bb
  !> The mesh, if this is a leaf node
  TYPE(XDMFMeshType_2D) :: mesh
  !> Parent mesh
  TYPE(RectHierarchicalMeshType), POINTER :: parent => NULL()
  !> Child meshes
  TYPE(RectHierarchicalMeshType), POINTER :: children(:) => NULL()
  !> Map of the children within the parent
  !> Indexing looks like this:
  !> y
  !> | (1,3)  (2,3)
  !> | (1,2)  (2,2)
  !> | (1,1)  (2,1)
  !> +------> x
  INTEGER(SIK),ALLOCATABLE :: map(:,:)
  CONTAINS
    !> @copybrief RectHierarchicalMeshType::clear_RectHierarchicalMeshType
    !> @copydoc RectHierarchicalMeshType::clear_RectHierarchicalMeshType
    PROCEDURE,PASS :: clear => clear_RectHierarchicalMeshType
    !> @copybrief RectHierarchicalMeshType::nonRecusriveClear_RectHierarchicalMeshType
    !> @copydoc RectHierarchicalMeshType::nonRecursiveClear_RectHierarchicalMeshType
    PROCEDURE,PASS :: nonRecursiveClear => &
    nonRecursiveClear_RectHierarchicalMeshType
    !> @copybrief RectHierarchicalMeshType::distanceToLeaf_RectHierarchicalMeshType
    !> @copydoc RectHierarchicalMeshType::distanceToLeaf_RectHierarchicalMeshType
    PROCEDURE,PASS :: distanceToLeaf => distanceToLeaf_RectHierarchicalMeshType
    !> @copybrief RectHierarchicalMeshType::recomputeBoundingBox_RectHierarchicalMeshType
    !> @copydoc RectHierarchicalMeshType::recomputeBoundingBox_RectHierarchicalMeshType
    PROCEDURE,PASS :: recomputeBoundingBox => &
    recomputeBoundingBox_RectHierarchicalMeshType
    !> @copybrief RectHierarchicalMeshType::setupRectangularMap_RectHierarchicalMeshType
    !> @copydoc RectHierarchicalMeshType::setupRectangularMap_RectHierarchicalMeshType
    PROCEDURE,PASS :: setupRectangularMap => &
    setupRectangularMap_RectHierarchicalMeshType
!    !> @copybrief RectHierarchicalMeshType::setupEdges_RectHierarchicalMeshType
!    !> @copydoc RectHierarchicalMeshType::setupEdges_RectHierarchicalMeshType
!    PROCEDURE,PASS :: setupEdges => setupEdges_RectHierarchicalMeshType
!    !> @copybrief RectHierarchicalMeshType::clearEdges_RectHierarchicalMeshType
!    !> @copydoc RectHierarchicalMeshType::clearEdges_RectHierarchicalMeshType
!    PROCEDURE,PASS :: clearEdges => clearEdges_RectHierarchicalMeshType
    !> @copybrief RectHierarchicalMeshType::getNLeaves_RectHierarchicalMeshType
    !> @copydoc RectHierarchicalMeshType::getNLeaves_RectHierarchicalMeshType
    PROCEDURE,PASS :: getNLeaves => getNLeaves_RectHierarchicalMeshType
!    !> @copybrief RectHierarchicalMeshType::getLeaves_RectHierarchicalMeshType
!    !> @copydoc RectHierarchicalMeshType::getLeaves_RectHierarchicalMeshType
!    PROCEDURE,PASS :: getLeaves => getLeaves_RectHierarchicalMeshType
    !> @copybrief RectHierarchicalMeshType::getNNodesAtDepth_RectHierarchicalMeshType
    !> @copydoc RectHierarchicalMeshType::getNNodesAtDepth_RectHierarchicalMeshType
    PROCEDURE,PASS :: getNNodesAtDepth => &
    getNNodesAtDepth_RectHierarchicalMeshType
    !> @copybrief RectHierarchicalMeshType::getNodesAtDepth_RectHierarchicalMeshType
    !> @copydoc RectHierarchicalMeshType::getNodesAtDepth_RectHierarchicalMeshType
    PROCEDURE,PASS :: getNodesAtDepth => getNodesAtDepth_RectHierarchicalMeshType
!    !> @copybrief RectHierarchicalMeshType::getCellArea_RectHierarchicalMeshType
!    !> @copydoc RectHierarchicalMeshType::getCellArea_RectHierarchicalMeshType
!    PROCEDURE,PASS :: getCellArea => getCellArea_RectHierarchicalMeshType
!    !> @copybrief RectHierarchicalMeshType::pointInsideCell_RectHierarchicalMeshType
!    !> @copydoc RectHierarchicalMeshType::pointInsideCell_RectHierarchicalMeshType
!    PROCEDURE,PASS :: pointInsideCell => pointInsideCell_RectHierarchicalMeshType
ENDTYPE RectHierarchicalMeshType

!> To allow an array of pointers to XDMF meshes
TYPE :: RectHierarchicalMeshTypePtrArry
  TYPE(RectHierarchicalMeshType), POINTER :: RHM => NULL()
ENDTYPE RectHierarchicalMeshTypePtrArry

!> @brief Interface for assignment operator (=)
INTERFACE ASSIGNMENT(=)
  !> @copybrief RectHierarchicalMesh::assign_RectHierarchicalMeshType
  !> @copydoc RectHierarchicalMesh::assign_RectHierarchicalMeshType
  MODULE PROCEDURE assign_RectHierarchicalMeshType
ENDINTERFACE
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Returns the hdf5 group where heavy data is stored
!> @param xmle XML element storing the file path and group to the heavy data
!> @returns group The hdf5 group where the heavy data is stored
!>
FUNCTION getH5GroupFromXMLContent(xmle) RESULT(group)
  TYPE(XMLElementType),INTENT(IN) :: xmle
  TYPE(StringType) :: content, group
  TYPE(StringType), ALLOCATABLE :: segments(:)
  ! Content of xmle should be the h5 filename with the
  ! path to the data
  content=xmle%getContent()
  ! Split file from group data
  segments=content%split(':')
  ! Just grab the group data
  group=segments(2)%substr(2,LEN(segments(2)))
  ! Replace newline char
  group = group%replace(NEW_LINE("A"),"")
ENDFUNCTION getH5GroupFromXMLContent
!
!-------------------------------------------------------------------------------
!> @brief Create the RHM object
!> @param mesh the parent mesh
!> @param xmle the child XML element
!> @param h5 the HDF5 fil containing mesh data
!>
RECURSIVE SUBROUTINE create_RHM_from_file(RHM, xmle, h5)
  CHARACTER(LEN=*),PARAMETER :: myName='create_RHM_from_file'
  TYPE(RectHierarchicalMeshType),TARGET, INTENT(INOUT)  :: RHM
  TYPE(XMLElementType), INTENT(INOUT) :: xmle
  TYPE(HDF5FileType), INTENT(INOUT) :: h5
  TYPE(XMLElementType), POINTER :: xmle_children(:)
  TYPE(StringType) :: strIn, strOut
  INTEGER(SIK) :: i, nGrid, iMesh

  ! If this xml element has children
  IF(xmle%hasChildren()) THEN
    ! Determine the number or XML children that are grids
    CALL xmle%getChildren(xmle_children)
    nGrid = 0
    DO i=1,SIZE(xmle_children)
      IF(xmle_children(i)%name%upper() == 'GRID') nGrid = nGrid + 1
    ENDDO

    ! If some children are grids, it is not a leaf
    IF(nGrid > 0) THEN
      ! Allocate children for current RHM and create entities
      ALLOCATE(RHM%children(nGrid))
      iMesh=1
      ! Recursively create children of each grid
      DO i=1,SIZE(xmle_children)
        IF(xmle_children(i)%name%upper() == 'GRID') THEN
          strIn='Name'
          CALL xmle_children(i)%getAttributeValue(strIn,strOut)
          RHM%children(iMesh)%mesh%name = strOut
          RHM%children(iMesh)%parent => RHM
          CALL create_RHM_from_file(RHM%children(iMesh), xmle_children(i), h5)
          iMesh = iMesh + 1
        ENDIF
      ENDDO
    ! If this mesh does not have grid children it is a leaf on the tree.
    ! Add vertices, cells, etc.
    ELSE
      CALL setup_leaf_RHM_from_file(RHM, xmle, h5)
    ENDIF
  ELSE
    CALL eRHM%raiseError(modName//'::'//myName// &
      ' - Expected the XML element '//RHM%mesh%name//' to have children.')
  ENDIF
ENDSUBROUTINE create_RHM_from_file
!
!-------------------------------------------------------------------------------
!> @brief Setup the leaf RHM objects which contain vertices, cells, etc.
!> @param RHM the parent mesh
!> @param xmle the child XML element
!> @param h5 the HDF5 file containing mesh data
!>
SUBROUTINE setup_leaf_RHM_from_file(RHM, xmle, h5)
  CHARACTER(LEN=*),PARAMETER :: myName='setup_leaf_RHM_from_file'
  TYPE(RectHierarchicalMeshType),INTENT(INOUT),TARGET  :: RHM
  TYPE(XMLElementType), INTENT(INOUT) :: xmle
  TYPE(HDF5FileType), INTENT(INOUT) :: h5
  TYPE(XMLElementType), POINTER :: xmle_children(:), ele_children(:)
  TYPE(StringType) :: elname, strIn, strOut, group, dtype, toponame, &
    xdmf_id_str
  TYPE(StringType),ALLOCATABLE :: strArray(:)
  INTEGER(SIK) :: nverts, ncells,ivert,i,j
  INTEGER(SIK) :: ncell_sets, xdmf_id, verts_per_cell
  INTEGER(SIK),ALLOCATABLE :: dshape(:)
  REAL(SSK),ALLOCATABLE :: vals4_2d(:,:)
  REAl(SDK),ALLOCATABLE :: vals8_2d(:,:)
  INTEGER(SIK),ALLOCATABLE :: ivals4_1d(:),ivals4_2d(:,:)
  INTEGER(SIK),ALLOCATABLE :: ivals8_1d(:),ivals8_2d(:,:)
  TYPE(XDMFCellSet), ALLOCATABLE :: cell_sets_temp(:)

  IF(.NOT.xmle%hasChildren())  CALL eRHM%raiseError(modName//'::'//myName// &
    ' - XML element for '//CHAR(RHM%mesh%name)//' should have children.')
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
        CALL eRHM%raiseError(modName//'::'//myName// &
          ' - GeometryType only supports XYZ right now.')
      ENDIF
      ! Format
      CALL xmle_children(i)%getChildren(ele_children)
      IF(SIZE(ele_children) /= 1) CALL eRHM%raiseError(modName//'::'//myName//&
        ' - Expected Geometry to have only one child.')
      strIn='Format'
      CALL ele_children(1)%getAttributeValue(strIn,strOut)
      IF(strOut /= 'HDF') THEN
        CALL eRHM%raiseError(modName//'::'//myName// &
          ' - only supports HDF5 geometry data right now.')
      ENDIF
      ! Vertex Data
      strIn='Dimensions'
      CALL ele_children(1)%getAttributeValue(strIn,strOut)
      strArray=strOut%split()
      IF(strArray(2) /= '3') CALL eRHM%raiseError(modName//'::'//myName//&
        ' - Expected vertex data to be 3 dimensional.')
      nverts=strArray(1)%stoi()
      group=getH5GroupFromXMLContent(ele_children(1))
      ! Make sure the h5 path exists
      IF(.NOT.h5%pathExists(CHAR(group)))THEN
        CALL eRHM%raiseError(modName//'::'//myName//&
          ' - HDF5 group containing vertex data does not exist in h5 file.')
      ENDIF
      group = group%replace("/", "->")
      ! Data shape
      dshape=h5%getDataShape(CHAR(group))
      IF(.NOT.(dshape(1) == 3 .AND. dshape(2) == nverts))THEN
        CALL eRHM%raiseError(modName//'::'//myName//&
          ' - HDF5 vertex data shape does not match XDMF vertex data shape.')
      ENDIF
      ! Data type
      dtype=h5%getDataType(CHAR(group))
      IF(dtype == 'SSK') THEN
        CALL h5%fread(CHAR(group),vals4_2d)
      ELSE
        CALL h5%fread(CHAR(group),vals8_2d)
      ENDIF
      ALLOCATE(RHM%mesh%points(nverts))
      IF(dtype == 'SSK') THEN
        DO j = 1, nverts
          CALL RHM%mesh%points(j)%init(DIM=2, X=DBLE(vals4_2d(1, j)), &
                                              Y=DBLE(vals4_2d(2, j)))
        ENDDO
        DEALLOCATE(vals4_2d)
      ELSE
        DO j = 1, nverts
          CALL RHM%mesh%points(j)%init(DIM=2, X=vals8_2d(1, j), &
                                              Y=vals8_2d(2, j))
        ENDDO
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
        IF(SIZE(ele_children) /= 1) CALL eRHM%raiseError(modName//'::'//myName//&
          ' - Expected Topology to have only one child.')
        strIn='Format'
        CALL ele_children(1)%getAttributeValue(strIn,strOut)
        IF(strOut /= 'HDF') THEN
          CALL eRHM%raiseWarning(modName//'::'//myName// &
            ' - only supports HDF5 topology data right now.')
        ENDIF
        ! Topology Data
        strIn='NumberOfElements'
        CALL xmle_children(i)%getAttributeValue(strIn,strOut)
        ncells=strOut%stoi()
        group=getH5GroupFromXMLContent(ele_children(1))
        ! Make sure the h5 path exists
        IF(.NOT.h5%pathExists(CHAR(group)))THEN
          CALL eRHM%raiseError(modName//'::'//myName//&
            ' - HDF5 group containing topology data does not exist in h5 file.')
        ENDIF
        group = group%replace("/", "->")
        ! Data shape
        dshape=h5%getDataShape(CHAR(group))
        IF(SIZE(dshape) /= 1)THEN
          CALL eRHM%raiseError(modName//'::'//myName//&
            ' - HDF5 mixed topology data shape does not match XDMF data shape.')
        ENDIF
        ! Data type
        dtype=h5%getDataType(CHAR(group))
        IF(dtype == 'SIK') THEN
          CALL h5%fread(CHAR(group),ivals4_1d)
        ELSE
          CALL h5%fread(CHAR(group),ivals8_1d)
        ENDIF
        ALLOCATE(RHM%mesh%cells(ncells))
        ivert = 1
        IF(dtype == 'SIK') THEN
          DO j=1,ncells
            xdmf_id = ivals4_1d(ivert)
            xdmf_id_str = xdmf_id
            IF(.NOT.XDMFTopologyList%has('XDMFID->'//ADJUSTL(xdmf_id_str))) THEN
              CALL eRHM%raiseError(modName//'::'//myName//&
                ' - Topology type '//TRIM(xdmf_id_str)//' not supported')
            ELSE
              CALL XDMFTopologyList%get('XDMFID->'//ADJUSTL(xdmf_id_str), toponame)
              CALL XDMFTopologyList%get(ADJUSTL(toponame)//'->n', nverts)
            ENDIF
            ALLOCATE(RHM%mesh%cells(j)%point_list(nverts+1))
            RHM%mesh%cells(j)%point_list(1) = xdmf_id
            RHM%mesh%cells(j)%point_list(2:nverts+1) = ivals4_1d(ivert:ivert+nverts) + 1
            ivert = ivert + nverts
          ENDDO
          DEALLOCATE(ivals4_1d)
        ELSE
          DO j=1,ncells
            ! This number should be well below HUGE(ivals8_1d), so narrowing
            ! is not a concern
            xdmf_id = ivals8_1d(ivert)
            xdmf_id_str = xdmf_id
            IF(.NOT.XDMFTopologyList%has('XDMFID->'//ADJUSTL(xdmf_id_str))) THEN
              CALL eRHM%raiseError(modName//'::'//myName//&
                ' - Topology type '//TRIM(xdmf_id_str)//' not supported')
            ELSE
              CALL XDMFTopologyList%get('XDMFID->'//ADJUSTL(xdmf_id_str), toponame)
              CALL XDMFTopologyList%get(ADJUSTL(toponame)//'->n', &
                verts_per_cell)
            ENDIF
            ALLOCATE(RHM%mesh%cells(j)%point_list(verts_per_cell+1))
            RHM%mesh%cells(j)%point_list(1) = xdmf_id
            RHM%mesh%cells(j)%point_list(2:verts_per_cell+1) = &
              ivals8_1d(ivert+1:ivert+verts_per_cell) + 1
            ivert = ivert + verts_per_cell + 1
          ENDDO
          DEALLOCATE(ivals8_1d)
        ENDIF
      ELSE
        ! Single topology
        IF(.NOT.XDMFTopologyList%has(CHAR(toponame))) CALL eRHM%raiseError(modName// &
          '::'//myName//' - Topology type '//TRIM(strOut)//' not supported')
        ! XDMF ID
        CALL XDMFTopologyList%get(CHAR(toponame)//'->id', xdmf_id)
        ! Format
        CALL xmle_children(i)%getChildren(ele_children)
        IF(SIZE(ele_children) /= 1) CALL eRHM%raiseError(modName//'::'//myName//&
          ' - Expected Topology to have only one child.')
        strIn='Format'
        CALL ele_children(1)%getAttributeValue(strIn,strOut)
        IF(strOut /= 'HDF') THEN
          CALL eRHM%raiseWarning(modName//'::'//myName// &
            ' - only supports HDF5 topology data right now.')
        ENDIF
        ! Topology Data
        strIn='NumberOfElements'
        CALL xmle_children(i)%getAttributeValue(strIn,strOut)
        ncells=strOut%stoi()
        strIn='NodesPerElement'
        CALL xmle_children(i)%getAttributeValue(strIn,strOut)
        nverts=strOut%stoi()
        group=getH5GroupFromXMLContent(ele_children(1))
        ! Make sure the h5 path exists
        IF(.NOT.h5%pathExists(CHAR(group)))THEN
          CALL eRHM%raiseError(modName//'::'//myName//&
            ' - HDF5 group containing topology data does not exist in h5 file.')
        ENDIF
        group = group%replace("/", "->")
        ! Data shape
        dshape=h5%getDataShape(CHAR(group))
        IF(.NOT.(dshape(1) == nverts .AND. dshape(2) == ncells))THEN
          CALL eRHM%raiseError(modName//'::'//myName//&
            ' - HDF5 mixed topology data shape does not match XDMF data shape.')
        ENDIF
        ! Data type
        dtype=h5%getDataType(CHAR(group))
        IF(dtype == 'SIK') THEN
          CALL h5%fread(CHAR(group),ivals4_2d)
        ELSE
          CALL h5%fread(CHAR(group),ivals8_2d)
        ENDIF
        ALLOCATE(RHM%mesh%cells(ncells))
        IF(dtype == 'SIK') THEN
          DO j=1,ncells
            ALLOCATE(RHM%mesh%cells(j)%point_list(nverts + 1))
            RHM%mesh%cells(j)%point_list(1) = xdmf_id
            ! Account for 0 based to 1 based index switch
            RHM%mesh%cells(j)%point_list(2:) = ivals4_2d(:, j) + 1
          ENDDO
          DEALLOCATE(ivals4_2d)
        ELSE
          DO j=1,ncells
            ALLOCATE(RHM%mesh%cells(j)%point_list(nverts + 1))
            RHM%mesh%cells(j)%point_list(1) = xdmf_id
            ! Account for 0 based to 1 based index switch
            RHM%mesh%cells(j)%point_list(2:) = ivals8_2d(:, j) + 1
          ENDDO
          DEALLOCATE(ivals8_2d)
        ENDIF
      ENDIF
    CASE("ATTRIBUTE")
      strIn='Name'
      CALL xmle_children(i)%getAttributeValue(strIn,strOut)
      IF(strOut%upper() == 'MATERIALID') THEN
        ! Format
        CALL xmle_children(i)%getChildren(ele_children)
        IF(SIZE(ele_children) /= 1) CALL eRHM%raiseError(modName//'::'//myName//&
          ' - Expected Attribute to have only one child.')
        strIn='Format'
        CALL ele_children(1)%getAttributeValue(strIn,strOut)
        IF(strOut /= 'HDF') THEN
          CALL eRHM%raiseWarning(modName//'::'//myName// &
            ' - only supports HDF5 material data right now.')
        ENDIF
        ! Material Data
        strIn='Dimensions'
        CALL ele_children(1)%getAttributeValue(strIn,strOut)
        ncells=strOut%stoi()
        IF(.NOT.(ALLOCATED(RHM%mesh%cells) .AND. ncells == SIZE(RHM%mesh%cells))) THEN
          CALL eRHM%raiseError(modName//'::'//myName//&
            ' - material data is before topology data, or is the wrong size.')
        ENDIF
        group=getH5GroupFromXMLContent(ele_children(1))
        ! Make sure the h5 path exists
        IF(.NOT.h5%pathExists(CHAR(group)))THEN
          CALL eRHM%raiseError(modName//'::'//myName//&
            ' - HDF5 group containing material data does not exist in h5 file.')
        ENDIF
        group = group%replace("/", "->")
        ! Data shape
        dshape=h5%getDataShape(CHAR(group))
        IF(.NOT.(SIZE(dshape) == 1 .AND. dshape(1) == ncells)) THEN
          CALL eRHM%raiseError(modName//'::'//myName//&
            ' - material data in h5 file is the wrong size or shape.')
        ENDIF
        ! Data type
        dtype=h5%getDataType(CHAR(group))
        IF(dtype == 'SIK') THEN
          CALL h5%fread(CHAR(group),ivals4_1d)
        ELSE
          CALL h5%fread(CHAR(group),ivals8_1d)
        ENDIF
        ALLOCATE(RHM%mesh%material_ids(ncells))
        IF(dtype == 'SIK') THEN
          ! Account for 0 based to 1 based index switch
          RHM%mesh%material_ids = ivals4_1d + 1
          DEALLOCATE(ivals4_1d)
        ELSE
          ! Account for 0 based to 1 based index switch
          ! material ids will not exceed MAX(INTEGER(4)),
          ! so narrowing will not occur.
          RHM%mesh%material_ids = ivals8_1d + 1
          DEALLOCATE(ivals8_1d)
        ENDIF
      ELSE
        CALL eRHM%raiseWarning(modName//'::'//myName//' - mesh attribute '//&
          TRIM(strOut)//' not supported')
      ENDIF
    CASE("SET")
      ! SetType
      strIn='SetType'
      CALL xmle_children(i)%getAttributeValue(strIn,strOut)
      IF(strOut /= 'Cell') THEN
        CALL eRHM%raiseWarning(modName//'::'//myName// &
          ' - only supports SetType="Cell" right now.')
      ENDIF
      ! SetName
      strIn='Name'
      CALL xmle_children(i)%getAttributeValue(strIn,elname)
      ! Format
      CALL xmle_children(i)%getChildren(ele_children)
      IF(SIZE(ele_children) /= 1) CALL eRHM%raiseError(modName//'::'//myName//&
        ' - Expected Set to have only one child.')
      strIn='Format'
      strIn='Format'
      CALL ele_children(1)%getAttributeValue(strIn,strOut)
      IF(strOut /= 'HDF') THEN
        CALL eRHM%raiseWarning(modName//'::'//myName// &
          ' - only supports HDF5 cell set data right now.')
      ENDIF
      ! Cell Set Data
      strIn='Dimensions'
      CALL ele_children(1)%getAttributeValue(strIn,strOut)
      ncells=strOut%stoi()
      IF(.NOT.(ALLOCATED(RHM%mesh%cells) .AND. ncells <= SIZE(RHM%mesh%cells))) THEN
        CALL eRHM%raiseError(modName//'::'//myName//&
          ' - material data is before topology data, or is too big.')
      ENDIF
      group=getH5GroupFromXMLContent(ele_children(1))
      ! Make sure the h5 path exists
      IF(.NOT.h5%pathExists(CHAR(group)))THEN
        CALL eRHM%raiseError(modName//'::'//myName//&
          ' - HDF5 group containing set data does not exist in h5 file.')
      ENDIF
      group = group%replace("/", "->")
      ! Data shape
      dshape=h5%getDataShape(CHAR(group))
      IF(.NOT.(SIZE(dshape) == 1 .AND. dshape(1) == ncells)) THEN
        CALL eRHM%raiseError(modName//'::'//myName//&
          ' - set data in h5 file is the wrong size or shape.')
      ENDIF
      ! Data type
      dtype=h5%getDataType(CHAR(group))
      IF(dtype == 'SIK') THEN
        CALL h5%fread(CHAR(group),ivals4_1d)
      ELSE
        CALL h5%fread(CHAR(group),ivals8_1d)
      ENDIF
      ! Resize cell sets if needed
      ! This is expected to happen infrequently
      IF(ALLOCATED(RHM%mesh%cell_sets)) THEN
        ! Copy current sets to temp, deallocate current sets
        ncell_sets = SIZE(RHM%mesh%cell_sets)
        ALLOCATE(cell_sets_temp(ncell_sets))
        DO j=1, ncell_sets
          ALLOCATE(cell_sets_temp(j)%cell_list(SIZE(RHM%mesh%cell_sets(j)%cell_list)))
          cell_sets_temp(j)%cell_list = RHM%mesh%cell_sets(j)%cell_list
          cell_sets_temp(j)%name = RHM%mesh%cell_sets(j)%name
          DEALLOCATE(RHM%mesh%cell_sets(j)%cell_list)
        ENDDO
        DEALLOCATE(RHM%mesh%cell_sets)
        ! Reallocate cell sets to be on bigger and copy all old sets over
        ALLOCATE(RHM%mesh%cell_sets(ncell_sets+1))
        DO j=1, ncell_sets
          ALLOCATE(RHM%mesh%cell_sets(j)%cell_list(SIZE(cell_sets_temp(j)%cell_list)))
          RHM%mesh%cell_sets(j)%cell_list = cell_sets_temp(j)%cell_list
          RHM%mesh%cell_sets(j)%name = cell_sets_temp(j)%name
          DEALLOCATE(cell_sets_temp(j)%cell_list)
          CALL cell_sets_temp(j)%name%clear()
        ENDDO
        DEALLOCATE(cell_sets_temp)
      ELSE
        ncell_sets = 0
        ALLOCATE(RHM%mesh%cell_sets(1))
      ENDIF
      ! Add the one new cell set
      RHM%mesh%cell_sets(ncell_sets + 1)%name = elname
      ALLOCATE(RHM%mesh%cell_sets(ncell_sets + 1)%cell_list(ncells))
      IF(dtype == 'SIK') THEN
        ! Account for 0 based to 1 based index switch
        RHM%mesh%cell_sets(ncell_sets + 1)%cell_list = ivals4_1d + 1
        DEALLOCATE(ivals4_1d)
      ELSE
        RHM%mesh%cell_sets(ncell_sets + 1)%cell_list = ivals8_1d + 1
        DEALLOCATE(ivals8_1d)
      ENDIF
    CASE DEFAULT
      CALL eRHM%raiseWarning(modName//'::'//myName// &
        ' - Unsupported data in XDMF file '//CHAR(elname))
    ENDSELECT
  ENDDO
ENDSUBROUTINE setup_leaf_RHM_from_file
!
!-------------------------------------------------------------------------------
!> @brief Imports the mesh data in the file to a RHM object.
!> @param strpath the string holding the path to the XDMF file
!> @param RHM the RectHierarchicalMeshType
!>
SUBROUTINE importRectXDMFMesh(strpath, RHM)
  CHARACTER(LEN=*),PARAMETER :: myName='importRectXDMFMesh'
  TYPE(StringType),INTENT(INOUT) :: strpath
  TYPE(RectHierarchicalMeshType),INTENT(OUT),TARGET  :: RHM
  TYPE(XMLFileType) :: xml
  TYPE(HDF5FileType) :: h5
  TYPE(XMLElementType),POINTER :: xmle, children(:)
  TYPE(StringType) :: strIn, strOut
  INTEGER(SIK) :: i, gridIdx
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
  IF(.NOT.ASSOCIATED(xmle)) CALL eRHM%raiseError(modName//'::'//myName// &
    ' - XML data import encountered an error. Pointer to root not associated.')
  IF(.NOT.xmle%name%upper() == 'XDMF') CALL eRHM%raiseError(modName//'::'//&
    myName//' - Expected XDMF XML element to be the root element.')

  ! Version
  strIn='Version'
  CALL xmle%getAttributeValue(strIn,strOut)
  IF(strOut /= '3.0') THEN
    CALL eRHM%raiseError(modName//'::'//myName// &
      ' - Currently only supports XDMF version 3.0')
  ENDIF

  ! Domain
  CALL xmle%getChildren(children)
  IF(.NOT.(SIZE(children) == 1 .AND. children(1)%name%upper() == 'DOMAIN'))THEN
    CALL eRHM%raiseError(modName//'::'//myName// &
    ' - Expected XDMF XML element to have one child (Domain).')
  ENDIF

  ! Information
  ! NOTE: It is assumed that material information is before any grids
  ! and that all grids are contained in one overall grid.
  CALL children(1)%getChildren(children)
  IF (SIZE(children) == 2) THEN
    IF(.NOT.children(1)%name%upper() == 'INFORMATION')THEN
      CALL eRHM%raiseError(modName//'::'//myName// &
        ' - Expected Domain XML element to have one Information before Grid.')
    ENDIF
    IF(.NOT.children(2)%name%upper() == 'GRID')THEN
      CALL eRHM%raiseError(modName//'::'//myName// &
        ' - Expected Domain XML element to have Grid after Information.')
    ENDIF
    gridIdx = 2
  ELSE IF(SIZE(children) == 1) THEN
    IF(.NOT.children(1)%name%upper() == 'GRID')THEN
      CALL eRHM%raiseError(modName//'::'//myName// &
        ' - Expected Domain XML element to have Grid child.')
    ENDIF
    gridIdx = 1
  ELSE
    CALL eRHM%raiseError(modName//'::'//myName// &
      ' - Expecting information and grid elements only.')
  ENDIF

  ! Init root RHM
  strIn="Name"
  CALL children(gridIdx)%getAttributeValue(strIn,strOut)
  RHM%mesh%name = strOut

  ! Create grids
  CALL create_RHM_from_file(RHM, children(gridIdx), h5)

  ! Setup bounding boxes
  CALL RHM%recomputeBoundingBox()

  ! Setup map
  CALL RHM%setupRectangularMap()
!
!  ! Setup edges
!  CALL mesh%setupEdges()
ENDSUBROUTINE importRectXDMFMesh
!
!-------------------------------------------------------------------------------
!> @brief Clears the RHM
!> @param thismesh the RHM
!>
RECURSIVE SUBROUTINE clear_RectHierarchicalMeshType(thismesh)
  CLASS(RectHierarchicalMeshType), INTENT(INOUT) :: thismesh
  INTEGER(SIK) :: i

  CALL thismesh%mesh%clear()
  DO i = 1, 4
    CALL thismesh%bb%points(i)%clear()
  ENDDO
  IF(ALLOCATED(thismesh%map)) DEALLOCATE(thismesh%map)
  IF(ASSOCIATED(thismesh%parent)) thismesh%parent => NULL()
  IF(ASSOCIATED(thismesh%children)) THEN
    DO i=1,SIZE(thismesh%children)
      CALL thismesh%children(i)%clear()
    ENDDO
    thismesh%children => NULL()
  ENDIF
ENDSUBROUTINE clear_RectHierarchicalMeshType
!
!-------------------------------------------------------------------------------
!> @brief Clears the RHM, without recursing to children
!> @param thismesh the RHM object
!>
RECURSIVE SUBROUTINE nonRecursiveClear_RectHierarchicalMeshType(thismesh)
  CLASS(RectHierarchicalMeshType), INTENT(INOUT) :: thismesh
  INTEGER(SIK) :: i

  CALL thismesh%mesh%clear()
  DO i = 1, 4                          
    CALL thismesh%bb%points(i)%clear()
  ENDDO
  IF(ALLOCATED(thismesh%map)) DEALLOCATE(thismesh%map)
  IF(ASSOCIATED(thismesh%parent)) thismesh%parent => NULL()
  IF(ASSOCIATED(thismesh%children)) THEN
    thismesh%children => NULL()
  ENDIF
ENDSUBROUTINE nonRecursiveClear_RectHierarchicalMeshType
!
!-------------------------------------------------------------------------------
!> @brief Gets the number of grid levels to a leaf node
!> @param thismesh the RHM object
!> @returns n the number of levels to a leaf node
!>
RECURSIVE FUNCTION distanceToLeaf_RectHierarchicalMeshType(thismesh) RESULT(n)
  CLASS(RectHierarchicalMeshType), INTENT(INOUT) :: thismesh
  INTEGER(SIK) :: n
  ! It is assumed that all siblings have children if any sibling has children
  ! Hence it is sufficient to assess thismesh%children(1)'s depth recursively
  ! Ex:     Possible                Not Possible
  !           L1                        L1
  !          /  \                      /  \
  !      L2_1   L2_2               L2_1   L2_2 <-- Should have children.
  !     /  \     |                /  \
  ! L3_1  L3_2  L3_3           L3_1  L3_2
  IF(ASSOCIATED(thismesh%children))THEN
    n = thismesh%children(1)%distanceToLeaf() + 1
  ELSE
    n = 0
  ENDIF
ENDFUNCTION distanceToLeaf_RectHierarchicalMeshType
!
!-------------------------------------------------------------------------------
!> @brief Recompute the bounding box for this mesh and all children.
!> @param thismesh the RHM object
!>
RECURSIVE SUBROUTINE recomputeBoundingBox_RectHierarchicalMeshType(thismesh)
  CLASS(RectHierarchicalMeshType), INTENT(INOUT) :: thismesh
  REAL(SDK) :: xmin, xmax, ymin, ymax
  INTEGER(SIK) :: i
  xmin = HUGE(xmin)
  xmax = -HUGE(xmax)
  ymin = HUGE(ymin)
  ymax = -HUGE(ymax)
  IF(ASSOCIATED(thismesh%children))THEN
    DO i = 1, SIZE(thismesh%children)
      CALL thismesh%children(i)%recomputeBoundingBox()
    ENDDO
    DO i = 1, SIZE(thismesh%children)
      IF(thismesh%children(i)%bb%points(1)%coord(1) < xmin) &
        xmin = thismesh%children(i)%bb%points(1)%coord(1)
      IF(thismesh%children(i)%bb%points(3)%coord(1) > xmax) &
        xmax = thismesh%children(i)%bb%points(3)%coord(1)
      IF(thismesh%children(i)%bb%points(1)%coord(2) < ymin) &
        ymin = thismesh%children(i)%bb%points(1)%coord(2)
      IF(thismesh%children(i)%bb%points(3)%coord(2) > ymax) &
        ymax = thismesh%children(i)%bb%points(3)%coord(2)
    ENDDO
  ELSE
    DO i = 1, SIZE(thismesh%mesh%points)
      IF(thismesh%mesh%points(i)%coord(1) < xmin) xmin = thismesh%mesh%points(i)%coord(1)
      IF(thismesh%mesh%points(i)%coord(1) > xmax) xmax = thismesh%mesh%points(i)%coord(1)
      IF(thismesh%mesh%points(i)%coord(2) < ymin) ymin = thismesh%mesh%points(i)%coord(2)
      IF(thismesh%mesh%points(i)%coord(2) > ymax) ymax = thismesh%mesh%points(i)%coord(2)
    ENDDO
  ENDIF
  DO i = 1, 4
    CALL thismesh%bb%points(i)%clear()
  ENDDO
  CALL thismesh%bb%points(1)%init(DIM=2, X=xmin, Y=ymin)
  CALL thismesh%bb%points(2)%init(DIM=2, X=xmax, Y=ymin)
  CALL thismesh%bb%points(3)%init(DIM=2, X=xmax, Y=ymax)
  CALL thismesh%bb%points(4)%init(DIM=2, X=xmin, Y=ymax)
ENDSUBROUTINE recomputeBoundingBox_RectHierarchicalMeshType
!
!-------------------------------------------------------------------------------
!> @brief Assigns an RHM type to another
!> @param thismesh the RHM object being assigned to
!> @param thatmesh the RHM object being assigned from
!>
RECURSIVE SUBROUTINE assign_RectHierarchicalMeshType(thismesh, thatmesh)
  TYPE(RectHierarchicalMeshType), INTENT(INOUT) :: thismesh
  TYPE(RectHierarchicalMeshType), INTENT(IN) :: thatmesh
  INTEGER(SIK) :: i,j

  thismesh%mesh = thatmesh%mesh
  DO i = 1, 4
    thismesh%bb%points(i) = thatmesh%bb%points(i)
  ENDDO
  IF(ALLOCATED(thatmesh%map))THEN
    i = SIZE(thatmesh%map, DIM=1)
    j = SIZE(thatmesh%map, DIM=2)
    IF(ALLOCATED(thismesh%map)) DEALLOCATE(thismesh%map)
    ALLOCATE(thismesh%map(i,j))
    thismesh%map = thatmesh%map
  ENDIF
  IF(ASSOCIATED(thatmesh%parent)) thismesh%parent => thatmesh%parent
  ! NOTE: Children cannot be recursively cleared without risk of
  ! modifying other mesh objects due to the pointer to other meshes.
  ! Therefore, it is assumed that one will manually clear a mesh
  ! if the children are to be deleted.
  IF(ASSOCIATED(thatmesh%children)) THEN
    ALLOCATE(thismesh%children(SIZE(thatmesh%children)))
    thismesh%children => thatmesh%children
  ENDIF
ENDSUBROUTINE assign_RectHierarchicalMeshType
!
!-------------------------------------------------------------------------------
!> @brief Setup a rectangular map for this mesh and all children.
!> @param thismesh the RHM object
!>
RECURSIVE SUBROUTINE setupRectangularMap_RectHierarchicalMeshType(thismesh)
  CHARACTER(LEN=*),PARAMETER :: &
  myName='setupRectangularMap_RectHierarchicalMeshType'
  CLASS(RectHierarchicalMeshType), INTENT(INOUT) :: thismesh
  INTEGER(SIK) :: i, xmin, ymin, xmax, ymax, j, x, y
  TYPE(StringType) :: meshname, xstr, ystr
  TYPE(StringType), ALLOCATABLE :: segments(:)

  IF(ALLOCATED(thismesh%map)) DEALLOCATE(thismesh%map)
  xmin = HUGE(xmin)
  ymin = HUGE(ymin)
  xmax = -HUGE(xmax)
  ymax = -HUGE(ymax)
  IF(ASSOCIATED(thismesh%children))THEN
    ! Loop through all children names and find the bottom left child
    ! and top right child (xmin, ymin) and (xmax, ymax)
    DO i = 1, SIZE(thismesh%children)
      meshname = thismesh%children(i)%mesh%name
      segments = meshname%split('_')
      xstr = segments(SIZE(segments) - 1)
      ystr = segments(SIZE(segments))
      x = xstr%stoi()
      y = ystr%stoi()
      IF(x < xmin) xmin = x
      IF(y < ymin) ymin = y
      IF(x > xmax) xmax = x
      IF(y > ymax) ymax = y
    ENDDO
    ! Check that the product of the 2D dimensions equals the 1D dimension
    IF((xmax - xmin + 1) * (ymax - ymin + 1) /= SIZE(thismesh%children)) &
      CALL eRHM%raiseError(modName//'::'//myName//' - the number of '// &
      'entries in the map ('//CHAR((xmax - xmin + 1) * (ymax - ymin + 1))// &
      ') does not equal the number of children ('//CHAR(SIZE(thismesh%children))// &
      '). Are the grid indices correct?')

    ALLOCATE(thismesh%map(xmax - xmin + 1, ymax - ymin + 1))
    thismesh%map = 0
    DO i = 1, SIZE(thismesh%children)
      meshname = thismesh%children(i)%mesh%name
      segments = meshname%split('_')
      xstr = segments(SIZE(segments) - 1)
      ystr = segments(SIZE(segments))
      x = xstr%stoi()
      y = ystr%stoi()
      thismesh%map(x - xmin + 1, y - ymin + 1) = i
    ENDDO
    ! Check that all entries are non-zero
    DO i = 1, xmax - xmin + 1
      DO j = 1, ymax - ymin + 1
        IF(thismesh%map(i,j) == 0) CALL eRHM%raiseError(modName//'::'//myName// &
          ' - An entry in the map was not assigned! Are grid indices continuous?')
      ENDDO
    ENDDO
    DO i = 1, SIZE(thismesh%children)
      CALL thismesh%children(i)%setupRectangularMap()
    ENDDO
  ENDIF
ENDSUBROUTINE setupRectangularMap_RectHierarchicalMeshType 
!!
!!-------------------------------------------------------------------------------
!!> @brief Setup the edges for this mesh and all children.
!!> @param thismesh the XDMF mesh object
!!>
!RECURSIVE SUBROUTINE setupEdges_XDMFMeshType(thismesh)
!  CHARACTER(LEN=*),PARAMETER :: myName='setupEdges_XDMFMeshType'
!  CLASS(XDMFMeshType), INTENT(INOUT) :: thismesh
!  INTEGER(SIK) :: i,j,k,xid, maxEdges, nCells, iEdge, nEdge, icell, &
!    total_nEdges, cells_in_edges
!  INTEGER(SIK) :: edge_verts(3),swap
!  INTEGER(SIK), ALLOCATABLE :: all_edge_verts(:,:), all_edge_cells(:,:)
!  LOGICAL(SBK) :: duplicate_edge
!  TYPE(PointType) :: p1, p2, p3
!  !
!  !
!  ! NOTE: There is potential for an overflow issue if the ID of a vertex exceeds
!  !   the maximum value of a 32bit integer. The point_list of each cell
!  !   defaults to 64bit integers to account for very large meshes, but the
!  !   sorting routines only accept SIK. Therefore, if this becomes an issue,
!  !   just copy and past all the sorting routines, replace SIK with SIK, and add
!  !   an interface in Sorting.f90, or just compile with 64bit integers.
!  !
!
!!)  IF(ALLOCATED(thismesh%edges)) CALL thismesh%clearEdges()
!  REQUIRE(.NOT.ALLOCATED(thismesh%edges))
!  IF(ASSOCIATED(thismesh%children))THEN
!    ! Not a leaf, recurse
!    DO i = 1, SIZE(thismesh%children)
!      CALL thismesh%children(i)%setupEdges()
!    ENDDO
!  ELSE
!    total_nEdges = 0
!    ! Leaf, setup edges
!    nCells = SIZE(thismesh%cells)
!    ! Setup oversized arrays to hold all the edges and cells
!    maxEdges = 4
!    ALLOCATE(all_edge_verts(3,maxEdges*nCells))
!    ALLOCATE(all_edge_cells(2,maxEdges*nCells))
!    all_edge_verts = -1
!    all_edge_cells = -1
!    iEdge = 0
!    ! Loop over each cell to get all unique edges
!    DO i = 1, SIZE(thismesh%cells)
!      xid = thismesh%cells(i)%point_list(1)
!      IF(xid == 4_SIK .OR. xid == 5_SIK) THEN! linear edges
!        IF(xid == 4_SIK)THEN
!          nEdge = 3
!        ELSE
!          nEdge = 4
!        ENDIF
!        ! Setup this cell's edge_list
!        ALLOCATE(thismesh%cells(i)%edge_list(nEdge))
!        thismesh%cells(i)%edge_list = -1
!        total_nEdges = total_nEdges + nEdge
!        ! For each edge
!        DO j = 2, SIZE(thismesh%cells(i)%point_list) - 1
!          edge_verts = -1
!          edge_verts(1) = thismesh%cells(i)%point_list(j)
!          edge_verts(2) = thismesh%cells(i)%point_list(j+1)
!          CALL sort(edge_verts)
!          ! If this edge is unique, add it
!          duplicate_edge = .FALSE.
!          DO k = 1, iEdge
!            IF(ALL(edge_verts == all_edge_verts(:,k))) THEN
!              duplicate_edge = .TRUE.
!              ! add this cell to edge cell list
!              all_edge_cells(2,k) = i
!              EXIT
!            ENDIF
!          ENDDO
!          IF(.NOT.duplicate_edge) THEN
!            all_edge_verts(:,iEdge + 1) = edge_verts
!            all_edge_cells(1,iEdge + 1) = i
!            iEdge = iEdge + 1
!          ENDIF
!        ENDDO
!        ! Final edge
!        edge_verts = -1
!        edge_verts(1) = thismesh%cells(i)%point_list(j)
!        edge_verts(2) = thismesh%cells(i)%point_list(2)
!        CALL sort(edge_verts)
!        duplicate_edge = .FALSE.
!        DO k = 1, iEdge
!          IF(ALL(edge_verts == all_edge_verts(:,k))) THEN
!            duplicate_edge = .TRUE.
!            all_edge_cells(2,k) = i
!            EXIT
!          ENDIF
!        ENDDO
!        IF(.NOT.duplicate_edge) THEN
!          all_edge_verts(:,iEdge + 1) = edge_verts
!          all_edge_cells(1,iEdge + 1) = i
!          iEdge = iEdge + 1
!        ENDIF
!      ELSEIF(xid == 36_SIK .OR. xid == 37_SIK) THEN ! quad edges
!        IF(xid == 36_SIK)THEN
!          nEdge = 3
!        ELSE
!          nEdge = 4
!        ENDIF
!        ! Setup this cell's edge_list
!        ALLOCATE(thismesh%cells(i)%edge_list(nEdge))
!        thismesh%cells(i)%edge_list = -1
!        total_nEdges = total_nEdges + nEdge
!        ! For each edge
!        DO j = 2, (SIZE(thismesh%cells(i)%point_list) - 1)/2
!          edge_verts(1) = thismesh%cells(i)%point_list(j)
!          edge_verts(2) = thismesh%cells(i)%point_list(j+1)
!          edge_verts(3) = thismesh%cells(i)%point_list(j+nEdge)
!          CALL sort(edge_verts)
!          ! If this edge is unique, add it
!          duplicate_edge = .FALSE.
!          DO k = 1, iEdge
!            IF(ALL(edge_verts == all_edge_verts(:,k))) THEN
!              duplicate_edge = .TRUE.
!              ! add this cell to edge cell list
!              all_edge_cells(2,k) = i
!              EXIT
!            ENDIF
!          ENDDO
!          IF(.NOT.duplicate_edge) THEN
!            all_edge_verts(:,iEdge + 1) = edge_verts
!            all_edge_cells(1,iEdge + 1) = i
!            iEdge = iEdge + 1
!          ENDIF
!        ENDDO
!        ! Last edge
!        edge_verts(1) = thismesh%cells(i)%point_list(j)
!        edge_verts(2) = thismesh%cells(i)%point_list(2)
!        edge_verts(3) = thismesh%cells(i)%point_list(2*nEdge+1)
!        CALL sort(edge_verts)
!        ! If this edge is unique, add it
!        duplicate_edge = .FALSE.
!        DO k = 1, iEdge
!          IF(ALL(edge_verts == all_edge_verts(:,k))) THEN
!            duplicate_edge = .TRUE.
!            ! add this cell to edge cell list
!            all_edge_cells(2,k) = i
!            EXIT
!          ENDIF
!        ENDDO
!        IF(.NOT.duplicate_edge) THEN
!          all_edge_verts(:,iEdge + 1) = edge_verts
!          all_edge_cells(1,iEdge + 1) = i
!          iEdge = iEdge + 1
!        ENDIF
!      ELSE
!        CALL eRHM%raiseError(modName//'::'//myName// &
!          ' - Unsupported XDMF cell type.')
!      ENDIF
!    ENDDO
!
!    ! Allocate the mesh edges
!    ALLOCATE(thismesh%edges(iEdge))
!    ! Setup each edge
!    DO i = 1, iEdge
!      IF(all_edge_verts(1,i) == -1) THEN !Linear
!        ! isLinear
!        thismesh%edges(i)%isLinear = .TRUE.
!        ! Setup the line
!        CALL p1%init(DIM=2, X=thismesh%vertices(1,all_edge_verts(2,i)), &
!                            Y=thismesh%vertices(2,all_edge_verts(2,i)))
!        CALL p2%init(DIM=2, X=thismesh%vertices(1,all_edge_verts(3,i)), &
!                            Y=thismesh%vertices(2,all_edge_verts(3,i)))
!        CALL thismesh%edges(i)%line%set(p1, p2)
!      ELSE ! quad
!        ! isLinear
!        thismesh%edges(i)%isLinear = .FALSE.
!        ! It is non-trivial to determine which point is the middle of the arc
!        ! when the arc can be rotated arbitrarily. So we simply examine one of the
!        ! cells it belongs to, to see which verts are linear. The XDMF cell
!        ! format lists linear then quadratic verts, so the last vertex we see
!        ! cycling through the vertex list is the quadratic vertex.
!        icell = all_edge_cells(1,i) ! The cell the edge belongs to
!        edge_verts = all_edge_verts(:,i)
!        ! Mark seen vert IDs as 0. The non-zero vertex is the midpoint
!        ! The order of the other two points doesn't matter.
!        DO j = 1,(SIZE(thismesh%cells(icell)%point_list) - 1)/2 ! just lin verts
!          DO k = 1,3
!            IF(edge_verts(k) == thismesh%cells(icell)%point_list(j+1)) edge_verts(k) = 0
!          ENDDO
!        ENDDO
!        ! Move the middle vert to the 3rd index of the array.
!        IF(edge_verts(3) == 0) THEN
!          DO k = 1,3
!            IF(edge_verts(k) /= 0) THEN
!              swap = edge_verts(k)
!              edge_verts(k) = 0
!              edge_verts(3) = swap
!              ! Change for the vertices as well
!              swap = all_edge_verts(k,i)
!              all_edge_verts(k,i) = all_edge_verts(3,i)
!              all_edge_verts(3,i) = swap
!            ENDIF
!          ENDDO
!        ENDIF
!        CALL p3%init(DIM=2, X=thismesh%vertices(1,all_edge_verts(3,i)), &
!                            Y=thismesh%vertices(2,all_edge_verts(3,i)))
!        CALL p2%init(DIM=2, X=thismesh%vertices(1,all_edge_verts(2,i)), &
!                            Y=thismesh%vertices(2,all_edge_verts(2,i)))
!        CALL p1%init(DIM=2, X=thismesh%vertices(1,all_edge_verts(1,i)), &
!                            Y=thismesh%vertices(2,all_edge_verts(1,i)))
!        CALL thismesh%edges(i)%quad%set(p1, p2, p3)
!        ! If the a term for the quad is less than 1.0e-3, say it's linear.
!        ! Setup the line too.
!        IF(ABS(thismesh%edges(i)%quad%a) < 1.0e-3) THEN
!          CALL p1%clear()
!          CALL p2%clear()
!          CALL p2%init(DIM=2, X=thismesh%vertices(1,all_edge_verts(2,i)), &
!                              Y=thismesh%vertices(2,all_edge_verts(2,i)))
!          CALL p1%init(DIM=2, X=thismesh%vertices(1,all_edge_verts(1,i)), &
!                              Y=thismesh%vertices(2,all_edge_verts(1,i)))
!          CALL thismesh%edges(i)%line%set(p1, p2)
!        ELSE
!        ENDIF
!      ENDIF
!      ! vertices
!      thismesh%edges(i)%vertices = all_edge_verts(:,i)
!      ! cells
!      thismesh%edges(i)%cells = all_edge_cells(:,i)
!      CALL p1%clear()
!      CALL p2%clear()
!      CALL p3%clear()
!    ENDDO
!
!    ! Perform a check to make sure the total number of edges and the sum of the
!    ! cells associated with each edge is equal.
!    cells_in_edges = 0
!    DO i = 1, iEdge
!      DO j = 1, 2
!        IF(thismesh%edges(i)%cells(j) > 0) THEN
!          cells_in_edges = cells_in_edges + 1
!        ENDIF
!      ENDDO
!    ENDDO
!    ENSURE(total_nEdges == cells_in_edges)
!
!    ! For each edge, setup the cell's edge list
!    DO i = 1, iEdge
!      DO j = 1, 2
!        IF(thismesh%edges(i)%cells(j) /= -1) THEN
!          icell = thismesh%edges(i)%cells(j)
!          DO k = 1, SIZE(thismesh%cells(icell)%edge_list)
!            IF(thismesh%cells(icell)%edge_list(k) == -1) THEN
!              thismesh%cells(icell)%edge_list(k) = i
!              EXIT
!            ENDIF
!          ENDDO
!        ENDIF
!      ENDDO
!    ENDDO
!
!    ! Check that each cell's edge list was assigned.
!    DO i = 1,nCells
!      ENSURE(.NOT.ANY(thismesh%cells(i)%edge_list == -1))
!    ENDDO
!  ENDIF
!ENDSUBROUTINE setupEdges_XDMFMeshType
!!
!!-------------------------------------------------------------------------------
!!> @brief Clears the edges of the XDMF mesh
!!> @param thismesh the XDMF mesh object
!!>
!RECURSIVE SUBROUTINE clearEdges_XDMFMeshType(thismesh)
!  CLASS(XDMFMeshType), INTENT(INOUT) :: thismesh
!  INTEGER(SIK) :: i
!
!  IF(ASSOCIATED(thismesh%children)) THEN
!    DO i=1,SIZE(thismesh%children)
!      CALL thismesh%children(i)%clearEdges()
!    ENDDO
!  ENDIF
!  IF( ALLOCATED(thismesh%edges)) THEN
!    DO i=1, SIZE(thismesh%edges)
!      CALL thismesh%edges(i)%quad%clear()
!      CALL thismesh%edges(i)%line%clear()
!    ENDDO
!    DEALLOCATE(thismesh%edges)
!  ENDIF
!  IF( ALLOCATED(thismesh%cells) ) THEN
!    DO i=1, SIZE(thismesh%cells)
!      IF( ALLOCATED(thismesh%cells(i)%edge_list) ) DEALLOCATE(thismesh%cells(i)%edge_list)
!    ENDDO
!  ENDIF
!ENDSUBROUTINE clearEdges_XDMFMeshType
!
!-------------------------------------------------------------------------------
!> @brief Gets the number of leaf nodes in this mesh
!> @param thismesh the XDMF mesh object
!> @param d the depth
!> @returns n the number of nodes
!
RECURSIVE FUNCTION getNNodesAtDepth_RectHierarchicalMeshType(thismesh, d) RESULT(n)
  CHARACTER(LEN=*),PARAMETER :: myName='getNNodesAtDepth_RectHierarchicalMeshType'
  CLASS(RectHierarchicalMeshType), INTENT(INOUT) :: thismesh
  INTEGER(SIK), INTENT(IN) :: d
  INTEGER(SIK) :: n, i, d_relative
  n = 0
  IF(ASSOCIATED(thismesh%children) .AND. d > 0)THEN
    d_relative = d - 1
    DO i=1,SIZE(thismesh%children)
      n = n + thismesh%children(i)%getNNodesAtDepth(d_relative)
    ENDDO
  ELSE
    IF(d > 0) CALL eRHM%raiseError(modName//'::'//myName// &
      ' - requested depth was greater than actual depth.')
    ! If d = 0, it is the desired depth
    n = 1
  ENDIF
ENDFUNCTION getNNodesAtDepth_RectHierarchicalMeshType
!
!-------------------------------------------------------------------------------
!> @brief Gets the number of leaf nodes in this mesh
!> @param thismesh the RHM object
!> @returns n the number of leaf nodes
!>
RECURSIVE FUNCTION getNLeaves_RectHierarchicalMeshType(thismesh) RESULT(n)
  CLASS(RectHierarchicalMeshType), INTENT(INOUT) :: thismesh
  INTEGER(SIK) :: n, d
  d = thismesh%distanceToLeaf()
  n = thismesh%getNNodesAtDepth(d)
ENDFUNCTION getNLeaves_RectHierarchicalMeshType
!
!-------------------------------------------------------------------------------
!> @brief Gets an array of pointers to the nodes at depth d
!> @param thismesh the RHM object
!> @param nodes a pointer to an array of RHM meshes
!> @param d the depth at which the nodes should be retieved
!> @returns a pointer array
!>
RECURSIVE SUBROUTINE getNodesAtDepth_RectHierarchicalMeshType(thismesh, nodes, d, idx)
  CLASS(RectHierarchicalMeshType), INTENT(INOUT), TARGET :: thismesh
  TYPE(RectHierarchicalMeshTypePtrArry), INTENT(INOUT), POINTER :: nodes(:)
  INTEGER(SIK), INTENT(IN) :: d
  INTEGER(SIK), OPTIONAL, INTENT(INOUT) :: idx
  INTEGER(SIK) :: n, i, iidx

  ! If no idx, assumed to be top level
  IF(.NOT.PRESENT(idx))THEN
    IF(ASSOCIATED(nodes)) DEALLOCATE(nodes)
    NULLIFY(nodes)
    n = thismesh%getNNodesAtDepth(d)
    ALLOCATE(nodes(n))
    iidx = 1
    IF(ASSOCIATED(thismesh%children) .AND. d > 0)THEN
      DO i=1,SIZE(thismesh%children)
        CALL thismesh%children(i)%getNodesAtDepth(nodes, d-1, iidx)
      ENDDO
    ELSE ! leaf
      nodes(iidx)%RHM => thismesh
    ENDIF
  ELSE
    IF(ASSOCIATED(thismesh%children) .AND. d > 0)THEN
      DO i=1,SIZE(thismesh%children)
        CALL thismesh%children(i)%getNodesAtDepth(nodes, d-1, idx)
      ENDDO
    ELSE ! leaf
      nodes(idx)%RHM => thismesh
      idx = idx + 1
    ENDIF
  ENDIF
ENDSUBROUTINE getNodesAtDepth_RectHierarchicalMeshType
!!
!!-------------------------------------------------------------------------------
!!> @brief Gets an array of pointers to the leaf nodes in this mesh
!!> @param thismesh the XDMF mesh object
!!> @param leaves a pointer to an array of XDMF meshes (the leaves)
!!> @returns a pointer array
!!>
!RECURSIVE SUBROUTINE getLeaves_XDMFMeshType(thismesh, leaves)
!  CLASS(XDMFMeshType), INTENT(INOUT), TARGET :: thismesh
!  TYPE(XDMFMeshPtrArry), INTENT(INOUT), POINTER :: leaves(:)
!  INTEGER(SIK) :: d
!
!  d = thismesh%distanceToLeaf()
!  CALL thismesh%getNodesAtDepth(leaves, d)
!ENDSUBROUTINE getLeaves_XDMFMeshType
!!
!!-------------------------------------------------------------------------------
!!> @brief Export the leaf nodes of the mesh hierarchy
!!> @param mesh the mesh
!!> @param xmle the XML element
!!> @param strpath the string holding the path to the XDMF file
!!> @param h5 the HDF5 file
!!>
!RECURSIVE SUBROUTINE export_leaf_XDMF(mesh, xmle, strpath, h5)
!  CHARACTER(LEN=*),PARAMETER :: myName='export_leaf_XDMF'
!  TYPE(XDMFMeshType),INTENT(IN)  :: mesh
!  TYPE(XMLElementType),TARGET,INTENT(INOUT) :: xmle
!  TYPE(StringType),INTENT(IN) :: strpath
!  TYPE(HDF5FileType), INTENT(INOUT) :: h5
!  TYPE(XMLElementType),POINTER :: current_xml, child_xml, children(:), &
!    children2(:)
!  TYPE(StringType) :: str_name, str_value, str1, str2, toponame, xdmf_id_str
!  INTEGER(SIK) :: nchildren, ichild, verts_in_cell
!  INTEGER(SIK) :: xdmf_id, nverts, ncells, i, j, ivert
!  CHARACTER(LEN=200) :: charpath
!  INTEGER(SIK), ALLOCATABLE :: point_list_2d(:, :), point_list_1d(:), cell_list_1d(:)
!  INTEGER(SIK),PARAMETER :: GEOMETRY_IDX=1
!  INTEGER(SIK),PARAMETER :: TOPOLOGY_IDX=2
!
!  REQUIRE(ALLOCATED(mesh%vertices) .AND. ALLOCATED(mesh%cells))
!
!  ! Create HDF5 group
!  CALL h5%mkdir(CHAR(mesh%name))
!
!  ! Determine number of xmle children
!  ! Geometry, Topology, Material, Cell sets
!  ! Assumes mesh has geometry and topology
!  nchildren = 2
!  IF(ALLOCATED(mesh%material_ids)) nchildren = nchildren + 1
!  IF(ALLOCATED(mesh%cell_sets)) nchildren = nchildren + SIZE(mesh%cell_sets)
!  ALLOCATE(children(nchildren))
!  CALL xmle%setChildren(children)
!
!  ! GEOMETRY
!  current_xml => children(GEOMETRY_IDX)
!  str_name="Geometry"
!  CALL current_xml%setName(str_name)
!  CALL current_xml%setParent(xmle)
!
!  str_name= "GeometryType"
!  str_value = "XYZ"
!  CALL current_xml%setAttribute(str_name, str_value)
!
!  children => NULL()
!  ALLOCATE(children(1))
!  CALL current_xml%setChildren(children)
!  child_xml => children(1)
!  str_name="DataItem"
!  CALL child_xml%setName(str_name)
!  CALL child_xml%setParent(current_xml)
!
!  str_name= "DataType"
!  str_value = "Float"
!  CALL child_xml%setAttribute(str_name, str_value)
!
!  nverts=SIZE(mesh%vertices, DIM=2)
!  str_name="Dimensions"
!  str1 = nverts
!  str2 = "3"
!  str_value = str1//" "//str2
!  CALL child_xml%setAttribute(str_name, str_value)
!
!  str_name= "Format"
!  str_value = "HDF"
!  CALL child_xml%setAttribute(str_name, str_value)
!
!  str_name= "Precision"
!  str_value = "8"
!  CALL child_xml%setAttribute(str_name, str_value)
!
!  i = LEN_TRIM(strpath)
!  charpath = CHAR(strpath)
!  child_xml%content = charpath(1:i-4)//"h5:/"//mesh%name//"/vertices"
!
!  CALL h5%fwrite(CHAR(mesh%name)//'->vertices',mesh%vertices)
!
!  children => NULL()
!
!  ! TOPOLOGY
!  CALL xmle%getChildren(children)
!  current_xml => children(TOPOLOGY_IDX)
!  str_name="Topology"
!  CALL current_xml%setName(str_name)
!  CALL current_xml%setParent(xmle)
!
!  ! Single topology
!  IF(mesh%singleTopology)THEN
!    str_name= "TopologyType"
!    xdmf_id = mesh%cells(1)%point_list(1)
!    xdmf_id_str = xdmf_id
!    CALL XDMFTopologyList%get('XDMFID->'//ADJUSTL(xdmf_id_str), toponame)
!    CALL XDMFTopologyList%get(ADJUSTL(toponame)//'->n', verts_in_cell)
!    str_value = toponame
!    CALL current_xml%setAttribute(str_name, str_value)
!
!    str_name= "NumberOfElements"
!    ncells = SIZE(mesh%cells)
!    str_value = ncells
!    CALL current_xml%setAttribute(str_name, str_value)
!
!    str_name= "NodesPerElement"
!    str_value = verts_in_cell
!    CALL current_xml%setAttribute(str_name, str_value)
!
!    children => NULL()
!    ALLOCATE(children(1))
!
!    CALL current_xml%setChildren(children)
!    child_xml => children(1)
!    str_name="DataItem"
!    CALL child_xml%setName(str_name)
!    CALL child_xml%setParent(current_xml)
!
!    str_name= "DataType"
!    str_value = "Int"
!    CALL child_xml%setAttribute(str_name, str_value)
!
!    str_name="Dimensions"
!    str1 = ncells
!    str2 = verts_in_cell
!    str_value = str1//" "//str2
!    CALL child_xml%setAttribute(str_name, str_value)
!
!    str_name= "Format"
!    str_value = "HDF"
!    CALL child_xml%setAttribute(str_name, str_value)
!
!    str_name= "Precision"
!    str_value = "8"
!    CALL child_xml%setAttribute(str_name, str_value)
!
!    i = LEN_TRIM(strpath)
!    charpath = CHAR(strpath)
!    child_xml%content = charpath(1:i-4)//"h5:/"//mesh%name//"/cells"
!
!    ALLOCATE(point_list_2d(verts_in_cell, ncells))
!    DO i = 1, ncells
!      ! Convert 1 based to 0 based index
!      point_list_2d(:,i) = mesh%cells(i)%point_list(2:) - 1
!    ENDDO
!    CALL h5%fwrite(CHAR(mesh%name)//'->cells',point_list_2d)
!    DEALLOCATE(point_list_2d)
!  ! Mixed topology
!  ELSE
!    str_name= "TopologyType"
!    str_value = "Mixed"
!    CALL current_xml%setAttribute(str_name, str_value)
!
!    str_name= "NumberOfElements"
!    ncells = SIZE(mesh%cells)
!    str_value = ncells
!    CALL current_xml%setAttribute(str_name, str_value)
!
!    children => NULL()
!    ALLOCATE(children(1))
!    CALL current_xml%setChildren(children)
!    child_xml => children(1)
!    str_name="DataItem"
!    CALL child_xml%setName(str_name)
!    CALL child_xml%setParent(current_xml)
!
!    str_name= "DataType"
!    str_value = "Int"
!    CALL child_xml%setAttribute(str_name, str_value)
!
!    nverts = 0
!    DO i = 1, ncells
!      nverts = nverts + SIZE(mesh%cells(i)%point_list)
!    ENDDO
!    str_name="Dimensions"
!    str_value = nverts
!    CALL child_xml%setAttribute(str_name, str_value)
!
!    str_name= "Format"
!    str_value = "HDF"
!    CALL child_xml%setAttribute(str_name, str_value)
!
!    str_name= "Precision"
!    str_value = "8"
!    CALL child_xml%setAttribute(str_name, str_value)
!
!    i = LEN_TRIM(strpath)
!    charpath = CHAR(strpath)
!    child_xml%content = charpath(1:i-4)//"h5:/"//mesh%name//"/cells"
!
!    ALLOCATE(point_list_1d(nverts))
!    ivert = 1
!    DO i = 1, ncells
!      nverts = SIZE(mesh%cells(i)%point_list)
!      ! Convert 1 based to 0 based index
!      point_list_1d(ivert) = mesh%cells(i)%point_list(1)
!      point_list_1d(ivert + 1 : ivert + nverts - 1) = mesh%cells(i)%point_list(2:) - 1
!      ivert = ivert + nverts
!    ENDDO
!    CALL h5%fwrite(CHAR(mesh%name)//'->cells',point_list_1d)
!    DEALLOCATE(point_list_1d)
!  ENDIF
!
!  ichild = 3
!
!  ! MATERIAL ID
!  IF(ALLOCATED(mesh%material_ids))THEN
!    children => NULL()
!    CALL xmle%getChildren(children)
!    current_xml => children(ichild)
!    str_name="Attribute"
!    CALL current_xml%setName(str_name)
!    CALL current_xml%setParent(xmle)
!
!    str_name= "Center"
!    str_value = "Cell"
!    CALL current_xml%setAttribute(str_name, str_value)
!
!    str_name= "Name"
!    str_value = "MaterialID"
!    CALL current_xml%setAttribute(str_name, str_value)
!
!    children => NULL()
!    ALLOCATE(children(1))
!    CALL current_xml%setChildren(children)
!    child_xml => children(1)
!
!    str_name="DataItem"
!    CALL child_xml%setName(str_name)
!    CALL child_xml%setParent(current_xml)
!
!    str_name= "DataType"
!    str_value = "Int"
!    CALL child_xml%setAttribute(str_name, str_value)
!
!    str_name="Dimensions"
!    ncells = SIZE(mesh%cells)
!    str_value = ncells
!    CALL child_xml%setAttribute(str_name, str_value)
!
!    str_name= "Format"
!    str_value = "HDF"
!    CALL child_xml%setAttribute(str_name, str_value)
!
!    str_name= "Precision"
!    str_value = "4"
!    CALL child_xml%setAttribute(str_name, str_value)
!
!    i = LEN_TRIM(strpath)
!    charpath = CHAR(strpath)
!    child_xml%content = charpath(1:i-4)//"h5:/"//mesh%name//"/material_id"
!
!    ALLOCATE(point_list_1d(ncells))
!    ! Convert 1 based to 0 based index
!    point_list_1d = mesh%material_ids - 1
!    CALL h5%fwrite(CHAR(mesh%name)//'->material_id',point_list_1d)
!    DEALLOCATE(point_list_1d)
!    ichild = ichild + 1
!  ENDIF
!
!  ! CELL SETS
!  IF(ALLOCATED(mesh%cell_sets))THEN
!    children => NULL()
!    CALL xmle%getChildren(children)
!    DO i=ichild, nchildren
!      current_xml => children(i)
!      str_name="Set"
!      CALL current_xml%setName(str_name)
!      CALL current_xml%setParent(xmle)
!
!      str_name= "Name"
!      str_value = mesh%cell_sets(i - ichild + 1)%name
!      CALL current_xml%setAttribute(str_name, str_value)
!
!      str_name= "SetType"
!      str_value = "Cell"
!      CALL current_xml%setAttribute(str_name, str_value)
!
!      ALLOCATE(children2(1))
!      CALL current_xml%setChildren(children2)
!      child_xml => children2(1)
!      str_name="DataItem"
!      CALL child_xml%setName(str_name)
!      CALL child_xml%setParent(current_xml)
!
!      str_name= "DataType"
!      str_value = "Int"
!      CALL child_xml%setAttribute(str_name, str_value)
!
!      str_name="Dimensions"
!      ncells = SIZE(mesh%cell_sets(i-ichild+1)%cell_list)
!      str_value = ncells
!      CALL child_xml%setAttribute(str_name, str_value)
!
!      str_name= "Format"
!      str_value = "HDF"
!      CALL child_xml%setAttribute(str_name, str_value)
!
!      str_name= "Precision"
!      str_value = "8"
!      CALL child_xml%setAttribute(str_name, str_value)
!
!      j = LEN_TRIM(strpath)
!      charpath = CHAR(strpath)
!      child_xml%content = charpath(1:j-4)//"h5:/"//mesh%name//"/"//mesh%cell_sets(i-ichild+1)%name
!
!      ALLOCATE(cell_list_1d(ncells))
!      ! Convert 1 based to 0 based index
!      cell_list_1d = mesh%cell_sets(i-ichild+1)%cell_list - 1
!      CALL h5%fwrite(CHAR(mesh%name)//'->'//CHAR(mesh%cell_sets(i-ichild+1)%name),cell_list_1d)
!      DEALLOCATE(cell_list_1d)
!    ENDDO
!  ENDIF
!ENDSUBROUTINE export_leaf_XDMF
!!
!!-------------------------------------------------------------------------------
!!> @brief Create the xml hierarchy for the mesh
!!> @param mesh the mesh
!!> @param xmle the XML element
!!> @param strpath the string holding the path to the XDMF file
!!> @param h5 the HDF5 file
!!>
!RECURSIVE SUBROUTINE create_xml_hierarchy_XDMF(mesh, xmle, strpath, h5)
!  CHARACTER(LEN=*),PARAMETER :: myName='create_xml_hierarchy_XDMF'
!  TYPE(XDMFMeshType),INTENT(INOUT)  :: mesh
!  TYPE(XMLElementType),TARGET,INTENT(INOUT) :: xmle
!  TYPE(StringType),INTENT(INOUT) :: strpath
!  TYPE(HDF5FileType), INTENT(INOUT) :: h5
!  TYPE(XMLElementType), POINTER :: children(:)
!  INTEGER(SIK) :: i
!  TYPE(StringType) :: str_name, str_value
!
!  ! If this mesh has children
!  IF(ASSOCIATED(mesh%children)) THEN
!    ! Add XML element children
!    ALLOCATE(children(SIZE(mesh%children)))
!    CALL xmle%setChildren(children)
!    DO i=1,SIZE(mesh%children)
!      ! Set attributes then recurse
!      str_name="Grid"
!      CALL children(i)%setName(str_name)
!      CALL children(i)%setParent(xmle)
!      str_name='Name'
!      str_value = mesh%children(i)%name
!      CALL children(i)%setAttribute(str_name, str_value)
!      str_name='GridType'
!      IF(ASSOCIATED(mesh%children(i)%children))THEN
!        str_value = 'Tree'
!      ELSE
!        str_value = 'Uniform'
!      ENDIF
!      CALL children(i)%setAttribute(str_name, str_value)
!
!      CALL create_xml_hierarchy_XDMF(mesh%children(i), children(i), strpath, h5)
!    ENDDO
!  ELSE
!    CALL export_leaf_XDMF(mesh, xmle, strpath, h5)
!  ENDIF
!ENDSUBROUTINE create_xml_hierarchy_XDMF
!!
!!-------------------------------------------------------------------------------
!!> @brief Exports mesh data to an XDMF file.
!!> @param strpath the string holding the path to the XDMF file
!!> @param mesh the XDMF mesh object
!!>
!SUBROUTINE exportXDMFMesh(strpath, mesh)
!  CHARACTER(LEN=*),PARAMETER :: myName='exportXDMFMesh'
!  TYPE(StringType),INTENT(INOUT) :: strpath
!  TYPE(XDMFMeshType),INTENT(INOUT)  :: mesh
!  TYPE(XMLFileType) :: xml
!  TYPE(HDF5FileType) :: h5
!  TYPE(XMLElementType),POINTER :: xmle, children(:), children2(:)
!  TYPE(StringType) :: str_name, str_value
!  INTEGER(SIK) :: i
!  CHARACTER(LEN=200) :: charpath
!
!  ! Create HDF5 file
!  i = LEN_TRIM(strpath)
!  charpath = CHAR(strpath)
!  CALL h5%init(charpath(1:i-4)//"h5",'NEW')
!  CALL h5%fopen()
!
!  ! Create XML file
!  CALL xml%init(ADJUSTL(strpath),.FALSE.)
!  xmle => xml%root
!  IF(.NOT.ASSOCIATED(xmle)) CALL eRHM%raiseError(modName//'::'//myName// &
!    ' - XML data init encountered an error. Pointer to root not associated.')
!  !   Set Xdmf
!  str_name='Xdmf'
!  CALL xmle%setName(str_name)
!  str_name='Version'
!  str_value = '3.0'
!  CALL xmle%setAttribute(str_name, str_value)
!  !   Set Domain
!  ALLOCATE(children(1))
!  CALL xmle%setChildren(children)
!  str_name='Domain'
!  CALL children(1)%setName(str_name)
!  CALL children(1)%setParent(xml%root)
!  ! Setup the grid that contains everything
!  xmle => children(1)
!
!  ALLOCATE(children2(1))
!  CALL xmle%setChildren(children2)
!  str_name="Grid"
!  CALL children2(1)%setName(str_name)
!  CALL children2(1)%setParent(xmle)
!  str_name='Name'
!  str_value = mesh%name
!  CALL children2(1)%setAttribute(str_name, str_value)
!  str_name='GridType'
!  IF(.NOT.ASSOCIATED(mesh%children))THEN
!    str_value = 'Uniform'
!  ELSE
!    str_value = 'Tree'
!  ENDIF
!  CALL children2(1)%setAttribute(str_name, str_value)
!
!  ! Recursively add xml elements for each grid. Only the leaves have vertices,
!  ! so only the leaves have HDF5 groups/data.
!  xmle => children2(1)
!  CALL create_xml_hierarchy_XDMF(mesh, xmle, strpath, h5)
!
!  ! Finish up
!  CALL xml%exportToDisk(CHAR(strpath))
!  CALL h5%fclose()
!
!ENDSUBROUTINE exportXDMFMesh
!!
!!-------------------------------------------------------------------------------
!!> @brief Returns the area of cell iCell.
!!> @param mesh the XMDF mesh
!!> @param iCell the index of the cell in mesh%cells
!!> @returns cell area
!!>
!ELEMENTAL FUNCTION getCellArea_XDMFMeshType(mesh, iCell) RESULT(area)
!  CLASS(XDMFMeshType), INTENT(IN) :: mesh
!  INTEGER(SIK), INTENT(IN) :: iCell
!  REAL(SRK) :: area
!
!  REAL(SDK),PARAMETER :: pi = 3.14159265358979311599796346854
!  REAL(SRK), ALLOCATABLE :: x(:), y(:), x_quad(:), y_quad(:), x_lin(:), y_lin(:)
!  REAL(SRK) :: main_area, correction, x_edge(3), y_edge(3), theta, rotation_mat(2,2), &
!    xy(2), a, b, quad_area
!  INTEGER(SIK) :: xid
!  INTEGER(SIK) nverts, i, j
!
!  area = 0.0_SRK
!  xid = mesh%cells(iCell)%point_list(1)
!  nverts = SIZE(mesh%cells(iCell)%point_list) - 1
!  IF(xid == 4 .OR. xid == 5) THEN ! Linear edges
!    ! Shoelace formula may be used for linear edges
!    ! Assumes that vertices are in clockwise or counterclockwise order
!    ALLOCATE(x(nverts))
!    ALLOCATE(y(nverts))
!    ALLOCATE(x_lin(nverts))
!    ALLOCATE(y_lin(nverts))
!    x = mesh%vertices(1, mesh%cells(iCell)%point_list(2:nverts+1))
!    y = mesh%vertices(2, mesh%cells(iCell)%point_list(2:nverts+1))
!    x_lin = x - SUM(x)/nverts
!    y_lin = y - SUM(y)/nverts
!    ! Narrowing may occur here. This is intended.
!    correction = x_lin(nverts)*y_lin(1) - y_lin(nverts)*x_lin(1)
!    main_area = DOT_PRODUCT(x_lin(1:nverts-1), y_lin(2:nverts)) - DOT_PRODUCT(y_lin(1:nverts-1), x_lin(2:nverts))
!    area = 0.5*ABS(main_area + correction)
!  ELSEIF(xid == 36 .OR. xid == 37) THEN ! There are quadratic edges
!    ! Assumed vertices are in counterclockwise order
!    !
!    ! Overall, the process is to get the linear area and adjust for quadratic edges.
!    !
!    ! All quadratic vertices (middle of 3 vert edge) are in the second half of
!    ! the point_list
!    nverts = nverts/2
!    ALLOCATE(x(nverts))
!    ALLOCATE(y(nverts))
!    ALLOCATE(x_quad(nverts))
!    ALLOCATE(y_quad(nverts))
!    ALLOCATE(x_lin(nverts))
!    ALLOCATE(y_lin(nverts))
!    x = mesh%vertices(1, mesh%cells(iCell)%point_list(2:nverts+1))
!    y = mesh%vertices(2, mesh%cells(iCell)%point_list(2:nverts+1))
!    x_quad = mesh%vertices(1, mesh%cells(iCell)%point_list(nverts+2:))
!    y_quad = mesh%vertices(2, mesh%cells(iCell)%point_list(nverts+2:))
!    ! Get linear area using shoelace formula
!    x_lin = x - SUM(x)/nverts
!    y_lin = y - SUM(y)/nverts
!    ! Narrowing may occur here. This is intended.
!    correction = x_lin(nverts)*y_lin(1) - y_lin(nverts)*x_lin(1)
!    main_area = DOT_PRODUCT(x_lin(1:nverts-1), y_lin(2:nverts)) - DOT_PRODUCT(y_lin(1:nverts-1), x_lin(2:nverts))
!    area = 0.5*ABS(main_area + correction)
!
!    ! Assumed points are in counterclockwise order. Area for the linear
!    ! polygon is computed, then adjusted based on integrals for the quad edges.
!    ! If a quadratic vertex is to the left of the linear edge, the area is added.
!    ! Otherwise it is subtracted.
!    ! Consider the following quadratic triangle with one quad edge:
!    !        2                   2
!    !       /  \                /| \
!    !      /    \              / |  \
!    !     5      4            5  |   \
!    !      \      \            \ |    \
!    !       \      \            \|     \
!    !        0---3--1            0------1
!    !     Quad edge (2,5,0)     Linear edges
!    ! Since, point 5 is to the right of linear edge (2,0), the area of the polygon
!    ! constructed by edges {(2,5,0), (0,2)} is added to the total area.
!    !
!    ! For each edge, compute additional area using quadratic function
!    ! Shift point to origin, rotate so line is x-axis, find quadratic function,
!    ! integrate, add or subtract based on right or left
!
!    !For each edge
!    DO i = 1, nverts
!      ! edge coords
!      IF(i == nverts)THEN
!        x_edge  = (/x(i), x(1), x_quad(i)/)
!        y_edge  = (/y(i), y(1), y_quad(i)/)
!      ELSE
!        x_edge  = (/x(i), x(i+1), x_quad(i)/)
!        y_edge  = (/y(i), y(i+1), y_quad(i)/)
!      ENDIF
!      ! shift first vertex to origin
!      x_edge = x_edge - x_edge(1)
!      y_edge = y_edge - y_edge(1)
!      ! rotate linear edge to become the x-axis
!      IF( x_edge(2) .APPROXEQ. 0.0_SRK ) THEN
!        IF( y_edge(2) >= 0.0_SRK ) THEN
!          theta = pi/2.0_SDK
!        ELSE
!          theta = -pi/2.0_SDK
!        ENDIF
!      ELSE
!        theta = ATAN(y_edge(2)/x_edge(2))
!      ENDIF
!
!      IF(x_edge(2) < 0.0) theta = theta + pi
!
!      rotation_mat(1,:) = (/COS(theta), SIN(theta)/)
!      rotation_mat(2,:) = (/-SIN(theta), COS(theta)/)
!      DO j = 1,3
!        xy(1) = x_edge(j)
!        xy(2) = y_edge(j)
!        xy = MATMUL(rotation_mat, xy)
!        x_edge(j) = xy(1)
!        y_edge(j) = xy(2)
!      ENDDO
!
!      ! Get quadratic coefficients
!      !   y = ax^2 + bx + c
!      ! Since x_edge(1) = 0, y_edge(1) = 0 due to the shift to the origin,
!      !   0 = 0 + 0 + c --> c = 0
!      ! Due to the rotation to make the linear edge the x-axis, y_edge(2) = 0
!      !   0 = ax_2^2 + bx_2 --> ax_2 + b = 0 --> b = -ax_2
!      ! Lastly,
!      !   y_3 = ax_3^2 + bx_3
!      ! Using, b = -ax_2
!      !   y_3 = ax_3(x_3  - x_2) --> a = y_3/x_3 1/(x_3 - x_2)
!      ! Note if x_3 = 0 --> y_3 = 0 --> (x_1, y_1) = (x_3, y_3), which is invalid
!      a = (y_edge(3)/x_edge(3))/(x_edge(3) - x_edge(2))
!      b = -a*x_edge(2)
!      ! Integrate from 0 to x_2
!      !  ax_2^3/3 + bx_2^2/2
!      quad_area = a*x_edge(2)**3/3.0_SRK + b*x_edge(2)**2/2.0_SRK
!      ! quad_area will be opposite of correct sign
!      area = area - quad_area
!    ENDDO
!  ELSE ! invalid type. return number that is so wrong, you better realize.
!    area = -HUGE(1.0_SRK)
!  ENDIF
!ENDFUNCTION
!!
!!-------------------------------------------------------------------------------
!!> @brief This routine determines whether a point lies within a 2D mesh cell
!!> @param thisCell The cell used in the query
!!> @param point The point type to check if it lies inside the cell
!!> @param bool The logical result of this operation.  TRUE if the point is inside.
!!>
!FUNCTION pointInsideCell_XDMFMeshType(thismesh,iCell,point) RESULT(bool)
!  CLASS(XDMFMeshType),INTENT(IN) :: thismesh
!  INTEGER(SIK),INTENT(IN) :: iCell
!  TYPE(PointType),INTENT(IN) :: point
!  LOGICAL(SBK) :: bool
!
!  INTEGER(SIK) :: i,j, lastvert_idx
!  INTEGER(SIK) :: iEdge, iVert, p1ID, p2ID, iLastVert, ifirstVert
!  LOGICAL(SBK) :: isLeft
!
!  ! If the point isLeft of each edge, it must be interior, since the
!  ! vertices are in counter-clockwise order.
!  ! Orientation of the edges matters, so if the vertices of the edge are opposite
!  ! of the way they are in the cell, flip the boolean.
!  bool = .TRUE.
!  REQUIRE(ALLOCATED(thismesh%edges))
!!  IF(.NOT.ALLOCATED(thismesh%edges)) CALL thismesh%setupEdges()
!  DO i = 1, SIZE(thismesh%cells(iCell)%edge_list)
!    iEdge = thismesh%cells(iCell)%edge_list(i)
!    IF(thismesh%edges(iEdge)%isLinear)THEN
!      isLeft = thismesh%edges(iEdge)%line%pointIsLeft(point)
!      p1ID = thismesh%edges(iEdge)%vertices(2)
!      p2ID = thismesh%edges(iEdge)%vertices(3)
!    ELSE
!      IF(ABS(thismesh%edges(iEdge)%quad%a) < 1.0E-3) THEN
!        isLeft = thismesh%edges(iEdge)%line%pointIsLeft(point)
!      ELSE
!        isLeft = thismesh%edges(iEdge)%quad%pointIsLeft(point)  
!      ENDIF
!      p1ID = thismesh%edges(iEdge)%vertices(1)
!      p2ID = thismesh%edges(iEdge)%vertices(2)
!    ENDIF
!    ! Loop through the vertices. If point 1 in encountered first, isLeft is
!    ! correct. If point 2 is encountered 1st, flip isLeft. Vertices are in
!    ! counter clockwise order, hence the orientation is known.
!    ! The exception is on the last edge, where the verts wrap around an p2
!    ! should be encountered first.
!    IF(thismesh%edges(iEdge)%isLinear)THEN
!      lastvert_idx = SIZE(thismesh%cells(iCell)%point_list)
!      iLastVert = thismesh%cells(iCell)%point_list(lastvert_idx)
!    ELSE
!      ! total list - 1 for the xid, /2 to only address linear elements + 1 to
!      ! skip xid
!      lastvert_idx = (SIZE(thismesh%cells(iCell)%point_list) - 1)/2 + 1
!      iLastVert = thismesh%cells(iCell)%point_list(lastvert_idx)
!    ENDIF
!    ifirstVert = thismesh%cells(iCell)%point_list(2)
!    ! Test for wrap around 1st.
!    IF(p1ID == ifirstVert .AND. p2ID == iLastVert) THEN
!      isLeft = .NOT.isLeft
!    ELSEIF(p1ID == iLastVert .AND. p2ID == ifirstVert) THEN
!      ! Correct. nothing to do
!    ELSE
!      DO j = 2, lastvert_idx ! skip xid
!        iVert = thismesh%cells(iCell)%point_list(j)
!        IF(iVert == p1ID) THEN
!          EXIT
!        ENDIF
!        IF(iVert == p2ID) THEN
!          isLeft = .NOT.isLeft
!          EXIT
!        ENDIF
!      ENDDO
!    ENDIF
!    ! If the point isLeft, keep going until all edges have been verified,
!    ! otherwise, stop. The point cannot be in this cell if it is right of any
!    ! edge.
!    IF(.NOT.isLeft) THEN
!      bool = .FALSE.
!      RETURN
!    ENDIF
!  ENDDO
!ENDFUNCTION pointInsideCell_XDMFMeshType
#endif
ENDMODULE RectHierarchicalMesh
