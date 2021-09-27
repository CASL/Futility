!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module defines objects for representing an XDMF mesh
!>
!> This module reads an XDMF file and stores the information in a
!> hierarchical mesh type. It can also write the hierarchical mesh to XDMF.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE XDMFMesh
#include "Futility_DBC.h"
USE ExceptionHandler
USE Futility_DBC
USE ISO_FORTRAN_ENV
USE IntrType
USE Strings
USE FileType_XML
USE FileType_HDF5
USE ParameterLists
USE Geom
USE Sorting

IMPLICIT NONE
PRIVATE

#ifdef FUTILITY_HAVE_HDF5
! Public members
PUBLIC :: XDMFMeshType_2D
PUBLIC :: XDMFCellSet
!PUBLIC :: XDMFMeshPtrArry
PUBLIC :: XDMFTopologyList
PUBLIC :: init_XDMFTopologyList
!PUBLIC :: ExportXDMFMesh
PUBLIC :: ASSIGNMENT(=)

!> The module name
CHARACTER(LEN=*),PARAMETER :: modName='XDMFMesh'

!> Exception handler for the module
TYPE(ExceptionHandlerType),SAVE :: eXDMF

!> Parameter list that holds XDMF topology names, ids, etc.
TYPE(ParamType),SAVE :: XDMFTopologyList

!!> Type to hold an edge
!TYPE :: XDMFEdge
!  !> Is the edge linear or quadratic
!  LOGICAL(SBK) :: isLinear=.TRUE.
!  !> The cells which share the edge
!  INTEGER(SIK) :: cells(2) = -1
!  !> The vertices which make up the edge
!  INTEGER(SIK) :: vertices(3) = -1
!  !> The quadratic edge
!  !> Note, if the coefficient a > 0, the edge is convex.
!  !> a < 0 is convex. a = 0, is a straight line
!  TYPE(QuadraticType) :: quad
!  !> The linear edge
!  TYPE(LineType) :: line
!ENDTYPE XDMFEdge
!
!> Type to hold the points that make up a mesh cell
TYPE :: XDMFCell
  !> The cell type id followed by the point ids
  !> XDMF ID, p1, p2, ..., p_n
  INTEGER(SIK), ALLOCATABLE :: point_list(:)
  !> Edges
!  INTEGER(SIK), ALLOCATABLE :: edge_list(:)
ENDTYPE XDMFCell

!> Type to hold a list of cell IDs that make up a named set.
TYPE :: XDMFCellSet
  !> The name of the set
  TYPE(StringType) :: name
  !> The cell IDs
  INTEGER(SIK), ALLOCATABLE :: cell_list(:)
ENDTYPE XDMFCellSet

!> Type to hold the XDMF mesh data
TYPE :: XDMFMeshType_2D
  !> The name of the mesh
  TYPE(StringType) :: name
  !> The points that compose the mesh
  TYPE(PointType), ALLOCATABLE :: points(:)
  !> The mesh cells
  TYPE(XDMFCell), ALLOCATABLE :: cells(:)
  !> Material for each mesh cell
  INTEGER(SIK), ALLOCATABLE :: material_ids(:)
  !> Named sets within the mesh
  TYPE(XDMFCellSet), ALLOCATABLE :: cell_sets(:)
  CONTAINS
    !> @copybrief XDMFMeshType::clear_XDMFMeshType_2D
    !> @copydoc XDMFMeshType::clear_XDMFMeshType_2D
    PROCEDURE,PASS :: clear => clear_XDMFMeshType_2D
    !> @copybrief XDMFMeshType::getPoints_XDMFMeshType_2D
    !> @copydoc XDMFMeshType::getPoints_XDMFMeshType_2D
    PROCEDURE,PASS :: getPoints => getPoints_XDMFMeshType_2D
    !> @copybrief XDMFMeshType::getCellArea_XDMFMeshType_2D
    !> @copydoc XDMFMeshType::getCellArea_XDMFMeshType_2D
    PROCEDURE,PASS :: getCellArea => getCellArea_XDMFMeshType_2D
    !> @copybrief XDMFMeshType::pointInsideCell_XDMFMeshType
    !> @copydoc XDMFMeshType::pointInsideCell_XDMFMeshType
    PROCEDURE,PASS :: pointInsideCell => pointInsideCell_XDMFMeshType_2D
ENDTYPE XDMFMeshType_2D
!
!> @brief Interface for assignment operator (=)
INTERFACE ASSIGNMENT(=)
  !> @copybrief XDMFMeshType::assign_XDMFMeshType_2D
  !> @copydoc XDMFMeshType::assign_XDMFMeshType_2D
  MODULE PROCEDURE assign_XDMFMeshType_2D
ENDINTERFACE
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the XDMFTopologyList
!>
SUBROUTINE init_XDMFTopologyList()
  ! Setup param list for cell type conversions
  ! id is XDMF topology id,
  ! n is number of vertices,
  ! multiple valid names exist for the same topology, ex: Tri_6 == Triangle_6
  CALL XDMFTopologyList%add('Topology->Triangle->id'            , 4_SIK)
  CALL XDMFTopologyList%add('Topology->Triangle->n'             , 3_SIK)
  CALL XDMFTopologyList%add('Topology->Triangle_6->id'          ,36_SIK)
  CALL XDMFTopologyList%add('Topology->Triangle_6->n'           , 6_SIK)
  CALL XDMFTopologyList%add('Topology->Tri_6->id'               ,36_SIK)
  CALL XDMFTopologyList%add('Topology->Tri_6->n'                , 6_SIK)
  CALL XDMFTopologyList%add('Topology->Quadrilateral->id'       , 5_SIK)
  CALL XDMFTopologyList%add('Topology->Quadrilateral->n'        , 4_SIK)
  CALL XDMFTopologyList%add('Topology->Quadrilateral_8->id'     ,37_SIK)
  CALL XDMFTopologyList%add('Topology->Quadrilateral_8->n'      , 8_SIK)
  CALL XDMFTopologyList%add('Topology->Quad_8->id'              ,37_SIK)
  CALL XDMFTopologyList%add('Topology->Quad_8->n'               , 8_SIK)
  CALL XDMFTopologyList%add('XDMFID->4' ,'Triangle'       )
  CALL XDMFTopologyList%add('XDMFID->36','Triangle_6'     )
  CALL XDMFTopologyList%add('XDMFID->5' ,'Quadrilateral'  )
  CALL XDMFTopologyList%add('XDMFID->37','Quadrilateral_8')
ENDSUBROUTINE init_XDMFTopologyList
!
!-------------------------------------------------------------------------------
!> @brief Clears the XDMF mesh
!> @param thismesh the XDMF mesh object
!>
SUBROUTINE clear_XDMFMeshType_2D(thismesh)
  CLASS(XDMFMeshType_2D), INTENT(INOUT) :: thismesh
  INTEGER(SIK) :: i

  CALL thismesh%name%clear()
  IF( ALLOCATED(thismesh%points) ) THEN
    DO i=1, SIZE(thismesh%points)
      CALL thismesh%points(i)%clear()
    ENDDO
    DEALLOCATE(thismesh%points)
  ENDIF
  IF( ALLOCATED(thismesh%cells) ) THEN
    DO i=1, SIZE(thismesh%cells)
      DEALLOCATE(thismesh%cells(i)%point_list)
    ENDDO
    DEALLOCATE(thismesh%cells)
  ENDIF
  IF( ALLOCATED(thismesh%material_ids) ) DEALLOCATE(thismesh%material_ids)
  IF( ALLOCATED(thismesh%cell_sets) ) THEN
    DO i=1, SIZE(thismesh%cell_sets)
      CALL thismesh%cell_sets(i)%name%clear()
      DEALLOCATE(thismesh%cell_sets(i)%cell_list)
    ENDDO
    DEALLOCATE(thismesh%cell_sets)
  ENDIF
ENDSUBROUTINE clear_XDMFMeshType_2D
!
!-------------------------------------------------------------------------------
!> @brief Assigns an XDMF mesh type to another
!> @param thismesh the XDMF mesh object being assigned to
!> @param thatmesh the XDMF mesh object being assigned from
!>
SUBROUTINE assign_XDMFMeshType_2D(thismesh, thatmesh)
  TYPE(XDMFMeshType_2D), INTENT(INOUT) :: thismesh
  TYPE(XDMFMeshType_2D), INTENT(IN) :: thatmesh
  INTEGER(SIK) :: i

  CALL thismesh%clear()
  thismesh%name = thatmesh%name
  IF( ALLOCATED(thatmesh%points) ) THEN
    ALLOCATE(thismesh%points(SIZE(thatmesh%points)))
    DO i = 1, SIZE(thatmesh%points)
      thismesh%points(i) = thatmesh%points(i)
    ENDDO
  ENDIF
  IF( ALLOCATED(thatmesh%cells) ) THEN
    ALLOCATE(thismesh%cells(SIZE(thatmesh%cells)))
    DO i = 1, SIZE(thatmesh%cells)
      ALLOCATE(thismesh%cells(i)%point_list(SIZE(thatmesh%cells(i)%point_list)))
      thismesh%cells(i)%point_list = thatmesh%cells(i)%point_list
    ENDDO
  ENDIF
  IF( ALLOCATED(thatmesh%material_ids) )THEN
    ALLOCATE(thismesh%material_ids(SIZE(thatmesh%material_ids)))
    thismesh%material_ids = thatmesh%material_ids
  ENDIF
  IF( ALLOCATED(thatmesh%cell_sets) ) THEN
    ALLOCATE(thismesh%cell_sets(SIZE(thatmesh%cell_sets)))
    DO i = 1, SIZE(thatmesh%cell_sets)
      ALLOCATE(thismesh%cell_sets(i)%cell_list(SIZE(thatmesh%cell_sets(i)%cell_list)))
      thismesh%cell_sets(i)%cell_list = thatmesh%cell_sets(i)%cell_list
      thismesh%cell_sets(i)%name = thatmesh%cell_sets(i)%name
    ENDDO
  ENDIF
ENDSUBROUTINE assign_XDMFMeshType_2D
!
!-------------------------------------------------------------------------------
!> @brief Returns the points associated with an array of IDs.
!> @param mesh the XMDF mesh
!> @param ids the index of the point in mesh%points
!> @returns cell area
!>
PURE FUNCTION getPoints_XDMFMeshType_2D(mesh, ids) RESULT(points)
  CLASS(XDMFMeshType_2D), INTENT(IN) :: mesh
  INTEGER(SIK), INTENT(IN) :: ids(:)
  TYPE(PointType), ALLOCATABLE :: points(:)
  INTEGER(SIK) :: i
  
  ALLOCATE(points(SIZE(ids)))
  FORALL (i=1:SIZE(ids)) points(i) = mesh%points(ids(i))
ENDFUNCTION
!
!-------------------------------------------------------------------------------
!> @brief Returns the area of cell id.
!> @param mesh the XMDF mesh
!> @param id the index of the cell in mesh%cells
!> @returns cell area
!>
ELEMENTAL FUNCTION getCellArea_XDMFMeshType_2D(mesh, id) RESULT(a)
  CLASS(XDMFMeshType_2D), INTENT(IN) :: mesh
  INTEGER(SIK), INTENT(IN) :: id
  REAL(SRK) :: a
  INTEGER(SIK) :: xdmfid, npoints
  TYPE(PointType), ALLOCATABLE :: points(:)
  TYPE(Triangle_2D) :: tri
  TYPE(Triangle6_2D) :: tri6
  TYPE(Quadrilateral_2D) :: quad
  TYPE(Quadrilateral8_2D) :: quad8

  a = 0.0_SRK
  xdmfid = mesh%cells(id)%point_list(1)
  npoints = SIZE(mesh%cells(id)%point_list) - 1
  ALLOCATE(points(npoints))
  points = mesh%getPoints(mesh%cells(id)%point_list(2:npoints+1))
  SELECT CASE (xdmfid)
  CASE(4)! Triangle 
    CALL tri%set(points)
    a = area(tri)
    CALL tri%clear()
  CASE(5)! Quadrilateral 
    CALL quad%set(points)
    a = area(quad)
    CALL quad%clear()
  CASE(36)! Triangle6 
    CALL tri6%set(points)
    a = area(tri6)
    CALL tri6%clear()
  CASE(37)! Quadrilateral8
    CALL quad8%set(points)
    a = area(quad8)
    CALL quad8%clear()
  CASE DEFAULT ! invalid type. return number that is so wrong, you better realize.
    a = -HUGE(1.0_SRK)
  ENDSELECT
ENDFUNCTION
!
!-------------------------------------------------------------------------------
!> @brief This routine determines whether a point lies within a 2D mesh cell
!> @param id The cell id used in the query
!> @param point The point type to check if it lies inside the cell
!> @param bool The logical result of this operation.  TRUE if the point is inside.
!>
ELEMENTAL FUNCTION pointInsideCell_XDMFMeshType_2D(mesh,id,point) RESULT(bool)
  CLASS(XDMFMeshType_2D),INTENT(IN) :: mesh
  INTEGER(SIK),INTENT(IN) :: id
  TYPE(PointType),INTENT(IN) :: point
  LOGICAL(SBK) :: bool
  INTEGER(SIK) :: xdmfid, npoints
  TYPE(PointType), ALLOCATABLE :: points(:)
  TYPE(Triangle_2D) :: tri
  TYPE(Triangle6_2D) :: tri6
  TYPE(Quadrilateral_2D) :: quad
  TYPE(Quadrilateral8_2D) :: quad8

  xdmfid = mesh%cells(id)%point_list(1)
  npoints = SIZE(mesh%cells(id)%point_list) - 1
  ALLOCATE(points(npoints))
  points = mesh%getPoints(mesh%cells(id)%point_list(2:npoints+1))
  SELECT CASE (xdmfid)
  CASE(4)! Triangle 
    CALL tri%set(points)
    bool = pointInside(tri, point)
    CALL tri%clear()
  CASE(5)! Quadrilateral 
    CALL quad%set(points)
    bool = pointInside(quad, point)
    CALL quad%clear()
  CASE(36)! Triangle6 
    CALL tri6%set(points)
    bool = pointInside(tri6, point)
    CALL tri6%clear()
  CASE(37)! Quadrilateral8
    CALL quad8%set(points)
    bool = pointInside(quad8, point)
    CALL quad8%clear()
  CASE DEFAULT ! invalid type. return number that is so wrong, you better realize.
    bool = .FALSE.
  ENDSELECT
ENDFUNCTION pointInsideCell_XDMFMeshType_2D
#endif
ENDMODULE XDMFMesh
