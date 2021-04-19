!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief The global geometry module, collecting all public members of
!> other geometry modules. This is the module that should be used elsewhere
!> in the code.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom
USE IntrType
USE Constants_Conversion
USE Geom_Graph
USE Geom_Points
USE Geom_Line
USE Geom_Quadratic
USE Geom_Plane
USE Geom_CircCyl
USE Geom_Box
USE Geom_Poly
USE ParameterLists

IMPLICIT NONE
PRIVATE

PUBLIC :: newGeom

PUBLIC :: GraphType
PUBLIC :: MAX_COORD_STR_LEN
PUBLIC :: PointType
PUBLIC :: LinkedListPointType
PUBLIC :: LineType
PUBLIC :: QuadraticType
PUBLIC :: PlaneType
PUBLIC :: CircleType
PUBLIC :: CylinderType
PUBLIC :: OBBoxType
PUBLIC :: ABBoxType
PUBLIC :: PolygonType
PUBLIC :: Distance
PUBLIC :: midPoint
PUBLIC :: ClearLinkedListPointType
PUBLIC :: Polygonize
PUBLIC :: OPERATOR(+)
PUBLIC :: OPERATOR(-)
PUBLIC :: OPERATOR(==)
PUBLIC :: OPERATOR(/=)
PUBLIC :: OPERATOR(.APPROXEQA.)
PUBLIC :: ASSIGNMENT(=)

INTERFACE newGeom
  !> @copybrief Geom::newGeom_line
  !> @copydetails Geom::newGeom_line
  MODULE PROCEDURE newGeom_line
  !> @copybrief Geom::newGeom_plane
  !> @copydetails Geom::newGeom_plane
  MODULE PROCEDURE newGeom_plane
  !> @copybrief Geom::newGeom_box
  !> @copydetails Geom::newGeom_box
  MODULE PROCEDURE newGeom_box
  !> @copybrief Geom::newGeom_circ
  !> @copydetails Geom::newGeom_circ
  MODULE PROCEDURE newGeom_circ
  !> @copybrief Geom::newGeom_cyl
  !> @copydetails Geom::newGeom_cyl
  MODULE PROCEDURE newGeom_cyl
  !> @copybrief Geom::newGeom_poly
  !> @copydetails Geom::newGeom_poly
  MODULE PROCEDURE newGeom_poly
ENDINTERFACE
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief
!> @param params
!> @param geom
!>
SUBROUTINE newGeom_line(params,geom)
  TYPE(ParamType),INTENT(IN) :: params
  TYPE(LineType),INTENT(INOUT) :: geom

  REAL(SRK),ALLOCATABLE :: c0(:),c1(:)
  TYPE(PointType) :: p0,p1

  CALL params%get('LineGeom -> StartPoint',c0)
  CALL params%get('LineGeom -> EndPoint',c1)
  IF(ALLOCATED(c0)) CALL p0%init(COORD=c0)
  IF(ALLOCATED(c1)) CALL p1%init(COORD=c1)
  CALL geom%set(p0,p1)
ENDSUBROUTINE newGeom_line
!
!-------------------------------------------------------------------------------
!> @brief
!> @param params
!> @param geom
!>
SUBROUTINE newGeom_plane(params,geom)
  TYPE(ParamType),INTENT(IN) :: params
  TYPE(PlaneType),INTENT(INOUT) :: geom

  REAL(SRK),ALLOCATABLE :: c0(:),v0(:)
  TYPE(PointType) :: p0

  CALL params%get('PlaneGeom -> Point',c0)
  CALL params%get('PlaneGeom -> NormalVector',v0)
  IF(ALLOCATED(c0)) CALL p0%init(COORD=c0)
  IF(ALLOCATED(v0)) CALL geom%set(v0,p0)
ENDSUBROUTINE newGeom_plane
!
!-------------------------------------------------------------------------------
!> @brief
!> @param params
!> @param geom
!>
SUBROUTINE newGeom_box(params,geom)
  TYPE(ParamType),INTENT(IN) :: params
  TYPE(OBBoxType),INTENT(INOUT) :: geom

  REAL(SRK),ALLOCATABLE :: c0(:),v1(:),v2(:),v3(:),extent(:)
  TYPE(PointType) :: p0

  CALL params%get('BoxGeom -> CornerPoint',c0)
  CALL params%get('BoxGeom -> Vector1',v1)
  CALL params%get('BoxGeom -> Vector2',v2)
  CALL params%get('BoxGeom -> ExtentVector',extent)
  IF(params%has('BoxGeom -> Vector3')) &
      CALL params%get('BoxGeom -> Vector3',v3)
  IF(ALLOCATED(c0)) CALL p0%init(COORD=c0)
  IF(ALLOCATED(v3)) THEN
    CALL geom%set(p0,extent,v1,v2,v3)
  ELSEIF(ALLOCATED(v1) .AND. ALLOCATED(v2)) THEN
    CALL geom%set(p0,extent,v1,v2)
  ENDIF
ENDSUBROUTINE newGeom_box
!
!-------------------------------------------------------------------------------
!> @brief
!> @param params
!> @param geom
!>
SUBROUTINE newGeom_circ(params,geom)
  TYPE(ParamType),INTENT(IN) :: params
  TYPE(CircleType),INTENT(INOUT) :: geom

  REAL(SRK) :: r,theta1,theta2
  REAL(SRK),ALLOCATABLE :: c0(:)
  TYPE(PointType) :: p0

  theta1=0.0_SRK
  theta2=TWOPI
  CALL params%get('CircleGeom -> Radius',r)
  CALL params%get('CircleGeom -> Centroid',c0)
  IF(params%has('CircleGeom -> StartingAngle')) &
      CALL params%get('CircleGeom -> StartingAngle',theta1)
  IF(params%has('CircleGeom -> StoppingAngle')) &
      CALL params%get('CircleGeom -> StoppingAngle',theta2)

  IF(ALLOCATED(c0)) CALL p0%init(COORD=c0)
  CALL geom%set(p0,r,theta1,theta2)
ENDSUBROUTINE newGeom_circ
!
!-------------------------------------------------------------------------------
!> @brief
!> @param params
!> @param geom
!>
SUBROUTINE newGeom_cyl(params,geom)
  TYPE(ParamType),INTENT(IN) :: params
  TYPE(CylinderType),INTENT(INOUT) :: geom

  REAL(SRK) :: r,theta1,theta2
  REAL(SRK),ALLOCATABLE :: c0(:),c1(:)
  TYPE(PointType) :: p0,p1

  theta1=0.0_SRK
  theta2=TWOPI
  CALL params%get('CylinderGeom -> Radius',r)
  CALL params%get('CylinderGeom -> BottomCentroid',c0)
  CALL params%get('CylinderGeom -> TopCentroid',c1)
  IF(params%has('CylinderGeom -> StartingAngle')) &
      CALL params%get('CylinderGeom -> StartingAngle',theta1)
  IF(params%has('CylinderGeom -> StoppingAngle')) &
      CALL params%get('CylinderGeom -> StoppingAngle',theta2)

  IF(ALLOCATED(c0)) CALL p0%init(COORD=c0)
  IF(ALLOCATED(c1)) CALL p1%init(COORD=c1)
  CALL geom%set(p0,p1,r,theta1,theta2)
ENDSUBROUTINE newGeom_cyl
!
!-------------------------------------------------------------------------------
!> @brief
!> @param params
!> @param geom
!>
SUBROUTINE newGeom_poly(params,geom)
  TYPE(ParamType),INTENT(IN) :: params
  TYPE(PolygonType),INTENT(INOUT) :: geom
  CHARACTER(LEN=8) :: ivchar
  INTEGER(SIK) :: i,n,m
  REAL(SRK),ALLOCATABLE :: vert(:),vPrev(:)
  TYPE(GraphType) :: tmpG

  n=0; m=0
  CALL params%get('PolygonGeom->nVert',n)
  CALL params%get('PolygonGeom->nQuad',m)
  IF(n > 2) THEN
    CALL params%get('PolygonGeom->vertex 1',vert)
    CALL tmpG%insertVertex(vert)
    DO i=2,n
      CALL MOVE_ALLOC(vert,vPrev)
      WRITE(ivchar,'(i8)') i; ivchar=ADJUSTL(ivchar)
      CALL params%get('PolygonGeom->vertex '//TRIM(ivchar),vert)
      CALL tmpG%insertVertex(vert)
      CALL tmpG%defineEdge(vPrev,vert)
    ENDDO
    CALL params%get('PolygonGeom->vertex 1',vPrev)
    CALL tmpG%defineEdge(vPrev,vert)
    DO i=1,m
      WRITE(ivchar,'(i8)') i; ivchar=ADJUSTL(ivchar)
      !CALL params%get('PolygonGeom->quad edge '//TRIM(ivchar))
      !Set the quadratic edge data
    ENDDO
    CALL geom%set(tmpG)
    CALL tmpG%clear()
  ENDIF
ENDSUBROUTINE newGeom_poly
!
ENDMODULE Geom
