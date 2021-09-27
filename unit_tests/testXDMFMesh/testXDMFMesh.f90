!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testXDMFMesh
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE Strings
USE XDMFMesh
USE Geom
IMPLICIT NONE

REAL(SDK) :: two_pins_pin1_vertices(3,109) = RESHAPE( (/ &
    0.0000000000000000_SDK,   0.0000000000000000_SDK,   0.0000000000000000_SDK,&
    2.0000000000000000_SDK,   0.0000000000000000_SDK,   0.0000000000000000_SDK,&
    0.0000000000000000_SDK,   2.0000000000000000_SDK,   0.0000000000000000_SDK,&
    2.0000000000000000_SDK,   2.0000000000000000_SDK,   0.0000000000000000_SDK,&
    1.5000000000000000_SDK,   1.0000000000000000_SDK,   0.0000000000000000_SDK,&
   0.50000000000000000_SDK,   0.0000000000000000_SDK,   0.0000000000000000_SDK,&
    1.0000000000000000_SDK,   0.0000000000000000_SDK,   0.0000000000000000_SDK,&
    1.5000000000000000_SDK,   0.0000000000000000_SDK,   0.0000000000000000_SDK,&
   0.25000000000000000_SDK,   0.0000000000000000_SDK,   0.0000000000000000_SDK,&
   0.75000000000000000_SDK,   0.0000000000000000_SDK,   0.0000000000000000_SDK,&
    1.2500000000000000_SDK,   0.0000000000000000_SDK,   0.0000000000000000_SDK,&
    1.7500000000000000_SDK,   0.0000000000000000_SDK,   0.0000000000000000_SDK,&
    0.0000000000000000_SDK,   1.5000000000000000_SDK,   0.0000000000000000_SDK,&
    0.0000000000000000_SDK,   1.0000000000000000_SDK,   0.0000000000000000_SDK,&
    0.0000000000000000_SDK,  0.50000000000000000_SDK,   0.0000000000000000_SDK,&
    0.0000000000000000_SDK,   1.7500000000000000_SDK,   0.0000000000000000_SDK,&
    0.0000000000000000_SDK,   1.2500000000000000_SDK,   0.0000000000000000_SDK,&
    0.0000000000000000_SDK,  0.75000000000000000_SDK,   0.0000000000000000_SDK,&
    0.0000000000000000_SDK,  0.25000000000000000_SDK,   0.0000000000000000_SDK,&
    2.0000000000000000_SDK,  0.50000000000000000_SDK,   0.0000000000000000_SDK,&
    2.0000000000000000_SDK,   1.0000000000000000_SDK,   0.0000000000000000_SDK,&
    2.0000000000000000_SDK,   1.5000000000000000_SDK,   0.0000000000000000_SDK,&
    2.0000000000000000_SDK,  0.25000000000000000_SDK,   0.0000000000000000_SDK,&
    2.0000000000000000_SDK,  0.75000000000000000_SDK,   0.0000000000000000_SDK,&
    2.0000000000000000_SDK,   1.2500000000000000_SDK,   0.0000000000000000_SDK,&
    2.0000000000000000_SDK,   1.7500000000000000_SDK,   0.0000000000000000_SDK,&
    1.5000000000000000_SDK,   2.0000000000000000_SDK,   0.0000000000000000_SDK,&
    1.0000000000000000_SDK,   2.0000000000000000_SDK,   0.0000000000000000_SDK,&
   0.50000000000000000_SDK,   2.0000000000000000_SDK,   0.0000000000000000_SDK,&
    1.7500000000000000_SDK,   2.0000000000000000_SDK,   0.0000000000000000_SDK,&
    1.2500000000000000_SDK,   2.0000000000000000_SDK,   0.0000000000000000_SDK,&
   0.75000000000000000_SDK,   2.0000000000000000_SDK,   0.0000000000000000_SDK,&
   0.25000000000000000_SDK,   2.0000000000000000_SDK,   0.0000000000000000_SDK,&
    1.3117449009294000_SDK,   1.3909157412340001_SDK,   0.0000000000000000_SDK,&
   0.88873953302183994_SDK,   1.4874639560909000_SDK,   0.0000000000000000_SDK,&
   0.54951556604879004_SDK,   1.2169418695587999_SDK,   0.0000000000000000_SDK,&
   0.54951556604879004_SDK,  0.78305813044121997_SDK,   0.0000000000000000_SDK,&
   0.88873953302183994_SDK,  0.51253604390909002_SDK,   0.0000000000000000_SDK,&
    1.3117449009294000_SDK,  0.60908425876599004_SDK,   0.0000000000000000_SDK,&
    1.4504844339512000_SDK,   1.2169418695587999_SDK,   0.0000000000000000_SDK,&
    1.1112604669782000_SDK,   1.4874639560909000_SDK,   0.0000000000000000_SDK,&
   0.68825509907062998_SDK,   1.3909157412340001_SDK,   0.0000000000000000_SDK,&
   0.50000000000000000_SDK,   1.0000000000000000_SDK,   0.0000000000000000_SDK,&
   0.68825509907062998_SDK,  0.60908425876599004_SDK,   0.0000000000000000_SDK,&
    1.1112604669782000_SDK,  0.51253604390909002_SDK,   0.0000000000000000_SDK,&
    1.4504844339512000_SDK,  0.78305813044121997_SDK,   0.0000000000000000_SDK,&
    1.0000000000000000_SDK,   1.0000000000000000_SDK,   0.0000000000000000_SDK,&
    1.1558724504647000_SDK,   1.1954578706170000_SDK,   0.0000000000000000_SDK,&
    1.2500000000000000_SDK,   1.0000000000000000_SDK,   0.0000000000000000_SDK,&
   0.94436976651091997_SDK,   1.2437319780455001_SDK,   0.0000000000000000_SDK,&
    1.1558724504647000_SDK,  0.80454212938298997_SDK,   0.0000000000000000_SDK,&
   0.77475778302440002_SDK,   1.1084709347794000_SDK,   0.0000000000000000_SDK,&
   0.77475778302440002_SDK,  0.89152906522061004_SDK,   0.0000000000000000_SDK,&
   0.94436976651091997_SDK,  0.75626802195454002_SDK,   0.0000000000000000_SDK,&
   0.39945256244833000_SDK,   1.6005474375516999_SDK,   0.0000000000000000_SDK,&
   0.39945256244833000_SDK,  0.39945256244833000_SDK,   0.0000000000000000_SDK,&
    1.6749575669735000_SDK,  0.67495756697348996_SDK,   0.0000000000000000_SDK,&
    1.6749575669735000_SDK,   1.3250424330265000_SDK,   0.0000000000000000_SDK,&
    1.6076357762794000_SDK,   1.6521790377186001_SDK,   0.0000000000000000_SDK,&
    1.6076357762794000_SDK,  0.34782096228136000_SDK,   0.0000000000000000_SDK,&
    1.2323349413967000_SDK,   1.7035415484495000_SDK,   0.0000000000000000_SDK,&
    1.2323349413967000_SDK,  0.29645845155054001_SDK,   0.0000000000000000_SDK,&
   0.27475778302440002_SDK,  0.89152906522061004_SDK,   0.0000000000000000_SDK,&
   0.27475778302440002_SDK,   1.1084709347794000_SDK,   0.0000000000000000_SDK,&
   0.27475778302440002_SDK,  0.64152906522061004_SDK,   0.0000000000000000_SDK,&
   0.27475778302440002_SDK,   1.3584709347794000_SDK,   0.0000000000000000_SDK,&
   0.94436976651091997_SDK,  0.25626802195454002_SDK,   0.0000000000000000_SDK,&
   0.69436976651091997_SDK,  0.25626802195454002_SDK,   0.0000000000000000_SDK,&
   0.69436976651091997_SDK,   1.7437319780455001_SDK,   0.0000000000000000_SDK,&
   0.94436976651091997_SDK,   1.7437319780455001_SDK,   0.0000000000000000_SDK,&
   0.44972628122415997_SDK,   1.8002737187758000_SDK,   0.0000000000000000_SDK,&
   0.64409604773508999_SDK,   1.5440056968213001_SDK,   0.0000000000000000_SDK,&
   0.64409604773508999_SDK,  0.45599430317870998_SDK,   0.0000000000000000_SDK,&
   0.44972628122415997_SDK,  0.19972628122416000_SDK,   0.0000000000000000_SDK,&
   0.47448406424855999_SDK,   1.4087446535552000_SDK,   0.0000000000000000_SDK,&
   0.19972628122416000_SDK,   1.5502737187758000_SDK,   0.0000000000000000_SDK,&
   0.19972628122416000_SDK,  0.44972628122415997_SDK,   0.0000000000000000_SDK,&
   0.47448406424855999_SDK,  0.59125534644476996_SDK,   0.0000000000000000_SDK,&
    1.8038178881397000_SDK,  0.42391048114067997_SDK,   0.0000000000000000_SDK,&
    1.8038178881397000_SDK,  0.17391048114068000_SDK,   0.0000000000000000_SDK,&
    1.8038178881397000_SDK,   1.8260895188593000_SDK,   0.0000000000000000_SDK,&
    1.8038178881397000_SDK,   1.5760895188593000_SDK,   0.0000000000000000_SDK,&
   0.19972628122416000_SDK,  0.19972628122416000_SDK,   0.0000000000000000_SDK,&
   0.19972628122416000_SDK,   1.8002737187758000_SDK,   0.0000000000000000_SDK,&
    1.5538178881397000_SDK,  0.17391048114068000_SDK,   0.0000000000000000_SDK,&
    1.5538178881397000_SDK,   1.8260895188593000_SDK,   0.0000000000000000_SDK,&
    1.1161674706983999_SDK,  0.14822922577527001_SDK,   0.0000000000000000_SDK,&
    1.0605372372093000_SDK,  0.40449724772981999_SDK,   0.0000000000000000_SDK,&
    1.0605372372093000_SDK,   1.5955027522702001_SDK,   0.0000000000000000_SDK,&
    1.1161674706983999_SDK,   1.8517707742247000_SDK,   0.0000000000000000_SDK,&
    1.5874787834866999_SDK,  0.83747878348673999_SDK,   0.0000000000000000_SDK,&
    1.8374787834866999_SDK,  0.83747878348673999_SDK,   0.0000000000000000_SDK,&
    1.7500000000000000_SDK,   1.0000000000000000_SDK,   0.0000000000000000_SDK,&
    1.8374787834866999_SDK,   1.1625212165133001_SDK,   0.0000000000000000_SDK,&
    1.5874787834866999_SDK,   1.1625212165133001_SDK,   0.0000000000000000_SDK,&
    1.8374787834866999_SDK,   1.4125212165133001_SDK,   0.0000000000000000_SDK,&
    1.8374787834866999_SDK,  0.58747878348673999_SDK,   0.0000000000000000_SDK,&
    1.3661674706983999_SDK,  0.14822922577527001_SDK,   0.0000000000000000_SDK,&
    1.3661674706983999_SDK,   1.8517707742247000_SDK,   0.0000000000000000_SDK,&
    1.4933512339513999_SDK,  0.64202091286973995_SDK,   0.0000000000000000_SDK,&
    1.4933512339513999_SDK,   1.3579790871302999_SDK,   0.0000000000000000_SDK,&
    1.6412966716263999_SDK,  0.51138926462741996_SDK,   0.0000000000000000_SDK,&
    1.6412966716263999_SDK,   1.4886107353725999_SDK,   0.0000000000000000_SDK,&
    1.2720399211630999_SDK,  0.45277135515826000_SDK,   0.0000000000000000_SDK,&
    1.2720399211630999_SDK,   1.5472286448417001_SDK,   0.0000000000000000_SDK,&
    1.4199853588381000_SDK,   1.6778602930840001_SDK,   0.0000000000000000_SDK,&
    1.4199853588381000_SDK,  0.32213970691595001_SDK,   0.0000000000000000_SDK,&
    1.4596903386044000_SDK,   1.5215473894763001_SDK,   0.0000000000000000_SDK,&
    1.4596903386044000_SDK,  0.47845261052367000_SDK,   0.0000000000000000_SDK &
/), (/3, 109/))

INTEGER(SIK) :: two_pins_pin1_cells(7,46) = RESHAPE( (/ &
36,  33,  46,   4,  47,  48,  39,&
36,  34,  46,  33,  49,  47,  40,&
36,   4,  46,  38,  48,  50,  45,&
36,  35,  46,  34,  51,  49,  41,&
36,  36,  46,  35,  52,  51,  42,&
36,  38,  46,  37,  50,  53,  44,&
36,  37,  46,  36,  53,  52,  43,&
36,  13,  36,  35,  62,  42,  63,&
36,  14,  36,  13,  64,  62,  17,&
36,  12,  13,  35,  16,  63,  65,&
36,   5,   6,  37,   9,  66,  67,&
36,  28,  34,  27,  68,  69,  31,&
36,  28,  54,  34,  70,  71,  68,&
36,  37,  55,   5,  72,  73,  67,&
36,  35,  54,  12,  74,  75,  65,&
36,  14,  55,  36,  76,  77,  64,&
36,  19,  59,   1,  78,  79,  22,&
36,   3,  58,  21,  80,  81,  25,&
36,   5,  55,   0,  73,  82,   8,&
36,   0,  55,  14,  82,  76,  18,&
36,  12,  54,   2,  75,  83,  15,&
36,   2,  54,  28,  83,  70,  32,&
36,  34,  54,  35,  71,  74,  41,&
36,  36,  55,  37,  77,  72,  43,&
36,   1,  59,   7,  79,  84,  11,&
36,  26,  58,   3,  85,  80,  29,&
36,   6,  61,  37,  86,  87,  66,&
36,  34,  60,  27,  88,  89,  69,&
36,   4,  56,  20,  90,  91,  92,&
36,  20,  57,   4,  93,  94,  92,&
36,  21,  57,  20,  95,  93,  24,&
36,  20,  56,  19,  91,  96,  23,&
36,   7,  61,   6,  97,  86,  10,&
36,  27,  60,  26,  89,  98,  30,&
36,  38,  56,   4,  99,  90,  45,&
36,   4,  57,  33,  94, 100,  39,&
36,  56,  59,  19, 101,  78,  96,&
36,  21,  58,  57,  81, 102,  95,&
36,  37,  61,  38,  87, 103,  44,&
36,  33,  60,  34, 104,  88,  40,&
36,  26,  60,  58,  98, 105,  85,&
36,  59,  61,   7, 106,  97,  84,&
36,  58,  60,  33, 105, 104, 107,&
36,  38,  61,  59, 103, 106, 108,&
36,  38,  59,  56, 108, 101,  99,&
36,  57,  58,  33, 102, 107, 100 &
/), (/7, 46/))

INTEGER(SIK) :: two_pins_pin1_material_ids(46) = (/ &
0, 0, 0, 0, 0, 0, 0,          &
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
1, 1, 1, 1, 1, 1, 1, 1, 1     &
/)

CREATE_TEST('XDMF TYPE')
REGISTER_SUBTEST('CLEAR', testClear)
REGISTER_SUBTEST('ASSIGNMENT', testAssign)
REGISTER_SUBTEST('GET POINTS', testGetPoints)
REGISTER_SUBTEST('GET CELL AREA', testGetCellArea)
REGISTER_SUBTEST('POINT INSIDE CELL', testPointInsideCell)
FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE setup_pin1(mesh)
  TYPE(XDMFMeshType_2D), INTENT(INOUT), TARGET :: mesh
  INTEGER(SIK) :: i
  TYPE(PointType) :: p1, p2, p3
  CALL p1%init(DIM = 2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM = 2, X=2.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM = 2, X=1.0_SRK, Y=1.0_SRK)

  ! Setup a mesh equivalent to gridmesh_two_pins.xdmf, only containing pin1
  mesh%name = "GRID_L1_1_1"
  ALLOCATE(mesh%points(109))
  DO i = 1, 109
    CALL mesh%points(i)%init(DIM=2, X=two_pins_pin1_vertices(1,i), &
                                    Y=two_pins_pin1_vertices(2,i))
  ENDDO
  ALLOCATE(mesh%cells(46))
  DO i = 1,46
    ALLOCATE(mesh%cells(i)%point_list(7))
    mesh%cells(i)%point_list(1) = two_pins_pin1_cells(1,i)
    mesh%cells(i)%point_list(2:) = two_pins_pin1_cells(2:,i) + 1
  ENDDO
  mesh%material_ids = two_pins_pin1_material_ids + 1
  ALLOCATE(mesh%cell_sets(1))
  ALLOCATE(mesh%cell_sets(1)%cell_list(46))
  mesh%cell_sets(1)%name = "Pin_1"
  DO i = 1,46
    mesh%cell_sets(1)%cell_list(i) = i
  ENDDO
ENDSUBROUTINE setup_pin1
!
!-------------------------------------------------------------------------------
SUBROUTINE testClear()
  TYPE(XDMFMeshType_2D) :: mesh

  CALL setup_pin1(mesh)

  CALL mesh%clear()
  ASSERT(mesh%name == "", "mesh mesh name is incorrect")
  ASSERT(.NOT.ALLOCATED(mesh%points), "Vertices are allocated")
  ASSERT(.NOT.ALLOCATED(mesh%cells), "Cells are allocated")
  ASSERT(.NOT.ALLOCATED(mesh%material_ids), "materials are allocated")
  ASSERT(.NOT.ALLOCATED(mesh%cell_sets), "Cell sets are allocated")
ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
SUBROUTINE testAssign()
  TYPE(XDMFMeshType_2D) :: mesh1, mesh2
  INTEGER(SIK) :: i,j
  TYPE(PointType) :: p1, p2, p3
  CALL p1%init(DIM = 2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM = 2, X=2.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM = 2, X=1.0_SRK, Y=1.0_SRK)

  CALL setup_pin1(mesh1)
  mesh2 = mesh1
  ASSERT(mesh2%name == "GRID_L1_1_1", "mesh2 mesh name is incorrect")
  ! points
  ASSERT(ALLOCATED(mesh2%points), "points not allocated")
  ASSERT(SIZE(mesh2%points)==109, "Wrong number of points")
  DO i=1,109
    DO j=1,2
      ASSERT( (ABS(mesh2%points(i)%coord(j) - two_pins_pin1_vertices(j,i)) < 1.0E-9), "Unequal points")
    ENDDO
  ENDDO
  ! cells
  ASSERT(ALLOCATED(mesh2%cells), "Cells not allocated")
  ASSERT(SIZE(mesh2%cells)==46, "Wrong number of cells")
  DO i=1,46
    ASSERT(SIZE(mesh2%cells(i)%point_list)==7, "Wrong size for point list")
    ASSERT( mesh2%cells(i)%point_list(1) == two_pins_pin1_cells(1, i), "Wrong cell type")
    DO j=2,7
      ASSERT( mesh2%cells(i)%point_list(j) == two_pins_pin1_cells(j, i) + 1, "Wrong point id")
    ENDDO
  ENDDO
  ! material_ids
  ASSERT(ALLOCATED(mesh2%material_ids), "material_ids not allocated")
  ASSERT(SIZE(mesh2%material_ids)==46, "Wrong number of cells")
  DO i=1,46
    ASSERT( mesh2%material_ids(i) == two_pins_pin1_material_ids(i) + 1, "Unequal material_id")
  ENDDO
  ! cell_sets
  ASSERT(ALLOCATED(mesh2%cell_sets), "cell_sets not allocated")
  ASSERT(SIZE(mesh2%cell_sets)==1, "Wrong number of cell sets")
  ASSERT(SIZE(mesh2%cell_sets(1)%cell_list)==46, "Wrong number of cells")
  ASSERT(mesh2%cell_sets(1)%name=="Pin_1", "Wrong cell_set name")
  DO i=1,46
    ASSERT( mesh2%cell_sets(1)%cell_list(i) == i, "Wrong cells")
  ENDDO

  CALL mesh1%clear()
  CALL mesh2%clear()
ENDSUBROUTINE testAssign
!
!-------------------------------------------------------------------------------
SUBROUTINE testGetPoints()
  TYPE(XDMFMeshType_2D) :: mesh
  INTEGER(SIK) :: ids(3), i
  TYPE(PointType) :: points(3)

  CALL setup_pin1(mesh)
  ids = (/ 1, 2, 3/)  
  points = mesh%getPoints(ids)
  DO i = 1, 3
    ASSERT(points(i)%dim == 2, "Wrong dim")
  ENDDO
  ASSERT(points(1)%coord(1) == 0.0_SRK, "Wrong coord")
  ASSERT(points(1)%coord(2) == 0.0_SRK, "Wrong coord")
  ASSERT(points(2)%coord(1) == 2.0_SRK, "Wrong coord")
  ASSERT(points(2)%coord(2) == 0.0_SRK, "Wrong coord")
  ASSERT(points(3)%coord(1) == 0.0_SRK, "Wrong coord")
  ASSERT(points(3)%coord(2) == 2.0_SRK, "Wrong coord")

  CALL mesh%clear()
  CALL points%clear()
ENDSUBROUTINE testGetPoints
!
!-------------------------------------------------------------------------------
SUBROUTINE testGetCellArea()
  TYPE(XDMFMeshType_2D) :: mesh
  REAL(SRK) :: area

  ALLOCATE(mesh%points(21))
  ALLOCATE(mesh%cells(4))
  ! Triangle
  CALL mesh%points(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL mesh%points(2)%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL mesh%points(3)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  ALLOCATE(mesh%cells(1)%point_list(4))
  mesh%cells(1)%point_list = (/ 4, 1, 2, 3/)
  area = mesh%getCellArea(1)
  ASSERT( area .APPROXEQA. 0.5_SRK, "Wrong area")
  ! Quadrilateral
  CALL mesh%points(4)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL mesh%points(5)%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL mesh%points(6)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL mesh%points(7)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  ALLOCATE(mesh%cells(2)%point_list(5))
  mesh%cells(2)%point_list = (/ 5, 4, 5, 6, 7/)
  area = mesh%getCellArea(2)
  ASSERT( area .APPROXEQA. 1.0_SRK, "Wrong area")
  ! Triangle6
  CALL mesh%points(8)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL mesh%points(9)%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL mesh%points(10)%init(DIM=2, X=2.0_SRK, Y=2.0_SRK)
  CALL mesh%points(11)%init(DIM=2, X=1.5_SRK, Y=0.25_SRK)
  CALL mesh%points(12)%init(DIM=2, X=3.0_SRK, Y=1.0_SRK)
  CALL mesh%points(13)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  ALLOCATE(mesh%cells(3)%point_list(7))
  mesh%cells(3)%point_list = (/ 36, 8, 9, 10, 11, 12, 13/)
  area = mesh%getCellArea(3)
  ASSERT( (area - 3.0_SRK) < 1.0E-5, "Wrong area")
  ! Quadrilateral8
  CALL mesh%points(14)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL mesh%points(15)%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL mesh%points(16)%init(DIM=2, X=2.0_SRK, Y=3.0_SRK)
  CALL mesh%points(17)%init(DIM=2, X=0.0_SRK, Y=3.0_SRK)
  CALL mesh%points(18)%init(DIM=2, X=1.5_SRK, Y=0.5_SRK)
  CALL mesh%points(19)%init(DIM=2, X=2.5_SRK, Y=1.5_SRK)
  CALL mesh%points(20)%init(DIM=2, X=1.5_SRK, Y=2.5_SRK)
  CALL mesh%points(21)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  ALLOCATE(mesh%cells(4)%point_list(9))
  mesh%cells(4)%point_list = (/ 37, 14, 15, 16, 17, 18, 19, 20, 21/)
  area = mesh%getCellArea(4)
  ASSERT( SOFTEQ(area, 17.0_SRK/3.0_SRK, 1.0E-6_SRK), "Wrong area")
  ! ELEMENTAL
  area = SUM(mesh%getCellArea((/1, 2, 3, 4/)))
  ASSERT( SOFTEQ(area, 4.5_SRK + 17.0_SRK/3.0_SRK, 1.0E-6_SRK), "Wrong area")
ENDSUBROUTINE testGetCellArea
!
!-------------------------------------------------------------------------------
SUBROUTINE testPointInsideCell()
  TYPE(XDMFMeshType_2D) :: mesh
  TYPE(PointType) :: p

  ALLOCATE(mesh%points(21))
  ALLOCATE(mesh%cells(4))
  ! Triangle
  CALL mesh%points(1)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL mesh%points(2)%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL mesh%points(3)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  ALLOCATE(mesh%cells(1)%point_list(4))
  mesh%cells(1)%point_list = (/ 4, 1, 2, 3/)
  ! Quadrilateral
  CALL mesh%points(4)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL mesh%points(5)%init(DIM=2, X=1.0_SRK, Y=0.0_SRK)
  CALL mesh%points(6)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL mesh%points(7)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  ALLOCATE(mesh%cells(2)%point_list(5))
  mesh%cells(2)%point_list = (/ 5, 4, 5, 6, 7/)
  ! Triangle6
  CALL mesh%points(8)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL mesh%points(9)%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL mesh%points(10)%init(DIM=2, X=2.0_SRK, Y=2.0_SRK)
  CALL mesh%points(11)%init(DIM=2, X=1.5_SRK, Y=0.25_SRK)
  CALL mesh%points(12)%init(DIM=2, X=3.0_SRK, Y=1.0_SRK)
  CALL mesh%points(13)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  ALLOCATE(mesh%cells(3)%point_list(7))
  mesh%cells(3)%point_list = (/ 36, 8, 9, 10, 11, 12, 13/)
  ! Quadrilateral8
  CALL mesh%points(14)%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL mesh%points(15)%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL mesh%points(16)%init(DIM=2, X=2.0_SRK, Y=3.0_SRK)
  CALL mesh%points(17)%init(DIM=2, X=0.0_SRK, Y=3.0_SRK)
  CALL mesh%points(18)%init(DIM=2, X=1.5_SRK, Y=0.5_SRK)
  CALL mesh%points(19)%init(DIM=2, X=2.5_SRK, Y=1.5_SRK)
  CALL mesh%points(20)%init(DIM=2, X=1.5_SRK, Y=2.5_SRK)
  CALL mesh%points(21)%init(DIM=2, X=0.0_SRK, Y=1.0_SRK)
  ALLOCATE(mesh%cells(4)%point_list(9))
  mesh%cells(4)%point_list = (/ 37, 14, 15, 16, 17, 18, 19, 20, 21/)

  CALL p%init(DIM=2, X=0.5_SRK, Y=0.1_SRK)
  ASSERT(mesh%pointInsideCell(1, p), "Point should be in") 
  ASSERT(mesh%pointInsideCell(2, p), "Point should be in") 
  CALL p%clear()
  CALL p%init(DIM=2, X=1.0_SRK, Y=0.5_SRK)
  ASSERT(mesh%pointInsideCell(3, p), "Point should be in") 
  ASSERT(mesh%pointInsideCell(4, p), "Point should be in") 
  CALL p%clear()
  CALL p%init(DIM=2, X=-10.0_SRK, Y=0.5_SRK)
  ASSERT(.NOT.mesh%pointInsideCell(1, p), "Point should not be in") 
  ASSERT(.NOT.mesh%pointInsideCell(2, p), "Point should not be in") 
  ASSERT(.NOT.mesh%pointInsideCell(3, p), "Point should not be in") 
  ASSERT(.NOT.mesh%pointInsideCell(4, p), "Point should not be in") 
  CALL p%clear()
  CALL mesh%clear()
ENDSUBROUTINE testPointInsideCell
ENDPROGRAM
