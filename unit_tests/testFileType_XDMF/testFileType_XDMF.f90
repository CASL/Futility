!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testXDMFFileType
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE Strings
USE FileType_XDMF
IMPLICIT NONE

REAL(SDK) :: two_pins_pin1_vertices(3,109) = RESHAPE( (/ &
    0.0000000000000000,        0.0000000000000000,        0.0000000000000000,&
    2.0000000000000000,        0.0000000000000000,        0.0000000000000000,&
    0.0000000000000000,        2.0000000000000000,        0.0000000000000000,&
    2.0000000000000000,        2.0000000000000000,        0.0000000000000000,&
    1.5000000000000000,        1.0000000000000000,        0.0000000000000000,&
   0.50000000000000000,        0.0000000000000000,        0.0000000000000000,&
    1.0000000000000000,        0.0000000000000000,        0.0000000000000000,&
    1.5000000000000000,        0.0000000000000000,        0.0000000000000000,&
   0.25000000000000000,        0.0000000000000000,        0.0000000000000000,&
   0.75000000000000000,        0.0000000000000000,        0.0000000000000000,&
    1.2500000000000000,        0.0000000000000000,        0.0000000000000000,&
    1.7500000000000000,        0.0000000000000000,        0.0000000000000000,&
    0.0000000000000000,        1.5000000000000000,        0.0000000000000000,&
    0.0000000000000000,        1.0000000000000000,        0.0000000000000000,&
    0.0000000000000000,       0.50000000000000000,        0.0000000000000000,&
    0.0000000000000000,        1.7500000000000000,        0.0000000000000000,&
    0.0000000000000000,        1.2500000000000000,        0.0000000000000000,&
    0.0000000000000000,       0.75000000000000000,        0.0000000000000000,&
    0.0000000000000000,       0.25000000000000000,        0.0000000000000000,&
    2.0000000000000000,       0.50000000000000000,        0.0000000000000000,&
    2.0000000000000000,        1.0000000000000000,        0.0000000000000000,&
    2.0000000000000000,        1.5000000000000000,        0.0000000000000000,&
    2.0000000000000000,       0.25000000000000000,        0.0000000000000000,&
    2.0000000000000000,       0.75000000000000000,        0.0000000000000000,&
    2.0000000000000000,        1.2500000000000000,        0.0000000000000000,&
    2.0000000000000000,        1.7500000000000000,        0.0000000000000000,&
    1.5000000000000000,        2.0000000000000000,        0.0000000000000000,&
    1.0000000000000000,        2.0000000000000000,        0.0000000000000000,&
   0.50000000000000000,        2.0000000000000000,        0.0000000000000000,&
    1.7500000000000000,        2.0000000000000000,        0.0000000000000000,&
    1.2500000000000000,        2.0000000000000000,        0.0000000000000000,&
   0.75000000000000000,        2.0000000000000000,        0.0000000000000000,&
   0.25000000000000000,        2.0000000000000000,        0.0000000000000000,&
    1.3117449009294000,        1.3909157412340001,        0.0000000000000000,&
   0.88873953302183994,        1.4874639560909000,        0.0000000000000000,&
   0.54951556604879004,        1.2169418695587999,        0.0000000000000000,&
   0.54951556604879004,       0.78305813044121997,        0.0000000000000000,&
   0.88873953302183994,       0.51253604390909002,        0.0000000000000000,&
    1.3117449009294000,       0.60908425876599004,        0.0000000000000000,&
    1.4504844339512000,        1.2169418695587999,        0.0000000000000000,&
    1.1112604669782000,        1.4874639560909000,        0.0000000000000000,&
   0.68825509907062998,        1.3909157412340001,        0.0000000000000000,&
   0.50000000000000000,        1.0000000000000000,        0.0000000000000000,&
   0.68825509907062998,       0.60908425876599004,        0.0000000000000000,&
    1.1112604669782000,       0.51253604390909002,        0.0000000000000000,&
    1.4504844339512000,       0.78305813044121997,        0.0000000000000000,&
    1.0000000000000000,        1.0000000000000000,        0.0000000000000000,&
    1.1558724504647000,        1.1954578706170000,        0.0000000000000000,&
    1.2500000000000000,        1.0000000000000000,        0.0000000000000000,&
   0.94436976651091997,        1.2437319780455001,        0.0000000000000000,&
    1.1558724504647000,       0.80454212938298997,        0.0000000000000000,&
   0.77475778302440002,        1.1084709347794000,        0.0000000000000000,&
   0.77475778302440002,       0.89152906522061004,        0.0000000000000000,&
   0.94436976651091997,       0.75626802195454002,        0.0000000000000000,&
   0.39945256244833000,        1.6005474375516999,        0.0000000000000000,&
   0.39945256244833000,       0.39945256244833000,        0.0000000000000000,&
    1.6749575669735000,       0.67495756697348996,        0.0000000000000000,&
    1.6749575669735000,        1.3250424330265000,        0.0000000000000000,&
    1.6076357762794000,        1.6521790377186001,        0.0000000000000000,&
    1.6076357762794000,       0.34782096228136000,        0.0000000000000000,&
    1.2323349413967000,        1.7035415484495000,        0.0000000000000000,&
    1.2323349413967000,       0.29645845155054001,        0.0000000000000000,&
   0.27475778302440002,       0.89152906522061004,        0.0000000000000000,&
   0.27475778302440002,        1.1084709347794000,        0.0000000000000000,&
   0.27475778302440002,       0.64152906522061004,        0.0000000000000000,&
   0.27475778302440002,        1.3584709347794000,        0.0000000000000000,&
   0.94436976651091997,       0.25626802195454002,        0.0000000000000000,&
   0.69436976651091997,       0.25626802195454002,        0.0000000000000000,&
   0.69436976651091997,        1.7437319780455001,        0.0000000000000000,&
   0.94436976651091997,        1.7437319780455001,        0.0000000000000000,&
   0.44972628122415997,        1.8002737187758000,        0.0000000000000000,&
   0.64409604773508999,        1.5440056968213001,        0.0000000000000000,&
   0.64409604773508999,       0.45599430317870998,        0.0000000000000000,&
   0.44972628122415997,       0.19972628122416000,        0.0000000000000000,&
   0.47448406424855999,        1.4087446535552000,        0.0000000000000000,&
   0.19972628122416000,        1.5502737187758000,        0.0000000000000000,&
   0.19972628122416000,       0.44972628122415997,        0.0000000000000000,&
   0.47448406424855999,       0.59125534644476996,        0.0000000000000000,&
    1.8038178881397000,       0.42391048114067997,        0.0000000000000000,&
    1.8038178881397000,       0.17391048114068000,        0.0000000000000000,&
    1.8038178881397000,        1.8260895188593000,        0.0000000000000000,&
    1.8038178881397000,        1.5760895188593000,        0.0000000000000000,&
   0.19972628122416000,       0.19972628122416000,        0.0000000000000000,&
   0.19972628122416000,        1.8002737187758000,        0.0000000000000000,&
    1.5538178881397000,       0.17391048114068000,        0.0000000000000000,&
    1.5538178881397000,        1.8260895188593000,        0.0000000000000000,&
    1.1161674706983999,       0.14822922577527001,        0.0000000000000000,&
    1.0605372372093000,       0.40449724772981999,        0.0000000000000000,&
    1.0605372372093000,        1.5955027522702001,        0.0000000000000000,&
    1.1161674706983999,        1.8517707742247000,        0.0000000000000000,&
    1.5874787834866999,       0.83747878348673999,        0.0000000000000000,&
    1.8374787834866999,       0.83747878348673999,        0.0000000000000000,&
    1.7500000000000000,        1.0000000000000000,        0.0000000000000000,&
    1.8374787834866999,        1.1625212165133001,        0.0000000000000000,&
    1.5874787834866999,        1.1625212165133001,        0.0000000000000000,&
    1.8374787834866999,        1.4125212165133001,        0.0000000000000000,&
    1.8374787834866999,       0.58747878348673999,        0.0000000000000000,&
    1.3661674706983999,       0.14822922577527001,        0.0000000000000000,&
    1.3661674706983999,        1.8517707742247000,        0.0000000000000000,&
    1.4933512339513999,       0.64202091286973995,        0.0000000000000000,&
    1.4933512339513999,        1.3579790871302999,        0.0000000000000000,&
    1.6412966716263999,       0.51138926462741996,        0.0000000000000000,&
    1.6412966716263999,        1.4886107353725999,        0.0000000000000000,&
    1.2720399211630999,       0.45277135515826000,        0.0000000000000000,&
    1.2720399211630999,        1.5472286448417001,        0.0000000000000000,&
    1.4199853588381000,        1.6778602930840001,        0.0000000000000000,&
    1.4199853588381000,       0.32213970691595001,        0.0000000000000000,&
    1.4596903386044000,        1.5215473894763001,        0.0000000000000000,&
    1.4596903386044000,       0.47845261052367000,        0.0000000000000000 &
/), (/3, 109/))

INTEGER(SLK) :: two_pins_pin1_cells(7,46) = RESHAPE( (/ &
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

INTEGER(SNK) :: two_pins_pin1_material_ids(46) = (/ &
0, 0, 0, 0, 0, 0, 0,          &
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
1, 1, 1, 1, 1, 1, 1, 1, 1     &
/)

REAL(SDK) :: three_level_grid_L3_vertices(3,5) = RESHAPE( (/ &
 2.0, 1.5, 0.0,&
 2.0, 1.0, 0.0,&
 3.0, 1.0, 0.0,&
 2.0, 2.0, 0.0,&
 3.0, 2.0, 0.0 &
/), (/3, 5/))


INTEGER(SLK) :: three_level_grid_L3_cells(3,3) = RESHAPE( (/ &
0, 2, 4, &
1, 2, 0, &
3, 0, 4  &
/), (/3, 3/))


CREATE_TEST('XDMF TYPE')
REGISTER_SUBTEST('two_pins',test_two_pins)
REGISTER_SUBTEST('three_level_grid',test_three_level_grid)
REGISTER_SUBTEST('three_level_grid_IH', test_three_level_grid_implicit_hierarchy)


FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE test_two_pins()
  TYPE(XDMFFileType) :: testXDMFFile
  TYPE(XDMFMeshType) :: mesh, pin1
  TYPE(StringType) :: fname
  INTEGER(SIK) :: i,j

  fname='gridmesh_two_pins.xdmf'
  CALL testXDMFFile%importFromDisk(fname, mesh)
  ! Check correct number of children
  ASSERT(mesh%name == "mesh_domain", "Root mesh name is incorrect")
  ASSERT(ASSOCIATED(mesh%children), "Children not associated")
  ASSERT(SIZE(mesh%children)==2, "Wrong number of children")
  ! Check pin1
  pin1 = mesh%children(1)
  ASSERT(pin1%name == "GRID_L1_1_1", "pin1 mesh name is incorrect")
  ASSERT(.NOT.ASSOCIATED(pin1%children), "Children are associated")
  ASSERT(ASSOCIATED(pin1%parent), "Parent not associated")
  ASSERT(pin1%parent%name == "mesh_domain", "pin1 parent name is incorrect")
  ASSERT(pin1%singleTopology == .TRUE., "pin1 is not single topology")
  !     pin1 vertices
  ASSERT(ALLOCATED(pin1%vertices), "Vertices not allocated")
  ASSERT(SIZE(pin1%vertices)==109*3, "Wrong number of vertices")
  ASSERT(SIZE(pin1%vertices, DIM=2)==109, "Wrong shape of vertices")
  DO i=1,109
    DO j=1,3
      ASSERT( (ABS(pin1%vertices(j, i) - two_pins_pin1_vertices(j,i)) < 1.0E-6), "Unequal vertices")
    ENDDO
  ENDDO
  !     pin1 cells
  ASSERT(ALLOCATED(pin1%cells), "Cells not allocated")
  ASSERT(SIZE(pin1%cells)==46, "Wrong number of cells")
  DO i=1,46
    ASSERT(SIZE(pin1%cells(i)%vertex_list)==7, "Wrong size for vertex list")
    ASSERT( pin1%cells(i)%vertex_list(1) == two_pins_pin1_cells(1, i), "Wrong cell type")
    DO j=2,7
      ASSERT( pin1%cells(i)%vertex_list(j) == two_pins_pin1_cells(j, i) + 1, "Wrong vertex id")
    ENDDO
  ENDDO
  !     pin1 material_ids
  ASSERT(ALLOCATED(pin1%material_ids), "material_ids not allocated")
  ASSERT(SIZE(pin1%material_ids)==46, "Wrong number of cells")
  DO i=1,46
    ASSERT( pin1%material_ids(i) == two_pins_pin1_material_ids(i) + 1, "Unequal material_id")
  ENDDO
  !     pin1 cell_sets
  ASSERT(ALLOCATED(pin1%cell_sets), "cell_sets not allocated")
  ASSERT(SIZE(pin1%cell_sets)==1, "Wrong number of cell sets")
  ASSERT(SIZE(pin1%cell_sets(1)%cell_list)==46, "Wrong number of cells")
  ASSERT(pin1%cell_sets(1)%name=="Pin_1", "Wrong cell_set name")
  DO i=1,46
    ASSERT( pin1%cell_sets(1)%cell_list(i) == i, "Wrong cells")
  ENDDO
  !     pin1 clear
  CALL pin1%clear()
  ASSERT(pin1%name == "", "pin1 mesh name is incorrect")
  ASSERT(pin1%singleTopology == .FALSE., "single topology did not reset")
  ASSERT(.NOT.ALLOCATED(pin1%vertices), "Vertices are associated")
  ASSERT(.NOT.ALLOCATED(pin1%cells), "Cells are associated")
  ASSERT(.NOT.ALLOCATED(pin1%material_ids), "materials are associated")
  ASSERT(.NOT.ALLOCATED(pin1%cell_sets), "Cell sets are associated")
  ASSERT(.NOT.ASSOCIATED(pin1%children), "Children are associated")
  ASSERT(.NOT.ASSOCIATED(pin1%parent), "Parent is associated")
  !     Check that clear did not interfere with the original mesh
  pin1 = mesh%children(1)
  ASSERT(pin1%name == "GRID_L1_1_1", "pin1 mesh name is incorrect")
  ASSERT(.NOT.ASSOCIATED(pin1%children), "Children are associated")
  ASSERT(ASSOCIATED(pin1%parent), "Parent not associated")
  ASSERT(pin1%parent%name == "mesh_domain", "pin1 parent name is incorrect")
  ASSERT(pin1%singleTopology == .TRUE., "pin1 is not single topology")
  !     pin1 vertices
  ASSERT(ALLOCATED(pin1%vertices), "Vertices not allocated")
  ASSERT(SIZE(pin1%vertices)==109*3, "Wrong number of vertices")
  ASSERT(SIZE(pin1%vertices, DIM=2)==109, "Wrong shape of vertices")
  DO i=1,109
    DO j=1,3
      ASSERT( (ABS(pin1%vertices(j, i) - two_pins_pin1_vertices(j,i)) < 1.0E-6), "Unequal vertices")
    ENDDO
  ENDDO
  !     pin1 cells
  ASSERT(ALLOCATED(pin1%cells), "Cells not allocated")
  ASSERT(SIZE(pin1%cells)==46, "Wrong number of cells")
  DO i=1,46
    ASSERT(SIZE(pin1%cells(i)%vertex_list)==7, "Wrong size for vertex list")
    ASSERT( pin1%cells(i)%vertex_list(1) == two_pins_pin1_cells(1, i), "Wrong cell type")
    DO j=2,7
      ASSERT( pin1%cells(i)%vertex_list(j) == two_pins_pin1_cells(j, i) + 1, "Wrong vertex id")
    ENDDO
  ENDDO
  !     pin1 material_ids
  ASSERT(ALLOCATED(pin1%material_ids), "material_ids not allocated")
  ASSERT(SIZE(pin1%material_ids)==46, "Wrong number of cells")
  DO i=1,46
    ASSERT( pin1%material_ids(i) == two_pins_pin1_material_ids(i) + 1, "Unequal material_id")
  ENDDO
  !     pin1 cell_sets
  ASSERT(ALLOCATED(pin1%cell_sets), "cell_sets not allocated")
  ASSERT(SIZE(pin1%cell_sets)==1, "Wrong number of cell sets")
  ASSERT(SIZE(pin1%cell_sets(1)%cell_list)==46, "Wrong number of cells")
  ASSERT(pin1%cell_sets(1)%name=="Pin_1", "Wrong cell_set name")
  DO i=1,46
    ASSERT( pin1%cell_sets(1)%cell_list(i) == i, "Wrong cells")
  ENDDO
ENDSUBROUTINE test_two_pins
!
!-------------------------------------------------------------------------------
SUBROUTINE test_three_level_grid()
  TYPE(XDMFFileType) :: testXDMFFile
  TYPE(XDMFMeshType) :: mesh, L1, L2, L3
  TYPE(StringType) :: fname
  INTEGER(SIK) :: i,j

  fname='gridmesh_three_level_grid.xdmf'
  CALL testXDMFFile%importFromDisk(fname, mesh)
  ! Check correct number of children
  ASSERT(mesh%name == "three_lvl_grid", "Root mesh name is incorrect")
  ASSERT(ASSOCIATED(mesh%children), "Children not associated")
  ASSERT(SIZE(mesh%children)==1, "Wrong number of children")
  ! Check L1
  L1 = mesh%children(1)
  ASSERT(L1%name == "GRID_L1_1_1", "L1 mesh name is incorrect")
  ASSERT(ASSOCIATED(L1%children), "Children are not associated")
  ASSERT(ASSOCIATED(L1%parent), "Parent not associated")
  ASSERT(L1%parent%name == "three_lvl_grid", "L1 parent name is incorrect")
  ASSERT(SIZE(L1%children) == 4, "Wrong number of children")
  ! Check L2_2_1
  L2 = L1%children(2)
  ASSERT(L2%name == "GRID_L2_2_1", "L2 mesh name is incorrect")
  ASSERT(ASSOCIATED(L2%children), "Children are not associated")
  ASSERT(ASSOCIATED(L2%parent), "Parent not associated")
  ASSERT(L2%parent%name == "GRID_L1_1_1", "L2 parent name is incorrect")
  ASSERT(SIZE(L2%children) == 4, "Wrong number of children")
  ! Check L3_3_2
  L3 = L2%children(3)
  ASSERT(L3%name == "GRID_L3_3_2", "L3 mesh name is incorrect")
  ASSERT(.NOT. ASSOCIATED(L3%children), "Children are associated")
  ASSERT(ASSOCIATED(L3%parent), "Parent not associated")
  ASSERT(L3%parent%name == "GRID_L2_2_1", "L3 parent name is incorrect")
  !     L3_3_2 vertices
  ASSERT(ALLOCATED(L3%vertices), "Vertices not allocated")
  ASSERT(SIZE(L3%vertices)==5*3, "Wrong number of vertices")
  ASSERT(SIZE(L3%vertices, DIM=2)==5, "Wrong shape of vertices")
  DO i=1,5
    DO j=1,3
      ASSERT( (ABS(L3%vertices(j, i) - three_level_grid_L3_vertices(j,i)) < 1.0E-6), "Unequal vertices")
    ENDDO
  ENDDO
  !     L3_3_2 cells
  ASSERT(ALLOCATED(L3%cells), "Cells not allocated")
  ASSERT(SIZE(L3%cells)==3, "Wrong number of cells")
  ASSERT(L3%singleTopology == .TRUE., "L3 is not single topology")
  DO i=1,3
    ASSERT(SIZE(L3%cells(i)%vertex_list)==4, "Wrong size for vertex list")
    ASSERT( L3%cells(i)%vertex_list(1) == 4, "Wrong cell type, should be triangle=4")
    DO j=2,4
      ASSERT( L3%cells(i)%vertex_list(j) == three_level_grid_L3_cells(j-1, i) + 1, "Wrong vertex id")
    ENDDO
  ENDDO
  ASSERT(.NOT. ALLOCATED(L3%material_ids), "Material IDS are allocated")
  ASSERT(.NOT. ALLOCATED(L3%cell_sets), "Cell sets are allocated")
ENDSUBROUTINE test_three_level_grid
!
!-------------------------------------------------------------------------------
SUBROUTINE test_three_level_grid_implicit_hierarchy()
  TYPE(XDMFFileType) :: testXDMFFile
  TYPE(XDMFMeshType) :: mesh
  TYPE(StringType) :: fname
  INTEGER(SIK) :: i,j
  INTEGER(SLK),ALLOCATABLE :: cells_ref(:)

  fname='three_level_grid.xdmf'
  CALL testXDMFFile%importFromDisk(fname, mesh)
  ! Check correct number of children
  ASSERT(mesh%name == "three_lvl_grid", "Root mesh name is incorrect")
  ASSERT(.NOT.ASSOCIATED(mesh%children), "Children are associated")
  ! vertices
  ASSERT(ALLOCATED(mesh%vertices), "Vertices not allocated")
  ASSERT(SIZE(mesh%vertices)==42*3, "Wrong number of vertices")
  ASSERT(SIZE(mesh%vertices, DIM=2)==42, "Wrong shape of vertices")
  ! cells
  ASSERT(ALLOCATED(mesh%cells), "Cells not allocated")
  ASSERT(SIZE(mesh%cells)==46, "Wrong number of cells")
  ASSERT(mesh%singleTopology == .FALSE., "Mesh is single topology")
  ! Spot check cells
  ! Cell 1, quad
  j=1
  ALLOCATE(cells_ref(5))
  cells_ref = (/5, 26, 2, 27, 38/)
  ASSERT(SIZE(mesh%cells(j)%vertex_list)==5, "Wrong size for vertex list")
  DO i=1,5
    ASSERT( mesh%cells(j)%vertex_list(i) == cells_ref(i), "Wrong vertex id or mesh id")
  ENDDO
  ! Cell 4, quad
  j=4
  cells_ref = (/5, 4, 29, 38, 28/)
  ASSERT(SIZE(mesh%cells(j)%vertex_list)==5, "Wrong size for vertex list")
  DO i=1,5
    ASSERT( mesh%cells(j)%vertex_list(i) == cells_ref(i), "Wrong vertex id or mesh id")
  ENDDO
  DEALLOCATE(cells_ref)
  ! Cell 18, tri
  j=18
  ALLOCATE(cells_ref(4))
  cells_ref = (/4, 6, 40, 8/)
  ASSERT(SIZE(mesh%cells(j)%vertex_list)==4, "Wrong size for vertex list")
  DO i=1,4
    ASSERT( mesh%cells(j)%vertex_list(i) == cells_ref(i), "Wrong vertex id or mesh id")
  ENDDO
  DEALLOCATE(cells_ref)
  ASSERT(.NOT. ALLOCATED(mesh%material_ids), "Material IDS are allocated")
  ! Check cell sets
  ASSERT(ALLOCATED(mesh%cell_sets), "Cell sets are allocated")
  ASSERT(SIZE(mesh%cell_sets)==21, "Wrong number of cell sets")
  ASSERT(mesh%cell_sets(6)%name=="GRID_L3_1_1", "Wrong set name")
  ASSERT(SIZE(mesh%cell_sets(6)%cell_list)==4, "Wrong set size")
  DO i =1,4
    ASSERT(mesh%cell_sets(6)%cell_list(i) == i, "Wrong cell id")
  ENDDO
ENDSUBROUTINE test_three_level_grid_implicit_hierarchy
ENDPROGRAM testXDMFFileType
