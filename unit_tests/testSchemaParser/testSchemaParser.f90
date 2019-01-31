!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE DummyEntryModule
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE Strings
USE IO_Strings
USE ParameterLists
USE SchemaParserModule

IMPLICIT NONE
PRIVATE

PUBLIC :: SCHEMA_DUMMY_ENTRY

TYPE,EXTENDS(SchemaEntry) :: SchemaEntryDummy
  CONTAINS
    PROCEDURE,PASS :: parse => parseDummy_SchEnt
ENDTYPE SchemaEntryDummy

TYPE(SchemaEntryDummy),SAVE :: SCHEMA_DUMMY_ENTRY

CONTAINS
SUBROUTINE parseDummy_SchEnt(this,entryStr,paramList,pListPathCrd)
  CLASS(SchemaEntryDummy),INTENT(IN) :: this
  CLASS(StringType),INTENT(IN) :: entryStr
  CLASS(ParamType),INTENT(INOUT) :: paramList
  TYPE(StringType),INTENT(IN) :: pListPathCrd

  TYPE(StringType) :: pListPath
  INTEGER(SIK) :: ierr,nEntries,ientry
  TYPE(StringType),ALLOCATABLE :: stra1(:)
  INTEGER(SIK),ALLOCATABLE :: sika1(:)

  !Add to path
  pListPath=pListPathCrd
  CALL this%addPLPath(pListPath)

  !Parse string into Dummy Type      
  REQUIRE(MOD(nFields(entryStr),2)==0)
  nEntries=nFields(entryStr)/2_SIK
  ALLOCATE(stra1(nEntries)); ALLOCATE(sika1(nEntries))
  DO ientry=1,nEntries        
    CALL getField((ientry-1)*2+1,entryStr,stra1(ientry),ierr)
    CALL getField((ientry-1)*2+2,entryStr,sika1(ientry),ierr)
  ENDDO

  !Add to parameter list
  CALL paramList%add(TRIM(pListPath)//"->STRA1",stra1)
  CALL paramList%add(TRIM(pListPath)//"->SIKA1",sika1)
ENDSUBROUTINE parseDummy_SchEnt
ENDMODULE DummyEntryModule

PROGRAM testSchemaParser
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE SchemaParserModule
USE ParameterLists
USE FileType_Input
USE Strings

IMPLICIT NONE

INTEGER(SIK) :: valsik
REAL(SRK)    :: valsrk
LOGICAL(SBK) :: valsbk
TYPE(StringType) :: valstr
REAL(SRK),ALLOCATABLE :: valsrka1(:)
INTEGER(SIK),ALLOCATABLE :: valsika1(:)
LOGICAL(SBK),ALLOCATABLE :: valsbka1(:)
TYPE(StringType),ALLOCATABLE :: valstra1(:)
LOGICAL(SBK) :: bool

TYPE(SchemaParser) :: mySP
TYPE(ParamType) :: testParam
TYPE(InputFileType) :: testFile

CREATE_TEST("SchemaParser")

CALL testFile%initialize(UNIT=73,FILE='testFile.inp')
CALL testFile%fopen()

REGISTER_SUBTEST('SchemaParser',testSchema)

FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testSchema()
  USE DummyEntryModule

  CALL testParam%clear()

  COMPONENT_TEST('%init')
  CALL mySP%init()
  ASSERT(mySP%isInit,'isInit')

  COMPONENT_TEST('Add SIK Entry')
  CALL mySP%addBlock("SINGLE_SIK_BLOCK",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addCard("SINGLE_SIK_BLOCK","SINGLE_SIK_CARD",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addEntry("SINGLE_SIK_BLOCK","SINGLE_SIK_CARD",SCHEMA_SIK_ENTRY)

  COMPONENT_TEST('Add SRK Entry')
  CALL mySP%addBlock("SINGLE_SRK_BLOCK",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addCard("SINGLE_SRK_BLOCK","SINGLE_SRK_CARD",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addEntry("SINGLE_SRK_BLOCK","SINGLE_SRK_CARD",SCHEMA_SRK_ENTRY)

  COMPONENT_TEST('Add SBK Entry')
  CALL mySP%addBlock("SINGLE_SBK_BLOCK",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addCard("SINGLE_SBK_BLOCK","SINGLE_SBK_CARD",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addEntry("SINGLE_SBK_BLOCK","SINGLE_SBK_CARD",SCHEMA_SBK_ENTRY)

  COMPONENT_TEST('Add STR Entry')
  CALL mySP%addBlock("SINGLE_STR_BLOCK",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addCard("SINGLE_STR_BLOCK","SINGLE_STR_CARD",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addEntry("SINGLE_STR_BLOCK","SINGLE_STR_CARD",SCHEMA_STR_ENTRY)

  COMPONENT_TEST('Add unrequired Block')
  CALL mySP%addBlock("NOT_RQD_BLOCK",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_NOT_REQUIRED)

  COMPONENT_TEST('Add MultiBlock / MultCard Entry')
  CALL mySP%addBlock("MULTPL_SBK_BLOCK",SCHEMA_MULTPL_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED,'multi_sbk_blk')
  CALL mySP%addCard("MULTPL_SBK_BLOCK","MULTPL_SBK_CARD",SCHEMA_MULTPL_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED,'multi_sbk_crd')
  CALL mySP%addEntry("MULTPL_SBK_BLOCK","MULTPL_SBK_CARD",SCHEMA_SBK_ENTRY,'sbk_ent')
  CALL mySP%addCard("MULTPL_SBK_BLOCK","NOT_RQD_CARD",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_NOT_REQUIRED)

  COMPONENT_TEST('Add SIKa1 Entry')
  CALL mySP%addBlock("SINGLE_SIKA1_BLOCK",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addCard("SINGLE_SIKA1_BLOCK","SINGLE_SIKA1_CARD",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addEntry("SINGLE_SIKA1_BLOCK","SINGLE_SIKA1_CARD",SCHEMA_SIKA1_ENTRY)

  COMPONENT_TEST('Add SRKa1 Entry')
  CALL mySP%addBlock("SINGLE_SRKA1_BLOCK",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addCard("SINGLE_SRKA1_BLOCK","SINGLE_SRKA1_CARD",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addEntry("SINGLE_SRKA1_BLOCK","SINGLE_SRKA1_CARD",SCHEMA_SRKA1_ENTRY)

  COMPONENT_TEST('Add SBKa1 Entry')
  CALL mySP%addBlock("SINGLE_SBKA1_BLOCK",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addCard("SINGLE_SBKA1_BLOCK","SINGLE_SBKA1_CARD",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addEntry("SINGLE_SBKA1_BLOCK","SINGLE_SBKA1_CARD",SCHEMA_SBKA1_ENTRY)

  COMPONENT_TEST('Add STRa1 Entry')
  CALL mySP%addBlock("SINGLE_STRA1_BLOCK",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addCard("SINGLE_STRA1_BLOCK","SINGLE_STRA1_CARD",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addEntry("SINGLE_STRA1_BLOCK","SINGLE_STRA1_CARD",SCHEMA_STRA1_ENTRY)

  COMPONENT_TEST('Add GEOM / ASSEM Entry')
  CALL mySP%addBlock("GEOM",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addCard("GEOM","ASSEM",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addEntry("GEOM","ASSEM",SCHEMA_SIK_ENTRY,'PIN_X')
  CALL mySP%addEntry("GEOM","ASSEM",SCHEMA_SIK_ENTRY,'PIN_Y')
  CALL mySP%addEntry("GEOM","ASSEM",SCHEMA_SIKA1_ENTRY,'PIN_MAP')

  COMPONENT_TEST('Add User Defined Entry')
  CALL mySP%addBlock("USER_BLOCK",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addCard("USER_BLOCK","USER_CARD",SCHEMA_SINGLE_OCCURRENCE,SCHEMA_ELEMENT_REQUIRED)
  CALL mySP%addEntry("USER_BLOCK","USER_CARD",SCHEMA_DUMMY_ENTRY)



  COMPONENT_TEST('%parse')
  CALL mySP%parse(testFile,testParam)

  ASSERT(testParam%has('SINGLE_SIK_BLOCK->SINGLE_SIK_CARD'),'SIK_Entry ParamList')
  CALL testParam%get('SINGLE_SIK_BLOCK->SINGLE_SIK_CARD',valsik)
  ASSERT(valsik==5, 'SIK_Entry Value')

  ASSERT(testParam%has('SINGLE_SRK_BLOCK->SINGLE_SRK_CARD'),'SRK_Entry ParamList')
  CALL testParam%get('SINGLE_SRK_BLOCK->SINGLE_SRK_CARD',valsrk)
  ASSERT(valsrk .APPROXEQ. 3.0_SRK, 'SRK_Entry Value')

  ASSERT(testParam%has('SINGLE_SBK_BLOCK->SINGLE_SBK_CARD'),'SBK_Entry ParamList')
  CALL testParam%get('SINGLE_SBK_BLOCK->SINGLE_SBK_CARD',valsbk)
  ASSERT(valsbk==.TRUE., 'SBK_Entry Value')

  ASSERT(testParam%has('SINGLE_STR_BLOCK->SINGLE_STR_CARD'),'STR_Entry ParamList')
  CALL testParam%get('SINGLE_STR_BLOCK->SINGLE_STR_CARD',valstr)
  ASSERT(valstr=='string', 'STR_Entry Value')

  ASSERT(testParam%has('multi_sbk_blk_1->multi_sbk_crd_1->sbk_ent'),'Multi_Entry_1 ParamList')
  ASSERT(testParam%has('multi_sbk_blk_1->multi_sbk_crd_2->sbk_ent'),'Multi_Entry_2 ParamList')
  ASSERT(testParam%has('multi_sbk_blk_2->multi_sbk_crd_1->sbk_ent'),'Multi_Entry_3 ParamList')
  ASSERT(testParam%has('multi_sbk_blk_3->multi_sbk_crd_1->sbk_ent'),'Multi_Entry_4 ParamList')
  CALL testParam%get('multi_sbk_blk_1->multi_sbk_crd_1->sbk_ent',valsbk)
  ASSERT(valsbk==.TRUE., 'Multi_Entry_1 Value')
  CALL testParam%get('multi_sbk_blk_1->multi_sbk_crd_2->sbk_ent',valsbk)
  ASSERT(valsbk==.TRUE., 'Multi_Entry_2 Value')
  CALL testParam%get('multi_sbk_blk_2->multi_sbk_crd_1->sbk_ent',valsbk)
  ASSERT(valsbk==.FALSE., 'Multi_Entry_3 Value')
  CALL testParam%get('multi_sbk_blk_3->multi_sbk_crd_1->sbk_ent',valsbk)
  ASSERT(valsbk==.FALSE., 'Multi_Entry_4 Value')

  ASSERT(testParam%has('SINGLE_SIKA1_BLOCK->SINGLE_SIKA1_CARD'),'SIKA1_Entry ParamList')
  CALL testParam%get('SINGLE_SIKA1_BLOCK->SINGLE_SIKA1_CARD',valsika1)
  ASSERT(ALL(valsika1==(/1,2,3,4,5/)), 'SIKA1_Entry Value')

  ASSERT(testParam%has('SINGLE_SRKA1_BLOCK->SINGLE_SRKA1_CARD'),'SRKA1_Entry ParamList')
  CALL testParam%get('SINGLE_SRKA1_BLOCK->SINGLE_SRKA1_CARD',valsrka1)
  ASSERT(ALL(valsrka1 .APPROXEQ. (/1.0_SRK,2.0_SRK,3.0_SRK,4.0_SRK/)), 'SRKA1_Entry Value')

  ASSERT(testParam%has('SINGLE_SBKA1_BLOCK->SINGLE_SBKA1_CARD'),'SBKA1_Entry ParamList')
  CALL testParam%get('SINGLE_SBKA1_BLOCK->SINGLE_SBKA1_CARD',valsbka1)
  ASSERT(ALL(valsbka1==(/.TRUE.,.FALSE.,.TRUE.,.TRUE./)), 'SBKA1_Entry Value')

  ASSERT(testParam%has('SINGLE_STRA1_BLOCK->SINGLE_STRA1_CARD'),'STRA1_Entry ParamList')
  CALL testParam%get('SINGLE_STRA1_BLOCK->SINGLE_STRA1_CARD',valstra1)
  ASSERT(ALL(valstra1==(/'Word1','wOrD2','WORD3'/)), 'STRA1_Entry Value')

  ASSERT(testParam%has('GEOM->ASSEM->PIN_X'),'GEOM / ASSEM Entry PIN_X ParamList')
  ASSERT(testParam%has('GEOM->ASSEM->PIN_Y'),'GEOM / ASSEM Entry PIN_Y ParamList')
  ASSERT(testParam%has('GEOM->ASSEM->PIN_MAP'),'GEOM / ASSEM Entry PIN_MAP ParamList')
  CALL testParam%get('GEOM->ASSEM->PIN_X',valsik)
  ASSERT(valsik==4, 'GEOM / ASSEM Entry PIN_X Value')
  CALL testParam%get('GEOM->ASSEM->PIN_Y',valsik)
  ASSERT(valsik==4, 'GEOM / ASSEM Entry PIN_Y Value')
  CALL testParam%get('GEOM->ASSEM->PIN_Y',valsik)
  CALL testParam%get('GEOM->ASSEM->PIN_MAP',valsika1)
  bool=ALL(valsika1==(/1,2,3,4, &
                       5,6,7,8, &
                       8,7,6,5, &
                       4,3,2,1/))
  ASSERT(bool, 'GEOM / ASSEM Entry PIN_MAP Value')

  ASSERT(testParam%has('USER_BLOCK->USER_CARD'),'User Defined Entry ParamList')
  CALL testParam%get('USER_BLOCK->USER_CARD->SIKA1',valsika1)
  ASSERT(ALL(valsika1==(/20,31,8/)), 'User Defined Entry SIK Value')
  ASSERT(testParam%has('USER_BLOCK->USER_CARD'),'User Defined Entry ParamList')
  CALL testParam%get('USER_BLOCK->USER_CARD->STRA1',valstra1)
  ASSERT(ALL(valstra1==(/'str1','str2','str3'/)), 'User Defined Entry STR Value')

  COMPONENT_TEST('%clear')
  CALL mySP%clear()
  ASSERT(.NOT. mySP%isInit,'isInit')
ENDSUBROUTINE testSchema
!
ENDPROGRAM testSchemaParser

