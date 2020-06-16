!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module for parsing elements of an input text file into
!>        a parameter list based on a provided schema
!>
!> This package provides a parser which will read an input text file and
!> extract relevant information from said file based on a provided
!> schema and place it into a target parameterlist.  The parser will
!> also perform some light-weight error checking based on limits
!> specifications also provided in the schema.  This error checking will
!> be limited to what may be accomplished using the information
!> available from a single keyword-value pair.
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE SchemaParserModule
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE Strings
USE IO_Strings
USE ExceptionHandler
USE ParameterLists
USE FileType_Fortran
USE FileType_Input

IMPLICIT NONE
PRIVATE
!
! List of public members
PUBLIC :: eSchemaParser
PUBLIC :: SchemaParser
PUBLIC :: SchemaEntry
PUBLIC :: SCHEMA_SINGLE_OCCURRENCE
PUBLIC :: SCHEMA_MULTPL_OCCURRENCE
PUBLIC :: SCHEMA_ELEMENT_REQUIRED
PUBLIC :: SCHEMA_ELEMENT_NOT_REQUIRED
PUBLIC :: SCHEMA_SIK_ENTRY
PUBLIC :: SCHEMA_SRK_ENTRY
PUBLIC :: SCHEMA_SBK_ENTRY
PUBLIC :: SCHEMA_STR_ENTRY
PUBLIC :: SCHEMA_SIKA1_ENTRY
PUBLIC :: SCHEMA_SRKA1_ENTRY
PUBLIC :: SCHEMA_SBKA1_ENTRY
PUBLIC :: SCHEMA_STRA1_ENTRY

!> Enumerations for the BLOCK TYPES
INTEGER(SIK),PARAMETER :: SCHEMA_SINGLE_OCCURRENCE=1
INTEGER(SIK),PARAMETER :: SCHEMA_MULTPL_OCCURRENCE=2

!> Aliasing of Element requirement
LOGICAL(SBK),PARAMETER :: SCHEMA_ELEMENT_REQUIRED=.TRUE.
LOGICAL(SBK),PARAMETER :: SCHEMA_ELEMENT_NOT_REQUIRED=.FALSE.

!> Enumeration for undefined status
INTEGER(SIK),PARAMETER :: UNDEFINED_OCCURRENCE_TYPE=-1

!> Enumeration for undefined element
INTEGER(SIK),PARAMETER :: UNDEFINED_ELEMENT=-1

!> Name of module
CHARACTER(LEN=*),PARAMETER :: modName='SCHEMAPARSER'

!> Max allowed lengths for text file reading
INTEGER(SIK),PARAMETER :: MAX_ELEMENT_NAME_LEN = 20
INTEGER(SIK),PARAMETER :: MAX_LINE_LEN = 5000

!> Base Entry Type
TYPE,ABSTRACT :: SchemaEntry
  !> the Parameter List to write the entry data to
  TYPE(StringType),PRIVATE :: pListPath
  !
  !List of type bound procedures
  CONTAINS
    !> @copybrief SchemaCard::addPLPath_SchEnt
    !> @copydetails SchemaCard::addPLPath_SchEnt
    PROCEDURE,PASS :: addPLPath => addPLPath_SchEnt
    !> @copybrief SchemaEntry::parse_SchEnt_absintfc
    !> @copydetails SchemaEntry::parse_SchEnt_absintfc
    PROCEDURE(parse_SchEnt_absintfc),DEFERRED,PASS :: parse
ENDTYPE SchemaEntry

!> Type that is a container so as to have an array of pointers of
!> Entry types
TYPE :: SchemaEntryPtrArry
  !> Polymorphic pointer array of assemblies
  CLASS(SchemaEntry),POINTER :: entryPtr => NULL()
ENDTYPE SchemaEntryPtrArry

!> SIK Entry Type
TYPE,EXTENDS(SchemaEntry) :: SchemaEntrySIK
  !List of type bound procedures
  CONTAINS
    !> @copybrief SchemaEntry::parseSIK_SchEnt
    !> @copydetails SchemaEntry::parseSIK_SchEnt
    PROCEDURE,PASS :: parse => parseSIK_SchEnt
ENDTYPE SchemaEntrySIK

!> SRK Entry Type
TYPE,EXTENDS(SchemaEntry) :: SchemaEntrySRK
  !List of type bound procedures
  CONTAINS
    !> @copybrief SchemaEntry::parseSRK_SchEnt
    !> @copydetails SchemaEntry::parseSRK_SchEnt
    PROCEDURE,PASS :: parse => parseSRK_SchEnt
ENDTYPE SchemaEntrySRK

!> SBK Entry Type
TYPE,EXTENDS(SchemaEntry) :: SchemaEntrySBK
  !List of type bound procedures
  CONTAINS
    !> @copybrief SchemaEntry::parseSBK_SchEnt
    !> @copydetails SchemaEntry::parseSBK_SchEnt
    PROCEDURE,PASS :: parse => parseSBK_SchEnt
ENDTYPE SchemaEntrySBK

!> STR Entry Type
TYPE,EXTENDS(SchemaEntry) :: SchemaEntrySTR
  !List of type bound procedures
  CONTAINS
    !> @copybrief SchemaEntry::parseSTR_SchEnt
    !> @copydetails SchemaEntry::parseSTR_SchEnt
    PROCEDURE,PASS :: parse => parseSTR_SchEnt
ENDTYPE SchemaEntrySTR

!> SIKa1 Entry Type
TYPE,EXTENDS(SchemaEntry) :: SchemaEntrySIKa1
  !List of type bound procedures
  CONTAINS
    !> @copybrief SchemaEntry::parseSIKa1_SchEnt
    !> @copydetails SchemaEntry::parseSIKa1_SchEnt
    PROCEDURE,PASS :: parse => parseSIKa1_SchEnt
ENDTYPE SchemaEntrySIKa1

!> SRKa1 Entry Type
TYPE,EXTENDS(SchemaEntry) :: SchemaEntrySRKa1
  !List of type bound procedures
  CONTAINS
    !> @copybrief SchemaEntry::parseSRKa1_SchEnt
    !> @copydetails SchemaEntry::parseSRKa1_SchEnt
    PROCEDURE,PASS :: parse => parseSRKa1_SchEnt
ENDTYPE SchemaEntrySRKa1

!> SBKa1 Entry Type
TYPE,EXTENDS(SchemaEntry) :: SchemaEntrySBKa1
  !List of type bound procedures
  CONTAINS
    !> @copybrief SchemaEntry::parseSBKa1_SchEnt
    !> @copydetails SchemaEntry::parseSBKa1_SchEnt
    PROCEDURE,PASS :: parse => parseSBKa1_SchEnt
ENDTYPE SchemaEntrySBKa1

!> STRa1 Entry Type
TYPE,EXTENDS(SchemaEntry) :: SchemaEntrySTRa1
  !List of type bound procedures
  CONTAINS
    !> @copybrief SchemaEntry::parseSTRa1_SchEnt
    !> @copydetails SchemaEntry::parseSTRa1_SchEnt
    PROCEDURE,PASS :: parse => parseSTRa1_SchEnt
ENDTYPE SchemaEntrySTRa1

!> Base entry types which are supported by schema parser
!> These are used when calling %addEntry to designate
!> which entry to instantiate.  Though Public and mutable
!> they should ONLY BE USED FOR ADDING ENTRIES!
TYPE(SchemaEntrySIK),SAVE :: SCHEMA_SIK_ENTRY
TYPE(SchemaEntrySRK),SAVE :: SCHEMA_SRK_ENTRY
TYPE(SchemaEntrySBK),SAVE :: SCHEMA_SBK_ENTRY
TYPE(SchemaEntrySTR),SAVE :: SCHEMA_STR_ENTRY
TYPE(SchemaEntrySIKa1),SAVE :: SCHEMA_SIKA1_ENTRY
TYPE(SchemaEntrySRKa1),SAVE :: SCHEMA_SRKA1_ENTRY
TYPE(SchemaEntrySBKa1),SAVE :: SCHEMA_SBKA1_ENTRY
TYPE(SchemaEntrySTRa1),SAVE :: SCHEMA_STRA1_ENTRY

!> Type that defines a Element of a schema
TYPE :: SchemaElement
  !> element name (as it should appear in the input file)
  TYPE(StringType),PRIVATE :: name
  !> the Parameter List to write the element data to
  TYPE(StringType),PRIVATE :: pListPath
  !> Occurence Type
  INTEGER(SIK),PRIVATE :: occurrenceType=UNDEFINED_OCCURRENCE_TYPE
  !> Whether or not the element is required
  LOGICAL(SBK),PRIVATE :: isRequired=.FALSE.
  !> Number of Elment Occurrences when reading input
  INTEGER(SIK),PRIVATE :: nOccurrences=0
  !> Starting lines for each Element Occurrence
  INTEGER(SIK),ALLOCATABLE,PRIVATE :: firstLine(:)
  !> Stopping lines for each Element Occurrence
  INTEGER(SIK),ALLOCATABLE,PRIVATE :: lastLine(:)
  !> Starting field of the element within the starting line
  INTEGER(SIK),ALLOCATABLE,PRIVATE :: firstField(:)
  !> Stopping field of the element within the stopping line
  INTEGER(SIK),ALLOCATABLE,PRIVATE :: lastField(:)
  !
  !List of type bound procedures
  CONTAINS
    !> @copybrief SchemaElement::init_SchElm
    !> @copydetails SchemaElement::init_SchElm
    PROCEDURE,PASS :: init => init_SchElm
    !> @copybrief SchemaCard::hasName_SchElm
    !> @copydetails SchemaCard::hasName_SchElm
    PROCEDURE,PASS :: hasName => hasName_SchElm
    !> @copybrief SchemaCard::countOccurrences_SchElm
    !> @copydetails SchemaCard::countOccurrences_SchElm
    PROCEDURE,PASS :: countOccurrences => countOccurrences_SchElm
    !> @copybrief SchemaCard::nOccurrencesIsValid_SchElm
    !> @copydetails SchemaCard::nOccurrencesIsValid_SchElm
    PROCEDURE,PASS :: nOccurrencesIsValid => nOccurrencesIsValid_SchElm
    !> @copybrief SchemaCard::determineExtentsWithinTextFile_SchElm
    !> @copydetails SchemaCard::determineExtentsWithinTextFile_SchElm
    PROCEDURE,PASS :: determineExtentsWithinTextFile => determineExtentsWithinTextFile_SchElm
    !> @copybrief SchemaCard::addPLPath_SchElm
    !> @copydetails SchemaCard::addPLPath_SchElm
    PROCEDURE,PASS :: addPLPath => addPLPath_SchElm
ENDTYPE SchemaElement

!> Type that defines a card of a block
TYPE,EXTENDS(SchemaElement) :: SchemaCard
  !> The cards defining this block
  TYPE(SchemaEntryPtrArry),ALLOCATABLE,PRIVATE :: entry(:)
  !
  !List of type bound procedures
  CONTAINS
    !> @copybrief SchemaCard::addEntry_SchCrd
    !> @copydetails SchemaCard::addEntry_SchCrd
    PROCEDURE,PASS :: addEntry => addEntry_SchCrd
    !> @copybrief SchemaCard::clear_SchCrd
    !> @copydetails SchemaCard::clear_SchCrd
    PROCEDURE,PASS :: clear => clear_SchCrd
    !> @copybrief SchemaCard::parse_SchCrd
    !> @copydetails SchemaCard::parse_SchCrd
    PROCEDURE,PASS :: parse => parse_SchCrd
ENDTYPE SchemaCard

!> Type that defines a block of a schema
TYPE,EXTENDS(SchemaElement) :: SchemaBlock
  !> The cards defining this block
  TYPE(SchemaCard),ALLOCATABLE,PRIVATE :: card(:)
  !
  !List of type bound procedures
  CONTAINS
    !> @copybrief SchemaBlock::addCard_SchBlk
    !> @copydetails SchemaBlock::addCard_SchBlk
    PROCEDURE,PASS :: addCard => addCard_SchBlk
    !> @copybrief SchemaBlock::addEntry_SchBlk
    !> @copydetails SchemaBlock::addEntry_SchBlk
    PROCEDURE,PASS :: addEntry => addEntry_SchBlk
    !> @copybrief SchemaBlock::clear_SchBlk
    !> @copydetails SchemaBlock::clear_SchBlk
    PROCEDURE,PASS :: clear => clear_SchBlk
    !> @copybrief SchemaBlock::parse_SchBlk
    !> @copydetails SchemaBlock::parse_SchBlk
    PROCEDURE,PASS :: parse => parse_SchBlk

ENDTYPE SchemaBlock

!> Type that contains a schema described by blocks and cards and uses
!> this schema to parse a given input into a parameter list
TYPE :: SchemaParser
  !> Initialization status
  LOGICAL(SBK) :: isInit=.FALSE.
  !> The blocks defining this schema
  TYPE(SchemaBlock),ALLOCATABLE,PRIVATE :: block(:)
  !
  !List of type bound procedures
  CONTAINS
    !>TODO:We could also add an init from parameter_list or external file type
    !> @copybrief SchemaParser::init_SchPar
    !> @copydetails SchemaParser::init_SchPar
    PROCEDURE,PASS :: init => init_SchPar
    !> @copybrief SchemaParser::clear_SchPar
    !> @copydetails SchemaParser::clear_SchPar
    PROCEDURE,PASS :: clear => clear_SchPar
    !> @copybrief SchemaParser::addBlock_SchPar
    !> @copydetails SchemaParser::addBlock_SchPar
    PROCEDURE,PASS :: addBlock => addBlock_SchPar
    !> @copybrief SchemaParser::addCard_SchPar
    !> @copydetails SchemaParser::addCard_SchPar
    PROCEDURE,PASS :: addCard => addCard_SchPar
    !> @copybrief SchemaParser::addEntry_SchPar
    !> @copydetails SchemaParser::addEntry_SchPar
    PROCEDURE,PASS :: addEntry => addEntry_SchPar
    !> @copybrief SchemaParser::parse_SchPar
    !> @copydetails SchemaParser::parse_SchPar
    PROCEDURE,PASS :: parse => parse_SchPar

ENDTYPE SchemaParser

ABSTRACT INTERFACE
  SUBROUTINE parse_SchEnt_absintfc(this,entryStr,paramList,pListPathCrd)
    IMPORT :: SchemaEntry,StringType,ParamType
    CLASS(SchemaEntry),INTENT(IN) :: this
    CLASS(StringType),INTENT(IN) :: entryStr
    CLASS(ParamType),INTENT(INOUT) :: paramList
    TYPE(StringType),INTENT(IN) :: pListPathCrd
  ENDSUBROUTINE parse_SchEnt_absintfc
ENDINTERFACE

!> Exception Handler for use in SchemaParser
TYPE(ExceptionHandlerType),SAVE :: eSchemaParser
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Constructor for the schema parser
!> @param this the variable to initialize
!>
!> The constructor for the schema parser
!>
SUBROUTINE init_SchPar(this)
  CLASS(SchemaParser),INTENT(INOUT) :: this

  ALLOCATE(this%block(0))
  this%isInit=.TRUE.
ENDSUBROUTINE init_SchPar
!
!-------------------------------------------------------------------------------
!> @brief Routine clears the data in Schema Parser type variable
!> @param this the type variable to clear
!>
SUBROUTINE clear_SchPar(this)
  CLASS(SchemaParser),INTENT(INOUT) :: this

  INTEGER(SIK) iblock,nBlocks

  REQUIRE(this%isInit)
  nBlocks=SIZE(this%block)
  DO iblock=1,nBlocks
    CALL this%block(iblock)%clear()
  ENDDO
  DEALLOCATE(this%block)
  this%isInit=.FALSE.
ENDSUBROUTINE clear_SchPar
!
!-------------------------------------------------------------------------------
!> @brief initializes this schema element
!> @param this        the element to be initialized
!> @param name        the name of the element as it will appear in the input
!> @param pListPath   the parameter list path under which the element data will
!>                    put in the outgoing parameter list
!> @param type        occurence type, either single or multi occurrence
!> @param required    whether or not the element is required
!>
SUBROUTINE init_SchElm(this,name,pListPath,type,required)
  CLASS(SchemaElement),INTENT(INOUT) :: this
  TYPE(StringType),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: pListPath
  INTEGER(SIK),INTENT(IN) :: type
  LOGICAL(SBK),INTENT(IN) :: required

  REQUIRE(LEN_TRIM(name) > 0)
  REQUIRE(LEN_TRIM(pListPath) > 0)

  ! Create the element
  this%name = TRIM(name)
  this%pListPath = TRIM(pListPath)
  this%occurrenceType = type
  this%isRequired = required
ENDSUBROUTINE init_SchElm
!
!-------------------------------------------------------------------------------
!> @brief States whether or not the element has the provided name
!> @param this  the element whose name is to be checked
!> @param name  the name
!>
FUNCTION hasName_SchElm(this,name) RESULT(hasName)
  CLASS(SchemaElement),INTENT(IN) :: this
  TYPE(StringType),INTENT(IN) :: name
  LOGICAL(SBK) :: hasName

  hasName=(TRIM(name) == TRIM(this%name))
ENDFUNCTION hasName_SchElm
!
!-------------------------------------------------------------------------------
!> @brief Determines the number of times an element occurs in a given file
!> @param this       the element whose number of occurrences is to be determined
!> @param inputfile  the input file to be read
!> @param firstLine  the first line within the file to start considering
!> @param lastLine   the last line within the file to start considering
!> @param firstField the first field within the first line to start considering
!> @param lastField  the last field within the last line to start considering
!>
SUBROUTINE countOccurrences_SchElm(this,inputFile,firstLine,lastLine,firstField,lastField)
  CLASS(SchemaElement),INTENT(INOUT) :: this
  TYPE(InputFileType),INTENT(INOUT) :: inputFile
  INTEGER(SIK),INTENT(IN),OPTIONAL :: firstLine
  INTEGER(SIK),INTENT(IN),OPTIONAL :: lastLine
  INTEGER(SIK),INTENT(IN),OPTIONAL :: firstField
  INTEGER(SIK),INTENT(IN),OPTIONAL :: lastField

  TYPE(StringType) :: line,fieldStr
  INTEGER(SIK) :: iline,ifield
  INTEGER(SIK) :: fstLine,lstLine,fstField,lstField,sttField,stpField

  !Set the bounds on the text file being considered
  fstLine=0; fstField=0; lstLine=HUGE(1_SIK); lstField=HUGE(1_SIK)
  IF(PRESENT(firstLine))   fstLine=firstLine;  IF(PRESENT(lastLine))   lstLine=lastLine
  IF(PRESENT(firstField)) fstField=firstField; IF(PRESENT(lastField)) lstField=lastField

  REQUIRE(inputFile%isOpen())
  REQUIRE(fstLine >= 0 .AND. fstLine <= lstLine)
  REQUIRE(fstField >= 0 .AND. fstField <= lstField)

  iline=0
  this%nOccurrences=0
  DO WHILE(.NOT.atEndOfFile(inputFile))
    CALL inputfile%fgetl(line)
    CALL stripComment(line)
    iline=iline+1
    IF(iline > lstLine) EXIT
    IF(iline >= fstLine) THEN
      IF(atContentLine(inputFile)) THEN
        sttField=1;             IF(iline == fstLine) sttField=MAX(fstField,sttField)
        stpField=nFields(line); IF(iline == lstLine) stpField=MIN(lstField,stpField)
        DO ifield=sttField,stpField
          CALL getField(ifield,line,fieldStr)
          IF(fieldStr == this%name) this%nOccurrences=this%nOccurrences+1
        ENDDO
      ENDIF
    ENDIF
  ENDDO
  CALL inputfile%frewind()
ENDSUBROUTINE countOccurrences_SchElm
!
!-------------------------------------------------------------------------------
!> @brief Determines the starting and stopping lines and fields for each
!>        occurrence of the element within the given text file
!> @param this          the element whose extents are to be determined and set
!> @param inputfile     the input file to be read
!> @param validElements an array of the all valid elements that are at the same
!>                      element level as this element
!> @param firstLine     the first line within the file to start considering
!> @param lastLine      the last line within the file to start considering
!> @param firstField    the first field within the first line to start
!>                      considering
!> @param lastField     the last field within the last line to start considering
!>
SUBROUTINE determineExtentsWithinTextFile_SchElm(this,inputFile,validElements,firstLine,lastLine,firstField,lastField)
  CLASS(SchemaElement),INTENT(INOUT) :: this
  TYPE(InputFileType),INTENT(INOUT) :: inputFile
  CLASS(SchemaElement),INTENT(IN) :: validElements(:)
  INTEGER(SIK),INTENT(IN),OPTIONAL :: firstLine
  INTEGER(SIK),INTENT(IN),OPTIONAL :: lastLine
  INTEGER(SIK),INTENT(IN),OPTIONAL :: firstField
  INTEGER(SIK),INTENT(IN),OPTIONAL :: lastField

  TYPE(StringType) :: line,fieldStr
  INTEGER(SIK) :: ifield,ioccur,iline
  LOGICAL(SBK) :: readingThisElement
  INTEGER(SIK) :: fstLine,lstLine,fstField,lstField,sttField,stpField

  !Set the bounds on the text file being considered
  fstLine=0; fstField=0; lstLine=HUGE(1_SIK); lstField=HUGE(1_SIK)
  IF(PRESENT(firstLine))   fstLine=firstLine;  IF(PRESENT(lastLine))   lstLine=lastLine
  IF(PRESENT(firstField)) fstField=firstField; IF(PRESENT(lastField)) lstField=lastField

  REQUIRE(inputFile%isOpen())
  REQUIRE(fstLine >= 0 .AND. fstLine <= lstLine)
  REQUIRE(fstField >= 0 .AND. fstField <= lstField)

  IF(ALLOCATED(this%firstLine))  DEALLOCATE(this%firstLine);  ALLOCATE(this%firstLine(this%nOccurrences))
  IF(ALLOCATED(this%lastLine))   DEALLOCATE(this%lastLine);   ALLOCATE(this%lastLine(this%nOccurrences))
  IF(ALLOCATED(this%firstField)) DEALLOCATE(this%firstField); ALLOCATE(this%firstField(this%nOccurrences))
  IF(ALLOCATED(this%lastField))  DEALLOCATE(this%lastField);  ALLOCATE(this%lastField(this%nOccurrences))

  ioccur=0
  iline=0
  readingThisElement=.FALSE.
  DO WHILE(.NOT.atEndOfFile(inputFile))
    CALL inputfile%fgetl(line)
    CALL stripComment(line)
    iline=iline+1
    IF(iline > lstLine) EXIT
    IF(iline >= fstLine) THEN
      IF(atContentLine(inputFile)) THEN
        sttField=1;             IF(iline == fstLine) sttField=MAX(fstField,sttField)
        stpField=nFields(line); IF(iline == lstLine) stpField=MIN(lstField,stpField)
        DO ifield=sttField,stpField
          CALL getField(ifield,line,fieldStr)
          IF(fieldStr == this%name) THEN
            ioccur=ioccur+1
            this%firstLine(ioccur)=iline
            this%lastLine(ioccur)=iline
            this%firstField(ioccur)=ifield
            this%lastField(ioccur)=ifield
            readingThisElement=.TRUE.
          ELSEIF(ANY(fieldStr == validElements(:)%name)) THEN
            readingThisElement=.FALSE.
          ENDIF
          IF(readingThisElement) this%lastField(ioccur)=ifield
        ENDDO
      ENDIF
    ENDIF
    IF(readingThisElement) this%lastLine(ioccur)=iline
  ENDDO
  CALL inputfile%frewind()
ENDSUBROUTINE determineExtentsWithinTextFile_SchElm
!
!-------------------------------------------------------------------------------
!> @brief Checks to see if a provided integer (i.e. the element occurrence
!>        count) represents a valid occurrence count for this element and throws
!>        an error if not
!> @param this    the element whose count occurrence to check
!> @param isValid a logical indicating whether or not the occurrence count is
!>                valid
!>
FUNCTION nOccurrencesIsValid_SchElm(this) RESULT(isValid)
  CHARACTER(LEN=*),PARAMETER :: myName='nOccurrencesIsValid_SchElm'
  CLASS(SchemaElement),INTENT(IN) :: this
  LOGICAL(SBK) :: isValid

  isValid=.TRUE.
  IF(this%nOccurrences == 0 .AND. this%isRequired) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName//' - "'// TRIM(this%name)//' Not Defined!')
    isValid=.FALSE.
  ELSEIF(this%nOccurrences > 1 .AND. this%occurrenceType == SCHEMA_SINGLE_OCCURRENCE) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName//' - "'// TRIM(this%name)//' Defined more than once!')
    isValid=.FALSE.
  ENDIF
ENDFUNCTION nOccurrencesIsValid_SchElm
!
!-------------------------------------------------------------------------------
!> @brief Appends the given pListPath with the element path
!> @param this      the element whose pListPath will be appended to the input
!>                  pListPath
!> @param pListPath the pListPath to append the element pListPath to
!> @param ioccur    the occurrence number to be included in the pListPath
!>                  if this element type is a multiple occurrence type
!>
SUBROUTINE addPLPath_SchElm(this,pListPath,ioccur)
  CLASS(SchemaElement),INTENT(IN) :: this
  TYPE(StringType),INTENT(INOUT) ::pListPath
  INTEGER(SIK),INTENT(IN) :: ioccur

  REQUIRE(ioccur > 0)
  REQUIRE(ioccur <= this%nOccurrences)

  IF(this%occurrenceType == SCHEMA_SINGLE_OCCURRENCE) THEN
    pListPath=pListPath//this%pListPath//'->'
  ELSE
    pListPath=pListPath//this%pListPath//'_'//str(ioccur)//'->'
  ENDIF
ENDSUBROUTINE addPLPath_SchElm
!
!-------------------------------------------------------------------------------
!> @brief Adds an empty block to the schema
!> @param this        the schema parser to add the block to
!> @param blockName   the name of the block as it will appear in the input
!> @param type        occurence type, either single or multi occurrence
!> @param required    whether or not the block is required.  Clients should use
!>                    SCHEMA_ELEMENT_REQUIRED and SCHEMA_ELEMENT_NOT_REQUIRED
!>                    when populating this value
!> @param pListPath   the parameter list path under which the block data will
!>                    put in the outgoing parameter list
!>
SUBROUTINE addBlock_SchPar(this,blockName,type,required,pListPath)
  CLASS(SchemaParser),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: blockName
  INTEGER(SIK),INTENT(IN) :: type
  LOGICAL(SBK),INTENT(IN) :: required
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: pListPath

  TYPE(StringType) :: blockNameStr,pListPathStr
  TYPE(SchemaBlock) :: block
  TYPE(SchemaBlock),ALLOCATABLE :: prevblocks(:)
  INTEGER(SIK) :: iblock,nBlocks

  REQUIRE(this%isInit)

  ! Initialize the block
  blockNameStr=TRIM(blockName)
  pListPathStr=TRIM(blockName)
  IF(PRESENT(pListPath)) pListPathStr=TRIM(pListPath)
  CALL block%init(blockNameStr,pListPathStr,type,required)
  ALLOCATE(block%card(0))

  ! Append this block to the list of blocks
  nBlocks=SIZE(this%block)
  CALL MOVE_ALLOC(this%block,prevblocks)
  ALLOCATE(this%block(nBlocks+1))
  DO iblock=1,nBlocks
    this%block(iblock)=prevblocks(iblock)
  ENDDO
  this%block(nBlocks+1)=block
ENDSUBROUTINE addBlock_SchPar
!
!-------------------------------------------------------------------------------
!> @brief Adds an empty card to the schema under the specified block
!> @param this        the schema parser to add the card to
!> @param blockName   the name of the block to add the card to
!> @param cardName    the name of the card as it will appear in the input
!> @param type        occurence type, either single or multi occurrence
!> @param required    whether or not the card is required.  Clients should use
!>                    SCHEMA_ELEMENT_REQUIRED and SCHEMA_ELEMENT_NOT_REQUIRED
!>                    when populating this value
!> @param pListPath   the parameter list path under which the card data will be
!>                    put in the outgoing parameter list
!>
SUBROUTINE addCard_SchPar(this,blockName,cardName,type,required,pListPath)
  CLASS(SchemaParser),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: blockName
  CHARACTER(LEN=*),INTENT(IN) :: cardName
  INTEGER(SIK),INTENT(IN) :: type
  LOGICAL(SBK),INTENT(IN) :: required
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: pListPath

  TYPE(StringType) :: cardNameStr,blockNameStr,pListPathStr
  INTEGER(SIK) :: iblock

  REQUIRE(this%isInit)

  !Add the card to the block
  blockNameStr=TRIM(blockName)
  cardNameStr=TRIM(cardName)
  pListPathStr=TRIM(cardName)
  iblock=findElementByName(this%block,blockNameStr)
  REQUIRE(iblock /= UNDEFINED_ELEMENT)
  IF(PRESENT(pListPath)) pListPathStr=TRIM(pListPath)
  CALL this%block(iblock)%addCard(cardNameStr,pListPathStr,type,required)

ENDSUBROUTINE addCard_SchPar
!
!-------------------------------------------------------------------------------
!> @brief Adds an entry to the schema under the specified block and card
!> @param this        the schema parser to add the entry to
!> @param blockName   the name of the block to add the entry to
!> @param cardName    the name of the card to add the entry to
!> @param type        a defined entry type to be used for parsing
!> @param pListPath   the parameter list path under which the entry data will be
!>                    put in the outgoing parameter list
!>
SUBROUTINE addEntry_SchPar(this,blockName,cardName,type,pListPath)
  CLASS(SchemaParser),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: blockName
  CHARACTER(LEN=*),INTENT(IN) :: cardName
  CLASS(SchemaEntry),INTENT(IN) :: type
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: pListPath

  TYPE(StringType) :: cardNameStr,blockNameStr,pListPathStr
  INTEGER(SIK) :: iblock

  REQUIRE(this%isInit)

  !Add the entry to the block
  blockNameStr=TRIM(blockName)
  cardNameStr=TRIM(cardName)
  iblock=findElementByName(this%block,blockNameStr)
  REQUIRE(iblock /= UNDEFINED_ELEMENT)
  IF(PRESENT(pListPath)) THEN
    pListPathStr=TRIM(pListPath)
  ELSE
    pListPathStr=''
  ENDIF
  CALL this%block(iblock)%addEntry(cardNameStr,pListPathStr,type)

ENDSUBROUTINE addEntry_SchPar
!
!-------------------------------------------------------------------------------
!> @brief Parsing routine for the given input file and schema
!> @param this       the schema parser to perform parsing
!> @param inputFile  the input file to be parsed
!> @param paramList  the target parameter list in which to put the parsed data
!>
SUBROUTINE parse_SchPar(this,inputFile,paramList)
  CHARACTER(LEN=*),PARAMETER :: myName='parse_SchPar'
  CLASS(SchemaParser),INTENT(INOUT) :: this
  TYPE(InputFileType),INTENT(INOUT) :: inputFile
  TYPE(ParamType),INTENT(INOUT) :: paramList

  TYPE(StringType) line
  INTEGER(SIK) :: iblock,ioccur


  REQUIRE(this%isInit)
  REQUIRE(inputFile%isOpen())

  !Ensure no line in the input file exceeds the max line limit
  DO WHILE(.NOT.atEndOfFile(inputFile))
    CALL inputfile%fgetl(line)
    CALL stripComment(line)
    IF(atContentLine(inputFile)) THEN
      IF(LEN(line)>MAX_LINE_LEN) THEN
        CALL eSchemaParser%raiseError(modName//'::'//myName// &
            ' - "A content line exceeds the max line limit of '//str(MAX_LINE_LEN)//' characters')
        RETURN
      ENDIF
    ENDIF
  ENDDO
  CALL inputfile%frewind()

  !Parse each block
  DO iblock=1,SIZE(this%block)
    CALL this%block(iblock)%countOccurrences(inputFile)
    IF(.NOT.this%block(iblock)%nOccurrencesIsValid()) RETURN
    CALL this%block(iblock)%determineExtentsWithinTextFile(inputFile,this%block)
    DO ioccur=1,this%block(iblock)%nOccurrences
      CALL this%block(iblock)%parse(inputFile,paramList,ioccur)
    ENDDO
  ENDDO
ENDSUBROUTINE parse_SchPar
!
!-------------------------------------------------------------------------------
!> @brief Adds an empty card to the block
!> @param this        the block to add the card to
!> @param name        the name of the card as it will appear in the input
!> @param pListPath  the parameter list path under which the card data will
!>                    put in the outgoing parameter list
!> @param type        occurence type, either single or multi occurrence
!> @param required    whether or not the card is required`
!>
SUBROUTINE addCard_SchBlk(this,name,pListPath,type,required)
  CLASS(SchemaBlock),INTENT(INOUT) :: this
  TYPE(StringType),INTENT(IN) :: name
  TYPE(StringType),INTENT(IN) :: pListPath
  INTEGER(SIK),INTENT(IN) :: type
  LOGICAL(SBK),INTENT(IN) :: required


  TYPE(SchemaCard) :: card
  TYPE(SchemaCard),ALLOCATABLE :: prevcards(:)
  INTEGER(SIK) :: icard,nCards

  ! Initialize the card
  CALL card%init(name,pListPath,type,required)
  ALLOCATE(card%entry(0))

  ! Append this card to the list of cards
  nCards=SIZE(this%card)
  CALL MOVE_ALLOC(this%card,prevcards)
  ALLOCATE(this%card(nCards+1))
  DO icard=1,nCards
    this%card(icard)=prevcards(icard)
  ENDDO
  this%card(nCards+1)=card
ENDSUBROUTINE addCard_SchBlk
!
!-------------------------------------------------------------------------------
!> @brief Adds an entry to the schema under the specified block and card
!> @param this        the schema parser to add the entry to
!> @param cardName    the name of the card to add the entry to
!> @param pListPath   the parameter list path under which the entry data will be
!>                    put in the outgoing parameter list
!> @param type        a defined entry type used for parsing
!>
SUBROUTINE addEntry_SchBlk(this,cardName,pListPath,type)
  CLASS(SchemaBlock),INTENT(INOUT) :: this
  TYPE(StringType),INTENT(IN) :: cardName
  TYPE(StringType),INTENT(IN) :: pListPath
  CLASS(SchemaEntry),INTENT(IN) :: type

  INTEGER(SIK) :: icard

  !Add the entry to the card
  icard=findElementByName(this%card,cardName)
  REQUIRE(icard /= UNDEFINED_ELEMENT)
  CALL this%card(icard)%addEntry(pListPath,type)

ENDSUBROUTINE addEntry_SchBlk
!
!-------------------------------------------------------------------------------
!> @brief Routine clears the data in Schema Block type variable
!> @param this the type variable to clear
!>
SUBROUTINE clear_SchBlk(this)
  CLASS(SchemaBlock),INTENT(INOUT) :: this

  INTEGER(SIK) icard,nCards

  nCards=SIZE(this%card)
  DO icard=1,nCards
    CALL this%card(icard)%clear()
  ENDDO
  DEALLOCATE(this%card)
ENDSUBROUTINE clear_SchBlk
!
!-------------------------------------------------------------------------------
!> @brief Parsing routine for the given input file and block
!> @param this       the block to perform parsing
!> @param inputFile  the input file to be parsed
!> @param paramList  the target parameter list in which to put the parsed data
!> @param ioccurBlk  an integer representing the block occurrence being
!>                   considered
!>
SUBROUTINE parse_SchBlk(this,inputFile,paramList,ioccurBlk)
  CHARACTER(LEN=*),PARAMETER :: myName='parse_SchBlk'
  CLASS(SchemaBlock),INTENT(INOUT) :: this
  TYPE(InputFileType),INTENT(INOUT) :: inputFile
  TYPE(ParamType),INTENT(INOUT) :: paramList
  INTEGER(SIK),INTENT(IN) :: ioccurBlk

  INTEGER(SIK) :: nerr
  INTEGER(SIK) :: icard,ioccur,firstLine,lastLine,firstField,lastField
  TYPE(StringType) :: pListPath

  nerr=eSchemaParser%getCounter(EXCEPTION_ERROR)

  firstLine=this%firstLine(ioccurBlk); lastLine=this%lastLine(ioccurBlk);
  firstField=this%firstField(ioccurBlk); lastField=this%lastField(ioccurBlk);
  pListPath=''
  CALL this%addPLPath(pListPath,ioccurBlk)

  !Parse each Card
  DO icard=1,SIZE(this%card)
    CALL this%card(icard)%countOccurrences(inputFile,firstLine,lastLine,firstField,lastField)
    IF(.NOT.this%card(icard)%nOccurrencesIsValid()) RETURN
    CALL this%card(icard)%determineExtentsWithinTextFile(inputFile,this%card,firstLine,lastLine,firstField,lastField)
    DO ioccur=1,this%card(icard)%nOccurrences
      CALL this%card(icard)%parse(inputFile,paramList,ioccur,pListPath)
    ENDDO
  ENDDO

  IF(eSchemaParser%getCounter(EXCEPTION_ERROR) > nerr) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName//' - Error parsing block '//TRIM(this%name)//' Instance '//str(ioccurBlk)//'!')
  ENDIF
ENDSUBROUTINE parse_SchBlk
!
!-------------------------------------------------------------------------------
!> @brief Appends the entry to the list of entries owned by the card
!> @param this    the card to add the entry to
!> @param entry   the pointer to the entry to be added
!> @param type    a defined entry type used for parsing
!>
SUBROUTINE addEntry_SchCrd(this,pListPath,type)
  CLASS(SchemaCard),INTENT(INOUT) :: this
  TYPE(StringType),INTENT(IN) :: pListPath
  CLASS(SchemaEntry),INTENT(IN) :: type

  TYPE(SchemaEntryPtrArry) :: entry

  TYPE(SchemaEntryPtrArry),ALLOCATABLE :: preventries(:)
  INTEGER(SIK) :: ientry,nEntries

  ALLOCATE(entry%entryPtr, SOURCE=type)
  entry%entryPtr%pListPath=pListPath

  ! Append this card to the list of cards
  nEntries=SIZE(this%entry)
  CALL MOVE_ALLOC(this%entry,preventries)
  ALLOCATE(this%entry(nEntries+1))
  DO ientry=1,nEntries
    this%entry(ientry)=preventries(ientry)
  ENDDO
  this%entry(nEntries+1)=entry
ENDSUBROUTINE addEntry_SchCrd
!
!-------------------------------------------------------------------------------
!> @brief Parsing routine for the given input file and card
!> @param this         the card to perform parsing
!> @param inputFile    the input file to be parsed
!> @param paramList    the target parameter list in which to put the parsed data
!> @param ioccurCrd    an integer representing the card occurrence being
!>                     considered
!> @param pListPathBlk the block parameter list path
!>
SUBROUTINE parse_SchCrd(this,inputFile,paramList,ioccurCrd,pListPathBlk)
  CHARACTER(LEN=*),PARAMETER :: myName='parse_SchCrd'
  CLASS(SchemaCard),INTENT(INOUT) :: this
  TYPE(InputFileType),INTENT(INOUT) :: inputFile
  TYPE(ParamType),INTENT(INOUT) :: paramList
  INTEGER(SIK),INTENT(IN) :: ioccurCrd
  TYPE(StringType),INTENT(IN) :: pListPathBlk

  INTEGER(SIK) :: nerr
  INTEGER(SIK) :: fstLine,lstLine,fstField,lstField,sttField,stpField
  TYPE(StringType) :: pListPath,line,fieldStr
  INTEGER(SIK) :: ientry,nEntries,iline,ifield
  TYPE(StringType),ALLOCATABLE :: entryStr(:)

  nerr=eSchemaParser%getCounter(EXCEPTION_ERROR)

  fstLine=this%firstLine(ioccurCrd); lstLine=this%lastLine(ioccurCrd);
  fstField=this%firstField(ioccurCrd); lstField=this%lastField(ioccurCrd);

  REQUIRE(inputFile%isOpen())
  REQUIRE(fstLine >= 0 .AND. fstLine <= lstLine)
  REQUIRE(fstField >= 0 .AND. fstField <= lstField)

  pListPath=pListPathBlk
  CALL this%addPLPath(pListPath,ioccurCrd)

  !Read the card entries into strings for each entry using the "/" as a delimiter
  nEntries=SIZE(this%entry)
  ALLOCATE(entryStr(nEntries))
  entryStr=''
  iline=0
  ientry=1
  DO WHILE(.NOT.atEndOfFile(inputFile))
    CALL inputfile%fgetl(line)
    CALL stripComment(line)
    iline=iline+1
    IF(iline > lstLine) EXIT
    IF(iline >= fstLine) THEN
      IF(atContentLine(inputFile)) THEN
        sttField=1;             IF(iline == fstLine) sttField=MAX(fstField+1,sttField)
        stpField=nFields(line); IF(iline == lstLine) stpField=MIN(lstField,stpField)
        DO ifield=sttField,stpField
          CALL getField(ifield,line,fieldStr)
          IF(fieldStr == "/") THEN
            ientry=ientry+1
          ELSE
            entryStr(ientry)=entryStr(ientry)//fieldStr//' '
          ENDIF
        ENDDO
      ENDIF
    ENDIF
  ENDDO
  CALL inputfile%frewind()

  !Parse each Entry
  DO ientry=1,nEntries
    CALL this%entry(ientry)%entryPtr%parse(entryStr(ientry),paramList,pListPath)
  ENDDO

  IF(eSchemaParser%getCounter(EXCEPTION_ERROR) > nerr) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName//' - Error parsing card '//TRIM(this%name)//' Instance '//str(ioccurCrd)//'!')
  ENDIF
ENDSUBROUTINE parse_SchCrd
!
!-------------------------------------------------------------------------------
!> @brief Routine clears the data in Schema Card type variable
!> @param this the type variable to clear
!>
SUBROUTINE clear_SchCrd(this)
  CLASS(SchemaCard),INTENT(INOUT) :: this

  INTEGER(SIK) ientry,nEntries

  nEntries=SIZE(this%entry)
  DO ientry=1,nEntries
    DEALLOCATE(this%entry(ientry)%entryPtr)
    this%entry(ientry)%entryPtr => NULL()
  ENDDO
  DEALLOCATE(this%entry)
ENDSUBROUTINE clear_SchCrd
!
!-------------------------------------------------------------------------------
!> @brief Appends the give pListPath with the element path
!> @param this      the entry whose pListPath will be appended to the input
!>                  pListPath
!> @param pListPath the pListPath to append the entry pListPath to
!>
SUBROUTINE addPLPath_SchEnt(this,pListPath)
  CLASS(SchemaEntry),INTENT(IN) :: this
  TYPE(StringType),INTENT(INOUT) :: pListPath

  IF(this%pListPath == '') THEN
    pListPath=pListPath%substr(1,LEN(pListPath)-2)
  ELSE
    pListPath=pListPath//this%pListPath
  ENDIF
ENDSUBROUTINE addPLPath_SchEnt
!
!-------------------------------------------------------------------------------
!> @brief Parsing routine for the given SIK entry string
!> @param this         the entry to perform parsing
!> @param entryStr     the entry string to be parsed
!> @param paramList    the target parameter list in which to put the parsed data
!> @param pListPathCrd the card parameter list path
!>
SUBROUTINE parseSIK_SchEnt(this,entryStr,paramList,pListPathCrd)
  CHARACTER(LEN=*),PARAMETER :: myName='parseSIK_SchEnt'
  CLASS(SchemaEntrySIK),INTENT(IN) :: this
  CLASS(StringType),INTENT(IN) :: entryStr
  CLASS(ParamType),INTENT(INOUT) :: paramList
  TYPE(StringType),INTENT(IN) :: pListPathCrd

  TYPE(StringType) :: pListPath
  INTEGER(SIK) :: ierr
  INTEGER(SIK) :: entry

  !Add to path
  pListPath=pListPathCrd
  CALL this%addPLPath(pListPath)

  !Parse string into SIK
  IF(isEmptyEntry(entryStr)) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName// &
        ' - Entry string contains no entries!')
    RETURN
  ENDIF
  IF(.NOT.isScalarEntry(entryStr)) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName// &
        ' - Expected Scalar entry but provided multiple entries at "'//TRIM(entryStr)//'"!')
    RETURN
  ENDIF
  IF(.NOT.isSIKEntry(entryStr)) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName// &
        ' - Expected Integer_SIK entry but provided other data type at "'//TRIM(entryStr)//'"!')
    RETURN
  ENDIF
  CALL getField(1,entryStr,entry,ierr)

  !Add to parameter list
  CALL paramList%add(TRIM(pListPath),entry)

ENDSUBROUTINE parseSIK_SchEnt
!
!-------------------------------------------------------------------------------
!> @brief Parsing routine for the given SRK entry string
!> @param this         the entry to perform parsing
!> @param entryStr     the entry string to be parsed
!> @param paramList    the target parameter list in which to put the parsed data
!> @param pListPathCrd the card parameter list path
!>
SUBROUTINE parseSRK_SchEnt(this,entryStr,paramList,pListPathCrd)
  CHARACTER(LEN=*),PARAMETER :: myName='parseSRK_SchEnt'
  CLASS(SchemaEntrySRK),INTENT(IN) :: this
  CLASS(StringType),INTENT(IN) :: entryStr
  CLASS(ParamType),INTENT(INOUT) :: paramList
  TYPE(StringType),INTENT(IN) :: pListPathCrd

  TYPE(StringType) :: pListPath
  INTEGER(SIK) :: ierr
  REAL(SRK) :: entry

  !Add to path
  pListPath=pListPathCrd
  CALL this%addPLPath(pListPath)

  !Parse string into SRK
  IF(isEmptyEntry(entryStr)) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName// &
        ' - Entry string contains no entries!')
    RETURN
  ENDIF
  IF(.NOT.isScalarEntry(entryStr)) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName// &
        ' - Expected Scalar entry but provided multiple entries at "'//TRIM(entryStr)//'"!')
    RETURN
  ENDIF
  IF(.NOT.isSRKEntry(entryStr)) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName// &
        ' - Expected Real_SRK entry but provided other data type at "'//TRIM(entryStr)//'"!')
    RETURN
  ENDIF
  CALL getField(1,entryStr,entry,ierr)

  !Add to parameter list
  CALL paramList%add(TRIM(pListPath),entry)

ENDSUBROUTINE parseSRK_SchEnt
!
!-------------------------------------------------------------------------------
!> @brief Parsing routine for the given SBK entry string
!> @param this         the entry to perform parsing
!> @param entryStr     the entry string to be parsed
!> @param paramList    the target parameter list in which to put the parsed data
!> @param pListPathCrd the card parameter list path
!>
SUBROUTINE parseSBK_SchEnt(this,entryStr,paramList,pListPathCrd)
  CHARACTER(LEN=*),PARAMETER :: myName='parseSBK_SchEnt'
  CLASS(SchemaEntrySBK),INTENT(IN) :: this
  CLASS(StringType),INTENT(IN) :: entryStr
  CLASS(ParamType),INTENT(INOUT) :: paramList
  TYPE(StringType),INTENT(IN) :: pListPathCrd

  TYPE(StringType) :: pListPath
  INTEGER(SIK) :: ierr
  TYPE(StringType) :: entry
  LOGICAL(SBK) :: entrySBK

  !Add to path
  pListPath=pListPathCrd
  CALL this%addPLPath(pListPath)

  !Parse string into SBK
  IF(isEmptyEntry(entryStr)) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName// &
        ' - Entry string contains no entries!')
    RETURN
  ENDIF
  IF(.NOT.isScalarEntry(entryStr)) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName// &
        ' - Expected Scalar entry but provided multiple entries at "'//TRIM(entryStr)//'"!')
    RETURN
  ENDIF
  IF(.NOT.isSBKEntry(entryStr)) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName// &
        ' - Expected Logical_SBK entry but provided other data type at "'//TRIM(entryStr)//'"!')
    RETURN
  ENDIF
  CALL getField(1,entryStr,entry,ierr)
  entry = entry%upper()
  IF(entry == 'TRUE' .OR. entry == 'T') THEN
    entrySBK=.TRUE.
  ELSE
    entrySBK=.FALSE.
  ENDIF

  !Add to parameter list
  CALL paramList%add(TRIM(pListPath),entrySBK)

ENDSUBROUTINE parseSBK_SchEnt
!
!-------------------------------------------------------------------------------
!> @brief Parsing routine for the given entry string
!> @param this         the entry to perform parsing
!> @param entryStr     the entry string to be parsed
!> @param paramList    the target parameter list in which to put the parsed data
!> @param pListPathCrd the card parameter list path
!>
SUBROUTINE parseSTR_SchEnt(this,entryStr,paramList,pListPathCrd)
  CHARACTER(LEN=*),PARAMETER :: myName='parseSTR_SchEnt'
  CLASS(SchemaEntrySTR),INTENT(IN) :: this
  CLASS(StringType),INTENT(IN) :: entryStr
  CLASS(ParamType),INTENT(INOUT) :: paramList
  TYPE(StringType),INTENT(IN) :: pListPathCrd

  TYPE(StringType) :: pListPath
  INTEGER(SIK) :: ierr
  TYPE(StringType) :: entry

  !Add to path
  pListPath=pListPathCrd
  CALL this%addPLPath(pListPath)

  !Parse string into STR
  IF(isEmptyEntry(entryStr)) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName// &
        ' - Entry string contains no entries!')
    RETURN
  ENDIF
  IF(.NOT.isScalarEntry(entryStr)) THEN
    CALL eSchemaParser%raiseError(modName//'::'//myName// &
        ' - Expected Scalar entry but provided multiple entries at "'//TRIM(entryStr)//'"!')
    RETURN
  ENDIF
  CALL getField(1,entryStr,entry,ierr)

  !Add to parameter list
  CALL paramList%add(TRIM(pListPath),entry)

ENDSUBROUTINE parseSTR_SchEnt
!
!-------------------------------------------------------------------------------
!> @brief Parsing routine for the given SIKa1 entry string
!> @param this         the entry to perform parsing
!> @param entryStr     the entry string to be parsed
!> @param paramList    the target parameter list in which to put the parsed data
!> @param pListPathCrd the card parameter list path
!>
SUBROUTINE parseSIKa1_SchEnt(this,entryStr,paramList,pListPathCrd)
  CHARACTER(LEN=*),PARAMETER :: myName='parseSIKa1_SchEnt'
  CLASS(SchemaEntrySIKa1),INTENT(IN) :: this
  CLASS(StringType),INTENT(IN) :: entryStr
  CLASS(ParamType),INTENT(INOUT) :: paramList
  TYPE(StringType),INTENT(IN) :: pListPathCrd

  TYPE(StringType) :: pListPath,tmpstr
  INTEGER(SIK) :: ierr,ientry
  INTEGER(SIK),ALLOCATABLE :: entry(:)

  !Add to path
  pListPath=pListPathCrd
  CALL this%addPLPath(pListPath)

  !Parse string into SIKa1
  ALLOCATE(entry(nFields(entryStr)))
  DO ientry=1,nFields(entryStr)
    CALL getField(ientry,entryStr,tmpstr,ierr)
    IF(.NOT.isSIKEntry(tmpstr)) THEN
      CALL eSchemaParser%raiseError(modName//'::'//myName// &
          ' - Expected Integer_SIK entry but provided other data type at "'//TRIM(tmpstr)//'"!')
      RETURN
    ENDIF
    CALL getField(1,tmpstr,entry(ientry),ierr)
  ENDDO

  !Add to parameter list
  CALL paramList%add(TRIM(pListPath),entry)

ENDSUBROUTINE parseSIKa1_SchEnt
!
!-------------------------------------------------------------------------------
!> @brief Parsing routine for the given SRKa1 entry string
!> @param this         the entry to perform parsing
!> @param entryStr     the entry string to be parsed
!> @param paramList    the target parameter list in which to put the parsed data
!> @param pListPathCrd the card parameter list path
!>
SUBROUTINE parseSRKa1_SchEnt(this,entryStr,paramList,pListPathCrd)
  CHARACTER(LEN=*),PARAMETER :: myName='parseSRKa1_SchEnt'
  CLASS(SchemaEntrySRKa1),INTENT(IN) :: this
  CLASS(StringType),INTENT(IN) :: entryStr
  CLASS(ParamType),INTENT(INOUT) :: paramList
  TYPE(StringType),INTENT(IN) :: pListPathCrd

  TYPE(StringType) :: pListPath,tmpstr
  INTEGER(SIK) :: ierr,ientry
  REAL(SRK),ALLOCATABLE :: entry(:)

  !Add to path
  pListPath=pListPathCrd
  CALL this%addPLPath(pListPath)

  !Parse string into SRK
  ALLOCATE(entry(nFields(entryStr)))
  DO ientry=1,nFields(entryStr)
    CALL getField(ientry,entryStr,tmpstr,ierr)
    IF(.NOT.isSRKEntry(tmpstr)) THEN
      CALL eSchemaParser%raiseError(modName//'::'//myName// &
          ' - Expected Real_SRK entry but provided other data type at "'//TRIM(tmpstr)//'"!')
      RETURN
    ENDIF
    CALL getField(1,tmpstr,entry(ientry),ierr)
  ENDDO

  !Add to parameter list
  CALL paramList%add(TRIM(pListPath),entry)

ENDSUBROUTINE parseSRKa1_SchEnt
!
!-------------------------------------------------------------------------------
!> @brief Parsing routine for the given SBK entry string
!> @param this         the entry to perform parsing
!> @param entryStr     the entry string to be parsed
!> @param paramList    the target parameter list in which to put the parsed data
!> @param pListPathCrd the card parameter list path
!>
SUBROUTINE parseSBKa1_SchEnt(this,entryStr,paramList,pListPathCrd)
  CHARACTER(LEN=*),PARAMETER :: myName='parseSBKa1_SchEnt'
  CLASS(SchemaEntrySBKa1),INTENT(IN) :: this
  CLASS(StringType),INTENT(IN) :: entryStr
  CLASS(ParamType),INTENT(INOUT) :: paramList
  TYPE(StringType),INTENT(IN) :: pListPathCrd

  TYPE(StringType) :: pListPath
  INTEGER(SIK) :: ierr,ientry
  TYPE(StringType) :: entry
  LOGICAL(SBK),ALLOCATABLE :: entrySBK(:)

  !Add to path
  pListPath=pListPathCrd
  CALL this%addPLPath(pListPath)

  !Parse string into SBK
  ALLOCATE(entrySBK(nFields(entryStr)))
  DO ientry=1,nFields(entryStr)
    CALL getField(ientry,entryStr,entry,ierr)
    IF(.NOT.isSBKEntry(entry)) THEN
      CALL eSchemaParser%raiseError(modName//'::'//myName// &
          ' - Expected Logical_SBK entry but provided other data type at "'//TRIM(entry)//'"!')
      RETURN
    ENDIF
    entry = entry%upper()
    IF(entry == 'TRUE' .OR. entry == 'T') THEN
      entrySBK(ientry)=.TRUE.
    ELSE
      entrySBK(ientry)=.FALSE.
    ENDIF
  ENDDO

  !Add to parameter list
  CALL paramList%add(TRIM(pListPath),entrySBK)

ENDSUBROUTINE parseSBKa1_SchEnt
!
!-------------------------------------------------------------------------------
!> @brief Parsing routine for the given entry STRa1
!> @param this         the entry to perform parsing
!> @param entryStr     the entry string to be parsed
!> @param paramList    the target parameter list in which to put the parsed data
!> @param pListPathCrd the card parameter list path
!>
SUBROUTINE parseSTRa1_SchEnt(this,entryStr,paramList,pListPathCrd)
  CLASS(SchemaEntrySTRa1),INTENT(IN) :: this
  CLASS(StringType),INTENT(IN) :: entryStr
  CLASS(ParamType),INTENT(INOUT) :: paramList
  TYPE(StringType),INTENT(IN) :: pListPathCrd

  TYPE(StringType) :: pListPath
  INTEGER(SIK) :: ierr,ientry
  TYPE(StringType),ALLOCATABLE :: entry(:)

  !Add to path
  pListPath=pListPathCrd
  CALL this%addPLPath(pListPath)

  !Parse string into STRa1
  ALLOCATE(entry(nFields(entryStr)))
  DO ientry=1,nFields(entryStr)
    CALL getField(ientry,entryStr,entry(ientry),ierr)
  ENDDO

  !Add to parameter list
  CALL paramList%add(TRIM(pListPath),entry)

ENDSUBROUTINE parseSTRa1_SchEnt
!
!-------------------------------------------------------------------------------
!> @brief Finds the index which corresponds to the provided element name
!> @param elements  the element array to search for the given name
!> @param name      the element name we are looking to match in elements
!> @param index     the index number of the matching element in the array
!>
FUNCTION findElementByName(elements,name) RESULT(index)
  CLASS(SchemaElement),INTENT(IN) :: elements(:)
  TYPE(StringType),INTENT(IN) :: name
  INTEGER(SIK) :: index

  INTEGER(SIK) :: ielement

  !Ensure the provided blockName matches a block name on this schema
  index=UNDEFINED_ELEMENT
  DO ielement=1,SIZE(elements)
    IF(elements(ielement)%hasName(name)) THEN
      index=ielement
      EXIT
    ENDIF
  ENDDO
ENDFUNCTION findElementByName
!
!-------------------------------------------------------------------------------
!> @brief Determines if the input text file line is currently at End of the File
!> @param file   the file currently being read
!> @param isEOF  logical determining whether or not the file is at End of File
!>
FUNCTION atEndOfFile(file) RESULT(isEOF)
  TYPE(InputFileType),INTENT(IN) :: file
  LOGICAL(SBK) :: isEOF

  REQUIRE(file%isOpen())
  isEOF=file%isEOF() .OR. file%getProbe() == DOT
ENDFUNCTION atEndOfFile
!
!-------------------------------------------------------------------------------
!> @brief Determines if the input text file line contains readable content
!> @param file        the file currently being read
!> @param hasContent  logical determining whether or not the file has content
!>
FUNCTION atContentLine(file) RESULT(hasContent)
  TYPE(InputFileType),INTENT(IN) :: file
  LOGICAL(SBK) :: hasContent

  REQUIRE(file%isOpen())
  hasContent=file%getProbe() /= BANG .AND. file%getProbe() /= DOT
ENDFUNCTION atContentLine
!
!-------------------------------------------------------------------------------
!> @brief Checks to see if entry string is empty
!> @param string   the string to be checked
!> @param isEmpty  logical corresponding to if the entry string is empty
!>
FUNCTION isEmptyEntry(string) RESULT(isEmpty)
  TYPE(StringType),INTENT(IN) :: string
  LOGICAL(SBK) :: isEmpty

  isEmpty=.NOT.nFields(string) > 0
ENDFUNCTION isEmptyEntry
!
!-------------------------------------------------------------------------------
!> @brief Checks to ensure a string has a single field (i.e. is scalar value)
!> @param string   the string to be checked
!> @param isScalar logical corresponding to if the string is a scalar
!>
FUNCTION isScalarEntry(string) RESULT(isScalar)
  TYPE(StringType),INTENT(IN) :: string
  LOGICAL(SBK) :: isScalar

  isScalar=nFields(string) == 1
ENDFUNCTION isScalarEntry
!
!-------------------------------------------------------------------------------
!> @brief Checks to ensure an entry is a valid SIK entry
!> @param string   the string to be checked
!> @param isSIK    logical corresponding to if the string is an SIK
!>
FUNCTION isSIKEntry(string) RESULT(isSIK)
  TYPE(StringType),INTENT(IN) :: string
  LOGICAL(SBK) :: isSIK

  INTEGER(SIK) :: entry,ioerr

  REQUIRE(isScalarEntry(string))

  CALL getField(1,string,entry,ioerr)
  isSIK=ioerr == 0
ENDFUNCTION isSIKEntry
!
!-------------------------------------------------------------------------------
!> @brief Checks to ensure an entry is a valid SRK entry
!> @param string  the string to be checked
!> @param isSRK   logical corresponding to if the string is an SRK
!>
FUNCTION isSRKEntry(string) RESULT(isSRK)
  TYPE(StringType),INTENT(IN) :: string
  LOGICAL(SBK) :: isSRK

  REAL(SRK) :: entry
  INTEGER(SIK) :: ioerr

  REQUIRE(isScalarEntry(string))

  CALL getField(1,string,entry,ioerr)
  isSRK=ioerr == 0
ENDFUNCTION isSRKEntry
!
!-------------------------------------------------------------------------------
!> @brief Checks to ensure an entry is a valid SBK entry
!> @param string  the string to be checked
!> @param isSBK   logical corresponding to if the string is valid
!>
FUNCTION isSBKEntry(string) RESULT(isSBK)
  TYPE(StringType),INTENT(IN) :: string
  LOGICAL(SBK) :: isSBK

  TYPE(StringType) :: entry

  REQUIRE(isScalarEntry(string))

  CALL getField(1,string,entry)
  entry = entry%upper()
  isSBK=(entry == 'TRUE' .OR. entry == 'T' .OR. entry == 'FALSE' .OR. entry == 'F')
ENDFUNCTION isSBKEntry
!
ENDMODULE SchemaParserModule

