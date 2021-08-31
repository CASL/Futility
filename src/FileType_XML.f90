!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module defines an object for representing an XML file.
!>
!> This module reads an XML file and stores the element information as
!> character information in a doubly-linked list/tree. Outside of the
!> initialization the only other functionality is for traversing the elements.
!> This type is basically used by the parameter list type to provide an
!> alternative way of initializing a parameter list.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FileType_XML
USE ISO_FORTRAN_ENV
USE IntrType
USE Strings
USE IO_Strings
USE FileType_Base

IMPLICIT NONE
PRIVATE

PUBLIC :: XMLElementType
PUBLIC :: XMLFileType

!> Character constant for a single space
CHARACTER(LEN=1),PARAMETER :: SP=' '
!> Character constant for a carraige return
CHARACTER(LEN=1),PARAMETER :: CR=CHAR(13)
!> Character constant for a line feed
CHARACTER(LEN=1),PARAMETER :: LF=CHAR(10)
!> Character constant for a tab
CHARACTER(LEN=1),PARAMETER :: TB=CHAR(9)

!> Module local constant for indicating a bad tag
INTEGER(SIK) :: BAD_TAG=-1
!> Module local constant for indicating the start of a tag
INTEGER(SIK) :: START_TAG=1
!> Module local constant for indicating the end of a tag
INTEGER(SIK) :: END_TAG=2
!> Module local constant for indicating an empty tag
INTEGER(SIK) :: EMPTY_ELEMENT_TAG=3
!> Module local constant for indicating comment tag
INTEGER(SIK) :: COMMENT_TAG=4
!> Module local constant for indicating ??? tag
INTEGER(SIK) :: PROCESSING_INST_TAG=5
!> Module local constant for indicating a declaration tag
!> the declaration tag, is usually the first tag in the file.
INTEGER(SIK) :: DECLARATION_TAG=6

!> Derived type for an XML element
TYPE :: XMLElementType
  !> The number of attributes defined on the element
  INTEGER(SIK),PRIVATE :: nAttr=0
  !> The name of the element
  TYPE(StringType) :: name
  !> The content of the element (excluding attributes)
  TYPE(StringType) :: content
  !> The names of the attributes on this element
  TYPE(StringType),ALLOCATABLE,PRIVATE :: attr_names(:)
  !> The corresponding values of the attributes on this element
  TYPE(StringType),ALLOCATABLE,PRIVATE :: attr_values(:)
  !> The parent element of this XML element
  TYPE(XMLElementType),POINTER,PRIVATE :: parent => NULL()
  !> The child elements of this element
  TYPE(XMLElementType),POINTER,PRIVATE :: children(:) => NULL()
!
!List of type bound procedures
  CONTAINS
    !> @copybrief FileType_XML::init_XMLElementType
    !> @copydoc FileType_XML::init_XMLElementType
    PROCEDURE,PASS :: init => init_XMLElementType
    !> @copybrief FileType_XML::write_XMLElementType
    !> @copydoc FileType_XML::write_XMLElementType
    PROCEDURE,PASS :: fwrite => write_XMLElementType
    !> @copybrief FileType_XML::clear_XMLElementType
    !> @copydoc FileType_XML::clear_XMLElementType
    PROCEDURE,PASS :: clear => clear_XMLElementType
    !> @copybrief FileType_XML::isEmpty_XMLElementType
    !> @copydoc FileType_XML::isEmpty_XMLElementType
    PROCEDURE,PASS :: isEmpty => isEmpty_XMLElementType
    !> @copybrief FileType_XML::hasParent_XMLElementType
    !> @copydoc FileType_XML::hasParent_XMLElementType
    PROCEDURE,PASS :: hasParent => hasParent_XMLElementType
    !> @copybrief FileType_XML::getParent_XMLElementType
    !> @copydoc FileType_XML::getParent_XMLElementType
    PROCEDURE,PASS :: getParent => getParent_XMLElementType
    !> @copybrief FileType_XML::setParent_XMLElementType
    !> @copydoc FileType_XML::setParent_XMLElementType
    PROCEDURE,PASS :: setParent => setParent_XMLElementType
    !> @copybrief FileType_XML::hasChildren_XMLElementType
    !> @copydoc FileType_XML::hasChildren_XMLElementType
    PROCEDURE,PASS :: hasChildren => hasChildren_XMLElementType
    !> @copybrief FileType_XML::getChildren_XMLElementType
    !> @copydoc FileType_XML::getChildren_XMLElementType
    PROCEDURE,PASS :: getChildren => getChildren_XMLElementType
    !> @copybrief FileType_XML::getAttributes_XMLElementType
    !> @copydoc FileType_XML::getAttributes_XMLElementType
    PROCEDURE,PASS :: getAttributes => getAttributes_XMLElementType
    !> @copybrief FileType_XML::getAttributeValue_XMLElementType
    !> @copydoc FileType_XML::getAttributeValue_XMLElementType
    PROCEDURE,PASS :: getAttributeValue => getAttributeValue_XMLElementType
    !> @copybrief FileType_XML::getContent_XMLElementType
    !> @copydoc FileType_XML::getContent_XMLElementType
    PROCEDURE,PASS :: getContent => getContent_XMLElementType
    !> @copybrief FileType_XML::setName_XMLElementType
    !> @copydoc FileType_XML::setName_XMLElementType
    PROCEDURE,PASS :: setName => setName_XMLElementType
    !> @copybrief FileType_XML::setChildren_XMLElementType
    !> @copydoc FileType_XML::setChildren_XMLElementType
    PROCEDURE,PASS :: setChildren => setChildren_XMLElementType
    !> @copybrief FileType_XML::setAttribute_XMLElementType
    !> @copydoc FileType_XML::setAttribute_XMLElementType
    PROCEDURE,PASS :: setAttribute => setAttribute_XMLElementType
ENDTYPE XMLElementType

!> The XML File type
TYPE,EXTENDS(BaseFileType) :: XMLFileType
  !> Logical indicating if file was initialized
  LOGICAL(SBK) :: isInit=.FALSE.
  !> The unit number assigned to the file
  INTEGER(SIK) :: unitNo=-1
  !> The XML version
  REAL(SRK) :: version=1.0_SRK
  !> The XML file encoding
  CHARACTER(LEN=32) :: encoding='UTF-8'
  !>
  TYPE(StringType) :: style_sheet
  !>
  LOGICAL(SBK) :: standalone=.FALSE.
  !> The root XML element of the file
  TYPE(XMLElementType),POINTER :: root => NULL()
!
!List of type bound procedures
  CONTAINS
    !> @copybrief FileType_XML::init_XMLFileType
    !> @copydoc FileType_XML::init_XMLFileType
    PROCEDURE,PASS :: init => init_XMLFileType
    !> @copybrief FileType_XML::clear_XMLFileType
    !> @copydoc FileType_XML::clear_XMLFileType
    PROCEDURE,PASS :: clear => clear_XMLFileType
    !> @copybrief FileType_XML::fopen_XMLFileType
    !> @copydoc FileType_XML::fopen_XMLFileType
    PROCEDURE,PASS :: fopen => fopen_XMLFileType
    !> @copybrief FileType_XML::fclose_XMLFileType
    !> @copydoc FileType_XML::fclose_XMLFileType
    PROCEDURE,PASS :: fclose => fclose_XMLFileType
    !> @copybrief FileType_XML::fdelete_XMLFileType
    !> @copydoc FileType_XML::fdelete_XMLFileType
    PROCEDURE,PASS :: fdelete => fdelete_XMLFileType
    !> @copybrief FileType_XML::importFromDisk_XMLFileType
    !> @copydoc FileType_XML::importFromDisk_XMLFileType
    PROCEDURE,PASS :: importFromDisk => importFromDisk_XMLFileType
    !> @copybrief FileType_XML::exportToDisk_XMLFileType
    !> @copydoc FileType_XML::exportToDisk_XMLFileType
    PROCEDURE,PASS :: exportToDisk => exportToDisk_XMLFileType
ENDTYPE XMLFileType

CHARACTER(LEN=*),PARAMETER :: modName='FILETYPE_XML'
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes an XML element object
!> @param thisXMLE the XML element object
!> @param cachedFile a cached XML file in the form of a character array
!> @param itag the positions of all the tags in the element
!> @param lines the positions of the line endings in the file
!> @param tagBegin the index of the starting tag for the element
!> @param tagEnd the index of the end tag for the element
!>
RECURSIVE SUBROUTINE init_XMLElementType(thisXMLE,cachedFile,itag,lines,tagBegin,tagEnd)
  CLASS(XMLElementType),TARGET,INTENT(INOUT) :: thisXMLE
  CHARACTER(LEN=1),INTENT(IN) :: cachedFile(:)
  INTEGER(SIK),INTENT(IN) :: itag(:,:)
  INTEGER(SIK),INTENT(IN) :: lines(:)
  INTEGER(SIK),INTENT(IN) :: tagBegin
  INTEGER(SIK),INTENT(IN) :: tagEnd

  INTEGER(SIK) :: nChildren,ichild,ierr
  INTEGER(SIK),ALLOCATABLE :: childTags(:,:)
  TYPE(StringType) :: startTagName,endTagName,tmpStr

  IF(iTag(3,tagBegin) == EMPTY_ELEMENT_TAG) THEN
!
!Empty Element
    IF(tagEnd == tagBegin) THEN
      !Get the element Name
      CALL getTagName(cachedFile(iTag(1,tagBegin):iTag(2,tagBegin)),ierr,thisXMLE%name)

      !Process the attributes
      CALL charArrytoStr(cachedFile(iTag(1,tagBegin):iTag(2,tagBegin)), tmpStr)
      CALL processTagAttributes(CHAR(tmpStr),thisXMLE%nAttr, &
          thisXMLE%attr_names,thisXMLE%attr_values,ierr)
      IF(ierr /= 0) THEN
        !Oh No!
      ENDIF
    ELSE
      !Bad input!
    ENDIF
  ELSEIF(iTag(3,tagBegin) == START_TAG) THEN
!
!Start/End Tagged Element
    IF(tagEnd > tagBegin) THEN
      IF(iTag(3,tagEnd) == END_TAG) THEN
        !Verify matching element names
        CALL getTagName(cachedFile(iTag(1,tagBegin):iTag(2,tagBegin)),ierr,startTagName)
        CALL getTagName(cachedFile(iTag(1,tagEnd):iTag(2,tagEnd)), &
            ierr,endTagName)
        IF(startTagName == endTagName .AND. ierr == 0) THEN
          !Store the name
          thisXMLE%name=startTagName

          !Process attributes
          CALL charArrytoStr(cachedFile(iTag(1,tagBegin):iTag(2,tagBegin)),tmpStr)
          CALL processTagAttributes(CHAR(tmpStr),thisXMLE%nAttr, &
              thisXMLE%attr_names,thisXMLE%attr_values,ierr)
          IF(ierr /= 0) THEN
            !Failed to process the attributes
          ENDIF

          !Determine the number of children
          CALL DetermineNChildren(tagBegin,tagEnd,iTag,nChildren, &
              childTags,ierr)
          IF(nChildren > 0) THEN
            !Process the children if any
            ALLOCATE(thisXMLE%children(nChildren))
            DO iChild=1,nChildren
              !Find the tag begin and end for the child
              CALL thisXMLE%children(ichild)%init(cachedFile,iTag,lines, &
                  childTags(1,iChild),childTags(2,iChild))
              SELECTTYPE(xmle => thisXMLE)
              TYPE IS(XMLElementType); thisXMLE%children(ichild)%parent => xmle
              ENDSELECT
            ENDDO
          ELSE
            !Store Content
            CALL charArrytoStr(cachedFile(iTag(2,tagBegin)+1:iTag(1,tagEnd)-1),thisXMLE%content)
            thisXMLE%content=TRIM(thisXMLE%content)
          ENDIF
        ELSE
          !Mismatched tags!
        ENDIF
      ELSE
        !Not matched with an end tag!
      ENDIF
    ELSE
      !Bad input!
    ENDIF
  ELSE
    !Cannot process tag!
  ENDIF
ENDSUBROUTINE init_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief Writes an XML element object to a file
!> @param thisXMLE the XML element object
!> @param unitNo the Fortran unit number of the file
!> @param nindent the amount of indentation to use when writing the element
!>
RECURSIVE SUBROUTINE write_XMLElementType(thisXMLE,unitNo,nindent)
  CLASS(XMLElementType),INTENT(IN) :: thisXMLE
  INTEGER(SIK),INTENT(IN) :: unitNo
  INTEGER(SIK),INTENT(IN),OPTIONAL :: nindent
  CHARACTER(LEN=16) :: sint
  INTEGER(SIK) :: i,ierr,nspace
  TYPE(StringType) :: fmt,tmpTag

  IF(LEN_TRIM(thisXMLE%name) > 0) THEN
    nspace=0
    IF(PRESENT(nindent)) nspace=2*nindent
    IF(ASSOCIATED(thisXMLE%children)) THEN
      !start tag
      tmpTag='<'//thisXMLE%name
      !Add attributes
      DO i=1,thisXMLE%nAttr
        tmpTag=tmpTag//' '//thisXMLE%attr_names(i)//'="'// &
            thisXMLE%attr_values(i)//'"'
      ENDDO
      tmpTag=tmpTag//'>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unitNo,FMT=CHAR(fmt),IOSTAT=ierr) CHAR(tmpTag)

      !children
      DO i=1,SIZE(thisXMLE%children)
        CALL thisXMLE%children(i)%fwrite(unitNo,nspace/2+1)
      ENDDO

      !end tag
      tmpTag='</'//thisXMLE%name//'>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unitNo,FMT=CHAR(fmt),IOSTAT=ierr) CHAR(tmpTag)
    ELSEIF(LEN(thisXMLE%content) > 0) THEN
      !start tag
      tmpTag='<'//thisXMLE%name
      !Add attributes
      DO i=1,thisXMLE%nAttr
        tmpTag=tmpTag//' '//thisXMLE%attr_names(i)//'="'// &
            thisXMLE%attr_values(i)//'"'
      ENDDO
      tmpTag=tmpTag//'>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unitNo,FMT=CHAR(fmt),IOSTAT=ierr) CHAR(tmpTag)

      !content
      WRITE(sint,'(i16)',IOSTAT=ierr) nspace+2
      fmt='('//TRIM(ADJUSTL(sint))//'x'
      WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(thisXMLE%content)
      fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      IF(thisXMLE%content /= LF) &
          WRITE(unitNo,FMT=CHAR(fmt),IOSTAT=ierr) CHAR(thisXMLE%content)

      !endtag
      tmpTag='</'//thisXMLE%name//'>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unitNo,FMT=CHAR(fmt),IOSTAT=ierr) CHAR(tmpTag)
    ELSE
      !empty element tag
      tmpTag='<'//thisXMLE%name
      !Add attributes
      DO i=1,thisXMLE%nAttr
        tmpTag=tmpTag//' '//thisXMLE%attr_names(i)//'="'// &
            thisXMLE%attr_values(i)//'"'
      ENDDO
      tmpTag=tmpTag//'/>'
      IF(nspace > 0) THEN
        WRITE(sint,'(i16)',IOSTAT=ierr) nspace
        fmt='('//TRIM(ADJUSTL(sint))//'x'
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt=fmt//',a'//TRIM(ADJUSTL(sint))//')'
      ELSE
        WRITE(sint,'(i16)',IOSTAT=ierr) LEN_TRIM(tmpTag)
        fmt='(a'//TRIM(ADJUSTL(sint))//')'
      ENDIF
      WRITE(unitNo,FMT=CHAR(fmt),IOSTAT=ierr) CHAR(tmpTag)
    ENDIF
    fmt=''
    tmpTag=''
  ENDIF
ENDSUBROUTINE write_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief Clears an XML element object and all subobjects.
!> @param thisXMLE the XML element object
!>
RECURSIVE SUBROUTINE clear_XMLElementType(thisXMLE)
  CLASS(XMLElementType),INTENT(INOUT) :: thisXMLE
  INTEGER(SIK) :: i

  IF(ASSOCIATED(thisXMLE%children)) THEN
    DO i=SIZE(thisXMLE%children),1,-1
      CALL clear_XMLElementType(thisXMLE%children(i))
    ENDDO
    DEALLOCATE(thisXMLE%children)
  ENDIF
  NULLIFY(thisXMLE%parent)
  IF(thisXMLE%nAttr > 0) THEN
    DO i=thisXMLE%nAttr,1,-1
      thisXMLE%attr_names(i)=''
      thisXMLE%attr_values(i)=''
    ENDDO
    IF(ALLOCATED(thisXMLE%attr_names)) DEALLOCATE(thisXMLE%attr_names)
    IF(ALLOCATED(thisXMLE%attr_values)) DEALLOCATE(thisXMLE%attr_values)
  ENDIF
  thisXMLE%content=''
  thisXMLE%name=''
  thisXMLE%nAttr=0
ENDSUBROUTINE clear_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief Determines if an XML element object is empty
!> @param thisXMLE the XML element object
!> @returns bool logical indicating if the XML element object is empty
!>
PURE FUNCTION isEmpty_XMLElementType(thisXMLE) RESULT(bool)
  CLASS(XMLElementType),INTENT(IN) :: thisXMLE
  LOGICAL(SBK) :: bool
  bool=(LEN_TRIM(thisXMLE%content) == 0 .AND. &
      .NOT.ASSOCIATED(thisXMLE%children))
ENDFUNCTION isEmpty_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief Determines if an XML element has a parent.
!> @param thisXMLE the XML element object
!> @returns bool logical indicating if the XML element object has a parent
!>
PURE FUNCTION hasParent_XMLElementType(thisXMLE) RESULT(bool)
  CLASS(XMLElementType),INTENT(IN) :: thisXMLE
  LOGICAL(SBK) :: bool
  bool=ASSOCIATED(thisXMLE%parent)
ENDFUNCTION hasParent_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief Gets a pointer to the XML element objects parent
!> @param thisXMLE the XML element object
!> @param parent the parent XML element object
!>
PURE SUBROUTINE getParent_XMLElementType(thisXMLE,parent)
  CLASS(XMLElementType),INTENT(INOUT) :: thisXMLE
  TYPE(XMLElementType),POINTER,INTENT(INOUT) :: parent
  NULLIFY(parent)
  parent => thisXMLE%parent
ENDSUBROUTINE getParent_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief Sets the XML element objects parent
!> @param thisXMLE the XML element object
!> @param parent the parent XML element object
!>
PURE SUBROUTINE setParent_XMLElementType(thisXMLE,parent)
  CLASS(XMLElementType),INTENT(INOUT) :: thisXMLE
  CLASS(XMLElementType),INTENT(INOUT),TARGET :: parent
  NULLIFY(thisXMLE%parent)
  thisXMLE%parent => parent
ENDSUBROUTINE setParent_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief Returns a logical of whether or not the XML element has child
!>        elements.
!> @param thisXMLE the XML element object
!> @returns bool logical indicating if the XML element has children
!>
PURE FUNCTION hasChildren_XMLElementType(thisXMLE) RESULT(bool)
  CLASS(XMLElementType),INTENT(IN) :: thisXMLE
  LOGICAL(SBK) :: bool
  bool=ASSOCIATED(thisXMLE%children)
ENDFUNCTION hasChildren_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief Gets a pointer to the children elements of an XML element
!> @param thisXMLE the XML element object
!> @param children a pointer to the XML elements children
!>
PURE SUBROUTINE getChildren_XMLElementType(thisXMLE,children)
  CLASS(XMLElementType),INTENT(INOUT) :: thisXMLE
  TYPE(XMLElementType),POINTER,INTENT(INOUT) :: children(:)
  NULLIFY(children)
  children => thisXMLE%children
ENDSUBROUTINE getChildren_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief Get a list of the attributes of an XML element
!> @param thisXMLE the XML element object
!> @param names the names of all the attributes
!> @param values the values of all the attributes
!>
PURE SUBROUTINE getAttributes_XMLElementType(thisXMLE,names,values)
  CLASS(XMLElementType),INTENT(INOUT) :: thisXMLE
  TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: names(:)
  TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: values(:)
  INTEGER(SIK) :: i

  IF(ALLOCATED(names)) DEALLOCATE(names)
  IF(ALLOCATED(values)) DEALLOCATE(values)
  ALLOCATE(names(thisXMLE%nAttr))
  ALLOCATE(values(thisXMLE%nAttr))
  DO i=1,thisXMLE%nAttr
    names(i)=thisXMLE%attr_names(i)
    values(i)=thisXMLE%attr_values(i)
  ENDDO
ENDSUBROUTINE getAttributes_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief Gets the value of an attribute of an XML element
!> @param thisXMLE the XML element
!> @param name the name of the attribute
!> @param val the value of the attribute with name
!>
PURE SUBROUTINE getAttributeValue_XMLElementType(thisXMLE,name,val)
  CLASS(XMLElementType),INTENT(INOUT) :: thisXMLE
  TYPE(StringType),INTENT(IN) :: name
  TYPE(StringType),INTENT(OUT) :: val
  INTEGER(SIK) :: i

  val=''
  DO i=1,thisXMLE%nAttr
    IF(name == thisXMLE%attr_names(i)) THEN
      val=thisXMLE%attr_values(i)
      EXIT
    ENDIF
  ENDDO
ENDSUBROUTINE getAttributeValue_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief Returns the content of an XML element as a string
!> @param thisXMLE the XML element
!> @returns content the content of an XML element
!>
PURE FUNCTION getContent_XMLElementType(thisXMLE) RESULT(content)
  CLASS(XMLElementType),INTENT(IN) :: thisXMLE
  TYPE(StringType) :: content
  content=thisXMLE%content
ENDFUNCTION getContent_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief sets the name of an XMLElementType
!> @param thisXMLE the XML element
!> @param the new name
!>
SUBROUTINE setName_XMLElementType(thisXMLE,name)
  CLASS(XMLElementType),INTENT(INOUT) :: thisXMLE
  TYPE(StringType),INTENT(IN) :: name
  thisXMLE%name=TRIM(name)
ENDSUBROUTINE setName_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief Sets the children of an XML element as input array of XMLElementTypes
!> @param thisXMLE the parent XML element
!> @param children the array of children element types
!>
SUBROUTINE setChildren_XMLElementType(thisXMLE,children)
  CLASS(XMLElementType),TARGET,INTENT(INOUT) :: thisXMLE
  TYPE(XMLElementType),POINTER,INTENT(INOUT) :: children(:)
  INTEGER(SIK) :: i,nChildren

  !if thisXMLE already has children, clear and deallocate them first
  IF(thisXMLE%hasChildren()) THEN
    DO i=SIZE(thisXMLE%children),1,-1
      CALL clear_XMLElementType(thisXMLE%children(i))
    ENDDO
    DEALLOCATE(thisXMLE%children)
  ENDIF

  IF(SIZE(children) > 0) THEN
    nChildren=SIZE(children)
    thisXMLE%children => children
    SELECTTYPE(thisXMLE); TYPE IS(XMLElementType)
      DO i=1,nChildren
        thisXMLE%children(i)%parent => thisXMLE
      ENDDO
    ENDSELECT
  ENDIF
ENDSUBROUTINE setChildren_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief sets or adds attribute to XMLElementType
!> @param thisXMLE the XML element
!> @param the attribute name
!> @param the value of the attribute(always a string for XML)
!>
SUBROUTINE setAttribute_XMLElementType(thisXMLE,name,value)
  CLASS(XMLElementType),INTENT(INOUT) :: thisXMLE
  TYPE(StringType),INTENT(IN) :: name,value

  INTEGER(SIK) :: i
  TYPE(StringType),ALLOCATABLE :: tmpNames(:),tmpVals(:)

  DO i=1,thisXMLE%nAttr
    IF(name == thisXMLE%attr_names(i)) THEN
      thisXMLE%attr_values(i)=value
      EXIT
    ENDIF
  ENDDO
  !If not currently an attribute add it
  IF(thisXMLE%nAttr > 0) THEN
    ALLOCATE(tmpNames(thisXMLE%nAttr))
    ALLOCATE(tmpVals(thisXMLE%nAttr))

    tmpNames=thisXMLE%attr_names
    tmpVals=thisXMLE%attr_values
    IF(ALLOCATED(thisXMLE%attr_names)) DEALLOCATE(thisXMLE%attr_names)
    IF(ALLOCATED(thisXMLE%attr_values)) DEALLOCATE(thisXMLE%attr_values)
    thisXMLE%nAttr=thisXMLE%nAttr+1
    ALLOCATE(thisXMLE%attr_names(thisXMLE%nAttr))
    ALLOCATE(thisXMLE%attr_values(thisXMLE%nAttr))
    DO i=1,thisXMLE%nAttr-1
      thisXMLE%attr_names(i)=tmpNames(i)
      thisXMLE%attr_values(i)=tmpVals(i)
    ENDDO

    thisXMLE%attr_names(thisXMLE%nAttr)=name
    thisXMLE%attr_values(thisXMLE%nAttr)=value

    DEALLOCATE(tmpNames)
    DEALLOCATE(tmpVals)
  ELSE
    IF(ALLOCATED(thisXMLE%attr_names)) DEALLOCATE(thisXMLE%attr_names)
    IF(ALLOCATED(thisXMLE%attr_values)) DEALLOCATE(thisXMLE%attr_values)
    thisXMLE%nAttr=thisXMLE%nAttr+1
    ALLOCATE(thisXMLE%attr_names(thisXMLE%nAttr))
    ALLOCATE(thisXMLE%attr_values(thisXMLE%nAttr))
    thisXMLE%attr_names=name
    thisXMLE%attr_values=value
  ENDIF
ENDSUBROUTINE setAttribute_XMLElementType
!
!-------------------------------------------------------------------------------
!> @brief Initializes an XML file type
!> @param thisXMLFile the XML file type object
!> @param fname the name of the file to process
!> @param lread whether or not the file will be opened for reading or writing
!>
SUBROUTINE init_XMLFileType(thisXMLFile,fname,lread)
  CHARACTER(LEN=*),PARAMETER :: myName='init_XMLFileType'
  CLASS(XMLFileType),INTENT(INOUT) :: thisXMLFile
  CHARACTER(LEN=*) :: fname
  LOGICAL(SBK) :: lread
  CHARACTER(LEN=LEN(fname)) :: fpath,fnm,fext

  IF(.NOT.thisXMLFile%isInit) THEN
    !Initialize the file
    CALL getFileParts(fname,fpath,fnm,fext,thisXMLFile%e)
    CALL thisXMLFile%setFilePath(fpath)
    CALL thisXMLFile%setFileName(fnm)
    CALL thisXMLFile%setFileExt(fext)
    CALL thisXMLFile%setReadStat(lread)
    CALL thisXMLFile%setWriteStat(.NOT.lread)

    ALLOCATE(thisXMLFile%root)
    thisXMLFile%isInit=.TRUE.
  ELSE
    CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
        ' - File is already initialized!')
  ENDIF
ENDSUBROUTINE init_XMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Clears the XML File object
!> @param thisXMLFile the XML file object
!>
SUBROUTINE clear_XMLFileType(thisXMLFile)
  CLASS(XMLFileType),INTENT(INOUT) :: thisXMLFile
  IF(ASSOCIATED(thisXMLFile%root)) THEN
    CALL thisXMLFile%root%clear()
    DEALLOCATE(thisXMLFile%root)
  ENDIF
  CALL thisXMLFile%setEOFStat(.FALSE.)
  CALL thisXMLFile%fclose()
  CALL thisXMLFile%setFilePath('')
  CALL thisXMLFile%setFileName('')
  CALL thisXMLFile%setFileExt('')
  CALL thisXMLFile%setReadStat(.FALSE.)
  CALL thisXMLFile%setWriteStat(.FALSE.)
  thisXMLFile%version=1.0
  thisXMLFile%encoding='UTF-8'
ENDSUBROUTINE clear_XMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Opens the XML file type for I/O
!> @param file the XML file type object
!>
!> @todo fix how the unit number is set
!>
SUBROUTINE fopen_XMLFileType(file)
  CHARACTER(LEN=*),PARAMETER :: myName='fopen_XMLFileType'
  CLASS(XMLFileType),INTENT(INOUT) :: file
  LOGICAL(SBK) :: lopen
  INTEGER(SIK) :: funit,ierr
  TYPE(StringType) :: fname

  IF(.NOT.file%isOpen()) THEN
    !Find a valid unit number
    funit=700
    INQUIRE(UNIT=funit,OPENED=lopen)
    DO WHILE(lopen)
      funit=funit+1
      INQUIRE(UNIT=funit,OPENED=lopen)
    ENDDO

    file%unitNo=funit
    fname=TRIM(file%getFilePath())//TRIM(file%getFileName())// &
        TRIM(file%getFileExt())

    IF(file%isRead()) THEN
      !Open the file for reading
      OPEN(UNIT=file%unitNo,FILE=CHAR(fname),STATUS='OLD', &
          ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ',IOSTAT=ierr)
    ELSE
      !Open the file for writing
      OPEN(UNIT=file%unitNo,FILE=CHAR(fname),STATUS='REPLACE', &
          ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='WRITE', &
          ENCODING=file%encoding,IOSTAT=ierr)
    ENDIF
    IF(ierr == 0) THEN
      CALL file%setOpenStat(.TRUE.)
    ELSE
      CALL file%e%raiseError(modName//'::'//myName// &
          ' - Trouble opening file!')
      CALL file%setOpenStat(.FALSE.)
    ENDIF
  ELSE
    CALL file%e%raiseError(modName//'::'//myName// &
        ' - File is already open!')
  ENDIF
ENDSUBROUTINE fopen_XMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Closes the XML file object
!> @param file teh XML file object
!>
SUBROUTINE fclose_XMLFileType(file)
  CHARACTER(LEN=*),PARAMETER :: myName='fclose_XMLFileType'
  CLASS(XMLFileType),INTENT(INOUT) :: file
  INTEGER(SIK) :: ierr
  IF(file%isOpen()) THEN
    CLOSE(file%unitNo,IOSTAT=ierr)
    IF(ierr == 0) THEN
      CALL file%setOpenStat(.FALSE.)
    ELSE
      CALL file%e%raiseError(modName//'::'//myName// &
          ' - trouble closing file!')
    ENDIF
  ENDIF
ENDSUBROUTINE fclose_XMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Deletes the XML file from disk
!> @param file the XML file object
!>
SUBROUTINE fdelete_XMLFileType(file)
  CHARACTER(LEN=*),PARAMETER :: myName='fdelete_XMLFileType'
  CLASS(XMLFileType),INTENT(INOUT) :: file
  INTEGER(SIK) :: ierr
  IF(file%isOpen()) THEN
    CLOSE(file%unitNo,STATUS='DELETE',IOSTAT=ierr)
  ELSE
    CALL file%fopen()
    CLOSE(file%unitNo,STATUS='DELETE',IOSTAT=ierr)
  ENDIF
  IF(ierr == 0) THEN
    CALL file%setOpenStat(.FALSE.)
  ELSE
    CALL file%e%raiseError(modName//'::'//myName// &
        ' - trouble closing file!')
  ENDIF
ENDSUBROUTINE fdelete_XMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Loads an XML file from disk into the XML File type object in memory
!> @param thisXMLFile the XML file to populate from disk
!> @param fname the name of the file to process
!>
SUBROUTINE importFromDisk_XMLFileType(thisXMLFile,fname)
  CHARACTER(LEN=*),PARAMETER :: myName='importFromDisk_XMLFileType'
  CLASS(XMLFileType),INTENT(INOUT) :: thisXMLFile
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: fname

  CHARACTER(LEN=1),ALLOCATABLE :: cachedFile(:)
  INTEGER(SIK) :: nchars,nopen,nclose,nTags,ic,i,nlines
  INTEGER(SIK) :: rootTagEnd,rootTagBegin
  INTEGER(SIK),ALLOCATABLE :: itag(:,:),lines(:)
  TYPE(StringType) :: tagStr

  !Initialize and open the file if needed
  IF(.NOT.thisXMLFile%isInit) THEN
    IF(PRESENT(fname)) CALL thisXMLFile%init(fname,.TRUE.)
  ENDIF
  IF(.NOT.thisXMLFile%isOpen()) CALL thisXMLFile%fopen()

  IF(thisXMLFile%isInit) THEN
    CALL thisXMLFile%root%clear()

    IF(thisXMLFile%isOpen() .AND. thisXMLFile%isRead()) THEN
      !Get the XML Declaration info
      CALL processXMLDecl(thisXMLFile)

      !Cache the file for processing
      SELECTTYPE(thisXMLFile); TYPE IS(XMLFileType)
        CALL cacheXMLFile(thisXMLFile,nchars,cachedFile)
      ENDSELECT

      !Count the number of markup characters "<" and ">" and lines
      nopen=0
      nclose=0
      nlines=0
      DO ic=1,nchars
        IF(cachedFile(ic) == '<') nopen=nopen+1
        IF(cachedFile(ic) == '>') nclose=nclose+1
        IF(cachedFile(ic) == LF) nlines=nlines+1
      ENDDO
      IF(nopen /= nclose) THEN
        CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
            ' - mismatched markup characters!')
      ELSE
        !Store the locations of all the markup characters "<" and ">" lines
        nTags=nopen
        ALLOCATE(itag(3,nTags))
        ALLOCATE(lines(nlines))
        nopen=0
        nclose=0
        nlines=0
        DO ic=1,nchars
          IF(cachedFile(ic) == '<') THEN
            nopen=nopen+1
            itag(1,nopen)=ic
          ENDIF
          IF(cachedFile(ic) == '>') THEN
            nclose=nclose+1
            itag(2,nopen)=ic
          ENDIF
          IF(cachedFile(ic) == LF) THEN
            nlines=nlines+1
            lines(nlines)=ic
          ENDIF
        ENDDO

        !Verify that they are all matching (interleaved)
        DO i=1,nTags
          IF(itag(1,i) > itag(2,i)) THEN
            CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
                ' - mismatched markup characters!')
            EXIT
          ENDIF
        ENDDO

        DO i=1,nTags
          !Create temporary string
          CALL charArrytoStr(cachedFile(itag(1,i):itag(2,i)), tagStr)
          itag(3,i)=BAD_TAG
          !Determine tag types
          IF(INDEX(tagStr,'<?') == 1) THEN
            !Processing Instruction
            !Check closing marker to insure tag validity
            IF(INDEX(tagStr,'?>',.TRUE.) == LEN_TRIM(tagStr)-1) &
                itag(3,i)=PROCESSING_INST_TAG
          ELSEIF(INDEX(tagStr,'<!--') == 1) THEN
            !Comment
            !Check closing marker to insure tag validity
            IF(INDEX(tagStr,'-->',.TRUE.) /= LEN_TRIM(tagStr)-1 .AND. &
                INDEX(tagStr,'--->',.TRUE.) == 0) itag(3,i)=COMMENT_TAG
          ELSEIF(INDEX(tagStr,'<!') == 1) THEN
            !Declaration (Not currently treated)
            itag(3,i)=DECLARATION_TAG
          ELSEIF(INDEX(tagStr,'</') == 1) THEN
            !EndTag
            !Check closing marker to insure tag validity
            itag(3,i)=END_TAG
          ELSE
            !Determine if start tag or empty-element tag
            IF(INDEX(tagStr,'/>',.TRUE.) == LEN_TRIM(tagStr)-1) THEN
              itag(3,i)=EMPTY_ELEMENT_TAG
            ELSE
              itag(3,i)=START_TAG
            ENDIF
          ENDIF
          IF(itag(3,i) == BAD_TAG) THEN
            CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
                ' - Unrecognizable markup in "'//tagStr//'"!')
          ENDIF
        ENDDO

        !Find first start tag
        DO i=1,nTags
          IF(itag(3,i) == START_TAG) THEN
            rootTagBegin=i
            EXIT
          ELSEIF(itag(3,i) == END_TAG .OR. &
              (itag(3,i) == EMPTY_ELEMENT_TAG .AND. i < nTags)) THEN
              CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
              ' - Could not locate start of root element!')
          ENDIF
        ENDDO

        !Find last end tag
        DO i=nTags,1,-1
          IF(itag(3,i) == END_TAG) THEN
            rootTagEnd=i
            EXIT
          ELSEIF(itag(3,i) == START_TAG .OR. &
              (itag(3,i) == EMPTY_ELEMENT_TAG .AND. i < nTags) .OR. &
              itag(3,i) == DECLARATION_TAG) THEN
              CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
              ' - Could not locate end of root element!')
          ENDIF
        ENDDO

        !Process the elements
        CALL thisXMLFile%root%init(cachedFile,itag,lines,rootTagBegin, &
            rootTagEnd)
      ENDIF
    ELSE
      CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
          ' - XML File could not be opened for reading!')
    ENDIF
  ELSE
    CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
        ' - XML File could not be initialized!')
  ENDIF
ENDSUBROUTINE importFromDisk_XMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Dumps an XML file stored in memory to disk.
!> @param thisXMLFile the XML file type to write to disk
!> @param fname the file name to use on disk
!>
SUBROUTINE exportToDisk_XMLFileType(thisXMLFile,fname)
  CLASS(XMLFileType),INTENT(INOUT) :: thisXMLFile
  CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: fname
  CHARACTER(LEN=4) :: version
  INTEGER(SIK) :: ierr
  TYPE(StringType) :: header
  TYPE(XMLFileType) :: tmpFile

  IF(ASSOCIATED(thisXMLFile%root)) THEN
    CALL tmpFile%init(fname,.FALSE.)
    CALL tmpFile%fopen()

    !Write the header
    WRITE(version,FMT='(f4.1)',IOSTAT=ierr) thisXMLFile%version
    header='<?xml version="'//TRIM(ADJUSTL(version))// &
        '" encoding="'//TRIM(thisXMLFile%encoding)//'"?>'
    WRITE(tmpFile%unitNo,FMT='(a)') CHAR(header)

    !Write style-sheet info
    IF(LEN(thisXMLFile%style_sheet) > 0) THEN
      WRITE(tmpFile%unitNo,FMT='(a)') &
          '<?xml-stylesheet version="1.0" type="text/xsl" href="'// &
          thisXMLFile%style_sheet//'"?>'
    ENDIF

    !Write the XML Elements
    CALL thisXMLFile%root%fwrite(tmpFile%unitNo,0)
    CALL tmpFile%clear()
  ENDIF
ENDSUBROUTINE exportToDisk_XMLFileType
!
!-------------------------------------------------------------------------------
!> @brief Processes the XML declaration
!> @param thisXMLFile the XML file to process the declaration
!>
SUBROUTINE processXMLDecl(thisXMLFile)
  CHARACTER(LEN=*),PARAMETER :: myName='processXMLDecl'
  INTEGER(SIK),PARAMETER :: maxbuf=512
  CLASS(XMLFileType),INTENT(INOUT) :: thisXMLFile

  CHARACTER(LEN=1) :: tmpChar
  CHARACTER(LEN=7) :: fencoding,curEncoding
  CHARACTER(LEN=maxbuf) :: ioBuffer
  LOGICAL(SBK) :: lread
  INTEGER(SIK) :: i,ierr,ibuf,nattr
  REAL(SRK) :: version
  TYPE(StringType) :: tagStr,firstTag,fname
  TYPE(StringType),ALLOCATABLE :: anames(:),avalues(:)

  fencoding='DEFAULT'

  !Rewind the file
  REWIND(thisXMLFile%unitNo)

  !Get the first tag.
  ioBuffer=''
  ierr=0
  lread=.TRUE.
  tagStr=''
  DO WHILE(lread)
    READ(thisXMLFile%unitNo,'(a1)',ADVANCE='NO',IOSTAT=ierr) tmpChar
    IF(ierr == IOSTAT_EOR) tmpChar=CHAR(10)
    IF(ierr == IOSTAT_END) THEN
      CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
          ' - Reached end of file before finding start of first tag "<"!')
      EXIT !No more tags
    ENDIF

    IF(tmpChar == '<') THEN
      !At the start of the tag
      ibuf=1
      ioBuffer(1:1)=tmpChar

      !Stream to closing marker
      DO WHILE(ierr /= IOSTAT_END .AND. tmpChar /= '>')
        ibuf=ibuf+1
        READ(thisXMLFile%unitNo,'(a1)',ADVANCE='NO',IOSTAT=ierr) tmpChar
        ioBuffer(ibuf:ibuf)=tmpChar
        IF(ibuf == maxbuf) THEN
          tagStr=tagStr//ioBuffer
          ibuf=1
        ENDIF
      ENDDO

      !Insure that the last character of the ioBuffer is '>'
      IF(ioBuffer(ibuf:ibuf) == '>') THEN
        tagStr=tagStr//ioBuffer(1:ibuf)
      ELSE
        !Throw an exception end of file before end marker
        CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
            ' - Reached end of file before finding closing marker'// &
            ' ">" for XML tag!')
      ENDIF
      EXIT
    ELSE
      !Insure that only whitespace was read
      IF(.NOT.(SCAN(tmpChar,SP//CR//LF//TB) > 0)) THEN
        !Throw an exception, illegal characters
        CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
            ' - Illegal characters between XML tags!')
        EXIT
      ENDIF
    ENDIF
  ENDDO
  firstTag=tagStr

  !Check that the first Tag starting and ending markers are
  !the XML declaration
  IF(INDEX(firstTag,'<?xml') == 1 .AND. &
      INDEX(firstTag,'?>',.TRUE.) == LEN_TRIM(firstTag)-1) THEN
    !Process the attributes if it is an xml declaration
    CALL processTagAttributes(CHAR(firstTag),nattr,anames,avalues,ierr)

    IF(ierr == 0) THEN
      !The XML declaration only has 3 possible attributes
      ! - version (required, always first)
      ! - encoding (optional)
      ! - standalone (optional)
      IF(0 < nattr .AND. nattr < 4) THEN
        !Set the version
        IF(anames(1) == 'version') THEN
          IF(INDEX(avalues(1),'1.') == 1) THEN
            version=avalues(1)%stof()
            IF(1.0_SRK < version .AND. version < 2.0_SRK) &
                thisXMLFile%version=version
          ELSE
            !Illegal value for version
            CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
                ' - XML Version "'//TRIM(avalues(1))//'" is not supported!')
          ENDIF
        ELSE
          !first attribute must be 'version'!
          CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
              ' - The first attribute must be the XML version!')
        ENDIF

        !Check other attributes
        IF(nattr > 1) THEN
          DO i=2,nattr
            IF(anames(i) == 'encoding') THEN
              !check if this module can process it the encoding
              IF(avalues(i) == 'UTF-8' .OR. avalues(i) == 'utf-8') THEN
                thisXMLFile%encoding='UTF-8'
                fencoding='UTF-8'
              ELSEIF(avalues(i) == 'US-ASCII' .OR. avalues(i) == 'us-ascii') THEN
                thisXMLFile%encoding='US-ASCII'
              ELSE
                CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
                    ' - File encoding "'//TRIM(avalues(i))//'" is not supported!')
              ENDIF

              !Re-open file if it was not opened with the matching encoding
              INQUIRE(UNIT=thisXMLFile%unitNo,ENCODING=curEncoding)
              IF(fencoding /= curEncoding) THEN
                !Close the file
                CALL thisXMLFile%fclose()

                !Open the file for reading
                fname=TRIM(thisXMLFile%getFilePath())// &
                    TRIM(thisXMLFile%getFileName())// &
                    TRIM(thisXMLFile%getFileExt())
                OPEN(UNIT=thisXMLFile%unitNo,FILE=CHAR(fname),STATUS='OLD', &
                    ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ', &
                    ENCODING=fencoding,IOSTAT=ierr)
                CALL thisXMLFile%setOpenStat(.TRUE.)
              ENDIF
            ELSEIF(anames(i) == 'standalone') THEN
              IF(avalues(i) == 'yes') THEN
                thisXMLFile%standalone=.TRUE.
              ELSEIF(avalues(i) == 'no') THEN
                thisXMLFile%standalone=.FALSE.
              ELSE
                !Illegal value for 'standalone' attribute
                CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
                    ' - illegal value "'//TRIM(avalues(i))// &
                    '" for "standalone" attribute in XML declaration.')
              ENDIF
            ELSE
              !Illegal attribute
              CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
                  ' - illegal attribute "'//TRIM(anames(i))// &
                  '" for XML declaration.')
            ENDIF
          ENDDO
        ENDIF
      ELSE
        !Illegal number of attributes in declaration
        CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
            ' - illegal  number of attributes in XML declaration.')
      ENDIF
    ELSE
      !Failed to process attributes for XMLDecl
      CALL thisXMLFile%e%raiseError(modName//'::'//myName// &
          ' - Failed to process attributes in XML declaration.')
    ENDIF
  ENDIF
  REWIND(thisXMLFile%unitNo)
ENDSUBROUTINE processXMLDecl
!
!-------------------------------------------------------------------------------
!> @brief Loads the XML file into memory
!> @param thisXMLFile the xml file type pointing to the file to load into memory
!> @param nchars the number of characters in the file (max is 1000000)
!> @param fileCache a 1-D character array with all the characters from the file
!>
!> If the file has more than 1 million characters it will not be cached.
!>
SUBROUTINE cacheXMLFile(thisXMLFile,nchars,fileCache)
  TYPE(XMLFileType),INTENT(INOUT) :: thisXMLFile
  INTEGER(SIK),INTENT(OUT) :: nchars
  CHARACTER(LEN=1),ALLOCATABLE,INTENT(INOUT) :: fileCache(:)
  CHARACTER(LEN=1024) :: tmpChar
  INTEGER(SIK) :: i,j,ierr

  IF(ALLOCATED(fileCache)) DEALLOCATE(fileCache)
  REWIND(thisXMLFile%unitNo)

  ierr=0
  nchars=0
  DO WHILE(ierr /= IOSTAT_END)
    READ(thisXMLFile%unitNo,'(a1024)',ADVANCE='NO',IOSTAT=ierr) tmpChar
    IF(ierr == 0) THEN
      nchars=nchars+1024
    ELSEIF(ierr == IOSTAT_EOR) THEN
      nchars=nchars+LEN_TRIM(tmpChar)+1
    ENDIF
  ENDDO

  IF(nchars > 0) THEN
    ALLOCATE(fileCache(nchars))
    REWIND(thisXMLFile%unitNo)

    ierr=0
    i=0
    DO WHILE(ierr /= IOSTAT_END)
      READ(thisXMLFile%unitNo,'(a1024)',ADVANCE='NO',IOSTAT=ierr) tmpChar
      IF(ierr == 0) THEN
        DO j=1,1024
          fileCache(i+j)=tmpChar(j:j)
        ENDDO
        i=i+1024
      ELSEIF(ierr == IOSTAT_EOR) THEN
        DO j=1,LEN_TRIM(tmpChar)
          fileCache(i+j)=tmpChar(j:j)
        ENDDO
        fileCache(i+j)=LF
        i=i+j
      ENDIF
    ENDDO
    CALL thisXMLFile%fclose()
  ELSE
    nchars=-1
  ENDIF
ENDSUBROUTINE cacheXMLFile
!
!-------------------------------------------------------------------------------
!> @brief Get the name of an XML tag
!> @param fulltag the full tag string
!> @param ierr return error code
!> @returns sname a string with the tag name
!>
!> For the return error codes the values are:
!>  0: Success
!> -1: Bad value for fulltag
!> -2: illegal first character for tag name
!> -3: illegal character in tag name
!> -4: tag name starts with "xml"
!>
SUBROUTINE getTagName(fullTag,ierr,sname)
  CHARACTER(LEN=1),INTENT(IN) :: fullTag(:)
  INTEGER(SIK),INTENT(INOUT) :: ierr
  TYPE(StringType),INTENT(INOUT) :: sname
  CHARACTER(LEN=3) :: xml
  INTEGER(SIK) :: nchar,istp,inamechar,i,charval,istt
  sname=''
  nchar=SIZE(fullTag)
  IF(fullTag(1) == '<' .AND. fullTag(nchar) == '>') THEN
    istt=2
    IF(fullTag(2) == '/') istt=3 !This is an endtag
    inamechar=IACHAR(fullTag(istt))

    IF(inamechar == 58 .OR. inamechar == 95 .OR. &
        (64 < inamechar .AND. inamechar < 91) .OR. &
        (96 < inamechar .AND. inamechar < 123)) THEN

      IF(nchar-istt > 2) THEN
        xml(1:1)=fullTag(istt)
        xml(2:2)=fullTag(istt+1)
        xml(3:3)=fullTag(istt+2)
        CALL toUpper(xml)
        IF(xml == 'XML') THEN
          !Names cannot start with "xml"
          ierr=-4
          istp=-1
          nchar=1 !Skip executing the loop
        ENDIF
      ENDIF

      istp=0
      DO i=istt,nchar-1
        IF(ANY(fulltag(i) == (/LF,CR,SP,TB/))) THEN
          istp=i-1
          EXIT
        ENDIF
        charval=IACHAR(fulltag(i))

        !Check that the character is valid in a name
        IF(.NOT.(charval == 45 .OR. charval == 96 .OR. &
                charval == 95 .OR. &
                (64 < charval .AND. charval < 91) .OR. &
                (96 < charval .AND. charval < 123) .OR. &
                (47 < charval .AND. charval < 59))) THEN
          istp=-1
          EXIT
        ENDIF
      ENDDO
      IF(istp == 0) istp=nchar-1
      IF(istp > 0) THEN
        CALL charArrytoStr(fullTag(istt:istp), sname)
        ierr=0
      ELSE
        ierr=-3 !Illegal character in tag name
      ENDIF
    ELSE
      ierr=-2 !Illegal first character of tag name
    ENDIF
  ELSE
    ierr=-1 !Bad Tag
  ENDIF
ENDSUBROUTINE getTagName
!
!-------------------------------------------------------------------------------
!> @brief For a given XML element extract the tag information
!> @param elStartTag the element string with start tag
!> @param nattr the number of attributes for the element
!> @param anames the names of the attributes
!> @param avalues the values of the attributes
!> @param ierr return error code
!>
!> Return values of ierr are:
!>  0: no errors
!> -1: the element string does not have start/end tags
!> -2: "==" was encountered
!> -3: bad attribute name
!> -4: bad attribute value
!>
SUBROUTINE processTagAttributes(elStartTag,nattr,anames,avalues,ierr)
  CHARACTER(LEN=*),INTENT(IN) :: elStartTag
  INTEGER(SIK),INTENT(OUT) :: nattr
  TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: anames(:)
  TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: avalues(:)
  INTEGER(SIK),INTENT(OUT) :: ierr

  CHARACTER(LEN=1) :: quote
  CHARACTER(LEN=LEN(elStartTag)) :: startTag
  INTEGER(SIK) :: ic,i,nchars,namestt,valstt,valstp,bs_i
  INTEGER(SIK),ALLOCATABLE :: anchorLoc(:)
  LOGICAL(SBK) :: word_encountered

  !Initialize return arguments
  ierr=0
  nattr=0
  IF(ALLOCATED(anames)) DEALLOCATE(anames)
  IF(ALLOCATED(avalues)) DEALLOCATE(avalues)

  !Copy input arg to temporary
  startTag=elStartTag
  startTag=ADJUSTL(startTag)
  nchars=LEN_TRIM(startTag)

  IF(startTag(1:1) == '<' .AND. startTag(nchars:nchars) == '>') THEN
    !Make sure this is not an end tag or comment
    IF(startTag(1:2) /= '<!') THEN

      !Count the number of attributes (count the number of '=' characters)
      DO ic=2,nchars
        IF(startTag(ic:ic) == '=') THEN
          nattr=nattr+1

          !Check to make sure occurrence of '=' is not in '=='
          IF(startTag(ic-1:ic-1) == '=') THEN
            ierr=-2
            nattr=0
            EXIT
          ENDIF
        ENDIF
      ENDDO

      IF(ierr /= -2) THEN
        !Allocate the return arguments
        ALLOCATE(anames(nattr))
        ALLOCATE(avalues(nattr))
        ALLOCATE(anchorLoc(nattr))

        !Get the locations of the '=' characters.
        nattr=0
        DO ic=2,nchars
          IF(startTag(ic:ic) == '=') THEN
            nattr=nattr+1
            anchorLoc(nattr)=ic
          ENDIF
        ENDDO
      ENDIF

      !Get the names (names precede the '=' character with possible whitespace preceeding '=')
      !attribute names cannot contain whitespace, must be unique and preceded
      !by whitespace
      DO i=1,nattr
        ! March backwards from anchorLoc(i) through char array if whitespace preceeds the '='
        IF(startTag(anchorLoc(i)-1:anchorLoc(i)-1) == ' ') THEN
          ! march through word string backwards until whitespace is encountered
          word_encountered=.FALSE.
          DO bs_i=anchorLoc(i)-2,1,-1
            IF(startTag(bs_i:bs_i) /= ' ' .AND. .NOT. word_encountered) word_encountered=.TRUE.
            IF(startTag(bs_i:bs_i) == ' ' .AND. word_encountered) THEN
              namestt=bs_i
              EXIT
            ENDIF
            IF(bs_i==1) ierr=-3
          ENDDO
        ELSE
          ! CHAR(9) == TAB, CHAR(10) == Newline, CHAR(13) == Carriage return
          ! SCAN(...,TRUE) returns rightmost position
          namestt=SCAN(startTag(1:anchorLoc(i)),' '//CHAR(9)//CHAR(10)//CHAR(13),.TRUE.)
        ENDIF
        IF(0 < namestt .AND. namestt < anchorLoc(i)) THEN
          anames(i)=startTag(namestt+1:anchorLoc(i)-1)
        ELSE
          ierr=-3
        ENDIF
      ENDDO

      !Get the values
      DO i=1,nattr
        valstt=anchorLoc(i)+1
        ! In case of whitespace following '='
        IF(startTag(valstt:valstt)==' ') THEN
          ! march forward until quote is encountered
          DO bs_i=anchorLoc(i)+1,nchars
            IF(startTag(bs_i:bs_i) == '"') THEN
              valstt=bs_i
              EXIT
            ENDIF
            IF(bs_i==nchars) ierr=-4
          ENDDO
        ENDIF
        quote=startTag(valstt:valstt)
        valstp=INDEX(startTag(valstt+1:nchars),quote)+valstt
        IF(valstt < valstp) THEN
          avalues(i)=startTag(valstt+1:valstp-1)
        ELSE
          ierr=-4
        ENDIF
      ENDDO
    ENDIF
  ELSE
    ierr=-1
  ENDIF
ENDSUBROUTINE processTagAttributes
!
!-------------------------------------------------------------------------------
!> @brief Determines the number of child elements fora  given XML element
!> @param tagBegin the index of the first tag
!> @param tagEnd the index of the last tag
!> @param iTag the XML tags for a given element
!> @param nChildren the number of child elements
!> @param childTags the start/stop indeces of the tags of any children
!> @param ierr error code
!>
PURE SUBROUTINE DetermineNChildren(tagBegin,tagEnd,iTag,nChildren,childTags,ierr)
  INTEGER(SIK),INTENT(IN) :: tagBegin
  INTEGER(SIK),INTENT(IN) :: tagEnd
  INTEGER(SIK),INTENT(IN) :: iTag(:,:)
  INTEGER(SIK),INTENT(OUT) :: nChildren
  INTEGER(SIK),ALLOCATABLE,INTENT(INOUT) :: childTags(:,:)
  INTEGER(SIK),INTENT(OUT) :: ierr

  INTEGER(SIK) :: nTagRemain,iLevel,i,iChild

  nChildren=0
  IF(ALLOCATED(childTags)) DEALLOCATE(childTags)

  nTagRemain=tagEnd-tagBegin-1
  ierr=-1
  IF(nTagRemain > 0) THEN
    iLevel=0
    DO i=tagBegin+1,tagEnd-1
      IF(iTag(3,i) == EMPTY_ELEMENT_TAG .AND. iLevel == 0) THEN
        nChildren=nChildren+1
      ELSEIF(iTag(3,i) == START_TAG) THEN
        iLevel=iLevel+1
      ELSEIF(iTag(3,i) == END_TAG) THEN
        iLevel=iLevel-1
        IF(iLevel == 0) nChildren=nChildren+1
      ENDIF
    ENDDO
    IF(iLevel /= 0) THEN
      !This means there were not matching end tags!
      ierr=-2
      nChildren=0
    ELSE
      ierr=0
      ALLOCATE(childTags(2,nChildren))
      iLevel=0
      iChild=0
      DO i=tagBegin+1,tagEnd-1
        IF(iTag(3,i) == EMPTY_ELEMENT_TAG .AND. iLevel == 0) THEN
          iChild=iChild+1
          childTags(:,iChild)=i
        ELSEIF(iTag(3,i) == START_TAG) THEN
          iLevel=iLevel+1
          IF(iLevel == 1) THEN
            iChild=iChild+1
            childTags(1,iChild)=i
          ENDIF
        ELSEIF(iTag(3,i) == END_TAG) THEN
          iLevel=iLevel-1
          IF(iLevel == 0) childTags(2,iChild)=i
        ENDIF
      ENDDO
    ENDIF
  ENDIF
ENDSUBROUTINE DetermineNChildren
!
!-------------------------------------------------------------------------------
!> @brief Converts a character array to a string
!> @param charry an array of characters
!> @returns s a string type
!>
PURE SUBROUTINE charArryToStr(charry, s)
  CHARACTER(LEN=1),INTENT(IN) :: charry(:)
  TYPE(StringType),INTENT(INOUT) :: s
  CHARACTER(LEN=SIZE(charry)) :: tmpChar
  INTEGER(SIK) :: i
  DO i=1,SIZE(charry)
    tmpChar(i:i)=charry(i)
  ENDDO
  s=tmpChar
ENDSUBROUTINE charArryToStr
!
ENDMODULE FileType_XML
