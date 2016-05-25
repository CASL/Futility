!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                              Copyright (C) 2012                              !
!                   The Regents of the University of Michigan                  !
!              MPACT Development Group and Prof. Thomas J. Downar              !
!                             All rights reserved.                             !
!                                                                              !
! Copyright is reserved to the University of Michigan for purposes of          !
! controlled dissemination, commercialization through formal licensing, or     !
! other disposition. The University of Michigan nor any of their employees,    !
! makes any warranty, express or implied, or assumes any liability or          !
! responsibility for the accuracy, completeness, or usefulness of any          !
! information, apparatus, product, or process disclosed, or represents that    !
! its use would not infringe privately owned rights. Reference herein to any   !
! specific commercial products, process, or service by trade name, trademark,  !
! manufacturer, or otherwise, does not necessarily constitute or imply its     !
! endorsement, recommendation, or favoring by the University of Michigan.      !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testXMLFileType
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE IntrType
  USE Strings
  USE FileType_XML
  IMPLICIT NONE
  
  TYPE(XMLFileType) :: testXMLFile
  TYPE(XMLElementType),POINTER :: root
  
  !ALLOCATE(testXMLFile%e)
  CREATE_TEST('XML TYPES')
  CALL testXMLFile%importFromDisk('./testFile.xml')
  CALL testXMLFile%exportToDisk('./testWrite.xml')
  
  !DEALLOCATE(testXMLFile%e)
  root => testXMLFile%root
  ASSERTFAIL(ASSOCIATED(root),'root Element')
  REGISTER_SUBTEST('%get',testGet)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!Test get on XMLElementType
  SUBROUTINE testGet()
    LOGICAL(SBK) :: bool
    INTEGER(SIK) :: ich,nch,iattr,nattr
    TYPE(StringType) :: attr_name,val
    TYPE(StringType),ALLOCATABLE :: attr_names(:),attr_values(:)
    TYPE(XMLElementType),POINTER :: parent,children(:)

    !Test get children
    CALL root%getChildren(children)
    bool=ASSOCIATED(children)
    ASSERT(bool,'getChildren')
    nch=SIZE(children)
    ASSERT(nch == 11,'number of children')

    !Test get parent
    DO ich=1,nch
      CALL children(ich)%getParent(parent)
      bool=ASSOCIATED(parent,root)
      ASSERT(bool,'getParent')
      NULLIFY(parent)
    ENDDO !ich

    CALL root%getParent(parent)
    bool=ASSOCIATED(parent)
    ASSERT(.NOT. bool,'no parent')

    !Test get attributes and get attribute value
    CALL children(1)%getAttributes(attr_names,attr_values)
    nattr=SIZE(attr_values)
    ASSERT(nattr == 3,'# Attributes')

    bool=(TRIM(attr_names(1)) == 'name')
    ASSERT(bool,'attr_name')
    bool=(TRIM(attr_names(2)) == 'type')
    ASSERT(bool,'attr_name')
    bool=(TRIM(attr_names(3)) == 'value')
    ASSERT(bool,'attr_name')

    DO iattr=1,nattr
      CALL children(1)%getAttributeValue(attr_names(iattr),val)
      ASSERT(val == attr_values(iattr),'getAttributeValue')
    ENDDO !iattr

    NULLIFY(children)
    NULLIFY(parent)
    DEALLOCATE(attr_names)
    DEALLOCATE(attr_values)
  ENDSUBROUTINE testGet
!
ENDPROGRAM testXMLFileType
