!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
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

!ALLOCATE(testXMLFile%e)
CREATE_TEST('XML TYPES')
CALL testXMLFile%importFromDisk('./testFile.xml')
CALL testXMLFile%exportToDisk('./testWrite.xml')

!DEALLOCATE(testXMLFile%e)
ASSERTFAIL(ASSOCIATED(testXMLFile%root),'root Element')
REGISTER_SUBTEST('%get',testGet)
REGISTER_SUBTEST('%set',testSet)

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
TYPE(StringType) :: val
TYPE(StringType),ALLOCATABLE :: attr_names(:),attr_values(:)
TYPE(XMLElementType),POINTER :: parent,children(:)

!Test get children
CALL testXMLFile%root(1)%getChildren(children)
bool=ASSOCIATED(children)
ASSERT(bool,'getChildren')
nch=SIZE(children)
ASSERT(nch == 11,'number of children')

!Test get parent
DO ich=1,nch
  CALL children(ich)%getParent(parent)
  bool=ASSOCIATED(parent,testXMLFile%root(1))
  ASSERT(bool,'getParent')
  NULLIFY(parent)
ENDDO !ich

CALL testXMLFile%root(1)%getParent(parent)
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
!-------------------------------------------------------------------------------
SUBROUTINE testSet()
LOGICAL(SBK) :: bool
INTEGER(SIK) :: nch,nchComp
TYPE(StringType) :: attr_name,val,refval
TYPE(StringType) :: refName,setName
TYPE(XMLElementType),POINTER :: setChildren(:),getChildren(:)

refName=testXMLFile%root(1)%name
bool=(TRIM(refName) == 'ParameterList')
ASSERT(bool,'correct Name')

!set Name
setName='Parameter'
CALL testXMLFile%root(1)%setName(setName)
setName=testXMLFile%root(1)%name
bool=(TRIM(setName) == 'Parameter')
ASSERT(bool,'setName')
!Reset to correct value
CALL testXMLFile%root(1)%setName(refName)

!Get the children to modify/set
CALL testXMLFile%root(1)%getChildren(getChildren)
nch=SIZE(getChildren)
ALLOCATE(setChildren(nch-1))
nch=nch-1
setChildren=getChildren(1:nch)
NULLIFY(getChildren)
CALL testXMLFile%root(1)%setChildren(setChildren)

!Check that the set worked
CALL testXMLFile%root(1)%getChildren(getChildren)
nchComp=SIZE(getChildren)
bool=(nch == nchComp)
ASSERT(bool,'set Children')

!Test set attribute
attr_name='name'
refval='testName'
CALL setChildren(1)%setAttribute(attr_name,refval)
CALL setChildren(1)%getAttributeValue(attr_name,val)
bool=(TRIM(refval) == TRIM(val))
ASSERT(bool,'set existing attribute')

attr_name='testSet'
refval='testvalue'
CALL setChildren(1)%setAttribute(attr_name,refval)

CALL setChildren(1)%getAttributeValue(attr_name,val)
bool=(TRIM(refval) == TRIM(val))
ASSERT(bool,'set non-existing attribute')

attr_name='name'
refval='testName'
CALL setChildren(1)%getAttributeValue(attr_name,val)
bool=(TRIM(refval) == TRIM(val))
ASSERT(bool,'set non-existing attribute')

NULLIFY(getChildren)
ENDSUBROUTINE testSet
!
ENDPROGRAM testXMLFileType
