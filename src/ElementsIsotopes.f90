!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module for coverting different forms of elements and isotopes
!>
!> This package provides an interface to convert isotope and element character
!> strings to integer representations and back.  It also provides an interface
!> to determine if an isotope string is a metastable isotope.  The isotope
!> string has the atomic symbol and the mass number seperated by a "-" such
!> as "U-235".  Also, "NAT" can be used for natural isotopes and the mass number
!> would be 0.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE ElementsIsotopes
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE ExceptionHandler
USE IO_Strings
IMPLICIT NONE
PRIVATE
!
! List of public members
PUBLIC :: eElementsIsotopes
PUBLIC :: ElementsIsotopesType
PUBLIC :: getDecayType

INTERFACE getDecayType
  !> @copybrief ElementsIsotopes::getDecayType_ZAID
  !> @copydetails ElementsIsotopes::getDecayType_ZAID
  MODULE PROCEDURE getDecayType_ZAID
  !Could be useful to add a "string" version too, but not needed right now
ENDINTERFACE getDecayType

! List of element names for string matching
CHARACTER(LEN=2) :: elementlist(99)=(/' H','HE','LI','BE',' B',' C',' N', &
   ' O',' F','NE','NA','MG','AL','SI',' P',' S','CL','AR',' K','CA','SC','TI', &
   ' V','CR','MN','FE','CO','NI','CU','ZN','GA','GE','AS','SE','BR','KR','RB', &
   'SR',' Y','ZR','NB','MO','TC','RU','RH','PD','AG','CD','IN','SN','SB','TE', &
   ' I','XE','CS','BA','LA','CE','PR','ND','PM','SM','EU','GD','TB','DY','HO', &
   'ER','TM','YB','LU','HF','TA',' W','RE','OS','IR','PT','AU','HG','TL','PB', &
   'BI','PO','AT','RN','FR','RA','AC','TH','PA',' U','NP','PU','AM','CM','BK','CF','ES'/)

!> Invalid decay enumeration
INTEGER(SIK),PARAMETER,PUBLIC :: DECAY_NULL=0
!> Beta- to excited state
INTEGER(SIK),PARAMETER,PUBLIC :: DECAY_BETAMINUS_EXCITED=1
!> Beta+ to ground state
INTEGER(SIK),PARAMETER,PUBLIC :: DECAY_BETAPLUS_GROUND=2
!> Beta+ to excited state
INTEGER(SIK),PARAMETER,PUBLIC :: DECAY_BETAPLUS_EXCITED=3
!> Alpha decay
INTEGER(SIK),PARAMETER,PUBLIC :: DECAY_ALPHA=4
!> Isomeric decay (de-excitation)
INTEGER(SIK),PARAMETER,PUBLIC :: DECAY_ISOMERIC=5
!> Spontaneous fission
INTEGER(SIK),PARAMETER,PUBLIC :: DECAY_FISSION=6
!> beta+ + neutron
INTEGER(SIK),PARAMETER,PUBLIC :: DECAY_BETAPLUSNEUTRON=7
!> beta- to ground state
INTEGER(SIK),PARAMETER,PUBLIC :: DECAY_BETAMINUS_GROUND=8
!> Number of valid decay enumerations
INTEGER(SIK),PARAMETER,PUBLIC :: N_VALID_DECAY=8
!> List of all valid decay enumerations
INTEGER(SIK),PARAMETER,PUBLIC :: VALID_DECAY(N_VALID_DECAY)=(/ &
    DECAY_BETAMINUS_EXCITED,DECAY_BETAPLUS_GROUND,DECAY_BETAPLUS_EXCITED, &
    DECAY_ALPHA,DECAY_ISOMERIC,DECAY_FISSION,DECAY_BETAPLUSNEUTRON, &
    DECAY_BETAMINUS_GROUND/)

!> Name of module
CHARACTER(LEN=*),PARAMETER :: modName='ELEMENTS_ISOTOPES'

!> Type that contains converstion methods between elements and isotopes
TYPE :: ElementsIsotopesType
  !> Initialization status (not really needed)
  LOGICAL(SBK) :: isInit=.FALSE.
!
!List of type bound procedures
  CONTAINS
    !> @copybrief ElementsIsotopes::init_ElemIso
    !> @copydetails ElementsIsotopes::init_ElemIso
    PROCEDURE,PASS :: init => init_ElemIso
    !> @copybrief ElementsIsotopes::clear_ElemIso
    !> @copydetails ElementsIsotopes::clear_ElemIso
    PROCEDURE,PASS :: clear => clear_ElemIso
    !> @copybrief ElementsIsotopes::isValidIsoName_ElemIso
    !> @copydetails ElementsIsotopes::isValidIsoName_ElemIso
    PROCEDURE,PASS :: isValidIsoName => isValidIsoName_ElemIso
    !> @copybrief ElementsIsotopes::isValidElemName_ElemIso
    !> @copydetails ElementsIsotopes::isValidElemName_ElemIso
    PROCEDURE,PASS :: isValidElemName => isValidElemName_ElemIso
    !> @copybrief ElementsIsotopes::getZAID_ElemIso
    !> @copydetails ElementsIsotopes::getZAID_ElemIso
    PROCEDURE,PASS :: getZAID => getZAID_ElemIso
    !> @copybrief ElementsIsotopes::getZZZAAAI_ElemIso
    !> @copydetails ElementsIsotopes::getZZZAAAI_ElemIso
    PROCEDURE,PASS :: getZZZAAAI => getZZZAAAI_ElemIso
    !> @copybrief ElementsIsotopes::getIsoName_ElemIso
    !> @copydetails ElementsIsotopes::getIsoName_ElemIso
    PROCEDURE,PASS :: getIsoName => getIsoName_ElemIso
    !> @copybrief ElementsIsotopes::getElementName_ZAID_Z
    !> @copydetails ElementsIsotopes::getElementName_ZAID_Z
    PROCEDURE,PASS,PRIVATE :: getElementName_ZAID_Z
    !> @copybrief ElementsIsotopes::getElementName_IsoStr
    !> @copydetails ElementsIsotopes::getElementName_IsoStr
    PROCEDURE,PASS,PRIVATE :: getElementName_IsoStr
    !> Generic method to capture both getElementName methods
    GENERIC :: getElementName => getElementName_ZAID_Z,getElementName_IsoStr
    !> @copybrief ElementsIsotopes::getAtomicNumber_ZAID
    !> @copydetails ElementsIsotopes::getAtomicNumber_ZAID
    PROCEDURE,PASS,PRIVATE :: getAtomicNumber_ZAID
    !> @copybrief ElementsIsotopes::getAtomicNumber_IsoStr_ElemStr
    !> @copydetails ElementsIsotopes::getAtomicNumber_IsoStr_ElemStr
    PROCEDURE,PASS,PRIVATE :: getAtomicNumber_IsoStr_ElemStr
    !> Generic method to capture both getAtomicNumber methods
    GENERIC :: getAtomicNumber => getAtomicNumber_ZAID,getAtomicNumber_IsoStr_ElemStr
    !> @copybrief ElementsIsotopes::getMassNumber_ZAID
    !> @copydetails ElementsIsotopes::getMassNumber_ZAID
    PROCEDURE,PASS,PRIVATE :: getMassNumber_ZAID
    !> @copybrief ElementsIsotopes::getMassNumber_IsoStr
    !> @copydetails ElementsIsotopes::getMassNumber_IsoStr
    PROCEDURE,PASS,PRIVATE :: getMassNumber_IsoStr
    !> Generic method to capture both getMassNumber methods
    GENERIC :: getMassNumber => getMassNumber_ZAID,getMassNumber_IsoStr
    !> @copybrief ElementsIsotopes::isMetastable_ElemIso
    !> @copydetails ElementsIsotopes::isMetastable_ElemIso
    PROCEDURE,PASS :: isMetastable => isMetastable_ElemIso
ENDTYPE ElementsIsotopesType

!> Exception Handler for use in ElementsIsotopes
TYPE(ExceptionHandlerType),SAVE :: eElementsIsotopes
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Calculates the type of radioactive decay from one isotope to another
!> @param source the source isotope ZAID
!> @param product the product isotope ZAID
!> @param source_metastable logical indicating that the source is metastable; optional
!> @param product_metastable logical indicating that the product is metastable; optional
!> @returns reaction the enumeration for the reaction type
!>
!> Returns @c DECAY_NULL if no known reaction can be calculated.
!>
FUNCTION getDecayType_ZAID(source,product,source_metastable,product_metastable) RESULT(reaction)
  INTEGER(SIK),INTENT(IN) :: source
  INTEGER(SIK),INTENT(IN) :: product
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: source_metastable
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: product_metastable
  INTEGER(SIK) :: reaction
  !
  LOGICAL(SBK) :: lmetasource,lmetaproduct
  INTEGER(SIK) :: sourceZZ,productZZ,sourceAAA,productAAA
  TYPE(ElementsIsotopesType) :: elemIso

  REQUIRE(source > 1000)
  REQUIRE(product > 1000)

  reaction=DECAY_NULL
  CALL elemIso%init()

  lmetasource=.FALSE.
  lmetaproduct=.FALSE.
  IF(PRESENT(source_metastable)) lmetasource=source_metastable
  IF(PRESENT(product_metastable)) lmetaproduct=product_metastable

  !De-excitation - same ZAID, but source is metastable
  IF(lmetasource .AND. source == product) THEN
    reaction=DECAY_ISOMERIC
    RETURN
  ENDIF

  !Decompose ZAIDs to check for other reaction types
  sourceZZ=elemIso%getAtomicNumber(source)
  sourceAAA=elemIso%getMassNumber(source)
  productZZ=elemIso%getAtomicNumber(product)
  productAAA=elemIso%getMassNumber(product)

  !Beta+ - Same mass, one less proton
  IF(sourceZZ-1 == productZZ .AND. sourceAAA == productAAA) THEN
    IF(lmetaproduct) THEN
      reaction=DECAY_BETAPLUS_EXCITED
    ELSE
      reaction=DECAY_BETAPLUS_GROUND
    ENDIF
  !Beta- - Same mass, one more proton
  ELSEIF(sourceZZ+1 == productZZ .AND. sourceAAA == productAAA) THEN
    IF(lmetaproduct) THEN
      reaction=DECAY_BETAMINUS_EXCITED
    ELSE
      reaction=DECAY_BETAMINUS_GROUND
    ENDIF
  !Alpha - two fewer protons, two fewer neutrons
  ELSEIF(sourceZZ-2 == productZZ .AND. sourceAAA-4 == productAAA) THEN
    reaction=DECAY_ALPHA
  ENDIF

  ENSURE(reaction == DECAY_NULL .OR. ANY(reaction == VALID_DECAY))

ENDFUNCTION getDecayType_ZAID
!
!-------------------------------------------------------------------------------
!> @brief Constructor for the element and isotope converter
!> @param this the variable to initialize
!>
!> The constructor for the element and isotope converter
!>
SUBROUTINE init_ElemIso(this)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this

  this%isInit=.TRUE.
ENDSUBROUTINE init_ElemIso
!
!-------------------------------------------------------------------------------
!> @brief Routine clears the data in ElemIso type variable
!> @param this the type variable to clear
!>
SUBROUTINE clear_ElemIso(this)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this

  this%isInit=.FALSE.
ENDSUBROUTINE clear_ElemIso
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a bool corresponding whether or not the provided
!>        isoName is a valid isotope name
!> @param this the object
!> @param isoName the name of the isotope such as "U-235" or "am-242m"
!>
FUNCTION isValidIsoName_ElemIso(this,isoName) RESULT(isValid)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: isoName
  LOGICAL(SBK) :: isValid
  !
  CHARACTER(LEN=6) :: tmpChar
  INTEGER(SIK) :: dashloc,Z,A,ioerr


  isValid=.FALSE.
  tmpChar=TRIM(ADJUSTL(isoName))
  CALL toUpper(tmpChar)
  IF(this%isMetastable(tmpChar)) tmpChar=tmpChar(1:LEN_TRIM(tmpChar)-1)
  dashloc=INDEX(tmpChar,"-")
  IF(dashloc>1) THEN
    IF(dashloc==2) THEN
      Z=strarraymatchind(elementlist," "//tmpChar(1:1))
    ELSE
      Z=strarraymatchind(elementlist,tmpChar(dashloc-2:dashloc-1))
    ENDIF
    IF(Z>0) THEN
      IF(INDEX(tmpChar(dashloc+1:LEN(tmpChar)),"NAT")>0) THEN
        isValid=.TRUE.
      ELSE
        READ(tmpChar(dashloc+1:LEN(tmpChar)),*,IOSTAT=ioerr) A
        IF(ioerr == 0) isValid=.TRUE.
      ENDIF
    ENDIF
  ENDIF

ENDFUNCTION isValidIsoName_ElemIso
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a bool corresponding whether or not the provided
!>        elemName is a valid element name
!> @param this the object
!> @param elemName the name of the element such as "U" or "am"
!>
FUNCTION isValidElemName_ElemIso(this,elemName) RESULT(isValid)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: elemName
  LOGICAL(SBK) :: isValid
  !
  CHARACTER(LEN=2) :: tmpChar
  INTEGER(SIK) :: Z

  isValid=.FALSE.
  tmpChar=TRIM(ADJUSTL(elemName))
  CALL toUpper(tmpChar)
  Z=strarraymatchind(elementlist,ADJUSTR(tmpChar))
  IF(Z > 0) THEN
    isValid=.TRUE.
  ENDIF

ENDFUNCTION isValidElemName_ElemIso
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the ZAID based on a specified isotope name
!> @param this the object
!> @param isoName the name of the isotope such as "U-235" or "am-242m"
!>
FUNCTION getZAID_ElemIso(this,isoName) RESULT(zaid)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: isoName
  INTEGER(SIK) :: zaid
  !
  CHARACTER(LEN=6) :: tmpChar
  INTEGER(SIK) :: dashloc,Z,A

  REQUIRE(this%isInit)
  REQUIRE(this%isValidIsoName(isoName))

  tmpChar=TRIM(ADJUSTL(isoName))
  CALL toUpper(tmpChar)
  IF(this%isMetastable(tmpChar)) tmpChar=tmpChar(1:LEN_TRIM(tmpChar)-1)
  dashloc=INDEX(tmpChar,"-")
  IF(dashloc==2) THEN
    Z=strarraymatchind(elementlist," "//tmpChar(1:1))
  ELSE
    Z=strarraymatchind(elementlist,tmpChar(dashloc-2:dashloc-1))
  ENDIF
  IF(INDEX(tmpChar(dashloc+1:LEN(tmpChar)),"NAT")>0) THEN
    A=0
  ELSE
    READ(tmpChar(dashloc+1:LEN(tmpChar)),*) A
  ENDIF

  zaid=Z*1000+A
ENDFUNCTION getZAID_ElemIso
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the ZZZAAAI based on a specified isotope name
!> @param this the object
!> @param isoName the name of the isotope such as "U-235" or "am-242m"
!>
FUNCTION getZZZAAAI_ElemIso(this,isoName) RESULT(ZZZAAAI)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: isoName
  INTEGER(SIK) :: ZZZAAAI
  !
  CHARACTER(LEN=6) :: tmpChar
  INTEGER(SIK) :: dashloc,Z,A,i

  REQUIRE(this%isInit)
  REQUIRE(this%isValidIsoName(isoName))

  tmpChar=TRIM(ADJUSTL(isoName))
  CALL toUpper(tmpChar)
  IF(this%isMetastable(tmpChar)) THEN
    i=1
    tmpChar=tmpChar(1:LEN_TRIM(tmpChar)-1)
  ELSE
    i=0
  ENDIF
  dashloc=INDEX(tmpChar,"-")
  IF(dashloc==2) THEN
    Z=strarraymatchind(elementlist," "//tmpChar(1:1))
  ELSE
    Z=strarraymatchind(elementlist,tmpChar(dashloc-2:dashloc-1))
  ENDIF
  IF(INDEX(tmpChar(dashloc+1:LEN(tmpChar)),"NAT")>0) THEN
    A=0
  ELSE
    READ(tmpChar(dashloc+1:LEN(tmpChar)),*) A
  ENDIF

  ZZZAAAI=Z*10000+A*10+i
ENDFUNCTION getZZZAAAI_ElemIso
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the isotope name based on user specified ZAID
!> @param this the object
!> @param zaid the integer representation of the atomic number and mass number: Z*1000+A
!>
FUNCTION getIsoName_ElemIso(this,zaid) RESULT(isoName)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: zaid
  CHARACTER(LEN=6) :: isoName
  !
  CHARACTER(LEN=3) :: massName
  INTEGER(SIK) :: Z,A

  REQUIRE(this%isInit)
  REQUIRE(zaid>=1000)

  Z=this%getAtomicNumber(zaid)
  A=this%getMassNumber(zaid)
  IF(A==0) THEN
    massName="NAT"
  ELSE
    WRITE(massName,"(i3)") A
  ENDIF
  IF(Z > SIZE(elementlist)) THEN
    isoName=TRIM(ADJUSTL(str(Z)//'-'//ADJUSTL(massName)))
  ELSE
    isoName=TRIM(ADJUSTL(elementlist(Z)//"-"//ADJUSTL(massName)))
  ENDIF
ENDFUNCTION getIsoName_ElemIso
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the element name based on specified atomic number or ZAID
!> @param this the object
!> @param id the zaid or the atomic number
!>
FUNCTION getElementName_ZAID_Z(this,id) RESULT(elemName)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: id
  CHARACTER(LEN=2) :: elemName

  REQUIRE(this%isInit)
  REQUIRE(id>0)

  IF(id>SIZE(elementlist)) THEN
    elemName=TRIM(ADJUSTL(elementlist(this%getAtomicNumber(id))))
  ELSE
    elemName=TRIM(ADJUSTL(elementlist(id)))
  ENDIF
ENDFUNCTION getElementName_ZAID_Z
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the element name based on specified isotope name
!> @param this the object
!> @param name the name of the element or isotope such as "U" or "am-242m"
!>
FUNCTION getElementName_IsoStr(this,name) RESULT(elemName)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  CHARACTER(LEN=2) :: elemName
  !
  INTEGER(SIK) :: dashloc

  REQUIRE(this%isInit)
  REQUIRE(this%isValidIsoName(name) .OR. this%isValidElemName(name))

  dashloc=INDEX(name,'-')
  IF(dashloc==0) dashloc=LEN(name)+1
  elemName=TRIM(ADJUSTL(name(1:dashloc-1)))
  CALL toUpper(elemName)
ENDFUNCTION getElementName_IsoStr
!
!-------------------------------------------------------------------------------
!> @brief Routine returns atomic number based on ZAID
!> @param this the object
!> @param zaid the integer representation of the atomic number and mass number: Z*1000+A
!>
FUNCTION getAtomicNumber_ZAID(this,zaid) RESULT(Z)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: zaid
  INTEGER(SIK) :: Z

  REQUIRE(this%isInit)
  REQUIRE(zaid>=1000)

  Z=ZAID/1000
ENDFUNCTION getAtomicNumber_ZAID
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the atomic number based on element or isotope name
!> @param this the object
!> @param name the name of the element or isotope such as "Xe" or "U-235"
!>
FUNCTION getAtomicNumber_IsoStr_ElemStr(this,name) RESULT(Z)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SIK) :: Z
  !
  CHARACTER(LEN=2) ::Elem

  REQUIRE(this%isInit)

  IF(INDEX(name,'-')>0) THEN
    Z=this%getAtomicNumber(this%getZAID(name))
  ELSE
    Elem=name
    CALL toUpper(Elem)
    IF(LEN_TRIM(Elem) == 1) Elem=" "//TRIM(Elem)
    Z=strarraymatchind(elementlist,Elem)
  ENDIF
ENDFUNCTION getAtomicNumber_IsoStr_ElemStr
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the mass number based on ZAID
!> @param this the object
!> @param zaid the integer representation of the atomic number and mass number: Z*1000+A
!>
FUNCTION getMassNumber_ZAID(this,zaid) RESULT(A)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this
  INTEGER(SIK),INTENT(IN) :: zaid
  INTEGER(SIK) :: A

  REQUIRE(this%isInit)
  REQUIRE(zaid>=1000)

  A=MOD(zaid,1000)
ENDFUNCTION getMassNumber_ZAID
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the mass number based on element or isotope name
!> @param this the object
!> @param isoName the name of the isotope such as "U-235" or "am-242m"
!>
FUNCTION getMassNumber_IsoStr(this,isoName) RESULT(A)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: isoName
  INTEGER(SIK) :: A

  REQUIRE(this%isInit)

  A=this%getMassNumber(this%getZAID(isoName))
ENDFUNCTION getMassNumber_IsoStr
!
!-------------------------------------------------------------------------------
!> @brief Routine returns logical if specified isotope name is for a metastable isotope
!> @param this the object
!> @param isoName the name of the isotope such as "U-235" or "am-242m"
!>
FUNCTION isMetastable_ElemIso(this,isoName) RESULT(isMeta)
  CLASS(ElementsIsotopesType),INTENT(INOUT) :: this
  CHARACTER(LEN=*),INTENT(IN) :: isoName
  LOGICAL(SBK) :: isMeta
  !
  CHARACTER(LEN=7) :: tmpchar

  REQUIRE(this%isInit)

  tmpchar=TRIM(isoName)
  CALL toUpper(tmpchar)

  isMeta=(tmpchar(LEN_TRIM(tmpchar):LEN_TRIM(tmpchar))=='M')

ENDFUNCTION isMetastable_ElemIso
!
ENDMODULE ElementsIsotopes
