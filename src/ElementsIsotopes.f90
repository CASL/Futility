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
USE Strings
USE IO_Strings
IMPLICIT NONE
PRIVATE
!
! List of public members
PUBLIC :: getDecayType
PUBLIC :: isValidIsoName
PUBLIC :: isValidElemName
PUBLIC :: isValidIsoMass
PUBLIC :: getZAID
PUBLIC :: getZZZAAAI
PUBLIC :: getIsoName
PUBLIC :: getElementName
PUBLIC :: getAtomicNumber
PUBLIC :: getMassNumber
PUBLIC :: isMetastable
PUBLIC :: removeMetastable

INTERFACE getElementName
  MODULE PROCEDURE :: getElementName_ZAID_Z
  MODULE PROCEDURE :: getElementName_IsoStr
ENDINTERFACE getElementName

INTERFACE getAtomicNumber
  MODULE PROCEDURE :: getAtomicNumber_ZAID
  MODULE PROCEDURE :: getAtomicNumber_IsoStr_ElemStr
ENDINTERFACE getAtomicNumber

INTERFACE getMassNumber
  MODULE PROCEDURE :: getMassNumber_ZAID
  MODULE PROCEDURE :: getMassNumber_IsoStr
ENDINTERFACE getMassNumber

INTERFACE getDecayType
  !> @copybrief ElementsIsotopes::getDecayType_ZAID
  !> @copydetails ElementsIsotopes::getDecayType_ZAID
  MODULE PROCEDURE getDecayType_ZAID
  !Could be useful to add a "string" version too, but not needed right now
ENDINTERFACE getDecayType

! List of element names for string matching
TYPE(StringType),ALLOCATABLE :: elementList(:)
LOGICAL(SBK) :: listIsAlloc=.FALSE.

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
CHARACTER(LEN=*),PARAMETER :: modName='ElementsIsotopes'

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Performs allocation of internal list of elements
!>
SUBROUTINE setupElementString()
  INTEGER(SIK) :: i

  IF(.NOT.ALLOCATED(elementList)) THEN
    ALLOCATE(elementList(99))
    elementList = [' H','He','Li','Be',' B',' C',' N', &
        ' O',' F','Ne','Na','Mg','Al','Si',' P',' S','Cl','Ar',' K','Ca','Sc','Ti', &
        ' V','Cr','Mn','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr','Rb', &
        'Sr',' Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn','Sb','Te', &
        ' I','Xe','Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy','Ho', &
        'Er','Tm','Yb','Lu','Hf','Ta',' W','Re','Os','Ir','Pt','Au','Hg','Tl','Pb', &
        'Bi','Po','At','Rn','Fr','Ra','Ac','Th','Pa',' U','Np','Pu','Am','Cm','Bk','Cf','Es']
    DO i=1,99
      elementList(i)=TRIM(ADJUSTL(elementList(i)))
    ENDDO !i
    listIsAlloc=.TRUE.
  ENDIF

ENDSUBROUTINE setupElementString
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

  REQUIRE(source > 1000)
  REQUIRE(product > 1000)

  reaction=DECAY_NULL

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
  sourceZZ=getAtomicNumber(source)
  sourceAAA=getMassNumber(source)
  productZZ=getAtomicNumber(product)
  productAAA=getMassNumber(product)

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
!> @brief Routine returns a bool corresponding whether or not the provided
!>        isoName is a valid isotope name
!> @param isoName the name of the isotope such as "U-235" or "am-242m"
!> @returns isValid indicates if the isotope name is valid
!>
FUNCTION isValidIsoName(isoName) RESULT(isValid)
  CHARACTER(LEN=*),INTENT(IN) :: isoName
  LOGICAL(SBK) :: isValid

  isValid = (isValidElemName(isoName) .AND. isValidIsoMass(isoName))

ENDFUNCTION isValidIsoName
!
!-------------------------------------------------------------------------------
!> @brief Routine returns a bool corresponding whether or not the provided
!>        elemName is a valid element name
!> @param elemName the name of the element such as "U" or "am"
!> @returns isValid indicates if the element name is valid
!>
FUNCTION isValidElemName(elemName) RESULT(isValid)
  CHARACTER(LEN=*),INTENT(IN) :: elemName
  LOGICAL(SBK) :: isValid

  IF(.NOT.listIsAlloc) CALL setupElementString()

  isValid=(getAtomicNumber(elemName) > 0)

ENDFUNCTION isValidElemName
!
!-------------------------------------------------------------------------------
!> @brief Indicates if the isotope name has a valid mass number
!> @param isoName the isotope name
!>
!> Searches for `-###` or `-NAT` to see if the mass number is valid
!>
FUNCTION isValidIsoMass(isoName) RESULT(isValid)
  CHARACTER(LEN=*),INTENT(IN) :: isoName
  LOGICAL(SBK) :: isValid
  !
  INTEGER(SIK) :: A,i
  TYPE(StringType) :: tmpStr

  isValid = .FALSE.
  A = getMassNumber(isoName)
  IF(A == 0) THEN
    tmpStr=TRIM(ADJUSTL(removeMetastable(isoName)))
    i=INDEX(tmpStr%upper(),'-NAT')
    IF((i == 2 .OR. i == 3) .AND. i == LEN(tmpStr)-3) isValid=.TRUE.
  ELSE
    isValid=.TRUE.
  ENDIF

ENDFUNCTION isValidIsoMass
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the ZAID based on a specified isotope name
!> @param isoName the name of the isotope such as "U-235" or "am-242m"
!> @returns zaid the ZAID of the isotope
!>
FUNCTION getZAID(isoName) RESULT(zaid)
  CHARACTER(LEN=*),INTENT(IN) :: isoName
  INTEGER(SIK) :: zaid
  !
  TYPE(Stringtype) :: tmpStr

  REQUIRE(isValidIsoName(isoName))
  IF(.NOT.listIsAlloc) CALL setupElementString()

  tmpStr=TRIM(ADJUSTL(removeMetastable(isoName)))

  zaid=getAtomicNumber(isoName)*1000+getMassNumber(isoName)
ENDFUNCTION getZAID
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the ZZZAAAI based on a specified isotope name
!> @param isoName the name of the isotope such as "U-235" or "am-242m"
!> @returns ZZZAAAI the modified ZAID, with I being 1 for metastable, 0 otherwise
!>
FUNCTION getZZZAAAI(isoName) RESULT(ZZZAAAI)
  CHARACTER(LEN=*),INTENT(IN) :: isoName
  INTEGER(SIK) :: ZZZAAAI

  REQUIRE(isValidIsoName(isoName))
  IF(.NOT.listIsAlloc) CALL setupElementString()

  ZZZAAAI=getZAID(isoName)*10+MERGE(1,0,isMetastable(isoName))

ENDFUNCTION getZZZAAAI
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the isotope name based on user specified ZAID
!> @param zaid the integer representation of the atomic number and mass number: Z*1000+A
!> @returns isoName the isotope name
!>
FUNCTION getIsoName(zaid) RESULT(isoName)
  INTEGER(SIK),INTENT(IN) :: zaid
  CHARACTER(LEN=6) :: isoName
  !
  INTEGER(SIK) :: Z,A
  TYPE(StringType) :: massName

  REQUIRE(zaid>=1000)
  IF(.NOT.listIsAlloc) CALL setupElementString()

  Z=getAtomicNumber(zaid)
  A=getMassNumber(zaid)
  IF(A==0) THEN
    massName="NAT"
  ELSE
    massName=str(A)
  ENDIF
  IF(Z > SIZE(elementList)) THEN
    isoName=str(Z)//'-'//massName
  ELSE
    isoName=elementList(Z)//"-"//massName
  ENDIF

ENDFUNCTION getIsoName
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the element name based on specified atomic number or ZAID
!> @param id the zaid or the atomic number
!> @returns elemName the element name
!>
FUNCTION getElementName_ZAID_Z(id) RESULT(elemName)
  INTEGER(SIK),INTENT(IN) :: id
  CHARACTER(LEN=2) :: elemName

  REQUIRE(id>0)
  IF(.NOT.listIsAlloc) CALL setupElementString()

  IF(id>SIZE(elementList)) THEN
    elemName=CHAR(elementList(getAtomicNumber(id)))
  ELSE
    elemName=CHAR(elementList(id))
  ENDIF

ENDFUNCTION getElementName_ZAID_Z
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the element name based on specified isotope name
!> @param name the name of the element or isotope such as "U" or "am-242m"
!> @returns elemName the element name
!>
FUNCTION getElementName_IsoStr(name) RESULT(elemName)
  CHARACTER(LEN=*),INTENT(IN) :: name
  CHARACTER(LEN=:),ALLOCATABLE :: elemName
  !
  INTEGER(SIK) :: dashloc
  TYPE(StringType) :: tmpStr,tmpStr2

  REQUIRE(isValidIsoName(name) .OR. isValidElemName(name))

  dashloc=INDEX(name,'-')
  IF(dashloc==0) dashloc=LEN(name)+1
  tmpStr=TRIM(ADJUSTL(name(1:dashloc-1)))
  tmpStr=tmpStr%upper()
  ALLOCATE(CHARACTER(LEN=LEN(tmpSTr)) :: elemName)
  IF(LEN(tmpStr) == 2) THEN
    tmpStr2=tmpStr%substr(2,2)
    tmpStr=tmpStr%subStr(1,1)//tmpStr2%lower()
  ENDIF
  elemName=CHAR(tmpStr)
ENDFUNCTION getElementName_IsoStr
!
!-------------------------------------------------------------------------------
!> @brief Routine returns atomic number based on ZAID
!> @param zaid the integer representation of the atomic number and mass number: Z*1000+A
!> @returns Z the atomic number
!>
FUNCTION getAtomicNumber_ZAID(zaid) RESULT(Z)
  INTEGER(SIK),INTENT(IN) :: zaid
  INTEGER(SIK) :: Z

  REQUIRE(zaid>=1000)

  Z=ZAID/1000

ENDFUNCTION getAtomicNumber_ZAID
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the atomic number based on element or isotope name
!> @param name the name of the element or isotope such as "Xe" or "U-235"
!> @returns Z the atomic number
!>
FUNCTION getAtomicNumber_IsoStr_ElemStr(name) RESULT(Z)
  CHARACTER(LEN=*),INTENT(IN) :: name
  INTEGER(SIK) :: Z
  !
  INTEGER(SIK) :: i
  TYPE(StringType) :: elem

  IF(.NOT.listIsAlloc) CALL setupElementString()

  elem=TRIM(ADJUSTL(name))
  i=INDEX(elem,'-')
  IF(i > 0) THEN
    elem=elem%substr(1,i-1)
  ENDIF
  Z=-1
  DO i=1,SIZE(elementList)
    IF(elem%upper() == elementList(i)%upper()) THEN
      Z=i
      EXIT
    ENDIF
  ENDDO !i

ENDFUNCTION getAtomicNumber_IsoStr_ElemStr
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the mass number based on ZAID
!> @param zaid the integer representation of the atomic number and mass number: Z*1000+A
!> @returns A the mass number
!>
FUNCTION getMassNumber_ZAID(zaid) RESULT(A)
  INTEGER(SIK),INTENT(IN) :: zaid
  INTEGER(SIK) :: A

  REQUIRE(zaid>=1000)

  A=MOD(zaid,1000)
ENDFUNCTION getMassNumber_ZAID
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the mass number based on element or isotope name
!> @param isoName the name of the isotope such as "U-235" or "am-242m"
!> @returns A the mass number
!>
FUNCTION getMassNumber_IsoStr(isoName) RESULT(A)
  CHARACTER(LEN=*),INTENT(IN) :: isoName
  INTEGER(SIK) :: A
  !
  INTEGER(SIK) :: i
  TYPE(StringType) :: elem


  A=0
  elem=TRIM(ADJUSTL(removeMetastable(isoName)))
  i=INDEX(elem,'-')
  IF(i > 0) THEN
    elem=elem%substr(i+1)
    IF(elem%isInteger()) THEN
      A=elem%stoi()
    ENDIF
  ENDIF

ENDFUNCTION getMassNumber_IsoStr
!
!-------------------------------------------------------------------------------
!> @brief Routine returns logical if specified isotope name is for a metastable isotope
!> @param isoName the name of the isotope such as "U-235" or "am-242m"
!> @param isMeta logical indicating if the isotope is metastable or not
!>
FUNCTION isMetastable(isoName) RESULT(isMeta)
  CHARACTER(LEN=*),INTENT(IN) :: isoName
  LOGICAL(SBK) :: isMeta
  !
  TYPE(StringType) :: tmpStr


  tmpStr=TRIM(ADJUSTL(isoName))
  tmpStr=tmpStr%upper()

  isMeta=(INDEX(tmpStr,'-') > 0 .AND. tmpStr%substr(LEN(tmpStr)) == 'M')

ENDFUNCTION isMetastable
!
!-------------------------------------------------------------------------------
!> @brief Removes the "m" from a metastable isotope's string
!> @param isoName the isotope name to modify
!> @returns newName the modified isotope name
!>
!> If the isotope is not a metastable isotope, nothing is done
!>
FUNCTION removeMetastable(isoName) RESULT(newName)
  CHARACTER(LEN=*),INTENT(IN) :: isoName
  CHARACTER(LEN=:),ALLOCATABLE :: newName

  IF(isMetastable(isoname)) THEN
    newName=isoName(1:LEN_TRIM(isoName)-1)
  ELSE
    newName=isoName
  ENDIF

ENDFUNCTION removeMetastable
!
ENDMODULE ElementsIsotopes
