
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
!> This package provides an interface to convert a chemical formula into an 
!> input array for coupling with Thermochimica
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref Strings "Strings": @copybrief Strings
!>  - @ref ElementsIsotopes "ElementsIsotopes": @copybrief ElementsIsotopes
!>
!> @author Zack Taylor
!>   @date 01/24/19
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE SpeciesElements
  USE IntrType
  USE Strings
  USE ElementsIsotopes
  IMPLICIT NONE
  PRIVATE

  ! Member
  PUBLIC :: SpeciesElementsType

  !> Type that converts a given species chemical formula and molar amount
  !! to an array of molar amounts ordered by atomic number 
  TYPE :: SpeciesElementsType 

  !List of type bound procedures
  CONTAINS
    PROCEDURE,PASS :: getElementArray
    PROCEDURE,PASS,PRIVATE :: isChar
    PROCEDURE,PASS,PRIVATE :: isCharCap
    PROCEDURE,PASS,PRIVATE :: isCharLow
  ENDTYPE SpeciesElementsType

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Returns a 0:118 size array with the atomic number index containing
!> the stoichiometric coefficient in the input formula
!> @param formula chemical formula
!>
   FUNCTION getElementArray(this, formula) RESULT(eleArray)
      CLASS(SpeciesElementsType),INTENT(INOUT) :: this
      type(StringType), INTENT(IN) :: formula

      REAL(SDK), dimension(0:118) :: eleArray ! Return Value
      INTEGER(SIK) :: i, Z
      REAL(SDK) :: coeff
      CHARACTER(LEN=1) :: letter, nextLetter
      CHARACTER(LEN=2) :: element
      TYPE(ElementsIsotopesType) :: myEI

      eleArray = 0.0

      DO i=1, LEN(formula)
         coeff = 1.0
         ! Gets the value in the formula
         letter = CHAR(formula,i,i)
         ! Test to see if its a letter         
         IF (this%isChar(letter)) THEN
            ! Test to see if its capital
            IF (this%isCharCap(letter)) THEN
               element = letter
               IF (i /= LEN(formula)) THEN
                  ! Gets the next letter to see if its lower or upper.
                  nextLetter = CHAR(formula,i+1,i+1)
               ELSE
                  nextLetter = 'A'
               END IF
               ! If its lower case then it needs to be added to the 
               ! current letter
               IF (this%isCharLow(nextLetter)) THEN
                  element = letter // nextLetter
               END IF
            END IF
         ! If the character is lower case then do nothing
         ELSE IF (this%isCharLow(letter)) THEN
            CONTINUE 
         ! If not a letter then its a stoichiometrix coefficient
         ELSE
            coeff = letter
         END IF
         ! Gets the atomic number
         Z = myEI%getAtomicNumber(element)
         ! Sets the coeff in the array
         eleArray(Z) = coeff 
      END DO
   ENDFUNCTION  getElementArray
!
!-------------------------------------------------------------------------------
!> @brief Determins whether a character is in the alphabet using ASCII format
!> @param letter character to check
!>
   FUNCTION isChar(this, letter) RESULT(isValid)
      CLASS(SpeciesElementsType),INTENT(INOUT) :: this
      CHARACTER(LEN=*), INTENT(IN) :: letter

      LOGICAL(SBK) :: isValid
      INTEGER(SIK) :: val

      val = IACHAR(letter)

      isValid = .FALSE.
      ! Checks against the upper case letters
      IF (65<= val .AND. val<=90) THEN
         isValid = .TRUE.
      ! Checks against the lower case letters
      ELSE IF  (97<= val .AND. val<=122) THEN
         isValid = .TRUE.
      end if 

   ENDFUNCTION isChar 
!
!-------------------------------------------------------------------------------
!> @brief Determins whether a character is capitilized using ASCII format
!> @param letter character to check
!>
   FUNCTION isCharCap(this, letter) RESULT(isValid)
      CLASS(SpeciesElementsType),INTENT(INOUT) :: this
      CHARACTER(LEN=*), INTENT(IN) :: letter

      LOGICAL(SBK) :: isValid
      INTEGER(SIK) :: val

      val = IACHAR(letter)

      isValid = .FALSE.
      IF (65<= val .AND. val<=90) THEN
         isValid = .TRUE.
      END IF 

   ENDFUNCTION isCharCap
!
!-------------------------------------------------------------------------------
!> @brief Determins whether a character is lower case using ASCII format
!> @param letter character to check
!>
   FUNCTION isCharLow(this, letter) RESULT(isValid)
      CLASS(SpeciesElementsType),INTENT(INOUT) :: this
      CHARACTER(LEN=*), INTENT(IN) :: letter

      LOGICAL(SBK) :: isValid
      INTEGER(SIK) :: val

      val = IACHAR(letter)

      isValid = .FALSE.
      IF (97<= val .AND. val<=122) THEN
         isValid = .TRUE.
      END IF 

   ENDFUNCTION isCharLow
!
ENDMODULE SpeciesElements
