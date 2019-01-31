
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
!> This package provides an interface to convert a chemical formula into a
!> (0:118) size array.
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE SpeciesElementsModule
  USE IntrType
  USE Strings
  USE ElementsIsotopes
  IMPLICIT NONE
  PRIVATE

  !List of Public Members
  PUBLIC :: getElementArray

  !INTERFACE getElementArray
  !  !> @copybrief IO_Strings::stripComment_char
  !  !> @copydetails IO_Strings::stripComment_char
  !  MODULE PROCEDURE getElementArray
  !ENDINTERFACE getElementArray

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Returns a 0:118 size array with the atomic number index containing
!> the stoichiometric coefficient in the input formula
!> @param formula chemical formula
!>
   FUNCTION getElementArray(formula) RESULT(eleArray)
      TYPE(StringType), INTENT(IN) :: formula

      REAL(SRK), dimension(0:118) :: eleArray ! Return Value
      INTEGER(SIK) :: i, Z
      REAL(SRK) :: coeff
      CHARACTER(LEN=1) :: letter, nextLetter
      CHARACTER(LEN=2) :: element
      TYPE(ElementsIsotopesType) :: myEI

      eleArray = 0.0

      DO i=1, LEN(formula)
         coeff = 1.0
         ! Gets the value in the formula
         letter = CHAR(formula,i,i)
         ! Test to see if its a letter         
         IF (isChar(letter)) THEN
            ! Test to see if its capital
            IF (isCharCap(letter)) THEN
               element = letter
               IF (i /= LEN(formula)) THEN
                  ! Gets the next letter to see if its lower or upper.
                  nextLetter = CHAR(formula,i+1,i+1)
               ELSE
                  nextLetter = 'A'
               END IF
               ! If its lower case then it needs to be added to the 
               ! current letter
               IF (isCharLow(nextLetter)) THEN
                  element = letter // nextLetter
               END IF
            END IF
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
!> @brief Determines whether a character is in the alphabet using ASCII format
!> @param letter character to check
!>
   FUNCTION isChar(letter) RESULT(isValid)
      CHARACTER(LEN=1), INTENT(IN) :: letter

      LOGICAL(SBK) :: isValid

      isValid = isCharCap(letter) .OR. isCharLow(letter)

   ENDFUNCTION isChar 
!
!-------------------------------------------------------------------------------
!> @brief Determines whether a character is capitilized using ASCII format
!> @param letter character to check
!>
   FUNCTION isCharCap(letter) RESULT(isValid)
      CHARACTER(LEN=1), INTENT(IN) :: letter

      LOGICAL(SBK) :: isValid
      INTEGER(SIK) :: val

      val = IACHAR(letter)

      isValid = .FALSE.
      IF (65 <= val .AND. val <= 90) THEN
         isValid = .TRUE.
      END IF 

   ENDFUNCTION isCharCap
!
!-------------------------------------------------------------------------------
!> @brief Determines whether a character is lower case using ASCII format
!> @param letter character to check
!>
   FUNCTION isCharLow(letter) RESULT(isValid)
      CHARACTER(LEN=1), INTENT(IN) :: letter

      LOGICAL(SBK) :: isValid
      INTEGER(SIK) :: val

      val = IACHAR(letter)

      isValid = .FALSE.
      IF (97 <= val .AND. val <= 122) THEN
         isValid = .TRUE.
      END IF 

   ENDFUNCTION isCharLow
!
ENDMODULE SpeciesElementsModule
