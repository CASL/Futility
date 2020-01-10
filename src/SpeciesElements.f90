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
!> (1:119) size array.
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE SpeciesElementsModule
  USE IntrType
  USE Strings
  USE ElementsIsotopes
  USE IO_Strings
  IMPLICIT NONE
  PRIVATE

  !List of Public Members
  PUBLIC :: SpeciesElements_getElementArray

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Returns a 1:119 size array with the atomic number index containing
!> the stoichiometric coefficient in the input formula
!> @param formula chemical formula
!> @returns eleArray 1:119 size array  
!>
   FUNCTION SpeciesElements_getElementArray(formula) RESULT(eleArray)
      TYPE(StringType), INTENT(IN) :: formula

      REAL(SRK), DIMENSION(1:119) :: eleArray ! Return Value
      INTEGER(SIK) :: i, Z
      REAL(SRK) :: coeff
      CHARACTER(LEN=1) :: letter, nextLetter
      CHARACTER(LEN=2) :: element
      TYPE(ElementsIsotopesType) :: myEI
   
      CALL myEI%init()
      eleArray = 0.0

      DO i=1, LEN(formula)
         coeff = 1.0
         ! Gets the value in the formula
         letter = formula%at(i)
         ! Test to see if its a letter         
         IF (isChar(letter)) THEN
            ! Test to see if its capital
            IF (isCharCap(letter)) THEN
               element = letter
               IF (i /= LEN(formula)) THEN
                  ! Gets the next letter to see if its lower or upper.
                  nextLetter = formula%at(i+1)
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
   ENDFUNCTION  SpeciesElements_getElementArray
!
ENDMODULE SpeciesElementsModule
