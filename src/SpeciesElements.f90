
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
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref IO_Strings "IO_Strings": @copybrief IO_Strings
!>
!> @author Zack Taylor
!>   @date 01/24/19
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE SpeciesElements
#include "Futility_DBC.h"
  USE Futility_DBC
  USE IntrType
  USE ExceptionHandler
  USE IO_Strings
  USE ElementsIsotopes
  IMPLICIT NONE
  PRIVATE

  ! Member
  PUBLIC :: SpeciesElementsType

  ! List of capital letters
  CHARACTER(LEN=1) :: capitalList=(/'A','B','C','D','E','F','G','H','I','J',&
     'K','L','M','N','O','P','Q','R','S','T','U','P','W','X','Y','Z'/)
  ! List of lower case letters
  CHARACTER(LEN=1) :: lowerList=(/'a','b','c','d','e','f','g','h','i','j',&
     'k','l','m','n','o','p','q','r','s','t','u','p','w','x','y','z'/)

  !> Type that converts a given species chemical formula and molar amount
  !! to an array of molar amounts ordered by atomic number 
  TYPE :: SpeciesElementsType 
    !> Initialization status (not really needed)
    LOGICAL(SBK) :: isInit=.FALSE.


!List of type bound procedures
  CONTAINS
    PROCEDURE,PASS :: init
    PROCEDURE,PASS :: clear
    PROCEDURE,PASS,PRIVATE :: splitString
    PROCEDURE,PASS :: getElementArray
  ENDTYPE SpeciesElementsType

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Constructor for the species to elements converter
!> @param this the variable to initialize
!>
!> The constructor for the species to elements converter
!>
    SUBROUTINE init(this)
      CLASS(SpeciesElementsType),INTENT(INOUT) :: this
      
      this%isInit=.TRUE.
    ENDSUBROUTINE init


!
ENDMODULE SpeciesElementsType
