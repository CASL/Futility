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
!> @brief Module for specifying a type for arbitrary length strings
!> 
!> This module defines a type for a string which can be an arbitrary length.
!> It then provides overloaded assignment operators and intrinsic functions so
!> that it can be used almost exactly like a Fortran @c CHARACTER type variable.
!>
!> The following intrinsic Fortran functions can also be used with the 
!> @c StringType
!>  - @c LEN
!>  - @c LEN_TRIM
!>  - @c TRIM
!>  - @c ADJUSTL
!>  - @c ADJUSTR
!>
!> @par EXAMPLE
!> @code
!> PROGRAM
!>   USE Strings
!>   IMPLICIT NONE
!>   
!>   CHARACTER(LEN=20) :: char20
!>   TYPE(StringType) :: myString
!>
!>   WRITE(*,*) '"'//myString%sPrint()//'"'
!>   WRITE(*,*) '"'//TRIM(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//ADJUSTL(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//ADJUSTR(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//LEN(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//LEN_TRIM(myString%sPrint())//'"'
!>
!>   myString=' hello world'
!>   char20=myString
!>   WRITE(*,*) '"'//myString%sPrint()//'"'
!>   WRITE(*,*) '"'//char20//'"'
!>   WRITE(*,*) '"'//TRIM(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//TRIM(char20)//'"'
!>   WRITE(*,*) '"'//ADJUSTL(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//ADJUSTL(char20)//'"'
!>   WRITE(*,*) '"'//ADJUSTR(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//ADJUSTR(char20)//'"'
!>   WRITE(*,*) '"'//LEN(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//LEN(char20)//'"'
!>   WRITE(*,*) '"'//LEN_TRIM(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//LEN_TRIM(char20)//'"'
!>
!>   myString=char20
!>   WRITE(*,*) '"'//myString%sPrint()//'"'
!>   WRITE(*,*) '"'//char20//'"'
!>   WRITE(*,*) '"'//TRIM(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//TRIM(char20)//'"'
!>   WRITE(*,*) '"'//ADJUSTL(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//ADJUSTL(char20)//'"'
!>   WRITE(*,*) '"'//ADJUSTR(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//ADJUSTR(char20)//'"'
!>   WRITE(*,*) '"'//LEN(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//LEN(char20)//'"'
!>   WRITE(*,*) '"'//LEN_TRIM(myString%sPrint())//'"'
!>   WRITE(*,*) '"'//LEN_TRIM(char20)//'"'
!>
!> END PROGRAM
!> @endcode
!>
!> @author Brendan Kochunas
!>   @date 07/25/2012
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Strings
  
  USE IntrType
  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: StringType
  PUBLIC :: ASSIGNMENT(=)
  PUBLIC :: LEN
  PUBLIC :: LEN_TRIM
  PUBLIC :: TRIM
  PUBLIC :: ADJUSTL
  PUBLIC :: ADJUSTR
  
  !> Derived type for an arbitrary length string
  TYPE :: StringType
    !> The size of the string
    !>
    !> This is needed because SIZE(%s) does not reliably return 0 when
    !> s has been allocated and then unallocated.
    INTEGER(SIK),PRIVATE :: n=0
    !> The trim length of the string
    !>
    !> This is needed for the TRIM routine
    INTEGER(SIK),PRIVATE :: ntrim=0
    !> The string stored as an array of length 1 character strings
    CHARACTER(LEN=1),ALLOCATABLE,PRIVATE :: s(:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief Strings::length_StringType
      !> @copydetails Strings::length_StringType
      PROCEDURE,PASS :: sPrint => sPrint_StringType
  ENDTYPE StringType

  !> @brief Overloads the assignment operator.
  !>
  !> This is so string types can be assigned to characters and vice-versa
  INTERFACE ASSIGNMENT(=)
    !> @copybrief Strings::assign_char_to_StringType
    !> @copydetails Strings::assign_char_to_StringType
    MODULE PROCEDURE assign_char_to_StringType
    !> @copybrief Strings::assign_StringType_to_char
    !> @copydetails Strings::assign_StringType_to_char
    MODULE PROCEDURE assign_StringType_to_char
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic procedure LEN() so a 
  !> a string type argument may be passed.
  INTERFACE LEN
    !> @copybrief Strings::LEN_StringType
    !> @copydetails Strings::LEN_StringType
    MODULE PROCEDURE LEN_StringType
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic procedure LEN_TRIM() so a 
  !> a string type argument may be passed.
  INTERFACE LEN_TRIM
    !> @copybrief Strings::LEN_TRIM_StringType
    !> @copydetails Strings::LEN_TRIM_StringType
    MODULE PROCEDURE LEN_TRIM_StringType
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic procedure TRIM() so a 
  !> a string type argument may be passed.
  INTERFACE TRIM
    !> @copybrief Strings::TRIM_StringType
    !> @copydetails Strings::TRIM_StringType
    MODULE PROCEDURE TRIM_StringType
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic procedure ADJUSTL() so a 
  !> a string type argument may be passed.
  INTERFACE ADJUSTL
    !> @copybrief Strings::ADJUSTL_StringType
    !> @copydetails Strings::ADJUSTL_StringType
    MODULE PROCEDURE ADJUSTL_StringType
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic procedure ADJUSTR() so a 
  !> a string type argument may be passed.
  INTERFACE ADJUSTR
    !> @copybrief Strings::ADJUSTR_StringType
    !> @copydetails Strings::ADJUSTR_StringType
    MODULE PROCEDURE ADJUSTR_StringType
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Returns the length of the string including all whitespace.
!> @param thisStr the string object
!> @returns n the length of the string
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c LEN does for character variables.
!>
    ELEMENTAL FUNCTION LEN_StringType(thisStr) RESULT(n)
      CLASS(StringType),INTENT(IN) :: thisStr
      INTEGER(SIK) :: n
      n=thisStr%n
    ENDFUNCTION LEN_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the length of the string excluding trailing whitespace.
!> @param thisStr the string object
!> @returns n the length of the string
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c LEN_TRIM does for character variables.
!>
    ELEMENTAL FUNCTION LEN_TRIM_StringType(thisStr) RESULT(n)
      CLASS(StringType),INTENT(IN) :: thisStr
      INTEGER(SIK) :: n
      n=thisStr%ntrim
    ENDFUNCTION LEN_TRIM_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the contents of the string as an intrinsic character type
!> variable.
!> @param thisStr the string object
!> @returns s the character type with the value of @c thisStr
!>
!> This routine basically converts a @c StringType to a @c CHARACTER type.
!>
    PURE FUNCTION sPrint_StringType(thisStr) RESULT(s)
      CLASS(StringType),INTENT(IN) :: thisStr
      CHARACTER(LEN=thisStr%n) :: s
      INTEGER(SIK) :: i,n
      s=''
      IF(thisStr%n > 0) THEN
        DO i=1,thisStr%n
          s(i:i)=thisStr%s(i)
        ENDDO
      ENDIF
    ENDFUNCTION sPrint_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the contents of the string excluding all trailing whitespace
!> as an intrinsic character type variable.
!> @param thisStr the string object
!> @returns s the character string
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c TRIM does for character variables.
!>
    PURE FUNCTION TRIM_StringType(thisStr) RESULT(s)
      CLASS(StringType),INTENT(IN) :: thisStr
      CHARACTER(LEN=thisStr%ntrim) :: s
      INTEGER(SIK) :: i,n
      s=''
      IF(thisStr%ntrim > 0) THEN
        DO i=1,thisStr%ntrim
          s(i:i)=thisStr%s(i)
        ENDDO
      ENDIF
    ENDFUNCTION TRIM_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the contents of the string as an intrinsic character type
!> variable with all preceding whitespace moved to the end.
!> @param thisStr the string object
!> @returns s the character string
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c ADJUSTL does for character variables.
!>
    PURE FUNCTION ADJUSTL_StringType(thisStr) RESULT(s)
      CLASS(StringType),INTENT(IN) :: thisStr
      CHARACTER(LEN=thisStr%n) :: s
      s=ADJUSTL(thisStr%sPrint())
    ENDFUNCTION ADJUSTL_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the contents of the string as an intrinsic character type
!> variable with all trailing whitespace moved to the beginning.
!> @param thisStr the string object
!> @returns s the character string
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c ADJUSTR does for character variables.
!>
    PURE FUNCTION ADJUSTR_StringType(thisStr) RESULT(s)
      CLASS(StringType),INTENT(IN) :: thisStr
      CHARACTER(LEN=thisStr%n) :: s
      s=ADJUSTR(thisStr%sPrint())
    ENDFUNCTION ADJUSTR_StringType
!
!-------------------------------------------------------------------------------
!> @brief Assigns the contents of a string to an intrinsic character type
!> variable.
!> @param s the length of the string
!> @param thisStr the string object
!>
!> The intent is that this will overload the assignment operator so a
!> @c StringType can be assigned to a @c CHARACTER type.
!>
    PURE SUBROUTINE assign_StringType_to_char(s,thisStr)
      CHARACTER(LEN=*),INTENT(INOUT) :: s  
      CLASS(StringType),INTENT(IN) :: thisStr
      s=thisStr%sPrint()
    ENDSUBROUTINE assign_StringType_to_char
!
!-------------------------------------------------------------------------------
!> @brief Assigns the contents of an intrinsic character type variable to a
!> @c StringType.
!> @param thisStr the string object
!> @param s the length of the string
!>
!> The intent is that this will overload the assignment operator so a
!> @c CHARACTER type can be assigned to a @c StringType.
!>
    PURE SUBROUTINE assign_char_to_StringType(thisStr,s)
      CLASS(StringType),INTENT(INOUT) :: thisStr
      CHARACTER(LEN=*),INTENT(IN) :: s
      INTEGER(SIK) :: i
      
      IF(thisStr%n > 0) THEN
        DEALLOCATE(thisStr%s)
        thisStr%n=0
        thisStr%ntrim=0
      ENDIF
      
      IF(LEN(s) > 0) THEN
        thisStr%n=LEN(s)
        thisStr%ntrim=LEN_TRIM(s)
        ALLOCATE(thisStr%s(thisStr%n))
        DO i=1,thisStr%n
          thisStr%s(i)=s(i:i)
        ENDDO
      ENDIF
    ENDSUBROUTINE assign_char_to_StringType
!
ENDMODULE Strings
