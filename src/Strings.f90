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
!> It then provides overloaded operators and intrinsic functions so
!> that it can be used almost exactly like a Fortran @c CHARACTER type variable.
!>
!> The following operators have been overloaded for use with @c StringType
!>  - @c ASSIGNMENT(=)
!>  - @c OPERATOR(//)
!>  - @c OPERATOR(==)
!>  - @c OPERATOR(/=)
!>
!> The following intrinsic Fortran functions have been overloaded for use with
!> @c StringType
!>  - @ref Strings::LEN_StringType "LEN": @copybrief Strings::LEN_StringType
!>  - @ref Strings::LEN_TRIM_StringType "LEN_TRIM": @copybrief Strings::LEN_TRIM_StringType
!>  - @ref Strings::TRIM_StringType "TRIM": @copybrief Strings::TRIM_StringType
!>  - @ref Strings::ADJUSTL_StringType "ADJUSTL": @copybrief Strings::ADJUSTL_StringType
!>  - @ref Strings::ADJUSTR_StringType "ADJUSTR": @copybrief Strings::ADJUSTR_StringType
!>  - @c INDEX
!>  - @c ANY
!>  - @c ALL
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
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
!>   WRITE(*,*) '"'//CHAR(myString)//'"'
!>   WRITE(*,*) '"'//TRIM(myString)//'"'
!>   WRITE(*,*) '"'//ADJUSTL(myString)//'"'
!>   WRITE(*,*) '"'//ADJUSTR(myString)//'"'
!>   WRITE(*,*) '"'//LEN(myString)//'"'
!>   WRITE(*,*) '"'//LEN_TRIM(myString)//'"'
!>
!>   myString=' hello world'
!>   char20=myString
!>   WRITE(*,*) '"'//CHAR(myString)//'"'
!>   WRITE(*,*) '"'//char20//'"'
!>   WRITE(*,*) '"'//TRIM(CHAR(myString))//'"'
!>   WRITE(*,*) '"'//TRIM(char20)//'"'
!>   WRITE(*,*) '"'//ADJUSTL(CHAR(myString))//'"'
!>   WRITE(*,*) '"'//ADJUSTL(char20)//'"'
!>   WRITE(*,*) '"'//ADJUSTR(myString)//'"'
!>   WRITE(*,*) '"'//ADJUSTR(char20)//'"'
!>   WRITE(*,*) '"'//LEN(myString)//'"'
!>   WRITE(*,*) '"'//LEN(char20)//'"'
!>   WRITE(*,*) '"'//LEN_TRIM(myString)//'"'
!>   WRITE(*,*) '"'//LEN_TRIM(char20)//'"'
!>
!>   myString=char20
!>   WRITE(*,*) '"'//CHAR(myString)//'"'
!>   WRITE(*,*) '"'//char20//'"'
!>   WRITE(*,*) '"'//TRIM(myString)//'"'
!>   WRITE(*,*) '"'//TRIM(char20)//'"'
!>   WRITE(*,*) '"'//ADJUSTL(myString)//'"'
!>   WRITE(*,*) '"'//ADJUSTL(char20)//'"'
!>   WRITE(*,*) '"'//ADJUSTR(myString)//'"'
!>   WRITE(*,*) '"'//ADJUSTR(char20)//'"'
!>   WRITE(*,*) '"'//LEN(myString)//'"'
!>   WRITE(*,*) '"'//LEN(char20)//'"'
!>   WRITE(*,*) '"'//LEN_TRIM(myString)//'"'
!>   WRITE(*,*) '"'//LEN_TRIM(char20)//'"'
!>
!> END PROGRAM
!> @endcode
!>
!> @author Brendan Kochunas
!>   @date 07/25/2012
!> @par Revisions:
!>   (10/25/2013) - Dan Jabaay
!>   - Overloaded Instrinsic ALL and ANY operators
!>   (05/18/2016) - Dan Jabaay
!>   - Added overloading of ALL for 1-D, 2-D, and 3-D arrays.
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Strings
  
  USE IntrType
  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: StringType
  PUBLIC :: CHAR
  PUBLIC :: LEN
  PUBLIC :: LEN_TRIM
  PUBLIC :: TRIM
  PUBLIC :: ADJUSTL
  PUBLIC :: ADJUSTR
  PUBLIC :: INDEX
  PUBLIC :: ANY
  PUBLIC :: ALL
  PUBLIC :: ASSIGNMENT(=)
  PUBLIC :: OPERATOR(//)
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(/=)
  
  !> Derived type for an arbitrary length string
  TYPE :: StringType
    !> The size of the string
    !>
    !> This is needed because SIZE(%s) does not reliably return 0 when
    !> s has been allocated and then unallocated.
    INTEGER(SIK) :: n=0
    !> The trim length of the string
    !>
    !> This is needed for the TRIM routine
    INTEGER(SIK) :: ntrim=0
    !> The string stored as an array of length 1 character strings
    CHARACTER(LEN=1),ALLOCATABLE :: s(:)
  ENDTYPE StringType
  
  !> @brief Overloads the Fortran intrinsic procedure CHAR() so
  !> a string type argument may be passed.
  INTERFACE CHAR
    !> @copybrief Strings::CHAR_StringType
    !> @copydetails Strings::CHAR_StringType
    MODULE PROCEDURE  CHAR_StringType
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic procedure LEN() so
  !> a string type argument may be passed.
  INTERFACE LEN
    !> @copybrief Strings::LEN_StringType
    !> @copydetails Strings::LEN_StringType
    MODULE PROCEDURE LEN_StringType
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic procedure LEN_TRIM() so
  !> a string type argument may be passed.
  INTERFACE LEN_TRIM
    !> @copybrief Strings::LEN_TRIM_StringType
    !> @copydetails Strings::LEN_TRIM_StringType
    MODULE PROCEDURE LEN_TRIM_StringType
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic procedure TRIM() so
  !> a string type argument may be passed.
  INTERFACE TRIM
    !> @copybrief Strings::TRIM_StringType
    !> @copydetails Strings::TRIM_StringType
    MODULE PROCEDURE TRIM_StringType
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic procedure ADJUSTL() so 
  !> a string type argument may be passed.
  INTERFACE ADJUSTL
    !> @copybrief Strings::ADJUSTL_StringType
    !> @copydetails Strings::ADJUSTL_StringType
    MODULE PROCEDURE ADJUSTL_StringType
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic procedure ADJUSTR() so 
  !> a string type argument may be passed.
  INTERFACE ADJUSTR
    !> @copybrief Strings::ADJUSTR_StringType
    !> @copydetails Strings::ADJUSTR_StringType
    MODULE PROCEDURE ADJUSTR_StringType
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic procedure INDEX() so
  !> string type arguments may be passed.
  INTERFACE INDEX
    !> @copybrief Strings::INDEX_StringType_char
    !> @copydetails Strings::INDEX_StringType_char
    MODULE PROCEDURE INDEX_StringType_char
    !> @copybrief Strings::INDEX_char_StringType
    !> @copydetails Strings::INDEX_char_StringType
    MODULE PROCEDURE INDEX_char_StringType
    !> @copybrief Strings::INDEX_StringType_StringType
    !> @copydetails Strings::INDEX_StringType_StringType
    MODULE PROCEDURE INDEX_StringType_StringType
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic procedure ANY() so 
  !> an array of string type arguments may be passed.
  INTERFACE ANY
    !> @copybrief Strings::ANY_StringTypeArray_char
    !> @copydetails Strings::ANY_StringTypeArray_char
    MODULE PROCEDURE ANY_StringTypeArray_char
    !> @copybrief Strings::ANY_StringTypeArray_StringType
    !> @copydetails Strings::ANY_StringTypeArray_StringType
    MODULE PROCEDURE ANY_StringTypeArray_StringType
    !> @copybrief Strings::ANY_char_StringTypeArray
    !> @copydetails Strings::ANY_char_StringTypeArray
    !MODULE PROCEDURE ANY_char_StringTypeArray
    !> @copybrief Strings::ANY_StringTypeStringTypeArray
    !> @copydetails Strings::ANY_StringTypeStringTypeArray
    !MODULE PROCEDURE ANY_StringTypeStringTypeArray
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic procedure ALL() so 
  !> an array of string type arguments may be passed.
  INTERFACE ALL
    !> @copybrief Strings::ALL_StringTypeArray_char
    !> @copydetails Strings::ALL_StringTypeArray_char
    MODULE PROCEDURE ALL_StringTypeArray_char
    !> @copybrief Strings::ALL_StringTypeArray_StringType
    !> @copydetails Strings::ALL_StringTypeArray_StringType
    MODULE PROCEDURE ALL_StringTypeArray_StringType
    !> @copybrief Strings::ALL_char_StringTypeArray
    !> @copydetails Strings::ALL_char_StringTypeArray
    !MODULE PROCEDURE ALL_char_StringTypeArray
    !!> @copybrief Strings::ALL_StringType_StringTypeArray
    !!> @copydetails Strings::ALL_StringType_StringTypeArray
    !MODULE PROCEDURE ALL_StringType_StringTypeArray
! at some point, add the ALL(1-D == scalar)...
    !> @copybrief Strings::ALL_StringType1a_StringType1a
    !> @copydetails Strings::ALL_StringType1a_StringType1a
    MODULE PROCEDURE ALL_StringType1a_StringType1a
    !> @copybrief Strings::ALL_StringType2a_StringType2a
    !> @copydetails Strings::ALL_StringType2a_StringType2a
    MODULE PROCEDURE ALL_StringType2a_StringType2a
    !> @copybrief Strings::ALL_StringType3a_StringType3a
    !> @copydetails Strings::ALL_StringType3a_StringType3a
    MODULE PROCEDURE ALL_StringType3a_StringType3a
  ENDINTERFACE
  
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
    !> @copybrief Strings::assign_StringType_to_StringType
    !> @copydetails Strings::assign_StringType_to_StringType
    MODULE PROCEDURE assign_StringType_to_StringType
    !> @copybrief Strings::assign_StringType1A_to_StringType1A
    !> @copydetails Strings::assign_StringType1A_to_StringType1A
    MODULE PROCEDURE assign_StringType1A_to_StringType1A
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic operator for concatenating
  !> character strings.
  INTERFACE OPERATOR(//)
    !> @copybrief Strings::concatenate_char_onto_StringType
    !> @copydetails Strings::concatenate_char_onto_StringType
    MODULE PROCEDURE concatenate_char_onto_StringType
    !> @copybrief Strings::concatenate_StringType_onto_char
    !> @copydetails Strings::concatenate_StringType_onto_char
    MODULE PROCEDURE concatenate_StringType_onto_char
    !> @copybrief Strings::concatenate_StringType_onto_StringType
    !> @copydetails Strings::concatenate_StringType_onto_StringType
    MODULE PROCEDURE concatenate_StringType_onto_StringType
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic operator for comparing
  !> two variables to see if they are equal
  INTERFACE OPERATOR(==)
    !> @copybrief Strings::equalto_char_StringType
    !> @copydetails Strings::equalto_char_StringType
    MODULE PROCEDURE equalto_char_StringType
    !> @copybrief Strings::equalto_StringType_char
    !> @copydetails Strings::equalto_StringType_char
    MODULE PROCEDURE equalto_StringType_char
    !> @copybrief Strings::equalto_StringType_StringType
    !> @copydetails Strings::equalto_StringType_StringType
    MODULE PROCEDURE equalto_StringType_StringType
  ENDINTERFACE
  
  !> @brief Overloads the Fortran intrinsic operator for comparing
  !> two variables to see if they are not equal
  INTERFACE OPERATOR(/=)
    !> @copybrief Strings::notequalto_char_StringType
    !> @copydetails Strings::notequalto_char_StringType
    MODULE PROCEDURE notequalto_char_StringType
    !> @copybrief Strings::notequalto_StringType_char
    !> @copydetails Strings::notequalto_StringType_char
    MODULE PROCEDURE notequalto_StringType_char
    !> @copybrief Strings::notequalto_StringType_StringType
    !> @copydetails Strings::notequalto_StringType_StringType
    MODULE PROCEDURE notequalto_StringType_StringType
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
    PURE FUNCTION CHAR_StringType(thisStr,stt,stp) RESULT(s)
      CLASS(StringType),INTENT(IN) :: thisStr
      INTEGER(SIK),INTENT(IN),OPTIONAL :: stt
      INTEGER(SIK),INTENT(IN),OPTIONAL :: stp
      CHARACTER(LEN=thisStr%n) :: s
      INTEGER(SIK) :: i,istt,istp,j
      s=''
      IF(thisStr%n > 0) THEN
        istt=1
        IF(PRESENT(stt)) istt=MAX(1,stt)
        istp=thisStr%n
        IF(PRESENT(stp)) istp=MIN(thisStr%n,stp)
        j=0
        DO i=istt,istp
          j=j+1
          s(j:j)=thisStr%s(i)
        ENDDO
      ENDIF
    ENDFUNCTION CHAR_StringType
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
      INTEGER(SIK) :: i
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
      s=ADJUSTL(CHAR(thisStr))
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
      s=ADJUSTR(CHAR(thisStr))
    ENDFUNCTION ADJUSTR_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the starting position of a @c CHARACTER substring within a 
!> @c StringType string.
!> @param string the @c StringType object in which @c substring is located
!> @param substring the @c CHARACTER string to locate within @c string
!> @param back a logical indicating whether or not to return the position of
!>        the first or last instance of the @c substring within @c string
!> @returns ipos the position in @c string of the @c substring
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c INDEX does for character variables.
!>
    ELEMENTAL FUNCTION INDEX_StringType_char(string,substring,back) RESULT(ipos)
      CLASS(StringType),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(IN) :: substring
      LOGICAL,INTENT(IN),OPTIONAL :: back
      INTEGER :: ipos
      ipos=INDEX(CHAR(string),substring,back)
    ENDFUNCTION INDEX_StringType_char
!
!-------------------------------------------------------------------------------
!> @brief Returns the starting position of a @c StringType substring within a 
!> @c CHARACTER string.
!> @param string the @c CHARACTER string in which @c substring is located
!> @param substring the @c StringType object to locate within @c string
!> @param back a logical indicating whether or not to return the position of
!>        the first or last instance of the @c substring within @c string
!> @returns ipos the position in @c string of the @c substring
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c INDEX does for character variables.
!>
    ELEMENTAL FUNCTION INDEX_char_StringType(string,substring,back) RESULT(ipos)
      CHARACTER(LEN=*),INTENT(IN) :: string  
      CLASS(StringType),INTENT(IN) :: substring
      LOGICAL,INTENT(IN),OPTIONAL :: back
      INTEGER :: ipos
      ipos=INDEX(string,CHAR(substring),back)
    ENDFUNCTION INDEX_char_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the starting position of a @c StringType substring within a 
!> @c StringType string.
!> @param string the @c StringType object in which @c substring is located
!> @param substring the @c StringType object to locate within @c string
!> @param back a logical indicating whether or not to return the position of
!>        the first or last instance of the @c substring within @c string
!> @returns ipos the position in @c string of the @c substring
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c INDEX does for character variables.
!>
    ELEMENTAL FUNCTION INDEX_StringType_StringType(string,substring,back) RESULT(ipos)
      CLASS(StringType),INTENT(IN) :: string
      CLASS(StringType),INTENT(IN) :: substring
      LOGICAL,INTENT(IN),OPTIONAL :: back
      INTEGER :: ipos
      ipos=INDEX(CHAR(string),CHAR(substring),back)
    ENDFUNCTION INDEX_StringType_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the boolean of comparison of all of the contents of the array 
!> of string types to character s where only one entry in the array need to 
!> equal s.
!> @param thisStr the array of string objects
!> @param s the character string to check against
!> @param not the optional argument to invert the logic of the operation
!> @returns bool the logical result of the operation
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c ANY does for character variables.
!>
    PURE FUNCTION ANY_StringTypeArray_char(thisStr,s,not) RESULT(bool)
      TYPE(StringType),INTENT(IN) :: thisStr(:)
      CHARACTER(LEN=*),INTENT(IN) :: s  
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: not
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i
      
      bool=.FALSE.
      IF(PRESENT(not)) bool=not
      DO i=1,SIZE(thisStr,DIM=1)
        IF(thisStr(i) == s) THEN
          bool=.TRUE.
          IF(PRESENT(not)) bool=.NOT.not
          EXIT
        ENDIF
      ENDDO
    ENDFUNCTION ANY_StringTypeArray_char
!
!-------------------------------------------------------------------------------
!> @brief Returns the boolean of comparison of all of the contents of the array 
!> of string types to string s where only one entry in the array need to 
!> equal s.
!> @param thisStr the array of string objects
!> @param s the string type to check against
!> @param not the optional argument to invert the logic of the operation
!> @returns bool the logical result of the operation
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c ANY does for character variables.
!>
    PURE FUNCTION ANY_StringTypeArray_StringType(thisStr,s,not) RESULT(bool)
      TYPE(StringType),INTENT(IN) :: thisStr(:)
      CLASS(StringType),INTENT(IN) :: s
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: not
      LOGICAL(SBK) :: bool
      
      IF(PRESENT(not)) THEN
        bool=ANY_StringTypeArray_char(thisStr,CHAR(s),not)
      ELSE
        bool=ANY_StringTypeArray_char(thisStr,CHAR(s))
      ENDIF
    ENDFUNCTION ANY_StringTypeArray_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the boolean of comparison of all of the contents of the array 
!> of string types to character s where only one entry in the array need to 
!> equal s.
!> @param thisStr the array of string objects
!> @param s the character string to check against
!> @param not the optional argument to invert the logic of the operation
!> @returns bool the logical result of the operation
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c ANY does for character variables.
!>
!    PURE FUNCTION ANY_char_StringTypeArray(s,thisStr,not) RESULT(bool)
!      CHARACTER(LEN=*),INTENT(IN) :: s  
!      TYPE(StringType),INTENT(IN) :: thisStr(:)
!      LOGICAL(SBK),INTENT(IN),OPTIONAL :: not
!      LOGICAL(SBK) :: bool
!      
!      IF(PRESENT(not)) THEN
!        bool=ANY_StringTypeArray_char(thisStr,s,not)
!      ELSE
!        bool=ANY_StringTypeArray_char(thisStr,s)
!      ENDIF
!    ENDFUNCTION ANY_char_StringTypeArray
!
!-------------------------------------------------------------------------------
!> @brief Returns the boolean of comparison of all of the contents of the array 
!> of string types to string s where only one entry in the array need to 
!> equal s.
!> @param thisStr the array of string objects
!> @param s the string type to check against
!> @param not the optional argument to invert the logic of the operation
!> @returns bool the logical result of the operation
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c ANY does for character variables.
!>
!    PURE FUNCTION ANY_StringTypeStringTypeArray(s,thisStr,not) RESULT(bool)
!      CLASS(StringType),INTENT(IN) :: s 
!      TYPE(StringType),INTENT(IN) :: thisStr(:)
!      LOGICAL(SBK),INTENT(IN),OPTIONAL :: not
!      LOGICAL(SBK) :: bool
!      
!      IF(PRESENT(not)) THEN
!        bool=ANY_StringTypeArray_char(thisStr,CHAR(s),not)
!      ELSE
!        bool=ANY_StringTypeArray_char(thisStr,CHAR(s))
!      ENDIF
!    ENDFUNCTION ANY_StringTypeStringTypeArray
!
!-------------------------------------------------------------------------------
!> @brief Returns the boolean of comparison of all of the contents of the array 
!> of string types to character s where all entries in the array need to 
!> equal s.
!> @param thisStr the array of string objects
!> @param s the character string to check against
!> @param not the optional argument to invert the logic of the operation
!> @returns bool the logical result of the operation
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c ANY does for character variables.
!>
    PURE FUNCTION ALL_StringTypeArray_char(thisStr,s,not) RESULT(bool)
      TYPE(StringType),INTENT(IN) :: thisStr(:)
      CHARACTER(LEN=*),INTENT(IN) :: s  
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: not
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i
      
      bool=.TRUE.
      IF(PRESENT(not)) bool=.NOT.not
      DO i=1,SIZE(thisStr,DIM=1)
        IF(thisStr(i) /= s) THEN
          bool=.FALSE.
          IF(PRESENT(not)) bool=not
          EXIT
        ENDIF
      ENDDO
    ENDFUNCTION ALL_StringTypeArray_char
!
!-------------------------------------------------------------------------------
!> @brief Returns the boolean of comparison of all of the contents of the array 
!> of string types to string type s where all entries in the array need to 
!> equal s.
!> @param thisStr the array of string objects
!> @param s the string type to check against
!> @param not the optional argument to invert the logic of the operation
!> @returns bool the logical result of the operation
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c ANY does for character variables.
!>
    PURE FUNCTION ALL_StringTypeArray_StringType(thisStr,s,not) RESULT(bool)
      TYPE(StringType),INTENT(IN) :: thisStr(:)
      CLASS(StringType),INTENT(IN) :: s 
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: not
      LOGICAL(SBK) :: bool
      
      IF(PRESENT(not)) THEN
        bool=ALL_StringTypeArray_char(thisStr,CHAR(s),not)
      ELSE
        bool=ALL_StringTypeArray_char(thisStr,CHAR(s))
      ENDIF
    ENDFUNCTION ALL_StringTypeArray_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the boolean of comparison of all of the contents of the array
!> of string types to string type s where all entries in the array need to
!> equal s.
!> @param thisStr the array of string objects
!> @param s the string type to check against
!> @param not the optional argument to invert the logic of the operation
!> @returns bool the logical result of the operation
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c ANY does for character variables.
!>
    PURE FUNCTION ALL_StringType1a_StringType1a(thisStr,s,not) RESULT(bool)
      TYPE(StringType),INTENT(IN) :: thisStr(:)
      TYPE(StringType),INTENT(IN) :: s(:)
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: not
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i

      bool=.FALSE.
      IF(SIZE(thisStr,DIM=1) == SIZE(s,DIM=1)) THEN
        bool=.TRUE.
        IF(PRESENT(not)) THEN
          IF(not) THEN
            DO i=1,SIZE(s,DIM=1)
              bool=bool .AND. (thisStr(i) /= s(i))
            ENDDO
          ELSE
            DO i=1,SIZE(s,DIM=1)
              bool=bool .AND. (thisStr(i) == s(i))
            ENDDO
          ENDIF
        ELSE
          DO i=1,SIZE(s,DIM=1)
            bool=bool .AND. (thisStr(i) == s(i))
          ENDDO
        ENDIF
      ENDIF
    ENDFUNCTION ALL_StringType1a_StringType1a
!
!-------------------------------------------------------------------------------
!> @brief Returns the boolean of comparison of all of the contents of the array
!> of string types to string type s where all entries in the array need to
!> equal s.
!> @param thisStr the array of string objects
!> @param s the string type to check against
!> @param not the optional argument to invert the logic of the operation
!> @returns bool the logical result of the operation
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c ANY does for character variables.
!>
    PURE FUNCTION ALL_StringType2a_StringType2a(thisStr,s,not) RESULT(bool)
      TYPE(StringType),INTENT(IN) :: thisStr(:,:)
      TYPE(StringType),INTENT(IN) :: s(:,:)
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: not
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,j

      bool=.FALSE.
      IF((SIZE(thisStr,DIM=1) == SIZE(s,DIM=1)) .AND. &
         (SIZE(thisStr,DIM=2) == SIZE(s,DIM=2))) THEN
        bool=.TRUE.
        IF(PRESENT(not)) THEN
          IF(not) THEN
            DO j=1,SIZE(s,DIM=2)
              DO i=1,SIZE(s,DIM=1)
                bool=bool .AND. (thisStr(i,j) /= s(i,j))
              ENDDO
            ENDDO
          ELSE
            DO j=1,SIZE(s,DIM=2)
              DO i=1,SIZE(s,DIM=1)
                bool=bool .AND. (thisStr(i,j) == s(i,j))
              ENDDO
            ENDDO
          ENDIF
        ELSE
          DO j=1,SIZE(s,DIM=2)
            DO i=1,SIZE(s,DIM=1)
              bool=bool .AND. (thisStr(i,j) == s(i,j))
            ENDDO
          ENDDO
        ENDIF
      ENDIF
    ENDFUNCTION ALL_StringType2a_StringType2a
!
!-------------------------------------------------------------------------------
!> @brief Returns the boolean of comparison of all of the contents of the array
!> of string types to string type s where all entries in the array need to
!> equal s.
!> @param thisStr the array of string objects
!> @param s the string type to check against
!> @param not the optional argument to invert the logic of the operation
!> @returns bool the logical result of the operation
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c ANY does for character variables.
!>
    PURE FUNCTION ALL_StringType3a_StringType3a(thisStr,s,not) RESULT(bool)
      TYPE(StringType),INTENT(IN) :: thisStr(:,:,:)
      TYPE(StringType),INTENT(IN) :: s(:,:,:)
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: not
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,j,k

      bool=.FALSE.
      IF((SIZE(thisStr,DIM=1) == SIZE(s,DIM=1)) .AND. &
         (SIZE(thisStr,DIM=2) == SIZE(s,DIM=2)) .AND. &
         (SIZE(thisStr,DIM=3) == SIZE(s,DIM=3))) THEN
        bool=.TRUE.
        IF(PRESENT(not)) THEN
          IF(not) THEN
            DO k=1,SIZE(s,DIM=3)
              DO j=1,SIZE(s,DIM=2)
                DO i=1,SIZE(s,DIM=1)
                  bool=bool .AND. (thisStr(i,j,k) /= s(i,j,k))
                ENDDO
              ENDDO
            ENDDO
          ELSE
            DO k=1,SIZE(s,DIM=3)
              DO j=1,SIZE(s,DIM=2)
                DO i=1,SIZE(s,DIM=1)
                  bool=bool .AND. (thisStr(i,j,k) == s(i,j,k))
                ENDDO
              ENDDO
            ENDDO
          ENDIF
        ELSE
          DO k=1,SIZE(s,DIM=3)
            DO j=1,SIZE(s,DIM=2)
              DO i=1,SIZE(s,DIM=1)
                bool=bool .AND. (thisStr(i,j,k) == s(i,j,k))
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
    ENDFUNCTION ALL_StringType3a_StringType3a
!
!-------------------------------------------------------------------------------
!> @brief Returns the boolean of comparison of all of the contents of the array 
!> of string types to character s where all entries in the array need to 
!> equal s.
!> @param thisStr the array of string objects
!> @param s the character string to check against
!> @param not the optional argument to invert the logic of the operation
!> @returns bool the logical result of the operation
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c ANY does for character variables.
!>
!    PURE FUNCTION ALL_char_StringTypeArray(s,thisStr,not) RESULT(bool)
!      CHARACTER(LEN=*),INTENT(IN) :: s  
!      TYPE(StringType),INTENT(IN) :: thisStr(:)
!      LOGICAL(SBK),INTENT(IN),OPTIONAL :: not
!      LOGICAL(SBK) :: bool
!      
!      IF(PRESENT(not)) THEN
!        bool=ALL_StringTypeArray_char(thisStr,s,not)
!      ELSE
!        bool=ALL_StringTypeArray_char(thisStr,s)
!      ENDIF
!    ENDFUNCTION ALL_char_StringTypeArray
!
!-------------------------------------------------------------------------------
!> @brief Returns the boolean of comparison of all of the contents of the array 
!> of string types to string type s where all entries in the array need to 
!> equal s.
!> @param thisStr the array of string objects
!> @param s the string type to check against
!> @param not the optional argument to invert the logic of the operation
!> @returns bool the logical result of the operation
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c ANY does for character variables.
!>
!    PURE FUNCTION ALL_StringType_StringTypeArray(s,thisStr,not) RESULT(bool)
!      CLASS(StringType),INTENT(IN) :: s 
!      TYPE(StringType),INTENT(IN) :: thisStr(:)
!      LOGICAL(SBK),INTENT(IN),OPTIONAL :: not
!      LOGICAL(SBK) :: bool
!      
!      IF(PRESENT(not)) THEN
!        bool=ALL_StringTypeArray_char(thisStr,CHAR(s),not)
!      ELSE
!        bool=ALL_StringTypeArray_char(thisStr,CHAR(s))
!      ENDIF
!    ENDFUNCTION ALL_StringType_StringTypeArray
!
!-------------------------------------------------------------------------------
!> @brief Assigns the contents of a string to an intrinsic character type
!> variable.
!> @param s the character string
!> @param thisStr the string object
!>
!> The intent is that this will overload the assignment operator so a
!> @c StringType can be assigned to a @c CHARACTER type.
!>
    PURE SUBROUTINE assign_StringType_to_char(s,thisStr)
      CHARACTER(LEN=*),INTENT(INOUT) :: s  
      CLASS(StringType),INTENT(IN) :: thisStr
      s=CHAR(thisStr)
    ENDSUBROUTINE assign_StringType_to_char
!
!-------------------------------------------------------------------------------
!> @brief Assigns the contents of an intrinsic character type variable to a
!> @c StringType.
!> @param thisStr the string object
!> @param s the character string
!>
!> The intent is that this will overload the assignment operator so a
!> @c CHARACTER type can be assigned to a @c StringType.
!>
    PURE SUBROUTINE assign_char_to_StringType(thisStr,s)
      CLASS(StringType),INTENT(INOUT) :: thisStr
      CHARACTER(LEN=*),INTENT(IN) :: s
      INTEGER(SIK) :: i
      
      IF(thisStr%n > 0) THEN
        IF(ALLOCATED(thisStr%s)) DEALLOCATE(thisStr%s)
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
!-------------------------------------------------------------------------------
!> @brief Assigns the contents of a @c StringType variable to a @c StringType.
!> @param thisStr the string object
!> @param s another string object
!>
!> The intent is that this will overload the assignment operator so a
!> @c Stringtype can be assigned to a @c StringType. This is used instead
!> of the intrinsic operation because I think there are some issues with
!> the allocatable component.
!>
    PURE SUBROUTINE assign_StringType_to_StringType(thisStr,s)
      CLASS(StringType),INTENT(INOUT) :: thisStr
      CLASS(StringType),INTENT(IN) :: s
      INTEGER(SIK) :: i
      
      IF(thisStr%n > 0) THEN
        DEALLOCATE(thisStr%s)
        thisStr%n=0
        thisStr%ntrim=0
      ENDIF
      
      IF(s%n > 0) THEN
        thisStr%n=s%n
        thisStr%ntrim=s%ntrim
        ALLOCATE(thisStr%s(thisStr%n))
        DO i=1,thisStr%n
          thisStr%s(i)=s%s(i)
        ENDDO
      ENDIF
    ENDSUBROUTINE assign_StringType_to_StringType
!
!-------------------------------------------------------------------------------
!> @brief Assigns the contents of a @c StringType variable to a @c StringType.
!> @param thisStr the string object
!> @param s another string object
!>
!> The intent is that this will overload the assignment operator so a
!> @c Stringtype can be assigned to a @c StringType. This is used instead
!> of the intrinsic operation because I think there are some issues with
!> the allocatable component.
!>
    PURE SUBROUTINE assign_StringType1A_to_StringType1A(thisStr,s)
      TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: thisStr(:)
      TYPE(StringType),INTENT(IN) :: s(:)
      INTEGER(SIK) :: i,j

      IF(ALLOCATED(thisStr)) THEN
        DO i=1,SIZE(thisStr,DIM=1)
          IF(thisStr(i)%n > 0) THEN
            DEALLOCATE(thisStr(i)%s)
            thisStr(i)%n=0
            thisStr(i)%ntrim=0
          ENDIF
        ENDDO
        DEALLOCATE(thisStr)
      ENDIF

      IF(SIZE(s) > 0) THEN
        ALLOCATE(thisStr(SIZE(s,DIM=1)))
        DO i=1,SIZE(s,DIM=1)
          IF(s(i)%n > 0) THEN
            thisStr(i)%n=s(i)%n
            thisStr(i)%ntrim=s(i)%ntrim
            ALLOCATE(thisStr(i)%s(thisStr(i)%n))
            DO j=1,thisStr(i)%n
              thisStr(i)%s(j)=s(i)%s(j)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
    ENDSUBROUTINE assign_StringType1A_to_StringType1A
!
!-------------------------------------------------------------------------------
!> @brief Concatenates an intrinsic character type variable with a
!> @c StringType.
!> @param thisStr the string object
!> @param s the length of the string
!> @returns newstring a character string that is a concatenation of a
!> @c StringType and character string
!>
!> The intent is that this will overload the // operator so a
!> @c CHARACTER type can be concatenated with a @c StringType.
!>
    PURE FUNCTION concatenate_StringType_onto_char(s,thisStr) RESULT(newstring)
      CHARACTER(LEN=*),INTENT(IN) :: s  
      CLASS(StringType),INTENT(IN) :: thisStr
      CHARACTER(LEN=thisStr%n+LEN(s)) :: newstring
      newstring=s//CHAR(thisStr)
    ENDFUNCTION concatenate_StringType_onto_char
!
!-------------------------------------------------------------------------------
!> @brief Concatenates an intrinsic character type variable with a
!> @c StringType.
!> @param s the length of the string
!> @param thisStr the string object
!> @returns newstring a character string that is a concatenation of a
!> @c StringType and character string
!>
!> The intent is that this will overload the // operator so a
!> @c CHARACTER type can be concatenated with a @c StringType.
!>
    PURE FUNCTION concatenate_char_onto_StringType(thisStr,s) RESULT(newstring)
      CLASS(StringType),INTENT(IN) :: thisStr
      CHARACTER(LEN=*),INTENT(IN) :: s
      CHARACTER(LEN=thisStr%n+LEN(s)) :: newstring
      newstring=CHAR(thisStr)//s
    ENDFUNCTION concatenate_char_onto_StringType
!
!-------------------------------------------------------------------------------
!> @brief Concatenates a @c StringType type variable with a @c StringType.
!> @param s1 the string object
!> @param s2 the string object
!> @returns newstring a character string that is a concatenation of a
!> two @c StringTypes
!>
!> The intent is that this will overload the // operator so a
!> @c StringType can be concatenated with a @c StringType.
!>
    PURE FUNCTION concatenate_StringType_onto_StringType(s1,s2) RESULT(s)
      CLASS(StringType),INTENT(IN) :: s1
      CLASS(StringType),INTENT(IN) :: s2
      CHARACTER(LEN=s1%n+s2%n) :: s
      s=CHAR(s1)//CHAR(s2)
    ENDFUNCTION concatenate_StringType_onto_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs an equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param s a @c CHARACTER type
!> @param thisStr a @c StringType object
!> @returns bool the result of the == operation
!>
!> The intent is that this will overload the == operator so a
!> @c CHARACTER type can compared with a @c StringType.
!>
    PURE FUNCTION equalto_char_StringType(s,thisStr) RESULT(bool)
      CHARACTER(LEN=*),INTENT(IN) :: s
      CLASS(StringType),INTENT(IN) :: thisStr
      LOGICAL(SBK) :: bool
      bool=(s == CHAR(thisStr))
    ENDFUNCTION equalto_char_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs an equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param thisStr a @c StringType object
!> @param s a @c CHARACTER type
!> @returns bool the result of the == operation
!>
!> The intent is that this will overload the == operator so a
!> @c CHARACTER type can compared with a @c StringType.
!>
    PURE FUNCTION equalto_StringType_char(thisStr,s) RESULT(bool)
      CLASS(StringType),INTENT(IN) :: thisStr
      CHARACTER(LEN=*),INTENT(IN) :: s
      LOGICAL(SBK) :: bool
      bool=(s == CHAR(thisStr))
    ENDFUNCTION equalto_StringType_char
!
!-------------------------------------------------------------------------------
!> @brief Performs an equal to operation of a @c StringType and a
!> @c StringType.
!> @param s1 a @c StringType object
!> @param s2 another @c StringType object
!> @returns bool the result of the == operation
!>
!> The intent is that this will overload the == operator so a
!> @c StringType type can compared with a @c StringType.
!>
    PURE FUNCTION equalto_StringType_StringType(s1,s2) RESULT(bool)
      CLASS(StringType),INTENT(IN) :: s1
      CLASS(StringType),INTENT(IN) :: s2
      LOGICAL(SBK) :: bool
      bool=(CHAR(s1) == CHAR(s2))
    ENDFUNCTION equalto_StringType_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs a not equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param s a @c CHARACTER type
!> @param thisStr a @c StringType object
!> @returns bool the result of the /= operation
!>
!> The intent is that this will overload the /= operator so a
!> @c CHARACTER type can compared with a @c StringType.
!>
    PURE FUNCTION notequalto_char_StringType(s,thisStr) RESULT(bool)
      CHARACTER(LEN=*),INTENT(IN) :: s
      CLASS(StringType),INTENT(IN) :: thisStr
      LOGICAL(SBK) :: bool
      bool=(s /= CHAR(thisStr))
    ENDFUNCTION notequalto_char_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs a not equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param thisStr a @c StringType object
!> @param s a @c CHARACTER type
!> @returns bool the result of the /= operation
!>
!> The intent is that this will overload the /= operator so a
!> @c CHARACTER type can compared with a @c StringType.
!>
    PURE FUNCTION notequalto_StringType_char(thisStr,s) RESULT(bool)
      CLASS(StringType),INTENT(IN) :: thisStr
      CHARACTER(LEN=*),INTENT(IN) :: s
      LOGICAL(SBK) :: bool
      bool=(s /= CHAR(thisStr))
    ENDFUNCTION notequalto_StringType_char
!
!-------------------------------------------------------------------------------
!> @brief Performs a not equal to operation of a @c StringType and a
!> @c StringType.
!> @param s1 a @c StringType object
!> @param s2 another @c StringType object
!> @returns bool the result of the /= operation
!>
!> The intent is that this will overload the /= operator so a
!> @c StringType type can compared with a @c StringType.
!>
    PURE FUNCTION notequalto_StringType_StringType(s1,s2) RESULT(bool)
      CLASS(StringType),INTENT(IN) :: s1
      CLASS(StringType),INTENT(IN) :: s2
      LOGICAL(SBK) :: bool
      bool=(CHAR(s1) /= CHAR(s2))
    ENDFUNCTION notequalto_StringType_StringType
!
ENDMODULE Strings
