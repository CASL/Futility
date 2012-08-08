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
  PUBLIC :: LEN
  PUBLIC :: LEN_TRIM
  PUBLIC :: TRIM
  PUBLIC :: ADJUSTL
  PUBLIC :: ADJUSTR
  PUBLIC :: INDEX
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
    PURE FUNCTION sPrint_StringType(thisStr,stt,stp) RESULT(s)
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
      IF(PRESENT(back)) THEN
        ipos=INDEX(string%sPrint(),substring,back)
      ELSE
        ipos=INDEX(string%sPrint(),substring)
      ENDIF
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
      IF(PRESENT(back)) THEN
        ipos=INDEX(string,substring%sPrint(),back)
      ELSE
        ipos=INDEX(string,substring%sPrint())
      ENDIF
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
      IF(PRESENT(back)) THEN
        ipos=INDEX(string%sPrint(),substring%sPrint(),back)
      ELSE
        ipos=INDEX(string%sPrint(),substring%sPrint())
      ENDIF
    ENDFUNCTION INDEX_StringType_StringType
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
      s=thisStr%sPrint()
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
      newstring=s//thisStr%sPrint()
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
      newstring=thisStr%sPrint()//s
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
      s=s1%sPrint()//s2%sPrint()
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
      bool=(s == thisStr%sPrint())
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
      bool=(s == thisStr%sPrint())
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
      bool=(s1%sPrint() == s2%sPrint())
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
      bool=(s /= thisStr%sPrint())
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
      bool=(s /= thisStr%sPrint())
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
      bool=(s1%sPrint() /= s2%sPrint())
    ENDFUNCTION notequalto_StringType_StringType
!
ENDMODULE Strings
