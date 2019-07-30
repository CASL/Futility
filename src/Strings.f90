!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
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
  PRIVATE

! List of Public items
  PUBLIC :: StringType
  PUBLIC :: getSubstring
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
    !> 
    CHARACTER(LEN=:),ALLOCATABLE,PRIVATE :: s
    CONTAINS
      !> copybrief StringType::toupper_string
      !> copydetails StringType::toupper_string
      PROCEDURE,PASS :: upper => toupper_string
      !> copybrief StringType::toLower_string
      !> copydetails StringType::toLower_string
      PROCEDURE,PASS :: lower => toLower_string
      !> copybrief StringType::at_string
      !> copydetails StringType::at_string
      PROCEDURE,PASS :: at_string
      !> copybrief StringType::at_string_slice
      !> copydetails StringType::at_string_slice
      PROCEDURE,PASS :: at_string_slice
      GENERIC :: at => at_string, at_string_slice
      !> copybrief StringType::replace_slice
      !> copydetails StringType::replace_slice
      PROCEDURE,PASS :: replace => replace_slice
      !> copybrief StringType:: split_string
      !> copydetails StringType:: split_string
      PROCEDURE,PASS :: split =>  split_string
      !> copybrief StringType::assign_char_to_StringType
      !> copydetails StringType::assign_char_to_StringType
      PROCEDURE,PASS :: assign_char_to_StringType
      !> @copybrief Strings::assign_Short_Integer_to_StringType
      !> @copydetails Strings::assign_Short_Integer_to_StringType
      PROCEDURE assign_Short_Integer_to_StringType
      !> @copybrief Strings::assign_Long_Integer_to_StringType
      !> @copydetails Strings::assign_Long_Integer_to_StringType
      PROCEDURE assign_Long_Integer_to_StringType
      !> @copybrief Strings::assign_Single_Real_to_StringType
      !> @copydetails Strings::assign_Single_Real_to_StringType
      PROCEDURE assign_Single_Real_to_StringType
      !> @copybrief Strings::assign_Double_Real_to_StringType
      !> @copydetails Strings::assign_Double_Real_to_StringType
      PROCEDURE assign_Double_Real_to_StringType
      !> copybrief StringType::assign_Logical_to_StringType
      !> copydetails StringType::assign_Logical_to_StringType
      PROCEDURE assign_Logical_to_StringType
      GENERIC :: ASSIGNMENT(=) => assign_char_to_StringType, &
          assign_Logical_to_StringType,assign_Single_Real_to_StringType, &
          assign_Double_Real_to_StringType,assign_Short_Integer_to_StringType, &
          assign_Long_Integer_to_StringType
      !> copybrief StringType::str_to_sik
      !> copydetails StringType::str_to_sik
      PROCEDURE,PASS :: str_to_sik
      GENERIC :: stoi => str_to_sik
      !> copybrief StringType::str_to_srk
      !> copydetails StringType::str_to_srk
      PROCEDURE,PASS :: str_to_srk
      GENERIC :: stof => str_to_srk
      !> copybrief StringType::isInteger
      !> copydetails StringType::isInteger
      PROCEDURE,PASS :: isInteger
      !> copybrief StringType::isFloat
      !> copydetails StringType::isFloat
      PROCEDURE,PASS :: isFloat
      !> copybrief StringType::isNumeric_str
      !> copydetails StringType::isNumeric_str
      PROCEDURE,PASS :: isNumeric => isNumeric_str
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
  INTERFACE ASSIGNMENT(=)
    !> @copybrief Strings::assign_StringType_to_char
    !> @copydetails Strings::assign_StringType_to_char
    MODULE PROCEDURE assign_StringType_to_char
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
!> @brief Assigns an intrinsic character array to a string
!> @param lhs the string type receiving the characters
!> @param rhs the character string that will be assigned
!>
  ELEMENTAL SUBROUTINE assign_char_to_StringType(lhs,rhs)
    CLASS(StringType),INTENT(INOUT) :: lhs
    CHARACTER(LEN=*),INTENT(IN) :: rhs

    lhs%n=LEN(rhs)
    lhs%ntrim=LEN_TRIM(rhs)
    lhs%s = rhs
    
  ENDSUBROUTINE assign_char_to_StringType
!
!-------------------------------------------------------------------------------
!> @brief splits a StringType at specified delimiter and returns the substrings
!> in an array of StringTypes. If the delimiter is not found or the supplied
!> string is empty, the returned array is size 0 with an empty string type inside
!> @param this the string to be split
!> @param sub_str the returned array of substrings
!> @param delim the delimiter for split locations, OPTIONAL:default " "
!>
! The idea of this routine is to mirror the python .split() method as closely
! as possible, currently consecutive delimiters will be treated as one
  FUNCTION split_string(this,delim) RESULT(sub_str)
    CLASS(StringType),INTENT(IN) :: this
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: delim
    TYPE(StringType),ALLOCATABLE :: sub_str(:)
    !
    CHARACTER(LEN=:),ALLOCATABLE :: separator
    INTEGER(SIK) :: iSplit,nSplits,stt,stp,pos

    ! Use user supplied delimiter if provided. DEFAULT = " "
    IF(PRESENT(delim)) THEN
      ALLOCATE(CHARACTER(LEN(delim)) :: separator)
      separator = delim
    ELSE
      ALLOCATE(CHARACTER(1) :: separator)
      separator = " "
    ENDIF

    ! Count the number of splits
    nSplits = 0
    stt = 1
    stp = LEN(this%s)
    IF(stp < 1) THEN
      ALLOCATE(sub_str(0))
      RETURN
    ENDIF !stp < 1
    ! Search until the delimiter isn't found
    DO WHILE(INDEX(this%s(stt:stp),separator) > 0)
    ! If the index is greater than 1 then the first character must be a string
      IF(INDEX(this%s(stt:stp),separator) > 1) THEN
        ! Count the string
        nSplits = nSplits + 1
        ! Increment to the next delimiter
        stt = stt + INDEX(this%s(stt:stp),separator) + LEN(separator) - 1
      ELSE
        ! The first character was the delimiter so skip past it
        stt = stt + LEN(separator)
      ENDIF
    ENDDO
    ! If the string ends with a word, then it wasn't counted before
    IF(.NOT.(this%s(stp-LEN(separator)+1:stp) == separator)) THEN
      nSplits = nSplits + 1
    ENDIF
    ALLOCATE(sub_str(nSplits))

    ! Split along delimiters and store in the provided array
    stt = 1
    iSplit = 0
    DO WHILE(INDEX(this%s(stt:stp),separator) > 0)
      IF(INDEX(this%s(stt:stp),separator) > 1) THEN
        iSplit = iSplit + 1
        ! Strip out the string...subtract 2 (1 for exclusive, 1 for delimiter)
        sub_str(iSplit) = &
            this%s(stt:stt+INDEX(this%s(stt:stp),separator) - 2)
        stt = stt + INDEX(this%s(stt:stp),separator) + LEN(separator) - 1
      ELSE
        stt = stt + LEN(separator)
      ENDIF
    ENDDO
    ! If the string ends with a word, then it wasn't snatched out before
    IF(.NOT.(this%s(stp-LEN(separator)+1:stp) == separator)) THEN
      sub_str(nSplits) = &
          this%s(INDEX(this%s(1:stp),separator,.TRUE.)+LEN(separator):stp)
    ENDIF
  ENDFUNCTION split_string
!
!-------------------------------------------------------------------------------
!> @brief replaces a slice from a string with the designated character(s). If
!> the replacement characters are not the same size as the slice, the characters
!> are repeated until the slice is filled, any excess is truncated
!> @param this the string where the substitution is occurring
!> @param stt the start of the slice to be replaced
!> @param stp the end of the slice to be replaced
!> @param in_char the characters that will be subbed in
!>
  SUBROUTINE replace_slice(this,stt,stp,in_char)
    CLASS(StringType),INTENT(INOUT) :: this
    INTEGER(SIK),INTENT(IN) :: stt
    INTEGER(SIK),INTENT(IN) :: stp
    CHARACTER(LEN=*),INTENT(IN) :: in_char
    !
    CHARACTER(LEN=stp-stt+1) :: tmp_str
    INTEGER(SIK) :: full,part,step
      
    IF((stt <= stp) .AND. (stt > 0) .AND. (stp <= this%n)) THEN
      ! Find the number of characters being replaced
      step = stp-stt+1
      ! Find the number of full replacement strings required
      full = LEN(in_char)/step
      ! Find the length of any partial replacements
      part = MOD(LEN(in_char),step)
      
      !Construct the replacement string
      tmp_str(1:step) = " "
      IF(full > 0) THEN
        tmp_str = REPEAT(in_char,full)
      ENDIF
      IF(part > 0) THEN
        tmp_str = TRIM(tmp_str)//in_char(1:part)
      ENDIF

      ! Direct substitution of characters
      this%s(stt:stp) = tmp_str(1:step)
    ENDIF
    
  ENDSUBROUTINE replace_slice
!
!-------------------------------------------------------------------------------
!> @brief Returns the character at the index provided, if the index is out of
!> or the string is not allocated then the ascii null character is returned.
!> @param this the string being indexed into
!> @param pos the index the character should be taken from
!>
  FUNCTION at_string(this,pos) RESULT(letter)
    CLASS(StringType),INTENT(IN) :: this
    INTEGER(SIK),INTENT(IN) :: pos
    CHARACTER(LEN=1) :: letter

    IF((pos > 0) .AND. (pos <= this%n)) THEN
      letter = this%s(pos:pos)
    ELSE
      letter = CHAR(0)
    ENDIF
    
  ENDFUNCTION at_string
!
!-------------------------------------------------------------------------------
!> @brief Returns the characters in the slice indicated, if the either index is
!> out of range, then the ascii null character is returned
!> @param this the string being sliced
!> @param stt the first index of the slice
!> @param stp the last index of the slice
!>
  FUNCTION at_string_slice(this,stt,stp) RESULT(slice)
    CLASS(StringType),INTENT(IN) :: this
    INTEGER(SIK),INTENT(IN) :: stt
    INTEGER(SIK),INTENT(IN) :: stp
    CHARACTER(LEN=:),ALLOCATABLE :: slice
    !
    INTEGER(SIK) :: iCh,i

    IF((stt <= stp) .AND. (stt > 0) .AND. (stp <= this%n)) THEN
      slice = this%s(stt:stp)
    ELSE
      slice = CHAR(0)
    ENDIF
    
  ENDFUNCTION at_string_slice
!
!-------------------------------------------------------------------------------
!> @brief Utility function takes a string and converts all upper case letters to
!> lower case letters.
!> @param word input is a string, output has all lower case letters
!>
    PURE FUNCTION toLower_string(word) RESULT(upper_str)
      CLASS(StringType),INTENT(IN) :: word
      INTEGER(SIK) :: i
      !
      TYPE(StringType) :: upper_str
      upper_str = word
      DO i=1,LEN(word)
        IF('A' <= word%s(i:i) .AND. word%s(i:i) <= 'Z') &
          upper_str%s(i:i)=ACHAR(IACHAR(word%s(i:i))+32)
      ENDDO
    ENDFUNCTION toLower_string
!
!-------------------------------------------------------------------------------
!> @brief Utility function takes a string and converts all lower case letters to
!> upper case letters.
!> @param word input is a string, output has all upper case letters
!>
    PURE FUNCTION toupper_string(word) RESULT(lower_str)
      CLASS(StringType),INTENT(IN) :: word
      INTEGER(SIK) :: i
      !
      TYPE(StringType) :: lower_str
      lower_str = word
      DO i=1,LEN(word)
        IF('a' <= word%s(i:i) .AND. word%s(i:i) <= 'z') &
          lower_str%s(i:i)=ACHAR(IACHAR(word%s(i:i))-32)
      ENDDO
    ENDFUNCTION toupper_string
!
!-------------------------------------------------------------------------------
!> Returns a StringType object which is a section of the input StringType object
!> @param string the StringType object to get substring from
!> @param substring the substring stored as a StringType object
!> @param stt the index of input string at which the substring starts
!> @param stp the index of input string at which the substring stops
!>
    PURE SUBROUTINE getSubstring(string,substring,stt,stp)
      TYPE(StringType),INTENT(IN) :: string
      TYPE(StringType),INTENT(OUT) :: substring
      INTEGER(SIK),INTENT(IN) :: stt,stp
      INTEGER(SIK) :: i,sublen

      IF(stp >= stt) THEN
        IF(stp <= string%n) THEN
          IF(stt > 0) THEN
            sublen=stp-stt+1
            ALLOCATE(CHARACTER(sublen) :: substring%s)
            substring%n=sublen
            DO i=stt,stp
              substring%s(i-stt+1:i-stt+1)=string%s(i:i)
            ENDDO
            substring%ntrim=substring%n
            DO i=sublen,1,-1
              IF(substring%s(i:i) == ' ') THEN
                substring%ntrim=substring%ntrim-1
              ELSE
                EXIT
              ENDIF
            ENDDO
          ELSE
            substring=string
          ENDIF
        ELSE
          substring=string
        ENDIF
      ELSE
        substring=''
      ENDIF
    ENDSUBROUTINE getSubstring
!
!-------------------------------------------------------------------------------
!> @brief Returns the length of the string including all whitespace.
!> @param this the string object
!> @returns n the length of the string
!>
!> Should behave exactly as it would if called directly on a character array
    ELEMENTAL FUNCTION LEN_StringType(this) RESULT(n)
      CLASS(StringType),INTENT(IN) :: this
      INTEGER(SIK) :: n
      n=LEN(this%s)
    ENDFUNCTION LEN_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the length of the string excluding trailing whitespace.
!> @param this the string object
!> @returns n the length of the string
!>
!> Should behave exactly as it would if called directly on a character array
    ELEMENTAL FUNCTION LEN_TRIM_StringType(this) RESULT(n)
      CLASS(StringType),INTENT(IN) :: this
      INTEGER(SIK) :: n
      n=LEN_TRIM(this%s)
    ENDFUNCTION LEN_TRIM_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the contents of the string as an intrinsic character type
!> variable.
!> @param this the string object
!> @returns str the character type with the value of @c this
!>
!> This routine basically converts a @c StringType to a @c CHARACTER type.
!>
    PURE FUNCTION CHAR_StringType(this) RESULT(str)
      CLASS(StringType),INTENT(IN) :: this
      CHARACTER(LEN=:),ALLOCATABLE :: str
      
      IF(ALLOCATED(this%s)) THEN
        str = this%s
      ELSE
        ALLOCATE(CHARACTER(0) :: str)
      ENDIF
    ENDFUNCTION CHAR_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the contents of the string excluding all trailing whitespace
!> as an intrinsic character type variable.
!> @param this the string object
!> @returns str the character string
!>
!> The intent is that this behaves exactly the same way as the intrinsic
!> function @c TRIM does for character variables.
!>
    PURE FUNCTION TRIM_StringType(this) RESULT(str)
      CLASS(StringType),INTENT(IN) :: this
      CHARACTER(LEN=:),ALLOCATABLE :: str

      IF(ALLOCATED(this%s)) THEN
        str = TRIM(this%s)
      ELSE
        ALLOCATE(CHARACTER(0) :: str)
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
      s=ADJUSTL(thisStr%s)
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
      s=ADJUSTR(thisStr%s)
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
      ipos=INDEX(string%s,substring,back)
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
      ipos=INDEX(string,substring%s,back)
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
      ipos=INDEX(string%s,substring%s,back)
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
!> @param lhs the character string
!> @param rhs the string object
!>
!> The intent is that rhs will overload the assignment operator so a
!> @c StringType can be assigned to a @c CHARACTER type.
!>
    PURE SUBROUTINE assign_StringType_to_char(lhs,rhs)
      CHARACTER(LEN=:),ALLOCATABLE,INTENT(INOUT) :: lhs
      CLASS(StringType),INTENT(IN) :: rhs
      lhs = rhs%s
    ENDSUBROUTINE assign_StringType_to_char
!
!!-------------------------------------------------------------------------------
!!> @brief Assigns the contents of an intrinsic character type variable to a
!!> @c StringType.
!!> @param thisStr the string object
!!> @param s the character string
!!>
!!> The intent is that this will overload the assignment operator so a
!!> @c CHARACTER type can be assigned to a @c StringType.
!!>
!    PURE SUBROUTINE assign_char_array_to_StringType(thisStr,s)
!      CLASS(StringType),INTENT(INOUT) :: thisStr
!      CHARACTER,INTENT(IN) :: s(:)
!      INTEGER(SIK) :: i
!      !
!      CHARACTER(LEN=SIZE(s)) :: s2
!
!      DO i=1,SIZE(s)
!        s2(i:i)=s(i)
!      ENDDO !i
!      CALL assign_char_to_StringType(thisStr,s2)
!
!    ENDSUBROUTINE assign_char_array_to_StringType
!!
!!-------------------------------------------------------------------------------
!!> @brief Assigns the contents of a @c StringType variable to a @c StringType.
!!> @param thisStr the string object
!!> @param s another string object
!!>
!!> The intent is that this will overload the assignment operator so a
!!> @c Stringtype can be assigned to a @c StringType. This is used instead
!!> of the intrinsic operation because I think there are some issues with
!!> the allocatable component.
!!>
!    PURE SUBROUTINE assign_StringType_to_StringType(thisStr,s)
!      CLASS(StringType),INTENT(INOUT) :: thisStr
!      CLASS(StringType),INTENT(IN) :: s
!      INTEGER(SIK) :: i
!
!      IF(thisStr%n > 0) THEN
!        DEALLOCATE(thisStr%s)
!        thisStr%n=0
!        thisStr%ntrim=0
!      ENDIF
!
!      IF(s%n > 0) THEN
!        thisStr%n=s%n
!        thisStr%ntrim=s%ntrim
!        ALLOCATE(CHARACTER(thisStr%n) :: thisStr%s)
!        DO i=1,thisStr%n
!          thisStr%s(i:i)=s%s(i:i)
!        ENDDO
!      ENDIF
!    ENDSUBROUTINE assign_StringType_to_StringType
!!
!!-------------------------------------------------------------------------------
!!> @brief Assigns the contents of a @c StringType variable to a @c StringType.
!!> @param thisStr the string object
!!> @param s another string object
!!>
!!> The intent is that this will overload the assignment operator so a
!!> @c Stringtype can be assigned to a @c StringType. This is used instead
!!> of the intrinsic operation because I think there are some issues with
!!> the allocatable component.
!!>
!    PURE SUBROUTINE assign_char_to_StringType1A(thisStr,s)
!      TYPE(StringType),INTENT(INOUT) :: thisStr(:)
!      CHARACTER(LEN=*),INTENT(IN) :: s
!      INTEGER(SIK) :: i
!
!      DO i=1,SIZE(thisStr,DIM=1)
!        CALL assign_char_to_StringType(thisStr(i),s)
!      ENDDO
!    ENDSUBROUTINE assign_char_to_StringType1A
!!
!!-------------------------------------------------------------------------------
!!> @brief Assigns the contents of a @c StringType variable to a @c StringType.
!!> @param thisStr the string object
!!> @param s another string object
!!>
!!> The intent is that this will overload the assignment operator so a
!!> @c Stringtype can be assigned to a @c StringType. This is used instead
!!> of the intrinsic operation because I think there are some issues with
!!> the allocatable component.
!!>
!    PURE SUBROUTINE assign_char_to_StringType2A(thisStr,s)
!      TYPE(StringType),INTENT(INOUT) :: thisStr(:,:)
!      CHARACTER(LEN=*),INTENT(IN) :: s
!      INTEGER(SIK) :: i,j
!
!      DO j=1,SIZE(thisStr,DIM=2)
!        DO i=1,SIZE(thisStr,DIM=1)
!          CALL assign_char_to_StringType(thisStr(i,j),s)
!        ENDDO
!      ENDDO
!    ENDSUBROUTINE assign_char_to_StringType2A
!!
!!-------------------------------------------------------------------------------
!!> @brief Assigns the contents of a @c StringType variable to a @c StringType.
!!> @param lhs the string object
!!> @param rhs another string object
!!>
!!> The intent is that this will overload the assignment operator so a
!!> @c Stringtype can be assigned to a @c StringType. This is used instead
!!> of the intrinsic operation because I think there are some issues with
!!> the allocatable component.
!!>
!    PURE SUBROUTINE assign_StringType1A_to_StringType1A(lhs,rhs)
!      TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: lhs(:)
!      TYPE(StringType),INTENT(IN) :: rhs(:)
!      INTEGER(SIK) :: i,j
!
!      IF(ALLOCATED(lhs)) THEN
!        DO i=1,SIZE(lhs)
!          IF(ALLOCATED(lhs(i)%s)) THEN
!            DEALLOCATE(lhs(i)%s)
!            lhs(i)%n=0
!            lhs(i)%ntrim=0
!          ENDIF
!        ENDDO
!        DEALLOCATE(lhs)
!      ENDIF
!
!      IF(SIZE(rhs)>0) THEN
!        ALLOCATE(lhs(SIZE(rhs)))
!        DO i=1,SIZE(rhs)
!          IF(ALLOCATED(rhs(i)%s)) THEN
!            lhs(i)%n=rhs(i)%n
!            lhs(i)%ntrim=rhs(i)%ntrim
!            ALLOCATE(CHARACTER(lhs(i)%n) :: lhs(i)%s)
!            lhs(i)%s = rhs(i)%s
!          ENDIF
!        ENDDO
!      ELSE
!        ALLOCATE(lhs(0))
!      ENDIF
!    ENDSUBROUTINE assign_StringType1A_to_StringType1A
!!
!!-------------------------------------------------------------------------------
!!> @brief Assigns the contents of a @c StringType variable to a @c StringType.
!!> @param thisStr the string object
!!> @param s another string object
!!>
!!> The intent is that this will overload the assignment operator so a
!!> @c Stringtype can be assigned to a @c StringType. This is used instead
!!> of the intrinsic operation because I think there are some issues with
!!> the allocatable component.
!!>
!    PURE SUBROUTINE assign_StringType2A_to_StringType2A(thisStr,s)
!      TYPE(StringType),INTENT(INOUT) :: thisStr(:,:)
!      TYPE(StringType),INTENT(IN) :: s(:,:)
!      INTEGER(SIK) :: i,j,k,n,m
!
!      n=SIZE(thisStr,DIM=1)
!      m=SIZE(thisStr,DIM=2)
!      IF((n > 0) .AND. (m > 0))THEN
!        DO i=1,SIZE(thisStr,DIM=2)
!          DO j=1,SIZE(thisStr,DIM=1)
!            IF(thisStr(j,i)%n > 0) THEN
!              DEALLOCATE(thisStr(j,i)%s)
!              thisStr(j,i)%n=0
!              thisStr(j,i)%ntrim=0
!            ENDIF
!          ENDDO
!        ENDDO
!        !DEALLOCATE(thisStr)
!      ENDIF
!
!      IF((SIZE(s,DIM=1) > 0) .AND. (SIZE(s,DIM=2) > 0) .AND. &
!          (n == SIZE(s,DIM=1)) .AND. (m == SIZE(s,DIM=2))) THEN
!        !ALLOCATE(thisStr(SIZE(s,DIM=1)))
!        DO i=1,SIZE(s,DIM=2)
!          DO j=1,SIZE(s,DIM=1)
!            IF(s(j,i)%n > 0) THEN
!              thisStr(j,i)%n=s(j,i)%n
!              thisStr(j,i)%ntrim=s(j,i)%ntrim
!              ALLOCATE(CHARACTER(thisStr(j,i)%n) :: thisStr(j,i)%s)
!              DO k=1,thisStr(j,i)%n
!                thisStr(j,i)%s(k:k)=s(j,i)%s(k:k)
!              ENDDO
!            ENDIF
!          ENDDO
!        ENDDO
!      ENDIF
!    ENDSUBROUTINE assign_StringType2A_to_StringType2A
!
!-------------------------------------------------------------------------------
!> @brief Assigns a short integer to a StringType.
!> @param thisStr the string object
!> @param i short integer
!>
!> The intent is that this will overload the assignment operator so a short
!> (32-bit) integer can be assigned to a StringType.
!>
    ELEMENTAL SUBROUTINE assign_Short_Integer_to_StringType(thisStr,i)
      CLASS(StringType),INTENT(INOUT) :: thisStr
      INTEGER(SNK),INTENT(IN) :: i
      CHARACTER(LEN=11) :: l
      WRITE(l,'(i11)') i
      thisStr=TRIM(ADJUSTL(l))
    ENDSUBROUTINE assign_Short_Integer_to_StringType
!
!-------------------------------------------------------------------------------
!> @brief Assigns a long integer to a StringType.
!> @param thisStr the string object
!> @param i long integer
!>
!> The intent is that this will overload the assignment operator so a long
!> (64-bit) integer can be assigned to a StringType.
!>
    ELEMENTAL SUBROUTINE assign_Long_Integer_to_StringType(thisStr,i)
      CLASS(StringType),INTENT(INOUT) :: thisStr
      INTEGER(SLK),INTENT(IN) :: i
      CHARACTER(LEN=20) :: l
      WRITE(l,'(i20)') i
      thisStr=TRIM(ADJUSTL(l))
    ENDSUBROUTINE assign_Long_Integer_to_StringType
!
!-------------------------------------------------------------------------------
!> @brief Assigns a single real to a StringType.
!> @param thisStr the string object
!> @param r single real
!>
!> The intent is that this will overload the assignment operator so a single
!> (32-bit) real can be assigned to a StringType.
!>
    ELEMENTAL SUBROUTINE assign_Single_Real_to_StringType(thisStr,r)
      CLASS(StringType),INTENT(INOUT) :: thisStr
      REAL(SSK),INTENT(IN) :: r
      CHARACTER(LEN=15) :: l
      WRITE(l,'(es15.5)') r
      thisStr=TRIM(ADJUSTL(l))
    ENDSUBROUTINE assign_Single_Real_to_StringType
!
!-------------------------------------------------------------------------------
!> @brief Assigns a double real to a StringType.
!> @param thisStr the string object
!> @param r double real
!>
!> The intent is that this will overload the assignment operator so a double
!> (64-bit) real can be assigned to a StringType.
!>
    ELEMENTAL SUBROUTINE assign_Double_Real_to_StringType(thisStr,r)
      CLASS(StringType),INTENT(INOUT) :: thisStr
      REAL(SDK),INTENT(IN) :: r
      CHARACTER(LEN=22) :: l
      WRITE(l,'(es22.15)') r
      thisStr=TRIM(ADJUSTL(l))
    ENDSUBROUTINE assign_Double_Real_to_StringType
!
!-------------------------------------------------------------------------------
!> @brief Assigns a logical to a StringType.
!> @param this the string object
!> @param bool the logical to be written
!>
  SUBROUTINE assign_Logical_to_StringType(this,bool)
    CLASS(StringType),INTENT(INOUT) :: this
    LOGICAL(SBK),INTENT(IN) :: bool
    CHARACTER(LEN=1) :: tmp
    WRITE(tmp,'(L1)') bool
    this = tmp
  ENDSUBROUTINE assign_Logical_to_StringType
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
      IF(ALLOCATED(thisStr%s)) THEN
        newstring=thisStr%s//s
      ELSE
        newstring=s
      ENDIF
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
      s=(s1%s//s2%s)
    ENDFUNCTION concatenate_StringType_onto_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs an equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param s a @c CHARACTER type
!> @param rhs a @c StringType object
!> @returns bool the result of the == operation
!>
!> The intent is that this will overload the == operator so a
!> @c CHARACTER type can compared with a @c StringType.
!>
    ELEMENTAL FUNCTION equalto_char_StringType(s,rhs) RESULT(bool)
      CHARACTER(LEN=*),INTENT(IN) :: s
      CLASS(StringType),INTENT(IN) ::rhs 
      LOGICAL(SBK) :: bool
      bool = s == CHAR(rhs)
    ENDFUNCTION equalto_char_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs an equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param lhs a @c StringType object
!> @param s a @c CHARACTER type
!> @returns bool the result of the == operation
!>
!> The intent is that this will overload the == operator so a
!> @c CHARACTER type can compared with a @c StringType.
!>
    ELEMENTAL FUNCTION equalto_StringType_char(lhs,s) RESULT(bool)
      CLASS(StringType),INTENT(IN) ::lhs 
      CHARACTER(LEN=*),INTENT(IN) :: s
      LOGICAL(SBK) :: bool
        bool = s == CHAR(lhs)
    ENDFUNCTION equalto_StringType_char
!
!-------------------------------------------------------------------------------
!> @brief Performs an equal to operation of a @c StringType and a
!> @c StringType.
!> @param lhs a @c StringType object
!> @param s2 another @c StringType object
!> @returns bool the result of the == operation
!>
!> The intent is that this will overload the == operator so a
!> @c StringType type can compared with a @c StringType.
!>
    ELEMENTAL FUNCTION equalto_StringType_StringType(lhs,s2) RESULT(bool)
      CLASS(StringType),INTENT(IN) :: lhs 
      CLASS(StringType),INTENT(IN) :: s2
      LOGICAL(SBK) :: bool
      bool = CHAR(lhs) == CHAR(s2)
    ENDFUNCTION equalto_StringType_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs a not equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param s a @c CHARACTER type
!> @param rhs a @c StringType object
!> @returns bool the result of the /= operation
!>
!> The intent is that this will overload the /= operator so a
!> @c CHARACTER type can compared with a @c StringType.
!>
    ELEMENTAL FUNCTION notequalto_char_StringType(s,rhs) RESULT(bool)
      CHARACTER(LEN=*),INTENT(IN) :: s
      CLASS(StringType),INTENT(IN) :: rhs 
      LOGICAL(SBK) :: bool
      bool = s /= CHAR(rhs)
    ENDFUNCTION notequalto_char_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs a not equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param lhs a @c StringType object
!> @param s a @c CHARACTER type
!> @returns bool the result of the /= operation
!>
!> The intent is that this will overload the /= operator so a
!> @c CHARACTER type can compared with a @c StringType.
!>
    ELEMENTAL FUNCTION notequalto_StringType_char(lhs,s) RESULT(bool)
      CLASS(StringType),INTENT(IN) :: lhs
      CHARACTER(LEN=*),INTENT(IN) :: s
      LOGICAL(SBK) :: bool
      bool = s /= CHAR(lhs)
    ENDFUNCTION notequalto_StringType_char
!
!-------------------------------------------------------------------------------
!> @brief Performs a not equal to operation of a @c StringType and a
!> @c StringType.
!> @param lhs a @c StringType object
!> @param s2 another @c StringType object
!> @returns bool the result of the /= operation
!>
!> The intent is that this will overload the /= operator so a
!> @c StringType type can compared with a @c StringType.
!>
    ELEMENTAL FUNCTION notequalto_StringType_StringType(lhs,s2) RESULT(bool)
      CLASS(StringType),INTENT(IN) :: lhs
      CLASS(StringType),INTENT(IN) :: s2
      LOGICAL(SBK) :: bool
      bool = CHAR(lhs) /= CHAR(s2)
    ENDFUNCTION notequalto_StringType_StringType
!
!-------------------------------------------------------------------------------
!> @brief convert a string to an int
!> @param this the string being converted
!> @param i the integer that will be output
!> @param stt position for starting conversion
!> @param stp end of conversion
!>
  FUNCTION str_to_sik(this,stt,stp) RESULT(i)
    CLASS(StringType),INTENT(IN) :: this
    INTEGER(SIK),INTENT(IN),OPTIONAL :: stt
    INTEGER(SIK),INTENT(IN),OPTIONAL :: stp
    INTEGER(SIK) :: i
    !
    CHARACTER(LEN=:),ALLOCATABLE :: scratch

    IF(PRESENT(stt) .OR.  PRESENT(stp)) THEN
!      REQUIRE(PRESENT(stt) .AND. PRESENT(stp))
      scratch = this%s(stt:stp)
    ELSE
      scratch = this%s
    ENDIF

    IF(LEN(this%s) > 0) THEN
      READ(scratch,*) i
    ELSE
      i = -HUGE(i)
    ENDIF
  ENDFUNCTION str_to_sik
!
!-------------------------------------------------------------------------------
!> @brief convert a string to a float
!> @param this the string being converted
!> @param i the integer that will be output
!>
  FUNCTION str_to_srk(this) RESULT(i)
    CLASS(StringType),INTENT(IN) :: this
    REAL(SRK) :: i

    IF(LEN(this%s) > 0) THEN
      READ(this%s,*) i
    ELSE
      ! You should never do this, maybe it is better for this to be DBC'd
      i = -HUGE(i)
    ENDIF
  ENDFUNCTION str_to_srk
!
!-------------------------------------------------------------------------------
!> @brief decides if a string represents an integer
!> @param this the string being examined
!>
  FUNCTION isInteger(this) RESULT(bool)
    CLASS(StringType),INTENT(IN) :: this
    LOGICAL(SBK) :: bool
    INTEGER(SIK) :: i,stt

    stt = MAX(INDEX(this%s,'+'),INDEX(this%s,'-'))
    bool = isNumeric(this%s(stt+1:LEN(this%s)))

  ENDFUNCTION isInteger
!
!-------------------------------------------------------------------------------
!> @brief decides if a string represents an integer
!> @param this the string being examined
!>
  FUNCTION isFloat(this) RESULT(bool)
    CLASS(StringType),INTENT(IN) :: this
    LOGICAL(SBK) :: bool
    INTEGER(SIK) :: i
    INTEGER(SIK) :: stt,dec_pos,exp_pos,pos_neg_exp

    ! Look for '+' and '-'
    stt = MAX(INDEX(this%s,'+'),INDEX(this%s,'-'))
    ! Find the decimal if there is one
    dec_pos = INDEX(this%s,'.')
    ! Look for Currently acceptable exponents
    exp_pos = MAX(INDEX(this%s,'e'),INDEX(this%s,'E'),INDEX(this%s,'d'))
    pos_neg_exp = MAX(INDEX(this%s,'-'),INDEX(this%s,'+'))

    ! Enforce that a decimal is required in order to be a float
    IF(dec_pos > 0) THEN
      ! Check potential number leading up to decimal
      bool = isNumeric(this%s(stt+1:dec_pos-1))
      ! There is a possible exponent
      IF(exp_pos > 0) THEN
        ! Check number between decimal and exponent
        bool = bool .AND. isNumeric(this%s(dec_pos+1:exp_pos-1))
        ! Account for any sign on the exponent
        IF(pos_neg_exp > 0) THEN
          bool = bool .AND. isNumeric(this%s(pos_neg_exp+1:LEN(this%s)))
        ELSE
          bool = bool .AND. isNumeric(this%s(exp_pos+1:LEN(this%s)))
        ENDIF
      ELSE
        ! No exponent check decimal to end
        bool = bool .AND. isNumeric(this%s(dec_pos+1:LEN(this%s)))
      ENDIF
    ELSE
      ! If there isn't a decimal then it isn't a float
      bool = .FALSE.
    ENDIF

  ENDFUNCTION isFloat
!
!-------------------------------------------------------------------------------
!> @brief decides if a string represents a numeric value (either int or float)
!> @param this the string be examined
!> @param bool logical indicating decision
!>
  FUNCTION isNumeric_str(this) RESULT(bool)
    CLASS(StringType),INTENT(IN) :: this
    LOGICAL(SBK) :: bool
    INTEGER(SIK) :: dec_pos

    IF(this%isInteger()) THEN
      bool = .TRUE.
      RETURN
    ELSE
      bool = this%isFloat()
    ENDIF

  ENDFUNCTION isNumeric_str
!
ENDMODULE Strings
