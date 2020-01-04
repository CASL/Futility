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
  USE ISO_C_BINDING
  USE IntrType
  IMPLICIT NONE
  PRIVATE

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
    !> The stored intrinsic character string
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
      PROCEDURE,PASS :: at => at_string
      !> copybrief StringType::substr_str
      !> copydetails StringType::substr_str
      PROCEDURE,PASS :: substr => substr_str
      !> copybrief StringType::replace_slice
      !> copydetails StringType::replace_slice
      PROCEDURE,PASS,PRIVATE :: replace_slice
      !> copybrief StringType::replace_pattern
      !> copydetails StringType::replace_pattern
      PROCEDURE,PASS,PRIVATE :: replace_pattern
      GENERIC :: replace => replace_slice,replace_pattern
      !> copybrief StringType:: split_string
      !> copydetails StringType:: split_string
      PROCEDURE,PASS,PRIVATE :: split_string
      !> copybrief StringType::split_string_space
      !> copydetails StringType::split_string_space
      PROCEDURE,PASS,PRIVATE :: split_string_space
      GENERIC :: split => split_string, split_string_space
      !> copybrief StringType::assign_char_to_StringType
      !> copydetails StringType::assign_char_to_StringType
      PROCEDURE,PASS,PRIVATE :: assign_char_to_StringType
      !> @copybrief StringType::assign_Short_Integer_to_StringType
      !> @copydetails StringType::assign_Short_Integer_to_StringType
      PROCEDURE,PASS,PRIVATE :: assign_Short_Integer_to_StringType
      !> @copybrief StringType::assign_Long_Integer_to_StringType
      !> @copydetails StringType::assign_Long_Integer_to_StringType
      PROCEDURE,PASS,PRIVATE :: assign_Long_Integer_to_StringType
      !> @copybrief StringType::assign_Single_Real_to_StringType
      !> @copydetails StringType::assign_Single_Real_to_StringType
      PROCEDURE,PASS,PRIVATE :: assign_Single_Real_to_StringType
      !> @copybrief StringType::assign_Double_Real_to_StringType
      !> @copydetails StringType::assign_Double_Real_to_StringType
      PROCEDURE,PASS,PRIVATE :: assign_Double_Real_to_StringType
      !> copybrief StringType::assign_Logical_to_StringType
      !> copydetails StringType::assign_Logical_to_StringType
      PROCEDURE,PASS,PRIVATE :: assign_Logical_to_StringType
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
      !> copybrief StringType::clean_str
      !> copydetails StringType::clean_str
      PROCEDURE,PASS :: clear => clear_str
  ENDTYPE StringType

  !> @brief Overloads the Fortran intrinsic procedure CHAR() so
  !> a string type argument may be passed.
  INTERFACE CHAR
    !> @copybrief StringType::CHAR_StringType
    !> @copydetails StringType::CHAR_StringType
    MODULE PROCEDURE CHAR_StringType
    !> copybrief StringType::cchar_to_fchar
    !> copydetails StringType::cchar_to_fchar
    MODULE PROCEDURE cchar_to_fchar
  ENDINTERFACE

  !> @brief Overloads the Fortran intrinsic procedure LEN() so
  !> a string type argument may be passed.
  INTERFACE LEN
    !> @copybrief StringType::LEN_StringType
    !> @copydetails StringType::LEN_StringType
    MODULE PROCEDURE LEN_StringType
  ENDINTERFACE

  !> @brief Overloads the Fortran intrinsic procedure LEN_TRIM() so
  !> a string type argument may be passed.
  INTERFACE LEN_TRIM
    !> @copybrief StringType::LEN_TRIM_StringType
    !> @copydetails StringType::LEN_TRIM_StringType
    MODULE PROCEDURE LEN_TRIM_StringType
  ENDINTERFACE

  !> @brief Overloads the Fortran intrinsic procedure TRIM() so
  !> a string type argument may be passed.
  INTERFACE TRIM
    !> @copybrief StringType::TRIM_StringType
    !> @copydetails StringType::TRIM_StringType
    MODULE PROCEDURE TRIM_StringType
  ENDINTERFACE

  !> @brief Overloads the Fortran intrinsic procedure ADJUSTL() so
  !> a string type argument may be passed.
  INTERFACE ADJUSTL
    !> @copybrief StringType::ADJUSTL_StringType
    !> @copydetails StringType::ADJUSTL_StringType
    MODULE PROCEDURE ADJUSTL_StringType
  ENDINTERFACE

  !> @brief Overloads the Fortran intrinsic procedure ADJUSTR() so
  !> a string type argument may be passed.
  INTERFACE ADJUSTR
    !> @copybrief StringType::ADJUSTR_StringType
    !> @copydetails StringType::ADJUSTR_StringType
    MODULE PROCEDURE ADJUSTR_StringType
  ENDINTERFACE

  !> @brief Overloads the Fortran intrinsic procedure INDEX() so
  !> string type arguments may be passed.
  INTERFACE INDEX
    !> @copybrief StringType::INDEX_StringType_char
    !> @copydetails StringType::INDEX_StringType_char
    MODULE PROCEDURE INDEX_StringType_char
    !> @copybrief StringType::INDEX_char_StringType
    !> @copydetails StringType::INDEX_char_StringType
    MODULE PROCEDURE INDEX_char_StringType
    !> @copybrief StringType::INDEX_StringType_StringType
    !> @copydetails StringType::INDEX_StringType_StringType
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

  !> @brief Overloads the Fortran intrinsic operator for concatenating
  !> character strings.
  INTERFACE OPERATOR(//)
    !> @copybrief StringType::concatenate_char_onto_StringType
    !> @copydetails StringType::concatenate_char_onto_StringType
    MODULE PROCEDURE concatenate_char_onto_StringType
    !> @copybrief StringType::concatenate_StringType_onto_char
    !> @copydetails StringType::concatenate_StringType_onto_char
    MODULE PROCEDURE concatenate_StringType_onto_char
    !> @copybrief StringType::concatenate_StringType_onto_StringType
    !> @copydetails StringType::concatenate_StringType_onto_StringType
    MODULE PROCEDURE concatenate_StringType_onto_StringType
  ENDINTERFACE

  !> @brief Overloads the Fortran intrinsic operator for comparing
  !> two variables to see if they are equal
  INTERFACE OPERATOR(==)
    !> @copybrief StringType::equalto_char_StringType
    !> @copydetails StringType::equalto_char_StringType
    MODULE PROCEDURE equalto_char_StringType
    !> @copybrief StringType::equalto_StringType_char
    !> @copydetails StringType::equalto_StringType_char
    MODULE PROCEDURE equalto_StringType_char
    !> @copybrief StringType::equalto_StringType_StringType
    !> @copydetails StringType::equalto_StringType_StringType
    MODULE PROCEDURE equalto_StringType_StringType
  ENDINTERFACE

  !> @brief Overloads the Fortran intrinsic operator for comparing
  !> two variables to see if they are not equal
  INTERFACE OPERATOR(/=)
    !> @copybrief StringType::notequalto_char_StringType
    !> @copydetails StringType::notequalto_char_StringType
    MODULE PROCEDURE notequalto_char_StringType
    !> @copybrief StringType::notequalto_StringType_char
    !> @copydetails StringType::notequalto_StringType_char
    MODULE PROCEDURE notequalto_StringType_char
    !> @copybrief StringType::notequalto_StringType_StringType
    !> @copydetails StringType::notequalto_StringType_StringType
    MODULE PROCEDURE notequalto_StringType_StringType
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief cleans up string objects
!> @param this the StringType being garbaged collected
!>
SUBROUTINE clear_str(this)
  CLASS(StringType),INTENT(INOUT) :: this
  IF(ALLOCATED(this%s)) DEALLOCATE(this%s)
ENDSUBROUTINE clear_str
!
!-------------------------------------------------------------------------------
!> @brief Assigns an intrinsic character array to a string
!> @param lhs the string type receiving the characters
!> @param rhs the character string that will be assigned
!>
  ELEMENTAL SUBROUTINE assign_char_to_StringType(lhs,rhs)
    CLASS(StringType),INTENT(INOUT) :: lhs
    CHARACTER(LEN=*),INTENT(IN) :: rhs
    IF(ALLOCATED(lhs%s)) DEALLOCATE(lhs%s)
    IF(LEN(rhs)>0) THEN
      lhs%s = rhs
    ELSE
      ALLOCATE(CHARACTER(0) :: lhs%s)
    ENDIF
  ENDSUBROUTINE assign_char_to_StringType
!
!-------------------------------------------------------------------------------
!> @brief converts a c-style character array to an intrinsic Fortran array
!>
  FUNCTION cchar_to_fchar(cchar) RESULT(fchar)
    CHARACTER(KIND=C_CHAR),TARGET,INTENT(IN) :: cchar(:)
    CHARACTER(LEN=:),ALLOCATABLE :: fchar
    INTEGER(SIK) :: i,n

    n = SIZE(cchar)
    ALLOCATE(CHARACTER(n) :: fchar)
    DO i=1,n
      fchar(i:i) = cchar(i)
    ENDDO

  ENDFUNCTION cchar_to_fchar
!
!-------------------------------------------------------------------------------
!> @brief splits a StringType using a single space as the delimiter. This
!> routine consumes consecutive spaces as a if they were a single space. The
!> @param this the StringType being split
!> @returns sub_str an array of substings
!>
!> The returned array of strings is will not contain any empty strings. If the
!> string being split is empty the returned array will be size 0. If there were
!> no separators found to split on the array will be size 1, and contain the
!> original string.
!>
FUNCTION split_string_space(this) RESULT(sub_str)
  CLASS(StringType),INTENT(IN) :: this
  TYPE(StringType),ALLOCATABLE :: sub_str(:)
  !
  CHARACTER(LEN=:),ALLOCATABLE :: separator
  INTEGER(SIK) :: iSplit,nSplits,stt,stp,sepLoc

  separator = ' '
  stp = LEN(this%s)
  sepLoc = MERGE(INDEX(this%s,separator),0,stp > 1)
  IF(sepLoc == 0) THEN
    !This indicates that either the string or the separator were empty, or
    !the separator was not found in the string.
    ALLOCATE(sub_str(1))
    sub_str(1) = this%s
    RETURN !For these conditions the original string is returned
  ENDIF

  ! Search until the delimiter isn't found
  nSplits = 0
  stt = 1
  DO WHILE(sepLoc > 0)
  ! If the index is greater than 1 then the first character must be a string
    IF(sepLoc > 1) THEN
      ! Count the string
      nSplits = nSplits + 1
    ENDIF
    ! Increment to the next delimiter
    stt = stt + sepLoc
    sepLoc = INDEX(this%s(stt:stp),separator)
  ENDDO
  !Account for strings that don't end in a delimiter
  IF(stt <= stp) nSplits = nSplits + 1
  ALLOCATE(sub_str(nSplits))

  ! Split along delimiters and store in the provided array
  stt = 1
  iSplit = 0
  sepLoc = INDEX(this%s(stt:stp),separator)
  DO WHILE(sepLoc > 0)
    IF(sepLoc > 1) THEN
      iSplit = iSplit + 1
      ! Strip out the string...subtract 2 (1 for exclusive, 1 for delimiter)
      sub_str(iSplit) = &
          this%s(stt:stt+INDEX(this%s(stt:stp),separator) - 2)
    ENDIF
    stt = stt + sepLoc
    sepLoc = INDEX(this%s(stt:stp),separator)
  ENDDO
  ! If the string ends with a word, then it wasn't snatched out before
  IF(.NOT.(this%s(stp:stp) == separator)) THEN
    sub_str(nSplits) = &
        this%s(INDEX(this%s(1:stp),separator,.TRUE.)+1:stp)
  ENDIF
ENDFUNCTION split_string_space
!
!-------------------------------------------------------------------------------
!> @brief splits a StringType at specified delimiter and returns the substrings
!> in an array of StringTypes.
!> @param this the string to be split
!> @param separator the delimiter for split locations
!> @returns sub_str the returned array of substrings
!>
  FUNCTION split_string(this,separator) RESULT(sub_str)
    CLASS(StringType),INTENT(IN) :: this
    CHARACTER(LEN=*),INTENT(IN) :: separator 
    TYPE(StringType),ALLOCATABLE :: sub_str(:)
    !
    INTEGER(SIK) :: iSplit,nSplits,stt,stp,sepLoc

    stp = LEN(this%s)
    sepLoc = MERGE(INDEX(this%s,separator),0,stp > 1 .OR. LEN(separator) > 0)
    IF(sepLoc == 0) THEN
      !This indicates that either the string or the separator were empty, or
      !the separator was not found in the string.
      ALLOCATE(sub_str(1))
      sub_str(1) = this%s
      RETURN !For these conditions the original string is returned
    ENDIF

    ! Count the number of splits
    nSplits = 0
    stt = 1
    DO WHILE(sepLoc > 0)
      nSplits = nSplits + 1
      stt = stt + sepLoc + LEN(separator) - 1
      sepLoc = INDEX(this%s(stt:stp),separator)
    ENDDO
    !Account for strings that don't end in a delimiter
    IF(stt <= stp) nSplits = nSplits + 1
    ALLOCATE(sub_str(nSplits))

    stt = 1
    iSplit = 0
    sepLoc = INDEX(this%s(stt:stp),separator)
    DO WHILE(sepLoc > 0)
      iSplit = iSplit + 1
      sub_str(iSplit) = this%s(stt:stt+sepLoc-2)
      stt = stt + sepLoc + LEN(separator) - 1
      sepLoc = INDEX(this%s(stt:stp),separator)
    ENDDO
    ! If the string ends with a word, then it wasn't snatched out before
    IF(iSplit < nSplits) THEN
      sub_str(nSplits) = &
          this%s(INDEX(this%s(1:stp),separator,.TRUE.)+LEN(separator):stp)
    ENDIF
  ENDFUNCTION split_string
!
!-------------------------------------------------------------------------------
!> @brief returns a copy of the string with every occurrence of a specified
!> search pattern with the designated replacement pattern.
!> @param this the StringType being operated on
!> @param oldPat the pattern being replaced
!> @param newPat the pattern being inserted
!> @returns retStr the newly formed string
!> This routine will automatically account for size changes of the underlying
!> character array. If the search pattern is not found, then the original string
!> is returned
!>
FUNCTION replace_pattern(this,oldPat,newPat) RESULT(retStr)
  CLASS(StringType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: oldPat
  CHARACTER(LEN=*),INTENT(IN) :: newPat
  CHARACTER(LEN=:),ALLOCATABLE :: retStr
  !
  TYPE(StringType),ALLOCATABLE :: tokens(:)
  INTEGER(SIK) :: iToken

  !Split out the parts to be retained
  tokens = this%split(oldPat)

  !Set-up the new string
  IF(SIZE(tokens) > 1) THEN
    retStr = ''
  ELSEIF(SIZE(tokens) == 1) THEN
    retStr = char(tokens(1))
    RETURN
  ELSE
    retStr = ''
    RETURN
  ENDIF

  DO iToken=1,SIZE(tokens)-1
    IF(tokens(iToken) == '') THEN
      retStr = retStr//newPat
    ELSE
      retStr = retStr//tokens(iToken)//newPat
    ENDIF
  ENDDO
  IF(tokens(SIZE(tokens)) /= '') THEN
    retStr = retStr//tokens(SIZE(tokens))
  ELSE
    retStr = retStr//newPat
  ENDIF

ENDFUNCTION replace_pattern
!
!-------------------------------------------------------------------------------
!> @brief replaces a slice from a string with the designated character(s). If
!> the replacement characters are not the same size as the slice, the characters
!> are repeated until the slice is filled, any excess is truncated
!> @param this the string where the substitution is occurring
!> @param stt the start of the slice to be replaced
!> @param stp the end of the slice to be replaced
!> @param in_char the characters that will be subbed in
!> @returns retStr the newly formed string
!>
  FUNCTION replace_slice(this,stt,stp,in_char) RESULT(retStr)
    CLASS(StringType),INTENT(INOUT) :: this
    INTEGER(SIK),INTENT(IN) :: stt
    INTEGER(SIK),INTENT(IN) :: stp
    CHARACTER(LEN=*),INTENT(IN) :: in_char
    !
    CHARACTER(LEN=stp-stt+1) :: tmp_str
    INTEGER(SIK) :: full,part,step
    CHARACTER(LEN=:),ALLOCATABLE :: retStr

    IF((stt <= stp) .AND. (stt > 0) .AND. (stp <= LEN(this))) THEN
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
      retStr = this%s
      retStr(stt:stp) = tmp_str(1:step)
    ELSE
      retStr =this%s
    ENDIF
  ENDFUNCTION replace_slice
!
!-------------------------------------------------------------------------------
!> @brief Returns the character at the index provided, if the index is out of
!> or the string is not allocated then the ascii null character is returned.
!> @param this the string being indexed into
!> @param pos the index the character should be taken from
!> @returns letter the character being returned
!>
  PURE FUNCTION at_string(this,pos) RESULT(letter)
    CLASS(StringType),INTENT(IN) :: this
    INTEGER(SIK),INTENT(IN) :: pos
    CHARACTER(LEN=:),ALLOCATABLE :: letter

    IF((pos > 0) .AND. (pos <= LEN(this))) THEN
      letter = this%s(pos:pos)
    ELSE
      ALLOCATE(CHARACTER(0) :: letter)
    ENDIF
  ENDFUNCTION at_string
!
!-------------------------------------------------------------------------------
!> @brief Returns the characters in the slice indicated, if the either index is
!> out of range, then the ascii null character is returned
!> @param this the string being sliced
!> @param stt the first index of the slice
!> @param stp the last index of the slice OPTIONAL: Default = LEN(this)
!> @returns slice the character(s) being returned
!
  PURE FUNCTION substr_str(this,stt,stp) RESULT(slice)
    CLASS(StringType),INTENT(IN) :: this
    INTEGER(SIK),INTENT(IN) :: stt
    INTEGER(SIK),INTENT(IN),OPTIONAL :: stp
    CHARACTER(LEN=:),ALLOCATABLE :: slice
    INTEGER(SIK) :: sub_stp

    IF(PRESENT(stp)) THEN
      sub_stp = stp
    ELSE
      sub_stp = LEN(this)
    ENDIF
    IF((stt <= sub_stp) .AND. (stt > 0) .AND. (sub_stp <= LEN(this))) THEN
      slice = this%s(stt:sub_stp)
    ELSE
      ALLOCATE(CHARACTER(0) :: slice)
    ENDIF
  ENDFUNCTION substr_str
!
!-------------------------------------------------------------------------------
!> @brief Utility function takes a string and converts all upper case letters to
!> lower case letters.
!> @param word input is a string, output has all lower case letters
!> @lower_str the lower cased version of this
!>
    PURE FUNCTION toLower_string(word) RESULT(lower_str)
      CLASS(StringType),INTENT(IN) :: word
      INTEGER(SIK) :: i
      !
      TYPE(StringType) :: lower_str
      lower_str = word
      DO i=1,LEN(word)
        IF('A' <= word%s(i:i) .AND. word%s(i:i) <= 'Z') &
          lower_str%s(i:i)=ACHAR(IACHAR(word%s(i:i))+32)
      ENDDO
    ENDFUNCTION toLower_string
!
!-------------------------------------------------------------------------------
!> @brief Utility function takes a string and converts all lower case letters to
!> upper case letters.
!> @param word input is a string, output has all upper case letters
!> @upper_str the upper cased version of this
!>
    PURE FUNCTION toupper_string(word) RESULT(upper_str)
      CLASS(StringType),INTENT(IN) :: word
      INTEGER(SIK) :: i
      !
      TYPE(StringType) :: upper_str
      upper_str = word
      DO i=1,LEN(word)
        IF('a' <= word%s(i:i) .AND. word%s(i:i) <= 'z') &
          upper_str%s(i:i)=ACHAR(IACHAR(word%s(i:i))-32)
      ENDDO
    ENDFUNCTION toupper_string
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
      IF(ALLOCATED(this%s)) THEN
        n=LEN_TRIM(this%s)
      ELSE
        n=0
      ENDIF
    ENDFUNCTION LEN_TRIM_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the contents of the string as an intrinsic character type
!> variable. If the string is unallocated a zero-length character is returned
!> @param this the string object
!> @returns str the character type with the value of @c this
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
!> @returns str this trimmed of trailing whitespace
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
!> @returns s left aligned version of this
!>
    PURE FUNCTION ADJUSTL_StringType(thisStr) RESULT(s)
      CLASS(StringType),INTENT(IN) :: thisStr
      CHARACTER(LEN=:),ALLOCATABLE :: s
      s = ADJUSTL(thisStr%s)
    ENDFUNCTION ADJUSTL_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the contents of the string as an intrinsic character type
!> variable with all trailing whitespace moved to the beginning.
!> @param thisStr the string object
!> @returns s right aligned version of this
!>
    PURE FUNCTION ADJUSTR_StringType(thisStr) RESULT(s)
      CLASS(StringType),INTENT(IN) :: thisStr
      CHARACTER(LEN=:),ALLOCATABLE :: s
      s = ADJUSTR(thisStr%s)
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
!> @brief Assigns a short integer to a StringType.
!> @param lhs the string object
!> @param rhs short integer
!>
    ELEMENTAL SUBROUTINE assign_Short_Integer_to_StringType(lhs,rhs)
      CLASS(StringType),INTENT(INOUT) :: lhs
      INTEGER(SNK),INTENT(IN) :: rhs
      CHARACTER(LEN=11) :: l
      WRITE(l,'(i11)') rhs
      lhs = TRIM(ADJUSTL(l))
    ENDSUBROUTINE assign_Short_Integer_to_StringType
!
!-------------------------------------------------------------------------------
!> @brief Assigns a long integer to a StringType.
!> @param lhs the string object
!> @param rhs long integer
!>
    ELEMENTAL SUBROUTINE assign_Long_Integer_to_StringType(lhs,rhs)
      CLASS(StringType),INTENT(INOUT) ::lhs
      INTEGER(SLK),INTENT(IN) :: rhs
      CHARACTER(LEN=20) :: l
      WRITE(l,'(i20)') rhs
      lhs = TRIM(ADJUSTL(l))
    ENDSUBROUTINE assign_Long_Integer_to_StringType
!
!-------------------------------------------------------------------------------
!> @brief Assigns a single real to a StringType.
!> @param lhs the string object
!> @param rhs single real
!>
    ELEMENTAL SUBROUTINE assign_Single_Real_to_StringType(lhs,rhs)
      CLASS(StringType),INTENT(INOUT) :: lhs
      REAL(SSK),INTENT(IN) :: rhs
      CHARACTER(LEN=15) :: l
      WRITE(l,'(es15.5)') rhs
      lhs = TRIM(ADJUSTL(l))
    ENDSUBROUTINE assign_Single_Real_to_StringType
!
!-------------------------------------------------------------------------------
!> @brief Assigns a double real to a StringType.
!> @param lhs the string object
!> @param rhs double real
!>
    ELEMENTAL SUBROUTINE assign_Double_Real_to_StringType(lhs,rhs)
      CLASS(StringType),INTENT(INOUT) :: lhs
      REAL(SDK),INTENT(IN) :: rhs
      CHARACTER(LEN=22) :: l
      WRITE(l,'(es22.15)') rhs
      lhs = TRIM(ADJUSTL(l))
    ENDSUBROUTINE assign_Double_Real_to_StringType
!
!-------------------------------------------------------------------------------
!> @brief Assigns a logical to a StringType.
!> @param lhs the string object
!> @param rhs the logical to be written
!>
  SUBROUTINE assign_Logical_to_StringType(lhs,rhs)
    CLASS(StringType),INTENT(INOUT) :: lhs
    LOGICAL(SBK),INTENT(IN) :: rhs
    CHARACTER(LEN=1) :: tmp
    WRITE(tmp,'(L1)') rhs
    lhs = tmp
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
    PURE FUNCTION concatenate_StringType_onto_char(lhs,rhs) RESULT(out_str)
      CHARACTER(LEN=*),INTENT(IN) :: lhs
      CLASS(StringType),INTENT(IN) :: rhs
      CHARACTER(LEN=:),ALLOCATABLE :: out_str
      out_str = (lhs//CHAR(rhs))
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
    PURE FUNCTION concatenate_char_onto_StringType(lhs,rhs) RESULT(out_str)
      CLASS(StringType),INTENT(IN) :: lhs
      CHARACTER(LEN=*),INTENT(IN) :: rhs
      CHARACTER(LEN=:),ALLOCATABLE :: out_str
      out_str = (CHAR(lhs)//rhs)
    ENDFUNCTION concatenate_char_onto_StringType
!
!-------------------------------------------------------------------------------
!> @brief Concatenates a @c StringType type variable with a @c StringType.
!> @param s1 the string object
!> @param s2 the string object
!> @returns newstring a character string that is a concatenation of a
!> two @c StringTypes
!>
    PURE FUNCTION concatenate_StringType_onto_StringType(lhs,rhs) RESULT(out_str)
      CLASS(StringType),INTENT(IN) :: lhs
      CLASS(StringType),INTENT(IN) :: rhs
      CHARACTER(LEN=:),ALLOCATABLE :: out_str
      out_str = (CHAR(lhs)//CHAR(rhs))
    ENDFUNCTION concatenate_StringType_onto_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs an equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param lhs a @c CHARACTER type
!> @param rhs a @c StringType object
!> @returns bool the result of the == operation
!>
    ELEMENTAL FUNCTION equalto_char_StringType(lhs,rhs) RESULT(bool)
      CHARACTER(LEN=*),INTENT(IN) :: lhs
      CLASS(StringType),INTENT(IN) ::rhs
      LOGICAL(SBK) :: bool
      bool = (lhs == CHAR(rhs))
    ENDFUNCTION equalto_char_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs an equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param lhs a @c StringType object
!> @param rhs a @c CHARACTER type
!> @returns bool the result of the == operation
!>
    ELEMENTAL FUNCTION equalto_StringType_char(lhs,rhs) RESULT(bool)
      CLASS(StringType),INTENT(IN) ::lhs
      CHARACTER(LEN=*),INTENT(IN) :: rhs
      LOGICAL(SBK) :: bool
        bool =  (CHAR(lhs) == rhs)
    ENDFUNCTION equalto_StringType_char
!
!-------------------------------------------------------------------------------
!> @brief Performs an equal to operation of a @c StringType and a
!> @c StringType.
!> @param lhs a @c StringType object
!> @param rhs another @c StringType object
!> @returns bool the result of the == operation
!>
    ELEMENTAL FUNCTION equalto_StringType_StringType(lhs,rhs) RESULT(bool)
      CLASS(StringType),INTENT(IN) :: lhs
      CLASS(StringType),INTENT(IN) :: rhs
      LOGICAL(SBK) :: bool
      bool = (CHAR(lhs) == CHAR(rhs))
    ENDFUNCTION equalto_StringType_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs a not equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param lhs a @c CHARACTER type
!> @param rhs a @c StringType object
!> @returns bool the result of the /= operation
!>
    ELEMENTAL FUNCTION notequalto_char_StringType(lhs,rhs) RESULT(bool)
      CHARACTER(LEN=*),INTENT(IN) :: lhs
      CLASS(StringType),INTENT(IN) :: rhs
      LOGICAL(SBK) :: bool
      bool = (lhs /= CHAR(rhs))
    ENDFUNCTION notequalto_char_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs a not equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param lhs a @c StringType object
!> @param rhs a @c CHARACTER type
!> @returns bool the result of the /= operation
!>
    ELEMENTAL FUNCTION notequalto_StringType_char(lhs,rhs) RESULT(bool)
      CLASS(StringType),INTENT(IN) :: lhs
      CHARACTER(LEN=*),INTENT(IN) :: rhs
      LOGICAL(SBK) :: bool
      bool = (CHAR(lhs) /= rhs)
    ENDFUNCTION notequalto_StringType_char
!
!-------------------------------------------------------------------------------
!> @brief Performs a not equal to operation of a @c StringType and a
!> @c StringType.
!> @param lhs a @c StringType object
!> @param rhs another @c StringType object
!> @returns bool the result of the /= operation
!>
    ELEMENTAL FUNCTION notequalto_StringType_StringType(lhs,rhs) RESULT(bool)
      CLASS(StringType),INTENT(IN) :: lhs
      CLASS(StringType),INTENT(IN) :: rhs
      LOGICAL(SBK) :: bool
      bool = (CHAR(lhs) /= CHAR(rhs))
    ENDFUNCTION notequalto_StringType_StringType
!
!-------------------------------------------------------------------------------
!> @brief convert a string to an int
!> @param this the string being converted
!> @param stt position for starting conversion
!> @param stp end of conversion
!> @returns i the integer that will be output
!>
  FUNCTION str_to_sik(this,stt,stp) RESULT(i)
    CLASS(StringType),INTENT(IN) :: this
    INTEGER(SIK),INTENT(IN),OPTIONAL :: stt
    INTEGER(SIK),INTENT(IN),OPTIONAL :: stp
    INTEGER(SIK) :: i
    !
    CHARACTER(LEN=:),ALLOCATABLE :: scratch

    IF(PRESENT(stt) .OR. PRESENT(stp)) THEN
      scratch = this%s(stt:stp)
    ELSE
      scratch = this%s
    ENDIF

    IF(LEN(scratch) > 0) THEN
      READ(scratch,*) i
    ELSE
      i = -HUGE(i)
    ENDIF
  ENDFUNCTION str_to_sik
!
!-------------------------------------------------------------------------------
!> @brief convert a string to a float
!> @param this the string being converted
!> @param stt position for starting conversion
!> @param stp end of conversion
!> @returns i the integer that will be output
!>
  FUNCTION str_to_srk(this,stt,stp) RESULT(i)
    CLASS(StringType),INTENT(IN) :: this
    INTEGER(SIK),INTENT(IN),OPTIONAL :: stt
    INTEGER(SIK),INTENT(IN),OPTIONAL :: stp
    REAL(SRK) :: i
    !
    CHARACTER(LEN=:),ALLOCATABLE :: scratch

    IF(PRESENT(stt) .OR.  PRESENT(stp)) THEN
      scratch = this%s(stt:stp)
    ELSE
      scratch = this%s
    ENDIF

    IF(LEN(scratch) > 0) THEN
      READ(scratch,*) i
    ELSE
      i = -HUGE(i)
    ENDIF
  ENDFUNCTION str_to_srk
!
!-------------------------------------------------------------------------------
!> @brief decides if a string represents an integer
!> @param this the string being examined
!> @return bool logical indicating decision
!>
  FUNCTION isInteger(this) RESULT(bool)
    CLASS(StringType),INTENT(IN) :: this
    LOGICAL(SBK) :: bool
    INTEGER(SIK) :: stt

    stt = MAX(INDEX(this%s,'+'),INDEX(this%s,'-'))
    bool = isNumeric(this%s(stt+1:LEN(this%s)))
  ENDFUNCTION isInteger
!
!-------------------------------------------------------------------------------
!> @brief decides if a string represents an integer
!> @param this the string being examined
!> @return bool logical indicating decision
!>
  FUNCTION isFloat(this) RESULT(bool)
    CLASS(StringType),INTENT(IN) :: this
    LOGICAL(SBK) :: bool
    INTEGER(SIK) :: stt,dec_pos,exp_pos,pos_neg_exp

    bool = .FALSE.
    ! Enforce that a decimal is required in order to be a float
    dec_pos = INDEX(this%s,'.')
    IF(dec_pos > 0) THEN
      ! Look for leading '+' and '-'
      stt = MAX(INDEX(this%s(1:dec_pos),'+'),INDEX(this%s(1:dec_pos),'-'))
      ! Check that the leading characters are numeric
      IF(isNumeric(this%s(stt+1:dec_pos-1))) THEN
        ! Look for Currently acceptable exponents
        exp_pos = MAX(INDEX(this%s,'e'),INDEX(this%s,'E'),INDEX(this%s,'d'))
        IF(exp_pos > 0) THEN
          ! Account for sign on exponent
          pos_neg_exp = exp_pos + MAX(INDEX(this%s(exp_pos+1:LEN(this%s)),'-'),INDEX(this%s(exp_pos+1:LEN(this%s)),'+'))
          IF(pos_neg_exp > 0) THEN
            bool = isNumeric(this%s(dec_pos+1:exp_pos-1)) .AND. &
                isNumeric(this%s(pos_neg_exp+1:LEN(this%s)))
          ELSE
            bool = isNumeric(this%s(dec_pos+1:exp_pos-1)) .AND. &
                isNumeric(this%s(exp_pos+1:LEN(this%s)))
          ENDIF
        ELSE
          ! No exponent check decimal to end
          bool = isNumeric(this%s(dec_pos+1:LEN(this%s)))
        ENDIF
      ENDIF
    ENDIF
  ENDFUNCTION isFloat
!
!-------------------------------------------------------------------------------
!> @brief decides if a string represents a numeric value (either int or float)
!> @param this the string be examined
!> @return bool logical indicating decision
!>
  FUNCTION isNumeric_str(this) RESULT(bool)
    CLASS(StringType),INTENT(IN) :: this
    LOGICAL(SBK) :: bool
    bool = this%isInteger() .OR. this%isFloat()
  ENDFUNCTION isNumeric_str
!
ENDMODULE Strings
