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
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Strings
USE ISO_C_BINDING
USE IntrType
IMPLICIT NONE
PRIVATE

PUBLIC :: StringType
PUBLIC :: CHAR
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
PUBLIC :: OPERATOR(<)
PUBLIC :: OPERATOR(>)
PUBLIC :: OPERATOR(<=)
PUBLIC :: OPERATOR(>=)

!> Derived type for an arbitrary length string
TYPE :: StringType
  !> The stored intrinsic character string
  CHARACTER(LEN=:),ALLOCATABLE,PRIVATE :: s
  CONTAINS
    !> copybrief Strings::toupper_string
    !> copydetails Strings::toupper_string
    PROCEDURE,PASS :: upper => toupper_string
    !> copybrief Strings::toLower_string
    !> copydetails Strings::toLower_string
    PROCEDURE,PASS :: lower => toLower_string
    !> copybrief Strings::at_string
    !> copydetails Strings::at_string
    PROCEDURE,PASS :: at => at_string
    !> copybrief Strings::substr_str
    !> copydetails Strings::substr_str
    PROCEDURE,PASS :: substr => substr_str
    !> copybrief Strings::replace_slice
    !> copydetails Strings::replace_slice
    PROCEDURE,PASS,PRIVATE :: replace_slice
    !> copybrief Strings::replace_pattern
    !> copydetails Strings::replace_pattern
    PROCEDURE,PASS,PRIVATE :: replace_pattern
    GENERIC :: replace => replace_slice,replace_pattern
    !> copybrief Strings:: split_string
    !> copydetails Strings:: split_string
    PROCEDURE,PASS,PRIVATE :: split_string
    !> copybrief Strings::split_string_space
    !> copydetails Strings::split_string_space
    PROCEDURE,PASS,PRIVATE :: split_string_space
    GENERIC :: split => split_string, split_string_space
    !> copybrief Strings::partition
    !> copydetails Strings::partition
    PROCEDURE,PASS :: partition
    !> copybrief Strings::rPartition
    !> copydetails Strings::rPartition
    PROCEDURE,PASS :: rPartition
    !> copybrief Strings::assign_char_to_StringType
    !> copydetails Strings::assign_char_to_StringType
    PROCEDURE,PASS,PRIVATE :: assign_char_to_StringType
    !> @copybrief Strings::assign_Short_Integer_to_StringType
    !> @copydetails Strings::assign_Short_Integer_to_StringType
    PROCEDURE,PASS,PRIVATE :: assign_Short_Integer_to_StringType
    !> @copybrief Strings::assign_Long_Integer_to_StringType
    !> @copydetails Strings::assign_Long_Integer_to_StringType
    PROCEDURE,PASS,PRIVATE :: assign_Long_Integer_to_StringType
    !> @copybrief Strings::assign_Single_Real_to_StringType
    !> @copydetails Strings::assign_Single_Real_to_StringType
    PROCEDURE,PASS,PRIVATE :: assign_Single_Real_to_StringType
    !> @copybrief Strings::assign_Double_Real_to_StringType
    !> @copydetails Strings::assign_Double_Real_to_StringType
    PROCEDURE,PASS,PRIVATE :: assign_Double_Real_to_StringType
    !> copybrief Strings::assign_Logical_to_StringType
    !> copydetails Strings::assign_Logical_to_StringType
    PROCEDURE,PASS,PRIVATE :: assign_Logical_to_StringType
    GENERIC :: ASSIGNMENT(=) => assign_char_to_StringType, &
        assign_Logical_to_StringType,assign_Single_Real_to_StringType, &
        assign_Double_Real_to_StringType,assign_Short_Integer_to_StringType, &
            assign_Long_Integer_to_StringType
    !> copybrief Strings::str_to_sik
    !> copydetails Strings::str_to_sik
    PROCEDURE,PASS :: str_to_sik
    GENERIC :: stoi => str_to_sik
    !> copybrief Strings::str_to_srk
    !> copydetails Strings::str_to_srk
    PROCEDURE,PASS :: str_to_srk
    GENERIC :: stof => str_to_srk
    !> copybrief Strings::isInteger
    !> copydetails Strings::isInteger
    PROCEDURE,PASS :: isInteger
    !> copybrief Strings::isFloat
    !> copydetails Strings::isFloat
    PROCEDURE,PASS :: isFloat
    !> copybrief Strings::isNumeric_str
    !> copydetails Strings::isNumeric_str
    PROCEDURE,PASS :: isNumeric => isNumeric_str
    !> copybrief Strings::clear_str
    !> copydetails Strings::clear_str
    PROCEDURE,PASS :: clear => clear_str
    PROCEDURE :: read_formatted_StringType
    GENERIC :: READ(FORMATTED) => read_formatted_StringType
    PROCEDURE :: read_unformatted_StringType
    GENERIC :: READ(UNFORMATTED) => read_unformatted_StringType
    PROCEDURE :: write_formatted_StringType
    GENERIC :: WRITE(FORMATTED) => write_formatted_StringType
    PROCEDURE :: write_unformatted_StringType
    GENERIC :: WRITE(UNFORMATTED) => write_unformatted_StringType
ENDTYPE StringType

INTERFACE StringType
  !> @copybrief Strings::charToStringType
  !> @copydetails Strings::charToStringType
  MODULE PROCEDURE charToStringType
ENDINTERFACE

!> @brief Overloads the Fortran intrinsic procedure CHAR() so
!> a string type argument may be passed.
INTERFACE CHAR
  !> @copybrief Strings::CHAR_StringType
  !> @copydetails Strings::CHAR_StringType
  MODULE PROCEDURE CHAR_StringType
  !> copybrief Strings::cchar_to_fchar
  !> copydetails Strings::cchar_to_fchar
  MODULE PROCEDURE cchar_to_fchar
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

!> @brief Overloads the Fortran intrinsic operator for comparing
!> two variables to see if one is less than the other
!>
!> NOTE: This assumes that the system is using ASCII ordering and
!> that this is therefore equal to LLT (lexical less than).
INTERFACE OPERATOR(<)
  !> @copybrief Strings::lessthan_char_StringType
  !> @copydetails Strings::lessthan_char_StringType
  MODULE PROCEDURE lessthan_char_StringType
  !> @copybrief Strings::lessthan_StringType_char
  !> @copydetails Strings::lessthan_StringType_char
  MODULE PROCEDURE lessthan_StringType_char
  !> @copybrief Strings::lessthan_StringType_StringType
  !> @copydetails Strings::lessthan_StringType_StringType
  MODULE PROCEDURE lessthan_StringType_StringType
ENDINTERFACE

!> @brief Overloads the Fortran intrinsic operator for comparing
!> two variables to see one is greater than the other
!>
!> NOTE: This assumes that the system is using ASCII ordering and
!> that this is therefore equal to LGT (lexical greater than).
INTERFACE OPERATOR(>)
  !> @copybrief Strings::greaterthan_char_StringType
  !> @copydetails Strings::greaterthan_char_StringType
  MODULE PROCEDURE greaterthan_char_StringType
  !> @copybrief Strings::greaterthan_StringType_char
  !> @copydetails Strings::greaterthan_StringType_char
  MODULE PROCEDURE greaterthan_StringType_char
  !> @copybrief Strings::greaterthan_StringType_StringType
  !> @copydetails Strings::greaterthan_StringType_StringType
  MODULE PROCEDURE greaterthan_StringType_StringType
ENDINTERFACE

!> @brief Overloads the Fortran intrinsic operator for comparing
!> two variables to see if one is less than or equal to the other
!>
!> NOTE: This assumes that the system is using ASCII ordering and
!> that this is therefore equal to LLE (lexical less than/equal to).
INTERFACE OPERATOR(<=)
  !> @copybrief Strings::lessthanequal_char_StringType
  !> @copydetails Strings::lessthanequal_char_StringType
  MODULE PROCEDURE lessthanequal_char_StringType
  !> @copybrief Strings::lessthanequal_StringType_char
  !> @copydetails Strings::lessthanequal_StringType_char
  MODULE PROCEDURE lessthanequal_StringType_char
  !> @copybrief Strings::lessthanequal_StrinequalgType_StringType
  !> @copydetails Strings::lessthanequal_StrinequalgType_StringType
  MODULE PROCEDURE lessthanequal_StringType_StringType
ENDINTERFACE

!> @brief Overloads the Fortran intrinsic operator for comparing
!> two variables to see one is greater than or equal to the other
!>
!> NOTE: This assumes that the system is using ASCII ordering and
!> that this is therefore equal to LGE (lexical greater than/equal to).
INTERFACE OPERATOR(>=)
  !> @copybrief Strings::greaterthanequal_char_StringType
  !> @copydetails Strings::greaterthanequal_char_StringType
  MODULE PROCEDURE greaterthanequal_char_StringType
  !> @copybrief Strings::greaterthanequal_StringType_char
  !> @copydetails Strings::greaterthanequal_StringType_char
  MODULE PROCEDURE greaterthanequal_StringType_char
  !> @copybrief Strings::greaterthanequal_StringType_StringType
  !> @copydetails Strings::greaterthanequal_StringType_StringType
  MODULE PROCEDURE greaterthanequal_StringType_StringType
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
!> @brief breaks a string into partitions at the first instance of a delimiter
!> @param this the string being partitioned
!> @param sep the delimiter used for partitioning
!> @returns tokens size 3 array containing the characters before the delimiter,
!> the delimiter, and the characters after the delimiter in that order
!>
PURE FUNCTION partition(this,sep) RESULT(tokens)
  CLASS(StringType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: sep
  TYPE(StringType) :: tokens(3)
  INTEGER(SIK) :: sepPos
  IF(ALLOCATED(this%s)) THEN
    sepPos = MERGE(0,INDEX(this%s,sep),sep == '')
    IF(sepPos > 0) THEN
      tokens(1) = this%s(1:sepPos-1)
      tokens(3) = this%s(sepPos+LEN(SEP):LEN(this%s))
    ELSE
      tokens(1) = this%s
      tokens(3) = ''
    ENDIF
    tokens(2) = sep
  ELSE
    tokens(1)=''
    tokens(2)=sep
    tokens(3)=''
  ENDIF
ENDFUNCTION partition
!
!-------------------------------------------------------------------------------
!> @brief breaks a string into partitions at the last instance of a delimiter
!> @param this the string being partitioned
!> @param sep the delimiter used for partitioning
!> @returns tokens size 3 array containing the characters before the delimiter,
!> the delimiter, and the characters after the delimiter in that order
!>
PURE FUNCTION rPartition(this,sep) RESULT(tokens)
  CLASS(StringType),INTENT(IN) :: this
  CHARACTER(LEN=*),INTENT(IN) :: sep
  TYPE(StringType) :: tokens(3)
  INTEGER(SIK) :: sepPos
  IF(ALLOCATED(this%s)) THEN
    sepPos = MERGE(0,INDEX(this%s,sep,.TRUE.),sep == '')
    IF(sepPos > 0) THEN
      tokens(1) = this%s(1:sepPos-1)
      tokens(3) = this%s(sepPos+LEN(SEP):LEN(this%s))
    ELSE
      tokens(1) = ''
      tokens(3) = this%s
    ENDIF
    tokens(2) = sep
  ELSE
    tokens(1)=''
    tokens(2)=sep
    tokens(3)=''
  ENDIF
ENDFUNCTION rPartition
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
!> The returned array of strings will not contain any empty strings. If the
!> string being split is empty the returned array will be size 0. If there were
!> no separators found to split on the array will be size 1, and contain the
!> original string.
!>
FUNCTION split_string_space(this) RESULT(sub_str)
  CLASS(StringType),INTENT(IN) :: this
  TYPE(StringType),ALLOCATABLE :: sub_str(:)
  !
  INTEGER(SIK) :: iSplit,nSplits,stt,stp,sepLoc

  IF(ALLOCATED(this%s)) THEN
    stp = LEN(this%s)
    sepLoc = MERGE(INDEX(this%s,' '),0,stp > 1)
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
      sepLoc = INDEX(this%s(stt:stp),' ')
    ENDDO
    !Account for strings that don't end in a delimiter
    IF(stt <= stp) nSplits = nSplits + 1
    ALLOCATE(sub_str(nSplits))

    ! Split along delimiters and store in the provided array
    stt = 1
    iSplit = 0
    sepLoc = INDEX(this%s(stt:stp),' ')
    DO WHILE(sepLoc > 0)
      IF(sepLoc > 1) THEN
        iSplit = iSplit + 1
        ! Strip out the string...subtract 2 (1 for exclusive, 1 for delimiter)
        sub_str(iSplit) = this%s(stt:stt+sepLoc-2)
      ENDIF
      stt = stt + sepLoc
      sepLoc = INDEX(this%s(stt:stp),' ')
    ENDDO
    ! If the string ends with a word, then it wasn't snatched out before
    IF(.NOT.(this%s(stp:stp) == ' ')) THEN
      sub_str(nSplits) = &
          this%s(INDEX(this%s(1:stp),' ',.TRUE.)+1:stp)
    ENDIF
  ELSE
    ALLOCATE(sub_str(1))
    sub_str=''
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

  IF(ALLOCATED(this%s)) THEN
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
  ELSE
    ALLOCATE(sub_str(1))
    sub_str=''
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
  ELSE
    retStr = char(tokens(1))
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

  IF(ALLOCATED(this%s) .AND. (pos > 0) .AND. (pos <= LEN(this))) THEN
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
  IF(ALLOCATED(this%s) .AND. (stt > 0) .AND. (sub_stp <= LEN(this))) THEN
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
  IF(ALLOCATED(this%s)) THEN
    n=LEN(this%s)
  ELSE
    n = 0
  ENDIF
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
!> @param this the string object
!> @returns s left aligned version of this
!>
PURE FUNCTION ADJUSTL_StringType(this) RESULT(s)
  CLASS(StringType),INTENT(IN) :: this
  CHARACTER(LEN=:),ALLOCATABLE :: s
  IF(ALLOCATED(this%s)) THEN
    s = ADJUSTL(this%s)
  ELSE
    s = ''
  ENDIF
ENDFUNCTION ADJUSTL_StringType
!
!-------------------------------------------------------------------------------
!> @brief Returns the contents of the string as an intrinsic character type
!> variable with all trailing whitespace moved to the beginning.
!> @param this the string object
!> @returns s right aligned version of this
!>
PURE FUNCTION ADJUSTR_StringType(this) RESULT(s)
  CLASS(StringType),INTENT(IN) :: this
  CHARACTER(LEN=:),ALLOCATABLE :: s
  IF(ALLOCATED(this%s)) THEN
    s = ADJUSTR(this%s)
  ELSE
    s = ''
  ENDIF
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
  IF(ALLOCATED(string%s) .AND. LEN(substring) > 0) THEN
    ipos=INDEX(string%s,substring,back)
  ELSE
    ipos = 0
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
ELEMENTAL FUNCTION INDEX_char_StringType(string,substring,back) RESULT(ipos)
  CHARACTER(LEN=*),INTENT(IN) :: string
  CLASS(StringType),INTENT(IN) :: substring
  LOGICAL,INTENT(IN),OPTIONAL :: back
  INTEGER :: ipos
  IF(ALLOCATED(substring%s) .AND. LEN(string) > 0) THEN
    ipos=INDEX(string,substring%s,back)
  ELSE
    ipos = 0
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
ELEMENTAL FUNCTION INDEX_StringType_StringType(string,substring,back) RESULT(ipos)
  CLASS(StringType),INTENT(IN) :: string
  CLASS(StringType),INTENT(IN) :: substring
  LOGICAL,INTENT(IN),OPTIONAL :: back
  INTEGER :: ipos
  IF(ALLOCATED(string%s) .AND. ALLOCATED(substring%s)) THEN
    ipos=INDEX(string%s,substring%s,back)
  ELSE
    ipos = 0
  ENDIF
ENDFUNCTION INDEX_StringType_StringType
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
!> @brief Performs a less than operation of a @c CHARACTER and a
!> @c StringType.
!> @param lhs a @c CHARACTER type
!> @param rhs a @c StringType object
!> @returns bool the result of the < operation
!>
ELEMENTAL FUNCTION lessthan_char_StringType(lhs,rhs) RESULT(bool)
  CHARACTER(LEN=*),INTENT(IN) :: lhs
  CLASS(StringType),INTENT(IN) ::rhs
  LOGICAL(SBK) :: bool
  bool = (lhs < CHAR(rhs))
ENDFUNCTION lessthan_char_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs a less than operation of a @c StringType and a
!> @c CHARACTER.
!> @param lhs a @c StringType object
!> @param rhs a @c CHARACTER type
!> @returns bool the result of the < operation
!>
ELEMENTAL FUNCTION lessthan_StringType_char(lhs,rhs) RESULT(bool)
  CLASS(StringType),INTENT(IN) ::lhs
  CHARACTER(LEN=*),INTENT(IN) :: rhs
  LOGICAL(SBK) :: bool
    bool = (CHAR(lhs) < rhs)
ENDFUNCTION lessthan_StringType_char
!
!-------------------------------------------------------------------------------
!> @brief Performs a less than operation of a @c StringType and a
!> @c StringType.
!> @param lhs a @c StringType object
!> @param rhs another @c StringType object
!> @returns bool the result of the < operation
!>
ELEMENTAL FUNCTION lessthan_StringType_StringType(lhs,rhs) RESULT(bool)
  CLASS(StringType),INTENT(IN) :: lhs
  CLASS(StringType),INTENT(IN) :: rhs
  LOGICAL(SBK) :: bool
  bool = (CHAR(lhs) < CHAR(rhs))
ENDFUNCTION lessthan_StringType_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs a greater than operation of a @c CHARACTER and a
!> @c StringType.
!> @param lhs a @c CHARACTER type
!> @param rhs a @c StringType object
!> @returns bool the result of the < operation
!>
ELEMENTAL FUNCTION greaterthan_char_StringType(lhs,rhs) RESULT(bool)
  CHARACTER(LEN=*),INTENT(IN) :: lhs
  CLASS(StringType),INTENT(IN) ::rhs
  LOGICAL(SBK) :: bool
  bool = (lhs > CHAR(rhs))
ENDFUNCTION greaterthan_char_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs a greater than operation of a @c StringType and a
!> @c CHARACTER.
!> @param lhs a @c StringType object
!> @param rhs a @c CHARACTER type
!> @returns bool the result of the < operation
!>
ELEMENTAL FUNCTION greaterthan_StringType_char(lhs,rhs) RESULT(bool)
  CLASS(StringType),INTENT(IN) ::lhs
  CHARACTER(LEN=*),INTENT(IN) :: rhs
  LOGICAL(SBK) :: bool
    bool = (CHAR(lhs) > rhs)
ENDFUNCTION greaterthan_StringType_char
!
!-------------------------------------------------------------------------------
!> @brief Performs a greater than operation of a @c StringType and a
!> @c StringType.
!> @param lhs a @c StringType object
!> @param rhs another @c StringType object
!> @returns bool the result of the < operation
!>
ELEMENTAL FUNCTION greaterthan_StringType_StringType(lhs,rhs) RESULT(bool)
  CLASS(StringType),INTENT(IN) :: lhs
  CLASS(StringType),INTENT(IN) :: rhs
  LOGICAL(SBK) :: bool
  bool = (CHAR(lhs) > CHAR(rhs))
ENDFUNCTION greaterthan_StringType_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs a less than or equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param lhs a @c CHARACTER type
!> @param rhs a @c StringType object
!> @returns bool the result of the <= operation
!>
ELEMENTAL FUNCTION lessthanequal_char_StringType(lhs,rhs) RESULT(bool)
  CHARACTER(LEN=*),INTENT(IN) :: lhs
  CLASS(StringType),INTENT(IN) ::rhs
  LOGICAL(SBK) :: bool
  bool = (lhs <= CHAR(rhs))
ENDFUNCTION lessthanequal_char_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs a less than or equal to operation of a @c StringType and a
!> @c CHARACTER.
!> @param lhs a @c StringType object
!> @param rhs a @c CHARACTER type
!> @returns bool the result of the <= operation
!>
ELEMENTAL FUNCTION lessthanequal_StringType_char(lhs,rhs) RESULT(bool)
  CLASS(StringType),INTENT(IN) ::lhs
  CHARACTER(LEN=*),INTENT(IN) :: rhs
  LOGICAL(SBK) :: bool
    bool = (CHAR(lhs) <= rhs)
ENDFUNCTION lessthanequal_StringType_char
!
!-------------------------------------------------------------------------------
!> @brief Performs a less than or equal to operation of a @c StringType and a
!> @c StringType.
!> @param lhs a @c StringType object
!> @param rhs another @c StringType object
!> @returns bool the result of the <= operation
!>
ELEMENTAL FUNCTION lessthanequal_StringType_StringType(lhs,rhs) RESULT(bool)
  CLASS(StringType),INTENT(IN) :: lhs
  CLASS(StringType),INTENT(IN) :: rhs
  LOGICAL(SBK) :: bool
  bool = (CHAR(lhs) <= CHAR(rhs))
ENDFUNCTION lessthanequal_StringType_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs a greater than or equal to operation of a @c CHARACTER and a
!> @c StringType.
!> @param lhs a @c CHARACTER type
!> @param rhs a @c StringType object
!> @returns bool the result of the <= operation
!>
ELEMENTAL FUNCTION greaterthanequal_char_StringType(lhs,rhs) RESULT(bool)
  CHARACTER(LEN=*),INTENT(IN) :: lhs
  CLASS(StringType),INTENT(IN) ::rhs
  LOGICAL(SBK) :: bool
  bool = (lhs >= CHAR(rhs))
ENDFUNCTION greaterthanequal_char_StringType
!
!-------------------------------------------------------------------------------
!> @brief Performs a greater than or equal to operation of a @c StringType and a
!> @c CHARACTER.
!> @param lhs a @c StringType object
!> @param rhs a @c CHARACTER type
!> @returns bool the result of the <= operation
!>
ELEMENTAL FUNCTION greaterthanequal_StringType_char(lhs,rhs) RESULT(bool)
  CLASS(StringType),INTENT(IN) ::lhs
  CHARACTER(LEN=*),INTENT(IN) :: rhs
  LOGICAL(SBK) :: bool
    bool = (CHAR(lhs) >= rhs)
ENDFUNCTION greaterthanequal_StringType_char
!
!-------------------------------------------------------------------------------
!> @brief Performs a greater than or equal to operation of a @c StringType and a
!> @c StringType.
!> @param lhs a @c StringType object
!> @param rhs another @c StringType object
!> @returns bool the result of the <= operation
!>
ELEMENTAL FUNCTION greaterthanequal_StringType_StringType(lhs,rhs) RESULT(bool)
  CLASS(StringType),INTENT(IN) :: lhs
  CLASS(StringType),INTENT(IN) :: rhs
  LOGICAL(SBK) :: bool
  bool = (CHAR(lhs) >= CHAR(rhs))
ENDFUNCTION greaterthanequal_StringType_StringType
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
  INTEGER(SIK) :: begStr,endStr,ioerr

  !Bypass the world if the string isn't allocated
  ioerr = -HUGE(ioerr)
  IF(ALLOCATED(this%s)) THEN
    !Find the beginning
    IF(PRESENT(stt)) THEN
      begStr = stt
    ELSE
      begStr = 1
    ENDIF
    !Find the end
    IF(PRESENT(stp)) THEN
      endStr = stp
    ELSE
      endStr = LEN(this%s)
    ENDIF
    READ(this%s(begStr:endStr),*,IOSTAT=ioerr) i
  ENDIF

  !Test to see if it worked, return 0 on any error code
  IF(ioerr /= 0) THEN
    i = -HUGE(i)
  ENDIF
ENDFUNCTION str_to_sik
!
!-------------------------------------------------------------------------------
!> @brief convert a string to a float
!> @param this the string being converted
!> @param stt position for starting conversion
!> @param stp end of conversion
!> @returns r the float that will be output
!>
FUNCTION str_to_srk(this,stt,stp) RESULT(r)
  CLASS(StringType),INTENT(IN) :: this
  INTEGER(SIK),INTENT(IN),OPTIONAL :: stt
  INTEGER(SIK),INTENT(IN),OPTIONAL :: stp
  REAL(SRK) :: r
  !
  CHARACTER(LEN=:),ALLOCATABLE :: scratch
  INTEGER(SIK) :: begStr,endStr,ioerr

  !Bypass the world if the string isn't allocated
  ioerr = -HUGE(ioerr)
  IF(ALLOCATED(this%s)) THEN
    !Find the beginning
    IF(PRESENT(stt)) THEN
      begStr = stt
    ELSE
      begStr = 1
    ENDIF
    !Find the end
    IF(PRESENT(stp)) THEN
      endStr = stp
    ELSE
      endStr = LEN(this%s)
    ENDIF
    READ(this%s(begStr:endStr),*,IOSTAT=ioerr) r
  ENDIF

  !Test to see if it worked, return nan on any error code
  IF(ioerr /= 0) THEN
    scratch='NAN()'
    READ(scratch,*) r
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
  INTEGER(SIK) :: x,ioerr

  IF(ALLOCATED(this%s)) THEN
    READ(this%s,*,IOSTAT=ioerr) x
  ELSE
    ioerr = -HUGE(1_SIK)
  ENDIF
  bool = (ioerr == 0)

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
  INTEGER(SIK) :: ioerr
  REAL(SRK) :: x

  IF(ALLOCATED(this%s)) THEN
    READ(this%s,*,IOSTAT=ioerr) x
  ELSE
    ioerr = -HUGE(1_SIK)
  ENDIF
  bool = (ioerr == 0)
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
!-------------------------------------------------------------------------------
!> @brief Constructor for a @c StringType from a character
!> @param char the character to use
!> @returns string the resulting @c StringType object
ELEMENTAL FUNCTION charToStringType(char) RESULT(string)
  CHARACTER(LEN=*),INTENT(IN) :: char
  TYPE(StringType) :: string
  string=char
ENDFUNCTION charToStringType
!
!-------------------------------------------------------------------------------
!> @brief overrides @c READ(FORMATTED) for @c StringType
!> @param dtv the string to read
!> @param iotype the type of IO being done
!> @param v_list intrinsic read interface parameter defined by fortran
!> @param iostat the status after attempting the read
!> @param iomsg the error message associated with @c iostat
!>
SUBROUTINE read_formatted_StringType(dtv,unit,iotype,v_list,iostat,iomsg)
  CLASS(StringType),INTENT(INOUT) :: dtv
  INTEGER,INTENT(IN) :: unit
  CHARACTER(LEN=*),INTENT(IN) :: iotype
  INTEGER,INTENT(IN) :: v_list(:)
  INTEGER,INTENT(OUT) :: iostat
  CHARACTER(LEN=*),INTENT(INOUT) :: iomsg
  !
  CHARACTER(LEN=256) :: buffer

  CALL dtv%clear()
  buffer=''
  READ(unit,FMT='(a)',IOSTAT=iostat,IOMSG=iomsg) buffer
  dtv=TRIM(ADJUSTL(buffer))

ENDSUBROUTINE read_formatted_StringType
!
!-------------------------------------------------------------------------------
!> @brief overrides @c READ(UNFORMATTED) for @c StringType
!> @param dtv the string to read
!> @param iostat the status after attempting the read
!> @param iomsg the error message associated with @c iostat
!>
!> This routine is not yet implemented and should not be used.
!>
SUBROUTINE read_unformatted_StringType(dtv,unit,iostat,iomsg)
  CLASS(StringType),INTENT(INOUT) :: dtv
  INTEGER,INTENT(IN) :: unit
  INTEGER,INTENT(OUT) :: iostat
  CHARACTER(LEN=*),INTENT(INOUT) :: iomsg

  STOP "Unformatted StringType reads are not supported"

ENDSUBROUTINE read_unformatted_StringType
!
!-------------------------------------------------------------------------------
!> @brief overrides @c WRITE(FORMATTED) for @c StringType
!> @param dtv the string to write
!> @param iotype the type of IO being done
!> @param v_list intrinsic read interface parameter defined by fortran
!> @param iostat the status after attempting the write
!> @param iomsg the error message associated with @c iostat
!>
SUBROUTINE write_formatted_StringType(dtv,unit,iotype,v_list,iostat,iomsg)
  CLASS(StringType),INTENT(IN) :: dtv
  INTEGER,INTENT(IN) :: unit
  CHARACTER(LEN=*),INTENT(IN) :: iotype
  INTEGER,INTENT(IN) :: v_list(:)
  INTEGER,INTENT(OUT) :: iostat
  CHARACTER(LEN=*),INTENT(INOUT) :: iomsg

  IF(ALLOCATED(dtv%s)) THEN
    WRITE(unit,FMT=*,IOSTAT=iostat,IOMSG=iomsg) dtv%s
  ELSE
    WRITE(unit,FMT=*,IOSTAT=iostat,IOMSG=iomsg) ''
  ENDIF

ENDSUBROUTINE write_formatted_StringType
!
!-------------------------------------------------------------------------------
!> @brief overrides @c WRITE(UNFORMATTED) for @c StringType
!> @param dtv the string to write
!> @param iostat the status after attempting the write
!> @param iomsg the error message associated with @c iostat
!>
!> This routine is not yet implemented and should not be used.
!>
SUBROUTINE write_unformatted_StringType(dtv,unit,iostat,iomsg)
  CLASS(StringType),INTENT(IN) :: dtv
  INTEGER,INTENT(IN) :: unit
  INTEGER,INTENT(OUT) :: iostat
  CHARACTER(LEN=*),INTENT(INOUT) :: iomsg

  STOP "Unformatted StringType writes are not supported"

ENDSUBROUTINE write_unformatted_StringType
!
ENDMODULE Strings
