!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Utility module for I/O providing routines for manipulating strings.
!>
!> This module provides several routines and parameters for string manipulation.
!> Because it is an I/O utility module its public members should be accessed
!> from @ref IOutil "IOutil". It should not be accessed directly unless it is
!> needed within another I/O utility module. This module is tested by @c
!> testIOutil.f90 and the coverage report can be found at the @ref
!> CodeCoverageReports "Code Coverage Reports" page. An example of how to use
!> the routines in this module is provided below and in the test.
!>
!> @par EXAMPLES
!> @code
!> PROGRAM FileExample
!>
!> USE IOutil
!> IMPLICIT NONE
!>
!> CHARACTER(LEN=32) :: string,fname,fpath,fext
!>
!> !Get the individuals parts of a path-file string
!> CALL getFilePath('/home/usr/dir/tmp.txt',fpath)
!> CALL getFileName('/home/usr/dir/tmp.txt',fname)
!> CALL getFileNameExt('/home/usr/dir/tmp.txt',fext)
!> WRITE(*,*) fpath
!> WRITE(*,*) fname
!> WRITE(*,*) fext
!>
!> !Get all the file parts
!> CALL getFileParts('/home/usr/dir/tmp.txt',fpath,fname,fext)
!> WRITE(*,*) fpath
!> WRITE(*,*) fname
!> WRITE(*,*) fext
!>
!> WRITE(*,*) nFields('1 2 2*3 5')
!> CALL getField(4,'1 2 2*3 5',string)
!> WRITE(*,*) string
!>
!> string='upper'
!> CALL toUPPER(string)
!> WRITE(*,*) string
!>
!> !Returns T/F if '/' is found in '/home/usr/dir/tmp'
!> WRITE(*,*) strmatch('/home/usr/dir/tmp','/')
!>
!> !Returns the number of times '/' is found in '/home/usr/dir/tmp'
!> WRITE(*,*) nmatchstr('/home/usr/dir/tmp','/')
!>
!> !Returns the indices in '/home/usr/dir/tmp' where '/' is found
!> WRITE(*,*) strfind('/home/usr/dir/tmp','/')
!>
!> !Replaces '/' with '\' in string
!> string='/home/usr/dir/tmp'
!> CALL strrep(string,'/','\')
!> WRITE(*,*) string
!>
!> ENDPROGRAM
!> @endcode
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE IO_Strings
  USE ISO_FORTRAN_ENV
#include "Futility_DBC.h"
  USE Futility_DBC
  USE IntrType
  USE Strings
  USE ExceptionHandler
  IMPLICIT NONE
  PRIVATE !Default private

  !List of Public Members
  PUBLIC :: BLANK
  PUBLIC :: BANG
  PUBLIC :: DOT
  PUBLIC :: FSLASH
  PUBLIC :: BSLASH
  PUBLIC :: SLASH
  PUBLIC :: COLON
  PUBLIC :: getFilePath
  PUBLIC :: getFileName
  PUBLIC :: getFileNameExt
  PUBLIC :: getFileParts
  PUBLIC :: getRealFormat
  PUBLIC :: nFields
  PUBLIC :: getField
  PUBLIC :: getSubstring
  PUBLIC :: toUPPER
  PUBLIC :: strfind
  PUBLIC :: strmatch
  PUBLIC :: strarraymatch
  PUBLIC :: strarraymatchind
  PUBLIC :: strarrayeqind
  PUBLIC :: nmatchstr
  PUBLIC :: strrep
  PUBLIC :: stripComment
  PUBLIC :: SlashRep
  PUBLIC :: printCentered
  PUBLIC :: str
  PUBLIC :: isChar
  PUBLIC :: isCharCap
  PUBLIC :: isCharLow
  !
  !PUBLIC :: charToStringArray

  !> Character representing a space symbol
  CHARACTER(LEN=*),PARAMETER :: BLANK=" "
  !> Character representing a comment symbol
  CHARACTER(LEN=*),PARAMETER :: BANG="!"
  !> Character representing a period
  CHARACTER(LEN=*),PARAMETER :: DOT="."
  !> Character representing a forward slash
  CHARACTER(LEN=*),PARAMETER :: FSLASH="/"
  !> Character representing a backward slash
  CHARACTER(LEN=*),PARAMETER :: BSLASH="\"
  !> Character representing a colon
  CHARACTER(LEN=*),PARAMETER :: COLON=":"
  !"This is needed for doxygen to parse correctly
#ifdef WIN32
  !> The slash symbol used by the file system
  !> (BLASH for Windows, FSLASH for everything else)
  CHARACTER(LEN=*),PARAMETER :: SLASH=BSLASH
#else
  !> The slash symbol used by the file system
  !> (BLASH for Windows, FSLASH for everything else)
  CHARACTER(LEN=*),PARAMETER :: SLASH=FSLASH
#endif

  !> Module Name for exception handler
  CHARACTER(LEN=*),PARAMETER :: modName='IO_STRINGS'

  !> @brief Generic interface to remove comments from strings.
  !>
  !> This interfaces allows for the input argument to be either a
  !> character array or a StringType.
  INTERFACE stripComment
    !> @copybrief IO_Strings::stripComment_char
    !> @copydetails IO_Strings::stripComment_char
    MODULE PROCEDURE stripComment_char
    !> @copybrief IO_Strings::stripComment_string
    !> @copydetails IO_Strings::stripComment_string
    MODULE PROCEDURE stripComment_string
  ENDINTERFACE stripComment

  !> @brief Generic interface to get the parts of the path-file name.
  !>
  !> This interfaces is presently redundant but is provided on the
  !> assumption that it may be needed later.
  INTERFACE getFileParts
    !> @copybrief IO_Strings::getFileParts_char
    !> @copydetails IO_Strings::getFileParts_char
    MODULE PROCEDURE getFileParts_char
    !> @copybrief IO_Strings::getFileParts_string
    !> @copydetails IO_Strings::getFileParts_string
    MODULE PROCEDURE getFileParts_string
  ENDINTERFACE getFileParts

  !> @brief Generic interface to get the file name.
  !>
  !> This interfaces is presently redundant but is provided on the
  !> assumption that it may be needed later.
  INTERFACE getFilePath
    !> @copybrief IO_Strings::getPath_string
    !> @copydetails IO_Strings::getPath_string
    MODULE PROCEDURE getPath_string
  ENDINTERFACE getFilePath

  !> @brief Generic interface to get the file name.
  !>
  !> This interfaces is presently redundant but is provided on the
  !> assumption that it may be needed later.
  INTERFACE getFileName
    !> @copybrief IO_Strings::getFileName_string
    !> @copydetails IO_Strings::getFileName_string
    MODULE PROCEDURE getFileName_string
  ENDINTERFACE getFileName

  !> @brief Generic interface to get the extension of a file name.
  !>
  !> This interfaces is presently redundant but is provided on the
  !> assumption that it may be needed later.
  INTERFACE getFileNameExt
    !> @copybrief IO_Strings::getFileNameExt_string
    !> @copydetails IO_Strings::getFileNameExt_string
    MODULE PROCEDURE getFileNameExt_string
  ENDINTERFACE getFileNameExt

  !> @brief Generic interface for strmatch
  !>
  !> This interfaces allows for the input argument to be either a
  !> character array or a StringType.
  INTERFACE strmatch
    !> @copybrief IO_Strings::strmatch_char
    !> @copydetails IO_Strings::strmatch_char
    MODULE PROCEDURE strmatch_char
    !> @copybrief IO_Strings::strmatch_string
    !> @copydetails IO_Strings::strmatch_string
    MODULE PROCEDURE strmatch_string
  ENDINTERFACE strmatch

  !> @brief Generic interface for strrep
  !>
  !> This interfaces allows for the input argument to be either a
  !> character array or a StringType.
  INTERFACE strrep
    !> @copybrief IO_Strings::strrep_char_char_char
    !> @copydetails IO_Strings::strrep_char_char_char
    MODULE PROCEDURE strrep_char_char_char
    !> @copybrief IO_Strings::strrep_string_char_char
    !> @copydetails IO_Strings::strrep_string_char_char
    MODULE PROCEDURE strrep_string_char_char
  ENDINTERFACE strrep

  !> @brief Generic interface for toUPPER
  !>
  !> This interfaces allows for the input argument to be either a
  !> character array or a StringType.
  INTERFACE toUPPER
    !> @copybrief IO_Strings::toUPPER_char
    !> @copydetails IO_Strings::toUPPER_char
    MODULE PROCEDURE toUPPER_char
    !> @copybrief IO_Strings::toUPPER_string
    !> @copydetails IO_Strings::toUPPER_string
    MODULE PROCEDURE toUPPER_string
  ENDINTERFACE toUPPER

  !> @brief Generic interface for nFields
  !>
  !> This interfaces allows for the input argument to be either a
  !> character array or a StringType.
  INTERFACE nFields
    !> @copybrief IO_Strings::nFields_char
    !> @copydetails IO_Strings::nFields_char
    MODULE PROCEDURE nFields_char
    !> @copybrief IO_Strings::nFields_string
    !> @copydetails IO_Strings::nFields_string
    MODULE PROCEDURE nFields_string
  ENDINTERFACE nFields

  !> @brief Generic interface for getField
  !>
  !> This interfaces allows for the input argument to be either a
  !> character array or a StringType.
  INTERFACE getField
    !> @copybrief IO_Strings::getField_char_char
    !> @copydetails IO_Strings::getField_char_char
    MODULE PROCEDURE getField_char_char
    !> @copybrief IO_Strings::getField_string_string
    !> @copydetails IO_Strings::getField_string_string
    MODULE PROCEDURE getField_string_string
    !> @copybrief IO_Strings::getField_string_char
    !> @copydetails IO_Strings::getField_string_char
    MODULE PROCEDURE getField_string_char
    !> @copybrief IO_Strings::getField_char_string
    !> @copydetails IO_Strings::getField_char_string
    MODULE PROCEDURE getField_char_string
    !> @copybrief IO_Strings::getField_string_int
    !> @copydetails IO_Strings::getField_string_int
    MODULE PROCEDURE getField_string_int
    !> @copybrief IO_Strings::getField_string_real
    !> @copydetails IO_Strings::getField_string_real
    MODULE PROCEDURE getField_string_real
  ENDINTERFACE getField

  !> @brief Generic interface for strarraymatch
  !>
  !> This interfaces allows for the input argument to be either a
  !> character array or a StringType.
  INTERFACE strarraymatch
    !> @copybrief IO_Strings::strarraymatch_char
    !> @copydetails IO_Strings::strarraymatch_char
    MODULE PROCEDURE strarraymatch_char
    !> @copybrief IO_Strings::strarraymatch_string
    !> @copydetails IO_Strings::strarraymatch_string
    MODULE PROCEDURE strarraymatch_string
  ENDINTERFACE strarraymatch

  !> @brief Generic interface for strarraymatch
  !>
  !> This interfaces allows for the input argument to be either a
  !> character array or a StringType.
  INTERFACE strarraymatchind
    !> @copybrief IO_Strings::strarraymatchind_char
    !> @copydetails IO_Strings::strarraymatchind_char
    MODULE PROCEDURE strarraymatchind_char
    !> @copybrief IO_Strings::strarraymatchind_string
    !> @copydetails IO_Strings::strarraymatchind_string
    MODULE PROCEDURE strarraymatchind_string
  ENDINTERFACE strarraymatchind

  !> @brief Generic interface for strarrayeq
  !>
  !> This interfaces allows for the input argument to be either a
  !> character array or a StringType.
  INTERFACE strarrayeqind
    !> @copybrief IO_Strings::strarrayeqind_char
    !> @copydetails IO_Strings::strarrayeqind_char
    MODULE PROCEDURE strarrayeqind_char
    !> @copybrief IO_Strings::strarrayeqind_string
    !> @copydetails IO_Strings::strarrayeqind_string
    MODULE PROCEDURE strarrayeqind_string
    !> @copybrief IO_Strings::strarrayeqind_string_string
    !> @copydetails IO_Strings::strarrayeqind_string_string
    MODULE PROCEDURE strarrayeqind_string_string
  ENDINTERFACE strarrayeqind

  !> @brief Generic interface for getRealFormat
  !>
  !> This interfaces allows for the input argument to be either a
  !> character array or a StringType.
  INTERFACE getRealFormat
    !> @copybrief IO_Strings::getRealFormat_char_char
    !> @copydetails IO_Strings::getRealFormat_char_char
    MODULE PROCEDURE getRealFormat_char_char
    !> @copybrief IO_Strings::getRealFormat_char_str
    !> @copydetails IO_Strings::getRealFormat_char_str
    MODULE PROCEDURE getRealFormat_char_str
    !> @copybrief IO_Strings::getRealFormat_str_char
    !> @copydetails IO_Strings::getRealFormat_str_char
    MODULE PROCEDURE getRealFormat_str_char
    !> @copybrief IO_Strings::getRealFormat_str_str
    !> @copydetails IO_Strings::getRealFormat_str_str
    MODULE PROCEDURE getRealFormat_str_str
  ENDINTERFACE getRealFormat

  !> @brief Generic interface to SlashRep
  !>
  !> Allows for SlashRep to be called with intrinsic
  !> character types or a StringType
  INTERFACE SlashRep
    !> @copybrief IO_Strings::SlashRep_s
    !> @copydetails IO_Strings::SlashRep_s
    MODULE PROCEDURE SlashRep_s
    !> @copybrief IO_Strings::SlashRep_c
    !> @copydetails IO_Strings::SlashRep_c
    MODULE PROCEDURE SlashRep_c
  ENDINTERFACE SlashRep

  !> @brief Generic interface for converting variables to strings
  INTERFACE str
    !> @copybrief IO_Strings::str_SNK
    !> @copydetails IO_Strings::str_SNK
    MODULE PROCEDURE str_SNK
    !> @copybrief IO_Strings::str_SNK_pad
    !> @copydetails IO_Strings::str_SNK_pad
    MODULE PROCEDURE str_SNK_pad
    !> @copybrief IO_Strings::str_SLK
    !> @copydetails IO_Strings::str_SLK
    MODULE PROCEDURE str_SLK
    !> @copybrief IO_Strings::str_SLK_pad
    !> @copydetails IO_Strings::str_SLK_pad
    MODULE PROCEDURE str_SLK_pad
    !> @copybrief IO_Strings::str_SSK
    !> @copydetails IO_Strings::str_SSK
    MODULE PROCEDURE str_SSK
    !> @copybrief IO_Strings::str_SSK_nDecimal
    !> @copydetails IO_Strings::str_SSK_nDecimal
    MODULE PROCEDURE str_SSK_nDecimal
    !> @copybrief IO_Strings::str_SDK
    !> @copydetails IO_Strings::str_SDK
    MODULE PROCEDURE str_SDK
    !> @copybrief IO_Strings::str_SDK_nDecimal
    !> @copydetails IO_Strings::str_SDK_nDecimal
    MODULE PROCEDURE str_SDK_nDecimal
  ENDINTERFACE str
!
!===============================================================================
  CONTAINS
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
            ALLOCATE(substring%s(sublen))
            substring%n=sublen
            DO i=stt,stp
              substring%s(i-stt+1)=string%s(i)
            ENDDO
            substring%ntrim=substring%n
            DO i=sublen,1,-1
              IF(substring%s(i) == ' ') THEN
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
        substring=' '
      ENDIF
    ENDSUBROUTINE getSubstring
!
!-------------------------------------------------------------------------------
!> Strips the end of a line based on the comment symbol (BANG)
!> @param string the character array to strip any ending comments from
!>
    PURE SUBROUTINE stripComment_char(string)
      CHARACTER(LEN=*),INTENT(INOUT) :: string
      INTEGER(SIK) :: stt,stp
      INTEGER(SIK),ALLOCATABLE :: bangloc(:)

      stt=1
      stp=LEN(string)
      IF(strmatch(string,BANG)) THEN
        !Determine the location of the first '!' character and remove all
        !text after this character (including the '!' character)
        CALL strfind(string,BANG,bangloc)
        stp=bangloc(1)-1
        DEALLOCATE(bangloc)
      ENDIF
      string=string(stt:stp)
    ENDSUBROUTINE stripComment_char
!
!-------------------------------------------------------------------------------
!> Strips the end of a line based on the comment symbol (BANG)
!> @param string the StringType object to strip any ending comments from
!>
    PURE SUBROUTINE stripComment_string(string)
      TYPE(StringType),INTENT(INOUT) :: string
      TYPE(StringType) :: tempString
      INTEGER(SIK) :: stt,stp
      INTEGER(SIK),ALLOCATABLE :: bangloc(:)

      stt=1
      stp=LEN(string)
      IF(strmatch(string,BANG)) THEN
        !Determine the location of the first '!' character and remove all
        !text after this character (including the '!' character)
        CALL strfind(CHAR(string),BANG,bangloc)
        stp=bangloc(1)-1
        DEALLOCATE(bangloc)
      ENDIF
      CALL getSubString(string,tempString,stt,stp)
      string=tempString
    ENDSUBROUTINE stripComment_string
!
!-------------------------------------------------------------------------------
!> @brief Function returns the indices in a string where substring pattern
!> is found.
!> @param string the string to search
!> @param pattern the substring to look for within string
!> @returns indices a vector with the indices in string where pattern starts
!>
    PURE SUBROUTINE strfind(string,pattern,indices)
      CHARACTER(LEN=*),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(IN) :: pattern
      INTEGER(SIK),ALLOCATABLE,INTENT(OUT) :: indices(:)
      INTEGER(SIK) :: i,n

      n=0
      ALLOCATE(indices(nmatchstr(string,pattern)))
      DO i=1,LEN(string)
        IF(i+LEN(pattern)-1 > LEN(string)) EXIT
        IF(string(i:i+LEN(pattern)-1) == pattern) THEN
          n=n+1
          indices(n)=i
        ENDIF
      ENDDO
    ENDSUBROUTINE strfind
!
!-------------------------------------------------------------------------------
!> @brief Returns the total number of times the substring @c pattern is found in
!> @c string.
!> @param string the string to search
!> @param pattern the substring to match
!> @returns n the number of times @c pattern was found in @c string
!>
!> @note Does not handle trailing spaces that can be eliminated by TRIM() so
!> strings should be trimmed when passing into function.
!>
    PURE FUNCTION nmatchstr(string,pattern) RESULT(n)
      CHARACTER(LEN=*),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(IN) :: pattern
      INTEGER(SIK) :: i,n

      n=0
      DO i=1,LEN(string)
        IF(i+LEN(pattern)-1 > LEN(string)) EXIT
        IF(string(i:i+LEN(pattern)-1) == pattern) n=n+1
      ENDDO
    ENDFUNCTION nmatchstr
!
!-------------------------------------------------------------------------------
!> @brief Returns whether or not a substring @c pattern is found within @c
!> string
!> @param string the string to search
!> @param pattern the substring to find
!> @returns bool whether or not @c pattern was found in @c string
!>
!> @note Does not handle trailing spaces that can be eliminated by TRIM() so
!> strings should be trimmed when passing into function.
!>
    PURE FUNCTION strmatch_char(string,pattern) RESULT(bool)
      CHARACTER(LEN=*),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(IN) :: pattern
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i

      bool=.FALSE.
      IF(LEN(pattern) > 0) THEN
        DO i=1,LEN(string)
          IF(i+LEN(pattern)-1 > LEN(string)) EXIT
          IF(string(i:i+LEN(pattern)-1) == pattern) THEN
            bool=.TRUE.
            EXIT
          ENDIF
        ENDDO
      ENDIF
    ENDFUNCTION strmatch_char
!
!-------------------------------------------------------------------------------
!> @brief Returns whether or not a substring @c pattern is found within @c
!> string
!> @param string the string to search
!> @param pattern the substring to find
!> @returns bool whether or not @c pattern was found in @c string
!>
!> @note Does not handle trailing spaces that can be eliminated by TRIM() so
!> strings should be trimmed when passing into function.
!>
    PURE FUNCTION strmatch_string(string,pattern) RESULT(bool)
      TYPE(StringType),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(IN) :: pattern
      LOGICAL(SBK) :: bool

      bool=strmatch_char(CHAR(string),pattern)
    ENDFUNCTION strmatch_string
!
!-------------------------------------------------------------------------------
!> @brief Replaces a substring pattern with a different substring in a string
!> @param string the string which will have substrings replaced
!> @param findp the substring pattern to find and replace
!> @param repp the new substring that will be replace parts of string
!> @c repp can be larger than @c findp and as long as the size of string can
!> accomodate the increased length of all replacements. Trailing and preceding
!> spaces are counted in all strings.
!>
    PURE SUBROUTINE strrep_char_char_char(string,findp,repp)
      CHARACTER(LEN=*),INTENT(INOUT) :: string
      CHARACTER(LEN=*),INTENT(IN) :: findp
      CHARACTER(LEN=*),INTENT(IN) :: repp
      CHARACTER(LEN=LEN(string)) :: string2
      INTEGER(SIK),ALLOCATABLE :: indices(:)
      INTEGER(SIK) :: i,n,stt,stp,dlen,slen,rlen,flen,tlen

      slen=LEN(string)
      tlen=LEN_TRIM(string)
      rlen=LEN(repp)
      flen=LEN(findp)
      dlen=rlen-flen
      string2=string
      n=nmatchstr(string,findp)
      CALL strfind(string,findp,indices)
      IF(slen >= tlen+n*dlen) THEN
        DO i=1,n
          stt=indices(i)
          stp=stt+rlen-1
          string(stt:stp)=repp
          string(stp+1:slen)=string2(stt+flen-(i-1)*dlen:slen)
          IF(i < n) THEN
            indices(i+1)=indices(i+1)+dlen*i
          ENDIF
        ENDDO
      ENDIF
      DEALLOCATE(indices)
    ENDSUBROUTINE strrep_char_char_char
!
!-------------------------------------------------------------------------------
!> @brief Replaces a substring pattern with a different substring in a string
!> @param string the string which will have substrings replaced
!> @param findp the substring pattern to find and replace
!> @param repp the new substring that will be replace parts of string
!>
!> Adapter for @ref strrep_char_char_char so that a string may be passed.
!>
    PURE SUBROUTINE strrep_string_char_char(string,findp,repp)
      TYPE(StringType),INTENT(INOUT) :: string
      CHARACTER(LEN=*),INTENT(IN) :: findp
      CHARACTER(LEN=*),INTENT(IN) :: repp
      CHARACTER(LEN=string%n) :: tmp
      tmp=string
      CALL strrep_char_char_char(tmp,findp,repp)
      string=tmp
    ENDSUBROUTINE strrep_string_char_char
!
!-------------------------------------------------------------------------------
!> @brief Utility function takes a string and converts all lower case letters to
!> upper case letters.
!> @param word input is a string, output has all upper case letters
!>
    PURE SUBROUTINE toUPPER_string(word)
      TYPE(StringType),INTENT(INOUT) :: word
      INTEGER(SIK) :: i
      DO i=1,LEN(word)
        IF('a' <= word%s(i) .AND. word%s(i) <= 'z') &
          word%s(i)=ACHAR(IACHAR(word%s(i))-32)
      ENDDO
    ENDSUBROUTINE toUPPER_string
!
!-------------------------------------------------------------------------------
!> @brief Utility function takes a character array and converts all lower case
!> letters to upper case letters.
!> @param word input is a character array, output has all upper case letters
!>
    PURE SUBROUTINE toUPPER_char(word)
      CHARACTER(LEN=*),INTENT(INOUT) :: word
      INTEGER(SIK) :: i
      DO i=1,LEN(word)
        IF('a' <= word(i:i) .AND. word(i:i) <= 'z') &
          word(i:i)=ACHAR(IACHAR(word(i:i))-32)
      ENDDO
    ENDSUBROUTINE toUPPER_char
!
!-------------------------------------------------------------------------------
!> @brief Counts the number of non-blank entries on a line. An "entry" is
!> recognized as a character that is not a space that precedes a space.
!> @param aline input character array
!> @returns nfields output value for number of fields
!>
!> @note A continuous set of non-blank characters is one entry a continous set
!>       of blank space characters is one blank.
!>
    PURE FUNCTION nFields_char(aline)  RESULT(nfields)
      INTEGER(SIK) :: nfields
      CHARACTER(LEN=*),INTENT(IN) :: aline
      INTEGER(SIK) :: i,multidcol,n,ncol,nmult,ioerr
      LOGICAL(SIK) :: nonblankd,nonblank,multidata,inQuotes
!
      !Check for single-quoted strings, if the number of single quotes is odd
      !then return with a value of -1 to signal an error
      IF(MOD(nmatchstr(TRIM(aline),"'"),2) /= 0) THEN
        nfields=-1
        RETURN
      ENDIF

      !Check for double-quoted strings, if the number of double quotes is odd
      !then return with a value of -2 to signal an error
      IF(MOD(nmatchstr(TRIM(aline),'"'),2) /= 0) THEN
        nfields=-2
        RETURN
      ENDIF

      nonblankd=.FALSE.
      multidata=.FALSE.
      inQuotes=.FALSE.
      ncol=LEN_TRIM(aline)
      IF(ncol > 2) THEN
        nfields=1
      ELSE
        nfields=0
      ENDIF

      n=0
      DO i=ncol,1,-1
        IF(aline(i:i) == "'" .OR. aline(i:i) == '"') THEN
          IF(inQuotes) THEN
            inQuotes=.FALSE.
            n=n+2
            CYCLE
          ELSE
            inQuotes=.TRUE.
          ENDIF
        ENDIF
        !Process the spaces and multiplier characters if not in a quoted string
        IF(.NOT.inQuotes) THEN
          IF(aline(i:i) == ' ' .OR. ICHAR(aline(i:i)) == 9) THEN !ichar(tab)=9
            nonblank=.FALSE.
          ELSE
            IF(aline(i:i) == '*') THEN
              multidata=.TRUE.
              multidcol=i
            ENDIF
            nonblank=.TRUE.
          ENDIF
          IF((.NOT.nonblankd .AND. nonblank) .OR. &
             (nonblankd .AND. .NOT.nonblank)) THEN
            n=n+1
          ENDIF
          IF(multidata .AND. (nonblankd .AND. .NOT.nonblank)) THEN
            !ioerr will be non-zero if the sub-string is not an integer
            READ(aline(i+1:multidcol-1),*,IOSTAT=ioerr) nmult
            IF(ioerr /= 0) nmult=1
            n=n+(nmult-1)*2

            !If we are multiplying a quoted string need to subtract 1.
            IF(multidcol < ncol) THEN
              IF(aline(multidcol+1:multidcol+1) == '"' .OR. &
                 aline(multidcol+1:multidcol+1) == "'") &
                n=n-1
            ENDIF

            multidata=.FALSE.
          ENDIF
          nonblankd=nonblank
        ENDIF
      ENDDO
      IF(MOD(n,2) /= 0) THEN
        nfields=n/2+1
      ELSE
        nfields=n/2
      ENDIF
    ENDFUNCTION nFields_char
!
!-------------------------------------------------------------------------------
!> @brief Counts the number of non-blank entries on a line. An "entry" is
!> recognized as a character that is not a space that precedes a space.
!> @param aline input a StringType
!> @returns nfields output value for number of fields
!>
!> @note A continuous set of non-blank characters is one entry a continous set
!>       of blank space characters is one blank.
!>
    PURE FUNCTION nFields_string(aline) RESULT(nfields)
      INTEGER(SIK) :: nfields
      TYPE(StringType),INTENT(IN) :: aline
      nfields=nFields_char(CHAR(aline))
    ENDFUNCTION nFields_string
!
!-------------------------------------------------------------------------------
!> @brief Return the ith field of a character array as a string
!> @param i the ith field
!> @param string the input string
!> @param field the output string containing the ith field
!> @param ierrout an optional return argument with the IOSTAT value
!>
!> If i is outside the range of the number of fields it returns an empty string.
!> A bug is present such that if nFields is is greater than the actual number
!> of readable fields then the read statement will return a non-zero IOSTAT
!> value. An optional return argument was added to be able to return this
!> error value.
!>
    PURE SUBROUTINE getField_char_string(i,string,field,ierrout)
      INTEGER(SIK),INTENT(IN) :: i
      CHARACTER(LEN=*),INTENT(IN) :: string
      TYPE(StringType),INTENT(OUT) :: field
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierrout
      INTEGER(SIK) :: j,ioerr,nf
      CHARACTER(LEN=LEN(string)) :: temp,temp2

      temp=string
      temp2=''
      nf=nFields(temp)
      IF(0 < i .AND. i <= nf) THEN
        !The fortran READ(*,*) parses at the '/' character
        !we don't want this to occur. We only want it to parse for '*'
        !and ' ' characters. So if slashes are present we treat things
        !differently.
        IF(strmatch(temp,FSLASH)) THEN
          !Temporarily change the FSLASH character to a BSLASH character
          !to get correct parsing behavior
          CALL strrep(temp,FSLASH,BSLASH)
          READ(temp,*,IOSTAT=ioerr) (temp2,j=1,i)
          CALL strrep(temp,BSLASH,FSLASH)
          CALL strrep(temp2,BSLASH,FSLASH)
        ELSE
          READ(temp,*,IOSTAT=ioerr) (temp2,j=1,i)
        ENDIF
        field=TRIM(temp2)
        IF(PRESENT(ierrout)) ierrout=ioerr
      ELSE
        IF(PRESENT(ierrout)) ierrout=IOSTAT_END
      ENDIF
    ENDSUBROUTINE getField_char_string
!
!-------------------------------------------------------------------------------
!> @brief Return the ith field of a character array as a character array
!> @param i the ith field
!> @param string the input character array
!> @param field the output character array containing the ith field
!> @param ierrout an optional return argument with the IOSTAT value
!>
!> See getField_char_string.
!>
    PURE SUBROUTINE getField_char_char(i,string,field,ierrout)
      INTEGER(SIK),INTENT(IN) :: i
      CHARACTER(LEN=*),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(OUT) :: field
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierrout
      INTEGER(SIK) :: ierr
      TYPE(StringType) :: tmpField
      CALL getField_char_string(i,string,tmpField,ierr)
      field=tmpField
      IF(LEN(tmpField) > LEN(field)) THEN
        ierr=666
        field=''
      ENDIF
      IF(PRESENT(ierrout)) ierrout=ierr
    ENDSUBROUTINE getField_char_char
!
!-------------------------------------------------------------------------------
!> @brief Return the ith field of a string as a string
!> @param i the ith field
!> @param string the input string
!> @param field the output string containing the ith field
!> @param ierrout an optional return argument with the IOSTAT value
!>
!> See getField_char_string.
!>
    PURE SUBROUTINE getField_string_string(i,string,field,ierrout)
      INTEGER(SIK),INTENT(IN) :: i
      TYPE(StringType),INTENT(IN) :: string
      TYPE(StringType),INTENT(OUT) :: field
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierrout
      CALL getField_char_string(i,CHAR(string),field,ierrout)
    ENDSUBROUTINE getField_string_string
!
!-------------------------------------------------------------------------------
!> @brief Return the ith field of a string as a character array
!> @param i the ith field
!> @param string the input string
!> @param field the output character array containing the ith field
!> @param ierrout an optional return argument with the IOSTAT value
!>
!> See getField_char_string.
!>
    PURE SUBROUTINE getField_string_char(i,string,field,ierrout)
      INTEGER(SIK),INTENT(IN) :: i
      TYPE(StringType),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(OUT) :: field
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierrout
      CALL getField_char_char(i,CHAR(string),field,ierrout)
    ENDSUBROUTINE getField_string_char
!
!-------------------------------------------------------------------------------
!> @brief Return the ith field of a string as an integer
!> @param i the ith field
!> @param string the input string
!> @param field the output integer containing the ith field
!> @param ierrout an optional return argument with the IOSTAT value
!>
!> See getField_char_string.
!>
    PURE SUBROUTINE getField_string_int(i,string,field,ierrout)
      INTEGER(SIK),INTENT(IN) :: i
      TYPE(StringType),INTENT(IN) :: string
      INTEGER(SIK),INTENT(OUT) :: field
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierrout
      CHARACTER(LEN=LEN_TRIM(string)) :: char_field

      CALL getField_char_char(i,CHAR(string),char_field,ierrout)
      IF(ierrout == 0) READ(char_field,*,IOSTAT=ierrout) field
    ENDSUBROUTINE getField_string_int
!
!-------------------------------------------------------------------------------
!> @brief Return the ith field of a string as a real
!> @param i the ith field
!> @param string the input string
!> @param field the output real containing the ith field
!> @param ierrout an optional return argument with the IOSTAT value
!>
!> See getField_char_string.
!>
    PURE SUBROUTINE getField_string_real(i,string,field,ierrout)
      INTEGER(SIK),INTENT(IN) :: i
      TYPE(StringType),INTENT(IN) :: string
      REAL(SRK),INTENT(OUT) :: field
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierrout
      CHARACTER(LEN=LEN_TRIM(string)) :: char_field

      CALL getField_char_char(i,CHAR(string),char_field,ierrout)
      IF(ierrout == 0) READ(char_field,*,IOSTAT=ierrout) field
    ENDSUBROUTINE getField_string_real
!
!-------------------------------------------------------------------------------
!> @brief Separate the path and filename.
!> @param string input that is a path and filename
!> @param path output string containing just the path (includes file separator
!>        at the end)
!> @param fname output string with the filename
!> @param ext output string with the filename extension (including the '.')
!>
!> Given a path/filename string it returns the path, filename, and file
!> extension as separate strings. The full path to the file can be reconstructed
!> as @c path//filename//ext. The path is everything in string up to
!> and including the last SLASH character. The filename is everything after the
!> last SLASH and before the last DOT. If there are no slash characters the path
!> is an empty string and fname is essentially the string.
!> @par
!> The file extension is everything in string after and including the last '.'
!> character. If there is no '.' character in the file name then the extension
!> is an empty string.
!>
    SUBROUTINE getFileParts_char(string,path,fname,ext,e)
      CHARACTER(LEN=*),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(OUT) :: path
      CHARACTER(LEN=*),INTENT(OUT) :: fname
      CHARACTER(LEN=*),INTENT(OUT) :: ext
      TYPE(ExceptionHandlerType),INTENT(INOUT),OPTIONAL :: e
      INTEGER(SIK) :: i

      CALL getPath_string(string,path,e)
      CALL getFileName_string(string,fname,e)
      DO i=LEN_TRIM(fname),1,-1
        IF(fname(i:i) == DOT) THEN
          fname=fname(1:i-1)
          EXIT
        ENDIF
      ENDDO
      CALL getFileNameExt_string(string,ext,e)
    ENDSUBROUTINE getFileParts_char
!
!-------------------------------------------------------------------------------
!> @brief Separate the path and filename.
!> @param string input that is a path and filename
!> @param path output string containing just the path (includes file separator
!>        at the end)
!> @param fname output string with the filename
!> @param ext output string with the filename extension (including the '.')
!>
!> This is a wrapper routine for @ref IO_Strings::getFileParts_char
!> "getFileParts_char".
!>
!> See also: @ref IO_Strings::getFileParts_char "getFileParts_char"
!>
    SUBROUTINE getFileParts_string(string,path,fname,ext,e)
      CHARACTER(LEN=*),INTENT(IN) :: string
      TYPE(StringType),INTENT(OUT) :: path
      TYPE(StringType),INTENT(OUT) :: fname
      TYPE(StringType),INTENT(OUT) :: ext
      TYPE(ExceptionHandlerType),INTENT(INOUT),OPTIONAL :: e
      CHARACTER(LEN=LEN(string)) :: pathchar
      CHARACTER(LEN=LEN(string)) :: fnamechar
      CHARACTER(LEN=LEN(string)) :: extchar

      CALL getFileParts_char(string,pathchar,fnamechar,extchar,e)
      path=pathchar
      fname=fnamechar
      ext=extchar
    ENDSUBROUTINE getFileParts_string
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the path part of a string containing a path and
!> filename.
!> @param string input string with path and filename
!> @param path output string with the path (including a slash at the end)
!>
    SUBROUTINE getPath_string(string,path,e)
      CHARACTER(LEN=*),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(OUT) :: path
      TYPE(ExceptionHandlerType),OPTIONAL,INTENT(INOUT) :: e
      TYPE(ExceptionHandlerType) :: e2
      CHARACTER(LEN=LEN(string)) :: string2
      INTEGER(SIK) :: i

      string2=string
      CALL SlashRep(string2)
      path=''
      DO i=LEN_TRIM(string2),1,-1
        IF(string2(i:i) == SLASH) THEN
          IF(LEN_TRIM(string2(1:i)) > LEN(path)) THEN
            IF(PRESENT(e)) THEN
              CALL e%raiseError(modName//'::GETPATH_STRING - '// &
                'Path is being truncated!')
            ELSE
              CALL e2%raiseDebug(modName//'::GETPATH_STRING - '// &
                'Path is being truncated!')
            ENDIF
          ENDIF
          path=string2(1:i)
          EXIT
        ENDIF
      ENDDO
    ENDSUBROUTINE getPath_string
!
!-------------------------------------------------------------------------------
!> @brief Routine to return the filename from a string containing a path and
!> filename.
!> @param string input string with path and filename
!> @param fname output string with the filename (including extension)
!>
    SUBROUTINE getFileName_string(string,fname,e)
      CHARACTER(LEN=*),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(OUT) :: fname
      TYPE(ExceptionHandlerType) :: e2
      TYPE(ExceptionHandlerType),OPTIONAL,INTENT(INOUT) :: e
      CHARACTER(LEN=LEN(string)) :: string2
      INTEGER(SIK) :: i

      string2=string
      CALL SlashRep(string2)
      fname=string
      DO i=LEN_TRIM(string2),1,-1
        IF(string2(i:i) == SLASH) THEN
          IF(LEN_TRIM(string2(i+1:LEN_TRIM(string2))) > LEN(fname)) THEN
            IF(PRESENT(e)) THEN
              CALL e%raiseError(modName//'::GETFILENAME_STRING - '// &
                'File name is being truncated!')
            ELSE
              CALL e2%raiseDebug(modName//'::GETFILENAME_STRING - '// &
                'File name is being truncated!')
            ENDIF
          ENDIF
          fname=string2(i+1:LEN_TRIM(string2))
          EXIT
        ENDIF
      ENDDO
    ENDSUBROUTINE getFileName_string
!
!-------------------------------------------------------------------------------
!> @brief Routine to get the file name extension from a string containing a
!> filename and path.
!> @param string input string with path and filename
!> @param ext output string with the filename extension (including the '.')
!>
    SUBROUTINE getFileNameExt_string(string,ext,e)
      CHARACTER(LEN=*),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(OUT) :: ext
      TYPE(ExceptionHandlerType),OPTIONAL,INTENT(INOUT) :: e
      TYPE(ExceptionHandlerType) :: e2
      CHARACTER(LEN=LEN(string)) :: string2
      INTEGER(SIK) :: i,SLASHloc

      string2=string
      CALL SlashRep(string2)

      ext=''
      SLASHloc=1
      DO i=LEN_TRIM(string2),1,-1
        IF(string2(i:i) == SLASH) THEN
          SLASHloc=i
          EXIT
        ENDIF
      ENDDO
      DO i=LEN_TRIM(string2),SLASHloc,-1
        IF(string2(i:i) == DOT) THEN
          IF(LEN_TRIM(string2(i:LEN_TRIM(string2))) > LEN(ext)) THEN
            IF(PRESENT(e)) THEN
              CALL e%raiseError(modName//'::GETFILENAMEEXT_STRING - '// &
                'File name extension is being truncated!')
            ELSE
              CALL e2%raiseDebug(modName//'::GETFILENAMEEXT_STRING - '// &
                'File name extension is being truncated!')
            ENDIF
          ENDIF
          ext=string2(i:LEN_TRIM(string2))
          EXIT
        ENDIF
      ENDDO
    ENDSUBROUTINE getFileNameExt_string
!
!-------------------------------------------------------------------------------
!> @brief
!> @param
!>
!>
    FUNCTION printCentered(string,width) RESULT(line)
      CHARACTER(LEN=*),PARAMETER :: myName='printCentered'
      CHARACTER(LEN=*),INTENT(IN) :: string
      INTEGER(SIK),INTENT(IN):: width
      !
      INTEGER(SIK) :: stt, stp
      CHARACTER(LEN=width) :: line

      line=""
      stt=(width-LEN(string))/2
      stp=stt+LEN(string)
      line(stt:stp)=string
    ENDFUNCTION printCentered
!
!-------------------------------------------------------------------------------
!> @brief Returns whether or not a substring @c pattern is found within @c
!> string array
!> @param string the stringarray to search
!> @param pattern the substring to find
!> @returns bool whether or not @c pattern was found in @c string
!>
!> @note Does not handle trailing spaces that can be eliminated by TRIM() so
!> strings should be trimmed when passing into function.
!>
    PURE FUNCTION strarraymatch_char(string,pattern) RESULT(bool)
      CHARACTER(LEN=*),INTENT(IN) :: string(:)
      CHARACTER(LEN=*),INTENT(IN) :: pattern
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i

      bool=.FALSE.
      DO i=1,SIZE(string,DIM=1)
        bool=strmatch_char(string(i),pattern)
        IF(bool) EXIT
      ENDDO
    ENDFUNCTION strarraymatch_char
!
!-------------------------------------------------------------------------------
!> @brief Returns whether or not a substring @c pattern is found within @c
!> string array
!> @param string the stringarray to search
!> @param pattern the substring to find
!> @returns ind the index in string array @c string where the substring was
!> found.
!>
!> @note Does not handle trailing spaces that can be eliminated by TRIM() so
!> strings should be trimmed when passing into function.
!>
    PURE FUNCTION strarraymatch_string(string,pattern) RESULT(bool)
      TYPE(StringType),INTENT(IN) :: string(:)
      CHARACTER(LEN=*),INTENT(IN) :: pattern
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i

      bool=.FALSE.
      DO i=1,SIZE(string,DIM=1)
        bool=strmatch_char(CHAR(string(i)),pattern)
        IF(bool) EXIT
      ENDDO
    ENDFUNCTION strarraymatch_string
!
!-------------------------------------------------------------------------------
!> @brief Returns the index where a substring @c pattern is found within @c
!> string array.
!> @param string the stringarray to search
!> @param pattern the substring to find
!> @param lreverse the logical as to whether to perform the search in reverse.
!> @returns ind the index in string array @c string where the substring was
!> found.
!>
!> @note Does not handle trailing spaces that can be eliminated by TRIM() so
!> strings should be trimmed when passing into function.
!>
    PURE FUNCTION strarraymatchind_char(string,pattern,lreverse) RESULT(ind)
      CHARACTER(LEN=*),INTENT(IN) :: string(:)
      CHARACTER(LEN=*),INTENT(IN) :: pattern
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: lreverse
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,ind,istt,istp,incr

      ind=-1
      istt=1; istp=SIZE(string,DIM=1); incr=1
      IF(PRESENT(lreverse)) THEN
        IF(lreverse) THEN
          istt=SIZE(string,DIM=1); istp=1; incr=-1
        ENDIF
      ENDIF
      DO i=istt,istp,incr
        bool=strmatch_char(string(i),pattern)
        IF(bool) THEN
          ind=i
          EXIT
        ENDIF
      ENDDO
    ENDFUNCTION strarraymatchind_char
!
!-------------------------------------------------------------------------------
!> @brief Returns the index where a substring @c pattern is found within @c
!> string array.
!> @param string the stringarray to search
!> @param pattern the substring to find
!> @param lreverse the logical as to whether to perform the search in reverse.
!> @returns bool whether or not @c pattern was found in @c string
!>
!> @note Does not handle trailing spaces that can be eliminated by TRIM() so
!> strings should be trimmed when passing into function.
!>
    PURE FUNCTION strarraymatchind_string(string,pattern,lreverse) RESULT(ind)
      TYPE(StringType),INTENT(IN) :: string(:)
      CHARACTER(LEN=*),INTENT(IN) :: pattern
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: lreverse
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,ind,istt,istp,incr

      ind=-1
      istt=1; istp=SIZE(string,DIM=1); incr=1
      IF(PRESENT(lreverse)) THEN
        IF(lreverse) THEN
          istt=SIZE(string,DIM=1); istp=1; incr=-1
        ENDIF
      ENDIF
      DO i=istt,istp,incr
        bool=strmatch_string(string(i),pattern)
        IF(bool) THEN
          ind=i
          EXIT
        ENDIF
      ENDDO
    ENDFUNCTION strarraymatchind_string
!
!-------------------------------------------------------------------------------
!> @brief Returns the index where a substring @c pattern is found that equals @c
!> string array.
!> @param string the stringarray to search
!> @param pattern the substring to find
!> @param lreverse the logical as to whether to perform the search in reverse.
!> @returns ind the index in string array @c string where the substring was
!> found.
!>
!> @note Does not handle trailing spaces that can be eliminated by TRIM() so
!> strings should be trimmed when passing into function.
!>
    PURE FUNCTION strarrayeqind_char(string,pattern,lreverse) RESULT(ind)
      CHARACTER(LEN=*),INTENT(IN) :: string(:)
      CHARACTER(LEN=*),INTENT(IN) :: pattern
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: lreverse
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,ind,istt,istp,incr

      ind=-1
      istt=1; istp=SIZE(string,DIM=1); incr=1
      IF(PRESENT(lreverse)) THEN
        IF(lreverse) THEN
          istt=SIZE(string,DIM=1); istp=1; incr=-1
        ENDIF
      ENDIF
      DO i=istt,istp,incr
        bool=string(i) == pattern
        IF(bool) THEN
          ind=i
          EXIT
        ENDIF
      ENDDO
    ENDFUNCTION strarrayeqind_char
!
!-------------------------------------------------------------------------------
!> @brief Returns the index where a substring @c pattern is found within @c
!> string array.
!> @param string the stringarray to search
!> @param pattern the substring to find
!> @param lreverse the logical as to whether to perform the search in reverse.
!> @returns bool whether or not @c pattern was found in @c string
!>
!> @note Does not handle trailing spaces that can be eliminated by TRIM() so
!> strings should be trimmed when passing into function.
!>
    PURE FUNCTION strarrayeqind_string(string,pattern,lreverse) RESULT(ind)
      TYPE(StringType),INTENT(IN) :: string(:)
      CHARACTER(LEN=*),INTENT(IN) :: pattern
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: lreverse
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,ind,istt,istp,incr

      ind=-1
      istt=1; istp=SIZE(string,DIM=1); incr=1
      IF(PRESENT(lreverse)) THEN
        IF(lreverse) THEN
          istt=SIZE(string,DIM=1); istp=1; incr=-1
        ENDIF
      ENDIF
      DO i=istt,istp,incr
        bool=CHAR(string(i)) == pattern
        IF(bool) THEN
          ind=i
          EXIT
        ENDIF
      ENDDO
    ENDFUNCTION strarrayeqind_string
!
!-------------------------------------------------------------------------------
!> @brief Returns the index where a substring @c pattern is found within @c
!> string array.
!> @param string the stringarray to search
!> @param pattern the substring to find
!> @param lreverse the logical as to whether to perform the search in reverse.
!> @returns bool whether or not @c pattern was found in @c string
!>
!> @note Does not handle trailing spaces that can be eliminated by TRIM() so
!> strings should be trimmed when passing into function.
!>
    PURE FUNCTION strarrayeqind_string_string(string,pattern,lreverse) RESULT(ind)
      TYPE(StringType),INTENT(IN) :: string(:)
      TYPE(StringType),INTENT(IN) :: pattern
      LOGICAL(SBK),INTENT(IN),OPTIONAL :: lreverse
      LOGICAL(SBK) :: bool
      INTEGER(SIK) :: i,ind,istt,istp,incr

      ind=-1
      istt=1; istp=SIZE(string,DIM=1); incr=1
      IF(PRESENT(lreverse)) THEN
        IF(lreverse) THEN
          istt=SIZE(string,DIM=1); istp=1; incr=-1
        ENDIF
      ENDIF
      DO i=istt,istp,incr
        bool=string(i) == pattern
        IF(bool) THEN
          ind=i
          EXIT
        ENDIF
      ENDDO
    ENDFUNCTION strarrayeqind_string_string
!
!-------------------------------------------------------------------------------
!> @brief
    PURE SUBROUTINE getRealFormat_char_char(valstr,fmtstr)
      CHARACTER(LEN=*),INTENT(IN) :: valstr
      CHARACTER(LEN=*),INTENT(INOUT) :: fmtstr
      TYPE(StringType) :: vstr,fstr
      vstr=valstr
      fstr=fmtstr
      CALL getRealFormat_str_str(vstr,fstr)
      fmtstr=fstr
    ENDSUBROUTINE getRealFormat_char_char
!
!-------------------------------------------------------------------------------
!> @brief
    PURE SUBROUTINE getRealFormat_str_char(valstr,fmtstr)
      TYPE(StringType),INTENT(IN) :: valstr
      CHARACTER(LEN=*),INTENT(INOUT) :: fmtstr
      TYPE(StringType) :: fstr
      fstr=fmtstr
      CALL getRealFormat_str_str(valstr,fstr)
      fmtstr=fstr
    ENDSUBROUTINE getRealFormat_str_char
!
!-------------------------------------------------------------------------------
!> @brief
    PURE SUBROUTINE getRealFormat_char_str(valstr,fmtstr)
      CHARACTER(LEN=*),INTENT(IN) :: valstr
      TYPE(StringType),INTENT(INOUT) :: fmtstr
      TYPE(StringType) :: vstr
      vstr=valstr
      CALL getRealFormat_str_str(vstr,fmtstr)
    ENDSUBROUTINE getRealFormat_char_str
!
!-------------------------------------------------------------------------------
!> @brief
    PURE SUBROUTINE getRealFormat_str_str(valstr,fmtstr)
      TYPE(StringType),INTENT(IN) :: valstr
      TYPE(StringType),INTENT(INOUT) :: fmtstr

      CHARACTER(LEN=32) :: tmpchar
      INTEGER(SIK) :: w,d,e,ioerr
      REAL(SRK) :: tmpval
      TYPE(StringType) :: vstr

      fmtstr=''
      vstr=valstr
      IF(LEN_TRIM(vstr) == 0) RETURN
      tmpChar=vstr
      READ(tmpChar,*,IOSTAT=ioerr) tmpval
      IF(ioerr == 0) THEN
        vstr=TRIM(ADJUSTL(vstr)) !eliminate whitespace
        w=LEN(vstr)              !Get the total width
        d=0
        e=MAX(INDEX(vstr,'e'),INDEX(vstr,'E'))
        IF(e > 0) THEN
          IF(INDEX(vstr,'.') > 0) THEN
            d=e-INDEX(vstr,'.')-1 !Always one digit left of decimal
          ELSE
            w=w+1
          ENDIF
          e=LEN(vstr)-e-1
          IF(e == 0) THEN !This accounts for a degenerate case like "3.2e1"
            e=1
            w=w+1
          ENDIF
          IF(e > 0) THEN !This accounts for stupid intel for case "3.2e"
            fmtstr='es'
            WRITE(tmpchar,*) w; fmtstr=fmtstr//TRIM(ADJUSTL(tmpchar));
            WRITE(tmpchar,*) d; fmtstr=fmtstr//'.'//TRIM(ADJUSTL(tmpchar));
            WRITE(tmpchar,*) e; fmtstr=fmtstr//'e'//TRIM(ADJUSTL(tmpchar));
          ENDIF
        ELSE
          IF(INDEX(vstr,'.') > 0) THEN
            d=w-INDEX(vstr,'.')
          ELSE
            w=w+1 !Add 1 for decimal point in output
          ENDIF
          fmtstr='f'
          WRITE(tmpchar,*) w; fmtstr=fmtstr//TRIM(ADJUSTL(tmpchar));
          WRITE(tmpchar,*) d; fmtstr=fmtstr//'.'//TRIM(ADJUSTL(tmpchar));
        ENDIF
      ENDIF
    ENDSUBROUTINE getRealFormat_str_str
!
!-------------------------------------------------------------------------------
!> @brief Pure routine replaces slash character in file path names with
!> the system appropriate file separator slash.
!> @param string a StringType with the filepath
!>
!> Wraps call to SlashRep_c
    PURE SUBROUTINE SlashRep_s(string)
      TYPE(StringType),INTENT(INOUT) :: string
      CHARACTER(LEN=LEN_TRIM(string)) :: cstring

      cstring=TRIM(string)
      CALL SlashRep_c(cstring)
      string=cstring
    ENDSUBROUTINE SlashRep_s

!
!-------------------------------------------------------------------------------
!> @brief Pure routine replaces slash character in file path names with
!> the system appropriate file separator slash.
!> @param string a character array with the filepath
!>
    PURE SUBROUTINE SlashRep_c(string)
      CHARACTER(LEN=*),INTENT(INOUT) :: string
      INTEGER(SIK) :: i

      DO i=1,LEN_TRIM(string)
#ifdef WIN32
        IF(string(i:i) == FSLASH) string(i:i)=SLASH
#else
        IF(string(i:i) == BSLASH) string(i:i)=SLASH
#endif
      ENDDO
    ENDSUBROUTINE SlashRep_c
!
!-------------------------------------------------------------------------------
!> @brief Converts an integer to a character
!> @param i the value to convert
!> @returns string the string
!>
!> Prints with no white space
!>
    FUNCTION str_SNK(i) RESULT(string)
      INTEGER(SNK),INTENT(IN) :: i
      CHARACTER(LEN=:),ALLOCATABLE :: string
      !
      INTEGER(SIK) :: length

      length=FLOOR(ABS(LOG10(ABS(REAL(i)))))+1
      IF(i < 0) length=length+1
      ALLOCATE(CHARACTER(length) :: string)
      WRITE(string,'(i0)') i

    ENDFUNCTION str_SNK
!
!
!-------------------------------------------------------------------------------
!> @brief Converts an integer to a character and pads with leading 0s
!> @param i the value to convert
!> @param nPadZero the number of leading 0s to pad with
!> @returns string the string
!>
!> Prints with no white space and pads with leading 0s so that the string is
!> @c nPadZero in length (not counting a possible negative sign)
!>
    FUNCTION str_SNK_pad(i,nPadZero) RESULT(string)
      INTEGER(SNK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: nPadZero
      CHARACTER(LEN=:),ALLOCATABLE :: string
      !
      INTEGER(SIK) :: length

      length=FLOOR(ABS(LOG10(ABS(REAL(i)))))+1
      REQUIRE(nPadZero >= length)

      length=nPadZero
      IF(i < 0) THEN
        ALLOCATE(CHARACTER(length+1) :: string)
      ELSE
        ALLOCATE(CHARACTER(length) :: string)
      ENDIF
      WRITE(string,'(i0.'//str(length)//')') i

    ENDFUNCTION str_SNK_pad
!
!-------------------------------------------------------------------------------
!> @brief Converts a long integer to a character
!> @param i the value to convert
!> @returns string the string
!>
!> Prints with no white space
!>
    FUNCTION str_SLK(i) RESULT(string)
      INTEGER(SLK),INTENT(IN) :: i
      CHARACTER(LEN=:),ALLOCATABLE :: string
      !
      INTEGER(SIK) :: length

      length=FLOOR(ABS(LOG10(ABS(REAL(i)))))+1
      IF(i < 0) length=length+1
      ALLOCATE(CHARACTER(length) :: string)
      WRITE(string,'(i0)') i

    ENDFUNCTION str_SLK
!
!
!-------------------------------------------------------------------------------
!> @brief Converts a long integer to a character and pads with leading 0s
!> @param i the value to convert
!> @param nPadZero the number of leading 0s to pad with
!> @returns string the string
!>
!> Prints with no white space and pads with leading 0s so that the string is
!> @c nPadZero in length (not counting a possible negative sign)
!>
    FUNCTION str_SLK_pad(i,nPadZero) RESULT(string)
      INTEGER(SLK),INTENT(IN) :: i
      INTEGER(SIK),INTENT(IN) :: nPadZero
      CHARACTER(LEN=:),ALLOCATABLE :: string
      !
      INTEGER(SIK) :: length

      length=FLOOR(ABS(LOG10(ABS(REAL(i)))))+1
      REQUIRE(nPadZero >= length)

      length=nPadZero
      IF(i < 0) THEN
        ALLOCATE(CHARACTER(length+1) :: string)
      ELSE
        ALLOCATE(CHARACTER(length) :: string)
      ENDIF
      WRITE(string,'(i0.'//str(length)//')') i

    ENDFUNCTION str_SLK_pad
!
!-------------------------------------------------------------------------------
!> @brief Converts a single precision real to a character
!> @param r the value to convert
!> @returns string the string
!>
!> Prints in scientific notation with no white space.
!>
    FUNCTION str_SSK(r) RESULT(string)
      REAL(SSK),INTENT(IN) :: r
      CHARACTER(LEN=:),ALLOCATABLE :: string
      !
      INTEGER(SIK) :: length

      IF(r < 0.0_SSK) THEN
        length=14
      ELSE
        length=13
      ENDIF
      ALLOCATE(CHARACTER(length) :: string)
      WRITE(string,'(es'//str(length)//'.7)') r

    ENDFUNCTION str_SSK
!> @brief Determines whether a character is in the alphabet using ASCII format
!> @param letter character to check
!> @returns isValid logical representing if the letter is an alphabetic 
!> character
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
!> @returns isValid logical representing if the letter is a capital alphabetic 
!> character
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
!> @returns isValid logical representing if the letter is a lower alphabetic 
!> character
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
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of a character
!> string to an array of strings
!> @param dArr the array of strings
!> @param c the character value
!    SUBROUTINE charToStringArray(sArr,c)
!      TYPE(StringType),ALLOCATABLE,INTENT(OUT) :: sArr(:)
!      TYPE(StringType),INTENT(IN) :: c
!      CHARACTER(LEN=100) :: tmpStr
!      TYPE(StringType) :: tmpElt
!      INTEGER(SIK) :: numElts
!      INTEGER(SIK) :: i,j,k
!
!-------------------------------------------------------------------------------
!> @brief Converts a single precision real to a character
!> @param r the value to convert
!> @param nDecimal the number of decimal places
!> @returns string the string
!>
!> Prints in scientific notation with the number of decimal places specified by
!> @c nDecimal and no white space.
!>
    FUNCTION str_SSK_nDecimal(r,nDecimal) RESULT(string)
      REAL(SSK),INTENT(IN) :: r
      INTEGER(SIK),INTENT(IN) :: nDecimal
      CHARACTER(LEN=:),ALLOCATABLE :: string
      !
      INTEGER(SIK) :: length

      REQUIRE(nDecimal >= 0)

      length=nDecimal+6
      IF(r < 0.0_SSK) length=length+1
      ALLOCATE(CHARACTER(length) :: string)
      WRITE(string,'(es'//str(length)//'.'//str(nDecimal)//')') r

    ENDFUNCTION str_SSK_nDecimal
!
!-------------------------------------------------------------------------------
!> @brief Converts a double precision real to a character
!> @param r the value to convert
!> @returns string the string
!>
!> Prints in scientific notation with no white space.
!>
    FUNCTION str_SDK(r) RESULT(string)
      REAL(SDK),INTENT(IN) :: r
      CHARACTER(LEN=:),ALLOCATABLE :: string
      !
      INTEGER(SIK) :: length

      IF(r < 0.0_SSK) THEN
        length=22
      ELSE
        length=21
      ENDIF
      ALLOCATE(CHARACTER(length) :: string)
      WRITE(string,'(es'//str(length)//'.15)') r

    ENDFUNCTION str_SDK
!
!-------------------------------------------------------------------------------
!> @brief Converts a double precision real to a character
!> @param r the value to convert
!> @param nDecimal the number of decimal places
!> @returns string the string
!>
!> Prints in scientific notation with the number of decimal places specified by
!> @c nDecimal and no white space.
!>
    FUNCTION str_SDK_nDecimal(r,nDecimal) RESULT(string)
      REAL(SDK),INTENT(IN) :: r
      INTEGER(SIK),INTENT(IN) :: nDecimal
      CHARACTER(LEN=:),ALLOCATABLE :: string
      !
      INTEGER(SIK) :: length

      REQUIRE(nDecimal >= 0)

      length=nDecimal+6
      IF(r < 0.0_SDK) length=length+1
      ALLOCATE(CHARACTER(length) :: string)
      WRITE(string,'(es'//str(length)//'.'//str(nDecimal)//')') r

    ENDFUNCTION str_SDK_nDecimal
!
ENDMODULE IO_Strings
