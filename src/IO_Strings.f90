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
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief Exceptionhandler
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
!> !Returns the indeces in '/home/usr/dir/tmp' where '/' is found
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
!> @author Brendan Kochunas
!>   @date 07/05/2011
!> 
!> @todo
!>  - Add optional inputs to nFields and getField to specify delimiter symbols
!>    and repeater symbols.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE IO_Strings
  
  USE IntrType
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
  PUBLIC :: nFields
  PUBLIC :: getField
  PUBLIC :: toUPPER
  PUBLIC :: strfind
  PUBLIC :: strmatch
  PUBLIC :: nmatchstr
  PUBLIC :: strrep
  PUBLIC :: stripComment
      
  !> Character representing a space symbol
  CHARACTER(LEN=1),PARAMETER :: BLANK=" "
  !> Character representing a comment symbol
  CHARACTER(LEN=1),PARAMETER :: BANG="!"
  !> Character representing a period
  CHARACTER(LEN=1),PARAMETER :: DOT="."
  !> Character representing a forward slash
  CHARACTER(LEN=1),PARAMETER :: FSLASH="/"
  !> Character representing a backward slash
  CHARACTER(LEN=1),PARAMETER :: BSLASH="\"
  !> Character representing a colon
  CHARACTER(LEN=1),PARAMETER :: COLON=":"
  !"This is needed for doxygen to parse correctly
#ifdef WIN32
  !> The slash symbol used by the file system
  !> (BLASH for Windows, FSLASH for everything else)
  CHARACTER(LEN=1),PARAMETER :: SLASH=BSLASH
#else
  !> The slash symbol used by the file system
  !> (BLASH for Windows, FSLASH for everything else)
  CHARACTER(LEN=1),PARAMETER :: SLASH=FSLASH
#endif
  
  !> Module Name for exception handler
  CHARACTER(LEN=10),PARAMETER :: modName='IO_STRINGS'
      
  !> @brief Generic interface to get the parts of the path-file name.
  !>
  !> This interfaces is presently redundant but is provided on the
  !> assumption that it may be needed later.
  INTERFACE getFileParts
    !> @copybrief IO_Strings::getFileParts_string
    !> @copydetails IO_Strings::getFileParts_string
    MODULE PROCEDURE getFileParts_string
  ENDINTERFACE
      
  !> @brief Generic interface to get the file name.
  !>
  !> This interfaces is presently redundant but is provided on the
  !> assumption that it may be needed later.
  INTERFACE getFilePath
    !> @copybrief IO_Strings::getPath_string
    !> @copydetails IO_Strings::getPath_string
    MODULE PROCEDURE getPath_string
  ENDINTERFACE
      
  !> @brief Generic interface to get the file name.
  !>
  !> This interfaces is presently redundant but is provided on the
  !> assumption that it may be needed later.
  INTERFACE getFileName
    !> @copybrief IO_Strings::getFileName_string
    !> @copydetails IO_Strings::getFileParts_string
    MODULE PROCEDURE getFileName_string
  ENDINTERFACE
      
  !> @brief Generic interface to get the extension of a file name.
  !>
  !> This interfaces is presently redundant but is provided on the
  !> assumption that it may be needed later.
  INTERFACE getFileNameExt
    !> @copybrief IO_Strings::getFileNameExt_string
    !> @copydetails IO_Strings::getFileNameExt_string
    MODULE PROCEDURE getFileNameExt_string
  ENDINTERFACE
!
!===============================================================================      
CONTAINS
!
!-------------------------------------------------------------------------------
!> Strips the end of a line based on the comment symbol (BANG)
!> @param string the string to strip any ending comments from
    PURE SUBROUTINE stripComment(string)
      CHARACTER(LEN=*),INTENT(INOUT) :: string
      INTEGER(SIK) :: nBang,stt,stp
      INTEGER(SIK),ALLOCATABLE :: bangloc(:)
      
      stt=1
      stp=LEN(string)
      IF(strmatch(string,BANG)) THEN
        !Determine the location of the first '!' character and remove all
        !text after this character (including the '!' character)
        nBANG=nmatchstr(string,BANG)
        ALLOCATE(bangloc(nBANG))
        bangloc=strfind(string,BANG)
        stp=bangloc(1)-1
        DEALLOCATE(bangloc)          
      ENDIF
      string=string(stt:stp)
    ENDSUBROUTINE stripComment
!
!-------------------------------------------------------------------------------
!> @brief Function returns the indeces in a string where substring pattern
!> is found.
!> @param string the string to search
!> @param pattern the substring to look for within string
!> @returns indeces a vector with the indeces in string where pattern starts
    PURE FUNCTION strfind(string,pattern) RESULT(indeces)
      CHARACTER(LEN=*),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(IN) :: pattern
      INTEGER(SIK) :: indeces(nmatchstr(string,pattern))
      INTEGER(SIK) :: i,n
      
      n=0
      DO i=1,LEN(string)
        IF(i+LEN(pattern)-1 > LEN(string)) EXIT
        IF(string(i:i+LEN(pattern)-1) == pattern) THEN
          n=n+1
          indeces(n)=i
        ENDIF
      ENDDO
    ENDFUNCTION strfind
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
    PURE FUNCTION strmatch(string,pattern) RESULT(bool)
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
    ENDFUNCTION strmatch
!
!-------------------------------------------------------------------------------
!> @brief Replaces a substring pattern with a different substring in a string
!> @param string the string which will have substrings replaced
!> @param findp the substring pattern to find and replace
!> @param repp the new substring that will be replace parts of string
!> @c repp can be larger than @c findp and as long as the size of string can
!> accomodate the increased length of all replacements. Trailing and preceding
!> spaces are counted in all strings.
    PURE SUBROUTINE strrep(string,findp,repp)
      CHARACTER(LEN=*),INTENT(INOUT) :: string
      CHARACTER(LEN=*),INTENT(IN) :: findp
      CHARACTER(LEN=*),INTENT(IN) :: repp
      CHARACTER(LEN=LEN(string)) :: string2
      INTEGER(SIK) :: n,indeces(nmatchstr(string,findp))
      INTEGER(SIK) :: i,stt,stp,dlen,slen,rlen,flen,tlen
      
      slen=LEN(string)
      tlen=LEN_TRIM(string)
      rlen=LEN(repp)
      flen=LEN(findp)
      dlen=rlen-flen
      string2=string
      n=nmatchstr(string,findp)
      indeces=strfind(string,findp)
      IF(slen >= tlen+n*dlen) THEN
        DO i=1,n
          stt=indeces(i)
          stp=stt+rlen-1
          string(stt:stp)=repp
          string(stp+1:slen)=string2(stt+flen-(i-1)*dlen:slen)
          IF(i < n) THEN
            indeces(i+1)=indeces(i+1)+dlen
          ENDIF
        ENDDO
      ENDIF
    ENDSUBROUTINE strrep
!
!-------------------------------------------------------------------------------
!> @brief Utility function takes a string and converts all lower case letters to
!> upper case letters.
!> @param word input is a string, output has all upper case letters
    PURE SUBROUTINE toUPPER(word)
      CHARACTER(LEN=*),INTENT(INOUT) :: word
      INTEGER(SIK) :: i
      DO i=1,LEN(word)
        IF('a' <= word(i:i) .AND. word(i:i) <= 'z') THEN
          word(i:i)=ACHAR(IACHAR(word(i:i))-32)
        ENDIF
      ENDDO
    ENDSUBROUTINE toUPPER
!
!-------------------------------------------------------------------------------
!> @brief Counts the number of non-blank entries on a line. An "entry" is 
!> recognized as a character that is not a space that precedes a space.
!> @param aline input string
!> @returns nfields output value for number of fields
!>
!> @note A continuous set of non-blank characters is one entry a continous set
!>       of blank space characters is one blank.
    PURE FUNCTION nFields(aline)
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
            n=n+1
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
    ENDFUNCTION nFields
!
!-------------------------------------------------------------------------------
!> @brief Return the ith field of a string as a string
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
    PURE SUBROUTINE getField(i,string,field,ierrout)
      INTEGER(SIK),INTENT(IN) :: i
      CHARACTER(LEN=*),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(OUT) :: field
      INTEGER(SIK),INTENT(OUT),OPTIONAL :: ierrout
      INTEGER(SIK) :: j,ioerr,nf
      CHARACTER(LEN=LEN(string)) :: temp
                 
      field=''
      temp=string
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
          READ(temp,*,IOSTAT=ioerr) (field,j=1,i)
          CALL strrep(temp,BSLASH,FSLASH)
          CALL strrep(field,BSLASH,FSLASH)
        ELSE
          READ(temp,*,IOSTAT=ioerr) (field,j=1,i)
        ENDIF
        IF(ioerr /= 0) field=''
        IF(PRESENT(ierrout)) ierrout=ioerr
      ENDIF
    ENDSUBROUTINE getField
!
!-------------------------------------------------------------------------------
!> @name Private Functions/Subroutines
!< @{
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
    SUBROUTINE getFileParts_string(string,path,fname,ext,e)
      CHARACTER(LEN=*),INTENT(IN) :: string
      CHARACTER(LEN=*),INTENT(OUT) :: path
      CHARACTER(LEN=*),INTENT(OUT) :: fname
      CHARACTER(LEN=*),INTENT(OUT) :: ext
      INTEGER(SIK) :: i
      TYPE(ExceptionHandlerType),OPTIONAL :: e

      IF(PRESENT(e)) THEN
        CALL getPath_string(string,path,e)
        CALL getFileName_string(string,fname,e)
      ELSE
        CALL getPath_string(string,path)
        CALL getFileName_string(string,fname)
      ENDIF
      DO i=LEN_TRIM(fname),1,-1
        IF(fname(i:i) == DOT) THEN
          fname=fname(1:i-1)
          EXIT
        ENDIF
      ENDDO
      IF(PRESENT(e)) THEN
        CALL getFileNameExt_string(string,ext,e)
      ELSE
        CALL getFileNameExt_string(string,ext)
      ENDIF
    ENDSUBROUTINE getFileParts_string
!
!-------------------------------------------------------------------------------
!> @brief Routine returns the path part of a string containing a path and
!> filename.
!> @param string input string with path and filename
!> @param path output string with the path (including a slash at the end)
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
              CALL e2%raiseWarning(modName//'::GETPATH_STRING - '// &
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
              CALL e2%raiseWarning(modName//'::GETFILENAME_STRING - '// &
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
              CALL e2%raiseWarning(modName//'::GETFILENAMEEXT_STRING - '// &
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
!> @brief Private routine replaces slash character in file path names with 
!> the system appropriate file separator slash.
    PURE SUBROUTINE SlashRep(string)
      CHARACTER(LEN=*),INTENT(INOUT) :: string
      INTEGER(SIK) :: i

      DO i=1,LEN_TRIM(string)
#ifdef WIN32
        IF(string(i:i) == FSLASH) string(i:i)=SLASH
#else
        IF(string(i:i) == BSLASH) string(i:i)=SLASH
#endif
      ENDDO
    ENDSUBROUTINE SlashRep
!> @}
! 
ENDMODULE IO_Strings
