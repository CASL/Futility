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
PROGRAM testIOutil
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE Strings
  USE ExceptionHandler
  USE IOutil
  IMPLICIT NONE
  
  TYPE(FortranFileType) :: testFile,testFile2
  TYPE(LogFileType) :: testLogFile
  TYPE(InputFileType) :: testInpFile
  CHARACTER(LEN=MAX_INPUT_FILE_LINE_LEN) :: string,string1,string2,string3
  CHARACTER(LEN=1) :: shortstring1,shortstring2,shortstring3
  INTEGER :: ioerr,i
  LOGICAL :: lexist
  TYPE(ExceptionHandlerType),TARGET :: e
      
  CREATE_TEST('IOutil')
  
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  
  REGISTER_SUBTEST('PARAMETERS',testParameters)
  REGISTER_SUBTEST('IO_Strings',testIO_Strings)
  REGISTER_SUBTEST('FileType_Base',testBaseFileType)
  REGISTER_SUBTEST('FileType_Fortran',testFortranFileType)
  REGISTER_SUBTEST('FileType_Log',testLogFileType)
  REGISTER_SUBTEST('FileType_Input',testInputFileType)
  REGISTER_SUBTEST('Run-time Environment',testRTEnv)
  
  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testParameters()
      WRITE(*,*) '  Passed:          MAX_PATH_LENGTH = ',MAX_PATH_LENGTH
      WRITE(*,*) '  Passed:         MAX_FNAME_LENGTH = ',MAX_FNAME_LENGTH
      WRITE(*,*) '  Passed:          MAX_FEXT_LENGTH = ',MAX_FEXT_LENGTH
      WRITE(*,*) '  Passed:  MAX_INPUT_FILE_LINE_LEN = ',MAX_INPUT_FILE_LINE_LEN
      WRITE(*,*) '  Passed:                    BLANK = '//BLANK
      WRITE(*,*) '  Passed:                     BANG = '//BANG
      WRITE(*,*) '  Passed:                      DOT = '//DOT
      WRITE(*,*) '  Passed:                   FSLASH = '//FSLASH
      WRITE(*,*) '  Passed:                   BSLASH = '//BSLASH
      WRITE(*,*) '  Passed:                    SLASH = '//SLASH
    ENDSUBROUTINE testParameters
!
!-------------------------------------------------------------------------------
    SUBROUTINE testIO_Strings()
      LOGICAL :: bool
      INTEGER :: stat
      CHARACTER(LEN=32) :: char
      TYPE(StringType) :: tmpStr,tmpStr2,tmpStrArray(10)
      
      COMPONENT_TEST('strmatch')
      ASSERT(strmatch('testing','test'),'testing')
      ASSERT(.NOT.(strmatch('TEAM','I')),'TEAM')
      
      COMPONENT_TEST('strarraymatch')
      DO stat=1,10
        WRITE(char,'(i32)') stat-1; char=ADJUSTL(char)
        tmpStrArray(stat)='test'//TRIM(char)
      ENDDO
      ASSERT(strarraymatch(tmpStrArray,'test'),'testing')
      ASSERT(strarraymatch(tmpStrArray,'1'),'testing 1')
      ASSERT(.NOT.strarraymatch(tmpStrArray,'11'),'testing 11')
      
      COMPONENT_TEST('strarraymatchind')
      ASSERT(strarraymatchind(tmpStrArray,'test') == 1,'testing')
      ASSERT(strarraymatchind(tmpStrArray,'1') == 2,'testing 1')
      ASSERT(strarraymatchind(tmpStrArray,'9') == 10,'testing 9')
      ASSERT(strarraymatchind(tmpStrArray,'11') == -1,'testing 11')
      
      COMPONENT_TEST('nmatchstr')
      ASSERT(nmatchstr('testing','test') == 1,'testing')
      ASSERT(nmatchstr('TEAM','I') == 0,'TEAM')
      ASSERT(nmatchstr('t e s t',' ') == 3 ,'t e s t')
      
      COMPONENT_TEST('strfind')
      ASSERT(ALL(strfind('stesting','test') == (/2/)),'testing')
      ASSERT(SIZE(strfind('TEAM','I')) == 0,'TEAM')
      ASSERT(ALL(strfind('t e s t',' ') == (/2,4,6/)),'t e s t')
      
      COMPONENT_TEST('strrep')
      string='testing'
      CALL strrep(string,'t','TT')
      ASSERT(TRIM(string) == 'TTesTTing','testing')
      tmpStr='testing  '
      CALL strrep(tmpStr,'t','TT')
      ASSERT(TRIM(tmpStr) == 'TTesTTing','testing')
      
      COMPONENT_TEST('nFields (character)')
      string=''
      ASSERT(nFields(string) == 0,'blank')
      string=' '
      ASSERT(nFields(string) == 0,'" "')
      string='''arg1'
      ASSERT(nFields(string) == -1,'bad single-quote')
      string='"arg1'
      ASSERT(nFields(string) == -2,'bad double-quote')
      string='1'
      ASSERT(nFields(string) == 1,'1')
      string='arg1 nmult*arg2'
      ASSERT(nFields(string) == 2,'bad multi-arg')
      string='arg1 2*arg2'
      ASSERT(nFields(string) == 3,'good multi-arg')
      string='arg1 2*"arg 2"'
      ASSERT(nFields(string) == 3,'good "multi-arg"')
      string='cardname 1 1 2 3 1 0.2'
      ASSERT(nFields(string) == 7,'card')
      string='     cardname 200*3.1 15*0.2'
      ASSERT(nFields(string) == 216,'card multi-arg')
      
      COMPONENT_TEST('nFields (string)')
      tmpStr=''
      ASSERT(nFields(tmpStr) == 0,'blank')
      tmpStr=' '
      ASSERT(nFields(tmpStr) == 0,'" "')
      tmpStr='''arg1'
      ASSERT(nFields(tmpStr) == -1,'bad single-quote')
      tmpStr='"arg1'
      ASSERT(nFields(tmpStr) == -2,'bad double-quote')
      tmpStr='1'
      ASSERT(nFields(tmpStr) == 1,'1')
      tmpStr='arg1 nmult*arg2'
      ASSERT(nFields(tmpStr) == 2,'bad multi-arg')
      tmpStr='arg1 2*arg2'
      ASSERT(nFields(tmpStr) == 3,'good multi-arg')
      tmpStr='arg1 2*"arg 2"'
      ASSERT(nFields(tmpStr) == 3,'good "multi-arg"')
      tmpStr='cardname 1 1 2 3 1 0.2'
      ASSERT(nFields(tmpStr) == 7,'card')
      tmpStr='     cardname 200*3.1 15*0.2'
      ASSERT(nFields(tmpStr) == 216,'card multi-arg')
      
      COMPONENT_TEST('getField (char,char)')
      string='     cardname 200*3.1 15*0.2'
      CALL getField(50,string,string2,stat)
      ASSERT(TRIM(string2) == '3.1','50th (field)')
      ASSERT(stat == 0,'50th (stat)')
      CALL getField(205,string,string2,stat)
      ASSERT(TRIM(string2) == '0.2','205th (field)')
      ASSERT(stat == 0,'205th (stat)')
      string='cardname 1 1 2 3 1 0.2'
      CALL getField(50,string,string2,stat)
      ASSERT(LEN_TRIM(string2) == 0,'too many (field)')
      ASSERT(stat == IOSTAT_END,'too many (stat)')
      CALL getField(5,string,string2,stat)
      ASSERT(TRIM(string2) == '3','normal (field)')
      ASSERT(stat == 0,'normal (stat)')
      string='''/some dir/file'' arg2'
      CALL getField(1,string,string2,stat)
      ASSERT(TRIM(string2) == '/some dir/file','filepath (field)')
      ASSERT(stat == 0,'filepath (stat)')
      CALL getField(1,string,shortstring1,stat)
      CALL getField(1,string,shortstring1)
      ASSERT(TRIM(shortstring1) == '','shortstring (field)')
      ASSERT(stat == 666,'shortstring (stat)')
      CALL getField(2,string,string2,stat)
      ASSERT(TRIM(string2) == 'arg2','filepath 2 (field)')
      ASSERT(stat == 0,'filepath 2 (stat)')
      string='arg1 2*"arg 2"'
      CALL getField(2,string,string2,stat)
      ASSERT(TRIM(string2) == 'arg 2','multi-arg double-quote 2 (field)')
      ASSERT(stat == 0,'multi-arg double-quote 2 (stat)')
      string2=''
      CALL getField(3,string,string2,stat)
      ASSERT(TRIM(string2) == 'arg 2','multi-arg double-quote 3 (field)')
      ASSERT(stat == 0,'multi-arg double-quote 3 (stat)')
      string='../../file.inp 40*2'
      CALL getField(1,string,string2,stat)
      ASSERT(TRIM(string2) == '../../file.inp','mult-arg filepath 3 (field)')
      ASSERT(stat == 0,'mult-arg filepath 3 (stat)')
      CALL getField(35,string,string2,stat)
      ASSERT(TRIM(string2) == '2','multi-arg filepath 35 (field)')
      ASSERT(stat == 0,'multi-arg filepath 35 (stat)')
      
      COMPONENT_TEST('getField (string,char)')
      tmpStr='     cardname 200*3.1 15*0.2'
      CALL getField(50,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '3.1','50th (field)')
      ASSERT(stat == 0,'50th (stat)')
      CALL getField(205,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '0.2','205th (field)')
      ASSERT(stat == 0,'205th (stat)')
      tmpStr='cardname 1 1 2 3 1 0.2'
      CALL getField(50,tmpStr,tmpStr2,stat)
      ASSERT(LEN_TRIM(tmpStr2) == 0,'too many (field)')
      ASSERT(stat == IOSTAT_END,'too many (stat)')
      CALL getField(5,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '3','normal (field)')
      ASSERT(stat == 0,'normal (stat)')
      tmpStr='''/some dir/file'' arg2'
      CALL getField(1,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '/some dir/file','filepath (field)')
      ASSERT(stat == 0,'filepath (stat)')
      CALL getField(1,tmpStr,shortstring1,stat)
      CALL getField(1,tmpStr,shortstring1)
      ASSERT(TRIM(shortstring1) == '','shortstring (field)')
      ASSERT(stat == 666,'shortstring (stat)')
      CALL getField(2,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == 'arg2','filepath 2 (field)')
      ASSERT(stat == 0,'filepath 2 (stat)')
      tmpStr='arg1 2*"arg 2"'
      CALL getField(2,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == 'arg 2','multi-arg double-quote 2 (field)')
      ASSERT(stat == 0,'multi-arg double-quote 2 (stat)')
      tmpStr2=''
      CALL getField(3,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == 'arg 2','multi-arg double-quote 3 (field)')
      ASSERT(stat == 0,'multi-arg double-quote 3 (stat)')
      tmpStr='../../file.inp 40*2'
      CALL getField(1,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '../../file.inp','mult-arg filepath 3 (field)')
      ASSERT(stat == 0,'mult-arg filepath 3 (stat)')
      CALL getField(35,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '2','multi-arg filepath 35 (field)')
      ASSERT(stat == 0,'multi-arg filepath 35 (stat)')
      
      COMPONENT_TEST('getField (char,string)')
      string='     cardname 200*3.1 15*0.2'
      CALL getField(50,string,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '3.1','50th (field)')
      ASSERT(stat == 0,'50th (stat)')
      CALL getField(205,string,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '0.2','205th (field)')
      ASSERT(stat == 0,'205th (stat)')
      string='cardname 1 1 2 3 1 0.2'
      CALL getField(50,string,tmpStr2,stat)
      ASSERT(LEN_TRIM(tmpStr2) == 0,'too many (field)')
      ASSERT(stat == IOSTAT_END,'too many (stat)')
      CALL getField(5,string,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '3','normal (field)')
      ASSERT(stat == 0,'normal (stat)')
      string='''/some dir/file'' arg2'
      CALL getField(1,string,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '/some dir/file','filepath (field)')
      ASSERT(stat == 0,'filepath (stat)')
      CALL getField(2,string,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == 'arg2','filepath 2 (field)')
      ASSERT(stat == 0,'filepath 2 (stat)')
      string='arg1 2*"arg 2"'
      CALL getField(2,string,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == 'arg 2','multi-arg double-quote 2 (field)')
      ASSERT(stat == 0,'multi-arg double-quote 2 (stat)')
      tmpStr2=''
      CALL getField(3,string,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == 'arg 2','multi-arg double-quote 3 (field)')
      ASSERT(stat == 0,'multi-arg double-quote 3 (stat)')
      string='../../file.inp 40*2'
      CALL getField(1,string,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '../../file.inp','mult-arg filepath 3 (field)')
      ASSERT(stat == 0,'mult-arg filepath 3 (stat)')
      CALL getField(35,string,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '2','multi-arg filepath 35 (field)')
      ASSERT(stat == 0,'multi-arg filepath 35 (stat)')
      
      COMPONENT_TEST('getField (string,string)')
      tmpStr='     cardname 200*3.1 15*0.2'
      CALL getField(50,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '3.1','50th (field)')
      ASSERT(stat == 0,'50th (stat)')
      CALL getField(205,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '0.2','205th (field)')
      ASSERT(stat == 0,'205th (stat)')
      tmpStr='cardname 1 1 2 3 1 0.2'
      CALL getField(50,tmpStr,tmpStr2,stat)
      ASSERT(LEN_TRIM(tmpStr2) == 0,'too many (field)')
      ASSERT(stat == IOSTAT_END,'too many (stat)')
      CALL getField(5,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '3','normal (field)')
      ASSERT(stat == 0,'normal (stat)')
      tmpStr='''/some dir/file'' arg2'
      CALL getField(1,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '/some dir/file','filepath (field)')
      ASSERT(stat == 0,'filepath (stat)')
      CALL getField(2,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == 'arg2','filepath 2 (field)')
      ASSERT(stat == 0,'filepath 2 (stat)')
      tmpStr='arg1 2*"arg 2"'
      CALL getField(2,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == 'arg 2','multi-arg double-quote 2 (field)')
      ASSERT(stat == 0,'multi-arg double-quote 2 (stat)')
      tmpStr2=''
      CALL getField(3,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == 'arg 2','multi-arg double-quote 3 (field)')
      ASSERT(stat == 0,'multi-arg double-quote 3 (stat)')
      tmpStr='../../file.inp 40*2'
      CALL getField(1,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '../../file.inp','mult-arg filepath 3 (field)')
      ASSERT(stat == 0,'mult-arg filepath 3 (stat)')
      CALL getField(35,tmpStr,tmpStr2,stat)
      ASSERT(TRIM(tmpStr2) == '2','multi-arg filepath 35 (field)')
      ASSERT(stat == 0,'multi-arg filepath 35 (stat)')
      
      COMPONENT_TEST('stripComment')
      string='some data !a comment !another comment?'
      CALL stripComment(string)
      ASSERT(TRIM(string) == 'some data','2 comments')
      string='some data !a comment'
      CALL stripComment(string)
      ASSERT(TRIM(string) == 'some data','one comment')
      string='!a comment'
      CALL stripComment(string)
      ASSERT(LEN_TRIM(string) == 0,'only comment')
      string=' !a comment'
      CALL stripComment(string)
      ASSERT(LEN_TRIM(string) == 0,' leading whitespace only comment')
      string=''
      CALL stripComment(string)
      ASSERT(LEN_TRIM(string) == 0,'empty')
      string='some data'
      CALL stripComment(string)
      ASSERT(TRIM(string) == 'some data','no comment')
      
      COMPONENT_TEST('toUPPER')
      string='239p84uyqh;jndf:JKDFH./'
      CALL toUPPER(string)
      ASSERT(TRIM(string) == '239P84UYQH;JNDF:JKDFH./','gibberish (char)')
      tmpStr='239p84uyqh;jndf:JKDFH./'
      CALL toUPPER(tmpStr)
      ASSERT(TRIM(tmpStr) == '239P84UYQH;JNDF:JKDFH./','gibberish (string)')
      
      COMPONENT_TEST('getFilePath')
      string='C:\fullpath\dir1\filenoext'
      CALL getFilePath(string,string2)
      bool=(TRIM(string2) == 'C:'//SLASH//'fullpath'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string)
      string='..\relpath\..\dir1\filenoext'
      CALL getFilePath(string,string2)
      bool=(TRIM(string2) == '..'//SLASH//'relpath'//SLASH//'..'//SLASH// &
        'dir1'//SLASH)
      ASSERT(bool,string)
      string='/fullpath/dir1/filenoext'
      CALL getFilePath(string,string2)
      bool=(TRIM(string2) == SLASH//'fullpath'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string)
      string='../relpath/../dir1/filenoext'
      CALL getFilePath(string,string2)
      bool=(TRIM(string2) == '..'//SLASH//'relpath'//SLASH// &
        '..'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string)
      string='C:\fullpath\dir1\file.two.ext'
      CALL getFilePath(string,string2)
      bool=(TRIM(string2) == 'C:'//SLASH//'fullpath'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string)
      string='..\relpath\..\dir1\file.two.ext'
      CALL getFilePath(string,string2)
      bool=(TRIM(string2) == '..'//SLASH//'relpath'//SLASH// &
        '..'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string)
      string='/fullpath/dir1/file.two.ext'
      CALL getFilePath(string,string2)
      bool=(TRIM(string2) == SLASH//'fullpath'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string)
      string='../relpath/../dir1/file.two.ext'
      CALL getFilePath(string,string2)
      bool=(TRIM(string2) == '..'//SLASH//'relpath'//SLASH// &
        '..'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string)
      
      COMPONENT_TEST('getFileName')
      string='C:\fullpath\dir1\filenoext'
      CALL getFileName(string,string2)
      ASSERT(TRIM(string2) == 'filenoext',string)
      string='..\relpath\..\dir1\filenoext'
      CALL getFileName(string,string2)
      ASSERT(TRIM(string2) == 'filenoext',string)
      string='/fullpath/dir1/filenoext'
      CALL getFileName(string,string2)
      ASSERT(TRIM(string2) == 'filenoext',string)
      string='../relpath/../dir1/filenoext'
      CALL getFileName(string,string2)
      ASSERT(TRIM(string2) == 'filenoext',string)
      string='C:\fullpath\dir1\file.two.ext'
      CALL getFileName(string,string2)
      ASSERT(TRIM(string2) == 'file.two.ext',string)
      string='..\relpath\..\dir1\file.two.ext'
      CALL getFileName(string,string2)
      ASSERT(TRIM(string2) == 'file.two.ext',string)
      string='/fullpath/dir1/file.two.ext'
      CALL getFileName(string,string2)
      ASSERT(TRIM(string2) == 'file.two.ext',string)
      string='../relpath/../dir1/file.two.ext'
      CALL getFileName(string,string2)
      ASSERT(TRIM(string2) == 'file.two.ext',string)
      
      COMPONENT_TEST('getFileNameExt')
      string='C:\fullpath\dir1\filenoext'
      CALL getFileNameExt(string,string2)
      ASSERT(TRIM(string2) == '',string)
      string='..\relpath\..\dir1\filenoext'
      CALL getFileNameExt(string,string2)
      ASSERT(TRIM(string2) == '',string)
      string='/fullpath/dir1/filenoext'
      CALL getFileNameExt(string,string2)
      ASSERT(TRIM(string2) == '',string)
      string='../relpath/../dir1/filenoext'
      CALL getFileNameExt(string,string2)
      ASSERT(TRIM(string2) == '',string)
      string='C:\fullpath\dir1\file.two.ext'
      CALL getFileNameExt(string,string2)
      ASSERT(TRIM(string2) == '.ext',string)
      string='..\relpath\..\dir1\file.two.ext'
      CALL getFileNameExt(string,string2)
      ASSERT(TRIM(string2) == '.ext',string)
      string='/fullpath/dir1/file.two.ext'
      CALL getFileNameExt(string,string2)
      ASSERT(TRIM(string2) == '.ext',string)
      string='../relpath/../dir1/file.two.ext'
      CALL getFileNameExt(string,string2)
      ASSERT(TRIM(string2) == '.ext',string)
      
      COMPONENT_TEST('getFileParts')
      string='C:\fullpath\dir1\filenoext'
      CALL getFileParts(string,string1,string2,string3)
      bool=(TRIM(string1) == 'C:'//SLASH//'fullpath'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string//' path')
      ASSERT(TRIM(string2) == 'filenoext',string//' name')
      ASSERT(TRIM(string3) == '',string//' ext')
      string='..\relpath\..\dir1\filenoext'
      CALL getFileParts(string,string1,string2,string3)
      bool=(TRIM(string1) == '..'//SLASH//'relpath'//SLASH// &
        '..'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string//' path')
      ASSERT(TRIM(string2) == 'filenoext',string//' name')
      ASSERT(TRIM(string3) == '',string//' ext')
      string='/fullpath/dir1/filenoext'
      CALL getFileParts(string,string1,string2,string3)
      bool=(TRIM(string1) == SLASH//'fullpath'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string//' path')
      ASSERT(TRIM(string2) == 'filenoext',string//' name')
      ASSERT(TRIM(string3) == '',string//' ext')
      string='../relpath/../dir1/filenoext'
      CALL getFileParts(string,string1,string2,string3)
      bool=(TRIM(string1) == '..'//SLASH//'relpath'//SLASH// &
        '..'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string//' path')
      ASSERT(TRIM(string2) == 'filenoext',string//' name')
      ASSERT(TRIM(string3) == '',string//' ext')
      string='C:\fullpath\dir1\file.two.ext'
      CALL getFileParts(string,string1,string2,string3)
      bool=(TRIM(string1) == 'C:'//SLASH//'fullpath'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string//' path')
      ASSERT(TRIM(string2) == 'file.two',string//' name')
      ASSERT(TRIM(string3) == '.ext',string//' ext')
      string='..\relpath\..\dir1\file.two.ext'
      CALL getFileParts(string,string1,string2,string3)
      bool=(TRIM(string1) == '..'//SLASH//'relpath'//SLASH// &
        '..'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string//' path')
      ASSERT(TRIM(string2) == 'file.two',string//' name')
      ASSERT(TRIM(string3) == '.ext',string//' ext')
      string='/fullpath/dir1/file.two.ext'
      CALL getFileParts(string,string1,string2,string3)
      bool=(TRIM(string1) == SLASH//'fullpath'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string//' path')
      ASSERT(TRIM(string2) == 'file.two',string//' name')
      ASSERT(TRIM(string3) == '.ext',string//' ext')
      string='../relpath/../dir1/file.two.ext'
      CALL getFileParts(string,string1,string2,string3)
      bool=(TRIM(string1) == '..'//SLASH//'relpath'//SLASH// &
        '..'//SLASH//'dir1'//SLASH)
      ASSERT(bool,string//' path')
      ASSERT(TRIM(string2) == 'file.two',string//' name')
      ASSERT(TRIM(string3) == '.ext',string//' ext')
  
      COMPONENT_TEST('Error Checking')
      testFile%e => e
      CALL getFileParts(string,shortstring1,shortstring2,shortstring3)
      CALL getFileParts(string,shortstring1,shortstring2,shortstring3, &
        testFile%e)
    ENDSUBROUTINE testIO_Strings
!
!-------------------------------------------------------------------------------
    SUBROUTINE testBaseFileType()
      ASSERT(.NOT.(testFile%isRead()),'%isRead()')
      ASSERT(.NOT.(testFile%isWrite()),'%isWrite()')
      ASSERT(.NOT.(testFile%isEOF()),'%isEOF()')
      ASSERT(.NOT.(testFile%isOpen()),'%isOpen()')
      ASSERT(LEN_TRIM(testFile%getFilePath()) == 0,'%getFilePath() (empty)')
      ASSERT(LEN_TRIM(testFile%getFileName()) == 0,'%getFileName() (empty)')
      ASSERT(LEN_TRIM(testFile%getFileExt()) == 0,'%getFileExt() (empty)')
      CALL testFile%getFileParts(string1,string2,string3)
      ASSERT(LEN_TRIM(string1) == 0,'%getFileParts(...) path')
      ASSERT(LEN_TRIM(string2) == 0,'%getFileParts(...) name')
      ASSERT(LEN_TRIM(string3) == 0,'%getFileParts(...) ext')
      testFile%e => NULL()
      CALL testFile%setFilePath('filepath/')
      ASSERT(TRIM(testFile%getFilePath()) == 'filepath/','%getFilePath()')
      CALL testFile%setFileName('filename')
      ASSERT(TRIM(testFile%getFileName()) == 'filename','%getFileName()')
      CALL testFile%setFileExt('.fext')
      ASSERT(TRIM(testFile%getFileExt()) == '.fext','%getFileExt()')
      CALL testFile%getFileParts(string1,string2,string3)
      ASSERT(TRIM(string1) == 'filepath/','%getFileParts(...) path')
      ASSERT(TRIM(string2) == 'filename','%getFileParts(...) name')
      ASSERT(TRIM(string3) == '.fext','%getFileParts(...) ext')
      testFile%e => e
      CALL testFile%setReadStat(.TRUE.)
      ASSERT(testFile%isRead(),'%setReadStat()')
      CALL testFile%setWriteStat(.TRUE.)
      ASSERT(testFile%isWrite(),'%setWriteStat()')

      !Error checking and coverage
      CALL testFile%getFileParts(string1(1:1),string2(1:1),string3(1:1))
      string(MAX_INPUT_FILE_LINE_LEN:MAX_INPUT_FILE_LINE_LEN)='.'
      CALL testFile%setFilePath(string)
      CALL testFile%setFileName(string)
      CALL testFile%setFileExt(string)
      CALL testFile%setEOFstat(.TRUE.)
      CALL testFile%setOpenStat(.TRUE.)
      ASSERT(testFile%isOpen(),'%setOpenStat')
      CALL testFile%setEOFstat(.TRUE.)
      ASSERT(testFile%isEOF(),'%setEOFStat')
      CALL testFile%setFilePath('filepath/')
      CALL testFile%setFileName('filename')
      CALL testFile%setFileExt('.fext')
      CALL testFile%setEOFstat(.FALSE.)
      CALL testFile%setReadStat(.FALSE.)
      CALL testFile%setWriteStat(.FALSE.)
      CALL testFile%setOpenStat(.FALSE.)
    ENDSUBROUTINE testBaseFileType
!
!-------------------------------------------------------------------------------
    SUBROUTINE testFortranFileType()
      
      COMPONENT_TEST('%clear()')
      CALL testFile%clear()
      !Configure exception handler for testing
      testFile%e => e
      testFile2%e => e
      ASSERT(testFile%getUnitNo() == -1,'%getUnitNo()')
      ASSERT(.NOT.(testFile%isFormatted()),'%isFormatted()')
      ASSERT(.NOT.(testFile%isDirect()),'%isDirect()')
      ASSERT(testFile%getRecLen() == -1,'%getRecLen()')
      ASSERT(.NOT.(testFile%isPadded()),'%isPadded()')
      ASSERT(.NOT.(testFile%isNew()),'%isNew()')
      ASSERT(.NOT.(testFile%isOverwrite()),'%isOverwrite()')
      
      !Called for coverage/error checking
      COMPONENT_TEST('%initialize()')
      CALL testFile%fopen()
      CALL testFile%fclose()
      CALL testFile%fdelete()
      CALL testFile%frewind()
      CALL testFile%fbackspace()
      CALL testFile%initialize(UNIT=OUTPUT_UNIT,FILE='./testFile.txt',STATUS='OOPS',&
        ACCESS='OOPS',FORM='OOPS',POSITION='OOPS',ACTION='OOPS',PAD='OOPS',RECL=-1)
      CALL testFile%initialize(UNIT=ERROR_UNIT,FILE='./testFile.txt',STATUS='OLD',&
        ACCESS='SEQUENTIAL',FORM='FORMATTED',POSITION='ASIS',ACTION='WRITE',PAD='NO')
      CALL testFile%initialize(UNIT=INPUT_UNIT,FILE='./testFile.txt',STATUS='NEW',&
        ACCESS='DIRECT',FORM='UNFORMATTED',POSITION='APPEND',ACTION='READ', &
          PAD='YES')
      CALL testFile%initialize(UNIT=OUTPUT_UNIT,FILE='oops.txt',STATUS='REPLACE', &
        ACCESS='DIRECT',POSITION='REWIND',ACTION='READWRITE',RECL=100)
      CALL testFile%initialize(UNIT=OUTPUT_UNIT,FILE='oops.txt',STATUS='SCRATCH')
      CALL testFile%initialize(UNIT=OUTPUT_UNIT,FILE='oops.txt',STATUS='UNKNOWN')
  
      CALL testFile%initialize(UNIT=12,FILE='./testFile.txt',PAD='NO')
      ASSERT(testFile%isInit(),'%isInit(...)')
      ASSERT(TRIM(testFile%getFileName()) == 'testFile','%getFileName()')
      
      COMPONENT_TEST('%fopen()')
      CALL testFile%fopen()
      ASSERT(testFile%isOpen(),'testFile%fopen()')
      
      !Coverage/Error checking
      CALL testFile%fopen()
      CALL testFile2%initialize(UNIT=12,FILE='./testFile.txt')
  
      testFile%e => NULL()
      CALL testFile%fbackspace()
      CALL testFile%frewind()
      testFile%e => e
      CALL testFile%fclose()
      ASSERT(.NOT.(testFile%isOpen()),'%fclose()')
      CALL testFile%fdelete()
      INQUIRE(FILE='./testFile.txt',EXIST=lexist)
      ASSERT(.NOT.lexist,'%fdelete()')
      
      !Coverage/Error checking
      CALL testFile%fclose()
      CALL testFile%fbackspace()
      CALL testFile%frewind()
      CALL testFile%setOpenStat(.TRUE.)
      CALL testFile%fbackspace()
      CALL testFile%frewind()
      CALL testFile%initialize(UNIT=12,FILE='./testFile.txt')
      CALL testFile2%initialize(UNIT=13,FILE='./testFile2',ACCESS='DIRECT', &
        STATUS='NEW',FORM='UNFORMATTED',RECL=100*NUMERIC_STORAGE_SIZE, &
          ACTION='WRITE')
      CALL testFile2%fopen()
      CALL testFile2%fdelete()
      CALL testFile%clear()
      testFile%e => e
      CALL testFile%initialize(UNIT=12,FILE='./testFile.txt',STATUS='OLD', &
        ACCESS='DIRECT',ACTION='READ',RECL=100,FORM='FORMATTED')
      CALL testFile%fopen()
      CALL testFile%fdelete()
      CALL testFile%clear()
      testFile%e => e
      CALL testFile%initialize(UNIT=12,FILE='./testFile.txt',STATUS='OLD', &
        ACTION='READ',FORM='UNFORMATTED')
      CALL testFile%fopen()
      CALL testFile%fdelete()
      CALL testFile%clear()
    ENDSUBROUTINE testFortranFileType
!
!-------------------------------------------------------------------------------
    SUBROUTINE testLogFileType()
      LOGICAL :: bool
      testLogFile%e => e
      ASSERT(.NOT.(testLogFile%isEcho()),'%isEcho()')
      CALL testLogFile%setEcho(.TRUE.)
      ASSERT(testLogFile%isEcho(),'%setEcho(...)')
      CALL testLogFile%initialize(UNIT=66,FILE='./test.log')
      ASSERT(TRIM(testLogFile%getFileName()) == 'test','%initialize(...)')
      CALL testLogFile%fopen()
      testLogFile%e => NULL()
      CALL testLogFile%message('Passed: CALL testLogFile%message(...)',.TRUE.,.TRUE.)
      testLogFile%e => e
      CALL testLogFile%message('Passed: CALL testLogFile%message(...)',.FALSE.,.TRUE.)
      CALL testLogFile%clear(.FALSE.)
      ASSERT(testLogFile%getUnitNo() == -1,'%clear')
      CALL testFile%initialize(UNIT=66,FILE='./test.log',STATUS='OLD',ACTION='READ')
      CALL testFile%fopen()
      READ(66,'(a)') string
      bool=(TRIM(string(12:LEN(string))) == ' Passed: CALL testLogFile%message(...)')
      ASSERT(bool,'%message(...)')
      CALL testFile%clear(.TRUE.)
    ENDSUBROUTINE testLogFileType
!
!-------------------------------------------------------------------------------
    SUBROUTINE testInputFileType()
  
      testInpFile%e => e
      ASSERT(.NOT.(testInpFile%getEchoStat()),'%getEchoStat()')
      ASSERT(testInpFile%getEchoUnit() == -1,'%getEchoUnit()')
      CALL testInpFile%setEchoStat(.TRUE.)
      ASSERT(testInpFile%getEchoStat(),'%setEchoStat(...)')
      CALL testInpFile%setEchoUnit(0)
      CALL testInpFile%setEchoUnit(25)
      ASSERT(testInpFile%getEchoUnit() == 25,'%setEchoUnit(...)')
      CALL testFile%initialize(UNIT=66,FILE='./test.inp',STATUS='REPLACE', &
        ACTION='WRITE')
      CALL testFile%fopen()
      WRITE(testFile%getUnitNo(),'(a,i2)') 'sample oneline',1
      CALL testFile%clear()
      CALL testFile%initialize(UNIT=25,FILE='./test.out',STATUS='REPLACE', &
        ACCESS='DIRECT',RECL=100,ACTION='WRITE')
      CALL testFile%fopen()
      CALL testInpFile%initialize(UNIT=46,FILE='./test.inp')
      CALL testInpFile%fopen()
      string=testInpFile%fgetl()
      ASSERT(TRIM(string) == 'sample oneline 1','%fgetl()')
      ASSERT(testInpFile%getProbe() == 's','%getProbe()')
      CALL testInpFile%frewind()
      ASSERT(LEN_TRIM(testInpFile%getProbe()) == 0,'%frewind()')
      string=testInpFile%fgetl()
      CALL testInpFile%fbackspace()
      ASSERT(LEN_TRIM(testInpFile%getProbe()) == 0,'%fbackspace()')
      CALL testFile%clear(.TRUE.)
      CALL testInpFile%setEchoStat(.FALSE.)
      testInpFile%e => NULL()
      string=testInpFile%fgetl()
      testInpFile%e => e
      CALL testInpFile%clear(.TRUE.)
      ASSERT(testInpFile%getEchoUnit() == -1,'%clear() (echo unit)')
      ASSERT(.NOT.(testInpFile%getEchostat()),'%clear() (echo stat)')
    ENDSUBROUTINE testInputFileType
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRTEnv()
      INTEGER :: n,stat,n0
      CHARACTER(LEN=128) :: tmpChar,varname
      TYPE(StringType) :: tmpStr
      
      COMPONENT_TEST('GET_COMMAND')
      CALL GET_COMMAND(LENGTH=n)
      CALL GET_COMMAND(STATUS=stat)
      CALL GET_COMMAND(LENGTH=n,STATUS=stat)
      IF(0 < n .AND. n <= LEN(tmpChar)) THEN
        !The environment variable exists, execute the test
        CALL GET_COMMAND(COMMAND=tmpStr)
        CALL GET_COMMAND(COMMAND=tmpChar)
        ASSERT(TRIM(tmpChar) == TRIM(tmpStr),'command')
      ELSE
        INFO(0) 'Could not run test for GET_COMMAND'
      ENDIF
      
      COMPONENT_TEST('GET_ENVIRONMENT_VARIABLE')
#ifdef WIN32
      varname='USERNAME'
#else
      varname='USER'
#endif
      CALL GET_ENVIRONMENT_VARIABLE(NAME=varname,VALUE=tmpStr,LENGTH=n, &
        STATUS=stat)
      CALL GET_ENVIRONMENT_VARIABLE(NAME=varname,LENGTH=n,STATUS=stat)
      
      CALL GET_ENVIRONMENT_VARIABLE(NAME=varname,VALUE=tmpChar(1:1),LENGTH=n, &
        STATUS=stat)
      IF(0 < n .AND. n <= LEN(tmpChar)) THEN
        !The environment variable exists, execute the test
        CALL GET_ENVIRONMENT_VARIABLE(NAME=varname,VALUE=tmpStr)
        CALL GET_ENVIRONMENT_VARIABLE(NAME=varname,VALUE=tmpChar)
        ASSERT(TRIM(tmpChar) == TRIM(tmpStr),'value')
      ELSE
        INFO(0) 'Could not run test for GET_ENVIRONMENT_VARIABLE'
      ENDIF
      
      COMPONENT_TEST('GET_CURRENT_DIRECTORY')
      CALL GET_CURRENT_DIRECTORY(DIR=tmpStr,LENGTH=n,STATUS=stat)
      ASSERT(stat == 0,'STATUS (string)')
      ASSERT(strmatch(CHAR(tmpStr),'testIOutil'),'DIR (string)')
      ASSERT(n == LEN(tmpStr),'LENGTH (string)')
      n0=n; n=0;
      CALL GET_CURRENT_DIRECTORY(DIR=tmpChar,LENGTH=n,STATUS=stat)
      ASSERT(n == n0,'LENGTH (char)')
      ASSERT(strmatch(TRIM(tmpChar),CHAR(tmpStr)),'DIR (char)')
      ASSERT(stat == 0,'STATUS (char)')
      CALL GET_CURRENT_DIRECTORY(LENGTH=n,STATUS=stat)
      ASSERT(n == n0,'LENGTH')
      ASSERT(stat == 0,'STATUS')
      CALL GET_CURRENT_DIRECTORY(LENGTH=n)
      ASSERT(n == n0,'LENGTH')
      CALL GET_CURRENT_DIRECTORY(STATUS=stat)
      ASSERT(stat == 0,'STATUS')
    ENDSUBROUTINE testRTEnv
!
ENDPROGRAM testIOutil
