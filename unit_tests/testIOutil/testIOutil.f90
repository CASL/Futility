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
  USE IO_Strings
  USE IOutil
  
  IMPLICIT NONE
  
  CHARACTER(LEN=256) :: string,string1,string2,string3
  CHARACTER(LEN=1) :: shortstring1,shortstring2,shortstring3
  TYPE(ExceptionHandlerType),TARGET :: e
      
  CREATE_TEST('IOUTIL')
  
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  
  REGISTER_SUBTEST('PARAMETERS',testParameters)
  REGISTER_SUBTEST('IO_Strings',testIO_Strings)
  REGISTER_SUBTEST('Run-time Environment',testRTEnv)
  
  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testParameters()
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
      INTEGER,ALLOCATABLE :: tmpint(:)
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
      CALL strfind('stesting','test',tmpint)
      ASSERTFAIL(SIZE(tmpint) == 1,'testing SIZE')
      ASSERT(ALL(tmpint == (/2/)),'testing')
      DEALLOCATE(tmpint)
      CALL strfind('TEAM','I',tmpint)
      ASSERTFAIL(SIZE(tmpint) == 0,'TEAM')
      DEALLOCATE(tmpint)
      CALL strfind('t e s t',' ',tmpint)
      ASSERTFAIL(SIZE(tmpint) == 3,'t e s t SIZE')
      ASSERT(ALL(tmpint == (/2,4,6/)),'t e s t')
      DEALLOCATE(tmpint)
      
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
      !string='XSMACRO "core mat mod" 0'
      !ASSERT(nFields(string) == 3,'quotes')
      !string=' XSMACRO "core mat mod" 0'
      !ASSERT(nFields(string) == 3,'quotes')
      !string='XSMACRO "core mat mod" 0 '
      !ASSERT(nFields(string) == 3,'quotes')
      !string=' XSMACRO "core mat mod" 0 '
      !ASSERT(nFields(string) == 3,'quotes')
      
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
      CALL getFileParts(string,shortstring1,shortstring2,shortstring3)
      CALL getFileParts(string,shortstring1,shortstring2,shortstring3,e)
    ENDSUBROUTINE testIO_Strings
!
!-------------------------------------------------------------------------------
    SUBROUTINE testRTEnv()
      INTEGER :: n,stat,n0
      CHARACTER(LEN=1024) :: tmpChar,varname
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
