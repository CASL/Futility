!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
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
  REGISTER_SUBTEST('MAKE_DIRECTORY',testMKDIR)
  REGISTER_SUBTEST('SlashRep',testSlashRep)

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
      !So, if you make a loop to assign these, you get valgrind problems.
      tmpStrArray(1)='test0'
      tmpStrArray(2)='test1'
      tmpStrArray(3)='test2'
      tmpStrArray(4)='test3'
      tmpStrArray(5)='test4'
      tmpStrArray(6)='test5'
      tmpStrArray(7)='test6'
      tmpStrArray(8)='test7'
      tmpStrArray(9)='test8'
      !have value 10 be the same as 9 for a duplicate/reverse test
      tmpStrArray(10)='test8'
      ASSERT(strarraymatch(tmpStrArray,'test'),'testing')
      ASSERT(strarraymatch(tmpStrArray,'1'),'testing 1')
      ASSERT(.NOT.strarraymatch(tmpStrArray,'11'),'testing 11')

      COMPONENT_TEST('strarraymatchind')
      ASSERT(strarraymatchind(tmpStrArray,'test') == 1,'testing')
      ASSERT(strarraymatchind(tmpStrArray,'1') == 2,'testing 1')
      ASSERT(strarraymatchind(tmpStrArray,'8') == 9,'testing 8')
      ASSERT(strarraymatchind(tmpStrArray,'8',.FALSE.) == 9,'testing 8')
      ASSERT(strarraymatchind(tmpStrArray,'8',.TRUE.) == 10,'testing 8 reverse')
      ASSERT(strarraymatchind(tmpStrArray,'11') == -1,'testing 11')

      COMPONENT_TEST('strarrayeqind')
      ASSERT(strarrayeqind(tmpStrArray,'test') == -1,'match not eq testing')
      ASSERT(strarrayeqind(tmpStrArray,'test1') == 2,'testing')
      ASSERT(strarrayeqind(tmpStrArray,'1') == -1,'match not eq testing 1')
      ASSERT(strarrayeqind(tmpStrArray,'test7') == 8,'testing test7')
      ASSERT(strarrayeqind(tmpStrArray,'test7',.FALSE.) == 8,'testing test7 F reverse')
      ASSERT(strarrayeqind(tmpStrArray,'test7',.TRUE.) == 8,'testing test7 reverse')
      ASSERT(strarrayeqind(tmpStrArray,'test8') == 9,'testing test8')
      ASSERT(strarrayeqind(tmpStrArray,'test8',.FALSE.) == 9,'testing test8 F reverse')
      ASSERT(strarrayeqind(tmpStrArray,'test8',.TRUE.) == 10,'testing test8 reverse')

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
      string='A->B->C->D->EFGH'
      CALL strrep(string,'->','/')
      ASSERT(TRIM(string) == 'A/B/C/D/EFGH','-> to /')
      CALL strrep(string,'/','->')
      ASSERT(TRIM(string) == 'A->B->C->D->EFGH','/ to ->')

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

      COMPONENT_TEST('getRealFormat')
      string='1'
      CALL getRealFormat(string,string1)
      ASSERT(string1 == 'f2.0','1')
      string='1.'
      CALL getRealFormat(string,string1)
      ASSERT(string1 == 'f2.0','1.')
      string='1.0001'
      CALL getRealFormat(string,string1)
      ASSERT(string1 == 'f6.4','1.0001')
      string='1e-6'
      CALL getRealFormat(string,string1)
      ASSERT(string1 == 'es5.0e1','1e-6')
      string='1e-06'
      CALL getRealFormat(string,string1)
      ASSERT(string1 == 'es6.0e2','1e-06')
      string='-3.21e+02'
      CALL getRealFormat(string,string1)
      ASSERT(string1 == 'es9.2e2','-3.21e+02')
      string='3.21e'
      CALL getRealFormat(string,string1)
      ASSERT(string1 == '','3.21e')
      string='3.21e1'
      CALL getRealFormat(string,string1)
      ASSERT(string1 == 'es7.2e1','3.21e1')
      string='hello'
      CALL getRealFormat(string,string1)
      ASSERT(string1 == '','hello')
      FINFO() '"'//string1//'"'
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
!-------------------------------------------------------------------------------
    SUBROUTINE testMKDIR()
#ifdef __INTEL_COMPILER
      USE IFPORT !For SYSTEM function
#endif
      CHARACTER(LEN=32) :: char_dir,tmp
      INTEGER :: mystat
      TYPE(StringType) :: str_dir


      char_dir='d1/d 2'
      str_dir='d1/d 2/d3'
      CALL MAKE_DIRECTORY(char_dir,STATUS=mystat)
      ASSERT(mystat == 0,'d1/d 2 STAT')
      CALL MAKE_DIRECTORY(str_dir,STATUS=mystat)
      ASSERT(mystat == 0,'str_dir')
      !Test if directories already exist
      CALL MAKE_DIRECTORY(char_dir,STATUS=mystat)
      ASSERT(mystat == 0,'char_dir')
      CALL MAKE_DIRECTORY(str_dir,STATUS=mystat)
      ASSERT(mystat == 0,'str_dir')

      !Clean up tests, remove directories
      tmp='rmdir "d1/d 2/d3"'
      CALL SlashRep(tmp)
      mystat=SYSTEM(tmp)
      ASSERT(mystat == 0,'d1/d 2/d3 does not exist')
      tmp='rmdir "d1/d 2"'
      CALL SlashRep(tmp)
      mystat=SYSTEM(tmp)
      ASSERT(mystat == 0,'d1/d 2 does not exist')
      tmp='rmdir d1'
      CALL SlashRep(tmp)
      mystat=SYSTEM(tmp)
      ASSERT(mystat == 0,'d1 does not exist')
    ENDSUBROUTINE testMKDIR
!
!-------------------------------------------------------------------------------
    SUBROUTINE testSlashRep()
      CHARACTER(LEN=32) :: char_dir
      TYPE(StringType) :: str_dir
#ifdef WIN32
      char_dir='d1'//FSLASH//'d 2'
      str_dir='d1'//FSLASH//'d 2'//FSLASH//'d3'
#else
      char_dir='d1'//BSLASH//'d 2'
      str_dir='d1'//BSLASH//'d 2'//BSLASH//'d3'
#endif

      CALL SlashRep(char_dir)
      ASSERT(char_dir == 'd1'//SLASH//'d 2','char')
      CALL SlashRep(str_dir)
      ASSERT(str_dir == 'd1'//SLASH//'d 2'//SLASH//'d3','string')
    ENDSUBROUTINE testSlashRep
!
ENDPROGRAM testIOutil
