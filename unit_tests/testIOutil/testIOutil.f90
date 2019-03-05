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
  USE IntrType
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
      !have value 10 be the same as 9 for a duplicate/reverse test
      tmpStrArray(10)='test8'
      ASSERT(strarraymatch(tmpStrArray,'test'),'testing')
      ASSERT(strarraymatch(tmpStrArray,'1'),'testing 1')
      ASSERT(.NOT.strarraymatch(tmpStrArray,'11'),'testing 11')

      COMPONENT_TEST('strarraymatchind')
      ASSERT_EQ(strarraymatchind(tmpStrArray,'test'),1,'testing')
      ASSERT_EQ(strarraymatchind(tmpStrArray,'1'),2,'testing 1')
      ASSERT_EQ(strarraymatchind(tmpStrArray,'8'),9,'testing 8')
      ASSERT_EQ(strarraymatchind(tmpStrArray,'8',.FALSE.),9,'testing 8')
      ASSERT_EQ(strarraymatchind(tmpStrArray,'8',.TRUE.),10,'testing 8 reverse')
      ASSERT_EQ(strarraymatchind(tmpStrArray,'11'),-1,'testing 11')

      COMPONENT_TEST('strarrayeqind')
      ASSERT_EQ(strarrayeqind(tmpStrArray,'test'),-1,'match not eq testing')
      ASSERT_EQ(strarrayeqind(tmpStrArray,'test1'),2,'testing')
      ASSERT_EQ(strarrayeqind(tmpStrArray,'1'),-1,'match not eq testing 1')
      ASSERT_EQ(strarrayeqind(tmpStrArray,'test7'),8,'testing test7')
      ASSERT_EQ(strarrayeqind(tmpStrArray,'test7',.FALSE.),8,'testing test7 F reverse')
      ASSERT_EQ(strarrayeqind(tmpStrArray,'test7',.TRUE.),8,'testing test7 reverse')
      ASSERT_EQ(strarrayeqind(tmpStrArray,'test8'),9,'testing test8')
      ASSERT_EQ(strarrayeqind(tmpStrArray,'test8',.FALSE.),9,'testing test8 F reverse')
      ASSERT_EQ(strarrayeqind(tmpStrArray,'test8',.TRUE.),10,'testing test8 reverse')

      COMPONENT_TEST('nmatchstr')
      ASSERT_EQ(nmatchstr('testing','test'),1,'testing')
      ASSERT_EQ(nmatchstr('TEAM','I'),0,'TEAM')
      ASSERT_EQ(nmatchstr('t e s t',' '),3 ,'t e s t')

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
      ASSERT_EQ(TRIM(string),'TTesTTing','testing')
      tmpStr='testing  '
      CALL strrep(tmpStr,'t','TT')
      ASSERT_EQ(TRIM(tmpStr),'TTesTTing','testing')
      string='A->B->C->D->EFGH'
      CALL strrep(string,'->','/')
      ASSERT_EQ(TRIM(string),'A/B/C/D/EFGH','-> to /')
      CALL strrep(string,'/','->')
      ASSERT_EQ(TRIM(string),'A->B->C->D->EFGH','/ to ->')

      COMPONENT_TEST('nFields (character)')
      string=''
      ASSERT_EQ(nFields(string),0,'blank')
      string=' '
      ASSERT_EQ(nFields(string),0,'" "')
      string='''arg1'
      ASSERT_EQ(nFields(string),-1,'bad single-quote')
      string='"arg1'
      ASSERT_EQ(nFields(string),-2,'bad double-quote')
      string='1'
      ASSERT_EQ(nFields(string),1,'1')
      string='XSMACRO "core mat mod" 0'
      ASSERT_EQ(nFields(string),3,'quotes')
      string=' XSMACRO "core mat mod" 0'
      ASSERT_EQ(nFields(string),3,'quotes')
      string='XSMACRO "core mat mod" 0 '
      ASSERT_EQ(nFields(string),3,'quotes')
      string=' XSMACRO "core mat mod" 0 '
      ASSERT_EQ(nFields(string),3,'quotes')
      string=' XSMACRO "core mat mod" 0 "number 2"'
      ASSERT_EQ(nFields(string),4,'quotes')
      string='arg1 nmult*arg2'
      ASSERT_EQ(nFields(string),2,'bad multi-arg')
      string='arg1 2*arg2'
      ASSERT_EQ(nFields(string),3,'good multi-arg')
      string='arg1 2*"arg 2"'
      ASSERT_EQ(nFields(string),3,'good "multi-arg"')
      string='cardname 1 1 2 3 1 0.2'
      ASSERT_EQ(nFields(string),7,'card')
      string='     cardname 200*3.1 15*0.2'
      ASSERT_EQ(nFields(string),216,'card multi-arg')

      COMPONENT_TEST('nFields (string)')
      tmpStr=''
      ASSERT_EQ(nFields(tmpStr),0,'blank')
      tmpStr=' '
      ASSERT_EQ(nFields(tmpStr),0,'" "')
      tmpStr='''arg1'
      ASSERT_EQ(nFields(tmpStr),-1,'bad single-quote')
      tmpStr='"arg1'
      ASSERT_EQ(nFields(tmpStr),-2,'bad double-quote')
      tmpStr='1'
      ASSERT_EQ(nFields(tmpStr),1,'1')
      tmpStr='arg1 nmult*arg2'
      ASSERT_EQ(nFields(tmpStr),2,'bad multi-arg')
      tmpStr='arg1 2*arg2'
      ASSERT_EQ(nFields(tmpStr),3,'good multi-arg')
      tmpStr='arg1 2*"arg 2"'
      ASSERT_EQ(nFields(tmpStr),3,'good "multi-arg"')
      tmpStr='cardname 1 1 2 3 1 0.2'
      ASSERT_EQ(nFields(tmpStr),7,'card')
      tmpStr='     cardname 200*3.1 15*0.2'
      ASSERT_EQ(nFields(tmpStr),216,'card multi-arg')

      COMPONENT_TEST('getField (char,char)')
      string='     cardname 200*3.1 15*0.2'
      CALL getField(50,string,string2,stat)
      ASSERT_EQ(TRIM(string2),'3.1','50th (field)')
      ASSERT_EQ(stat,0,'50th (stat)')
      CALL getField(205,string,string2,stat)
      ASSERT_EQ(TRIM(string2),'0.2','205th (field)')
      ASSERT_EQ(stat,0,'205th (stat)')
      string='cardname 1 1 2 3 1 0.2'
      CALL getField(50,string,string2,stat)
      ASSERT_EQ(LEN_TRIM(string2),0,'too many (field)')
      ASSERT_EQ(stat,IOSTAT_END,'too many (stat)')
      CALL getField(5,string,string2,stat)
      ASSERT_EQ(TRIM(string2),'3','normal (field)')
      ASSERT_EQ(stat,0,'normal (stat)')
      string='''/some dir/file'' arg2'
      CALL getField(1,string,string2,stat)
      ASSERT_EQ(TRIM(string2),'/some dir/file','filepath (field)')
      ASSERT_EQ(stat,0,'filepath (stat)')
      CALL getField(1,string,shortstring1,stat)
      CALL getField(1,string,shortstring1)
      ASSERT_EQ(TRIM(shortstring1),'','shortstring (field)')
      ASSERT_EQ(stat,666,'shortstring (stat)')
      CALL getField(2,string,string2,stat)
      ASSERT_EQ(TRIM(string2),'arg2','filepath 2 (field)')
      ASSERT_EQ(stat,0,'filepath 2 (stat)')
      string='arg1 2*"arg 2"'
      CALL getField(2,string,string2,stat)
      ASSERT_EQ(TRIM(string2),'arg 2','multi-arg double-quote 2 (field)')
      ASSERT_EQ(stat,0,'multi-arg double-quote 2 (stat)')
      string2=''
      CALL getField(3,string,string2,stat)
      ASSERT_EQ(TRIM(string2),'arg 2','multi-arg double-quote 3 (field)')
      ASSERT_EQ(stat,0,'multi-arg double-quote 3 (stat)')
      string='../../file.inp 40*2'
      CALL getField(1,string,string2,stat)
      ASSERT_EQ(TRIM(string2),'../../file.inp','mult-arg filepath 3 (field)')
      ASSERT_EQ(stat,0,'mult-arg filepath 3 (stat)')
      CALL getField(35,string,string2,stat)
      ASSERT_EQ(TRIM(string2),'2','multi-arg filepath 35 (field)')
      ASSERT_EQ(stat,0,'multi-arg filepath 35 (stat)')

      COMPONENT_TEST('getField (string,char)')
      tmpStr='     cardname 200*3.1 15*0.2'
      CALL getField(50,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'3.1','50th (field)')
      ASSERT_EQ(stat,0,'50th (stat)')
      CALL getField(205,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'0.2','205th (field)')
      ASSERT_EQ(stat,0,'205th (stat)')
      tmpStr='cardname 1 1 2 3 1 0.2'
      CALL getField(50,tmpStr,tmpStr2,stat)
      ASSERT_EQ(LEN_TRIM(tmpStr2),0,'too many (field)')
      ASSERT_EQ(stat,IOSTAT_END,'too many (stat)')
      CALL getField(5,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'3','normal (field)')
      ASSERT_EQ(stat,0,'normal (stat)')
      tmpStr='''/some dir/file'' arg2'
      CALL getField(1,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'/some dir/file','filepath (field)')
      ASSERT_EQ(stat,0,'filepath (stat)')
      CALL getField(1,tmpStr,shortstring1,stat)
      CALL getField(1,tmpStr,shortstring1)
      ASSERT_EQ(TRIM(shortstring1),'','shortstring (field)')
      ASSERT_EQ(stat,666,'shortstring (stat)')
      CALL getField(2,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'arg2','filepath 2 (field)')
      ASSERT_EQ(stat,0,'filepath 2 (stat)')
      tmpStr='arg1 2*"arg 2"'
      CALL getField(2,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'arg 2','multi-arg double-quote 2 (field)')
      ASSERT_EQ(stat,0,'multi-arg double-quote 2 (stat)')
      tmpStr2=''
      CALL getField(3,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'arg 2','multi-arg double-quote 3 (field)')
      ASSERT_EQ(stat,0,'multi-arg double-quote 3 (stat)')
      tmpStr='../../file.inp 40*2'
      CALL getField(1,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'../../file.inp','mult-arg filepath 3 (field)')
      ASSERT_EQ(stat,0,'mult-arg filepath 3 (stat)')
      CALL getField(35,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'2','multi-arg filepath 35 (field)')
      ASSERT_EQ(stat,0,'multi-arg filepath 35 (stat)')

      COMPONENT_TEST('getField (char,string)')
      string='     cardname 200*3.1 15*0.2'
      CALL getField(50,string,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'3.1','50th (field)')
      ASSERT_EQ(stat,0,'50th (stat)')
      CALL getField(205,string,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'0.2','205th (field)')
      ASSERT_EQ(stat,0,'205th (stat)')
      string='cardname 1 1 2 3 1 0.2'
      CALL getField(50,string,tmpStr2,stat)
      ASSERT_EQ(LEN_TRIM(tmpStr2),0,'too many (field)')
      ASSERT_EQ(stat,IOSTAT_END,'too many (stat)')
      CALL getField(5,string,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'3','normal (field)')
      ASSERT_EQ(stat,0,'normal (stat)')
      string='''/some dir/file'' arg2'
      CALL getField(1,string,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'/some dir/file','filepath (field)')
      ASSERT_EQ(stat,0,'filepath (stat)')
      CALL getField(2,string,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'arg2','filepath 2 (field)')
      ASSERT_EQ(stat,0,'filepath 2 (stat)')
      string='arg1 2*"arg 2"'
      CALL getField(2,string,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'arg 2','multi-arg double-quote 2 (field)')
      ASSERT_EQ(stat,0,'multi-arg double-quote 2 (stat)')
      tmpStr2=''
      CALL getField(3,string,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'arg 2','multi-arg double-quote 3 (field)')
      ASSERT_EQ(stat,0,'multi-arg double-quote 3 (stat)')
      string='../../file.inp 40*2'
      CALL getField(1,string,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'../../file.inp','mult-arg filepath 3 (field)')
      ASSERT_EQ(stat,0,'mult-arg filepath 3 (stat)')
      CALL getField(35,string,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'2','multi-arg filepath 35 (field)')
      ASSERT_EQ(stat,0,'multi-arg filepath 35 (stat)')

      COMPONENT_TEST('getField (string,string)')
      tmpStr='     cardname 200*3.1 15*0.2'
      CALL getField(50,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'3.1','50th (field)')
      ASSERT_EQ(stat,0,'50th (stat)')
      CALL getField(205,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'0.2','205th (field)')
      ASSERT_EQ(stat,0,'205th (stat)')
      tmpStr='cardname 1 1 2 3 1 0.2'
      CALL getField(50,tmpStr,tmpStr2,stat)
      ASSERT_EQ(LEN_TRIM(tmpStr2),0,'too many (field)')
      ASSERT_EQ(stat,IOSTAT_END,'too many (stat)')
      CALL getField(5,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'3','normal (field)')
      ASSERT_EQ(stat,0,'normal (stat)')
      tmpStr='''/some dir/file'' arg2'
      CALL getField(1,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'/some dir/file','filepath (field)')
      ASSERT_EQ(stat,0,'filepath (stat)')
      CALL getField(2,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'arg2','filepath 2 (field)')
      ASSERT_EQ(stat,0,'filepath 2 (stat)')
      tmpStr='arg1 2*"arg 2"'
      CALL getField(2,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'arg 2','multi-arg double-quote 2 (field)')
      ASSERT_EQ(stat,0,'multi-arg double-quote 2 (stat)')
      tmpStr2=''
      CALL getField(3,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'arg 2','multi-arg double-quote 3 (field)')
      ASSERT_EQ(stat,0,'multi-arg double-quote 3 (stat)')
      tmpStr='../../file.inp 40*2'
      CALL getField(1,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'../../file.inp','mult-arg filepath 3 (field)')
      ASSERT_EQ(stat,0,'mult-arg filepath 3 (stat)')
      CALL getField(35,tmpStr,tmpStr2,stat)
      ASSERT_EQ(TRIM(tmpStr2),'2','multi-arg filepath 35 (field)')
      ASSERT_EQ(stat,0,'multi-arg filepath 35 (stat)')

      COMPONENT_TEST('stripComment')
      string='some data !a comment !another comment?'
      CALL stripComment(string)
      ASSERT_EQ(TRIM(string),'some data','2 comments')
      string='some data !a comment'
      CALL stripComment(string)
      ASSERT_EQ(TRIM(string),'some data','one comment')
      string='!a comment'
      CALL stripComment(string)
      ASSERT_EQ(LEN_TRIM(string),0,'only comment')
      string=' !a comment'
      CALL stripComment(string)
      ASSERT_EQ(LEN_TRIM(string),0,' leading whitespace only comment')
      string=''
      CALL stripComment(string)
      ASSERT_EQ(LEN_TRIM(string),0,'empty')
      string='some data'
      CALL stripComment(string)
      ASSERT_EQ(TRIM(string),'some data','no comment')

      COMPONENT_TEST('toUPPER')
      string='239p84uyqh;jndf:JKDFH./'
      CALL toUPPER(string)
      ASSERT_EQ(TRIM(string),'239P84UYQH;JNDF:JKDFH./','gibberish (char)')
      tmpStr='239p84uyqh;jndf:JKDFH./'
      CALL toUPPER(tmpStr)
      ASSERT_EQ(TRIM(tmpStr),'239P84UYQH;JNDF:JKDFH./','gibberish (string)')

      COMPONENT_TEST('toLower')
      string='239p84uyqh;jndf:JKDFH./'
      CALL toLower(string)
      ASSERT_EQ(TRIM(string),'239p84uyqh;jndf:jkdfh./','gibberish (char)')
      tmpStr='239p84uyqh;jndf:JKDFH./'
      CALL toLower(tmpStr)
      ASSERT_EQ(TRIM(tmpstr),'239p84uyqh;jndf:jkdfh./','gibberish (string)')

      COMPONENT_TEST('getFilePath')
      string='C:\fullpath\dir1\filenoext'
      CALL getFilePath(string,string2)
      ASSERT_EQ(TRIM(string2),'C:'//SLASH//'fullpath'//SLASH//'dir1'//SLASH,string)
      string='..\relpath\..\dir1\filenoext'
      CALL getFilePath(string,string2)
      ASSERT_EQ(TRIM(string2),'..'//SLASH//'relpath'//SLASH//'..'//SLASH//'dir1'//SLASH,string)
      string='/fullpath/dir1/filenoext'
      CALL getFilePath(string,string2)
      ASSERT_EQ(TRIM(string2),SLASH//'fullpath'//SLASH//'dir1'//SLASH,string)
      string='../relpath/../dir1/filenoext'
      CALL getFilePath(string,string2)
      ASSERT_EQ(TRIM(string2),'..'//SLASH//'relpath'//SLASH//'..'//SLASH//'dir1'//SLASH,string)
      string='C:\fullpath\dir1\file.two.ext'
      CALL getFilePath(string,string2)
      ASSERT_EQ(TRIM(string2),'C:'//SLASH//'fullpath'//SLASH//'dir1'//SLASH,string)
      string='..\relpath\..\dir1\file.two.ext'
      CALL getFilePath(string,string2)
      ASSERT_EQ(TRIM(string2),'..'//SLASH//'relpath'//SLASH//'..'//SLASH//'dir1'//SLASH,string)
      string='/fullpath/dir1/file.two.ext'
      CALL getFilePath(string,string2)
      ASSERT_EQ(TRIM(string2),SLASH//'fullpath'//SLASH//'dir1'//SLASH,string)
      string='../relpath/../dir1/file.two.ext'
      CALL getFilePath(string,string2)
      ASSERT_EQ(TRIM(string2),'..'//SLASH//'relpath'//SLASH//'..'//SLASH//'dir1'//SLASH,string)

      COMPONENT_TEST('getFileName')
      string='C:\fullpath\dir1\filenoext'
      CALL getFileName(string,string2)
      ASSERT_EQ(TRIM(string2),'filenoext',string)
      string='..\relpath\..\dir1\filenoext'
      CALL getFileName(string,string2)
      ASSERT_EQ(TRIM(string2),'filenoext',string)
      string='/fullpath/dir1/filenoext'
      CALL getFileName(string,string2)
      ASSERT_EQ(TRIM(string2),'filenoext',string)
      string='../relpath/../dir1/filenoext'
      CALL getFileName(string,string2)
      ASSERT_EQ(TRIM(string2),'filenoext',string)
      string='C:\fullpath\dir1\file.two.ext'
      CALL getFileName(string,string2)
      ASSERT_EQ(TRIM(string2),'file.two.ext',string)
      string='..\relpath\..\dir1\file.two.ext'
      CALL getFileName(string,string2)
      ASSERT_EQ(TRIM(string2),'file.two.ext',string)
      string='/fullpath/dir1/file.two.ext'
      CALL getFileName(string,string2)
      ASSERT_EQ(TRIM(string2),'file.two.ext',string)
      string='../relpath/../dir1/file.two.ext'
      CALL getFileName(string,string2)
      ASSERT_EQ(TRIM(string2),'file.two.ext',string)

      COMPONENT_TEST('getFileNameExt')
      string='C:\fullpath\dir1\filenoext'
      CALL getFileNameExt(string,string2)
      ASSERT_EQ(TRIM(string2),'',string)
      string='..\relpath\..\dir1\filenoext'
      CALL getFileNameExt(string,string2)
      ASSERT_EQ(TRIM(string2),'',string)
      string='/fullpath/dir1/filenoext'
      CALL getFileNameExt(string,string2)
      ASSERT_EQ(TRIM(string2),'',string)
      string='../relpath/../dir1/filenoext'
      CALL getFileNameExt(string,string2)
      ASSERT_EQ(TRIM(string2),'',string)
      string='C:\fullpath\dir1\file.two.ext'
      CALL getFileNameExt(string,string2)
      ASSERT_EQ(TRIM(string2),'.ext',string)
      string='..\relpath\..\dir1\file.two.ext'
      CALL getFileNameExt(string,string2)
      ASSERT_EQ(TRIM(string2),'.ext',string)
      string='/fullpath/dir1/file.two.ext'
      CALL getFileNameExt(string,string2)
      ASSERT_EQ(TRIM(string2),'.ext',string)
      string='../relpath/../dir1/file.two.ext'
      CALL getFileNameExt(string,string2)
      ASSERT_EQ(TRIM(string2),'.ext',string)

      COMPONENT_TEST('getFileParts')
      string='C:\fullpath\dir1\filenoext'
      CALL getFileParts(string,string1,string2,string3)
      ASSERT_EQ(TRIM(string1),'C:'//SLASH//'fullpath'//SLASH//'dir1'//SLASH,string//' path')
      ASSERT_EQ(TRIM(string2),'filenoext',string//' name')
      ASSERT_EQ(TRIM(string3),'',string//' ext')
      string='..\relpath\..\dir1\filenoext'
      CALL getFileParts(string,string1,string2,string3)
      ASSERT_EQ(TRIM(string1),'..'//SLASH//'relpath'//SLASH//'..'//SLASH//'dir1'//SLASH,string//' path')
      ASSERT_EQ(TRIM(string2),'filenoext',string//' name')
      ASSERT_EQ(TRIM(string3),'',string//' ext')
      string='/fullpath/dir1/filenoext'
      CALL getFileParts(string,string1,string2,string3)
      ASSERT_EQ(TRIM(string1),SLASH//'fullpath'//SLASH//'dir1'//SLASH,string//' path')
      ASSERT_EQ(TRIM(string2),'filenoext',string//' name')
      ASSERT_EQ(TRIM(string3),'',string//' ext')
      string='../relpath/../dir1/filenoext'
      CALL getFileParts(string,string1,string2,string3)
      ASSERT_EQ(TRIM(string1),'..'//SLASH//'relpath'//SLASH//'..'//SLASH//'dir1'//SLASH,string//' path')
      ASSERT_EQ(TRIM(string2),'filenoext',string//' name')
      ASSERT_EQ(TRIM(string3),'',string//' ext')
      string='C:\fullpath\dir1\file.two.ext'
      CALL getFileParts(string,string1,string2,string3)
      ASSERT_EQ(TRIM(string1),'C:'//SLASH//'fullpath'//SLASH//'dir1'//SLASH,string//' path')
      ASSERT_EQ(TRIM(string2),'file.two',string//' name')
      ASSERT_EQ(TRIM(string3),'.ext',string//' ext')
      string='..\relpath\..\dir1\file.two.ext'
      CALL getFileParts(string,string1,string2,string3)
      ASSERT_EQ(TRIM(string1),'..'//SLASH//'relpath'//SLASH//'..'//SLASH//'dir1'//SLASH,string//' path')
      ASSERT_EQ(TRIM(string2),'file.two',string//' name')
      ASSERT_EQ(TRIM(string3),'.ext',string//' ext')
      string='/fullpath/dir1/file.two.ext'
      CALL getFileParts(string,string1,string2,string3)
      ASSERT_EQ(TRIM(string1),SLASH//'fullpath'//SLASH//'dir1'//SLASH,string//' path')
      ASSERT_EQ(TRIM(string2),'file.two',string//' name')
      ASSERT_EQ(TRIM(string3),'.ext',string//' ext')
      string='../relpath/../dir1/file.two.ext'
      CALL getFileParts(string,string1,string2,string3)
      ASSERT_EQ(TRIM(string1),'..'//SLASH//'relpath'//SLASH//'..'//SLASH//'dir1'//SLASH,string//' path')
      ASSERT_EQ(TRIM(string2),'file.two',string//' name')
      ASSERT_EQ(TRIM(string3),'.ext',string//' ext')

      COMPONENT_TEST('Error Checking')
      CALL getFileParts(string,shortstring1,shortstring2,shortstring3)
      CALL getFileParts(string,shortstring1,shortstring2,shortstring3,e)

      COMPONENT_TEST('getRealFormat')
      string='1'
      CALL getRealFormat(string,string1)
      ASSERT_EQ(string1,'f2.0','1')
      string='1.'
      CALL getRealFormat(string,string1)
      ASSERT_EQ(string1,'f2.0','1.')
      string='1.0001'
      CALL getRealFormat(string,string1)
      ASSERT_EQ(string1,'f6.4','1.0001')
      string='1e-6'
      CALL getRealFormat(string,string1)
      ASSERT_EQ(string1,'es5.0e1','1e-6')
      string='1e-06'
      CALL getRealFormat(string,string1)
      ASSERT_EQ(string1,'es6.0e2','1e-06')
      string='-3.21e+02'
      CALL getRealFormat(string,string1)
      ASSERT_EQ(string1,'es9.2e2','-3.21e+02')
      string='3.21e'
      CALL getRealFormat(string,string1)
      ASSERT_EQ(string1,'','3.21e')
      string='3.21e1'
      CALL getRealFormat(string,string1)
      ASSERT_EQ(string1,'es7.2e1','3.21e1')
      string='hello'
      CALL getRealFormat(string,string1)
      ASSERT_EQ(string1,'','hello')
      FINFO() '"'//string1//'"'

      COMPONENT_TEST('str')
      !SNK
      ASSERT_EQ(str(2_SNK),'2','str(SNK)')
      ASSERT_EQ(str(-2_SNK),'-2','str(SNK)')
      ASSERT_EQ(str(375_SNK,9),'000000375','str(SNK,pad)')
      ASSERT_EQ(str(3_SNK,3),'003','str(SNK,pad)')
      ASSERT_EQ(str(-3_SNK,3),'-003','str(SNK,pad)')
      ASSERT_EQ(str(-375_SNK,9),'-000000375','str(SNK,pad)')
      !SLK
      ASSERT_EQ(str(3_SLK),'3','str(SLK)')
      ASSERT_EQ(str(-3_SLK),'-3','str(SLK)')
      ASSERT_EQ(str(375_SLK,9),'000000375','str(SLK,pad)')
      ASSERT_EQ(str(3_SLK,3),'003','str(SLK,pad)')
      ASSERT_EQ(str(-3_SLK,3),'-003','str(SLK,pad)')
      ASSERT_EQ(str(-375_SLK,9),'-000000375','str(SLK,pad)')
      !SSK
      ASSERT_EQ(str(2.5_SSK),'2.5000000E+00','str(SSK)')
      ASSERT_EQ(str(2.5_SSK,2),'2.50E+00','str(SSK,nDecimal)')
      ASSERT_EQ(str(-0.0025_SSK),'-2.4999999E-03','str(SSK)')
      ASSERT_EQ(str(-0.0025_SSK,2),'-2.50E-03','str(SSK,nDecimal)')
      !SDK
      ASSERT_EQ(str(0.5_SDK),'5.000000000000000E-01','str(SDK)')
      ASSERT_EQ(str(0.5_SDK,5),'5.00000E-01','str(SDK,nDecimal)')
      ASSERT_EQ(str(-5000.0_SDK),'-5.000000000000000E+03','str(SDK)')
      ASSERT_EQ(str(-5000.0_SDK,5),'-5.00000E+03','str(SDK,nDecimal)')

      COMPONENT_TEST('getFloatFormat')
      ASSERT_EQ(getFloatFormat(0.0_SRK),1_SIK,'zero')
      ASSERT_EQ(getFloatFormat(-0.0_SRK),2_SIK,'zero')
      ASSERT_EQ(getFloatFormat(0.1_SRK),1_SIK,'zero')
      ASSERT_EQ(getFloatFormat(-0.1_SRK),2_SIK,'zero')
      ASSERT_EQ(getFloatFormat(1.1_SRK),1_SIK,'zero')
      ASSERT_EQ(getFloatFormat(-1.1_SRK),2_SIK,'zero')
      ASSERT_EQ(getFloatFormat(12.1_SRK),2_SIK,'zero')
      ASSERT_EQ(getFloatFormat(-21.1_SRK),3_SIK,'zero')
      ASSERT_EQ(getFloatFormat(123.1_SRK),3_SIK,'zero')
      ASSERT_EQ(getFloatFormat(-213.1_SRK),4_SIK,'zero')
      ASSERT_EQ(getFloatFormat(4213.1_SRK),4_SIK,'zero')
      ASSERT_EQ(getFloatFormat(0.01_SRK),1_SIK,'zero')
      ASSERT_EQ(getFloatFormat(-0.01_SRK),2_SIK,'zero')
      ASSERT_EQ(getFloatFormat(0.123_SRK),1_SIK,'zero')
      ASSERT_EQ(getFloatFormat(-0.123_SRK),2_SIK,'zero')
      ASSERT_EQ(getFloatFormat(1.1234_SRK),1_SIK,'zero')
      ASSERT_EQ(getFloatFormat(-1.1342_SRK),2_SIK,'zero')
      ASSERT_EQ(getFloatFormat(12.12_SRK),2_SIK,'zero')
      ASSERT_EQ(getFloatFormat(-21.12_SRK),3_SIK,'zero')
      ASSERT_EQ(getFloatFormat(123.12_SRK),3_SIK,'zero')
      ASSERT_EQ(getFloatFormat(-213.12_SRK),4_SIK,'zero')
      ASSERT_EQ(getFloatFormat(4213.12_SRK),4_SIK,'zero')

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
        ASSERT_EQ(TRIM(tmpChar),TRIM(tmpStr),'command')
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
        ASSERT_EQ(TRIM(tmpChar),TRIM(tmpStr),'value')
      ELSE
        INFO(0) 'Could not run test for GET_ENVIRONMENT_VARIABLE'
      ENDIF

      COMPONENT_TEST('GET_CURRENT_DIRECTORY')
      CALL GET_CURRENT_DIRECTORY(DIR=tmpStr,LENGTH=n,STATUS=stat)
      ASSERT_EQ(stat,0,'STATUS (string)')
      ASSERT(strmatch(CHAR(tmpStr),'testIOutil'),'DIR (string)')
      ASSERT_EQ(n,LEN(tmpStr),'LENGTH (string)')
      n0=n; n=0;
      CALL GET_CURRENT_DIRECTORY(DIR=tmpChar,LENGTH=n,STATUS=stat)
      ASSERT_EQ(n,n0,'LENGTH (char)')
      ASSERT(strmatch(TRIM(tmpChar),CHAR(tmpStr)),'DIR (char)')
      ASSERT_EQ(stat,0,'STATUS (char)')
      CALL GET_CURRENT_DIRECTORY(LENGTH=n,STATUS=stat)
      ASSERT_EQ(n,n0,'LENGTH')
      ASSERT_EQ(stat,0,'STATUS')
      CALL GET_CURRENT_DIRECTORY(LENGTH=n)
      ASSERT_EQ(n,n0,'LENGTH')
      CALL GET_CURRENT_DIRECTORY(STATUS=stat)
      ASSERT_EQ(stat,0,'STATUS')
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
      ASSERT_EQ(mystat,0,'d1/d 2 STAT')
      CALL MAKE_DIRECTORY(str_dir,STATUS=mystat)
      ASSERT_EQ(mystat,0,'str_dir')
      !Test if directories already exist
      CALL MAKE_DIRECTORY(char_dir,STATUS=mystat)
      ASSERT_EQ(mystat,0,'char_dir')
      CALL MAKE_DIRECTORY(str_dir,STATUS=mystat)
      ASSERT_EQ(mystat,0,'str_dir')

      !Clean up tests, remove directories
      tmp='rmdir "d1/d 2/d3"'
      CALL SlashRep(tmp)
      mystat=SYSTEM(tmp)
      ASSERT_EQ(mystat,0,'d1/d 2/d3 does not exist')
      tmp='rmdir "d1/d 2"'
      CALL SlashRep(tmp)
      mystat=SYSTEM(tmp)
      ASSERT_EQ(mystat,0,'d1/d 2 does not exist')
      tmp='rmdir d1'
      CALL SlashRep(tmp)
      mystat=SYSTEM(tmp)
      ASSERT_EQ(mystat,0,'d1 does not exist')
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
      ASSERT_EQ(char_dir,'d1'//SLASH//'d 2','char')
      CALL SlashRep(str_dir)
      ASSERT_EQ(TRIM(str_dir),'d1'//SLASH//'d 2'//SLASH//'d3','string')
    ENDSUBROUTINE testSlashRep
!
ENDPROGRAM testIOutil
