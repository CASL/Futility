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
      
  USE ISO_FORTRAN_ENV
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
      
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING IOUTIL...'
  WRITE(*,*) '==================================================='
  
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  
  WRITE(*,*) 'TESTING PARAMETERS'
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
  WRITE(*,*) '---------------------------------------------------'
  
  WRITE(*,*) 'TESTING  IO_STRINGS'
  IF(.NOT.strmatch('testing','test') .OR. strmatch('TEAM','I')) THEN
    WRITE(*,*) 'strmatch(...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: strmatch(...)'
  ENDIF
  IF(nmatchstr('testing','test') /= 1 .OR. nmatchstr('TEAM','I') /= 0 .OR. &
     nmatchstr('t e s t',' ') /= 3) THEN
    WRITE(*,*) 'nmatchstr(...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: nmatchstr(...)'
  ENDIF
  IF(ANY(strfind('stesting','test') /= (/2/)) .OR. &
     SIZE(strfind('TEAM','I')) /= 0 .OR. &
     ANY(strfind('t e s t',' ') /= (/2,4,6/))) THEN
    WRITE(*,*) 'strfind(...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: strfind(...)'
  ENDIF
  string='testing'
  CALL strrep(string,'t','TT')
  IF(TRIM(string) /= 'TTesTTing') THEN
    WRITE(*,*) 'strrep(...) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: strrep(...)'
  ENDIF
!
!Test nFields
  string=''
  IF(nFields(string) /= 0) THEN
    WRITE(*,*) 'nFields('''') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: nFields('''') = 0'
  ENDIF
  string=' '
  IF(nFields(string) /= 0) THEN
    WRITE(*,*) 'nFields('' '') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: nFields('' '') = 0'
  ENDIF
  string='''arg1'
  IF(nFields(string) /= -1) THEN
    WRITE(*,*) 'nFields(''''arg1'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: nFields(''''arg1'') = -1'
  ENDIF
  string='"arg1'
  IF(nFields(string) /= -2) THEN
    WRITE(*,*) 'nFields(''"arg1'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: nFields(''"arg1'') = -2'
  ENDIF
  string='1'
  IF(nFields(string) /= 1) THEN
    WRITE(*,*) 'nFields(''1'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: nFields(''1'') = 1'
  ENDIF
  string='arg1 nmult*arg2'
  IF(nFields(string) /= 2) THEN
    WRITE(*,*) 'nFields(''arg1 nmult*arg2'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: nFields(''arg1 nmult*arg2'') = 2'
  ENDIF
  string='arg1 2*arg2'
  IF(nFields(string) /= 3) THEN
    WRITE(*,*) 'nFields(''arg1 2*arg2'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: nFields(''arg1 2*arg2'') = 3'
  ENDIF
  string='arg1 2*"arg 2"'
  IF(nFields(string) /= 3) THEN
    WRITE(*,*) 'nFields(''arg1 2*"arg 2"'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: nFields(''arg1 2*"arg 2"'') = 3'
  ENDIF
  string='cardname 1 1 2 3 1 0.2'
  IF(nFields(string) /= 7) THEN
    WRITE(*,*) 'nFields(''cardname 1 1 2 3 1 0.2'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: nFields(''cardname 1 1 2 3 1 0.2'') = 7'
  ENDIF
  string='     cardname 200*3.1 15*0.2'
  IF(nFields(string) /= 216) THEN
    WRITE(*,*) 'nFields(''    cardname 200*3.1 15*0.2'') FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: nFields(''    cardname 200*3.1 15*0.2'') = 216'
  ENDIF
  CALL getField(50,string,string2)
  IF(TRIM(string2) /= '3.1') THEN
    WRITE(*,*) 'CALL getField(50,''    cardname 200*3.1 15*0.2'',string2)' &
               //' FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getField(50,''    cardname 200*3.1 15*0.2''' &
               //',string2) = '//TRIM(string2)
  ENDIF
  CALL getField(205,string,string2)
  IF(TRIM(string2) /= '0.2') THEN
    WRITE(*,*) 'CALL getField(205,''    cardname 200*3.1 15*0.2'',string2)' &
               //' FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getField(205,''    cardname 200*3.1 15*0.2''' &
               //',string2) = '//TRIM(string2)
  ENDIF
  string='cardname 1 1 2 3 1 0.2'
  CALL getField(50,string,string2)
  IF(LEN_TRIM(string2) /= 0) THEN
    WRITE(*,*) 'CALL getField(50,''cardname 1 1 2 3 1 0.2'',string2)' &
               //' FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getField(50,''cardname 1 1 2 3 1 0.2''' &
               //',string2) = '//TRIM(string2)
  ENDIF
  CALL getField(5,string,string2)
  IF(TRIM(string2) /= '3') THEN
    WRITE(*,*) 'CALL getField(5,''cardname 1 1 2 3 1 0.2'',string2)' &
               //' FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getField(5,''cardname 1 1 2 3 1 0.2''' &
               //',string2) = '//TRIM(string2)
  ENDIF
  
  string='''/some dir/file'' arg2'
  CALL getField(1,string,string2)
  IF(TRIM(string2) /= '/some dir/file') THEN
    WRITE(*,*) 'CALL getField(1,''''/some dir/file'' arg2'//  &
               ',string2)'' FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getField(1,''''/some dir/file'''//  &
               ' arg2'',string2) = '//TRIM(string2)
  ENDIF
  CALL getField(2,string,string2)
  IF(TRIM(string2) /= 'arg2') THEN
    WRITE(*,*) 'CALL getField(2,''''/some dir/file'' arg2'//  &
               ',string2)'' FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getField(2,''''/some dir/file'''//  &
               ' arg2'',string2) = '//TRIM(string2)
  ENDIF
  string='arg1 2*"arg 2"'
  CALL getField(2,string,string2)
  IF(TRIM(string2) /= 'arg 2') THEN
    WRITE(*,*) 'CALL getField(2,''arg1 2*"arg 2"'',string2) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getField(2,''arg1 2*"arg 2"'',string2) = '// &
      TRIM(string2)
  ENDIF
  CALL getField(3,string,string2)
  IF(TRIM(string2) /= 'arg 2') THEN
    WRITE(*,*) 'CALL getField(3,''arg1 2*"arg 2"'',string2) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getField(3,''arg1 2*"arg 2"'',string2) = '// &
      TRIM(string2)
  ENDIF
  string='../../file.inp 40*2'
  CALL getField(1,string,string2)
  IF(TRIM(string2) /= '../../file.inp') THEN
    WRITE(*,*) 'CALL getField(1,''../../file.inp 40*2'',string2)' &
               //' FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getField(1,''../../file.inp 40*2''' &
               //',string2)'
  ENDIF
  CALL getField(35,string,string2)
  IF(TRIM(string2) /= '2') THEN
    WRITE(*,*) 'CALL getField(35,''../../file.inp 40*2'',string2)' &
               //' FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getField(35,''../../file.inp 40*2''' &
               //',string2)'
  ENDIF
  string='some data !a comment !another comment?'
  CALL stripComment(string)
  IF(TRIM(string) /= 'some data') THEN
    WRITE(*,*) 'CALL stripComment(string) FAILED!',string
    STOP 666
  ENDIF
  string='some data !a comment'
  CALL stripComment(string)
  IF(TRIM(string) /= 'some data') THEN
    WRITE(*,*) 'CALL stripComment(string) FAILED!',string
    STOP 666
  ENDIF
  string='!a comment'
  CALL stripComment(string)
  IF(LEN_TRIM(string) /= 0) THEN
    WRITE(*,*) 'CALL stripComment(string) FAILED!',string
    STOP 666
  ENDIF
  string=' !a comment'
  CALL stripComment(string)
  IF(LEN_TRIM(string) /= 0) THEN
    WRITE(*,*) 'CALL stripComment(string) FAILED!',string
    STOP 666
  ENDIF
  string=''
  CALL stripComment(string)
  IF(LEN_TRIM(string) /= 0) THEN
    WRITE(*,*) 'CALL stripComment(string) FAILED!',string
    STOP 666
  ENDIF
  string='some data'
  CALL stripComment(string)
  IF(TRIM(string) /= 'some data') THEN
    WRITE(*,*) 'CALL stripComment(string) FAILED!',string
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL stripComment(string)'
  ENDIF
!
!Test toUPPER
  WRITE(*,*) 'TESTING STRING MANIPULATION'
  string='239p84uyqh;jndf:JKDFH./'
  CALL toUPPER(string)
  IF(TRIM(string) /= '239P84UYQH;JNDF:JKDFH./') THEN
    WRITE(*,*) 'CALL toUPPER(string) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL toUPPER(string) = '//TRIM(string)
  ENDIF
!
!Test getFilePath, getFileName, getFileNameExt, getFileParts
  string='C:\fullpath\dir1\filenoext'
  CALL getFilePath(string,string2)
  IF(TRIM(string2) /= 'C:'//SLASH//'fullpath'//SLASH//'dir1'//SLASH) THEN
    WRITE(*,*) 'CALL getFilePath(''C:\fullpath\dir1\filenoext'',path) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFilePath(''C:\fullpath\dir1\filenoext'''// &
               ',path) = '
    WRITE(*,*) '          '//TRIM(string2)
  ENDIF
  CALL getFileName(string,string2)
  IF(TRIM(string2) /= 'filenoext') THEN
    WRITE(*,*) 'CALL getFileName(''C:\fullpath\dir1\filenoext'',fname) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileName('// &
               '''C:\fullpath\dir1\filenoext'',fname) = '//TRIM(string2)
  ENDIF
  CALL getFileNameExt(string,string2)
  IF(TRIM(string2) /= '') THEN
    WRITE(*,*) 'CALL getFileNameExt(''C:\fullpath\dir1\filenoext'',ext) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileNameExt('// &
               '''C:\fullpath\dir1\filenoext'',ext) = '//TRIM(string2)
  ENDIF
  CALL getFileParts(string,string1,string2,string3)
  IF(TRIM(string1) /= 'C:'//SLASH//'fullpath'//SLASH//'dir1'//SLASH .OR. &
     TRIM(string2) /= 'filenoext' .OR. TRIM(string3) /= '') THEN
    WRITE(*,*) 'CALL getFileParts(''C:\fullpath\dir1\filenoext'''// &
               ',path,fname,ext) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL getFileParts(''C:\fullpath\dir1\filenoext'''// &
               ',path,fname,ext)'
  ENDIF
  
  string='..\relpath\..\dir1\filenoext'
  CALL getFilePath(string,string2)
  IF(TRIM(string2) /= '..'//SLASH//'relpath'//SLASH// &
                      '..'//SLASH//'dir1'//SLASH) THEN
    WRITE(*,*) 'CALL getFilePath(''..\relpath\..\dir1\filenoext'''// &
               ',path) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFilePath(''..\relpath\..\dir1\filenoext'''// &
               ',path) = '
    WRITE(*,*) '          '//TRIM(string2)
  ENDIF
  CALL getFileName(string,string2)
  IF(TRIM(string2) /= 'filenoext') THEN
    WRITE(*,*) 'CALL getFileName(''..\relpath\..\dir1\filenoext'',fname) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileName('// &
               '''..\relpath\..\dir1\filenoext'',fname) = '//TRIM(string2)
  ENDIF
  CALL getFileNameExt(string,string2)
  IF(TRIM(string2) /= '') THEN
    WRITE(*,*) 'CALL getFileNameExt(''..\relpath\..\dir1\filenoext'',ext) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileNameExt('// &
               '''..\relpath\..\dir1\filenoext'',ext) = '//TRIM(string2)
  ENDIF
  CALL getFileParts(string,string1,string2,string3)
  IF(TRIM(string1) /= '..'//SLASH//'relpath'//SLASH// &
                      '..'//SLASH//'dir1'//SLASH .OR. &
     TRIM(string2) /= 'filenoext' .OR. TRIM(string3) /= '') THEN
    WRITE(*,*) 'CALL getFileParts(''..\relpath\..\dir1\filenoext'''// &
               ',path,fname,ext) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL getFileParts(''..\relpath\..\dir1\filenoext'''// &
               ',path,fname,ext)'
  ENDIF
  
  string='/fullpath/dir1/filenoext'
  CALL getFilePath(string,string2)
  IF(TRIM(string2) /= SLASH//'fullpath'//SLASH//'dir1'//SLASH) THEN
    WRITE(*,*) 'CALL getFilePath(''/fullpath/dir1/filenoext'''// &
               ',path) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFilePath(''/fullpath/dir1/filenoext'''// &
               ',path) = '
    WRITE(*,*) '          '//TRIM(string2)
  ENDIF
  CALL getFileName(string,string2)
  IF(TRIM(string2) /= 'filenoext') THEN
    WRITE(*,*) 'CALL getFileName(''/fullpath/dir1/filenoext'',fname) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileName('// &
               '''/fullpath/dir1/filenoext'',fname) = '//TRIM(string2)
  ENDIF
  CALL getFileNameExt(string,string2)
  IF(TRIM(string2) /= '') THEN
    WRITE(*,*) 'CALL getFileNameExt(''/fullpath/dir1/filenoext'',ext) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileNameExt('// &
               '''/fullpath/dir1/filenoext'',ext) = '//TRIM(string2)
  ENDIF
  CALL getFileParts(string,string1,string2,string3)
  IF(TRIM(string1) /= SLASH//'fullpath'//SLASH//'dir1'//SLASH .OR. &
     TRIM(string2) /= 'filenoext' .OR. TRIM(string3) /= '') THEN
    WRITE(*,*) 'CALL getFileParts(''/fullpath/dir1/filenoext'''// &
               ',path,fname,ext) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL getFileParts(''/fullpath/dir1/filenoext'''// &
               ',path,fname,ext)'
  ENDIF
  
  string='../relpath/../dir1/filenoext'
  CALL getFilePath(string,string2)
  IF(TRIM(string2) /= '..'//SLASH//'relpath'//SLASH// &
                      '..'//SLASH//'dir1'//SLASH) THEN
    WRITE(*,*) 'CALL getFilePath(''../relpath/../dir1/filenoext'''// &
               ',path) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFilePath(''../relpath/../dir1/filenoext'''// &
               ',path) = '
    WRITE(*,*) '          '//TRIM(string2)
  ENDIF
  CALL getFileName(string,string2)
  IF(TRIM(string2) /= 'filenoext') THEN
    WRITE(*,*) 'CALL getFileName(''../relpath/../dir1/filenoext'',fname) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileName('// &
               '''../relpath/../dir1/filenoext'',fname) = '//TRIM(string2)
  ENDIF
  CALL getFileNameExt(string,string2)
  IF(TRIM(string2) /= '') THEN
    WRITE(*,*) 'CALL getFileNameExt(''../relpath/../dir1/filenoext'',ext) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileNameExt('// &
               '''../relpath/../dir1/filenoext'',ext) = '//TRIM(string2)
  ENDIF
  CALL getFileParts(string,string1,string2,string3)
  IF(TRIM(string1) /= '..'//SLASH//'relpath'//SLASH// &
                      '..'//SLASH//'dir1'//SLASH .OR. &
     TRIM(string2) /= 'filenoext' .OR. TRIM(string3) /= '') THEN
    WRITE(*,*) 'CALL getFileParts(''../relpath/../dir1/filenoext'''// &
               ',path,fname,ext) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL getFileParts(''../relpath/../dir1/filenoext'''// &
               ',path,fname,ext)'
  ENDIF
  
  string='C:\fullpath\dir1\file.two.ext'
  CALL getFilePath(string,string2)
  IF(TRIM(string2) /= 'C:'//SLASH//'fullpath'//SLASH//'dir1'//SLASH) THEN
    WRITE(*,*) 'CALL getFilePath(''C:\fullpath\dir1\file.two.ext'',path) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFilePath(''C:\fullpath\dir1\file.two.ext'''// &
               ',path) = '
    WRITE(*,*) '          '//TRIM(string2)
  ENDIF
  CALL getFileName(string,string2)
  IF(TRIM(string2) /= 'file.two.ext') THEN
    WRITE(*,*) 'CALL getFileName(''C:\fullpath\dir1\file.two.ext'',fname) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileName('// &
               '''C:\fullpath\dir1\file.two.ext'',fname) = '//TRIM(string2)
  ENDIF
  CALL getFileNameExt(string,string2)
  IF(TRIM(string2) /= '.ext') THEN
    WRITE(*,*) 'CALL getFileNameExt(''C:\fullpath\dir1\file.two.ext'',ext) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileNameExt('// &
               '''C:\fullpath\dir1\file.two.ext'',ext) = '//TRIM(string2)
  ENDIF
  CALL getFileParts(string,string1,string2,string3)
  IF(TRIM(string1) /= 'C:'//SLASH//'fullpath'//SLASH//'dir1'//SLASH .OR. &
     TRIM(string2) /= 'file.two' .OR. TRIM(string3) /= '.ext') THEN
    WRITE(*,*) 'CALL getFileParts(''C:\fullpath\dir1\file.two.ext'''// &
               ',path,fname,ext) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL getFileParts(''C:\fullpath\dir1\file.two.ext'''// &
               ',path,fname,ext)'
  ENDIF
  
  string='..\relpath\..\dir1\file.two.ext'
  CALL getFilePath(string,string2)
  IF(TRIM(string2) /= '..'//SLASH//'relpath'//SLASH// &
                      '..'//SLASH//'dir1'//SLASH) THEN
    WRITE(*,*) 'CALL getFilePath(''..\relpath\..\dir1\file.two.ext'''// &
               ',path) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFilePath(''..\relpath\..\dir1\file.two.ext'''// &
               ',path) = '
    WRITE(*,*) '          '//TRIM(string2)
  ENDIF
  CALL getFileName(string,string2)
  IF(TRIM(string2) /= 'file.two.ext') THEN
    WRITE(*,*) 'CALL getFileName(''..\relpath\..\dir1\file.two.ext'',fname) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileName('// &
               '''..\relpath\..\dir1\file.two.ext'',fname) = '//TRIM(string2)
  ENDIF
  CALL getFileNameExt(string,string2)
  IF(TRIM(string2) /= '.ext') THEN
    WRITE(*,*) 'CALL getFileNameExt(''..\relpath\..\dir1\file.two.ext'',ext) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileNameExt('// &
               '''..\relpath\..\dir1\file.two.ext'',ext) = '//TRIM(string2)
  ENDIF
  CALL getFileParts(string,string1,string2,string3)
  IF(TRIM(string1) /= '..'//SLASH//'relpath'//SLASH// &
                      '..'//SLASH//'dir1'//SLASH .OR. &
     TRIM(string2) /= 'file.two' .OR. TRIM(string3) /= '.ext') THEN
    WRITE(*,*) 'CALL getFileParts(''..\relpath\..\dir1\file.two.ext'''// &
               ',path,fname,ext) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL getFileParts(''..\relpath\..\dir1\file.two.ext'''// &
               ',path,fname,ext)'
  ENDIF
  
  string='/fullpath/dir1/file.two.ext'
  CALL getFilePath(string,string2)
  IF(TRIM(string2) /= SLASH//'fullpath'//SLASH//'dir1'//SLASH) THEN
    WRITE(*,*) 'CALL getFilePath(''/fullpath/dir1/file.two.ext'''// &
               ',path) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFilePath(''/fullpath/dir1/file.two.ext'''// &
               ',path) = '
    WRITE(*,*) '          '//TRIM(string2)
  ENDIF
  CALL getFileName(string,string2)
  IF(TRIM(string2) /= 'file.two.ext') THEN
    WRITE(*,*) 'CALL getFileName(''/fullpath/dir1/file.two.ext'',fname) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileName('// &
               '''/fullpath/dir1/file.two.ext'',fname) = '//TRIM(string2)
  ENDIF
  CALL getFileNameExt(string,string2)
  IF(TRIM(string2) /= '.ext') THEN
    WRITE(*,*) 'CALL getFileNameExt(''/fullpath/dir1/file.two.ext'',ext) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileNameExt('// &
               '''/fullpath/dir1/file.two.ext'',ext) = '//TRIM(string2)
  ENDIF
  CALL getFileParts(string,string1,string2,string3)
  IF(TRIM(string1) /= SLASH//'fullpath'//SLASH//'dir1'//SLASH .OR. &
     TRIM(string2) /= 'file.two' .OR. TRIM(string3) /= '.ext') THEN
    WRITE(*,*) 'CALL getFileParts(''/fullpath/dir1/file.two.ext'''// &
               ',path,fname,ext) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL getFileParts(''/fullpath/dir1/file.two.ext'''// &
               ',path,fname,ext)'
  ENDIF
  
  string='../relpath/../dir1/file.two.ext'
  CALL getFilePath(string,string2)
  IF(TRIM(string2) /= '..'//SLASH//'relpath'//SLASH// &
                      '..'//SLASH//'dir1'//SLASH) THEN
    WRITE(*,*) 'CALL getFilePath(''../relpath/../dir1/file.two.ext'''// &
               ',path) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFilePath(''../relpath/../dir1/file.two.ext'''// &
               ',path) = '
    WRITE(*,*) '          '//TRIM(string2)
  ENDIF
  CALL getFileName(string,string2)
  IF(TRIM(string2) /= 'file.two.ext') THEN
    WRITE(*,*) 'CALL getFileName(''../relpath/../dir1/file.two.ext'',fname) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileName('// &
               '''../relpath/../dir1/file.two.ext'',fname) = '//TRIM(string2)
  ENDIF
  CALL getFileNameExt(string,string2)
  IF(TRIM(string2) /= '.ext') THEN
    WRITE(*,*) 'CALL getFileNameExt(''../relpath/../dir1/file.two.ext'',ext) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL getFileNameExt('// &
               '''../relpath/../dir1/file.two.ext'',ext) = '//TRIM(string2)
  ENDIF
  CALL getFileParts(string,string1,string2,string3)
  IF(TRIM(string1) /= '..'//SLASH//'relpath'//SLASH// &
                      '..'//SLASH//'dir1'//SLASH .OR. &
     TRIM(string2) /= 'file.two' .OR. TRIM(string3) /= '.ext') THEN
    WRITE(*,*) 'CALL getFileParts(''../relpath/../dir1/file.two.ext'''// &
               ',path,fname,ext) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL getFileParts(''../relpath/../dir1/file.two.ext'''// &
               ',path,fname,ext)'
  ENDIF
  !Coverage/Error checking
  testFile%e => e
  CALL getFileParts(string,shortstring1,shortstring2,shortstring3)
  CALL getFileParts(string,shortstring1,shortstring2,shortstring3, &
    testFile%e)
  WRITE(*,*) '---------------------------------------------------'
!
!Test BaseFileType methods
  WRITE(*,*) 'TESTING BASE FILE TYPE METHODS'
  
  IF(testFile%isRead()) THEN
    WRITE(*,*) 'testFile%isRead() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%isRead()'
  ENDIF
  IF(testFile%isWrite()) THEN
    WRITE(*,*) 'testFile%isWrite() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%isWrite()'
  ENDIF
  IF(testFile%isEOF()) THEN
    WRITE(*,*) 'testFile%isEOF() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%isEOF()'
  ENDIF
  IF(testFile%isOpen()) THEN
    WRITE(*,*) 'testFile%isOpen() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%isOpen()'
  ENDIF
  IF(LEN_TRIM(testFile%getFilePath()) /= 0) THEN
    WRITE(*,*) 'testFile%getFilePath() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%getFilePath()'
  ENDIF
  IF(LEN_TRIM(testFile%getFileName()) /= 0) THEN
    WRITE(*,*) 'testFile%getFileName() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%getFileName()'
  ENDIF
  IF(LEN_TRIM(testFile%getFileExt()) /= 0) THEN
    WRITE(*,*) 'testFile%getFileExt() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%getFileExt()'
  ENDIF
  CALL testFile%getFileParts(string1,string2,string3)
  IF(LEN_TRIM(string1) /= 0 .OR. LEN_TRIM(string2) /= 0 .OR. &
    LEN_TRIM(string3) /= 0) THEN
    WRITE(*,*) 'CALL testFile%getFileParts(string1,string2,string3) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testFile%getFileParts(string1,string2,string3)'
  ENDIF
  testFile%e => NULL()
  CALL testFile%setFilePath('filepath/')
  testFile%e => e
  IF(TRIM(testFile%getFilePath()) /= 'filepath/') THEN
    WRITE(*,*) 'CALL testFile%setFilePath(...) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testFile%setFilePath(...)'
  ENDIF
  testFile%e => NULL()
  CALL testFile%setFileName('filename')
  testFile%e => e
  IF(TRIM(testFile%getFileName()) /= 'filename') THEN
    WRITE(*,*) 'CALL testFile%setFileName(...) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testFile%setFileName(...)'
  ENDIF
  testFile%e => NULL()
  CALL testFile%setFileExt('.fext')
  testFile%e => e
  IF(TRIM(testFile%getFileExt()) /= '.fext') THEN
    WRITE(*,*) 'CALL testFile%setFileExt(...) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testFile%setFileExt(...)'
  ENDIF
  testFile%e => NULL()
  CALL testFile%getFileParts(string1,string2,string3)
  testFile%e => e
  CALL testFile%setReadStat(.TRUE.)
  IF(.NOT.testFile%isRead()) THEN
    WRITE(*,*) 'CALL testFile%setReadStat(...) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testFile%setReadStat(...)'
  ENDIF
  CALL testFile%setWriteStat(.TRUE.)
  IF(.NOT.testFile%isWrite()) THEN
    WRITE(*,*) 'CALL testFile%setWriteStat(...) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testFile%setWriteStat(...)'
  ENDIF

  !Error checking and coverage
  CALL testFile%getFileParts(string1(1:1),string2(1:1),string3(1:1))
  string(MAX_INPUT_FILE_LINE_LEN:MAX_INPUT_FILE_LINE_LEN)='.'
  CALL testFile%setFilePath(string)
  CALL testFile%setFileName(string)
  CALL testFile%setFileExt(string)
  CALL testFile%setEOFstat(.TRUE.)
  CALL testFile%setOpenStat(.TRUE.)
  IF(.NOT.testFile%isOpen()) THEN
    WRITE(*,*) 'CALL testFile%setOpen(...) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testFile%setOpen(...)'
  ENDIF
  CALL testFile%setEOFstat(.TRUE.)
  IF(.NOT.testFile%isEOF()) THEN
    WRITE(*,*) 'CALL testFile%setEOFstat(...) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testFile%setEOFstat(...)'
  ENDIF
  CALL testFile%setFilePath('filepath/')
  CALL testFile%setFileName('filename')
  CALL testFile%setFileExt('.fext')
  CALL testFile%setEOFstat(.FALSE.)
  CALL testFile%setReadStat(.FALSE.)
  CALL testFile%setWriteStat(.FALSE.)
  CALL testFile%setOpenStat(.FALSE.)
  WRITE(*,*) '---------------------------------------------------'
!
!Test FortranFileType
  WRITE(*,*) 'TESTING FORTRAN FILE TYPE'
  
  CALL testFile%clear()
  WRITE(*,*) '  Passed: CALL testFile%clear()'
  
  !Configure exception handler for testing
  testFile%e => e
  testFile2%e => e
  
  IF(testFile%getUnitNo() /= -1) THEN
    WRITE(*,*) 'testFile%getUnitNo() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%getUnitNo()'
  ENDIF
  IF(testFile%isFormatted()) THEN
    WRITE(*,*) 'testFile%isFormatted() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%isFormatted()'
  ENDIF
  IF(testFile%isDirect()) THEN
    WRITE(*,*) 'testFile%isDirect() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%isDirect()'
  ENDIF
  IF(testFile%getRecLen() /= -1) THEN
    WRITE(*,*) 'testFile%getRecLen() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%getRecLen()'
  ENDIF
  IF(testFile%isPadded()) THEN
    WRITE(*,*) 'testFile%isPadded() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%isPadded()'
  ENDIF
  IF(testFile%isNew()) THEN
    WRITE(*,*) 'testFile%isNew() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%isNew()'
  ENDIF
  IF(testFile%isOverwrite()) THEN
    WRITE(*,*) 'testFile%isOverwrite() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%isOverwrite()'
  ENDIF
  !Called for coverage/error checking
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
  IF(.NOT.testFile%isInit()) THEN
    WRITE(*,*) 'testFile%isInit() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testFile%isInit()'
  ENDIF
  IF(TRIM(testFile%getFileName()) /= 'testFile') THEN
    WRITE(*,*) 'CALL testFile%initialize(...) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testFile%initialize(...)'
  ENDIF
  CALL testFile%fopen()
  IF(.NOT.testFile%isOpen()) THEN
    WRITE(*,*) 'CALL testFile%fopen() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testFile%fopen()'
  ENDIF
  !Coverage/Error checking
  CALL testFile%fopen()
  CALL testFile2%initialize(UNIT=12,FILE='./testFile.txt')
  
  testFile%e => NULL()
  CALL testFile%fbackspace()
  WRITE(*,*) '  Passed: CALL testFile%fbackspace()'
  CALL testFile%frewind()
  testFile%e => e
  WRITE(*,*) '  Passed: CALL testFile%frewind()'
  CALL testFile%fclose()
  IF(testFile%isOpen()) THEN
    WRITE(*,*) 'CALL testFile%fclose() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testFile%fclose()'
  ENDIF
  CALL testFile%fdelete()
  INQUIRE(FILE='./testFile.txt',EXIST=lexist)
  IF(lexist) THEN
    WRITE(*,*) 'CALL testFile%fdelete() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testFile%fdelete()'
  ENDIF
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
  WRITE(*,*) '---------------------------------------------------'
  
  WRITE(*,*) 'TESTING LOG FILE TYPE'
  testLogFile%e => e
  IF(testLogFile%isEcho()) THEN
    WRITE(*,*) 'CALL testLogFile%isEcho() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testLogFile%isEcho()'
  ENDIF
  CALL testLogFile%setEcho(.TRUE.)
  IF(.NOT.testLogFile%isEcho()) THEN
    WRITE(*,*) 'CALL testLogFile%setEcho(.TRUE.) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testLogFile%setEcho(.TRUE.)'
  ENDIF
  CALL testLogFile%initialize(UNIT=66,FILE='./test.log')
  IF(TRIM(testLogFile%getFileName()) /= 'test') THEN
    WRITE(*,*) 'CALL testLogFile%initialize(...) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testLogFile%initialize(...)'
  ENDIF
  CALL testLogFile%fopen()
  testLogFile%e => NULL()
  CALL testLogFile%message('Passed: CALL testLogFile%message(...)',.TRUE.,.TRUE.)
  testLogFile%e => e
  CALL testLogFile%message('Passed: CALL testLogFile%message(...)',.FALSE.,.TRUE.)
  CALL testLogFile%clear(.FALSE.)
  IF(testLogFile%getUnitNo() /= -1) THEN
    WRITE(*,*) 'CALL testLogFile%clear() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testLogFile%clear()'
  ENDIF
  CALL testFile%initialize(UNIT=66,FILE='./test.log',STATUS='OLD',ACTION='READ')
  CALL testFile%fopen()
  READ(66,'(a)') string
  IF(TRIM(string(12:LEN(string))) /= ' Passed: CALL testLogFile%message(...)') THEN
    WRITE(*,*) TRIM(string(12:LEN(string)))
    WRITE(*,*) 'CALL testLogFile%message(...) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testLogFile%message(...)'
  ENDIF
  CALL testFile%clear(.TRUE.)
  WRITE(*,*) '---------------------------------------------------'
  
  WRITE(*,*) 'TESTING INPUT FILE TYPE'
  testInpFile%e => e
  IF(testInpFile%getEchoStat()) THEN
    WRITE(*,*) 'testInpFile%getEchoStat() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testInpFile%getEchoStat()'
  ENDIF
  IF(testInpFile%getEchoUnit() /= -1) THEN
    WRITE(*,*) 'testInpFile%getEchoUnit() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testInpFile%getEchoUnit()'
  ENDIF
  CALL testInpFile%setEchoStat(.TRUE.)
  IF(.NOT.testInpFile%getEchoStat()) THEN
    WRITE(*,*) 'CALL testInpFile%setEchoStat(...) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testInpFile%setEchoStat(...)'
  ENDIF
  CALL testInpFile%setEchoUnit(0)
  CALL testInpFile%setEchoUnit(25)
  IF(testInpFile%getEchoUnit() /= 25) THEN
    WRITE(*,*) 'CALL testInpFile%setEchoUnit(...) FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testInpFile%setEchoUnit(...)'
  ENDIF
  
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
  IF(TRIM(string) /= 'sample oneline 1') THEN
    WRITE(*,*) 'testInpFile%fgetl() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testInpFile%fgetl()'
  ENDIF
  IF(testInpFile%getProbe() /= 's') THEN
    WRITE(*,*) 'testInpFile%getProbe() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testInpFile%getProbe()'
  ENDIF
  CALL testInpFile%frewind()
  IF(LEN_TRIM(testInpFile%getProbe()) /= 0) THEN
    WRITE(*,*) 'CALL testInpFile%frewind() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testInpFile%frewind()'
  ENDIF
  string=testInpFile%fgetl()
  CALL testInpFile%fbackspace()
  IF(LEN_TRIM(testInpFile%getProbe()) /= 0) THEN
    WRITE(*,*) 'CALL testInpFile%fbackspace() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: CALL testInpFile%fbackspace()'
  ENDIF
  CALL testFile%clear(.TRUE.)
  CALL testInpFile%setEchoStat(.FALSE.)
  testInpFile%e => NULL()
  string=testInpFile%fgetl()
  testInpFile%e => e
  CALL testInpFile%clear(.TRUE.)
  IF(testInpFile%getEchoUnit() /= -1 .OR. testInpFile%getEchostat()) THEN
    WRITE(*,*) 'testInpFile%clear() FAILED!'
    STOP 666 
  ELSE
    WRITE(*,*) '  Passed: testInpFile%clear()'
  ENDIF
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING IOUTIL PASSED!'
  WRITE(*,*) '==================================================='
  
ENDPROGRAM testIOutil
