!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testStrings
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE Strings

  IMPLICIT NONE

  LOGICAL(SBK) :: bool
  INTEGER(SIK) :: i
  INTEGER(SNK) :: testSNK,testSNKarray(2)
  INTEGER(SLK) :: testSLK,testSLKarray(2)
  REAL(SSK) :: testSSK,testSSKarray(2)
  REAL(SDK) :: testSDK,testSDKarray(2)
  CHARACTER(LEN=10) :: char10
  CHARACTER(LEN=20) :: char20
  TYPE(StringType) :: testString,testString2,testStringArray(10),testarray(2)
  TYPE(StringType) :: test1a(2),test1a2(2),test2a(2,2),test2a2(2,2)
  TYPE(StringType) :: test3a(2,2,2),test3a2(2,2,2)
  TYPE(StringType),ALLOCATABLE :: s1a(:),s1a2(:),s2a(:,:),s2a2(:,:)

  CREATE_TEST('TEST STRINGS')
!
!Test methods for uninitialized state
  !length of an empty string
  COMPONENT_TEST('Assignment Char to String Scalar')
  ASSERT(LEN(testString) == 0,'LEN(testString) (uninit)')
  ASSERT(LEN_TRIM(testString) == 0,'LEN_TRIM(testString) (uninit)')
  ASSERT(CHAR(testString) == '','CHAR(testString) (uninit)')
  ASSERT(TRIM(testString) == '','TRIM(testString) (uninit)')
!
!Test assigning an empty character to a string
  char10=''
  testString=char10
  ASSERT(CHAR(testString) == char10,'CHAR(testString) (empty)')
  ASSERT(LEN(testString) == LEN(char10),'LEN(testString) (empty)')
  ASSERT(LEN_TRIM(testString) == LEN_TRIM(char10),'LEN_TRIM(testString) (empty)')
  ASSERT(TRIM(testString) == '','TRIM(testString) (empty)')
!
!Test assigning a zero length character to a string
  testString=TRIM(char10)
  ASSERT(CHAR(testString) == TRIM(char10),'CHAR(testString) (zero-length)')
  ASSERT(LEN(testString) == 0,'LEN(testString) (zero-length)')
  ASSERT(LEN_TRIM(testString) == 0,'LEN_TRIM(testString) (zero-length)')
  ASSERT(TRIM(testString) == '','TRIM(testString) (zero-length)')
!
!Test assigning a non-empty character to a string from a constant
  testString='constant'
  ASSERT(CHAR(testString) == 'constant','CHAR(testString) (=''constant'')')
  ASSERT(CHAR(testString,1,1) == 'c       ','CHAR(testString,1,1) (=''constant'')')
  ASSERT(CHAR(testString,2,3) == 'on       ','CHAR(testString,2,3) (=''constant'')')
  ASSERT(CHAR(testString,-1,100) == 'constant','CHAR(testString,-1,100) (=''constant'')')
  ASSERT(LEN(testString) == 8,'LEN(testString) (=''constant'')')
  ASSERT(LEN_TRIM(testString) == 8,'LEN_TRIM(testString) (=''constant'')')
  ASSERT(TRIM(testString) == 'constant','TRIM(testString) (=''constant'')')
!
!Test assigning a non-empty character to a string from a variable
  char10='variable'
  testString=char10
  ASSERT(CHAR(testString) == 'variable  ','CHAR(testString) (=''variable'')')
  ASSERT(LEN(testString) == 10,'LEN(testString) (=''variable'')')
  ASSERT(LEN_TRIM(testString) == 8,'LEN_TRIM(testString) (=''variable'')')
  ASSERT(TRIM(testString) == 'variable','TRIM(testString) (=''variable'')')
!
!Test assigning a string to a character
  COMPONENT_TEST('Assignment String Scalar to Char')
  char10='-'
  testString='testString1'
  char10=testString
  char20=testString
  ASSERT(char10 == 'testString','char10=testString')
  ASSERT(char20 == 'testString1         ','char20=testString')
!
!Test assigning a string to a string
  COMPONENT_TEST('Assignment String Scalar to String Scalar')
  testString2='testString2'
  testString2=testString
  ASSERT(CHAR(testString2) == 'testString1','testString2=testString')
!
!Test assigning a short integer to string
  COMPONENT_TEST('Assignment 32-bit Integer to String')
  testSNK=1_SNK
  testString=testSNK
  ASSERT(CHAR(testString) == '1','testString=testSNK : 1')
  testString=-HUGE(testSNK)
  ASSERT(CHAR(testString) == '-2147483647','testString=testSNK : lowest')
  testString=HUGE(testSNK)
  ASSERT(CHAR(testString) == '2147483647','testString=testSNK : highest')
  testSNKarray=(/0,5/)
  testarray=testSNKarray
  ASSERT(CHAR(testarray(1)) == '0','testString=testSNK : array(1)')
  ASSERT(CHAR(testarray(2)) == '5','testString=testSNK : array(2)')
!
!Test assigning a long integer to string
  COMPONENT_TEST('Assignment 64-bit Integer to String')
  testSLK=1_SLK
  testString=testSLK
  ASSERT(CHAR(testString) == '1','testString=testSLK : 1')
  testString=-HUGE(testSLK)
  ASSERT(CHAR(testString) == '-9223372036854775807','testString=testSLK : lowest')
  testString=HUGE(testSLK)
  ASSERT(CHAR(testString) == '9223372036854775807','testString=testSLK : highest')
  testSLKarray=(/0,5/)
  testarray=testSLKarray
  ASSERT(CHAR(testarray(1)) == '0','testString=testSLK : array(1)')
  ASSERT(CHAR(testarray(2)) == '5','testString=testSLK : array(2)')
!
!Test assigning a single real to string
  COMPONENT_TEST('Assignment 32-bit Real to String')
  testSSK=1.01_SSK
  testString=testSSK
  ASSERT(CHAR(testString) == '1.01000E+00','testString=testSSK : 1')
  testSSK=-testSSK
  testString=SQRT(testSSK)
  ASSERT(CHAR(testString) == 'NaN','testString=testSSK : NaN')
  testString=-HUGE(testSSK)
  ASSERT(CHAR(testString) == '-3.40282E+38','testString=testSSK : lowest')
  testString=HUGE(testSSK)
  ASSERT(CHAR(testString) == '3.40282E+38','testString=testSSK : highest')
  testString=-TINY(testSSK)
  ASSERT(CHAR(testString) == '-1.17549E-38','testString=testSSK : smallest negative')
  testString=TINY(testSSK)
  ASSERT(CHAR(testString) == '1.17549E-38','testString=testSSK : smallest positive')
  testSSKarray=(/-2.0_SSK,50.0_SSK/)
  testarray=testSSKarray
  ASSERT(CHAR(testarray(1)) == '-2.00000E+00','testString=testSSK : array(1)')
  ASSERT(CHAR(testarray(2)) == '5.00000E+01','testString=testSSK : array(2)')
!
!Test assigning a double real to string
  COMPONENT_TEST('Assignment 64-bit Real to String')
  testSDK=1.01_SDK
  testString=testSDK
  ASSERT(CHAR(testString) == '1.010000000000000E+00','testString=testSDK : 1.01')
  testSDK=-testSDK
  testString=SQRT(testSDK)
  ASSERT(CHAR(testString) == 'NaN','testString=testSDK : NaN')
  !Since HUGE produces a triple digit exponent, it seems the 'E' disappears when
  !writing.
  testString=-HUGE(testSDK)
  ASSERT(CHAR(testString) == '-1.797693134862316+308','testString=testSDK : lowest')
  testString=HUGE(testSDK)
  ASSERT(CHAR(testString) == '1.797693134862316+308','testString=testSDK : highest')
  testString=-TINY(testSDK)
  ASSERT(CHAR(testString) == '-2.225073858507201-308','testString=testSDK : smallest negative')
  testString=TINY(testSDK)
  ASSERT(CHAR(testString) == '2.225073858507201-308','testString=testSDK : smallest positive')
  testSDKarray=(/-2.0_SDK,50.0_SDK/)
  testarray=testSDKarray
  ASSERT(CHAR(testarray(1)) == '-2.000000000000000E+00','testString=testSDK : array(1)')
  ASSERT(CHAR(testarray(2)) == '5.000000000000000E+01','testString=testSDK : array(2)')
!
!Test assigning a char to a 1-D string array
  COMPONENT_TEST('Assignment char to 1-D String Array')
  ALLOCATE(s1a(2))
  s1a(1)='test'
  s1a(2)='test2'
  s1a='pass'
  ASSERT(s1a(1) == 'pass' .AND. s1a(2) == 'pass','char to 1-D array')
  s1a=''
  ASSERT(s1a(1) == '' .AND. s1a(2) == '','char to 1-D array nullify')
  DEALLOCATE(s1a)
!
!Test assigning a char to a 2-D string array
  COMPONENT_TEST('Assignment char to 2-D String Array')
  ALLOCATE(s2a(2,2))
  s2a(1,1)='test'; s2a(1,2)='test'
  s2a(2,1)='test2'; s2a(2,2)='test2'
  s2a='pass'
  bool=s2a(1,1) == 'pass' .AND. s2a(1,2) == 'pass' .AND. s2a(2,1) == 'pass' .AND. &
    s2a(2,2) == 'pass'
  ASSERT(bool,'char to 2-D array')
  s2a=''
  bool=s2a(1,1) == '' .AND. s2a(1,2) == '' .AND. s2a(2,1) == '' .AND. s2a(2,2) == ''
  ASSERT(bool,'char to 2-D array nullify')
  DEALLOCATE(s2a)

!
!Test assigning an array of strings to an array of strings
  !null assignmnet (should deallocate the array)
  COMPONENT_TEST('Assignment 1-D String Array to 1-D String Array')
  testString2='testString2'
  ALLOCATE(s1a(1))
  ALLOCATE(s1a2(1))
  s1a(1)='test'
  s1a=s1a2(1:0)
  ASSERT(ALLOCATED(s1a),'null array assignment')
  ASSERT(SIZE(s1a,DIM=1) == 1,'SIZE null array assignment')
  ASSERT(s1a(1) == '','null array assignment')
  !assign array to allocated array
  s1a2(1)='test2'
  s1a=s1a2
  ASSERT(ALLOCATED(s1a),'allocated array assignment')
  ASSERT(SIZE(s1a,DIM=1) == 1,'SIZE array assignment')
  ASSERT(s1a(1)=='test2','test array assignment')
  !assign array to allocated array
  DEALLOCATE(s1a2)
  ALLOCATE(s1a2(2))
  s1a2(1)='one'; s1a2(2)='two'
  s1a=s1a2
  ASSERT(ALLOCATED(s1a),'allocated array assignment')
  ASSERT(SIZE(s1a,DIM=1) == 1,'SIZE array assignment')
  ASSERT(s1a(1)=='','test array assignment')
  DEALLOCATE(s1a)
  DEALLOCATE(s1a2)
!
!Test assigning an array of strings to an array of strings
  !null assignmnet (should deallocate the array)
  COMPONENT_TEST('Assignment 2-D String Array to 2-D String Array')
  testString2='testString2'
  ALLOCATE(s2a(2,2))
  ALLOCATE(s2a2(2,2))
  s2a='test'
  s2a=s2a2(1:0,1:0)
  ASSERT(ALLOCATED(s2a),'null array assignment')
  ASSERT(SIZE(s2a,DIM=1) == 2 .AND. SIZE(s2a,DIM=2) == 2,'SIZE null array assignment')
  bool=s2a(1,1) == '' .AND. s2a(1,2) == '' .AND. s2a(2,1) == '' .AND. s2a(2,2) == ''
  ASSERT(bool,'null array assignment')
  !assign array to allocated array
  s2a2='test2'
  s2a=s2a2
  ASSERT(ALLOCATED(s2a),'allocated array assignment')
  ASSERT(SIZE(s2a,DIM=1) == 2 .AND. SIZE(s2a,DIM=2) == 2,'SIZE array assignment')
  bool=s2a(1,1) == 'test2' .AND. s2a(1,2) == 'test2' .AND. s2a(2,1) == 'test2' .AND. s2a(2,2) == 'test2'
  ASSERT(bool,'test array assignment')
  !assign array to allocated array
  DEALLOCATE(s2a2)
  ALLOCATE(s2a2(2,3))
  s2a2='one'
  s2a=s2a2
  ASSERT(ALLOCATED(s2a),'allocated array assignment')
  ASSERT(SIZE(s2a,DIM=1) == 2 .AND. SIZE(s2a,DIM=2) == 2,'SIZE array assignment')
  bool=s2a(1,1) == '' .AND. s2a(1,2) == '' .AND. s2a(2,1) == '' .AND. s2a(2,2) == ''
  ASSERT(bool,'test array assignment')
  DEALLOCATE(s2a)
  DEALLOCATE(s2a2)

!
!Test ADJUSTL and ADJUSTR
  COMPONENT_TEST('ADJUSTL')
  char10=' - '
  testString=char10
  ASSERT(ADJUSTL(testString) == ADJUSTL(char10),'ADJUSTL(testString)')
  COMPONENT_TEST('ADJUSTR')
  char10=' - '
  testString=char10
  ASSERT(ADJUSTR(testString) == ADJUSTR(char10),'ADJUSTR(testString)')
!
!Test INDEX
  COMPONENT_TEST('INDEX')
  char10='  - -'
  testString=char10
  testString2='-'
  ASSERT(INDEX(testString,'-') == 3,'INDEX(testString,''-'')')
  ASSERT(INDEX(testString,'-',.TRUE.) == 5,'INDEX(testString,''-'',.TRUE.)')
  ASSERT(INDEX(char10,testString2) == 3,'INDEX(char10,testString2)')
  ASSERT(INDEX(char10,testString2,.TRUE.) == 5,'INDEX(char10,testString2,.TRUE.)')
  ASSERT(INDEX(testString,testString2) == 3,'INDEX(testString,testString2)')
  ASSERT(INDEX(testString,testString2,.TRUE.) == 5,'INDEX(testString,testString2,.TRUE.)')
!
!Test // operator
  COMPONENT_TEST('//')
  char10='  -'
  testString=char10
  ASSERT('"'//testString == '"  -       ','''"''//testString')
  ASSERT(testString//'"' == '  -       "','testString//''"''')
  ASSERT(testString//testString == '  -         -       ','testString//testString')

!
!Test == operator
  COMPONENT_TEST('==')
  testString2=ADJUSTL(char10)
  ASSERT(testString == char10,'testString == char10')
  ASSERT(.NOT. char10 == testString2,'char10 == testString2')
  ASSERT(.NOT. testString == testString2,'testString == testString2')
!
!Test /= operator
  COMPONENT_TEST('/=')
  ASSERT(.NOT.(testString /= char10),'testString == char10')
  ASSERT(char10 /= testString2,'char10 /= testString2')
  ASSERT(testString /= testString2,'testString /= testString2')

!
!Test ANY operator
  COMPONENT_TEST('ANY')
  DO i=1,10
    testStringArray(i)='test'
  ENDDO
  testString='bad test'
  char10='bad test'
  !ASSERT(.NOT. ANY(testString,testStringArray),'ANY(testString,testStringArray), bad')
  ASSERT(.NOT. ANY(testStringArray,testString),'ANY(testStringArray,testString), bad')
  !ASSERT(.NOT. ANY(char10,testStringArray),'ANY(char10,testStringArray), bad')
  ASSERT(.NOT. ANY(testStringArray,char10),'ANY(testStringArray,char10), bad')
  !ASSERT(ANY(testString,testStringArray,.TRUE.),'ANY(testString,testStringArray,.TRUE.), bad')
  ASSERT(ANY(testStringArray,testString,.TRUE.),'ANY(testStringArray,testString,.TRUE.), bad')
  !ASSERT(ANY(char10,testStringArray,.TRUE.),'ANY(char10,testStringArray,.TRUE.), bad')
  ASSERT(ANY(testStringArray,char10,.TRUE.),'ANY(testStringArray,char10,.TRUE.), bad')
  !ASSERT(.NOT.ANY(testString,testStringArray,.FALSE.),'ANY(testString,testStringArray,.FALSE.), bad')
  ASSERT(.NOT.ANY(testStringArray,testString,.FALSE.),'ANY(testStringArray,testString,.FALSE.), bad')
  !ASSERT(.NOT.ANY(char10,testStringArray,.FALSE.),'ANY(char10,testStringArray,.FALSE.), bad')
  ASSERT(.NOT.ANY(testStringArray,char10,.FALSE.),'ANY(testStringArray,char10,.FALSE.), bad')
  testString='good test'
  testStringArray(10)='good test'
  char10='good test'
  !ASSERT(ANY(testString,testStringArray),'ANY(testString,testStringArray)')
  ASSERT(ANY(testStringArray,testString),'ANY(testStringArray,testString)')
  !ASSERT(ANY(char10,testStringArray),'ANY(char10,testStringArray)')
  ASSERT(ANY(testStringArray,char10),'ANY(testStringArray,char10)')
  !ASSERT(.NOT.ANY(testString,testStringArray,.TRUE.),'ANY(testString,testStringArray,.TRUE.)')
  ASSERT(.NOT.ANY(testStringArray,testString,.TRUE.),'ANY(testStringArray,testString,.TRUE.)')
  !ASSERT(.NOT.ANY(char10,testStringArray,.TRUE.),'ANY(char10,testStringArray,.TRUE.)')
  ASSERT(.NOT.ANY(testStringArray,char10,.TRUE.),'ANY(testStringArray,char10,.TRUE.)')
  !ASSERT(ANY(testString,testStringArray,.FALSE.),'ANY(testString,testStringArray,.FALSE.)')
  ASSERT(ANY(testStringArray,testString,.FALSE.),'ANY(testStringArray,testString,.FALSE.)')
  !ASSERT(ANY(char10,testStringArray,.FALSE.),'ANY(char10,testStringArray,.FALSE.)')
  ASSERT(ANY(testStringArray,char10,.FALSE.),'ANY(testStringArray,char10,.FALSE.)')
!
!Test ALL operator
  !Test ALL of 1-D array and scalar
  COMPONENT_TEST('ALL 1-D & Scalar')
  DO i=1,10
    testStringArray(i)='test'
  ENDDO
  testString='bad test'
  char10='bad test'
  !ASSERT(.NOT. ALL(testString,testStringArray),'ALL(testString,testStringArray), bad')
  ASSERT(.NOT. ALL(testStringArray,testString),'ALL 1-D & scalar, str')
  !ASSERT(.NOT. ALL(char10,testStringArray),'ALL(char10,testStringArray), bad')
  ASSERT(.NOT. ALL(testStringArray,char10),'ALL 1-D & scalar, char')
  !ASSERT(ALL(testString,testStringArray,.TRUE.),'ALL(testString,testStringArray,.TRUE.), bad')
  ASSERT(ALL(testStringArray,testString,.TRUE.),'ALL 1-D & scalar, str, w/ NOT=T')
  !ASSERT(ALL(char10,testStringArray,.TRUE.),'ALL(char10,testStringArray,.TRUE.), bad')
  ASSERT(ALL(testStringArray,char10,.TRUE.),'ALL 1-D & scalar, char, w/ NOT=T')
  !ASSERT(.NOT. ALL(testString,testStringArray,.FALSE.),'ALL(testString,testStringArray,.FALSE.), bad')
  ASSERT(.NOT. ALL(testStringArray,testString,.FALSE.),'ALL 1-D & scalar, str, w/ NOT=F')
  !ASSERT(.NOT. ALL(char10,testStringArray,.FALSE.),'ALL(char10,testStringArray,.FALSE.), bad')
  ASSERT(.NOT. ALL(testStringArray,char10,.FALSE.),'ALL 1-D & scalar, char, w/ NOT=F')
  testString='test'
  char10='test'
  !ASSERT(ALL(testString,testStringArray),'ALL(testString,testStringArray)')
  ASSERT(ALL(testStringArray,testString),'ALL 1-D & scalar, str')
  !ASSERT(ALL(char10,testStringArray),'ALL(char10,testStringArray)')
  ASSERT(ALL(testStringArray,char10),'ALL 1-D & scalar, char')
  !ASSERT(.NOT.ALL(testString,testStringArray,.TRUE.),'ALL(testString,testStringArray,.TRUE)')
  ASSERT(.NOT.ALL(testStringArray,testString,.TRUE.),'ALL 1-D & scalar, str, w/ NOT=T')
  !ASSERT(.NOT.ALL(char10,testStringArray,.TRUE.),'ALL(char10,testStringArray,.TRUE)')
  ASSERT(.NOT.ALL(testStringArray,char10,.TRUE.),'ALL 1-D & scalar, char, w/ NOT=T')
  !ASSERT(ALL(testString,testStringArray,.FALSE.),'ALL(testString,testStringArray,.FALSE.)')
  ASSERT(ALL(testStringArray,testString,.FALSE.),'ALL 1-D & scalar, str, w/ NOT=F')
  !ASSERT(ALL(char10,testStringArray,.FALSE.),'ALL(char10,testStringArray,.FALSE.)')
  ASSERT(ALL(testStringArray,char10,.FALSE.),'ALL 1-D & scalar, char, w/ NOT=F')

  COMPONENT_TEST('ALL 1-D & 1-D')
  !Test ALL of two 1-D arrays
  test1a(1)='test'; test1a(2)='test'
  test1a2(1)='test'; test1a2(2)='test'
  ASSERT(ALL(test1a,test1a2),'ALL 1-D & 1-D')
  ASSERT(.NOT.ALL(test1a,test1a2,.TRUE.),'ALL 1-D & 1-D, w/ NOT=T')
  ASSERT(ALL(test1a,test1a2,.FALSE.),'ALL 1-D & 1-D, w/ NOT=F')
  test1a2(2)='bad test'
  ASSERT(.NOT.ALL(test1a,test1a2),'ALL 1-D & 1-D')
  ASSERT(.NOT.ALL(test1a,test1a2,.TRUE.),'ALL 1-D & 1-D, w/ NOT=T')
  ASSERT(.NOT.ALL(test1a,test1a2,.FALSE.),'ALL 1-D & 1-D, w/ NOT=F')
  test1a2(1)='bad test'
  ASSERT(.NOT.ALL(test1a,test1a2),'ALL 1-D & 1-D')
  ASSERT(ALL(test1a,test1a2,.TRUE.),'ALL 1-D & 1-D, w/ NOT=T')
  ASSERT(.NOT.ALL(test1a,test1a2,.FALSE.),'ALL 1-D & 1-D, w/ NOT=F')

  COMPONENT_TEST('ALL 2-D & 2-D')
  !Test ALL of two 2-D arrays
  test2a(1,1)='test'; test2a(1,2)='test'
  test2a(2,1)='test'; test2a(2,2)='test'
  test2a2(1,1)='test'; test2a2(1,2)='test'
  test2a2(2,1)='test'; test2a2(2,2)='test'
  ASSERT(ALL(test2a,test2a2),'ALL 2-D & 2-D')
  ASSERT(.NOT.ALL(test2a,test2a2,.TRUE.),'ALL 2-D & 2-D, w/ NOT=T')
  ASSERT(ALL(test2a,test2a2,.FALSE.),'ALL 2-D & 2-D, w/ NOT=F')
  test2a2(2,2)='bad test'
  ASSERT(.NOT.ALL(test2a,test2a2),'ALL 2-D & 2-D')
  ASSERT(.NOT.ALL(test2a,test2a2,.TRUE.),'ALL 2-D & 2-D, w/ NOT=T')
  ASSERT(.NOT.ALL(test2a,test2a2,.FALSE.),'ALL 2-D & 2-D, w/ NOT=F')
  test2a2(1,1)='bad test'; test2a2(1,2)='bad test'; test2a2(2,1)='bad test'
  ASSERT(.NOT.ALL(test2a,test2a2),'ALL 2-D & 2-D')
  ASSERT(ALL(test2a,test2a2,.TRUE.),'ALL 2-D & 2-D, w/ NOT=T')
  ASSERT(.NOT.ALL(test2a,test2a2,.FALSE.),'ALL 2-D & 2-D, w/ NOT=F')

  COMPONENT_TEST('ALL 3-D & 3-D')
  !Test ALL of two 2-D arrays
  test3a(1,1,1)='test'; test3a(1,2,1)='test'
  test3a(2,1,1)='test'; test3a(2,2,1)='test'
  test3a(1,1,2)='test'; test3a(1,2,2)='test'
  test3a(2,1,2)='test'; test3a(2,2,2)='test'
  test3a2(1,1,1)='test'; test3a2(1,2,1)='test'
  test3a2(2,1,1)='test'; test3a2(2,2,1)='test'
  test3a2(1,1,2)='test'; test3a2(1,2,2)='test'
  test3a2(2,1,2)='test'; test3a2(2,2,2)='test'
  ASSERT(ALL(test3a,test3a2),'ALL 3-D & 3-D')
  ASSERT(.NOT.ALL(test3a,test3a2,.TRUE.),'ALL 3-D & 3-D, w/ NOT=T')
  ASSERT(ALL(test3a,test3a2,.FALSE.),'ALL 3-D & 3-D, w/ NOT=F')
  test3a2(2,2,2)='bad test'
  ASSERT(.NOT.ALL(test3a,test3a2),'ALL 3-D & 3-D')
  ASSERT(.NOT.ALL(test3a,test3a2,.TRUE.),'ALL 3-D & 3-D, w/ NOT=T')
  ASSERT(.NOT.ALL(test3a,test3a2,.FALSE.),'ALL 3-D & 3-D, w/ NOT=F')
  test3a2(1,1,1)='bad test'; test3a2(1,2,1)='bad test'; test3a2(2,1,1)='bad test'
  test3a2(2,2,1)='bad test'; test3a2(1,1,2)='bad test'; test3a2(1,2,2)='bad test'
  test3a2(2,1,2)='bad test'
  ASSERT(.NOT.ALL(test3a,test3a2),'ALL 3-D & 3-D')
  ASSERT(ALL(test3a,test3a2,.TRUE.),'ALL 3-D & 3-D, w/ NOT=T')
  ASSERT(.NOT.ALL(test3a,test3a2,.FALSE.),'ALL 3-D & 3-D, w/ NOT=F')

  FINALIZE_TEST()
!
ENDPROGRAM testStrings
