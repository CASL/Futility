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

  CREATE_TEST('TEST STRINGS')
  REGISTER_SUBTEST('Un-Iniitialized State',testUnInit)
  REGISTER_SUBTEST('Character Assignments',testAssign_chars)
  REGISTER_SUBTEST('Numberal Assignments',testAssign_nums)
  REGISTER_SUBTEST('Array Assignments',testAssign_arrays)
  REGISTER_SUBTEST('Intrinsics',testIntrinsic)
  REGISTER_SUBTEST('string_functs',testStrFunct)
  FINALIZE_TEST()
!
!-------------------------------------------------------------------------------
CONTAINS
!
!-------------------------------------------------------------------------------
!>
  SUBROUTINE testUnInit()
    TYPE(StringType) :: testString
    COMPONENT_TEST('Test Unitialized')
    !Test methods for uninitialized state
    ASSERT_EQ(LEN(testString),0,'LEN(testString) (uninit)')
    ASSERT_EQ(LEN_TRIM(testString),0,'LEN_TRIM(testString) (uninit)')
    ASSERT_EQ(CHAR(testString),'','CHAR(testString) (uninit)')
    ASSERT_EQ(TRIM(testString),'','TRIM(testString) (uninit)')
  ENDSUBROUTINE testUnInit
!
!-------------------------------------------------------------------------------
!>
  SUBROUTINE testAssign_chars()
    TYPE(StringType) :: testString,testString2
    CHARACTER(LEN=:),ALLOCATABLE :: char_str

    COMPONENT_TEST('Assignment Char to String Scalar')
    !Test assigning a zero length character to a string
    testString=""
    ASSERT_EQ(CHAR(testString),"",'CHAR(testString) (zero-length)')
    ASSERT_EQ(LEN(testString),0,'LEN(testString) (zero-length)')
    ASSERT_EQ(LEN_TRIM(testString),0,'LEN_TRIM(testString) (zero-length)')
    ASSERT_EQ(TRIM(testString),'','TRIM(testString) (zero-length)')

    !Test assigning a non-empty character to a string from a constant
    testString='constant'
    WRITE(*,*) CHAR(testString)
    ASSERT_EQ(CHAR(testString),'constant','CHAR(testString) (=''constant'')')
    ASSERT_EQ(testString%at(1),'c','CHAR(testString,1,1) (=''constant'')')
    ASSERT_EQ(testString%substr(2,3),'on','CHAR(testString,2,3) (=''constant'')')
    !ASSERT(testString%at(-1,100) == 'constant','CHAR(testString,-1,100) (=''constant'')')
    ASSERT_EQ(LEN(testString),8,'LEN(testString) (=''constant'')')
    ASSERT_EQ(LEN_TRIM(testString),8,'LEN_TRIM(testString) (=''constant'')')
    ASSERT_EQ(TRIM(testString),'constant','TRIM(testString) (=''constant'')')

    !Test assigning a non-empty character to a string from a variable
    ALLOCATE(CHARACTER(10) :: char_str)
    char_str='variable  '
    testString=char_str
    ASSERT_EQ(CHAR(testString),'variable  ','CHAR(testString) (=''variable'')')
    ASSERT_EQ(LEN(testString),10,'LEN(testString) (=''variable'')')
    ASSERT_EQ(LEN_TRIM(testString),8,'LEN_TRIM(testString) (=''variable'')')
    ASSERT_EQ(TRIM(testString),'variable','TRIM(testString) (=''variable'')')
    DEALLOCATE(char_str)

    !Test assigning a string to a character
    COMPONENT_TEST('Assignment String Scalar to Char')
    char_str=CHAR(testString)
    ASSERT_EQ(char_str,'variable','char_str')

    !Test assigning a string to a string
    COMPONENT_TEST('Assignment String Scalar to String Scalar')
    testString2='testString2'
    testString2=testString
    ASSERT_EQ(CHAR(testString2),'variable','variable')
  ENDSUBROUTINE testAssign_chars
!
!-------------------------------------------------------------------------------
!>
  SUBROUTINE testAssign_nums()
    TYPE(StringType) :: testString
    TYPE(StringType) :: testarray(2)
    INTEGER(SNK) :: testSNK,testSNKarray(2)
    INTEGER(SLK) :: testSLK,testSLKarray(2)
    REAL(SSK) :: testSSK,testSSKarray(2)
    REAL(SDK) :: testSDK,testSDKarray(2)

    !Test assigning a short integer to string
    COMPONENT_TEST('Assignment 32-bit Integer to String')
    testString=-HUGE(testSNK)
    ASSERT_EQ(CHAR(testString),'-2147483647','testSNK : lowest')
    testString=HUGE(testSNK)
    ASSERT_EQ(CHAR(testString),'2147483647','testSNK : highest')
    testSNKarray=(/0,5/)
    testarray=testSNKarray
    ASSERT_EQ(CHAR(testarray(1)),'0','testSNK : array(1)')
    ASSERT_EQ(CHAR(testarray(2)),'5','testSNK : array(2)')

    !Test assigning a long integer to string
    COMPONENT_TEST('Assignment 64-bit Integer to String')
    testString=-HUGE(testSLK)
    ASSERT_EQ(CHAR(testString),'-9223372036854775807','testSLK : lowest')
    testString=HUGE(testSLK)
    ASSERT_EQ(CHAR(testString),'9223372036854775807','testSLK : highest')
    testSLKarray=(/0,5/)
    testarray=testSLKarray
    ASSERT_EQ(CHAR(testarray(1)),'0','testSLK : array(1)')
    ASSERT_EQ(CHAR(testarray(2)),'5','testSLK : array(2)')

    !Test assigning a single real to string
    COMPONENT_TEST('Assignment 32-bit Real to String')
    testSSK=1.01_SSK
    testString=testSSK
    ASSERT_EQ(CHAR(testString),'1.01000E+00','testSSK : 1')
    testSSK=-testSSK
    testString=SQRT(testSSK)
    ASSERT_EQ(CHAR(testString),'NaN','testSSK : NaN')
    testString=-HUGE(testSSK)
    ASSERT_EQ(CHAR(testString),'-3.40282E+38','testSSK : lowest')
    testString=HUGE(testSSK)
    ASSERT_EQ(CHAR(testString),'3.40282E+38','testSSK : highest')
    testString=-TINY(testSSK)
    ASSERT_EQ(CHAR(testString),'-1.17549E-38','testSSK : smallest negative')
    testString=TINY(testSSK)
    ASSERT_EQ(CHAR(testString),'1.17549E-38','testSSK : smallest positive')
    testSSKarray=(/-2.0_SSK,50.0_SSK/)
    testarray=testSSKarray
    ASSERT_EQ(CHAR(testarray(1)),'-2.00000E+00','testSSK : array(1)')
    ASSERT_EQ(CHAR(testarray(2)),'5.00000E+01','testSSK : array(2)')

    !Test assigning a double real to string
    COMPONENT_TEST('Assignment 64-bit Real to String')
    testSDK=1.01_SDK
    testString=testSDK
    ASSERT_EQ(CHAR(testString),'1.010000000000000E+00','testSDK : 1.01')
    testSDK=-testSDK
    testString=SQRT(testSDK)
    ASSERT_EQ(CHAR(testString),'NaN','testString=testSDK : NaN')
    !Since HUGE produces a triple digit exponent, it seems the 'E' disappears when
    !writing.
    testString=-HUGE(testSDK)
    ASSERT_EQ(CHAR(testString),'-1.797693134862316+308','testSDK : lowest')
    testString=HUGE(testSDK)
    ASSERT_EQ(CHAR(testString),'1.797693134862316+308','testSDK : highest')
    testString=-TINY(testSDK)
    ASSERT_EQ(CHAR(testString),'-2.225073858507201-308','testSDK : smallest negative')
    testString=TINY(testSDK)
    ASSERT_EQ(CHAR(testString),'2.225073858507201-308','testSDK : smallest positive')
    testSDKarray=(/-2.0_SDK,50.0_SDK/)
    testarray=testSDKarray
    ASSERT_EQ(CHAR(testarray(1)),'-2.000000000000000E+00','testSDK : array(1)')
    ASSERT_EQ(CHAR(testarray(2)),'5.000000000000000E+01','testSDK : array(2)')
   
  ENDSUBROUTINE testAssign_nums
!
!-------------------------------------------------------------------------------
!>
  SUBROUTINE testAssign_arrays()
    TYPE(StringType),ALLOCATABLE :: s1a(:),s1a2(:),s2a(:,:),s2a2(:,:)
    TYPE(StringType) :: testString2
    !Test assigning a char to a 1-D string array
    COMPONENT_TEST('Assignment char to 1-D String Array')
    ALLOCATE(s1a(2))
    ! Load dummy values manually
    s1a(1)='test'
    s1a(2)='test2'
    ! Attempt to overwrite with check value
    s1a='pass'
    ASSERT_EQ(CHAR(s1a(1)),'pass','char to 1-D array')
    ASSERT_EQ(CHAR(s1a(2)),'pass','char to 1-D array')
    s1a=''
    ASSERT_EQ(CHAR(s1a(1)),'','char to 1-D array nullify')
    ASSERT_EQ(CHAR(s1a(2)),'','char to 1-D array nullify')
    DEALLOCATE(s1a)

    !Test assigning a char to a 2-D string array
    COMPONENT_TEST('Assignment char to 2-D String Array')
    ALLOCATE(s2a(2,2))
    ! Load dummy values manually
    s2a(1,1)='test'
    s2a(1,2)='test'
    s2a(2,1)='test2'
    s2a(2,2)='test2'
    ! Attempt to overwrite with check value
    s2a='pass'
    ASSERT_EQ(CHAR(s2a(1,1)),'pass',"char to 2-D array")
    ASSERT_EQ(CHAR(s2a(2,1)),'pass',"char to 2-D array")
    ASSERT_EQ(CHAR(s2a(1,1)),'pass',"char to 2-D array")
    ASSERT_EQ(CHAR(s2a(2,2)),'pass',"char to 2-D array")
    s2a=''
    ASSERT_EQ(CHAR(s2a(1,1)),'',"char to 2-D array")
    ASSERT_EQ(CHAR(s2a(2,1)),'',"char to 2-D array")
    ASSERT_EQ(CHAR(s2a(1,1)),'',"char to 2-D array")
    ASSERT_EQ(CHAR(s2a(2,2)),'',"char to 2-D array")
    DEALLOCATE(s2a)

    !Test assigning an array of strings to an array of strings
    COMPONENT_TEST('Assignment 1-D String Array to 1-D String Array')
    testString2='testString2'
    ALLOCATE(s1a(1))
    ALLOCATE(s1a2(1))
    s1a(1)='test'
    !assign array to allocated array
    s1a2(1)='test2'
    s1a=s1a2
    ASSERT(ALLOCATED(s1a),'allocated array assignment')
    ASSERT(SIZE(s1a,DIM=1) == 1,'SIZE array assignment')
    ASSERT(s1a(1)=='test2','test array assignment')
    !assign array to allocated array
    DEALLOCATE(s1a2)

    ALLOCATE(s1a2(2))
    s1a2(1)='one'
    s1a2(2)='two'
    s1a(:)=s1a2(1)
    ASSERT(ALLOCATED(s1a),'allocated array assignment')
    ASSERT_EQ(SIZE(s1a,DIM=1),1,'SIZE array assignment')
    ASSERT_EQ(CHAR(s1a(1)),'one','test array assignment')
    DEALLOCATE(s1a)
    DEALLOCATE(s1a2)

    !Test assigning an array of strings to an array of strings
    COMPONENT_TEST('Assignment 2-D String Array to 2-D String Array')
    testString2='testString2'
    ALLOCATE(s2a(2,2))
    ALLOCATE(s2a2(2,2))
    s2a='test'
    ASSERT_EQ(CHAR(s2a(1,1)),"test","rray")
    ASSERT_EQ(CHAR(s2a(2,1)),"test","rray")
    ASSERT_EQ(CHAR(s2a(1,1)),"test","rray")
    ASSERT_EQ(CHAR(s2a(2,2)),"test","rray")
    !assign array to allocated array
    s2a2='test2'
    s2a=s2a2
    ASSERT(ALLOCATED(s2a),'allocated array assignment')
    ASSERT(SIZE(s2a,DIM=1) == 2 .AND. SIZE(s2a,DIM=2) == 2,'SIZE array assignment')
    ASSERT_EQ(CHAR(s2a(1,1)),"test2","rray")
    ASSERT_EQ(CHAR(s2a(2,1)),"test2","rray")
    ASSERT_EQ(CHAR(s2a(1,1)),"test2","rray")
    ASSERT_EQ(CHAR(s2a(2,2)),"test2","rray")
    DEALLOCATE(s2a2)

    ALLOCATE(s2a2(2,3))
    s2a2='one'
    s2a=s2a2(:,1:2)
    ASSERT(ALLOCATED(s2a),'allocated array assignment')
    ASSERT(SIZE(s2a,DIM=1) == 2 .AND. SIZE(s2a,DIM=2) == 2,'SIZE array assignment')
    ASSERT_EQ(CHAR(s2a(1,1)),"one","rray")
    ASSERT_EQ(CHAR(s2a(2,1)),"one","rray")
    ASSERT_EQ(CHAR(s2a(1,1)),"one","rray")
    ASSERT_EQ(CHAR(s2a(2,2)),"one","rray")
    DEALLOCATE(s2a)
    DEALLOCATE(s2a2)
   
  ENDSUBROUTINE testAssign_arrays
!
!-------------------------------------------------------------------------------
!>
  SUBROUTINE testIntrinsic()
    TYPE(StringType) :: testString,testString2
    CHARACTER(LEN=10) :: char10
    INTEGER(SIK) :: i
    TYPE(StringType) :: testStringArray(10)
    TYPE(StringType) :: test1a(2),test1a2(2),test2a(2,2),test2a2(2,2)
    TYPE(StringType) :: test3a(2,2,2),test3a2(2,2,2)

    !Test ADJUSTL and ADJUSTR
    COMPONENT_TEST('ADJUSTL')
    char10=' - '
    testString=char10
    ASSERT(ADJUSTL(testString) == ADJUSTL(char10),'ADJUSTL(testString)')
    COMPONENT_TEST('ADJUSTR')
    char10=' - '
    testString=char10
    ASSERT(ADJUSTR(testString) == ADJUSTR(char10),'ADJUSTR(testString)')

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

    !Test // operator
    COMPONENT_TEST('//')
    char10='  -'
    testString=char10
    ASSERT('"'//testString == '"  -       ','''"''//testString')
    ASSERT(testString//'"' == '  -       "','testString//''"''')
    ASSERT(testString//testString == '  -         -       ','testString//testString')

    !Test == operator
    COMPONENT_TEST('==')
    testString2=ADJUSTL(char10)
    ASSERT(testString == char10,'testString == char10')
    ASSERT(.NOT. char10 == testString2,'char10 == testString2')
    ASSERT(.NOT. testString == testString2,'testString == testString2')

    !Test /= operator
    COMPONENT_TEST('/=')
    ASSERT(.NOT.(testString /= char10),'testString == char10')
    ASSERT(char10 /= testString2,'char10 /= testString2')
    ASSERT(testString /= testString2,'testString /= testString2')

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

    !Test replacing characters in a string
    testString="this_and_that"
    CALL testString%replace(1,4,"that")
    ASSERT_EQ(CHAR(testString),"that_and_that","replace this")
   
  ENDSUBROUTINE testIntrinsic
!
!-------------------------------------------------------------------------------
!>
  SUBROUTINE testStrFunct()
    TYPE(StringType) :: str
    TYPE(StringType), ALLOCATABLE :: str_list(:),ref_list(:)
    INTEGER(SIK) :: iWord
    ALLOCATE(ref_list(9))
    ref_list(1) = "The"
    ref_list(2) = "quick"
    ref_list(3) = "brown"
    ref_list(4) = "fox"
    ref_list(5) = "jumped"
    ref_list(6) = "over"
    ref_list(7) = "a"
    ref_list(8) = "lazy"
    ref_list(9) = "dog."

    str = "The quick brown fox jumped over a lazy dog."
    str_list = str%split()
    ASSERT_EQ(SIZE(str_list),9,"splits count")
    DO iWord=1,SIZE(str_list)
      ASSERT_EQ(CHAR(str_list(iWord)),CHAR(ref_list(iWord)),"splits")
    ENDDO
    str = "    The   quick   brown   fox   jumped    over a lazy   dog.   "
    str_list = str%split()
    DO iWord=1,SIZE(str_list)
      ASSERT_EQ(CHAR(str_list(iWord)),CHAR(ref_list(iWord)),"splits2")
    ENDDO
    
  ENDSUBROUTINE testStrFunct
!
ENDPROGRAM testStrings
