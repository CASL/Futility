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
USE ISO_C_BINDING
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

  COMPONENT_TEST('Test Clear Un-init')
  CALL testString%clear()
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
  COMPONENT_TEST('Test Clear Assigned string')
  CALL testString%clear()
  ASSERT_EQ(LEN(testString),0,'LEN(testString) (uninit)')
  ASSERT_EQ(LEN_TRIM(testString),0,'LEN_TRIM(testString) (uninit)')
  ASSERT_EQ(CHAR(testString),'','CHAR(testString) (uninit)')
  ASSERT_EQ(TRIM(testString),'','TRIM(testString) (uninit)')

  !Test assigning a non-empty character to a string
  testString='constant '
  ASSERT_EQ(CHAR(testString),'constant ','CHAR(testString) (="constant")')
  ASSERT_EQ(testString%at(1),'c','%at(1) return 1st character')
  ASSERT_EQ(testString%at(-1),'','%at(-1) return null string')
  ASSERT_EQ(testString%substr(2,3),'on','%substr(2,3) return 2nd and 3rd char')
  ASSERT_EQ(LEN(testString),9,'LEN(testString) (=''constant'')')
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
    CHARACTER(LEN=:),ALLOCATABLE :: char10
    CHARACTER(KIND=C_CHAR) :: cchar(5)
    TYPE(StringType) :: tstString
    TYPE(StringType) :: testStringArray(10)
    TYPE(StringType) :: test1a(2),test1a2(2),test2a(2,2),test2a2(2,2)
    TYPE(StringType) :: test3a(2,2,2),test3a2(2,2,2)

    !Test the CHAR interface for converting c_chars to StringType
    COMPONENT_TEST('cCHAR')
    cchar = (/"c","C","h","a","r"/)
    tstString = CHAR(cchar)
    ASSERT(CHAR(tstString) == "cChar","c character conversion to StringType")

    !Test ADJUSTL and ADJUSTR
    char10=' - '
    COMPONENT_TEST('ADJUSTL')
    testString=char10
    ASSERT(ADJUSTL(testString) == ADJUSTL(char10),'ADJUSTL(testString)')
    COMPONENT_TEST('ADJUSTR')
    testString=char10
    ASSERT(ADJUSTR(testString) == ADJUSTR(char10),'ADJUSTR(testString)')

    !Test INDEX
    char10='  - -'
    COMPONENT_TEST('INDEX')
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
    ASSERT(testString//'"' == '  -"','testString//''"''')
    ASSERT(testString//testString == '  -  -','testString//testString')

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
    testStringArray='test'
    testString='bad test'
    char10='bad test'
    ASSERT(.NOT. ANY(testStringArray==testString),'ANY(testStringArray,testString), bad')
    ASSERT(.NOT. ANY(testStringArray==char10),'ANY(testStringArray,char10), bad')
    testString='good test'
    testStringArray(10)='good test'
    char10='good test'
    ASSERT(ANY(testStringArray==testString),'ANY(testStringArray,testString)')
    ASSERT(ANY(testStringArray==char10),'ANY(testStringArray,char10)')

    !Test ALL operator
    !Test ALL of 1-D array and scalar
    COMPONENT_TEST('ALL 1-D & Scalar')
    testStringArray='test'
    testString='bad test'
    char10='bad test'
    ASSERT(.NOT. ALL(testStringArray==testString),'ALL 1-D & scalar, str')
    ASSERT(.NOT. ALL(testStringArray==char10),'ALL 1-D & scalar, char')
    testString='test'
    char10='test'
    ASSERT(ALL(testStringArray==testString),'ALL 1-D & scalar, str')
    ASSERT(ALL(testStringArray==char10),'ALL 1-D & scalar, char')

    COMPONENT_TEST('ALL 1-D & 1-D')
    !Test ALL of two 1-D arrays
    test1a(1)='test'; test1a(2)='test'
    test1a2(1)='test'; test1a2(2)='test'
    ASSERT(ALL(test1a==test1a2),'ALL 1-D & 1-D')
    test1a2(2)='bad test'
    ASSERT(.NOT.ALL(test1a==test1a2),'ALL 1-D & 1-D')
    test1a2(1)='bad test'
    ASSERT(.NOT.ALL(test1a==test1a2),'ALL 1-D & 1-D')

    COMPONENT_TEST('ALL 2-D & 2-D')
    !Test ALL of two 2-D arrays
    test2a(1,1)='test'; test2a(1,2)='test'
    test2a(2,1)='test'; test2a(2,2)='test'
    test2a2(1,1)='test'; test2a2(1,2)='test'
    test2a2(2,1)='test'; test2a2(2,2)='test'
    ASSERT(ALL(test2a==test2a2),'ALL 2-D & 2-D')
    test2a2(2,2)='bad test'
    ASSERT(.NOT.ALL(test2a==test2a2),'ALL 2-D & 2-D')
    test2a2(1,1)='bad test'; test2a2(1,2)='bad test'; test2a2(2,1)='bad test'
    ASSERT(.NOT.ALL(test2a==test2a2),'ALL 2-D & 2-D')

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
    ASSERT(ALL(test3a==test3a2),'ALL 3-D & 3-D')
    test3a2(2,2,2)='bad test'
    ASSERT(.NOT.ALL(test3a==test3a2),'ALL 3-D & 3-D')
    test3a2(1,1,1)='bad test'; test3a2(1,2,1)='bad test'; test3a2(2,1,1)='bad test'
    test3a2(2,2,1)='bad test'; test3a2(1,1,2)='bad test'; test3a2(1,2,2)='bad test'
    test3a2(2,1,2)='bad test'
    ASSERT(.NOT.ALL(test3a==test3a2),'ALL 3-D & 3-D')

  ENDSUBROUTINE testIntrinsic
!
!-------------------------------------------------------------------------------
!>
  SUBROUTINE testStrFunct()
    TYPE(StringType) :: str
    TYPE(StringType), ALLOCATABLE :: str_list(:),ref_list(:)
    INTEGER(SIK) :: iWord
    COMPONENT_TEST('String Split')
    str = ''
    str_list = str%split()
    ASSERT_EQ(SIZE(str_list),1,"splits count")
    ASSERT_EQ(CHAR(str_list(1)),'',"splits")
    !Deallocating here just to ensure the proper function of split
    DEALLOCATE(str_list)
    str = ''
    str_list = str%split(' ')
    ASSERT_EQ(SIZE(str_list),1,"splits count")
    ASSERT_EQ(CHAR(str_list(1)),'',"splits")
    str = "the"
    str_list = str%split()
    ASSERT_EQ(SIZE(str_list),1,"splits count")
    ASSERT_EQ(CHAR(str_list(1)),'the',"splits")

    str = "the"
    str_list = str%split('the')
    ASSERT_EQ(SIZE(str_list),1,"splits count")
    ASSERT_EQ(CHAR(str_list(1)),'',"splits")

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
    str_list = str%split(' ')
    ASSERT_EQ(SIZE(str_list),9,"splits count")
    DO iWord=1,SIZE(str_list)
      ASSERT_EQ(CHAR(str_list(iWord)),CHAR(ref_list(iWord)),"splits")
    ENDDO
    DEALLOCATE(str_list)
    str_list = str%split()
    ASSERT_EQ(SIZE(str_list),9,"splits count")
    DO iWord=1,SIZE(str_list)
      ASSERT_EQ(CHAR(str_list(iWord)),CHAR(ref_list(iWord)),"splits")
    ENDDO
    DEALLOCATE(str_list)

    str = "   The   quick   brown   fox   jumped   over   a   lazy   dog.   "
    str_list = str%split()
    ASSERT_EQ(SIZE(str_list),9,"splits count")
    DO iWord=1,SIZE(str_list)
      ASSERT_EQ(CHAR(str_list(iWord)),CHAR(ref_list(iWord)),"splits2")
    ENDDO
    DEALLOCATE(ref_list)
    ALLOCATE(ref_list(12))
    ref_list(1) = ""
    ref_list(2) = "The"
    ref_list(3) = "quick"
    ref_list(4) = "brown"
    ref_list(5) = "fox"
    ref_list(6) = "jumped"
    ref_list(7) = "over"
    ref_list(8) = "a"
    ref_list(9) = "lazy"
    ref_list(10) = ""
    ref_list(11) = ""
    ref_list(12) = "dog."
    str = " The quick brown fox jumped over a lazy   dog. "
    str_list = str%split(' ')
    ASSERT_EQ(SIZE(str_list),12,"splits count")
    DO iWord=1,SIZE(str_list)
      ASSERT_EQ(CHAR(str_list(iWord)),CHAR(ref_list(iWord)),"splits2")
    ENDDO
    DEALLOCATE(str_list)

    COMPONENT_TEST('%isInteger')
    str = ''
    ASSERT(.NOT.str%isInteger(),'empty')
    str = '13579'
    ASSERT(str%isInteger(),'13579')
    str = '03579'
    ASSERT(str%isInteger(),'with 0')
    str = '+13579'
    ASSERT(str%isInteger(),'with +')
    str = '-13579'
    ASSERT(str%isInteger(),'with -')
    str = 'e13579'
    ASSERT(.NOT.str%isInteger(),'with e')
    str = '$13579'
    ASSERT(.NOT.str%isInteger(),'with $')

    COMPONENT_TEST('%isFloat')
    str = ''
    ASSERT(.NOT.str%isFloat(),'empty')
    str = '0.2468'
    ASSERT(str%isFloat(),'0.2468')
    str = '+0.2468'
    ASSERT(str%isFloat(),'with +')
    str = '-0.2468'
    ASSERT(str%isFloat(),'with -')
    str = '-0.2468E000'
    ASSERT(str%isFloat(),'with e000')
    str = '-0.2468d000'
    ASSERT(str%isFloat(),'with e000')
    str = '-0.2468e000'
    ASSERT(str%isFloat(),'with e000')
    str = '-0.2468e+000'
    ASSERT(str%isFloat(),'with e+000')
    str = '-0.2468e-000'
    ASSERT(str%isFloat(),'with e-000')
    str = '-.2468e-000'
    ASSERT(.NOT.str%isFloat(),'no leading digit')
    str = '-0.2468-000'
    ASSERT(.NOT.str%isFloat(),'mixing exponent symbol')
    str = '-0.2a68000'
    ASSERT(.NOT.str%isFloat(),'with a')
    str = '-0.2a68e-000'
    ASSERT(.NOT.str%isFloat(),'with a and exponent')
    str = '-0.2468e-'
    ASSERT(.NOT.str%isFloat(),'missing exponent')
    str = '-0.2468e'
    ASSERT(.NOT.str%isFloat(),'missing exponent')

    COMPONENT_TEST('%isNumeric')
    str = ''
    ASSERT(.NOT.str%isNumeric(),'empty')
    str = '13579'
    ASSERT(str%isNumeric(),'13579')
    str = '03579'
    ASSERT(str%isNumeric(),'with 0')
    str = '+13579'
    ASSERT(str%isNumeric(),'with +')
    str = '-13579'
    ASSERT(str%isNumeric(),'with -')
    str = 'e13579'
    ASSERT(.NOT.str%isNumeric(),'with e')
    str = '$13579'
    ASSERT(.NOT.str%isNumeric(),'with $')
    str = '0.2468'
    ASSERT(str%isNumeric(),'0.2468')
    str = '+0.2468'
    ASSERT(str%isNumeric(),'with +')
    str = '-0.2468'
    ASSERT(str%isNumeric(),'with -')
    str = '-0.2468E000'
    ASSERT(str%isNumeric(),'with e000')
    str = '-0.2468d000'
    ASSERT(str%isNumeric(),'with e000')
    str = '-0.2468e000'
    ASSERT(str%isNumeric(),'with e000')
    str = '-0.2468e+000'
    ASSERT(str%isNumeric(),'with e+000')
    str = '-0.2468e-000'
    ASSERT(str%isNumeric(),'with e-000')
    str = '-.2468e-000'
    ASSERT(.NOT.str%isNumeric(),'no leading digit')
    str = '-0.2468-000'
    ASSERT(.NOT.str%isNumeric(),'mixing exponent symbol')
    str = '-0.2a68000'
    ASSERT(.NOT.str%isNumeric(),'with a')
    str = '-0.2a68e-000'
    ASSERT(.NOT.str%isNumeric(),'with a and exponent')
    str = '-0.2468e-'
    ASSERT(.NOT.str%isNumeric(),'missing exponent')
    str = '-0.2468e'
    ASSERT(.NOT.str%isNumeric(),'missing exponent')

    COMPONENT_TEST('%replace')
    !Test replacing characters in a string
    str="AABBAA"
    str = str%replace("A","B")
    ASSERT_EQ(CHAR(str),"BBBBBB","replace")
    str="AACAACAA"
    str = str%replace("A","Bb")
    ASSERT_EQ(CHAR(str),"BbBbCBbBbCBbBb","replace")
    str="AACAACAACC"
    str = str%replace("AA","c")
    ASSERT_EQ(CHAR(str),"cCcCcCC","replace")
    str="AACCAA"
    str = str%replace("AAA","c")
    ASSERT_EQ(CHAR(str),"AACCAA","replace")

    COMPONENT_TEST('%partition')
    str = ''
    str_list = str%partition('')
    ASSERT_EQ(SIZE(str_list),3,'null string / null delimiter')
    ASSERT(ALL(str_list == ''),'null string / null delimiter')
    str_list = str%rPartition('')
    ASSERT_EQ(SIZE(str_list),3,'null string / null delimiter')
    ASSERT(ALL(str_list == ''),'null string / null delimiter')

    str = 'abc'
    str_list = str%partition('')
    ASSERT_EQ(SIZE(str_list),3,'non-null string / null delimiter')
    ASSERT_EQ(CHAR(str_list(1)),'abc','non-null string / null delimiter')
    ASSERT_EQ(CHAR(str_list(2)),'','non-null string / null delimiter')
    ASSERT_EQ(CHAR(str_list(3)),'','non-null string / null delimiter')
    str_list = str%rPartition('')
    ASSERT_EQ(SIZE(str_list),3,'non-null string / null delimiter')
    ASSERT_EQ(CHAR(str_list(1)),'','non-null string / null delimiter')
    ASSERT_EQ(CHAR(str_list(2)),'','non-null string / null delimiter')
    ASSERT_EQ(CHAR(str_list(3)),'abc','non-null string / null delimiter')

    str = ''
    str_list = str%partition('---')
    ASSERT_EQ(SIZE(str_list),3,'null string / non-null delimiter')
    ASSERT_EQ(CHAR(str_list(1)),'','null string / non-null delimiter')
    ASSERT_EQ(CHAR(str_list(2)),'---','null string / non-null delimiter')
    ASSERT_EQ(CHAR(str_list(3)),'','null string / non-null delimiter')
    str_list = str%rPartition('---')
    ASSERT_EQ(SIZE(str_list),3,'null string / non-null delimiter')
    ASSERT_EQ(CHAR(str_list(1)),'','null string / non-null delimiter')
    ASSERT_EQ(CHAR(str_list(2)),'---','null string / non-null delimiter')
    ASSERT_EQ(CHAR(str_list(3)),'','null string / non-null delimiter')

    str = 'abc'
    str_list = str%partition('---')
    ASSERT_EQ(SIZE(str_list),3,'string, delimiter not found')
    ASSERT_EQ(CHAR(str_list(1)),'abc','string, delimiter not found')
    ASSERT_EQ(CHAR(str_list(2)),'---','string, delimiter not found')
    ASSERT_EQ(CHAR(str_list(3)),'','string, delimiter not found')
    str_list = str%rPartition('---')
    ASSERT_EQ(CHAR(str_list(1)),'','string, delimiter not found')
    ASSERT_EQ(CHAR(str_list(2)),'---','string, delimiter not found')
    ASSERT_EQ(CHAR(str_list(3)),'abc','string, delimiter not found')

    str = 'abc---def'
    str_list = str%partition('---')
    ASSERT_EQ(SIZE(str_list),3,'string with single delimter found')
    ASSERT_EQ(CHAR(str_list(1)),'abc','string with single delimter found')
    ASSERT_EQ(CHAR(str_list(2)),'---','string with single delimter found')
    ASSERT_EQ(CHAR(str_list(3)),'def','string with single delimter found')
    str_list = str%rPartition('---')
    ASSERT_EQ(SIZE(str_list),3,'string with single delimter found')
    ASSERT_EQ(CHAR(str_list(1)),'abc','string with single delimter found')
    ASSERT_EQ(CHAR(str_list(2)),'---','string with single delimter found')
    ASSERT_EQ(CHAR(str_list(3)),'def','string with single delimter found')

    str = 'abc---def---ghi'
    str_list = str%partition('---')
    ASSERT_EQ(SIZE(str_list),3,'string with multiple delimter found')
    ASSERT_EQ(CHAR(str_list(1)),'abc','string with multiple delimter found')
    ASSERT_EQ(CHAR(str_list(2)),'---','string with multiple delimter found')
    ASSERT_EQ(CHAR(str_list(3)),'def---ghi','string with multiple delimter found')
    str_list = str%rPartition('---')
    ASSERT_EQ(SIZE(str_list),3,'string with multiple delimter found')
    ASSERT_EQ(CHAR(str_list(1)),'abc---def','string with multiple delimter found')
    ASSERT_EQ(CHAR(str_list(2)),'---','string with multiple delimter found')
    ASSERT_EQ(CHAR(str_list(3)),'ghi','string with multiple delimter found')

  ENDSUBROUTINE testStrFunct
!
ENDPROGRAM testStrings
