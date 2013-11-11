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
PROGRAM testStrings
#include "UnitTest.h"
  USE UnitTest  
  USE IntrType
  USE Strings
  IMPLICIT NONE
  
  INTEGER(SIK) :: i
  CHARACTER(LEN=10) :: char10
  CHARACTER(LEN=20) :: char20
  TYPE(StringType) :: testString,testString2,testStringArray(10)
  
  CREATE_TEST('TEST STRINGS')
!
!Test methods for uninitialized state
  !length of an empty string
  COMPONENT_TEST('Assignment')
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
  char10='-'
  testString='testString1'
  char10=testString
  char20=testString
  ASSERT(char10 == 'testString','char10=testString')
  ASSERT(char20 == 'testString1         ','char20=testString')
!
!Test assigning a string to a string
  testString2='testString2'
  testString2=testString
  ASSERT(CHAR(testString2) == 'testString1','testString2=testString')
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
!Test ANY operator
  COMPONENT_TEST('ALL')
  DO i=1,10
    testStringArray(i)='test'
  ENDDO
  testString='bad test'
  char10='bad test'
  !ASSERT(.NOT. ALL(testString,testStringArray),'ALL(testString,testStringArray), bad')
  ASSERT(.NOT. ALL(testStringArray,testString),'ALL(testStringArray,testString), bad')
  !ASSERT(.NOT. ALL(char10,testStringArray),'ALL(char10,testStringArray), bad')
  ASSERT(.NOT. ALL(testStringArray,char10),'ALL(testStringArray,char10), bad')
  !ASSERT(ALL(testString,testStringArray,.TRUE.),'ALL(testString,testStringArray,.TRUE.), bad')
  ASSERT(ALL(testStringArray,testString,.TRUE.),'ALL(testStringArray,testString,.TRUE.), bad')
  !ASSERT(ALL(char10,testStringArray,.TRUE.),'ALL(char10,testStringArray,.TRUE.), bad')
  ASSERT(ALL(testStringArray,char10,.TRUE.),'ALL(testStringArray,char10,.TRUE.), bad')
  !ASSERT(.NOT. ALL(testString,testStringArray,.FALSE.),'ALL(testString,testStringArray,.FALSE.), bad')
  ASSERT(.NOT. ALL(testStringArray,testString,.FALSE.),'ALL(testStringArray,testString,.FALSE.), bad')
  !ASSERT(.NOT. ALL(char10,testStringArray,.FALSE.),'ALL(char10,testStringArray,.FALSE.), bad')
  ASSERT(.NOT. ALL(testStringArray,char10,.FALSE.),'ALL(testStringArray,char10,.FALSE.), bad')
  testString='test'
  char10='test'
  !ASSERT(ALL(testString,testStringArray),'ALL(testString,testStringArray)')
  ASSERT(ALL(testStringArray,testString),'ALL(testStringArray,testString)')
  !ASSERT(ALL(char10,testStringArray),'ALL(char10,testStringArray)')
  ASSERT(ALL(testStringArray,char10),'ALL(testStringArray,char10)')
  !ASSERT(.NOT.ALL(testString,testStringArray,.TRUE.),'ALL(testString,testStringArray,.TRUE)')
  ASSERT(.NOT.ALL(testStringArray,testString,.TRUE.),'ALL(testStringArray,testString,.TRUE)')
  !ASSERT(.NOT.ALL(char10,testStringArray,.TRUE.),'ALL(char10,testStringArray,.TRUE)')
  ASSERT(.NOT.ALL(testStringArray,char10,.TRUE.),'ALL(testStringArray,char10,.TRUE)')
  !ASSERT(ALL(testString,testStringArray,.FALSE.),'ALL(testString,testStringArray,.FALSE.)')
  ASSERT(ALL(testStringArray,testString,.FALSE.),'ALL(testStringArray,testString,.FALSE.)')
  !ASSERT(ALL(char10,testStringArray,.FALSE.),'ALL(char10,testStringArray,.FALSE.)')
  ASSERT(ALL(testStringArray,char10,.FALSE.),'ALL(testStringArray,char10,.FALSE.)')
  
  FINALIZE_TEST()
!
ENDPROGRAM testStrings
