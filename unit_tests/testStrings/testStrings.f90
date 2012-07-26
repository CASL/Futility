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
      
  USE Strings
  IMPLICIT NONE
  
  CHARACTER(LEN=10) :: char10
  CHARACTER(LEN=20) :: char20
  TYPE(StringType) :: testString
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING STRINGS...'
  WRITE(*,*) '==================================================='
!
!Test methods for uninitialized state
  !length of an empty string
  IF(LEN(testString) /= 0) THEN
    WRITE(*,*) 'LEN(testString) (uninit) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: LEN(testString) (uninit)'
  ENDIF
  IF(LEN_TRIM(testString) /= 0) THEN
    WRITE(*,*) 'LEN_TRIM(testString) (uninit) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: LEN_TRIM(testString) (uninit)'
  ENDIF
  IF(testString%sPrint() /= '') THEN
    WRITE(*,*) 'testString%sPrint() (uninit) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: testString%sPrint() (uninit)'
  ENDIF
  IF(TRIM(testString) /= '') THEN
    WRITE(*,*) 'TRIM(testString) (uninit) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: TRIM(testString) (uninit)'
  ENDIF
!
!Test assigning an empty character to a string
  char10=''
  testString=char10
  IF(testString%sPrint() /= char10) THEN
    WRITE(*,*) 'testString%sPrint() (empty) FAILED!'
    STOP 666
  ENDIF
  IF(LEN(testString) /= LEN(char10)) THEN
    WRITE(*,*) 'LEN(testString) (empty) FAILED!'
    STOP 666
  ENDIF
  IF(LEN_TRIM(testString) /= LEN_TRIM(char10)) THEN
    WRITE(*,*) 'LEN_TRIM(testString) (empty) FAILED!'
    STOP 666
  ENDIF
  IF(TRIM(testString) /= '') THEN
    WRITE(*,*) 'TRIM(testString) (uninit) FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: testString=''          '''
!
!Test assigning a zero length character to a string
  testString=TRIM(char10)
  IF(testString%sPrint() /= TRIM(char10)) THEN
    WRITE(*,*) 'testString%sPrint() (zero-length) FAILED!'
    STOP 666
  ENDIF
  IF(LEN(testString) /= 0) THEN
    WRITE(*,*) 'LEN(testString) (zero-length) FAILED!'
    STOP 666
  ENDIF
  IF(LEN_TRIM(testString) /= 0) THEN
    WRITE(*,*) 'LEN_TRIM(testString) (zero-length) FAILED!'
    STOP 666
  ENDIF
  IF(TRIM(testString) /= '') THEN
    WRITE(*,*) 'TRIM(testString) (uninit) FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: testString='''''
!
!Test assigning a non-empty character to a string from a constant
  testString='constant'
  IF(testString%sPrint() /= 'constant') THEN
    WRITE(*,*) 'testString%sPrint() (=''constant'') FAILED!'
    STOP 666
  ENDIF
  IF(LEN(testString) /= 8) THEN
    WRITE(*,*) 'LEN(testString) (=''constant'') FAILED!'
    STOP 666
  ENDIF
  IF(LEN_TRIM(testString) /= 8) THEN
    WRITE(*,*) 'LEN_TRIM(testString) (=''constant'') FAILED!'
    STOP 666
  ENDIF
  IF(TRIM(testString) /= 'constant') THEN
    WRITE(*,*) 'TRIM(testString) (=''constant'') FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: testString=''constant'''
!
!Test assigning a non-empty character to a string from a variable
  char10='variable'
  testString=char10
  IF(testString%sPrint() /= 'variable  ') THEN
    WRITE(*,*) 'testString%sPrint() (=variable) FAILED!'
    STOP 666
  ENDIF
  IF(LEN(testString) /= 10) THEN
    WRITE(*,*) 'LEN(testString) (=variable) FAILED!'
    STOP 666
  ENDIF
  IF(LEN_TRIM(testString) /= 8) THEN
    WRITE(*,*) 'LEN_TRIM(testString) (=variable) FAILED!'
    STOP 666
  ENDIF
  IF(TRIM(testString) /= 'variable') THEN
    WRITE(*,*) 'TRIM(testString) (=variable) FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: testString=variable'
!  
!Test assigning a string to a character
  char10='-'
  testString='testString1'
  char10=testString
  char20=testString
  IF(char10 /= 'testString') THEN
    WRITE(*,*) 'char10=testString FAILED!'
    STOP 666
  ENDIF
  IF(char20 /= 'testString1         ') THEN
    WRITE(*,*) 'char20=testString FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: variable=testString'
!
!Test ADJUSTL and ADJUSTR
  char10='  -'
  testString=char10
  IF(ADJUSTL(testString) /= ADJUSTL(char10)) THEN
    WRITE(*,*) 'ADJUSTL(testString) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: ADJUSTL(testString)'
  ENDIF
  IF(ADJUSTR(testString) /= ADJUSTR(char10)) THEN
    WRITE(*,*) 'ADJUSTR(testString) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: ADJUSTR(testString)'
  ENDIF
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING STRINGS PASSED!'
  WRITE(*,*) '==================================================='
!
ENDPROGRAM testStrings
