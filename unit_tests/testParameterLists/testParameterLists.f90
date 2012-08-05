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
PROGRAM testParameterLists
  
  USE ISO_FORTRAN_ENV
  USE IntrType
  USE Strings
  USE ExceptionHandler
  USE ParameterLists
  IMPLICIT NONE
  
  REAL(SSK) :: val
  TYPE(ExceptionHandlerType),POINTER :: e
  
  TYPE(ParamType) :: testParam,testParam2,testList(5),testList2(3)
  CLASS(ParamType),POINTER :: someParam
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING PARAMETERLISTS...'
  WRITE(*,*) '==================================================='
  
  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  eParams => e
!
!Test SSK support
  ALLOCATE(testParam2%pdat)
  testParam2%pdat%name='testSSK'
  val=5._SSK
  !test init
  CALL testParam%init('testError->testSSK',val,'The number 5.0')
  eParams => NULL()
  CALL testParam%init('testSSK',val,'The number 5.0')
  IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
    WRITE(*,*) 'CALL testParam%init(...) %pdat (SSK) FAILED!'
    STOP 666
  ENDIF
  IF(testParam%pdat%name /= 'testSSK') THEN
    WRITE(*,*) 'CALL testParam%init(...) %name (SSK) FAILED!'
    STOP 666
  ENDIF
  IF(testParam%pdat%datatype /= 'REAL(SSK)') THEN
    WRITE(*,*) 'CALL testParam%init(...) %datatype (SSK) FAILED!'
    STOP 666
  ENDIF
  IF(testParam%pdat%description /= 'The number 5.0') THEN
    WRITE(*,*) 'CALL testParam%init(...) %description (SSK) FAILED!'
    STOP 666
  ENDIF
  CALL testParam%edit(OUTPUT_UNIT,0) !test edit
  eParams => e
  CALL testParam%init('testError',val)
  WRITE(*,*) '  Passed: CALL testParam%init(...) (SSK)'
  
  !test get
  eParams => NULL()
  CALL testParam%get('testSSK',someParam)
  IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
    WRITE(*,*) 'CALL testParam%get(''testSSK'',someParam) FAILED!'
    STOP 666
  ENDIF
  CALL someParam%get('testSSK',val)
  IF(val /= 5.0_SSK) THEN
    WRITE(*,*) 'CALL someParam%get(''testSSK'',val) FAILED!'
    STOP 666
  ENDIF
  val=0.0_SSK
  CALL testParam%get('testSSK',val)
  IF(val /= 5.0_SSK) THEN
    WRITE(*,*) 'CALL testParam%get(''testSSK'',val) FAILED!'
    STOP 666
  ENDIF
  eParams => e
  CALL testParam2%get('testSSK',val)
  CALL testParam%get('testError',val)
  CALL someParam%get('testError',val)
  WRITE(*,*) '  Passed: CALL testParam%get(...) (SSK)'
  
  !test set
  eParams => NULL()
  CALL someParam%set('testSSK',3.0_SSK,'The number 3.0')
  CALL testParam%get('testSSK',val)
  IF(val /= 3.0_SSK .OR. someParam%description /= 'The number 3.0') THEN
    WRITE(*,*) 'someParam%set(''testSSK'',3.0_SSK,''The number 3.0'') FAILED!'
    STOP 666
  ENDIF
  CALL testParam%set('testSSK',5.0_SSK,'The number 5.0')
  CALL testParam%get('testSSK',val)
  IF(val /= 5.0_SSK .OR. someParam%description /= 'The number 5.0') THEN
    WRITE(*,*) 'testParam%set(''testSSK'',5.0_SSK) FAILED!'
    STOP 666
  ENDIF
  eParams => e
  CALL testParam2%set('testSSK',val)
  CALL someParam%set('testError',val)
  CALL testParam%set('testError',val)
  WRITE(*,*) '  Passed: CALL testParam%set(...) (SSK)'
  
  !Test clear
  eParams => NULL()
  CALL testParam%clear()
  IF(LEN(testParam%name%sPrint()) /= 0) THEN
    WRITE(*,*) 'CALL testParam%clear() %name (SSK) FAILED!'
    STOP 666
  ENDIF
  IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
    WRITE(*,*) 'CALL testParam%clear() %datatype (SSK) FAILED!'
    STOP 666
  ENDIF
  IF(LEN(testParam%description%sPrint()) /= 0) THEN
    WRITE(*,*) 'CALL testParam%clear() %description (SSK) FAILED!'
    STOP 666
  ENDIF
  IF(ASSOCIATED(testParam%pdat)) THEN
    WRITE(*,*) 'CALL testParam%clear() %pdat (SSK) FAILED!'
    STOP 666
  ENDIF
  eParams => e
  WRITE(*,*) '  Passed: CALL testParam%clear() (SSK)'
  
  !test assignment
  eParams => NULL()
  CALL testParam%init('testSSK',4.0_SSK)
  testParam2=testparam
  IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
    WRITE(*,*) 'ASSIGNMENT(=) %pdat (SSK) FAILED!'
    STOP 666
  ENDIF
  IF(testParam2%pdat%name /= 'testSSK') THEN
    WRITE(*,*) 'ASSIGNMENT(=) %name (SSK) FAILED!'
    STOP 666
  ENDIF
  IF(testParam2%pdat%datatype /= 'REAL(SSK)') THEN
    WRITE(*,*) 'ASSIGNMENT(=) %datatype (SSK) FAILED!'
    STOP 666
  ENDIF
  eParams => e
  CALL testParam%get('testSSK',someParam)
  someParam=testParam
  WRITE(*,*) '  Passed: ASSIGNMENT(=) (SSK)'
!
!Test ParamList support
  testList(1)=testParam
  CALL testParam%clear()
  CALL testParam2%clear()
  ALLOCATE(testParam2%pdat)
  testParam2%pdat%name='testPL'
  
  !test init
  CALL testParam%init('testError->testPL',testList)
  eParams => NULL()
  CALL testParam%init('testPL',testList,'A test parameter list')
  IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
    WRITE(*,*) 'CALL testParam%init(...) %pdat (List) FAILED!'
    STOP 666
  ENDIF
  IF(testParam%pdat%name /= 'testPL') THEN
    WRITE(*,*) 'CALL testParam%init(...) %name (List) FAILED!'
    STOP 666
  ENDIF
  IF(testParam%pdat%datatype /= 'TYPE(ParamType_List)') THEN
    WRITE(*,*) 'CALL testParam%init(...) %datatype (List) FAILED!'
    STOP 666
  ENDIF
  IF(testParam%pdat%description /= 'A test parameter list') THEN
    WRITE(*,*) 'CALL testParam%init(...) %description (List) FAILED!'
    STOP 666
  ENDIF
  CALL testParam%edit(OUTPUT_UNIT,0) !test edit
  eParams => e
  CALL testParam%init('testError',testList,'A test parameter list')
  WRITE(*,*) '  Passed: CALL testParam%init(...) (List)'
  
  !Test get
  eParams => NULL()
  CALL testParam%get('testPL',someParam)
  CALL someParam%get('testPL',testList)
  IF(testList(1)%pdat%name /= 'testSSK') THEN
    WRITE(*,*) 'CALL someParam%get(''testPL'',testList) FAILED!'
    STOP 666
  ENDIF
  CALL testParam%get('testPL',testList)
  IF(testList(1)%pdat%name /= 'testSSK') THEN
    WRITE(*,*) 'CALL testParam%get(''testPL'',testList) FAILED!'
    STOP 666
  ENDIF
  eParams => e
  CALL someParam%get('testPL',testList2)
  CALL testParam%get('testPL',testList2)
  CALL testParam2%get('testPL',testList2)
  CALL testParam%get('testError',testList2)
  CALL testParam%get('->testError',testList2)
  CALL someParam%get('testError',testList2)
  WRITE(*,*) '  Passed: CALL testParam%get(...) (List)'
  
  !Test set
  !testList(2)=testList(1) GNU does not like this, seems to use the intrinsic assignment
  !                        rather than the overloaded assignment and produces a run
  !                        time error in memcopy instead of calling assign_ParamType.
  testParam2=testList(1)
  testList(2)=testParam2
  testList(2)%pdat%name='testSSK2'
  eParams => NULL()
  CALL someParam%set('testPL',testList,'A second list')
  val=0.0_SSK
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%get('testSSK2',val)
  IF(val /= 4.0_SSK .OR. someParam%description /= 'A second list') THEN
    WRITE(*,*) 'CALL someParam%set(''testPL'',testList,''A second list'') FAILED!'
    STOP 666
  ENDIF
  CALL testList(2)%clear()
  CALL testParam%set('testPL',testList,'A test parameter list')
  val=0.0_SSK
  eParams => e
  CALL testParam%get('testPL->testSSK2',val)
  IF(val /= 0.0_SSK .OR. someParam%description /= 'A test parameter list') THEN
    WRITE(*,*) 'CALL testParam%set(''testPL'',testList,''A test parameter list'') FAILED!'
    STOP 666
  ENDIF
  CALL testParam%set('testPL',testList2)
  CALL someParam%set('testPL',testList2)
  CALL testParam2%set('testSSK',testList)
  CALL someParam%set('testError',testList)
  CALL testParam%set('testError',testList)
  WRITE(*,*) '  Passed: CALL testParam%set(...) (List)'
  
  !test clear
  eParams => NULL()
  CALL testParam%clear()
  IF(LEN(testParam%name%sPrint()) /= 0) THEN
    WRITE(*,*) 'CALL testParam%clear() %name (List) FAILED!'
    STOP 666
  ENDIF
  IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
    WRITE(*,*) 'CALL testParam%clear() %datatype (List) FAILED!'
    STOP 666
  ENDIF
  IF(LEN(testParam%description%sPrint()) /= 0) THEN
    WRITE(*,*) 'CALL testParam%clear() %description (List) FAILED!'
    STOP 666
  ENDIF
  IF(ASSOCIATED(testParam%pdat)) THEN
    WRITE(*,*) 'CALL testParam%clear() %pdat (List) FAILED!'
    STOP 666
  ENDIF
  eParams => e
  WRITE(*,*) '  Passed: CALL testParam%clear() (List)'
  
  !Test assignment
  CALL testParam%init('testPL',testList)
  testParam2=testParam
  IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
    WRITE(*,*) 'ASSIGNMENT(=) %pdat (List) FAILED!'
    STOP 666
  ENDIF
  IF(testParam2%pdat%name /= 'testPL') THEN
    WRITE(*,*) 'ASSIGNMENT(=) %name (List) FAILED!'
    STOP 666
  ENDIF
  IF(testParam2%pdat%datatype /= 'TYPE(ParamType_List)') THEN
    WRITE(*,*) 'ASSIGNMENT(=) %datatype (List) FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: ASSIGNMENT(=) (List)'
  
  !Clear all variables
  CALL testParam%clear()
  CALL testParam2%clear()
  CALL testList(1)%clear()
  CALL testList(2)%clear()
  CALL testList(3)%clear()
  CALL testList(4)%clear()
  CALL testList(5)%clear()
  CALL testList2(1)%clear()
  CALL testList2(2)%clear()
  CALL testList2(3)%clear()
  
  !test add routines
  CALL e%setQuietMode(.FALSE.)
  eParams => NULL()
  CALL testParam%add('testSSK',6.0_SSK)
  CALL testParam%edit(OUTPUT_UNIT)
  eParams => e
  CALL testParam%add('testSSK',7.0_SSK)
  CALL testParam%add('testSSK2',7.0_SSK)
  CALL testParam%clear()
  CALL testParam%add('testPL->testSSK',7.0_SSK)
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%add('testPL->testSSK2',8.0_SSK)
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%add('testSSK',9.0_SSK)
  CALL testParam%add('testPL2->testSSK',9.0_SSK,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%add('testPL2->testSSK',9.0_SSK)
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%add('testPL2->testSSK2',-10.0e5_SSK)
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%add('testPL->testPL2->testSSK',11.0)
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%add('testPL->testPL2->testSSK3',11.0e6_SSK)
  CALL testParam%edit(OUTPUT_UNIT)
  eParams => NULL()
  CALL testParam2%add('testPL3->sublist1',testParam)
  CALL testParam2%edit(OUTPUT_UNIT)
  
  
  CALL testParam2%clear()
  CALL testParam2%add('testList->List1',testList)
  CALL testParam2%add('List2',testList2,'Empty list')
  CALL testParam2%edit(OUTPUT_UNIT)
  eParams => e
  CALL testParam2%add('List2',testList2,'Empty list')
  
  
  !test remove
  CALL testParam%edit(OUTPUT_UNIT)
  eParams => NULL()
  CALL testParam%remove('testSSK3')
  eParams => e
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%remove('->error')
  CALL testParam%remove('testSSK')
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%remove('testPL2->testSSK2')
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%remove('testPL2')
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%remove('testPL2')
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%remove('testPL->testSSK2')
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%remove('testPL->testSSK2')
  CALL testParam%remove('testPL2->testSSK2')
  
  !Clean-up variables
  CALL testParam2%clear()
  CALL testParam%clear()
  
  !Setup reference list
  CALL testParam2%add('TestReq->p1',0.0_SSK)
  CALL testParam2%add('TestReq->p2',0.1_SSK)
  CALL testParam2%add('TestReq->sublist1->p1',1.0_SSK)
  CALL testParam2%add('TestReq->sublist1->p3',1.1_SSK)
  CALL testParam2%add('TestReq->sublist1->sublist2->p2',2.0_SSK)
  CALL testParam2%add('TestReq->sublist1->sublist2->sublist3->null',-1.0_SSK)
  CALL testParam2%remove('TestReq->sublist1->sublist2->sublist3->null')
  CALL testParam2%add('TestReq->p4',0.2_SSK)
  CALL testParam2%edit(OUTPUT_UNIT)
  
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING PARAMETERLISTS PASSED!'
  WRITE(*,*) '==================================================='
!
ENDPROGRAM testParameterLists
