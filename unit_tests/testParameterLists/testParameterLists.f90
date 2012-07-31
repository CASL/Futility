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
  eParams => e
!
!Test SSK support
  val=5._SSK
  CALL testParam%init('testError->testSSK',val,'The number 5.0')
  eParams => NULL()
  CALL testParam%init('testSSK',val,'The number 5.0')
  testList(1)=testParam
  eParams => e
  CALL testParam%init('testError',val)
  eParams => NULL()
  CALL testParam%edit(OUTPUT_UNIT,2)
  CALL testParam%get('testSSK',someParam)
  CALL someParam%set('testSSK',3.0_SSK,'The number 3.0')
  CALL someParam%get('testSSK',val)
  CALL testParam%set('testSSK',val,'The number 5.0')
  eParams => e
  someParam=testParam !This produces an error
  CALL testParam%set('testSSK',testlist2)
  CALL someParam%set('testError',val)
  CALL someParam%get('testError',val)
  eParams => NULL()
  CALL testParam%get('testSSK',val)
  eParams => e
  CALL testParam%clear()
  CALL testParam%set('testError',3.0_SSK)
  CALL testParam%get('testError',val)
!
!Test ParamList support
  CALL testParam%init('testError->testPL',testList)
  eParams => NULL()
  CALL testParam%init('testPL',testList,'A test parameter list')
  testList(1)=testParam
  eParams => e
  CALL testParam%init('testError',testList,'A test parameter list')
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testList(1)%edit(OUTPUT_UNIT)
  CALL testParam%set('testPL',val)
  CALL testParam%get('testPL',val)
  eParams => NULL()
  CALL testParam%get('testPL',testList)
  eParams => e
  CALL testParam%get('testPL',testList2)
  CALL testParam%get('testPL->testSSK',testList2)
  CALL testParam%get('testPL',someParam)
  CALL someParam%get('testPL',testList2)
  CALL someParam%get('testSSK',testList2)
  
  CALL someParam%set('testError',testList2)
  CALL someParam%set('testPL',testList2,'Empty list')
  CALL testParam%set('testPL',testList2)
  eParams => NULL()
  CALL testParam%set('testPL',testList,'Original list')
  eParams => e
  CALL testParam%clear()
  CALL testParam%set('testError',testList)
  CALL testParam%get('testError',testList)
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING PARAMETERLISTS PASSED!'
  WRITE(*,*) '==================================================='
!
ENDPROGRAM testParameterLists
