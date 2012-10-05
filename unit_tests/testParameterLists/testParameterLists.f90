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
  
  REAL(SSK) :: valssk
  REAL(SDK) :: valsdk
  INTEGER(SNK) :: valsnk
  INTEGER(SLK) :: valslk
  LOGICAL(SBK) :: valsbk
  TYPE(StringType) :: valstr
  REAL(SSK),ALLOCATABLE :: valssk1a(:)
  REAL(SDK),ALLOCATABLE :: valsdk1a(:)
  INTEGER(SNK),ALLOCATABLE :: valsnk1a(:)
  INTEGER(SLK),ALLOCATABLE :: valslk1a(:)
  LOGICAL(SBK),ALLOCATABLE :: valsbk1a(:)
  TYPE(StringType),ALLOCATABLE :: valstr1a(:)
  REAL(SSK),ALLOCATABLE :: valssk2a(:,:)
  REAL(SDK),ALLOCATABLE :: valsdk2a(:,:)
  INTEGER(SNK),ALLOCATABLE :: valsnk2a(:,:)
  INTEGER(SLK),ALLOCATABLE :: valslk2a(:,:)
  REAL(SSK),ALLOCATABLE :: valssk3a(:,:,:)
  REAL(SDK),ALLOCATABLE :: valsdk3a(:,:,:)
  INTEGER(SNK),ALLOCATABLE :: valsnk3a(:,:,:)
  INTEGER(SLK),ALLOCATABLE :: valslk3a(:,:,:)
  TYPE(ExceptionHandlerType),POINTER :: e
  
  TYPE(ParamType) :: testParam,testParam2,testParam3,testList(5),testList2(3)
  CLASS(ParamType),POINTER :: someParam
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING PARAMETERLISTS...'
  WRITE(*,*) '==================================================='
  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  eParams => e
  !Test the parameter type list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING  PARAMETER TYPE LISTS...'
  CALL testParamListType()
  !Test the scalar SSK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Scalar SSK PARAMETERLISTS...'
  CALL testSSK()
  !Test the scalar SDK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Scalar SDK PARAMETERLISTS...'
  CALL testSDK()
  !Test the scalar SNK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Scalar SNK PARAMETERLISTS...'
  CALL testSNK()
  !Test the scalar SLK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Scalar SLK PARAMETERLISTS...'
  CALL testSLK()
  !Test the scalar SBK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Scalar SBK PARAMETERLISTS...'
  CALL testSBK()
  !Test the scalar SBK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Scalar STR PARAMETERLISTS...'
  CALL testSTR()
  CALL testCHAR()
  !Test the 1-D array of SSK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING One-Dimensional Array SSK PARAMETERLISTS...'
  CALL testSSK1a()
  !Test the 1-D array of SDK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING One-Dimensional Array SDK PARAMETERLISTS...'
  CALL testSDK1a()
  !Test the 1-D array of SNK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING One-Dimensional Array SNK PARAMETERLISTS...'
  CALL testSNK1a()
  !Test the 1-D array of SLK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING One-Dimensional Array SLK PARAMETERLISTS...'
  CALL testSLK1a()
  !Test the 1-D array of SBK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING One-Dimensional Array SBK PARAMETERLISTS...'
  CALL testSBK1a()
  !Test the 1-D array of STR parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING One-Dimensional Array STR PARAMETERLISTS...'
  CALL testSTR1a()
  !Test the 2-D array of SSK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Two-Dimensional Array SSK PARAMETERLISTS...'
  CALL testSSK2a()
  !Test the 2-D array of SDK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Two-Dimensional Array SDK PARAMETERLISTS...'
  CALL testSDK2a()
  !Test the 2-D array of SNK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Two-Dimensional Array SNK PARAMETERLISTS...'
  CALL testSNK2a()
  !Test the 2-D array of SLK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Two-Dimensional Array SLK PARAMETERLISTS...'
  CALL testSLK2a()
  !Test the 3-D array of SSK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Two-Dimensional Array SSK PARAMETERLISTS...'
  CALL testSSK3a()
  !Test the 3-D array of SDK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Two-Dimensional Array SDK PARAMETERLISTS...'
  CALL testSDK3a()
  !Test the 3-D array of SNK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Two-Dimensional Array SNK PARAMETERLISTS...'
  CALL testSNK3a()
  !Test the 3-D array of SLK parameter list
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Two-Dimensional Array SLK PARAMETERLISTS...'
  CALL testSLK3a()
  
  CALL testClear()
  
  !Test add routines
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Add Routines...'
  
  CALL e%setQuietMode(.FALSE.)
  eParams => NULL()
  !Testing addition of SSK routine
  CALL testParam%add('testSSK',6.0_SSK)
  CALL testParam%edit(OUTPUT_UNIT)
  eParams => e
  !Testing addition of SSK routine, error for already existing parameter
  CALL testParam%add('testSSK',9.0_SSK)
  !Testing addition of SSK routine, error for not a parameter list
  CALL testParam%add('testSSK2',7.0_SSK)
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam%clear()
  !Testing addition of SSK routine to parameter list
  CALL testParam%add('testPL->testSSK',7.0_SSK)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of multiple SSK parameters to parameter list
  CALL testParam%add('testPL->testSSK2',8.0_SSK)
  CALL testParam%edit(OUTPUT_UNIT)
  eParams => NULL()
  !Testing addition of SDK routine to parameter list
  CALL testParam%add('testPL->testSDK',1.0_SDK)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of SNK routine to parameter list
  CALL testParam%add('testPL->testSNK',2_SNK)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of SLK routine to parameter list
  CALL testParam%add('testPL->testSLK',3_SLK)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of SBK routine to parameter list
  CALL testParam%add('testPL->testSBK',.TRUE.)
  CALL testParam%edit(OUTPUT_UNIT)
  valstr='string1'
  !Testing addition of STR routine to parameter list
  CALL testParam%add('testPL->testSTR',valstr)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SSK routine to parameter list
  IF(ALLOCATED(valssk1a)) DEALLOCATE(valssk1a)
  ALLOCATE(valssk1a(2))
  valssk1a=1.5_SSK
  CALL testParam%add('testPL->testSSK1a',valssk1a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SDK routine to parameter list
  IF(ALLOCATED(valsdk1a)) DEALLOCATE(valsdk1a)
  ALLOCATE(valsdk1a(2))
  valsdk1a=2.5_SDK
  CALL testParam%add('testPL->testSDK1a',valsdk1a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SNK routine to parameter list
  IF(ALLOCATED(valsnk1a)) DEALLOCATE(valsnk1a)
  ALLOCATE(valsnk1a(2))
  valsnk1a=-2_SNK
  CALL testParam%add('testPL->testSNK1a',valsnk1a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SLK routine to parameter list
  IF(ALLOCATED(valslk1a)) DEALLOCATE(valslk1a)
  ALLOCATE(valslk1a(2))
  valslk1a=-4_SLK
  CALL testParam%add('testPL->testSLK1a',valslk1a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SBK routine to parameter list
  IF(ALLOCATED(valsbk1a)) DEALLOCATE(valsbk1a)
  ALLOCATE(valsbk1a(2))
  valsbk1a=.TRUE.
  CALL testParam%add('testPL->testSBK1a',valsbk1a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array STR routine to parameter list
  IF(ALLOCATED(valstr1a)) DEALLOCATE(valstr1a)
  ALLOCATE(valstr1a(2))
  valstr1a(1)='stringarray1'
  valstr1a(2)='stringarray2'
  CALL testParam%add('testPL->testSTR1a',valstr1a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 2-D array SSK routine to parameter list
  IF(ALLOCATED(valssk2a)) DEALLOCATE(valssk2a)
  ALLOCATE(valssk2a(2,2))
  valssk2a=1.5_SSK
  CALL testParam%add('testPL->testSSK2a',valssk2a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 2-D array SDK routine to parameter list
  IF(ALLOCATED(valsdk2a)) DEALLOCATE(valsdk2a)
  ALLOCATE(valsdk2a(2,2))
  valsdk2a=2.5_SDK
  CALL testParam%add('testPL->testSDK2a',valsdk2a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 2-D array SNK routine to parameter list
  IF(ALLOCATED(valsnk2a)) DEALLOCATE(valsnk2a)
  ALLOCATE(valsnk2a(2,2))
  valsnk2a=-2_SNK
  CALL testParam%add('testPL->testSNK2a',valsnk2a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 2-D array SLK routine to parameter list
  IF(ALLOCATED(valslk2a)) DEALLOCATE(valslk2a)
  ALLOCATE(valslk2a(2,2))
  valslk2a=-4_SLK
  CALL testParam%add('testPL->testSLK2a',valslk2a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 3-D array SSK routine to parameter list
  IF(ALLOCATED(valssk3a)) DEALLOCATE(valssk3a)
  ALLOCATE(valssk3a(2,2,2))
  valssk3a=1.5_SSK
  CALL testParam%add('testPL->testSSK3a',valssk3a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 3-D array SDK routine to parameter list
  IF(ALLOCATED(valsdk3a)) DEALLOCATE(valsdk3a)
  ALLOCATE(valsdk3a(2,2,2))
  valsdk3a=2.5_SDK
  CALL testParam%add('testPL->testSDK3a',valsdk3a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 3-D array SNK routine to parameter list
  IF(ALLOCATED(valsnk3a)) DEALLOCATE(valsnk3a)
  ALLOCATE(valsnk3a(2,2,2))
  valsnk3a=-2_SNK
  CALL testParam%add('testPL->testSNK3a',valsnk3a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 3-D array SLK routine to parameter list
  IF(ALLOCATED(valslk3a)) DEALLOCATE(valslk3a)
  ALLOCATE(valslk3a(2,2,2))
  valslk3a=-4_SLK
  CALL testParam%add('testPL->testSLK3a',valslk3a)
  CALL testParam%edit(OUTPUT_UNIT)
  eParams => e
  !Testing addition of SSK routine to parameter list, error for already existing parameter
  CALL testParam%add('testPL->testSSK',7.0_SSK)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of SDK routine to parameter list, error for already existing parameter
  CALL testParam%add('testPL->testSDK',2.0_SDK)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of SNK routine to parameter list, error for already existing parameter
  CALL testParam%add('testPL->testSNK',3_SNK)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of SNK routine to parameter list, error for already existing parameter
  CALL testParam%add('testPL->testSLK',4_SLK)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of SBK routine to parameter list, error for already existing parameter
  CALL testParam%add('testPL->testSBK',.FALSE.)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of STR routine to parameter list, error for already existing parameter
  valstr='string2'
  CALL testParam%add('testPL->testSTR',valstr)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SSK routine to parameter list, error for already existing parameter
  valssk1a=10.5_SSK
  CALL testParam%add('testPL->testSSK1a',valssk1a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SDK routine to parameter list, error for already existing parameter
  valsdk1a=20.5_SDK
  CALL testParam%add('testPL->testSDK1a',valsdk1a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SNK routine to parameter list, error for already existing parameter
  valsnk1a=-6_SNK
  CALL testParam%add('testPL->testSNK1a',valsnk1a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SLK routine to parameter list, error for already existing parameter
  valslk1a=-60_SLK
  CALL testParam%add('testPL->testSLK1a',valslk1a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SBK routine to parameter list, error for already existing parameter
  valsbk1a=.FALSE.
  CALL testParam%add('testPL->testSBK1a',valsbk1a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array STR routine to parameter list, error for already existing parameter
  valstr1a(1)='another stringarray1'
  valstr1a(2)='another stringarray2'
  CALL testParam%add('testPL->testSTR1a',valstr1a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 2-D array SSK routine to parameter list, error for already existing parameter
  valssk2a=10.5_SSK
  CALL testParam%add('testPL->testSSK2a',valssk2a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 2-D array SDK routine to parameter list, error for already existing parameter
  valsdk2a=20.5_SDK
  CALL testParam%add('testPL->testSDK2a',valsdk2a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 2-D array SNK routine to parameter list, error for already existing parameter
  valsnk2a=-6_SNK
  CALL testParam%add('testPL->testSNK2a',valsnk2a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 2-D array SLK routine to parameter list, error for already existing parameter
  valslk2a=-60_SLK
  CALL testParam%add('testPL->testSLK2a',valslk2a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 3-D array SSK routine to parameter list, error for already existing parameter
  valssk3a=10.5_SSK
  CALL testParam%add('testPL->testSSK3a',valssk3a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 3-D array SDK routine to parameter list, error for already existing parameter
  valsdk3a=20.5_SDK
  CALL testParam%add('testPL->testSDK3a',valsdk3a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 3-D array SNK routine to parameter list, error for already existing parameter
  valsnk3a=-6_SNK
  CALL testParam%add('testPL->testSNK3a',valsnk3a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 3-D array SLK routine to parameter list, error for already existing parameter
  valslk3a=-60_SLK
  CALL testParam%add('testPL->testSLK3a',valslk3a)
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of SSK routine to parameter list with description
  CALL testParam%add('testPL2->testSSK',9.0_SSK,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of SSK routine to parameter list with description, error for already existing parameter
  CALL testParam%add('testPL2->testSSK',9.0_SSK,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of SDK routine to parameter list with description
  CALL testParam%add('testPL2->testSDK',2.0_SDK,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of SNK routine to parameter list with description
  CALL testParam%add('testPL2->testSNK',3_SNK,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of SLK routine to parameter list with description
  CALL testParam%add('testPL2->testSLK',4_SLK,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of SBK routine to parameter list with description
  CALL testParam%add('testPL2->testSBK',.FALSE.,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of STR routine to parameter list with description
  valstr='string3'
  CALL testParam%add('testPL2->testSTR',valstr,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SSK routine to parameter list with description
  valssk1a=2.5_SSK
  CALL testParam%add('testPL2->testSSK1a',valssk1a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SDK routine to parameter list with description
  valsdk1a=4.5_SDK
  CALL testParam%add('testPL2->testSDK1a',valsdk1a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SNK routine to parameter list with description
  valsnk1a=123_SNK
  CALL testParam%add('testPL2->testSNK1a',valsnk1a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SLK routine to parameter list with description
  valslk1a=-1230_SLK
  CALL testParam%add('testPL2->testSLK1a',valslk1a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array SBK routine to parameter list with description
  valsbk1a=.FALSE.
  CALL testParam%add('testPL2->testSBK1a',valsbk1a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 1-D array STR routine to parameter list with description
  valstr1a(1)='yet another stringarray1'
  valstr1a(2)='yet another stringarray2'
  CALL testParam%add('testPL2->testSTR1a',valstr1a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 2-D array SSK routine to parameter list with description
  valssk2a=2.5_SSK
  CALL testParam%add('testPL2->testSSK2a',valssk2a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 2-D array SDK routine to parameter list with description
  valsdk2a=4.5_SDK
  CALL testParam%add('testPL2->testSDK2a',valsdk2a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 2-D array SNK routine to parameter list with description
  valsnk2a=123_SNK
  CALL testParam%add('testPL2->testSNK2a',valsnk2a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 2-D array SLK routine to parameter list with description
  valslk2a=-1230_SLK
  CALL testParam%add('testPL2->testSLK2a',valslk2a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 3-D array SSK routine to parameter list with description
  valssk3a=2.5_SSK
  CALL testParam%add('testPL2->testSSK3a',valssk3a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 3-D array SDK routine to parameter list with description
  valsdk3a=4.5_SDK
  CALL testParam%add('testPL2->testSDK3a',valsdk3a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 3-D array SNK routine to parameter list with description
  valsnk3a=123_SNK
  CALL testParam%add('testPL2->testSNK3a',valsnk3a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing addition of 3-D array SLK routine to parameter list with description
  valslk3a=-1230_SLK
  CALL testParam%add('testPL2->testSLK3a',valslk3a,'Creates a new sublist')
  CALL testParam%edit(OUTPUT_UNIT)
  eParams => NULL()
  !Testing the local exception handler while adding a parameter to another parameter
  CALL testParam2%add('testPL3->sublist1',testParam)
  CALL testParam2%edit(OUTPUT_UNIT)
  
  !Testing the clear routine
  CALL testParam2%clear()
  !Testing adding an empty parameter list
  CALL testParam2%add('testList->List1',testList)
  !Testing adding an empty parameter list with a description
  CALL testParam2%add('List2',testList2,'Empty list')
  CALL testParam2%edit(OUTPUT_UNIT)
  eParams => e
  !Testing adding an empty parameter list with a description with exception handler
  CALL testParam2%add('List2',testList2,'Empty list')
  !Clearing the allocated variables
  DEALLOCATE(valssk1a)
  DEALLOCATE(valsdk1a)
  DEALLOCATE(valsnk1a)
  DEALLOCATE(valslk1a)
  DEALLOCATE(valsbk1a)
  DEALLOCATE(valstr1a)
  DEALLOCATE(valssk2a)
  DEALLOCATE(valsdk2a)
  DEALLOCATE(valsnk2a)
  DEALLOCATE(valslk2a)
  DEALLOCATE(valssk3a)
  DEALLOCATE(valsdk3a)
  DEALLOCATE(valsnk3a)
  DEALLOCATE(valslk3a)
  
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING Remove Routines...'
  !test remove
  CALL testParam%edit(OUTPUT_UNIT)
  eParams => NULL()
  !Testing removal of parameter that doesn't exist
  CALL testParam%remove('testSSK3')
  eParams => e
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing removal of parameter that is erroneous, need to have a value before '->'
  CALL testParam%remove('->error')
  !Testing the SSK removal routine
  CALL testParam%remove('testSSK')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the SSK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSSK2')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the SDK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSDK')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the SNK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSNK')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the SLK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSLK')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the SBK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSBK')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the STR removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSTR')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 1-D array SSK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSSK1a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 1-D array SDK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSDK1a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 1-D array SNK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSNK1a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 1-D array SLK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSLK1a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 1-D array SBK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSBK1a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 1-D array STR removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSTR1a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 2-D array SSK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSSK2a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 2-D array SDK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSDK2a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 2-D array SNK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSNK2a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 2-D array SLK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSLK2a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 3-D array SSK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSSK3a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 3-D array SDK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSDK3a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 3-D array SNK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSNK3a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 3-D array SLK removal routine in a parameter list (with description)
  CALL testParam%remove('testPL2->testSLK3a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the parameter list removal routine
  CALL testParam%remove('testPL2')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the SSK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSSK2')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the SDK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSDK')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the SNK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSNK')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the SLK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSLK')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the SBK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSBK')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the STR removal routine in a parameter list 
  CALL testParam%remove('testPL->testSTR')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 1-D array SSK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSSK1a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 1-D array SDK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSDK1a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 1-D array SNK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSNK1a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 1-D array SLK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSLK1a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 1-D array SBK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSBK1a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 1-D array STR removal routine in a parameter list 
  CALL testParam%remove('testPL->testSTR1a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 2-D array SSK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSSK2a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 2-D array SDK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSDK2a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 2-D array SNK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSNK2a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 2-D array SLK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSLK2a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 3-D array SSK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSSK3a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 3-D array SDK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSDK3a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 3-D array SNK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSNK3a')
  CALL testParam%edit(OUTPUT_UNIT)
  !Testing the 3-D array SLK removal routine in a parameter list 
  CALL testParam%remove('testPL->testSLK3a')
  CALL testParam%edit(OUTPUT_UNIT)
  
  !Clean-up variables
  CALL testParam2%clear()
  CALL testParam%clear()
  
  !Setup reference list and test validation subroutines
  CALL testParam%add('TestReq->p1',0.0_SSK)
  !test the null case for both params.
  CALL testParam%validate(testParam2,testParam3)
  !Testing the assignment operation
  testParam3=testParam
  eParams => NULL()
  !test the null required param, but existing optional param
  CALL testParam%validate(testParam2,testParam3)
  eParams => e
  testParam2=testParam
  CALL testParam3%clear()
  !test existing required params with null optional params
  CALL testParam%validate(testParam2,testParam3)
  CALL testParam2%add('TestReq->p2',0.1_SSK)
  !test only required params, no optional params
  CALL testParam%validate(testParam2)
  !Clear the test param, and test against an existing req param list
  CALL testParam%clear()
  CALL testParam%validate(testParam2)
  !Setting the test param to have all the required params
  !This allows for optional params to be tested
  testParam=testParam2
  !Adding an extra parameter to the test list
  CALL testParam%add('TestReq->sublist1->p3',1.1_SSK)
  CALL testParam%validate(testParam2,testParam3)
  !Adding the same extra parameter to the optional list
  CALL testParam3%add('TestReq->sublist1->p3',1.1_SSK)
  CALL testParam%validate(testParam2,testParam3)
  !Making sure the req param has all the test param data
  testParam2=testParam
  !Clearing the options for now
  CALL testParam3%clear()
  CALL testParam%clear()
  CALL testParam2%add('TestReq->sublist1->sublist2->sublist3->null',-1.0_SSK)
  !Making sure the test param has all the required params
  testParam=testParam2
  !Valid Req parameters, covering the line for checkExtras_Paramtype w/o optional params (line 1538)
  CALL testParam%validate(testParam2)
  !Valid Req parameters, covering different parameter type for optional input (lines 1424-1431)
  CALL testParam%add('TestReq->sublist1->sublist2->sublist3->opt3',5.0_SDK)
  CALL testParam%validate(testParam2,testParam3)
  CALL testParam3%add('TestReq->sublist1->sublist2->sublist3->opt3',5.0_SSK)
  CALL testParam%validate(testParam2,testParam3)
  !Finds a meaningful REQ parameter, returns an associated parameter of a different type (lines 1337-1340)
  CALL testParam%add('TestReq->p6',6.0_SSK)
  CALL testParam2%add('TestReq->p6',6_SNK)
  CALL testParam%validate(testParam2,testParam3)
  !
  CALL testClear()
  !Test param 2 is the required values, test param 3 is the optional
  !Test param is the list being checked.
  !Checks all the extras...
  CALL eParams%SetQuietMode(.TRUE.)
  CALL testParam2%add('TestReq->p1',0.2_SSK)
  CALL testParam3%add('TestOpt->p1',.TRUE.)
  CALL testParam%add('TestOther->p1',.TRUE.)
  CALL testParam%remove('TestOther')
  CALL testParam2%add('TestReq->p2->2far->veryfar',7._SDK)
  CALL testParam2%remove('TestReq->p2->2far->veryfar')
  !Test param for an existing parameter list, but the pointer isn't associated (lines 1313-1315)
  CALL testParam%validate(testParam2,testParam3)
  !Since an empty req param list exists, add a SLK parameter to test param 
  !so they aren't the same type  (lines 1319-1323)
  CALL testParam%add('TestReq->p2->2far',6_SLK)
  CALL testParam%validate(testParam2,testParam3)
  !Remove the extra required parameters and add TestRq->p1 so the validate req params is true
  CALL testParam2%remove('TestReq->p2')
  CALL testParam%add('TestReq->p1',0.2_SSK)
  !Setting up an optional param that has a parameter list but no associated pointer
  CALL testParam3%add('TestOpt->p2->2far->veryfar->veryveryfar',3.0_SDK)
  CALL testParam3%remove('TestOpt->p2->2far->veryfar->veryveryfar')
  WRITE(*,*) '--------------------------------------------------'
  CALL testParam%remove('TestOpt->p2->2far->veryfar')
  CALL testParam%add('TestOpt->p2->2far->veryfar',3.0_SSK)
  WRITE(*,*) '--------------------------------------------------'
  !Testing an optional param list against a parameter type (lines 1400-1410)
  CALL testParam%validate(testParam2,testParam3)
  WRITE(*,*) '--------------------------------------------------'
  CALL testParam%remove('TestOpt->p2->2far->veryfar')
  WRITE(*,*) '--------------------------------------------------'
  !Testing an unassociated test param from the remove statement above (lines 1388-1396)
  CALL testParam%validate(testParam2,testParam3)
  WRITE(*,*) '--------------------------------------------------'
  
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING PARAMETERLISTS PASSED!'
  WRITE(*,*) '==================================================='  
  CALL testClear()
  DEALLOCATE(e)
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!Test SSK support
  SUBROUTINE testSSK()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSSK'
    valssk=5._SSK
    !test init
    CALL testParam%init('testError->testSSK',valssk,'The number 5.0')
    eParams => NULL()
    CALL testParam%init('testSSK',valssk,'The number 5.0')
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
    CALL testParam%init('testError',valssk)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SSK)'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSSK',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSSK'',someParam) FAILED!'
      STOP 666
    ENDIF
    CALL someParam%get('testSSK',valssk)
    IF(valssk /= 5.0_SSK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSSK'',valssk) FAILED!'
      STOP 666
    ENDIF
    valssk=0.0_SSK
    CALL testParam%get('testSSK',valssk)
    IF(valssk /= 5.0_SSK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSSK'',valssk) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSSK',valssk)
    CALL testParam%get('testError',valssk)
    CALL someParam%get('testError',valssk)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SSK)'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSSK',3.0_SSK,'The number 3.0')
    CALL testParam%get('testSSK',valssk)
    IF(valssk /= 3.0_SSK .OR. someParam%description /= 'The number 3.0') THEN
      WRITE(*,*) 'someParam%set(''testSSK'',3.0_SSK,''The number 3.0'') FAILED!'
      STOP 666
    ENDIF
    CALL testParam%set('testSSK',5.0_SSK,'The number 5.0')
    CALL testParam%get('testSSK',valssk)
    IF(valssk /= 5.0_SSK .OR. someParam%description /= 'The number 5.0') THEN
      WRITE(*,*) 'testParam%set(''testSSK'',5.0_SSK) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%set('testSSK',valssk)
    CALL someParam%set('testError',valssk)
    CALL testParam%set('testError',valssk)
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
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSSK
!
!-------------------------------------------------------------------------------
!Test ParamList support
  SUBROUTINE testParamListType()
    !Carry over from testSSK
    CALL testParam%init('testSSK',4.0_SSK)
    CALL testParam%get('testSSK',someParam)
    someParam=testParam
    !
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
    valssk=0.0_SSK
    CALL testParam%edit(OUTPUT_UNIT)
    CALL testParam%get('testSSK2',valssk)
    IF(valssk /= 4.0_SSK .OR. someParam%description /= 'A second list') THEN
      WRITE(*,*) 'CALL someParam%set(''testPL'',testList,''A second list'') FAILED!'
      STOP 666
    ENDIF
    CALL testList(2)%clear()
    CALL testParam%set('testPL',testList,'A test parameter list')
    valssk=0.0_SSK
    eParams => e
    CALL testParam%get('testPL->testSSK2',valssk)
    IF(valssk /= 0.0_SSK .OR. someParam%description /= 'A test parameter list') THEN
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
  
    CALL testClear()
  ENDSUBROUTINE testParamListType
!
!-------------------------------------------------------------------------------
!Clear all the test variables
  SUBROUTINE testClear()
    
    CALL testParam%clear()
    CALL testParam2%clear()
    CALL testParam3%clear()
    CALL testList(1)%clear()
    CALL testList(2)%clear()
    CALL testList(3)%clear()
    CALL testList(4)%clear()
    CALL testList(5)%clear()
    CALL testList2(1)%clear()
    CALL testList2(2)%clear()
    CALL testList2(3)%clear()
  
  ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
!Test SDK support
  SUBROUTINE testSDK()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSDK'
    valsdk=5._SDK
    !test init
    CALL testParam%init('testError->testSDK',valsdk,'The number 5.0')
    eParams => NULL()
    CALL testParam%init('testSDK',valsdk,'The number 5.0')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SDK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSDK') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SDK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= 'REAL(SDK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SDK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The number 5.0') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SDK) FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valsdk)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SDK)'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSDK',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSDK'',someParam) FAILED!'
      STOP 666
    ENDIF
    CALL someParam%get('testSDK',valsdk)
    IF(valsdk /= 5.0_SDK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSDK'',valsdk) FAILED!'
      STOP 666
    ENDIF
    valsdk=0.0_SDK
    CALL testParam%get('testSDK',valsdk)
    IF(valsdk /= 5.0_SDK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSDK'',valsdk) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSDK',valsdk)
    CALL testParam%get('testError',valsdk)
    CALL someParam%get('testError',valsdk)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SDK)'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSDK',3.0_SDK,'The number 3.0')
    CALL testParam%get('testSDK',valsdk)
    IF(valsdk /= 3.0_SDK .OR. someParam%description /= 'The number 3.0') THEN
      WRITE(*,*) 'someParam%set(''testSDK'',3.0_SDK,''The number 3.0'') FAILED!'
      STOP 666
    ENDIF
    CALL testParam%set('testSDK',5.0_SDK,'The number 5.0')
    CALL testParam%get('testSDK',valsdk)
    IF(valsdk /= 5.0_SDK .OR. someParam%description /= 'The number 5.0') THEN
      WRITE(*,*) 'testParam%set(''testSDK'',5.0_SDK) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%set('testSDK',valsdk)
    CALL someParam%set('testError',valsdk)
    CALL testParam%set('testError',valsdk)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SDK)'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SDK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SDK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SDK) FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SDK) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SDK)'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSDK',4.0_SDK)
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SDK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSDK') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SDK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= 'REAL(SDK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SDK) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSDK',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SDK)'
    !Clear the variables for the next call
    CALL testClear()
    
  ENDSUBROUTINE testSDK
!
!-------------------------------------------------------------------------------
!Test SNK support
  SUBROUTINE testSNK()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSNK'
    valsnk=5_SNK
    !test init
    CALL testParam%init('testError->testSNK',valsnk,'The number 5')
    eParams => NULL()
    CALL testParam%init('testSNK',valsnk,'The number 5')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SNK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSNK') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SNK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= 'INTEGER(SNK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SNK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The number 5') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SNK) FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valsnk)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SNK)'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSNK',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSNK'',someParam) FAILED!'
      STOP 666
    ENDIF
    CALL someParam%get('testSNK',valsnk)
    IF(valsnk /= 5_SNK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSNK'',valsnk) FAILED!'
      STOP 666
    ENDIF
    valsnk=0_SNK
    CALL testParam%get('testSNK',valsnk)
    IF(valsnk /= 5_SNK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSNK'',valsnk) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSNK',valsnk)
    CALL testParam%get('testError',valsnk)
    CALL someParam%get('testError',valsnk)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SNK)'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSNK',3_SNK,'The number 3')
    CALL testParam%get('testSNK',valsnk)
    IF(valsnk /= 3_SNK .OR. someParam%description /= 'The number 3') THEN
      WRITE(*,*) 'someParam%set(''testSNK'',3_SNK,''The number 3'') FAILED!'
      STOP 666
    ENDIF
    CALL testParam%set('testSNK',5_SNK,'The number 5')
    CALL testParam%get('testSNK',valsnk)
    IF(valsnk /= 5_SNK .OR. someParam%description /= 'The number 5') THEN
      WRITE(*,*) 'testParam%set(''testSNK'',5_SNK) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%set('testSNK',valsnk)
    CALL someParam%set('testError',valsnk)
    CALL testParam%set('testError',valsnk)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SNK)'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SNK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SNK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SNK) FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SNK) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SNK)'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSNK',4_SNK)
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SNK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSNK') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SNK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= 'INTEGER(SNK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SNK) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSNK',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SNK)'
    !Clear the variables for the next call
    CALL testClear()
    
  ENDSUBROUTINE testSNK
!
!-------------------------------------------------------------------------------
!Test SLK support
  SUBROUTINE testSLK()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSLK'
    valslk=5_SLK
    !test init
    CALL testParam%init('testError->testSLK',valslk,'The number 5')
    eParams => NULL()
    CALL testParam%init('testSLK',valslk,'The number 5')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SLK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSLK') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SLK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= 'INTEGER(SLK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SLK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The number 5') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SLK) FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valslk)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SLK)'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSLK',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSLK'',someParam) FAILED!'
      STOP 666
    ENDIF
    CALL someParam%get('testSLK',valslk)
    IF(valslk /= 5_SLK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSLK'',valslk) FAILED!'
      STOP 666
    ENDIF
    valslk=0_SLK
    CALL testParam%get('testSLK',valslk)
    IF(valslk /= 5_SLK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSLK'',valslk) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSLK',valslk)
    CALL testParam%get('testError',valslk)
    CALL someParam%get('testError',valslk)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SLK)'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSLK',3_SLK,'The number 3')
    CALL testParam%get('testSLK',valslk)
    IF(valslk /= 3_SLK .OR. someParam%description /= 'The number 3') THEN
      WRITE(*,*) 'someParam%set(''testSLK'',3_SLK,''The number 3'') FAILED!'
      STOP 666
    ENDIF
    CALL testParam%set('testSLK',5_SLK,'The number 5')
    CALL testParam%get('testSLK',valslk)
    IF(valslk /= 5_SLK .OR. someParam%description /= 'The number 5') THEN
      WRITE(*,*) 'testParam%set(''testSLK'',5_SLK) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%set('testSLK',valslk)
    CALL someParam%set('testError',valslk)
    CALL testParam%set('testError',valslk)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SLK)'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SLK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SLK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SLK) FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SLK) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SLK)'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSLK',4_SLK)
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SLK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSLK') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SLK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= 'INTEGER(SLK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SLK) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSLK',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SLK)'
    !Clear the variables for the next call
    CALL testClear()
    
  ENDSUBROUTINE testSLK
!
!-------------------------------------------------------------------------------
!Test SBK support
  SUBROUTINE testSBK()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSBK'
    valsbk=.TRUE.
    !test init
    CALL testParam%init('testError->testSBK',valsbk,'The value is TRUE')
    eParams => NULL()
    CALL testParam%init('testSBK',valsbk,'The value is TRUE')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SBK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSBK') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SBK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= 'LOGICAL(SBK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SBK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The value is TRUE') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SBK) FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valsbk)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SBK)'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSBK',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSBK'',someParam) FAILED!'
      STOP 666
    ENDIF
    CALL someParam%get('testSBK',valsbk)
    IF(.NOT.valsbk) THEN
      WRITE(*,*) 'CALL someParam%get(''testSBK'',valsbk) FAILED!'
      STOP 666
    ENDIF
    valsbk=.FALSE.
    CALL testParam%get('testSBK',valsbk)
    IF(.NOT.valsbk) THEN
      WRITE(*,*) 'CALL testParam%get(''testSBK'',valsbk) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSBK',valsbk)
    CALL testParam%get('testError',valsbk)
    CALL someParam%get('testError',valsbk)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SBK)'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSBK',.TRUE.,'The value is TRUE')
    CALL testParam%get('testSBK',valsbk)
    IF(.NOT.valsbk .OR. someParam%description /= 'The value is TRUE') THEN
      WRITE(*,*) 'someParam%set(''testSBK'',TRUE,''The value is TRUE'') FAILED!'
      STOP 666
    ENDIF
    CALL testParam%set('testSBK',.FALSE.,'The value is FALSE')
    CALL testParam%get('testSBK',valsbk)
    IF(valsbk .OR. someParam%description /= 'The value is FALSE') THEN
      WRITE(*,*) 'testParam%set(''testSBK'',FALSE) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%set('testSBK',valsbk)
    CALL someParam%set('testError',valsbk)
    CALL testParam%set('testError',valsbk)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SBK)'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SBK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SBK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SBK) FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SBK) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SBK)'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSBK',.TRUE.)
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SBK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSBK') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SBK) FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= 'LOGICAL(SBK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SBK) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSBK',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SBK)'
    !Clear the variables for the next call
    CALL testClear()
    
  ENDSUBROUTINE testSBK
!
!-------------------------------------------------------------------------------
!Test StringType support
  SUBROUTINE testSTR()
    
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSTR'
    valstr='''testing'''
    !test init
    CALL testParam%init('testError->testSTR',valstr,'The value is testing')
    eParams => NULL()
    CALL testParam%init('testSTR',valstr,'The value is testing')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat StringType (STR) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSTR') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name StringType (STR) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= 'TYPE(StringType)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype StringType (STR) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The value is testing') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description StringType (STR) FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valstr)
    WRITE(*,*) '  Passed: CALL testParam%init(...) StringType (STR)'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSTR',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR'',someParam) FAILED!'
      STOP 666
    ENDIF
    CALL someParam%get('testSTR',valstr)
    IF(valstr /= '''testing''') THEN
      WRITE(*,*) 'CALL someParam%get(''testSTR'',valstr) FAILED!'
      STOP 666
    ENDIF
    valstr='test again'
    CALL testParam%get('testSTR',valstr)
    IF(valstr /= '''testing''') THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR'',valstr) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSTR',valstr)
    CALL testParam%get('testError',valstr)
    CALL someParam%get('testError',valstr)
    WRITE(*,*) '  Passed: CALL testParam%get(...) StringType (STR)'
  
    !test set
    eParams => NULL()
    !For strings, they must be stored in a string type first, then passed in.
    valstr='another test'
    CALL someParam%set('testSTR',valstr,'The value is another test')
    !Clear the variable to confirm it gets set.
    valstr=''
    CALL testParam%get('testSTR',valstr)
    IF(valstr /= 'another test' .OR. someParam%description /= 'The value is another test') THEN
      WRITE(*,*) 'someParam%set(''testSTR'',''another test'',''The value is another test'') FAILED!'
      STOP 666
    ENDIF
    valstr='a different test'
    CALL testParam%set('testSTR',valstr,'The value is ''a different test''')
    valstr=''
    CALL testParam%get('testSTR',valstr)
    IF(valstr /= 'a different test' .OR. someParam%description /= 'The value is ''a different test''') THEN
      WRITE(*,*) 'testParam%set(''testSTR'',''a different test'') FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%set('testSTR',valstr)
    CALL someParam%set('testError',valstr)
    CALL testParam%set('testError',valstr)
    WRITE(*,*) '  Passed: CALL testParam%set(...) StringType (STR)'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name StringType (STR) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype StringType (STR) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description StringType (STR) FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat StringType (STR) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() StringType (STR)'
  
    !test assignment
    eParams => NULL()
    valstr='assignment test'
    CALL testParam%init('testSTR',valstr)
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat StringType (STR) FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSTR') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name StringType (STR) FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= 'TYPE(StringType)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype StringType (STR) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSTR',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) StringType (STR)'
    !Clear the variables for the next call
    CALL testClear()
    
  ENDSUBROUTINE testSTR
!
!-------------------------------------------------------------------------------
!Test Character/StringType support
  SUBROUTINE testCHAR()
    
    CHARACTER(LEN=100) :: valchar
  
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSTR'
    CALL testParam%init('testError->testSTR','''testing''','The value is testing')
    eParams => NULL()
    CALL testParam%init('testSTR','''testing''','The value is testing')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat StringType (CHAR) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSTR') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name StringType (CHAR) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= 'TYPE(StringType)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype StringType (CHAR) FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The value is testing') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description StringType (CHAR) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%init('testSTR','''testing''')
    WRITE(*,*) '  Passed: CALL testParam%init(...) StringType (CHAR)'
  
    !test get
    eParams => NULL()
    valchar='test again'
    CALL testParam%get('testSTR',valchar)
    IF(TRIM(valchar) /= '''testing''') THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR'',valchar) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%get(...) StringType (CHAR)'
  
    !test set
    eParams => NULL()
    !For strings, they must be stored in a string type first, then passed in.
    CALL testParam%set('testSTR','another test','The value is another test')
    !Clear the variable to confirm it gets set.
    valchar=''
    CALL testParam%get('testSTR',valchar)
    IF(valchar /= 'another test' .OR. testParam%pdat%description /= 'The value is another test') THEN
      WRITE(*,*) 'testParam%set(''testSTR'',''another test'',''The value is another test'') FAILED!'
      STOP 666
    ENDIF
    CALL testParam%set('testSTR','a different test')
    CALL testParam%get('testSTR',valchar)
    IF(TRIM(valchar) /= 'a different test') THEN
      WRITE(*,*) 'testParam%set(''testSTR'',''a different test'') FAILED!'
      STOP 666
    ENDIF
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%set(...) StringType (CHAR)'
    
    CALL testParam%clear()
    CALL testParam%add('testSTR','last test','actually second to last')
    CALL testParam%get('testSTR',valchar)
    IF(TRIM(valchar) /= 'last test') THEN
      WRITE(*,*) 'testParam%set(''testSTR'',''last test'') FAILED!'
      STOP 666
    ENDIF
    CALL testParam%clear()
    CALL testParam%add('testSTR','seriously last test')
    CALL testParam%get('testSTR',valchar)
    IF(TRIM(valchar) /= 'seriously last test') THEN
      WRITE(*,*) 'testParam%set(''testSTR'',''seriously last test'') FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL testParam%add(...) StringType (CHAR)'
    CALL testClear()
  ENDSUBROUTINE testCHAR
!
!-------------------------------------------------------------------------------
!Test 1-D Array SSK support
  SUBROUTINE testSSK1a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSSK1a'
    ALLOCATE(valssk1a(2))
    valssk1a(1)=5._SSK
    valssk1a(2)=7._SSK
    !test init
    CALL testParam%init('testError->testSSK1a',valssk1a,'The numbers 5.0 & 7.0')
    eParams => NULL()
    CALL testParam%init('testSSK1a',valssk1a,'The numbers 5.0 & 7.0')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SSK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSSK1a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SSK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '1-D ARRAY REAL(SSK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SSK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The numbers 5.0 & 7.0') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SSK) 1-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valssk1a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SSK) 1-D'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSSK1a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSSK1a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSSK1a',valssk1a)
    IF(valssk1a(1) /= 5.0_SSK .OR. valssk1a(2) /= 7.0_SSK ) THEN
      WRITE(*,*) 'CALL someParam%get(''testSSK1a'',valssk1a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valssk1a)
    ALLOCATE(valssk1a(1))
    CALL someParam%get('testSSK1a',valssk1a)
    IF(valssk1a(1) /= 5.0_SSK .OR. valssk1a(2) /= 7.0_SSK ) THEN
      WRITE(*,*) 'CALL someParam%get(''testSSK1a'',valssk1a) FAILED!'
      STOP 666
    ENDIF
    valssk1a=0.0_SSK
    CALL testParam%get('testSSK1a',valssk1a)
    IF(valssk1a(1) /= 5.0_SSK .OR. valssk1a(2) /= 7.0_SSK ) THEN
      WRITE(*,*) 'CALL testParam%get(''testSSK1a'',valssk1a) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSSK1a',valssk1a)
    CALL testParam%get('testError',valssk1a)
    CALL someParam%get('testError',valssk1a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SSK) 1-D'
  
    !test set
    eParams => NULL()
    !
    CALL someParam%set('testSSK1a',(/3.0_SSK,1.0_SSK/),'The number 3.0, and 1.0')
    CALL testParam%get('testSSK1a',valssk1a)
    IF(valssk1a(1) /= 3.0_SSK .OR. valssk1a(2) /= 1.0_SSK .OR. &
        someParam%description /= 'The number 3.0, and 1.0') THEN
      WRITE(*,*) 'someParam%set(''testSSK1a'',(/3.0_SSK,1.0_SSK/),''The number 3.0, and 1.0'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    CALL testParam%set('testSSK1a',(/5.0_SSK/),'The number 5.0')
    CALL testParam%get('testSSK1a',valssk1a)
    IF(valssk1a(1) /= 5.0_SSK .OR. SIZE(valssk1a) /= 1_SIK .OR. &
        someParam%description /= 'The number 5.0') THEN
      WRITE(*,*) 'testParam%set(''testSSK1a'',5.0_SSK) FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    CALL someParam%set('testSSK1a',(/1.0_SSK,1.5_SSK,2.0_SSK/),'The numbers 1.0, 1.5, and 2.0')
    CALL testParam%get('testSSK1a',valssk1a)
    IF(valssk1a(1) /= 1.0_SSK .OR. valssk1a(2) /= 1.5_SSK .OR. &
        valssk1a(3) /= 2.0_SSK .OR. SIZE(valssk1a) /= 3_SIK .OR. &
          someParam%description /= 'The numbers 1.0, 1.5, and 2.0') THEN
      WRITE(*,*) 'someParam%set(''testSSK1a'',(/1.0_SSK,1.5_SSK,2.0_SSK/),'// &
        '''The numbers 1.0, 1.5, and 2.0'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    CALL testParam%set('testSSK1a',(/5.0_SSK,5.5_SSK,6.0_SSK/),'The numbers 5.0, 5.5, and 6.0')
    CALL testParam%get('testSSK1a',valssk1a)
    IF(valssk1a(1) /= 5.0_SSK .OR. valssk1a(2) /= 5.5_SSK .OR. &
        valssk1a(3) /= 6.0_SSK .OR. SIZE(valssk1a) /= 3_SIK .OR. &
          someParam%description /= 'The numbers 5.0, 5.5, and 6.0') THEN
      WRITE(*,*) 'testParam%set(''testSSK1a'',5.0_SSK) FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%set('testSSK1a',valssk1a)
    CALL someParam%set('testError',valssk1a)
    CALL testParam%set('testError',valssk1a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SSK) 1-D'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SSK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SSK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SSK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SSK) 1-D FAILED!'
      STOP 666
    ENDIF

    
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SSK) 1-D'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSSK1a',(/4.0_SSK/))
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SSK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSSK1a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SSK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '1-D ARRAY REAL(SSK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SSK) 1-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSSK1a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SSK) 1-D'
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSSK1a
!
!-------------------------------------------------------------------------------
!Test 1-D Array SDK support
  SUBROUTINE testSDK1a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSDK1a'
    ALLOCATE(valsdk1a(2))
    valsdk1a(1)=5.5_SDK
    valsdk1a(2)=7.5_SDK
    !test init
    CALL testParam%init('testError->testSDK1a',valsdk1a,'The numbers 5.5 & 7.5')
    eParams => NULL()
    CALL testParam%init('testSDK1a',valsdk1a,'The numbers 5.5 & 7.5')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSDK1a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '1-D ARRAY REAL(SDK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The numbers 5.5 & 7.5') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valsdk1a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SDK) 1-D'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSDK1a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSDK1a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSDK1a',valsdk1a)
    IF(valsdk1a(1) /= 5.5_SDK .OR. valsdk1a(2) /= 7.5_SDK ) THEN
      WRITE(*,*) 'CALL someParam%get(''testSDK1a'',valsdk1a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valsdk1a)
    ALLOCATE(valsdk1a(1))
    CALL someParam%get('testSDK1a',valsdk1a)
    IF(valsdk1a(1) /= 5.5_SDK .OR. valsdk1a(2) /= 7.5_SDK ) THEN
      WRITE(*,*) 'CALL someParam%get(''testSDK1a'',valsdk1a) FAILED!'
      STOP 666
    ENDIF
    valsdk1a=0.0_SDK
    CALL testParam%get('testSDK1a',valsdk1a)
    IF(valsdk1a(1) /= 5.5_SDK .OR. valsdk1a(2) /= 7.5_SDK ) THEN
      WRITE(*,*) 'CALL testParam%get(''testSDK1a'',valsdk1a) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSDK1a',valsdk1a)
    CALL testParam%get('testError',valsdk1a)
    CALL someParam%get('testError',valsdk1a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SDK) 1-D'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSDK1a',(/3.5_SDK,1.5_SDK/),'The number 3.5, and 1.5')
    CALL testParam%get('testSDK1a',valsdk1a)
    IF(valsdk1a(1) /= 3.5_SDK .OR. valsdk1a(2) /= 1.5_SDK .OR. &
        someParam%description /= 'The number 3.5, and 1.5') THEN
      WRITE(*,*) 'someParam%set(''testSDK1a'',(/3.5_SDK,1.5_SDK/),''The number 3.5, and 1.5'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    CALL testParam%set('testSDK1a',(/5.5_SDK/),'The number 5.5')
    CALL testParam%get('testSDK1a',valsdk1a)
    IF(valsdk1a(1) /= 5.5_SDK .OR. SIZE(valsdk1a) /= 1_SIK .OR. &
        someParam%description /= 'The number 5.5') THEN
      WRITE(*,*) 'testParam%set(''testSDK1a'',5.5_SDK) FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    CALL someParam%set('testSDK1a',(/10.0_SDK,10.5_SDK,20.0_SDK/),'The numbers 10.0, 10.5, and 20.0')
    CALL testParam%get('testSDK1a',valsdk1a)
    IF(valsdk1a(1) /= 10.0_SDK .OR. valsdk1a(2) /= 10.5_SDK .OR. &
        valsdk1a(3) /= 20.0_SDK .OR. SIZE(valsdk1a) /= 3_SIK .OR. &
          someParam%description /= 'The numbers 10.0, 10.5, and 20.0') THEN
      WRITE(*,*) 'someParam%set(''testSDK1a'',(/10.0_SDK,10.5_SDK,20.0_SDK/),'// &
        '''The numbers 10.0, 10.5, and 20.0'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    CALL testParam%set('testSDK1a',(/50.0_SDK,50.5_SDK,60.0_SDK/),'The numbers 50.0, 50.5, and 60.0')
    CALL testParam%get('testSDK1a',valsdk1a)
    IF(valsdk1a(1) /= 50.0_SDK .OR. valsdk1a(2) /= 50.5_SDK .OR. &
        valsdk1a(3) /= 60.0_SDK .OR. SIZE(valsdk1a) /= 3_SIK .OR. &
          someParam%description /= 'The numbers 50.0, 50.5, and 60.0') THEN
      WRITE(*,*) 'testParam%set(''testSDK1a'',(/50.0_SDK,50.5_SDK,60.0_SDK/)) FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%set('testSDK1a',valsdk1a)
    CALL someParam%set('testError',valsdk1a)
    CALL testParam%set('testError',valsdk1a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SDK) 1-D'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SDK) 1-D FAILED!'
      STOP 666
    ENDIF

    
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SDK) 1-D'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSDK1a',(/4.0_SDK/))
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSDK1a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '1-D ARRAY REAL(SDK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSDK1a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SDK) 1-D'
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSDK1a
!
!-------------------------------------------------------------------------------
!Test 1-D Array SNK support
  SUBROUTINE testSNK1a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSNK1a'
    ALLOCATE(valsnk1a(2))
    valsnk1a(1)=5_SNK
    valsnk1a(2)=7_SNK
    !test init
    CALL testParam%init('testError->testSNK1a',valsnk1a,'The numbers 5 & 7')
    eParams => NULL()
    CALL testParam%init('testSNK1a',valsnk1a,'The numbers 5 & 7')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SNK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSNK1a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SNK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '1-D ARRAY INTEGER(SNK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SNK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The numbers 5 & 7') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SNK) 1-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valsnk1a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SNK) 1-D'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSNK1a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSNK1a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSNK1a',valsnk1a)
    IF(valsnk1a(1) /= 5_SNK .OR. valsnk1a(2) /= 7_SNK ) THEN
      WRITE(*,*) 'CALL someParam%get(''testSNK1a'',valsnk1a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valsnk1a)
    ALLOCATE(valsnk1a(1))
    CALL someParam%get('testSNK1a',valsnk1a)
    IF(valsnk1a(1) /= 5_SNK .OR. valsnk1a(2) /= 7_SNK ) THEN
      WRITE(*,*) 'CALL someParam%get(''testSNK1a'',valsnk1a) FAILED!'
      STOP 666
    ENDIF
    valsnk1a=0_SNK
    CALL testParam%get('testSNK1a',valsnk1a)
    IF(valsnk1a(1) /= 5_SNK .OR. valsnk1a(2) /= 7_SNK ) THEN
      WRITE(*,*) 'CALL testParam%get(''testSNK1a'',valsnk1a) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSNK1a',valsnk1a)
    CALL testParam%get('testError',valsnk1a)
    CALL someParam%get('testError',valsnk1a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SNK) 1-D'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSNK1a',(/3_SNK,1_SNK/),'The number 3, and 1')
    CALL testParam%get('testSNK1a',valsnk1a)
    IF(valsnk1a(1) /= 3_SNK .OR. valsnk1a(2) /= 1_SNK .OR. &
        someParam%description /= 'The number 3, and 1') THEN
      WRITE(*,*) 'someParam%set(''testSNK1a'',(/3_SNK,1_SNK/),''The number 3, and 1'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    CALL testParam%set('testSNK1a',(/5_SNK/),'The number 5')
    CALL testParam%get('testSNK1a',valsnk1a)
    IF(valsnk1a(1) /= 5_SNK .OR. SIZE(valsnk1a) /= 1_SIK .OR. &
        someParam%description /= 'The number 5') THEN
      WRITE(*,*) 'testParam%set(''testSNK1a'',5_SNK) FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    CALL someParam%set('testSNK1a',(/10_SNK,10_SNK,20_SNK/),'The numbers 10, 10, and 20')
    CALL testParam%get('testSNK1a',valsnk1a)
    IF(valsnk1a(1) /= 10_SNK .OR. valsnk1a(2) /= 10_SNK .OR. &
        valsnk1a(3) /= 20_SNK .OR. SIZE(valsnk1a) /= 3_SIK .OR. &
          someParam%description /= 'The numbers 10, 10, and 20') THEN
      WRITE(*,*) 'someParam%set(''testSNK1a'',(/10_SNK,10_SNK,20_SNK/),'// &
        '''The numbers 10, 10, and 20'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    CALL testParam%set('testSNK1a',(/50_SNK,55_SNK,60_SNK/),'The numbers 50, 55, and 60')
    CALL testParam%get('testSNK1a',valsnk1a)
    IF(valsnk1a(1) /= 50_SNK .OR. valsnk1a(2) /= 55_SNK .OR. &
        valsnk1a(3) /= 60_SNK .OR. SIZE(valsnk1a) /= 3_SIK .OR. &
          someParam%description /= 'The numbers 50, 55, and 60') THEN
      WRITE(*,*) 'testParam%set(''testSNK1a'',(/50_SNK,55_SNK,60_SNK/)) FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%set('testSNK1a',valsnk1a)
    CALL someParam%set('testError',valsnk1a)
    CALL testParam%set('testError',valsnk1a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SNK) 1-D'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SNK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SNK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SNK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SNK) 1-D FAILED!'
      STOP 666
    ENDIF

    
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SNK) 1-D'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSNK1a',(/4_SNK/))
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SNK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSNK1a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SNK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '1-D ARRAY INTEGER(SNK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SNK) 1-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSNK1a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SNK) 1-D'
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSNK1a
!
!-------------------------------------------------------------------------------
!Test 1-D Array SLK support
  SUBROUTINE testSLK1a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSLK1a'
    ALLOCATE(valslk1a(2))
    valslk1a(1)=6_SLK
    valslk1a(2)=8_SLK
    !test init
    CALL testParam%init('testError->testSLK1a',valslk1a,'The numbers 6 & 8')
    eParams => NULL()
    CALL testParam%init('testSLK1a',valslk1a,'The numbers 6 & 8')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SLK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSLK1a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SLK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '1-D ARRAY INTEGER(SLK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SLK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The numbers 6 & 8') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SLK) 1-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valslk1a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SLK) 1-D'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSLK1a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSLK1a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSLK1a',valslk1a)
    IF(valslk1a(1) /= 6_SLK .OR. valslk1a(2) /= 8_SLK ) THEN
      WRITE(*,*) 'CALL someParam%get(''testSLK1a'',valslk1a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valslk1a)
    ALLOCATE(valslk1a(1))
    CALL someParam%get('testSLK1a',valslk1a)
    IF(valslk1a(1) /= 6_SLK .OR. valslk1a(2) /= 8_SLK ) THEN
      WRITE(*,*) 'CALL someParam%get(''testSLK1a'',valslk1a) FAILED!'
      STOP 666
    ENDIF
    valslk1a=0_SLK
    CALL testParam%get('testSLK1a',valslk1a)
    IF(valslk1a(1) /= 6_SLK .OR. valslk1a(2) /= 8_SLK ) THEN
      WRITE(*,*) 'CALL testParam%get(''testSLK1a'',valslk1a) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSLK1a',valslk1a)
    CALL testParam%get('testError',valslk1a)
    CALL someParam%get('testError',valslk1a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SLK) 1-D'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSLK1a',(/3_SLK,1_SLK/),'The number 3, and 1')
    CALL testParam%get('testSLK1a',valslk1a)
    IF(valslk1a(1) /= 3_SLK .OR. valslk1a(2) /= 1_SLK .OR. &
        someParam%description /= 'The number 3, and 1') THEN
      WRITE(*,*) 'someParam%set(''testSLK1a'',(/3_SLK,1_SLK/),''The number 3, and 1'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    CALL testParam%set('testSLK1a',(/6_SLK/),'The number 6')
    CALL testParam%get('testSLK1a',valslk1a)
    IF(valslk1a(1) /= 6_SLK .OR. SIZE(valslk1a) /= 1_SIK .OR. &
        someParam%description /= 'The number 6') THEN
      WRITE(*,*) 'testParam%set(''testSLK1a'',6_SLK) FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    CALL someParam%set('testSLK1a',(/15_SLK,-15_SLK,20_SLK/),'The numbers 15, -15, and 20')
    CALL testParam%get('testSLK1a',valslk1a)
    IF(valslk1a(1) /= 15_SLK .OR. valslk1a(2) /= -15_SLK .OR. &
        valslk1a(3) /= 20_SLK .OR. SIZE(valslk1a) /= 3_SIK .OR. &
          someParam%description /= 'The numbers 15, -15, and 20') THEN
      WRITE(*,*) 'someParam%set(''testSLK1a'',(/15_SLK,-15_SLK,20_SLK/),'// &
        '''The numbers 15, -15, and 20'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    CALL testParam%set('testSLK1a',(/-50_SLK,-55_SLK,-60_SLK/),'The numbers -50, -55, and -60')
    CALL testParam%get('testSLK1a',valslk1a)
    IF(valslk1a(1) /= -50_SLK .OR. valslk1a(2) /= -55_SLK .OR. &
        valslk1a(3) /= -60_SLK .OR. SIZE(valslk1a) /= 3_SIK .OR. &
          someParam%description /= 'The numbers -50, -55, and -60') THEN
      WRITE(*,*) 'testParam%set(''testSLK1a'',(/-50_SLK,-55_SLK,-60_SLK/)) FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%set('testSLK1a',valslk1a)
    CALL someParam%set('testError',valslk1a)
    CALL testParam%set('testError',valslk1a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SLK) 1-D'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SLK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SLK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SLK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SLK) 1-D FAILED!'
      STOP 666
    ENDIF

    
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SLK) 1-D'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSLK1a',(/4_SLK/))
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SLK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSLK1a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SLK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '1-D ARRAY INTEGER(SLK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SLK) 1-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSLK1a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SLK) 1-D'
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSLK1a
!
!-------------------------------------------------------------------------------
!Test 1-D Array SBK support
  SUBROUTINE testSBK1a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSBK1a'
    ALLOCATE(valsbk1a(2))
    valsbk1a(1)=.TRUE.
    valsbk1a(2)=.FALSE.
    !test init
    CALL testParam%init('testError->testSBK1a',valsbk1a,'The values .TRUE. & .FALSE.')
    eParams => NULL()
    CALL testParam%init('testSBK1a',valsbk1a,'The values .TRUE. & .FALSE.')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SBK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSBK1a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SBK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '1-D ARRAY LOGICAL(SBK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SBK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The values .TRUE. & .FALSE.') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SBK) 1-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valsbk1a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SBK) 1-D'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSBK1a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSBK1a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSBK1a',valsbk1a)
    IF(.NOT.valsbk1a(1) .OR. valsbk1a(2)) THEN
      WRITE(*,*) 'CALL someParam%get(''testSBK1a'',valsbk1a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valsbk1a)
    ALLOCATE(valsbk1a(1))
    CALL someParam%get('testSBK1a',valsbk1a)
    IF(.NOT.valsbk1a(1) .OR. valsbk1a(2)) THEN
      WRITE(*,*) 'CALL someParam%get(''testSBK1a'',valsbk1a) FAILED!'
      STOP 666
    ENDIF
    valsbk1a=.FALSE.
    CALL testParam%get('testSBK1a',valsbk1a)
    IF(.NOT.valsbk1a(1) .OR. valsbk1a(2)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSBK1a'',valsbk1a) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSBK1a',valsbk1a)
    CALL testParam%get('testError',valsbk1a)
    CALL someParam%get('testError',valsbk1a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SBK) 1-D'
  
    !test set
    eParams => NULL()
    !
    CALL someParam%set('testSBK1a',(/.FALSE.,.TRUE./),'The values .FALSE. and .TRUE.')
    CALL testParam%get('testSBK1a',valsbk1a)
    IF(valsbk1a(1) .OR. .NOT.valsbk1a(2) .OR. &
        someParam%description /= 'The values .FALSE. and .TRUE.') THEN
      WRITE(*,*) 'someParam%set(''testSBK1a'',(/.FALSE.,.TRUE./),''The values .FALSE. and .TRUE.'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    CALL testParam%set('testSBK1a',(/.TRUE./),'The value .TRUE.')
    CALL testParam%get('testSBK1a',valsbk1a)
    IF(.NOT.valsbk1a(1) .OR. SIZE(valsbk1a) /= 1_SIK .OR. &
        someParam%description /= 'The value .TRUE.') THEN
      WRITE(*,*) 'testParam%set(''testSBK1a'',.TRUE.) FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    CALL someParam%set('testSBK1a',(/.FALSE.,.TRUE.,.TRUE./),'The values .FALSE.,.TRUE., and .TRUE.')
    CALL testParam%get('testSBK1a',valsbk1a)
    IF(valsbk1a(1) .OR. .NOT.valsbk1a(2) .OR. &
        .NOT.valsbk1a(3) .OR. SIZE(valsbk1a) /= 3_SIK .OR. &
          someParam%description /= 'The values .FALSE.,.TRUE., and .TRUE.') THEN
      WRITE(*,*) 'someParam%set(''testSBK1a'',(/.FALSE.,.TRUE.,.TRUE./),'// &
        '''The values .FALSE.,.TRUE., and .TRUE.'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    CALL testParam%set('testSBK1a',(/.TRUE.,.TRUE.,.FALSE./),'The values .TRUE.,.TRUE., and .FALSE.')
    CALL testParam%get('testSBK1a',valsbk1a)
    IF(.NOT.valsbk1a(1) .OR. .NOT.valsbk1a(2) .OR. &
        valsbk1a(3) .OR. SIZE(valsbk1a) /= 3_SIK .OR. &
          someParam%description /= 'The values .TRUE.,.TRUE., and .FALSE.') THEN
      WRITE(*,*) 'testParam%set(''testSBK1a'',(/.TRUE.,.TRUE.,.FALSE./),'// &
       '''The values .TRUE.,.TRUE., and .FALSE.'') FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%set('testSBK1a',valsbk1a)
    CALL someParam%set('testError',valsbk1a)
    CALL testParam%set('testError',valsbk1a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SBK) 1-D'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SBK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SBK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SBK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SBK) 1-D FAILED!'
      STOP 666
    ENDIF

    
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SBK) 1-D'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSBK1a',(/.TRUE./))
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SBK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSBK1a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SBK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '1-D ARRAY LOGICAL(SBK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SBK) 1-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSBK1a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SBK) 1-D'
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSBK1a
!
!-------------------------------------------------------------------------------
!Test 1-D array StringType support
  SUBROUTINE testSTR1a()
    
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSTR1a'
    ALLOCATE(valstr1a(2))
    valstr1a(1)='''testing'''
    valstr1a(2)='more testing'
    !test init
    CALL testParam%init('testError->testSTR1a',valstr1a,'The value is testing and more testing')
    eParams => NULL()
    CALL testParam%init('testSTR1a',valstr1a,'The value is testing and more testing')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSTR1a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '1-D ARRAY TYPE(StringType)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The value is testing and more testing') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valstr1a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) StringType (STR)'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSTR1a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR1a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSTR1a',valstr1a)
    IF(valstr1a(1) /= '''testing''' .OR. valstr1a(2) /= 'more testing' .OR. &
        SIZE(valstr1a) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSTR1a'',valstr1a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valstr1a)
    ALLOCATE(valstr1a(1))
    valstr1a(1)=''
    CALL someParam%get('testSTR1a',valstr1a)
    IF(valstr1a(1) /= '''testing''' .OR. valstr1a(2) /= 'more testing' .OR. &
        SIZE(valstr1a) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR1a'',valstr1a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valstr1a)
    ALLOCATE(valstr1a(10))
    !valstr1a(1)='test again'
    !valstr1a(2)='test again'
    CALL testParam%get('testSTR1a',valstr1a)
    IF(valstr1a(1) /= '''testing''' .OR. valstr1a(2) /= 'more testing' .OR. &
        SIZE(valstr1a) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR1a'',valstr1a) FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%get('testSTR1a',valstr1a)
    CALL testParam%get('testError',valstr1a)
    CALL someParam%get('testError',valstr1a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) StringType (STR)'
  
    !test set
    eParams => NULL()
    !For strings, they must be stored in a string type first, then passed in.
    valstr1a(1)='another test'
    valstr1a(2)='one more test'
    CALL someParam%set('testSTR1a',valstr1a,'The value is another test and one more test')
    !Clear the variable to confirm it gets set.
    valstr1a(1)=''
    valstr1a(2)=''
    CALL testParam%get('testSTR1a',valstr1a)
    IF(valstr1a(1) /= 'another test' .OR. valstr1a(2) /= 'one more test' .OR.  &
        SIZE(valstr1a) /= 2_SIK .OR. &
        someParam%description /= 'The value is another test and one more test') THEN
      WRITE(*,*) 'someParam%set(''testSTR1a'',''valstr1a,'// &
        ' The value is another test and one more test'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    DEALLOCATE(valstr1a)
    ALLOCATE(valstr1a(1))
    valstr1a(1)='a different size test'
    CALL testParam%set('testSTR1a',valstr1a,'The value is a different size test')
    DEALLOCATE(valstr1a)
    ALLOCATE(valstr1a(2))
    valstr1a(1)=''
    valstr1a(2)=''
    CALL testParam%get('testSTR1a',valstr1a)
    IF(valstr1a(1) /= 'a different size test' .OR. SIZE(valstr1a) /= 1_SIK .OR. &
        someParam%description /= 'The value is a different size test') THEN
      WRITE(*,*) 'someParam%set(''testSTR1a'',''valstr1a,'// &
        ' The value is a different size test'') FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    DEALLOCATE(valstr1a)
    ALLOCATE(valstr1a(3))
    valstr1a(1)='a same but different test'
    valstr1a(2)='a same but different test'
    valstr1a(3)='a same but different test'
    CALL someParam%set('testSTR1a',valstr1a,'The value is a same but different test')
    !Clear the variable to confirm it gets set.
    DEALLOCATE(valstr1a)
    ALLOCATE(valstr1a(2))
    valstr1a(1)=''
    valstr1a(2)=''
    CALL testParam%get('testSTR1a',valstr1a)
    IF(valstr1a(1) /= 'a same but different test' .OR. &
         valstr1a(2) /= 'a same but different test' .OR. &
           valstr1a(3) /= 'a same but different test' .OR. &
             SIZE(valstr1a) /= 3_SIK .OR. &
               someParam%description /= 'The value is a same but different test') THEN
      WRITE(*,*) 'someParam%set(''testSTR1a'',''valstr1a,'// &
        ' The value is a same but different test'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    valstr1a(1)='a different but same test'
    valstr1a(2)='a different but same test'
    valstr1a(3)='a different but same test'
    CALL testParam%set('testSTR1a',valstr1a,'The value is a different but same test')
    valstr1a(1)=''
    valstr1a(2)=''
    valstr1a(3)=''
    CALL testParam%get('testSTR1a',valstr1a)
    IF(valstr1a(1) /= 'a different but same test' .OR. &
         valstr1a(2) /= 'a different but same test' .OR. &
           valstr1a(3) /= 'a different but same test' .OR. &
             SIZE(valstr1a) /= 3_SIK .OR. &
               someParam%description /= 'The value is a different but same test') THEN
      WRITE(*,*) 'testParam%set(''testSTR1a'',''a different but same test'') FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%set('testSTR1a',valstr1a)
    CALL someParam%set('testError',valstr1a)
    CALL testParam%set('testError',valstr1a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) StringType (STR)'
    
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() StringType (STR)'
  
    !test assignment
    eParams => NULL()
    valstr1a(1)='assignment test'
    valstr1a(2)='assignment test'
    valstr1a(3)='assignment test'
    CALL testParam%init('testSTR1a',valstr1a)
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSTR1a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '1-D ARRAY TYPE(StringType)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSTR1a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) StringType (STR)'
    !Clear the variables for the next call
    DEALLOCATE(valstr1a)
    CALL testClear()
    
  ENDSUBROUTINE testSTR1a
!
!-------------------------------------------------------------------------------
!Test 2-D Array SSK support
  SUBROUTINE testSSK2a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSSK2a'
    ALLOCATE(valssk2a(2,2))
    valssk2a(1,1)=5._SSK
    valssk2a(2,1)=7._SSK
    valssk2a(1,2)=6._SSK
    valssk2a(2,2)=8._SSK
    !test init
    CALL testParam%init('testError->testSSK2a',valssk2a,'The numbers 5.0, 7.0, 6.0, & 8.0')
    eParams => NULL()
    CALL testParam%init('testSSK2a',valssk2a,'The numbers 5.0, 7.0, 6.0, & 8.0')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SSK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSSK2a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SSK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '2-D ARRAY REAL(SSK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SSK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The numbers 5.0, 7.0, 6.0, & 8.0') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SSK) 2-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valssk2a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SSK) 2-D'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSSK2a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSSK2a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSSK2a',valssk2a)
    IF(valssk2a(1,1) /= 5.0_SSK .OR. valssk2a(2,1) /= 7.0_SSK .OR. &
        valssk2a(1,2) /= 6.0_SSK .OR. valssk2a(2,2) /= 8.0_SSK .OR. &
          SIZE(valssk2a,1) /= 2_SIK .OR. SIZE(valssk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSSK2a'',valssk2a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valssk2a)
    ALLOCATE(valssk2a(1,1))
    CALL someParam%get('testSSK2a',valssk2a)
    IF(valssk2a(1,1) /= 5.0_SSK .OR. valssk2a(2,1) /= 7.0_SSK .OR. &
        valssk2a(1,2) /= 6.0_SSK .OR. valssk2a(2,2) /= 8.0_SSK .OR. &
          SIZE(valssk2a,1) /= 2_SIK .OR. SIZE(valssk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSSK2a'',valssk2a) FAILED!'
      STOP 666
    ENDIF
    valssk2a=0.0_SSK
    CALL testParam%get('testSSK2a',valssk2a)
    IF(valssk2a(1,1) /= 5.0_SSK .OR. valssk2a(2,1) /= 7.0_SSK .OR. &
        valssk2a(1,2) /= 6.0_SSK .OR. valssk2a(2,2) /= 8.0_SSK .OR. &
          SIZE(valssk2a,1) /= 2_SIK .OR. SIZE(valssk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSSK2a'',valssk2a) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSSK2a',valssk2a)
    CALL testParam%get('testError',valssk2a)
    CALL someParam%get('testError',valssk2a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SSK) 2-D'
  
    !test set
    eParams => NULL()
    !
    CALL someParam%set('testSSK2a',RESHAPE((/3.0_SSK,1.0_SSK,4.0_SSK,2.0_SSK/),(/2,2/) ),'The number 3.0, 1.0, 4.0, and 2.0')
    CALL testParam%get('testSSK2a',valssk2a)
    IF(valssk2a(1,1) /= 3.0_SSK .OR. valssk2a(2,1) /= 1.0_SSK .OR. &
        valssk2a(1,2) /= 4.0_SSK .OR. valssk2a(2,2) /= 2.0_SSK .OR. &
          SIZE(valssk2a,1) /= 2_SIK .OR. SIZE(valssk2a,2) /= 2_SIK .OR. &
            someParam%description /= 'The number 3.0, 1.0, 4.0, and 2.0') THEN
      WRITE(*,*) 'someParam%set(''testSSK2a'',(/3.0_SSK,1.0_SSK/,/4.0_SSK,2.0_SSK/),''The number 3.0, 1.0, 4.0, and 2.0'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    CALL testParam%set('testSSK2a',RESHAPE((/5.0_SSK/),(/1,1/)),'The number 5.0')
    CALL testParam%get('testSSK2a',valssk2a)
    IF(valssk2a(1,1) /= 5.0_SSK .OR. SIZE(valssk2a,1) /= 1_SIK .OR. &
        SIZE(valssk2a,2) /= 1_SIK .OR. &
          someParam%description /= 'The number 5.0') THEN
      WRITE(*,*) 'testParam%set(''testSSK2a'',5.0_SSK) FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    CALL someParam%set('testSSK2a',RESHAPE((/1.0_SSK,1.5_SSK,2.0_SSK,-1.0_SSK,-1.5_SSK,-2.0_SSK/),(/3,2/)), &
      'The numbers 1.0, 1.5, 2.0, -1.0, -1.5, and -2.0')
    CALL testParam%get('testSSK2a',valssk2a)
    IF(valssk2a(1,1) /= 1.0_SSK .OR. valssk2a(2,1) /= 1.5_SSK .OR. &
        valssk2a(3,1) /= 2.0_SSK .OR. SIZE(valssk2a,1) /= 3_SIK .OR. &
          valssk2a(1,2) /= -1.0_SSK .OR. valssk2a(2,2) /= -1.5_SSK .OR. &
            valssk2a(3,2) /= -2.0_SSK .OR. SIZE(valssk2a,2) /= 2_SIK .OR. &
              someParam%description /= 'The numbers 1.0, 1.5, 2.0, -1.0, -1.5, and -2.0') THEN
      WRITE(*,*) 'someParam%set(''testSSK2a'',(/1.0_SSK,1.5_SSK,2.0_SSK/,/-1.0_SSK,'// &
        '-1.5_SSK,-2.0_SSK/), ''The numbers 1.0, 1.5, 2.0, -1.0, -1.5, and -2.0'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    CALL testParam%set('testSSK2a',RESHAPE((/5.0_SSK,5.5_SSK,6.0_SSK,-5.0_SSK,-5.5_SSK,-6.0_SSK/),(/3,2/)), &
      'The numbers 5.0, 5.5, 6.0, -5.0, -5.5, and -6.0')
    CALL testParam%get('testSSK2a',valssk2a)
    IF(valssk2a(1,1) /= 5.0_SSK .OR. valssk2a(2,1) /= 5.5_SSK .OR. &
        valssk2a(3,1) /= 6.0_SSK .OR. SIZE(valssk2a,1) /= 3_SIK .OR. &
          valssk2a(1,2) /= -5.0_SSK .OR. valssk2a(2,2) /= -5.5_SSK .OR. &
            valssk2a(3,2) /= -6.0_SSK .OR. SIZE(valssk2a,2) /= 2_SIK .OR. &
              someParam%description /= 'The numbers 5.0, 5.5, 6.0, -5.0, -5.5, and -6.0') THEN
      WRITE(*,*) 'testParam%set(''testSSK2a'',5.0_SSK) FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%set('testSSK2a',valssk2a)
    CALL someParam%set('testError',valssk2a)
    CALL testParam%set('testError',valssk2a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SSK) 2-D'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SSK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SSK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SSK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SSK) 2-D FAILED!'
      STOP 666
    ENDIF

    
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SSK) 2-D'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSSK2a',RESHAPE((/4.0_SSK/),(/1,1/)) )
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SSK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSSK2a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SSK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '2-D ARRAY REAL(SSK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SSK) 2-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSSK2a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SSK) 2-D'
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSSK2a
!
!-------------------------------------------------------------------------------
!Test 2-D Array SDK support
  SUBROUTINE testSDK2a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSDK2a'
    ALLOCATE(valsdk2a(2,2))
    valsdk2a(1,1)=5.5_SDK
    valsdk2a(2,1)=7.5_SDK
    valsdk2a(1,2)=6.5_SDK
    valsdk2a(2,2)=8.5_SDK
    !test init
    CALL testParam%init('testError->testSDK2a',valsdk2a,'The numbers 5.5, 7.5, 6.5, & 8.5')
    eParams => NULL()
    CALL testParam%init('testSDK2a',valsdk2a,'The numbers 5.5, 7.5, 6.5, & 8.5')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SDK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSDK2a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SDK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '2-D ARRAY REAL(SDK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SDK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The numbers 5.5, 7.5, 6.5, & 8.5') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SDK) 2-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valsdk2a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SDK) 2-D'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSDK2a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSDK2a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSDK2a',valsdk2a)
    IF(valsdk2a(1,1) /= 5.5_SDK .OR. valsdk2a(2,1) /= 7.5_SDK .OR. &
        valsdk2a(1,2) /= 6.5_SDK .OR. valsdk2a(2,2) /= 8.5_SDK .OR. &
          SIZE(valsdk2a,1) /= 2_SIK .OR. SIZE(valsdk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSDK2a'',valsdk2a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valsdk2a)
    ALLOCATE(valsdk2a(1,1))
    CALL someParam%get('testSDK2a',valsdk2a)
    IF(valsdk2a(1,1) /= 5.5_SDK .OR. valsdk2a(2,1) /= 7.5_SDK .OR. &
        valsdk2a(1,2) /= 6.5_SDK .OR. valsdk2a(2,2) /= 8.5_SDK .OR. &
          SIZE(valsdk2a,1) /= 2_SIK .OR. SIZE(valsdk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSDK2a'',valsdk2a) FAILED!'
      STOP 666
    ENDIF
    valsdk2a=0.0_SDK
    CALL testParam%get('testSDK2a',valsdk2a)
    IF(valsdk2a(1,1) /= 5.5_SDK .OR. valsdk2a(2,1) /= 7.5_SDK .OR. &
        valsdk2a(1,2) /= 6.5_SDK .OR. valsdk2a(2,2) /= 8.5_SDK .OR. &
          SIZE(valsdk2a,1) /= 2_SIK .OR. SIZE(valsdk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSDK2a'',valsdk2a) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSDK2a',valsdk2a)
    CALL testParam%get('testError',valsdk2a)
    CALL someParam%get('testError',valsdk2a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SDK) 2-D'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSDK2a',RESHAPE((/3.5_SDK,1.5_SDK,4.5_SDK,2.5_SDK/),(/2,2/)), &
        'The numbers 3.5, 1.5, 4.5, and 2.5')
    CALL testParam%get('testSDK2a',valsdk2a)
    IF(valsdk2a(1,1) /= 3.5_SDK .OR. valsdk2a(2,1) /= 1.5_SDK .OR. &
        valsdk2a(1,2) /= 4.5_SDK .OR. valsdk2a(2,2) /= 2.5_SDK .OR. &
          SIZE(valsdk2a,1) /= 2_SIK .OR. SIZE(valsdk2a,2) /= 2_SIK .OR. &
          someParam%description /= 'The numbers 3.5, 1.5, 4.5, and 2.5') THEN
      WRITE(*,*) 'someParam%set(''testSDK2a'',(/3.5_SDK,1.5_SDK/,/4.5_SDK,2.5_SDK/),'// &
      '''The numbers 3.5, 1.5, 4.5, and 2.5'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    CALL testParam%set('testSDK2a',RESHAPE((/5.5_SDK/),(/1,1/)),'The number 5.5')
    CALL testParam%get('testSDK2a',valsdk2a)
    IF(valsdk2a(1,1) /= 5.5_SDK .OR. SIZE(valsdk2a,1) /= 1_SIK .OR. &
        SIZE(valsdk2a,2) /= 1_SIK .OR. &
          someParam%description /= 'The number 5.5') THEN
      WRITE(*,*) 'testParam%set(''testSDK2a'',5.5_SDK) FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    CALL someParam%set('testSDK2a',RESHAPE((/10.0_SDK,10.5_SDK,20.0_SDK,-10.0_SDK,-10.5_SDK,-20.0_SDK/),(/3,2/)), &
      'The numbers 10.0, 10.5, 20.0, -10.0, -10.5, and -20.0')
    CALL testParam%get('testSDK2a',valsdk2a)
    IF(valsdk2a(1,1) /= 10.0_SDK .OR. valsdk2a(2,1) /= 10.5_SDK .OR. &
        valsdk2a(3,1) /= 20.0_SDK .OR. SIZE(valsdk2a,1) /= 3_SIK .OR. &
          valsdk2a(1,2) /= -10.0_SDK .OR. valsdk2a(2,2) /= -10.5_SDK .OR. &
            valsdk2a(3,2) /= -20.0_SDK .OR. SIZE(valsdk2a,2) /= 2_SIK .OR. &
              someParam%description /= 'The numbers 10.0, 10.5, 20.0, -10.0, -10.5, and -20.0') THEN
      WRITE(*,*) 'someParam%set(''testSDK2a'',(/10.0_SDK,10.5_SDK,20.0_SDK/,/-10.0_SDK'// &
        ',-10.5_SDK,-20.0_SDK/),''The numbers 10.0, 10.5, and 20.0'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    CALL testParam%set('testSDK2a',RESHAPE((/50.0_SDK,50.5_SDK,60.0_SDK,-50.0_SDK,-50.5_SDK,-60.0_SDK/),(/3,2/)), &
      'The numbers 50.0, 50.5, 60.0, -50.0, -50.5, and -60.0')
    CALL testParam%get('testSDK2a',valsdk2a)
    IF(valsdk2a(1,1) /= 50.0_SDK .OR. valsdk2a(2,1) /= 50.5_SDK .OR. &
        valsdk2a(3,1) /= 60.0_SDK .OR. SIZE(valsdk2a,1) /= 3_SIK .OR. &
          valsdk2a(1,2) /= -50.0_SDK .OR. valsdk2a(2,2) /= -50.5_SDK .OR. &
            valsdk2a(3,2) /= -60.0_SDK .OR. SIZE(valsdk2a,2) /= 2_SIK .OR. &
              someParam%description /= 'The numbers 50.0, 50.5, 60.0, -50.0, -50.5, and -60.0') THEN
      WRITE(*,*) 'testParam%set(''testSDK2a'',(/50.0_SDK,50.5_SDK,60.0_SDK/)) FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%set('testSDK2a',valsdk2a)
    CALL someParam%set('testError',valsdk2a)
    CALL testParam%set('testError',valsdk2a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SDK) 2-D'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SDK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SDK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SDK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SDK) 2-D FAILED!'
      STOP 666
    ENDIF

    
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SDK) 2-D'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSDK2a',RESHAPE((/4.0_SDK/),(/1,1/)) )
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SDK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSDK2a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SDK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '2-D ARRAY REAL(SDK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SDK) 2-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSDK2a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SDK) 2-D'
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSDK2a
!
!-------------------------------------------------------------------------------
!Test 2-D Array SNK support
  SUBROUTINE testSNK2a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSNK2a'
    ALLOCATE(valsnk2a(2,2))
    valsnk2a(1,1)=5_SNK
    valsnk2a(2,1)=7_SNK
    valsnk2a(1,2)=6_SNK
    valsnk2a(2,2)=8_SNK
    !test init
    CALL testParam%init('testError->testSNK2a',valsnk2a,'The numbers 5, 7, 6, & 8')
    eParams => NULL()
    CALL testParam%init('testSNK2a',valsnk2a,'The numbers 5, 7, 6, & 8')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SNK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSNK2a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SNK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '2-D ARRAY INTEGER(SNK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SNK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The numbers 5, 7, 6, & 8') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SNK) 2-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valsnk2a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SNK) 2-D'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSNK2a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSNK2a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSNK2a',valsnk2a)
    IF(valsnk2a(1,1) /= 5_SNK .OR. valsnk2a(2,1) /= 7_SNK .OR. &
        valsnk2a(1,2) /= 6_SNK .OR. valsnk2a(2,2) /= 8_SNK .OR. &
          SIZE(valsnk2a,1) /= 2_SIK .OR. SIZE(valsnk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSNK2a'',valsnk2a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valsnk2a)
    ALLOCATE(valsnk2a(1,1))
    CALL someParam%get('testSNK2a',valsnk2a)
    IF(valsnk2a(1,1) /= 5_SNK .OR. valsnk2a(2,1) /= 7_SNK .OR. &
        valsnk2a(1,2) /= 6_SNK .OR. valsnk2a(2,2) /= 8_SNK .OR. &
          SIZE(valsnk2a,1) /= 2_SIK .OR. SIZE(valsnk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSNK2a'',valsnk2a) FAILED!'
      STOP 666
    ENDIF
    valsnk2a=0_SNK
    CALL testParam%get('testSNK2a',valsnk2a)
    IF(valsnk2a(1,1) /= 5_SNK .OR. valsnk2a(2,1) /= 7_SNK .OR. &
        valsnk2a(1,2) /= 6_SNK .OR. valsnk2a(2,2) /= 8_SNK .OR. &
          SIZE(valsnk2a,1) /= 2_SIK .OR. SIZE(valsnk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSNK2a'',valsnk2a) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSNK2a',valsnk2a)
    CALL testParam%get('testError',valsnk2a)
    CALL someParam%get('testError',valsnk2a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SNK) 2-D'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSNK2a',RESHAPE((/3_SNK,1_SNK,4_SNK,2_SNK/),(/2,2/)),'The numbers 3, 1, 4, and 2')
    CALL testParam%get('testSNK2a',valsnk2a)
    IF(valsnk2a(1,1) /= 3_SNK .OR. valsnk2a(2,1) /= 1_SNK .OR. &
        valsnk2a(1,2) /= 4_SNK .OR. valsnk2a(2,2) /= 2_SNK .OR. &
          SIZE(valsnk2a,1) /= 2_SIK .OR. SIZE(valsnk2a,2) /= 2_SIK .OR. &
          someParam%description /= 'The numbers 3, 1, 4, and 2') THEN
      WRITE(*,*) 'someParam%set(''testSNK2a'',(/3_SNK,1_SNK/),''The numbers 3, 1, 4, and 2'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    CALL testParam%set('testSNK2a',RESHAPE((/5_SNK/),(/1,1/)),'The number 5')
    CALL testParam%get('testSNK2a',valsnk2a)
    IF(valsnk2a(1,1) /= 5_SNK .OR. SIZE(valsnk2a,1) /= 1_SIK .OR. &
        SIZE(valsnk2a,2) /= 1_SIK .OR. &
          someParam%description /= 'The number 5') THEN
      WRITE(*,*) 'testParam%set(''testSNK2a'',5_SNK) FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    CALL someParam%set('testSNK2a',RESHAPE((/10_SNK,10_SNK,20_SNK,-10_SNK,-10_SNK,-20_SNK/),(/3,2/)), &
        'The numbers 10, 10, 20, -10, -10, and -20')
    CALL testParam%get('testSNK2a',valsnk2a)
    IF(valsnk2a(1,1) /= 10_SNK .OR. valsnk2a(2,1) /= 10_SNK .OR. &
        valsnk2a(3,1) /= 20_SNK .OR. SIZE(valsnk2a,1) /= 3_SIK .OR. &
          valsnk2a(1,2) /= -10_SNK .OR. valsnk2a(2,2) /= -10_SNK .OR. &
            valsnk2a(3,2) /= -20_SNK .OR. SIZE(valsnk2a,2) /= 2_SIK .OR. &
              someParam%description /= 'The numbers 10, 10, 20, -10, -10, and -20') THEN
      WRITE(*,*) 'someParam%set(''testSNK2a'',(/10_SNK,10_SNK,20_SNK/,/-10_SNK,-10_SNK,-20_SNK/),'// &
        '''The numbers 10, 10, 20, -10, -10, and -20'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    CALL testParam%set('testSNK2a',RESHAPE((/50_SNK,55_SNK,60_SNK,-50_SNK,-55_SNK,-60_SNK/),(/3,2/)), &
      'The numbers 50, 55, 60, -50, -55, and -60')
    CALL testParam%get('testSNK2a',valsnk2a)
    IF(valsnk2a(1,1) /= 50_SNK .OR. valsnk2a(2,1) /= 55_SNK .OR. &
        valsnk2a(3,1) /= 60_SNK .OR. SIZE(valsnk2a,1) /= 3_SIK .OR. &
          valsnk2a(1,2) /= -50_SNK .OR. valsnk2a(2,2) /= -55_SNK .OR. &
            valsnk2a(3,2) /= -60_SNK .OR. SIZE(valsnk2a,2) /= 2_SIK .OR. &
              someParam%description /= 'The numbers 50, 55, 60, -50, -55, and -60') THEN
      WRITE(*,*) 'testParam%set(''testSNK2a'',(/50_SNK,55_SNK,60_SNK/,/-50_SNK,-55_SNK,-60_SNK/)) FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%set('testSNK2a',valsnk2a)
    CALL someParam%set('testError',valsnk2a)
    CALL testParam%set('testError',valsnk2a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SNK) 2-D'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SNK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SNK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SNK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SNK) 2-D FAILED!'
      STOP 666
    ENDIF

    
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SNK) 2-D'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSNK2a',RESHAPE((/4_SNK/),(/1,1/)) )
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SNK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSNK2a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SNK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '2-D ARRAY INTEGER(SNK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SNK) 2-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSNK2a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SNK) 2-D'
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSNK2a
!
!-------------------------------------------------------------------------------
!Test 2-D Array SLK support
  SUBROUTINE testSLK2a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSLK2a'
    ALLOCATE(valslk2a(2,2))
    valslk2a(1,1)=6_SLK
    valslk2a(2,1)=8_SLK
    valslk2a(1,2)=7_SLK
    valslk2a(2,2)=9_SLK
    !test init
    CALL testParam%init('testError->testSLK2a',valslk2a,'The numbers 6, 8, 7, & 9')
    eParams => NULL()
    CALL testParam%init('testSLK2a',valslk2a,'The numbers 6, 8, 7, & 9')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SLK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSLK2a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SLK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '2-D ARRAY INTEGER(SLK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SLK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The numbers 6, 8, 7, & 9') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SLK) 2-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valslk2a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SLK) 2-D'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSLK2a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSLK2a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSLK2a',valslk2a)
    IF(valslk2a(1,1) /= 6_SLK .OR. valslk2a(2,1) /= 8_SLK .OR. &
        valslk2a(1,2) /= 7_SLK .OR. valslk2a(2,2) /= 9_SLK .OR. &
          SIZE(valslk2a,1) /= 2_SIK .OR. SIZE(valslk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSLK2a'',valslk2a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valslk2a)
    ALLOCATE(valslk2a(1,1))
    CALL someParam%get('testSLK2a',valslk2a)
    IF(valslk2a(1,1) /= 6_SLK .OR. valslk2a(2,1) /= 8_SLK .OR. &
        valslk2a(1,2) /= 7_SLK .OR. valslk2a(2,2) /= 9_SLK .OR. &
          SIZE(valslk2a,1) /= 2_SIK .OR. SIZE(valslk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSLK2a'',valslk2a) FAILED!'
      STOP 666
    ENDIF
    valslk2a=0_SLK
    CALL testParam%get('testSLK2a',valslk2a)
    IF(valslk2a(1,1) /= 6_SLK .OR. valslk2a(2,1) /= 8_SLK .OR. &
        valslk2a(1,2) /= 7_SLK .OR. valslk2a(2,2) /= 9_SLK .OR. &
          SIZE(valslk2a,1) /= 2_SIK .OR. SIZE(valslk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSLK2a'',valslk2a) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSLK2a',valslk2a)
    CALL testParam%get('testError',valslk2a)
    CALL someParam%get('testError',valslk2a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SLK) 2-D'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSLK2a',RESHAPE((/3_SLK,1_SLK,4_SLK,2_SLK/),(/2,2/)),'The numbers 3, 1, 4 and 2')
    CALL testParam%get('testSLK2a',valslk2a)
    IF(valslk2a(1,1) /= 3_SLK .OR. valslk2a(2,1) /= 1_SLK .OR. &
        valslk2a(1,2) /= 4_SLK .OR. valslk2a(2,2) /= 2_SLK .OR. &
          SIZE(valslk2a,1) /= 2_SIK .OR. SIZE(valslk2a,2) /= 2_SIK .OR. &
            someParam%description /= 'The numbers 3, 1, 4 and 2') THEN
      WRITE(*,*) 'someParam%set(''testSLK2a'',(/3_SLK,1_SLK/,/4_SLK,2_SLK/),''The numbers 3, 1, 4 and 2'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    CALL testParam%set('testSLK2a',RESHAPE((/6_SLK/),(/1,1/)),'The number 6')
    CALL testParam%get('testSLK2a',valslk2a)
    IF(valslk2a(1,1) /= 6_SLK .OR. SIZE(valslk2a,1) /= 1_SIK .OR. &
        SIZE(valslk2a,2) /= 1_SIK .OR. &
          someParam%description /= 'The number 6') THEN
      WRITE(*,*) 'testParam%set(''testSLK2a'',6_SLK) FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    CALL someParam%set('testSLK2a',RESHAPE((/15_SLK,-15_SLK,20_SLK,-15_SLK,15_SLK,-20_SLK/),(/3,2/)), &
      'The numbers 15, -15, 20, -15, 15, and -20')
    CALL testParam%get('testSLK2a',valslk2a)
    IF(valslk2a(1,1) /= 15_SLK .OR. valslk2a(2,1) /= -15_SLK .OR. &
        valslk2a(3,1) /= 20_SLK .OR. SIZE(valslk2a,1) /= 3_SIK .OR. &
          valslk2a(1,2) /= -15_SLK .OR. valslk2a(2,2) /= 15_SLK .OR. &
            valslk2a(3,2) /= -20_SLK .OR. SIZE(valslk2a,2) /= 2_SIK .OR. &
              someParam%description /= 'The numbers 15, -15, 20, -15, 15, and -20') THEN
      WRITE(*,*) 'someParam%set(''testSLK2a'',(/15_SLK,-15_SLK,20_SLK/,/-15_SLK,'// &
        '15_SLK,-20_SLK/),''The numbers 15, -15, 20, -15, 15, and -20'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    CALL testParam%set('testSLK2a',RESHAPE((/-50_SLK,-55_SLK,-60_SLK,50_SLK,55_SLK,60_SLK/),(/3,2/)), &
      'The numbers -50, -55, -60, 50, 55, and 60')
    CALL testParam%get('testSLK2a',valslk2a)
    IF(valslk2a(1,1) /= -50_SLK .OR. valslk2a(2,1) /= -55_SLK .OR. &
        valslk2a(3,1) /= -60_SLK .OR. SIZE(valslk2a,1) /= 3_SIK .OR. &
          valslk2a(1,2) /= 50_SLK .OR. valslk2a(2,2) /= 55_SLK .OR. &
            valslk2a(3,2) /= 60_SLK .OR. SIZE(valslk2a,2) /= 2_SIK .OR. &
              someParam%description /= 'The numbers -50, -55, -60, 50, 55, and 60') THEN
      WRITE(*,*) 'testParam%set(''testSLK2a'',(/-50_SLK,-55_SLK,-60_SLK/,'// &
        '/50_SLK,55_SLK,60_SLK/)) FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%set('testSLK2a',valslk2a)
    CALL someParam%set('testError',valslk2a)
    CALL testParam%set('testError',valslk2a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SLK) 2-D'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SLK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SLK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SLK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SLK) 2-D FAILED!'
      STOP 666
    ENDIF

    
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SLK) 2-D'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSLK2a',RESHAPE((/4_SLK/),(/1,1/)) )
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SLK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSLK2a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SLK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '2-D ARRAY INTEGER(SLK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SLK) 2-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSLK2a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SLK) 2-D'
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSLK2a
!
!-------------------------------------------------------------------------------
!Test 3-D Array SSK support
  SUBROUTINE testSSK3a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSSK3a'
    ALLOCATE(valssk3a(2,2,2))
    valssk3a(1,1,:)=5._SSK
    valssk3a(2,1,:)=7._SSK
    valssk3a(1,2,:)=6._SSK
    valssk3a(2,2,:)=8._SSK
    !test init
    CALL testParam%init('testError->testSSK3a',valssk3a,'The numbers 5.0, 7.0, 6.0, & 8.0')
    eParams => NULL()
    CALL testParam%init('testSSK3a',valssk3a,'The numbers 5.0, 7.0, 6.0, & 8.0')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SSK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSSK3a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SSK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '3-D ARRAY REAL(SSK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SSK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The numbers 5.0, 7.0, 6.0, & 8.0') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SSK) 3-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valssk3a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SSK) 3-D'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSSK3a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSSK3a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSSK3a',valssk3a)
    IF(ANY(valssk3a(1,1,:) /= 5.0_SSK) .OR. ANY(valssk3a(2,1,:) /= 7.0_SSK) .OR. &
        ANY(valssk3a(1,2,:) /= 6.0_SSK) .OR. ANY(valssk3a(2,2,:) /= 8.0_SSK) .OR. &
          SIZE(valssk3a,1) /= 2_SIK .OR. SIZE(valssk3a,2) /= 2_SIK .OR. &
            SIZE(valssk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSSK3a'',valssk3a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valssk3a)
    ALLOCATE(valssk3a(1,1,1))
    CALL someParam%get('testSSK3a',valssk3a)
    IF(ANY(valssk3a(1,1,:) /= 5.0_SSK) .OR. ANY(valssk3a(2,1,:) /= 7.0_SSK) .OR. &
        ANY(valssk3a(1,2,:) /= 6.0_SSK) .OR. ANY(valssk3a(2,2,:) /= 8.0_SSK) .OR. &
          SIZE(valssk3a,1) /= 2_SIK .OR. SIZE(valssk3a,2) /= 2_SIK .OR. &
            SIZE(valssk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSSK3a'',valssk3a) FAILED!'
      STOP 666
    ENDIF
    valssk3a=0.0_SSK
    CALL testParam%get('testSSK3a',valssk3a)
    IF(ANY(valssk3a(1,1,:) /= 5.0_SSK) .OR. ANY(valssk3a(2,1,:) /= 7.0_SSK) .OR. &
        ANY(valssk3a(1,2,:) /= 6.0_SSK) .OR. ANY(valssk3a(2,2,:) /= 8.0_SSK) .OR. &
          SIZE(valssk3a,1) /= 2_SIK .OR. SIZE(valssk3a,2) /= 2_SIK .OR. &
            SIZE(valssk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSSK3a'',valssk3a) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSSK3a',valssk3a)
    CALL testParam%get('testError',valssk3a)
    CALL someParam%get('testError',valssk3a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SSK) 3-D'
  
    !test set
    eParams => NULL()
    !
    CALL someParam%set('testSSK3a', &
      RESHAPE((/3.0_SSK,1.0_SSK,4.0_SSK,2.0_SSK,3.0_SSK,1.0_SSK,4.0_SSK,2.0_SSK/), &
        (/2,2,2/) ), 'The number 3.0, 1.0, 4.0, and 2.0')
    CALL testParam%get('testSSK3a',valssk3a)
    IF(ANY(valssk3a(1,1,:) /= 3.0_SSK) .OR. ANY(valssk3a(2,1,:) /= 1.0_SSK) .OR. &
        ANY(valssk3a(1,2,:) /= 4.0_SSK) .OR. ANY(valssk3a(2,2,:) /= 2.0_SSK) .OR. &
          SIZE(valssk3a,1) /= 2_SIK .OR. SIZE(valssk3a,2) /= 2_SIK .OR. &
            SIZE(valssk3a,3) /= 2_SIK .OR. &
            someParam%description /= 'The number 3.0, 1.0, 4.0, and 2.0') THEN
      WRITE(*,*) 'someParam%set(''testSSK3a'',(/3.0_SSK,1.0_SSK,4.0_SSK,2.0_SSK,3.0_SSK,'// &
         '1.0_SSK,4.0_SSK,2.0_SSK/),''The number 3.0, 1.0, 4.0, and 2.0'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    CALL testParam%set('testSSK3a',RESHAPE((/5.0_SSK/),(/1,1,1/)),'The number 5.0')
    CALL testParam%get('testSSK3a',valssk3a)
    IF(valssk3a(1,1,1) /= 5.0_SSK .OR. SIZE(valssk3a,1) /= 1_SIK .OR. &
        SIZE(valssk3a,2) /= 1_SIK .OR. SIZE(valssk3a,3) /= 1_SIK .OR. &
          someParam%description /= 'The number 5.0') THEN
      WRITE(*,*) 'testParam%set(''testSSK3a'',5.0_SSK) FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    CALL someParam%set('testSSK3a', &
      RESHAPE((/1.0_SSK,1.5_SSK,2.0_SSK,-1.0_SSK,-1.5_SSK,-2.0_SSK/),(/3,2,1/)), &
        'The numbers 1.0, 1.5, 2.0, -1.0, -1.5, and -2.0')
    CALL testParam%get('testSSK3a',valssk3a)
    IF(valssk3a(1,1,1) /= 1.0_SSK .OR. valssk3a(2,1,1) /= 1.5_SSK .OR. &
        valssk3a(3,1,1) /= 2.0_SSK .OR. SIZE(valssk3a,1) /= 3_SIK .OR. &
          valssk3a(1,2,1) /= -1.0_SSK .OR. valssk3a(2,2,1) /= -1.5_SSK .OR. &
            valssk3a(3,2,1) /= -2.0_SSK .OR. SIZE(valssk3a,2) /= 2_SIK .OR. &
              SIZE(valssk3a,3) /= 1_SIK .OR. &
                someParam%description /= 'The numbers 1.0, 1.5, 2.0, -1.0, -1.5, and -2.0') THEN
      WRITE(*,*) 'someParam%set(''testSSK3a'',(/1.0_SSK,1.5_SSK,2.0_SSK/,/-1.0_SSK,'// &
        '-1.5_SSK,-2.0_SSK/), ''The numbers 1.0, 1.5, 2.0, -1.0, -1.5, and -2.0'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    CALL testParam%set('testSSK3a',RESHAPE((/5.0_SSK,5.5_SSK,6.0_SSK,-5.0_SSK,-5.5_SSK,-6.0_SSK/),(/3,2,1/)), &
      'The numbers 5.0, 5.5, 6.0, -5.0, -5.5, and -6.0')
    CALL testParam%get('testSSK3a',valssk3a)
    IF(valssk3a(1,1,1) /= 5.0_SSK .OR. valssk3a(2,1,1) /= 5.5_SSK .OR. &
        valssk3a(3,1,1) /= 6.0_SSK .OR. SIZE(valssk3a,1) /= 3_SIK .OR. &
          valssk3a(1,2,1) /= -5.0_SSK .OR. valssk3a(2,2,1) /= -5.5_SSK .OR. &
            valssk3a(3,2,1) /= -6.0_SSK .OR. SIZE(valssk3a,2) /= 2_SIK .OR. &
              SIZE(valssk3a,3) /= 1_SIK .OR. &
                someParam%description /= 'The numbers 5.0, 5.5, 6.0, -5.0, -5.5, and -6.0') THEN
      WRITE(*,*) 'testParam%set(''testSSK3a'',5.0_SSK) FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%set('testSSK3a',valssk3a)
    CALL someParam%set('testError',valssk3a)
    CALL testParam%set('testError',valssk3a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SSK) 3-D'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SSK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SSK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SSK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SSK) 3-D FAILED!'
      STOP 666
    ENDIF

    
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SSK) 3-D'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSSK3a',RESHAPE((/4.0_SSK/),(/1,1,1/)) )
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SSK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSSK3a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SSK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '3-D ARRAY REAL(SSK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SSK) 3-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSSK3a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SSK) 3-D'
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSSK3a
!
!-------------------------------------------------------------------------------
!Test 3-D Array SDK support
  SUBROUTINE testSDK3a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSDK3a'
    ALLOCATE(valsdk3a(2,2,2))
    valsdk3a(1,1,:)=5.5_SDK
    valsdk3a(2,1,:)=7.5_SDK
    valsdk3a(1,2,:)=6.5_SDK
    valsdk3a(2,2,:)=8.5_SDK
    !test init
    CALL testParam%init('testError->testSDK3a',valsdk3a,'The numbers 5.5, 7.5, 6.5, & 8.5')
    eParams => NULL()
    CALL testParam%init('testSDK3a',valsdk3a,'The numbers 5.5, 7.5, 6.5, & 8.5')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SDK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSDK3a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SDK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '3-D ARRAY REAL(SDK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SDK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The numbers 5.5, 7.5, 6.5, & 8.5') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SDK) 3-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valsdk3a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SDK) 3-D'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSDK3a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSDK3a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSDK3a',valsdk3a)
    IF(ANY(valsdk3a(1,1,:) /= 5.5_SDK) .OR. ANY(valsdk3a(2,1,:) /= 7.5_SDK) .OR. &
        ANY(valsdk3a(1,2,:) /= 6.5_SDK) .OR. ANY(valsdk3a(2,2,:) /= 8.5_SDK) .OR. &
          SIZE(valsdk3a,1) /= 2_SIK .OR. SIZE(valsdk3a,2) /= 2_SIK .OR. &
            SIZE(valsdk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSDK3a'',valsdk3a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valsdk3a)
    ALLOCATE(valsdk3a(1,1,1))
    CALL someParam%get('testSDK3a',valsdk3a)
    IF(ANY(valsdk3a(1,1,:) /= 5.5_SDK) .OR. ANY(valsdk3a(2,1,:) /= 7.5_SDK) .OR. &
        ANY(valsdk3a(1,2,:) /= 6.5_SDK) .OR. ANY(valsdk3a(2,2,:) /= 8.5_SDK) .OR. &
          SIZE(valsdk3a,1) /= 2_SIK .OR. SIZE(valsdk3a,2) /= 2_SIK .OR. &
            SIZE(valsdk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSDK3a'',valsdk3a) FAILED!'
      STOP 666
    ENDIF
    valsdk3a=0.0_SDK
    CALL testParam%get('testSDK3a',valsdk3a)
    IF(ANY(valsdk3a(1,1,:) /= 5.5_SDK) .OR. ANY(valsdk3a(2,1,:) /= 7.5_SDK) .OR. &
        ANY(valsdk3a(1,2,:) /= 6.5_SDK) .OR. ANY(valsdk3a(2,2,:) /= 8.5_SDK) .OR. &
          SIZE(valsdk3a,1) /= 2_SIK .OR. SIZE(valsdk3a,2) /= 2_SIK .OR. &
            SIZE(valsdk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSDK3a'',valsdk3a) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSDK3a',valsdk3a)
    CALL testParam%get('testError',valsdk3a)
    CALL someParam%get('testError',valsdk3a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SDK) 3-D'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSDK3a', &
      RESHAPE((/3.5_SDK,1.5_SDK,4.5_SDK,2.5_SDK,3.5_SDK,1.5_SDK,4.5_SDK,2.5_SDK/), &
        (/2,2,2/)), 'The numbers 3.5, 1.5, 4.5, and 2.5')
    CALL testParam%get('testSDK3a',valsdk3a)
    IF(ANY(valsdk3a(1,1,:) /= 3.5_SDK) .OR. ANY(valsdk3a(2,1,:) /= 1.5_SDK) .OR. &
        ANY(valsdk3a(1,2,:) /= 4.5_SDK) .OR. ANY(valsdk3a(2,2,:) /= 2.5_SDK) .OR. &
          SIZE(valsdk3a,1) /= 2_SIK .OR. SIZE(valsdk3a,2) /= 2_SIK .OR. &
            SIZE(valsdk3a,3) /= 2_SIK .OR. &
              someParam%description /= 'The numbers 3.5, 1.5, 4.5, and 2.5') THEN
      WRITE(*,*) 'someParam%set(''testSDK3a'',(/3.5_SDK,1.5_SDK/,/4.5_SDK,2.5_SDK/),'// &
      '''The numbers 3.5, 1.5, 4.5, and 2.5'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    CALL testParam%set('testSDK3a',RESHAPE((/5.5_SDK/),(/1,1,1/)),'The number 5.5')
    CALL testParam%get('testSDK3a',valsdk3a)
    IF(valsdk3a(1,1,1) /= 5.5_SDK .OR. SIZE(valsdk3a,1) /= 1_SIK .OR. &
        SIZE(valsdk3a,2) /= 1_SIK .OR. SIZE(valsdk3a,3) /= 1_SIK .OR. &
          someParam%description /= 'The number 5.5') THEN
      WRITE(*,*) 'testParam%set(''testSDK3a'',5.5_SDK) FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    CALL someParam%set('testSDK3a', &
      RESHAPE((/10.0_SDK,10.5_SDK,20.0_SDK,-10.0_SDK,-10.5_SDK,-20.0_SDK/), & 
        (/3,2,1/)),'The numbers 10.0, 10.5, 20.0, -10.0, -10.5, and -20.0')
    CALL testParam%get('testSDK3a',valsdk3a)
    IF(valsdk3a(1,1,1) /= 10.0_SDK .OR. valsdk3a(2,1,1) /= 10.5_SDK .OR. &
        valsdk3a(3,1,1) /= 20.0_SDK .OR. SIZE(valsdk3a,1) /= 3_SIK .OR. &
          valsdk3a(1,2,1) /= -10.0_SDK .OR. valsdk3a(2,2,1) /= -10.5_SDK .OR. &
            valsdk3a(3,2,1) /= -20.0_SDK .OR. SIZE(valsdk3a,2) /= 2_SIK .OR. &
              SIZE(valsdk3a,3) /= 1_SIK .OR. &
                someParam%description /= 'The numbers 10.0, 10.5, 20.0, -10.0, -10.5, and -20.0') THEN
      WRITE(*,*) 'someParam%set(''testSDK3a'',(/10.0_SDK,10.5_SDK,20.0_SDK/,/-10.0_SDK'// &
        ',-10.5_SDK,-20.0_SDK/),''The numbers 10.0, 10.5, and 20.0'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    CALL testParam%set('testSDK3a', &
      RESHAPE((/50.0_SDK,50.5_SDK,60.0_SDK,-50.0_SDK,-50.5_SDK,-60.0_SDK/), &
        (/3,2,1/)),'The numbers 50.0, 50.5, 60.0, -50.0, -50.5, and -60.0')
    CALL testParam%get('testSDK3a',valsdk3a)
    IF(valsdk3a(1,1,1) /= 50.0_SDK .OR. valsdk3a(2,1,1) /= 50.5_SDK .OR. &
        valsdk3a(3,1,1) /= 60.0_SDK .OR. SIZE(valsdk3a,1) /= 3_SIK .OR. &
          valsdk3a(1,2,1) /= -50.0_SDK .OR. valsdk3a(2,2,1) /= -50.5_SDK .OR. &
            valsdk3a(3,2,1) /= -60.0_SDK .OR. SIZE(valsdk3a,2) /= 2_SIK .OR. &
              SIZE(valsdk3a,3) /= 1_SIK .OR. &
                someParam%description /= 'The numbers 50.0, 50.5, 60.0, -50.0, -50.5, and -60.0') THEN
      WRITE(*,*) 'testParam%set(''testSDK3a'',(/50.0_SDK,50.5_SDK,60.0_SDK/)) FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%set('testSDK3a',valsdk3a)
    CALL someParam%set('testError',valsdk3a)
    CALL testParam%set('testError',valsdk3a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SDK) 3-D'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SDK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SDK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SDK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SDK) 3-D FAILED!'
      STOP 666
    ENDIF

    
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SDK) 3-D'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSDK3a',RESHAPE((/4.0_SDK/),(/1,1,1/)) )
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SDK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSDK3a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SDK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '3-D ARRAY REAL(SDK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SDK) 3-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSDK3a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SDK) 3-D'
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSDK3a
!
!-------------------------------------------------------------------------------
!Test 3-D Array SNK support
  SUBROUTINE testSNK3a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSNK3a'
    ALLOCATE(valsnk3a(2,2,2))
    valsnk3a(1,1,:)=5_SNK
    valsnk3a(2,1,:)=7_SNK
    valsnk3a(1,2,:)=6_SNK
    valsnk3a(2,2,:)=8_SNK
    !test init
    CALL testParam%init('testError->testSNK3a',valsnk3a,'The numbers 5, 7, 6, & 8')
    eParams => NULL()
    CALL testParam%init('testSNK3a',valsnk3a,'The numbers 5, 7, 6, & 8')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SNK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSNK3a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SNK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '3-D ARRAY INTEGER(SNK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SNK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The numbers 5, 7, 6, & 8') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SNK) 3-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valsnk3a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SNK) 3-D'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSNK3a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSNK3a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSNK3a',valsnk3a)
    IF(ANY(valsnk3a(1,1,:) /= 5_SNK) .OR. ANY(valsnk3a(2,1,:) /= 7_SNK) .OR. &
        ANY(valsnk3a(1,2,:) /= 6_SNK) .OR. ANY(valsnk3a(2,2,:) /= 8_SNK) .OR. &
          SIZE(valsnk3a,1) /= 2_SIK .OR. SIZE(valsnk3a,2) /= 2_SIK .OR. &
            SIZE(valsnk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSNK3a'',valsnk3a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valsnk3a)
    ALLOCATE(valsnk3a(1,1,1))
    CALL someParam%get('testSNK3a',valsnk3a)
    IF(ANY(valsnk3a(1,1,:) /= 5_SNK) .OR. ANY(valsnk3a(2,1,:) /= 7_SNK) .OR. &
        ANY(valsnk3a(1,2,:) /= 6_SNK) .OR. ANY(valsnk3a(2,2,:) /= 8_SNK) .OR. &
          SIZE(valsnk3a,1) /= 2_SIK .OR. SIZE(valsnk3a,2) /= 2_SIK .OR. &
            SIZE(valsnk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSNK3a'',valsnk3a) FAILED!'
      STOP 666
    ENDIF
    valsnk3a=0_SNK
    CALL testParam%get('testSNK3a',valsnk3a)
    IF(ANY(valsnk3a(1,1,:) /= 5_SNK) .OR. ANY(valsnk3a(2,1,:) /= 7_SNK) .OR. &
        ANY(valsnk3a(1,2,:) /= 6_SNK) .OR. ANY(valsnk3a(2,2,:) /= 8_SNK) .OR. &
          SIZE(valsnk3a,1) /= 2_SIK .OR. SIZE(valsnk3a,2) /= 2_SIK .OR. &
            SIZE(valsnk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSNK3a'',valsnk3a) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSNK3a',valsnk3a)
    CALL testParam%get('testError',valsnk3a)
    CALL someParam%get('testError',valsnk3a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SNK) 3-D'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSNK3a', &
      RESHAPE((/3_SNK,1_SNK,4_SNK,2_SNK,3_SNK,1_SNK,4_SNK,2_SNK/), &
        (/2,2,2/)),'The numbers 3, 1, 4, and 2')
    CALL testParam%get('testSNK3a',valsnk3a)
    IF(ANY(valsnk3a(1,1,:) /= 3_SNK) .OR. ANY(valsnk3a(2,1,:) /= 1_SNK) .OR. &
        ANY(valsnk3a(1,2,:) /= 4_SNK) .OR. ANY(valsnk3a(2,2,:) /= 2_SNK) .OR. &
          SIZE(valsnk3a,1) /= 2_SIK .OR. SIZE(valsnk3a,2) /= 2_SIK .OR. &
            SIZE(valsnk3a,3) /= 2_SIK .OR. &
              someParam%description /= 'The numbers 3, 1, 4, and 2') THEN
      WRITE(*,*) 'someParam%set(''testSNK3a'',(/3_SNK,1_SNK/),''The numbers 3, 1, 4, and 2'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    CALL testParam%set('testSNK3a',RESHAPE((/5_SNK/),(/1,1,1/)),'The number 5')
    CALL testParam%get('testSNK3a',valsnk3a)
    IF(valsnk3a(1,1,1) /= 5_SNK .OR. SIZE(valsnk3a,1) /= 1_SIK .OR. &
        SIZE(valsnk3a,2) /= 1_SIK .OR. SIZE(valsnk3a,3) /= 1_SIK .OR. &
          someParam%description /= 'The number 5') THEN
      WRITE(*,*) 'testParam%set(''testSNK3a'',5_SNK) FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    CALL someParam%set('testSNK3a', &
      RESHAPE((/10_SNK,10_SNK,20_SNK,-10_SNK,-10_SNK,-20_SNK/),(/3,2,1/)), &
        'The numbers 10, 10, 20, -10, -10, and -20')
    CALL testParam%get('testSNK3a',valsnk3a)
    IF(valsnk3a(1,1,1) /= 10_SNK .OR. valsnk3a(2,1,1) /= 10_SNK .OR. &
        valsnk3a(3,1,1) /= 20_SNK .OR. SIZE(valsnk3a,1) /= 3_SIK .OR. &
          valsnk3a(1,2,1) /= -10_SNK .OR. valsnk3a(2,2,1) /= -10_SNK .OR. &
            valsnk3a(3,2,1) /= -20_SNK .OR. SIZE(valsnk3a,2) /= 2_SIK .OR. &
              SIZE(valsnk3a,3) /= 1_SIK .OR. &
                someParam%description /= 'The numbers 10, 10, 20, -10, -10, and -20') THEN
      WRITE(*,*) 'someParam%set(''testSNK3a'',(/10_SNK,10_SNK,20_SNK,'// &
        '-10_SNK,-10_SNK,-20_SNK/),''The numbers 10, 10, 20, -10, -10, and -20'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    CALL testParam%set('testSNK3a', &
      RESHAPE((/50_SNK,55_SNK,60_SNK,-50_SNK,-55_SNK,-60_SNK/),(/3,2,1/)), &
        'The numbers 50, 55, 60, -50, -55, and -60')
    CALL testParam%get('testSNK3a',valsnk3a)
    IF(valsnk3a(1,1,1) /= 50_SNK .OR. valsnk3a(2,1,1) /= 55_SNK .OR. &
        valsnk3a(3,1,1) /= 60_SNK .OR. SIZE(valsnk3a,1) /= 3_SIK .OR. &
          valsnk3a(1,2,1) /= -50_SNK .OR. valsnk3a(2,2,1) /= -55_SNK .OR. &
            valsnk3a(3,2,1) /= -60_SNK .OR. SIZE(valsnk3a,2) /= 2_SIK .OR. &
              SIZE(valsnk3a,3) /= 1_SIK .OR. &
              someParam%description /= 'The numbers 50, 55, 60, -50, -55, and -60') THEN
      WRITE(*,*) 'testParam%set(''testSNK3a'',(/50_SNK,55_SNK,60_SNK/,/-50_SNK,-55_SNK,-60_SNK/)) FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%set('testSNK3a',valsnk3a)
    CALL someParam%set('testError',valsnk3a)
    CALL testParam%set('testError',valsnk3a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SNK) 3-D'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SNK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SNK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SNK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SNK) 3-D FAILED!'
      STOP 666
    ENDIF

    
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SNK) 3-D'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSNK3a',RESHAPE((/4_SNK/),(/1,1,1/)) )
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SNK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSNK3a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SNK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '3-D ARRAY INTEGER(SNK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SNK) 3-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSNK3a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SNK) 3-D'
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSNK3a
!
!-------------------------------------------------------------------------------
!Test 3-D Array SLK support
  SUBROUTINE testSLK3a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSLK3a'
    ALLOCATE(valslk3a(2,2,2))
    valslk3a(1,1,:)=6_SLK
    valslk3a(2,1,:)=8_SLK
    valslk3a(1,2,:)=7_SLK
    valslk3a(2,2,:)=9_SLK
    !test init
    CALL testParam%init('testError->testSLK3a',valslk3a,'The numbers 6, 8, 7, & 9')
    eParams => NULL()
    CALL testParam%init('testSLK3a',valslk3a,'The numbers 6, 8, 7, & 9')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat (SLK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSLK3a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name (SLK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '3-D ARRAY INTEGER(SLK)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype (SLK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The numbers 6, 8, 7, & 9') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description (SLK) 3-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    eParams => e
    CALL testParam%init('testError',valslk3a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SLK) 3-D'
  
    !test get
    eParams => NULL()
    CALL testParam%get('testSLK3a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSLK3a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSLK3a',valslk3a)
    IF(ANY(valslk3a(1,1,:) /= 6_SLK) .OR. ANY(valslk3a(2,1,:) /= 8_SLK) .OR. &
        ANY(valslk3a(1,2,:) /= 7_SLK) .OR. ANY(valslk3a(2,2,:) /= 9_SLK) .OR. &
          SIZE(valslk3a,1) /= 2_SIK .OR. SIZE(valslk3a,2) /= 2_SIK .OR. &
            SIZE(valslk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSLK3a'',valslk3a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valslk3a)
    ALLOCATE(valslk3a(1,1,1))
    CALL someParam%get('testSLK3a',valslk3a)
    IF(ANY(valslk3a(1,1,:) /= 6_SLK) .OR. ANY(valslk3a(2,1,:) /= 8_SLK) .OR. &
        ANY(valslk3a(1,2,:) /= 7_SLK) .OR. ANY(valslk3a(2,2,:) /= 9_SLK) .OR. &
          SIZE(valslk3a,1) /= 2_SIK .OR. SIZE(valslk3a,2) /= 2_SIK .OR. &
            SIZE(valslk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSLK3a'',valslk3a) FAILED!'
      STOP 666
    ENDIF
    valslk3a=0_SLK
    CALL testParam%get('testSLK3a',valslk3a)
    IF(ANY(valslk3a(1,1,:) /= 6_SLK) .OR. ANY(valslk3a(2,1,:) /= 8_SLK) .OR. &
        ANY(valslk3a(1,2,:) /= 7_SLK) .OR. ANY(valslk3a(2,2,:) /= 9_SLK) .OR. &
          SIZE(valslk3a,1) /= 2_SIK .OR. SIZE(valslk3a,2) /= 2_SIK .OR. &
            SIZE(valslk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSLK3a'',valslk3a) FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam2%get('testSLK3a',valslk3a)
    CALL testParam%get('testError',valslk3a)
    CALL someParam%get('testError',valslk3a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SLK) 3-D'
  
    !test set
    eParams => NULL()
    CALL someParam%set('testSLK3a', &
      RESHAPE((/3_SLK,1_SLK,4_SLK,2_SLK,3_SLK,1_SLK,4_SLK,2_SLK/), &
        (/2,2,2/)),'The numbers 3, 1, 4 and 2')
    CALL testParam%get('testSLK3a',valslk3a)
    IF(ANY(valslk3a(1,1,:) /= 3_SLK) .OR. ANY(valslk3a(2,1,:) /= 1_SLK) .OR. &
        ANY(valslk3a(1,2,:) /= 4_SLK) .OR. ANY(valslk3a(2,2,:) /= 2_SLK) .OR. &
          SIZE(valslk3a,1) /= 2_SIK .OR. SIZE(valslk3a,2) /= 2_SIK .OR. &
            SIZE(valslk3a,3) /= 2_SIK .OR. &
              someParam%description /= 'The numbers 3, 1, 4 and 2') THEN
      WRITE(*,*) 'someParam%set(''testSLK3a'',(/3_SLK,1_SLK/,/4_SLK,2_SLK/),''The numbers 3, 1, 4 and 2'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    CALL testParam%set('testSLK3a',RESHAPE((/6_SLK/),(/1,1,1/)),'The number 6')
    CALL testParam%get('testSLK3a',valslk3a)
    IF(valslk3a(1,1,1) /= 6_SLK .OR. SIZE(valslk3a,1) /= 1_SIK .OR. &
        SIZE(valslk3a,2) /= 1_SIK .OR. SIZE(valslk3a,3) /= 1_SIK .OR. &
          someParam%description /= 'The number 6') THEN
      WRITE(*,*) 'testParam%set(''testSLK3a'',6_SLK) FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    CALL someParam%set('testSLK3a', &
      RESHAPE((/15_SLK,-15_SLK,20_SLK,-15_SLK,15_SLK,-20_SLK/), &
        (/3,2,1/)),'The numbers 15, -15, 20, -15, 15, and -20')
    CALL testParam%get('testSLK3a',valslk3a)
    IF(valslk3a(1,1,1) /= 15_SLK .OR. valslk3a(2,1,1) /= -15_SLK .OR. &
        valslk3a(3,1,1) /= 20_SLK .OR. SIZE(valslk3a,1) /= 3_SIK .OR. &
          valslk3a(1,2,1) /= -15_SLK .OR. valslk3a(2,2,1) /= 15_SLK .OR. &
            valslk3a(3,2,1) /= -20_SLK .OR. SIZE(valslk3a,2) /= 2_SIK .OR. &
              SIZE(valslk3a,3) /= 1_SIK .OR. &
              someParam%description /= 'The numbers 15, -15, 20, -15, 15, and -20') THEN
      WRITE(*,*) 'someParam%set(''testSLK3a'',(/15_SLK,-15_SLK,20_SLK,-15_SLK,'// &
        '15_SLK,-20_SLK/),''The numbers 15, -15, 20, -15, 15, and -20'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    CALL testParam%set('testSLK3a', &
      RESHAPE((/-50_SLK,-55_SLK,-60_SLK,50_SLK,55_SLK,60_SLK/),(/3,2,1/)), &
        'The numbers -50, -55, -60, 50, 55, and 60')
    CALL testParam%get('testSLK3a',valslk3a)
    IF(valslk3a(1,1,1) /= -50_SLK .OR. valslk3a(2,1,1) /= -55_SLK .OR. &
        valslk3a(3,1,1) /= -60_SLK .OR. SIZE(valslk3a,1) /= 3_SIK .OR. &
          valslk3a(1,2,1) /= 50_SLK .OR. valslk3a(2,2,1) /= 55_SLK .OR. &
            valslk3a(3,2,1) /= 60_SLK .OR. SIZE(valslk3a,2) /= 2_SIK .OR. &
              SIZE(valslk3a,3) /= 1_SIK .OR. &
              someParam%description /= 'The numbers -50, -55, -60, 50, 55, and 60') THEN
      WRITE(*,*) 'testParam%set(''testSLK3a'',(/-50_SLK,-55_SLK,-60_SLK,'// &
        '50_SLK,55_SLK,60_SLK/)) FAILED!'
      STOP 666
    ENDIF
    
    eParams => e
    CALL testParam2%set('testSLK3a',valslk3a)
    CALL someParam%set('testError',valslk3a)
    CALL testParam%set('testError',valslk3a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SLK) 3-D'
  
    !Test clear
    eParams => NULL()
    CALL testParam%clear()
    IF(LEN(testParam%name%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SLK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SLK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description%sPrint()) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SLK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SLK) 3-D FAILED!'
      STOP 666
    ENDIF

    
    eParams => e
    WRITE(*,*) '  Passed: CALL testParam%clear() (SLK) 3-D'
  
    !test assignment
    eParams => NULL()
    CALL testParam%init('testSLK3a',RESHAPE((/4_SLK/),(/1,1,1/)) )
    testParam2=testparam
    IF(.NOT.ASSOCIATED(testParam2%pdat)) THEN
      WRITE(*,*) 'ASSIGNMENT(=) %pdat (SLK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%name /= 'testSLK3a') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %name (SLK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam2%pdat%datatype /= '3-D ARRAY INTEGER(SLK)') THEN
      WRITE(*,*) 'ASSIGNMENT(=) %datatype (SLK) 3-D FAILED!'
      STOP 666
    ENDIF
    eParams => e
    CALL testParam%get('testSLK3a',someParam)
    someParam=testParam
    WRITE(*,*) '  Passed: ASSIGNMENT(=) (SLK) 3-D'
    !Clear the variables
    CALL testClear()
    
  ENDSUBROUTINE testSLK3a
!
ENDPROGRAM testParameterLists
