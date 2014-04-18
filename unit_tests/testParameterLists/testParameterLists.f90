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
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
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
  TYPE(StringType),ALLOCATABLE :: valstr2a(:,:)
  REAL(SSK),ALLOCATABLE :: valssk3a(:,:,:)
  REAL(SDK),ALLOCATABLE :: valsdk3a(:,:,:)
  INTEGER(SNK),ALLOCATABLE :: valsnk3a(:,:,:)
  INTEGER(SLK),ALLOCATABLE :: valslk3a(:,:,:)
  TYPE(ExceptionHandlerType),POINTER :: e
  
  TYPE(ParamType) :: testParam,testParam2,testParam3,testList(5),testList2(3)
  CLASS(ParamType),POINTER :: someParam
  
  CREATE_TEST('PARAMETER TYPES')
  
  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.TRUE.)
  CALL eParams%addSurrogate(e)
  
  REGISTER_SUBTEST('Scalar SBK',testSBK)
  REGISTER_SUBTEST('Scalar SNK',testSNK)
  REGISTER_SUBTEST('Scalar SLK',testSLK)
  REGISTER_SUBTEST('Scalar SSK',testSSK)
  REGISTER_SUBTEST('Scalar SDK',testSDK)
  REGISTER_SUBTEST('Scalar CHAR',testCHAR)
  REGISTER_SUBTEST('Scalar STR',testSTR)
  REGISTER_SUBTEST('1-D SBK',testSBK1a)
  REGISTER_SUBTEST('1-D SNK',testSNK1a)
  REGISTER_SUBTEST('1-D SLK',testSLK1a)
  REGISTER_SUBTEST('1-D SSK',testSSK1a)
  REGISTER_SUBTEST('1-D SDK',testSDK1a)
  REGISTER_SUBTEST('1-D STR',testSTR1a)
  REGISTER_SUBTEST('2-D SNK',testSNK2a)
  REGISTER_SUBTEST('2-D SLK',testSLK2a)
  REGISTER_SUBTEST('2-D SSK',testSSK2a)
  REGISTER_SUBTEST('2-D SDK',testSDK2a)
  REGISTER_SUBTEST('2-D STR',testSTR2a)
  REGISTER_SUBTEST('3-D SNK',testSNK3a)
  REGISTER_SUBTEST('3-D SLK',testSLK3a)
  REGISTER_SUBTEST('3-D SSK',testSSK3a)
  REGISTER_SUBTEST('3-D SDK',testSDK3a)
  REGISTER_SUBTEST('ParameterList',testParamListType)
  REGISTER_SUBTEST('%add(...)',testAdd)
  REGISTER_SUBTEST('%has(...)',testHas)
  REGISTER_SUBTEST('%getNextParam(...)',testGetNextParam)
  REGISTER_SUBTEST('%remove(...)',testRemove)
  REGISTER_SUBTEST('%validate(...)',testValidate)
  
  FINALIZE_TEST()
  CALL clear_test_vars()
  DEALLOCATE(e)
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!Test SBK support
  SUBROUTINE testSBK()
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    valsbk=.TRUE.
    !test init
    
    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSBK',.TRUE.,'The value is TRUE')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SBK'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSBK',.TRUE.,'The value is TRUE')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSBK','%pdat%name')
    ASSERT(testParam%pdat%datatype == 'LOGICAL(SBK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The value is TRUE','%pdat%description')
    CALL testParam%init('testError',.TRUE.)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SBK'// &
      ' - parameter is already initialized! Use set method!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'double init error')
    
    !Test clear
    COMPONENT_TEST('%clear()')
    CALL testParam%clear()
    ASSERT(LEN(testParam%name) == 0,'%name')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype')
    ASSERT(LEN(testParam%description) == 0,'%description')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat')
    
    COMPONENT_TEST('%edit(...)')
    CALL testParam%init('testSBK',.TRUE.,'The value is TRUE')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSBK',.TRUE.)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)
    
    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSBK',.TRUE.,'The value is TRUE')
    CALL testParam%get('testSBK',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSBK',valsbk)
    ASSERT(valsbk,'someParam valsbk')
    valsbk=.FALSE.
    CALL testParam%get('testSBK',valsbk)
    ASSERT(valsbk,'testParam valsbk')
    CALL testParam%get('testError',valsbk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SBK'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valsbk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SBK'// &
      ' - parameter name mismatch "testError" in "testSBK"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSBK'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSBK',valsbk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SBK'// &
      ' - parameter data type mismatch! Parameter type is test_type and'// &
      ' must be LOGICAL(SBK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()
  
    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSBK',.FALSE.,'The value is FALSE')
    CALL testParam%get('testSBK',valsbk)
    ASSERT(.NOT.valsbk,'valsbk')
    ASSERT(testParam%pdat%name == 'testSBK','%name')
    ASSERT(testParam%pdat%description == 'The value is FALSE','%description')
    CALL someParam%set('testSBK',.TRUE.,'The value is TRUE')
    ASSERT(someParam%name == 'testSBK','someParam%name')
    ASSERT(someParam%datatype == 'LOGICAL(SBK)','someParam%datatype')
    ASSERT(someParam%description == 'The value is TRUE','someParam%description')
    CALL someParam%set('testError',.FALSE.) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SBK -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSBK"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSBK',.TRUE.) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SBK -'// &
      ' unable to locate parameter "testSBK" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSBK'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSBK',.TRUE.) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SBK -'// &
      ' parameter data type mismatch! Parameter type is test_type'// &
      ' and must be LOGICAL(SBK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()
    
    COMPONENT_TEST('Operators')
    CALL testParam%init('testSBK',.TRUE.)
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSBK','%name')
    ASSERT(testParam2%pdat%datatype == 'LOGICAL(SBK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSBK
!
!-------------------------------------------------------------------------------
!Test SNK support
  SUBROUTINE testSNK()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSNK'
    valsnk=5_SNK
    !test init
    CALL testParam%init('testError->testSNK',valsnk,'The number 5')
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
    CALL testParam%init('testError',valsnk)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SNK)'
  
    !test get
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
    CALL testParam2%get('testSNK',valsnk)
    CALL testParam%get('testError',valsnk)
    CALL someParam%get('testError',valsnk)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SNK)'
  
    !test set
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
    CALL testParam2%set('testSNK',valsnk)
    CALL someParam%set('testError',valsnk)
    CALL testParam%set('testError',valsnk)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SNK)'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SNK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SNK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SNK) FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SNK) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL testParam%clear() (SNK)'
  
    !test assignment
    CALL testParam%init('testSNK',4_SNK)
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSNK','%name')
    ASSERT(testParam2%pdat%datatype == 'INTEGER(SNK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
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
    CALL testParam%init('testError',valslk)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SLK)'
  
    !test get
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
    CALL testParam2%get('testSLK',valslk)
    CALL testParam%get('testError',valslk)
    CALL someParam%get('testError',valslk)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SLK)'
  
    !test set
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
    CALL testParam2%set('testSLK',valslk)
    CALL someParam%set('testError',valslk)
    CALL testParam%set('testError',valslk)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SLK)'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SLK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SLK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SLK) FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SLK) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL testParam%clear() (SLK)'
  
    !test assignment
    CALL testParam%init('testSLK',4_SLK)
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSLK','%name')
    ASSERT(testParam2%pdat%datatype == 'INTEGER(SLK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSLK
!
!-------------------------------------------------------------------------------
!Test SSK support
  SUBROUTINE testSSK()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSSK'
    valssk=5._SSK
    !test init
    CALL testParam%init('testError->testSSK',valssk,'The number 5.0')
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
    CALL testParam%init('testError',valssk)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SSK)'
  
    !test get
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
    CALL testParam2%get('testSSK',valssk)
    CALL testParam%get('testError',valssk)
    CALL someParam%get('testError',valssk)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SSK)'
  
    !test set
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
    CALL testParam2%set('testSSK',valssk)
    CALL someParam%set('testError',valssk)
    CALL testParam%set('testError',valssk)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SSK)'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SSK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SSK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SSK) FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SSK) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL testParam%clear() (SSK)'
  
    !test assignment
    CALL testParam%init('testSSK',4.0_SSK)
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSSK','%name')
    ASSERT(testParam2%pdat%datatype == 'REAL(SSK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSSK
!
!-------------------------------------------------------------------------------
!Test SDK support
  SUBROUTINE testSDK()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSDK'
    valsdk=5._SDK
    !test init
    CALL testParam%init('testError->testSDK',valsdk,'The number 5.0')
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
    CALL testParam%init('testError',valsdk)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SDK)'
  
    !test get
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
    CALL testParam2%get('testSDK',valsdk)
    CALL testParam%get('testError',valsdk)
    CALL someParam%get('testError',valsdk)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SDK)'
  
    !test set
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
    CALL testParam2%set('testSDK',valsdk)
    CALL someParam%set('testError',valsdk)
    CALL testParam%set('testError',valsdk)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SDK)'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SDK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SDK) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SDK) FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SDK) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL testParam%clear() (SDK)'
  
    !test assignment
    CALL testParam%init('testSDK',4.0_SDK)
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSDK','%name')
    ASSERT(testParam2%pdat%datatype == 'REAL(SDK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSDK
!
!-------------------------------------------------------------------------------
!Test Character/StringType support
  SUBROUTINE testCHAR()
    CHARACTER(LEN=100) :: valchar
  
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSTR'
    CALL testParam%init('testError->testSTR','''testing''','The value is testing')
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
    CALL testParam%init('testSTR','''testing''')
    WRITE(*,*) '  Passed: CALL testParam%init(...) StringType (CHAR)'
  
    !test get
    valchar='test again'
    CALL testParam%get('testSTR',valchar)
    IF(TRIM(valchar) /= '''testing''') THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR'',valchar) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL testParam%get(...) StringType (CHAR)'
  
    !test set
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
    CALL clear_test_vars()
  ENDSUBROUTINE testCHAR
!
!-------------------------------------------------------------------------------
!Test StringType support
  SUBROUTINE testSTR()
    
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSTR'
    valstr='''testing'''
    !test init
    CALL testParam%init('testError->testSTR',valstr,'The value is testing')
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
    CALL testParam%init('testError',valstr)
    WRITE(*,*) '  Passed: CALL testParam%init(...) StringType (STR)'
  
    !test get
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
    CALL testParam2%get('testSTR',valstr)
    CALL testParam%get('testError',valstr)
    CALL someParam%get('testError',valstr)
    WRITE(*,*) '  Passed: CALL testParam%get(...) StringType (STR)'
  
    !test set
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
    CALL testParam2%set('testSTR',valstr)
    CALL someParam%set('testError',valstr)
    CALL testParam%set('testError',valstr)
    WRITE(*,*) '  Passed: CALL testParam%set(...) StringType (STR)'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name StringType (STR) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype StringType (STR) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description StringType (STR) FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat StringType (STR) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL testParam%clear() StringType (STR)'
  
    !test assignment
    valstr='assignment test'
    CALL testParam%init('testSTR',valstr)
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSTR','%name')
    ASSERT(testParam2%pdat%datatype == 'TYPE(StringType)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSTR
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
    CALL testParam%init('testError',valsbk1a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SBK) 1-D'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valsbk1a)
    CALL someParam%get('testSBK1a',valsbk1a)
    IF(.NOT.valsbk1a(1) .OR. valsbk1a(2)) THEN
      WRITE(*,*) 'CALL someParam%get(''testSBK1a'',valsbk1a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valsbk1a)
    CALL testParam%get('testSBK1a',valsbk1a)
    IF(.NOT.valsbk1a(1) .OR. valsbk1a(2)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSBK1a'',valsbk1a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSBK1a',valsbk1a)
    CALL testParam%get('testError',valsbk1a)
    CALL someParam%get('testError',valsbk1a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SBK) 1-D'
  
    !test set
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
    
    CALL testParam2%set('testSBK1a',valsbk1a)
    CALL someParam%set('testError',valsbk1a)
    CALL testParam%set('testError',valsbk1a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SBK) 1-D'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SBK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SBK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SBK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SBK) 1-D FAILED!'
      STOP 666
    ENDIF

    
    WRITE(*,*) '  Passed: CALL testParam%clear() (SBK) 1-D'
  
    !test assignment
    CALL testParam%init('testSBK1a',(/.TRUE./))
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSBK1a','%name')
    ASSERT(testParam2%pdat%datatype ==  '1-D ARRAY LOGICAL(SBK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSBK1a
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
    CALL testParam%init('testError',valsnk1a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SNK) 1-D'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valsnk1a)
    CALL someParam%get('testSNK1a',valsnk1a)
    IF(valsnk1a(1) /= 5_SNK .OR. valsnk1a(2) /= 7_SNK ) THEN
      WRITE(*,*) 'CALL someParam%get(''testSNK1a'',valsnk1a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valsnk1a)
    CALL testParam%get('testSNK1a',valsnk1a)
    IF(valsnk1a(1) /= 5_SNK .OR. valsnk1a(2) /= 7_SNK ) THEN
      WRITE(*,*) 'CALL testParam%get(''testSNK1a'',valsnk1a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSNK1a',valsnk1a)
    CALL testParam%get('testError',valsnk1a)
    CALL someParam%get('testError',valsnk1a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SNK) 1-D'
  
    !test set
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
    
    CALL testParam2%set('testSNK1a',valsnk1a)
    CALL someParam%set('testError',valsnk1a)
    CALL testParam%set('testError',valsnk1a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SNK) 1-D'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SNK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SNK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SNK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SNK) 1-D FAILED!'
      STOP 666
    ENDIF

    
    WRITE(*,*) '  Passed: CALL testParam%clear() (SNK) 1-D'
  
    !test assignment
    CALL testParam%init('testSNK1a',(/4_SNK/))
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSNK1a','%name')
    ASSERT(testParam2%pdat%datatype == '1-D ARRAY INTEGER(SNK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
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
    CALL testParam%init('testError',valslk1a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SLK) 1-D'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valslk1a)
    CALL someParam%get('testSLK1a',valslk1a)
    IF(valslk1a(1) /= 6_SLK .OR. valslk1a(2) /= 8_SLK ) THEN
      WRITE(*,*) 'CALL someParam%get(''testSLK1a'',valslk1a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valslk1a)
    CALL testParam%get('testSLK1a',valslk1a)
    IF(valslk1a(1) /= 6_SLK .OR. valslk1a(2) /= 8_SLK ) THEN
      WRITE(*,*) 'CALL testParam%get(''testSLK1a'',valslk1a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSLK1a',valslk1a)
    CALL testParam%get('testError',valslk1a)
    CALL someParam%get('testError',valslk1a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SLK) 1-D'
  
    !test set
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
    
    CALL testParam2%set('testSLK1a',valslk1a)
    CALL someParam%set('testError',valslk1a)
    CALL testParam%set('testError',valslk1a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SLK) 1-D'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SLK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SLK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SLK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SLK) 1-D FAILED!'
      STOP 666
    ENDIF

    
    WRITE(*,*) '  Passed: CALL testParam%clear() (SLK) 1-D'
  
    !test assignment
    CALL testParam%init('testSLK1a',(/4_SLK/))
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSLK1a','%name')
    ASSERT(testParam2%pdat%datatype == '1-D ARRAY INTEGER(SLK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSLK1a
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
    CALL testParam%init('testError',valssk1a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SSK) 1-D'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valssk1a)
    CALL someParam%get('testSSK1a',valssk1a)
    IF(valssk1a(1) /= 5.0_SSK .OR. valssk1a(2) /= 7.0_SSK ) THEN
      WRITE(*,*) 'CALL someParam%get(''testSSK1a'',valssk1a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valssk1a)
    CALL testParam%get('testSSK1a',valssk1a)
    IF(valssk1a(1) /= 5.0_SSK .OR. valssk1a(2) /= 7.0_SSK ) THEN
      WRITE(*,*) 'CALL testParam%get(''testSSK1a'',valssk1a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSSK1a',valssk1a)
    CALL testParam%get('testError',valssk1a)
    CALL someParam%get('testError',valssk1a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SSK) 1-D'
  
    !test set
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
    
    CALL testParam2%set('testSSK1a',valssk1a)
    CALL someParam%set('testError',valssk1a)
    CALL testParam%set('testError',valssk1a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SSK) 1-D'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SSK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SSK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SSK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SSK) 1-D FAILED!'
      STOP 666
    ENDIF

    
    WRITE(*,*) '  Passed: CALL testParam%clear() (SSK) 1-D'
  
    !test assignment
    CALL testParam%init('testSSK1a',(/4.0_SSK/))
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSSK1a','%name')
    ASSERT(testParam2%pdat%datatype == '1-D ARRAY REAL(SSK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
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
    CALL testParam%init('testError',valsdk1a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SDK) 1-D'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valsdk1a)
    CALL someParam%get('testSDK1a',valsdk1a)
    IF(valsdk1a(1) /= 5.5_SDK .OR. valsdk1a(2) /= 7.5_SDK ) THEN
      WRITE(*,*) 'CALL someParam%get(''testSDK1a'',valsdk1a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valsdk1a)
    CALL testParam%get('testSDK1a',valsdk1a)
    IF(valsdk1a(1) /= 5.5_SDK .OR. valsdk1a(2) /= 7.5_SDK ) THEN
      WRITE(*,*) 'CALL testParam%get(''testSDK1a'',valsdk1a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSDK1a',valsdk1a)
    CALL testParam%get('testError',valsdk1a)
    CALL someParam%get('testError',valsdk1a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SDK) 1-D'
  
    !test set
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
    
    CALL testParam2%set('testSDK1a',valsdk1a)
    CALL someParam%set('testError',valsdk1a)
    CALL testParam%set('testError',valsdk1a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SDK) 1-D'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SDK) 1-D FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL testParam%clear() (SDK) 1-D'
  
    !test assignment
    CALL testParam%init('testSDK1a',valsdk1a)
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSDK1a','%name')
    ASSERT(testParam2%pdat%datatype == '1-D ARRAY REAL(SDK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSDK1a
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
    CALL testParam%init('testError',valstr1a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) StringType (STR)'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valstr1a)
    CALL someParam%get('testSTR1a',valstr1a)
    IF(valstr1a(1) /= '''testing''' .OR. valstr1a(2) /= 'more testing' .OR. &
        SIZE(valstr1a) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR1a'',valstr1a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valstr1a)
    CALL testParam%get('testSTR1a',valstr1a)
    IF(valstr1a(1) /= '''testing''' .OR. valstr1a(2) /= 'more testing' .OR. &
        SIZE(valstr1a) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR1a'',valstr1a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSTR1a',valstr1a)
    CALL testParam%get('testError',valstr1a)
    CALL someParam%get('testError',valstr1a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) StringType (STR)'
  
    !test set
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
    CALL testParam2%set('testSTR1a',valstr1a)
    CALL someParam%set('testError',valstr1a)
    CALL testParam%set('testError',valstr1a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) StringType (STR)'
    
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat StringType (STR) 1-D FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL testParam%clear() StringType (STR)'
  
    !test assignment
    valstr1a(1)='assignment test1'
    valstr1a(2)='assignment test2'
    valstr1a(3)='assignment test3'
    CALL testParam%init('testSTR1a',valstr1a)
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSTR1a','%name')
    ASSERT(testParam2%pdat%datatype == '1-D ARRAY TYPE(StringType)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSTR1a
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
    CALL testParam%init('testError',valsnk2a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SNK) 2-D'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valsnk2a)
    CALL someParam%get('testSNK2a',valsnk2a)
    IF(valsnk2a(1,1) /= 5_SNK .OR. valsnk2a(2,1) /= 7_SNK .OR. &
        valsnk2a(1,2) /= 6_SNK .OR. valsnk2a(2,2) /= 8_SNK .OR. &
          SIZE(valsnk2a,1) /= 2_SIK .OR. SIZE(valsnk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSNK2a'',valsnk2a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valsnk2a)
    CALL testParam%get('testSNK2a',valsnk2a)
    IF(valsnk2a(1,1) /= 5_SNK .OR. valsnk2a(2,1) /= 7_SNK .OR. &
        valsnk2a(1,2) /= 6_SNK .OR. valsnk2a(2,2) /= 8_SNK .OR. &
          SIZE(valsnk2a,1) /= 2_SIK .OR. SIZE(valsnk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSNK2a'',valsnk2a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSNK2a',valsnk2a)
    CALL testParam%get('testError',valsnk2a)
    CALL someParam%get('testError',valsnk2a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SNK) 2-D'
  
    !test set
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
    
    CALL testParam2%set('testSNK2a',valsnk2a)
    CALL someParam%set('testError',valsnk2a)
    CALL testParam%set('testError',valsnk2a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SNK) 2-D'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SNK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SNK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SNK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SNK) 2-D FAILED!'
      STOP 666
    ENDIF

    
    WRITE(*,*) '  Passed: CALL testParam%clear() (SNK) 2-D'
  
    !test assignment
    CALL testParam%init('testSNK2a',RESHAPE((/4_SNK/),(/1,1/)) )
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSNK2a','%name')
    ASSERT(testParam2%pdat%datatype ==  '2-D ARRAY INTEGER(SNK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
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
    CALL testParam%init('testError',valslk2a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SLK) 2-D'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valslk2a)
    CALL someParam%get('testSLK2a',valslk2a)
    IF(valslk2a(1,1) /= 6_SLK .OR. valslk2a(2,1) /= 8_SLK .OR. &
        valslk2a(1,2) /= 7_SLK .OR. valslk2a(2,2) /= 9_SLK .OR. &
          SIZE(valslk2a,1) /= 2_SIK .OR. SIZE(valslk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSLK2a'',valslk2a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valslk2a)
    CALL testParam%get('testSLK2a',valslk2a)
    IF(valslk2a(1,1) /= 6_SLK .OR. valslk2a(2,1) /= 8_SLK .OR. &
        valslk2a(1,2) /= 7_SLK .OR. valslk2a(2,2) /= 9_SLK .OR. &
          SIZE(valslk2a,1) /= 2_SIK .OR. SIZE(valslk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSLK2a'',valslk2a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSLK2a',valslk2a)
    CALL testParam%get('testError',valslk2a)
    CALL someParam%get('testError',valslk2a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SLK) 2-D'
  
    !test set
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
    
    CALL testParam2%set('testSLK2a',valslk2a)
    CALL someParam%set('testError',valslk2a)
    CALL testParam%set('testError',valslk2a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SLK) 2-D'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SLK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SLK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SLK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SLK) 2-D FAILED!'
      STOP 666
    ENDIF

    
    WRITE(*,*) '  Passed: CALL testParam%clear() (SLK) 2-D'
  
    !test assignment
    CALL testParam%init('testSLK2a',RESHAPE((/4_SLK/),(/1,1/)) )
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSLK2a','%name')
    ASSERT(testParam2%pdat%datatype ==  '2-D ARRAY INTEGER(SLK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSLK2a
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
    CALL testParam%init('testError',valssk2a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SSK) 2-D'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valssk2a)
    CALL someParam%get('testSSK2a',valssk2a)
    IF(valssk2a(1,1) /= 5.0_SSK .OR. valssk2a(2,1) /= 7.0_SSK .OR. &
        valssk2a(1,2) /= 6.0_SSK .OR. valssk2a(2,2) /= 8.0_SSK .OR. &
          SIZE(valssk2a,1) /= 2_SIK .OR. SIZE(valssk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSSK2a'',valssk2a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valssk2a)
    CALL testParam%get('testSSK2a',valssk2a)
    IF(valssk2a(1,1) /= 5.0_SSK .OR. valssk2a(2,1) /= 7.0_SSK .OR. &
        valssk2a(1,2) /= 6.0_SSK .OR. valssk2a(2,2) /= 8.0_SSK .OR. &
          SIZE(valssk2a,1) /= 2_SIK .OR. SIZE(valssk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSSK2a'',valssk2a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSSK2a',valssk2a)
    CALL testParam%get('testError',valssk2a)
    CALL someParam%get('testError',valssk2a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SSK) 2-D'
  
    !test set
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
    
    CALL testParam2%set('testSSK2a',valssk2a)
    CALL someParam%set('testError',valssk2a)
    CALL testParam%set('testError',valssk2a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SSK) 2-D'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SSK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SSK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SSK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SSK) 2-D FAILED!'
      STOP 666
    ENDIF

    
    WRITE(*,*) '  Passed: CALL testParam%clear() (SSK) 2-D'
  
    !test assignment
    CALL testParam%init('testSSK2a',RESHAPE((/4.0_SSK/),(/1,1/)) )
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSSK2a','%name')
    ASSERT(testParam2%pdat%datatype ==  '2-D ARRAY REAL(SSK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
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
    CALL testParam%init('testError',valsdk2a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SDK) 2-D'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valsdk2a)
    CALL someParam%get('testSDK2a',valsdk2a)
    IF(valsdk2a(1,1) /= 5.5_SDK .OR. valsdk2a(2,1) /= 7.5_SDK .OR. &
        valsdk2a(1,2) /= 6.5_SDK .OR. valsdk2a(2,2) /= 8.5_SDK .OR. &
          SIZE(valsdk2a,1) /= 2_SIK .OR. SIZE(valsdk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSDK2a'',valsdk2a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valsdk2a)
    CALL testParam%get('testSDK2a',valsdk2a)
    IF(valsdk2a(1,1) /= 5.5_SDK .OR. valsdk2a(2,1) /= 7.5_SDK .OR. &
        valsdk2a(1,2) /= 6.5_SDK .OR. valsdk2a(2,2) /= 8.5_SDK .OR. &
          SIZE(valsdk2a,1) /= 2_SIK .OR. SIZE(valsdk2a,2) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSDK2a'',valsdk2a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSDK2a',valsdk2a)
    CALL testParam%get('testError',valsdk2a)
    CALL someParam%get('testError',valsdk2a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SDK) 2-D'
  
    !test set
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
    
    CALL testParam2%set('testSDK2a',valsdk2a)
    CALL someParam%set('testError',valsdk2a)
    CALL testParam%set('testError',valsdk2a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SDK) 2-D'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SDK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SDK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SDK) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SDK) 2-D FAILED!'
      STOP 666
    ENDIF

    
    WRITE(*,*) '  Passed: CALL testParam%clear() (SDK) 2-D'
  
    !test assignment
    CALL testParam%init('testSDK2a',RESHAPE((/4.0_SDK/),(/1,1/)) )
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSDK2a','%name')
    ASSERT(testParam2%pdat%datatype ==  '2-D ARRAY REAL(SDK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSDK2a
!
!-------------------------------------------------------------------------------
!Test 2-D array StringType support
  SUBROUTINE testSTR2a()
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSTR2a'
    ALLOCATE(valstr2a(2,2))
    valstr2a(1,1)='''testing'''
    valstr2a(1,2)='more testing'
    valstr2a(2,1)='''testing2'''
    valstr2a(2,2)='more testing2'
    !test init
    CALL testParam%init('testError->testSTR2a',valstr2a,'The value is testing and more testing')
    CALL testParam%init('testSTR2a',valstr2a,'The value is testing and more testing')
    IF(.NOT.ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%init(...) %pdat StringType (STR) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%name /= 'testSTR2a') THEN
      WRITE(*,*) 'CALL testParam%init(...) %name StringType (STR) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%datatype /= '2-D ARRAY TYPE(StringType)') THEN
      WRITE(*,*) 'CALL testParam%init(...) %datatype StringType (STR) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(testParam%pdat%description /= 'The value is testing and more testing') THEN
      WRITE(*,*) 'CALL testParam%init(...) %description StringType (STR) 2-D FAILED!'
      STOP 666
    ENDIF
    CALL testParam%edit(OUTPUT_UNIT,0) !test edit
    CALL testParam%init('testError',valstr2a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) StringType (STR)'
  
    !test get
    CALL testParam%get('testSTR2a',someParam)
    IF(.NOT.ASSOCIATED(someParam,testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR2a'',someParam) FAILED!'
      STOP 666
    ENDIF
    !Test same size
    CALL someParam%get('testSTR2a',valstr2a)
    IF(valstr2a(1,1) /= '''testing''' .OR. valstr2a(1,2) /= 'more testing' .OR. &
        valstr2a(2,1) /= '''testing2''' .OR. valstr2a(2,2) /= 'more testing2' .OR. &
        SIZE(valstr2a,1) /= 2_SIK .OR. SIZE(valstr2a,2) /= 2_SIK ) THEN
      WRITE(*,*) 'CALL someParam%get(''testSTR2a'',valstr2a) FAILED!'
      STOP 666
    ENDIF
    !Test different size size
    DEALLOCATE(valstr2a)
    ALLOCATE(valstr2a(1,1))
    valstr2a(1,1)=''
    CALL someParam%get('testSTR2a',valstr2a)
    IF(valstr2a(1,1) /= '''testing''' .OR. valstr2a(1,2) /= 'more testing' .OR. &
        valstr2a(2,1) /= '''testing2''' .OR. valstr2a(2,2) /= 'more testing2' .OR. &
        SIZE(valstr2a,1) /= 2_SIK .OR. SIZE(valstr2a,2) /= 2_SIK ) THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR2a'',valstr2a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valstr2a)
    ALLOCATE(valstr2a(1,10))
    !valstr1a(1)='test again'
    !valstr1a(2)='test again'
    CALL testParam%get('testSTR2a',valstr2a)
    IF(valstr2a(1,1) /= '''testing''' .OR. valstr2a(1,2) /= 'more testing' .OR. &
        valstr2a(2,1) /= '''testing2''' .OR. valstr2a(2,2) /= 'more testing2' .OR. &
        SIZE(valstr2a,1) /= 2_SIK .OR. SIZE(valstr2a,2) /= 2_SIK ) THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR2a'',valstr2a) FAILED!'
      STOP 666
    ENDIF
    !Test with deallocated array
    DEALLOCATE(valstr2a)
    CALL someParam%get('testSTR2a',valstr2a)
    IF(valstr2a(1,1) /= '''testing''' .OR. valstr2a(1,2) /= 'more testing' .OR. &
        valstr2a(2,1) /= '''testing2''' .OR. valstr2a(2,2) /= 'more testing2' .OR. &
        SIZE(valstr2a,1) /= 2_SIK .OR. SIZE(valstr2a,2) /= 2_SIK ) THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR2a'',valstr2a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valstr2a)
    CALL testParam%get('testSTR2a',valstr2a)
    IF(valstr2a(1,1) /= '''testing''' .OR. valstr2a(1,2) /= 'more testing' .OR. &
        valstr2a(2,1) /= '''testing2''' .OR. valstr2a(2,2) /= 'more testing2' .OR. &
        SIZE(valstr2a,1) /= 2_SIK .OR. SIZE(valstr2a,2) /= 2_SIK ) THEN
      WRITE(*,*) 'CALL testParam%get(''testSTR2a'',valstr2a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSTR2a',valstr2a)
    CALL testParam%get('testError',valstr2a)
    CALL someParam%get('testError',valstr2a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) StringType (STR)'
  
    !test set
    !For strings, they must be stored in a string type first, then passed in.
    valstr2a(1,1)='another test'
    valstr2a(1,2)='one more test'
    valstr2a(2,1)='another test2'
    valstr2a(2,2)='one more test2'
    CALL someParam%set('testSTR2a',valstr2a,'The value is another test and one more test')
    !Clear the variable to confirm it gets set.
    valstr2a(1,1)=''
    valstr2a(1,2)=''
    valstr2a(2,1)=''
    valstr2a(2,2)=''
    CALL testParam%get('testSTR2a',valstr2a)
    IF(valstr2a(1,1) /= 'another test' .OR. valstr2a(1,2) /= 'one more test' .OR.  &
        valstr2a(2,1) /= 'another test2' .OR. valstr2a(2,2) /= 'one more test2' .OR.  &
        SIZE(valstr2a,1) /= 2_SIK .OR. SIZE(valstr2a,2) /= 2_SIK .OR. &
        someParam%description /= 'The value is another test and one more test') THEN
      WRITE(*,*) 'someParam%set(''testSTR2a'',''valstr2a,'// &
        ' The value is another test and one more test'') FAILED!'
      STOP 666
    ENDIF
    !Different size for test param
    DEALLOCATE(valstr2a)
    ALLOCATE(valstr2a(1,2))
    valstr2a(1,1)='a different size test'
    valstr2a(1,2)='a different size test2'
    CALL testParam%set('testSTR2a',valstr2a,'The value is a different size test')
    DEALLOCATE(valstr2a)
    ALLOCATE(valstr2a(1,2))
    valstr2a(1,1)=''
    valstr2a(1,2)=''
    CALL testParam%get('testSTR2a',valstr2a)
    IF(valstr2a(1,1) /= 'a different size test' .OR. SIZE(valstr2a,1) /= 1_SIK .OR. &
        valstr2a(1,2) /= 'a different size test2' .OR. SIZE(valstr2a,2) /= 2_SIK .OR. &
          someParam%description /= 'The value is a different size test') THEN
      WRITE(*,*) 'someParam%set(''testSTR2a'',''valstr2a,'// &
        ' The value is a different size test'') FAILED!'
      STOP 666
    ENDIF
    !Different size for some param
    DEALLOCATE(valstr2a)
    ALLOCATE(valstr2a(3,1))
    valstr2a(1,1)='a same but different test'
    valstr2a(2,1)='a same but different test'
    valstr2a(3,1)='a same but different test'
    CALL someParam%set('testSTR2a',valstr2a,'The value is a same but different test')
    !Clear the variable to confirm it gets set.
    DEALLOCATE(valstr2a)
    ALLOCATE(valstr2a(1,2))
    valstr2a(1,1)=''
    valstr2a(1,2)=''
    CALL testParam%get('testSTR2a',valstr2a)
    IF(valstr2a(1,1) /= 'a same but different test' .OR. &
         valstr2a(2,1) /= 'a same but different test' .OR. &
           valstr2a(3,1) /= 'a same but different test' .OR. &
             SIZE(valstr2a,1) /= 3_SIK .OR. SIZE(valstr2a,2) /= 1_SIK .OR. &
               someParam%description /= 'The value is a same but different test') THEN
      WRITE(*,*) 'someParam%set(''testSTR2a'',''valstr2a,'// &
        ' The value is a same but different test'') FAILED!'
      STOP 666
    ENDIF
    !Same size for test param
    valstr2a(1,1)='a different but same test'
    valstr2a(2,1)='a different but same test'
    valstr2a(3,1)='a different but same test'
    CALL testParam%set('testSTR2a',valstr2a,'The value is a different but same test')
    valstr2a(1,1)=''
    valstr2a(2,1)=''
    valstr2a(3,1)=''
    CALL testParam%get('testSTR2a',valstr2a)
    IF(valstr2a(1,1) /= 'a different but same test' .OR. &
         valstr2a(2,1) /= 'a different but same test' .OR. &
           valstr2a(3,1) /= 'a different but same test' .OR. &
             SIZE(valstr2a,1) /= 3_SIK .OR. SIZE(valstr2a,2) /= 1_SIK .OR. &
               someParam%description /= 'The value is a different but same test') THEN
      WRITE(*,*) 'testParam%set(''testSTR2a'',''a different but same test'') FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%set('testSTR2a',valstr2a)
    CALL someParam%set('testError',valstr2a)
    CALL testParam%set('testError',valstr2a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) StringType (STR)'
    
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name StringType (STR) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype StringType (STR) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description StringType (STR) 2-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat StringType (STR) 2-D FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL testParam%clear() StringType (STR)'
  
    !test assignment
    valstr2a(1,1)='assignment test'
    valstr2a(2,1)='assignment test2'
    valstr2a(3,1)='assignment test3'
    CALL testParam%init('testSTR2a',valstr2a)
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSTR2a','%name')
    ASSERT(testParam2%pdat%datatype ==  '2-D ARRAY TYPE(StringType)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSTR2a
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
    CALL testParam%init('testError',valsnk3a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SNK) 3-D'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valsnk3a)
    CALL someParam%get('testSNK3a',valsnk3a)
    IF(ANY(valsnk3a(1,1,:) /= 5_SNK) .OR. ANY(valsnk3a(2,1,:) /= 7_SNK) .OR. &
        ANY(valsnk3a(1,2,:) /= 6_SNK) .OR. ANY(valsnk3a(2,2,:) /= 8_SNK) .OR. &
          SIZE(valsnk3a,1) /= 2_SIK .OR. SIZE(valsnk3a,2) /= 2_SIK .OR. &
            SIZE(valsnk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSNK3a'',valsnk3a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valsnk3a)
    CALL testParam%get('testSNK3a',valsnk3a)
    IF(ANY(valsnk3a(1,1,:) /= 5_SNK) .OR. ANY(valsnk3a(2,1,:) /= 7_SNK) .OR. &
        ANY(valsnk3a(1,2,:) /= 6_SNK) .OR. ANY(valsnk3a(2,2,:) /= 8_SNK) .OR. &
          SIZE(valsnk3a,1) /= 2_SIK .OR. SIZE(valsnk3a,2) /= 2_SIK .OR. &
            SIZE(valsnk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSNK3a'',valsnk3a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSNK3a',valsnk3a)
    CALL testParam%get('testError',valsnk3a)
    CALL someParam%get('testError',valsnk3a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SNK) 3-D'
  
    !test set
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
    
    CALL testParam2%set('testSNK3a',valsnk3a)
    CALL someParam%set('testError',valsnk3a)
    CALL testParam%set('testError',valsnk3a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SNK) 3-D'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SNK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SNK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SNK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SNK) 3-D FAILED!'
      STOP 666
    ENDIF

    
    WRITE(*,*) '  Passed: CALL testParam%clear() (SNK) 3-D'
  
    !test assignment
    CALL testParam%init('testSNK3a',RESHAPE((/4_SNK/),(/1,1,1/)) )
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSNK3a','%name')
    ASSERT(testParam2%pdat%datatype ==  '3-D ARRAY INTEGER(SNK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
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
    CALL testParam%init('testError',valslk3a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SLK) 3-D'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valslk3a)
    CALL someParam%get('testSLK3a',valslk3a)
    IF(ANY(valslk3a(1,1,:) /= 6_SLK) .OR. ANY(valslk3a(2,1,:) /= 8_SLK) .OR. &
        ANY(valslk3a(1,2,:) /= 7_SLK) .OR. ANY(valslk3a(2,2,:) /= 9_SLK) .OR. &
          SIZE(valslk3a,1) /= 2_SIK .OR. SIZE(valslk3a,2) /= 2_SIK .OR. &
            SIZE(valslk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSLK3a'',valslk3a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valslk3a)
    CALL testParam%get('testSLK3a',valslk3a)
    IF(ANY(valslk3a(1,1,:) /= 6_SLK) .OR. ANY(valslk3a(2,1,:) /= 8_SLK) .OR. &
        ANY(valslk3a(1,2,:) /= 7_SLK) .OR. ANY(valslk3a(2,2,:) /= 9_SLK) .OR. &
          SIZE(valslk3a,1) /= 2_SIK .OR. SIZE(valslk3a,2) /= 2_SIK .OR. &
            SIZE(valslk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSLK3a'',valslk3a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSLK3a',valslk3a)
    CALL testParam%get('testError',valslk3a)
    CALL someParam%get('testError',valslk3a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SLK) 3-D'
  
    !test set
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
    
    CALL testParam2%set('testSLK3a',valslk3a)
    CALL someParam%set('testError',valslk3a)
    CALL testParam%set('testError',valslk3a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SLK) 3-D'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SLK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SLK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SLK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SLK) 3-D FAILED!'
      STOP 666
    ENDIF

    
    WRITE(*,*) '  Passed: CALL testParam%clear() (SLK) 3-D'
  
    !test assignment
    CALL testParam%init('testSLK3a',RESHAPE((/4_SLK/),(/1,1,1/)) )
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSLK3a','%name')
    ASSERT(testParam2%pdat%datatype ==  '3-D ARRAY INTEGER(SLK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSLK3a
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
    CALL testParam%init('testError',valssk3a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SSK) 3-D'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valssk3a)
    CALL someParam%get('testSSK3a',valssk3a)
    IF(ANY(valssk3a(1,1,:) /= 5.0_SSK) .OR. ANY(valssk3a(2,1,:) /= 7.0_SSK) .OR. &
        ANY(valssk3a(1,2,:) /= 6.0_SSK) .OR. ANY(valssk3a(2,2,:) /= 8.0_SSK) .OR. &
          SIZE(valssk3a,1) /= 2_SIK .OR. SIZE(valssk3a,2) /= 2_SIK .OR. &
            SIZE(valssk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSSK3a'',valssk3a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valssk3a)
    CALL testParam%get('testSSK3a',valssk3a)
    IF(ANY(valssk3a(1,1,:) /= 5.0_SSK) .OR. ANY(valssk3a(2,1,:) /= 7.0_SSK) .OR. &
        ANY(valssk3a(1,2,:) /= 6.0_SSK) .OR. ANY(valssk3a(2,2,:) /= 8.0_SSK) .OR. &
          SIZE(valssk3a,1) /= 2_SIK .OR. SIZE(valssk3a,2) /= 2_SIK .OR. &
            SIZE(valssk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSSK3a'',valssk3a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSSK3a',valssk3a)
    CALL testParam%get('testError',valssk3a)
    CALL someParam%get('testError',valssk3a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SSK) 3-D'
  
    !test set
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
    
    CALL testParam2%set('testSSK3a',valssk3a)
    CALL someParam%set('testError',valssk3a)
    CALL testParam%set('testError',valssk3a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SSK) 3-D'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SSK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SSK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SSK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SSK) 3-D FAILED!'
      STOP 666
    ENDIF

    
    WRITE(*,*) '  Passed: CALL testParam%clear() (SSK) 3-D'
  
    !test assignment
    CALL testParam%init('testSSK3a',RESHAPE((/4.0_SSK/),(/1,1,1/)) )
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSSK3a','%name')
    ASSERT(testParam2%pdat%datatype ==  '3-D ARRAY REAL(SSK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
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
    CALL testParam%init('testError',valsdk3a)
    WRITE(*,*) '  Passed: CALL testParam%init(...) (SDK) 3-D'
  
    !test get
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
    !Test with deallocated array
    DEALLOCATE(valsdk3a)
    CALL someParam%get('testSDK3a',valsdk3a)
    IF(ANY(valsdk3a(1,1,:) /= 5.5_SDK) .OR. ANY(valsdk3a(2,1,:) /= 7.5_SDK) .OR. &
        ANY(valsdk3a(1,2,:) /= 6.5_SDK) .OR. ANY(valsdk3a(2,2,:) /= 8.5_SDK) .OR. &
          SIZE(valsdk3a,1) /= 2_SIK .OR. SIZE(valsdk3a,2) /= 2_SIK .OR. &
            SIZE(valsdk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL someParam%get(''testSDK3a'',valsdk3a) FAILED!'
      STOP 666
    ENDIF
    DEALLOCATE(valsdk3a)
    CALL testParam%get('testSDK3a',valsdk3a)
    IF(ANY(valsdk3a(1,1,:) /= 5.5_SDK) .OR. ANY(valsdk3a(2,1,:) /= 7.5_SDK) .OR. &
        ANY(valsdk3a(1,2,:) /= 6.5_SDK) .OR. ANY(valsdk3a(2,2,:) /= 8.5_SDK) .OR. &
          SIZE(valsdk3a,1) /= 2_SIK .OR. SIZE(valsdk3a,2) /= 2_SIK .OR. &
            SIZE(valsdk3a,3) /= 2_SIK) THEN
      WRITE(*,*) 'CALL testParam%get(''testSDK3a'',valsdk3a) FAILED!'
      STOP 666
    ENDIF
    CALL testParam2%get('testSDK3a',valsdk3a)
    CALL testParam%get('testError',valsdk3a)
    CALL someParam%get('testError',valsdk3a)
    WRITE(*,*) '  Passed: CALL testParam%get(...) (SDK) 3-D'
  
    !test set
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
    
    CALL testParam2%set('testSDK3a',valsdk3a)
    CALL someParam%set('testError',valsdk3a)
    CALL testParam%set('testError',valsdk3a)
    WRITE(*,*) '  Passed: CALL testParam%set(...) (SDK) 3-D'
  
    !Test clear
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (SDK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (SDK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (SDK) 3-D FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (SDK) 3-D FAILED!'
      STOP 666
    ENDIF

    
    WRITE(*,*) '  Passed: CALL testParam%clear() (SDK) 3-D'
  
    !test assignment
    CALL testParam%init('testSDK3a',RESHAPE((/4.0_SDK/),(/1,1,1/)) )
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSDK3a','%name')
    ASSERT(testParam2%pdat%datatype ==  '3-D ARRAY REAL(SDK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSDK3a
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
    CALL testParam%init('testError',testList,'A test parameter list')
    WRITE(*,*) '  Passed: CALL testParam%init(...) (List)'
  
    !Test get
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
    CALL testParam%clear()
    IF(LEN(testParam%name) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %name (List) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%datatype) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %datatype (List) FAILED!'
      STOP 666
    ENDIF
    IF(LEN(testParam%description) /= 0) THEN
      WRITE(*,*) 'CALL testParam%clear() %description (List) FAILED!'
      STOP 666
    ENDIF
    IF(ASSOCIATED(testParam%pdat)) THEN
      WRITE(*,*) 'CALL testParam%clear() %pdat (List) FAILED!'
      STOP 666
    ENDIF
    WRITE(*,*) '  Passed: CALL testParam%clear() (List)'
  
    !Test assignment
    CALL testParam%init('testPL',testList)
    COMPONENT_TEST('Operators')
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testPL','%name')
    ASSERT(testParam2%pdat%datatype == 'TYPE(ParamType_List)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testParamListType
!
!-------------------------------------------------------------------------------
  SUBROUTINE testAdd()
    INTEGER(SNK) :: snk2(2,2),snk3(2,2,2)
    INTEGER(SLK) :: slk2(2,2),slk3(2,2,2)
    REAL(SSK) :: ssk2(2,2),ssk3(2,2,2)
    REAL(SDK) :: sdk2(2,2),sdk3(2,2,2)
    TYPE(StringType) :: str1(2),str2(2,2)
  
    !Testing addition of SSK routine
    CALL testParam%add('testSSK',6.0_SSK)
    !Testing addition of SSK routine, error for already existing parameter
    CALL testParam%add('testSSK',9.0_SSK)
    !Testing addition of SSK routine, error for not a parameter list
    CALL testParam%add('testSSK2',7.0_SSK)
    CALL testParam%clear()
  
    !Testing addition of SSK routine to parameter list
    CALL testParam%add('testPL->testSSK',7.0_SSK)
  
    !Testing addition of multiple SSK parameters to parameter list
    CALL testParam%add('testPL->testSSK2',8.0_SSK)
  
    !Testing addition of SDK routine to parameter list
    CALL testParam%add('testPL->testSDK',1.0_SDK)
  
    !Testing addition of SNK routine to parameter list
    CALL testParam%add('testPL->testSNK',2_SNK)
  
    !Testing addition of SLK routine to parameter list
    CALL testParam%add('testPL->testSLK',3_SLK)
  
    !Testing addition of SBK routine to parameter list
    CALL testParam%add('testPL->testSBK',.TRUE.)
  
    !Testing addition of STR routine to parameter list
    CALL testParam%add('testPL->testSTR','string1')
  
    !Testing addition of 1-D array SSK routine to parameter list
    CALL testParam%add('testPL->testSSK1a',(/1.5_SSK,1.6_SSK/))
    
    !Testing addition of 1-D array SDK routine to parameter list
    CALL testParam%add('testPL->testSDK1a',(/2.5_SDK,2.6_SDK/))
    
    !Testing addition of 1-D array SNK routine to parameter list
    CALL testParam%add('testPL->testSNK1a',(/-2_SNK,-3_SNK/))
    
    !Testing addition of 1-D array SLK routine to parameter list
    CALL testParam%add('testPL->testSLK1a',(/-4_SLK,-5_SLK/))
    
    !Testing addition of 1-D array SBK routine to parameter list
    CALL testParam%add('testPL->testSBK1a',(/.TRUE.,.FALSE./))
    
    !Testing addition of 1-D array STR routine to parameter list
    str1(1)='stringarray1'
    str1(2)='stringarray2'
    CALL testParam%add('testPL->testSTR1a',str1)
    
    !Testing addition of 2-D array SSK routine to parameter list
    ssk2(1,1)=1.1_SSK
    ssk2(2,1)=2.1_SSK
    ssk2(1,2)=1.2_SSK
    ssk2(2,2)=2.2_SSK
    CALL testParam%add('testPL->testSSK2a',ssk2)
    
    !Testing addition of 2-D array SDK routine to parameter list
    sdk2(1,1)=11.0_SDK
    sdk2(2,1)=21.0_SDK
    sdk2(1,2)=12.0_SDK
    sdk2(2,2)=22.0_SDK
    CALL testParam%add('testPL->testSDK2a',sdk2)
    
    !Testing addition of 2-D array SNK routine to parameter list
    snk2(1,1)=11
    snk2(2,1)=21
    snk2(1,2)=12
    snk2(2,2)=22
    CALL testParam%add('testPL->testSNK2a',snk2)
    
    !Testing addition of 2-D array SLK routine to parameter list
    slk2(1,1)=110
    slk2(2,1)=210
    slk2(1,2)=120
    slk2(2,2)=220
    CALL testParam%add('testPL->testSLK2a',slk2)
    
    !Testing addition of 2-D array STR routine to parameter list
    str2(1,1)='stringarray1'
    str2(2,1)='stringarray2'
    str2(1,2)='stringarray3'
    str2(2,2)='stringarray4'
    CALL testParam%add('testPL->testSTR2a',str2)
    
    !Testing addition of 3-D array SSK routine to parameter list
    ssk3(1,1,1)=1.11_SSK
    ssk3(2,1,1)=2.11_SSK
    ssk3(1,2,1)=1.21_SSK
    ssk3(2,2,1)=2.21_SSK
    ssk3(1,1,2)=1.12_SSK
    ssk3(2,1,2)=2.12_SSK
    ssk3(1,2,2)=1.22_SSK
    ssk3(2,2,2)=2.22_SSK
    CALL testParam%add('testPL->testSSK3a',ssk3)
    
    !Testing addition of 3-D array SDK routine to parameter list
    sdk3(1,1,1)=11.1_SDK
    sdk3(2,1,1)=21.1_SDK
    sdk3(1,2,1)=12.1_SDK
    sdk3(2,2,1)=22.1_SDK
    sdk3(1,1,2)=11.2_SDK
    sdk3(2,1,2)=21.2_SDK
    sdk3(1,2,2)=12.2_SDK
    sdk3(2,2,2)=22.2_SDK
    CALL testParam%add('testPL->testSDK3a',sdk3)
    
    !Testing addition of 3-D array SNK routine to parameter list
    snk3(1,1,1)=111
    snk3(2,1,1)=211
    snk3(1,2,1)=121
    snk3(2,2,1)=221
    snk3(1,1,2)=112
    snk3(2,1,2)=212
    snk3(1,2,2)=122
    snk3(2,2,2)=222
    CALL testParam%add('testPL->testSNK3a',valsnk3a)
    
    !Testing addition of 3-D array SLK routine to parameter list
    slk3(1,1,1)=1110
    slk3(2,1,1)=2110
    slk3(1,2,1)=1210
    slk3(2,2,1)=2210
    slk3(1,1,2)=1120
    slk3(2,1,2)=2120
    slk3(1,2,2)=1220
    slk3(2,2,2)=2220
    CALL testParam%add('testPL->testSLK3a',slk3)
    
    
    !!Testing addition of SSK routine to parameter list with description
    !CALL testParam%add('testPL2->testSSK',9.0_SSK,'Creates a new sublist')
    !
    !!Testing addition of SDK routine to parameter list with description
    !CALL testParam%add('testPL2->testSDK',2.0_SDK,'Creates a new sublist')
    !
    !!Testing addition of SNK routine to parameter list with description
    !CALL testParam%add('testPL2->testSNK',3_SNK,'Creates a new sublist')
    !
    !!Testing addition of SLK routine to parameter list with description
    !CALL testParam%add('testPL2->testSLK',4_SLK,'Creates a new sublist')
    !
    !!Testing addition of SBK routine to parameter list with description
    !CALL testParam%add('testPL2->testSBK',.FALSE.,'Creates a new sublist')
    !
    !!Testing addition of STR routine to parameter list with description
    !valstr='string3'
    !CALL testParam%add('testPL2->testSTR',valstr,'Creates a new sublist')
    !
    !!Testing addition of 1-D array SSK routine to parameter list with description
    !valssk1a=2.5_SSK
    !CALL testParam%add('testPL2->testSSK1a',valssk1a,'Creates a new sublist')
    !
    !!Testing addition of 1-D array SDK routine to parameter list with description
    !valsdk1a=4.5_SDK
    !CALL testParam%add('testPL2->testSDK1a',valsdk1a,'Creates a new sublist')
    !
    !!Testing addition of 1-D array SNK routine to parameter list with description
    !valsnk1a=123_SNK
    !CALL testParam%add('testPL2->testSNK1a',valsnk1a,'Creates a new sublist')
    !
    !!Testing addition of 1-D array SLK routine to parameter list with description
    !valslk1a=-1230_SLK
    !CALL testParam%add('testPL2->testSLK1a',valslk1a,'Creates a new sublist')
    !
    !!Testing addition of 1-D array SBK routine to parameter list with description
    !valsbk1a=.FALSE.
    !CALL testParam%add('testPL2->testSBK1a',valsbk1a,'Creates a new sublist')
    !
    !!Testing addition of 1-D array STR routine to parameter list with description
    !valstr1a(1)='yet another stringarray1'
    !valstr1a(2)='yet another stringarray2'
    !CALL testParam%add('testPL2->testSTR1a',valstr1a,'Creates a new sublist')
    !
    !!Testing addition of 2-D array SSK routine to parameter list with description
    !valssk2a=2.5_SSK
    !CALL testParam%add('testPL2->testSSK2a',valssk2a,'Creates a new sublist')
    !
    !!Testing addition of 2-D array SDK routine to parameter list with description
    !valsdk2a=4.5_SDK
    !CALL testParam%add('testPL2->testSDK2a',valsdk2a,'Creates a new sublist')
    !
    !!Testing addition of 2-D array SNK routine to parameter list with description
    !valsnk2a=123_SNK
    !CALL testParam%add('testPL2->testSNK2a',valsnk2a,'Creates a new sublist')
    !
    !!Testing addition of 2-D array SLK routine to parameter list with description
    !valslk2a=-1230_SLK
    !CALL testParam%add('testPL2->testSLK2a',valslk2a,'Creates a new sublist')
    !
    !!Testing addition of 1-D array STR routine to parameter list with description
    !valstr2a(1,1)='yet another stringarray1'
    !valstr2a(1,2)='yet another stringarray2'
    !valstr2a(2,1)='yet another stringarray3'
    !valstr2a(2,2)='yet another stringarray4'
    !CALL testParam%add('testPL2->testSTR2a',valstr2a,'Creates a new sublist')
    !
    !!Testing addition of 3-D array SSK routine to parameter list with description
    !valssk3a=2.5_SSK
    !CALL testParam%add('testPL2->testSSK3a',valssk3a,'Creates a new sublist')
    !
    !!Testing addition of 3-D array SDK routine to parameter list with description
    !valsdk3a=4.5_SDK
    !CALL testParam%add('testPL2->testSDK3a',valsdk3a,'Creates a new sublist')
    !
    !!Testing addition of 3-D array SNK routine to parameter list with description
    !valsnk3a=123_SNK
    !CALL testParam%add('testPL2->testSNK3a',valsnk3a,'Creates a new sublist')
    !
    !!Testing addition of 3-D array SLK routine to parameter list with description
    !valslk3a=-1230_SLK
    !CALL testParam%add('testPL2->testSLK3a',valslk3a,'Creates a new sublist')

    !
    !Calling %verify here to check all of the possible PL types
    !
    !testParam2=testParam
    !CALL testParam%verify(testParam2,valsbk)
    !CALL testParam2%clear()
    !!Testing the local exception handler while adding a parameter to another parameter
    !CALL testParam2%add('testPL3->sublist1',testParam)
    CALL clear_test_vars()
  ENDSUBROUTINE testAdd
!
!-------------------------------------------------------------------------------
  SUBROUTINE testHas()
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    
    ASSERT(.NOT.testParam%has('testPL->error->'),'testPL->error->')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::has_ParamType'// &
      ' - cannot search for a blank name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'')
    
    CALL testParam%add('testPL->testSDK',0.2_SDK)
    ASSERT(testParam%has('testPL'),'testPL')
    ASSERT(testParam%has('testPL->testSDK'),'testPL->testSDK')
    ASSERT(.NOT.testParam%has('testPL->shenanigans'),'shenanigans')
    CALL clear_test_vars()
  ENDSUBROUTINE testHas
!
!-------------------------------------------------------------------------------
  SUBROUTINE testGetNextParam()
    INTEGER(SIK) :: n
    TYPE(StringType) :: addr,refaddr(6),refname(6)
    CLASS(ParamType),POINTER :: iParam
    
    refaddr(1)='level 1'
    refaddr(2)='level 1->level 2'
    refaddr(3)='level 1->level 2->val 1'
    refaddr(4)='level 1->level 2->level 3'
    refaddr(5)='level 1->level 2->val 2'
    refaddr(6)='level 1->val 3'
    
    refname(1)='level 1'
    refname(2)='level 2'
    refname(3)='val 1'
    refname(4)='level 3'
    refname(5)='val 2'
    refname(6)='val 3'
    
    CALL testParam%add('level 1->level 2->val 1',1.0)
    CALL testParam%add('level 1->val 3',3.0)
    CALL testParam%add('level 1->level 2->level 3->dummy',0.0)
    CALL testParam%remove('level 1->level 2->level 3->dummy')
    CALL testParam%add('level 1->level 2->val 2',2.0)
    
    n=0
    CALL testParam%getNextParam(addr,iParam)
    DO WHILE(ASSOCIATED(iParam))
      n=n+1
      ASSERT(refaddr(n) == addr,'addr')
      FINFO() n,'"'//TRIM(refaddr(n))//'" "'//TRIM(addr)//'"'
      ASSERT(refname(n) == iParam%name,'addr')
      FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%name)//'"'
      CALL testParam%getNextParam(addr,iParam)
    ENDDO
    ASSERT(n == 6,'number of iters')
  ENDSUBROUTINE testGetNextParam
!
!-------------------------------------------------------------------------------
  SUBROUTINE testRemove()
    !Setup list for test
    
    
    !!Testing removal of parameter that doesn't exist
    !CALL testParam%remove('testSSK3')
    !
    !!Testing removal of parameter that is erroneous, need to have a value before '->'
    !CALL testParam%remove('->error')
    !!Testing the SSK removal routine
    !CALL testParam%remove('testSSK')
    !
    !!Testing the SSK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSSK2')
    !
    !!Testing the SDK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSDK')
    !
    !!Testing the SNK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSNK')
    !
    !!Testing the SLK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSLK')
    !
    !!Testing the SBK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSBK')
    !
    !!Testing the STR removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSTR')
    !
    !!Testing the 1-D array SSK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSSK1a')
    !
    !!Testing the 1-D array SDK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSDK1a')
    !
    !!Testing the 1-D array SNK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSNK1a')
    !
    !!Testing the 1-D array SLK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSLK1a')
    !
    !!Testing the 1-D array SBK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSBK1a')
    !
    !!Testing the 1-D array STR removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSTR1a')
    !
    !!Testing the 2-D array SSK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSSK2a')
    !
    !!Testing the 2-D array SDK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSDK2a')
    !
    !!Testing the 2-D array SNK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSNK2a')
    !
    !!Testing the 2-D array SLK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSLK2a')
    !
    !!Testing the 2-D array STR removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSTR2a')
    !
    !!Testing the 3-D array SSK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSSK3a')
    !
    !!Testing the 3-D array SDK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSDK3a')
    !
    !!Testing the 3-D array SNK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSNK3a')
    !
    !!Testing the 3-D array SLK removal routine in a parameter list (with description)
    !CALL testParam%remove('testPL2->testSLK3a')
    !
    !!Testing the parameter list removal routine
    !CALL testParam%remove('testPL2')
    !
    !!Testing the SSK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSSK2')
    !
    !!Testing the SDK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSDK')
    !
    !!Testing the SNK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSNK')
    !
    !!Testing the SLK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSLK')
    !
    !!Testing the SBK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSBK')
    !
    !!Testing the STR removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSTR')
    !
    !!Testing the 1-D array SSK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSSK1a')
    !
    !!Testing the 1-D array SDK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSDK1a')
    !
    !!Testing the 1-D array SNK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSNK1a')
    !
    !!Testing the 1-D array SLK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSLK1a')
    !
    !!Testing the 1-D array SBK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSBK1a')
    !
    !!Testing the 1-D array STR removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSTR1a')
    !
    !!Testing the 2-D array SSK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSSK2a')
    !
    !!Testing the 2-D array SDK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSDK2a')
    !
    !!Testing the 2-D array SNK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSNK2a')
    !
    !!Testing the 2-D array SLK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSLK2a')
    !
    !!Testing the 2-D array STR removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSTR2a')
    !
    !!Testing the 3-D array SSK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSSK3a')
    !
    !!Testing the 3-D array SDK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSDK3a')
    !
    !!Testing the 3-D array SNK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSNK3a')
    !
    !!Testing the 3-D array SLK removal routine in a parameter list 
    !CALL testParam%remove('testPL->testSLK3a')
    !
  ENDSUBROUTINE testRemove
!
!-------------------------------------------------------------------------------
  SUBROUTINE testValidate()
    !Setup reference list and test validation subroutines
    CALL testParam%add('TestReq->p1',0.0_SSK)
    !test the null case for both params.
    CALL testParam%validate(testParam2,testParam3)
    !Testing the assignment operation
    testParam3=testParam
    !test the null required param, but existing optional param
    CALL testParam%validate(testParam2,testParam3)
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
    CALL clear_test_vars()
    
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
    CALL testParam%remove('TestOpt->p2->2far->veryfar')
    CALL testParam%add('TestOpt->p2->2far->veryfar',3.0_SSK)
    !Testing an optional param list against a parameter type (lines 1400-1410)
    CALL testParam%validate(testParam2,testParam3)
    CALL testParam%remove('TestOpt->p2->2far->veryfar')
    !Testing an unassociated test param from the remove statement above (lines 1388-1396)
    CALL testParam%validate(testParam2,testParam3)
    
    CALL clear_test_vars()
  ENDSUBROUTINE testValidate
!
!-------------------------------------------------------------------------------
!Clear all the test variables
  SUBROUTINE clear_test_vars()
    
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
  
  ENDSUBROUTINE clear_test_vars
!
ENDPROGRAM testParameterLists
