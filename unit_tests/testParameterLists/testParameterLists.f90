!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testParameterLists
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV
  USE UnitTest
  USE IntrType
  USE Strings
  USE ExceptionHandler
  USE ParameterLists
#ifdef FUTILITY_HAVE_Trilinos
  USE ForTeuchos_ParameterList
  USE ISO_C_BINDING
#endif

  IMPLICIT NONE

  REAL(SSK) :: valssk
  REAL(SDK) :: valsdk
  INTEGER(SNK) :: valsnk
  INTEGER(SLK) :: valslk
  LOGICAL(SBK) :: valsbk
  CHARACTER(LEN=100) :: valchar
  CHARACTER(:),ALLOCATABLE :: valchar1
  TYPE(StringType) :: valstr
  REAL(SSK),ALLOCATABLE :: valsska1(:)
  REAL(SDK),ALLOCATABLE :: valsdka1(:)
  INTEGER(SNK),ALLOCATABLE :: valsnka1(:)
  INTEGER(SLK),ALLOCATABLE :: valslka1(:)
  LOGICAL(SBK),ALLOCATABLE :: valsbka1(:)
  TYPE(StringType),ALLOCATABLE :: valstra1(:)
  REAL(SSK),ALLOCATABLE :: valsska2(:,:)
  REAL(SDK),ALLOCATABLE :: valsdka2(:,:)
  INTEGER(SNK),ALLOCATABLE :: valsnka2(:,:)
  INTEGER(SLK),ALLOCATABLE :: valslka2(:,:)
  TYPE(StringType),ALLOCATABLE :: valstra2(:,:)
  REAL(SSK),ALLOCATABLE :: valsska3(:,:,:)
  REAL(SDK),ALLOCATABLE :: valsdka3(:,:,:)
  INTEGER(SNK),ALLOCATABLE :: valsnka3(:,:,:)
  INTEGER(SLK),ALLOCATABLE :: valslka3(:,:,:)
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
  REGISTER_SUBTEST('1-D SBK',testSBKa1)
  REGISTER_SUBTEST('1-D SNK',testSNKa1)
  REGISTER_SUBTEST('1-D SLK',testSLKa1)
  REGISTER_SUBTEST('1-D SSK',testSSKa1)
  REGISTER_SUBTEST('1-D SDK',testSDKa1)
  REGISTER_SUBTEST('1-D STR',testSTRa1)
  REGISTER_SUBTEST('2-D SNK',testSNKa2)
  REGISTER_SUBTEST('2-D SLK',testSLKa2)
  REGISTER_SUBTEST('2-D SSK',testSSKa2)
  REGISTER_SUBTEST('2-D SDK',testSDKa2)
  REGISTER_SUBTEST('2-D STR',testSTRa2)
  REGISTER_SUBTEST('3-D SNK',testSNKa3)
  REGISTER_SUBTEST('3-D SLK',testSLKa3)
  REGISTER_SUBTEST('3-D SSK',testSSKa3)
  REGISTER_SUBTEST('3-D SDK',testSDKa3)
  REGISTER_SUBTEST('ParameterList',testParamListType)
  REGISTER_SUBTEST('%add(...)',testAdd)
  REGISTER_SUBTEST('%getString(...)',testGetString)
  REGISTER_SUBTEST('%has(...)',testHas)
  REGISTER_SUBTEST('%convertTo2DStringArray(...)',testConvertTo2DStringArray)
  REGISTER_SUBTEST('%remove(...)',testRemove)
  REGISTER_SUBTEST('%getNextParam(...)',testGetNextParam)
  REGISTER_SUBTEST('%getSubParams(...)',testGetSubParams)
  REGISTER_SUBTEST('%getSubPL(...)',testGetSubPL)
  REGISTER_SUBTEST('%validate(...)',testValidate)
  REGISTER_SUBTEST('%verify(...)',testVerify)
  REGISTER_SUBTEST('%verifyList(...)',testVerifyList)
  REGISTER_SUBTEST('Partial Matching',testPartialMatch)

  REGISTER_SUBTEST('%initFromXML',testInitFromXML)
  REGISTER_SUBTEST('%editToXML',testEditToXML)

#ifdef FUTILITY_HAVE_Trilinos
  REGISTER_SUBTEST('Convert to Teuchos', testConvertTeuchos)
#endif

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
    CALL testParam%init('testError->testSBK',valsbk,'The value is TRUE')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SBK'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSBK',valsbk,'The value is TRUE')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSBK','%pdat%name')
    ASSERT(testParam%pdat%datatype == 'LOGICAL(SBK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The value is TRUE','%pdat%description')
    CALL testParam%init('testError',valsbk)
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
    CALL testParam%init('testSBK',valsbk,'The value is TRUE')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSBK',.TRUE.)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSBK',valsbk,'The value is TRUE')
    CALL testParam%get('testSBK->no subparam',someParam)
    ASSERT(.NOT.ASSOCIATED(someParam),'no subparam')
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
      ' - parameter data type mismatch! Parameter testSBK type is test_type and'// &
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
    CALL someParam%get('testSBK',valsbk)
    ASSERT(valsbk,'valsbk')
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
      ' parameter data type mismatch! Parameter testSBK type is test_type'// &
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
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    valsnk=5_SNK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSNK',valsnk,'The number 5')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SNK'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSNK',valsnk,'The number 5')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSNK','%pdat%name')
    ASSERT(testParam%pdat%datatype == 'INTEGER(SNK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The number 5','%pdat%description')
    CALL testParam%init('testError',valsnk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SNK'// &
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
    CALL testParam%init('testSNK',valsnk,'The number 5')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSNK',valsnk)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSNK',valsnk,'The number 5')
    CALL testParam%get('testSNK',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSNK',valsnk)
    ASSERT(valsnk == 5,'someParam valsnk')
    valsnk=0
    CALL testParam%get('testSNK',valsnk)
    ASSERT(valsnk == 5,'testParam valsnk')
    CALL testParam%get('testError',valsnk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SNK'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valsnk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SNK'// &
      ' - parameter name mismatch "testError" in "testSNK"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSNK'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSNK',valsnk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SNK'// &
      ' - parameter data type mismatch! Parameter testSNK type is test_type and'// &
      ' must be INTEGER(SNK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    valsnk=-4
    CALL testParam%set('testSNK',valsnk,'The number -4')
    CALL testParam%get('testSNK',valsnk)
    ASSERT(valsnk == -4,'valsnk')
    ASSERT(testParam%pdat%name == 'testSNK','%name')
    ASSERT(testParam%pdat%description == 'The number -4','%description')
    valsnk=3
    CALL someParam%set('testSNK',valsnk,'The number 3')
    CALL someParam%get('testSNK',valsnk)
    ASSERT(valsnk == 3,'valsnk')
    ASSERT(someParam%name == 'testSNK','someParam%name')
    ASSERT(someParam%datatype == 'INTEGER(SNK)','someParam%datatype')
    ASSERT(someParam%description == 'The number 3','someParam%description')
    CALL someParam%set('testError',valsnk) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SNK -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSNK"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSNK',valsnk) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SNK -'// &
      ' unable to locate parameter "testSNK" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSNK'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSNK',valsnk) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SNK -'// &
      ' parameter data type mismatch! Parameter testSNK type is test_type'// &
      ' and must be INTEGER(SNK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    valsnk=4
    CALL testParam%init('testSNK',valsnk)
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
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    valslk=5_SLK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSLK',valslk,'The number 5')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SLK'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSLK',valslk,'The number 5')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSLK','%pdat%name')
    ASSERT(testParam%pdat%datatype == 'INTEGER(SLK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The number 5','%pdat%description')
    CALL testParam%init('testError',valslk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SLK'// &
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
    CALL testParam%init('testSLK',valslk,'The number 5')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSLK',valslk)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSLK',valslk,'The number 5')
    CALL testParam%get('testSLK',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSLK',valslk)
    ASSERT(valslk == 5_SLK,'someParam valslk')
    valslk=0
    CALL testParam%get('testSLK',valslk)
    ASSERT(valslk == 5_SLK,'testParam valslk')
    CALL testParam%get('testError',valslk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SLK'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valslk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SLK'// &
      ' - parameter name mismatch "testError" in "testSLK"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSLK'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSLK',valslk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SLK'// &
      ' - parameter data type mismatch! Parameter testSLK type is test_type and'// &
      ' must be INTEGER(SLK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    valslk=-4_SLK
    CALL testParam%set('testSLK',valslk,'The number -4')
    CALL testParam%get('testSLK',valslk)
    ASSERT(valslk == -4_SLK,'valslk')
    ASSERT(testParam%pdat%name == 'testSLK','%name')
    ASSERT(testParam%pdat%description == 'The number -4','%description')
    valslk=3_SLK
    CALL someParam%set('testSLK',valslk,'The number 3')
    CALL someParam%get('testSLK',valslk)
    ASSERT(valslk == 3_SLK,'valslk')
    ASSERT(someParam%name == 'testSLK','someParam%name')
    ASSERT(someParam%datatype == 'INTEGER(SLK)','someParam%datatype')
    ASSERT(someParam%description == 'The number 3','someParam%description')
    CALL someParam%set('testError',valslk) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SLK -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSLK"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSLK',valslk) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SLK -'// &
      ' unable to locate parameter "testSLK" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSLK'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSLK',valslk) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SLK -'// &
      ' parameter data type mismatch! Parameter testSLK type is test_type'// &
      ' and must be INTEGER(SLK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    valslk=4_SLK
    CALL testParam%init('testSLK',valslk)
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
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    valssk=5._SSK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSSK',valssk,'The number 5.0')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SSK'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSSK',valssk,'The number 5.0')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSSK','%pdat%name')
    ASSERT(testParam%pdat%datatype == 'REAL(SSK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The number 5.0','%pdat%description')
    CALL testParam%init('testError',valssk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SSK'// &
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
    CALL testParam%init('testSSK',valssk,'The number 5.0')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSSK',valssk)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSSK',valssk,'The number 5.0')
    CALL testParam%get('testSSK',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSSK',valssk)
    ASSERT(valssk == 5._SSK,'someParam valssk')
    valssk=0
    CALL testParam%get('testSSK',valssk)
    ASSERT(valssk == 5._SSK,'testParam valssk')
    CALL testParam%get('testError',valssk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SSK'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valssk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SSK'// &
      ' - parameter name mismatch "testError" in "testSSK"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSSK'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSSK',valssk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SSK'// &
      ' - parameter data type mismatch! Parameter testSSK type is test_type and'// &
      ' must be REAL(SSK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    valssk=-4._SSK
    CALL testParam%set('testSSK',valssk,'The number -4.0')
    CALL testParam%get('testSSK',valssk)
    ASSERT(valssk == -4_SSK,'valssk')
    ASSERT(testParam%pdat%name == 'testSSK','%name')
    ASSERT(testParam%pdat%description == 'The number -4.0','%description')
    valssk=3._SSK
    CALL someParam%set('testSSK',valssk,'The number 3.0')
    CALL someParam%get('testSSK',valssk)
    ASSERT(valssk == 3_SSK,'valssk')
    ASSERT(someParam%name == 'testSSK','someParam%name')
    ASSERT(someParam%datatype == 'REAL(SSK)','someParam%datatype')
    ASSERT(someParam%description == 'The number 3.0','someParam%description')
    CALL someParam%set('testError',valssk) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SSK -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSSK"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSSK',valssk) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SSK -'// &
      ' unable to locate parameter "testSSK" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSSK'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSSK',valssk) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SSK -'// &
      ' parameter data type mismatch! Parameter testSSK type is test_type'// &
      ' and must be REAL(SSK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    valssk=3.0_SSK
    CALL testParam%init('testSSK',valssk)
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
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    valsdk=5._SDK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSDK',valsdk,'The number 5.0')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SDK'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSDK',valsdk,'The number 5.0')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSDK','%pdat%name')
    ASSERT(testParam%pdat%datatype == 'REAL(SDK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The number 5.0','%pdat%description')
    CALL testParam%init('testError',valsdk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SDK'// &
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
    CALL testParam%init('testSDK',valsdk,'The number 5.0')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSDK',valsdk)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSDK',valsdk,'The number 5.0')
    CALL testParam%get('testSDK',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSDK',valsdk)
    ASSERT(valsdk == 5._SDK,'someParam valsdk')
    valsdk=0
    CALL testParam%get('testSDK',valsdk)
    ASSERT(valsdk == 5._SDK,'testParam valsdk')
    CALL testParam%get('testError',valsdk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SDK'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valsdk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SDK'// &
      ' - parameter name mismatch "testError" in "testSDK"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSDK'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSDK',valsdk)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SDK'// &
      ' - parameter data type mismatch! Parameter testSDK type is test_type and'// &
      ' must be REAL(SDK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    valsdk=-4._SDK
    CALL testParam%set('testSDK',valsdk,'The number -4.0')
    CALL testParam%get('testSDK',valsdk)
    ASSERT(valsdk == -4_SDK,'valsdk')
    ASSERT(testParam%pdat%name == 'testSDK','%name')
    ASSERT(testParam%pdat%description == 'The number -4.0','%description')
    valsdk=3._SDK
    CALL someParam%set('testSDK',valsdk,'The number 3.0')
    CALL someParam%get('testSDK',valsdk)
    ASSERT(valsdk == 3_SDK,'valsdk')
    ASSERT(someParam%name == 'testSDK','someParam%name')
    ASSERT(someParam%datatype == 'REAL(SDK)','someParam%datatype')
    ASSERT(someParam%description == 'The number 3.0','someParam%description')
    CALL someParam%set('testError',valsdk) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SDK -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSDK"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSDK',valsdk) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SDK -'// &
      ' unable to locate parameter "testSDK" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSDK'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSDK',valsdk) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SDK -'// &
      ' parameter data type mismatch! Parameter testSDK type is test_type'// &
      ' and must be REAL(SDK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    valsdk=3.0_SDK
    CALL testParam%init('testSDK',valsdk)
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
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    valchar='test'
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testCHAR',valchar,'The value is test')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_STR'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testCHAR',valchar,'The value is test')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testCHAR','%pdat%name')
    ASSERT(testParam%pdat%datatype == 'TYPE(StringType)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The value is test','%pdat%description')
    CALL testParam%init('testError',TRIM(valchar))
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_STR'// &
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
    CALL testParam%init('testCHAR',TRIM(valchar),'The value is test')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testCHAR',TRIM(valchar))
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testCHAR',TRIM(valchar),'The value is test')
    CALL testParam%get('testCHAR',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testCHAR',valchar1)
    ASSERT(TRIM(valchar1) == 'test','someParam valchar')
    valchar='testing'
    CALL testParam%get('testCHAR',valchar1)
    ASSERT(TRIM(valchar1) == 'test','testParam valchar')
    CALL testParam%get('testError',valchar1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_STR'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valchar1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_STR'// &
      ' - parameter name mismatch "testError" in "testCHAR"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testCHAR'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testCHAR',valchar1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_STR'// &
      ' - parameter data type mismatch! Parameter testCHAR type is test_type and'// &
      ' must be TYPE(StringType)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    valchar='testing'
    CALL testParam%set('testCHAR',valchar,'The value is testing')
    CALL testParam%get('testCHAR',valchar1)
    ASSERT(TRIM(valchar1) == 'testing','valchar')
    ASSERT(testParam%pdat%name == 'testCHAR','%name')
    ASSERT(testParam%pdat%description == 'The value is testing','%description')
    valchar='more tests'
    CALL someParam%set('testCHAR',TRIM(valchar),'The value is more tests')
    CALL someParam%get('testCHAR',valchar1)
    ASSERT(TRIM(valchar1) == 'more tests','valchar')
    ASSERT(someParam%name == 'testCHAR','someParam%name')
    ASSERT(someParam%datatype == 'TYPE(StringType)','someParam%datatype')
    ASSERT(someParam%description == 'The value is more tests','someParam%description')
    CALL someParam%set('testError',valchar) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_STR -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testCHAR"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testCHAR',TRIM(valchar)) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_STR -'// &
      ' unable to locate parameter "testCHAR" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testCHAR'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testCHAR',TRIM(valchar)) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_STR -'// &
      ' parameter data type mismatch! Parameter testCHAR type is test_type'// &
      ' and must be TYPE(StringType)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    valchar='last test'
    CALL testParam%init('testCHAR',TRIM(valchar))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testCHAR','%name')
    ASSERT(testParam2%pdat%datatype == 'TYPE(StringType)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testCHAR
!
!-------------------------------------------------------------------------------
!Test StringType support
  SUBROUTINE testSTR()
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    valstr='test'
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSTR',valstr,'The value is test')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_STR'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSTR',valstr,'The value is test')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSTR','%pdat%name')
    ASSERT(testParam%pdat%datatype == 'TYPE(StringType)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The value is test','%pdat%description')
    CALL testParam%init('testError',TRIM(valstr))
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_STR'// &
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
    CALL testParam%init('testSTR',TRIM(valstr),'The value is test')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSTR',TRIM(valstr))
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSTR',TRIM(valstr),'The value is test')
    CALL testParam%get('testSTR',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSTR',valstr)
    ASSERT(TRIM(valstr) == 'test','someParam valstr')
    valstr='testing'
    CALL testParam%get('testSTR',valstr)
    ASSERT(TRIM(valstr) == 'test','testParam valstr')
    CALL testParam%get('testError',valstr)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_STR'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valstr)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_STR'// &
      ' - parameter name mismatch "testError" in "testSTR"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSTR'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSTR',valstr)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_STR'// &
      ' - parameter data type mismatch! Parameter testSTR type is test_type and'// &
      ' must be TYPE(StringType)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    valstr='testing'
    CALL testParam%set('testSTR',valstr,'The value is testing')
    CALL testParam%get('testSTR',valstr)
    ASSERT(TRIM(valstr) == 'testing','valstr')
    ASSERT(testParam%pdat%name == 'testSTR','%name')
    ASSERT(testParam%pdat%description == 'The value is testing','%description')
    valstr='more tests'
    CALL someParam%set('testSTR',TRIM(valstr),'The value is more tests')
    CALL testParam%get('testSTR',valstr)
    ASSERT(TRIM(valstr) == 'more tests','valstr')
    ASSERT(someParam%name == 'testSTR','someParam%name')
    ASSERT(someParam%datatype == 'TYPE(StringType)','someParam%datatype')
    ASSERT(someParam%description == 'The value is more tests','someParam%description')
    CALL someParam%set('testError',valstr) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_STR -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSTR"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSTR',TRIM(valstr)) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_STR -'// &
      ' unable to locate parameter "testSTR" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSTR'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSTR',TRIM(valstr)) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_STR -'// &
      ' parameter data type mismatch! Parameter testSTR type is test_type'// &
      ' and must be TYPE(StringType)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    valstr='last test'
    CALL testParam%init('testSTR',TRIM(valstr))
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
  SUBROUTINE testSBKa1()
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    ALLOCATE(valsbka1(2))
    valsbka1(1)=.TRUE.
    valsbka1(2)=.FALSE.
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSBKa1',valsbka1,'The values .TRUE. & .FALSE.')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SBK_a1'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSBKa1',valsbka1,'The values .TRUE. & .FALSE.')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSBKa1','%pdat%name')
    ASSERT(testParam%pdat%datatype == '1-D ARRAY LOGICAL(SBK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The values .TRUE. & .FALSE.','%pdat%description')
    CALL testParam%init('testError',valsbka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SBK_a1'// &
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
    CALL testParam%init('testSBKa1',valsbka1,'The values .TRUE. & .FALSE.')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSBKa1',valsbka1)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSBKa1',valsbka1,'The values .TRUE. & .FALSE.')
    CALL testParam%get('testSBKa1',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSBKa1',valsbka1)
    ASSERT(SIZE(valsbka1,DIM=1) == 2,'someParam valsbka1 size 1')
    ASSERT(valsbka1(1) .AND. .NOT. valsbka1(2),'someParam valsbka1 1')
    DEALLOCATE(valsbka1)
    ALLOCATE(valsbka1(1))
    CALL someParam%get('testSBKa1',valsbka1)
    ASSERT(SIZE(valsbka1,DIM=1) == 2,'someParam valsbka1 different size')
    ASSERT(valsbka1(1) .AND. .NOT. valsbka1(2),'someParam valsbka1 2')
    DEALLOCATE(valsbka1)
    CALL someParam%get('testSBKa1',valsbka1)
    ASSERT(SIZE(valsbka1,DIM=1) == 2,'someParam valsbka1 unallocated')
    ASSERT(valsbka1(1) .AND. .NOT. valsbka1(2),'someParam valsbka1 3')
    DEALLOCATE(valsbka1)
    ALLOCATE(valsbka1(1))
    CALL testParam%get('testSBKa1',valsbka1)
    ASSERT(SIZE(valsbka1,DIM=1) == 2,'testParam valsbka1 size 1')
    ASSERT(valsbka1(1) .AND. .NOT. valsbka1(2),'testParam valsbka1 1')
    DEALLOCATE(valsbka1)
    CALL testParam%get('testSBKa1',valsbka1)
    ASSERT(SIZE(valsbka1,DIM=1) == 2,'testParam valsbka1 unallocated')
    ASSERT(valsbka1(1) .AND. .NOT. valsbka1(2),'testParam valsbka1 2')
    CALL testParam%get('testError',valsbka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SBK_a1'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valsbka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SBK_a1'// &
      ' - parameter name mismatch "testError" in "testSBKa1"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSBKa1'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSBKa1',valsbka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SBK_a1'// &
      ' - parameter data type mismatch! Parameter testSBKa1 type is test_type and'// &
      ' must be 1-D ARRAY LOGICAL(SBK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSBKa1',(/.FALSE./),'The value is FALSE')
    CALL testParam%get('testSBKa1',valsbka1)
    ASSERT(testParam%pdat%name == 'testSBKa1','%name')
    ASSERT(testParam%pdat%datatype == '1-D ARRAY LOGICAL(SBK)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The value is FALSE','%description')
    ASSERT(SIZE(valsbka1,DIM=1) == 1,'testParam valsbka1 size 1')
    ASSERT(.NOT.valsbka1(1),'testParam valsbka1 1')
    CALL testParam%set('testSBKa1',(/.TRUE.,.FALSE./),'The values .TRUE. & .FALSE.')
    CALL testParam%get('testSBKa1',valsbka1)
    ASSERT(SIZE(valsbka1,DIM=1) == 2,'testParam valsbka1 size 2')
    ASSERT(valsbka1(1) .AND. .NOT. valsbka1(2),'testParam valsbka1 2')
    ASSERT(testParam%pdat%description == 'The values .TRUE. & .FALSE.','%description')
    !
    CALL someParam%set('testSBKa1',(/.TRUE./),'The value is TRUE')
    CALL someParam%get('testSBKa1',valsbka1)
    ASSERT(someParam%name == 'testSBKa1','someParam%name')
    ASSERT(someParam%datatype == '1-D ARRAY LOGICAL(SBK)','someParam%datatype')
    ASSERT(someParam%description == 'The value is TRUE','someParam%description')
    ASSERT(SIZE(valsbka1,DIM=1) == 1,'someParam valsbka1 size 1')
    ASSERT(valsbka1(1),'someParam valsbka1 1')
    CALL someParam%set('testSBKa1',(/.FALSE.,.TRUE./),'The values .FALSE. & .TRUE.')
    CALL someParam%get('testSBKa1',valsbka1)
    ASSERT(someParam%name == 'testSBKa1','someParam%name')
    ASSERT(someParam%datatype == '1-D ARRAY LOGICAL(SBK)','someParam%datatype')
    ASSERT(someParam%description == 'The values .FALSE. & .TRUE.','someParam%description')
    ASSERT(SIZE(valsbka1,DIM=1) == 2,'someParam valsbka1 size 2')
    ASSERT(.NOT.valsbka1(1) .AND. valsbka1(2),'testParam valsbka1 2')
    DEALLOCATE(valsbka1)
    !
    CALL someParam%set('testError',(/.FALSE./)) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SBK_a1 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSBKa1"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSBKa1',(/.TRUE./)) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SBK_a1 -'// &
      ' unable to locate parameter "testSBKa1" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSBKa1'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSBKa1',(/.TRUE./)) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SBK_a1 -'// &
      ' parameter data type mismatch! Parameter testSBKa1 type is test_type'// &
      ' and must be 1-D ARRAY LOGICAL(SBK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSBKa1',(/.TRUE./))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSBKa1','%name')
    ASSERT(testParam2%pdat%datatype == '1-D ARRAY LOGICAL(SBK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSBKa1
!
!-------------------------------------------------------------------------------
!Test 1-D Array SNK support
  SUBROUTINE testSNKa1()
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    ALLOCATE(valsnka1(2))
    valsnka1(1)=5_SNK
    valsnka1(2)=7_SNK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSNKa1',valsnka1,'The numbers 5 & 7')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SNK_a1'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSNKa1',valsnka1,'The numbers 5 & 7')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSNKa1','%pdat%name')
    ASSERT(testParam%pdat%datatype == '1-D ARRAY INTEGER(SNK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The numbers 5 & 7','%pdat%description')
    CALL testParam%init('testError',valsnka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SNK_a1'// &
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
    CALL testParam%init('testSNKa1',valsnka1,'The numbers 5 & 7')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSNKa1',valsnka1)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSNKa1',valsnka1,'The numbers 5 & 7')
    CALL testParam%get('testSNKa1',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSNKa1',valsnka1)
    ASSERT(SIZE(valsnka1,DIM=1) == 2,'someParam valsnka1 size 1')
    ASSERT(valsnka1(1) == 5 .AND. valsnka1(2) == 7,'someParam valsnka1 1')
    DEALLOCATE(valsnka1)
    ALLOCATE(valsnka1(1))
    CALL someParam%get('testSNKa1',valsnka1)
    ASSERT(SIZE(valsnka1,DIM=1) == 2,'someParam valsnka1 different size')
    ASSERT(valsnka1(1) == 5 .AND. valsnka1(2) == 7,'someParam valsnka1 2')
    DEALLOCATE(valsnka1)
    CALL someParam%get('testSNKa1',valsnka1)
    ASSERT(SIZE(valsnka1,DIM=1) == 2,'someParam valsnka1 unallocated')
    ASSERT(valsnka1(1) == 5 .AND. valsnka1(2) == 7,'someParam valsnka1 3')
    DEALLOCATE(valsnka1)
    ALLOCATE(valsnka1(1))
    CALL testParam%get('testSNKa1',valsnka1)
    ASSERT(SIZE(valsnka1,DIM=1) == 2,'testParam valsnka1 size 1')
    ASSERT(valsnka1(1) == 5 .AND. valsnka1(2) == 7,'testParam valsnka1 1')
    DEALLOCATE(valsnka1)
    CALL testParam%get('testSNKa1',valsnka1)
    ASSERT(SIZE(valsnka1,DIM=1) == 2,'testParam valsnka1 unallocated')
    ASSERT(valsnka1(1) == 5 .AND. valsnka1(2) == 7,'testParam valsnka1 2')
    CALL testParam%get('testError',valsnka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SNK_a1'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valsnka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SNK_a1'// &
      ' - parameter name mismatch "testError" in "testSNKa1"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSNKa1'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSNKa1',valsnka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SNK_a1'// &
      ' - parameter data type mismatch! Parameter testSNKa1 type is test_type and'// &
      ' must be 1-D ARRAY INTEGER(SNK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSNKa1',(/-10/),'The number -10')
    CALL testParam%get('testSNKa1',valsnka1)
    ASSERT(testParam%pdat%name == 'testSNKa1','%name')
    ASSERT(testParam%pdat%datatype == '1-D ARRAY INTEGER(SNK)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The number -10','%description')
    ASSERT(SIZE(valsnka1,DIM=1) == 1,'testParam valsnka1 size 1')
    ASSERT(valsnka1(1) == -10,'testParam valsnka1 1')
    CALL testParam%set('testSNKa1',(/3,-5/),'The numbers 3 & -5')
    CALL testParam%get('testSNKa1',valsnka1)
    ASSERT(SIZE(valsnka1,DIM=1) == 2,'testParam valsnka1 size 2')
    ASSERT(valsnka1(1) == 3 .AND. valsnka1(2) == -5,'testParam valsnka1 2')
    ASSERT(testParam%pdat%description == 'The numbers 3 & -5','%description')
    !
    CALL someParam%set('testSNKa1',(/-1/),'The number -1')
    CALL someParam%get('testSNKa1',valsnka1)
    ASSERT(someParam%name == 'testSNKa1','someParam%name')
    ASSERT(someParam%datatype == '1-D ARRAY INTEGER(SNK)','someParam%datatype')
    ASSERT(someParam%description == 'The number -1','someParam%description')
    ASSERT(SIZE(valsnka1,DIM=1) == 1,'someParam valsnka1 size 1')
    ASSERT(valsnka1(1) == -1,'someParam valsnka1 1')
    CALL someParam%set('testSNKa1',(/5,7/),'The numbers 5 & 7')
    CALL someParam%get('testSNKa1',valsnka1)
    ASSERT(someParam%name == 'testSNKa1','someParam%name')
    ASSERT(someParam%datatype == '1-D ARRAY INTEGER(SNK)','someParam%datatype')
    ASSERT(someParam%description == 'The numbers 5 & 7','someParam%description')
    ASSERT(SIZE(valsnka1,DIM=1) == 2,'someParam valsnka1 size 2')
    ASSERT(valsnka1(1) == 5 .AND. valsnka1(2) == 7,'testParam valsnka1 2')
    DEALLOCATE(valsnka1)
    !
    CALL someParam%set('testError',(/-1/)) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SNK_a1 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSNKa1"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSNKa1',(/-1/)) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SNK_a1 -'// &
      ' unable to locate parameter "testSNKa1" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSNKa1'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSNKa1',(/-1/)) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SNK_a1 -'// &
      ' parameter data type mismatch! Parameter testSNKa1 type is test_type'// &
      ' and must be 1-D ARRAY INTEGER(SNK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSNKa1',(/-1/))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSNKa1','%name')
    ASSERT(testParam2%pdat%datatype == '1-D ARRAY INTEGER(SNK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSNKa1
!
!-------------------------------------------------------------------------------
!Test 1-D Array SLK support
  SUBROUTINE testSLKa1()
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    ALLOCATE(valslka1(2))
    valslka1(1)=5_SLK
    valslka1(2)=7_SLK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSLKa1',valslka1,'The numbers 5 & 7')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SLK_a1'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSLKa1',valslka1,'The numbers 5 & 7')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSLKa1','%pdat%name')
    ASSERT(testParam%pdat%datatype == '1-D ARRAY INTEGER(SLK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The numbers 5 & 7','%pdat%description')
    CALL testParam%init('testError',valslka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SLK_a1'// &
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
    CALL testParam%init('testSLKa1',valslka1,'The numbers 5 & 7')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSLKa1',valslka1)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSLKa1',valslka1,'The numbers 5 & 7')
    CALL testParam%get('testSLKa1',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSLKa1',valslka1)
    ASSERT(SIZE(valslka1,DIM=1) == 2,'someParam valslka1 size 1')
    ASSERT(valslka1(1) == 5_SLK .AND. valslka1(2) == 7_SLK,'someParam valslka1 1')
    DEALLOCATE(valslka1)
    ALLOCATE(valslka1(1))
    CALL someParam%get('testSLKa1',valslka1)
    ASSERT(SIZE(valslka1,DIM=1) == 2,'someParam valslka1 different size')
    ASSERT(valslka1(1) == 5_SLK .AND. valslka1(2) == 7_SLK,'someParam valslka1 2')
    DEALLOCATE(valslka1)
    CALL someParam%get('testSLKa1',valslka1)
    ASSERT(SIZE(valslka1,DIM=1) == 2,'someParam valslka1 unallocated')
    ASSERT(valslka1(1) == 5_SLK .AND. valslka1(2) == 7_SLK,'someParam valslka1 3')
    DEALLOCATE(valslka1)
    ALLOCATE(valslka1(1))
    CALL testParam%get('testSLKa1',valslka1)
    ASSERT(SIZE(valslka1,DIM=1) == 2,'testParam valslka1 size 1')
    ASSERT(valslka1(1) == 5_SLK .AND. valslka1(2) == 7_SLK,'testParam valslka1 1')
    DEALLOCATE(valslka1)
    CALL testParam%get('testSLKa1',valslka1)
    ASSERT(SIZE(valslka1,DIM=1) == 2,'testParam valslka1 unallocated')
    ASSERT(valslka1(1) == 5_SLK .AND. valslka1(2) == 7_SLK,'testParam valslka1 2')
    CALL testParam%get('testError',valslka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SLK_a1'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valslka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SLK_a1'// &
      ' - parameter name mismatch "testError" in "testSLKa1"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSLKa1'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSLKa1',valslka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SLK_a1'// &
      ' - parameter data type mismatch! Parameter testSLKa1 type is test_type and'// &
      ' must be 1-D ARRAY INTEGER(SLK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSLKa1',(/-10_SLK/),'The number -10')
    CALL testParam%get('testSLKa1',valslka1)
    ASSERT(testParam%pdat%name == 'testSLKa1','%name')
    ASSERT(testParam%pdat%datatype == '1-D ARRAY INTEGER(SLK)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The number -10','%description')
    ASSERT(SIZE(valslka1,DIM=1) == 1,'testParam valslka1 size 1')
    ASSERT(valslka1(1) == -10_SLK,'testParam valslka1 1')
    CALL testParam%set('testSLKa1',(/3_SLK,-5_SLK/),'The numbers 3 & -5')
    CALL testParam%get('testSLKa1',valslka1)
    ASSERT(SIZE(valslka1,DIM=1) == 2,'testParam valslka1 size 2')
    ASSERT(valslka1(1) == 3_SLK .AND. valslka1(2) == -5_SLK,'testParam valslka1 2')
    ASSERT(testParam%pdat%description == 'The numbers 3 & -5','%description')
    !
    CALL someParam%set('testSLKa1',(/-1_SLK/),'The number -1')
    CALL someParam%get('testSLKa1',valslka1)
    ASSERT(someParam%name == 'testSLKa1','someParam%name')
    ASSERT(someParam%datatype == '1-D ARRAY INTEGER(SLK)','someParam%datatype')
    ASSERT(someParam%description == 'The number -1','someParam%description')
    ASSERT(SIZE(valslka1,DIM=1) == 1,'someParam valslka1 size 1')
    ASSERT(valslka1(1) == -1_SLK,'someParam valslka1 1')
    CALL someParam%set('testSLKa1',(/5_SLK,7_SLK/),'The numbers 5 & 7')
    CALL someParam%get('testSLKa1',valslka1)
    ASSERT(someParam%name == 'testSLKa1','someParam%name')
    ASSERT(someParam%datatype == '1-D ARRAY INTEGER(SLK)','someParam%datatype')
    ASSERT(someParam%description == 'The numbers 5 & 7','someParam%description')
    ASSERT(SIZE(valslka1,DIM=1) == 2,'someParam valslka1 size 2')
    ASSERT(valslka1(1) == 5_SLK .AND. valslka1(2) == 7_SLK,'testParam valslka1 2')
    DEALLOCATE(valslka1)
    !
    CALL someParam%set('testError',(/-1_SLK/)) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SLK_a1 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSLKa1"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSLKa1',(/-1_SLK/)) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SLK_a1 -'// &
      ' unable to locate parameter "testSLKa1" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSLKa1'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSLKa1',(/-1_SLK/)) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SLK_a1 -'// &
      ' parameter data type mismatch! Parameter testSLKa1 type is test_type'// &
      ' and must be 1-D ARRAY INTEGER(SLK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSLKa1',(/-1_SLK/))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSLKa1','%name')
    ASSERT(testParam2%pdat%datatype == '1-D ARRAY INTEGER(SLK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSLKa1
!
!-------------------------------------------------------------------------------
!Test 1-D Array SSK support
  SUBROUTINE testSSKa1()
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    LOGICAL(SBK) :: bool
    ALLOCATE(valsska1(2))
    valsska1(1)=5.0_SSK
    valsska1(2)=7.0_SSK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSSKa1',valsska1,'The numbers 5.0 & 7.0')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SSK_a1'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSSKa1',valsska1,'The numbers 5.0 & 7.0')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSSKa1','%pdat%name')
    ASSERT(testParam%pdat%datatype == '1-D ARRAY REAL(SSK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The numbers 5.0 & 7.0','%pdat%description')
    CALL testParam%init('testError',valsska1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SSK_a1'// &
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
    CALL testParam%init('testSSKa1',valsska1,'The numbers 5.0 & 7.0')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSSKa1',valsska1)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSSKa1',valsska1,'The numbers 5.0 & 7.0')
    CALL testParam%get('testSSKa1',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSSKa1',valsska1)
    ASSERT(SIZE(valsska1,DIM=1) == 2,'someParam valsska1 size 1')
    bool=ALL(valsska1 .APPROXEQA. (/5.0_SSK,7.0_SSK/))
    ASSERT(bool,'someParam valsska1 1')
    DEALLOCATE(valsska1)
    ALLOCATE(valsska1(1))
    CALL someParam%get('testSSKa1',valsska1)
    ASSERT(SIZE(valsska1,DIM=1) == 2,'someParam valsska1 different size')
    bool=ALL(valsska1 .APPROXEQA. (/5.0_SSK,7.0_SSK/))
    ASSERT(bool,'someParam valsska1 2')
    DEALLOCATE(valsska1)
    CALL someParam%get('testSSKa1',valsska1)
    ASSERT(SIZE(valsska1,DIM=1) == 2,'someParam valsska1 unallocated')
    bool=ALL(valsska1 .APPROXEQA. (/5.0_SSK,7.0_SSK/))
    ASSERT(bool,'someParam valsska1 3')
    DEALLOCATE(valsska1)
    ALLOCATE(valsska1(1))
    CALL testParam%get('testSSKa1',valsska1)
    ASSERT(SIZE(valsska1,DIM=1) == 2,'testParam valsska1 size 1')
    bool=ALL(valsska1 .APPROXEQA. (/5.0_SSK,7.0_SSK/))
    ASSERT(bool,'testParam valsska1 1')
    DEALLOCATE(valsska1)
    CALL testParam%get('testSSKa1',valsska1)
    ASSERT(SIZE(valsska1,DIM=1) == 2,'testParam valsska1 unallocated')
    bool=ALL(valsska1 .APPROXEQA. (/5.0_SSK,7.0_SSK/))
    ASSERT(bool,'testParam valsska1 2')
    CALL testParam%get('testError',valsska1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SSK_a1'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valsska1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SSK_a1'// &
      ' - parameter name mismatch "testError" in "testSSKa1"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSSKa1'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSSKa1',valsska1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SSK_a1'// &
      ' - parameter data type mismatch! Parameter testSSKa1 type is test_type and'// &
      ' must be 1-D ARRAY REAL(SSK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSSKa1',(/-10.0_SSK/),'The number -10.0')
    CALL testParam%get('testSSKa1',valsska1)
    ASSERT(testParam%pdat%name == 'testSSKa1','%name')
    ASSERT(testParam%pdat%datatype == '1-D ARRAY REAL(SSK)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The number -10.0','%description')
    ASSERT(SIZE(valsska1,DIM=1) == 1,'testParam valsska1 size 1')
    ASSERT(valsska1(1) .APPROXEQA. -10.0_SSK,'testParam valsska1 1')
    CALL testParam%set('testSSKa1',(/3.0_SSK,-5.0_SSK/),'The numbers 3.0 & -5.0')
    CALL testParam%get('testSSKa1',valsska1)
    ASSERT(SIZE(valsska1,DIM=1) == 2,'testParam valsska1 size 2')
    bool=ALL(valsska1 .APPROXEQA. (/3.0_SSK,-5.0_SSK/))
    ASSERT(bool,'testParam valsska1 2')
    ASSERT(testParam%pdat%description == 'The numbers 3.0 & -5.0','%description')
    !
    CALL someParam%set('testSSKa1',(/-1.0_SSK/),'The number -1.0')
    CALL someParam%get('testSSKa1',valsska1)
    ASSERT(someParam%name == 'testSSKa1','someParam%name')
    ASSERT(someParam%datatype == '1-D ARRAY REAL(SSK)','someParam%datatype')
    ASSERT(someParam%description == 'The number -1.0','someParam%description')
    ASSERT(SIZE(valsska1,DIM=1) == 1,'someParam valsska1 size 1')
    ASSERT(valsska1(1) .APPROXEQA. -1.0_SSK,'someParam valsska1 1')
    CALL someParam%set('testSSKa1',(/5.0_SSK,7.0_SSK/),'The numbers 5.0 & 7.0')
    CALL someParam%get('testSSKa1',valsska1)
    ASSERT(someParam%name == 'testSSKa1','someParam%name')
    ASSERT(someParam%datatype == '1-D ARRAY REAL(SSK)','someParam%datatype')
    ASSERT(someParam%description == 'The numbers 5.0 & 7.0','someParam%description')
    ASSERT(SIZE(valsska1,DIM=1) == 2,'someParam valsska1 size 2')
    bool=ALL(valsska1 .APPROXEQA. (/5.0_SSK,7.0_SSK/))
    ASSERT(bool,'testParam valsska1 2')
    DEALLOCATE(valsska1)
    !
    CALL someParam%set('testError',(/-1.0_SSK/)) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SSK_a1 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSSKa1"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSSKa1',(/-1.0_SSK/)) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SSK_a1 -'// &
      ' unable to locate parameter "testSSKa1" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSSKa1'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSSKa1',(/-1.0_SSK/)) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SSK_a1 -'// &
      ' parameter data type mismatch! Parameter testSSKa1 type is test_type'// &
      ' and must be 1-D ARRAY REAL(SSK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSSKa1',(/-1.0_SSK/))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSSKa1','%name')
    ASSERT(testParam2%pdat%datatype == '1-D ARRAY REAL(SSK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSSKa1
!
!-------------------------------------------------------------------------------
!Test 1-D Array SDK support
  SUBROUTINE testSDKa1()
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    LOGICAL(SBK) :: bool
    ALLOCATE(valsdka1(2))
    valsdka1(1)=5.0_SDK
    valsdka1(2)=7.0_SDK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSDKa1',valsdka1,'The numbers 5.0 & 7.0')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SDK_a1'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSDKa1',valsdka1,'The numbers 5.0 & 7.0')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSDKa1','%pdat%name')
    ASSERT(testParam%pdat%datatype == '1-D ARRAY REAL(SDK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The numbers 5.0 & 7.0','%pdat%description')
    CALL testParam%init('testError',valsdka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SDK_a1'// &
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
    CALL testParam%init('testSDKa1',valsdka1,'The numbers 5.0 & 7.0')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSDKa1',valsdka1)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSDKa1',valsdka1,'The numbers 5.0 & 7.0')
    CALL testParam%get('testSDKa1',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSDKa1',valsdka1)
    ASSERT(SIZE(valsdka1,DIM=1) == 2,'someParam valsdka1 size 1')
    bool=ALL(valsdka1 .APPROXEQA. (/5.0_SDK,7.0_SDK/))
    ASSERT(bool,'someParam valsdka1 1')
    DEALLOCATE(valsdka1)
    ALLOCATE(valsdka1(1))
    CALL someParam%get('testSDKa1',valsdka1)
    ASSERT(SIZE(valsdka1,DIM=1) == 2,'someParam valsdka1 different size')
    bool=ALL(valsdka1 .APPROXEQA. (/5.0_SDK,7.0_SDK/))
    ASSERT(bool,'someParam valsdka1 2')
    DEALLOCATE(valsdka1)
    CALL someParam%get('testSDKa1',valsdka1)
    ASSERT(SIZE(valsdka1,DIM=1) == 2,'someParam valsdka1 unallocated')
    bool=ALL(valsdka1 .APPROXEQA. (/5.0_SDK,7.0_SDK/))
    ASSERT(bool,'someParam valsdka1 3')
    DEALLOCATE(valsdka1)
    ALLOCATE(valsdka1(1))
    CALL testParam%get('testSDKa1',valsdka1)
    ASSERT(SIZE(valsdka1,DIM=1) == 2,'testParam valsdka1 size 1')
    bool=ALL(valsdka1 .APPROXEQA. (/5.0_SDK,7.0_SDK/))
    ASSERT(bool,'testParam valsdka1 1')
    DEALLOCATE(valsdka1)
    CALL testParam%get('testSDKa1',valsdka1)
    ASSERT(SIZE(valsdka1,DIM=1) == 2,'testParam valsdka1 unallocated')
    bool=ALL(valsdka1 .APPROXEQA. (/5.0_SDK,7.0_SDK/))
    ASSERT(bool,'testParam valsdka1 2')
    CALL testParam%get('testError',valsdka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SDK_a1'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valsdka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SDK_a1'// &
      ' - parameter name mismatch "testError" in "testSDKa1"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSDKa1'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSDKa1',valsdka1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SDK_a1'// &
      ' - parameter data type mismatch! Parameter testSDKa1 type is test_type and'// &
      ' must be 1-D ARRAY REAL(SDK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSDKa1',(/-10.0_SDK/),'The number -10.0')
    CALL testParam%get('testSDKa1',valsdka1)
    ASSERT(testParam%pdat%name == 'testSDKa1','%name')
    ASSERT(testParam%pdat%datatype == '1-D ARRAY REAL(SDK)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The number -10.0','%description')
    ASSERT(SIZE(valsdka1,DIM=1) == 1,'testParam valsdka1 size 1')
    ASSERT(valsdka1(1) .APPROXEQA. -10.0_SDK,'testParam valsdka1 1')
    CALL testParam%set('testSDKa1',(/3.0_SDK,-5.0_SDK/),'The numbers 3.0 & -5.0')
    CALL testParam%get('testSDKa1',valsdka1)
    ASSERT(SIZE(valsdka1,DIM=1) == 2,'testParam valsdka1 size 2')
    bool=ALL(valsdka1 .APPROXEQA. (/3.0_SDK,-5.0_SDK/))
    ASSERT(bool,'testParam valsdka1 2')
    ASSERT(testParam%pdat%description == 'The numbers 3.0 & -5.0','%description')
    !
    CALL someParam%set('testSDKa1',(/-1.0_SDK/),'The number -1.0')
    CALL someParam%get('testSDKa1',valsdka1)
    ASSERT(someParam%name == 'testSDKa1','someParam%name')
    ASSERT(someParam%datatype == '1-D ARRAY REAL(SDK)','someParam%datatype')
    ASSERT(someParam%description == 'The number -1.0','someParam%description')
    ASSERT(SIZE(valsdka1,DIM=1) == 1,'someParam valsdka1 size 1')
    ASSERT(valsdka1(1) .APPROXEQA. -1.0_SDK,'someParam valsdka1 1')
    CALL someParam%set('testSDKa1',(/5.0_SDK,7.0_SDK/),'The numbers 5.0 & 7.0')
    CALL someParam%get('testSDKa1',valsdka1)
    ASSERT(someParam%name == 'testSDKa1','someParam%name')
    ASSERT(someParam%datatype == '1-D ARRAY REAL(SDK)','someParam%datatype')
    ASSERT(someParam%description == 'The numbers 5.0 & 7.0','someParam%description')
    ASSERT(SIZE(valsdka1,DIM=1) == 2,'someParam valsdka1 size 2')
    bool=ALL(valsdka1 .APPROXEQA. (/5.0_SDK,7.0_SDK/))
    ASSERT(bool,'testParam valsdka1 2')
    DEALLOCATE(valsdka1)
    !
    CALL someParam%set('testError',(/-1.0_SDK/)) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SDK_a1 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSDKa1"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSDKa1',(/-1.0_SDK/)) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SDK_a1 -'// &
      ' unable to locate parameter "testSDKa1" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSDKa1'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSDKa1',(/-1.0_SDK/)) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SDK_a1 -'// &
      ' parameter data type mismatch! Parameter testSDKa1 type is test_type'// &
      ' and must be 1-D ARRAY REAL(SDK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSDKa1',(/-1.0_SDK/))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSDKa1','%name')
    ASSERT(testParam2%pdat%datatype == '1-D ARRAY REAL(SDK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSDKa1
!
!-------------------------------------------------------------------------------
!Test 1-D array StringType support
  SUBROUTINE testSTRa1()
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    LOGICAL(SBK) :: bool
    ALLOCATE(valstra1(2))
    valstra1(1)='testing'
    valstra1(2)='more testing'
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSTRa1',valstra1,'The values are testing and more testing')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_STR_a1'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSTRa1',valstra1,'The values are testing and more testing')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSTRa1','%pdat%name')
    ASSERT(testParam%pdat%datatype == '1-D ARRAY TYPE(StringType)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The values are testing and more testing','%pdat%description')
    CALL testParam%init('testError',valstra1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_STR_a1'// &
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
    CALL testParam%init('testSTRa1',valstra1,'The numbers 5.0 & 7.0')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSTRa1',valstra1)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSTRa1',valstra1,'The values are testing and more testing')
    CALL testParam%get('testSTRa1',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSTRa1',valstra1)
    ASSERT(SIZE(valstra1,DIM=1) == 2,'someParam valstra1 size 1')
    bool=valstra1(1) == 'testing' .AND. valstra1(2) == 'more testing'
    ASSERT(bool,'someParam valstra1 1')
    DEALLOCATE(valstra1)
    ALLOCATE(valstra1(1))
    CALL someParam%get('testSTRa1',valstra1)
    ASSERT(SIZE(valstra1,DIM=1) == 2,'someParam valstra1 different size')
    bool=valstra1(1) == 'testing' .AND. valstra1(2) == 'more testing'
    ASSERT(bool,'someParam valstra1 2')
    DEALLOCATE(valstra1)
    CALL someParam%get('testSTRa1',valstra1)
    ASSERT(SIZE(valstra1,DIM=1) == 2,'someParam valstra1 unallocated')
    bool=valstra1(1) == 'testing' .AND. valstra1(2) == 'more testing'
    ASSERT(bool,'someParam valstra1 3')
    DEALLOCATE(valstra1)
    ALLOCATE(valstra1(1))
    CALL testParam%get('testSTRa1',valstra1)
    ASSERT(SIZE(valstra1,DIM=1) == 2,'testParam valstra1 size 1')
    bool=valstra1(1) == 'testing' .AND. valstra1(2) == 'more testing'
    ASSERT(bool,'testParam valstra1 1')
    DEALLOCATE(valstra1)
    CALL testParam%get('testSTRa1',valstra1)
    ASSERT(SIZE(valstra1,DIM=1) == 2,'testParam valstra1 unallocated')
    bool=valstra1(1) == 'testing' .AND. valstra1(2) == 'more testing'
    ASSERT(bool,'testParam valstra1 2')
    CALL testParam%get('testError',valstra1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_STR_a1'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valstra1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_STR_a1'// &
      ' - parameter name mismatch "testError" in "testSTRa1"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSTRa1'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSTRa1',valstra1)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_STR_a1'// &
      ' - parameter data type mismatch! Parameter testSTRa1 type is test_type and'// &
      ' must be 1-D ARRAY TYPE(StringType)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    valstra1(1)='another test'
    valstra1(2)='one more test'
    CALL testParam%set('testSTRa1',valstra1,'The values are another test and one more test')
    valstra1(1)=''
    valstra1(2)=''
    CALL testParam%get('testSTRa1',valstra1)
    ASSERT(testParam%pdat%name == 'testSTRa1','%name')
    ASSERT(testParam%pdat%datatype == '1-D ARRAY TYPE(StringType)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The values are another test and one more test','%description')
    ASSERT(SIZE(valstra1,DIM=1) == 2,'testParam valstra1 size 1')
    bool=valstra1(1) == 'another test' .AND. valstra1(2) == 'one more test'
    ASSERT(bool,'testParam valstra1 1')
    DEALLOCATE(valstra1)
    ALLOCATE(valstra1(1))
    valstra1(1)='a different size test'
    CALL testParam%set('testSTRa1',valstra1,'The value a different size test')
    DEALLOCATE(valstra1)
    ALLOCATE(valstra1(2))
    valstra1(1)=''
    valstra1(2)=''
    CALL testParam%get('testSTRa1',valstra1)
    ASSERT(SIZE(valstra1,DIM=1) == 1,'testParam valstra1 size 2')
    bool=valstra1(1) == 'a different size test'
    ASSERT(bool,'testParam valstra1 2')
    ASSERT(testParam%pdat%description == 'The value a different size test','%description')
    !
    DEALLOCATE(valstra1)
    ALLOCATE(valstra1(2))
    valstra1(1)='another test'
    valstra1(2)='one more test'
    CALL someParam%set('testSTRa1',valstra1,'The values are another test and one more test')
    valstra1(1)=''
    valstra1(2)=''
    CALL someParam%get('testSTRa1',valstra1)
    ASSERT(someParam%name == 'testSTRa1','someParam%name')
    ASSERT(someParam%datatype == '1-D ARRAY TYPE(StringType)','someParam%datatype')
    ASSERT(someParam%description == 'The values are another test and one more test','someParam%description')
    ASSERT(SIZE(valstra1,DIM=1) == 2,'someParam valstra1 size 1')
    bool=valstra1(1) == 'another test' .AND. valstra1(2) == 'one more test'
    ASSERT(bool,'someParam valstra1 1')
    DEALLOCATE(valstra1)
    ALLOCATE(valstra1(1))
    valstra1(1)='a different size test'
    CALL someParam%set('testSTRa1',valstra1,'The value a different size test')
    DEALLOCATE(valstra1)
    ALLOCATE(valstra1(2))
    valstra1(1)=''
    valstra1(2)=''
    CALL someParam%get('testSTRa1',valstra1)
    ASSERT(someParam%name == 'testSTRa1','someParam%name')
    ASSERT(someParam%datatype == '1-D ARRAY TYPE(StringType)','someParam%datatype')
    ASSERT(someParam%description == 'The value a different size test','someParam%description')
    ASSERT(SIZE(valstra1,DIM=1) == 1,'someParam valstra1 size 2')
    bool=valstra1(1) == 'a different size test'
    ASSERT(bool,'testParam valstra1 2')
    !
    CALL someParam%set('testError',valstra1) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_STR_a1 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSTRa1"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSTRa1',valstra1) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_STR_a1 -'// &
      ' unable to locate parameter "testSTRa1" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSTRa1'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSTRa1',valstra1) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_STR_a1 -'// &
      ' parameter data type mismatch! Parameter testSTRa1 type is test_type'// &
      ' and must be 1-D ARRAY TYPE(StringType)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSTRa1',valstra1)
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSTRa1','%name')
    ASSERT(testParam2%pdat%datatype == '1-D ARRAY TYPE(StringType)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    DEALLOCATE(valstra1)
    CALL clear_test_vars()
  ENDSUBROUTINE testSTRa1
!
!-------------------------------------------------------------------------------
!Test 2-D Array SNK support
  SUBROUTINE testSNKa2()
    LOGICAL(SBK) :: bool
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    ALLOCATE(valsnka2(2,2))
    valsnka2(1,1)=5_SNK
    valsnka2(2,1)=7_SNK
    valsnka2(1,2)=6_SNK
    valsnka2(2,2)=8_SNK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSNKa2',valsnka2,'The numbers 5, 7, 6, & 8')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SNK_a2'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSNKa2',valsnka2,'The numbers 5, 7, 6, & 8')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSNKa2','%pdat%name')
    ASSERT(testParam%pdat%datatype == '2-D ARRAY INTEGER(SNK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The numbers 5, 7, 6, & 8','%pdat%description')
    CALL testParam%init('testError',valsnka2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SNK_a2'// &
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
    CALL testParam%init('testSNKa2',valsnka2,'The numbers 5, 7, 6, & 8')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSNKa2',valsnka2)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSNKa2',valsnka2,'The numbers 5, 7, 6, & 8')
    CALL testParam%get('testSNKa2',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSNKa2',valsnka2)
    bool=SIZE(valsnka2,DIM=1) == 2 .AND. SIZE(valsnka2,DIM=2) == 2
    ASSERT(bool,'someParam valsnka2 size 1')
    bool=valsnka2(1,1) == 5 .AND. valsnka2(2,1) == 7 .AND. valsnka2(1,2) == 6 .AND. valsnka2(2,2) == 8
    ASSERT(bool,'someParam valsnka2 1')
    DEALLOCATE(valsnka2)
    ALLOCATE(valsnka2(1,1))
    CALL someParam%get('testSNKa2',valsnka2)
    bool=SIZE(valsnka2,DIM=1) == 2 .AND. SIZE(valsnka2,DIM=2) == 2
    ASSERT(bool,'someParam valsnka2 different size')
    bool=valsnka2(1,1) == 5 .AND. valsnka2(2,1) == 7 .AND. valsnka2(1,2) == 6 .AND. valsnka2(2,2) == 8
    ASSERT(bool,'someParam valsnka2 2')
    DEALLOCATE(valsnka2)
    CALL someParam%get('testSNKa2',valsnka2)
    bool=SIZE(valsnka2,DIM=1) == 2 .AND. SIZE(valsnka2,DIM=2) == 2
    ASSERT(bool,'someParam valsnka2 unallocated')
    bool=valsnka2(1,1) == 5 .AND. valsnka2(2,1) == 7 .AND. valsnka2(1,2) == 6 .AND. valsnka2(2,2) == 8
    ASSERT(bool,'someParam valsnka2 3')
    DEALLOCATE(valsnka2)
    ALLOCATE(valsnka2(2,1))
    CALL testParam%get('testSNKa2',valsnka2)
    bool=SIZE(valsnka2,DIM=1) == 2 .AND. SIZE(valsnka2,DIM=2) == 2
    ASSERT(bool,'testParam valsnka2 size 1')
    bool=valsnka2(1,1) == 5 .AND. valsnka2(2,1) == 7 .AND. valsnka2(1,2) == 6 .AND. valsnka2(2,2) == 8
    ASSERT(bool,'testParam valsnka2 1')
    DEALLOCATE(valsnka2)
    CALL testParam%get('testSNKa2',valsnka2)
    bool=SIZE(valsnka2,DIM=1) == 2 .AND. SIZE(valsnka2,DIM=2) == 2
    ASSERT(bool,'testParam valsnka2 unallocated')
    bool=valsnka2(1,1) == 5 .AND. valsnka2(2,1) == 7 .AND. valsnka2(1,2) == 6 .AND. valsnka2(2,2) == 8
    ASSERT(bool,'testParam valsnka2 2')
    CALL testParam%get('testError',valsnka2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SNK_a2'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valsnka2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SNK_a2'// &
      ' - parameter name mismatch "testError" in "testSNKa2"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSNKa2'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSNKa2',valsnka2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SNK_a2'// &
      ' - parameter data type mismatch! Parameter testSNKa2 type is test_type and'// &
      ' must be 2-D ARRAY INTEGER(SNK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSNKa2',RESHAPE((/-10/),(/1,1/)),'The number -10')
    CALL testParam%get('testSNKa2',valsnka2)
    ASSERT(testParam%pdat%name == 'testSNKa2','%name')
    ASSERT(testParam%pdat%datatype == '2-D ARRAY INTEGER(SNK)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The number -10','%description')
    bool=SIZE(valsnka2,DIM=1) == 1 .AND. SIZE(valsnka2,DIM=2) == 1
    ASSERT(bool,'testParam valsnka2 size 1')
    bool=valsnka2(1,1) == -10
    ASSERT(bool,'testParam valsnka2 1')
    CALL testParam%set('testSNKa2',RESHAPE((/3,-5/),(/2,1/)),'The numbers 3 & -5')
    CALL testParam%get('testSNKa2',valsnka2)
    bool=SIZE(valsnka2,DIM=1) == 2 .AND. SIZE(valsnka2,DIM=2) == 1
    ASSERT(bool,'testParam valsnka2 size 2')
    bool=valsnka2(1,1) == 3 .AND. valsnka2(2,1) == -5
    ASSERT(bool,'testParam valsnka2 2')
    ASSERT(testParam%pdat%description == 'The numbers 3 & -5','%description')
    !
    CALL someParam%set('testSNKa2',RESHAPE((/-10/),(/1,1/)),'The number -10')
    CALL someParam%get('testSNKa2',valsnka2)
    ASSERT(someParam%name == 'testSNKa2','someParam%name')
    ASSERT(someParam%datatype == '2-D ARRAY INTEGER(SNK)','someParam%datatype')
    ASSERT(someParam%description == 'The number -10','someParam%description')
    bool=SIZE(valsnka2,DIM=1) == 1 .AND. SIZE(valsnka2,DIM=2) == 1
    ASSERT(bool,'someParam valsnka2 size 1')
    bool=valsnka2(1,1) == -10
    ASSERT(bool,'someParam valsnka2 1')
    CALL someParam%set('testSNKa2',RESHAPE((/5,7/),(/1,2/)),'The numbers 5 & 7')
    CALL someParam%get('testSNKa2',valsnka2)
    ASSERT(someParam%name == 'testSNKa2','someParam%name')
    ASSERT(someParam%datatype == '2-D ARRAY INTEGER(SNK)','someParam%datatype')
    ASSERT(someParam%description == 'The numbers 5 & 7','someParam%description')
    bool=SIZE(valsnka2,DIM=1) == 1 .AND. SIZE(valsnka2,DIM=2) == 2
    ASSERT(bool,'someParam valsnka2 size 2')
    bool=valsnka2(1,1) == 5 .AND. valsnka2(1,2) == 7
    ASSERT(bool,'testParam valsnka2 2')
    DEALLOCATE(valsnka2)
    !
    CALL someParam%set('testError',RESHAPE((/-1/),(/1,1/))) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SNK_a2 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSNKa2"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSNKa2',RESHAPE((/-1/),(/1,1/))) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SNK_a2 -'// &
      ' unable to locate parameter "testSNKa2" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSNKa2'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSNKa2',RESHAPE((/-1/),(/1,1/))) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SNK_a2 -'// &
      ' parameter data type mismatch! Parameter testSNKa2 type is test_type'// &
      ' and must be 2-D ARRAY INTEGER(SNK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSNKa2',RESHAPE((/-1/),(/1,1/)))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSNKa2','%name')
    ASSERT(testParam2%pdat%datatype == '2-D ARRAY INTEGER(SNK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSNKa2
!
!-------------------------------------------------------------------------------
!Test 2-D Array SLK support
  SUBROUTINE testSLKa2()
    LOGICAL(SBK) :: bool
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    ALLOCATE(valslka2(2,2))
    valslka2(1,1)=5_SLK
    valslka2(2,1)=7_SLK
    valslka2(1,2)=6_SLK
    valslka2(2,2)=8_SLK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSLKa2',valslka2,'The numbers 5, 7, 6, & 8')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SLK_a2'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSLKa2',valslka2,'The numbers 5, 7, 6, & 8')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSLKa2','%pdat%name')
    ASSERT(testParam%pdat%datatype == '2-D ARRAY INTEGER(SLK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The numbers 5, 7, 6, & 8','%pdat%description')
    CALL testParam%init('testError',valslka2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SLK_a2'// &
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
    CALL testParam%init('testSLKa2',valslka2,'The numbers 5, 7, 6, & 8')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSLKa2',valslka2)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSLKa2',valslka2,'The numbers 5, 7, 6, & 8')
    CALL testParam%get('testSLKa2',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSLKa2',valslka2)
    bool=SIZE(valslka2,DIM=1) == 2 .AND. SIZE(valslka2,DIM=2) == 2
    ASSERT(bool,'someParam valslka2 size 1')
    bool=valslka2(1,1) == 5_SLK .AND. valslka2(2,1) == 7_SLK .AND. &
      valslka2(1,2) == 6_SLK .AND. valslka2(2,2) == 8_SLK
    ASSERT(bool,'someParam valslka2 1')
    DEALLOCATE(valslka2)
    ALLOCATE(valslka2(1,1))
    CALL someParam%get('testSLKa2',valslka2)
    bool=SIZE(valslka2,DIM=1) == 2 .AND. SIZE(valslka2,DIM=2) == 2
    ASSERT(bool,'someParam valslka2 different size')
    bool=valslka2(1,1) == 5_SLK .AND. valslka2(2,1) == 7_SLK .AND. &
      valslka2(1,2) == 6_SLK .AND. valslka2(2,2) == 8_SLK
    ASSERT(bool,'someParam valslka2 2')
    DEALLOCATE(valslka2)
    CALL someParam%get('testSLKa2',valslka2)
    bool=SIZE(valslka2,DIM=1) == 2 .AND. SIZE(valslka2,DIM=2) == 2
    ASSERT(bool,'someParam valslka2 unallocated')
    bool=valslka2(1,1) == 5_SLK .AND. valslka2(2,1) == 7_SLK .AND. &
      valslka2(1,2) == 6_SLK .AND. valslka2(2,2) == 8_SLK
    ASSERT(bool,'someParam valslka2 3')
    DEALLOCATE(valslka2)
    ALLOCATE(valslka2(2,1))
    CALL testParam%get('testSLKa2',valslka2)
    bool=SIZE(valslka2,DIM=1) == 2 .AND. SIZE(valslka2,DIM=2) == 2
    ASSERT(bool,'testParam valslka2 size 1')
    bool=valslka2(1,1) == 5_SLK .AND. valslka2(2,1) == 7_SLK .AND. &
      valslka2(1,2) == 6_SLK .AND. valslka2(2,2) == 8_SLK
    ASSERT(bool,'testParam valslka2 1')
    DEALLOCATE(valslka2)
    CALL testParam%get('testSLKa2',valslka2)
    bool=SIZE(valslka2,DIM=1) == 2 .AND. SIZE(valslka2,DIM=2) == 2
    ASSERT(bool,'testParam valslka2 unallocated')
    bool=valslka2(1,1) == 5_SLK .AND. valslka2(2,1) == 7_SLK .AND. &
      valslka2(1,2) == 6_SLK .AND. valslka2(2,2) == 8_SLK
    ASSERT(bool,'testParam valslka2 2')
    CALL testParam%get('testError',valslka2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SLK_a2'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valslka2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SLK_a2'// &
      ' - parameter name mismatch "testError" in "testSLKa2"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSLKa2'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSLKa2',valslka2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SLK_a2'// &
      ' - parameter data type mismatch! Parameter testSLKa2 type is test_type and'// &
      ' must be 2-D ARRAY INTEGER(SLK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSLKa2',RESHAPE((/-10_SLK/),(/1,1/)),'The number -10')
    CALL testParam%get('testSLKa2',valslka2)
    ASSERT(testParam%pdat%name == 'testSLKa2','%name')
    ASSERT(testParam%pdat%datatype == '2-D ARRAY INTEGER(SLK)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The number -10','%description')
    bool=SIZE(valslka2,DIM=1) == 1 .AND. SIZE(valslka2,DIM=2) == 1
    ASSERT(bool,'testParam valslka2 size 1')
    bool=valslka2(1,1) == -10_SLK
    ASSERT(bool,'testParam valslka2 1')
    CALL testParam%set('testSLKa2',RESHAPE((/3_SLK,-5_SLK/),(/2,1/)),'The numbers 3 & -5')
    CALL testParam%get('testSLKa2',valslka2)
    bool=SIZE(valslka2,DIM=1) == 2 .AND. SIZE(valslka2,DIM=2) == 1
    ASSERT(bool,'testParam valslka2 size 2')
    bool=valslka2(1,1) == 3_SLK .AND. valslka2(2,1) == -5_SLK
    ASSERT(bool,'testParam valslka2 2')
    ASSERT(testParam%pdat%description == 'The numbers 3 & -5','%description')
    !
    CALL someParam%set('testSLKa2',RESHAPE((/-10_SLK/),(/1,1/)),'The number -10')
    CALL someParam%get('testSLKa2',valslka2)
    ASSERT(someParam%name == 'testSLKa2','someParam%name')
    ASSERT(someParam%datatype == '2-D ARRAY INTEGER(SLK)','someParam%datatype')
    ASSERT(someParam%description == 'The number -10','someParam%description')
    bool=SIZE(valslka2,DIM=1) == 1 .AND. SIZE(valslka2,DIM=2) == 1
    ASSERT(bool,'someParam valslka2 size 1')
    bool=valslka2(1,1) == -10_SLK
    ASSERT(bool,'someParam valslka2 1')
    CALL someParam%set('testSLKa2',RESHAPE((/5_SLK,7_SLK/),(/1,2/)),'The numbers 5 & 7')
    CALL someParam%get('testSLKa2',valslka2)
    ASSERT(someParam%name == 'testSLKa2','someParam%name')
    ASSERT(someParam%datatype == '2-D ARRAY INTEGER(SLK)','someParam%datatype')
    ASSERT(someParam%description == 'The numbers 5 & 7','someParam%description')
    bool=SIZE(valslka2,DIM=1) == 1 .AND. SIZE(valslka2,DIM=2) == 2
    ASSERT(bool,'someParam valslka2 size 2')
    bool=valslka2(1,1) == 5_SLK .AND. valslka2(1,2) == 7_SLK
    ASSERT(bool,'testParam valslka2 2')
    DEALLOCATE(valslka2)
    !
    CALL someParam%set('testError',RESHAPE((/-1_SLK/),(/1,1/))) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SLK_a2 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSLKa2"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSLKa2',RESHAPE((/-1_SLK/),(/1,1/))) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SLK_a2 -'// &
      ' unable to locate parameter "testSLKa2" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSLKa2'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSLKa2',RESHAPE((/-1_SLK/),(/1,1/))) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SLK_a2 -'// &
      ' parameter data type mismatch! Parameter testSLKa2 type is test_type'// &
      ' and must be 2-D ARRAY INTEGER(SLK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSLKa2',RESHAPE((/-1_SLK/),(/1,1/)))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSLKa2','%name')
    ASSERT(testParam2%pdat%datatype == '2-D ARRAY INTEGER(SLK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSLKa2
!
!-------------------------------------------------------------------------------
!Test 2-D Array SSK support
  SUBROUTINE testSSKa2()
    LOGICAL(SBK) :: bool
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    ALLOCATE(valsska2(2,2))
    valsska2(1,1)=5.0_SSK
    valsska2(2,1)=7.0_SSK
    valsska2(1,2)=6.0_SSK
    valsska2(2,2)=8.0_SSK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSSKa2',valsska2,'The numbers 5, 7, 6, & 8')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SSK_a2'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSSKa2',valsska2,'The numbers 5, 7, 6, & 8')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSSKa2','%pdat%name')
    ASSERT(testParam%pdat%datatype == '2-D ARRAY REAL(SSK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The numbers 5, 7, 6, & 8','%pdat%description')
    CALL testParam%init('testError',valsska2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SSK_a2'// &
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
    CALL testParam%init('testSSKa2',valsska2,'The numbers 5, 7, 6, & 8')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSSKa2',valsska2)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSSKa2',valsska2,'The numbers 5, 7, 6, & 8')
    CALL testParam%get('testSSKa2',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSSKa2',valsska2)
    bool=SIZE(valsska2,DIM=1) == 2 .AND. SIZE(valsska2,DIM=2) == 2
    ASSERT(bool,'someParam valsska2 size 1')
    bool=valsska2(1,1) == 5.0_SSK .AND. valsska2(2,1) == 7.0_SSK .AND. &
      valsska2(1,2) == 6.0_SSK .AND. valsska2(2,2) == 8.0_SSK
    ASSERT(bool,'someParam valsska2 1')
    DEALLOCATE(valsska2)
    ALLOCATE(valsska2(1,1))
    CALL someParam%get('testSSKa2',valsska2)
    bool=SIZE(valsska2,DIM=1) == 2 .AND. SIZE(valsska2,DIM=2) == 2
    ASSERT(bool,'someParam valsska2 different size')
    bool=valsska2(1,1) == 5.0_SSK .AND. valsska2(2,1) == 7.0_SSK .AND. &
      valsska2(1,2) == 6.0_SSK .AND. valsska2(2,2) == 8.0_SSK
    ASSERT(bool,'someParam valsska2 2')
    DEALLOCATE(valsska2)
    CALL someParam%get('testSSKa2',valsska2)
    bool=SIZE(valsska2,DIM=1) == 2 .AND. SIZE(valsska2,DIM=2) == 2
    ASSERT(bool,'someParam valsska2 unallocated')
    bool=valsska2(1,1) == 5.0_SSK .AND. valsska2(2,1) == 7.0_SSK .AND. &
      valsska2(1,2) == 6.0_SSK .AND. valsska2(2,2) == 8.0_SSK
    ASSERT(bool,'someParam valsska2 3')
    DEALLOCATE(valsska2)
    ALLOCATE(valsska2(2,1))
    CALL testParam%get('testSSKa2',valsska2)
    bool=SIZE(valsska2,DIM=1) == 2 .AND. SIZE(valsska2,DIM=2) == 2
    ASSERT(bool,'testParam valsska2 size 1')
    bool=valsska2(1,1) == 5.0_SSK .AND. valsska2(2,1) == 7.0_SSK .AND. &
      valsska2(1,2) == 6.0_SSK .AND. valsska2(2,2) == 8.0_SSK
    ASSERT(bool,'testParam valsska2 1')
    DEALLOCATE(valsska2)
    CALL testParam%get('testSSKa2',valsska2)
    bool=SIZE(valsska2,DIM=1) == 2 .AND. SIZE(valsska2,DIM=2) == 2
    ASSERT(bool,'testParam valsska2 unallocated')
    bool=valsska2(1,1) == 5.0_SSK .AND. valsska2(2,1) == 7.0_SSK .AND. &
      valsska2(1,2) == 6.0_SSK .AND. valsska2(2,2) == 8.0_SSK
    ASSERT(bool,'testParam valsska2 2')
    CALL testParam%get('testError',valsska2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SSK_a2'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valsska2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SSK_a2'// &
      ' - parameter name mismatch "testError" in "testSSKa2"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSSKa2'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSSKa2',valsska2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SSK_a2'// &
      ' - parameter data type mismatch! Parameter testSSKa2 type is test_type and'// &
      ' must be 2-D ARRAY REAL(SSK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSSKa2',RESHAPE((/-10.0_SSK/),(/1,1/)),'The number -10')
    CALL testParam%get('testSSKa2',valsska2)
    ASSERT(testParam%pdat%name == 'testSSKa2','%name')
    ASSERT(testParam%pdat%datatype == '2-D ARRAY REAL(SSK)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The number -10','%description')
    bool=SIZE(valsska2,DIM=1) == 1 .AND. SIZE(valsska2,DIM=2) == 1
    ASSERT(bool,'testParam valsska2 size 1')
    bool=valsska2(1,1) == -10.0_SSK
    ASSERT(bool,'testParam valsska2 1')
    CALL testParam%set('testSSKa2',RESHAPE((/3.0_SSK,-5.0_SSK/),(/2,1/)),'The numbers 3 & -5')
    CALL testParam%get('testSSKa2',valsska2)
    bool=SIZE(valsska2,DIM=1) == 2 .AND. SIZE(valsska2,DIM=2) == 1
    ASSERT(bool,'testParam valsska2 size 2')
    bool=valsska2(1,1) == 3.0_SSK .AND. valsska2(2,1) == -5.0_SSK
    ASSERT(bool,'testParam valsska2 2')
    ASSERT(testParam%pdat%description == 'The numbers 3 & -5','%description')
    !
    CALL someParam%set('testSSKa2',RESHAPE((/-10.0_SSK/),(/1,1/)),'The number -10')
    CALL someParam%get('testSSKa2',valsska2)
    ASSERT(someParam%name == 'testSSKa2','someParam%name')
    ASSERT(someParam%datatype == '2-D ARRAY REAL(SSK)','someParam%datatype')
    ASSERT(someParam%description == 'The number -10','someParam%description')
    bool=SIZE(valsska2,DIM=1) == 1 .AND. SIZE(valsska2,DIM=2) == 1
    ASSERT(bool,'someParam valsska2 size 1')
    bool=valsska2(1,1) == -10.0_SSK
    ASSERT(bool,'someParam valsska2 1')
    CALL someParam%set('testSSKa2',RESHAPE((/5.0_SSK,7.0_SSK/),(/1,2/)),'The numbers 5 & 7')
    CALL someParam%get('testSSKa2',valsska2)
    ASSERT(someParam%name == 'testSSKa2','someParam%name')
    ASSERT(someParam%datatype == '2-D ARRAY REAL(SSK)','someParam%datatype')
    ASSERT(someParam%description == 'The numbers 5 & 7','someParam%description')
    bool=SIZE(valsska2,DIM=1) == 1 .AND. SIZE(valsska2,DIM=2) == 2
    ASSERT(bool,'someParam valsska2 size 2')
    bool=valsska2(1,1) == 5.0_SSK .AND. valsska2(1,2) == 7.0_SSK
    ASSERT(bool,'testParam valsska2 2')
    DEALLOCATE(valsska2)
    !
    CALL someParam%set('testError',RESHAPE((/-1.0_SSK/),(/1,1/))) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SSK_a2 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSSKa2"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSSKa2',RESHAPE((/-1.0_SSK/),(/1,1/))) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SSK_a2 -'// &
      ' unable to locate parameter "testSSKa2" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSSKa2'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSSKa2',RESHAPE((/-1.0_SSK/),(/1,1/))) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SSK_a2 -'// &
      ' parameter data type mismatch! Parameter testSSKa2 type is test_type'// &
      ' and must be 2-D ARRAY REAL(SSK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSSKa2',RESHAPE((/-1.0_SSK/),(/1,1/)))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSSKa2','%name')
    ASSERT(testParam2%pdat%datatype == '2-D ARRAY REAL(SSK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSSKa2
!
!-------------------------------------------------------------------------------
!Test 2-D Array SDK support
  SUBROUTINE testSDKa2()
    LOGICAL(SBK) :: bool
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    ALLOCATE(valsdka2(2,2))
    valsdka2(1,1)=5.0_SDK
    valsdka2(2,1)=7.0_SDK
    valsdka2(1,2)=6.0_SDK
    valsdka2(2,2)=8.0_SDK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSDKa2',valsdka2,'The numbers 5, 7, 6, & 8')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SDK_a2'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSDKa2',valsdka2,'The numbers 5, 7, 6, & 8')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSDKa2','%pdat%name')
    ASSERT(testParam%pdat%datatype == '2-D ARRAY REAL(SDK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The numbers 5, 7, 6, & 8','%pdat%description')
    CALL testParam%init('testError',valsdka2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SDK_a2'// &
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
    CALL testParam%init('testSDKa2',valsdka2,'The numbers 5, 7, 6, & 8')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSDKa2',valsdka2)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSDKa2',valsdka2,'The numbers 5, 7, 6, & 8')
    CALL testParam%get('testSDKa2',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSDKa2',valsdka2)
    bool=SIZE(valsdka2,DIM=1) == 2 .AND. SIZE(valsdka2,DIM=2) == 2
    ASSERT(bool,'someParam valsdka2 size 1')
    bool=valsdka2(1,1) == 5.0_SDK .AND. valsdka2(2,1) == 7.0_SDK .AND. &
      valsdka2(1,2) == 6.0_SDK .AND. valsdka2(2,2) == 8.0_SDK
    ASSERT(bool,'someParam valsdka2 1')
    DEALLOCATE(valsdka2)
    ALLOCATE(valsdka2(1,1))
    CALL someParam%get('testSDKa2',valsdka2)
    bool=SIZE(valsdka2,DIM=1) == 2 .AND. SIZE(valsdka2,DIM=2) == 2
    ASSERT(bool,'someParam valsdka2 different size')
    bool=valsdka2(1,1) == 5.0_SDK .AND. valsdka2(2,1) == 7.0_SDK .AND. &
      valsdka2(1,2) == 6.0_SDK .AND. valsdka2(2,2) == 8.0_SDK
    ASSERT(bool,'someParam valsdka2 2')
    DEALLOCATE(valsdka2)
    CALL someParam%get('testSDKa2',valsdka2)
    bool=SIZE(valsdka2,DIM=1) == 2 .AND. SIZE(valsdka2,DIM=2) == 2
    ASSERT(bool,'someParam valsdka2 unallocated')
    bool=valsdka2(1,1) == 5.0_SDK .AND. valsdka2(2,1) == 7.0_SDK .AND. &
      valsdka2(1,2) == 6.0_SDK .AND. valsdka2(2,2) == 8.0_SDK
    ASSERT(bool,'someParam valsdka2 3')
    DEALLOCATE(valsdka2)
    ALLOCATE(valsdka2(2,1))
    CALL testParam%get('testSDKa2',valsdka2)
    bool=SIZE(valsdka2,DIM=1) == 2 .AND. SIZE(valsdka2,DIM=2) == 2
    ASSERT(bool,'testParam valsdka2 size 1')
    bool=valsdka2(1,1) == 5.0_SDK .AND. valsdka2(2,1) == 7.0_SDK .AND. &
      valsdka2(1,2) == 6.0_SDK .AND. valsdka2(2,2) == 8.0_SDK
    ASSERT(bool,'testParam valsdka2 1')
    DEALLOCATE(valsdka2)
    CALL testParam%get('testSDKa2',valsdka2)
    bool=SIZE(valsdka2,DIM=1) == 2 .AND. SIZE(valsdka2,DIM=2) == 2
    ASSERT(bool,'testParam valsdka2 unallocated')
    bool=valsdka2(1,1) == 5.0_SDK .AND. valsdka2(2,1) == 7.0_SDK .AND. &
      valsdka2(1,2) == 6.0_SDK .AND. valsdka2(2,2) == 8.0_SDK
    ASSERT(bool,'testParam valsdka2 2')
    CALL testParam%get('testError',valsdka2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SDK_a2'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valsdka2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SDK_a2'// &
      ' - parameter name mismatch "testError" in "testSDKa2"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSDKa2'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSDKa2',valsdka2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SDK_a2'// &
      ' - parameter data type mismatch! Parameter testSDKa2 type is test_type and'// &
      ' must be 2-D ARRAY REAL(SDK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSDKa2',RESHAPE((/-10.0_SDK/),(/1,1/)),'The number -10')
    CALL testParam%get('testSDKa2',valsdka2)
    ASSERT(testParam%pdat%name == 'testSDKa2','%name')
    ASSERT(testParam%pdat%datatype == '2-D ARRAY REAL(SDK)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The number -10','%description')
    bool=SIZE(valsdka2,DIM=1) == 1 .AND. SIZE(valsdka2,DIM=2) == 1
    ASSERT(bool,'testParam valsdka2 size 1')
    bool=valsdka2(1,1) == -10.0_SDK
    ASSERT(bool,'testParam valsdka2 1')
    CALL testParam%set('testSDKa2',RESHAPE((/3.0_SDK,-5.0_SDK/),(/2,1/)),'The numbers 3 & -5')
    CALL testParam%get('testSDKa2',valsdka2)
    bool=SIZE(valsdka2,DIM=1) == 2 .AND. SIZE(valsdka2,DIM=2) == 1
    ASSERT(bool,'testParam valsdka2 size 2')
    bool=valsdka2(1,1) == 3.0_SDK .AND. valsdka2(2,1) == -5.0_SDK
    ASSERT(bool,'testParam valsdka2 2')
    ASSERT(testParam%pdat%description == 'The numbers 3 & -5','%description')
    !
    CALL someParam%set('testSDKa2',RESHAPE((/-10.0_SDK/),(/1,1/)),'The number -10')
    CALL someParam%get('testSDKa2',valsdka2)
    ASSERT(someParam%name == 'testSDKa2','someParam%name')
    ASSERT(someParam%datatype == '2-D ARRAY REAL(SDK)','someParam%datatype')
    ASSERT(someParam%description == 'The number -10','someParam%description')
    bool=SIZE(valsdka2,DIM=1) == 1 .AND. SIZE(valsdka2,DIM=2) == 1
    ASSERT(bool,'someParam valsdka2 size 1')
    bool=valsdka2(1,1) == -10.0_SDK
    ASSERT(bool,'someParam valsdka2 1')
    CALL someParam%set('testSDKa2',RESHAPE((/5.0_SDK,7.0_SDK/),(/1,2/)),'The numbers 5 & 7')
    CALL someParam%get('testSDKa2',valsdka2)
    ASSERT(someParam%name == 'testSDKa2','someParam%name')
    ASSERT(someParam%datatype == '2-D ARRAY REAL(SDK)','someParam%datatype')
    ASSERT(someParam%description == 'The numbers 5 & 7','someParam%description')
    bool=SIZE(valsdka2,DIM=1) == 1 .AND. SIZE(valsdka2,DIM=2) == 2
    ASSERT(bool,'someParam valsdka2 size 2')
    bool=valsdka2(1,1) == 5.0_SDK .AND. valsdka2(1,2) == 7.0_SDK
    ASSERT(bool,'testParam valsdka2 2')
    DEALLOCATE(valsdka2)
    !
    CALL someParam%set('testError',RESHAPE((/-1.0_SDK/),(/1,1/))) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SDK_a2 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSDKa2"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSDKa2',RESHAPE((/-1.0_SDK/),(/1,1/))) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SDK_a2 -'// &
      ' unable to locate parameter "testSDKa2" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSDKa2'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSDKa2',RESHAPE((/-1.0_SDK/),(/1,1/))) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SDK_a2 -'// &
      ' parameter data type mismatch! Parameter testSDKa2 type is test_type'// &
      ' and must be 2-D ARRAY REAL(SDK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSDKa2',RESHAPE((/-1.0_SDK/),(/1,1/)))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSDKa2','%name')
    ASSERT(testParam2%pdat%datatype == '2-D ARRAY REAL(SDK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSDKa2
!
!-------------------------------------------------------------------------------
!Test 2-D array StringType support
  SUBROUTINE testSTRa2()
    LOGICAL(SBK) :: bool
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    ALLOCATE(valstra2(2,2))
    valstra2(1,1)='testing 1'
    valstra2(2,1)='testing 2'
    valstra2(1,2)='testing 3'
    valstra2(2,2)='testing 4'
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSTRa2',valstra2,'The values testing 1, 2, 3, & 4')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_STR_a2'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSTRa2',valstra2,'The values testing 1, 2, 3, & 4')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSTRa2','%pdat%name')
    ASSERT(testParam%pdat%datatype == '2-D ARRAY TYPE(StringType)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The values testing 1, 2, 3, & 4','%pdat%description')
    CALL testParam%init('testError',valstra2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_STR_a2'// &
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
    CALL testParam%init('testSTRa2',valstra2,'The values testing 1, 2, 3, & 4')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSTRa2',valstra2)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSTRa2',valstra2,'The values testing 1, 2, 3, & 4')
    CALL testParam%get('testSTRa2',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSTRa2',valstra2)
    bool=SIZE(valstra2,DIM=1) == 2 .AND. SIZE(valstra2,DIM=2) == 2
    ASSERT(bool,'someParam valstra2 size 1')
    bool=valstra2(1,1) == 'testing 1' .AND. valstra2(2,1) == 'testing 2' .AND. &
      valstra2(1,2) == 'testing 3' .AND. valstra2(2,2) == 'testing 4'
    ASSERT(bool,'someParam valstra2 1')
    DEALLOCATE(valstra2)
    ALLOCATE(valstra2(1,1))
    CALL someParam%get('testSTRa2',valstra2)
    bool=SIZE(valstra2,DIM=1) == 2 .AND. SIZE(valstra2,DIM=2) == 2
    ASSERT(bool,'someParam valstra2 different size')
    bool=valstra2(1,1) == 'testing 1' .AND. valstra2(2,1) == 'testing 2' .AND. &
      valstra2(1,2) == 'testing 3' .AND. valstra2(2,2) == 'testing 4'
    ASSERT(bool,'someParam valstra2 2')
    DEALLOCATE(valstra2)
    CALL someParam%get('testSTRa2',valstra2)
    bool=SIZE(valstra2,DIM=1) == 2 .AND. SIZE(valstra2,DIM=2) == 2
    ASSERT(bool,'someParam valstra2 unallocated')
    bool=valstra2(1,1) == 'testing 1' .AND. valstra2(2,1) == 'testing 2' .AND. &
      valstra2(1,2) == 'testing 3' .AND. valstra2(2,2) == 'testing 4'
    ASSERT(bool,'someParam valstra2 3')
    DEALLOCATE(valstra2)
    ALLOCATE(valstra2(2,1))
    CALL testParam%get('testSTRa2',valstra2)
    bool=SIZE(valstra2,DIM=1) == 2 .AND. SIZE(valstra2,DIM=2) == 2
    ASSERT(bool,'testParam valstra2 size 1')
    bool=valstra2(1,1) == 'testing 1' .AND. valstra2(2,1) == 'testing 2' .AND. &
      valstra2(1,2) == 'testing 3' .AND. valstra2(2,2) == 'testing 4'
    ASSERT(bool,'testParam valstra2 1')
    DEALLOCATE(valstra2)
    CALL testParam%get('testSTRa2',valstra2)
    bool=SIZE(valstra2,DIM=1) == 2 .AND. SIZE(valstra2,DIM=2) == 2
    ASSERT(bool,'testParam valstra2 unallocated')
    bool=valstra2(1,1) == 'testing 1' .AND. valstra2(2,1) == 'testing 2' .AND. &
      valstra2(1,2) == 'testing 3' .AND. valstra2(2,2) == 'testing 4'
    ASSERT(bool,'testParam valstra2 2')
    CALL testParam%get('testError',valstra2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_STR_a2'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valstra2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_STR_a2'// &
      ' - parameter name mismatch "testError" in "testSTRa2"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSTRa2'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSTRa2',valstra2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_STR_a2'// &
      ' - parameter data type mismatch! Parameter testSTRa2 type is test_type and'// &
      ' must be 2-D ARRAY TYPE(StringType)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    DEALLOCATE(valstra2)
    ALLOCATE(valstra2(1,1))
    valstra2(1,1)='another test'
    CALL testParam%set('testSTRa2',valstra2,'The value another test')
    CALL testParam%get('testSTRa2',valstra2)
    ASSERT(testParam%pdat%name == 'testSTRa2','%name')
    ASSERT(testParam%pdat%datatype == '2-D ARRAY TYPE(StringType)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The value another test','%description')
    bool=SIZE(valstra2,DIM=1) == 1 .AND. SIZE(valstra2,DIM=2) == 1
    ASSERT(bool,'testParam valstra2 size 1')
    bool=valstra2(1,1) == 'another test'
    ASSERT(bool,'testParam valstra2 1')
    DEALLOCATE(valstra2)
    ALLOCATE(valstra2(2,1))
    valstra2(1,1)='test 1'
    valstra2(2,1)='test 2'
    CALL testParam%set('testSTRa2',valstra2,'The values test 1 & test 2')
    CALL testParam%get('testSTRa2',valstra2)
    bool=SIZE(valstra2,DIM=1) == 2 .AND. SIZE(valstra2,DIM=2) == 1
    ASSERT(bool,'testParam valstra2 size 2')
    bool=valstra2(1,1) == 'test 1' .AND. valstra2(2,1) == 'test 2'
    ASSERT(bool,'testParam valstra2 2')
    ASSERT(testParam%pdat%description == 'The values test 1 & test 2','%description')
    !
    DEALLOCATE(valstra2)
    ALLOCATE(valstra2(1,1))
    valstra2(1,1)='yet another test'
    CALL someParam%set('testSTRa2',valstra2,'The value yet another test')
    CALL someParam%get('testSTRa2',valstra2)
    ASSERT(someParam%name == 'testSTRa2','someParam%name')
    ASSERT(someParam%datatype == '2-D ARRAY TYPE(StringType)','someParam%datatype')
    ASSERT(someParam%description == 'The value yet another test','someParam%description')
    bool=SIZE(valstra2,DIM=1) == 1 .AND. SIZE(valstra2,DIM=2) == 1
    ASSERT(bool,'someParam valstra2 size 1')
    bool=valstra2(1,1) == 'yet another test'
    ASSERT(bool,'someParam valstra2 1')
    DEALLOCATE(valstra2)
    ALLOCATE(valstra2(1,2))
    valstra2(1,1)='final test 1'
    valstra2(1,2)='final test 2'
    CALL someParam%set('testSTRa2',valstra2,'The values final test 1 & final test 2')
    CALL someParam%get('testSTRa2',valstra2)
    ASSERT(someParam%name == 'testSTRa2','someParam%name')
    ASSERT(someParam%datatype == '2-D ARRAY TYPE(StringType)','someParam%datatype')
    ASSERT(someParam%description == 'The values final test 1 & final test 2','someParam%description')
    bool=SIZE(valstra2,DIM=1) == 1 .AND. SIZE(valstra2,DIM=2) == 2
    ASSERT(bool,'someParam valstra2 size 2')
    bool=valstra2(1,1) == 'final test 1' .AND. valstra2(1,2) == 'final test 2'
    ASSERT(bool,'testParam valstra2 2')
    !
    DEALLOCATE(valstra2)
    ALLOCATE(valstra2(1,1))
    valstra2(1,1)='test error'
    CALL someParam%set('testError',valstra2) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_STR_a2 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSTRa2"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSTRa2',valstra2) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_STR_a2 -'// &
      ' unable to locate parameter "testSTRa2" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSTRa2'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSTRa2',valstra2) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_STR_a2 -'// &
      ' parameter data type mismatch! Parameter testSTRa2 type is test_type'// &
      ' and must be 2-D ARRAY TYPE(StringType)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    DEALLOCATE(valstra2)
    ALLOCATE(valstra2(1,1))
    valstra2(1,1)='test operator'
    CALL testParam%init('testSTRa2',valstra2)
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSTRa2','%name')
    ASSERT(testParam2%pdat%datatype == '2-D ARRAY TYPE(StringType)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    DEALLOCATE(valstra2)
    CALL clear_test_vars()
  ENDSUBROUTINE testSTRa2
!
!-------------------------------------------------------------------------------
!Test 3-D Array SNK support
  SUBROUTINE testSNKa3()
    LOGICAL(SBK) :: bool
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    ALLOCATE(valsnka3(2,2,2))
    valsnka3(1,1,:)=5_SNK
    valsnka3(2,1,:)=7_SNK
    valsnka3(1,2,:)=6_SNK
    valsnka3(2,2,:)=8_SNK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSNKa3',valsnka3,'The numbers 5, 7, 6, & 8')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SNK_a3'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSNKa3',valsnka3,'The numbers 5, 7, 6, & 8')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSNKa3','%pdat%name')
    ASSERT(testParam%pdat%datatype == '3-D ARRAY INTEGER(SNK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The numbers 5, 7, 6, & 8','%pdat%description')
    CALL testParam%init('testError',valsnka3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SNK_a3'// &
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
    CALL testParam%init('testSNKa3',valsnka3,'The numbers 5, 7, 6, & 8')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSNKa3',valsnka3)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSNKa3',valsnka3,'The numbers 5, 7, 6, & 8')
    CALL testParam%get('testSNKa3',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSNKa3',valsnka3)
    bool=SIZE(valsnka3,DIM=1) == 2 .AND. SIZE(valsnka3,DIM=2) == 2 .AND. SIZE(valsnka3,DIM=3) == 2
    ASSERT(bool,'someParam valsnka3 size 1')
    bool=ALL(valsnka3(1,1,:) == 5) .AND. ALL(valsnka3(2,1,:) == 7) &
      .AND. ALL(valsnka3(1,2,:) == 6) .AND. ALL(valsnka3(2,2,:) == 8)
    ASSERT(bool,'someParam valsnka3 1')
    DEALLOCATE(valsnka3)
    ALLOCATE(valsnka3(1,1,1))
    CALL someParam%get('testSNKa3',valsnka3)
    bool=SIZE(valsnka3,DIM=1) == 2 .AND. SIZE(valsnka3,DIM=2) == 2 .AND. SIZE(valsnka3,DIM=3) == 2
    ASSERT(bool,'someParam valsnka3 different size')
    bool=ALL(valsnka3(1,1,:) == 5) .AND. ALL(valsnka3(2,1,:) == 7) &
      .AND. ALL(valsnka3(1,2,:) == 6) .AND. ALL(valsnka3(2,2,:) == 8)
    ASSERT(bool,'someParam valsnka3 2')
    DEALLOCATE(valsnka3)
    CALL someParam%get('testSNKa3',valsnka3)
    bool=SIZE(valsnka3,DIM=1) == 2 .AND. SIZE(valsnka3,DIM=2) == 2 .AND. SIZE(valsnka3,DIM=3) == 2
    ASSERT(bool,'someParam valsnka3 unallocated')
    bool=ALL(valsnka3(1,1,:) == 5) .AND. ALL(valsnka3(2,1,:) == 7) &
      .AND. ALL(valsnka3(1,2,:) == 6) .AND. ALL(valsnka3(2,2,:) == 8)
    ASSERT(bool,'someParam valsnka3 3')
    DEALLOCATE(valsnka3)
    ALLOCATE(valsnka3(2,2,1))
    CALL testParam%get('testSNKa3',valsnka3)
    bool=SIZE(valsnka3,DIM=1) == 2 .AND. SIZE(valsnka3,DIM=2) == 2 .AND. SIZE(valsnka3,DIM=3) == 2
    ASSERT(bool,'testParam valsnka3 size 1')
    bool=ALL(valsnka3(1,1,:) == 5) .AND. ALL(valsnka3(2,1,:) == 7) &
      .AND. ALL(valsnka3(1,2,:) == 6) .AND. ALL(valsnka3(2,2,:) == 8)
    ASSERT(bool,'testParam valsnka3 1')
    DEALLOCATE(valsnka3)
    CALL testParam%get('testSNKa3',valsnka3)
    bool=SIZE(valsnka3,DIM=1) == 2 .AND. SIZE(valsnka3,DIM=2) == 2 .AND. SIZE(valsnka3,DIM=3) == 2
    ASSERT(bool,'testParam valsnka3 unallocated')
    bool=ALL(valsnka3(1,1,:) == 5) .AND. ALL(valsnka3(2,1,:) == 7) &
      .AND. ALL(valsnka3(1,2,:) == 6) .AND. ALL(valsnka3(2,2,:) == 8)
    ASSERT(bool,'testParam valsnka3 2')
    CALL testParam%get('testError',valsnka3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SNK_a3'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valsnka3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SNK_a3'// &
      ' - parameter name mismatch "testError" in "testSNKa3"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSNKa3'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSNKa3',valsnka3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SNK_a3'// &
      ' - parameter data type mismatch! Parameter testSNKa3 type is test_type and'// &
      ' must be 3-D ARRAY INTEGER(SNK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSNKa3',RESHAPE((/-10/),(/1,1,1/)),'The number -10')
    CALL testParam%get('testSNKa3',valsnka3)
    ASSERT(testParam%pdat%name == 'testSNKa3','%name')
    ASSERT(testParam%pdat%datatype == '3-D ARRAY INTEGER(SNK)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The number -10','%description')
    bool=SIZE(valsnka3,DIM=1) == 1 .AND. SIZE(valsnka3,DIM=2) == 1
    ASSERT(bool,'testParam valsnka3 size 1')
    bool=valsnka3(1,1,1) == -10
    ASSERT(bool,'testParam valsnka3 1')
    CALL testParam%set('testSNKa3',RESHAPE((/3,-5,3,-5/),(/2,2,1/)),'The numbers 3 & -5')
    CALL testParam%get('testSNKa3',valsnka3)
    bool=SIZE(valsnka3,DIM=1) == 2 .AND. SIZE(valsnka3,DIM=2) == 2 .AND. SIZE(valsnka3,DIM=3) == 1
    ASSERT(bool,'testParam valsnka3 size 2')
    bool=valsnka3(1,1,1) == 3 .AND. valsnka3(2,1,1) == -5 .AND. &
      valsnka3(1,2,1) == 3 .AND. valsnka3(2,2,1) == -5
    ASSERT(bool,'testParam valsnka3 2')
    ASSERT(testParam%pdat%description == 'The numbers 3 & -5','%description')
    !
    CALL someParam%set('testSNKa3',RESHAPE((/-10/),(/1,1,1/)),'The number -10')
    CALL someParam%get('testSNKa3',valsnka3)
    ASSERT(someParam%name == 'testSNKa3','someParam%name')
    ASSERT(someParam%datatype == '3-D ARRAY INTEGER(SNK)','someParam%datatype')
    ASSERT(someParam%description == 'The number -10','someParam%description')
    bool=SIZE(valsnka3,DIM=1) == 1 .AND. SIZE(valsnka3,DIM=2) == 1
    ASSERT(bool,'someParam valsnka3 size 1')
    bool=valsnka3(1,1,1) == -10
    ASSERT(bool,'someParam valsnka3 1')
    CALL someParam%set('testSNKa3',RESHAPE((/5,7/),(/1,1,2/)),'The numbers 5 & 7')
    CALL someParam%get('testSNKa3',valsnka3)
    ASSERT(someParam%name == 'testSNKa3','someParam%name')
    ASSERT(someParam%datatype == '3-D ARRAY INTEGER(SNK)','someParam%datatype')
    ASSERT(someParam%description == 'The numbers 5 & 7','someParam%description')
    bool=SIZE(valsnka3,DIM=1) == 1 .AND. SIZE(valsnka3,DIM=2) == 1 .AND. SIZE(valsnka3,DIM=3) == 2
    ASSERT(bool,'someParam valsnka3 size 2')
    bool=valsnka3(1,1,1) == 5 .AND. valsnka3(1,1,2) == 7
    ASSERT(bool,'testParam valsnka3 2')
    DEALLOCATE(valsnka3)
    !
    CALL someParam%set('testError',RESHAPE((/-1/),(/1,1,1/))) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SNK_a3 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSNKa3"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSNKa3',RESHAPE((/-1/),(/1,1,1/))) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SNK_a3 -'// &
      ' unable to locate parameter "testSNKa3" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSNKa3'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSNKa3',RESHAPE((/-1/),(/1,1,1/))) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SNK_a3 -'// &
      ' parameter data type mismatch! Parameter testSNKa3 type is test_type'// &
      ' and must be 3-D ARRAY INTEGER(SNK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSNKa3',RESHAPE((/-1/),(/1,1,1/)))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSNKa3','%name')
    ASSERT(testParam2%pdat%datatype == '3-D ARRAY INTEGER(SNK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSNKa3
!
!-------------------------------------------------------------------------------
!Test 3-D Array SLK support
  SUBROUTINE testSLKa3()
    LOGICAL(SBK) :: bool
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    ALLOCATE(valslka3(2,2,2))
    valslka3(1,1,:)=5_SLK
    valslka3(2,1,:)=7_SLK
    valslka3(1,2,:)=6_SLK
    valslka3(2,2,:)=8_SLK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSLKa3',valslka3,'The numbers 5, 7, 6, & 8')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SLK_a3'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSLKa3',valslka3,'The numbers 5, 7, 6, & 8')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSLKa3','%pdat%name')
    ASSERT(testParam%pdat%datatype == '3-D ARRAY INTEGER(SLK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The numbers 5, 7, 6, & 8','%pdat%description')
    CALL testParam%init('testError',valslka3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SLK_a3'// &
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
    CALL testParam%init('testSLKa3',valslka3,'The numbers 5, 7, 6, & 8')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSLKa3',valslka3)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSLKa3',valslka3,'The numbers 5, 7, 6, & 8')
    CALL testParam%get('testSLKa3',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSLKa3',valslka3)
    bool=SIZE(valslka3,DIM=1) == 2 .AND. SIZE(valslka3,DIM=2) == 2 .AND. SIZE(valslka3,DIM=3) == 2
    ASSERT(bool,'someParam valslka3 size 1')
    bool=ALL(valslka3(1,1,:) == 5_SLK) .AND. ALL(valslka3(2,1,:) == 7_SLK) &
      .AND. ALL(valslka3(1,2,:) == 6_SLK) .AND. ALL(valslka3(2,2,:) == 8_SLK)
    ASSERT(bool,'someParam valslka3 1')
    DEALLOCATE(valslka3)
    ALLOCATE(valslka3(1,1,1))
    CALL someParam%get('testSLKa3',valslka3)
    bool=SIZE(valslka3,DIM=1) == 2 .AND. SIZE(valslka3,DIM=2) == 2 .AND. SIZE(valslka3,DIM=3) == 2
    ASSERT(bool,'someParam valslka3 different size')
    bool=ALL(valslka3(1,1,:) == 5_SLK) .AND. ALL(valslka3(2,1,:) == 7_SLK) &
      .AND. ALL(valslka3(1,2,:) == 6_SLK) .AND. ALL(valslka3(2,2,:) == 8_SLK)
    ASSERT(bool,'someParam valslka3 2')
    DEALLOCATE(valslka3)
    CALL someParam%get('testSLKa3',valslka3)
    bool=SIZE(valslka3,DIM=1) == 2 .AND. SIZE(valslka3,DIM=2) == 2 .AND. SIZE(valslka3,DIM=3) == 2
    ASSERT(bool,'someParam valslka3 unallocated')
    bool=ALL(valslka3(1,1,:) == 5_SLK) .AND. ALL(valslka3(2,1,:) == 7_SLK) &
      .AND. ALL(valslka3(1,2,:) == 6_SLK) .AND. ALL(valslka3(2,2,:) == 8_SLK)
    ASSERT(bool,'someParam valslka3 3')
    DEALLOCATE(valslka3)
    ALLOCATE(valslka3(2,2,1))
    CALL testParam%get('testSLKa3',valslka3)
    bool=SIZE(valslka3,DIM=1) == 2 .AND. SIZE(valslka3,DIM=2) == 2 .AND. SIZE(valslka3,DIM=3) == 2
    ASSERT(bool,'testParam valslka3 size 1')
    bool=ALL(valslka3(1,1,:) == 5_SLK) .AND. ALL(valslka3(2,1,:) == 7_SLK) &
      .AND. ALL(valslka3(1,2,:) == 6_SLK) .AND. ALL(valslka3(2,2,:) == 8_SLK)
    ASSERT(bool,'testParam valslka3 1')
    DEALLOCATE(valslka3)
    CALL testParam%get('testSLKa3',valslka3)
    bool=SIZE(valslka3,DIM=1) == 2 .AND. SIZE(valslka3,DIM=2) == 2 .AND. SIZE(valslka3,DIM=3) == 2
    ASSERT(bool,'testParam valslka3 unallocated')
    bool=ALL(valslka3(1,1,:) == 5_SLK) .AND. ALL(valslka3(2,1,:) == 7_SLK) &
      .AND. ALL(valslka3(1,2,:) == 6_SLK) .AND. ALL(valslka3(2,2,:) == 8_SLK)
    ASSERT(bool,'testParam valslka3 2')
    CALL testParam%get('testError',valslka3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SLK_a3'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valslka3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SLK_a3'// &
      ' - parameter name mismatch "testError" in "testSLKa3"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSLKa3'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSLKa3',valslka3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SLK_a3'// &
      ' - parameter data type mismatch! Parameter testSLKa3 type is test_type and'// &
      ' must be 3-D ARRAY INTEGER(SLK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSLKa3',RESHAPE((/-10_SLK/),(/1,1,1/)),'The number -10')
    CALL testParam%get('testSLKa3',valslka3)
    ASSERT(testParam%pdat%name == 'testSLKa3','%name')
    ASSERT(testParam%pdat%datatype == '3-D ARRAY INTEGER(SLK)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The number -10','%description')
    bool=SIZE(valslka3,DIM=1) == 1 .AND. SIZE(valslka3,DIM=2) == 1
    ASSERT(bool,'testParam valslka3 size 1')
    bool=valslka3(1,1,1) == -10
    ASSERT(bool,'testParam valslka3 1')
    CALL testParam%set('testSLKa3',RESHAPE((/3_SLK,-5_SLK,3_SLK,-5_SLK/),(/2,2,1/)),'The numbers 3 & -5')
    CALL testParam%get('testSLKa3',valslka3)
    bool=SIZE(valslka3,DIM=1) == 2 .AND. SIZE(valslka3,DIM=2) == 2 .AND. SIZE(valslka3,DIM=3) == 1
    ASSERT(bool,'testParam valslka3 size 2')
    bool=valslka3(1,1,1) == 3_SLK .AND. valslka3(2,1,1) == -5_SLK .AND. &
      valslka3(1,2,1) == 3_SLK .AND. valslka3(2,2,1) == -5_SLK
    ASSERT(bool,'testParam valslka3 2')
    ASSERT(testParam%pdat%description == 'The numbers 3 & -5','%description')
    !
    CALL someParam%set('testSLKa3',RESHAPE((/-10_SLK/),(/1,1,1/)),'The number -10')
    CALL someParam%get('testSLKa3',valslka3)
    ASSERT(someParam%name == 'testSLKa3','someParam%name')
    ASSERT(someParam%datatype == '3-D ARRAY INTEGER(SLK)','someParam%datatype')
    ASSERT(someParam%description == 'The number -10','someParam%description')
    bool=SIZE(valslka3,DIM=1) == 1 .AND. SIZE(valslka3,DIM=2) == 1
    ASSERT(bool,'someParam valslka3 size 1')
    bool=valslka3(1,1,1) == -10_SLK
    ASSERT(bool,'someParam valslka3 1')
    CALL someParam%set('testSLKa3',RESHAPE((/5_SLK,7_SLK/),(/1,1,2/)),'The numbers 5 & 7')
    CALL someParam%get('testSLKa3',valslka3)
    ASSERT(someParam%name == 'testSLKa3','someParam%name')
    ASSERT(someParam%datatype == '3-D ARRAY INTEGER(SLK)','someParam%datatype')
    ASSERT(someParam%description == 'The numbers 5 & 7','someParam%description')
    bool=SIZE(valslka3,DIM=1) == 1 .AND. SIZE(valslka3,DIM=2) == 1 .AND. SIZE(valslka3,DIM=3) == 2
    ASSERT(bool,'someParam valslka3 size 2')
    bool=valslka3(1,1,1) == 5_SLK .AND. valslka3(1,1,2) == 7_SLK
    ASSERT(bool,'testParam valslka3 2')
    DEALLOCATE(valslka3)
    !
    CALL someParam%set('testError',RESHAPE((/-1_SLK/),(/1,1,1/))) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SLK_a3 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSLKa3"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSLKa3',RESHAPE((/-1_SLK/),(/1,1,1/))) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SLK_a3 -'// &
      ' unable to locate parameter "testSLKa3" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSLKa3'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSLKa3',RESHAPE((/-1_SLK/),(/1,1,1/))) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SLK_a3 -'// &
      ' parameter data type mismatch! Parameter testSLKa3 type is test_type'// &
      ' and must be 3-D ARRAY INTEGER(SLK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSLKa3',RESHAPE((/-1_SLK/),(/1,1,1/)))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSLKa3','%name')
    ASSERT(testParam2%pdat%datatype == '3-D ARRAY INTEGER(SLK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSLKa3
!
!-------------------------------------------------------------------------------
!Test 3-D Array SSK support
  SUBROUTINE testSSKa3()
    LOGICAL(SBK) :: bool
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    ALLOCATE(valsska3(2,2,2))
    valsska3(1,1,:)=5.0_SSK
    valsska3(2,1,:)=7.0_SSK
    valsska3(1,2,:)=6.0_SSK
    valsska3(2,2,:)=8.0_SSK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSSKa3',valsska3,'The numbers 5, 7, 6, & 8')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SSK_a3'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSSKa3',valsska3,'The numbers 5, 7, 6, & 8')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSSKa3','%pdat%name')
    ASSERT(testParam%pdat%datatype == '3-D ARRAY REAL(SSK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The numbers 5, 7, 6, & 8','%pdat%description')
    CALL testParam%init('testError',valsska3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SSK_a3'// &
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
    CALL testParam%init('testSSKa3',valsska3,'The numbers 5, 7, 6, & 8')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSSKa3',valsska3)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSSKa3',valsska3,'The numbers 5, 7, 6, & 8')
    CALL testParam%get('testSSKa3',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSSKa3',valsska3)
    bool=SIZE(valsska3,DIM=1) == 2 .AND. SIZE(valsska3,DIM=2) == 2 .AND. SIZE(valsska3,DIM=3) == 2
    ASSERT(bool,'someParam valsska3 size 1')
    bool=ALL(valsska3(1,1,:) == 5.0_SSK) .AND. ALL(valsska3(2,1,:) == 7.0_SSK) &
      .AND. ALL(valsska3(1,2,:) == 6.0_SSK) .AND. ALL(valsska3(2,2,:) == 8.0_SSK)
    ASSERT(bool,'someParam valsska3 1')
    DEALLOCATE(valsska3)
    ALLOCATE(valsska3(1,1,1))
    CALL someParam%get('testSSKa3',valsska3)
    bool=SIZE(valsska3,DIM=1) == 2 .AND. SIZE(valsska3,DIM=2) == 2 .AND. SIZE(valsska3,DIM=3) == 2
    ASSERT(bool,'someParam valsska3 different size')
    bool=ALL(valsska3(1,1,:) == 5.0_SSK) .AND. ALL(valsska3(2,1,:) == 7.0_SSK) &
      .AND. ALL(valsska3(1,2,:) == 6.0_SSK) .AND. ALL(valsska3(2,2,:) == 8.0_SSK)
    ASSERT(bool,'someParam valsska3 2')
    DEALLOCATE(valsska3)
    CALL someParam%get('testSSKa3',valsska3)
    bool=SIZE(valsska3,DIM=1) == 2 .AND. SIZE(valsska3,DIM=2) == 2 .AND. SIZE(valsska3,DIM=3) == 2
    ASSERT(bool,'someParam valsska3 unallocated')
    bool=ALL(valsska3(1,1,:) == 5.0_SSK) .AND. ALL(valsska3(2,1,:) == 7.0_SSK) &
      .AND. ALL(valsska3(1,2,:) == 6.0_SSK) .AND. ALL(valsska3(2,2,:) == 8.0_SSK)
    ASSERT(bool,'someParam valsska3 3')
    DEALLOCATE(valsska3)
    ALLOCATE(valsska3(2,2,1))
    CALL testParam%get('testSSKa3',valsska3)
    bool=SIZE(valsska3,DIM=1) == 2 .AND. SIZE(valsska3,DIM=2) == 2 .AND. SIZE(valsska3,DIM=3) == 2
    ASSERT(bool,'testParam valsska3 size 1')
    bool=ALL(valsska3(1,1,:) == 5.0_SSK) .AND. ALL(valsska3(2,1,:) == 7.0_SSK) &
      .AND. ALL(valsska3(1,2,:) == 6.0_SSK) .AND. ALL(valsska3(2,2,:) == 8.0_SSK)
    ASSERT(bool,'testParam valsska3 1')
    DEALLOCATE(valsska3)
    CALL testParam%get('testSSKa3',valsska3)
    bool=SIZE(valsska3,DIM=1) == 2 .AND. SIZE(valsska3,DIM=2) == 2 .AND. SIZE(valsska3,DIM=3) == 2
    ASSERT(bool,'testParam valsska3 unallocated')
    bool=ALL(valsska3(1,1,:) == 5.0_SSK) .AND. ALL(valsska3(2,1,:) == 7.0_SSK) &
      .AND. ALL(valsska3(1,2,:) == 6.0_SSK) .AND. ALL(valsska3(2,2,:) == 8.0_SSK)
    ASSERT(bool,'testParam valsska3 2')
    CALL testParam%get('testError',valsska3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SSK_a3'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valsska3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SSK_a3'// &
      ' - parameter name mismatch "testError" in "testSSKa3"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSSKa3'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSSKa3',valsska3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SSK_a3'// &
      ' - parameter data type mismatch! Parameter testSSKa3 type is test_type and'// &
      ' must be 3-D ARRAY REAL(SSK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSSKa3',RESHAPE((/-10.0_SSK/),(/1,1,1/)),'The number -10')
    CALL testParam%get('testSSKa3',valsska3)
    ASSERT(testParam%pdat%name == 'testSSKa3','%name')
    ASSERT(testParam%pdat%datatype == '3-D ARRAY REAL(SSK)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The number -10','%description')
    bool=SIZE(valsska3,DIM=1) == 1 .AND. SIZE(valsska3,DIM=2) == 1
    ASSERT(bool,'testParam valsska3 size 1')
    bool=valsska3(1,1,1) == -10
    ASSERT(bool,'testParam valsska3 1')
    CALL testParam%set('testSSKa3',RESHAPE((/3.0_SSK,-5.0_SSK,3.0_SSK,-5.0_SSK/),(/2,2,1/)),'The numbers 3 & -5')
    CALL testParam%get('testSSKa3',valsska3)
    bool=SIZE(valsska3,DIM=1) == 2 .AND. SIZE(valsska3,DIM=2) == 2 .AND. SIZE(valsska3,DIM=3) == 1
    ASSERT(bool,'testParam valsska3 size 2')
    bool=valsska3(1,1,1) == 3.0_SSK .AND. valsska3(2,1,1) == -5.0_SSK .AND. &
      valsska3(1,2,1) == 3.0_SSK .AND. valsska3(2,2,1) == -5.0_SSK
    ASSERT(bool,'testParam valsska3 2')
    ASSERT(testParam%pdat%description == 'The numbers 3 & -5','%description')
    !
    CALL someParam%set('testSSKa3',RESHAPE((/-10.0_SSK/),(/1,1,1/)),'The number -10')
    CALL someParam%get('testSSKa3',valsska3)
    ASSERT(someParam%name == 'testSSKa3','someParam%name')
    ASSERT(someParam%datatype == '3-D ARRAY REAL(SSK)','someParam%datatype')
    ASSERT(someParam%description == 'The number -10','someParam%description')
    bool=SIZE(valsska3,DIM=1) == 1 .AND. SIZE(valsska3,DIM=2) == 1
    ASSERT(bool,'someParam valsska3 size 1')
    bool=valsska3(1,1,1) == -10.0_SSK
    ASSERT(bool,'someParam valsska3 1')
    CALL someParam%set('testSSKa3',RESHAPE((/5.0_SSK,7.0_SSK/),(/1,1,2/)),'The numbers 5 & 7')
    CALL someParam%get('testSSKa3',valsska3)
    ASSERT(someParam%name == 'testSSKa3','someParam%name')
    ASSERT(someParam%datatype == '3-D ARRAY REAL(SSK)','someParam%datatype')
    ASSERT(someParam%description == 'The numbers 5 & 7','someParam%description')
    bool=SIZE(valsska3,DIM=1) == 1 .AND. SIZE(valsska3,DIM=2) == 1 .AND. SIZE(valsska3,DIM=3) == 2
    ASSERT(bool,'someParam valsska3 size 2')
    bool=valsska3(1,1,1) == 5.0_SSK .AND. valsska3(1,1,2) == 7.0_SSK
    ASSERT(bool,'testParam valsska3 2')
    DEALLOCATE(valsska3)
    !
    CALL someParam%set('testError',RESHAPE((/-1.0_SSK/),(/1,1,1/))) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SSK_a3 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSSKa3"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSSKa3',RESHAPE((/-1.0_SSK/),(/1,1,1/))) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SSK_a3 -'// &
      ' unable to locate parameter "testSSKa3" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSSKa3'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSSKa3',RESHAPE((/-1.0_SSK/),(/1,1,1/))) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SSK_a3 -'// &
      ' parameter data type mismatch! Parameter testSSKa3 type is test_type'// &
      ' and must be 3-D ARRAY REAL(SSK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSSKa3',RESHAPE((/-1.0_SSK/),(/1,1,1/)))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSSKa3','%name')
    ASSERT(testParam2%pdat%datatype == '3-D ARRAY REAL(SSK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSSKa3
!
!-------------------------------------------------------------------------------
!Test 3-D Array SDK support
  SUBROUTINE testSDKa3()
    LOGICAL(SBK) :: bool
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    ALLOCATE(valsdka3(2,2,2))
    valsdka3(1,1,:)=5.0_SDK
    valsdka3(2,1,:)=7.0_SDK
    valsdka3(1,2,:)=6.0_SDK
    valsdka3(2,2,:)=8.0_SDK
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testSDKa3',valsdka3,'The numbers 5, 7, 6, & 8')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SDK_a3'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSDKa3',valsdka3,'The numbers 5, 7, 6, & 8')
    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testSDKa3','%pdat%name')
    ASSERT(testParam%pdat%datatype == '3-D ARRAY REAL(SDK)','%pdat%datatype')
    ASSERT(testParam%pdat%description == 'The numbers 5, 7, 6, & 8','%pdat%description')
    CALL testParam%init('testError',valsdka3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_SDK_a3'// &
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
    CALL testParam%init('testSDKa3',valsdka3,'The numbers 5, 7, 6, & 8')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testSDKa3',valsdka3)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%init('testSDKa3',valsdka3,'The numbers 5, 7, 6, & 8')
    CALL testParam%get('testSDKa3',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testSDKa3',valsdka3)
    bool=SIZE(valsdka3,DIM=1) == 2 .AND. SIZE(valsdka3,DIM=2) == 2 .AND. SIZE(valsdka3,DIM=3) == 2
    ASSERT(bool,'someParam valsdka3 size 1')
    bool=ALL(valsdka3(1,1,:) == 5.0_SDK) .AND. ALL(valsdka3(2,1,:) == 7.0_SDK) &
      .AND. ALL(valsdka3(1,2,:) == 6.0_SDK) .AND. ALL(valsdka3(2,2,:) == 8.0_SDK)
    ASSERT(bool,'someParam valsdka3 1')
    DEALLOCATE(valsdka3)
    ALLOCATE(valsdka3(1,1,1))
    CALL someParam%get('testSDKa3',valsdka3)
    bool=SIZE(valsdka3,DIM=1) == 2 .AND. SIZE(valsdka3,DIM=2) == 2 .AND. SIZE(valsdka3,DIM=3) == 2
    ASSERT(bool,'someParam valsdka3 different size')
    bool=ALL(valsdka3(1,1,:) == 5.0_SDK) .AND. ALL(valsdka3(2,1,:) == 7.0_SDK) &
      .AND. ALL(valsdka3(1,2,:) == 6.0_SDK) .AND. ALL(valsdka3(2,2,:) == 8.0_SDK)
    ASSERT(bool,'someParam valsdka3 2')
    DEALLOCATE(valsdka3)
    CALL someParam%get('testSDKa3',valsdka3)
    bool=SIZE(valsdka3,DIM=1) == 2 .AND. SIZE(valsdka3,DIM=2) == 2 .AND. SIZE(valsdka3,DIM=3) == 2
    ASSERT(bool,'someParam valsdka3 unallocated')
    bool=ALL(valsdka3(1,1,:) == 5.0_SDK) .AND. ALL(valsdka3(2,1,:) == 7.0_SDK) &
      .AND. ALL(valsdka3(1,2,:) == 6.0_SDK) .AND. ALL(valsdka3(2,2,:) == 8.0_SDK)
    ASSERT(bool,'someParam valsdka3 3')
    DEALLOCATE(valsdka3)
    ALLOCATE(valsdka3(2,2,1))
    CALL testParam%get('testSDKa3',valsdka3)
    bool=SIZE(valsdka3,DIM=1) == 2 .AND. SIZE(valsdka3,DIM=2) == 2 .AND. SIZE(valsdka3,DIM=3) == 2
    ASSERT(bool,'testParam valsdka3 size 1')
    bool=ALL(valsdka3(1,1,:) == 5.0_SDK) .AND. ALL(valsdka3(2,1,:) == 7.0_SDK) &
      .AND. ALL(valsdka3(1,2,:) == 6.0_SDK) .AND. ALL(valsdka3(2,2,:) == 8.0_SDK)
    ASSERT(bool,'testParam valsdka3 1')
    DEALLOCATE(valsdka3)
    CALL testParam%get('testSDKa3',valsdka3)
    bool=SIZE(valsdka3,DIM=1) == 2 .AND. SIZE(valsdka3,DIM=2) == 2 .AND. SIZE(valsdka3,DIM=3) == 2
    ASSERT(bool,'testParam valsdka3 unallocated')
    bool=ALL(valsdka3(1,1,:) == 5.0_SDK) .AND. ALL(valsdka3(2,1,:) == 7.0_SDK) &
      .AND. ALL(valsdka3(1,2,:) == 6.0_SDK) .AND. ALL(valsdka3(2,2,:) == 8.0_SDK)
    ASSERT(bool,'testParam valsdka3 2')
    CALL testParam%get('testError',valsdka3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SDK_a3'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',valsdka3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SDK_a3'// &
      ' - parameter name mismatch "testError" in "testSDKa3"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSDKa3'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testSDKa3',valsdka3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_SDK_a3'// &
      ' - parameter data type mismatch! Parameter testSDKa3 type is test_type and'// &
      ' must be 3-D ARRAY REAL(SDK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    CALL testParam%set('testSDKa3',RESHAPE((/-10.0_SDK/),(/1,1,1/)),'The number -10')
    CALL testParam%get('testSDKa3',valsdka3)
    ASSERT(testParam%pdat%name == 'testSDKa3','%name')
    ASSERT(testParam%pdat%datatype == '3-D ARRAY REAL(SDK)','testParam%datatype')
    ASSERT(testParam%pdat%description == 'The number -10','%description')
    bool=SIZE(valsdka3,DIM=1) == 1 .AND. SIZE(valsdka3,DIM=2) == 1
    ASSERT(bool,'testParam valsdka3 size 1')
    bool=valsdka3(1,1,1) == -10
    ASSERT(bool,'testParam valsdka3 1')
    CALL testParam%set('testSDKa3',RESHAPE((/3.0_SDK,-5.0_SDK,3.0_SDK,-5.0_SDK/),(/2,2,1/)),'The numbers 3 & -5')
    CALL testParam%get('testSDKa3',valsdka3)
    bool=SIZE(valsdka3,DIM=1) == 2 .AND. SIZE(valsdka3,DIM=2) == 2 .AND. SIZE(valsdka3,DIM=3) == 1
    ASSERT(bool,'testParam valsdka3 size 2')
    bool=valsdka3(1,1,1) == 3.0_SDK .AND. valsdka3(2,1,1) == -5.0_SDK .AND. &
      valsdka3(1,2,1) == 3.0_SDK .AND. valsdka3(2,2,1) == -5.0_SDK
    ASSERT(bool,'testParam valsdka3 2')
    ASSERT(testParam%pdat%description == 'The numbers 3 & -5','%description')
    !
    CALL someParam%set('testSDKa3',RESHAPE((/-10.0_SDK/),(/1,1,1/)),'The number -10')
    CALL someParam%get('testSDKa3',valsdka3)
    ASSERT(someParam%name == 'testSDKa3','someParam%name')
    ASSERT(someParam%datatype == '3-D ARRAY REAL(SDK)','someParam%datatype')
    ASSERT(someParam%description == 'The number -10','someParam%description')
    bool=SIZE(valsdka3,DIM=1) == 1 .AND. SIZE(valsdka3,DIM=2) == 1
    ASSERT(bool,'someParam valsdka3 size 1')
    bool=valsdka3(1,1,1) == -10.0_SDK
    ASSERT(bool,'someParam valsdka3 1')
    CALL someParam%set('testSDKa3',RESHAPE((/5.0_SDK,7.0_SDK/),(/1,1,2/)),'The numbers 5 & 7')
    CALL someParam%get('testSDKa3',valsdka3)
    ASSERT(someParam%name == 'testSDKa3','someParam%name')
    ASSERT(someParam%datatype == '3-D ARRAY REAL(SDK)','someParam%datatype')
    ASSERT(someParam%description == 'The numbers 5 & 7','someParam%description')
    bool=SIZE(valsdka3,DIM=1) == 1 .AND. SIZE(valsdka3,DIM=2) == 1 .AND. SIZE(valsdka3,DIM=3) == 2
    ASSERT(bool,'someParam valsdka3 size 2')
    bool=valsdka3(1,1,1) == 5.0_SDK .AND. valsdka3(1,1,2) == 7.0_SDK
    ASSERT(bool,'testParam valsdka3 2')
    DEALLOCATE(valsdka3)
    !
    CALL someParam%set('testError',RESHAPE((/-1.0_SDK/),(/1,1,1/))) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SDK_a3 -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testSDKa3"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testSDKa3',RESHAPE((/-1.0_SDK/),(/1,1,1/))) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SDK_a3 -'// &
      ' unable to locate parameter "testSDKa3" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testSDKa3'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testSDKa3',RESHAPE((/-1.0_SDK/),(/1,1,1/))) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_SDK_a3 -'// &
      ' parameter data type mismatch! Parameter testSDKa3 type is test_type'// &
      ' and must be 3-D ARRAY REAL(SDK)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testSDKa3',RESHAPE((/-1.0_SDK/),(/1,1,1/)))
    testParam2=testParam
    ASSERT(ASSOCIATED(testParam2%pdat),'ASSOCIATED %pdat')
    ASSERT(testParam2%pdat%name == 'testSDKa3','%name')
    ASSERT(testParam2%pdat%datatype == '3-D ARRAY REAL(SDK)','%datatype')
    ASSERT(testParam2 == testParam,'OPERATOR(==)')
    CALL clear_test_vars()
  ENDSUBROUTINE testSDKa3
!
!-------------------------------------------------------------------------------
!Test ParamList support
  SUBROUTINE testParamListType()
    LOGICAL(SBK) :: bool
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    !test init

    COMPONENT_TEST('%init(...)')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 1')
    ASSERT(LEN(testParam%name) == 0,'%name 1')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 1')
    ASSERT(LEN(testParam%description) == 0,'%description 1')
    CALL testParam%init('testError->testPL',testlist,'The param list')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat 2')
    ASSERT(LEN(testParam%name) == 0,'%name 2')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 2')
    ASSERT(LEN(testParam%description) == 0,'%description 2')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_List'// &
      ' - "->" symbol is not allowed in name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'init bad symbol error')
    CALL testParam%init('testSSK',valssk,'The number 5.0')
    testList(1)=testParam
    CALL testParam%clear()
    CALL testParam%init('testPL',testList,'The param list')

    ASSERT(LEN(testParam%name) == 0,'%name 3')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype 3')
    ASSERT(LEN(testParam%description) == 0,'%description 3')
    ASSERT(ASSOCIATED(testParam%pdat),'%pdat 3')
    ASSERT(testParam%pdat%name == 'testPL','%pdat%name')
    ASSERT(testParam%pdat%datatype == 'TYPE(ParamType_List)','%pdat%datatype')
    bool=TRIM(testParam%pdat%description) == 'The param list'
    ASSERT(bool,'%pdat%description')
    CALL testParam%init('testError',testList)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::init_ParamType_List'// &
      ' - parameter  is already initialized! Use set method!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'double init error')

    !Test clear
    COMPONENT_TEST('%clear()')
    CALL testParam%clear()
    ASSERT(LEN(testParam%name) == 0,'%name')
    ASSERT(LEN(testParam%datatype) == 0,'%datatype')
    ASSERT(LEN(testParam%description) == 0,'%description')
    ASSERT(.NOT.ASSOCIATED(testParam%pdat),'%pdat')

    COMPONENT_TEST('%edit(...)')
    CALL testParam%init('testSSK',5.0_SSK,'The number 5.0')
    testList(1)=testParam
    CALL testParam%clear()
    CALL testParam%init('testSDK',5.0_SDK)
    testList(2)=testParam
    CALL testParam%clear()
    CALL testParam%init('testPL',testList,'test edit')
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%init('testPL',testList)
    CALL testParam%edit(OUTPUT_UNIT,0)
    CALL testParam%clear()
    CALL testParam%edit(OUTPUT_UNIT,0)

    COMPONENT_TEST('%get(...)')
    CALL testParam%get('',someParam)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType'// &
      ' - cannot search for a blank name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL testParam%init('testPL',testList,'The param lists')
    CALL testParam%get('testPL',someParam)
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'someParam')
    CALL someParam%get('testPL',testList2)
    bool=testList(1) == testList2(1) .AND. testList(2) == testList2(2)
    ASSERT(bool,'someParam param lists')
    CALL testList2(1)%clear()
    CALL testList2(2)%clear()
    CALL testParam%get('testPL',testList2)
    bool=testList(1) == testList2(1) .AND. testList(2) == testList2(2)
    ASSERT(bool,'testParam param lists')
    CALL testParam%get('testError',testList2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_List'// &
      ' - unable to locate parameter "testError" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'not found error')
    CALL someParam%get('testError',testList2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_List'// &
      ' - parameter name mismatch "testError" in "testPL"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'name mismatch error')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testPL'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%get('testPL',testList2)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::get_ParamType_List'// &
      ' - parameter data type mismatch! Parameter testPL type is test_type and'// &
      ' must be TYPE(ParamType_List)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'type mismatch error')
    CALL testParam2%clear()

    !test set
    COMPONENT_TEST('%set(...)')
    testList2(1)=testList(1)
    testList2(2)=testList(1)
    CALL testParam%set('testPL',testList2,'The new param list')
    CALL testList2(1)%clear()
    CALL testList2(2)%clear()
    CALL testParam%get('testPL',testList2)
    bool=testList(1) == testList2(1) .AND. testList(1) == testList2(2)
    ASSERT(bool,'param list')
    ASSERT(testParam%pdat%name == 'testPL','%name')
    ASSERT(testParam%pdat%description == 'The new param list','%description')
    testList2(1)=testList(1)
    testList2(2)=testList(1)
    testList2(3)=testList(2)
    CALL someParam%set('testPL',testList2,'The bigger param list')
    CALL testList2(1)%clear()
    CALL testList2(2)%clear()
    CALL testList2(3)%clear()
    CALL someParam%get('testPL',testList2)
    bool=testList(1) == testList2(1) .AND. testList(1) == testList2(2) .AND. testList(2) == testList2(3)
    ASSERT(bool,'param list')
    ASSERT(someParam%name == 'testPL','someParam%name')
    ASSERT(someParam%datatype == 'TYPE(ParamType_List)','%pdat%datatype')
    ASSERT(someParam%description == 'The bigger param list','someParam%description')
    CALL someParam%set('testError',testList2) !Name mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_List -'// &
      ' parameter name mismatch! Tried to set "testError" but name is'// &
      ' "testPL"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name mismatch error')
    CALL testParam2%set('testPL',testList2) !Name not found
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_List -'// &
      ' unable to locate parameter "testPL" in ""!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Name not found error!')
    ALLOCATE(testParam2%pdat)
    testParam2%pdat%name='testPL'
    testParam2%pdat%datatype='test_type'
    CALL testParam2%set('testPL',testList2) !Type mismatch
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::set_ParamType_List -'// &
      ' parameter data type mismatch! Parameter testPL type is test_type'// &
      ' and must be TYPE(ParamType_List)!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'Type mismatch error')
    CALL testParam2%clear()

    COMPONENT_TEST('Operators')
    CALL testParam%init('testPL',testList2)
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
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg
    LOGICAL(SBK) :: bool,sbk0
    LOGICAL(SBK),ALLOCATABLE :: sbk1(:)
    INTEGER(SNK) :: snk0
    INTEGER(SNK),ALLOCATABLE :: snk1(:),snk2(:,:),snk3(:,:,:)
    INTEGER(SLK) :: slk0
    INTEGER(SLK),ALLOCATABLE :: slk1(:),slk2(:,:),slk3(:,:,:)
    REAL(SSK) :: ssk0
    REAL(SSK),ALLOCATABLE :: ssk1(:),ssk2(:,:),ssk3(:,:,:)
    REAL(SDK) :: sdk0
    REAL(SDK),ALLOCATABLE :: sdk1(:),sdk2(:,:),sdk3(:,:,:)
    TYPE(StringType) :: str0
    TYPE(StringType),ALLOCATABLE :: str1(:),str2(:,:)


    !Testing addition of SBK routine to parameter list
    CALL testParam%add('testPL->testSBK',.TRUE.)
    CALL testParam%add('testPL->testSBK2',.FALSE.,'comment')
    CALL testParam%add('testPL->testSBK2',.FALSE.,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SBK -'// &
      ' parameter name "testPL->testSBK2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSBK',sbk0)
    ASSERT(sbk0,'%add sbk0')
    CALL testParam%get('testPL->testSBK2',sbk0)
    ASSERT(.NOT.sbk0,'%add sbk0')

    !Testing addition of SSK routine to parameter list
    CALL testParam%add('testPL->testSSK',7.0_SSK)
    CALL testParam%add('testPL->testSSK2',8.0_SSK,'comment')
    CALL testParam%add('testPL->testSSK2',8.0_SSK,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SSK -'// &
      ' parameter name "testPL->testSSK2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSSK',ssk0)
    ASSERT(ssk0 .APPROXEQA. 7.0_SSK,'%add ssk0')
    CALL testParam%get('testPL->testSSK2',ssk0)
    ASSERT(ssk0 .APPROXEQA. 8.0_SSK,'%add ssk0')

    !Testing addition of SDK routine to parameter list
    CALL testParam%add('testPL->testSDK',7.0_SDK)
    CALL testParam%add('testPL->testSDK2',8.0_SDK,'comment')
    CALL testParam%add('testPL->testSDK2',8.0_SDK,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SDK -'// &
      ' parameter name "testPL->testSDK2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSDK',sdk0)
    ASSERT(sdk0 .APPROXEQA. 7.0_SDK,'%add sdk0')
    CALL testParam%get('testPL->testSDK2',sdk0)
    ASSERT(sdk0 .APPROXEQA. 8.0_SDK,'%add sdk0')

    !Testing addition of SNK routine to parameter list
    CALL testParam%add('testPL->testSNK',7_SNK)
    CALL testParam%add('testPL->testSNK2',8_SNK,'comment')
    CALL testParam%add('testPL->testSNK2',8_SNK,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SNK -'// &
      ' parameter name "testPL->testSNK2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSNK',snk0)
    ASSERT(snk0 == 7_SNK,'%add snk0')
    CALL testParam%get('testPL->testSNK2',snk0)
    ASSERT(snk0 == 8_SNK,'%add snk0')

    !Testing addition of SLK routine to parameter list
    CALL testParam%add('testPL->testSLK',7_SLK)
    CALL testParam%add('testPL->testSLK2',8_SLK,'comment')
    CALL testParam%add('testPL->testSLK2',8_SLK,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SLK -'// &
      ' parameter name "testPL->testSLK2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSLK',slk0)
    ASSERT(slk0 == 7_SLK,'%add slk0')
    CALL testParam%get('testPL->testSLK2',slk0)
    ASSERT(slk0 == 8_SLK,'%add slk0')

    !Testing addition of STR routine to parameter list
    str0='string1'
    CALL testParam%add('testPL->testSTR',str0)
    str0='string2'
    CALL testParam%add('testPL->testSTR2',str0,'comment')
    CALL testParam%add('testPL->testSTR2',str0,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_STR -'// &
      ' parameter name "testPL->testSTR2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSTR',str0)
    ASSERT(str0 == 'string1','%add str0')
    CALL testParam%get('testPL->testSTR2',str0)
    ASSERT(str0 == 'string2','%add str0')

    !Testing addition of CHAR routine to parameter list
    CALL testParam%add('testPL->testCHAR','char1')
    CALL testParam%add('testPL->testCHAR2','char2','comment')
    CALL testParam%add('testPL->testCHAR2','char2','comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_STR -'// &
      ' parameter name "testPL->testCHAR2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testCHAR',str0)
    ASSERT(str0 == 'char1','%add char')
    CALL testParam%get('testPL->testCHAR2',str0)
    ASSERT(str0 == 'char2','%add char')

    !Testing addition of 1-D array SSK routine to parameter list
    CALL testParam%add('testPL->testSSKa1',(/1.5_SSK,1.6_SSK/))
    CALL testParam%add('testPL->testSSKa1_2',(/1.7_SSK,1.8_SSK/),'comment')
    CALL testParam%add('testPL->testSSKa1_2',(/1.7_SSK,1.8_SSK/),'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SSK_a1 -'// &
      ' parameter name "testPL->testSSKa1_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSSKa1',ssk1)
    ASSERT(ALL(ssk1 .APPROXEQA. (/1.5_SSK,1.6_SSK/)),'%add ssk1')
    CALL testParam%get('testPL->testSSKa1_2',ssk1)
    ASSERT(ALL(ssk1 .APPROXEQA. (/1.7_SSK,1.8_SSK/)),'%add ssk1')

    !Testing addition of 1-D array SDK routine to parameter list
    CALL testParam%add('testPL->testSDKa1',(/2.5_SDK,2.6_SDK/))
    CALL testParam%add('testPL->testSDKa1_2',(/2.7_SDK,2.8_SDK/),'comment')
    CALL testParam%add('testPL->testSDKa1_2',(/2.7_SDK,2.8_SDK/),'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SDK_a1 -'// &
      ' parameter name "testPL->testSDKa1_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSDKa1',sdk1)
    ASSERT(ALL(sdk1 .APPROXEQA. (/2.5_SDK,2.6_SDK/)),'%add sdk1')
    CALL testParam%get('testPL->testSDKa1_2',sdk1)
    ASSERT(ALL(sdk1 .APPROXEQA. (/2.7_SDK,2.8_SDK/)),'%add sdk1')

    !Testing addition of 1-D array SNK routine to parameter list
    CALL testParam%add('testPL->testSNKa1',(/-2_SNK,-3_SNK/))
    CALL testParam%add('testPL->testSNKa1_2',(/2_SNK,3_SNK/),'comment')
    CALL testParam%add('testPL->testSNKa1_2',(/2_SNK,3_SNK/),'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SNK_a1 -'// &
      ' parameter name "testPL->testSNKa1_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSNKa1',snk1)
    ASSERT(ALL(snk1 == (/-2_SNK,-3_SNK/)),'%add snk1')
    CALL testParam%get('testPL->testSNKa1_2',snk1)
    ASSERT(ALL(snk1 == (/2_SNK,3_SNK/)),'%add snk1')

    !Testing addition of 1-D array SLK routine to parameter list
    CALL testParam%add('testPL->testSLKa1',(/-4_SLK,-5_SLK/))
    CALL testParam%add('testPL->testSLKa1_2',(/4_SLK,5_SLK/),'comment')
    CALL testParam%add('testPL->testSLKa1_2',(/4_SLK,5_SLK/),'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SLK_a1 -'// &
      ' parameter name "testPL->testSLKa1_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSLKa1',slk1)
    ASSERT(ALL(slk1 == (/-4_SLK,-5_SLK/)),'%add slk1')
    CALL testParam%get('testPL->testSLKa1_2',slk1)
    ASSERT(ALL(slk1 == (/4_SLK,5_SLK/)),'%add slk1')

    !Testing addition of 1-D array SBK routine to parameter list
    CALL testParam%add('testPL->testSBKa1',(/.TRUE.,.FALSE./))
    CALL testParam%add('testPL->testSBKa1_2',(/.FALSE.,.FALSE./),'comment')
    CALL testParam%add('testPL->testSBKa1_2',(/.FALSE.,.FALSE./),'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SBK_a1 -'// &
      ' parameter name "testPL->testSBKa1_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSBKa1',sbk1)
    ASSERT(sbk1(1) .AND. .NOT.sbk1(2),'%add sbk1')
    CALL testParam%get('testPL->testSBKa1_2',sbk1)
    ASSERT(.NOT.sbk1(1) .AND. .NOT.sbk1(2),'%add sbk1')

    !Testing addition of 1-D array STR routine to parameter list
    ALLOCATE(str1(2))
    str1(1)='stringarray1'
    str1(2)='stringarray2'
    CALL testParam%add('testPL->testSTRa1',str1)
    str1(1)='stringarray3'
    str1(2)='stringarray4'
    CALL testParam%add('testPL->testSTRa1_2',str1,'comment')
    CALL testParam%add('testPL->testSTRa1_2',str1,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_STR_a1 -'// &
      ' parameter name "testPL->testSTRa1_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSTRa1',str1)
    bool=str1(1) == 'stringarray1' .AND. str1(2) == 'stringarray2'
    ASSERT(bool,'%add str1')
    CALL testParam%get('testPL->testSTRa1_2',str1)
    bool=str1(1) == 'stringarray3' .AND. str1(2) == 'stringarray4'
    ASSERT(bool,'%add str1')

    !Testing addition of 2-D array SSK routine to parameter list
    ALLOCATE(ssk2(2,2))
    ssk2(1,1)=1.1_SSK
    ssk2(2,1)=2.1_SSK
    ssk2(1,2)=1.2_SSK
    ssk2(2,2)=2.2_SSK
    CALL testParam%add('testPL->testSSKa2',ssk2)
    ssk2(1,1)=3.1_SSK
    ssk2(2,1)=4.1_SSK
    ssk2(1,2)=3.2_SSK
    ssk2(2,2)=4.2_SSK
    CALL testParam%add('testPL->testSSKa2_2',ssk2,'comment')
    CALL testParam%add('testPL->testSSKa2_2',ssk2,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SSK_a2 -'// &
      ' parameter name "testPL->testSSKa2_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSSKa2',ssk2)
    bool=ALL(ssk2 == RESHAPE((/1.1_SSK,2.1_SSK,1.2_SSK,2.2_SSK/),(/2,2/)))
    ASSERT(bool,'%add ssk2')
    CALL testParam%get('testPL->testSSKa2_2',ssk2)
    bool=ALL(ssk2 == RESHAPE((/3.1_SSK,4.1_SSK,3.2_SSK,4.2_SSK/),(/2,2/)))
    ASSERT(bool,'%add ssk2')

    !Testing addition of 2-D array SDK routine to parameter list
    ALLOCATE(sdk2(2,2))
    sdk2(1,1)=11.0_SDK
    sdk2(2,1)=21.0_SDK
    sdk2(1,2)=12.0_SDK
    sdk2(2,2)=22.0_SDK
    CALL testParam%add('testPL->testSDKa2',sdk2)
    sdk2(1,1)=31.0_SDK
    sdk2(2,1)=41.0_SDK
    sdk2(1,2)=32.0_SDK
    sdk2(2,2)=42.0_SDK
    CALL testParam%add('testPL->testSDKa2_2',sdk2,'comment')
    CALL testParam%add('testPL->testSDKa2_2',sdk2,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SDK_a2 -'// &
      ' parameter name "testPL->testSDKa2_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSDKa2',sdk2)
    bool=ALL(sdk2 == RESHAPE((/11._SDK,21._SDK,12._SDK,22._SDK/),(/2,2/)))
    ASSERT(bool,'%add sdk2')
    CALL testParam%get('testPL->testSDKa2_2',sdk2)
    bool=ALL(sdk2 == RESHAPE((/31._SDK,41._SDK,32._SDK,42._SDK/),(/2,2/)))
    ASSERT(bool,'%add sdk2')

    !Testing addition of 2-D array SNK routine to parameter list
    ALLOCATE(snk2(2,2))
    snk2(1,1)=11
    snk2(2,1)=21
    snk2(1,2)=12
    snk2(2,2)=22
    CALL testParam%add('testPL->testSNKa2',snk2)
    snk2(1,1)=31
    snk2(2,1)=41
    snk2(1,2)=32
    snk2(2,2)=42
    CALL testParam%add('testPL->testSNKa2_2',snk2,'comment')
    CALL testParam%add('testPL->testSNKa2_2',snk2,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SNK_a2 -'// &
      ' parameter name "testPL->testSNKa2_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSNKa2',snk2)
    bool=ALL(snk2 == RESHAPE((/11_SNK,21_SNK,12_SNK,22_SNK/),(/2,2/)))
    ASSERT(bool,'%add snk2')
    CALL testParam%get('testPL->testSNKa2_2',snk2)
    bool=ALL(snk2 == RESHAPE((/31_SNK,41_SNK,32_SNK,42_SNK/),(/2,2/)))
    ASSERT(bool,'%add snk2')

    !Testing addition of 2-D array SLK routine to parameter list
    ALLOCATE(slk2(2,2))
    slk2(1,1)=110
    slk2(2,1)=210
    slk2(1,2)=120
    slk2(2,2)=220
    CALL testParam%add('testPL->testSLKa2',slk2)
    slk2(1,1)=310
    slk2(2,1)=410
    slk2(1,2)=320
    slk2(2,2)=420
    CALL testParam%add('testPL->testSLKa2_2',slk2,'comment')
    CALL testParam%add('testPL->testSLKa2_2',slk2,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SLK_a2 -'// &
      ' parameter name "testPL->testSLKa2_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSLKa2',slk2)
    bool=ALL(slk2 == RESHAPE((/110_SLK,210_SLK,120_SLK,220_SLK/),(/2,2/)))
    ASSERT(bool,'%add slk2')
    CALL testParam%get('testPL->testSLKa2_2',slk2)
    bool=ALL(slk2 == RESHAPE((/310_SLK,410_SLK,320_SLK,420_SLK/),(/2,2/)))
    ASSERT(bool,'%add slk2')

    !Testing addition of 2-D array STR routine to parameter list
    ALLOCATE(str2(2,2))
    str2(1,1)='stringarray1'
    str2(2,1)='stringarray2'
    str2(1,2)='stringarray3'
    str2(2,2)='stringarray4'
    CALL testParam%add('testPL->testSTRa2',str2)
    str2(1,1)='stringarray5'
    str2(2,1)='stringarray6'
    str2(1,2)='stringarray7'
    str2(2,2)='stringarray8'
    CALL testParam%add('testPL->testSTRa2_2',str2,'comment')
    CALL testParam%add('testPL->testSTRa2_2',str2,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_STR_a2 -'// &
      ' parameter name "testPL->testSTRa2_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSTRa2',str2)
    bool=str2(1,1) == 'stringarray1' .AND. str2(2,1) == 'stringarray2' .AND. &
      str2(1,2) == 'stringarray3' .AND. str2(2,2) == 'stringarray4'
    ASSERT(bool,'%add str2')
    CALL testParam%get('testPL->testSTRa2_2',str2)
    bool=str2(1,1) == 'stringarray5' .AND. str2(2,1) == 'stringarray6' .AND. &
      str2(1,2) == 'stringarray7' .AND. str2(2,2) == 'stringarray8'
    ASSERT(bool,'%add str2')

    !Testing addition of 3-D array SSK routine to parameter list
    ALLOCATE(ssk3(2,2,2))
    ssk3(1,1,1)=1.11_SSK
    ssk3(2,1,1)=2.11_SSK
    ssk3(1,2,1)=1.21_SSK
    ssk3(2,2,1)=2.21_SSK
    ssk3(1,1,2)=1.12_SSK
    ssk3(2,1,2)=2.12_SSK
    ssk3(1,2,2)=1.22_SSK
    ssk3(2,2,2)=2.22_SSK
    CALL testParam%add('testPL->testSSKa3',ssk3)
    ssk3(1,1,1)=3.11_SSK
    ssk3(2,1,1)=4.11_SSK
    ssk3(1,2,1)=3.21_SSK
    ssk3(2,2,1)=4.21_SSK
    ssk3(1,1,2)=3.12_SSK
    ssk3(2,1,2)=4.12_SSK
    ssk3(1,2,2)=3.22_SSK
    ssk3(2,2,2)=4.22_SSK
    CALL testParam%add('testPL->testSSKa3_2',ssk3,'comment')
    CALL testParam%add('testPL->testSSKa3_2',ssk3,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SSK_a3 -'// &
      ' parameter name "testPL->testSSKa3_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSSKa3',ssk3)
    bool=ALL(ssk3 .APPROXEQA. RESHAPE((/1.11_SSK,2.11_SSK,1.21_SSK,2.21_SSK, &
      1.12_SSK,2.12_SSK,1.22_SSK,2.22_SSK/),(/2,2,2/)))
    ASSERT(bool,'%add ssk3')
    CALL testParam%get('testPL->testSSKa3_2',ssk3)
    bool=ALL(ssk3 .APPROXEQA. RESHAPE((/3.11_SSK,4.11_SSK,3.21_SSK,4.21_SSK, &
      3.12_SSK,4.12_SSK,3.22_SSK,4.22_SSK/),(/2,2,2/)))
    ASSERT(bool,'%add ssk3')

    !Testing addition of 3-D array SDK routine to parameter list
    ALLOCATE(sdk3(2,2,2))
    sdk3(1,1,1)=11.1_SDK
    sdk3(2,1,1)=21.1_SDK
    sdk3(1,2,1)=12.1_SDK
    sdk3(2,2,1)=22.1_SDK
    sdk3(1,1,2)=11.2_SDK
    sdk3(2,1,2)=21.2_SDK
    sdk3(1,2,2)=12.2_SDK
    sdk3(2,2,2)=22.2_SDK
    CALL testParam%add('testPL->testSDKa3',sdk3)
    sdk3(1,1,1)=31.1_SDK
    sdk3(2,1,1)=41.1_SDK
    sdk3(1,2,1)=32.1_SDK
    sdk3(2,2,1)=42.1_SDK
    sdk3(1,1,2)=31.2_SDK
    sdk3(2,1,2)=41.2_SDK
    sdk3(1,2,2)=32.2_SDK
    sdk3(2,2,2)=42.2_SDK
    CALL testParam%add('testPL->testSDKa3_2',sdk3,'comment')
    CALL testParam%add('testPL->testSDKa3_2',sdk3,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SDK_a3 -'// &
      ' parameter name "testPL->testSDKa3_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSDKa3',sdk3)
    bool=ALL(sdk3 .APPROXEQA. RESHAPE((/11.1_SDK,21.1_SDK,12.1_SDK,22.1_SDK, &
      11.2_SDK,21.2_SDK,12.2_SDK,22.2_SDK/),(/2,2,2/)))
    ASSERT(bool,'%add sdk3')
    CALL testParam%get('testPL->testSDKa3_2',sdk3)
    bool=ALL(sdk3 .APPROXEQA. RESHAPE((/31.1_SDK,41.1_SDK,32.1_SDK,42.1_SDK, &
      31.2_SDK,41.2_SDK,32.2_SDK,42.2_SDK/),(/2,2,2/)))
    ASSERT(bool,'%add sdk3')

    !Testing addition of 3-D array SNK routine to parameter list
    ALLOCATE(snk3(2,2,2))
    snk3(1,1,1)=111
    snk3(2,1,1)=211
    snk3(1,2,1)=121
    snk3(2,2,1)=221
    snk3(1,1,2)=112
    snk3(2,1,2)=212
    snk3(1,2,2)=122
    snk3(2,2,2)=222
    CALL testParam%add('testPL->testSNKa3',snk3)
    snk3(1,1,1)=311
    snk3(2,1,1)=411
    snk3(1,2,1)=321
    snk3(2,2,1)=421
    snk3(1,1,2)=312
    snk3(2,1,2)=412
    snk3(1,2,2)=322
    snk3(2,2,2)=422
    CALL testParam%add('testPL->testSNKa3_2',snk3,'comment')
    CALL testParam%add('testPL->testSNKa3_2',snk3,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SNK_a3 -'// &
      ' parameter name "testPL->testSNKa3_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSNKa3',snk3)
    bool=ALL(snk3 == RESHAPE((/111_SNK,211_SNK,121_SNK,221_SNK, &
      112_SNK,212_SNK,122_SNK,222_SNK/),(/2,2,2/)))
    ASSERT(bool,'%add snk3')
    CALL testParam%get('testPL->testSNKa3_2',snk3)
    bool=ALL(snk3 == RESHAPE((/311_SNK,411_SNK,321_SNK,421_SNK, &
      312_SNK,412_SNK,322_SNK,422_SNK/),(/2,2,2/)))
    ASSERT(bool,'%add snk3')

    !Testing addition of 3-D array SLK routine to parameter list
    ALLOCATE(slk3(2,2,2))
    slk3(1,1,1)=111
    slk3(2,1,1)=211
    slk3(1,2,1)=121
    slk3(2,2,1)=221
    slk3(1,1,2)=112
    slk3(2,1,2)=212
    slk3(1,2,2)=122
    slk3(2,2,2)=222
    CALL testParam%add('testPL->testSLKa3',slk3)
    slk3(1,1,1)=311
    slk3(2,1,1)=411
    slk3(1,2,1)=321
    slk3(2,2,1)=421
    slk3(1,1,2)=312
    slk3(2,1,2)=412
    slk3(1,2,2)=322
    slk3(2,2,2)=422
    CALL testParam%add('testPL->testSLKa3_2',slk3,'comment')
    CALL testParam%add('testPL->testSLKa3_2',slk3,'comment')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_SLK_a3 -'// &
      ' parameter name "testPL->testSLKa3_2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add')
    CALL testParam%get('testPL->testSLKa3',slk3)
    bool=ALL(slk3 == RESHAPE((/111_SLK,211_SLK,121_SLK,221_SLK, &
      112_SLK,212_SLK,122_SLK,222_SLK/),(/2,2,2/)))
    ASSERT(bool,'%add slk3')
    CALL testParam%get('testPL->testSLKa3_2',slk3)
    bool=ALL(slk3 == RESHAPE((/311_SLK,411_SLK,321_SLK,421_SLK, &
      312_SLK,412_SLK,322_SLK,422_SLK/),(/2,2,2/)))
    ASSERT(bool,'%add slk3')

    !CALL testList(1)%add('item 1',0)
    !CALL testList(2)%add('item 2',0)
    CALL testParam%add('testPL->testList',testList,'this is a test list')
    CALL testParam%add('testPL->testList2',testList2)
    CALL testParam%add('testPL->testList2',testList)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType_List -'// &
      ' parameter name "testPL->testList2" already exists! Use set method or full'// &
        ' parameter list path!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add (list)')
    CALL testParam%get('testPL->testList',someParam)
    ASSERT(someParam%name == 'testList','%addList (name)')
    ASSERT(someParam%datatype == 'TYPE(ParamType_List)','%addList (datatype)')

    CALL clear_test_vars()
    !Deallocate locals
    DEALLOCATE(sbk1)
    DEALLOCATE(snk1)
    DEALLOCATE(snk2)
    DEALLOCATE(snk3)
    DEALLOCATE(slk1)
    DEALLOCATE(slk2)
    DEALLOCATE(slk3)
    DEALLOCATE(ssk1)
    DEALLOCATE(ssk2)
    DEALLOCATE(ssk3)
    DEALLOCATE(sdk1)
    DEALLOCATE(sdk2)
    DEALLOCATE(sdk3)
    DEALLOCATE(str1)
    DEALLOCATE(str2)

    COMPONENT_TEST('Misplaced sublist')
    CALL testParam%add('A -> B',1)
    CALL testParam%add('A -> C',1.0_SRK)
    CALL testParam%add('A -> Sub A -> D',1)
    CALL testParam%add('A -> Sub A -> Sub B -> Sub C -> X',1.0_SRK)
    CALL testParam%add('A -> Sub A -> Sub B -> Sub C -> Y',0.0_SRK)
    CALL testParam%add('A -> Sub A -> Sub B -> Sub C -> Sub D -> n',5)
    CALL testParam%add('A -> Sub D -> bad location',0)
    ASSERT(testParam%has('A -> Sub D -> bad location'),'bad location')
    bool=testParam%has('A -> Sub A -> Sub B -> Sub C -> Sub D -> bad location')
    ASSERT(.NOT.bool,'not bad location')
    CALL testParam%clear()


    CALL testParam%add('A -> B',1)
    CALL testParam%add('A -> C',1.0_SRK)
    CALL testParam%add('A -> Sub A -> D',1)
    CALL testParam%add('A -> Sub A -> Sub B -> Sub C -> X',1.0_SRK)
    CALL testParam%add('A -> Sub A -> Sub B -> Sub C -> Y',0.0_SRK)
    CALL testParam%add('A -> Sub A -> Sub B -> Sub C -> Sub D -> n',5)
    CALL testParam2%add('someParam -> x',0)
    CALL testParam2%add('someParam -> y',1)
    CALL testParam2%get('someParam',someParam)
    CALL testParam%add('A -> Sub D -> bad location 2',someParam)
    ASSERT(testParam%has('A -> Sub D -> bad location 2'),'bad location 2')
    bool=testParam%has('A -> Sub A -> Sub B -> Sub C -> Sub D -> bad location 2')
    ASSERT(.NOT.bool,'not bad location 2')
    !Redundant Add
    CALL testParam%add('A -> Sub D -> bad location 2',someParam)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::add_ParamType -'// &
      ' parameter name "SOMEPARAM" already exists! Use set method!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'redundant add (param)')
    CALL testParam%clear()
    CALL testParam2%clear()
    NULLIFY(someParam)

    COMPONENT_TEST('Sublist not added')
    CALL testParam%add('A->C',1)
    CALL testParam2%add('B000001->p',1)
    CALL testParam%add('A->',testParam2)
    CALL testParam%edit(0)
    CALL testParam2%clear()
    CALL testParam2%add('B000002->p',2)
    CALL testParam%add('A->',testParam2)
    CALL testParam%edit(0)
    bool=testParam%has('A->B000002')
    ASSERT(bool,'not added')
    CALL clear_test_vars()
  ENDSUBROUTINE testAdd
!
!-------------------------------------------------------------------------------
  SUBROUTINE testGetString()
    LOGICAL(SBK) :: tmpbool
    INTEGER(SNK),ALLOCATABLE :: snk2(:,:),snk3(:,:,:)
    INTEGER(SLK),ALLOCATABLE :: slk2(:,:),slk3(:,:,:)
    REAL(SSK),ALLOCATABLE :: ssk2(:,:),ssk3(:,:,:)
    REAL(SDK),ALLOCATABLE :: sdk2(:,:),sdk3(:,:,:)
    TYPE(StringType) :: tmpstr,str0
    TYPE(StringType),ALLOCATABLE :: str1(:),str2(:,:),str3(:,:,:)
    TYPE(StringType),ALLOCATABLE :: tmpstr1(:),tmpstr2(:,:),tmpstr3(:,:,:)


    !Testing addition of SBK routine to parameter list
    CALL testParam%add('testPL->testSBK',.TRUE.)
    CALL testParam%add('testPL->testSBK2',.FALSE.,'comment')
    CALL testParam%add('testPL->testSSK',7.0_SSK)
    CALL testParam%add('testPL->testSSK2',8.0_SSK,'comment')
    CALL testParam%add('testPL->testSDK',7.0_SDK)
    CALL testParam%add('testPL->testSDK2',8.0_SDK,'comment')
    CALL testParam%add('testPL->testSNK',7_SNK)
    CALL testParam%add('testPL->testSNK2',8_SNK,'comment')
    CALL testParam%add('testPL->testSLK',7_SLK)
    CALL testParam%add('testPL->testSLK2',8_SLK,'comment')
    str0='string1'
    CALL testParam%add('testPL->testSTR',str0)
    str0='string2'
    CALL testParam%add('testPL->testSTR2',str0,'comment')
    CALL testParam%add('testPL->testCHAR','char1')
    CALL testParam%add('testPL->testCHAR2','char2','comment')
    !1-D
    CALL testParam%add('testPL->testSSKa1',(/1.5_SSK,1.6_SSK/))
    CALL testParam%add('testPL->testSSKa1_2',(/1.7_SSK,1.8_SSK/),'comment')
    CALL testParam%add('testPL->testSDKa1',(/2.5_SDK,2.6_SDK/))
    CALL testParam%add('testPL->testSDKa1_2',(/2.7_SDK,2.8_SDK/),'comment')
    CALL testParam%add('testPL->testSNKa1',(/-2_SNK,-3_SNK/))
    CALL testParam%add('testPL->testSNKa1_2',(/2_SNK,3_SNK/),'comment')
    CALL testParam%add('testPL->testSLKa1',(/-4_SLK,-5_SLK/))
    CALL testParam%add('testPL->testSLKa1_2',(/4_SLK,5_SLK/),'comment')
    CALL testParam%add('testPL->testSBKa1',(/.TRUE.,.FALSE./))
    CALL testParam%add('testPL->testSBKa1_2',(/.FALSE.,.FALSE./),'comment')
    ALLOCATE(str1(2))
    str1(1)='stringarray1'; str1(2)='stringarray2'
    CALL testParam%add('testPL->testSTRa1',str1)
    str1(1)='stringarray3'; str1(2)='stringarray4'
    CALL testParam%add('testPL->testSTRa1_2',str1,'comment')
    !2-D
    ALLOCATE(ssk2(2,2))
    ssk2(1,1)=1.1_SSK; ssk2(2,1)=2.1_SSK
    ssk2(1,2)=1.2_SSK; ssk2(2,2)=2.2_SSK
    CALL testParam%add('testPL->testSSKa2',ssk2)
    ssk2(1,1)=3.1_SSK; ssk2(2,1)=4.1_SSK
    ssk2(1,2)=3.2_SSK; ssk2(2,2)=4.2_SSK
    CALL testParam%add('testPL->testSSKa2_2',ssk2,'comment')
    ALLOCATE(sdk2(2,2))
    sdk2(1,1)=11.0_SDK; sdk2(2,1)=21.0_SDK
    sdk2(1,2)=12.0_SDK; sdk2(2,2)=22.0_SDK
    CALL testParam%add('testPL->testSDKa2',sdk2)
    sdk2(1,1)=31.0_SDK; sdk2(2,1)=41.0_SDK
    sdk2(1,2)=32.0_SDK; sdk2(2,2)=42.0_SDK
    CALL testParam%add('testPL->testSDKa2_2',sdk2,'comment')
    ALLOCATE(snk2(2,2))
    snk2(1,1)=11; snk2(2,1)=21
    snk2(1,2)=12; snk2(2,2)=22
    CALL testParam%add('testPL->testSNKa2',snk2)
    snk2(1,1)=31; snk2(2,1)=41
    snk2(1,2)=32; snk2(2,2)=42
    CALL testParam%add('testPL->testSNKa2_2',snk2,'comment')
    ALLOCATE(slk2(2,2))
    slk2(1,1)=110; slk2(2,1)=210
    slk2(1,2)=120; slk2(2,2)=220
    CALL testParam%add('testPL->testSLKa2',slk2)
    slk2(1,1)=310; slk2(2,1)=410
    slk2(1,2)=320; slk2(2,2)=420
    CALL testParam%add('testPL->testSLKa2_2',slk2,'comment')
    ALLOCATE(str2(2,2))
    str2(1,1)='stringarray1'; str2(2,1)='stringarray2'
    str2(1,2)='stringarray3'; str2(2,2)='stringarray4'
    CALL testParam%add('testPL->testSTRa2',str2)
    str2(1,1)='stringarray5'; str2(2,1)='stringarray6'
    str2(1,2)='stringarray7'; str2(2,2)='stringarray8'
    CALL testParam%add('testPL->testSTRa2_2',str2,'comment')
    !3-D
    ALLOCATE(ssk3(2,2,2))
    ssk3(1,1,1)=1.11_SSK; ssk3(2,1,1)=2.11_SSK
    ssk3(1,2,1)=1.21_SSK; ssk3(2,2,1)=2.21_SSK
    ssk3(1,1,2)=1.12_SSK; ssk3(2,1,2)=2.12_SSK
    ssk3(1,2,2)=1.22_SSK; ssk3(2,2,2)=2.22_SSK
    CALL testParam%add('testPL->testSSKa3',ssk3)
    ssk3(1,1,1)=3.11_SSK; ssk3(2,1,1)=4.11_SSK
    ssk3(1,2,1)=3.21_SSK; ssk3(2,2,1)=4.21_SSK
    ssk3(1,1,2)=3.12_SSK; ssk3(2,1,2)=4.12_SSK
    ssk3(1,2,2)=3.22_SSK; ssk3(2,2,2)=4.22_SSK
    CALL testParam%add('testPL->testSSKa3_2',ssk3,'comment')
    ALLOCATE(sdk3(2,2,2))
    sdk3(1,1,1)=11.1_SDK; sdk3(2,1,1)=21.1_SDK
    sdk3(1,2,1)=12.1_SDK; sdk3(2,2,1)=22.1_SDK
    sdk3(1,1,2)=11.2_SDK; sdk3(2,1,2)=21.2_SDK
    sdk3(1,2,2)=12.2_SDK; sdk3(2,2,2)=22.2_SDK
    CALL testParam%add('testPL->testSDKa3',sdk3)
    sdk3(1,1,1)=31.1_SDK; sdk3(2,1,1)=41.1_SDK
    sdk3(1,2,1)=32.1_SDK; sdk3(2,2,1)=42.1_SDK
    sdk3(1,1,2)=31.2_SDK; sdk3(2,1,2)=41.2_SDK
    sdk3(1,2,2)=32.2_SDK; sdk3(2,2,2)=42.2_SDK
    CALL testParam%add('testPL->testSDKa3_2',sdk3,'comment')
    ALLOCATE(snk3(2,2,2))
    snk3(1,1,1)=111; snk3(2,1,1)=211
    snk3(1,2,1)=121; snk3(2,2,1)=221
    snk3(1,1,2)=112; snk3(2,1,2)=212
    snk3(1,2,2)=122; snk3(2,2,2)=222
    CALL testParam%add('testPL->testSNKa3',snk3)
    snk3(1,1,1)=311; snk3(2,1,1)=411
    snk3(1,2,1)=321; snk3(2,2,1)=421
    snk3(1,1,2)=312; snk3(2,1,2)=412
    snk3(1,2,2)=322; snk3(2,2,2)=422
    CALL testParam%add('testPL->testSNKa3_2',snk3,'comment')
    ALLOCATE(slk3(2,2,2))
    slk3(1,1,1)=111; slk3(2,1,1)=211
    slk3(1,2,1)=121; slk3(2,2,1)=221
    slk3(1,1,2)=112; slk3(2,1,2)=212
    slk3(1,2,2)=122; slk3(2,2,2)=222
    CALL testParam%add('testPL->testSLKa3',slk3)
    slk3(1,1,1)=311; slk3(2,1,1)=411
    slk3(1,2,1)=321; slk3(2,2,1)=421
    slk3(1,1,2)=312; slk3(2,1,2)=412
    slk3(1,2,2)=322; slk3(2,2,2)=422
    CALL testParam%add('testPL->testSLKa3_2',slk3,'comment')
    ALLOCATE(str3(2,2,2))

    !Test scalars.
    CALL testParam%getString('testPL->testSBK',tmpstr)
    ASSERT(tmpstr == 'T','testSBK string')
    CALL testParam%getString('testPL->testSBK2',tmpstr)
    ASSERT(tmpstr == 'F','testSBK2 string')
    CALL testParam%getString('testPL->testSSK',tmpstr)
    ASSERT(tmpstr == '7.000000E+00','testSSK string')
    CALL testParam%getString('testPL->testSSK2',tmpstr)
    ASSERT(tmpstr == '8.000000E+00','testSSK2 string')
    CALL testParam%getString('testPL->testSDK',tmpstr)
    ASSERT(tmpstr == '7.000000000000000E+00','testSDK string')
    CALL testParam%getString('testPL->testSDK2',tmpstr)
    ASSERT(tmpstr == '8.000000000000000E+00','testSDK2 string')
    CALL testParam%getString('testPL->testSNK',tmpstr)
    ASSERT(tmpstr == '7','testSNK string')
    CALL testParam%getString('testPL->testSNK2',tmpstr)
    ASSERT(tmpstr == '8','testSNK2 string')
    CALL testParam%getString('testPL->testSLK',tmpstr)
    ASSERT(tmpstr == '7','testSLK string')
    CALL testParam%getString('testPL->testSLK2',tmpstr)
    ASSERT(tmpstr == '8','testSLK2 string')
    CALL testParam%getString('testPL->testSTR',tmpstr)
    ASSERT(tmpstr == 'string1','testSTR string')
    CALL testParam%getString('testPL->testSTR2',tmpstr)
    ASSERT(tmpstr == 'string2','testSTR2 string')
    CALL testParam%getString('testPL->testCHAR',tmpstr)
    ASSERT(tmpstr == 'char1','testCHAR string')
    CALL testParam%getString('testPL->testCHAR2',tmpstr)
    ASSERT(tmpstr == 'char2','testCHAR2 string')
    CALL testParam%getString('testPL->testSSKa1',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"1.500000E+00" "1.600000E+00"','testSSKa1 string')
    CALL testParam%getString('testPL->testSSKa1_2',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"1.700000E+00" "1.800000E+00"','testSSKa1_2 string')
    CALL testParam%getString('testPL->testSDKa1',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"2.500000000000000E+00" "2.600000000000000E+00"','testSDKa1 string')
    CALL testParam%getString('testPL->testSDKa1_2',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"2.700000000000000E+00" "2.800000000000000E+00"','testSDKa1_2 string')
    CALL testParam%getString('testPL->testSNKa1',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"-2" "-3"','testSNKa1 string')
    CALL testParam%getString('testPL->testSNKa1_2',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"2" "3"','testSNKa1_2 string')
    CALL testParam%getString('testPL->testSLKa1',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"-4" "-5"','testSLKa1 string')
    CALL testParam%getString('testPL->testSLKa1_2',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"4" "5"','testSLKa1_2 string')
    CALL testParam%getString('testPL->testSBKa1',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"T" "F"','testSBKa1 string')
    CALL testParam%getString('testPL->testSBKa1_2',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"F" "F"','testSBKa1_2 string')
    CALL testParam%getString('testPL->testSTRa1',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"stringarray1" "stringarray2"','testSTRa1 string')
    CALL testParam%getString('testPL->testSTRa1_2',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"stringarray3" "stringarray4"','testSTRa1_2 string')

    !Test 2-D arrays
    CALL testParam%getString('testPL->testSSKa2',tmpstr)
    tmpbool=(tmpstr == '"1.100000E+00" "2.100000E+00" "1.200000E+00" "2.200000E+00"')
    ASSERT(tmpbool,'testSSKa2 string')
    FINFO() CHAR(tmpstr)
    CALL testParam%getString('testPL->testSSKa2_2',tmpstr)
    tmpbool=(tmpstr == '"3.100000E+00" "4.100000E+00" "3.200000E+00" "4.200000E+00"')
    ASSERT(tmpbool,'testSSKa2_2 string')
    FINFO() CHAR(tmpstr)
    CALL testParam%getString('testPL->testSDKa2',tmpstr)
    tmpbool=(tmpstr == '"1.100000000000000E+01" "2.100000000000000E+01" '// &
        '"1.200000000000000E+01" "2.200000000000000E+01"')
    ASSERT(tmpbool,'testSDKa2 string')
    CALL testParam%getString('testPL->testSDKa2_2',tmpstr)
    tmpbool=(tmpstr == '"3.100000000000000E+01" "4.100000000000000E+01" '// &
        '"3.200000000000000E+01" "4.200000000000000E+01"')
    ASSERT(tmpbool,'testSDKa2_2 string')
    CALL testParam%getString('testPL->testSNKa2',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"11" "21" "12" "22"','testSNKa2 string')
    CALL testParam%getString('testPL->testSNKa2_2',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"31" "41" "32" "42"','testSNKa2_2 string')
    CALL testParam%getString('testPL->testSLKa2',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"110" "210" "120" "220"','testSLKa2 string')
    CALL testParam%getString('testPL->testSLKa2_2',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"310" "410" "320" "420"','testSLKa2_2 string')
    CALL testParam%getString('testPL->testSTRa2',tmpstr)
    tmpbool=(tmpstr == '"stringarray1" "stringarray2" "stringarray3" "stringarray4"')
    ASSERT(tmpbool,'testSTRa2 string')
    CALL testParam%getString('testPL->testSTRa2_2',tmpstr)
    tmpbool=(tmpstr == '"stringarray5" "stringarray6" "stringarray7" "stringarray8"')
    ASSERT(tmpbool,'testSTRa2_2 string')

    !Test 3-D arrays
    CALL testParam%getString('testPL->testSSKa3',tmpstr)
    tmpbool=(tmpstr == '"1.110000E+00" "2.110000E+00" "1.210000E+00" "2.210000E+00" '// &
        '"1.120000E+00" "2.120000E+00" "1.220000E+00" "2.220000E+00"')
    ASSERT(tmpbool,'testSSKa3 string')
    CALL testParam%getString('testPL->testSSKa3_2',tmpstr)
    tmpbool=(tmpstr == '"3.110000E+00" "4.110000E+00" "3.210000E+00" "4.210000E+00" '// &
        '"3.120000E+00" "4.120000E+00" "3.220000E+00" "4.220000E+00"')
    ASSERT(tmpbool,'testSSKa3_2 string')
    CALL testParam%getString('testPL->testSDKa3',tmpstr)
    tmpbool=(tmpstr == '"1.110000000000000E+01" "2.110000000000000E+01" '// &
        '"1.210000000000000E+01" "2.210000000000000E+01" '// &
        '"1.120000000000000E+01" "2.120000000000000E+01" '// &
        '"1.220000000000000E+01" "2.220000000000000E+01"')
    ASSERT(tmpbool,'testSDKa3 string')
    FINFO() CHAR(tmpstr)
    CALL testParam%getString('testPL->testSDKa3_2',tmpstr)
    tmpbool=(tmpstr == '"3.110000000000000E+01" "4.110000000000000E+01" '// &
        '"3.210000000000000E+01" "4.210000000000000E+01" '// &
        '"3.120000000000000E+01" "4.120000000000000E+01" '// &
        '"3.220000000000000E+01" "4.220000000000000E+01"')
    ASSERT(tmpbool,'testSDKa3_2 string')
    CALL testParam%getString('testPL->testSNKa3',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"111" "211" "121" "221" "112" "212" "122" "222"','testSNKa3 string')
    CALL testParam%getString('testPL->testSNKa3_2',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"311" "411" "321" "421" "312" "412" "322" "422"','testSNKa3_2 string')
    CALL testParam%getString('testPL->testSLKa3',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"111" "211" "121" "221" "112" "212" "122" "222"','testSLKa3 string')
    CALL testParam%getString('testPL->testSLKa3_2',tmpstr)
    ASSERT_EQ(CHAR(tmpstr),'"311" "411" "321" "421" "312" "412" "322" "422"','testSLKa3_2 string')

    !Test 1-D arrays
    CALL testParam%getString('testPL->testSSKa1',tmpstr1)
    str1(1)='1.500000E+00'; str1(2)='1.600000E+00'
    ASSERT(ALL(tmpstr1,str1),'testSSKa1 string')
    CALL testParam%getString('testPL->testSSKa1_2',tmpstr1)
    str1(1)='1.700000E+00'; str1(2)='1.800000E+00'
    ASSERT(ALL(tmpstr1,str1),'testSSKa1_2 string')
    CALL testParam%getString('testPL->testSDKa1',tmpstr1)
    str1(1)='2.500000000000000E+00'; str1(2)='2.600000000000000E+00'
    ASSERT(ALL(tmpstr1,str1),'testSDKa1 string')
    CALL testParam%getString('testPL->testSDKa1_2',tmpstr1)
    str1(1)='2.700000000000000E+00'; str1(2)='2.800000000000000E+00'
    ASSERT(ALL(tmpstr1,str1),'testSDKa1_2 string')
    CALL testParam%getString('testPL->testSNKa1',tmpstr1)
    str1(1)='-2'; str1(2)='-3'
    ASSERT(ALL(tmpstr1,str1),'testSNKa1 string')
    CALL testParam%getString('testPL->testSNKa1_2',tmpstr1)
    str1(1)='2'; str1(2)='3'
    ASSERT(ALL(tmpstr1,str1),'testSNKa1_2 string')
    CALL testParam%getString('testPL->testSLKa1',tmpstr1)
    str1(1)='-4'; str1(2)='-5'
    ASSERT(ALL(tmpstr1,str1),'testSLKa1 string')
    CALL testParam%getString('testPL->testSLKa1_2',tmpstr1)
    str1(1)='4'; str1(2)='5'
    ASSERT(ALL(tmpstr1,str1),'testSLKa1_2 string')
    CALL testParam%getString('testPL->testSBKa1',tmpstr1)
    str1(1)='T'; str1(2)='F'
    ASSERT(ALL(tmpstr1,str1),'testSBKa1 string')
    CALL testParam%getString('testPL->testSBKa1_2',tmpstr1)
    str1(1)='F'; str1(2)='F'
    ASSERT(ALL(tmpstr1,str1),'testSBKa1_2 string')
    CALL testParam%getString('testPL->testSTRa1',tmpstr1)
    str1(1)='stringarray1'; str1(2)='stringarray2'
    ASSERT(ALL(tmpstr1,str1),'testSTRa1 string')
    CALL testParam%getString('testPL->testSTRa1_2',tmpstr1)
    str1(1)='stringarray3'; str1(2)='stringarray4'
    ASSERT(ALL(tmpstr1,str1),'testSTRa1_2 string')

    !Test 2-D arrays
    CALL testParam%getString('testPL->testSSKa2',tmpstr2)
    str2(1,1)='1.100000E+00'; str2(2,1)='2.100000E+00'
    str2(1,2)='1.200000E+00'; str2(2,2)='2.200000E+00'
    ASSERT(ALL(tmpstr2,str2),'testSSKa2 string')
    CALL testParam%getString('testPL->testSSKa2_2',tmpstr2)
    str2(1,1)='3.100000E+00'; str2(2,1)='4.100000E+00'
    str2(1,2)='3.200000E+00'; str2(2,2)='4.200000E+00'
    ASSERT(ALL(tmpstr2,str2),'testSSKa2_2 string')
    CALL testParam%getString('testPL->testSDKa2',tmpstr2)
    str2(1,1)='1.100000000000000E+01'; str2(2,1)='2.100000000000000E+01'
    str2(1,2)='1.200000000000000E+01'; str2(2,2)='2.200000000000000E+01'
    ASSERT(ALL(tmpstr2,str2),'testSDKa2 string')
    CALL testParam%getString('testPL->testSDKa2_2',tmpstr2)
    str2(1,1)='3.100000000000000E+01'; str2(2,1)='4.100000000000000E+01'
    str2(1,2)='3.200000000000000E+01'; str2(2,2)='4.200000000000000E+01'
    ASSERT(ALL(tmpstr2,str2),'testSDKa2_2 string')
    CALL testParam%getString('testPL->testSNKa2',tmpstr2)
    str2(1,1)='11'; str2(2,1)='21'
    str2(1,2)='12'; str2(2,2)='22'
    ASSERT(ALL(tmpstr2,str2),'testSNKa2 string')
    CALL testParam%getString('testPL->testSNKa2_2',tmpstr2)
    str2(1,1)='31'; str2(2,1)='41'
    str2(1,2)='32'; str2(2,2)='42'
    ASSERT(ALL(tmpstr2,str2),'testSNKa2_2 string')
    CALL testParam%getString('testPL->testSLKa2',tmpstr2)
    str2(1,1)='110'; str2(2,1)='210'
    str2(1,2)='120'; str2(2,2)='220'
    ASSERT(ALL(tmpstr2,str2),'testSLKa2 string')
    CALL testParam%getString('testPL->testSLKa2_2',tmpstr2)
    str2(1,1)='310'; str2(2,1)='410'
    str2(1,2)='320'; str2(2,2)='420'
    ASSERT(ALL(tmpstr2,str2),'testSLKa2_2 string')
    CALL testParam%getString('testPL->testSTRa2',tmpstr2)
    str2(1,1)='stringarray1'; str2(2,1)='stringarray2'
    str2(1,2)='stringarray3'; str2(2,2)='stringarray4'
    ASSERT(ALL(tmpstr2,str2),'testSTRa2 string')
    CALL testParam%getString('testPL->testSTRa2_2',tmpstr2)
    str2(1,1)='stringarray5'; str2(2,1)='stringarray6'
    str2(1,2)='stringarray7'; str2(2,2)='stringarray8'
    ASSERT(ALL(tmpstr2,str2),'testSTRa2_2 string')

    !Test 3-D arrays
    CALL testParam%getString('testPL->testSSKa3',tmpstr3)
    str3(1,1,1)='1.110000E+00'; str3(2,1,1)='2.110000E+00'
    str3(1,2,1)='1.210000E+00'; str3(2,2,1)='2.210000E+00'
    str3(1,1,2)='1.120000E+00'; str3(2,1,2)='2.120000E+00'
    str3(1,2,2)='1.220000E+00'; str3(2,2,2)='2.220000E+00'
    ASSERT(ALL(tmpstr3,str3),'testSSKa3 string')
    CALL testParam%getString('testPL->testSSKa3_2',tmpstr3)
    str3(1,1,1)='3.110000E+00'; str3(2,1,1)='4.110000E+00'
    str3(1,2,1)='3.210000E+00'; str3(2,2,1)='4.210000E+00'
    str3(1,1,2)='3.120000E+00'; str3(2,1,2)='4.120000E+00'
    str3(1,2,2)='3.220000E+00'; str3(2,2,2)='4.220000E+00'
    ASSERT(ALL(tmpstr3,str3),'testSSKa3_2 string')
    CALL testParam%getString('testPL->testSDKa3',tmpstr3)
    str3(1,1,1)='1.110000000000000E+01'; str3(2,1,1)='2.110000000000000E+01'
    str3(1,2,1)='1.210000000000000E+01'; str3(2,2,1)='2.210000000000000E+01'
    str3(1,1,2)='1.120000000000000E+01'; str3(2,1,2)='2.120000000000000E+01'
    str3(1,2,2)='1.220000000000000E+01'; str3(2,2,2)='2.220000000000000E+01'
    ASSERT(ALL(tmpstr3,str3),'testSDKa3 string')
    FINFO() CHAR(tmpstr3(1,1,1))
    CALL testParam%getString('testPL->testSDKa3_2',tmpstr3)
    str3(1,1,1)='3.110000000000000E+01'; str3(2,1,1)='4.110000000000000E+01'
    str3(1,2,1)='3.210000000000000E+01'; str3(2,2,1)='4.210000000000000E+01'
    str3(1,1,2)='3.120000000000000E+01'; str3(2,1,2)='4.120000000000000E+01'
    str3(1,2,2)='3.220000000000000E+01'; str3(2,2,2)='4.220000000000000E+01'
    ASSERT(ALL(tmpstr3,str3),'testSDKa3_2 string')
    CALL testParam%getString('testPL->testSNKa3',tmpstr3)
    str3(1,1,1)='111'; str3(2,1,1)='211'
    str3(1,2,1)='121'; str3(2,2,1)='221'
    str3(1,1,2)='112'; str3(2,1,2)='212'
    str3(1,2,2)='122'; str3(2,2,2)='222'
    ASSERT(ALL(tmpstr3,str3),'testSNKa3 string')
    CALL testParam%getString('testPL->testSNKa3_2',tmpstr3)
    str3(1,1,1)='311'; str3(2,1,1)='411'
    str3(1,2,1)='321'; str3(2,2,1)='421'
    str3(1,1,2)='312'; str3(2,1,2)='412'
    str3(1,2,2)='322'; str3(2,2,2)='422'
    ASSERT(ALL(tmpstr3,str3),'testSNKa3_2 string')
    CALL testParam%getString('testPL->testSLKa3',tmpstr3)
    str3(1,1,1)='111'; str3(2,1,1)='211'
    str3(1,2,1)='121'; str3(2,2,1)='221'
    str3(1,1,2)='112'; str3(2,1,2)='212'
    str3(1,2,2)='122'; str3(2,2,2)='222'
    ASSERT(ALL(tmpstr3,str3),'testSLKa3 string')
    CALL testParam%getString('testPL->testSLKa3_2',tmpstr3)
    str3(1,1,1)='311'; str3(2,1,1)='411'
    str3(1,2,1)='321'; str3(2,2,1)='421'
    str3(1,1,2)='312'; str3(2,1,2)='412'
    str3(1,2,2)='322'; str3(2,2,2)='422'
    ASSERT(ALL(tmpstr3,str3),'testSLKa3_2 string')

    CALL clear_test_vars()
    !Deallocate locals
    DEALLOCATE(snk2)
    DEALLOCATE(snk3)
    DEALLOCATE(slk2)
    DEALLOCATE(slk3)
    DEALLOCATE(ssk2)
    DEALLOCATE(ssk3)
    DEALLOCATE(sdk2)
    DEALLOCATE(sdk3)
    DEALLOCATE(str1)
    DEALLOCATE(str2)

    CALL testParam%clear()
    CALL testParam2%clear()
  ENDSUBROUTINE testGetString
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
    ASSERT(.NOT.testParam%has('testPL->testSDK->bad'),'param of not param')

    !Confirm bug fix for incorrect matching from string truncation
    CALL testParam%clear()
    CALL testParam%add('testPL->testPL_10->num_10',10_SNK)
    ASSERT(testParam%has('testPL->testPL_10'),'%has(testPL->testPL_10)')
    ASSERT(.NOT.testParam%has('testPL->testPL_1'),'%has(testPL->testPL_1)')

    CALL clear_test_vars()
  ENDSUBROUTINE testHas
!
!-------------------------------------------------------------------------------
  SUBROUTINE testConvertTo2DStringArray()
    INTEGER(SIK) :: i,j
    TYPE(StringType) :: addr
    TYPE(StringType),ALLOCATABLE :: table(:,:),reftable(:,:)

    !Test null call
    CALL testParam%clear()
    addr=''
    CALL testParam%convertTo2DStringArray(addr,table)
    ASSERT(.NOT. ALLOCATED(table),'empty call')

    !Setup the PL
    CALL testParam%add('TestPL->List1->1->Row1','Row1')
    CALL testParam%add('TestPL->List1->1->testRow2','testRow2')
    CALL testParam%add('TestPL->List1->1->testRow3; Extra','testRow3; Extra')
    CALL testParam%add('TestPL->List1->2->Row1',10)
    CALL testParam%add('TestPL->List1->2->testRow2',1000000)
    CALL testParam%add('TestPL->List1->2->testRow3; Extra',23.0E+10_SRK)
    CALL testParam%add('TestPL->List1->3->Row1',.TRUE.)
    CALL testParam%add('TestPL->List1->3->testRow2',.FALSE.)
    CALL testParam%add('TestPL->List1->3->testRow3; Extra','23.0E+10')
    CALL testParam%add('TestPL->List1->4->Row1','TRUE')
    CALL testParam%add('TestPL->List1->4->testRow2','FALSE')
    CALL testParam%add('TestPL->List1->4->testRow3; Extra','-')

    !Test bad addr call
    addr='wrong'
    CALL testParam%convertTo2DStringArray(addr,table)
    ASSERT(.NOT. ALLOCATED(table),'bad addr call')

    ALLOCATE(reftable(4,3))
    reftable='-'
    reftable(1,1)='Row1'; reftable(2,1)='10'; reftable(3,1)='T'; reftable(4,1)='TRUE' 
    reftable(1,2)='testRow2'; reftable(2,2)='1000000'; reftable(3,2)='F'; reftable(4,2)='FALSE' 
    reftable(1,3)='testRow3; Extra'; reftable(2,3)='2.300000000000000E+11'; reftable(3,3)='23.0E+10'; reftable(4,3)='-' 

    addr='TestPL->List1'
    CALL testParam%convertTo2DStringArray(addr,table)
    ASSERTFAIL(ALLOCATED(table),'valid addr call')
    ASSERTFAIL(SIZE(table,DIM=1) == 4,'table dim=1')
    ASSERTFAIL(SIZE(table,DIM=2) == 3,'table dim=2')

    DO j=1,SIZE(table,DIM=2)
      DO i=1,SIZE(table,DIM=1)
        ASSERT(reftable(i,j) == table(i,j),'reftable')
        FINFO() i,j,CHAR(table(i,j))
      ENDDO
    ENDDO

  ENDSUBROUTINE testConvertTo2DStringArray
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
    addr=''
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

    !Root search with a paramtype pointer.
    n=0
    CALL testParam%get('level 1',someParam)
    addr=''
    CALL someParam%getNextParam(addr,iParam)
    DO WHILE(ASSOCIATED(iParam))
      n=n+1
      ASSERT(refaddr(n) == addr,'addr')
      FINFO() n,'"'//TRIM(refaddr(n))//'" "'//TRIM(addr)//'"'
      ASSERT(refname(n) == iParam%name,'addr')
      FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%name)//'"'
      CALL someParam%getNextParam(addr,iParam)
    ENDDO
    ASSERT(n == 6,'number of iters')

    !search with a non root starting point
    n=2
    addr='level 1->level 2'
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

    !search for the last value in the list (Can't find out how to get to the uncovered code)
    n=6
    addr='level 1->val 3'
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

    CALL clear_test_vars()
  ENDSUBROUTINE testGetNextParam
!
!-------------------------------------------------------------------------------
  SUBROUTINE testGetSubParams()
    INTEGER(SIK) :: n
    TYPE(StringType) :: addr,refname(3)
    CLASS(ParamType),POINTER :: iParam => NULL()

    CALL testParam%add('level 1->level 2->val 1',1.0)
    CALL testParam%add('level 1->val 3',3.0)
    CALL testParam%add('level 1->level 2->level 3->dummy',0.0)
    CALL testParam%add('level 1->level 2->val 2',2.0)

    refname(1)='level 2'
    refname(2)='val 3'

    n=0
    CALL testParam%get('level 1',someParam)
    addr=''
    CALL testParam%getSubParams(addr,iParam)
    ASSERT(ASSOCIATED(iParam),'First Sub Param')
    DO WHILE(ASSOCIATED(iParam))
      n=n+1
      ASSERT(refname(n) == TRIM(iParam%name),'name')
      FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%name)//'"'
      CALL testParam%getSubParams(addr,iParam)
    ENDDO
    ASSERT(n == 2,'number of subParams')

    refname(1)='level 2'
    refname(2)='val 3'
    n=0
    CALL testParam%get('level 1',someParam)
    addr='level 1'
    CALL testParam%getSubParams(addr,iParam)
    ASSERT(ASSOCIATED(iParam),'First Sub Param')
    DO WHILE(ASSOCIATED(iParam))
      n=n+1
      ASSERT(refname(n) == TRIM(iParam%name),'name')
      FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%name)//'"'
      CALL testParam%getSubParams(addr,iParam)
    ENDDO
    ASSERT(n == 2,'number of subParams')


    n=0
    refname(1)='val 1'
    refname(2)='level 3'
    refname(3)='val 2'
    addr='level 1->level 2'
    CALL testParam%getSubParams(addr,iParam)
    ASSERT(ASSOCIATED(iParam),'First Sub Param')
    DO WHILE(ASSOCIATED(iParam))
      n=n+1
      ASSERT(refname(n) == TRIM(iParam%name),'name')
      FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%name)//'"'
      CALL testParam%getSubParams(addr,iParam)
    ENDDO
    ASSERT(n == 3,'number of subParams')

    n=0
    refname(1)='dummy'
    addr='level 1->level 2->level 3'
    CALL testParam%getSubParams(addr,iParam)
    ASSERT(ASSOCIATED(iParam),'First Sub Param')
    DO WHILE(ASSOCIATED(iParam))
      n=n+1
      ASSERT(refname(n) == TRIM(iParam%name),'name')
      FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%name)//'"'
      CALL testParam%getSubParams(addr,iParam)
    ENDDO
    ASSERT(n == 1,'number of subParams')

    CALL clear_test_vars()
  ENDSUBROUTINE testGetSubParams
!
!-------------------------------------------------------------------------------
  SUBROUTINE testGetSubPL()
    CLASS(ParamType),POINTER :: refPtr,testClass
    refPtr => NULL()

    valstr=''
    CALL testParam%clear()

    COMPONENT_TEST('empty')
    CALL testParam%getSubPL(valstr,someParam)
    ASSERT(.NOT.ASSOCIATED(someParam),'someParam')

    COMPONENT_TEST('1st Level')
    CALL testParam%add('First param->Second level->p','p')
    CALL testParam%add('First param->a','a')
    CALL testParam%add('First param->Second list->b','b')

    CALL testParam%getSubPL(valstr,someParam)
    ASSERTFAIL(ASSOCIATED(someParam),'ASSOCIATED')
    ASSERT(ASSOCIATED(someParam,testParam%pdat),'ASSOCIATED(pdat)')
    ASSERT(someParam%name == 'First param','name')
    CALL testParam%getSubPL(valstr,someParam)
    ASSERT(.NOT.ASSOCIATED(someParam),'iter 2')

    COMPONENT_TEST('2nd Level')
    valstr='First param'
    CALL testParam%get('First param->Second level',refPtr)
    CALL testParam%getSubPL(valStr,someParam)
    ASSERTFAIL(ASSOCIATED(someParam),'ASSOCIATEDi 1st iter')
    ASSERT(ASSOCIATED(someParam,refPtr),'ASSOCIATED(refPtr) 1st iter')
    ASSERT(someParam%name == 'Second level','name 1st iter')

    CALL testParam%get('First param->Second list',refPtr)
    CALL testParam%getSubPL(valStr,someParam)
    ASSERTFAIL(ASSOCIATED(someParam),'ASSOCIATED 2nd iter')
    ASSERT(ASSOCIATED(someParam,refPtr),'ASSOCIATED(refPtr) 2nd iter')
    ASSERT(someParam%name == 'Second list','name 2nd iter')

    CALL testParam%getSubPL(valStr,someParam)
    ASSERT(.NOT.ASSOCIATED(someParam),'ASSOCIATED 3rd iter')

    COMPONENT_TEST('bad addresses')
    valStr='b'
    CALL testParam%getSubPL(valStr,someParam)
    ASSERT(.NOT.ASSOCIATED(someParam),'b')
    valStr='Second level'
    CALL testParam%getSubPL(valStr,someParam)
    ASSERT(.NOT.ASSOCIATED(someParam),'Second level')
    valStr='bad name'
    CALL testParam%getSubPL(valStr,someParam)
    ASSERT(.NOT.ASSOCIATED(someParam),'bad name')

    COMPONENT_TEST('CLASS(ParamType)')
    testClass => testParam%pdat
    valStr=''
    CALL testClass%getSubPL(valStr,someParam)
    ASSERT(ASSOCIATED(testClass,someParam),'ASSOCIATED(root)')
    CALL testClass%getSubPL(valStr,someParam)
    ASSERT(.NOT.ASSOCIATED(someParam),'2nd iter')

    CALL testClass%getSubPL(testClass%name,someParam)
    CALL testClass%get('First param->Second level',refPtr)
    ASSERTFAIL(ASSOCIATED(someParam),'ASSOCIATED 1st iter self')
    ASSERT(ASSOCIATED(someParam,refPtr),'ASSOCIATED(refPtr) 1st iter self')
    ASSERT(someParam%name == 'Second level','name 1st iter self')

    CALL clear_test_vars()
  ENDSUBROUTINE testGetSubPL
!
!-------------------------------------------------------------------------------
  SUBROUTINE testRemove()
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg

    someParam => NULL()

    !Testing removal of parameter that doesn't exist
    CALL testParam%remove('testSSK3')
    !
    !Testing removal of parameter that is erroneous, need to have a value before '->'
    CALL testParam%remove('->error')

    !Testing removal of a blank
    CALL testParam%remove('')
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::remove_ParamType -'// &
      ' cannot search for a blank name!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'blank remove')

    !Set up actual values for removal
    CALL testParam%add('TestList->testSSK',1.0_SSK)
    CALL testParam%add('TestList->FirstList->testSSK',1.0_SSK)
    CALL testParam%add('TestList->NextList->testSDK',1.0_SDK)
    CALL testParam%add('TestList->OtherList->testSNK',1_SNK)
    CALL testParam%add('TestList->OtherList->testSLK',1_SLK)

    !Remove parameter at end of list
    CALL testParam%remove('TestList->NextList->testSDK')
    ASSERT(.NOT.testParam%has('TestList->NextList->testSDK'),'remove parameter')

    !Remove empty PL
    CALL testParam%remove('TestList->NextList')
    ASSERT(.NOT.testParam%has('TestList->NextList'),'remove empty pl')

    !Remove a sublist
    CALL testParam%remove('TestList->OtherList')
    ASSERT(.NOT.testParam%has('TestList->OtherList'),'remove parameter list')

    !Remove everything
    CALL testParam%remove('TestList')
    ASSERT(.NOT.testParam%has('TestList'),'remove everything')

    CALL clear_test_vars()
  ENDSUBROUTINE testRemove
!
!-------------------------------------------------------------------------------
  SUBROUTINE testValidate()
    CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: msg,refmsg

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
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::validateReq_ParamType -'// &
      ' Failed to locate required parameter "TestReq->p2"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'testReq->p2 validate')

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
    CALL testParam%validate(testParam2,testParam3,.TRUE.)

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
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_WARNING #### - PARAMETERLISTS::validateOpt_ParamType -'// &
      ' Optional parameter "TestReq->sublist1->sublist2->sublist3->opt3" has type'// &
      ' "REAL(SDK)" and should be type "REAL(SSK)"!  It is being overriden with default value.'
    ASSERT(TRIM(msg) == TRIM(refmsg),'TestReq->sublist1->sublist2->sublist3->opt3 SSK validate optional input')

    !Finds a meaningful REQ parameter, returns an associated parameter of a different type (lines 1337-1340)
    CALL testParam%add('TestReq->p6',6.0_SSK)
    CALL testParam2%add('TestReq->p6',6_SNK)
    CALL testParam%validate(testParam2,testParam3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::validateReq_ParamType -'// &
      ' Required parameter "TestReq->p6" has type "REAL(SSK)" and must be type "INTEGER(SNK)"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'TestReq->p6 SNK validate required input')
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
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::validateReq_ParamType -'// &
      ' Failed to locate required parameter "TestReq->p2->2far"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'TestReq->p2->2far validate failed to locate')

    !Since an empty req param list exists, add a SLK parameter to test param
    !so they aren't the same type  (lines 1319-1323)
    CALL testParam%add('TestReq->p2->2far',6_SLK)
    CALL testParam%validate(testParam2,testParam3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::validateReq_ParamType -'// &
      ' Required parameter "TestReq->p2->2far" has type "INTEGER(SLK)" and must be type "TYPE(ParamType_List)"!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'TestReq->p2->2far validate different type, PList')

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
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_WARNING #### - PARAMETERLISTS::validateOpt_ParamType -'// &
      ' Optional parameter "TestOpt->p2->2far->veryfar" has type "REAL(SSK)" and'// &
      ' should be type "TYPE(ParamType_List)"!  Since has no default value, it will remain unset!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'TestReq->p2->2far validate different type, PList')

    CALL testParam%remove('TestOpt->p2->2far->veryfar')
    !Testing an unassociated test param from the remove statement above (lines 1388-1396)
    CALL testParam%validate(testParam2,testParam3)
    msg=eParams%getLastMessage()
    refmsg='#### EXCEPTION_DEBUG_MESG #### - PARAMETERLISTS::validateOpt_ParamType -'// &
      ' Failed to locate optional parameter "TestOpt->p2->2far->veryfar"! It is being'// &
      ' added with no default value!'
    ASSERT(TRIM(msg) == TRIM(refmsg),'TestOpt->p2->2far->veryfar validate different type, PList')

    CALL clear_test_vars()
  ENDSUBROUTINE testValidate
!
!-------------------------------------------------------------------------------
  SUBROUTINE testVerify()
    LOGICAL(SBK) :: bool
    INTEGER(SNK),ALLOCATABLE :: snk2(:,:),snk3(:,:,:)
    INTEGER(SLK),ALLOCATABLE :: slk2(:,:),slk3(:,:,:)
    REAL(SSK),ALLOCATABLE :: ssk2(:,:),ssk3(:,:,:)
    REAL(SDK),ALLOCATABLE :: sdk2(:,:),sdk3(:,:,:)
    TYPE(StringType) :: str0
    TYPE(StringType),ALLOCATABLE :: str1(:),str2(:,:)

    CALL testParam%verify(testParam2,bool)
    ASSERT(bool,'empty lists')

    !Testing addition of SBK routine to parameter list
    CALL testParam%add('testPL->testSBK',.TRUE.)
    CALL testParam%add('testPL->testSBK2',.FALSE.,'comment')

    !Testing addition of SSK routine to parameter list
    CALL testParam%add('testPL->testSSK',7.0_SSK)
    CALL testParam%add('testPL->testSSK2',8.0_SSK,'comment')

    !Testing addition of SDK routine to parameter list
    CALL testParam%add('testPL->testSDK',7.0_SDK)
    CALL testParam%add('testPL->testSDK2',8.0_SDK,'comment')

    !Testing addition of SNK routine to parameter list
    CALL testParam%add('testPL->testSNK',7_SNK)
    CALL testParam%add('testPL->testSNK2',8_SNK,'comment')

    !Testing addition of SLK routine to parameter list
    CALL testParam%add('testPL->testSLK',7_SLK)
    CALL testParam%add('testPL->testSLK2',8_SLK,'comment')

    !Testing addition of STR routine to parameter list
    str0='string1'
    CALL testParam%add('testPL->testSTR',str0)
    str0='string2'
    CALL testParam%add('testPL->testSTR2',str0,'comment')

    !Testing addition of CHAR routine to parameter list
    CALL testParam%add('testPL->testCHAR','char1')
    CALL testParam%add('testPL->testCHAR2','char2','comment')

    !Testing addition of 1-D array SSK routine to parameter list
    CALL testParam%add('testPL->testSSKa1',(/1.5_SSK,1.6_SSK/))
    CALL testParam%add('testPL->testSSKa1_2',(/1.7_SSK,1.8_SSK/),'comment')

    !Testing addition of 1-D array SDK routine to parameter list
    CALL testParam%add('testPL->testSDKa1',(/2.5_SDK,2.6_SDK/))
    CALL testParam%add('testPL->testSDKa1_2',(/2.7_SDK,2.8_SDK/),'comment')

    !Testing addition of 1-D array SNK routine to parameter list
    CALL testParam%add('testPL->testSNKa1',(/-2_SNK,-3_SNK/))
    CALL testParam%add('testPL->testSNKa1_2',(/2_SNK,3_SNK/),'comment')

    !Testing addition of 1-D array SLK routine to parameter list
    CALL testParam%add('testPL->testSLKa1',(/-4_SLK,-5_SLK/))
    CALL testParam%add('testPL->testSLKa1_2',(/4_SLK,5_SLK/),'comment')

    !Testing addition of 1-D array SBK routine to parameter list
    CALL testParam%add('testPL->testSBKa1',(/.TRUE.,.FALSE./))
    CALL testParam%add('testPL->testSBKa1_2',(/.FALSE.,.FALSE./),'comment')

    !Testing addition of 1-D array STR routine to parameter list
    ALLOCATE(str1(2))
    str1(1)='stringarray1'
    str1(2)='stringarray2'
    CALL testParam%add('testPL->testSTRa1',str1)
    str1(1)='stringarray3'
    str1(2)='stringarray4'
    CALL testParam%add('testPL->testSTRa1_2',str1,'comment')

    !Testing addition of 2-D array SSK routine to parameter list
    ALLOCATE(ssk2(2,2))
    ssk2(1,1)=1.1_SSK
    ssk2(2,1)=2.1_SSK
    ssk2(1,2)=1.2_SSK
    ssk2(2,2)=2.2_SSK
    CALL testParam%add('testPL->testSSKa2',ssk2)
    ssk2(1,1)=3.1_SSK
    ssk2(2,1)=4.1_SSK
    ssk2(1,2)=3.2_SSK
    ssk2(2,2)=4.2_SSK
    CALL testParam%add('testPL->testSSKa2_2',ssk2,'comment')

    !Testing addition of 2-D array SDK routine to parameter list
    ALLOCATE(sdk2(2,2))
    sdk2(1,1)=11.0_SDK
    sdk2(2,1)=21.0_SDK
    sdk2(1,2)=12.0_SDK
    sdk2(2,2)=22.0_SDK
    CALL testParam%add('testPL->testSDKa2',sdk2)
    sdk2(1,1)=31.0_SDK
    sdk2(2,1)=41.0_SDK
    sdk2(1,2)=32.0_SDK
    sdk2(2,2)=42.0_SDK
    CALL testParam%add('testPL->testSDKa2_2',sdk2,'comment')

    !Testing addition of 2-D array SNK routine to parameter list
    ALLOCATE(snk2(2,2))
    snk2(1,1)=11
    snk2(2,1)=21
    snk2(1,2)=12
    snk2(2,2)=22
    CALL testParam%add('testPL->testSNKa2',snk2)
    snk2(1,1)=31
    snk2(2,1)=41
    snk2(1,2)=32
    snk2(2,2)=42
    CALL testParam%add('testPL->testSNKa2_2',snk2,'comment')

    !Testing addition of 2-D array SLK routine to parameter list
    ALLOCATE(slk2(2,2))
    slk2(1,1)=110
    slk2(2,1)=210
    slk2(1,2)=120
    slk2(2,2)=220
    CALL testParam%add('testPL->testSLKa2',slk2)
    slk2(1,1)=310
    slk2(2,1)=410
    slk2(1,2)=320
    slk2(2,2)=420
    CALL testParam%add('testPL->testSLKa2_2',slk2,'comment')

    !Testing addition of 2-D array STR routine to parameter list
    ALLOCATE(str2(2,2))
    str2(1,1)='stringarray1'
    str2(2,1)='stringarray2'
    str2(1,2)='stringarray3'
    str2(2,2)='stringarray4'
    CALL testParam%add('testPL->testSTRa2',str2)
    str2(1,1)='stringarray5'
    str2(2,1)='stringarray6'
    str2(1,2)='stringarray7'
    str2(2,2)='stringarray8'
    CALL testParam%add('testPL->testSTRa2_2',str2,'comment')

    !Testing addition of 3-D array SSK routine to parameter list
    ALLOCATE(ssk3(2,2,2))
    ssk3(1,1,1)=1.11_SSK
    ssk3(2,1,1)=2.11_SSK
    ssk3(1,2,1)=1.21_SSK
    ssk3(2,2,1)=2.21_SSK
    ssk3(1,1,2)=1.12_SSK
    ssk3(2,1,2)=2.12_SSK
    ssk3(1,2,2)=1.22_SSK
    ssk3(2,2,2)=2.22_SSK
    CALL testParam%add('testPL->testSSKa3',ssk3)
    ssk3(1,1,1)=3.11_SSK
    ssk3(2,1,1)=4.11_SSK
    ssk3(1,2,1)=3.21_SSK
    ssk3(2,2,1)=4.21_SSK
    ssk3(1,1,2)=3.12_SSK
    ssk3(2,1,2)=4.12_SSK
    ssk3(1,2,2)=3.22_SSK
    ssk3(2,2,2)=4.22_SSK
    CALL testParam%add('testPL->testSSKa3_2',ssk3,'comment')

    !Testing addition of 3-D array SDK routine to parameter list
    ALLOCATE(sdk3(2,2,2))
    sdk3(1,1,1)=11.1_SDK
    sdk3(2,1,1)=21.1_SDK
    sdk3(1,2,1)=12.1_SDK
    sdk3(2,2,1)=22.1_SDK
    sdk3(1,1,2)=11.2_SDK
    sdk3(2,1,2)=21.2_SDK
    sdk3(1,2,2)=12.2_SDK
    sdk3(2,2,2)=22.2_SDK
    CALL testParam%add('testPL->testSDKa3',sdk3)
    sdk3(1,1,1)=31.1_SDK
    sdk3(2,1,1)=41.1_SDK
    sdk3(1,2,1)=32.1_SDK
    sdk3(2,2,1)=42.1_SDK
    sdk3(1,1,2)=31.2_SDK
    sdk3(2,1,2)=41.2_SDK
    sdk3(1,2,2)=32.2_SDK
    sdk3(2,2,2)=42.2_SDK
    CALL testParam%add('testPL->testSDKa3_2',sdk3,'comment')

    !Testing addition of 3-D array SNK routine to parameter list
    ALLOCATE(snk3(2,2,2))
    snk3(1,1,1)=111
    snk3(2,1,1)=211
    snk3(1,2,1)=121
    snk3(2,2,1)=221
    snk3(1,1,2)=112
    snk3(2,1,2)=212
    snk3(1,2,2)=122
    snk3(2,2,2)=222
    CALL testParam%add('testPL->testSNKa3',snk3)
    snk3(1,1,1)=311
    snk3(2,1,1)=411
    snk3(1,2,1)=321
    snk3(2,2,1)=421
    snk3(1,1,2)=312
    snk3(2,1,2)=412
    snk3(1,2,2)=322
    snk3(2,2,2)=422
    CALL testParam%add('testPL->testSNKa3_2',snk3,'comment')

    !Testing addition of 3-D array SLK routine to parameter list
    ALLOCATE(slk3(2,2,2))
    slk3(1,1,1)=111
    slk3(2,1,1)=211
    slk3(1,2,1)=121
    slk3(2,2,1)=221
    slk3(1,1,2)=112
    slk3(2,1,2)=212
    slk3(1,2,2)=122
    slk3(2,2,2)=222
    CALL testParam%add('testPL->testSLKa3',slk3)
    slk3(1,1,1)=311
    slk3(2,1,1)=411
    slk3(1,2,1)=321
    slk3(2,2,1)=421
    slk3(1,1,2)=312
    slk3(2,1,2)=412
    slk3(1,2,2)=322
    slk3(2,2,2)=422
    CALL testParam%add('testPL->testSLKa3_2',slk3,'comment')

    !assign the same PL
    testParam2=testParam
    CALL testParam%verify(testParam2,bool)
    ASSERT(bool,'verify the copy')

    CALL testParam2%clear()
    CALL testParam%verify(testParam2,bool)
    ASSERT(.NOT.bool,'arg uninit')
    CALL testParam2%verify(testParam,bool)
    ASSERT(.NOT.bool,'this uninit')
    FINFO() bool

    !Deallocate locals
    DEALLOCATE(snk2)
    DEALLOCATE(snk3)
    DEALLOCATE(slk2)
    DEALLOCATE(slk3)
    DEALLOCATE(ssk2)
    DEALLOCATE(ssk3)
    DEALLOCATE(sdk2)
    DEALLOCATE(sdk3)
    DEALLOCATE(str1)
    DEALLOCATE(str2)

    CALL clear_test_vars()
  ENDSUBROUTINE testVerify
!
!-------------------------------------------------------------------------------
  SUBROUTINE testVerifyList()
    LOGICAL(SBK) :: bool
    INTEGER(SIK) :: nerror
    INTEGER(SNK),ALLOCATABLE :: snk2(:,:),snk3(:,:,:)
    INTEGER(SLK),ALLOCATABLE :: slk2(:,:),slk3(:,:,:)
    REAL(SSK),ALLOCATABLE :: ssk2(:,:),ssk3(:,:,:)
    REAL(SDK),ALLOCATABLE :: sdk2(:,:),sdk3(:,:,:)
    TYPE(StringType) :: str0
    TYPE(StringType),ALLOCATABLE :: str1(:),str2(:,:)
    TYPE(ExceptionHandlerType) :: tmpe

    CALL testParam%verifyList(testParam2,bool,e)
    ASSERT(bool,'empty lists')

    !Testing addition of SBK routine to parameter list
    CALL testParam%add('testPL->testSBK',.TRUE.)
    CALL testParam%add('testPL->testSBK2',.FALSE.,'comment')

    !Testing addition of SSK routine to parameter list
    CALL testParam%add('testPL->testSSK',7.0_SSK)
    CALL testParam%add('testPL->testSSK2',8.0_SSK,'comment')

    !Testing addition of SDK routine to parameter list
    CALL testParam%add('testPL->testSDK',7.0_SDK)
    CALL testParam%add('testPL->testSDK2',8.0_SDK,'comment')

    !Testing addition of SNK routine to parameter list
    CALL testParam%add('testPL->testSNK',7_SNK)
    CALL testParam%add('testPL->testSNK2',8_SNK,'comment')

    !Testing addition of SLK routine to parameter list
    CALL testParam%add('testPL->testSLK',7_SLK)
    CALL testParam%add('testPL->testSLK2',8_SLK,'comment')

    !Testing addition of STR routine to parameter list
    str0='string1'
    CALL testParam%add('testPL->testSTR',str0)
    str0='string2'
    CALL testParam%add('testPL->testSTR2',str0,'comment')

    !Testing addition of CHAR routine to parameter list
    CALL testParam%add('testPL->testCHAR','char1')
    CALL testParam%add('testPL->testCHAR2','char2','comment')

    !Testing addition of 1-D array SSK routine to parameter list
    CALL testParam%add('testPL->testSSKa1',(/1.5_SSK,1.6_SSK/))
    CALL testParam%add('testPL->testSSKa1_2',(/1.7_SSK,1.8_SSK/),'comment')

    !Testing addition of 1-D array SDK routine to parameter list
    CALL testParam%add('testPL->testSDKa1',(/2.5_SDK,2.6_SDK/))
    CALL testParam%add('testPL->testSDKa1_2',(/2.7_SDK,2.8_SDK/),'comment')

    !Testing addition of 1-D array SNK routine to parameter list
    CALL testParam%add('testPL->testSNKa1',(/-2_SNK,-3_SNK/))
    CALL testParam%add('testPL->testSNKa1_2',(/2_SNK,3_SNK/),'comment')

    !Testing addition of 1-D array SLK routine to parameter list
    CALL testParam%add('testPL->testSLKa1',(/-4_SLK,-5_SLK/))
    CALL testParam%add('testPL->testSLKa1_2',(/4_SLK,5_SLK/),'comment')

    !Testing addition of 1-D array SBK routine to parameter list
    CALL testParam%add('testPL->testSBKa1',(/.TRUE.,.FALSE./))
    CALL testParam%add('testPL->testSBKa1_2',(/.FALSE.,.FALSE./),'comment')

    !Testing addition of 1-D array STR routine to parameter list
    ALLOCATE(str1(2))
    str1(1)='stringarray1'
    str1(2)='stringarray2'
    CALL testParam%add('testPL->testSTRa1',str1)
    str1(1)='stringarray3'
    str1(2)='stringarray4'
    CALL testParam%add('testPL->testSTRa1_2',str1,'comment')

    !Testing addition of 2-D array SSK routine to parameter list
    ALLOCATE(ssk2(2,2))
    ssk2(1,1)=1.1_SSK
    ssk2(2,1)=2.1_SSK
    ssk2(1,2)=1.2_SSK
    ssk2(2,2)=2.2_SSK
    CALL testParam%add('testPL->testSSKa2',ssk2)
    ssk2(1,1)=3.1_SSK
    ssk2(2,1)=4.1_SSK
    ssk2(1,2)=3.2_SSK
    ssk2(2,2)=4.2_SSK
    CALL testParam%add('testPL->testSSKa2_2',ssk2,'comment')

    !Testing addition of 2-D array SDK routine to parameter list
    ALLOCATE(sdk2(2,2))
    sdk2(1,1)=11.0_SDK
    sdk2(2,1)=21.0_SDK
    sdk2(1,2)=12.0_SDK
    sdk2(2,2)=22.0_SDK
    CALL testParam%add('testPL->testSDKa2',sdk2)
    sdk2(1,1)=31.0_SDK
    sdk2(2,1)=41.0_SDK
    sdk2(1,2)=32.0_SDK
    sdk2(2,2)=42.0_SDK
    CALL testParam%add('testPL->testSDKa2_2',sdk2,'comment')

    !Testing addition of 2-D array SNK routine to parameter list
    ALLOCATE(snk2(2,2))
    snk2(1,1)=11
    snk2(2,1)=21
    snk2(1,2)=12
    snk2(2,2)=22
    CALL testParam%add('testPL->testSNKa2',snk2)
    snk2(1,1)=31
    snk2(2,1)=41
    snk2(1,2)=32
    snk2(2,2)=42
    CALL testParam%add('testPL->testSNKa2_2',snk2,'comment')

    !Testing addition of 2-D array SLK routine to parameter list
    ALLOCATE(slk2(2,2))
    slk2(1,1)=110
    slk2(2,1)=210
    slk2(1,2)=120
    slk2(2,2)=220
    CALL testParam%add('testPL->testSLKa2',slk2)
    slk2(1,1)=310
    slk2(2,1)=410
    slk2(1,2)=320
    slk2(2,2)=420
    CALL testParam%add('testPL->testSLKa2_2',slk2,'comment')

    !Testing addition of 2-D array STR routine to parameter list
    ALLOCATE(str2(2,2))
    str2(1,1)='stringarray1'
    str2(2,1)='stringarray2'
    str2(1,2)='stringarray3'
    str2(2,2)='stringarray4'
    CALL testParam%add('testPL->testSTRa2',str2)
    str2(1,1)='stringarray5'
    str2(2,1)='stringarray6'
    str2(1,2)='stringarray7'
    str2(2,2)='stringarray8'
    CALL testParam%add('testPL->testSTRa2_2',str2,'comment')

    !Testing addition of 3-D array SSK routine to parameter list
    ALLOCATE(ssk3(2,2,2))
    ssk3(1,1,1)=1.11_SSK
    ssk3(2,1,1)=2.11_SSK
    ssk3(1,2,1)=1.21_SSK
    ssk3(2,2,1)=2.21_SSK
    ssk3(1,1,2)=1.12_SSK
    ssk3(2,1,2)=2.12_SSK
    ssk3(1,2,2)=1.22_SSK
    ssk3(2,2,2)=2.22_SSK
    CALL testParam%add('testPL->testSSKa3',ssk3)
    ssk3(1,1,1)=3.11_SSK
    ssk3(2,1,1)=4.11_SSK
    ssk3(1,2,1)=3.21_SSK
    ssk3(2,2,1)=4.21_SSK
    ssk3(1,1,2)=3.12_SSK
    ssk3(2,1,2)=4.12_SSK
    ssk3(1,2,2)=3.22_SSK
    ssk3(2,2,2)=4.22_SSK
    CALL testParam%add('testPL->testSSKa3_2',ssk3,'comment')

    !Testing addition of 3-D array SDK routine to parameter list
    ALLOCATE(sdk3(2,2,2))
    sdk3(1,1,1)=11.1_SDK
    sdk3(2,1,1)=21.1_SDK
    sdk3(1,2,1)=12.1_SDK
    sdk3(2,2,1)=22.1_SDK
    sdk3(1,1,2)=11.2_SDK
    sdk3(2,1,2)=21.2_SDK
    sdk3(1,2,2)=12.2_SDK
    sdk3(2,2,2)=22.2_SDK
    CALL testParam%add('testPL->testSDKa3',sdk3)
    sdk3(1,1,1)=31.1_SDK
    sdk3(2,1,1)=41.1_SDK
    sdk3(1,2,1)=32.1_SDK
    sdk3(2,2,1)=42.1_SDK
    sdk3(1,1,2)=31.2_SDK
    sdk3(2,1,2)=41.2_SDK
    sdk3(1,2,2)=32.2_SDK
    sdk3(2,2,2)=42.2_SDK
    CALL testParam%add('testPL->testSDKa3_2',sdk3,'comment')

    !Testing addition of 3-D array SNK routine to parameter list
    ALLOCATE(snk3(2,2,2))
    snk3(1,1,1)=111
    snk3(2,1,1)=211
    snk3(1,2,1)=121
    snk3(2,2,1)=221
    snk3(1,1,2)=112
    snk3(2,1,2)=212
    snk3(1,2,2)=122
    snk3(2,2,2)=222
    CALL testParam%add('testPL->testSNKa3',snk3)
    snk3(1,1,1)=311
    snk3(2,1,1)=411
    snk3(1,2,1)=321
    snk3(2,2,1)=421
    snk3(1,1,2)=312
    snk3(2,1,2)=412
    snk3(1,2,2)=322
    snk3(2,2,2)=422
    CALL testParam%add('testPL->testSNKa3_2',snk3,'comment')

    !Testing addition of 3-D array SLK routine to parameter list
    ALLOCATE(slk3(2,2,2))
    slk3(1,1,1)=111
    slk3(2,1,1)=211
    slk3(1,2,1)=121
    slk3(2,2,1)=221
    slk3(1,1,2)=112
    slk3(2,1,2)=212
    slk3(1,2,2)=122
    slk3(2,2,2)=222
    CALL testParam%add('testPL->testSLKa3',slk3)
    slk3(1,1,1)=311
    slk3(2,1,1)=411
    slk3(1,2,1)=321
    slk3(2,2,1)=421
    slk3(1,1,2)=312
    slk3(2,1,2)=412
    slk3(1,2,2)=322
    slk3(2,2,2)=422
    CALL testParam%add('testPL->testSLKa3_2',slk3,'comment')

    !assign the same PL
    !Test a dummy exception handler
    tmpe=e
    testParam2=testParam
    CALL testParam%verifyList(testParam2,bool,tmpe)
    ASSERT(bool,'verify the copy')

    nerror=tmpe%getCounter(EXCEPTION_ERROR)
    CALL testParam2%clear()
    CALL testParam%verifyList(testParam2,bool,tmpe)
    ASSERT(.NOT.bool,'arg uninit')
    ASSERT(nerror == tmpe%getCounter(EXCEPTION_ERROR),'arg uninit')
    nerror=tmpe%getCounter(EXCEPTION_ERROR)
    CALL testParam2%verifyList(testParam,bool,tmpe)
    ASSERT(.NOT.bool,'this uninit')
    ASSERT(nerror+44 == tmpe%getCounter(EXCEPTION_ERROR),'this uninit')

    !assign the same PL
    !Test using the eParams exception handler
    testParam2=testParam
    CALL testParam%verifyList(testParam2,bool,eParams)
    ASSERT(bool,'verify the copy')

    nerror=eParams%getCounter(EXCEPTION_ERROR)
    CALL testParam2%clear()
    CALL testParam%verifyList(testParam2,bool,eParams)
    ASSERT(.NOT.bool,'arg uninit')
    ASSERT(nerror == eParams%getCounter(EXCEPTION_ERROR),'arg uninit')
    nerror=eParams%getCounter(EXCEPTION_ERROR)
    CALL testParam2%verifyList(testParam,bool,eParams)
    ASSERT(.NOT.bool,'this uninit')
    ASSERT(nerror+44 == eParams%getCounter(EXCEPTION_ERROR),'this uninit')

    !Deallocate locals
    DEALLOCATE(snk2)
    DEALLOCATE(snk3)
    DEALLOCATE(slk2)
    DEALLOCATE(slk3)
    DEALLOCATE(ssk2)
    DEALLOCATE(ssk3)
    DEALLOCATE(sdk2)
    DEALLOCATE(sdk3)
    DEALLOCATE(str1)
    DEALLOCATE(str2)

    CALL clear_test_vars()
  ENDSUBROUTINE testVerifyList
!
!-------------------------------------------------------------------------------
!Test to make sure we've eliminated a long standing bug.
  SUBROUTINE testPartialMatch()

    CALL testParam%clear()
    CALL testParam%add('List->sublist->a',1)
    ASSERT(testParam%has('a'),'partial match no list')
    ASSERT(.NOT.testParam%has('List->a'),'no partial match')
    ASSERT(testParam%has('sublist->a'),'partial sublist match')

    CALL testParam%get('List',someParam)
    ASSERT(someParam%has('a'),'partial match no list (pointer)')
    ASSERT(.NOT.someParam%has('List->a'),'no partial match (pointer)')
    ASSERT(someParam%has('sublist->a'),'partial sublist match (pointer)')

    CALL testParam%clear()
  ENDSUBROUTINE testPartialMatch

!
!-------------------------------------------------------------------------------
  SUBROUTINE testInitFromXML()
    TYPE(StringType) :: str
    TYPE(StringType),ALLOCATABLE :: astr(:)
    INTEGER(SIK) :: tmpint
    LOGICAL(SBK) :: bool

    str='CASL AMA Benchmark Problem 2 - Fuel Lattice - Public'
    CALL testParam2%add('CASEID->case_id',TRIM(str))
    tmpint=10
    CALL testParam2%add('CASEID->STATE->testVal',tmpint)
    str='qtr'
    CALL testParam2%add('CASEID->STATE->sym',str)
    ALLOCATE(astr(1))
    astr(1)='LAT1'
    CALL testParam2%add('CASEID->ASSEMBLIES->Assembly_ASSY1->axial_labels',astr)
    DEALLOCATE(astr)
    CALL testParam2%add('CASEID->ASSEMBLIES->ASSEMBLY_ASSY1->SpacerGrids',testParam3)
    str='ASSY1'
    CALL testParam2%add('CASEID->ASSEMBLIES->Assembly_ASSY1->label',str)

    CALL testParam%clear()
    CALL testParam%initFromXML('testInit.xml')
    CALL testParam%verify(testParam2,bool)
    ASSERT(bool,"init from XML file")

    CALL testParam%clear()
    CALL testParam2%clear()
  ENDSUBROUTINE testInitFromXML
!
!-------------------------------------------------------------------------------
#ifdef FUTILITY_HAVE_Trilinos
  SUBROUTINE testConvertTeuchos()
    TYPE(ParamType) :: params
    TYPE(ForTeuchos_ParameterList_ID) :: teuchos_plist
    INTEGER(C_INT) :: ierr
    INTEGER(C_INT) :: int
    REAL(SDK) :: float
    LOGICAL(SBK) :: bool
    CHARACTER(LEN=1024) :: string
    TYPE(StringType) :: strtype


    CALL params%add("test_params->some_int", 5_SNK)
    CALL params%add("test_params->some_double", 3.14_SDK)
    CALL params%add("test_params->some_string", "fa la la la la!")
    CALL params%add("test_params->some_level->data_int", 4_SNK)
    CALL params%add("test_params->some_level->data_int2", 8_SNK)
    CALL params%add("test_params->more_lower_stuff", 8.7_SDK)
    CALL params%add("test_params->look_a_bool", .true.)
    CALL params%add("test_params->a_false_bool", .false.)


    teuchos_plist = Teuchos_ParameterList_Create(ierr)
    CALL params%toTeuchosPlist(teuchos_plist)
    CALL Teuchos_ParameterList_print(teuchos_plist, ierr)

    int = ForTeuchos_PL_get_int(teuchos_plist, "some_int", ierr);
    ASSERT(int==5,"some_int")

    int = ForTeuchos_PL_get_int(&
      ForTeuchos_PL_sublist_existing(teuchos_plist, "some_level", ierr),&
      "data_int", ierr)
    ASSERT(int==4,"some_level->data_int")
    FINFO()int,ierr

    float = ForTeuchos_PL_get_double(teuchos_plist, "some_double", ierr)
    ASSERT(float == 3.14_SDK, "some_double")

    CALL ForTeuchos_PL_get_string(teuchos_plist, "some_string", string, ierr)
    strtype = TRIM(string)
    ASSERT(strtype=="fa la la la la!", "some_string")

    bool = ForTeuchos_PL_get_bool(teuchos_plist, "look_a_bool", ierr)
    ASSERT(bool,"bool")

    bool = ForTeuchos_PL_get_bool(teuchos_plist, "a_false_bool", ierr)
    ASSERT(.not.bool,"bool")


  ENDSUBROUTINE testConvertTeuchos
#endif
!
!-------------------------------------------------------------------------------
  SUBROUTINE testEditToXML()
    LOGICAL(SBK) :: bool
    CALL testParam%initFromXML('testInit.xml')
    CALL testParam%editToXML('testEdit.xml')
    CALL testParam2%initFromXML('testEdit.xml')
    CALL testParam%verify(testParam2,bool)
    ASSERT(bool,"edit to XML file")

    CALL testParam%clear()
    CALL testParam2%clear()
  ENDSUBROUTINE testEditToXML
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
