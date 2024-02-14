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
USE HashModule
USE Strings
USE IO_Strings
USE Times
USE ExceptionHandler
USE ParameterLists
USE FileType_HDF5
#ifdef FUTILITY_HAVE_Trilinos
USE ForTeuchos_ParameterList
USE ISO_C_BINDING
#endif

IMPLICIT NONE

REAL(SSK) :: valssk,refssk
REAL(SDK) :: valsdk,refsdk
INTEGER(SNK) :: valsnk,refsnk
INTEGER(SLK) :: valslk,refslk
LOGICAL(SBK) :: valsbk,refsbk
CHARACTER(:),ALLOCATABLE :: valchar
TYPE(StringType) :: valstr,refstr
REAL(SSK),ALLOCATABLE :: valsska1(:),refsska1(:)
REAL(SDK),ALLOCATABLE :: valsdka1(:),refsdka1(:)
INTEGER(SNK),ALLOCATABLE :: valsnka1(:),refsnka1(:)
INTEGER(SLK),ALLOCATABLE :: valslka1(:),refslka1(:)
LOGICAL(SBK),ALLOCATABLE :: valsbka1(:),refsbka1(:)
TYPE(StringType),ALLOCATABLE :: valstra1(:),refstra1(:)
REAL(SSK),ALLOCATABLE :: valsska2(:,:),refsska2(:,:)
REAL(SDK),ALLOCATABLE :: valsdka2(:,:),refsdka2(:,:)
INTEGER(SNK),ALLOCATABLE :: valsnka2(:,:),refsnka2(:,:)
INTEGER(SLK),ALLOCATABLE :: valslka2(:,:),refslka2(:,:)
LOGICAL(SBK),ALLOCATABLE :: valsbka2(:,:),refsbka2(:,:)
TYPE(StringType),ALLOCATABLE :: valstra2(:,:),refstra2(:,:)
REAL(SSK),ALLOCATABLE :: valsska3(:,:,:),refsska3(:,:,:)
REAL(SDK),ALLOCATABLE :: valsdka3(:,:,:),refsdka3(:,:,:)
INTEGER(SNK),ALLOCATABLE :: valsnka3(:,:,:),refsnka3(:,:,:)
INTEGER(SLK),ALLOCATABLE :: valslka3(:,:,:),refslka3(:,:,:)
LOGICAL(SBK),ALLOCATABLE :: valsbka3(:,:,:),refsbka3(:,:,:)
TYPE(StringType),ALLOCATABLE :: valstra3(:,:,:),refstra3(:,:,:)
REAL(SSK),ALLOCATABLE :: valsska4(:,:,:,:),refsska4(:,:,:,:)
REAL(SDK),ALLOCATABLE :: valsdka4(:,:,:,:),refsdka4(:,:,:,:)
INTEGER(SNK),ALLOCATABLE :: valsnka4(:,:,:,:),refsnka4(:,:,:,:)
INTEGER(SLK),ALLOCATABLE :: valslka4(:,:,:,:),refslka4(:,:,:,:)
REAL(SSK),ALLOCATABLE :: valsska5(:,:,:,:,:),refsska5(:,:,:,:,:)
REAL(SDK),ALLOCATABLE :: valsdka5(:,:,:,:,:),refsdka5(:,:,:,:,:)
INTEGER(SNK),ALLOCATABLE :: valsnka5(:,:,:,:,:),refsnka5(:,:,:,:,:)
INTEGER(SLK),ALLOCATABLE :: valslka5(:,:,:,:,:),refslka5(:,:,:,:,:)
REAL(SSK),ALLOCATABLE :: valsska6(:,:,:,:,:,:),refsska6(:,:,:,:,:,:)
REAL(SDK),ALLOCATABLE :: valsdka6(:,:,:,:,:,:),refsdka6(:,:,:,:,:,:)
INTEGER(SNK),ALLOCATABLE :: valsnka6(:,:,:,:,:,:),refsnka6(:,:,:,:,:,:)
INTEGER(SLK),ALLOCATABLE :: valslka6(:,:,:,:,:,:),refslka6(:,:,:,:,:,:)
REAL(SSK),ALLOCATABLE :: valsska7(:,:,:,:,:,:,:),refsska7(:,:,:,:,:,:,:)
REAL(SDK),ALLOCATABLE :: valsdka7(:,:,:,:,:,:,:),refsdka7(:,:,:,:,:,:,:)
INTEGER(SNK),ALLOCATABLE :: valsnka7(:,:,:,:,:,:,:),refsnka7(:,:,:,:,:,:,:)
INTEGER(SLK),ALLOCATABLE :: valslka7(:,:,:,:,:,:,:),refslka7(:,:,:,:,:,:,:)
TYPE(ExceptionHandlerType),POINTER :: e

TYPE(ParamType) :: testParam,testParam2,testParam3,someParam

CALL runTestParameterLists()

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE runTestParameterLists()

  CREATE_TEST('PARAMETER TYPES')

  ALLOCATE(e)
  CALL e%setStopOnError(.FALSE.)
  CALL e%setQuietMode(.FALSE.)
  CALL eParams%addSurrogate(e)

  REGISTER_SUBTEST('Iterator',testIterator)
  REGISTER_SUBTEST('testClear',testClear)
  REGISTER_SUBTEST('testHas',testHas)
  REGISTER_SUBTEST('testRemove',testRemove)
  REGISTER_SUBTEST('testAssignment',testAssignment)
#ifdef FUTILITY_HAVE_HDF5
  REGISTER_SUBTEST('testHDF5',testHDF5)
#endif
  REGISTER_SUBTEST('Scalar SNK',testSNK)
  REGISTER_SUBTEST('Scalar SLK',testSLK)
  REGISTER_SUBTEST('Scalar SSK',testSSK)
  REGISTER_SUBTEST('Scalar SBK',testSBK)
  REGISTER_SUBTEST('Scalar SDK',testSDK)
  REGISTER_SUBTEST('Scalar CHAR',testCHAR)
  REGISTER_SUBTEST('Scalar CHAR->string',testCHAR2STR)
  REGISTER_SUBTEST('Scalar STR',testSTR)
  REGISTER_SUBTEST('1-D SBK',testSBKa1)
  REGISTER_SUBTEST('1-D SNK',testSNKa1)
  REGISTER_SUBTEST('1-D SLK',testSLKa1)
  REGISTER_SUBTEST('1-D SSK',testSSKa1)
  REGISTER_SUBTEST('1-D SDK',testSDKa1)
  REGISTER_SUBTEST('1-D STR',testSTRa1)
  REGISTER_SUBTEST('2-D SBK',testSBKa2)
  REGISTER_SUBTEST('2-D SNK',testSNKa2)
  REGISTER_SUBTEST('2-D SLK',testSLKa2)
  REGISTER_SUBTEST('2-D SSK',testSSKa2)
  REGISTER_SUBTEST('2-D SDK',testSDKa2)
  REGISTER_SUBTEST('2-D STR',testSTRa2)
  REGISTER_SUBTEST('3-D SBK',testSBKa3)
  REGISTER_SUBTEST('3-D SNK',testSNKa3)
  REGISTER_SUBTEST('3-D SLK',testSLKa3)
  REGISTER_SUBTEST('3-D SSK',testSSKa3)
  REGISTER_SUBTEST('3-D SDK',testSDKa3)
  REGISTER_SUBTEST('3-D STR',testSTRa3)
  REGISTER_SUBTEST('4-D SNK',testSNKa4)
  REGISTER_SUBTEST('4-D SLK',testSLKa4)
  REGISTER_SUBTEST('4-D SSK',testSSKa4)
  REGISTER_SUBTEST('4-D SDK',testSDKa4)
  REGISTER_SUBTEST('5-D SNK',testSNKa5)
  REGISTER_SUBTEST('5-D SLK',testSLKa5)
  REGISTER_SUBTEST('5-D SSK',testSSKa5)
  REGISTER_SUBTEST('5-D SDK',testSDKa5)
  REGISTER_SUBTEST('6-D SNK',testSNKa6)
  REGISTER_SUBTEST('6-D SLK',testSLKa6)
  REGISTER_SUBTEST('6-D SSK',testSSKa6)
  REGISTER_SUBTEST('6-D SDK',testSDKa6)
  REGISTER_SUBTEST('7-D SNK',testSNKa7)
  REGISTER_SUBTEST('7-D SLK',testSLKa7)
  REGISTER_SUBTEST('7-D SSK',testSSKa7)
  REGISTER_SUBTEST('7-D SDK',testSDKa7)
  REGISTER_SUBTEST('Tree',testTree)
  REGISTER_SUBTEST('%getSubPL(...)',testGetSubPL)
!   REGISTER_SUBTEST('%getString(...)',testGetString)
!   REGISTER_SUBTEST('%convertTo2DStringArray(...)',testConvertTo2DStringArray)
  REGISTER_SUBTEST('%validate(...)',testValidate)
  REGISTER_SUBTEST('%verify(...)',testVerify)
!   REGISTER_SUBTEST('%verifyList(...)',testVerifyList)
!   REGISTER_SUBTEST('Partial Matching',testPartialMatch)
  REGISTER_SUBTEST('Copy',testCopy)

  REGISTER_SUBTEST('%initFromXML',testInitFromXML)
  REGISTER_SUBTEST('%editToXML',testEditToXML)

! #ifdef FUTILITY_HAVE_Trilinos
!   REGISTER_SUBTEST('Convert to Teuchos', testConvertTeuchos)
! #endif
!   REGISTER_SUBTEST('Performance',testPerformance)

  FINALIZE_TEST()
!   CALL clear_test_vars()
  DEALLOCATE(e)

ENDSUBROUTINE runTestParameterLists
!
!-------------------------------------------------------------------------------
SUBROUTINE testIterator()
  INTEGER(SIK) :: i
  TYPE(StringType) :: name
  TYPE(ParamTypeIterator) :: testIter

  COMPONENT_TEST('Accessors')
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'','empty name')
  CALL testParam%add('bob',0_SNK)
  CALL testIter%activate(testParam,SUBLISTS=.TRUE.)
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'bob','one level name')
  ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_SNK0,'one level type')
  CALL testParam%clear()
  CALL testIter%clear()

  CALL testParam%add('bob -> loblaw',0.0_SDK)
  CALL testIter%activate(testParam,SUBLISTS=.TRUE.)
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'bob->loblaw','two level names')
  name = testIter%getCurrentName(.FALSE.)
  ASSERT_EQ(CHAR(name),'loblaw','two level names, only end')
  ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_SDK0,'two level type')
  CALL testParam%clear()
  CALL testIter%clear()

  CALL testParam%add('bob -> loblaw -> lobs -> law -> bomb',.FALSE.)
  CALL testIter%activate(testParam,SUBLISTS=.TRUE.)
  name = testIter%getCurrentName()
  ASSERT_EQ(char(name),'bob->loblaw->lobs->law->bomb','five level names')
  name = testIter%getCurrentName(.FALSE.)
  ASSERT_EQ(CHAR(name),'bomb','five level names, only end')
  ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_SBK0,'five level type')
  CALL testParam%clear()
  CALL testIter%clear()

  !NOTE: This tests the iterator methods %activate and %advance, but it also
  !implicitly tests the tree methods %getFirstNode and %getNextNode as well
  !as the module subroutines getRightmostNode and getLeftmostNode.
  COMPONENT_TEST('SingleTree')

  !Use the picture from https://stackoverflow.com/questions/2942517/how-do-i-iterate-over-binary-tree
  !to test basic node traversal
  CALL testParam%add(ACHAR(39),8_SNK) ! '
  CALL testParam%add(ACHAR(34),3_SNK) ! "
  CALL testParam%add(ACHAR(33),1_SNK) ! ! !this should be 32 to match stackoverflow, but 32 is whitespace, so shift by 1
  CALL testParam%add(ACHAR(37),6_SNK) ! %
  CALL testParam%add(ACHAR(35),4_SNK) ! #
  CALL testParam%add(ACHAR(38),7_SNK) ! &
  CALL testParam%add(ACHAR(41),10_SNK) ! )
  CALL testParam%add(ACHAR(45),14_SNK) ! -
  CALL testParam%add(ACHAR(44),13_SNK) ! ,

  CALL testIter%activate(testParam,SUBLISTS=.TRUE.)
  DO i=1,10
    name = testIter%getCurrentName()
    IF(i < 10) THEN
      ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_SNK0,'%dataType, i='//str(i))
      ASSERT(testIter%isActive,'%isActive, i='//str(i))
    ELSE
      ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_NULL,'%dataType, i='//str(i))
      ASSERT(.NOT.testIter%isActive,'%isActive, i='//str(i))
    ENDIF
    SELECTCASE(i)
    CASE(1)
      ASSERT_EQ(CHAR(name),ACHAR(33),'%name, i='//str(i))
    CASE(2)
      ASSERT_EQ(CHAR(name),ACHAR(34),'%name, i='//str(i))
    CASE(3)
      ASSERT_EQ(CHAR(name),ACHAR(35),'%name, i='//str(i))
    CASE(4)
      ASSERT_EQ(CHAR(name),ACHAR(37),'%name, i='//str(i))
    CASE(5)
      ASSERT_EQ(CHAR(name),ACHAR(38),'%name, i='//str(i))
    CASE(6)
      ASSERT_EQ(CHAR(name),ACHAR(39),'%name, i='//str(i))
    CASE(7)
      ASSERT_EQ(CHAR(name),ACHAR(41),'%name, i='//str(i))
    CASE(8)
      ASSERT_EQ(CHAR(name),ACHAR(44),'%name, i='//str(i))
    CASE(9)
      ASSERT_EQ(CHAR(name),ACHAR(45),'%name, i='//str(i))
    CASE(10)
      ASSERT_EQ(CHAR(name),'','%name, i='//str(i))
      EXIT
    ENDSELECT
    CALL testIter%advance()
  ENDDO !i

  COMPONENT_TEST('Nested Tree')

  !Test nested trees too.  Extending the stackoverflow example, we'll add a
  !subtree as the right leaf of 4 (leaf=5), a subtree as the left leaft of 10 (leaf=9), and
  !a subtree as the right leaf of 9->14 (leaf=15)
  CALL testParam%add(ACHAR(36)//'->'//ACHAR(39),8_SNK)
  CALL testParam%add(ACHAR(36)//'->'//ACHAR(34),3_SNK)
  CALL testParam%add(ACHAR(36)//'->'//ACHAR(33),1_SNK) !this should be 32 to match stackoverflow, but 32 is whitespace, so shift by 1
  CALL testParam%add(ACHAR(36)//'->'//ACHAR(37),6_SNK)
  CALL testParam%add(ACHAR(36)//'->'//ACHAR(35),4_SNK)
  CALL testParam%add(ACHAR(36)//'->'//ACHAR(38),7_SNK)
  CALL testParam%add(ACHAR(36)//'->'//ACHAR(41),10_SNK)
  CALL testParam%add(ACHAR(36)//'->'//ACHAR(45),14_SNK)
  CALL testParam%add(ACHAR(36)//'->'//ACHAR(44),13_SNK)
  !
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(39),8_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(34),3_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(33),1_SNK) !this should be 32 to match stackoverflow, but 32 is whitespace, so shift by 1
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(37),6_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(35),4_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(38),7_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(41),10_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(45),14_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(44),13_SNK)
  !
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(39),8_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(34),3_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(33),1_SNK) !this should be 32 to match stackoverflow, but 32 is whitespace, so shift by 1
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(37),6_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(35),4_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(38),7_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(41),10_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(45),14_SNK)
  CALL testParam%add(ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(44),13_SNK)

  CALL testIter%activate(testParam,SUBLISTS=.TRUE.)
  DO i=1,40
    name = testIter%getCurrentName()
    IF(i == 4 .OR. i == 17 .OR. i == 27) THEN
      ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_TREE,'%dataType, i='//str(i))
      ASSERT(testIter%isActive,'%isActive, i='//str(i))
    ELSEIF(i < 40) THEN
      ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_SNK0,'%dataType, i='//str(i))
      ASSERT(testIter%isActive,'%isActive, i='//str(i))
    ELSE
      ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_NULL,'%dataType, i='//str(i))
      ASSERT(.NOT.testIter%isActive,'%isActive, i='//str(i))
    ENDIF
    SELECTCASE(i)
    CASE(1) !1
      ASSERT_EQ(CHAR(name),ACHAR(33),'%name, i='//str(i))
    CASE(2) !3
      ASSERT_EQ(CHAR(name),ACHAR(34),'%name, i='//str(i))
    CASE(3) !4
      ASSERT_EQ(CHAR(name),ACHAR(35),'%name, i='//str(i))
    CASE(4)
      ASSERT_EQ(CHAR(name),ACHAR(36),'%name, i='//str(i))
    CASE(5)
      ASSERT_EQ(CHAR(name),ACHAR(36)//'->'//ACHAR(33),'%name, i='//str(i))
    CASE(6)
      ASSERT_EQ(CHAR(name),ACHAR(36)//'->'//ACHAR(34),'%name, i='//str(i))
    CASE(7)
      ASSERT_EQ(CHAR(name),ACHAR(36)//'->'//ACHAR(35),'%name, i='//str(i))
    CASE(8)
      ASSERT_EQ(CHAR(name),ACHAR(36)//'->'//ACHAR(37),'%name, i='//str(i))
    CASE(9)
      ASSERT_EQ(CHAR(name),ACHAR(36)//'->'//ACHAR(38),'%name, i='//str(i))
    CASE(10)
      ASSERT_EQ(CHAR(name),ACHAR(36)//'->'//ACHAR(39),'%name, i='//str(i))
    CASE(11)
      ASSERT_EQ(CHAR(name),ACHAR(36)//'->'//ACHAR(41),'%name, i='//str(i))
    CASE(12)
      ASSERT_EQ(CHAR(name),ACHAR(36)//'->'//ACHAR(44),'%name, i='//str(i))
    CASE(13)
      ASSERT_EQ(CHAR(name),ACHAR(36)//'->'//ACHAR(45),'%name, i='//str(i))
    CASE(14)
      ASSERT_EQ(CHAR(name),ACHAR(37),'%name, i='//str(i))
    CASE(15)
      ASSERT_EQ(CHAR(name),ACHAR(38),'%name, i='//str(i))
    CASE(16)
      ASSERT_EQ(CHAR(name),ACHAR(39),'%name, i='//str(i))
    CASE(17)
      ASSERT_EQ(CHAR(name),ACHAR(40),'%name, i='//str(i))
    CASE(18)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(33),'%name, i='//str(i))
    CASE(19)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(34),'%name, i='//str(i))
    CASE(20)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(35),'%name, i='//str(i))
    CASE(21)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(37),'%name, i='//str(i))
    CASE(22)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(38),'%name, i='//str(i))
    CASE(23)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(39),'%name, i='//str(i))
    CASE(24)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(41),'%name, i='//str(i))
    CASE(25)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(44),'%name, i='//str(i))
    CASE(26)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(45),'%name, i='//str(i))
    CASE(27)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(46),'%name, i='//str(i))
    CASE(28)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(33),'%name, i='//str(i))
    CASE(29)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(34),'%name, i='//str(i))
    CASE(30)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(35),'%name, i='//str(i))
    CASE(31)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(37),'%name, i='//str(i))
    CASE(32)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(38),'%name, i='//str(i))
    CASE(33)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(39),'%name, i='//str(i))
    CASE(34)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(41),'%name, i='//str(i))
    CASE(35)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(44),'%name, i='//str(i))
    CASE(36)
      ASSERT_EQ(CHAR(name),ACHAR(40)//'->'//ACHAR(46)//'->'//ACHAR(45),'%name, i='//str(i))
    CASE(37) !10
      ASSERT_EQ(CHAR(name),ACHAR(41),'%name, i='//str(i))
    CASE(38) !13
      ASSERT_EQ(CHAR(name),ACHAR(44),'%name, i='//str(i))
    CASE(39) !14
      ASSERT_EQ(CHAR(name),ACHAR(45),'%name, i='//str(i))
    CASE(40) !end of iterator
      ASSERT_EQ(CHAR(name),'','%name, i='//str(i))
      EXIT
    ENDSELECT
    CALL testIter%advance()
  ENDDO !i

  CALL testParam%clear()

  COMPONENT_TEST('Complicated Tree')

  CALL testParam%clear()
  CALL testParam%add('LinearSolverType -> A -> MatrixType -> n',0_SNK)
  CALL testParam%add('LinearSolverType -> A -> MatrixType -> nnz',0_SNK)
  CALL testParam%add('LinearSolverType -> A -> MatrixType -> isSym',0_SNK)
  CALL testParam%add('LinearSolverType -> b -> VectorType -> n',0_SNK)
  CALL testParam%add('LinearSolverType -> x -> VectorType -> n',0_SNK)
  CALL testParam%add('LinearSolverType -> PC -> PreCondType -> pcType',0_SNK)
  CALL testParam%add('LinearSolverType -> timerName',0_SNK)
  CALL testParam%add('LinearSolverType -> numberOMP',0_SNK)
  CALL testParam%add('LinearSolverType -> MPI_Comm_ID',0_SNK)
  CALL testParam%add('LinearSolverType -> TPLType',0_SNK)
  CALL testParam%add('LinearSolverType -> matType',0_SNK)
  CALL testParam%add('LinearSolverType -> solverMethod',0_SNK)
  CALL testIter%activate(testParam,SUBLISTS=.TRUE.)
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->A->MatrixType->n','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->A->MatrixType->nnz','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->A->MatrixType->isSym','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->b','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->b->VectorType->n','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->x','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->x->VectorType->n','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->PC','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->PC->PreCondType->pcType','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->timerName','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->numberOMP','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->MPI_Comm_ID','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->TPLType','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->matType','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'LinearSolverType->solverMethod','iterator name')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  ASSERT(.NOT.testIter%isActive,'iterator is not active')
  CALL testIter%clear()
  CALL testParam%clear()

  COMPONENT_TEST('Complicated Tree, no sublists')

  CALL testParam%clear()
  CALL testParam%add('A -> MatrixType -> n',0_SNK)
  CALL testParam%add('A -> MatrixType -> nnz',0_SNK)
  CALL testParam%add('A -> MatrixType -> isSym',0_SNK)
  CALL testParam%add('b -> VectorType -> n',0_SNK)
  CALL testParam%add('x -> VectorType -> n',0_SNK)
  CALL testParam%add('PC -> PreCondType -> pcType',0_SNK)
  CALL testParam%add('timerName',0_SNK)
  CALL testParam%add('numberOMP',0_SNK)
  CALL testParam%add('MPI_Comm_ID',0_SNK)
  CALL testParam%add('TPLType',0_SNK)
  CALL testParam%add('matType',0_SNK)
  CALL testParam%add('solverMethod',0_SNK)
  CALL testIter%activate(testParam,SUBLISTS=.FALSE.)
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'A','iterator name')
  ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_TREE,'iterator type')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'b','iterator name')
  ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_TREE,'iterator type')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'x','iterator name')
  ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_TREE,'iterator type')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'PC','iterator name')
  ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_TREE,'iterator type')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'timerName','iterator name')
  ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_SNK0,'iterator type')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'numberOMP','iterator name')
  ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_SNK0,'iterator type')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'MPI_Comm_ID','iterator name')
  ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_SNK0,'iterator type')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'TPLType','iterator name')
  ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_SNK0,'iterator type')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'matType','iterator name')
  ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_SNK0,'iterator type')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'solverMethod','iterator name')
  ASSERT_EQ(testIter%getCurrentType(),PL_DATA_TYPE_SNK0,'iterator type')
  ASSERT(testIter%isActive,'iterator is active')
  CALL testIter%advance()
  ASSERT(.NOT.testIter%isActive,'iterator is not active')
  CALL testIter%clear()
  CALL testParam%clear()

  COMPONENT_TEST('empty list')
  CALL testParam%add('a',1_SNK)
  CALL testParam%add('b',1_SNK)
  CALL testParam%add('c -> d -> e -> f',1_SNK)
  CALL testParam%remove('c -> d -> e -> f')
  CALL testIter%activate(testParam,SUBLISTS=.TRUE.)
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'b','b')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'c','c')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'c->d->e','c->d->e')
  CALL testIter%advance()
  name = testIter%getCurrentName()
  ASSERT_EQ(CHAR(name),'','')

ENDSUBROUTINE testIterator
!
!-------------------------------------------------------------------------------
SUBROUTINE testClear()

  CALL testParam%add('testSNK1',0_SNK)
  CALL testParam%add('testSNK2',0_SNK)
  CALL testParam%add('testSNK3',0_SNK)
  CALL testParam%add('group -> testSNK1',0_SNK)
  CALL testParam%add('group -> group 2 -> testSNK1',0_SNK)
  CALL testParam%add('group -> group 2 -> testSNK2',0_SNK)

  CALL testParam%clear()
  ASSERT(.NOT.testparam%has('testSNK2'),'%clear')
  ASSERT(.NOT.testParam%has('testSNK3'),'%clear')

  CALL testParam%clear()

  CALL testParam%clear()

ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
SUBROUTINE testHas()

  CALL testParam%add('testSNK1',0_SNK)
  CALL testParam%add('testSNK2',0_SNK)
  CALL testParam%add('testSNK3',0_SNK)
  CALL testParam%add('group -> testSNK1',0_SNK)
  CALL testParam%add('group -> group 2 -> testSNK1',0_SNK)
  CALL testParam%add('group -> group 2 -> testSNK2',0_SNK)

  ASSERT(testParam%has('testSNK1'),'%has')
  ASSERT(testParam%has('testSNK2'),'%has')
  ASSERT(testParam%has('testSNK3'),'%has')
  ASSERT(testParam%has('group'),'%has')
  ASSERT(testParam%has('group -> testSNK1'),'%has')
  ASSERT(testParam%has('group -> group 2'),'%has')
  ASSERT(testParam%has('group -> group 2 -> testSNK1'),'%has')
  ASSERT(testParam%has('group -> group 2 -> testSNK2'),'%has')
  ASSERT(.NOT.testParam%has('testSNK11'),'%has')
  ASSERT(.NOT.testParam%has('testSNK2_'),'%has')
  ASSERT(.NOT.testParam%has('testSNK3-'),'%has')
  ASSERT(.NOT.testParam%has('group1 -> testSNK1'),'%has')
  ASSERT(.NOT.testParam%has('group1 -> group 2 -> testSNK1'),'%has')
  ASSERT(.NOT.testParam%has('group -> testSNK11'),'%has')
  ASSERT(.NOT.testParam%has('group -> group 21 -> testSNK1'),'%has')
  ASSERT(.NOT.testParam%has('group -> group 2 -> testSNK11'),'%has')
  ASSERT(.not.testParam%has('group -> group 2 -> testSNK21'),'%has')
  CALL testparam%clear()

ENDSUBROUTINE testHas
!
!-------------------------------------------------------------------------------
SUBROUTINE testRemove()

  COMPONENT_TEST('Single layer')
  !                 6
  !           /           \
  !       2                    10
  !    /     \             /         \
  !   1       4           8           12
  !         /   \       /   \       /    \
  !        3     5     7     9    11      13
  CALL testParam%add('6',0_SNK)
  CALL testParam%add('2',0_SNK)
  CALL testParam%add('1',0_SNK)
  CALL testParam%add('4',0_SNK)
  CALL testParam%add('3',0_SNK)
  CALL testParam%add('5',0_SNK)
  CALL testParam%add('10',0_SNK)
  CALL testParam%add('8',0_SNK)
  CALL testParam%add('7',0_SNK)
  CALL testParam%add('9',0_SNK)
  CALL testParam%add('12',0_SNK)
  CALL testParam%add('11',0_SNK)
  CALL testParam%add('13',0_SNK)

  CALL testParam%remove('3')
  ASSERT(.NOT.testParam%has('3'),'left leaf, no children')
  CALL testParam%remove('5')
  ASSERT(.NOT.testParam%has('5'),'right leaf, no children')
  CALL testParam%remove('2')
  ASSERT(.NOT.testParam%has('2'),'left leaf, 2 children')
  CALL testParam%remove('12')
  ASSERT(.NOT.testParam%has('12'),'right leaf, no children')
  CALL testParam%remove('3')
  ASSERT(.NOT.testParam%has('3'),'left leaf, no children')
  CALL testParam%remove('11')
  ASSERT(.NOT.testParam%has('3'),'right leaf, right child')
  CALL testParam%remove('7')
  ASSERT(.NOT.testParam%has('7'),'left leaf, right child')
  CALL testParam%remove('10')
  ASSERT(.NOT.testParam%has('10'),'right leaf, 2 children')

  CALL testParam%clear()

  CALL testParam%add('6',0_SNK)
  CALL testParam%add('2',0_SNK)
  CALL testParam%add('1',0_SNK)
  CALL testParam%add('4',0_SNK)
  CALL testParam%add('3',0_SNK)
  CALL testParam%add('5',0_SNK)
  CALL testParam%add('10',0_SNK)
  CALL testParam%add('8',0_SNK)
  CALL testParam%add('7',0_SNK)
  CALL testParam%add('9',0_SNK)
  CALL testParam%add('12',0_SNK)
  CALL testParam%add('11',0_SNK)
  CALL testParam%add('13',0_SNK)

  CALL testParam%remove('5')
  ASSERT(.NOT.testParam%has('5'),'right leaf, no children')
  CALL testParam%remove('4')
  ASSERT(.NOT.testParam%has('4'),'right leaf, left child')
  CALL testParam%remove('3')
  ASSERT(.NOT.testParam%has('3'),'right leaf, no children')
  CALL testParam%remove('2')
  ASSERT(.NOT.testParam%has('2'),'left leaf, left child')
  CALL testParam%remove('6')
  ASSERT(.NOT.testParam%has('6'),'root, 2 children')
  CALL testParam%remove('1')
  ASSERT(.NOT.testParam%has('1'),'root, right child')
  CALL testParam%remove('12')
  ASSERT(.NOT.testParam%has('12'),'right left, 2 children')
  CALL testParam%remove('10')
  ASSERT(.NOT.testParam%has('10'),'root, left child')
  CALL testParam%remove('8')
  ASSERT(.NOT.testParam%has('8'),'root, 2 children')
  CALL testParam%remove('7')
  ASSERT(.NOT.testParam%has('7'),'root, right child')
  CALL testParam%remove('9')
  ASSERT(.NOT.testParam%has('9'),'root, no children')

  CALL testParam%clear()

ENDSUBROUTINE testRemove
!
!-------------------------------------------------------------------------------
SUBROUTINE testAssignment()
  TYPE(ParamType) :: copyParam

  COMPONENT_TEST('single layer')
  CALL testParam%add('6',0_SNK)
  CALL testParam%add('2',0_SNK)
  CALL testParam%add('1',0_SNK)
  CALL testParam%add('4',0_SNK)
  CALL testParam%add('3',0_SNK)
  CALL testParam%add('5',0_SNK)
  CALL testParam%add('10',0_SNK)
  CALL testParam%add('8',0_SNK)
  CALL testParam%add('7',0_SNK)
  CALL testParam%add('9',0_SNK)
  CALL testParam%add('12',0_SNK)
  CALL testParam%add('11',0_SNK)
  CALL testParam%add('13',0_SNK)

  copyParam = testParam
  ASSERT(copyParam%has('1'),'1, post-copy')
  ASSERT(copyParam%has('2'),'2, post-copy')
  ASSERT(copyParam%has('3'),'3, post-copy')
  ASSERT(copyParam%has('4'),'4, post-copy')
  ASSERT(copyParam%has('5'),'5, post-copy')
  ASSERT(copyParam%has('6'),'6, post-copy')
  ASSERT(copyParam%has('7'),'7, post-copy')
  ASSERT(copyParam%has('8'),'8, post-copy')
  ASSERT(copyParam%has('9'),'9, post-copy')
  ASSERT(copyParam%has('10'),'10, post-copy')
  ASSERT(copyParam%has('11'),'11, post-copy')
  ASSERT(copyParam%has('12'),'12, post-copy')
  ASSERT(copyParam%has('13'),'13, post-copy')
  CALL testParam%clear()
  ASSERT(copyParam%has('1'),'1, post-clear')
  ASSERT(copyParam%has('2'),'2, post-clear')
  ASSERT(copyParam%has('3'),'3, post-clear')
  ASSERT(copyParam%has('4'),'4, post-clear')
  ASSERT(copyParam%has('5'),'5, post-clear')
  ASSERT(copyParam%has('6'),'6, post-clear')
  ASSERT(copyParam%has('7'),'7, post-clear')
  ASSERT(copyParam%has('8'),'8, post-clear')
  ASSERT(copyParam%has('9'),'9, post-clear')
  ASSERT(copyParam%has('10'),'10, post-clear')
  ASSERT(copyParam%has('11'),'11, post-clear')
  ASSERT(copyParam%has('12'),'12, post-clear')
  ASSERT(copyParam%has('13'),'13, post-clear')
  CALL copyParam%clear()

  COMPONENT_TEST('two layers - simple')
  CALL testParam%add('list -> 6',0_SNK)
  CALL testParam%add('list -> 2',0_SNK)
  CALL testParam%add('list -> 1',0_SNK)
  CALL testParam%add('list -> 4',0_SNK)
  CALL testParam%add('list -> 3',0_SNK)
  CALL testParam%add('list -> 5',0_SNK)
  CALL testParam%add('list -> 10',0_SNK)
  CALL testParam%add('list -> 8',0_SNK)
  CALL testParam%add('list -> 7',0_SNK)
  CALL testParam%add('list -> 9',0_SNK)
  CALL testParam%add('list -> 12',0_SNK)
  CALL testParam%add('list -> 11',0_SNK)
  CALL testParam%add('list -> 13',0_SNK)

  copyParam = testParam
  ASSERT(copyParam%has('1'),'1, post-copy')
  ASSERT(copyParam%has('2'),'2, post-copy')
  ASSERT(copyParam%has('3'),'3, post-copy')
  ASSERT(copyParam%has('4'),'4, post-copy')
  ASSERT(copyParam%has('5'),'5, post-copy')
  ASSERT(copyParam%has('6'),'6, post-copy')
  ASSERT(copyParam%has('7'),'7, post-copy')
  ASSERT(copyParam%has('8'),'8, post-copy')
  ASSERT(copyParam%has('9'),'9, post-copy')
  ASSERT(copyParam%has('10'),'10, post-copy')
  ASSERT(copyParam%has('11'),'11, post-copy')
  ASSERT(copyParam%has('12'),'12, post-copy')
  ASSERT(copyParam%has('13'),'13, post-copy')
  CALL testParam%clear()
  ASSERT(copyParam%has('1'),'1, post-clear')
  ASSERT(copyParam%has('2'),'2, post-clear')
  ASSERT(copyParam%has('3'),'3, post-clear')
  ASSERT(copyParam%has('4'),'4, post-clear')
  ASSERT(copyParam%has('5'),'5, post-clear')
  ASSERT(copyParam%has('6'),'6, post-clear')
  ASSERT(copyParam%has('7'),'7, post-clear')
  ASSERT(copyParam%has('8'),'8, post-clear')
  ASSERT(copyParam%has('9'),'9, post-clear')
  ASSERT(copyParam%has('10'),'10, post-clear')
  ASSERT(copyParam%has('11'),'11, post-clear')
  ASSERT(copyParam%has('12'),'12, post-clear')
  ASSERT(copyParam%has('13'),'13, post-clear')
  CALL copyParam%clear()

ENDSUBROUTINE testAssignment
!
!-------------------------------------------------------------------------------
SUBROUTINE testHDF5()
  TYPE(HDF5FileType) :: h5

  COMPONENT_TEST('Read PL')
  CALL setupHDF5testFile()
  CALL h5%init('readtest.h5','READ')

  CALL testParam%read(h5,'')
  CALL hdf5readTest(testParam,'')

  CALL h5%clear(.TRUE.)

  COMPONENT_TEST('Write PL to root')
  CALL h5%init('writetest2.h5','NEW')
  CALL h5%fopen()
  CALL testParam%edit(h5,'')
  CALL testParam2%read(h5,'')
  CALL hdf5readTest(testParam2,'')
  CALL testParam2%clear()
  CALL h5%fclose()
  CALL h5%clear(.TRUE.)

  COMPONENT_TEST('Write PL to group')
  CALL h5%init('writetest3.h5','NEW')
  CALL h5%fopen()
  CALL testParam%edit(h5,'path/to/test')
  CALL h5%clear()
  CALL h5%init('writetest3.h5','READ')
  CALL testParam2%read(h5,'path/to/test')
  CALL hdf5readTest(testParam2,'')
  CALL testParam2%clear()
  CALL h5%clear()

  COMPONENT_TEST('Write PL to group with slash')
  CALL h5%init('writetest4.h5','NEW')
  CALL h5%fopen()
  CALL testparam%edit(h5,'path/to/test/slash/')
  CALL h5%clear()
  CALL h5%init('writetest4.h5','READ')
  CALL testParam2%read(h5,'')
  CALL hdf5readTest(testParam2,'path -> to -> test -> slash ->')
  CALL testParam2%clear()
  CALL h5%clear()

  CALL testparam%clear()

  DEALLOCATE(refsnka1,refsnka2,refsnka3,refsnka4,refsnka5,refsnka6,refsnka7)
  DEALLOCATE(refslka1,refslka2,refslka3,refslka4,refslka5,refslka6,refslka7)
  DEALLOCATE(refsska1,refsska2,refsska3,refsska4,refsska5,refsska6,refsska7)
  DEALLOCATE(refsdka1,refsdka2,refsdka3,refsdka4,refsdka5,refsdka6,refsdka7)
  DEALLOCATE(refstra1,refstra2,refstra3)
  DEALLOCATE(refsbka1,refsbka2,refsbka3)
  refstr = ''

ENDSUBROUTINE testHDF5
!
!-------------------------------------------------------------------------------
SUBROUTINE hdf5readTest(param,prefix)
  CLASS(ParamType),INTENT(IN) :: param
  CHARACTER(LEN=*),INTENT(IN) :: prefix
  !
  INTEGER(SIK) :: i,j,k

  !Logicals
  CALL param%get(prefix//'groupB->memB0',valsbk)
  ASSERT(.NOT.valsbk,'B0 read PL Failure')
  CALL param%get(prefix//'groupB->memB1',valsbka1)
  ASSERT_EQ(SIZE(valsbka1),6,'SIZE, DIM=1')
  DO i=1,SIZE(valsbka1),2
    ASSERT_EQ(valsbka1(i),(MOD(i-1,2) /= 0),'B1 read PL Failure, ('//str(i)//')')
  ENDDO !i
  CALL param%get(prefix//'groupB->memB2',valsbka2)
  ASSERT_EQ(SIZE(valsbka2,DIM=1),6,'SIZE, DIM=1')
  ASSERT_EQ(SIZE(valsbka2,DIM=2),5,'SIZE, DIM=2')
  DO i=1,SIZE(valsbka2,DIM=1),2
    DO j=1,SIZE(valsbka2,DIM=2),3
      ASSERT_EQ(valsbka2(i,j),((MOD(i-1,2) /= 0) .AND. (MOD(j-1,3) /= 0)),'B2 read PL Failure, ('//str(i)//','//str(j)//')')
    ENDDO !j
  ENDDO !i
  CALL param%get(prefix//'groupB->memB3',valsbka3)
  ASSERT_EQ(SIZE(valsbka3,DIM=1),6,'SIZE, DIM=1')
  ASSERT_EQ(SIZE(valsbka3,DIM=2),5,'SIZE, DIM=2')
  ASSERT_EQ(SIZE(valsbka3,DIM=3),2,'SIZE, DIM=3')
  DO i=1,SIZE(valsbka3,DIM=1),2
    DO j=1,SIZE(valsbka3,DIM=2),3
      DO k=1,SIZE(valsbka3,DIM=3)
        ASSERT_EQ(valsbka3(i,j,k),((MOD(i-1,2) /= 0) .AND. (MOD(j-1,3) /= 0)),'B3 read PL Failure, ('//str(i)//','//str(j)//','//str(k)//')')
      ENDDO !k
    ENDDO !j
  ENDDO !i
  !Integers
  CALL param%get(prefix//'groupI->memL0',valslk)
  ASSERT_EQ(valslk,refslk,'L0 read PL Failure')
  CALL param%get(prefix//'groupI->memL1',valslka1)
  ASSERT(ALL(valslka1 == refslka1),'L1 read PL Failure')
  CALL param%get(prefix//'groupI->memL2',valslka2)
  ASSERT(ALL(valslka2 == refslka2),'L2 read PL Failure')
  CALL param%get(prefix//'groupI->memL3',valslka3)
  ASSERT(ALL(valslka3 == refslka3),'L3 read PL Failure')
  CALL param%get(prefix//'groupI->memL4',valslka4)
  ASSERT(ALL(valslka4 == refslka4),'L4 read PL Failure')
  CALL param%get(prefix//'groupI->memL5',valslka5)
  ASSERT(ALL(valslka5 == refslka5),'L5 read PL Failure')
  CALL param%get(prefix//'groupI->memL6',valslka6)
  ASSERT(ALL(valslka6 == refslka6),'L6 read PL Failure')
  CALL param%get(prefix//'groupI->memL7',valslka7)
  ASSERT(ALL(valslka7 == refslka7),'L7 read PL Failure')
  CALL param%get(prefix//'groupI->memN0',valsnk)
  ASSERT_EQ(valsnk,refsnk,'N0 read PL Failure')
  CALL param%get(prefix//'groupI->memN1',valsnka1)
  ASSERT(ALL(valsnka1 == refsnka1),'N1 read PL Failure')
  CALL param%get(prefix//'groupI->memN2',valsnka2)
  ASSERT(ALL(valsnka2 == refsnka2),'N2 read PL Failure')
  CALL param%get(prefix//'groupI->memN3',valsnka3)
  ASSERT(ALL(valsnka3 == refsnka3),'N3 read PL Failure')
  CALL param%get(prefix//'groupI->memN4',valsnka4)
  ASSERT(ALL(valsnka4 == refsnka4),'N4 read PL Failure')
  CALL param%get(prefix//'groupI->memN5',valsnka5)
  ASSERT(ALL(valsnka5 == refsnka5),'N5 read PL Failure')
  CALL param%get(prefix//'groupI->memN6',valsnka6)
  ASSERT(ALL(valsnka6 == refsnka6),'N6 read PL Failure')
  CALL param%get(prefix//'groupI->memN7',valsnka7)
  ASSERT(ALL(valsnka7 == refsnka7),'N7 read PL Failure')
  !Integer attributes
  valstr = param%getDescription(prefix//'groupI -> memL0')
  ASSERT_EQ(CHAR(valstr),'test description','memL0 description')
  valstr = param%getDescription(prefix//'groupI -> memL1')
  ASSERT_EQ(CHAR(valstr),'test description','memL1 description')
  valstr = param%getDescription(prefix//'groupI -> memL2')
  ASSERT_EQ(CHAR(valstr),'test description','memL2 description')
  !Reals
  CALL param%get(prefix//'groupR->memD0',valsdk)
  ASSERT_EQ(valsdk,refsdk,'D0 read PL Failure')
  CALL param%get(prefix//'groupR->memD1',valsdka1)
  ASSERT(ALL(valsdka1 == refsdka1),'D1 read PL Failure')
  CALL param%get(prefix//'groupR->memD2',valsdka2)
  ASSERT(ALL(valsdka2 == refsdka2),'D2 read PL Failure')
  CALL param%get(prefix//'groupR->memD3',valsdka3)
  ASSERT(ALL(valsdka3 == refsdka3),'D3 read PL Failure')
  CALL param%get(prefix//'groupR->memD4',valsdka4)
  ASSERT(ALL(valsdka4 == refsdka4),'D4 read PL Failure')
  CALL param%get(prefix//'groupR->memD5',valsdka5)
  ASSERT(ALL(valsdka5 == refsdka5),'D5 read PL Failure')
  CALL param%get(prefix//'groupR->memD6',valsdka6)
  ASSERT(ALL(valsdka6 == refsdka6),'D6 read PL Failure')
  CALL param%get(prefix//'groupR->memD7',valsdka7)
  ASSERT(ALL(valsdka7 == refsdka7),'D7 read PL Failure')
  CALL param%get(prefix//'groupR->memS0',valssk)
  ASSERT_EQ(valssk,refssk,'S0 read PL Failure')
  CALL param%get(prefix//'groupR->memS1',valsska1)
  ASSERT(ALL(valsska1 == refsska1),'S1 read PL Failure')
  CALL param%get(prefix//'groupR->memS2',valsska2)
  ASSERT(ALL(valsska2 == refsska2),'S2 read PL Failure')
  CALL param%get(prefix//'groupR->memS3',valsska3)
  ASSERT(ALL(valsska3 == refsska3),'S3 read PL Failure')
  CALL param%get(prefix//'groupR->memS4',valsska4)
  ASSERT(ALL(valsska4 == refsska4),'S4 read PL Failure')
  CALL param%get(prefix//'groupR->memS5',valsska5)
  ASSERT(ALL(valsska5 == refsska5),'S5 read PL Failure')
  CALL param%get(prefix//'groupR->memS6',valsska6)
  ASSERT(ALL(valsska6 == refsska6),'S6 read PL Failure')
  CALL param%get(prefix//'groupR->memS7',valsska7)
  ASSERT(ALL(valsska7 == refsska7),'S7 read PL Failure')
  !Strings
  CALL param%get(prefix//'groupST->memST0',valstr)
  ASSERT_EQ(CHAR(valstr),CHAR(refstr),'ST0 read PL Failure')
  CALL param%get(prefix//'groupST->memST1',valstra1)
  ASSERT_EQ(SIZE(valstra1),SIZE(refstra1),'ST1 Sizes')
  DO i=1,SIZE(valstra1)
    ASSERT_EQ(CHAR(valstra1(i)),CHAR(refstra1(i)),'ST1 read PL Failure')
  ENDDO
  CALL param%get(prefix//'groupST->memST2',valstra2)
  valsbk=(SIZE(valstra2,DIM=1) == SIZE(refstra2,DIM=1)) .AND. &
      (SIZE(valstra2,DIM=2) == SIZE(refstra2,DIM=2))
  ASSERT(valsbk,'ST2 Sizes')
  DO j=1,SIZE(valstra2,DIM=2)
    DO i=1,SIZE(valstra2,DIM=1)
      ASSERT_EQ(CHAR(valstra2(i,j)),CHAR(refstra2(i,j)),'ST2 read PL Failure')
     ENDDO
   ENDDO
   CALL param%get(prefix//'groupST->memST3',valstra3)
   valsbk=(SIZE(valstra3,DIM=1) == SIZE(refstra3,DIM=1)) .AND. &
       (SIZE(valstra3,DIM=2) == SIZE(refstra3,DIM=2)) .AND. &
       (SIZE(valstra3,DIM=3) == SIZE(refstra3,DIM=3))
   ASSERT(valsbk,'ST3 Sizes')
   DO k=1,SIZE(valstra3,DIM=3)
    DO j=1,SIZE(valstra2,DIM=2)
      DO i=1,SIZE(valstra2,DIM=1)
        ASSERT_EQ(CHAR(valstra3(i,j,k)),CHAR(refstra3(i,j,k)),'ST3 read PL Failure')
      ENDDO
    ENDDO
  ENDDO

  DEALLOCATE(valsnka1,valsnka2,valsnka3,valsnka4,valsnka5,valsnka6,valsnka7)
  DEALLOCATE(valslka1,valslka2,valslka3,valslka4,valslka5,valslka6,valslka7)
  DEALLOCATE(valsska1,valsska2,valsska3,valsska4,valsska5,valsska6,valsska7)
  DEALLOCATE(valsdka1,valsdka2,valsdka3,valsdka4,valsdka5,valsdka6,valsdka7)
  DEALLOCATE(valstra1,valstra2,valstra3)
  DEALLOCATE(valsbka1,valsbka2,valsbka3)

ENDSUBROUTINE hdf5readTest
!
!-------------------------------------------------------------------------------
SUBROUTINE testSNK()
  TYPE(StringType) :: msg,refmsg
  valsnk=5_SNK

  COMPONENT_TEST('add')
  CALL testParam%add('testSNK1',valsnk)
  CALL testParam%add('testSNK2',valsnk)
  CALL testParam%add('testSNK3',valsnk)
  CALL testParam%add('group - > testSNK1',valsnk)
  CALL testParam%add('group - > group 2 -> testSNK1',valsnk)

  COMPONENT_TEST('get')
  valsnk=0_SNK
  CALL testParam%get('testSNK1',valsnk)
  ASSERT_EQ(valsnk,5_SNK,'%get')
  valsnk=0_SNK
  CALL testParam%get('testSNK2',valsnk)
  ASSERT_EQ(valsnk,5_SNK,'%get')
  valsnk=0_SNK
  CALL testParam%get('testSNK3',valsnk)
  ASSERT_EQ(valsnk,5_SNK,'%get')
  valsnk=0_SNK
  CALL testParam%get('group - > testSNK1',valsnk)
  ASSERT_EQ(valsnk,5_SNK,'%get')
  valsnk=0_SNK
  CALL testParam%get('group - > group 2 -> testSNK1',valsnk)
  ASSERT_EQ(valsnk,5_SNK,'%get')
  valsnk=0_SNK
  CALL testParam%get('testSNK4',valsnk)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSNK4" of type "INTEGER(SNK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  valsnk=0_SNK
  CALL testParam%get('testSNK4',valsnk,DEFAULT=5_SNK)
  ASSERT_EQ(valsnk,5_SNK,'%get, default')

  COMPONENT_TEST('set')
  CALL testParam%set('testSNK2',8_SNK)
  valsnk=0_SNK
  CALL testParam%get('testSNK2',valsnk)
  ASSERT_EQ(valsnk,8_SNK,'%set')
  CALL testParam%set('testSNK4',8_SNK)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSNK4" of type "INTEGER(SNK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()

ENDSUBROUTINE testSNK
!
!-------------------------------------------------------------------------------
SUBROUTINE testSNKa1()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSNKa1(1))
  valSNKa1=5_SNK

  COMPONENT_TEST('add')
  CALL testParam%add('testSNK1',valSNKa1)
  CALL testParam%add('testSNK2',valSNKa1)
  CALL testParam%add('testSNK3',valSNKa1)
  CALL testParam%add('group - > testSNK1',valSNKa1)
  CALL testParam%add('group - > group 2 -> testSNK1',valSNKa1)

  COMPONENT_TEST('get')
  valSNKa1(1)=0_SNK
  CALL testParam%get('testSNK1',valSNKa1)
  ASSERT_EQ(valSNKa1(1),5_SNK,'%get')
  valSNKa1(1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa1)
  ASSERT_EQ(valSNKa1(1),5_SNK,'%get')
  valSNKa1(1)=0_SNK
  CALL testParam%get('testSNK3',valSNKa1)
  ASSERT_EQ(valSNKa1(1),5_SNK,'%get')
  valSNKa1(1)=0_SNK
  CALL testParam%get('group - > testSNK1',valSNKa1)
  ASSERT_EQ(valSNKa1(1),5_SNK,'%get')
  valSNKa1(1)=0_SNK
  CALL testParam%get('group - > group 2 -> testSNK1',valSNKa1)
  ASSERT_EQ(valSNKa1(1),5_SNK,'%get')
  valSNKa1(1)=0_SNK
  CALL testParam%get('testSNK4',valSNKa1)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSNK4" of type "1-D ARRAY INTEGER(SNK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSNKa1(1))
  valSNKa1(1)=0_SNK
  ALLOCATE(refSNKa1(1))
  refSNKa1=5_SNK
  CALL testParam%get('testSNK4',valSNKa1,DEFAULT=refSNKa1)
  ASSERT_EQ(valSNKa1(1),5_SNK,'%get, default')

  COMPONENT_TEST('set')
  refSNKa1=8_SNK
  CALL testParam%set('testSNK2',refSNKa1)
  valSNKa1(1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa1)
  ASSERT_EQ(valSNKa1(1),8_SNK,'%set')
  CALL testParam%set('testSNK4',refSNKa1)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSNK4" of type "1-D ARRAY INTEGER(SNK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSNKa1)
  DEALLOCATE(valSNKa1)

ENDSUBROUTINE testSNKa1
!
!-------------------------------------------------------------------------------
SUBROUTINE testSNKa2()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSNKa2(1,1))
  valSNKa2=5_SNK

  COMPONENT_TEST('add')
  CALL testParam%add('testSNK1',valSNKa2)
  CALL testParam%add('testSNK2',valSNKa2)
  CALL testParam%add('testSNK3',valSNKa2)
  CALL testParam%add('group - > testSNK1',valSNKa2)
  CALL testParam%add('group - > group 2 -> testSNK1',valSNKa2)

  COMPONENT_TEST('get')
  valSNKa2(1,1)=0_SNK
  CALL testParam%get('testSNK1',valSNKa2)
  ASSERT_EQ(valSNKa2(1,1),5_SNK,'%get')
  valSNKa2(1,1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa2)
  ASSERT_EQ(valSNKa2(1,1),5_SNK,'%get')
  valSNKa2(1,1)=0_SNK
  CALL testParam%get('testSNK3',valSNKa2)
  ASSERT_EQ(valSNKa2(1,1),5_SNK,'%get')
  valSNKa2(1,1)=0_SNK
  CALL testParam%get('group - > testSNK1',valSNKa2)
  ASSERT_EQ(valSNKa2(1,1),5_SNK,'%get')
  valSNKa2(1,1)=0_SNK
  CALL testParam%get('group - > group 2 -> testSNK1',valSNKa2)
  ASSERT_EQ(valSNKa2(1,1),5_SNK,'%get')
  valSNKa2(1,1)=0_SNK
  CALL testParam%get('testSNK4',valSNKa2)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSNK4" of type "2-D ARRAY INTEGER(SNK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSNKa2(1,1))
  valSNKa2(1,1)=0_SNK
  ALLOCATE(refSNKa2(1,1))
  refSNKa2=5_SNK
  CALL testParam%get('testSNK4',valSNKa2,DEFAULT=refSNKa2)
  ASSERT_EQ(valSNKa2(1,1),5_SNK,'%get, default')

  COMPONENT_TEST('set')
  refSNKa2=8_SNK
  CALL testParam%set('testSNK2',refSNKa2)
  valSNKa2(1,1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa2)
  ASSERT_EQ(valSNKa2(1,1),8_SNK,'%set')
  CALL testParam%set('testSNK4',refSNKa2)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSNK4" of type "2-D ARRAY INTEGER(SNK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSNKa2)
  DEALLOCATE(valSNKa2)

ENDSUBROUTINE testSNKa2
!
!-------------------------------------------------------------------------------
SUBROUTINE testSNKa3()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSNKa3(1,1,1))
  valSNKa3=5_SNK

  COMPONENT_TEST('add')
  CALL testParam%add('testSNK1',valSNKa3)
  CALL testParam%add('testSNK2',valSNKa3)
  CALL testParam%add('testSNK3',valSNKa3)
  CALL testParam%add('group - > testSNK1',valSNKa3)
  CALL testParam%add('group - > group 2 -> testSNK1',valSNKa3)

  COMPONENT_TEST('get')
  valSNKa3(1,1,1)=0_SNK
  CALL testParam%get('testSNK1',valSNKa3)
  ASSERT_EQ(valSNKa3(1,1,1),5_SNK,'%get')
  valSNKa3(1,1,1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa3)
  ASSERT_EQ(valSNKa3(1,1,1),5_SNK,'%get')
  valSNKa3(1,1,1)=0_SNK
  CALL testParam%get('testSNK3',valSNKa3)
  ASSERT_EQ(valSNKa3(1,1,1),5_SNK,'%get')
  valSNKa3(1,1,1)=0_SNK
  CALL testParam%get('group - > testSNK1',valSNKa3)
  ASSERT_EQ(valSNKa3(1,1,1),5_SNK,'%get')
  valSNKa3(1,1,1)=0_SNK
  CALL testParam%get('group - > group 2 -> testSNK1',valSNKa3)
  ASSERT_EQ(valSNKa3(1,1,1),5_SNK,'%get')
  valSNKa3(1,1,1)=0_SNK
  CALL testParam%get('testSNK4',valSNKa3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSNK4" of type "3-D ARRAY INTEGER(SNK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSNKa3(1,1,1))
  valSNKa3(1,1,1)=0_SNK
  ALLOCATE(refSNKa3(1,1,1))
  refSNKa3=5_SNK
  CALL testParam%get('testSNK4',valSNKa3,DEFAULT=refSNKa3)
  ASSERT_EQ(valSNKa3(1,1,1),5_SNK,'%get, default')

  COMPONENT_TEST('set')
  refSNKa3=8_SNK
  CALL testParam%set('testSNK2',refSNKa3)
  valSNKa3(1,1,1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa3)
  ASSERT_EQ(valSNKa3(1,1,1),8_SNK,'%set')
  CALL testParam%set('testSNK4',refSNKa3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSNK4" of type "3-D ARRAY INTEGER(SNK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSNKa3)
  DEALLOCATE(valSNKa3)

ENDSUBROUTINE testSNKa3
!
!-------------------------------------------------------------------------------
SUBROUTINE testSNKa4()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSNKa4(1,1,1,1))
  valSNKa4=5_SNK

  COMPONENT_TEST('add')
  CALL testParam%add('testSNK1',valSNKa4)
  CALL testParam%add('testSNK2',valSNKa4)
  CALL testParam%add('testSNK3',valSNKa4)
  CALL testParam%add('group - > testSNK1',valSNKa4)
  CALL testParam%add('group - > group 2 -> testSNK1',valSNKa4)

  COMPONENT_TEST('get')
  valSNKa4(1,1,1,1)=0_SNK
  CALL testParam%get('testSNK1',valSNKa4)
  ASSERT_EQ(valSNKa4(1,1,1,1),5_SNK,'%get')
  valSNKa4(1,1,1,1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa4)
  ASSERT_EQ(valSNKa4(1,1,1,1),5_SNK,'%get')
  valSNKa4(1,1,1,1)=0_SNK
  CALL testParam%get('testSNK3',valSNKa4)
  ASSERT_EQ(valSNKa4(1,1,1,1),5_SNK,'%get')
  valSNKa4(1,1,1,1)=0_SNK
  CALL testParam%get('group - > testSNK1',valSNKa4)
  ASSERT_EQ(valSNKa4(1,1,1,1),5_SNK,'%get')
  valSNKa4(1,1,1,1)=0_SNK
  CALL testParam%get('group - > group 2 -> testSNK1',valSNKa4)
  ASSERT_EQ(valSNKa4(1,1,1,1),5_SNK,'%get')
  valSNKa4(1,1,1,1)=0_SNK
  CALL testParam%get('testSNK4',valSNKa4)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSNK4" of type "4-D ARRAY INTEGER(SNK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSNKa4(1,1,1,1))
  valSNKa4(1,1,1,1)=0_SNK
  ALLOCATE(refSNKa4(1,1,1,1))
  refSNKa4=5_SNK
  CALL testParam%get('testSNK4',valSNKa4,DEFAULT=refSNKa4)
  ASSERT_EQ(valSNKa4(1,1,1,1),5_SNK,'%get, default')

  COMPONENT_TEST('set')
  refSNKa4=8_SNK
  CALL testParam%set('testSNK2',refSNKa4)
  valSNKa4(1,1,1,1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa4)
  ASSERT_EQ(valSNKa4(1,1,1,1),8_SNK,'%set')
  CALL testParam%set('testSNK4',refSNKa4)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSNK4" of type "4-D ARRAY INTEGER(SNK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSNKa4)
  DEALLOCATE(valSNKa4)

ENDSUBROUTINE testSNKa4
!
!-------------------------------------------------------------------------------
SUBROUTINE testSNKa5()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSNKa5(1,1,1,1,1))
  valSNKa5=5_SNK

  COMPONENT_TEST('add')
  CALL testParam%add('testSNK1',valSNKa5)
  CALL testParam%add('testSNK2',valSNKa5)
  CALL testParam%add('testSNK3',valSNKa5)
  CALL testParam%add('group - > testSNK1',valSNKa5)
  CALL testParam%add('group - > group 2 -> testSNK1',valSNKa5)

  COMPONENT_TEST('get')
  valSNKa5(1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK1',valSNKa5)
  ASSERT_EQ(valSNKa5(1,1,1,1,1),5_SNK,'%get')
  valSNKa5(1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa5)
  ASSERT_EQ(valSNKa5(1,1,1,1,1),5_SNK,'%get')
  valSNKa5(1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK3',valSNKa5)
  ASSERT_EQ(valSNKa5(1,1,1,1,1),5_SNK,'%get')
  valSNKa5(1,1,1,1,1)=0_SNK
  CALL testParam%get('group - > testSNK1',valSNKa5)
  ASSERT_EQ(valSNKa5(1,1,1,1,1),5_SNK,'%get')
  valSNKa5(1,1,1,1,1)=0_SNK
  CALL testParam%get('group - > group 2 -> testSNK1',valSNKa5)
  ASSERT_EQ(valSNKa5(1,1,1,1,1),5_SNK,'%get')
  valSNKa5(1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK4',valSNKa5)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSNK4" of type "5-D ARRAY INTEGER(SNK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSNKa5(1,1,1,1,1))
  valSNKa5(1,1,1,1,1)=0_SNK
  ALLOCATE(refSNKa5(1,1,1,1,1))
  refSNKa5=5_SNK
  CALL testParam%get('testSNK4',valSNKa5,DEFAULT=refSNKa5)
  ASSERT_EQ(valSNKa5(1,1,1,1,1),5_SNK,'%get, default')

  COMPONENT_TEST('set')
  refSNKa5=8_SNK
  CALL testParam%set('testSNK2',refSNKa5)
  valSNKa5(1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa5)
  ASSERT_EQ(valSNKa5(1,1,1,1,1),8_SNK,'%set')
  CALL testParam%set('testSNK4',refSNKa5)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSNK4" of type "5-D ARRAY INTEGER(SNK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSNKa5)
  DEALLOCATE(valSNKa5)

ENDSUBROUTINE testSNKa5
!
!-------------------------------------------------------------------------------
SUBROUTINE testSNKa6()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSNKa6(1,1,1,1,1,1))
  valSNKa6=5_SNK

  COMPONENT_TEST('add')
  CALL testParam%add('testSNK1',valSNKa6)
  CALL testParam%add('testSNK2',valSNKa6)
  CALL testParam%add('testSNK3',valSNKa6)
  CALL testParam%add('group - > testSNK1',valSNKa6)
  CALL testParam%add('group - > group 2 -> testSNK1',valSNKa6)

  COMPONENT_TEST('get')
  valSNKa6(1,1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK1',valSNKa6)
  ASSERT_EQ(valSNKa6(1,1,1,1,1,1),5_SNK,'%get')
  valSNKa6(1,1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa6)
  ASSERT_EQ(valSNKa6(1,1,1,1,1,1),5_SNK,'%get')
  valSNKa6(1,1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK3',valSNKa6)
  ASSERT_EQ(valSNKa6(1,1,1,1,1,1),5_SNK,'%get')
  valSNKa6(1,1,1,1,1,1)=0_SNK
  CALL testParam%get('group - > testSNK1',valSNKa6)
  ASSERT_EQ(valSNKa6(1,1,1,1,1,1),5_SNK,'%get')
  valSNKa6(1,1,1,1,1,1)=0_SNK
  CALL testParam%get('group - > group 2 -> testSNK1',valSNKa6)
  ASSERT_EQ(valSNKa6(1,1,1,1,1,1),5_SNK,'%get')
  valSNKa6(1,1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK4',valSNKa6)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSNK4" of type "6-D ARRAY INTEGER(SNK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSNKa6(1,1,1,1,1,1))
  valSNKa6(1,1,1,1,1,1)=0_SNK
  ALLOCATE(refSNKa6(1,1,1,1,1,1))
  refSNKa6=5_SNK
  CALL testParam%get('testSNK4',valSNKa6,DEFAULT=refSNKa6)
  ASSERT_EQ(valSNKa6(1,1,1,1,1,1),5_SNK,'%get, default')

  COMPONENT_TEST('set')
  refSNKa6=8_SNK
  CALL testParam%set('testSNK2',refSNKa6)
  valSNKa6(1,1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa6)
  ASSERT_EQ(valSNKa6(1,1,1,1,1,1),8_SNK,'%set')
  CALL testParam%set('testSNK4',refSNKa6)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSNK4" of type "6-D ARRAY INTEGER(SNK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSNKa6)
  DEALLOCATE(valSNKa6)

ENDSUBROUTINE testSNKa6
!
!-------------------------------------------------------------------------------
SUBROUTINE testSNKa7()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSNKa7(1,1,1,1,1,1,1))
  valSNKa7=5_SNK

  COMPONENT_TEST('add')
  CALL testParam%add('testSNK1',valSNKa7)
  CALL testParam%add('testSNK2',valSNKa7)
  CALL testParam%add('testSNK3',valSNKa7)
  CALL testParam%add('group - > testSNK1',valSNKa7)
  CALL testParam%add('group - > group 2 -> testSNK1',valSNKa7)

  COMPONENT_TEST('get')
  valSNKa7(1,1,1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK1',valSNKa7)
  ASSERT_EQ(valSNKa7(1,1,1,1,1,1,1),5_SNK,'%get')
  valSNKa7(1,1,1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa7)
  ASSERT_EQ(valSNKa7(1,1,1,1,1,1,1),5_SNK,'%get')
  valSNKa7(1,1,1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK3',valSNKa7)
  ASSERT_EQ(valSNKa7(1,1,1,1,1,1,1),5_SNK,'%get')
  valSNKa7(1,1,1,1,1,1,1)=0_SNK
  CALL testParam%get('group - > testSNK1',valSNKa7)
  ASSERT_EQ(valSNKa7(1,1,1,1,1,1,1),5_SNK,'%get')
  valSNKa7(1,1,1,1,1,1,1)=0_SNK
  CALL testParam%get('group - > group 2 -> testSNK1',valSNKa7)
  ASSERT_EQ(valSNKa7(1,1,1,1,1,1,1),5_SNK,'%get')
  valSNKa7(1,1,1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK4',valSNKa7)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSNK4" of type "7-D ARRAY INTEGER(SNK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSNKa7(1,1,1,1,1,1,1))
  valSNKa7(1,1,1,1,1,1,1)=0_SNK
  ALLOCATE(refSNKa7(1,1,1,1,1,1,1))
  refSNKa7=5_SNK
  CALL testParam%get('testSNK4',valSNKa7,DEFAULT=refSNKa7)
  ASSERT_EQ(valSNKa7(1,1,1,1,1,1,1),5_SNK,'%get, default')

  COMPONENT_TEST('set')
  refSNKa7=8_SNK
  CALL testParam%set('testSNK2',refSNKa7)
  valSNKa7(1,1,1,1,1,1,1)=0_SNK
  CALL testParam%get('testSNK2',valSNKa7)
  ASSERT_EQ(valSNKa7(1,1,1,1,1,1,1),8_SNK,'%set')
  CALL testParam%set('testSNK4',refSNKa7)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSNK4" of type "7-D ARRAY INTEGER(SNK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSNKa7)
  DEALLOCATE(valSNKa7)

ENDSUBROUTINE testSNKa7
!
!-------------------------------------------------------------------------------
SUBROUTINE testSLK()
  TYPE(StringType) :: msg,refmsg
  valslk=5_SLK

  COMPONENT_TEST('add')
  CALL testParam%add('testSLK1',valSLK)
  CALL testParam%add('testSLK2',valSLK)
  CALL testParam%add('testSLK3',valSLK)
  CALL testParam%add('group - > testSLK1',valSLK)
  CALL testParam%add('group - > group 2 -> testSLK1',valSLK)

  COMPONENT_TEST('get')
  valSLK=0_SLK
  CALL testParam%get('testSLK1',valSLK)
  ASSERT_EQ(valSLK,5_SLK,'%get')
  valSLK=0_SLK
  CALL testParam%get('testSLK2',valSLK)
  ASSERT_EQ(valSLK,5_SLK,'%get')
  valSLK=0_SLK
  CALL testParam%get('testSLK3',valSLK)
  ASSERT_EQ(valSLK,5_SLK,'%get')
  valSLK=0_SLK
  CALL testParam%get('group - > testSLK1',valSLK)
  ASSERT_EQ(valSLK,5_SLK,'%get')
  valSLK=0_SLK
  CALL testParam%get('group - > group 2 -> testSLK1',valSLK)
  ASSERT_EQ(valSLK,5_SLK,'%get')
  valSLK=0_SLK
  CALL testParam%get('testSLK4',valSLK)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSLK4" of type "INTEGER(SLK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  valSLK=0_SLK
  CALL testParam%get('testSLK4',valSLK,DEFAULT=5_SLK)
  ASSERT_EQ(valSLK,5_SLK,'%get, default')

  COMPONENT_TEST('set')
  CALL testParam%set('testSLK2',8_SLK)
  valSLK=0_SLK
  CALL testParam%get('testSLK2',valSLK)
  ASSERT_EQ(valSLK,8_SLK,'%set')
  CALL testParam%set('testSLK4',8_SLK)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSLK4" of type "INTEGER(SLK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()

ENDSUBROUTINE testSLK
!
!-------------------------------------------------------------------------------
SUBROUTINE testSLKa1()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSLKa1(1))
  valSLKa1=5_SLK

  COMPONENT_TEST('add')
  CALL testParam%add('testSLK1',valSLKa1)
  CALL testParam%add('testSLK2',valSLKa1)
  CALL testParam%add('testSLK3',valSLKa1)
  CALL testParam%add('group - > testSLK1',valSLKa1)
  CALL testParam%add('group - > group 2 -> testSLK1',valSLKa1)

  COMPONENT_TEST('get')
  valSLKa1(1)=0_SLK
  CALL testParam%get('testSLK1',valSLKa1)
  ASSERT_EQ(valSLKa1(1),5_SLK,'%get')
  valSLKa1(1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa1)
  ASSERT_EQ(valSLKa1(1),5_SLK,'%get')
  valSLKa1(1)=0_SLK
  CALL testParam%get('testSLK3',valSLKa1)
  ASSERT_EQ(valSLKa1(1),5_SLK,'%get')
  valSLKa1(1)=0_SLK
  CALL testParam%get('group - > testSLK1',valSLKa1)
  ASSERT_EQ(valSLKa1(1),5_SLK,'%get')
  valSLKa1(1)=0_SLK
  CALL testParam%get('group - > group 2 -> testSLK1',valSLKa1)
  ASSERT_EQ(valSLKa1(1),5_SLK,'%get')
  valSLKa1(1)=0_SLK
  CALL testParam%get('testSLK4',valSLKa1)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSLK4" of type "1-D ARRAY INTEGER(SLK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSLKa1(1))
  valSLKa1(1)=0_SLK
  ALLOCATE(refSLKa1(1))
  refSLKa1=5_SLK
  CALL testParam%get('testSLK4',valSLKa1,DEFAULT=refSLKa1)
  ASSERT_EQ(valSLKa1(1),5_SLK,'%get, default')

  COMPONENT_TEST('set')
  refSLKa1=8_SLK
  CALL testParam%set('testSLK2',refSLKa1)
  valSLKa1(1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa1)
  ASSERT_EQ(valSLKa1(1),8_SLK,'%set')
  CALL testParam%set('testSLK4',refSLKa1)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSLK4" of type "1-D ARRAY INTEGER(SLK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSLKa1)
  DEALLOCATE(valSLKa1)

ENDSUBROUTINE testSLKa1
!
!-------------------------------------------------------------------------------
SUBROUTINE testSLKa2()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSLKa2(1,1))
  valSLKa2=5_SLK

  COMPONENT_TEST('add')
  CALL testParam%add('testSLK1',valSLKa2)
  CALL testParam%add('testSLK2',valSLKa2)
  CALL testParam%add('testSLK3',valSLKa2)
  CALL testParam%add('group - > testSLK1',valSLKa2)
  CALL testParam%add('group - > group 2 -> testSLK1',valSLKa2)

  COMPONENT_TEST('get')
  valSLKa2(1,1)=0_SLK
  CALL testParam%get('testSLK1',valSLKa2)
  ASSERT_EQ(valSLKa2(1,1),5_SLK,'%get')
  valSLKa2(1,1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa2)
  ASSERT_EQ(valSLKa2(1,1),5_SLK,'%get')
  valSLKa2(1,1)=0_SLK
  CALL testParam%get('testSLK3',valSLKa2)
  ASSERT_EQ(valSLKa2(1,1),5_SLK,'%get')
  valSLKa2(1,1)=0_SLK
  CALL testParam%get('group - > testSLK1',valSLKa2)
  ASSERT_EQ(valSLKa2(1,1),5_SLK,'%get')
  valSLKa2(1,1)=0_SLK
  CALL testParam%get('group - > group 2 -> testSLK1',valSLKa2)
  ASSERT_EQ(valSLKa2(1,1),5_SLK,'%get')
  valSLKa2(1,1)=0_SLK
  CALL testParam%get('testSLK4',valSLKa2)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSLK4" of type "2-D ARRAY INTEGER(SLK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSLKa2(1,1))
  valSLKa2(1,1)=0_SLK
  ALLOCATE(refSLKa2(1,1))
  refSLKa2=5_SLK
  CALL testParam%get('testSLK4',valSLKa2,DEFAULT=refSLKa2)
  ASSERT_EQ(valSLKa2(1,1),5_SLK,'%get, default')

  COMPONENT_TEST('set')
  refSLKa2=8_SLK
  CALL testParam%set('testSLK2',refSLKa2)
  valSLKa2(1,1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa2)
  ASSERT_EQ(valSLKa2(1,1),8_SLK,'%set')
  CALL testParam%set('testSLK4',refSLKa2)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSLK4" of type "2-D ARRAY INTEGER(SLK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSLKa2)
  DEALLOCATE(valSLKa2)

ENDSUBROUTINE testSLKa2
!
!-------------------------------------------------------------------------------
SUBROUTINE testSLKa3()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSLKa3(1,1,1))
  valSLKa3=5_SLK

  COMPONENT_TEST('add')
  CALL testParam%add('testSLK1',valSLKa3)
  CALL testParam%add('testSLK2',valSLKa3)
  CALL testParam%add('testSLK3',valSLKa3)
  CALL testParam%add('group - > testSLK1',valSLKa3)
  CALL testParam%add('group - > group 2 -> testSLK1',valSLKa3)

  COMPONENT_TEST('get')
  valSLKa3(1,1,1)=0_SLK
  CALL testParam%get('testSLK1',valSLKa3)
  ASSERT_EQ(valSLKa3(1,1,1),5_SLK,'%get')
  valSLKa3(1,1,1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa3)
  ASSERT_EQ(valSLKa3(1,1,1),5_SLK,'%get')
  valSLKa3(1,1,1)=0_SLK
  CALL testParam%get('testSLK3',valSLKa3)
  ASSERT_EQ(valSLKa3(1,1,1),5_SLK,'%get')
  valSLKa3(1,1,1)=0_SLK
  CALL testParam%get('group - > testSLK1',valSLKa3)
  ASSERT_EQ(valSLKa3(1,1,1),5_SLK,'%get')
  valSLKa3(1,1,1)=0_SLK
  CALL testParam%get('group - > group 2 -> testSLK1',valSLKa3)
  ASSERT_EQ(valSLKa3(1,1,1),5_SLK,'%get')
  valSLKa3(1,1,1)=0_SLK
  CALL testParam%get('testSLK4',valSLKa3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSLK4" of type "3-D ARRAY INTEGER(SLK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSLKa3(1,1,1))
  valSLKa3(1,1,1)=0_SLK
  ALLOCATE(refSLKa3(1,1,1))
  refSLKa3=5_SLK
  CALL testParam%get('testSLK4',valSLKa3,DEFAULT=refSLKa3)
  ASSERT_EQ(valSLKa3(1,1,1),5_SLK,'%get, default')

  COMPONENT_TEST('set')
  refSLKa3=8_SLK
  CALL testParam%set('testSLK2',refSLKa3)
  valSLKa3(1,1,1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa3)
  ASSERT_EQ(valSLKa3(1,1,1),8_SLK,'%set')
  CALL testParam%set('testSLK4',refSLKa3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSLK4" of type "3-D ARRAY INTEGER(SLK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSLKa3)
  DEALLOCATE(valSLKa3)

ENDSUBROUTINE testSLKa3
!
!-------------------------------------------------------------------------------
SUBROUTINE testSLKa4()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSLKa4(1,1,1,1))
  valSLKa4=5_SLK

  COMPONENT_TEST('add')
  CALL testParam%add('testSLK1',valSLKa4)
  CALL testParam%add('testSLK2',valSLKa4)
  CALL testParam%add('testSLK3',valSLKa4)
  CALL testParam%add('group - > testSLK1',valSLKa4)
  CALL testParam%add('group - > group 2 -> testSLK1',valSLKa4)

  COMPONENT_TEST('get')
  valSLKa4(1,1,1,1)=0_SLK
  CALL testParam%get('testSLK1',valSLKa4)
  ASSERT_EQ(valSLKa4(1,1,1,1),5_SLK,'%get')
  valSLKa4(1,1,1,1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa4)
  ASSERT_EQ(valSLKa4(1,1,1,1),5_SLK,'%get')
  valSLKa4(1,1,1,1)=0_SLK
  CALL testParam%get('testSLK3',valSLKa4)
  ASSERT_EQ(valSLKa4(1,1,1,1),5_SLK,'%get')
  valSLKa4(1,1,1,1)=0_SLK
  CALL testParam%get('group - > testSLK1',valSLKa4)
  ASSERT_EQ(valSLKa4(1,1,1,1),5_SLK,'%get')
  valSLKa4(1,1,1,1)=0_SLK
  CALL testParam%get('group - > group 2 -> testSLK1',valSLKa4)
  ASSERT_EQ(valSLKa4(1,1,1,1),5_SLK,'%get')
  valSLKa4(1,1,1,1)=0_SLK
  CALL testParam%get('testSLK4',valSLKa4)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSLK4" of type "4-D ARRAY INTEGER(SLK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSLKa4(1,1,1,1))
  valSLKa4(1,1,1,1)=0_SLK
  ALLOCATE(refSLKa4(1,1,1,1))
  refSLKa4=5_SLK
  CALL testParam%get('testSLK4',valSLKa4,DEFAULT=refSLKa4)
  ASSERT_EQ(valSLKa4(1,1,1,1),5_SLK,'%get, default')

  COMPONENT_TEST('set')
  refSLKa4=8_SLK
  CALL testParam%set('testSLK2',refSLKa4)
  valSLKa4(1,1,1,1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa4)
  ASSERT_EQ(valSLKa4(1,1,1,1),8_SLK,'%set')
  CALL testParam%set('testSLK4',refSLKa4)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSLK4" of type "4-D ARRAY INTEGER(SLK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSLKa4)
  DEALLOCATE(valSLKa4)

ENDSUBROUTINE testSLKa4
!
!-------------------------------------------------------------------------------
SUBROUTINE testSLKa5()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSLKa5(1,1,1,1,1))
  valSLKa5=5_SLK

  COMPONENT_TEST('add')
  CALL testParam%add('testSLK1',valSLKa5)
  CALL testParam%add('testSLK2',valSLKa5)
  CALL testParam%add('testSLK3',valSLKa5)
  CALL testParam%add('group - > testSLK1',valSLKa5)
  CALL testParam%add('group - > group 2 -> testSLK1',valSLKa5)

  COMPONENT_TEST('get')
  valSLKa5(1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK1',valSLKa5)
  ASSERT_EQ(valSLKa5(1,1,1,1,1),5_SLK,'%get')
  valSLKa5(1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa5)
  ASSERT_EQ(valSLKa5(1,1,1,1,1),5_SLK,'%get')
  valSLKa5(1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK3',valSLKa5)
  ASSERT_EQ(valSLKa5(1,1,1,1,1),5_SLK,'%get')
  valSLKa5(1,1,1,1,1)=0_SLK
  CALL testParam%get('group - > testSLK1',valSLKa5)
  ASSERT_EQ(valSLKa5(1,1,1,1,1),5_SLK,'%get')
  valSLKa5(1,1,1,1,1)=0_SLK
  CALL testParam%get('group - > group 2 -> testSLK1',valSLKa5)
  ASSERT_EQ(valSLKa5(1,1,1,1,1),5_SLK,'%get')
  valSLKa5(1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK4',valSLKa5)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSLK4" of type "5-D ARRAY INTEGER(SLK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSLKa5(1,1,1,1,1))
  valSLKa5(1,1,1,1,1)=0_SLK
  ALLOCATE(refSLKa5(1,1,1,1,1))
  refSLKa5=5_SLK
  CALL testParam%get('testSLK4',valSLKa5,DEFAULT=refSLKa5)
  ASSERT_EQ(valSLKa5(1,1,1,1,1),5_SLK,'%get, default')

  COMPONENT_TEST('set')
  refSLKa5=8_SLK
  CALL testParam%set('testSLK2',refSLKa5)
  valSLKa5(1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa5)
  ASSERT_EQ(valSLKa5(1,1,1,1,1),8_SLK,'%set')
  CALL testParam%set('testSLK4',refSLKa5)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSLK4" of type "5-D ARRAY INTEGER(SLK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSLKa5)
  DEALLOCATE(valSLKa5)

ENDSUBROUTINE testSLKa5
!
!-------------------------------------------------------------------------------
SUBROUTINE testSLKa6()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSLKa6(1,1,1,1,1,1))
  valSLKa6=5_SLK

  COMPONENT_TEST('add')
  CALL testParam%add('testSLK1',valSLKa6)
  CALL testParam%add('testSLK2',valSLKa6)
  CALL testParam%add('testSLK3',valSLKa6)
  CALL testParam%add('group - > testSLK1',valSLKa6)
  CALL testParam%add('group - > group 2 -> testSLK1',valSLKa6)

  COMPONENT_TEST('get')
  valSLKa6(1,1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK1',valSLKa6)
  ASSERT_EQ(valSLKa6(1,1,1,1,1,1),5_SLK,'%get')
  valSLKa6(1,1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa6)
  ASSERT_EQ(valSLKa6(1,1,1,1,1,1),5_SLK,'%get')
  valSLKa6(1,1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK3',valSLKa6)
  ASSERT_EQ(valSLKa6(1,1,1,1,1,1),5_SLK,'%get')
  valSLKa6(1,1,1,1,1,1)=0_SLK
  CALL testParam%get('group - > testSLK1',valSLKa6)
  ASSERT_EQ(valSLKa6(1,1,1,1,1,1),5_SLK,'%get')
  valSLKa6(1,1,1,1,1,1)=0_SLK
  CALL testParam%get('group - > group 2 -> testSLK1',valSLKa6)
  ASSERT_EQ(valSLKa6(1,1,1,1,1,1),5_SLK,'%get')
  valSLKa6(1,1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK4',valSLKa6)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSLK4" of type "6-D ARRAY INTEGER(SLK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSLKa6(1,1,1,1,1,1))
  valSLKa6(1,1,1,1,1,1)=0_SLK
  ALLOCATE(refSLKa6(1,1,1,1,1,1))
  refSLKa6=5_SLK
  CALL testParam%get('testSLK4',valSLKa6,DEFAULT=refSLKa6)
  ASSERT_EQ(valSLKa6(1,1,1,1,1,1),5_SLK,'%get, default')

  COMPONENT_TEST('set')
  refSLKa6=8_SLK
  CALL testParam%set('testSLK2',refSLKa6)
  valSLKa6(1,1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa6)
  ASSERT_EQ(valSLKa6(1,1,1,1,1,1),8_SLK,'%set')
  CALL testParam%set('testSLK4',refSLKa6)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSLK4" of type "6-D ARRAY INTEGER(SLK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSLKa6)
  DEALLOCATE(valSLKa6)

ENDSUBROUTINE testSLKa6
!
!-------------------------------------------------------------------------------
SUBROUTINE testSLKa7()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSLKa7(1,1,1,1,1,1,1))
  valSLKa7=5_SLK

  COMPONENT_TEST('add')
  CALL testParam%add('testSLK1',valSLKa7)
  CALL testParam%add('testSLK2',valSLKa7)
  CALL testParam%add('testSLK3',valSLKa7)
  CALL testParam%add('group - > testSLK1',valSLKa7)
  CALL testParam%add('group - > group 2 -> testSLK1',valSLKa7)

  COMPONENT_TEST('get')
  valSLKa7(1,1,1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK1',valSLKa7)
  ASSERT_EQ(valSLKa7(1,1,1,1,1,1,1),5_SLK,'%get')
  valSLKa7(1,1,1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa7)
  ASSERT_EQ(valSLKa7(1,1,1,1,1,1,1),5_SLK,'%get')
  valSLKa7(1,1,1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK3',valSLKa7)
  ASSERT_EQ(valSLKa7(1,1,1,1,1,1,1),5_SLK,'%get')
  valSLKa7(1,1,1,1,1,1,1)=0_SLK
  CALL testParam%get('group - > testSLK1',valSLKa7)
  ASSERT_EQ(valSLKa7(1,1,1,1,1,1,1),5_SLK,'%get')
  valSLKa7(1,1,1,1,1,1,1)=0_SLK
  CALL testParam%get('group - > group 2 -> testSLK1',valSLKa7)
  ASSERT_EQ(valSLKa7(1,1,1,1,1,1,1),5_SLK,'%get')
  valSLKa7(1,1,1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK4',valSLKa7)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSLK4" of type "7-D ARRAY INTEGER(SLK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSLKa7(1,1,1,1,1,1,1))
  valSLKa7(1,1,1,1,1,1,1)=0_SLK
  ALLOCATE(refSLKa7(1,1,1,1,1,1,1))
  refSLKa7=5_SLK
  CALL testParam%get('testSLK4',valSLKa7,DEFAULT=refSLKa7)
  ASSERT_EQ(valSLKa7(1,1,1,1,1,1,1),5_SLK,'%get, default')

  COMPONENT_TEST('set')
  refSLKa7=8_SLK
  CALL testParam%set('testSLK2',refSLKa7)
  valSLKa7(1,1,1,1,1,1,1)=0_SLK
  CALL testParam%get('testSLK2',valSLKa7)
  ASSERT_EQ(valSLKa7(1,1,1,1,1,1,1),8_SLK,'%set')
  CALL testParam%set('testSLK4',refSLKa7)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSLK4" of type "7-D ARRAY INTEGER(SLK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSLKa7)
  DEALLOCATE(valSLKa7)

ENDSUBROUTINE testSLKa7
!
!-------------------------------------------------------------------------------
SUBROUTINE testSSK()
  TYPE(StringType) :: msg,refmsg
  valSSK=5.0_SSK

  COMPONENT_TEST('add')
  CALL testParam%add('testSSK1',valSSK)
  CALL testParam%add('testSSK2',valSSK)
  CALL testParam%add('testSSK3',valSSK)
  CALL testParam%add('group - > testSSK1',valSSK)
  CALL testParam%add('group - > group 2 -> testSSK1',valSSK)

  COMPONENT_TEST('get')
  valSSK=0.0_SSK
  CALL testParam%get('testSSK1',valSSK)
  ASSERT_EQ(valSSK,5.0_SSK,'%get')
  valSSK=0.0_SSK
  CALL testParam%get('testSSK2',valSSK)
  ASSERT_EQ(valSSK,5.0_SSK,'%get')
  valSSK=0.0_SSK
  CALL testParam%get('testSSK3',valSSK)
  ASSERT_EQ(valSSK,5.0_SSK,'%get')
  valSSK=0.0_SSK
  CALL testParam%get('group - > testSSK1',valSSK)
  ASSERT_EQ(valSSK,5.0_SSK,'%get')
  valSSK=0.0_SSK
  CALL testParam%get('group - > group 2 -> testSSK1',valSSK)
  ASSERT_EQ(valSSK,5.0_SSK,'%get')
  valSSK=0.0_SSK
  CALL testParam%get('testSSK4',valSSK)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSSK4" of type "REAL(SSK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  valSSK=0.0_SSK
  CALL testParam%get('testSSK4',valSSK,DEFAULT=5.0_SSK)
  ASSERT_EQ(valSSK,5.0_SSK,'%get, default')

  COMPONENT_TEST('set')
  CALL testParam%set('testSSK2',8.0_SSK)
  valSSK=0.0_SSK
  CALL testParam%get('testSSK2',valSSK)
  ASSERT_EQ(valSSK,8.0_SSK,'%set')
  CALL testParam%set('testSSK4',8.0_SSK)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSSK4" of type "REAL(SSK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()

ENDSUBROUTINE testSSK
!
!-------------------------------------------------------------------------------
SUBROUTINE testSSKa1()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSSKa1(1))
  valSSKa1=5.0_SSK

  COMPONENT_TEST('add')
  CALL testParam%add('testSSK1',valSSKa1)
  CALL testParam%add('testSSK2',valSSKa1)
  CALL testParam%add('testSSK3',valSSKa1)
  CALL testParam%add('group - > testSSK1',valSSKa1)
  CALL testParam%add('group - > group 2 -> testSSK1',valSSKa1)

  COMPONENT_TEST('get')
  valSSKa1(1)=0.0_SSK
  CALL testParam%get('testSSK1',valSSKa1)
  ASSERT_EQ(valSSKa1(1),5.0_SSK,'%get')
  valSSKa1(1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa1)
  ASSERT_EQ(valSSKa1(1),5.0_SSK,'%get')
  valSSKa1(1)=0.0_SSK
  CALL testParam%get('testSSK3',valSSKa1)
  ASSERT_EQ(valSSKa1(1),5.0_SSK,'%get')
  valSSKa1(1)=0.0_SSK
  CALL testParam%get('group - > testSSK1',valSSKa1)
  ASSERT_EQ(valSSKa1(1),5.0_SSK,'%get')
  valSSKa1(1)=0.0_SSK
  CALL testParam%get('group - > group 2 -> testSSK1',valSSKa1)
  ASSERT_EQ(valSSKa1(1),5.0_SSK,'%get')
  valSSKa1(1)=0.0_SSK
  CALL testParam%get('testSSK4',valSSKa1)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSSK4" of type "1-D ARRAY REAL(SSK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSSKa1(1))
  valSSKa1(1)=0.0_SSK
  ALLOCATE(refSSKa1(1))
  refSSKa1=5.0_SSK
  CALL testParam%get('testSSK4',valSSKa1,DEFAULT=refSSKa1)
  ASSERT_EQ(valSSKa1(1),5.0_SSK,'%get, default')

  COMPONENT_TEST('set')
  refSSKa1=8.0_SSK
  CALL testParam%set('testSSK2',refSSKa1)
  valSSKa1(1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa1)
  ASSERT_EQ(valSSKa1(1),8.0_SSK,'%set')
  CALL testParam%set('testSSK4',refSSKa1)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSSK4" of type "1-D ARRAY REAL(SSK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSSKa1)
  DEALLOCATE(valSSKa1)

ENDSUBROUTINE testSSKa1
!
!-------------------------------------------------------------------------------
SUBROUTINE testSSKa2()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSSKa2(1,1))
  valSSKa2=5.0_SSK

  COMPONENT_TEST('add')
  CALL testParam%add('testSSK1',valSSKa2)
  CALL testParam%add('testSSK2',valSSKa2)
  CALL testParam%add('testSSK3',valSSKa2)
  CALL testParam%add('group - > testSSK1',valSSKa2)
  CALL testParam%add('group - > group 2 -> testSSK1',valSSKa2)

  COMPONENT_TEST('get')
  valSSKa2(1,1)=0.0_SSK
  CALL testParam%get('testSSK1',valSSKa2)
  ASSERT_EQ(valSSKa2(1,1),5.0_SSK,'%get')
  valSSKa2(1,1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa2)
  ASSERT_EQ(valSSKa2(1,1),5.0_SSK,'%get')
  valSSKa2(1,1)=0.0_SSK
  CALL testParam%get('testSSK3',valSSKa2)
  ASSERT_EQ(valSSKa2(1,1),5.0_SSK,'%get')
  valSSKa2(1,1)=0.0_SSK
  CALL testParam%get('group - > testSSK1',valSSKa2)
  ASSERT_EQ(valSSKa2(1,1),5.0_SSK,'%get')
  valSSKa2(1,1)=0.0_SSK
  CALL testParam%get('group - > group 2 -> testSSK1',valSSKa2)
  ASSERT_EQ(valSSKa2(1,1),5.0_SSK,'%get')
  valSSKa2(1,1)=0.0_SSK
  CALL testParam%get('testSSK4',valSSKa2)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSSK4" of type "2-D ARRAY REAL(SSK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSSKa2(1,1))
  valSSKa2(1,1)=0.0_SSK
  ALLOCATE(refSSKa2(1,1))
  refSSKa2=5.0_SSK
  CALL testParam%get('testSSK4',valSSKa2,DEFAULT=refSSKa2)
  ASSERT_EQ(valSSKa2(1,1),5.0_SSK,'%get, default')

  COMPONENT_TEST('set')
  refSSKa2=8.0_SSK
  CALL testParam%set('testSSK2',refSSKa2)
  valSSKa2(1,1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa2)
  ASSERT_EQ(valSSKa2(1,1),8.0_SSK,'%set')
  CALL testParam%set('testSSK4',refSSKa2)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSSK4" of type "2-D ARRAY REAL(SSK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSSKa2)
  DEALLOCATE(valSSKa2)

ENDSUBROUTINE testSSKa2
!
!-------------------------------------------------------------------------------
SUBROUTINE testSSKa3()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSSKa3(1,1,1))
  valSSKa3=5.0_SSK

  COMPONENT_TEST('add')
  CALL testParam%add('testSSK1',valSSKa3)
  CALL testParam%add('testSSK2',valSSKa3)
  CALL testParam%add('testSSK3',valSSKa3)
  CALL testParam%add('group - > testSSK1',valSSKa3)
  CALL testParam%add('group - > group 2 -> testSSK1',valSSKa3)

  COMPONENT_TEST('get')
  valSSKa3(1,1,1)=0.0_SSK
  CALL testParam%get('testSSK1',valSSKa3)
  ASSERT_EQ(valSSKa3(1,1,1),5.0_SSK,'%get')
  valSSKa3(1,1,1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa3)
  ASSERT_EQ(valSSKa3(1,1,1),5.0_SSK,'%get')
  valSSKa3(1,1,1)=0.0_SSK
  CALL testParam%get('testSSK3',valSSKa3)
  ASSERT_EQ(valSSKa3(1,1,1),5.0_SSK,'%get')
  valSSKa3(1,1,1)=0.0_SSK
  CALL testParam%get('group - > testSSK1',valSSKa3)
  ASSERT_EQ(valSSKa3(1,1,1),5.0_SSK,'%get')
  valSSKa3(1,1,1)=0.0_SSK
  CALL testParam%get('group - > group 2 -> testSSK1',valSSKa3)
  ASSERT_EQ(valSSKa3(1,1,1),5.0_SSK,'%get')
  valSSKa3(1,1,1)=0.0_SSK
  CALL testParam%get('testSSK4',valSSKa3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSSK4" of type "3-D ARRAY REAL(SSK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSSKa3(1,1,1))
  valSSKa3(1,1,1)=0.0_SSK
  ALLOCATE(refSSKa3(1,1,1))
  refSSKa3=5.0_SSK
  CALL testParam%get('testSSK4',valSSKa3,DEFAULT=refSSKa3)
  ASSERT_EQ(valSSKa3(1,1,1),5.0_SSK,'%get, default')

  COMPONENT_TEST('set')
  refSSKa3=8.0_SSK
  CALL testParam%set('testSSK2',refSSKa3)
  valSSKa3(1,1,1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa3)
  ASSERT_EQ(valSSKa3(1,1,1),8.0_SSK,'%set')
  CALL testParam%set('testSSK4',refSSKa3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSSK4" of type "3-D ARRAY REAL(SSK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSSKa3)
  DEALLOCATE(valSSKa3)

ENDSUBROUTINE testSSKa3
!
!-------------------------------------------------------------------------------
SUBROUTINE testSSKa4()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSSKa4(1,1,1,1))
  valSSKa4=5.0_SSK

  COMPONENT_TEST('add')
  CALL testParam%add('testSSK1',valSSKa4)
  CALL testParam%add('testSSK2',valSSKa4)
  CALL testParam%add('testSSK3',valSSKa4)
  CALL testParam%add('group - > testSSK1',valSSKa4)
  CALL testParam%add('group - > group 2 -> testSSK1',valSSKa4)

  COMPONENT_TEST('get')
  valSSKa4(1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK1',valSSKa4)
  ASSERT_EQ(valSSKa4(1,1,1,1),5.0_SSK,'%get')
  valSSKa4(1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa4)
  ASSERT_EQ(valSSKa4(1,1,1,1),5.0_SSK,'%get')
  valSSKa4(1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK3',valSSKa4)
  ASSERT_EQ(valSSKa4(1,1,1,1),5.0_SSK,'%get')
  valSSKa4(1,1,1,1)=0.0_SSK
  CALL testParam%get('group - > testSSK1',valSSKa4)
  ASSERT_EQ(valSSKa4(1,1,1,1),5.0_SSK,'%get')
  valSSKa4(1,1,1,1)=0.0_SSK
  CALL testParam%get('group - > group 2 -> testSSK1',valSSKa4)
  ASSERT_EQ(valSSKa4(1,1,1,1),5.0_SSK,'%get')
  valSSKa4(1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK4',valSSKa4)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSSK4" of type "4-D ARRAY REAL(SSK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSSKa4(1,1,1,1))
  valSSKa4(1,1,1,1)=0.0_SSK
  ALLOCATE(refSSKa4(1,1,1,1))
  refSSKa4=5.0_SSK
  CALL testParam%get('testSSK4',valSSKa4,DEFAULT=refSSKa4)
  ASSERT_EQ(valSSKa4(1,1,1,1),5.0_SSK,'%get, default')

  COMPONENT_TEST('set')
  refSSKa4=8.0_SSK
  CALL testParam%set('testSSK2',refSSKa4)
  valSSKa4(1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa4)
  ASSERT_EQ(valSSKa4(1,1,1,1),8.0_SSK,'%set')
  CALL testParam%set('testSSK4',refSSKa4)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSSK4" of type "4-D ARRAY REAL(SSK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSSKa4)
  DEALLOCATE(valSSKa4)

ENDSUBROUTINE testSSKa4
!
!-------------------------------------------------------------------------------
SUBROUTINE testSSKa5()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSSKa5(1,1,1,1,1))
  valSSKa5=5.0_SSK

  COMPONENT_TEST('add')
  CALL testParam%add('testSSK1',valSSKa5)
  CALL testParam%add('testSSK2',valSSKa5)
  CALL testParam%add('testSSK3',valSSKa5)
  CALL testParam%add('group - > testSSK1',valSSKa5)
  CALL testParam%add('group - > group 2 -> testSSK1',valSSKa5)

  COMPONENT_TEST('get')
  valSSKa5(1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK1',valSSKa5)
  ASSERT_EQ(valSSKa5(1,1,1,1,1),5.0_SSK,'%get')
  valSSKa5(1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa5)
  ASSERT_EQ(valSSKa5(1,1,1,1,1),5.0_SSK,'%get')
  valSSKa5(1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK3',valSSKa5)
  ASSERT_EQ(valSSKa5(1,1,1,1,1),5.0_SSK,'%get')
  valSSKa5(1,1,1,1,1)=0.0_SSK
  CALL testParam%get('group - > testSSK1',valSSKa5)
  ASSERT_EQ(valSSKa5(1,1,1,1,1),5.0_SSK,'%get')
  valSSKa5(1,1,1,1,1)=0.0_SSK
  CALL testParam%get('group - > group 2 -> testSSK1',valSSKa5)
  ASSERT_EQ(valSSKa5(1,1,1,1,1),5.0_SSK,'%get')
  valSSKa5(1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK4',valSSKa5)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSSK4" of type "5-D ARRAY REAL(SSK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSSKa5(1,1,1,1,1))
  valSSKa5(1,1,1,1,1)=0.0_SSK
  ALLOCATE(refSSKa5(1,1,1,1,1))
  refSSKa5=5.0_SSK
  CALL testParam%get('testSSK4',valSSKa5,DEFAULT=refSSKa5)
  ASSERT_EQ(valSSKa5(1,1,1,1,1),5.0_SSK,'%get, default')

  COMPONENT_TEST('set')
  refSSKa5=8.0_SSK
  CALL testParam%set('testSSK2',refSSKa5)
  valSSKa5(1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa5)
  ASSERT_EQ(valSSKa5(1,1,1,1,1),8.0_SSK,'%set')
  CALL testParam%set('testSSK4',refSSKa5)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSSK4" of type "5-D ARRAY REAL(SSK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSSKa5)
  DEALLOCATE(valSSKa5)

ENDSUBROUTINE testSSKa5
!
!-------------------------------------------------------------------------------
SUBROUTINE testSSKa6()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSSKa6(1,1,1,1,1,1))
  valSSKa6=5.0_SSK

  COMPONENT_TEST('add')
  CALL testParam%add('testSSK1',valSSKa6)
  CALL testParam%add('testSSK2',valSSKa6)
  CALL testParam%add('testSSK3',valSSKa6)
  CALL testParam%add('group - > testSSK1',valSSKa6)
  CALL testParam%add('group - > group 2 -> testSSK1',valSSKa6)

  COMPONENT_TEST('get')
  valSSKa6(1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK1',valSSKa6)
  ASSERT_EQ(valSSKa6(1,1,1,1,1,1),5.0_SSK,'%get')
  valSSKa6(1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa6)
  ASSERT_EQ(valSSKa6(1,1,1,1,1,1),5.0_SSK,'%get')
  valSSKa6(1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK3',valSSKa6)
  ASSERT_EQ(valSSKa6(1,1,1,1,1,1),5.0_SSK,'%get')
  valSSKa6(1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('group - > testSSK1',valSSKa6)
  ASSERT_EQ(valSSKa6(1,1,1,1,1,1),5.0_SSK,'%get')
  valSSKa6(1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('group - > group 2 -> testSSK1',valSSKa6)
  ASSERT_EQ(valSSKa6(1,1,1,1,1,1),5.0_SSK,'%get')
  valSSKa6(1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK4',valSSKa6)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSSK4" of type "6-D ARRAY REAL(SSK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSSKa6(1,1,1,1,1,1))
  valSSKa6(1,1,1,1,1,1)=0.0_SSK
  ALLOCATE(refSSKa6(1,1,1,1,1,1))
  refSSKa6=5.0_SSK
  CALL testParam%get('testSSK4',valSSKa6,DEFAULT=refSSKa6)
  ASSERT_EQ(valSSKa6(1,1,1,1,1,1),5.0_SSK,'%get, default')

  COMPONENT_TEST('set')
  refSSKa6=8.0_SSK
  CALL testParam%set('testSSK2',refSSKa6)
  valSSKa6(1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa6)
  ASSERT_EQ(valSSKa6(1,1,1,1,1,1),8.0_SSK,'%set')
  CALL testParam%set('testSSK4',refSSKa6)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSSK4" of type "6-D ARRAY REAL(SSK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSSKa6)
  DEALLOCATE(valSSKa6)

ENDSUBROUTINE testSSKa6
!
!-------------------------------------------------------------------------------
SUBROUTINE testSSKa7()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSSKa7(1,1,1,1,1,1,1))
  valSSKa7=5.0_SSK

  COMPONENT_TEST('add')
  CALL testParam%add('testSSK1',valSSKa7)
  CALL testParam%add('testSSK2',valSSKa7)
  CALL testParam%add('testSSK3',valSSKa7)
  CALL testParam%add('group - > testSSK1',valSSKa7)
  CALL testParam%add('group - > group 2 -> testSSK1',valSSKa7)

  COMPONENT_TEST('get')
  valSSKa7(1,1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK1',valSSKa7)
  ASSERT_EQ(valSSKa7(1,1,1,1,1,1,1),5.0_SSK,'%get')
  valSSKa7(1,1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa7)
  ASSERT_EQ(valSSKa7(1,1,1,1,1,1,1),5.0_SSK,'%get')
  valSSKa7(1,1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK3',valSSKa7)
  ASSERT_EQ(valSSKa7(1,1,1,1,1,1,1),5.0_SSK,'%get')
  valSSKa7(1,1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('group - > testSSK1',valSSKa7)
  ASSERT_EQ(valSSKa7(1,1,1,1,1,1,1),5.0_SSK,'%get')
  valSSKa7(1,1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('group - > group 2 -> testSSK1',valSSKa7)
  ASSERT_EQ(valSSKa7(1,1,1,1,1,1,1),5.0_SSK,'%get')
  valSSKa7(1,1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK4',valSSKa7)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSSK4" of type "7-D ARRAY REAL(SSK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSSKa7(1,1,1,1,1,1,1))
  valSSKa7(1,1,1,1,1,1,1)=0.0_SSK
  ALLOCATE(refSSKa7(1,1,1,1,1,1,1))
  refSSKa7=5.0_SSK
  CALL testParam%get('testSSK4',valSSKa7,DEFAULT=refSSKa7)
  ASSERT_EQ(valSSKa7(1,1,1,1,1,1,1),5.0_SSK,'%get, default')

  COMPONENT_TEST('set')
  refSSKa7=8.0_SSK
  CALL testParam%set('testSSK2',refSSKa7)
  valSSKa7(1,1,1,1,1,1,1)=0.0_SSK
  CALL testParam%get('testSSK2',valSSKa7)
  ASSERT_EQ(valSSKa7(1,1,1,1,1,1,1),8.0_SSK,'%set')
  CALL testParam%set('testSSK4',refSSKa7)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSSK4" of type "7-D ARRAY REAL(SSK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSSKa7)
  DEALLOCATE(valSSKa7)

ENDSUBROUTINE testSSKa7
!
!-------------------------------------------------------------------------------
SUBROUTINE testSDK()
  TYPE(StringType) :: msg,refmsg
  valSDK=5.0_SDK

  COMPONENT_TEST('add')
  CALL testParam%add('testSDK1',valSDK)
  CALL testParam%add('testSDK2',valSDK)
  CALL testParam%add('testSDK3',valSDK)
  CALL testParam%add('group - > testSDK1',valSDK)
  CALL testParam%add('group - > group 2 -> testSDK1',valSDK)

  COMPONENT_TEST('get')
  valSDK=0.0_SDK
  CALL testParam%get('testSDK1',valSDK)
  ASSERT_EQ(valSDK,5.0_SDK,'%get')
  valSDK=0.0_SDK
  CALL testParam%get('testSDK2',valSDK)
  ASSERT_EQ(valSDK,5.0_SDK,'%get')
  valSDK=0.0_SDK
  CALL testParam%get('testSDK3',valSDK)
  ASSERT_EQ(valSDK,5.0_SDK,'%get')
  valSDK=0.0_SDK
  CALL testParam%get('group - > testSDK1',valSDK)
  ASSERT_EQ(valSDK,5.0_SDK,'%get')
  valSDK=0.0_SDK
  CALL testParam%get('group - > group 2 -> testSDK1',valSDK)
  ASSERT_EQ(valSDK,5.0_SDK,'%get')
  valSDK=0.0_SDK
  CALL testParam%get('testSDK4',valSDK)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSDK4" of type "REAL(SDK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  valSDK=0.0_SDK
  CALL testParam%get('testSDK4',valSDK,DEFAULT=5.0_SDK)
  ASSERT_EQ(valSDK,5.0_SDK,'%get, default')

  COMPONENT_TEST('set')
  CALL testParam%set('testSDK2',8.0_SDK)
  valSDK=0.0_SDK
  CALL testParam%get('testSDK2',valSDK)
  ASSERT_EQ(valSDK,8.0_SDK,'%set')
  CALL testParam%set('testSDK4',8.0_SDK)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSDK4" of type "REAL(SDK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()

ENDSUBROUTINE testSDK
!
!-------------------------------------------------------------------------------
SUBROUTINE testSDKa1()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSDKa1(1))
  valSDKa1=5.0_SDK

  COMPONENT_TEST('add')
  CALL testParam%add('testSDK1',valSDKa1)
  CALL testParam%add('testSDK2',valSDKa1)
  CALL testParam%add('testSDK3',valSDKa1)
  CALL testParam%add('group - > testSDK1',valSDKa1)
  CALL testParam%add('group - > group 2 -> testSDK1',valSDKa1)

  COMPONENT_TEST('get')
  valSDKa1(1)=0.0_SDK
  CALL testParam%get('testSDK1',valSDKa1)
  ASSERT_EQ(valSDKa1(1),5.0_SDK,'%get')
  valSDKa1(1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa1)
  ASSERT_EQ(valSDKa1(1),5.0_SDK,'%get')
  valSDKa1(1)=0.0_SDK
  CALL testParam%get('testSDK3',valSDKa1)
  ASSERT_EQ(valSDKa1(1),5.0_SDK,'%get')
  valSDKa1(1)=0.0_SDK
  CALL testParam%get('group - > testSDK1',valSDKa1)
  ASSERT_EQ(valSDKa1(1),5.0_SDK,'%get')
  valSDKa1(1)=0.0_SDK
  CALL testParam%get('group - > group 2 -> testSDK1',valSDKa1)
  ASSERT_EQ(valSDKa1(1),5.0_SDK,'%get')
  valSDKa1(1)=0.0_SDK
  CALL testParam%get('testSDK4',valSDKa1)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSDK4" of type "1-D ARRAY REAL(SDK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSDKa1(1))
  valSDKa1(1)=0.0_SDK
  ALLOCATE(refSDKa1(1))
  refSDKa1=5.0_SDK
  CALL testParam%get('testSDK4',valSDKa1,DEFAULT=refSDKa1)
  ASSERT_EQ(valSDKa1(1),5.0_SDK,'%get, default')

  COMPONENT_TEST('set')
  refSDKa1=8.0_SDK
  CALL testParam%set('testSDK2',refSDKa1)
  valSDKa1(1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa1)
  ASSERT_EQ(valSDKa1(1),8.0_SDK,'%set')
  CALL testParam%set('testSDK4',refSDKa1)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSDK4" of type "1-D ARRAY REAL(SDK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSDKa1)
  DEALLOCATE(valSDKa1)

ENDSUBROUTINE testSDKa1
!
!-------------------------------------------------------------------------------
SUBROUTINE testSDKa2()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSDKa2(1,1))
  valSDKa2=5.0_SDK

  COMPONENT_TEST('add')
  CALL testParam%add('testSDK1',valSDKa2)
  CALL testParam%add('testSDK2',valSDKa2)
  CALL testParam%add('testSDK3',valSDKa2)
  CALL testParam%add('group - > testSDK1',valSDKa2)
  CALL testParam%add('group - > group 2 -> testSDK1',valSDKa2)

  COMPONENT_TEST('get')
  valSDKa2(1,1)=0.0_SDK
  CALL testParam%get('testSDK1',valSDKa2)
  ASSERT_EQ(valSDKa2(1,1),5.0_SDK,'%get')
  valSDKa2(1,1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa2)
  ASSERT_EQ(valSDKa2(1,1),5.0_SDK,'%get')
  valSDKa2(1,1)=0.0_SDK
  CALL testParam%get('testSDK3',valSDKa2)
  ASSERT_EQ(valSDKa2(1,1),5.0_SDK,'%get')
  valSDKa2(1,1)=0.0_SDK
  CALL testParam%get('group - > testSDK1',valSDKa2)
  ASSERT_EQ(valSDKa2(1,1),5.0_SDK,'%get')
  valSDKa2(1,1)=0.0_SDK
  CALL testParam%get('group - > group 2 -> testSDK1',valSDKa2)
  ASSERT_EQ(valSDKa2(1,1),5.0_SDK,'%get')
  valSDKa2(1,1)=0.0_SDK
  CALL testParam%get('testSDK4',valSDKa2)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSDK4" of type "2-D ARRAY REAL(SDK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSDKa2(1,1))
  valSDKa2(1,1)=0.0_SDK
  ALLOCATE(refSDKa2(1,1))
  refSDKa2=5.0_SDK
  CALL testParam%get('testSDK4',valSDKa2,DEFAULT=refSDKa2)
  ASSERT_EQ(valSDKa2(1,1),5.0_SDK,'%get, default')

  COMPONENT_TEST('set')
  refSDKa2=8.0_SDK
  CALL testParam%set('testSDK2',refSDKa2)
  valSDKa2(1,1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa2)
  ASSERT_EQ(valSDKa2(1,1),8.0_SDK,'%set')
  CALL testParam%set('testSDK4',refSDKa2)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSDK4" of type "2-D ARRAY REAL(SDK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSDKa2)
  DEALLOCATE(valSDKa2)

ENDSUBROUTINE testSDKa2
!
!-------------------------------------------------------------------------------
SUBROUTINE testSDKa3()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSDKa3(1,1,1))
  valSDKa3=5.0_SDK

  COMPONENT_TEST('add')
  CALL testParam%add('testSDK1',valSDKa3)
  CALL testParam%add('testSDK2',valSDKa3)
  CALL testParam%add('testSDK3',valSDKa3)
  CALL testParam%add('group - > testSDK1',valSDKa3)
  CALL testParam%add('group - > group 2 -> testSDK1',valSDKa3)

  COMPONENT_TEST('get')
  valSDKa3(1,1,1)=0.0_SDK
  CALL testParam%get('testSDK1',valSDKa3)
  ASSERT_EQ(valSDKa3(1,1,1),5.0_SDK,'%get')
  valSDKa3(1,1,1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa3)
  ASSERT_EQ(valSDKa3(1,1,1),5.0_SDK,'%get')
  valSDKa3(1,1,1)=0.0_SDK
  CALL testParam%get('testSDK3',valSDKa3)
  ASSERT_EQ(valSDKa3(1,1,1),5.0_SDK,'%get')
  valSDKa3(1,1,1)=0.0_SDK
  CALL testParam%get('group - > testSDK1',valSDKa3)
  ASSERT_EQ(valSDKa3(1,1,1),5.0_SDK,'%get')
  valSDKa3(1,1,1)=0.0_SDK
  CALL testParam%get('group - > group 2 -> testSDK1',valSDKa3)
  ASSERT_EQ(valSDKa3(1,1,1),5.0_SDK,'%get')
  valSDKa3(1,1,1)=0.0_SDK
  CALL testParam%get('testSDK4',valSDKa3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSDK4" of type "3-D ARRAY REAL(SDK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSDKa3(1,1,1))
  valSDKa3(1,1,1)=0.0_SDK
  ALLOCATE(refSDKa3(1,1,1))
  refSDKa3=5.0_SDK
  CALL testParam%get('testSDK4',valSDKa3,DEFAULT=refSDKa3)
  ASSERT_EQ(valSDKa3(1,1,1),5.0_SDK,'%get, default')

  COMPONENT_TEST('set')
  refSDKa3=8.0_SDK
  CALL testParam%set('testSDK2',refSDKa3)
  valSDKa3(1,1,1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa3)
  ASSERT_EQ(valSDKa3(1,1,1),8.0_SDK,'%set')
  CALL testParam%set('testSDK4',refSDKa3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSDK4" of type "3-D ARRAY REAL(SDK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSDKa3)
  DEALLOCATE(valSDKa3)

ENDSUBROUTINE testSDKa3
!
!-------------------------------------------------------------------------------
SUBROUTINE testSDKa4()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSDKa4(1,1,1,1))
  valSDKa4=5.0_SDK

  COMPONENT_TEST('add')
  CALL testParam%add('testSDK1',valSDKa4)
  CALL testParam%add('testSDK2',valSDKa4)
  CALL testParam%add('testSDK3',valSDKa4)
  CALL testParam%add('group - > testSDK1',valSDKa4)
  CALL testParam%add('group - > group 2 -> testSDK1',valSDKa4)

  COMPONENT_TEST('get')
  valSDKa4(1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK1',valSDKa4)
  ASSERT_EQ(valSDKa4(1,1,1,1),5.0_SDK,'%get')
  valSDKa4(1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa4)
  ASSERT_EQ(valSDKa4(1,1,1,1),5.0_SDK,'%get')
  valSDKa4(1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK3',valSDKa4)
  ASSERT_EQ(valSDKa4(1,1,1,1),5.0_SDK,'%get')
  valSDKa4(1,1,1,1)=0.0_SDK
  CALL testParam%get('group - > testSDK1',valSDKa4)
  ASSERT_EQ(valSDKa4(1,1,1,1),5.0_SDK,'%get')
  valSDKa4(1,1,1,1)=0.0_SDK
  CALL testParam%get('group - > group 2 -> testSDK1',valSDKa4)
  ASSERT_EQ(valSDKa4(1,1,1,1),5.0_SDK,'%get')
  valSDKa4(1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK4',valSDKa4)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSDK4" of type "4-D ARRAY REAL(SDK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSDKa4(1,1,1,1))
  valSDKa4(1,1,1,1)=0.0_SDK
  ALLOCATE(refSDKa4(1,1,1,1))
  refSDKa4=5.0_SDK
  CALL testParam%get('testSDK4',valSDKa4,DEFAULT=refSDKa4)
  ASSERT_EQ(valSDKa4(1,1,1,1),5.0_SDK,'%get, default')

  COMPONENT_TEST('set')
  refSDKa4=8.0_SDK
  CALL testParam%set('testSDK2',refSDKa4)
  valSDKa4(1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa4)
  ASSERT_EQ(valSDKa4(1,1,1,1),8.0_SDK,'%set')
  CALL testParam%set('testSDK4',refSDKa4)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSDK4" of type "4-D ARRAY REAL(SDK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSDKa4)
  DEALLOCATE(valSDKa4)

ENDSUBROUTINE testSDKa4
!
!-------------------------------------------------------------------------------
SUBROUTINE testSDKa5()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSDKa5(1,1,1,1,1))
  valSDKa5=5.0_SDK

  COMPONENT_TEST('add')
  CALL testParam%add('testSDK1',valSDKa5)
  CALL testParam%add('testSDK2',valSDKa5)
  CALL testParam%add('testSDK3',valSDKa5)
  CALL testParam%add('group - > testSDK1',valSDKa5)
  CALL testParam%add('group - > group 2 -> testSDK1',valSDKa5)

  COMPONENT_TEST('get')
  valSDKa5(1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK1',valSDKa5)
  ASSERT_EQ(valSDKa5(1,1,1,1,1),5.0_SDK,'%get')
  valSDKa5(1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa5)
  ASSERT_EQ(valSDKa5(1,1,1,1,1),5.0_SDK,'%get')
  valSDKa5(1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK3',valSDKa5)
  ASSERT_EQ(valSDKa5(1,1,1,1,1),5.0_SDK,'%get')
  valSDKa5(1,1,1,1,1)=0.0_SDK
  CALL testParam%get('group - > testSDK1',valSDKa5)
  ASSERT_EQ(valSDKa5(1,1,1,1,1),5.0_SDK,'%get')
  valSDKa5(1,1,1,1,1)=0.0_SDK
  CALL testParam%get('group - > group 2 -> testSDK1',valSDKa5)
  ASSERT_EQ(valSDKa5(1,1,1,1,1),5.0_SDK,'%get')
  valSDKa5(1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK4',valSDKa5)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSDK4" of type "5-D ARRAY REAL(SDK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSDKa5(1,1,1,1,1))
  valSDKa5(1,1,1,1,1)=0.0_SDK
  ALLOCATE(refSDKa5(1,1,1,1,1))
  refSDKa5=5.0_SDK
  CALL testParam%get('testSDK4',valSDKa5,DEFAULT=refSDKa5)
  ASSERT_EQ(valSDKa5(1,1,1,1,1),5.0_SDK,'%get, default')

  COMPONENT_TEST('set')
  refSDKa5=8.0_SDK
  CALL testParam%set('testSDK2',refSDKa5)
  valSDKa5(1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa5)
  ASSERT_EQ(valSDKa5(1,1,1,1,1),8.0_SDK,'%set')
  CALL testParam%set('testSDK4',refSDKa5)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSDK4" of type "5-D ARRAY REAL(SDK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSDKa5)
  DEALLOCATE(valSDKa5)

ENDSUBROUTINE testSDKa5
!
!-------------------------------------------------------------------------------
SUBROUTINE testSDKa6()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSDKa6(1,1,1,1,1,1))
  valSDKa6=5.0_SDK

  COMPONENT_TEST('add')
  CALL testParam%add('testSDK1',valSDKa6)
  CALL testParam%add('testSDK2',valSDKa6)
  CALL testParam%add('testSDK3',valSDKa6)
  CALL testParam%add('group - > testSDK1',valSDKa6)
  CALL testParam%add('group - > group 2 -> testSDK1',valSDKa6)

  COMPONENT_TEST('get')
  valSDKa6(1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK1',valSDKa6)
  ASSERT_EQ(valSDKa6(1,1,1,1,1,1),5.0_SDK,'%get')
  valSDKa6(1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa6)
  ASSERT_EQ(valSDKa6(1,1,1,1,1,1),5.0_SDK,'%get')
  valSDKa6(1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK3',valSDKa6)
  ASSERT_EQ(valSDKa6(1,1,1,1,1,1),5.0_SDK,'%get')
  valSDKa6(1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('group - > testSDK1',valSDKa6)
  ASSERT_EQ(valSDKa6(1,1,1,1,1,1),5.0_SDK,'%get')
  valSDKa6(1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('group - > group 2 -> testSDK1',valSDKa6)
  ASSERT_EQ(valSDKa6(1,1,1,1,1,1),5.0_SDK,'%get')
  valSDKa6(1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK4',valSDKa6)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSDK4" of type "6-D ARRAY REAL(SDK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSDKa6(1,1,1,1,1,1))
  valSDKa6(1,1,1,1,1,1)=0.0_SDK
  ALLOCATE(refSDKa6(1,1,1,1,1,1))
  refSDKa6=5.0_SDK
  CALL testParam%get('testSDK4',valSDKa6,DEFAULT=refSDKa6)
  ASSERT_EQ(valSDKa6(1,1,1,1,1,1),5.0_SDK,'%get, default')

  COMPONENT_TEST('set')
  refSDKa6=8.0_SDK
  CALL testParam%set('testSDK2',refSDKa6)
  valSDKa6(1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa6)
  ASSERT_EQ(valSDKa6(1,1,1,1,1,1),8.0_SDK,'%set')
  CALL testParam%set('testSDK4',refSDKa6)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSDK4" of type "6-D ARRAY REAL(SDK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSDKa6)
  DEALLOCATE(valSDKa6)

ENDSUBROUTINE testSDKa6
!
!-------------------------------------------------------------------------------
SUBROUTINE testSDKa7()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSDKa7(1,1,1,1,1,1,1))
  valSDKa7=5.0_SDK

  COMPONENT_TEST('add')
  CALL testParam%add('testSDK1',valSDKa7)
  CALL testParam%add('testSDK2',valSDKa7)
  CALL testParam%add('testSDK3',valSDKa7)
  CALL testParam%add('group - > testSDK1',valSDKa7)
  CALL testParam%add('group - > group 2 -> testSDK1',valSDKa7)

  COMPONENT_TEST('get')
  valSDKa7(1,1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK1',valSDKa7)
  ASSERT_EQ(valSDKa7(1,1,1,1,1,1,1),5.0_SDK,'%get')
  valSDKa7(1,1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa7)
  ASSERT_EQ(valSDKa7(1,1,1,1,1,1,1),5.0_SDK,'%get')
  valSDKa7(1,1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK3',valSDKa7)
  ASSERT_EQ(valSDKa7(1,1,1,1,1,1,1),5.0_SDK,'%get')
  valSDKa7(1,1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('group - > testSDK1',valSDKa7)
  ASSERT_EQ(valSDKa7(1,1,1,1,1,1,1),5.0_SDK,'%get')
  valSDKa7(1,1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('group - > group 2 -> testSDK1',valSDKa7)
  ASSERT_EQ(valSDKa7(1,1,1,1,1,1,1),5.0_SDK,'%get')
  valSDKa7(1,1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK4',valSDKa7)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSDK4" of type "7-D ARRAY REAL(SDK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(valSDKa7(1,1,1,1,1,1,1))
  valSDKa7(1,1,1,1,1,1,1)=0.0_SDK
  ALLOCATE(refSDKa7(1,1,1,1,1,1,1))
  refSDKa7=5.0_SDK
  CALL testParam%get('testSDK4',valSDKa7,DEFAULT=refSDKa7)
  ASSERT_EQ(valSDKa7(1,1,1,1,1,1,1),5.0_SDK,'%get, default')

  COMPONENT_TEST('set')
  refSDKa7=8.0_SDK
  CALL testParam%set('testSDK2',refSDKa7)
  valSDKa7(1,1,1,1,1,1,1)=0.0_SDK
  CALL testParam%get('testSDK2',valSDKa7)
  ASSERT_EQ(valSDKa7(1,1,1,1,1,1,1),8.0_SDK,'%set')
  CALL testParam%set('testSDK4',refSDKa7)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSDK4" of type "7-D ARRAY REAL(SDK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSDKa7)
  DEALLOCATE(valSDKa7)

ENDSUBROUTINE testSDKa7
!
!-------------------------------------------------------------------------------
SUBROUTINE testCHAR()
  TYPE(StringType) :: msg,refmsg

  COMPONENT_TEST('add')
  CALL testParam%add('testSTR1','test 1')
  CALL testParam%add('testSTR2','test 2')
  CALL testParam%add('testSTR3','test 3')
  CALL testParam%add('group - > testSTR1','test 4')
  CALL testParam%add('group - > group 2 -> testSTR1','test 5')

  COMPONENT_TEST('get')
  valCHAR='test'
  CALL testParam%get('testSTR1',valCHAR)
  ASSERT_EQ(valCHAR,'test 1','%get')
  CALL testParam%get('testSTR2',valCHAR)
  ASSERT_EQ(valCHAR,'test 2','%get')
  CALL testParam%get('testSTR3',valCHAR)
  ASSERT_EQ(valCHAR,'test 3','%get')
  CALL testParam%get('group - > testSTR1',valCHAR)
  ASSERT_EQ(valCHAR,'test 4','%get')
  CALL testParam%get('group - > group 2 -> testSTR1',valCHAR)
  ASSERT_EQ(valCHAR,'test 5','%get')
  CALL testParam%get('testSTR4',valCHAR)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSTR4" of type "TYPE(StringType)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  refSTR = 'test 10'
  CALL testParam%get('testSTR4',valCHAR,DEFAULT='test 10')
  ASSERT_EQ(valCHAR,'test 10','%get')

  COMPONENT_TEST('set')
  CALL testParam%set('testSTR2','test 20')
  CALL testParam%get('testSTR2',valCHAR)
  ASSERT_EQ(valCHAR,'test 20','%get')
  CALL testParam%set('testSTR4','test 30')
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSTR4" of type "TYPE(StringType)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(valChar)

ENDSUBROUTINE testCHAR
!
!-------------------------------------------------------------------------------
SUBROUTINE testCHAR2STR()
  TYPE(StringType) :: msg,refmsg

  COMPONENT_TEST('add')
  CALL testParam%add('testSTR1','test 1')
  CALL testParam%add('testSTR2','test 2')
  CALL testParam%add('testSTR3','test 3')
  CALL testParam%add('group - > testSTR1','test 4')
  CALL testParam%add('group - > group 2 -> testSTR1','test 5')

  COMPONENT_TEST('get')
  valSTR='test'
  CALL testParam%get('testSTR1',valSTR)
  ASSERT_EQ(CHAR(valStr),'test 1','%get')
  CALL testParam%get('testSTR2',valSTR)
  ASSERT_EQ(CHAR(valStr),'test 2','%get')
  CALL testParam%get('testSTR3',valSTR)
  ASSERT_EQ(CHAR(valStr),'test 3','%get')
  CALL testParam%get('group - > testSTR1',valSTR)
  ASSERT_EQ(CHAR(valStr),'test 4','%get')
  CALL testParam%get('group - > group 2 -> testSTR1',valSTR)
  ASSERT_EQ(CHAR(valStr),'test 5','%get')
  CALL testParam%get('testSTR4',valSTR)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSTR4" of type "TYPE(StringType)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  refSTR = 'test 10'
  CALL testParam%get('testSTR4',valSTR,DEFAULT=refSTR)
  ASSERT_EQ(CHAR(valStr),'test 10','%get')
  CALL testParam%get('testSTR4',valSTR,'test 15')
  ASSERT_EQ(CHAR(valStr),'test 15','%get')

  COMPONENT_TEST('set')
  CALL testParam%set('testSTR2','test 20')
  CALL testParam%get('testSTR2',valSTR)
  ASSERT_EQ(CHAR(valStr),'test 20','%get')
  CALL testParam%set('testSTR4','test 30')
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSTR4" of type "TYPE(StringType)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()

ENDSUBROUTINE testCHAR2STR
!
!-------------------------------------------------------------------------------
SUBROUTINE testSTR()
  TYPE(StringType) :: msg,refmsg

  COMPONENT_TEST('add')
  valSTR='test 1'
  CALL testParam%add('testSTR1',valSTR)
  valSTR='test 2'
  CALL testParam%add('testSTR2',valSTR)
  valSTR='test 3'
  CALL testParam%add('testSTR3',valSTR)
  valSTR='test 4'
  CALL testParam%add('group - > testSTR1',valSTR)
  valSTR='test 5'
  CALL testParam%add('group - > group 2 -> testSTR1',valSTR)

  COMPONENT_TEST('get')
  valSTR='test'
  CALL testParam%get('testSTR1',valSTR)
  ASSERT_EQ(CHAR(valStr),'test 1','%get')
  CALL testParam%get('testSTR2',valSTR)
  ASSERT_EQ(CHAR(valStr),'test 2','%get')
  CALL testParam%get('testSTR3',valSTR)
  ASSERT_EQ(CHAR(valStr),'test 3','%get')
  CALL testParam%get('group - > testSTR1',valSTR)
  ASSERT_EQ(CHAR(valStr),'test 4','%get')
  CALL testParam%get('group - > group 2 -> testSTR1',valSTR)
  ASSERT_EQ(CHAR(valStr),'test 5','%get')
  CALL testParam%get('testSTR4',valSTR)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSTR4" of type "TYPE(StringType)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  refSTR = 'test 10'
  CALL testParam%get('testSTR4',valSTR,DEFAULT=refSTR)
  ASSERT_EQ(CHAR(valStr),'test 10','%get')

  COMPONENT_TEST('set')
  refSTR = 'test 20'
  CALL testParam%set('testSTR2',refSTR)
  CALL testParam%get('testSTR2',valSTR)
  ASSERT_EQ(CHAR(valStr),'test 20','%get')
  CALL testParam%set('testSTR4','test 30')
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSTR4" of type "TYPE(StringType)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  refSTR = ''

  CALL testParam%clear()

ENDSUBROUTINE testSTR
!
!-------------------------------------------------------------------------------
SUBROUTINE testSTRa1()
  TYPE(StringType) :: msg,refmsg

  ALLOCATE(valSTRa1(1))
  ALLOCATE(refSTRa1(1))

  COMPONENT_TEST('add')
  valSTRa1(1)='test 1'
  CALL testParam%add('testSTR1',valSTRa1)
  valSTRa1(1)='test 2'
  CALL testParam%add('testSTR2',valSTRa1)
  valSTRa1(1)='test 3'
  CALL testParam%add('testSTR3',valSTRa1)
  valSTRa1(1)='test 4'
  CALL testParam%add('group - > testSTR1',valSTRa1)
  valSTRa1(1)='test 5'
  CALL testParam%add('group - > group 2 -> testSTR1',valSTRa1)

  COMPONENT_TEST('get')
  valSTRa1(1)='test'
  CALL testParam%get('testSTR1',valSTRa1)
  ASSERT_EQ(CHAR(valSTRa1(1)),'test 1','%get')
  CALL testParam%get('testSTR2',valSTRa1)
  ASSERT_EQ(CHAR(valSTRa1(1)),'test 2','%get')
  CALL testParam%get('testSTR3',valSTRa1)
  ASSERT_EQ(CHAR(valSTRa1(1)),'test 3','%get')
  CALL testParam%get('group - > testSTR1',valSTRa1)
  ASSERT_EQ(CHAR(valSTRa1(1)),'test 4','%get')
  CALL testParam%get('group - > group 2 -> testSTR1',valSTRa1)
  ASSERT_EQ(CHAR(valSTRa1(1)),'test 5','%get')
  CALL testParam%get('testSTR4',valSTRa1)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSTR4" of type "1-D ARRAY TYPE(StringType)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  refSTRa1(1) = 'test 10'
  CALL testParam%get('testSTR4',valSTRa1,DEFAULT=refSTRa1)
  ASSERT_EQ(CHAR(valSTRa1(1)),'test 10','%get')

  COMPONENT_TEST('set')
  refSTRa1 = 'test 20'
  CALL testParam%set('testSTR2',refSTRa1)
  CALL testParam%get('testSTR2',valSTRa1)
  ASSERT_EQ(CHAR(valSTRa1(1)),'test 20','%get')
  refSTRa1 = 'test 30'
  CALL testParam%set('testSTR4',refSTRa1)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSTR4" of type "1-D ARRAY TYPE(StringType)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSTRa1)
  DEALLOCATE(valSTRa1)

ENDSUBROUTINE testSTRa1
!
!-------------------------------------------------------------------------------
SUBROUTINE testSTRa2()
  TYPE(StringType) :: msg,refmsg

  ALLOCATE(valSTRa2(1,1))
  ALLOCATE(refSTRa2(1,1))

  COMPONENT_TEST('add')
  valSTRa2(1,1)='test 1'
  CALL testParam%add('testSTR1',valSTRa2)
  valSTRa2(1,1)='test 2'
  CALL testParam%add('testSTR2',valSTRa2)
  valSTRa2(1,1)='test 3'
  CALL testParam%add('testSTR3',valSTRa2)
  valSTRa2(1,1)='test 4'
  CALL testParam%add('group - > testSTR1',valSTRa2)
  valSTRa2(1,1)='test 5'
  CALL testParam%add('group - > group 2 -> testSTR1',valSTRa2)

  COMPONENT_TEST('get')
  valSTRa2(1,1)='test'
  CALL testParam%get('testSTR1',valSTRa2)
  ASSERT_EQ(CHAR(valSTRa2(1,1)),'test 1','%get')
  CALL testParam%get('testSTR2',valSTRa2)
  ASSERT_EQ(CHAR(valSTRa2(1,1)),'test 2','%get')
  CALL testParam%get('testSTR3',valSTRa2)
  ASSERT_EQ(CHAR(valSTRa2(1,1)),'test 3','%get')
  CALL testParam%get('group - > testSTR1',valSTRa2)
  ASSERT_EQ(CHAR(valSTRa2(1,1)),'test 4','%get')
  CALL testParam%get('group - > group 2 -> testSTR1',valSTRa2)
  ASSERT_EQ(CHAR(valSTRa2(1,1)),'test 5','%get')
  CALL testParam%get('testSTR4',valSTRa2)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSTR4" of type "2-D ARRAY TYPE(StringType)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  refSTRa2(1,1) = 'test 10'
  CALL testParam%get('testSTR4',valSTRa2,DEFAULT=refSTRa2)
  ASSERT_EQ(CHAR(valSTRa2(1,1)),'test 10','%get')

  COMPONENT_TEST('set')
  refSTRa2 = 'test 20'
  CALL testParam%set('testSTR2',refSTRa2)
  CALL testParam%get('testSTR2',valSTRa2)
  ASSERT_EQ(CHAR(valSTRa2(1,1)),'test 20','%get')
  refSTRa2 = 'test 30'
  CALL testParam%set('testSTR4',refSTRa2)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSTR4" of type "2-D ARRAY TYPE(StringType)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSTRa2)
  DEALLOCATE(valSTRa2)

ENDSUBROUTINE testSTRa2
!
!-------------------------------------------------------------------------------
SUBROUTINE testSTRa3()
  TYPE(StringType) :: msg,refmsg

  ALLOCATE(valSTRa3(1,1,1))
  ALLOCATE(refSTRa3(1,1,1))

  COMPONENT_TEST('add')
  valSTRa3(1,1,1)='test 1'
  CALL testParam%add('testSTR1',valSTRa3)
  valSTRa3(1,1,1)='test 2'
  CALL testParam%add('testSTR2',valSTRa3)
  valSTRa3(1,1,1)='test 3'
  CALL testParam%add('testSTR3',valSTRa3)
  valSTRa3(1,1,1)='test 4'
  CALL testParam%add('group - > testSTR1',valSTRa3)
  valSTRa3(1,1,1)='test 5'
  CALL testParam%add('group - > group 2 -> testSTR1',valSTRa3)

  COMPONENT_TEST('get')
  valSTRa3(1,1,1)='test'
  CALL testParam%get('testSTR1',valSTRa3)
  ASSERT_EQ(CHAR(valSTRa3(1,1,1)),'test 1','%get')
  CALL testParam%get('testSTR2',valSTRa3)
  ASSERT_EQ(CHAR(valSTRa3(1,1,1)),'test 2','%get')
  CALL testParam%get('testSTR3',valSTRa3)
  ASSERT_EQ(CHAR(valSTRa3(1,1,1)),'test 3','%get')
  CALL testParam%get('group - > testSTR1',valSTRa3)
  ASSERT_EQ(CHAR(valSTRa3(1,1,1)),'test 4','%get')
  CALL testParam%get('group - > group 2 -> testSTR1',valSTRa3)
  ASSERT_EQ(CHAR(valSTRa3(1,1,1)),'test 5','%get')
  CALL testParam%get('testSTR4',valSTRa3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSTR4" of type "3-D ARRAY TYPE(StringType)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  refSTRa3(1,1,1) = 'test 10'
  CALL testParam%get('testSTR4',valSTRa3,DEFAULT=refSTRa3)
  ASSERT_EQ(CHAR(valSTRa3(1,1,1)),'test 10','%get')

  COMPONENT_TEST('set')
  refSTRa3 = 'test 20'
  CALL testParam%set('testSTR2',refSTRa3)
  CALL testParam%get('testSTR2',valSTRa3)
  ASSERT_EQ(CHAR(valSTRa3(1,1,1)),'test 20','%get')
  refSTRa3 = 'test 30'
  CALL testParam%set('testSTR4',refSTRa3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSTR4" of type "3-D ARRAY TYPE(StringType)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSTRa3)
  DEALLOCATE(valSTRa3)

ENDSUBROUTINE testSTRa3
!
!-------------------------------------------------------------------------------
SUBROUTINE testSBK()
  TYPE(StringType) :: msg,refmsg
  valsbk=.TRUE.

  COMPONENT_TEST('add')
  CALL testParam%add('testSBK1',valSBK)
  CALL testParam%add('testSBK2',valSBK)
  CALL testParam%add('testSBK3',valSBK)
  CALL testParam%add('group - > testSBK1',valSBK)
  CALL testParam%add('group - > group 2 -> testSBK1',valSBK)

  COMPONENT_TEST('get')
  valsbk=.FALSE.
  CALL testParam%get('testSBK1',valSBK)
  ASSERT(valSBK,'%get')
  valsbk=.FALSE.
  CALL testParam%get('testSBK2',valSBK)
  ASSERT(valSBK,'%get')
  valsbk=.FALSE.
  CALL testParam%get('testSBK3',valSBK)
  ASSERT(valSBK,'%get')
  valsbk=.FALSE.
  CALL testParam%get('group - > testSBK1',valSBK)
  ASSERT(valSBK,'%get')
  valsbk=.FALSE.
  CALL testParam%get('group - > group 2 -> testSBK1',valSBK)
  ASSERT(valSBK,'%get')
  valsbk=.FALSE.
  CALL testParam%get('testSBK4',valSBK)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSBK4" of type "LOGICAL(SBK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  valSBK=.FALSE.
  CALL testParam%get('testSBK4',valSBK,DEFAULT=.TRUE.)
  ASSERT(valSBK,'%get, default')

  COMPONENT_TEST('set')
  CALL testParam%set('testSBK2',.FALSE.)
  CALL testParam%get('testSBK2',valSBK)
  ASSERT(.NOT.valSBK,'%set')
  CALL testParam%set('testSBK4',.TRUE.)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSBK4" of type "LOGICAL(SBK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()

ENDSUBROUTINE testSBK
!
!-------------------------------------------------------------------------------
SUBROUTINE testSBKa1()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSBKa1(1))
  valSBKa1=.TRUE.

  COMPONENT_TEST('add')
  CALL testParam%add('testSBK1',valSBKa1)
  CALL testParam%add('testSBK2',valSBKa1)
  CALL testParam%add('testSBK3',valSBKa1)
  CALL testParam%add('group - > testSBK1',valSBKa1)
  CALL testParam%add('group - > group 2 -> testSBK1',valSBKa1)

  COMPONENT_TEST('get')
  valSBKa1=.FALSE.
  CALL testParam%get('testSBK1',valSBKa1)
  ASSERT(valSBKa1(1),'%get')
  valSBKa1=.FALSE.
  CALL testParam%get('testSBK2',valSBKa1)
  ASSERT(valSBKa1(1),'%get')
  valSBKa1=.FALSE.
  CALL testParam%get('testSBK3',valSBKa1)
  ASSERT(valSBKa1(1),'%get')
  valSBKa1=.FALSE.
  CALL testParam%get('group - > testSBK1',valSBKa1)
  ASSERT(valSBKa1(1),'%get')
  valSBKa1=.FALSE.
  CALL testParam%get('group - > group 2 -> testSBK1',valSBKa1)
  ASSERT(valSBKa1(1),'%get')
  valSBKa1=.FALSE.
  CALL testParam%get('testSBK4',valSBKa1)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSBK4" of type "1-D ARRAY LOGICAL(SBK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(refSBKa1(1))
  refSBKa1=.TRUE.
  CALL testParam%get('testSBK4',valSBKa1,DEFAULT=refSBKa1)
  ASSERT(valSBKa1(1),'%get, default')

  COMPONENT_TEST('set')
  refSBKa1=.FALSE.
  CALL testParam%set('testSBK2',refSBKa1)
  CALL testParam%get('testSBK2',valSBKa1)
  ASSERT(.NOT.valSBKa1(1),'%set')
  refSBKa1=.TRUE.
  CALL testParam%set('testSBK4',refSBKa1)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSBK4" of type "1-D ARRAY LOGICAL(SBK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refsbka1)
  DEALLOCATE(valsbka1)

ENDSUBROUTINE testSBKa1
!
!-------------------------------------------------------------------------------
SUBROUTINE testSBKa2()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSBKa2(1,1))
  valSBKa2=.TRUE.

  COMPONENT_TEST('add')
  CALL testParam%add('testSBK1',valSBKa2)
  CALL testParam%add('testSBK2',valSBKa2)
  CALL testParam%add('testSBK3',valSBKa2)
  CALL testParam%add('group - > testSBK1',valSBKa2)
  CALL testParam%add('group - > group 2 -> testSBK1',valSBKa2)

  COMPONENT_TEST('get')
  valSBKa2=.FALSE.
  CALL testParam%get('testSBK1',valSBKa2)
  ASSERT(valSBKa2(1,1),'%get')
  valSBKa2=.FALSE.
  CALL testParam%get('testSBK2',valSBKa2)
  ASSERT(valSBKa2(1,1),'%get')
  valSBKa2=.FALSE.
  CALL testParam%get('testSBK3',valSBKa2)
  ASSERT(valSBKa2(1,1),'%get')
  valSBKa2=.FALSE.
  CALL testParam%get('group - > testSBK1',valSBKa2)
  ASSERT(valSBKa2(1,1),'%get')
  valSBKa2=.FALSE.
  CALL testParam%get('group - > group 2 -> testSBK1',valSBKa2)
  ASSERT(valSBKa2(1,1),'%get')
  valSBKa2=.FALSE.
  CALL testParam%get('testSBK4',valSBKa2)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSBK4" of type "2-D ARRAY LOGICAL(SBK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(refSBKa2(1,1))
  refSBKa2=.TRUE.
  CALL testParam%get('testSBK4',valSBKa2,DEFAULT=refSBKa2)
  ASSERT(valSBKa2(1,1),'%get, default')

  COMPONENT_TEST('set')
  refSBKa2=.FALSE.
  CALL testParam%set('testSBK2',refSBKa2)
  CALL testParam%get('testSBK2',valSBKa2)
  ASSERT(.NOT.valSBKa2(1,1),'%set')
  refSBKa2=.TRUE.
  CALL testParam%set('testSBK4',refSBKa2)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSBK4" of type "2-D ARRAY LOGICAL(SBK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSBKa2)
  DEALLOCATE(valSBKa2)

ENDSUBROUTINE testSBKa2
!
!-------------------------------------------------------------------------------
SUBROUTINE testSBKa3()
  TYPE(StringType) :: msg,refmsg
  ALLOCATE(valSBKa3(1,1,1))
  valSBKa3=.TRUE.

  COMPONENT_TEST('add')
  CALL testParam%add('testSBK1',valSBKa3)
  CALL testParam%add('testSBK2',valSBKa3)
  CALL testParam%add('testSBK3',valSBKa3)
  CALL testParam%add('group - > testSBK1',valSBKa3)
  CALL testParam%add('group - > group 2 -> testSBK1',valSBKa3)

  COMPONENT_TEST('get')
  valSBKa3=.FALSE.
  CALL testParam%get('testSBK1',valSBKa3)
  ASSERT(valSBKa3(1,1,1),'%get')
  valSBKa3=.FALSE.
  CALL testParam%get('testSBK2',valSBKa3)
  ASSERT(valSBKa3(1,1,1),'%get')
  valSBKa3=.FALSE.
  CALL testParam%get('testSBK3',valSBKa3)
  ASSERT(valSBKa3(1,1,1),'%get')
  valSBKa3=.FALSE.
  CALL testParam%get('group - > testSBK1',valSBKa3)
  ASSERT(valSBKa3(1,1,1),'%get')
  valSBKa3=.FALSE.
  CALL testParam%get('group - > group 2 -> testSBK1',valSBKa3)
  ASSERT(valSBKa3(1,1,1),'%get')
  valSBKa3=.FALSE.
  CALL testParam%get('testSBK4',valSBKa3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "testSBK4" of type "3-D ARRAY LOGICAL(SBK)" could not be found!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')
  ALLOCATE(refSBKa3(1,1,1))
  refSBKa3=.TRUE.
  CALL testParam%get('testSBK4',valSBKa3,DEFAULT=refSBKa3)
  ASSERT(valSBKa3(1,1,1),'%get, default')

  COMPONENT_TEST('set')
  refSBKa3=.FALSE.
  CALL testParam%set('testSBK2',refSBKa3)
  CALL testParam%get('testSBK2',valSBKa3)
  ASSERT(.NOT.valSBKa3(1,1,1),'%set')
  refSBKa3=.TRUE.
  CALL testParam%set('testSBK4',refSBKa3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_set - Parameter "testSBK4" of type "3-D ARRAY LOGICAL(SBK)" could not be found!  Either set addmissing=.TRUE. or use add method.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'%get, not present')

  CALL testParam%clear()
  DEALLOCATE(refSBKa3)
  DEALLOCATE(valSBKa3)

ENDSUBROUTINE testSBKa3
!
!-------------------------------------------------------------------------------
SUBROUTINE testTree()
  TYPE(StringType) :: msg,refmsg
  TYPE(ParamType) :: plist1,plist2,plist3
  TYPE(ParamTypeIterator) :: iterator

  COMPONENT_TEST('getTree')
  CALL plist1%add('A -> B -> C',1_SNK)
  CALL plist1%get('B',plist2)
  ASSERT(plist2%has('C'),'%has("C")')
  ASSERT(plist2%has('B -> C'),'%has("B -> C")')
  CALL plist1%clear()
  CALL plist2%clear()

  !This mimicks a particular case that was problematic
  CALL plist1%add('A -> B1 -> C -> D1',1_SIK)
  CALL plist1%add('A -> B1 -> C -> D2',2_SIK)
  CALL plist1%add('A -> B2 -> C -> D1',21_SIK)
  CALL plist1%add('A -> B2 -> C -> D2',22_SIK)
  CALL plist1%add('A -> B3 -> C -> D1',31_SIK)
  CALL plist1%add('A -> B3 -> C -> D2',32_SIK)
  CALL plist1%add('A -> B4 -> C -> D1',41_SIK)
  CALL plist1%add('A -> B4 -> C -> D2',42_SIK)
  CALl plist1%get('A -> B1',plist2)
  ASSERT(plist2%has('B1'),'B1')
  ASSERT(.NOT.plist2%has('B2'),'B2')
  ASSERT(.NOT.plist2%has('B3'),'B3')
  ASSERT(.NOT.plist2%has('B4'),'B4')
  CALL plist1%clear()
  CALL plist2%clear()

  COMPONENT_TEST('remove')
  CALL plist1%add('A -> B1',1_SNK)
  CALL plist1%remove('A -> B1')
  valslk = eParams%getCounter(EXCEPTION_ERROR)
  CALL plist1%get('A',plist2)
  ASSERT_EQ(eParams%getCounter(EXCEPTION_ERROR),valslk,'no new errors')
  CALL plist1%get('B1',valsnk)
  ASSERT_EQ(eParams%getCounter(EXCEPTION_ERROR),valslk+1,'1 new error')
  msg = eParams%getLastMessage()
  refmsg = '#### EXCEPTION_ERROR #### - PARAMETERLISTS::errorChecks_get - Parameter "B1" of type "INTEGER(SNK)" could not be found!'
  ASSERT_EQ(CHAR(msg),CHAR(refmsg),'error message')
  ASSERT(plist1%has('A'),'%has("A")')
  ASSERT(.NOT.plist1%has('B1'),'.NOT. %has("B1")')
  ASSERT(.NOT.plist1%has('A-> B1'),'.NOT. %has("A-> B1")')

  COMPONENT_TEST('addTree')
  CALL plist1%add('A -> B1',1_SNK)
  CALL plist2%add('A -> B2 -> C',2_SNK)
  CALL plist1%add('A',plist2)
  valsnk = 0_SNK
  CALL plist1%get('A -> B1',valsnk)
  ASSERT_EQ(valsnk,1_SNK,'pre-existing parameter')
  valsnk = 0_SNK
  CALL plist1%get('A -> A -> B2 -> C',valsnk)
  ASSERT_EQ(valsnk,2_SNK,'new parameter')
  CALL plist1%clear()
  CALL plist2%clear()

  CALL plist1%add('A -> B -> C -> D1',1_SNK)
  CALL plist2%add('C -> D2',2_SNK)
  CALL plist1%add('B -> C',plist2)
  ASSERT(plist1%has('A -> B -> C -> D1'),'%has("A -> B -> C -> D1")')
  ASSERT(plist1%has('B -> C -> C -> D2'),'%has("B -> C -> C -> D2")')
  CALL plist1%clear()
  CALL plist2%clear()

  CALL plist1%add('A -> B1',1_SNK)
  CALL plist1%add('A',plist2)
  CALL iterator%activate(plist1,SUBLISTS=.TRUE.)
  valstr = iterator%getCurrentName(.TRUE.)
  ASSERT_EQ(CHAR(valstr),'A->B1','first parameter')
  CALL iterator%advance()
  valstr = iterator%getCurrentName(.TRUE.)
  ASSERT_EQ(CHAR(valstr),'','no more parameters')
  CALL plist1%clear()
  CALL plist2%clear()

  !This test mimics a particular use case that was problematic
  CALL plist1%add('A -> B1',[0.0_SDK])
  CALL plist1%add('A -> B2 -> C','string')
  CALL plist1%get('A -> B1',valsdka1)
  CALL plist2%add('D -> B1',valsdka1)
  DEALLOCATE(valsdka1)
  CALL plist1%get('A -> B2',plist3)
  CALL plist2%add('D',plist3)
  ASSERT(plist2%has('D -> B1'),'%has("D -> B1")')
  ASSERT(plist2%has('D -> B2'),'%has("D -> B2")')
  ASSERT(plist2%has('D -> B2 -> C'),'%has("D -> B2 -> C")')
  CALL plist1%clear()
  CALL plist2%clear()

  COMPONENT_TEST('getIterator')
  CALL plist1%add('A -> B1 -> C -> D1',1_SIK)
  CALL plist1%add('A -> B1 -> C -> D2',2_SIK)
  CALL plist1%add('A -> B2 -> C -> D1',21_SIK)
  CALL plist1%add('A -> B2 -> C -> D2',22_SIK)
  CALL plist1%add('A -> B3 -> C -> D1',31_SIK)
  CALL plist1%add('A -> B3 -> C -> D2',32_SIK)
  CALL plist1%add('A -> B4 -> C -> D1',41_SIK)
  CALL plist1%add('A -> B4 -> C -> D2',42_SIK)
  CALL plist1%get('A -> B1 -> C',iterator) !D1
  ASSERT(iterator%isActive,'getIterator 1')
  CALL iterator%advance() !D2
  ASSERT(iterator%isActive,'getIterator 2')
  CALL iterator%advance() !end of PL
  ASSERT(.NOT.iterator%isActive,'getIterator done')

  CALL plist1%get('A',iterator,SUBLISTS=.FALSE.) !B1
  ASSERT(iterator%isActive,'getIterator 1')
  CALL iterator%advance() !B2
  ASSERT(iterator%isActive,'getIterator 2')
  CALL iterator%advance() !B3
  ASSERT(iterator%isActive,'getIterator 3')
  CALL iterator%advance() !B4
  ASSERT(iterator%isActive,'getIterator 4')
  CALL iterator%advance() !end of PL
  ASSERT(.NOT.iterator%isActive,'getIterator done')

  CALL plist1%get('A',iterator,SUBLISTS=.TRUE.) !B1 -> C -> D1
  ASSERT(iterator%isActive,'getIterator 1')
  CALL iterator%advance() !B1 -> C -> D2
  ASSERT(iterator%isActive,'getIterator 2')
  CALL iterator%advance() !B2
  ASSERT(iterator%isActive,'getIterator 3')
  CALL iterator%advance() !B2 -> C -> D1
  ASSERT(iterator%isActive,'getIterator 4')
  CALL iterator%advance() !B2 -> C -> D1
  ASSERT(iterator%isActive,'getIterator 5')
  CALL iterator%advance() !B3
  ASSERT(iterator%isActive,'getIterator 6')
  CALL iterator%advance() !B3 -> C -> D1
  ASSERT(iterator%isActive,'getIterator 7')
  CALL iterator%advance() !B3 -> C -> D2
  ASSERT(iterator%isActive,'getIterator 8')
  CALL iterator%advance() !B4
  ASSERT(iterator%isActive,'getIterator 9')
  CALL iterator%advance() !B4 -> C -> D1
  ASSERT(iterator%isActive,'getIterator 10')
  CALL iterator%advance() !B4 -> C -> D2
  ASSERT(iterator%isActive,'getIterator 11')
  CALL iterator%advance() !end of PL
  ASSERT(.NOT.iterator%isActive,'getIterator done')
  CALL plist1%clear()
  CALL plist2%clear()
  CALL plist3%clear()

ENDSUBROUTINE testTree
! !
! !-------------------------------------------------------------------------------
! SUBROUTINE testGetString()
!   LOGICAL(SBK) :: tmpbool
!   INTEGER(SNK),ALLOCATABLE :: snk2(:,:),snk3(:,:,:)
!   INTEGER(SLK),ALLOCATABLE :: slk2(:,:),slk3(:,:,:)
!   REAL(SSK),ALLOCATABLE :: ssk2(:,:),ssk3(:,:,:)
!   REAL(SDK),ALLOCATABLE :: sdk2(:,:),sdk3(:,:,:)
!   TYPE(StringType) :: tmpstr,str0
!   TYPE(StringType),ALLOCATABLE :: str1(:),str2(:,:),str3(:,:,:)
!   TYPE(StringType),ALLOCATABLE :: tmpstr1(:),tmpstr2(:,:),tmpstr3(:,:,:)


!   !Testing addition of SBK routine to parameter list
!   CALL testParam%add('testPL->testSBK',.TRUE.)
!   CALL testParam%add('testPL->testSBK2',.FALSE.,'comment')
!   CALL testParam%add('testPL->testSSK',7.0_SSK)
!   CALL testParam%add('testPL->testSSK2',8.0_SSK,'comment')
!   CALL testParam%add('testPL->testSDK',7.0_SDK)
!   CALL testParam%add('testPL->testSDK2',8.0_SDK,'comment')
!   CALL testParam%add('testPL->testSNK',7_SNK)
!   CALL testParam%add('testPL->testSNK2',8_SNK,'comment')
!   CALL testParam%add('testPL->testSLK',7_SLK)
!   CALL testParam%add('testPL->testSLK2',8_SLK,'comment')
!   str0='string1'
!   CALL testParam%add('testPL->testSTR',str0)
!   str0='string2'
!   CALL testParam%add('testPL->testSTR2',str0,'comment')
!   CALL testParam%add('testPL->testCHAR','char1')
!   CALL testParam%add('testPL->testCHAR2','char2','comment')
!   !1-D
!   CALL testParam%add('testPL->testSSKa1',(/1.5_SSK,1.6_SSK/))
!   CALL testParam%add('testPL->testSSKa1_2',(/1.7_SSK,1.8_SSK/),'comment')
!   CALL testParam%add('testPL->testSDKa1',(/2.5_SDK,2.6_SDK/))
!   CALL testParam%add('testPL->testSDKa1_2',(/2.7_SDK,2.8_SDK/),'comment')
!   CALL testParam%add('testPL->testSNKa1',(/-2_SNK,-3_SNK/))
!   CALL testParam%add('testPL->testSNKa1_2',(/2_SNK,3_SNK/),'comment')
!   CALL testParam%add('testPL->testSLKa1',(/-4_SLK,-5_SLK/))
!   CALL testParam%add('testPL->testSLKa1_2',(/4_SLK,5_SLK/),'comment')
!   CALL testParam%add('testPL->testSBKa1',(/.TRUE.,.FALSE./))
!   CALL testParam%add('testPL->testSBKa1_2',(/.FALSE.,.FALSE./),'comment')
!   ALLOCATE(str1(2))
!   str1(1)='stringarray1'; str1(2)='stringarray2'
!   CALL testParam%add('testPL->testSTRa1',str1)
!   str1(1)='stringarray3'; str1(2)='stringarray4'
!   CALL testParam%add('testPL->testSTRa1_2',str1,'comment')
!   !2-D
!   ALLOCATE(ssk2(2,2))
!   ssk2(1,1)=1.1_SSK; ssk2(2,1)=2.1_SSK
!   ssk2(1,2)=1.2_SSK; ssk2(2,2)=2.2_SSK
!   CALL testParam%add('testPL->testSSKa2',ssk2)
!   ssk2(1,1)=3.1_SSK; ssk2(2,1)=4.1_SSK
!   ssk2(1,2)=3.2_SSK; ssk2(2,2)=4.2_SSK
!   CALL testParam%add('testPL->testSSKa2_2',ssk2,'comment')
!   ALLOCATE(sdk2(2,2))
!   sdk2(1,1)=11.0_SDK; sdk2(2,1)=21.0_SDK
!   sdk2(1,2)=12.0_SDK; sdk2(2,2)=22.0_SDK
!   CALL testParam%add('testPL->testSDKa2',sdk2)
!   sdk2(1,1)=31.0_SDK; sdk2(2,1)=41.0_SDK
!   sdk2(1,2)=32.0_SDK; sdk2(2,2)=42.0_SDK
!   CALL testParam%add('testPL->testSDKa2_2',sdk2,'comment')
!   ALLOCATE(snk2(2,2))
!   snk2(1,1)=11; snk2(2,1)=21
!   snk2(1,2)=12; snk2(2,2)=22
!   CALL testParam%add('testPL->testSNKa2',snk2)
!   snk2(1,1)=31; snk2(2,1)=41
!   snk2(1,2)=32; snk2(2,2)=42
!   CALL testParam%add('testPL->testSNKa2_2',snk2,'comment')
!   ALLOCATE(slk2(2,2))
!   slk2(1,1)=110; slk2(2,1)=210
!   slk2(1,2)=120; slk2(2,2)=220
!   CALL testParam%add('testPL->testSLKa2',slk2)
!   slk2(1,1)=310; slk2(2,1)=410
!   slk2(1,2)=320; slk2(2,2)=420
!   CALL testParam%add('testPL->testSLKa2_2',slk2,'comment')
!   ALLOCATE(str2(2,2))
!   str2(1,1)='stringarray1'; str2(2,1)='stringarray2'
!   str2(1,2)='stringarray3'; str2(2,2)='stringarray4'
!   CALL testParam%add('testPL->testSTRa2',str2)
!   str2(1,1)='stringarray5'; str2(2,1)='stringarray6'
!   str2(1,2)='stringarray7'; str2(2,2)='stringarray8'
!   CALL testParam%add('testPL->testSTRa2_2',str2,'comment')
!   !3-D
!   ALLOCATE(ssk3(2,2,2))
!   ssk3(1,1,1)=1.11_SSK; ssk3(2,1,1)=2.11_SSK
!   ssk3(1,2,1)=1.21_SSK; ssk3(2,2,1)=2.21_SSK
!   ssk3(1,1,2)=1.12_SSK; ssk3(2,1,2)=2.12_SSK
!   ssk3(1,2,2)=1.22_SSK; ssk3(2,2,2)=2.22_SSK
!   CALL testParam%add('testPL->testSSKa3',ssk3)
!   ssk3(1,1,1)=3.11_SSK; ssk3(2,1,1)=4.11_SSK
!   ssk3(1,2,1)=3.21_SSK; ssk3(2,2,1)=4.21_SSK
!   ssk3(1,1,2)=3.12_SSK; ssk3(2,1,2)=4.12_SSK
!   ssk3(1,2,2)=3.22_SSK; ssk3(2,2,2)=4.22_SSK
!   CALL testParam%add('testPL->testSSKa3_2',ssk3,'comment')
!   ALLOCATE(sdk3(2,2,2))
!   sdk3(1,1,1)=11.1_SDK; sdk3(2,1,1)=21.1_SDK
!   sdk3(1,2,1)=12.1_SDK; sdk3(2,2,1)=22.1_SDK
!   sdk3(1,1,2)=11.2_SDK; sdk3(2,1,2)=21.2_SDK
!   sdk3(1,2,2)=12.2_SDK; sdk3(2,2,2)=22.2_SDK
!   CALL testParam%add('testPL->testSDKa3',sdk3)
!   sdk3(1,1,1)=31.1_SDK; sdk3(2,1,1)=41.1_SDK
!   sdk3(1,2,1)=32.1_SDK; sdk3(2,2,1)=42.1_SDK
!   sdk3(1,1,2)=31.2_SDK; sdk3(2,1,2)=41.2_SDK
!   sdk3(1,2,2)=32.2_SDK; sdk3(2,2,2)=42.2_SDK
!   CALL testParam%add('testPL->testSDKa3_2',sdk3,'comment')
!   ALLOCATE(snk3(2,2,2))
!   snk3(1,1,1)=111; snk3(2,1,1)=211
!   snk3(1,2,1)=121; snk3(2,2,1)=221
!   snk3(1,1,2)=112; snk3(2,1,2)=212
!   snk3(1,2,2)=122; snk3(2,2,2)=222
!   CALL testParam%add('testPL->testSNKa3',snk3)
!   snk3(1,1,1)=311; snk3(2,1,1)=411
!   snk3(1,2,1)=321; snk3(2,2,1)=421
!   snk3(1,1,2)=312; snk3(2,1,2)=412
!   snk3(1,2,2)=322; snk3(2,2,2)=422
!   CALL testParam%add('testPL->testSNKa3_2',snk3,'comment')
!   ALLOCATE(slk3(2,2,2))
!   slk3(1,1,1)=111; slk3(2,1,1)=211
!   slk3(1,2,1)=121; slk3(2,2,1)=221
!   slk3(1,1,2)=112; slk3(2,1,2)=212
!   slk3(1,2,2)=122; slk3(2,2,2)=222
!   CALL testParam%add('testPL->testSLKa3',slk3)
!   slk3(1,1,1)=311; slk3(2,1,1)=411
!   slk3(1,2,1)=321; slk3(2,2,1)=421
!   slk3(1,1,2)=312; slk3(2,1,2)=412
!   slk3(1,2,2)=322; slk3(2,2,2)=422
!   CALL testParam%add('testPL->testSLKa3_2',slk3,'comment')
!   ALLOCATE(str3(2,2,2))

!   !Test scalars.
!   CALL testParam%getString('testPL->testSBK',tmpstr)
!   ASSERT(tmpstr == 'T','testSBK string')
!   CALL testParam%getString('testPL->testSBK2',tmpstr)
!   ASSERT(tmpstr == 'F','testSBK2 string')
!   CALL testParam%getString('testPL->testSSK',tmpstr)
!   ASSERT(tmpstr == '7.000000E+00','testSSK string')
!   CALL testParam%getString('testPL->testSSK2',tmpstr)
!   ASSERT(tmpstr == '8.000000E+00','testSSK2 string')
!   CALL testParam%getString('testPL->testSDK',tmpstr)
!   ASSERT(tmpstr == '7.000000000000000E+00','testSDK string')
!   CALL testParam%getString('testPL->testSDK2',tmpstr)
!   ASSERT(tmpstr == '8.000000000000000E+00','testSDK2 string')
!   CALL testParam%getString('testPL->testSNK',tmpstr)
!   ASSERT(tmpstr == '7','testSNK string')
!   CALL testParam%getString('testPL->testSNK2',tmpstr)
!   ASSERT(tmpstr == '8','testSNK2 string')
!   CALL testParam%getString('testPL->testSLK',tmpstr)
!   ASSERT(tmpstr == '7','testSLK string')
!   CALL testParam%getString('testPL->testSLK2',tmpstr)
!   ASSERT(tmpstr == '8','testSLK2 string')
!   CALL testParam%getString('testPL->testSTR',tmpstr)
!   ASSERT(tmpstr == 'string1','testSTR string')
!   CALL testParam%getString('testPL->testSTR2',tmpstr)
!   ASSERT(tmpstr == 'string2','testSTR2 string')
!   CALL testParam%getString('testPL->testCHAR',tmpstr)
!   ASSERT(tmpstr == 'char1','testCHAR string')
!   CALL testParam%getString('testPL->testCHAR2',tmpstr)
!   ASSERT(tmpstr == 'char2','testCHAR2 string')
!   CALL testParam%getString('testPL->testSSKa1',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"1.500000E+00" "1.600000E+00"','testSSKa1 string')
!   CALL testParam%getString('testPL->testSSKa1_2',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"1.700000E+00" "1.800000E+00"','testSSKa1_2 string')
!   CALL testParam%getString('testPL->testSDKa1',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"2.500000000000000E+00" "2.600000000000000E+00"','testSDKa1 string')
!   CALL testParam%getString('testPL->testSDKa1_2',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"2.700000000000000E+00" "2.800000000000000E+00"','testSDKa1_2 string')
!   CALL testParam%getString('testPL->testSNKa1',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"-2" "-3"','testSNKa1 string')
!   CALL testParam%getString('testPL->testSNKa1_2',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"2" "3"','testSNKa1_2 string')
!   CALL testParam%getString('testPL->testSLKa1',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"-4" "-5"','testSLKa1 string')
!   CALL testParam%getString('testPL->testSLKa1_2',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"4" "5"','testSLKa1_2 string')
!   CALL testParam%getString('testPL->testSBKa1',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"T" "F"','testSBKa1 string')
!   CALL testParam%getString('testPL->testSBKa1_2',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"F" "F"','testSBKa1_2 string')
!   CALL testParam%getString('testPL->testSTRa1',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"stringarray1" "stringarray2"','testSTRa1 string')
!   CALL testParam%getString('testPL->testSTRa1_2',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"stringarray3" "stringarray4"','testSTRa1_2 string')

!   !Test 2-D arrays
!   CALL testParam%getString('testPL->testSSKa2',tmpstr)
!   tmpbool=(tmpstr == '"1.100000E+00" "2.100000E+00" "1.200000E+00" "2.200000E+00"')
!   ASSERT(tmpbool,'testSSKa2 string')
!   FINFO() CHAR(tmpstr)
!   CALL testParam%getString('testPL->testSSKa2_2',tmpstr)
!   tmpbool=(tmpstr == '"3.100000E+00" "4.100000E+00" "3.200000E+00" "4.200000E+00"')
!   ASSERT(tmpbool,'testSSKa2_2 string')
!   FINFO() CHAR(tmpstr)
!   CALL testParam%getString('testPL->testSDKa2',tmpstr)
!   tmpbool=(tmpstr == '"1.100000000000000E+01" "2.100000000000000E+01" '// &
!       '"1.200000000000000E+01" "2.200000000000000E+01"')
!   ASSERT(tmpbool,'testSDKa2 string')
!   CALL testParam%getString('testPL->testSDKa2_2',tmpstr)
!   tmpbool=(tmpstr == '"3.100000000000000E+01" "4.100000000000000E+01" '// &
!       '"3.200000000000000E+01" "4.200000000000000E+01"')
!   ASSERT(tmpbool,'testSDKa2_2 string')
!   CALL testParam%getString('testPL->testSNKa2',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"11" "21" "12" "22"','testSNKa2 string')
!   CALL testParam%getString('testPL->testSNKa2_2',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"31" "41" "32" "42"','testSNKa2_2 string')
!   CALL testParam%getString('testPL->testSLKa2',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"110" "210" "120" "220"','testSLKa2 string')
!   CALL testParam%getString('testPL->testSLKa2_2',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"310" "410" "320" "420"','testSLKa2_2 string')
!   CALL testParam%getString('testPL->testSTRa2',tmpstr)
!   tmpbool=(tmpstr == '"stringarray1" "stringarray2" "stringarray3" "stringarray4"')
!   ASSERT(tmpbool,'testSTRa2 string')
!   CALL testParam%getString('testPL->testSTRa2_2',tmpstr)
!   tmpbool=(tmpstr == '"stringarray5" "stringarray6" "stringarray7" "stringarray8"')
!   ASSERT(tmpbool,'testSTRa2_2 string')

!   !Test 3-D arrays
!   CALL testParam%getString('testPL->testSSKa3',tmpstr)
!   tmpbool=(tmpstr == '"1.110000E+00" "2.110000E+00" "1.210000E+00" "2.210000E+00" '// &
!       '"1.120000E+00" "2.120000E+00" "1.220000E+00" "2.220000E+00"')
!   ASSERT(tmpbool,'testSSKa3 string')
!   CALL testParam%getString('testPL->testSSKa3_2',tmpstr)
!   tmpbool=(tmpstr == '"3.110000E+00" "4.110000E+00" "3.210000E+00" "4.210000E+00" '// &
!       '"3.120000E+00" "4.120000E+00" "3.220000E+00" "4.220000E+00"')
!   ASSERT(tmpbool,'testSSKa3_2 string')
!   CALL testParam%getString('testPL->testSDKa3',tmpstr)
!   tmpbool=(tmpstr == '"1.110000000000000E+01" "2.110000000000000E+01" '// &
!       '"1.210000000000000E+01" "2.210000000000000E+01" '// &
!       '"1.120000000000000E+01" "2.120000000000000E+01" '// &
!       '"1.220000000000000E+01" "2.220000000000000E+01"')
!   ASSERT(tmpbool,'testSDKa3 string')
!   FINFO() CHAR(tmpstr)
!   CALL testParam%getString('testPL->testSDKa3_2',tmpstr)
!   tmpbool=(tmpstr == '"3.110000000000000E+01" "4.110000000000000E+01" '// &
!       '"3.210000000000000E+01" "4.210000000000000E+01" '// &
!       '"3.120000000000000E+01" "4.120000000000000E+01" '// &
!       '"3.220000000000000E+01" "4.220000000000000E+01"')
!   ASSERT(tmpbool,'testSDKa3_2 string')
!   CALL testParam%getString('testPL->testSNKa3',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"111" "211" "121" "221" "112" "212" "122" "222"','testSNKa3 string')
!   CALL testParam%getString('testPL->testSNKa3_2',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"311" "411" "321" "421" "312" "412" "322" "422"','testSNKa3_2 string')
!   CALL testParam%getString('testPL->testSLKa3',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"111" "211" "121" "221" "112" "212" "122" "222"','testSLKa3 string')
!   CALL testParam%getString('testPL->testSLKa3_2',tmpstr)
!   ASSERT_EQ(CHAR(tmpstr),'"311" "411" "321" "421" "312" "412" "322" "422"','testSLKa3_2 string')

!   !Test 1-D arrays
!   CALL testParam%getString('testPL->testSSKa1',tmpstr1)
!   str1(1)='1.500000E+00'; str1(2)='1.600000E+00'
!   ASSERT(ALL(tmpstr1==str1),'testSSKa1 string')
!   CALL testParam%getString('testPL->testSSKa1_2',tmpstr1)
!   str1(1)='1.700000E+00'; str1(2)='1.800000E+00'
!   ASSERT(ALL(tmpstr1==str1),'testSSKa1_2 string')
!   CALL testParam%getString('testPL->testSDKa1',tmpstr1)
!   str1(1)='2.500000000000000E+00'; str1(2)='2.600000000000000E+00'
!   ASSERT(ALL(tmpstr1==str1),'testSDKa1 string')
!   CALL testParam%getString('testPL->testSDKa1_2',tmpstr1)
!   str1(1)='2.700000000000000E+00'; str1(2)='2.800000000000000E+00'
!   ASSERT(ALL(tmpstr1==str1),'testSDKa1_2 string')
!   CALL testParam%getString('testPL->testSNKa1',tmpstr1)
!   str1(1)='-2'; str1(2)='-3'
!   ASSERT(ALL(tmpstr1==str1),'testSNKa1 string')
!   CALL testParam%getString('testPL->testSNKa1_2',tmpstr1)
!   str1(1)='2'; str1(2)='3'
!   ASSERT(ALL(tmpstr1==str1),'testSNKa1_2 string')
!   CALL testParam%getString('testPL->testSLKa1',tmpstr1)
!   str1(1)='-4'; str1(2)='-5'
!   ASSERT(ALL(tmpstr1==str1),'testSLKa1 string')
!   CALL testParam%getString('testPL->testSLKa1_2',tmpstr1)
!   str1(1)='4'; str1(2)='5'
!   ASSERT(ALL(tmpstr1==str1),'testSLKa1_2 string')
!   CALL testParam%getString('testPL->testSBKa1',tmpstr1)
!   str1(1)='T'; str1(2)='F'
!   ASSERT(ALL(tmpstr1==str1),'testSBKa1 string')
!   CALL testParam%getString('testPL->testSBKa1_2',tmpstr1)
!   str1(1)='F'; str1(2)='F'
!   ASSERT(ALL(tmpstr1==str1),'testSBKa1_2 string')
!   CALL testParam%getString('testPL->testSTRa1',tmpstr1)
!   str1(1)='stringarray1'; str1(2)='stringarray2'
!   ASSERT(ALL(tmpstr1==str1),'testSTRa1 string')
!   CALL testParam%getString('testPL->testSTRa1_2',tmpstr1)
!   str1(1)='stringarray3'; str1(2)='stringarray4'
!   ASSERT(ALL(tmpstr1==str1),'testSTRa1_2 string')

!   !Test 2-D arrays
!   CALL testParam%getString('testPL->testSSKa2',tmpstr2)
!   str2(1,1)='1.100000E+00'; str2(2,1)='2.100000E+00'
!   str2(1,2)='1.200000E+00'; str2(2,2)='2.200000E+00'
!   ASSERT(ALL(tmpstr2==str2),'testSSKa2 string')
!   CALL testParam%getString('testPL->testSSKa2_2',tmpstr2)
!   str2(1,1)='3.100000E+00'; str2(2,1)='4.100000E+00'
!   str2(1,2)='3.200000E+00'; str2(2,2)='4.200000E+00'
!   ASSERT(ALL(tmpstr2==str2),'testSSKa2_2 string')
!   CALL testParam%getString('testPL->testSDKa2',tmpstr2)
!   str2(1,1)='1.100000000000000E+01'; str2(2,1)='2.100000000000000E+01'
!   str2(1,2)='1.200000000000000E+01'; str2(2,2)='2.200000000000000E+01'
!   ASSERT(ALL(tmpstr2==str2),'testSDKa2 string')
!   CALL testParam%getString('testPL->testSDKa2_2',tmpstr2)
!   str2(1,1)='3.100000000000000E+01'; str2(2,1)='4.100000000000000E+01'
!   str2(1,2)='3.200000000000000E+01'; str2(2,2)='4.200000000000000E+01'
!   ASSERT(ALL(tmpstr2==str2),'testSDKa2_2 string')
!   CALL testParam%getString('testPL->testSNKa2',tmpstr2)
!   str2(1,1)='11'; str2(2,1)='21'
!   str2(1,2)='12'; str2(2,2)='22'
!   ASSERT(ALL(tmpstr2==str2),'testSNKa2 string')
!   CALL testParam%getString('testPL->testSNKa2_2',tmpstr2)
!   str2(1,1)='31'; str2(2,1)='41'
!   str2(1,2)='32'; str2(2,2)='42'
!   ASSERT(ALL(tmpstr2==str2),'testSNKa2_2 string')
!   CALL testParam%getString('testPL->testSLKa2',tmpstr2)
!   str2(1,1)='110'; str2(2,1)='210'
!   str2(1,2)='120'; str2(2,2)='220'
!   ASSERT(ALL(tmpstr2==str2),'testSLKa2 string')
!   CALL testParam%getString('testPL->testSLKa2_2',tmpstr2)
!   str2(1,1)='310'; str2(2,1)='410'
!   str2(1,2)='320'; str2(2,2)='420'
!   ASSERT(ALL(tmpstr2==str2),'testSLKa2_2 string')
!   CALL testParam%getString('testPL->testSTRa2',tmpstr2)
!   str2(1,1)='stringarray1'; str2(2,1)='stringarray2'
!   str2(1,2)='stringarray3'; str2(2,2)='stringarray4'
!   ASSERT(ALL(tmpstr2==str2),'testSTRa2 string')
!   CALL testParam%getString('testPL->testSTRa2_2',tmpstr2)
!   str2(1,1)='stringarray5'; str2(2,1)='stringarray6'
!   str2(1,2)='stringarray7'; str2(2,2)='stringarray8'
!   ASSERT(ALL(tmpstr2==str2),'testSTRa2_2 string')

!   !Test 3-D arrays
!   CALL testParam%getString('testPL->testSSKa3',tmpstr3)
!   str3(1,1,1)='1.110000E+00'; str3(2,1,1)='2.110000E+00'
!   str3(1,2,1)='1.210000E+00'; str3(2,2,1)='2.210000E+00'
!   str3(1,1,2)='1.120000E+00'; str3(2,1,2)='2.120000E+00'
!   str3(1,2,2)='1.220000E+00'; str3(2,2,2)='2.220000E+00'
!   ASSERT(ALL(tmpstr3==str3),'testSSKa3 string')
!   CALL testParam%getString('testPL->testSSKa3_2',tmpstr3)
!   str3(1,1,1)='3.110000E+00'; str3(2,1,1)='4.110000E+00'
!   str3(1,2,1)='3.210000E+00'; str3(2,2,1)='4.210000E+00'
!   str3(1,1,2)='3.120000E+00'; str3(2,1,2)='4.120000E+00'
!   str3(1,2,2)='3.220000E+00'; str3(2,2,2)='4.220000E+00'
!   ASSERT(ALL(tmpstr3==str3),'testSSKa3_2 string')
!   CALL testParam%getString('testPL->testSDKa3',tmpstr3)
!   str3(1,1,1)='1.110000000000000E+01'; str3(2,1,1)='2.110000000000000E+01'
!   str3(1,2,1)='1.210000000000000E+01'; str3(2,2,1)='2.210000000000000E+01'
!   str3(1,1,2)='1.120000000000000E+01'; str3(2,1,2)='2.120000000000000E+01'
!   str3(1,2,2)='1.220000000000000E+01'; str3(2,2,2)='2.220000000000000E+01'
!   ASSERT(ALL(tmpstr3==str3),'testSDKa3 string')
!   FINFO() CHAR(tmpstr3(1,1,1))
!   CALL testParam%getString('testPL->testSDKa3_2',tmpstr3)
!   str3(1,1,1)='3.110000000000000E+01'; str3(2,1,1)='4.110000000000000E+01'
!   str3(1,2,1)='3.210000000000000E+01'; str3(2,2,1)='4.210000000000000E+01'
!   str3(1,1,2)='3.120000000000000E+01'; str3(2,1,2)='4.120000000000000E+01'
!   str3(1,2,2)='3.220000000000000E+01'; str3(2,2,2)='4.220000000000000E+01'
!   ASSERT(ALL(tmpstr3==str3),'testSDKa3_2 string')
!   CALL testParam%getString('testPL->testSNKa3',tmpstr3)
!   str3(1,1,1)='111'; str3(2,1,1)='211'
!   str3(1,2,1)='121'; str3(2,2,1)='221'
!   str3(1,1,2)='112'; str3(2,1,2)='212'
!   str3(1,2,2)='122'; str3(2,2,2)='222'
!   ASSERT(ALL(tmpstr3==str3),'testSNKa3 string')
!   CALL testParam%getString('testPL->testSNKa3_2',tmpstr3)
!   str3(1,1,1)='311'; str3(2,1,1)='411'
!   str3(1,2,1)='321'; str3(2,2,1)='421'
!   str3(1,1,2)='312'; str3(2,1,2)='412'
!   str3(1,2,2)='322'; str3(2,2,2)='422'
!   ASSERT(ALL(tmpstr3==str3),'testSNKa3_2 string')
!   CALL testParam%getString('testPL->testSLKa3',tmpstr3)
!   str3(1,1,1)='111'; str3(2,1,1)='211'
!   str3(1,2,1)='121'; str3(2,2,1)='221'
!   str3(1,1,2)='112'; str3(2,1,2)='212'
!   str3(1,2,2)='122'; str3(2,2,2)='222'
!   ASSERT(ALL(tmpstr3==str3),'testSLKa3 string')
!   CALL testParam%getString('testPL->testSLKa3_2',tmpstr3)
!   str3(1,1,1)='311'; str3(2,1,1)='411'
!   str3(1,2,1)='321'; str3(2,2,1)='421'
!   str3(1,1,2)='312'; str3(2,1,2)='412'
!   str3(1,2,2)='322'; str3(2,2,2)='422'
!   ASSERT(ALL(tmpstr3==str3),'testSLKa3_2 string')

!   CALL clear_test_vars()
!   !Deallocate locals
!   DEALLOCATE(snk2)
!   DEALLOCATE(snk3)
!   DEALLOCATE(slk2)
!   DEALLOCATE(slk3)
!   DEALLOCATE(ssk2)
!   DEALLOCATE(ssk3)
!   DEALLOCATE(sdk2)
!   DEALLOCATE(sdk3)
!   DEALLOCATE(str1)
!   DEALLOCATE(str2)

!   CALL testParam%clear()
!   CALL testParam2%clear()
! ENDSUBROUTINE testGetString
! !
! !-------------------------------------------------------------------------------
! SUBROUTINE testConvertTo2DStringArray()
!   INTEGER(SIK) :: i,j
!   TYPE(StringType) :: addr
!   TYPE(StringType),ALLOCATABLE :: str1a(:),str2a(:,:),table(:,:),reftable(:,:)

!   !Test null call
!   CALL testParam%clear()
!   addr=''
!   CALL testParam%convertTo2DStringArray(addr,table)
!   ASSERT(.NOT. ALLOCATED(table),'empty call')

!   !Setup the PL
!   CALL testParam%add('TestPL->List1->1->"TitleRow"','"TitleRow"')
!   CALL testParam%add('TestPL->List1->1->"Scalar Row1"','"Scalar Row1"')
!   CALL testParam%add('TestPL->List1->1->"1-D testRow2"','"1-D testRow2"')
!   CALL testParam%add('TestPL->List1->1->"2-D testRow3" "Extra"','"2-D testRow3" "Extra"')
!   CALL testParam%add('TestPL->List1->1->"3-D testRow4"','"3-D testRow4"')
!   CALL testParam%add('TestPL->List1->2->"TitleRow"','SSK')
!   CALL testParam%add('TestPL->List1->2->"Scalar Row1"',1.0_SSK)
!   CALL testParam%add('TestPL->List1->2->"1-D testRow2"',(/2.0_SSK,3.0_SSK/))
!   CALL testParam%add('TestPL->List1->2->"2-D testRow3" "Extra"', &
!       RESHAPE((/4.0_SSK,5.0_SSK,6.0_SSK,7.0_SSK/),(/2,2/)))
!   CALL testParam%add('TestPL->List1->2->"3-D testRow4"', &
!       RESHAPE((/8.0_SSK,9.0_SSK,1.1_SSK,1.2_SSK,1.3_SSK,1.4_SSK,1.5_SSK,1.6_SSK/),(/2,2,2/)))
!   CALL testParam%add('TestPL->List1->3->"TitleRow"','SDK')
!   CALL testParam%add('TestPL->List1->3->"Scalar Row1"',1.0_SDK)
!   CALL testParam%add('TestPL->List1->3->"1-D testRow2"',(/2.0_SDK,3.0_SDK/))
!   CALL testParam%add('TestPL->List1->3->"2-D testRow3" "Extra"', &
!       RESHAPE((/4.0_SDK,5.0_SDK,6.0_SDK,7.0_SDK/),(/2,2/)))
!   CALL testParam%add('TestPL->List1->3->"3-D testRow4"', &
!       RESHAPE((/8.0_SDK,9.0_SDK,1.1_SDK,1.2_SDK,1.3_SDK,1.4_SDK,1.5_SDK,1.6_SDK/),(/2,2,2/)))
!   CALL testParam%add('TestPL->List1->4->"TitleRow"','SNK')
!   CALL testParam%add('TestPL->List1->4->"Scalar Row1"',1_SNK)
!   CALL testParam%add('TestPL->List1->4->"1-D testRow2"',(/2_SNK,3_SNK/))
!   CALL testParam%add('TestPL->List1->4->"2-D testRow3" "Extra"', &
!       RESHAPE((/4_SNK,5_SNK,6_SNK,7_SNK/),(/2,2/)))
!   CALL testParam%add('TestPL->List1->4->"3-D testRow4"', &
!       RESHAPE((/8_SNK,9_SNK,11_SNK,12_SNK,13_SNK,14_SNK,15_SNK,16_SNK/),(/2,2,2/)))
!   CALL testParam%add('TestPL->List1->5->"TitleRow"','SLK')
!   CALL testParam%add('TestPL->List1->5->"Scalar Row1"',1_SLK)
!   CALL testParam%add('TestPL->List1->5->"1-D testRow2"',(/2_SLK,3_SLK/))
!   CALL testParam%add('TestPL->List1->5->"2-D testRow3" "Extra"', &
!       RESHAPE((/4_SLK,5_SLK,6_SLK,7_SLK/),(/2,2/)))
!   CALL testParam%add('TestPL->List1->5->"3-D testRow4"', &
!       RESHAPE((/8_SLK,9_SLK,11_SLK,12_SLK,13_SLK,14_SLK,15_SLK,16_SLK/),(/2,2,2/)))
!   CALL testParam%add('TestPL->List1->6->"TitleRow"','SBK')
!   CALL testParam%add('TestPL->List1->6->"Scalar Row1"',.TRUE.)
!   CALL testParam%add('TestPL->List1->6->"1-D testRow2"',(/.FALSE.,.TRUE./))
!   CALL testParam%add('TestPL->List1->7->"TitleRow"','STR')
!   CALL testParam%add('TestPL->List1->7->"Scalar Row1"','"Scalar String"')
!   ALLOCATE(str1a(2)); str1a(1)='test 1-D 1'; str1a(2)='test 1-D 2'
!   CALL testParam%add('TestPL->List1->7->"1-D testRow2"',str1a)
!   ALLOCATE(str2a(2,2)); str2a(1,1)='2-D 1'; str2a(2,1)='2-D 2'
!   str2a(1,2)='2-D 3'; str2a(2,2)='2-D 4'
!   CALL testParam%add('TestPL->List1->7->"2-D testRow3" "Extra"',str2a)

!   !Test bad addr call
!   addr='wrong'
!   CALL testParam%convertTo2DStringArray(addr,table)
!   ASSERT(.NOT. ALLOCATED(table),'bad addr call')

!   ALLOCATE(reftable(7,5))
!   reftable='-'
!   reftable(1,1)='"TitleRow"'; reftable(2,1)='SSK'; reftable(3,1)='SDK'; reftable(4,1)='SNK'
!   reftable(5,1)='SLK'; reftable(6,1)='SBK'; reftable(7,1)='STR'
!   reftable(1,2)='"Scalar Row1"'; reftable(2,2)='1.000000E+00'; reftable(3,2)='1.000000000000000E+00'
!   reftable(4,2)='1'; reftable(5,2)='1'; reftable(6,2)='T'; reftable(7,2)='"Scalar String"'
!   reftable(1,3)='"1-D testRow2"'; reftable(2,3)='"2.000000E+00" "3.000000E+00"'
!   reftable(3,3)='"2.000000000000000E+00" "3.000000000000000E+00"'; reftable(4,3)='"2" "3"'
!   reftable(5,3)='"2" "3"'; reftable(6,3)='"F" "T"'; reftable(7,3)='"test 1-D 1" "test 1-D 2"'
!   reftable(1,4)='"2-D testRow3" "Extra"'; reftable(2,4)='"4.000000E+00" "5.000000E+00" "6.000000E+00" "7.000000E+00"'
!   reftable(3,4)='"4.000000000000000E+00" "5.000000000000000E+00" "6.000000000000000E+00" "7.000000000000000E+00"'
!   reftable(4,4)='"4" "5" "6" "7"'; reftable(5,4)='"4" "5" "6" "7"'; reftable(6,4)='-'
!   reftable(7,4)='"2-D 1" "2-D 2" "2-D 3" "2-D 4"'
!   reftable(1,5)='"3-D testRow4"'; reftable(2,5)='"8.000000E+00" "9.000000E+00" "1.100000E+00"'// &
!       ' "1.200000E+00" "1.300000E+00" "1.400000E+00" "1.500000E+00" "1.600000E+00"'
!   reftable(3,5)='"8.000000000000000E+00" "9.000000000000000E+00" "1.100000000000000E+00" '// &
!       '"1.200000000000000E+00" "1.300000000000000E+00" "1.400000000000000E+00" '// &
!       '"1.500000000000000E+00" "1.600000000000000E+00"'
!   reftable(4,5)='"8" "9" "11" "12" "13" "14" "15" "16"'; reftable(5,5)='"8" "9" "11" "12" "13" "14" "15" "16"'

!   addr='TestPL->List1'
!   CALL testParam%convertTo2DStringArray(addr,table)
!   ASSERTFAIL(ALLOCATED(table),'valid addr call')
!   ASSERTFAIL(SIZE(table,DIM=1) == 7,'table dim=1')
!   ASSERTFAIL(SIZE(table,DIM=2) == 5,'table dim=2')

!   DO j=1,SIZE(table,DIM=2)
!     DO i=1,SIZE(table,DIM=1)
!       ASSERT(reftable(i,j) == table(i,j),'reftable')
!       FINFO() i,j,CHAR(table(i,j))
!     ENDDO
!   ENDDO
!   DEALLOCATE(table)

! ENDSUBROUTINE testConvertTo2DStringArray
! !
! !-------------------------------------------------------------------------------
! SUBROUTINE testGetNextParam()
!   INTEGER(SIK) :: n
!   TYPE(StringType) :: addr,refaddr(6),refname(6)
!   CLASS(ParamType),POINTER :: iParam

!   refaddr(1)='level 1'
!   refaddr(2)='level 1->level 2'
!   refaddr(3)='level 1->level 2->val 1'
!   refaddr(4)='level 1->level 2->level 3'
!   refaddr(5)='level 1->level 2->val 2'
!   refaddr(6)='level 1->val 3'

!   refname(1)='level 1'
!   refname(2)='level 2'
!   refname(3)='val 1'
!   refname(4)='level 3'
!   refname(5)='val 2'
!   refname(6)='val 3'

!   CALL testParam%add('level 1->level 2->val 1',1.0)
!   CALL testParam%add('level 1->val 3',3.0)
!   CALL testParam%add('level 1->level 2->level 3->dummy',0.0)
!   CALL testParam%remove('level 1->level 2->level 3->dummy')
!   CALL testParam%add('level 1->level 2->val 2',2.0)

!   n=0
!   addr=''
!   CALL testParam%getNextParam(addr,iParam)
!   DO WHILE(ASSOCIATED(iParam))
!     n=n+1
!     ASSERT(refaddr(n) == addr,'addr')
!     FINFO() n,'"'//TRIM(refaddr(n))//'" "'//TRIM(addr)//'"'
!     ASSERT(refname(n) == iParam%getName(),'addr')
!     FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%getName())//'"'
!     CALL testParam%getNextParam(addr,iParam)
!   ENDDO
!   ASSERT(n == 6,'number of iters')

!   !Root search with a paramtype pointer.
!   n=0
!   CALL testParam%get('level 1',someParam)
!   addr=''
!   CALL someParam%getNextParam(addr,iParam)
!   DO WHILE(ASSOCIATED(iParam))
!     n=n+1
!     ASSERT(refaddr(n) == addr,'addr')
!     FINFO() n,'"'//TRIM(refaddr(n))//'" "'//TRIM(addr)//'"'
!     ASSERT(refname(n) == iParam%getName(),'addr')
!     FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%getName())//'"'
!     CALL someParam%getNextParam(addr,iParam)
!   ENDDO
!   ASSERT(n == 6,'number of iters')

!   !search with a non root starting point
!   n=2
!   addr='level 1->level 2'
!   CALL testParam%getNextParam(addr,iParam)
!   DO WHILE(ASSOCIATED(iParam))
!     n=n+1
!     ASSERT(refaddr(n) == addr,'addr')
!     FINFO() n,'"'//TRIM(refaddr(n))//'" "'//TRIM(addr)//'"'
!     ASSERT(refname(n) == iParam%getName(),'addr')
!     FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%getName())//'"'
!     CALL testParam%getNextParam(addr,iParam)
!   ENDDO
!   ASSERT(n == 6,'number of iters')

!   !search for the last value in the list (Can't find out how to get to the uncovered code)
!   n=6
!   addr='level 1->val 3'
!   CALL testParam%getNextParam(addr,iParam)
!   DO WHILE(ASSOCIATED(iParam))
!     n=n+1
!     ASSERT(refaddr(n) == addr,'addr')
!     FINFO() n,'"'//TRIM(refaddr(n))//'" "'//TRIM(addr)//'"'
!     ASSERT(refname(n) == iParam%getName(),'addr')
!     FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%getName())//'"'
!     CALL testParam%getNextParam(addr,iParam)
!   ENDDO
!   ASSERT(n == 6,'number of iters')

!   CALL clear_test_vars()
! ENDSUBROUTINE testGetNextParam
! !
! !-------------------------------------------------------------------------------
! SUBROUTINE testGetSubParams()
!   INTEGER(SIK) :: n
!   TYPE(StringType) :: addr,refname(3)
!   CLASS(ParamType),POINTER :: iParam => NULL()

!   CALL testParam%add('level 1->level 2->val 1',1.0)
!   CALL testParam%add('level 1->val 3',3.0)
!   CALL testParam%add('level 1->level 2->level 3->dummy',0.0)
!   CALL testParam%add('level 1->level 2->val 2',2.0)

!   refname(1)='level 2'
!   refname(2)='val 3'

!   n=0
!   CALL testParam%get('level 1',someParam)
!   addr=''
!   CALL testParam%getSubParams(addr,iParam)
!   ASSERT(ASSOCIATED(iParam),'First Sub Param')
!   DO WHILE(ASSOCIATED(iParam))
!     n=n+1
!     ASSERT(refname(n) == TRIM(iParam%getName()),'name')
!     FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%getName())//'"'
!     CALL testParam%getSubParams(addr,iParam)
!   ENDDO
!   ASSERT(n == 2,'number of subParams')

!   refname(1)='level 2'
!   refname(2)='val 3'
!   n=0
!   CALL testParam%get('level 1',someParam)
!   addr='level 1'
!   CALL testParam%getSubParams(addr,iParam)
!   ASSERT(ASSOCIATED(iParam),'First Sub Param')
!   DO WHILE(ASSOCIATED(iParam))
!     n=n+1
!     ASSERT(refname(n) == TRIM(iParam%getName()),'name')
!     FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%getName())//'"'
!     CALL testParam%getSubParams(addr,iParam)
!   ENDDO
!   ASSERT(n == 2,'number of subParams')


!   n=0
!   refname(1)='val 1'
!   refname(2)='level 3'
!   refname(3)='val 2'
!   addr='level 1->level 2'
!   CALL testParam%getSubParams(addr,iParam)
!   ASSERT(ASSOCIATED(iParam),'First Sub Param')
!   DO WHILE(ASSOCIATED(iParam))
!     n=n+1
!     ASSERT(refname(n) == TRIM(iParam%getName()),'name')
!     FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%getName())//'"'
!     CALL testParam%getSubParams(addr,iParam)
!   ENDDO
!   ASSERT(n == 3,'number of subParams')

!   n=0
!   refname(1)='dummy'
!   addr='level 1->level 2->level 3'
!   CALL testParam%getSubParams(addr,iParam)
!   ASSERT(ASSOCIATED(iParam),'First Sub Param')
!   DO WHILE(ASSOCIATED(iParam))
!     n=n+1
!     ASSERT(refname(n) == TRIM(iParam%getName()),'name')
!     FINFO() n,'"'//TRIM(refname(n))//'" "'//TRIM(iParam%getName())//'"'
!     CALL testParam%getSubParams(addr,iParam)
!   ENDDO
!   ASSERT(n == 1,'number of subParams')

!   CALL clear_test_vars()
! ENDSUBROUTINE testGetSubParams
!
!-------------------------------------------------------------------------------
SUBROUTINE testGetSubPL()
  TYPE(ParamTypeIterator) :: iterator

  valstr=''
  CALL testParam%clear()

  COMPONENT_TEST('1st Level, no Root')
  CALL testParam%add('First param->Second level->p','p')
  CALL testParam%add('First param->a','a')
  CALL testParam%add('First param->Second list->b','b')

  CALL testParam%get('First param',someParam,.FALSE.)
  CALL iterator%activate(someParam,SUBLISTS=.FALSE.)
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'a','First param sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'Second level','First param sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'Second list','First param sublist')
  CALL iterator%advance()
  CALL iterator%clear()

  CALL testParam%get('First param -> Second list',someParam,.FALSE.)
  CALL iterator%activate(someParam,SUBLISTS=.FALSE.)
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'b','second list sublist')
  CALL iterator%clear()

  CALL testParam%get('Second list',someParam,.FALSE.)
  CALL iterator%activate(someParam,SUBLISTS=.FALSE.)
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'b','second list sublist')
  CALL iterator%clear()
  CALL someParam%clear()
  valstr = ''

  CALL clear_test_vars()

  valstr=''
  CALL testParam%clear()

  COMPONENT_TEST('All Levels, no Root')
  CALL testParam%add('First param->Second level->p','p')
  CALL testParam%add('First param->a','a')
  CALL testParam%add('First param->Second list->b','b')

  CALL testParam%get('First param',someParam,.FALSE.)
  CALL iterator%activate(someParam,SUBLISTS=.TRUE.)
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'a','First param sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'Second level','First param sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'Second level->p','First param sublist')
  valstr = iterator%getCurrentName(.TRUE.)
  ASSERT_EQ(CHAR(valstr),'Second level->p','First param sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'Second list','First param sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'Second list->b','First param sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'','First param sublist')
  CALL iterator%clear()

  CALL testParam%get('First param -> Second list',someParam,.FALSE.)
  CALL iterator%activate(someParam,SUBLISTS=.TRUE.)
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'b','second list sublist')
  CALL iterator%clear()

  CALL testParam%get('Second list',someParam,.FALSE.)
  CALL iterator%activate(someParam,SUBLISTS=.TRUE.)
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'b','second list sublist')
  CALL iterator%clear()
  CALL someParam%clear()
  valstr = ''

  CALL clear_test_vars()

  valstr=''
  CALL testParam%clear()

  COMPONENT_TEST('1st Level, Root')
  CALL testParam%add('First param->Second level->p','p')
  CALL testParam%add('First param->a','a')
  CALL testParam%add('First param->Second list->b','b')

  CALL testParam%get('First param',someParam)
  CALL iterator%activate(someParam,SUBLISTS=.FALSE.)
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'First param','First param sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'','First param sublist')
  CALL iterator%clear()

  CALL testParam%get('First param -> Second list',someParam)
  CALL iterator%activate(someParam,SUBLISTS=.FALSE.)
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'Second list','second list sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'','First param sublist')
  CALL iterator%clear()

  CALL testParam%get('Second list',someParam)
  CALL iterator%activate(someParam,SUBLISTS=.FALSE.)
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'Second list','second list sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'','First param sublist')
  CALL iterator%clear()
  CALL someParam%clear()
  valstr = ''

  CALL clear_test_vars()

  valstr=''
  CALL testParam%clear()

  COMPONENT_TEST('All levels, Root')
  CALL testParam%add('First param->Second level->p','p')
  CALL testParam%add('First param->a','a')
  CALL testParam%add('First param->Second list->b','b')

  CALL testParam%get('First param',someParam)
  CALL iterator%activate(someParam,SUBLISTS=.TRUE.)
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'First param->a','First param sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'First param->Second level','First param sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'First param->Second level->p','First param sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'First param->Second list','First param sublist')
  CALL iterator%advance()
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'First param->Second list->b','First param sublist')
  CALL iterator%advance()
  CALL iterator%clear()

  CALL testParam%get('First param -> Second list',someParam)
  CALL iterator%activate(someParam,SUBLISTS=.TRUE.)
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'Second list->b','second list sublist')
  CALL iterator%clear()

  CALL testParam%get('Second list',someParam)
  CALL iterator%activate(someParam,SUBLISTS=.TRUE.)
  valstr = iterator%getCurrentName()
  ASSERT_EQ(CHAR(valstr),'Second list->b','second list sublist')
  CALL iterator%clear()
  CALL someParam%clear()
  valstr = ''

  CALL clear_test_vars()
ENDSUBROUTINE testGetSubPL
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
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::validate -'// &
      ' Failed to locate required parameter "TestReq->p2"!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'testReq->p2 validate')

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
  refmsg='#### EXCEPTION_WARNING #### - PARAMETERLISTS::validate -'// &
      ' Optional parameter "TestReq->sublist1->sublist2->sublist3->opt3" has type'// &
      ' "REAL(SDK)" and should be type "REAL(SSK)"!  It is being overridden with default value.'
  ASSERT_EQ(TRIM(msg), TRIM(refmsg),'TestReq->sublist1->sublist2->sublist3->opt3 SSK validate optional input')

  !Finds a meaningful REQ parameter, returns an associated parameter of a different type (lines 1337-1340)
  CALL testParam%add('TestReq->p6',6.0_SSK)
  CALL testParam2%add('TestReq->p6',6_SNK)
  CALL testParam%validate(testParam2,testParam3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::validate -'// &
      ' Required parameter "TestReq->p6" has type "REAL(SSK)" and must be type "INTEGER(SNK)"!'
  ASSERT_EQ(TRIM(msg), TRIM(refmsg),'TestReq->p6 SNK validate required input')
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
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::validate -'// &
      ' Failed to locate required parameter "TestReq->p2->2far"!'
  ASSERT_EQ(TRIM(msg), TRIM(refmsg),'TestReq validate failed to locate')

  !Since an empty req param list exists, add a SLK parameter to test param
  !so they aren't the same type  (lines 1319-1323)
  CALl testParam%remove('TestReq->p2->2far')
  CALL testParam%add('TestReq->p2->2far',6_SLK)
  CALL testParam%validate(testParam2,testParam3)
  msg=eParams%getLastMessage()
  refmsg='#### EXCEPTION_ERROR #### - PARAMETERLISTS::validate -'// &
      ' Required parameter "TestReq->p2->2far" has type "INTEGER(SLK)" and must be type "TYPE(ParamType)"!'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'TestReq->p2->2far validate different type, PList')

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
  refmsg='#### EXCEPTION_WARNING #### - PARAMETERLISTS::validate -'// &
      ' Optional parameter "TestOpt->p2->2far->veryfar" has type "REAL(SSK)" and'// &
      ' should be type "TYPE(ParamType)"!  It is being overridden with default value.'
  ASSERT_EQ(TRIM(msg),TRIM(refmsg),'TestReq->p2->2far validate different type, PList')

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

  bool = testParam%verify(testParam2,ASSERT=.TRUE.)
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
  bool = testParam%verify(testParam2,ASSERT=.TRUE.)
  ASSERT(bool,'verify the copy')

  CALL testParam2%clear()
  bool = testParam%verify(testParam2,ASSERT=.TRUE.)
  ASSERT(bool,'arg uninit')
  bool = testParam2%verify(testParam,ASSERT=.FALSE.)
  ASSERT(.NOT.bool,'this uninit')

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
! !
! !-------------------------------------------------------------------------------
! SUBROUTINE testVerifyList()
!   LOGICAL(SBK) :: bool
!   INTEGER(SIK) :: nerror
!   INTEGER(SNK),ALLOCATABLE :: snk2(:,:),snk3(:,:,:)
!   INTEGER(SLK),ALLOCATABLE :: slk2(:,:),slk3(:,:,:)
!   REAL(SSK),ALLOCATABLE :: ssk2(:,:),ssk3(:,:,:)
!   REAL(SDK),ALLOCATABLE :: sdk2(:,:),sdk3(:,:,:)
!   TYPE(StringType) :: str0
!   TYPE(StringType),ALLOCATABLE :: str1(:),str2(:,:)
!   TYPE(ExceptionHandlerType) :: tmpe

!   CALL testParam%verifyList(testParam2,bool,e)
!   ASSERT(bool,'empty lists')

!   !Testing addition of SBK routine to parameter list
!   CALL testParam%add('testPL->testSBK',.TRUE.)
!   CALL testParam%add('testPL->testSBK2',.FALSE.,'comment')

!   !Testing addition of SSK routine to parameter list
!   CALL testParam%add('testPL->testSSK',7.0_SSK)
!   CALL testParam%add('testPL->testSSK2',8.0_SSK,'comment')

!   !Testing addition of SDK routine to parameter list
!   CALL testParam%add('testPL->testSDK',7.0_SDK)
!   CALL testParam%add('testPL->testSDK2',8.0_SDK,'comment')

!   !Testing addition of SNK routine to parameter list
!   CALL testParam%add('testPL->testSNK',7_SNK)
!   CALL testParam%add('testPL->testSNK2',8_SNK,'comment')

!   !Testing addition of SLK routine to parameter list
!   CALL testParam%add('testPL->testSLK',7_SLK)
!   CALL testParam%add('testPL->testSLK2',8_SLK,'comment')

!   !Testing addition of STR routine to parameter list
!   str0='string1'
!   CALL testParam%add('testPL->testSTR',str0)
!   str0='string2'
!   CALL testParam%add('testPL->testSTR2',str0,'comment')

!   !Testing addition of CHAR routine to parameter list
!   CALL testParam%add('testPL->testCHAR','char1')
!   CALL testParam%add('testPL->testCHAR2','char2','comment')

!   !Testing addition of 1-D array SSK routine to parameter list
!   CALL testParam%add('testPL->testSSKa1',(/1.5_SSK,1.6_SSK/))
!   CALL testParam%add('testPL->testSSKa1_2',(/1.7_SSK,1.8_SSK/),'comment')

!   !Testing addition of 1-D array SDK routine to parameter list
!   CALL testParam%add('testPL->testSDKa1',(/2.5_SDK,2.6_SDK/))
!   CALL testParam%add('testPL->testSDKa1_2',(/2.7_SDK,2.8_SDK/),'comment')

!   !Testing addition of 1-D array SNK routine to parameter list
!   CALL testParam%add('testPL->testSNKa1',(/-2_SNK,-3_SNK/))
!   CALL testParam%add('testPL->testSNKa1_2',(/2_SNK,3_SNK/),'comment')

!   !Testing addition of 1-D array SLK routine to parameter list
!   CALL testParam%add('testPL->testSLKa1',(/-4_SLK,-5_SLK/))
!   CALL testParam%add('testPL->testSLKa1_2',(/4_SLK,5_SLK/),'comment')

!   !Testing addition of 1-D array SBK routine to parameter list
!   CALL testParam%add('testPL->testSBKa1',(/.TRUE.,.FALSE./))
!   CALL testParam%add('testPL->testSBKa1_2',(/.FALSE.,.FALSE./),'comment')

!   !Testing addition of 1-D array STR routine to parameter list
!   ALLOCATE(str1(2))
!   str1(1)='stringarray1'
!   str1(2)='stringarray2'
!   CALL testParam%add('testPL->testSTRa1',str1)
!   str1(1)='stringarray3'
!   str1(2)='stringarray4'
!   CALL testParam%add('testPL->testSTRa1_2',str1,'comment')

!   !Testing addition of 2-D array SSK routine to parameter list
!   ALLOCATE(ssk2(2,2))
!   ssk2(1,1)=1.1_SSK
!   ssk2(2,1)=2.1_SSK
!   ssk2(1,2)=1.2_SSK
!   ssk2(2,2)=2.2_SSK
!   CALL testParam%add('testPL->testSSKa2',ssk2)
!   ssk2(1,1)=3.1_SSK
!   ssk2(2,1)=4.1_SSK
!   ssk2(1,2)=3.2_SSK
!   ssk2(2,2)=4.2_SSK
!   CALL testParam%add('testPL->testSSKa2_2',ssk2,'comment')

!   !Testing addition of 2-D array SDK routine to parameter list
!   ALLOCATE(sdk2(2,2))
!   sdk2(1,1)=11.0_SDK
!   sdk2(2,1)=21.0_SDK
!   sdk2(1,2)=12.0_SDK
!   sdk2(2,2)=22.0_SDK
!   CALL testParam%add('testPL->testSDKa2',sdk2)
!   sdk2(1,1)=31.0_SDK
!   sdk2(2,1)=41.0_SDK
!   sdk2(1,2)=32.0_SDK
!   sdk2(2,2)=42.0_SDK
!   CALL testParam%add('testPL->testSDKa2_2',sdk2,'comment')

!   !Testing addition of 2-D array SNK routine to parameter list
!   ALLOCATE(snk2(2,2))
!   snk2(1,1)=11
!   snk2(2,1)=21
!   snk2(1,2)=12
!   snk2(2,2)=22
!   CALL testParam%add('testPL->testSNKa2',snk2)
!   snk2(1,1)=31
!   snk2(2,1)=41
!   snk2(1,2)=32
!   snk2(2,2)=42
!   CALL testParam%add('testPL->testSNKa2_2',snk2,'comment')

!   !Testing addition of 2-D array SLK routine to parameter list
!   ALLOCATE(slk2(2,2))
!   slk2(1,1)=110
!   slk2(2,1)=210
!   slk2(1,2)=120
!   slk2(2,2)=220
!   CALL testParam%add('testPL->testSLKa2',slk2)
!   slk2(1,1)=310
!   slk2(2,1)=410
!   slk2(1,2)=320
!   slk2(2,2)=420
!   CALL testParam%add('testPL->testSLKa2_2',slk2,'comment')

!   !Testing addition of 2-D array STR routine to parameter list
!   ALLOCATE(str2(2,2))
!   str2(1,1)='stringarray1'
!   str2(2,1)='stringarray2'
!   str2(1,2)='stringarray3'
!   str2(2,2)='stringarray4'
!   CALL testParam%add('testPL->testSTRa2',str2)
!   str2(1,1)='stringarray5'
!   str2(2,1)='stringarray6'
!   str2(1,2)='stringarray7'
!   str2(2,2)='stringarray8'
!   CALL testParam%add('testPL->testSTRa2_2',str2,'comment')

!   !Testing addition of 3-D array SSK routine to parameter list
!   ALLOCATE(ssk3(2,2,2))
!   ssk3(1,1,1)=1.11_SSK
!   ssk3(2,1,1)=2.11_SSK
!   ssk3(1,2,1)=1.21_SSK
!   ssk3(2,2,1)=2.21_SSK
!   ssk3(1,1,2)=1.12_SSK
!   ssk3(2,1,2)=2.12_SSK
!   ssk3(1,2,2)=1.22_SSK
!   ssk3(2,2,2)=2.22_SSK
!   CALL testParam%add('testPL->testSSKa3',ssk3)
!   ssk3(1,1,1)=3.11_SSK
!   ssk3(2,1,1)=4.11_SSK
!   ssk3(1,2,1)=3.21_SSK
!   ssk3(2,2,1)=4.21_SSK
!   ssk3(1,1,2)=3.12_SSK
!   ssk3(2,1,2)=4.12_SSK
!   ssk3(1,2,2)=3.22_SSK
!   ssk3(2,2,2)=4.22_SSK
!   CALL testParam%add('testPL->testSSKa3_2',ssk3,'comment')

!   !Testing addition of 3-D array SDK routine to parameter list
!   ALLOCATE(sdk3(2,2,2))
!   sdk3(1,1,1)=11.1_SDK
!   sdk3(2,1,1)=21.1_SDK
!   sdk3(1,2,1)=12.1_SDK
!   sdk3(2,2,1)=22.1_SDK
!   sdk3(1,1,2)=11.2_SDK
!   sdk3(2,1,2)=21.2_SDK
!   sdk3(1,2,2)=12.2_SDK
!   sdk3(2,2,2)=22.2_SDK
!   CALL testParam%add('testPL->testSDKa3',sdk3)
!   sdk3(1,1,1)=31.1_SDK
!   sdk3(2,1,1)=41.1_SDK
!   sdk3(1,2,1)=32.1_SDK
!   sdk3(2,2,1)=42.1_SDK
!   sdk3(1,1,2)=31.2_SDK
!   sdk3(2,1,2)=41.2_SDK
!   sdk3(1,2,2)=32.2_SDK
!   sdk3(2,2,2)=42.2_SDK
!   CALL testParam%add('testPL->testSDKa3_2',sdk3,'comment')

!   !Testing addition of 3-D array SNK routine to parameter list
!   ALLOCATE(snk3(2,2,2))
!   snk3(1,1,1)=111
!   snk3(2,1,1)=211
!   snk3(1,2,1)=121
!   snk3(2,2,1)=221
!   snk3(1,1,2)=112
!   snk3(2,1,2)=212
!   snk3(1,2,2)=122
!   snk3(2,2,2)=222
!   CALL testParam%add('testPL->testSNKa3',snk3)
!   snk3(1,1,1)=311
!   snk3(2,1,1)=411
!   snk3(1,2,1)=321
!   snk3(2,2,1)=421
!   snk3(1,1,2)=312
!   snk3(2,1,2)=412
!   snk3(1,2,2)=322
!   snk3(2,2,2)=422
!   CALL testParam%add('testPL->testSNKa3_2',snk3,'comment')

!   !Testing addition of 3-D array SLK routine to parameter list
!   ALLOCATE(slk3(2,2,2))
!   slk3(1,1,1)=111
!   slk3(2,1,1)=211
!   slk3(1,2,1)=121
!   slk3(2,2,1)=221
!   slk3(1,1,2)=112
!   slk3(2,1,2)=212
!   slk3(1,2,2)=122
!   slk3(2,2,2)=222
!   CALL testParam%add('testPL->testSLKa3',slk3)
!   slk3(1,1,1)=311
!   slk3(2,1,1)=411
!   slk3(1,2,1)=321
!   slk3(2,2,1)=421
!   slk3(1,1,2)=312
!   slk3(2,1,2)=412
!   slk3(1,2,2)=322
!   slk3(2,2,2)=422
!   CALL testParam%add('testPL->testSLKa3_2',slk3,'comment')

!   !assign the same PL
!   !Test a dummy exception handler
!   tmpe=e
!   testParam2=testParam
!   CALL testParam%verifyList(testParam2,bool,tmpe)
!   ASSERT(bool,'verify the copy')

!   nerror=tmpe%getCounter(EXCEPTION_ERROR)
!   CALL testParam2%clear()
!   CALL testParam%verifyList(testParam2,bool,tmpe)
!   ASSERT(.NOT.bool,'arg uninit')
!   ASSERT(nerror == tmpe%getCounter(EXCEPTION_ERROR),'arg uninit')
!   nerror=tmpe%getCounter(EXCEPTION_ERROR)
!   CALL testParam2%verifyList(testParam,bool,tmpe)
!   ASSERT(.NOT.bool,'this uninit')
!   ASSERT(nerror+44 == tmpe%getCounter(EXCEPTION_ERROR),'this uninit')

!   !assign the same PL
!   !Test using the eParams exception handler
!   testParam2=testParam
!   CALL testParam%verifyList(testParam2,bool,eParams)
!   ASSERT(bool,'verify the copy')

!   nerror=eParams%getCounter(EXCEPTION_ERROR)
!   CALL testParam2%clear()
!   CALL testParam%verifyList(testParam2,bool,eParams)
!   ASSERT(.NOT.bool,'arg uninit')
!   ASSERT(nerror == eParams%getCounter(EXCEPTION_ERROR),'arg uninit')
!   nerror=eParams%getCounter(EXCEPTION_ERROR)
!   CALL testParam2%verifyList(testParam,bool,eParams)
!   ASSERT(.NOT.bool,'this uninit')
!   ASSERT_EQ(nerror+44, eParams%getCounter(EXCEPTION_ERROR),'this uninit')

!   !Deallocate locals
!   DEALLOCATE(snk2)
!   DEALLOCATE(snk3)
!   DEALLOCATE(slk2)
!   DEALLOCATE(slk3)
!   DEALLOCATE(ssk2)
!   DEALLOCATE(ssk3)
!   DEALLOCATE(sdk2)
!   DEALLOCATE(sdk3)
!   DEALLOCATE(str1)
!   DEALLOCATE(str2)

!   CALL clear_test_vars()
! ENDSUBROUTINE testVerifyList
!
!-------------------------------------------------------------------------------
!Test to make sure we've eliminated a long standing bug.
! SUBROUTINE testPartialMatch()

!   CALL testParam%clear()
!   CALL testParam%add('List->sublist->a',1)
!   ASSERT(testParam%has('a'),'partial match no list')
!   ASSERT(.NOT.testParam%has('List->a'),'no partial match')
!   ASSERT(testParam%has('sublist->a'),'partial sublist match')

!   CALL testParam%get('List',someParam)
!   ASSERT(someParam%has('a'),'partial match no list (pointer)')
!   ASSERT(.NOT.someParam%has('List->a'),'no partial match (pointer)')
!   ASSERT(someParam%has('sublist->a'),'partial sublist match (pointer)')

!   CALL testParam%clear()
! ENDSUBROUTINE testPartialMatch
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
  CALL testParam3%add('name','dummy')
  CALL testParam2%add('CASEID->ASSEMBLIES->ASSEMBLY_ASSY1->SpacerGrids',testParam3)
  CALL testParam3%clear()
  CALL testParam2%remove('CASEID->ASSEMBLIES->ASSEMBLY_ASSY1->SpacerGrids->name')
  str='ASSY1'
  CALL testParam2%add('CASEID->ASSEMBLIES->Assembly_ASSY1->label',str)

  CALL testParam%clear()
  CALL testParam%initFromXML('testInit.xml')
  bool = testParam%verify(testParam2,ASSERT=.FALSE.)
  ASSERT(bool,"init from XML file")

  CALL testParam%clear()
  CALL testParam2%clear()
ENDSUBROUTINE testInitFromXML
!
!-------------------------------------------------------------------------------
#ifdef FUTILITY_HAVE_Trilinos
! SUBROUTINE testConvertTeuchos()
!   TYPE(ParamType) :: params
!   TYPE(ForTeuchos_ParameterList_ID) :: teuchos_plist
!   INTEGER(C_INT) :: ierr
!   INTEGER(C_INT) :: int
!   REAL(SDK) :: float
!   LOGICAL(SBK) :: bool
!   CHARACTER(LEN=1024) :: string
!   TYPE(StringType) :: strtype


!   CALL params%add("test_params->some_int", 5_SNK)
!   CALL params%add("test_params->some_double", 3.14_SDK)
!   CALL params%add("test_params->some_string", "fa la la la la!")
!   CALL params%add("test_params->some_level->data_int", 4_SNK)
!   CALL params%add("test_params->some_level->data_int2", 8_SNK)
!   CALL params%add("test_params->more_lower_stuff", 8.7_SDK)
!   CALL params%add("test_params->look_a_bool", .true.)
!   CALL params%add("test_params->a_false_bool", .false.)


!   teuchos_plist = Teuchos_ParameterList_Create(ierr)
!   CALL params%toTeuchosPlist(teuchos_plist)
!   CALL Teuchos_ParameterList_print(teuchos_plist, ierr)

!   int = ForTeuchos_PL_get_int(teuchos_plist, "some_int", ierr);
!   ASSERT(int==5,"some_int")

!   int = ForTeuchos_PL_get_int(&
!       ForTeuchos_PL_sublist_existing(teuchos_plist, "some_level", ierr),&
!       "data_int", ierr)
!   ASSERT(int==4,"some_level->data_int")
!   FINFO()int,ierr

!   float = ForTeuchos_PL_get_double(teuchos_plist, "some_double", ierr)
!   ASSERT(float == 3.14_SDK, "some_double")

!   CALL ForTeuchos_PL_get_string(teuchos_plist, "some_string", string, ierr)
!   strtype = TRIM(string)
!   ASSERT(strtype=="fa la la la la!", "some_string")

!   bool = ForTeuchos_PL_get_bool(teuchos_plist, "look_a_bool", ierr)
!   ASSERT(bool,"bool")

!   bool = ForTeuchos_PL_get_bool(teuchos_plist, "a_false_bool", ierr)
!   ASSERT(.not.bool,"bool")


! ENDSUBROUTINE testConvertTeuchos
#endif
!
!-------------------------------------------------------------------------------
SUBROUTINE testEditToXML()
  LOGICAL(SBK) :: bool
  CALL testParam%initFromXML('testInit.xml')
CALL testParam%edit(6)
  CALL testParam%editToXML('testEdit.xml')
  CALL testParam2%initFromXML('testEdit.xml')
CALL testparam2%edit(6)
  bool = testParam2%verify(testParam)
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

ENDSUBROUTINE clear_test_vars
!
!-------------------------------------------------------------------------------
SUBROUTINE testCopy()
  TYPE(ParamType) :: listA,listB

  CALL listA%add('A -> A1 -> A1A',1.0_SRK)
  CALL listA%add('A -> A1 -> A1B',2.0_SRK)
  CALL listA%add('A -> A2 -> A2A',3.0_SRK)
  CALL listA%add('A -> A2 -> A2B',4.0_SRK)
  CALL listA%add('A -> A3',5.0_SRK)
  CALL listA%add('B -> B1 -> B1A',6.0_SRK)

  COMPONENT_TEST('Copy single param')
  CALL listB%add('C -> C1',1_SNK)
  CALL listB%add('C -> C2',1_SNK)
  CALL listB%copyParam('C -> C1','C1',listA)
  CALL listB%clear()
  ASSERT(listA%has('C1'),'copied C1')

  CALL listB%add('C -> A1',1_SNK)
  CALL listB%copyParam('C -> A1','A -> A1 -> A1A',listA)
  ASSERT_EQ(listA%getDataType('A -> A1 -> A1A'),PL_DATA_TYPE_SDK0,'failed overwrite')
  CALL listB%copyParam('C -> A1','A -> A1 -> A1A',listA,OVERWRITE=.TRUE.)
  CALL listB%clear()
  ASSERT_EQ(listA%getDataType('A -> A1 -> A1A'),PL_DATA_TYPE_SNK0,'overwrite')

  COMPONENT_TEST('Copy list')
  CALL listB%add('C -> C1',1_SNK)
  CALL listB%add('C -> C2',1_SNK)
  CALL listB%copyParam('C','copy -> C',listA)
  CALL listB%clear()
  ASSERT(listA%has('copy -> C'),'copied copy -> C')
  ASSERT(listA%has('copy -> C -> C1'),'copied copy -> C -> C1')
  ASSERT(listA%has('copy -> C -> C2'),'copied copy -> C -> C2')

  CALL listB%add('1 -> 2 -> 3 -> 4 -> 5 -> 6A',5.0_SRK)
  CALL listB%add('1 -> 2 -> 3 -> 4 -> 5 -> 6B',5.0_SRK)
  CALL listB%copyParam('1 -> 2 -> 3','copy -> 1',listA)

  COMPONENT_TEST('Null copy')
  CALL listB%add('C -> C1',1_SNK)
  CALL listB%add('C -> C2',1_SNK)
  CALL listB%copyParam('B','copy -> B',listA)
  CALL listB%clear()
  ASSERT(.NOT.listA%has('copy -> B'),'null copy')

  COMPONENT_TEST('overwrite copy')
  CALL listB%add('C -> C3',2_SLK)
  CALL listB%add('C -> C2',2_SLK)
  CALL listB%copyParam('C','copy -> C',listA)
  ASSERT(.NOT.listA%has('copy -> C -> C3'),'copied copy -> C -> C3')
  CALL listB%copyParam('C','copy -> C',listA,OVERWRITE=.TRUE.)
  CALL listB%clear()
  ASSERT(listA%has('copy -> C'),'copied copy -> C')
  ASSERT(listA%has('copy -> C -> C1'),'copied copy -> C -> C1')
  ASSERT(listA%has('copy -> C -> C3'),'copied copy -> C -> C3')
  ASSERT(listA%has('copy -> C -> C2'),'copied copy -> C -> C2')
  ASSERT_EQ(listA%getDataType('copy -> C -> C2'),PL_DATA_TYPE_SLK0,'copy overwrite')

  CALL listA%clear()
  CALL listB%clear()

ENDSUBROUTINE testCopy
!
!-------------------------------------------------------------------------------
SUBROUTINE testPerformance()
    INTEGER(SIK),PARAMETER :: nrand=10000000
    INTEGER(SIK) :: index,i
    INTEGER(SIK),ALLOCATABLE :: random_paths(:)
    INTEGER(SLK),ALLOCATABLE :: testSLK(:)
    REAL(SRK) :: rand
    CHARACTER(LEN=128) :: tmpchar
    TYPE(StringType),ALLOCATABLE :: paths(:)
    TYPE(TimerType) :: timer

    CALL timer%setTimerName('testPLs')

    COMPONENT_TEST('initFromXML')
    CALL timer%tic()
    CALL testParam%initFromXML('performance_test.xml')
    ! CALL testParam%edit(950)
    CALL timer%toc()
    WRITE(*,*) 'time='//timer%getTimeHHMMSS()

    COMPONENT_TEST('buildRandomAccess get')
    CALL timer%tic()
    OPEN(UNIT=100,FILE='performance_test_pl_paths.txt')
    ALLOCATE(paths(173662))
    DO i=1,173662
      READ(UNIT=100,FMT='(a)') tmpchar
      paths(i)=tmpchar
    ENDDO !i
    ALLOCATE(random_paths(nrand))
    DO i=1,nrand
      CALL RANDOM_NUMBER(rand)
      random_paths(i)=CEILING(rand*173662)
    ENDDO !i
    CALL timer%toc()
    WRITE(*,*) 'time='//timer%getTimeHHMMSS()

    COMPONENT_TEST('random get')
    CALL timer%tic()
    DO i=1,nrand
      CALL testParam%get(CHAR(paths(random_paths(i))),testSLK)
    ENDDO !i
    CALL timer%toc()
    WRITE(*,*) 'time='//timer%getTimeHHMMSS()

    COMPONENT_TEST('buildRandomAccess set')
    CALL timer%tic()
    DO i=1,nrand
      CALL RANDOM_NUMBER(rand)
      random_paths(i)=CEILING(rand*173662)
    ENDDO !i
    CALL timer%toc()
    WRITE(*,*) 'time='//timer%getTimeHHMMSS()

    COMPONENT_TEST('random set')
    CALL timer%tic()
    testSLK=[1, 2, 3, 4, 5]
    DO i=1,nrand
      CALL testParam%set(CHAR(paths(random_paths(i))),testSLK)
    ENDDO !i
    CALL timer%toc()
    WRITE(*,*) 'time='//timer%getTimeHHMMSS()

    COMPONENT_TEST('buildRandomAccess add')
    CALL timer%tic()
    DO i=1,nrand
      CALL RANDOM_NUMBER(rand)
      random_paths(i)=CEILING(rand*173662)
    ENDDO !i
    CALL timer%toc()
    WRITE(*,*) 'time='//timer%getTimeHHMMSS()

    COMPONENT_TEST('random add')
    CALL timer%tic()
    testSLK=[1, 2, 3, 4, 5]
    DO i=1,nrand
      CALL testParam%add(paths(random_paths(i))//str(i),testSLK)
      IF(MOD(i,nrand/10) == 0) THEN
        CALL timer%toc()
        WRITE(*,*) 'intermittent time='//timer%getTimeHHMMSS(),i
        CALL timer%tic()
      ENDIF
    ENDDO !i
    CALL timer%toc()
    WRITE(*,*) 'time='//timer%getTimeHHMMSS()

    COMPONENT_TEST('edit random adds')
    CALL timer%tic()
    CALL testParam%edit(999)
    CALL timer%toc()
    WRITE(*,*) 'time='//timer%getTimeHHMMSS()

  ENDSUBROUTINE testPerformance
  !
  !-------------------------------------------------------------------------------
  SUBROUTINE setupHDF5testFile()
    TYPE(HDF5FileType) :: h5
    INTEGER(SIK) :: i,j,k,m,n,p,q,z

    ALLOCATE(refsdka1(10))
    ALLOCATE(refsska1(10))
    ALLOCATE(refslka1(10))
    ALLOCATE(refsnka1(10))
    ALLOCATE(refsnka2(4,5))
    ALLOCATE(refslka2(4,5))
    ALLOCATE(refsska2(4,5))
    ALLOCATE(refsdka2(4,5))
    ALLOCATE(refsdka3(3,4,5))
    ALLOCATE(refsska3(3,4,5))
    ALLOCATE(refslka3(3,4,5))
    ALLOCATE(refsnka3(3,4,5))
    ALLOCATE(refsnka4(3,4,5,6))
    ALLOCATE(refslka4(3,4,5,6))
    ALLOCATE(refsska4(3,4,5,6))
    ALLOCATE(refsdka4(3,4,5,6))
    ALLOCATE(refsdka5(3,4,5,6,7))
    ALLOCATE(refsska5(3,4,5,6,7))
    ALLOCATE(refslka5(3,4,5,6,7))
    ALLOCATE(refsnka5(3,4,5,6,7))
    ALLOCATE(refsnka6(3,4,5,6,7,8))
    ALLOCATE(refslka6(3,4,5,6,7,8))
    ALLOCATE(refsska6(3,4,5,6,7,8))
    ALLOCATE(refsdka6(3,4,5,6,7,8))
    ALLOCATE(refsdka7(3,4,5,6,7,8,9))
    ALLOCATE(refsska7(3,4,5,6,7,8,9))
    ALLOCATE(refslka7(3,4,5,6,7,8,9))
    ALLOCATE(refsnka7(3,4,5,6,7,8,9))

    refsdk=42.123456789_SDK
    refssk=42.000_SSK
    refslk=12345678912345678_SLK
    refsnk=42_SNK
    DO i = 1,10
      refsdka1(i) = i*1.0000000001_SDK
      refsska1(i) = i
      refslka1(i) = i+1000000000
      refsnka1(i) = i
    ENDDO !i
    i = 0
    DO j = 1,5
      DO k = 1,4
        i = i+1
        refsnka2(k,j) = i
        refslka2(k,j) = i+1000000000
        refsska2(k,j) = i
        refsdka2(k,j) = i*1.0000000001_SDK
      ENDDO !k
    ENDDO !j
    i = 0
    DO m = 1,5
      DO j = 1,4
        DO k = 1,3
          i = i+1
          refsdka3(k,j,m) = i*1.0000000001_SDK
          refsska3(k,j,m) = i
          refslka3(k,j,m) = i+1000000000
          refsnka3(k,j,m) = i
        ENDDO !k
      ENDDO !j
    ENDDO !m
    i = 0
    DO n = 1,6
      DO m = 1,5
        DO j = 1,4
          DO k = 1,3
            i = i+1
            refsnka4(k,j,m,n) = i
            refslka4(k,j,m,n) = i+1000000000
            refsska4(k,j,m,n) = i
            refsdka4(k,j,m,n) = i*1.0000000001_SDK
          ENDDO !k
        ENDDO !j
      ENDDO !m
    ENDDO !n
    i = 0
    DO p = 1,7
      DO n = 1,6
        DO m = 1,5
          DO j = 1,4
            DO k = 1,3
              i = i+1
              refsdka5(k,j,m,n,p) = i*1.0000000001_SDK
              refsska5(k,j,m,n,p) = i
              refslka5(k,j,m,n,p) = i+1000000000
              refsnka5(k,j,m,n,p) = i
            ENDDO !k
          ENDDO !j
        ENDDO !m
      ENDDO !n
    ENDDO !p
    i = 0
    DO q = 1,8
      DO p = 1,7
        DO n = 1,6
          DO m = 1,5
            DO j = 1,4
              DO k = 1,3
                i = i+1
                refsnka6(k,j,m,n,p,q) = i
                refslka6(k,j,m,n,p,q) = i+1000000000
                refsska6(k,j,m,n,p,q) = i
                refsdka6(k,j,m,n,p,q) = i*1.0000000001_SDK
              ENDDO !k
            ENDDO !j
          ENDDO !m
        ENDDO !n
      ENDDO !p
    ENDDO !q
    i = 0
    DO z = 1,9
      DO q = 1,8
        DO p = 1,7
          DO n = 1,6
            DO m = 1,5
              DO j = 1,4
                DO k = 1,3
                  i = i+1
                  refsdka7(k,j,m,n,p,q,z) = i*1.0000000001_SDK
                  refsska7(k,j,m,n,p,q,z) = i
                  refslka7(k,j,m,n,p,q,z) = i+1000000000
                  refsnka7(k,j,m,n,p,q,z) = i
                ENDDO !k
              ENDDO !j
            ENDDO !m
          ENDDO !n
        ENDDO !p
      ENDDO !q
    ENDDO !z

    ALLOCATE(refstra1(3))
    ALLOCATE(refstra2(2,3))
    ALLOCATE(refstra3(3,4,5))
    refstr = 'Rank-0 (Not-an-array) String Test'
    DO i=1,SIZE(refstra1)
      refstra1(i) = 'String '//str(i)
    ENDDO !i
    DO i = 1,SIZE(refstra2,DIM=1)
      DO j = 1,SIZE(refstra2,DIM=2)
        refstra2(i,j) = 'String '//str(i)//','//str(j)
      ENDDO !j
    ENDDO !j
    refstra2(1,1)='String 1,1';refstra2(1,2)='String 1,2';refstra2(2,1)='String 2,1'
    refstra2(2,2)='String 2,2';
    DO i=1,SIZE(refstra3,1)
      DO j=1,SIZE(refstra3,2)
        DO k=1,SIZE(refstra3,3)
          refstra3(i,j,k) = 'String '//str(i)//','//str(j)//','//str(k)
        ENDDO !k
      ENDDO !j
    ENDDO !i

    ALLOCATE(refsbka1(6))
    ALLOCATE(refsbka2(6,5))
    ALLOCATE(refsbka3(6,5,2))
    refsbk=.FALSE.
    refsbka1(:)=.TRUE.; refsbka2(:,:)=.TRUE.; refsbka3(:,:,:)=.TRUE.
    DO i=1,SIZE(refsbka1),2
      refsbka1(i)=.FALSE.
      DO j=1,SIZE(refsbka2,DIM=2),3
        refsbka2(i,j)=.FALSE.
        DO k=1,SIZE(refsbka3,DIM=3)
          refsbka3(i,j,k)=.FALSE.
        ENDDO !k
      ENDDO !j
    ENDDO !i

    !write the valserence file for the read side
    CALL h5%init('readtest.h5','NEW')
    CALL h5%fwrite('groupB->memB0',refsbk)
    CALL h5%fwrite('groupB->memB1',refsbka1)
    CALL h5%fwrite('groupB->memB2',refsbka2)
    CALL h5%fwrite('groupB->memB3',refsbka3)
    CALL h5%mkdir('groupC')
    CALL h5%mkdir('groupC->anotherGroup->moreGroups->almostLastGroup')
    CALL h5%mkdir('groupC->anotherGroup->moreGroups->LastGroup')
    CALL h5%mkdir('groupI')
    CALL h5%fwrite('groupI->memL0',refslk)
    CALL h5%fwrite('groupI->memL1',refslka1)
    CALL h5%fwrite('groupI->memL2',refslka2)
    CALL h5%fwrite('groupI->memL3',refslka3)
    CALL h5%fwrite('groupI->memL4',refslka4)
    CALL h5%fwrite('groupI->memL5',refslka5)
    CALL h5%fwrite('groupI->memL6',refslka6)
    CALL h5%fwrite('groupI->memL7',refslka7)
    CALL h5%fwrite('groupI->memN0',refsnk)
    CALL h5%fwrite('groupI->memN1',refsnka1)
    CALL h5%fwrite('groupI->memN2',refsnka2)
    CALL h5%fwrite('groupI->memN3',refsnka3)
    CALL h5%fwrite('groupI->memN4',refsnka4)
    CALL h5%fwrite('groupI->memN5',refsnka5)
    CALL h5%fwrite('groupI->memN6',refsnka6)
    CALL h5%fwrite('groupI->memN7',refsnka7)
    CALL h5%mkdir('groupR')
    CALL h5%fwrite('groupR->memD0',refsdk)
    CALL h5%fwrite('groupR->memD1',refsdka1)
    CALL h5%fwrite('groupR->memD2',refsdka2)
    CALL h5%fwrite('groupR->memD3',refsdka3)
    CALL h5%fwrite('groupR->memD4',refsdka4)
    CALL h5%fwrite('groupR->memD5',refsdka5)
    CALL h5%fwrite('groupR->memD6',refsdka6)
    CALL h5%fwrite('groupR->memD7',refsdka7)
    CALL h5%fwrite('groupR->memS0',refssk)
    CALL h5%fwrite('groupR->memS1',refsska1)
    CALL h5%fwrite('groupR->memS2',refsska2)
    CALL h5%fwrite('groupR->memS3',refsska3)
    CALL h5%fwrite('groupR->memS4',refsska4)
    CALL h5%fwrite('groupR->memS5',refsska5)
    CALL h5%fwrite('groupR->memS6',refsska6)
    CALL h5%fwrite('groupR->memS7',refsska7)
    CALL h5%mkdir('groupST')
    CALL h5%fwrite('groupST->memST0',refstr)
    CALL h5%fwrite('groupST->memST1',refstra1)
    CALL h5%fwrite('groupST->memST2',refstra2)
    CALL h5%fwrite('groupST->memST3',refstra3)

    !Write Attribute Values
    CALL h5%write_attribute('groupI->memL0','description','test description')
    CALL h5%write_attribute('groupI->memL1','description','test description')
    CALL h5%write_attribute('groupI->memL2','description','test description')

    CALL h5%clear()

  ENDSUBROUTINE setupHDF5testFile
!
ENDPROGRAM testParameterLists
