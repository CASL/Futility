SUBMODULE(FunctionTableGeneratorModule) testFunctionTableGenerator
#include "UnitTest.h"
USE Futility_DBC
USE IntrType
USE IO_Strings
USE ParameterLists
USE FutilityComputingEnvironmentModule
USE UnitTest

TYPE(FutilityComputingEnvironment),TARGET :: ce

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE runTestFunctionTableGenerator()

  CREATE_TEST('FunctionTableGenerator')

  REGISTER_SUBTEST('FunctionTable',testFunctionTable)
  REGISTER_SUBTEST('TableGenerator',testGenerator)

  FINALIZE_TEST()

ENDSUBROUTINE runTestFunctionTableGenerator
!
!-------------------------------------------------------------------------------
SUBROUTINE testFunctionTable()
  REAL(SRK) :: x,refval
  TYPE(ParamType) :: testParams
  TYPE(FunctionTable) :: testTable

  COMPONENT_TEST('Init')
  CALL testParams%add('table -> minRange',-10.0_SRK)
  CALL testParams%add('table -> maxRange',20.0_SRK)
  CALL testParams%add('table -> boundsErrorHigh',.FALSE.)
  CALL testParams%add('table -> boundsErrorLow',.FALSE.)
  CALL testParams%add('table -> highDefaultValue',1000.0_SRK)
  CALL testParams%add('table -> lowDefaultValue',500.0_SRK)
  CALL testTable%init(ce,testParams,ExactQuadraticTest)
  CALL testParams%clear()
  ASSERT(.NOT.testTable%boundsErrorHigh,'%boundsErrorHigh')
  ASSERT(.NOT.testTable%boundsErrorLow,'%boundsErrorLow')
  ASSERT_APPROXEQA(testTable%highDefaultValue,1000.0_SRK,'%highDefaultValue')
  ASSERT_APPROXEQA(testTable%lowDefaultValue,500.0_SRK,'%lowDefaultValue')
  ASSERT_APPROXEQA(testTable%maxRange,20.0_SRK,'%maxRange')
  ASSERT_APPROXEQA(testTable%minRange,-10.0_SRK,'%minRange')
  ASSERT(ASSOCIATED(testTable%func,ExactQuadraticTest),'%func')
  ASSERT(testTable%isInit,'%isInit')
  ASSERT(.NOT.testTable%hasData,'%hasData')
  CALL testTable%clear()

  CALL testParams%add('table -> minRange',-20.0_SRK)
  CALL testParams%add('table -> maxRange',10.0_SRK)
  CALL testParams%add('table -> boundsErrorHigh',.TRUE.)
  CALL testParams%add('table -> boundsErrorLow',.TRUE.)
  CALL testParams%add('table -> highDefaultValue',1000.0_SRK)
  CALL testParams%add('table -> lowDefaultValue',500.0_SRK)
  CALL testTable%init(ce,testParams,ExactQuadraticTest)
  CALL testParams%clear()
  ASSERT(testTable%boundsErrorHigh,'%boundsErrorHigh')
  ASSERT(testTable%boundsErrorLow,'%boundsErrorLow')
  ASSERT_APPROXEQA(testTable%highDefaultValue,0.0_SRK,'%highDefaultValue')
  ASSERT_APPROXEQA(testTable%lowDefaultValue,0.0_SRK,'%lowDefaultValue')
  ASSERT_APPROXEQA(testTable%maxRange,10.0_SRK,'%maxRange')
  ASSERT_APPROXEQA(testTable%minRange,-20.0_SRK,'%minRange')
  ASSERT(ASSOCIATED(testTable%func,ExactQuadraticTest),'%func')
  ASSERT(testTable%isInit,'%isInit')
  ASSERT(.NOT.testTable%hasData,'%hasData')
  CALL testTable%clear()

  COMPONENT_TEST('Clear')
  testTable%isInit=.TRUE.
  testTable%hasData=.TRUE.
  testTable%boundsErrorHigh=.FALSE.
  testTable%boundsErrorLow=.FALSE.
  testTable%minRange=ZERO
  testTable%maxRange=ZERO
  testTable%highDefaultValue=ONE
  testTable%lowDefaultValue=ONE
  testTable%spacing=ZERO
  testTable%inverseSpacing=ZERO
  ALLOCATE(testTable%funcCoordinates(2))
  ALLOCATE(testTable%funcValues(2))
  testTable%func => ExactQuadraticTest
  CALL testTable%clear()
  ASSERT(.NOT.testTable%isInit,'%isInit')
  ASSERT(.NOT.testTable%hasData,'%hasData')
  ASSERT(testTable%boundsErrorHigh,'%boundsErrorHigh')
  ASSERT(testTable%boundsErrorLow,'%boundsErrorLow')
  ASSERT_EQ(testTable%minRange,HUGE(ZERO),'%minRange')
  ASSERT_EQ(testTable%maxRange,-HUGE(ZERO),'%maxRange')
  ASSERT_EQ(testTable%highDefaultValue,ZERO,'%highDefaultValue')
  ASSERT_EQ(testTable%lowDefaultValue,ZERO,'%lowDefaultValue')
  ASSERT_EQ(testTable%spacing,-HUGE(ZERO),'%spacing')
  ASSERT_EQ(testTable%inverseSpacing,-HUGE(ZERO),'%inverseSpacing')
  ASSERT(.NOT.ALLOCATED(testTable%funcCoordinates),'%funcCoordinates')
  ASSERT(.NOT.ALLOCATED(testTable%funcValues),'%funcValues')
  ASSERT(.NOT.ASSOCIATED(testTable%func),'%func')
  CALL testTable%clear()

  COMPONENT_TEST('Evaluate')
  CALL testParams%add('table -> minRange',-10.0_SRK)
  CALL testParams%add('table -> maxRange',20.0_SRK)
  CALL testParams%add('table -> boundsErrorHigh',.FALSE.)
  CALL testParams%add('table -> boundsErrorLow',.FALSE.)
  CALL testParams%add('table -> highDefaultValue',1000.0_SRK)
  CALL testParams%add('table -> lowDefaultValue',500.0_SRK)
  CALL testTable%init(ce,testParams,ExactQuadraticTest)
  testTable%spacing = 0.1_SRK
  testTable%inverseSpacing = 10.0_SRK
  testTable%nPoints = 301
  CALL testParams%clear()
  ALLOCATE(testTable%funcCoordinates(testTable%nPoints))
  ALLOCATE(testTable%funcValues(testTable%nPoints))
  DO i=1,testTable%nPoints
    testTable%funcCoordinates(i)=-10.0_SRK+REAL(i-1,SRK)*0.1_SRK
    testTable%funcValues(i)=100.0_SRK + REAL(i-1,SRK)*1.0_SRK
  ENDDO !i
  testTable%hasData = .TRUE.
  DO i=1,testTable%nPoints-1
    x = -10.0_SRK + REAL(i-1,SRK)*0.1_SRK
    refval = 100.0_SRK + REAL(i-1,SRK)*1.0_SRK
    ASSERT_SOFTEQ(testTable%evaluate(x),refval,1.0E-13_SRK,'%evaluate(x)')
    FINFO() i,x,testTable%evaluate(x),refval
    x = x + 0.025_SRK
    refval = 100.0_SRK + REAL(i-1,SRK)*1.0_SRK + 0.25_SRK
    ASSERT_SOFTEQ(testTable%evaluate(x),refval,1.0E-13_SRK,'%evaluate(x)')
    FINFO() i,x,testTable%evaluate(x),refval
    x = x + 0.025_SRK
    refval = 100.0_SRK + REAL(i-1,SRK)*1.0_SRK + 0.5_SRK
    ASSERT_SOFTEQ(testTable%evaluate(x),refval,1.0E-13_SRK,'%evaluate(x)')
    FINFO() i,x,testTable%evaluate(x),refval
    x = x + 0.025_SRK
    refval = 100.0_SRK + REAL(i-1,SRK)*1.0_SRK + 0.75_SRK
    ASSERT_SOFTEQ(testTable%evaluate(x),refval,1.0E-13_SRK,'%evaluate(x)')
    FINFO() i,x,testTable%evaluate(x),refval
  ENDDO !i
  ASSERT_APPROXEQA(testTable%evaluate(-11.0_SRK),testTable%lowDefaultValue,'%lowDefaultValue')
  ASSERT_APPROXEQA(testTable%evaluate(21.0_SRK),testTable%highDefaultValue,'%highDefaultValue')
  CALL testTable%clear()

ENDSUBROUTINE testFunctionTable
!
!-------------------------------------------------------------------------------
SUBROUTINE testGenerator()
  REAL(SRK) :: x,y1,y2
  TYPE(ParamType) :: testParams
  CLASS(FunctionTable),POINTER :: testTable

  COMPONENT_TEST('Spacing')
  CALL testParams%add('table -> minRange',-10.0_SRK)
  CALL testParams%add('table -> maxRange',20.0_SRK)
  CALL testParams%add('table -> boundsErrorHigh',.FALSE.)
  CALL testParams%add('table -> boundsErrorLow',.FALSE.)
  CALL testParams%add('table -> highDefaultValue',1000.0_SRK)
  CALL testParams%add('table -> lowDefaultValue',500.0_SRK)
  CALL testParams%add('table -> spacing',0.1_SRK)
  testTable => generateFunctionTable(ce,testParams,ExactQuadraticTest)
  CALL testParams%clear()
  ASSERT(ASSOCIATED(testTable),'ASSOCIATED table pointer')
  ASSERT(.NOT.testTable%boundsErrorHigh,'%boundsErrorHigh')
  ASSERT(.NOT.testTable%boundsErrorLow,'%boundsErrorLow')
  ASSERT_APPROXEQA(testTable%highDefaultValue,1000.0_SRK,'%highDefaultValue')
  ASSERT_APPROXEQA(testTable%lowDefaultValue,500.0_SRK,'%lowDefaultValue')
  ASSERT_APPROXEQA(testTable%maxRange,20.0_SRK,'%maxRange')
  ASSERT_APPROXEQA(testTable%minRange,-10.0_SRK,'%minRange')
  ASSERT(ASSOCIATED(testTable%func,ExactQuadraticTest),'%func')
  ASSERT(testTable%isInit,'%isInit')
  ASSERT(testTable%hasData,'%hasData')
  ASSERT_APPROXEQA(testTable%spacing,0.1_SRK,'%spacing')
  ASSERT_APPROXEQA(testTable%inverseSpacing,ONE/testTable%spacing,'%inverseSpacing')
  ASSERT(ALLOCATED(testTable%funcCoordinates),'ALLOCATED %funcCoordinates')
  ASSERT_EQ(SIZE(testTable%funcCoordinates),301,'SIZE %funcCoordinates')
  ASSERT(ALLOCATED(testTable%funcValues),'ALLOCATED %funcValues')
  ASSERT_EQ(SIZE(testTable%funcValues),301,'SIZE %funcValues')
  DO i=1,301
    x=-10.0_SRK+REAL(i-1,SRK)*testTable%spacing
    ASSERT_SOFTEQ(testTable%funcCoordinates(i),x,1.0E-13_SRK,'%funcCoordinates('//str(i)//')')
    ASSERT_SOFTEQ(testTable%funcValues(i),x*x,1.0E-11_SRK,'%funcValues('//str(i)//')')
  ENDDO !i
  CALL testTable%clear()
  DEALLOCATE(testTable)

  COMPONENT_TEST('maxRelError')
  CALL testParams%add('table -> minRange',-10.0_SRK)
  CALL testParams%add('table -> maxRange',20.0_SRK)
  CALL testParams%add('table -> boundsErrorHigh',.FALSE.)
  CALL testParams%add('table -> boundsErrorLow',.FALSE.)
  CALL testParams%add('table -> highDefaultValue',1000.0_SRK)
  CALL testParams%add('table -> lowDefaultValue',500.0_SRK)
  CALL testParams%add('table -> maxRelError',0.1_SRK)
  testTable => generateFunctionTable(ce,testParams,ApproximateQuadraticTest)
  CALL testParams%clear()
  ASSERT(ASSOCIATED(testTable),'ASSOCIATED table pointer')
  ASSERT(.NOT.testTable%boundsErrorHigh,'%boundsErrorHigh')
  ASSERT(.NOT.testTable%boundsErrorLow,'%boundsErrorLow')
  ASSERT_APPROXEQA(testTable%highDefaultValue,1000.0_SRK,'%highDefaultValue')
  ASSERT_APPROXEQA(testTable%lowDefaultValue,500.0_SRK,'%lowDefaultValue')
  ASSERT_APPROXEQA(testTable%maxRange,20.0_SRK,'%maxRange')
  ASSERT_APPROXEQA(testTable%minRange,-10.0_SRK,'%minRange')
  ASSERT(ASSOCIATED(testTable%func,ApproximateQuadraticTest),'%func')
  ASSERT(testTable%isInit,'%isInit')
  ASSERT(testTable%hasData,'%hasData')
  ASSERT_APPROXEQA(testTable%spacing,1.875E-3_SRK,'%spacing')
  ASSERT_APPROXEQA(testTable%inverseSpacing,ONE/testTable%spacing,'%inverseSpacing')
  ASSERT(ALLOCATED(testTable%funcCoordinates),'ALLOCATED %funcCoordinates')
  ASSERT_EQ(SIZE(testTable%funcCoordinates),16001,'SIZE %funcCoordinates')
  ASSERT(ALLOCATED(testTable%funcValues),'ALLOCATED %funcValues')
  ASSERT_EQ(SIZE(testTable%funcValues),16001,'SIZE %funcValues')
  x = -10.0_SRK + testTable%spacing*HALF
  DO i=1,16000
    y1 = ApproximateQuadraticTest(x)
    y2 = testTable%evaluate(x)
    ASSERT((y2-y1)/y1 < 0.1_SRK,'relative error')
    FINFO() i,y1,y2
    x=x+testTable%spacing
  ENDDO !i
  CALL testTable%clear()
  DEALLOCATE(testTable)

  COMPONENT_TEST('maxAbsError')
  CALL testParams%add('table -> minRange',-10.0_SRK)
  CALL testParams%add('table -> maxRange',20.0_SRK)
  CALL testParams%add('table -> boundsErrorHigh',.FALSE.)
  CALL testParams%add('table -> boundsErrorLow',.FALSE.)
  CALL testParams%add('table -> highDefaultValue',1000.0_SRK)
  CALL testParams%add('table -> lowDefaultValue',500.0_SRK)
  CALL testParams%add('table -> maxAbsError',1.0E-7_SRK)
  testTable => generateFunctionTable(ce,testParams,ExactQuadraticTest)
  CALL testParams%clear()
  ASSERT(ASSOCIATED(testTable),'ASSOCIATED table pointer')
  ASSERT(.NOT.testTable%boundsErrorHigh,'%boundsErrorHigh')
  ASSERT(.NOT.testTable%boundsErrorLow,'%boundsErrorLow')
  ASSERT_APPROXEQA(testTable%highDefaultValue,1000.0_SRK,'%highDefaultValue')
  ASSERT_APPROXEQA(testTable%lowDefaultValue,500.0_SRK,'%lowDefaultValue')
  ASSERT_APPROXEQA(testTable%maxRange,20.0_SRK,'%maxRange')
  ASSERT_APPROXEQA(testTable%minRange,-10.0_SRK,'%minRange')
  ASSERT(ASSOCIATED(testTable%func,ExactQuadraticTest),'%func')
  ASSERT(testTable%isInit,'%isInit')
  ASSERT(testTable%hasData,'%hasData')
  ASSERT_APPROXEQA(testTable%spacing,4.6875E-4_SRK,'%spacing')
  ASSERT_APPROXEQA(testTable%inverseSpacing,ONE/testTable%spacing,'%inverseSpacing')
  ASSERT(ALLOCATED(testTable%funcCoordinates),'ALLOCATED %funcCoordinates')
  ASSERT_EQ(SIZE(testTable%funcCoordinates),64001,'SIZE %funcCoordinates')
  ASSERT(ALLOCATED(testTable%funcValues),'ALLOCATED %funcValues')
  ASSERT_EQ(SIZE(testTable%funcValues),64001,'SIZE %funcValues')
  x = -10.0_SRK + testTable%spacing*HALF
  DO i=1,64000
    y1 = ApproximateQuadraticTest(x)
    y2 = testTable%evaluate(x)
    ASSERT((y2-y1)/y1 < 0.01_SRK,'relative error')
    FINFO() i,y1,y2
    x=x+testTable%spacing
  ENDDO !i
  CALL testTable%clear()
  DEALLOCATE(testTable)

  COMPONENT_TEST('maxRelError & maxAbsError')
  CALL testParams%add('table -> minRange',-10.0_SRK)
  CALL testParams%add('table -> maxRange',20.0_SRK)
  CALL testParams%add('table -> boundsErrorHigh',.FALSE.)
  CALL testParams%add('table -> boundsErrorLow',.FALSE.)
  CALL testParams%add('table -> highDefaultValue',1000.0_SRK)
  CALL testParams%add('table -> lowDefaultValue',500.0_SRK)
  CALL testParams%add('table -> maxRelError',0.01_SRK)
  CALL testParams%add('table -> maxAbsError',1.0E-6_SRK)
  testTable => generateFunctionTable(ce,testParams,ApproximateQuadraticTest)
  CALL testParams%clear()
  ASSERT(ASSOCIATED(testTable),'ASSOCIATED table pointer')
  ASSERT(.NOT.testTable%boundsErrorHigh,'%boundsErrorHigh')
  ASSERT(.NOT.testTable%boundsErrorLow,'%boundsErrorLow')
  ASSERT_APPROXEQA(testTable%highDefaultValue,1000.0_SRK,'%highDefaultValue')
  ASSERT_APPROXEQA(testTable%lowDefaultValue,500.0_SRK,'%lowDefaultValue')
  ASSERT_APPROXEQA(testTable%maxRange,20.0_SRK,'%maxRange')
  ASSERT_APPROXEQA(testTable%minRange,-10.0_SRK,'%minRange')
  ASSERT(ASSOCIATED(testTable%func,ApproximateQuadraticTest),'%func')
  ASSERT(testTable%isInit,'%isInit')
  ASSERT(testTable%hasData,'%hasData')
  ASSERT_APPROXEQA(testTable%spacing,4.6875E-4_SRK,'%spacing')
  ASSERT_APPROXEQA(testTable%inverseSpacing,ONE/testTable%spacing,'%inverseSpacing')
  ASSERT(ALLOCATED(testTable%funcCoordinates),'ALLOCATED %funcCoordinates')
  ASSERT_EQ(SIZE(testTable%funcCoordinates),64001,'SIZE %funcCoordinates')
  ASSERT(ALLOCATED(testTable%funcValues),'ALLOCATED %funcValues')
  ASSERT_EQ(SIZE(testTable%funcValues),64001,'SIZE %funcValues')
  x = -10.0_SRK + testTable%spacing*HALF
  DO i=1,64000
    y1 = ApproximateQuadraticTest(x)
    y2 = testTable%evaluate(x)
    ASSERT((y2-y1)/y1 < 0.01_SRK,'relative error')
    FINFO() i,y1,y2
    x=x+testTable%spacing
  ENDDO !i
  CALL testTable%clear()
  DEALLOCATE(testTable)

ENDSUBROUTINE testGenerator
!
!-------------------------------------------------------------------------------
FUNCTION ExactQuadraticTest(x) RESULT(y)
  REAL(SRK),INTENT(IN) :: x
  REAL(SRK) :: y

  y = x*x

ENDFUNCTION ExactQuadraticTest
!
!-------------------------------------------------------------------------------
FUNCTION ApproximateQuadraticTest(x) RESULT(y)
  REAL(SRK),INTENT(IN) :: x
  REAL(SRK) :: y

  y = x*x + EPSS

ENDFUNCTION ApproximateQuadraticTest
!
ENDSUBMODULE
!
PROGRAM testFunctionTableGenerator
USE FunctionTableGeneratorModule
CALL runTestFunctionTableGenerator()
ENDPROGRAM testFunctionTableGenerator