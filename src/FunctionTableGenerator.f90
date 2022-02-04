!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief This module defines the @c FunctionTable object and a factory subroutine
!> to generate the table
!>
!> The @c generateFunctionTable function should be called to retrieve a pointer
!> to the new @c FunctionTable.  The function to be tabluated is provided as an
!> argument.  A parameter list provides such parameters as the minimum and maximum
!> values for which the function should be tabulated, behavior for values outside
!> that range, and generation of the table.  The table can be generated using
!> fixed spacing, or it can be dynamically refined until client-specified criteria
!> such as maximum error are achieved.
!>
!> Once the @c FunctionTable pointer is returned to the client, that client is
!> now the owner of the pointer and the memory.  This module provides no
!> mechanisms for memory management of the table.  The client should call
!> @c evaluate on the table to obtain the value of the function for an input.
!> Once the client is done with the table, it should call @c clear, then
!> deallocate the pointer.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FunctionTableGeneratorModule
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE Constants_Conversion
USE IO_Strings
USE ParameterLists
USE FutilityComputingEnvironmentModule

IMPLICIT NONE
PRIVATE

PUBLIC :: FunctionTable
PUBLIC :: generateFunctionTable

#ifdef UNIT_TEST
PUBLIC :: generateTableData
PUBLIC :: runTestFunctionTableGenerator
INTERFACE
  MODULE SUBROUTINE runTestFunctionTableGenerator()
  ENDSUBROUTINE runTestFunctionTableGenerator
ENDINTERFACE
#endif

INTERFACE generateFunctionTable
  MODULE PROCEDURE generateFunctionTable1D
ENDINTERFACE generateFunctionTable

!> @brief abstract interface for a 1D tabulated function
!> @param x the input value
!> @returns y the output value
ABSTRACT INTERFACE
  FUNCTION tabulatedFunction1D(x) RESULT(y)
    IMPORT :: SRK
    REAL(SRK),INTENT(IN) :: x
    REAL(SRK) :: y
  ENDFUNCTION tabulatedFunction1D
ENDINTERFACE

!> @brief Stores tabulated data for a function and returns the approximate function value
TYPE :: FunctionTable
  PRIVATE
  !> Logical indicating initialization status
  LOGICAL(SBK) :: isInit=.FALSE.
  !> Logical indicating if the table has been populated with data
  LOGICAL(SBK) :: hasData=.FALSE.
  !> Logical indicating if an error should be thrown for input values greater than @c maxRange
  LOGICAL(SBK) :: boundsErrorHigh=.TRUE.
  !> Logical indicating if an error should be thrown for input values less than @c minRange
  LOGICAL(SBK) :: boundsErrorLow=.TRUE.
  !> Number of data points contained in the table
  INTEGER(SIK) :: nPoints=0_SIK
  !> The minimum input value stored on the table
  REAL(SRK) :: minRange=HUGE(ZERO)
  !> The maximum input value stored on the table
  REAL(SRK) :: maxRange=-HUGE(ZERO)
  !> The return value for inputs greater than @c maxRange if @c boundsErrorHigh is false
  REAL(SRK) :: highDefaultValue=ZERO
  !> The return value for inputs greater than @c minRange if @c boundsErrorLow is false
  REAL(SRK) :: lowDefaultValue=ZERO
  !> The spacing between input values stored by the table
  REAL(SRK) :: spacing=-HUGE(ZERO)
  !> The inverse of @c spacing; stored for optimization purposes
  REAL(SRK) :: inverseSpacing=-HUGE(ZERO)
  !> The list of input values for which the function has been explicitly evaluated
  REAL(SRK),ALLOCATABLE :: funcCoordinates(:)
  !> The list of function values calculated from @c funcCoordinates and used for interpolation
  REAL(SRK),ALLOCATABLE :: funcValues(:)
  !> Pointer to the function used for tabulation
  PROCEDURE(tabulatedFunction1D),NOPASS,POINTER :: func => NULL()
  !> Pointer to the computing environment
  CLASS(FutilityComputingEnvironment),POINTER :: ce => NULL()
  !
  CONTAINS
    !> @copybrief FunctionTableGeneratorModule::initFunctionTable1D
    !> @copydetails FunctionTableGeneratorModule::initFunctionTable1D
    PROCEDURE,PASS :: init => initFunctionTable1D
    !> @copybrief FunctionTableGeneratorModule::clearFunctionTable1D
    !> @copydetails FunctionTableGeneratorModule::clearFunctionTable1D
    PROCEDURE,PASS :: clear => clearFunctionTable1D
    !> @copybrief FunctionTableGeneratorModule::evaluateFunctionTable1D
    !> @copydetails FunctionTableGeneratorModule::evaluateFunctionTable1D
    PROCEDURE,PASS :: evaluate => evaluateFunctionTable1D
ENDTYPE FunctionTable

CHARACTER(LEN=*),PARAMETER :: modName='FunctionTableGenerator'
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief generates a @c FunctionTable object and returns a pointer to it
!> @param ce the computing environment
!> @param params the input parameter list controlling the generation of the table
!> @param func the procedure to be tabulated; must have an interface matching @c tabulatedFunction1D
!> @returns table pointer to the initialized function table
!>
FUNCTION generateFunctionTable1D(ce,params,func) RESULT(table)
  CLASS(FutilityComputingEnvironment),TARGET,INTENT(INOUT) :: ce
  CLASS(ParamType),INTENT(IN) :: params
  PROCEDURE(tabulatedFunction1D) :: func
  CLASS(FunctionTable),POINTER :: table
  !
  REAL(SRK) :: maxRelError,maxRelErrorData,maxAbsError,maxAbsErrorData

  REQUIRE(params%has('spacing') /= (params%has('maxRelError') .OR. params%has('maxAbsError')))

  ALLOCATE(table)
  CALL table%init(ce,params,func)

  !Now setup the table.  If spacing is specified, just call setup once for that spacing
  IF(params%has('spacing')) THEN
    CALL params%get('spacing',table%spacing)
    REQUIRE(table%spacing > ZERO)
    CALL generateTableData(table)
  !If error is specified, start with 1000 points and refine until the error is small enough
  ELSE
    IF(params%has('maxRelError')) THEN
      CALL params%get('maxRelError',maxRelError)
      REQUIRE(maxRelError > ZERO)
    ELSE
      maxRelError=HUGE(maxRelError)
    ENDIF
    IF(params%has('maxAbsError')) THEN
      CALL params%get('maxAbsError',maxAbsError)
      REQUIRE(maxAbsError > ZERO)
    ELSE
      maxAbsError=HUGE(maxAbsError)
    ENDIF
    table%spacing = (table%maxRange - table%minRange)*0.001_SRK
    maxRelErrorData = HUGE(maxRelError)
    maxAbsErrorData = HUGE(maxAbsError)
    DO WHILE(maxRelErrorData > maxRelError .OR. maxAbsErrorData > maxAbsError)
      CALL generateTableData(table)
      CALL evaluateTableQuality(table,maxRelErrorData,maxAbsErrorData)
    ENDDO
  ENDIF

ENDFUNCTION generateFunctionTable1D
!
!-------------------------------------------------------------------------------
!> @brief Initializes a function table without populating its data
!> @param this the @c FunctionTable to initialize
!> @param ce the computing environment
!> @param params the input parameter list controlling the initialization of the table
!> @param func the procedure to be tabulated; must have an interface matching @c tabulatedFunction1D
!>
SUBROUTINE initFunctionTable1D(this,ce,params,func)
  CLASS(FunctionTable),INTENT(INOUT) :: this
  CLASS(FutilityComputingEnvironment),TARGET,INTENT(INOUT) :: ce
  CLASS(ParamType),INTENT(IN) :: params
  PROCEDURE(tabulatedFunction1D) :: func

  REQUIRE(params%has('minRange'))
  REQUIRE(params%has('maxRange'))
  REQUIRE(params%has('boundsErrorHigh'))
  REQUIRE(params%has('boundsErrorLow'))

  !Set the boundaries and default values (or error behavior) outside the boundaries
  CALL params%get('boundsErrorHigh',this%boundsErrorHigh)
  CALL params%get('boundsErrorLow',this%boundsErrorLow)
  IF(.NOT.this%boundsErrorHigh) THEN
    IF(params%has('highDefaultValue')) THEN
      CALL params%get('highDefaultValue',this%highDefaultValue)
    ENDIF
  ENDIF
  IF(.NOT.this%boundsErrorLow) THEN
    IF(params%has('lowDefaultValue')) THEN
      CALL params%get('lowDefaultValue',this%lowDefaultValue)
    ENDIF
  ENDIF
  CALL params%get('minRange',this%minRange)
  CALL params%get('maxRange',this%maxRange)
  REQUIRE(this%minRange < this%maxRange)

  !Associate pointers
  this%func => func
  this%ce => ce

  this%isInit=.TRUE.
  this%hasData=.FALSE.

ENDSUBROUTINE initFunctionTable1D
!
!-------------------------------------------------------------------------------
!> @brief Clears a @c FunctionTable object
!> @param this the table to clear
!>
SUBROUTINE clearFunctionTable1D(this)
  CLASS(FunctionTable),INTENT(INOUT) :: this

  this%isInit = .FALSE.
  this%hasData = .FALSE.
  this%boundsErrorHigh = .TRUE.
  this%boundsErrorLow = .TRUE.
  this%nPoints = 0_SIK
  this%minRange = HUGE(ZERO)
  this%maxRange = -HUGE(ZERO)
  this%highDefaultValue = ZERO
  this%lowDefaultValue = ZERO
  this%spacing = -HUGE(ZERO)
  this%inverseSpacing = -HUGE(ZERO)
  IF(ALLOCATED(this%funcCoordinates)) DEALLOCATE(this%funcCoordinates)
  IF(ALLOCATED(this%funcValues)) DEALLOCATE(this%funcValues)
  this%func => NULL()
  this%ce => NULL()

ENDSUBROUTINE clearFunctionTable1D
!
!-------------------------------------------------------------------------------
!> @brief Approximately evaluates the tabulated function at @c x
!> @param this the @c FunctionTable object
!> @param x the input value for the function
!> @returns y the approximate function values at @c x
!>
FUNCTION evaluateFunctionTable1D(this,x) RESULT(y)
  CLASS(FunctionTable),INTENT(IN) :: this
  REAL(SRK),INTENT(IN) :: x
  REAL(SRK) :: y
  !
  CHARACTER(LEN=*),PARAMETER :: myName='evaluateFunctionTable1D'
  INTEGER :: tableIndex
  REAL(SRK) :: interval, fraction

  REQUIRE(this%isInit)
  REQUIRE(this%hasData)
  REQUIRE(this%inverseSpacing > ZERO)
  REQUIRE(ALLOCATED(this%funcCoordinates))
  REQUIRE(ALLOCATED(this%funcValues))

  interval = x - this%minRange
  tableIndex = FLOOR(interval * this%inverseSpacing) + 1
  IF(tableIndex < 1) THEN
    IF(this%boundsErrorLow) THEN
      CALL this%ce%exceptHandler%raiseError(modName//'::'//myName// &
          ' - Value '//str(x)//' is below the minimum table value '//str(this%minRange)//'!')
    ELSE
      y = this%lowDefaultValue
    ENDIF
  ELSEIF(tableIndex >= this%nPoints) THEN !Use >= to avoid index out of bounds in ELSE statement
    IF(this%boundsErrorHigh) THEN
      CALL this%ce%exceptHandler%raiseError(modName//'::'//myName// &
          ' - Value '//str(x)//' is above the maximum table value '//str(this%maxRange)//'!')
    ELSE
      y = this%highDefaultValue
    ENDIF
  ELSE
    fraction = (x - this%funcCoordinates(tableIndex)) * this%inverseSpacing
    y = (ONE - fraction) * this%funcValues(tableIndex) + fraction * this%funcValues(tableIndex+1)
  ENDIF

ENDFUNCTION evaluateFunctionTable1D
!
!-------------------------------------------------------------------------------
!> @brief Generates the data for a @c FunctionTable object
!> @param table the object to populate data for
!>
!> If @c hasData is true, then it is assumed that the function table data is being
!> refined.  The spacing will be cut in half and the data regenerated.  If false, then
!> new data will be populated using whatever the current value of @c spacing is.
!>
SUBROUTINE generateTableData(table)
  CLASS(FunctionTable),INTENT(INOUT) :: table
  !
  INTEGER(SIK) :: i,stride
  REAL(SRK) :: x
  REAL(SRK),ALLOCATABLE :: oldValues(:),oldCoordinates(:)

  REQUIRE(ASSOCIATED(table%func))
  REQUIRE(table%maxRange > -HUGE(table%maxRange))
  REQUIRE(table%minRange < HUGE(table%minRange))
  REQUIRE(table%spacing > ZERO)

  IF(table%hasData) THEN
    !Assume we've already generated the table once and are refining by 2x
    IF((SIZE(table%funcValues) > 2) .AND. (MOD(SIZE(table%funcValues),2) == 1)) THEN
      CALL MOVE_ALLOC(table%funcValues,oldValues)
      CALL MOVE_ALLOC(table%funcCoordinates,oldCoordinates)
      stride = 2
      table%spacing = table%spacing * HALF
    !Generated some table that doesn't conform to how we're going to generate it this time,
    ! so just throw out the old values
    ELSE
      DEALLOCATE(table%funcValues)
      DEALLOCATE(table%funcCoordinates)
      stride = 1
    ENDIF
  ELSE
    stride = 1
  ENDIF

  table%inverseSpacing = ONE / table%spacing
  table%nPoints = NINT((table%maxRange - table%minRange) * table%inverseSpacing) + 1
  ALLOCATE(table%funcValues(table%nPoints))
  ALLOCATE(table%funcCoordinates(table%nPoints))


  IF(stride == 2) THEN
    table%funcValues(1:table%nPoints:2)=oldValues(:)
    table%funcCoordinates(1:table%nPoints:2)=oldCoordinates(:)
    DEALLOCATE(oldValues)
    DEALLOCATE(oldCoordinates)
  ENDIF

  DO i=stride,table%nPoints,stride
    x = table%minRange + REAL(i-1,SRK)*table%spacing
    table%funcCoordinates(i) = x
    table%funcValues(i) = table%func(x)
  ENDDO !i

  table%hasData=.TRUE.

ENDSUBROUTINE generateTableData
!
!-------------------------------------------------------------------------------
!> @brief Evaluates the quality of a @c FunctionTable compared to the real function
!> @param table the @c FunctionTable to evaluate
!> @returns maxRelError the maximum relative error of the function table
!> @returns maxAbsError the maximum absolute error of the function table
!>
!> If the function returns 0.0 within double precision tolerance, then the value
!> of the table at that input is used to calculate the relative error.  This may
!> be of importance when generating the table dynamically.
!>
SUBROUTINE evaluateTableQuality(table,maxRelError,maxAbsError)
  CLASS(FunctionTable),INTENT(IN) :: table
  REAL(SRK),INTENT(OUT) :: maxRelError
  REAL(SRK),INTENT(OUT) :: maxAbsError
  !
  INTEGER(SIK) :: i
  REAL(SRK) :: x,yTab,yRef

  REQUIRE(ALLOCATED(table%funcValues))
  REQUIRE(ASSOCIATED(table%func))
  REQUIRE(table%maxRange > -HUGE(table%maxRange))
  REQUIRE(table%minRange < HUGE(table%minRange))
  REQUIRE(table%spacing > ZERO)

  maxRelError = ZERO
  maxAbsError = ZERO
  DO i=1,table%nPoints-1
    x = table%minRange + (REAL(i-1,SRK)+HALF) * table%spacing
    yTab = table%evaluate(x)
    yRef = table%func(x)
    IF((yRef .APPROXEQA. ZERO) .AND. (.NOT.(yTab .APPROXEQA. ZERO))) THEN
      maxRelError = MAX(maxRelError,yTab)
    ELSE
      maxRelError = MAX(maxRelError,ABS((yTab - yRef)/yRef))
    ENDIF
    maxAbsError = MAX(maxAbsError,ABS(yTab - yRef))
  ENDDO

ENDSUBROUTINE evaluateTableQuality
!
ENDMODULE FunctionTableGeneratorModule