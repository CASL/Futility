!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module that encompasses a base class for solution
!>        acceleration.  Module also includes both picard iteration and a
!>        modified picard iteration.  Anderson Acceleration extends this base
!>        class in the AndersonAcceleration module.
MODULE SolutionAccelerationModule
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE FutilityComputingEnvironmentModule
USE ParameterLists
USE ParallelEnv
USE VectorTypes

IMPLICIT NONE

PRIVATE

!List of public members
PUBLIC :: SolutionAccelerationType
!PUBLIC :: RelaxedPicardType
!PUBLIC :: ModifiedPicardType

!> @brief the base Solution Acceleration Type
TYPE,ABSTRACT :: SolutionAccelerationType
  !> Initialization status
  LOGICAL(SBK) :: isInit=.FALSE.
  !> Size of solution vector/matrix
  INTEGER(SIK) :: n=-1
  !> Current iteration count
  INTEGER(SIK) :: s=0
  !> Starting iteration
  INTEGER(SIK) :: start=1
  !> Initial iterates
  CLASS(VectorType),ALLOCATABLE :: x(:)
  !> Futility computing environment
  TYPE(FutilityComputingEnvironment),POINTER :: ce => NULL()
!
!List of Type Bound Procedures
  CONTAINS
    !> @copybrief SolutionAccelerationType::init
    !> @copydetails SolutionAccelerationType::init
    PROCEDURE(init_absintfc),DEFERRED,PASS :: init
    !> @copybrief SolutionAccelerationType::init_base_SolutionAccelerationType
    !> @copydetails SolutionAccelerationType::init_base_SolutionAccelerationType
    PROCEDURE,PASS :: init_base => init_base_SolutionAccelerationType
    !> @copybrief SolutionAccelerationType::clear
    !> @copydetails SolutionAccelerationType::clear
    PROCEDURE(clear_absintfc),DEFERRED,PASS :: clear
    !> @copybrief SolutionAccelerationType::setInitial
    !> @copydetails SolutionAccelerationType::setInitial
    PROCEDURE(step_absintfc),DEFERRED,PASS :: setInitial
    !> @copybrief SolutionAccelerationType::step
    !> @copydetails SolutionAccelerationType::step
    PROCEDURE(step_absintfc),DEFERRED,PASS :: step
    !> @copybrief SolutionAccelerationType::reset
    !> @copydetails SolutionAccelerationType::reset
    PROCEDURE(step_absintfc),DEFERRED,PASS :: reset
ENDTYPE SolutionAccelerationType

!> Definition of interface for init
ABSTRACT INTERFACE
  SUBROUTINE init_absintfc(this,ce,Params)
    IMPORT :: SolutionAccelerationType,FutilityComputingEnvironment,ParamType
    CLASS(SolutionAccelerationType),INTENT(INOUT) :: this
    TYPE(FutilityComputingEnvironment),TARGET,INTENT(IN) :: ce
    TYPE(ParamType),INTENT(IN) :: Params
  ENDSUBROUTINE init_absintfc
ENDINTERFACE

!> Definition of interface for clear
ABSTRACT INTERFACE
  SUBROUTINE clear_absintfc(this)
    IMPORT :: SolutionAccelerationType
    CLASS(SolutionAccelerationType),INTENT(INOUT) :: this
  ENDSUBROUTINE clear_absintfc
ENDINTERFACE

!> Definition of interface for setInitial, step, and reset
ABSTRACT INTERFACE
  SUBROUTINE step_absintfc(this,x)
    IMPORT :: SolutionAccelerationType,VectorType
    CLASS(SolutionAccelerationType),INTENT(INOUT) :: this
    CLASS(VectorType),INTENT(INOUT) :: x
  ENDSUBROUTINE step_absintfc
ENDINTERFACE

!> Name of module
CHARACTER(LEN=*),PARAMETER :: modName='SolutionAccelerationModules'
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Solution Acceleration base class
!> @param solver The Solution Acceleration solver to act on
!> @param Params The options parameter list
!> @param ce The computing environment to use for the calculation
!>
SUBROUTINE init_base_SolutionAccelerationType(solver,ce,Params)
  CLASS(SolutionAccelerationType),INTENT(INOUT) :: solver
  TYPE(FutilityComputingEnvironment),TARGET,INTENT(IN) :: ce
  TYPE(ParamType),INTENT(IN) :: Params

  CHARACTER(LEN=*),PARAMETER :: myName='init_base_SolutionAccelerationType'

  REQUIRE(Params%has('SolutionAccelerationType->n'))
  REQUIRE(.NOT.solver%isInit)

  !Pull Data from Parameter List
  CALL Params%get('SolutionAccelerationType->n',solver%n)
  IF(Params%has('SolutionAccelerationType->start')) &
      CALL Params%get('SolutionAccelerationType->start',solver%start)

  IF(solver%n < 1) CALL ce%exceptHandler%raiseError('Incorrect input to '//modName// &
      '::'//myName//' - Number of unkowns (n) must be greater than 0!')

  IF(solver%start < 1) CALL ce%exceptHandler%raiseError('Incorrect input to '//modName// &
      '::'//myName//' - Anderson starting point must be greater than 1!')

  solver%s=0
  solver%ce => ce
ENDSUBROUTINE init_base_SolutionAccelerationType
!
ENDMODULE SolutionAccelerationModule
