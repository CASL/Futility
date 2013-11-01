
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
!> @brief Module provides a preconditioner type and methods to set up different
!> preconditioners
!>
!> The precontioner type owns its own matrix. It is initialized with a matrix. 
!> In general any third party library (TPL) that may be interfaced with this 
!> module should be implemented such that it's optional.
!>
!> Currently supported TPLs include:
!>  - 
!>
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref BLAS "BLAS": @copybrief BLAS
!>  - @ref Times "Times": @copybrief Times
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>  - @ref Allocs "Allocs": @copybrief Allocs
!>  - @ref ParameterLists "ParameterLists": @copybrief ParameterLists
!>  - @ref ParallelEnv "ParallelEnv": @copybrief ParallelEnv
!>  - @ref VectorTypes "VectorTypes": @copybrief VectorTypes
!>  - @ref MatrixTypes "MatrixTypes": @copybrief MatrixTypes
!>
!> @par EXAMPLES
!> @code
!> 
!> @endcode
!>
!> @author Aaron Graham, Andrew Gerlach
!>   @date 11/01/2013
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE PreconditionerTypes

  USE IntrType
  USE BLAS
  USE Times
  USE ExceptionHandler
  USE Allocs
  USE ParameterLists
  USE ParallelEnv
  USE VectorTypes
  USE MatrixTypes

  IMPLICIT NONE

  PRIVATE

  !
  ! List of Public members
  PUBLIC :: PreconditionerType_Base
  PUBLIC :: PreconditionerType_ILU

  !> @brief the base preconditioner type
  TYPE,ABSTRACT :: PreconditionerType_Base
    !> Initialization status
    LOGICAL(SBK) :: isInit=.FALSE.
    !> Flag to indicated whether this is the preconditioner or its inverse
    LOGICAL(SBK) :: isInverse=.FALSE.
    !
    !List of Type Bound Procedures
    CONTAINS
      !> Deferred routine for clearing the preconditioner
      PROCEDURE(precond_sub_absintfc),DEFERRED,PASS :: clear
      !> Deferred routine for initializing the preconditioner
      PROCEDURE(precond_sub_absintfc),DEFERRED,PASS :: init
  ENDTYPE PreconditionerType_Base

  TYPE,EXTENDS(PreconditionerType_Base) :: PreconditionerType_ILU
    !> ILU Preconditioner Matrix
    CLASS(MatrixType),ALLOCATABLE :: M
    !
    ! List of Type Bound Procedures
    CONTAINS
      !> @copybrief LinearSolverTypes::clear_PreconditionerType_ILU
      !> @copydetails LinearSolvertypes::clear_PreconditionerType_ILU
      PROCEDURE :: clear => clear_PreconditionerType_ILU
      !> @copybrief LinearSolverTypes::init_PreconditionerType_ILU
      !> @copydetails LinearSolvertypes::init_PreconditionerType_ILU
      PROCEDURE :: init => init_PreconditionerType_ILU
  ENDTYPE PreconditionerType_ILU

  ABSTRACT INTERFACE
    SUBROUTINE precond_sub_absintfc(PC)
      IMPORT :: PreconditionerType_Base
      CLASS(PreconditionerType_Base),INTENT(INOUT) :: PC
    ENDSUBROUTINE precond_sub_absintfc
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
    SUBROUTINE init_PreconditionerType_ILU(PC)
      CLASS(PreconditionerType_ILU),INTENT(INOUT) :: PC
    ENDSUBROUTINE init_PreconditionerType_ILU
!
!-------------------------------------------------------------------------------
!> @brief Initializes the Linear Solver Type with a parameter list
!> @param pList the parameter list
!>
!> @param solver The linear solver to act on
    SUBROUTINE clear_PreconditionerType_ILU(PC)
      CLASS(PreconditionerType_ILU),INTENT(INOUT) :: PC
    ENDSUBROUTINE clear_PreconditionerType_ILU
!
!-------------------------------------------------------------------------------
END MODULE
