!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module provides the ability to import and interact with
!> Functional Mock Up Units (FMU).
!>
!> Currently supported FMU versions:
!>  - 2.0
!>
!> See: https://fmi-standard.org for additional info.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FMU_Wrapper
USE ISO_C_BINDING
USE IntrType
USE BLAS
!USE trilinos_interfaces
USE ExceptionHandler
USE Allocs
USE ParameterLists
USE ParallelEnv
USE VectorTypes
USE Strings
USE FMU_interface

IMPLICIT NONE
PRIVATE

!
! List of public members
PUBLIC :: FMU2_Slave
PUBLIC :: FMU_n
PUBLIC :: eFMU_Wrapper

!> Total number of FMU loaded
INTEGER(SIK) :: FMU_n=0


!> @brief the base fmu type
TYPE,ABSTRACT :: FMU_Base
  !> Initialization status
  LOGICAL(SBK) :: isInit=.FALSE.
  !> Integer flag for the solution methodology desired
  INTEGER(SIK) :: solverMethod=-1
  !> FMU C Pointer
  TYPE(C_PTR) :: fmu_c_ptr
!
!List of Type Bound Procedures
  CONTAINS
    !> Defered routine
    PROCEDURE(fmu_init_sub_absintfc),DEFERRED,PASS :: init
ENDTYPE FMU_Base

ABSTRACT INTERFACE
  SUBROUTINE fmu_init_sub_absintfc(self,id,Params)
    IMPORT FMU_Base,SIK,ParamType
    CLASS(FMU_Base),INTENT(INOUT) :: self
    INTEGER(SIK),INTENT(IN) :: id
    TYPE(ParamType),INTENT(IN) :: Params
  ENDSUBROUTINE
ENDINTERFACE

!> @brief FMU run in slave mode intended for use with external driver, such as CTF
TYPE,EXTENDS(FMU_Base) :: FMU2_Slave
  !> FMU version
  INTEGER(SIK) :: FMU_version=2
!
!List of Type Bound Procedures
  CONTAINS
    !> @copybrief FMU_Wrapper::init_FMU2_Slave
    !> @copydetails FMU_Wrapper::init_FMU2_Slave
    PROCEDURE,PASS :: init => init_FMU2_Slave
ENDTYPE FMU2_Slave

!> Exception Handler for use in MatrixTypes
TYPE(ExceptionHandlerType),SAVE :: eFMU_Wrapper

!> Name of module
CHARACTER(LEN=*),PARAMETER :: modName='FMU_Wrapper'

CONTAINS

!
!-------------------------------------------------------------------------------
!> @brief Initializes the FMU Slave
!>
!> @param self
!> @param id Unique id for this FMU
!> @param Params A parameter list with input options
!>
SUBROUTINE init_FMU2_Slave(self,id,Params)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: id
  TYPE(ParamType),INTENT(IN) :: Params

  ! Store path to FMU .so file that holds implemented fmu symbols
  ! Store path to FMU xml file that holds model description and io vars

  ! Initilize the FMU
  CALL InitilizeFMU2_Slave(self%fmu_c_ptr, id)

  self%isInit=.TRUE.

ENDSUBROUTINE

ENDMODULE FMU_Wrapper
