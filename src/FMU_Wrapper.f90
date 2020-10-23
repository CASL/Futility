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
!> The FMU is basically a zip file contaning an XML file and a *.so library.
!> The file layout of an unzipped FMU is as follows:
!>
!>  FMU_unzipDirectory
!>   |-> binaries
!>   |   |-> linux64
!>   |       |-> modelIdentifier.so
!>   |-> modelIdentifier.xml
!>
!> Where modelIdentifier and unzipDirectory can be specified in the input pList
!>
!> An example v2.0 FMU is available at:
!> https://github.com/modelica/fmi-cross-check/raw/master/fmus/2.0/cs/linux64/MapleSim/2018/Rectifier/Rectifier.fmu
!> The FMU is actually a zip file, and can be unziped in linux with:
!>
!>   unzip Rectifier.fmu
!>
!> See: https://fmi-standard.org for additional info.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FMU_Wrapper
#include "Futility_DBC.h"
USE Futility_DBC
USE ISO_C_BINDING
USE IntrType
USE ExceptionHandler
USE ParameterLists
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
  !> Model id
  INTEGER(SIK) :: idFMU
  !> Model globaly unique id
  TYPE(StringType) :: guid
  !> Model file name used in FMU XML file open
  TYPE(StringType) :: modelIdentifier
  !> Path to unziped FMU
  TYPE(StringType) :: unzipDirectory
  !> User defined FMU instance label
  TYPE(StringType) :: instanceName
!
!List of Type Bound Procedures
  CONTAINS
    !> Defered init routine
    PROCEDURE(fmu_init_sub_absintfc),DEFERRED,PASS :: init
    !> Defered clear routine
    PROCEDURE(fmu_clear_sub_absintfc),DEFERRED,PASS :: clear
    !> Defered setupExperiment routine
    PROCEDURE(fmu_setupExperiment_sub_absintfc),DEFERRED,PASS :: setupExperiment
    !> Defered getReal routine
    PROCEDURE(fmu_getReal_sub_absintfc),DEFERRED,PASS :: getReal
    !> Defered setReal routine
    PROCEDURE(fmu_setReal_sub_absintfc),DEFERRED,PASS :: setReal
    PROCEDURE(fmu_getInteger_sub_absintfc),DEFERRED,PASS :: getInteger
    PROCEDURE(fmu_setInteger_sub_absintfc),DEFERRED,PASS :: setInteger
    PROCEDURE(fmu_getBoolean_sub_absintfc),DEFERRED,PASS :: getBoolean
    PROCEDURE(fmu_setBoolean_sub_absintfc),DEFERRED,PASS :: setBoolean
    !> Defered doStep routine
    PROCEDURE(fmu_doStep_sub_absintfc),DEFERRED,PASS :: doStep
ENDTYPE FMU_Base

ABSTRACT INTERFACE
  SUBROUTINE fmu_init_sub_absintfc(self,id,pList)
    IMPORT FMU_Base,SIK,ParamType
    CLASS(FMU_Base),INTENT(INOUT) :: self
    INTEGER(SIK),INTENT(IN) :: id
    TYPE(ParamType),INTENT(IN) :: pList
  ENDSUBROUTINE
  SUBROUTINE fmu_clear_sub_absintfc(self)
    IMPORT FMU_Base,SIK,ParamType
    CLASS(FMU_Base),INTENT(INOUT) :: self
  ENDSUBROUTINE
  SUBROUTINE fmu_setupExperiment_sub_absintfc(self, toleranceDefined, tolerance, startTime, stopTimeDefined, stopTime)
    IMPORT FMU_Base,SIK,SBK,SRK
    CLASS(FMU_Base),INTENT(INOUT) :: self
    LOGICAL(SBK),INTENT(IN) :: toleranceDefined
    REAL(SRK),INTENT(IN) :: tolerance
    REAL(SRK),INTENT(IN) :: startTime
    LOGICAL(SBK),INTENT(IN) :: stopTimeDefined
    REAL(SRK),INTENT(IN) :: stopTime
  ENDSUBROUTINE
  SUBROUTINE fmu_getReal_sub_absintfc(self, valueReference, val)
    IMPORT FMU_Base,SIK,SRK
    CLASS(FMU_Base),INTENT(INOUT) :: self
    INTEGER(SIK),INTENT(IN) :: valueReference
    REAL(SRK),INTENT(INOUT) :: val
  ENDSUBROUTINE
  SUBROUTINE fmu_setReal_sub_absintfc(self, valueReference, val)
    IMPORT FMU_Base,SIK,SRK
    CLASS(FMU_Base),INTENT(INOUT) :: self
    INTEGER(SIK),INTENT(IN) :: valueReference
    REAL(SRK),INTENT(IN) :: val
  ENDSUBROUTINE
  SUBROUTINE fmu_getInteger_sub_absintfc(self, valueReference, val)
    IMPORT FMU_Base,SIK,SRK
    CLASS(FMU_Base),INTENT(INOUT) :: self
    INTEGER(SIK),INTENT(IN) :: valueReference
    INTEGER(SIK),INTENT(INOUT) :: val
  ENDSUBROUTINE
  SUBROUTINE fmu_setInteger_sub_absintfc(self, valueReference, val)
    IMPORT FMU_Base,SIK,SRK
    CLASS(FMU_Base),INTENT(INOUT) :: self
    INTEGER(SIK),INTENT(IN) :: valueReference
    INTEGER(SIK),INTENT(IN) :: val
  ENDSUBROUTINE
  SUBROUTINE fmu_getBoolean_sub_absintfc(self, valueReference, val)
    IMPORT FMU_Base,SIK,SRK,SBK
    CLASS(FMU_Base),INTENT(INOUT) :: self
    INTEGER(SIK),INTENT(IN) :: valueReference
    LOGICAL(SBK),INTENT(INOUT) :: val
  ENDSUBROUTINE
  SUBROUTINE fmu_setBoolean_sub_absintfc(self, valueReference, val)
    IMPORT FMU_Base,SIK,SRK,SBK
    CLASS(FMU_Base),INTENT(INOUT) :: self
    INTEGER(SIK),INTENT(IN) :: valueReference
    LOGICAL(SBK),INTENT(IN) :: val
  ENDSUBROUTINE
  SUBROUTINE fmu_doStep_sub_absintfc(self,h)
    IMPORT FMU_Base,SRK
    CLASS(FMU_Base),INTENT(INOUT) :: self
    REAL(SRK),INTENT(IN) :: h
  ENDSUBROUTINE
ENDINTERFACE

!> FMU C Pointer to opaque FMU obj
TYPE(C_PTR),SAVE :: fmu_c_ptr=c_null_ptr
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
    PROCEDURE,PASS :: clear => clear_FMU2_Slave
    PROCEDURE,PASS :: setupExperiment => setupExperiment_FMU2_Slave
    PROCEDURE,PASS :: getReal => getReal_FMU2_Slave
    PROCEDURE,PASS :: setReal => setReal_FMU2_Slave
    PROCEDURE,PASS :: getInteger => getInteger_FMU2_Slave
    PROCEDURE,PASS :: setInteger => setInteger_FMU2_Slave
    PROCEDURE,PASS :: getBoolean => getBoolean_FMU2_Slave
    PROCEDURE,PASS :: setBoolean => setBoolean_FMU2_Slave
    PROCEDURE,PASS :: doStep => doStep_FMU2_Slave
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
!> @param pList A parameter list with input options
!>
!>
SUBROUTINE init_FMU2_Slave(self,id,pList)
  CHARACTER(LEN=*),PARAMETER :: myName='init_FM2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: id
  TYPE(ParamType),INTENT(IN) :: pList
  ! REAL,POINTER :: f_ptr

  ! Required FMU pList
  IF(.NOT. pList%has('guid')) CALL eFMU_Wrapper%raiseError(modName//'::'//myName//' - No FMU guid')
  CALL pList%get('guid', self%guid)
  IF(.NOT. pList%has('unzipDirectory')) CALL eFMU_Wrapper%raiseError(modName//'::'//myName//' - No FMU unzipDirectory')
  CALL pList%get('unzipDirectory', self%unzipDirectory)
  IF(.NOT. pList%has('modelIdentifier')) CALL eFMU_Wrapper%raiseError(modName//'::'//myName//' - No FMU modelIdentifier')
  CALL pList%get('modelIdentifier', self%modelIdentifier)
  ! Optional FMU pList
  IF(pList%has('instanceName')) THEN
    CALL pList%get('instanceName', self%instanceName)
  ELSE
    self%instanceName = "default_fmu_instance"
  ENDIF
  self%idFMU=id

  ! Initilize the FMU
  fmu_c_ptr = InitilizeFMU2_Slave(self%idFMU, &
    CHAR(self%guid)//c_null_char, &
    CHAR(self%modelIdentifier)//c_null_char, &
    CHAR(self%unzipDirectory)//c_null_char, &
    CHAR(self%instanceName)//c_null_char)

  self%isInit=.TRUE.

ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief
!>
!> @param self
!>
SUBROUTINE setupExperiment_FMU2_Slave(self, toleranceDefined, tolerance, startTime, stopTimeDefined, stopTime)
  CHARACTER(LEN=*),PARAMETER :: myName='setupExperiment_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  LOGICAL(SBK),INTENT(IN) :: toleranceDefined
  REAL(SRK),INTENT(IN) :: tolerance
  REAL(SRK),INTENT(IN) :: startTime
  LOGICAL(SBK),INTENT(IN) :: stopTimeDefined
  REAL(SRK),INTENT(IN) :: stopTime

  REQUIRE(self%isInit)
  REQUIRE(c_associated(fmu_c_ptr))

  CALL setupExperimentFMU2_Slave(fmu_c_ptr, LOGICAL(toleranceDefined,1), tolerance, startTime, &
    LOGICAL(stopTimeDefined,1), stopTime)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief
!>
!> @param self
!>
SUBROUTINE getReal_FMU2_Slave(self, valueReference, val)
  CHARACTER(LEN=*),PARAMETER :: myName='getReal_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: valueReference
  REAL(SRK),INTENT(INOUT) :: val

  REQUIRE(self%isInit)
  REQUIRE(c_associated(fmu_c_ptr))

  CALL getRealFMU2_Slave(fmu_c_ptr, valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief
!>
!> @param self
!>
SUBROUTINE setReal_FMU2_Slave(self, valueReference, val)
  CHARACTER(LEN=*),PARAMETER :: myName='setReal_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: valueReference
  REAL(SRK),INTENT(IN) :: val

  REQUIRE(self%isInit)
  REQUIRE(c_associated(fmu_c_ptr))

  CALL setRealFMU2_Slave(fmu_c_ptr, valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief
!>
!> @param self
!>
SUBROUTINE getInteger_FMU2_Slave(self, valueReference, val)
  CHARACTER(LEN=*),PARAMETER :: myName='getInteger_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: valueReference
  INTEGER(SIK),INTENT(INOUT) :: val

  REQUIRE(self%isInit)
  REQUIRE(c_associated(fmu_c_ptr))

  CALL getIntegerFMU2_Slave(fmu_c_ptr, valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief
!>
!> @param self
!>
SUBROUTINE setInteger_FMU2_Slave(self, valueReference, val)
  CHARACTER(LEN=*),PARAMETER :: myName='setInteger_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: valueReference
  INTEGER(SIK),INTENT(IN) :: val

  REQUIRE(self%isInit)
  REQUIRE(c_associated(fmu_c_ptr))

  CALL setIntegerFMU2_Slave(fmu_c_ptr, valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief
!>
!> @param self
!>
SUBROUTINE getBoolean_FMU2_Slave(self, valueReference, val)
  CHARACTER(LEN=*),PARAMETER :: myName='getBoolean_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: valueReference
  LOGICAL(SBK),INTENT(INOUT) :: val
  LOGICAL(1) :: tmp_val

  REQUIRE(self%isInit)
  REQUIRE(c_associated(fmu_c_ptr))

  CALL getBooleanFMU2_Slave(fmu_c_ptr, valueReference, tmp_val)
  val = tmp_val
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief
!>
!> @param self
!>
SUBROUTINE setBoolean_FMU2_Slave(self, valueReference, val)
  CHARACTER(LEN=*),PARAMETER :: myName='setBoolean_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: valueReference
  LOGICAL(SBK),INTENT(IN) :: val

  REQUIRE(self%isInit)
  REQUIRE(c_associated(fmu_c_ptr))

  CALL setBooleanFMU2_Slave(fmu_c_ptr, valueReference, LOGICAL(val,1))
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief
!>
!> @param self
!>
SUBROUTINE doStep_FMU2_Slave(self,h)
  CHARACTER(LEN=*),PARAMETER :: myName='doStep_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  REAL(SRK),INTENT(IN) :: h

  REQUIRE(self%isInit)
  REQUIRE(c_associated(fmu_c_ptr))

  CALL doStepFMU2_Slave(fmu_c_ptr, h)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief
!>
!> @param self
!>
SUBROUTINE clear_FMU2_Slave(self)
  CHARACTER(LEN=*),PARAMETER :: myName='clear_FM2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self

  CALL clearFMU2_Slave(fmu_c_ptr)
  self%isInit=.FALSE.
  fmu_c_ptr = c_null_ptr

  ENSURE(.NOT. c_associated(fmu_c_ptr))

ENDSUBROUTINE

ENDMODULE FMU_Wrapper
