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
!> The FMU is a zip file contaning an XML file and a *.so library.
!> The file layout of an unzipped FMU is as follows:
!>
!>  FMU_unzipDirectory
!>   |-> binaries
!>   |   |-> linux64
!>   |       |-> modelIdentifier.so
!>   |-> modelDescription.xml
!>
!> Where modelIdentifier and unzipDirectory are specified in the input pList
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
USE FMU2_f_api

IMPLICIT NONE
PRIVATE

!
! List of public members
PUBLIC :: FMU2_Slave
PUBLIC :: eFMU_Wrapper

!> @brief the base fmu type.
!> All FMU implementations must provide init, clear, get/set vars and doStep.
TYPE,ABSTRACT :: FMU_Base
  !> Initialization status
  LOGICAL(SBK) :: isInit=.FALSE.
  !> FMU version
  INTEGER(SIK) :: FMU_version
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
  !> XML derived model description
  TYPE(ParamType) :: modelDescription
  !> FMU C opaque pointer to FMU obj
  TYPE(C_PTR) :: fmu_c_ptr=c_null_ptr
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
    !> Defered getInteger routine
    PROCEDURE(fmu_getInteger_sub_absintfc),DEFERRED,PASS :: getInteger
    !> Defered setInteger routine
    PROCEDURE(fmu_setInteger_sub_absintfc),DEFERRED,PASS :: setInteger
    !> Defered getBoolean routine
    PROCEDURE(fmu_getBoolean_sub_absintfc),DEFERRED,PASS :: getBoolean
    !> Defered setBoolean routine
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

!> @brief FMU run in slave mode intended for use with external driver, such as CTF
TYPE,EXTENDS(FMU_Base) :: FMU2_Slave
!
!List of Type Bound Procedures
  CONTAINS
    !> @copybrief FMU_Wrapper::init_FMU2_Slave
    !> @copydetails FMU_Wrapper::init_FMU2_Slave
    PROCEDURE,PASS :: init => init_FMU2_Slave
    !> @copybrief FMU_Wrapper::clear_FMU2_Slave
    !> @copydetails FMU_Wrapper::clear_FMU2_Slave
    PROCEDURE,PASS :: clear => clear_FMU2_Slave
    !> @copybrief FMU_Wrapper::setupExperiment_FMU2_Slave
    !> @copydetails FMU_Wrapper::setupExperiment_FMU2_Slave
    PROCEDURE,PASS :: setupExperiment => setupExperiment_FMU2_Slave
    !> @copybrief FMU_Wrapper::getReal_FMU2_Slave
    !> @copydetails FMU_Wrapper::getReal_FMU2_Slave
    PROCEDURE,PASS :: getReal => getReal_FMU2_Slave
    !> @copybrief FMU_Wrapper::setReal_FMU2_Slave
    !> @copydetails FMU_Wrapper::setReal_FMU2_Slave
    PROCEDURE,PASS :: setReal => setReal_FMU2_Slave
    !> @copybrief FMU_Wrapper::getInteger_FMU2_Slave
    !> @copydetails FMU_Wrapper::getInteger_FMU2_Slave
    PROCEDURE,PASS :: getInteger => getInteger_FMU2_Slave
    !> @copybrief FMU_Wrapper::setInteger_FMU2_Slave
    !> @copydetails FMU_Wrapper::setInteger_FMU2_Slave
    PROCEDURE,PASS :: setInteger => setInteger_FMU2_Slave
    !> @copybrief FMU_Wrapper::getBoolean_FMU2_Slave
    !> @copydetails FMU_Wrapper::getBoolean_FMU2_Slave
    PROCEDURE,PASS :: getBoolean => getBoolean_FMU2_Slave
    !> @copybrief FMU_Wrapper::setBoolean_FMU2_Slave
    !> @copydetails FMU_Wrapper::setBoolean_FMU2_Slave
    PROCEDURE,PASS :: setBoolean => setBoolean_FMU2_Slave
    !> @copybrief FMU_Wrapper::doStep_FMU2_Slave
    !> @copydetails FMU_Wrapper::doStep_FMU2_Slave
    PROCEDURE,PASS :: doStep => doStep_FMU2_Slave
    !> @copybrief FMU_Wrapper::setNoRewindFlag_FMU2_Slave
    !> @copydetails FMU_Wrapper::setNoRewindFlag_FMU2_Slave
    PROCEDURE,PASS :: setNoRewindFlag => setNoRewindFlag_FMU2_Slave
    !> @copybrief FMU_Wrapper::getValueReference_FMU2_Slave
    !> @copydetails FMU_Wrapper::getValueReference_FMU2_Slave
    PROCEDURE,PASS :: getValueReference => getValueReference_FMU2_Slave
    !> @copybrief FMU_Wrapper::isXmlVar_FMU2_Slave
    !> @copydetails FMU_Wrapper::isXmlVar_FMU2_Slave
    PROCEDURE,PASS :: isXmlVar => isXmlVar_FMU2_Slave
    !> @copybrief FMU_Wrapper::getCausality_FMU2_Slave
    !> @copydetails FMU_Wrapper::getCausality_FMU2_Slave
    PROCEDURE,PASS :: getCausality => getCausality_FMU2_Slave
    !> @copybrief FMU_Wrapper::setRestart_FMU2_Slave
    !> @copydetails FMU_Wrapper::setRestart_FMU2_Slave
    PROCEDURE,PASS :: setRestart => setRestart_FMU2_Slave
    !> @copybrief FMU_Wrapper::rewindToRestart_FMU2_Slave
    !> @copydetails FMU_Wrapper::rewindToRestart_FMU2_Slave
    PROCEDURE,PASS :: rewindToRestart => rewindToRestart_FMU2_Slave
    !
    ! Convinience FMU wrapper getters and setters
    !
    !> @copybrief FMU_Wrapper::getNamedReal_FMU2_Slave
    !> @copydetails FMU_Wrapper::getNamedReal_FMU2_Slave
    PROCEDURE,PASS,PRIVATE :: getNamedReal_FMU2_Slave
    !> @copybrief FMU_Wrapper::setNamedReal_FMU2_Slave
    !> @copydetails FMU_Wrapper::setNamedReal_FMU2_Slave
    PROCEDURE,PASS,PRIVATE :: setNamedReal_FMU2_Slave
    !> @copybrief FMU_Wrapper::getNamedInteger_FMU2_Slave
    !> @copydetails FMU_Wrapper::getNamedInteger_FMU2_Slave
    PROCEDURE,PASS,PRIVATE :: getNamedInteger_FMU2_Slave
    !> @copybrief FMU_Wrapper::setNamedInteger_FMU2_Slave
    !> @copydetails FMU_Wrapper::setNamedInteger_FMU2_Slave
    PROCEDURE,PASS,PRIVATE :: setNamedInteger_FMU2_Slave
    !> @copybrief FMU_Wrapper::getNamedBoolean_FMU2_Slave
    !> @copydetails FMU_Wrapper::getNamedBoolean_FMU2_Slave
    PROCEDURE,PASS,PRIVATE :: getNamedBoolean_FMU2_Slave
    !> @copybrief FMU_Wrapper::setNamedBoolean_FMU2_Slave
    !> @copydetails FMU_Wrapper::setNamedBoolean_FMU2_Slave
    PROCEDURE,PASS,PRIVATE :: setNamedBoolean_FMU2_Slave
    GENERIC :: getNamedVariable => getNamedReal_FMU2_Slave, &
        getNamedInteger_FMU2_Slave, getNamedBoolean_FMU2_Slave
    GENERIC :: setNamedVariable => setNamedReal_FMU2_Slave, &
        setNamedInteger_FMU2_Slave, setNamedBoolean_FMU2_Slave
ENDTYPE FMU2_Slave

!> Exception Handler for use in the FMU_Wrapper
TYPE(ExceptionHandlerType),SAVE :: eFMU_Wrapper

!> Name of module
CHARACTER(LEN=*),PARAMETER :: modName='FMU_Wrapper'

!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes the FMU Slave
!>
!> @param self the FMU2_Slave to act on
!> @param id Unique id for this FMU
!> @param pList A parameter list with input options
!>
SUBROUTINE init_FMU2_Slave(self,id,pList)
  CHARACTER(LEN=*),PARAMETER :: myName='init_FM2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: id
  TYPE(ParamType),INTENT(IN) :: pList

  ! Required FMU pList
  IF(.NOT. pList%has('unzipDirectory')) CALL eFMU_Wrapper%raiseError(modName//'::'//myName//' - No FMU unzipDirectory')
  CALL pList%get('unzipDirectory', self%unzipDirectory)
  self%idFMU=id
  self%FMU_version=2_SIK

  ! Parse the FMU XML model description
  CALL eFMU_Wrapper%raiseDebug('Opening FMU XML File: '//self%unzipDirectory//'/modelDescription.xml' )
  CALL self%modelDescription%initFromXML(self%unzipDirectory//'/modelDescription.xml',.TRUE.)

  ! Optional FMU pList
  IF(pList%has('instanceName')) THEN
    CALL pList%get('instanceName', self%instanceName)
  ELSE
    self%instanceName = "default_fmu_instance"
  ENDIF
  IF(pList%has('modelIdentifier')) THEN
    CALL pList%get('modelIdentifier', self%modelIdentifier)
  ELSE
    CALL self%modelDescription%get('modelIdentifier', self%modelIdentifier)
  ENDIF
  IF(pList%has('guid')) THEN
    CALL pList%get('guid', self%guid)
  ELSE
    CALL self%modelDescription%get('guid', self%guid)
  ENDIF

  ! Initilize the FMU
  self%fmu_c_ptr = InitilizeFMU2_Slave(self%idFMU, &
    CHAR(self%guid)//c_null_char, &
    CHAR(self%modelIdentifier)//c_null_char, &
    CHAR(self%unzipDirectory)//c_null_char, &
    CHAR(self%instanceName)//c_null_char)

  self%isInit=.TRUE.

ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Set the FMU internal ODE integrator settings
!>
!> @param self the FMU2_Slave to act on
!> @param toleranceDefined flag to override internal FMU ODE tolerance settings
!> @param tolerance tolerance of FMU ODE integrator
!> @param startTime start time of FMU ODE integration
!> @param stopTimeDefined flag to determine if ODE integration stops at stopTime
!> @param stopTime  end time of ODE integration
!>
SUBROUTINE setupExperiment_FMU2_Slave(self, toleranceDefined, tolerance, startTime, stopTimeDefined, stopTime)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  LOGICAL(SBK),INTENT(IN) :: toleranceDefined
  REAL(SRK),INTENT(IN) :: tolerance
  REAL(SRK),INTENT(IN) :: startTime
  LOGICAL(SBK),INTENT(IN) :: stopTimeDefined
  REAL(SRK),INTENT(IN) :: stopTime

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  CALL setupExperimentFMU2_Slave(self%fmu_c_ptr, LOGICAL(toleranceDefined,1), tolerance, startTime, &
    LOGICAL(stopTimeDefined,1), stopTime)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Get the valueReference that corresponds to the FMU variableName
!>
!> @param self the FMU2_Slave to act on
!> @param variableName name of FMU variable
!> @returns valueReference the value reference of variable in the FMU.
!>          The valueReference is an integer handle to an FMU variable.
!>
FUNCTION getValueReference_FMU2_Slave(self, variableName) RESULT(valueReference)
  CHARACTER(LEN=*),PARAMETER :: myName='getValueReference_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  TYPE(StringType),INTENT(IN) :: variableName
  INTEGER(SIK) :: valueReference

  TYPE(StringType) :: valueReference_str
  TYPE(StringType) :: baseAddr

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  ! check that requrested variable exists in the modelDescription
  baseAddr='MODELVARIABLES->'//variableName
  IF(self%isXmlVar(variableName)) THEN
    ! get valueReference assc with this variableName
    CALL self%modelDescription%get(baseAddr//'->valueReference', valueReference_str)
    ! convert str to int and check valueReference is valid
    valueReference = valueReference_str%str_to_sik()
    IF(valueReference<0) CALL eFMU_Wrapper%raiseError(modName//'::'//' - variable has invalid valueReference')
  ELSE
    CALL eFMU_Wrapper%raiseError(modName//'::'//myName//' - No Variable named: '//variableName)
  ENDIF
ENDFUNCTION
!
!-------------------------------------------------------------------------------
!> @brief Check if variableName is in the FMU XML file
!>
!> @param self the FMU2_Slave to act on
!> @param variableName name of FMU variable
!> @returns isVar boolean if the FMU XML contains the variableName
!>
FUNCTION isXmlVar_FMU2_Slave(self, variableName) RESULT(isVar)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  TYPE(StringType),INTENT(IN) :: variableName
  LOGICAL(SBK) :: isVar

  REQUIRE(self%isInit)

  ! check that requrested variable exists in the modelDescription
  IF(self%modelDescription%has(CHAR(variableName))) THEN
    isVar=.TRUE.
  ELSE
    isVar=.FALSE.
  ENDIF

ENDFUNCTION
!
!-------------------------------------------------------------------------------
!> @brief Get the causality of the FMU variable
!>
!> @param self the FMU2_Slave to act on
!> @param variableName name of FMU variable
!> @returns causality the FMU variable causality.  The causality is an FMU
!>          variable property that signifies how the FMU variable obtains
!>          its value.  See fmi-standard.org for additional info.
!>
FUNCTION getCausality_FMU2_Slave(self, variableName) RESULT(causality)
  CHARACTER(LEN=*),PARAMETER :: myName='getCausality_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  TYPE(StringType),INTENT(IN) :: variableName
  TYPE(StringType) :: causality

  TYPE(StringType) :: baseAddr
  TYPE(StringType) :: causalityAddr

  REQUIRE(self%isInit)

  ! check that requrested variable exists in the modelDescription
  baseAddr='MODELVARIABLES->'//variableName
  IF(self%isXmlVar(variableName)) THEN
    ! get valueReference assc with this variableName
    causalityAddr = baseAddr//'->causality'
    IF(self%modelDescription%has(CHAR(causalityAddr))) THEN
      CALL self%modelDescription%get(CHAR(causalityAddr), causality)
    ELSE
      ! default causality is local as defined by FMI standard
      causality='local'
    ENDIF
  ELSE
    CALL eFMU_Wrapper%raiseError(modName//'::'//myName//' - No Variable named: '//variableName)
  ENDIF
ENDFUNCTION
!
!-------------------------------------------------------------------------------
!> @brief Get real FMU variable
!>
!> @param self the FMU2_Slave to act on
!> @param valueReference the integer identifer of the FMU variable
!> @param val variable value
!>
SUBROUTINE getReal_FMU2_Slave(self, valueReference, val)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: valueReference
  REAL(SRK),INTENT(INOUT) :: val

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  CALL getRealFMU2_Slave(self%fmu_c_ptr, valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Set real FMU variable
!>
!> @param self the FMU2_Slave to act on
!> @param valueReference the integer identifer of the FMU variable
!> @param val variable value
!>
SUBROUTINE setReal_FMU2_Slave(self, valueReference, val)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: valueReference
  REAL(SRK),INTENT(IN) :: val

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  CALL setRealFMU2_Slave(self%fmu_c_ptr, valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Get integer FMU variable
!>
!> @param self the FMU2_Slave to act on
!> @param valueReference the integer identifer of the FMU variable
!> @param val variable value
!>
SUBROUTINE getInteger_FMU2_Slave(self, valueReference, val)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: valueReference
  INTEGER(SIK),INTENT(INOUT) :: val

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  CALL getIntegerFMU2_Slave(self%fmu_c_ptr, valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Set integer FMU variable
!>
!> @param self the FMU2_Slave to act on
!> @param valueReference the integer identifer of the FMU variable
!> @param val variable value
!>
SUBROUTINE setInteger_FMU2_Slave(self, valueReference, val)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: valueReference
  INTEGER(SIK),INTENT(IN) :: val

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  CALL setIntegerFMU2_Slave(self%fmu_c_ptr, valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Get boolean FMU variable
!>
!> @param self the FMU2_Slave to act on
!> @param valueReference the integer identifer of the FMU variable
!> @param val variable value
!>
SUBROUTINE getBoolean_FMU2_Slave(self, valueReference, val)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: valueReference
  LOGICAL(SBK),INTENT(INOUT) :: val
  LOGICAL(1) :: tmp_val

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  CALL getBooleanFMU2_Slave(self%fmu_c_ptr, valueReference, tmp_val)
  val = tmp_val
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Set boolean FMU variable
!>
!> @param self the FMU2_Slave to act on
!> @param valueReference the integer identifer of the FMU variable
!> @param val variable value
!>
SUBROUTINE setBoolean_FMU2_Slave(self, valueReference, val)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: valueReference
  LOGICAL(SBK),INTENT(IN) :: val

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  CALL setBooleanFMU2_Slave(self%fmu_c_ptr, valueReference, LOGICAL(val,1))
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Set flag to disable FMU rewind capability
!>
!> @param self the FMU2_Slave to act on
!> @param noRw flag to disable rewind capability. Default in FMU2_Slave is
!>   to allow the FMU to be rewound
!>
!> Depending on the FMU implementation, this flag may be ignored
SUBROUTINE setNoRewindFlag_FMU2_Slave(self,noRw)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  LOGICAL(SBK),INTENT(IN) :: noRw

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  CALL setNoRewindFlagFMU2_Slave(self%fmu_c_ptr, LOGICAL(noRw,1))
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Step the FMU model forward in time
!>
!> @param self the FMU2_Slave to act on
!> @param h time step size
!>
SUBROUTINE doStep_FMU2_Slave(self,h)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  REAL(SRK),INTENT(IN) :: h

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  CALL doStepFMU2_Slave(self%fmu_c_ptr, h)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Set FMU model restart point
!>
!> @param self the FMU2_Slave to act on
!>
SUBROUTINE setRestart_FMU2_Slave(self)
  CHARACTER(LEN=*),PARAMETER :: myName='setRestart_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self

  TYPE(StringType) :: baseAddr
  TYPE(StringType) :: getSetAddr
  TYPE(StringType) :: isGetSet_str

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  ! check that requrested variable exists in the modelDescription
  baseAddr='COSIMULATION'
  getSetAddr=baseAddr//'->canGetAndSetFMUstate'
  IF(self%modelDescription%has(CHAR(getSetAddr))) THEN
    CALL self%modelDescription%get(baseAddr//'->canGetAndSetFMUstate', isGetSet_str)
    IF(.NOT. (isGetSet_str=="true" .OR. isGetSet_str=="True" .OR. isGetSet_str=="TRUE")) &
        CALL eFMU_Wrapper%raiseError(modName//'::'//' - FMU does not support GetSetFMUstate.')
  ELSE
    CALL eFMU_Wrapper%raiseWarning(modName//'::'//myName//' - GetSetFMUstate capability was not detected.')
  ENDIF

  CALL serializeStateFMU2_Slave(self%fmu_c_ptr)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Rewind FMU model to restart point
!>
!> @param self the FMU2_Slave to act on
!>
SUBROUTINE rewindToRestart_FMU2_Slave(self)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  CALL deSerializeStateFMU2_Slave(self%fmu_c_ptr)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Get a real variable in the FMU model
!>
!> @param self the FMU2_Slave to act on
!> @param variableName name of FMU variable
!> @param val variable value
!>
SUBROUTINE getNamedReal_FMU2_Slave(self, variableName, val)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  TYPE(StringType),INTENT(IN) :: variableName
  REAL(SRK),INTENT(OUT) :: val

  INTEGER(SIK) :: valueReference

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  valueReference = self%getValueReference(variableName)
  CALL self%getReal(valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Set a real variable in the FMU model
!>
!> @param self the FMU2_Slave to act on
!> @param variableName name of FMU variable
!> @param val variable value
!>
SUBROUTINE setNamedReal_FMU2_Slave(self, variableName, val)
  CHARACTER(LEN=*),PARAMETER :: myName='setNamedReal_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  TYPE(StringType),INTENT(IN) :: variableName
  REAL(SRK),INTENT(IN) :: val

  INTEGER(SIK) :: valueReference
  TYPE(StringType) :: causality

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  valueReference = self%getValueReference(variableName)
  causality = self%getCausality(variableName)
  IF(.NOT.(causality=='parameter' .OR. causality=='input')) &
      CALL eFMU_Wrapper%raiseWarning(modName//'::'//myName// &
      ' - Attempting to set variable '//variableName//' with causality: '//causality)
  CALL self%setReal(valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Get an integer variable in the FMU model
!>
!> @param self the FMU2_Slave to act on
!> @param variableName name of FMU variable
!> @param val variable value
!>
SUBROUTINE getNamedInteger_FMU2_Slave(self, variableName, val)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  TYPE(StringType),INTENT(IN) :: variableName
  INTEGER(SIK),INTENT(OUT) :: val

  INTEGER(SIK) :: valueReference

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  valueReference = self%getValueReference(variableName)
  CALL self%getInteger(valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Set an integer variable in the FMU model to val
!>
!> @param self the FMU2_Slave to act on
!> @param variableName name of FMU variable
!> @param val variable value
!>
SUBROUTINE setNamedInteger_FMU2_Slave(self, variableName, val)
  CHARACTER(LEN=*),PARAMETER :: myName='setNamedInteger_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  TYPE(StringType),INTENT(IN) :: variableName
  INTEGER(SIK),INTENT(IN) :: val

  INTEGER(SIK) :: valueReference
  TYPE(StringType) :: causality

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  valueReference = self%getValueReference(variableName)
  causality = self%getCausality(variableName)
  IF(.NOT.(causality=='parameter' .OR. causality=='input')) &
      CALL eFMU_Wrapper%raiseWarning(modName//'::'//myName// &
      ' - Attempting to set variable '//variableName//' with causality: '//causality)
  CALL self%setInteger(valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Get a boolean variable in the FMU model to val
!>
!> @param self the FMU2_Slave to act on
!> @param variableName name of FMU variable
!> @param val variable value
!>
SUBROUTINE getNamedBoolean_FMU2_Slave(self, variableName, val)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  TYPE(StringType),INTENT(IN) :: variableName
  LOGICAL(SBK),INTENT(OUT) :: val

  INTEGER(SIK) :: valueReference

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  valueReference = self%getValueReference(variableName)
  CALL self%getBoolean(valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Set a boolean variable in the FMU model to val
!>
!> @param self the FMU2_Slave to act on
!> @param variableName name of FMU variable
!> @param val variable value
!>
SUBROUTINE setNamedBoolean_FMU2_Slave(self, variableName, val)
  CHARACTER(LEN=*),PARAMETER :: myName='setNamedBoolean_FMU2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  TYPE(StringType),INTENT(IN) :: variableName
  LOGICAL(SBK),INTENT(IN) :: val

  INTEGER(SIK) :: valueReference
  TYPE(StringType) :: causality

  REQUIRE(self%isInit)
  REQUIRE(c_associated(self%fmu_c_ptr))

  valueReference = self%getValueReference(variableName)
  causality = self%getCausality(variableName)
  IF(.NOT.(causality=='parameter' .OR. causality=='input')) &
      CALL eFMU_Wrapper%raiseWarning(modName//'::'//myName// &
      ' - Attempting to set variable '//variableName//' with causality: '//causality)
  CALL self%setBoolean(valueReference, val)
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Clear the FMU model
!>
!> @param self the FMU2_Slave to act on
!>
SUBROUTINE clear_FMU2_Slave(self)
  CLASS(FMU2_Slave),INTENT(INOUT) :: self

  CALL clearFMU2_Slave(self%fmu_c_ptr)
  self%isInit=.FALSE.
  self%fmu_c_ptr = c_null_ptr

  CALL self%modelDescription%clear()
  ENSURE(.NOT. c_associated(self%fmu_c_ptr))

ENDSUBROUTINE

ENDMODULE FMU_Wrapper
