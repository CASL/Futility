!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Module provides an interface to Functional Mock Up Units.
!>
!> Currently supported FMU versions:
!>  - 2.0
!>
!> Provides a thin wrapper to the fmu c interface for use in other
!> fortran modules.
!> The ability to parse the XML file that describes the FMU variables is
!> not implemented here.  See FMU_Wrapper.f90 for the FMU XML parser and
!> high level FMU control operations.
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FMU2_f_api
USE ISO_C_BINDING

  !-------------------------------------------------------------------------------
  ! FMU2 Interface
  !-------------------------------------------------------------------------------
  INTERFACE
    ! When calling from FORTRAN, need to append C_NULL_CHAR to name:
    !    CHAR(my_guid)//c_null_char
    FUNCTION InitilizeFMU2_Base(fmu_id, guid, modelIdentifier, unzipDirectory, instanceName) result(fmu_ptr)bind(C,NAME="InitilizeFMU2_Base")
      USE ISO_C_BINDING
      INTEGER(C_INT),INTENT(IN),VALUE :: fmu_id
      CHARACTER(C_CHAR),INTENT(IN) :: guid
      CHARACTER(C_CHAR),INTENT(IN) :: modelIdentifier
      CHARACTER(C_CHAR),INTENT(IN) :: unzipDirectory
      CHARACTER(C_CHAR),INTENT(IN) :: instanceName
      TYPE(C_PTR) :: fmu_ptr
    ENDFUNCTION

    SUBROUTINE setupExperimentFMU2_Base(fmu_ptr, toleranceDefined, tolerance, startTime, stopTimeDefined, stopTime, finalizeInitialization) bind(C,NAME="setupExperimentFMU2_Base")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
      LOGICAL(C_BOOL),INTENT(IN),VALUE :: toleranceDefined
      REAL(C_DOUBLE),INTENT(IN),VALUE :: tolerance
      REAL(C_DOUBLE),INTENT(IN),VALUE :: startTime
      LOGICAL(C_BOOL),INTENT(IN),VALUE :: stopTimeDefined
      REAL(C_DOUBLE),INTENT(IN),VALUE :: stopTime
      LOGICAL(C_BOOL),INTENT(IN),VALUE :: finalizeInitialization
    ENDSUBROUTINE

    SUBROUTINE getRealFMU2_Base(fmu_ptr, valueReference, val) bind(C,NAME="getRealFMU2_Base")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
      INTEGER(C_INT),INTENT(IN),VALUE :: valueReference
      REAL(C_DOUBLE),INTENT(INOUT) :: val
    ENDSUBROUTINE

    SUBROUTINE setRealFMU2_Base(fmu_ptr, valueReference, val) bind(C,NAME="setRealFMU2_Base")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
      INTEGER(C_INT),INTENT(IN),VALUE :: valueReference
      REAL(C_DOUBLE),INTENT(IN),VALUE :: val
    ENDSUBROUTINE

    SUBROUTINE getIntegerFMU2_Base(fmu_ptr, valueReference, val) bind(C,NAME="getIntegerFMU2_Base")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
      INTEGER(C_INT),INTENT(IN),VALUE :: valueReference
      INTEGER(C_INT),INTENT(INOUT) :: val
    ENDSUBROUTINE

    SUBROUTINE setIntegerFMU2_Base(fmu_ptr, valueReference, val) bind(C,NAME="setIntegerFMU2_Base")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
      INTEGER(C_INT),INTENT(IN),VALUE :: valueReference
      INTEGER(C_INT),INTENT(IN),VALUE :: val
    ENDSUBROUTINE

    SUBROUTINE getBooleanFMU2_Base(fmu_ptr, valueReference, val) bind(C,NAME="getBooleanFMU2_Base")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
      INTEGER(C_INT),INTENT(IN),VALUE :: valueReference
      LOGICAL(C_BOOL),INTENT(INOUT) :: val
    ENDSUBROUTINE

    SUBROUTINE setBooleanFMU2_Base(fmu_ptr, valueReference, val) bind(C,NAME="setBooleanFMU2_Base")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
      INTEGER(C_INT),INTENT(IN),VALUE :: valueReference
      LOGICAL(C_BOOL),INTENT(IN),VALUE :: val
    ENDSUBROUTINE

    SUBROUTINE enterInitializationModeFMU2_Base(fmu_ptr) bind(C,NAME="enterInitializationModeFMU2_Base")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
    ENDSUBROUTINE

    SUBROUTINE exitInitializationModeFMU2_Base(fmu_ptr) bind(C,NAME="exitInitializationModeFMU2_Base")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
    ENDSUBROUTINE

    SUBROUTINE clearFMU2_Base(fmu_ptr) bind(C,NAME="clearFMU2_Base")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
    ENDSUBROUTINE

    ! Methods for Co-Simulation only
    SUBROUTINE setNoRewindFlagFMU2_Slave(fmu_ptr,noRw) bind(C,NAME="setNoRewindFlagFMU2_Slave")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
      LOGICAL(C_BOOl),INTENT(IN),VALUE :: noRw
    ENDSUBROUTINE

    SUBROUTINE doStepFMU2_Slave(fmu_ptr,h) bind(C,NAME="doStepFMU2_Slave")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
      REAL(C_DOUBLE),INTENT(IN),VALUE :: h
    ENDSUBROUTINE

    SUBROUTINE serializeStateFMU2_Slave(fmu_ptr) bind(C,NAME="serializeStateFMU2_Slave")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
    ENDSUBROUTINE

    SUBROUTINE deSerializeStateFMU2_Slave(fmu_ptr) bind(C,NAME="deSerializeStateFMU2_Slave")
      USE ISO_C_BINDING
      TYPE(C_PTR),INTENT(IN),VALUE :: fmu_ptr
    ENDSUBROUTINE

  ENDINTERFACE
ENDMODULE FMU2_f_api
