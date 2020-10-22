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
MODULE FMU_interface
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

  INTERFACE
!-------------------------------------------------------------------------------
! FMU2 Interface
!-------------------------------------------------------------------------------
    ! When calling from FORTRAN, need to append C_NULL_CHAR to name:
    !    CHAR(my_guid)//c_null_char
    FUNCTION InitilizeFMU2_Slave(slave_id, guid, modelIdentifier, unzipDirectory, instanceName) result(slave_ptr)bind(C,NAME="InitilizeFMU2_Slave")
      USE ISO_C_BINDING
      IMPORT :: C_PTR
      INTEGER(C_INT),INTENT(IN),VALUE :: slave_id
      CHARACTER(C_CHAR),INTENT(IN) :: guid
      CHARACTER(C_CHAR),INTENT(IN) :: modelIdentifier
      CHARACTER(C_CHAR),INTENT(IN) :: unzipDirectory
      CHARACTER(C_CHAR),INTENT(IN) :: instanceName
      TYPE(C_PTR) :: slave_ptr
    ENDFUNCTION

    SUBROUTINE setupExperimentFMU2_Slave(slave_ptr, toleranceDefined, tolerance, startTime, stopTimeDefined, stopTime) bind(C,NAME="setupExperimentFMU2_Slave")
      USE ISO_C_BINDING
      IMPORT :: C_PTR
      TYPE(C_PTR),INTENT(IN),VALUE :: slave_ptr
      LOGICAL(C_BOOL),INTENT(IN) :: toleranceDefined
      REAL(C_DOUBLE),INTENT(IN) :: tolerance
      REAL(C_DOUBLE),INTENT(IN) :: startTime
      LOGICAL(C_BOOL),INTENT(IN) :: stopTimeDefined
      REAL(C_DOUBLE),INTENT(IN) :: stopTime
    ENDSUBROUTINE

    SUBROUTINE getRealFMU2_Slave(slave_ptr, valueReference, val) bind(C,NAME="getRealFMU2_Slave")
      USE ISO_C_BINDING
      IMPORT :: C_PTR
      TYPE(C_PTR),INTENT(IN),VALUE :: slave_ptr
      INTEGER(C_INT),INTENT(IN),VALUE :: valueReference
      REAL(C_DOUBLE),INTENT(INOUT) :: val
    ENDSUBROUTINE

    SUBROUTINE setRealFMU2_Slave(slave_ptr, valueReference, val) bind(C,NAME="setRealFMU2_Slave")
      USE ISO_C_BINDING
      IMPORT :: C_PTR
      TYPE(C_PTR),INTENT(IN),VALUE :: slave_ptr
      INTEGER(C_INT),INTENT(IN) :: valueReference
      REAL(C_DOUBLE),INTENT(IN) :: val
    ENDSUBROUTINE

    SUBROUTINE doStepFMU2_Slave(slave_ptr,h) bind(C,NAME="doStepFMU2_Slave")
      USE ISO_C_BINDING
      IMPORT :: C_PTR
      TYPE(C_PTR),INTENT(IN),VALUE :: slave_ptr
      REAL(C_DOUBLE),INTENT(IN),VALUE :: h
    ENDSUBROUTINE

  ENDINTERFACE
ENDMODULE FMU_interface
