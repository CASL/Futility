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
  !> FMU C Pointer
  TYPE(C_PTR) :: fmu_c_ptr
  !> Model id
  INTEGER(SIK) :: idFMU
  TYPE(StringType) :: guid
  TYPE(StringType) :: modelIdentifier
  TYPE(StringType) :: unzipDirectory
  TYPE(StringType) :: instanceName
!
!List of Type Bound Procedures
  CONTAINS
    !> Defered routine
    PROCEDURE(fmu_init_sub_absintfc),DEFERRED,PASS :: init
ENDTYPE FMU_Base

ABSTRACT INTERFACE
  SUBROUTINE fmu_init_sub_absintfc(self,id,pList)
    IMPORT FMU_Base,SIK,ParamType
    CLASS(FMU_Base),INTENT(INOUT) :: self
    INTEGER(SIK),INTENT(IN) :: id
    TYPE(ParamType),INTENT(IN) :: pList
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
!> @param pList A parameter list with input options
!>
!>
SUBROUTINE init_FMU2_Slave(self,id,pList)
  CHARACTER(LEN=*),PARAMETER :: myName='init_FM2_Slave'
  CLASS(FMU2_Slave),INTENT(INOUT) :: self
  INTEGER(SIK),INTENT(IN) :: id
  TYPE(ParamType),INTENT(IN) :: pList

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
   ! WRITE(*,*) "GUID len: ", LEN(self%guid)
  CALL InitilizeFMU2_Slave(self%fmu_c_ptr, self%idFMU, &
    CHAR(self%guid)//c_null_char, &
    CHAR(self%modelIdentifier)//c_null_char, &
    CHAR(self%unzipDirectory)//c_null_char, &
    CHAR(self%instanceName)//c_null_char)

  self%isInit=.TRUE.

ENDSUBROUTINE

ENDMODULE FMU_Wrapper
