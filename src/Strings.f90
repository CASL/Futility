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
!> @brief Module for specifying a type for arbitrary length strings
!> 
!>
!>
!> @par EXAMPLE
!> @code
!> PROGRAM
!>   USE Strings
!>   IMPLICIT NONE
!>  
!>   
!>   ! ... some executable code ...
!>
!> END PROGRAM
!> @endcode
!>
!> @author Brendan Kochunas
!>   @date 07/25/2012
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Strings
  
  USE IntrType
  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: StringType
  PUBLIC :: ASSIGNMENT(=)
  
  !> Derived type for an arbitrary length string
  TYPE :: StringType
    !> The string stored as an array of length 1 character strings
    CHARACTER(LEN=1),ALLOCATABLE,PRIVATE :: s(:)
!
!List of type bound procedures
    CONTAINS
      !> @copybrief Strings::length_StringType
      !> @copydetails Strings::length_StringType
      PROCEDURE,PASS :: length => length_StringType
      !> @copybrief Strings::length_StringType
      !> @copydetails Strings::length_StringType
      PROCEDURE,PASS :: printStr => printStr_StringType
  ENDTYPE StringType

  !> @brief Overloads the assignment operator.  
  INTERFACE ASSIGNMENT(=)
    MODULE PROCEDURE assign_char_to_StringType
    MODULE PROCEDURE assign_StringType_to_char
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    ELEMENTAL FUNCTION length_StringType(thisStr) RESULT(n)
      CLASS(StringType),INTENT(IN) :: thisStr
      INTEGER(SIK) :: n
      n=0
      IF(ALLOCATED(thisStr%s)) n=SIZE(thisStr%s)
    ENDFUNCTION length_StringType
!
!-------------------------------------------------------------------------------
    PURE FUNCTION printStr_StringType(thisStr) RESULT(s)
      CLASS(StringType),INTENT(IN) :: thisStr
      CHARACTER(LEN=SIZE(thisStr%s)) :: s
      INTEGER(SIK) :: i
      s=''
      DO i=1,LEN(s)
        s(i:i)=thisStr%s(i)
      ENDDO
    ENDFUNCTION printStr_StringType
!
!-------------------------------------------------------------------------------
    PURE SUBROUTINE assign_StringType_to_char(s,thisStr)
      CHARACTER(LEN=*),INTENT(INOUT) :: s  
      CLASS(StringType),INTENT(IN) :: thisStr
      s=''
      s=thisStr%printStr()
    ENDSUBROUTINE assign_StringType_to_char
!
!-------------------------------------------------------------------------------
    PURE SUBROUTINE assign_char_to_StringType(thisStr,s)
      CLASS(StringType),INTENT(INOUT) :: thisStr
      CHARACTER(LEN=*),INTENT(IN) :: s
      INTEGER(SIK) :: i
      
      IF(ALLOCATED(thisStr%s)) DEALLOCATE(thisStr%s)
      ALLOCATE(thisStr%s(LEN(s)))
      DO i=1,LEN(s)
        thisStr%s(i)=s(i:i)
      ENDDO
    ENDSUBROUTINE assign_char_to_StringType
!
ENDMODULE Strings
