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
!> @brief 
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief ExceptionHandler
!>
!> @author Daniel Jabaay
!>    @date 6/4/2013
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE DTK_Base
  USE ISO_C_BINDING
  USE IntrType
  USE ExceptionHandler
  
  IMPLICIT NONE
  PRIVATE
  
  PUBLIC :: getGeom
  
  !> The module name
  CHARACTER(LEN=*),PARAMETER :: modName='DTK_BASE'
  
  !> The module exception handler
  TYPE(ExceptionHandlerType),POINTER,SAVE :: eDTK => NULL()
  
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief 
!> @param myModMesh Modular Mesh object to use to calculate pin positions in the
!>        global core geometry
!>
    SUBROUTINE getGeom()
      CHARACTER(LEN=*),PARAMETER :: myName='getGeom'
      !TYPE(ModMeshType),POINTER,INTENT(IN) :: myModMesh
      
      LOGICAL(SBK) :: localalloc
      !CLASS(CoreMeshType),POINTER :: coremesh => NULL()
      
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(eDTK)) THEN
        localalloc=.TRUE.
        ALLOCATE(eDTK)
      ENDIF
      
      !IF(.NOT. myModMesh%isinit) THEN
      !  CALL eDTK%raiseError(modName//'::'//myName// &
      !    ' - Modular Mesh is not initialized!')
      !ELSE
      !  coremesh => myModMesh%core
      !  
      !ENDIF
      
      
    ENDSUBROUTINE getGeom
    
!
ENDMODULE DTK_Base
