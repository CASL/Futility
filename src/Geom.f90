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
!> @brief The global geometry module, collecting all public members of 
!> other geometry modules. This is the module that should be used elsewhere
!> in the code.
!>
!> @par Module Dependencies
!>  - @ref Geom_Points "Geom_Points": @copybrief Geom_Points
!>  - @ref Geom_Line "Geom_Line": @copybrief Geom_Line
!>  - @ref Geom_Plane "Geom_Plane": @copybrief Geom_Plane
!>  - @ref Geom_CircCyl "Geom_CircCyl": @copybrief Geom_CircCyl
!>  - @ref Geom_Box "Geom_Box": @copybrief Geom_Box
!>
!> @author Brendan Kochunas
!>    @date 5/26/2011
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Geom

  USE Geom_Points
  USE Geom_Line
  USE Geom_Plane
  USE Geom_CircCyl
  USE Geom_Box
  IMPLICIT NONE
!
ENDMODULE Geom
