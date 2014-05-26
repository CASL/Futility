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
!> @brief A Fortran 2003 module defining all constants and conversions for
!> different units to be used throughout the code.
!> 
!> All constants used within the code will be contained in this module.
!> This module will contain Neighbor Indexing for all operations.
!> This module will also provide functionality to convert between units of
!> similar types (i.e. Kelvin to Celsius, or inches to centimeters).
!> Functionality may be added to do complex or multiple unit conversion
!> in one subroutine call. (Unsure if this will work).
!> 
!> 
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>
!> @author Brendan Kochunas, Dan Jabaay
!>    @date 12/2/2011
!>
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Constants_Conversion
  
  USE IntrType
  IMPLICIT NONE
  
  REAL(SRK),PARAMETER ::   ZERO=0.000000000000000_SRK !16 digits
  REAL(SRK),PARAMETER ::    ONE=1.000000000000000_SRK !16 digits
  REAL(SRK),PARAMETER ::    TWO=2.000000000000000_SRK !16 digits
  REAL(SRK),PARAMETER ::  QTRPI=7.853981633974483E-1_SRK !16 digits
  REAL(SRK),PARAMETER :: HALFPI=1.570796326794897E+0_SRK !16 digits
  REAL(SRK),PARAMETER ::     PI=3.141592653589793E+0_SRK !16 digits
  REAL(SRK),PARAMETER ::  TWOPI=6.283185307179586E+0_SRK !16 digits
  REAL(SRK),PARAMETER :: FOURPI=1.256637061435917E+1_SRK !16 digits
  !> Avogadro's Number
  REAL(SRK),PARAMETER :: Na=6.02214129000E+23_SRK !16 digits

  !> Direction enumeration
  !INTEGER(SIK),PARAMETER :: WEST=1
  !INTEGER(SIK),PARAMETER :: NORTH=2
  !INTEGER(SIK),PARAMETER :: EAST=3
  !INTEGER(SIK),PARAMETER :: SOUTH=4
  !INTEGER(SIK),PARAMETER :: BOTTOM=5
  !INTEGER(SIK),PARAMETER :: TOP=6
  
  
ENDMODULE Constants_Conversion
