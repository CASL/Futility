!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
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
  PRIVATE

  REAL(SRK),PUBLIC,PARAMETER ::    ZERO=0.000000000000000_SRK !16 digits
  REAL(SRK),PUBLIC,PARAMETER ::     ONE=1.000000000000000_SRK !16 digits
  REAL(SRK),PUBLIC,PARAMETER ::     TWO=2.000000000000000_SRK !16 digits
  REAL(SRK),PUBLIC,PARAMETER ::   QTRPI=ATAN(ONE)
  REAL(SRK),PUBLIC,PARAMETER ::  HALFPI=TWO*QTRPI
  REAL(SRK),PUBLIC,PARAMETER ::      PI=TWO*HALFPI
  REAL(SRK),PUBLIC,PARAMETER ::   TWOPI=TWO*PI
  REAL(SRK),PUBLIC,PARAMETER ::  FOURPI=TWO*TWOPI
  REAL(SRK),PUBLIC,PARAMETER ::     RPI=ONE/PI
  REAL(SRK),PUBLIC,PARAMETER ::  RTWOPI=ONE/TWOPI
  REAL(SRK),PUBLIC,PARAMETER :: RFOURPI=ONE/FOURPI
  !> Avogadro's Number
  REAL(SRK),PUBLIC,PARAMETER :: Na=6.02214129000E+23_SRK !16 digits

  !> Conversion factor from MPa to psia (should move to constants conversion)
  REAL(SRK),PUBLIC,PARAMETER :: MPa2PSIA=145.0_SRK+377.0_SRK/9990.0_SRK

  !> May be used for comparing real numbers relating to geometry
  !> Tolerance is 0.0001 cm
  REAL(SRK),PUBLIC,PARAMETER :: TOL_GEOM_cm=0.0001_SRK
ENDMODULE Constants_Conversion
