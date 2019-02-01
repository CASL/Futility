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

  !> Parameters for commonly used constants
  REAL(SRK),PUBLIC,PARAMETER ::    ZERO=0.000000000000000_SRK !16 digits
  REAL(SRK),PUBLIC,PARAMETER ::     ONE=1.000000000000000_SRK !16 digits
  REAL(SRK),PUBLIC,PARAMETER ::     TWO=2.000000000000000_SRK !16 digits
  REAL(SRK),PUBLIC,PARAMETER ::    HALF=0.500000000000000_SRK !16 digits
  !> Useful fractions of PI
  REAL(SRK),PUBLIC,PARAMETER ::   QTRPI=ATAN(ONE)
  REAL(SRK),PUBLIC,PARAMETER ::  HALFPI=TWO*QTRPI
  REAL(SRK),PUBLIC,PARAMETER ::      PI=TWO*HALFPI
  REAL(SRK),PUBLIC,PARAMETER :: SIXTHPI=PI/6._SRK
  REAL(SRK),PUBLIC,PARAMETER :: THIRDPI=TWO*SIXTHPI
  REAL(SRK),PUBLIC,PARAMETER ::   TWOPI=TWO*PI
  REAL(SRK),PUBLIC,PARAMETER ::  FOURPI=TWO*TWOPI
  REAL(SRK),PUBLIC,PARAMETER ::     RPI=ONE/PI
  REAL(SRK),PUBLIC,PARAMETER ::  RTWOPI=ONE/TWOPI
  REAL(SRK),PUBLIC,PARAMETER :: RFOURPI=ONE/FOURPI

  !> Convert from radians to degrees
  REAL(SRK),PUBLIC,PARAMETER :: rad2degrees=180.0_SRK/PI
  !> Convert from degrees to radians
  REAL(SRK),PUBLIC,PARAMETER :: degrees2rad=PI/180.0_SRK

  !> Avogadro's Number; source: https://physics.nist.gov/cgi-bin/cuu/Value?na
  REAL(SRK),PUBLIC,PARAMETER :: NA=6.022140857E+23_SRK

  ! Area conversion
  !> Conversion factor from barns to cm**2
  REAL(SRK),PUBLIC,PARAMETER :: barn2cm=1.0E-24_SRK
  !> Conversion factor from cm**2 to barns
  REAL(SRK),PUBLIC,PARAMETER :: cm2barn=1.0E24_SRK

  !Pressure Conversions
  !> Conversion factor from MPa to psia
  REAL(SRK),PUBLIC,PARAMETER :: MPa2PSIA=145.0_SRK+377.0_SRK/9990.0_SRK
  !> Conversion factor from bar to MPa
  REAL(SRK),PUBLIC,PARAMETER :: Bar2MPa=0.1_SRK
  !> Conversion factor from MPa to bar
  REAL(SRK),PUBLIC,PARAMETER :: MPa2Bar=ONE/Bar2MPa
  !> Conversion factor from MPa to Pa
  REAL(SRK),PUBLIC,PARAMETER :: MPa2Pa=1000000.0_SRK
  !> Conversion factor from Pa to MPa
  REAL(SRK),PUBLIC,PARAMETER :: Pa2MPa=ONE/MPa2Pa

  !Mass conversions
  !> Conversion factor from kg to g
  REAL(SRK),PUBLIC,PARAMETER :: kg2g=1000.0_SRK
  !> Conversion factor from g to kg
  REAL(SRK),PUBLIC,PARAMETER :: g2kg=ONE/kg2g
  !> Conversion factor from g to mg
  REAL(SRK),PUBLIC,PARAMETER :: g2mg=1000.0_SRK
  !> Conversion factor from mg to g
  REAL(SRK),PUBLIC,PARAMETER :: mg2g=ONE/g2mg

  !Length conversions
  !> Conversion factor from m to cm
  REAL(SRK),PUBLIC,PARAMETER :: m2cm=100.0_SRK
  !> Conversion factor from cm to m
  REAL(SRK),PUBLIC,PARAMETER :: cm2m=ONE/m2cm
  !> Conversion factor from ft to cm
  REAL(SRK),PUBLIC,PARAMETER :: ft2cm=30.48_SRK
  !> Conversion factor from cm to ft
  REAL(SRK),PUBLIC,PARAMETER :: cm2ft=ONE/ft2cm

  !Temperature conversions
  !> Conversion factor from Celsius to Kelvin
  REAL(SRK),PUBLIC,PARAMETER :: C2K=273.15_SRK

  !> May be used for comparing real numbers relating to geometry
  !> Tolerance is 0.0001 cm
  REAL(SRK),PUBLIC,PARAMETER :: TOL_GEOM_cm=0.0001_SRK
ENDMODULE Constants_Conversion
