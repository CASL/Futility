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
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE Constants_Conversion
USE IntrType
IMPLICIT NONE
PRIVATE

PUBLIC :: K_to_C
PUBLIC :: C_to_K
PUBLIC :: F_to_C
PUBLIC :: C_to_F
PUBLIC :: F_to_K
PUBLIC :: K_to_F
PUBLIC :: tempTo_K
PUBLIC :: tempTo_C
PUBLIC :: tempTo_F
PUBLIC :: tempTo_R
PUBLIC :: convertTemp

!> Parameters for commonly used constants
REAL(SRK),PUBLIC,PARAMETER ::    ZERO=0.000000000000000_SRK !16 digits
REAL(SRK),PUBLIC,PARAMETER ::     ONE=1.000000000000000_SRK !16 digits
REAL(SRK),PUBLIC,PARAMETER ::     TWO=2.000000000000000_SRK !16 digits
REAL(SRK),PUBLIC,PARAMETER ::   THREE=3.000000000000000_SRK !16 digits
REAL(SRK),PUBLIC,PARAMETER ::    FOUR=4.000000000000000_SRK !16 digits
REAL(SRK),PUBLIC,PARAMETER ::    FIVE=5.000000000000000_SRK !16 digits
REAL(SRK),PUBLIC,PARAMETER ::     SIX=6.000000000000000_SRK !16 digits
REAL(SRK),PUBLIC,PARAMETER ::   SEVEN=7.000000000000000_SRK !16 digits
REAL(SRK),PUBLIC,PARAMETER ::   EIGHT=8.000000000000000_SRK !16 digits
REAL(SRK),PUBLIC,PARAMETER ::    NINE=9.000000000000000_SRK !16 digits
REAL(SRK),PUBLIC,PARAMETER ::     TEN=10.000000000000000_SRK !16 digits
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

!> Gravity (m/s^2); source: https://en.wikipedia.org/wiki/Gravity_of_Earth
REAL(SRK),PUBLIC,PARAMETER :: GRAV=9.80665_SRK

! Area conversion
!> Conversion factor from barns to cm**2
REAL(SRK),PUBLIC,PARAMETER :: barn2cm=1.0E-24_SRK
!> Conversion factor from cm**2 to barns
REAL(SRK),PUBLIC,PARAMETER :: cm2barn=1.0E24_SRK

!> Conversions for metric units for all combinations from pico to tera.
REAL(SRK),PUBLIC,PARAMETER ::   tera2base=1.000000000000000E+12_SRK
REAL(SRK),PUBLIC,PARAMETER ::   giga2base=1.000000000000000E+09_SRK
REAL(SRK),PUBLIC,PARAMETER ::   mega2base=1.000000000000000E+06_SRK
REAL(SRK),PUBLIC,PARAMETER ::   kilo2base=1.000000000000000E+03_SRK
REAL(SRK),PUBLIC,PARAMETER ::  centi2base=1.000000000000000E-02_SRK
REAL(SRK),PUBLIC,PARAMETER ::  milli2base=1.000000000000000E-03_SRK
REAL(SRK),PUBLIC,PARAMETER ::  micro2base=1.000000000000000E-06_SRK
REAL(SRK),PUBLIC,PARAMETER ::   nano2base=1.000000000000000E-09_SRK
REAL(SRK),PUBLIC,PARAMETER ::   pico2base=1.000000000000000E-12_SRK
REAL(SRK),PUBLIC,PARAMETER ::   base2tera=ONE/tera2base
REAL(SRK),PUBLIC,PARAMETER ::   base2giga=ONE/giga2base
REAL(SRK),PUBLIC,PARAMETER ::   base2mega=ONE/mega2base
REAL(SRK),PUBLIC,PARAMETER ::   base2kilo=ONE/kilo2base
REAL(SRK),PUBLIC,PARAMETER ::  base2centi=ONE/centi2base
REAL(SRK),PUBLIC,PARAMETER ::  base2milli=ONE/milli2base
REAL(SRK),PUBLIC,PARAMETER ::  base2micro=ONE/micro2base
REAL(SRK),PUBLIC,PARAMETER ::   base2nano=ONE/nano2base
REAL(SRK),PUBLIC,PARAMETER ::   base2pico=ONE/pico2base
REAL(SRK),PUBLIC,PARAMETER ::   tera2giga=tera2base*base2giga
REAL(SRK),PUBLIC,PARAMETER ::   tera2mega=tera2base*base2mega
REAL(SRK),PUBLIC,PARAMETER ::   tera2kilo=tera2base*base2kilo
REAL(SRK),PUBLIC,PARAMETER ::  tera2centi=tera2base*base2centi
REAL(SRK),PUBLIC,PARAMETER ::  tera2milli=tera2base*base2milli
REAL(SRK),PUBLIC,PARAMETER ::  tera2micro=tera2base*base2micro
REAL(SRK),PUBLIC,PARAMETER ::   tera2nano=tera2base*base2nano
REAL(SRK),PUBLIC,PARAMETER ::   tera2pico=tera2base*base2pico
REAL(SRK),PUBLIC,PARAMETER ::   giga2tera=giga2base*base2tera
REAL(SRK),PUBLIC,PARAMETER ::   giga2mega=giga2base*base2mega
REAL(SRK),PUBLIC,PARAMETER ::   giga2kilo=giga2base*base2kilo
REAL(SRK),PUBLIC,PARAMETER ::  giga2centi=giga2base*base2centi
REAL(SRK),PUBLIC,PARAMETER ::  giga2milli=giga2base*base2milli
REAL(SRK),PUBLIC,PARAMETER ::  giga2micro=giga2base*base2micro
REAL(SRK),PUBLIC,PARAMETER ::   giga2nano=giga2base*base2nano
REAL(SRK),PUBLIC,PARAMETER ::   giga2pico=giga2base*base2pico
REAL(SRK),PUBLIC,PARAMETER ::   mega2tera=mega2base*base2tera
REAL(SRK),PUBLIC,PARAMETER ::   mega2giga=mega2base*base2giga
REAL(SRK),PUBLIC,PARAMETER ::   mega2kilo=mega2base*base2kilo
REAL(SRK),PUBLIC,PARAMETER ::  mega2centi=mega2base*base2centi
REAL(SRK),PUBLIC,PARAMETER ::  mega2milli=mega2base*base2milli
REAL(SRK),PUBLIC,PARAMETER ::  mega2micro=mega2base*base2micro
REAL(SRK),PUBLIC,PARAMETER ::   mega2nano=mega2base*base2nano
REAL(SRK),PUBLIC,PARAMETER ::   mega2pico=mega2base*base2pico
REAL(SRK),PUBLIC,PARAMETER ::   kilo2tera=kilo2base*base2tera
REAL(SRK),PUBLIC,PARAMETER ::   kilo2giga=kilo2base*base2giga
REAL(SRK),PUBLIC,PARAMETER ::   kilo2mega=kilo2base*base2mega
REAL(SRK),PUBLIC,PARAMETER ::  kilo2centi=kilo2base*base2centi
REAL(SRK),PUBLIC,PARAMETER ::  kilo2milli=kilo2base*base2milli
REAL(SRK),PUBLIC,PARAMETER ::  kilo2micro=kilo2base*base2micro
REAL(SRK),PUBLIC,PARAMETER ::   kilo2nano=kilo2base*base2nano
REAL(SRK),PUBLIC,PARAMETER ::   kilo2pico=kilo2base*base2pico
REAL(SRK),PUBLIC,PARAMETER ::  centi2tera=centi2base*base2tera
REAL(SRK),PUBLIC,PARAMETER ::  centi2giga=centi2base*base2giga
REAL(SRK),PUBLIC,PARAMETER ::  centi2mega=centi2base*base2mega
REAL(SRK),PUBLIC,PARAMETER ::  centi2kilo=centi2base*base2kilo
REAL(SRK),PUBLIC,PARAMETER :: centi2milli=centi2base*base2milli
REAL(SRK),PUBLIC,PARAMETER :: centi2micro=centi2base*base2micro
REAL(SRK),PUBLIC,PARAMETER ::  centi2nano=centi2base*base2nano
REAL(SRK),PUBLIC,PARAMETER ::  centi2pico=centi2base*base2pico
REAL(SRK),PUBLIC,PARAMETER ::  milli2tera=milli2base*base2tera
REAL(SRK),PUBLIC,PARAMETER ::  milli2giga=milli2base*base2giga
REAL(SRK),PUBLIC,PARAMETER ::  milli2mega=milli2base*base2mega
REAL(SRK),PUBLIC,PARAMETER ::  milli2kilo=milli2base*base2kilo
REAL(SRK),PUBLIC,PARAMETER :: milli2centi=milli2base*base2centi
REAL(SRK),PUBLIC,PARAMETER :: milli2micro=milli2base*base2micro
REAL(SRK),PUBLIC,PARAMETER ::  milli2nano=milli2base*base2nano
REAL(SRK),PUBLIC,PARAMETER ::  milli2pico=milli2base*base2pico
REAL(SRK),PUBLIC,PARAMETER ::  micro2tera=micro2base*base2tera
REAL(SRK),PUBLIC,PARAMETER ::  micro2giga=micro2base*base2giga
REAL(SRK),PUBLIC,PARAMETER ::  micro2mega=micro2base*base2mega
REAL(SRK),PUBLIC,PARAMETER ::  micro2kilo=micro2base*base2kilo
REAL(SRK),PUBLIC,PARAMETER :: micro2centi=micro2base*base2centi
REAL(SRK),PUBLIC,PARAMETER :: micro2milli=micro2base*base2milli
REAL(SRK),PUBLIC,PARAMETER ::  micro2nano=micro2base*base2nano
REAL(SRK),PUBLIC,PARAMETER ::  micro2pico=micro2base*base2pico
REAL(SRK),PUBLIC,PARAMETER ::   nano2tera=nano2base*base2tera
REAL(SRK),PUBLIC,PARAMETER ::   nano2giga=nano2base*base2giga
REAL(SRK),PUBLIC,PARAMETER ::   nano2mega=nano2base*base2mega
REAL(SRK),PUBLIC,PARAMETER ::   nano2kilo=nano2base*base2kilo
REAL(SRK),PUBLIC,PARAMETER ::  nano2centi=nano2base*base2centi
REAL(SRK),PUBLIC,PARAMETER ::  nano2milli=nano2base*base2milli
REAL(SRK),PUBLIC,PARAMETER ::  nano2micro=nano2base*base2micro
REAL(SRK),PUBLIC,PARAMETER ::   nano2pico=nano2base*base2pico
REAL(SRK),PUBLIC,PARAMETER ::   pico2tera=pico2base*base2tera
REAL(SRK),PUBLIC,PARAMETER ::   pico2giga=pico2base*base2giga
REAL(SRK),PUBLIC,PARAMETER ::   pico2mega=pico2base*base2mega
REAL(SRK),PUBLIC,PARAMETER ::   pico2kilo=pico2base*base2kilo
REAL(SRK),PUBLIC,PARAMETER ::  pico2centi=pico2base*base2centi
REAL(SRK),PUBLIC,PARAMETER ::  pico2milli=pico2base*base2milli
REAL(SRK),PUBLIC,PARAMETER ::  pico2micro=pico2base*base2micro
REAL(SRK),PUBLIC,PARAMETER ::   pico2nano=pico2base*base2nano

!Pressure Conversions
!> Conversion factor from MPa to psia
REAL(SRK),PUBLIC,PARAMETER :: MPa2PSIA=145.0_SRK+377.0_SRK/9990.0_SRK
!> Conversion factor from psia to MPa
REAL(SRK),PUBLIC,PARAMETER :: PSIA2MPa=ONE/MPa2PSIA
!> Conversion factor from bar to MPa
REAL(SRK),PUBLIC,PARAMETER :: Bar2MPa=0.1_SRK
!> Conversion factor from MPa to bar
REAL(SRK),PUBLIC,PARAMETER :: MPa2Bar=ONE/Bar2MPa
!> Conversion factor from MPa to Pa
REAL(SRK),PUBLIC,PARAMETER :: MPa2Pa=mega2base
!> Conversion factor from Pa to MPa
REAL(SRK),PUBLIC,PARAMETER :: Pa2MPa=base2mega

!Mass conversions
!> Conversion factor from kg to g
REAL(SRK),PUBLIC,PARAMETER :: kg2g=kilo2base
!> Conversion factor from g to kg
REAL(SRK),PUBLIC,PARAMETER :: g2kg=base2kilo
!> Conversion factor from g to mg
REAL(SRK),PUBLIC,PARAMETER :: g2mg=base2milli
!> Conversion factor from mg to g
REAL(SRK),PUBLIC,PARAMETER :: mg2g=milli2base
!> Conversion factor from kg to lbm
REAL(SRK),PUBLIC,PARAMETER :: kg2lb=2.2046226218488_SRK
!> Conversion factor from lbm to kg
REAL(SRK),PUBLIC,PARAMETER :: lb2kg=ONE/kg2lb

!Energy conversions
!> Conversion factor from eV to J
REAL(SRK),PUBLIC,PARAMETER :: eV2J=1.602176487e-19_SRK
!> Conversion factor from J to eV
REAL(SRK),PUBLIC,PARAMETER :: J2eV=ONE/eV2J
!> Conversion factor from BTU to kJ
REAL(SRK),PUBLIC,PARAMETER :: BTU2kJ=1.05505585262_SRK
!> Conversion factor from kJ to BTU
REAL(SRK),PUBLIC,PARAMETER :: kJ2BTU=ONE/BTU2kJ


!Time conversions
!> Conversion factor from min to s
REAL(SRK),PUBLIC,PARAMETER :: min2s=60.0_SRK
!> Conversion factor from hr to min
REAL(SRK),PUBLIC,PARAMETER :: hr2min=60.0_SRK
!> Conversion factor from day to hr
REAL(SRK),PUBLIC,PARAMETER :: day2hr=24.0_SRK
!> Conversion factor from year to day
REAL(SRK),PUBLIC,PARAMETER :: yr2day=365.25_SRK
!> Conversion factor from hr to sec
REAL(SRK),PUBLIC,PARAMETER :: hr2s=hr2min*min2s
!> Conversion factor from day to sec
REAL(SRK),PUBLIC,PARAMETER :: day2s=day2hr*hr2s
!> Conversion factor from year to sec
REAL(SRK),PUBLIC,PARAMETER :: yr2s=yr2day*day2s
!> Conversion factor from day to min
REAL(SRK),PUBLIC,PARAMETER :: day2min=day2hr*hr2min
!> Conversion factor from year to min
REAL(SRK),PUBLIC,PARAMETER :: yr2min=yr2day*day2min
!> Conversion factor from year to hr
REAL(SRK),PUBLIC,PARAMETER :: yr2hr=yr2day*day2hr
!> Conversion factor from s to min
REAL(SRK),PUBLIC,PARAMETER :: s2min=ONE/min2s
!> Conversion factor from min to hr
REAL(SRK),PUBLIC,PARAMETER :: min2hr=ONE/hr2min
!> Conversion factor from hr to day
REAL(SRK),PUBLIC,PARAMETER :: hr2day=ONE/day2hr
!> Conversion factor from day to year
REAL(SRK),PUBLIC,PARAMETER :: day2yr=ONE/yr2day
!> Conversion factor from s to hr
REAL(SRK),PUBLIC,PARAMETER :: s2hr=ONE/hr2s
!> Conversion factor from s to day
REAL(SRK),PUBLIC,PARAMETER :: s2day=ONE/day2s
!> Conversion factor from s to year
REAL(SRK),PUBLIC,PARAMETER :: s2yr=ONE/yr2s
!> Conversion factor from min to day
REAL(SRK),PUBLIC,PARAMETER :: min2day=ONE/day2min
!> Conversion factor from min to year
REAL(SRK),PUBLIC,PARAMETER :: min2yr=ONE/yr2min
!> Conversion factor from hr to year
REAL(SRK),PUBLIC,PARAMETER :: hr2yr=ONE/yr2hr

!Length conversions
!> Conversion factor from m to cm
REAL(SRK),PUBLIC,PARAMETER :: m2cm=base2centi
!> Conversion factor from cm to m
REAL(SRK),PUBLIC,PARAMETER :: cm2m=centi2base
!> Conversion factor from ft to cm
REAL(SRK),PUBLIC,PARAMETER :: ft2cm=30.48_SRK
!> Conversion factor from cm to ft
REAL(SRK),PUBLIC,PARAMETER :: cm2ft=ONE/ft2cm
!> Conversion factor from in to cm
REAL(SRK),PUBLIC,PARAMETER :: in2cm=2.54_SRK
!> Conversion factor from cm to in
REAL(SRK),PUBLIC,PARAMETER :: cm2in=ONE/in2cm

!Temperature conversions
!> Additive Conversion factor from Celsius to Kelvin
REAL(SRK),PUBLIC,PARAMETER :: C2K=273.15_SRK
!> Multiplicative Conversion factor from Rankine to Kelvin
REAL(SRK),PUBLIC,PARAMETER :: R2K=5.0_SRK/9.0_SRK
!> Additive Conversion factor from Fahrenheit to Rankine
REAL(SRK),PUBLIC,PARAMETER :: F2R=459.67_SRK

!> May be used for comparing real numbers relating to geometry
!> Tolerance is 0.0001 cm
REAL(SRK),PUBLIC,PARAMETER :: TOL_GEOM_cm=0.0001_SRK
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief converts temperature from Kelvin to Celsius
!> @param in_temp the temperature input as Kelvin
!> @return out_temp the temperature converted to Celsius
!>
ELEMENTAL FUNCTION K_to_C(in_temp) RESULT(out_temp)
  REAL(SRK),INTENT(IN) :: in_temp
  REAL(SRK) :: out_temp

  out_temp=in_temp-273.15_SRK

ENDFUNCTION K_to_C
!
!-------------------------------------------------------------------------------
!> @brief converts temperature from Celsius to Kelvin
!> @param in_temp the temperature input as Celsius
!> @return out_temp the temperature converted to Kelvin
!>
ELEMENTAL FUNCTION C_to_K(in_temp) RESULT(out_temp)
  REAL(SRK),INTENT(IN) :: in_temp
  REAL(SRK) :: out_temp

  out_temp=in_temp+273.15_SRK

ENDFUNCTION C_to_K
!
!-------------------------------------------------------------------------------
!> @brief converts temperature from Fahrenheit to Celsius
!> @param in_temp the temperature input as Fahrenheit
!> @return out_temp the temperature converted to Celsius
!>
ELEMENTAL FUNCTION F_to_C(in_temp) RESULT(out_temp)
  REAL(SRK),INTENT(IN) :: in_temp
  REAL(SRK) :: out_temp

  out_temp=(in_temp-32.0_SRK)*5.0_SRK/9.0_SRK

ENDFUNCTION F_to_C
!
!-------------------------------------------------------------------------------
!> @brief converts temperature from Celsius to Fahrenheit
!> @param in_temp the temperature input as Celsius
!> @return out_temp the temperature converted to Fahrenheit
!>
ELEMENTAL FUNCTION C_to_F(in_temp) RESULT(out_temp)
  REAL(SRK),INTENT(IN) :: in_temp
  REAL(SRK) :: out_temp

  out_temp=in_temp*9.0_SRK/5.0_SRK+32.0_SRK

ENDFUNCTION C_to_F
!
!-------------------------------------------------------------------------------
!> @brief converts temperature from Fahrenheit to Kelvin
!> @param in_temp the temperature input as Fahrenheit
!> @return out_temp the temperature converted to Kelvin
!>
ELEMENTAL FUNCTION F_to_K(in_temp) RESULT(out_temp)
  REAL(SRK),INTENT(IN) :: in_temp
  REAL(SRK) :: out_temp

  out_temp=C_to_K(F_to_C(in_temp))

ENDFUNCTION F_to_K
!
!-------------------------------------------------------------------------------
!> @brief converts temperature from Kelvin to Fahrenheit
!> @param in_temp the temperature input as Kelvin
!> @return out_temp the temperature converted to Fahrenheit
!>
ELEMENTAL FUNCTION K_to_F(in_temp) RESULT(out_temp)
  REAL(SRK),INTENT(IN) :: in_temp
  REAL(SRK) :: out_temp

  out_temp=C_to_F(K_to_C(in_temp))

ENDFUNCTION K_to_F
!
!-------------------------------------------------------------------------------
!> @brief converts temperature to Kelvin from Celsius, Rankine, or Fahrenheit,
!> if any other unit is presented in_temp is simply returned
!> @param in_temp the temperature input as the specified units
!> @param units character indicating the units of in_temp
!> @return out_temp the temperature converted to Kelvin
!>
ELEMENTAL FUNCTION tempTo_K(in_temp,units) RESULT(out_temp)
  REAL(SRK),INTENT(IN) :: in_temp
  CHARACTER(LEN=1),INTENT(IN) :: units
  REAL(SRK) :: out_temp

  SELECTCASE(units)
  CASE('C')
    out_temp = C_to_K(in_temp)
  CASE('R')
    out_temp = in_temp * R2K
  CASE('F')
    out_temp = F_to_K(in_temp)
  CASE DEFAULT
    out_temp = in_temp
  ENDSELECT

ENDFUNCTION tempTo_K
!
!-------------------------------------------------------------------------------
!> @brief converts temperature to Celsius from Kelvin, Rankine, or Fahrenheit,
!> if any other unit is presented in_temp is simply returned
!> @param in_temp the temperature input as the specified units
!> @param units character indicating the units of in_temp
!> @return out_temp the temperature converted to Celsius
!>
ELEMENTAL FUNCTION tempTo_C(in_temp,units) RESULT(out_temp)
  REAL(SRK),INTENT(IN) :: in_temp
  CHARACTER(LEN=1),INTENT(IN) :: units
  REAL(SRK) :: out_temp

  SELECTCASE(units)
  CASE('K')
    out_temp = K_to_C(in_temp)
  CASE('R')
    out_temp = in_temp * R2K - C2K
  CASE('F')
    out_temp = F_to_C(in_temp)
  CASE DEFAULT
    out_temp = in_temp
  ENDSELECT

ENDFUNCTION tempTo_C
!
!-------------------------------------------------------------------------------
!> @brief converts temperature to Fahrenheit from Celsius, Rankine, or Kelvin,
!> if any other unit is presented in_temp is simply returned
!> @param in_temp the temperature input as the specified units
!> @param units character indicating the units of in_temp
!> @return out_temp the temperature converted to Fahrenheit
!>
ELEMENTAL FUNCTION tempTo_F(in_temp,units) RESULT(out_temp)
  REAL(SRK),INTENT(IN) :: in_temp
  CHARACTER(LEN=1),INTENT(IN) :: units
  REAL(SRK) :: out_temp

  SELECTCASE(units)
  CASE('K')
    out_temp=K_to_F(in_temp)
  CASE('R')
    out_temp = in_temp - F2R
  CASE('C')
    out_temp = C_to_F(in_temp)
  CASE DEFAULT
    out_temp = in_temp
  ENDSELECT

ENDFUNCTION tempTo_F
!
!-------------------------------------------------------------------------------
!> @brief converts temperature to Rankine from Celsius, Kelvin, or Fahrenheit,
!> if any other unit is presented in_temp is simply returned
!> @param in_temp the temperature input as the specified units
!> @param units character indicating the units of in_temp
!> @return out_temp the temperature converted to Fahrenheit
!>
ELEMENTAL FUNCTION tempTo_R(in_temp,units) RESULT(out_temp)
  REAL(SRK),INTENT(IN) :: in_temp
  CHARACTER(LEN=1),INTENT(IN) :: units
  REAL(SRK) :: out_temp

  SELECTCASE(units)
  CASE('K')
    out_temp = in_temp / R2K
  CASE('F')
    out_temp = in_temp + F2R
  CASE('C')
    out_temp = C_to_K(in_temp) / R2K
  CASE DEFAULT
    out_temp = in_temp
  ENDSELECT

ENDFUNCTION tempTo_R
!
!-------------------------------------------------------------------------------
!> @brief converts temperature to a given unit from the provided unit. Celsius(C),
!> Fahrenheit(F), Kelvin(K), and Rankine(R) are the only valid units
!> @param in_temp the temperature input as the specified units
!> @param in_units character indicating the units of in_temp
!> @param out_units character indicating the units of out_temp
!> @return out_temp the temperature converted to Fahrenheit
!>
ELEMENTAL FUNCTION convertTemp(in_temp,in_units,out_units) RESULT(out_temp)
  REAL(SRK),INTENT(IN) :: in_temp
  CHARACTER(LEN=1),INTENT(IN) :: in_units
  CHARACTER(LEN=1),INTENT(IN) :: out_units
  REAL(SRK) :: out_temp

  SELECTCASE(out_units)
  CASE('K')
    out_temp = tempTo_K(in_temp,in_units)
  CASE('C')
    out_temp = tempTo_C(in_temp,in_units)
  CASE('F')
    out_temp = tempTo_F(in_temp,in_units)
  CASE('R')
    out_temp = tempTo_R(in_temp,in_units)
  ENDSELECT

ENDFUNCTION convertTemp
!
ENDMODULE Constants_Conversion
