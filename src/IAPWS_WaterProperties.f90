!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> IAPWS Industrial Formulation 1997
!> for the Thermodynamic Properties of Water and Steam
!> (IAPWS-IF97)
!>
!> References:
!>  -# "Revised Release on the IAPWS Industrial Formulation 1997 for the
!>  Thermodynamic Properties of Water and Steam"
!>  The International Association for the Properties of Water and Steam
!>  Lucerne, Switzerland
!>  2007
!>  http://www.iapws.ORg/relguide/IF97-Rev.pdf
!>  -# "IAPWS Release on Surface Tension of Ordinary Water Substance"
!>  The International Association for the Properties of Water and Steam
!>  1994
!>  http://www.iapws.ORg/relguide/surf.pdf
!>  -# "Release on the IAPWS Formulation 2008 for the Viscosity of Ordinary
!>  Water Substance."
!>  The International Association for the Properties of Water and Steam
!>  Berlin, Germany
!>  2008
!>  http://www.iapws.ORg/relguide/visc.pdf
!>  -# "Release on the IAPWS Formulation 2011 for the Thermal Conductivity
!>  of Ordinary Water Substance."
!>  The International Association for the Properties of Water and Steam
!>  Plzen,,Czech Republic
!>  2011
!>  http://www.iapws.ORg/relguide/ThCond.pdf
!>
!-------------------------------------------------------------------------------
!> These functions follow reference 1 by first calculating the dimmensionless
!> free energy and/or its derivatives, and using those values to calculate
!> material properties and derivatives. This can be done relatively easily since
!> the correlations are almost entirely in the form of power series. After the
!> dimm. free energy is found the rest of the material properties can be
!> evaulated based off of these values. These functions were optimized in
!> regions 1 and 2 by expanding the looped calculations and hard coding the
!> exponents.
!>
!> A small speed up was implemented in region 3. Instead of using the
!> power_derivatives function, each derivative of the gibbs free energy instead
!> has its own function.
!-------------------------------------------------------------------------------
!
MODULE IAPWSWaterPropertiesModule
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE ExceptionHandler

IMPLICIT NONE

PRIVATE

! Parameters
PUBLIC :: Tcrit, Dcrit, Pcrit
! accessor functions
PUBLIC :: regsopt, regsph
! property functions
PUBLIC :: vpt, hpt, cppt, cvpt, viscvt, thconvt, surftt, tsatpn, psattn, tph, pvt3n
PUBLIC :: dvdp, dvdt, dhdp, dhdt, dvdh
PUBLIC :: setRepresentation, clean_IAPWS
PUBLIC :: eWaterProp

!> Exception Handler for Water Properties
TYPE(ExceptionHandlerType),SAVE :: eWaterProp

!> expr_opt = 1 means do the original analytical solution
!> expr_opt = 2 means represent with simplified representation to reduce
!> computational burden
INTEGER(SIK) :: expr_opt = 1

INTEGER(SIK), PARAMETER :: MAX_CHAR_LEN=240

! R Consant and Critical Point of Water
REAL(SRK), PARAMETER :: tolerance = 1.0E-10_SRK, &
                      R = 0.461526E0_SRK, & ! Gas Constant [kJ/kg-K]
                      Dcrit = 322.0E0_SRK, & ! Critical Density [kg/m**3]
                      Pcrit = 22.064E0_SRK, & ! Critical Pressure [MPa]
                      XMOL = 0.1801525701E-01_SRK, & ! Molar mass of water mol/kg
                      Tcrit = 647.096E0_SRK ! Critical Temperature [K]

!===============================================================================
! Coefficient Tables in Reference 1
! All values here are dimensionless
!===============================================================================
! Region 1:
!-------------------------------------------------------------------------------
REAL(SRK), PARAMETER, DIMENSION(34) :: & ! Table 2
  N1 = [0.14632971213167E0_SRK, &
        -0.84548187169114E0_SRK, &
        -0.37563603672040E01_SRK, &
        0.33855169168385E01_SRK, &
        -0.95791963387872E0_SRK, &
        0.15772038513228E0_SRK, &
        -0.16616417199501E-01_SRK, &
        0.81214629983568E-03_SRK, &
        0.28319080123804E-03_SRK, &
        -0.60706301565874E-03_SRK, &
        -0.18990068218419E-01_SRK, &
        -0.32529748770505E-01_SRK, &
        -0.21841717175414E-01_SRK, &
        -0.52838357969930E-04_SRK, &
        -0.47184321073267E-03_SRK, &
        -0.30001780793026E-03_SRK, &
        0.47661393906987E-04_SRK, &
        -0.44141845330846E-05_SRK, &
        -0.72694996297594E-15_SRK, &
        -0.31679644845054E-04_SRK, &
        -0.28270797985312E-05_SRK, &
        -0.85205128120103E-09_SRK, &
        -0.22425281908000E-05_SRK, &
        -0.65171222895601E-06_SRK, &
        -0.14341729937924E-12_SRK, &
        -0.40516996860117E-06_SRK, &
        -0.12734301741641E-08_SRK, &
        -0.17424871230634E-09_SRK, &
        -0.68762131295531E-18_SRK, &
        0.14478307828521E-19_SRK, &
        0.26335781662795E-22_SRK, &
        -0.11947622640071E-22_SRK, &
        0.18228094581404E-23_SRK, &
        -0.93537087292458E-25_SRK]
INTEGER(SIK), PARAMETER, DIMENSION(SIZE(N1)) :: &
  I1 = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, & !
        1, 1, 1, 1, 2, 2, 2, 2, 2, 3, &
        3, 3, 4, 4, 4, 5, 8, 8, 21, 23, &
        29, 30, 31, 32], &
  J1 = [-2, -1, 0, 1, 2, 3, 4, 5, -9, -7, &
        -1, 0, 1, 3, -3, 0, 1, 3, 17, -4, &
        0, 6, -5, -2, 10, -8, -11, -6, -29, -31, &
        -38, -39, -40, -41]

!===============================================================================
! Region 2: Tables 10 and 11
!-------------------------------------------------------------------------------
REAL(SRK), PARAMETER, DIMENSION(9) :: & ! Table 10
  No2 = [-0.96927686500217E01_SRK, &
         0.10086655968018E02_SRK, &
         -0.56087911283020E-2_SRK, &
         0.71452738081455E-1_SRK, &
         -0.40710498223928E00_SRK, &
         0.14240819171444E01_SRK, &
         -0.43839511319450E01_SRK, &
         -0.28408632460772E00_SRK, &
         0.21268463753307E-1_SRK]
INTEGER(SIK), PARAMETER, DIMENSION(SIZE(No2)) :: &
  Jo2 = [0, 1, -5, -4, -3, -2, -1, 2, 3]

REAL(SRK), PARAMETER, DIMENSION(43) :: & ! Table 11
  N2 = [-0.17731742473213E-02_SRK, & ! 1
        -0.17834862292358E-01_SRK, & ! 2
        -0.45996013696365E-01_SRK, & ! 3
        -0.57581259083432E-01_SRK, & ! 4
        -0.50325278727930E-01_SRK, & ! 5
        -0.33032641670203E-04_SRK, & ! 6
        -0.18948987516315E-03_SRK, & ! 7
        -0.39392777243355E-02_SRK, & ! 8
        -0.43797295650573E-01_SRK, & ! 9
        -0.26674547914087E-04_SRK, & ! 10
        0.20481737692309E-07_SRK, &
        0.43870667284435E-06_SRK, &
        -0.32277677238570E-04_SRK, &
        -0.15033924542148E-02_SRK, &
        -0.40668253562649E-01_SRK, & ! 15
        -0.78847309559367E-09_SRK, &
        0.12790717852285E-07_SRK, &
        0.48225372718507E-06_SRK, &
        0.22922076337661E-05_SRK, &
        -0.16714766451061E-10_SRK, & ! 20
        -0.21171472321355E-02_SRK, &
        -0.23895741934104E02_SRK, &
        -0.59059564324270E-17_SRK, &
        -0.12621808899101E-05_SRK, &
        -0.38946842435739E-01_SRK, & ! 25
        0.11256211360459E-10_SRK, &
        -0.82311340897998E01_SRK, &
        0.19809712802088E-07_SRK, &
        0.10406965210174E-18_SRK, &
        -0.10234747095929E-12_SRK, & ! 30
        -0.10018179379511E-08_SRK, &
        -0.80882908646985E-10_SRK, &
        0.10693031879409E0_SRK, &
        -0.33662250574171E0_SRK, &
        0.89185845355421E-24_SRK, & ! 35
        0.30629316876232E-12_SRK, &
        -0.42002467698208E-05_SRK, &
        -0.59056029685639E-25_SRK, &
        0.37826947613457E-05_SRK, &
        -0.12768608934681E-14_SRK, & ! 40
        0.73087610595061E-28_SRK, &
        0.55414715350778E-16_SRK, &
        -0.94369707241210E-06_SRK]
INTEGER(SIK), PARAMETER, DIMENSION(SIZE(N2)) :: &
  I2 = [1, 1, 1, 1, 1, 2, 2, 2, 2, 2, &
        3, 3, 3, 3, 3, 4, 4, 4, 5, 6, &
        6, 6, 7, 7, 7, 8, 8, 9, 10, 10, &
        10, 16, 16, 18, 20, 20, 20, 21, 22, 23, &
        24, 24, 24], &
  J2 = [0, 1, 2, 3, 6, 1, 2, 4, 7, 36, &
        0, 1, 3, 6, 35, 1, 2, 3, 7, 3, &
        16, 35, 0, 11, 25, 8, 36, 13, 4, 10, &
        14, 29, 50, 57, 20, 35, 48, 21, 53, 39, &
        26, 40, 58]
!===============================================================================
! Region 3: Table 30
!-------------------------------------------------------------------------------
REAL(SRK), PARAMETER, DIMENSION(40) :: &
  N3 = [0.10658070028513E01_SRK, &
        -0.15732845290239E02_SRK, &
        0.20944396974307E02_SRK, &
        -0.76867707878716E01_SRK, &
        0.26185947787954E01_SRK, &
        -0.28080781148620E01_SRK, &
        0.12053369696517E01_SRK, &
        -0.84566812812502E-02_SRK, &
        -0.12654315477714E01_SRK, &
        -0.11524407806681E01_SRK, &
        0.88521043984318E00_SRK, &
        -0.64207765181607E00_SRK, &
        0.38493460186671E00_SRK, &
        -0.85214708824206E00_SRK, &
        0.48972281541877E01_SRK, &
        -0.30502617256965E01_SRK, &
        0.39420536879154E-01_SRK, &
        0.12558408424308E00_SRK, &
        -0.27999329698710E00_SRK, &
        0.13899799569460E01_SRK, &
        -0.20189915023570E01_SRK, &
        -0.82147637173963E-02_SRK, &
        -0.47596035734923E00_SRK, &
        0.43984074473500E-01_SRK, &
        -0.44476435428739E00_SRK, &
        0.90572070719733E00_SRK, &
        0.70522450087967E00_SRK, &
        0.10770512626332E00_SRK, &
        -0.32913623258954E00_SRK, &
        -0.50871062041158E00_SRK, &
        -0.22175400873096E-01_SRK, &
        0.94260751665092E-01_SRK, &
        0.16436278447961E00_SRK, &
        -0.13503372241348E-01_SRK, &
        -0.14834345352472E-01_SRK, &
        0.57922953628084E-03_SRK, &
        0.32308904703711E-02_SRK, &
        0.80964802996215E-04_SRK, &
        -0.16557679795037E-03_SRK, &
        -0.44923899061815E-04_SRK]
INTEGER(SIK), PARAMETER, DIMENSION(SIZE(N3)) :: &
  I3 = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, &
        1, 1, 2, 2, 2, 2, 2, 2, 3, 3, &
        3, 3, 3, 4, 4, 4, 4, 5, 5, 5, &
        6, 6, 6, 7, 8, 9, 9, 10, 10, 11], &
  J3 = [0, 0, 1, 2, 7, 10, 12, 23, 2, 6, &
        15, 17, 0, 2, 6, 7, 22, 26, 0, 2, &
        4, 16, 26, 0, 2, 4, 26, 1, 3, 26, &
        0, 2, 26, 2, 26, 2, 26, 0, 1, 26]

!===============================================================================
! Region 5: Tables 37 and 38
!-------------------------------------------------------------------------------
REAL(SRK), PARAMETER, DIMENSION(6) :: &
  N5 = [0.15736404855259E-02_SRK, &
        0.90153761673944E-03_SRK, &
        -0.50270077677648E-02_SRK, &
        0.22440037409485E-05_SRK, &
        -0.41163275453471E-05_SRK, &
        0.37919454822955E-07_SRK]

INTEGER(SIK), PARAMETER, DIMENSION(SIZE(N5)) :: &
  I5 = [1, 1, 1, 2, 2, 3], &
  J5 = [1, 2, 3, 3, 9, 7]

REAL(SRK), PARAMETER, DIMENSION(6) :: &
  No5 = [-0.13179983674201E02_SRK, &
         0.68540841634434E01_SRK, &
         -0.24805148933466E-1_SRK, &
         0.36901534980333E00_SRK, &
         -0.31161318213925E01_SRK, &
         -0.32961626538917E00_SRK]
INTEGER(SIK), PARAMETER, DIMENSION(SIZE(No5)) :: &
  Jo5 = [0, 1, -3, -2, -1, 2]

CONTAINS
!===============================================================================
! FUNCTIONS
!===============================================================================
!
!-------------------------------------------------------------------------------
!> Call to override the default IAPWS representation with a simplified one
!> to improve performance
!>
!> @param expr_opt 1 - original, 2 - simplified
!>
SUBROUTINE setRepresentation(opt)
  INTEGER(SIK), INTENT(IN) :: opt
  REQUIRE(opt == 1 .OR. opt == 2)
  expr_opt = opt
ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> TODO add documentation
SUBROUTINE Clean_IAPWS()
  expr_opt = 1
ENDSUBROUTINE
!
!===============================================================================
! Section: Supporting Custom Functions
!===============================================================================
!
!-------------------------------------------------------------------------------
!> This function calculates the coefficient of a power series from taking a derivative of any order
!> f=x^(n) where n is the exponent of the function
!> derivative_order(f)= C*x^(n-order) where C is the value of this function
!> C= n*(n-1)*(n-2)*...(n-order+1)
!> @param expnt the exponent of the function
!> @param order the order of the derivative
!> @return coefficient of the derivative
!>
FUNCTION power_derivative(expnt, order)
  INTEGER(SIK), INTENT(IN) :: expnt
  INTEGER(SIK), INTENT(IN) :: order

  INTEGER(SIK) :: i
  INTEGER(SIK) :: power_derivative

  power_derivative = 1 ! Coefficient starts at 1

  ! Loop through each derivative
  DO i = 1, order
    power_derivative = power_derivative*(expnt - i + 1)
  ENDDO
ENDFUNCTION power_derivative
!
!-------------------------------------------------------------------------------
!> Calculates dimensionless density
!> \f$\delta=\frac{D}{D^{*}}\f$
!> D* is a reference parameter from Reference 1 (Density at the Critical Point)
!> @param rho density in \f$\frac{kg}{m^3}\f$
!> @return dimensionless density \f$\delta\f$
!>
FUNCTION normalize_density(rho)
  REAL(SRK), INTENT(IN) :: rho

  REAL(SRK) :: normalize_density

  normalize_density = rho/Dcrit
ENDFUNCTION
!
!-------------------------------------------------------------------------------
!> Calculates dimensionless pressure
!> \f$\pi=\frac{P}{P^{*}}\f$
!> P* is a reference parameter from Reference 1
!> @param P Pressure in MPa
!> @param ireg region number [1,2,3,5]
!> @return dimensionless pressure \f$\pi\f$
!>
FUNCTION normalize_pressure(P, ireg)
  REAL(SRK), INTENT(IN) :: P
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: normalize_pressure

  ! Normalization Reference Values for each region
  REAL(SRK), PARAMETER :: P1n = 16.53E0_SRK, P2n = 1.0E0_SRK, P5n = 1.0E0_SRK ! MPa
  normalize_pressure = 0.0_SRK

  ! Normalize based on fluid region
  IF (ireg == 1) THEN
    normalize_pressure = P/P1n
  ELSEIF (ireg == 2) THEN
    normalize_pressure = P/P2n
  ELSEIF (ireg == 3) THEN
    normalize_pressure = P/Pcrit ! Should never get hit?
  ELSEIF (ireg == 5) THEN
    normalize_pressure = P/P5n
  ELSE
    REQUIRE(.FALSE.)
  ENDIF
ENDFUNCTION
!
!-------------------------------------------------------------------------------
!> Calculates dimensionless temperature
!> \f$\tau=\frac{T}{T^{*}}\f$
!> T* is a reference parameter from Reference 1
!> @param T Temperature in K
!> @param ireg region number [1,2,3,5]
!> @return dimensionless temperature \f$\tau\f$
!>
FUNCTION normalize_temperature(T, ireg)
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: normalize_temperature

  ! Normalization Reference Values (T*) for each region
  REAL(SRK), PARAMETER :: T1n = 1386.0E0_SRK, T2n = 540.0E0_SRK, T5n = 1000.0E0_SRK   ! K

  normalize_temperature = 0.0_SRK
  IF (ireg == 1) THEN
    normalize_temperature = T1n/T
  ELSEIF (ireg == 2) THEN
    normalize_temperature = T2n/T
  ELSEIF (ireg == 3) THEN
    normalize_temperature = Tcrit/T
  ELSEIF (ireg == 5) THEN
    normalize_temperature = T5n/T
  ELSE
    REQUIRE(.FALSE.)
  ENDIF
ENDFUNCTION

!===============================================================================
! Gibbs Free Energy and Hemholtz Free energy and their derivatives
!===============================================================================
!
!-------------------------------------------------------------------------------
!> Calculation of Dimensionless Gibbs free energy first derivative with respect pi \f$\gamma_{\pi}\f$  for region 1
!> Reference 1
!> Table : 4
!> @param pi Dimensionless Pressure \f$\pi\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\gamma_{\pi}\f$
!>
FUNCTION gamma1_p1_t0(pi, tau)
  REAL(SRK), INTENT(IN) :: pi
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: gamma1_p1_t0

  REAL(SRK), PARAMETER ::C1 = 7.1E0_SRK, C2 = 1.222E0_SRK
  REAL(SRK) ::  pin1
  REAL(SRK) :: taun1

  !> Table 4.
  !> \f[
  !> \gamma_{\pi}=-\sum\limits_{i=1}^{34} n_{i}I_{i}(7.1-\pi)^{I_{i}-1}(\tau-1.222)^{J_{i}}
  !> \f]

  pin1 = C1 - pi
  taun1 = tau - C2

  ! Calculate Residual component of the derivative of gibbs free energy with respect to pi and tau
  gamma1_p1_t0 = 0.0_SRK &                                        !  i |  I | J
                 - N1(1)*I1(1)*(pin1)**(0 - 1)*taun1**(-2) & !  1 |  0 |-2
                 - N1(2)*I1(2)*(pin1)**(0 - 1)*taun1**(-1) & !  2 |  0 |-1
                 - N1(3)*I1(3)*(pin1)**(0 - 1)*taun1**(0) & !  3 |  0 | 0
                 - N1(4)*I1(4)*(pin1)**(0 - 1)*taun1**(1) & !  4 |  0 | 1
                 - N1(5)*I1(5)*(pin1)**(0 - 1)*taun1**(2) & !  5 |  0 | 2
                 - N1(6)*I1(6)*(pin1)**(0 - 1)*taun1**(3) & !  6 |  0 | 3
                 - N1(7)*I1(7)*(pin1)**(0 - 1)*taun1**(4) & !  7 |  0 | 4
                 - N1(8)*I1(8)*(pin1)**(0 - 1)*taun1**(5) & !  8 |  0 | 5
                 - N1(9)*I1(9)*(pin1)**(1 - 1)*taun1**(-9) & !  9 |  1 |-9
                 - N1(10)*I1(10)*(pin1)**(1 - 1)*taun1**(-7) & ! 10 |  1 |-7
                 - N1(11)*I1(11)*(pin1)**(1 - 1)*taun1**(-1) & ! 11 |  1 |-1
                 - N1(12)*I1(12)*(pin1)**(1 - 1)*taun1**(0) & ! 12 |  1 | 0
                 - N1(13)*I1(13)*(pin1)**(1 - 1)*taun1**(1) & ! 13 |  1 | 1
                 - N1(14)*I1(14)*(pin1)**(1 - 1)*taun1**(3) & ! 14 |  1 | 3
                 - N1(15)*I1(15)*(pin1)**(2 - 1)*taun1**(-3) & ! 15 |  2 |-3
                 - N1(16)*I1(16)*(pin1)**(2 - 1)*taun1**(0) & ! 16 |  2 | 0
                 - N1(17)*I1(17)*(pin1)**(2 - 1)*taun1**(1) & ! 17 |  2 | 1
                 - N1(18)*I1(18)*(pin1)**(2 - 1)*taun1**(3) & ! 18 |  2 | 3
                 - N1(19)*I1(19)*(pin1)**(2 - 1)*taun1**(17) & ! 19 |  2 | 17
                 - N1(20)*I1(20)*(pin1)**(3 - 1)*taun1**(-4) & ! 20 |  3 |-4
                 - N1(21)*I1(21)*(pin1)**(3 - 1)*taun1**(0) & ! 21 |  3 | 0
                 - N1(22)*I1(22)*(pin1)**(3 - 1)*taun1**(6) & ! 22 |  3 | 6
                 - N1(23)*I1(23)*(pin1)**(4 - 1)*taun1**(-5) & ! 23 |  4 |-5
                 - N1(24)*I1(24)*(pin1)**(4 - 1)*taun1**(-2) & ! 24 |  4 |-2
                 - N1(25)*I1(25)*(pin1)**(4 - 1)*taun1**(10) & ! 25 |  4 | 10
                 - N1(26)*I1(26)*(pin1)**(5 - 1)*taun1**(-8) & ! 26 |  5 |-8
                 - N1(27)*I1(27)*(pin1)**(8 - 1)*taun1**(-11) & ! 27 |  8 |-11
                 - N1(28)*I1(28)*(pin1)**(8 - 1)*taun1**(-6) & ! 28 |  8 |-6
                 - N1(29)*I1(29)*(pin1)**(21 - 1)*taun1**(-29) & ! 29 | 21 |-29
                 - N1(30)*I1(30)*(pin1)**(23 - 1)*taun1**(-31) & ! 30 | 23 |-31
                 - N1(31)*I1(31)*(pin1)**(29 - 1)*taun1**(-38) & ! 31 | 29 |-38
                 - N1(32)*I1(32)*(pin1)**(30 - 1)*taun1**(-39) & ! 32 | 30 |-39
                 - N1(33)*I1(33)*(pin1)**(31 - 1)*taun1**(-40) & ! 33 | 31 |-40
                 - N1(34)*I1(34)*(pin1)**(32 - 1)*taun1**(-41)    ! 34 | 32 |-41

  ! Non-Optimized Calculation
  !  DO i=1,SIZE(N1)
  !    gamma1 = gamma1 - N1(i)*I1(i)*(C1-pi)**(I1(i)-order_pi)*(tau-C2)**(J1(i)-order_tau)
  !  ENDDO
ENDFUNCTION gamma1_p1_t0
!
!-------------------------------------------------------------------------------
!> Calculation of Dimensionless Gibbs free energy first derivative with respect tau and pi \f$\gamma_{\pi\tau}\f$  for region 1
!> Reference 1
!> Table : 4
!> @param pi Dimensionless Pressure \f$\pi\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\gamma_{\pi\tau}\f$
!>
FUNCTION gamma1_p1_t1(pi, tau)
  REAL(SRK), INTENT(IN) :: pi
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: gamma1_p1_t1

  REAL(SRK), PARAMETER ::C1 = 7.1E0_SRK, C2 = 1.222E0_SRK
  REAL(SRK) :: pin1
  REAL(SRK) :: taun1

  !> Table 4.
  !> \f[
  !> \gamma_{\pi\tau}=-\sum\limits_{i=1}^{34} n_{i}I_{i}J_{i}(7.1-\pi)^{I_{i}-1}(\tau-1.222)^{J_{i}-1}
  !> \f]

  pin1 = C1 - pi
  taun1 = tau - C2
  ! Calculate Residual component of the derivative of gibbs free energy with respect to pi and tau
  gamma1_p1_t1 = 0.0_SRK &                                                !  i |  I | J
                 - N1(9)*I1(9)*J1(9)*(pin1)**(1 - 1)*taun1**(-9 - 1) & !  9 |  1 |-9
                 - N1(10)*I1(10)*J1(10)*(pin1)**(1 - 1)*taun1**(-7 - 1) & ! 10 |  1 |-7
                 - N1(11)*I1(11)*J1(11)*(pin1)**(1 - 1)*taun1**(-1 - 1) & ! 11 |  1 |-1
                 - N1(12)*I1(12)*J1(12)*(pin1)**(1 - 1)*taun1**(0 - 1) & ! 12 |  1 | 0
                 - N1(13)*I1(13)*J1(13)*(pin1)**(1 - 1)*taun1**(1 - 1) & ! 13 |  1 | 1
                 - N1(14)*I1(14)*J1(14)*(pin1)**(1 - 1)*taun1**(3 - 1) & ! 14 |  1 | 3
                 - N1(15)*I1(15)*J1(15)*(pin1)**(2 - 1)*taun1**(-3 - 1) & ! 15 |  2 |-3
                 - N1(16)*I1(16)*J1(16)*(pin1)**(2 - 1)*taun1**(0 - 1) & ! 16 |  2 | 0
                 - N1(17)*I1(17)*J1(17)*(pin1)**(2 - 1)*taun1**(1 - 1) & ! 17 |  2 | 1
                 - N1(18)*I1(18)*J1(18)*(pin1)**(2 - 1)*taun1**(3 - 1) & ! 18 |  2 | 3
                 - N1(19)*I1(19)*J1(19)*(pin1)**(2 - 1)*taun1**(17 - 1) & ! 19 |  2 | 17
                 - N1(20)*I1(20)*J1(20)*(pin1)**(3 - 1)*taun1**(-4 - 1) & ! 20 |  3 |-4
                 - N1(21)*I1(21)*J1(21)*(pin1)**(3 - 1)*taun1**(0 - 1) & ! 21 |  3 | 0
                 - N1(22)*I1(22)*J1(22)*(pin1)**(3 - 1)*taun1**(6 - 1) & ! 22 |  3 | 6
                 - N1(23)*I1(23)*J1(23)*(pin1)**(4 - 1)*taun1**(-5 - 1) & ! 23 |  4 |-5
                 - N1(24)*I1(24)*J1(24)*(pin1)**(4 - 1)*taun1**(-2 - 1) & ! 24 |  4 |-2
                 - N1(25)*I1(25)*J1(25)*(pin1)**(4 - 1)*taun1**(10 - 1) & ! 25 |  4 | 10
                 - N1(26)*I1(26)*J1(26)*(pin1)**(5 - 1)*taun1**(-8 - 1) & ! 26 |  5 |-8
                 - N1(27)*I1(27)*J1(27)*(pin1)**(8 - 1)*taun1**(-11 - 1) & ! 27 |  8 |-11
                 - N1(28)*I1(28)*J1(28)*(pin1)**(8 - 1)*taun1**(-6 - 1) & ! 28 |  8 |-6
                 - N1(29)*I1(29)*J1(29)*(pin1)**(21 - 1)*taun1**(-29 - 1) & ! 29 | 21 |-29
                 - N1(30)*I1(30)*J1(30)*(pin1)**(23 - 1)*taun1**(-31 - 1) & ! 30 | 23 |-31
                 - N1(31)*I1(31)*J1(31)*(pin1)**(29 - 1)*taun1**(-38 - 1) & ! 31 | 29 |-38
                 - N1(32)*I1(32)*J1(32)*(pin1)**(30 - 1)*taun1**(-39 - 1) & ! 32 | 30 |-39
                 - N1(33)*I1(33)*J1(33)*(pin1)**(31 - 1)*taun1**(-40 - 1) & ! 33 | 31 |-40
                 - N1(34)*I1(34)*J1(34)*(pin1)**(32 - 1)*taun1**(-41 - 1)    ! 34 | 32 |-41

! Non-Optimized Calculation
!    DO i=1,SIZE(N1)
!        gamma1 = gamma1 - N1(i)*I1(i)*J1(i)*(C1-pi)**(I1(i)-order_pi)*(tau-C2)**(J1(i)-order_tau)
!    ENDDO
ENDFUNCTION gamma1_p1_t1
!
!-------------------------------------------------------------------------------
!> Calculation of Dimensionless Gibbs free energy first derivative with respect tau \f$\gamma_{\tau}\f$  for region 1
!> Reference 1
!> Table : 4
!> @param pi Dimensionless Pressure \f$\pi\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\gamma_{\tau}\f$
!>
FUNCTION gamma1_p0_t1(pi, tau)
  REAL(SRK), INTENT(IN) :: pi
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: gamma1_p0_t1

  REAL(SRK), PARAMETER ::C1 = 7.1E0_SRK, C2 = 1.222E0_SRK
  REAL(SRK) :: pin1
  REAL(SRK) :: taun1

  !> Table 4.
  !> \f[
  !> \gamma_{\tau}=\sum\limits_{i=1}^{34} n_{i}J_{i}(7.1-\pi)^{I_{i}}(\tau-1.222)^{J_{i}-1}
  !> \f]

  pin1 = C1 - pi
  taun1 = tau - C2
  ! Calculate Residual component of the derivative of gibbs free energy with respect to pi and tau for region 1
  gamma1_p0_t1 = 0.0_SRK &                                       !  i |  I | J
                 + N1(1)*J1(1)*(pin1)**(0)*taun1**(-2 - 1) & !  1 |  0 |-2
                 + N1(2)*J1(2)*(pin1)**(0)*taun1**(-1 - 1) & !  2 |  0 |-1
                 + N1(3)*J1(3)*(pin1)**(0)*taun1**(0 - 1) & !  3 |  0 | 0
                 + N1(4)*J1(4)*(pin1)**(0)*taun1**(1 - 1) & !  4 |  0 | 1
                 + N1(5)*J1(5)*(pin1)**(0)*taun1**(2 - 1) & !  5 |  0 | 2
                 + N1(6)*J1(6)*(pin1)**(0)*taun1**(3 - 1) & !  6 |  0 | 3
                 + N1(7)*J1(7)*(pin1)**(0)*taun1**(4 - 1) & !  7 |  0 | 4
                 + N1(8)*J1(8)*(pin1)**(0)*taun1**(5 - 1) & !  8 |  0 | 5
                 + N1(9)*J1(9)*(pin1)**(1)*taun1**(-9 - 1) & !  9 |  1 |-9
                 + N1(10)*J1(10)*(pin1)**(1)*taun1**(-7 - 1) & ! 10 |  1 |-7
                 + N1(11)*J1(11)*(pin1)**(1)*taun1**(-1 - 1) & ! 11 |  1 |-1
                 + N1(12)*J1(12)*(pin1)**(1)*taun1**(0 - 1) & ! 12 |  1 | 0
                 + N1(13)*J1(13)*(pin1)**(1)*taun1**(1 - 1) & ! 13 |  1 | 1
                 + N1(14)*J1(14)*(pin1)**(1)*taun1**(3 - 1) & ! 14 |  1 | 3
                 + N1(15)*J1(15)*(pin1)**(2)*taun1**(-3 - 1) & ! 15 |  2 |-3
                 + N1(16)*J1(16)*(pin1)**(2)*taun1**(0 - 1) & ! 16 |  2 | 0
                 + N1(17)*J1(17)*(pin1)**(2)*taun1**(1 - 1) & ! 17 |  2 | 1
                 + N1(18)*J1(18)*(pin1)**(2)*taun1**(3 - 1) & ! 18 |  2 | 3
                 + N1(19)*J1(19)*(pin1)**(2)*taun1**(17 - 1) & ! 19 |  2 | 17
                 + N1(20)*J1(20)*(pin1)**(3)*taun1**(-4 - 1) & ! 20 |  3 |-4
                 + N1(21)*J1(21)*(pin1)**(3)*taun1**(0 - 1) & ! 21 |  3 | 0
                 + N1(22)*J1(22)*(pin1)**(3)*taun1**(6 - 1) & ! 22 |  3 | 6
                 + N1(23)*J1(23)*(pin1)**(4)*taun1**(-5 - 1) & ! 23 |  4 |-5
                 + N1(24)*J1(24)*(pin1)**(4)*taun1**(-2 - 1) & ! 24 |  4 |-2
                 + N1(25)*J1(25)*(pin1)**(4)*taun1**(10 - 1) & ! 25 |  4 | 10
                 + N1(26)*J1(26)*(pin1)**(5)*taun1**(-8 - 1) & ! 26 |  5 |-8
                 + N1(27)*J1(27)*(pin1)**(8)*taun1**(-11 - 1) & ! 27 |  8 |-11
                 + N1(28)*J1(28)*(pin1)**(8)*taun1**(-6 - 1) & ! 28 |  8 |-6
                 + N1(29)*J1(29)*(pin1)**(21)*taun1**(-29 - 1) & ! 29 | 21 |-29
                 + N1(30)*J1(30)*(pin1)**(23)*taun1**(-31 - 1) & ! 30 | 23 |-31
                 + N1(31)*J1(31)*(pin1)**(29)*taun1**(-38 - 1) & ! 31 | 29 |-38
                 + N1(32)*J1(32)*(pin1)**(30)*taun1**(-39 - 1) & ! 32 | 30 |-39
                 + N1(33)*J1(33)*(pin1)**(31)*taun1**(-40 - 1) & ! 33 | 31 |-40
                 + N1(34)*J1(34)*(pin1)**(32)*taun1**(-41 - 1)    ! 34 | 32 |-41

! Non-Optimized Calculation
!    DO i=1,SIZE(N1)
!        gamma1_p0_t1 = gamma1_p0_t1 + N1(i)*J1(i)*(C1-pi)**(I1(i)-order_pi)*(tau-C2)**(J1(i)-order_tau)
!    ENDDO
ENDFUNCTION gamma1_p0_t1
!
!-------------------------------------------------------------------------------
!> Calculation of Dimensionless Gibbs free energy second derivative with respect pi \f$\gamma_{\pi\pi}\f$ for region 1
!> Reference 1
!> Table : 4
!> @param pi Dimensionless Pressure \f$\pi\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\gamma_{\pi\pi}\f$
!>
FUNCTION gamma1_p2_t0(pi, tau)
  REAL(SRK), INTENT(IN) :: pi
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: gamma1_p2_t0

  REAL(SRK), PARAMETER ::C1 = 7.1E0_SRK, C2 = 1.222E0_SRK
  REAL(SRK) :: pin1
  REAL(SRK) :: taun1

  !> Table 4.
  !> \f[
  !> \gamma_{\pi\pi}=\sum\limits_{i=1}^{34} n_{i}(I_{i}-1)I_{i}(7.1-\pi)^{I_{i}-2}(\tau-1.222)^{J_{i}}
  !> \f]

  pin1 = C1 - pi
  taun1 = tau - C2
  ! Calculate Residual component of the derivative of gibbs free energy with respect to pi and tau
  gamma1_p2_t0 = 0.0_SRK &                                                  !  i |  I | J
                 + N1(9)*(I1(9) - 1)*I1(9)*(pin1)**(1 - 2)*taun1**(-9) & !  9 |  1 |-9
                 + N1(10)*(I1(10) - 1)*I1(10)*(pin1)**(1 - 2)*taun1**(-7) & ! 10 |  1 |-7
                 + N1(11)*(I1(11) - 1)*I1(11)*(pin1)**(1 - 2)*taun1**(-1) & ! 11 |  1 |-1
                 + N1(12)*(I1(12) - 1)*I1(12)*(pin1)**(1 - 2)*taun1**(0) & ! 12 |  1 | 0
                 + N1(13)*(I1(13) - 1)*I1(13)*(pin1)**(1 - 2)*taun1**(1) & ! 13 |  1 | 1
                 + N1(14)*(I1(14) - 1)*I1(14)*(pin1)**(1 - 2)*taun1**(3) & ! 14 |  1 | 3
                 + N1(15)*(I1(15) - 1)*I1(15)*(pin1)**(2 - 2)*taun1**(-3) & ! 15 |  2 |-3
                 + N1(16)*(I1(16) - 1)*I1(16)*(pin1)**(2 - 2)*taun1**(0) & ! 16 |  2 | 0
                 + N1(17)*(I1(17) - 1)*I1(17)*(pin1)**(2 - 2)*taun1**(1) & ! 17 |  2 | 1
                 + N1(18)*(I1(18) - 1)*I1(18)*(pin1)**(2 - 2)*taun1**(3) & ! 18 |  2 | 3
                 + N1(19)*(I1(19) - 1)*I1(19)*(pin1)**(2 - 2)*taun1**(17) & ! 19 |  2 | 17
                 + N1(20)*(I1(20) - 1)*I1(20)*(pin1)**(3 - 2)*taun1**(-4) & ! 20 |  3 |-4
                 + N1(21)*(I1(21) - 1)*I1(21)*(pin1)**(3 - 2)*taun1**(0) & ! 21 |  3 | 0
                 + N1(22)*(I1(22) - 1)*I1(22)*(pin1)**(3 - 2)*taun1**(6) & ! 22 |  3 | 6
                 + N1(23)*(I1(23) - 1)*I1(23)*(pin1)**(4 - 2)*taun1**(-5) & ! 23 |  4 |-5
                 + N1(24)*(I1(24) - 1)*I1(24)*(pin1)**(4 - 2)*taun1**(-2) & ! 24 |  4 |-2
                 + N1(25)*(I1(25) - 1)*I1(25)*(pin1)**(4 - 2)*taun1**(10) & ! 25 |  4 | 10
                 + N1(26)*(I1(26) - 1)*I1(26)*(pin1)**(5 - 2)*taun1**(-8) & ! 26 |  5 |-8
                 + N1(27)*(I1(27) - 1)*I1(27)*(pin1)**(8 - 2)*taun1**(-11) & ! 27 |  8 |-11
                 + N1(28)*(I1(28) - 1)*I1(28)*(pin1)**(8 - 2)*taun1**(-6) & ! 28 |  8 |-6
                 + N1(29)*(I1(29) - 1)*I1(29)*(pin1)**(21 - 2)*taun1**(-29) & ! 29 | 21 |-29
                 + N1(30)*(I1(30) - 1)*I1(30)*(pin1)**(23 - 2)*taun1**(-31) & ! 30 | 23 |-31
                 + N1(31)*(I1(31) - 1)*I1(31)*(pin1)**(29 - 2)*taun1**(-38) & ! 31 | 29 |-38
                 + N1(32)*(I1(32) - 1)*I1(32)*(pin1)**(30 - 2)*taun1**(-39) & ! 32 | 30 |-39
                 + N1(33)*(I1(33) - 1)*I1(33)*(pin1)**(31 - 2)*taun1**(-40) & ! 33 | 31 |-40
                 + N1(34)*(I1(34) - 1)*I1(34)*(pin1)**(32 - 2)*taun1**(-41)    ! 34 | 32 |-41

! Non-Optimized Calculation
!        gamma1_p2_t0=0.0_SRK
!        DO i=1,SIZE(N1)
!            gamma1_p2_t0 = gamma1_p2_t0 + N1(i)*I1(i)*(I1(i)-1)*(C1-pi)**(I1(i)-2)*(tau-C2)**(J1(i)-0)
!        ENDDO
ENDFUNCTION gamma1_p2_t0
!
!-------------------------------------------------------------------------------
!> Calculation of Dimensionless Gibbs free energy second derivative with respect tau \f$\gamma_{\tau\tau}\f$ for region 1
!> Reference 1
!> Table : 4
!> @param pi Dimensionless Pressure \f$\pi\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\gamma_{\tau\tau}\f$
!>
FUNCTION gamma1_p0_t2(pi, tau)
  REAL(SRK), INTENT(IN) :: pi
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: gamma1_p0_t2

  REAL(SRK), PARAMETER ::C1 = 7.1E0_SRK, C2 = 1.222E0_SRK
  REAL(SRK) :: pin1
  REAL(SRK) :: taun1

  !> Table 4.
  !> \f[
  !> \gamma_{\tau\tau}=\sum\limits_{i=1}^{34} n_{i}(J_{i}-1)J_{i}(7.1-\pi)^{I_{i}}(\tau-1.222)^{J_{i}-2}
  !> \f]

  pin1 = C1 - pi
  taun1 = tau - C2
  ! Calculate Residual component of the derivative of gibbs free energy with respect to pi and tau
  gamma1_p0_t2 = 0.0_SRK &                                                  !  i |  I | J
                 + N1(1)*(J1(1) - 1)*J1(1)*(pin1)**(0)*taun1**(-2 - 2) & !  1 |  0 |-2
                 + N1(2)*(J1(2) - 1)*J1(2)*(pin1)**(0)*taun1**(-1 - 2) & !  2 |  0 |-1
                 + N1(3)*(J1(3) - 1)*J1(3)*(pin1)**(0)*taun1**(0 - 2) & !  3 |  0 | 0
                 + N1(4)*(J1(4) - 1)*J1(4)*(pin1)**(0)*taun1**(1 - 2) & !  4 |  0 | 1
                 + N1(5)*(J1(5) - 1)*J1(5)*(pin1)**(0)*taun1**(2 - 2) & !  5 |  0 | 2
                 + N1(6)*(J1(6) - 1)*J1(6)*(pin1)**(0)*taun1**(3 - 2) & !  6 |  0 | 3
                 + N1(7)*(J1(7) - 1)*J1(7)*(pin1)**(0)*taun1**(4 - 2) & !  7 |  0 | 4
                 + N1(8)*(J1(8) - 1)*J1(8)*(pin1)**(0)*taun1**(5 - 2) & !  8 |  0 | 5
                 + N1(9)*(J1(9) - 1)*J1(9)*(pin1)**(1)*taun1**(-9 - 2) & !  9 |  1 |-9
                 + N1(10)*(J1(10) - 1)*J1(10)*(pin1)**(1)*taun1**(-7 - 2) & ! 10 |  1 |-7
                 + N1(11)*(J1(11) - 1)*J1(11)*(pin1)**(1)*taun1**(-1 - 2) & ! 11 |  1 |-1
                 + N1(12)*(J1(12) - 1)*J1(12)*(pin1)**(1)*taun1**(0 - 2) & ! 12 |  1 | 0
                 + N1(13)*(J1(13) - 1)*J1(13)*(pin1)**(1)*taun1**(1 - 2) & ! 13 |  1 | 1
                 + N1(14)*(J1(14) - 1)*J1(14)*(pin1)**(1)*taun1**(3 - 2) & ! 14 |  1 | 3
                 + N1(15)*(J1(15) - 1)*J1(15)*(pin1)**(2)*taun1**(-3 - 2) & ! 15 |  2 |-3
                 + N1(16)*(J1(16) - 1)*J1(16)*(pin1)**(2)*taun1**(0 - 2) & ! 16 |  2 | 0
                 + N1(17)*(J1(17) - 1)*J1(17)*(pin1)**(2)*taun1**(1 - 2) & ! 17 |  2 | 1
                 + N1(18)*(J1(18) - 1)*J1(18)*(pin1)**(2)*taun1**(3 - 2) & ! 18 |  2 | 3
                 + N1(19)*(J1(19) - 1)*J1(19)*(pin1)**(2)*taun1**(17 - 2) & ! 19 |  2 | 17
                 + N1(20)*(J1(20) - 1)*J1(20)*(pin1)**(3)*taun1**(-4 - 2) & ! 20 |  3 |-4
                 + N1(21)*(J1(21) - 1)*J1(21)*(pin1)**(3)*taun1**(0 - 2) & ! 21 |  3 | 0
                 + N1(22)*(J1(22) - 1)*J1(22)*(pin1)**(3)*taun1**(6 - 2) & ! 22 |  3 | 6
                 + N1(23)*(J1(23) - 1)*J1(23)*(pin1)**(4)*taun1**(-5 - 2) & ! 23 |  4 |-5
                 + N1(24)*(J1(24) - 1)*J1(24)*(pin1)**(4)*taun1**(-2 - 2) & ! 24 |  4 |-2
                 + N1(25)*(J1(25) - 1)*J1(25)*(pin1)**(4)*taun1**(10 - 2) & ! 25 |  4 | 10
                 + N1(26)*(J1(26) - 1)*J1(26)*(pin1)**(5)*taun1**(-8 - 2) & ! 26 |  5 |-8
                 + N1(27)*(J1(27) - 1)*J1(27)*(pin1)**(8)*taun1**(-11 - 2) & ! 27 |  8 |-11
                 + N1(28)*(J1(28) - 1)*J1(28)*(pin1)**(8)*taun1**(-6 - 2) & ! 28 |  8 |-6
                 + N1(29)*(J1(29) - 1)*J1(29)*(pin1)**(21)*taun1**(-29 - 2) & ! 29 | 21 |-29
                 + N1(30)*(J1(30) - 1)*J1(30)*(pin1)**(23)*taun1**(-31 - 2) & ! 30 | 23 |-31
                 + N1(31)*(J1(31) - 1)*J1(31)*(pin1)**(29)*taun1**(-38 - 2) & ! 31 | 29 |-38
                 + N1(32)*(J1(32) - 1)*J1(32)*(pin1)**(30)*taun1**(-39 - 2) & ! 32 | 30 |-39
                 + N1(33)*(J1(33) - 1)*J1(33)*(pin1)**(31)*taun1**(-40 - 2) & ! 33 | 31 |-40
                 + N1(34)*(J1(34) - 1)*J1(34)*(pin1)**(32)*taun1**(-41 - 2)    ! 34 | 32 |-41

! Non-Optimized Calculation
!    gamma1_p0_t2=0.0_SRK
!    DO i=1,SIZE(N1)
!      gamma1_p0_t2 = gamma1_p0_t2 + N1(i)*J1(i)*(J1(i)-1)*(C1-pi)**(I1(i)-0)*(tau-C2)**(J1(i)-2)
!    ENDDO
ENDFUNCTION gamma1_p0_t2
!
!-------------------------------------------------------------------------------
!> Calculation of Residual Component of the Dimensionless Gibbs free energy first derivative with respect pi \f$\gamma^{r}_{\pi}\f$  for region 2
!> Reference 1
!> Table : 14
!> @param pi Dimensionless Pressure \f$\pi\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\gamma^{r}_{\pi}\f$
!>
FUNCTION gamma2_r_p1_t0(pi, tau)
  REAL(SRK), INTENT(IN) :: pi
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: gamma2_r_p1_t0

  REAL(SRK) :: taun1

  !> Table 14.
  !> \f[
  !> \gamma^{r}_{\pi}=\sum\limits_{i=1}^{43} n_{i}I_{i}\pi^{I_{i}-1}(\tau-0.5)^{J_{i}}
  !> \f]

  taun1 = (tau - 0.50_SRK)
  ! Calculate Residual component of the derivative of gibbs free energy with respect to pi and tau
  gamma2_r_p1_t0 = 0.0_SRK &                                      !  i |  I | J
                   + N2(1)*I2(1)*(pi)**(1 - 1)*taun1**(0) & !  1 |  1 | 0
                   + N2(2)*I2(2)*(pi)**(1 - 1)*taun1**(1) & !  2 |  1 | 1
                   + N2(3)*I2(3)*(pi)**(1 - 1)*taun1**(2) & !  3 |  1 | 2
                   + N2(4)*I2(4)*(pi)**(1 - 1)*taun1**(3) & !  4 |  1 | 3
                   + N2(5)*I2(5)*(pi)**(1 - 1)*taun1**(6) & !  5 |  1 | 6
                   + N2(6)*I2(6)*(pi)**(2 - 1)*taun1**(1) & !  6 |  2 | 1
                   + N2(7)*I2(7)*(pi)**(2 - 1)*taun1**(2) & !  7 |  2 | 2
                   + N2(8)*I2(8)*(pi)**(2 - 1)*taun1**(4) & !  8 |  2 | 4
                   + N2(9)*I2(9)*(pi)**(2 - 1)*taun1**(7) & !  9 |  2 | 7
                   + N2(10)*I2(10)*(pi)**(2 - 1)*taun1**(36) & ! 10 |  2 | 36
                   + N2(11)*I2(11)*(pi)**(3 - 1)*taun1**(0) & ! 11 |  3 | 0
                   + N2(12)*I2(12)*(pi)**(3 - 1)*taun1**(1) & ! 12 |  3 | 1
                   + N2(13)*I2(13)*(pi)**(3 - 1)*taun1**(3) & ! 13 |  3 | 3
                   + N2(14)*I2(14)*(pi)**(3 - 1)*taun1**(6) & ! 14 |  3 | 6
                   + N2(15)*I2(15)*(pi)**(3 - 1)*taun1**(35) & ! 15 |  3 | 35
                   + N2(16)*I2(16)*(pi)**(4 - 1)*taun1**(1) & ! 16 |  4 | 1
                   + N2(17)*I2(17)*(pi)**(4 - 1)*taun1**(2) & ! 17 |  4 | 2
                   + N2(18)*I2(18)*(pi)**(4 - 1)*taun1**(3) & ! 18 |  4 | 3
                   + N2(19)*I2(19)*(pi)**(5 - 1)*taun1**(7) & ! 19 |  5 | 7
                   + N2(20)*I2(20)*(pi)**(6 - 1)*taun1**(3) & ! 20 |  6 | 3
                   + N2(21)*I2(21)*(pi)**(6 - 1)*taun1**(16) & ! 21 |  6 | 16
                   + N2(22)*I2(22)*(pi)**(6 - 1)*taun1**(35) & ! 22 |  6 | 35
                   + N2(23)*I2(23)*(pi)**(7 - 1)*taun1**(0) & ! 23 |  7 | 0
                   + N2(24)*I2(24)*(pi)**(7 - 1)*taun1**(11) & ! 24 |  7 | 11
                   + N2(25)*I2(25)*(pi)**(7 - 1)*taun1**(25) & ! 25 |  7 | 25
                   + N2(26)*I2(26)*(pi)**(8 - 1)*taun1**(8) & ! 26 |  8 | 8
                   + N2(27)*I2(27)*(pi)**(8 - 1)*taun1**(36) & ! 27 |  8 | 36
                   + N2(28)*I2(28)*(pi)**(9 - 1)*taun1**(13) & ! 28 |  9 | 13
                   + N2(29)*I2(29)*(pi)**(10 - 1)*taun1**(4) & ! 29 | 10 | 4
                   + N2(30)*I2(30)*(pi)**(10 - 1)*taun1**(10) & ! 30 | 10 | 10
                   + N2(31)*I2(31)*(pi)**(10 - 1)*taun1**(14) & ! 31 | 10 | 14
                   + N2(32)*I2(32)*(pi)**(16 - 1)*taun1**(29) & ! 32 | 16 | 29
                   + N2(33)*I2(33)*(pi)**(16 - 1)*taun1**(50) & ! 33 | 16 | 50
                   + N2(34)*I2(34)*(pi)**(18 - 1)*taun1**(57) & ! 34 | 18 | 57
                   + N2(35)*I2(35)*(pi)**(20 - 1)*taun1**(20) & ! 35 | 20 | 20
                   + N2(36)*I2(36)*(pi)**(20 - 1)*taun1**(35) & ! 36 | 20 | 35
                   + N2(37)*I2(37)*(pi)**(20 - 1)*taun1**(48) & ! 37 | 20 | 48
                   + N2(38)*I2(38)*(pi)**(21 - 1)*taun1**(21) & ! 38 | 21 | 21
                   + N2(39)*I2(39)*(pi)**(22 - 1)*taun1**(53) & ! 39 | 22 | 53
                   + N2(40)*I2(40)*(pi)**(23 - 1)*taun1**(39) & ! 40 | 23 | 39
                   + N2(41)*I2(41)*(pi)**(24 - 1)*taun1**(26) & ! 41 | 24 | 26
                   + N2(42)*I2(42)*(pi)**(24 - 1)*taun1**(40) & ! 42 | 24 | 40
                   + N2(43)*I2(43)*(pi)**(24 - 1)*taun1**(58)    ! 43 | 24 | 58

! Non-Optimized Calculation
!    DO i=1,SIZE(N2)
!        gamma2_r_p1_t0 = gamma2_r_p1_t0 + N2(i)*I2(i)*(pi)**(I2(i)-1)*(tau-0.50_SRK)**(J2(i))
!    ENDDO
ENDFUNCTION gamma2_r_p1_t0
!
!-------------------------------------------------------------------------------
!> Calculation of Residual Component of the Dimensionless Gibbs free energy first derivative with respect pi and tau \f$\gamma^{r}_{\pi\tau}\f$  for region 2
!> Reference 1
!> Table : 14
!> @param pi Dimensionless Pressure \f$\pi\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\gamma^{r}_{\pi\tau}\f$
!>
FUNCTION gamma2_r_p1_t1(pi, tau)
  REAL(SRK), INTENT(IN) :: pi
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: gamma2_r_p1_t1

  REAL(SRK) :: taun1

  !> Table 14.
  !> \f[
  !> \gamma^{r}_{\pi\tau}=\sum\limits_{i=1}^{43} n_{i}I_{i}J_{i}\pi^{I_{i}-1}(\tau-0.5)^{J_{i}-1}
  !> \f]

  taun1 = (tau - 0.50_SRK)
  ! Calculate Residual component of the derivative of gibbs free energy with respect to pi and tau
  gamma2_r_p1_t1 = 0.0_SRK &                                               !  i |  I | J
                   + N2(1)*I2(1)*J2(1)*(pi)**(1 - 1)*taun1**(0 - 1) & !  1 |  1 | 0
                   + N2(2)*I2(2)*J2(2)*(pi)**(1 - 1)*taun1**(1 - 1) & !  2 |  1 | 1
                   + N2(3)*I2(3)*J2(3)*(pi)**(1 - 1)*taun1**(2 - 1) & !  3 |  1 | 2
                   + N2(4)*I2(4)*J2(4)*(pi)**(1 - 1)*taun1**(3 - 1) & !  4 |  1 | 3
                   + N2(5)*I2(5)*J2(5)*(pi)**(1 - 1)*taun1**(6 - 1) & !  5 |  1 | 6
                   + N2(6)*I2(6)*J2(6)*(pi)**(2 - 1)*taun1**(1 - 1) & !  6 |  2 | 1
                   + N2(7)*I2(7)*J2(7)*(pi)**(2 - 1)*taun1**(2 - 1) & !  7 |  2 | 2
                   + N2(8)*I2(8)*J2(8)*(pi)**(2 - 1)*taun1**(4 - 1) & !  8 |  2 | 4
                   + N2(9)*I2(9)*J2(9)*(pi)**(2 - 1)*taun1**(7 - 1) & !  9 |  2 | 7
                   + N2(10)*I2(10)*J2(10)*(pi)**(2 - 1)*taun1**(36 - 1) & ! 10 |  2 | 36
                   + N2(11)*I2(11)*J2(11)*(pi)**(3 - 1)*taun1**(0 - 1) & ! 11 |  3 | 0
                   + N2(12)*I2(12)*J2(12)*(pi)**(3 - 1)*taun1**(1 - 1) & ! 12 |  3 | 1
                   + N2(13)*I2(13)*J2(13)*(pi)**(3 - 1)*taun1**(3 - 1) & ! 13 |  3 | 3
                   + N2(14)*I2(14)*J2(14)*(pi)**(3 - 1)*taun1**(6 - 1) & ! 14 |  3 | 6
                   + N2(15)*I2(15)*J2(15)*(pi)**(3 - 1)*taun1**(35 - 1) & ! 15 |  3 | 35
                   + N2(16)*I2(16)*J2(16)*(pi)**(4 - 1)*taun1**(1 - 1) & ! 16 |  4 | 1
                   + N2(17)*I2(17)*J2(17)*(pi)**(4 - 1)*taun1**(2 - 1) & ! 17 |  4 | 2
                   + N2(18)*I2(18)*J2(18)*(pi)**(4 - 1)*taun1**(3 - 1) & ! 18 |  4 | 3
                   + N2(19)*I2(19)*J2(19)*(pi)**(5 - 1)*taun1**(7 - 1) & ! 19 |  5 | 7
                   + N2(20)*I2(20)*J2(20)*(pi)**(6 - 1)*taun1**(3 - 1) & ! 20 |  6 | 3
                   + N2(21)*I2(21)*J2(21)*(pi)**(6 - 1)*taun1**(16 - 1) & ! 21 |  6 | 16
                   + N2(22)*I2(22)*J2(22)*(pi)**(6 - 1)*taun1**(35 - 1) & ! 22 |  6 | 35
                   + N2(23)*I2(23)*J2(23)*(pi)**(7 - 1)*taun1**(0 - 1) & ! 23 |  7 | 0
                   + N2(24)*I2(24)*J2(24)*(pi)**(7 - 1)*taun1**(11 - 1) & ! 24 |  7 | 11
                   + N2(25)*I2(25)*J2(25)*(pi)**(7 - 1)*taun1**(25 - 1) & ! 25 |  7 | 25
                   + N2(26)*I2(26)*J2(26)*(pi)**(8 - 1)*taun1**(8 - 1) & ! 26 |  8 | 8
                   + N2(27)*I2(27)*J2(27)*(pi)**(8 - 1)*taun1**(36 - 1) & ! 27 |  8 | 36
                   + N2(28)*I2(28)*J2(28)*(pi)**(9 - 1)*taun1**(13 - 1) & ! 28 |  9 | 13
                   + N2(29)*I2(29)*J2(29)*(pi)**(10 - 1)*taun1**(4 - 1) & ! 29 | 10 | 4
                   + N2(30)*I2(30)*J2(30)*(pi)**(10 - 1)*taun1**(10 - 1) & ! 30 | 10 | 10
                   + N2(31)*I2(31)*J2(31)*(pi)**(10 - 1)*taun1**(14 - 1) & ! 31 | 10 | 14
                   + N2(32)*I2(32)*J2(32)*(pi)**(16 - 1)*taun1**(29 - 1) & ! 32 | 16 | 29
                   + N2(33)*I2(33)*J2(33)*(pi)**(16 - 1)*taun1**(50 - 1) & ! 33 | 16 | 50
                   + N2(34)*I2(34)*J2(34)*(pi)**(18 - 1)*taun1**(57 - 1) & ! 34 | 18 | 57
                   + N2(35)*I2(35)*J2(35)*(pi)**(20 - 1)*taun1**(20 - 1) & ! 35 | 20 | 20
                   + N2(36)*I2(36)*J2(36)*(pi)**(20 - 1)*taun1**(35 - 1) & ! 36 | 20 | 35
                   + N2(37)*I2(37)*J2(37)*(pi)**(20 - 1)*taun1**(48 - 1) & ! 37 | 20 | 48
                   + N2(38)*I2(38)*J2(38)*(pi)**(21 - 1)*taun1**(21 - 1) & ! 38 | 21 | 21
                   + N2(39)*I2(39)*J2(39)*(pi)**(22 - 1)*taun1**(53 - 1) & ! 39 | 22 | 53
                   + N2(40)*I2(40)*J2(40)*(pi)**(23 - 1)*taun1**(39 - 1) & ! 40 | 23 | 39
                   + N2(41)*I2(41)*J2(41)*(pi)**(24 - 1)*taun1**(26 - 1) & ! 41 | 24 | 26
                   + N2(42)*I2(42)*J2(42)*(pi)**(24 - 1)*taun1**(40 - 1) & ! 42 | 24 | 40
                   + N2(43)*I2(43)*J2(43)*(pi)**(24 - 1)*taun1**(58 - 1)    ! 43 | 24 | 58

! Non-Optimized Calculation
!    DO i=1,SIZE(N2)
!        gamma2_r_p1_t1 = gamma2_r_p1_t1 + N2(i)*I2(i)*J2(i)*(pi)**(I2(i)-1)*(tau-0.50_SRK)**(J2(i)-1)
!    ENDDO
ENDFUNCTION gamma2_r_p1_t1
!
!-------------------------------------------------------------------------------
!> Calculation of Residual Component of the Dimensionless Gibbs free energy first derivative with respect tau \f$\gamma^{r}_{\tau}\f$  for region 2
!> Reference 1
!> Table : 14
!> @param pi Dimensionless Pressure \f$\pi\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\gamma^{r}_{\tau}\f$
!>
FUNCTION gamma2_r_p0_t1(pi, tau)
  REAL(SRK), INTENT(IN) :: pi
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: gamma2_r_p0_t1

  REAL(SRK) :: taun1

  !> Table 14.
  !> \f[
  !> \gamma^{r}_{\pi\tau}=\sum\limits_{i=1}^{43} n_{i}J_{i}\pi^{I_{i}}(\tau-0.5)^{J_{i}-1}
  !> \f]

  taun1 = (tau - 0.50_SRK)
  ! Calculate Residual component of the derivative of gibbs free energy with respect to pi and tau
  gamma2_r_p0_t1 = 0.0_SRK &                                      !  i |  I | J
                   + N2(1)*J2(1)*(pi)**(1)*taun1**(0 - 1) & !  1 |  1 | 0
                   + N2(2)*J2(2)*(pi)**(1)*taun1**(1 - 1) & !  2 |  1 | 1
                   + N2(3)*J2(3)*(pi)**(1)*taun1**(2 - 1) & !  3 |  1 | 2
                   + N2(4)*J2(4)*(pi)**(1)*taun1**(3 - 1) & !  4 |  1 | 3
                   + N2(5)*J2(5)*(pi)**(1)*taun1**(6 - 1) & !  5 |  1 | 6
                   + N2(6)*J2(6)*(pi)**(2)*taun1**(1 - 1) & !  6 |  2 | 1
                   + N2(7)*J2(7)*(pi)**(2)*taun1**(2 - 1) & !  7 |  2 | 2
                   + N2(8)*J2(8)*(pi)**(2)*taun1**(4 - 1) & !  8 |  2 | 4
                   + N2(9)*J2(9)*(pi)**(2)*taun1**(7 - 1) & !  9 |  2 | 7
                   + N2(10)*J2(10)*(pi)**(2)*taun1**(36 - 1) & ! 10 |  2 | 36
                   + N2(11)*J2(11)*(pi)**(3)*taun1**(0 - 1) & ! 11 |  3 | 0
                   + N2(12)*J2(12)*(pi)**(3)*taun1**(1 - 1) & ! 12 |  3 | 1
                   + N2(13)*J2(13)*(pi)**(3)*taun1**(3 - 1) & ! 13 |  3 | 3
                   + N2(14)*J2(14)*(pi)**(3)*taun1**(6 - 1) & ! 14 |  3 | 6
                   + N2(15)*J2(15)*(pi)**(3)*taun1**(35 - 1) & ! 15 |  3 | 35
                   + N2(16)*J2(16)*(pi)**(4)*taun1**(1 - 1) & ! 16 |  4 | 1
                   + N2(17)*J2(17)*(pi)**(4)*taun1**(2 - 1) & ! 17 |  4 | 2
                   + N2(18)*J2(18)*(pi)**(4)*taun1**(3 - 1) & ! 18 |  4 | 3
                   + N2(19)*J2(19)*(pi)**(5)*taun1**(7 - 1) & ! 19 |  5 | 7
                   + N2(20)*J2(20)*(pi)**(6)*taun1**(3 - 1) & ! 20 |  6 | 3
                   + N2(21)*J2(21)*(pi)**(6)*taun1**(16 - 1) & ! 21 |  6 | 16
                   + N2(22)*J2(22)*(pi)**(6)*taun1**(35 - 1) & ! 22 |  6 | 35
                   + N2(23)*J2(23)*(pi)**(7)*taun1**(0 - 1) & ! 23 |  7 | 0
                   + N2(24)*J2(24)*(pi)**(7)*taun1**(11 - 1) & ! 24 |  7 | 11
                   + N2(25)*J2(25)*(pi)**(7)*taun1**(25 - 1) & ! 25 |  7 | 25
                   + N2(26)*J2(26)*(pi)**(8)*taun1**(8 - 1) & ! 26 |  8 | 8
                   + N2(27)*J2(27)*(pi)**(8)*taun1**(36 - 1) & ! 27 |  8 | 36
                   + N2(28)*J2(28)*(pi)**(9)*taun1**(13 - 1) & ! 28 |  9 | 13
                   + N2(29)*J2(29)*(pi)**(10)*taun1**(4 - 1) & ! 29 | 10 | 4
                   + N2(30)*J2(30)*(pi)**(10)*taun1**(10 - 1) & ! 30 | 10 | 10
                   + N2(31)*J2(31)*(pi)**(10)*taun1**(14 - 1) & ! 31 | 10 | 14
                   + N2(32)*J2(32)*(pi)**(16)*taun1**(29 - 1) & ! 32 | 16 | 29
                   + N2(33)*J2(33)*(pi)**(16)*taun1**(50 - 1) & ! 33 | 16 | 50
                   + N2(34)*J2(34)*(pi)**(18)*taun1**(57 - 1) & ! 34 | 18 | 57
                   + N2(35)*J2(35)*(pi)**(20)*taun1**(20 - 1) & ! 35 | 20 | 20
                   + N2(36)*J2(36)*(pi)**(20)*taun1**(35 - 1) & ! 36 | 20 | 35
                   + N2(37)*J2(37)*(pi)**(20)*taun1**(48 - 1) & ! 37 | 20 | 48
                   + N2(38)*J2(38)*(pi)**(21)*taun1**(21 - 1) & ! 38 | 21 | 21
                   + N2(39)*J2(39)*(pi)**(22)*taun1**(53 - 1) & ! 39 | 22 | 53
                   + N2(40)*J2(40)*(pi)**(23)*taun1**(39 - 1) & ! 40 | 23 | 39
                   + N2(41)*J2(41)*(pi)**(24)*taun1**(26 - 1) & ! 41 | 24 | 26
                   + N2(42)*J2(42)*(pi)**(24)*taun1**(40 - 1) & ! 42 | 24 | 40
                   + N2(43)*J2(43)*(pi)**(24)*taun1**(58 - 1)    ! 43 | 24 | 58

! Non-Optimized Calculation
!    DO i=1,SIZE(N2)
!        gamma2_r_p0_t1 = gamma2_r_p0_t1 + N2(i)*J2(i)*(pi)**(I2(i))*(tau-0.50_SRK)**(J2(i)-1)
!    ENDDO
ENDFUNCTION gamma2_r_p0_t1
!
!-------------------------------------------------------------------------------
!> Calculation of Residual Component of the Dimensionless Gibbs free energy second derivative with respect pi \f$\gamma^{r}_{\pi\pi}\f$  for region 2
!> Reference 1
!> Table : 14
!> @param pi Dimensionless Pressure \f$\pi\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\gamma^{r}_{\pi\pi}\f$
!>
FUNCTION gamma2_r_p2_t0(pi, tau)
  REAL(SRK), INTENT(IN) :: pi
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: gamma2_r_p2_t0

  REAL(SRK) :: taun1
  !> Table 14.
  !> \f[
  !> \gamma^{r}_{\pi\pi}=\sum\limits_{i=1}^{43} n_{i}(I_{i}-1)I_{i}\pi^{I_{i}-2}(\tau-0.5)^{J_{i}}
  !> \f]

  taun1 = (tau - 0.50_SRK)
  ! Calculate Residual component of the derivative of gibbs free energy with respect to pi and tau
  gamma2_r_p2_t0 = 0.0_SRK &                                                 !  i |  I | J
                   + N2(1)*(I2(1) - 1)*I2(1)*(pi)**(1 - 2)*taun1**(0) & !  1 |  1 | 0
                   + N2(2)*(I2(2) - 1)*I2(2)*(pi)**(1 - 2)*taun1**(1) & !  2 |  1 | 1
                   + N2(3)*(I2(3) - 1)*I2(3)*(pi)**(1 - 2)*taun1**(2) & !  3 |  1 | 2
                   + N2(4)*(I2(4) - 1)*I2(4)*(pi)**(1 - 2)*taun1**(3) & !  4 |  1 | 3
                   + N2(5)*(I2(5) - 1)*I2(5)*(pi)**(1 - 2)*taun1**(6) & !  5 |  1 | 6
                   + N2(6)*(I2(6) - 1)*I2(6)*(pi)**(2 - 2)*taun1**(1) & !  6 |  2 | 1
                   + N2(7)*(I2(7) - 1)*I2(7)*(pi)**(2 - 2)*taun1**(2) & !  7 |  2 | 2
                   + N2(8)*(I2(8) - 1)*I2(8)*(pi)**(2 - 2)*taun1**(4) & !  8 |  2 | 4
                   + N2(9)*(I2(9) - 1)*I2(9)*(pi)**(2 - 2)*taun1**(7) & !  9 |  2 | 7
                   + N2(10)*(I2(10) - 1)*I2(10)*(pi)**(2 - 2)*taun1**(36) & ! 10 |  2 | 36
                   + N2(11)*(I2(11) - 1)*I2(11)*(pi)**(3 - 2)*taun1**(0) & ! 11 |  3 | 0
                   + N2(12)*(I2(12) - 1)*I2(12)*(pi)**(3 - 2)*taun1**(1) & ! 12 |  3 | 1
                   + N2(13)*(I2(13) - 1)*I2(13)*(pi)**(3 - 2)*taun1**(3) & ! 13 |  3 | 3
                   + N2(14)*(I2(14) - 1)*I2(14)*(pi)**(3 - 2)*taun1**(6) & ! 14 |  3 | 6
                   + N2(15)*(I2(15) - 1)*I2(15)*(pi)**(3 - 2)*taun1**(35) & ! 15 |  3 | 35
                   + N2(16)*(I2(16) - 1)*I2(16)*(pi)**(4 - 2)*taun1**(1) & ! 16 |  4 | 1
                   + N2(17)*(I2(17) - 1)*I2(17)*(pi)**(4 - 2)*taun1**(2) & ! 17 |  4 | 2
                   + N2(18)*(I2(18) - 1)*I2(18)*(pi)**(4 - 2)*taun1**(3) & ! 18 |  4 | 3
                   + N2(19)*(I2(19) - 1)*I2(19)*(pi)**(5 - 2)*taun1**(7) & ! 19 |  5 | 7
                   + N2(20)*(I2(20) - 1)*I2(20)*(pi)**(6 - 2)*taun1**(3) & ! 20 |  6 | 3
                   + N2(21)*(I2(21) - 1)*I2(21)*(pi)**(6 - 2)*taun1**(16) & ! 21 |  6 | 16
                   + N2(22)*(I2(22) - 1)*I2(22)*(pi)**(6 - 2)*taun1**(35) & ! 22 |  6 | 35
                   + N2(23)*(I2(23) - 1)*I2(23)*(pi)**(7 - 2)*taun1**(0) & ! 23 |  7 | 0
                   + N2(24)*(I2(24) - 1)*I2(24)*(pi)**(7 - 2)*taun1**(11) & ! 24 |  7 | 11
                   + N2(25)*(I2(25) - 1)*I2(25)*(pi)**(7 - 2)*taun1**(25) & ! 25 |  7 | 25
                   + N2(26)*(I2(26) - 1)*I2(26)*(pi)**(8 - 2)*taun1**(8) & ! 26 |  8 | 8
                   + N2(27)*(I2(27) - 1)*I2(27)*(pi)**(8 - 2)*taun1**(36) & ! 27 |  8 | 36
                   + N2(28)*(I2(28) - 1)*I2(28)*(pi)**(9 - 2)*taun1**(13) & ! 28 |  9 | 13
                   + N2(29)*(I2(29) - 1)*I2(29)*(pi)**(10 - 2)*taun1**(4) & ! 29 | 10 | 4
                   + N2(30)*(I2(30) - 1)*I2(30)*(pi)**(10 - 2)*taun1**(10) & ! 30 | 10 | 10
                   + N2(31)*(I2(31) - 1)*I2(31)*(pi)**(10 - 2)*taun1**(14) & ! 31 | 10 | 14
                   + N2(32)*(I2(32) - 1)*I2(32)*(pi)**(16 - 2)*taun1**(29) & ! 32 | 16 | 29
                   + N2(33)*(I2(33) - 1)*I2(33)*(pi)**(16 - 2)*taun1**(50) & ! 33 | 16 | 50
                   + N2(34)*(I2(34) - 1)*I2(34)*(pi)**(18 - 2)*taun1**(57) & ! 34 | 18 | 57
                   + N2(35)*(I2(35) - 1)*I2(35)*(pi)**(20 - 2)*taun1**(20) & ! 35 | 20 | 20
                   + N2(36)*(I2(36) - 1)*I2(36)*(pi)**(20 - 2)*taun1**(35) & ! 36 | 20 | 35
                   + N2(37)*(I2(37) - 1)*I2(37)*(pi)**(20 - 2)*taun1**(48) & ! 37 | 20 | 48
                   + N2(38)*(I2(38) - 1)*I2(38)*(pi)**(21 - 2)*taun1**(21) & ! 38 | 21 | 21
                   + N2(39)*(I2(39) - 1)*I2(39)*(pi)**(22 - 2)*taun1**(53) & ! 39 | 22 | 53
                   + N2(40)*(I2(40) - 1)*I2(40)*(pi)**(23 - 2)*taun1**(39) & ! 40 | 23 | 39
                   + N2(41)*(I2(41) - 1)*I2(41)*(pi)**(24 - 2)*taun1**(26) & ! 41 | 24 | 26
                   + N2(42)*(I2(42) - 1)*I2(42)*(pi)**(24 - 2)*taun1**(40) & ! 42 | 24 | 40
                   + N2(43)*(I2(43) - 1)*I2(43)*(pi)**(24 - 2)*taun1**(58)    ! 43 | 24 | 58

! Non-Optimized Calculation
!    DO i=1,SIZE(N2)
!        gamma2_r_p2_t0 = gamma2_r_p2_t0 + N2(i)*I2(i)*(I2(i)-1)*(pi)**(I2(i)-2)*(tau-0.50_SRK)**(J2(i))
!    ENDDO
ENDFUNCTION gamma2_r_p2_t0
!
!-------------------------------------------------------------------------------
!> Calculation of Residual Component of the Dimensionless Gibbs
!> free energy second derivative with respect pi \f$\gamma^{r}_{\tau\tau}\f$  for region 2
!!
!> Reference 1
!> Table : 14
!> @param pi Dimensionless Pressure \f$\pi\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\gamma^{r}_{\tau\tau}\f$
!>
FUNCTION gamma2_r_p0_t2(pi, tau)
  REAL(SRK), INTENT(IN) :: pi
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: gamma2_r_p0_t2

  REAL(SRK) :: taun1

  !> Table 14.
  !> \f[
  !> \gamma^{r}_{\tau\tau}=\sum\limits_{i=1}^{43} n_{i}(J_{i}-1)J_{i}\pi^{I_{i}}(\tau-0.5)^{J_{i}-2}
  !> \f]

  taun1 = (tau - 0.50_SRK)
  ! Calculate Residual component of the derivative of gibbs free energy with respect to pi and tau
  gamma2_r_p0_t2 = 0.0_SRK &                                             !  i |  I | J
                   + N2(1)*(J2(1) - 1)*J2(1)*(pi)**(1)*taun1**(0 - 2) & !  1 |  1 | 0
                   + N2(2)*(J2(2) - 1)*J2(2)*(pi)**(1)*taun1**(1 - 2) & !  2 |  1 | 1
                   + N2(3)*(J2(3) - 1)*J2(3)*(pi)**(1)*taun1**(2 - 2) & !  3 |  1 | 2
                   + N2(4)*(J2(4) - 1)*J2(4)*(pi)**(1)*taun1**(3 - 2) & !  4 |  1 | 3
                   + N2(5)*(J2(5) - 1)*J2(5)*(pi)**(1)*taun1**(6 - 2) & !  5 |  1 | 6
                   + N2(6)*(J2(6) - 1)*J2(6)*(pi)**(2)*taun1**(1 - 2) & !  6 |  2 | 1
                   + N2(7)*(J2(7) - 1)*J2(7)*(pi)**(2)*taun1**(2 - 2) & !  7 |  2 | 2
                   + N2(8)*(J2(8) - 1)*J2(8)*(pi)**(2)*taun1**(4 - 2) & !  8 |  2 | 4
                   + N2(9)*(J2(9) - 1)*J2(9)*(pi)**(2)*taun1**(7 - 2) & !  9 |  2 | 7
                   + N2(10)*(J2(10) - 1)*J2(10)*(pi)**(2)*taun1**(36 - 2) & ! 10 |  2 | 36
                   + N2(11)*(J2(11) - 1)*J2(11)*(pi)**(3)*taun1**(0 - 2) & ! 11 |  3 | 0
                   + N2(12)*(J2(12) - 1)*J2(12)*(pi)**(3)*taun1**(1 - 2) & ! 12 |  3 | 1
                   + N2(13)*(J2(13) - 1)*J2(13)*(pi)**(3)*taun1**(3 - 2) & ! 13 |  3 | 3
                   + N2(14)*(J2(14) - 1)*J2(14)*(pi)**(3)*taun1**(6 - 2) & ! 14 |  3 | 6
                   + N2(15)*(J2(15) - 1)*J2(15)*(pi)**(3)*taun1**(35 - 2) & ! 15 |  3 | 35
                   + N2(16)*(J2(16) - 1)*J2(16)*(pi)**(4)*taun1**(1 - 2) & ! 16 |  4 | 1
                   + N2(17)*(J2(17) - 1)*J2(17)*(pi)**(4)*taun1**(2 - 2) & ! 17 |  4 | 2
                   + N2(18)*(J2(18) - 1)*J2(18)*(pi)**(4)*taun1**(3 - 2) & ! 18 |  4 | 3
                   + N2(19)*(J2(19) - 1)*J2(19)*(pi)**(5)*taun1**(7 - 2) & ! 19 |  5 | 7
                   + N2(20)*(J2(20) - 1)*J2(20)*(pi)**(6)*taun1**(3 - 2) & ! 20 |  6 | 3
                   + N2(21)*(J2(21) - 1)*J2(21)*(pi)**(6)*taun1**(16 - 2) & ! 21 |  6 | 16
                   + N2(22)*(J2(22) - 1)*J2(22)*(pi)**(6)*taun1**(35 - 2) & ! 22 |  6 | 35
                   + N2(23)*(J2(23) - 1)*J2(23)*(pi)**(7)*taun1**(0 - 2) & ! 23 |  7 | 0
                   + N2(24)*(J2(24) - 1)*J2(24)*(pi)**(7)*taun1**(11 - 2) & ! 24 |  7 | 11
                   + N2(25)*(J2(25) - 1)*J2(25)*(pi)**(7)*taun1**(25 - 2) & ! 25 |  7 | 25
                   + N2(26)*(J2(26) - 1)*J2(26)*(pi)**(8)*taun1**(8 - 2) & ! 26 |  8 | 8
                   + N2(27)*(J2(27) - 1)*J2(27)*(pi)**(8)*taun1**(36 - 2) & ! 27 |  8 | 36
                   + N2(28)*(J2(28) - 1)*J2(28)*(pi)**(9)*taun1**(13 - 2) & ! 28 |  9 | 13
                   + N2(29)*(J2(29) - 1)*J2(29)*(pi)**(10)*taun1**(4 - 2) & ! 29 | 10 | 4
                   + N2(30)*(J2(30) - 1)*J2(30)*(pi)**(10)*taun1**(10 - 2) & ! 30 | 10 | 10
                   + N2(31)*(J2(31) - 1)*J2(31)*(pi)**(10)*taun1**(14 - 2) & ! 31 | 10 | 14
                   + N2(32)*(J2(32) - 1)*J2(32)*(pi)**(16)*taun1**(29 - 2) & ! 32 | 16 | 29
                   + N2(33)*(J2(33) - 1)*J2(33)*(pi)**(16)*taun1**(50 - 2) & ! 33 | 16 | 50
                   + N2(34)*(J2(34) - 1)*J2(34)*(pi)**(18)*taun1**(57 - 2) & ! 34 | 18 | 57
                   + N2(35)*(J2(35) - 1)*J2(35)*(pi)**(20)*taun1**(20 - 2) & ! 35 | 20 | 20
                   + N2(36)*(J2(36) - 1)*J2(36)*(pi)**(20)*taun1**(35 - 2) & ! 36 | 20 | 35
                   + N2(37)*(J2(37) - 1)*J2(37)*(pi)**(20)*taun1**(48 - 2) & ! 37 | 20 | 48
                   + N2(38)*(J2(38) - 1)*J2(38)*(pi)**(21)*taun1**(21 - 2) & ! 38 | 21 | 21
                   + N2(39)*(J2(39) - 1)*J2(39)*(pi)**(22)*taun1**(53 - 2) & ! 39 | 22 | 53
                   + N2(40)*(J2(40) - 1)*J2(40)*(pi)**(23)*taun1**(39 - 2) & ! 40 | 23 | 39
                   + N2(41)*(J2(41) - 1)*J2(41)*(pi)**(24)*taun1**(26 - 2) & ! 41 | 24 | 26
                   + N2(42)*(J2(42) - 1)*J2(42)*(pi)**(24)*taun1**(40 - 2) & ! 42 | 24 | 40
                   + N2(43)*(J2(43) - 1)*J2(43)*(pi)**(24)*taun1**(58 - 2)    ! 43 | 24 | 58

! Non-Optimized Calculation
!    DO i=1,SIZE(N2)
!        gamma2_r_p0_t2 = gamma2_r_p0_t2 + N2(i)*(J2(i)-1)*J2(i)*(pi)**(I2(i))*(tau-0.50_SRK)**(J2(i)-1)
!    ENDDO
ENDFUNCTION gamma2_r_p0_t2
!
!-------------------------------------------------------------------------------
!> Calculation of Ideal Gas Component of the Dimensionless Gibbs free energy and its derivatives with respect to order of pi and order of tau for region 2 \f$\gamma^{o}\f$
!> Reference 1
!> Table : 13
!> @param pi Dimensionless Pressure \f$\pi\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @param order_pi Order of the derivative with respect to \f$\pi\f$
!> @param order_tau Order of the derivative with respect to \f$\tau\f$
!> @return \f$\gamma^{o}\f$
!>
FUNCTION gamma2_o(pi, tau, order_pi, order_tau)
  REAL(SRK), INTENT(IN) :: pi
  REAL(SRK), INTENT(IN) :: tau
  INTEGER(SIK), INTENT(IN) :: order_pi
  INTEGER(SIK), INTENT(IN) :: order_tau
  REAL(SRK) :: gamma2_o

  INTEGER(SIK) :: i
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  gamma2_o = 0.0_SRK

  !> Check the order of derivative with respect to density and temperature is good
  IF (order_tau < 0 .OR. order_tau > 2 .OR. order_pi < 0 .OR. order_pi > 2) THEN
    WRITE (msg, *) &
        "Derivative order [0 < x < 2] "// &
        "Derivative order with respect to temperature", order_tau, &
        "Derivative order with respect to pressure", order_pi
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF

  ! Calculate the derivative of gibbs free energy with respect to pi and tau
  ! Take into account higher order derivatives for pi and tau
  ! Ideal Gas component of the gibbs free energy
  ! Only dependent on tau, if derivative of tau taken, do not perform the rest of the derivative
  IF (order_pi == 0) THEN
    IF (order_tau == 0) THEN
      gamma2_o = log(pi)
      DO i = 1, SIZE(No2)
        gamma2_o = gamma2_o + No2(i)*(tau)**(Jo2(i) - order_tau)
      ENDDO
    ELSEIF (order_tau == 1) THEN
      gamma2_o = 0.0_SRK
      DO i = 1, SIZE(No2)
        gamma2_o = gamma2_o + No2(i)*Jo2(i)*(tau)**(Jo2(i) - order_tau)
      ENDDO
    ELSEIF (order_tau == 2) THEN
      gamma2_o = 0.0_SRK
      DO i = 1, SIZE(No2)
        gamma2_o = gamma2_o + No2(i)*Jo2(i)*(Jo2(i) - 1)*(tau)**(Jo2(i) - order_tau)
      ENDDO
    ENDIF
  ELSEIF (order_pi == 1 .AND. order_tau == 0) THEN
    gamma2_o = 1.0_SRK/pi
  ELSEIF (order_pi == 1 .AND. order_tau == 1) THEN
    gamma2_o = 0.0_SRK
  ELSEIF (order_pi == 2 .AND. order_tau == 0) THEN
    gamma2_o = -1.0_SRK/(pi*pi)
  ENDIF
ENDFUNCTION gamma2_o
!
!-------------------------------------------------------------------------------
!> Calculation of Residual Component of the Dimensionless Gibbs free energy and its derivatives with respect to order of pi and order of tau for region 5 \f$\gamma^{r}\f$
!> Reference 1
!> Table : 41
!> Equations 32,34
!> @param pi Dimensionless Pressure \f$\pi\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @param order_pi Order of the derivative with respect to \f$\pi\f$
!> @param order_tau Order of the derivative with respect to \f$\tau\f$
!> @return \f$\gamma^{r}\f$
!>
FUNCTION gamma5_r(pi, tau, order_pi, order_tau)
  REAL(SRK), INTENT(IN) :: pi
  REAL(SRK), INTENT(IN) :: tau
  INTEGER(SIK), INTENT(IN) :: order_pi
  INTEGER(SIK), INTENT(IN) :: order_tau
  REAL(SRK) :: gamma5_r

  INTEGER(SIK) :: i
  REAL(SRK) :: term_pi, term_tau
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  !> Check the order of derivative with respect to density and temperature is good
  IF (order_tau < 0 .OR. order_pi < 0) THEN
    WRITE (msg, *) &
        "Derivative order must be greater than or equal to 0 "// &
        "Derivative order with respect to temperature", order_tau, &
        "Derivative order with respect to pressure", order_pi
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF

  ! Calculate Residual component of the derivative of gibbs free energy with respect to pi and tau
  ! Take into account higher order derivatives for pi and tau
  ! Equation 34.
  gamma5_r = 0.0_SRK
  DO i = 1, SIZE(N5)
    term_pi = power_derivative(I5(i), order_pi)*(pi)**(I5(i) - order_pi)
    term_tau = power_derivative(J5(i), order_tau)*(tau)**(J5(i) - order_tau)
    gamma5_r = gamma5_r + N5(i)*term_pi*term_tau
  ENDDO
ENDFUNCTION gamma5_r
!
!-------------------------------------------------------------------------------
!> Calculation of Ideal Gas Component of the Dimensionless Gibbs free energy and its derivatives with respect to order of pi and order of tau for region 5 \f$\gamma^{o}\f$
!> Reference 1
!> Table : 40
!> @param pi Dimensionless Pressure \f$\pi\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @param order_pi Order of the derivative with respect to \f$\pi\f$
!> @param order_tau Order of the derivative with respect to \f$\tau\f$
!> @return \f$\gamma^{o}\f$
!>
FUNCTION gamma5_o(pi, tau, order_pi, order_tau)
  REAL(SRK), INTENT(IN) :: pi
  REAL(SRK), INTENT(IN) :: tau
  INTEGER(SIK), INTENT(IN) :: order_pi
  INTEGER(SIK), INTENT(IN) :: order_tau
  REAL(SRK) :: gamma5_o

  INTEGER(SIK) :: i
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  !> Check the order of derivative with respect to density and temperature is good
  IF (order_tau < 0 .OR. order_pi < 0) THEN
    WRITE (msg, *) &
        "Derivative order must be greater than or equal to 0 "// &
        "Derivative order with respect to temperature", order_tau, &
        "Derivative order with respect to pressure", order_pi
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF

  ! Calculate the first term of the equation
  IF (order_tau >= 1) THEN
    ! First term is independent of tau
    gamma5_o = 0
  ELSEIF (order_pi == 0) THEN
    !> Base value for the first term
    gamma5_o = log(pi)
  ELSE
    !> Calculate the derivative of the first term to the order of the derivative
    gamma5_o = power_derivative(-1, order_pi - 1)*pi**(-1*order_pi) ! Power Derivative
  ENDIF

  ! Calculate ideal gas component of the derivative of gibbs free energy with respect to pi and tau
  ! Take into account higher order derivatives for pi and tau
  IF (order_pi == 0) THEN
    DO i = 1, SIZE(No5)
      gamma5_o = gamma5_o + No5(i)*power_derivative(Jo5(i), order_tau)*(tau)**(Jo5(i) - order_tau)
    ENDDO
  ENDIF
ENDFUNCTION gamma5_o
!
!-------------------------------------------------------------------------------
!> Calculation the Dimensionless Helmholtz free energy and its derivatives with respect to order of delta and order of tau for region 3 \f$\phi\f$
!> Reference 1
!> Table : 32
!> @param pi Dimensionless Density \f$\delta\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @param order_pi Order of the derivative with respect to \f$\delta\f$
!> @param order_tau Order of the derivative with respect to \f$\tau\f$
!> @return \f$\phi\f$
!>
FUNCTION phi3(delta, tau, order_delta, order_tau)
  REAL(SRK), INTENT(IN) :: delta
  REAL(SRK), INTENT(IN) :: tau
  INTEGER(SIK), INTENT(IN) :: order_delta
  INTEGER(SIK), INTENT(IN) :: order_tau
  REAL(SRK) :: phi3

  phi3 = 0.0_SRK
  IF (order_delta == 0 .AND. order_tau == 1) THEN
    phi3 = phi_tau(delta, tau)
  ELSEIF (order_delta == 0 .AND. order_tau == 2) THEN
    phi3 = phi_tau_tau(delta, tau)
  ELSEIF (order_delta == 1 .AND. order_tau == 1) THEN
    phi3 = phi_delta_tau(delta, tau)
  ELSEIF (order_delta == 1 .AND. order_tau == 0) THEN
    phi3 = phi_delta(delta, tau)
  ELSEIF (order_delta == 2 .AND. order_tau == 0) THEN
    phi3 = phi_delta_delta(delta, tau)
  ELSE
    CALL eWaterProp%raiseFatalError( &
       "Function phi3 was given a derivative order that was out of bounds "// &
       "order_delta = 0, 1, or 2 "// &
       "order_tau = 0, 1, or 2")
  ENDIF
ENDFUNCTION phi3
!
!-------------------------------------------------------------------------------
!> Calculation the Dimensionless Helmholtz free energy with respect to tau for region 3 \f$\phi_{\tau}\f$
!> Reference 1
!> Table : 32
!> @param pi Dimensionless Density \f$\delta\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\phi_{\tau}\f$
!>
FUNCTION phi_tau(delta, tau)
  REAL(SRK), INTENT(IN) :: delta
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: phi_tau

  INTEGER(SIK) :: i
  REAL(SRK) :: delta_term, tau_term

  REAL(SRK) :: delta_old = 0, tau_old = 0, value_old = 0
  !
  ! Check to see if at the same D/T
  IF (ABS(delta - delta_old) < tolerance .AND. ABS(tau - tau_old) < tolerance) THEN
    phi_tau = value_old
  ELSE
    phi_tau = 0

    DO i = 2, SIZE(N3)
      delta_term = delta**(I3(i))
      tau_term = J3(i)*tau**(J3(i) - 1)
      phi_tau = phi_tau + N3(i)*delta_term*tau_term
    ENDDO

    delta_old = delta
    tau_old = tau
    value_old = phi_tau
  ENDIF
ENDFUNCTION phi_tau
!
!-------------------------------------------------------------------------------
!> Calculation the Dimensionless Helmholtz free energy with second respect to tau for region 3 \f$\phi_{\tau\tau}\f$
!> Reference 1
!> Table : 32
!> @param pi Dimensionless Density \f$\delta\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\phi_{\tau\tau}\f$
!>
FUNCTION phi_tau_tau(delta, tau)
  REAL(SRK), INTENT(IN) :: delta
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: phi_tau_tau

  INTEGER(SIK) :: i
  REAL(SRK) :: delta_term, tau_term
!TODO   see if SAVE can be used else, remove these variables
  REAL(SRK) :: delta_old = 0, tau_old = 0, value_old = 0
  !
  ! Check to see if at the same D/T
  IF (ABS(delta - delta_old) < tolerance .AND. ABS(tau - tau_old) < tolerance) THEN
    phi_tau_tau = value_old
  ELSE
    phi_tau_tau = 0

    DO i = 2, SIZE(N3)
      delta_term = delta**(I3(i))
      tau_term = (J3(i) - 1)*J3(i)*tau**(J3(i) - 2)
      phi_tau_tau = phi_tau_tau + N3(i)*delta_term*tau_term
    ENDDO

    delta_old = delta
    tau_old = tau
    value_old = phi_tau_tau
  ENDIF
ENDFUNCTION phi_tau_tau
!
!-------------------------------------------------------------------------------
!> Calculation the Dimensionless Helmholtz free energy with second respect to tau and delta for region 3 \f$\phi_{\delta\tau}\f$
!> Reference 1
!> Table : 32
!> @param pi Dimensionless Density \f$\delta\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\phi_{\delta\tau}\f$
!>
FUNCTION phi_delta_tau(delta, tau)
  REAL(SRK), INTENT(IN) :: delta
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: phi_delta_tau

  INTEGER(SIK) :: i
  REAL(SRK) :: delta_term, tau_term
!TODO   see if SAVE can be used else, remove these variables
  REAL(SRK) :: delta_old = 0, tau_old = 0, value_old = 0
  !
  ! Check to see if at the same D/T
  IF (ABS(delta - delta_old) < tolerance .AND. ABS(tau - tau_old) < tolerance) THEN
    phi_delta_tau = value_old
  ELSE
    ! First term is independent of tau
    phi_delta_tau = 0

    DO i = 2, SIZE(N3)
      delta_term = I3(i)*delta**(I3(i) - 1)
      tau_term = J3(i)*tau**(J3(i) - 1)
      phi_delta_tau = phi_delta_tau + N3(i)*delta_term*tau_term
    ENDDO

    delta_old = delta
    tau_old = tau
    value_old = phi_delta_tau
  ENDIF
ENDFUNCTION phi_delta_tau
!
!-------------------------------------------------------------------------------
!> Calculation the Dimensionless Helmholtz free energy with second respect to delta for region 3 \f$\phi_{\delta}\f$
!> Reference 1
!> Table : 32
!> @param pi Dimensionless Density \f$\delta\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\phi_{\delta}\f$
!>
FUNCTION phi_delta(delta, tau)
  REAL(SRK), INTENT(IN) :: delta
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: phi_delta

  INTEGER(SIK) :: i
  REAL(SRK) :: delta_term, tau_term
!TODO   see if SAVE can be used else, remove these variables
  REAL(SRK) :: delta_old = 0, tau_old = 0, value_old = 0
  !
  ! Check to see if at the same D and T
  IF (ABS(delta - delta_old) < tolerance .AND. ABS(tau - tau_old) < tolerance) THEN
    phi_delta = value_old
  ELSE
    !> Calculate the derivative of the first term to the order of the derivative
    phi_delta = N3(1)/delta ! Power Derivative

    DO i = 2, SIZE(N3)
      delta_term = I3(i)*delta**(I3(i) - 1)
      tau_term = tau**(J3(i))
      phi_delta = phi_delta + N3(i)*delta_term*tau_term
    ENDDO

    delta_old = delta
    tau_old = tau
    value_old = phi_delta
  ENDIF
ENDFUNCTION phi_delta
!
!-------------------------------------------------------------------------------
!> Calculation the Dimensionless Helmholtz free energy with second respect to delta for region 3 \f$\phi_{\delta\delta}\f$
!> Reference 1
!> Table : 32
!> @param pi Dimensionless Density \f$\delta\f$
!> @param tau Dimensionless Temperature \f$\tau\f$
!> @return \f$\phi_{\delta\delta}\f$
!>
FUNCTION phi_delta_delta(delta, tau)
  REAL(SRK), INTENT(IN) :: delta
  REAL(SRK), INTENT(IN) :: tau
  REAL(SRK) :: phi_delta_delta

  INTEGER(SIK) :: i
  REAL(SRK) :: delta_term, tau_term
!TODO   see if SAVE can be used else, remove these variables
  REAL(SRK) :: delta_old = 0, tau_old = 0, value_old = 0
  !
  ! Check to see if at the same D and T
  IF (ABS(delta - delta_old) < tolerance .AND. ABS(tau - tau_old) < tolerance) THEN
    phi_delta_delta = value_old
  ELSE
    !> Calculate the derivative of the first term to the order of the derivative
    phi_delta_delta = -N3(1)/(delta*delta) ! Power Derivative

    DO i = 2, SIZE(N3)
      delta_term = I3(i)*(I3(i) - 1)*delta**(I3(i) - 2)
      tau_term = tau**(J3(i))
      phi_delta_delta = phi_delta_delta + N3(i)*delta_term*tau_term
    ENDDO

    delta_old = delta
    tau_old = tau
    value_old = phi_delta_delta
  ENDIF
ENDFUNCTION phi_delta_delta
!
!===============================================================================
! Section: Base Property Functions
!===============================================================================
!
!-------------------------------------------------------------------------------
!> Calculation of Specific Volume \f$v\f$
!> Reference 1
!> Tables : 3,12,31,39
!> @param P Pressure in MPa
!> @param T Temperature in Kelvin
!> @param ireg flow regime ID
!> @return \f$v\f$ m^3/kg
!>
FUNCTION vpt(P, T, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: vpt

  REAL(SRK) :: pi, tau ! Dimensionless pressure and temperature
  INTEGER(SIK) :: iregInt
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  IF (ireg == 0) THEN
    CALL regsopt(P, T, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  ! Normalize variables
  pi = normalize_pressure(P, iregInt)
  tau = normalize_temperature(T, iregInt)

  !> Reference 1: Thermohydrodynamic Relation
  !> \f[
  !> v = \left( \frac{\partial g}{\partial P} \right)_{T}
  !> \f]

  IF (iregInt == 1) THEN
    !> Region 1: Table 3
    !> \f[
    !> v = \frac{RT}{P} \pi \gamma_{\pi}
    !> \f]
    vpt = (pi/P)*gamma1_p1_t0(pi, tau)*(R*T)*(1.0E-3_SRK) ! [m^3/kg]
  ELSEIF (iregInt == 2) THEN
    !> Region 2: Table 12
    !> \f[
    !> v = \frac{RT}{P} \pi \left( \gamma^{o}_{\pi} + \gamma^{r}_{\pi} \right)
    !> \f]
    vpt = (pi/P)*(gamma2_o(pi, tau, 1, 0) + gamma2_r_p1_t0(pi, tau))*R*T*1.0E-3_SRK ! [m^3/kg]
  ELSEIF (iregInt == 3) THEN
    !> Region 3:
    !> Use unique function vpt3n
    vpt = vpt3n(P, T)
  ELSEIF (iregInt == 5) THEN
    !> Region 2: Table 39
    !> \f[
    !> v = \frac{RT}{P} \pi \left( \gamma^{o}_{\pi} + \gamma^{r}_{\pi} \right)
    !> \f]
    vpt = (R*T*pi/P)*(gamma5_r(pi, tau, 1, 0) + gamma5_o(pi, tau, 1, 0))*1.0E-3_SRK ! [m^3/kg]
  ELSE
    VPT = 0.0_SRK
    WRITE (msg, *) &
       "Pressure and Temperature are out of bounds "// &
       " Pressure [bar] Temperature [C] ", &
       P*1.0E1_SRK, T - 273.15_SRK
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF
ENDFUNCTION vpt
!
!-------------------------------------------------------------------------------
!> Calculation of Specific Enthalpy \f$h\f$
!> Reference 1
!> Tables : 3,12,31,39
!> @param P Pressure in MPa
!> @param T Temperature in Kelvin
!> @param ireg flow regime ID
!> @return \f$h\f$ kJ/kg
!>
FUNCTION hpt(P, T, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: hpt

  REAL(SRK) :: pi, tau ! Dimensionless pressure and temperature
  REAL(SRK) :: V
  INTEGER(SIK) :: iregInt
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  IF (ireg == 0) THEN
    CALL regsopt(P, T, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  ! Normalize variables
  pi = normalize_pressure(P, iregInt)
  tau = normalize_temperature(T, iregInt)

  !> Reference 1: Thermohydrodynamic Relation
  !> \f[
  !> h = g - T \left( \frac{\partial g}{\partial T} \right)_{P}
  !> \f]

  IF (iregInt == 1) THEN
    !> Region 1: Table 3
    !> \f[
    !> h = RT \tau \gamma_{\tau}
    !> \f]
    hpt = tau*gamma1_p0_t1(pi, tau)*(r*t) ! [kj/kg]
  ELSEIF (iregInt == 2) THEN
    !> Region 2: Table 12
    !> \f[
    !> h = RT \tau \left( \gamma^{o}_{\tau} + \gamma^{r}_{\tau} \right)
    !> \f]
    hpt = tau*(gamma2_o(pi, tau, 0, 1) + gamma2_r_p0_t1(pi, tau))*(r*t) ! [kj/kg]
  ELSEIF (iregInt == 3) THEN
    !> Region 3: Table 31
    !> \f[
    !> h = RT \left( \tau \phi_{\tau} + \delta \phi_{\delta} \right)
    !> \f]
    v = vpt(p, t, iregInt)
    hpt = hvt3n(v, t)
  ELSEIF (iregInt == 5) THEN
    !> Region 5: Table 39
    !> \f[
    !> h = - RT \tau \left( \gamma^{o}_{\tau} + \gamma^{r}_{\tau} \right)
    !> \f]
    hpt = R*T*tau*(gamma5_r(pi, tau, 0, 1) + gamma5_o(pi, tau, 0, 1)) ! [kJ/kg]
  ELSE
    hpt = 0.0_SRK
    WRITE (msg, *) &
       "Pressure and Temperature are out of bounds "// &
       " Pressure [bar] Temperature [C] ", &
       P*1.0E1_SRK, T - 273.15_SRK
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF
ENDFUNCTION hpt
!
!-------------------------------------------------------------------------------
!> Calculation of Isobaric Specific heat \f$c_{p}\f$
!> Reference 1
!> Tables : 3,12,31,39
!> @param P Pressure in MPa
!> @param T Temperature in Kelvin
!> @param ireg flow regime ID
!> @return \f$c_{p}\f$ kJ/kg-K
!>
FUNCTION cppt(P, T, ireg, V_in)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK), INTENT(IN), OPTIONAL :: V_in
  REAL(SRK) :: cppt

  REAL(SRK) :: pi, tau, delta ! Dimensionless pressure, temperature, delta
  REAL(SRK) :: V
  REAL(SRK) :: phi_d, phi_dt, phi_dd, phi_tt
  INTEGER(SIK) :: iregInt
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  cppt = 0.0_SRK

  IF (ireg == 0) THEN
    CALL regsopt(P, T, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  ! Normalize variables
  pi = normalize_pressure(P, iregInt)
  tau = normalize_temperature(T, iregInt)

  !> Reference 1: Thermohydrodynamic Relation
  !> \f[
  !> c_{p} = \left( \frac{\partial h}{\partial T} \right)_{P}
  !> \f]

  IF (iregInt == 1) THEN
    !> Region 1:Table 3
    !> \f[
    !> c_{p} = - R \tau^{2} \gamma_{\tau\tau}
    !> \f]
    cppt = -R*(tau*tau)*gamma1_p0_t2(pi, tau) ! [kj/kg-K]
  ELSEIF (iregInt == 2) THEN
    !> Region 1:Table 12
    !> \f[
    !> c_{p} = - R\tau^{2} \left(  \gamma^{o}_{\tau\tau} + \gamma^{r}_{\tau\tau} \right)
    !> \f]
    cppt = R*(-tau*tau)*(gamma2_o(pi, tau, 0, 2) + gamma2_r_p0_t2(pi, tau))
  ELSEIF (iregInt == 3) THEN
    !> Region 1:Table 31
    !> \f[
    !> c_{p} = - R \left(
    !>               -\tau^{2} \phi_{\tau\tau}
    !>               +\frac{\left( \delta \phi_{\delta} - \delta \tau \phi_{\delta\tau} \right)^{2}}
    !>                     {2\delta\phi_{\delta}+\delta^{^2}\phi_{\delta\delta}}
    !>           \right)
    !> \f]
    IF (present(V_in)) THEN
      v = v_in
    ELSE
      v = vpt3n(P, T)
    ENDIF
    delta = normalize_density(1.0_SRK/V)
    ! Free Energy derivatives
    phi_d = phi3(delta, tau, 1, 0)
    phi_dt = phi3(delta, tau, 1, 1)
    phi_dd = phi3(delta, tau, 2, 0)
    phi_tt = phi3(delta, tau, 0, 2)

    cppt = R*(-phi_tt*tau*tau + ((delta*phi_d - delta*tau*phi_dt)**2)/(2*delta*phi_d + phi_dd*delta*delta)) ! [kj/kg-K]
  ELSEIF (iregInt == 5) THEN
    !> Region 1:Table 39
    !> \f[
    !> c_{p} = - R\tau^{2} \left(  \gamma^{o}_{\tau\tau} + \gamma^{r}_{\tau\tau} \right)
    !> \f]
    cppt = (-R*tau*tau)*(gamma5_r(pi, tau, 0, 2) + gamma5_o(pi, tau, 0, 2)) ! [kJ/kg-K]
  ELSE
    WRITE (msg, *) &
       "Pressure and Temperature are out of bounds "// &
       " Pressure [bar] Temperature [C] ", &
       P*1.0E1_SRK, T - 273.15_SRK
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF
ENDFUNCTION cppt
!
!-------------------------------------------------------------------------------
!> Calculation of Isochoric Specific heat \f$c_{v}\f$
!> Reference 1
!> Tables :   3,12,31,39
!> @param P Pressure in MPa
!> @param T Temperature in Kelvin
!> @param ireg flow regime ID
!> @return \f$c_{v}\f$ kJ/kg-K
!>
FUNCTION cvpt(P, T, ireg, V_in)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK), INTENT(IN), OPTIONAL :: V_in
  REAL(SRK) :: cvpt

  REAL(SRK) :: pi, tau, delta ! Dimensionless pressure, temperature, delta
  REAL(SRK) :: gamma_tau_tau, gamma_pi_pi, gamma_pi_tau, gamma_pi
  REAL(SRK) :: v
  INTEGER(SIK) :: iregInt
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  cvpt = 0.0_SRK

  IF (ireg == 0) THEN
    CALL regsopt(p, t, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  ! Normalize variables
  pi = normalize_pressure(P, iregInt)
  tau = normalize_temperature(T, iregInt)

  !> Reference 1: Thermohydrodynamic Relation
  !> \f[
  !> c_{v} = \left( \frac{\partial u}{\partial T} \right)_{v}
  !> \f]

  IF (iregInt == 1) THEN
    !> Region 1:Table 3
    !> \f[
    !> c_{v} = R \left(
    !>              - \tau^{2} \gamma_{\tau\tau}
    !>              - \frac{\left(\gamma_{\pi}-\tau\gamma_{\tau\pi}\right)^{2}}{\gamma_{\pi\pi}}
    !>           \right)
    !> \f]
    gamma_tau_tau = gamma1_p0_t2(pi, tau)
    gamma_pi_pi = gamma1_p2_t0(pi, tau)
    gamma_pi_tau = gamma1_p1_t1(pi, tau)
    gamma_pi = gamma1_p1_t0(pi, tau)
    cvpt = R*( &
           ((gamma_pi - tau*gamma_pi_tau)**2) &
           /(gamma_pi_pi) - (tau*tau)*gamma_tau_tau)  ! [kj/kg-K]
  ELSEIF (iregInt == 2) THEN
    !> Region 2:Table 12
    !> \f[
    !> c_{v} = R \left(
    !>              - \tau^{2} (\gamma^{o}_{\tau\tau}+\gamma^{r}_{\tau\tau})
    !>              - \frac{\left(1+\pi\gamma^{r}_{\pi}-\tau\pi\gamma^{r}_{\tau\pi}\right)^{2}}{1-\pi^{2}\gamma^{r}_{\pi\pi}}
    !>           \right)
    !> \f]
    gamma_tau_tau = gamma2_r_p0_t2(pi, tau) + gamma2_o(pi, tau, 0, 2)
    gamma_pi_pi = gamma2_r_p2_t0(pi, tau)
    gamma_pi_tau = gamma2_r_p1_t1(pi, tau)
    gamma_pi = gamma2_r_p1_t0(pi, tau)
    cvpt = R*( &
           -tau*tau*gamma_tau_tau &
           - ((1.0_SRK + pi*gamma_pi - pi*tau*gamma_pi_tau)**2) &
           /(1.0_SRK - pi*pi*gamma_pi_pi))
  ELSEIF (iregInt == 3) THEN
    !> Region 3: Table 31
    !> \f[
    !> c_{v} = R \left(
    !>              - \tau^{2} \phi_{\tau\tau}
    !>           \right)
    !> \f]
    IF (present(V_in)) THEN
      v = v_in
    ELSE
      v = vpt3n(P, T)
    ENDIF
    delta = normalize_density(1.0_SRK/V)
    cvpt = R*(-tau*tau*phi3(delta, tau, 0, 2))  ! [kJ/kg-K]
  ELSEIF (iregInt == 5) THEN
    !> Region 5:Table 39
    !> \f[
    !> c_{v} = R \left(
    !>              - \tau^{2} (\gamma^{o}_{\tau\tau}+\gamma^{r}_{\tau\tau})
    !>              - \frac{\left(1+\pi\gamma^{r}_{\pi}-\tau\pi\gamma^{r}_{\tau\pi}\right)^{2}}{1-\pi^{2}\gamma^{r}_{\pi\pi}}
    !>           \right)
    !> \f]
    gamma_tau_tau = gamma5_r(pi, tau, 0, 2) + gamma5_o(pi, tau, 0, 2)
    gamma_pi = gamma5_r(pi, tau, 1, 0)
    gamma_pi_pi = gamma5_r(pi, tau, 2, 0)
    gamma_pi_tau = gamma5_r(pi, tau, 1, 1)
    cvpt = R*(-tau*tau*gamma_tau_tau - (1 + pi*gamma_pi - tau*pi*gamma_pi_tau)/(1 - pi*pi*gamma_pi_pi)) ! [kJ/kg-K]
  ELSE
    WRITE (msg, *) &
       "Pressure and Temperature are out of bounds "// &
       " Pressure [bar] Temperature [C] ", &
       P*1.0E1_SRK, T - 273.15_SRK
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF
ENDFUNCTION cvpt
!
!===============================================================================
! Section: Transport Property Functions
!===============================================================================
!
!-------------------------------------------------------------------------------
!> Calculation of Surface Tension \f$c_{v}\f$
!> Reference 2
!> Equation : 1
!> @param T Temperature in Kelvin
!> @return \f$\sigma\f$ N/m
!>
FUNCTION surftt(T)
  REAL(SRK), INTENT(IN) :: T
  REAL(SRK) :: surftt

  REAL(SRK) :: tau
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  surftt = 0.0_SRK
  IF ((T >= 273.15_SRK) .AND. (T <= Tcrit)) THEN
    !> Equation 1:
    !> Valid for \f$273.15<T<T_{crit}\f$
    !> \f[
    !>    \sigma=B\tau^{\mu}(1+b\tau)
    !> \f]
    tau = (1.0_SRK - T/647.096_SRK)
    SURFTT = (235.8_SRK*(tau**1.256_SRK)*(1.0_SRK - 0.625E+0_SRK*tau))*1.0E-3_SRK
  ELSE
    WRITE (msg, *) &
       "Surface Tension Temperature is out of bounds "// &
       "Temperature: [K]", T
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF
ENDFUNCTION surftt
!
!-------------------------------------------------------------------------------
!> Calculation of Dynamic Viscosity \f$\mu\f$ in Pa-s.
!> @param V Specific volume in m^3/kg
!> @param T Temperature in Kelvin
!> @param [P]    Only required for range checking. Pressure in MPa
!> @param [ireg] Only required for range checking. flow regime ID
!> @return \f$\mu\f$ Pa-s
FUNCTION viscvt(V, T, P, ireg)
  REAL(SRK), INTENT(IN) :: V
  REAL(SRK), INTENT(IN) :: T
  REAL(SRK), OPTIONAL, INTENT(IN) :: P
  INTEGER(SIK), OPTIONAL, INTENT(IN) :: ireg
  REAL(SRK) :: viscvt

  REAL(SRK), PARAMETER, DIMENSION(4) ::  A = [1.67752_SRK, 2.20462_SRK, 0.6366564_SRK, -0.241605_SRK] ! Parameters for eq. 11 in table 1.
  REAL(SRK), PARAMETER, DIMENSION(21) :: & ! Parameters for eq. 12 in table 2.
    H = [5.20094E-1_SRK, 8.50895E-2_SRK, -1.08374E0_SRK, -2.89555E-1_SRK, &
         2.22531E-1_SRK, 9.99115E-1_SRK, 1.88797E0_SRK, 1.26613E0_SRK, 1.20573E-1_SRK, &
         -2.81378E-1_SRK, -9.06851E-1_SRK, -7.72479E-1_SRK, -4.89837E-1_SRK, -2.57040E-1_SRK, &
         1.61913E-1_SRK, 2.57399E-1_SRK, -3.25372E-2_SRK, 6.98452E-2_SRK, &
         8.72102E-3_SRK, -4.35673E-3_SRK, -5.93264E-4_SRK]
  INTEGER(SIK), PARAMETER, DIMENSION(21) :: & ! Exponential orders for eq. 12 in table 2.
    I = [0, 1, 2, 3, 0, 1, 2, 3, 5, 0, 1, 2, 3, 4, 0, 1, 0, 3, 4, 3, 5], &
    J = [0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 4, 4, 5, 6, 6]
  REAL(SRK) :: Tr, Tn, D, Dr, Dn
  REAL(SRK) :: u0, u1  ! u = u0*u1*u2 where u2=1 for ind. applications
  INTEGER(SIK) :: iregInt
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  D = 1.0_SRK/V

  IF (present(ireg) .AND. present(p)) THEN
    IF (ireg == 0) THEN
      CALL regsopt(P, T, iregInt)
    ELSE
      iregInt = ireg
    ENDIF

    IF (.NOT. ((iregInt /= 0 .AND. iregInt /= 9) .OR. (iregInt == 5 .AND. T < 1173.15_SRK))) THEN
      WRITE (msg, *) &
         'Thermal Conductivity: Pressure and Temperature are out of bounds '// &
         'P [MPa] = ', P, &
         'T [K] = ', T, &
         'IREG = ', iregInt
      CALL eWaterProp%raiseFatalError(trim(msg))
    ENDIF
  ENDIF

  ! Normalize Values
  Tr = T/Tcrit
  Dr = D/Dcrit

  !> Equation 11:
  !> \f[
  !>  \mu_{0}(\bar{T})=\frac{100\sqrt{\bar{T}}}{\sum\limits_{i=0}^3 \frac{H_{i}}{\bar{T}^{i}}}
  !> \f]
  ! Optimized Calculation
  u0 = 0.0_SRK &          ! ii
       + A(1)/(Tr**(0)) & !  1
       + A(2)/(Tr**(1)) & !  2
       + A(3)/(Tr**(2)) & !  3
       + A(4)/(Tr**(3))   !  4
  ! Non-optimized calculation
  !      DO ii = 1, SIZE(A,1)
  !        u0=u0+A(ii)/(Tr**(ii-1))
  !      ENDDO
  u0 = 100.0_SRK*sqrt(Tr)/u0

  !> Equation 12:
  !> \f[
  !>  \mu_{1}(\bar{T},\bar{\rho}) = exp \left[
  !>      \bar{\rho} \sum\limits_{i=0}^5 \left( \frac{1}{\bar{T}} - 1 \right)^{i}
  !>                 \sum\limits_{j=0}^6 H_{i,j} \left( \bar{\rho} - 1 \right)^{j}
  !>     \right]
  !> \f]
  ! Optimized Calculation
  Tn = (1.0_SRK/Tr - 1.0_SRK)
  Dn = (Dr - 1.0_SRK)
  u1 = 0.0_SRK &                       ! ii | I(ii) | J(ii)
       + H(1)*(Tn**(0))*(Dn**(0)) & !  1 |    0  |     0
       + H(2)*(Tn**(1))*(Dn**(0)) & !  2 |    1  |     0
       + H(3)*(Tn**(2))*(Dn**(0)) & !  3 |    2  |     0
       + H(4)*(Tn**(3))*(Dn**(0)) & !  4 |    3  |     0
       + H(5)*(Tn**(0))*(Dn**(1)) & !  5 |    0  |     1
       + H(6)*(Tn**(1))*(Dn**(1)) & !  6 |    1  |     1
       + H(7)*(Tn**(2))*(Dn**(1)) & !  7 |    2  |     1
       + H(8)*(Tn**(3))*(Dn**(1)) & !  8 |    3  |     1
       + H(9)*(Tn**(5))*(Dn**(1)) & !  9 |    5  |     1
       + H(10)*(Tn**(0))*(Dn**(2)) & ! 10 |    0  |     2
       + H(11)*(Tn**(1))*(Dn**(2)) & ! 11 |    1  |     2
       + H(12)*(Tn**(2))*(Dn**(2)) & ! 12 |    2  |     2
       + H(13)*(Tn**(3))*(Dn**(2)) & ! 13 |    3  |     2
       + H(14)*(Tn**(4))*(Dn**(2)) & ! 14 |    4  |     2
       + H(15)*(Tn**(0))*(Dn**(3)) & ! 15 |    0  |     3
       + H(16)*(Tn**(1))*(Dn**(3)) & ! 16 |    1  |     3
       + H(17)*(Tn**(0))*(Dn**(4)) & ! 17 |    0  |     4
       + H(18)*(Tn**(3))*(Dn**(4)) & ! 18 |    3  |     4
       + H(19)*(Tn**(4))*(Dn**(5)) & ! 19 |    4  |     5
       + H(20)*(Tn**(3))*(Dn**(6)) & ! 20 |    3  |     6
       + H(21)*(Tn**(5))*(Dn**(6))   ! 21 |    5  |     6
  ! Non-optimized calculation
  !      DO ii = 1, SIZE(H,1)
  !        Th=(1.0_SRK/Tr-1.0_SRK)**I(ii)
  !        Dh=(Dr-1.0_SRK)**J(ii)
  !        u1=u1+Th*Dh*H(ii)
  !      ENDDO
  u1 = EXP(Dr*u1)

  !> The critical heat enhancement factor \f$\mu_{2}=1\f$ for increased numerical
  !> computations as recommended for Industrial Application in section 3

  !> Equation 10:
  !> \f[
  !>  \mu=\mu_{0}(\bar{T}) \times \mu_{1}(\bar{T},\bar{\rho}) \times \mu_{2}(\bar{T},\bar{\rho})
  !> \f]
  viscvt = u0*u1*1E-6_SRK ! Convert from uPa-s to Pa-s
ENDFUNCTION viscvt
!
!-------------------------------------------------------------------------------
!> Calculation of thermal conductivity for all regions
!> Reference 4
!> @param P Pressure in bar
!> @param V Specific volume in m**3/kg
!> @param T Temperature in Kelvin
!> @param ireg flow regime ID
!> @return \f$\lambda\f$ W/m-K
!>
FUNCTION thconvt(P, V, T, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: V
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: thconvt

  CHARACTER(LEN=MAX_CHAR_LEN) :: msg
  REAL(SRK), PARAMETER, DIMENSION(5) :: &  ! Table 1. Coefficients for  eq. 11
    L0 = [2.443221E-3_SRK, 1.323095E-2_SRK, 6.770357E-3_SRK, -3.454586E-3_SRK, 4.096266E-4_SRK]

  REAL(SRK), PARAMETER, DIMENSION(5, 6) :: & ! Table 2. Coefficients for eq. 17
    L1 = RESHAPE([1.60397357_SRK, 2.33771842_SRK, 2.19650529_SRK, &
                  -1.21051378_SRK, -2.7203370_SRK, &
                  -0.646013523_SRK, -2.78843778_SRK, -4.54580785_SRK, &
                  1.60812989_SRK, 4.57586331_SRK, &
                  0.111443906_SRK, 1.53616167_SRK, 3.55777244_SRK, &
                  -0.621178141_SRK, -3.18369245_SRK, &
                  0.102997357_SRK, -0.463045512_SRK, -1.40944978_SRK, &
                  0.0716373224_SRK, 1.1168348_SRK, &
                  -0.0504123634_SRK, 0.0832827019_SRK, 0.275418278_SRK, &
                  0.0_SRK, -0.19268305_SRK, &
                  0.00609859258_SRK, -0.0071901245_SRK, -0.0205938816_SRK, &
                  0.0_SRK, 0.012913842_SRK], &
                 SHAPE(L1))

  REAL(SRK), PARAMETER, DIMENSION(5, 6) :: & ! Table 4. Coefficients for eq. 25
    A = RESHAPE( &
    [6.53786807199516_SRK, 6.52717759281799_SRK, 5.35500529896124_SRK, & ! i=1
     1.55225959906681_SRK, 1.11999926419994_SRK, &
     -5.61149954923348_SRK, -6.30816983387575_SRK, -3.96415689925446_SRK, & ! i=2
     0.464621290821181_SRK, 0.595748562571649_SRK, &
     3.39624167361325_SRK, 8.08379285492595_SRK, 8.91990208918795_SRK, & ! i=3
     8.93237374861479_SRK, 9.88952565078920_SRK, &
     -2.27492629730878_SRK, -9.82240510197603_SRK, -12.0338729505790_SRK, & ! i=4
     -11.0321960061126_SRK, -10.3255051147040_SRK, &
     10.2631854662709_SRK, 12.1358413791395_SRK, 9.19494865194302_SRK, & ! i=5
     6.16780999933360_SRK, 4.66861294457414_SRK, &
     1.97815050331519_SRK, -5.54349664571295_SRK, -2.16866274479712_SRK, & ! i=6
     -0.965458722086812_SRK, -0.503243546373828_SRK], SHAPE(A))

  ! Ranges for array A from Table 4 in Dimensionless Density
  REAL(SRK), PARAMETER, DIMENSION(5) :: B = [0.0_SRK, 0.310559006_SRK, 0.776397516_SRK, 1.242236025_SRK, 1.863354037_SRK]

  ! Parameters for k2
  REAL(SRK), PARAMETER :: &! Parameters from table 3
    lambda = 177.8514_SRK, qd = 1.0_SRK/(0.40_SRK), T_crit = 1.5_SRK, &
    Gamma_o = 0.06_SRK, u_g = 0.630_SRK/1.239_SRK, xi_o = 0.13_SRK, &
    pi = 4.0_SRK*ATAN(1.0_SRK) ! This is just pi(numerical const.)

  REAL(SRK), PARAMETER ::  u_ref = 1.0E-6_SRK ! Reference Viscosity [Pa-s]

  ! Variables used in calculations
  REAL(SRK) :: Tr, Th, Tn, D, Dr, Dh, Dn ! Normalized and Manipulated Temperature and Densities
  REAL(SRK) :: zeta1, zeta2, Chi, xi, y, z, mu, cp, cv, cp_cv ! Calculated suport variables
  REAL(SRK) :: k0, k1, k2 ! k=(k0*k1+k2) Base equation of calculation

  ! Index Counters
  INTEGER(SIK) :: i, j, jj, iregInt

  thconvt = 0.0_SRK
  cv = 0.0_SRK
  cp = 0.0_SRK
  D = 0.0_SRK

  IF (ireg == 0) THEN
    CALL regsopt(P, T, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  IF (iregInt /= 0) THEN
    D = 1.0_SRK/V
  ELSE
    WRITE (msg, *) &
       "Thermal Conducvity is out of bounds "// &
       "IREG must equal 1,2,3 or 5 "// &
       "Pressure: [MPa]", P, &
       "Temperature: [K]", T, &
       "IREG: ", iregInt
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF

  ! Normalize Values
  Tr = T/Tcrit
  Dr = D/Dcrit

  ! Initialize Variables
  zeta2 = 0.0_SRK
  k0 = 0.0_SRK
  k1 = 0.0_SRK
  k2 = 0.0_SRK
  thconvt = 0.0_SRK

  !> Equation 16
  !> \f[
  !>    \bar{\lambda}_{0}(\bar{T})=\frac{\sqrt{\bar{T}}}{\sum\limits_{k=0}^4 \frac{L_{k}}{\bar{T}^{k}}}
  !> \f]
  DO i = 1, SIZE(L0)
    k0 = k0 + L0(i)/(Tr**(i - 1))
  ENDDO
  k0 = sqrt(Tr)/k0

  !> Equation 17
  !> \f[
  !>  \bar{\lambda}_{1}(\bar{T},\bar{\rho}) = exp \left[
  !>      \bar{\rho} \sum\limits_{i=0}^4 \left( \frac{1}{\bar{T}} - 1 \right)^{i}
  !>                 \sum\limits_{j=0}^5 L_{i,j} \left( \bar{\rho} - 1 \right)^{j}
  !>     \right]
  !> \f]
  ! Optimized Calculation
  Dn = (Dr - 1.0_SRK)
  Tn = (1.0_SRK/Tr - 1.0_SRK)
  DO j = 1, SIZE(L1, 2)
    Dh = Dn**(j - 1)
    DO i = 1, SIZE(L1, 1)
      Th = Tn**(i - 1)
      k1 = k1 + Th*Dh*L1(i, j)
    ENDDO
  ENDDO
  ! Non-Optimized Code
  !       DO i=1, SIZE(L1,1)
  !         Th=(1.0_SRK/Tr-1.0_SRK)**(i-1)
  !         DO j=1, SIZE(L1,2)
  !           Dh=(Dr-1.0_SRK)**(j-1)
  !           k1=k1+Th*Dh*L1(i,j)
  !         ENDDO
  !       ENDDO
  k1 = EXP(Dr*k1)

  !> Calculate \f$\bar{\lambda}_{2}\f$ using equation 18 and the industrial formulation recommendations
  !> \f[
  !>  \bar{\lambda}_{2}(\bar{T},\bar{\rho})= \Lambda \frac{\bar{\rho}\bar{c_{p}}\bar{T}}{\bar{\mu}}Z(y)
  !> \f]

  ! Select the correct jj value for array A, from table 6
  jj = SIZE(B)
  DO j = 1, (SIZE(B, 1) - 1)
    IF ((Dr > B(j)) .AND. (Dr <= B(j + 1))) THEN
      jj = j
    ENDIF
  ENDDO

  !> Compute \f$\zeta_{2}\f$ equation 25
  DO i = 1, SIZE(A, 2)
    zeta2 = zeta2 + (A(jj, i)*Dr**(i - 1))
  ENDDO
  zeta2 = 1.0_SRK/zeta2

  !> Compute dimensionless isothermal compressibility \f$\zeta_{1}\f$ using eq. 24
  zeta1 = -D*D*Pcrit/Dcrit*(dvdppt(P, V, T, iregInt))  ! dvdp in m^3/(kg*MPa) ! Zeta is unitless
  IF (zeta1 < 0.0_SRK) zeta1 = 0.0_SRK
  IF (zeta1 > 1.0E13_SRK) zeta1 = 1.0E13_SRK

  !> Calculate \f$\Delta\chi\f$ from equation 23
  Chi = Dr*(zeta1 - zeta2*T_crit/Tr)
  IF (Chi < 0.0_SRK) Chi = 0.0_SRK

  !> Calculate \f$\xi\f$ from equation 22
  xi = xi_o*(Chi/Gamma_o)**u_g

  !> Calculate Specific Heats (if region 3, account for spec. vol. fraction)
  IF (iregInt == 1 .OR. iregInt == 2 .OR. iregInt == 5 .OR. iregInt == 3) THEN
    cp = cppt(P, T, iregInt, V)       ! Isobaric Specific Heat  [kJ/kg-K]
    cv = cvpt(P, T, iregInt, V)       ! Isochoric Specific Heat [kJ/kg-K]
  ELSE
    WRITE (msg, *) &
       "Thermal Conducvity is out of bounds "// &
       "IREG must equal 1,2,3 or 5 "// &
       "Pressure: [MPa]", P, &
       "Temperature: [K]", T, &
       "IREG: ", iregInt
       STOP
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF

  ! Supporting Variables / dimmensionless properties
  y = qd*xi     ! Dimensionless value for chord length [nm/nm]
  cp_cv = cp/cv ! Ratio of specific heats
  mu = viscvt(V, T, P, iregInt)   ! Dynamic Viscosity   [Pa-s]
  cp = cp/R      ! Reference to R
  mu = mu/u_ref  ! Reference to ref. visc.

  !> Compute \f$Z(y)\f$ according to equation 19
  IF (y < 1.2E-7_SRK) THEN
    z = 0.0_SRK
  ELSE
    z = 2.0_SRK/(pi*y)*(((1.0_SRK - 1.0_SRK/cp_cv)*ATAN(y) + y/cp_cv) - (1.0_SRK - EXP(-1.0_SRK/(1.0_SRK/y + y*y/(3.0_SRK*Dr*Dr)))))
  ENDIF
  IF (z < 0.0_SRK) THEN
    z = 0.0_SRK
  ENDIF

  ! Compute k2
  IF (iregInt /= 5) THEN
    k2 = lambda*Dr*cp*Tr/mu*z
  ELSE
    k2 = 0.0_SRK
  ENDIF

  !> Combine correlations to evaluate thermal conductivity using equation 15
  !> \f[
  !> \bar{\lambda}=\bar{\lambda}_{0} \times (\bar{T}) \bar{\lambda}_{1}(\bar{T},\bar{\rho})+\bar{\lambda}_{2}(\bar{T},\bar{\rho})
  !> \f]
  thconvt = (k0*k1 + k2)*1.0E-3_SRK ! Conversion factor from mW to W
ENDFUNCTION thconvt
!
!===============================================================================
! Section: Derivative Wrappers
!===============================================================================
!
!-------------------------------------------------------------------------------
!> Calculates derivative of Specific Volume with respect to Enthalpy
!> \f$\left(\frac{\partial v}{\partial h}\right)_{P}\f$
!> Always evaluated using a chain rule
!> @param P Pressure in MPa
!> @param T Temperature in K
!> @param ireg region number [1,2,3,5]
!> @return derivative m^3/kg/(kJ/kg)
!>
FUNCTION dvdh(P, T, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: dvdh

  REAL(SRK) :: dhdt_val, dvdt_val ! Analytical Derivatives to chain rule
  INTEGER(SIK) :: iregInt

  ! Determine the fluid region if necessary
  IF (ireg == 0) THEN
    CALL regsopt(P, T, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  !> Analytical Evaluation as a chain rule.
  !> \f[
  !> \left(\frac{\partial v}{\partial h}\right)_{P}=
  !> \frac{\left(\frac{\partial v}{\partial T}\right)}{ \left(\frac{\partial h}{\partial T}\right) }|_{P}
  !> \f]
  dhdt_val = dhdt(P, T, iregInt)
  dvdt_val = dvdt(P, T, iregInt)

  ! Partial Derivative assuming different constant properties
  !dvdh= ( dvdp/dhdp ) + ( dvdt/dhdt ) ! Total Partial (variable pressure and temperature)
  !dvdh= ( dvdp/dhdp )                 ! Constant Temperature
  dvdh = (dvdt_val/dhdt_val)  ! Constant Pressure
ENDFUNCTION dvdh
!
!-------------------------------------------------------------------------------
!> Calculates derivative of Enthalpy with respect to Temperature at constant Pressure
!> \f$\left(\frac{\partial h}{\partial T}\right)_{P}\f$
!> If iproperties = 2, then the derivative is evaluated as a second order accurate central finite difference of a first derivative
!> @param P Pressure in MPa
!> @param T Temperature in K
!> @param ireg region number [1,2,3,5]
!> @return derivative (kJ/kg)/K
!>
FUNCTION dhdt(P, T, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: dhdt

  REAL(SRK) :: H1, H2 ! Enthalpies 1 and 2 [kJ/kg]
  REAL(SRK), PARAMETER :: dT = 1.0E-6_SRK ! Pertubation value for taylor series [K]
  INTEGER(SIK) :: iregInt

  ! Determine the fluid region if necessary
  IF (ireg == 0) THEN
    CALL regsopt(P, T, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  IF (expr_opt == 1) THEN
    !> Analytical Evaluation
    !> \f[
    !> \left(\frac{\partial h}{\partial T}\right)_{P}=c_{p}(P,T)
    !> \f]
    dhdt = cppt(P, T, iregInt) ! [ kJ/(kg-MPa) ]
  ELSEIF (expr_opt == 2) THEN
    !> Second Order Accurate Taylor Series Approximation
    !> \f[
    !> \left(\frac{\partial h}{\partial T}\right)_{P} \approx
    !> \frac{h(P,T+\Delta T)-h(P,T-\Delta T)}{2\Delta T}
    !> \f]
    H1 = hpt(P, T + dT, iregInt) ! Point ahead  [ kJ/(kg-MPa) ]
    H2 = hpt(P, T - dT, iregInt) ! Point behind [ kJ/(kg-MPa) ]
    dhdt = (H1 - H2)/(2*dT) ! [ kJ/(kg-MPa) ]
  ELSE
    dhdt = 0.0_SRK
    REQUIRE(.FALSE.)
  ENDIF
ENDFUNCTION dhdt
!
!-------------------------------------------------------------------------------
!> Calculates derivative of Enthalpy with respect to Pressure at constant Temperature
!> \f$\left(\frac{\partial h}{\partial P}\right)_{T}\f$
!> If iproperties = 2, then the derivative is evaluated as a second order accurate central finite difference of a first derivative
!> @param P Pressure in MPa
!> @param T Temperature in K
!> @param ireg region number [1,2,3,5]
!> @return derivative (kJ/kg)/MPa
!>
FUNCTION dhdp(P, T, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: dhdp

  REAL(SRK) :: V, H1, H2 ! Specific Volume [m^3/kg] and Enthalpies 1 and 2 [kJ/kg]
  REAL(SRK), PARAMETER :: dP = 1.0E-6_SRK ! Pertubation value for taylor series [MPa]
  INTEGER(SIK) :: iregInt

  ! Determine the fluid region if necessary
  IF (ireg == 0) THEN
    CALL regsopt(P, T, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  IF (expr_opt == 1) THEN
     !> Analytical Evaluation
     !> \f[
     !> \left(\frac{\partial h}{\partial P}\right)_{T}(P,v,T)
     !> \f]
     V = vpt(P, T, iregInt)
     dhdp = dhdppt(P, V, T, iregInt) ! [ kJ/(kg-MPa) ]
  ELSEIF (expr_opt == 2) THEN
    !> Second Order Accurate Taylor Series Approximation
    !> \f[
    !> \left(\frac{\partial h}{\partial P}\right)_{T} \approx
    !> \frac{h(P+\Delta P,T)-h(P-\Delta P,T)}{2\Delta P}
    !> \f]
    H1 = hpt(P + dp, T, iregInt) ! Point ahead  [ kJ/(kg-MPa) ]
    H2 = hpt(P - dp, T, iregInt) ! Point behind [ kJ/(kg-MPa) ]
    dhdp = (H1 - H2)/(2*dP) ! [ kJ/(kg-MPa) ]
  ELSE
    dhdp = 0.0_SRK
    REQUIRE(.FALSE.)
  ENDIF
ENDFUNCTION dhdp
!
!-------------------------------------------------------------------------------
!> Calculates derivative of Specific Volume with respect to Pressure at constant Temperature
!> \f$\left(\frac{\partial v}{\partial P}\right)_{T}\f$
!> If iproperties = 2, then the derivative is evaluated as a second order accurate central finite difference of a first derivative
!> @param P Pressure in MPa
!> @param T Temperature in K
!> @param ireg region number [1,2,3,5]
!> @return derivative (m^3/kg)/MPa
!>
FUNCTION dvdp(P, T, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: dvdp

  REAL(SRK) :: V, V1, V2  ! Specific Volume [m^3/kg] at Current P,T and Taylor Points 1 and 2
  REAL(SRK), PARAMETER :: dP = 1.0E-6_SRK
  INTEGER(SIK) :: iregInt

  ! Determine the fluid region if necessary
  IF (ireg == 0) THEN
    CALL regsopt(P, T, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  IF (expr_opt == 1) THEN
    !> Analytical Evaluation
    !> \f[
    !> \left(\frac{\partial v}{\partial P}\right)_{T}(P,v,T)
    !> \f]
    V = vpt(P, T, iregInt) ! [m^3/kg]
    dvdp = dvdppt(P, V, T, iregInt) ! [m^3/(kg-MPa)]
  ELSEIF (expr_opt == 2) THEN
    !> Second Order Accurate Taylor Series Approximation
    !> \f[
    !> \left(\frac{\partial v}{\partial P}\right)_{T} \approx
    !> \frac{v(P+\Delta P,T)-v(P-\Delta P,T)}{2\Delta P}
    !> \f]
    V1 = vpt(P + dp, T, iregInt) ! Point Ahead [m^3/kg]
    V2 = vpt(P - dp, T, iregInt) ! Point Behind [m^3/kg]
    dvdp = (V1 - V2)/(2*dP) ! [m^3/(kg-MPa)]
  ELSE
    dvdp = 0.0_SRK
    REQUIRE(.FALSE.)
  ENDIF
ENDFUNCTION dvdp
!
!-------------------------------------------------------------------------------
!> Calculates derivative of Specific Volume with respect to Temperature at constant Pressure
!> \f$\left(\frac{\partial v}{\partial T}\right)_{P}\f$
!> If iproperties = 2, then the derivative is evaluated as a second order accurate central finite difference of a first derivative
!> @param P Pressure in MPa
!> @param T Temperature in K
!> @param ireg region number [1,2,3,5]
!> @return derivative (m^3/kg)/K
!>
FUNCTION dvdt(P, T, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: dvdt

  REAL(SRK) :: V, V1, V2
  REAL(SRK), PARAMETER :: dT = 1.0E-6_SRK
  INTEGER(SIK) :: iregInt

  ! Determine the fluid region if necessary
  IF (ireg == 0) THEN
    CALL regsopt(P, T, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  IF (expr_opt == 1) THEN
    !> Analytical Evaluation
    !> \f[
    !> \left(\frac{\partial v}{\partial T}\right)_{P}(P,v,T)
    !> \f]
     V = vpt(P, T, iregInt) ! [m^3/kg]
     dvdt = dvdtpt(P, V, T, iregInt) ! [m^3/(kg-K)]
  ELSEIF (expr_opt == 2) THEN
    !> Second Order Accurate Taylor Series Approximation
    !> \f[
    !> \left(\frac{\partial v}{\partial T}\right)_{P} \approx
    !> \frac{v(P,T+\Delta T)-v(P,T-\Delta T)}{2\Delta T}
    !> \f]
    V1 = vpt(P, T + dT, iregInt) ! [m^3/(kg-K)]
    V2 = vpt(P, T - dT, iregInt) ! [m^3/(kg-K)]
    dvdt = (V1 - V2)/(2*dT) ! [m^3/(kg-K)]
  ELSE
    dvdt = 0.0_SRK
    REQUIRE(.FALSE.)
  ENDIF
ENDFUNCTION dvdt
!
!===============================================================================
! Section: Property Derivatives
!===============================================================================
!
!-------------------------------------------------------------------------------
!> Analytical derivative of Specific Volume with respect to Pressure at constant Temperature
!> \f$\left(\frac{\partial v}{\partial T}\right)_{P}\f$
!> If iproperties = 2, then the derivative is evaluated as a second order accurate central finite difference of a first derivative
!> @param P Pressure in MPa
!> @param V Specific Volume in m^3/kg
!> @param T Temperature in K
!> @param ireg region number [1,2,3,5]
!> @return derivative (m^3/kg)/MPa
!>
FUNCTION dvdppt(P, V, T, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: V
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: dvdppt

  REAL(SRK) :: pi, tau, delta, rho, & ! Normalized Pressure, Temperature, and Density
          gamma_pi_pi, gamma_pi, & ! Derivative of Gibbs Free Energy w/r pi and tau
          phi_d, phi_dd   ! Derivatives of Hemlotz Free Energy
  INTEGER(SIK) :: iregInt
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  ! Initialize variables
  dvdppt = 0
  gamma_pi_pi = 0
  gamma_pi = 0
  ! Check Region
  IF (ireg == 0) THEN
    CALL regsopt(P, T, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  ! Calculate Density
  rho = 1.0_SRK/V ! kg/m^3

  ! Normalize Pressure, Temperature, and density
  pi = normalize_pressure(P, iregInt)
  tau = normalize_temperature(T, iregInt)
  delta = normalize_density(rho)

  IF (iregInt == 1) THEN
    !> derivative of sepcific volume with respect to \f$pi\f$ for region 1 Table 3
    !> \f[
    !> \left(\frac{\partial v}{\partial P}\right)_{T}=RT \left(\frac{\pi}{P}\right)^{2} \gamma_{\pi\pi}
    !> \f]
    dvdppt = gamma1_p2_t0(pi, tau)*R*T*(pi*pi/P/P)*(1.E-3) ![m^3/kg-MPa]
  ELSEIF (iregInt == 2) THEN
    ! Calculate the derivative of gibbs free energy with respect to
    ! pressure (pi) for both first and second order
    gamma_pi_pi = gamma2_r_p2_t0(pi, tau) + gamma2_o(pi, tau, 2, 0)
    !gamma_pi   =gamma2_r_p1_t0(pi,tau)+gamma2_o(pi,tau,1,0)

    !> derivative of sepcific volume with respect to \f$pi\f$ for region 1 Table 12
    !> \f[
    !> \left(\frac{\partial v}{\partial P}\right)_{T}=RT \left(\frac{\pi}{P}\right)^{2} \left(\gamma^o_{\pi\pi}+\gamma^r_{\pi\pi}\right)
    !> \f]
    dvdppt = gamma_pi_pi*R*T*(pi*pi/P/P)*(1.E-3) ![m^3/kg-MPa]
  ELSEIF (iregInt == 3) THEN
    ! Calculate the derivative of hemholtz free energy with respect to
    ! the second order of density
    phi_dd = phi3(delta, tau, 2, 0)
    phi_d = phi3(delta, tau, 1, 0)

    !> derivative of specific volume with respect to pressure is inverse of derivative of pressure with respect to specific volume for region 3
    !> \f[
    !> \left(\frac{\partial v}{\partial P}\right)_{T}=\frac{1}{\left(\frac{\partial P}{\partial v}\right)_{T}}
    !> \f]
    !> derivative of pressure with respect to specific volume in terms of f$\delta\f$ for region 3
    !> \f[
    !> \left(\frac{\partial P}{\partial v}\right)_{T}=-\rho \delta \left(\frac{\partial P}{\partial \delta}\right)_{T}
    !> \f]
    !> derivative of pressure with respect to \f$\delta\f$ for region 3 Table 31
    !> \f[
    !> \left(\frac{\partial P}{\partial \delta}\right)_{T}=\rho RT \left( 2 \phi_{\delta} + \delta \phi_{\delta\delta} \right)
    !> \f]
    ! R*T*rho^2/1000=[MPa/[m^3/kg]]
    dvdppt = -1.0E3_SRK/(rho*rho*delta*R*T*(2*phi_d + phi_dd*delta))![m^3/kg-MPa]
  ELSEIF (iregInt == 5) THEN
    ! Calculate the derivative of gibbs free energy with respect to
    ! pressure (pi) for both first and second order
    gamma_pi_pi = gamma5_r(pi, tau, 2, 0) + gamma5_o(pi, tau, 2, 0)
    !gamma_pi   =gamma5_r(pi,tau,1,0)+gamma5_o(pi,tau,1,0)

    !> derivative of sepcific volume with respect to \f$pi\f$ for region 5 Table 39
    !> \f[
    !> \left(\frac{\partial v}{\partial P}\right)_{T}=RT \left(\frac{\pi}{P}\right)^{2} \left(\gamma^o_{\pi\pi}+\gamma^r_{\pi\pi}\right)
    !> \f]
    dvdppt = gamma_pi_pi*R*T*((pi/P)*(pi/P))*(1.E-3) ![m^3/kg-MPa]

  ELSE
    ! Error out if saturated or out of bounds
    ! Not supposed to be saturated exactly (9), should be either
    ! liquid (1) or vapor (2) at saturated P and T
    WRITE (msg, *) "Derivative dv/dp is out of bounds", P, T, iregInt
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF
ENDFUNCTION dvdppt
!
!-------------------------------------------------------------------------------
!> Analytical derivative of Enthalpy with respect to Pressure at constant Temperature
!> \f$\left(\frac{\partial h}{\partial T}\right)_{P}\f$
!> If iproperties = 2, then the derivative is evaluated as a second order accurate central finite difference of a first derivative
!> @param P Pressure in MPa
!> @param V Specific Volume in m^3/kg
!> @param T Temperature in K
!> @param ireg region number [1,2,3,5]
!> @return derivative (kJ/kg)/MPa
!>
FUNCTION dhdppt(P, V, T, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: V
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: dhdppt

  CHARACTER(LEN=MAX_CHAR_LEN) :: msg
  REAL(SRK) :: pi, tau, delta, rho, & ! Normalized Pressure, Temperature, and Density
               gamma_pi_tau, &  ! Derivative of Gibbs Free Energy w/r pi and tau
               phi_d, phi_dt, phi_dd, & ! Derivatives of Hemlotz Free Energy
               dh_ddelta, dP_ddelta     ! Derivatives of properties w/r to delta
  INTEGER(SIK) :: iregInt

  ! Initialize variables
  dhdppt = 0
  gamma_pi_tau = 0

  ! Check Region
  IF (ireg == 0) THEN
    CALL regsopt(P, T, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  ! Calculate Density
  rho = 1.0_SRK/V ! kg/m^3

  ! Normalize Pressure, Temperature, and density
  pi = normalize_pressure(P, iregInt)
  tau = normalize_temperature(T, iregInt)
  delta = normalize_density(rho)

  IF (iregInt == 1) THEN
    ! Calculate the derivative of gibbs free energy with respect to
    ! pressure (pi) and temperature (tau)
    gamma_pi_tau = gamma1_p1_t1(pi, tau)

    !> derivative of enthalpy with respect to \f$pi\f$ for region 1 Table 3
    !> \f[
    !> \left(\frac{\partial h}{\partial P}\right)_{T}=RT \frac{\tau\pi}{P} \gamma_{\tau\pi}
    !> \f]
    dhdppt = gamma_pi_tau*R*T*tau*pi/P
  ELSEIF (iregInt == 2) THEN
    ! Calculate the derivative of gibbs free energy with respect to
    ! pressure (pi) and temperature (tau)
    ! Equation 17 and tables 16 and 14
    gamma_pi_tau = gamma2_r_p1_t1(pi, tau) + gamma2_o(pi, tau, 1, 1)

    !> derivative of enthalpy with respect to \f$pi\f$ for region 2 Table 12
    !> \f[
    !> \left(\frac{\partial h}{\partial P}\right)_{T}=RT \frac{\tau\pi}{P} \left(\gamma^o_{\tau\pi}+\gamma^r_{\tau\pi}\right)
    !> \f]
    dhdppt = gamma_pi_tau*R*T*tau*pi/P
  ELSEIF (iregInt == 3) THEN
    ! Calculate the derivative of hemholtz free energy with respect to
    ! density
    phi_d = phi3(delta, tau, 1, 0)

    ! Calculate the derivative of hemholtz free energy with respect to
    ! density and temperature
    phi_dt = phi3(delta, tau, 1, 1)

    ! Calculate the derivative of hemholtz free energy with respect to
    ! the second order of density
    phi_dd = phi3(delta, tau, 2, 0)

    ! Calculate dh/ddelta [kJ/kg]
    dh_ddelta = (R*T)*(tau*phi_dt + phi_d + delta*phi_dd)

    ! Calculate dP/ddelta [MPa]
    dP_ddelta = (R*rho*T)/1000.0_SRK*(2.0_SRK*phi_d + delta*phi_dd)

    !> For region 3, calculate the derivative of enthalpy with respect to
    !> pressure (pi) and temperature (tau)
    !> This equation is a chain rule in this region, since both
    !> pressure and enthalpy are a function of density and temperature.
    !> However dh/dP is at constant temperature, making dh/dtau=0
    !> so the second term drops out!
    !> \f[
    !>  \frac{\partial h}{\partial P}=\frac{\partial h}{\partial \delta} / \frac{\partial P}{\partial \delta}
    !> \f]

    dhdppt = (dh_ddelta)/(dP_ddelta)
  ELSEIF (iregInt == 5) THEN
    ! Calculate the derivative of gibbs free energy with respect to
    ! pressure (pi) and temperature (tau)
    ! Equation 32 and Table 41
    gamma_pi_tau = gamma5_r(pi, tau, 1, 1) + gamma5_o(pi, tau, 1, 1)

    !> derivative of enthalpy with respect to \f$pi\f$ for region 5 Table 39
    !> \f[
    !> \left(\frac{\partial h}{\partial P}\right)_{T}=RT \frac{\tau\pi}{P} \left(\gamma^o_{\tau\pi}+\gamma^r_{\tau\pi}\right)
    !> \f]
    dhdppt = gamma_pi_tau*R*T*tau/(P/pi)
  ELSE
    ! Error out if saturated or out of bounds
    ! Not supposed to be saturated exactly (9), should be either
    ! liquid (1) or vapor (2) at saturated P and T
    WRITE (msg, *) "Derivative dh/dp is out of bounds", P, T, iregInt
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF
ENDFUNCTION dhdppt
!
!-------------------------------------------------------------------------------
!> Analytical derivative of Specific Volume with respect to Temperature at constant Pressure
!> \f$\left(\frac{\partial v}{\partial T}\right)_{P}\f$
!> If iproperties = 2, then the derivative is evaluated as a second order accurate central finite difference of a first derivative
!> @param P Pressure in MPa
!> @param V Specific Volume in m^3/kg
!> @param T Temperature in K
!> @param ireg region number [1,2,3,5]
!> @return derivative (m^3/kg)/K
!>
FUNCTION dvdtpt(P, V, T, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: V
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: dvdtpt

  REAL(SRK) :: pi, tau, delta, rho, &  ! Normalized Pressure, Temperature, and Density
               gamma_pi_tau, gamma_pi, &  ! Derivative of Gibbs Free Energy w/r pi and tau
               phi_d, phi_dt, phi_dd, & ! Derivatives of Hemlotz Free Energy
               dP_dT, dP_dV     ! Derivatives of properties w/r to delta
  INTEGER(SIK) :: iregInt
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  ! Initialize variables
  dvdtpt = 0
  gamma_pi_tau = 0

  ! Check Region
  IF (ireg == 0) THEN
    CALL regsopt(P, T, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  ! Calculate Density
  rho = 1.0_SRK/V ! kg/m^3

  ! Normalize Pressure, Temperature, and density
  pi = normalize_pressure(P, iregInt)
  tau = normalize_temperature(T, iregInt)
  delta = normalize_density(rho)

  IF (iregInt == 1) THEN
    !> Derivative of specific volume equation from table 3 for region 1.
    !> \f[
    !> \left(\frac{\partial v}{\partial T}\right)_{P}=-\frac{\tau}{T}\left(\frac{\partial v}{\partial \tau}\right)_{P}
    !> \f]
    !> \f[
    !> \left(\frac{\partial v}{\partial \tau}\right)_{P}=R \frac{\pi}{P} \left( \gamma_{\pi} - \tau \gamma_{\pi\tau} \right)
    !> \f]

    dvdtpt = 1.E-3*R*pi/P*(gamma1_p1_t0(pi, tau) - tau*gamma1_p1_t1(pi, tau))
  ELSEIF (iregInt == 2) THEN
    !> Derivative of specific volume equation from table 12 for region 2.
    !> \f[
    !> \left(\frac{\partial v}{\partial T}\right)_{P}=-\frac{\tau}{T}\left(\frac{\partial v}{\partial \tau}\right)_{P}
    !> \f]
    !> \f[
    !> \left(\frac{\partial v}{\partial \tau}\right)_{P}=R \frac{\pi}{P} \left( \gamma^r_{\pi}+\gamma^o_{\pi} - \tau \left(\gamma^r_{\pi\tau}+\gamma^o_{\pi\tau}\right) \right)
    !> \f]

    gamma_pi = gamma2_o(pi, tau, 1, 0) + gamma2_r_p1_t0(pi, tau)
    gamma_pi_tau = gamma2_o(pi, tau, 1, 1) + gamma2_r_p1_t1(pi, tau)

    dvdtpt = 1.E-3*R*pi/P*(gamma_pi - tau*gamma_pi_tau)
  ELSEIF (iregInt == 3) THEN
    !> Derivative of equation from table 31 for region 3.
    !> \f[ \frac{\partial v}{\partial T} = \frac{\partial P}{\partial  T}    *\frac{     V}{\partial P} \f]
    !> \f[ \frac{\partial v}{\partial T} = \frac{\partial P}{\partial  T}    /\frac{     P}{\partial V} \f]
    !> \f[ \frac{\partial P}{\partial T} = \frac{\partial P}{\partial \tau}  *\frac{\tau  }{\partial T} \f]
    !> \f[ \frac{\partial P}{\partial V} = \frac{\partial P}{\partial \delta}*\frac{\delta}{\partial V} \f]

    ! Calculate the derivative of hemholtz free energy with respect to
    ! density
    phi_d = phi3(delta, tau, 1, 0)

    ! Calculate the derivative of hemholtz free energy with respect to
    ! density and temperature
    phi_dt = phi3(delta, tau, 1, 1)

    ! Calculate the derivative of hemholtz free energy with respect to
    ! the second order of density
    phi_dd = phi3(delta, tau, 2, 0)

    dP_dT = -1*rho*R*delta*(tau*phi_dt - phi_d) ! kPa/K
    dP_dV = -rho*rho*R*T*phi_dd  ! kPa/ [m^3/kg]

    dvdtpt = (dP_dT)/(dP_dV)
  ELSEIF (iregInt == 5) THEN
    !> Derivative of specific volume equation from table 39 for region 5.
    !> \f[
    !> \left(\frac{\partial v}{\partial T}\right)_{P}=-\frac{\tau}{T}\left(\frac{\partial v}{\partial \tau}\right)_{P}
    !> \f]
    !> \f[
    !> \left(\frac{\partial v}{\partial \tau}\right)_{P}=R \frac{\pi}{P} \left( \gamma^r_{\pi}+\gamma^o_{\pi} - \tau \left(\gamma^r_{\pi\tau}+\gamma^o_{\pi\tau}\right) \right)
    !> \f]
    gamma_pi = gamma5_o(pi, tau, 1, 0) + gamma5_r(pi, tau, 1, 0)
    gamma_pi_tau = gamma5_o(pi, tau, 1, 1) + gamma5_r(pi, tau, 1, 1)

    dvdtpt = 1.E-3*R*pi/P*(gamma_pi - tau*gamma_pi_tau)
  ELSE
    ! Error out if saturated or out of bounds
    ! Not supposed to be saturated exactly (9), should be either
    ! liquid (1) or vapor (2) at saturated P and T
    WRITE (msg, *) "Derivative dv/dt is out of bounds", P, T, iregInt
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF
ENDFUNCTION dvdtpt
!
!===============================================================================
! Section: Saturation Temperature and Pressure Functions
!===============================================================================
!
!-------------------------------------------------------------------------------
!> Calculation of Saturated Temperature
!> Reference 1 Section 8.2 equation 31
!> @param P Pressure in MPa
!> @return Tsat Saturated Temperature in K
!>
FUNCTION tsatpn(P)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK) :: tsatpn

  REAL(SRK), PARAMETER :: a1 = 1167.0521452767_SRK, &
                          a2 = -724213.16703206_SRK*(-1.0_SRK), &
                          a3 = -17.073846940092_SRK, &
                          a4 = 12020.824702470_SRK, &
                          a5 = -3232555.0322333_SRK*(-1.0_SRK), &
                          a6 = 14.915108613530_SRK, &
                          a7 = -4823.2657361591_SRK, &
                          a8 = 405113.40542057_SRK*(-1.0_SRK), &
                          a9 = -0.23855557567849_SRK, &
                          a0 = 650.17534844798_SRK*0.5_SRK
  REAL(SRK) :: aa, ab
  REAL(SRK) :: y, f, g, t1
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  aa = -4.0_SRK*a0
  ab = 4.0_SRK*a0**2 - a9

  IF ((P >= 5.E-4) .AND. (P <= 30.0_SRK)) THEN
    y = dsqrt(dsqrt(P))
    f = a7 + y*(a4 + y*a1)
    g = a8 + y*(a5 + y*a2)
    t1 = g/(f + dsqrt(f*f + ((a3 + y)*y + a6)*g*4.0_SRK)) + a0
    tsatpn = t1 - dsqrt((aa + t1)*t1 + ab)
  ELSE
    tsatpn = 0.0_SRK
    WRITE (msg, *) &
       "tsatpn(P) Pressure is out of bounds "// &
       "P [MPa]: 5.0E-4 < P < 30.0 "// &
       "P [MPa]: ", P
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF
ENDFUNCTION tsatpn
!
!-------------------------------------------------------------------------------
!> Calculation of Saturated Pressure
!> Reference 1 Section 8.1 equations 29-30
!> @param T Saturated Temperature in K
!> @return Psat Saturated Pressure in MPa
!>
FUNCTION psattn(ts)
  REAL(SRK), INTENT(IN) :: ts
  REAL(SRK) psattn

  REAL(SRK), PARAMETER :: &
        a1 = 1167.0521452767_SRK, &
        a2 = -724213.16703206_SRK, &
        a3 = -17.073846940092_SRK, &
        a4 = 12020.824702470_SRK, &
        a5 = -3232555.0322333_SRK, &
        a6 = 14.915108613530_SRK*2.0_SRK, &
        a7 = -4823.2657361591_SRK*2.0_SRK, &
        a8 = 405113.40542057_SRK*2.0_SRK, &
        a9 = -0.23855557567849_SRK, &
        a0 = 650.17534844798_SRK
  REAL(SRK) :: y, b, c, ps
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  psattn = 0.0_SRK
  IF ((ts >= 270.0_SRK) .AND. (ts <= 650.0_SRK)) THEN
    y = ts + a9/(ts - a0)
    b = a5 + y*(a4 + y*a3)
    c = a8 + y*(a7 + y*a6)
    ps = c/(dsqrt(b**2 - ((a1 + y)*y + a2)*c*2.0_SRK) - b)
    ps = ps*ps
    psattn = ps*ps
  ELSE
    WRITE (msg, *) &
       "psattn(ts) "// &
       "Temperature is out of bounds "// &
       " 270.0 < ts < 650.0 [K] "// &
       "Temperature [K] ", ts
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF
ENDFUNCTION psattn
!
!-------------------------------------------------------------------------------
!> Calculation for the temperature at the boundary between regions 2 and 3
!> Reference 1
!> @param P Pressure in MPa
!> @return T23 Temperature in K
!>
FUNCTION fb23p(P)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK) :: fb23p

  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  fb23p = 0.0_SRK
  IF (ABS(p - 0.13918839778870E+02_SRK) > 1.0E-12_SRK) THEN
    fb23p = 0.57254459862746E+03_SRK + dsqrt((p - 0.13918839778870E+02_SRK)*0.98106832075625E+03_SRK)
  ELSE
    WRITE (msg, *) &
       "fb23p(p): Pressure is out of bounds "// &
       " p < 13.91884 MPa "// &
       "Pressure [MPa] ", p
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF
ENDFUNCTION fb23p
!
!-------------------------------------------------------------------------------
!> Calculation for the pressure at the boundary between regions 2 and 3
!> Reference 1
!> @param  T Temperature in K
!> @return P23 Pressure in MPa
!>
FUNCTION FB23(T)
  REAL(SRK), INTENT(IN) :: T
  REAL(SRK) :: FB23

  FB23 = (0.10192970039326E-02_SRK*T - 0.11671859879975E+01_SRK)*T + 0.34805185628969E+03_SRK
ENDFUNCTION FB23
!
!===============================================================================
! Section: Backwards Equations
!===============================================================================
!
!-------------------------------------------------------------------------------
!> Backward calculations for the temperature given pressure and enthalpy
!> Reference 1
!> @param P Pressure in MPa
!> @param h Enthalpy in kJ/kg
!> @return T Temperature in K
!>
RECURSIVE FUNCTION tph(P, h, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: h
  INTEGER(SIK), INTENT(IN) :: ireg
  REAL(SRK) :: tph

  INTEGER(SIK) :: iregInt
  REAL(SRK) :: pbar, tsat
  CHARACTER(LEN=MAX_CHAR_LEN) :: msg

  tph = 0.0_SRK

  IF (ireg == 0) THEN
    CALL regsph(p, h, iregInt)
  ELSE
    iregInt = ireg
  ENDIF

  IF (iregInt == 1) THEN
    tph = tph1n(p, h)
    IF ((tph >= 273.125_SRK) .AND. (tph < 273.15_SRK)) THEN
      tph = 273.15_SRK
    ENDIF
  ELSEIF (iregInt == 2) THEN
    tph = tph2n(p, h)
    IF ((tph > 1073.15_SRK) .AND. (tph <= 1073.175_SRK)) THEN
      tph = 1073.15_SRK
    ENDIF
  ELSEIF (iregInt == 3) THEN
    tph = tph3n(p, h)
  ELSEIF (iregInt == 5) THEN
    tph = tph5n(p, h)
  ELSEIF (iregInt == 9) THEN
    tph = tsatpn(p)
  ELSE
    pbar = p*1.0E1_SRK
    WRITE (msg, *) &
       "Function: tph(p,h,iregInt) "// &
       "iregInt is not 1,2,3,5, or 9 "// &
       "IREG: ", iregInt, &
       "Pressure [MPa]: ", p, &
       "Enthalpy [kJ/kg]: ", h
    CALL eWaterProp%raiseFatalError(trim(msg))
  ENDIF

  IF ((iregInt == 1) .AND. (p <= 16.529164253_SRK)) THEN
    tsat = tsatpn(p)
    IF (tph > tsat) tph = tsat - 1.E-5
  ENDIF

  IF ((iregInt == 2) .AND. (p <= 16.529164253_SRK)) THEN
    tsat = tsatpn(p)
    IF (tph < tsat) tph = tsat + 1.E-5
  ENDIF
ENDFUNCTION tph
!
!-------------------------------------------------------------------------------
!> Backward calculations for the temperature given pressure and enthalpy for
!> region 1
!> Reference 1
!> @param P Pressure in MPa
!> @param h Enthalpy in kJ/kg
!> @return T Temperature in K
!>
FUNCTION tph1n(P, h)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: h
  REAL(SRK) :: tph1n

  INTEGER(SIK), PARAMETER, DIMENSION(20) :: &
    I = [0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6], &
    J = [0, 1, 2, 6, 22, 32, 0, 1, 2, 3, 4, 10, 32, 10, 32, 10, 32, 32, 32, 32]
  REAL(SRK), PARAMETER, DIMENSION(20) :: &
    n = [-.23872489924521E+03_SRK, &
         .40421188637945E+03_SRK, &
         .11349746881718E+03_SRK, &
         -.58457616048039E+01_SRK, &
         -.15285482413140E-03_SRK, &
         -.10866707695377E-05_SRK, &
         -.13391744872602E+02_SRK, &
         .43211039183559E+02_SRK, &
         -.54010067170506E+02_SRK, &
         .30535892203916E+02_SRK, &
         -.65964749423638E+01_SRK, &
         .93965400878363E-02_SRK, &
         .11573647505340E-06_SRK, &
         -.25858641282073E-04_SRK, &
         -.40644363084799E-08_SRK, &
         .66456186191635E-07_SRK, &
         .80670734103027E-10_SRK, &
         -.93477771213947E-12_SRK, &
         .58265442020601E-14_SRK, &
         -.15020185953503E-16_SRK]
  REAL(SRK) :: pi, eta
  INTEGER(SIK) :: k

  ! Normalize Arguments
  pi = p/1.0_SRK
  eta = h/2500.0_SRK
  tph1n = 0.0_SRK

  ! Equation 11
  DO k = 1, SIZE(n)
    tph1n = tph1n + n(k)*pi**(I(k))*(eta + 1)**(J(k))
  ENDDO
  tph1n = tph1n*1.0_SRK ! Un-normalize tph1n (T*=1K)
ENDFUNCTION tph1n
!
!-------------------------------------------------------------------------------
!> Backward calculations for the temperature given pressure and enthalpy for
!> region 2
!> Reference 1
!> @param P Pressure in MPa
!> @param h Enthalpy in kJ/kg
!> @return T Temperature in K
!>
FUNCTION tph2n(P, h)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: h
  REAL(SRK) ::tph2n

  ! B2bc-equation, table 19
  ! The last two values aren't used. They are for the calculating h as a function of p, which isn't done here
  REAL(SRK), PARAMETER, DIMENSION(5) :: &
    N = [0.90584278514723E03_SRK, &
         -0.67955786399241E00_SRK, &
         0.12809002730136E-3_SRK, &
         0.26526571908428E04_SRK, &
         0.45257578905948E01_SRK]
  ! Region 2a, table 20
  INTEGER(SIK), PARAMETER, DIMENSION(34) :: &
    Ia = [0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, &
          2, 2, 2, 2, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 7], &
    Ja = [0, 1, 2, 3, 7, 20, 0, 1, 2, 3, 7, 9, 11, 18, 44, 0, 2, &
          7, 36, 38, 40, 42, 44, 24, 44, 12, 32, 44, 32, 36, 42, 34, 44, 28]
  REAL(SRK), PARAMETER, DIMENSION(34) :: &
    Na = [1089.8952318288E0_SRK, &
          849.51654495535E0_SRK, &
          -107.81748091826E0_SRK, &
          33.153654801263E0_SRK, &
          -7.4232016790248E0_SRK, &
          11.765048724356E0_SRK, &
          1.8445749355790E0_SRK, &
          -4.1792700549624E0_SRK, &
          6.2478196935812E0_SRK, &
          -17.344563108114E0_SRK, &
          -200.58176862096E0_SRK, &
          271.96065473796E0_SRK, &
          -455.11318285818E0_SRK, &
          3091.9688604755E0_SRK, &
          252266.40357872E0_SRK, &
          -0.61707422868339E-02_SRK, &
          -0.31078046629583E0_SRK, &
          11.670873077107E0_SRK, &
          128127984.04046E0_SRK, &
          -985549096.23276E0_SRK, &
          2822454697.3002E0_SRK, &
          -3594897141.0703E0_SRK, &
          1722734991.3197E0_SRK, &
          -13551.334240775E0_SRK, &
          12848734.664650E0_SRK, &
          1.3865724283226E0_SRK, &
          235988.32556514E0_SRK, &
          -13105236.545054E0_SRK, &
          7399.9835474766E0_SRK, &
          -551966.97030060E0_SRK, &
          3715408.5996233E0_SRK, &
          19127.729239660E0_SRK, &
          -415351.64835634E0_SRK, &
          -62.459855192507E0_SRK]
  ! Region 2b, table 21
  INTEGER(SIK), PARAMETER, DIMENSION(38) :: &
    Ib = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, &
          2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 6, 7, 7, 9, 9], &
    Jb = [0, 1, 2, 12, 18, 24, 28, 40, 0, 2, 6, 12, 18, 24, 28, 40, 2, 8, 18, &
          40, 1, 2, 12, 24, 2, 12, 18, 24, 28, 40, 18, 24, 40, 28, 2, 28, 1, 40]
  REAL(SRK), PARAMETER, DIMENSION(38)  :: &
    Nb = [1489.5041079516E0_SRK, &
          743.07798314034E0_SRK, &
          -97.708318797837E0_SRK, &
          2.4742464705674E0_SRK, &
          -0.63281320016026E0_SRK, &
          1.1385952129658E0_SRK, &
          -0.47811863648625E0_SRK, &
          0.85208123431544E-02_SRK, &
          0.93747147377932E0_SRK, &
          3.3593118604916E0_SRK, &
          3.3809355601454E0_SRK, &
          0.16844539671904E0_SRK, &
          0.73875745236695E0_SRK, &
          -0.47128737436186E0_SRK, &
          0.15020273139707E0_SRK, &
          -0.21764114219750E-02_SRK, &
          -0.21810755324761E-01_SRK, &
          -0.10829784403677E0_SRK, &
          -0.46333324635812E-01_SRK, &
          0.71280351959551E-04_SRK, &
          0.11032831789999E-03_SRK, &
          0.18955248387902E-03_SRK, &
          0.30891541160537E-02_SRK, &
          0.13555504554949E-02_SRK, &
          0.28640237477456E-06_SRK, &
          -0.10779857357512E-04_SRK, &
          -0.76462712454814E-04_SRK, &
          0.14052392818316E-04_SRK, &
          -0.31083814331434E-04_SRK, &
          -0.10302738212103E-05_SRK, &
          0.28217281635040E-06_SRK, &
          0.12704902271945E-05_SRK, &
          0.73803353468292E-07_SRK, &
          -0.11030139238909E-07_SRK, &
          -0.81456365207833E-13_SRK, &
          -0.25180545682962E-10_SRK, &
          -0.17565233969407E-17_SRK, &
          0.86934156344163E-14_SRK]
  ! Region 2c, table 22
  INTEGER(SIK), PARAMETER, DIMENSION(23) :: &
    Ic = [-7, -7, -6, -6, -5, -5, -2, -2, -1, -1, 0, 0, 1, 1, 2, 6, 6, 6, 6, 6, 6, 6, 6], &
    Jc = [0, 4, 0, 2, 0, 2, 0, 1, 0, 2, 0, 1, 4, 8, 4, 0, 1, 4, 10, 12, 16, 20, 22]
  REAL(SRK), PARAMETER, DIMENSION(23) :: &
    Nc = [-0.32368398555242E13_SRK, &
          0.73263350902181E13_SRK, &
          0.35825089945447E12_SRK, &
          -0.58340131851590E12_SRK, &
          -0.10783068217470E11_SRK, &
          0.20825544563171E11_SRK, &
          0.61074783564516E06_SRK, &
          0.85977722535580E06_SRK, &
          -0.25745723604170E05_SRK, &
          0.31081088422714E05_SRK, &
          0.12082315865936E04_SRK, &
          0.48219755109255E03_SRK, &
          0.37966001272486E01_SRK, &
          -0.10842984880077E02_SRK, &
          -0.45364172676660E-01_SRK, &
          0.14559115658698E-12_SRK, &
          0.11261597407230E-11_SRK, &
          -0.17804982240686E-10_SRK, &
          0.12324579690832E-06_SRK, &
          -0.11606921130984E-05_SRK, &
          0.27846367088554E-04_SRK, &
          -0.59270038474176E-03_SRK, &
          0.12918582991878E-02_SRK]

  REAL(SRK) :: p585
  REAL(SRK) :: pi, eta
  INTEGER(SIK) :: k
  ! Normalize P, and H
  pi = p/1.0_SRK
  eta = h/2000.0_SRK
  IF (p <= 4.0_SRK) THEN ! Region 2a
    ! Equation 22 [1], is normalized to 1 [K]. Omitted conversion
    tph2n = 0.0_SRK
    DO k = 1, SIZE(Na)
      tph2n = tph2n + Na(k)*pi**(Ia(k))*(eta - 2.1_SRK)**(Ja(k))
    ENDDO
  ELSE ! 20
    ! B2bc-equation, equation 20 [1], is normalized to 1 MPa and kJ/kg. Omitted conversion
    ! Approximates line where entropy = 5.85 [kJ/kg-K]
    p585 = (N(1) + N(2)*h + n(3)*h*h)

    IF (p <= p585) THEN ! Region 2b
      ! Equation  23 [1], is normalized to 1 [K]. Omitted conversion
      tph2n = 0.0_SRK
      DO k = 1, SIZE(Nb)
        tph2n = tph2n + Nb(k)*(pi - 2.0_SRK)**(Ib(k))*(eta - 2.6_SRK)**(Jb(k))
      ENDDO
    ELSE ! Region 2c
      ! Equation 24 [1], is normalized to 1 [K]. Omitted conversion
      tph2n = 0.0_SRK
      DO k = 1, SIZE(Nc)
        tph2n = tph2n + Nc(k)*(pi + 25.0_SRK)**(Ic(k))*(eta - 1.8_SRK)**(Jc(k))
      ENDDO
    ENDIF
  ENDIF
ENDFUNCTION tph2n
!
!-------------------------------------------------------------------------------
!> Backward calculations for the temperature given pressure and enthalpy for
!> region 5
!> Reference 1
!> @param P Pressure in MPa
!> @param h Enthalpy in kJ/kg
!> @return T Temperature in K
!>
FUNCTION tph5n(P, h)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: h
  REAL(SRK) ::tph5n

  REAL(SRK), PARAMETER :: eps = 1.E-6
  REAL(SRK) :: t1, t2, x
  INTEGER(SIK) :: ix

  IF (h < 5000.0_SRK) THEN
    t1 = 1073.15_SRK
    t2 = 1573.15_SRK
  ELSEIF (h < 6500.0_SRK) THEN
    t1 = 1323.15_SRK
    t2 = 2073.15_SRK
  ELSE
    t1 = 1823.15_SRK
    t2 = 2273.15_SRK
  ENDIF

  CALL wnph3(t1, t2, 5, h, p, eps, x, ix)
  tph5n = x
ENDFUNCTION tph5n
!
!===============================================================================
! Section: Fluid Region Functions
!===============================================================================
!
!-------------------------------------------------------------------------------
!> Calculation of fluid region number based off of pressure and temperature
!> Reference 1.
!>  -# Subcooled Liquid
!>  -# Superheated Vapor
!>  -# Near Critical Point
!>  -# Saturated Fluid
!>  -# Supercritical Fluid
!> @param P Pressure in MPa
!> @param T Temperature in K
!> @param ireg Fluid Region ID number
!>
SUBROUTINE regsopt(P, T, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(INOUT) :: ireg

  REAL(SRK), PARAMETER :: &
     rpsr = 5.E-4_SRK, &! relative tolerance value.  should not be lower than 2.E-4
     epsr = 1.E-8  ! absolute tolerance value.  should not be lower than 1.E-8
  REAL(SRK) :: tg, pg
  ! saved varialbes
  REAL(SRK), SAVE :: told = 0.0_SRK, pold = 0.0_SRK
  INTEGER(SIK), SAVE :: iregold = 0

  IF (ireg /= 0 .AND. ireg /= 1 .AND. ireg /= 2 .AND. ireg /= 3 .AND. ireg /= 5 .AND. ireg /= 9) ireg = 0

  IF ((ABS(t - told) < tolerance) .AND. (ABS(p - pold) < tolerance)) THEN
    ireg = iregold
    RETURN
  ENDIF

  ! (czd)
  ! change cut off for region 5 to 900 c to account for limit of transport properties
  IF ((t < 273.15_SRK) .OR. (t > 1173.15_SRK) .OR. (p > 100.0_SRK) .OR. (p < 5.0E-4_SRK)) THEN
    ireg = 0
    RETURN
  ENDIF

  ireg = 0

  ! include a REAL(SRKative tolerance value (czd)
  ! this was included to better estimate if the t,p where in
  ! a saturated region (ireg=9). failure to set ireg=9 causes
  ! and error to occur in taf97. in addition, the absolute
  ! tolerance was changed from 1e-9 to 1e-7.
  IF (t <= 623.15_SRK) THEN
    pg = psattn(t)
    IF (p > (pg + epsr) .AND. rpsr < (p - pg)/pg) THEN
      ireg = 1
    ELSEif (p < (pg - epsr) .AND. rpsr < (pg - p)/pg) THEN
      ireg = 2
    ELSE
      ireg = 9
    ENDIF
  ELSEIF (t <= 863.15_SRK) THEN
    pg = fb23(t)
    IF ((p > pg) .AND. (p <= 22.064_SRK)) THEN
      tg = tsatpn(p)
      IF ((epsr < ABS(tg - t)) .AND. (rpsr < ABS((pg - p)/pg))) THEN
        ireg = 3
      ELSE
        ireg = 9
      ENDIF
    ELSEif (p > pg) THEN
      ireg = 3
    ELSE
      ireg = 2
    ENDIF
  ELSEIF (t <= 1073.15_SRK) THEN
    ireg = 2
  ELSE
    IF (p <= 10.0_SRK) THEN
      ireg = 5
    ELSE
      ireg = 0
    ENDIF
  ENDIF

  told = t
  pold = p
  iregold = ireg
ENDSUBROUTINE regsopt
!
!-------------------------------------------------------------------------------
!> Calculation of fluid region number based off of pressure and enthalpy
!> Reference 1.
!>  -# Subcooled Liquid
!>  -# Superheated Vapor
!>  -# Near Critical Point
!>  -# Saturated Fluid
!>  -# Supercritical Fluid
!> @param P Pressure in MPa
!> @param h Enthalpy in kJ/kg
!> @param ireg Fluid Region ID number
!>
SUBROUTINE regsph(P, h, ireg)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: h
  INTEGER(SIK), INTENT(INOUT) :: ireg

  REAL(SRK), PARAMETER :: pgr = 16.52916425260452_SRK, tgr13 = 623.15E+0_SRK, eps = 1.0E-10_SRK
  REAL(SRK) :: dl, dv, h1, h2, h1g, h2g, tg, ts, &
               hg01, hg13, hg2, hg20, hg23, tb, tn, tout, v1, v2
  ! saved variables
  REAL(SRK), SAVE :: hold = 0.0_SRK, pold = 0.0_SRK
  INTEGER(SIK), SAVE :: iregold = 0

  IF ((ABS(h - hold) < eps) .AND. (ABS(p - pold) < eps)) THEN
    ireg = iregold
    RETURN
  ENDIF

  IF ((p > 100.0_SRK) .OR. (p < 5.0E-4_SRK) .OR. (h < -0.05_SRK) .OR. (h > 7.4E3_SRK)) THEN
     ireg = 0
     RETURN
  ENDIF

  ireg = 0

  IF (p <= pgr) THEN
    tg = tsatpn(p)
    h1g = hpt(p, tg, 1)
    IF (h <= h1g) THEN
      tn = tph(p, h, 1)
      IF ((tn >= 273.125_SRK) .AND. (tn < 273.15_SRK)) THEN
        tn = 273.15_SRK
      ENDIF
      IF (tn >= 273.15_SRK) THEN
        ireg = 1
      ELSE
        ireg = 0
      ENDIF
    ELSE
      h2g = hpt(p, tg, 2)
      IF (h < h2g) THEN
        ireg = 9
      ELSEIF (h <= 4080.0_SRK) THEN
        ireg = 2
      ELSE
        hg2 = hpt(p, 1073.1501_SRK, 2)
        IF (h <= hg2) THEN
          ireg = 2
        ELSEIF (p > 10.0_SRK) THEN
          ireg = 0
        ELSEIF (h < 7374.751711911952_SRK) THEN
          ireg = 5
        ELSEIF (h < 7376.980306192378_SRK) THEN
          tb = tph(p, h, 5)
          ! limit region 5 to 900 c to match limits of transport properties
          IF (tb <= 1173.15_SRK) THEN ! 2273.15) THEN
            ireg = 5
          ELSE
            ireg = 0
          ENDIF
        ELSE
          ireg = 0
        ENDIF
      ENDIF
    ENDIF
  ELSEIF (h >= 2563.592003888252_SRK) THEN
    IF (h > 4090.0_SRK) THEN
      ireg = 0
    ELSE
      tg = fb23p(p)
      hg23 = hpt(p, tg, 2)
      IF (h < hg23) THEN
        ireg = 3
      ELSE
        hg20 = hpt(p, 1073.175_SRK, 2)
        IF (h <= hg20) THEN
          ireg = 2
        ELSE
          ireg = 0
        ENDIF
      ENDIF
    ENDIF
  ELSE
    hg13 = hpt(p, 623.15_SRK, 1)
    IF (h > hg13) THEN
      ireg = 3
    ELSE
      hg01 = hpt(p, 273.125_SRK, 1)
      IF (h >= hg01) THEN
        ireg = 1
      ELSE
        ireg = 0
      ENDIF
    ENDIF
  ENDIF
  IF ((ireg == 3) .AND. (p > pgr) .AND. (p < pcrit)) THEN
    ts = tsatpn(p)
    CALL fsatp(dv, dl, tout, ts, p)
    v1 = 1.0_SRK/dl
    v2 = 1.0_SRK/dv
    h1 = hvt3n(v1, ts)
    h2 = hvt3n(v2, ts)
    IF ((h > h1) .AND. (h < h2)) THEN
      ireg = 9
    ENDIF
  ENDIF

  hold = h
  pold = p
  iregold = ireg
ENDSUBROUTINE regsph
!
!===============================================================================
! Section: Region 3 Iterative Functions
!===============================================================================
!
!-------------------------------------------------------------------------------
!TODO needs header
FUNCTION vpt3n(P, T)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  REAL(SRK) :: vpt3n

  REAL(SRK), SAVE :: P_old = 0.0_SRK, T_old = 0.0_SRK, vpt3n_old = 0.0_SRK
  REAL(SRK), PARAMETER :: EPS = 1.0E-10_SRK
  REAL(SRK) :: DEST
  INTEGER(SIK) :: ireg3

  IF ((ABS(P - P_old) < EPS) .AND. (ABS(T - vpt3n_old) < EPS)) THEN
    vpt3n = vpt3n_old
    RETURN
  ENDIF

  CALL REG3S(P, T, IREG3)
  DEST = 1.0_SRK/VEST3(P, T, IREG3)
  VPT3N = 1.0_SRK/DITER3(P, T, DEST, EPS)

  P_old = P
  T_old = T
  vpt3n_old = VPT3N
ENDFUNCTION vpt3n
!
!-------------------------------------------------------------------------------
!TODO needs header
SUBROUTINE reg3s(P, T, ireg3)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(OUT) :: ireg3

  REAL(SRK) :: ts, pg

  IF (p <= 40.0_SRK) THEN
    IF (p <= pcrit) THEN
      ts = tsatpn(p)
      IF (t < ts) THEN
        ireg3 = 1
      ELSE
        ireg3 = 4
      ENDIF
    ELSE
      pg = pcrit + (t - tcrit)/3.727888_SRK
      IF (p <= 24.0_SRK) THEN
        IF (t <= tcrit) THEN
          ireg3 = 1
        ELSEIF (p > pg) THEN
          ireg3 = 2
        ELSE
          ireg3 = 3
        ENDIF
      ELSEIF (p > pg) THEN
        ireg3 = 5
      ELSE
        ireg3 = 6
      ENDIF
    ENDIF
  ELSE
    ireg3 = 7
  ENDIF
ENDSUBROUTINE reg3s
!
!-------------------------------------------------------------------------------
!TODO needs header
FUNCTION vest3(P, T, ireg3)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  INTEGER(SIK), INTENT(INOUT) :: ireg3
  REAL(SRK) :: vest3

  REAL(SRK) :: vpt3n
  REAL(SRK) :: c34, p2, p2t2, p4, p5
  REAL(SRK) :: pr, t2, t3, t4, t6, tr, z1, z2, z3, z4, z8
  REAL(SRK), PARAMETER :: vc = 1.0_SRK/322.0_SRK
  REAL(SRK), PARAMETER :: dtdp = 3.727888_SRK
  REAL(SRK), PARAMETER :: &
              a01 = 0.748874448725080E-02_SRK, &! parameters for region 3a
              a02 = 0.348351238844209E-03_SRK, &
              a03 = -0.118427102787648E-03_SRK, &
              a04 = -0.332156380721166E-01_SRK, &
              a05 = -0.797144480049465E-03_SRK, &
              a06 = 0.608359392259313E-01_SRK, &
              a07 = 0.617493752916276E-03_SRK, &
              a08 = -0.564778221803544E-01_SRK, &
              a09 = -0.161628813019544E-03_SRK, &
              a10 = -0.876842774061156E-02_SRK, &
              a11 = 0.263271308078056E-01_SRK, &
              a12 = 0.172887471616688E-01_SRK, &
              a13 = -0.490643103428227E-02_SRK, &
              a14 = -0.106812857513379E-01_SRK, &
              a15 = 0.153431224177324E-02_SRK, &
              a16 = -0.383218936572771E-08_SRK, &
              a17 = -0.455907876060086E-04_SRK, &
              a18 = -0.251644090693395E-11_SRK

  REAL(SRK), PARAMETER :: &
              b01 = -0.487687959756292E-01_SRK, &! parameters for region 3b
              b02 = 0.135336193587336E-02_SRK, &
              b03 = 0.413013399556249E0_SRK, &
              b04 = 0.122945704230431E0_SRK, &
              b05 = -0.141140213612559E-03_SRK, &
              b06 = 0.178357790645547E-01_SRK, &
              b07 = 0.395174104863528E-07_SRK, &
              b08 = -0.585398228522495E0_SRK, &
              b09 = 0.594938907295817E-04_SRK, &
              b10 = -0.119057019174713E0_SRK, &
              b11 = -0.194570794816001E-02_SRK, &
              b12 = 0.427529172119353E-01_SRK, &
              b13 = -0.760561375637742E-01_SRK, &
              b14 = 0.299546836134716E0_SRK, &
              b15 = 0.139391657221250E-01_SRK, &
              b16 = -0.278468126229603E-01_SRK, &
              b17 = 0.195041855369227E-02_SRK, &
              b18 = 0.375831122504734E-04_SRK, &
              b19 = 0.125685384163741E-03_SRK, &
              b20 = -0.762583183784896E-02_SRK, &
              b21 = -0.938937201479048E-06_SRK, &
              b22 = 0.134876477124427E-01_SRK, &
              b23 = 0.634557820845727E-02_SRK, &
              b24 = -0.291792573898258E-01_SRK, &
              b25 = -0.136017045350123E-05_SRK, &
              b26 = 0.454845318075243E-07_SRK, &
              b27 = -0.239512088942747E-09_SRK

  REAL(SRK), PARAMETER :: &
              c01 = -0.151226672652382E-01_SRK, &! parameters for region 3c
              c02 = 0.219075237766159E-03_SRK, &
              c03 = 0.114144924756274E-04_SRK, &
              c04 = -0.470815410341398E-02_SRK, &
              c05 = 0.105510662596481E-03_SRK, &
              c06 = 0.487932009131791E0_SRK, &
              c07 = 0.424198281757227E-03_SRK, &
              c08 = 0.305426466180436E-01_SRK, &
              c09 = -0.174690467895005E-04_SRK, &
              c10 = 0.509486478795057E-06_SRK, &
              c11 = -1.43708991982910E0_SRK, &
              c12 = -0.526750160303121E-04_SRK, &
              c13 = 1.22116000890535E0_SRK, &
              c14 = 0.104163340234817E-06_SRK, &
              c15 = 0.218460997189951E-02_SRK, &
              c16 = -0.152017319222412E-01_SRK, &
              c17 = -0.294106550573793E-01_SRK, &
              c18 = 0.165284534427183E-01_SRK, &
              c19 = -0.335234582911578E0_SRK, &
              c20 = -0.212663865441498E-02_SRK, &
              c21 = 0.753543651141502E-01_SRK, &
              c22 = -0.769927079971342E-01_SRK, &
              c23 = -0.247039860992736E-01_SRK, &
              c24 = -0.223745526548978E-02_SRK, &
              c25 = -0.113782324304171E-01_SRK, &
              c26 = 0.122241653100711E-01_SRK, &
              c27 = -0.163238458626065E-06_SRK, &
              c28 = 0.580172883857322E-09_SRK, &
              c29 = -0.419492118766324E-11_SRK, &
              c30 = -0.660478916724586E-06_SRK, &
              c31 = -0.119419038095450E-05_SRK, &
              c32 = 0.310771050242007E-01_SRK

  REAL(SRK), PARAMETER :: &
              d01 = 0.664933791260178E-01_SRK, &! parameters for region 3d
              d02 = 0.571985898251817E-06_SRK, &
              d03 = -0.274711031859796E0_SRK, &
              d04 = -0.252680942664202E0_SRK, &
              d05 = -0.392289014485859E-06_SRK, &
              d06 = 0.256374705633001E-10_SRK, &
              d07 = 1.04023037508896E0_SRK, &
              d08 = 0.643907516287541E-03_SRK, &
              d09 = 0.389907326497054E0_SRK, &
              d10 = -1.60832918484839E0_SRK, &
              d11 = 0.713823865906627E-03_SRK, &
              d12 = -0.303433314767063E0_SRK, &
              d13 = 1.25833184484538E0_SRK, &
              d14 = -0.792816174523651E-04_SRK, &
              d15 = 0.118639803204261E0_SRK, &
              d16 = -0.496046407723234E0_SRK, &
              d17 = -0.185997001981635E-01_SRK, &
              d18 = 0.785791469799311E-01_SRK

  REAL(SRK), PARAMETER :: &
              e01 = 0.301448430808593E-01_SRK, &! parameters for region 3e
              e02 = -0.335916193888413E-03_SRK, &
              e03 = -0.112702460700323E0_SRK, &
              e04 = -0.877499173794533E-20_SRK, &
              e05 = 0.187152530917710E-03_SRK, &
              e06 = 0.356911241995526E-02_SRK, &
              e07 = -0.418986486825425E-11_SRK, &
              e08 = 0.322726674818840E-06_SRK, &
              e09 = -0.120527379090314E-10_SRK, &
              e10 = -0.817132957960820E-13_SRK, &
              e11 = -0.289296115834392E-07_SRK, &
              e12 = -0.194260348835721E-06_SRK, &
              e13 = -0.813610292866497E-09_SRK, &
              e14 = 0.391792707971363E-13_SRK, &
              e15 = -0.519162117274822E-07_SRK, &
              e16 = -0.538255397523665E-06_SRK, &
              e17 = 0.200848495263495E-07_SRK, &
              e18 = -0.721941889977446E-02_SRK, &
              e19 = -0.313609300142694E-02_SRK, &
              e20 = 0.260485012629641E-04_SRK, &
              e21 = -0.370031971083042E-03_SRK, &
              e22 = 0.759164825488741E-07_SRK, &
              e23 = -0.169209023050985E-09_SRK, &
              e24 = 0.396910783770869E-01_SRK, &
              e25 = 0.127931680641201E-01_SRK, &
              e26 = 0.191456960283807E-06_SRK, &
              e27 = 0.766425815924100E-03_SRK, &
              e28 = -0.431282885175170E-05_SRK, &
              e29 = 0.225545818343096E-04_SRK, &
              e30 = 0.488412051812406E-07_SRK, &
              e31 = -0.356918730340587E-09_SRK, &
              e32 = 0.303625591962761E-03_SRK, &
              e33 = 0.124536067659580E-12_SRK, &
              e34 = -0.550701260682904E-02_SRK, &
              e35 = 0.252719052207758E-11_SRK, &
              e36 = 0.133491926757520E-12_SRK, &
              e37 = -0.607043036830454E-02_SRK, &
              e38 = 0.287671539256386E-05_SRK, &
              e39 = -0.958920650759504E-04_SRK, &
              e40 = 0.102650393925122E-07_SRK, &
              e41 = -0.155995837253683E-15_SRK

  REAL(SRK), PARAMETER :: &
              f01 = 0.329890852403526E-04_SRK, &! parameters for region 3f
              f02 = 0.379781955709120E-04_SRK, &
              f03 = 0.252360667718127E-06_SRK, &
              f04 = -0.407113420864393E-03_SRK, &
              f05 = -0.357933830043717E-04_SRK, &
              f06 = -0.245865969502991E-03_SRK, &
              f07 = 0.525013249033141E-09_SRK, &
              f08 = 0.321621088787326E-02_SRK, &
              f09 = 0.439053317238974E-03_SRK, &
              f10 = -0.587768799763869E-07_SRK, &
              f11 = 0.262763491213355E-05_SRK, &
              f12 = -0.214747822942469E-02_SRK, &
              f13 = -0.635909964040088E-04_SRK, &
              f14 = -0.115529419916092E-06_SRK, &
              f15 = 0.785448130272979E-03_SRK, &
              f16 = -0.259174703895765E-15_SRK, &
              f17 = 0.184266268449228E-10_SRK

  REAL(SRK), PARAMETER :: &
              g01 = 0.125537070183712E-02_SRK, &! c parameters for region 3g
              g02 = 0.130245527248376E-02_SRK, &
              g03 = 0.103367194207180E-01_SRK, &
              g04 = -0.254795720214314E-01_SRK, &
              g05 = -0.185955960512067E-03_SRK, &
              g06 = -0.960082399513164E-02_SRK, &
              g07 = 0.474944869074855E0_SRK, &
              g08 = -0.503420527214133E-02_SRK, &
              g09 = -0.687909934564732E0_SRK, &
              g10 = 30.7310678953686E0_SRK, &
              g11 = -0.168658389645091E-03_SRK, &
              g12 = 654.223452156635E0_SRK, &
              g13 = -320317.604761443E0_SRK, &
              g14 = 197.974246206705E0_SRK, &
              g15 = 11416042249924.4E0_SRK, &
              g16 = -9156698413.12590E0_SRK, &
              g17 = 2783392577409.60E0_SRK, &
              g18 = -281900719117892.0E0_SRK, &
              g19 = 0.119502836257688E+17_SRK, &
              g20 = -0.183657231751509E+18_SRK

  ! region 3a (t<tc, p<24 MPa, liquid)
  IF (ireg3 == 1) THEN
    pr = p - PCrit
    tr = t - Tcrit
    z8 = (dtdp + 0.05_SRK*pr)*pr - (1.0_SRK - 0.05_SRK*tr)*tr
    ! to avoid negative roots outside of range of validity
    IF (z8 < 0.0_SRK) THEN
      z4 = -1.0_SRK*sqrt(-1.0_SRK*z8)
      z2 = -1.0_SRK*sqrt(-1.0_SRK*z4)
      z1 = -1.0_SRK*sqrt(-1.0_SRK*z2)
    ELSE
      z4 = sqrt(z8)
      z2 = sqrt(z4)
      z1 = sqrt(z2)
    ENDIF
    vpt3n = a03*z1 + z4*(a10 + z1*(a12 + z1*(a14 + z2*(a15 + z4*a17)))) &
            + tr*(a01 + z1*(a04 + z1*(a06 + z1*(a08 + z1*(a11 + z1*a13)))) &
              + tr*(a02 + z1*(a05 + z1*(a07 + z1*(a09 + tr*z1*z4*(a16 + tr*z4*a18))))))
    vest3 = vpt3n + vc
    ! region 3b (t>tc, v<~vc, p<24 MPa)
  ELSEIF (ireg3 == 2) THEN
    pr = p - PCrit
    p2 = pr*pr
    p4 = p2*p2
    tr = t - Tcrit
    t2 = tr*tr
    c34 = c32*2.0_SRK
    z8 = (dtdp + 0.02_SRK*pr)*pr - (1.0_SRK - 0.02_SRK*tr)*tr
    ! to avoid negative roots outside of range of validity
    IF (z8 < 0.0_SRK) THEN
      z4 = -1.0_SRK*sqrt(-1.0_SRK*z8)
      z2 = -1.0_SRK*sqrt(-1.0_SRK*z4)
      z1 = -1.0_SRK*sqrt(-1.0_SRK*z2)
    ELSE
      z4 = sqrt(z8)
      z2 = sqrt(z4)
      z1 = sqrt(z2)
    ENDIF
    vpt3n = p2*b03 + t2*(b01 + pr*b02) &
            + z1*(pr*b06 + p2*b08 + tr*(p4*b09 + tr*(b04 + tr*(b05 + t2*pr*b07))) &
              + z1*(t2*b10 &
                + z1*(b11 + pr*b13 + p2*b14 + t2*b12 &
                  + z1*(tr*b15 &
                    + z1*(tr*pr*b16 &
                      + z1*(b17 &
                        + z2*(p2*(pr*b19 + t2*b18) &
                          + z2*(tr*b20 + t2*p4*pr*b21 &
                            + z2*(b22 + pr*b24 + tr*(b23 + t2*p2*(b25 + p4*(b26 + t2*b27))))))))))))
    vest3 = vpt3n + vc
    ! region 3c (v>~vc, PCrit<p<24 MPa)
  ELSEIF (ireg3 == 3) THEN
    pr = p - PCrit
    p2 = pr*pr
    tr = t - Tcrit
    t2 = tr*tr
    t3 = t2*tr
    t4 = t2*t2
    z8 = (0.2_SRK*pr - dtdp)*pr + (1.0_SRK + 0.02_SRK*tr)*tr
    ! to avoid negative roots outside of range of validity
    IF (z8 < 0.0_SRK) THEN
      z4 = -1.0_SRK*sqrt(-1.0_SRK*z8)
      z2 = -1.0_SRK*sqrt(-1.0_SRK*z4)
      z1 = -1.0_SRK*sqrt(-1.0_SRK*z2)
    ELSE
      z4 = sqrt(z8)
      z2 = sqrt(z4)
      z1 = sqrt(z2)
    ENDIF
    vpt3n = t2*c01 + t3*c02 + t4*c03 + pr*(c04 + t3*c05 + pr*(c06 + p2*c07)) &
            + z1*(t2*c08 + t4*(c09 + tr*c10) + p2*c11 &
              + z1*(pr*t3*c12 + p2*(c13 + t4*c14) &
                + z1*(c15 + t2*c16 + pr*(c17 + tr*c18) + p2*(c19 + pr*c20) &
                  + z2*(tr*c21 &
                    + z1*(tr*c22 + pr*c23 &
                      + z2*(p2*c24 &
                        + z2*(c25 &
                          + tr*(c26 + t3*c27 &
                            + z2*(t4*(c28 + tr*c29) + p2*(tr*c30 + pr*c31))))))))))
    vest3 = vpt3n + vc
    ! region 3d (t>tsat, p<PCrit)
  ELSEIF (ireg3 == 4) THEN
    pr = p - PCrit
    tr = t - Tcrit
    t2 = tr*tr
    z8 = (0.5_SRK*pr - dtdp)*pr + (1.0_SRK + 0.01_SRK*tr)*tr
    ! to avoid negative roots outside of range of validity
    IF (z8 < 0.0_SRK) THEN
      z4 = -1.0_SRK*sqrt(-1.0_SRK*z8)
      z2 = -1.0_SRK*sqrt(-1.0_SRK*z4)
      z1 = -1.0_SRK*sqrt(-1.0_SRK*z2)
    ELSE
      z4 = sqrt(z8)
      z2 = sqrt(z4)
      z1 = sqrt(z2)
    ENDIF
    vpt3n = z2*(d08 + z1*(d11 + z1*d14)) &
            + tr*(d01 + z1*(d04 + z1*(d09 + z1*(d12 + z1*(d15 + z1*d17)))) &
              + t2*(d02 + z1*(d05 + t2*d06))) &
                + pr*(d03 + z1*(d07 + z1*(d10 + z1*(d13 + z1*(d16 + z1*d18)))))
    vest3 = vpt3n + vc
    ! region 3e (v<~vc, 24 MPa<p<40 MPa)
  ELSEIF (ireg3 == 5) THEN
    pr = p - PCrit
    p2 = pr*pr
    p4 = p2*p2
    p5 = p4*pr
    tr = t - Tcrit
    t2 = tr*tr
    t3 = t2*tr
    z8 = (dtdp + 0.02_SRK*pr)*pr + (0.005_SRK*tr - 1.0_SRK)*tr
    ! to avoid negative roots outside of range of validity
    IF (z8 < 0.0_SRK) THEN
      z4 = -1.0_SRK*sqrt(-1.0_SRK*z8)
      z2 = -1.0_SRK*sqrt(-1.0_SRK*z4)
      z1 = -1.0_SRK*sqrt(-1.0_SRK*z2)
    ELSE
      z4 = sqrt(z8)
      z2 = sqrt(z4)
      z1 = sqrt(z2)
    ENDIF
    vpt3n = pr*e03 + tr*e01 + t2*(e02 + t2*t2*p5*e04) &
            + z1*(t2*e05 &
              + z1*(pr*e06 + t2*p5*e07 &
                + z1*(tr*p4*e11 + t3*(e08 + tr*p2*pr*e10 + t2*e09) &
                  + z1*(p5*e15 + t3*(e12 + tr*e13 + t3*e14) &
                    + z1*(p4*(e16 + tr*e17) &
                      + z1*(e18 + tr*(e19 + pr*e21 + p5*e23 + tr*(e20 + p2*e22)) &
                        + z2*(e24 + pr*(e25 + pr*e27 + p2*e29 + tr*(pr*e28 + p2*e30 + tr*(e26 + p2*e31 &
                                                                                      ))) &
                          + z2*(pr*e34 + tr*(e32 + p5*pr*e36 + tr*(p4*e35 + t3*e33)) &
                            + z2*(e37 + p2*e39 + p4*e40 + tr*pr*(e38 + tr*p5*e41))))))))))
    vest3 = vpt3n + vc
  ELSEIF (ireg3 == 6) THEN
    pr = p - PCrit
    p2 = pr*pr
    tr = t - Tcrit
    z4 = (0.1_SRK*pr - dtdp)*pr + (1.0_SRK + 0.002_SRK*tr)*tr
    ! to avoid negative roots outside of range of validity
    IF (z4 < 0.0_SRK) THEN
      z2 = -1.0_SRK*sqrt(-1.0_SRK*z4)
      z1 = -1.0_SRK*sqrt(-1.0_SRK*z2)
    ELSE
      z2 = sqrt(z4)
      z1 = sqrt(z2)
    ENDIF
    z3 = z1*z2
    vpt3n = z3*(f08 + z2*(f12 + p2*f14 + z1*f15)) &
            + tr*(f01 + z1*(f04 + z1*(f06 + z1*(f09 + z2*(f13 + z1*p2*pr*f17)))) &
              + tr*(f02 + z1*(f05 + z3*f11) &
                + tr*(f03 + z3*f10 &
                  + tr*z2*(f07 + z4*p2*f16))))
    vest3 = vpt3n + vc
    ! region 3g (40 MPa<p<100 MPa)
  ELSEIF (ireg3 == 7) THEN
    pr = p*(1.0_SRK/103.0_SRK) - 1.0_SRK
    p2 = pr*pr
    p4 = p2*p2
    tr = t*(1.0_SRK/600.0_SRK) - 1.0_SRK
    t2 = tr*tr
    t3 = t2*tr
    t4 = t2*t2
    t6 = t3*t3
    p2t2 = p2*t2
    vpt3n = g01 + tr*(g02 + t2*(g03 + t3*g04)) &
            + pr*(g05 + t2*g06 &
              + pr*(tr*t4*g07 &
                + pr*(tr*(g08 + t3*(g09 + t4*g10)) &
                  + p2*(g11 &
                    + p2*pr*(t6*g12 &
                      + p2*t4*(t6*g13 &
                        + p4*(g14 &
                          + p2*t6*(t6*t6*g15 &
                            + p4*p4*(g16 &
                              + p2t2*(g17 &
                                + p2t2*(g18 &
                                  + p2t2*(g19 + p2t2*g20))))))))))))
    vest3 = vpt3n
  ENDIF
ENDFUNCTION vest3
!
!-------------------------------------------------------------------------------
!> Returns the specific volume of the phase.  Works for saturated liquid or
!> vapor.
!> @param P    The saturation pressure
!> @param T    The saturation temperature
!> @param DEST The density of the fluid [kg/m**3]
!> @param EPS  TODO - what is EPS
!>
FUNCTION diter3(P, T, DEST, EPS)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  REAL(SRK), INTENT(IN) :: DEST
  REAL(SRK), INTENT(IN) :: EPS
  REAL(SRK) :: diter3

  REAL(SRK) :: d1v, d2v, d1, d2, x

  d1v = dest*0.999_SRK
  d2v = dest*1.001_SRK
  IF (dest <= DCrit) THEN
    d1 = d1v
    IF (d2v <= DCrit) THEN
      d2 = d2v
    ELSE
      d2 = DCrit
    ENDIF
  ELSE
    d2 = d2v
    IF (d1v >= DCrit) THEN
      d1 = d1v
    ELSE
      d1 = DCrit
    ENDIF
  ENDIF

  CALL WNPT3(d1, d2, p, t, eps, x)
  DITER3 = x
ENDFUNCTION diter3
!
!-------------------------------------------------------------------------------
!TODO needs header
FUNCTION NULLP3N(D, T, P)
  REAL(SRK), INTENT(IN) :: D
  REAL(SRK), INTENT(IN) :: T
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK) :: NULLP3N

  REAL(SRK) :: V

  V = 1.0_SRK/D
  NULLP3N = PVT3N(V, T) - P
ENDFUNCTION nullp3n
!
!-------------------------------------------------------------------------------
!TODO needs header
SUBROUTINE WNPT3(XA, XB, P, T, EPS, X)
  REAL(SRK), INTENT(IN) :: XA
  REAL(SRK), INTENT(IN) :: XB
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  REAL(SRK), INTENT(IN) :: EPS
  REAL(SRK), INTENT(OUT) :: X

  REAL(SRK) :: X1, F1, F2, F3, X2, P1, P3, X3
  INTEGER(SIK) :: irem, i

  X1 = XA

  F1 = nullp3n(X1, T, P)
  X3 = XB
  F3 = nullp3n(X3, T, P)

  x = 0.0_SRK
  DO I = 1, 40
    IF (F1 /= F3) THEN
      X = X1 + (X3 - X1)*F1/(F1 - F3)
    ELSE
      EXIT
    ENDIF
    IF (X < 0.0_SRK) X = (X1 + X3)/2.0_SRK
    IF (ABS(X) < 1.E-8) THEN
      IF (ABS(X - X1) < EPS) RETURN
    ELSE
      IF (ABS((X - X1)/X) < EPS) RETURN
    ENDIF
    F2 = nullp3n(X, T, P)
    X2 = X1 - (X1 - X3)/2.0_SRK
    P1 = F2*F1
    P3 = F2*F3
    IF ((P1 < 0.0_SRK) .AND. (P3 < 0.0_SRK)) THEN
      P1 = ABS(P1)
      P3 = ABS(P3)
    ENDIF
    IF (P1 <= P3) THEN
      X3 = X1
      F3 = F1
    END IF
    X1 = X
    F1 = F2
    IF ((X2 - X3)*(X2 - X1) >= 0.0_SRK) CYCLE
    IF ((ABS(F1/F3)) <= (0.3_SRK)) THEN
      X = X1 + ABS(F1/F3)*1.5_SRK*(X3 - X1)
      IREM = 1
    ELSE IF ((ABS(F1/F3)) >= (3.0_SRK)) THEN
      IREM = 2
    ELSE
      X = (X1 + X3)/2.0_SRK
      IREM = 0
    ENDIF
    F2 = nullp3n(X, T, P)
    IF ((((F2*F1) >= 0.0_SRK) .AND. (IREM == 1)) .OR. (((F2*F3) >= 0.0_SRK) .AND. (IREM == 2))) THEN
      X = (X1 + X3)/2.0_SRK
      F2 = nullp3n(X, T, P)
    ENDIF
    IF (F2*F1 <= F2*F3) THEN
      X3 = X1
      F3 = F1
    ENDIF
    X1 = X
    F1 = F2
  ENDDO
ENDSUBROUTINE WNPT3
!
!-------------------------------------------------------------------------------
!TODO needs header
FUNCTION tph3n(P, h)
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: h
  REAL(SRK) :: tph3n

  REAL(SRK) :: eps, TE
  REAL(SRK) :: T1, T2, TG, X
  INTEGER(SIK) :: IX, i2
  REAL(SRK), PARAMETER :: &
    hc = 2087.54684511650_SRK, &
    a1 = 646.836937239618_SRK, &
    a2 = 62.7217723183194_SRK, &
    a3 = -18.0176207184787_SRK, &
    a4 = 2.01708278241311_SRK, &
    a5 = 247.531572315595_SRK, &
    a6 = -41.1717709139789_SRK, &
    a7 = -150.208861198343_SRK, &
    a8 = 1431.77901876243_SRK, &
    a9 = -862.734816079672_SRK, &
    a0 = 49.9470109089612_SRK
  REAL(SRK) :: hr, pr

  EPS = 1.E-12

  ! Normalize variables
  hr = h*(1.0_SRK/hc) - 1.0_SRK
  pr = p*(1.0_SRK/PCrit) - 1.0_SRK

  ! Initial Estimate for Temperature
  TE = a1 + pr*(a2 + pr*(a3 + pr*a4)) + hr*(pr*(a5 + pr*a6) + hr*(a7 + hr*(a8 + pr*(a9 + pr*pr*a0))))

  ! Set Initial Guess for T1
  IF (TE < 629.0_SRK) THEN
    T1 = 623.15_SRK
  ELSE
    T1 = TE*0.996_SRK
  ENDIF

  ! Get the Saturated Temperature
  TG = FB23P(P)
  ! Set Initial Guess for T2
  IF (TE > (TG/1.01_SRK)) THEN
    T2 = TG
  ELSE
    T2 = TE*1.004_SRK
  ENDIF

  i2 = 3
  CALL wnph3(t1, t2, i2, h, p, eps, x, ix)

  IF (ix <= 0) THEN
     tph3n = x
  ELSE
     tph3n = 0.0_SRK
  ENDIF
ENDFUNCTION tph3n
!
!-------------------------------------------------------------------------------
!TODO needs header
FUNCTION nullh35n(T, P, h, reg)
  REAL(SRK), INTENT(IN) :: T
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: h
  INTEGER(SIK), INTENT(IN) :: reg
  REAL(SRK) :: nullh35n

  REAL(SRK) :: VE

  IF (REG == 3) THEN
    VE = VPT3N(P, T)
    NULLH35N = HVT3N(VE, T) - H
  ELSE
    NULLH35N = hpt(P, T, 5) - H
  ENDIF
ENDFUNCTION nullh35n
!
!-------------------------------------------------------------------------------
!TODO needs header
SUBROUTINE wnph3(xa, xb, reg, P, T, eps, x, ix)
  REAL(SRK), INTENT(IN) :: xa
  REAL(SRK), INTENT(IN) :: xb
  INTEGER(SIK), INTENT(IN) :: reg
  REAL(SRK), INTENT(IN) :: P
  REAL(SRK), INTENT(IN) :: T
  REAL(SRK), INTENT(IN) :: eps
  REAL(SRK), INTENT(OUT) :: x
  INTEGER(SIK),INTENT(OUT) :: ix

  INTEGER(SIK) :: i, irem
  REAL(SRK) :: x1, x2, x3, f1, f2, f3, p1, p3

  x1 = xa
  f1 = nullh35n(x1, t, p, reg)
  x3 = xb
  f3 = nullh35n(x3, t, p, reg)
  ix = 0
  DO i = 1, 40
    IF (f1 /= f3) THEN
      x = x1 + (x3 - x1)*f1/(f1 - f3)
    ELSE
      ix = 3
      RETURN
    ENDIF
    IF (x < 0.0_SRK) x = (x1 + x3)/2.0_SRK
    IF (ABS(x) < 1.0E-8) THEN
      IF (ABS(x - x1) < eps) RETURN
    ELSE
      IF (ABS((x - x1)/x) < eps) RETURN
    ENDIF
    f2 = nullh35n(x, t, p, reg)
    x2 = x1 - (x1 - x3)/2.0_SRK
    p1 = f2*f1
    p3 = f2*f3
    IF ((p1 < 0.0_SRK) .AND. (p3 < 0.0_SRK)) THEN
      p1 = ABS(p1)
      p3 = ABS(p3)
    ENDIF
    IF (p1 <= p3) THEN
      x3 = x1
      f3 = f1
    ENDIF
    x1 = x
    f1 = f2
    IF ((x2 - x3)*(x2 - x1) >= 0.0_SRK) CYCLE
    !      x=(x1+x3)/2.0_SRK
    IF ((ABS(f1/f3)) <= (0.3_SRK)) THEN
      x = x1 + ABS(f1/f3)*1.5_SRK*(x3 - x1)
      irem = 1
    ELSEIF ((ABS(f1/f3)) >= (3.0_SRK)) THEN
      irem = 2
    ELSE
      x = (x1 + x3)/2.0_SRK
      irem = 0
    ENDIF
    f2 = nullh35n(x, t, p, reg)
    IF ((((f2*f1) >= 0.0_SRK) .AND. (irem == 1)) .OR. (((f2*f3) >= 0.0_SRK) .AND. (irem == 2))) THEN
      x = (x1 + x3)/2.0_SRK
      f2 = nullh35n(x, t, p, reg)
    ENDIF
    IF (f2*f1 <= f2*f3) THEN
      x3 = x1
      f3 = f1
    ENDIF
    x1 = x
    f1 = f2
  ENDDO
  ix = 1
ENDSUBROUTINE wnph3
!
!-------------------------------------------------------------------------------
!> Calculating tsat,saturated vapour and liquid volume for a given p
!> @param dvout Specific volume of vapor [m**3/kg]
!> @param dlout Specific volume of liquid [m**3/kg]
!> @param tin Temperature
!TODO all inputs need header
!>
SUBROUTINE fsatp(dvout, dlout, tout, tin, pin)
  REAL(SRK), INTENT(OUT) :: dvout
  REAL(SRK), INTENT(OUT) :: dlout
  REAL(SRK), INTENT(OUT) :: tout
  REAL(SRK), INTENT(IN) :: tin
  REAL(SRK), INTENT(IN) :: pin

  REAL(SRK) :: p, t
  REAL(SRK), PARAMETER :: eps = 1.E-10
  REAL(SRK) :: dl, dv, t0, dle, dve

  p = pin

  IF (ABS(PCrit - pin) < eps) THEN
    tout = tsatpn(p)
    DLOUT = dlest(tout)
    DVOUT = dvest(tout)
    RETURN
  ENDIF

  dl = 0.0_SRK
  dv = 0.0_SRK
  t0 = 0.0_SRK

  t = tin
  dle = dlest(t)
  dve = dvest(t)

  dlout = diter3(p, t, dle, eps)
  dvout = diter3(p, t, dve, eps)
  tout = t
ENDSUBROUTINE fsatp
!
!-------------------------------------------------------------------------------
!> Estimates the liquid density for region 3
!> Returns density [kg/m^3]
!> @param t The saturation temperature at which liquid density will be evaluated
!>          [K]
!>
FUNCTION dlest(T)
  REAL(SRK), INTENT(IN) :: T
  REAL(SRK) :: dlest

  REAL(SRK), PARAMETER :: &
      b1 = 1.99274064_SRK, b2 = 1.09965342_SRK, &
      b3 = -0.510839303_SRK, b4 = -1.75493479_SRK, &
      b5 = -45.5170352_SRK, b6 = -6.74694450E+05_SRK
  REAL(SRK) :: tau, tauh

  tau = 1.0_SRK - t/Tcrit
  tauh = tau**(1.0_SRK/3.0_SRK)

  dlest = Dcrit*(1.0_SRK + b1*tauh + b2*tauh*tauh + b3*tauh**5 &
                 + b4*tauh**16 + b5*tauh**43 + b6*tauh**110)
ENDFUNCTION dlest
!
!-------------------------------------------------------------------------------
!TODO needs header
! Estimates the vapor density for region 3
! Returns density [kg/m^3]
FUNCTION dvest(t)
  REAL(SRK), INTENT(IN) :: t
  REAL(SRK) :: dvest

  REAL(SRK), PARAMETER :: &
    c1 = -2.03150240_SRK, c2 = -2.68302940_SRK, &
    c3 = -5.38626492_SRK, c4 = -17.2991605_SRK, &
    c5 = -44.7586581_SRK, c6 = -63.9201063_SRK
  REAL(SRK) :: dh, tau, tauh

  tau = 1.0_SRK - t/tcrit
  tauh = tau**(1.0_SRK/6.0_SRK)

  dh = c1*tauh**2 + c2*tauh**4 + c3*tauh**8 + c4*tauh**18 + c5*tauh**37 + c6*tauh**71
  dvest = Dcrit*EXP(dh)

ENDFUNCTION dvest
!
!-------------------------------------------------------------------------------
!> Calculation of Specific Volume \f$v\f$
!> Reference 1
!> Tables : 31
!> @param P Pressure in MPa
!> @param T Temperature in Kelvin
!> @return \f$v\f$ m^3/kg
!>
FUNCTION hvt3n(V, T)
  REAL(SRK), INTENT(IN) :: V
  REAL(SRK), INTENT(IN) :: T
  REAL(SRK) ::hvt3n

  REAL(SRK) :: delta, tau ! Dimensionless density and temperature

  ! Normalize variables
  delta = normalize_density(1.0_SRK/V)
  tau = normalize_temperature(T, 3)

  ! Calculate property based off of table 31.
  hvt3n = R*T*(tau*phi3(delta, tau, 0, 1) + delta*phi3(delta, tau, 1, 0)) ! [kJ/kg]

ENDFUNCTION hvt3n
!
!-------------------------------------------------------------------------------
!> Calculation of Pressure \f$v\f$
!> Reference 1
!> Tables : 31
!> @param P Pressure in MPa
!> @param T Temperature in Kelvin
!> @return P [MPa]
!>
FUNCTION pvt3n(V, T)
  REAL(SRK), INTENT(IN) :: V
  REAL(SRK), INTENT(IN) :: T
  REAL(SRK) :: pvt3n

  REAL(SRK) :: delta, tau ! Dimensionless density and temperature

  ! Normalize variables
  delta = normalize_density(1.0_SRK/V)
  tau = normalize_temperature(T, 3)

  ! Calculate property based off of table 31.
  pvt3n = R*T/V*delta*phi3(delta, tau, 1, 0)*1.0E-3_SRK ! [MPa]
ENDFUNCTION pvt3n
!
ENDMODULE IAPWSWaterPropertiesModule
