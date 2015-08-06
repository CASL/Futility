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
!> @brief Module for specifying kind parameters for intrinsic data types
!>
!> The kind parameters are to be used elsewhere throughout the code.
!> The provided kinds are:
!>   - SBK: Selected boolean Kind for the LOGICAL type.
!>   - SNK: Selected Normal integer Kind for the INTEGER type.
!>   - SLK: Selected Long integer Kind for the INTEGER type.
!>   - SIK: Selected Integer Kind for the INTEGER type.
!>   - SSK: Selected Single precision Kind for the REAL type.
!>   - SDK: Selected Double precision Kind for the REAL type.
!>   - SRK: Selected Real Kind for the REAL type.
!>
!> The definitions of SRK and SIK can be changed based on the preprocessor
!> symbols DBL and DBLINT respectively. If DBL is defined all real types are
!> double precision (64-bit), if DBLINT is defined all integers are 64-bit.
!> This module is tested using @c testSelectedKinds.f90. An example of how to
!> use this module is given below, the unit test also shows how to use the
!> module.
!>
!> @par EXAMPLE
!> @code
!> PROGRAM
!>   USE IntrType
!>   IMPLICIT NONE
!>
!>   LOGICAL(SBK) :: bool
!>   INTEGER(SNK) :: i32bit
!>   INTEGER(SLK) :: i64bit
!>   INTEGER(SIK) :: i
!>   REAL(SSK) :: r32bit !single precision
!>   REAL(SDK) :: r64bit !double precision
!>   REAL(SRK) :: r
!>
!>   ! ... some executable code ...
!>
!> END PROGRAM
!> @endcode
!>
!> @author Brendan Kochunas
!>   @date 03/19/2009
!>
!> @par Revisions:
!> (01/17/2011) - Brendan Kochunas
!>   - Added documentation for doxygen. Set private public access to variables.
!>     Added SBK variable for logical types.
!> @par
!> (04/20/2011) - Brendan Kochunas
!>   - Added Long (64-bit) integer kind and put under test.
!> @par
!> (06/23/2011) - Brendan Kochunas
!>   - Updated module documentation.
!> @par
!> (08/29/2011) - Brendan Kochunas
!>   - Added EPSREAL for comparing real variables
!> @par
!> (08/30/2011) - Brendan Kochunas
!>   - Added operator .APPROXEQ. for "approximately equal"
!> @par
!> (09/06/2012) - Ben Collins
!>   - Added function SOFTEQ for "soft equalivalence"
!> @par
!> (09/24/2011) - Brendan Kochunas
!>   - Added operator .APPROXLE. and .APPROXGE. for "approximately less/greater than"
!> @par
!> (11/26/2011) - Zhouyu Liu
!>   - Added function SOFTEQR for "soft relative equalivalence"
!>
!> @todo
!> - Add support for Fortran 2003 C-interoperable types.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE IntrType
  USE ISO_C_BINDING
  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: SBK,SNK,SLK,SIK,SSK,SDK,SRK
  PUBLIC :: EPSS
  PUBLIC :: EPSD
  PUBLIC :: EPSREAL
  PUBLIC :: OPERATOR(.APPROXEQ.)
  PUBLIC :: OPERATOR(.APPROXEQF.)
  PUBLIC :: OPERATOR(.APPROXEQA.)
  PUBLIC :: OPERATOR(.APPROXEQR.)
  PUBLIC :: OPERATOR(.APPROXLE.)
  PUBLIC :: OPERATOR(.APPROXGE.)
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(/=)
  PUBLIC :: ASSIGNMENT(=)
  PUBLIC :: SOFTEQ
  PUBLIC :: SOFTEQR
  PUBLIC :: SOFTLE
  PUBLIC :: SOFTGE
  PUBLIC :: isNAN
  PUBLIC :: isINF
  PUBLIC :: char_to_int_array
  PUBLIC :: char_to_double_array
!
! Variables
  !> @name Private Variables
  !< @{
  !> @brief Exponent for range of integers.
  !>
  !> Means the range of integers in -10^(N_INT_ORDER) to +10^(N_INT_ORDER)
  INTEGER,PARAMETER :: N_INT_ORDER=8

  !> @brief Exponent for range of long integers.
  !>
  !> Means the range of integers in -10^(N_LONG_ORDER) to +10^(N_LONG_ORDER)
  INTEGER,PARAMETER :: N_LONG_ORDER=18

  !> @brief Significant digits for single precision.
  INTEGER,PARAMETER :: N_SGL_DIGITS=6

  !> @brief Significant digits for double precision.
  INTEGER,PARAMETER :: N_DBL_DIGITS=15
  !> @}

  !> @brief LOGICAL (boolean) kind
  !>
  !> The selected logical kind. SBK is also an acronym for Selected Boolean
  !> Kind; the number of bytes of memory occupied by a logical variable.
  INTEGER,PARAMETER :: SBK=KIND(.TRUE.)

  !> @brief INTEGER kind for normal (32-bit) integers
  !>
  !> The selected integer kind ensuring integers up to 10^(N_INT_ORDER)
  !> SNK is also an acronym for Selected Normal integer Kind.
  INTEGER,PARAMETER :: SNK=SELECTED_INT_KIND(N_INT_ORDER)

  !> @brief INTEGER kind for long (64-bit) integers
  !>
  !> The selected integer kind ensuring integers up to 10^(N_INT_ORDER)
  !> SIK is also an acronym for Selected Long integer Kind.
  INTEGER,PARAMETER :: SLK=SELECTED_INT_KIND(N_LONG_ORDER)

  !> @brief REAL kind for single precision (32-bit)
  !>
  !> The selected real kind ensuring N_SGL_DIGITS precision on a given
  !> machine. SSK is also an acronym for Selected Single Kind; the number
  !> of bytes of memory occupied by a single precision floating point
  !> variable.
  INTEGER,PARAMETER :: SSK=SELECTED_REAL_KIND(N_SGL_DIGITS)

  !> @brief REAL kind for double precision (64-bit)
  !>
  !> The kind type parameter ensuring N_DBL_DIGITS of precision on a given
  !> machine. SDK is also an acronym for Selected Double Kind; the number
  !> of bytes of memory occupied by a floating point variable.
  INTEGER,PARAMETER :: SDK=SELECTED_REAL_KIND(N_DBL_DIGITS)

  !> @brief REAL kind
  !>
  !> The kind type parameter for REAL intrinsic types or floating point
  !> types. Also an acronym for Selected Real Kind; the number of bytes of
  !> memory occupied by a floating point variable. Changing SRK=SSK means
  !> the code will use use single precision floating point arithmetic
  !> instead of double precision.
#ifdef DBL
  INTEGER,PARAMETER :: SRK=SDK
#else
  INTEGER,PARAMETER :: SRK=SSK
#endif

  !> @brief INTEGER kind for integers
  !>
  !> The selected integer kind ensuring integers up to 10^(N_INT_ORDER)
  !> SIK is also an acronym for Selected Integer Kind.
#ifdef DBLINT
  INTEGER,PARAMETER :: SIK=SLK
#else
  INTEGER,PARAMETER :: SIK=SNK
#endif

  !> @brief The number of significant digits to use when comparing
  !> double precision real numbers.
  !>
  !> Only relevant for .APPROXEQ. operator
  REAL(SDK),PARAMETER :: EPSD=1.e-14_SDK

  !> @brief The number of significant digits to use when comparing
  !> single precision real numbers
  !>
  !> Only relevant for .APPROXEQ. operator
  REAL(SDK),PARAMETER :: EPSS=1.e-5_SDK

  !> @brief The number of significant digits to use when comparing
  !> real numbers.
  !>
  !> Only relevant for .APPROXEQ. operator
#ifdef DBL
  REAL(SDK),PARAMETER :: EPSREAL=EPSD
#else
  REAL(SDK),PARAMETER :: EPSREAL=EPSS
#endif

  !> @brief Interface for the operator for "approximately equals" for intrinsic
  !> real kinds. Compares significant decimal digits
  INTERFACE OPERATOR(.APPROXEQ.)
    !> @copybrief IntrType::approxeq_single
    MODULE PROCEDURE approxeq_single
    !> @copybrief IntrType::approxeq_double
    MODULE PROCEDURE approxeq_double
  ENDINTERFACE

  !> @brief Interface for the operator for "approximately equals" for intrinsic
  !> real kinds. Performs absolute comparison to EPSREAL
  INTERFACE OPERATOR(.APPROXEQA.)
    !> @copybrief IntrType::approxeq_abs_single
    MODULE PROCEDURE approxeq_abs_single
    !> @copybrief IntrType::approxeq_abs_double
    MODULE PROCEDURE approxeq_abs_double
  ENDINTERFACE

  !> @brief Interface for the operator for "approximately equals" for intrinsic
  !> real kinds. Performs relative comparison to EPSREAL
  INTERFACE OPERATOR(.APPROXEQR.)
    !> @copybrief IntrType::approxeq_rel_single
    MODULE PROCEDURE approxeq_rel_single
    !> @copybrief IntrType::approxeq_rel_double
    MODULE PROCEDURE approxeq_rel_double
  ENDINTERFACE

  !> @brief Interface for the operator for "approximately equals" for intrinsic
  !> real kinds. Performs bitwise comparison allowing for 10 nearest floats.
  INTERFACE OPERATOR(.APPROXEQF.)
    !> @copybrief IntrType::approxeq_ulp_single
    MODULE PROCEDURE approxeq_ulp_single
    !> @copybrief IntrType::approxeq_ulp_double
    MODULE PROCEDURE approxeq_ulp_double
  ENDINTERFACE

  !> @brief Interface for the operator for "approximately less than" for intrinsic
  !> types
  INTERFACE OPERATOR(.APPROXLE.)
    !> @copybrief IntrType::approxle_single
    MODULE PROCEDURE approxle_single
    !> @copybrief IntrType::approxle_double
    MODULE PROCEDURE approxle_double
  ENDINTERFACE

  !> @brief Interface for the operator for "approximately greater than" for intrinsic
  !> types
  INTERFACE OPERATOR(.APPROXGE.)
    !> @copybrief IntrType::approxge_single
    MODULE PROCEDURE approxge_single
    !> @copybrief IntrType::approxge_double
    MODULE PROCEDURE approxge_double
  ENDINTERFACE

  !> @brief Interface for the operator for "soft equivalence" for intrinsic
  !> types
  INTERFACE SOFTEQ
    !> @copybrief IntrType::softeq_single
    MODULE PROCEDURE softeq_single
    !> @copybrief IntrType::softeq_double
    MODULE PROCEDURE softeq_double
  ENDINTERFACE

  !> @brief Interface for the operator for "soft relative equivalence" for intrinsic
  !> types
  INTERFACE SOFTEQR
    !> @copybrief IntrType::softeqr_single
    MODULE PROCEDURE softeqr_single
    !> @copybrief IntrType::softeqr_double
    MODULE PROCEDURE softeqr_double
  ENDINTERFACE

  !> @brief Interface for the operator for "soft less than or equal to" for intrinsic
  !> types
  INTERFACE SOFTLE
    !> @copybrief IntrType::softle_single
    MODULE PROCEDURE softle_single
    !> @copybrief IntrType::softle_double
    MODULE PROCEDURE softle_double
  ENDINTERFACE

  !> @brief Interface for the operator for "soft greater than or equal to" for intrinsic
  !> types
  INTERFACE SOFTGE
    !> @copybrief IntrType::softge_single
    MODULE PROCEDURE softge_single
    !> @copybrief IntrType::softge_double
    MODULE PROCEDURE softge_double
  ENDINTERFACE

  !> @brief Interface for the function for isNAN for real
  !> kinds
  INTERFACE isNAN
    !> @copybrief IntrType::isnan_single
    MODULE PROCEDURE isnan_single
    !> @copybrief IntrType::isnan_double
    MODULE PROCEDURE isnan_double
  ENDINTERFACE

  !> @brief Interface for the function for isINF for real
  !> kinds
  INTERFACE isINF
    !> @copybrief IntrType::isinf_single
    MODULE PROCEDURE isinf_single
    !> @copybrief IntrType::isinf_double
    MODULE PROCEDURE isinf_double
  ENDINTERFACE

  !> Definition of external C interfaces defined in CUtils.
  INTERFACE
    FUNCTION isNAN_float_c(x) RESULT(bool) &
      BIND(C,NAME="isNAN_float_c")
      USE ISO_C_BINDING
      REAL(C_FLOAT),INTENT(IN) :: x
      INTEGER(C_INT) :: bool
    ENDFUNCTION isNAN_float_c

    FUNCTION isNAN_double_c(x) RESULT(bool) &
      BIND(C,NAME="isNAN_double_c")
      USE ISO_C_BINDING
      REAL(C_DOUBLE),INTENT(IN) :: x
      INTEGER(C_INT) :: bool
    ENDFUNCTION isNAN_double_c

    FUNCTION isINF_float_c(x) RESULT(bool) &
      BIND(C,NAME="isINF_float_c")
      USE ISO_C_BINDING
      REAL(C_FLOAT),INTENT(IN) :: x
      INTEGER(C_INT) :: bool
    ENDFUNCTION isINF_float_c

    FUNCTION isINF_double_c(x) RESULT(bool) &
      BIND(C,NAME="isINF_double_c")
      USE ISO_C_BINDING
      REAL(C_DOUBLE),INTENT(IN) :: x
      INTEGER(C_INT) :: bool
    ENDFUNCTION isINF_double_c
  ENDINTERFACE

  !> @brief Overloads the Fortran intrinsic operator for comparing
  !> two logicals to see if they are equal
  INTERFACE OPERATOR(==)
    !> @copybrief IntrType::equalto_logical
    !> @copydetails IntrType::equalto_logical
    MODULE PROCEDURE equalto_logical
  ENDINTERFACE

  !> @brief Overloads the Fortran intrinsic operator for comparing
  !> two logicals to see if they are not equal
  INTERFACE OPERATOR(/=)
    !> @copybrief IntrType::notequalto_logical
    !> @copydetails IntrType::notequalto_logical
    MODULE PROCEDURE notequalto_logical
  ENDINTERFACE

  INTERFACE ASSIGNMENT(=)
    !> @copybrief IntrType::assign_char_to_int
    !> @copydetails IntrType::assign_char_to_int
    MODULE PROCEDURE assign_char_to_int
    !> @copybrief IntrType::assign_char_to_bool
    !> @copydetails IntrType::assign_char_to_bool
    MODULE PROCEDURE assign_char_to_bool
    !> @copybrief IntrType::assign_char_to_single
    !> @copydetails IntrType::assign_char_to_single
    MODULE PROCEDURE assign_char_to_single
    !> @copybrief IntrType::assign_char_to_double
    !> @copydetails IntrType::assign_char_to_double
    MODULE PROCEDURE assign_char_to_double
    !> @copybrief IntrType::assign_char_to_string
    !> @copydetails IntrType::assign_char_to_string
!    MODULE PROCEDURE assign_char_to_string
!    !> @copybrief IntrType::assign_char_to_array_int
!    !> @copydetails IntrType::assign_char_to_array_int
!    MODULE PROCEDURE assign_char_to_array_int
!    !> @copybrief IntrType::assign_char_to_array_double
!    !> @copydetails IntrType::assign_char_to_array_double
!    MODULE PROCEDURE assign_char_to_array_double
!    !> @copybrief IntrType::assign_char_to_array_string
!    !> @copydetails IntrType::assign_char_to_array_string
!    MODULE PROCEDURE assign_char_to_array_string
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two single precision reals
!> with .APPROXEQ.
!> @param a
!> @param b
!> @returns bool logical indicating if a and b are approximately equal
!>
!> In this module single precision numbers are defined as having 6 digits of
!> precision. This function defines "approximately equals to" such that the
!> numbers are allowed to by vary exactly 1.0 or less in the 6th significant
!> digit.
!>
!> This function works by "printing" the first 14 digits of a real
!> and doing a string comparison to insure that the first 5 digits and the
!> exponent agree exactly and the 6th and 7th digit are then converted to
!> an integer and their absolute difference must then be less than or equal
!> to 10.
!>
    ELEMENTAL FUNCTION approxeq_single(a,b) RESULT(bool)
      REAL(SSK),INTENT(IN) :: a,b
      LOGICAL(SBK) :: bool
      CHARACTER(LEN=13) :: aString,bString
      CHARACTER(LEN=7) :: expString
      INTEGER(SNK) :: intA_left,intB_left,intA_exp,intB_exp,diffExp
      INTEGER(SLK) :: intA_right,intB_right
      REAL(SSK) :: tol

      !Convert to character variable
      WRITE(aString,'(es13.6E2)') a
      WRITE(bString,'(es13.6E2)') b

      !First digit that is left of decimal
      READ(aString(1:2),'(i2)') intA_left
      READ(bString(1:2),'(i2)') intB_left

      IF(intA_left == 0 .OR. intB_left == 0) THEN
        !Special case for 0, use absolute comparison
        bool=(ABS(a-b) <= EPSS)
      ELSE
        !Conver exponent and numbers right of decimal to integers
        READ(aString(4:9),'(i6)') intA_right
        READ(bString(4:9),'(i6)') intB_right
        READ(aString(11:13),'(i4)') intA_exp
        READ(bString(11:13),'(i4)') intB_exp
        diffExp=ABS(intA_exp-intB_exp)

        IF(diffExp < 2) THEN
          IF(diffExp == 0 .AND. intA_left == intB_left) THEN
            !exponents are the same and left of decimal numbers are same,
            !so compare sig figs to the right of the decimal
            bool=(ABS(intA_right-intB_right) < 15)
          ELSE
            !Exponent or number may have rolled over (e.g. 1.0 and 0.9)
            !Compute tolerance based on larger exponent
            WRITE(expString,'(a,sp,i3.2)') '1.0e',(MAX(intA_exp,intB_exp)-5)
            READ(expString,*) tol
            bool=(ABS(a-b) <= tol)
          ENDIF
        ELSE
          bool=.FALSE.
        ENDIF
      ENDIF
    ENDFUNCTION approxeq_single
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two double precision reals
!> with .APPROXEQ.
!> @param a
!> @param b
!> @returns bool logical indicating if a and b are approximately equal
!>
!> In this module double precision numbers are defined as having 15 digits of
!> precision. This function defines "approximately equals to" such that the
!> numbers are allowed to by vary exactly 1.0 or less in the 15th significant
!> digit.
!>
!> This function works by "printing" the first 16 digits of a real
!> and doing a string comparison to insure that the first 14 digits and the
!> exponent agree exactly and the 15th and 16th digit are then converted to
!> an integer and their absolute difference must then be less than or equal
!> to 10.
!>
    ELEMENTAL FUNCTION approxeq_double(a,b) RESULT(bool)
      REAL(SDK),INTENT(IN) :: a,b
      LOGICAL(SBK) :: bool
      CHARACTER(LEN=23) :: aString,bString
      CHARACTER(LEN=8) :: expString
      INTEGER(SNK) :: intA_left,intB_left,intA_exp,intB_exp,diffExp
      INTEGER(SLK) :: intA_right,intB_right
      REAL(SDK) :: tol

      !Convert to character variable
      WRITE(aString,'(es23.15E3)') a
      WRITE(bString,'(es23.15E3)') b

      !First digit that is left of decimal
      READ(aString(1:2),'(i2)') intA_left
      READ(bString(1:2),'(i2)') intB_left

      IF(intA_left == 0 .OR. intB_left == 0) THEN
        !Special case for 0, use absolute comparison
        bool=(ABS(a-b) <= EPSD)
      ELSE
        !Conver exponent and numbers right of decimal to integers
        READ(aString(4:18),'(i15)') intA_right
        READ(bString(4:18),'(i15)') intB_right
        READ(aString(20:23),'(i4)') intA_exp
        READ(bString(20:23),'(i4)') intB_exp
        diffExp=ABS(intA_exp-intB_exp)

        IF(diffExp < 2) THEN
          IF(diffExp == 0 .AND. intA_left == intB_left) THEN
            !exponents are the same and left of decimal numbers are same,
            !so compare sig figs to the right of the decimal
            bool=(ABS(intA_right-intB_right) < 15)
          ELSE
            !Exponent or number may have rolled over (e.g. 1.0 and 0.9)
            !Compute tolerance based on larger exponent
            WRITE(expString,'(a,sp,i4.3)') '1.0d',(MAX(intA_exp,intB_exp)-14)
            READ(expString,*) tol
            bool=(ABS(a-b) <= tol)
          ENDIF
        ELSE
          bool=.FALSE.
        ENDIF
      ENDIF
    ENDFUNCTION approxeq_double
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an absolute comparison of two
!> single precision reals with .APPROXEQA.
!> @param a
!> @param b
!> @returns bool logical indicating if a and b are approximately equal
!>
!> This routine just does a simple absolute comparison using an epsilon that is
!> a compile time constant. It should be used whenever possible because it has
!> the least overhead. However, it is not appropriate to use when @c a and @c b
!> are either very large or very small.
!>
    ELEMENTAL FUNCTION approxeq_abs_single(a,b) RESULT(bool)
      REAL(SSK),INTENT(IN) :: a,b
      LOGICAL(SBK) :: bool
      bool=(ABS(a-b) <= EPSS)
    ENDFUNCTION approxeq_abs_single
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an absolute comparison of two
!> double precision reals with .APPROXEQA.
!> @param a
!> @param b
!> @returns bool logical indicating if a and b are approximately equal
!>
!> This routine just does a simple absolute comparison using an epsilon that is
!> a compile time constant. It should be used whenever possible because it has
!> the least overhead. However, it is not appropriate to use when @c a and @c b
!> are either very large or very small.
!>
    ELEMENTAL FUNCTION approxeq_abs_double(a,b) RESULT(bool)
      REAL(SDK),INTENT(IN) :: a,b
      LOGICAL(SBK) :: bool
      bool=(ABS(a-b) <= EPSD)
    ENDFUNCTION approxeq_abs_double
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing a relative comparison of two
!> single precision reals with .APPROXEQA.
!> @param a
!> @param b
!> @returns bool logical indicating if a and b are approximately equal
!>
!> This performs a relative comparison by scaling the default epsilon value to
!> the size of the larger of the two. It should be used when @c and @b are of
!> the same magnitude and very large or very small. If either @c a or @c b is
!> zero (exactly) then this routine is equivalent to an absolute comparison.
!>
    ELEMENTAL FUNCTION approxeq_rel_single(a,b) RESULT(bool)
      REAL(SSK),INTENT(IN) :: a,b
      LOGICAL(SBK) :: bool
      REAL(SDK) :: eps
      eps=MAX(ABS(a),ABS(b))*EPSS
      IF(a == 0.0_SSK .OR. b == 0.0_SSK) eps=EPSS
      bool=(ABS(a-b) <= eps)
    ENDFUNCTION approxeq_rel_single
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing a relative comparison of two
!> double precision reals with .APPROXEQR.
!> @param a
!> @param b
!> @returns bool logical indicating if a and b are approximately equal
!>
!> This performs a relative comparison by scaling the default epsilon value to
!> the size of the larger of the two. It should be used when @c and @b are of
!> the same magnitude and very large or very small. If either @c a or @c b is
!> zero (exactly) then this routine is equivalent to an absolute comparison.
!>
    ELEMENTAL FUNCTION approxeq_rel_double(a,b) RESULT(bool)
      REAL(SDK),INTENT(IN) :: a,b
      LOGICAL(SBK) :: bool
      REAL(SDK) :: eps
      eps=MAX(ABS(a),ABS(b))*EPSD
      IF(a == 0.0_SDK .OR. b == 0.0_SDK) eps=EPSD
      bool=(ABS(a-b) <= eps)
    ENDFUNCTION approxeq_rel_double
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing a comparison of two single
!> precision reals on the floating point number line with .APPROXEQF.
!> @param a
!> @param b
!> @returns bool logical indicating if a and b are approximately equal
!>
!> This performs a comparison of the binary representation of the two reals
!> to compare the binary units in the last place (ULP). If the two reals differ
!> on the floating point number line by 10 or less representable floating point
!> reals then they are considered equal. In theory, this is the most appropriate
!> comparison to use, but will break down near zero.
!>
    ELEMENTAL FUNCTION approxeq_ulp_single(a,b) RESULT(bool)
      REAL(SSK),INTENT(IN) :: a,b
      LOGICAL(SBK) :: bool
      IF(a == 0.0_SSK .OR. b == 0.0_SSK .OR. (a > 0._SSK .AND. b < 0._SSK) &
                                        .OR. (a < 0._SSK .AND. b > 0._SSK)) THEN
        bool=approxeq_abs_single(a,b)
      ELSE
        bool=(ABS(TRANSFER(a,1_SNK)-TRANSFER(b,1_SNK)) <= 10_SNK)
      ENDIF
    ENDFUNCTION approxeq_ulp_single
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing a comparison of two double
!> precision reals on the floating point number line with .APPROXEQF.
!> @param a
!> @param b
!> @returns bool logical indicating if a and b are approximately equal
!>
!> This performs a comparison of the binary representation of the two reals
!> to compare the binary units in the last place (ULP). If the two reals differ
!> on the floating point number line by 10 or less representable floating point
!> reals then they are considered equal. In theory, this is the most appropriate
!> comparison to use, but will break down near zero.
!>
    ELEMENTAL FUNCTION approxeq_ulp_double(a,b) RESULT(bool)
      REAL(SDK),INTENT(IN) :: a,b
      LOGICAL(SBK) :: bool
      IF(a == 0.0_SDK .OR. b == 0.0_SDK .OR. (a > 0._SDK .AND. b < 0._SDK) &
                                        .OR. (a < 0._SDK .AND. b > 0._SDK)) THEN
        bool=approxeq_abs_double(a,b)
      ELSE
        bool=(ABS(TRANSFER(a,1_SLK)-TRANSFER(b,1_SLK)) <= 10_SLK)
      ENDIF
    ENDFUNCTION approxeq_ulp_double
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two single precision reals
!> with .APPROXLE.
!> @param r1 a single precision real number
!> @param r2 a single precision real number
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION approxle_single(r1,r2) RESULT(bool)
      REAL(SSK),INTENT(IN) :: r1
      REAL(SSK),INTENT(IN) :: r2
      LOGICAL(SBK) :: bool
      bool=(r1 <= r2+EPSS)
    ENDFUNCTION approxle_single
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two double precision reals
!> with .APPROXLE.
!> @param r1 a double precision real number
!> @param r2 a double precision real number
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION approxle_double(r1,r2) RESULT(bool)
      REAL(SDK),INTENT(IN) :: r1
      REAL(SDK),INTENT(IN) :: r2
      LOGICAL(SBK) :: bool
      bool=(r1 <= r2+EPSD)
    ENDFUNCTION approxle_double
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two single precision reals
!> with .APPROXGE.
!> @param r1 a single precision real number
!> @param r2 a single precision real number
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION approxge_single(r1,r2) RESULT(bool)
      REAL(SSK),INTENT(IN) :: r1
      REAL(SSK),INTENT(IN) :: r2
      LOGICAL(SBK) :: bool
      bool=(r1+EPSS >= r2)
    ENDFUNCTION approxge_single
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two double precision reals
!> with .APPROXGE.
!> @param r1 a double precision real number
!> @param r2 a double precision real number
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION approxge_double(r1,r2) RESULT(bool)
      REAL(SDK),INTENT(IN) :: r1
      REAL(SDK),INTENT(IN) :: r2
      LOGICAL(SBK) :: bool
      bool=(r1+EPSD >= r2)
    ENDFUNCTION approxge_double
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two single precision reals
!> with SOFTEQ
!> @param r1 a single precision real number
!> @param r2 a single precision real number
!> @param tol a single precision real number
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION softeq_single(r1,r2,tol) RESULT(bool)
      REAL(SSK),INTENT(IN) :: r1
      REAL(SSK),INTENT(IN) :: r2
      REAL(SSK),INTENT(IN) :: tol
      LOGICAL(SBK) :: bool
      bool=(ABS(r1 - r2) <= tol)
    ENDFUNCTION softeq_single
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two double precision reals
!> with SOFTEQ
!> @param r1 a double precision real number
!> @param r2 a double precision real number
!> @param tol a double precision real number
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION softeq_double(r1,r2,tol) RESULT(bool)
      REAL(SDK),INTENT(IN) :: r1
      REAL(SDK),INTENT(IN) :: r2
      REAL(SDK),INTENT(IN) :: tol
      LOGICAL(SBK) :: bool
      bool=(ABS(r1 - r2) <= tol)
    ENDFUNCTION softeq_double
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two single precision reals
!> with SOFTEQR
!> @param r1 a single precision real number
!> @param r2 a single precision real number
!> @param tol a single precision real number
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION softeqr_single(r1,r2,tol) RESULT(bool)
      REAL(SSK),INTENT(IN) :: r1
      REAL(SSK),INTENT(IN) :: r2
      REAL(SSK),INTENT(IN) :: tol
      LOGICAL(SBK) :: bool
      REAL(SSK) :: eps
      eps=MAX(ABS(r1),ABS(r2))*tol
      IF(r1 == 0.0_SSK .OR. r1 == 0.0_SSK) eps=REAL(EPSS,SSK)
      bool=(ABS(r1-r2) <= eps)
    ENDFUNCTION softeqr_single
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two double precision reals
!> with SOFTEQR
!> @param r1 a double precision real number
!> @param r2 a double precision real number
!> @param tol a double precision real number
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION softeqr_double(r1,r2,tol) RESULT(bool)
      REAL(SDK),INTENT(IN) :: r1
      REAL(SDK),INTENT(IN) :: r2
      REAL(SDK),INTENT(IN) :: tol
      LOGICAL(SBK) :: bool
      REAL(SDK) :: eps
      eps=MAX(ABS(r1),ABS(r2))*tol
      IF(r1 == 0.0_SSK .OR. r1 == 0.0_SSK) eps=EPSD
      bool=(ABS(r1-r2) <= eps)
    ENDFUNCTION softeqr_double
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two single precision reals
!> with SOFTLE
!> @param r1 a single precision real number
!> @param r2 a single precision real number
!> @param tol a single precision real number
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION softle_single(r1,r2,tol) RESULT(bool)
      REAL(SSK),INTENT(IN) :: r1
      REAL(SSK),INTENT(IN) :: r2
      REAL(SSK),INTENT(IN) :: tol
      LOGICAL(SBK) :: bool
      bool=(r1 <= r2+tol)
    ENDFUNCTION softle_single
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two double precision reals
!> with SOFTLE
!> @param r1 a double precision real number
!> @param r2 a double precision real number
!> @param tol a double precision real number
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION softle_double(r1,r2,tol) RESULT(bool)
      REAL(SDK),INTENT(IN) :: r1
      REAL(SDK),INTENT(IN) :: r2
      REAL(SDK),INTENT(IN) :: tol
      LOGICAL(SBK) :: bool
      bool=(r1 <= r2+tol)
    ENDFUNCTION softle_double
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two single precision reals
!> with SOFTGE
!> @param r1 a single precision real number
!> @param r2 a single precision real number
!> @param tol a single precision real number
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION softge_single(r1,r2,tol) RESULT(bool)
      REAL(SSK),INTENT(IN) :: r1
      REAL(SSK),INTENT(IN) :: r2
      REAL(SSK),INTENT(IN) :: tol
      LOGICAL(SBK) :: bool
      bool=(r1+tol >= r2)
    ENDFUNCTION softge_single
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two double precision reals
!> with SOFTGE
!> @param r1 a double precision real number
!> @param r2 a double precision real number
!> @param tol a double precision real number
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION softge_double(r1,r2,tol) RESULT(bool)
      REAL(SDK),INTENT(IN) :: r1
      REAL(SDK),INTENT(IN) :: r2
      REAL(SDK),INTENT(IN) :: tol
      LOGICAL(SBK) :: bool
      bool=(r1+tol >= r2)
    ENDFUNCTION softge_double
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation to determine if number is NAN
!> @param x a single precision real number
!> @returns @c bool result of comparison
!>
    FUNCTION isnan_single(x) RESULT(bool)
      REAL(SSK),INTENT(IN) :: x
      LOGICAL(SBK) :: bool
      INTEGER(C_INT) :: c_bool
      bool=.FALSE.
      c_bool=isNAN_float_c(REAL(x,C_FLOAT))
      IF(c_bool == 1) bool=.TRUE.
    ENDFUNCTION isnan_single
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation to determine if number is NAN
!> @param x a double precision real number
!> @returns @c bool result of comparison
!>
    FUNCTION isnan_double(x) RESULT(bool)
      REAL(SDK),INTENT(IN) :: x
      LOGICAL(SBK) :: bool
      INTEGER(C_INT) :: c_bool
      bool=.FALSE.
      c_bool=isNAN_double_c(REAL(x,C_DOUBLE))
      IF(c_bool == 1) bool=.TRUE.
    ENDFUNCTION isnan_double
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation to determine if number is INF
!> @param x a single precision real number
!> @returns @c bool result of comparison
!>
    FUNCTION isinf_single(x) RESULT(bool)
      REAL(SSK),INTENT(IN) :: x
      LOGICAL(SBK) :: bool
      INTEGER(C_INT) :: c_bool
      bool=.FALSE.
      c_bool=isINF_float_c(REAL(x,C_FLOAT))
      IF(c_bool == 1) bool=.TRUE.
    ENDFUNCTION isinf_single
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation to determine if number is INF
!> @param x a double precision real number
!> @returns @c bool result of comparison
!>
    FUNCTION isINF_double(x) RESULT(bool)
      REAL(SDK),INTENT(IN) :: x
      LOGICAL(SBK) :: bool
      INTEGER(C_INT) :: c_bool
      bool=.FALSE.
      c_bool=isINF_double_c(REAL(x,C_DOUBLE))
      IF(c_bool == 1) bool=.TRUE.
    ENDFUNCTION isinf_double
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two logical variables
!> @param l1 a logical of type SBK
!> @param l2 a logical of type SBK
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION equalto_logical(l1,l2) RESULT(bool)
      LOGICAL(SBK),INTENT(IN) :: l1
      LOGICAL(SBK),INTENT(IN) :: l2
      LOGICAL(SBK) :: bool
      bool=(l1 .EQV. l2)
    ENDFUNCTION equalto_logical
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two logical variables
!> @param l1 a logical of type SBK
!> @param l2 a logical of type SBK
!> @returns @c bool result of comparison
!>
    ELEMENTAL FUNCTION notequalto_logical(l1,l2) RESULT(bool)
      LOGICAL(SBK),INTENT(IN) :: l1
      LOGICAL(SBK),INTENT(IN) :: l2
      LOGICAL(SBK) :: bool
      bool=(l1 .NEQV. l2)
    ENDFUNCTION notequalto_logical
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of a string to an
!> integer with the = operator
!> @param i the integer value
!> @param c the character value
!> @returns i integer value of c
!>
!> This assigns an int with a value held in a string
!>
    ELEMENTAL SUBROUTINE assign_char_to_int(i,c)
      INTEGER(SNK),INTENT(OUT) :: i
      CHARACTER(LEN=*),INTENT(IN) :: c
      READ(c, '(I12)') i
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of a string to a
!> logical variable
!> @param l the logical value
!> @param c the character value
!> @returns s the logical value of c
!>
    ELEMENTAL SUBROUTINE assign_char_to_bool(b,c)
      LOGICAL(SBK),INTENT(OUT) :: b
      CHARACTER(LEN=*),INTENT(IN) :: c
      IF(c == 'true') THEN
        b = .TRUE.
      ELSE
        b = .FALSE.
      ENDIF
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of a string to a
!> single precision real variable
!> @param s the real value
!> @param c the character value
!> @returns s the logical value of c
!>
    ELEMENTAL SUBROUTINE assign_char_to_single(s,c)
      REAL(SSK),INTENT(OUT) :: s
      CHARACTER(LEN=*),INTENT(IN) :: c
      INTEGER(SNK) :: tmpInt

      READ(c, '(f19.0)') s
    ENDSUBROUTINE

!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of a string to a
!> double precision real variable
!> @param d the real value
!> @param c the character value
!> @returns d the logical value of c
!>
    ELEMENTAL SUBROUTINE assign_char_to_double(d,c)
      REAL(SDK),INTENT(OUT) :: d
      CHARACTER(LEN=*),INTENT(IN) :: c
      INTEGER(SLK) :: tmpInt

      READ(c, '(d35.0)') d
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of a character
!> string to an array of integers
!> @param iArr the array of integers
!> @param c the character value
!TODO arrays
    ELEMENTAL SUBROUTINE assign_char_to_array_int(iArr,c)
      INTEGER(SIK),INTENT(OUT) :: iArr
      CHARACTER(LEN=*),INTENT(IN) :: c
      CHARACTER(LEN=50) :: tmpStr
      INTEGER(SIK) :: tmpInt
      INTEGER(SIK) :: i,j,k

      j=1
      k=1 ! iArr index
      DO i=2,LEN(c)-1
        IF(c(i:i) /= ',') THEN
          tmpStr(j:j)=c(i:i)
          j=j+1
        ELSE
          tmpStr=tmpStr(1:j)
          READ(tmpStr, '(I12)') tmpInt
          !iArr(k:k)=tmpInt
          !Read string into integer
          !How to have it only read a certain amount?
          j=1
          k=k+1
        ENDIF
      ENDDO
      !for char : c
      !  if char != ','
      !    push to tmpStr
      !  else
      !    convert tmpStr to int
      !    add int to iArr
      !end
!      READ(c, '(i16)') iArr
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of a character
!> string to an array of doubles
!> @param d the array of doubles
!> @param c the character value
!TODO arrays
    ELEMENTAL SUBROUTINE assign_char_to_array_double(dArr,c)
      REAL(SDK),INTENT(OUT) :: dArr
      CHARACTER(LEN=*),INTENT(IN) :: c
!      READ(c, '(i16)') dArr
    ENDSUBROUTINE

!    ELEMENTAL FUNCTION assign_char_to_array_int(iArr,c) RESULT(iArr)
!    ELEMENTAL FUNCTION assign_char_to_single_arr(sArr,c) RESULT(sArr)
!    ELEMENTAL FUNCTION assign_char_to_double_arr(dArr,c) RESULT(dArr)
!    ELEMENTAL FUNCTION assign_char_to_string_arr(dArr,c) RESULT(dArr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    FUNCTION countArrayElts(charArr) RESULT(numElts)
      INTEGER(SIK) :: numElts
      CHARACTER(LEN=*),INTENT(IN) :: charArr
      INTEGER(SIK) :: i

      numElts=0
      !If length is 2, array is empty
      IF(LEN(charArr) /= 2) THEN
        DO i=2,LEN(charArr)-1
          IF(charArr(i:i) == ',') THEN
            numElts=numElts+1
          ENDIF
        ENDDO
        numElts=numElts+1
      ENDIF
    ENDFUNCTION countArrayElts
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of a character
!> string to an array of integers
!> @param iArr the array of integers
!> @param c the character value
    SUBROUTINE char_to_int_array(iArr,c)
      INTEGER(SIK),ALLOCATABLE,INTENT(OUT) :: iArr(:)
      CHARACTER(LEN=*),INTENT(IN) :: c
      CHARACTER(LEN=50) :: tmpStr
      INTEGER(SIK) :: tmpInt
      INTEGER(SIK) :: numElts
      INTEGER(SIK) :: i,j,k,commas

      numElts=countArrayElts(c)
      !Empty array case
      IF(numElts == 0) THEN
        RETURN
      ENDIF

      j=0
      k=1 ! iArr index
      ALLOCATE(iArr(numElts))
      DO i=2,LEN(c)
        IF(c(i:i) /= ',' .AND. c(i:i) /= '}') THEN
          j=j+1
          tmpStr(j:j)=c(i:i)
        ELSE
          tmpStr=tmpStr(1:j)
          READ(tmpStr, '(I12)') tmpInt
          iArr(k:k)=tmpInt
          j=0
          k=k+1
        ENDIF
      ENDDO
    ENDSUBROUTINE
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation for performing an assignment of a character
!> string to an array of doubles
!> @param dArr the array of doubles
!> @param c the character value
    SUBROUTINE char_to_double_array(dArr,c)
      REAL(SDK),ALLOCATABLE,INTENT(OUT) :: dArr(:)
      CHARACTER(LEN=*),INTENT(IN) :: c
      CHARACTER(LEN=50) :: tmpStr
      REAL(SDK) :: tmpDouble
      INTEGER(SIK) :: numElts
      INTEGER(SIK) :: i,j,k

      numElts=countArrayElts(c)
      !Empty array case
      IF(numElts == 0) THEN
        RETURN
      ENDIF

      j=0
      k=1 ! dArr index
      ALLOCATE(dArr(numElts))
      DO i=2,LEN(c)
        IF(c(i:i) /= ',' .AND. c(i:i) /= '}') THEN
          j=j+1
          tmpStr(j:j)=c(i:i)
        ELSE
          tmpStr=tmpStr(1:j)
          READ(tmpStr, '(F35.0)') tmpDouble
          dArr(k:k)=tmpDouble
          j=0
          k=k+1
        ENDIF
      ENDDO
    ENDSUBROUTINE char_to_double_array
!
ENDMODULE IntrType

