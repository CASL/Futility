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
!>
!> @todo
!> - Add support for Fortran 2003 C-interoperable types.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE IntrType
      
  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: SBK,SNK,SLK,SIK,SHK,SSK,SDK,SRK
  PUBLIC :: EPSS
  PUBLIC :: EPSD
  PUBLIC :: EPSREAL
  PUBLIC :: OPERATOR(.APPROXEQ.)
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
  
  !> @brief Significant digits for half precision.
  INTEGER,PARAMETER :: N_HLF_DIGITS=3
  
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
  
  !> @brief REAL kind for half precision (16-bit)
  !>
  !> The selected real kind ensuring N_HLF_DIGITS precision on a given 
  !> machine. SHK is now an acronym for Selected Half Kind; the number
  !> of bytes of memory occupied by a half precision floating point
  !> variable.
  INTEGER,PARAMETER :: SHK=SELECTED_REAL_KIND(N_HLF_DIGITS)
  
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
  !> types
  INTERFACE OPERATOR(.APPROXEQ.)
    !> @copybrief IntrType::approxeq_single
    MODULE PROCEDURE approxeq_single
    !> @copybrief IntrType::approxeq_double
    MODULE PROCEDURE approxeq_double
  ENDINTERFACE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two single precision reals
!> with .APPROXEQ.
!> @param r1 a single precision real number
!> @param r2 a single precision real number
!> @returns @c bool result of comparison
    ELEMENTAL FUNCTION approxeq_single(r1,r2) RESULT(bool)
      REAL(SSK),INTENT(IN) :: r1
      REAL(SSK),INTENT(IN) :: r2
      LOGICAL(SBK) :: bool
      bool=(ABS(r1 - r2) <= EPSS)
    ENDFUNCTION approxeq_single
!
!-------------------------------------------------------------------------------
!> @brief Defines the operation when comparing two double precision reals
!> with .APPROXEQ.
!> @param r1 a double precision real number
!> @param r2 a double precision real number
!> @returns @c bool result of comparison
    ELEMENTAL FUNCTION approxeq_double(r1,r2) RESULT(bool)
      REAL(SDK),INTENT(IN) :: r1
      REAL(SDK),INTENT(IN) :: r2
      LOGICAL(SBK) :: bool
      bool=(ABS(r1 - r2) <= EPSD)
    ENDFUNCTION approxeq_double

ENDMODULE IntrType
