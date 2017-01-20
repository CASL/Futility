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
!> @brief A utility module providing an interface to Level 1 BLAS functionality.
!>
!> This module provides simplified generic interfaces to level 1 BLAS routines.
!> It only supports the operations for single precision and double precision
!> types. In general it provides simplified interfaces by making some input
!> arguments optional. It provides extensions to the BLAS routines usually in
!> the form of allowing for a scalar input argument to be a vector (e.g._axpy)
!> or by providing a complementary operation (e.g. i_amin).
!>
!> The following interfaces are provided:
!>  - @ref BLAS1::BLAS_asum "BLAS_asum": @copybrief BLAS1::BLAS_asum
!>  - @ref BLAS1::BLAS_axpy "BLAS_axpy": @copybrief BLAS1::BLAS_axpy
!>  - @ref BLAS1::BLAS_copy "BLAS_copy": @copybrief BLAS1::BLAS_copy
!>  - @ref BLAS1::BLAS_dot "BLAS_dot": @copybrief BLAS1::BLAS_dot
!>  - @ref BLAS1::BLAS_iamax "BLAS_iamax": @copybrief BLAS1::BLAS_iamax
!>  - @ref BLAS1::BLAS_iamin "BLAS_iamin": @copybrief BLAS1::BLAS_iamin
!>  - @ref BLAS1::BLAS_nrm2 "BLAS_nrm2": @copybrief BLAS1::BLAS_nrm2
!>  - @ref BLAS1::BLAS_scal "BLAS_scal": @copybrief BLAS1::BLAS_scal
!>  - @ref BLAS1::BLAS_swap "BLAS_asum": @copybrief BLAS1::BLAS_swap
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>
!> @par EXAMPLES
!> @code
!> @endcode
!>
!> @author Brendan Kochunas
!>    @date 03/08/2012
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE BLAS1

  USE IntrType
  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: BLAS_asum
  PUBLIC :: BLAS_axpy
  PUBLIC :: BLAS_copy
  PUBLIC :: BLAS_dot
  PUBLIC :: BLAS_iamax
  PUBLIC :: BLAS_iamin
  PUBLIC :: BLAS_nrm2
  PUBLIC :: BLAS_scal
  PUBLIC :: BLAS_swap
  
  !> @brief Generic interface to the Level 1 BLAS routine _asum
  !>
  !> The _asum routine computes the sum of the absolute values of a vector.
  !>
  !> The generic interface supports passing of single precision or double
  !> precision vectors and the @c N and @c INCX arguments are optional. These
  !> routines are resolved at compile time.
  !>
  INTERFACE BLAS_asum
    !> @copybrief BLAS1::sasum_all
    !> @copydetails BLAS1::sasum_all
    MODULE PROCEDURE sasum_all
    !> @copybrief BLAS1::sasum_noINCX
    !> @copydetails BLAS1::sasum_noINCX
    MODULE PROCEDURE sasum_noINCX
    !> @copybrief BLAS1::sasum_noN
    !> @copydetails BLAS1::sasum_noN
    MODULE PROCEDURE sasum_noN
    !> @copybrief BLAS1::sasum_onlyX
    !> @copydetails BLAS1::sasum_onlyX
    MODULE PROCEDURE sasum_onlyX
    !> @copybrief BLAS1::dasum_all
    !> @copydetails BLAS1::dasum_all
    MODULE PROCEDURE dasum_all
    !> @copybrief BLAS1::dasum_noINCX
    !> @copydetails BLAS1::dasum_noINCX
    MODULE PROCEDURE dasum_noINCX
    !> @copybrief BLAS1::dasum_noN
    !> @copydetails BLAS1::dasum_noN
    MODULE PROCEDURE dasum_noN
    !> @copybrief BLAS1::dasum_onlyX
    !> @copydetails BLAS1::dasum_onlyX
    MODULE PROCEDURE dasum_onlyX
  ENDINTERFACE BLAS_asum
  
  !> @brief Generic interface to the Level 1 BLAS routine _axpy
  !>
  !> _axpy performs the operation y <- a*x+y
  !>
  !> The generic interface supports passing of single precision or 
  !> double precision vectors and the @c N, @c A, @c INCX and @c INCY arguments
  !> are all made to be optional. The functionality is also extended such that
  !> @c A may be a vector the same size as @c X. These routines are resolved at
  !> compile time.
  !>
  INTERFACE BLAS_axpy
    !> @copybrief BLAS1::saxpy_all
    !> @copydetails BLAS1::saxpy_all
    MODULE PROCEDURE saxpy_all
    !> @copybrief BLAS1::saxpy_naxyinc
    !> @copydetails BLAS1::saxpy_naxyinc
    MODULE PROCEDURE saxpy_naxyinc
    !> @copybrief BLAS1::saxpy_axyinc
    !> @copydetails BLAS1::saxpy_axyinc
    MODULE PROCEDURE saxpy_axyinc
    !> @copybrief BLAS1::saxpy_naxy
    !> @copydetails BLAS1::saxpy_naxy
    MODULE PROCEDURE saxpy_naxy
    !> @copybrief BLAS1::saxpy_axy
    !> @copydetails BLAS1::saxpy_axy
    MODULE PROCEDURE saxpy_axy
    !> @copybrief BLAS1::saxpy_vecA_all
    !> @copydetails BLAS1::saxpy_vecA_all
    MODULE PROCEDURE saxpy_vecA_all
    !> @copybrief BLAS1::saxpy_vecA_naxyinc
    !> @copydetails BLAS1::saxpy_vecA_naxyinc
    MODULE PROCEDURE saxpy_vecA_naxyinc
    !> @copybrief BLAS1::saxpy_vecA_axyinc
    !> @copydetails BLAS1::saxpy_vecA_axyinc
    MODULE PROCEDURE saxpy_vecA_axyinc
    !> @copybrief BLAS1::saxpy_vecA_naxy
    !> @copydetails BLAS1::saxpy_vecA_naxy
    MODULE PROCEDURE saxpy_vecA_naxy
    !> @copybrief BLAS1::saxpy_vecA_axy
    !> @copydetails BLAS1::saxpy_vecA_axy
    MODULE PROCEDURE saxpy_vecA_axy
    !> @copybrief BLAS1::saxpy_nxyincxy
    !> @copydetails BLAS1::saxpy_nxyincxy
    MODULE PROCEDURE saxpy_nxyincxy
    !> @copybrief BLAS1::saxpy_nxyinc
    !> @copydetails BLAS1::saxpy_nxyinc
    MODULE PROCEDURE saxpy_nxyinc
    !> @copybrief BLAS1::saxpy_nxy
    !> @copydetails BLAS1::saxpy_nxy
    MODULE PROCEDURE saxpy_nxy
    !> @copybrief BLAS1::saxpy_xy
    !> @copydetails BLAS1::saxpy_xy
    MODULE PROCEDURE saxpy_xy
    !> @copybrief BLAS1::daxpy_all
    !> @copydetails BLAS1::daxpy_all
    MODULE PROCEDURE daxpy_all
    !> @copybrief BLAS1::daxpy_naxyinc
    !> @copydetails BLAS1::daxpy_naxyinc
    MODULE PROCEDURE daxpy_naxyinc
    !> @copybrief BLAS1::daxpy_axyinc
    !> @copydetails BLAS1::daxpy_axyinc
    MODULE PROCEDURE daxpy_axyinc
    !> @copybrief BLAS1::daxpy_naxy
    !> @copydetails BLAS1::daxpy_naxy
    MODULE PROCEDURE daxpy_naxy
    !> @copybrief BLAS1::daxpy_axy
    !> @copydetails BLAS1::daxpy_axy
    MODULE PROCEDURE daxpy_axy
    !> @copybrief BLAS1::daxpy_vecA_all
    !> @copydetails BLAS1::daxpy_vecA_all
    MODULE PROCEDURE daxpy_vecA_all
    !> @copybrief BLAS1::daxpy_vecA_naxyinc
    !> @copydetails BLAS1::daxpy_vecA_naxyinc
    MODULE PROCEDURE daxpy_vecA_naxyinc
    !> @copybrief BLAS1::daxpy_vecA_axyinc
    !> @copydetails BLAS1::daxpy_vecA_axyinc
    MODULE PROCEDURE daxpy_vecA_axyinc
    !> @copybrief BLAS1::daxpy_vecA_naxy
    !> @copydetails BLAS1::daxpy_vecA_naxy
    MODULE PROCEDURE daxpy_vecA_naxy
    !> @copybrief BLAS1::daxpy_vecA_axy
    !> @copydetails BLAS1::daxpy_vecA_axy
    MODULE PROCEDURE daxpy_vecA_axy
    !> @copybrief BLAS1::daxpy_nxyincxy
    !> @copydetails BLAS1::daxpy_nxyincxy
    MODULE PROCEDURE daxpy_nxyincxy
    !> @copybrief BLAS1::daxpy_nxyinc
    !> @copydetails BLAS1::daxpy_nxyinc
    MODULE PROCEDURE daxpy_nxyinc
    !> @copybrief BLAS1::daxpy_nxy
    !> @copydetails BLAS1::daxpy_nxy
    MODULE PROCEDURE daxpy_nxy
    !> @copybrief BLAS1::daxpy_xy
    !> @copydetails BLAS1::daxpy_xy
    MODULE PROCEDURE daxpy_xy
  ENDINTERFACE BLAS_axpy
  
  !> @brief Generic interface to the Level 1 BLAS routine _copy
  !>
  !> _copy performs the operation y <- x
  !>
  !> The generic interface supports passing of single precision or double
  !> precision vectors and the @c N, @c INCX, @c INCY arguments are made to be
  !> optional. These routines are resolved at compile time.
  !>
  INTERFACE BLAS_copy
    !> @copybrief BLAS1::scopy_all
    !> @copydetails BLAS1::scopy_all
    MODULE PROCEDURE scopy_all
    !> @copybrief BLAS1::scopy_noN
    !> @copydetails BLAS1::scopy_noN
    MODULE PROCEDURE scopy_noN
    !> @copybrief BLAS1::scopy_nxyinc
    !> @copydetails BLAS1::scopy_nxyinc
    MODULE PROCEDURE scopy_nxyinc
    !> @copybrief BLAS1::scopy_xyinc
    !> @copydetails BLAS1::scopy_xyinc
    MODULE PROCEDURE scopy_xyinc
    !> @copybrief BLAS1::scopy_nxy
    !> @copydetails BLAS1::scopy_nxy
    MODULE PROCEDURE scopy_nxy
    !> @copybrief BLAS1::scopy_xy
    !> @copydetails BLAS1::scopy_xy
    MODULE PROCEDURE scopy_xy
    !> @copybrief BLAS1::dcopy_all
    !> @copydetails BLAS1::dcopy_all
    MODULE PROCEDURE dcopy_all
    !> @copybrief BLAS1::dcopy_noN
    !> @copydetails BLAS1::dcopy_noN
    MODULE PROCEDURE dcopy_noN
    !> @copybrief BLAS1::dcopy_nxyinc
    !> @copydetails BLAS1::dcopy_nxyinc
    MODULE PROCEDURE dcopy_nxyinc
    !> @copybrief BLAS1::dcopy_xyinc
    !> @copydetails BLAS1::dcopy_xyinc
    MODULE PROCEDURE dcopy_xyinc
    !> @copybrief BLAS1::dcopy_nxy
    !> @copydetails BLAS1::dcopy_nxy
    MODULE PROCEDURE dcopy_nxy
    !> @copybrief BLAS1::dcopy_xy
    !> @copydetails BLAS1::dcopy_xy
    MODULE PROCEDURE dcopy_xy
  ENDINTERFACE BLAS_copy
  
  !> @brief Generic interface to the Level 1 BLAS routine _dot
  !>
  !> _dot performs the operation dot <- (x^T)*y
  !>
  !> The generic interface supports passing of single precision or double
  !> precision vectors and the @c N, @c INCX, @c INCY arguments are made to be
  !> optional. These routines are resolved at compile time.
  !>
  INTERFACE BLAS_dot
    !> @copybrief BLAS1::sdot_all
    !> @copydetails BLAS1::sdot_all
    MODULE PROCEDURE sdot_all
    !> @copybrief BLAS1::sdot_noN
    !> @copydetails BLAS1::sdot_noN
    MODULE PROCEDURE sdot_noN
    !> @copybrief BLAS1::sdot_nxyinc
    !> @copydetails BLAS1::sdot_nxyinc
    MODULE PROCEDURE sdot_nxyinc
    !> @copybrief BLAS1::sdot_nxy
    !> @copydetails BLAS1::sdot_nxy
    MODULE PROCEDURE sdot_nxy
    !> @copybrief BLAS1::sdot_xyinc
    !> @copydetails BLAS1::sdot_xyinc
    MODULE PROCEDURE sdot_xyinc
    !> @copybrief BLAS1::sdot_xy
    !> @copydetails BLAS1::sdot_xy
    MODULE PROCEDURE sdot_xy
    !> @copybrief BLAS1::ddot_all
    !> @copydetails BLAS1::ddot_all
    MODULE PROCEDURE ddot_all
    !> @copybrief BLAS1::ddot_noN
    !> @copydetails BLAS1::ddot_noN
    MODULE PROCEDURE ddot_noN
    !> @copybrief BLAS1::ddot_nxyinc
    !> @copydetails BLAS1::ddot_nxyinc
    MODULE PROCEDURE ddot_nxyinc
    !> @copybrief BLAS1::ddot_nxy
    !> @copydetails BLAS1::ddot_nxy
    MODULE PROCEDURE ddot_nxy
    !> @copybrief BLAS1::ddot_xyinc
    !> @copydetails BLAS1::ddot_xyinc
    MODULE PROCEDURE ddot_xyinc
    !> @copybrief BLAS1::ddot_xy
    !> @copydetails BLAS1::ddot_xy
    MODULE PROCEDURE ddot_xy
  ENDINTERFACE BLAS_dot
  
  !> @brief Generic interface to the Level 1 BLAS routine i_amax
  !>
  !> The i_amax routine returns the index of the absolute maximum of a vector.
  !>
  !> The generic interface supports passing of single precision or double
  !> precision vectors and the @c N and @c INCX arguments are optional. These
  !> routines are resolved at compile time.
  !>
  INTERFACE BLAS_iamax
    !> @copybrief BLAS1::isamax_all
    !> @copydetails BLAS1::isamax_all
    MODULE PROCEDURE isamax_all
    !> @copybrief BLAS1::isamax_noN
    !> @copydetails BLAS1::isamax_noN
    MODULE PROCEDURE isamax_noN
    !> @copybrief BLAS1::isamax_noINCX
    !> @copydetails BLAS1::isamax_noINCX
    MODULE PROCEDURE isamax_noINCX
    !> @copybrief BLAS1::isamax_onlyX
    !> @copydetails BLAS1::isamax_onlyX
    MODULE PROCEDURE isamax_onlyX
    !> @copybrief BLAS1::idamax_all
    !> @copydetails BLAS1::idamax_all
    MODULE PROCEDURE idamax_all
    !> @copybrief BLAS1::idamax_noN
    !> @copydetails BLAS1::idamax_noN
    MODULE PROCEDURE idamax_noN
    !> @copybrief BLAS1::idamax_noINCX
    !> @copydetails BLAS1::idamax_noINCX
    MODULE PROCEDURE idamax_noINCX
    !> @copybrief BLAS1::idamax_onlyX
    !> @copydetails BLAS1::idamax_onlyX
    MODULE PROCEDURE idamax_onlyX
  ENDINTERFACE BLAS_iamax
  
  !> @brief Generic interface for an extension to the Level 1 BLAS routines
  !>
  !> The i_amin routine returns the index of the absolute minimum of a vector.
  !>
  !> The generic interface supports passing of single precision or double
  !> precision vectors and the @c N and @c INCX arguments are optional. These
  !> routines are resolved at compile time.
  !>
  INTERFACE BLAS_iamin
    !> @copybrief BLAS1::isamin_all
    !> @copydetails BLAS1::isamin_all
    MODULE PROCEDURE isamin_all
    !> @copybrief BLAS1::isamin_noN
    !> @copydetails BLAS1::isamin_noN
    MODULE PROCEDURE isamin_noN
    !> @copybrief BLAS1::isamin_noINCX
    !> @copydetails BLAS1::isamin_noINCX
    MODULE PROCEDURE isamin_noINCX
    !> @copybrief BLAS1::isamin_onlyX
    !> @copydetails BLAS1::isamin_onlyX
    MODULE PROCEDURE isamin_onlyX
    !> @copybrief BLAS1::idamin_all
    !> @copydetails BLAS1::idamin_all
    MODULE PROCEDURE idamin_all
    !> @copybrief BLAS1::idamin_noN
    !> @copydetails BLAS1::idamin_noN
    MODULE PROCEDURE idamin_noN
    !> @copybrief BLAS1::idamin_noINCX
    !> @copydetails BLAS1::idamin_noINCX
    MODULE PROCEDURE idamin_noINCX
    !> @copybrief BLAS1::idamin_onlyX
    !> @copydetails BLAS1::idamin_onlyX
    MODULE PROCEDURE idamin_onlyX
  ENDINTERFACE BLAS_iamin
  
  !> @brief Generic interface to the Level 1 BLAS routine _nrm2
  !>
  !> The _nrm2 routine computes the 2-norm of a vector.
  !>
  !> The generic interface supports passing of single precision or double
  !> precision vectors and the @c N and @c INCX arguments are optional. These
  !> routines are resolved at compile time.
  !>
  INTERFACE BLAS_nrm2
    !> @copybrief BLAS1::snrm2_all
    !> @copydetails BLAS1::snrm2_all
    MODULE PROCEDURE snrm2_all
    !> @copybrief BLAS1::snrm2_noN
    !> @copydetails BLAS1::snrm2_noN
    MODULE PROCEDURE snrm2_noN
    !> @copybrief BLAS1::snrm2_noINCX
    !> @copydetails BLAS1::snrm2_noINCX
    MODULE PROCEDURE snrm2_noINCX
    !> @copybrief BLAS1::snrm2_onlyX
    !> @copydetails BLAS1::snrm2_onlyX
    MODULE PROCEDURE snrm2_onlyX
    !> @copybrief BLAS1::dnrm2_all
    !> @copydetails BLAS1::dnrm2_all
    MODULE PROCEDURE dnrm2_all
    !> @copybrief BLAS1::dnrm2_noN
    !> @copydetails BLAS1::dnrm2_noN
    MODULE PROCEDURE dnrm2_noN
    !> @copybrief BLAS1::dnrm2_noINCX
    !> @copydetails BLAS1::dnrm2_noINCX
    MODULE PROCEDURE dnrm2_noINCX
    !> @copybrief BLAS1::dnrm2_onlyX
    !> @copydetails BLAS1::dnrm2_onlyX
    MODULE PROCEDURE dnrm2_onlyX
  ENDINTERFACE BLAS_nrm2
  
  !> @brief Generic interface to the Level 1 BLAS routine _scal
  !>
  !> _scal performs the operation x <- a*x
  !>
  !> The generic interface supports passing of single precision or double
  !> precision vectors and the @c N and @c INCX arguments are made to be
  !> optional. The functionality is also extended such that @c A may be a vector
  !> the same size as @c X. These routines are resolved at compile time.
  !>
  INTERFACE BLAS_scal
    !> @copybrief BLAS1::sscal_all
    !> @copydetails BLAS1::sscal_all
    MODULE PROCEDURE sscal_all
    !> @copybrief BLAS1::sscal_noINCX
    !> @copydetails BLAS1::sscal_noINCX
    MODULE PROCEDURE sscal_noINCX
    !> @copybrief BLAS1::sscal_noN
    !> @copydetails BLAS1::sscal_noN
    MODULE PROCEDURE sscal_noN
    !> @copybrief BLAS1::sscal_noNINCX
    !> @copydetails BLAS1::sscal_noNINCX
    MODULE PROCEDURE sscal_noNINCX
    !> @copybrief BLAS1::sscal_vecA_all
    !> @copydetails BLAS1::sscal_vecA_all
    MODULE PROCEDURE sscal_vecA_all
    !> @copybrief BLAS1::sscal_vecA_noINCX
    !> @copydetails BLAS1::sscal_vecA_noINCX
    MODULE PROCEDURE sscal_vecA_noINCX
    !> @copybrief BLAS1::sscal_vecA_noN
    !> @copydetails BLAS1::sscal_vecA_noN
    MODULE PROCEDURE sscal_vecA_noN
    !> @copybrief BLAS1::sscal_vecA_noNINCX
    !> @copydetails BLAS1::sscal_vecA_noNINCX
    MODULE PROCEDURE sscal_vecA_noNINCX
    !> @copybrief BLAS1::dscal_all
    !> @copydetails BLAS1::dscal_all
    MODULE PROCEDURE dscal_all
    !> @copybrief BLAS1::dscal_noINCX
    !> @copydetails BLAS1::dscal_noINCX
    MODULE PROCEDURE dscal_noINCX
    !> @copybrief BLAS1::dscal_noN
    !> @copydetails BLAS1::dscal_noN
    MODULE PROCEDURE dscal_noN
    !> @copybrief BLAS1::dscal_noNINCX
    !> @copydetails BLAS1::dscal_noNINCX
    MODULE PROCEDURE dscal_noNINCX
    !> @copybrief BLAS1::dscal_vecA_all
    !> @copydetails BLAS1::dscal_vecA_all
    MODULE PROCEDURE dscal_vecA_all
    !> @copybrief BLAS1::dscal_vecA_noINCX
    !> @copydetails BLAS1::dscal_vecA_noINCX
    MODULE PROCEDURE dscal_vecA_noINCX
    !> @copybrief BLAS1::dscal_vecA_noN
    !> @copydetails BLAS1::dscal_vecA_noN
    MODULE PROCEDURE dscal_vecA_noN
    !> @copybrief BLAS1::dscal_vecA_noNINCX
    !> @copydetails BLAS1::dscal_vecA_noNINCX
    MODULE PROCEDURE dscal_vecA_noNINCX
  ENDINTERFACE BLAS_scal
  
  !> @brief Generic interface to the Level 1 BLAS routine _swap
  !>
  !> _swap performs the operation y <-> x
  !>
  !> The generic interface supports passing of single precision or double
  !> precision vectors and the @c N, @c INCX, @c INCY arguments are made to be
  !> optional. These routines are resolved at compile time.
  !>
  INTERFACE BLAS_swap
    !> @copybrief BLAS1::sswap_all
    !> @copydetails BLAS1::sswap_all
    MODULE PROCEDURE sswap_all
    !> @copybrief BLAS1::sswap_noN
    !> @copydetails BLAS1::sswap_noN
    MODULE PROCEDURE sswap_noN
    !> @copybrief BLAS1::sswap_nxyinc
    !> @copydetails BLAS1::sswap_nxyinc
    MODULE PROCEDURE sswap_nxyinc
    !> @copybrief BLAS1::sswap_nxy
    !> @copydetails BLAS1::sswap_nxy
    MODULE PROCEDURE sswap_nxy
    !> @copybrief BLAS1::sswap_xyinc
    !> @copydetails BLAS1::sswap_xyinc
    MODULE PROCEDURE sswap_xyinc
    !> @copybrief BLAS1::sswap_xy
    !> @copydetails BLAS1::sswap_xy
    MODULE PROCEDURE sswap_xy
    !> @copybrief BLAS1::dswap_all
    !> @copydetails BLAS1::dswap_all
    MODULE PROCEDURE dswap_all
    !> @copybrief BLAS1::dswap_noN
    !> @copydetails BLAS1::dswap_noN
    MODULE PROCEDURE dswap_noN
    !> @copybrief BLAS1::dswap_nxyinc
    !> @copydetails BLAS1::dswap_nxyinc
    MODULE PROCEDURE dswap_nxyinc
    !> @copybrief BLAS1::dswap_nxy
    !> @copydetails BLAS1::dswap_nxy
    MODULE PROCEDURE dswap_nxy
    !> @copybrief BLAS1::dswap_xyinc
    !> @copydetails BLAS1::dswap_xyinc
    MODULE PROCEDURE dswap_xyinc
    !> @copybrief BLAS1::dswap_xy
    !> @copydetails BLAS1::dswap_xy
    MODULE PROCEDURE dswap_xy
  ENDINTERFACE BLAS_swap
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Function computes the sum of the absolute values of a single precision
!> vector.
!> @param n the size of the vector @c x
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return r the sum of the absolute values of @c x
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sasum.f but has some minor
!> modifications. The loop unrolling is not done for any vector with increment
!> 1, but now only for vectors with size greater than 100. For those less than
!> that, the intrinsic Fortran functions are used. The value of 100 was just
!> chosen arbitrarily and a value leading to more optimal performance most
!> certainly exists but will be architecture dependent.
!>
  PURE FUNCTION sasum_all(n,x,incx) RESULT(r)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SSK) :: r
#ifdef HAVE_BLAS
    !Not sure if this will actually work at link time, if it doesn't then 
    !"REAL(KIND(0.0e0)),EXTERNAL :: sasum" should work. But then the pure
    !attribute will need to be removed from the function.
    INTERFACE
      PURE REAL(KIND(0.0e0)) FUNCTION sasum(n,x,incx)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0e0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
      ENDFUNCTION sasum
    ENDINTERFACE
    !Added this check because if incx=0 and linked against MLK sasum
    !returns as if incx=1.
    r=0.0_SDK
    IF(incx > 0) r=sasum(n,x,incx)
#else
    INTEGER(SIK) :: i,nincx,m
    REAL(SSK) :: tmpsum
    INTRINSIC ABS,MOD,SUM
    
    r=0.0_SSK
    tmpsum=0.0_SSK
    IF(n > 0 .AND. incx > 0) THEN
      IF(incx == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 6 times when n is sufficiently large
          m=MOD(n,6)
          IF(m /= 0) tmpsum=SUM(ABS(x(1:m)))
          DO i=m+1,n,6
            tmpsum=tmpsum+ABS(x(i))+ABS(x(i+1))+ABS(x(i+2))+ABS(x(i+3))+ &
              ABS(x(i+4))+ABS(x(i+5))
          ENDDO
        ELSE
          !Nothing special
          tmpsum=SUM(ABS(x(1:n)))
        ENDIF
      ELSE
        !Nothing special just do the loop
        !I don't think this is correct but it's what is in netlib
        nincx=n*incx
        DO i=1,nincx,incx
          tmpsum=tmpsum+ABS(x(i))
        ENDDO
      ENDIF
    ENDIF
    r=tmpsum
#endif
  ENDFUNCTION sasum_all
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::sasum_all "sasum_all" for when the user
!> only wants to pass @c n and @c x.
!> @param n the size of the vector @c x
!> @param x the single-precision real vector to operate on
!> @return r the sum of the absolute values of @c x
!>
!> It might be more efficient to copy the block of code from @ref 
!> BLAS1::sasum_all "sasum_all" for @c incx==1 but I am hoping that if
!> inter-procedural optimizations are turned on for the compiler optimizations
!> it will do this correctly.
!>
  PURE FUNCTION sasum_noINCX(n,x) RESULT(r)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(n)
    REAL(SSK) :: r
    r=sasum_all(n,x,1)
  ENDFUNCTION sasum_noINCX
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::sasum_all "sasum_all" for when the user
!> only wants to pass @c x and @c incx.
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return r the sum of the absolute values of @c x
!>
  PURE FUNCTION sasum_noN(x,incx) RESULT(r)
    REAL(SSK),INTENT(IN) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SSK) :: r
    r=sasum_all(SIZE(x),x,incx)
  ENDFUNCTION sasum_noN
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::sasum_all "sasum_all" for when user only 
!> wants to pass @c x.
!> @param x the single-precision real vector to operate on
!> @return r the sum of the absolute values of @c x
!>
  PURE FUNCTION sasum_onlyX(x) RESULT(r)
    REAL(SSK),INTENT(IN) :: x(:)
    REAL(SSK) :: r
    r=sasum_all(SIZE(x),x,1)
  ENDFUNCTION sasum_onlyX
!
!-------------------------------------------------------------------------------
!> @brief Function computes the sum of the absolute values of a double precision
!> vector.
!> @param n the size of the vector @c x
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return r the sum of the absolute values of @c x
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dasum.f but has some minor
!> modifications. The loop unrolling is not done for any vector with increment
!> 1, but now only for vectors with size greater than 100. For those less than
!> that, the intrinsic Fortran functions are used. The value of 100 was just
!> chosen arbitrarily and a value leading to more optimal performance most
!> certainly exists but will be architecture dependent.
!>
  PURE FUNCTION dasum_all(n,x,incx) RESULT(r)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SDK) :: r
    
#ifdef HAVE_BLAS
    !Not sure if this will actually work at link time, if it doesn't then 
    !"REAL(KIND(0.0d0)),EXTERNAL :: sasum" should work. But then the pure
    !attribute will need to be removed from the function.
    INTERFACE
      PURE REAL(KIND(0.0d0)) FUNCTION dasum(n,x,incx)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0d0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
      ENDFUNCTION dasum
    ENDINTERFACE
    !Added this check because if incx=0 and linked against MLK dasum
    !returns as if incx=1.
    r=0.0_SDK
    IF(incx > 0) r=dasum(n,x,incx)
#else
    INTEGER(SIK) :: i,nincx,m
    REAL(SDK) :: tmpsum
    INTRINSIC ABS,MOD,SUM
    
    r=0.0_SDK
    tmpsum=0.0_SDK
    IF(n > 0 .AND. incx > 0) THEN
      IF(incx == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 6 times when n is sufficiently large
          m=MOD(n,6)
          IF(m /= 0) tmpsum=SUM(ABS(x(1:m)))
          DO i=m+1,n,6
            tmpsum=tmpsum+ABS(x(i))+ABS(x(i+1))+ABS(x(i+2))+ABS(x(i+3))+ &
              ABS(x(i+4))+ABS(x(i+5))
          ENDDO
        ELSE
          !Nothing special
          tmpsum=SUM(ABS(x(1:n)))
        ENDIF
      ELSE
        !Nothing special just do the loop
        !I don't think this is correct but it's what is in netlib
        nincx=n*incx
        DO i=1,nincx,incx
          tmpsum=tmpsum+ABS(x(i))
        ENDDO
      ENDIF
    ENDIF
    r=tmpsum
#endif
  ENDFUNCTION dasum_all
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::dasum_all "dasum_all" for when the user
!> only wants to pass @c n and @c x.
!> @param n the size of the vector @c x
!> @param x the double-precision real vector to operate on
!> @return r the sum of the absolute values of @c x
!>
!> It might be more efficient to copy the block of code from @ref 
!> BLAS1::dasum_all "dasum_all" for @c incx==1 but I am hoping that if
!> inter-procedural optimizations are turned on for the compiler optimizations
!> it will do this correctly.
!>
  PURE FUNCTION dasum_noINCX(n,x) RESULT(r)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(n)
    REAL(SDK) :: r
    r=dasum_all(n,x,1)
  ENDFUNCTION dasum_noINCX
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::dasum_all "dasum_all" for when the user
!> only wants to pass @c x and @c incx
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return r the sum of the absolute values of @c x
!>
  PURE FUNCTION dasum_noN(x,incx) RESULT(r)
    REAL(SDK),INTENT(IN) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SDK) :: r
    r=dasum_all(SIZE(x),x,incx)
  ENDFUNCTION dasum_noN
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::dasum_all "dasum_all" for when the user
!> only wants to pass @c x.
!> @param x the double-precision real vector to operate on
!> @return r the sum of the absolute values of @c x
!>
  PURE FUNCTION dasum_onlyX(x) RESULT(r)
    REAL(SDK),INTENT(IN) :: x(:)
    REAL(SDK) :: r
    r=dasum_all(SIZE(x),x,1)
  ENDFUNCTION dasum_onlyX
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the result of a vector plus a vector times a
!> constant for single precision values.
!> @param n the size of the vectors @c x and @c y
!> @param a the constant to multiply with @c x
!> @param x the single-precision real vector to add to @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the single-precision real vector that is the return argument
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/saxpy.f but has some minor
!> modifications. The loop unrolling is not done for any vector with increment
!> 1, but now only for vectors with size greater than 100. For those less than
!> that, the intrinsic Fortran operations are used. The value of 100 was just
!> chosen arbitrarily and a value leading to more optimal performance most
!> certainly exists but will be architecture dependent.
!>
  PURE SUBROUTINE saxpy_all(n,a,x,incx,y,incy)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: a
    REAL(SSK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SSK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
    !Not sure if this will actually work at link time, if it doesn't then 
    !"REAL(KIND(0.0e0)),EXTERNAL :: saxpy" should work. But then the pure
    !attribute will need to be removed from all the routines.
    INTERFACE
      PURE SUBROUTINE saxpy(n,a,x,incx,y,incy)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0e0)),INTENT(IN) :: a
        REAL(KIND(0.0e0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
        REAL(KIND(0.0e0)),INTENT(INOUT) :: y(*)
        INTEGER,INTENT(IN) :: incy
      ENDSUBROUTINE saxpy
    ENDINTERFACE
    
    CALL saxpy(n,a,x,incx,y,incy)
#else
    INTEGER(SIK) :: i,ix,iy,m
    INTRINSIC MOD
    IF(n > 0 .AND. a /= 0.0_SSK) THEN
      IF(incx == 1 .AND. incy == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 4 times
          m=MOD(n,4)
          IF(m /= 0) y(1:m)=y(1:m)+a*x(1:m)
          DO i=m+1,n,4
            y(i)=y(i)+a*x(i)
            y(i+1)=y(i+1)+a*x(i+1)
            y(i+2)=y(i+2)+a*x(i+2)
            y(i+3)=y(i+3)+a*x(i+3)
          ENDDO
        ELSE
          !Nothing special use intrinsic Fortran array operators
          y(1:n)=a*x(1:n)+y(1:n)
        ENDIF
      ELSE
        !I'm not sure about this coding because ix and iy will become > n
        ix=1
        iy=1
        IF(incx < 0) ix=(1-n)*incx+1
        IF(incy < 0) iy=(1-n)*incy+1
        DO i=1,n
          y(iy)=y(iy)+a*x(ix)
          ix=ix+incx
          iy=iy+incy
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDSUBROUTINE saxpy_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::saxpy_all "saxpy_all" for when the user
!> only wants to pass @c n, @c a, @c x, @c y, and one increment to use for both
!> @c incx and @c incy.
!> @param n the size of the vectors @c x and @c y
!> @param a the constant to multiply with @c x
!> @param x the single-precision real vector to add to @c y
!> @param y the single-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in @c x and @c y
!>
  PURE SUBROUTINE saxpy_naxyinc(n,a,x,y,inc)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: a
    REAL(SSK),INTENT(IN) :: x(*)
    REAL(SSK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: inc
    CALL saxpy_all(n,a,x,inc,y,inc)
  ENDSUBROUTINE saxpy_naxyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::saxpy_all "saxpy_all" for when the user
!> only wants to pass @c a, @c x, @c y, and one increment to use for both 
!> @c incx and @c incy.
!> @param a the constant to multiply with @c x
!> @param x the single-precision real vector to add to @c y
!> @param y the single-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in @c x and @c y
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::saxpy_all "saxpy_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE saxpy_axyinc(a,x,y,inc)
    REAL(SSK),INTENT(IN) :: a
    REAL(SSK),INTENT(IN) :: x(:)
    REAL(SSK),INTENT(INOUT) :: y(:)
    INTEGER(SIK),INTENT(IN) :: inc
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL saxpy_all(n,a,x,inc,y,inc)
  ENDSUBROUTINE saxpy_axyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::saxpy_all "saxpy_all" for when the user
!> only wants to pass @c n, @c a, @c x, and @c y. The increment is set to 1.
!> @param n the size of the vectors @c x and @c y
!> @param a the constant to multiply with @c x
!> @param x the single-precision real vector to add to @c y
!> @param y the single-precision real vector that is the return argument
!>
  PURE SUBROUTINE saxpy_naxy(n,a,x,y)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: a
    REAL(SSK),INTENT(IN) :: x(*)
    REAL(SSK),INTENT(INOUT) :: y(*)
    CALL saxpy_all(n,a,x,1,y,1)
  ENDSUBROUTINE saxpy_naxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::saxpy_all "saxpy_all" for when the user
!> only wants to pass @c a, @c x, and @c y.
!> @param a the constant to multiply with @c x
!> @param x the single-precision real vector to add to @c y
!> @param y the single-precision real vector that is the return argument
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::saxpy_all "saxpy_all" if the sizes of
!> @c x and @c y are the same. The increment is set to 1.
!>
  PURE SUBROUTINE saxpy_axy(a,x,y)
    REAL(SSK),INTENT(IN) :: a
    REAL(SSK),INTENT(IN) :: x(:)
    REAL(SSK),INTENT(INOUT) :: y(:)
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL saxpy_all(n,a,x,1,y,1)
  ENDSUBROUTINE saxpy_axy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the result of a vector plus a vector times a
!> vector for single precision values.
!> @param n the size of the vectors @c x and @c y
!> @param a the vector to multiply with @c x
!> @param x the single-precision real vector to add to @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the single-precision real vector that is the return argument
!> @param incy the increment to use when looping over elements in @c y
!>
!> This routine is an extension to the BLAS functionality where @c a is a vector
!> which is multiplied with @c x on an element-wise basis.
!>
  PURE SUBROUTINE saxpy_vecA_all(n,a,x,incx,y,incy)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: a(*)
    REAL(SSK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SSK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: incy
    INTEGER(SIK) :: i,ix,iy,m
    INTRINSIC MOD
    
    IF(n > 0 .AND. ANY(a(1:n) /= 0.0_SSK)) THEN
      IF(incx == 1 .AND. incy == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 4 times
          m=MOD(n,4)
          IF(m /= 0) y(1:m)=y(1:m)+a(1:m)*x(1:m)
          DO i=m+1,n,4
            y(i)=y(i)+a(i)*x(i)
            y(i+1)=y(i+1)+a(i+1)*x(i+1)
            y(i+2)=y(i+2)+a(i+2)*x(i+2)
            y(i+3)=y(i+3)+a(i+3)*x(i+3)
          ENDDO
        ELSE
          !Nothing special use intrinsic Fortran array operators
          y(1:n)=a(1:n)*x(1:n)+y(1:n)
        ENDIF
      ELSE
        !I'm not sure about this coding because ix and iy will become > n
        ix=1
        iy=1
        IF(incx < 0) ix=(1-n)*incx+1
        IF(incy < 0) iy=(1-n)*incy+1
        DO i=1,n
          y(iy)=y(iy)+a(ix)*x(ix)
          ix=ix+incx
          iy=iy+incy
        ENDDO
      ENDIF
    ENDIF
  ENDSUBROUTINE saxpy_vecA_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::saxpy_vecA_all "saxpy_vecA_all" for when
!> the user only wants to pass @c n,@c a, @c x, @c y, and one increment to use
!> for both @c incx and @c incy.
!> @param n the size of the vectors @c x and @c y
!> @param a the vector to multiply with @c x
!> @param x the single-precision real vector to add to @c y
!> @param y the single-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in @c x and @c y
!>
  PURE SUBROUTINE saxpy_vecA_naxyinc(n,a,x,y,inc)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: a(*)
    REAL(SSK),INTENT(IN) :: x(*)
    REAL(SSK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: inc
    CALL saxpy_vecA_all(n,a,x,inc,y,inc)
  ENDSUBROUTINE saxpy_vecA_naxyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::saxpy_vecA_all "saxpy_vecA_all" for when
!> the user only wants to pass @c a, @c x, @c y, and one increment to use for
!> both @c incx and @c incy.
!> @param a the vector to multiply with @c x
!> @param x the single-precision real vector to add to @c y
!> @param y the single-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in @c x and @c y
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::saxpy_vecA_all "saxpy_vecA_all" if the
!> sizes of @c x and @c y are the same. The size of @c x and @c a must be the
!> same.
!>
  PURE SUBROUTINE saxpy_vecA_axyinc(a,x,y,inc)
    REAL(SSK),INTENT(IN) :: a(:)
    REAL(SSK),INTENT(IN) :: x(:)
    REAL(SSK),INTENT(INOUT) :: y(:)
    INTEGER(SIK),INTENT(IN) :: inc
    INTEGER(SIK) :: n
    IF(SIZE(x) == SIZE(a)) THEN
      n=MIN(SIZE(x),SIZE(y))
      CALL saxpy_vecA_all(n,a,x,inc,y,inc)
    ENDIF
  ENDSUBROUTINE saxpy_vecA_axyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::saxpy_vecA_all "saxpy_vecA_all" for when
!> the user only wants to pass @c n, @c a, @c x, and @c y. The increment is set
!> to 1.
!> @param n the size of the vectors @c x and @c y
!> @param a the vector to multiply with @c x
!> @param x the single-precision real vector to add to @c y
!> @param y the single-precision real vector that is the return argument
!>
  PURE SUBROUTINE saxpy_vecA_naxy(n,a,x,y)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: a(*)
    REAL(SSK),INTENT(IN) :: x(*)
    REAL(SSK),INTENT(INOUT) :: y(*)
    CALL saxpy_vecA_all(n,a,x,1,y,1)
  ENDSUBROUTINE saxpy_vecA_naxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::saxpy_vecA_all "saxpy_vecA_all" for when
!> the user only wants to pass @c a, @c x, and @c y.
!> @param a the vector to multiply with @c x
!> @param x the single-precision real vector to add to @c y
!> @param y the single-precision real vector that is the return argument
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::saxpy_vecA_all "saxpy_vecA_all" if the
!> sizes of @c x and @c y are the same. The size of @c x and @c a must be the
!> same.
!>
  PURE SUBROUTINE saxpy_vecA_axy(a,x,y)
    REAL(SSK),INTENT(IN) :: a(:)
    REAL(SSK),INTENT(IN) :: x(:)
    REAL(SSK),INTENT(INOUT) :: y(:)
    INTEGER(SIK) :: n
    IF(SIZE(x) == SIZE(a)) THEN
      n=MIN(SIZE(x),SIZE(y))
      CALL saxpy_vecA_all(n,a,x,1,y,1)
    ENDIF
  ENDSUBROUTINE saxpy_vecA_axy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the result of a vector plus a vector for single
!> precision values.
!> @param n the size of the vectors @c x and @c y
!> @param x the single-precision real vector to add to @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the single-precision real vector that is the return argument
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/saxpy.f but has some minor
!> modifications. The loop unrolling is not done for any vector with increment
!> 1, but now only for vectors with size greater than 100. For those less than
!> that, the intrinsic Fortran operations are used. The value of 100 was just
!> chosen arbitrarily and a value leading to more optimal performance most
!> certainly exists but will be architecture dependent. Also since a is not
!> passed as in the BLAS routine, it is assumed to be 1, and the loops are
!> rewritten here without the multiplication operation so they should be faster.
!>
  PURE SUBROUTINE saxpy_nxyincxy(n,x,incx,y,incy)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SSK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
    !Not sure if this will actually work at link time, if it doesn't then 
    !"REAL(KIND(0.0e0)),EXTERNAL :: saxpy" should work. But then the pure
    !attribute will need to be removed from all the routines.
    INTERFACE
      PURE SUBROUTINE saxpy(n,a,x,incx,y,incy)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0e0)),INTENT(IN) :: a
        REAL(KIND(0.0e0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
        REAL(KIND(0.0e0)),INTENT(INOUT) :: y(*)
        INTEGER,INTENT(IN) :: incy
      ENDSUBROUTINE saxpy
    ENDINTERFACE
    CALL saxpy(n,1.0_SSK,x,incx,y,incy)
#else
    INTEGER(SIK) :: i,ix,iy,m
    
    IF(n > 0) THEN
      IF(incx == 1 .AND. incy == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 4 times
          m=MOD(n,4)
          IF(m /= 0) y(1:m)=y(1:m)+x(1:m)
          DO i=m+1,n,4
            y(i)=y(i)+x(i)
            y(i+1)=y(i+1)+x(i+1)
            y(i+2)=y(i+2)+x(i+2)
            y(i+3)=y(i+3)+x(i+3)
          ENDDO
        ELSE
          !Nothing special use intrinsic Fortran array operators
          y(1:n)=y(1:n)+x(1:n)
        ENDIF
      ELSE
        !I'm not sure about this coding because ix and iy will become > n
        ix=1
        iy=1
        IF(incx < 0) ix=(1-n)*incx+1
        IF(incy < 0) iy=(1-n)*incy+1
        DO i=1,n
          y(iy)=y(iy)+x(ix)
          ix=ix+incx
          iy=iy+incy
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDSUBROUTINE saxpy_nxyincxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::saxpy_nxyincxy "saxpy_nxyincxy" for when
!> the user only wants to pass @c n, @c x, @c y, and one increment to use for
!> both @c incx and @c incy.
!> @param n the size of the vectors @c x and @c y
!> @param x the single-precision real vector to add to @c y
!> @param y the single-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in @c x and @c y
!>
  PURE SUBROUTINE saxpy_nxyinc(n,x,y,inc)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    REAL(SSK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: inc
    CALL saxpy_nxyincxy(n,x,inc,y,inc)
  ENDSUBROUTINE saxpy_nxyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::saxpy_nxyincxy "saxpy_nxyincxy" for when
!> the user only wants to pass @c n, @c x, and @c y. The increment is set to 1.
!> @param n the size of the vectors @c x and @c y
!> @param x the single-precision real vector to add to @c y
!> @param y the single-precision real vector that is the return argument
!>
  PURE SUBROUTINE saxpy_nxy(n,x,y)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    REAL(SSK),INTENT(INOUT) :: y(*)
    CALL saxpy_nxyincxy(n,x,1,y,1)
  ENDSUBROUTINE saxpy_nxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::saxpy_nxyincxy "saxpy_nxyincxy" for when
!> the user only wants to pass @c x and @c y.
!> @param x the single-precision real vector to add to @c y
!> @param y the single-precision real vector that is the return argument
!>
!> This routine computes n as the minimum of the sizes x and y, so they do not
!> have to necessarily be the same size. We may decide later that this routine
!> should only call @ref BLAS1::saxpy_nxyincxy "saxpy_nxyincxy" if the sizes of
!> @c x and @c y are the same. The increment is set to 1.
!>
  PURE SUBROUTINE saxpy_xy(x,y)
    REAL(SSK),INTENT(IN) :: x(:)
    REAL(SSK),INTENT(INOUT) :: y(:)
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL saxpy_nxyincxy(n,x,1,y,1)
  ENDSUBROUTINE saxpy_xy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the result of a vector plus a vector times a
!> constant for double precision values.
!> @param n the size of the vectors @c x and @c y
!> @param a the constant to multiply with @c x
!> @param x the double-precision real vector to add to @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the double-precision real vector that is the return argument
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/daxpy.f but has some minor
!> modifications. The loop unrolling is not done for any vector with increment
!> 1, but now only for vectors with size greater than 100. For those less than
!> that, the intrinsic Fortran operations are used. The value of 100 was just
!> chosen arbitrarily and a value leading to more optimal performance most
!> certainly exists but will be architecture dependent.
!>
  PURE SUBROUTINE daxpy_all(n,a,x,incx,y,incy)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: a
    REAL(SDK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SDK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
    !Not sure if this will actually work at link time, if it doesn't then 
    !"REAL(KIND(0.0d0)),EXTERNAL :: daxpy" should work. But then the pure
    !attribute will need to be removed from all the routines.
    INTERFACE
      PURE SUBROUTINE daxpy(n,a,x,incx,y,incy)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0d0)),INTENT(IN) :: a
        REAL(KIND(0.0d0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
        REAL(KIND(0.0d0)),INTENT(INOUT) :: y(*)
        INTEGER,INTENT(IN) :: incy
      ENDSUBROUTINE daxpy
    ENDINTERFACE
    
    CALL daxpy(n,a,x,incx,y,incy)
#else
    INTEGER(SIK) :: i,ix,iy,m
    INTRINSIC MOD
    
    IF(n > 0 .AND. a /= 0.0_SDK) THEN
      IF(incx == 1 .AND. incy == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 4 times
          m=MOD(n,4)
          IF(m /= 0) y(1:m)=y(1:m)+a*x(1:m)
          DO i=m+1,n,4
            y(i)=y(i)+a*x(i)
            y(i+1)=y(i+1)+a*x(i+1)
            y(i+2)=y(i+2)+a*x(i+2)
            y(i+3)=y(i+3)+a*x(i+3)
          ENDDO
        ELSE
          !Nothing special use intrinsic Fortran array operators
          y(1:n)=a*x(1:n)+y(1:n)
        ENDIF
      ELSE
        !I'm not sure about this coding because ix and iy will become > n
        ix=1
        iy=1
        IF(incx < 0) ix=(1-n)*incx+1
        IF(incy < 0) iy=(1-n)*incy+1
        DO i=1,n
          y(iy)=y(iy)+a*x(ix)
          ix=ix+incx
          iy=iy+incy
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDSUBROUTINE daxpy_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::daxpy_all "daxpy_all" for when the user
!> only wants to pass @c n, @c a, @c x, @c y, and one increment to use for both
!> @c incx and @c incy.
!> @param n the size of the vectors @c x and @c y
!> @param a the constant to multiply with @c x
!> @param x the double-precision real vector to add to @c y
!> @param y the double-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in @c x and @c y
!>
  PURE SUBROUTINE daxpy_naxyinc(n,a,x,y,inc)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: a
    REAL(SDK),INTENT(IN) :: x(n)
    REAL(SDK),INTENT(INOUT) :: y(n)
    INTEGER(SIK),INTENT(IN) :: inc
    CALL daxpy_all(n,a,x,inc,y,inc)
  ENDSUBROUTINE daxpy_naxyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::daxpy_all "daxpy_all" for when the user
!> only wants to pass @c a, @c x, @c y, and one increment to use for both
!> @c incx and @c incy.
!> @param a the constant to multiply with @c x
!> @param x the double-precision real vector to add to @c y
!> @param y the double-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in x and y
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::daxpy_all "daxpy_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE daxpy_axyinc(a,x,y,inc)
    REAL(SDK),INTENT(IN) :: a
    REAL(SDK),INTENT(IN) :: x(:)
    REAL(SDK),INTENT(INOUT) :: y(:)
    INTEGER(SIK),INTENT(IN) :: inc
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL daxpy_all(n,a,x,inc,y,inc)
  ENDSUBROUTINE daxpy_axyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::daxpy_all "daxpy_all" for when the user
!> only wants to pass @c n, @c a, @c x, and @c y. The increment is set to 1.
!> @param n the size of the vectors @c x and @c y
!> @param a the constant to multiply with @c x
!> @param x the double-precision real vector to add to @c y
!> @param y the double-precision real vector that is the return argument
!>
  PURE SUBROUTINE daxpy_naxy(n,a,x,y)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: a
    REAL(SDK),INTENT(IN) :: x(n)
    REAL(SDK),INTENT(INOUT) :: y(n)
    CALL daxpy_all(n,a,x,1,y,1)
  ENDSUBROUTINE daxpy_naxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::daxpy_all "daxpy_all" for when the user
!> only wants to pass @c a, @c x, and @c y.
!> @param a the constant to multiply with @c x
!> @param x the double-precision real vector to add to @c y
!> @param y the double-precision real vector that is the return argument
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::daxpy_all "daxpy_all" if the sizes of
!> @c x and @c y are the same. The increment is set to 1.
!>
  PURE SUBROUTINE daxpy_axy(a,x,y)
    REAL(SDK),INTENT(IN) :: a
    REAL(SDK),INTENT(IN) :: x(:)
    REAL(SDK),INTENT(INOUT) :: y(:)
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL daxpy_all(n,a,x,1,y,1)
  ENDSUBROUTINE daxpy_axy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the result of a vector plus a vector times a
!> vector for double precision values.
!> @param n the size of the vectors @c x and @c y
!> @param a the vector to multiply with @c x
!> @param x the double-precision real vector to add to @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the double-precision real vector that is the return argument
!> @param incy the increment to use when looping over elements in @c y
!>
!> This routine is an extension to the BLAS functionality where @c a is a vector
!> which is multiplied with @c x on an element-wise basis.
!>
  PURE SUBROUTINE daxpy_vecA_all(n,a,x,incx,y,incy)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: a(*)
    REAL(SDK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SDK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: incy
    INTEGER(SIK) :: i,ix,iy,m
    INTRINSIC MOD
    
    IF(n > 0 .AND. ANY(a(1:n) /= 0.0_SDK)) THEN
      IF(incx == 1 .AND. incy == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 4 times
          m=MOD(n,4)
          IF(m /= 0) y(1:m)=y(1:m)+a(1:m)*x(1:m)
          DO i=m+1,n,4
            y(i)=y(i)+a(i)*x(i)
            y(i+1)=y(i+1)+a(i+1)*x(i+1)
            y(i+2)=y(i+2)+a(i+2)*x(i+2)
            y(i+3)=y(i+3)+a(i+3)*x(i+3)
          ENDDO
        ELSE
          !Nothing special use intrinsic Fortran array operators
          y(1:n)=a(1:n)*x(1:n)+y(1:n)
        ENDIF
      ELSE
        !I'm not sure about this coding because ix and iy will become > n
        ix=1
        iy=1
        IF(incx < 0) ix=(1-n)*incx+1
        IF(incy < 0) iy=(1-n)*incy+1
        DO i=1,n
          y(iy)=y(iy)+a(ix)*x(ix)
          ix=ix+incx
          iy=iy+incy
        ENDDO
      ENDIF
    ENDIF
  ENDSUBROUTINE daxpy_vecA_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::daxpy_vecA_all "daxpy_vecA_all" for when
!> the user only wants to pass @c n,@c a, @c x, @c y, and one increment to use
!> for both @c incx and @c incy.
!> @param n the size of the vectors @c x and @c y
!> @param a the vector to multiply with @c x
!> @param x the double-precision real vector to add to @c y
!> @param y the double-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in @c x and @c y
!>
  PURE SUBROUTINE daxpy_vecA_naxyinc(n,a,x,y,inc)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: a(*)
    REAL(SDK),INTENT(IN) :: x(*)
    REAL(SDK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: inc
    CALL daxpy_vecA_all(n,a,x,inc,y,inc)
  ENDSUBROUTINE daxpy_vecA_naxyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::daxpy_vecA_all "daxpy_vecA_all" for when
!> the user only wants to pass @c a, @c x, @c y, and one increment to use for
!> both @c incx and @c incy.
!> @param a the vector to multiply with @c x
!> @param x the double-precision real vector to add to @c y
!> @param y the double-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in @c x and @c y
!>
!> This routine computes n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::daxpy_vecA_all "daxpy_vecA_all" if the
!> sizes of @c x and @c y are the same. The size of @c x and @c a must be the
!> same.
!>
  PURE SUBROUTINE daxpy_vecA_axyinc(a,x,y,inc)
    REAL(SDK),INTENT(IN) :: a(:)
    REAL(SDK),INTENT(IN) :: x(:)
    REAL(SDK),INTENT(INOUT) :: y(:)
    INTEGER(SIK),INTENT(IN) :: inc
    INTEGER(SIK) :: n
    IF(SIZE(x) == SIZE(a)) THEN
      n=MIN(SIZE(x),SIZE(y))
      CALL daxpy_vecA_all(n,a,x,inc,y,inc)
    ENDIF
  ENDSUBROUTINE daxpy_vecA_axyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::daxpy_vecA_all "daxpy_vecA_all" for when
!> the user only wants to pass @c n, @c a, @c x, and @c y. The increment is set
!> to 1.
!> @param n the size of the vectors @c x and @c y
!> @param a the vector to multiply with @c x
!> @param x the double-precision real vector to add to @c y
!> @param y the double-precision real vector that is the return argument
!>
  PURE SUBROUTINE daxpy_vecA_naxy(n,a,x,y)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: a(*)
    REAL(SDK),INTENT(IN) :: x(*)
    REAL(SDK),INTENT(INOUT) :: y(*)
    CALL daxpy_vecA_all(n,a,x,1,y,1)
  ENDSUBROUTINE daxpy_vecA_naxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::daxpy_vecA_all "daxpy_vecA_all" for when
!> the user only wants to pass @c a, @c x, and @c y.
!> @param a the vector to multiply with @c x
!> @param x the double-precision real vector to add to @c y
!> @param y the double-precision real vector that is the return argument
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::daxpy_vecA_all "daxpy_vecA_all" if the
!> sizes of @c x and @c y are the same. The size of @c x and @c a must be the
!> same.
!>
  PURE SUBROUTINE daxpy_vecA_axy(a,x,y)
    REAL(SDK),INTENT(IN) :: a(:)
    REAL(SDK),INTENT(IN) :: x(:)
    REAL(SDK),INTENT(INOUT) :: y(:)
    INTEGER(SIK) :: n
    IF(SIZE(x) == SIZE(a)) THEN
      n=MIN(SIZE(x),SIZE(y))
      CALL daxpy_vecA_all(n,a,x,1,y,1)
    ENDIF
  ENDSUBROUTINE daxpy_vecA_axy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the result of a vector plus a vector for double
!> precision values.
!> @param n the size of the vectors @c x and @c y
!> @param x the double-precision real vector to add to @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the double-precision real vector that is the return argument
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/daxpy.f but has some minor
!> modifications. The loop unrolling is not done for any vector with increment
!> 1, but now only for vectors with size greater than 100. For those less than
!> that, the intrinsic Fortran operations are used. The value of 100 was just
!> chosen arbitrarily and a value leading to more optimal performance most
!> certainly exists but will be architecture dependent. Also since a is not
!> passed as in the BLAS routine, it is assumed to be 1, and the loops are
!> rewritten here without the multiplication operation so they should be faster.
!>
  PURE SUBROUTINE daxpy_nxyincxy(n,x,incx,y,incy)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SDK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
    !Not sure if this will actually work at link time, if it doesn't then 
    !"REAL(KIND(0.0d0)),EXTERNAL :: daxpy" should work. But then the pure
    !attribute will need to be removed from all the routines.
    INTERFACE
      PURE SUBROUTINE daxpy(n,a,x,incx,y,incy)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0d0)),INTENT(IN) :: a
        REAL(KIND(0.0d0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
        REAL(KIND(0.0d0)),INTENT(INOUT) :: y(*)
        INTEGER,INTENT(IN) :: incy
      ENDSUBROUTINE daxpy
    ENDINTERFACE
    CALL daxpy(n,1.0_SDK,x,incx,y,incy)
#else
    INTEGER(SIK) :: i,ix,iy,m
    INTRINSIC MOD
    
    IF(n > 0) THEN
      IF(incx == 1 .AND. incy == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 4 times
          m=MOD(n,4)
          IF(m /= 0) y(1:m)=y(1:m)+x(1:m)
          DO i=m+1,n,4
            y(i)=y(i)+x(i)
            y(i+1)=y(i+1)+x(i+1)
            y(i+2)=y(i+2)+x(i+2)
            y(i+3)=y(i+3)+x(i+3)
          ENDDO
        ELSE
          !Nothing special use intrinsic Fortran array operators
          y(1:n)=y(1:n)+x(1:n)
        ENDIF
      ELSE
        !I'm not sure about this coding because ix and iy will become > n
        ix=1
        iy=1
        IF(incx < 0) ix=(1-n)*incx+1
        IF(incy < 0) iy=(1-n)*incy+1
        DO i=1,n
          y(iy)=y(iy)+x(ix)
          ix=ix+incx
          iy=iy+incy
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDSUBROUTINE daxpy_nxyincxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::daxpy_nxyincxy "daxpy_nxyincxy" for when
!> the user only wants to pass @c n, @c x, @c y, and one increment to use for
!> both @c incx and @c incy.
!> @param n the size of the vectors @c x and @c y
!> @param x the double-precision real vector to add to @c y
!> @param y the double-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in @c x and @c y
!>
  PURE SUBROUTINE daxpy_nxyinc(n,x,y,inc)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(n)
    REAL(SDK),INTENT(INOUT) :: y(n)
    INTEGER(SIK),INTENT(IN) :: inc
    CALL daxpy_nxyincxy(n,x,inc,y,inc)
  ENDSUBROUTINE daxpy_nxyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::daxpy_nxyincxy "daxpy_nxyincxy" for when
!> the user only wants to pass @c n, @c x, and @c y. The increment is set to 1.
!> @param n the size of the vectors @c x and @c y
!> @param x the double-precision real vector to add to @c y
!> @param y the double-precision real vector that is the return argument
!>
  PURE SUBROUTINE daxpy_nxy(n,x,y)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(n)
    REAL(SDK),INTENT(INOUT) :: y(n)
    CALL daxpy_nxyincxy(n,x,1,y,1)
  ENDSUBROUTINE daxpy_nxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::daxpy_nxyincxy "daxpy_nxyincxy" for when
!> the user only wants to pass @c x and @c y.
!> @param x the double-precision real vector to add to @c y
!> @param y the double-precision real vector that is the return argument
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::daxpy_nxyincxy "daxpy_nxyincxy" if the
!> sizes of @c x and @c y are the same. The increment is set to 1.
!>
  PURE SUBROUTINE daxpy_xy(x,y)
    REAL(SDK),INTENT(IN) :: x(:)
    REAL(SDK),INTENT(INOUT) :: y(:)
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL daxpy_nxyincxy(n,x,1,y,1)
  ENDSUBROUTINE daxpy_xy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine rescales a vector @c x by a constant @c a for single
!> precision values.
!> @param n the size of the vector @c x
!> @param a the constant to multiply with @c x
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sscal.f but has some minor
!> modifications. The loop unrolling is not done for any vector with increment
!> 1, but now only for vectors with size greater than 100. For those less than
!> that, the intrinsic Fortran operations are used. The value of 100 was just
!> chosen arbitrarily and a value leading to more optimal performance most
!> certainly exists but will be architecture dependent.
!>
  PURE SUBROUTINE sscal_all(n,a,x,incx)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: a
    REAL(SSK),INTENT(INOUT) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
#ifdef HAVE_BLAS
    !Not sure if this will actually work at link time, if it doesn't then 
    !"REAL(KIND(0.0e0)),EXTERNAL :: sscal" should work. But then the pure
    !attribute will need to be removed from all the routines.
    INTERFACE
      PURE SUBROUTINE sscal(n,a,x,incx)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0e0)),INTENT(IN) :: a
        REAL(KIND(0.0e0)),INTENT(INOUT) :: x(*)
        INTEGER,INTENT(IN) :: incx
      ENDSUBROUTINE sscal
    ENDINTERFACE
    !Added this check because if incx < 0 and linked against MLK sscal
    !returns as if incx=ABS(inxc).
    IF(incx > 0) CALL sscal(n,a,x,incx)
#else
    INTEGER(SIK) :: i,nincx,m
    INTRINSIC MOD
    
    IF(n > 0 .AND. incx > 0) THEN
      IF(incx == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 5 times
          m=MOD(n,5)
          IF(m /= 0) x(1:m)=a*x(1:m)
          DO i=m+1,n,5
            x(i)=a*x(i)
            x(i+1)=a*x(i+1)
            x(i+2)=a*x(i+2)
            x(i+3)=a*x(i+3)
            x(i+4)=a*x(i+4)
          ENDDO
        ELSE
          !Nothing special use intrinsic Fortran array operators
          x(1:n)=a*x(1:n)
        ENDIF
      ELSE
        !I'm not sure about this coding because i will become > n
        nincx=n*incx
        DO i=1,nincx,incx
          x(i)=a*x(i)
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDSUBROUTINE sscal_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::sscal_all "sscal_all" for when
!> the user only wants to pass @c n, @c a and @c x. The increment is set to 1.
!> @param n the size of the vector @c x
!> @param a the constant to multiply with @c x
!> @param x the single-precision real vector to operate on
!>
  PURE SUBROUTINE sscal_noINCX(n,a,x)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: a
    REAL(SSK),INTENT(INOUT) :: x(*)
    CALL sscal_all(n,a,x,1)
  ENDSUBROUTINE sscal_noINCX
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::sscal_all "sscal_all" for when
!> the user only wants to pass @c a, @c x, and @incx.
!> @param a the constant to multiply with @c x
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!>
  PURE SUBROUTINE sscal_noN(a,x,incx)
    REAL(SSK),INTENT(IN) :: a
    REAL(SSK),INTENT(INOUT) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    CALL sscal_all(SIZE(x),a,x,incx)
  ENDSUBROUTINE sscal_noN
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::sscal_all "sscal_all" for when
!> the user only wants to pass @c a and @c x. The increment is set to 1.
!> @param a the constant to multiply with @c x
!> @param x the single-precision real vector to operate on
!>
  PURE SUBROUTINE sscal_noNINCX(a,x)
    REAL(SSK),INTENT(IN) :: a
    REAL(SSK),INTENT(INOUT) :: x(:)
    CALL sscal_all(SIZE(x),a,x,1)
  ENDSUBROUTINE sscal_noNINCX
!
!-------------------------------------------------------------------------------
!> @brief Subroutine rescales a vector @c x by a vector @c a for single
!> precision values.
!> @param n the size of the vector @c x
!> @param a the vector to multiply with @c x
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x and @c a
!>
!> This routine is an extension to the BLAS functionality where @c a is a vector
!> which is multiplied with @c x on an element-wise basis.
!>
  PURE SUBROUTINE sscal_vecA_all(n,a,x,incx)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: a(*)
    REAL(SSK),INTENT(INOUT) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    INTEGER(SIK) :: i,nincx,m
    INTRINSIC MOD
    
    IF(n > 0 .AND. incx > 0) THEN
      IF(incx == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 5 times
          m=MOD(n,5)
          IF(m /= 0) x(1:m)=a(1:m)*x(1:m)
          DO i=m+1,n,5
            x(i)=a(i)*x(i)
            x(i+1)=a(i+1)*x(i+1)
            x(i+2)=a(i+2)*x(i+2)
            x(i+3)=a(i+3)*x(i+3)
            x(i+4)=a(i+4)*x(i+4)
          ENDDO
        ELSE
          !Nothing special use intrinsic Fortran array operators
          x(1:n)=a(1:n)*x(1:n)
        ENDIF
      ELSE
        !I'm not sure about this coding because i will become > n
        nincx=n*incx
        DO i=1,nincx,incx
          x(i)=a(i)*x(i)
        ENDDO
      ENDIF
    ENDIF
  ENDSUBROUTINE sscal_vecA_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::sscal_vecA_all "sscal_vecA_all" for when
!> the user only wants to pass @c n, @c a and @c x. The increment is set to 1.
!> @param n the size of the vector @c x
!> @param a the vector to multiply with @c x
!> @param x the single-precision real vector to operate on
!>
  PURE SUBROUTINE sscal_vecA_noINCX(n,a,x)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: a(*)
    REAL(SSK),INTENT(INOUT) :: x(*)
    CALL sscal_vecA_all(n,a,x,1)
  ENDSUBROUTINE sscal_vecA_noINCX
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::sscal_vecA_all "sscal_vecA_all" for when
!> the user only wants to pass @c a, @c x, and @incx.
!> @param a the vector to multiply with @c x
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!>
!> The sizes of @c a and @c x must be the same.
!>
  PURE SUBROUTINE sscal_vecA_noN(a,x,incx)
    REAL(SSK),INTENT(IN) :: a(:)
    REAL(SSK),INTENT(INOUT) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    IF(SIZE(x) == SIZE(a)) CALL sscal_vecA_all(SIZE(x),a,x,incx)
  ENDSUBROUTINE sscal_vecA_noN
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::sscal_vecA_all "sscal_vecA_all" for when
!> the user only wants to pass @c a and @c x. The increment is set to 1.
!> @param a the vector to multiply with @c x
!> @param x the single-precision real vector to operate on
!>
!> The sizes of @c a and @c x must be the same.
!>
  PURE SUBROUTINE sscal_vecA_noNINCX(a,x)
    REAL(SSK),INTENT(IN) :: a(:)
    REAL(SSK),INTENT(INOUT) :: x(:)
    IF(SIZE(x) == SIZE(a)) CALL sscal_vecA_all(SIZE(x),a,x,1)
  ENDSUBROUTINE sscal_vecA_noNINCX
!
!-------------------------------------------------------------------------------
!> @brief Subroutine rescales a vector @c x by a constant @c a for double
!> precision values.
!> @param n the size of the vector @c x
!> @param a the constant to multiply with @c x
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dscal.f but has some minor
!> modifications. The loop unrolling is not done for any vector with increment
!> 1, but now only for vectors with size greater than 100. For those less than
!> that, the intrinsic Fortran operations are used. The value of 100 was just
!> chosen arbitrarily and a value leading to more optimal performance most
!> certainly exists but will be architecture dependent.
!>
  PURE SUBROUTINE dscal_all(n,a,x,incx)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: a
    REAL(SDK),INTENT(INOUT) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
#ifdef HAVE_BLAS
    !Not sure if this will actually work at link time, if it doesn't then 
    !"REAL(KIND(0.0d0)),EXTERNAL :: dscal" should work. But then the pure
    !attribute will need to be removed from all the routines.
    INTERFACE
      PURE SUBROUTINE dscal(n,a,x,incx)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0d0)),INTENT(IN) :: a
        REAL(KIND(0.0d0)),INTENT(INOUT) :: x(*)
        INTEGER,INTENT(IN) :: incx
      ENDSUBROUTINE dscal
    ENDINTERFACE
    !Added this check because if incx < 0 and linked against MLK dscal
    !returns as if incx=ABS(inxc).
    IF(incx > 0) CALL dscal(n,a,x,incx)
#else
    INTEGER(SIK) :: i,nincx,m
    INTRINSIC MOD
    
    IF(n > 0 .AND. incx > 0) THEN
      IF(incx == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 5 times
          m=MOD(n,5)
          IF(m /= 0) x(1:m)=a*x(1:m)
          DO i=m+1,n,5
            x(i)=a*x(i)
            x(i+1)=a*x(i+1)
            x(i+2)=a*x(i+2)
            x(i+3)=a*x(i+3)
            x(i+4)=a*x(i+4)
          ENDDO
        ELSE
          !Nothing special use intrinsic Fortran array operators
          x(1:n)=a*x(1:n)
        ENDIF
      ELSE
        !I'm not sure about this coding because i will become > n
        nincx=n*incx
        DO i=1,nincx,incx
          x(i)=a*x(i)
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDSUBROUTINE dscal_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dscal_all "dscal_all" for when
!> the user only wants to pass @c n, @c a and @c x. The increment is set to 1.
!> @param n the size of the vector @c x
!> @param a the constant to multiply with @c x
!> @param x the double-precision real vector to operate on
!>
  PURE SUBROUTINE dscal_noINCX(n,a,x)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: a
    REAL(SDK),INTENT(INOUT) :: x(*)
    CALL dscal_all(n,a,x,1)
  ENDSUBROUTINE dscal_noINCX
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dscal_all "dscal_all" for when
!> the user only wants to pass @c a, @c x, and @incx.
!> @param a the constant to multiply with @c x
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!>
  PURE SUBROUTINE dscal_noN(a,x,incx)
    REAL(SDK),INTENT(IN) :: a
    REAL(SDK),INTENT(INOUT) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    CALL dscal_all(SIZE(x),a,x,incx)
  ENDSUBROUTINE dscal_noN
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dscal_all "dscal_all" for when
!> the user only wants to pass @c a and @c x. The increment is set to 1.
!> @param a the constant to multiply with @c x
!> @param x the double-precision real vector to operate on
!>
  PURE SUBROUTINE dscal_noNINCX(a,x)
    REAL(SDK),INTENT(IN) :: a
    REAL(SDK),INTENT(INOUT) :: x(:)
    CALL dscal_all(SIZE(x),a,x,1)
  ENDSUBROUTINE dscal_noNINCX
!
!-------------------------------------------------------------------------------
!> @brief Subroutine rescales a vector @c x by a vector @c a for double
!> precision values.
!> @param n the size of the vector @c x
!> @param a the vector to multiply with @c x
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x and @c a
!>
!> This routine is an extension to the BLAS functionality where @c a is a vector
!> which is multiplied with @c x on an element-wise basis.
!>
  PURE SUBROUTINE dscal_vecA_all(n,a,x,incx)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: a(*)
    REAL(SDK),INTENT(INOUT) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    INTEGER(SIK) :: i,nincx,m
    INTRINSIC MOD
    
    IF(n > 0 .AND. incx > 0) THEN
      IF(incx == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 5 times
          m=MOD(n,5)
          IF(m /= 0) x(1:m)=a(1:m)*x(1:m)
          DO i=m+1,n,5
            x(i)=a(i)*x(i)
            x(i+1)=a(i+1)*x(i+1)
            x(i+2)=a(i+2)*x(i+2)
            x(i+3)=a(i+3)*x(i+3)
            x(i+4)=a(i+4)*x(i+4)
          ENDDO
        ELSE
          !Nothing special use intrinsic Fortran array operators
          x(1:n)=a(1:n)*x(1:n)
        ENDIF
      ELSE
        !I'm not sure about this coding because i will become > n
        nincx=n*incx
        DO i=1,nincx,incx
          x(i)=a(i)*x(i)
        ENDDO
      ENDIF
    ENDIF
  ENDSUBROUTINE dscal_vecA_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dscal_vecA_all "dscal_vecA_all" for when
!> the user only wants to pass @c n, @c a and @c x. The increment is set to 1.
!> @param n the size of the vector @c x
!> @param a the vector to multiply with @c x
!> @param x the double-precision real vector to operate on
!>
  PURE SUBROUTINE dscal_vecA_noINCX(n,a,x)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: a(*)
    REAL(SDK),INTENT(INOUT) :: x(*)
    CALL dscal_vecA_all(n,a,x,1)
  ENDSUBROUTINE dscal_vecA_noINCX
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dscal_vecA_all "dscal_vecA_all" for when
!> the user only wants to pass @c a, @c x, and @incx.
!> @param a the vector to multiply with @c x
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!>
!> The sizes of @c a and @c x must be the same.
!>
  PURE SUBROUTINE dscal_vecA_noN(a,x,incx)
    REAL(SDK),INTENT(IN) :: a(:)
    REAL(SDK),INTENT(INOUT) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    IF(SIZE(x) == SIZE(a)) CALL dscal_vecA_all(SIZE(x),a,x,incx)
  ENDSUBROUTINE dscal_vecA_noN
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dscal_vecA_all "dscal_vecA_all" for when
!> the user only wants to pass @c a and @c x. The increment is set to 1.
!> @param a the vector to multiply with @c x
!> @param x the double-precision real vector to operate on
!>
!> The sizes of @c a and @c x must be the same.
!>
  PURE SUBROUTINE dscal_vecA_noNINCX(a,x)
    REAL(SDK),INTENT(IN) :: a(:)
    REAL(SDK),INTENT(INOUT) :: x(:)
    IF(SIZE(x) == SIZE(a)) CALL dscal_vecA_all(SIZE(x),a,x,1)
  ENDSUBROUTINE dscal_vecA_noNINCX
!
!-------------------------------------------------------------------------------
!> @brief Function computes the 2-norm of a single-precision vector.
!> @param n the size of the vector @c x
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return norm2 the 2-norm of @c x
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based
!> somewhat on the code available on http://netlib.org/blas/snrm2.f but has some
!> modifications. Loop unrolling is added for vectors with more than 100
!> elements and an increment of The value of 100 was just chosen arbitrarily and
!> a value leading to more optimal performance most certainly exists but will be
!> architecture dependent.
!>
!> The netlib implementation uses an algorithm that rescales the sum as its 
!> computed. This is what is used when the increment is not 1 or the size of
!> @c x is smaller than 100. It is assumed that the rescaling approach will
!> produce a result with less round-off error.
!>
  PURE FUNCTION snrm2_all(n,x,incx) RESULT(norm2)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SSK) :: norm2
#ifdef HAVE_BLAS
    INTERFACE
      PURE REAL(KIND(0.0e0)) FUNCTION snrm2(n,x,incx)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0e0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
      ENDFUNCTION snrm2
    ENDINTERFACE
    norm2=snrm2(n,x,incx)
#else
    INTEGER(SIK) :: i,m
    REAL(SSK) :: sum2,r,rscale
    INTRINSIC ABS,MOD,SUM,SQRT
    
    norm2=0.0_SSK
    IF(n == 1) norm2=ABS(x(1))
    IF(n > 1 .AND. incx > 0) THEN
      IF(incx == 1 .AND. n > 100) THEN
        !Manually unroll 4 times
        sum2=0.0_SSK
        m=MOD(n,4)
        IF(m /= 0) sum2=SUM(x(1:m)*x(1:m))
        DO i=m+1,n,4
          sum2=sum2+x(i)*x(i)+x(i+1)*x(i+1)+x(i+2)*x(i+2)+x(i+3)*x(i+3)
        ENDDO
        norm2=SQRT(sum2)
      ELSE
        !This is similar to what is in http://netlib.org/blas/snrm2.f but I 
        !think I was able to improve it slightly by moving divisions into one
        !part of the loop and eliminating the first iteration of the loop.
        !
        !In general I don't think this implementation would be as fast as the
        !one I've implemented above. I'm curious as to why they did it this way.
        !My guess is that it might be more accurate if the values in x vary
        !over large orders of magnitude because this will cause non-trivial
        !floating point round-off error. As its implemented below it rescales
        !the elements on the fly and keeps all the values and the sum in a
        !smaller range of magnitudes.
        !
        !Also I think the loop will produce values of i greater than n
        sum2=0.0_SSK
        rscale=1.0_SSK
        IF(x(1) /= 0.0_SSK) THEN
            rscale=1.0_SSK/ABS(x(1))
            sum2=1.0_SSK
        ENDIF
        DO i=1+incx,1+(n-1)*incx,incx
          IF(x(i) /= 0.0_SSK) THEN
            r=ABS(x(i))*rscale
            IF(1._SSK < r) THEN
              sum2=1.0_SSK+sum2/(r*r)
              rscale=rscale/r
            ELSE
              sum2=sum2+r*r
            ENDIF
          ENDIF
        ENDDO
        norm2=SQRT(sum2)/rscale
      ENDIF
    ENDIF
#endif
  ENDFUNCTION snrm2_all
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::snrm2_all "snrm2_all" for when the user
!> only wants to pass @c x and @c incx.
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return norm2 the 2-norm of @c x
!>
  PURE FUNCTION snrm2_noN(x,incx) RESULT(norm2)
    REAL(SSK),INTENT(IN) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SSK) :: norm2
    norm2=snrm2_all(SIZE(x),x,incx)
  ENDFUNCTION snrm2_noN
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::snrm2_all "snrm2_all" for when the
!> user only wants to pass @c n and @c x.
!> @param n the size of the vector @c x
!> @param x the single-precision real vector to operate on
!> @return norm2 the 2-norm of @c x
!>
!> It might be more efficient to copy the block of code from @ref 
!> BLAS1::snrm2_all "snrm2_all" for @c incx==1 but I am hoping that if
!> inter-procedural optimizations are turned on for the compiler optimizations
!> it will do this correctly.
!>
  PURE FUNCTION snrm2_noINCX(n,x) RESULT(norm2)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    REAL(SSK) :: norm2
    norm2=snrm2_all(n,x,1)
  ENDFUNCTION snrm2_noINCX
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::snrm2_all "snrm2_all" for when user only 
!> wants to pass @c x.
!> @param x the single-precision real vector to operate on
!> @return norm2 the 2-norm of @c x
!>
  PURE FUNCTION snrm2_onlyX(x) RESULT(norm2)
    REAL(SSK),INTENT(IN) :: x(:)
    REAL(SSK) :: norm2
    norm2=snrm2_all(SIZE(x),x,1)
  ENDFUNCTION snrm2_onlyX
!
!-------------------------------------------------------------------------------
!> @brief Function computes the 2-norm of a double-precision vector.
!> @param n the size of the vector @c x
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return norm2 the 2-norm of @c x
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based
!> somewhat on the code available on http://netlib.org/blas/dnrm2.f but has some
!> modifications. Loop unrolling is added for vectors with more than 100
!> elements and an increment of The value of 100 was just chosen arbitrarily and
!> a value leading to more optimal performance most certainly exists but will be
!> architecture dependent.
!>
!> The netlib implementation uses an algorithm that rescales the sum as its 
!> computed. This is what is used when the increment is not 1 or the size of
!> @c x is smaller than 100. It is assumed that the rescaling approach will
!> produce a result with less round-off error.
!>
  PURE FUNCTION dnrm2_all(n,x,incx) RESULT(norm2)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SDK) :: norm2
#ifdef HAVE_BLAS
    INTERFACE
      PURE REAL(KIND(0.0d0)) FUNCTION dnrm2(n,x,incx)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0d0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
      ENDFUNCTION dnrm2
    ENDINTERFACE
    norm2=dnrm2(n,x,incx)
#else
    INTEGER(SIK) :: i,m
    REAL(SDK) :: sum2,r,rscale
    INTRINSIC ABS,MOD,SUM,SQRT
    
    norm2=0.0_SDK
    IF(n == 1) norm2=ABS(x(1))
    IF(n > 1 .AND. incx > 0) THEN
      IF(incx == 1 .AND. n > 100) THEN
        !Manually unroll 4 times
        sum2=0.0_SRK
        m=MOD(n,4)
        IF(m /= 0) sum2=SUM(x(1:m)*x(1:m))
        DO i=m+1,n,4
          sum2=sum2+x(i)*x(i)+x(i+1)*x(i+1)+x(i+2)*x(i+2)+x(i+3)*x(i+3)
        ENDDO
        norm2=SQRT(sum2)
      ELSE
        !This is similar to what is in http://netlib.org/blas/dnrm2.f but I 
        !think I was able to improve it slightly by moving divisions into one
        !part of the loop and eliminating the first iteration of the loop.
        !
        !In general I don't think this implementation would be as fast as the
        !one I've implemented above. I'm curious as to why they did it this way.
        !My guess is that it might be more accurate if the values in x vary
        !over large orders of magnitude because this will cause non-trivial
        !floating point round-off error. As its implemented below it rescales
        !the elements on the fly and keeps all the values and the sum in a
        !smaller range of magnitudes.
        !
        !Also I think the loop will produce values of i greater than n
        sum2=0.0_SDK
        rscale=1.0_SDK
        IF(x(1) /= 0.0_SDK) THEN
          rscale=1.0_SDK/ABS(x(1))
          sum2=1.0_SDK
        ENDIF
        DO i=1+incx,1+(n-1)*incx,incx
          IF(x(i) /= 0.0_SDK) THEN
            r=ABS(x(i))*rscale
            IF(1._SDK < r) THEN
              sum2=1.0_SDK+sum2/(r*r)
              rscale=rscale/r
            ELSE
              sum2=sum2+r*r
            ENDIF
          ENDIF
        ENDDO
        norm2=SQRT(sum2)/rscale
      ENDIF
    ENDIF
#endif
  ENDFUNCTION dnrm2_all
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::dnrm2_all "dnrm2_all" for when the user
!> only wants to pass @c x and @c incx.
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return norm2 the 2-norm of @c x
!>
  PURE FUNCTION dnrm2_noN(x,incx) RESULT(norm2)
    REAL(SDK),INTENT(IN) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SDK) :: norm2
    norm2=dnrm2_all(SIZE(x),x,incx)
  ENDFUNCTION dnrm2_noN
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::dnrm2_all "dnrm2_all" for when the
!> user only wants to pass @c n and @c x.
!> @param n the size of the vector @c x
!> @param x the double-precision real vector to operate on
!> @return norm2 the 2-norm of @c x
!>
!> It might be more efficient to copy the block of code from @ref 
!> BLAS1::dnrm2_all "dnrm2_all" for @c incx==1 but I am hoping that if
!> inter-procedural optimizations are turned on for the compiler optimizations
!> it will do this correctly.
!>
  PURE FUNCTION dnrm2_noINCX(n,x) RESULT(norm2)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    REAL(SDK) :: norm2
    norm2=dnrm2_all(n,x,1)
  ENDFUNCTION dnrm2_noINCX
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::dnrm2_all "dnrm2_all" for when user only 
!> wants to pass @c x.
!> @param x the double-precision real vector to operate on
!> @return norm2 the 2-norm of @c x
!>
  PURE FUNCTION dnrm2_onlyX(x) RESULT(norm2)
    REAL(SDK),INTENT(IN) :: x(:)
    REAL(SDK) :: norm2
    norm2=dnrm2_all(SIZE(x),x,1)
  ENDFUNCTION dnrm2_onlyX
!
!-------------------------------------------------------------------------------
!> @brief Subroutine copies a vector @c x to a vector @c y
!> @param n the number of elements to operate on
!> @param x the single-precision real vector to copy to @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the single-precision real vector that is the return argument
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/scopy.f but has some minor
!> modifications. The loop unrolling is not done for any vector with increment
!> 1, but now only for vectors with size greater than 100. For those less than
!> that, the intrinsic Fortran operations are used. The value of 100 was just
!> chosen arbitrarily and a value leading to more optimal performance most
!> certainly exists but will be architecture dependent.
!>
  PURE SUBROUTINE scopy_all(n,x,incx,y,incy)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SSK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
    INTERFACE
      PURE SUBROUTINE scopy(n,x,incx,y,incy)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0e0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
        REAL(KIND(0.0e0)),INTENT(INOUT) :: y(*)
        INTEGER,INTENT(IN) :: incy
      ENDSUBROUTINE
    ENDINTERFACE
    CALL scopy(n,x,incx,y,incy)
#else
    INTEGER(SIK) :: i,ix,iy,m
    INTRINSIC MOD
    
    IF(n > 0) THEN
      IF(incx == 1 .AND. incy == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 7 times
          m=MOD(n,7)
          IF(m /= 0) y(1:m)=x(1:m)
          DO i=m+1,n,7
            y(i)=x(i)
            y(i+1)=x(i+1)
            y(i+2)=x(i+2)
            y(i+3)=x(i+3)
            y(i+4)=x(i+4)
            y(i+5)=x(i+5)
            y(i+6)=x(i+6)
          ENDDO
        ELSE
          !Nothing fancy, just use intrinsic Fortran operations
          y(1:n)=x(1:n)
        ENDIF
      ELSE
        ix=1
        iy=1
        IF(incx < 0) ix=(1-n)*incx+1
        IF(incy < 0) iy=(1-n)*incy+1
        !Nothing special just do the loop
        !I don't think this is correct because ix and iy will be larger than n
        !but it's what is in netlib
        DO i=1,n
          y(iy)=x(ix)
          ix=ix+incx
          iy=iy+incy
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDSUBROUTINE scopy_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::scopy_all "scopy_all" for when
!> the user only wants to pass @c x, @c y, @c incx, and @c incy.
!> @param x the single-precision real vector to copy to @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the single-precision real vector that is the return argument
!> @param incy the increment to use when looping over elements in @c y
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::scopy_all "scopy_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE scopy_noN(x,incx,y,incy)
    REAL(SSK),INTENT(IN) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SSK),INTENT(INOUT) :: y(:)
    INTEGER(SIK),INTENT(IN) :: incy
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL scopy_all(n,x,incx,y,incy)
  ENDSUBROUTINE scopy_noN
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::scopy_all "scopy_all" for when
!> the user only wants to pass @c n, @c x, @c y, and one increment to use for
!> both @c incx and @c incy.
!> @param n the number of elements to operate on
!> @param x the single-precision real vector to copy to @c y
!> @param y the single-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in @c x and @c y
!>
  PURE SUBROUTINE scopy_nxyinc(n,x,y,inc)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    REAL(SSK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: inc
    CALL scopy_all(n,x,inc,y,inc)
  ENDSUBROUTINE scopy_nxyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::scopy_all "scopy_all" for when
!> the user only wants to pass @c x, @c y, and and one increment to use for
!> both @c incx and @c incy.
!> @param x the single-precision real vector to copy to @c y
!> @param y the single-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in @c x and @c y
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::scopy_all "scopy_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE scopy_xyinc(x,y,inc)
    REAL(SSK),INTENT(IN) :: x(:)
    REAL(SSK),INTENT(INOUT) :: y(:)
    INTEGER(SIK),INTENT(IN) :: inc
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL scopy_all(n,x,inc,y,inc)
  ENDSUBROUTINE scopy_xyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::scopy_all "scopy_all" for when
!> the user only wants to pass @c n, @c x, and @c y. The increment is set to 1.
!> @param n the number of elements to operate on
!> @param x the single-precision real vector to copy to @c y
!> @param y the single-precision real vector that is the return argument
!>
  PURE SUBROUTINE scopy_nxy(n,x,y)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    REAL(SSK),INTENT(INOUT) :: y(*)
    CALL scopy_all(n,x,1,y,1)
  ENDSUBROUTINE scopy_nxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::scopy_all "scopy_all" for when
!> the user only wants to pass @c x and @c y. The increment is set to 1.
!> @param n the number of elements to operate on
!> @param x the single-precision real vector to copy to @c y
!> @param y the single-precision real vector that is the return argument
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::scopy_all "scopy_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE scopy_xy(x,y)
    REAL(SSK),INTENT(IN) :: x(:)
    REAL(SSK),INTENT(INOUT) :: y(:)
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL scopy_all(n,x,1,y,1)
  ENDSUBROUTINE scopy_xy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine copies a vector @c x to a vector @c y
!> @param n the number of elements to operate on
!> @param x the double-precision real vector to copy to @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the double-precision real vector that is the return argument
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dcopy.f but has some minor
!> modifications. The loop unrolling is not done for any vector with increment
!> 1, but now only for vectors with size greater than 100. For those less than
!> that, the intrinsic Fortran operations are used. The value of 100 was just
!> chosen arbitrarily and a value leading to more optimal performance most
!> certainly exists but will be architecture dependent.
!>
  PURE SUBROUTINE dcopy_all(n,x,incx,y,incy)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SDK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
    INTERFACE
      PURE SUBROUTINE dcopy(n,x,incx,y,incy)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0d0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
        REAL(KIND(0.0d0)),INTENT(INOUT) :: y(*)
        INTEGER,INTENT(IN) :: incy
      ENDSUBROUTINE
    ENDINTERFACE
    CALL dcopy(n,x,incx,y,incy)
#else
    INTEGER(SIK) :: i,ix,iy,m
    INTRINSIC MOD
    
    IF(n > 0) THEN
      IF(incx == 1 .AND. incy == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 7 times
          m=MOD(n,7)
          IF(m /= 0) y(1:m)=x(1:m)
          DO i=m+1,n,7
            y(i)=x(i)
            y(i+1)=x(i+1)
            y(i+2)=x(i+2)
            y(i+3)=x(i+3)
            y(i+4)=x(i+4)
            y(i+5)=x(i+5)
            y(i+6)=x(i+6)
          ENDDO
        ELSE
          !Nothing fancy, just use intrinsic Fortran operations
          y(1:n)=x(1:n)
        ENDIF
      ELSE
        ix=1
        iy=1
        IF(incx < 0) ix=(1-n)*incx+1
        IF(incy < 0) iy=(1-n)*incy+1
        !Nothing special just do the loop
        !I don't think this is correct because ix and iy will be larger than n
        !but it's what is in netlib
        DO i=1,n
          y(iy)=x(ix)
          ix=ix+incx
          iy=iy+incy
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDSUBROUTINE dcopy_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dcopy_all "dcopy_all" for when
!> the user only wants to pass @c x, @c y, @c incx, and @c incy.
!> @param x the double-precision real vector to copy to @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the double-precision real vector that is the return argument
!> @param incy the increment to use when looping over elements in @c y
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::dcopy_all "dcopy_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE dcopy_noN(x,incx,y,incy)
    REAL(SDK),INTENT(IN) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SDK),INTENT(INOUT) :: y(:)
    INTEGER(SIK),INTENT(IN) :: incy
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL dcopy_all(n,x,incx,y,incy)
  ENDSUBROUTINE dcopy_noN
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dcopy_all "dcopy_all" for when
!> the user only wants to pass @c n, @c x, @c y, and one increment to use for
!> both @c incx and @c incy.
!> @param n the number of elements to operate on
!> @param x the double-precision real vector to copy to @c y
!> @param y the double-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in @c x and @c y
!>
  PURE SUBROUTINE dcopy_nxyinc(n,x,y,inc)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    REAL(SDK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: inc
    CALL dcopy_all(n,x,inc,y,inc)
  ENDSUBROUTINE dcopy_nxyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dcopy_all "dcopy_all" for when
!> the user only wants to pass @c x, @c y, and and one increment to use for
!> both @c incx and @c incy.
!> @param x the double-precision real vector to copy to @c y
!> @param y the double-precision real vector that is the return argument
!> @param inc the increment to use when looping over elements in @c x and @c y
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::dcopy_all "dcopy_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE dcopy_xyinc(x,y,inc)
    REAL(SDK),INTENT(IN) :: x(:)
    REAL(SDK),INTENT(INOUT) :: y(:)
    INTEGER(SIK),INTENT(IN) :: inc
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL dcopy_all(n,x,inc,y,inc)
  ENDSUBROUTINE dcopy_xyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dcopy_all "dcopy_all" for when
!> the user only wants to pass @c n, @c x, and @c y. The increment is set to 1.
!> @param n the number of elements to operate on
!> @param x the double-precision real vector to copy to @c y
!> @param y the double-precision real vector that is the return argument
!>
  PURE SUBROUTINE dcopy_nxy(n,x,y)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    REAL(SDK),INTENT(INOUT) :: y(*)
    CALL dcopy_all(n,x,1,y,1)
  ENDSUBROUTINE dcopy_nxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dcopy_all "dcopy_all" for when
!> the user only wants to pass @c x and @c y. The increment is set to 1.
!> @param n the number of elements to operate on
!> @param x the double-precision real vector to copy to @c y
!> @param y the double-precision real vector that is the return argument
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::dcopy_all "dcopy_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE dcopy_xy(x,y)
    REAL(SDK),INTENT(IN) :: x(:)
    REAL(SDK),INTENT(INOUT) :: y(:)
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL dcopy_all(n,x,1,y,1)
  ENDSUBROUTINE dcopy_xy
!
!-------------------------------------------------------------------------------
!> @brief Function computes the dot product of two single precision vectors.
!> @param n the size of the vectors @c x and @c y
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @param y the single-precision real vector to operate on
!> @param incy the increment to use when looping over elements in @c y
!> @return r the dot product of @c x and @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sdot.f but has some minor
!> modifications. The loop unrolling is not done for any vector with increment
!> 1, but now only for vectors with size greater than 100. For those less than
!> that, the intrinsic Fortran functions are used. The value of 100 was just
!> chosen arbitrarily and a value leading to more optimal performance most
!> certainly exists but will be architecture dependent.
!>
  PURE FUNCTION sdot_all(n,x,incx,y,incy) RESULT(r)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SSK),INTENT(IN) :: y(*)
    INTEGER(SIK),INTENT(IN) :: incy
    REAL(SSK) :: r
#ifdef HAVE_BLAS
    INTERFACE
      PURE REAL(KIND(0.0e0)) FUNCTION sdot(n,x,incx,y,incy)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0e0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
        REAL(KIND(0.0e0)),INTENT(IN) :: y(*)
        INTEGER,INTENT(IN) :: incy
      ENDFUNCTION sdot
    ENDINTERFACE
    r=sdot(n,x,incx,y,incy)
#else
    INTEGER(SIK) :: i,ix,iy,m
    INTRINSIC MOD,SUM
    
    r=0.0_SSK
    IF(n > 0) THEN
      IF(incx == 1 .AND. incy == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 5 times
          m=MOD(n,5)
          IF(m /= 0) r=SUM(x(1:m)*y(1:m))
          DO i=m+1,n,5
            r=r+x(i)*y(i)+x(i+1)*y(i+1)+x(i+2)*y(i+2)+x(i+3)*y(i+3)+x(i+4)*y(i+4)
          ENDDO
        ELSE
          !Nothing special, just use intrinsic Fortran array operations
          r=SUM(x(1:n)*y(1:n))
        ENDIF
      ELSE
        !Nothing special, just write the loop for variable increments
        ix=1
        iy=1
        IF(incx < 0) ix=(1-n)*incx+1
        IF(incy < 0) iy=(1-n)*incy+1
        DO i=1,n
          r=r+x(ix)*y(iy)
          ix=ix+incx
          iy=iy+incy
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDFUNCTION sdot_all
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::sdot_all "sdot_all" for when the user
!> only wants to pass @c x, @c incx, @c y, and @c incy.
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @param y the single-precision real vector to operate on
!> @param incy the increment to use when looping over elements in @c y
!> @return r the dot product of @c x and @c y
!>
  PURE FUNCTION sdot_noN(x,incx,y,incy) RESULT(r)
    REAL(SSK),INTENT(IN) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SSK),INTENT(IN) :: y(:)
    INTEGER(SIK),INTENT(IN) :: incy
    REAL(SSK) :: r
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    r=sdot_all(n,x,incx,y,incy)
  ENDFUNCTION sdot_noN
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::sdot_all "sdot_all" for when the user
!> only wants to pass @c n, @c x, @c y, and one increment to use for @c x and 
!> @c y.
!> @param n the size of the vectors @c x and @c y
!> @param x the single-precision real vector to operate on
!> @param y the single-precision real vector to operate on
!> @param inc the increment to use when looping over elements in @c x and @c y
!> @return r the dot product of @c x and @c y
!>
  PURE FUNCTION sdot_nxyinc(n,x,y,inc) RESULT(r)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    REAL(SSK),INTENT(IN) :: y(*)
    INTEGER(SIK),INTENT(IN) :: inc
    REAL(SSK) :: r
    r=sdot_all(n,x,inc,y,inc)
  ENDFUNCTION sdot_nxyinc
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::sdot_all "sdot_all" for when the user
!> only wants to pass @c n, @c x, and @c y. The increment is set to 1.
!> @param n the size of the vectors @c x and @c y
!> @param x the single-precision real vector to operate on
!> @param y the single-precision real vector to operate on
!> @return r the dot product of @c x and @c y
!>
!> It might be more efficient to copy the block of code from @ref 
!> BLAS1::sdot_all "sdot_all" for @c incx==1 and @c incy==1 but I am hoping
!> that if inter-procedural optimizations are turned on for the compiler
!> optimizations it will do this correctly.
!>
  PURE FUNCTION sdot_nxy(n,x,y) RESULT(r)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    REAL(SSK),INTENT(IN) :: y(*)
    REAL(SSK) :: r
    r=sdot_all(n,x,1,y,1)
  ENDFUNCTION sdot_nxy
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::sdot_all "sdot_all" for when the user
!> only wants to pass @c x, @c y, and one increment to use for @c x and @c y.
!> @param x the single-precision real vector to operate on
!> @param y the single-precision real vector to operate on
!> @param inc the increment to use when looping over elements in @c x and @c y
!> @return r the dot product of @c x and @c y
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::sdot_all "sdot_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE FUNCTION sdot_xyinc(x,y,inc) RESULT(r)
    REAL(SSK),INTENT(IN) :: x(:)
    REAL(SSK),INTENT(IN) :: y(:)
    INTEGER(SIK),INTENT(IN) :: inc
    REAL(SSK) :: r
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    r=sdot_all(n,x,inc,y,inc)
  ENDFUNCTION sdot_xyinc
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::sdot_all "sdot_all" for when the user
!> only wants to pass @c x and @c y. The increment is set to 1.
!> @param x the single-precision real vector to operate on
!> @param y the single-precision real vector to operate on
!> @return r the dot product of @c x and @c y
!>
!> It might be more efficient to copy the block of code from @ref 
!> BLAS1::sdot_all "sdot_all" for @c incx==1 and @c incy==1 but I am hoping
!> that if inter-procedural optimizations are turned on for the compiler
!> optimizations it will do this correctly.
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::sdot_all "sdot_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE FUNCTION sdot_xy(x,y) RESULT(r)
    REAL(SSK),INTENT(IN) :: x(:)
    REAL(SSK),INTENT(IN) :: y(:)
    REAL(SSK) :: r
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    r=sdot_all(n,x,1,y,1)
  ENDFUNCTION sdot_xy
!
!-------------------------------------------------------------------------------
!> @brief Function computes the dot product of two double precision vectors.
!> @param n the size of the vectors @c x and @c y
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @param y the double-precision real vector to operate on
!> @param incy the increment to use when looping over elements in @c y
!> @return r the dot product of @c x and @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/ddot.f but has some minor
!> modifications. The loop unrolling is not done for any vector with increment
!> 1, but now only for vectors with size greater than 100. For those less than
!> that, the intrinsic Fortran functions are used. The value of 100 was just
!> chosen arbitrarily and a value leading to more optimal performance most
!> certainly exists but will be architecture dependent.
!>
  PURE FUNCTION ddot_all(n,x,incx,y,incy) RESULT(r)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SDK),INTENT(IN) :: y(*)
    INTEGER(SIK),INTENT(IN) :: incy
    REAL(SDK) :: r
#ifdef HAVE_BLAS
    INTERFACE
      PURE REAL(KIND(0.0d0)) FUNCTION ddot(n,x,incx,y,incy)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0d0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
        REAL(KIND(0.0d0)),INTENT(IN) :: y(*)
        INTEGER,INTENT(IN) :: incy
      ENDFUNCTION ddot
    ENDINTERFACE
    r=ddot(n,x,incx,y,incy)
#else
    INTEGER(SIK) :: i,ix,iy,m
    INTRINSIC MOD,SUM
    
    r=0.0_SDK
    IF(n > 0) THEN
      IF(incx == 1 .AND. incy == 1) THEN
        IF(n > 100) THEN
          !Manually unroll loop 5 times
          m=MOD(n,5)
          IF(m /= 0) r=SUM(x(1:m)*y(1:m))
          DO i=m+1,n,5
            r=r+x(i)*y(i)+x(i+1)*y(i+1)+x(i+2)*y(i+2)+x(i+3)*y(i+3)+x(i+4)*y(i+4)
          ENDDO
        ELSE
          !Nothing special, just use intrinsic Fortran array operations
          r=SUM(x(1:n)*y(1:n))
        ENDIF
      ELSE
        !Nothing special, just write the loop for variable increments
        ix=1
        iy=1
        IF(incx < 0) ix=(1-n)*incx+1
        IF(incy < 0) iy=(1-n)*incy+1
        DO i=1,n
          r=r+x(ix)*y(iy)
          ix=ix+incx
          iy=iy+incy
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDFUNCTION ddot_all
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::ddot_all "ddot_all" for when the user
!> only wants to pass @c x, @c incx, @c y, and @c incy.
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @param y the double-precision real vector to operate on
!> @param incy the increment to use when looping over elements in @c y
!> @return r the dot product of @c x and @c y
!>
  PURE FUNCTION ddot_noN(x,incx,y,incy) RESULT(r)
    REAL(SDK),INTENT(IN) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SDK),INTENT(IN) :: y(:)
    INTEGER(SIK),INTENT(IN) :: incy
    REAL(SDK) :: r
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    r=ddot_all(n,x,incx,y,incy)
  ENDFUNCTION ddot_noN
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::ddot_all "ddot_all" for when the user
!> only wants to pass @c n, @c x, @c y, and one increment to use for @c x and 
!> @c y.
!> @param n the size of the vectors @c x and @c y
!> @param x the double-precision real vector to operate on
!> @param y the double-precision real vector to operate on
!> @param inc the increment to use when looping over elements in @c x and @c y
!> @return r the dot product of @c x and @c y
!>
  PURE FUNCTION ddot_nxyinc(n,x,y,inc) RESULT(r)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    REAL(SDK),INTENT(IN) :: y(*)
    INTEGER(SIK),INTENT(IN) :: inc
    REAL(SDK) :: r
    r=ddot_all(n,x,inc,y,inc)
  ENDFUNCTION ddot_nxyinc
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::ddot_all "ddot_all" for when the user
!> only wants to pass @c n, @c x, and @c y. The increment is set to 1.
!> @param n the size of the vectors @c x and @c y
!> @param x the double-precision real vector to operate on
!> @param y the double-precision real vector to operate on
!> @return r the dot product of @c x and @c y
!>
!> It might be more efficient to copy the block of code from @ref 
!> BLAS1::ddot_all "ddot_all" for @c incx==1 and @c incy==1 but I am hoping
!> that if inter-procedural optimizations are turned on for the compiler
!> optimizations it will do this correctly.
!>
  PURE FUNCTION ddot_nxy(n,x,y) RESULT(r)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    REAL(SDK),INTENT(IN) :: y(*)
    REAL(SDK) :: r
    r=ddot_all(n,x,1,y,1)
  ENDFUNCTION ddot_nxy
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::ddot_all "ddot_all" for when the user
!> only wants to pass @c x, @c y, and one increment to use for @c x and @c y.
!> @param x the double-precision real vector to operate on
!> @param y the double-precision real vector to operate on
!> @param inc the increment to use when looping over elements in @c x and @c y
!> @return r the dot product of @c x and @c y
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::ddot_all "ddot_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE FUNCTION ddot_xyinc(x,y,inc) RESULT(r)
    REAL(SDK),INTENT(IN) :: x(:)
    REAL(SDK),INTENT(IN) :: y(:)
    INTEGER(SIK),INTENT(IN) :: inc
    REAL(SDK) :: r
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    r=ddot_all(n,x,inc,y,inc)
  ENDFUNCTION ddot_xyinc
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::ddot_all "ddot_all" for when the user
!> only wants to pass @c x and @c y. The increment is set to 1.
!> @param x the double-precision real vector to operate on
!> @param y the double-precision real vector to operate on
!> @return r the dot product of @c x and @c y
!>
!> It might be more efficient to copy the block of code from @ref 
!> BLAS1::ddot_all "ddot_all" for @c incx==1 and @c incy==1 but I am hoping
!> that if inter-procedural optimizations are turned on for the compiler
!> optimizations it will do this correctly.
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::ddot_all "ddot_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE FUNCTION ddot_xy(x,y) RESULT(r)
    REAL(SDK),INTENT(IN) :: x(:)
    REAL(SDK),INTENT(IN) :: y(:)
    REAL(SDK) :: r
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    r=ddot_all(n,x,1,y,1)
  ENDFUNCTION ddot_xy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine swaps a vector @c x with a vector @c y
!> @param n the number of elements to operate on
!> @param x the single-precision real vector to swap with @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the single-precision real vector to swap with @c x
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sswap.f but has some minor
!> modifications. The temporary swap variables are defined as separate 
!> variables when unrolling the loop. This may improve the ability of the 
!> compiler to optimize the loop by eliminating dependencies between the 
!> operations for elements @c i, @c i+1, and @c i+2.
!>
  PURE SUBROUTINE sswap_all(n,x,incx,y,incy)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(INOUT) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SSK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
    INTERFACE
      PURE SUBROUTINE sswap(n,x,incx,y,incy)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0e0)),INTENT(INOUT) :: x(*)
        INTEGER,INTENT(IN) :: incx
        REAL(KIND(0.0e0)),INTENT(INOUT) :: y(*)
        INTEGER,INTENT(IN) :: incy
      ENDSUBROUTINE sswap
    ENDINTERFACE
    CALL sswap(n,x,incx,y,incy)
#else
    INTEGER(SIK) :: i,ix,iy,m
    REAL(SSK) :: stmp1,stmp2,stmp3
    INTRINSIC MOD
    
    IF(n > 0) THEN
      IF(incx == 1 .AND. incy == 1) THEN
        !Manually unroll loop 3 times
        m=MOD(n,3)
        IF(m /= 0) THEN
          DO i=1,m
            stmp1=x(i)
            x(i)=y(i)
            y(i)=stmp1
          ENDDO
        ENDIF
        DO i=m+1,n,3
          stmp1=x(i)
          stmp2=x(i+1)
          stmp3=x(i+2)
          x(i)=y(i)
          x(i+1)=y(i+1)
          x(i+2)=y(i+2)
          y(i)=stmp1
          y(i+1)=stmp2
          y(i+2)=stmp3
        ENDDO
      ELSE
        !Nothing special, just do the loop for variable increments
        ix=1
        iy=1
        IF(incx < 0) ix=(1-n)*incx+1
        IF(incy < 0) iy=(1-n)*incy+1
        DO i=1,n
          stmp1=x(ix)
          x(ix)=y(iy)
          y(iy)=stmp1
          ix=ix+incx
          iy=iy+incy
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDSUBROUTINE sswap_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::sswap_all "sswap_all" for when
!> the user only wants to pass @c x, @c y, @c incx, and @c incy.
!> @param x the single-precision real vector to swap with @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the single-precision real vector to swap with @c x
!> @param incy the increment to use when looping over elements in @c y
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::ddot_all "ddot_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE sswap_noN(x,incx,y,incy)
    REAL(SSK),INTENT(INOUT) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SSK),INTENT(INOUT) :: y(:)
    INTEGER(SIK),INTENT(IN) :: incy
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL sswap_all(n,x,incx,y,incy)
  ENDSUBROUTINE sswap_noN
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::sswap_all "sswap_all" for when
!> the user only wants to pass @c n @c x, @c y, and one increment for both 
!> @c incx and @c incy.
!> @param n the number of elements to operate on
!> @param x the single-precision real vector to swap with @c y
!> @param y the single-precision real vector to swap with @c x
!> @param inc the increment to use when looping over elements in @c y
!>
  PURE SUBROUTINE sswap_nxyinc(n,x,y,inc)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(INOUT) :: x(*)
    REAL(SSK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: inc
    CALL sswap_all(n,x,inc,y,inc)
  ENDSUBROUTINE sswap_nxyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::sswap_all "sswap_all" for when
!> the user only wants to pass @c x, @c y, and one increment for both 
!> @c incx and @c incy.
!> @param x the single-precision real vector to swap with @c y
!> @param y the single-precision real vector to swap with @c x
!> @param inc the increment to use when looping over elements in @c y
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::ddot_all "ddot_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE sswap_xyinc(x,y,inc)
    REAL(SSK),INTENT(INOUT) :: x(:)
    REAL(SSK),INTENT(INOUT) :: y(:)
    INTEGER(SIK),INTENT(IN) :: inc
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL sswap_all(n,x,inc,y,inc)
  ENDSUBROUTINE sswap_xyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::sswap_all "sswap_all" for when
!> the user only wants to pass @c n, @c x, and @c y. The increment is set to 1.
!> @param n the number of elements to operate on
!> @param x the single-precision real vector to swap with @c y
!> @param y the single-precision real vector to swap with @c x
!>
  PURE SUBROUTINE sswap_nxy(n,x,y)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(INOUT) :: x(*)
    REAL(SSK),INTENT(INOUT) :: y(*)
    CALL sswap_all(n,x,1,y,1)
  ENDSUBROUTINE sswap_nxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::sswap_all "sswap_all" for when
!> the user only wants to pass @c x and @c y. The increment is set to 1.
!> @param x the single-precision real vector to swap with @c y
!> @param y the single-precision real vector to swap with @c x
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::ddot_all "ddot_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE sswap_xy(x,y)
    REAL(SSK),INTENT(INOUT) :: x(:)
    REAL(SSK),INTENT(INOUT) :: y(:)
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL sswap_all(n,x,1,y,1)
  ENDSUBROUTINE sswap_xy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine swaps a vector @c x with a vector @c y
!> @param n the number of elements to operate on
!> @param x the double-precision real vector to swap with @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the double-precision real vector to swap with @c x
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dswap.f but has some minor
!> modifications. The temporary swap variables are defined as separate 
!> variables when unrolling the loop. This may improve the ability of the 
!> compiler to optimize the loop by eliminating dependencies between the 
!> operations for elements @c i, @c i+1, and @c i+2.
!>
  PURE SUBROUTINE dswap_all(n,x,incx,y,incy)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(INOUT) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SDK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
    INTERFACE
      PURE SUBROUTINE dswap(n,x,incx,y,incy)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0d0)),INTENT(INOUT) :: x(*)
        INTEGER,INTENT(IN) :: incx
        REAL(KIND(0.0d0)),INTENT(INOUT) :: y(*)
        INTEGER,INTENT(IN) :: incy
      ENDSUBROUTINE dswap
    ENDINTERFACE
    CALL dswap(n,x,incx,y,incy)
#else
    INTEGER(SIK) :: i,ix,iy,m
    REAL(SDK) :: stmp1,stmp2,stmp3
    INTRINSIC MOD
    
    IF(n > 0) THEN
      IF(incx == 1 .AND. incy == 1) THEN
        !Manually unroll loop 3 times
        m=MOD(n,3)
        IF(m /= 0) THEN
          DO i=1,m
            stmp1=x(i)
            x(i)=y(i)
            y(i)=stmp1
          ENDDO
        ENDIF
        DO i=m+1,n,3
          stmp1=x(i)
          stmp2=x(i+1)
          stmp3=x(i+2)
          x(i)=y(i)
          x(i+1)=y(i+1)
          x(i+2)=y(i+2)
          y(i)=stmp1
          y(i+1)=stmp2
          y(i+2)=stmp3
        ENDDO
      ELSE
        !Nothing special, just do the loop for variable increments
        ix=1
        iy=1
        IF(incx < 0) ix=(1-n)*incx+1
        IF(incy < 0) iy=(1-n)*incy+1
        DO i=1,n
          stmp1=x(ix)
          x(ix)=y(iy)
          y(iy)=stmp1
          ix=ix+incx
          iy=iy+incy
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDSUBROUTINE dswap_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dswap_all "dswap_all" for when
!> the user only wants to pass @c x, @c y, @c incx, and @c incy.
!> @param x the double-precision real vector to swap with @c y
!> @param incx the increment to use when looping over elements in @c x
!> @param y the double-precision real vector to swap with @c x
!> @param incy the increment to use when looping over elements in @c y
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::ddot_all "ddot_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE dswap_noN(x,incx,y,incy)
    REAL(SDK),INTENT(INOUT) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    REAL(SDK),INTENT(INOUT) :: y(:)
    INTEGER(SIK),INTENT(IN) :: incy
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL dswap_all(n,x,incx,y,incy)
  ENDSUBROUTINE dswap_noN
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dswap_all "dswap_all" for when
!> the user only wants to pass @c n @c x, @c y, and one increment for both 
!> @c incx and @c incy.
!> @param n the number of elements to operate on
!> @param x the double-precision real vector to swap with @c y
!> @param y the double-precision real vector to swap with @c x
!> @param inc the increment to use when looping over elements in @c y
!>
  PURE SUBROUTINE dswap_nxyinc(n,x,y,inc)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(INOUT) :: x(*)
    REAL(SDK),INTENT(INOUT) :: y(*)
    INTEGER(SIK),INTENT(IN) :: inc
    CALL dswap_all(n,x,inc,y,inc)
  ENDSUBROUTINE dswap_nxyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dswap_all "dswap_all" for when
!> the user only wants to pass @c x, @c y, and one increment for both 
!> @c incx and @c incy.
!> @param x the double-precision real vector to swap with @c y
!> @param y the double-precision real vector to swap with @c x
!> @param inc the increment to use when looping over elements in @c y
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::ddot_all "ddot_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE dswap_xyinc(x,y,inc)
    REAL(SDK),INTENT(INOUT) :: x(:)
    REAL(SDK),INTENT(INOUT) :: y(:)
    INTEGER(SIK),INTENT(IN) :: inc
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL dswap_all(n,x,inc,y,inc)
  ENDSUBROUTINE dswap_xyinc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dswap_all "dswap_all" for when
!> the user only wants to pass @c n, @c x, and @c y. The increment is set to 1.
!> @param n the number of elements to operate on
!> @param x the double-precision real vector to swap with @c y
!> @param y the double-precision real vector to swap with @c x
!>
  PURE SUBROUTINE dswap_nxy(n,x,y)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(INOUT) :: x(*)
    REAL(SDK),INTENT(INOUT) :: y(*)
    CALL dswap_all(n,x,1,y,1)
  ENDSUBROUTINE dswap_nxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS1::dswap_all "dswap_all" for when
!> the user only wants to pass @c x and @c y. The increment is set to 1.
!> @param x the double-precision real vector to swap with @c y
!> @param y the double-precision real vector to swap with @c x
!>
!> This routine computes @c n as the minimum of the sizes @c x and @c y, so they
!> do not have to necessarily be the same size. We may decide later that this
!> routine should only call @ref BLAS1::ddot_all "ddot_all" if the sizes of
!> @c x and @c y are the same.
!>
  PURE SUBROUTINE dswap_xy(x,y)
    REAL(SDK),INTENT(INOUT) :: x(:)
    REAL(SDK),INTENT(INOUT) :: y(:)
    INTEGER(SIK) :: n
    n=MIN(SIZE(x),SIZE(y))
    CALL dswap_all(n,x,1,y,1)
  ENDSUBROUTINE dswap_xy
!
!-------------------------------------------------------------------------------
!> @brief Function returns the index of the absolute maximum of a single 
!> precision vector.
!> @param n the size of the vector @c x
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return imax index of the absolute max of @c x
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/isamax.f but has some minor
!> modifications to the if constructs to avoid use of a @c RETURN statement.
!>
  PURE FUNCTION isamax_all(n,x,incx) RESULT(imax)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    INTEGER(SIK) :: imax
#ifdef HAVE_BLAS
    INTERFACE
      PURE INTEGER FUNCTION isamax(n,x,incx)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0e0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
      ENDFUNCTION isamax
    ENDINTERFACE
    imax=isamax(n,x,incx)
#else
    INTEGER(SIK) :: i,ix
    REAL(SSK) :: tmpmax
    INTRINSIC ABS
    
    imax=0
    IF(n == 1) imax=1
    IF(n > 1 .AND. incx > 0) THEN
      imax=1
      IF(incx == 1) THEN
        !This could be alternatively computed using intrinsic Fortran array
        !operations like this: imax=MAXLOC(ABS(x(1:n)))
        tmpmax=ABS(x(1))
        DO i=2,n
          IF(ABS(x(i)) > tmpmax) THEN
            imax=i
            tmpmax=ABS(x(i))
          ENDIF
        ENDDO
      ELSE
        !I'm not sure about this coding. Note that imax is set to i, not ix.
        !This seems strange to me but its what's on netlib.
        ix=1+incx
        tmpmax=ABS(x(1))
        DO i=2,n
          IF(ABS(x(ix)) > tmpmax) THEN
            imax=i
            tmpmax=ABS(x(ix))
          ENDIF
          ix=ix+incx
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDFUNCTION isamax_all
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::isamax_all "isamax_all" for when the user
!> only wants to pass @c x and @c incx.
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return imax index of the absolute max of @c x
!>
  PURE FUNCTION isamax_noN(x,incx) RESULT(imax)
    REAL(SSK),INTENT(IN) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    INTEGER(SIK) :: imax
    imax=isamax_all(SIZE(x),x,incx)
  ENDFUNCTION isamax_noN
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::isamax_all "isamax_all" for when the user
!> only wants to pass @c n and @c x. The increment is set to 1.
!> @param n the size of the vector @c x
!> @param x the single-precision real vector to operate on
!> @return imax index of the absolute max of @c x
!>
  PURE FUNCTION isamax_noINCX(n,x) RESULT(imax)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    INTEGER(SIK) :: imax
    imax=isamax_all(n,x,1)
  ENDFUNCTION isamax_noINCX
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::isamax_all "isamax_all" for when the user
!> only wants to pass @c x. The increment is set to 1.
!> @param x the single-precision real vector to operate on
!> @return imax index of the absolute max of @c x
!>
  PURE FUNCTION isamax_onlyX(x) RESULT(imax)
    REAL(SSK),INTENT(IN) :: x(:)
    INTEGER(SIK) :: imax
    imax=isamax_all(SIZE(x),x,1)
  ENDFUNCTION isamax_onlyX
!
!-------------------------------------------------------------------------------
!> @brief Function returns the index of the absolute maximum of a double
!> precision vector.
!> @param n the size of the vector @c x
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return imax index of the absolute max of @c x
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/idamax.f but has some minor
!> modifications to the if constructs to avoid use of a @c RETURN statement.
!>
  PURE FUNCTION idamax_all(n,x,incx) RESULT(imax)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    INTEGER(SIK) :: imax
#ifdef HAVE_BLAS
    INTERFACE
      PURE INTEGER FUNCTION idamax(n,x,incx)
        INTEGER,INTENT(IN) :: n
        REAL(KIND(0.0d0)),INTENT(IN) :: x(*)
        INTEGER,INTENT(IN) :: incx
      ENDFUNCTION idamax
    ENDINTERFACE
    imax=idamax(n,x,incx)
#else
    INTEGER(SIK) :: i,ix
    REAL(SDK) :: tmpmax
    INTRINSIC ABS
    
    imax=0
    IF(n == 1) imax=1
    IF(n > 1 .AND. incx > 0) THEN
      imax=1
      IF(incx == 1) THEN
        !This could be alternatively computed using intrinsic Fortran array
        !operations like this: imax=MAXLOC(ABS(x(1:n)))
        tmpmax=ABS(x(1))
        DO i=2,n
          IF(ABS(x(i)) > tmpmax) THEN
            imax=i
            tmpmax=ABS(x(i))
          ENDIF
        ENDDO
      ELSE
        !I'm not sure about this coding. Note that imax is set to i, not ix.
        !This seems strange to me but its what's on netlib.
        ix=1+incx
        tmpmax=ABS(x(1))
        DO i=2,n
          IF(ABS(x(ix)) > tmpmax) THEN
            imax=i
            tmpmax=ABS(x(ix))
          ENDIF
          ix=ix+incx
        ENDDO
      ENDIF
    ENDIF
#endif
  ENDFUNCTION idamax_all
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::idamax_all "idamax_all" for when the user
!> only wants to pass @c x and @c incx.
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return imax index of the absolute max of @c x
!>
  PURE FUNCTION idamax_noN(x,incx) RESULT(imax)
    REAL(SDK),INTENT(IN) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    INTEGER(SIK) :: imax
    imax=idamax_all(SIZE(x),x,incx)
  ENDFUNCTION idamax_noN
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::idamax_all "idamax_all" for when the user
!> only wants to pass @c n and @c x. The increment is set to 1.
!> @param n the size of the vector @c x
!> @param x the double-precision real vector to operate on
!> @return imax index of the absolute max of @c x
!>
  PURE FUNCTION idamax_noINCX(n,x) RESULT(imax)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    INTEGER(SIK) :: imax
    imax=idamax_all(n,x,1)
  ENDFUNCTION idamax_noINCX
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::idamax_all "idamax_all" for when the user
!> only wants to pass @c x. The increment is set to 1.
!> @param x the double-precision real vector to operate on
!> @return imax index of the absolute max of @c x
!>
  PURE FUNCTION idamax_onlyX(x) RESULT(imax)
    REAL(SDK),INTENT(IN) :: x(:)
    INTEGER(SIK) :: imax
    imax=idamax_all(SIZE(x),x,1)
  ENDFUNCTION idamax_onlyX
!
!-------------------------------------------------------------------------------
!> @brief Function returns the index of the absolute minimum of a single 
!> precision vector.
!> @param n the size of the vector @c x
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return imin index of the absolute min of @c x
!>
!> This is an extension to the BLAS library which returns a minimum instead of
!> the maximum.
!>
  PURE FUNCTION isamin_all(n,x,incx) RESULT(imin)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    INTEGER(SIK) :: imin
    INTEGER(SIK) :: i,ix
    REAL(SSK) :: tmpmin

    imin=0
    IF(n == 1) imin=1
    IF(n > 1 .AND. incx > 0) THEN
      imin=1
      IF(incx == 1) THEN
        !This could be alternatively computed using intrinsic Fortran array
        !operations like this: imax=MINLOC(ABS(x(1:n)))
        tmpmin=ABS(x(1))
        DO i=2,n
          IF(ABS(x(i)) < tmpmin) THEN
            imin=i
            tmpmin=ABS(x(i))
          ENDIF
        ENDDO
      ELSE
        !I'm not sure about this coding. Note that imin is set to i, not ix.
        ix=1+incx
        tmpmin=ABS(x(1))
        DO i=2,n
          IF(ABS(x(ix)) < tmpmin) THEN
            imin=i
            tmpmin=ABS(x(ix))
          ENDIF
          ix=ix+incx
        ENDDO
      ENDIF
    ENDIF
  ENDFUNCTION isamin_all
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::isamin_all "isamin_all" for when the user
!> only wants to pass @c x and @c incx.
!> @param x the single-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return imin index of the absolute max of @c x
!>
  PURE FUNCTION isamin_noN(x,incx) RESULT(imin)
    REAL(SSK),INTENT(IN) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    INTEGER(SIK) :: imin
    imin=isamin_all(SIZE(x),x,incx)
  ENDFUNCTION isamin_noN
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::isamin_all "isamin_all" for when the user
!> only wants to pass @c n and @c x. The increment is set to 1.
!> @param n the size of the vector @c x
!> @param x the single-precision real vector to operate on
!> @return imin index of the absolute max of @c x
!>
  PURE FUNCTION isamin_noINCX(n,x) RESULT(imin)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SSK),INTENT(IN) :: x(*)
    INTEGER(SIK) :: imin
    imin=isamin_all(n,x,1)
  ENDFUNCTION isamin_noINCX
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::isamin_all "isamin_all" for when the user
!> only wants to pass @c x. The increment is set to 1.
!> @param x the single-precision real vector to operate on
!> @return imin index of the absolute max of @c x
!>
  PURE FUNCTION isamin_onlyX(x) RESULT(imin)
    REAL(SSK),INTENT(IN) :: x(:)
    INTEGER(SIK) :: imin
    imin=isamin_all(SIZE(x),x,1)
  ENDFUNCTION isamin_onlyX
!
!-------------------------------------------------------------------------------
!> @brief Function returns the index of the absolute minimum of a double
!> precision vector.
!> @param n the size of the vector @c x
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return imin index of the absolute min of @c x
!>
!> This is an extension to the BLAS library which returns a minimum instead of
!> the maximum.
!>
  PURE FUNCTION idamin_all(n,x,incx) RESULT(imin)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    INTEGER(SIK),INTENT(IN) :: incx
    INTEGER(SIK) :: imin
    INTEGER(SIK) :: i,ix
    REAL(SDK) :: tmpmin

    imin=0
    IF(n == 1) imin=1
    IF(n > 1 .AND. incx > 0) THEN
      imin=1
      IF(incx == 1) THEN
        !This could be alternatively computed using intrinsic Fortran array
        !operations like this: imax=MINLOC(ABS(x(1:n)))
        tmpmin=ABS(x(1))
        DO i=2,n
          IF(ABS(x(i)) < tmpmin) THEN
            imin=i
            tmpmin=ABS(x(i))
          ENDIF
        ENDDO
      ELSE
        !I'm not sure about this coding. Note that imin is set to i, not ix.
        ix=1+incx
        tmpmin=ABS(x(1))
        DO i=2,n
          IF(ABS(x(ix)) < tmpmin) THEN
            imin=i
            tmpmin=ABS(x(ix))
          ENDIF
          ix=ix+incx
        ENDDO
      ENDIF
    ENDIF
  ENDFUNCTION idamin_all
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::idamin_all "idamin_all" for when the user
!> only wants to pass @c x and @c incx.
!> @param x the double-precision real vector to operate on
!> @param incx the increment to use when looping over elements in @c x
!> @return imin index of the absolute max of @c x
!>
  PURE FUNCTION idamin_noN(x,incx) RESULT(imin)
    REAL(SDK),INTENT(IN) :: x(:)
    INTEGER(SIK),INTENT(IN) :: incx
    INTEGER(SIK) :: imin
    imin=idamin_all(SIZE(x),x,incx)
  ENDFUNCTION idamin_noN
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::idamin_all "idamin_all" for when the user
!> only wants to pass @c n and @c x. The increment is set to 1.
!> @param n the size of the vector @c x
!> @param x the double-precision real vector to operate on
!> @return imin index of the absolute max of @c x
!>
  PURE FUNCTION idamin_noINCX(n,x) RESULT(imin)
    INTEGER(SIK),INTENT(IN) :: n
    REAL(SDK),INTENT(IN) :: x(*)
    INTEGER(SIK) :: imin
    imin=idamin_all(n,x,1)
  ENDFUNCTION idamin_noINCX
!
!-------------------------------------------------------------------------------
!> @brief Function wraps @ref BLAS1::idamin_all "idamin_all" for when the user
!> only wants to pass @c x. The increment is set to 1.
!> @param x the double-precision real vector to operate on
!> @return imin index of the absolute max of @c x
!>
  PURE FUNCTION idamin_onlyX(x) RESULT(imin)
    REAL(SDK),INTENT(IN) :: x(:)
    INTEGER(SIK) :: imin
    imin=idamin_all(SIZE(x),x,1)
  ENDFUNCTION idamin_onlyX
!
ENDMODULE BLAS1
