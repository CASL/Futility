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
!> @brief A utility module providing an interface to Level 2 BLAS functionality.
!>
!> This module provides simplified generic interfaces to level 2 BLAS routines.
!> It currently only provides an interface to matrix vector multiplication.
!>
!> The following interfaces are provided:
!>  - @ref BLAS2::BLAS_matvec "BLAS_matvec": @copybrief BLAS2::BLAS_matvec
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref BLAS1 "BLAS1": @copybrief BLAS1
!>
!> @par EXAMPLES
!> @code
!> @endcode
!>
!> @author Brendan Kochunas
!> @author Ben Collins
!>    @date 03/15/2012
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE BLAS2

  USE IntrType
  USE BLAS1
  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: BLAS_matvec
  
  !> @brief Generic interface to the Level 2 BLAS routine ???MV
  !>
  !> It provides an interface to routines that perform one of the following
  !> operations:
  !> y <- alpha*A*x+beta*y
  !> y <- alpha*A^T*x+beta*y
  !> x <- A*x
  !> x <- A^T*x    
  !>
  !> Simplified interfaces are also provided so that dimension information
  !> is optional as is @c alpha and @c beta.  If these inputs are excluded, a different
  !> algorithm is used (when no external BLAS library is present).
  !>
  !> Routines for performing those operations on sparse matrices in CSR format
  !> are also included in the interface, which is an extension to standard BLAS.
  !>
  INTERFACE BLAS_matvec
    !> @copybrief BLAS2::sgemv_all @copydetails BLAS2::sgemv_all
    MODULE PROCEDURE sgemv_all
    !> @copybrief BLAS2::sgemv_tmnaaxby @copydetails BLAS2::sgemv_tmnaaxby
    MODULE PROCEDURE sgemv_tmnaaxby
    !> @copybrief BLAS2::sgemv_mnaaxby @copydetails BLAS2::sgemv_mnaaxby
    MODULE PROCEDURE sgemv_mnaaxby
    !> @copybrief BLAS2::sgemv_tnaaxby @copydetails BLAS2::sgemv_tnaaxby
    MODULE PROCEDURE sgemv_tnaaxby
    !> @copybrief BLAS2::sgemv_naaxby @copydetails BLAS2::sgemv_naaxby
    MODULE PROCEDURE sgemv_naaxby
    !> @copybrief BLAS2::sgemv_aaxby @copydetails BLAS2::sgemv_aaxby
    MODULE PROCEDURE sgemv_aaxby
    !> @copybrief BLAS2::sgemv_noalpha @copydetails BLAS2::sgemv_noalpha
    MODULE PROCEDURE sgemv_noalpha
    !> @copybrief BLAS2::sgemv_tmnaxby @copydetails BLAS2::sgemv_tmnaxby
    MODULE PROCEDURE sgemv_tmnaxby
    !> @copybrief BLAS2::sgemv_mnaxby @copydetails BLAS2::sgemv_mnaxby
    MODULE PROCEDURE sgemv_mnaxby
    !> @copybrief BLAS2::sgemv_tnaxby @copydetails BLAS2::sgemv_tnaxby
    MODULE PROCEDURE sgemv_tnaxby
    !> @copybrief BLAS2::sgemv_naxby @copydetails BLAS2::sgemv_naxby
    MODULE PROCEDURE sgemv_naxby
    !> @copybrief BLAS2::sgemv_axby @copydetails BLAS2::sgemv_axby
    MODULE PROCEDURE sgemv_axby
    !> @copybrief BLAS2::sgemv_nobeta @copydetails BLAS2::sgemv_nobeta
    MODULE PROCEDURE sgemv_nobeta
    !> @copybrief BLAS2::sgemv_tmnaaxy @copydetails BLAS2::sgemv_tmnaaxy
    MODULE PROCEDURE sgemv_tmnaaxy
    !> @copybrief BLAS2::sgemv_mnaaxy @copydetails BLAS2::sgemv_mnaaxy
    MODULE PROCEDURE sgemv_mnaaxy
    !> @copybrief BLAS2::sgemv_tnaaxy @copydetails BLAS2::sgemv_tnaaxy
    MODULE PROCEDURE sgemv_tnaaxy
    !> @copybrief BLAS2::sgemv_naaxy @copydetails BLAS2::sgemv_naaxy
    MODULE PROCEDURE sgemv_naaxy
    !> @copybrief BLAS2::sgemv_aaxy @copydetails BLAS2::sgemv_aaxy
    MODULE PROCEDURE sgemv_aaxy
    !> @copybrief BLAS2::sgemv_noalphabeta @copydetails BLAS2::sgemv_noalphabeta
    MODULE PROCEDURE sgemv_noalphabeta
    !> @copybrief BLAS2::sgemv_tmnaxy @copydetails BLAS2::sgemv_tmnaxy
    MODULE PROCEDURE sgemv_tmnaxy
    !> @copybrief BLAS2::sgemv_mnaxy @copydetails BLAS2::sgemv_mnaxy
    MODULE PROCEDURE sgemv_mnaxy
    !> @copybrief BLAS2::sgemv_tnaxy @copydetails BLAS2::sgemv_tnaxy
    MODULE PROCEDURE sgemv_tnaxy
    !> @copybrief BLAS2::sgemv_naxy @copydetails BLAS2::sgemv_naxy
    MODULE PROCEDURE sgemv_naxy
    !> @copybrief BLAS2::sgemv_axy @copydetails BLAS2::sgemv_axy
    MODULE PROCEDURE sgemv_axy
    !> @copybrief BLAS2::sgemv_tax @copydetails BLAS2::sgemv_tax
    MODULE PROCEDURE sgemv_tax
    !> @copybrief BLAS2::sgemv_ax @copydetails BLAS2::sgemv_ax
    MODULE PROCEDURE sgemv_ax
    !> @copybrief BLAS2::dgemv_all @copydetails BLAS2::dgemv_all
    MODULE PROCEDURE dgemv_all
    !> @copybrief BLAS2::dgemv_tmnaaxby @copydetails BLAS2::dgemv_tmnaaxby
    MODULE PROCEDURE dgemv_tmnaaxby
    !> @copybrief BLAS2::dgemv_mnaaxby @copydetails BLAS2::dgemv_mnaaxby
    MODULE PROCEDURE dgemv_mnaaxby
    !> @copybrief BLAS2::dgemv_tnaaxby @copydetails BLAS2::dgemv_tnaaxby
    MODULE PROCEDURE dgemv_tnaaxby
    !> @copybrief BLAS2::dgemv_naaxby @copydetails BLAS2::dgemv_naaxby
    MODULE PROCEDURE dgemv_naaxby
    !> @copybrief BLAS2::dgemv_aaxby @copydetails BLAS2::dgemv_aaxby
    MODULE PROCEDURE dgemv_aaxby
    !> @copybrief BLAS2::dgemv_noalpha @copydetails BLAS2::dgemv_noalpha
    MODULE PROCEDURE dgemv_noalpha
    !> @copybrief BLAS2::dgemv_tmnaxby @copydetails BLAS2::dgemv_tmnaxby
    MODULE PROCEDURE dgemv_tmnaxby
    !> @copybrief BLAS2::dgemv_mnaxby @copydetails BLAS2::dgemv_mnaxby
    MODULE PROCEDURE dgemv_mnaxby
    !> @copybrief BLAS2::dgemv_tnaxby @copydetails BLAS2::dgemv_tnaxby
    MODULE PROCEDURE dgemv_tnaxby
    !> @copybrief BLAS2::dgemv_naxby @copydetails BLAS2::dgemv_naxby
    MODULE PROCEDURE dgemv_naxby
    !> @copybrief BLAS2::dgemv_axby @copydetails BLAS2::dgemv_axby
    MODULE PROCEDURE dgemv_axby
    !> @copybrief BLAS2::dgemv_nobeta @copydetails BLAS2::dgemv_nobeta
    MODULE PROCEDURE dgemv_nobeta
    !> @copybrief BLAS2::dgemv_tmnaaxy @copydetails BLAS2::dgemv_tmnaaxy
    MODULE PROCEDURE dgemv_tmnaaxy
    !> @copybrief BLAS2::dgemv_mnaaxy @copydetails BLAS2::dgemv_mnaaxy
    MODULE PROCEDURE dgemv_mnaaxy
    !> @copybrief BLAS2::dgemv_tnaaxy @copydetails BLAS2::dgemv_tnaaxy
    MODULE PROCEDURE dgemv_tnaaxy
    !> @copybrief BLAS2::dgemv_naaxy @copydetails BLAS2::dgemv_naaxy
    MODULE PROCEDURE dgemv_naaxy
    !> @copybrief BLAS2::dgemv_aaxy @copydetails BLAS2::dgemv_aaxy
    MODULE PROCEDURE dgemv_aaxy
    !> @copybrief BLAS2::dgemv_noalphabeta @copydetails BLAS2::dgemv_noalphabeta
    MODULE PROCEDURE dgemv_noalphabeta
    !> @copybrief BLAS2::dgemv_tmnaxy @copydetails BLAS2::dgemv_tmnaxy
    MODULE PROCEDURE dgemv_tmnaxy
    !> @copybrief BLAS2::dgemv_mnaxy @copydetails BLAS2::dgemv_mnaxy
    MODULE PROCEDURE dgemv_mnaxy
    !> @copybrief BLAS2::dgemv_tnaxy @copydetails BLAS2::dgemv_tnaxy
    MODULE PROCEDURE dgemv_tnaxy
    !> @copybrief BLAS2::dgemv_naxy @copydetails BLAS2::dgemv_naxy
    MODULE PROCEDURE dgemv_naxy
    !> @copybrief BLAS2::dgemv_axy @copydetails BLAS2::dgemv_axy
    MODULE PROCEDURE dgemv_axy
    !> @copybrief BLAS2::dgemv_tax @copydetails BLAS2::dgemv_tax
    MODULE PROCEDURE dgemv_tax
    !> @copybrief BLAS2::dgemv_ax @copydetails BLAS2::dgemv_ax
    MODULE PROCEDURE dgemv_ax
    !> @copybrief BLAS2::scsrmv_all @copydetails BLAS2::scsrmv_all
    MODULE PROCEDURE scsrmv_all
    !> @copybrief BLAS2::scsrmv_noNNZ @copydetails BLAS2::scsrmv_noNNZ
    MODULE PROCEDURE scsrmv_noNNZ
    !> @copybrief BLAS2::scsrmv_noNNZ @copydetails BLAS2::scsrmv_noNNZ
    MODULE PROCEDURE scsrmv_noNNNZ
    !> @copybrief BLAS2::scsrmv_noALPHA @copydetails BLAS2::scsrmv_noALPHA
    MODULE PROCEDURE scsrmv_noALPHA
    !> @copybrief BLAS2::scsrmv_noALPHANNZ @copydetails BLAS2::scsrmv_noALPHANNZ
    MODULE PROCEDURE scsrmv_noALPHANNZ
    !> @copybrief BLAS2::scsrmv_noALPHANNNZ
    !> @copydetails BLAS2::scsrmv_noALPHANNNZ
    MODULE PROCEDURE scsrmv_noALPHANNNZ
    !> @copybrief BLAS2::scsrmv_noBETA @copydetails BLAS2::scsrmv_noBETA
    MODULE PROCEDURE scsrmv_noBETA
    !> @copybrief BLAS2::scsrmv_noBETANNZ @copydetails BLAS2::scsrmv_noBETANNZ
    MODULE PROCEDURE scsrmv_noBETANNZ
    !> @copybrief BLAS2::scsrmv_noBETANNNZ @copydetails BLAS2::scsrmv_noBETANNNZ
    MODULE PROCEDURE scsrmv_noBETANNNZ
    !> @copybrief BLAS2::scsrmv_noALPHABETA 
    !> @copydetails BLAS2::scsrmv_noALPHABETA
    MODULE PROCEDURE scsrmv_noALPHABETA
    !> @copybrief BLAS2::scsrmv_noALPHABETANNZ 
    !> @copydetails BLAS2::scsrmv_noALPHABETANNZ
    MODULE PROCEDURE scsrmv_noALPHABETANNZ
    !> @copybrief BLAS2::scsrmv_noALPHABETANNNZ
    !> @copydetails BLAS2::scsrmv_noALPHABETANNNZ
    MODULE PROCEDURE scsrmv_noALPHABETANNNZ
    !> @copybrief BLAS2::dcsrmv_all @copydetails BLAS2::dcsrmv_all
    MODULE PROCEDURE dcsrmv_all
    !> @copybrief BLAS2::dcsrmv_noNNZ @copydetails BLAS2::dcsrmv_noNNZ
    MODULE PROCEDURE dcsrmv_noNNZ
    !> @copybrief BLAS2::dcsrmv_noNNZ @copydetails BLAS2::dcsrmv_noNNZ
    MODULE PROCEDURE dcsrmv_noNNNZ
    !> @copybrief BLAS2::dcsrmv_noALPHA @copydetails BLAS2::dcsrmv_noALPHA
    MODULE PROCEDURE dcsrmv_noALPHA
    !> @copybrief BLAS2::dcsrmv_noALPHANNZ @copydetails BLAS2::dcsrmv_noALPHANNZ
    MODULE PROCEDURE dcsrmv_noALPHANNZ
    !> @copybrief BLAS2::dcsrmv_noALPHANNNZ
    !> @copydetails BLAS2::dcsrmv_noALPHANNNZ
    MODULE PROCEDURE dcsrmv_noALPHANNNZ
    !> @copybrief BLAS2::dcsrmv_noBETA @copydetails BLAS2::dcsrmv_noBETA
    MODULE PROCEDURE dcsrmv_noBETA
    !> @copybrief BLAS2::dcsrmv_noBETANNZ @copydetails BLAS2::dcsrmv_noBETANNZ
    MODULE PROCEDURE dcsrmv_noBETANNZ
    !> @copybrief BLAS2::dcsrmv_noBETANNNZ @copydetails BLAS2::dcsrmv_noBETANNNZ
    MODULE PROCEDURE dcsrmv_noBETANNNZ
    !> @copybrief BLAS2::dcsrmv_noALPHABETA 
    !> @copydetails BLAS2::dcsrmv_noALPHABETA
    MODULE PROCEDURE dcsrmv_noALPHABETA
    !> @copybrief BLAS2::dcsrmv_noALPHABETANNZ 
    !> @copydetails BLAS2::dcsrmv_noALPHABETANNZ
    MODULE PROCEDURE dcsrmv_noALPHABETANNZ
    !> @copybrief BLAS2::dcsrmv_noALPHABETANNNZ
    !> @copydetails BLAS2::dcsrmv_noALPHABETANNNZ
    MODULE PROCEDURE dcsrmv_noALPHABETANNNZ
    !> @copybrief BLAS2::strsv_all
    !> @copydetails BLAS2::strsv_all
    MODULE PROCEDURE strsv_all
    !> @copybrief BLAS2::dtrsv_all
    !> @copydetails BLAS2::dtrsv_all
    MODULE PROCEDURE dtrsv_all
    !> @copybrief BLAS2::sstrsv_all
    !> @copydetails BLAS2::sstrsv_all
    MODULE PROCEDURE strsv_all_sparse
    !> @copybrief BLAS2::dstrsv_all
    !> @copydetails BLAS2::dstrsv_all
    MODULE PROCEDURE dtrsv_all_sparse
  ENDINTERFACE BLAS_matvec
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes a matrix vector product for a general matrix.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param alpha the single-precision scalar used to scale @c A
!> @param A the single-precision matrix multiply with @c x
!> @param lda the size of the leading (first) dimension of @c A
!> @param x the single-precision vector to multiply with @c A
!> @param incx the increment to use when looping over elements in @c x
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemv.f but has some minor
!> modifications. The application of @c beta to @c y is explicitly unrolled and
!> the error checking is somewhat different.
!>
    PURE SUBROUTINE sgemv_all(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: alpha
      INTEGER(SIK),INTENT(IN) :: lda
      REAL(SSK),INTENT(IN) :: a(lda,*)
      REAL(SSK),INTENT(IN) :: x(*)
      INTEGER(SIK),INTENT(IN) :: incx
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(*)
      INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
      INTERFACE
        PURE SUBROUTINE sgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
          CHARACTER(LEN=1),INTENT(IN) :: trans
          INTEGER,INTENT(IN) :: m
          INTEGER,INTENT(IN) :: n
          REAL(KIND(0.0e0)),INTENT(IN) :: alpha
          INTEGER,INTENT(IN) :: lda
          REAL(KIND(0.0e0)),INTENT(IN) :: a(lda,*)
          REAL(KIND(0.0e0)),INTENT(IN) :: x(*)
          INTEGER,INTENT(IN) :: incx
          REAL(KIND(0.0e0)),INTENT(IN) :: beta
          REAL(KIND(0.0e0)),INTENT(INOUT) :: y(*)
          INTEGER,INTENT(IN) :: incy
        ENDSUBROUTINE sgemv
      ENDINTERFACE
      CALL sgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
#else
      LOGICAL(SBK) :: ltrans
      INTEGER(SIK) :: i,ix,iy,j,jx,jy,kx,ky,lenx,leny,mleny
      REAL(SSK) :: tmp
      INTRINSIC MAX,MOD
    
      IF(m > 0 .AND. n > 0 .AND. incx /= 0 .AND. incy /= 0 .AND. &
        .NOT.(alpha == 0.0_SSK .AND. beta == 1.0_SSK) .AND. lda >= MAX(1,m) .AND. &
          (trans == 't' .OR. trans == 'T' .OR. trans == 'c' .OR. trans == 'C' .OR. &
            trans == 'n' .OR. trans == 'N')) THEN
      
        IF(trans == 'n' .OR. trans == 'N') THEN
          lenx=n
          leny=m
          ltrans=.FALSE.
        ELSE
          lenx=m
          leny=n
          ltrans=.TRUE.
        ENDIF
      
        IF(incx > 0) THEN
          kx=1
        ELSE
          kx=1-(lenx-1)*incx
        ENDIF
      
        IF(incy > 0) THEN
          ky=1
        ELSE
          ky=1-(leny-1)*incy
        ENDIF
      
        !Compute y=beta*y
        IF(beta /= 1.0_SSK) THEN
          IF(incy == 1) THEN
            IF(beta == 0.0_SSK) THEN
              IF(leny > 100) THEN
                mleny=MOD(leny,5)
                IF(mleny /= 0) y(1:mleny)=0.0_SSK
                DO i=mleny+1,leny,5
                  y(i)=0.0_SSK
                  y(i+1)=0.0_SSK
                  y(i+2)=0.0_SSK
                  y(i+3)=0.0_SSK
                  y(i+4)=0.0_SSK
                ENDDO
              ELSE
                y(1:leny)=0.0_SSK
              ENDIF
            ELSE
              IF(leny > 100) THEN
                mleny=MOD(leny,5)
                IF(mleny /= 0) y(1:mleny)=beta*y(1:mleny)
                DO i=mleny+1,leny,5
                  y(i)=beta*y(i)
                  y(i+1)=beta*y(i+1)
                  y(i+2)=beta*y(i+2)
                  y(i+3)=beta*y(i+3)
                  y(i+4)=beta*y(i+4)
                ENDDO
              ELSE
                y(1:leny)=beta*y(1:leny)
              ENDIF
            ENDIF
          ELSE
            iy=ky
            IF(beta == 0.0_SSK) THEN
              DO i=1,leny
                y(iy)=0.0_SSK
                iy=iy+incy
              ENDDO
            ELSE
              DO i=1,leny
                y(iy)=beta*y(iy)
                iy=iy+incy
              ENDDO
            ENDIF
          ENDIF  
        ENDIF
      
        IF(alpha /= 0.0_SSK) THEN
          IF(ltrans) THEN
            !Compute y=alpha*A^T*x+y
            jy=ky
            IF(incx == 1) THEN
              DO j=1,n
                tmp=0.0_SSK
                DO i=1,m
                  tmp=tmp+a(i,j)*x(i)
                ENDDO
                y(jy)=y(jy)+alpha*tmp
                jy=jy+incy
              ENDDO
            ELSE
              DO j=1,n
                tmp=0.0_SSK
                ix=kx
                DO i=1,m
                  tmp=tmp+a(i,j)*x(ix)
                  ix=ix+incx
                ENDDO
                y(jy)=y(jy)+alpha*tmp
                jy=jy+incy
              ENDDO
            ENDIF
          ELSE
            !Compute y=alpha*A*x+y
            jx=kx
            IF(incy == 1) THEN
              DO j=1,n
                IF(x(jx) /= 0.0_SSK) THEN
                  tmp=alpha*x(jx)
                  DO i=1,m
                    y(i)=y(i)+tmp*a(i,j)
                  ENDDO
                ENDIF
                jx=jx+incx
              ENDDO
            ELSE
              DO j=1,n
                IF(x(jx) /= 0.0_SSK) THEN
                  tmp=alpha*x(jx)
                  iy=ky
                  DO i=1,m
                    y(iy)=y(iy)+tmp*a(i,j)
                    iy=iy+incy
                  ENDDO
                ENDIF
                jx=jx+incx
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE sgemv_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_all "sgemv_all" for when the user
!> does not want to supply @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param alpha the single-precision scalar used to scale @c A
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_all "sgemv_all".
!>
    PURE SUBROUTINE sgemv_tmnaaxby(trans,m,n,alpha,a,x,beta,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n) THEN
        IF(trans == 't' .OR. trans == 'T') THEN
          IF(SIZE(x) >= m .AND. SIZE(y) >= n) &
            CALL sgemv_all(trans,m,n,alpha,a,m,x,1,beta,y,1)
        ELSE
          IF(SIZE(x) >= n .AND. SIZE(y) >= m) &
            CALL sgemv_all(trans,m,n,alpha,a,m,x,1,beta,y,1)
        ENDIF
      ENDIF
    ENDSUBROUTINE sgemv_tmnaaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_all "sgemv_all" for when the user
!> does not want to supply @c trans, @c lda, @c incx, and @c incy.
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param alpha the single-precision scalar used to scale @c A
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_all "sgemv_all".
!>
    PURE SUBROUTINE sgemv_mnaaxby(m,n,alpha,a,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= m) CALL sgemv_all('n',m,n,alpha,a,m,x,1,beta,y,1)
    ENDSUBROUTINE sgemv_mnaaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_all "sgemv_all" for when the user
!> does not want to supply @c m, @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param n the dimension of @c A (square)
!> @param alpha the single-precision scalar used to scale @c A
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_all "sgemv_all".
!>
    PURE SUBROUTINE sgemv_tnaaxby(trans,n,alpha,a,x,beta,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL sgemv_all(trans,n,n,alpha,a,n,x,1,beta,y,1)
    ENDSUBROUTINE sgemv_tnaaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_all "sgemv_all" for when the user
!> does not want to supply @c trans, @c m, @c lda, @c incx, and @c incy.
!> @param n the dimension of @c A (square)
!> @param alpha the single-precision scalar used to scale @c A
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_all "sgemv_all".
!>
    PURE SUBROUTINE sgemv_naaxby(n,alpha,a,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL sgemv_all('n',n,n,alpha,a,n,x,1,beta,y,1)
    ENDSUBROUTINE sgemv_naaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_all "sgemv_all" for when the user
!> does not want to supply @c trans, @c m, @c n, @c lda, @c incx, and @c incy.
!> @param alpha the single-precision scalar used to scale @c A
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_all "sgemv_all". Note that @c A need not be square.
!>
    PURE SUBROUTINE sgemv_aaxby(alpha,a,x,beta,y)
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: m,n
      m=SIZE(a,DIM=1)
      n=SIZE(a,DIM=2)
      IF(m == SIZE(y) .AND. n == SIZE(x)) &
        CALL sgemv_all('n',m,n,alpha,a,m,x,1,beta,y,1)
    ENDSUBROUTINE sgemv_aaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes a matrix vector product for a general matrix.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param A the single-precision matrix multiply with @c x
!> @param lda the size of the leading (first) dimension of @c A
!> @param x the single-precision vector to multiply with @c A
!> @param incx the increment to use when looping over elements in @c x
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemv.f but has some minor
!> modifications. The application of @c beta to @c y is explicitly unrolled and
!> the error checking is somewhat different. @c alpha is assumed to be 1 so 
!> the multiplication operation is explicitly removed from the loops.
!>
    PURE SUBROUTINE sgemv_noalpha(trans,m,n,a,lda,x,incx,beta,y,incy)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: lda
      REAL(SSK),INTENT(IN) :: a(lda,*)
      REAL(SSK),INTENT(IN) :: x(*)
      INTEGER(SIK),INTENT(IN) :: incx
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(*)
      INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
      INTERFACE
        PURE SUBROUTINE sgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
          CHARACTER(LEN=1),INTENT(IN) :: trans
          INTEGER,INTENT(IN) :: m
          INTEGER,INTENT(IN) :: n
          REAL(KIND(0.0e0)),INTENT(IN) :: alpha
          INTEGER,INTENT(IN) :: lda
          REAL(KIND(0.0e0)),INTENT(IN) :: a(lda,*)
          REAL(KIND(0.0e0)),INTENT(IN) :: x(*)
          INTEGER,INTENT(IN) :: incx
          REAL(KIND(0.0e0)),INTENT(IN) :: beta
          REAL(KIND(0.0e0)),INTENT(INOUT) :: y(*)
          INTEGER,INTENT(IN) :: incy
        ENDSUBROUTINE sgemv
      ENDINTERFACE
      CALL sgemv(trans,m,n,1.0_SSK,a,lda,x,incx,beta,y,incy)
#else
      LOGICAL(SBK) :: ltrans
      INTEGER(SIK) :: i,ix,iy,j,jx,jy,kx,ky,lenx,leny,mleny
      REAL(SSK) :: tmp
      INTRINSIC MAX,MOD
    
      IF(m > 0 .AND. n > 0 .AND. incx /= 0 .AND. incy /= 0 .AND. & 
          lda >= MAX(1,m) .AND. (trans == 't' .OR. trans == 'T' .OR. &
            trans == 'c' .OR. trans == 'C' .OR. trans == 'n' .OR. trans == 'N')) THEN
        IF(trans == 'n' .OR. trans == 'N') THEN
          lenx=n
          leny=m
          ltrans=.FALSE.
        ELSE
          lenx=m
          leny=n
          ltrans=.TRUE.
        ENDIF
      
        IF(incx > 0) THEN
          kx=1
        ELSE
          kx=1-(lenx-1)*incx
        ENDIF
      
        IF(incy > 0) THEN
          ky=1
        ELSE
          ky=1-(leny-1)*incy
        ENDIF
      
        !Compute y=beta*y
        IF(beta /= 1.0_SSK) THEN
          IF(incy == 1) THEN
            IF(beta == 0.0_SSK) THEN
              IF(leny > 100) THEN
                mleny=MOD(leny,5)
                IF(mleny /= 0) y(1:mleny)=0.0_SSK
                DO i=mleny+1,leny,5
                  y(i)=0.0_SSK
                  y(i+1)=0.0_SSK
                  y(i+2)=0.0_SSK
                  y(i+3)=0.0_SSK
                  y(i+4)=0.0_SSK
                ENDDO
              ELSE
                y(1:leny)=0.0_SSK
              ENDIF
            ELSE
              IF(leny > 100) THEN
                mleny=MOD(leny,5)
                IF(mleny /= 0) y(1:mleny)=beta*y(1:mleny)
                DO i=mleny+1,leny,5
                  y(i)=beta*y(i)
                  y(i+1)=beta*y(i+1)
                  y(i+2)=beta*y(i+2)
                  y(i+3)=beta*y(i+3)
                  y(i+4)=beta*y(i+4)
                ENDDO
              ELSE
                y(1:leny)=beta*y(1:leny)
              ENDIF
            ENDIF
          ELSE
            iy=ky
            IF(beta == 0.0_SSK) THEN
              DO i=1,leny
                y(iy)=0.0_SSK
                iy=iy+incy
              ENDDO
            ELSE
              DO i=1,leny
                y(iy)=beta*y(iy)
                iy=iy+incy
              ENDDO
            ENDIF
          ENDIF  
        ENDIF
      
        IF(ltrans) THEN
          !Compute y=A^T*x+y
          jy=ky
          IF(incx == 1) THEN
            DO j=1,n
              tmp=0.0_SSK
              DO i=1,m
                tmp=tmp+a(i,j)*x(i)
              ENDDO
              y(jy)=y(jy)+tmp
              jy=jy+incy
            ENDDO
          ELSE
            DO j=1,n
              tmp=0.0_SSK
              ix=kx
              DO i=1,m
                tmp=tmp+a(i,j)*x(ix)
                ix=ix+incx
              ENDDO
              y(jy)=y(jy)+tmp
              jy=jy+incy
            ENDDO
          ENDIF
        ELSE
          !Compute y=A*x+y
          jx=kx
          IF(incy == 1) THEN
            DO j=1,n
              IF(x(jx) /= 0.0_SSK) THEN
                DO i=1,m
                  y(i)=y(i)+x(jx)*a(i,j)
                ENDDO
              ENDIF
              jx=jx+incx
            ENDDO
          ELSE
            DO j=1,n
              IF(x(jx) /= 0.0_SSK) THEN
                iy=ky
                DO i=1,m
                  y(iy)=y(iy)+x(jx)*a(i,j)
                  iy=iy+incy
                ENDDO
              ENDIF
              jx=jx+incx
            ENDDO
          ENDIF
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE sgemv_noalpha
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_noalpha "sgemv_noalpha" for when the user
!> does not want to supply @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_noalpha "sgemv_noalpha".
!>
    PURE SUBROUTINE sgemv_tmnaxby(trans,m,n,a,x,beta,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n) THEN
        IF(trans == 't' .OR. trans == 'T') THEN
          IF(SIZE(x) >= m .AND. SIZE(y) >= n) &
            CALL sgemv_noalpha(trans,m,n,a,m,x,1,beta,y,1)
        ELSE
          IF(SIZE(x) >= n .AND. SIZE(y) >= m) &
            CALL sgemv_noalpha(trans,m,n,a,m,x,1,beta,y,1)
        ENDIF
      ENDIF
    ENDSUBROUTINE sgemv_tmnaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_noalpha "sgemv_noalpha" for when
!> the user does not want to supply @c trans, @c lda, @c incx, and @c incy.
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_noalpha "sgemv_noalpha".
!>
    PURE SUBROUTINE sgemv_mnaxby(m,n,a,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= m) CALL sgemv_noalpha('n',m,n,a,m,x,1,beta,y,1)
    ENDSUBROUTINE sgemv_mnaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutines wraps @ref BLAS2::sgemv_noalpha "sgemv_noalpha" for when
!> the user does not want to supply @c m, @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param n the dimension of @c A (square)
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_noalpha "sgemv_noalpha".
!>
    PURE SUBROUTINE sgemv_tnaxby(trans,n,a,x,beta,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL sgemv_noalpha(trans,n,n,a,n,x,1,beta,y,1)
    ENDSUBROUTINE sgemv_tnaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_noalpha "sgemv_noalpha" for when
!> the user does not want to supply @c trans, @c m, @c lda, @c incx, and
!> @c incy.
!> @param n the dimension of @c A (square)
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_noalpha "sgemv_noalpha".
!>
    PURE SUBROUTINE sgemv_naxby(n,a,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL sgemv_noalpha('n',n,n,a,n,x,1,beta,y,1)
    ENDSUBROUTINE sgemv_naxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_noalpha "sgemv_noalpha" for when
!> the user does not want to supply @c trans, @c m, @c n, @c lda, @c incx, and
!> @c incy.
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_noalpha "sgemv_noalpha". Note that @c A need not be square.
!>
    PURE SUBROUTINE sgemv_axby(a,x,beta,y)
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: m,n
      m=SIZE(a,DIM=1)
      n=SIZE(a,DIM=2)
      IF(m == SIZE(y) .AND. n == SIZE(x)) &
        CALL sgemv_noalpha('n',m,n,a,m,x,1,beta,y,1)
    ENDSUBROUTINE sgemv_axby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes a matrix vector product for a general matrix.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param alpha the single-precision scalar used to scale @c A
!> @param A the single-precision matrix multiply with @c x
!> @param lda the size of the leading (first) dimension of @c A
!> @param x the single-precision vector to multiply with @c A
!> @param incx the increment to use when looping over elements in @c x
!> @param y the single-precision vector to add to the product of @c A and @c x
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemv.f but has some minor
!> modifications. @c beta is assumed to be 1 so its application to @c y is
!> explicitly removed.
!>
    PURE SUBROUTINE sgemv_nobeta(trans,m,n,alpha,a,lda,x,incx,y,incy)
         CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: alpha
      INTEGER(SIK),INTENT(IN) :: lda
      REAL(SSK),INTENT(IN) :: a(lda,*)
      REAL(SSK),INTENT(IN) :: x(*)
      INTEGER(SIK),INTENT(IN) :: incx
      REAL(SSK),INTENT(INOUT) :: y(*)
      INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
      INTERFACE
        PURE SUBROUTINE sgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
          CHARACTER(LEN=1),INTENT(IN) :: trans
          INTEGER,INTENT(IN) :: m
          INTEGER,INTENT(IN) :: n
          REAL(KIND(0.0e0)),INTENT(IN) :: alpha
          INTEGER,INTENT(IN) :: lda
          REAL(KIND(0.0e0)),INTENT(IN) :: a(lda,*)
          REAL(KIND(0.0e0)),INTENT(IN) :: x(*)
          INTEGER,INTENT(IN) :: incx
          REAL(KIND(0.0e0)),INTENT(IN) :: beta
          REAL(KIND(0.0e0)),INTENT(INOUT) :: y(*)
          INTEGER,INTENT(IN) :: incy
        ENDSUBROUTINE sgemv
      ENDINTERFACE
      CALL sgemv(trans,m,n,alpha,a,lda,x,incx,1.0_SSK,y,incy)
#else
      LOGICAL(SBK) :: ltrans
      INTEGER(SIK) :: i,ix,iy,j,jx,jy,kx,ky,lenx,leny
      REAL(SSK) :: tmp
      INTRINSIC MAX,MOD
    
      IF(m > 0 .AND. n > 0 .AND. incx /= 0 .AND. incy /= 0 .AND. &
        alpha /= 0.0_SSK .AND. lda >= MAX(1,m) .AND. (trans == 't' .OR. &
          trans == 'T' .OR. trans == 'c' .OR. trans == 'C' .OR. &
            trans == 'n' .OR. trans == 'N')) THEN
        IF(trans == 'n' .OR. trans == 'N') THEN
          lenx=n
          leny=m
          ltrans=.FALSE.
        ELSE
          lenx=m
          leny=n
          ltrans=.TRUE.
        ENDIF
      
        IF(incx > 0) THEN
          kx=1
        ELSE
          kx=1-(lenx-1)*incx
        ENDIF
      
        IF(incy > 0) THEN
          ky=1
        ELSE
          ky=1-(leny-1)*incy
        ENDIF
      
        IF(ltrans) THEN
          !Compute y=alpha*A^T*x+y
          jy=ky
          IF(incx == 1) THEN
            DO j=1,n
              tmp=0.0_SSK
              DO i=1,m
                tmp=tmp+a(i,j)*x(i)
              ENDDO
              y(jy)=y(jy)+alpha*tmp
              jy=jy+incy
            ENDDO
          ELSE
            DO j=1,n
              tmp=0.0_SSK
              ix=kx
              DO i=1,m
                tmp=tmp+a(i,j)*x(ix)
                ix=ix+incx
              ENDDO
              y(jy)=y(jy)+alpha*tmp
              jy=jy+incy
            ENDDO
          ENDIF
        ELSE
          !Compute y=alpha*A*x+y
          jx=kx
          IF(incy == 1) THEN
            DO j=1,n
              IF(x(jx) /= 0.0_SSK) THEN
                tmp=alpha*x(jx)
                DO i=1,m
                  y(i)=y(i)+tmp*a(i,j)
                ENDDO
              ENDIF
              jx=jx+incx
            ENDDO
          ELSE
            DO j=1,n
              IF(x(jx) /= 0.0_SSK) THEN
                tmp=alpha*x(jx)
                iy=ky
                DO i=1,m
                  y(iy)=y(iy)+tmp*a(i,j)
                  iy=iy+incy
                ENDDO
              ENDIF
              jx=jx+incx
            ENDDO
          ENDIF
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE sgemv_nobeta
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_nobeta "sgemv_nobeta" for when the
!> user does not want to supply @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param alpha the single-precision scalar used to scale @c A
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_nobeta "sgemv_nobeta".
!>
    PURE SUBROUTINE sgemv_tmnaaxy(trans,m,n,alpha,a,x,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n) THEN
        IF(trans == 't' .OR. trans == 'T') THEN
          IF(SIZE(x) >= m .AND. SIZE(y) >= n) &
            CALL sgemv_nobeta(trans,m,n,alpha,a,m,x,1,y,1)
        ELSE
          IF(SIZE(x) >= n .AND. SIZE(y) >= m) &
            CALL sgemv_nobeta(trans,m,n,alpha,a,m,x,1,y,1)
        ENDIF
      ENDIF
    ENDSUBROUTINE sgemv_tmnaaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_nobeta "sgemv_nobeta" for when the
!> user does not want to supply @c trans, @c lda, @c incx, and @c incy.
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param alpha the single-precision scalar used to scale @c A
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_nobeta "sgemv_nobeta".
!>
    PURE SUBROUTINE sgemv_mnaaxy(m,n,alpha,a,x,y)
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= m) CALL sgemv_nobeta('n',m,n,alpha,a,m,x,1,y,1)
    ENDSUBROUTINE sgemv_mnaaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_nobeta "sgemv_nobeta" for when the
!> user does not want to supply @c m, @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param n the dimension of @c A (square)
!> @param alpha the single-precision scalar used to scale @c A
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_nobeta "sgemv_nobeta".
!>
    PURE SUBROUTINE sgemv_tnaaxy(trans,n,alpha,a,x,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL sgemv_nobeta(trans,n,n,alpha,a,n,x,1,y,1)
    ENDSUBROUTINE sgemv_tnaaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_nobeta "sgemv_nobeta" for when the
!> user does not want to supply @c trans, @c m, @c lda, @c incx, and @c incy.
!> @param n the dimension of @c A (square)
!> @param alpha the single-precision scalar used to scale @c A
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_nobeta "sgemv_nobeta".
!>
    PURE SUBROUTINE sgemv_naaxy(n,alpha,a,x,y)
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL sgemv_nobeta('n',n,n,alpha,a,n,x,1,y,1)
    ENDSUBROUTINE sgemv_naaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_nobeta "sgemv_nobeta" for when the
!> user does not want to supply @c trans, @c m, @c n, @c lda, @c incx, and
!> @c incy.
!> @param alpha the single-precision scalar used to scale @c A
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_nobeta "sgemv_nobeta". Note that @c A need not be square.
!>
    PURE SUBROUTINE sgemv_aaxy(alpha,a,x,y)
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: m,n
      m=SIZE(a,DIM=1)
      n=SIZE(a,DIM=2)
      IF(m == SIZE(y) .AND. n == SIZE(x)) &
        CALL sgemv_nobeta('n',m,n,alpha,a,m,x,1,y,1)
    ENDSUBROUTINE sgemv_aaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes a matrix vector product for a general matrix.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param A the single-precision matrix multiply with @c x
!> @param lda the size of the leading (first) dimension of @c A
!> @param x the single-precision vector to multiply with @c A
!> @param incx the increment to use when looping over elements in @c x
!> @param y the single-precision vector to add to the product of @c A and @c x
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemv.f but has some minor
!> modifications. The application of @c beta to @c y is explicitly unrolled and
!> the error checking is somewhat different. @c alpha is assumed to be 1 so 
!> the multiplication operation is explicitly removed from the loops. @c beta
!> is also assumed to be 1 so its application to @c y is explicitly removed.
!>
    PURE SUBROUTINE sgemv_noalphabeta(trans,m,n,a,lda,x,incx,y,incy)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: lda
      REAL(SSK),INTENT(IN) :: a(lda,*)
      REAL(SSK),INTENT(IN) :: x(*)
      INTEGER(SIK),INTENT(IN) :: incx
      REAL(SSK),INTENT(INOUT) :: y(*)
      INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
      INTERFACE
        PURE SUBROUTINE sgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
          CHARACTER(LEN=1),INTENT(IN) :: trans
          INTEGER,INTENT(IN) :: m
          INTEGER,INTENT(IN) :: n
          REAL(KIND(0.0e0)),INTENT(IN) :: alpha
          INTEGER,INTENT(IN) :: lda
          REAL(KIND(0.0e0)),INTENT(IN) :: a(lda,*)
          REAL(KIND(0.0e0)),INTENT(IN) :: x(*)
          INTEGER,INTENT(IN) :: incx
          REAL(KIND(0.0e0)),INTENT(IN) :: beta
          REAL(KIND(0.0e0)),INTENT(INOUT) :: y(*)
          INTEGER,INTENT(IN) :: incy
        ENDSUBROUTINE sgemv
      ENDINTERFACE
      CALL sgemv(trans,m,n,1.0_SSK,a,lda,x,incx,1.0_SSK,y,incy)
#else
      LOGICAL(SBK) :: ltrans
      INTEGER(SIK) :: i,ix,iy,j,jx,jy,kx,ky,lenx,leny
      REAL(SSK) :: tmp
      INTRINSIC MAX,MOD
    
      IF(m > 0 .AND. n > 0 .AND. incx /= 0 .AND. incy /= 0 .AND. &
        lda >= MAX(1,m) .AND. (trans == 't' .OR. trans == 'T' .OR. &
          trans == 'c' .OR. trans == 'C' .OR. trans == 'n' .OR. trans == 'N')) THEN
        IF(trans == 'n' .OR. trans == 'N') THEN
          lenx=n
          leny=m
          ltrans=.FALSE.
        ELSE
          lenx=m
          leny=n
          ltrans=.TRUE.
        ENDIF
      
        IF(incx > 0) THEN
          kx=1
        ELSE
          kx=1-(lenx-1)*incx
        ENDIF
      
        IF(incy > 0) THEN
          ky=1
        ELSE
          ky=1-(leny-1)*incy
        ENDIF
      
        IF(ltrans) THEN
          !Compute y=A^T*x+y
          jy=ky
          IF(incx == 1) THEN
            DO j=1,n
              tmp=0.0_SSK
              DO i=1,m
                tmp=tmp+a(i,j)*x(i)
              ENDDO
              y(jy)=y(jy)+tmp
              jy=jy+incy
            ENDDO
          ELSE
            DO j=1,n
              tmp=0.0_SSK
              ix=kx
              DO i=1,m
                tmp=tmp+a(i,j)*x(ix)
                ix=ix+incx
              ENDDO
              y(jy)=y(jy)+tmp
              jy=jy+incy
            ENDDO
          ENDIF
        ELSE
          !Compute y=A*x+y
          jx=kx
          IF(incy == 1) THEN
            DO j=1,n
              IF(x(jx) /= 0.0_SSK) THEN
                DO i=1,m
                  y(i)=y(i)+x(jx)*a(i,j)
                ENDDO
              ENDIF
              jx=jx+incx
            ENDDO
          ELSE
            DO j=1,n
              IF(x(jx) /= 0.0_SSK) THEN
                iy=ky
                DO i=1,m
                  y(iy)=y(iy)+x(jx)*a(i,j)
                  iy=iy+incy
                ENDDO
              ENDIF
              jx=jx+incx
            ENDDO
          ENDIF
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE sgemv_noalphabeta
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_noalphabeta "sgemv_noalphabeta"
!> for when the user does not want to supply @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_noalphabeta "sgemv_noalphabeta".
!>
    PURE SUBROUTINE sgemv_tmnaxy(trans,m,n,a,x,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n) THEN
        IF(trans == 't' .OR. trans == 'T') THEN
          IF(SIZE(x) >= m .AND. SIZE(y) >= n) &
            CALL sgemv_noalphabeta(trans,m,n,a,m,x,1,y,1)
        ELSE
          IF(SIZE(x) >= n .AND. SIZE(y) >= m) &
            CALL sgemv_noalphabeta(trans,m,n,a,m,x,1,y,1)
        ENDIF
      ENDIF
    ENDSUBROUTINE sgemv_tmnaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_noalphabeta "sgemv_noalphabeta"
!> for when the user does not want to supply @c trans, @c lda, @c incx, and
!> @c incy.
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_noalphabeta "sgemv_noalphabeta".
!>
    PURE SUBROUTINE sgemv_mnaxy(m,n,a,x,y)
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= m) CALL sgemv_noalphabeta('n',m,n,a,m,x,1,y,1)
    ENDSUBROUTINE sgemv_mnaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_noalphabeta "sgemv_noalphabeta"
!> for when the user does not want to supply @c m, @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param n the dimension of @c A (square)
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_noalphabeta "sgemv_noalphabeta".
!>
    PURE SUBROUTINE sgemv_tnaxy(trans,n,a,x,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL sgemv_noalphabeta(trans,n,n,a,n,x,1,y,1)
    ENDSUBROUTINE sgemv_tnaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_noalphabeta "sgemv_noalphabeta"
!> for when the user does not want to supply @c trans, @c m, @c lda, @c incx,
!> and @c incy.
!> @param n the dimension of @c A (square)
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_noalphabeta "sgemv_noalphabeta".
!>
    PURE SUBROUTINE sgemv_naxy(n,a,x,y)
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL sgemv_noalphabeta('n',n,n,a,n,x,1,y,1)
    ENDSUBROUTINE sgemv_naxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_noalphabeta "sgemv_noalphabeta"
!> for when the user does not want to supply @c trans, @c m, @c n, @c lda, 
!> @c incx, and @c incy.
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::sgemv_nobeta "sgemv_nobeta". Note that @c A need not be square.
!>
    PURE SUBROUTINE sgemv_axy(a,x,y)
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: m,n
      m=SIZE(a,DIM=1)
      n=SIZE(a,DIM=2)
      IF(m == SIZE(y) .AND. n == SIZE(x)) &
        CALL sgemv_noalphabeta('n',m,n,a,m,x,1,y,1)
    ENDSUBROUTINE sgemv_axy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes a matrix vector product for a general square
!> matrix.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!>
!> This routine is a very specific form of the matrix-vector multiply where
!> @c A is square and @c x is the output argument. The increments over the 
!> elements are 1.
!>
    PURE SUBROUTINE sgemv_tax(trans,a,x)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(INOUT) :: x(:)
      INTEGER(SIK) :: n,m,i,j
      REAL(SSK) :: tmp,b(SIZE(x))
    
      n=SIZE(a,DIM=1)
      IF(n > 0 .AND. SIZE(a,DIM=2) == n .AND. SIZE(x) == n .AND. &
          (trans == 't' .OR. trans == 'T' .OR. trans == 'c' .OR. &
          trans == 'C' .OR. trans == 'n' .OR. trans == 'N')) THEN
        IF(trans == 'n' .OR. trans == 'N') THEN
          !Compute x=A*x
          DO j=1,n
            tmp=0.0_SSK
            IF(x(j) /= 0.0_SSK) THEN
              DO i=1,n
                tmp=tmp+x(j)*a(i,j)
              ENDDO
            ENDIF
            x(j)=tmp
          ENDDO
        ELSE
          !Compute x=A^T*x
          DO j=1,n
            tmp=0.0_SSK
            DO i=1,n
              tmp=tmp+a(i,j)*x(i)
            ENDDO
            b(j)=tmp
          ENDDO
          
          !Copy b to x
          IF(n > 100) THEN
            !Inlining BLAS_copy and unrolling 4 times
            !if a call is made to BLAS_copy it seems to clear parts of x
            !that are not passed in if a sub-array is passed in. I think in general
            !this is a compiler bug with Intel 12.1
            m=MOD(n,4)
            IF(m /= 0) x(1:m)=b(1:m)
            DO i=m+1,n,4
              x(i)=b(i)
              x(i+1)=b(i+1)
              x(i+2)=b(i+2)
              x(i+3)=b(i+3)
            ENDDO
          ELSE
            x=b
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE sgemv_tax
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::sgemv_tax "sgemv_tax"
!> for when the user does not want to supply @c trans.
!> @param A the single-precision matrix multiply with @c x
!> @param x the single-precision vector to multiply with @c A
!>
    PURE SUBROUTINE sgemv_ax(a,x)
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(INOUT) :: x(:)
      CALL sgemv_tax('n',a,x)
    ENDSUBROUTINE sgemv_ax
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes a matrix vector product for a general matrix.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param alpha the double-precision scalar used to scale @c A
!> @param A the double-precision matrix multiply with @c x
!> @param lda the size of the leading (first) dimension of @c A
!> @param x the double-precision vector to multiply with @c A
!> @param incx the increment to use when looping over elements in @c x
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemv.f but has some minor
!> modifications. The application of @c beta to @c y is explicitly unrolled and
!> the error checking is somewhat different.
!>
    PURE SUBROUTINE dgemv_all(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: alpha
      INTEGER(SIK),INTENT(IN) :: lda
      REAL(SDK),INTENT(IN) :: a(lda,*)
      REAL(SDK),INTENT(IN) :: x(*)
      INTEGER(SIK),INTENT(IN) :: incx
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(*)
      INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
      INTERFACE
        PURE SUBROUTINE dgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
          CHARACTER(LEN=1),INTENT(IN) :: trans
          INTEGER,INTENT(IN) :: m
          INTEGER,INTENT(IN) :: n
          REAL(KIND(0.0d0)),INTENT(IN) :: alpha
          INTEGER,INTENT(IN) :: lda
          REAL(KIND(0.0d0)),INTENT(IN) :: a(lda,*)
          REAL(KIND(0.0d0)),INTENT(IN) :: x(*)
          INTEGER,INTENT(IN) :: incx
          REAL(KIND(0.0d0)),INTENT(IN) :: beta
          REAL(KIND(0.0d0)),INTENT(INOUT) :: y(*)
          INTEGER,INTENT(IN) :: incy
        ENDSUBROUTINE dgemv
      ENDINTERFACE
      CALL dgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
#else
      LOGICAL(SBK) :: ltrans
      INTEGER(SIK) :: i,ix,iy,j,jx,jy,kx,ky,lenx,leny,mleny
      REAL(SDK) :: tmp
      INTRINSIC MAX,MOD
    
      IF(m > 0 .AND. n > 0 .AND. incx /= 0 .AND. incy /= 0 .AND. &
        .NOT.(alpha == 0.0_SDK .AND. beta == 1.0_SDK) .AND. lda >= MAX(1,m) .AND. &
          (trans == 't' .OR. trans == 'T' .OR. trans == 'c' .OR. trans == 'C' .OR. &
            trans == 'n' .OR. trans == 'N')) THEN
      
        IF(trans == 'n' .OR. trans == 'N') THEN
          lenx=n
          leny=m
          ltrans=.FALSE.
        ELSE
          lenx=m
          leny=n
          ltrans=.TRUE.
        ENDIF
      
        IF(incx > 0) THEN
          kx=1
        ELSE
          kx=1-(lenx-1)*incx
        ENDIF
      
        IF(incy > 0) THEN
          ky=1
        ELSE
          ky=1-(leny-1)*incy
        ENDIF
      
        !Compute y=beta*y
        IF(beta /= 1.0_SDK) THEN
          IF(incy == 1) THEN
            IF(beta == 0.0_SDK) THEN
              IF(leny > 100) THEN
                mleny=MOD(leny,5)
                IF(mleny /= 0) y(1:mleny)=0.0_SDK
                DO i=mleny+1,leny,5
                  y(i)=0.0_SDK
                  y(i+1)=0.0_SDK
                  y(i+2)=0.0_SDK
                  y(i+3)=0.0_SDK
                  y(i+4)=0.0_SDK
                ENDDO
              ELSE
                y(1:leny)=0.0_SDK
              ENDIF
            ELSE
              IF(leny > 100) THEN
                mleny=MOD(leny,5)
                IF(mleny /= 0) y(1:mleny)=beta*y(1:mleny)
                DO i=mleny+1,leny,5
                  y(i)=beta*y(i)
                  y(i+1)=beta*y(i+1)
                  y(i+2)=beta*y(i+2)
                  y(i+3)=beta*y(i+3)
                  y(i+4)=beta*y(i+4)
                ENDDO
              ELSE
                y(1:leny)=beta*y(1:leny)
              ENDIF
            ENDIF
          ELSE
            iy=ky
            IF(beta == 0.0_SDK) THEN
              DO i=1,leny
                y(iy)=0.0_SDK
                iy=iy+incy
              ENDDO
            ELSE
              DO i=1,leny
                y(iy)=beta*y(iy)
                iy=iy+incy
              ENDDO
            ENDIF
          ENDIF  
        ENDIF
      
        IF(alpha /= 0.0_SDK) THEN
          IF(ltrans) THEN
            !Compute y=alpha*A^T*x+y
            jy=ky
            IF(incx == 1) THEN
              DO j=1,n
                tmp=0.0_SDK
                DO i=1,m
                  tmp=tmp+a(i,j)*x(i)
                ENDDO
                y(jy)=y(jy)+alpha*tmp
                jy=jy+incy
              ENDDO
            ELSE
              DO j=1,n
                tmp=0.0_SDK
                ix=kx
                DO i=1,m
                  tmp=tmp+a(i,j)*x(ix)
                  ix=ix+incx
                ENDDO
                y(jy)=y(jy)+alpha*tmp
                jy=jy+incy
              ENDDO
            ENDIF
          ELSE
            !Compute y=alpha*A*x+y
            jx=kx
            IF(incy == 1) THEN
              DO j=1,n
                IF(x(jx) /= 0.0_SDK) THEN
                  tmp=alpha*x(jx)
                  DO i=1,m
                    y(i)=y(i)+tmp*a(i,j)
                  ENDDO
                ENDIF
                jx=jx+incx
              ENDDO
            ELSE
              DO j=1,n
                IF(x(jx) /= 0.0_SDK) THEN
                  tmp=alpha*x(jx)
                  iy=ky
                  DO i=1,m
                    y(iy)=y(iy)+tmp*a(i,j)
                    iy=iy+incy
                  ENDDO
                ENDIF
                jx=jx+incx
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE dgemv_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_all "dgemv_all" for when the user
!> does not want to supply @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param alpha the double-precision scalar used to scale @c A
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_all "dgemv_all".
!>
    PURE SUBROUTINE dgemv_tmnaaxby(trans,m,n,alpha,a,x,beta,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n) THEN
        IF(trans == 't' .OR. trans == 'T') THEN
          IF(SIZE(x) >= m .AND. SIZE(y) >= n) &
            CALL dgemv_all(trans,m,n,alpha,a,m,x,1,beta,y,1)
        ELSE
          IF(SIZE(x) >= n .AND. SIZE(y) >= m) &
            CALL dgemv_all(trans,m,n,alpha,a,m,x,1,beta,y,1)
        ENDIF
      ENDIF
    ENDSUBROUTINE dgemv_tmnaaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_all "dgemv_all" for when the user
!> does not want to supply @c trans, @c lda, @c incx, and @c incy.
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param alpha the double-precision scalar used to scale @c A
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_all "dgemv_all".
!>
    PURE SUBROUTINE dgemv_mnaaxby(m,n,alpha,a,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= m) CALL dgemv_all('n',m,n,alpha,a,m,x,1,beta,y,1)
    ENDSUBROUTINE dgemv_mnaaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_all "dgemv_all" for when the user
!> does not want to supply @c m, @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param n the dimension of @c A (square)
!> @param alpha the double-precision scalar used to scale @c A
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_all "dgemv_all".
!>
    PURE SUBROUTINE dgemv_tnaaxby(trans,n,alpha,a,x,beta,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL dgemv_all(trans,n,n,alpha,a,n,x,1,beta,y,1)
    ENDSUBROUTINE dgemv_tnaaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_all "dgemv_all" for when the user
!> does not want to supply @c trans, @c m, @c lda, @c incx, and @c incy.
!> @param n the dimension of @c A (square)
!> @param alpha the double-precision scalar used to scale @c A
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_all "dgemv_all".
!>
    PURE SUBROUTINE dgemv_naaxby(n,alpha,a,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL dgemv_all('n',n,n,alpha,a,n,x,1,beta,y,1)
    ENDSUBROUTINE dgemv_naaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_all "dgemv_all" for when the user
!> does not want to supply @c trans, @c m, @c n, @c lda, @c incx, and @c incy.
!> @param alpha the double-precision scalar used to scale @c A
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_all "dgemv_all". Note that @c A need not be square.
!>
    PURE SUBROUTINE dgemv_aaxby(alpha,a,x,beta,y)
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: m,n
      m=SIZE(a,DIM=1)
      n=SIZE(a,DIM=2)
      IF(m == SIZE(y) .AND. n == SIZE(x)) &
        CALL dgemv_all('n',m,n,alpha,a,m,x,1,beta,y,1)
    ENDSUBROUTINE dgemv_aaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes a matrix vector product for a general matrix.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param A the double-precision matrix multiply with @c x
!> @param lda the size of the leading (first) dimension of @c A
!> @param x the double-precision vector to multiply with @c A
!> @param incx the increment to use when looping over elements in @c x
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemv.f but has some minor
!> modifications. The application of @c beta to @c y is explicitly unrolled and
!> the error checking is somewhat different. @c alpha is assumed to be 1 so 
!> the multiplication operation is explicitly removed from the loops.
!>
    PURE SUBROUTINE dgemv_noalpha(trans,m,n,a,lda,x,incx,beta,y,incy)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: lda
      REAL(SDK),INTENT(IN) :: a(lda,*)
      REAL(SDK),INTENT(IN) :: x(*)
      INTEGER(SIK),INTENT(IN) :: incx
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(*)
      INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
      INTERFACE
        PURE SUBROUTINE dgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
          CHARACTER(LEN=1),INTENT(IN) :: trans
          INTEGER,INTENT(IN) :: m
          INTEGER,INTENT(IN) :: n
          REAL(KIND(0.0d0)),INTENT(IN) :: alpha
          INTEGER,INTENT(IN) :: lda
          REAL(KIND(0.0d0)),INTENT(IN) :: a(lda,*)
          REAL(KIND(0.0d0)),INTENT(IN) :: x(*)
          INTEGER,INTENT(IN) :: incx
          REAL(KIND(0.0d0)),INTENT(IN) :: beta
          REAL(KIND(0.0d0)),INTENT(INOUT) :: y(*)
          INTEGER,INTENT(IN) :: incy
        ENDSUBROUTINE dgemv
      ENDINTERFACE
      CALL dgemv(trans,m,n,1.0_SDK,a,lda,x,incx,beta,y,incy)
#else
      LOGICAL(SBK) :: ltrans
      INTEGER(SIK) :: i,ix,iy,j,jx,jy,kx,ky,lenx,leny,mleny
      REAL(SDK) :: tmp
      INTRINSIC MAX,MOD
    
      IF(m > 0 .AND. n > 0 .AND. incx /= 0 .AND. incy /= 0 .AND. & 
          lda >= MAX(1,m) .AND. (trans == 't' .OR. trans == 'T' .OR. &
            trans == 'c' .OR. trans == 'C' .OR. trans == 'n' .OR. trans == 'N')) THEN
        IF(trans == 'n' .OR. trans == 'N') THEN
          lenx=n
          leny=m
          ltrans=.FALSE.
        ELSE
          lenx=m
          leny=n
          ltrans=.TRUE.
        ENDIF
      
        IF(incx > 0) THEN
          kx=1
        ELSE
          kx=1-(lenx-1)*incx
        ENDIF
      
        IF(incy > 0) THEN
          ky=1
        ELSE
          ky=1-(leny-1)*incy
        ENDIF
      
        !Compute y=beta*y
        IF(beta /= 1.0_SDK) THEN
          IF(incy == 1) THEN
            IF(beta == 0.0_SDK) THEN
              IF(leny > 100) THEN
                mleny=MOD(leny,5)
                IF(mleny /= 0) y(1:mleny)=0.0_SDK
                DO i=mleny+1,leny,5
                  y(i)=0.0_SDK
                  y(i+1)=0.0_SDK
                  y(i+2)=0.0_SDK
                  y(i+3)=0.0_SDK
                  y(i+4)=0.0_SDK
                ENDDO
              ELSE
                y(1:leny)=0.0_SDK
              ENDIF
            ELSE
              IF(leny > 100) THEN
                mleny=MOD(leny,5)
                IF(mleny /= 0) y(1:mleny)=beta*y(1:mleny)
                DO i=mleny+1,leny,5
                  y(i)=beta*y(i)
                  y(i+1)=beta*y(i+1)
                  y(i+2)=beta*y(i+2)
                  y(i+3)=beta*y(i+3)
                  y(i+4)=beta*y(i+4)
                ENDDO
              ELSE
                y(1:leny)=beta*y(1:leny)
              ENDIF
            ENDIF
          ELSE
            iy=ky
            IF(beta == 0.0_SDK) THEN
              DO i=1,leny
                y(iy)=0.0_SDK
                iy=iy+incy
              ENDDO
            ELSE
              DO i=1,leny
                y(iy)=beta*y(iy)
                iy=iy+incy
              ENDDO
            ENDIF
          ENDIF  
        ENDIF
      
        IF(ltrans) THEN
          !Compute y=A^T*x+y
          jy=ky
          IF(incx == 1) THEN
            DO j=1,n
              tmp=0.0_SDK
              DO i=1,m
                tmp=tmp+a(i,j)*x(i)
              ENDDO
              y(jy)=y(jy)+tmp
              jy=jy+incy
            ENDDO
          ELSE
            DO j=1,n
              tmp=0.0_SDK
              ix=kx
              DO i=1,m
                tmp=tmp+a(i,j)*x(ix)
                ix=ix+incx
              ENDDO
              y(jy)=y(jy)+tmp
              jy=jy+incy
            ENDDO
          ENDIF
        ELSE
          !Compute y=A*x+y
          jx=kx
          IF(incy == 1) THEN
            DO j=1,n
              IF(x(jx) /= 0.0_SDK) THEN
                DO i=1,m
                  y(i)=y(i)+x(jx)*a(i,j)
                ENDDO
              ENDIF
              jx=jx+incx
            ENDDO
          ELSE
            DO j=1,n
              IF(x(jx) /= 0.0_SDK) THEN
                iy=ky
                DO i=1,m
                  y(iy)=y(iy)+x(jx)*a(i,j)
                  iy=iy+incy
                ENDDO
              ENDIF
              jx=jx+incx
            ENDDO
          ENDIF
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE dgemv_noalpha
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_noalpha "dgemv_noalpha" for when the user
!> does not want to supply @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_noalpha "dgemv_noalpha".
!>
    PURE SUBROUTINE dgemv_tmnaxby(trans,m,n,a,x,beta,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n) THEN
        IF(trans == 't' .OR. trans == 'T') THEN
          IF(SIZE(x) >= m .AND. SIZE(y) >= n) &
            CALL dgemv_noalpha(trans,m,n,a,m,x,1,beta,y,1)
        ELSE
          IF(SIZE(x) >= n .AND. SIZE(y) >= m) &
            CALL dgemv_noalpha(trans,m,n,a,m,x,1,beta,y,1)
        ENDIF
      ENDIF
    ENDSUBROUTINE dgemv_tmnaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_noalpha "dgemv_noalpha" for when
!> the user does not want to supply @c trans, @c lda, @c incx, and @c incy.
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_noalpha "dgemv_noalpha".
!>
    PURE SUBROUTINE dgemv_mnaxby(m,n,a,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= m) CALL dgemv_noalpha('n',m,n,a,m,x,1,beta,y,1)
    ENDSUBROUTINE dgemv_mnaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutines wraps @ref BLAS2::dgemv_noalpha "dgemv_noalpha" for when
!> the user does not want to supply @c m, @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param n the dimension of @c A (square)
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_noalpha "dgemv_noalpha".
!>
    PURE SUBROUTINE dgemv_tnaxby(trans,n,a,x,beta,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL dgemv_noalpha(trans,n,n,a,n,x,1,beta,y,1)
    ENDSUBROUTINE dgemv_tnaxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_noalpha "dgemv_noalpha" for when
!> the user does not want to supply @c trans, @c m, @c lda, @c incx, and
!> @c incy.
!> @param n the dimension of @c A (square)
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_noalpha "dgemv_noalpha".
!>
    PURE SUBROUTINE dgemv_naxby(n,a,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL dgemv_noalpha('n',n,n,a,n,x,1,beta,y,1)
    ENDSUBROUTINE dgemv_naxby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_noalpha "dgemv_noalpha" for when
!> the user does not want to supply @c trans, @c m, @c n, @c lda, @c incx, and
!> @c incy.
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_noalpha "dgemv_noalpha". Note that @c A need not be square.
!>
    PURE SUBROUTINE dgemv_axby(a,x,beta,y)
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: m,n
      m=SIZE(a,DIM=1)
      n=SIZE(a,DIM=2)
      IF(m == SIZE(y) .AND. n == SIZE(x)) &
        CALL dgemv_noalpha('n',m,n,a,m,x,1,beta,y,1)
    ENDSUBROUTINE dgemv_axby
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes a matrix vector product for a general matrix.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param alpha the double-precision scalar used to scale @c A
!> @param A the double-precision matrix multiply with @c x
!> @param lda the size of the leading (first) dimension of @c A
!> @param x the double-precision vector to multiply with @c A
!> @param incx the increment to use when looping over elements in @c x
!> @param y the double-precision vector to add to the product of @c A and @c x
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemv.f but has some minor
!> modifications. @c beta is assumed to be 1 so its application to @c y is
!> explicitly removed.
!>
    PURE SUBROUTINE dgemv_nobeta(trans,m,n,alpha,a,lda,x,incx,y,incy)
         CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: alpha
      INTEGER(SIK),INTENT(IN) :: lda
      REAL(SDK),INTENT(IN) :: a(lda,*)
      REAL(SDK),INTENT(IN) :: x(*)
      INTEGER(SIK),INTENT(IN) :: incx
      REAL(SDK),INTENT(INOUT) :: y(*)
      INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
      INTERFACE
        PURE SUBROUTINE dgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
          CHARACTER(LEN=1),INTENT(IN) :: trans
          INTEGER,INTENT(IN) :: m
          INTEGER,INTENT(IN) :: n
          REAL(KIND(0.0d0)),INTENT(IN) :: alpha
          INTEGER,INTENT(IN) :: lda
          REAL(KIND(0.0d0)),INTENT(IN) :: a(lda,*)
          REAL(KIND(0.0d0)),INTENT(IN) :: x(*)
          INTEGER,INTENT(IN) :: incx
          REAL(KIND(0.0d0)),INTENT(IN) :: beta
          REAL(KIND(0.0d0)),INTENT(INOUT) :: y(*)
          INTEGER,INTENT(IN) :: incy
        ENDSUBROUTINE dgemv
      ENDINTERFACE
      CALL dgemv(trans,m,n,alpha,a,lda,x,incx,1.0_SDK,y,incy)
#else
      LOGICAL(SBK) :: ltrans
      INTEGER(SIK) :: i,ix,iy,j,jx,jy,kx,ky,lenx,leny
      REAL(SDK) :: tmp
      INTRINSIC MAX,MOD
    
      IF(m > 0 .AND. n > 0 .AND. incx /= 0 .AND. incy /= 0 .AND. &
        alpha /= 0.0_SDK .AND. lda >= MAX(1,m) .AND. (trans == 't' .OR. &
          trans == 'T' .OR. trans == 'c' .OR. trans == 'C' .OR. &
            trans == 'n' .OR. trans == 'N')) THEN
        IF(trans == 'n' .OR. trans == 'N') THEN
          lenx=n
          leny=m
          ltrans=.FALSE.
        ELSE
          lenx=m
          leny=n
          ltrans=.TRUE.
        ENDIF
      
        IF(incx > 0) THEN
          kx=1
        ELSE
          kx=1-(lenx-1)*incx
        ENDIF
      
        IF(incy > 0) THEN
          ky=1
        ELSE
          ky=1-(leny-1)*incy
        ENDIF
      
        IF(ltrans) THEN
          !Compute y=alpha*A^T*x+y
          jy=ky
          IF(incx == 1) THEN
            DO j=1,n
              tmp=0.0_SDK
              DO i=1,m
                tmp=tmp+a(i,j)*x(i)
              ENDDO
              y(jy)=y(jy)+alpha*tmp
              jy=jy+incy
            ENDDO
          ELSE
            DO j=1,n
              tmp=0.0_SDK
              ix=kx
              DO i=1,m
                tmp=tmp+a(i,j)*x(ix)
                ix=ix+incx
              ENDDO
              y(jy)=y(jy)+alpha*tmp
              jy=jy+incy
            ENDDO
          ENDIF
        ELSE
          !Compute y=alpha*A*x+y
          jx=kx
          IF(incy == 1) THEN
            DO j=1,n
              IF(x(jx) /= 0.0_SDK) THEN
                tmp=alpha*x(jx)
                DO i=1,m
                  y(i)=y(i)+tmp*a(i,j)
                ENDDO
              ENDIF
              jx=jx+incx
            ENDDO
          ELSE
            DO j=1,n
              IF(x(jx) /= 0.0_SDK) THEN
                tmp=alpha*x(jx)
                iy=ky
                DO i=1,m
                  y(iy)=y(iy)+tmp*a(i,j)
                  iy=iy+incy
                ENDDO
              ENDIF
              jx=jx+incx
            ENDDO
          ENDIF
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE dgemv_nobeta
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_nobeta "dgemv_nobeta" for when the
!> user does not want to supply @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param alpha the double-precision scalar used to scale @c A
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_nobeta "dgemv_nobeta".
!>
    PURE SUBROUTINE dgemv_tmnaaxy(trans,m,n,alpha,a,x,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n) THEN
        IF(trans == 't' .OR. trans == 'T') THEN
          IF(SIZE(x) >= m .AND. SIZE(y) >= n) &
            CALL dgemv_nobeta(trans,m,n,alpha,a,m,x,1,y,1)
        ELSE
          IF(SIZE(x) >= n .AND. SIZE(y) >= m) &
            CALL dgemv_nobeta(trans,m,n,alpha,a,m,x,1,y,1)
        ENDIF
      ENDIF
    ENDSUBROUTINE dgemv_tmnaaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_nobeta "dgemv_nobeta" for when the
!> user does not want to supply @c trans, @c lda, @c incx, and @c incy.
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param alpha the double-precision scalar used to scale @c A
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_nobeta "dgemv_nobeta".
!>
    PURE SUBROUTINE dgemv_mnaaxy(m,n,alpha,a,x,y)
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= m) CALL dgemv_nobeta('n',m,n,alpha,a,m,x,1,y,1)
    ENDSUBROUTINE dgemv_mnaaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_nobeta "dgemv_nobeta" for when the
!> user does not want to supply @c m, @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param n the dimension of @c A (square)
!> @param alpha the double-precision scalar used to scale @c A
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_nobeta "dgemv_nobeta".
!>
    PURE SUBROUTINE dgemv_tnaaxy(trans,n,alpha,a,x,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL dgemv_nobeta(trans,n,n,alpha,a,n,x,1,y,1)
    ENDSUBROUTINE dgemv_tnaaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_nobeta "dgemv_nobeta" for when the
!> user does not want to supply @c trans, @c m, @c lda, @c incx, and @c incy.
!> @param n the dimension of @c A (square)
!> @param alpha the double-precision scalar used to scale @c A
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_nobeta "dgemv_nobeta".
!>
    PURE SUBROUTINE dgemv_naaxy(n,alpha,a,x,y)
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL dgemv_nobeta('n',n,n,alpha,a,n,x,1,y,1)
    ENDSUBROUTINE dgemv_naaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_nobeta "dgemv_nobeta" for when the
!> user does not want to supply @c trans, @c m, @c n, @c lda, @c incx, and
!> @c incy.
!> @param alpha the double-precision scalar used to scale @c A
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_nobeta "dgemv_nobeta". Note that @c A need not be square.
!>
    PURE SUBROUTINE dgemv_aaxy(alpha,a,x,y)
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: m,n
      m=SIZE(a,DIM=1)
      n=SIZE(a,DIM=2)
      IF(m == SIZE(y) .AND. n == SIZE(x)) &
        CALL dgemv_nobeta('n',m,n,alpha,a,m,x,1,y,1)
    ENDSUBROUTINE dgemv_aaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes a matrix vector product for a general matrix.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param A the double-precision matrix multiply with @c x
!> @param lda the size of the leading (first) dimension of @c A
!> @param x the double-precision vector to multiply with @c A
!> @param incx the increment to use when looping over elements in @c x
!> @param y the double-precision vector to add to the product of @c A and @c x
!> @param incy the increment to use when looping over elements in @c y
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemv.f but has some minor
!> modifications. The application of @c beta to @c y is explicitly unrolled and
!> the error checking is somewhat different. @c alpha is assumed to be 1 so 
!> the multiplication operation is explicitly removed from the loops. @c beta
!> is also assumed to be 1 so its application to @c y is explicitly removed.
!>
    PURE SUBROUTINE dgemv_noalphabeta(trans,m,n,a,lda,x,incx,y,incy)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: lda
      REAL(SDK),INTENT(IN) :: a(lda,*)
      REAL(SDK),INTENT(IN) :: x(*)
      INTEGER(SIK),INTENT(IN) :: incx
      REAL(SDK),INTENT(INOUT) :: y(*)
      INTEGER(SIK),INTENT(IN) :: incy
#ifdef HAVE_BLAS
      INTERFACE
        PURE SUBROUTINE dgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
          CHARACTER(LEN=1),INTENT(IN) :: trans
          INTEGER,INTENT(IN) :: m
          INTEGER,INTENT(IN) :: n
          REAL(KIND(0.0d0)),INTENT(IN) :: alpha
          INTEGER,INTENT(IN) :: lda
          REAL(KIND(0.0d0)),INTENT(IN) :: a(lda,*)
          REAL(KIND(0.0d0)),INTENT(IN) :: x(*)
          INTEGER,INTENT(IN) :: incx
          REAL(KIND(0.0d0)),INTENT(IN) :: beta
          REAL(KIND(0.0d0)),INTENT(INOUT) :: y(*)
          INTEGER,INTENT(IN) :: incy
        ENDSUBROUTINE dgemv
      ENDINTERFACE
      CALL dgemv(trans,m,n,1.0_SDK,a,lda,x,incx,1.0_SDK,y,incy)
#else
      LOGICAL(SBK) :: ltrans
      INTEGER(SIK) :: i,ix,iy,j,jx,jy,kx,ky,lenx,leny
      REAL(SDK) :: tmp
      INTRINSIC MAX,MOD
    
      IF(m > 0 .AND. n > 0 .AND. incx /= 0 .AND. incy /= 0 .AND. &
        lda >= MAX(1,m) .AND. (trans == 't' .OR. trans == 'T' .OR. &
          trans == 'c' .OR. trans == 'C' .OR. trans == 'n' .OR. trans == 'N')) THEN
        IF(trans == 'n' .OR. trans == 'N') THEN
          lenx=n
          leny=m
          ltrans=.FALSE.
        ELSE
          lenx=m
          leny=n
          ltrans=.TRUE.
        ENDIF
      
        IF(incx > 0) THEN
          kx=1
        ELSE
          kx=1-(lenx-1)*incx
        ENDIF
      
        IF(incy > 0) THEN
          ky=1
        ELSE
          ky=1-(leny-1)*incy
        ENDIF
      
        IF(ltrans) THEN
          !Compute y=A^T*x+y
          jy=ky
          IF(incx == 1) THEN
            DO j=1,n
              tmp=0.0_SDK
              DO i=1,m
                tmp=tmp+a(i,j)*x(i)
              ENDDO
              y(jy)=y(jy)+tmp
              jy=jy+incy
            ENDDO
          ELSE
            DO j=1,n
              tmp=0.0_SDK
              ix=kx
              DO i=1,m
                tmp=tmp+a(i,j)*x(ix)
                ix=ix+incx
              ENDDO
              y(jy)=y(jy)+tmp
              jy=jy+incy
            ENDDO
          ENDIF
        ELSE
          !Compute y=A*x+y
          jx=kx
          IF(incy == 1) THEN
            DO j=1,n
              IF(x(jx) /= 0.0_SDK) THEN
                DO i=1,m
                  y(i)=y(i)+x(jx)*a(i,j)
                ENDDO
              ENDIF
              jx=jx+incx
            ENDDO
          ELSE
            DO j=1,n
              IF(x(jx) /= 0.0_SDK) THEN
                iy=ky
                DO i=1,m
                  y(iy)=y(iy)+x(jx)*a(i,j)
                  iy=iy+incy
                ENDDO
              ENDIF
              jx=jx+incx
            ENDDO
          ENDIF
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE dgemv_noalphabeta
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_noalphabeta "dgemv_noalphabeta"
!> for when the user does not want to supply @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_noalphabeta "dgemv_noalphabeta".
!>
    PURE SUBROUTINE dgemv_tmnaxy(trans,m,n,a,x,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n) THEN
        IF(trans == 't' .OR. trans == 'T') THEN
          IF(SIZE(x) >= m .AND. SIZE(y) >= n) &
            CALL dgemv_noalphabeta(trans,m,n,a,m,x,1,y,1)
        ELSE
          IF(SIZE(x) >= n .AND. SIZE(y) >= m) &
            CALL dgemv_noalphabeta(trans,m,n,a,m,x,1,y,1)
        ENDIF
      ENDIF
    ENDSUBROUTINE dgemv_tmnaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_noalphabeta "dgemv_noalphabeta"
!> for when the user does not want to supply @c trans, @c lda, @c incx, and
!> @c incy.
!> @param m the size of the first dimension of @c A (number of rows)
!> @param n the size of the second dimension of @c A (number of columns)
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_noalphabeta "dgemv_noalphabeta".
!>
    PURE SUBROUTINE dgemv_mnaxy(m,n,a,x,y)
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= m .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= m) CALL dgemv_noalphabeta('n',m,n,a,m,x,1,y,1)
    ENDSUBROUTINE dgemv_mnaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_noalphabeta "dgemv_noalphabeta"
!> for when the user does not want to supply @c m, @c lda, @c incx, and @c incy.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param n the dimension of @c A (square)
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_noalphabeta "dgemv_noalphabeta".
!>
    PURE SUBROUTINE dgemv_tnaxy(trans,n,a,x,y)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL dgemv_noalphabeta(trans,n,n,a,n,x,1,y,1)
    ENDSUBROUTINE dgemv_tnaxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_noalphabeta "dgemv_noalphabeta"
!> for when the user does not want to supply @c trans, @c m, @c lda, @c incx,
!> and @c incy.
!> @param n the dimension of @c A (square)
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c n and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_noalphabeta "dgemv_noalphabeta".
!>
    PURE SUBROUTINE dgemv_naxy(n,a,x,y)
      INTEGER(SIK),INTENT(IN) :: n
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(INOUT) :: y(:)
      IF(SIZE(a,DIM=1) >= n .AND. SIZE(a,DIM=2) >= n .AND. SIZE(x) >= n .AND. &
        SIZE(y) >= n) CALL dgemv_noalphabeta('n',n,n,a,n,x,1,y,1)
    ENDSUBROUTINE dgemv_naxy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_noalphabeta "dgemv_noalphabeta"
!> for when the user does not want to supply @c trans, @c m, @c n, @c lda, 
!> @c incx, and @c incy.
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> @c lda is taken to be equal to @c m and the increments are set to 1. The
!> dimensions of @c A, @c x, and @c y are also checked prior to calling 
!> @ref BLAS2::dgemv_nobeta "dgemv_nobeta". Note that @c A need not be square.
!>
    PURE SUBROUTINE dgemv_axy(a,x,y)
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: m,n
      m=SIZE(a,DIM=1)
      n=SIZE(a,DIM=2)
      IF(m == SIZE(y) .AND. n == SIZE(x)) &
        CALL dgemv_noalphabeta('n',m,n,a,m,x,1,y,1)
    ENDSUBROUTINE dgemv_axy
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes a matrix vector product for a general square
!> matrix.
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!>
!> This routine is a very specific form of the matrix-vector multiply where
!> @c A is square and @c x is the output argument. The increments over the 
!> elements are 1.
!>
    PURE SUBROUTINE dgemv_tax(trans,a,x)
      CHARACTER(LEN=1),INTENT(IN) :: trans
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(INOUT) :: x(:)
      INTEGER(SIK) :: n,m,i,j
      REAL(SDK) :: tmp,b(SIZE(x))
    
      n=SIZE(a,DIM=1)
      IF(n > 0 .AND. SIZE(a,DIM=2) == n .AND. SIZE(x) == n .AND. &
          (trans == 't' .OR. trans == 'T' .OR. trans == 'c' .OR. &
          trans == 'C' .OR. trans == 'n' .OR. trans == 'N')) THEN
        IF(trans == 'n' .OR. trans == 'N') THEN
          !Compute x=A*x
          DO j=1,n
            tmp=0.0_SDK
            IF(x(j) /= 0.0_SDK) THEN
              DO i=1,n
                tmp=tmp+x(j)*a(i,j)
              ENDDO
            ENDIF
            x(j)=tmp
          ENDDO
        ELSE
          !Compute x=A^T*x
          DO j=1,n
            tmp=0.0_SDK
            DO i=1,n
              tmp=tmp+a(i,j)*x(i)
            ENDDO
            b(j)=tmp
          ENDDO
          !Copy b to x
          IF(n > 100) THEN
            !Inlining BLAS_copy and unrolling 4 times
            !if a call is made to BLAS_copy it seems to clear parts of x
            !that are not passed in if a sub-array is passed in. I think in general
            !this is a compiler bug with Intel 12.1
            m=MOD(n,4)
            IF(m /= 0) x(1:m)=b(1:m)
            DO i=m+1,n,4
              x(i)=b(i)
              x(i+1)=b(i+1)
              x(i+2)=b(i+2)
              x(i+3)=b(i+3)
            ENDDO
          ELSE
            x=b
          ENDIF
        ENDIF
      ENDIF
    ENDSUBROUTINE dgemv_tax
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dgemv_tax "dgemv_tax"
!> for when the user does not want to supply @c trans.
!> @param A the double-precision matrix multiply with @c x
!> @param x the double-precision vector to multiply with @c A
!>
    PURE SUBROUTINE dgemv_ax(a,x)
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(INOUT) :: x(:)
      CALL dgemv_tax('n',a,x)
    ENDSUBROUTINE dgemv_ax
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the matrix-vector product for a square sparse
!> matrix stored in CSR format.
!> @param n order of the linear system
!> @param nnz the number of non-zero elements in @c A
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param alpha the single-precision scalar used to scale @c A
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE scsrmv_all(n,nnz,ia,ja,aa,alpha,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: nnz
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(nnz)
      REAL(SSK),INTENT(IN) :: aa(nnz)
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: x(n)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(n)
             
      INTEGER(SIK) :: i,j,k
      REAL(SSK) :: tmp
      
      IF(n > 0 .AND. nnz >= n .AND. .NOT.(alpha == 0.0_SSK .AND. &
        beta == 1.0_SSK)) THEN
        
        !Compute y <- beta*y
        IF(beta /= 1.0_SSK) CALL BLAS_scal(n,beta,y,1)
        
        IF(alpha /= 0.0_SSK) THEN
          !y <- Ax+y
          DO i=1,n
            tmp=0.0_SSK
            DO k=ia(i),ia(i+1)-1
              j=ja(k)
              tmp=tmp+aa(k)*x(j)
            ENDDO
            y(i)=y(i)+alpha*tmp
          ENDDO
        ENDIF
      ENDIF
    ENDSUBROUTINE scsrmv_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::scsrmv_all "scsrmv_all" for when the 
!> user does not want to pass @c nnz.
!> @param n order of the linear system
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param alpha the single-precision scalar used to scale @c A
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE scsrmv_noNNZ(n,ia,ja,aa,alpha,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SSK),INTENT(IN) :: aa(:)
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: x(n)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(n)
      INTEGER(SIK) :: nnz
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja)) CALL scsrmv_all(n,nnz,ia,ja,aa,alpha,x,beta,y)
    ENDSUBROUTINE scsrmv_noNNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::scsrmv_all "scsrmv_all" for when the 
!> user does not want to pass @c nnz or @c n.
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param alpha the single-precision scalar used to scale @c A
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE scsrmv_noNNNZ(ia,ja,aa,alpha,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: ia(:)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SSK),INTENT(IN) :: aa(:)
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: nnz,n
      n=SIZE(x)
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja) .AND. SIZE(y) == n .AND. SIZE(ia) == n+1) &
        CALL scsrmv_all(n,nnz,ia,ja,aa,alpha,x,beta,y)
    ENDSUBROUTINE scsrmv_noNNNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the matrix-vector product for a square sparse
!> matrix stored in CSR format.
!> @param n order of the linear system
!> @param nnz the number of non-zero elements in @c A
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> This routine is similar to @ref BLAS2::scsrmv_all "scsrmv_all" except 
!> @c alpha is assumed to be 1 so the floating point operation multiplying 
!> @c alpha is explicitly removed reducing the FLOP count by a factor of @c n 
!> multiplications.
!>
    PURE SUBROUTINE scsrmv_noALPHA(n,nnz,ia,ja,aa,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: nnz
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(nnz)
      REAL(SSK),INTENT(IN) :: aa(nnz)
      REAL(SSK),INTENT(IN) :: x(n)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(n)
      
      INTEGER(SIK) :: i,j,k
      REAL(SSK) :: tmp
      
      IF(n > 0 .AND. nnz >= n) THEN
        
        !Compute y <- beta*y
        IF(beta /= 1.0_SSK) CALL BLAS_scal(n,beta,y,1)
        
        !y <- Ax+y
        DO i=1,n
          tmp=0.0_SSK
          DO k=ia(i),ia(i+1)-1
            j=ja(k)
            tmp=tmp+aa(k)*x(j)
          ENDDO
          y(i)=y(i)+tmp
        ENDDO
      ENDIF
    ENDSUBROUTINE scsrmv_noALPHA
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::scsrmv_noALPHA "scsrmv_noALPHA" for when
!> the user does not want to pass @c nnz.
!> @param n order of the linear system
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE scsrmv_noALPHANNZ(n,ia,ja,aa,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SSK),INTENT(IN) :: aa(:)
      REAL(SSK),INTENT(IN) :: x(n)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(n)
      INTEGER(SIK) :: nnz
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja)) CALL scsrmv_noALPHA(n,nnz,ia,ja,aa,x,beta,y)
    ENDSUBROUTINE scsrmv_noALPHANNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::scsrmv_noALPHA "scsrmv_noALPHA" for when
!> the user does not want to pass @c nnz and @c n.
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param x the single-precision vector to multiply with @c A
!> @param beta the single-precision scalar used to scale @c y
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE scsrmv_noALPHANNNZ(ia,ja,aa,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: ia(:)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SSK),INTENT(IN) :: aa(:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: nnz,n
      n=SIZE(x)
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja) .AND. SIZE(y) == n .AND. SIZE(ia) == n+1) &
        CALL scsrmv_noALPHA(n,nnz,ia,ja,aa,x,beta,y)
    ENDSUBROUTINE scsrmv_noALPHANNNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the matrix-vector product for a square sparse
!> matrix stored in CSR format.
!> @param n order of the linear system
!> @param nnz the number of non-zero elements in @c A
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param alpha the single-precision scalar used to scale @c A
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> This routine is similar to @ref BLAS2::scsrmv_all "scsrmv_all" except @c beta
!> is assumed to be 1 so the branching statement to @ref BLAS1::BLAS_scal 
!> "BLAS_scal" is explicitly removed.
!>
    PURE SUBROUTINE scsrmv_noBETA(n,nnz,ia,ja,aa,alpha,x,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: nnz
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(nnz)
      REAL(SSK),INTENT(IN) :: aa(nnz)
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: x(n)
      REAL(SSK),INTENT(INOUT) :: y(n)
             
      INTEGER(SIK) :: i,j,k
      REAL(SSK) :: tmp
      
      IF(n > 0 .AND. nnz >= n .AND. alpha /= 0.0_SSK) THEN
        !y <- Ax+y
        DO i=1,n
          tmp=0.0_SSK
          DO k=ia(i),ia(i+1)-1
            j=ja(k)
            tmp=tmp+aa(k)*x(j)
          ENDDO
          y(i)=y(i)+alpha*tmp
        ENDDO
      ENDIF
    ENDSUBROUTINE scsrmv_noBETA
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::scsrmv_noBETA "scsrmv_noBETA" for when
!> the user does not want to pass @c nnz.
!> @param n order of the linear system
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param alpha the single-precision scalar used to scale @c A
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE scsrmv_noBETANNZ(n,ia,ja,aa,alpha,x,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SSK),INTENT(IN) :: aa(:)
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: x(n)
      REAL(SSK),INTENT(INOUT) :: y(n)
      INTEGER(SIK) :: nnz
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja)) CALL scsrmv_noBETA(n,nnz,ia,ja,aa,alpha,x,y)
    ENDSUBROUTINE scsrmv_noBETANNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::scsrmv_noBETA "scsrmv_noBETA" for when
!> the user does not want to pass @c nnz and @c n.
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param alpha the single-precision scalar used to scale @c A
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE scsrmv_noBETANNNZ(ia,ja,aa,alpha,x,y)
      INTEGER(SIK),INTENT(IN) :: ia(:)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SSK),INTENT(IN) :: aa(:)
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: nnz,n
      n=SIZE(x)
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja) .AND. SIZE(y) == n .AND. SIZE(ia) == n+1) &
        CALL scsrmv_noBETA(n,nnz,ia,ja,aa,alpha,x,y)
    ENDSUBROUTINE scsrmv_noBETANNNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the matrix-vector product for a square sparse
!> matrix stored in CSR format.
!> @param n order of the linear system
!> @param nnz the number of non-zero elements in @c A
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
!> This routine is similar to @ref BLAS2::scsrmv_all "scsrmv_all" except @c beta
!> is assumed to be 1 so the branching statement to @ref BLAS1::BLAS_scal 
!> "BLAS_scal" is explicitly removed. @c alpha is also assumed to be 1 so the
!> floating point operation multiplying @c alpha is explicitly removed reducing
!> the FLOP count by a factor of @c n multiplications.
!>
    PURE SUBROUTINE scsrmv_noALPHABETA(n,nnz,ia,ja,aa,x,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: nnz
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(nnz)
      REAL(SSK),INTENT(IN) :: aa(nnz)
      REAL(SSK),INTENT(IN) :: x(n)
      REAL(SSK),INTENT(INOUT) :: y(n)
             
      INTEGER(SIK) :: i,j,k
      REAL(SSK) :: tmp
      
      IF(n > 0 .AND. nnz >= n) THEN
        !y <- Ax+y
        DO i=1,n
          tmp=0.0_SSK
          DO k=ia(i),ia(i+1)-1
            j=ja(k)
            tmp=tmp+aa(k)*x(j)
          ENDDO
          y(i)=y(i)+tmp
        ENDDO
      ENDIF
    ENDSUBROUTINE scsrmv_noALPHABETA
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::scsrmv_noALPHABETA "scsrmv_noALPHABETA"
!> for when the user does not want to pass @c nnz.
!> @param n order of the linear system
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE scsrmv_noALPHABETANNZ(n,ia,ja,aa,x,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SSK),INTENT(IN) :: aa(:)
      REAL(SSK),INTENT(IN) :: x(n)
      REAL(SSK),INTENT(INOUT) :: y(n)
      INTEGER(SIK) :: nnz
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja)) CALL scsrmv_noALPHABETA(n,nnz,ia,ja,aa,x,y)
    ENDSUBROUTINE scsrmv_noALPHABETANNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::scsrmv_noALPHABETA "scsrmv_noALPHABETA"
!> for when the user does not want to pass @c nnz and @c n.
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param x the single-precision vector to multiply with @c A
!> @param y the single-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE scsrmv_noALPHABETANNNZ(ia,ja,aa,x,y)
      INTEGER(SIK),INTENT(IN) :: ia(:)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SSK),INTENT(IN) :: aa(:)
      REAL(SSK),INTENT(IN) :: x(:)
      REAL(SSK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: nnz,n
      n=SIZE(x)
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja) .AND. SIZE(y) == n .AND. SIZE(ia) == n+1) &
        CALL scsrmv_noALPHABETA(n,nnz,ia,ja,aa,x,y)
    ENDSUBROUTINE scsrmv_noALPHABETANNNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the matrix-vector product for a square sparse
!> matrix stored in CSR format.
!> @param n order of the linear system
!> @param nnz the number of non-zero elements in @c A
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param alpha the double-precision scalar used to scale @c A
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE dcsrmv_all(n,nnz,ia,ja,aa,alpha,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: nnz
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(nnz)
      REAL(SDK),INTENT(IN) :: aa(nnz)
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: x(n)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(n)
             
      INTEGER(SIK) :: i,j,k
      REAL(SDK) :: tmp
      
      IF(n > 0 .AND. nnz >= n .AND. .NOT.(alpha == 0.0_SDK .AND. &
        beta == 1.0_SDK)) THEN
        
        !Compute y <- beta*y
        IF(beta /= 1.0_SDK) CALL BLAS_scal(n,beta,y,1)
        
        IF(alpha /= 0.0_SDK) THEN
          !y <- Ax+y
          DO i=1,n
            tmp=0.0_SDK
            DO k=ia(i),ia(i+1)-1
              j=ja(k)
              tmp=tmp+aa(k)*x(j)
            ENDDO
            y(i)=y(i)+alpha*tmp
          ENDDO
        ENDIF
      ENDIF
    ENDSUBROUTINE dcsrmv_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dcsrmv_all "dcsrmv_all" for when the 
!> user does not want to pass @c nnz.
!> @param n order of the linear system
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param alpha the double-precision scalar used to scale @c A
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE dcsrmv_noNNZ(n,ia,ja,aa,alpha,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SDK),INTENT(IN) :: aa(:)
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: x(n)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(n)
      INTEGER(SIK) :: nnz
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja)) CALL dcsrmv_all(n,nnz,ia,ja,aa,alpha,x,beta,y)
    ENDSUBROUTINE dcsrmv_noNNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dcsrmv_all "dcsrmv_all" for when the 
!> user does not want to pass @c nnz or @c n.
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param alpha the double-precision scalar used to scale @c A
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE dcsrmv_noNNNZ(ia,ja,aa,alpha,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: ia(:)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SDK),INTENT(IN) :: aa(:)
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: nnz,n
      n=SIZE(x)
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja) .AND. SIZE(y) == n .AND. SIZE(ia) == n+1) &
        CALL dcsrmv_all(n,nnz,ia,ja,aa,alpha,x,beta,y)
    ENDSUBROUTINE dcsrmv_noNNNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the matrix-vector product for a square sparse
!> matrix stored in CSR format.
!> @param n order of the linear system
!> @param nnz the number of non-zero elements in @c A
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> This routine is similar to @ref BLAS2::dcsrmv_all "dcsrmv_all" except 
!> @c alpha is assumed to be 1 so the floating point operation multiplying 
!> @c alpha is explicitly removed reducing the FLOP count by a factor of @c n 
!> multiplications.
!>
    PURE SUBROUTINE dcsrmv_noALPHA(n,nnz,ia,ja,aa,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: nnz
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(nnz)
      REAL(SDK),INTENT(IN) :: aa(nnz)
      REAL(SDK),INTENT(IN) :: x(n)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(n)
      
      INTEGER(SIK) :: i,j,k
      REAL(SDK) :: tmp
      
      IF(n > 0 .AND. nnz >= n) THEN
        
        !Compute y <- beta*y
        IF(beta /= 1.0_SDK) CALL BLAS_scal(n,beta,y,1)
        
        !y <- Ax+y
        DO i=1,n
          tmp=0.0_SDK
          DO k=ia(i),ia(i+1)-1
            j=ja(k)
            tmp=tmp+aa(k)*x(j)
          ENDDO
          y(i)=y(i)+tmp
        ENDDO
      ENDIF
    ENDSUBROUTINE dcsrmv_noALPHA
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dcsrmv_noALPHA "dcsrmv_noALPHA" for when
!> the user does not want to pass @c nnz.
!> @param n order of the linear system
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE dcsrmv_noALPHANNZ(n,ia,ja,aa,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SDK),INTENT(IN) :: aa(:)
      REAL(SDK),INTENT(IN) :: x(n)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(n)
      INTEGER(SIK) :: nnz
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja)) CALL dcsrmv_noALPHA(n,nnz,ia,ja,aa,x,beta,y)
    ENDSUBROUTINE dcsrmv_noALPHANNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dcsrmv_noALPHA "dcsrmv_noALPHA" for when
!> the user does not want to pass @c nnz and @c n.
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param x the double-precision vector to multiply with @c A
!> @param beta the double-precision scalar used to scale @c y
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE dcsrmv_noALPHANNNZ(ia,ja,aa,x,beta,y)
      INTEGER(SIK),INTENT(IN) :: ia(:)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SDK),INTENT(IN) :: aa(:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: nnz,n
      n=SIZE(x)
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja) .AND. SIZE(y) == n .AND. SIZE(ia) == n+1) &
        CALL dcsrmv_noALPHA(n,nnz,ia,ja,aa,x,beta,y)
    ENDSUBROUTINE dcsrmv_noALPHANNNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the matrix-vector product for a square sparse
!> matrix stored in CSR format.
!> @param n order of the linear system
!> @param nnz the number of non-zero elements in @c A
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param alpha the double-precision scalar used to scale @c A
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> This routine is similar to @ref BLAS2::dcsrmv_all "dcsrmv_all" except @c beta
!> is assumed to be 1 so the branching statement to @ref BLAS1::BLAS_scal 
!> "BLAS_scal" is explicitly removed.
!>
    PURE SUBROUTINE dcsrmv_noBETA(n,nnz,ia,ja,aa,alpha,x,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: nnz
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(nnz)
      REAL(SDK),INTENT(IN) :: aa(nnz)
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: x(n)
      REAL(SDK),INTENT(INOUT) :: y(n)
             
      INTEGER(SIK) :: i,j,k
      REAL(SDK) :: tmp
      
      IF(n > 0 .AND. nnz >= n .AND. alpha /= 0.0_SDK) THEN
        !y <- Ax+y
        DO i=1,n
          tmp=0.0_SDK
          DO k=ia(i),ia(i+1)-1
            j=ja(k)
            tmp=tmp+aa(k)*x(j)
          ENDDO
          y(i)=y(i)+alpha*tmp
        ENDDO
      ENDIF
    ENDSUBROUTINE dcsrmv_noBETA
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dcsrmv_noBETA "dcsrmv_noBETA" for when
!> the user does not want to pass @c nnz.
!> @param n order of the linear system
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param alpha the double-precision scalar used to scale @c A
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE dcsrmv_noBETANNZ(n,ia,ja,aa,alpha,x,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SDK),INTENT(IN) :: aa(:)
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: x(n)
      REAL(SDK),INTENT(INOUT) :: y(n)
      INTEGER(SIK) :: nnz
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja)) CALL dcsrmv_noBETA(n,nnz,ia,ja,aa,alpha,x,y)
    ENDSUBROUTINE dcsrmv_noBETANNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dcsrmv_noBETA "dcsrmv_noBETA" for when
!> the user does not want to pass @c nnz and @c n.
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param alpha the double-precision scalar used to scale @c A
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE dcsrmv_noBETANNNZ(ia,ja,aa,alpha,x,y)
      INTEGER(SIK),INTENT(IN) :: ia(:)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SDK),INTENT(IN) :: aa(:)
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: nnz,n
      n=SIZE(x)
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja) .AND. SIZE(y) == n .AND. SIZE(ia) == n+1) &
        CALL dcsrmv_noBETA(n,nnz,ia,ja,aa,alpha,x,y)
    ENDSUBROUTINE dcsrmv_noBETANNNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes the matrix-vector product for a square sparse
!> matrix stored in CSR format.
!> @param n order of the linear system
!> @param nnz the number of non-zero elements in @c A
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
!> This routine is similar to @ref BLAS2::dcsrmv_all "dcsrmv_all" except @c beta
!> is assumed to be 1 so the branching statement to @ref BLAS1::BLAS_scal 
!> "BLAS_scal" is explicitly removed. @c alpha is also assumed to be 1 so the
!> floating point operation multiplying @c alpha is explicitly removed reducing
!> the FLOP count by a factor of @c n multiplications.
!>
    PURE SUBROUTINE dcsrmv_noALPHABETA(n,nnz,ia,ja,aa,x,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: nnz
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(nnz)
      REAL(SDK),INTENT(IN) :: aa(nnz)
      REAL(SDK),INTENT(IN) :: x(n)
      REAL(SDK),INTENT(INOUT) :: y(n)
             
      INTEGER(SIK) :: i,j,k
      REAL(SDK) :: tmp
      
      IF(n > 0 .AND. nnz >= n) THEN
        !y <- Ax+y
        DO i=1,n
          tmp=0.0_SDK
          DO k=ia(i),ia(i+1)-1
            j=ja(k)
            tmp=tmp+aa(k)*x(j)
          ENDDO
          y(i)=y(i)+tmp
        ENDDO
      ENDIF
    ENDSUBROUTINE dcsrmv_noALPHABETA
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dcsrmv_noALPHABETA "dcsrmv_noALPHABETA"
!> for when the user does not want to pass @c nnz.
!> @param n order of the linear system
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE dcsrmv_noALPHABETANNZ(n,ia,ja,aa,x,y)
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: ia(n+1)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SDK),INTENT(IN) :: aa(:)
      REAL(SDK),INTENT(IN) :: x(n)
      REAL(SDK),INTENT(INOUT) :: y(n)
      INTEGER(SIK) :: nnz
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja)) CALL dcsrmv_noALPHABETA(n,nnz,ia,ja,aa,x,y)
    ENDSUBROUTINE dcsrmv_noALPHABETANNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS2::dcsrmv_noALPHABETA "dcsrmv_noALPHABETA"
!> for when the user does not want to pass @c nnz and @c n.
!> @param ia indexing array for @c ja and @c aa whose consecutive elements gives
!>        the number of non-zero elements in the row.
!> @param ja indexing array containing the column index
!> @param aa the non-zero entries in @c A in CSR format.
!> @param x the double-precision vector to multiply with @c A
!> @param y the double-precision vector to add to the product of @c A and @c x
!>
    PURE SUBROUTINE dcsrmv_noALPHABETANNNZ(ia,ja,aa,x,y)
      INTEGER(SIK),INTENT(IN) :: ia(:)
      INTEGER(SIK),INTENT(IN) :: ja(:)
      REAL(SDK),INTENT(IN) :: aa(:)
      REAL(SDK),INTENT(IN) :: x(:)
      REAL(SDK),INTENT(INOUT) :: y(:)
      INTEGER(SIK) :: nnz,n
      n=SIZE(x)
      nnz=SIZE(aa)
      IF(nnz == SIZE(ja) .AND. SIZE(y) == n .AND. SIZE(ia) == n+1) &
        CALL dcsrmv_noALPHABETA(n,nnz,ia,ja,aa,x,y)
    ENDSUBROUTINE dcsrmv_noALPHABETANNNZ
!
!-------------------------------------------------------------------------------
!> @brief Subroutine solves a triangular matrix linear system.
!> @param uplo single character input indicating if an upper (U) or lower (L) 
!>        maxtrix is stored in @c A
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param diag single character input indicating whether or not a unity
!>        diagonal is used
!> @param n the size of the dimension of @c A (number of rows and columns)
!> @param A the single-precision matrix multiply with @c x
!> @param lda the size of the leading (first) dimension of @c A
!> @param x the single-precision vector to multiply with @c A
!> @param incx the increment to use when looping over elements in @c x
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/strsv.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE strsv_all(uplo,trans,diag,a,x)
      CHARACTER(LEN=1),INTENT(IN) :: uplo
      CHARACTER(LEN=1),INTENT(IN) :: trans
      CHARACTER(LEN=1),INTENT(IN) :: diag
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(INOUT) :: x(:)
      INTEGER(SIK) :: incx
      INTEGER(SIK) :: lda
      INTEGER(SIK) :: n

#ifdef HAVE_BLAS
      INTERFACE
        PURE SUBROUTINE strsv(uplo,trans,diag,n,a,lda,x,incx)
          CHARACTER(LEN=1),INTENT(IN) :: uplo
          CHARACTER(LEN=1),INTENT(IN) :: trans
          CHARACTER(LEN=1),INTENT(IN) :: diag
          INTEGER,INTENT(IN) :: n
          INTEGER,INTENT(IN) :: lda
          REAL(KIND(0.0e0)),INTENT(IN) :: a(lda,*)
          REAL(KIND(0.0e0)),INTENT(INOUT) :: x(*)
          INTEGER,INTENT(IN) :: incx
        ENDSUBROUTINE strsv
      ENDINTERFACE
      n=SIZE(a,DIM=2)
      lda=SIZE(a,DIM=1)
      incx=1_SIK
      CALL strsv(uplo,trans,diag,n,a,lda,x,incx)
#else
      LOGICAL(SBK) :: ltrans, nounit
      INTEGER(SIK) :: i,ix,j,jx,kx
      REAL(SSK) :: temp
      REAL(SSK),PARAMETER :: ZERO=0.0_SSK
      INTRINSIC MAX
    
      n=SIZE(a,DIM=2)
      incx=1_SIK
      IF(n > 0 .AND. incx /= 0 .AND. &
          (trans == 't' .OR. trans == 'T' .OR. trans == 'c' .OR. trans == 'C' .OR. &
            trans == 'n' .OR. trans == 'N') .AND. &
          (uplo == 'u' .OR. uplo == 'U' .OR. uplo == 'l' .OR. uplo == 'L') .AND. &
          (diag == 't' .OR. diag == 'T' .OR. diag == 'n' .OR. diag == 'N')) THEN

        IF (diag == 'n' .OR. diag == 'N') nounit=.TRUE.
 
        IF (incx<=0) THEN
            kx = 1 - (n-1)*incx
        ELSEIF (incx/=1) THEN
            kx = 1
        END IF

        IF (trans == 'n' .OR. trans == 'N') THEN  ! Form  x := inv( A )*x.
          IF (uplo == 'u' .OR. uplo == 'U') THEN  ! Upper triangular
            IF (incx.EQ.1) THEN
              DO j = n,1,-1
                IF (x(j)/=ZERO) THEN
                  IF (nounit) x(j)=x(j)/a(j,j)
                  temp=x(j)
                  DO i=j-1,1,-1
                    x(i)=x(i)-temp*a(i,j)
                  ENDDO
                ENDIF
              ENDDO
            ELSE
              jx=kx+(n-1)*incx
              DO J=n,1,-1
                IF (x(jx)/=ZERO) THEN
                  IF (nounit) x(jx)=x(jx)/a(j,j)
                  temp=x(jx)
                  ix=jx
                  DO i=j-1,1,-1
                    ix=ix-incx
                    x(ix)=x(ix)-temp*a(i,j)
                  ENDDO
                ENDIF
                jx=jx-incx
              ENDDO
            ENDIF
          ELSE  ! Lower Triangular
            IF (incx==1) THEN
              DO j=1,n
                IF (x(j)/=ZERO) THEN
                  IF (nounit) x(j)=x(j)/a(j,j)
                  temp=x(j)
                  DO i=j+1,n
                    x(i)=x(i)-temp*a(i,j)
                  ENDDO
                ENDIF
              ENDDO
            ELSE
              jx=kx
              DO j=1,n
                IF (x(jx)/=ZERO) THEN
                  IF (nounit) x(jx)=x(jx)/a(j,j)
                  temp=x(jx)
                  ix=jx
                  DO i=j+1,n
                    ix=ix+incx
                    x(ix)=x(ix)-temp*a(i,j)
                  ENDDO
                ENDIF
                jx=jx+incx
              ENDDO
            ENDIF
          ENDIF
        ELSE  ! Form  x := inv( A**T )*x.
          IF (uplo == 'u' .OR. uplo == 'U') THEN
            IF (incx==1) THEN
              DO j=1,n
                temp=x(j)
                DO i=1,j - 1
                  temp=temp-a(i,j)*x(i)
                ENDDO
                IF (nounit) temp=temp/a(j,j)
                x(j)=temp
              ENDDO
            ELSE
              jx=kx
              DO j=1,n
                temp=x(jx)
                ix=kx
                DO i=1,j-1
                  temp=temp-a(i,j)*x(ix)
                  ix=ix+incx
                ENDDO
                IF (nounit) temp=temp/a(j,j)
                x(jx)=temp
                jx=jx+incx
              ENDDO
            ENDIF
          ELSE  ! Lower Triangular
            IF (incx==1) THEN
              DO j=n,1,-1
                temp=x(j)
                DO i=n,j + 1,-1
                  temp=temp-a(i,j)*x(i)
                ENDDO
                IF (nounit) temp=temp/a(j,j)
                x(j)=temp
              ENDDO
            ELSE
              kx=kx+(n-1)*incx
              jx=kx
              DO j=n,1,-1
                temp=x(jx)
                ix=kx
                DO i=n,j+1,-1
                  temp=temp-a(i,j)*x(ix)
                  ix=ix-incx
                ENDDO
                IF (nounit) temp=temp/a(j,j)
                x(jx)=temp
                jx=jx-incx
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDIF

#endif
    ENDSUBROUTINE strsv_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine solves a triangular matrix linear system.
!> @param uplo single character input indicating if an upper (U) or lower (L) 
!>        maxtrix is stored in @c A
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param diag single character input indicating whether or not a unity
!>        diagonal is used
!> @param n the size of the dimension of @c A (number of rows and columns)
!> @param A the double-precision matrix multiply with @c x
!> @param lda the size of the leading (first) dimension of @c A
!> @param x the double-precision vector to multiply with @c A
!> @param incx the increment to use when looping over elements in @c x
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/strsv.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE dtrsv_all(uplo,trans,diag,a,x)
      CHARACTER(LEN=1),INTENT(IN) :: uplo
      CHARACTER(LEN=1),INTENT(IN) :: trans
      CHARACTER(LEN=1),INTENT(IN) :: diag
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(INOUT) :: x(:)
      INTEGER(SIK) :: incx
      INTEGER(SIK) :: lda
      INTEGER(SIK) :: n

#ifdef HAVE_BLAS
      INTERFACE
        PURE SUBROUTINE dtrsv(uplo,trans,diag,n,a,lda,x,incx)
          CHARACTER(LEN=1),INTENT(IN) :: uplo
          CHARACTER(LEN=1),INTENT(IN) :: trans
          CHARACTER(LEN=1),INTENT(IN) :: diag
          INTEGER,INTENT(IN) :: n
          INTEGER,INTENT(IN) :: lda
          REAL(KIND(0.0d0)),INTENT(IN) :: a(lda,*)
          REAL(KIND(0.0d0)),INTENT(INOUT) :: x(*)
          INTEGER,INTENT(IN) :: incx
        ENDSUBROUTINE dtrsv
      ENDINTERFACE
      n=SIZE(a,DIM=2)
      lda=SIZE(a,DIM=1)
      incx=1_SIK
      CALL dtrsv(uplo,trans,diag,n,a,lda,x,incx)
#else
      LOGICAL(SBK) :: ltrans, nounit
      INTEGER(SIK) :: i,ix,j,jx,kx
      REAL(SDK) :: temp
      REAL(SDK),PARAMETER :: ZERO=0.0_SSK
      INTRINSIC MAX
    
      n=SIZE(a,DIM=2)
      lda=SIZE(a,DIM=1)
      incx=1_SIK
      IF(n > 0 .AND. incx /= 0 .AND. lda >= MAX(1,N) .AND. &
          (trans == 't' .OR. trans == 'T' .OR. trans == 'c' .OR. trans == 'C' .OR. &
            trans == 'n' .OR. trans == 'N') .AND. &
          (uplo == 'u' .OR. uplo == 'U' .OR. uplo == 'l' .OR. uplo == 'L') .AND. &
          (diag == 't' .OR. diag == 'T' .OR. diag == 'n' .OR. diag == 'N')) THEN

        IF (diag == 'n' .OR. diag == 'N') THEN
          nounit=.TRUE.
        ELSE
          nounit=.FALSE.
        ENDIF
 
        IF (incx<=0) THEN
            kx = 1 - (n-1)*incx
        ELSEIF (incx/=1) THEN
            kx = 1
        END IF

        IF (trans == 'n' .OR. trans == 'N') THEN  ! Form  x := inv( A )*x.
          IF (uplo == 'u' .OR. uplo == 'U') THEN  ! Upper triangular
            IF (incx.EQ.1) THEN
              DO j = n,1,-1
                IF (x(j)/=ZERO) THEN
                  IF (nounit) x(j)=x(j)/a(j,j)
                  temp=x(j)
                  DO i=j-1,1,-1
                    x(i)=x(i)-temp*a(i,j)
                  ENDDO
                ENDIF
              ENDDO
            ELSE
              jx=kx+(n-1)*incx
              DO J=n,1,-1
                IF (x(jx)/=ZERO) THEN
                  IF (nounit) x(jx)=x(jx)/a(j,j)
                  temp=x(jx)
                  ix=jx
                  DO i=j-1,1,-1
                    ix=ix-incx
                    x(ix)=x(ix)-temp*a(i,j)
                  ENDDO
                ENDIF
                jx=jx-incx
              ENDDO
            ENDIF
          ELSE  ! Lower Triangular
            IF (incx==1) THEN
              DO j=1,n
                IF (x(j)/=ZERO) THEN
                  IF (nounit) x(j)=x(j)/a(j,j)
                  temp=x(j)
                  DO i=j+1,n
                    x(i)=x(i)-temp*a(i,j)
                  ENDDO
                ENDIF
              ENDDO
            ELSE
              jx=kx
              DO j=1,n
                IF (x(jx)/=ZERO) THEN
                  IF (nounit) x(jx)=x(jx)/a(j,j)
                  temp=x(jx)
                  ix=jx
                  DO i=j+1,n
                    ix=ix+incx
                    x(ix)=x(ix)-temp*a(i,j)
                  ENDDO
                ENDIF
                jx=jx+incx
              ENDDO
            ENDIF
          ENDIF
        ELSE  ! Form  x := inv( A**T )*x.
          IF (uplo == 'u' .OR. uplo == 'U') THEN
            IF (incx==1) THEN
              DO j=1,n
                temp=x(j)
                DO i=1,j - 1
                  temp=temp-a(i,j)*x(i)
                ENDDO
                IF (nounit) temp=temp/a(j,j)
                x(j)=temp
              ENDDO
            ELSE
              jx=kx
              DO j=1,n
                temp=x(jx)
                ix=kx
                DO i=1,j-1
                  temp=temp-a(i,j)*x(ix)
                  ix=ix+incx
                ENDDO
                IF (nounit) temp=temp/a(j,j)
                x(jx)=temp
                jx=jx+incx
              ENDDO
            ENDIF
          ELSE  ! Lower Triangular
            IF (incx==1) THEN
              DO j=n,1,-1
                temp=x(j)
                DO i=n,j + 1,-1
                  temp=temp-a(i,j)*x(i)
                ENDDO
                IF (nounit) temp=temp/a(j,j)
                x(j)=temp
              ENDDO
            ELSE
              kx=kx+(n-1)*incx
              jx=kx
              DO j=n,1,-1
                temp=x(jx)
                ix=kx
                DO i=n,j+1,-1
                  temp=temp-a(i,j)*x(ix)
                  ix=ix-incx
                ENDDO
                IF (nounit) temp=temp/a(j,j)
                x(jx)=temp
                jx=jx-incx
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDIF

#endif
    ENDSUBROUTINE dtrsv_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine solves a triangular matrix linear system.
!> @param uplo single character input indicating if an upper (U) or lower (L) 
!>        maxtrix is stored in @c A
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param diag single character input indicating whether or not a unity
!>        diagonal is used
!> @param n the size of the dimension of @c A (number of rows and columns)
!> @param A the double-precision matrix multiply with @c x
!> @param lda the size of the leading (first) dimension of @c A
!> @param x the double-precision vector to multiply with @c A
!> @param incx the increment to use when looping over elements in @c x
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/strsv.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE strsv_all_sparse(uplo,trans,diag,a,ia,ja,x)
      CHARACTER(LEN=1),INTENT(IN) :: uplo
      CHARACTER(LEN=1),INTENT(IN) :: trans
      CHARACTER(LEN=1),INTENT(IN) :: diag
      REAL(SSK),INTENT(IN) :: a(:)
      INTEGER(SIK),INTENT(IN) :: ia(:)
      INTEGER(SIK),INTENT(IN) :: ja(SIZE(a))
      REAL(SSK),INTENT(INOUT) :: x(SIZE(ia)-1)
      INTEGER(SIK) :: n

!#ifdef HAVE_BLAS
!  Need to track down the BLAS routine for this, if it exists, and insert it here
!#else
      LOGICAL(SBK) :: ltrans, nounit
      INTEGER(SIK) :: i,ix,j,jx,kx
      REAL(SSK) :: temp
      REAL(SSK),PARAMETER :: ZERO=0.0_SSK
      INTRINSIC MAX
    
      n=SIZE(x)
      IF((trans == 't' .OR. trans == 'T' .OR. trans == 'c' .OR. trans == 'C' .OR. &
           trans == 'n' .OR. trans == 'N') .AND. &
          (uplo == 'u' .OR. uplo == 'U' .OR. uplo == 'l' .OR. uplo == 'L') .AND. &
          (diag == 't' .OR. diag == 'T' .OR. diag == 'n' .OR. diag == 'N')) THEN

        IF (diag == 'n' .OR. diag == 'N') nounit=.TRUE.
 
        IF (trans == 'n' .OR. trans == 'N') THEN  ! Form  x := inv( A )*x.
          IF (uplo == 'u' .OR. uplo == 'U') THEN  ! Upper triangular
            IF((ANY(ia <= 0)) .AND. nounit) n=0 ! In case a row does not have any elements
            DO i=n,1,-1
              DO j=ia(i+1)-1,ia(i),-1
                IF(ja(j) <= i) EXIT
                x(i)=x(i)-a(j)*x(ja(j))
              ENDDO
              IF(ja(j) == i) THEN
                x(i)=x(i)/a(j)
              ELSEIF(nounit) THEN
                x(i)=0.0_SDK
              ENDIF
            ENDDO
          ELSE  ! Lower Triangular
            IF((ANY(ia <= 0)) .AND. nounit) n=0 ! In case a row does not have any elements
            DO i=1,n
              DO j=ia(i),ia(i+1)-1
                IF(ja(j) >= i) EXIT
                x(i)=x(i)-a(j)*x(ja(j))
              ENDDO
              IF(ja(j) == i) THEN
                x(i)=x(i)/a(j)
              ELSEIF(nounit) THEN
                x(i)=0.0_SDK
              ENDIF
            ENDDO
          ENDIF
        ELSE  ! Form  x := inv( A**T )*x.
          IF (uplo == 'u' .OR. uplo == 'U') THEN
            DO j = 1,SIZE(ia)-1
              IF(.NOT.(x(j) .APPROXEQA. ZERO)) THEN
                IF (nounit) x(j)=x(j)/a(ia(j))
                temp=x(j)
                DO i=ia(j)+1,ia(j+1)-1
                  IF(ja(i) <= j) CYCLE
                  x(ja(i))=x(ja(i))-temp*a(i)
                ENDDO
              ENDIF
            ENDDO
          ELSE  ! Lower Triangular
            DO i=n,1,-1
              IF(.NOT.(x(i) .APPROXEQA. ZERO)) THEN
                IF(nounit) x(i)=x(i)/a(ia(i+1)-1)
                temp=x(i)
                DO j=ia(i+1)-2,ia(i),-1
                  IF(ja(j) > i) CYCLE
                  x(ja(j))=x(ja(j))-a(j)*temp
                ENDDO
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDIF

!#endif
    ENDSUBROUTINE strsv_all_sparse
!
!-------------------------------------------------------------------------------
!> @brief Subroutine solves a triangular matrix linear system.
!> @param uplo single character input indicating if an upper (U) or lower (L) 
!>        maxtrix is stored in @c A
!> @param trans single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param diag single character input indicating whether or not a unity
!>        diagonal is used
!> @param n the size of the dimension of @c A (number of rows and columns)
!> @param A the double-precision matrix multiply with @c x
!> @param lda the size of the leading (first) dimension of @c A
!> @param x the double-precision vector to multiply with @c A
!> @param incx the increment to use when looping over elements in @c x
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/strsv.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE dtrsv_all_sparse(uplo,trans,diag,a,ia,ja,x)
      CHARACTER(LEN=1),INTENT(IN) :: uplo
      CHARACTER(LEN=1),INTENT(IN) :: trans
      CHARACTER(LEN=1),INTENT(IN) :: diag
      REAL(SDK),INTENT(IN) :: a(:)
      INTEGER(SIK),INTENT(IN) :: ia(:)
      INTEGER(SIK),INTENT(IN) :: ja(SIZE(a))
      REAL(SDK),INTENT(INOUT) :: x(SIZE(ia)-1)

!#ifdef HAVE_BLAS
!  Need to track down the BLAS routine for this, if it exists, and insert it here
!#else
      LOGICAL(SBK) :: ltrans, nounit
      INTEGER(SIK) :: i,ix,j,jx,kx,n
      REAL(SDK) :: temp
      REAL(SDK),PARAMETER :: ZERO=0.0_SDK
      INTRINSIC MAX
    
      n=SIZE(x)
      IF((trans == 't' .OR. trans == 'T' .OR. trans == 'c' .OR. trans == 'C' .OR. &
           trans == 'n' .OR. trans == 'N') .AND. &
          (uplo == 'u' .OR. uplo == 'U' .OR. uplo == 'l' .OR. uplo == 'L') .AND. &
          (diag == 't' .OR. diag == 'T' .OR. diag == 'n' .OR. diag == 'N')) THEN

        IF (diag == 'n' .OR. diag == 'N') THEN
          nounit=.TRUE.
        ELSE
          nounit=.FALSE.
        ENDIF
 
        IF (trans == 'n' .OR. trans == 'N') THEN  ! Form  x := inv( A )*x.
          IF (uplo == 'u' .OR. uplo == 'U') THEN  ! Upper triangular
            IF((ANY(ia <= 0)) .AND. nounit) n=0 ! In case a row does not have any elements
            DO i=n,1,-1
              DO j=ia(i+1)-1,ia(i),-1
                IF(ja(j) <= i) EXIT
                x(i)=x(i)-a(j)*x(ja(j))
              ENDDO
              IF((ja(j) == i) .AND. nounit) THEN
                x(i)=x(i)/a(j)
              ENDIF
            ENDDO
          ELSE  ! Lower Triangular
            IF((ANY(ia <= 0)) .AND. nounit) n=0 ! In case a row does not have any elements
            DO i=1,n
              DO j=ia(i),ia(i+1)-1
                IF(ja(j) >= i) EXIT
                x(i)=x(i)-a(j)*x(ja(j))
                IF(j == SIZE(ja)) EXIT
              ENDDO
              IF((ja(j) == i) .AND. nounit) THEN
                x(i)=x(i)/a(j)
              ENDIF
            ENDDO
          ENDIF
        ELSE  ! Form  x := inv( A**T )*x.
          IF (uplo == 'u' .OR. uplo == 'U') THEN
            DO j = 1,SIZE(ia)-1
              IF(.NOT.(x(j) .APPROXEQA. ZERO)) THEN
                IF (nounit) x(j)=x(j)/a(ia(j))
                temp=x(j)
                DO i=ia(j)+1,ia(j+1)-1
                  IF(ja(i) <= j) CYCLE
                  x(ja(i))=x(ja(i))-temp*a(i)
                ENDDO
              ENDIF
            ENDDO
          ELSE  ! Lower Triangular
            DO i=n,1,-1
              IF(.NOT.(x(i) .APPROXEQA. ZERO)) THEN
                IF(nounit) x(i)=x(i)/a(ia(i+1)-1)
                temp=x(i)
                DO j=ia(i+1)-2,ia(i),-1
                  IF(ja(j) > i) CYCLE
                  x(ja(j))=x(ja(j))-a(j)*temp
                ENDDO
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDIF

!#endif
    ENDSUBROUTINE dtrsv_all_sparse
!
ENDMODULE BLAS2
