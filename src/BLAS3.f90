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
!> @brief A utility module providing an interface to Level 3 BLAS functionality.
!>
!> This module provides simplified generic interfaces to level 3 BLAS routines.
!> It currently only provides an interface to matrix matrix multiplication.
!>
!> The following interfaces are provided:
!>  - @ref BLAS3::BLAS_matmat "BLAS_matmat": @copybrief BLAS3::BLAS_matmat
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref BLAS1 "BLAS1": @copybrief BLAS1
!>  - @ref BLAS2 "BLAS2": @copybrief BLAS2
!>
!> @par EXAMPLES
!> @code
!> @endcode
!>
!> @author Daniel Jabaay
!>    @date 03/26/2012
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE BLAS3

  USE IntrType
  USE BLAS1
  USE BLAS2
  IMPLICIT NONE
  PRIVATE !Default private for module contents
!
! List of Public items
  PUBLIC :: BLAS_matmat
  
  !> @brief Generic interface to the Level 3 BLAS routine (MM)
  !>
  !> It provides an interface to routines that perform one of the following
  !> operations:
  !> C <- alpha*A*B+beta*C
  !> C <- alpha*A^T*B+beta*C
  !> C <- alpha*A*B^T+beta*C
  !> C <- alpha*A^T*B^T+beta*C
  !>
  !> Where T is the transpose operator.
  !> Simplified interfaces are also provided so that dimension information
  !> is optional as is @c alpha and @c beta.  If these inputs are excluded, a different
  !> algorithm is used (when no external BLAS library is present).
  !>
  !> Currently, only routines for performing those operations on full, dense matrices. 
  !> Extensions will include CSR matrix multiplication to the interface in the future.
  !>
  INTERFACE BLAS_matmat
    !> @copybrief BLAS3::sgemm_all @copydetails BLAS3::sgemm_all
    MODULE PROCEDURE sgemm_all
    !> @copybrief BLAS3::sgemm_ttmnkaabbc @copydetails BLAS3::sgemm_ttmnkaabbc
    MODULE PROCEDURE sgemm_ttmnkaabbc
    !> @copybrief BLAS3::sgemm_ttmnkabbc @copydetails BLAS3::sgemm_ttmnkabbc
    MODULE PROCEDURE sgemm_ttmnkabbc
    !> @copybrief BLAS3::sgemm_ttmnkaabc @copydetails BLAS3::sgemm_ttmnkaabc
    MODULE PROCEDURE sgemm_ttmnkaabc
    !> @copybrief BLAS3::sgemm_ttmnkabc @copydetails BLAS3::sgemm_ttmnkabc
    MODULE PROCEDURE sgemm_ttmnkabc
    !> @copybrief BLAS3::sgemm_mnkaabbc @copydetails BLAS3::sgemm_mnkaabbc
    MODULE PROCEDURE sgemm_mnkaabbc
    !> @copybrief BLAS3::sgemm_ttaabbc @copydetails BLAS3::sgemm_ttaabbc
    MODULE PROCEDURE sgemm_ttaabbc
    !> @copybrief BLAS3::sgemm_aabbc @copydetails BLAS3::sgemm_aabbc
    MODULE PROCEDURE sgemm_aabbc
    !> @copybrief BLAS3::sgemm_abbc @copydetails BLAS3::sgemm_abbc
    MODULE PROCEDURE sgemm_abbc
    !> @copybrief BLAS3::sgemm_aabc @copydetails BLAS3::sgemm_aabc
    MODULE PROCEDURE sgemm_aabc
    !> @copybrief BLAS3::sgemm_abc @copydetails BLAS3::sgemm_abc
    MODULE PROCEDURE sgemm_abc
    !> @copybrief BLAS3::dgemv_all @copydetails BLAS3::dgemv_all
    MODULE PROCEDURE dgemm_all
    !> @copybrief BLAS3::dgemm_ttmnkaabbc @copydetails BLAS3::dgemm_ttmnkaabbc
    MODULE PROCEDURE dgemm_ttmnkaabbc
    !> @copybrief BLAS3::dgemm_ttmnkabbc @copydetails BLAS3::dgemm_ttmnkabbc
    MODULE PROCEDURE dgemm_ttmnkabbc
    !> @copybrief BLAS3::dgemm_ttmnkaabc @copydetails BLAS3::dgemm_ttmnkaabc
    MODULE PROCEDURE dgemm_ttmnkaabc
    !> @copybrief BLAS3::dgemm_ttmnkabc @copydetails BLAS3::dgemm_ttmnkabc
    MODULE PROCEDURE dgemm_ttmnkabc
    !> @copybrief BLAS3::dgemm_mnkaabbc @copydetails BLAS3::dgemm_mnkaabbc
    MODULE PROCEDURE dgemm_mnkaabbc
    !> @copybrief BLAS3::dgemm_ttaabbc @copydetails BLAS3::dgemm_ttaabbc
    MODULE PROCEDURE dgemm_ttaabbc
    !> @copybrief BLAS3::dgemm_aabbc @copydetails BLAS3::dgemm_aabbc
    MODULE PROCEDURE dgemm_aabbc
    !> @copybrief BLAS3::dgemm_abbc @copydetails BLAS3::dgemm_abbc
    MODULE PROCEDURE dgemm_abbc
    !> @copybrief BLAS3::dgemm_aabc @copydetails BLAS3::dgemm_aabc
    MODULE PROCEDURE dgemm_aabc
    !> @copybrief BLAS3::dgemm_abc @copydetails BLAS3::dgemm_abc
    MODULE PROCEDURE dgemm_abc
  ENDINTERFACE BLAS_matmat
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes a matrix matrix product for general matricies
!>        A and B.
!> @param transa single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param transb single character input indicating whether or not to use the 
!>        transpose of @c B
!> @param m the size of the first dimension of @c A and of @c C (number of rows)
!> @param n the size of the second dimension of @c B and of @c C (number of columns)
!> @param k the size of the second dimension of @c A (number of columns) 
!>        and the first dimension of @c B (number of rows)
!> @param alpha the single-precision scalar used to scale @c A and @c B
!> @param A the single-precision matrix to multiply with @c B and @c alpha 
!> @param lda the size of the leading (first) dimension of @c A
!> @param B the single-precision matrix to multiply with @c A and @c alpha 
!> @param ldb the size of the leading (first) dimension of @c B
!> @param beta the single-precision scalar used to scale @c C
!> @param C the single-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!> @param ldc the size of the leading (first) dimension of @c C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE sgemm_all(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      CHARACTER(LEN=1),INTENT(IN) :: transa
      CHARACTER(LEN=1),INTENT(IN) :: transb
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: k
      REAL(SSK),INTENT(IN) :: alpha
      INTEGER(SIK),INTENT(IN) :: lda
      REAL(SSK),INTENT(IN) :: a(lda,*)
      INTEGER(SIK),INTENT(IN) :: ldb
      REAL(SSK),INTENT(IN) :: b(ldb,*)
      REAL(SSK),INTENT(IN) :: beta
      INTEGER(SIK),INTENT(IN) :: ldc
      REAL(SSK),INTENT(INOUT) :: c(ldc,*)
      
#ifdef HAVE_BLAS
      INTERFACE
        PURE SUBROUTINE sgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
          CHARACTER(LEN=1),INTENT(IN) :: transa
          CHARACTER(LEN=1),INTENT(IN) :: transb
          INTEGER,INTENT(IN) :: m
          INTEGER,INTENT(IN) :: n
          INTEGER,INTENT(IN) :: k
          REAL(KIND(0.0e0)),INTENT(IN) :: alpha
          INTEGER,INTENT(IN) :: lda
          REAL(KIND(0.0e0)),INTENT(IN) :: a(lda,*)
          INTEGER,INTENT(IN) :: ldb
          REAL(KIND(0.0e0)),INTENT(IN) :: b(ldb,*)
          REAL(KIND(0.0e0)),INTENT(IN) :: beta
          INTEGER,INTENT(IN) :: ldc
          REAL(KIND(0.0e0)),INTENT(INOUT) :: c(ldc,*)
        ENDSUBROUTINE sgemm
      ENDINTERFACE
      CALL sgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
#else
      LOGICAL(SBK) :: ltransa,ltransb
      INTEGER(SIK) :: i,j,l,akm,bkn,amaxval,bmaxval
      REAL(SSK) :: tmp
      INTRINSIC MAX
      
      !Check for transpose of A
      IF(transa == 'n' .OR. transa == 'N') THEN
        !If not transposed, akm is column number, amaxval is row number
        akm=k
        amaxval=m
        ltransa=.FALSE.
      ELSE
        !If transposed, akm is row number, amaxval is column number
        akm=m
        amaxval=k
        ltransa=.TRUE.
      ENDIF
      !Check for transpose of B
      IF(transb == 'n' .OR. transb == 'N') THEN
        !If not transposed, bkn is column number, bmaxval is row number
        bkn=n
        bmaxval=k
        ltransb=.FALSE.
      ELSE
        !If transposed, bkn is row number, bmaxval is column number
        bkn=k
        bmaxval=n
        ltransb=.TRUE.
      ENDIF
      !Input argument check
      IF(m > 0 .AND. n > 0 .AND. k > 0 .AND. ldc >= MAX(1,m)) THEN
      !.AND. &
      IF (  lda >= MAX(1,amaxval) .AND. ldb >= MAX(1,bmaxval) .AND. &
          .NOT.(alpha == 0.0_SSK .AND. beta == 1.0_SSK)) THEN
      !.AND. &
       IF(     (transa == 't' .OR. transa == 'T' .OR. transa == 'c' .OR. transa == 'C' .OR. &
              transa == 'n' .OR. transa == 'N')) THEN
       !.AND. 
       IF (transb == 't' .OR. transb == 'T' & 
                .OR. transb == 'c' .OR. transb == 'C' .OR. transb == 'n' .OR. transb == 'N') THEN
        
        !Compute beta*C for alpha=0. C := beta*C
        IF(alpha == 0.0_SSK) THEN
          !All entries become zero if alpha and beta are 0.
          IF(beta == 0.0_SSK) THEN
            !Possibly increase the IF branch to check n and m individually,
            !and accelerate individually
            IF(n*m > 10000) THEN
              DO j=1,n
                DO i=1,m
                  c(i,j)=0.0_SSK
                ENDDO
              ENDDO
            ELSE
              c(1:m,1:n)=0.0_SSK
            ENDIF
          ELSE
            IF(n*m > 10000) THEN
              DO j=1,n
                DO i=1,m
                  c(i,j)=beta*c(i,j)
                ENDDO
              ENDDO
            ELSE
              c(1:m,1:n)=beta*c(1:m,1:n)
            ENDIF
          ENDIF
        !Alpha /= 0.
        ELSE
          IF(.NOT.ltransa) THEN
            !Operation 1, untransposed. Form  C := alpha*A*B + beta*C.
            IF(.NOT.ltransb) THEN
              DO j=1,n
                IF(beta == 0.0_SSK) THEN
                  DO i=1,m
                    c(i,j)=0.0_SSK
                  ENDDO
                ELSEIF(beta /= 1.0_SSK) THEN
                  DO i=1,m
                    c(i,j)=beta*c(i,j)
                  ENDDO
                ENDIF
                DO l=1,k
                  IF(b(l,j) /= 0.0_SSK) THEN
                    tmp=alpha*b(l,j)
                    DO i=1,m
                      c(i,j)=c(i,j)+tmp*a(i,l)
                    ENDDO
                  ENDIF
                ENDDO
              ENDDO
            !Operation 2, B transposed. Form  C := alpha*A*B^T + beta*C.
            ELSE
              DO j=1,n
                IF(beta == 0.0_SSK) THEN
                  DO i=1,m
                    c(i,j)=0.0_SSK
                  ENDDO
                ELSE
                  DO i=1,m
                    c(i,j)=beta*c(i,j)
                  ENDDO
                ENDIF
                DO l=1,k
                  IF(b(j,l) /= 0.0_SSK) THEN
                    tmp=alpha*b(j,l)
                    DO i=1,m
                      c(i,j)=c(i,j)+tmp*a(i,l)
                    ENDDO
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
          ELSE
            !Operation 3, A transposed. Form  C := alpha*A^T*B + beta*C.
            IF(.NOT.ltransb) THEN
              DO j=1,n
                DO i=1,m
                  tmp=0.0_SSK
                  DO l=1,k
                    tmp=tmp+a(l,i)*b(l,j)
                  ENDDO
                  IF(beta == 0.0_SSK) THEN
                    c(i,j)=alpha*tmp
                  ELSE
                    c(i,j)=alpha*tmp+beta*c(i,j)
                  ENDIF
                ENDDO
              ENDDO
            !Operation 4, A & B transposed. Form  C := alpha*A^T*B^T + beta*C.  
            ELSE
              DO j=1,n
                DO i=1,m
                  tmp=0.0_SSK
                  DO l=1,k
                    tmp=tmp+a(l,i)*b(j,l)
                  ENDDO
                  IF(beta == 0.0_SSK) THEN
                    c(i,j)=alpha*tmp
                  ELSE
                    c(i,j)=alpha*tmp+beta*c(i,j)
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
          ENDIF
        ENDIF
              ENDIF  
          ENDIF
      ENDIF
    ENDIF
    
#endif
    ENDSUBROUTINE sgemm_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::sgemm_all "sgemm_all" for when the user
!>        does not want to supply @c lda, @c ldb, and @c ldc.
!> @param transa single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param transb single character input indicating whether or not to use the 
!>        transpose of @c B
!> @param m the size of the first dimension of @c A and of @c C (number of rows)
!> @param n the size of the second dimension of @c B and of @c C (number of columns)
!> @param k the size of the second dimension of @c A (number of columns) 
!>        and the first dimension of @c B (number of rows)
!> @param alpha the single-precision scalar used to scale @c A and @c B
!> @param A the single-precision matrix to multiply with @c B and @c alpha 
!> @param B the single-precision matrix to multiply with @c A and @c alpha 
!> @param beta the single-precision scalar used to scale @c C
!> @param C the single-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE sgemm_ttmnkaabbc(transa,transb,m,n,k,alpha,a,b,beta,c)
      CHARACTER(LEN=1),INTENT(IN) :: transa
      CHARACTER(LEN=1),INTENT(IN) :: transb
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: k
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: b(:,:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: c(:,:)

      CALL sgemm_all(transa,transb,m,n,k,alpha,a,m,b,k,beta,c,m)
      
    ENDSUBROUTINE sgemm_ttmnkaabbc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::sgemm_all "sgemm_all" for when the user
!>        does not want to supply @c alpha, @c lda, @c ldb, and @c ldc.
!> @param transa single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param transb single character input indicating whether or not to use the 
!>        transpose of @c B
!> @param m the size of the first dimension of @c A and of @c C (number of rows)
!> @param n the size of the second dimension of @c B and of @c C (number of columns)
!> @param k the size of the second dimension of @c A (number of columns) 
!>        and the first dimension of @c B (number of rows)
!> @param A the single-precision matrix to multiply with @c B and @c alpha 
!> @param B the single-precision matrix to multiply with @c A and @c alpha 
!> @param beta the single-precision scalar used to scale @c C
!> @param C the single-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE sgemm_ttmnkabbc(transa,transb,m,n,k,a,b,beta,c)
      CHARACTER(LEN=1),INTENT(IN) :: transa
      CHARACTER(LEN=1),INTENT(IN) :: transb
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: k
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: b(:,:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: c(:,:)

      CALL sgemm_all(transa,transb,m,n,k,1.0_SSK,a,m,b,k,beta,c,m)
      
    ENDSUBROUTINE sgemm_ttmnkabbc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::sgemm_all "sgemm_all" for when the user
!>        does not want to supply @c beta, @c lda, @c ldb, and @c ldc.
!> @param transa single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param transb single character input indicating whether or not to use the 
!>        transpose of @c B
!> @param m the size of the first dimension of @c A and of @c C (number of rows)
!> @param n the size of the second dimension of @c B and of @c C (number of columns)
!> @param k the size of the second dimension of @c A (number of columns) 
!>        and the first dimension of @c B (number of rows)
!> @param alpha the single-precision scalar used to scale @c A and @c B
!> @param A the single-precision matrix to multiply with @c B and @c alpha 
!> @param B the single-precision matrix to multiply with @c A and @c alpha 
!> @param C the single-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE sgemm_ttmnkaabc(transa,transb,m,n,k,alpha,a,b,c)
      CHARACTER(LEN=1),INTENT(IN) :: transa
      CHARACTER(LEN=1),INTENT(IN) :: transb
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: k
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: b(:,:)
      REAL(SSK),INTENT(INOUT) :: c(:,:)

      CALL sgemm_all(transa,transb,m,n,k,alpha,a,m,b,k,1.0_SSK,c,m)
      
    ENDSUBROUTINE sgemm_ttmnkaabc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::sgemm_all "sgemm_all" for when the user
!>        does not want to supply @c alpha, @c beta, @c lda, @c ldb, and @c ldc.
!> @param transa single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param transb single character input indicating whether or not to use the 
!>        transpose of @c B
!> @param m the size of the first dimension of @c A and of @c C (number of rows)
!> @param n the size of the second dimension of @c B and of @c C (number of columns)
!> @param k the size of the second dimension of @c A (number of columns) 
!>        and the first dimension of @c B (number of rows)
!> @param A the single-precision matrix to multiply with @c B and @c alpha 
!> @param B the single-precision matrix to multiply with @c A and @c alpha 
!> @param C the single-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE sgemm_ttmnkabc(transa,transb,m,n,k,a,b,c)
      CHARACTER(LEN=1),INTENT(IN) :: transa
      CHARACTER(LEN=1),INTENT(IN) :: transb
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: k
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: b(:,:)
      REAL(SSK),INTENT(INOUT) :: c(:,:)

      CALL sgemm_all(transa,transb,m,n,k,1.0_SSK,a,m,b,k,1.0_SSK,c,m)
      
    ENDSUBROUTINE sgemm_ttmnkabc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::sgemm_all "sgemm_all" for when the user
!>        does not want to supply @c lda, @c ldb, @c ldc, @c transa, and @c transb.
!> @param m the size of the first dimension of @c A and of @c C (number of rows)
!> @param n the size of the second dimension of @c B and of @c C (number of columns)
!> @param k the size of the second dimension of @c A (number of columns) 
!>        and the first dimension of @c B (number of rows)
!> @param alpha the single-precision scalar used to scale @c A and @c B
!> @param A the single-precision matrix to multiply with @c B and @c alpha 
!> @param B the single-precision matrix to multiply with @c A and @c alpha 
!> @param beta the single-precision scalar used to scale @c C
!> @param C the single-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE sgemm_mnkaabbc(m,n,k,alpha,a,b,beta,c)
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: k
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: b(:,:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: c(:,:)

      CALL sgemm_all('n','n',m,n,k,alpha,a,m,b,k,beta,c,1)
      
    ENDSUBROUTINE sgemm_mnkaabbc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::sgemm_all "sgemm_all" for when the user
!>        does not want to supply @c m, @c n, @c k, @c lda, @c ldb, and @c ldc.
!> @param transa single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param transb single character input indicating whether or not to use the 
!>        transpose of @c B
!> @param alpha the single-precision scalar used to scale @c A and @c B
!> @param A the single-precision matrix to multiply with @c B and @c alpha 
!> @param B the single-precision matrix to multiply with @c A and @c alpha 
!> @param beta the single-precision scalar used to scale @c C
!> @param C the single-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE sgemm_ttaabbc(transa,transb,alpha,a,b,beta,c)
      CHARACTER(LEN=1),INTENT(IN) :: transa
      CHARACTER(LEN=1),INTENT(IN) :: transb
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: b(:,:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: c(:,:)
      
      INTEGER(SIK) :: m
      INTEGER(SIK) :: n
      INTEGER(SIK) :: k

      m=SIZE(c,DIM=1)
      n=SIZE(c,DIM=2)
      k=SIZE(b,DIM=1)

      CALL sgemm_all(transa,transb,m,n,k,alpha,a,m,b,k,beta,c,m)
      
    ENDSUBROUTINE sgemm_ttaabbc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::sgemm_all "sgemm_all" for when the user
!>        does not want to supply @c lda, @c ldb, @c ldc, @c m, @c n, @c k, 
!>        @c transa, and @c transb.
!> @param alpha the single-precision scalar used to scale @c A and @c B
!> @param A the single-precision matrix to multiply with @c B and @c alpha 
!> @param B the single-precision matrix to multiply with @c A and @c alpha 
!> @param beta the single-precision scalar used to scale @c C
!> @param C the single-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE sgemm_aabbc(alpha,a,b,beta,c)
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: b(:,:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: c(:,:)
      
      INTEGER(SIK) :: m
      INTEGER(SIK) :: n
      INTEGER(SIK) :: k
      
      m=SIZE(c,DIM=1)
      n=SIZE(c,DIM=2)
      k=SIZE(b,DIM=1)

      CALL sgemm_all('n','n',m,n,k,alpha,a,m,b,k,beta,c,1)
      
    ENDSUBROUTINE sgemm_aabbc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::sgemm_all "sgemm_all" for when the user
!>        does not want to supply @c lda, @c ldb, @c ldc, @c m, @c n, @c k, 
!>        @c transa, and @c transb.
!> @param alpha the single-precision scalar used to scale @c A and @c B
!> @param A the single-precision matrix to multiply with @c B and @c alpha 
!> @param B the single-precision matrix to multiply with @c A and @c alpha 
!> @param beta the single-precision scalar used to scale @c C
!> @param C the single-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE sgemm_abbc(a,b,beta,c)
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: b(:,:)
      REAL(SSK),INTENT(IN) :: beta
      REAL(SSK),INTENT(INOUT) :: c(:,:)
      
      INTEGER(SIK) :: m
      INTEGER(SIK) :: n
      INTEGER(SIK) :: k
      
      m=SIZE(c,DIM=1)
      n=SIZE(c,DIM=2)
      k=SIZE(b,DIM=1)

      CALL sgemm_all('n','n',m,n,k,1.0_SSK,a,m,b,k,beta,c,1)
      
    ENDSUBROUTINE sgemm_abbc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::sgemm_all "sgemm_all" for when the user
!>        does not want to supply @c lda, @c ldb, @c ldc, @c m, @c n, @c k, 
!>        @c beta, @c transa, and @c transb.
!> @param alpha the single-precision scalar used to scale @c A and @c B
!> @param A the single-precision matrix to multiply with @c B and @c alpha 
!> @param B the single-precision matrix to multiply with @c A and @c alpha 
!> @param C the single-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE sgemm_aabc(alpha,a,b,c)
      REAL(SSK),INTENT(IN) :: alpha
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: b(:,:)
      REAL(SSK),INTENT(INOUT) :: c(:,:)
      
      INTEGER(SIK) :: m
      INTEGER(SIK) :: n
      INTEGER(SIK) :: k
      
      m=SIZE(c,DIM=1)
      n=SIZE(c,DIM=2)
      k=SIZE(b,DIM=1)

      CALL sgemm_all('n','n',m,n,k,alpha,a,m,b,k,1.0_SSK,c,1)
      
    ENDSUBROUTINE sgemm_aabc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::sgemm_all "sgemm_all" for when the user
!>        does not want to supply @c lda, @c ldb, @c ldc, @c m, @c n, @c k, 
!>        @c alpha, @c beta, @c transa, and @c transb.
!> @param A the single-precision matrix to multiply with @c B and @c alpha 
!> @param B the single-precision matrix to multiply with @c A and @c alpha 
!> @param C the single-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/sgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE sgemm_abc(a,b,c)
      REAL(SSK),INTENT(IN) :: a(:,:)
      REAL(SSK),INTENT(IN) :: b(:,:)
      REAL(SSK),INTENT(INOUT) :: c(:,:)
      
      INTEGER(SIK) :: m
      INTEGER(SIK) :: n
      INTEGER(SIK) :: k
      
      m=SIZE(c,DIM=1)
      n=SIZE(c,DIM=2)
      k=SIZE(b,DIM=1)

      CALL sgemm_all('n','n',m,n,k,1.0_SSK,a,m,b,k,1.0_SSK,c,1)
      
    ENDSUBROUTINE sgemm_abc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine computes a matrix matrix product for general matricies
!>        A and B.
!> @param transa single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param transb single character input indicating whether or not to use the 
!>        transpose of @c B
!> @param m the size of the first dimension of @c A and of @c C (number of rows)
!> @param n the size of the second dimension of @c B and of @c C (number of columns)
!> @param k the size of the second dimension of @c A (number of columns) 
!>        and the first dimension of @c B (number of rows)
!> @param alpha the double-precision scalar used to scale @c A and @c B
!> @param A the double-precision matrix to multiply with @c B and @c alpha 
!> @param lda the size of the leading (first) dimension of @c A
!> @param B the double-precision matrix to multiply with @c A and @c alpha 
!> @param ldb the size of the leading (first) dimension of @c B
!> @param beta the double-precision scalar used to scale @c C
!> @param C the double-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!> @param ldc the size of the leading (first) dimension of @c C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE dgemm_all(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
      CHARACTER(LEN=1),INTENT(IN) :: transa
      CHARACTER(LEN=1),INTENT(IN) :: transb
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: k
      REAL(SDK),INTENT(IN) :: alpha
      INTEGER(SIK),INTENT(IN) :: lda
      REAL(SDK),INTENT(IN) :: a(lda,*)
      INTEGER(SIK),INTENT(IN) :: ldb
      REAL(SDK),INTENT(IN) :: b(ldb,*)
      REAL(SDK),INTENT(IN) :: beta
      INTEGER(SIK),INTENT(IN) :: ldc
      REAL(SDK),INTENT(INOUT) :: c(ldc,*)
      
#ifdef HAVE_BLAS
      INTERFACE
        PURE SUBROUTINE dgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
          CHARACTER(LEN=1),INTENT(IN) :: transa
          CHARACTER(LEN=1),INTENT(IN) :: transb
          INTEGER,INTENT(IN) :: m
          INTEGER,INTENT(IN) :: n
          INTEGER,INTENT(IN) :: k
          REAL(KIND(0.0d0)),INTENT(IN) :: alpha
          INTEGER,INTENT(IN) :: lda
          REAL(KIND(0.0d0)),INTENT(IN) :: a(lda,*)
          INTEGER,INTENT(IN) :: ldb
          REAL(KIND(0.0d0)),INTENT(IN) :: b(ldb,*)
          REAL(KIND(0.0d0)),INTENT(IN) :: beta
          INTEGER,INTENT(IN) :: ldc
          REAL(KIND(0.0d0)),INTENT(INOUT) :: c(ldc,*)
        ENDSUBROUTINE dgemm
      ENDINTERFACE
      CALL dgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
#else
      LOGICAL(SBK) :: ltransa,ltransb
      INTEGER(SIK) :: i,j,l,akm,bkn,amaxval,bmaxval
      REAL(SDK) :: tmp
      INTRINSIC MAX
      
      !Check for transpose of A
      IF(transa == 'n' .OR. transa == 'N') THEN
        !If not transposed, akm is column number, amaxval is row number
        akm=k
        amaxval=m
        ltransa=.FALSE.
      ELSE
        !If transposed, akm is row number, amaxval is column number
        akm=m
        amaxval=k
        ltransa=.TRUE.
      ENDIF
      !Check for transpose of B
      IF(transb == 'n' .OR. transb == 'N') THEN
        !If not transposed, bkn is column number, bmaxval is row number
        bkn=n
        bmaxval=k
        ltransb=.FALSE.
      ELSE
        !If transposed, bkn is row number, bmaxval is column number
        bkn=k
        bmaxval=n
        ltransb=.TRUE.
      ENDIF
      !Input argument check
      IF(m > 0 .AND. n > 0 .AND. k > 0 .AND. ldc >= MAX(1,m) .AND. &
        lda >= MAX(1,amaxval) .AND. ldb >= MAX(1,bmaxval) .AND. &
          .NOT.(alpha == 0.0_SSK .AND. beta == 1.0_SSK) .AND. &
            (transa == 't' .OR. transa == 'T' .OR. transa == 'c' .OR. transa == 'C' .OR. &
              transa == 'n' .OR. transa == 'N') .AND. (transb == 't' .OR. transb == 'T' & 
                .OR. transb == 'c' .OR. transb == 'C' .OR. transb == 'n' .OR. transb == 'N')) THEN
        
        !Compute beta*C for alpha=0. C := beta*C
        IF(alpha == 0.0_SSK) THEN
          !All entries become zero if alpha and beta are 0.
          IF(beta == 0.0_SSK) THEN
            !Possibly increase the IF branch to check n and m individually,
            !and accelerate individually
            IF(n*m > 10000) THEN
              DO j=1,n
                DO i=1,m
                  c(i,j)=0.0_SSK
                ENDDO
              ENDDO
            ELSE
              c(1:m,1:n)=0.0_SSK
            ENDIF
          ELSE
            IF(n*m > 10000) THEN
              DO j=1,n
                DO i=1,m
                  c(i,j)=beta*c(i,j)
                ENDDO
              ENDDO
            ELSE
              c(1:m,1:n)=beta*c(1:m,1:n)
            ENDIF
          ENDIF
        !Alpha = 1.
        ELSE
          IF(.NOT.ltransa) THEN
            !Operation 1, untransposed. Form  C := alpha*A*B + beta*C.
            IF(.NOT.ltransb) THEN
              DO j=1,n
                IF(beta == 0.0_SSK) THEN
                  DO i=1,m
                    c(i,j)=0.0_SSK
                  ENDDO
                ELSEIF(beta /= 1.0_SSK) THEN
                  DO i=1,m
                    c(i,j)=beta*c(i,j)
                  ENDDO
                ENDIF
                DO l=1,k
                  IF(b(l,j) /= 0.0_SSK) THEN
                    tmp=alpha*b(l,j)
                    DO i=1,m
                      c(i,j)=c(i,j)+tmp*a(i,l)
                    ENDDO
                  ENDIF
                ENDDO
              ENDDO
            !Operation 2, B transposed. Form  C := alpha*A*B^T + beta*C.
            ELSE
              DO j=1,n
                IF(beta == 0.0_SSK) THEN
                  DO i=1,m
                    c(i,j)=0.0_SSK
                  ENDDO
                ELSE
                  DO i=1,m
                    c(i,j)=beta*c(i,j)
                  ENDDO
                ENDIF
                DO l=1,k
                  IF(b(j,l) /= 0.0_SSK) THEN
                    tmp=alpha*b(j,l)
                    DO i=1,m
                      c(i,j)=c(i,j)+tmp*a(i,l)
                    ENDDO
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
          ELSE
            !Operation 3, A transposed. Form  C := alpha*A^T*B + beta*C.
            IF(.NOT.ltransb) THEN
              DO j=1,n
                DO i=1,m
                  tmp=0.0_SSK
                  DO l=1,k
                    tmp=tmp+a(l,i)*b(l,j)
                  ENDDO
                  IF(beta == 0.0_SSK) THEN
                    c(i,j)=alpha*tmp
                  ELSE
                    c(i,j)=alpha*tmp+beta*c(i,j)
                  ENDIF
                ENDDO
              ENDDO
            !Operation 4, A & B transposed. Form  C := alpha*A^T*B^T + beta*C.  
            ELSE
              DO j=1,n
                DO i=1,m
                  tmp=0.0_SSK
                  DO l=1,k
                    tmp=tmp+a(l,i)*b(j,l)
                  ENDDO
                  IF(beta == 0.0_SSK) THEN
                    c(i,j)=alpha*tmp
                  ELSE
                    c(i,j)=alpha*tmp+beta*c(i,j)
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDIF  
#endif
    ENDSUBROUTINE dgemm_all
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::dgemm_all "dgemm_all" for when the user
!>        does not want to supply @c lda, @c ldb, and @c ldc.
!> @param transa single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param transb single character input indicating whether or not to use the 
!>        transpose of @c B
!> @param m the size of the first dimension of @c A and of @c C (number of rows)
!> @param n the size of the second dimension of @c B and of @c C (number of columns)
!> @param k the size of the second dimension of @c A (number of columns) 
!>        and the first dimension of @c B (number of rows)
!> @param alpha the double-precision scalar used to scale @c A and @c B
!> @param A the double-precision matrix to multiply with @c B and @c alpha 
!> @param B the double-precision matrix to multiply with @c A and @c alpha 
!> @param beta the double-precision scalar used to scale @c C
!> @param C the double-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE dgemm_ttmnkaabbc(transa,transb,m,n,k,alpha,a,b,beta,c)
      CHARACTER(LEN=1),INTENT(IN) :: transa
      CHARACTER(LEN=1),INTENT(IN) :: transb
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: k
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: b(:,:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: c(:,:)

      CALL dgemm_all(transa,transb,m,n,k,alpha,a,m,b,k,beta,c,m)
      
    ENDSUBROUTINE dgemm_ttmnkaabbc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::dgemm_all "dgemm_all" for when the user
!>        does not want to supply @c alpha, @c lda, @c ldb, and @c ldc.
!> @param transa double character input indicating whether or not to use the 
!>        transpose of @c A
!> @param transb double character input indicating whether or not to use the 
!>        transpose of @c B
!> @param m the size of the first dimension of @c A and of @c C (number of rows)
!> @param n the size of the second dimension of @c B and of @c C (number of columns)
!> @param k the size of the second dimension of @c A (number of columns) 
!>        and the first dimension of @c B (number of rows)
!> @param A the double-precision matrix to multiply with @c B and @c alpha 
!> @param B the double-precision matrix to multiply with @c A and @c alpha 
!> @param beta the double-precision scalar used to scale @c C
!> @param C the double-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE dgemm_ttmnkabbc(transa,transb,m,n,k,a,b,beta,c)
      CHARACTER(LEN=1),INTENT(IN) :: transa
      CHARACTER(LEN=1),INTENT(IN) :: transb
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: k
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: b(:,:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: c(:,:)

      CALL dgemm_all(transa,transb,m,n,k,1.0_SDK,a,m,b,k,beta,c,m)
      
    ENDSUBROUTINE dgemm_ttmnkabbc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::dgemm_all "dgemm_all" for when the user
!>        does not want to supply @c beta, @c lda, @c ldb, and @c ldc.
!> @param transa double character input indicating whether or not to use the 
!>        transpose of @c A
!> @param transb double character input indicating whether or not to use the 
!>        transpose of @c B
!> @param m the size of the first dimension of @c A and of @c C (number of rows)
!> @param n the size of the second dimension of @c B and of @c C (number of columns)
!> @param k the size of the second dimension of @c A (number of columns) 
!>        and the first dimension of @c B (number of rows)
!> @param alpha the double-precision scalar used to scale @c A and @c B
!> @param A the double-precision matrix to multiply with @c B and @c alpha 
!> @param B the double-precision matrix to multiply with @c A and @c alpha 
!> @param C the double-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE dgemm_ttmnkaabc(transa,transb,m,n,k,alpha,a,b,c)
      CHARACTER(LEN=1),INTENT(IN) :: transa
      CHARACTER(LEN=1),INTENT(IN) :: transb
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: k
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: b(:,:)
      REAL(SDK),INTENT(INOUT) :: c(:,:)

      CALL dgemm_all(transa,transb,m,n,k,alpha,a,m,b,k,1.0_SDK,c,m)
      
    ENDSUBROUTINE dgemm_ttmnkaabc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::dgemm_all "dgemm_all" for when the user
!>        does not want to supply @c alpha, @c beta, @c lda, @c ldb, and @c ldc.
!> @param transa double character input indicating whether or not to use the 
!>        transpose of @c A
!> @param transb double character input indicating whether or not to use the 
!>        transpose of @c B
!> @param m the size of the first dimension of @c A and of @c C (number of rows)
!> @param n the size of the second dimension of @c B and of @c C (number of columns)
!> @param k the size of the second dimension of @c A (number of columns) 
!>        and the first dimension of @c B (number of rows)
!> @param A the double-precision matrix to multiply with @c B and @c alpha 
!> @param B the double-precision matrix to multiply with @c A and @c alpha 
!> @param C the double-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE dgemm_ttmnkabc(transa,transb,m,n,k,a,b,c)
      CHARACTER(LEN=1),INTENT(IN) :: transa
      CHARACTER(LEN=1),INTENT(IN) :: transb
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: k
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: b(:,:)
      REAL(SDK),INTENT(INOUT) :: c(:,:)

      CALL dgemm_all(transa,transb,m,n,k,1.0_SDK,a,m,b,k,1.0_SDK,c,m)
      
    ENDSUBROUTINE dgemm_ttmnkabc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::dgemm_all "dgemm_all" for when the user
!>        does not want to supply @c lda, @c ldb, @c ldc, @c transa, and @c transb.
!> @param m the size of the first dimension of @c A and of @c C (number of rows)
!> @param n the size of the second dimension of @c B and of @c C (number of columns)
!> @param k the size of the second dimension of @c A (number of columns) 
!>        and the first dimension of @c B (number of rows)
!> @param alpha the double-precision scalar used to scale @c A and @c B
!> @param A the double-precision matrix to multiply with @c B and @c alpha 
!> @param B the double-precision matrix to multiply with @c A and @c alpha 
!> @param beta the double-precision scalar used to scale @c C
!> @param C the double-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE dgemm_mnkaabbc(m,n,k,alpha,a,b,beta,c)
      INTEGER(SIK),INTENT(IN) :: m
      INTEGER(SIK),INTENT(IN) :: n
      INTEGER(SIK),INTENT(IN) :: k
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: b(:,:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: c(:,:)

      CALL dgemm_all('n','n',m,n,k,alpha,a,m,b,k,beta,c,1)
      
    ENDSUBROUTINE dgemm_mnkaabbc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::dgemm_all "dgemm_all" for when the user
!>        does not want to supply @c m, @c n, @c k, @c lda, @c ldb, and @c ldc.
!> @param transa single character input indicating whether or not to use the 
!>        transpose of @c A
!> @param transb single character input indicating whether or not to use the 
!>        transpose of @c B
!> @param alpha the double-precision scalar used to scale @c A and @c B
!> @param A the double-precision matrix to multiply with @c B and @c alpha 
!> @param B the double-precision matrix to multiply with @c A and @c alpha 
!> @param beta the double-precision scalar used to scale @c C
!> @param C the double-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE dgemm_ttaabbc(transa,transb,alpha,a,b,beta,c)
      CHARACTER(LEN=1),INTENT(IN) :: transa
      CHARACTER(LEN=1),INTENT(IN) :: transb
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: b(:,:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: c(:,:)
      
      INTEGER(SIK) :: m
      INTEGER(SIK) :: n
      INTEGER(SIK) :: k

      m=SIZE(c,DIM=1)
      n=SIZE(c,DIM=2)
      k=SIZE(b,DIM=1)

      CALL dgemm_all(transa,transb,m,n,k,alpha,a,m,b,k,beta,c,m)
      
    ENDSUBROUTINE dgemm_ttaabbc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::dgemm_all "dgemm_all" for when the user
!>        does not want to supply @c lda, @c ldb, @c ldc, @c m, @c n, @c k, 
!>        @c transa, and @c transb.
!> @param alpha the double-precision scalar used to scale @c A and @c B
!> @param A the double-precision matrix to multiply with @c B and @c alpha 
!> @param B the double-precision matrix to multiply with @c A and @c alpha 
!> @param beta the double-precision scalar used to scale @c C
!> @param C the double-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE dgemm_aabbc(alpha,a,b,beta,c)
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: b(:,:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: c(:,:)
      
      INTEGER(SIK) :: m
      INTEGER(SIK) :: n
      INTEGER(SIK) :: k
      
      m=SIZE(c,DIM=1)
      n=SIZE(c,DIM=2)
      k=SIZE(b,DIM=1)

      CALL dgemm_all('n','n',m,n,k,alpha,a,m,b,k,beta,c,1)
!      
    ENDSUBROUTINE dgemm_aabbc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::dgemm_all "dgemm_all" for when the user
!>        does not want to supply @c lda, @c ldb, @c ldc, @c m, @c n, @c k, 
!>        @c transa, and @c transb.
!> @param alpha the double-precision scalar used to scale @c A and @c B
!> @param A the double-precision matrix to multiply with @c B and @c alpha 
!> @param B the double-precision matrix to multiply with @c A and @c alpha 
!> @param beta the double-precision scalar used to scale @c C
!> @param C the double-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE dgemm_abbc(a,b,beta,c)
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: b(:,:)
      REAL(SDK),INTENT(IN) :: beta
      REAL(SDK),INTENT(INOUT) :: c(:,:)
      
      INTEGER(SIK) :: m
      INTEGER(SIK) :: n
      INTEGER(SIK) :: k
      
      m=SIZE(c,DIM=1)
      n=SIZE(c,DIM=2)
      k=SIZE(b,DIM=1)

      CALL dgemm_all('n','n',m,n,k,1.0_SDK,a,m,b,k,beta,c,1)
      
    ENDSUBROUTINE dgemm_abbc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::dgemm_all "dgemm_all" for when the user
!>        does not want to supply @c lda, @c ldb, @c ldc, @c m, @c n, @c k, 
!>        @c beta, @c transa, and @c transb.
!> @param alpha the double-precision scalar used to scale @c A and @c B
!> @param A the double-precision matrix to multiply with @c B and @c alpha 
!> @param B the double-precision matrix to multiply with @c A and @c alpha 
!> @param C the double-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE dgemm_aabc(alpha,a,b,c)
      REAL(SDK),INTENT(IN) :: alpha
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: b(:,:)
      REAL(SDK),INTENT(INOUT) :: c(:,:)
      
      INTEGER(SIK) :: m
      INTEGER(SIK) :: n
      INTEGER(SIK) :: k
      
      m=SIZE(c,DIM=1)
      n=SIZE(c,DIM=2)
      k=SIZE(b,DIM=1)

      CALL dgemm_all('n','n',m,n,k,alpha,a,m,b,k,1.0_SDK,c,1)
     
    ENDSUBROUTINE dgemm_aabc
!
!-------------------------------------------------------------------------------
!> @brief Subroutine wraps @ref BLAS3::dgemm_all "dgemm_all" for when the user
!>        does not want to supply @c lda, @c ldb, @c ldc, @c m, @c n, @c k, 
!>        @c alpha, @c beta, @c transa, and @c transb.
!> @param A the double-precision matrix to multiply with @c B and @c alpha 
!> @param B the double-precision matrix to multiply with @c A and @c alpha 
!> @param C the double-precision matrix to be overwritten by
!>        C := alpha*op(A)*op(B) + beta*C
!>
!> If an external BLAS library is available at link time then that library
!> routine that gets called, otherwise the supplied code is used. It is based on
!> the code available on http://netlib.org/blas/dgemm.f but has some minor
!> modifications. The error checking is somewhat different.
!>
    PURE SUBROUTINE dgemm_abc(a,b,c)
      REAL(SDK),INTENT(IN) :: a(:,:)
      REAL(SDK),INTENT(IN) :: b(:,:)
      REAL(SDK),INTENT(INOUT) :: c(:,:)
      
      INTEGER(SIK) :: m
      INTEGER(SIK) :: n
      INTEGER(SIK) :: k
      
      m=SIZE(c,DIM=1)
      n=SIZE(c,DIM=2)
      k=SIZE(b,DIM=1)

      CALL dgemm_all('n','n',m,n,k,1.0_SDK,a,m,b,k,1.0_SDK,c,1)
      
    ENDSUBROUTINE dgemm_abc
!
ENDMODULE BLAS3
