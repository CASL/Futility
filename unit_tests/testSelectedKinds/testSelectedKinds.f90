!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testSelectedKinds
#include "UnitTest.h"
USE UnitTest
USE IntrType

IMPLICIT NONE

REAL(SSK) :: singlefloat
REAL(SDK) :: doublefloat
INTEGER(SNK) :: singleint
INTEGER(SLK) :: doubleint

CREATE_TEST('Testing Selected Kinds')

CALL evalIntModel()
CALL evalFPModel()

REGISTER_SUBTEST('APPROXEQ', testAPPROXEQ)
REGISTER_SUBTEST('APPROXEQA', testAPPROXEQA)
REGISTER_SUBTEST('APPROXEQR', testAPPROXEQR)
REGISTER_SUBTEST('APPROXEQF', testAPPROXEQF)
REGISTER_SUBTEST('APPROXLE', testAPPROXLE)
REGISTER_SUBTEST('APPROXGE', testAPPROXGE)
REGISTER_SUBTEST('SOFTEQ', testSOFTEQ)
REGISTER_SUBTEST('SOFT...', testSOFTCompare)
REGISTER_SUBTEST('isNAN', testisNAN)
REGISTER_SUBTEST('isINF', testisINF)
REGISTER_SUBTEST('CharToInt', testCharToInt)
REGISTER_SUBTEST('CharToSingle', testCharToSingle)
REGISTER_SUBTEST('CharToDouble', testCharToDouble)
REGISTER_SUBTEST('CharToBool', testCharToBool)
!REGISTER_SUBTEST('CharToIntArray', testCharToIntArray)
!REGISTER_SUBTEST('CharToDoubleArray', testCharToDoubleArray)
!REGISTER_SUBTEST('CharToStrArray', testCharToStrArray)

FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE evalFPModel()
  INTEGER :: id

  WRITE(*,*) 'TESTING FLOATING POINT MODEL'
  WRITE(*,*)
  WRITE(*,'(a,i1,a)') ' REAL(',SSK,') :: singlefloat'
  WRITE(*,*) '      RADIX(singlefloat) = ',RADIX(singlefloat)
  WRITE(*,*) '     DIGITS(singlefloat) = ',DIGITS(singlefloat)
  WRITE(*,*) 'MINEXPONENT(singlefloat) = ',MINEXPONENT(singlefloat)
  WRITE(*,*) 'MAXEXPONENT(singlefloat) = ',MAXEXPONENT(singlefloat)
  WRITE(*,*) '      RANGE(singlefloat) = ',RANGE(singlefloat)
  WRITE(*,*) '  PRECISION(singlefloat) = ',PRECISION(singlefloat)
  WRITE(*,*) '    EPSILON(singlefloat) = ',EPSILON(singlefloat)
  WRITE(*,*) '       HUGE(singlefloat) = ',HUGE(singlefloat)
  WRITE(*,*) '       TINY(singlefloat) = ',TINY(singlefloat)
  DO id=0,15
    singlefloat=REAL(10**id,SSK)
    WRITE(*,'(a,i2.2,a,es13.6)') '            SPACING(1e',id,') = ',SPACING(singlefloat)
  ENDDO

  WRITE(*,*)
  WRITE(*,'(a,i1,a)') ' REAL(',SDK,') :: doublefloat'
  WRITE(*,*) '      RADIX(doublefloat) = ',RADIX(doublefloat)
  WRITE(*,*) '     DIGITS(doublefloat) = ',DIGITS(doublefloat)
  WRITE(*,*) 'MINEXPONENT(doublefloat) = ',MINEXPONENT(doublefloat)
  WRITE(*,*) 'MAXEXPONENT(doublefloat) = ',MAXEXPONENT(doublefloat)
  WRITE(*,*) '      RANGE(doublefloat) = ',RANGE(doublefloat)
  WRITE(*,*) '  PRECISION(doublefloat) = ',PRECISION(doublefloat)
  WRITE(*,*) '    EPSILON(doublefloat) = ',EPSILON(doublefloat)
  WRITE(*,*) '       HUGE(doublefloat) = ',HUGE(doublefloat)
  WRITE(*,*) '       TINY(doublefloat) = ',TINY(doublefloat)
  DO id=0,15
    doublefloat=REAL(10**id,SDK)
    WRITE(*,'(a,i2.2,a,es13.6)') '            SPACING(1e',id,') = ',SPACING(doublefloat)
  ENDDO
  WRITE(*,*) '---------------------------------------------------'
ENDSUBROUTINE evalFPModel
!
!-------------------------------------------------------------------------------
SUBROUTINE evalIntModel()
  WRITE(*,*) 'TESTING INTEGER MODEL'
  WRITE(*,*)
  WRITE(*,'(a,i1,a)') ' INTEGER(',SNK,') :: singleint'
  WRITE(*,*) '   BIT_SIZE(singleint) = ',BIT_SIZE(singleint)
  WRITE(*,*) '      RADIX(singleint) = ',RADIX(singleint)
  WRITE(*,*) '     DIGITS(singleint) = ',DIGITS(singleint)
  WRITE(*,*) '      RANGE(singleint) = ',RANGE(singleint)
  WRITE(*,*) '       HUGE(singleint) = ',HUGE(singleint)

  WRITE(*,*)
  WRITE(*,'(a,i1,a)') ' INTEGER(',SLK,') :: doubleint'
  WRITE(*,*) '   BIT_SIZE(doubleint) = ',BIT_SIZE(doubleint)
  WRITE(*,*) '      RADIX(doubleint) = ',RADIX(doubleint)
  WRITE(*,*) '     DIGITS(doubleint) = ',DIGITS(doubleint)
  WRITE(*,*) '      RANGE(doubleint) = ',RANGE(doubleint)
  WRITE(*,*) '       HUGE(doubleint) = ',HUGE(doubleint)

  WRITE(*,*) '---------------------------------------------------'
ENDSUBROUTINE evalIntModel
!
!-------------------------------------------------------------------------------
FUNCTION approxeq_test(a,b) RESULT(bool)
  REAL(SDK),INTENT(IN) :: a,b
  LOGICAL(SBK) :: bool
  REAL(SDK) :: eps

  eps=MAX(ABS(a),ABS(b))*1.e-14_SDK
  IF(a == 0.0_SRK .OR. b == 0.0_SRK) eps=1.e-14_SDK

  bool=(ABS(a-b) <= eps)

  !CHARACTER(LEN=23) :: aString,bString
  !CHARACTER(LEN=8) :: expString
  !INTEGER(SNK) :: intA_left,intB_left,intA_exp,intB_exp,diffExp
  !INTEGER(SLK) :: intA_right,intB_right
  !REAL(SDK) :: tol
  !
  !!Convert to character variable
  !WRITE(aString,'(es23.15E3)') a
  !WRITE(bString,'(es23.15E3)') b
  !
  !!First digit that is left of decimal
  !READ(aString(1:2),'(i2)') intA_left
  !READ(bString(1:2),'(i2)') intB_left
  !
  !IF(intA_left == 0 .OR. intB_left == 0) THEN
  !  !Special case for 0, use absolute comparison
  !  bool=(ABS(a-b) <= EPSD)
  !ELSE
  !  !Conver exponent and numbers right of decimal to integers
  !  READ(aString(4:18),'(i15)') intA_right
  !  READ(bString(4:18),'(i15)') intB_right
  !  READ(aString(20:23),'(i4)') intA_exp
  !  READ(bString(20:23),'(i4)') intB_exp
  !  diffExp=ABS(intA_exp-intB_exp)
  !
  !  IF(diffExp < 2) THEN
  !    IF(diffExp == 0 .AND. intA_left == intB_left) THEN
  !      !exponents are the same and left of decimal numbers are same,
  !      !so compare sig figs to the right of the decimal
  !      bool=(ABS(intA_right-intB_right) < 11)
  !    ELSE
  !      !Exponent or number may have rolled over (e.g. 1.0 and 0.9)
  !      !Compute tolerance based on larger exponent
  !      WRITE(expString,'(a,sp,i4.3)') '1.0d',(MAX(intA_exp,intB_exp)-14)
  !      READ(expString,*) tol
  !      bool=(ABS(a-b) <= tol)
  !    ENDIF
  !  ELSE
  !    bool=.FALSE.
  !  ENDIF
  !ENDIF
ENDFUNCTION approxeq_test
!
!-------------------------------------------------------------------------------
SUBROUTINE testAPPROXEQ()
  LOGICAL(SBK) :: bool

  COMPONENT_TEST('.APPROXEQ. (SINGLE PRECISION)')
  bool=.NOT.(1.234560_SSK .APPROXEQ. 1.234571_SSK)
  ASSERT(bool, '1.234560_SSK .APPROXEQ. 1.234571_SSK')
  bool=1.234560_SSK .APPROXEQ. 1.234569_SSK
  ASSERT(bool, '1.234560_SSK .APPROXEQ. 1.234570_SSK')
  bool=.NOT.(1.234560e10_SSK .APPROXEQ. 1.234575e10_SSK)
  ASSERT(bool, '1.234560e10_SSK .APPROXEQ. 1.234575e10_SSK')
  bool=.NOT.(1.234560e10_SSK .APPROXEQ. 1.234570e10_SSK)
  ASSERT(bool, '1.234560e10_SSK .APPROXEQ. 1.234570e10_SSK')
  bool=1.234560e-10_SSK .APPROXEQ. 1.234575e-10_SSK
  ASSERT(bool, '1.234560e-10_SSK .APPROXEQ. 1.234575e-10_SSK')
  bool=1.234560e-10_SSK .APPROXEQ. 1.234570e-10_SSK
  ASSERT(bool, '1.234560e-10_SSK .APPROXEQ. 1.234570e-10_SSK')
  bool=0.8_SSK .APPROXEQ. 0.79999999_SSK
  ASSERT(bool, '0.8_SSK .APPROXEQ. 0.79999999_SSK')
  bool=.NOT.(1.0e20_SSK .APPROXEQ. 0.9999999e20_SSK)
  ASSERT(bool, '1.0e20_SSK .APPROXEQ. 0.9999999e20_SSK')
  bool=0.0_SSK .APPROXEQ. 1.2345678e-18_SSK
  ASSERT(bool, '0.0_SSK .APPROXEQ. 1.2345678e-18_SSK')
  bool=.NOT.(1.0_SSK .APPROXEQ. -1.0_SSK)
  ASSERT(bool, '1.0_SSK .APPROXEQ. -1.0_SSK')
  bool=.NOT.(1.0e10_SSK .APPROXEQ. 1.0e-10_SSK)
  ASSERT(bool, '1.0e10_SSK .APPROXEQ. 1.0e-10_SSK')

  COMPONENT_TEST('.APPROXEQ. (DOUBLE PRECISION)')
  bool=.NOT.(1.234567890123450_SDK .APPROXEQ. 1.234567890123465_SDK)
  ASSERT(bool, '1.234567890123450_SDK .APPROXEQ. 1.234567890123465_SDK')
  bool=1.234567890123450_SDK .APPROXEQ. 1.234567890123460_SDK
  ASSERT(bool, '1.234567890123450_SDK .APPROXEQ. 1.234567890123460_SDK')
  bool=.NOT.(1.234567890123450e10_SDK .APPROXEQ. 1.234567890123465e10_SDK)
  ASSERT(bool, '1.234567890123450e10_SDK .APPROXEQ. 1.234567890123465e10_SDK')
  bool=.NOT.(1.234567890123450e10_SDK .APPROXEQ. 1.234567890123460e10_SDK)
  ASSERT(bool, '1.234567890123450e10_SDK .APPROXEQ. 1.234567890123460e10_SDK')
  bool=1.234567890123450e-10_SDK .APPROXEQ. 1.234567890123465e-10_SDK
  ASSERT(bool, '1.234567890123450e-10_SDK .APPROXEQ. 1.234567890123465e-10_SDK')
  bool=1.234567890123450e-10_SDK .APPROXEQ. 1.234567890123460e-10_SDK
  ASSERT(bool, '1.234567890123450e-10_SDK .APPROXEQ. 1.234567890123460e-10_SDK')
  bool=0.8_SDK .APPROXEQ. 0.7999999999999999_SDK
  ASSERT(bool, '0.8_SDK .APPROXEQ. 0.7999999999999999_SDK')
  bool=.NOT.(1.0e20_SDK .APPROXEQ. 0.9999999999999999e20_SDK)
  ASSERT(bool, '1.0e20_SDK .APPROXEQ. 0.9999999999999999e20_SDK')
  bool=0.0_SDK .APPROXEQ. 1.234567890123450e-18_SDK
  ASSERT(bool, '0.0_SDK .APPROXEQ. 1.234567890123450e-18_SDK')
  bool=.NOT.(1.0_SDK .APPROXEQ. -1.0_SDK)
  ASSERT(bool, '1.0_SDK .APPROXEQ. -1.0_SDK')
  bool=.NOT.(1.0e10_SDK .APPROXEQ. 1.0e-10_SDK)
  ASSERT(bool, '1.0e10_SDK .APPROXEQ. 1.0e-10_SDK')
ENDSUBROUTINE testAPPROXEQ
!
!-------------------------------------------------------------------------------
SUBROUTINE testAPPROXEQA()
  LOGICAL(SBK) :: bool

  COMPONENT_TEST('.APPROXEQA. (SINGLE PRECISION)')
  bool=.NOT.(1.234560_SSK .APPROXEQA. 1.234571_SSK)
  ASSERT(bool, '1.234560_SSK .APPROXEQA. 1.234571_SSK')
  bool=1.234560_SSK .APPROXEQA. 1.234569_SSK
  ASSERT(bool, '1.234560_SSK .APPROXEQA. 1.234570_SSK')
  bool=.NOT.(1.234560e10_SSK .APPROXEQA. 1.234575e10_SSK)
  ASSERT(bool, '1.234560e10_SSK .APPROXEQA. 1.234575e10_SSK')
  bool=.NOT.(1.234560e10_SSK .APPROXEQA. 1.234570e10_SSK)
  ASSERT(bool, '1.234560e10_SSK .APPROXEQA. 1.234570e10_SSK')
  bool=1.234560e-10_SSK .APPROXEQA. 1.234575e-10_SSK
  ASSERT(bool, '1.234560e-10_SSK .APPROXEQA. 1.234575e-10_SSK')
  bool=1.234560e-10_SSK .APPROXEQA. 1.234570e-10_SSK
  ASSERT(bool, '1.234560e-10_SSK .APPROXEQA. 1.234570e-10_SSK')
  bool=0.8_SSK .APPROXEQA. 0.79999999_SSK
  ASSERT(bool, '0.8_SSK .APPROXEQA. 0.79999999_SSK')
  bool=.NOT.(1.0e20_SSK .APPROXEQA. 0.9999999e20_SSK)
  ASSERT(bool, '1.0e20_SSK .APPROXEQA. 0.9999999e20_SSK')
  bool=0.0_SSK .APPROXEQA. 1.2345678e-18_SSK
  ASSERT(bool, '0.0_SSK .APPROXEQA. 1.2345678e-18_SSK')
  bool=.NOT.(1.0_SSK .APPROXEQA. -1.0_SSK)
  ASSERT(bool, '1.0_SSK .APPROXEQA. -1.0_SSK')
  bool=.NOT.(1.0e10_SSK .APPROXEQA. 1.0e-10_SSK)
  ASSERT(bool, '1.0e10_SSK .APPROXEQA. 1.0e-10_SSK')

  COMPONENT_TEST('.APPROXEQA. (DOUBLE PRECISION)')
  bool=.NOT.(1.234567890123450_SDK .APPROXEQA. 1.234567890123465_SDK)
  ASSERT(bool, '1.234567890123450_SDK .APPROXEQA. 1.234567890123465_SDK')
  bool=1.234567890123450_SDK .APPROXEQA. 1.234567890123460_SDK
  ASSERT(bool, '1.234567890123450_SDK .APPROXEQA. 1.234567890123460_SDK')
  bool=.NOT.(1.234567890123450e10_SDK .APPROXEQA. 1.234567890123465e10_SDK)
  ASSERT(bool, '1.234567890123450e10_SDK .APPROXEQA. 1.234567890123465e10_SDK')
  bool=.NOT.(1.234567890123450e10_SDK .APPROXEQA. 1.234567890123460e10_SDK)
  ASSERT(bool, '1.234567890123450e10_SDK .APPROXEQA. 1.234567890123460e10_SDK')
  bool=1.234567890123450e-10_SDK .APPROXEQA. 1.234567890123465e-10_SDK
  ASSERT(bool, '1.234567890123450e-10_SDK .APPROXEQA. 1.234567890123465e-10_SDK')
  bool=1.234567890123450e-10_SDK .APPROXEQA. 1.234567890123460e-10_SDK
  ASSERT(bool, '1.234567890123450e-10_SDK .APPROXEQA. 1.234567890123460e-10_SDK')
  bool=0.8_SDK .APPROXEQA. 0.7999999999999999_SDK
  ASSERT(bool, '0.8_SDK .APPROXEQA. 0.7999999999999999_SDK')
  bool=.NOT.(1.0e20_SDK .APPROXEQA. 0.9999999999999999e20_SDK)
  ASSERT(bool, '1.0e20_SDK .APPROXEQA. 0.9999999999999999e20_SDK')
  bool=0.0_SDK .APPROXEQA. 1.234567890123450e-18_SDK
  ASSERT(bool, '0.0_SDK .APPROXEQA. 1.234567890123450e-18_SDK')
  bool=.NOT.(1.0_SDK .APPROXEQA. -1.0_SDK)
  ASSERT(bool, '1.0_SDK .APPROXEQA. -1.0_SDK')
  bool=.NOT.(1.0e10_SDK .APPROXEQA. 1.0e-10_SDK)
  ASSERT(bool, '1.0e10_SDK .APPROXEQA. 1.0e-10_SDK')
ENDSUBROUTINE testAPPROXEQA
!
!-------------------------------------------------------------------------------
SUBROUTINE testAPPROXEQR()
  LOGICAL(SBK) :: bool

  COMPONENT_TEST('.APPROXEQR. (SINGLE PRECISION)')
  bool=.NOT.(1.234560_SSK .APPROXEQR. 1.234575_SSK)
  ASSERT(bool, '1.234560_SSK .APPROXEQR. 1.234575_SSK')
  bool=1.234560_SSK .APPROXEQR. 1.234570_SSK
  ASSERT(bool, '1.234560_SSK .APPROXEQR. 1.234570_SSK')
  bool=.NOT.(1.234560e10_SSK .APPROXEQR. 1.234575e10_SSK)
  ASSERT(bool, '1.234560e10_SSK .APPROXEQR. 1.234575e10_SSK')
  bool=1.234560e10_SSK .APPROXEQR. 1.234570e10_SSK
  ASSERT(bool, '1.234560e10_SSK .APPROXEQR. 1.234570e10_SSK')
  bool=.NOT.(1.234560e-10_SSK .APPROXEQR. 1.234575e-10_SSK)
  ASSERT(bool, '1.234560e-10_SSK .APPROXEQR. 1.234575e-10_SSK')
  bool=1.234560e-10_SSK .APPROXEQR. 1.234570e-10_SSK
  ASSERT(bool, '1.234560e-10_SSK .APPROXEQR. 1.234570e-10_SSK')
  bool=0.8_SSK .APPROXEQR. 0.79999999_SSK
  ASSERT(bool, '0.8_SSK .APPROXEQR. 0.79999999_SSK')
  bool=1.0e20_SSK .APPROXEQR. 0.9999999e20_SSK
  ASSERT(bool, '1.0e20_SSK .APPROXEQR. 0.9999999e20_SSK')
  bool=0.0_SSK .APPROXEQR. 1.2345678e-18_SSK
  ASSERT(bool, '0.0_SSK .APPROXEQR. 1.2345678e-18_SSK')
  bool=.NOT.(1.0_SSK .APPROXEQR. -1.0_SSK)
  ASSERT(bool, '1.0_SSK .APPROXEQR. -1.0_SSK')
  bool=.NOT.(1.0e10_SSK .APPROXEQR. 1.0e-10_SSK)
  ASSERT(bool, '1.0e10_SSK .APPROXEQR. 1.0e-10_SSK')

  COMPONENT_TEST('.APPROXEQR. (DOUBLE PRECISION)')
  bool=.NOT.(1.234567890123450_SDK .APPROXEQR. 1.234567890123465_SDK)
  ASSERT(bool, '1.234567890123450_SDK .APPROXEQR. 1.234567890123465_SDK')
  bool=1.234567890123450_SDK .APPROXEQR. 1.234567890123460_SDK
  ASSERT(bool, '1.234567890123450_SDK .APPROXEQR. 1.234567890123460_SDK')
  bool=.NOT.(1.234567890123450e10_SDK .APPROXEQR. 1.234567890123465e10_SDK)
  ASSERT(bool, '1.234567890123450e10_SDK .APPROXEQR. 1.234567890123465e10_SDK')
  bool=1.234567890123450e10_SDK .APPROXEQR. 1.234567890123460e10_SDK
  ASSERT(bool, '1.234567890123450e10_SDK .APPROXEQR. 1.234567890123460e10_SDK')
  bool=.NOT.(1.234567890123450e-10_SDK .APPROXEQR. 1.234567890123465e-10_SDK)
  ASSERT(bool, '1.234567890123450e-10_SDK .APPROXEQR. 1.234567890123465e-10_SDK')
  bool=1.234567890123450e-10_SDK .APPROXEQR. 1.234567890123460e-10_SDK
  ASSERT(bool, '1.234567890123450e-10_SDK .APPROXEQR. 1.234567890123460e-10_SDK')
  bool=0.8_SDK .APPROXEQR. 0.7999999999999999_SDK
  ASSERT(bool, '0.8_SDK .APPROXEQR. 0.7999999999999999_SDK')
  bool=1.0e20_SDK .APPROXEQR. 0.9999999999999999e20_SDK
  ASSERT(bool, '1.0e20_SDK .APPROXEQR. 0.9999999999999999e20_SDK')
  bool=0.0_SDK .APPROXEQR. 1.234567890123450e-18_SDK
  ASSERT(bool, '0.0_SDK .APPROXEQR. 1.234567890123450e-18_SDK')
  bool=.NOT.(1.0_SDK .APPROXEQR. -1.0_SDK)
  ASSERT(bool, '1.0_SDK .APPROXEQR. -1.0_SDK')
  bool=.NOT.(1.0e10_SDK .APPROXEQR. 1.0e-10_SDK)
  ASSERT(bool, '1.0e10_SDK .APPROXEQR. 1.0e-10_SDK')
ENDSUBROUTINE testAPPROXEQR
!
!-------------------------------------------------------------------------------
SUBROUTINE testAPPROXEQF()
  LOGICAL(SBK) :: bool
  COMPONENT_TEST('.APPROXEQF. (SINGLE PRECISION)')
  bool=.NOT.(1.234560_SSK .APPROXEQF. 1.234575_SSK)
  ASSERT(bool, '1.234560_SSK .APPROXEQF. 1.234575_SSK')
  bool=1.234560_SSK .APPROXEQF. 1.234561_SSK
  ASSERT(bool, '1.234560_SSK .APPROXEQF. 1.234561_SSK')
  bool=.NOT.(1.234560e10_SSK .APPROXEQF. 1.234575e10_SSK)
  ASSERT(bool, '1.234560e10_SSK .APPROXEQF. 1.234575e10_SSK')
  bool=1.234560e10_SSK .APPROXEQF. 1.234561e10_SSK
  ASSERT(bool, '1.234560e10_SSK .APPROXEQF. 1.234561e10_SSK')
  bool=.NOT.(1.234560e-10_SSK .APPROXEQF. 1.234575e-10_SSK)
  ASSERT(bool, '1.234560e-10_SSK .APPROXEQF. 1.234575e-10_SSK')
  bool=1.234560e-10_SSK .APPROXEQF. 1.234561e-10_SSK
  ASSERT(bool, '1.234560e-10_SSK .APPROXEQF. 1.234561e-10_SSK')
  bool=0.8_SSK .APPROXEQF. 0.79999999_SSK
  ASSERT(bool, '0.8_SSK .APPROXEQF. 0.79999999_SSK')
  bool=1.0e20_SSK .APPROXEQF. 0.9999999e20_SSK
  ASSERT(bool, '1.0e20_SSK .APPROXEQF. 0.9999999e20_SSK')
  bool=0.0_SSK .APPROXEQF. 1.2345678e-18_SSK
  ASSERT(bool, '0.0_SSK .APPROXEQF. 1.2345678e-18_SSK')
  bool=.NOT.(1.0_SSK .APPROXEQF. -1.0_SSK)
  ASSERT(bool, '1.0_SSK .APPROXEQF. -1.0_SSK')
  bool=.NOT.(1.0e10_SSK .APPROXEQF. 1.0e-10_SSK)
  ASSERT(bool, '1.0e10_SSK .APPROXEQF. 1.0e-10_SSK')

  COMPONENT_TEST('.APPROXEQF. (DOUBLE PRECISION)')
  bool=.NOT.(1.234567890123450_SDK .APPROXEQF. 1.234567890123465_SDK)
  ASSERT(bool, '1.234567890123450_SDK .APPROXEQF. 1.234567890123465_SDK')
  bool=1.234567890123450_SDK .APPROXEQF. 1.234567890123452_SDK
  ASSERT(bool, '1.234567890123450_SDK .APPROXEQF. 1.234567890123452_SDK')
  bool=.NOT.(1.234567890123450e10_SDK .APPROXEQF. 1.234567890123465e10_SDK)
  ASSERT(bool, '1.234567890123450e10_SDK .APPROXEQF. 1.234567890123465e10_SDK')
  bool=1.234567890123450e10_SDK .APPROXEQF. 1.234567890123452e10_SDK
  ASSERT(bool, '1.234567890123450e10_SDK .APPROXEQF. 1.234567890123452e10_SDK')
  bool=.NOT.(1.234567890123450e-10_SDK .APPROXEQF. 1.234567890123465e-10_SDK)
  ASSERT(bool, '1.234567890123450e-10_SDK .APPROXEQF. 1.234567890123465e-10_SDK')
  bool=1.234567890123450e-10_SDK .APPROXEQF. 1.234567890123452e-10_SDK
  ASSERT(bool, '1.234567890123450e-10_SDK .APPROXEQF. 1.234567890123452e-10_SDK')
  bool=0.8_SDK .APPROXEQF. 0.7999999999999999_SDK
  ASSERT(bool, '0.8_SDK .APPROXEQF. 0.7999999999999999_SDK')
  bool=1.0e20_SDK .APPROXEQF. 0.9999999999999999e20_SDK
  ASSERT(bool, '1.0e20_SDK .APPROXEQF. 0.9999999999999999e20_SDK')
  bool=0.0_SDK .APPROXEQF. 1.234567890123450e-18_SDK
  ASSERT(bool, '0.0_SDK .APPROXEQF. 1.234567890123450e-18_SDK')
  bool=.NOT.(1.0_SDK .APPROXEQF. -1.0_SDK)
  ASSERT(bool, '1.0_SDK .APPROXEQF. -1.0_SDK')
  bool=.NOT.(1.0e10_SDK .APPROXEQF. 1.0e-10_SDK)
  ASSERT(bool, '1.0e10_SDK .APPROXEQF. 1.0e-10_SDK')
ENDSUBROUTINE testAPPROXEQF
!
!-------------------------------------------------------------------------------
SUBROUTINE testAPPROXLE()
  LOGICAL(SBK) :: bool
  COMPONENT_TEST('.APPROXLE. (DOUBLE PRECISION)')
  bool=.NOT.(.NOT.(1.00000000000001_SDK .APPROXLE. 1._SDK) .OR. &
             .NOT.(1._SDK               .APPROXLE. 3._SDK) .OR. &
                  (1.000000000000011_SDK .APPROXLE. 1._SDK) .OR. &
                  (3._SDK                .APPROXLE. 1._SDK))
  ASSERT(bool, '.APPROXLE. (DOUBLE PRECISION)')
  COMPONENT_TEST('.APPROXLE. (SINGLE PRECISION)')
  bool=.NOT.(.NOT.(1.000009_SSK .APPROXLE. 1._SSK) .OR. &
             .NOT.(1._SSK       .APPROXLE. 3._SSK) .OR. &
                  (1.000011_SSK .APPROXLE. 1._SSK) .OR. &
                  (3._SSK       .APPROXLE. 1._SSK))
  ASSERT(bool, '.APPROXLE. (SINGLE PRECISION)')
ENDSUBROUTINE testAPPROXLE
!
!-------------------------------------------------------------------------------
SUBROUTINE testAPPROXGE()
  LOGICAL(SBK) :: bool
  COMPONENT_TEST('.APPROXGE. (DOUBLE PRECISION)')
  bool=.NOT.(.NOT.(1._SDK .APPROXGE. 1.00000000000001_SDK) .OR. &
             .NOT.(3._SDK .APPROXGE.               1._SDK) .OR. &
                  (1._SDK .APPROXGE. 1.000000000000011_SDK) .OR. &
                  (1._SDK .APPROXGE.               3._SDK))
  ASSERT(bool, '.APPROXGE. (DOUBLE PRECISION)')
  COMPONENT_TEST('.APPROXGE. (SINGLE PRECISION)')
  bool=.NOT.(.NOT.(1._SSK .APPROXGE. 1.000009_SSK) .OR. &
             .NOT.(3._SSK .APPROXGE.       1._SSK) .OR. &
                  (1._SSK .APPROXGE. 1.000011_SSK) .OR. &
                  (1._SSK .APPROXGE.        3._SSK))
  ASSERT(bool, '.APPROXGE. (SINGLE PRECISION)')
ENDSUBROUTINE testAPPROXGE
!
!-------------------------------------------------------------------------------
SUBROUTINE testSOFTEQ()
  LOGICAL(SBK) :: bool
  COMPONENT_TEST('.SOFTEQ. (DOUBLE PRECISION)')
  bool=.NOT.(.NOT.(SOFTEQ(1.0000001_SDK,1._SDK,1.0E-6_SDK)) .OR. &
                  (SOFTEQ(1.0000011_SDK,1._SDK,1.0E-6_SDK)))
  ASSERT(bool, 'SOFTEQ(...) (DOUBLE PRECISION)')
  COMPONENT_TEST('.SOFTEQ. (SINGLE PRECISION)')
  bool=.NOT.(.NOT.(SOFTEQ(1.00001_SSK,1._SSK,1.0E-4_SSK)) .OR. &
                  (SOFTEQ(1.00011_SSK,1._SSK,1.0E-4_SSK)))
  ASSERT(bool, 'SOFTEQ(...) (SINGLE PRECISION)')
ENDSUBROUTINE testSOFTEQ
!
!-------------------------------------------------------------------------------
SUBROUTINE testSOFTCompare()
  LOGICAL(SBK) :: bool
  COMPONENT_TEST('SOFTLT (DOUBLE PRECISION)')
  bool = SOFTLT(1.0_SDK,1.11_SDK,0.1_SDK)
  ASSERT(bool, "is less than")
  bool = .NOT.SOFTLT(1.0_SDK,1.09_SDK,0.1_SDK)
  ASSERT(bool, "not less than")

  COMPONENT_TEST('SOFTLT (SINGLE PRECISION)')
  bool = SOFTLT(1.0_SSK,1.11_SSK,0.1_SSK)
  ASSERT(bool, "is less than")
  bool = .NOT.SOFTLT(1.0_SSK,1.09_SSK,0.1_SSK)
  ASSERT(bool, "not less than")

  COMPONENT_TEST('SOFTLE (DOUBLE PRECISION)')
  bool = SOFTLE(1.09_SDK,1.0_SDK,0.1_SDK)
  ASSERT(bool, "is less than/equal to")
  bool = .NOT.SOFTLT(1.11_SDK,1.0_SDK,0.1_SDK)
  ASSERT(bool, "too big")

  COMPONENT_TEST('SOFTLE (SINGLE PRECISION)')
  bool = SOFTLE(1.09_SSK,1.0_SSK,0.1_SSK)
  ASSERT(bool, "is less than/equal to")
  bool = .NOT.SOFTLT(1.11_SSK,1.0_SSK,0.1_SSK)
  ASSERT(bool, "too big")

  COMPONENT_TEST('SOFTGT (DOUBLE PRECISION)')
  bool = SOFTGT(1.11_SDK,1.0_SDK,0.1_SDK)
  ASSERT(bool, "is greater than")
  bool = .NOT.SOFTGT(1.09_SDK,1.0_SDK,0.1_SDK)
  ASSERT(bool, "too small")

  COMPONENT_TEST('SOFTGT (SINGLE PRECISION)')
  bool = SOFTGT(1.11_SSK,1.0_SSK,0.1_SSK)
  ASSERT(bool, "is greater than")
  bool = .NOT.SOFTGT(1.09_SSK,1.0_SSK,0.1_SSK)
  ASSERT(bool, "too small")

  COMPONENT_TEST('SOFTGE (DOUBLE PRECISION)')
  bool = SOFTGE(0.91_SDK,1.0_SDK,0.1_SDK)
  ASSERT(bool, "is greater than")
  bool = .NOT.SOFTGE(0.89_SDK,1.0_SDK,0.1_SDK)
  ASSERT(bool, "too small")

  COMPONENT_TEST('SOFTGE (SINGLE PRECISION)')
  bool = SOFTGE(0.91_SSK,1.0_SSK,0.1_SSK)
  ASSERT(bool, "is greater than")
  bool = .NOT.SOFTGE(0.89_SSK,1.0_SSK,0.1_SSK)
  ASSERT(bool, "too small")
ENDSUBROUTINE testSOFTCompare
!
!-------------------------------------------------------------------------------
SUBROUTINE testisNAN()
  LOGICAL(SBK) :: bool
  COMPONENT_TEST('isNAN (DOUBLE PRECISION)')
  doublefloat=-1.0_SDK
  bool=.NOT.(.NOT.(isNAN(REAL(SQRT(doublefloat),SDK))) .OR. &
                  (isNAN(REAL(SQRT(1.0_SDK),SDK))))
  ASSERT(bool, 'isNAN(...) (DOUBLE PRECISION)')
  COMPONENT_TEST('isNAN (SINGLE PRECISION)')
  singlefloat=-1.0_SSK
  bool=.NOT.(.NOT.(isNAN(REAL(SQRT(singlefloat),SSK))) .OR. &
                  (isNAN(REAL(SQRT(1.0_SSK),SSK))))
  ASSERT(bool, 'isNAN(...) (SINGLE PRECISION)')
ENDSUBROUTINE testisNAN
!
!-------------------------------------------------------------------------------
SUBROUTINE testisINF()
  LOGICAL(SBK) :: bool
  COMPONENT_TEST('isInf (DOUBLE PRECISION)')
  doublefloat=0.0_SDK
  bool=.NOT.(.NOT.(isINF(REAL(1.0_SDK/doublefloat,SDK))) .OR. &
                  (isINF(REAL(1.0_SDK,SDK))))
  ASSERT(bool, 'isINF(...) (DOUBLE PRECISION)')
  COMPONENT_TEST('isInf (SINGLE PRECISION)')
  singlefloat=0.0_SDK
  bool=.NOT.(.NOT.(isINF(REAL(1.0_SSK/singlefloat,SSK))) .OR. &
                  (isINF(REAL(1.0_SSK,SSK))))
  ASSERT(bool, 'isINF(...) (SINGLE PRECISION)')
ENDSUBROUTINE testisInf
!
!-------------------------------------------------------------------------------
SUBROUTINE testCharToInt()
  CHARACTER(LEN=*),PARAMETER :: c1='1234567890'
  CHARACTER(LEN=*),PARAMETER :: c2='-1234567890'
  CHARACTER(LEN=*),PARAMETER :: c3='0'
  CHARACTER(LEN=*),PARAMETER :: c4='-0'
  CHARACTER(LEN=*),PARAMETER :: c5='2147483647'
  CHARACTER(LEN=*),PARAMETER :: c6='-2147483647'
  INTEGER(SIK) :: intVal

  COMPONENT_TEST('Char to integer assignment')
  intVal=c1
  ASSERT(intVal == 1234567890, 'char to int assignment')
  intVal=c2
  ASSERT(intVal == -1234567890, 'char to int assignment')
  intVal=c3
  ASSERT(intVal == 0, 'char to int assignment, 0')
  intVal=c4
  ASSERT(intVal == 0, 'char to int assignment, -0')
  intVal=c5
  ASSERT(intVal == 2147483647, 'char to int assignment, maximum SIK value')
  intVal=c6
  ASSERT(intVal == -2147483647, 'char to int assignment, minimum SIK value')
ENDSUBROUTINE testCharToInt
!
!-------------------------------------------------------------------------------
SUBROUTINE testCharToSingle()
  CHARACTER(LEN=*),PARAMETER :: s1='1.1'
  CHARACTER(LEN=*),PARAMETER :: s2='-734723741.00003213'
  CHARACTER(LEN=*),PARAMETER :: s3='0.0000100'
  CHARACTER(LEN=*),PARAMETER :: s4='.5'
  CHARACTER(LEN=*),PARAMETER :: s5='2147483647.2147483647'
  CHARACTER(LEN=*),PARAMETER :: s6='1.4363e06'
  CHARACTER(LEN=*),PARAMETER :: s7='13612.59723'
  CHARACTER(LEN=*),PARAMETER :: s8='1234'
  CHARACTER(LEN=*),PARAMETER :: s9='1e-5'
  REAL(SSK) :: realVal

  COMPONENT_TEST('Char to single precision real')
  realVal=s1
  ASSERT(realVal .APPROXEQF. 1.1, 'Char to single precision real assignment')
  realVal=s2
  ASSERT(realVal .APPROXEQF. -734723741.000033, 'Char to single precision real assignment')
  realVal=s3
  ASSERT(realVal .APPROXEQF. 0., 'Char to single precision real assignment')
  realVal=s4
  ASSERT(realVal .APPROXEQF. .5, 'Char to single precision real assignment')
  realVal=s5
  ASSERT(realVal .APPROXEQF. 2147483647.3147483647, 'Char to single precision real assignment')
  realVal=s6
  ASSERT(realVal .APPROXEQF. 1.4363e06, 'Char to single precision real assignment, scientific')
  realVal=s7
  ASSERT(realVal .APPROXEQF. 13612.59723, 'Char to single precision real assignment')
  realVal=s8
  ASSERT(realVal == 1234, 'Char to single precision real assignment, integer value')
  realVal=s9
  ASSERT(realVal .APPROXEQF. 0.00001, 'Char to single precision real assignment, scientfic no decimal')
ENDSUBROUTINE testCharToSingle
!
!-------------------------------------------------------------------------------
SUBROUTINE testCharToDouble()
  CHARACTER(LEN=*),PARAMETER :: d1='1.1'
  CHARACTER(LEN=*),PARAMETER :: d2='-734723741.000033'
  CHARACTER(LEN=*),PARAMETER :: d3='0.0000100'
  CHARACTER(LEN=*),PARAMETER :: d4='.5'
  CHARACTER(LEN=*),PARAMETER :: d5='2147483647.2147483647'
  CHARACTER(LEN=*),PARAMETER :: d6='1.4363e06'
  CHARACTER(LEN=*),PARAMETER :: d8='11.000000000000100'
  CHARACTER(LEN=*),PARAMETER :: d9='1234'
  CHARACTER(LEN=*),PARAMETER :: d10='1e-5'
  REAL(SDK) :: doubleVal

  COMPONENT_TEST('Char to double precision real')
  doubleVal=d1
  ASSERT(doubleVal .APPROXEQF. 1.1_SDK, 'Char to double precision real assignment')
  doubleVal=d2
  ASSERT(doubleVal .APPROXEQF. -734723741.000033_SDK, 'Char to double precision real assignment')
  FINFO() doubleVal, d2
  doubleVal=d3
  ASSERT(doubleVal .APPROXEQF. 0.0000100_SDK, 'Char to double precision real assignment')
  doubleVal=d4
  ASSERT(doubleVal .APPROXEQF. .5_SDK, 'Char to double precision real assignment')
  doubleVal=d5
  ASSERT(doubleVal .APPROXEQF. 2147483647.2147483647_SDK, 'Char to double precision real assignment')
  FINFO() doubleVal, d5
  doubleVal=d6
  ASSERT(doubleVal .APPROXEQF. 1.4363e06_SDK, 'Char to double precision real assignment')
  doubleVal=d8
  ASSERT(doubleVal .APPROXEQF. 11.000000000000100_SDK, 'Char to double precision real assignment')
  FINFO() doubleVal, d8
  doubleVal=d9
  ASSERT(doubleVal == 1234, 'Char to double precision real assignment, integer value')
  doubleVal=d10
  ASSERT(doubleVal .APPROXEQF. 0.00001_SDK, 'Char to double precision value, scientific no decimal')
ENDSUBROUTINE testCharToDouble
!
!-------------------------------------------------------------------------------
SUBROUTINE testCharToBool()
  LOGICAL(SBK) :: bool
  CHARACTER(LEN=*),PARAMETER :: b1='true'
  CHARACTER(LEN=*),PARAMETER :: b2='false'

  COMPONENT_TEST('Char to boolean assignment')
  bool=b1
  ASSERT(bool == .TRUE., 'char to bool assignment, true')
  bool=b2
  ASSERT(bool == .FALSE., 'char to bool assignment, false')
ENDSUBROUTINE testCharToBool
!
!-------------------------------------------------------------------------------
!SUBROUTINE testCharToIntArray()
!  INTEGER(SIK),ALLOCATABLE :: intArray(:)
!  CHARACTER(LEN=*),PARAMETER :: ia1='{1,2,3,4,5,6,2394}'
!  CHARACTER(LEN=*),PARAMETER :: ia2='{10}'
!  CHARACTER(LEN=*),PARAMETER :: ia3='{}'
!  CHARACTER(LEN=*),PARAMETER :: ia4='{2147483647,-2147483647,0,-0, -1352}'
!  LOGICAL(SBK) :: bool
!  COMPONENT_TEST('Char to Int Array assignment')

!  CALL char_to_int_array(intArray,ia1)
!  bool=ALL(intArray == (/1,2,3,4,5,6,2394/))
!  ASSERT(bool, 'Basic char to int array assignment')
!  CALL char_to_int_array(intArray,ia2)
!  bool=ALL(intArray == (/10/))
!  ASSERT(bool, 'Char to int array assignment, one value')
!  CALL char_to_int_array(intArray,ia3)
!  ASSERT(.NOT.(ALLOCATED(intArray)), 'Char to int array, empty')
!  CALL char_to_int_array(intArray,ia4)
!  bool=ALL(intArray == (/2147483647,-2147483647,0,0/))
!  ASSERT(bool, 'Char to int array assignment, various values, negatives')

!ENDSUBROUTINE testCharToIntArray
!
!---!----------------------------------------------------------------------------
!SUBROUTINE testCharToDoubleArray()
!  REAL(SDK),ALLOCATABLE :: doubleArray(:)
!  CHARACTER(LEN=*),PARAMETER :: da1='{1.0,2.1,3.2,4.5,235.235}'
!  CHARACTER(LEN=*),PARAMETER :: da2='{-734723741.000033,2147483647.2147483647}'
!  CHARACTER(LEN=*),PARAMETER :: da3='{1e5,1e-5,1.351632e12,-4.324E-2}'
!  CHARACTER(LEN=*),PARAMETER :: da4='{8.2085e+05,1.1109e+05,5.5308e+03,&
!    1.8644e+02,3.7612e+01,3.5379e+01,2.7697e+01,2.1684e+01,2.0397e+01,&
!      1.5968e+01,7.1500e+00,6.7000e+00,6.3000e+00,1.0970e+00,1.0450e+00,&
!      9.5000e-01,3.5000e-01,2.0600e-01,1.0700e-01,5.8000e-02,2.5000e-02,&
!      1.0000e-02,1.0000e-05}'
!  LOGICAL(SBK) :: bool
!  COMPONENT_TEST('Char to Double Array assignment')

!  CALL char_to_double_array(doubleArray,da1)
!  bool=ALL(doubleArray .APPROXEQF. (/1.0_SDK,2.1_SDK,3.2_SDK,4.5_SDK,235.235_SDK/))
!  ASSERT(bool, 'Char to double array assignment')
!  CALL char_to_double_array(doubleArray,da2)
!  bool=ALL(doubleArray .APPROXEQF. (/-734723741.000033_SDK,2147483647.2147483647_SDK/))
!  ASSERT(bool, 'Char to double array assignment, big numbers')
!  CALL char_to_double_array(doubleArray,da3)
!  bool=ALL(doubleArray .APPROXEQF. (/1e5_SDK,1e-5_SDK,1.351632e12_SDK,-4.324E-2_SDK/))
!  ASSERT(bool, 'Char to double array assignment, scientific notation')
!  CALL char_to_double_array(doubleArray,da4)
!  bool=ALL(doubleArray .APPROXEQF. (/8.2085e+05_SDK,1.1109e+05_SDK,5.5308e+03_SDK,&
!    1.8644e+02_SDK,3.7612e+01_SDK,3.5379e+01_SDK,2.7697e+01_SDK,2.1684e+01_SDK,2.0397e+01_SDK,&
!      1.5968e+01_SDK,7.1500e+00_SDK,6.7000e+00_SDK,6.3000e+00_SDK,1.0970e+00_SDK,1.0450e+00_SDK,&
!      9.5000e-01_SDK,3.5000e-01_SDK,2.0600e-01_SDK,1.0700e-01_SDK,5.8000e-02_SDK,2.5000e-02_SDK,&
!      1.0000e-02_SDK,1.0000e-05_SDK/))
!  ASSERT(bool, 'Char to double array, large array')
!ENDSUBROUTINE testCharToDoubleArray
!
!-------------------------------------------------------------------------------
!SUBROUTINE testCharToStrArray()
!  COMPONENT_TEST('Char to String Array assignment')
!ENDSUBROUTINE testCharToStrArray
!
ENDPROGRAM testSelectedKinds

