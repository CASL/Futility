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
PROGRAM testSelectedKinds
      
  USE IntrType
  IMPLICIT NONE

  REAL(SSK) :: singlefloat
  REAL(SDK) :: doublefloat
  INTEGER(SNK) :: singleint
  INTEGER(SLK) :: doubleint

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING SELECTED KINDS...'
  WRITE(*,*) '==================================================='

  WRITE(*,*) 'TESTING PARAMETERS'
  WRITE(*,*) '    SBK = ',SBK
  WRITE(*,*) '    SNK = ',SNK
  WRITE(*,*) '    SLK = ',SLK
  WRITE(*,*) '    SIK = ',SIK
  WRITE(*,*) '    SSK = ',SSK
  WRITE(*,*) '    SDK = ',SDK
  WRITE(*,*) '    SRK = ',SRK
  WRITE(*,*) '   EPSS = ',EPSS
  WRITE(*,*) '   EPSD = ',EPSD
  WRITE(*,*) 'EPSREAL = ',EPSREAL
  WRITE(*,*) 'MINEXPONENT(singlefloat) = ',MINEXPONENT(singlefloat)
  WRITE(*,*) 'MINEXPONENT(doublefloat) = ',MINEXPONENT(doublefloat)
  WRITE(*,*) 'MAXEXPONENT(singlefloat) = ',MAXEXPONENT(singlefloat)
  WRITE(*,*) 'MAXEXPONENT(doublefloat) = ',MAXEXPONENT(doublefloat)
  WRITE(*,*) 'EPSILON(singlefloat) = ',EPSILON(singlefloat)
  WRITE(*,*) 'EPSILON(doublefloat) = ',EPSILON(doublefloat)
  WRITE(*,*) 'PRECISION(singlefloat) = ',PRECISION(singlefloat)
  WRITE(*,*) 'PRECISION(doublefloat) = ',PRECISION(doublefloat)
  WRITE(*,*) 'DIGITS(singlefloat) = ',DIGITS(singlefloat)
  WRITE(*,*) 'DIGITS(doublefloat) = ',DIGITS(singlefloat)
  WRITE(*,*) 'DIGITS(singleint) = ',DIGITS(singleint)
  WRITE(*,*) 'DIGITS(doubleint) = ',DIGITS(doubleint)
  WRITE(*,*) 'BIT_SIZE(singleint) = ',BIT_SIZE(singleint)
  WRITE(*,*) 'BIT_SIZE(doubleint) = ',BIT_SIZE(doubleint)
  WRITE(*,*) '---------------------------------------------------'
  WRITE(*,*) 'TESTING .APPROXEQ.'
  IF(.NOT.(1.00000000000001_SDK .APPROXEQ. 1._SDK) .OR. &
          (1.000000000000011_SDK .APPROXEQ. 1._SDK)) THEN
    WRITE(*,*) '.APPROXEQ. (DOUBLE PRECISION) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: .APPROXEQ. (DOUBLE PRECISION)'
  ENDIF
  IF(.NOT.(1.000009_SSK .APPROXEQ. 1._SSK) .OR. &
          (1.000011_SSK .APPROXEQ. 1._SSK)) THEN
    WRITE(*,*) '.APPROXEQ. (SINGLE PRECISION) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: .APPROXEQ. (SINGLE PRECISION)'
  ENDIF
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING SELECTED KINDS PASSED!'
  WRITE(*,*) '==================================================='
!
ENDPROGRAM testSelectedKinds
