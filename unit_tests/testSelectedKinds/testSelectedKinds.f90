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
#include "UnitTest.h"
  USE UnitTest
  USE Utils
  
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
  WRITE(*,*) '---------------------------------------------------'
  
  CALL evalIntModel()
  CALL evalFPModel()
!
! Testing APPROXEQ
  WRITE(*,*) 'TESTING .APPROXEQ.'
  IF(1.234560_SSK .APPROXEQ. 1.234575_SSK) THEN
    WRITE(*,*) '1.234560_SSK .APPROXEQ. 1.234575_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234560_SSK .APPROXEQ. 1.234570_SSK)) THEN
    WRITE(*,*) '1.234560_SSK .APPROXEQ. 1.234570_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.234560e10_SSK .APPROXEQ. 1.234575e10_SSK) THEN
    WRITE(*,*) '1.234560e10_SSK .APPROXEQ. 1.234575e10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234560e10_SSK .APPROXEQ. 1.234570e10_SSK)) THEN
    WRITE(*,*) '1.234560e10_SSK .APPROXEQ. 1.234570e10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.234560e-10_SSK .APPROXEQ. 1.234575e-10_SSK) THEN
    WRITE(*,*) '1.234560e-10_SSK .APPROXEQ. 1.234575e-10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234560e-10_SSK .APPROXEQ. 1.234570e-10_SSK)) THEN
    WRITE(*,*) '1.234560e-10_SSK .APPROXEQ. 1.234570e-10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.8_SSK .APPROXEQ. 0.79999999_SSK)) THEN
    WRITE(*,*) '0.8_SSK .APPROXEQ. 0.79999999_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.0e20_SSK .APPROXEQ. 0.9999999e20_SSK)) THEN
    WRITE(*,*) '1.0e20_SSK .APPROXEQ. 0.9999999e20_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.0_SSK .APPROXEQ. 1.2345678e-18_SSK)) THEN
    WRITE(*,*) '0.0_SSK .APPROXEQ. 1.2345678e-18_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.0_SSK .APPROXEQ. -1.0_SSK) THEN
    WRITE(*,*) '1.0_SSK .APPROXEQ. -1.0_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.0e10_SSK .APPROXEQ. 1.0e-10_SSK) THEN
    WRITE(*,*) '1.0e10_SSK .APPROXEQ. 1.0e-10_SSK FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: .APPROXEQ. (SINGLE PRECISION)'
  
  IF(1.234567890123450_SDK .APPROXEQ. 1.234567890123465_SDK) THEN
    WRITE(*,*) '1.234567890123450_SDK .APPROXEQ. 1.234567890123465_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234567890123450_SDK .APPROXEQ. 1.234567890123460_SDK)) THEN
    WRITE(*,*) '1.234567890123450_SDK .APPROXEQ. 1.234567890123460_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.234567890123450e10_SDK .APPROXEQ. 1.234567890123465e10_SDK) THEN
    WRITE(*,*) '1.234567890123450e10_SDK .APPROXEQ. 1.234567890123465e10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234567890123450e10_SDK .APPROXEQ. 1.234567890123460e10_SDK)) THEN
    WRITE(*,*) '1.234567890123450e10_SDK .APPROXEQ. 1.234567890123460e10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.234567890123450e-10_SDK .APPROXEQ. 1.234567890123465e-10_SDK) THEN
    WRITE(*,*) '1.234567890123450e-10_SDK .APPROXEQ. 1.234567890123465e-10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234567890123450e-10_SDK .APPROXEQ. 1.234567890123460e-10_SDK)) THEN
    WRITE(*,*) '1.234567890123450e-10_SDK .APPROXEQ. 1.234567890123460e-10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.8_SDK .APPROXEQ. 0.7999999999999999_SDK)) THEN
    WRITE(*,*) '0.8_SDK .APPROXEQ. 0.7999999999999999_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.0e20_SDK .APPROXEQ. 0.9999999999999999e20_SDK)) THEN
    WRITE(*,*) '1.0e20_SDK .APPROXEQ. 0.9999999999999999e20_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.0_SDK .APPROXEQ. 1.234567890123450e-18_SDK)) THEN
    WRITE(*,*) '0.0_SDK .APPROXEQ. 1.234567890123450e-18_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.0_SDK .APPROXEQ. -1.0_SDK) THEN
    WRITE(*,*) '1.0_SDK .APPROXEQ. -1.0_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.0e10_SDK .APPROXEQ. 1.0e-10_SDK) THEN
    WRITE(*,*) '1.0e10_SDK .APPROXEQ. 1.0e-10_SDK FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: .APPROXEQ. (DOUBLE PRECISION)'
!
! Testing APPROXEQA  
  WRITE(*,*) 'TESTING .APPROXEQA.'
  IF(1.234560_SSK .APPROXEQA. 1.234571_SSK) THEN
    WRITE(*,*) '1.234560_SSK .APPROXEQA. 1.234571_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234560_SSK .APPROXEQA. 1.234569_SSK)) THEN
    WRITE(*,*) '1.234560_SSK .APPROXEQA. 1.234570_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.234560e10_SSK .APPROXEQA. 1.234575e10_SSK) THEN
    WRITE(*,*) '1.234560e10_SSK .APPROXEQA. 1.234575e10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.234560e10_SSK .APPROXEQA. 1.234570e10_SSK) THEN
    WRITE(*,*) '1.234560e10_SSK .APPROXEQA. 1.234570e10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234560e-10_SSK .APPROXEQA. 1.234575e-10_SSK)) THEN
    WRITE(*,*) '1.234560e-10_SSK .APPROXEQA. 1.234575e-10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234560e-10_SSK .APPROXEQA. 1.234570e-10_SSK)) THEN
    WRITE(*,*) '1.234560e-10_SSK .APPROXEQA. 1.234570e-10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.8_SSK .APPROXEQA. 0.79999999_SSK)) THEN
    WRITE(*,*) '0.8_SSK .APPROXEQA. 0.79999999_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.0e20_SSK .APPROXEQA. 0.9999999e20_SSK) THEN
    WRITE(*,*) '1.0e20_SSK .APPROXEQA. 0.9999999e20_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.0_SSK .APPROXEQA. 1.2345678e-18_SSK)) THEN
    WRITE(*,*) '0.0_SSK .APPROXEQA. 1.2345678e-18_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.0_SSK .APPROXEQA. -1.0_SSK) THEN
    WRITE(*,*) '1.0_SSK .APPROXEQA. -1.0_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.0e10_SSK .APPROXEQA. 1.0e-10_SSK) THEN
    WRITE(*,*) '1.0e10_SSK .APPROXEQA. 1.0e-10_SSK FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: .APPROXEQA. (SINGLE PRECISION)'
  
  IF(1.234567890123450_SDK .APPROXEQA. 1.234567890123465_SDK) THEN
    WRITE(*,*) '1.234567890123450_SDK .APPROXEQA. 1.234567890123465_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234567890123450_SDK .APPROXEQA. 1.234567890123460_SDK)) THEN
    WRITE(*,*) '1.234567890123450_SDK .APPROXEQA. 1.234567890123460_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.234567890123450e10_SDK .APPROXEQA. 1.234567890123465e10_SDK) THEN
    WRITE(*,*) '1.234567890123450e10_SDK .APPROXEQA. 1.234567890123465e10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.234567890123450e10_SDK .APPROXEQA. 1.234567890123460e10_SDK) THEN
    WRITE(*,*) '1.234567890123450e10_SDK .APPROXEQA. 1.234567890123460e10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234567890123450e-10_SDK .APPROXEQA. 1.234567890123465e-10_SDK)) THEN
    WRITE(*,*) '1.234567890123450e-10_SDK .APPROXEQA. 1.234567890123465e-10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234567890123450e-10_SDK .APPROXEQA. 1.234567890123460e-10_SDK)) THEN
    WRITE(*,*) '1.234567890123450e-10_SDK .APPROXEQA. 1.234567890123460e-10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.8_SDK .APPROXEQA. 0.7999999999999999_SDK)) THEN
    WRITE(*,*) '0.8_SDK .APPROXEQA. 0.7999999999999999_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.0e20_SDK .APPROXEQA. 0.9999999999999999e20_SDK) THEN
    WRITE(*,*) '1.0e20_SDK .APPROXEQA. 0.9999999999999999e20_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.0_SDK .APPROXEQA. 1.234567890123450e-18_SDK)) THEN
    WRITE(*,*) '0.0_SDK .APPROXEQA. 1.234567890123450e-18_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.0_SDK .APPROXEQA. -1.0_SDK) THEN
    WRITE(*,*) '1.0_SDK .APPROXEQA. -1.0_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.0e10_SDK .APPROXEQA. 1.0e-10_SDK) THEN
    WRITE(*,*) '1.0e10_SDK .APPROXEQA. 1.0e-10_SDK FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: .APPROXEQA. (DOUBLE PRECISION)'
!
! Testing APPROXEQR
  WRITE(*,*) 'TESTING .APPROXEQR.'
  IF(1.234560_SSK .APPROXEQR. 1.234575_SSK) THEN
    WRITE(*,*) '1.234560_SSK .APPROXEQR. 1.234575_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234560_SSK .APPROXEQR. 1.234570_SSK)) THEN
    WRITE(*,*) '1.234560_SSK .APPROXEQR. 1.234570_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.234560e10_SSK .APPROXEQR. 1.234575e10_SSK) THEN
    WRITE(*,*) '1.234560e10_SSK .APPROXEQR. 1.234575e10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234560e10_SSK .APPROXEQR. 1.234570e10_SSK)) THEN
    WRITE(*,*) '1.234560e10_SSK .APPROXEQR. 1.234570e10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.234560e-10_SSK .APPROXEQR. 1.234575e-10_SSK) THEN
    WRITE(*,*) '1.234560e-10_SSK .APPROXEQR. 1.234575e-10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234560e-10_SSK .APPROXEQR. 1.234570e-10_SSK)) THEN
    WRITE(*,*) '1.234560e-10_SSK .APPROXEQR. 1.234570e-10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.8_SSK .APPROXEQR. 0.79999999_SSK)) THEN
    WRITE(*,*) '0.8_SSK .APPROXEQR. 0.79999999_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.0e20_SSK .APPROXEQR. 0.9999999e20_SSK)) THEN
    WRITE(*,*) '1.0e20_SSK .APPROXEQR. 0.9999999e20_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.0_SSK .APPROXEQR. 1.2345678e-18_SSK)) THEN
    WRITE(*,*) '0.0_SSK .APPROXEQR. 1.2345678e-18_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.0_SSK .APPROXEQR. -1.0_SSK) THEN
    WRITE(*,*) '1.0_SSK .APPROXEQR. -1.0_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.0e10_SSK .APPROXEQR. 1.0e-10_SSK) THEN
    WRITE(*,*) '1.0e10_SSK .APPROXEQR. 1.0e-10_SSK FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: .APPROXEQR. (SINGLE PRECISION)'
  
  IF(1.234567890123450_SDK .APPROXEQR. 1.234567890123465_SDK) THEN
    WRITE(*,*) '1.234567890123450_SDK .APPROXEQR. 1.234567890123465_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234567890123450_SDK .APPROXEQR. 1.234567890123460_SDK)) THEN
    WRITE(*,*) '1.234567890123450_SDK .APPROXEQR. 1.234567890123460_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.234567890123450e10_SDK .APPROXEQR. 1.234567890123465e10_SDK) THEN
    WRITE(*,*) '1.234567890123450e10_SDK .APPROXEQR. 1.234567890123465e10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234567890123450e10_SDK .APPROXEQR. 1.234567890123460e10_SDK)) THEN
    WRITE(*,*) '1.234567890123450e10_SDK .APPROXEQR. 1.234567890123460e10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.234567890123450e-10_SDK .APPROXEQR. 1.234567890123465e-10_SDK) THEN
    WRITE(*,*) '1.234567890123450e-10_SDK .APPROXEQR. 1.234567890123465e-10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234567890123450e-10_SDK .APPROXEQR. 1.234567890123460e-10_SDK)) THEN
    WRITE(*,*) '1.234567890123450e-10_SDK .APPROXEQR. 1.234567890123460e-10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.8_SDK .APPROXEQR. 0.7999999999999999_SDK)) THEN
    WRITE(*,*) '0.8_SDK .APPROXEQR. 0.7999999999999999_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.0e20_SDK .APPROXEQR. 0.9999999999999999e20_SDK)) THEN
    WRITE(*,*) '1.0e20_SDK .APPROXEQR. 0.9999999999999999e20_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.0_SDK .APPROXEQR. 1.234567890123450e-18_SDK)) THEN
    WRITE(*,*) '0.0_SDK .APPROXEQR. 1.234567890123450e-18_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.0_SDK .APPROXEQR. -1.0_SDK) THEN
    WRITE(*,*) '1.0_SDK .APPROXEQR. -1.0_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.0e10_SDK .APPROXEQR. 1.0e-10_SDK) THEN
    WRITE(*,*) '1.0e10_SDK .APPROXEQR. 1.0e-10_SDK FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: .APPROXEQR. (DOUBLE PRECISION)'
!
! Testing APPROXEQF
  WRITE(*,*) 'TESTING .APPROXEQF.'
  IF(1.234560_SSK .APPROXEQF. 1.234575_SSK) THEN
    WRITE(*,*) '1.234560_SSK .APPROXEQF. 1.234575_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234560_SSK .APPROXEQF. 1.234561_SSK)) THEN
    WRITE(*,*) '1.234560_SSK .APPROXEQF. 1.234561_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.234560e10_SSK .APPROXEQF. 1.234575e10_SSK) THEN
    WRITE(*,*) '1.234560e10_SSK .APPROXEQF. 1.234575e10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234560e10_SSK .APPROXEQF. 1.234561e10_SSK)) THEN
    WRITE(*,*) '1.234560e10_SSK .APPROXEQF. 1.234561e10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.234560e-10_SSK .APPROXEQF. 1.234575e-10_SSK) THEN
    WRITE(*,*) '1.234560e-10_SSK .APPROXEQF. 1.234575e-10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234560e-10_SSK .APPROXEQF. 1.234561e-10_SSK)) THEN
    WRITE(*,*) '1.234560e-10_SSK .APPROXEQF. 1.234561e-10_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.8_SSK .APPROXEQF. 0.79999999_SSK)) THEN
    WRITE(*,*) '0.8_SSK .APPROXEQF. 0.79999999_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.0e20_SSK .APPROXEQF. 0.9999999e20_SSK)) THEN
    WRITE(*,*) '1.0e20_SSK .APPROXEQF. 0.9999999e20_SSK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.0_SSK .APPROXEQF. 1.2345678e-18_SSK)) THEN
    WRITE(*,*) '0.0_SSK .APPROXEQF. 1.2345678e-18_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.0_SSK .APPROXEQF. -1.0_SSK) THEN
    WRITE(*,*) '1.0_SSK .APPROXEQF. -1.0_SSK FAILED!'
    STOP 666
  ENDIF
  IF(1.0e10_SSK .APPROXEQF. 1.0e-10_SSK) THEN
    WRITE(*,*) '1.0e10_SSK .APPROXEQF. 1.0e-10_SSK FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: .APPROXEQF. (SINGLE PRECISION)'
  
  IF(1.234567890123450_SDK .APPROXEQF. 1.234567890123465_SDK) THEN
    WRITE(*,*) '1.234567890123450_SDK .APPROXEQF. 1.234567890123465_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234567890123450_SDK .APPROXEQF. 1.234567890123452_SDK)) THEN
    WRITE(*,*) '1.234567890123450_SDK .APPROXEQF. 1.234567890123452_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.234567890123450e10_SDK .APPROXEQF. 1.234567890123465e10_SDK) THEN
    WRITE(*,*) '1.234567890123450e10_SDK .APPROXEQF. 1.234567890123465e10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234567890123450e10_SDK .APPROXEQF. 1.234567890123452e10_SDK)) THEN
    WRITE(*,*) '1.234567890123450e10_SDK .APPROXEQF. 1.234567890123452e10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.234567890123450e-10_SDK .APPROXEQF. 1.234567890123465e-10_SDK) THEN
    WRITE(*,*) '1.234567890123450e-10_SDK .APPROXEQF. 1.234567890123465e-10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.234567890123450e-10_SDK .APPROXEQF. 1.234567890123452e-10_SDK)) THEN
    WRITE(*,*) '1.234567890123450e-10_SDK .APPROXEQF. 1.234567890123452e-10_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.8_SDK .APPROXEQF. 0.7999999999999999_SDK)) THEN
    WRITE(*,*) '0.8_SDK .APPROXEQF. 0.7999999999999999_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(1.0e20_SDK .APPROXEQF. 0.9999999999999999e20_SDK)) THEN
    WRITE(*,*) '1.0e20_SDK .APPROXEQF. 0.9999999999999999e20_SDK FAILED!'
    STOP 666
  ENDIF
  IF(.NOT.(0.0_SDK .APPROXEQF. 1.234567890123450e-18_SDK)) THEN
    WRITE(*,*) '0.0_SDK .APPROXEQF. 1.234567890123450e-18_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.0_SDK .APPROXEQF. -1.0_SDK) THEN
    WRITE(*,*) '1.0_SDK .APPROXEQF. -1.0_SDK FAILED!'
    STOP 666
  ENDIF
  IF(1.0e10_SDK .APPROXEQF. 1.0e-10_SDK) THEN
    WRITE(*,*) '1.0e10_SDK .APPROXEQF. 1.0e-10_SDK FAILED!'
    STOP 666
  ENDIF
  WRITE(*,*) '  Passed: .APPROXEQF. (DOUBLE PRECISION)'
!  
! Testing APPROXLE
  WRITE(*,*) 'TESTING .APPROXLE.'
  IF(.NOT.(1.00000000000001_SDK .APPROXLE. 1._SDK) .OR. &
     .NOT.(1._SDK               .APPROXLE. 3._SDK) .OR. &
          (1.000000000000011_SDK .APPROXLE. 1._SDK) .OR. &
          (3._SDK                .APPROXLE. 1._SDK)) THEN
    WRITE(*,*) '.APPROXLE. (DOUBLE PRECISION) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: .APPROXLE. (DOUBLE PRECISION)'
  ENDIF
  IF(.NOT.(1.000009_SSK .APPROXLE. 1._SSK) .OR. &
     .NOT.(1._SSK       .APPROXLE. 3._SSK) .OR. &
          (1.000011_SSK .APPROXLE. 1._SSK) .OR. &
          (3._SSK       .APPROXLE. 1._SSK)) THEN
    WRITE(*,*) '.APPROXLE. (SINGLE PRECISION) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: .APPROXLE. (SINGLE PRECISION)'
  ENDIF
!
! Testing APPROXGE
  WRITE(*,*) 'TESTING .APPROXGE.'
  IF(.NOT.(1._SDK .APPROXGE. 1.00000000000001_SDK) .OR. &
     .NOT.(3._SDK .APPROXGE.               1._SDK) .OR. &
          (1._SDK .APPROXGE. 1.000000000000011_SDK) .OR. &
          (1._SDK .APPROXGE.               3._SDK)) THEN
    WRITE(*,*) '.APPROXGE. (DOUBLE PRECISION) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: .APPROXGE. (DOUBLE PRECISION)'
  ENDIF
  IF(.NOT.(1._SSK .APPROXGE. 1.000009_SSK) .OR. &
     .NOT.(3._SSK .APPROXGE.       1._SSK) .OR. &
          (1._SSK .APPROXGE. 1.000011_SSK) .OR. &
          (1._SSK .APPROXGE.        3._SSK)) THEN
    WRITE(*,*) '.APPROXGE. (SINGLE PRECISION) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: .APPROXGE. (SINGLE PRECISION)'
  ENDIF
!
! Test SOFTEQ
  WRITE(*,*) 'TESTING SOFTEQ'
  IF(.NOT.(SOFTEQ(1.0000001_SDK,1._SDK,1.0E-6_SDK)) .OR. &
          (SOFTEQ(1.0000011_SDK,1._SDK,1.0E-6_SDK))) THEN
    WRITE(*,*) 'CALL SOFTEQ(...) (DOUBLE PRECISION) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL SOFTEQ(...) (DOUBLE PRECISION)'
  ENDIF
  IF(.NOT.(SOFTEQ(1.00001_SSK,1._SSK,1.0E-4_SSK)) .OR. &
          (SOFTEQ(1.00011_SSK,1._SSK,1.0E-4_SSK))) THEN
    WRITE(*,*) 'CALL SOFTEQ(...) (SINGLE PRECISION) FAILED!'
    STOP 666
  ELSE
    WRITE(*,*) '  Passed: CALL SOFTEQ(...) (SINGLE PRECISION)'
  ENDIF
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING SELECTED KINDS PASSED!'
  WRITE(*,*) '==================================================='
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
      REAL(SDK) :: largest,eps
      
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
ENDPROGRAM testSelectedKinds
