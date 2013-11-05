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
PROGRAM testSorting
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE Times
  IMPLICIT NONE
  
  TYPE(TimerType) :: testTimer
  
  INTEGER :: idum1,idum2,idum3,ioerr
  CHARACTER(LEN=1) :: adum1,adum2
  CHARACTER(LEN=5) :: adum3
  CHARACTER(LEN=2) :: adum4
  CHARACTER(LEN=MAXLEN_DATE_STRING) :: adate
  CHARACTER(LEN=MAXLEN_CLOCK_STRING) :: aclock
!
!Check the timer resolution
  CREATE_TEST('TIMERS')
!
  FINALIZE_TEST()
!
ENDPROGRAM testSorting
