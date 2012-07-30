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
PROGRAM testParameterLists
  
  USE ISO_FORTRAN_ENV
  USE ParameterLists
  IMPLICIT NONE
  
  TYPE(ParamType) :: testParam,testParam2
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING PARAMETERLISTS...'
  WRITE(*,*) '==================================================='
  
  CALL testParam2%init(5.0,'testSSK','The number 5.0')
  CALL testParam2%edit(OUTPUT_UNIT)
  CALL testParam%init(testParam2%pdat,'testParam','A single parameter')
  CALL testParam%edit(OUTPUT_UNIT)
  CALL testParam2%set(3.0,'testSSK')
  CALL testParam%edit(OUTPUT_UNIT)
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING PARAMETERLISTS PASSED!'
  WRITE(*,*) '==================================================='
!
ENDPROGRAM testParameterLists
