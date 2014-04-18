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
  USE Sorting
  
  IMPLICIT NONE
  
  LOGICAL(SBK) :: bool
  REAL(SRK) :: tmprealarray(10)
  INTEGER(SIK) :: tmpintarray(10)
!
!Check the timer resolution
  CREATE_TEST('SORTING')
!
  COMPONENT_TEST('1-D Real Array')
  tmprealarray(1)=100.0_SRK
  tmprealarray(2)=-5.0_SRK
  tmprealarray(3)=60.0_SRK
  tmprealarray(4)=10.0_SRK
  tmprealarray(5)=45.0_SRK
  tmprealarray(6)=-10.0_SRK
  tmprealarray(7)=20.0_SRK
  tmprealarray(8)=5.0_SRK
  tmprealarray(9)=-30.0_SRK
  tmprealarray(10)=20.0_SRK
  
  CALL sort(tmprealarray)
  
  bool=ALL(tmprealarray .APPROXEQ. (/-30.0_SRK,-10.0_SRK,-5.0_SRK,5.0_SRK,10.0_SRK, &
    20.0_SRK,20.0_SRK,45.0_SRK,60.0_SRK,100.0_SRK/))
  ASSERT(bool,'1-D array sort')
  
  COMPONENT_TEST('1-D Integer Array')
  tmpintarray(1)=-5
  tmpintarray(2)=100
  tmpintarray(3)=60
  tmpintarray(4)=10
  tmpintarray(5)=45
  tmpintarray(6)=-10
  tmpintarray(7)=20
  tmpintarray(8)=5
  tmpintarray(9)=-30
  tmpintarray(10)=20
  
  CALL sort(tmpintarray)
  
  bool=ALL(tmpintarray == (/-30,-10,-5,5,10,20,20,45,60,100/))
  ASSERT(bool,'1-D array sort')
  
  FINALIZE_TEST()
!
ENDPROGRAM testSorting
