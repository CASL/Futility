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
  REAL(SRK) :: tmprealarray(10),tmprealarray2(5,5)
  INTEGER(SIK) :: tmpintarray(10),tmpintarray2(5,5),tmpintarray3(5,5)
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
  
  COMPONENT_TEST('2-D Integer Array')
  tmpintarray2(1,1)=-5
  tmpintarray2(2,1)=100
  tmpintarray2(3,1)=60
  tmpintarray2(4,1)=10
  tmpintarray2(5,1)=45
  tmpintarray2(1,2)=-10
  tmpintarray2(2,2)=20
  tmpintarray2(3,2)=5
  tmpintarray2(4,2)=-30
  tmpintarray2(5,2)=20
  tmpintarray2(1,3)=0
  tmpintarray2(2,3)=-20
  tmpintarray2(3,3)=-15
  tmpintarray2(4,3)=30
  tmpintarray2(5,3)=21
  tmpintarray2(1,4)=4
  tmpintarray2(2,4)=-16
  tmpintarray2(3,4)=-50
  tmpintarray2(4,4)=-30
  tmpintarray2(5,4)=50
  tmpintarray2(1,5)=53
  tmpintarray2(2,5)=77
  tmpintarray2(3,5)=88
  tmpintarray2(4,5)=99
  tmpintarray2(5,5)=35
  
  CALL sort(tmpintarray2)
  bool=ALL(tmpintarray2 == RESHAPE((/-50,-30,-30,-20,-16,-15,-10,-5,0,4,5,10,20,20,21,30,35,45,50,53,60,77,88,99,100/),(/5,5/)))
  ASSERT(bool,'2-D array sort')
  
  FINALIZE_TEST()
!
ENDPROGRAM testSorting
