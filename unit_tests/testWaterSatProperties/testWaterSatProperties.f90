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
PROGRAM testWaterSatProperties
#include "UnitTest.h"
  USE UnitTest
  USE IntrType
  USE WaterSatProperties

  IMPLICIT NONE

  REAL(SRK) :: refval,testval

  CREATE_TEST('WATERSATPROPERTIES')

  !Test uninit
  COMPONENT_TEST('Uninititialized')
  refval=-HUGE(refval)
  ASSERT(WaterSatProperties_GetTemp(1040.0_SRK) == refval,'GetTemp')
  ASSERT(WaterSatProperties_GetPres(1040.0_SRK) == refval,'GetPres')
  ASSERT(WaterSatProperties_GetLiqDens(P=1040.0_SRK) == refval,'GetLiqDens(P)')
  ASSERT(WaterSatProperties_GetLiqDens(T=560.0_SRK) == refval,'GetLiqDens(T)')
  ASSERT(WaterSatProperties_GetVapDens(P=1040.0_SRK) == refval,'GetVapDens(P)')
  ASSERT(WaterSatProperties_GetVapDens(T=560.0_SRK) == refval,'GetVapDens(T)')

  CALL WaterSatProperties_Init()

  !Nominal BWR Conditions
  COMPONENT_TEST('Nominal BWR Properties')
  refval=560.610988281243_SRK
  ASSERT(refval .APPROXEQ. WaterSatProperties_GetTemp(1040.0_SRK),'GetTemp')
  refval=1040.0_SRK
  ASSERT(refval .APPROXEQ. WaterSatProperties_GetPres(560.610988281243_SRK),'GetTemp')
  refval=0.736690682364633_SRK
  ASSERT(refval .APPROXEQ. WaterSatProperties_GetLiqDens(P=1040.0_SRK),'GetLiqDens(P)')
  ASSERT(refval .APPROXEQ. WaterSatProperties_GetLiqDens(T=560.610988281243_SRK),'GetLiqDens(T)')
  refval=3.752512913761709E-02_SRK
  ASSERT(refval .APPROXEQ. WaterSatProperties_GetVapDens(P=1040.0_SRK),'GetLiqDens(P)')
  testval=WaterSatProperties_GetVapDens(T=WaterSatProperties_GetTemp(1040.0_SRK))
  ASSERT(refval .APPROXEQ. testval,'GetLiqDens(T)')
  
  !Test exact
  COMPONENT_TEST('Exact Evaluation')
  refval=300.0_SRK
  ASSERT(refval == WaterSatProperties_GetTemp(5.12970461890E-01_SRK),'GetTemp exact')
  refval=5.12970461890E-01_SRK
  ASSERT(refval == WaterSatProperties_GetPres(300.0_SRK),'GetPres exact')
  refval=9.96513027530E-01_SRK
  ASSERT(refval .APPROXEQ. WaterSatProperties_GetLiqDens(P=5.12970461890E-01_SRK),'GetLiqDens(P)')
  ASSERT(refval .APPROXEQ. WaterSatProperties_GetLiqDens(T=300.0_SRK),'GetLiqDens(T)')
  refval=2.55896736840E-05_SRK
  ASSERT(refval .APPROXEQ. WaterSatProperties_GetVapDens(P=5.12970461890E-01_SRK),'GetLiqDens(P)')
  ASSERT(refval .APPROXEQ. WaterSatProperties_GetVapDens(T=300.0_SRK),'GetLiqDens(T)')
  
  !Test out of bounds
  COMPONENT_TEST('Out of Bounds')
  refval=-HUGE(refval)
  ASSERT(WaterSatProperties_GetTemp(0.01_SRK) == refval,'GetTemp below')
  ASSERT(WaterSatProperties_GetTemp(3300.11_SRK) == refval,'GetTemp above')
  ASSERT(WaterSatProperties_GetPres(273.0_SRK) == refval,'GetPres below')
  ASSERT(WaterSatProperties_GetPres(678.0_SRK) == refval,'GetPres above')
  ASSERT(WaterSatProperties_GetLiqDens(P=0.01_SRK) == refval,'GetLiqDens P below')
  ASSERT(WaterSatProperties_GetLiqDens(P=3300.11_SRK) == refval,'GetLiqDens P above')
  ASSERT(WaterSatProperties_GetLiqDens(T=273.0_SRK) == refval,'GetLiqDens T below')
  ASSERT(WaterSatProperties_GetLiqDens(T=678.0_SRK) == refval,'GetLiqDens T above')
  ASSERT(WaterSatProperties_GetVapDens(P=0.01_SRK) == refval,'GetVapDens P below')
  ASSERT(WaterSatProperties_GetVapDens(P=3300.11_SRK) == refval,'GetVapDens P above')
  ASSERT(WaterSatProperties_GetVapDens(T=273.0_SRK) == refval,'GetVapDens T below')
  ASSERT(WaterSatProperties_GetVapDens(T=678.0_SRK) == refval,'GetVapDens T above')
!
ENDPROGRAM testWaterSatProperties
