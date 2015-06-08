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
PROGRAM testGeom
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV  
  USE UnitTest
  USE IntrType
  USE Constants_Conversion
  USE ParameterLists
  USE Geom
  
  IMPLICIT NONE
  
  TYPE(ParamType) :: params
  TYPE(PointType) :: point,point2,point3
  TYPE(LineType) :: line1
  TYPE(PlaneType) :: plane1
  TYPE(CircleType) :: circle1
  TYPE(CylinderType) :: cylinder1
  TYPE(OBBoxType) :: box
  REAL(SRK) :: e_3d(3)
  REAL(SRK) :: u1_3d(3),u2_3d(3),u3_3d(3)
  REAL(SRK) :: n(3)
  LOGICAL(SBK) :: bool
  
  CREATE_TEST('Test Geom')
  CALL eParams%setQuietMode(.TRUE.)
  CALL eParams%setStopOnError(.FALSE.)
  
  REGISTER_SUBTEST('Test Geom',testGeoms)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
  SUBROUTINE testGeoms
    !Test newGeom for OBBoxType
    CALL point%init(DIM=3,X=1._SRK,Y=1._SRK,Z=1._SRK)
    e_3d=(/SQRT(2._SRK),2*SQRT(2._SRK),1._SRK/)
    u1_3d=(/1._SRK,1._SRK,0._SRK/)
    u2_3d=(/-1._SRK,1._SRK,0._SRK/)
    u3_3d=(/0._SRK,0._SRK,2._SRK/)
    CALL newGeom(params,box)
    bool = .NOT.(ANY(box%u /= 0.0_SRK) .OR. ANY(box%e /= 0.0_SRK) &
                 .OR. box%p0%dim /= 0 .OR. ALLOCATED(box%p0%coord))
    ASSERT(bool, 'CALL newGeom(params,box) (empty params)')
    CALL params%add('BoxGeom -> CornerPoint',1)
    CALL params%add('BoxGeom -> Vector1',5)
    CALL params%add('BoxGeom -> Vector2',1)
    CALL params%add('BoxGeom -> ExtentVector',0)
    CALL newGeom(params,box)
    bool = .NOT.(ANY(box%u /= 0.0_SRK) .OR. ANY(box%e /= 0.0_SRK) &
                 .OR. box%p0%dim /= 0 .OR. ALLOCATED(box%p0%coord))
    ASSERT(bool, 'CALL newGeom(params,box) (bad params)')
    CALL params%clear()
    CALL params%add('BoxGeom -> CornerPoint',point%coord)
    CALL params%add('BoxGeom -> Vector1',u1_3d)
    CALL params%add('BoxGeom -> Vector2',u2_3d)
    CALL params%add('BoxGeom -> Vector3',u3_3d)
    CALL params%add('BoxGeom -> ExtentVector',e_3d)
    CALL newGeom(params,box)
    bool = .NOT.(box%p0 /= point .OR. ANY(box%e /= e_3d) &
                .OR. ANY(.NOT.(box%u(:,1) .APPROXEQ. u1_3d/SQRT(2._SRK))) &
                .OR. ANY(.NOT.(box%u(:,2) .APPROXEQ. u2_3d/SQRT(2._SRK))) &
                .OR. ANY(.NOT.(box%u(:,3) .APPROXEQ. u3_3d*0.5_SRK)) )
    ASSERT(bool, 'CALL newGeom(params,box)')
    CALL params%clear()

    !Test newGeom for circle and cylinder types
    CALL point%clear()
    CALL point%init(DIM=2,X=0.1_SRK,Y=0.2_SRK)
    CALL newGeom(params,circle1)
    bool=.NOT.(circle1%r /= 0.0_SRK .OR. circle1%c%dim /= 0 .OR. &
               ALLOCATED(circle1%c%coord))
    ASSERT(bool, 'CALL newGeom(params,circle1) (empty params)')
    CALL params%add('CircleGeom -> Radius',1)
    CALL params%add('CircleGeom -> Centroid',2)
    CALL params%add('CircleGeom -> StartingAngle',.FALSE.)
    CALL params%add('CircleGeom -> StoppingAngle',(/0.0,0.0/))
    CALL newGeom(params,circle1)
    bool=.NOT.(circle1%r /= 0.0_SRK .OR. circle1%c%dim /= 0 .OR. &
               ALLOCATED(circle1%c%coord))
    ASSERT(bool, 'CALL newGeom(params,circle1) (bad params)')
    CALL params%clear()
    CALL params%add('CircleGeom -> Radius',0.5_SRK)
    CALL params%add('CircleGeom -> Centroid',point%coord)
    CALL newGeom(params,circle1)
    bool=.NOT.(circle1%r /= 0.5_SRK .OR. circle1%c%dim /= 2 .OR. &
               circle1%c%coord(1) /= 0.1_SRK .OR. circle1%c%coord(2) /= 0.2_SRK)
    ASSERT(bool, 'CALL newGeom(params,circle1)')
    CALL params%clear()
    CALL point3%clear()
    CALL point2%clear()
    CALL point2%init(DIM=3,X=0.1_SRK,Y=0.2_SRK,Z=0.3_SRK)
    CALL point3%init(DIM=3,X=0.1_SRK,Y=0.2_SRK,Z=1.3_SRK)
    CALL newGeom(params,cylinder1)
    bool=.NOT.(cylinder1%r /= 0.0_SRK .OR. cylinder1%axis%p1%dim /= 0 .OR. &
               ALLOCATED(cylinder1%axis%p1%coord) .OR. cylinder1%axis%p2%dim /= 0 .OR. &
               ALLOCATED(cylinder1%axis%p2%coord))
    ASSERT(bool,'CALL newGeom(params,cylinder1) (empty params)')
    CALL params%add('CylinderGeom -> Radius',1)
    CALL params%add('CylinderGeom -> BottomCentroid',2)
    CALL params%add('CylinderGeom -> TopCentroid',3)
    CALL params%add('CylinderGeom -> StartingAngle',.FALSE.)
    CALL params%add('CylinderGeom -> StoppingAngle',(/0.0,0.0/))
    CALL newGeom(params,cylinder1)
    bool=.NOT.(cylinder1%r /= 0.0_SRK .OR. cylinder1%axis%p1%dim /= 0 .OR. &
               ALLOCATED(cylinder1%axis%p1%coord) .OR. cylinder1%axis%p2%dim /= 0 .OR. &
               ALLOCATED(cylinder1%axis%p2%coord))
    ASSERT(bool,'CALL newGeom(params,cylinder1) (bad params)')
    CALL params%clear()
    CALL params%add('CylinderGeom -> Radius',1.0_SRK)
    CALL params%add('CylinderGeom -> BottomCentroid',point2%coord)
    CALL params%add('CylinderGeom -> TopCentroid',point3%coord)
    CALL newGeom(params,cylinder1)
    bool=.NOT.(cylinder1%r /= 1.0_SRK .OR. cylinder1%axis%p1%dim /= 3 .OR. &
               ANY(cylinder1%axis%p1%coord /= (/0.1_SRK,0.2_SRK,0.3_SRK/)) .OR. &
               cylinder1%axis%p2%dim /= 3 .OR. &
               ANY(cylinder1%axis%p2%coord /= (/0.1_SRK,0.2_SRK,1.3_SRK/)))
    ASSERT(bool,'CALL newGeom(params,cylinder1)')
    CALL params%clear()
    CALL cylinder1%clear()

    !Test newGeom for line Type
    CALL point2%clear()
    CALL point3%clear()
    CALL point2%init(DIM=3,X=0.1_SRK,Y=0.2_SRK,Z=0.3_SRK)
    CALL point3%init(DIM=3,X=0.4_SRK,Y=0.5_SRK,Z=0.6_SRK)
    CALL newGeom(params,line1)
    bool=.NOT.(line1%p1%dim /= 0 .OR. line1%p2%dim /= 0 .OR. &
               ALLOCATED(line1%p1%coord) .OR. ALLOCATED(line1%p2%coord))
    ASSERT(bool, 'CALL newGeom(params,line1) (empty params)')
    CALL params%add('LineGeom->StartPoint',1)
    CALL params%add('LineGeom->EndPoint',3)
    CALL newGeom(params,line1)
    bool=.NOT.(line1%p1%dim /= 0 .OR. line1%p2%dim /= 0 .OR. &
               ALLOCATED(line1%p1%coord) .OR. ALLOCATED(line1%p2%coord))
    ASSERT(bool, 'CALL newGeom(params,line1) (badparams)')
    CALL params%clear()
    CALL params%add('LineGeom->StartPoint',point2%coord)
    CALL params%add('LineGeom->EndPoint',point3%coord)
    CALL newGeom(params,line1)
    bool=.NOT.(line1%p1%dim /= 3 .OR. line1%p2%dim /= 3 .OR. &
               line1%p1%coord(1) /= 0.1_SRK .OR. line1%p1%coord(2) /= 0.2_SRK .OR. &
               line1%p1%coord(3) /= 0.3_SRK .OR. line1%p2%coord(1) /= 0.4_SRK .OR. &
               line1%p2%coord(2) /= 0.5_SRK .OR. line1%p2%coord(3) /= 0.6_SRK)
    ASSERT(bool, 'CALL newGeom(params,line1)')
    CALL params%clear()
    CALL line1%clear()

    !Test newGeom for plane
    n=(/1.0_SRK,1.0_SRK,1.0_SRK/)
    CALL point%clear()
    CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
    CALL newGeom(params,plane1)
    bool = .NOT.(ANY(plane1%n /= 0.0_SRK) .OR. plane1%v0%dim /= 0 .OR. &
                 ALLOCATED(plane1%v0%coord))
    ASSERT(bool, 'CALL newGeom(params,plane1) (empty params)')
    CALL params%add('PlaneGeom->Point',1)
    CALL params%add('PlaneGeom->NormalVector',3)
    CALL newGeom(params,plane1)
    bool = .NOT.(ANY(plane1%n /= 0.0_SRK) .OR. plane1%v0%dim /= 0 .OR. &
                 ALLOCATED(plane1%v0%coord))
    ASSERT(bool, 'CALL newGeom(params,plane1) (bad params)')
    CALL params%clear()
    CALL params%add('PlaneGeom->Point',point%coord)
    CALL params%add('PlaneGeom->NormalVector',n)
    CALL newGeom(params,plane1)
    bool = .NOT.(ANY(plane1%v0%coord /= 0.5_SRK) .OR. ANY(.NOT.(plane1%n .APPROXEQ. &
                 (/1._SRK/SQRT(3.0_SRK),1._SRK/SQRT(3.0_SRK),1._SRK/SQRT(3.0_SRK)/))))
    ASSERT(bool, 'CALL newGeom(params,plane1)')
  ENDSUBROUTINE
ENDPROGRAM testGeom
