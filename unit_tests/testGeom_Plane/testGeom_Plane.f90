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
PROGRAM testGeom_Plane
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV  
  USE UnitTest
  USE IntrType
  USE Constants_Conversion
  USE ParameterLists
  USE Geom
  
  IMPLICIT NONE
  
  TYPE(PointType) :: point,point2
  TYPE(PointType) :: points(2)
  TYPE(LineType) :: line1,line2
  TYPE(PlaneType) :: plane1,plane2,planes(2)
  INTEGER(SIK) :: i
  REAL(SRK) :: d
  LOGICAL(SBK) :: bool
  
  CREATE_TEST('Test Geom')
  CALL eParams%setQuietMode(.TRUE.)
  CALL eParams%setStopOnError(.FALSE.)
  
  REGISTER_SUBTEST('Test Plane',TestPlane)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestPlane
      REAL(SRK) :: n(3)
      TYPE(ParamType) :: params
!
!Test clear
      COMPONENT_TEST('%clear()')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      plane1%n=(/1.0_SRK,1.0_SRK,1.0_SRK/)
      plane1%v0=point2
      CALL plane1%clear()
      bool = .NOT.(ANY(plane1%n /= 0.0_SRK) .OR. plane1%v0%dim /= 0 .OR. &
                   ALLOCATED(plane1%v0%coord))
      ASSERT(bool, 'plane1%clear()')
      
      COMPONENT_TEST('%set()')
      n=(/1.0_SRK,1.0_SRK,1.0_SRK/)
      CALL plane1%set(n,point)
      bool = .NOT.(ANY(plane1%v0%coord /= 0.5_SRK) .OR. ANY(.NOT.(plane1%n .APPROXEQ. &
                   (/1._SRK/SQRT(3.0_SRK),1._SRK/SQRT(3.0_SRK),1._SRK/SQRT(3.0_SRK)/))))
      ASSERT(bool, 'plane1%set(...)')
      CALL plane1%clear()
      
      !Test disjoint-ness
      COMPONENT_TEST('%intersect()')
      CALL line1%clear()
      CALL line1%p1%init(COORD=(/0._SRK,0._SRK,0._SRK/))
      CALL line1%p2%init(COORD=(/0.1_SRK,0.1_SRK,0.1_SRK/))
      point2=plane1%intersectLine(line1)
      ASSERT(point2%dim == -3, 'plane%intersect(...)')
      
      !Test for collinearity
      CALL line1%clear()
      CALL line1%p1%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      CALL line1%p2%init(COORD=(/0.75_SRK,0.75_SRK,0._SRK/))
      point2=plane1%intersectLine(line1)
      ASSERT(point2%dim == -2, 'plane%intersect(...)')
      
      !Test for parallel
      CALL line1%clear()
      CALL line1%p1%init(COORD=(/0.4_SRK,0.4_SRK,0.4_SRK/))
      CALL line1%p2%init(COORD=(/0.65_SRK,0.65_SRK,-0.1_SRK/))
      point2=plane1%intersectLine(line1)
      ASSERT(point2%dim == -3, 'plane%intersect(...)')
      
      CALL line1%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      point2=plane1%intersectLine(line1)
      bool = .NOT.(point2%dim /= 3 .OR. ANY(.NOT.(point2%coord .APPROXEQ. 0.5_SRK)))
      ASSERT(bool, 'plane1%intersect(...)')

      !Test for bad input
      CALL line1%clear()
      point2=plane1%intersectLine(line1)
      ASSERT(point2%dim == -1, 'plane%intersect(...)')
      
      CALL line2%clear
      CALL line2%p1%init(COORD=(/-0.5_SRK,2.5_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,2.5_SRK/))
      point2=plane1%intersectLine(line2)
      ASSERT(point2%dim == -1, 'plane%intersect(...)')
      
      CALL plane1%clear()
      point2=plane1%intersectLine(line2)
      ASSERT(point2%dim == -1, 'plane%intersect(...)')
      
      !Test for equivalence operation
      COMPONENT_TEST('OPERATOR(==)')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      n=(/1.0_SRK,1.0_SRK,1.0_SRK/)
      CALL plane1%set(n,point)
      plane2=plane1
      ASSERT(plane1 == plane2,'plane equivalence')
      CALL plane2%set((/1.0_SRK,0.0_SRK,1.0_SRK/),point)
      ASSERT(.NOT.(plane1 == plane2),'plane non-equivalence')
      
#ifdef __GFORTRAN__
      WRITE(*,*) 'ELEMENTAL METHODS FOR NON-SCALAR BASE OBJECTS NOT YET SUPPORTED BY COMPILER'
#else      
      COMPONENT_TEST('Elemental %set()')
      CALL plane1%set((/1.0_SRK,1.0_SRK,1.0_SRK/),point)
      planes=plane1
      CALL line1%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      points=planes%intersectLine(line1)
      bool = .NOT.(points(1)%dim /= 3 .OR. ANY(.NOT.(points(1)%coord .APPROXEQ. 0.5_SRK)) .OR. &
                   points(2)%dim /= 3 .OR. ANY(.NOT.(points(2)%coord .APPROXEQ. 0.5_SRK)))
      ASSERT(bool, 'planes%intersect(...)')
      
      COMPONENT_TEST('Elemental %clear()')
      CALL planes%clear()
      bool = .NOT.(ANY(planes(1)%n /= 0.0_SRK) .OR. planes(1)%v0%dim /= 0 .OR. &
                   ALLOCATED(planes(1)%v0%coord) .OR. ANY(planes(2)%n /= 0.0_SRK) .OR. &
                   planes(2)%v0%dim /= 0 .OR. ALLOCATED(planes(2)%v0%coord))
      ASSERT(bool, 'planes%clear()')
#endif
    ENDSUBROUTINE TestPlane

ENDPROGRAM testGeom_Plane
