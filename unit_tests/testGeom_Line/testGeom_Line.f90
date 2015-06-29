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
PROGRAM testGeom_Line
#include "UnitTest.h"
  USE ISO_FORTRAN_ENV  
  USE UnitTest
  USE IntrType
  USE Constants_Conversion
  USE ParameterLists
  USE Geom
  
  IMPLICIT NONE
  
  TYPE(PointType) :: point,point2,point3
  TYPE(PointType) :: points(2),points2(2),points3(2)
  TYPE(LineType) :: line1,line2,lines(2),dis,diss(2)
  INTEGER(SIK) :: ldim(2),i,ioerr
  REAL(SRK) :: d,mu1,mu2,s(2)
  LOGICAL(SBK) :: bool
  
  CREATE_TEST('Test Geom')
  CALL eParams%setQuietMode(.TRUE.)
  CALL eParams%setStopOnError(.FALSE.)
  
  REGISTER_SUBTEST('Test Lines',TestLine)

  FINALIZE_TEST()
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE TestLine
      TYPE(ParamType) :: params
!The LineType constructor allocates sp and ep, care should
!be taken not to code memory leaks. This is why clearPoints() exists.
      COMPONENT_TEST('%clear()')
      !Initialize by hand
      CALL line1%p1%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL line1%p2%init(DIM=2,X=1.0_SRK,Y=1.0_SRK)
!
!Test clear
      CALL line1%clear()
      bool=.NOT.(line1%p1%dim /= 0 .OR. line1%p2%dim /= 0 .OR. &
                   ALLOCATED(line1%p1%coord) .OR. ALLOCATED(line1%p2%coord))
      ASSERT(bool, 'line1%clear()')
      
      !Redundant call to clear
      CALL line1%clear()
!      
!Test init
      COMPONENT_TEST('%init()')
      CALL point2%init(DIM=3,X=0.1_SRK,Y=0.2_SRK,Z=0.3_SRK)
      CALL point3%init(DIM=3,X=0.4_SRK,Y=0.5_SRK,Z=0.6_SRK)
      CALL line1%set(point2,point3)
      bool=.NOT.(line1%p1%dim /= 3 .OR. line1%p2%dim /= 3 .OR. &
                 line1%p1%coord(1) /= 0.1_SRK .OR. line1%p1%coord(2) /= 0.2_SRK .OR. &
                 line1%p1%coord(3) /= 0.3_SRK .OR. line1%p2%coord(1) /= 0.4_SRK .OR. &
                 line1%p2%coord(2) /= 0.5_SRK .OR. line1%p2%coord(3) /= 0.6_SRK)
      ASSERT(bool, 'line1%set(...)')
      CALL line1%clear()
      
      !Redundant call to test input error
      CALL line1%set(point,point2)
      bool = .NOT.(line1%p1%dim /= 0 .OR. line1%p2%dim /= 0 .OR. &
                   ALLOCATED(line1%p1%coord) .OR. ALLOCATED(line1%p2%coord))
      ASSERT(bool, 'line1%set(...)')
      CALL line1%clear()
!
!Test getDim
      COMPONENT_TEST('%getDim()')
      CALL line1%set(point2,point3)
      ASSERT(line1%getDim() == 3, 'line1%getDim()')
      CALL line1%clear()
      
      !Redundant call to test input error
      ASSERT(line1%getDim() == 0, 'line1%getDim()')
!      
!Test length
      COMPONENT_TEST('%length()')
      CALL line1%set(point2,point3)
      d=line1%length()
      ASSERT(d .APPROXEQ. 0.519615242270663_SRK, 'line1%length()')
!
!Test midpoint
      COMPONENT_TEST('%midpoint()')
      point=line1%midpoint()
      bool = .NOT.(point%dim /= 3 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.25_SRK) .OR. &
                   .NOT.(point%coord(2) .APPROXEQ. 0.35_SRK) .OR. &
                   .NOT.(point%coord(3) .APPROXEQ. 0.45_SRK))
      ASSERT(bool, 'line1%midpoint()')
!
!Test intersect
      !2D
      COMPONENT_TEST('%intersectLine()')
      CALL line1%clear()
      CALL line1%p1%init(DIM=2,X=0.0_SRK,Y=0.0_SRK)
      CALL line1%p2%init(DIM=2,X=0.0_SRK,Y=1.0_SRK)
      CALL line2%p1%init(DIM=2,X=1.0_SRK,Y=0.0_SRK)
      CALL line2%p2%init(DIM=2,X=1.0_SRK,Y=1.0_SRK)
      !Overlap
      point=line1%intersectLine(line1)
      ASSERT(point%dim == -2, 'line1%intersectLine(...)')
      !disjoint
      point=line1%intersectLine(line2)
      ASSERT(point%dim == -3, 'line1%intersectLine(...)')
      !Normal
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/0.0_SRK,1.0_SRK/))
      CALL line2%p1%init(COORD=(/-0.5_SRK,0.5_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.5_SRK/))
      point=line1%intersectLine(line2)
      bool = .NOT.(point%dim /= 2 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.0_SRK) .OR. &
                   .NOT.(point%coord(2) .APPROXEQ. 0.5_SRK))
      ASSERT(bool, 'line1%intersectLine(...)')
      !3D
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,1.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.0_SRK,1.0_SRK/))
      !Overlap
      point=line1%intersectLine(line1)
      ASSERT(point%dim == -2, 'line1%intersectLine(...)')
      !disjoint
      point=line1%intersectLine(line2)
      ASSERT(point%dim == -3, 'line1%intersectLine(...)')
      !Normal
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      CALL line2%p1%init(COORD=(/1.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line2%p2%init(COORD=(/0.0_SRK,1.0_SRK,1.0_SRK/))
      point=line1%intersectLine(line2)
      bool = .NOT.(point%dim /= 3 .OR. .NOT.(point%coord(1) .APPROXEQ. 0.5_SRK) .OR. &
                   .NOT.(point%coord(2) .APPROXEQ. 0.5_SRK) .OR. &
                   .NOT.(point%coord(3) .APPROXEQ. 0.5_SRK))
      ASSERT(bool, 'line1%intersectLine(...)')

      !Redundant calls to test error checking.
      CALL line2%clear()
      point=line1%intersectLine(line2) !mismatched dimensions
      ASSERT(point%dim == -1, 'point=line1%intersectLine(line2)')
      CALL line1%clear()
      CALL line1%p1%init(DIM=1,X=0.5_SRK)
      CALL line1%p2%init(DIM=1,X=0.0_SRK)
      point=line1%intersectLine(line1)
      ASSERT(point%dim == -2, '1-D line1%intersectLine(...)')
      
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/0.0_SRK,1.0_SRK/))
      CALL line2%p1%init(COORD=(/-0.5_SRK,2.5_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,2.5_SRK/))
      point=line1%intersectLine(line2)
      ASSERT(point%dim == -3, 'line1%intersectLine(...)')
      
!Test for %distance2Line(...)
      COMPONENT_TEST('%distance2Line()')
      !Line dimension error check
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/0.0_SRK,1.0_SRK/))
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      bool = .NOT.(dis%p1%dim /= -1 .AND. dis%p2%dim /= -1)
      ASSERT(bool, 'line2%distance2Line(...)')
      !Line input check
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      bool = .NOT.(dis%p1%dim /= -1 .AND. dis%p2%dim /= -1)
      ASSERT(bool, 'line2%distance2Line(...)')
      !Overlap
      CALL line1%clear()
      CALL dis%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      bool = .NOT.(dis%p1%dim /= -2 .AND. dis%p2%dim /= -2)
      ASSERT(bool, 'line2%distance2Line(...)')
      !Parallel
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      bool = .NOT.(dis%p1%dim /= -3 .AND. dis%p2%dim /= -3)
      ASSERT(bool, 'line2%distance2Line(...)')
      !Normal with interscetion
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/0.0_SRK,0.0_SRK,1.0_SRK/))
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      bool = .NOT.(dis%getDim() /= 3 .OR. mu1 /= 0._SRK .OR. mu2 /= -1._SRK &
                   .OR. dis%p1 /= line2%p1 .OR. dis%length() > EPSREAL)
      ASSERT(bool, 'line2%distance2Line(...)')
      !Normal without interscetion
      CALL line1%clear()
      CALL line2%clear()
      CALL dis%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/0.0_SRK,1.0_SRK,0.0_SRK/))
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,0.0_SRK,-1.0_SRK/))
      CALL line2%distance2Line(line1,dis,mu1,mu2)
      bool = .NOT.(dis%getDim() /= 3 .OR. mu1 /= 0._SRK .OR. mu2 /= 0._SRK &
                   .OR. dis%p1 /= line2%p1 .OR. dis%p2 /= line1%p1)
      ASSERT(bool, 'line2%distance2Line(...)')
      
!Test for %distance2Point(...)
      COMPONENT_TEST('%distance2Point()')
      CALL line1%clear()
      CALL point%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK/))
      CALL point%init(COORD=(/-0.5_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 0.25_SRK
      ASSERT(bool, '1-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 0.0_SRK
      ASSERT(bool, '1-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/1.5_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 0.25_SRK
      ASSERT(bool, '1-D line1%distance2Point(...)')
      
      CALL line1%clear()
      CALL point%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK/))
      CALL point%init(COORD=(/-0.5_SRK/))
      ASSERT(line1%distance2Point(point) == -1.0_SRK, '2-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK/))
      ASSERT(line1%distance2Point(point) .APPROXEQ. 0.0_SRK, '2-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.5_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 0.125_SRK
      ASSERT(bool, '2-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,0.5_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 1.25_SRK
      ASSERT(bool, '2-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/-0.5_SRK,-1.0_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 1.25_SRK
      ASSERT(bool, '2-D line1%distance2Point(...)')
      
      CALL line1%clear()
      CALL point%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 0.0_SRK
      ASSERT(bool, '3-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.0_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 0.1666666666666667_SRK
      ASSERT(bool, '3-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/1.0_SRK,1.0_SRK,2.0_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 1.0_SRK
      ASSERT(bool, '3-D line1%distance2Point(...)')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,0.0_SRK,-1.0_SRK/))
      bool = line1%distance2Point(point) .APPROXEQ. 1.0_SRK
      ASSERT(bool, '3-D line1%distance2Point(...)')
      
!Test pointIsLeft
      COMPONENT_TEST('%pointIsLeft()')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,1.0_SRK,-1.0_SRK/))
      ASSERT(line1%pointIsLeft(point),'Point (0.0,1.0,-1.0)')
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,1.0_SRK,-10.0_SRK/))
      ASSERT(.NOT.line1%pointIsLeft(point),'Point (2.0,1.0,-10.0)')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      ASSERT(.NOT.line1%pointIsLeft(point),'Point (0.5,0.5,0.5)')
      
!Test pointIsLeft
      CALL point%clear()
      CALL point%init(COORD=(/2.0_SRK,1.0_SRK,-10.0_SRK/))
      COMPONENT_TEST('%pointIsRight()')
      ASSERT(line1%pointIsRight(point),'Point (2.0,1.0,-10.0)')
      CALL point%clear()
      CALL point%init(COORD=(/0.0_SRK,1.0_SRK,-1.0_SRK/))
      ASSERT(.NOT.line1%pointIsRight(point),'Point (0.0,1.0,-1.0)')
      CALL point%clear()
      CALL point%init(COORD=(/0.5_SRK,0.5_SRK,0.5_SRK/))
      ASSERT(.NOT.line1%pointIsLeft(point),'Point (0.5,0.5,0.5)')
      
      !Test for equivalence operation
      COMPONENT_TEST('OPERATOR(==)')
      line2=line1
      ASSERT(line1 == line2,'3-D line equivalence')
      CALL line1%clear()
      CALL line2%clear()
      CALL line1%p1%init(COORD=(/0.0_SRK,0.0_SRK/))
      CALL line1%p2%init(COORD=(/1.0_SRK,1.0_SRK/))
      line2=line1
      ASSERT(line1 == line2,'2-D line equivalence')
      CALL line2%clear()
      CALL line2%p1%init(COORD=(/0.0_SRK,0.0_SRK,0.0_SRK/))
      CALL line2%p2%init(COORD=(/1.0_SRK,1.0_SRK,1.0_SRK/))
      ASSERT(.NOT.(line1 == line2),'2-D and 3-D line non-equivalence')
      
#ifdef __GFORTRAN__
      WRITE(*,*) 'ELEMENTAL METHODS FOR NON-SCALAR BASE OBJECTS NOT YET SUPPORTED BY COMPILER'
#else
      CALL points2%clear()
      CALL points3%clear()
      CALL points2(1)%init(DIM=3,X=0.5_SRK,Y=0.6_SRK,Z=0.7_SRK)
      CALL points2(2)%init(DIM=2,X=0.5_SRK,Y=0.6_SRK)
      points3=points2
      points3(1)%coord=points3(1)%coord+0.1_SRK
      points3(2)%coord=points3(2)%coord+0.1_SRK
!This no longer works with Intel 12
      COMPONENT_TEST('Elemental %set()')
      CALL lines%set(points2,points3)
!      CALL lines(1)%set(points2(1),points3(1))
!      CALL lines(2)%set(points2(2),points3(2))
      bool = .NOT.(lines(1)%p1%dim /= 3 .OR. lines(1)%p2%dim /= 3 .OR. & 
                  ANY(.NOT.(lines(1)%p1%coord .APPROXEQ. (/0.5_SRK,0.6_SRK,0.7_SRK/))) .OR. &
                  ANY(.NOT.(lines(1)%p2%coord .APPROXEQ. (/0.6_SRK,0.7_SRK,0.8_SRK/))) .OR. &
                  lines(2)%p1%dim /= 2 .OR. lines(2)%p2%dim /= 2 .OR. &
                  ANY(.NOT.(lines(2)%p1%coord .APPROXEQ. (/0.5_SRK,0.6_SRK/))) .OR. &
                  ANY(.NOT.(lines(2)%p2%coord .APPROXEQ. (/0.6_SRK,0.7_SRK/))))
      ASSERT(bool, 'lines%set(...)')
      
      COMPONENT_TEST('Elemental %getDim()')
      ldim=lines%getDim()
      bool = .NOT.(ldim(1) /= 3 .OR. ldim(2) /= 2)
      ASSERT(bool, 'lines%getDim()')
      
      COMPONENT_TEST('Elemental %length()')
      s=lines%length()
      bool = .NOT.(.NOT.(s(1) .APPROXEQ. 0.173205080756888_SRK) .OR. &
                   .NOT.(s(2) .APPROXEQ. 0.141421356237309_SRK))
      ASSERT(bool, 'lines%length()')
      
      COMPONENT_TEST('Elemental %midPoint()')
      points=lines%midPoint()
      bool = .NOT.(ANY(.NOT.(points(1)%coord .APPROXEQ. (/0.55_SRK,0.65_SRK,0.75_SRK/))) .OR. &
                   ANY(.NOT.(points(2)%coord .APPROXEQ. (/0.55_SRK,0.65_SRK/))))
      ASSERT(bool, 'lines%midPoint()')
      
      COMPONENT_TEST('Elemental %intersectLine()')
      points=lines%intersectLine(lines)
      bool = .NOT.(points(1)%dim /= -2 .OR. points(2)%dim /= -2)
      ASSERT(bool, 'lines%intersectLine(...)')
      
     !diss=lines%distance2Line(lines)
     ! IF(diss(1)%p(1)%dim /= -2 .OR. diss(2)%p(1)%dim /= -2) THEN
     !   WRITE(*,*) 'lines%distance2Line(...) FAILED!'
     !   STOP 666
     ! ELSE
     !   WRITE(*,*) '  Passed: lines%distance2Line(...)'
     ! ENDIF
     !
      COMPONENT_TEST('Elemental %distance2Point()')
      points=lines%midPoint()
      s=lines%distance2Point(points)
      bool = .NOT.(ANY(.NOT.(s .APPROXEQ. 0.0_SRK)))
      ASSERT(bool, 'lines%distance2Point(...)')
      
      COMPONENT_TEST('Elemental %clear()')
      CALL lines%clear()
      bool = .NOT.((lines(1)%p1%dim /= 0) .OR. (lines(1)%p2%dim /= 0) .OR. &
                   (lines(2)%p1%dim /= 0) .OR. (lines(2)%p2%dim /= 0) .OR. &
                   ALLOCATED(lines(1)%p1%coord) .OR. ALLOCATED(lines(1)%p2%coord) .OR. &
                   ALLOCATED(lines(2)%p1%coord) .OR. ALLOCATED(lines(2)%p2%coord))
      ASSERT(bool, 'lines%clear(...)')
#endif
    ENDSUBROUTINE TestLine

ENDPROGRAM testGeom_Line
