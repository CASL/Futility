!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testGeom_Quadratic
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE Strings
USE Geom_Quadratic
USE Geom_Points
USE Geom_Line

IMPLICIT NONE

CREATE_TEST('QUADRATIC TYPE')
REGISTER_SUBTEST('CLEAR', testClear)
REGISTER_SUBTEST('INIT', testInit)
REGISTER_SUBTEST('AREA', testArea)
REGISTER_SUBTEST('INTERSECT LINE', testIntersectLine)
FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testClear()
  TYPE(QuadraticType) :: arc
  INTEGER(SIK) :: i
  arc%a = 1.0_SRK
  arc%b = 1.0_SRK
  arc%theta = 1.0_SRK
  arc%shift_x = 1.0_SRK
  arc%shift_y = 1.0_SRK
  DO i=1,3
    CALL arc%points(i)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  ENDDO

  CALL arc%clear()
  ASSERT(arc%a == 0.0, "Value not cleared")
  ASSERT(arc%b == 0.0, "Value not cleared")
  ASSERT(arc%theta == 0.0, "Value not cleared")
  ASSERT(arc%shift_x == 0.0, "Value not cleared")
  ASSERT(arc%shift_y == 0.0, "Value not cleared")
  DO i=1,3
    ASSERT(arc%points(i)%dim == 0, "Value not cleared")
    ASSERT(.NOT. ALLOCATED(arc%points(i)%coord), "Value not cleared")
  ENDDO                                  
ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
SUBROUTINE testInit()
  TYPE(QuadraticType) :: arc
  REAL(SDK), PARAMETER :: PI=3.14159265358979311599796346854
  REAL(SDK) :: theta, rotation_mat(2,2)
  TYPE(PointType) :: p1, p2, p3, p1s, p2s, p3s, p11
  INTEGER(SIK) :: i

  COMPONENT_TEST("No rotation or shift needed")
  ! y = -x^2 + 2x
  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL arc%set(p1, p2, p3)
  ASSERT(arc%a        .APPROXEQA. -1.0_SRK, "Wrong a")
  ASSERT(arc%b        .APPROXEQA.  2.0_SRK, "Wrong b")
  ASSERT(arc%theta    .APPROXEQA.  0.0_SRK, "Wrong theta")
  ASSERT(arc%shift_x  .APPROXEQA.  0.0_SRK, "Wrong shift_x")
  ASSERT(arc%shift_y  .APPROXEQA.  0.0_SRK, "Wrong shift_y")
  ASSERT(arc%points(1) == p1, "Point assigned incorrectly")
  ASSERT(arc%points(2) == p2, "Point assigned incorrectly")
  ASSERT(arc%points(3) == p3, "Point assigned incorrectly")

  COMPONENT_TEST("Rotate i*pi/4, i = 1,8 ")
  DO i = 1,8
    p1s = p1; p2s = p2; p3s = p3;
    theta = i*PI/4
    rotation_mat(1,:) = (/COS(theta), -SIN(theta)/)
    rotation_mat(2,:) = (/SIN(theta), COS(theta)/)
    p1s%coord = MATMUL(rotation_mat, p1s%coord)
    p2s%coord = MATMUL(rotation_mat, p2s%coord)
    p3s%coord = MATMUL(rotation_mat, p3s%coord)
    ! y = -x^2 + 2x, but rotated by i*pi/4
    CALL arc%set(p1s, p2s, p3s)
    ASSERT(ABS(arc%a        +  1.0_SRK) < 1.0E-6, "Wrong a")
    ASSERT(ABS(arc%b        -  2.0_SRK) < 1.0E-6, "Wrong b")
    IF( i < 6) THEN
      ASSERT(ABS(arc%theta    -  i*PI/4 ) < 1.0E-6, "Wrong theta")
    ELSE
      ASSERT(ABS(arc%theta    -  (i-8)*PI/4 ) < 1.0E-6, "Wrong theta")
    ENDIF
    ASSERT(ABS(arc%shift_x  -  0.0_SRK) < 1.0E-6, "Wrong shift_x")
    ASSERT(ABS(arc%shift_y  -  0.0_SRK) < 1.0E-6, "Wrong shift_y")
    ASSERT(arc%points(1) == p1s, "Point assigned incorrectly")
    ASSERT(arc%points(2) == p2s, "Point assigned incorrectly")
    ASSERT(arc%points(3) == p3s, "Point assigned incorrectly")
  ENDDO

  COMPONENT_TEST("Rotate i*pi/4, i = 1,8 and shift 1,1")
  CALL p11%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  DO i = 1,8
    p1s = p1; p2s = p2; p3s = p3;
    theta = i*PI/4
    rotation_mat(1,:) = (/COS(theta), -SIN(theta)/)
    rotation_mat(2,:) = (/SIN(theta), COS(theta)/)
    p1s%coord = MATMUL(rotation_mat, p1s%coord)
    p2s%coord = MATMUL(rotation_mat, p2s%coord)
    p3s%coord = MATMUL(rotation_mat, p3s%coord)
    p1s = p1s + p11; p2s = p2s + p11; p3s = p3s + p11;

    ! y = -x^2 + 2x, but rotated by i*pi/4 and shifted 1,1
    CALL arc%set(p1s, p2s, p3s)
    ASSERT(ABS(arc%a        +  1.0_SRK) < 1.0E-6, "Wrong a")
    ASSERT(ABS(arc%b        -  2.0_SRK) < 1.0E-6, "Wrong b")
    IF( i < 6) THEN
      ASSERT(ABS(arc%theta    -  i*PI/4 ) < 1.0E-6, "Wrong theta")
    ELSE
      ASSERT(ABS(arc%theta    -  (i-8)*PI/4 ) < 1.0E-6, "Wrong theta")
    ENDIF
    ASSERT(ABS(arc%shift_x  -  1.0_SRK) < 1.0E-6, "Wrong shift_x")
    ASSERT(ABS(arc%shift_y  -  1.0_SRK) < 1.0E-6, "Wrong shift_y")
    ASSERT(arc%points(1) == p1s, "Point assigned incorrectly")
    ASSERT(arc%points(2) == p2s, "Point assigned incorrectly")
    ASSERT(arc%points(3) == p3s, "Point assigned incorrectly")
  ENDDO

  COMPONENT_TEST("y=x^2 -2x with roatation and shift")
  CALL p3%clear()
  CALL p3%init(DIM=2, X=1.0_SRK, Y=-1.0_SRK)
  DO i = 1,8
    p1s = p1; p2s = p2; p3s = p3;
    theta = i*PI/4
    rotation_mat(1,:) = (/COS(theta), -SIN(theta)/)
    rotation_mat(2,:) = (/SIN(theta), COS(theta)/)
    p1s%coord = MATMUL(rotation_mat, p1s%coord)
    p2s%coord = MATMUL(rotation_mat, p2s%coord)
    p3s%coord = MATMUL(rotation_mat, p3s%coord)
    p1s = p1s + p11; p2s = p2s + p11; p3s = p3s + p11;

    ! y = x^2 - 2x, but rotated by i*pi/4 and shifted 1,1
    CALL arc%set(p1s, p2s, p3s)
    ASSERT(ABS(arc%a        -  1.0_SRK) < 1.0E-6, "Wrong a")
    ASSERT(ABS(arc%b        +  2.0_SRK) < 1.0E-6, "Wrong b")
    IF( i < 6) THEN
      ASSERT(ABS(arc%theta    -  i*PI/4 ) < 1.0E-6, "Wrong theta")
    ELSE
      ASSERT(ABS(arc%theta    -  (i-8)*PI/4 ) < 1.0E-6, "Wrong theta")
    ENDIF
    ASSERT(ABS(arc%shift_x  -  1.0_SRK) < 1.0E-6, "Wrong shift_x")
    ASSERT(ABS(arc%shift_y  -  1.0_SRK) < 1.0E-6, "Wrong shift_y")
    ASSERT(arc%points(1) == p1s, "Point assigned incorrectly")
    ASSERT(arc%points(2) == p2s, "Point assigned incorrectly")
    ASSERT(arc%points(3) == p3s, "Point assigned incorrectly")
  ENDDO
ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
SUBROUTINE testArea()
  TYPE(QuadraticType) :: arc
  REAL(SDK), PARAMETER :: PI=3.14159265358979311599796346854
  REAL(SDK) :: theta, rotation_mat(2,2)
  TYPE(PointType) :: p1, p2, p3, p1s, p2s, p3s, p11
  INTEGER(SIK) :: i

  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL p11%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  DO i = 1,8
    p1s = p1; p2s = p2; p3s = p3;
    theta = i*PI/4
    rotation_mat(1,:) = (/COS(theta), -SIN(theta)/)
    rotation_mat(2,:) = (/SIN(theta), COS(theta)/)
    p1s%coord = MATMUL(rotation_mat, p1s%coord)
    p2s%coord = MATMUL(rotation_mat, p2s%coord)
    p3s%coord = MATMUL(rotation_mat, p3s%coord)
    p1s = p1s + p11; p2s = p2s + p11; p3s = p3s + p11;

    ! y = -x^2 + 2x, but rotated by i*pi/4 and shifted 1,1
    CALL arc%set(p1s, p2s, p3s)
    ASSERT(ABS(arc%area() - 4.0/3.0) < 1.0E-6, "Point assigned incorrectly")
  ENDDO
ENDSUBROUTINE testArea
!
!-------------------------------------------------------------------------------
SUBROUTINE testIntersectLine()
  TYPE(QuadraticType) :: arc
  TYPE(LineType) :: line
  REAL(SDK), PARAMETER :: PI=3.14159265358979311599796346854
  REAL(SDK) :: theta, rotation_mat(2,2)
  TYPE(PointType) :: p1, p2, p3, p4, p5
  TYPE(PointType), ALLOCATABLE :: ipoints(:)
  INTEGER(SIK) :: i

  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)

  CALL p1%clear()
  CALL p2%clear()
  CALL p3%clear()
ENDSUBROUTINE testIntersectLine
ENDPROGRAM testGeom_Quadratic
