!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM testGeom_QuadraticSegment
#include "UnitTest.h"
USE ISO_FORTRAN_ENV
USE UnitTest
USE IntrType
USE Strings
USE Geom_QuadraticSegment
USE Geom_Points
USE Geom_Line

IMPLICIT NONE

CREATE_TEST('QUADRATIC TYPE')
REGISTER_SUBTEST('CLEAR', testClear)
REGISTER_SUBTEST('INIT', testInit)
!REGISTER_SUBTEST('INTERSECT LINE', testIntersectLine)
FINALIZE_TEST()
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE testClear()
  TYPE(QuadraticSegment_2D) :: quad
  INTEGER(SIK) :: i
  DO i=1,3
    CALL quad%points(i)%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  ENDDO

  CALL quad%clear()
  DO i=1,3
    ASSERT(quad%points(i)%dim == 0, "Value not cleared")
    ASSERT(.NOT. ALLOCATED(quad%points(i)%coord), "Value not cleared")
  ENDDO
ENDSUBROUTINE testClear
!
!-------------------------------------------------------------------------------
SUBROUTINE testInit()
  TYPE(QuadraticSegment_2D) :: quad
  TYPE(PointType) :: p1, p2, p3

  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
  CALL p2%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
  CALL quad%set(p1, p2, p3)
  ASSERT(quad%points(1) == p1, "Point assigned incorrectly")
  ASSERT(quad%points(2) == p2, "Point assigned incorrectly")
  ASSERT(quad%points(3) == p3, "Point assigned incorrectly")
ENDSUBROUTINE testInit
!
!-------------------------------------------------------------------------------
!SUBROUTINE testIntersectLine()
!  TYPE(QuadraticSegment_2D) :: quad
!  TYPE(LineType) :: line
!  REAL(SDK), PARAMETER :: PI=3.14159265358979311599796346854
!  REAL(SDK) :: theta, rotation_mat(2,2)
!  TYPE(PointType) :: p11, p1, p2, p3, p4, p5, ipoints(2), p1s, p2s, p3s, p4s, &
!    p5s, p_soln, p_soln2
!  INTEGER(SIK) :: i,j
!  REAL(SRK) :: lines(4,8)
!
!  ! Array of lines to test
!  lines(:, 1) = (/0.0_SRK, 0.9999999_SRK, 2.0_SRK, 0.9999999_SRK/)    ! Tangent
!  lines(:, 2) = (/0.0_SRK, 0.5_SRK, 2.0_SRK, 0.5_SRK/)    ! 2 intersections
!  lines(:, 3) = (/0.0_SRK, 400.0_SRK, 1.0_SRK, 0.0_SRK/)  ! 1 intersection
!  lines(:, 4) = (/0.0_SRK, 3.0_SRK, 2.0_SRK, 3.0_SRK/)    ! no intersection
!  lines(:, 5) = (/3.0_SRK, 0.0_SRK, 3.0_SRK, 3.0_SRK/)    ! vertical, no intersection
!  lines(:, 6) = (/1.0_SRK, 0.0_SRK, 1.0_SRK, 2.0_SRK/)    ! vertical, 1 intersection
!  lines(:, 7) = (/0.0_SRK, 400.0_SRK, 1.0_SRK, 2.0_SRK/)  ! line intersects, segment does not
!  lines(:, 8) = (/2.0_SRK, -1.0_SRK, 2.0_SRK, 1.0_SRK/)  ! vertex intersection
!
!  CALL p1%init(DIM=2, X=0.0_SRK, Y=0.0_SRK)
!  CALL p2%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)
!  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
!  CALL p3%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
!  CALL p11%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
!
!  COMPONENT_TEST("Tangent")
!  j=1
!  CALL p4%init(DIM=2, X=lines(1,j), Y=lines(2,j))
!  CALL p5%init(DIM=2, X=lines(3,j), Y=lines(4,j))
!  DO i = 0,8
!    CALL p_soln%init(DIM=2, X=0.999683772234_SRK, Y=0.9999999_SRK)
!    p1s = p1; p2s = p2; p3s = p3; p4s = p4; p5s = p5;
!    theta = i*PI/4
!    rotation_mat(1,:) = (/COS(theta), -SIN(theta)/)
!    rotation_mat(2,:) = (/SIN(theta), COS(theta)/)
!    p1s%coord = MATMUL(rotation_mat, p1s%coord)
!    p2s%coord = MATMUL(rotation_mat, p2s%coord)
!    p3s%coord = MATMUL(rotation_mat, p3s%coord)
!    p4s%coord = MATMUL(rotation_mat, p4s%coord)
!    p5s%coord = MATMUL(rotation_mat, p5s%coord)
!    p_soln%coord = MATMUL(rotation_mat, p_soln%coord)
!    p1s = p1s + p11
!    p2s = p2s + p11
!    p3s = p3s + p11
!    p4s = p4s + p11
!    p5s = p5s + p11
!    p_soln = p_soln + p11
!    ! y = -x^2 + 2x, but rotated by i*pi/4 and shifted 1,1
!    CALL quad%set(p1s, p2s, p3s)
!    ! y = 1 for x in [0, 2], but transformed as above
!    CALL line%set(p4s, p5s)
!    CALL quad%intersectLine(line, ipoints)
!    ASSERT(ipoints(1)%dim == 1, "Should have found intersection")
!    ASSERT(ABS(ipoints(1)%coord(1) - p_soln%coord(1)) < 1.0E-6, "Wrong intersection")
!    ASSERT(ABS(ipoints(1)%coord(2) - p_soln%coord(2)) < 1.0E-6, "Wrong intersection")
!    CALL p_soln%clear()
!  ENDDO
!  CALL p4%clear()
!  CALL p5%clear()
!  CALL p_soln%clear()
!
!  COMPONENT_TEST("Two intersections")
!  j=2
!  CALL p4%init(DIM=2, X=lines(1,j), Y=lines(2,j))
!  CALL p5%init(DIM=2, X=lines(3,j), Y=lines(4,j))
!  DO i = 0,8
!    CALL p_soln%init(DIM=2, X=0.292893_SRK, Y=0.5_SRK)
!    CALL p_soln2%init(DIM=2, X=1.70711_SRK, Y=0.5_SRK)
!    p1s = p1; p2s = p2; p3s = p3; p4s = p4; p5s = p5;
!    theta = i*PI/4
!    rotation_mat(1,:) = (/COS(theta), -SIN(theta)/)
!    rotation_mat(2,:) = (/SIN(theta), COS(theta)/)
!    p1s%coord = MATMUL(rotation_mat, p1s%coord)
!    p2s%coord = MATMUL(rotation_mat, p2s%coord)
!    p3s%coord = MATMUL(rotation_mat, p3s%coord)
!    p4s%coord = MATMUL(rotation_mat, p4s%coord)
!    p5s%coord = MATMUL(rotation_mat, p5s%coord)
!    p_soln%coord = MATMUL(rotation_mat, p_soln%coord)
!    p_soln2%coord = MATMUL(rotation_mat, p_soln2%coord)
!    p1s = p1s + p11
!    p2s = p2s + p11
!    p3s = p3s + p11
!    p4s = p4s + p11
!    p5s = p5s + p11
!    p_soln = p_soln + p11
!    p_soln2 = p_soln2 + p11
!    ! y = -x^2 + 2x, but rotated by i*pi/4 and shifted 1,1
!    CALL quad%set(p1s, p2s, p3s)
!    ! y = 1 for x in [0, 2], but transformed as above
!    CALL line%set(p4s, p5s)
!    CALL quad%intersectLine(line, ipoints)
!    ASSERT(ipoints(1)%dim == 2, "Should have found intersection")
!    ASSERT(ABS(ipoints(1)%coord(1) - p_soln%coord(1)) < 1.0E-4, "Wrong intersection")
!    ASSERT(ABS(ipoints(1)%coord(2) - p_soln%coord(2)) < 1.0E-4, "Wrong intersection")
!    ASSERT(ABS(ipoints(2)%coord(1) - p_soln2%coord(1)) < 1.0E-4, "Wrong intersection")
!    ASSERT(ABS(ipoints(2)%coord(2) - p_soln2%coord(2)) < 1.0E-4, "Wrong intersection")
!    CALL p_soln%clear()
!    CALL p_soln2%clear()
!  ENDDO
!  CALL p4%clear()
!  CALL p5%clear()
!
!  COMPONENT_TEST("One intersection")
!  j=3
!  CALL p4%init(DIM=2, X=lines(1,j), Y=lines(2,j))
!  CALL p5%init(DIM=2, X=lines(3,j), Y=lines(4,j))
!  DO i = 0,8
!    CALL p_soln%init(DIM=2, X=0.9975_SRK, Y=0.99999375_SRK)
!    p1s = p1; p2s = p2; p3s = p3; p4s = p4; p5s = p5;
!    theta = i*PI/4
!    rotation_mat(1,:) = (/COS(theta), -SIN(theta)/)
!    rotation_mat(2,:) = (/SIN(theta), COS(theta)/)
!    p1s%coord = MATMUL(rotation_mat, p1s%coord)
!    p2s%coord = MATMUL(rotation_mat, p2s%coord)
!    p3s%coord = MATMUL(rotation_mat, p3s%coord)
!    p4s%coord = MATMUL(rotation_mat, p4s%coord)
!    p5s%coord = MATMUL(rotation_mat, p5s%coord)
!    p_soln%coord = MATMUL(rotation_mat, p_soln%coord)
!    p1s = p1s + p11
!    p2s = p2s + p11
!    p3s = p3s + p11
!    p4s = p4s + p11
!    p5s = p5s + p11
!    p_soln = p_soln + p11
!    ! y = -x^2 + 2x, but rotated by i*pi/4 and shifted 1,1
!    CALL quad%set(p1s, p2s, p3s)
!    ! y = 1 for x in [0, 2], but transformed as above
!    CALL line%set(p4s, p5s)
!    CALL quad%intersectLine(line, ipoints)
!    ASSERT(ipoints(1)%dim == 1, "Should have found intersection")
!    ASSERT(ABS(ipoints(1)%coord(1) - p_soln%coord(1)) < 1.0E-4, "Wrong intersection")
!    ASSERT(ABS(ipoints(1)%coord(2) - p_soln%coord(2)) < 1.0E-4, "Wrong intersection")
!    CALL p_soln%clear()
!  ENDDO
!  CALL p4%clear()
!  CALL p5%clear()
!
!  COMPONENT_TEST("No intersection")
!  j=4
!  CALL p4%init(DIM=2, X=lines(1,j), Y=lines(2,j))
!  CALL p5%init(DIM=2, X=lines(3,j), Y=lines(4,j))
!  DO i = 0,8
!    p1s = p1; p2s = p2; p3s = p3; p4s = p4; p5s = p5;
!    theta = i*PI/4
!    rotation_mat(1,:) = (/COS(theta), -SIN(theta)/)
!    rotation_mat(2,:) = (/SIN(theta), COS(theta)/)
!    p1s%coord = MATMUL(rotation_mat, p1s%coord)
!    p2s%coord = MATMUL(rotation_mat, p2s%coord)
!    p3s%coord = MATMUL(rotation_mat, p3s%coord)
!    p4s%coord = MATMUL(rotation_mat, p4s%coord)
!    p5s%coord = MATMUL(rotation_mat, p5s%coord)
!    p1s = p1s + p11
!    p2s = p2s + p11
!    p3s = p3s + p11
!    p4s = p4s + p11
!    p5s = p5s + p11
!    ! y = -x^2 + 2x, but rotated by i*pi/4 and shifted 1,1
!    CALL quad%set(p1s, p2s, p3s)
!    ! y = 1 for x in [0, 2], but transformed as above
!    CALL line%set(p4s, p5s)
!    CALL quad%intersectLine(line, ipoints)
!    ASSERT(ipoints(1)%dim == -3, "Should not have found intersection")
!  ENDDO
!  CALL p4%clear()
!  CALL p5%clear()
!
!  COMPONENT_TEST("Vertical, no intersection")
!  j=5
!  CALL p4%init(DIM=2, X=lines(1,j), Y=lines(2,j))
!  CALL p5%init(DIM=2, X=lines(3,j), Y=lines(4,j))
!  DO i = 0,8
!    p1s = p1; p2s = p2; p3s = p3; p4s = p4; p5s = p5;
!    theta = i*PI/4
!    rotation_mat(1,:) = (/COS(theta), -SIN(theta)/)
!    rotation_mat(2,:) = (/SIN(theta), COS(theta)/)
!    p1s%coord = MATMUL(rotation_mat, p1s%coord)
!    p2s%coord = MATMUL(rotation_mat, p2s%coord)
!    p3s%coord = MATMUL(rotation_mat, p3s%coord)
!    p4s%coord = MATMUL(rotation_mat, p4s%coord)
!    p5s%coord = MATMUL(rotation_mat, p5s%coord)
!    p1s = p1s + p11
!    p2s = p2s + p11
!    p3s = p3s + p11
!    p4s = p4s + p11
!    p5s = p5s + p11
!    ! y = -x^2 + 2x, but rotated by i*pi/4 and shifted 1,1
!    CALL quad%set(p1s, p2s, p3s)
!    ! y = 1 for x in [0, 2], but transformed as above
!    CALL line%set(p4s, p5s)
!    CALL quad%intersectLine(line, ipoints)
!    ASSERT(ipoints(1)%dim == -3, "Should not have found intersection")
!  ENDDO
!  CALL p4%clear()
!  CALL p5%clear()
!
!  COMPONENT_TEST("Vertical, one intersection")
!  j=6
!  CALL p4%init(DIM=2, X=lines(1,j), Y=lines(2,j))
!  CALL p5%init(DIM=2, X=lines(3,j), Y=lines(4,j))
!  DO i = 0,8
!    CALL p_soln%init(DIM=2, X=1.0_SRK, Y=1.0_SRK)
!    p1s = p1; p2s = p2; p3s = p3; p4s = p4; p5s = p5;
!    theta = i*PI/4
!    rotation_mat(1,:) = (/COS(theta), -SIN(theta)/)
!    rotation_mat(2,:) = (/SIN(theta), COS(theta)/)
!    p1s%coord = MATMUL(rotation_mat, p1s%coord)
!    p2s%coord = MATMUL(rotation_mat, p2s%coord)
!    p3s%coord = MATMUL(rotation_mat, p3s%coord)
!    p4s%coord = MATMUL(rotation_mat, p4s%coord)
!    p5s%coord = MATMUL(rotation_mat, p5s%coord)
!    p_soln%coord = MATMUL(rotation_mat, p_soln%coord)
!    p1s = p1s + p11
!    p2s = p2s + p11
!    p3s = p3s + p11
!    p4s = p4s + p11
!    p5s = p5s + p11
!    p_soln = p_soln + p11
!    ! y = -x^2 + 2x, but rotated by i*pi/4 and shifted 1,1
!    CALL quad%set(p1s, p2s, p3s)
!    ! y = 1 for x in [0, 2], but transformed as above
!    CALL line%set(p4s, p5s)
!    CALL quad%intersectLine(line, ipoints)
!    ASSERT(ipoints(1)%dim == 1, "Should have found intersection")
!    ASSERT(ABS(ipoints(1)%coord(1) - p_soln%coord(1)) < 1.0E-6, "Wrong intersection")
!    ASSERT(ABS(ipoints(1)%coord(2) - p_soln%coord(2)) < 1.0E-6, "Wrong intersection")
!    CALL p_soln%clear()
!  ENDDO
!  CALL p4%clear()
!  CALL p5%clear()
!
!  COMPONENT_TEST("Line intersects, segment does not")
!  j=7
!  CALL p4%init(DIM=2, X=lines(1,j), Y=lines(2,j))
!  CALL p5%init(DIM=2, X=lines(3,j), Y=lines(4,j))
!  DO i = 0,8
!    p1s = p1; p2s = p2; p3s = p3; p4s = p4; p5s = p5;
!    theta = i*PI/4
!    rotation_mat(1,:) = (/COS(theta), -SIN(theta)/)
!    rotation_mat(2,:) = (/SIN(theta), COS(theta)/)
!    p1s%coord = MATMUL(rotation_mat, p1s%coord)
!    p2s%coord = MATMUL(rotation_mat, p2s%coord)
!    p3s%coord = MATMUL(rotation_mat, p3s%coord)
!    p4s%coord = MATMUL(rotation_mat, p4s%coord)
!    p5s%coord = MATMUL(rotation_mat, p5s%coord)
!    p1s = p1s + p11
!    p2s = p2s + p11
!    p3s = p3s + p11
!    p4s = p4s + p11
!    p5s = p5s + p11
!    ! y = -x^2 + 2x, but rotated by i*pi/4 and shifted 1,1
!    CALL quad%set(p1s, p2s, p3s)
!    ! y = 1 for x in [0, 2], but transformed as above
!    CALL line%set(p4s, p5s)
!    CALL quad%intersectLine(line, ipoints)
!    ASSERT(ipoints(1)%dim == -3, "Should have found intersection")
!  ENDDO
!  CALL p4%clear()
!  CALL p5%clear()
!
!  COMPONENT_TEST("Vertex intersection")
!  j=8
!  CALL p4%init(DIM=2, X=lines(1,j), Y=lines(2,j))
!  CALL p5%init(DIM=2, X=lines(3,j), Y=lines(4,j))
!  DO i = 0,8
!    CALL p_soln%init(DIM=2, X=2.0_SRK, Y=0.0_SRK)   
!    p1s = p1; p2s = p2; p3s = p3; p4s = p4; p5s = p5;
!    theta = i*PI/4
!    rotation_mat(1,:) = (/COS(theta), -SIN(theta)/)
!    rotation_mat(2,:) = (/SIN(theta), COS(theta)/)
!    p1s%coord = MATMUL(rotation_mat, p1s%coord)
!    p2s%coord = MATMUL(rotation_mat, p2s%coord)
!    p3s%coord = MATMUL(rotation_mat, p3s%coord)
!    p4s%coord = MATMUL(rotation_mat, p4s%coord)
!    p5s%coord = MATMUL(rotation_mat, p5s%coord)
!    p_soln%coord = MATMUL(rotation_mat, p_soln%coord)
!    p1s = p1s + p11
!    p2s = p2s + p11
!    p3s = p3s + p11
!    p4s = p4s + p11
!    p5s = p5s + p11
!    p_soln = p_soln + p11
!    ! y = -x^2 + 2x, but rotated by i*pi/4 and shifted 1,1
!    CALL quad%set(p1s, p2s, p3s)
!    ! y = 1 for x in [0, 2], but transformed as above
!    CALL line%set(p4s, p5s)
!    CALL quad%intersectLine(line, ipoints)
!    ASSERT(ipoints(1)%dim == 1, "Should have found intersection")
!    ASSERT(ABS(ipoints(1)%coord(1) - p_soln%coord(1)) < 1.0E-6, "Wrong intersection")
!    ASSERT(ABS(ipoints(1)%coord(2) - p_soln%coord(2)) < 1.0E-6, "Wrong intersection")
!    CALL p_soln%clear()
!  ENDDO
!  CALL p4%clear()
!  CALL p5%clear()
!ENDSUBROUTINE testIntersectLine
ENDPROGRAM testGeom_QuadraticSegment
