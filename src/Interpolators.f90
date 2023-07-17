!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!                          Futility Development Group                          !
!                             All rights reserved.                             !
!                                                                              !
! Futility is a jointly-maintained, open-source project between the University !
! of Michigan and Oak Ridge National Laboratory.  The copyright and license    !
! can be found in LICENSE.txt in the head directory of this repository.        !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief A Fortran 2003 module implementing interpolation routines for input
!>        arrays returning either interpolated values or a flat "extrapolation"
!>        when  requested interpolation is out of range. Right now only 1D, 2D,
!>        and 3D routines are available.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE InterpolatorsModule
#include "Futility_DBC.h"
USE Futility_DBC
USE IntrType
USE ArrayUtils
USE Search

IMPLICIT NONE
PRIVATE

PUBLIC :: Interp

!> @brief Interface to interpolate whatever is handed in.
!>
INTERFACE Interp
  !> @copybrief InterpolatorsModule::Interp_1D
  !> @copydetails InterpolatorsModule::Interp_1D
  MODULE PROCEDURE Interp_1D
  !> @copybrief InterpolatorsModule::Interp_2D
  !> @copydetails InterpolatorsModule::Interp_2D
  MODULE PROCEDURE Interp_2D
  !> @copybrief InterpolatorsModule::Interp_3D
  !> @copydetails InterpolatorsModule::Interp_3D
  MODULE PROCEDURE Interp_3D
ENDINTERFACE Interp
!
!===============================================================================
CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief This routine takes a 1D data "table" and linearly interpolates to find
!>        a desired point.
!> @param labels axis labels at which data points in table are defined
!> @param table contains data to be interpolated between defined at points
!>        given in labels
!> @param point point at which interpolation needs to be done to return a value
!> @param lextrap logical indicating if extrapolation should be performed if called for
!> @returns Interpolant interpolated data found through linear interpolation to the point point
FUNCTION Interp_1D(labels,table,point,lextrap) RESULT(Interpolant)
  REAL(SRK),INTENT(IN) :: labels(:)
  REAL(SRK),INTENT(IN) :: table(:)
  REAL(SRK),INTENT(IN) :: point
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: lextrap
  REAL(SRK) :: Interpolant,f(2),t(2)
  LOGICAL(SBK) :: canExtrapolate

  REQUIRE(SIZE(labels) > 1)
  REQUIRE(SIZE(labels) == SIZE(table))
  REQUIRE(isStrictlyIncDec(labels))

  canExtrapolate=.FALSE.
  IF(PRESENT(lextrap)) THEN
    canExtrapolate=lextrap
  ENDIF
  
  CALL Get_points_and_weights(labels,table,point,f,t,canExtrapolate)
  Interpolant=f(1)*t(1)+f(2)*t(2)

ENDFUNCTION Interp_1D
!
!-------------------------------------------------------------------------------
!> @brief This routine takes a 2D data table and linearly interpolates to find
!>        a desired point.
!> @param labels1 axis labels at which data points in table are defined for 1st dimension
!> @param labels2 axis labels at which data points in table are defined for 2nd dimension
!> @param table contains data to be interpolated between defined at points
!>        given in labels1 and labels2
!> @param point point at which interpolation needs to be done to return a value
!> @param lextrap logical indicating if extrapolation should be performed if called for
!> @returns Interpolant interpolated data found through linear interpolation to the point point
FUNCTION Interp_2D(labels1,labels2,table,point,lextrap) RESULT(Interpolant)
  REAL(SRK),INTENT(IN) :: labels1(:)
  REAL(SRK),INTENT(IN) :: labels2(:)
  REAL(SRK),INTENT(IN) :: table(:,:)
  REAL(SRK),INTENT(IN) :: point(:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: lextrap
  REAL(SRK) :: Interpolant,f(2),t(2),Interp1D(2)
  INTEGER(SIK) :: j,i_p(2),j_p(2)
  LOGICAL(SBK) :: canExtrapolate

  REQUIRE(SIZE(labels1) > 0)
  REQUIRE(SIZE(labels2) > 0)
  REQUIRE(SIZE(labels1) == SIZE(table(:,1)))
  REQUIRE(SIZE(labels2) == SIZE(table(1,:)))
  REQUIRE(isStrictlyIncDec(labels1))
  REQUIRE(isStrictlyIncDec(labels2))

  IF(SIZE(labels1) == 1) THEN
    Interpolant=Interp_1D(labels2, table(1,:),point(2),lextrap)
  ELSEIF(SIZE(labels2) == 1) THEN
    Interpolant=Interp_1D(labels1, table(:,1),point(1),lextrap)
  ELSE
    canExtrapolate=.FALSE.
    IF(PRESENT(lextrap)) THEN
      canExtrapolate=lextrap
    ENDIF
    !Get interval in labels around the point
    CALL Get_interval(labels1,point(1),i_p)
    CALL Get_interval(labels2,point(2),j_p)
    !Interpolate w.r.t labels 1
    DO j=1,2
      CALL Get_points_and_weights(labels1,table(:,j_p(j)),point(1),f,t,canExtrapolate)
      Interp1D(j)=f(1)*t(1)+f(2)*t(2)
    ENDDO
    !Interpolate w.r.t labels 2
    CALL Get_points_and_weights(labels2(j_p(1):j_p(2)),Interp1D,point(2),f,t,canExtrapolate)
    Interpolant=f(1)*t(1)+f(2)*t(2)
  ENDIF
ENDFUNCTION Interp_2D
!
!-------------------------------------------------------------------------------
!> @brief This routine takes a 3D data table and linearly interpolates to find
!>        a desired point.
!> @param labels1 axis labels at which data points in table are defined for 1st dimension
!> @param labels2 axis labels at which data points in table are defined for 2nd dimension
!> @param labels3 axis labels at which data points in table are defined for 3rd dimension
!> @param table contains data to be interpolated between defined at points
!>        given in labels1, labels2, and labels 3
!> @param point point at which interpolation needs to be done to return a value
!> @param lextrap logical indicating if extrapolation should be performed if called for
!> @returns Interpolant interpolated data found through linear interpolation to the point point
!>
FUNCTION Interp_3D(labels1,labels2,labels3,table,point,lextrap) RESULT(Interpolant)
  REAL(SRK),INTENT(IN) :: labels1(:)
  REAL(SRK),INTENT(IN) :: labels2(:)
  REAL(SRK),INTENT(IN) :: labels3(:)
  REAL(SRK),INTENT(IN) :: table(:,:,:)
  REAL(SRK),INTENT(IN) :: point(:)
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: lextrap
  REAL(SRK) :: Interpolant,f(2),t(2),Interp2D(2,2),Interp1D(2)
  INTEGER(SIK) :: j,k,i_p(2),j_p(2),k_p(2)
  LOGICAL(SBK) :: canExtrapolate

  REQUIRE(SIZE(labels1) > 0)
  REQUIRE(SIZE(labels2) > 0)
  REQUIRE(SIZE(labels3) > 0)
  REQUIRE(SIZE(labels1) == SIZE(table(:,1,1)))
  REQUIRE(SIZE(labels2) == SIZE(table(1,:,1)))
  REQUIRE(SIZE(labels3) == SIZE(table(1,1,:)))
  REQUIRE(isStrictlyIncDec(labels1))
  REQUIRE(isStrictlyIncDec(labels2))
  REQUIRE(isStrictlyIncDec(labels3))

  IF(SIZE(labels1) == 1) THEN
    Interpolant=Interp_2D(labels2,labels3,table(1,:,:),[point(2),point(3)],lextrap)
  ELSEIF(SIZE(labels2) == 1) THEN
    Interpolant=Interp_2D(labels1,labels3,table(:,1,:),[point(1),point(3)],lextrap)
  ELSEIF(SIZE(labels3) == 1) THEN
    Interpolant=Interp_2D(labels1,labels2,table(:,:,1),[point(1),point(2)],lextrap)
  ELSE
    canExtrapolate=.FALSE.
    IF(PRESENT(lextrap)) THEN
      canExtrapolate=lextrap
    ENDIF
    !Get interval in labels around the point
    CALL Get_interval(labels1,point(1),i_p)
    CALL Get_interval(labels2,point(2),j_p)
    CALL Get_interval(labels3,point(3),k_p)
    !Interpolate w.r.t labels 1
    DO k=1,2
      DO j=1,2
        CALL Get_points_and_weights(labels1,table(:,j_p(j),k_p(k)),point(1),f,t,canExtrapolate)
        Interp2D(j,k)=f(1)*t(1)+f(2)*t(2)
      ENDDO
    ENDDO
    !Interpolate w.r.t labels 2
    DO k=1,2
      CALL Get_points_and_weights(labels2(j_p(1):j_p(2)),Interp2D(:,k),point(2),f,t,canExtrapolate)
      Interp1D(k)=f(1)*t(1)+f(2)*t(2)
    ENDDO
    !Interpolate w.r.t labels 3
    CALL Get_points_and_weights(labels3(k_p(1):k_p(2)),Interp1D,point(3),f,t,canExtrapolate)
    Interpolant=f(1)*t(1)+f(2)*t(2)
  ENDIF
ENDFUNCTION Interp_3D
!
!-------------------------------------------------------------------------------
!> @brief This routine takes a 1D array of table labels (the points along a single
!>        dimension at which table entries are defined) and the interpolation point
!>        coordinate along that dimension and returns the weights and table
!>        values for linear interpolation
!> @param labels axis labels at which data points in table are defined
!> @param table  the 1-D table that contains values associated with labels
!> @param point point at which interpolation needs to be done to return a value
!> @param f weights associated with the points on either side of point
!> @param t table associated with the table values on either side of point
!> @param lextrap logical to perform extrapolation if neccessary
!>
SUBROUTINE Get_points_and_weights(labels,table,point,f,t,lextrap)
  REAL(SRK),INTENT(IN) :: labels(:),table(:)
  REAL(SRK),INTENT(IN) :: point
  LOGICAL(SBK),INTENT(IN),OPTIONAL :: lextrap
  REAL(SRK),INTENT(OUT) :: f(2),t(2)
  INTEGER(SIK) :: i_p,N_i
  LOGICAL(SBK) :: canExtrapolate
  N_i=SIZE(labels)

  canExtrapolate=.FALSE.
  IF(PRESENT(lextrap)) THEN
    canExtrapolate=lextrap
  ENDIF

  !Determine labels index(s) and weights
  f(:)=1.0_SRK
  IF(labels(1) < labels(2)) THEN
    !Ascending order
    i_p=getFirstGreater(labels,point)
    IF(i_p > 1 .AND. i_p < N_i+1) THEN
      f(1)=(labels(i_p)-point)/(labels(i_p)-labels(i_p-1))
      t(1)=table(i_p-1)
      t(2)=table(i_p)
    ELSEIF(i_p == 1) THEN
      t(1)=table(1)
      t(2)=table(2)
      IF(canExtrapolate) f(1)=(labels(2)-point)/(labels(2)-labels(1))
    ELSEIF(i_p == N_i+1) THEN
      t(1)=table(N_i)
      t(2)=table(N_i-1)
      IF(canExtrapolate) f(1)=(point-labels(N_i-1))/(labels(N_i)-labels(N_i-1))
    ENDIF
    f(2)=1.0_SRK-f(1)
  ELSE
    !Descending order
    i_p=getFirstGreaterEqual(labels,point)
    IF(i_p > 1 .AND. i_p < N_i+1) THEN
      t(1)=table(i_p-1)
      t(2)=table(i_p)
      f(2)=(labels(i_p-1)-point)/(labels(i_p-1)-labels(i_p))
    ELSEIF(i_p == 1) THEN
      t(1)=table(2)
      t(2)=table(1)
      IF(canExtrapolate) f(2)=(labels(2)-point)/(labels(2)-labels(1))
    ELSEIF(i_p == N_i+1) THEN
      t(1)=table(N_i-1)
      t(2)=table(N_i)
      IF(canExtrapolate) f(2)=(point-labels(N_i-1))/(labels(N_i)-labels(N_i-1))
    ENDIF
    f(1)=1.0_SRK-f(2)
  ENDIF
ENDSUBROUTINE Get_points_and_weights
!
!-------------------------------------------------------------------------------
!> @brief This routine gets the interval in labels that contains point.
!>        If the point lays outside of labels then the closest interval
!>        in labels is returned.
!> @param labels axis labels at which data points in table are defined
!> @param point point at which interpolation needs to be done to return a value
!> @param i_p index of the interval around point in labels
!>
SUBROUTINE  Get_interval(labels,point,i_p)
  REAL(SRK),INTENT(IN) :: labels(:)
  REAL(SRK),INTENT(IN) :: point
  INTEGER(SIK),INTENT(OUT) :: i_p(2)

  IF(labels(1) < labels(2)) THEN
    !Ascending order
    i_p(2)=getFirstGreater(labels,point)
  ELSE
    !Descending order
    i_p(2)=getFirstGreaterEqual(labels,point)
  ENDIF
  IF(i_p(2) == 1) i_p(2)=i_p(2)+1
  IF(i_p(2) > SIZE(labels)) i_p(2)=i_p(2)-1
  i_p(1)=i_p(2)-1
 
ENDSUBROUTINE Get_interval
!
ENDMODULE InterpolatorsModule
