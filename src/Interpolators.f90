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
MODULE Interpolators

USE IntrType
IMPLICIT NONE

PUBLIC :: Interp

!> @brief Generic interface to interpolate whatever is handed in.
!>
INTERFACE Interp
  !> @copybrief Interpolators::Interp_1D
  !> @copydetails Interpolators::Interp_1D
  MODULE PROCEDURE Interp_1D
  !> @copybrief Interpolators::Interp_2D
  !> @copydetails Interpolators::Interp_2D
  MODULE PROCEDURE Interp_2D
  !> @copybrief Interpolators::Interp_3D
  !> @copydetails Interpolators::Interp_3D
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
!> @param axis_p point at which interpolation needs to be done to return a value
!> @param data_p interpolated data found through linear interpolation to the point axis_p
FUNCTION Interp_1D(labels,table,axis_p) RESULT(data_p)
  REAL(SRK),INTENT(IN) :: labels(:),table(:),axis_p
  REAL(SRK) :: data_p,f(2)
  INTEGER(SIK) :: i,N_i,i_p

  CALL Get_points_and_weights(labels,axis_p,f,i_p,N_i)

  !Get interpolated value
  data_p=0.0_SRK
  DO i=MAX(i_p-1,1),MIN(i_p,N_i)
    data_p=data_p+f(i-i_p+2)*table(i)
  ENDDO

ENDFUNCTION Interp_1D
!
!-------------------------------------------------------------------------------
!> @brief This routine takes a 2D data table and linearly interpolates to find
!>        a desired point.
!> @param labels1 axis labels at which data points in table are defined for 1st dimension
!> @param labels2 axis labels at which data points in table are defined for 2nd dimension
!> @param table contains data to be interpolated between defined at points
!>        given in labels1 and labels2
!> @param axis_p point at which interpolation needs to be done to return a value
!> @param data_p interpolated data found through linear interpolation to the point axis_p
FUNCTION Interp_2D(labels1,labels2,table,axis_p) RESULT(data_p)
  REAL(SRK),INTENT(IN) :: labels1(:),labels2(:),table(:,:),axis_p(:)
  REAL(SRK) :: data_p,f1(2),f2(2)
  INTEGER(SIK) :: i,j,N_i,N_j,i_p,j_p

  CALL Get_points_and_weights(labels1,axis_p(1),f1,i_p,N_i)
  CALL Get_points_and_weights(labels2,axis_p(2),f2,j_p,N_j)

  !Get interpolated value
  data_p=0.0_SRK
  DO j=MAX(j_p-1,1),MIN(j_p,N_j)
    DO i=MAX(i_p-1,1),MIN(i_p,N_i)
      data_p=data_p+f1(i-i_p+2)*f2(j-j_p+2)*table(i,j)
    ENDDO
  ENDDO

ENDFUNCTION Interp_2D
!
!-------------------------------------------------------------------------------
!> @brief This routine takes a 2D data table and linearly interpolates to find
!>        a desired point.
!> @param labels1 axis labels at which data points in table are defined for 1st dimension
!> @param labels2 axis labels at which data points in table are defined for 2nd dimension
!> @param labels3 axis labels at which data points in table are defined for 3rd dimension
!> @param table contains data to be interpolated between defined at points
!>        given in labels1, labels2, and labels 3
!> @param axis_p point at which interpolation needs to be done to return a value
!> @param data_p interpolated data found through linear interpolation to the point axis_p
!>
FUNCTION Interp_3D(labels1,labels2,labels3,table,axis_p) RESULT(data_p)
  REAL(SRK),INTENT(IN) :: labels1(:),labels2(:),labels3(:),table(:,:,:),axis_p(:)
  REAL(SRK) :: data_p,f1(2),f2(2),f3(2)
  INTEGER(SIK) :: i,j,k,N_i,N_j,N_k,i_p,j_p,k_p

  CALL Get_points_and_weights(labels1,axis_p(1),f1,i_p,N_i)
  CALL Get_points_and_weights(labels2,axis_p(2),f2,j_p,N_j)
  CALL Get_points_and_weights(labels3,axis_p(3),f3,k_p,N_k)

  !Get interpolated value
  data_p=0.0_SRK
  DO k=MAX(k_p-1,1),MIN(k_p,N_k)
    DO j=MAX(j_p-1,1),MIN(j_p,N_j)
      DO i=MAX(i_p-1,1),MIN(i_p,N_i)
        data_p=data_p+f1(i-i_p+2)*f2(j-j_p+2)*f3(k-k_p+2)*table(i,j,k)
      ENDDO
    ENDDO
  ENDDO

ENDFUNCTION Interp_3D
!
!-------------------------------------------------------------------------------
!> @brief This routine takes a 1D array of table labels (the points along a single
!>        dimension at which table entries are defined) and the interpolation point
!>        coordinate along that dimension and returns the index of the label entry
!>        directly ahead of that point along with the weights of that point and the
!>        preceeding point.
!> @param labels axis labels at which data points in table are defined
!> @param axis_p point at which interpolation needs to be done to return a value
!> @param f weights associated with the points on either side of axis_p
!> @param i_p index of point directly ahead of axis_p in labels
!> @param N_i number of entries of labels
!>
SUBROUTINE Get_points_and_weights(labels,axis_p,f,i_p,N_i)
  REAL(SRK),INTENT(IN) :: labels(:),axis_p
  REAL(SRK),INTENT(OUT) :: f(2)
  INTEGER(SIK),INTENT(OUT) :: i_p,N_i
  INTEGER(SIK) :: i
  N_i=SIZE(labels)

  !Determine labels index(s) and weights
  f(:)=1.0_SRK
  IF(labels(1) < labels(2)) THEN
    !Acending order
    DO i_p=1,N_i
      IF(axis_p < labels(i_p)) EXIT
    ENDDO
    IF(i_p > 1 .AND. i_p < N_i+1) THEN
      f(1)=(labels(i_p)-axis_p)/(labels(i_p)-labels(i_p-1))
      f(2)=1.0_SRK-f(1)
    ENDIF
  ELSE
    !Decending order
    DO i_p=1,N_i
      IF(axis_p > labels(i_p)) EXIT
    ENDDO
    IF(i_p > 1 .AND. i_p < N_i+1) THEN
      f(1)=(axis_p-labels(i_p))/(labels(i_p-1)-labels(i_p))
      f(2)=1.0_SRK-f(1)
    ENDIF
  ENDIF
ENDSUBROUTINE Get_points_and_weights

ENDMODULE Interpolators
