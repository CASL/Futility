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
USE FutilityComputingEnvironmentModule
USE IntrType
USE ExceptionHandler
USE IO_Strings

IMPLICIT NONE
PRIVATE

PUBLIC :: Interp

CHARACTER(LEN=*),PARAMETER :: modName='InterpolatorsModule'

TYPE(ExceptionHandlerType),SAVE :: eInterp

!> @brief Generic interface to interpolate whatever is handed in.
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

INTERFACE Data_Verify
  !> @copybrief InterpolatorsModule::Data_Verify_1D
  !> @copydetails InterpolatorsModule::Data_Verify_1D
  MODULE PROCEDURE Data_Verify_1D
  !> @copybrief InterpolatorsModule::Data_Verify_2D
  !> @copydetails InterpolatorsModule::Data_Verify_2D
  MODULE PROCEDURE Data_Verify_2D
  !> @copybrief InterpolatorsModule::Data_Verify_3D
  !> @copydetails InterpolatorsModule::Data_Verify_3D
  MODULE PROCEDURE Data_Verify_3D
ENDINTERFACE Data_Verify
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
!> @returns Interpolant interpolated data found through linear interpolation to the point point
FUNCTION Interp_1D(labels,table,point) RESULT(Interpolant)
  REAL(SRK),INTENT(IN) :: labels(:)
  REAL(SRK),INTENT(IN) :: table(:)
  REAL(SRK),INTENT(IN) :: point
  REAL(SRK) :: Interpolant,f(2)
  INTEGER(SIK) :: i,N_i,i_p

  REQUIRE(Data_Verify(labels,table))

  CALL Get_points_and_weights(labels,point,f,i_p,N_i)

  !Get interpolated value
  Interpolant=0.0_SRK
  DO i=MAX(i_p-1,1),MIN(i_p,N_i)
    Interpolant=Interpolant+f(i-i_p+2)*table(i)
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
!> @param point point at which interpolation needs to be done to return a value
!> @returns Interpolant interpolated data found through linear interpolation to the point point
FUNCTION Interp_2D(labels1,labels2,table,point) RESULT(Interpolant)
  REAL(SRK),INTENT(IN) :: labels1(:)
  REAL(SRK),INTENT(IN) :: labels2(:)
  REAL(SRK),INTENT(IN) :: table(:,:)
  REAL(SRK),INTENT(IN) :: point(:)
  REAL(SRK) :: Interpolant,f1(2),f2(2)
  INTEGER(SIK) :: i,j,N_i,N_j,i_p,j_p

  REQUIRE(Data_Verify(labels1,labels2,table))

  CALL Get_points_and_weights(labels1,point(1),f1,i_p,N_i)
  CALL Get_points_and_weights(labels2,point(2),f2,j_p,N_j)

  !Get interpolated value
  Interpolant=0.0_SRK
  DO j=MAX(j_p-1,1),MIN(j_p,N_j)
    DO i=MAX(i_p-1,1),MIN(i_p,N_i)
      Interpolant=Interpolant+f1(i-i_p+2)*f2(j-j_p+2)*table(i,j)
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
!> @param point point at which interpolation needs to be done to return a value
!> @returns Interpolant interpolated data found through linear interpolation to the point point
!>
FUNCTION Interp_3D(labels1,labels2,labels3,table,point) RESULT(Interpolant)
  REAL(SRK),INTENT(IN) :: labels1(:)
  REAL(SRK),INTENT(IN) :: labels2(:)
  REAL(SRK),INTENT(IN) :: labels3(:)
  REAL(SRK),INTENT(IN) :: table(:,:,:)
  REAL(SRK),INTENT(IN) :: point(:)
  REAL(SRK) :: Interpolant,f1(2),f2(2),f3(2)
  INTEGER(SIK) :: i,j,k,N_i,N_j,N_k,i_p,j_p,k_p

  REQUIRE(Data_Verify(labels1,labels2,labels3,table))

  CALL Get_points_and_weights(labels1,point(1),f1,i_p,N_i)
  CALL Get_points_and_weights(labels2,point(2),f2,j_p,N_j)
  CALL Get_points_and_weights(labels3,point(3),f3,k_p,N_k)

  !Get interpolated value
  Interpolant=0.0_SRK
  DO k=MAX(k_p-1,1),MIN(k_p,N_k)
    DO j=MAX(j_p-1,1),MIN(j_p,N_j)
      DO i=MAX(i_p-1,1),MIN(i_p,N_i)
        Interpolant=Interpolant+f1(i-i_p+2)*f2(j-j_p+2)*f3(k-k_p+2)*table(i,j,k)
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
!> @param point point at which interpolation needs to be done to return a value
!> @param f weights associated with the points on either side of point
!> @param i_p index of data point directly ahead of the interpolation point in labels
!> @param N_i number of entries of labels
!>
SUBROUTINE Get_points_and_weights(labels,point,f,i_p,N_i)
  REAL(SRK),INTENT(IN) :: labels(:)
  REAL(SRK),INTENT(IN) :: point
  REAL(SRK),INTENT(OUT) :: f(2)
  INTEGER(SIK),INTENT(OUT) :: i_p,N_i
  N_i=SIZE(labels)

  !Determine labels index(s) and weights
  f(:)=1.0_SRK
  IF(labels(1) < labels(2)) THEN
    !Ascending order
    DO i_p=1,N_i
      IF(point < labels(i_p)) EXIT
    ENDDO
    IF(i_p > 1 .AND. i_p < N_i+1) THEN
      f(1)=(labels(i_p)-point)/(labels(i_p)-labels(i_p-1))
      f(2)=1.0_SRK-f(1)
    ENDIF
  ELSE
    !Descending order
    DO i_p=1,N_i
      IF(point > labels(i_p)) EXIT
    ENDDO
    IF(i_p > 1 .AND. i_p < N_i+1) THEN
      f(1)=(point-labels(i_p))/(labels(i_p-1)-labels(i_p))
      f(2)=1.0_SRK-f(1)
    ENDIF
  ENDIF
ENDSUBROUTINE Get_points_and_weights
!
!-------------------------------------------------------------------------------
!> @brief This routine takes the input for the 1D interpolator routine
!>        and checks it to ensure it is correct (Monotonically increasing
!>        or decreasing axis labels, and correct dimensions for table/labels).
!> @param labels axis labels at which data points in table are defined
!> @param table contains data to be interpolated between defined at points
!>        given in labels.
!> @returns good boolean indicating whether the data is good or not.
!>
FUNCTION Data_Verify_1D(labels,table) RESULT(good)
  REAL(SRK),INTENT(IN) :: labels(:)
  REAL(SRK),INTENT(IN) :: table(:)
  CHARACTER(LEN=*),PARAMETER :: myName='Data_Verify_1D'
  LOGICAL(SBK) :: good

  good=Check_label_and_table(labels,table,1,myName)

END FUNCTION Data_Verify_1D
!
!-------------------------------------------------------------------------------
!> @brief This routine takes the input for the 2D interpolator routine
!>        and checks it to ensure it is correct (Monotonically increasing
!>        or decreasing axis labels, and correct dimensions for table/labels).
!> @param labels1 axis labels at which data points in table are defined for 1st dimension
!> @param labels2 axis labels at which data points in table are defined for 2nd dimension
!> @param table contains data to be interpolated between defined at points
!>        given in labels1 and labels2.
!> @returns good boolean indicating whether the data is good or not.
!>
FUNCTION Data_Verify_2D(labels1,labels2,table) RESULT(good)
  REAL(SRK),INTENT(IN) :: labels1(:)
  REAL(SRK),INTENT(IN) :: labels2(:)
  REAL(SRK),INTENT(IN) :: table(:,:)
  CHARACTER(LEN=*),PARAMETER :: myName='Data_Verify_2D'
  LOGICAL(SBK) :: good

  good=Check_label_and_table(labels1,table(:,1),1,myName)
  good=Check_label_and_table(labels2,table(1,:),2,myName)

END FUNCTION Data_Verify_2D
!
!-------------------------------------------------------------------------------
!> @brief This routine takes the input for the 3D interpolator routine
!>        and checks it to ensure it is correct (Monotonically increasing
!>        or decreasing axis labels, and correct dimensions for table/labels).
!> @param labels1 axis labels at which data points in table are defined for 1st dimension
!> @param labels2 axis labels at which data points in table are defined for 2nd dimension
!> @param labels3 axis labels at which data points in table are defined for 3rd dimension
!> @param table contains data to be interpolated between defined at points
!>        given in labels1, labels2, and labels 3.
!> @returns good boolean indicating whether the data is good or not.
!>
FUNCTION Data_Verify_3D(labels1,labels2,labels3,table) RESULT(good)
  REAL(SRK),INTENT(IN) :: labels1(:)
  REAL(SRK),INTENT(IN) :: labels2(:)
  REAL(SRK),INTENT(IN) :: labels3(:)
  REAL(SRK),INTENT(IN) :: table(:,:,:)
  CHARACTER(LEN=*),PARAMETER :: myName='Data_Verify_3D'
  LOGICAL(SBK) :: good

  good=Check_label_and_table(labels1,table(:,1,1),1,myName)
  good=Check_label_and_table(labels2,table(1,:,1),2,myName)
  good=Check_label_and_table(labels3,table(1,1,:),3,myName)

END FUNCTION Data_Verify_3D

FUNCTION Check_label_and_table(labels,table,dim,myName) RESULT(good)
  REAL(SRK),INTENT(IN) :: labels(:)
  REAL(SRK),INTENT(IN) :: table(:)
  INTEGER(SIK),INTENT(IN) :: dim
  CHARACTER(LEN=*),INTENT(IN) :: myName
  INTEGER(SIK) :: i
  LOGICAL(SBK) :: good

  IF(SIZE(labels) == 1) THEN
    CALL eInterp%raiseError(modName// &
        '::'//myName//' - Size of table insufficient for interpolation!')
  ELSEIF(SIZE(labels) /= SIZE(table)) THEN
    CALL eInterp%raiseError(modName// &
        '::'//myName//' - Table dimension do not match label dimensions dimension '//str(dim)//'!')
  ELSE
    DO i=2,SIZE(labels)
      IF(labels(i) .APPROXEQR. labels(i-1)) CALL eInterp%raiseError(modName// &
          '::'//myName//' - Identical data labels found dimension '//str(dim)//'!')
    ENDDO
    IF(labels(1) < labels(2)) THEN
      DO i=2,SIZE(labels)
        IF(labels(i) < labels(i-1)) CALL eInterp%raiseError(modName// &
            '::'//myName//' - Nonmonotonicity detected in data labels dimension '//str(dim)//'!')
      ENDDO
    ELSE
      DO i=2,SIZE(labels)
        IF(labels(i) > labels(i-1)) CALL eInterp%raiseError(modName// &
            '::'//myName//' - Nonmonotonicity detected in data labels dimension '//str(dim)//'!')
      ENDDO
    ENDIF
  ENDIF

  good=.TRUE.

ENDFUNCTION Check_label_and_table

ENDMODULE InterpolatorsModule
