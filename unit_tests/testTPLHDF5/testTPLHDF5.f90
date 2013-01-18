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
PROGRAM testTPLHDF5
  
#ifdef HAVE_HDF5
  USE HDF5
#endif

  IMPLICIT NONE

  CHARACTER(LEN=8),PARAMETER :: hdf_file='hdf01.h5'

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING HDF5 TPL...'
  WRITE(*,*) '==================================================='

#ifdef HAVE_HDF5
  CALL testHDF5()
#else
  WRITE(*,*) ' HDF5 not enabled!'
#endif
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING HDF5 TPL PASSED!'
  WRITE(*,*) '==================================================='

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5()
#ifdef HAVE_HDF5
      INTEGER(HID_T) :: error,file_id,dspace_id,dset_id,grp_id
      INTEGER(HSIZE_T),DIMENSION(2),PARAMETER :: dims=(/10,5/)
      INTEGER :: i,j
      INTEGER,DIMENSION(10,5) :: dataz

      DO i=1,10
        DO j=1,5
          dataz(i,j)=i+j*2
        ENDDO
      ENDDO

      ! Initialize the interface
      CALL h5open_f(error)

      ! Create/open an HDF file
      CALL h5fcreate_f(hdf_file,H5F_ACC_TRUNC_F,file_id,error)

      ! Create a dataspace   rank  dims   
      CALL h5screate_simple_f(2,dims,dspace_id,error)
     
      ! Group structures
      CALL h5gcreate_f(file_id,'grp_a',grp_id,error)


      ! Create a dataset
      CALL h5dcreate_f(file_id,'testdata',H5T_NATIVE_INTEGER,dspace_id, &
        dset_id,error)

      ! Write data to the dataset
      CALL h5dwrite_f(dset_id,H5T_NATIVE_INTEGER,dataz,dims,error)

      ! Close the dataset
      CALL h5dclose_f(dset_id,error)

      ! close the dataspace
      CALL h5sclose_f(dspace_id,error)

      ! Close the file
      CALL h5fclose_f(file_id,error)

      ! Close the interface
      CALL h5close_f(error)

#endif
    ENDSUBROUTINE testHDF5

ENDPROGRAM
