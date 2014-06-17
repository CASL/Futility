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

#ifdef HAVE_MPI
  INCLUDE 'mpif.h'
#endif

  CHARACTER(LEN=8),PARAMETER :: hdf_file='hdf01.h5'
  INTEGER :: mpierr

  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING HDF5 TPL...'
  WRITE(*,*) '==================================================='

#ifdef HAVE_MPI
  CALL MPI_init(mpierr)
#endif


#ifdef HAVE_HDF5
  CALL testHDF5()
#else
  WRITE(*,*) ' HDF5 not enabled!'
#endif
  
  WRITE(*,*) '==================================================='
  WRITE(*,*) 'TESTING HDF5 TPL PASSED!'
  WRITE(*,*) '==================================================='

#ifdef HAVE_MPI
  CALL MPI_Finalize(mpierr)
#endif

!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE testHDF5()
#ifdef HAVE_HDF5
      INTEGER(HID_T) :: error,file_id,dspace_id,gspace_id,dset_id
      INTEGER(HID_T) :: grp_id,plist_id,acc,rank
      INTEGER(HSIZE_T),DIMENSION(2) :: dims=(/10,5/)
      INTEGER(HSIZE_T),DIMENSION(2) :: one=(/1,1/)
      INTEGER(HSIZE_T),DIMENSION(2) :: gdims,ldims,offset
      INTEGER :: i,j,node,nproc,l1,u1,l2,u2,npp,ndir
      INTEGER,DIMENSION(10,5) :: dataz
      DOUBLE PRECISION,ALLOCATABLE :: datal(:,:)

#ifndef HAVE_MPI

      dims=(/10,5/)

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

#else
      CALL MPI_Comm_rank(MPI_COMM_WORLD,node,mpierr)
      CALL MPI_Comm_size(MPI_COMM_WORLD,nproc,mpierr)

      IF(nproc /= 16)THEN
        WRITE(*,*)'should be runnign with 16 processors!'
        stop
      ENDIF

      gdims = (/16,16/)
      ldims = (/4,4/)
      rank  = 2
      offset(1)=node

      npp=4
      ndir=4
      l1=(MOD(node,ndir))*npp+1
      u1=l1+npp-1
      l2=(node/ndir)*npp+1
      u2=l2+npp-1
      ALLOCATE(datal(l1:u1,l2:u2))
      datal=node+1

      offset(1)=lbound(datal,1)-1
      offset(2)=lbound(datal,2)-1
      
      CALL h5open_f(error)
      
      CALL h5pcreate_f(H5P_FILE_ACCESS_F,plist_id,error)
      CALL h5pset_fapl_mpio_f(plist_id,MPI_COMM_WORLD,MPI_INFO_NULL,error)
      acc=H5F_ACC_TRUNC_F
      CALL h5fcreate_f('parallel.h5',acc,file_id,error,access_prp=plist_id)
      CALL h5pclose_f(plist_id,error)

      ! Create and HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      ! Create the dataset
      CALL h5dcreate_f(file_id, 'test', H5T_NATIVE_DOUBLE, gspace_id, &
                       dset_id,error,plist_id)
      CALL h5pclose_f(plist_id,error)
      
      CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id,error)
      CALL h5dget_space_f(dset_id,gspace_id,error)
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_F,offset,one,error, &
                                 one,ldims)
      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, datal, gdims, error, &
                      dspace_id,gspace_id,plist_id)
      CALL h5dclose_f(dset_id,error)
      CALL h5sclose_f(dspace_id,error)
      CALL h5sclose_f(gspace_id,error)

      CALL h5fclose_f(file_id,error)

      CALL h5close_f(error)

      DEALLOCATE(datal)

#endif



#endif
    ENDSUBROUTINE testHDF5

ENDPROGRAM
