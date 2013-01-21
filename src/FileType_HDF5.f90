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
!> @brief Utility module for I/O defines the HDF5 file type object.
!> 
!> This type is an extension of the base file type implemented in FileType_Base.
!> It contains specific routines for interrogating an HDF5-format binary file
!> and for writing to and reading from records in an HDF5 file. Since the module
!> relies of the HDF5 library to be present, the entire module is disabled and
!> not compiled if the build system does not have HDF5 support enabled.
!> 
!> There are actually two versions of the HDF library, a serial implementation
!> and a parallel implementation. The latter relies on MPI and ahs separate
!> subroutine calls, so there are preprocessor directives switching between the
!> two depending on which library is present.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief Exceptionhandler
!>  - @ref FileType_Base "FileType_Base": @copybrief FileType_Base
!>
!> @author Mitchell T.H. Young
!>   @date 01/20/2013
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FileType_HDF5
  
#ifdef HAVE_HDF5
  USE HDF5
#endif
  USE IO_Strings
  USE FileType_Base
  USE IntrType
  USE ExceptionHandler
  IMPLICIT NONE
  PRIVATE
  
  !List of Public Members
  PUBLIC :: HDF5FileType

  TYPE,EXTENDS(BaseFileType) :: HDF5FileType
    !> Initialization status
    LOGICAL(SBK) :: isInit=.FALSE.
    !> Full path to the file
    CHARACTER(LEN=MAX_PATH_LENGTH+MAX_FNAME_LENGTH+MAX_FEXT_LENGTH) :: fullname=''
    !> Exception handler for this object

#ifdef HAVE_HDF5
    !> File id assigned by the HDF5 library when file is opened
    INTEGER(HID_T) :: file_id=0
#endif

    CONTAINS
      
      !> Initialize and open the HDF5 file
      PROCEDURE,PASS :: init => init_HDF5FileType
      !> Close and destroy object
      PROCEDURE,PASS :: clear => clear_HDF5FileType
      !> Open the HDF5 file and obtain ID
      PROCEDURE,PASS :: fopen => open_HDF5FileType
      !> Close the HDF5 file
      PROCEDURE,PASS :: fclose => close_HDF5FileType
      !> Delete the file
      PROCEDURE,PASS :: fdelete => delete_HDF5FileType
      !> List the members of a group
!      PROCEDURE,PASS :: ls => ls_HDF5FileType
      !> Determine the number of members of a group
!      PROCEDURE,PASS :: ngrp => ngrp_HDF5FileType
      !> Write: real, rank 1
      PROCEDURE,PASS :: write_r1
      !> Write: real, rank 2
      PROCEDURE,PASS :: write_r2
      !> ...
      !> Write data to the file as a dataset
      GENERIC :: write => write_r1,write_r2
      !> Write: real, rank 1
!      PROCEDURE,PASS :: read_r1
      !> Write: real, rank 2
!      PROCEDURE,PASS :: read_r2
      ! ...
      !> Read data from a dataset in the file.
!      GENERIC  :: read => read_r1,read_r2!,...
  ENDTYPE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
    SUBROUTINE init_HDF5FileType(this,filename,w)
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: filename
      LOGICAL(SBK),INTENT(IN) :: w
      CHARACTER(LEN=MAX_PATH_LENGTH) :: fpath
      CHARACTER(LEN=MAX_FNAME_LENGTH) :: fname
      CHARACTER(LEN=MAX_FEXT_LENGTH) :: fext
#ifdef HAVE_HDF5
      INTEGER(HID_T) :: error
#endif      
      IF(.NOT.ASSOCIATED(this%e)) THEN
        ALLOCATE(this%e)
      ENDIF
#ifdef HAVE_HDF5
      CALL getFileParts(filename,fpath,fname,fext,this%e)
      CALL this%setFilePath(fpath)
      CALL this%setFileName(fname)
      CALL this%setFileExt(fext)

      ! Set read/write status
      CALL this%setWriteStat(.TRUE.)

      this%fullname=filename

      ! Initialize the HDF5 interface. This needs be done before any other calls
      ! to the HF5 interface can be made.
      CALL h5open_f(error)

      ! Open the HDF5 file
      CALL this%fopen()

      this%isinit=.TRUE.
#else
      ! We dont have HDF5, so we can't initialize
      CALL this%e%raiseWarning('The HDF5 library is not present in this build')
#endif
    ENDSUBROUTINE init_HDF5FileType
!
!-------------------------------------------------------------------------------
    SUBROUTINE clear_HDF5FileType(this)
      CLASS(HDF5FileType),INTENT(INOUT) :: this
#ifdef HAVE_HDF5    
      INTEGER(HID_T) :: error
      ! Close the HDF5 file.
      CALL h5fclose_f(this%file_id,error)

      ! Close the HDF5 interface. This can only be done once all calls to the
      ! HDF5 library are complete.
      CALL h5close_f(error)
#endif
      this%isinit=.FALSE.
      IF(ASSOCIATED(this%e)) THEN
        DEALLOCATE(this%e)
      ENDIF
    ENDSUBROUTINE clear_HDF5FileType
!
!-------------------------------------------------------------------------------
    SUBROUTINE open_HDF5FileType(file)
      CLASS(HDF5FileType),INTENT(INOUT) :: file
#ifdef HAVE_HDF5    
      INTEGER(HID_T) :: acc
      INTEGER(HID_T) :: error


      ! Decide what access type to use
      IF(file%isWrite()) THEN
        acc=H5F_ACC_TRUNC_F
        CALL h5fcreate_f(file%fullname,acc,file%file_id,error)
      ELSE
        acc=H5F_ACC_RDONLY_F
        CALL h5fopen_f(file%fullname,acc,file%file_id,error)
      ENDIF
#endif
    ENDSUBROUTINE open_HDF5FileType
!
!-------------------------------------------------------------------------------
    SUBROUTINE close_HDF5FileType(file)
      CLASS(HDF5FileType),INTENT(INOUT) :: file
#ifdef HAVE_HDF5    
      INTEGER(HID_T) :: error
      
      CALL h5fclose_f(file%file_id,error)
#endif
    ENDSUBROUTINE close_HDF5FileType
!
!-------------------------------------------------------------------------------
    SUBROUTINE delete_HDF5FileType(file)
      CLASS(HDF5FileType),INTENT(INOUT) :: file
      
      RETURN 
    ENDSUBROUTINE delete_HDF5FileType
!
!-------------------------------------------------------------------------------
    SUBROUTINE write_r1(this,dsetname,data)
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SRK),ALLOCATABLE :: data(:)
#ifdef HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: dims
      INTEGER(HID_T),PARAMETER :: rank=1
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Determine the dimensions for the dataspace
      dims=SHAPE(data)

      ! Create the dataspace
      CALL h5screate_simple_f(rank,dims,dspace_id,error)

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, dsetname, H5T_NATIVE_DOUBLE, dspace_id, &
                       dset_id,error)
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, dims, error)

      ! Close the dataset
      CALL h5dclose_f(dset_id,error)

      ! Close the dataspace
      CALL h5sclose_f(dspace_id,error)
#endif
    ENDSUBROUTINE write_r1
!
!-------------------------------------------------------------------------------
    SUBROUTINE write_r2(this,dsetname,data)
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SRK),ALLOCATABLE :: data(:,:)
#ifdef HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(2) :: dims
      INTEGER(HID_T),PARAMETER :: rank=2
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Determine the dimensions for the dataspace
      dims=SHAPE(data)

      ! Create the dataspace
      CALL h5screate_simple_f(rank,dims,dspace_id,error)

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, dsetname, H5T_NATIVE_DOUBLE, dspace_id, &
                       dset_id,error)
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, dims, error)

      ! Close the dataset
      CALL h5dclose_f(dset_id,error)

      ! Close the dataspace
      CALL h5sclose_f(dspace_id,error)
#endif
    ENDSUBROUTINE write_r2

ENDMODULE FileType_HDF5
