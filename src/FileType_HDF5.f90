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
!> and a parallel implementation. The latter relies on MPI and has separate
!> subroutine calls, so there will need to be preprocessor directives switching
!> between the two depending on which library is present.
!>
!> It is important to note that the read and write routines are typed to use
!> fixed precision (for instance SSK and SDK instead of SRK). This is important
!> because we need to use the supported types of the HDF5 library and file
!> format.
!>
!> @par Module Dependencies
!>  - @ref IntrType "IntrType": @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler": @copybrief Exceptionhandler
!>  - @ref FileType_Base "FileType_Base": @copybrief FileType_Base
!>
!> @author Mitchell T.H. Young
!>   @date 01/20/2013
!> 
!> @todo
!>  - Implement MPI/parallel HDF5
!>  - Make sure routines are safe (check for initialized object, etc.)
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

  !> This type extends the base file type, and adds support for writing to and
  !> reading from HDF5 binary files. As implemented, there are three modes for
  !> accessing a file can be opened as: 'read' and 'write' and 'new'. Read mode
  !> opens an already extant HDF5 file and allows the client code to interrogate
  !> that file and extract data without permissions to alter the file. Write
  !> mode opens an extant HDF5 file and allows the client code to alter its
  !> contents. New mode overwrites any file by the same name and starts from
  !> scratch.
  TYPE,EXTENDS(BaseFileType) :: HDF5FileType
    !> Initialization status
    LOGICAL(SBK) :: isInit=.FALSE.
    !> Full path to the file
    CHARACTER(LEN=MAX_PATH_LENGTH+MAX_FNAME_LENGTH+MAX_FEXT_LENGTH) :: fullname=''
    !> Access mode for creating/opening file.
    CHARACTER(LEN=5) :: mode

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
      PROCEDURE,PASS :: ls => ls_HDF5FileType
      !> Create a group in the HDF5 File
      PROCEDURE,PASS :: mkdir => mkdir_HDF5FileType
      !> Determine the number of members of a group
      PROCEDURE,PASS :: ngrp => ngrp_HDF5FileType
      !> Write: real(SDK), rank 1
      PROCEDURE,PASS :: write_d1
      !> Write: real(SDK), rank 2
      PROCEDURE,PASS :: write_d2
      !> Write: real(SSK), rank 1
!      PROCEDURE,PASS :: write_r1
      !> Write: logical, rank1
      PROCEDURE,PASS :: write_l1
      !> ...
      !> Write data to the file as a dataset
      GENERIC :: write => write_d1,write_d2,write_l1
      !> Read: real(SDK), rank 1
      PROCEDURE,PASS :: read_d1
      !> Read: real(SDK), rank 2
!      PROCEDURE,PASS :: read_d2
      !> Read: real(SSK), rank 1
!      PROCEDURE,PASS :: read_r1
      ! ...
      !> Read data from a dataset in the file.
      GENERIC  :: read => read_d1!,read_d2,...
  ENDTYPE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes an HDF5 file object.
!> @param this the object to be initialized
!> @param filename the relative path to the file on the filesystem
!> @param mode the access mode. Can be 'READ', 'WRITE' or 'NEW'
!>
!> This routine initializes an HDF5 file object by setting the objects
!> attributes, initializing the HDF5 library interface and calling the @c open
!> routine.
    SUBROUTINE init_HDF5FileType(this,filename,mode)
      CHARACTER(LEN=*),PARAMETER :: myName='init_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: filename
      CHARACTER(LEN=*),INTENT(IN) :: mode
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

      ! Store the access mode
      this%mode=mode
      CALL toUPPER(this%mode)
      SELECTCASE(TRIM(this%mode))
      CASE('READ')
        CALL this%setWriteStat(.FALSE.)
      CASE('WRITE')
        CALL this%setWriteStat(.TRUE.)
      CASE('NEW')
        CALL this%setWriteStat(.TRUE.)
      CASE DEFAULT
        CALL this%e%raiseError(myName//": Unrecognized access mode.")
      ENDSELECT

      this%fullname=filename

      ! Initialize the HDF5 interface. This needs be done before any other calls
      ! to the HF5 interface can be made.
      CALL h5open_f(error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Could not initialize HDF5 INTERFACE.")
      ENDIF

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
!> @brief Destructs an HDF5 file object.
!> @param this the HDF5 object to be destroyed
!>
!> This routine releases resources used for interacting with the HSF5 file. It
!> closes the file and the HDF5 library interface.
    SUBROUTINE clear_HDF5FileType(this)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_HDF5FileType'
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
!> @brief Open an HDF5 file
!> @param file the HDF5FileType object to open
!>
!> This routine implements the abstract @c fopen routine in the base file type.
!> It uses the HDF5 library interface that was initialized in @c init to open
!> the file.
    SUBROUTINE open_HDF5FileType(file)
      CHARACTER(LEN=*),PARAMETER :: myName='open_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: file
#ifdef HAVE_HDF5    
      INTEGER(HID_T) :: acc
      INTEGER(HID_T) :: error

      ! Decide what access type to use
      SELECTCASE(TRIM(file%mode))
      CASE('READ')
        acc=H5F_ACC_RDONLY_F
        CALL h5fopen_f(file%fullname,acc,file%file_id,error)
      CASE('WRITE')
        acc=H5F_ACC_RDWR_F
        CALL h5fopen_f(file%fullname,acc,file%file_id,error)
      CASE('NEW')
        acc=H5F_ACC_TRUNC_F
        CALL h5fcreate_f(file%fullname,acc,file%file_id,error)
      CASE DEFAULT
        CALL file%e%raiseError(myName//": Unrecognized access mode.")
      ENDSELECT
#endif
    ENDSUBROUTINE open_HDF5FileType
!
!-------------------------------------------------------------------------------
!> @brief Closes access to an HDF5 file
!> @param file the HDF5FileType object to close
!>
!> This routine implements the abstract @c fclose routine in the base file type.
    SUBROUTINE close_HDF5FileType(file)
      CHARACTER(LEN=*),PARAMETER :: myName='close_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: file
#ifdef HAVE_HDF5    
      INTEGER(HID_T) :: error
      
      CALL h5fclose_f(file%file_id,error)
#endif
    ENDSUBROUTINE close_HDF5FileType
!
!-------------------------------------------------------------------------------
!> @brief delete and HDF5 file
!>
!> @param file the HDF5FileType object to delet
!>
!> This routine implements the abstract @c fdelete routine in the base file
!> type.
    SUBROUTINE delete_HDF5FileType(file)
      CHARACTER(LEN=*),PARAMETER :: myName='delete_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: file
      
      RETURN 
    ENDSUBROUTINE delete_HDF5FileType
!
!-------------------------------------------------------------------------------
!> @brief List the contents of a group
!> @param this the HDF5FileType object to operate on
!> @param path the absolute path to the group of interest (group heirarchy
!> represented with '->')
!> @param objs the list of objects to be returned. See below.
!>
!> This routine is useful for discovering the contents of an HDF5 file. A path
!> to a group in the file is provided in the subroutine call and the names of
!> its constituents is stored in the objs list. If objs is passed in, it will be
!> deallocated and reallocated to the proper size to store the names of all of
!> the objects in the group.
    SUBROUTINE ls_HDF5FileType(this,path,objs)
      CHARACTER(LEN=*),PARAMETER :: myName='ls_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: path
      CHARACTER(LEN=*),ALLOCATABLE,INTENT(INOUT) :: objs(:)
      CHARACTER(LEN=MAX_PATH_LENGTH),ALLOCATABLE :: path2
#ifdef HAVE_HDF5
      INTEGER(HSIZE_T) :: num_obj,i
      INTEGER(HID_T) :: grp_id,error
      INTEGER :: store_type,nlinks,max_corder

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      path2=convertPath(path)
     
      CALL h5gopen_f(this%file_id, path2, grp_id, error)

      CALL h5gget_info_f(grp_id, store_type, nlinks, max_corder, error)

      IF(ALLOCATED(objs))THEN
        DEALLOCATE(objs)
      ENDIF
      ALLOCATE(objs(nlinks))

      DO i=0,nlinks-1
        CALL h5lget_name_by_idx_f(this%file_id, path2, H5_INDEX_NAME_F, &
                                  H5_ITER_INC_F, i, objs(i+1), error)
      ENDDO

      CALL h5gclose_f(grp_id, error)

#endif
    ENDSUBROUTINE ls_HDF5FileType
!
!-------------------------------------------------------------------------------
!> @brief Creates a new group in the HDF file.
!> @param this the HDF5FileType object to operate on
!> @param path the path to the group to be created
!>
!> This routine is used to create a new group in an HDF5 file. It can only be
!> called if the file has write access.
    SUBROUTINE mkdir_HDF5FileType(this,path)
      CHARACTER(LEN=*),PARAMETER :: myNAme='mkdir_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: path
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path2
#ifdef HAVE_HDF5
      INTEGER(HID_T) :: group_id,error

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Ensure that we have write permissions to the file
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseWarning(myName//": Can not create group in read-only file.")
      ENDIF

      ! Convert the path to use slashes
      path2=convertPath(path)

      ! Create the group
      CALL h5gcreate_f(this%file_id,path2,group_id,error)
      
      IF(error == 0)THEN
        ! Close the group
        CALL h5gclose_f(group_id,error)
        IF(error /= 0)THEN
          CALL this%e%raiseWarning(myName//": Failed to close HDF group")
        ENDIF
      ELSE
        CALL this%e%raiseWarning(myName//": Failed to create HDF5 group.")
      ENDIF
#endif
    ENDSUBROUTINE mkdir_HDF5FileType
!
!-------------------------------------------------------------------------------
!> @brief Returns the number of objects in a group
!> @param this the HDF5FileType object to interrogate
!> @param path the group in the file to interrogate
!>
!> This function returns how many objects are in the group @c path.
    FUNCTION ngrp_HDF5FileType(this,path) RESULT(ngrp)
      CHARACTER(LEN=*),PARAMETER :: myName='ngrp_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: path
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path2
      INTEGER(SIK) :: ngrp
#ifdef HAVE_HDF5
      INTEGER(HSIZE_T) :: num_obj,i
      INTEGER(HID_T) :: grp_id,error
      INTEGER :: store_type,nlinks,max_corder

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      path2=convertPath(path)
 
      CALL h5gopen_f(this%file_id, path2, grp_id, error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Could not open group in HDF5 file.")
      ENDIF

      CALL h5gget_info_f(grp_id, store_type, nlinks, max_corder, error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Could not get group info in HDF5 file.")
      ENDIF

      ngrp=nlinks
#else
      ngrp=0
#endif
    ENDFUNCTION ngrp_HDF5FileType
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of doubles to a dataset
    SUBROUTINE write_d1(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='writed1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),ALLOCATABLE :: data(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: dims
      INTEGER(HID_T),PARAMETER :: rank=1
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      dims=SHAPE(data)

      ! Create the dataspace
      CALL h5screate_simple_f(rank,dims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_DOUBLE, dspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, dims, error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not write to the dataset.')
      ENDIF

      ! Close the dataset
      CALL h5dclose_f(dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataset.')
      ENDIF

      ! Close the dataspace
      CALL h5sclose_f(dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF

#endif
    ENDSUBROUTINE write_d1
!
!-------------------------------------------------------------------------------
    SUBROUTINE write_d2(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='writed2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),ALLOCATABLE :: data(:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(2) :: dims
      INTEGER(HID_T),PARAMETER :: rank=2
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      dims=SHAPE(data)

      ! Create the dataspace
      CALL h5screate_simple_f(rank,dims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_DOUBLE, dspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, dims, error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not write to the dataset.')
      ENDIF

      ! Close the dataset
      CALL h5dclose_f(dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataset.')
      ENDIF

      ! Close the dataspace
      CALL h5sclose_f(dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE write_d2
!
!-------------------------------------------------------------------------------
    SUBROUTINE write_l1(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='writel1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),ALLOCATABLE,INTENT(IN) :: data(:)
      CHARACTER(LEN=SIZE(data)) :: datac
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: dims
      INTEGER(HID_T),PARAMETER :: rank=1
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id
      INTEGER(SIK) :: i

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF

      ! Convert the path name
      path = convertPath(dsetname)

      ! For now we are conferting to characters which store T/F since there is
      ! not native boolean support for hdf5. We need to use a surrugate
      ! character string
      DO i=1,SIZE(data)
        IF(data(i))THEN
          datac(i:i)='T'
        ELSE
          datac(i:i)='F'
        ENDIF
      ENDDO


      ! Determine the dimensions for the dataspace
      dims=SHAPE(data)

      ! Create the dataspace
      CALL h5screate_simple_f(rank,dims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to create dataspace.")
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_CHARACTER, dspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to create dataset.")
      ENDIF

      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, datac, dims, error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to write to dataset.")
      ENDIF

      ! Close the dataset
      CALL h5dclose_f(dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to close dataset.")
      ENDIF

      ! Close the dataspace
      CALL h5sclose_f(dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to close dataspace.")
      ENDIF

#endif     

    ENDSUBROUTINE write_l1
!
!-------------------------------------------------------------------------------
    SUBROUTINE write_i1(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='writei1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SIK),ALLOCATABLE :: data(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: dims
      INTEGER(HID_T),PARAMETER :: rank=1
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      dims=SHAPE(data)

      ! Create the dataspace
      CALL h5screate_simple_f(rank,dims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to create dataspace.")
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_INTEGER, dspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to create dataset.")
      ENDIF

      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dims, error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to write to dataset.")
      ENDIF

      ! Close the dataset
      CALL h5dclose_f(dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to close dataset.")
      ENDIF

      ! Close the dataspace
      CALL h5sclose_f(dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to close dataspace.")
      ENDIF

#endif
    ENDSUBROUTINE write_i1
!
!-------------------------------------------------------------------------------
    SUBROUTINE read_d1(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readd1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: data(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims,len1
      INTEGER(HID_T),PARAMETER :: rank=1
      
      INTEGER(HID_T) :: error,mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Convert the path name to use slashes
      path=convertPath(dsetname)

      ! Open the dataset
      CALL h5dopen_f(this%file_id, path, dset_id, error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to open dataset.")
      ENDIF

      ! Get dataset dimensions for allocation
      CALL h5dget_space_f(dset_id,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to obtain the dataspace.")
      ENDIF
      ! Make sure the rank is right
      CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
      IF(error < 0)THEN
        CALL this%e%raiseError(myName//": Failed to retrieve number of dataspace dimensions.")
      ENDIF
      IF(ndims /= rank)THEN
        CALL this%e%raiseError(myName//": Using wrong read function for rank.")
      ENDIF

      CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
      IF(error < 0)THEN
        CALL this%e%raiseError(myName//": Failed to retrieve dataspace dimensions.")
      ENDIF

      ! Allocate space if needed
      IF(ALLOCATED(data))THEN
        ! Make sure the data is the right size
        IF(ANY(SHAPE(data) /= dims))THEN
          CALL this%e%raiseError(myName//": data array is the wrong size.")
        ENDIF
      ELSE
        ! Allocate to size
        ALLOCATE(data(dims(1)))
      ENDIF

      ! Read the dataset
      mem=H5T_NATIVE_DOUBLE
      CALL h5dread_f(dset_id,mem,data,dims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to read data from dataset.")
      ENDIF

      ! Close the dataset
      CALL h5dclose_f(dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to close dataset.")
      ENDIF

      ! Close the dataspace
      CALL h5sclose_f(dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to close dataspace.")
      ENDIF

#endif
    ENDSUBROUTINE read_d1

!
!-------------------------------------------------------------------------------
!> @brief Convert a path provided with '->' separators to '/'
!> @param path the path string to convert.
!>
!> Paths in MPACT use '->' to resolve heirarchy. HSF5 uses '/'.
    FUNCTION convertPath(path)
      CHARACTER(LEN=*),INTENT(IN) :: path
      CHARACTER(LEN=80) :: convertpath

      INTEGER :: i,ipos,ipos2,nelem,last,ind

      convertpath=''

      last=len(trim(path))
      ! Split the path string by '->'
      ipos=1
      DO
        ind=INDEX(path(ipos:last),'->')
        ipos2=ind+ipos-1
        IF(ind>0)THEN
          convertPath=trim(convertpath)//'/'//path(ipos:ipos2-1)
          ipos=ipos2+2
        ELSE
          convertPath=trim(convertPath)//'/'//path(ipos:last)
          EXIT
        ENDIF
      ENDDO ! Elements in path string
    ENDFUNCTION convertPath
!
ENDMODULE FileType_HDF5
