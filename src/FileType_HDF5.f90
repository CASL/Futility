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

#ifdef MPACT_HAVE_HDF5
  USE HDF5
#endif
  USE IO_Strings
  USE FileType_Base
  USE IntrType
  USE ExceptionHandler
  USE ParallelEnv
  USE Strings
  IMPLICIT NONE
  PRIVATE
  
#ifdef HAVE_MPI
  INCLUDE 'mpif.h'
#endif

  !> This type extends the base file type, and adds support for writing to and
  !List of Public Members
  PUBLIC :: HDF5FileType
  !> Maximum HDF5 string length
  INTEGER(HSIZE_T),PARAMETER :: LEN_HDF5_STRING=256

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
    !> Parallel environment for MPI I/O
    TYPE(MPI_EnvType) :: pe

#ifdef MPACT_HAVE_HDF5
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
      !> Write: double(SDK), rank 1
      PROCEDURE,PASS :: write_d1
      !> Write: double(SDK), rank 2
      PROCEDURE,PASS :: write_d2
      !> Write: double(SDK), rank 3
      PROCEDURE,PASS :: write_d3
      !> Write: real(SSK), rank 1
      PROCEDURE,PASS :: write_s1
      !> Write: real(SSK), rank 2
      PROCEDURE,PASS :: write_s2
      !> Write: real(SSK), rank 3
      PROCEDURE,PASS :: write_s3
      !> Write: logical(SBK), rank 1
      PROCEDURE,PASS :: write_l1
      !> Write: logical(SBK), rank 2
      PROCEDURE,PASS :: write_l2
      !> Write: logical(SBK), rank 3
      PROCEDURE,PASS :: write_l3
      !> Write: integer(SIK), rank 1
      PROCEDURE,PASS :: write_i1
      !> Write: integer(SIK), rank 2
      PROCEDURE,PASS :: write_i2
      !> Write: integer(SIK), rank 3
      PROCEDURE,PASS :: write_i3
      !> Write: character string
      PROCEDURE,PASS :: write_string_helper
      !> Write: character string, rank 1
      PROCEDURE,PASS :: write_c1
      !> Write data to the file as a dataset
      GENERIC :: write => write_d1,write_d2,write_d3,write_s1,write_s2, &
            write_s3,write_l1,write_l2,write_l3,write_i1,write_i2,write_i3, &
            write_c1!,write_c2,write_c3
      !> Read: real(SDK), rank 1
      PROCEDURE,PASS :: read_d1
      !> Read: real(SDK), rank 2
      PROCEDURE,PASS :: read_d2
      !> Read: real(SDK), rank 3
      PROCEDURE,PASS :: read_d3
      !> Read: real(SSK), rank 1
      PROCEDURE,PASS :: read_s1
      !> Read: real(SSK), rank 2
      PROCEDURE,PASS :: read_s2
      !> Read: real(SSK), rank 3
      PROCEDURE,PASS :: read_s3
      !> Read: integer(SIK), rank 1
      PROCEDURE,PASS :: read_i1
      !> Read: integer(SIK), rank 2
      PROCEDURE,PASS :: read_i2
      !> Read: integer(SIK), rank 3
      PROCEDURE,PASS :: read_i3
      !> Read: logical(SBK), rank 1
      PROCEDURE,PASS :: read_l1
      !> Read: logical(SBK), rank 2
      PROCEDURE,PASS :: read_l2
      !> Read: logical(SBK), rank 3)
      PROCEDURE,PASS :: read_l3
      ! ...
      !> Read data from a dataset in the file.
      GENERIC  :: read => read_d1,read_d2,read_d3,read_s1,read_s2,read_s3, &
            read_i1,read_i2,read_i3,read_l1,read_l2,read_l3
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
    SUBROUTINE init_HDF5FileType(this,filename,mode,pe)
      CHARACTER(LEN=*),PARAMETER :: myName='init_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: filename
      CHARACTER(LEN=*),INTENT(IN) :: mode
      TYPE(MPI_EnvType),INTENT(IN),TARGET,OPTIONAL :: pe
      CHARACTER(LEN=MAX_PATH_LENGTH) :: fpath
      CHARACTER(LEN=MAX_FNAME_LENGTH) :: fname
      CHARACTER(LEN=MAX_FEXT_LENGTH) :: fext
#ifdef MPACT_HAVE_HDF5
      INTEGER(HID_T) :: error
#endif
      IF(.NOT.ASSOCIATED(this%e)) THEN
        ALLOCATE(this%e)
      ENDIF
#ifdef MPACT_HAVE_HDF5
#ifdef HAVE_MPI
      ! Set up the communicator
      CALL this%pe%init(PE_COMM_WORLD)
#endif

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
#ifdef MPACT_HAVE_HDF5
      INTEGER(HID_T) :: error
      ! Close the HDF5 file.
      CALL h5fclose_f(this%file_id,error)

      ! Close the HDF5 interface. This can only be done once all calls to the
      ! HDF5 library are complete.
      CALL h5close_f(error)
#ifdef HAVE_MPI
!      CALL this%pe%finalize()
      CALL this%pe%clear()
#endif
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
#ifdef MPACT_HAVE_HDF5 
      INTEGER(HID_T) :: acc
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: plist_id

      CALL h5pcreate_f(H5P_FILE_ACCESS_F,plist_id,error)
      !TODO error
#ifdef HAVE_MPI
      !TODO something more robust than MPI_COMM_WORLD
      CALL h5pset_fapl_mpio_f(plist_id,MPI_COMM_WORLD,MPI_INFO_NULL,error)
      !TODO error
#endif

      ! Decide what access type to use
      SELECTCASE(TRIM(file%mode))
      CASE('READ')
        acc=H5F_ACC_RDONLY_F
        CALL h5fopen_f(file%fullname,acc,file%file_id,error,access_prp=plist_id)
      CASE('WRITE')
        acc=H5F_ACC_RDWR_F
        CALL h5fopen_f(file%fullname,acc,file%file_id,error,access_prp=plist_id)
      CASE('NEW')
        acc=H5F_ACC_TRUNC_F
        CALL h5fcreate_f(file%fullname,acc,file%file_id,error,access_prp=plist_id)
      CASE DEFAULT
        CALL file%e%raiseError(myName//": Unrecognized access mode.")
      ENDSELECT

      CALL h5pclose_f(plist_id,error)
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
#ifdef MPACT_HAVE_HDF5
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
#ifdef MPACT_HAVE_HDF5
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
#ifdef MPACT_HAVE_HDF5
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
#ifdef MPACT_HAVE_HDF5
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
    SUBROUTINE write_d1(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writed1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),ALLOCATABLE,INTENT(IN) :: data(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF
      
      ! stash offset
      offset(1) = LBOUND(data,1)-1
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(data)
      
      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ELSE
        gdims=ldims
      ENDIF
      
      !Create an HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace
#ifdef HAVE_MPI
      ! Make sure that the global dims are present if needed
      IF (this%pe%rank == 0)THEN
        IF(.NOT.PRESENT(gdims_in))THEN
          CALL this%e%raiseError(myName//': For parallel,write, global '//&
            'dimensions are required.')
        ENDIF
      ENDIF
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set chunk.')
      ENDIF
#endif
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF
      
      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_DOUBLE, gspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF
      
      ! Destroy the property list
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close parameter list.')
      ENDIF
      
      ! Select the global dataspace for the dataset
      CALL h5dget_space_f(dset_id,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select global dataspace'//&
          ' for the dataset.')
      ENDIF
      
      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property list '//&
          'for write operation.')
      ENDIF
#ifdef HAVE_MPI
      ! Set to parallel write
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set parallel write.')
      ENDIF
      
      ! Select the hyperslab
!      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_
!> @brief Write a rank-1 array of doubles to a datasetF,offset,one,error, &
!                                 one,ldims)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select hyperslab.')
      ENDIF
#endif
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, gdims, error, &
                      dspace_id,gspace_id,plist_id)
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
      CALL h5sclose_f(gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
      
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_d1
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of doubles to a dataset
    SUBROUTINE write_d2(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writed2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),ALLOCATABLE,INTENT(IN) :: data(:,:)
      INTEGER(SIK),DIMENSION(2),INTENT(IN),OPTIONAL :: gdims_in
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(2) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=2
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,gspace_id,dset_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF

      ! stash offset
      offset(1) = LBOUND(data,1)-1
      offset(2) = LBOUND(data,2)-1

      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(data)

      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ELSE
        gdims=ldims
      ENDIF

      ! Create and HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace. This changes for parallel datasets.
#ifdef HAVE_MPI
      ! Make sure that the global dims are present if needed
      IF (this%pe%rank == 0)THEN
        IF(.NOT.PRESENT(gdims_in))THEN
          CALL this%e%raiseError(myName//': For parallel write, global '//&
          'dimensions are required.')
        ENDIF
      ENDIF
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set chunk.')
      ENDIF
#endif
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_DOUBLE, gspace_id, &
                       dset_id,error,plist_id)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF

      ! Destroy the property list
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not destroy property list.')
      ENDIF
      
      ! Select the global dataspace for the dataset
      CALL h5dget_space_f(dset_id,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select global dataspace.')
      ENDIF

      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property list.')
      ENDIF
      
#ifdef HAVE_MPI
      ! Set to parallel write
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set to parallel write.')
      ENDIF
      ! Select the hyperslab
      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_F,offset,one,error, &
                                 one,ldims)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select hyperslab.')
      ENDIF
#endif
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, gdims, error, &
                      dspace_id,gspace_id,plist_id)
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
      CALL h5sclose_f(gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
      
      CALL h5pclose_f(plist_id,error)
#endif
    ENDSUBROUTINE write_d2
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of doubles to a dataset
    SUBROUTINE write_d3(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writed3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),ALLOCATABLE,INTENT(IN) :: data(:,:,:)
      INTEGER(SIK),DIMENSION(3),INTENT(IN),OPTIONAL :: gdims_in
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(3) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=3
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,gspace_id,dset_id,plist_id


      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF

      ! stash offset
      offset(1) = LBOUND(data,1)-1
      offset(2) = LBOUND(data,2)-1

      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(data)

      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ELSE
        gdims=ldims
      ENDIF

      ! Create and HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace. This changes for parallel datasets.
#ifdef HAVE_MPI
      ! Make sure that the global dims are present if needed
      IF (this%pe%rank == 0)THEN
        IF(.NOT.PRESENT(gdims_in))THEN
          CALL this%e%raiseError(myName//': For parallel write, global '//&
          'dimensions are required.')
        ENDIF
      ENDIF
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set chunk.')
      ENDIF
#endif
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_DOUBLE, gspace_id, &
                       dset_id,error,plist_id)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF

      ! Destroy the property list
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not destroy property list.')
      ENDIF
      
      ! Select the global dataspace for the dataset
      CALL h5dget_space_f(dset_id,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select global dataspace.')
      ENDIF

      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property list.')
      ENDIF
      
#ifdef HAVE_MPI
      ! Set to parallel write
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set to parallel write.')
      ENDIF
      ! Select the hyperslab
      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_F,offset,one,error, &
                                 one,ldims)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select hyperslab.')
      ENDIF
#endif
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, gdims, error, &
                      dspace_id,gspace_id,plist_id)
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
      CALL h5sclose_f(gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
      
      CALL h5pclose_f(plist_id,error)
#endif
    ENDSUBROUTINE write_d3
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of reals to a dataset
    SUBROUTINE write_s1(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writer1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),ALLOCATABLE,INTENT(IN) :: data(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF
      
      ! stash offset
      offset(1) = LBOUND(data,1)-1
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(data)
      
      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ELSE
        gdims=ldims
      ENDIF
      
      !Create an HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace
#ifdef HAVE_MPI
      ! Make sure that the global dims are present if needed
      IF (this%pe%rank == 0)THEN
        IF(.NOT.PRESENT(gdims_in))THEN
          CALL this%e%raiseError(myName//': For parallel write, global '//&
            'dimensions are required.')
        ENDIF
      ENDIF
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set chunk.')
      ENDIF
#endif
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF
      
      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_REAL, gspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF
      
      ! Destroy the property list
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close parameter list.')
      ENDIF
      
      ! Select the global dataspace for the dataset
      CALL h5dget_space_f(dset_id,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select global dataspace'//&
          ' for the dataset.')
      ENDIF
      
      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property list '//&
          'for write operation.')
      ENDIF
#ifdef HAVE_MPI
      ! Set to parallel write
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set parallel write.')
      ENDIF
      
      ! Select the hyperslab
      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_F,offset,one,error, &
                                 one,ldims)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select hyperslab.')
      ENDIF
#endif
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL, data, gdims, error, &
                      dspace_id,gspace_id,plist_id)
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
      CALL h5sclose_f(gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
      
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_s1
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of reals to a dataset
    SUBROUTINE write_s2(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writer2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),ALLOCATABLE,INTENT(IN) :: data(:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(2),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(2) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=2
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF
      
      ! stash offset
      offset(1) = LBOUND(data,1)-1
      offset(2) = LBOUND(data,1)-1
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(data)
      
      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ELSE
        gdims=ldims
      ENDIF
      
      !Create an HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace
#ifdef HAVE_MPI
      ! Make sure that the global dims are present if needed
      IF (this%pe%rank == 0)THEN
        IF(.NOT.PRESENT(gdims_in))THEN
          CALL this%e%raiseError(myName//': For parallel,write, global '//&
            'dimensions are required.')
        ENDIF
      ENDIF
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set chunk.')
      ENDIF
#endif
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF
      
      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_REAL, gspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF
      
      ! Destroy the property list
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close parameter list.')
      ENDIF
      
      ! Select the global dataspace for the dataset
      CALL h5dget_space_f(dset_id,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select global dataspace'//&
          ' for the dataset.')
      ENDIF
      
      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property list '//&
          'for write operation.')
      ENDIF
#ifdef HAVE_MPI
      ! Set to parallel write
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set parallel write.')
      ENDIF
      
      ! Select the hyperslab
      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_F,offset,one,error, &
                                 one,ldims)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select hyperslab.')
      ENDIF
#endif
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL, data, gdims, error, &
                      dspace_id,gspace_id,plist_id)
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
      CALL h5sclose_f(gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
      
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_s2
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of reals to a dataset
    SUBROUTINE write_s3(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writer3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),ALLOCATABLE,INTENT(IN) :: data(:,:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(3),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(3) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=3
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF
      
      ! stash offset
      offset(1) = LBOUND(data,1)-1
      offset(2) = LBOUND(data,2)-1
      offset(3) = LBOUND(data,3)-1
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(data)
      
      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ELSE
        gdims=ldims
      ENDIF
      
      !Create an HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace
#ifdef HAVE_MPI
      ! Make sure that the global dims are present if needed
      IF (this%pe%rank == 0)THEN
        IF(.NOT.PRESENT(gdims_in))THEN
          CALL this%e%raiseError(myName//': For parallel,write, global '//&
            'dimensions are required.')
        ENDIF
      ENDIF
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set chunk.')
      ENDIF
#endif
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF
      
      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_REAL, gspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF
      
      ! Destroy the property list
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close parameter list.')
      ENDIF
      
      ! Select the global dataspace for the dataset
      CALL h5dget_space_f(dset_id,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select global dataspace'//&
          ' for the dataset.')
      ENDIF
      
      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property list '//&
          'for write operation.')
      ENDIF
#ifdef HAVE_MPI
      ! Set to parallel write
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set parallel write.')
      ENDIF
      
      ! Select the hyperslab
      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_F,offset,one,error, &
                                 one,ldims)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select hyperslab.')
      ENDIF
#endif
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL, data, gdims, error, &
                      dspace_id,gspace_id,plist_id)
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
      CALL h5sclose_f(gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
      
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_s3
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of logicals to a dataset
    SUBROUTINE write_l1(this,dsetname,data,gdims_in)
      IMPLICIT NONE
      CHARACTER(LEN=*),PARAMETER :: myName='writel1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),ALLOCATABLE,INTENT(IN) :: data(:)
      CHARACTER :: datac(1:SIZE(data))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
      INTEGER(SIK) :: i
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF
      
      ! stash offset
      offset(1) = LBOUND(data,1)-1
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(data)
      
      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ELSE
        gdims=ldims
      ENDIF
      
      !Create an HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace
#ifdef HAVE_MPI
      ! Make sure that the global dims are present if needed
      IF (this%pe%rank == 0)THEN
        IF(.NOT.PRESENT(gdims_in))THEN
          CALL this%e%raiseError(myName//': For parallel,write, global '//&
            'dimensions are required.')
        ENDIF
      ENDIF
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set chunk.')
      ENDIF
#endif
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF
      
      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_CHARACTER, gspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF
      
      ! Destroy the property list
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close parameter list.')
      ENDIF
      
      ! Select the global dataspace for the dataset
      CALL h5dget_space_f(dset_id,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select global dataspace'//&
          ' for the dataset.')
      ENDIF
      
      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property list '//&
          'for write operation.')
      ENDIF
#ifdef HAVE_MPI
      ! Set to parallel write
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set parallel write.')
      ENDIF
      
      ! Select the hyperslab
      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_F,offset,one,error, &
                                 one,ldims)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select hyperslab.')
      ENDIF
#endif

      ! Convert to surrogate character array, since HDF5 does not support
      ! Boolean variables
      datac='F'
      FORALL(i=1:SIZE(data),data(i))
        datac(i:i)='T'
      END FORALL
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, datac, gdims, error, &
                      dspace_id,gspace_id,plist_id)
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
      CALL h5sclose_f(gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
      
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_l1
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of logicals to a dataset
    SUBROUTINE write_l2(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writel1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),ALLOCATABLE,INTENT(IN) :: data(:,:)
      CHARACTER :: datac(SIZE(data,DIM=1),SIZE(data,DIM=2))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(2),INTENT(IN),OPTIONAL :: gdims_in
      INTEGER(SIK) :: i,j
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(2) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=2
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF
      
      ! stash offset
      offset(1) = LBOUND(data,1)-1
      offset(2) = LBOUND(data,2)-1
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(data)
      
      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ELSE
        gdims=ldims
      ENDIF
      
      !Create an HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace
#ifdef HAVE_MPI
      ! Make sure that the global dims are present if needed
      IF (this%pe%rank == 0)THEN
        IF(.NOT.PRESENT(gdims_in))THEN
          CALL this%e%raiseError(myName//': For parallel,write, global '//&
            'dimensions are required.')
        ENDIF
      ENDIF
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set chunk.')
      ENDIF
#endif
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF
      
      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_CHARACTER, gspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF
      
      ! Destroy the property list
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close parameter list.')
      ENDIF
      
      ! Select the global dataspace for the dataset
      CALL h5dget_space_f(dset_id,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select global dataspace'//&
          ' for the dataset.')
      ENDIF
      
      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property list '//&
          'for write operation.')
      ENDIF
#ifdef HAVE_MPI
      ! Set to parallel write
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set parallel write.')
      ENDIF
      
      ! Select the hyperslab
      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_F,offset,one,error, &
                                 one,ldims)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select hyperslab.')
      ENDIF
#endif

      ! Convert to surrogate character array, since HDF5 does not support
      ! Boolean variables
      datac(:,:)='F'
      FORALL(i=1:SIZE(data,DIM=2),j=1:SIZE(data,DIM=2),data(i,j))
        datac(i,j)='T'
      END FORALL
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, datac, gdims, error, &
                      dspace_id,gspace_id,plist_id)
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
      CALL h5sclose_f(gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
      
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_l2
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of logicals to a dataset
    SUBROUTINE write_l3(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writel3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),ALLOCATABLE,INTENT(IN) :: data(:,:,:)
      CHARACTER :: datac(SIZE(data,DIM=1),SIZE(data,DIM=2),SIZE(data,DIM=3))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(3),INTENT(IN),OPTIONAL :: gdims_in
      INTEGER(SIK) :: i,j,k
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(3) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=3
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF
      
      ! stash offset
      offset(1) = LBOUND(data,1)-1
      offset(2) = LBOUND(data,2)-1
      offset(3) = LBOUND(data,3)-1
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(data)
      
      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ELSE
        gdims=ldims
      ENDIF
      
      !Create an HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace
#ifdef HAVE_MPI
      ! Make sure that the global dims are present if needed
      IF (this%pe%rank == 0)THEN
        IF(.NOT.PRESENT(gdims_in))THEN
          CALL this%e%raiseError(myName//': For parallel,write, global '//&
            'dimensions are required.')
        ENDIF
      ENDIF
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set chunk.')
      ENDIF
#endif
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF
      
      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_CHARACTER, gspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF
      
      ! Destroy the property list
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close parameter list.')
      ENDIF
      
      ! Select the global dataspace for the dataset
      CALL h5dget_space_f(dset_id,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select global dataspace'//&
          ' for the dataset.')
      ENDIF
      
      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property list '//&
          'for write operation.')
      ENDIF
#ifdef HAVE_MPI
      ! Set to parallel write
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set parallel write.')
      ENDIF
      
      ! Select the hyperslab
      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_F,offset,one,error, &
                                 one,ldims)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select hyperslab.')
      ENDIF
#endif

      ! Convert to surrogate character array, since HDF5 does not support
      ! Boolean variables
      datac(:,:,:)='F'
      FORALL(i=1:SIZE(data,DIM=1),j=1:SIZE(data,DIM=2),k=1:SIZE(data,DIM=3) &
            ,data(i,j,k))
        datac(i,j,k)='T'
      END FORALL
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, datac, gdims, error, &
                      dspace_id,gspace_id,plist_id)
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
      CALL h5sclose_f(gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
      
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_l3
    !-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of integers to a dataset
    SUBROUTINE write_i1(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writei1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SIK),ALLOCATABLE,INTENT(IN) :: data(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF
      
      ! stash offset
      offset(1) = LBOUND(data,1)-1
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(data)
      
      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ELSE
        gdims=ldims
      ENDIF
      
      !Create an HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace
#ifdef HAVE_MPI
      ! Make sure that the global dims are present if needed
      IF (this%pe%rank == 0)THEN
        IF(.NOT.PRESENT(gdims_in))THEN
          CALL this%e%raiseError(myName//': For parallel,write, global '//&
            'dimensions are required.')
        ENDIF
      ENDIF
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set chunk.')
      ENDIF
#endif
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF
      
      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_DOUBLE, gspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF
      
      ! Destroy the property list
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close parameter list.')
      ENDIF
      
      ! Select the global dataspace for the dataset
      CALL h5dget_space_f(dset_id,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select global dataspace'//&
          ' for the dataset.')
      ENDIF
      
      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property list '//&
          'for write operation.')
      ENDIF
#ifdef HAVE_MPI
      ! Set to parallel write
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set parallel write.')
      ENDIF
      
      ! Select the hyperslab
      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_F,offset,one,error, &
                                 one,ldims)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select hyperslab.')
      ENDIF
#endif
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, gdims, error, &
                      dspace_id,gspace_id,plist_id)
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
      CALL h5sclose_f(gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
      
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_i1
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of integers to a dataset
    SUBROUTINE write_i2(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writei2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SIK),ALLOCATABLE,INTENT(IN) :: data(:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(2),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(2) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=2
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF
      
      ! stash offset
      offset(1) = LBOUND(data,1)-1
      offset(2) = LBOUND(data,2)-1
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(data)
      
      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ELSE
        gdims=ldims
      ENDIF
      
      !Create an HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace
#ifdef HAVE_MPI
      ! Make sure that the global dims are present if needed
      IF (this%pe%rank == 0)THEN
        IF(.NOT.PRESENT(gdims_in))THEN
          CALL this%e%raiseError(myName//': For parallel,write, global '//&
            'dimensions are required.')
        ENDIF
      ENDIF
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set chunk.')
      ENDIF
#endif
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF
      
      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_DOUBLE, gspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF
      
      ! Destroy the property list
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close parameter list.')
      ENDIF
      
      ! Select the global dataspace for the dataset
      CALL h5dget_space_f(dset_id,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select global dataspace'//&
          ' for the dataset.')
      ENDIF
      
      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property list '//&
          'for write operation.')
      ENDIF
#ifdef HAVE_MPI
      ! Set to parallel write
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set parallel write.')
      ENDIF
      
      ! Select the hyperslab
      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_F,offset,one,error, &
                                 one,ldims)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select hyperslab.')
      ENDIF
#endif
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, gdims, error, &
                      dspace_id,gspace_id,plist_id)
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
      CALL h5sclose_f(gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
      
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_i2
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of integers to a dataset
    SUBROUTINE write_i3(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writei3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SIK),ALLOCATABLE,INTENT(IN) :: data(:,:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(3),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(3) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=3
      
      INTEGER(HID_T) :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF
      
      ! stash offset
      offset(1) = LBOUND(data,1)-1
      offset(2) = LBOUND(data,2)-1
      offset(3) = LBOUND(data,3)-1
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(data)
      
      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ELSE
        gdims=ldims
      ENDIF
      
      !Create an HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace
#ifdef HAVE_MPI
      ! Make sure that the global dims are present if needed
      IF (this%pe%rank == 0)THEN
        IF(.NOT.PRESENT(gdims_in))THEN
          CALL this%e%raiseError(myName//': For parallel,write, global '//&
            'dimensions are required.')
        ENDIF
      ENDIF
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set chunk.')
      ENDIF
#endif
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF
      
      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_DOUBLE, gspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF
      
      ! Destroy the property list
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close parameter list.')
      ENDIF
      
      ! Select the global dataspace for the dataset
      CALL h5dget_space_f(dset_id,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select global dataspace'//&
          ' for the dataset.')
      ENDIF
      
      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property list '//&
          'for write operation.')
      ENDIF
#ifdef HAVE_MPI
      ! Set to parallel write
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set parallel write.')
      ENDIF
      
      ! Select the hyperslab
      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_F,offset,one,error, &
                                 one,ldims)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select hyperslab.')
      ENDIF
#endif
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, gdims, error, &
                      dspace_id,gspace_id,plist_id)
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
      CALL h5sclose_f(gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
      
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_i3
!
!-------------------------------------------------------------------------------
!> @brief Write a single character string to a dataset
    SUBROUTINE write_c1(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='write_string_helper_HDF5FileType'
      INTEGER(SIK) :: i,j
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),INTENT(IN) :: data
      CHARACTER(LEN=LEN_TRIM(data)) :: datas
      CHARACTER, ALLOCATABLE :: datac(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1
      
      INTEGER :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.this%isinit)THEN
        CALL this%e%raiseError(myName//': File object not initialized.')
      ENDIF

      ! Check that the file is writable. Best to catch this before HDF5 does.
      IF(.NOT.this%isWrite())THEN
        CALL this%e%raiseError(myName//': File is readonly!')
        RETURN
      ENDIF

      ! Convert StringType to character vector
      datas=TRIM(data)
      ALLOCATE(datac(LEN(datas)))
      DO i=1,SIZE(datac)
        datac(i)=datas(i:i)
      ENDDO
      
      ! stash offset
      offset(1) = LBOUND(datac,1)-1
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(datac)
      
      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ELSE
        gdims=ldims
      ENDIF
      
      !Create an HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace
#ifdef HAVE_MPI
      ! Make sure that the global dims are present if needed
      IF (this%pe%rank == 0)THEN
        IF(.NOT.PRESENT(gdims_in))THEN
          CALL this%e%raiseError(myName//': For parallel,write, global '//&
            'dimensions are required.')
        ENDIF
      ENDIF
      CALL h5pset_chunk_f(plist_id,rank,ldims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set chunk.')
      ENDIF
#endif
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF
      
      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataspace.')
      ENDIF

      ! Create the dataset
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_CHARACTER, gspace_id, &
                       dset_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create dataset.')
      ENDIF
      
      ! Destroy the property list
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close parameter list.')
      ENDIF
      
      ! Select the global dataspace for the dataset
      CALL h5dget_space_f(dset_id,gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select global dataspace'//&
          ' for the dataset.')
      ENDIF
      
      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property list '//&
          'for write operation.')
      ENDIF
#ifdef HAVE_MPI
      ! Set to parallel write
      CALL h5pset_dxpl_mpio_f(plist_id,H5FD_MPIO_COLLECTIVE_F,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not set parallel write.')
      ENDIF
      
      ! Select the hyperslab
      CALL h5sselect_hyperslab_f(gspace_id,H5S_SELECT_SET_F,offset,one,error, &
                                 one,ldims)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not select hyperslab.')
      ENDIF
#endif
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, datac, gdims, error, &
                      dspace_id,gspace_id,plist_id)
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
      CALL h5sclose_f(gspace_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the dataspace.')
      ENDIF
      
      CALL h5pclose_f(plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_c1
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of strings to a dataset
    SUBROUTINE write_string_helper(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writec1_HDF5FileType'
      INTEGER(SIK) :: i
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(IN) :: data(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path_shape,path_string
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
      INTEGER(SIK),ALLOCATABLE :: data_shape(:)
      
      
      ! This and write_c1 will swap names.  Just did this to make sure it builds
      ! until I can finish.
      
#ifdef MPACT_HAVE_HDF5
      data_shape=SHAPE(data)
      
      
      WRITE(path_shape,*) TRIM(path),"SHAPE"
      CALL this%write_i1(path_shape,data_shape)
      
      DO i=1,SIZE(data)
        WRITE(path_string,*) TRIM(path),i
!        CALL this%write_string_helper(path_string,data(i))
      ENDDO
      
    
#endif MPACT_HAVE_HDF5
    ENDSUBROUTINE write_string_helper
!
!-------------------------------------------------------------------------------
    SUBROUTINE read_d1(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readd1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: data(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
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
    SUBROUTINE read_d2(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readd3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: data(:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(2) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=2
      
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
        ALLOCATE(data(dims(1),dims(2)))
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
    ENDSUBROUTINE read_d2

!
!-------------------------------------------------------------------------------
    SUBROUTINE read_d3(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readd3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: data(:,:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(3) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=3
      
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
        ALLOCATE(data(dims(1),dims(2),dims(3)))
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
    ENDSUBROUTINE read_d3

!
!-------------------------------------------------------------------------------
SUBROUTINE read_s1(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readr1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: data(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
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
      mem=H5T_NATIVE_REAL
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
    ENDSUBROUTINE read_s1
!
!-------------------------------------------------------------------------------
SUBROUTINE read_s2(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readr2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: data(:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(2) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=2
      
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
        ALLOCATE(data(dims(1),dims(2)))
      ENDIF

      ! Read the dataset
      mem=H5T_NATIVE_REAL
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
    ENDSUBROUTINE read_s2
!
!-------------------------------------------------------------------------------
SUBROUTINE read_s3(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readr3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: data(:,:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(3) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=3
      
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
        ALLOCATE(data(dims(1),dims(2),dims(3)))
      ENDIF

      ! Read the dataset
      mem=H5T_NATIVE_REAL
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
    ENDSUBROUTINE read_s3
!
!-------------------------------------------------------------------------------
    SUBROUTINE read_i1(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readi1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SIK),ALLOCATABLE,INTENT(INOUT) :: data(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
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
      mem=H5T_NATIVE_INTEGER
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
    ENDSUBROUTINE read_i1
!
!-------------------------------------------------------------------------------
    SUBROUTINE read_i2(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readi2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SIK),ALLOCATABLE,INTENT(INOUT) :: data(:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(2) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=2
      
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
        ALLOCATE(data(dims(1),dims(2)))
      ENDIF

      ! Read the dataset
      mem=H5T_NATIVE_INTEGER
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
    ENDSUBROUTINE read_i2
!
!-------------------------------------------------------------------------------
    SUBROUTINE read_i3(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readi3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SIK),ALLOCATABLE,INTENT(INOUT) :: data(:,:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(3) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=3
      
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
        ALLOCATE(data(dims(1),dims(2),dims(3)))
      ENDIF

      ! Read the dataset
      mem=H5T_NATIVE_INTEGER
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
    ENDSUBROUTINE read_i3
!
!-------------------------------------------------------------------------------
SUBROUTINE read_l1(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readl1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: data(:)
      INTEGER(SIK) :: datac(SIZE(data))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER :: i
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
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
      mem=H5T_NATIVE_INTEGER
      CALL h5dread_f(dset_id,mem,datac,dims,error)
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
      
      ! Convert from surrogate character array to boolean array
      data=.FALSE.
      FORALL(i=1:SIZE(data),datac(i)==1)
        data(i)=.TRUE.
      END FORALL

#endif
    ENDSUBROUTINE read_l1
!
!-------------------------------------------------------------------------------
SUBROUTINE read_l2(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readl2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: data(:,:)
      INTEGER(SIK) :: datac(SIZE(data,DIM=1),SIZE(data,DIM=2))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK) :: i,j
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(2) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=2
      
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
        ALLOCATE(data(dims(1),dims(2)))
      ENDIF

      ! Read the dataset
      mem=H5T_NATIVE_INTEGER
      CALL h5dread_f(dset_id,mem,datac,dims,error)
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
      
      ! Convert from surrogate character array to boolean array
      data=.FALSE.
      FORALL(i=1:SIZE(data,DIM=1),j=1:SIZE(data,DIM=2), &
            datac(i,j)==1)
        data(i,j)=.TRUE.
      END FORALL

#endif
    ENDSUBROUTINE read_l2
!
!-------------------------------------------------------------------------------
SUBROUTINE read_l3(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readl3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: data(:,:,:)
      INTEGER(SIK) :: datac(SIZE(data,DIM=1),SIZE(data,DIM=2),SIZE(data,DIM=3))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK) :: i,j,k
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(3) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=3
      
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
        ALLOCATE(data(dims(1),dims(2),dims(3)))
      ENDIF

      ! Read the dataset
      mem=H5T_NATIVE_INTEGER
      CALL h5dread_f(dset_id,mem,datac,dims,error)
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
      
      ! Convert from surrogate character array to boolean array
      data(:,:,:)=.FALSE.
      FORALL(i=1:SIZE(data,DIM=1),j=1:SIZE(data,DIM=2),k=1:SIZE(data,DIM=3), &
            datac(i,j,k)==1)
        data(i,j,k)=.TRUE.
      END FORALL

#endif
    ENDSUBROUTINE read_l3
!
!-------------------------------------------------------------------------------
!> @brief Convert a path provided with '->' separators to '/'
!> @param path the path string to convert.
!>
!> Paths in MPACT use '->' to resolve heirarchy. HSF5 uses '/'.
    FUNCTION convertPath(path)
      CHARACTER(LEN=*),INTENT(IN) :: path
      CHARACTER(LEN=80) :: convertpath

      INTEGER :: ipos,ipos2,last,ind

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
