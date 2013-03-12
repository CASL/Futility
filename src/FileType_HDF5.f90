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

  !> This type extends the base file type, and adds support for writing to and
  !List of Public Members
  PUBLIC :: HDF5FileType

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
      !> Write: double(SKD)
      PROCEDURE,PASS :: write_d0
      !> Write: double(SDK), rank 1
      PROCEDURE,PASS :: write_d1
      !> Write: double(SDK), rank 2
      PROCEDURE,PASS :: write_d2
      !> Write: double(SDK), rank 3
      PROCEDURE,PASS :: write_d3
      !> Write: real(SSK)
      PROCEDURE,PASS :: write_s0
      !> Write: real(SSK), rank 1
      PROCEDURE,PASS :: write_s1
      !> Write: real(SSK), rank 2
      PROCEDURE,PASS :: write_s2
      !> Write: real(SSK), rank 3
      PROCEDURE,PASS :: write_s3
      !> Write: logical(SBK)
      PROCEDURE,PASS :: write_b0
      !> Write: logical(SBK), rank 1
      PROCEDURE,PASS :: write_b1
      !> Write: logical(SBK), rank 2
      PROCEDURE,PASS :: write_b2
      !> Write: logical(SBK), rank 3
      PROCEDURE,PASS :: write_b3
      !> Write: integer(SNK), rank 0
      PROCEDURE,PASS :: write_n0
      !> Write: integer(SNK), rank 1
      PROCEDURE,PASS :: write_n1
      !> Write: integer(SNK), rank 2
      PROCEDURE,PASS :: write_n2
      !> Write: integer(SNK), rank 3
      PROCEDURE,PASS :: write_n3
      !> Write: integer(SLK), rank 0
      PROCEDURE,PASS :: write_l0
      !> Write: integer(SLK), rank 1
      PROCEDURE,PASS :: write_l1
      !> Write: integer(SLK), rank 2
      PROCEDURE,PASS :: write_l2
      !> Write: integer(SLK), rank 3
      PROCEDURE,PASS :: write_l3
      !> Write: character string
      PROCEDURE,PASS :: write_c0
      !> Write: character string, rank 1 helper
      PROCEDURE,PASS :: write_c1_helper
      !> Write: character string, rank 1
      PROCEDURE,PASS :: write_c1
      !> Write: character string, rank 2 helper
      PROCEDURE,PASS :: write_c2_helper
      !> Write: character string, rank 2
      PROCEDURE,PASS :: write_c2
      !> Write: character string, rank 3 helper
      PROCEDURE,PASS :: write_c3_helper
      !> Write: character string, rank 3
      PROCEDURE,PASS :: write_c3
      !> Write data to the file as a dataset
      GENERIC :: write => write_d0,write_d1,write_d2,write_d3,write_s0, &
      write_s1,write_s2,write_s3,write_b0,write_b1,write_b2,write_b3,write_n0, &
      write_n1,write_n2,write_n3,write_c0,write_c1_helper,write_c1, &
      write_c2_helper,write_c2,write_c3_helper,write_c3,write_l0,write_l1, &
      write_l2,write_l3
      !> Read: real(SDK)
      PROCEDURE,PASS :: read_d0
      !> Read: real(SDK), rank 1
      PROCEDURE,PASS :: read_d1
      !> Read: real(SDK), rank 2
      PROCEDURE,PASS :: read_d2
      !> Read: real(SDK), rank 3
      PROCEDURE,PASS :: read_d3
      !> Read: real(SSK)
      PROCEDURE,PASS :: read_s0
      !> Read: real(SSK), rank 1
      PROCEDURE,PASS :: read_s1
      !> Read: real(SSK), rank 2
      PROCEDURE,PASS :: read_s2
      !> Read: real(SSK), rank 3
      PROCEDURE,PASS :: read_s3
      !> Read: integer(SNK)
      PROCEDURE,PASS :: read_n0
      !> Read: integer(SNK), rank 1
      PROCEDURE,PASS :: read_n1
      !> Read: integer(SNK), rank 2
      PROCEDURE,PASS :: read_n2
      !> Read: integer(SNK), rank 3
      PROCEDURE,PASS :: read_n3
      !> Read: integer(SLK)
      PROCEDURE,PASS :: read_l0
      !> Read: integer(SLK), rank 1
      PROCEDURE,PASS :: read_l1
      !> Read: integer(SLK), rank 2
      PROCEDURE,PASS :: read_l2
      !> Read: integer(SLK), rank 3
      PROCEDURE,PASS :: read_l3
      !> Read: logical(SBK)
      PROCEDURE,PASS :: read_b0
      !> Read: logical(SBK), rank 1
      PROCEDURE,PASS :: read_b1
      !> Read: logical(SBK), rank 2
      PROCEDURE,PASS :: read_b2
      !> Read: logical(SBK), rank 3)
      PROCEDURE,PASS :: read_b3
      !> Read: character string
      PROCEDURE,PASS :: read_c0
      !> Read: character string, rank 1
      PROCEDURE,PASS :: read_c1
      !> Read: character string, rank 2
      PROCEDURE,PASS :: read_c2
      !> Read: character string, rank 3
      PROCEDURE,PASS :: read_c3
      ! ...
      !> Read data from a dataset in the file.
      GENERIC  :: read => read_d1,read_d2,read_d3,read_s1,read_s2,read_s3, &
            read_l1,read_l2,read_l3,read_b1,read_b2,read_b3,read_c0,read_d0, &
            read_s0,read_l0,read_b0,read_c1,read_c2,read_c3,read_n0,read_n1, &
            read_n2,read_n3
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
#ifdef MPACT_HAVE_HDF5
      INTEGER(HID_T) :: error
#endif
      IF(.NOT.ASSOCIATED(this%e)) THEN
        ALLOCATE(this%e)
      ENDIF
#ifdef MPACT_HAVE_HDF5

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
      INTEGER(HSIZE_T) :: i
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
!> @brief Write a double to a dataset
    SUBROUTINE write_d0(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writed0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),INTENT(IN) :: data
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
      offset(1) = 0
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=1
      
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
    ENDSUBROUTINE write_d0
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
      
      ! Create the dataspace. 
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
      
      ! Create the dataspace.
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
!> @brief Write a real to a dataset
    SUBROUTINE write_s0(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writes0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),INTENT(IN) :: data
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
      offset(1) = 0
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=1
      
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
    ENDSUBROUTINE write_s0
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of reals to a dataset
    SUBROUTINE write_s1(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writes1_HDF5FileType'
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
      CHARACTER(LEN=*),PARAMETER :: myName='writes2_HDF5FileType'
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
      CHARACTER(LEN=*),PARAMETER :: myName='writes3_HDF5FileType'
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
!> @brief Write a logical to a dataset
    SUBROUTINE write_b0(this,dsetname,data,gdims_in)
      IMPLICIT NONE
      CHARACTER(LEN=*),PARAMETER :: myName='writeb0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),INTENT(IN) :: data
      CHARACTER :: datac
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
      offset(1) = 0
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=1
      
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

      ! Convert to surrogate character array, since HDF5 does not support
      ! Boolean variables
      IF(data .EQV. .TRUE.) THEN
        datac='T'
      ELSE
        datac='F'
      ENDIF
      
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
    ENDSUBROUTINE write_b0
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of logicals to a dataset
    SUBROUTINE write_b1(this,dsetname,data,gdims_in)
      IMPLICIT NONE
      CHARACTER(LEN=*),PARAMETER :: myName='writeb1_HDF5FileType'
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
    ENDSUBROUTINE write_b1
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of logicals to a dataset
    SUBROUTINE write_b2(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writeb2_HDF5FileType'
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

      ! Convert to surrogate character array, since HDF5 does not support
      ! Boolean variables
      datac(:,:)='F'
      FORALL(i=1:SIZE(data,DIM=1),j=1:SIZE(data,DIM=2),data(i,j))
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
    ENDSUBROUTINE write_b2
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of logicals to a dataset
    SUBROUTINE write_b3(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writeb3_HDF5FileType'
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
    ENDSUBROUTINE write_b3
!-------------------------------------------------------------------------------
!> @brief Write a 32-bit integer to a dataset
    SUBROUTINE write_n0(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writen0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),INTENT(IN) :: data
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
      offset(1) = 0
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=1
      
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
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_INTEGER, gspace_id, &
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
    ENDSUBROUTINE write_n0
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of 32-bit integers to a dataset
    SUBROUTINE write_n1(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writen1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),ALLOCATABLE,INTENT(IN) :: data(:)
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
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_INTEGER, gspace_id, &
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
    ENDSUBROUTINE write_n1
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of 32-bit integers to a dataset
    SUBROUTINE write_n2(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writen2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),ALLOCATABLE,INTENT(IN) :: data(:,:)
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
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_INTEGER, gspace_id, &
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
    ENDSUBROUTINE write_n2
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of 32-bit integers to a dataset
    SUBROUTINE write_n3(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writen3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),ALLOCATABLE,INTENT(IN) :: data(:,:,:)
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
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_INTEGER, gspace_id, &
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
    ENDSUBROUTINE write_n3
!
!-------------------------------------------------------------------------------
!> @brief Write a 64-bit "integer" to a dataset
    SUBROUTINE write_l0(this,dsetname,datat,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writel0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),INTENT(IN) :: datat
      REAL(SDK) :: data
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
      offset(1) = 0
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)

      ! Determine the dimensions for the dataspace
      ldims=1
      
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
      CALL h5dcreate_f(this%file_id, path, H5T_STD_I64LE, gspace_id, &
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
      
      ! Convert data type
      data=datat
      CALL this%e%raiseWarning(myName//': Converting from long integer to '// &
                  'double to accomodate HDF5!!!')
      
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
    ENDSUBROUTINE write_l0
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of 64-bit "integers" to a dataset
    SUBROUTINE write_l1(this,dsetname,datat,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writel1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),ALLOCATABLE,INTENT(IN) :: datat(:)
      REAL(SDK) :: data(SIZE(datat))
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
      CALL h5dcreate_f(this%file_id, path, H5T_STD_I64LE, gspace_id, &
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
      
      ! Convert to different datatyp
      data=datat
      CALL this%e%raiseWarning(myName//': Converting from long integer to '// &
                'double to accomodate HDF5!!!')
      
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
    ENDSUBROUTINE write_l1
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of 64-bit "integers" to a dataset
    SUBROUTINE write_l2(this,dsetname,datat,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writel2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),ALLOCATABLE,INTENT(IN) :: datat(:,:)
      REAL(SDK) :: data(SIZE(datat,1),SIZE(datat,2))
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
      CALL h5dcreate_f(this%file_id, path, H5T_STD_I64LE, gspace_id, &
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
      
      ! Convert data type
      data=datat
      CALL this%e%raiseWarning(myName//': Converting from long intger to '// &
                  'double to accomodate HDF5!!!')
      
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
    ENDSUBROUTINE write_l2
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of 64-bit "integers" to a dataset
    SUBROUTINE write_l3(this,dsetname,datat,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writel3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),ALLOCATABLE,INTENT(IN) :: datat(:,:,:)
      REAL(SDK) :: data(SIZE(datat,1),SIZE(datat,2),SIZE(datat,3))
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
      CALL h5dcreate_f(this%file_id, path, H5T_STD_I64LE, gspace_id, &
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
      
      ! Convert Data type
      data=datat
      CALL this%e%raiseWarning(myName//': Converting from long integer to '// &
                  'double to accomodate HDF5!!!')
      
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
    ENDSUBROUTINE write_l3
!
!-------------------------------------------------------------------------------
!> @brief Write a StringType to dataset
    SUBROUTINE write_c0(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writec0_HDF5FileType'
      INTEGER(SIK) :: i
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
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, datac, gdims, error, &
                      dspace_id,gspace_id,plist_id)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not write to the dataset.')
      ENDIF
      DEALLOCATE(datac)

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
    ENDSUBROUTINE write_c0
!
!-------------------------------------------------------------------------------
!> @brief Find max string length, then write rank-1 array of Strings to dataset
    SUBROUTINE write_c1_helper(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: MyName='writec1helper_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(IN) :: data(:)
      INTEGER(SIK) :: length_max,i
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
      
      length_max=0
      DO i=1,SIZE(data)
        length_max=MAXVAL(([LEN_TRIM(data(i)),length_max]))
      ENDDO
      
      IF(PRESENT(gdims_in))THEN
        CALL this%write(dsetname,data,length_max,[length_max,gdims_in])
      ELSE
        CALL this%write(dsetname,data,length_max)
      ENDIF
      
    ENDSUBROUTINE write_c1_helper

!
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of StringTypes to dataset
    SUBROUTINE write_c1(this,dsetname,data,length_max,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writec1_HDF5FileType'
      INTEGER(SIK) :: i,j
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(IN) :: data(:)
      INTEGER(SIK),INTENT(IN) :: length_max
      CHARACTER(LEN=length_max) :: datas
      CHARACTER :: datac(length_max,SIZE(data))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(2),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(2) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=2
      
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
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)
      
      ! Fill character array
      DO i=1,SIZE(data)
        datas=TRIM(data(i))
        DO j=1,length_max
          datac(j,i)=datas(j:j)
        ENDDO
      ENDDO
      
      ! stash offset
      offset(1) = LBOUND(datac,1)-1
      offset(2) = LBOUND(datac,2)-1

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
!> @brief Find max string length, then write rank-2 array of Strings to dataset
    SUBROUTINE write_c2_helper(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: MyName='writec2helper_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(IN) :: data(:,:)
      INTEGER(SIK) :: length_max,i,j
      INTEGER(SIK),DIMENSION(2),INTENT(IN),OPTIONAL :: gdims_in
      
      length_max=0
      DO j=1,SIZE(data,1)
        DO i=1,SIZE(data,2)
          length_max=MAXVAL(([LEN_TRIM(data(j,i)),length_max]))
        ENDDO
      ENDDO
      
      IF(PRESENT(gdims_in))THEN
        CALL this%write(dsetname,data,length_max,[length_max,gdims_in])
      ELSE
        CALL this%write(dsetname,data,length_max)
      ENDIF
      
    ENDSUBROUTINE write_c2_helper

!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of StringTypes to dataset
    SUBROUTINE write_c2(this,dsetname,data,length_max,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writec2_HDF5FileType'
      INTEGER(SIK) :: i,j,k
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SIK),INTENT(IN) :: length_max
      TYPE(StringType),ALLOCATABLE,INTENT(IN) :: data(:,:)
      CHARACTER :: datac(length_max,SIZE(data,1),SIZE(data,2))
      CHARACTER(LEN=length_max) :: datas
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(3),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(3) :: gdims,ldims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=3
      
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
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)
      
      
      DO k=1,SIZE(data,2)
        DO i=1,SIZE(data,1)
          datas=TRIM(data(i,k))
          DO j=1,length_max
            datac(j,i,k)=datas(j:j)
          ENDDO
        ENDDO
      ENDDO
      
      ! stash offset
      offset(1) = LBOUND(datac,1)-1
      offset(2) = LBOUND(datac,2)-1
      offset(3) = LBOUND(datac,3)-1

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(datac)
      
      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ENDIF
      
      !Create an HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter list.')
      ENDIF
      
      ! Create the dataspace
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
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_CHARACTER, &
                      gspace_id, dset_id,error)
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
        CALL this%e%raiseError(myName//': Could not select global '//&
                'dataspace for the dataset.')
      ENDIF
      
      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create property '//&
          'list for write operation.')
      ENDIF
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, datac, ldims, error, &
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
    ENDSUBROUTINE write_c2
!
!-------------------------------------------------------------------------------
!> @brief Find max string length, then write rank-3 array of Strings to dataset
    SUBROUTINE write_c3_helper(this,dsetname,data,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: MyName='writec3helper_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(IN) :: data(:,:,:)
      INTEGER(SIK) :: length_max,i,j,k
      INTEGER(SIK),DIMENSION(3),INTENT(IN),OPTIONAL :: gdims_in
      
      length_max=0
      DO k=1,SIZE(data,3)
        DO j=1,SIZE(data,1)
          DO i=1,SIZE(data,2)
            length_max=MAXVAL(([LEN_TRIM(data(j,i,k)),length_max]))
          ENDDO
        ENDDO
      ENDDO
      
      IF(PRESENT(gdims_in))THEN
        CALL this%write(dsetname,data,length_max,[length_max,gdims_in])
      ELSE
        CALL this%write(dsetname,data,length_max)
      ENDIF
      
    ENDSUBROUTINE write_c3_helper

!
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of StringTypes to dataset
    SUBROUTINE write_c3(this,dsetname,data,length_max,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writec3_HDF5FileType'
      INTEGER(SIK) :: i,j,k,m
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(IN) :: data(:,:,:)
      INTEGER(SIK),INTENT(IN) :: length_max
      CHARACTER(LEN=length_max) :: datas
      CHARACTER :: datac(length_max,SIZE(data,1),SIZE(data,2),SIZE(data,3))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK),DIMENSION(4),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(4) :: gdims,ldims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=4
      
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
      
      ! set one to ones. This is usually used for more complicated parallel
      ! chunking schemes, but we are doing a simplified case
      one=1

      ! Convert the path name
      path = convertPath(dsetname)
      
      ! Fill character array
      DO m=1,SIZE(data,3)
        DO k=1,SIZE(data,2)
          DO i=1,SIZE(data,1)
            datas=TRIM(data(i,k,m))
            DO j=1,length_max
              datac(j,i,k,m)=datas(j:j)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      
      ! stash offset
      offset(1) = LBOUND(datac,1)-1
      offset(2) = LBOUND(datac,2)-1
      offset(3) = LBOUND(datac,3)-1
      offset(4) = LBOUND(datac,4)-1

      ! Determine the dimensions for the dataspace
      ldims=SHAPE(datac)
      
      ! Store the dimensions from global if present
      IF(PRESENT(gdims_in))THEN
        gdims=gdims_in
      ENDIF
      
      !Create an HDF5 parameter list for the dataset creation.
      CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not create parameter'// &
                ' list.')
      ENDIF
      
      ! Create the dataspace
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
      CALL h5dcreate_f(this%file_id, path, H5T_NATIVE_CHARACTER, &
                      gspace_id, dset_id,error)
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
        CALL this%e%raiseError(myName//': Could not select global '//&
                'dataspace for the dataset.')
      ENDIF
      
      ! Create a property list for the write operation
      CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
      IF(error /= 0)THEN
         CALL this%e%raiseError(myName//': Could not create property '//&
          'list for write operation.')
      ENDIF
      
      ! Write to the dataset
      CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, datac, ldims, error,&
                      dspace_id,gspace_id,plist_id)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//': Could not write to the '// &
                    'dataset.')
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
    ENDSUBROUTINE write_c3
!-------------------------------------------------------------------------------
!> @brief Read a double from dataset
    SUBROUTINE read_d0(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readd0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),INTENT(INOUT) :: data
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
    ENDSUBROUTINE read_d0
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-2 array of doubles from dataset
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
!> @brief Read a rank-2 array of doubles from dataset
    SUBROUTINE read_d2(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readd2_HDF5FileType'
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
!> @brief Read a rank-3 array of doubles from dataset
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
!> @brief Read a real from dataset
    SUBROUTINE read_s0(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='reads0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),INTENT(INOUT) :: data
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
    ENDSUBROUTINE read_s0
!

!-------------------------------------------------------------------------------
!> @brief Read a rank-1 array of reals from dataset
    SUBROUTINE read_s1(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='reads1_HDF5FileType'
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
!> @brief Read a rank-2 array of reals from dataset
    SUBROUTINE read_s2(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='reads2_HDF5FileType'
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
!> @brief Read a rank-3 array of reals from dataset
    SUBROUTINE read_s3(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='read23_HDF5FileType'
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
!> @brief Read a 32-bit integer from dataset
    SUBROUTINE read_n0(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readn0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),INTENT(INOUT) :: data
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
    ENDSUBROUTINE read_n0
!

!-------------------------------------------------------------------------------
!> @brief Read a rank-1 array of 32-bit integers from dataset
    SUBROUTINE read_n1(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readn1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: data(:)
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
    ENDSUBROUTINE read_n1
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-2 array of 32-bit integers from dataset
    SUBROUTINE read_n2(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readn2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: data(:,:)
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
    ENDSUBROUTINE read_n2
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-3 array of 32-bit integers from dataset
    SUBROUTINE read_n3(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readn3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: data(:,:,:)
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
    ENDSUBROUTINE read_n3
!
!-------------------------------------------------------------------------------
!> @brief Read a 64-bit "integer" from dataset
    SUBROUTINE read_l0(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readl0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),INTENT(INOUT) :: data
      REAL(SDK) :: datat
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

      ! Read the dataset
      mem=H5T_NATIVE_DOUBLE
      CALL h5dread_f(dset_id,mem,datat,dims,error)
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
      
      ! Convert data type
      data=datat
      CALL this%e%raiseWarning(myName//": Converting from double to long "// &
                  "integer!")

#endif
    ENDSUBROUTINE read_l0
!

!-------------------------------------------------------------------------------
!> @brief Read a rank-1 array of 64-bit "integers" from dataset
    SUBROUTINE read_l1(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readl1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: data(:)
      INTEGER(SNK),ALLOCATABLE :: datat(:)
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
      ALLOCATE(datat(dims(1)))

      ! Read the dataset
      mem=H5T_NATIVE_INTEGER
      CALL h5dread_f(dset_id,mem,datat,dims,error)
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
      
      ! Conver data type
      data=datat
      CALL this%e%raiseWarning(myName//": Converting from double to long "// &
                  "integer!")

#endif
    ENDSUBROUTINE read_l1
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-2 array of 64-bit "integers" from dataset
    SUBROUTINE read_l2(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readl2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: data(:,:)
      INTEGER(SNK),ALLOCATABLE :: datat(:,:)
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
      ALLOCATE(datat(dims(1),dims(2)))

      ! Read the dataset
      mem=H5T_NATIVE_INTEGER
      CALL h5dread_f(dset_id,mem,datat,dims,error)
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
      
      ! Conver data type
      data=datat
      CALL this%e%raiseWarning(myName//": Converting from double to long "// &
                  "integer!")

#endif
    ENDSUBROUTINE read_l2
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-3 array of 64-bit "integers" from dataset
    SUBROUTINE read_l3(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readl3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: data(:,:,:)
      INTEGER(SNK),ALLOCATABLE :: datat(:,:,:)
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
      ALLOCATE(datat(dims(1),dims(2),dims(3)))

      ! Read the dataset
      mem=H5T_NATIVE_INTEGER
      CALL h5dread_f(dset_id,mem,datat,dims,error)
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
      
      ! Conver data type
      data=datat
      CALL this%e%raiseWarning(myName//": Converting from double to long "// &
                  "integer!")

#endif
    ENDSUBROUTINE read_l3
!
!-------------------------------------------------------------------------------
!> @brief Read a logical from dataset
    SUBROUTINE read_b0(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readb0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),INTENT(INOUT) :: data
      CHARACTER(LEN=1) :: datac
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

      ! Read the dataset
      mem=H5T_NATIVE_CHARACTER
      CALL h5dread_f(dset_id,mem,datac,dims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to read data from dataset.")
      ENDIF
      
      ! Convert to logical from character
      IF(datac=='F')THEN
        data=.FALSE.
      ELSE
        data=.TRUE.
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
    ENDSUBROUTINE read_b0
!

!-------------------------------------------------------------------------------
!> @brief Read a rank-1 array of logicals from dataset
    SUBROUTINE read_b1(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readb1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: data(:)
      CHARACTER,ALLOCATABLE :: datac(:)
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

      ! Allocate space in data if needed
      IF(ALLOCATED(data))THEN
        ! Make sure the data is the right size
        IF(ANY(SHAPE(data) /= dims))THEN
          CALL this%e%raiseError(myName//": data array is the wrong size.")
        ENDIF
      ELSE
        ! Allocate to size
        ALLOCATE(data(dims(1)))
      ENDIF
      
      ! Allocate space in datac
      ALLOCATE(datac(dims(1)))

      ! Read the dataset
      mem=H5T_NATIVE_CHARACTER
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
      FORALL(i=1:SIZE(data),datac(i)=='T')
        data(i)=.TRUE.
      END FORALL
      
      DEALLOCATE(datac)

#endif
    ENDSUBROUTINE read_b1
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-2 array of logicals from dataset
    SUBROUTINE read_b2(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readb2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: data(:,:)
      CHARACTER(LEN=1),ALLOCATABLE :: datac(:,:)
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

      ! Allocate space for data if needed
      IF(ALLOCATED(data))THEN
        ! Make sure the data is the right size
        IF(ANY(SHAPE(data) /= dims))THEN
          CALL this%e%raiseError(myName//": data array is the wrong size.")
        ENDIF
      ELSE
        ! Allocate to size
        ALLOCATE(data(dims(1),dims(2)))
      ENDIF
      
      ! Allocate space for datac
      ALLOCATE(datac(dims(1),dims(2)))

      ! Read the dataset
      mem=H5T_NATIVE_CHARACTER
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
            datac(i,j)=='T')
        data(i,j)=.TRUE.
      END FORALL
      
      DEALLOCATE(datac)

#endif
    ENDSUBROUTINE read_b2
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-3 array of logicals from dataset
    SUBROUTINE read_b3(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readb3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: data(:,:,:)
      CHARACTER(LEN=1),ALLOCATABLE :: datac(:,:,:)
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

      ! Allocate space for data if needed
      IF(ALLOCATED(data))THEN
        ! Make sure the data is the right size
        IF(ANY(SHAPE(data) /= dims))THEN
          CALL this%e%raiseError(myName//": data array is the wrong size.")
        ENDIF
      ELSE
        ! Allocate to size
        ALLOCATE(data(dims(1),dims(2),dims(3)))
      ENDIF
      
      ! Allocate space for datac
      ALLOCATE(datac(dims(1),dims(2),dims(3)))

      ! Read the dataset
      mem=H5T_NATIVE_CHARACTER
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
            datac(i,j,k)=='T')
        data(i,j,k)=.TRUE.
      END FORALL
      
      DEALLOCATE(datac)

#endif
    ENDSUBROUTINE read_b3
!
!-------------------------------------------------------------------------------
!> @brief Read a string from dataset
    SUBROUTINE read_c0(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readc0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),INTENT(INOUT) :: data
      CHARACTER,ALLOCATABLE :: datac(:)
      INTEGER(SIK) :: i
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

      ! Allocate character array
      ALLOCATE(datac(dims(1)))

      ! Read the dataset
      mem=H5T_NATIVE_CHARACTER
      CALL h5dread_f(dset_id,mem,datac,dims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to read data from dataset.")
      ENDIF
      
      ! Convert to StringType
      DO i=1,SIZE(datac)
        data=data//datac(i)
      ENDDO

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
    ENDSUBROUTINE read_c0
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-1 array of strings from dataset
    SUBROUTINE read_c1(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readc1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: data(:)
      CHARACTER,ALLOCATABLE :: datac(:,:)
      INTEGER(SIK) :: i,j
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


      ! Allocate character array to size
      ALLOCATE(datac(dims(1),dims(2)))

      ! Read the dataset
      mem=H5T_NATIVE_CHARACTER
      CALL h5dread_f(dset_id,mem,datac,dims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to read data from dataset.")
      ENDIF
      
      ! Allocate space if needed
      IF(ALLOCATED(data))THEN
        ! Make sure the data is the right size
        IF(SIZE(data) /= dims(2))THEN
          CALL this%e%raiseError(myName//": data array is the wrong size.")
        ENDIF
      ELSE
        ! Allocate to size
        ALLOCATE(data(dims(2)))
      ENDIF
      
      ! Convert to StringType
      DO i=1,SIZE(data)
        DO j=1,SIZE(datac(:,i))
          data(i)=data(i)//datac(j,i)
        ENDDO
      ENDDO
      
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
    ENDSUBROUTINE read_c1
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-2 array of strings from dataset
    SUBROUTINE read_c2(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readc2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: data(:,:)
      CHARACTER,ALLOCATABLE :: datac(:,:,:)
      INTEGER(SIK) :: i,j,k
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


      ! Allocate character array to size
      ALLOCATE(datac(dims(1),dims(2),dims(3)))

      ! Read the dataset
      mem=H5T_NATIVE_CHARACTER
      CALL h5dread_f(dset_id,mem,datac,dims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to read data from dataset.")
      ENDIF
      
      ! Allocate space if needed
      IF(ALLOCATED(data))THEN
        ! Make sure the data is the right size
        IF(ANY(SIZE(data) /= [dims(2),dims(3)]))THEN
          CALL this%e%raiseError(myName//": data array is the wrong size.")
        ENDIF
      ELSE
        ! Allocate to size
        ALLOCATE(data(dims(2),dims(3)))
      ENDIF
      
      ! Convert to StringType
      DO i=1,SIZE(data,1)
        DO j=1,SIZE(data,2)
          DO k=1,SIZE(datac(:,i,j))
            data(i,j)=data(i,j)//datac(k,i,j)
          ENDDO
        ENDDO
      ENDDO
      
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
    ENDSUBROUTINE read_c2
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-3 array of strings from dataset
    SUBROUTINE read_c3(this,dsetname,data)
      CHARACTER(LEN=*),PARAMETER :: myName='readc3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: this
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: data(:,:,:)
      CHARACTER,ALLOCATABLE :: datac(:,:,:,:)
      INTEGER(SIK) :: i,j,k,m
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
#ifdef MPACT_HAVE_HDF5
      INTEGER(HSIZE_T),DIMENSION(4) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=4
      
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


      ! Allocate character array to size
      ALLOCATE(datac(dims(1),dims(2),dims(3),dims(4)))

      ! Read the dataset
      mem=H5T_NATIVE_CHARACTER
      CALL h5dread_f(dset_id,mem,datac,dims,error)
      IF(error /= 0)THEN
        CALL this%e%raiseError(myName//": Failed to read data from dataset.")
      ENDIF
      
      ! Allocate space if needed
      IF(ALLOCATED(data))THEN
        ! Make sure the data is the right size
        IF(ANY(SIZE(data) /= [dims(2),dims(3),dims(4)]))THEN
          CALL this%e%raiseError(myName//": data array is the wrong size.")
        ENDIF
      ELSE
        ! Allocate to size
        ALLOCATE(data(dims(2),dims(3),dims(4)))
      ENDIF
      
      ! Convert to StringType
      DO i=1,SIZE(data,1)
        DO j=1,SIZE(data,2)
          DO m=1,SIZE(data,3)
            DO k=1,SIZE(datac(:,i,j,m))
             data(i,j,m)=data(i,j,m)//datac(k,i,j,m)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      
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
    ENDSUBROUTINE read_c3
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
