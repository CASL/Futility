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
!>  - @ref IO_Strings "IO_Strings' - @copybrief IO_Strings
!>  - @ref FileType_Base "FileType_Base' - @copybrief FileType_Base
!>  - @ref IntrType "IntrType' - @copybrief IntrType
!>  - @ref ExceptionHandler "ExceptionHandler' - @copybrief ExceptionHandler
!>  - @ref ParallelEnv "ParallelEnv' - @copybrief ParallelEnv
!>  - @ref Strings "Strings' - @copybrief Strings
!>
!> @author Mitchell T.H. Young
!>   @date 01/20/2013
!>
!> @par Revisions:
!> (03/12/2013) - Aaron Graham
!>   - Added read/write routines for SSK, SDK, SNK, SLK, SBK, and StringType
!>     variable types for arrays of rank 0-3.
!>   - Removed MPI/parallel code since it needs to be redesigned.
!> (03/28/2013) - Aaron Graham
!>   - Added delete routine
!>
!> @todo
!>  - Implement MPI/Parallel HDF5
!>  - Make sure routines are safe (check for initialized object, etc.)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE FileType_HDF5

#ifdef MPACT_HAVE_HDF5
  USE HDF5
#endif
  USE IntrType
  USE Strings
  USE ExceptionHandler
  USE IO_Strings
  USE ParallelEnv
  USE FileType_Base

  IMPLICIT NONE
  PRIVATE

#ifdef MPACT_HAVE_HDF5
  INTEGER(HID_T),PRIVATE :: error
#endif

  !> Name of the module
  CHARACTER(LEN=*),PARAMETER :: modName='FileType_HDF5'

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
    !> The unit number of the file
    INTEGER(SIK),PRIVATE :: unitno=-1
    !> Parallel environment for MPI I/O
    TYPE(MPI_EnvType) :: pe

#ifdef MPACT_HAVE_HDF5
    !> File id assigned by the HDF5 library when file is opened
    INTEGER(HID_T) :: file_id=0
#endif

    CONTAINS
      !> @copybrief FileType_HDF5::init_HDF5FileType
      !> @copydetails FileType_HDF5::init_HDF5FileType
      PROCEDURE,PASS :: init => init_HDF5FileType
      !> @copybrief FileType_HDF5::clear_HDF5FileType
      !> @copydetails FileType_HDF5::clear_HDF5FileType
      PROCEDURE,PASS :: clear => clear_HDF5FileType
      !> @copybrief FileType_HDF5::open_HDF5FileType
      !> @copydetails FileType_HDF5::open_HDF5FileType
      PROCEDURE,PASS :: fopen => open_HDF5FileType
      !> @copybrief FileType_HDF5::close_HDF5FileType
      !> @copydetails FileType_HDF5::close_HDF5FileType
      PROCEDURE,PASS :: fclose => close_HDF5FileType
      !> @copybrief FileType_HDF5::delete_HDF5FileType
      !> @copydetails FileType_HDF5::delete_HDF5FileType
      PROCEDURE,PASS :: fdelete => delete_HDF5FileType
      !> @copybrief FileType_HDF5::ls_HDF5FileType
      !> @copydetails FileType_HDF5::ls_HDF5FileType
      PROCEDURE,PASS :: ls => ls_HDF5FileType
      !> @copybrief FileType_HDF5::mkdir_HDF5FileType
      !> @copydetails FileType_HDF5::mkdir_HDF5FileType
      PROCEDURE,PASS :: mkdir => mkdir_HDF5FileType
      !> @copybrief FileType_HDF5::ngrp_HDF5FileType
      !> @copydetails FileType_HDF5::ngrp_HDF5FileType
      PROCEDURE,PASS :: ngrp => ngrp_HDF5FileType
      !> @copybrief FileType_HDF5::write_d0
      !> @copydoc FileType_HDF5::write_d0
      PROCEDURE,PASS,PRIVATE :: write_d0
      !> @copybrief FileType_HDF5::write_d1
      !> @copydoc FileType_HDF5::write_d1
      PROCEDURE,PASS,PRIVATE :: write_d1
      !> @copybrief FileType_HDF5::write_d2
      !> @copydoc FileType_HDF5::write_d2
      PROCEDURE,PASS,PRIVATE :: write_d2
      !> @copybrief FileType_HDF5::write_d3
      !> @copydoc FileType_HDF5::write_d3
      PROCEDURE,PASS,PRIVATE :: write_d3
      !> @copybrief FileType_HDF5::write_d4
      !> @copydoc FileType_HDF5::write_d4
      PROCEDURE,PASS,PRIVATE :: write_d4
      !> @copybrief FileType_HDF5::write_s0
      !> @copydoc FileType_HDF5::write_s0
      PROCEDURE,PASS,PRIVATE :: write_s0
      !> @copybrief FileType_HDF5::write_s1
      !> @copydoc FileType_HDF5::write_s1
      PROCEDURE,PASS,PRIVATE :: write_s1
      !> @copybrief FileType_HDF5::write_s2
      !> @copydoc FileType_HDF5::write_s2
      PROCEDURE,PASS,PRIVATE :: write_s2
      !> @copybrief FileType_HDF5::write_s3
      !> @copydoc FileType_HDF5::write_s3
      PROCEDURE,PASS,PRIVATE :: write_s3
      !> @copybrief FileType_HDF5::write_s4
      !> @copydoc FileType_HDF5::write_s4
      PROCEDURE,PASS,PRIVATE :: write_s4
      !> @copybrief FileType_HDF5::write_b0
      !> @copydoc FileType_HDF5::write_b0
      PROCEDURE,PASS,PRIVATE :: write_b0
      !> @copybrief FileType_HDF5::write_b1
      !> @copydoc FileType_HDF5::write_b1
      PROCEDURE,PASS,PRIVATE :: write_b1
      !> @copybrief FileType_HDF5::write_b2
      !> @copydoc FileType_HDF5::write_b2
      PROCEDURE,PASS,PRIVATE :: write_b2
      !> @copybrief FileType_HDF5::write_b3
      !> @copydoc FileType_HDF5::write_b3
      PROCEDURE,PASS,PRIVATE :: write_b3
      !> @copybrief FileType_HDF5::write_n0
      !> @copydoc FileType_HDF5::write_n0
      PROCEDURE,PASS,PRIVATE :: write_n0
      !> @copybrief FileType_HDF5::write_n1
      !> @copydoc FileType_HDF5::write_n1
      PROCEDURE,PASS,PRIVATE :: write_n1
      !> @copybrief FileType_HDF5::write_n2
      !> @copydoc FileType_HDF5::write_n2
      PROCEDURE,PASS,PRIVATE :: write_n2
      !> @copybrief FileType_HDF5::write_n3
      !> @copydoc FileType_HDF5::write_n3
      PROCEDURE,PASS,PRIVATE :: write_n3
      !> @copybrief FileType_HDF5::write_l0
      !> @copydoc FileType_HDF5::write_l0
      PROCEDURE,PASS,PRIVATE :: write_l0
      !> @copybrief FileType_HDF5::write_l1
      !> @copydoc FileType_HDF5::write_l1
      PROCEDURE,PASS,PRIVATE :: write_l1
      !> @copybrief FileType_HDF5::write_l2
      !> @copydoc FileType_HDF5::write_l2
      PROCEDURE,PASS,PRIVATE :: write_l2
      !> @copybrief FileType_HDF5::write_l3
      !> @copydoc FileType_HDF5::write_l3
      PROCEDURE,PASS,PRIVATE :: write_l3
      !> @copybrief FileType_HDF5::write_st0
      !> @copydoc FileType_HDF5::write_st0
      PROCEDURE,PASS,PRIVATE :: write_st0
      !> @copybrief FileType_HDF5::write_st1_helper
      !> @copydoc FileType_HDF5::write_st1_helper
      PROCEDURE,PASS,PRIVATE :: write_st1_helper
      !> @copybrief FileType_HDF5::write_st1
      !> @copydoc FileType_HDF5::write_st1
      PROCEDURE,PASS,PRIVATE :: write_st1
      !> @copybrief FileType_HDF5::write_st2_helper
      !> @copydoc FileType_HDF5::write_st2_helper
      PROCEDURE,PASS,PRIVATE :: write_st2_helper
      !> @copybrief FileType_HDF5::write_st2
      !> @copydoc FileType_HDF5::write_st2
      PROCEDURE,PASS,PRIVATE :: write_st2
      !> @copybrief FileType_HDF5::write_st3_helper
      !> @copydoc FileType_HDF5::write_st3_helper
      PROCEDURE,PASS,PRIVATE :: write_st3_helper
      !> @copybrief FileType_HDF5::write_st3
      !> @copydoc FileType_HDF5::write_st3
      PROCEDURE,PASS,PRIVATE :: write_st3
      !> @copybrief FileType_HDF5::write_c1
      !> @copydoc FileType_HDF5::write_c1
      PROCEDURE,PASS,PRIVATE :: write_c1
      !> Generic typebound interface for all @c write operations
      GENERIC :: fwrite => write_d0, write_d1, write_d2, write_d3, write_d4,   &
      write_s0, write_s1, write_s2, write_s3, write_s4, write_b0, write_b1,    &
      write_b2, write_b3, write_n0, write_n1, write_n2, write_n3, write_st0,    &
      write_st1_helper, write_st1, write_st2_helper, write_st2, write_st3_helper,   &
      write_st3, write_l0, write_l1, write_l2, write_l3, write_c1
      !> @copybrief FileType_HDF5::read_d0
      !> @copydoc FileType_HDF5::read_d0
      PROCEDURE,PASS,PRIVATE :: read_d0
      !> @copybrief FileType_HDF5::read_d1
      !> @copydoc FileType_HDF5::read_d1
      PROCEDURE,PASS,PRIVATE :: read_d1
      !> @copybrief FileType_HDF5::read_d2
      !> @copydoc FileType_HDF5::read_d2
      PROCEDURE,PASS,PRIVATE :: read_d2
      !> @copybrief FileType_HDF5::read_d3
      !> @copydoc FileType_HDF5::read_d3
      PROCEDURE,PASS,PRIVATE :: read_d3
      !> @copybrief FileType_HDF5::read_d4
      !> @copydoc FileType_HDF5::read_d4
      PROCEDURE,PASS,PRIVATE :: read_d4
      !> @copybrief FileType_HDF5::read_dp4
      !> @copydoc FileType_HDF5::read_dp4
      PROCEDURE,PASS,PRIVATE :: read_dp4
      !> @copybrief FileType_HDF5::read_s0
      !> @copydoc FileType_HDF5::read_s0
      PROCEDURE,PASS,PRIVATE :: read_s0
      !> @copybrief FileType_HDF5::read_s1
      !> @copydoc FileType_HDF5::read_s1
      PROCEDURE,PASS,PRIVATE :: read_s1
      !> @copybrief FileType_HDF5::read_s2
      !> @copydoc FileType_HDF5::read_s2
      PROCEDURE,PASS,PRIVATE :: read_s2
      !> @copybrief FileType_HDF5::read_s3
      !> @copydoc FileType_HDF5::read_s3
      PROCEDURE,PASS,PRIVATE :: read_s3
      !> @copybrief FileType_HDF5::read_s4
      !> @copydoc FileType_HDF5::read_s4
      PROCEDURE,PASS,PRIVATE :: read_s4
      !> @copybrief FileType_HDF5::read_b0
      !> @copydoc FileType_HDF5::read_b0
      PROCEDURE,PASS,PRIVATE :: read_b0
      !> @copybrief FileType_HDF5::read_b1
      !> @copydoc FileType_HDF5::read_b1
      PROCEDURE,PASS,PRIVATE :: read_b1
      !> @copybrief FileType_HDF5::read_b2
      !> @copydoc FileType_HDF5::read_b2
      PROCEDURE,PASS,PRIVATE :: read_b2
      !> @copybrief FileType_HDF5::read_b3
      !> @copydoc FileType_HDF5::read_b3
      PROCEDURE,PASS,PRIVATE :: read_b3
      !> @copybrief FileType_HDF5::read_n0
      !> @copydoc FileType_HDF5::read_n0
      PROCEDURE,PASS,PRIVATE :: read_n0
      !> @copybrief FileType_HDF5::read_n1
      !> @copydoc FileType_HDF5::read_n1
      PROCEDURE,PASS,PRIVATE :: read_n1
      !> @copybrief FileType_HDF5::read_n2
      !> @copydoc FileType_HDF5::read_n2
      PROCEDURE,PASS,PRIVATE :: read_n2
      !> @copybrief FileType_HDF5::read_n3
      !> @copydoc FileType_HDF5::read_n3
      PROCEDURE,PASS,PRIVATE :: read_n3
      !> @copybrief FileType_HDF5::read_l0
      !> @copydoc FileType_HDF5::read_l0
      PROCEDURE,PASS,PRIVATE :: read_l0
      !> @copybrief FileType_HDF5::read_l1
      !> @copydoc FileType_HDF5::read_l1
      PROCEDURE,PASS,PRIVATE :: read_l1
      !> @copybrief FileType_HDF5::read_l2
      !> @copydoc FileType_HDF5::read_l2
      PROCEDURE,PASS,PRIVATE :: read_l2
      !> @copybrief FileType_HDF5::read_l3
      !> @copydoc FileType_HDF5::read_l3
      PROCEDURE,PASS,PRIVATE :: read_l3
      !> @copybrief FileType_HDF5::read_st0
      !> @copydoc FileType_HDF5::read_st0
      PROCEDURE,PASS,PRIVATE :: read_st0
      !> @copybrief FileType_HDF5::read_st1
      !> @copydoc FileType_HDF5::read_st1
      PROCEDURE,PASS,PRIVATE :: read_st1
      !> @copybrief FileType_HDF5::read_st2
      !> @copydoc FileType_HDF5::read_st2
      PROCEDURE,PASS,PRIVATE :: read_st2
      !> @copybrief FileType_HDF5::read_st3
      !> @copydoc FileType_HDF5::read_st3
      PROCEDURE,PASS,PRIVATE :: read_st3
      !> @copybrief FileType_HDF5::read_c1
      !> @copydoc FileType_HDF5::read_c1
      PROCEDURE,PASS,PRIVATE :: read_c1
      !> Generic typebound interface for all @c read operations
      GENERIC :: fread => read_d1, read_d2, read_d3, read_d4, read_s1, read_s2,&
        read_s3, read_s4, read_l1, read_l2, read_l3, read_b1, read_b2, read_b3,&
        read_st0, read_d0, read_s0, read_l0, read_b0, read_st1, read_st2, read_st3,&
        read_n0, read_n1, read_n2, read_n3, read_c1
      !> Generic typebound interface for pointer-based read operations
      GENERIC :: freadp => read_dp4
  ENDTYPE
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes an HDF5 file object.
!> @param thisHDF5File the object to be initialized
!> @param filename the relative path to the file on the filesystem
!> @param mode the access mode. Can be 'READ', 'WRITE' or 'NEW'
!>
!> This routine initializes an HDF5 file object by setting the objects
!> attributes, initializing the HDF5 library interface and calling the @c open
!> routine.
!>
    SUBROUTINE init_HDF5FileType(thisHDF5File,filename,mode)
      CHARACTER(LEN=*),PARAMETER :: myName='init_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: filename
      CHARACTER(LEN=*),INTENT(IN) :: mode
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: fpath
      CHARACTER(LEN=MAX_FNAME_LENGTH) :: fname
      CHARACTER(LEN=MAX_FEXT_LENGTH) :: fext
      INTEGER(SIK) :: unitno
      LOGICAL(SBK) :: ostat
#endif
      IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
#ifdef MPACT_HAVE_HDF5
      CALL getFileParts(filename,fpath,fname,fext,thisHDF5File%e)
      CALL thisHDF5File%setFilePath(fpath)
      CALL thisHDF5File%setFileName(fname)
      CALL thisHDF5File%setFileExt(fext)

      ! Store the access mode
      thisHDF5File%mode=mode
      CALL toUPPER(thisHDF5File%mode)
      SELECTCASE(TRIM(thisHDF5File%mode))
      CASE('READ')
        CALL thisHDF5File%setWriteStat(.FALSE.)
      CASE('WRITE')
        CALL thisHDF5File%setWriteStat(.TRUE.)
      CASE('NEW')
        CALL thisHDF5File%setWriteStat(.TRUE.)
      CASE DEFAULT
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
        ' - Unrecognized access mode.')
      ENDSELECT

      thisHDF5File%fullname=filename

      ! Initialize the HDF5 interface. This needs be done before any other calls
      ! to the HF5 interface can be made.
      CALL h5open_f(error)
      IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
        ' - Could not initialize HDF5 INTERFACE.')

      ! Open the HDF5 file
      CALL thisHDF5File%fopen()

      ! Assign arbitrary UNIT number to file.  Used only for deleting file.
      unitno=99
      INQUIRE(UNIT=unitno,OPENED=ostat)
      DO WHILE(thisHDF5File%unitno == -1)
        IF(ostat) THEN
          unitno=unitno-1_SIK
          INQUIRE(UNIT=unitno,OPENED=ostat)
        ELSE
          thisHDF5File%unitno=unitno
        ENDIF
      ENDDO

      thisHDF5File%isinit=.TRUE.
#else
      ! We dont have HDF5, so we can't initialize
      CALL thisHDF5File%e%raiseWarning('The HDF5 library is not present in '// &
        'this build')
#endif
    ENDSUBROUTINE init_HDF5FileType
!
!-------------------------------------------------------------------------------
!> @brief Destructs an HDF5 file object.
!> @param thisHDF5File the HDF5 object to be destroyed
!>
!> This routine releases resources used for interacting with the HSF5 file. It
!> closes the file and the HDF5 library interface.
!>
    SUBROUTINE clear_HDF5FileType(thisHDF5File)
      CHARACTER(LEN=*),PARAMETER :: myName='clear_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
#ifdef MPACT_HAVE_HDF5
      ! Close the HDF5 file.
      CALL h5fclose_f(thisHDF5File%file_id,error)
      IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
        ' - Unable to close HDF5 file.')

      ! Close the HDF5 interface. This can only be done once all calls to the
      ! HDF5 library are complete.
      CALL h5close_f(error)
      IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
        ' - Unable to close HDF5 INTERFACE.')
#endif
      thisHDF5File%isinit=.FALSE.
      IF(ASSOCIATED(thisHDF5File%e)) DEALLOCATE(thisHDF5File%e)
    ENDSUBROUTINE clear_HDF5FileType
!
!-------------------------------------------------------------------------------
!> @brief Open an HDF5 file
!> @param file the HDF5FileType object to open
!>
!> This routine implements the abstract @c fopen routine in the base file type.
!> It uses the HDF5 library interface that was initialized in @c init to open
!> the file.
!>
    SUBROUTINE open_HDF5FileType(file)
      CHARACTER(LEN=*),PARAMETER :: myName='open_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: file
#ifdef MPACT_HAVE_HDF5
      INTEGER(HID_T) :: acc
      INTEGER(HID_T) :: plist_id

      CALL h5pcreate_f(H5P_FILE_ACCESS_F,plist_id,error)
      IF (error /= 0) CALL file%e%raiseError(modName//'::'//myName// &
        ' - Unable to create property list for open operation.')

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
        CALL h5fcreate_f(file%fullname,acc,file%file_id,error, &
            access_prp=plist_id)
      CASE DEFAULT
        CALL file%e%raiseError(modName//'::'//myName// &
        ' - Unrecognized access mode.')
      ENDSELECT

      CALL h5pclose_f(plist_id,error)
      IF(error /= 0) CALL file%e%raiseError(modName//'::'//myName// &
        ' - Unable to destroy property list.')
#endif
    ENDSUBROUTINE open_HDF5FileType
!
!-------------------------------------------------------------------------------
!> @brief Closes access to an HDF5 file
!> @param file the HDF5FileType object to close
!>
!> This routine implements the abstract @c fclose routine in the base file type.
!>
    SUBROUTINE close_HDF5FileType(file)
      CHARACTER(LEN=*),PARAMETER :: myName='close_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: file
#ifdef MPACT_HAVE_HDF5
      IF(.NOT.ASSOCIATED(file%e)) ALLOCATE(file%e)
      CALL file%e%setStopOnError(.FALSE.)
      IF(.NOT.file%isinit) THEN
        CALL file%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE
        CALL h5fclose_f(file%file_id,error)
        IF(error /= 0) CALL file%e%raiseError(modName//'::'//myName// &
          ' - Unable to close HDF5 file.')
      ENDIF
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
!>
    SUBROUTINE delete_HDF5FileType(file)
      CHARACTER(LEN=*),PARAMETER :: myName='delete_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: file
#ifdef MPACT_HAVE_HDF5
      LOGICAL(SBK) :: localalloc
      CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: emesg
      localalloc=.FALSE.
      IF(.NOT.ASSOCIATED(file%e)) THEN
        ALLOCATE(file%e)
        localalloc=.TRUE.
      ENDIF
      OPEN(UNIT=1,FILE=TRIM(file%getFilePath())// &
          TRIM(file%getFileName())//TRIM(file%getFileExt()), &
          IOSTAT=error)
      IF(error /= 0) THEN
        CALL file%e%raiseError(modName//'::'//myName// &
          ' - Could not open file.')
      ELSE
        IF(file%isinit) CALL file%clear()
        CLOSE(UNIT=1,STATUS='DELETE',IOSTAT=error)
        IF(error /= 0) THEN
          WRITE(emesg,'(a,i4,a,i4)') 'Error deleting file (UNIT=', &
              1,') IOSTAT=',error
          CALL file%e%raiseError(modName//'::'//myName//' - '//emesg)
        ELSE
          CALL file%setOpenStat(.FALSE.)
        ENDIF
      ENDIF
      IF(localalloc) DEALLOCATE(file%e)
#else
      ! We dont have HDF5, so we can't initialize
      CALL file%e%raiseWarning('The HDF5 library is not present in '// &
        'this build')
#endif
    ENDSUBROUTINE delete_HDF5FileType
!
!-------------------------------------------------------------------------------
!> @brief List the contents of a group
!> @param thisHDF5File the HDF5FileType object to operate on
!> @param path the absolute path to the group of interest (group heirarchy
!> represented with '->')
!> @param objs the list of objects to be returned. See below.
!>
!> This routine is useful for discovering the contents of an HDF5 file. A path
!> to a group in the file is provided in the subroutine call and the names of
!> its constituents is stored in the objs list. If objs is passed in, it will be
!> deallocated and reallocated to the proper size to store the names of all of
!> the objects in the group.
!>
    SUBROUTINE ls_HDF5FileType(thisHDF5File,path,objs)
      CHARACTER(LEN=*),PARAMETER :: myName='ls_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: path
      CHARACTER(LEN=*),ALLOCATABLE,INTENT(INOUT) :: objs(:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path2
      INTEGER(HSIZE_T) :: i
      INTEGER(HID_T) :: grp_id,error
      INTEGER :: store_type,nlinks,max_corder

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE
        path2=convertPath(path)

        CALL h5gopen_f(thisHDF5File%file_id, path2, grp_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Unable to open file.')

        CALL h5gget_info_f(grp_id, store_type, nlinks, max_corder, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Unable to get group information.')

        IF(ALLOCATED(objs)) DEALLOCATE(objs)
        ALLOCATE(objs(nlinks))

        DO i=0,nlinks-1
          CALL h5lget_name_by_idx_f(thisHDF5File%file_id, path2, H5_INDEX_NAME_F,&
              H5_ITER_INC_F, i, objs(i+1), error)
          IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Unable to get object name.')
        ENDDO

        CALL h5gclose_f(grp_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Unable to close group.')
      ENDIF
#endif
    ENDSUBROUTINE ls_HDF5FileType
!
!-------------------------------------------------------------------------------
!> @brief Creates a new group in the HDF file.
!> @param thisHDF5File the HDF5FileType object to operate on
!> @param path the path to the group to be created
!>
!> This routine is used to create a new group in an HDF5 file. It can only be
!> called if the file has write access.
!>
    SUBROUTINE mkdir_HDF5FileType(thisHDF5File,path)
      CHARACTER(LEN=*),PARAMETER :: myNAme='mkdir_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: path
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path2
      INTEGER(HID_T) :: group_id,error

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Ensure that we have write permissions to the file
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName &
            //'::'//myName//' - Can not create group in read-only file.')
      ELSE

        ! Convert the path to use slashes
        path2=convertPath(path)

        ! Create the group
        CALL h5gcreate_f(thisHDF5File%file_id,path2,group_id,error)

        IF(error == 0) THEN
          ! Close the group
          CALL h5gclose_f(group_id,error)
          IF(error /= 0) CALL thisHDF5File%e%raiseDebugWarning(modName//'::'// &
              myName//' - Failed to close HDF group')
        ELSE
          CALL thisHDF5File%e%raiseDebugWarning(modName//'::'//myName// &
            ' - Failed to create HDF5 group.')
        ENDIF
      ENDIF
#endif
    ENDSUBROUTINE mkdir_HDF5FileType
!
!-------------------------------------------------------------------------------
!> @brief Returns the number of objects in a group
!> @param thisHDF5File the HDF5FileType object to interrogate
!> @param path the group in the file to interrogate
!>
!> This function returns how many objects are in the group @c path.
!>
    FUNCTION ngrp_HDF5FileType(thisHDF5File,path) RESULT(ngrp)
      CHARACTER(LEN=*),PARAMETER :: myName='ngrp_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: path
      INTEGER(SIK) :: ngrp
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path2
      INTEGER(HID_T) :: grp_id,error
      INTEGER :: store_type,nlinks,max_corder

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        path2=convertPath(path)

        CALL h5gopen_f(thisHDF5File%file_id, path2, grp_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not open group in HDF5 file.')

        CALL h5gget_info_f(grp_id, store_type, nlinks, max_corder, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not get group info in HDF5 file.')

        ngrp=nlinks
      ENDIF
#else
      ngrp=0
#endif
    ENDFUNCTION ngrp_HDF5FileType
!
!-------------------------------------------------------------------------------
!> @brief Write a double to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a double @c vals to a dataset of name and path @c
!> dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_d0(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writed0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),INTENT(IN) :: vals
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=0

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=1

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_DOUBLE, &
          gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, vals, gdims, error, &
          dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace

        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')


        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_d0
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of doubles to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-1 array of doubles @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_d1(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writed1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),INTENT(IN) :: vals(:)
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
         ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_DOUBLE, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')


        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_d1
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of doubles to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-2 array of doubles @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_d2(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writed2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),INTENT(IN) :: vals(:,:)
      INTEGER(SIK),DIMENSION(2),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(2) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=2

      INTEGER(HID_T) :: dspace_id,gspace_id,dset_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
         ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1
        offset(2)=LBOUND(vals,2)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        ! Create and HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace.
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_DOUBLE, &
                gspace_id, dset_id,error,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not destroy property list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')


        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
      ENDIF
#endif
    ENDSUBROUTINE write_d2
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of doubles to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-3 array of doubles @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_d3(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writed3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),INTENT(IN) :: vals(:,:,:)
      INTEGER(SIK),DIMENSION(3),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(3) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=3

      INTEGER(HID_T) :: dspace_id,gspace_id,dset_id,plist_id


      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1
        offset(2)=LBOUND(vals,2)-1
        offset(3)=LBOUND(vals,3)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        ! Create and HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace.
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_DOUBLE, &
            gspace_id, dset_id,error, plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not destroy property list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')


        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not destroy property list.')
      ENDIF
#endif
    ENDSUBROUTINE write_d3
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-4 array of doubles to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-4 array of doubles @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_d4(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writed4_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),INTENT(IN) :: vals(:,:,:,:)
      INTEGER(SIK),DIMENSION(4),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(4) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=4

      INTEGER(HID_T) :: dspace_id,gspace_id,dset_id,plist_id


      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1
        offset(2)=LBOUND(vals,2)-1
        offset(3)=LBOUND(vals,3)-1
        offset(4)=LBOUND(vals,4)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        ! Create and HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace.
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_DOUBLE, &
            gspace_id, dset_id,error, plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not destroy property list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')


        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not destroy property list.')
      ENDIF
#endif
    ENDSUBROUTINE write_d4
!
!-------------------------------------------------------------------------------
!> @brief Write a real to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a real @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_s0(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writes0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),INTENT(IN) :: vals
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=0

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=1

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_REAL, &
            gspace_id, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)

        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')


        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_s0
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of reals to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-1 array of reals @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_s1(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writes1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),INTENT(IN) :: vals(:)
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_REAL, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')


        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_s1
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of reals to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-2 array of reals @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_s2(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writes2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),INTENT(IN) :: vals(:,:)
      INTEGER(SIK),DIMENSION(2),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(2) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=2

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1
        offset(2)=LBOUND(vals,1)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_REAL, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')


        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_s2
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of reals to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-3 array of reals @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_s3(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writes3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),INTENT(IN) :: vals(:,:,:)
      INTEGER(SIK),DIMENSION(3),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(3) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=3

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1
        offset(2)=LBOUND(vals,2)-1
        offset(3)=LBOUND(vals,3)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_REAL, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')


        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_s3
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-4 array of reals to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-4 array of reals @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_s4(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writes4_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),INTENT(IN) :: vals(:,:,:,:)
      INTEGER(SIK),DIMENSION(4),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(4) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=4

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1
        offset(2)=LBOUND(vals,2)-1
        offset(3)=LBOUND(vals,3)-1
        offset(4)=LBOUND(vals,4)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_REAL, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')


        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_s4
!
!-------------------------------------------------------------------------------
!> @brief Write a logical to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a logical @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_b0(thisHDF5File,dsetname,vals,gdims_in)
      IMPLICIT NONE
      CHARACTER(LEN=*),PARAMETER :: myName='writeb0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),INTENT(IN) :: vals
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER :: valsc
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=0

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=1

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_CHARACTER, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Convert to surrogate character array, since HDF5 does not support
        ! Boolean variables
        IF(vals .EQV. .TRUE.) THEN
          valsc='T'
        ELSE
          valsc='F'
        ENDIF

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, valsc, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')


        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_b0
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of logicals to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-1 array of logicals @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_b1(thisHDF5File,dsetname,vals,gdims_in)
      IMPLICIT NONE
      CHARACTER(LEN=*),PARAMETER :: myName='writeb1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),INTENT(IN) :: vals(:)
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER :: valsc(1:SIZE(vals))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK) :: i
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_CHARACTER, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Convert to surrogate character array, since HDF5 does not support
        ! Boolean variables
        valsc='F'
        FORALL(i=1:SIZE(vals),vals(i))
          valsc(i:i)='T'
        END FORALL

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, valsc, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_b1
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of logicals to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-2 array of logicals @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_b2(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writeb2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),INTENT(IN) :: vals(:,:)
      INTEGER(SIK),DIMENSION(2),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER :: valsc(SIZE(vals,DIM=1),SIZE(vals,DIM=2))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK) :: i,j
      INTEGER(HSIZE_T),DIMENSION(2) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=2

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1
        offset(2)=LBOUND(vals,2)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_CHARACTER, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Convert to surrogate character array, since HDF5 does not support
        ! Boolean variables
        valsc(:,:)='F'
        FORALL(i=1:SIZE(vals,DIM=1),j=1:SIZE(vals,DIM=2),vals(i,j))
          valsc(i,j)='T'
        END FORALL

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, valsc, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_b2
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of logicals to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-3 array of logicals @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_b3(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writeb3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),INTENT(IN) :: vals(:,:,:)
      INTEGER(SIK),DIMENSION(3),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER :: valsc(SIZE(vals,DIM=1),SIZE(vals,DIM=2),SIZE(vals,DIM=3))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK) :: i,j,k
      INTEGER(HSIZE_T),DIMENSION(3) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=3

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1
        offset(2)=LBOUND(vals,2)-1
        offset(3)=LBOUND(vals,3)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_CHARACTER, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Convert to surrogate character array, since HDF5 does not support
        ! Boolean variables
        valsc(:,:,:)='F'
        FORALL(i=1:SIZE(vals,DIM=1),j=1:SIZE(vals,DIM=2),k=1:SIZE(vals,DIM=3), &
                     vals(i,j,k))
          valsc(i,j,k)='T'
        END FORALL

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, valsc, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_b3
!-------------------------------------------------------------------------------
!> @brief Write a 32-bit integer to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes an integer @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_n0(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writen0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),INTENT(IN) :: vals
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=0

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=1

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_INTEGER, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_n0
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of 32-bit integers to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-1 array of integers @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_n1(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writen1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),INTENT(IN) :: vals(:)
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_INTEGER, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_n1
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of 32-bit integers to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-2 array of integers @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_n2(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writen2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),INTENT(IN) :: vals(:,:)
      INTEGER(SIK),DIMENSION(2),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(2) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=2

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1
        offset(2)=LBOUND(vals,2)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_INTEGER, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_n2
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of 32-bit integers to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-3 array of integers @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_n3(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writen3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),INTENT(IN) :: vals(:,:,:)
      INTEGER(SIK),DIMENSION(3),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(3) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=3

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1
        offset(2)=LBOUND(vals,2)-1
        offset(3)=LBOUND(vals,3)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_INTEGER, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_n3
!
!-------------------------------------------------------------------------------
!> @brief Write a 64-bit "integer" to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param valst data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a long integer @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.  A double is
!> used for the write operation to compensate for the lack of an long integer
!> write interface in the HDF5 library.
!>
    SUBROUTINE write_l0(thisHDF5File,dsetname,valst,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writel0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),INTENT(IN) :: valst
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      REAL(SDK) :: vals
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=0

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=1

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_STD_I64LE, gspace_id, &
            dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Convert data type
        vals=valst
        CALL thisHDF5File%e%raiseDebugWarning(modName//'::'//myName// &
          ' - Converting from long integer to double to accommodate HDF5!!!')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_l0
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of 64-bit "integers" to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param valst data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-1 array of long integers @c vals to a dataset of
!> name and path @c dsetname using the shape @c gdims_in, if present.  Doubles
!> are used for the write operation to compensate for the lack of an long
!> integer write interface in the HDF5 library.
!>
    SUBROUTINE write_l1(thisHDF5File,dsetname,valst,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writel1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),INTENT(IN) :: valst(:)
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      REAL(SDK) :: vals(SIZE(valst))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_STD_I64LE, gspace_id, &
            dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Convert to different datatyp
        vals=valst
        CALL thisHDF5File%e%raiseDebugWarning(modName//'::'//myName// &
          ' - Converting from long integer to double to accommodate HDF5!!!')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_l1
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of 64-bit "integers" to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param valst data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-2 array of long integers @c vals to a dataset of
!> name and path @c dsetname using the shape @c gdims_in, if present.  Doubles
!> are used for the write operation to compensate for the lack of an long
!> integer write interface in the HDF5 library.
!>
    SUBROUTINE write_l2(thisHDF5File,dsetname,valst,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writel2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),INTENT(IN) :: valst(:,:)
      INTEGER(SIK),DIMENSION(2),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      REAL(SDK) :: vals(SIZE(valst,1),SIZE(valst,2))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(2) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=2

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1
        offset(2)=LBOUND(vals,2)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_STD_I64LE, gspace_id, &
            dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Convert data type
        vals=valst
        CALL thisHDF5File%e%raiseDebugWarning(modName//'::'//myName// &
          ' - Converting from long integer to double to accommodate HDF5!!!')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_l2
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of 64-bit "integers" to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param valst data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-3 array of long integers @c vals to a dataset of
!> name and path @c dsetname using the shape @c gdims_in, if present.  Doubles
!> are used for the write operation to compensate for the lack of an long
!> integer write interface in the HDF5 library.
!>
    SUBROUTINE write_l3(thisHDF5File,dsetname,valst,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writel3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),INTENT(IN) :: valst(:,:,:)
      INTEGER(SIK),DIMENSION(3),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      REAL(SDK) :: vals(SIZE(valst,1),SIZE(valst,2),SIZE(valst,3))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(3) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=3

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=LBOUND(vals,1)-1
        offset(2)=LBOUND(vals,2)-1
        offset(3)=LBOUND(vals,3)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_STD_I64LE, gspace_id, &
            dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Convert Data type
        vals=valst
        CALL thisHDF5File%e%raiseDebugWarning(modName//'::'//myName// &
          ' - Converting from long integer to double to accommodate HDF5!!!')

        ! Write to the valsset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_l3
!
!-------------------------------------------------------------------------------
!> @brief Write a StringType to dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param valst data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a stringType @c vals to a dataset of
!> name and path @c dsetname using the shape @c gdims_in, if present.
!> StringsTypes are represented as rank-1 arrays of characters.
!>
    SUBROUTINE write_st0(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writec0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),INTENT(IN) :: vals
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=LEN_TRIM(vals)) :: valss
      CHARACTER, ALLOCATABLE :: valsc(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK) :: i
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! Convert StringType to character vector
        valss=CHAR(vals)
        ALLOCATE(valsc(LEN(valss)))
        DO i=1,SIZE(valsc)
          valsc(i)=valss(i:i)
        ENDDO

        ! stash offset
        offset(1)=LBOUND(valsc,1)-1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(valsc)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_CHARACTER, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, valsc, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')
              DEALLOCATE(valsc)

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_st0
!
!-------------------------------------------------------------------------------
!> @brief Find max string length, then write rank-1 array of Strings to dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param valst data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine finds the length of the longest stringType in @c vals, stores
!> the value in @c length_max, and calls the @c fwrite routine.
!>
    SUBROUTINE write_st1_helper(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: MyName='writec1helper_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(IN) :: vals(:)
      INTEGER(SIK) :: length_max,i
      INTEGER(SIK),DIMENSION(1),INTENT(IN),OPTIONAL :: gdims_in

      length_max=0
      DO i=1,SIZE(vals)
        length_max=MAX(LEN(vals(i)),length_max)
      ENDDO

      IF(PRESENT(gdims_in)) THEN
        CALL thisHDF5File%fwrite(dsetname,vals,length_max,(/length_max,gdims_in/))
      ELSE
        CALL thisHDF5File%fwrite(dsetname,vals,length_max,(/length_max,SHAPE(vals)/))
      ENDIF
    ENDSUBROUTINE write_st1_helper
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of StringTypes to dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @length_max the length of the longest stringType to be written
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-1 array of stringTypes @c vals to a dataset of
!> name and path @c dsetname using the shape @c gdims_in, if present.
!> StringsTypes are represented as rank-1 arrays of characters.
!>
    SUBROUTINE write_st1(thisHDF5File,dsetname,vals,length_max,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writec1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(IN) :: vals(:)
      INTEGER(SIK),INTENT(IN) :: length_max
      INTEGER(SIK),DIMENSION(2),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=length_max) :: valss
      CHARACTER :: valsc(length_max,SIZE(vals))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK) :: i,j
      INTEGER(HSIZE_T),DIMENSION(2) :: ldims,gdims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=2

      INTEGER :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')

      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Fill character array
        DO i=1,SIZE(vals)
          valss=CHAR(vals(i))
          DO j=1,length_max
            valsc(j,i)=valss(j:j)
          ENDDO
        ENDDO

        ! stash offset
        offset(1)=LBOUND(valsc,1)-1
        offset(2)=LBOUND(valsc,2)-1

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(valsc)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_CHARACTER, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, valsc, gdims, error, &
          dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_st1
!
!-------------------------------------------------------------------------------
!> @brief Find max string length, then write rank-2 array of Strings to dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param valst data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine finds the length of the longest stringType in @c vals, stores
!> the value in @c length_max, and calls the @c fwrite routine.
!>
    SUBROUTINE write_st2_helper(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: MyName='writec2helper_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(IN) :: vals(:,:)
      INTEGER(SIK) :: length_max,i,j
      INTEGER(SIK),DIMENSION(2),INTENT(IN),OPTIONAL :: gdims_in

      length_max=0
      DO j=1,SIZE(vals,1)
        DO i=1,SIZE(vals,2)
          length_max=MAX(LEN(vals(j,i)),length_max)
        ENDDO
      ENDDO

      IF(PRESENT(gdims_in)) THEN
        CALL thisHDF5File%fwrite(dsetname,vals,length_max,(/length_max,gdims_in/))
      ELSE
        CALL thisHDF5File%fwrite(dsetname,vals,length_max)
      ENDIF
    ENDSUBROUTINE write_st2_helper
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-2 array of StringTypes to dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @length_max the length of the longest stringType to be written
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-2 array of stringTypes @c vals to a dataset of
!> name and path @c dsetname using the shape @c gdims_in, if present.
!> StringsTypes are represented as rank-1 arrays of characters.
!>
    SUBROUTINE write_st2(thisHDF5File,dsetname,vals,length_max,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writec2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SIK),INTENT(IN) :: length_max
      TYPE(StringType),ALLOCATABLE,INTENT(IN) :: vals(:,:)
      INTEGER(SIK),DIMENSION(3),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER :: valsc(length_max,SIZE(vals,1),SIZE(vals,2))
      CHARACTER(LEN=length_max) :: valss
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK) :: i,j,k
      INTEGER(HSIZE_T),DIMENSION(3) :: gdims,ldims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=3

      INTEGER :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')

      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE
        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        DO k=1,SIZE(vals,2)
          DO i=1,SIZE(vals,1)
            valss=CHAR(vals(i,k))
            DO j=1,length_max
              valsc(j,i,k)=valss(j:j)
            ENDDO
          ENDDO
        ENDDO

        ! stash offset
        offset(1)=LBOUND(valsc,1)-1
        offset(2)=LBOUND(valsc,2)-1
        offset(3)=LBOUND(valsc,3)-1

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(valsc)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_CHARACTER, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global '//&
            'dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, valsc, ldims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_st2
!
!-------------------------------------------------------------------------------
!> @brief Find max string length, then write rank-3 array of Strings to dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param valst data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine finds the length of the longest stringType in @c vals, stores
!> the value in @c length_max, and calls the @c fwrite routine.
!>
    SUBROUTINE write_st3_helper(thisHDF5File,dsetname,vals,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: MyName='writec3helper_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(IN) :: vals(:,:,:)
      INTEGER(SIK) :: length_max,i,j,k
      INTEGER(SIK),DIMENSION(3),INTENT(IN),OPTIONAL :: gdims_in

      length_max=0
      DO k=1,SIZE(vals,3)
        DO j=1,SIZE(vals,1)
          DO i=1,SIZE(vals,2)
            length_max=MAX(LEN_TRIM(vals(j,i,k)),length_max)
          ENDDO
        ENDDO
      ENDDO

      IF(PRESENT(gdims_in)) THEN
        CALL thisHDF5File%fwrite(dsetname,vals,length_max,(/length_max,gdims_in/))
      ELSE
        CALL thisHDF5File%fwrite(dsetname,vals,length_max)
      ENDIF
    ENDSUBROUTINE write_st3_helper
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-3 array of StringTypes to dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param length_max the length of the longest stringType to be written
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-3 array of stringTypes @c vals to a dataset of
!> name and path @c dsetname using the shape @c gdims_in, if present.
!> StringsTypes are represented as rank-1 arrays of characters.
!>
    SUBROUTINE write_st3(thisHDF5File,dsetname,vals,length_max,gdims_in)
      CHARACTER(LEN=*),PARAMETER :: myName='writec3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(IN) :: vals(:,:,:)
      INTEGER(SIK),INTENT(IN) :: length_max
      INTEGER(SIK),DIMENSION(4),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=length_max) :: valss
      CHARACTER :: valsc(length_max,SIZE(vals,1),SIZE(vals,2),SIZE(vals,3))
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK) :: i,j,k,m
      INTEGER(HSIZE_T),DIMENSION(4) :: gdims,ldims,offset,one
      INTEGER(HID_T),PARAMETER :: rank=4

      INTEGER :: error
      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')

      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Fill character array
        DO m=1,SIZE(vals,3)
          DO k=1,SIZE(vals,2)
            DO i=1,SIZE(vals,1)
              valss=TRIM(vals(i,k,m))
              DO j=1,length_max
                valsc(j,i,k,m)=valss(j:j)
              ENDDO
            ENDDO
          ENDDO
        ENDDO

        ! stash offset
        offset(1)=LBOUND(valsc,1)-1
        offset(2)=LBOUND(valsc,2)-1
        offset(3)=LBOUND(valsc,3)-1
        offset(4)=LBOUND(valsc,4)-1

        ! Determine the dimensions for the dataspace
        ldims=SHAPE(valsc)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims=gdims_in
        ELSE
          gdims=ldims
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter'//        ' list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_CHARACTER, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global '//&
            'dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, valsc, ldims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_st3
!
!-------------------------------------------------------------------------------
!> @brief Write a rank-1 array of character (Fortran string) to a dataset
!> @param thisHDF5File the HDF5FileType object to write to
!> @param dsetname dataset name and path to write to
!> @param vals data to write to dataset
!> @param gdims_in shape of data to write with
!>
!> This routine writes a rank-1 array of characters @c vals to a dataset of name
!> and path @c dsetname using the shape @c gdims_in, if present.
!>
    SUBROUTINE write_c1(thisHDF5File,dsetname,vals,gdims_in)
      IMPLICIT NONE
      CHARACTER(LEN=*),PARAMETER :: myName='writec1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      CHARACTER(LEN=*),INTENT(IN) :: vals
      INTEGER(SIK),INTENT(IN),OPTIONAL :: gdims_in
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: ldims,offset,one,gdims
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: dspace_id,dset_id,gspace_id,plist_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')

      ! Check that the file is writable. Best to catch this before HDF5 does.
      ELSEIF(.NOT.thisHDF5File%isWrite()) THEN
        CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - File is readonly!')
      ELSE

        ! stash offset
        offset(1)=1

        ! set one to ones. This is usually used for more complicated parallel
        ! chunking schemes, but we are doing a simplified case
        one=1

        ! Convert the path name
        path=convertPath(dsetname)

        ! Determine the dimensions for the dataspace
        ldims(1)=LEN(vals)

        ! Store the dimensions from global if present
        IF(PRESENT(gdims_in)) THEN
          gdims(1)=gdims_in
        ELSE
          gdims(1)=ldims(1)
        ENDIF

        !Create an HDF5 parameter list for the dataset creation.
        CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create parameter list.')

        ! Create the dataspace
        ! Global dataspace
        CALL h5screate_simple_f(rank,gdims,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Local dataspace
        CALL h5screate_simple_f(rank,ldims,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataspace.')

        ! Create the dataset
        CALL h5dcreate_f(thisHDF5File%file_id, path, H5T_NATIVE_CHARACTER, &
            gspace_id, dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create dataset:'//path)

        ! Destroy the property list
        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close parameter list.')

        ! Select the global dataspace for the dataset
        CALL h5dget_space_f(dset_id,gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not select global dataspace for the dataset.')

        ! Create a property list for the write operation
        CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not create property list for write operation.')

        ! Write to the dataset
        CALL h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, vals, gdims, error, &
            dspace_id,gspace_id,plist_id)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not write to the dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5sclose_f(gspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the dataspace.')

        CALL h5pclose_f(plist_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Could not close the parameter list.')
      ENDIF
#endif
    ENDSUBROUTINE write_c1
!
!-------------------------------------------------------------------------------
!> @brief Read a double from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a double from the dataset @c dsetname and stores
!> the value in @c vals
!>
    SUBROUTINE read_d0(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readd0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),INTENT(INOUT) :: vals
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
           ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Read the dataset
        mem=H5T_NATIVE_DOUBLE
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_d0
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-1 array of doubles from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-1 array of doubles from the dataset @c dsetname
!> and stores the values in @c vals
!>
    SUBROUTINE read_d1(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readd1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: vals(:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')

        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1)))
        ENDIF

        ! Read the dataset
        mem=H5T_NATIVE_DOUBLE
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_d1
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-2 array of doubles from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-2 array of doubles from the dataset @c dsetname
!> and stores the values in @c vals
!>
    SUBROUTINE read_d2(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readd2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: vals(:,:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(2) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=2

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1),dims(2)))
        ENDIF

        ! Read the dataset
        mem=H5T_NATIVE_DOUBLE
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_d2
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-3 array of doubles from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-3 array of doubles from the dataset @c dsetname
!> and stores the values in @c vals
!>
    SUBROUTINE read_d3(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readd3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: vals(:,:,:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(3) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=3

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1),dims(2),dims(3)))
        ENDIF

        ! Read the dataset
        mem=H5T_NATIVE_DOUBLE
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_d3
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-4 array of doubles from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-4 array of doubles from the dataset @c dsetname
!> and stores the values in @c vals
!>
    SUBROUTINE read_d4(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readd4_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),ALLOCATABLE,INTENT(INOUT) :: vals(:,:,:,:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(4) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=4

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1),dims(2),dims(3),dims(4)))
        ENDIF

        ! Read the dataset
        mem=H5T_NATIVE_DOUBLE
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_d4
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-4 array of doubles from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-4 array of doubles from the dataset @c dsetname
!> and stores the values in @c vals
!>
    SUBROUTINE read_dp4(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readd4_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SDK),POINTER,INTENT(INOUT) :: vals(:,:,:,:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(4) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=4

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
         CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ASSOCIATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1),dims(2),dims(3),dims(4)))
        ENDIF

        ! Read the dataset
        mem=H5T_NATIVE_DOUBLE
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_dp4
!
!-------------------------------------------------------------------------------
!> @brief Read a real from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a real from the dataset @c dsetname
!> and stores the value in @c vals
!>
    SUBROUTINE read_s0(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='reads0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),INTENT(INOUT) :: vals
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Read the dataset
        mem=H5T_NATIVE_REAL
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_s0
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-1 array of reals from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-1 array of reals from the dataset @c dsetname
!> and stores the values in @c vals
!>
    SUBROUTINE read_s1(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='reads1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: vals(:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the vals is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1)))
        ENDIF

        ! Read the dataset
        mem=H5T_NATIVE_REAL
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_s1
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-2 array of reals from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-2 array of reals from the dataset @c dsetname
!> and stores the values in @c vals
!>
    SUBROUTINE read_s2(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='reads2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: vals(:,:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(2) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=2

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1),dims(2)))
        ENDIF

        ! Read the dataset
        mem=H5T_NATIVE_REAL
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_s2
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-3 array of reals from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-3 array of reals from the dataset @c dsetname
!> and stores the values in @c vals
!>
    SUBROUTINE read_s3(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='reads3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: vals(:,:,:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(3) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=3

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
         ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1),dims(2),dims(3)))
        ENDIF

        ! Read the dataset
        mem=H5T_NATIVE_REAL
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_s3
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-4 array of reals from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-4 array of reals from the dataset @c dsetname
!> and stores the values in @c vals
!>
    SUBROUTINE read_s4(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='reads4_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      REAL(SSK),ALLOCATABLE,INTENT(INOUT) :: vals(:,:,:,:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(4) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=4

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1),dims(2),dims(3),dims(4)))
        ENDIF

        ! Read the dataset
        mem=H5T_NATIVE_REAL
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_s4
!
!-------------------------------------------------------------------------------
!> @brief Read a 32-bit integer from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads an integer from the dataset @c dsetname
!> and stores the values in @c vals
!>
    SUBROUTINE read_n0(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readn0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),INTENT(INOUT) :: vals
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Read the dataset
        mem=H5T_NATIVE_INTEGER
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

      ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF

#endif
    ENDSUBROUTINE read_n0
!

!-------------------------------------------------------------------------------
!> @brief Read a rank-1 array of 32-bit integers from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-1 array of integers from the dataset @c dsetname
!> and stores the values in @c vals
!>
    SUBROUTINE read_n1(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readn1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: vals(:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1)))
        ENDIF

        ! Read the dataset
        mem=H5T_NATIVE_INTEGER
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_n1
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-2 array of 32-bit integers from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-2 array of integers from the dataset @c dsetname
!> and stores the values in @c vals
!>
    SUBROUTINE read_n2(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readn2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: vals(:,:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(2) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=2

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1),dims(2)))
        ENDIF

        ! Read the dataset
        mem=H5T_NATIVE_INTEGER
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_n2
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-3 array of 32-bit integers from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-3 array of integers from the dataset @c dsetname
!> and stores the values in @c vals
!>
    SUBROUTINE read_n3(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readn3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SNK),ALLOCATABLE,INTENT(INOUT) :: vals(:,:,:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(3) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=3

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1),dims(2),dims(3)))
        ENDIF

        ! Read the dataset
        mem=H5T_NATIVE_INTEGER
        CALL h5dread_f(dset_id,mem,vals,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_n3
!
!-------------------------------------------------------------------------------
!> @brief Read a 64-bit "integer" from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a long integer from the dataset @c dsetname
!> and stores the values in @c vals.  A double is used to read the data to
!> compensate for the lack of a long integer read interface in the HDF5 library.
!>
    SUBROUTINE read_l0(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readl0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),INTENT(INOUT) :: vals
#ifdef MPACT_HAVE_HDF5
      REAL(SDK) :: valst
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Read the dataset
        mem=H5T_NATIVE_DOUBLE
        CALL h5dread_f(dset_id,mem,valst,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')

        ! Convert data type
        vals=valst
        CALL thisHDF5File%e%raiseDebugWarning(modName//'::'//myName// &
          ' - Converting from double to long integer!')
      ENDIF
#endif
    ENDSUBROUTINE read_l0
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-1 array of 64-bit "integers" from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-1 array of long integers from the dataset @c
!> dsetname and stores the values in @c vals.  A double is used to read the data
!> to compensate for the lack of a long integer read interface in the HDF5
!> library.
!>
    SUBROUTINE read_l1(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readl1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: vals(:)
#ifdef MPACT_HAVE_HDF5
      INTEGER(SNK),ALLOCATABLE :: valst(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1)))
        ENDIF
        ALLOCATE(valst(dims(1)))

        ! Read the dataset
        mem=H5T_NATIVE_INTEGER
        CALL h5dread_f(dset_id,mem,valst,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')

        ! Conver data type
        vals=valst
        CALL thisHDF5File%e%raiseDebugWarning(modName//'::'//myName// &
          ' - Converting from double to long integer!')
      ENDIF
#endif
    ENDSUBROUTINE read_l1
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-2 array of 64-bit "integers" from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-2 array of long integers from the dataset @c
!> dsetname and stores the values in @c vals.  A double is used to read the data
!> to compensate for the lack of a long integer read interface in the HDF5
!> library.
!>
    SUBROUTINE read_l2(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readl2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: vals(:,:)
#ifdef MPACT_HAVE_HDF5
      INTEGER(SNK),ALLOCATABLE :: valst(:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(2) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=2

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1),dims(2)))
        ENDIF
        ALLOCATE(valst(dims(1),dims(2)))

        ! Read the dataset
        mem=H5T_NATIVE_INTEGER
        CALL h5dread_f(dset_id,mem,valst,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')

        ! Conver data type
        vals=valst
        CALL thisHDF5File%e%raiseDebugWarning(modName//'::'//myName// &
          ' - Converting from double to long integer!')
      ENDIF
#endif
    ENDSUBROUTINE read_l2
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-3 array of 64-bit "integers" from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-3 array of long integers from the dataset @c
!> dsetname and stores the values in @c vals.  A double is used to read the data
!> to compensate for the lack of a long integer read interface in the HDF5
!> library.
!>
    SUBROUTINE read_l3(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readl3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      INTEGER(SLK),ALLOCATABLE,INTENT(INOUT) :: vals(:,:,:)
#ifdef MPACT_HAVE_HDF5
      INTEGER(SNK),ALLOCATABLE :: valst(:,:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(3) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=3

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1),dims(2),dims(3)))
        ENDIF
        ALLOCATE(valst(dims(1),dims(2),dims(3)))

        ! Read the dataset
        mem=H5T_NATIVE_INTEGER
        CALL h5dread_f(dset_id,mem,valst,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')

        ! Conver data type
        vals=valst
        CALL thisHDF5File%e%raiseDebugWarning(modName//'::'//myName// &
          ' - Converting from double to long integer!')
      ENDIF
#endif
    ENDSUBROUTINE read_l3
!
!-------------------------------------------------------------------------------
!> @brief Read a logical from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a logical from the dataset @c
!> dsetname and stores the values in @c vals.  The datum is read as a character
!> 'T' or 'F' then converted to a logical.
!>
    SUBROUTINE read_b0(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readb0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),INTENT(INOUT) :: vals
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=1) :: valsc
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Read the dataset
        mem=H5T_NATIVE_CHARACTER
        CALL h5dread_f(dset_id,mem,valsc,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Convert to logical from character
        IF(valsc=='F') THEN
          vals=.FALSE.
        ELSE
          vals=.TRUE.
        ENDIF

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_b0
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-1 array of logicals from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-1 array of logicals from the dataset @c
!> dsetname and stores the values in @c vals.  The data are read as characters
!> 'T' or 'F' then converted to logicals.
!>
    SUBROUTINE read_b1(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readb1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: vals(:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER,ALLOCATABLE :: valsc(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER :: i
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)

        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space in data if needed
        IF(ALLOCATED(vals)) THEN
           ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1)))
        ENDIF

        ! Allocate space in datac
        ALLOCATE(valsc(dims(1)))

        ! Read the dataset
        mem=H5T_NATIVE_CHARACTER
        CALL h5dread_f(dset_id,mem,valsc,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')

        ! Convert from surrogate character array to boolean array
        vals=.FALSE.
        FORALL(i=1:SIZE(vals),valsc(i) == 'T')
          vals(i)=.TRUE.
        ENDFORALL

        DEALLOCATE(valsc)
      ENDIF
#endif
    ENDSUBROUTINE read_b1
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-2 array of logicals from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-2 array of logicals from the dataset @c
!> dsetname and stores the values in @c vals.  The data are read as characters
!> 'T' or 'F' then converted to logicals.
!>
    SUBROUTINE read_b2(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readb2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: vals(:,:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=1),ALLOCATABLE :: valsc(:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK) :: i,j
      INTEGER(HSIZE_T),DIMENSION(2) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=2

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)

        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
         ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space for data if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1),dims(2)))
        ENDIF

        ! Allocate space for datac
        ALLOCATE(valsc(dims(1),dims(2)))

        ! Read the dataset
        mem=H5T_NATIVE_CHARACTER
        CALL h5dread_f(dset_id,mem,valsc,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')

        ! Convert from surrogate character array to boolean array
        vals=.FALSE.
        FORALL(i=1:SIZE(vals,DIM=1),j=1:SIZE(vals,DIM=2),valsc(i,j) == 'T')
          vals(i,j)=.TRUE.
        ENDFORALL

        DEALLOCATE(valsc)
      ENDIF
#endif
    ENDSUBROUTINE read_b2
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-3 array of logicals from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-3 array of logicals from the dataset @c
!> dsetname and stores the values in @c vals.  The data are read as characters
!> 'T' or 'F' then converted to logicals.
!>
    SUBROUTINE read_b3(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readb3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      LOGICAL(SBK),ALLOCATABLE,INTENT(INOUT) :: vals(:,:,:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER(LEN=1),ALLOCATABLE :: valsc(:,:,:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(SIK) :: i,j,k
      INTEGER(HSIZE_T),DIMENSION(3) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=3

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate space for data if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ANY(SHAPE(vals) /= dims)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(1),dims(2),dims(3)))
        ENDIF

        ! Allocate space for datac
        ALLOCATE(valsc(dims(1),dims(2),dims(3)))

        ! Read the dataset
        mem=H5T_NATIVE_CHARACTER
        CALL h5dread_f(dset_id,mem,valsc,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')

        ! Convert from surrogate character array to boolean array
        vals(:,:,:)=.FALSE.
        FORALL(i=1:SIZE(vals,DIM=1),j=1:SIZE(vals,DIM=2),k=1:SIZE(vals,DIM=3), &
          valsc(i,j,k) == 'T')
          vals(i,j,k)=.TRUE.
        ENDFORALL

        DEALLOCATE(valsc)
      ENDIF
#endif
    ENDSUBROUTINE read_b3
!
!-------------------------------------------------------------------------------
!> @brief Read a string from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a stringType from the dataset @c
!> dsetname and stores the values in @c vals.  Each string is read as an array
!> of characters then converted to a stringType.
!>
    SUBROUTINE read_st0(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readst0_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),INTENT(INOUT) :: vals
#ifdef MPACT_HAVE_HDF5
      CHARACTER,ALLOCATABLE :: valsc(:)
      INTEGER(SIK) :: i
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)

        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate character array
        ALLOCATE(valsc(dims(1)))

        ! Read the dataset
        mem=H5T_NATIVE_CHARACTER
        CALL h5dread_f(dset_id,mem,valsc,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Convert to StringType
        vals=''
        DO i=1,SIZE(valsc)
          vals=vals//valsc(i)
        ENDDO

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_st0
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-1 array of strings from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-1 array of stringTypes from the dataset @c
!> dsetname and stores the values in @c vals.  Each string is read as an array
!> of characters then converted to a stringType.
!>
    SUBROUTINE read_st1(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readst1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: vals(:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER,ALLOCATABLE :: valsc(:,:)
      INTEGER(SIK) :: i,j
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(2) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=2

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')

        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate character array to size
        ALLOCATE(valsc(dims(1),dims(2)))

        ! Read the dataset
        mem=H5T_NATIVE_CHARACTER
        CALL h5dread_f(dset_id,mem,valsc,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(SIZE(vals) /= dims(2)) CALL thisHDF5File%e%raiseError(modName// &
            '::'//myName//' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(2)))
        ENDIF

        ! Convert to StringType
        DO i=1,SIZE(vals)
          vals(i)=''
          DO j=1,SIZE(valsc(:,i))
            vals(i)=vals(i)//valsc(j,i)
          ENDDO
        ENDDO

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_st1
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-2 array of strings from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-2 array of stringTypes from the dataset @c
!> dsetname and stores the values in @c vals.  Each string is read as an array
!> of characters then converted to a stringType.
!>
    SUBROUTINE read_st2(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readst2_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: vals(:,:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER,ALLOCATABLE :: valsc(:,:,:)
      INTEGER(SIK) :: i,j,k
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(3) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=3

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)

        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')


        ! Allocate character array to size
        ALLOCATE(valsc(dims(1),dims(2),dims(3)))

        ! Read the dataset
        mem=H5T_NATIVE_CHARACTER
        CALL h5dread_f(dset_id,mem,valsc,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ALL(SHAPE(vals) /= (/dims(2),dims(3)/))) &
              CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
                ' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(2),dims(3)))
        ENDIF

        ! Convert to StringType
        DO i=1,SIZE(vals,1)
          DO j=1,SIZE(vals,2)
            vals(i,j)=''
            DO k=1,SIZE(valsc(:,i,j))
              vals(i,j)=vals(i,j)//valsc(k,i,j)
            ENDDO
          ENDDO
        ENDDO

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_st2
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-3 array of strings from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-3 array of stringTypes from the dataset @c
!> dsetname and stores the values in @c vals.  Each string is read as an array
!> of characters then converted to a stringType.
!>
    SUBROUTINE read_st3(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readst3_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      TYPE(StringType),ALLOCATABLE,INTENT(INOUT) :: vals(:,:,:)
#ifdef MPACT_HAVE_HDF5
      CHARACTER,ALLOCATABLE :: valsc(:,:,:,:)
      INTEGER(SIK) :: i,j,k,m
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER(HSIZE_T),DIMENSION(4) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=4

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)

        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')


        ! Allocate character array to size
        ALLOCATE(valsc(dims(1),dims(2),dims(3),dims(4)))

        ! Read the dataset
        mem=H5T_NATIVE_CHARACTER
        CALL h5dread_f(dset_id,mem,valsc,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Allocate space if needed
        IF(ALLOCATED(vals)) THEN
          ! Make sure the data is the right size
          IF(ALL(SHAPE(vals) /= (/dims(2),dims(3),dims(4)/))) &
            CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
              ' - data array is the wrong size.')
        ELSE
          ! Allocate to size
          ALLOCATE(vals(dims(2),dims(3),dims(4)))
        ENDIF

        ! Convert to StringType
        DO i=1,SIZE(vals,1)
          DO j=1,SIZE(vals,2)
            DO m=1,SIZE(vals,3)
              vals(i,j,m)=''
              DO k=1,SIZE(valsc(:,i,j,m))
               vals(i,j,m)=vals(i,j,m)//valsc(k,i,j,m)
              ENDDO
            ENDDO
          ENDDO
        ENDDO

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')
      ENDIF
#endif
    ENDSUBROUTINE read_st3
!
!-------------------------------------------------------------------------------
!> @brief Read a rank-1 array of characters (Fortran string) from dataset
!> @param thisHDF5File the HDF5FileType object to read from
!> @param dsetname dataset name and path to read from
!> @param vals variable to hold read data
!>
!> This routine reads a rank-1 array of characters from the dataset @c
!> dsetname and stores the values in @c vals.  The data are read as characters
!> 'T' or 'F' then converted to logicals.
!>
    SUBROUTINE read_c1(thisHDF5File,dsetname,vals)
      CHARACTER(LEN=*),PARAMETER :: myName='readc1_HDF5FileType'
      CLASS(HDF5FileType),INTENT(INOUT) :: thisHDF5File
      CHARACTER(LEN=*),INTENT(IN) :: dsetname
      CHARACTER(LEN=*),INTENT(INOUT) :: vals
#ifdef MPACT_HAVE_HDF5
      CHARACTER,ALLOCATABLE :: valsc(:)
      CHARACTER(LEN=MAX_PATH_LENGTH) :: path
      INTEGER :: i
      INTEGER(HSIZE_T),DIMENSION(1) :: dims,maxdims
      INTEGER(HID_T),PARAMETER :: rank=1

      INTEGER(HID_T) :: mem,ndims
      INTEGER(HID_T) :: dspace_id,dset_id

      ! Make sure the object is initialized
      IF(.NOT.thisHDF5File%isinit) THEN
        IF(.NOT.ASSOCIATED(thisHDF5File%e)) ALLOCATE(thisHDF5File%e)
        CALL thisHDF5File%e%setStopOnError(.FALSE.)
        CALL thisHDF5File%e%raiseError(modName// &
          '::'//myName//' - File object not initialized.')
      ELSE

        ! Convert the path name to use slashes
        path=convertPath(dsetname)

        ! Open the dataset
        CALL h5dopen_f(thisHDF5File%file_id, path, dset_id, error)

        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to open dataset.')

        ! Get dataset dimensions for allocation
        CALL h5dget_space_f(dset_id,dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to obtain the dataspace.')
        ! Make sure the rank is right
        CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve number of dataspace dimensions.')
        IF(ndims /= rank) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Using wrong read function for rank.')

        CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
        IF(error < 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to retrieve dataspace dimensions.')

        ! Allocate surrogate data
        ALLOCATE(valsc(dims(1)))

        ! Read the dataset
        mem=H5T_NATIVE_CHARACTER
        CALL h5dread_f(dset_id,mem,valsc,dims,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to read data from dataset.')

        ! Close the dataset
        CALL h5dclose_f(dset_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataset.')

        ! Close the dataspace
        CALL h5sclose_f(dspace_id,error)
        IF(error /= 0) CALL thisHDF5File%e%raiseError(modName//'::'//myName// &
          ' - Failed to close dataspace.')

        ! Convert from surrogate character array to boolean array
        vals(:)=" "
        DO i=1,SIZE(valsc)
          vals(i:i)=valsc(i)
        ENDDO
      ENDIF
#endif
    ENDSUBROUTINE read_c1
!
!-------------------------------------------------------------------------------
!> @brief Convert a path provided with '->' separators to '/'
!> @param path the path string to convert.
!>
!> Paths in MPACT use '->' to resolve heirarchy. HSF5 uses '/'.
!>
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
        IF(ind > 0) THEN
          convertPath=TRIM(convertpath)//'/'//path(ipos:ipos2-1)
          ipos=ipos2+2
        ELSE
          convertPath=TRIM(convertPath)//'/'//path(ipos:last)
          EXIT
        ENDIF
      ENDDO ! Elements in path string
    ENDFUNCTION convertPath
!
ENDMODULE FileType_HDF5
