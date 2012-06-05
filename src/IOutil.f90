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
!> @brief Global module for I/O utility modules, collecting all
!> public members of other I/O type modules. This is the module that should be
!> used elsewhere in the code.
!> 
!> This module and its dependent modules are tested using @c testIOutil.f90.
!> The dependent modules are listed below. Examples on how to use this
!> module can be found in the examples of each of the dependent modules. This
!> is the module that should be used elsewhere when generic I/O functionality is
!> required. If implementing a new capability for I/O then the modules used by
!> this module should be used and the new module should be used here.
!>
!> @par
!> The functionality presently, only encompasses Fortran file types. The file
!> type object is abstracted in such a way that it should be relatively painless
!> to add new types of files. One example of a new file  type may be a SILO
!> database or HDF5 file. 
!>
!> @par Module Dependencies
!>  - @ref IO_Strings "IO_Strings": @copybrief IO_Strings
!>  - @ref FileType_Base "FileType_Base": @copybrief FileType_Base
!>  - @ref FileType_Fortran "FileType_Fortran": @copybrief FileType_Fortran
!>  - @ref FileType_Log "FileType_Log": @copybrief FileType_Log
!>  - @ref FileType_Input "FileType_Input": @copybrief FileType_Input
!>
!> @author Brendan Kochunas
!>   @date 05/10/2011
!>
!> @par Revisions
!> (07/15/2011) - Brendan Kochunas
!>   - Changed overall design moving functions in IOutil into new modules whose
!>     capabalities are collected here. The reason for this was to provide
!>     better modularity, and better "hooks" for introducing new features later.
!> @par
!> (08/21/2011) - Brendan Kochunas
!>   - Added "input file" type
!> @todo
!>   - Design and add an "output file" abstract type
!>   - Finish documentation
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE IOutil

  USE IO_Strings
  USE FileType_Base
  USE FileType_Fortran
  USE FileType_Log
  USE FileType_Input
  IMPLICIT NONE
  PRIVATE
  
  !Public parameters
  PUBLIC :: MAX_PATH_LENGTH
  PUBLIC :: MAX_FNAME_LENGTH
  PUBLIC :: MAX_FEXT_LENGTH
  PUBLIC :: MAX_INPUT_FILE_LINE_LEN
  
  !Public members from IO_Strings
  PUBLIC :: BLANK
  PUBLIC :: BANG
  PUBLIC :: DOT
  PUBLIC :: FSLASH
  PUBLIC :: BSLASH
  PUBLIC :: SLASH
  PUBLIC :: COLON
  PUBLIC :: getFilePath
  PUBLIC :: getFileName
  PUBLIC :: getFileNameExt
  PUBLIC :: getFileParts
  PUBLIC :: nFields
  PUBLIC :: getField
  PUBLIC :: toUPPER
  PUBLIC :: strmatch
  PUBLIC :: nmatchstr
  PUBLIC :: strfind
  PUBLIC :: strrep
  PUBLIC :: stripComment
  
  !File Types
  PUBLIC :: BaseFileType
  PUBLIC :: FortranFileType
  PUBLIC :: LogFileType
  PUBLIC :: InputFileType
!
ENDMODULE IOutil
